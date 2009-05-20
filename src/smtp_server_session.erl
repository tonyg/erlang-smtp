%%---------------------------------------------------------------------------
%% Copyright (c) 2007 Tony Garnock-Jones <tonyg@kcbbs.gen.nz>
%% Copyright (c) 2007 LShift Ltd. <query@lshift.net>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use, copy,
%% modify, merge, publish, distribute, sublicense, and/or sell copies
%% of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
%% MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
%% BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
%% ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
%% CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
%% SOFTWARE.
%%---------------------------------------------------------------------------

-module(smtp_server_session).
%% Somewhat loosely based on rfc 2821.
%% Doesn't even begin to address rfc 2822 in any serious way.

%% FIXME: SMTP AUTH

-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-record(session, {socket, mode, reverse_path, forward_paths, data_buffer,
		  verification_callback, delivery_callback}).

reply_line(Code, Text, false) ->
    [integer_to_list(Code), " ", Text, "\r\n"];
reply_line(Code, Text, true) ->
    [integer_to_list(Code), "-", Text, "\r\n"].

reply(Code, Text, State = #session{socket = Socket}) ->
    gen_tcp:send(Socket, reply_line(Code, Text, false)),
    State.

reply_multi(Code, [], State = #session{socket = Socket}) ->
    gen_tcp:send(Socket, reply_line(Code, "nothing to see here, move along", false)),
    State;
reply_multi(Code, [Text], State = #session{socket = Socket}) ->
    gen_tcp:send(Socket, reply_line(Code, Text, false)),
    State;
reply_multi(Code, [Text | More], State = #session{socket = Socket}) ->
    gen_tcp:send(Socket, reply_line(Code, Text, true)),
    reply_multi(Code, More, State).

reset_buffers(State) ->
    State#session{reverse_path = undefined,
		  forward_paths = [],
		  data_buffer = []}.

address_to_path(Address) ->
    case regexp:match(Address, "[^@]+@") of
	{match, 1, Length} ->
	    {string:substr(Address, 1, Length - 1), string:substr(Address, Length + 1)};
	_ ->
	    Address
    end.

split_path_from_params(Str) ->
    case regexp:match(Str, "<[^>]*>") of
	{match, Start, Length} ->
	    Address = string:substr(Str, Start + 1, Length - 2),
	    Params = string:strip(string:substr(Str, Start + Length), left),
	    {address_to_path(Address), Params};
	_ ->
	    case httpd_util:split(Str, " ", 2) of
		{ok, [Address]} ->
		    {address_to_path(Address), ""};
		{ok, [Address, Params]} ->
		    {address_to_path(Address), Params}
	    end
    end.

parse_path_and_parameters(PrefixRegexp, Data) ->
    case regexp:first_match(Data, PrefixRegexp) of
	nomatch ->
	    unintelligible;
	{match, 1, Length} ->
	    PathAndParams = string:strip(string:substr(Data, Length + 1), left),
	    {Path, Params} = split_path_from_params(PathAndParams),
	    {ok, Path, Params}
    end.

handle_command_line(Line, State) ->
    {Command, Data} = case httpd_util:split(Line, " ", 2) of
			  {ok, [C]} -> {string:to_upper(C), ""};
			  {ok, [C, D]} -> {string:to_upper(C), D}
		      end,
    handle_command(Command, Data, State).

handle_command("QUIT", _ClientDomain, State) ->
    {stop, normal, reply(221, "Goodbye",
			 reset_buffers(State))};

handle_command("EHLO", _ClientDomain, State) ->
    ServerDomain = "bogus.smtp.server.domain", %% FIXME
    {noreply, reply(250, ServerDomain ++ " You have reached an SMTP service",
		    reset_buffers(State))};

handle_command("HELO", _ClientDomain, State) ->
    ServerDomain = "bogus.smtp.server.domain", %% FIXME
    {noreply, reply(250, ServerDomain ++ " You have reached an SMTP service",
		    reset_buffers(State))};

handle_command("MAIL", FromReversePathAndMailParameters, State) ->
    case parse_path_and_parameters("[fF][rR][oO][mM]:", FromReversePathAndMailParameters) of
	unintelligible ->
	    {noreply, reply(553, "Unintelligible reverse-path", State)};
	{ok, Path, _Params} ->
	    {noreply, reply(250, "OK",
			    State#session{reverse_path = Path})}
    end;

handle_command("RCPT", ToForwardPathAndMailParameters,
	       State = #session{reverse_path = ReversePath,
				forward_paths = ForwardPaths,
				verification_callback = VerificationCallback}) ->
    if
	ReversePath == undefined ->
	    {noreply, reply(503, "MAIL first", State)};
	true ->
	    case parse_path_and_parameters("[tT][oO]:", ToForwardPathAndMailParameters) of
		unintelligible ->
		    {noreply, reply(553, "Unintelligible forward-path", State)};
		{ok, Path, _Params} ->
		    NewForwardPaths = [Path | ForwardPaths],
		    case verify(VerificationCallback, ReversePath, NewForwardPaths) of
			ok ->
			    {noreply, reply(250, "OK",
					    State#session{forward_paths = NewForwardPaths})};
			_ ->
			    {noreply, reply(550, io_lib:format("Unacceptable recipient ~p",
							       [Path]),
					    State)}
		    end
	    end
    end;

handle_command("DATA", _Junk, State = #session{forward_paths = ForwardPaths}) ->
    if
	ForwardPaths == [] ->
	    {noreply, reply(503, "RCPT first", State)};
	true ->
	    {noreply, reply(354, "Go ahead", State#session{mode = data})}
    end;

handle_command("RSET", _Junk, State) ->
    {noreply, reply(250, "OK",
		    reset_buffers(State))};

handle_command("VRFY", _UserOrMailboxPossibly, State) ->
    {noreply, reply(252, "Will not VRFY", State)};

handle_command("EXPN", _MailingListPossibly, State) ->
    {noreply, reply(252, "Will not EXPN", State)};

handle_command("HELP", _MaybeCommand, State) ->
    {noreply, reply(502, "Unimplemented", State)};

handle_command("NOOP", _Junk, State) ->
    {noreply, reply(250, "OK", State)};

handle_command(Command, _Data, State) ->
    {noreply, reply(500, "Unsupported command " ++ Command, State)}.

handle_data_line(".\r\n", State = #session{reverse_path = ReversePath,
					   forward_paths = ForwardPaths,
					   data_buffer = DataBuffer,
					   delivery_callback = DeliveryCallback}) ->
    {Code, Text} = case deliver(DeliveryCallback, ReversePath, ForwardPaths, DataBuffer) of
		       ok -> {250, "OK"};
		       _ -> {554, "Transaction failed"}
		   end,
    {noreply, reply(Code, Text,
		    reset_buffers(State#session{mode = command}))};
handle_data_line("." ++ Line, State) ->
    accumulate_line(Line, State);
handle_data_line(Line, State) ->
    accumulate_line(Line, State).

accumulate_line(Line, State = #session{data_buffer = Buffer}) ->
    {noreply, State#session{data_buffer = [Line | Buffer]}}.

verify({M,F,A}, ReversePath, ForwardPaths) ->
    case catch apply(M, F, [ReversePath, ForwardPaths | A]) of
	{'EXIT', Reason} ->
	    error_logger:error_msg("SMTP verification callback failed: ~p", [Reason]),
	    callback_failure;
	Result -> Result
    end.

deliver({M,F,A}, ReversePath, Mailboxes, DataLinesRev) ->
    case catch apply(M, F, [ReversePath, Mailboxes, lists:reverse(DataLinesRev) | A]) of
	{'EXIT', Reason} ->
	    error_logger:error_msg("SMTP delivery callback failed: ~p", [Reason]),
	    callback_failure;
	Result -> Result
    end.

%---------------------------------------------------------------------------

init([Sock, DeliveryCallback]) ->
    init([Sock, DeliveryCallback, none]);
init([Sock, DeliveryCallback, VerificationCallback]) ->
    {ok, reset_buffers(#session{socket = Sock,
				mode = initializing,
				delivery_callback = DeliveryCallback,
				verification_callback = VerificationCallback})}.

terminate(_Reason, #session{socket = Sock}) ->
    gen_tcp:close(Sock),
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.

handle_call(Request, _From, State) ->
    {stop, {bad_call, Request}, State}.

handle_cast({socket_control_transferred, _Sock}, State = #session{socket = Sock}) ->
    inet:setopts(Sock, [{active, true}]),
    {noreply, reply(220, "Hi there", State#session{mode = command})};

handle_cast(Request, State) ->
    {stop, {bad_cast, Request}, State}.

handle_info({tcp, _Sock, FullLine}, State = #session{mode = command}) ->
    handle_command_line(smtp_util:strip_crlf(FullLine), State);

handle_info({tcp, _Sock, FullLine}, State = #session{mode = data}) ->
    handle_data_line(FullLine, State);

handle_info({tcp_closed, _Sock}, State) ->
    %%error_logger:warning_msg("SMTP session closed without warning"),
    {stop, normal, State};

handle_info({tcp_error, _Sock, Reason}, State) ->
    error_logger:warning_msg("SMTP session closed with socket error ~p", [Reason]),
    {stop, normal, State};

handle_info(Message, State) ->
    {stop, {bad_info, Message}, State}.
