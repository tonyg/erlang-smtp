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

-module(pop3_server_session).
%% Based on rfc 1939.

-behaviour(gen_server).

-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

%---------------------------------------------------------------------------

lookup_user("tonyg") ->
    {ok, "tonyg", "test"};
lookup_user(_) ->
    not_found.

make_message(Headers, Body) ->
    lists:flatten([lists:map(fun ({K, V}) -> [K, ": ", V, "\r\n"] end, Headers), "\r\n",
		   lists:map(fun (B) -> [B, "\r\n"] end, Body)]).

retrieve_mailbox() ->
    Messages = [make_message([{"Subject", "test2"},
			      {"To", "mailbox@somewhere.interesting"},
			      {"Message-ID", "<a@b>"}],
			     ["Hello!"]),
		make_message([{"Subject", "argh"},
			      {"Message-ID", "<b@b>"}],
			     ["Bar", ".", "..", "...", "Foo"])],
    {Count, Octets} = lists:foldl(fun (M, {C0, O0}) ->
					  put({msg, C0 + 1}, {live, M}),
					  {C0 + 1, O0 + length(M)}
				  end, {0, 0}, Messages),
    put(livecount, Count),
    put(totalcount, Count),
    put(liveoctets, Octets),
    ok.

release_mailbox() ->
    ok.

lookup_message(MsgNum) ->
    case get({msg, MsgNum}) of
	{live, M} ->
	    %% Both extant and non-deleted
	    {ok, M};
	_ ->
	    not_found
    end.

mark_for_deletion(MsgNum) ->
    case get({msg, MsgNum}) of
	{live, M} ->
	    put({msg, MsgNum}, {dead, M}),
	    put(livecount, get(livecount) - 1),
	    put(liveoctets, get(liveoctets) - length(M)),
	    ok;
	_ ->
	    ok
    end.

undelete_all(0) ->
    ok;
undelete_all(MsgNum) ->
    case get({msg, MsgNum}) of
	{dead, M} ->
	    put({msg, MsgNum}, {live, M}),
	    put(livecount, get(livecount) + 1),
	    put(liveoctets, get(liveoctets) + length(M));
	_ ->
	    ok
    end,
    undelete_all(MsgNum - 1).

scan_listing_line({Position, Count}) when Position >= Count ->
    none;
scan_listing_line({Position, Count}) ->
    MsgNum = Position + 1,
    NextSeed = {MsgNum, Count},
    case lookup_message(MsgNum) of
	{ok, M} ->
	    {ok, scan_listing_line1(MsgNum, M), NextSeed};
	not_found ->
	    scan_listing_line(NextSeed)
    end.

scan_listing_line1(MsgNum, M) ->
    io_lib:format("~p ~p", [MsgNum, length(M)]).

format_message_for_transmission(M) ->
    {ok, Lines} = regexp:split(M, "\r\n"),
    lists:map(fun byte_stuff_line/1, Lines).

byte_stuff_line("." ++ Line) ->
    ".." ++ Line;
byte_stuff_line(Line) ->
    Line.

split_headers(Lines) ->
    split_headers([], Lines).

split_headers(RevHeaders, []) ->
    {lists:reverse(RevHeaders), []};
split_headers(RevHeaders, ["" | Body]) ->
    {lists:reverse(RevHeaders), Body};
split_headers(RevHeaders, [Line | Rest]) ->
    split_headers([Line | RevHeaders], Rest).

headers_and_top(Count, Lines) ->
    {Headers, Body} = split_headers(Lines),
    Headers ++ [""] ++ lists:sublist(Body, Count).

stat_mailbox() ->
    {get(totalcount), get(livecount), get(liveoctets)}.

%---------------------------------------------------------------------------

-record(session, {socket, state, challenge, username}).

reply_line(ok, "") -> "+OK\r\n";
reply_line(ok, Text) -> ["+OK ", Text, "\r\n"];
reply_line(err, "") -> "-ERR\r\n";
reply_line(err, Text) -> ["-ERR ", Text, "\r\n"].

send(Sock, Block) ->
    io:format("--> ~p~n", [lists:flatten(Block)]),
    gen_tcp:send(Sock, Block).

reply(Code, Text, State = #session{socket = Socket}) ->
    send(Socket, reply_line(Code, Text)),
    State.

generate(Generator, Seed) ->
    generate(Generator, Seed, []).

generate(Generator, Seed, Acc) ->
    case Generator(Seed) of
	{ok, Value, NewSeed} ->
	    generate(Generator, NewSeed, [Value | Acc]);
	none ->
	    lists:reverse(Acc)
    end.

reply_multi(Code, Text, Lines, State = #session{socket = Socket}) ->
    send(Socket, reply_line(Code, Text)),
    lists:foreach(fun (Line) ->
			  send(Socket, [Line, "\r\n"])
		  end, Lines),
    send(Socket, ".\r\n"),
    State.

strip_crlf(S) ->
    lists:reverse(strip_crlf1(lists:reverse(S))).

strip_crlf1([$\n, $\r | S]) -> S.

fmt(F, A) ->
    lists:flatten(io_lib:format(F, A)).

make_challenge() ->
    {A,B,C} = now(),
    N = node(),
    fmt("<~p.~p.~p.~p>", [A, B, C, N]).

do_apop(Name, Digest, Challenge) ->
    case lookup_user(Name) of
	{ok, _Name, Password} ->
	    MyDigest = erlang:md5(Challenge ++ Password),
	    Width = 8 * size(MyDigest),
	    <<DigestInt:Width/integer>> = MyDigest,
	    DigestHex = string:to_lower(fmt("~*.16.0b", [size(MyDigest) * 2, DigestInt])),
	    if
		DigestHex == Digest ->
		    ok;
		true ->
		    err
	    end;
	not_found ->
	    err
    end.

enter_transaction_state(State) ->
    ok = retrieve_mailbox(),
    {noreply, reply(ok, "Ready", State#session{state = transaction})}.

lookup_message_k(MsgNumStr, State, K) ->
    MsgNum = list_to_integer(MsgNumStr),
    case lookup_message(MsgNum) of
	not_found ->
	    {noreply, reply(err, "No such message", State)};
	{ok, M} ->
	    K(MsgNum, M)
    end.

handle_command_line(Line, State = #session{state = SessionState}) ->
    [Command | Args] = string:tokens(Line, " "),
    io:format("Command: ~p~nState:   ~p~n", [Line, State]),
    handle_command(string:to_upper(Command), Args, SessionState, State).

handle_command("APOP", [Name, Digest], authorization, State = #session{challenge = Challenge}) ->
    case do_apop(Name, Digest, Challenge) of
	ok -> enter_transaction_state(State#session{username = Name});
	err -> {noreply, reply(err, "Permission denied", State)}
    end;

handle_command("STAT", _Args, transaction, State) ->
    {_TotalMessages, Messages, Octets} = stat_mailbox(),
    {noreply, reply(ok, io_lib:format("~p ~p", [Messages, Octets]), State)};

handle_command("LIST", [], transaction, State) ->
    {TotalMessages, Messages, Octets} = stat_mailbox(),
    {noreply, reply_multi(ok, io_lib:format("~p messages (~p octets)", [Messages, Octets]),
			  generate(fun scan_listing_line/1, {0, TotalMessages}),
			  State)};

handle_command("LIST", [MsgNumStr], transaction, State) ->
    lookup_message_k(MsgNumStr, State,
		     fun (MsgNum, M) ->
			     {noreply, reply(ok, scan_listing_line1(MsgNum, M), State)}
		     end);

handle_command("RETR", [MsgNumStr], transaction, State) ->
    lookup_message_k(MsgNumStr, State,
		     fun (_MsgNum, M) ->
			     {noreply, reply_multi(ok, io_lib:format("~p octets", [length(M)]),
						   format_message_for_transmission(M),
						   State)}
		     end);

handle_command("DELE", [MsgNumStr], transaction, State) ->
    lookup_message_k(MsgNumStr, State,
		     fun (MsgNum, _M) ->
			     ok = mark_for_deletion(MsgNum),
			     {noreply, reply(ok, "Deleted", State)}
		     end);

handle_command("NOOP", _Args, transaction, State) ->
    {noreply, reply(ok, "", State)};

handle_command("RSET", _Args, transaction, State) ->
    {TotalMessages, _Messages, _Octets} = stat_mailbox(),
    undelete_all(TotalMessages),
    {_TotalMessages, Messages, Octets} = stat_mailbox(),
    {noreply, reply(ok, io_lib:format("~p messages (~p octets)", [Messages, Octets]), State)};

handle_command("QUIT", _Args, _SessionState, State) ->
    ok = release_mailbox(),
    {stop, normal, reply(ok, "Goodbye", State)};

handle_command("TOP", [MsgNumStr, CountStr], transaction, State) ->
    Count = list_to_integer(CountStr),
    lookup_message_k(MsgNumStr, State,
		     fun (_MsgNum, M) ->
			     Lines = headers_and_top(Count, format_message_for_transmission(M)),
			     {noreply, reply_multi(ok, "", Lines, State)}
		     end);

handle_command(Command, _Args, _SessionState, State) ->
    {noreply, reply(err, "Bad command " ++ Command, State)}.

%---------------------------------------------------------------------------

init([Sock]) ->
    {ok, #session{socket = Sock,
		  state = authorization,
		  challenge = make_challenge(),
		  username = undefined}}.

terminate(_Reason, #session{socket = Sock}) ->
    gen_tcp:close(Sock),
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.

handle_call(Request, _From, State) ->
    {stop, {bad_call, Request}, State}.

handle_cast({socket_control_transferred, _Sock}, State = #session{socket = Sock,
								  challenge = Challenge}) ->
    inet:setopts(Sock, [{active, true}]),
    {noreply, reply(ok, Challenge, State)};

handle_cast(Request, State) ->
    {stop, {bad_cast, Request}, State}.

handle_info({tcp, _Sock, FullLine}, State) ->
    handle_command_line(strip_crlf(FullLine), State);

handle_info({tcp_closed, _Sock}, State) ->
    %%error_logger:warning_msg("POP3 session closed without warning"),
    {stop, normal, State};

handle_info({tcp_error, _Sock, Reason}, State) ->
    error_logger:warning_msg("POP3 session closed with socket error ~p", [Reason]),
    {stop, normal, State};

handle_info(Message, State) ->
    {stop, {bad_info, Message}, State}.
