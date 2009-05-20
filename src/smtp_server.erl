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

-module(smtp_server).

-export([start/0, stop/0, start/2, stop/1]).

%% Callbacks.
-export([log_delivery/3, log_new_rcpt/2]).

start() -> application:start(?MODULE).
stop() -> application:stop(?MODULE).

start(normal, []) ->
    {ok, Host} = application:get_env(listen_host),
    {ok, Port} = application:get_env(listen_port),
    generic_tcp_server:start_link(smtp_server_session, Host, Port,
				  [list,
				   {active, false},
				   {packet, line},
				   {reuseaddr, true}],
				  [{?MODULE, log_delivery, []},
				   {?MODULE, log_new_rcpt, []}]).

stop(_State) ->
    ok.

log_new_rcpt(_ReversePath, [Rcpt | _Rest]) ->
    io:format("New recipient: ~p~n", [Rcpt]),
    ok.

log_delivery(ReversePath, ForwardPaths, DataLines) ->
    Message = rfc2822:parse(DataLines),
    io:format("SMTP delivery:~n - reverse path ~p~n - forward paths ~p~n - ~p~n",
	      [ReversePath, ForwardPaths, Message]),
    ok.
