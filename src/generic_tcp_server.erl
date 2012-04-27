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

-module(generic_tcp_server).

-behaviour(gen_server).

-export([start_link/5]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

start_link(Module, Host, Port, ListenOpts, ModuleOpts) ->
    gen_server:start_link(?MODULE, [Module, Host, Port, ListenOpts, ModuleOpts], []).

%---------------------------------------------------------------------------

accept_and_start(Module, ModuleOpts, LSock) ->
    spawn_link(fun () ->
		       case gen_tcp:accept(LSock) of
			   {ok, Sock} ->
			       accept_and_start(Module, ModuleOpts, LSock),
			       {ok, Pid} = gen_server:start(Module, [Sock | ModuleOpts], []),
			       gen_tcp:controlling_process(Sock, Pid),
			       gen_server:cast(Pid, {socket_control_transferred, Sock});
			   {error, Reason} ->
			       exit({error, Reason})
		       end
	       end).

ip_listen_opt(any) ->
    [];
ip_listen_opt(Host) ->
    {ok, IP} = inet:getaddr(Host, inet),
    [{ip, IP}].

%---------------------------------------------------------------------------

init([Module, Host, Port, ListenOpts, ModuleOpts]) ->
    {ok, LSock} = gen_tcp:listen(Port, ip_listen_opt(Host) ++ ListenOpts),
    accept_and_start(Module, ModuleOpts, LSock),
    {ok, LSock}.

terminate(_Reason, State) ->
    LSock = State,
    gen_tcp:close(LSock),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Message, State) ->
    {noreply, State}.
