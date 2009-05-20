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

-module(rfc2822).
%% Not-yet-conformant implementation of RFC 2822 messages.
%% Needs to 
%%  - handle more of the syntax
%%  - handle the obsolete syntax
%%  - contain a printer, not just a parser
%%  - deal with structured fields
%%  - deal with comments
%%  - have an address parser?

-export([parse/1]).

-include("rfc2822.hrl").

is_header_separator("") -> true;
is_header_separator("\n") -> true;
is_header_separator("\r\n") -> true;
is_header_separator(_) -> false.

strip_crlf(S) ->
    lists:reverse(strip_crlf1(lists:reverse(S))).
strip_crlf1([$\n, $\r | S]) -> S.

split_header(Line) ->
    {ok, [Key, Value]} = httpd_util:split(Line, ":", 2),
    {string:strip(Key), strip_crlf(string:strip(Value, left))}.

finish_header(none, _Value, Message) ->
    Message;
finish_header(Key, Value, Message = #rfc2822{headers = Headers}) ->
    Message#rfc2822{headers = [{Key, Value} | Headers]}.

finish_message(Message = #rfc2822{headers = Headers}) ->
    Message#rfc2822{headers = lists:reverse(Headers)}.

parse(Lines) ->
    parse(Lines, none, none, #rfc2822{headers = [], bodylines = []}).

parse([], Key, Value, Message) ->
    % Never saw the empty line separating the headers from the
    % body. Odd, but we'll permit it. FIXME: Check the RFC
    finish_message(finish_header(Key, Value, Message));
parse([Line | Rest], Key, Value, Message) ->
    case is_header_separator(Line) of
	true ->
	    finish_message(Message#rfc2822{bodylines = Rest});
	false ->
	    case Line of
		[FirstChar | _RestChars] when FirstChar =< 32 ->
		    % Whitespace-led header line: continuation line
		    if
			Key =:= none ->
			    exit({invalid_continuation_header_line, Line});
			true ->
			    parse(Rest, Key, Value ++ strip_crlf(Line), Message)
		    end;
		_ ->
		    {NewKey, NewValue} = split_header(Line),
		    parse(Rest, NewKey, NewValue, finish_header(Key, Value, Message))
	    end
    end.
