% ==========================================================================================================
% 
% EvE ws
%
% Copyright (C) 2013, Jorge Garrido <zgbjgg@gmail.com>. 
% All rights reserved.
%
% BSD License
%
% Redistribution and use in source and binary forms, with or without modification, are permitted provided
% that the following conditions are met:
%
%  * Redistributions of source code must retain the above copyright notice, this list of conditions and the
%        following disclaimer.
%  * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and
%        the following disclaimer in the documentation and/or other materials provided with the distribution.
%  * Neither the name of the authors nor the names of its contributors may be used to endorse or promote
%        products derived from this software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED
% WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
% PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
% ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
% TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
% POSSIBILITY OF SUCH DAMAGE.
% ==========================================================================================================
-module('evews_rfc-6455').

%% API
-export([handshake/1, frame/1, unmask/5, format_msg/2, check_opcode/1]).

%% includes
-include("evews.hrl").

%% @doc Returns the handshake for the websocket.
%% @spec handshake(Request :: binary()) -> list()
-spec handshake(Request :: binary()) -> list().
handshake(Request) ->
        Headers = headers_as_plist(Request),
        ?LOG_DEBUG("headers request: ~p", [Headers]),
	Accept = key_accept(proplists:get_value("Sec-WebSocket-Key", Headers)),
	headers_handshake_response(Accept).
        %% gen_tcp:send(Socket, evews_handshake:res_headers(Accept)).

%% @doc Decode the requesting handshake.
%% @spec headers_as_plist(Headers :: binary() | list()) -> list()
-spec headers_as_plist(Headers :: binary() | list())  -> list().
headers_as_plist(Headers) when is_binary(Headers) ->
    [ _ | DecodeHeaders ] =  binary:split(Headers, [<<"\r\n">>], [global]),
    headers_as_plist(DecodeHeaders);
headers_as_plist([ <<>> | Remaining])             ->
    headers_as_plist(Remaining);
headers_as_plist([ H | Remaining ])               ->
    [ Header | Values ] = binary:split(H, [<<":">>], [global]),
    Value = lists:append([ binary_to_list(V) || V <- Values ]),
    [ {[ U || U <- binary_to_list(Header), U =/= 32], 
       [ U || U <- Value, U =/= 32]} | headers_as_plist(Remaining) ];
headers_as_plist([])                              ->
    [].

%% @doc Returns the accept key on the handshake response.
%% @spec key_accept(Key :: list() | binary()) -> string()
-spec key_accept(Key :: list() | binary()) -> string().
key_accept(Key) when is_list(Key) ->
    key_accept(list_to_binary(Key));
key_accept(Key) 		  ->
    base64:encode_to_string(crypto:sha(<<Key/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>)).

%% @doc Handshake headers response.
%% @spec headers_handshake_response(Accept :: string()) -> list()
-spec headers_handshake_response(Accept :: string()) -> list().
headers_handshake_response(Accept) ->
    ["HTTP/1.1 101 Switching Protocols\r\n",
     "Upgrade: websocket\r\n","Connection: Upgrade\r\n",
     "Sec-WebSocket-Accept: ", Accept, "\r\n\r\n"].

%% Returns the decoded frame as proplist.
%% @spec frame(Frame :: binary()) -> list()
-spec frame(Frame :: binary()) -> list().
frame(Frame) ->
    case Frame of
        <<Fin:1, Rsv1:1, 
	  Rsv2:1, Rsv3:1, 
	  Opcode:4, MaskBit:1, 
	  PayloadLen:7, MaskKey:4/binary, 
	  PayloadData:PayloadLen/binary-unit:8, 
	  Rest/binary>> when PayloadLen < 126 ->
	    ?FRAME(Fin, Rsv1, Rsv2, Rsv3, Opcode, MaskBit, PayloadLen, MaskKey, PayloadData, Rest);
	<<Fin:1, Rsv1:1, 
	  Rsv2:1, Rsv3:1, 
	  Opcode:4, MaskBit:1, 126:7, 
	  PayloadLen:16, MaskKey:4/binary, 
	  PayloadData:PayloadLen/binary-unit:8, 
	  Rest/binary>>                       ->
            ?FRAME(Fin, Rsv1, Rsv2, Rsv3, Opcode, MaskBit, PayloadLen, MaskKey, PayloadData, Rest);
	<<Fin:1, Rsv1:1, 
	  Rsv2:1, Rsv3:1, 
	  Opcode:4, MaskBit:1, 127:7, 0:1, 
	  PayloadLen:63, MaskKey:4/binary, 
	  PayloadData:PayloadLen/binary-unit:8, 
	  Rest/binary>> 		      ->
            ?FRAME(Fin, Rsv1, Rsv2, Rsv3, Opcode, MaskBit, PayloadLen, MaskKey, PayloadData, Rest);
	_ 				      ->
	    []
    end.

%% @doc Returns the message in the frame by unmask it.
%% @spec unmask(Key :: binary(), Len :: integer(), binary(), Y :: integer(), Acc :: binary) -> binary()
-spec unmask(Key :: binary(), Len :: integer(), binary(), Y :: integer(), Acc :: binary) -> binary().
unmask(_Key, 0, _, _, Acc) ->
   List = lists:reverse(binary_to_list(Acc)),
   ?LOG_DEBUG("unmask: ~p", [List]),
   list_to_binary(List);
unmask(Key, Len, <<X:8, Rest/binary>>, Y, Acc) ->
    U = X bxor ( binary:decode_unsigned(binary:part(Key, {Y rem 4, 1})) ),
    unmask(Key, Len - 1, Rest, Y + 1, <<U:8, Acc/binary>>).

%% @doc Returns the message formatted as RFC-6455 spec.
%% @spec format_msg(Data :: iolist(), OpCode :: integer()) -> binary()
-spec format_msg(Data :: iolist(), OpCode :: integer()) -> binary().
format_msg(Data, OpCode) ->
    BinData = erlang:iolist_to_binary(Data),
    Len = erlang:size(BinData),
    case is_integer(Len) of
        true when Len < 126 ->
	     ?LOG_DEBUG("sending format message: <<1:1, 0:3, ~p:4, 0:1, ~p:7, ~p/binary>>", [OpCode, Len, BinData]),
             <<1:1, 0:3, OpCode:4, 0:1, Len:7, BinData/binary>>;
        true when Len < 65536 ->
	     ?LOG_DEBUG("sending format message: <<1:1, 0:3, ~p:4, 0:1, 126:7, ~p:16, ~p/binary>>", [OpCode, Len, BinData]),
             <<1:1, 0:3, OpCode:4, 0:1, 126:7, Len:16, BinData/binary>>;
        true ->
	     ?LOG_DEBUG("sending format message: <<1:1, 0:3, ~p:4, 0:1, 127:7, 0:1, ~p:63, ~p/binary>>", [OpCode, Len, BinData]),
             <<1:1, 0:3, OpCode:4, 0:1, 127:7, 0:1, Len:63, BinData/binary>>
    end.

%% @doc Checks for a valid opcode in the frame.
%% @spec check_opcode(integer() | any()) -> cont | text | bin | close | ping | pong | any()
-spec check_opcode(integer() | any()) -> cont | text | bin | close | ping | pong | any().
check_opcode(0) -> cont;
check_opcode(1) -> text;
check_opcode(2) -> bin;
check_opcode(8) -> close;
check_opcode(9) -> ping;
check_opcode(10) -> pong;
check_opcode(_)  -> error_grave_null.
