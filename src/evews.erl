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
-module(evews).

%% API
-export([init/2, socket/1, send/2, get/1, peername/1, port/1, sockname/1]).

%% includes
-include("evews.hrl").

%% record for store the data module
%% supress the error: parametrised modules are no longer availables
-record(ws_info, { socket, mode }).

%% @doc Dump values into record
%% @spec init(Socket :: port(), Mode :: atom()) -> atom()
-spec init(Socket :: port(), Mode :: atom()) -> atom().
init(Socket, Mode) ->
    #ws_info{ socket = Socket, mode = Mode}.

%% @doc Returns the port for this socket.
%% @spec socket(WsInfo :: tuple()) -> port()
-spec socket(WsInfo :: tuple()) -> port.
socket(WsInfo) ->
    ?LOG_DEBUG("socket is: ~p", [WsInfo#ws_info.socket]),
    WsInfo#ws_info.socket.

%% @doc Sends message to the websocket client, encoding as format frame
%%	requires over this socket.
%% @spec send(Msg :: iolist(), WsInfo :: tuple()) -> ok | {error, Reason :: term()}
-spec send(Msg :: iolist(), WsInfo :: tuple()) -> ok | {error, Reason :: term()}.
send(Msg, WsInfo) ->
    FormatMsg = 'evews_rfc-6455':format_msg(Msg, 1),
    ?LOG_DEBUG("sending: ~p", [FormatMsg]),
    case WsInfo#ws_info.mode of
        tcp ->
	    gen_tcp:send(WsInfo#ws_info.socket, FormatMsg);
        ssl ->
	    ssl:send(WsInfo#ws_info.socket, FormatMsg)
    end.

%% @doc Returns the address and port for the other end of a connection.
%% @spec peername(WsInfo :: tuple()) -> {ok, {Address::tuple(), Port :: integer()}} | {error, term()}
-spec peername(WsInfo :: tuple()) -> {ok, {Address::tuple(), Port :: integer()}} | {error, term()}.
peername(WsInfo) ->
    ?LOG_DEBUG("socket peername: ~p", [inet:peername(WsInfo#ws_info.socket)]),
    inet:peername(WsInfo#ws_info.socket).

%% @doc Returns the local port number of this socket
%% @spec port(WsInfo :: tuple()) -> {ok, Port :: integer()} | {error, any()}
-spec port(WsInfo :: tuple()) -> {ok, Port :: integer()} | {error, any()}.
port(WsInfo) ->
    ?LOG_DEBUG("socket port: ~p", [inet:port(WsInfo#ws_info.socket)]),
    inet:port(WsInfo#ws_info.socket).

%% @doc Returns the local address and port number of this socket.
%% @spec sockname(WsInfo :: tuple()) -> {ok, {Address::tuple(), Port :: integer()}} | {error, term()}
-spec sockname(WsInfo :: tuple()) -> {ok, {Address::tuple(), Port :: integer()}} | {error, term()}.
sockname(WsInfo) ->
    ?LOG_DEBUG("sockname: ~p", [inet:sockname(WsInfo#ws_info.socket)]),
    inet:sockname(WsInfo#ws_info.socket).

%% @doc Gets the data in the frame sent by the websocket client.
%% @spec get(Frame :: list()) -> binary()
-spec get(Frame :: list()) -> binary().
get(Frame) ->
    MaskBit = proplists:get_value(mask_bit, Frame), 
    ?LOG_DEBUG("maskbit: ~p", [MaskBit]),
    case MaskBit of
        1 ->
	    MaskKey = proplists:get_value(mask_key, Frame),
	    PayloadLen = proplists:get_value(payload_len, Frame),
	    PayloadData = proplists:get_value(payload_data, Frame),
	    'evews_rfc-6455':unmask(MaskKey, PayloadLen, PayloadData, 0, <<>>);
	_ ->
	    proplists:get_value(payload_data, Frame)
    end.
