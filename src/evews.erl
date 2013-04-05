
-module(evews, [Socket]).

-export([socket/0, send/1, get/1, peername/0, port/0, sockname/0]).

socket() ->
    Socket.

send(Msg) ->
    gen_tcp:send(Socket, evews_handshake:send_format(Msg, 1, [])).

peername() ->
    inet:peername(Socket).

port() ->
    inet:port(Socket).

sockname() ->
    inet:sockname(Socket).

get(Data) ->
    evews_handshake:frame_r(Data).
