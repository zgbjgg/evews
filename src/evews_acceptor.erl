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
-module(evews_acceptor).

%% API
-export([accept/1, accept_ws/1, ws_loop/2]).

%% includes
-include("evews.hrl").

%% records
-record(state, {port, handler, lsocket, ws_handler}).

%% @doc Accepts the requesting connection over TCP.
%% @spec accept(State :: record()) -> record()
-spec accept(State :: record()) -> record().
accept(State = #state{lsocket=LSocket, handler=Handler, ws_handler=WsHandler}) ->
    proc_lib:spawn(?MODULE, accept_ws, [{self(), LSocket, Handler, WsHandler}]),
    State.

%% @doc Spawns the accepting connection to the handler ws loop.
%% @spec accept_ws({ Server :: pid(), LSocket :: port(), {ModuleH :: atom(), FunH :: atom()}, WsHandler :: tuple()}) -> any()
-spec accept_ws({ Server :: pid(), LSocket :: port(), {ModuleH :: atom(), FunH :: atom()}, WsHandler :: tuple()}) -> any().
accept_ws({Server, LSocket, {ModuleH, FunH}, WsHandler}) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    gen_server:cast(Server, {accepted, self()}),
    ?LOG_DEBUG("accepting connection on lsocket: ~p", [LSocket]),
    ModuleH:FunH(Socket, WsHandler).

%% @doc Process to handshake the headers in websocket connection.
%% @spec ws_loop(Socket :: port(), {CallbackWsModule :: atom(), CallbackWsFun :: atom()}) -> any()
-spec ws_loop(Socket :: port(), {CallbackWsModule :: atom(), CallbackWsFun :: atom()}) -> any().
ws_loop(Socket, {CallbackWsModule, CallbackWsFun}) ->
    ok = inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data}          ->
	    Handshake = 'evews_rfc-6455':handshake(Data),
	    ?LOG_DEBUG("sending handshake: ~p", [Handshake]),
            ok = gen_tcp:send(Socket, Handshake),
	    Ws = evews:new(Socket), 
	    Pid = spawn_link(fun() -> CallbackWsModule:CallbackWsFun(Ws) end),
	    callback_ws_loop(Socket, Pid);
        {tcp_closed, _}              ->
	    ?LOG_DEBUG("socket closed: ~p", [Socket]),
	    closed;
        {tcp_error, _Socket, Reason} ->
	    ?LOG_DEBUG("error on ~p reason: ~p", [Socket, Reason]),		
            Reason
    end.

%% @doc Process to handle in/out data over websocket
%% @spec callback_ws_loop(Socket :: port(), Pid :: pid()) -> any()
-spec callback_ws_loop(Socket :: port(), Pid :: pid()) -> any().
callback_ws_loop(Socket, Pid) ->
    ok = inet:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data}  ->
	    Frame = 'evews_rfc-6455':frame(Data),
	    case proplists:get_value(opcode, Frame) of
	        8 ->
		    ?LOG_DEBUG("closing ~p by opcode: ~p", [Socket, 8]),
		    exit(Pid, kill), 
		    gen_tcp:close(Socket);
		_ ->
	    	    Pid ! {browser, Data},
	    	    callback_ws_loop(Socket, Pid)
	    end;
	{tcp_closed, Socket} ->
 	    Pid ! {browser_closed, self()};
	{send, _Data}        ->
 	    % gen_tcp:send(Socket, [0,Data,255]),
 	    callback_ws_loop(Socket, Pid);
	close                ->
 	    gen_tcp:close(Socket);
	Any                  ->
 	    io:format("Received:~p~n",[Any]),
 	    callback_ws_loop(Socket, Pid)
    end.
