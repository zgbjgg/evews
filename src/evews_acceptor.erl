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
-export([accept/1, accept_ws/1, ws_loop/5]).

%% includes
-include("evews.hrl").

%% records
-record(state, {port, handler, lsocket, ws_handler, mode}).

%% @doc Accepts the requesting connection over TCP.
%% @spec accept(State :: record()) -> record()
-spec accept(State :: record()) -> record().
accept(State = #state{lsocket=LSocket, handler=Handler, ws_handler=WsHandler, mode=_Mode}) ->
    proc_lib:spawn_link(?MODULE, accept_ws, [{self(), LSocket, Handler, WsHandler}]),
    State.

%% @doc Spawns the accepting connection to the handler ws loop.
%% @spec accept_ws({ Server :: pid(), LSocket :: port(), {ModuleH :: atom(), FunH :: atom()}, WsHandler :: tuple()}) -> any()
-spec accept_ws({ Server :: pid(), LSocket :: port(), {ModuleH :: atom(), FunH :: atom()}, WsHandler :: tuple()}) -> any().
accept_ws({Server, LSocket, {ModuleH, FunH}, WsHandler}) ->
    case catch gen_tcp:accept(LSocket) of
        {ok, Socket} ->
    	    % gen_server:cast(Server, {accepted, self()}),
    	    ?LOG_DEBUG("accepting connection on lsocket: ~p and spawning process", [LSocket]),
	    Pid = spawn(fun() -> 
			    ws_loop(Socket, WsHandler, down, none, none)
			end),

	    ok = gen_tcp:controlling_process(Socket, Pid),	
	   
	    Pid ! set,
 
	    % go back to accept_ws
	    ?MODULE:accept_ws({Server, LSocket, {ModuleH, FunH}, WsHandler});
	_            ->
	    gen_tcp:close(LSocket)
    end.

%% @doc Process to handshake the headers in websocket connection.
%% @spec ws_loop(Socket :: port(), {CallbackWsModule :: atom(), CallbackWsFun :: atom()}) -> any()
%-spec ws_loop(Socket :: port(), {CallbackWsModule :: atom(), CallbackWsFun :: atom()}) -> any().
ws_loop(Socket, {CallbackWsModule, CallbackWsFun}, Status, Ref, Pid) ->
    receive
        {tcp, Socket, Data}          ->
	    case maybe_do_a_handshake({Status, Data, Socket, {CallbackWsModule, CallbackWsFun}, Pid}) of
	        {gen_tcp, close}               -> 
		    gen_tcp:close(Socket);
		{?MODULE, ws_loop, RefN, PidN} -> 
		    ok = inet:setopts(Socket, [{active, once}]),
		    ws_loop(Socket, {CallbackWsModule, CallbackWsFun}, up, RefN, PidN);
		{?MODULE, ws_loop}             -> 
		    ok = inet:setopts(Socket, [{active, once}]),
		    ws_loop(Socket, {CallbackWsModule, CallbackWsFun}, up, Ref, Pid)
	    end;	    
        {tcp_error, _Socket, Reason} ->
	    ?LOG_DEBUG("error on ~p reason: ~p", [Socket, Reason]),		
            Reason;
        {tcp_closed, Socket}         ->
            Pid ! {browser_closed, self()};
        {send, _Data}                ->
	    ok = inet:setopts(Socket, [{active, once}]),
            ws_loop(Socket, {CallbackWsModule, CallbackWsFun}, Status, Ref, Pid);
        {'DOWN', Ref, _, _, _}       ->
            gen_tcp:close(Socket);
        close                        ->
            gen_tcp:close(Socket);
	set			     ->
            ok = inet:setopts(Socket, [{active, once}]),  
 	    ws_loop(Socket, {CallbackWsModule, CallbackWsFun}, Status, Ref, Pid)
    end.

%% @doc Maybe a handshake or a process frame
%% @spec maybe_do_a_handshake({down | up, Data :: binary(), tuple(), Pid :: pid()}) -> tuple()
-spec maybe_do_a_handshake({down | up, Data :: binary(), tuple(), Pid :: pid()}) -> tuple().
maybe_do_a_handshake({down, Data, Socket, {CallbackWsModule, CallbackWsFun}, _}) ->
    Handshake = 'evews_rfc-6455':handshake(Data),
    ?LOG_DEBUG("handshake: ~p", [Handshake]),
    ok = gen_tcp:send(Socket, Handshake),
    WsInfo = evews:init(Socket, tcp),
    Pid = spawn_link(fun() -> CallbackWsModule:CallbackWsFun({evews, WsInfo}) end),
    Ref = erlang:monitor(process, Pid),
   {?MODULE, ws_loop, Ref, Pid};    
maybe_do_a_handshake({up, Data, _Socket, _, Pid})   -> 
    Frame = 'evews_rfc-6455':frame(Data, <<>>, tcp),
    case proplists:get_value(opcode, Frame) of
        8 ->
            ?LOG_DEBUG("closing ~p by opcode: ~p", [_Socket, 8]),
            erlang:unlink(Pid),
            exit(Pid, kill),
            {gen_tcp, close};
        _ ->
            Pid ! {browser, Frame},
            {?MODULE, ws_loop}
     end.
