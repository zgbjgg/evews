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
-module(evews_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args},permanent, 2000, Type, [I]}).

-include("evews.hrl").

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Options) ->
    Port = proplists:get_value(port, Options),
    [{callback_m, M}, {callback_f, F}] = proplists:get_value(ws_handler, Options),
    WsHandle = {M, F},
    SockMode = proplists:get_value(ssl, Options, tcp),
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Port, WsHandle, SockMode]).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([Port, WsHandler, SockMode]) ->
    {Args, _Mode} = case SockMode of
	               tcp   ->
	                   {[Port, {evews_acceptor, ws_loop}, WsHandler, tcp], tcp};
	       	       _     ->
		   	   {[Port, {evews_acceptor_ssl, ws_ssl_loop}, WsHandler, SockMode], ssl}
	    	   end,
    ?LOG_DEBUG("starting evews_sup on port: ~p, ws_handler: ~p \nMode ~p", [Port, WsHandler, _Mode]),
    Childs = [?CHILD(evews_socket, worker, Args)],
    {ok, { {one_for_all, 1000, 3600}, Childs} }.
