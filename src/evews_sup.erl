
-module(evews_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args},permanent, 2000, Type, [I]}).


%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    WsHandle = {loop, loop},
    supervisor:start_link({local, ?MODULE}, ?MODULE, [WsHandle]).


%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([WsHandler]) ->
    Port = case application:get_env(evews, port) of
               {ok, PortWs} -> 
	            PortWs;
	       _          ->
		    8091
	    end,
    Args = [Port, {evews_acceptor, ws_loop}, WsHandler],
    io:format("Args ~p \n", [Args]),
    Childs = [?CHILD(evews_socket, worker, Args)],
    {ok, { {one_for_all, 1000, 3600}, Childs} }.

