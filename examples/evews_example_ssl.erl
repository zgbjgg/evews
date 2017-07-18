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
-module(evews_example_ssl).

-export([start/1, loop/1]).

%% starts the evews websocket server on the given port
start(Port) ->
    ok = application:start(asn1),
    ok = application:start(crypto),
    ok = application:start(public_key),
    ok = application:start(ssl),
    %% We need port, but also the callback module (in this case 'evews_module') and the callback function
    %% on the module (in this case 'loop').
    %% SSL needs the certfile and keyfile (absolute paths) and password if any.
    evews_sup:start_link([{port, Port}, {ws_handler, [{callback_m, ?MODULE}, {callback_f, loop}]}, 
			  {ssl, [{certfile, "certfile.pem"}, {keyfile, "keyfile.pem"}, {password, "password"}]}]).

%% receive and sends messages with the process spawned on start_link
%% WsInfo contains all info about socket and let send/receive messages.
%% Ws is the callback module
%% This process terminates when disconnect from browser is detected!.
loop({Ws, WsInfo}) ->
    receive
	%% this messages is received from the browser, Data is a binary that can be decoded with 
	%% Ws:get(Data) to get only the message.
	{browser, Data} ->
	    io:format("receive ~p\n", [Ws:get(Data)]),
	    loop({Ws, WsInfo});
	Any ->
	    io:format("any ~p\n", [Any]),
            loop({Ws, WsInfo})
	%% after one second sends a message to the browser, then use Ws:send(["echo!"]) 
        after 1000 ->
	    Ws:send(["echo!"], WsInfo),
	    loop({Ws, WsInfo})
    end.
