
-module(evews_handshake).

-export([send_format/3,req_headers/1, key/1, res_headers/1, frame_r/1, handshake/2]).

frame_r(Hybi) ->
    % io:format("hybi ~p\n", [Hybi]),
    case Hybi of
        <<_Fin:1, Rsv1:1, Rsv2:1, Rsv3:1, Opcode:4, MaskBit:1, PayloadLen:7, MaskKey:4/binary, PayloadData:PayloadLen/binary-unit:8, Rest/binary>> when PayloadLen < 126 ->
            %io:format("mask len less than 126 ~p ~p ~p \n", [MaskKey, PayloadLen, PayloadData]),
            %io:format("message ~p \n", [un(MaskKey, PayloadLen, PayloadData, 0, <<>>)]), Opcode;
            un(MaskKey, PayloadLen, PayloadData, 0, <<>>);
	<<_Fin:1, Rsv1:1, Rsv2:1, Rsv3:1, Opcode:4, MaskBit:1, 126:7, PayloadLen:16, MaskKey:4/binary, PayloadData:PayloadLen/binary-unit:8, Rest/binary>> ->
            %io:format("mask len equal to 126 ~p \n", [MaskBit]), Opcode;
            Opcode;
	<<_Fin:1, Rsv1:1, Rsv2:1, Rsv3:1, Opcode:4, MaskBit:1, 127:7, 0:1, PayloadLen:63, MaskKey:4/binary, PayloadData:PayloadLen/binary-unit:8, Rest/binary>> ->
            %io:format("mask len equal to 127 ~p \n", [MaskBit]), Opcode
    	    Opcode
    end.
    %% io:format("<<~p,~p,~p,~p,~p,~p,~p,~p,~p,~p \n", [Fin, Rsv1, Rsv2, Rsv3, Opcode, MaskBit, PayloadLen, MaskKey, PayloadData, Rest]).

un(_Key, 0, _, _, Acc) ->
   List = lists:reverse(binary_to_list(Acc)),
   list_to_binary(List);
un(Key, Len, <<X:8, Rest/binary>>, Y, Acc) ->
    U = X bxor ( binary:decode_unsigned(binary:part(Key, {Y rem 4, 1})) ),
    %% io:format("U ~p\n", [U]),
    un(Key, Len - 1, Rest, Y + 1, <<U:8, Acc/binary>>).

send_format(Data, OpCode, _State) ->
        BData = erlang:iolist_to_binary(Data),
        Len = erlang:size(BData),
        case is_integer(Len) of
            true when Len < 126 ->
                        <<1:1, 0:3, OpCode:4, 0:1, Len:7, BData/binary>>;
             true when Len < 65536 ->
                        <<1:1, 0:3, OpCode:4, 0:1, 126:7, Len:16, BData/binary>>;
             true ->
                        <<1:1, 0:3, OpCode:4, 0:1, 127:7, 0:1, Len:63, BData/binary>>
        end.

handshake(Socket, Data) ->
        Headers = evews_handshake:req_headers(Data),
        Accept = evews_handshake:key(proplists:get_value("Sec-WebSocket-Key", Headers)),
        gen_tcp:send(Socket, evews_handshake:res_headers(Accept)).

req_headers(Headers) when is_binary(Headers) ->
    [ _ | DecodeHeaders ] =  binary:split(Headers, [<<"\r\n">>], [global]),
    req_headers(DecodeHeaders);
req_headers([ <<>> | T]) ->
    req_headers(T);
req_headers([ H | T]) ->
    [ Header | Values ] = binary:split(H, [<<":">>], [global]),
    Value = lists:append([ binary_to_list(V) || V <- Values ]),
    [ {[ U || U <- binary_to_list(Header), U =/= 32], [ U || U <- Value, U =/= 32]} | req_headers(T) ];
req_headers([]) ->
    [].

key(Key) when is_list(Key) ->
    key(list_to_binary(Key));
key(Key) ->
    base64:encode_to_string(crypto:sha(<<Key/binary, "258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>)).

res_headers(Accept) ->
    ["HTTP/1.1 101 Switching Protocols\r\n",
     "Upgrade: websocket\r\n","Connection: Upgrade\r\n",
     "Sec-WebSocket-Accept: ", Accept, "\r\n\r\n"].
