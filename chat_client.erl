%% @author kauff
%% @doc @todo Add description to c.


-module(chat_client).
-define(TCP_OPTIONS, [binary, {packet, 2}, {active, false}, {reuseaddr, true}]).


%% ====================================================================
%% API functions
%% ====================================================================
-export([connect/0, connect/1, send/2, disconnect/1, recv/1]).

% create a connection the server's listening socket
connect() -> connect(8080).
connect(Port) ->
	{ok, Socket} = gen_tcp:connect("localhost", Port, ?TCP_OPTIONS),
	spawn(fun() -> recv(Socket) end),
	Socket.

% create a function that just handles incoming messages
recv(Socket) ->
	case gen_tcp:recv(Socket, 0) of
		{ok, Bin} ->
			Answer = binary_to_term(Bin),
			io:format("~p~n", [Answer]),
				recv(Socket);
		{error, Reason} ->
				io:format("Error: ~p~n", [Reason]),
				ok
	end.

% have ability to send a message
send(Socket, Message) ->
	% convert to binary before sending
	Bin = term_to_binary(Message),
	gen_tcp:send(Socket, Bin).

% have ability to leave chat (disconnect socket)
disconnect(Socket) ->
	gen_tcp:close(Socket).

%% ====================================================================
%% Internal functions
%% ====================================================================


