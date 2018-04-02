%% @author kauff


-module(chat_listener).
-define(TCP_OPTIONS, [binary, {packet, 2}, {active, false}, {reuseaddr, true}]).
%% ====================================================================
%% API functions
%% ====================================================================
-export([listen/2]).

listen(Port, DictPid) ->
		% create a listener on a listening socket
		{ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
	
		% create an accept loop that waits for connections on this listening socket
		spawn_link(fun() -> accept(LSocket, DictPid) end),
		io:format("Listening on socket=~p~n", [LSocket]).
		
% loop accepts incoming connection, and then creates a new
	% process to handle the incoming packet
accept(LSocket, DictPid) ->
	{ok, Socket} = gen_tcp:accept(LSocket),
	Pid = spawn(fun() ->
					io:format("Connection accepted~n", []),
					loop(Socket, DictPid)
				end),
	gen_tcp:controlling_process(Socket, Pid),
	accept(LSocket, DictPid).

% this handler func will take the data, figure out which action needs done
% format it appropriately, and then call a send function to whoever
loop(Sock, DictPid) ->
	
	% in order to use this {tcp, socket, data} syntax, must be temp set to active
	inet:setopts(Sock, [{active, once}]),
	
	%define the format of connection data that accept will handle
	receive
		{tcp, Socket, Data} ->
			
			% figure out the data and what action needs done, and then 
			% send this info to the client dictionary to finish
			ProcessedData = process_data(Socket, Data, DictPid),
			
			% echo this back to the sender
			send_message(Socket, ProcessedData), 
			
			%repeat!
			loop(Socket, DictPid);
		
		% handling for if socket is closed
		{tcp_closed, Socket} ->
				io:format("~p left.~n", [Socket]);
		
		%handling for if there's an error
		{tcp_error, Socket, Reason} ->
				io:format("Error on socket ~p, Reason: ~p~n", [Socket, Reason])
		end.

%assumes the data will be sent as binary - possible add checking for stringss
% these handle all of the client actions, just sending a message to the group,
% or sending a message to an individual
process_data(Socket, Data, DictPid) ->
		
		case binary_to_term(Data) of
			
			% someone joins the chat room
			{join, Username} ->
					
					%tell dictionary to add the new client
					DictPid ! {add_new_client, Socket, Username};
			
			% we don't know the client's IDs, so we have to query the Dict to 
			% find a process ID and socket ID that matches the username we want
			{send_to, Username, Message} ->
					
				%first get sender's Pid
				DictPid ! {get_username, self(), Socket},
				receive
					{client_id, ClientId} ->
							Sender = ClientId;
					_ -> Sender = "Anonymous"
				end,
				
				FormattedMessage = lists:concat([Sender, "> ", Message]),
				
        DictPid ! {get_client_pid, self(), Username},
        receive
           {pid, SockPid} ->
            	 gen_tcp:send(SockPid, term_to_binary(FormattedMessage))
        end,
				term_to_binary({ok, delivered});
			
			%message everyone
			{broadcast, Message} ->
					
					%first get sender's Pid
					DictPid ! {get_username, self(), Socket},
					receive
						{client_id, ClientId} ->
								Sender = ClientId;
						_ -> Sender = "Anonymous"
					end,
					
					FormattedMessage = lists:concat([Sender, "> ", Message]),
				
					DictPid ! {get_all_pids, self()},
					receive
							{all_pids, Pids} ->
									dict:map(fun(K,V) ->
										io:format("Processing: ~w: ~w~n", [K,V]),
										gen_tcp:send(V, term_to_binary(FormattedMessage))end, Pids)
					end,
					term_to_binary({ok, delivered});
			
			_ -> {error, "That's not an option..."}
		
		end.
	
send_message(Socket, Bin) ->
		gen_tcp:send(Socket, Bin).
		
