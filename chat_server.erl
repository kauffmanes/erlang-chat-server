%% @author kauff
%% @doc @todo Add description to s.

-module(chat_server).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, start/1, stop/1]).

% start the server
start() -> start(8080).
start(Port) ->
	
	io:format("Starting up chat server...~n"),
	
	% create a dictionary to hold all connections (usernames and sockets)
	ClientDict = dict:new(),
	PidDict = dict:new(),
	
	% create a dictionary handler for inserts and lookups
	DictPid = spawn_link(fun() -> dict_handler(ClientDict, PidDict) end),
	
	% hand over handling of listeners to listener module "l"
	chat_listener:listen(Port, DictPid).

dict_handler(ClientDict, PidDict) ->
	receive
		{add_new_client, Socket, Username} ->
				io:format("Adding ~p~n", [Username]),
				TmpClients = dict:store(Username, Socket, ClientDict),
				TmpPids = dict:store(Socket, Username, PidDict),
				dict_handler(TmpClients, TmpPids);
		
		{get_client_pid, ReceiverPid, Username} ->
      {ok, Cpid} = dict:find(Username, ClientDict),
      ReceiverPid ! {pid, Cpid},
      dict_handler(ClientDict, PidDict);
		
		{get_username, ReceiverPid, Pid} ->
				{ok, ClientId} = dict:find(Pid, PidDict),
				ReceiverPid ! {client_id, ClientId},
				dict_handler(ClientDict, PidDict);
		
		{get_all_pids, ReceiverPid} ->
				ReceiverPid ! {all_pids, ClientDict},
				dict_handler(ClientDict, PidDict);
		
		% error handling
		_ ->
				{error, "That's not an acceptable action!"}
	end.

stop(Socket) ->
	gen_tcp:close(Socket).