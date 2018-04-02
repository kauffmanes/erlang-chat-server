Homework 4
Emily Kauffman
emk103@pitt.edu

NOTES:

My chat_server file was getting too large to handle, so I keep all of the dict/basic server interface functions in the chat_server, and broke everything else out into the chat_listener to handle incoming connections and messaging.

Here's a usage example:

1. Open three Erlang consoles side-by-side.

2. In the first console`chat_server:start().` OR `chat_server:start(ENTER YOUR PORT).` if you want to use a specific port. Not entering a Port defaults to 8080.

3. In the second console:
	`Sock = chat_client:connect(SAME PORT SERVER IS STARTED ON).`
	`chat_client:send(Sock, {join, "emk103"}).

4. In the third console:
	`Sock = chat_client:connect(SAME PORT SERVER IS STARTED ON).`
	`chat_client:send(Sock, {join, "rlomotey"}).

5. In the fourth console:
	`Sock = chat_client:connect(SAME PORT SERVER IS STARTED ON).`
	`chat_client:send(Sock, {join, "guest"}).

6. In the second console:
	`chat_client:send(Sock, {broadcast, "hello everyone in the chat!"}). %% will send a message to everyone connected

7. In the third console:
	`chat_client:send(Sock, {send_to, guest, "hey what is your name?"}). %% will send a message to only the connected user with that ID
