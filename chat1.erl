% chat1.erl -- simple chat server.
% Fred Barnes, University of Kent.

-module (chat1).
-export ([svr_start/1, chat_init/0, svr_listening/3]).


svr_start (Port) -> %{{{  starts the chat server on the specified `Port' (e.g. 4041).
	{ok, HostName} = inet:gethostname (),

	io:format ("chat server running on host: ~s  port: ~p~n", [HostName, Port]),

	case gen_tcp:listen (Port, [binary, {packet, 0}, {reuseaddr, true}, {active, true}]) of
		{ok, L} ->
			% start TCP acceptor and chat server, return chat-server PID.
			CP = spawn_link (?MODULE, chat_init, []),
			spawn_link (?MODULE, svr_listening, [L, CP, self ()]),
			CP;
		{error, Reason} ->
			io:format ("failed to listen on port ~p: ~p~n", [Port, Reason]),
			false
	end.

%}}}

chat_init () -> %{{{  initalises the chat server.
	CTab = ets:new (clients, [set, private]),
	chat_run (CTab).

%}}}
chat_run (Clients) -> %{{{  main chat server code.
	receive
		{add_client, Name, Pid} ->
			ets:insert (Clients, {Name, Pid}),
			chat_run (Clients);
		{del_client, Name, _Pid} ->
			ets:delete (Clients, Name),
			chat_run (Clients);
		{say, Name, Text} ->
			ets:foldl (fun ({_, P}, _) -> P ! {said, Name, Text} end, false, Clients),
			chat_run (Clients)
	end.
%}}}

cli_init (Sock, CProc) -> %{{{  called when a new connection is made, prompt for username.
	% send welcome message.
	{ok, HostName} = inet:gethostname (),
	gen_tcp:send (Sock, io_lib:format ("\r\n\r\nWelcome to the chat-server on host ~s\r\n", [HostName])),
	gen_tcp:send (Sock, "Please enter a username and press return...\r\n"),

	% wait for username -- arrives as a TCP packet
	receive
		{tcp, Sock, BinDat} ->
			% filter out non-printable characters, including newline.
			Str = lists:filter (fun (C) -> ((C >= 32) and (C < 127)) end, binary_to_list (BinDat)),
			% assume sensible ..;  tell chat server we're here.
			CProc ! {add_client, Str, self ()},
			cli_run (Sock, Str, CProc);
		{tcp_closed, Sock} ->
			% remote client disconnected.
			exit (normal)
	end.

%}}}
cli_run (Sock, Name, CProc) -> %{{{  main code for a client connection -- proxies messages from/to the socket.
	receive
		{tcp, Sock, BinDat} ->
			% filter out non-printable characters, including newline.
			Str = lists:filter (fun (C) -> ((C >= 32) and (C < 127)) end, binary_to_list (BinDat)),
			CProc ! {say, Name, Str},
			cli_run (Sock, Name, CProc);
		{tcp_closed, Sock} ->
			CProc ! {del_client, Name, self ()},
			exit (normal);

		{said, Who, What} ->
			gen_tcp:send (Sock, Who ++ ": " ++ What ++ "\r\n"),
			cli_run (Sock, Name, CProc)
	end.
%}}}

svr_listening (LSock, CProc, PPid) -> %{{{  listening socket process (accepts connections, spawns `cli_init').
	link (PPid),			% link to process that started the whole thing (chat_server_start).
	case gen_tcp:accept (LSock) of
		{ok, CSock} ->
			spawn_link (?MODULE, svr_listening, [LSock, CProc, PPid]),
			cli_init (CSock, CProc);
		{error, Reason} ->
			io:format ("failed to accept connection: ~p~n", [Reason]),
			exit (die)
	end.
			
%}}}

