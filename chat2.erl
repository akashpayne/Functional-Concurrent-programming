% chat2.erl -- simple chat server -- bit modified.
% Fred Barnes, University of Kent.

-module (chat2).
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
		{add_client, Name, Pid, Location} ->									% 6(a)
			case length (ets:lookup (Clients, Name)) of						% 8(b)
				0 ->										% 8(b)
					Pid ! {ok},								% 8(b)
					io:format ("New connection for username ~s from ~s~n", [Name, Location]),	% 6(a)
					ets:foldl (fun ({_, P}, _) -> P ! {said, "", "User " ++ Name ++ " joined the chat!"} end, false, Clients),	% 7
					ets:insert (Clients, {Name, Pid}),
					chat_run (Clients);
				_ ->										% 8(b)
					Pid ! {error},								% 8(b)
					chat_run (Clients)							% 8(b)
			end;											% 8(b)

		{del_client, Name, _Pid} ->
			ets:delete (Clients, Name),
			ets:foldl (fun ({_, P}, _) -> P ! {said, "", "User " ++ Name ++ " left the chat!"} end, false, Clients),	% 7
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
			if (length (Str) == 0) ->							% 8(a)
				gen_tcp:send (Sock, "invalid username, try again!\r\n"),		% 8(a)
				cli_init (Sock, CProc);							% 8(a)
			true ->										% 8(a)
				{ok, {Addr, Port}} = inet:peername (Sock),						% 6(a)
				CProc ! {add_client, Str, self (), io_lib:format ("~p:~p", [Addr, Port])},		% 6(a)
				receive										% 8(b)
					{ok} ->									% 8(b)
						random:seed (now ()),						% 9(a)
						cli_run (Sock, Str, CProc);
					{error} ->								% 8(b)
						gen_tcp:send (Sock, "username in use, try again!\r\n"),		% 8(b)
						cli_init (Sock, CProc)						% 8(b)
				end										% 8(b)
			end;										% 8(a)
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
			RVal = random:uniform (6) + 30,							% 9(b)
			CStr = io_lib:format ("\x1b[~p;1m~s: \x1b[0m", [RVal, Who]),			% 9(a), 9(b)
			gen_tcp:send (Sock, CStr ++ What ++ "\r\n"),					% 9(a)
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

