% service2.erl -- client-server in Erlang
% Fred Barnes, University of Kent.

-module (service2).
-compile ([export_all]).


my_server () ->
	my_server ([]).

find_term ([], _) -> undefined;
find_term ([{N,T}|_], N) -> T;
find_term ([_|Xs], N) ->
	find_term (Xs, N).


find_term_f ([], _, _, F) -> F();
find_term_f ([{N,T}|_], N, F, _) -> F(T);
find_term_f ([_|Xs], N, FT, FF) ->
	find_term_f (Xs, N, FT, FF).


remove_term (I, N) ->
	remove_term (I, N, []).

remove_term ([], _, NI) -> NI;
remove_term ([{N,_}|Xs], N, NI) ->
	remove_term (Xs, N, NI);
remove_term ([T|Xs], N, NI) ->
	remove_term (Xs, N, [T|NI]).


my_server (Items) ->
	NewItems = receive
		{store, From, Name, Term} ->
			find_term_f (Items, Name,
				fun (_) -> From ! {error, self (), "Already got this!"}, Items end,
				fun () -> From ! {stored, self(), Name, Term}, [{Name,Term}|Items] end);

		{retrieve, From, Name} ->
			find_term_f (Items, Name,
				fun (T) -> From ! {retrieved, self (), T}, Items end,
				fun () -> From ! {error, self (), "No such name"}, Items end);

		{query, From, Name} ->
			find_term_f (Items, Name,
				fun (_) -> From ! {query, self (), true}, Items end,
				fun () -> From ! {query, self (), false}, Items end);

		{remove, From, Name} ->
			find_term_f (Items, Name,
				fun (_) -> From ! {removed, self (), Name}, remove_term (Items, Name) end,
				fun () -> From ! {error, self (), "No such name"}, Items end);

		Other ->
			io:format ("my_server(): got unhandled message ~p~n", [Other]),
			Items
	end,
	my_server (NewItems).


do_store (Name, Term) ->
	a_service ! {store, self (), Name, Term},
	receive
		{stored, _, _, T} -> T;
		{error, _, M} -> M
	end.

do_retrieve (Name) ->
	a_service ! {retrieve, self (), Name},
	receive
		{retrieved, _, T} -> T;
		{error, _, M} -> M
	end.

do_query (Name) ->
	a_service ! {query, self (), Name},
	receive
		{query, _, R} -> R
	end.

do_remove (Name) ->
	a_service ! {remove, self (), Name},
	receive
		{removed, _, N} -> N;
		{error, _, M} -> M
	end.

start_service () ->
	% try and unregister first (in case still running).
	try unregister (a_service) catch
		error: Any -> Any
	end,

	Pid = spawn_link (?MODULE, my_server, []),
	register (a_service, Pid),
	started.

