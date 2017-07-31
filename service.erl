% service.erl -- client-server in Erlang
% Fred Barnes, University of Kent.

-module (service).
-compile ([export_all]).


my_server () ->
	my_server ([]).

find_term ([], _) -> undefined;
find_term ([{N,T}|_], N) -> T;
find_term ([_|Xs], N) ->
	find_term (Xs, N).

remove_term (I, N) ->
	remove_term (I, N, []).

remove_term ([], _, NI) -> NI;
remove_term ([{N,_}|Xs], N, NI) ->
	remove_term (Xs, N, NI);
remove_term ([T|Xs], N, NI) ->
	remove_term (Xs, N, [T|NI]).


my_server (Items) ->
	receive
		{store, From, Name, Term} ->
			R = find_term (Items, Name),
			if (R =:= undefined) ->
				From ! {stored, self (), Name, Term},
				my_server ([{Name, Term} | Items]);
			true ->
				From ! {error, self (), "Already got this!"},
				my_server (Items)
			end;

		{retrieve, From, Name} ->
			R = find_term (Items, Name),
			if (R =:= undefined) ->
				From ! {error, self (), "No such name"};
			true ->
				From ! {retrieved, self (), R}
			end,
			my_server (Items);

		{query, From, Name} ->
			R = find_term (Items, Name),
			if (R =:= undefined) ->
				From ! {query, self (), false};
			true ->
				From ! {query, self (), true}
			end,
			my_server (Items);

		{remove, From, Name} ->
			R = find_term (Items, Name),
			if (R =:= undefined) ->
				From ! {error, self (), "No such name"},
				my_server (Items);
			true ->
				From ! {removed, self (), Name},
				NewItems = remove_term (Items, Name),
				my_server (NewItems)
			end;
		Other ->
			io:format ("my_server(): got unhandled message ~p~n", [Other]),
			my_server (Items)
	end.


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

