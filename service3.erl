% service3.erl -- client-server in Erlang
% Fred Barnes, University of Kent.

-module (service3).
-compile ([export_all]).


my_server () ->
	my_server ([]).

% searches through the given list for a particular matching name (as in {name,Term}).
% returns either `undefined' (atom) or the term. -- obviously this means storing the
% Term `undefined' is going to result in confusion!
%
find_term ([], _) -> undefined;
find_term ([{N,T}|_], N) -> T;
find_term ([_|Xs], N) ->
	find_term (Xs, N).


% searches through the given list for a particular matching name (as in {name,Term}).
% also takes two functions (FT and FF) for "found true" and "found false" that are
% evaluated (and the results returned) in the event of finding, or not finding, the
% specified name.
%
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


% the server process.  For 'store', this expects clients to send two functions with
% the store request: one that is evaluated if the {Name,Term} is stored successfully (FStored)
% and another that is evaluated otherwise (because that name was already present).
%
% doing it for the others is an exercise for the reader!
%
my_server (Items) ->
	NewItems = receive
		{store, Name, Term, FStored, FGot} ->
			find_term_f (Items, Name,
				fun (_) -> FGot (), Items end,
				fun () -> FStored (), [{Name,Term}|Items] end);

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


% the client functions.  Only `store' uses higher-order functions here, essentially instructing
% the server how to reply to the request.  This means we don't explicitly pass our own PID when
% making the request, as that is embedded as part of the response function.  Note that the `self()'
% inside the `fun's here is evaluated *in the server*, so will produce the server's PID, not ours!
% (this is why we do "Me = self()" first, and then put "Me" inside the `fun').
%
do_store (Name, Term) ->
	Me = self (),
	a_service ! {store, Name, Term,
			fun () -> Me ! {stored, self (), Term} end,
			fun () -> Me ! {error, self (), "Already got!"} end},
	receive
		{stored, _, T} -> T;
		{error, P, M} -> io:format ("Message from server ~p: ~p~n", [P, M])
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

