% racey2.erl -- race-hazard test
% Fred Barnes, March 2014.

-module (racey2).
-compile ([export_all]).


val_svr (V) ->
	receive
		{set_val, NewV} ->
			val_svr (NewV);
		{get_val, Pid} ->
			Pid ! {val, V},
			val_svr (V)
	end.

increment (Svr, Mtx) ->
	mutex_lock (Mtx),
	Svr ! {get_val, self ()},
	X = receive {val, N} -> N end,

	Svr ! {set_val, X+1},
	mutex_unlock (Mtx).

test_loop (_, _, 0) ->
	true;
test_loop (Svr, Mtx, N) ->
	increment (Svr, Mtx),
	test_loop (Svr, Mtx, N-1).

test_proc (VS, Mtx, Parent) ->
	test_loop (VS, Mtx, 10000),
	Parent ! done.


sema4 () ->
	sema4 (0, []).
sema4 (N) ->
	sema4 (N, []).
sema4 (N, WSet) ->
	receive
		{signal} when (WSet == []) ->
			sema4 (N+1, WSet);
		{signal} when (WSet /= []) ->
			P = hd (WSet),
			P ! {waited},
			sema4 (N, tl (WSet));
		{wait, Pid} when (N > 0) ->
			Pid ! {waited},
			sema4 (N-1, WSet);
		{wait, Pid} when (N == 0) ->
			sema4 (N, WSet ++ [Pid])
	end.

mutex_create () ->
	spawn_link (?MODULE, sema4, [1]).
mutex_lock (M) ->
	M ! {wait, self()},
	receive {waited} -> true end.
mutex_unlock (M) ->
	M ! {signal}.


harness () ->
	VS = spawn_link (?MODULE, val_svr, [0]),
	M = mutex_create (),

	spawn_link (?MODULE, test_proc, [VS, M, self ()]),
	receive done -> true end,

	mutex_lock (M),
	VS ! {get_val, self()},
	X = receive {val, N} -> N end,
	mutex_unlock (M),

	io:format ("after test_proc, value = ~w~n", [X]),

	% and then in parallel
	spawn_link (?MODULE, test_proc, [VS, M, self ()]),
	spawn_link (?MODULE, test_proc, [VS, M, self ()]),
	spawn_link (?MODULE, test_proc, [VS, M, self ()]),
	spawn_link (?MODULE, test_proc, [VS, M, self ()]),
	spawn_link (?MODULE, test_proc, [VS, M, self ()]),

	% wait for three 'done' messages
	receive done -> receive done -> receive done -> receive done -> receive done -> true end end end end end,

	mutex_lock (M),
	VS ! {get_val, self()},
	Y = receive {val, Q} -> Q end,
	mutex_unlock (M),

	io:format ("after two more test_procs, value = ~w~n", [Y]).


