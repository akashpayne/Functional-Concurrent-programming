% racey.erl -- race-hazard test
% Fred Barnes, March 2014.

-module (racey).
-compile ([export_all]).


val_svr (V) ->
	receive
		{set_val, NewV} ->
			val_svr (NewV);
		{get_val, Pid} ->
			Pid ! {val, V},
			val_svr (V)
	end.

increment (Svr) ->
	Svr ! {get_val, self ()},
	X = receive {val, N} -> N end,

	Svr ! {set_val, X+1}.

test_loop (_, 0) ->
	true;
test_loop (Svr, N) ->
	increment (Svr),
	test_loop (Svr, N-1).

test_proc (VS, Parent) ->
	test_loop (VS, 1000),
	Parent ! done.

harness () ->
	VS = spawn_link (?MODULE, val_svr, [0]),

	spawn_link (?MODULE, test_proc, [VS, self ()]),
	receive done -> true end,

	VS ! {get_val, self()},
	X = receive {val, N} -> N end,

	io:format ("after test_proc, value = ~w~n", [X]),

	% and then in parallel
	spawn_link (?MODULE, test_proc, [VS, self ()]),
	spawn_link (?MODULE, test_proc, [VS, self ()]),
	spawn_link (?MODULE, test_proc, [VS, self ()]),

	% wait for three 'done' messages
	receive done -> receive done -> receive done -> true end end end,

	VS ! {get_val, self()},
	Y = receive {val, M} -> M end,

	io:format ("after two more test_procs, value = ~w~n", [Y]).


