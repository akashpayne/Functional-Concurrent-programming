% more things for week 19 terminal session (solution to last bit).

-module (wk19c).
-export ([sort/2, out_target0/0, out_target1/0, harness/0]).


sort (Target0, Target1) ->
	receive {in0, N0} -> N0 end,
	receive {in1, N1} -> N1 end,
	if
		(N0 < N1) ->
			Target0 ! {out0, N0},
			Target1 ! {out1, N1};
		true ->
			Target0 ! {out0, N1},
			Target1 ! {out1, N0}
	end,
	sort (Target0, Target1).


out_target0 () ->
	receive {out0, V} -> io:format ("Small: ~w~n", [V]) end,
	out_target0 ().


out_target1 () ->
	receive {out1, V} -> io:format ("Large: ~w~n", [V]) end,
	out_target1 ().


harness () ->
	T0 = spawn (?MODULE, out_target0, []),
	T1 = spawn (?MODULE, out_target1, []),
	S = spawn (?MODULE, sort, [T0,T1]),
	S.

