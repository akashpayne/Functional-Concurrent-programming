% more things for week 19 terminal session.

-module (wk19b).
-export ([adder/1, out_target/0, harness/0]).


adder (Target) ->
	receive {in0, N0} -> N0 end,
	receive {in1, N1} -> N1 end,
	Target ! {out, N0+N1},
	adder (Target).


out_target () ->
	receive {out, V} -> io:format ("Result: ~w~n", [V]) end,
	out_target ().


harness () ->
	T = spawn (?MODULE, out_target, []),
	A = spawn (?MODULE, adder, [T]),
	A.

