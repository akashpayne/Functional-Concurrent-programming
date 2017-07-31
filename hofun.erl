% hofun.erl -- higher order function fun
% Fred Barnes, University of Kent, March 2014

-module (hofun).
-compile ([export_all]).

is_circle ({circle,_,_}) -> true;
is_circle (_) -> false.

fapply1 (F, A) -> F(A).

test_gt (N) -> fun (X) -> (X > N) end.

