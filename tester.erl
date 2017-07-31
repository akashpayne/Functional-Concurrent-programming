-module(tester).

-export([foo/1, prop_foo/0]).

-include_lib("eqc/include/eqc.hrl").

foo(X) -> X+1.

prop_foo() ->
	?FORALL(Y,nat(), foo(Y) -- 1+Y).
