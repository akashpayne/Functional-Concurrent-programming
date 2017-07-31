-module(foo).   

-export([foo/1]).

foo(0) -> 37;
foo(N) -> 42. 

bar([N|Ns])  -> N.

