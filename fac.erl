-module(fac).
-export([fac/1]).

fac(N) -> 
    fac(N,1).

fac(0,P) ->
    P;
fac(N,P) ->
    fac(N-1,P*N). 
