-module(week14).
-compile([export_all]).

% see xOr.erl for 8 different definitions of exclusive or.

maxThree(X,Y,Z) ->
  max(X,max(Y,Z)).

howManyEqual(A,A,A) -> 3;
howManyEqual(A,A,B) -> 1;
howManyEqual(A,B,A) -> 1;
howManyEqual(B,A,A) -> 1;
howManyEqual(_,_,_) -> 0.

%see rps.erl for rock-paper-scissors functions.



