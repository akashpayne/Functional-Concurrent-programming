-module(first).
-export([double/1,triArea/3]).

double(X) ->
	times(X,X).

times(X,Y) ->
	X*Y.
 
triArea(A,B,C) ->
	S = (A+B+C)/2,
	math:sqrt(S*(S-A)*(S-B)*(S-C)).
