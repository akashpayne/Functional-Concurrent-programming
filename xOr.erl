-module(xOr).
-compile([export_all]).

% How many ways can you define xOr?
% Here are eight suggestions ...

xOr1(true,false) ->
    true;
xOr1(false,true) ->
    true;
xOr1(_,_) ->
    false. 

xOr2(X,X) ->
    false;
xOr2(_,_) ->
    true.

xOr3(X,Y) ->
    X =/= Y.

xOr4(X,Y) ->
    Or = (X or Y),
    And = (X and Y),
    Or and (not And).

xOr5(X,Y) ->
    (X orelse Y) andalso
	not (X andalso Y).

xOr6(true,Y) ->
    not Y;
xOr6(false,Y) ->
    Y.

xOr7(X,Y) ->
    not (X == Y).

xOr8(true,true) ->
    false; 
xOr8(true,false) ->
    true;
xOr8(false,true) ->
    true;
xOr8(false,false) ->
    false.



