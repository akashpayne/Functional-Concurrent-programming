-module(week16).
-compile([export_all]).

doubleAll([]) -> [];
doubleAll([X|Xs]) -> 
    [ 2*X | doubleAll(Xs) ].

evens([]) ->
     [];
evens([X|Xs]) when X rem 2 == 0 ->
    [X | evens(Xs) ];
evens([_|Xs]) ->
    evens(Xs).



