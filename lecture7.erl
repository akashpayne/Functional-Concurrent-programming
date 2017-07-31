-module(lecture7).
-compile([export_all]).

area({circle, _, R}) ->
    math:pi()*R*R;
area({rectangle, _, H, W}) ->
    H*W.

all_areas([])     -> [] ;
all_areas([X|Xs]) -> [ area(X) | all_areas(Xs) ].

% sum([])     -> 0 ;
% sum([X|Xs]) -> X + sum(Xs).

% total_area(Shapes) ->
%     AllAreas = all_areas(Shapes),
%     sum(AllAreas).

total_area(Shapes) ->
    AllAreas = map(fun area/1,Shapes),
    sum(AllAreas).

% circles([])     -> [] ; 

% circles( [{circle,{X,Y},R} | Xs] ) -> 
%     [ {circle,{X,Y},R} | circles(Xs) ];

% circles( [{rectangle,{_,_},_,_} | Xs] ) -> 
%     circles(Xs).

% circles([])     -> [] ; 

% circles( [ X | Xs] ) ->
%     case X of
% 	{circle,{X,Y},R} -> 
% 	    [ {circle,{X,Y},R} | circles(Xs) ];
% 	{rectangle,{_,_},_,_}  -> 
% 	    circles(Xs)
%     end.

circles(Shapes) ->
    filter(fun is_circle/1,Shapes).

test_shapes() ->
    [ {circle,{1,2},2}, {rectangle,{5,4},3,2}, {circle,{-1,-2},1}].

map(F,[]) ->
    [];
map(F,[X|Xs]) ->
    [ F(X) | map(F,Xs) ].

is_circle({circle,{_,_},_}) -> 
    true;
is_circle({rectangle,{_,_},_,_}) -> 
    false.

filter(P,[]) ->
    [];
filter(P,[X|Xs]) ->
    case P(X) of
	true -> [X,filter(P,Xs)];
	false -> filter(P,Xs)
    end.

reduce(Combine, Start, []) ->
     Start;
reduce(Combine, Start, [X|Xs]) ->
    Combine(X, reduce(Combine, Start, Xs)).

sum(Xs) ->
    reduce(fun plus/2, 0, Xs).

plus(X,Y) ->
    X+Y.
