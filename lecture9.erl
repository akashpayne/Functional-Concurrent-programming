-module(lecture9).
-compile([export_all]).

-spec takeR(integer(),[T]) -> [T].

takeR(N,Xs) when N<length(Xs) ->
    {Front,_Back} = lists:split(N,Xs),
    Front;
takeR(_N,Xs) ->
    Xs.

-spec take(integer(),[T]) -> [T].
     
take(0,_Xs) ->
     [];
take(_N,[]) ->
    [];
take(N,[X|Xs]) when N>0 ->
%   ... take(N-1,Xs) ...
    [X | take(N-1,Xs)].

% insertion sort
%
% idea: sort the tail of the list, and then insert the head in the right place
%
% example [3,6,1,2,4]
% - sort [6,1,2,4] to give [1,2,4,6]
% - insert 3 in the right place in [1,2,4,6]
%   to give [1,2,3,4,6]
% see discussion of insert below

-spec iSort([integer()]) -> [integer()].

iSort([]) ->
     [];
iSort([X|Xs]) ->
    insert(X,iSort(Xs)).

%
% inserting an element at the right place in a *sorted* list
%
% e.g. inserting 3 in [1,2,4,6]
% [Y|Ys] = [1,2,4,6] means that Y is 1, Ys is [2,4,6]
% how do we insert 3?
% - it's not =<1, so it doesn't go at the front
% - the head of the result is therefor Ys, and
% - the tail is got by inserting 3 in [2,4,6], i.e.
%   inserting X in Ys.

% if we're inserting in [4,6] we simply make it
% the first element [3|[4,6]].

-spec insert(integer(),[integer()]) -> [integer()].

insert(X,[]) ->
    [X];
insert(X,[Y|Ys]) when X<Y ->
    [X|[Y|Ys]];
insert(X,[Y|Ys]) when X==Y ->
    [Y|Ys];
insert(X,[Y|Ys]) ->
    [Y|insert(X,Ys)].



