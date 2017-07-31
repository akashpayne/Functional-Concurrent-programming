-module(week15).
-compile([export_all]).

% direct definition of factorial 

fac(0) ->
    1;
fac(N) when N>0 ->
    fac(N-1)*N.

% tail-recursive definition of factorial

facT(N) ->
    facT(N,1).

facT(0,P) ->
     P;
facT(N,P) when N>0 ->
    facT(N-1,P*N).

% First definition of produce_range.

product_range(M,M) ->
     M;
product_range(M,N) when M<N ->
    product_range(M,N-1)*N.

% Second definition of produce_range.

product_range2(M,M) ->
     M;
product_range2(M,N) when M<N ->
    M*product_range2(M+1,N).

% When there are N-1 lines, adding another makes N new areas.

pieces(0) ->
     1;
pieces(N) ->
    N+pieces(N-1).

% Function that is being summed etc.

f(0) -> 3;
f(1) -> 17;
f(2) -> -4;
f(3) -> 7;
f(N) when N>0 ->
     0.

% first definition of sum_fun

sum_fun(0) ->
     f(0);
sum_fun(N) when N>0 ->
    f(N) + sum_fun(N-1).

% other definitions of sum_fun
%  - tail recursion
%  - a two argument version doing
%    f(M) + f(M+1) + ... f(N).

% first version of find_max

find_max(M,M) ->
     f(M);
find_max(M,N) when N>M ->
    max(f(N),find_max(M,N-1)).

% second version of find_max
% returns the max and where it occurs

find_max2(M,M) ->
     {M,f(M)};
find_max2(M,N) when N>M ->
    {P,Max} = find_max2(M,N-1),
    case f(N)>Max of
	true -> {N,f(N)};
	false -> {P,Max}
    end.

% third version of find_max
% returns the max, where it occurs, and how often.

find_max3(M,M) ->
     {M,f(M),1};
find_max3(M,N) when N>M ->
    {P,Max,C} = find_max3(M,N-1),
    case f(N)>Max of
	true -> {N,f(N),1};
	_    -> case f(N)<Max of
		     true -> {P,Max,C};
		     _    -> {P,Max,C+1}
		 end
    end.

% building a list of numbers:

build(M,M) ->
     [M];
build(M,N) when M<N ->
    [M|build(M+1,N)].

% product of the numbers M * M+1 * ... * N.

product(M,M) ->
    M;
product(M,N) when M<N ->
    M*product(M+1,N).	     

% is N a member of the list

member(_,[]) ->
     false;
member(N,[N|_]) ->
    true;
member(N,[_|Xs]) ->
    member(N,Xs).
    
% which elements of the list are greater than N?

greater(_,[]) ->
     [];
greater(N,[X|Xs]) when N<X ->
    [N|greater(N,Xs)];
greater(N,[_|Xs]) ->
    greater(N,Xs).

% alternatively. let's use filter

greater2(N,Xs) ->
    lists:filter(fun(X) -> N<X end,Xs).
    
