-module(rps).
-export([play1/0,series_result/1,result/1,interact/1,echo/1,paper/1,random/1]).

% A simple implementation of rock-paper-scissors in Erlang.

% The result of a single play, from the point of view of the first player.
% Two versions: one takes a pair as a single argument,
% the other takes the two values separately.

% The "pair" version ...

result({X,Y}) -> result(X,Y).

% ... is defined in terms of the "two argument" version:

result(paper,paper)       -> draw;
result(paper,rock)        -> win;
result(paper,scissors)    -> lose;

result(rock,paper)        -> lose;
result(rock,rock)         -> draw;
result(rock,scissors)     -> win;

result(scissors,paper)    -> win;
result(scissors,rock)     -> lose;
result(scissors,scissors) -> draw.

% Getting a numeric result: win=+1, draw=0, lose=-1.

num_result(Z) -> res_to_num(result(Z)).

res_to_num(win)  -> 1;
res_to_num(lose) -> -1;
res_to_num(draw) -> 0.

% What is the result of a series of games, from the first
% player's point of view?

series_result(Games) ->
    Scores = lists:map(fun num_result/1, Games),
    Outcome = lists:sum(Scores),
    if
	Outcome > 0  -> win;
	Outcome == 0 -> draw;
	Outcome < 0  -> lose
    end.

% An example of a series of games.

play1() ->
   [{rock,rock},{paper,scissors},{rock,scissors},{paper,paper},{scissors,paper}].

% Play interactively.

% The argument is a strategy for the machine to play.
% For example,
%    rps:interact(fun rps:random/1)

interact(Strategy) ->
    interact(Strategy,[]).

% The second argument here is the accumulated input from the player
% Note that this function doesn't cheat: the Response is chosen
% before the Play from the player.

interact(Strategy,Xs) ->
    Response = Strategy(Xs), 
    {ok,[Play|_]} = io:fread('play one of rock, paper, scissors, or stop: ',"~a"),
    case Play of
	stop -> ok;
	_ ->
	    Result = result({Play,Response}),
	    io:format("Machine has played ~p, result is ~p~n",[Response,Result]),
	    interact(Strategy,[Play|Xs])
    end.



% strategies

% the random strategy

random(_) ->
    random_play().

% echo the previous choice of your opponent

echo([]) ->
     random_play();
echo([X|Xs]) ->
    X.

% assume that your opponent doesn't repeat themselves.

beat([]) ->
    random_play();
beat([X|_]) ->
    case X of
	rock -> scissors;
	paper -> rock;
	scissors -> paper
    end.

% always play paper (not much of a strategy ...)

paper(_) ->
    paper.

% A single random play, i.e. a random choice of rock, paper, scissors

random_play() ->
    case random:uniform(3) of
	1 -> rock;
	2 -> paper;
	3 -> scissors
    end.

    

