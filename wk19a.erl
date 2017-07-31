% things for week 19 terminal session

-module (wk19a).
-export ([program/0, producer/1, consumer/0]).

% program(): constructs a producer and consumer, where
%	the producer is given the PID of the consumer.

program () ->
	C = spawn (?MODULE, consumer, []),
	spawn (?MODULE, producer, [C]),
	finished.


% consumer(): the consumer process, this reacts to messages
%	in different ways.  The messages are:
%
%	{msg, M} -- to display a message (M).
%	status -- to display status information (how many messages received).
%	shutdown -- to self-terminate.

consumer () -> consumer (0).

consumer (N) ->
	receive
		{msg, M} ->
			io:format ("Message was: ~s~n", [M]),
			consumer (N+1);
		status ->
			io:format ("I (~w) have seen ~w messages so far~n",
				[self (), N]),
			consumer (N);
		shutdown -> exit (normal)
	end.

% producer(Target): the producer process, this sends various messages
%	the consumer, finishing with a termination message and then
%	self-terminates.

producer (Target) ->
	Target ! {msg, "hello, Erlang world!"},

	receive		% this is how we do a time-delay in Erlang
	after		% i.e. as part of an otherwise empty "receive".
		1000 -> true
	end,

	Target ! {msg, "hello, again!"},
	Target ! shutdown,
	exit (normal).

