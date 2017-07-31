% cspnets.erl -- CSP networks in Erlang
% Carl Ritson & Fred Barnes, University of Kent, March 2014.

-module (cspnets).
-export ([channel/0, channel/1, chan/1,
	send/2, recv/1,
	pwrapper/3, proc/2, wait/1, waitsubs/1,
	generate/2, gen/2, printer/1, print_ints/1, adder/3, delta/3, id/2, prefix/3, tail/2,
	integrate/2, pairs/2, pipeline1/0]).


%{{{  channel handling (asynchronous, no acknowledgements).

% chan (): target-less channel.
%	spawns a `channel' process that waits for an initial target message.
%
chan () ->
	spawn_link (?MODULE, channel, [no_target]).

% chan (Target): target-specified channel.
%	spawns a `channel' process associated with a specific target process.
%
chan (Target) ->
	spawn_link (?MODULE, channel, [Target]).

% channel (no_target): target-less channel process.
%	waits for an initial `target' message to set the process the channel.
%
channel (no_target) ->
	receive
		{target, Target} ->
			channel (Target)
	end;

% channel (Target): channel with target.
%	waits for a new `target' message (moving channels around), or a specific
%	message which is forwarded to the specified target process.
%
channel (Target) ->
	receive
		{target, New} ->
			channel (New);
		Msg ->
			% anything else is a message for the target we have.
			Target ! {self (), Msg},
			channel (Target)
	end.

% channel (): creates a new channel.
%	other processes should use this to create new channels.
%
channel () ->
	chan ().

%}}}
%{{{  channel communication

% send (Channel, Msg): send the message `Msg' to the specified `Channel'.
%	this does not wait for an acknowledgement.
%
send (Channel, Msg) ->
	Channel ! Msg.

% receive (Channel): waits for a message on the specified `Channel'.
%
recv (Channel) ->
	% Note: the first thing we do is tell the channel where to send messages (i.e. us).
	Channel ! {target, self ()},
	receive
		% Note: `Channel' is bound here, so we only receive from it
		% (and not another channel connected to the same process).
		{Channel, Msg} -> Msg
	end.

%}}}
%{{{  process abstraction.

% pwrapper (Parent, Fun, Args): process wrapper.
%	This mostly just instantiates the process, but tells a
%	parent process (`Parent') when done.
%
pwrapper (Parent, Fun, Args) ->
	apply (?MODULE, Fun, Args),
	Parent ! finished.


% proc (Fun, Args): process constructor.
%	This creates a new process that executes the function `Fun',
%	passing the specified `Args'.
%
proc (Fun, Args) ->
	Pid = spawn_link (?MODULE, pwrapper, [self (), Fun, Args]),
	Pid.				% result is the new process's ID.

% wait (N): pauses for the specified number of milliseconds.
%
wait (N) ->
	receive
	after
		N -> true
	end.

% waitsubs (N): waits for `N' sub-processes to terminate.
%	this expects `finished' messages from individual processes.
waitsubs (0) -> exit (normal);

waitsubs (N) ->
	receive
		finished -> waitsubs (N-1)
	end.

%}}}
%{{{  simple processes (cycles).
% Note: processes that are instantiated through `proc' (above) must
% have their parameters in the order: general-args; input-chans; output-chans.

% generate (N, Out): generates an infinite stream of numbers starting at `N' on `Out'.
%	there is a delay of approximately 1 second between sends.
%
generate (N, Out) ->
	send (Out, N),
	wait (1000),
	generate (N+1, Out).

% gen (N, Out): generates a series of `N' values starting 0, 1, 2, 3, ...
%
gen (N, Out) ->
	gen (0, N, Out).

gen (N, N, _) ->
	true;
gen (V, N, Out) ->
	send (Out, V),
	gen (V+1, N, Out).

% printer (In): prints out messages it receives.
%
printer (In) ->
	Msg = recv (In),
	io:format ("~p~n", [Msg]),
	printer (In).

% print_ints (InChans): receives values from each of the input channels (in a list) and displays them.
%
print_ints (InChans) ->
	print_ints (lists:reverse (InChans), [], []).

print_ints ([], DoneChans, Vals) ->
	lists:map (fun (V) -> io:format ("~-10w", [V]) end, Vals),
	io:format ("~n"),
	print_ints (DoneChans);

print_ints ([C|Cs], DoneChans, Vals) ->
	V = recv (C),
	print_ints (Cs, [C|DoneChans], [V|Vals]).

% adder (In0, In1, Out): adds together numbers arriving on `In0' and `In1', sends to `Out'.
%
adder (In0, In1, Out) ->
	N0 = recv (In0),
	N1 = recv (In1),
	send (Out, N0+N1),
	adder (In0, In1, Out).

% delta (In, Out0, Out1): duplicates values arriving on `In' to `Out0' and `Out1'.
%
delta (In, Out0, Out1) ->
	V = recv (In),
	send (Out0, V),
	send (Out1, V),
	delta (In, Out0, Out1).

% id (In, Out): ID process (does nothing except copy input to output).
%
id (In, Out) ->
	V = recv (In),
	send (Out, V),
	id (In, Out).

% prefix (N, In, Out): outputs the value `N' then behaves like `id'.
%
prefix (N, In, Out) ->
	send (Out, N),
	id (In, Out).

% tail (In, Out): consumes the first input on `In' then behaves like `id'.
%
tail (In, Out) ->
	recv (In),
	id (In, Out).

%}}}
%{{{  networks of processes

% integrate (In, Out): integrate process (network of processes)
%
integrate (In, Out) ->
	C0 = channel (),
	C1 = channel (),
	C2 = channel (),

	proc (adder      , [In, C0, C1]),
	proc (delta      , [C1, C2, Out]),
	proc (prefix     , [0, C2, C0]),
	waitsubs (3).

% pairs (In, Out): pairs process (network of processes)
%	the first output is the first input +0.
%
pairs (In, Out) ->
	C0 = channel (),
	C1 = channel (),
	C2 = channel (),

	proc (delta     , [In, C0, C1]),
	proc (prefix    , [0, C0, C2]),
	proc (adder     , [C1, C2, Out]),
	waitsubs (3).

% pipeline1 (): simple pipeline of processes.
%
pipeline1 () ->
	C0 = channel (),
	C1 = channel (),
	C2 = channel (),

	proc (generate   , [0, C0]),
	proc (integrate  , [C0, C1]),
	proc (pairs      , [C1, C2]),
	proc (print_ints , [[C2]]).

%}}}


