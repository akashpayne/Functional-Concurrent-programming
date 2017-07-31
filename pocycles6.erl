% pocycles6.erl -- final version! (3)
% Fred Barnes, University of Kent.

% this adds [TEST] poisoning

-module (pocycles6).
-export ([gen/2, make_gen/1,
		printer/1, make_printer/0,
		integrator/1, make_integrator/0,
		adder/1, make_adder/0,
		delta/1, make_delta/0,
		id/1, make_id/0,
		prefix/2, make_prefix/1,
		tail/1, make_tail/0,
		pmap/2, make_pmap/1,
		integrate/1, make_integrate/0,
		pairs/1, make_pairs/0,
		waitdone/1,
		harness1/0, harness2/0, harness3/0]).


% wait (N): waits the specified number of milliseconds.
%
wait (N) ->
	receive
	after
		N -> N
	end.

% make_gen(N): constructs a generator process that generates `N' numbers
%	starting at zero.  Returns a tuple `{Pid}'.
%
%	First communication is of the form `{set_out, Dst}'.
%
make_gen (N) ->
	G = spawn_link (?MODULE, gen, [self (), N]),
	{G}.

gen (P, N) ->
	receive {set_out, Dst} -> Dst end,
	gen (P, Dst, N, 0).

gen (P, {Target, _}, N, N) ->
	Target ! poison,
	P ! finished;

gen (P, Dst = {Target, M}, N, V) ->
	Target ! {M, V},
	wait (300),
	gen (P, Dst, N, V+1).



% make_printer(): constructs a printer process that receives messages (of
%	the form `{in, M}') and displays them.  Unexpected messages are also
%	printed out (so they don't sit in the message queue indefinitely).
%	Returns a tuple `{Pid, In}'.
%
make_printer () ->
	P = spawn_link (?MODULE, printer, [self ()]),
	{P, {P, in}}.

printer (P) ->
	receive
		poison ->
			io:format ("poisoned, finishing~n"),
			P ! finished;
		{in, M} ->
			io:format ("~w~n", [M]),
			printer (P);
		Other ->
			io:format ("** unexpected message: ~w~n", [Other]),
			printer (P)
	end.



% make_adder(): constructs an adder process that adds together pairs of numbers
%	received as messages `{in0,X}' and `{in1,Y}'.  Returns a tuple
%	`{Pid, In0, In1}'.
%
%	First communication is of the form `{set_out, Dst}'.
%
make_adder () ->
	A = spawn_link (?MODULE, adder, [self ()]),
	{A, {A, in0}, {A, in1}}.

adder (P) ->
	receive {set_out, Dst} -> Dst end,
	adder (P, Dst).

adder (P, Dst = {Target, Msg}) ->
	receive
		{in0, X} -> X;
		poison ->
			Target ! poison,
			P ! finished,
			X = exit (normal)
	end,
	receive
		{in1, Y} -> Y;
		poison ->
			Target ! poison,
			P ! finished,
			Y = exit (normal)
	end,
	Target ! {Msg, (X+Y)},
	adder (P, Dst).



% make_delta(): constructs a delta process that inputs messages in the form `{in, M}'
%	and outputs to both set outputs.  Returns a tuple `{Pid, In}'.
%
%	First communications are of the form `{set_out0, Dst1}', `{set_out1, Dst2}'.
%
make_delta () ->
	D = spawn_link (?MODULE, delta, [self ()]),
	{D, {D, in}}.

delta (P) ->
	receive {set_out0, Dst1} -> Dst1 end,
	receive {set_out1, Dst2} -> Dst2 end,
	delta (P, Dst1, Dst2).

delta (P, Dst1 = {Target1, Msg1}, Dst2 = {Target2, Msg2}) ->
	receive
		{in, M} -> M;
		poison ->
			Target1 ! poison,
			Target2 ! poison,
			P ! finished,
			M = exit (normal)
	end,
	Target1 ! {Msg1, M},
	Target2 ! {Msg2, M},
	delta (P, Dst1, Dst2).



% make_id(): constructs an identity process that inputs messages of the form `{in, M}'
%	and simply forwards these to the set output.  Returns a tuple `{Pid, In}'.
%
%	First communication is of the form `{set_out, Dst}'.
%
make_id () ->
	I = spawn_link (?MODULE, id, [self ()]),
	{I, {I, in}}.

id (P) ->
	receive {set_out, Dst} -> Dst end,
	id (P, Dst).

id (P, Dst = {Target, Msg}) ->
	receive
		{in, M} -> M;
		poison ->
			Target ! poison,
			P ! finished,
			M = exit (normal)
	end,
	Target ! {Msg, M},
	id (P, Dst).

% make_pmap(F): constructs a process using an arbitrary function.  Inputs
%	messages of the form `{in, M}' and outputs `{_, F(N)}'.  Returns a
%	tuple `{Pid, In}'.
%
%	First communication is of the form `{set_out, Dst}'
%
make_pmap (F) ->
	PF = spawn_link (?MODULE, pmap, [self (), F]),
	{PF, {PF, in}}.

pmap (P, F) ->
	receive {set_out, Dst} -> Dst end,
	pmap (P, Dst, F).

pmap (P, Dst = {Target, Msg}, F) ->
	receive
		{in, M} ->
			Target ! {Msg, F(M)},
			pmap (P, Dst, F);
		poison ->
			Target ! poison,
			P ! finished,
			exit (normal)
	end.


% make_prefix(N): constructs a prefix process that sends an initial value `N' to the
%	set output, then behaves like `id'.  Returns a tuple `{Pid, In}'.
%
%	First communication is of the form `{set_out, Dst}'.
%
make_prefix (N) ->
	P = spawn_link (?MODULE, prefix, [self (), N]),
	{P, {P, in}}.

prefix (P, N) ->
	receive {set_out, Dst} -> Dst end,
	prefix (P, Dst, N).

prefix (P, Dst = {Target, Msg}, N) ->
	Target ! {Msg, N},
	id (P, Dst).


% make_tail(): constructs a tail process that consumes an initial value then
%	behaves like `id'.  Returns a tuple `{Pid, In}'.
%
%	First communication is of the form `{set_out, Dst}'.
%
make_tail () ->
	T = spawn_link (?MODULE, tail, [self ()]),
	{T, {T, in}}.

tail (P) ->
	receive {set_out, Dst} -> Dst end,
	tail (P, Dst).

tail (P, Dst = {Target, _}) ->
	receive
		{in, M} -> M;
		poison ->
			Target ! poison,
			P ! finished,
			exit (normal)
	end,
	id (P, Dst).


% make_integrator(): constructs a serial version of the integrate process, that
%	maintains running sums of values received as messages of the form `{in,X}',
%	and for each input, outputs the new total.  Returns a tuple `{Pid, In}'.
%
%	First communication is of the form `{set_out, Dst}'.
%
make_integrator () ->
	I = spawn_link (?MODULE, integrator, [self ()]),
	{I, {I, in}}.

integrator (P) ->
	receive {set_out, Dst} -> Dst end,
	integrator (P, Dst, 0).

integrator (P, Dst = {Target, Msg}, Total) ->
	receive
		{in, X} -> X;
		poison ->
			Target ! poison,
			P ! finished,
			X = exit (normal)
	end,
	NewTotal = Total + X,
	Target ! {Msg, NewTotal},
	integrator (P, Dst, NewTotal).


% waitdone(N): waits for `N' "finished" messages.
%
waitdone (0) -> finished;

waitdone (N) -> receive finished -> waitdone (N-1) end.


% make_integrate(): constructs a process-network version of the integrator.
%	Returns a tuple `{Pid, In}'.
%
%	First communication is of the form `{set_out, Dst}'.
%
make_integrate () ->
	I = spawn_link (?MODULE, integrate, [self ()]),
	receive {inlink, I, Link} -> Link end,
	{I, Link}.

integrate (P) ->
	% create processes
	{D, Din} = make_delta (),
	{A, Ain0, Ain1} = make_adder (),
	{F, Fin} = make_prefix (0),

	% messages sent here need to go to the adder.
	P ! {inlink, self (), Ain0},

	% and *then* wait for some more plumbing information
	receive {set_out, Dst} -> Dst end,

	% handle plumbing
	D ! {set_out0, Dst},
	D ! {set_out1, Fin},
	A ! {set_out, Din},
	F ! {set_out, Ain1},

	%io:format ("integrate(): all done, shutting down with finished..~n"),

	P ! waitdone (3).


% make_pairs(): constructs a process-network version of pairs.
%	Returns a tuple `{Pid, In}'.
%
%	First communication is of the form `{set_out, Dst}'.
%
make_pairs () ->
	P = spawn_link (?MODULE, pairs, [self ()]),
	receive {inlink, P, Link} -> Link end,
	{P, Link}.

pairs (P) ->
	% create processes
	{D, Din} = make_delta (),
	{A, Ain0, Ain1} = make_adder (),
	{T, Tin} = make_tail (),

	% messages sent here need to go to the delta.
	P ! {inlink, self (), Din},

	% then wait for upstream plumbing info
	receive {set_out, Dst} -> Dst end,

	% handle plumbing
	D ! {set_out0, Tin},
	D ! {set_out1, Ain1},
	T ! {set_out, Ain0},
	A ! {set_out, Dst},

	P ! waitdone (3).


% harness1: runs a generator (gen) in parallel with a printer.
harness1 () ->
	% create processes
	{_, Pin} = make_printer (),
	{G} = make_gen (10),

	% handle plumbing
	G ! {set_out, Pin},

	waitdone (2).


% harness2: runs a generator (gen) in parallel with an integrator
%	and a printer.
harness2 () ->
	% create processes
	{_, Pin} = make_printer (),
	{I, Iin} = make_integrator (),
	{G} = make_gen (10),

	% handle plumbing
	G ! {set_out, Iin},
	I ! {set_out, Pin},

	waitdone (3).


% harness3: runs a generator (gen) in parallel with "integrate" (network version),
%	"pairs" and a printer.
harness3 () ->
	% create processes
	{_, Pin} = make_printer (),
	{I, Iin} = make_integrate (),
	{R, Rin} = make_pairs (),
	{G} = make_gen (100),

	%io:format ("harness3: setting up plumbing.. (I=~w, Pin=~w, G=~w)~n", [I, Pin, G]),

	% handle plumbing
	G ! {set_out, Iin},
	I ! {set_out, Rin},
	R ! {set_out, Pin},

	waitdone (4).


