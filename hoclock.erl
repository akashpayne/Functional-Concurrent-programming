% hoclock.erl -- higher-order clock
% Fred Barnes, University of Kent, March 2014.

-module (hoclock).
-compile ([export_all]).


% this just waits for an initial `reset' message.
%
hoclock (Id) ->
	receive
		{reset, N, Fun} -> hoclock (Id, N, Fun)
	end.

% this is the actual ticking clock.  Every `N' milliseconds evaluates
% the function `Fun', giving `Id' and `N' as arguments.
%
hoclock (Id, N, Fun) ->
	receive
		{reset, NewN, NewFun} -> hoclock (Id, NewN, NewFun)
	after 0 ->
		receive
		after N ->
			Fun (Id, N)
		end,
		hoclock (Id, N, Fun)
	end.

