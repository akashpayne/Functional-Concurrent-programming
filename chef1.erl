% chef1.erl -- sync choices in Erlang
% Fred Barnes, March 2014.

-module (chef1).
-compile ([export_all]).

servery () ->
	receive
		Meal -> io:format ("Om nom nom: ~p~n", [Meal])
	end,
	servery ().

kitchen_hand (Chef) ->
	random:seed (now ()),
	kitchen_hand2 (Chef).

kitchen_hand2 (Chef) ->
	Item = lists:nth (random:uniform (4), [meat, pastry, fruit, cream]),
	Chef ! Item,
	io:format ("kitchen-hand: ~p~n", [Item]),	% debug
	timer:sleep ((random:uniform (5) + 2) * 200),
	kitchen_hand2 (Chef).

chef (Servery) ->
	random:seed (now ()),
	chef (Servery, []).

chef (Servery, Shelf) ->
	receive
		I -> trycook (Servery, [I|Shelf])
	end.

trycook (Servery, Shelf) ->
	IngList = [meat, pastry, fruit, cream],
	HasIng = lists:map (fun (I) -> lists:member (I, Shelf) end, IngList),

	io:format ("chef: shelf is ~p~n", [Shelf]),

	case HasIng of
		[true, true, _, _] ->
			timer:sleep ((random:uniform (5) + 2) * 200),
			Servery ! pie,
			trycook (Servery, lists:delete (meat, lists:delete (pastry, Shelf)));
		[_, true, true, _] ->
			timer:sleep ((random:uniform (5) + 2) * 200),
			Servery ! pudding,
			trycook (Servery, lists:delete (pastry, lists:delete (fruit, Shelf)));
		[_, _, true, true] ->
			timer:sleep ((random:uniform (5) + 2) * 200),
			Servery ! snack,
			trycook (Servery, lists:delete (fruit, lists:delete (cream, Shelf)));
		[true, _, _, true] ->
			timer:sleep ((random:uniform (5) + 2) * 200),
			Servery ! curry,
			trycook (Servery, lists:delete (meat, lists:delete (cream, Shelf)));
		_ ->
			% cannot cook with these ingredients!
			chef (Servery, Shelf)
	end.

harness () ->
	S = spawn_link (?MODULE, servery, []),
	C = spawn_link (?MODULE, chef, [S]),
	spawn_link (?MODULE, kitchen_hand, [C]).


