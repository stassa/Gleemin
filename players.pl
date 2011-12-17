% First saved: 22/01/2011
% Last saved:  23/04/2011
%
%	Incremental save
%
% 	Status:
%	Should be OK
%
% Doing:
%	Done adding save_game (rudimentary)
%	save_player/2 needs to be extended (check notes)
%
% Todo:
%	string_to_list: check notes.
% NOTES:
/*	In multiplayer games, a team is needed.

	save_player/2 is not very sophisticated
	It just saves everything in the shared zones
	  or partitions. However, when you are casting
	  a spell, for example, you may need to cancel
	  the spell casting but not the activation of
	  mana abilities - so sources that are tapped
	  and mana pools that have mana in them should
	  not be restored to a previous point. In order
	  to do this of course you'll need to add a
	  options arguments and a bit more complicated
	  logic. Until then, I'm letting cast_spells
	  restore everything to the point before casting
	  whichever part of it is cancelled.

*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%         Swi Compatibility         %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	:- set_prolog_flag(backquoted_string,true).
	% Swi LPA backtic string compatibility.
	:- style_check(-singleton).
	% Stop Swi warning on singleton vars.
	:- style_check(-discontiguous).
	% Stop Swi warning on discontiguous clauses.

        :- redefine_system_predicate(string_to_list(_Atom,_List)).
        % ^ Because of same-name Swi builtin. This is redundant,
        %  so remove and revert to Swi default.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%          Initialisation           %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic
	mana_pool/2,
	life_total/2,
	poison_counters/2,
	hand_size/2,
	order_of_play/1,
	player/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%             Player Facts          %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%player/1 09/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* player(+Name) */

	% Who is a player?
	player('Player 1').
	player('Glee-min').
	% ^ Now asserted in main, else it's overwritten at game start.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%order_of_play/1 09/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* order_of_play(+Players) */

	% All the players, in turn from first to last
%	order_of_play(['Glee-min', 'Player 1']).
	order_of_play(['Player 1', 'Glee-min']).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%match_score/2 09/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* match_score(+Player, +Matches_won) */

	% Index numbers of matches won by player
	match_score('Player 1', []).
	match_score('Glee-min', []).
	% The game should check how many matches have been won
	% And also assign index numbers to matches.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%life_total/2 09/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* life_total(+Player, +Life). */

	% Life totals
	life_total('Player 1', 20).
	life_total('Glee-min', 20).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% poison_counters/2 21/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* poison_counters(+Player, +Poison). */

	% Poison counters
	poison_counters('Player 1', 0).
	poison_counters('Glee-min', 0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%hand_size/2 09/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* hand_size(+Player, +Hand) */

	% Hand sizes
	hand_size('Player 1', 7).
	hand_size('Glee-min', 7).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%mana_pool/2 19/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* mana_pool(Player, Mana) */
	% Where Mana == [Colourless, White, Blue, Black, Red, Green]

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	Mana quantities should be of the type:
	 <number> (colourless) or
	 w (white) or
	 u (blue) or
	 b (black) or
	 r (red) or
	 g (green)
	So for example, the cost two, red, black, black
	should look like:
	  2rbb
	This should then be turned into a list:
	  [2, r, b, b]
	and finally a mana_quantity:
	  [c-2, w-0, u-0, b-2, r-1, g-0]
	Yep, those are key-value pairs.
*/

	% Mana Pools
	mana_pool('Player 1', [c-0, w-0, u-0, b-0, r-0, g-0]).
	mana_pool('Glee-min', [c-0, w-0, u-0, b-0, r-0, g-0]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%player_state/2 27/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* player_state(+Player, +State) */

	player_state('Player 1', []).
	player_state('Glee-min', []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%           Player Rules            %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%empty_mana_pool/ 21/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* empty_mana_pool(+Player) */
	% Empties a player's mana pool.

	empty_mana_pool(Player):-
		mana_pool(Player, Mana_pool),
		retractall(mana_pool(Player, Mana_pool)),
		assert(mana_pool(Player, [c-0, w-0, u-0, b-0, r-0, g-0])).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% add_to_pool/4 21/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* add_to_pool(+Player, +Mana, -New_pool)  */
	% Adds a quantity of mana to a player's mana pool.

	% eg, enter: add_to_pool('Player 1', Mana_pool, '2rrbg', New_pool).
	add_to_pool(Player, Mana, New_pool):-
		player(Player),
		mana_pool(Player, Mana_pool),
		string_to_list(Mana, List),
		add_mana(List, Mana_pool, New_pool),
		retractall(mana_pool(Player, Mana_pool)),
		assert(mana_pool(Player, New_pool)).


%%%%%%%%%%%%%% add_mana/3 21/01/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* add_mana(Mana_list, Mana_pool, New_pool) */
	% Adds the mana to the player's pool.

%%%%%%%%%%%%%%add_mana/3 (1) 21/01/11

	% Boundary condition...
	add_mana([], New_pool, New_pool).

%%%%%%%%%%%%%%add_mana/3 (2) 21/01/11

	% For each symbol in the mana list, add 1 to the appropriate value
	%  in the mana pool. Numbers (representing colourless mana) are
	%  added wholesale.
	add_mana([Symbol | Rest], [c-C, w-W, u-U, b-B, r-R, g-G], New_pool):-
		(number(Symbol) -> C1 is C + Symbol; C1 is C),
		(Symbol == w -> W1 is W + 1; W1 is W),
		(Symbol == u -> U1 is U + 1; U1 is U),
		(Symbol == b -> B1 is B + 1; B1 is B),
		(Symbol == r -> R1 is R + 1; R1 is R),
		(Symbol == g -> G1 is G + 1; G1 is G),
		remove(Symbol, [Symbol | Rest], Rest),
		add_mana(Rest, [c-C1, w-W1, u-U1, b-B1, r-R1, g-G1], New_pool),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% take_from_pool/4 21/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* take_from_pool(+Player, +Mana, -New_pool)  */
	% Draws a quantity of mana from a player's mana pool.
	% Fails disgracefully if more Mana is attempted to be drawn than
	%  available.

	% eg, enter: take_from_pool('Player 1', '2rrbg', New_pool).
	take_from_pool(Player, Mana, New_pool):-
		player(Player),
		mana_pool(Player, Mana_pool),
		string_to_list(Mana, List),
		(
			(
				draw_mana(Mana_pool, List, New_pool) ->
				retractall(mana_pool(Player, Mana_pool)),
				assert(mana_pool(Player, New_pool)) %,
%				output(draw_mana, [drawn_mana, Player, Mana, New_pool])
			);
			New_pool = Mana_pool,
%			output(draw_mana, [failed_to_draw_mana, Player, New_pool]),
			fail, !
		).


%%%%%%%%%%%%%%draw_mana/3 21/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* draw_mana(Mana_pool, +Mana_list, -Remaining) */
	% draws mana from a mana pool and returns the new pool.

%%%%%%%%%%%%%%draw_mana/3 (0) 21/02/11

	% Whole amount drawn.
	draw_mana(Mana_pool, Mana_list, Mana_pool):-
		(Mana_list = [0] ; Mana_list = []).

%%%%%%%%%%%%%%draw_mana/3 (1) 21/02/11

	% Match coloured mana in the Mana_list to coloured mana
	%  in the Mana_pool
	draw_mana(Mana_pool, Mana_list, Remaining):-
		Mana_pool = [c - C, w - W, u - U, b - B, r - R, g - G],
		member(Symbol, Mana_list),
		\+ number(Symbol), % A coloured mana symbol
		member(Symbol - Amount, Mana_pool),
		Amount >= 1, % not sure if needed explicitly
		(
			((Symbol == w -> W1 is W - 1, W1 >= 0) ; W1 is W),
			((Symbol == u -> U1 is U - 1, U1 >= 0) ; U1 is U),
			((Symbol == b -> B1 is B - 1, B1 >= 0) ; B1 is B),
			((Symbol == r -> R1 is R - 1, R1 >= 0) ; R1 is R),
			((Symbol == g -> G1 is G - 1, G1 >= 0) ; G1 is G)
		),
		New_mana_pool = [c - C, w - W1, u - U1, b - B1, r - R1, g - G1],
		remove(Symbol, Mana_list, New_mana_list), !, %red cut. Trace it!
		draw_mana(New_mana_pool, New_mana_list, Remaining).

%%%%%%%%%%%%%%draw_mana/3 (2) 21/02/11

	% Match colourless mana symbols in Mana_list to coloured mana symbols
	%  in Mana_pool
	draw_mana(Mana_pool, Mana_list, Remaining):-
		Mana_pool = [c - C, w - W, u - U, b - B, r - R, g - G],
		member(Symbol_2, Mana_list),
		number(Symbol_2),  % There's only colourless mana left in Mana_list
		member(Symbol - Amount, Mana_pool),
		Symbol \= c, % Don't use colourless mana yet
		Amount >= 1,
		(
			((Symbol == w -> W1 is W - 1, W1 >= 0) ; W1 is W),
			((Symbol == u -> U1 is U - 1, U1 >= 0) ; U1 is U),
			((Symbol == b -> B1 is B - 1, B1 >= 0) ; B1 is B),
			((Symbol == r -> R1 is R - 1, R1 >= 0) ; R1 is R),
			((Symbol == g -> G1 is G - 1, G1 >= 0) ; G1 is G)
		),
		New_mana_pool = [c - C, w - W1, u - U1, b - B1, r - R1, g - G1],
		New_mana_list is Symbol_2 - 1,
		draw_mana(New_mana_pool, [New_mana_list], Remaining).

%%%%%%%%%%%%%%draw_mana/3 (3) 21/02/11

	% Match colourless mana symbols in Mana_list to colourless mana
	%  symbols in Mana_pool
	draw_mana(Mana_pool, Mana_list, Remaining):-
		Mana_pool = [c - C, w - W, u - U, b - B, r - R, g - G],
		findall(Symbol_2,
			(member(Symbol_2, Mana_list),
			\+ number(Symbol_2)), Symbols),
		Symbols = [], % no more coloured mana symbols in the Mana_list
		member(Symbol - Amount, Mana_pool),
		Symbol = c,
		Amount >= 1,
		C1 is C - 1,
		New_mana_pool = [c - C1, w - W, u - U, b - B, r - R, g - G],
		member(Symbol_3, Mana_list),
		New_mana_list is Symbol_3 - 1, New_mana_list >= 0,
		draw_mana(New_mana_pool, [New_mana_list], Remaining).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% string_to_list/2 19/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/*  string_to_list(+Atom, -List) */
	% Takes in a string of mana symbols and returns it in list form
	%  separating numbers from non-numbers.

	% If there are no numbers in the string.
	string_to_list(Atom, List):-
		name(Atom, Full_list),
		separate_numbers(Full_list, Numbers, Characters),
		Numbers == [],	% check here so as not to bind prematurely.
		to_list(Characters, _Temp_list, To_list),
		reverse(To_list, List), !.

	% If there are numbers in the string.
	string_to_list(Atom, List):-
		name(Atom, Full_list),
		separate_numbers(Full_list, Numbers, Characters),
		to_list(Characters, _Temp_list, To_list),
		reverse(To_list, Ordered_chars),
		append([Numbers], Ordered_chars, List), !.

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	a) to_list clause removed: is also in predicates.pl
	b) string_to_list (and others) should use atom_chars etc,
	  rather than name/2, apparently.
*/


%%%%%%%%%%%%%% separate_numbers/3 19/01/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* separate_numbers(+List, -Numbers, -Characters) */
	% Takes in a list and returns a number and a list of char codes

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
 	This is rather sloppy and lazy- it returns all digits in the atom
	  as one number, regardless of their position in the atom! Obviously
	  this opens the door for amusing errors. It should be fixed,
	  but currently I can't be bovvered. Mana should be always coming in as
	  strings of exactly one number at the front and any number of symbols.
	  So if everything else does its job as it should, this should cause no
	  trouble.
*/


	% Top-level goal (leave out don't-care vars.
	separate_numbers(List, Numbers, Characters):-
		separate_numbers(List, _, Numbers, _, Characters).

	% If there are no numbers in the string.
	separate_numbers([], [], [], Other_chars, Chars):-
		reverse(Other_chars, Chars).

	% Some reversing needed to trim out undef'ed vars and reorder
	%  things properly.
	separate_numbers([], Numbers, Atomic_numbers, Other_chars, Chars):-
		reverse(Numbers, Reversed),
		name(Atomic_numbers, Reversed),
		reverse(Other_chars, Chars).

	% While the next char code is that of a number, add it to the list
	%  of digits.
	separate_numbers([Char | Rest], List, Numbers, Other, Chars):-
		isa_num(Char),
		separate_numbers(Rest, [Char | List], Numbers, Other, Chars), !.

	% While the next char code is not that of a number, keep it separate.
	separate_numbers([Char | Rest], List, Numbers, Other, Chars):-
		not isa_num(Char),
		separate_numbers(Rest, List, Numbers, [Char | Other], Chars), !.

	% Number char codes.
	isa_num(Character) :-
		Character =< 0'9,
		Character >= 0'0.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%gain_life/3 21/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* gain_life(+Player, +Life, -New_life_total) */
	% adds to a player's life total.

	gain_life(Player, Life, New_total):-
		life_total(Player, Life_total),
		New_total is Life_total + Life,
		retractall(life_total(Player, Life_total)),
		assert(life_total(Player, New_total)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lose_life/3 21/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* lose_life(+Player, +Life, -New_life_total) */
	% reduces a player's life total.

	lose_life(Player, Life, New_total):-
		life_total(Player, Life_total),
		New_total is Life_total - Life,
		retractall(life_total(Player, Life_total)),
		assert(life_total(Player, New_total)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%gain_poison/3 21/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* gain_poison(+Player, +Poison, -Poison_counters) */
	% Gives poison counters to a player

	gain_poison(Player, Poison, New_counters):-
		poison_counters(Player, Poison_counters),
		New_counters is Poison_counters + Poison,
		retractall(poison_counters(Player, Poison_counters)),
		assert(poison_counters(Player, New_counters)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%increase_hand_size/3 21/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* increase_hand_size(+Player, +Increase_by, -Hand_size) */
	% Increases a player's hand size.

	increase_hand_size(Player, Increase, New_size):-
		hand_size(Player, Size),
		New_size is Size + Increase,
		retractall(hand_size(Player, Size)),
		assert(hand_size(Player, New_size)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%decrease_hand_size/3 21/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* increase_hand_size(+Player, +Increase_by, -Hand_size) */
	% Decreases a player's hand size.

	decrease_hand_size(Player, Increase, New_size):-
		hand_size(Player, Size),
		New_size is Size - Increase,
		retractall(hand_size(Player, Size)),
		assert(hand_size(Player, New_size)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%save_player 10/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/*  save_player(+Player, -Saved) */
	% Returns the player's current state, ie cards in all zones
	%  and all Attribute values (life, mana, poison and max hand size)

	save_player(Player, Saved):-
		findall(Zone, zone(Player, Zone, _), Zones),
		Attributes = [life_total, mana_pool, hand_size, poison_counters],
		save_zones(Player, Zones, [], Saved_zones),
		save_attributes(Player, Attributes, [], Saved_attributes),
		save_shared(['Battlefield', 'Exile', 'Stack'], _Temp, Shared),
		append([Saved_zones], [Saved_attributes], Saved_1),
		append(Saved_1, [Shared], Saved), !.

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	Note that "save" is a bit of a misnomer. This predicate keeps
	  the appropriate values safe, while other predicates modify
	  the player's state, then if the player needs to be reverted
	  to a previous state, it feeds the Saved information to
	  restore_player/2.
	However, nothing is actually written in the database and so
	  nothing "saved" in the traditional sense of assigning a value
	  to a variable (and possibly saving it to a file). But then, this
	  is Prolog! Maybe this should be named "get", but this I fear will
	  mix up the semantics something horrible with getter/setter methods.
	Another predicate can later use this (and others) to save a copy
	  of a game in a separate file.
*/


%%%%%%%%%%%%%%save_zone/4 10/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* save_zones(+Player, Zones, [], Saved) */
	% Collects information about the Player's cards in all zones

%%%%%%%%%%%%%%save_zone/4 (0) 10/02/11

	save_zones(Player, [], Zones, Zones).

%%%%%%%%%%%%%%save_zone/4 (1) 10/02/11

	save_zones(Player, [Zone | Rest], Temp, Zones):-
		player(Player),
		zone(Player, Zone, Cards),
		append(Temp, [zone(Player, Zone, Cards)], New_temp),
		save_zones(Player, Rest, New_temp, Zones).


%%%%%%%%%%%%%%save_shared/3 08/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* save_shared(['Battlefield', 'Hand', 'Stack'], [], _Shared) */
	% Saves the state of shared zones

%%%%%%%%%%%%%%save_shared/3 (0) 08/03/11

	save_shared([], Shared, Shared).

%%%%%%%%%%%%%%save_shared/3 (0) 08/03/11

	save_shared([Zone | Rest], Temp, Shared):-
		zone(Zone, Objects),
		append(Temp, [zone(Zone, Objects)], New_temp),
		save_shared(Rest, New_temp, Shared).


%%%%%%%%%%%%%%save_attributes/4 10/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* save_zones(+Player, Attributes, [], Saved) */
	% Collects information about the Player's Attributes
	%  ie: life_total, mana_pool, hand_size and poison_counters

%%%%%%%%%%%%%%save_attributes/4 (0) 10/02/11

	save_attributes(Player, [], Attrs, Attrs).

%%%%%%%%%%%%%%save_zone/4 (1) 10/02/11

	save_attributes(Player, [Attr | Rest], Temp, Attrs):-
		player(Player),
%		Attr(Player, Value), % Swi, 18/06/11
%		append(Temp, [Attr(Player, Value)], New_temp),
		Attribute =.. [Attr, Player, _Value],
		Attribute,
		append(Temp, [Attribute], New_temp),
		save_attributes(Player, Rest, New_temp, Attrs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%restore_player/2 10/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* restore_player(+Player, +Save) */
	% Restores the player's state, ie cards in all zones and all
	%  Attribute values to a previous State (returned by save_player/2).

	restore_player(Player, Saved):-
		Saved = [Saved_zones, Saved_attributes, Shared],
		restore_zones(Player, Saved_zones),
		restore_attributes(Player, Saved_attributes),
		restore_shared(Shared).


%%%%%%%%%%%%%%restore_zones/4 10/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* restore_zones(+Player, +Zones) */
	% Restores the player's zones to a previous state.

%%%%%%%%%%%%%%restore_zones/2 (0) 10/02/11

	restore_zones(Player, []).

%%%%%%%%%%%%%%restore_zones/2 (1) 10/02/11

	restore_zones(Player, [Zone | Rest]):-
		Zone = zone(Player, Name, Cards),
		retractall(zone(Player, Name, _)),
		asserta(Zone),
		restore_zones(Player, Rest).


%%%%%%%%%%%%%%restore_attributes/4 10/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* restore_attributes(+Player, +Attributes) */
	% Restores the player's Attributes to a previous state.

%%%%%%%%%%%%%%restore_attributes/2 (0) 10/02/11

	restore_attributes(Player, []).

%%%%%%%%%%%%%%restore_attributes/2 (1) 10/02/11

	restore_attributes(Player, [Attr | Rest]):-
%		Attr =.. [Name, Player, Value],
%		retractall(Name(Player, _)),
		Attr =.. [_Name, Player, _Value],
		retractall(Attr),
		asserta(Attr),
		restore_attributes(Player, Rest).


%%%%%%%%%%%%%%restore_shared/1 08/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* restore_shared(+Shared_zones) */
	% Restores the shared zones to a saved state.

%%%%%%%%%%%%%%restore_shared/1 (0) 08/03/11

	restore_shared([]).

%%%%%%%%%%%%%%restore_shared/1 (1) 08/03/11

	restore_shared([Zone | Rest]):-
		Zone = zone(Name, _),
		retractall(zone(Name, _)),
		asserta(Zone),
		restore_shared(Rest).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%save_game/1 22/04/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% Keeps safe various facts from the database
	% Currently only two. Later: add more facts, save to file


	save_game(Saved):-
		save_land(Land),
		save_ids(Ids),
		Saved = [Land, Ids].


%%%%%%%%%%%%%%save_land/1 22/04/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* save_land(Land_facts) */
	% Keeps "played_land(Player, YesNo)" facts for later restoration

	save_land(Saved_land):-
		findall(played_land(_Player,_YesNo),
			played_land(_Player,_YesNo),
		Saved_land).


%%%%%%%%%%%%%%save_ids/1 22/04/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* save_ids(Id_facts) */
	% Keeps "object_ids(Object, Ids)" facts for later restoration

	save_ids(Saved_ids):-
		findall(object_ids(_Object, _Ids),
			object_ids(_Object, _Ids),
			Saved_ids).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%restore_game/1 22/04/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% Restores various facts to the database
	% Currently only two. Later: add more facts, restore from file

	restore_game([Land, Ids]):-
		restore_land(Land),
		restore_ids(Ids).


%%%%%%%%%%%%%%restore_land/1 22/04/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* restore_land(Saved_land) */
	% Restores 'played_land(Player, YesNo)' facts to a saved state

	restore_land(Saved_land):-
		retractall(played_land(_,_)),
		findall(Land,
			(member(Land, Saved_land),
			asserta(Land) ),
		_Nevermind).


%%%%%%%%%%%%%%restore_ids/1 22/04/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* restore_land(Saved_ids) */
	% Restores 'object_ids(Object, Ids)' facts to a saved state

	restore_ids(Saved_ids):-
		retractall(object_ids(_,_)),
		findall(Id,
			(member(Id, Saved_ids),
			asserta(Id) ),
		_Nevermind).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              Notes                %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Multiplayer predicates:

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%team/2 09/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* team(Number, Players) */

	% Multiplayer teams
%	team(1, ['Player 1', 'Player 2']).
%	team(2, ['Player 3', 'Player 4']).
	% No use currently.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%order_of_play/1 09/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	% Multiplayer order of play lists teams rather than players:
%	order_of_play([1, 2]).
	% Uncomment multi/two-player version accordingly.








