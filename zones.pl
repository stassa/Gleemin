% First saved: 04/02/2011
% Last saved: 30/02/2011
%
%
%
% Doing:
% 	Objects overhaul
% Todo
%
% NOTES:
%	replaced:
%		retract with retractall in deck_to_library/3
%		assert with asserta in deck_to_library/3 and shuffle/2
%	Didn't test much- if something is fubgjed try undoing.
%
%	The controller of a permanent is the player under whose control
%  	  it entered the Battlefield. In the MVM controllership is
%	  derived by the presence of a card on a Battlefield partition.
%	  Local echantments should stay on their controller's Battlefield
% 	  partition; their effect should be included in the state of their
%	  targets (and their own). For example, enchants(Permanent) and
%	  enchanted_by(Permanent), where Permanent is always a object/3 entry.
%	Similar considerations apply to copies of spells targeting other
%	  spells on the stack.
%	When control of a permanent changes, the predicate responsible must
%	  add the information that changed_controller(Owner, Controller) to
% 	  its state, unless Owner = Controller (whereupon, the Owner has
%	  resumed control of the Permanent)



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%         Swi Compatibility         %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	:- set_prolog_flag(backquoted_string,true).
	% Swi LPA backtic string compatibility.
	:- style_check(-singleton).
	% Stop Swi warning on singleton vars.
	:- style_check(-discontiguous).
	% Stop Swi warning on discontiguous clauses.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%            Zone facts             %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	:- dynamic
		zone/3,
		zone/2.


%	:- ensure_loaded('predicates.pl').
%	:- ensure_loaded('decks.pl').
	% ^ For testing



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%zone/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* zone(Name) */

	% all zones.
	zone('Library').
	zone('Hand').
	zone('Stack').
	zone('Battlefield').
	zone('Graveyard').
	zone('Exile').
	zone('Command').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%zone/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* zone(+Zone, -Cards) */
	% Cards in shared zones

	zone('Stack', []).
	zone('Battlefield', []).
	zone('Exile', []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%zone/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* zone(+Player, +Zone, +Cards) */
	% cards in players' zones.

	% Library:
	zone('Player 1', 'Library', []).
	zone('Glee-min', 'Library', []).

	% Stack
	zone('Player 1', 'Stack', []).
	zone('Glee-min', 'Stack', []).

	% Hand
	zone('Player 1', 'Hand', []).
	zone('Glee-min', 'Hand', []).

	% Battlefield
	zone('Player 1', 'Battlefield', []).
	zone('Glee-min', 'Battlefield', []).

	% Graveyard
	zone('Player 1', 'Graveyard', []).
	zone('Glee-min', 'Graveyard', []).

	% Exile
	zone('Player 1', 'Exile', []).
	zone('Glee-min', 'Exile', []).

	% Command
	zone('Player 1', 'Command', []).
	zone('Glee-min', 'Command', []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%public_zone/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% Faces of cards in public zones are visible to all players

	public_zone('Stack').
	public_zone('Battlefield').
	public_zone('Graveyard').
	public_zone('Exile').
	public_zone('Command').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%hidden_zone/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% Faces of cards in hidden zones are not visible to all players.

	hidden_zone('Library').
	hidden_zone('Hand').



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%           Zone Rules              %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%deck_to_library/3 22/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* deck_to_library(+Player, +Deck, -Library) */
	% Creates the player's library from the player's deck

	% Populate Library
	deck_to_library(Player, Deck, Library):-
		deck(Player, Deck),
		decklist(Deck, Cards),
		expand(Cards, Library),
		retractall(zone(Player, 'Library', _)),
		asserta(zone(Player, 'Library', Library)), !.


%%%%%%%%%%%%%%expand/3 22/01/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* expand(+Cards, -Library) */
	% Takes in a list of [Card, Instances] and returns
	%  a list of Instances instances of Cards.

%%%%%%%%%%%%%%expand/2 22/01/11

	% Top-level call
	expand(Cards, Library):-
		expand(Cards, Temp, Library).

%%%%%%%%%%%%%%expand/3 (1) 22/01/11

	% Boundary condition
	expand([], Library, Library).

%%%%%%%%%%%%%%expand/3 (2) 22/01/11

	% Recursive call
	expand([[Element, Length] | Tail], Temp, Library):-
		populate_list(Element, Length, List ),
		append(Temp, List, Full),
		expand(Tail, Full, Library).


%%%%%%%%%%%%%%populate_list/3 22/01/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* populate_list(+Element, +Length, -List) */
	% Generates a List of Length copies of Element

	populate_list(Element, Length, List):-
		length(Long, Length),
		findall(Element, member(Element, Long), List) .


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%shuffle/2 22/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* shuffle(+Player, -Shuffled) */
	% Shuffles a player's library. Not the player :P

	% Top-level goal.
	shuffle(Player, Shuffled):-
		zone(Player, 'Library', Library),
		shuffle(Library, _Temp, Shuffled), !,
		retractall(zone(Player, 'Library', Library)),
		asserta(zone(Player, 'Library', Shuffled)).


%%%%%%%%%%%%%%shuffle/3 22/01/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* shuffle(+Library, -Temp, -Shuffled) */
	% Recursively move each card in the library to a random position
	%  and return the result.

%%%%%%%%%%%%%%shuffle/3 (1) 22/01/11

	% Boundary condition.
	shuffle([], Shuffled, Shuffled).

%%%%%%%%%%%%%%shuffle/3 (2) 22/01/11

	% Choose a card at a random position in a Library list
	%  and append it to the Shuffled list, then remove
	%  the card from the Library list and recurse.
	shuffle(Library, Temp, Shuffled):-
		length(Library, Length),
%		Position is ip(@(Length)), % Swi, 19/06/11
		Position is random(Length),
		check_zero(Position, Length, New_position),
		member(Card, Library, New_position),
		append(Temp, [Card], New_temp),
		remove(Card, Library, New_library),
		shuffle(New_library, New_temp, Shuffled), !.
	% Length is used to find a random number between 0 and the
	%  number of cards in the library. Each cycle, the randomly -
	%  selected card is removed from the library, so the length
	%  needs to be recalculated each time.

%%%%%%%%%%%%%%check_zero/3 22/01/11
%%%%%%%%%%%%%%%%%%%%%%%
	/*  check_zero(+Position, +Length, -New_position) */
	% Checks that the randomly selected card-Position exists, basically.
	% If it's zero, it doesn't. Exist, that is.

%%%%%%%%%%%%%%check_zero/3 (1) 22/01/11

	% If the randomly-chosen position is 0, add 1 to it.
	check_zero(0, Length, Position):-
%		Position is ip(@(Length)) + 1.
		Position is random(Length) + 1.
	% This probably reduces randomness...

%%%%%%%%%%%%%%check_zero/3 (2) 22/01/11

	% If the randomly-chosen position is not 0, return it.
	check_zero(Position, _, Position).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%draw_cards/2  24/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* draw_cards(+Player, +Count) */
	% Draw Count cards from Player's library.

%%%%%%%%%%%%%%draw_cards/2 (1) 24/01/11

	% Boundary condition- no more cards to draw.
	draw_cards(Player, 0).

%%%%%%%%%%%%%%draw_cards/2 (2) 24/01/11

	% Draw a card from the player's library,
	%  decrement the card count, then recurse.
	draw_cards(Player, Count):-
		zone(Player, 'Library', [Card | Library]),
		move_to_zone(Player, Card, 'Library', 'Hand'),
		New_count is Count -1,
		draw_cards(Player, New_count).

%%%%%%%%%%%%%%draw_cards/2 (3) 24/01/11

	% If the player's library is empty, warn.
	draw_cards(Player, _Count):-
		zone(Player, 'Library', Library),
		length(Library, 0),
		write(Player),
		write(` is attempting to draw from an empty library!`), nl.
	% At this point Prolog will have failed to move
	%  a card from the player's library to his or her hand,
	%  so no further action is needed.
	% This should also be used as a winning (well, losing) condition.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%cards_in_zone/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	cards_in_zone(Player, Zone, Cards):-
		setof(Card, zone(Player, Zone, Card), [Cards]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%is_in_zone/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	is_in_zone(Playername, Card, Zone):-
		zone(Playername, Zone, Cardlist),
		member(Card, Cardlist).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%move_to_zone/3 19/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* move_to_zone(+Playername, +Object, +Current_zone, +Destination_zone) */
	% Moves a Card controlled by Playername from its Current_zone to a Destination_zone
	% The Object's representation in the db may change as a result

%%%%%%%%%%%%%%move_to_zone/4 (1) 23/02/11

	% Something attempted to move an Object in tis current Zone.
	%  If you suspect it, report it.
	move_to_zone(Player, Object, Zone, Zone) :-
			output(move_to_zone, ['warning', [Player, Object, Zone]]).

%%%%%%%%%%%%%%move_to_zone/4 (2) 23/02/11

	% The card is unchanged:
	move_to_zone(Player, Object, Current_zone, Destination_zone) :-
			member(Current_zone, ['Library', 'Hand', 'Command']),
			member(Destination_zone, ['Library', 'Hand', 'Command']),
			mOve_to_zone(Player, Object, Object, Current_zone, Destination_zone).

%%%%%%%%%%%%%%move_to_zone/4 (3) 23/02/11

	% The object acquires an Id and State
	move_to_zone(Player, Object, Current_zone, Destination_zone) :-
			member(Current_zone, ['Library', 'Hand', 'Command']),
			member(Destination_zone, ['Stack', 'Battlefield', 'Graveyard', 'Exile']),
			make_unique(Object, Copy),
			mOve_to_zone(Player, Object, Copy, Current_zone, Destination_zone).

%%%%%%%%%%%%%%move_to_zone/4 (4) 23/02/11

	% The Object loses all Id and State information
	move_to_zone(Player, Object, Current_zone, Destination_zone) :-
			member(Current_zone, ['Stack', 'Battlefield', 'Graveyard', 'Exile']),
			member(Destination_zone, ['Library', 'Hand', 'Command']),
			Object = object(Name - _Id, _State),
			mOve_to_zone(Player, Object, Name, Current_zone, Destination_zone).

%%%%%%%%%%%%%%move_to_zone/4 (5) 23/02/11

	% The Object's State information is cleared
	move_to_zone(Player, Object, Current_zone, Destination_zone) :-
			member(Current_zone, ['Stack', 'Battlefield', 'Graveyard', 'Exile']),
			member(Destination_zone, ['Stack', 'Battlefield', 'Graveyard', 'Exile']),
			Object = object(Name - Id, _State),
			New_object = object(Name - Id, []),
			mOve_to_zone(Player, Object, New_object, Current_zone, Destination_zone).


%%%%%%%%%%%%%%move_to_zone/4 23/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* 	mOve_to_zone(+Player, +Object, +New_object, +Current_zone, +Destination_zone) */
	% Actually move the Object around

	mOve_to_zone(Player, Object, New_object, Current_zone, Destination_zone):-
			zone(Player, Current_zone, Objects),
			zone(Player, Destination_zone, Dest_objects),
			remove(Object, Objects, New_objects),
			retractall(zone(Player, Current_zone, _)),
			asserta(zone(Player, Current_zone, New_objects)),
			append([New_object], Dest_objects, New_dest_zone),
			retractall(zone(Player, Destination_zone, _)),
			asserta(zone(Player, Destination_zone, New_dest_zone)),
			update_shared_zone(Current_zone),
			update_shared_zone(Destination_zone).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%remove_from_zone/3 15/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* remove_from_zone(+Playername, +Card, +Current_zone) */
	% Removes a Card controlled by Playername from its Current_zone.

	remove_from_zone(Playername, Card, Current_zone) :-
		zone(Playername, Current_zone, Cards),
		remove(Card, Cards, Newcards),
		retractall(zone(Playername, Current_zone, _)),
		asserta(zone(Playername, Current_zone, Newcards)),
		update_shared_zone(Current_zone).


%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	I dont' think this is actually needed.
	I don't know of a case where something is removed from a zone
	  unless it was in one before. But keeping just in case.
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%create_in_zone/3 15/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* create_in_zone(+Playername, +Object, +Zone) */
	% Creates an Object in a Zone, eg an Ability on the Stack or a token
	%  on the battlefield. Where appropriate, a unique object is generated.

%%%%%%%%%%%%%%create_in_zone/1 (1) 23/02/11

	create_in_zone(Player, Object, Zone):-
		member(Zone, ['Stack', 'Battlefield', 'Graveyard', 'Exile']),
		make_unique(Object, Copy),
		kreate_in_zone(Player, Copy, Zone).

%%%%%%%%%%%%%%create_in_zone/1 (1) 23/02/11

	create_in_zone(Player, Object, Zone):-
		kreate_in_zone(Player, Object, Zone).


%%%%%%%%%%%%%%move_to_zone/4 23/02/11
%%%%%%%%%%%%%%%%%%%%%%%

	kreate_in_zone(Player, Object, Zone):-
		zone(Player, Zone, Objects),
		append([Object], Objects, New_objects),
		retractall(zone(Player, Zone, _)),
		asserta(zone(Player, Zone, New_objects)),
		update_shared_zone(Zone).

% Probmel with make_unique now. Its ids are not unique for the Kb,
%  anymore (its copies in shared zones share it)
	% Well, duh. That's what an Id is for, identifying the Object
	%  as the same, in two different zones.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%update_shared_zone/1 23/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* update_shared_zone(+Zone) */
	% Updates the contents of one shared zone

%%%%%%%%%%%%%%update_shared_zone/1 (1) 23/02/11

	% This is not a shared zone
	update_shared_zone(Zone):-
		\+ member(Zone, ['Stack', 'Battlefield', 'Exile']).

%%%%%%%%%%%%%%update_shared_zone/1 (2) 23/02/11

	update_shared_zone(Zone):-
		findall(Partition,
			(player(Player),
			zone(Player, Zone, Partition),
			Partition \= []),
			Partitions),
		flatten_list(Partitions, Parts),
		retractall(zone(Zone, _)),
		asserta(zone(Zone, Parts)).

%%%%%%%%%%%%%%update_shared_zone/1 (3) 23/02/11

	% All Partitions empty, Nothing to update?
	update_shared_zone(Zone).
	% Hm. Potential problem here, check.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%owned_by/2 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	% The Object's Owner is the same as its Controller, unless control
	%  of the Object has passed to another player.
	owned_by(Object, Owner):-
		Object = object(_Name - _Id, State),
		controlled_by(Object, Owner),
		\+ (member(changed_control(Owner, Controller), State),
		Controller \= Owner).

	% Otherwise, it's owned by its initial owner, stored in its State.
	owned_by(Object, Owner):-
		Object = object(_Name - Id, State),
		member(changed_controller(Owner, _Other_player), State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%controlled_by/2 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	% An Object is controlled by the player who put it into play
	% This is a generalisation of the controller -> permanent rule
	%   in the Comp rules, due to the Partitions hack.
	controlled_by(Object, Controller):-
		zone(Controller, _Zone, Objects),
		member(Object, Objects).

	% Otherwise it's controlled by its new controller, stored in its State.
	controlled_by(Object, Controller):-
		Object = object(_Name - _Id, State),
		member(changed_controller(_Owner, Controller), State).







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              Notes                %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


