% First saved: 13/02/11
% Last saved: 25/04/11
%
% 	Status:
%
% Doing:
%	add sandbox invocation to main menu, for tutorial.
%	Commenting out dynamic declaration of lethal_damage, unneded for now.
%	changing predicates to call object handles only,
%	  not objects directly. Look at Creature Combat.txt
%		Done for combat_damage
%		declare_attackers
%		declare_blockers
%	apply damage simultaneously
%	Disallow illegal damage assignments (less than lethal or more
%	  than creature's power)
%
% Todo
%	In assign_damage I'm using creature/6 in a bit of weird way and generally
%	  queries seem inefficient- take some time to fix those.
%	Add rules for First Strike, Double Strike, Trample
%	  (and other combat-modifying abilities?)
%	Creatures are allowed to block multiple attackers, but they
%	  should not, unless they have a multi-blocking ability.
%	tightly bind combat lists to the Battlefield (see Notes)
%	complete output (eg, attacking/ blocking creatures info)
%	More attack/block costs to implement
%	More attack and block legality tests to implement
%
% NOTES:
/*
	What happens when the player attempts to assign less than lethal
	  damage or more than a creature's Power?

	When something affects a creature in the attackers or blockers list
	  the list may need to be updated- for example, to remove the creature
	  from it (if the something destroys it or bounces it etc).
	At every combat step, the two lists should be updated; specificaly,
	  check the battlefield (the shared one) and drop any creature that
	  is not still there.
	Or maybe add a clause to update_shared?

	I should generalise "order"- it's the same for both blockers
	  and attackers.
	  Except for the findall clause (one finds all "blocking" and the
	  other finds all "blocked_by". Well. I can make sure the right
	  thing is happening. Actually, I can pass it as an argument,
	  to disambiguate between the two ordering processes.
	Note that each will have to call a different output/2 clause also.
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%           Combat facts            %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	:- dynamic(attackers/1).
	:- dynamic(blockers/1).
	:- dynamic(attackers_order/1).
	:- dynamic(blockers_order/1).
	:- dynamic(damage_assignments/1).
	%:- dynamic assign_lethal_damage/6.
	% ^ Dirty Prolog alert! This predicate is declared dynamic
	%  so glee_min can inject a clause at the top of its listing
	%  while evoking it during decision-making using the minimax
	%  algorithm.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%attackers/1 03/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* attackers(+Creatures) */
	% List of attacking Creatures

	attackers([]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%blockers/1 03/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* blockers(+Creatures) */
	% List of blockers

	blockers([]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%attackers_order/1 03/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* attackers_order(+Creatures) */
	% Order of damage to attackers
	% List of: [Blocker_1, [Attacker_1, Attacker_2...]]

	attackers_order([]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%blockers_order/1 03/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* blockers_order(+Creatures) */
	% Order of damage to blockers
	% List of: [Attacker_1, [Blocker_1, Blocker_2...]]

	blockers_order([]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%damage_assignments/1 03/04/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* damage_assignments(Assignments) */
	% List of: [Dealer, Recipient, Damage].
	% Used to store damage assignments for simultaneous application

	damage_assignments([]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%            Combat rules           %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%combat_begins/2 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* combat_begins(+Active_player, +Opponent) */
	% Testing stuff.

	combat_begins(Active_player, Opponent):-
		%sandbox_mode,
		%write(`Combat begins`), nl,
		declare_attackers(Active_player),
		attackers(Attackers), Attackers \= [],
		% ^ remember to add this to turn_sequence
		declare_blockers(Opponent, _),
		attackers_order(Attackers_order),
		blockers_order(Blockers_order),
		combat_damage(Opponent, Blockers_order, Attackers_order),
		combat_ends.

	combat_begins(Active_player, _Opponent):-
		write(`No creatures declared as attackers by ` - Active_player), nl.

	sandbox_mode:-
	retractall(object_ids( _, _ )),
	asserta( object_ids( [], [0] )),
	retractall(zone('Battlefield', _)),
	asserta(zone('Battlefield', [])),
	retractall(zone(_, 'Battlefield', _)),
	asserta(zone('Player 1', 'Battlefield', [])),
	asserta(zone('Glee-min', 'Battlefield', [])),
/*	update_attackers([]),
	update_blockers([]),
	update_attackers_order([]),
	update_blockers_order([]),*/
	combat_ends, % combat cleanups
%	move_to_zone('Player 1', 'Air Servant', 'Library', 'Battlefield'),
%	move_to_zone('Player 1', 'Maritime Guard', 'Library', 'Battlefield'),
	%move_to_zone('Player 1', 'Llanowar Elves', 'Library', 'Battlefield'),
%	move_to_zone('Player 1', 'Birds of Paradise', 'Library', 'Battlefield'),
	move_to_zone('Player 1', 'Runeclaw Bear', 'Library', 'Battlefield'),
	%move_to_zone('Glee-min', 'Goblin Balloon Brigade', 'Library', 'Battlefield'),
%	move_to_zone('Glee-min', 'Prodigal Pyromancer', 'Library', 'Battlefield'),
%	move_to_zone('Glee-min', 'Canyon Minotaur', 'Library', 'Battlefield').
	move_to_zone('Glee-min', 'Vulshok Berserker+', 'Library', 'Battlefield').

	% When Hu-max attacks, remember to set active_player to him/her
	%  otherwise errors occur in order_blockers/attackers and more.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%declare_attackers/1 06/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* declare_attackers(+Active_player) */
	% Allows the Active_player to choose Attackers.

	declare_attackers(Player):-
		zone(Player, 'Battlefield', Objects),
		findall([Permanent, Id],
				(member(object(Permanent -Id,State), Objects),
				creature_type(Permanent),
				legal_attacker(State)),
			Permanents),
		findall(Creature, member([Creature, Id], Permanents), Creatures),
		append([cancel, ok], Creatures, Full),
		prompt_switches(Full, Switches), % Switches: [Map, Switches]
		Switches = [Map, _Switches],
		identify_object(Map, Permanents, 2, [], Identified), !, %green cut
		% Identified: list of [Creature - Switch, Id]
		declare_attackers(Player, Switches, Identified).


%%%%%%%%%%%%%%legal_attacker/1 06/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* legal_attacker(+State) */
	% Checks that a creature can attack

	legal_attacker(State):-
		\+ member(tapped, State),
		\+ member('summoning sickness', State).


%%%%%%%%%%%%%%declare_attackers/3 06/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* declare_attackers(+Player, +Context, -Input) */
	% input/3 call for declare_attackers/1

	% There are no creatures to declare as attackers.
	declare_attackers(_Player, _Switches, []).

	% Gleemin is the attacker
	declare_attackers('Glee-min', [Map, _Switches], Identified):-
		glee_min(combat, declare_attackers, Identified, List),
		declare_attackers('Glee-min', Map, Identified, List).
	% As usual, Switches (ie, "[N]ame" 's of cards etc) are only
	%  needed to prompt the human player.

	% The human player is the attacker
	declare_attackers(Player, [Map, Switches], Identified):-
		Context = [Player, Switches, Identified],
		string_input(declare_attackers, Context, Input) ->
		atom_to_list(Input, List), % List: [characters (not codes)]
		declare_attackers(Player, Map, Identified, List);
		declare_attackers(Player, [Map, Switches], Identified).


%%%%%%%%%%%%%%declare_attackers/4 06/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* declare_attackers(+Player, +Map, +Identified, +Input) */
	% Processes the player's input from declare_attackers/3

%%%%%%%%%%%%%%declare_attackers/4 (0) 23/03/11

	% No more attackers to process
	declare_attackers(_Player, _Map, _Identified, []):-
		attackers(Attackers),
		output(declare_attackers, [attacking, Attackers]).

%%%%%%%%%%%%%%declare_attackers/4 (0) 06/03/11

	% The player is not attacking with any more creatures.
	declare_attackers(_Player, Map, _Identified, Input):-
		Input = [Switch],
		member(ok - Switch, Map).

%%%%%%%%%%%%%%declare_attackers/4 (0) 06/03/11

	% Updates one creature's State as "attacking" and adds it to the
	%  list of attacking creatures.
	declare_attackers(Player, Map, Identified, [Switch | Rest]):-
		member(Name - Switch, Map),
		member([Name - Switch, Id], Identified),
		Attacker = object(Name - Id, _State),
		attacker(Attacker, Player, _Updated),
		attackers(Attackers),
		append(Attackers, [Name - Id], New_attackers),
		update_attackers(New_attackers),
		remove(Name - Switch, Map, New_map),
		remove([Name - Switch, Id], Identified, New_identified),
		declare_attackers(Player, New_map, New_identified, Rest).
		% ^ and this should be taking a new attacker as an argument
		%  and appending it to the existing list of attackers, then
		%  returning the new list (if necessary).


%%%%%%%%%%%%%%update_attackers/1 03/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* update_attackers(+Attackers) */
	% Updates the list of attacking creatures

	update_attackers(Attackers):-
		retractall(attackers(_)),
		asserta(attackers(Attackers)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%attacker/3 01/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/*  attacker(+Creature, +Controller, -Updated) */
	% Adds a Creature to the list of attackers, updates its
	%  state to "attacking" and returns the updated state.

	attacker(Creature, Controller, Updated):-
		% ^ Controller used to check that only the active player's
		%  creatures can be declared as attackers!
		creature(Creature, _, Abilities, _, _, Controller),
		Creature =.. [_,Name - Id, _], %unused?
		tap_untap(Creature, Controller, tap, Tapped),
		add_state(Tapped, attacking, Updated),
		change_state(Tapped, 'Battlefield', Controller, Updated).
	% Each creature in the attackers list is an attacking creature.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%creature/6 02/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* creature(+Creature, ?Type, ?Abilities, ?Power, ?Toughness, ?Controller) */
	% Returns information about a creature on the battlefield.

	creature(Creature, Type, Abilities, Power, Toughness, Controller):-
		 Creature =.. [object, Name - _Id, _state],
		zone(Controller, 'Battlefield', Permanents),
		member(Creature, Permanents),
		card([card_name Name, _, type_line Type, text_box Abilities,
				power [Power], toughness [Toughness], _, _State]).
	% Ah, now. There's no type checking here- this will return information
	%  about any Permanent.

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	Btw, you know that you can query, with eg:
	creature(object('Maritime Guard',Id,State),
						Type, Abilities, P, T, 'Player 2').
	To get the creature's Id, or State, or Name or Whatever.
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%declare_blockers/2 06/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* declare_blockers(+Non_active_player, -Blockers) */
	% Allows the non-active player to choose blockers.

%%%%%%%%%%%%%%declare_blockers/2 (1) 23/03/11

	% The non-active player declares blockers,
	declare_blockers(Player, Blockers):-
		one_blocker(Player, Blocker),
		Blocker \= [],
		one_attacker(Player, Blocker, Attacker),
		Blocker = object(Blocker_name - Id, _State),
		blocker(Attacker, Blocker, _Updated),
		append(Blockers, [Blocker_name - Id], New_blockers),
		declare_blockers(Player, New_blockers).

%%%%%%%%%%%%%%declare_blockers/2 (0) 23/03/11

	% Players choose order of damage to creatures.
	% Most of the time, only damage to blockers needs to be
	%  ordered; the exception is creatures with multi-blocking abilities.
	declare_blockers(_Player, Blockers):-
		update_blockers(Blockers),
		% removed redundant clauses here; watch.
		output(declare_blockers, [blocking, Blockers]),
		attackers(Attackers),
		order_blockers(Attackers, [], Blockers_order),
		update_blockers_order(Blockers_order),
		%sort(Blockers, Sorted_blockers),
		order_attackers(Blockers, [], Attackers_order),
		update_attackers_order(Attackers_order).


%%%%%%%%%%%%%%one_blocker/2 06/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* one_blocker(+Controller, -Blocker) */
	% Lets the non-active player choose one blocker

	% Choose one blocker
	one_blocker(Player, Blocker):-
		all_blockers(Player, [Map, Switches], Identified), %!, % green cut
		one_blocker(Player, Map, Switches, Identified, Blocker), !.
		% Red cut there. If Blocker = [] then declare_blockers, will fail
		%  after this call and try to backtrack in here.


%%%%%%%%%%%%%%all_blockers/3 06/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* all_blockers(+Non_active_player, +Switches, -Identified) */
	% Identifies all creatures that can be declared as blockers.

	% All creatures on the defender's side of the Battlefield
	all_blockers(Player, Switches, Identified):-
		zone(Player, 'Battlefield', Objects),
		% ^ Temp. Check that multi-blocking is not legal.
		findall([Permanent, Id],
				(member(object(Permanent - Id,State), Objects),
				creature_type(Permanent),
				legal_blocker(State)),
			Permanents),
		findall(Creature, member([Creature, Id], Permanents), Creatures),
		append([cancel, ok], Creatures, Full),
		prompt_switches(Full, Switches), % Switches: [Map, Switches]
		Switches = [Map, _Switches],
		identify_object(Map, Permanents, 2, [], Identified).
		% Identified: list of [Creature - Switch, Id]


%%%%%%%%%%%%%%legal_blocker/X
%%%%%%%%%%%%%%%%%%%%%%%

	legal_blocker(State):-
		\+ member(tapped, State),
 		\+ member(blocking(_,_), State).
		% ^ Already declared as blocker- temp! Check whether the
		%  creature has a multi-blocking ability.


%%%%%%%%%%%%%%one_blocker/5  06/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* one_blocker(+Player, +Map, +Switches, +Identified, -Blocker) */
	% input/3 call for one_blocker/2

%%%%%%%%%%%%%%one_blocker/5 (0) 23/03/11

	% No (more) creatures to declare as blockers
	one_blocker(_Player, _Map, _Switches, [], []).

%%%%%%%%%%%%%%one_blocker/5 (1) 25/03/11

	% Gleemin is the defending player
	one_blocker('Glee-min', Map, _Switches, Identified, Blocker):-
		glee_min(combat, one_blocker, Identified, Switch),
		one_blocker(Map, Identified, Switch, Blocker).
		% Identified: list of [Name - Switch, Id]
		% Map: list of [Name - Switch]

%%%%%%%%%%%%%%one_blocker/5 (2) 06/03/11

	% Choose one creature to be a blocker
	one_blocker(Player, Map, Switches, Identified, Blocker):-
		Context = [Player, Switches, Identified],
		input(one_blocker, Context, Input) ->
		% ^ This should actually be string_input to allow for multi-blockers
		atom_codes(Switch, [Input]),
		one_blocker(Map, Identified, Switch, Blocker);
		one_blocker(Player, Map, Switches, Identified, Blocker).


%%%%%%%%%%%%%%one_blocker/4 06/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* one_blocker(+Player, +Map, +Switches, +Identified, -Blocker) */
	% Processes the player's input from one_blocker/5

%%%%%%%%%%%%%%one_blocker/4 (0) 23/03/11

	% The player chooses "ok"
	one_blocker(_Map, _Identified, o, []).

%%%%%%%%%%%%%%one_blocker/4 (1) 06/03/11

	% Identify the creature from the Switch chosen.
	one_blocker(Map, Identified, Switch, Blocker):-
		member(Name - Switch, Map),
		member([Name - Switch, Id], Identified),
		Blocker = object(Name - Id, _State).


%%%%%%%%%%%%%%one_attacker/2 06/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* one_blocker(+Non_active_player, +Blocker, -Attacker) */
	% Lets the player chooose one attacker to be blocked by one blocker

	% Choose which attacker to block with a single blocker.
	one_attacker(Player, Blocker, Attacker):-
		all_attackers([Map, Switches], Identified),
		one_attacker(Player, Blocker, Map, Switches, Identified, Attacker).


%%%%%%%%%%%%%%all_attackers/2 06/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* all_attackers(+Switches, -Identified) */
	% Identifies all attacking creatures

	all_attackers(Switches, Identified):-
		attackers(Attackers),
		findall( [Permanent, Id], member(Permanent - Id, Attackers), Permanents),
		findall(Creature, member(Creature - Id, Attackers), Creatures),
		append([cancel, ok], Creatures, Full),
		prompt_switches(Full, Switches), % Switches: [Map, Switches]
		Switches = [Map, _Switches],
		identify_object(Map, Permanents, 2, [], Identified).
		% Identified: list of [Permanent - Switch, Id]


%%%%%%%%%%%%%%one_attacker/5 06/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* one_attacker(+Blocker, +Map, +Switches, +Identified, -Attacker) */
	% input/3 call for one_attacker/2

%%%%%%%%%%%%%%one_attacker/5 (1) 23/03/11

	% Only one creature is attacking.
	one_attacker(_Player, _Blocker, _Map, _Switches, Identified, Attacker):-
		length(Identified, 1),
		member(Creature, Identified),
		Creature = [Name - _Switch, Id],
		Attacker = object(Name - Id, _State).

%%%%%%%%%%%%%%one_attacker/5 (2) 23/03/11

	% Glee-min is the defending player
	one_attacker('Glee-min', Blocker, Map, _Switches, Identified, Attacker):-
		glee_min(combat, one_attacker, [Blocker, Identified], Switch),
		one_attacker(Map, Identified, Switch, Attacker).

%%%%%%%%%%%%%%one_attacker/5 (3) 06/03/11

	% Choose one attacker to block with a blocker
	one_attacker(Player, Blocker, Map, Switches, Identified, Attacker):-
		Context = [Player, Blocker, Switches, Identified],
		input(one_attacker, Context, Input) ->
		atom_codes(Switch, [Input]),
		one_attacker(Map, Identified, Switch, Attacker);
		one_attacker(Player, Blocker, Map, Switches, Identified, Attacker).


%%%%%%%%%%%%%%one_attacker/4 06/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* one_attacker(+Map, +Identified, +Switch, -Attacker) */
	% Processes the player's input from one_attacker/5

	% Identifies the creature from the Switch chosen.
	one_attacker(Map, Identified, Switch, Attacker):-
		member(Name - Switch, Map),
		member([Name - Switch, Id], Identified),
		Attacker = object(Name - Id, _State).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%blocker/3 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* blocker(+Creature, +Controller, +Opponent) */
	% Adds a Creature to the list of blockers updates its
	%  state with blocking information and returns the updated state.

	blocker(Attacker, Blocker, Blocker_update):-
		Attacker =.. [_, Attacker_name - Attacker_Id ,_],
		Blocker =.. [_, Blocker_name - Blocker_Id ,_],
		% Update attacker's state:
		add_state(Attacker, blocked_by(Blocker_name, Blocker_Id ), Attacker_update),
		change_state(Attacker, 'Battlefield', _Opponent, Attacker_update),
		% Update blocker's state:
		add_state(Blocker, blocking(Attacker_name, Attacker_Id), Blocker_update),
		change_state(Blocker, 'Battlefield', _Controller, Blocker_update).


%%%%%%%%%%%%%%update_blockers/1 03/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* update_blockers(+Blockers) */
	% Updates the list of blocking creatures

%%%%%%%%%%%%%%order_blockers/1 (1) 03/03/11

	% No blockers are declared
	update_blockers(Blockers):-
		type(Blockers, 0),
		retractall(blockers(_)),
		asserta(blockers([])).

%%%%%%%%%%%%%%order_blockers/1 (2) 03/03/11

	update_blockers(Blockers):-
		retractall(blockers(_)),
		asserta(blockers(Blockers)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%order_blockers/3 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* order_blockers(+Attackers, [], -Blockers_order) */
	% Allows the active player to choose the damage assignment order for blockers

%%%%%%%%%%%%%%order_blockers/3 (0) 13/02/11

	% No more attackers for which to order blockers
	order_blockers([], Order, Order):-
		output(order_blockers, [ordered, Order]).
	% Incidentally, empty clauses trigger when there are no blockers,
	%  or attackers, below in order_attackers.

%%%%%%%%%%%%%%order_blockers/3 (1) 13/02/11

	% Order the blockers for the next attacker
	order_blockers([Attacker | Rest], Temp, Order):-
		Attacker = Name - Id,
		creature(ATtacker, _, _, _, _, _),
		% ^ returns the current state, in the db.
		ATtacker =.. [_,Name - Id, State],
		findall([Blocker_name - Blocker_id],
			(member(blocked_by(Blocker_name, Blocker_id), State),
			creature(Blocker, _, _, _, _, _),
			Blocker =.. [_,Blocker_name - Blocker_id,_Blocker_state]),
			Blockers), % All the attacker's blockers.
		reverse(Blockers, Srekcolb),
		order_blockers(Attacker, Srekcolb, [], Order_1), !,
		% ^ Hack, due to the way states are updated in change_state/4
		%  ie, newest states are added at the beginnign of the state list.
		append(Temp, [Order_1], New_temp),
		order_blockers(Rest, New_temp, Order).


%%%%%%%%%%%%%%order_blockers/4 (1) 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* order_blockers(+Attacker, +Blockers, [], -Blockers_order) */
	% Allows the active player to choose the damage assignment order
	%  for the creatures blocking one attacker

%%%%%%%%%%%%%%order_blockers/4 (0) 23/03/11

	% Only one blocker- nothing to order
	order_blockers(Attacker, Blockers, [], [Attacker, [Name - Id]]):-
		length(Blockers, 1),
		member([Name - Id], Blockers).

%%%%%%%%%%%%%%order_blockers/4 (0) 13/02/11

	% No more blockers to order
	order_blockers(Attacker, [], Order, [Attacker, Order]).

%%%%%%%%%%%%%%order_blockers/4 (1) 06/03/11

	order_blockers(Attacker, Blockers, Temp, Order):-
		findall(Name, member([Name - _Id], Blockers), BLockers),
		append([ok], BLockers, Full),
		prompt_switches(Full, Switches),
		blockers_order(Attacker, Blockers, Switches, [Name - Id]),
		append(Temp, [Name - Id], New_temp),
		remove([Name - Id], Blockers, New_blockers), % also checks membership btw
		% Blocker : [Name - Id]
		order_blockers(Attacker, New_blockers, New_temp, Order).

%%%%%%%%%%%%%%order_blockers/4 (2) 06/03/11

	% The player accepts the ordering of blockers as presented
	order_blockers(Attacker, Blockers, _Temp, Order):-
		flatten_list(Blockers, Flat),
		% ^ fun with brackets >
		append([Attacker], [Flat], Order).


%%%%%%%%%%%%%%blockers_order/4 06/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* blockers_order(+Attacker, +Blockers, +Switches, -Blocker) */
	% input/3 call for order_blockers/4

%%%%%%%%%%%%%%blockers_order/4 (2) 25/03/11

	blockers_order(Attacker, Blockers, [Map, _Switches], Blocker):-
	% controlled_by(Attacker, 'Glee-min'), % or query active_player
		active_player('Glee-min'),
		glee_min(combat, order_blockers, [Attacker, Blockers, Map], Switch),!,
		% Red cut. Switch = o causes identify_blocker to fail, then order_blockers/4
		%  fails and its last clause fires, as it should in this case. Without the
		%  cut, the next blockers_order clause is evaluated- and asks the human
		%  player to order the blockers for Gleemin.
		identify_blocker(Blockers, Switch, Map, Blocker).

%%%%%%%%%%%%%%blockers_order/4 (1) 06/03/11

	blockers_order(Attacker, Blockers, [Map, Switches], Blocker):-
		Context = [Attacker, Blockers, Map, Switches],
		input(order_blockers, Context, Input) ->
		% Input: charcode
		atom_codes(Switch, [Input]),
		identify_blocker(Blockers, Switch, Map, Blocker);
		blockers_order(Attacker, Blockers, [Map, Switches], Blocker).


%%%%%%%%%%%%%%identify_blocker/4 06/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* identify_blocker(+Blockers, +Switch, +Map, -Blocker) */
	% Returns the Blocker as a [Name - Id] pair

%	identify_blocker(Blockers, o, [_Ok | _Map], []).
	% ^ Should be doing this, but look above (blockers_order/4 (2))

	% The user choses the next blocker
	identify_blocker(Blockers, Switch, [_Ok | Map], [Name - Id]):-
		member([Name - Id], Blockers, Position),
		member(Name - Switch, Map, Position).


%%%%%%%%%%%%%%update_blockers_order/1 03/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* update_blockers_order(+Order) */

	update_blockers_order(Order):-
		retractall(blockers_order(_)),
		asserta(blockers_order(Order)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%order_attackers/3 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* order_attakers(+Blockers, [], -Attackers_order) */
	% Allows the defending player to choose the damage assignment order for attackers
	% Most creatures can only block one attacker so this is not needed often.

%%%%%%%%%%%%%%order_attakers/3 (0) 13/02/11

	% No more blockers
	order_attackers([], Order, Order):-
		output(order_attackers, [ordered, Order]).

%%%%%%%%%%%%%%order_attakers/3 (1) 13/02/11

	% Take the next Blocker and order the creatures it's blocking
	order_attackers([Blocker | Rest], Temp, Order):-
		Blocker = Name - Id,
		creature(BLocker, _, _Abilities, _, _, _),
		% ^ returns the current state, in the db.
		BLocker =.. [_,Name - Id, State],
		findall([Attacker_name - Attacker_id],
			(member(blocking(Attacker_name, Attacker_id), State),
			creature(Attacker, _, _, _, _, _),
			Attacker =.. [_,Attacker_name - Attacker_id,_Attacker_state]),
			Attackers), % All creatures blocked by Blocker
		%sort(Attackers, Sorted_attackers),
		reverse(Attackers, Srekcatta),
		% ^ hack due to change_state/4 (see order_blockers).
		order_attackers(Blocker, Srekcatta, [], Order_1), !,
		append(Temp, [Order_1], New_temp),
		order_attackers(Rest, New_temp, Order).


%%%%%%%%%%%%%%order_attackers/4 (1) 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* order_attackers(+Blocker, +Attackers, [], -Attackers_order) */
	% Order attackers for one blocker.

%%%%%%%%%%%%%%order_attakers/4 (0) 23/03/11

	% Only one attacker
	order_attackers(Blocker, Attackers, [], [Blocker, [Name - Id]]):-
		length(Attackers, 1),
		member([Name - Id], Attackers).

%%%%%%%%%%%%%%order_attakers/4 (0) 13/02/11

	% No more Attackers
	order_attackers(Blocker, [], Order, [Blocker, Order]). % No more attackers

%%%%%%%%%%%%%%order_attakers/4 (1) 06/03/11

	order_attackers(Blocker, Attackers, Temp, Order):-
		findall(Name, member([Name - _Id], Attackers), ATtackers),
		append([ok], ATtackers, Full),
		prompt_switches(Full, Switches),
		attackers_order(Blocker, Attackers, Switches, [Name - Id]),
		append(Temp, [Name - Id], New_temp),
		remove([Name - Id], Attackers, New_attackers), % also checks membership btw
		% Blocker : [Name - Id]
		order_attackers(Blocker, New_attackers, New_temp, Order).
	% Problem here? When the same creature blocks two or more attackers,
	%  its name is sent on as two different blockers.

%%%%%%%%%%%%%%order_attackers/4 (2) 06/03/11

	% The player accepts the ordering of attackers as presented
	order_attackers(Blocker, Attackers, _Temp, Order):-
		flatten_list(Attackers, Flat),
		% ^ fun with brackets >
		append([Blocker], [Flat], Order).


%%%%%%%%%%%%%%attackers_order/4 06/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* attackers_order(+Blocker, +Attackers, +Switches, -Attacker) */
	% input/3 call for order_blockers/4

	attackers_order(Blocker, Attackers, [Map, Switches], Attacker):-
		Context = [Blocker, Attackers, Map, Switches],
		input(order_attackers, Context, Input) ->
		% Input: charcode
		atom_codes(Switch, [Input]),
		identify_attacker(Attackers, Switch, Map, Attacker);
		attackers_order(Blocker, Attackers, [Map, Switches], Attacker).


%%%%%%%%%%%%%%identify_attacker/4 06/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* identify_Attacker(+Attackers, +Switch, +Map, -Attacker) */
	% Returns the Blocker as a [Name - Id] pair

	% The user choses the next blocker
	identify_attacker(Attackers, Switch, [_Ok | Map], [Name - Id]):-
		member([Name - Id], Attackers, Position),
		member(Name - Switch, Map, Position).


%%%%%%%%%%%%%%update_blockers_order/1 03/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* update_blockers_order(+Order) */

	update_attackers_order(Order):-
		retractall(attackers_order(_)),
		asserta(attackers_order(Order)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%combat_damage/4 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* combat_damage(+Active_player, +Opponent, +Blockers_order, +Attackers_order) */
	% Assigns combat damage to creatures, players and planeswalkers [to be implemented]
	% Both sides use this predicate

	combat_damage(Opponent, Blockers_order, Attackers_order):-
		% Assign damage to blocking creatures and defending player >
		assign_damage(Blockers_order, Opponent),
		% Assign damage to attacking creatures >
		assign_damage(Attackers_order, _Active_player),
		damage_assignments(Assignments),
		damage_assignment(Assignments).
	% Blockers_order and Attackers_order are of the form:
	% 	[Attacker, [Blockers] ] and [Blocker, [Attackers] ]
	%  so any information about each Attacker and Blocker can be
	%  extracted from them.


%%%%%%%%%%%%%%assign_damage/2 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* assign_damage(Bestower, Recipients) */
	% A bestower is a creature that deals damage, a recipient is a
	%  creature, player or planeswalker that receives it.

%%%%%%%%%%%%%%assign_damage/2 (0) 13/02/11

	% No more attackers or blockers.
	assign_damage([], _).

	% Bestower: the creature using its Power to deal damage
	% Recipient: the creature suffering the damage against its Toughness.

%%%%%%%%%%%%%%assign_damage/2 (1) 13/02/11

	% Bestower is engaged but the Recipient is not there anymore
	assign_damage([ [Bestower, Recipients] | Rest], Opponent):-
		blocked(Bestower),
		member(Recipient, Recipients),
		Recipient == '',	% OK, probably stupid
		assign_damage(Rest, Opponent).

%%%%%%%%%%%%%%assign_damage/2 (2) 13/02/11

	% Bestower is engaged by exactly one Recipient
	assign_damage([ [Bestower, Recipients] | Rest], Opponent):-
		(blocked(Bestower); blocking(Bestower)),
		length(Recipients, 1),
		Recipients = [Recipient],
		% Bestower: Name - Id,
		creature(Bestower, _,_,_,_,Bestower_P,_,_),
		store_damage_assignment([Bestower, Recipient, Bestower_P]),
		assign_damage(Rest, Opponent).

%%%%%%%%%%%%%%assign_damage/2 (3) 15/02/11

	% Bestower is blocked by more than one blocker
	assign_damage([ [Bestower, Recipients] | Rest], Opponent):-
		(blocked(Bestower); blocking(Bestower)),
		% Bestower: Name - Id,
		creature(Bestower, _,_,_,_,Bestower_P,_,_),
		% ^ I have this call in lethal_damage too...
		lethal_damage(Bestower, Recipients, Bestower_P),
		assign_damage(Rest, Opponent).

%%%%%%%%%%%%%%assign_damage/2 (4) 13/02/11

	% Bestower is not engaged and is an attacker
	assign_damage([ [Bestower, _] | Rest], Opponent):-
		% Bestower: Name - Id,
		creature(Bestower, _,_,_,_,Bestower_P,_,_),
		attackers(Attackers),
		member(Bestower, Attackers),
		store_damage_assignment([Bestower, Opponent, Bestower_P]),
		assign_damage(Rest, Opponent).

%%%%%%%%%%%%%%assign_damage/2 (5) 13/02/11

	% Bestower is not engaged and is a blocker
	assign_damage(_, _).


%%%%%%%%%%%%%%blocked/1 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* blocked(+Creature) */
	% True if Creature is a blocked creature

	blocked(Bestower):-
		object_state(Bestower, State),
		member(blocked_by(_Blocker, _Id), State).


%%%%%%%%%%%%%%blocking/1 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* blocking(+Creature) */
	% True if Creature is a blocking creature

	blocking(Bestower):-
		object_state(Bestower, State),
		member(blocking(_Attacker, _Id), State).


%%%%%%%%%%%%%%store_damage_assignment/1 03/04/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* store_damage_assignment(+Assignment) */
	% Assignment: list of [Dealer, Recipient, Damage]

	% Store the damage assignment to the list of dmg assignments
	store_damage_assignment(Assignment):-
		damage_assignments(Assignments),
		append(Assignments, [Assignment], New_assignments),
		retractall(damage_assignments(Assignments)),
		asserta(damage_assignments(New_assignments)).


%%%%%%%%%%%%%%lethal_damage/3 15/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* lethal_damage(+Bestower, +Recipients, +Bestower_Power) */
	% Causes a creature to deal lethal damage to each of their
	%  attackers or blockers in turn of their damage assignment order

%%%%%%%%%%%%%%lethal_damage/3 (1) 26/03/11

	% Only one Recipient remains to receive the Bestower's damage
	lethal_damage(Bestower, Recipients, Bestower_P):-
		length(Recipients, 1),
		Bestower_P \= 0, %just in case?
		member(Recipient, Recipients),
		store_damage_assignment([Bestower, Recipient, Bestower_P]).
		% ^ Deal all the Bestower's remaining Power as damage to
		%  the Recipient- no assignment options at this point.

%%%%%%%%%%%%%%lethal_damage/3 (2) 13/02/11

	% no more Power
	lethal_damage(Bestower, Recipients, 0).

%%%%%%%%%%%%%%lethal_damage/3 (3) 23/03/11

	% Assign lethal damage to each Recipient, before moving on
	%  to the next; stop when there are no more Recipients or when the
	%  Bestower is out of Power.
	lethal_damage(Bestower, [Recipient | Rest], Bestower_P):-
		creature(Recipient,State,_,_,_,_,Recipient_T,_),
		( member(damage(Amount), State) ->
		Recipient_T is Toughness - Amount ; % wut?? o_.
		Recipient_T = Toughness), % Added on 26/03/11
		% ^ If the creature has damage on it take it into account, effectively reducing
		%  its toughness for the purpose of dealing lethal damage to it.
		creature(Bestower, _,_,_,_,_,_,Controller),
		lethal_damage(Controller, Bestower, Recipient, Bestower_P, Recipient_T, New_bestower_P),
		lethal_damage(Bestower, Rest, New_bestower_P).


%%%%%%%%%%%%%%lethal_damage/4 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* lethal_damage(Recipient, Bestower_power, Recipient_toughness, New_Bestower_toughness) */
	% Assigns lethal damage to a recipient, keeping track of the bestower's power

%%%%%%%%%%%%%%lethal_damage/2 (1) 13/02/11

	% Assign lethal damage to one Recipient
	% The Bestower's Power is not enough or just enough to kill
	%  the Recipient
	lethal_damage(_Controller, Bestower, Recipient, Bestower_P, Recipient_T, 0):-
		Bestower_P =< Recipient_T,
		store_damage_assignment([Bestower, Recipient, Bestower_P]).
	% No need to let the player decide- all the Bestower's Power must
	%  be assigned as damage to the first recipient

%%%%%%%%%%%%%%lethal_damage/2 (2) 13/02/11

	% The Attacker's Power is more than enough to kill the Blocker
	lethal_damage(Controller, Bestower, Recipient, Bestower_P, Recipient_T, New_bestower_P):-
		Bestower_P > Recipient_T,
		object_handle(BEstower, Bestower),
		object_handle(REcipient, Recipient),
		% ^ This needs fixing here and in input_output.pl and in glee-min.pl
		assign_lethal_damage(Controller, BEstower, REcipient, Bestower_P, Recipient_T, Damage),
		Damage =< Bestower_P, Damage >= Recipient_T,
		% ^ Damage is no more than the Bestower's Power and is lethal
		store_damage_assignment([Bestower, Recipient, Recipient_T]),
		New_bestower_P is Bestower_P - Recipient_T.
	% In this case, the player can choose to assign enough dmg to
	%  the Recipient to kill it, or more.


%%%%%%%%%%%%%%assign_lethal_damage/6 13/02/11?
%%%%%%%%%%%%%%%%%%%%%%%
	/* assing_lethal_damage(+Bestower, +Recipient, -Damage) */
	% input/3 call for lethal_damage/5

%%%%%%%%%%%%%%assign_lethal_damage/6 (1) 26/03/11

	assign_lethal_damage('Glee-min', Bestower, Recipient, Bestower_P, _Recipient_T, Damage):-
		glee_min(combat, assign_lethal, [Bestower, Recipient, Bestower_P], Damage).

%%%%%%%%%%%%%%assign_lethal_damage/6 (2) 13/02/11?


	assign_lethal_damage(Controller, Bestower, Recipient, Bestower_P, Recipient_T, Damage):-
		Context = [Controller, Bestower, Recipient, Bestower_P, Recipient_T],
		input(assign_lethal_damage, Context, Input),
		% ^ Won't this fail on more than single-digit numbers?
		number_codes(Damage, [Input]);
		assign_lethal_damage(Controller, Bestower, Recipient, Bestower_P, Recipient_T, Damage).
	% Should check that the damage amount entered is legal (ie
	%  not more than the Bestower's power, not less than the
	%  Recipient's toughness. If we get here the Bestower does have
	%  enough power to deal lethal damage to the recipient (or at
	%  least attempt to do so, in which case all of its remaining
	%  power must be assigned to the Recipient).


%%%%%%%%%%%%%%damage_assignment/1 03/04/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* damage_assignment(+Damage_assignments) */
	% Damage_assignment: list of [Dealer, Receiver, Damage]

	% Damage is assigned simultaneously
	damage_assignment([]).
	damage_assignment([Assignment | Rest]):-
		Assignment = [Dealer, Recipient, Damage],
		damage(Dealer, Recipient, Damage),
		damage_assignment(Rest).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%combat_ends/0 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% Combat ends. This should handle triggers etc.
	% Also perform cleanups of attacking, blocking, blocked_by
	%  etc states.

	combat_ends:-
		update_attackers([]),
		update_blockers([]),
		update_attackers_order([]),
		update_blockers_order([]),
		clear_damage_assignments,
		clear_combat_states,
%		write(`Removing survivors from combat`), nl.
		output(combat_ends,[clear]).


%%%%%%%%%%%%%%update_damage_assignments/0 03/04/11
%%%%%%%%%%%%%%%%%%%%%%%

	clear_damage_assignments:-
		damage_assignments(Assignments),
		retractall(damage_assignments(Assignments)),
		asserta(damage_assignments([])).


%%%%%%%%%%%%%%clear_combat_states 07/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	% Clears all the state information asserted during combat from
	%  creatures' State lists.

%%%%%%%%%%%%%%clear_combat_states (1) 07/03/11

	clear_combat_states:-
		zone(Player, 'Battlefield', Objects),
		member(Object, Objects),
		remove_state_all(Object, attacking, Clear_1),
		remove_state_all(Clear_1, blocking, Clear_2),
		remove_state_all(Clear_2, blocking(_,_), Clear_3),
		remove_state_all(Clear_3, blocked_by(_,_), Clear_4),
		% ^ remove combat states from one Object's state list
		change_state(Object, 'Battlefield', Player, Clear_4),
		% ^ Update the Object
		fail.

%%%%%%%%%%%%%%clear_combat_states (0) 07/03/11

	clear_combat_states.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              Notes                %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% I think this is used in the auto-assignment?
/*
%%%%%%%%%%%%%%lethal_damage/4 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	lethal_damage(Recipient, Bestower_power, Recipient_toughness, New_Bestower_toughness)
	% Assigns lethal damage to a recipient, keeping track of the bestower's power

%%%%%%%%%%%%%%lethal_damage/2 (1) 13/02/11

	% Assign lethal damage to one Recipient
	% The Bestower's Power is not enough to kill the Recipient
	lethal_damage(Recipient, Bestower_P, Recipient_T, 0):-
		Bestower_P =< Recipient_T,
		deal_damage(Recipient, Bestower_P).

%%%%%%%%%%%%%%lethal_damage/2 (2) 13/02/11

	% The Attacker's Power is more than enough to kill the Blocker
	lethal_damage(Recipient, Bestower_P, Recipient_T, New_bestower_P):-
		Bestower_P > Recipient_T,
		deal_damage(Recipient, Recipient_T),
		New_bestower_P is Bestower_P - Recipient_T.
*/

