% First saved: 02/02/2011
% Last saved:  22/04/2011
%
%	Incremental Save
%
% 	Status:
%	OK (now with Creature Combat™!)
%	Some confusion with stack_resolves; Document and fix.
%
% Doing:
%	Done removing debug_cue output
%	Noticed possible incorrect handling of mana abilities by stack/cue
%	updating player_actions (with Gleemin in mind)
%	Fixed untap (was failing on untapped permanents- watch)
% Todo:
%	implement all cleanup step actions.
%	implement all state-based actions.
%	implement Cleanup step correctly.
%	add statistics etc output options to debug.
%	Fix the slight fudge.
%	step_ends can be skipped in [] steps and let phase_ends
%	  do its job (check for [] Stack and 'Full' Cue).
%	remove asserts of active_player (once not needed any more)
% NOTES:
%	Debug -> break/0 is a bit of a hack (it returns the play code for
%	  a cancelled action). It should return its own code and be
%	  intercepted by stack and so on.
%	Keep an eye on step_ends. It's a bit confusing.
%	Fixed receives_priority hacks (keep an eye out)
%	The slight fudge: [] steps should not exist and Phase
%		actions should be taken before their steps begin.
%	  noticed that the game doesn't yet know
%	  that the cards in players' stack, exile and battlefield
%	  are also in the _shared_ stack, exile and battlefield.
% 	At some point I'll need to deal with all those unbound
% 	  variables in predicate heads...
%	I'd like to return New_play as a list
%	  in order to remember playing land this turn without
%	  using asserts/retracts.


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
%%%%           Turn facts              %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	:- dynamic active_player/1.
        :- dynamic new_game/1.
%	:- dynamic turn/7.
	% I may not need to do that after all.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%phases/1 16/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	% Phase list
	phases(['Beginning', 'First Main', 'Combat', 'Second Main', 'Ending']).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%phase/1 16/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	% What phases?
	phase('Beginning').
	phase('First Main').
	phase('Combat').
	phase('Second Main').
	phase('Ending').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%phase/2 16/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	% phase(Name, Steps): steps of a phase.
	phase('Beginning', [
				'Untap',
				'Upkeep',
				'Draw'
				]).
	phase('First Main', []).
	phase('Combat', [
				'Beginning of Combat',
				'Declare Attackers',
				'Declare Blockers',
				'Combat Damage',
				'End of Combat'
				]).
	phase('Second Main', []).
	phase('Ending', [
				'End',
				'Cleanup'
				]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%step/1 16/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	% What steps?
	step('Untap').
	step('Upkeep').
	step('Draw').
	step('Beginning of Combat').
	step('Declare Attackers').
	step('Declare Blockers').
	step('Combat Damage').
	step('End of Combat').
	step('End').
	step('Cleanup').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%step/2 16/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	% step(Phase, Step): steps by phase.
	step('Beginning', 'Untap').
	step('Beginning', 'Upkeep').
	step('Beginning', 'Draw').
	step('First Main', []).
	step('Combat', 'Beginning of Combat').
	step('Combat', 'Declare Attackers').
	step('Combat', 'Declare Blockers').
	step('Combat', 'Combat Damage').
	step('Combat', 'End of Combat').
	step('Second Main', []).
	step('Ending', 'End').
	step('Ending', 'Cleanup').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%actions/2 16/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%actions/2 (1) 16/12/2010

	% actions(Phase, Action), Game Actions by phase.
	actions('Beginning', beginning).
	actions('First Main', first_main).
	actions('Second Main', second_main).

%%%%%%%%%%%%%%actions/2 (2) 16/12/2010

	% actions(Step, Action) Game Actions by step.
	actions('Untap', untap).
	actions('Upkeep', upkeep).
	actions('Draw', draw).
	actions('Beginning of Combat', begin_combat).
	actions('Declare Attackers', declare_attackers).
	actions('Declare Blockers', declare_blockers).
	actions('Combat Damage', damage).
	actions('End of Combat', end_combat).
	actions('End', end).
	actions('Cleanup', cleanup).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%game_action/2 16/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	game_action(untap).
	game_action(upkeep).
	game_action(draw).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%        Turn rules (1)             %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%turn/7 30/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* turn(+Player, +Phase, +Step, +Priority, +Cue, +Play, +Stack) */
	% Implements a turn sequence.

%%%%%%%%%%%%%%turn/4 (0) 15/02/2011

	% A player has conceded; the turn sequence ends
	turn(_, _, _, _, _, 67, _):- !. % 67: [C]oncede

%%%%%%%%%%%%%%turn/4 (1) 08/03/2011

	turn(_Player, _Phase, _Step, loss(Player, Condition), _Cue, _Play, _Stack):-
			%Loss == loss(Player, Condition),
			%Loss =.. [loss,Player,Condition],
			write(Player - ` loses the game because of ` - Condition), nl, !.
			%output(turn_sequence, [loss, Player]).

%%%%%%%%%%%%%%turn/4 (2) 29/12/2010

	turn(Player, Phase, Step, Priority, Cue, Play, Stack):-
		turn_begins(Player, Step), !,
			phase_begins(Phase, Step), !,
				step_begins(Step), !,
					game_actions(Phase, Step), !,
					receives_priority(Player, Step, Priority, Play, Stack, New_priority), !,
					player_actions(Player, New_priority, Step, Phase, New_play), !,
					stack_resolves(Stack, New_play, Cue, New_priority, New_cue, New_stack), !,
				step_ends(Step, Phase, New_cue, New_priority, New_stack, New_step), !,
			phase_ends(Phase, Step, New_phase), !,
		turn_ends(Player, Phase, Step, New_player), !,
	turn(New_player, New_phase, New_step, New_priority, New_cue, New_play, New_stack).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%turn_begins/2 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* turn_begins(+Player, +Step) */
	% "Every untap step there's a new active player"
% Marks the player whose turn it is as the active player.

%%%%%%%%%%%%%%turn_begins/2 (1) 25/12/2010

	% If this is the beginning of the first untap phase in a game,
	%  the active player is the first player in the
	%  order_of_play, so don't look for a new active player.
	turn_begins(Player, begins('Untap')):-
		new_game(yes),
		assert_active_player(Player),
		output(turn_begins, ['new', Player]),
		retractall(new_game(_)),
		assert(new_game(no)).

%%%%%%%%%%%%%%turn_begins/2 (2) 25/12/2010

	% If it's the beginning of the untap step,
	%  there's a new active player:
	turn_begins(Player, begins('Untap')):-
		assert_active_player(Player),
		output(turn_begins, ['new', Player]).

%%%%%%%%%%%%%%turn_begins/2 (3) 02/01/2011

	% During a Step other than the Untap or Cleanup
	% report the active player
	turn_begins(Player, Step_state):- % Swi, 19/06/11
	         Step_state =.. [State, Step],
		(State == begins ;
		State == ongoing),
		(Step \== 'Untap',
		Step \== 'Cleanup'),
		assert_active_player(Player),
		output(turn_begins, ['current', Player]).
	% What about Cleanup steps where players receive
	%  priority? Should report (from another pred).

%%%%%%%%%%%%%%turn_begins/2 (4) 25/12/2010

	% Otherwise, there is no new active player
	%  and no need to report the active player
	turn_begins(Player, Step):-
		true.


%%%%%%%%%%%%%%assert_active_player/1 28/01/2011

	% If there is an active player in the kb
	assert_active_player(Player):-
		retractall(active_player(_)),
		asserta(active_player(Player)).

	% If there is no active player in the kb
	assert_active_player(Player):-
		asserta(active_player(Player)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%phase_begins/2 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* phase_begins(+Phase, +Step) */
% "A phase ends at its last Step and then there is a new phase.

%%%%%%%%%%%%%%phase_begins/2 (1) 30/12/2010

	% A phase begins when its first step begins
	phase_begins(Phase, begins(First_step)):-
		phase(Phase, [ First_step | Rest ]),
		output(phase_begins, [Phase]).

%%%%%%%%%%%%%%phase_begins/2 (2) 25/12/2010

	% A phase with no steps begins here
	phase_begins(Phase, begins([])):-
		output(phase_begins, [Phase]).

%%%%%%%%%%%%%%phase_begins/3 (2) 25/12/2010

	% Otherwise, it continues.
%	phase_begins(Phase, State(Step)):- % Swi, 19/06/11
	phase_begins(Phase, Step_state):-
		Step_state =.. [State, Step],
		output(phase_begins, [State, Step, Phase]).
	% Could check membership of Step to Phase
	%  to make sure we're in the right phase/ step.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%step_begins/1 30/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* step_begins(+Step) */
	% Marks the beginning of the Step
	% Also deals with "at end of Step" effects- to be implemented.

%%%%%%%%%%%%%%step_begins/2 (1) 30/12/2010

	% Some phases have no steps:
%	step_begins(State([])):-  % Swi, 19/06/11
	step_begins(State_step):-
	      State_step =.. [_State, []],
	     output(step_begins, []).

%%%%%%%%%%%%%%step_begins/2 (2) 30/12/2010

	% A step begins:
	step_begins(begins(Step)):-
	output(step_begins, [Step]).

%%%%%%%%%%%%%%step_begins/2 (3) 30/12/2010

	% Otherwise, the step goes on
	step_begins(ongoing(Step)):-
	output(step_begins, [ongoing, Step]).

%%%%%%%%%%%%%%step_begins/2 (4) 30/12/2010

	% The end of the step is reported later.
	step_begins(ends(Step)):-
		true.

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	Note rule 103.7a: In a two-player game,
	 the player who plays first skips the draw step
	 of his or her first turn.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%game_actions/2 30/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* game_actions(+Phase, +Step) */
	% Take the necessary game actions in this phase or step.
	% This predicate will need to return a game state/ board position.

%%%%%%%%%%%%%%game_actions/2 (1) 30/12/2010

	% If this is the beginning of the first step of the current phase
	%  and there are turn-based_actions to take in this phase
	%  take them, then take the turn-based actions in the
	%  first step of that phase.
	game_actions(Phase, begins(First_step)):-
		phase(Phase, [ First_step | Rest ]),
		phase_actions(Phase),
		step_actions(First_step).

%%%%%%%%%%%%%%game_actions/2 (2) 30/12/2010

	% If the current phase has no steps,
	%  take the turn-based actions for this phase
	%  when its non-step "begins".
	game_actions(Phase, begins([])):-
		phase_actions(Phase).
	% Slight fudge. State-based actions
	%  in phases without steps happen
	%  when those phases begin or end,
	%  not when their non-existent steps do.

%%%%%%%%%%%%%%game_actions/2 (3) 30/12/2010

	% Otherwise, take the turn-based actions that need to
	%  be taken when the current step begins.
	game_actions(Phase, begins(Step)):-
		step_actions(Step).

%%%%%%%%%%%%%%game_actions/2 (X) 09/03/11

	% At the end of the End of Combat step, all creaures
	%  and planeswalkers are removed from combat
	game_actions(_Phase, ends('End of Combat')):-
		end_actions,
		combat_ends.

%%%%%%%%%%%%%%game_actions/2 (X) 09/03/11

	% At the end of the Cleanup step, players discard
	%  down to seven cards, then other cleanup
	%  actions are performed
	game_actions(_Phase, ends('Cleanup')):-
		end_actions,
		discard_to_max,
		clear_damage,
		at_end_of_turn.

%%%%%%%%%%%%%%game_actions/2 (4) 30/12/2010

	% When a phase or step ends,
	% all mana pools empty.
	game_actions(Phase, ends(Step)):-
		end_actions,
		output(end_actions, []).
	% Slight fudge again. end-of-phase
	%  actions will happen at the end
	%  of a phase.

%%%%%%%%%%%%%%game_actions/2 (5) 30/12/2010

	% When a non-step ends,
	% all mana pools empty.
	game_actions(Phase, ends([])):-
		end_actions,
		output(end_actions, []).

%%%%%%%%%%%%%%game_actions/2 (6) 30/12/2010

	% No turn-based actions are taken
	%  in the middle of a step or phase.
	game_actions(Phase, ongoing(Step)):-
		true.


%%%%%%%%%%%%%%phase_actions/1 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%%
	/* phase_actions(+Phase) */
	% Take the game actions that need to be taken in this phase.

%%%%%%%%%%%%%%phase_actions/1 (1) 25/12/2010

	% Take the necessary phase actions.
	phase_actions(Phase):-
		actions(Phase,Action),
		output(phase_actions, [Phase]),
		Action.
		% ^^ Reflect-Fu!!
		% There _is_ no separation
		% 'twixt data and operation :P

%%%%%%%%%%%%%%phase_actions/1 (2) 25/12/2010

	% If there are no actions in the current phase just succeed
	phase_actions(Phase):-
		true.


%%%%%%%%%%%%%%beginning/0 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%%
	% Beginning phase actions

	beginning:-	%	true.
	remove_summoning_sickness.

%%%%%%%%%%%%%%remove_summoning_sickness/0 09/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	% all creatures found to have summoning_sickness
	%  lose it now (they have been under their controller's control
	%  since the previous turn).

%%%%%%%%%%%%%%remove_summoning_sickness (1) 09/03/11

	remove_summoning_sickness:-
		active_player(Player),
		zone(Player, 'Battlefield', Objects),
		member(Object, Objects),
		remove_state_all(Object, 'summoning sickness', Clear),
		change_state(Object, 'Battlefield', Player, Clear),
		fail.

%%%%%%%%%%%%%%remove_summoning_sickness (0) 09/03/11

	remove_summoning_sickness.


%%%%%%%%%%%%%%first_main/0 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%%
	% First Main Phase actions

	first_main:-
		true.


%%%%%%%%%%%%%%second_main/0 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%%
	% Second Main Phase actions

	second_main:-
		true.


%%%%%%%%%%%%%%%step_actions 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%%
	/* step_actions(+Step) */
	% Take the game actions that need to be taken in the current step

%%%%%%%%%%%%%%step_actions/1 (1) 25/12/2010

	step_actions(Step):-
		actions(Step, Action),
		Action,
		output(step_actions, [Step]).

%%%%%%%%%%%%%%step_actions/1 (2) 25/12/2010

	% If there are no actions in the current step just succeed
	step_actions(Step):-
		true.


%%%%%%%%%%%%%%%untap/0 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%%
	% Untap Step actions

	untap:-
		active_player(Player),
		zone(Player, 'Battlefield', Permanents),
		findall(object(Name - Id, State),
				(member(object(Name - Id, State), Permanents ),
				member(tapped, State))
				, Tapped),
		untap(Player, Tapped).

	% Untap all of a player's permanents.
	untap(Player, []).
	untap(Player, [Permanent | Rest]):-
		tap_untap(Permanent, Player, 'untap'),
		untap(Player, Rest).
	% Needs checks for 'does not untap...' etc.


%%%%%%%%%%%%%%%upkeep/0 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%%
	% Upkeep step actions

	upkeep:-
		true.


%%%%%%%%%%%%%%%draw/0 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%%
	% Draw step actions

	draw:-
		active_player(Player),
		draw_cards(Player, 1).


%%%%%%%%%%%%%%%begin_combat/0 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%%
	% Beginning of Combat step actions

	begin_combat:- true.


%%%%%%%%%%%%%%%declare_attackers/0 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%%
	% Declare Attackers step actions

	declare_attackers:-
		active_player(Active_player),
		declare_attackers(Active_player).


%%%%%%%%%%%%%%%declare_blockers/0 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%%
	% Declare Blockers step actions

	declare_blockers:-
		attackers([]).

	declare_blockers:-
		player(Non_active_player),
		\+ active_player(Non_active_player),
		declare_blockers(Non_active_player, _Blockers).


%%%%%%%%%%%%%%%damage/0 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%%
	% Damage step actions


	damage:-
		player(Non_active_player),
		\+ active_player(Non_active_player),
		attackers_order(Attackers_order),
		blockers_order(Blockers_order),
		combat_damage(Non_active_player, Blockers_order, Attackers_order).


%%%%%%%%%%%%%%%end_combat/0 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%%
	% End Combat step actions

	end_combat:- true.


%%%%%%%%%%%%%%%end/0 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%%
	% End step actions

	end:-
		true.


%%%%%%%%%%%%%%%cleanup/0 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%%
	% Cleanup step actions

	cleanup:- true.


%%%%%%%%%%%%%%end_actions/0 30/12/2010
%%%%%%%%%%%%%%%%%%%%%%%
	% At the end of each phase and step, all mana pools empty
	% This is really not named appropriately.

	end_actions:- %true.
		player(Player),
		empty_mana_pool(Player),
		fail.

	end_actions.


%%%%%%%%%%%%%%discard_to_max/0 05/04/11
%%%%%%%%%%%%%%%%%%%%%%%
	% The active player discards down to his or her
	%  maximum hand size

	discard_to_max:-
		player(Player),
		hand_size(Player, Max_hand_size),
		zone(Player, 'Hand', Cards),
		length(Cards, Length),
		Length > Max_hand_size,
		prompt_switches(Cards, Switches),
		discard_to_max(Player, Max_hand_size, Length, Switches).

	% The player doesn't need to discard
	discard_to_max.

	discard_to_max(Player, Max_hand_size, Length, [Map, Switches]):-
		Context = [Max_hand_size, Length, Map, Switches],
		string_input(discard_to_max, Context, Input),
		atom_to_list(Input, Discards),
		Discard is Length - Max_hand_size,
		length(Discards, Length_2), Length_2 = Discard ->
		discard_to_max(Player, Map, Discards);
		discard_to_max(Player, Max_hand_size, Length, [Map, Switches]).

	discard_to_max(_Player, _Map, []).
	discard_to_max(Player, Map, Discards):-
		member(Switch, Discards),
		member(Card - Switch, Map),
		move_to_zone(Player, Card, 'Hand', 'Graveyard'),
		remove(Switch, Discards, New_discards),
		remove(Card-Switch, Map, New_map),
		discard_to_max(Player, New_map, New_discards).


%%%%%%%%%%%%%%clear_damage/0 09/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	% all damage marked on permanents
	%  (including phased-out permanents)is removed

%%%%%%%%%%%%%%clear_damage (1) 09/03/11

	clear_damage:-
		zone(Player, 'Battlefield', Objects),
		member(Object, Objects),
		remove_state_all(Object, damage(_Amount), Clear),
		change_state(Object, 'Battlefield', Player, Clear),
		fail.

%%%%%%%%%%%%%%clear_damage (0) 09/03/11

	clear_damage.


%%%%%%%%%%%%%%at_end_of_turn/0 05/04/11
%%%%%%%%%%%%%%%%%%%%%%%
	% all "until end of turn" and "this turn" effects end.

	at_end_of_turn:-
		findall([Player, Zone, Effect, Handle],
				( zone(Player, Zone, Objects),
				member(Object, Objects),
				object_handle(Object, Handle),
				object_state(Handle, State),
				( member(Effect, State),
				 Effect =.. [_,_,Duration],
				( Duration = 'at end of turn' ;
				Duration = 'until end of turn' ))),
			Effects),
		at_end_of_turn(Effects).

	at_end_of_turn([]).
	at_end_of_turn([ [Player, Zone, Effect, Handle] | Effects]):-
		remove_state(Handle, Effect, Removed),
		change_state(Handle, Zone, Player, Removed),
		at_end_of_turn(Effects).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%receives_priority/3 30/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* receives_priority(+Player, +Step, +Priority, +Play, +Stack, -New_priority) */
	% Determines which player should get priority (=New_priority).

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	 I think I should only be assigning priority when
		ongoing(Step), ongoing(Step)???
	 Players do get priority at the beginning of steps, no?
	 Depends. Normally players do receive priority at the beginning
	  of steps/ phases. The difference is that the game doesn't have
	  begins/ongoing/ends states. I use them just to know that it's not
	  time to fire turn-based actions. I could confine priority to
	  ongoing states... though that's a hack.
	Actually, that's how I have it... change it?
*/

%%%%%%%%%%%%%%receives_priority/3 (1) 01/01/2011

	% No player receives priority at the end of a step
	receives_priority(Player, ends(Step), Priority, Play, Stack, []):-
		true.

%%%%%%%%%%%%%%receives_priority/3 (2) 30/12/2010

	% No player receives priority during the untap step
%	receives_priority(Player, Status('Untap'), Priority, Play, [], []):-
	receives_priority(Player, Step_state, Priority, Play, [], []):-	% Swi, 19/06/11
	        Step_state =.. [State, 'Untap'],
		(   State = begins
		->
		output(receives_priority, [])
		;
		true).

%%%%%%%%%%%%%%receives_priority/3 (3) 30/12/2010

	% No player receives priority in the Cleanup step
	%  unless there are objects on the stack.
%	receives_priority(Player, Status('Cleanup'), Priority, Play, [], []):-
	receives_priority(Player, Step_state, Priority, Play, [], []):- %19/06/11
	        Step_state =.. [State, 'Cleanup'],
		(   State = begins
		->
		output(receives_priority, [])
		;
		true).
	% And this won't work exactly like that- the stack is built
	%  if there are objects to put on it. This should be implemented
	%  later on.

%%%%%%%%%%%%%%receives_priority/3 (4) 30/12/2010

	% The active Player receives priority at the beginning of
	%  all other steps and phases.
	receives_priority(Player, begins(_Step), _Priority, _Play, _Stack, New_priority):-
		state_based_actions(Player, New_priority),
		output(receives_priority, [New_priority]).

%%%%%%%%%%%%%%receives_priority/3 (5) 23/12/2010

	% The active Player receives priority after a spell or ability
	%  (other than a mana ability) resolves.
%	receives_priority(Player, _State(_Step), _Priority, 112, [resolved | _], New_priority):-
%	Swi, 19/06/11
	receives_priority(Player, _Step, _Priority, 112, [resolved | _], New_priority):-
		state_based_actions(Player, New_priority),
		output(receives_priority, ['new', New_priority]).
	% ^ hack fixed here; watch.

%%%%%%%%%%%%%%receives_priority/3 (6) 30/12/2010

	% During the Step, if a player passes, priority is passed
	%  to the next player in the order_of_play, after
	%  the current Priority player
	% 'Pass' == 112
%	receives_priority(_Player, _State(_Step), Priority, 112, _Stack, Final_priority):-
%	Swi, 19/06/11
	receives_priority(_Player, _Step, Priority, 112, _Stack, Final_priority):-
		state_based_actions(Priority, New_priority),
		% ^ This will eiher return the current priority player or
		%  a loss(Player,Condition) tuple.
		pass_priority(New_priority, Final_priority),
		% ^ Then this will either find the next player after Priority or
		%  return the same loss/2 tuple, causing the game to end.
		output(receives_priority, ['new', Final_priority]).

%%%%%%%%%%%%%%receives_priority/3 (7) 30/12/2010

	% During the step, if the current priority player
	%  does not pass, he or she receives priority again.
%	receives_priority(_Player, _State(_Step), Priority, _Play, _Stack, New_priority):-
%	Swi 19/06/11
	receives_priority(_Player, _Step, Priority, _Play, _Stack, New_priority):-
		state_based_actions(Priority, New_priority),
		output(receives_priority, [New_priority]).


%%%%%%%%%%%%%%%pass_priority/2 26/12/2010
%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%pass_priority/2 (X) 08/03/11

	% A player has lost the game. No player receives priority
	pass_priority(loss(Player, Condition), loss(Player, Condition)).

%%%%%%%%%%%%%%pass_priority/2 (1) 26/12/2010

	% Find the next player in the order of play
	pass_priority(Current_player, New_player):-
		order_of_play(Players),
		next(Current_player, New_player, Players).

%%%%%%%%%%%%%%pass_priority/2 (2) 26/12/2010

	% Reset the players pointer
	pass_priority(Current_player, First_player):-
		order_of_play([First_player | Rest]).
	% Fixed a hack here. Check older versions if
	%  something looks wrong.


%%%%%%%%%%%%%%%state_based_actions/1 08/03/11
%%%%%%%%%%%%%%%%%%%%%%%

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	If a player should lose the game, a loss/2 state is bound to
	  the new priority player, which should cause
	  turn_sequence to terminate.
*/

	state_based_actions(Priority, Player):-
		zero_life(Priority, Player), %!,
		zero_cards(Priority, Player), %!,
		ten_poison(Priority, Player), %!,
		zero_toughness,
		creature_destroyed,
		deathtouch_damage.
	% More state-based actions exist.

% Still need to add clauses to deal with loss conditions below.

%%%%%%%%%%%%%%%zero_life/1 08/03/11
%%%%%%%%%%%%%%%%%%%%%%%

	zero_life(_Priority, loss(Player, zero_life)):-
		life_total(Player, Life),
		Life =< 0.

	zero_life(_,_).


%%%%%%%%%%%%%%%zero_cards/1 08/03/11
%%%%%%%%%%%%%%%%%%%%%%%

	zero_cards(_Priority, loss(Player, milled)):-
		player_state(Player, State),
		member(milled, State).
	% This should ^^ be added by whatever attempted to
	%  draw a card from the Player's empty library.

	zero_cards(_,_).


%%%%%%%%%%%%%%%ten_poison/1 08/03/11
%%%%%%%%%%%%%%%%%%%%%%%

	ten_poison(_Priority, loss(Player, poisoned)):-
		poison_counters(Player, 10).

	% A player hasn't lost the game yet.
	ten_poison(Player,Player).

	% A player has.
	ten_poison(_,_).


%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	If a player should lose, each player-loss check after the check
	  that is found to be true, will fail on its first clause and
	  succeed on its last clause. However, if no player should lost the
	  game, the last check (this one) will succeed on its second clause
	  and return the priority player's name (bound to the one it was
	  passed).
*/

%%%%%%%%%%%%%%%zero_toughness/0 08/03/11
%%%%%%%%%%%%%%%%%%%%%%%

	% Subsequent clauses don't affect players.
	zero_toughness:-
		zone('Battlefield', Objects),
		findall(Controller - Creature,
				(member(Creature, Objects),
				creature(Creature, _, _, _, 0, Controller)), % 0 Toughness
			Creatures),
		bury_creatures(Creatures).

	% uncomment once tested
	%zero_toughness(_Player).

	bury_creatures([]).
	bury_creatures([Controller - Creature | Rest]):-
		move_to_zone(Controller, Creature, 'Battlefield', 'Graveyard'),
		bury_creatures(Rest).


%%%%%%%%%%%%%%%creature_destroyed/0 08/03/11
%%%%%%%%%%%%%%%%%%%%%%%

	creature_destroyed:-
		zone('Battlefield', Objects),
		findall(Controller - Creature,
				(member(Creature, Objects),
				lethal_damage(Creature, _Damage),
				zone(Controller, 'Battlefield', Permanents),
				member(Creature, Permanents)),
			Creatures),
		destroy_creatures(Creatures).

	% uncomment once tested
	%creature_destroyed(_Player).


%%%%%%%%%%%%%%%deathtouch_damage/0 08/03/11
%%%%%%%%%%%%%%%%%%%%%%%

	deathtouch_damage:-
		zone('Battlefield', Objects),
		findall(Controller - Creature,
				(member(Creature, Objects),
				Creature = object(_Name - _Id, State),
				member(deathtouch, State),
				zone(Controller, 'Battlefield', Permanents),
				member(Creature, Permanents)),
			Creatures),
		destroy_creatures(Creatures).

	% uncomment once tested.
	%deathtouch_damage.


%%%%%%%%%%%%%%%destroy_creatures/0 08/03/11
%%%%%%%%%%%%%%%%%%%%%%%

	destroy_creatures([]).
	destroy_creatures([Controller - Creature | Rest]):-
		destroy_permanent(Controller, Creature),
		destroy_creatures(Rest).

	% Move to er, zone?
	destroy_permanent(Controller, Permanent):-
		move_to_zone(Controller, Permanent, 'Battlefield', 'Graveyard').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%player_actions/5 01/02/2011
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* player_actions(+Active_player, +Priority_player, +Step, +Phase, -New_play) */
	% The priority player can now take the actions allowed for this Phase and Step

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 	 Currently, Step and Phase are unused- play/5 must check that the
	  player actions taken are valid for the Priority player
	  to take in the current Step or Phase.
*/


%%%%%%%%%%%%%%player_actions/5 (1) 08/03/11

	% When a player loses the game, no more players receive priority.
	player_actions(_, loss(_Player, _Condition), _, _, []).

%%%%%%%%%%%%%%player_actions/5 (2) 01/02/2011

	% No player actions are taken at the end of a Step
	%  or phase.
	player_actions(_, _, ends(_), _, []):-
		true.

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	I was leaving New_play unbound here and it was getting bound in
	  the first clause of resolve_stack (the debug clause- so it was
	  becoming 100) below. I shouldn't be leaving "don't-know" values
	  in Play, Stack, Cue etc.
*/

%%%%%%%%%%%%%%player_actions/5 (3) 01/02/2011

		% In steps where no player receives priority
		%  players don't take any actions.
%		player_actions(Player, [], State(Step), Phase, []):-
%		Swi, 19/06/11
		player_actions(Player, [], Step_state, Phase, []):-
			Step_state =.. [State, _Step],
			State = begins
			->
			output(player_actions, [])
			; true.

%%%%%%%%%%%%%%player_actions/5 (X) 15/03/2011


	player_actions(Active_player, 'Glee-min', Step, Phase, New_play):-
		glee_min(Active_player, Step, Phase, New_play).

/*	% The computer player has priority
	player_actions(Active_player, 'Glee-min', Step, Phase, New_play):-
		glee_min(Step, Phase, Move),
		play(Active_player, 'Glee-min', Step, Phase, Move, New_play).
		% ^ This needs to inform of the result of actions? Or no?
		%  eg, if an action is illegal, a different one should be
		%  chosen? Or should glee-min check the legality of actions?
*/

%%%%%%%%%%%%%%player_actions/5 (3) 01/02/2011

   	% In a Step or Phase where a player has received priority,
	%  that player can take the actions allowed for the Step or Phase.
	player_actions(Active_player, Priority_player, Step, Phase, New_play):-
		Context = [Active_player, Priority_player, Step, Phase, New_play],
		input(player_actions, Context, Input) ->
		play(Active_player, Priority_player, Step, Phase, Input, New_play);
		player_actions(Active_player, Priority_player, Step, Phase, New_play).


%%%%%%%%%%%%%%play/6 01/02/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* play(+Active_player, +Priority_player, +Step, +Phase, +Play, -New_play) */
	
% 09-10-11 This should be in player_actions.pl

%%%%%%%%%%%%%%play/6 (1) 01/02/2011

	% l: 'Play Land'.
	play(Active_player, Priority_player, Step, Phase, 108, Play):-
		play_land(Active_player, Priority_player, Step, Phase, Play).

%%%%%%%%%%%%%%play/5 (2) 30/12/2010

	% s: 'Cast Spells'
	play(_Active_player, Priority_player, _Step, _Phase, 115, Play):-
%		cast_spell(Priority_player, Play).
		%cast_spell(Priority_player, _Saved_state, Play).
%		output(play, [Priority_player, 'spells']).
%		cast_spell(Priority_player, _Spell, Play).
		cast_spell(_Active_player, Priority_player, _Step, _Phase, Play).

%%%%%%%%%%%%%%play/5 (3) 30/12/2010

	% a: 'Activate Abilities'
	play(Active_player, Priority_player, Step, Phase, 97, Play):-
		activate_ability(Active_player, Priority_player, Step, Phase, Play).
%		output(play, [Priority_player, 'abilities']).

%%%%%%%%%%%%%%play/1 (4) 26/12/2010

	% t: 'Take Special Action' (other than play land)
	play(Active_player, Priority_player, Step, Phase, 116, 116):-
		output(play, [Priority_player, 'special']).

%%%%%%%%%%%%%%play/1 (5) 26/12/2010

	% h: 'Help'
	play(Active_player, Priority_player, Step, Phase, 104, 104):-
		output(play, [Priority_player, 'help']).

%%%%%%%%%%%%%%play/1 (6) 26/12/2010

	% p: pass
	play(Active_player, Priority_player, Step, Phase, 112, 112):-
	output(play, [Priority_player, 'pass']).

%%%%%%%%%%%%%%play/1 (7) 26/12/2010

	% c: Concede (uppercase c)
	play(Active_player, Priority_player, Step, Phase, 67, 67):-
		output(play, [Priority_player, 'concede']).

%%%%%%%%%%%%%%play/1 (8) 26/12/2010

	% d: debug; 99: c (cancel)
	play(_Active_player, Priority_player, _Step, _Phase, 100, 99):-
		break,
		output(play, [Priority_player, 'debug']),
		true.

%%%%%%%%%%%%%%play/1 (X) 26/12/2010

	% i: 'Inspect'
	play(Active_player, Priority_player, Step, Phase, 105, 105):-
		inspect_game(Priority_player).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%stack_resolves/6 30/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* stack_resolves(+Stack, +New_play, +Cue, +New_priority, -New_cue, -New_stack) */
	% Builds the stack and resolves obejcts from it
	% builds the cue and passes it on to step_ends for the step to end.

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	The Cue is the list of players who have passed in succession
	  (without doing anything else in between).
	If it is full, the Stack resolves. If the Stack is empty,
	  the step or phase ends.
	The Stack resolves one object at a time: the top object
	  in it resolves, then that object's controller receives
	  priority again.
*/

	% Attempting to send a message to receives_priority
%	stack_resolves([resolved | Rest], New_play, Cue, New_priority, Cue, New_stack):-
%		stack_resolves(Rest, New_play, Cue, New_priority, Cue, New_stack).
	% Not good. Recursion causes the same step to go twice.


%%%%%%%%%%%%%%stack_resolves/6 (X) 08/03/2011

	% A player has lost the game. No Stack is built
	stack_resolves(_, _, _, loss(_Player, _Condition), [], []).

%%%%%%%%%%%%%%stack_resolves/6 (debug) 01/01/2011
% Debug clause

%	stack_resolves(_Stack, 100, _Cue, _New_priority, 'Full', []):-
%		true.

%%%%%%%%%%%%%%stack_resolves/6 (1) 31/12/2010

	% In a step or phase where no players receive priority
	%  there should be no Stack to build.
	stack_resolves([], [], [], [], [], []):-
		true.

%%%%%%%%%%%%%%stack_resolves/6 (2) 31/12/2010

	% In a step where players receive priority,
	%  we need to manage the cue of players passing or playing
	%  and build a stack of objects to be resolved.
	stack_resolves(Stack, New_play, Cue, New_priority, New_cue, New_stack):-
		cue(Stack, New_play, Cue, New_priority, New_cue),
%		debug_cue(New_cue),
		stack(Stack, New_play, New_cue, New_priority, New_stack).


%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	While debugging, we eventually fall here at the end of
	  a Step because at that point a player has received priority
	  in the current step but New_play is not 100 any more (it's
	  cleared at ends(Step). This should not be a problem unless
	  subsequent clauses are incorrect.
*/

%%%%%%%%%%%%%%%cue/4 31/12/2010ppp
%%%%%%%%%%%%%%%%%%%%%%%
	/* cue(+Stack, +New_play, +Cue, +New_priority, -New_cue) */

%%%%%%%%%%%%%%cue/4 (1.1) 02/02/2011

	% If the current priority player has cancelled
	%  an action, don't change the cue.
	cue(Stack, 99, Cue, New_priority, Cue).
%	cue(Stack, New_play, Cue, New_priority, Cue):-
%		New_play == 99;
%		New_play == 105.
	% ^ Acually, this broke Concede, apparently. Wut?


%%%%%%%%%%%%%%cue/4 (1.2) 02/02/2011

	% If the current priority player has simply inspected
	%  the state of the game, don't change the cue.
	cue(Stack, 105, Cue, New_priority, Cue).

%%%%%%%%%%%%%%cue/4 (1.3) 31/12/2010

	% If the current priority player has taken any action
	%  but pass, inspect or cancel an action, clear the Cue.
	cue(Stack, New_play, Cue, New_priority, []):-
		New_play \== 112.

%%%%%%%%%%%%%%cue/4 (2.1) 31/12/2010
% 2.* clauses: the Priority player has passed
%  and adding him or her to the Cue will fill the Cue.
% If the Cue is not empty, first we need to check that it will not
%  overflow after the current priority player is added to it.
% Of course this is relevant only if the player has passed-
%   else the player is not added to the cue!

	% The Stack is empty and the Priority player
	%  has passed. If adding him or her to the Cue
	%  will fill the Cue, return that the Cue is 'Full'.
	cue(Stack, 112, Cue, New_priority, 'Full'):-
		(Stack = []; Stack = [resolved]), % is an empty stack. Trust me.
		(Cue \== [], Cue \== 'Full'),
		append(Cue, [New_priority], New_cue),
		full_cue(New_cue).
	% This should be intercepted by step_ends/5
	% which should end the step

%%%%%%%%%%%%%%cue/4 (2.2) 31/12/2010

	% The Stack is not empty and the cue is not empty
	%  or 'Full' and the Priority player
	%  has passed. If adding him or her to the Cue
	%  will fill the Cue, clear the Cue.
	cue(Stack, 112, Cue, New_priority, []):-
		Stack \==[], Stack \==[resolved],
		(Cue \== [], Cue \== 'Full'),
		append(Cue, [New_priority], New_cue),
		full_cue(New_cue).
	% This should be intercepted by stack/4,
	%  which should resolve the first object
	%  on the Stack and allow a new player
	%  to receive priority in the next level of turn/7.

%%%%%%%%%%%%%%cue/4 (3.1) 31/12/2010
% 3.* clauses: the Priority player has passed
%  and adding him or her to the cue will not fill the Cue.
% At this point the Stack doesn't matter because
%  it will not begin to resolve until the Cue is full.

	% The Priority player has passed and the Cue is
	%  not empty or 'Full'. If adding him or her to the Cue
	% will not fill the Cue, add that player to the Cue.
	cue(Stack, 112, Cue, New_priority, New_cue):-
		(Cue \== [], Cue \== 'Full'),
		append(Cue, [New_priority], New_cue),
		not full_cue(New_cue).

%%%%%%%%%%%%%%cue/4 (3.2) 31/12/2010

	% The Priority player has passed and the Cue is
	%  empty or 'Full'. Adding that player to the Cue
	%  will not fill it up. Initialise the New_cue
	%  with the Priority player as its first element.
	cue(Stack, 112, Cue, New_priority, [New_priority]):-
		(Cue == []; Cue == 'Full').


%%%%%%%%%%%%%%%full_cue/ 0 31/12/2010
%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%full_cue/0 (1) 30/12/2010

	% Check that the cue is full.
	full_cue(Cue):-
		order_of_play(Players),
		length(Players, Number_of_players),
		call((length(Cue, Players_in_cue), !)),
		Number_of_players == Players_in_cue.


%%%%%%%%%%%%%%%debug_cue/1 31/12/2010
%%%%%%%%%%%%%%%%%%%%%%%

	debug_cue(Cue):-
		tab(25),
		write('Cue : '),
		write(Cue), nl.


%%%%%%%%%%%%%%%stack/5 24/02/2010
%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%stack/5 (1) 22/03/2011

	% A player has conceded the game. The Stack does not resolve.
	stack(_Stack, 67, _New_cue, _New_priority, _STack). %concede!

%%%%%%%%%%%%%%stack/5 (1) 24/02/2011

	% The priority player has taken an action, other than pass
	%  and there are objects on the Stack: it remains unchanged.
	stack(Stack, New_play, _New_cue, _New_priority, Stack):-
		(Stack \= [], Stack \= [resolved]), % empty Stack
		New_play \= 112, % pass
		New_play \= [].  % no actions, ie no priority player

%%%%%%%%%%%%%%stack/5 (2) 24/02/2011

	% An Object has resolved from the Stack, all players have passed
	%  in succession and there are still Objects on the Stack.
	% The topmost object on the Stack resolves.
	stack(Stack, 112, [], _Priority_player, [resolved | Resolved_stack]):-
		Stack = [resolved | Rest], Rest \== [],
		resolve(Rest, Resolved_stack).

%%%%%%%%%%%%%%stack/5 (3) 24/02/2011

	% The stack resolves when both players pass in succession on a
	%  non-empty Stack; ie, New_play = 112, Cue = [] (indeed), Stack \= []
	stack(Stack, 112, [], _Priority_player, [resolved | Resolved_stack]):-
		Stack \== [resolved | _Rest], Stack \== [],
		resolve(Stack, Resolved_stack).

%%%%%%%%%%%%%%stack/5 (4) 24/02/2011

	% When the turn begins, the Stack is empty. We need to build it.
	stack(_Stack, _New_play, _Cue, _Priority_player, New_stack):-
		zone('Stack', New_stack).
	% If the Stack doesn't resolve, "resolve" is cleared from it here.


%%%%%%%%%%%%%%resolve_stack/2 24/02/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* resolve_stack(+Object, -Stack) */
	% Resolves the first object from the Stack and returns the
	%  rest of the Stack.

	resolve([Object | Stack], Stack):-
		zone('Stack', [Object | Stack]),
		zone(Player, 'Stack', [Object |_]),
		generate_effect(Object, Player).
		% output/2 call? Or in generate_effect?


%%%%%%%%%%%%%%%%%%%%%%%%%%%%step_ends/5 01/01/2011
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* step_ends(+Step, +Phase, +New_cue, +New_priority, +New_stack, -New_step) */

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	Values of State(Step) and State(New_step):

	__Step state__	__New_step state__	__bindings__		__step progress__
	begins(Step)	ongoing(New_step)		Step == New_step		beginning of the step
	ongoing(Step)	ongoing(New_step)		Step == New_step		course of the step
	ongoing(Step)	ends(New_step)		Step == New_step		before the end of the step
	ends(Step)		begins(New_step)		step \== New_step		end of the step

	Note there is no begins(Step) followed by ends(New_step)!
	A step begins, then goes on, then ends. All three stages must be followed through.
*/


%%%%%%%%%%%%%%step_ends/5 (X) 08/03/11

	% A player has lost the game. This ends here.
%	step_ends(_State(Step), _, [], loss(_Player, _Condition), [], ends(Step)).
%	Swi, 19/06/11
	step_ends(Step_state, _, [], loss(_Player, _Condition), [], ends(Step)):-
	     Step_state =.. [_State, Step].

%%%%%%%%%%%%%%step_ends/5 (debug) 01/01/2011
% Debug clause

%	step_ends(begins(Step), Phase, 'Full', New_priority, [], ends(Step)):-
%		true.

%%%%%%%%%%%%%%step_ends/5 (1.1) 30/12/2010
% step_ends/5 clauses 1.*: players don't receive priority.

	% A step where players don't receive priority begins here.
	step_ends(begins(Step), Phase, [], [], [], ongoing(Step)):-
		true.

%%%%%%%%%%%%%%step_ends/5 (1.2) 30/12/2010

	% A step where players don't receive priority
	%  continues while there turn-based actions to take
	% to be implemented
%	step_ends(ongoing(Step), Phase, [], [], [], ongoing(Step)):-
%		true.

%%%%%%%%%%%%%%step_ends/5 (1.3) 30/12/2010

	% A step where players don't receive priority
	%  ends when all game actions have completed
	% Last bit to be implemented...
	step_ends(ongoing(Step), Phase, [], [], [], ends(Step)):-
		true.
	% The stack is empty and no players have passed.

%%%%%%%%%%%%%%step_ends/5 (1.4) 30/12/2010

	% After a step where players don't receive priority
	%  ends, a new step begins.
	step_ends(ends(Step), Phase, New_cue, [], New_stack, begins(New_step)):-
		next_step(Step, Phase, New_step),
		output(step_ends, [ends(Step)]).

%%%%%%%%%%%%%%step_ends/5 (2.1) 30/12/2010
% step_ends/5 clauses 2.*: players receive priority.

	% A step where players receive priority begins here
	step_ends(begins(Step), Phase, New_cue, New_priority, New_stack, ongoing(Step)):-
		true.
	% ^ Shouldn't the stack be empty here?

%%%%%%%%%%%%%%step_ends/5 (2.2) 30/12/2010

	% A step where players receive priority
	%  ends when all players pass in succession
	%  on an empty stack.
	step_ends(ongoing(Step), Phase, 'Full', New_priority, [], ends(Step)):-
		true.

%%%%%%%%%%%%%%step_ends/5 (2.3) 30/12/2010

	% After the Step ends, the next one begins.
	step_ends(ends(Step), Phase, New_cue, New_priority, [], begins(New_step)):-
		next_step(Step, Phase, New_step),
		output(step_ends, [ends(Step)]).

%%%%%%%%%%%%%%step_ends/5 (2.4) 30/12/2010

	% Otherwise, the Step goes on.
	step_ends(ongoing(Step), Phase, New_cue, New_priority, New_stack, ongoing(Step)):-
		true.


%%%%%%%%%%%%%%%next_step/3 30/12/2010
%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%next_step/3 (1) 30/12/2010

	% Find the next step in the current Phase
	next_step(Current_step, Phase, Next_step):-
		phase(Phase, Steps_this_phase),
		next(Current_step, Next_step, Steps_this_phase).

%%%%%%%%%%%%%%next_step/3 (2) 01/01/2011

	% If the current Phase has no more steps, find the first
	%  step in the next phase.
	next_step(Current_step, This_phase, First_step):-
		phases(Phases),
		next(This_phase, Next_phase, Phases),
		phase(Next_phase, [ First_step | Rest ]).

%%%%%%%%%%%%%%next_step/3 (3) 01/01/2011

	% The next Phase may have no steps.
	next_step(Current_step, This_phase, []):-
		phases(Phases),
		next(This_phase, Next_phase, Phases),
		phase(Next_phase, []).

%%%%%%%%%%%%%%next_step/3 (4) 30/12/2010

	% Reset the step pointer to Untap
	%  (this happens when the turn ends)
	next_step(Current_step, Phase, 'Untap'):-
		true.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%phase_ends/2 23/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* phase_ends(+Phase, +Step, -New_phase) */

%%%%%%%%%%%%%%phase_ends/4 (1) 30/12/2010

	% A phase ends at the end of its last step
	%  and then we find a new phase
	phase_ends(Phase, ends(Last_step), Next_phase):-
		phase(Phase, Steps),
		reverse(Steps, [Last_step | Rest]),
		next_phase(Phase, Next_phase),
		output(phase_ends, [Phase]).

%%%%%%%%%%%%%%phase_ends/4 (2) 30/12/2010

	% If this phase has no steps,
	%  it ends here (temporarily- it needs to check
	%  for the proper conditions!)
	phase_ends(Phase, ends([]), Next_phase):-
		next_phase(Phase, Next_phase),
		output(phase_ends, [Phase]).

%%%%%%%%%%%%%%phase_ends/4 (3) 30/12/2010

	% If this is not the end of the Phase's last Step,
	%  continue with this phase
	phase_ends(Phase, Step, Phase):-
		true.

%%%%%%%%%%%%%%next_phase/2 (1) 25/12/2010

	% Find the next phase
		next_phase(Current_phase, Next_phase):-
		phases(Phases),
		next(Current_phase, Next_phase, Phases).

%%%%%%%%%%%%%%next_phase/2 (2) 25/12/2010

	% Reset the phase pointer
		next_phase(Current_phase, 'Beginning'):-
		true.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%turn_ends/4 01/01/2011
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* 	turn_ends(+Player, +Phase, +New_step, -New_player) */

%%%%%%%%%%%%%%turn_ends/4 (1) 01/01/2011

	% In the Cleanup step the turn ends, so
	%  find the next player in the order of play
	turn_ends(Current_player, 'Ending', ends('Cleanup'), Next_player):-
		order_of_play(Players),
		next(Current_player, Next_player, Players),
		cleanup_turn(Current_player),
		output(turn_ends, [Current_player]).

%%%%%%%%%%%%%%turn_ends/4 (2) 01/01/2011

	% Reset the players pointer
	turn_ends(Current_player, 'Ending', ends('Cleanup'), Next_player):-
		order_of_play([Next_player | _Rest]),
		cleanup_turn(Current_player),
		output(turn_ends, [Current_player]).

%%%%%%%%%%%%%%turn_ends/4 (3) 30/12/2010

	% Otherwise, the turn doesn't end
	turn_ends(Player, Phase, Step, Player):-
		true.
		% I should be asserting the active_player here
		%  so I don't need to have one in the db.
		% IF! that's a good idea.

%%%%%%%%%%%%%%%cleanup_turn/ 02/02/2011
%%%%%%%%%%%%%%%%%%%%%%%

	cleanup_turn(Current_player):-
%		player(Player),
		retractall(played_land((Current_player), _)),
		asserta(played_land((Current_player), 'no')).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              Notes                %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

