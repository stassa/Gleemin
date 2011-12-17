% First saved: 08/02/11
% Last saved:  23/04/11
%
% 	Status:
%
%	play_land is: 100%
%	cast_spell: 80% [no timing checks, other_choices]
%	activate_ability: 80% [no timing checks, other_choices]
%	^ am I forgetting something?
%
% Doing:
%	Updating to new objects
%		Done for activate_abilities
% Todo
%	add timing checks in choose_spell, activate_ability
%	redefine mana_cost and mana_string in terms of the MGL interpreter.
%	Complete pay_cost (to cover all possible costs) (use DCG?)
%
% NOTES:
/*	Once finished redefining all:
 	bring cast_spell in line with mgl_interpreter.pl
		changing order of clauses
		adding choose_targets
		change determine_abilities
		add make_choices
	When casting spells or activating abilities, the player is prompted
	  to tap a source for mana. This is not strictly correct. The player
	  should be offered a chance to activate a mana ability. There is a
	  problem to adjust this right now, because a call to
	  activate_abilities would allow a player to activate a non-mana
	  ability.
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
%%%%      Player actions facts         %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	:- dynamic played_land/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%played_land/2 29/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* played_land(+Player, +Yes/No) */
	% Remembers whether a player has played a land this turn

	played_land('Player 1', 'no').
	played_land('Glee-min', 'no').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%      Player actions rules         %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%play_land/4 11/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/*  play_land(+Player, +Step, +Phase, -Play) */
	% Checks that it's legal to put a land into play, does so if it is
	%  and returns an appropriate Play character


%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	Some of the tests are redundant, since the option to play land is
	  only presented in legal situations. However, this may catch illegal
	  "[l]and" input. Anyway, it's extra safety.
*/

%%%%%%%%%%%%%%play_land/3 (1) 29/01/11

	% Is this the player's turn?
	play_land(Active_player, Priority_player, _Step, _Phase, 99):-
		(Priority_player = [Player, _Land] -> true ; Priority_player = Player),
		% ^ The local cut in A->B here prevents backtracking in the A;B on
		%  a failed next clause >
		Active_player \= Player,
		output(play_land, ['wrong_turn']).

%%%%%%%%%%%%%%play_land/3 (2) 29/01/11

	% Is this a main phase?
	play_land(_Active_player, _Priority_player, _Step, Phase, 99):-
		(
			not Phase == 'First Main',
			not Phase == 'Second Main'
		),
		output(play_land, ['wrong_phase']).

%%%%%%%%%%%%%%play_land/3 (3) 29/01/11

	% Has the player already played a land this turn?
	play_land(_Active_player, Priority_player, _Step, _Phase, 99):-
		(Priority_player = [Player, _Land] -> true ; Priority_player = Player),
		played_land(Player, 'yes'),
		output(play_land, ['land_limit']).

%%%%%%%%%%%%%%play_land/3 (4) 29/01/11

	% Is the stack empty?
	play_land(_Active_player, _Priority_player, _Step, Phase, 99):-
		not zone('Stack', []),
		output(play_land, ['stack_not_empty']).

%%%%%%%%%%%%%%play_land/3 (5.1) 22/03/11

	% Preparation for move_land input
	play_land(_Active_player, ['Glee-min', Land], _Step, _Phase, Play):-
		append(['cancel'], [Land], Full),
		prompt_switches(Full, Switches),
		play_land(['Glee-min', Land], Switches, Play).

%%%%%%%%%%%%%%play_land/3 (5.2) 29/01/11

	% List the land in the player's hand
	play_land(_Active_player, Priority_player, _Step, _Phase, Play):-
		zone(Priority_player, 'Hand', Cards),
		findall(Card, (member(Card, Cards),
		card([card_name Card, _, type_line Type, _, _, _, _, _]),
			Type = [_, ['Land'], _]
		),
			Land),
		sort(Land, Sorted),
		append(['cancel'], Sorted, Full),
		prompt_switches(Full, Switches),
		play_land(Priority_player, Switches, Play).


%%%%%%%%%%%%%%play_land/2 04/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* play_land(+Player, +Land, -Play) */
	% Puts a land in play, or cancels; returns the Play character

%%%%%%%%%%%%%%play_land/2 (1) 04/02/11

	% The player has no lands in hand.
	% Contents of Land are: [[cancel - c],['[c]ancel']]
	play_land(Player, [[cancel - c],['[c]ancel']], 99):-
		output(play_land, ['no_lands_in_hand']).

%%%%%%%%%%%%%%play_land/2 (2) 22/03/11

	play_land(['Glee-min', Land_card], Switches, 108):-
		Switches = [[Cancel | Land], _Switches],
		member(Land_card - Switch, Land),
		% ^ maps the Land card to the switch representing it
		% The human player does his or her own mapping ;)
		atom_codes(Switch, [Code]),
		move_land('Glee-min',[Cancel | Land], Code).
		% ^ This used to send the mapping for Cancel as undefined
		% Was that the source of the land bug? It did seem to be
		%  trying to play creatures instead of land...

%	play_land(['Glee-min', _Land_card], Switches, 99).
	% ^ This should not be necessary!

%%%%%%%%%%%%%%play_land/2 (3) 04/02/11

	% Move the land card from the player's hand to the player's
	%  side of the battlefield.
	play_land(Player, [Map, Switches], Play):-
		Context = [Player, Map, Switches],
		input(play_land, Context, Input) ->
		(
			Input = 99, Play = 99;	% Cancel
			move_land(Player, Map, Input),
			Play = 108
		);
		play_land(Player, [Map, Switches], Play).
		% ^ Go back if the input was wrong.


%%%%%%%%%%%%%%move_land/2 04/02/11
%%%%%%%%%%%%%%%%%%%%%%%

	% Choose the right land card to move
	move_land(Player, Map, Input):-
		atom_codes(Switch, [Input]),
		member(Land_card - Switch , Map),
		move_to_zone(Player, Land_card, 'Hand', 'Battlefield'),
		retractall(played_land(Player, _)),
		asserta(played_land(Player, 'yes')),
		output(play_land, ['played_land', Player, Land_card]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%cast_spell/5 20/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* cast_spell(+Active, +Priority, +Step, +Phase, -Play) */
	% Cast a spell from the Player's Hand or another Zone
	% Play is bound to 99 (cancel or fail), 115 (spell cast) or
	%  97 (ability activated).
	% This needs to check for timing restrictions (Step/Phase)!

	cast_spell(_Active, Priority, _Step, _Phase, Play):-
%		choose_spell(Active, Priority, Step, Phase, 'Hand', Spell),
		choose_spell(Priority, 'Hand', Spell, _Text_box),
		% ^ This doesn't really need to return Abilities...
		% (currently it does, in its last arg)
		% Now should also check timing restrictions.
		(Priority = [Player | _rest] ; Priority = Player),
		save_player(Player, Saved_state),
		cast_spell(Priority, Saved_state, Spell, Play),
		casting_outcome(Player, Saved_state, Spell, Play ).


%%%%%%%%%%%%%%choose_spell/4 06/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* choose_spell(+Controller, ?Zone -Spell, -Rules_text) */
	% Determines the Spell to be cast, and returns its Rules text
	%  and possibly the Zone it is in

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	If the Zone is not instantiated, the spell is in a Zone other than
	  the currently topmost zone in the database, the
	  'Input == 111, fail, !' clause in choose_spell/3 will cause
	  choose_spell/3 to backtrack into a new zone/3 and find a new
	  set of cards. That is a feature- the player may indeed be looking
	  for a spell  in that Zone.
	However, most spells will only be castable from the Hand
	  so I should be checking that a spell is in the right zone
	  to be cast from.
	I'll implement this when I get to a card that requires it. Currently
	  it just fails (when called from cast_spell).
*/


%%%%%%%%%%%%%%choose_spell (1) 16/03/11

	% Gleemin has already chosen a spell to play
	choose_spell(['Glee-min', Spell | _Rest], _Zone, Spell, _Abilities).

%%%%%%%%%%%%%%choose_spell (2) 16/03/11

	% Ask the Player which Spell to play
	choose_spell(Player, Zone, Spell, Rules):-
		zone(Player, Zone, Cards),
		findall(Spell, (member(Spell, Cards),
		card([card_name Spell, _, type_line Type, _, _, _, _, _]),
			Type \= [_, ['Land'], _]
		), Hand),
		sort(Hand, Sorted),
		(
			Sorted \= [],
			append(['other', 'cancel'], Sorted, Full),
			prompt_switches(Full, Switches),
			%^ Switches is really: [Map, Switches]!
			choose_spell([Switches, Player], Spell, Rules);
			choose_spell([no_spells, Player, Zone], Spell, Rules)
			% ^ No spells in hand...
		).
		% append(['other', 'cancel'], Hand, Full),
		% ^ When I sort and then append, I don't see my whole hand, obviously.
		% I can add a number at the end of each card name as in: [C]ard(3)
		%  to inform the player of how many copies of the card are in Hand.
		% But not now.


%%%%%%%%%%%%%%choose_spell/3 06/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* choose_spell(Context, Input, Abilities) */
	% Handles input and output for parent, choose_spell/4

	% There are no spells in the player's hand.
	choose_spell([no_spells, Player, Zone], [], []):-
		output(cast_spell, [no_spells, Player, Zone]).

	% Ask the Player which Spell to play
	choose_spell([[Map, Switches], Player], Spell, Rules):-
		Context = [Map, Switches], %also Player? For "P1 plays Sp1"?
		% Switches = [Cancel | [Other | Rest] ], %check rigorously
		input('cast_spell', Context, Input) ->
		(
			Input == 99, Spell = [], Rules = []; %true; % cancel
			Input == 111, fail, !; % other
			atom_codes(Switch, [Input]),
			member(Spell - Switch , Map),
			card([card_name Spell, _, _, text_box Rules, _, _, _, _])
		);
		choose_spell([[Map, Switches], Player], Spell, Rules).


%%%%%%%%%%%%%%cast_spell/4 20/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* cast_spell(+Player, +Saved_state, +Spell, -Play) */
	% Puts the card on the stack and continues casting the spell

%%%%%%%%%%%%%%cast_spell/4 (1) 20/03/11

	cast_spell(_Player, _Saved, [], 99).
	% ^ no spell chosen ie, casting cancelled.

%%%%%%%%%%%%%%cast_spell/4 (1) 20/03/11


	cast_spell(Priority, _Saved, Spell, Play):-
		(Priority = [Player, Spell | _rest] ; Priority = Player),
		move_to_zone(Player, Spell, 'Hand', 'Stack'),
		casting_choices(Priority, Play),
		pay_spell_costs(Priority, Spell, Play).


%%%%%%%%%%%%%%casting_choices/2 20/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* casting_choices(+Player) */
	% Lets the player make choices while
	%  casting a spell.

%%%%%%%%%%%%%%casting_choices/1 (1) 20/03/11

	casting_choices(Priority, Play):-
		zone('Stack', [Spell_object | _rest]),
		Spell_object = object(Name - _Id, _Rest),
		determine_abilities(Name, Abilities),
		% ^ not in the current cast_spell def
		% Also, btw, I should add a clause to determine_abilities/2
		%  to find abilities of MGL _Objects_ (not just by name)
		%other_choices(Player, Spell_object, Abilities, Choices),
		(Priority = [Player | _REst] ; Priority = Player),
		% ^ temp fix, until I update choose_targets
		choose_targets(Player, Spell_object, Abilities, Play).
		% Undefined if no targets chosen or choosing canc^^^elled

%%%%%%%%%%%%%%casting_choices/1 (1) 20/03/11

	% casting_choices failed or cancelled:
%	casting_choices(_Player, 99).


%%%%%%%%%%%%%%pay_spell_costs/3 20/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* pay_spell_costs(+Player, +Spell, -Play) */
	%  Lets the player pay the cost of a spell

%%%%%%%%%%%%%%pay_spell_costs/3 (1) 04/04/11

	pay_spell_costs(_Player, _Spell, Play):-
		\+ type(Play,0), Play = 99.

%%%%%%%%%%%%%%pay_spell_costs/3 (2) 20/03/11

	pay_spell_costs(Player, Spell, Play):-
		determine_cost(Spell, _Abilities, Cost),
		pay_cost(Cost, Player, Spell, Play).

%%%%%%%%%%%%%%pay_spell_costs/3 (2) 20/03/11

	% pay_spell_costs failed or was cancelled.
	pay_spell_costs(_Player, _Spell, 99).


%%%%%%%%%%%%%%other_choices/3 20/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* other_choices(+Player, +Spell, +Abilities, -Choices) */
	% Lets the player make choices, besides targets for a spell
	%  Coming soon.

	other_choices(_Player, _Spell_object, _Abilities, []).


%%%%%%%%%%%%%%casting_outcome/4 20/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* casting_outcome(+Player, +Saved_state, +Spell, +Play) */
	% Deals with output for cast_spell. Also restores a player
	%  to a previous state if casting was cancelled or failed.

%%%%%%%%%%%%%%casting_outcome/4 (1) 20/03/11

	% casting_outcome(Player, Saved_state, Spell, Play )
	casting_outcome(Player, _Saved, Spell, 115 ):-
		output(cast_spell, ['spell_cast', Player, Spell, 'Hand']).

%%%%%%%%%%%%%%casting_outcome/4 (1) 20/03/11

	casting_outcome(Player, Saved, [], 99 ):-
		restore_player(Player, Saved),
%		write(`Spell casting canceled`), nl.
	output(cast_spell, [spell_canceled, Player]).

%%%%%%%%%%%%%%casting_outcome/4 (1) 20/03/11

	casting_outcome(Player, Saved, Spell, 99 ):-
		restore_player(Player, Saved),
		output(cast_spell, ['spell_fails', Player, Spell, 'Hand']).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%activate_ability/5 31/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* activate_ability(+Active_player, +Priority_player, +Step, +Phase, -Action) */
	% Allows a player to activate an activated ability of a permanent,
	%  or of an object in a different zone (not implemented yet)

	activate_ability(Active, Priority, Step, Phase, Play):-
		which_ability(Active, Priority, Step, Phase, Source, Ability),
		(Priority = [Player | _rest] ; Priority = Player),
		% ^ If Glee-min is the priority player, some information about
		%  the ability and how to activate it will be included here.
		save_player(Player, Saved_state),
		ability_activation(Priority, Source, Ability, Play),
		activation_outcome(Player, Saved_state, Source, Ability, Play ).


%%%%%%%%%%%%%%which_ability/5 31/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* which_ability(+Active_player, +Priority_player, +Step, +Phase, -Ability) */
	% Lets the player choose an ability to activate

	which_ability(_Active, Priority, _Step, _Phase, Source, Ability):-
		choose_ability_source(Priority, Source),
		choose_ability(Priority, Source, Ability).


%%%%%%%%%%%%%%choose_ability_source/2  31/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* choose_ability_source(+Player, -Source) */
	% Lets the player choose an object that has an activated ability

	choose_ability_source(Player, Ability_source):-
		activated_abilities_sources(Player, Map, Switches, Identified),
		ability_source(Player, Map, Switches, Identified, Ability_source).


%%%%%%%%%%%%%%choose_ability_source/2 31/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* choose_ability_source(+Player, -Source) */
	% Lets the player choose an object that has an activated ability

	activated_abilities_sources(Player, Map, Switches, Identified):-
		zone(Player, 'Battlefield', Objects),
		findall([Name, Id],
			( member(Object, Objects),
			object_handle(Object, Handle),
			object_template(Handle, text_box, Abilities),
			( one(( member(Ability, Abilities),
			% ^ Prolog will report an Object once for each activated
			%  ability it has. We only need its name one time.
			phrase(activated_ability, Ability)) )),
			Handle = Name - Id ),
			Sources),
		findall(Name, member([Name, Id], Sources), Names),
		append([cancel], Names, Full),
		prompt_switches(Full, Switched),
		Switched = [Map, Switches],
		identify_object(Map, Sources, 1, [], Identified).


%%%%%%%%%%%%%%ability_source/5 31/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* ability_source(+Player, +Map, +Switches, +Identified, -Source) */
	% Handles player input for choose_ability_source

	ability_source(_Player, Map, Switches, Identified, Source):-
		Context = [Map, Switches, Identified],
		input(ability_source, Context, Input) ->
		atom_codes(Char, [Input]),
		ability_source(Char, Map, Identified, Source);
		ability_source(Input, Map, Switches, Identified, Source).


%%%%%%%%%%%%%%ability_source/5 31/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* ability_source(+Input, +Map, +Identified, -Source) */
	% Identifies the source of an ability chosen by a player

%%%%%%%%%%%%%%ability_source/5 (1) 31/03/11

	% The player cancels activation
	ability_source(Switch, Map, _Identified, []):-
		member('cancel' - Switch, Map).

%%%%%%%%%%%%%%ability_source/5 (2) 31/03/11

	ability_source(Switch, _Map, Identified, Name - Id):-
		member([Name - Switch, Id], Identified).


%%%%%%%%%%%%%%choose_ability/3 31/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* choose_ability(+Player, +Source, -Ability) */
	% Lets the player choose one of the activated abilities of a Source

%%%%%%%%%%%%%%choose_ability/3 (1) 31/03/11

	% The player has cancelled activation
	choose_ability(_Player, [], []).

%%%%%%%%%%%%%%choose_ability/3 (2) 31/03/11

	choose_ability(Player, Source, Ability):-
		object_template(Source, text_box, Abilities),
		map_ability(Abilities, Map, Switches),
		choose_ability(Player, Abilities, Map, Switches, Ability).


%%%%%%%%%%%%%%map_ability/3 31/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* map_ability(+Abilities, +Map, -Switches) */
	% Maps abilities for player input

	map_ability(Abilities, Map, Switches):-
		findall(Ability, member(Ability, Abilities), ABilities),
		ABilities \= [] -> % What if there are no abilties to activate?
		findall(Activated,( member(Activated, ABilities),
					one(phrase(activated_ability, Activated))),
			Activated_Abilities ),
		atom_word_list(Activated_Abilities, [], Atomic),
		append([cancel], Atomic, Full),
		prompt_switches(Full, [Map, Switches]).


%%%%%%%%%%%%%%choose_ability/5 31/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* choose_ability(Player, Abilities, Map, Switches, Ability) */
	% Handles user input for choose_ability/3

	choose_ability(Player, Abilities, Map, Switches, Ability):-
		Context = [Map, Switches],
		input(choose_ability, Context, Input) ->
		atom_codes(Char, [Input]),
		ability(Char, Map, Abilities, Ability);
		choose_ability(Player, Abilities, Map, Switches, Ability).


%%%%%%%%%%%%%%ability/4 31/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* ability(Switch, Map, Abilities, Ability) */
	% Identifieds the Ability chosen by the player and matches it
	%  back to its word-list form for the MGL interpeter

%%%%%%%%%%%%%%ability/4 (1) 31/03/11

	ability(Switch, Map, _Abilities, []):-
		member('cancel' - Switch, Map).

%%%%%%%%%%%%%%ability/4 (1) 31/03/11

	ability(Switch, [_Cancel | Map], Abilities, Ability):-
		member(_Atomic - Switch, Map, Position),
		member(Ability, Abilities, Position).


%%%%%%%%%%%%%%ability_activation/4 01/04/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* ability_activation(+Player, +Source, +Ability, -Play) */
	% Creates the ability as an object on the stack and continues
	%  with its activation

%%%%%%%%%%%%%%ability_activation/4 (0) 01/04/11

	% Activation cancelled
	ability_activation(_Priority, [], [], 99).

%%%%%%%%%%%%%%ability_activation/4 (0) 01/04/11

	% No source chosen
	ability_activation(_Priority, [], _Ability, 99).

%%%%%%%%%%%%%%ability_activation/4 (0) 01/04/11

	% No ability chosen
	ability_activation(_Priority, _Source, [], 99).

%%%%%%%%%%%%%%ability_activation/4 (1) 01/04/11

	ability_activation(Priority, Source, Ability, Play):-
		mana_ability(_Source_name, Ability, Mana),
		add_to_pool(Priority, Mana, _Pool),
		pay_activation_costs(Priority, Ability, Source, Play).

%%%%%%%%%%%%%%ability_activation/4 (2) 01/04/11

	ability_activation(Priority, Source, Ability, Play):-
		(Priority = [Player, Ability | _rest] ; Priority = Player),
		ability_effect(Ability, Effect),
		create_in_zone(Player, Effect, 'Stack'),
		activation_choices(Priority, Effect, Play),
		pay_activation_costs(Priority, Ability, Source, Play).
	% OK, ugly problem here. This needs to determine the effect
	%  and separate it from the rest of the ability but "this does
	%  something" effects don't work properly ("this" needs fixing).


%%%%%%%%%%%%%%ability_effect/2 06/04/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* ability_effect(+Ability, -Effect) */
	% Separates an ability's activation cost from its effect.
	% Will probably fail on multi-effect abilities.

	ability_effect(Ability, Effect):-
		sublist(Cost, Ability),
		phrase(cost, Cost),
		append(Cost, [:], Full),
		append(Full, Effect, Ability),
		phrase(effect, Effect).

       /* My GOD! but this is unnecessary!
       Use:
         phrase(cost, Ability, [:|Effect]),
	 append(Cost, [:|Effect], Ability)
       as for determine_cost/2 */


%%%%%%%%%%%%%%activation_choices/2 01/04/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* activation_choices(+Player, +Ability) */
	% Allows the player to choose targets and make any
	%  other choices required for an activated ability

	activation_choices(Priority, Effect, Play):-
		zone('Stack', [Ability_object | _rest]),
		%other_choices(Player, Ability_object, Choices),
		(Priority = [Player | _REst] ; Priority = Player),
		% ^ temp fix, until I update choose_targets with glee_min clauses
		choose_targets(Player, Ability_object, [Effect], Play).
		% ^ Effect needs to be in a list: choose_targets calls member/2 on it
		%  because objects may have more than one effect or ability


%%%%%%%%%%%%%%pay_activation_costs/4 01/04/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* pay_activation_costs(+Player, +Ability, +Source, -Play) */
	% Pays the activation cost(s) for an ability

%%%%%%%%%%%%%%pay_activation_costs/4 (1) 01/04/11

	pay_activation_costs(_Player, _Ability, _Source, Play):-
		\+ type(Play,0), Play = 99.

%%%%%%%%%%%%%%pay_activation_costs/4 (2) 01/04/11

	pay_activation_costs(Player, Ability, Source, Play):-
		determine_cost([], Ability, Cost),
		object_handle(Object, Source),
		pay_cost(Cost, Player, Object, Play).

%%%%%%%%%%%%%%pay_activation_costs/4 (3) 01/04/11

	% The player has cancelled the activation
	pay_activation_costs(_Player, _Ability, _Source, 99).


%%%%%%%%%%%%%%activation_outcome/4 01/04/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* activation_outcome(+Player, +Saved_state, +Ability, -Play) */
	% Reports the outcome of an ability's activation.
	% Also restores a player to a previous state if
	%  activation was cancelled or failed.

%%%%%%%%%%%%%%activation_outcome/4 (1) 01/04/11

	% Remember: if pay_cost has failed, pay_activation_cost will
	%  return with Play = 99 and not bind to this clause
	activation_outcome(Player, _Saved_state, Source, Ability, 97):-
		output(activate_ability, [ability_activated, Player, Source, Ability, _Zone]).
		% ^ Need to report the zone an ability was activated in

%%%%%%%%%%%%%%activation_outcome/4 (2) 01/04/11

	activation_outcome(Player, Saved_state, Source, [], 99):-
		restore_player(Player, Saved_state),
		output(activate_ability, [activation_cancelled, Player, Source, _Zone]).

%%%%%%%%%%%%%%activation_outcome/4 (3) 01/04/11

	activation_outcome(Player, Saved_state, Source, Ability, 99):-
		restore_player(Player, Saved_state),
		output(activate_ability, [activation_fails, Player, Source, Ability, _Zone]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%pay_cost/3 06/02/2011
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* pay_cost(+Cost, +Controller, +Object, -Result) */
	% Pay the Cost for an activated ability of a Permanent or to cast
	%  a spell. If paying a cost fails, all payments are undone
	% Result is either 115/97 (spell cast/ ability activated),
	%  or 99 (cancelled/ failed) (yeah, I don't like the ambiguity either)

%%%%%%%%%%%%%%pay_cost/3 (0) 20/02/2011

	pay_cost([], Player, Permanent, Play):- !.

%%%%%%%%%%%%%%pay_cost/3 (1) 20/02/2011

	% The cost is to tap the permanent
	pay_cost(['tap' | Rest], Player, Permanent, Play):-
		tap_untap(Permanent, Player, 'tap'),
		pay_cost(Rest, Player, Permanent, Play).

	pay_cost(['tap' | _Rest], _Player, Permanent, 99):-
		output(pay_cost, [Permanent, 'tap_failed']).

%%%%%%%%%%%%%%pay_cost/3 (2) 20/02/2011

	% The cost is to untap the permanent
	pay_cost(['untap' | Rest], Player, Permanent, Play):-
		tap_untap(Permanent, Player, 'untap'),
		pay_cost(Rest, Player, Permanent, Play).

	pay_cost(['untap' | _Rest], _Player, Permanent, 99):-
		output(pay_cost, [Permanent, 'untap_failed']).

%%%%%%%%%%%%%%pay_cost/3 (2) 20/02/2011

	% The cost is a mana cost
	pay_cost([Cost | Rest], Player, Object, Play):-
		mana_cost(Cost),
		pay_mana_cost(Cost, Rest, Player, Object, Play),
		pay_cost(Rest, Player, Object, Play).

	pay_cost([_Cost | _Rest], _Player, _Object, 99).


%%%%%%%%%%%%%%pay_mana_cost/5 20/03/2011
%%%%%%%%%%%%%%%%%%%%%%%

	pay_mana_cost(Cost, Rest, Player, Object, Play):-
		mana_sources(Player, Switches, Identified),
		tap_for_mana(Player, Switches, Identified, Play),
		spend_mana(Player, Cost, Play),
		pay_cost(Rest, Player, Object, Play).

	pay_mana_cost(_Cost, _Rest, _Player, Object, 99):-
		output(pay_cost, [Object, 'mana_failed']).


%%%%%%%%%%%%%%mana_cost/3 07/02/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* mana_cost(+Cost) */
	% Checks that a cost is a mana cost.

	% A mana cost is a (non-negative) number,
	%   optionally followed by a string of mana symbols.
	mana_cost(Cost):-
		string_to_list(Cost, [First | Rest]),
		((number(First), First >= 0,
		mana_string(Rest));
		mana_string([First | Rest])).


%%%%%%%%%%%%%%mana_string/1 07/02/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* mana_string(+String) */
	% Checks whether an atom is a string of mana symbols.

	% A mana string is a string of mana symbols.
	mana_string(Rest) :-
		\+ (
			member(Symbol, Rest),
			Symbol \== w,
			Symbol \== u,
			Symbol \== b,
			Symbol \== r,
			Symbol \== g
		).


%%%%%%%%%%%%%%mana_sources/2 22/02/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* mana_sources(+Player, -Switches, -Identified) */
	% Returns the mana sources on the Player's side of the battlefield
	%  Identified is a list of [Permanent - Switch, Id]'s


%%%%%%%%%%%%%%mana_sources/3 (1) 22/03/2011

	mana_sources( [ 'Glee-min' | _rest ], _Switches, _Identified).

%%%%%%%%%%%%%%mana_sources/3 (2) 22/03/2011

	mana_sources(Player, Switches, Identified):-
		findall([Permanent, Id], (zone(Player, 'Battlefield', Cards),
			member(object(Permanent - Id, State), Cards),
			mana_ability(Permanent, _Ability, _Mana),
			\+ member(tapped, State)),
			Sources), %write(Sources), nl,
		findall(Permanent, member([Permanent , Id], Sources), Permanents),
		append(['cancel', 'skip'], Permanents, Full),
		prompt_switches(Full, Switches), %write(Switched), nl,
		Switches = [Map, _SwitcheS],
		identify_object(Map, Sources, 2, [], Identified).

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	In the call to identify_object/4
	Maybe I can remove non-cancel/skip key-value pairs from Map
	  append Identified [key - value, Id] lists in their place and
	  send it all on... but it may cause problems further down?
	 Currently I'm working with two lists, Map and Identified, in
	  subsequent calls and it's a bit confusing.
*/


%%%%%%%%%%%%%%identify_object/4 22/02/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* identify_object(+Map, +Copies, +Padding, [], -Identified) */
	% Matches card copies and their Ids to a map of Card - Switch 's.
	% Returns a list of [Name - Switch, Id]'s used to identify card
	%  copies for user input/output.
	% Padding is the number of additional options added to the Map of
	%  Card - Switch, eg. [c]ancel, etc.

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	There doesn't seem to be a problem with non-land cards on the battlefield.
	  Those don't get reported by mana_sources anyway.
*/

%%%%%%%%%%%%%%identify_object/4 (2) 04/04/2011

	% Some predicates send a list containing non-objects, namely
	%  players and zones. Those should be weeded out for now
	identify_object(Map, Copies, Padding, Temp, Identified):-
		member(Object, Copies, Copies_position),
		(player(Object) ; zone(Object, _Objects)),
		Map_position is Copies_position + Padding,
		member(Object - Switch, Map, Map_position),
		append(Temp, [[Object - Switch]], New_temp),
		remove(Object - Switch, Map, New_map),
		remove(Object, Copies, New_copies),
		identify_object(New_map, New_copies, Padding, New_temp, Identified).

%%%%%%%%%%%%%%identify_object/4 (0) 22/02/2011

	identify_object(Map, [], Padding, Identified, Identified).

%%%%%%%%%%%%%%identify_object/4 (1) 22/02/2011

	identify_object(Map, Copies, Padding, Temp, Identified):-
		member([Copy, Id], Copies, Copies_position),
		Map_position is Copies_position + Padding,
		member(Copy - Switch, Map, Map_position),
		append(Temp, [[Copy - Switch, Id]], New_temp),
		remove(Copy - Switch, Map, New_map),
		remove([Copy, Id], Copies, New_copies),
		identify_object(New_map, New_copies, Padding, New_temp, Identified).


%%%%%%%%%%%%%%tap_for_mana/4 21/02/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* tap_for_mana(+Player, +Switches, +Identified, -Play) */
	% Handles player input for tap_for_mana

%%%%%%%%%%%%%%tap_for_mana/4 (1) 21/02/2011

	tap_for_mana(Player,  _Switches, _Identified, Play):-
		Player = ['Glee-min',_,_,_,Switches, Identified, Sources,_],
		Switches = [Map, _switches], % _switches only needed for human player input
		tap_for_mana('Glee-min', Map, Identified, Sources, Play).

%%%%%%%%%%%%%%tap_for_mana/4 (2) 21/02/2011

	tap_for_mana(Player, [Map, Switches], Identified, Play):-
		Context = [Player, Map, Switches, Identified],
		string_input(tap_for_mana, Context, Input) ->
		atom_to_list(Input, List),
		tap_for_mana(Player, Map, Identified, List, Play);
		tap_for_mana(Player, [Map, Switches], Identified, Play).
	% This fails for Permanents that call for a mana payment or an other
	%  additional cost for producing mana (not just tapping). The problem
	%  is in output/2 for this (still ?). Check it out.


%%%%%%%%%%%%%%tap_for_mana/4 20/02/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* tap_for_mana(+Player, +Map, +Identified, -Input) */
	% Fails on [c]ancel (because there is no "[c]ancel" on the battlefield.
	% [s]kip succeeds and lets you draw mana from your pool.

%%%%%%%%%%%%%%tap_for_mana/3 (0) 21/02/2011

	% All chosen permanents have been tapped
	tap_for_mana(Player, Map, Identified, [], _Play).
	% ^ leaving Play free for spend_mana to bind

%%%%%%%%%%%%%%tap_for_mana/3 (1) 21/02/2011

	% Input = s. Skip tapping any permanents for mana and go
	%  straight to drawing  mana from your pool.
	tap_for_mana(_Player, Map, _Identified, Input, _Play):-
	% ^ leaving Play free for spend_mana to bind
		Input = [Switch], % remove brackets...
		member('skip' - Switch, Map).
	%^ This will allow to enter a string containing 'skip'; a feature.
	% But test it.

%%%%%%%%%%%%%%tap_for_mana/3 (X) 20/03/2011

	% Input = c. Don't tap any permanents for mana and
	%  cancel casting the spell
	tap_for_mana(_Player, Map, _Identified, Input, 99):-
		Input = [Switch], % remove brackets...
		member('cancel' - Switch, Map).
	% Normally, Input will be [c], in this case, since cancel and skip
	%  will be first in line to receive switches, so I could just watch
	%  for [c] in the head; but, just in case, I'm making sure.

%%%%%%%%%%%%%%tap_for_mana/3 (2) 21/02/2011

	% The permanent the Player is attempting to tap is already tapped
	tap_for_mana(Player, Map, Identified, [Switch | Rest], _):-
		member(Permanent - Switch , Map),
		% ^ From the Switch we find the name of the Permanent
		member([Permanent - Switch, Id], Identified),
		% ^ From the Name and Switch we find the object's Id
		zone(Player, 'Battlefield', Cards), %redundant!
		member(object(Permanent - Id, State), Cards),
		member('tapped', State), % the Permanent is already tapped
		remove(Permanent - Switch, Map, New_map),
		remove([Permanent - Switch, Id], Identified, New_identified),
		tap_for_mana(Player, New_map, New_identified, Rest, _).
	%  This should not really happen (tapped permanents are not
	%  reported

%%%%%%%%%%%%%%tap_for_mana/3 (3) 21/02/2011

	% The Permanent is untapped. Draw the mana its mana ability can
	%  produce and tap it. Well, the other way around.
	tap_for_mana(Player, Map, Identified, [Switch | Rest], _):-
		member(Permanent - Switch , Map),
		member([Permanent - Switch, Id], Identified),
		zone(Player, 'Battlefield', Cards),
		member(object(Permanent - Id, State), Cards),
		(member('untapped', State) ; State = [] ),
		mana_ability(Permanent, _Ability, Mana),
		% ^ I'll have to choose abilities for permanents with multiple
		%  mana abilities
		tap_untap(object(Permanent - Id, State), Player, 'tap'),
		add_to_pool(Player, Mana, _New_pool),
		remove(Permanent - Switch, Map, New_map),
		remove([Permanent - Switch, Id], Identified, New_identified),
		tap_for_mana(Player, New_map, New_identified, Rest, _).

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	Currently, the player won't get to see permanents that
	  are already tapped, so the second clause here is
	  probably redundant.
*/


%%%%%%%%%%%%%%spend_mana/3 20/02/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* spend_mana(+Player, +Cost, -Play) */
	% Lets the Player spend mana to pay a Cost; verifies that the correct
	%  cost has been payed (well, that actually happens in output/2)

%%%%%%%%%%%%%%spend_mana/3 (1) 20/03/2011

	% Glee-min's mana pool should now have the correct
	%  amount of mana in it, or else something has gone wrong.
	spend_mana(['Glee-min',_,_,_,_,_,_,Mana], Cost, _Play):-
		Mana = Cost,  % should be!
		take_from_pool('Glee-min', Mana, _New_pool).

%%%%%%%%%%%%%%spend_mana/4 (3) 20/03/2011

	% tap_for_mana failed, cancelled or skipped
	spend_mana(_Player, _Cost, Play):-
		\+ type(Play, 0),
		Play = 99.

%%%%%%%%%%%%%%spend_mana/4 (4) 20/03/2011

	% Empty mana pool
%	spend_mana(Player, _Cost, 99):-
%		mana_pool(Player, [c - 0,w - 0,u - 0,b - 0,r - 0,g - 0]).
		% ^ Yep, OK, done that. But leave this in just in case.

%%%%%%%%%%%%%%spend_mana/4 (5) 20/03/2011

	spend_mana(Player, Cost, Play):-
		mana_pool(Player, Mana_pool),
		Context = [Mana_pool, Cost],
		(string_input(spend_mana, Context, Mana) -> %, !,
		match_cost(Cost, Mana),
		take_from_pool(Player, Mana, New_pool),
		output(draw_mana, [drawn_mana, Player, Mana, New_pool]);
		spend_mana(Player, Cost, Play)).

%%%%%%%%%%%%%%spend_mana/4 (6) 20/03/2011

	spend_mana(Player, _Cost, 99):-
	mana_pool(Player, Pool),
	output(draw_mana, [failed_to_draw_mana, Player, Pool]).


%%%%%%%%%%%%%%match_cost/2 22/02/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* match_cost(+Cost, +Payment) */
	% Checks whether Cost can be fullfilled by Payment. Both are
	%  Mana strings.

%%%%%%%%%%%%%%match_cost/2 (0) 20/03/2011

	% The player has canceled payment of the cost
	%  (in spend_mana/4)
	match_cost(_Cost, c):- !.
	% ^ Green cut. Definitely (stops backtracking here again, from
	%  take_from_pool (which fails if it takes c for input.

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	Note that this clause is not full-error proof. If the option to
	  cancel a mana payment is assigned a different switch than "c",
	  then this clause will not fire. In particular, if "cancel" ends up
	  being assinged a switch that is also a mana symbol, then
	  take_from_pool will succeed and there will be a lot of gnashing of
	  teef.
	Again though, because cancel and other default options take first pick
	  in switches (due to being first in the list) all should be OK.
	Ah. Famous last words! 					^	^   ^  ^
*/

%%%%%%%%%%%%%%match_cost/2 (0) 22/02/2011

	% The strings are identical- no more testing needed.
	match_cost(Cost, Cost).

%%%%%%%%%%%%%%match_cost/2 (1) 22/02/2011

	% Attempts to match Cost by Payment.
	match_cost(Cost, Payment):-
		string_to_list(Payment, Payment_list),
		string_to_list(Cost, Cost_list),
		Pool = [c - 0,w - 0,u - 0,b - 0,r - 0,g - 0],
		add_mana(Payment_list, Pool, New_pool),
		draw_mana(New_pool, Cost_list, _Empty_pool).
		% take_from_pool(Player, Mana, _New_pool).
		% ^ If we're here, it should be OK to take mana from the pool.
	% Also, for the benefit of castable_spells (in gleemin.pl)
	%  I may need to return the mana remaining inr the virtual pool.

%%%%%%%%%%%%%%match_cost/2 (2) 22/02/2011

	% Payment cannot satisfy Cost.
%	match_cost(_, _):-
%		write(`Wrong payment`), nl, fail, !.


%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	This is needed here to check that the amount of mana chosen from the
	  player to pay for a cost, can satisfy that cost. match_cost will not
	  draw mana from a player's pool and it will not fail if the payment
	  entered is correct but the amount of mana is not present in the
	  player's pool. Both of the above will happen in take_from_pool/3
	  (which is called right after match_cost).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%inspect_game/1 28/01/2011
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* inspect_game(+Priority_player) */
	% Output information about the game state.

	inspect_game(Priority_player):-
		Context = [Priority_player],
		input(inspect_game, Context, Input) ->
		inspect_output(Priority_player, Input);
		inspect_game(Priority_player).


%%%%%%%%%%%%%%inspect_output/2 28/01/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* inspect_output(+Priority_player, +Zone)*/
	% Outputs information about cards  in a Zone
	%  controlled by the Priority_player.

%%%%%%%%%%%%%%inspect_output/2 (1) 28/01/2011

	% l: library
	inspect_output(Priority_player, 108):-
		output(inspect_output, [Priority_player, 'Library']),
	   	inspect_game(Priority_player).
	% This won't reveal a player's library.
	%  Don't use for searching.

%%%%%%%%%%%%%%inspect_output/2 (2) 28/01/2011

	% h: hand
	inspect_output(Priority_player, 104):-
		read_card('Hand', Priority_player),
	   	inspect_game(Priority_player).

%%%%%%%%%%%%%%inspect_output/2 (3) 28/01/2011

	% s: stack
	inspect_output(Priority_player, 115):-
%		output(inspect_output, [Priority_player, 'Stack']),
		read_card('Stack', Priority_player),
	   	inspect_game(Priority_player).

%%%%%%%%%%%%%%inspect_output/2 (4) 28/01/2011

	% b: battlefield
	inspect_output(Priority_player, 98):-
		read_card('Battlefield', Priority_player),
	   	inspect_game(Priority_player).

%%%%%%%%%%%%%%inspect_output/2 (5) 28/01/2011

	% g: graveyard
	inspect_output(Priority_player, 103):-
		read_card('Graveyard', Priority_player),
	   	inspect_game(Priority_player).

%%%%%%%%%%%%%%inspect_output/2 (6) 28/01/2011

	% e: exile
	inspect_output(Priority_player, 101):-
		read_card('Exile', Priority_player),
	   	inspect_game(Priority_player).

%%%%%%%%%%%%%%inspect_output/2 (7) 21/02/2011

	% m: mana pools
	inspect_output(Priority_player, 109):-
		output(inspect_output, [Priority_player, 'mana_pools']),
	   	inspect_game(Priority_player).

%%%%%%%%%%%%%%inspect_output/2 (8) 12/03/11

	% w: winning conditions (life, poison, cards in library, other?)
	inspect_output(Priority_player, 119):-
		output(inspect_output, [Priority_player, 'win']),
	   	inspect_game(Priority_player).

%%%%%%%%%%%%%%inspect_output/2 (9) 28/01/2011

	% c: cancel
	inspect_output(_Priority_player, 99):-
		true.


%%%%%%%%%%%%%%read_card/2 22/04/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* read_card(+Zone, +Player) */
	% lets the player read a card's text.

%%%%%%%%%%%%%%read_card/2 (1) 22/04/11

	% Outputs information about cards in the player's hand.
	% Normally, players should only be able to see their own cards.
	read_card('Hand', Player):-
		zone(Player, 'Hand', Cards_in_zone),
		findall(Card,
				member(Card, Cards_in_zone),
			Cards),
		append([cancel], Cards, Full),
		prompt_switches(Full, [Map, Switches]),
		read_card('Hand', Map, Switches, []).
	% You can actually use this to reveal Glee-min's hand.
	% The prompt of course is to the human player, or to the console anyway.

%%%%%%%%%%%%%%read_card/2 (2) 23/04/11

	% Outputs partition information

	read_card(Zone, _Player):-
		findall(Full,
				( zone(_PLayer, Zone, Objects),
				member(Object, Objects),
				( object_handle(Object, Name-_Id);
				  object_handle_0(Object, Name-_Id)),
				( phrase(ability, Name)->
				atom_word_list([Name], [], [Full]);
				Full = Name  )		),
			Cards),
		sort(Cards, Sorted),
		% ^ Again, object_handle/_0 cause uncontrolled backtracking.
		% Fix it and remove this sort clause, because otherwise it's
		%  not needed.
		append([cancel], Sorted, Full),
		prompt_switches(Full, [Map, Switches]),
		read_card(Zone, Map, Switches, []).


%%%%%%%%%%%%%%read_card/3 23/04/11
%%%%%%%%%%%%%%%%%%%%%%%
	/*read_card(+Map, +Switches, :OK/Go-on) */
	% input/3 call for read_card/1. Loops until
	%  the user enters cancel

%%%%%%%%%%%%%%read_card/3 (1) 23/04/11

	read_card(_Zone, _Map, _Switches, ok).

%%%%%%%%%%%%%%read_card/3 (2) 23/04/11

	read_card(Zone, Map, Switches, _ok):-
		output(inspect_output, [Zone, Map, Switches]),
		Context = [Map, Switches],
		(input(read_card, Context, Input) ->
		atom_codes(Switch, [Input]),
		card_characteristics(Map, Switch, Ok),
		read_card(Zone, Map, Switches, Ok);
		read_card(Zone, Map, Switches, [])).


%%%%%%%%%%%%%%card_characteristics/3 22/04/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* card_characteristics(+Map, +Switch, :OK/Go-On) */
	% handles user input from read_card/1

%%%%%%%%%%%%%%card_characteristics/3 (1) 23/04/11

	card_characteristics(Map, Switch, ok):-
		member(cancel-Switch, Map).

%%%%%%%%%%%%%%card_characteristics/3 (2) 23/04/11

	card_characteristics(Map, Switch, []):-
		member(Card-Switch, Map),
		card_characteristics(Card).

%%%%%%%%%%%%%%card_characteristics/3 (3) 26/04/11

	% Control should only get here if the object on
	%  the Stack is an ability.
	card_characteristics(Map, Switch, []):-
		member(Ability-Switch, Map),
		output(read_card, [is_an_ability, Ability]).



%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	Ideally, this should let the user see a card,
	  then immediately choose another one (now it
	  returns to the previous menu).
*/












%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%               Notes               %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%actions_taken/ 29/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* actions_taken(+Player, +Actions) */
	% Lists the actions taken by a player in this turn.
	% Each action is a list of [+Action_type, Further_details]
	% Where further details can be any number of relevant terms.
	% OK, need to define this.

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	Action_types: 		Further_details: 		Example:
	'play_land'			(list of) Land_name	[Plains, Island]

It may have been convenient here to be keeping a turn count.
*/


%	actions_taken('Player 1', []).
%	actions_taken('Player 2', []).

	% This means I'll have to cleanup the facts at the end of turns?

%*/


