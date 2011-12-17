% First saved: 17/02/11
% Last saved: 26/04/11
%
% 	Status:
%
%
% Doing:
%	Done fixing inspect ouptut for abilities on the Stack
%	Noticed the inspect output takes in lowercase zone names as arguments!
%	adding activate_abilities.
% 	updating deal_damage outputs (to new inputs)
%	add [h]elp options to player actions sub-menus!
%
% Todo
%	add all the wrong_input clauses from combat output on!
%	add break option to player actions menu (needs player_actions change)?
%
% NOTES:
%	Fix the mise options in declare_blockers outputs (cancel and ok not
% 	  always both allowed).

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
%%%%              Input                %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%input/3 01/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* input(+Caller, +Context, -Input) */
	% Handles user input depending on the Caller predicate.
	% Context is a list of arguments used to establish the conditions
	%  of the call, eg, information about the current step or phase etc.

	% Check the user input, output a warning if it's wrong
	%  otherwise pass control back to the caller predicate.
	input(Caller, Context, Input):-
		prompt([Caller, Context]), !,
		% Red cut. If wrong_input/3 is true, we must not
		%  backtrack into prompt/1, but all the way to Caller.
		flush, get(Input), flush,
		(	wrong_input(Caller, Context, Input)	->
			warning(Caller, Context), fail ;
			true
		).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%string_input/3 22/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* string_input(+Caller, +Context, -Input) */
	% Same as input/3 but takes input as a string of characters
	%  (not character codes!) and returns it as an atom.

	% Check the user input, output a warning if it's wrong
	%  otherwise pass control back to the caller predicate.
	string_input(Caller, Context, Input):-
		prompt([Caller, Context]), !,
%		flush, fread(a, 0, -1, Input), flush, %Swi, 29/06/11
		%list_read(Input),
		readlist([Input]),  %, atom_to_list(Atom, Input),
		(	wrong_input(Caller, Context, Input)	->
			warning(Caller, Context), fail ;
			true
		).


%%%%%%%%%%%%%%prompt/1 31/01/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* prompt([+Caller, +Context]) */
	% Prompt user for input, depending on the Caller predicate.
	% Context is a list used to establish the conditions of the call.

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	Args are in a list because prompt/2 is a built-in.
*/

%%%%%%%%%%%%%%prompt/1  (1) 31/01/2011

	prompt([main, _]):-
		output(main, ['instructions']).

%%%%%%%%%%%%%%prompt/1  (2.1) 31/01/2011
% Context == [Active_player, Priority_player, Step, Phase, New_play]

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	Note that Step information comes as State(Step) tuples
	  but the player is prompted for (this) input only when he
	  or she gets priority, so only while continues(Step).
	Specifically, Combat actions choices like declaring attackers
	  or blockers etc are not taken while a player has priority
	  but at the beginning of a Combat step, as turn-based actions
	  and therefore input/ output is handled by game_actions,
	  _not_ player_actions.

*/

	% The Active_player has priority, it's a Main phase
	%  and he or she has not played a land this turn.
	prompt([player_actions, [Active, Active, _, Phase, _]]):-
		(Phase == 'First Main';
		Phase == 'Second Main'),
		played_land(Active, 'no'),
		output(player_actions, ['prompt', 'land']), !.
	% Cut or on a wrong input, we fall in the clause below
	%  which causes strange behaviour.

%%%%%%%%%%%%%%prompt/1  (2.2) 31/01/2011

	prompt([player_actions, _]):-
		output(player_actions, ['prompt', 'no_land']).

%%%%%%%%%%%%%%prompt/1  (3) 31/01/2011

	prompt([inspect_game, _]):-
		output(inspect_game, ['prompt']).

%%%%%%%%%%%%%%prompt/1  (4) 31/01/2011
% Switches: list of user input choices with bracketed switches,
%  eg "[P]lains, [S]wamp, [I]sland"

	prompt([play_land, [_Player, Map, Switches]]):-
		output(play_land, ['choose', Map, Switches]).


%%%%%%%%%%%%%%prompt/1  (5) 16/02/2011

	prompt([cast_spell, [Map, Switches]]):-
		output(cast_spell, ['choose', [Map, Switches]]).

%%%%%%%%%%%%%%prompt/1  (6) 16/02/2011

	prompt([spell_abilities, []]):-
		output(spell_abilities, ['prompt']).

%%%%%%%%%%%%%%prompt/1  (7) 31/03/2011

	prompt([ability_source, [Map, Switches, Identified]]):-
		output(ability_source, ['choose', [Map, Switches, Identified]]).


	prompt([choose_ability, [Map, Switches]]):-
		output(choose_ability, [prompt, [Map, Switches]]).


%%%%%%%%%%%%%%prompt/1  (8) 20/02/2011

	prompt([tap_for_mana, [Player, Map, Switches, Identified]]):-
		output(tap_for_mana, ['choose_source', Player, Map, Switches, Identified]).

%%%%%%%%%%%%%%prompt/1  (9) 20/02/2011

	prompt([spend_mana, [Mana_pool, Cost]]):-
		output(spend_mana, ['prompt', Mana_pool, Cost]).

%%%%%%%%%%%%%%prompt/1  (10) 05/03/2011

	prompt([declare_attackers, [Player, Switches, Identified]]):-
		output(declare_attackers, ['prompt', Player, Switches, Identified]).

%%%%%%%%%%%%%%prompt/1  (11) 05/03/2011

	prompt([one_blocker, [Player, Switches, Identified]]):-
		output(one_blocker, ['prompt', Player, Switches, Identified]).

%%%%%%%%%%%%%%prompt/1  (12) 06/03/2011

	prompt([one_attacker, [Controller, Blocker, Switches, Identified]]):-
		output(one_attacker, ['prompt', Controller, Blocker, Switches, Identified]).

%%%%%%%%%%%%%%prompt/1  (13) 06/03/2011

	prompt([order_blockers, [Attacker, Blockers, Map, Switches]]):-
		output(order_blockers, ['prompt', Attacker, Blockers, Map, Switches]).

%%%%%%%%%%%%%%prompt/1  (14) 06/03/2011

	prompt([order_attackers, [Blocker, Attackers, Map, Switches]]):-
		output(order_attackers, ['prompt', Blocker, Attackers, Map, Switches]).

%%%%%%%%%%%%%%prompt/1  (15) 06/03/2011

	prompt([assign_lethal_damage, [Controller, Bestower, Recipient, Bestower_P, Recipient_T]]):-
		output(assign_lethal_damage, [prompt, Controller, Bestower, Recipient, Bestower_P, Recipient_T]).

%%%%%%%%%%%%%%prompt/1  (16) 04/04/2011

	prompt([choose_targets, Context]):-
		output(choose_targets, [prompt, Context]).

%%%%%%%%%%%%%%prompt/1  (17) 05/04/2011

	prompt([discard_to_max, Context]):-
		output(discard_to_max, [prompt, Context]).

%%%%%%%%%%%%%%prompt/1  (18) 19/04/2011

	prompt([choose_deck, Context]):-
		output(choose_deck, [prompt, Context]).

%%%%%%%%%%%%%%prompt/1  (19) 22/04/2011

	prompt([read_card, Context]):-
		output(read_card, [prompt, Context]).


%%%%%%%%%%%%%%wrong_input/2 31/01/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/*  wrong_input(+Caller, +Context, +Input) */
	% Check for wrong input, in the given Contxt of the Caller predicate.

%%%%%%%%%%%%%%wrong_input/2 (1) 31/01/2011

	wrong_input(main, _, Input):-
		not
		(
			Input == 100;	% Decks
			Input == 115;	% Start
			Input == 104;	% Help
			Input == 113	% Quit
		).

%%%%%%%%%%%%%%wrong_input/2 (2) 31/01/2011

	wrong_input(player_actions, _, Input):-
		not
		(
			Input == 108;	% Land
			Input == 115;	% Spells
			Input == 97;	% Abilities
			Input == 116;	% Take
			Input == 112;	% Pass
			Input == 104;	% Help
			Input == 67;	% Concede (uppercase C)
			Input == 105;	% Inspect
			Input == 100	% Debug
		).

%%%%%%%%%%%%%%wrong_input/2 (3) 31/01/2011

	wrong_input(inspect_game, _, Input):-
		not
		(
			Input == 108;	% Library
			Input == 104;	% Hand
			Input == 115;	% Stack
			Input == 98;	% Battlefield
			Input == 103;	% Graveyard
			Input == 101;	% Exile
			Input == 99;	% Cancel
			Input == 109;	% Mana
			Input == 119	% Win conditions

		).

%%%%%%%%%%%%%%wrong_input/2 (4) 31/01/2011

	wrong_input(play_land, Context, Input):-
		Context = [_Player, Map, _Switches],
		atom_codes(Switch, [Input]),
		\+  member(_Land_card - Switch , Map).

%%%%%%%%%%%%%%wrong_input/2 (5) 16/02/2011

	wrong_input(cast_spell, Context, Input):-
		Context = [Map, _Switches],
		atom_codes(Switch, [Input]),
		\+  member(_Spell - Switch , Map).

%%%%%%%%%%%%%%wrong_input/2 (6) 31/03/2011

	wrong_input(ability_source, Context, Input):-
		Context = [Map, _Switches, _Identified],
		atom_codes(Char, [Input]),
		\+ member(_Source - Char, Map).

%%%%%%%%%%%%%%wrong_input/2 (7) 16/02/2011

	wrong_input(tap_for_mana, Context, Input):-
		Context = [_Player, Map, _Switches, _Identified],
		atom_to_list(Input, List),
		findall(Symbol,
			(member(Symbol, List),
			\+ member(_Source - Symbol, Map)),
		Symbols),
		Symbols \= [].

%%%%%%%%%%%%%%wrong_input/2 (8) 20/02/2011

	 wrong_input(spend_mana, _Context, Input):-
		string_to_list(Input, List),
		findall(Symbol,
			(member(Symbol, List),
			\+ (
				Symbol == w;	% w
				Symbol == u;	% u
				Symbol == b;	% b
				Symbol == r;	% r
				Symbol == g;	% g
				Symbol == c		% Cancel
			)),Symbols),
		Symbols \= [].


%%%%%%%%%%%%%%wrong_input/2 (X) 04/04/11

	wrong_input(choose_targets, Context, Input):-
		Context = [_Ability, Map, _Switches, _Identified],
		atom_codes(Switch, [Input]),
		\+  member(_Target - Switch , Map).


%%%%%%%%%%%%%%wrong_input/2 (X) 22/04/11

	wrong_input(read_card, Context, Input):-
		Context = [Map, _Switches],
		atom_codes(Switch, [Input]),
		\+  member(_Target - Switch , Map).


%%%%%%%%%%%%%%wrong_input/2 (9) 05/03/11

/*
	These don't really work. Do them properly habibi mou.
*/

	wrong_input(declare_attackers, Context, Input):-
		Context = [_Player, Map, _Switches, _Identified],
		atom_to_list(Input, List),
		findall(Switch,
			(member(Symbol, List),
			\+ member(_Creature - Switch, Map)),
		Switches),
		Switches \= [].

%%%%%%%%%%%%%%wrong_input/2 (10) 05/03/11

	wrong_input(declare_blockers, Context, Input):-
		Context = [_Player, Map, _Switches, _Identified],
		atom_to_list(Input, List),
		findall(Switch,
			(member(Symbol, List),
			\+ member(_Creature - Switch, Map)),
		Switches),
		Switches \= [].

%%%%%%%%%%%%%%wrong_input/2 (11) 05/03/11

	wrong_input(order_blockers, Context, Input):-
		Context = [_Attacker, _Blockers, Map, Switches],
		atom_to_list(Input, List),
		findall(Switch,
			(member(Symbol, List),
			\+ member(_Creature - Switch, Map)),
		Switches),
		Switches \= [].


%%%%%%%%%%%%%%wrong_input/2 (12) 07/03/11

	wrong_input(assign_lethal_damage, Context, Input):-
		Context = [_Attacker, _Blockers, Map, Switches],
		atom_to_list(Input, List),
		findall(Switch,
			(member(Symbol, List),
			\+ member(_Creature - Switch, Map)),
		Switches),
		Switches \= [].

%%%%%%%%%%%%%%wrong_input/2 (X) 05/04/11

	wrong_input(discard_to_max, Context, Input):-
		Context = [_Max_hand_size, _Length, Map, _Switches],
		atom_to_list(Input, List),
		findall(Switch,
			(member(Switch, List),
			\+ member(_Card - Switch, Map)),
		Switches),
		Switches \= [].


%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	A problem is I don't know what switch is chosen for cancel,
	  except that it's first in the picking so it will probably end up
	  with "c". That's not 100% certain though, so it's conceivable that
	  this may fail- plan ahead.

*/


%%%%%%%%%%%%%%warning/1 31/01/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* warning(+Caller) */
	% Warn user for wrong input, depending on the Caller predicate.

%%%%%%%%%%%%%%warning/1 (1) 31/01/2011

	warning(main, _):-
		output(main, ['warning']).

%%%%%%%%%%%%%%warning/1 (2) 31/01/2011

	warning(player_actions, _):-
		output(player_actions, [warning]).

%%%%%%%%%%%%%%warning/1 (3) 31/01/2011

	warning(inspect_game, _):-
		output(inspect_game, ['warning']).

%%%%%%%%%%%%%%warning/1 (4) 31/01/2011

	warning(play_land, _):-
		output(play_land, ['warning']).


%%%%%%%%%%%%%%warning/1 (5) 31/01/2011

	warning(cast_spell, _):-
		output(cast_spell, ['warning']).

%%%%%%%%%%%%%%warning/1 (5) 31/03/2011

	warning(ability_source, _):-
		output(ability_source, ['warning']).

%%%%%%%%%%%%%%warning/1 (6) 31/01/2011

	warning(tap_for_mana, _):-
		output(tap_for_mana, ['warning']).

%%%%%%%%%%%%%%warning/1 (6) 31/01/2011

	warning(spend_mana, _):-
		output(spend_mana, ['warning']).

%%%%%%%%%%%%%%warning/1 (7) 05/03/11

	warning(declare_attackers, _):-
		output(declare_attackers, ['warning']).

%%%%%%%%%%%%%%warning/1 (8) 05/03/11

	warning(declare_blockers, _):-
		output(declare_blockers, ['warning']).

%%%%%%%%%%%%%%warning/1 (9) 05/03/11

	warning(declare_blockers, _):-
		output(order_blockers, ['warning']).

%%%%%%%%%%%%%%warning/1 (10) 05/03/11

	warning(choose_targets, _):-
		output(choose_targets, [warning]).

%%%%%%%%%%%%%%warning/1 (11) 05/03/11

	warning(discard_to_max, _):-
		output(discard_to_max, [warning]).

%%%%%%%%%%%%%%warning/1 (12) 23/04/11

	warning(read_card, _):-
		output(read_card, [warning]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%             Output                %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* output(+Predicate_name, +[Arguments]) */
	% General output predicate for the parts of a turn
	% Output_clause is instantiated by the parent predicate
	%  depending on the desired/ necessary output.

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	Having checks about outputting or not here could be confusing.
	Currently utputs are generally stand-ins for functionality,
	so when they should happen should be decided by the calling
	predicates?
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%output/2 03/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* output(+Caller, +Context) */
	% Handles output for the Caller predicate.
	% Context is a list; the first element determines the specific
	%  output case for Caller and the remaining are arguments to output


%%%%%%%%%%%%%%output/2 (0.0) 31/01/11
% 0.0: minimaxing output.

	% Don't show the AI thinking...
	output(_, _):-
		minimaxing.

%%%%%%%%%%%%%%output/2 (0.1) 31/01/11
% 0.* clauses: main output.

	output(main, ['instructions']):-
		nl,
		write(`Enter: "[d]ecks", "[s]tart", "[h]elp", or "[q]uit" to proceed.`), nl.


%%%%%%%%%%%%%%output/2 (0.2) 31/01/11

	output(main, ['warning']):-
		write(`Please enter a valid option`), nl.


%%%%%%%%%%%%%%output/2 (0.3) 31/01/11

	output(main, ['help']):-
		nl,
		write(`You are at the application main menu. You have the following options:`), nl,
			%tab(5), write(`Type "[n]ame" to enter your name so that Gleemin can address you by it.`), nl,
			tab(5), write(`Type "[d]ecks" to choose a deck for yourself and for Gleemin.`), nl,
			tab(5), write(`Type "[s]tart" to start a game of Magic: the Gathering against Gleemin.`), nl,
			tab(5), write(`Type "[h]elp" to see this help text.`), nl,
			tab(5), write(`Type "[q]uit" to exit this program.`), nl, nl,
			tab(25), write(`How to enter text`), nl,
		write(`Whenever you are prompted for input you will be given a list of "switch" words.`), nl,
		write(`Each switch word has one of its letters, enclosed in square brackets,`), nl,
		  write(`  like this: [h]elp. You can type the whole word or just the "switched" letter.`), nl,
		  write(`  You must press Enter afterwards for the application to accept your option.`), nl,
		write(`In some cases you will be asked to enter a "string of switches". In that case`), nl,
		  write(`  you should enter the letter-switches one after the other, just like writing a`), nl,
		  write(`  word. If you enter a switch-word whole at that point, you may choose the`), nl,
		  write(`  wrong options.`), nl,
		write(`Please note that at the time of writing error-checking on user input is not`), nl,
		  write(`  complete so making a wrong choice may cause the application to crash or`), nl,
		  write(`  go into an infinite loop! If this happens, press ctrl & Scroll Lock to exit.`), nl,
		  write(`  (Scroll Lock is usually above the Home key, which is usually above the`), nl,
		  write(`  arrow keys; if all else fails, Alt+Ctrl+Del will do it!).`), nl,
		write(`On the bright side, you don't need to end your input with a "." or enclose it`), nl,
		  write(`  in single quotes for it to be read in correctly.`), nl,
		write(`All output/ input in Gleemin is textual. Help text will always fit in 80`), nl,
		  write(`  columns so you will not need a lot of screen space to read it. The actual`), nl,
		  write(`  game uses indentation to separate output, so for playing you may need to`), nl,
		  write(`  adjust your Prolog window a bit.`), nl.


%%%%%%%%%%%%%%output/2 (1.1) 03/01/11
% 1.* clauses: turn_begins output.

		% There is a new active Player
		output(turn_begins, ['new', Player]):-
			tab(5),
			write(Player),
			write(`'s turn begins`), nl.

%%%%%%%%%%%%%%output/3 (1.2) 03/01/11

		% The active Player remains the same.
		output(turn_begins, ['current', Player]):-
			tab(5),
			write(`Active player: `),
			write(Player), nl.


%%%%%%%%%%%%%%output/3 (2.1) 03/01/11
% 2.* clauses: phase_begins output.

	% A phase begins
	output(phase_begins, [Phase]):-
		tab(10),
		write(Phase),
		write(` phase begins`), nl.

%%%%%%%%%%%%%%output/3 (2.2) 03/01/11

	% The Untap or Cleanup Step is ongoing or ends. No output.
	output(phase_begins, [State, Step, _]):-
		(
			Step == 'Untap';
			Step == 'Cleanup'
		),
		(
			State == ongoing;
			State == ends
		).
	% This will have to be refined for Cleanup steps where
	%  players receive priority.

%%%%%%%%%%%%%%output/3 (2.3) 03/01/11

	% Any other Step of a Phase ends. No output.
	output(phase_begins, [ends, _, _]):-
		true.

%%%%%%%%%%%%%%output/3 (2.4) 03/01/11

	% Any other Step of a Phase is ongoing.
	output(phase_begins, [_, _, Phase]):-
		tab(10),
		write(`Current phase: `),
		write(Phase), nl.


%%%%%%%%%%%%%%output/3 (3.1) 03/01/11
% 3.* clauses: step_begins output.

	% Some phases have no steps.
	output(step_begins, []):-
		tab(15),
		write(`This phase has no steps.`), nl.
	% This will fire at the end of an [] Step
	%  because of the slight fudge, which I should correct
	%  anyway. So I leave it here until I have.

%%%%%%%%%%%%%%output/3 (3.2) 03/01/11

	% A step begins
	output(step_begins, [Step]):-
		tab(15),
		write(Step),
		write(` step begins.`), nl.

%%%%%%%%%%%%%%output/3 (3.3) 03/01/11

	% The Untap step is ongoing
	output(step_begins, [ongoing, 'Untap']):-
		true.

	% The Cleanup step may allow priority.
	% Will implement eventually.

%%%%%%%%%%%%%%output/3 (3.4) 03/01/11

	% Any other Step is ongoing.
	output(step_begins, [ongoing, Step]):-
		tab(15),
		write(`Current step: `),
		write(Step), nl.

%%%%%%%%%%%%%%output/3 (4.1) 03/01/11
% 4.* clauses: phase_actions output.

	output(phase_actions, ['Beginning']):-
		tab(20),
 		write(`Beginning phase actions`), nl.

%%%%%%%%%%%%%%output/3 (4.2) 03/01/11

	output(phase_actions, ['First Main']):-
		tab(20),
		write(`Playing spells and abilities (1)`), nl.

%%%%%%%%%%%%%%output/3 (4.3) 03/01/11

	output(phase_actions, ['Second Main']):-
		tab(20),
		write(`Playing spells and abilities (2)`), nl.

%%%%%%%%%%%%%%output/3 (5.1) 03/01/11
% 5.* clauses: step_actions output.

	output(step_actions, ['Untap']):-
		tab(20),
		write(`Untapping`), nl.

%%%%%%%%%%%%%%output/3 (5.2) 03/01/11

	output(step_actions, ['Upkeep']):-
		tab(20),
		write(`Managing Upkeep`), nl.

%%%%%%%%%%%%%%output/3 (5.3) 03/01/11

	output(step_actions, ['Draw']):-
		tab(20),
		write(`Drawing a card`), nl.

%%%%%%%%%%%%%%output/3 (5.4) 03/01/11

	output(step_actions, ['Beginning of Combat']):-
		tab(20),
		write(`Beginning combat`), nl.

%%%%%%%%%%%%%%output/3 (5.5) 03/01/11

	output(step_actions, ['Declare Attackers']):-
		tab(20),
		write(`Declaring attackers`), nl.

%%%%%%%%%%%%%%output/3 (5.6) 03/01/11

	output(step_actions, ['Declare Blockers']):-
		tab(20),
		write(`Declarking blockers`), nl.

%%%%%%%%%%%%%%output/3 (5.7) 03/01/11

	output(step_actions, ['Combat Damage']):-
		tab(20),
		write(`Assigning combat damage`), nl.

%%%%%%%%%%%%%%output/3 (5.8) 03/01/11

	output(step_actions, ['End of Combat']):-
		tab(20),
		write(`Ending combat`), nl.

%%%%%%%%%%%%%%output/3 (5.9) 03/01/11

	output(step_actions, ['End']):-
		tab(20),
		write(`Ending turn`), nl.

%%%%%%%%%%%%%%output/3 (5.10) 03/01/11

	output(step_actions, ['Cleanup']):-
		tab(20),
		write(`Doing cleanup`), nl.


%%%%%%%%%%%%%%output/3 (6) 03/01/11
% 6.* clauses: end_actions output.

		output(end_actions, []):-
		tab(20),
 		write(`Emptying mana pools`), nl.


%%%%%%%%%%%%%%output/3 (7.1) 03/01/11
% 7.* clauses: receives_priority output.

	% No player receives priority this step.
	output(receives_priority, []):-
		tab(25),
		write(`No Priority`), nl.

%%%%%%%%%%%%%%output/3 (7.2) 03/01/11

	% The priority Player has changed
	output(receives_priority, ['new', Player]):-
		tab(25),
		write(`New priority player : `),
		write(Player), nl.

%%%%%%%%%%%%%%output/3 (7.4) 03/01/11

	% The priority player keeps priority
	output(receives_priority, [Player]):-
		tab(25),
		write(`Priority player : `),
		write(Player), nl.


%%%%%%%%%%%%%%output/2 (8.1) 01/02/11
% 8.* clauses: player_actions output.

	output(player_actions, []):-
		tab(25),
		write(`No Prompt`), nl.

%%%%%%%%%%%%%%output/2 (8.X) 01/02/11

	% Active_player's Main phase actions.
	output(player_actions, ['prompt', Context]):-
		tab(25),
		write(`Enter:`), nl,
			player_actions_context(Context),
			tab(30),
			write(`"[s]pells"`), nl,
			tab(30),
			write(`"[a]bilities"`), nl,
			tab(30),
			write(`"[t]ake"`), nl,
			tab(30),
			write(`"[p]ass"`), nl,
			tab(30),
			write(`"[h]elp"`), nl,
			tab(30),
			write(`"[i]nspect"`), nl,
		tab(25),
		write(`to play.`), nl,
		tab(25),
		write(`Enter: "[C]oncede" to quit.`), nl,
			tab(30),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/3 (8.3) 03/01/11

	output(player_actions, [warning]):-
		tab(25),
		write(`Please enter a valid option`),
		nl,
			tab(35),
			write(`* * *`), nl.


%%%%%%%%%%%%%%output/3 (9.1) 03/01/11
% 9.* clauses: play output.

	output(play, [Priority_player, 'land']):-
		tab(25),
		write(Priority_player),
		write(` is playing land`),
		nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/3 (9.2) 03/01/11

	output(play, [Priority_player, 'spells']):-
		tab(25),
		write(Priority_player),
		write(` is casting spells`),
		nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/3 (9.3) 03/01/11

	output(play, [Priority_player, 'abilities']):-
		tab(25),
		write(Priority_player),
		write(` is activating abilities`),
		nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/3 (9.4) 03/01/11

	output(play, [Priority_player, 'special']):-
		tab(25),
		write(Priority_player),
		write(` is taking special actions`),
		nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/3 (9.5) 03/01/11

	output(play, [Priority_player, 'help']):-
		tab(25),
		write(Priority_player),
		write(` requested help`),
		nl,
		tab(25),
		write(`Enter: `), nl,
			tab(30),
			write(`"[l]and": to play land`), nl,
			tab(30),
			write(`"[s]pells": to cast spells`), nl,
			tab(30),
			write(`"[a]bilities": to activate abilities of permanents`), nl,
			tab(30),
			write(`"[t]ake": to take special actions (other than playing a land)`), nl,
			tab(30),
			write(`"[p]ass": to do nothing and pass priority`), nl,
			tab(30),
			write(`"[h]elp": to show this help text`), nl,
			tab(30),
			write(`"[i]nspect": to see your cards in each zone and the state of the game`), nl,
			tab(30),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/3 (9.6) 03/01/11

	output(play, [Priority_player, 'pass']):-
		tab(25),
		write(Priority_player),
		write(` passes priority`),
		nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/3 (9.7) 03/01/11

	output(play, [Priority_player, 'concede']):-
		tab(25),
		write(Priority_player),
		write(` concedes`),
		nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/3 (9.8) 03/01/11

	output(play, [Priority_player, 'debug']):-
		tab(25),
		write(Priority_player),
		write(` debug`), nl.


%%%%%%%%%%%%%%output/3 (10.1) 28/01/11
% 10.* clauses: inspect_game output.

%%%%%%%%%%%%%%output/3 (10.1) 28/01/11

	% Inspect prompt
	output(inspect_game, ['prompt']):-
		tab(25),
		write(`Enter:`), nl,
			tab(30),
			write(`"[l]ibraries"`),nl,
			tab(30),
			write(`"[h]ands"`), nl,
			tab(30),
			write(`"[s]tack"`), nl,
			tab(30),
			write(`"[b]attlefield"`), nl,
			tab(30),
			write(`"[g]raveyards"`), nl,
			tab(30),
			write(`"[e]xile"`), nl,
		tab(25),
		write(`to see cards in that zone.`), nl,
		tab(25),
		write(`Enter: [m]ana to see mana in players' pools.`), nl,
		tab(25),
		write(`Enter: [w]in to see how close each player is to winning the game.`), nl,
		tab(25),
		write(`Enter: [c]ancel to return to the previous menu.`), nl,
			tab(30),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/3 (10.2) 28/01/11

	% inspect warning
	output(inspect_game, ['warning']):-
		nl,
		tab(25),
		write(`Please enter a valid option`),
		nl,
			tab(35),
			write(`* * *`), nl.


%%%%%%%%%%%%%%output/3 (11.1) 28/01/11
% 11.* clauses: inspect_output output.

	output(inspect_output, [_Priority_player, 'Library']):-
		tab(25),
		write(`Cards in libraries: `), nl, fail, !.

	output(inspect_output, [_Priority_player, 'Library']):-
		zone(Player, 'Library', Cards),
		length(Cards, Length),
		tab(30),
		write(Player),
		write(`: `), write(Length), write(` cards`), nl, fail.

	output(inspect_output, [_Priority_player, 'Library']):-
			tab(30),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/3 (11.2) 28/01/11

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	Only the player should be able to see his or her hand!
	Also, the computer player doesn't need output for
	  inspection of game states.
*/

	output(inspect_output, ['Hand', _Map, _Switches]):-
		tab(25),
		write(`Cards in Hand:`), nl, fail, !.

	output(inspect_output, ['Hand', _Map, _Switches]):-
		zone(Player, 'Hand', Cards),
		length(Cards, Length),
		tab(30),
		write(Player),
		write(`: `),
		write(Length), write(` cards`), nl, fail.

	output(inspect_output, ['Hand', [_Cancel | Map], [_CAncel | Switches]]):-
		findall(Info,(member(Card-_Switch, Map, Position),
				member(Switched, Switches, Position),
				card([card_name Card, mana_cost Cost, type_line [_, [Type], _], _, _, _, _, _]),
				Info = [ Switched, Type, Cost ]), Infos),
		pp_style('Simple', Header, Body, Footer),
		pretty_print([`Name`, `Type`, `Mana Cost`],Infos, 25, Header,Body,Footer).

%%%%%%%%%%%%%%output/3 (11.3) 28/01/11

	output(read_card, [warning]):-
		tab(25), write(`Please enter a valid option`), nl,
			tab(35),
			write(`* * *`), nl, nl.

	% 23/04/11
	output(inspect_output, ['Stack', Map, Switches]):-
		findall(Card, (zone('Stack', Stack), member(Card, Stack)), Cards),
		length(Cards, Length),
		tab(25), write(Length), write(` objects on the Stack`), nl,
		output_stack(Cards, Map, Switches, 25),
			tab(35),
			write(`* * *`), nl.

	% 26/04/11
	output(read_card, [is_an_ability, Object]):-
		tab(25), write(`"`), write(Object),write(`"`),nl,
		tab(25), write(`This object is an ability; its text is its only characteristic.`), nl,
			tab(35),
			write(`* * *`), nl, nl.

%%%%%%%%%%%%%%output/3 (11.4) 28/01/11

	output(inspect_output, [Partition, Map, Switches]):-
		tab(25),
		(Partition = 'Battlefield' ->
		write(`Cards on the `);
		write(`Cards in the `)),
		write(Partition), write(`:`), nl, nl,
		output_partition(Partition, Map, Switches, 35, 25), nl.

%%%%%%%%%%%%%%output/3 (11.6) 28/01/11

	output(inspect_output, [_Priority_player, 'mana_pools']):-
		findall(Player, player(Player), Players),
		output_mana_pools(Players).

%%%%%%%%%%%%%%output/3 (11.7) 12/03/11

	output(inspect_output, [_Priority_player, 'win']):-
		findall([Player, Life, Poison, Cards],
				(player(Player),
				life_total(Player, Life),
				poison_counters(Player, Poison),
				zone(Player, 'Library', Library),
				length(Library, Cards)
				),
			Player_state),
		tab(25), write(`Players' winning conditions`), nl,
		pp_style('Simple', Header, Body, Footer),
		pretty_print([`Player`,`Life total`, `Poison counters`, `Cards in library`],Player_state, 25, Header,Body,Footer),
		tab(35),
			write(`* * *`), nl.


%%%%%%%%%%%%%%output/3 (12.1) 03/01/11
% 12.* clauses: step_ends output.

	output(step_ends, [ends(Step)]):-
				tab(15),
		write(`Ending step: `),
		write(Step), nl,
			tab(25),
			write(`#* * * *#`), nl, nl.


%%%%%%%%%%%%%%output/3 (13.1) 03/01/11
% 13.* clauses: phase_ends output.

	output(phase_ends, [Phase]):-
		tab(10),
		write(`Ending phase: `),
		write(Phase), nl,
			tab(20),
			write(`#* * * * *#`), nl, nl.


%%%%%%%%%%%%%%output/3 (14.1) 03/01/11
% 14.* clauses: turn_ends output.

	output(turn_ends, [Current_player]):-
		tab(5),
		write(Current_player),
		write(`'s turn ends `), nl,
		tab(15),
		write(`##* * * * * * * * * * * *##`), nl, nl.


%%%%%%%%%%%%%%output/2 (15.1) 31/01/11
% 15.* clauses: play_land output.

	output(play_land, ['wrong_turn']):-
		tab(25),
		write(`You can only play land during your turn`),  nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (15.2) 31/01/11

	output(play_land, ['wrong_phase']):-
		tab(25),
			write(`Not a main phase`), nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (15.3) 31/01/11

	output(play_land, ['land_limit']):-
		tab(25),
			write(`Already played land this turn`), nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (15.4) 31/01/11

	output(play_land, ['stack_not_empty']):-
		tab(25),
			write(`Stack not empty`), nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (15.5) 31/01/11

	output(play_land, ['no_lands_in_hand']):-
		tab(25),
			write(`No lands in hand`), nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (15.6) 20/02/11

	output(play_land, ['choose', [_Cancel | Map], [Cancel | Land]]):-
		tab(25), write(`Choose from: `), nl,
		mana_colours(Map, Land, Land_colours),
		pp_style('Simple', Header, Body, Footer),
		pretty_print([`Name`, `Mana Colours`],Land_colours, 25, Header,Body,Footer),
		tab(25),
		write(`Or enter `), write(Cancel),
		write(` to return to the previous menu`), nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (15.7) 31/01/11

	output(play_land, ['warning']):-
		tab(25),
		write(`Please enter a valid option`), nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (15.8) 31/01/11

	output(play_land, ['played_land', Player, Land_card]):-
		tab(25),
		write(Player),
		write(` plays a `), write(Land_card), nl,
			tab(35),
			write(`* * *`), nl.


%%%%%%%%%%%%%%output/2 (16.1) 16/02/11
% 16.* clauses: cast_spell output.

	output(cast_spell, [ 'choose', [ [_Other_ | [_Cancel | Map]] , [_Other | [Cancel | Hand]] ] ]):-
		findall(Info,(member(Card - _Switch, Map, Position),
				member(Switch, Hand, Position),
				card([card_name Card, mana_cost Cost, type_line [_, [Type], _], _, _, _, _, _]),
				% ^ At some point I'd like to replace this with a general querying predicate.
				Info = [ Switch, Type, Cost ]), Infos),
		tab(25), write(`Choose the spell to cast`), nl,
		pp_style('Simple', Header, Body, Footer),
		pretty_print([`Name`, `Type`, `Mana Cost`],Infos, 25, Header,Body,Footer),
		tab(25),
		write(`Enter `), write(Cancel),
		write(` to return to the previous menu `), nl,
%		tab(25),
%		write(`Or `), write(Other),
%		write(` to cast a spell from another zone than Hand`), nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (16.2) 16/02/11

	output(cast_spell, ['warning']):-
		tab(25),
		write(`Please enter a valid option`), nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (16.3) 16/02/11

	output(cast_spell, ['spell_cast', Player, Spell, Zone]):-
		tab(25),
		write(Player),
		write(` casts `), write(Spell),
		write(` from `), 	write(Zone), nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (16.4) 16/02/11

	output(cast_spell, ['no_spells', Player, Zone]):-
		tab(25),
		write(`No spells in `), write(Player),
		write(`'s `), write(Zone), nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (16.5) 22/04/11


	output(cast_spell, [spell_canceled, Player]):-
		tab(25),
		write(Player),
		write(` canceled spell casting.`), nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (16.6) 16/02/11

	output(cast_spell, ['spell_fails', Player, Spell, Zone]):-
		tab(25),
		write(Player),
		write(` fails to cast `), write(Spell),
		write(` from `), 	write(Zone), nl,
			tab(35),
			write(`* * *`), nl.


%%%%%%%%%%%%%%output/2 (17.1) 01/04/11
% 17.* clauses: pay_cost output.

	output(pay_cost, [Permanent, 'tap_failed']):-
		object_handle(Permanent, Handle),
		tab(25),
		write(Handle), write(` cannot be tapped`), nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (17.1) 01/04/11

	output(pay_cost, [Permanent, 'untap_failed']):-
		object_handle(Permanent, Handle),
		tab(25),
		write(Handle), write(` cannot be untapped`), nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (17.1) 20/02/11

	output(pay_cost, [Object, 'mana_failed']):-
		tab(25),
		write(`Mana cost could not be payed for `), write(Object), nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (18.1) 20/03/11
% 18.* clauses: draw_mana output.

	output(draw_mana, [drawn_mana, Player, Amount, [c - C,w - W,u - U,b - B,r - R,g - G]]):-
		Mana = [
				['Colourless', 	C],
				['White', 		W],
				['Blue', 		U],
				['Black', 		B],
				['Red', 		R],
				['Green', 		G]
			],
		tab(25),
		write(Amount), write(` drawn from `), write(Player), write(`'s pool`), nl,
		pp_style('Simple', Header, Body, Footer),
		pretty_print([`Name`, `Type`],Mana, 25, Header,Body,Footer),
			tab(35),
			write(`* * *`), nl, !. % Green cut. Stops take_from_pool in players.pl from
							%  backtracking over output/2 seven times. O.o

%%%%%%%%%%%%%%output/2 (18.2) 20/02/11

	output(draw_mana, [failed_to_draw_mana, Player, [c - C,w - W,u - U,b - B,r - R,g - G]]):-
		Mana = [
				['Colourless', 	C],
				['White', 		W],
				['Blue', 		U],
				['Black', 		B],
				['Red', 		R],
				['Green', 		G]
			],
		tab(25),
		write(`Failed to draw mana from `), write(Player), write(`'s pool`), nl,
		pp_style('Simple', Header, Body, Footer),
		pretty_print([`Name`, `Type`],Mana, 25, Header,Body,Footer),
			tab(35),
			write(`* * *`), nl, !. % Green cut. Stops take_from_pool in players.pl from
							%  backtracking over output/2 seven times. O.o


%%%%%%%%%%%%%%output/2 (19.1) 20/02/11
% 19.* clauses: tap_for_mana output.

	output(tap_for_mana, ['warning']):-
		tab(25),
		write(`Wrong input. Please enter a valid option`),nl,
			tab(35),
			write(`* * *`), nl.

	% Identified : [Name-Switch, Id]
	% Sources: [S]witched_sources
	output(tap_for_mana, ['choose_source', _Player, _Map, [Cancel | [Skip | []]], _Identified ]):-
		tab(25),
		write(`You have no untapped mana sources!`),nl,
		tab(25),
		write(`Enter `), write(Cancel),
		write(` to return to the previous menu`), nl,
		tab(25),
		write(`Enter `), write(Skip),
		write(` to continue casting without tapping for mana`), nl,
			tab(35),
			write(`* * *`), nl.

	output(tap_for_mana, ['choose_source', Player, _Map, [Cancel | [Skip | Sources]], Identified]):-
		tab(25), write(`Choose any number of permanents to tap for mana`), nl,
		tab(25), write(`(enter [s]witches as a string; no quotes needed)`), nl,
		output_sources(Player, Sources, Identified),
		tab(25),
		write(`Enter `), write(Skip),
		write(` to continue casting without tapping for mana`), nl,
		tab(25),
		write(`Or enter `), write(Cancel),
		write(` to return to the previous menu`), nl,
			tab(35),
			write(`* * *`), nl.


%%%%%%%%%%%%%%output/2 (20.1) 20/02/11
% 20.* clauses: spend_mana output.

	output(spend_mana, ['warning']):-
		tab(25),
		write(`Wrong mana amount`),nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (20.2) 20/02/11

	% This should take in the player's name and find his or her pool,
	%  then call output_mana_pools/1 and pass it a list with the player's
	%  name as its single element: output_mana_pools([Player])
	output(spend_mana, ['prompt', Mana_pool, Cost]):-
		Mana_pool = [c - C,w - W,u - U,b - B,r - R,g - G],
		tab(25),
		write(`Please pay `), write(Cost),
		write(` from your mana poool`), nl,
		Mana = [
				[`Colourless`, 	C],
				[`White`, 		W],
				[`Blue`,		U],
				[`Black`, 		B],
				[`Red`,		R],
				[`Green`, 		G]
			],
		tab(25), write(`Choose from: `), nl,
		pp_style('Simple', Header, Body, Footer),
		pretty_print([`Mana Colour`,`Amount`],Mana, 25, Header,Body,Footer),
		tab(25),
		write(`Or enter [c]ancel to stop casting this spell`), nl,
		tab(25),
		write(`(this will return you to the main player actions menu)`), nl,
			tab(35),
			write(`* * *`), nl.


%%%%%%%%%%%%%%output/2 (21.1) 05/03/11
% 21.* clauses: combat output.

/*
	Switches: [S]witches (eg ['[A]ir Servant'])
	Identified: [Name - Switch, Id]
*/
	output(declare_attackers, ['prompt', _Controller, _Switches, []]):-
		tab(25), write(`No attackers to declare`), nl,
			tab(35),
			write(`* * *`), nl.

	% I noticed pretty_print stops a row when it finds an empty state
	%  though if it finds the next row with a non-empty state, it prints it OK
	output(declare_attackers, ['prompt', Controller, Switches, Identified]):-
		Switches =  [Cancel | [Ok | Creatures]],
		tab(25), write(`Choose any number of creatues to attack your opponent`), nl,
		tab(25), write(`(enter [s]witches as a string; no quotes needed)`), nl,
		output_combatants(Controller, Creatures, Identified),
		tab(25),
		write(`Enter `), write(Ok),
		write(` when you're done`), nl,
		tab(25),
		write(`Or enter `), write(Cancel),
		write(` to return to the previous menu`), nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (21.X) 23/03/11

	% No creatures declared as attackers.
	output(declare_attackers, [attacking, []]).

	% Attackers: list of [Name - Id]
	output(declare_attackers, [attacking, Attackers]):-
		tab(25), write(`Attacking creatures: `), nl,
		output_combatants(Attackers),
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (21.2) 05/03/11

	output(one_blocker, ['prompt', Controller, Switches, Identified]):-
		Switches =  [_Cancel | [Ok | Creatures]],
		tab(25), write(`Choose a creatue to block with`), nl,
		output_combatants(Controller, Creatures, Identified),
		tab(25),
		write(`Enter `), write(Ok),
		write(` when you're done`), nl,
		% Can't really cancel when you're being attacked- just mise.
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (21.3) 06/03/11

	% This should take Controller as an argument also, just in case
	%  something breaks and output_combatants reports wrongly.
	output(one_attacker, ['prompt', Controller, Blocker, Switches, Identified]):-
		Switches =  [_Cancel | [Ok | Creatures]],
		Blocker = object(Name - _Id, _State),
		tab(25), write(`Choose an attacker to block with `), write(Name), nl,
		player(Opponent),
		Controller \= Opponent,
		% ^ Make a better way to find a player's opponent, especially with
		%  a view to multiplayer.
		output_combatants(Opponent, Creatures, Identified),
		tab(25),
		write(`Enter `), write(Ok),
		write(` when you're done`), nl,
		% Can't really cancel when you're being attacked- just mise.
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (21.X) 23/03/11

	% No creatures declared as blockers
	output(declare_blockers, [blocking, []]).

	% Blockers: list of [Name - Id]
	output(declare_blockers, [blocking, Blockers]):-
		tab(25), write(`Blocking creatures: `), nl,
		output_combatants(Blockers),
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (21.4) 06/03/11

% 	Blockers: listof [Name - Id] (eg, [Name - 1])
% 	Map: listof Name - Switch (eg, 'Name' - N)
%  	and Switches: [S]witch (eg, [N]ame)

	output(order_blockers, ['prompt', Attacker, Blockers, _Map, Switches]):-
		Switches =  [Ok | Creatures],
		Attacker = Name - Id,
		tab(25),
		write(`Order blockers `), nl,
		tab(25),
		write(`Choose the next blocker to assign `), write(Name - Id), write(`s damage to:`), nl,
		output_combatants(Creatures, Blockers),
		tab(25),
		write(`Enter `), write(Ok),
		write(` to accept the current ordering (topmost is first)`), nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (21.5) 06/03/11

	% OK-ish, though it outputs each blocker in a different line.
	% Better organisation needed.
	output(order_blockers, [ordered, Order]):-
		%Order = [ [ Attacker - _ID, Blockers ] ],
		% eg: [['Maritime Guard' - 1,	['Air Servant' - 2, 'Maritime Guard' - 2] ]]
		findall([Attacker, Id, Blocker, ID],
				( member( [ Attacker-Id, Block], Order),
				length(Block, X), X > 1,
				member(Blocker-ID, Block)
				), Blocks),
		Blocks \= [],
		% eg Blocks = [['Goblin Balloon Brigade',2,'Birds of Paradise',2]]
		%keysort(Blocks, Keyed),
		%[Keys] = Keyed, % This will fail, since Keyed won't be one list, but
		%  revisit this later and see how you can sort stuff.
		tab(25), write(`Order of damage to blockers `), nl,
		pp_style('Simple', Header, Body, Footer),
		pretty_print([`Attacker`, `Id`, `Blockers`, `Id`], Blocks, 25, Header,Body,Footer),
			tab(35),
			write(`* * *`), nl.

	output(order_blockers, [ordered, _Order]).
	% ^ Hopefully, we get here only if there are no blockers
	%  to order or each attacker is being blocked by a single blocker.

%%%%%%%%%%%%%%output/2 (21.6) 06/03/11

	output(order_attackers, ['prompt', Blocker, Attackers, _Map, Switches]):-
		Switches =  [Ok | Creatures],
		Blocker = Name - Id,
		tab(25),write(`Order attackers `), nl,
		tab(25),write(`Choose the next attacker to assign `), write(Name - Id), write(`'s damage to:`), nl,
		output_combatants(Creatures, Attackers),
		tab(25),
		write(`Enter `), write(Ok),
		write(` to accept the current ordering (topmost is first)`), nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (21.10) 06/03/11

	% Better organisation needed.
	output(order_attackers, [ordered, Order]):-
		findall([Blocker, Id, Attacker, ID],
				( member( [ Blocker-Id, Attack ], Order),
				length(Attack, X), X > 1,
				member(Attacker-ID, Attack)
				), Attacks),
		Attacks \= [],
		tab(25), write(`Order of damage to attackers `), nl,
		pp_style('Simple', Header, Body, Footer),
		pretty_print([`Blocker`, `Id`, `Attackers`, `Id`], Attacks, 25, Header,Body,Footer),
			tab(35),
			write(`* * *`), nl.

	output(order_attackers, [ordered, _Order]).

%%%%%%%%%%%%%%output/2 (21.11) 06/03/11

	output(assign_lethal_damage, [prompt, _Controller, Bestower, Recipient, Bestower_P, Recipient_T]):-
		tab(25),
		write(`Assign lethal damage`), nl,
		Bestower =.. [_,BEstower-ID,_],
		Recipient =.. [_,REcipient-Id,_],
		Combatants = [[BEstower, ID, Bestower_P, REcipient, Id, Recipient_T]],
		pp_style('Simple', Header, Body, Footer),
		pretty_print([`Bestower`, `Id`, `Power`, `Recipient`, `Id`, `Toughness`], Combatants, 25, Header,Body,Footer),
			tab(35),
				write(`* * *`), nl.


%%%%%%%%%%%%%%output/2 (21.12) 22/04/11

	output(combat_ends,[clear]):-
		tab(25),
		write(`Removing survivors from combat`), nl,
			tab(35),
			write(`* * *`), nl.


%%%%%%%%%%%%%%output/2 (22.1) 06/03/11
% 22.* clauses: damage output.

	output(deal_damage, [creature, Bestower, Recipient, Damage]):-
		tab(25),
			write(`Damage: `), nl,
%		(Bestower = BEstower-ID ; Bestower = BEstower, ID = '-'),
		(Bestower = BEstower-ID ; (Bestower = BEstower, ID = '-')),
		% ^ Temp hack
		Recipient = REcipient-Id,
		Assignment = [[BEstower, ID, Damage, REcipient, Id]],
		pp_style('Simple', Header, Body, Footer),
		pretty_print([`Source`, `Id`, ` Damage`, ` Dealt to`, `Id`], Assignment, 25, Header,Body,Footer),
			tab(35),
				write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (22.1) 06/03/11

	output(deal_damage, [player, Bestower, Recipient, Damage]):-
		tab(25),
			write(`Damage: `), nl,
%		(Bestower = BEstower-ID ; Bestower = BEstower, ID = '-'),
		(Bestower = BEstower-ID ; (Bestower = BEstower, ID = '-')),
		% ^ Temp hack- bestower should not be just a name-
		% it has to be an object in a zone
		Assignment = [[BEstower, ID, Damage, Recipient]],
		pp_style('Simple', Header, Body, Footer),
		pretty_print([`Source`, `Id`, ` Damage`, `Dealt to`], Assignment, 25, Header,Body,Footer),
			tab(35),
				write(`* * *`), nl.


%%%%%%%%%%%%%%output/2 (22.1) 30/03/11
% 22.* clauses: activate_abilities output.

	output(ability_source, ['warning']):-
		tab(25),
		write(`Wrong input!`),nl,
			tab(35),
			write(`* * *`), nl.

% 	Map: listof Name - Switch (eg, 'Name' - N)
%  	Sources: [S]witched source names (eg, [N]ame)
% 	and Identified: listof [Name - Switch, Id] (eg [Name - N, 1]

	output(ability_source, ['choose', [_Map, [Cancel | Sources], Identified]]):-
		tab(25), write(`Choose the source of an ability you wish to activate`), nl,
		output_permanents(Sources, Identified),
		tab(25),
		write(`Enter `), write(Cancel),
		write(` to return to the previous menu `), nl,
%		tab(25),
%		write(`Or `), write(Other),
%		write(` to activae an ability from another zone than the Battlefield`), nl,
			tab(35),
			write(`* * *`), nl.

	output(choose_ability, [prompt, [_Map, [Cancel | Switches]]]):-
		tab(25), write(`Choose the ability to activate`), nl,
		pp_style('Simple', Header, Body, Footer),
		pretty_print([`Abilities:`, ``], [Switches], 25, Header, Body, Footer),
		% 				    ^ dammit!
		tab(25),
		write(`Enter `), write(Cancel),
		write(` to return to the previous menu `), nl,
%		tab(25),
%		write(`Or `), write(Other),
%		write(` to activate an ability from another zone than the Battlefield`), nl,
			tab(35),
			write(`* * *`), nl.

	output(activate_ability, [ability_activated, Player, Source, Ability, _Zone]):-
		tab(25), write(Player), write(` successfully activates ` ),
		write(Source), write(`'s ability: ` ), nl,
		atom_word_list([Ability], [], [Atom]),
		tab(25), write(Atom), nl,
			tab(35),
			write(`* * *`), nl.


	output(activate_ability, [activation_cancelled, Player, Source, _Zone]):-
		tab(25), write(Player), write(` cancels the activation of ` ), nl,
		tab(25), write(Source), write(`'s abilities`), nl,
			tab(35),
			write(`* * *`), nl.

		output(activate_ability, [activation_fails, Player, Source, Ability, _Zone]):-
		tab(25), write(Player), write(` fails to activate ` ),
		write(Source), write(`'s ability: ` ), nl,
		atom_word_list([Ability], [], Atom),
		tab(25),  write(Atom), nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (23.1) 04/04/11
% 23.* clauses: activate_abilities output.

	output(choose_targets, [prompt, Context]):-
		Context = [Ability, _Map, [Cancel | Switches], Identified],
		tab(25), write(` Choose a target for the ability ` ), nl,
		atom_word_list([Ability], [], Atom),
		tab(25),  write(Atom), nl,
		output_targets(Switches, Identified),
		tab(25),
		write(`Enter `), write(Cancel),
		write(` to return to the previous menu `), nl,
			tab(35),
			write(`* * *`), nl.


%%%%%%%%%%%%%%output/2 (24.1) 05/04/11
% 24.* clauses: discard_to_max output.

	% This is copy-pasted from cast_spell. Bad, bad predicate.
	output(discard_to_max, [prompt, Context]):-
		Context = [Max_hand_size, Cards_in_hand, Map, Switches],
		tab(25), write(`You have ` ), write(Cards_in_hand), write(` cards in hand.`), nl,
		tab(25), write(`Your maximum hand size is `), write(Max_hand_size), nl,
		Discards is Cards_in_hand - Max_hand_size,
		tab(25), write(`Choose `), write(Discards), write(` cards to discard`), nl,
		findall(Info,(member(Card - _Switch, Map, Position),
				member(Switch, Switches, Position),
				card([card_name Card, mana_cost Cost, type_line [_, [Type], _], _, _, _, _, _]),
				Info = [ Switch, Type, Cost ]), Infos),
		pp_style('Simple', Header, Body, Footer),
		pretty_print([`Name`, `Type`, `Mana Cost`],Infos, 25, Header,Body,Footer),
		tab(25),write(`Enter [S]witches as a list- no punctuation needed!`), nl,
			tab(35),
			write(`* * *`), nl.


%%%%%%%%%%%%%%output/2 (25.1) 10/04/11
% 25.* clauses: glee_min taunts.

	output(taunt, [matchup_strategy, []]).

	output(taunt, [matchup_strategy, Matchup]):-
		player(Player),
		Player \= 'Glee-min',
		tab(25),write(`Glee-min says: `), nl,
		tab(25),write(`You're playing `), write(Matchup), write(`?`), nl,
		tab(25),write(`Really, `), write(Player),
			write(`, I expected better of you!`), nl,
			tab(35),
			write(`* * *`), nl.


%%%%%%%%%%%%%%output/2 (26.1) 19/04/11
% 26.* clauses: choose_deck output.

	output(choose_deck, [prompt, [Player, Decks, [_Cancel | Map], [Cancel | Switches]]]):-
		tab(5), write(`Please choose a deck for `), write(Player), nl,
		findall([Switched, Colour, Type],
					(member([Deck, Colour, Type], Decks),
					member(Deck-_Switch, Map, Position),
					member(Switched, Switches, Position)),
				Deck_info),
		pp_style('Simple', Header, Body, Footer),
		pretty_print([`Deck`, `Colour`, `Type`],Deck_info, 5, Header,Body,Footer),
		tab(5),
		write(`Enter `), write(Cancel),
		write(` to return to the previous menu `), nl,
		tab(5), write(`Current deck choices will be kept.`), nl,
			tab(25),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (26.2) 19/04/11

	output(choose_deck, [chosen]):-
		deck('Player 1', Deck),
		deck('Glee-min', DEck),
		tab(5),
		write(`You are playing: `), write(Deck), nl,
		tab(5),
		write(`(but your opponent doesn't know!)`), nl,
		tab(5),
		write(`Gleemin is playing: `), write(DEck), nl,
			tab(25),
			write(`* * *`), nl.


%%%%%%%%%%%%%%output/2 (27.1) 22/04/11
% 27.* clauses: generate_effect output.

	output(generate_effect,[permanent, Name-Id]):-
		tab(25), write(`Resolved effect: created `),
		write(Name-Id), nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (27.2) 22/04/11

	output(generate_effect,[non_permanent, Name]):-
		tab(25), write(`Resolved effect: `),
		write(Name), nl,
			tab(35),
			write(`* * *`), nl.


%%%%%%%%%%%%%%output/2 (28.1) 22/04/11
% 28.* clauses: card_characteristics output.

	output(card_characteristics, [info, Characteristics]):-
		pp_style('Simple', Header, Body, Footer),
		pretty_print([`Characteristic`, `Value`],Characteristics, 25, Header,Body,Footer),
			tab(35),
			write(`* * *`), nl.


%%%%%%%%%%%%%%output/2 (29.1) 22/04/11
% 29.* clauses: read_card output.

	output(read_card, [prompt, [_Map, [Cancel | _Switches]]]):-
		tab(25), write(`Choose a card to view its characteristics`),nl,
		tab(25), write(`Or enter `), write(Cancel),
		write(` to return to the previous menu`), nl,
			tab(35),
			write(`* * *`), nl.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%       Auxiliary Predicates        %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%player_actions_context/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	player_actions_context(Context):-
		(Context == 'land' -> tab(30), write(`"[l]and"`), nl);
		(Context == 'no_land' -> true).

% caller: output/2 (8.X)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%output_stack/4 26/04/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* stack_by_player(+Cards, [], +On_stack) */
	% Prints out the cards on the stack and their controllers.

%called by: output/2 (11.3)

	output_stack(Objects, Map, Switches, Margin):-
		findall(States - Spell,
				(member(Object, Objects),
				object_handle(Object , Name-Id),
				object_controller(Name-Id, Controller),
				object_state(Name-Id, State),
				( phrase(ability, Name) ->  % an ability on the Stack.
				atom_word_list([Name],[],[NAme]);
				% ^ This is also called in inspect_output. Bad bad.
				NAme = Name),
				member(NAme-_Switch, Map, Position),
				member(Switched, Switches, Position),
				flatten_list(State, Flat),
				length(Flat, States),
				% ^ The no. of states in the permanent's State list
				normalise_states(Flat, [], Normal),
				append([Switched, Id, Controller], Normal, Spell)),
			Permanents),
		findall(Datum, member(_Number - Datum, Permanents), Data),
		(keysort(Permanents, Sorted), reverse(Sorted, [S - _A | _Rest]) ; S = 0),
		% ^ the longest State list		there may be no Spells in it  ^
		(S = 0, Length = 1; Length = S), % a State list may be empty
		length(Long, Length), findall(`State`, member(`State`, Long), State_list),
		% ^ generates a list of length S and fills it with "State"
		append([`Name`,`Id`, `Controller`], State_list, Title),
		pp_style('Simple', Header, Body, Footer),
		pretty_print(Title, Data, Margin, Header, Body, Footer).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%output_partition/5 date created? modified: 23/04/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* output_partition(+Partition, +Tab, +Margin) */
	% Prints out each Player's partition of a zone.

	output_partition(Partition, Map, Switches, Tab, Margin):-
		findall(Player, player(Player), Players),
		output_partition(Players, Partition, Map, Switches, Tab, Margin).

% called by: output/2 (11.4)


%%%%%%%%%%%%%%output_partition/6 date created? modified: 23/04/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* output_partition(+Players, +Partition, +Tab, +Margin) */
	% business goal for output_partition/3

%%%%%%%%%%%%%%output_partition/6 (0) 23/04/11

	output_partition([], _Partition, _Map, _Switches, _Tab, _Margin).

%%%%%%%%%%%%%%output_partition/6 (1) 23/04/11

	output_partition([Player | Rest], Partition, Map, Switches, Tab, Margin):-
		zone(Player, Partition, Objects),
		findall(States - Permanent,
				(member(Object, Objects),
				Object = object(Name - Id, State),
				member(Name-Switch, Map, Position),
				member(Switched, Switches, Position),
				flatten_list(State, Flat),
				length(Flat, States),
				% ^ The no. of states in the permanent's State list
				normalise_states(Flat, [], Normal),
				append([Switched, Id], Normal, Permanent)),
			Permanents),
		findall(Datum, member(_Number - Datum, Permanents), Data),
		length(Permanents, PErmanents),
%		tab(Margin), write(Player), write(`'s side(`), write(PErmanents),write(`)`), nl,
		tab(Margin), write(Player), write(`'s side, `), write(PErmanents),write(` objects`), nl,
		(keysort(Permanents, Sorted), reverse(Sorted, [S - _A | _Rest]) ; S = 0),
		% ^ the longest State list		there may be no Permanents in it  ^
		(S = 0, Length = 1; Length = S), % a State list may be empty
		length(Long, Length), findall(`State`, member(`State`, Long), State_list),
		% ^ generates a list of length S and fills it with "State"
		append([`Name`,`Id`], State_list, Title),
		pp_style('Simple', Header, Body, Footer),
		pretty_print(Title, Data, Margin, Header, Body, Footer),
			tab(Tab),
			write(`* * *`), nl,
		output_partition(Rest, Partition, Map, Switches, Tab, Margin).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%output_mana_pools/1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* output_mana_pools(+Players) */
	% Prints out each player's mana pool

%%%%%%%%%%%%%%output_mana_pools/1 (0)

	output_mana_pools([]).

%%%%%%%%%%%%%%output_mana_pools/1 (1)

	output_mana_pools([Player | Rest]):-
		mana_pool(Player, Mana_pool),
		Mana_pool = [c - C,w - W,u - U,b - B,r - R,g - G],
		Mana = [
				[`Colourless`, 	C],
				[`White`, 		W],
				[`Blue`,		U],
				[`Black`, 		B],
				[`Red`,		R],
				[`Green`, 		G]
			],
		tab(25), write(Player), write(`'s Mana pool`), nl,
		pp_style('Simple', Header, Body, Footer),
		pretty_print([`Mana Colour`,`Amount`],Mana, 25, Header,Body,Footer),
		tab(35),
			write(`* * *`), nl,
		output_mana_pools(Rest).

%called by output/2 (11.6)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%mana_colours/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* mana_colours(+Map, +Land, -Colours) */
	% Finds the colours a mana source can produce

	mana_colours(Map, Land, Colours):-
		findall( [Switch, Colour] ,
				(member(Land_card - _Switch, Map, Position),
				member(Switch, Land, Position),
				mana_ability(Land_card, _Ability, Colour)
			), Colours).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%mana_colours/4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* mana_colours(+Map, +Land, +Identified, -Colours) */
	% Reports the colours a mana source can produce and its Id
	%  and state

% Land: list of [S]witched land names
% Map: list of Name - Switch
% Identified: list of [Name - Switch, Id]

/*	mana_colours(Map, Land, Identified, Colours):-
		zone('Battlefield', Permanents),
		findall( [Switched, Id, Colour, State] ,
				(member(Land_card - Switch, Map, Position),
				member(Switched, Land, Position),
				member([Land_card - Switch, Id], Identified),
				member(object(Land_card - Id, State), Permanents),
				mana_ability(Land_card, _Ability, Colour)
			), Colours). */

% Tested, OK, not called.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%output_sources/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* output_sources(+Controller, +Permanents, +Identified) */
	% Prints out permanents on the Battlefield, [S]witched for
	%  player input.

	% Identified: list of [Name - Switch, Id]
	% Permanents: list of [S]witched permanent names
	output_sources(Controller, Permanents, Identified):-
		zone(Controller, 'Battlefield', PErmanents),
		findall( States - Permanent,
		% ^ States: number of states in the Combatant's State list
				(member(Switched, Permanents, Position),
				member([Name - _Switch, Id], Identified, Position),
				member(object(Name-Id,State), PErmanents),
				mana_ability(Name, _Ability, Mana),
				flatten_list(State, Flat),
				length(Flat, States),
				normalise_states(Flat, [], Normal),
				append([Switched, Id, Mana], Normal, Permanent)),
			PERmanents),
		(keysort(PERmanents, Sorted), reverse(Sorted, [S - _P | _Rest]) ; S = 0),
		% ^ S = number of most states a combatant has
		(S = 0, Length = 1; Length = S),
		length(Long, Length), findall(`State`, member(`State`, Long), State_list),
		% ^ creates a list of `State`, of length Length, where Length is the number
		%  of individual states in a permanent's State list
		append([`Land`, `Id`, `Mana`], State_list, Title),
		findall(Datum, member(_number - Datum, PERmanents), Data),
		pp_style('Simple', Header, Body, Footer),
		pretty_print(Title, Data, 25, Header,Body,Footer).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%output_permanents/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* output_permanents(+Permanents, +Identified) */
	% Prints out permanents on the Battlefield, [S]witched for
	%  player input.

	% Identified: list of [Name - Switch, Id]
	% Permanents: list of [S]witched permanent names
	output_permanents(Permanents, Identified):-
		findall( States - Permanent,
		% ^ States: number of states in the Permanent's State list
				(member(Switched, Permanents, Position),
				member([Name - _Switch, Id], Identified, Position),
				object_state(Name-Id, State),
				flatten_list(State, Flat),
				length(Flat, States),
				normalise_states(Flat, [], Normal),
				append([Switched, Id], Normal, Permanent)
			), PERmanents),
		(keysort(PERmanents, Sorted), reverse(Sorted, [S - _A | _Rest]) ; S = 0),
		% ^ S = number of most states a permanent has
		(S = 0, Length = 1; Length = S),
		length(Long, Length), findall(`State`, member(`State`, Long), State_list),
		% ^ creates a list of `State`, of length Length, where Length is the number
		%  of individual states in a permanent's State list
		append([`Permanent`, `Id`], State_list, Title),
		findall(Datum, member(_number - Datum, PERmanents), Data),
		pp_style('Simple', Header, Body, Footer),
		pretty_print(Title, Data, 25, Header,Body,Footer).

% Updated to use new Object manipulation predicates
% Should eventually replace all the output_<type> predicates that do the same


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%output_combatants/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* output_combatants(+Controller, +Creatures, +Identified) */
	% Prints out creatures engaged in combat, [S]witched for
	%  player input.

	% Identified: list of [Name - Switch, Id]
	% Creatures: list of [S]witched creature names
	output_combatants(Controller, Creatures, Identified):-
		findall( States - Combatant,
		% ^ States: number of states in the Combatant's State list
				(member(Switched, Creatures, Position),
				member([Name - _Switch, Id], Identified, Position),
				creature(Name-Id, State, _, _, _, P, T, Controller),
				flatten_list(State, Flat),
				length(Flat, States),
				normalise_states(Flat, [], Normal),
				append([Switched, P, T], Normal, Combatant)),
			Combatants),
		(keysort(Combatants, Sorted), reverse(Sorted, [S - _A | _Rest]) ; S = 0),
		% ^ S = number of most states a combatant has
		(S = 0, Length = 1; Length = S),
		length(Long, Length), findall(`State`, member(`State`, Long), State_list),
		% ^ creates a list of `State`, of length Length, where Length is the number
		%  of individual states in a creature's State list
		append([`Creature`, `Power`, `Toughness`], State_list, Title),
		findall(Datum, member(_number - Datum, Combatants), Data),
		pp_style('Simple', Header, Body, Footer),
		pretty_print(Title, Data, 25, Header,Body,Footer).

%called by: output/2 (21.1) (and more)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%output_combatants/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* output_combatants(+Creatures, +Combatants) */
	% Prints out creatures engaged in combat, [S]witched for
	%  player input.

	% Combatants: list of [Name - Id]
	% Creatures: list of [S]witched creature names
	output_combatants(Creatures, Combatants):-
		findall( States - Combatant,
				(member(Switched, Creatures, Position),
				member([Name - Id], Combatants, Position),
				creature(Name-Id, State, _, _, _, P, T, _),
				flatten_list(State, Flat),
				length(Flat, States),
				normalise_states(Flat, [], Normal),
				append([Switched, P, T], Normal, Combatant)),
			COmbatants),
		(keysort(COmbatants, Sorted), reverse(Sorted, [S - _A | _Rest]) ; S = 0),
		(S = 0, Length = 1; Length = S),
		length(Long, Length), findall(`State`, member(`State`, Long), State_list),
		append([`Creature`, `Power`, `Toughness`], State_list, Title),
		findall(Datum, member(_number - Datum, COmbatants), Data),
		pp_style('Simple', Header, Body, Footer),
		pretty_print(Title, Data, 25, Header,Body,Footer).

%called by: output/2 (21.4) (and more)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%output_combatants/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* output_combatants(+Combatants) */
	% Prints out creatures engaged in combat

	% Combatants: list of [Name - Id].
	output_combatants(Combatants):-
		findall( States - Combatant,
				(member(Name - Id, Combatants),
				creature(Name-Id, State, _, _, _, P, T, _),
				flatten_list(State, Flat),
				length(Flat, States),
				normalise_states(Flat, [], Normal),
				append([Name, Id, P, T], Normal, Combatant)),
			COmbatants),
		(keysort(COmbatants, Sorted), reverse(Sorted, [S - _A | _Rest]) ; S = 0),
		(S = 0, Length = 1; Length = S),
		length(Long, Length), findall(`State`, member(`State`, Long), State_list),
		append([`Creature`, `Id`, `Power`, `Toughness`], State_list, Title),
		findall(Datum, member(_number - Datum, COmbatants), Data),
		pp_style('Simple', Header, Body, Footer),
		pretty_print(Title, Data, 25, Header,Body,Footer).

%called by: output/2 (21.4) (and more)


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%output_targets/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* output_targets(+Targets, +Identified) */
	% Prints out targets, [S]witched for player input.
	% Look here Patsantzis. You really need to stop copy-pasting
	%  code like this. Write a general form of all these
	%  output_stuff predicates, please. Ç'est pas la mêre a boire!

	% Identified: list of [Name - Switch, Id]
	% Targets: list of [S]witched target names
	output_targets(Targets, Identified):-
		findall( States - Target,
		% ^ States: number of states in the Target's State list
				(member(Switched, Targets, Position),
				(member([Name - _Switch, Id], Identified, Position),
					object_state(Name-Id, State),
					flatten_list(State, Flat),
					length(Flat, States),
					normalise_states(Flat, [], Normal),
					append([Switched, Id], Normal, Target);
					member([Name - _SWitch], Identified, Position),
					Target = [Switched, '-', '-'], States = 1)
					% ^ Players and Zones gain an empty state and Id
			), PERmanents),
		(keysort(PERmanents, Sorted), reverse(Sorted, [S - _A | _Rest]) ; S = 0),
		% ^ S = number of most states a permanent has
		(S = 0, Length = 1; Length = S),
		length(Long, Length), findall(`State`, member(`State`, Long), State_list),
		% ^ creates a list of `State`, of length Length, where Length is the number
		%  of individual states in a permanent's State list
		append([`Target`, `Id`], State_list, Title),
		findall(Datum, member(_number - Datum, PERmanents), Data),
		pp_style('Simple', Header, Body, Footer),
		pretty_print(Title, Data, 25, Header,Body,Footer).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%normalise_states/3 26/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* normalise_states(+States, [], -Normalised) */
	% Turns states expressed as tuples to a form more appropriate
	%  for pretty-printing.

%%%%%%%%%%%%%%normalise_states/3 (0) 26/03/11

	normalise_states([], Normal, Normal).

%%%%%%%%%%%%%%normalise_states/3 (1) 26/03/11

	normalise_states([State | States], Temp, Normal):-
		normalise_state(State, Normalised),
		append(Temp, [Normalised], New_temp),
		normalise_states(States, New_temp, Normal).


%%%%%%%%%%%%%%normalise_states/2 26/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* normalise_state(+State, -Normalised) */
	% Normalises one state for normalise_states/3
	% Rather crude- only works with one- or two- argument states. Refine.

%%%%%%%%%%%%%%normalise_state/2 (0) 26/03/11

	% The state is not a tuple
	normalise_state(State, State):-
		\+ type(State, 7).

%%%%%%%%%%%%%%normalise_state/2 (1) 26/03/11

	normalise_state(State, Normalised):-
		type(State, 7),
		State =.. [Functor, Arg],
		(type(Arg, 1) -> number_atom(Arg, ARg); Arg = ARg ),
		% ^ Convert numbers to atoms for the benefit of atom_to_list/2
		%  but it should be able to do that itself...
		( ARg = Name-Id -> % The argument is an object handle.
		List = [Functor, ' ', :, ' ', Name, ' ', -, ' ', Id];
		List = [Functor, ' ', :, ' ', ARg ]),
		atom_to_list(Normalised, List).
		% Normalised =  Functor : Arg.
		% e.g. damage(Amount)

%%%%%%%%%%%%%%normalise_state/2 (2) 26/03/11

	normalise_state(State, Normalised):-
		type(State, 7),
		State =.. [Functor, Arg_1, Arg_2],
		(type(Arg_1, 1) -> number_atom(Arg_1, ARg_1); Arg_1 = ARg_1 ),
		(type(Arg_2, 1) -> number_atom(Arg_2, ARg_2); Arg_2 = ARg_2 ),
		List = [Functor, ' ', :, ' ', ARg_1, ' ', -, ' ', ARg_2 ],
		atom_to_list(Normalised, List).
%		Normalised = Functor : Arg_1 - Arg_2.
		% e.g. blocked_by(Name, Id)





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              Notes                %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

