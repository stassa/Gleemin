% First saved: 15/02/2011
% Last saved:  19/04/2011
%
%	Stable step but I don't know what changed since the last; just in case.
%
% 	Status:
%	OK
%
% Doing:
%	Fixed concede error (turn_sequence lacked boundary condition).
% Todo:
%
% NOTES:
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%         Swi Compatibility         %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	:- set_prolog_flag(backquoted_string,true).
	% Swi LPA backtic string compatibility.
	:- style_check(-singleton).
	% Stop Swi warning on singleton vars.
	:- style_check(-discontiguous).
	% Stop Swi warning on discontiguous clauses.
        :- dynamic new_run/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%           Main Game Loop          %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%main/1 16/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%main/1 (1) 16/12/2010

	main:-
		new_run(yes),
		initialise,
		retractall(new_run(yes)),
		welcome,
		fail.

%%%%%%%%%%%%%%main/1 (2) 16/12/2010

	main:-
		input(main, _, Input) ->
		do(Input);
		main.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%new_run/1 18/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	new_run(yes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%source_files/X 18/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	source_files([
				'swi_compatibility_layer3.pl',
				'mtg_operators.pl',
				'mgl_parser_interface.pl',
				'decks.pl',
				'glee_min.pl',
				'matrices.pl',
				'players.pl',
				'predicates.pl',
				'pretty_printer.pl',
				'cards.pl',
				'creature_combat.pl',
				'input_output.pl',
		%		'MGL_interpreter.pl',
				'mgl_interpreter.pl',
				'player_actions.pl',
				'turn_sequence.pl',
				'zones.pl'
			]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%initialise/0 18/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	initialise:-
		source_files(Files),
		load(Files),
		declare_dynamic_facts,
		write(`Initialisation OK`), nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%load/1 18/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%load/1 (1) 18/12/2010

	% Load source files and output their names.
	load(Files):-
		member(File, Files),
%		consult(File), %Swi, 19/06/11
		load_files(Files, [silent(true)]),
		write(File),
		write(` loaded`), nl,
		fail.

%%%%%%%%%%%%%%load/1 (2) 18/12/2010

	% No more source files to load.
	load(_):-
		write(`Loading files OK`), nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%declare_dynamic_facts/0 18/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	declare_dynamic_facts:-
		dynamic(new_run/1),
		write(`Declaring dynamic facts OK`), nl.


%%%%%%%%%%%%%%welcome/1 16/12/2010
%%%%%%%%%%%%%%%%%%%%%%%

	welcome:-
		nl,
		write(`Welcome to Gleemin, the Magic Virtual Machine.`).


%%%%%%%%%%%%%%do/1
%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%do/1 (1) 19/04/2010

	% d: deck
	do(100):-
		choose_decks,
		main.


%%%%%%%%%%%%%%do/1 (1) 16/12/2010

	%s: start
	do(115):-
		reset_game,
		populate_libraries,
		shuffle_libraries,
		draw_starting_hand,
%		turn('Player 1', 'Beginning', begins('Untap'), [], [], [], []),
		order_of_play([ First | _Next ]),
		turn(First, 'Beginning', begins('Untap'), [], [], [], []),
		main.

%%%%%%%%%%%%%%do/1 (2) 16/12/2010

	% h: help
	do(104):-
		output(main, ['help']),
		main.

%%%%%%%%%%%%%%d/1 (3) 16/12/2010

	% q: quit
	do(113):-
		goodbye.



%%%%%%%%%%%%%%goodbye/1 16/12/2010
%%%%%%%%%%%%%%%%%%%%%%%

	goodbye:-
		nl,
		write(`Thank you for playing with Gleemin.`),
		assert(new_run(yes)), nl.


%%%%%%%%%%%%%%reset_game/0	24/01/11
%%%%%%%%%%%%%%%%%%%%%%%
	% clear facts from the kb and assert that it's a new game.

/*	reset_game:-
		clear_kb,
		retractall(new_game(_)),
		assert(new_game(yes)).
		% ^ The first turn in a new game needs to know
		%  that this is a new game.
*/

	% Swi, 18/06/11
	reset_game:-
		clear_kb,
		source_files(Files),
		load_files(Files, [silent(true)]),
		assert(new_game(yes)).


%%%%%%%%%%%%%%populate_libraries/0	24/01/11
%%%%%%%%%%%%%%%%%%%%%%%
	% turn the players' decks to their libraries.

%%%%%%%%%%%%%%populate_libraries/0 (1) 24/01/11

	populate_libraries:-
		player(Player),
		deck_to_library(Player, Deck, Library),
		fail.

%%%%%%%%%%%%%%populate_libraries/0 (2) 24/01/11

	populate_libraries:-
		true.


%%%%%%%%%%%%%%shuffle_libraries/0	24/01/11
%%%%%%%%%%%%%%%%%%%%%%%
	% shuffle players' libraries

%%%%%%%%%%%%%%shuffle_libraries/0 (1) 24/01/11

	shuffle_libraries:-
		player(Player),
		shuffle(Player, Shuffled),
		fail.

%%%%%%%%%%%%%%shuffle_libraries/0 (2) 24/01/11

	shuffle_libraries:-
		true.


%%%%%%%%%%%%%%draw_starting_hand/0	24/01/11
%%%%%%%%%%%%%%%%%%%%%%%
	% draw a starting hand (six cards) for all players
	% A little more work needed here. Simply failing just draws
	%  the whole library for all players.

	draw_starting_hand:-
		order_of_play(Players),
		draw_six(Players).

%%%%%%%%%%%%%%draw_six/1 24/01/11

	% Boundary condition... no more players to draw six cards for.
	draw_six([]).

%%%%%%%%%%%%%%draw_six/1 24/01/11

	% draw six cards and proceed with the remaining players.
	draw_six(Players):-
		member(Player, Players), !,
		draw_cards(Player, 7),
		remove(Player, Players, Rest), !,
		draw_six(Rest).


%%%%%%%%%%%%%%clear_kb/0	23/01/11
%%%%%%%%%%%%%%%%%%%%%%%
	% clear kb of previous game facts.
	% This causes

%%%%%%%%%%%%%%clear_kb/0 (1) 24/01/11

/*	clear_kb:-
		source_files(Files),
		member(File, Files),
		\+ File = 'decks.pl',
		% ^ Added 19/04- don't change
		%  chosen deck
		consult(File),
		fail.
*/

	% Swi, 18/06/11
	clear_kb:-
		predicate_property(X, dynamic),
		\+ X = deck(_,_), % don't remove deck choices!
		\+ predicate_property(X,built_in),
		retractall(X),
		fail.

%%%%%%%%%%%%%%clear_kb/0 (2) 24/01/11

	clear_kb.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%choose_decks/0 19/04/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% Lets the human player choose decks for him or herself
	%  and the AI player

	choose_decks:-
		output(choose_deck, [chosen]),
		findall(Player, player(Player), Players),
		choose_deck(Players),
		output(choose_deck, [chosen]).

	choose_decks.

%%%%%%%%%%%%%%choose_deck/1 19/04/11
%%%%%%%%%%%%%%%%%%%%%%%
	/*choose_deck(Players)*/
	% Choose a deck for each player

%%%%%%%%%%%%%%choose_deck/1 (0) 19/04/11

	choose_deck([]).

%%%%%%%%%%%%%%choose_deck/1 (1) 19/04/11

	choose_deck([Player| Players]):-
		findall([Deck, Colour, Type],
			(decklist(Deck, _Cardlist),
			deck_type(Deck, Colour, Type) ),
			Decks),
		findall(Name, member([Name, _C, _T], Decks), Names),
		append([cancel], Names, Full),
		prompt_switches(Full, [Map, Switches]),
		choose_deck(Player, Decks, Map, Switches, Switch),
		( \+ member('cancel' - Switch, Map) ->
		choose_deck(Players); true).


%%%%%%%%%%%%%%choose_deck/4 19/04/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* choose_deck(+Player, +Decks, +Map, +Switches) */
	% Input prompt for choose_deck/1

	choose_deck(Player, Decks, Map, Switches, Switch):-
		Context = [Player, Decks, Map, Switches],
		input(choose_deck, Context, Input) ->
		atom_codes(Switch, [Input]),
		choose_deck(Player, Map, Switch);
		choose_deck(Player, Decks ,Map, Switches, Switch).


%%%%%%%%%%%%%%choose_deck/3 19/04/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* choose_deck(+Player, +Map, +Swtich) */
	% Handles input from choose_deck/4

%%%%%%%%%%%%%%choose_deck/3 (0) 19/04/11

	choose_deck(_Player, Map, Switch):-
		member('cancel' - Switch, Map).

%%%%%%%%%%%%%%choose_deck/3 (1) 19/04/11

	choose_deck(Player, Map, Switch):-
		member(Deck-Switch, Map),
		retractall( deck(Player, _Deck) ),
		asserta( deck(Player, Deck) ).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              Notes                %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%












