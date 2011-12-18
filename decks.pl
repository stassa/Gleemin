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
%%%%              Decks                %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic decklist/3.  
:- dynamic deck/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%deck_type/3 19/04/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	deck_type(Name, Colour, Type)
	deck_type('Tutorial Blue', 'Blue','Control').
	deck_type('Tutorial Red', 'Red','Beatdown').
	deck_type('Curacao', 'Blue','Control').
	deck_type('Raspberry', 'Red', 'Beatdown').
	deck_type('Pistachio', 'Green', 'Beatdown').
%	deck_type('Chocolate Parfait', 'Black', 'Control').
	% ^ Includes an uncastable spell; also is red/black
	%  so need to report properly.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%deck/2 09/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* deck(+Player, +Name) */	
	% Default players' decks.

	% deck(Playername, Deckname).
	deck('Player 1', 'Curacao').
	deck('Glee-min', 'Raspberry').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%decklist/2 09/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	decklist('Tutorial Blue', 
			[
				['Island', 14], 
				['Air Servant+', 4],
				['Maritime Guard+', 4],
				['Unsummon', 4],
				['Boomerang', 4]
			]).


	decklist('Tutorial Red', 
			[
				['Mountain', 14], 
				['Goblin Balloon Brigade', 4],
				['Vulshok Berserker+', 4],
				['Lightning Bolt', 4],
				['Prodigal Pyromancer+', 4]
			]).


	decklist('Curacao', 
			[
				['Island', 14], 
				['Air Servant', 4],
				['Maritime Guard', 4],
				['Unsummon', 4],
				['Boomerang', 4]
			]).


	decklist('Raspberry', 
			[
				['Mountain', 20], 
				['Goblin Balloon Brigade', 4],
				['Goblin Piker', 4],
				['Lightning Bolt', 4],
				['Prodigal Pyromancer', 4],
				['Vulshok Berserker', 4]
			]).

	decklist('Pistachio', 
			[
				['Forest', 20], 
				['Giant Growth', 4],
				['Giant Spider', 4],
				['Llanowar Elves', 4],
				['Runeclaw Bear', 4],
				['Spined Wurm', 4]
			]).


	decklist('Chocolate Parfait', 
			[
				['Swamp', 10], 
				['Mountain', 10], 
				['Assassinate', 4],
				['Child of Night', 4],
				['Flensermite', 4],
				['Puncture Blast', 4],
				['Lava Axe', 4]
			]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%sideboard/2 09/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* sideboard(+Player, +Sideboard) */

	% Cards in a player's sideboard
	sideboard('Player 1', ['card a']).
	sideboard('Player 2', ['card b']).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%            Notes                 %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	% testing 
	deck_is(Player, Deck, Cards):-
		deck(Player, Deck),
		decklist(Deck, Cards).
		


