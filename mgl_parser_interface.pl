% First saved: 12/01/11
% Last saved: 14/04/11
%
%	Status: 
%	OK, see notes for proper input
%
% Doing:
%
% Todo
%
%	Read in mana costs as a single word.
%	What to do if there is more than one sentence.
%
% NOTES:
/*
	readlist - read a list of words, adapted from a Clocksin & Mellish
	  by Amzi.com, in : http://www.amzi.com/AdventureInProlog/appendix.php
	
	Rule no. 1: remember that not every Magic ability is recognised by 
	  the interpreter yet. 
	When calling read_MGL remember to input activated abilities' costs as 
	  _not_ separated by commas and always followed by a colon. 
	When you enter single quotes, disregard the limitations of Edinburgh 
	  syntax- just go ahead and enter the word the way you would normally.
	  For example, don't write: 'owner''s'. Do write: owner's.
	Names of cards with one or more spaces in them should be concatenated
	  using the "&" symbol, for example: Goblin&Balloon&Brigade.
	Enter each element of a +X/+X etc counters or gets ability separated by
	  spaces : "+ 1 / + 1" , not "+1/+1". The interpreter splits up each 
	  modifier in its components in order to verify that it's indeed a 
	  modifier. The result is rather clunky input- I'll have to fix that. 
	  Eventually. 
	When entering triggered abilities, separate the condition from the effect
	  with a comma, for example: 
		When Aether&Adept enters the Battlefield, 
			return target Creature to its owner's hand.
	  This will produce a rather ugly output, like: 
		[When,Aether Adept,enters,the,Battlefield,(,),return,
			target,Creature,to,its,owner's,hand]
	  And by "ugly" I do mean that weird "(,)" thing (no, it's not a smilie
	  of a merry elephant walking away).
	  I'm working on it. 
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
%%%%               Facts               %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%single_char/1 12/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	single_char(0',).
	single_char(0';).
	single_char(0':).
	single_char(0'?).
	single_char(0'!).
	single_char(0'.).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lastword/1 12/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% These symbols delineate end of sentence

	% end if new line entered
	lastword(10).   
	lastword(0'.).
	lastword(0'!).
	lastword(0'?).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              Rules                %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% :- ensure_loaded('MGL_interpreter.pl').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%identify_MGL/1 14/04/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% identifies the ability type of an input MGL term. 
	
	identify_MGL:- 
		readlist(MGL_term), 
		( ( ( phrase(keyword_ability, MGL_term) -> 	Type = 'keyword ability' ) ; 
		( phrase(spell_ability, MGL_term) -> 	Type = 'spell ability' ) ; 
		( phrase(mana_ability, MGL_term) -> 	Type = 'mana ability' ) ; 
		( phrase(activated_ability, MGL_term) -> 	Type = 'activated ability' ) ; 
		( phrase(triggered_ability, MGL_term) -> 	Type = 'triggered ability' ) ; 
		( phrase(static_ability, MGL_term) -> 	Type = 'static ability' ) ; 
		( phrase(ability_word, MGL_term) -> 	Type = 'ability word' ) ), 
		write(MGL_term), write(` is an MGL `), write(Type), nl; 
		write(MGL_term: `Input is not recognised as correct MGL!`), nl ). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%generate_MGL/2 14/04/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* generate_MGL(+MGL_term_type, -New_MGL_term) */
	% Generates successive MGL terms of a specified type.

	generate_MGL(MGL_term_type, New_term):- 
		phrase(MGL_term_type, Term), 
		atom_word_list([Term], [], [New_term]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%read_MGL/1 14/04/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* read_MGL(+MGL_term) */
	% Reads in and verifies the syntax of an MGL term.
	% Converts the input to a word-list for the MGL parser. 

	read_MGL(MGL_term):-
		 readlist(Word_list), 
		( phrase(MGL_term, Word_list), 
		write(Word_list: `Input is an MGL `), write(MGL_term), write(`.`), nl 
		 ;
		write(Word_list: `Input is not recognised as a correct MGL `),
		write(MGL_term), write(`!`), nl ). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%readlist/1 12/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* readlist(+Word_list) */

	readlist(Word_list):-
		write('> '),
		read_word_list(Word_list).


%%%%%%%%%%%%%%read_word_list/1 12/01/11
%%%%%%%%%%%%%%%%%%%%%%%
	/*  read_word_list([ Word | Rest_of_sentence ]) */

	read_word_list([ Word | Rest_of_sentence ]) :-
		get0(Character),
		readword(Character, Word, Next_character),      
		restsent(Next_character, Rest_of_sentence), !.      
	 % Read word starting with Character, Next_character 
	%  is first new character - use it to get rest of sentence


%%%%%%%%%%%%%%restsent/2 12/01/11
%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%restsent/2 (1) 12/01/11

	restsent(Character,[]):- 
		lastword(Character), !. 
	% Nothing left if we hit the last-word marker

%%%%%%%%%%%%%%restsent/2 (2) 12/01/11

	restsent(Character, [ Word | Rest_of_sentence ]) :-
		readword(Character, Word, Next_character),       
		restsent(Next_character, Rest_of_sentence).
	 % Else read next word and rest of sentence


%%%%%%%%%%%%%%readword/3 12/01/11
%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%readword/3 (1) 12/01/11

	% Some words are single characters
	%  i.e. punctuation
	readword(Character, Word, Next_character):-        
		single_char(Character),           
		!, 
		name(Word , [Character]),             
		get0(Next_character).
		% get as an atom

%%%%%%%%%%%%%%readword/3 (2) 12/01/11

	% if we have a number --
	% convert it to a genuine number
	readword(Character, Word, Next_character) :-
	  is_num(Character),                
	  !,
	  number_word(Character, Word, Next_character, _Power_of_10). 

%%%%%%%%%%%%%%readword/3 (3) 12/01/11

	% else if character does not delineate end of word - keep
	% accumulating them until we have all the word     
	% then make it an atom
	readword(Character, Word, Next_character) :-
		in_word(Character, New_character),
		get0(Character_next),        
		restword(Character_next, Remaining_characters, Next_character),
		name(Word, [ New_character | Remaining_characters ]).

%%%%%%%%%%%%%%readword/3 (3) 12/01/11

	% otherwise start a new word
	readword(_Character, Word, Next_character) :-         
		get0(Character_next),       
		readword(Character_next, Word, Next_character).        


%%%%%%%%%%%%%%restword_list/3 12/01/11
%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%restword_list/3 (1) 12/01/11

	restword(Character, [ New_character | Remaining_characters ], Next_character):-
		in_word(Character, New_character),
		get0(Character_next),
		restword(Character_next, Remaining_characters, Next_character).

%%%%%%%%%%%%%%restword_list/3 (2) 12/01/11

	restword(Character, [], Character).


%%%%%%%%%%%%%%in_word/2 12/01/11
%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%in_word/2 (0) 12/01/11

	%in_word(C, C):- is_num(C); 
	%	(C == 0'w; C == 0'u; C == 0'b; C == 0'r; C == 0'g).
	%	^ The word is a mana cost??

%%%%%%%%%%%%%%in_word/2 (1) 12/01/11

	% P/T counters form a single word each.
	in_word(Character, Character):- 
		Character == 0'+ ; 
		Character == 0'- ; 
		Character == 0'/ ; 
		is_num(Character).

%%%%%%%%%%%%%%in_word/2 (2) 12/01/11

	% Lowercase letters
	in_word(Character, Character):- 
		Character >= 0'a, 
		Character =< 0'z.

%%%%%%%%%%%%%%in_word/2 (3) 12/01/11

	% Upper case letters are kept in upper case
	in_word(Character, Character):- 
		Character >= 0'A, 
		Character =< 0'Z.

%%%%%%%%%%%%%%in_word/2 (4) 12/01/11

	in_word(0'',0'').
	% ^ When entering single quotes in a word, 
	%  ignore the Edinburgh syntax; for example: 
	%  "owner's" will parse to 'owner''s'

%%%%%%%%%%%%%%in_word/2 (5) 12/01/11

	in_word(0'-,0'-).
	% Have character C (known integer) - keep reading integers and build
	% up the number until we hit a non-integer. Return this in C1,
	% and return the computed number in W.

%%%%%%%%%%%%%%in_word/2 (6) 14/04/11

	% Use "&" to concatenate words as an atom. 
	%  eg: Lightning&Bolt = ['Lightning Bolt']
	in_word(0'&, 32).
	% ^ In practice, replaces "&" with a space.


%%%%%%%%%%%%%%number_word/4 12/01/11
%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%number_word/4 (1) 12/01/11

	number_word(Character, Word, Next_character, Pow10) :- 
	  is_num(Character),
	  !,
	  get0(Character_next),
	  number_word(Character_next, Word_1, Next_character, P10),
	  Pow10 is round(P10 * 10),	% Swi, 25/06/11
	  Word is (((Character - 0'0) * Pow10) + Word_1).

%%%%%%%%%%%%%%number_word/4 (2) 12/01/11

	number_word(Character, 0, Character, 0.1).


%%%%%%%%%%%%%%is_num/1 12/01/11
%%%%%%%%%%%%%%%%%%%%%%%

	is_num(Character) :-
	  Character =< 0'9,
	  Character >= 0'0.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              Notes                %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


		% ^ This word is a counter of the form S1 Num1/S2 Num 2
		%  where S1, S2 == (+ ; -).

