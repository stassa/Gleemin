/*
+-Contents-+
This file defines a grammar and auxiliary predicates for parsing and
managing mana in the rules engine.

Mana turns out to be a complex entity. The different classes of grammar
rule represent:

1) "mana_colour": the five colours of mana.
2) "mana_type": the six types of mana (five colours and colourless).
3) "<x>_mana_symbol": mana symbols, used to represent mana on cards.
4) "mana_cost": the syntax of mana costs, derived empirically by myself.

These rules correspond to sections "105. Colors", "106. Mana" and "107.
Numbers and Symbols" in the Comprehensive Rules (2012).

+-Mana costs structure-+
"mana_cost" rules define a structure: the order by which mana symbols
are expected to be encountered within a string of mana symbols
representing a mana cost, depending on their type (for example, generic
mana symbols precede coloured mana symbols). This order is not
explicitly listed in the Comprehensive rules and so it is necessarily
derived from my empirical knowledge of the game. A certain precedence
certainly seems to be kept on the printed cards and may or may not have
changed over time (I am not in a position to verify this). Legend has it
that there exists another rules document for the eyes of WOTC R&D only,
but those who have seen it never speak of it.

+-Generic is not Colourless-+
"Generic" and "colorless" are not the same thing. One refers to mana in
costs and the other to a type of mana.

From the Glossary:

[4928] Generic Mana
[4929] Mana in a cost not represented by colored mana symbols; it can be
paid with mana of any type. See rule 107.4.

[4611] Colorless
[4612] 1. An object with no color is colorless. Colorless is not a
color. See rule 105, "Colors," and rule 202, "Mana Cost and Color."
[4613] 2. A type of mana. See rule 106, "Mana," and rule 107.4c.

+-Version-+
This is version 2.0 of Gleemin's mana grammar. The original version
consisted of only 7 rules, defining five symbols for coloured mana,
and numerals. This one is rather more comprehensive and also defines
phyrexian mana, hybrid mana and mana variables. The textual symbols used
are now the same as those that are used in the CompRules plain-text
documents as stand-ins for the icons on the cards (eg, {R} is the red
mana sybmol etc). Symbols are also represented as Prolog strings, ie
lists of ASCII character codes. These changes make the representation of
mana in Gleemin fully compatible with card text downloaded from the
Gatherer or any other official source, and should allow its accurate
parsing by the rules engine without any further processing (as was
necessary before).

+-Generating mana costs-+
This may not work as expected. There are two things to keep in
mind. First, because each component of a mana cost may be present zero
or more times, rules have declarations like: "([] ; mana_numerals)", in
the generic_mana_cost definition. This will cause the first generated
generic_mana_cost string to contain [], so the first useful mana cost
output will be a coloured mana cost:

?- phrase(mana_cost, C).
C = [] ;
C = [] ;
C = ['{W}'] ;

The second thing to remember is that Prolog builds a list of tokens
incrementally, adding a new one at the _head_ of the list every time.
Since it starts building at the first terminal in the lexicon, which
in the case of mana costs happens to be the white mana symbol, the end
result is that generated mana costs will consist of a long string of
"{W}"'s, followed by strings of other mana symbols. You can observe
this with:

?- phrase(mana_cost, C).

A dedicated MGL expression generator would rectify this, but one is not
in the scope of this source file, therefore this discussion ends here.
Just be aware that you _can_ use the rules here to generate mana costs,
but they won't be very useful.

+-Other rules-+
Rules for mana abilities and all the predicates for interacting with
mana (drawing it from and adding it to mana pools etc) are also included
in this source file, which should eventually become a module- when a
standard for Prolog modules is established.

+-The Spellings of Colo/ur-+
Terminals use the US spelling ("color") as is used on the cards and in
rules documents since this will make parsing painless. The British
spelling ("colour") is used for non-terminals where it is highly
unlikely to cause any trouble (well, highly unlikely for a British
thing anyway). Personally, I prefer the Greek spelling: "chroma", with a
long "o" (omega): ????a. Isn't it simply fabulous to learn a new thing
every day?
*/

% Mana colours
% 106.1a There are five colors of mana: white, blue, black, red,
%  and green.

        mana_colour --> (['White'] ; [white]).
        mana_colour --> (['Blue'] ; [blue]).
        mana_colour --> (['Black'] ; [black]).
	mana_colour --> (['Red'] ; [red]).
        mana_colour --> (['Green'] ; [green]).


% Mana types
% 106.1b There are six types of mana: white, blue, black, red, green,
%   and colourless.

        mana_type --> (['White'] ; [white]).
        mana_type --> (['Blue'] ; [blue]).
        mana_type --> (['Black'] ; [black]).
	mana_type --> (['Red'] ; [red]).
        mana_type --> (['Green'] ; [green]).
        mana_type --> (['Colorless'] ; [colorless]).

% Symbols and costs
% 106.2. Mana is represented by mana symbols (see rule 107.4). Mana
%  symbols also represent mana costs (see rule 202).

% Mana costs.
	mana_cost --> [].
        mana_cost --> generic_mana_cost, coloured_mana_cost.

        generic_mana_cost -->
	    ([] ; mana_numerals),
	    ([] ; variable_mana_symbols),
	    ([] ; snow_mana_symbol),
	    ([] ; phyrexian_symbol).
        coloured_mana_cost -->
	    ([] ; phyrexian_mana_symbols),
	    ([] ; monocoloured_hybrid_mana_symbols),
	    ([] ; hybrid_mana_symbols),
	    ([] ; primary_colour_mana_symbols).

        mana_numerals --> mana_numeral.
        mana_numerals --> mana_numeral, mana_numerals.

        variable_mana_symbols -->
	   variable_mana_symbol.
        variable_mana_symbols -->
	    variable_mana_symbol,
	    variable_mana_symbols.

        phyrexian_mana_symbols -->
	    phyrexian_mana_symbol.
        phyrexian_mana_symbols -->
	    phyrexian_mana_symbol,
	    phyrexian_mana_symbols.

        monocoloured_hybrid_mana_symbols -->
	    monocoloured_hybrid_mana_symbol.
        monocoloured_hybrid_mana_symbols -->
	    monocoloured_hybrid_mana_symbol,
	    monocoloured_hybrid_mana_symbols.

        hybrid_mana_symbols -->
	    hybrid_mana_symbol.
        hybrid_mana_symbols -->
	    hybrid_mana_symbol,
	    hybrid_mana_symbols.

        primary_colour_mana_symbols -->
	    primary_colour_mana_symbol.
        primary_colour_mana_symbols -->
	    primary_colour_mana_symbol,
	    primary_colour_mana_symbols.


% Mana symbols
	%Note: mana cost can be [], but mana can't.
	mana_symbol --> colourless_mana_symbol.
        mana_symbol --> coloured_mana_symbol.


        colourless_mana_symbol --> mana_numeral.
        colourless_mana_symbol --> variable_mana_symbol.
        colourless_mana_symbol --> phyrexian_symbol.
        colourless_mana_symbol --> snow_mana_symbol.

        coloured_mana_symbol --> phyrexian_mana_symbol.
        coloured_mana_symbol --> hybrid_mana_symbol.
        coloured_mana_symbol --> monocoloured_hybrid_mana_symbol.
        coloured_mana_symbol --> primary_colour_mana_symbol.


% Coloured mana:
	% Primary mana colours
	primary_colour_mana_symbol --> ['{W}'].
	primary_colour_mana_symbol --> ['{U}'].
	primary_colour_mana_symbol --> ['{B}'].
	primary_colour_mana_symbol --> ['{R}'].
	primary_colour_mana_symbol --> ['{G}'].

        % Hybrid mana
	hybrid_mana_symbol --> ['{W/U}'].
	hybrid_mana_symbol --> ['{W/B}'].
	hybrid_mana_symbol --> ['{U/B}'].
	hybrid_mana_symbol --> ['{U/R}'].
	hybrid_mana_symbol --> ['{B/R}'].
	hybrid_mana_symbol --> ['{B/G}'].
	hybrid_mana_symbol --> ['{R/G}'].
	hybrid_mana_symbol --> ['{R/W}'].
	hybrid_mana_symbol --> ['{G/W}'].
	hybrid_mana_symbol --> ['{G/U}'].

        % Monocoloured hybrid mana
	monocoloured_hybrid_mana_symbol --> ['{2/W}'].
	monocoloured_hybrid_mana_symbol --> ['{2/U}'].
	monocoloured_hybrid_mana_symbol --> ['{2/B}'].
	monocoloured_hybrid_mana_symbol --> ['{2/R}'].
	monocoloured_hybrid_mana_symbol --> ['{2/G}'].

        % Phyrexian mana
	phyrexian_mana_symbol --> ['{W/P}'].
	phyrexian_mana_symbol --> ['{U/P}'].
	phyrexian_mana_symbol --> ['{B/P}'].
	phyrexian_mana_symbol --> ['{R/P}'].
	phyrexian_mana_symbol --> ['{G/P}'].

% Generic mana:
	% Mana Variables
	variable_mana_symbol --> ['{X}'].
	variable_mana_symbol --> ['{Y}'].

        % Phyrexian mana
        phyrexian_symbol --> ['{P}']. % cf MCR(2011-09-30): 107.4g

        % Snow mana
	snow_mana_symbol --> ['{S}'].

% Numerals
        % The first rule is here to allow some generation of numerical
	%  mana costs. Obviously, it will only generate 0-mana
        %  costs. Expand if necessary, comment if not needed.
       mana_numeral --> ['{0}'], !. % [0]
       % For mana numerals, we need to open up the atom
       %  and get the juicy bit inside. Er. The number code.
       % This takes a bit of doing.
       mana_numeral --> [N],
	    { atom_codes(N, Codes),
	    phrase(numeric_string, Codes)}.

       numeric_string --> "{", numerals, "}".

       numerals --> numeral.
       numerals --> numeral, numerals.

       numeral --> [N],
           {N >= 48, N =< 57,
	   % ^^ Protects from number_codes(Number, [125])
	   %  which throws exception: illegal number. [125] = "}".
	   % Also makes sure we have a number.
	   number_codes(Number, [N]),
	   Number > 0}.


% Mana symbols by colour and type
        mana_symbol(white) --> ['{W}']. %[123,87,125].
        mana_symbol(blue) --> ['{U}'].
        mana_symbol(black) --> ['{B}'].
	mana_symbol(red) --> ['{R}'].
        mana_symbol(green) --> ['{G}'].
        mana_symbol(colorless) --> mana_numeral.
        mana_symbol(colorless) --> variable_mana_symbol.
       % Think of this as "symbols for each colour or type of mana".

/* Usage:
?- phrase(mana_colour, [X]), phrase(mana_symbol(X), [S]).
X = white,
S = '{W}' ;
	*/



% Not sure why I wanted this. Probably to generate diff-list strings.
        % Single-symbol mana string:
        mana(X) --> {phrase(mana, [X])}, [X].
       /*?- phrase(mana('{R}'), [X]).
       X = '{R}'*/
        % Mana symbol string:
        mana(X) --> {phrase(mana, X)}, [X].
        /*?- phrase(mana(['{X}', '{R}']), [X]).
        X = ['{X}', '{R}'] . */

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%players > mana_pool/2 19/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* mana_pool(Player, Mana) */
	% Where Mana == [Colourless, White, Blue, Black, Red, Green]

%" 21-01-12 Corrects: '{X}' back to '{C}', because mana pools do not
%  store generic mana (since there is no such type of mana), and '{X}'
%  stands for generic mana in a cost.

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*

*/

	% Mana Pools
	mana_pool('Player 1', ['{C}'-0, '{W}'-0, '{U}'-0, '{B}'-0, '{R}'-0, '{G}'-0]).
	mana_pool('Glee-min', ['{C}'-0, '{W}'-0, '{U}'-0, '{B}'-0, '{R}'-0, '{G}'-0]).

%%%%%%%%%%%%%%player_actions>mana_cost/3 07/02/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* mana_cost(+Cost) */
	% Checks that a cost is a mana cost.

	mana_cost(Cost):-
		phrase(mana_cost, Cost).


%%%%%%%%%%%%%%players > add_mana/3 21/01/11
%%%%%%%%%%%%%%%%%%%%%%%
%Corrects: time complexity
	/* add_mana(Mana_amount, Mana_pool, New_pool) */
	% Adds an amount of mana to a player's pool.

%%%%%%%%%%%%%%add_mana/3 (2) 04/01/12

	mana_addition([Colourless | Mana], ['{C}'-Amount | Pool], Acc, Temp):-
	    number(Colourless), !, %red cut
	    New_amount is Amount + Colourless,
	    mana_addition(Mana, ['{C}'-New_amount | Pool], Acc, Temp).

        mana_addition([Symbol | Mana], [Symbol-Value | Pool], Acc , Temp):-
	    New_value is Value + 1,
	    mana_addition(Mana, [Symbol-New_value | Pool], Acc, Temp).

        mana_addition(Mana, [Symbol-Value | Pool], Acc, Temp):-
	    diff_append(Acc, [Symbol-Value|Y]-Y, New_acc),
	    mana_addition(Mana, Pool, New_acc , Temp).

        mana_addition([], _, _Pool, ['{C}'-_C,'{W}'-_W,'{U}'-_U,'{B}'-_B,'{R}'-_R,'{G}'-_G]).


/*
To understand the last unification, first try this:
?- mana_addition([a,a,a,a,b,b,b,b,c,c,c,c,c], [a-0,b-0,c-0], X-X, Y).
X = [a-4, b-4, c-5|_G1203],
Y = [a-4, b-4, c-5|_G1203]-_G1203

Since X binds to a diff list you can map it to a normal list as:
?- mana_addition([a,a,a,a,b,b,b,b,c,c,c,c,c], [a-0,b-0,c-0], X-X, Y), X = [a-A, b-B, c-C].
X = [a-4, b-4, c-5],
Y = [a-4, b-4, c-5]-[],
A = B, B = 4,
C = 5 .

So you can do the mapping directly, at the output arg. Which I reckon
saves you an additional operation to unify the output with a cleanup
pattern later.
 */



/*
+-Mana pools as tuples-+
A different take on the mana pool may be to store each colour of mana
in the database as a tuple of:

    mana_pool(Player, Colour-Amount).

So that then adding mana or taking mana out of the pool would be a
simple case of changing the value of Coulour-Amount to a new value.
For changing more than one colour you'd need to go through a list of
mana symbols, ie the mana string, but you would save yourself the pain
of going through the mana pool-list and doing appends or replaces:

    add_mana([], _Player).

    add_mana([Symbol | String], Player):-
	retract( mana_pool(Player, Symbol-Amount) ),
	% ^ is det, so functions as check.
	New_amount is Amount+1,
	assert( mana_pool(Player, Symbol-New_amount) ),
	add_mana(String, Player).

And:

    take_mana([], _Player).

    take_mana([Symbol | String], Player):-
	retract( mana_pool(Player, Symbol-Amount) ),
	New_amount is Amount-1,
	assert( mana_pool(Player, Symbol-New_amount) ),
	take_mana(String, Player).

You could then construct a list-like representation to display it to the
player, if you really had to, but you probably wouldn't. Anyway the
point is to have an efficient internal representation of the mana pool.
Actually, the problem is that such an efficient representation would be
an array, because you'll never really need to insert or remove elements
from the mana pool-list, but then we don't have arrays here. My thought
is whether using Prolog's database can make for an efficient
search-and-replace alternative.

+-Mana pools as left-right trees-+
A further option is to use the assoc module and save the mana pool as a
balanced tree, which then makes all operations on it O(log n) time,
apparently. The problem with this is that to make it readable you then
need to turn it back into a list, so you have to add two operations to
everything you do: assoc_to_list and list_to_assoc.

Well... won't this still be better than any list processing? You would
never have to go through an O(n) op, your time would never be worse than
O(log n)?

And in any case: again, why turn it back to a readable list? Like I
said, the point is to have a fast internal representation of a mana
pool. I can do the list_to_assoc and assoc_to_list while debugging
(thoug tracing will still be a bitch).
*/


















