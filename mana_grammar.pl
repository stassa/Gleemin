/*
+-Contents-+
This file defines a grammar and auxiliary predicates for parsing and
managing mana in the rules engine.

Mana turns out to be a complex entity. The different classes of grammar
rule declared here represent:

1) "mana_colour": the five colours of mana.
2) "mana_type": the six types of mana (five colours and colourless).
3) "<x>_mana_symbol": mana symbols, used to represent mana on cards.
4) "mana_cost": the syntax of mana costs.

These rules correspond to sections "105. Colors", "106. Mana" and "107.
Numbers and Symbols" in the Comprehensive Rules.

+-Comprehensive Rules-+
All references to the CompRules in this and later documentation are from
the text version of 2011-09-30, available free from WoTC's site, in the
following url:

http://www.wizards.com/magic/comprules/MagicCompRules_20110930.txt

Note that Gleemin was created using an earlier version of the CompRules
(September 2010, ie pre-Phyrexian mana).

+-Mana costs structure-+
"mana_cost" rules define a structure: the order of precedence of
mana symbols in a mana cost (for example, generic mana symbols precede
coloured mana symbols). This order is not explicitly listed in the
Comprehensive rules and so it is necessarily derived empirically,
through searches in The Gatherer. This order is:

  Generic mana cost, followed by
  Coloured mana cost.
  Where Generic mana cost is:
    Zero or more variable mana symbols, followed by
    Zero or more mana numerals, followed by
    Zero or more snow mana symbols, followed by
    Zero or more phyrexian symbols
  And Coloured mana cost is:
    Zero or more phyrexian mana symbols, followed by
    Zero or more monocoloured mana symbols, followed by
    Zero or more hybrid mana symbols, followed by
    Zero or more primary-colour mana symbols.

Note that the more exotic kinds of mana, ie phyrexian and hybrid mana,
do not currently appear together on any cards and are probably too
complicated (also visually) to do so in the future, so it might have
been more correct to define those as mutually exclusive.

+-Textual mana symbols-+
The text version of the CompRules and the Magic Set FAQs consistently
list mana values as capital letters or numbers enclosed in curly braces:
{12}{X}{W}{U}{B}{R}{G}, etc.

The Gatherer's text spoilers and text spoilers from sets' product
pages on the M:tG site take a more liberal approach.

The Gatehrer's text spoilers list mana values as capital letters or
numbers when they appear in cards' mana costs (the mana symbols in the
upper right corner), and as capital letters or numbers enclosed in curly
braces when they appear in a card's text box.

For example:

  ======================================================================
  Name:		Abbey Matron
  Cost:		2W
  Type:		Creature — Human Cleric
  Pow/Tgh:	(1/3)
  Rules Text:	{W}, {T}: Abbey Matron gets +0/+3 until end of turn.
  Set/Rarity:	Homelands Common, Homelands Common
  ======================================================================
  (http://gatherer.wizards.com/Pages/Search/Default.aspx?text=+[{W}])

On the other hand, hybrid mana is represented on The Gatherer as capital
letters enclosed in parentheses in cards' mana costs and as lower case
letters enclosed in parentheses, enclosed in curly braces in a card's
text box:

  ======================================================================
  Name:		Avatar of Discord
  Cost:		(B/R)(B/R)(B/R)
  Type:		Creature — Avatar
  Pow/Tgh:	(5/3)
  Rules Text:	({(b/r)} can be paid with either {B} or {R}.)
                Flying
                When Avatar of Discord enters the battlefield, sacrifice
	        it unless you discard two cards.
  Set/Rarity: Archenemy Rare, Dissension Rare
  =======================================================================
  (http://gatherer.wizards.com/Pages/Search/Default.aspx?set=[%22Dissension%22])

 - but the other way around minus the curly braces in the text spoilers
 on sets' product pages:

  =======================================================================
  Card Name: Avatar of Discord
       Cost: (b/r)(b/r)(b/r)
       Type: Creature - Avatar
    Pow/Tgh: 5/3
  Rules Text: ((B/R) can be paid with either B or R.)
             Flying
             When Avatar of Discord comes into play, sacrifice it
             unless you discard two cards.
  Set/Rarity: Dissension rare
  =======================================================================
  (http://www.wizards.com/magic/generic/cardlists/dissension_spoiler_en.txt)

It's also interesting to note that the reminder text in The Gatherer
text spoiler refers to a symbol that can not be found in the rest of
that text (since it doesn't use the same notation as the mana cost it is
meant to clarify).

Curiously, searching The Gatherer for B/R seems to find the same (7)
cards as searching for b/r, while searching for {(b/r)} finds nothing.

Similar considerations apply to:

Monocoloured hybrid mana (listed as {2/W} etc in the set FAQs and
CompRules):

  =======================================================================
  Name:	Advice from the Fae
  Cost:	(2/U)(2/U)(2/U)
  Type:	Sorcery
  Pow/Tgh:
  Rules Text: ({(2/u)} can be paid with any two mana or with {U}. This
		card's converted mana cost is 6.) Look at the top five
		cards of your library. If you control more creatures
		than each other player, put two of those cards into your
		hand. Otherwise, put one of them into your hand. Then
		put the rest on the bottom of your library in any order.
  Set/Rarity: Shadowmoor Uncommon
  =======================================================================
  (http://gatherer.wizards.com/Pages/Search/Default.aspx?output=spoiler&m
  ethod=text&action=advanced&set=|[%22Shadowmoor%22])

Phyrexian mana (listed as {W/P} etc in the set FAQs and CompRules):

  =======================================================================
  Name:		Act of Aggression
  Cost:		3(R/P)(R/P)
  Type:		Instant
  Pow/Tgh:
  Rules Text:	({(r/p)} can be paid with either {R} or 2 life.)
                Gain control of target creature an opponent controls
		until end of turn. Untap that creature. It gains haste
		until end of turn.
  Set/Rarity: New Phyrexia Uncommon
  =======================================================================
  (http://gatherer.wizards.com/Pages/Search/Default.aspx?set=[%22New%20Ph
  yrexia%22])

And also to Snow mana. In particular, the symbol for Snow mana is
listed as {S} in FAQs and the CompRules, but appears as {S}i} in
Gatherer searches, except apparently in the case of adjacent snow mana
symbols where it appears as {S}i{S}i}:

  =======================================================================
  Name:		Frost Raptor
  Cost:		2U
  Type:		Snow Creature — Bird
  Pow/Tgh:	(2/2)
  Rules Text:	Flying
               {S}i{S}i}: Frost Raptor gains shroud until end of turn.
	      ({S}i} can be paid with one mana from a snow permanent.)
  Set/Rarity: Coldsnap Common
  =======================================================================
  (http://gatherer.wizards.com/Pages/Search/Default.aspx?set=[%22Coldsnap%
  22])

-while in the text spoiler for Coldsnap, it appears as "oSi" (possibly
as some kind of obscure in-joke on standardisation):

  =======================================================================
  CardName: Adarkar Windform
  Cost: 4U
  Type: Snow Creature - Illusion
  Pow/Tgh: 3/3
  Rules Text: Flying 1oSi: Target creature loses flying until end of turn.
             (oSi can be paid with one mana from a snow permanent.)
  Set/Rarity: Coldsnap Uncommon
  =======================================================================
 (http://www.wizards.com/magic/generic/cardlists/coldsnap_spoiler_en.txt)

Fun as all this variety may be, the grammar in this source file will
have to focus on one kind of textual representation of mana symbols,
lest madness sets in. This has to be the one in the CompRules, which
are, after all, the "ultimate authority for the game". Any other syntax
will have to be compensated for at an earlier point in the parsing
sequence (most probably the tokeniser).

As a Rosewater-ish aside, yesterday I resigned from my job because the
senior programmer in the company called me a moron and told me to fuck
off, presumably for not being thorough enough in my work. Oh really.


+-Generic is not Colourless-+
"Generic" and "colorless" are not the same thing. One refers to mana in
costs and the other to a type of mana.

From the Glossary:

  [4928] Generic Mana
  [4929] Mana in a cost not represented by colored mana symbols; it can
     be paid with mana of any type. See rule 107.4.

  [4611] Colorless
  [4612] 1. An object with no color is colorless. Colorless is not a
    color. See rule 105, "Colors," and rule 202, "Mana Cost and Color."
  [4613] 2. A type of mana. See rule 106, "Mana," and rule 107.4c.

On the other hand, the symbols used to represent generic and colourless
mana are the same:

 107.4b Numeral symbols (such as {1}) and variable symbols (such as {X})
  represent generic mana in costs. Generic mana in costs can be paid
  with any type of mana. For more information about {X}, see rule 107.3.

 107.4c Numeral symbols (such as {1}) and variable symbols (such as {X})
  can also represent colorless mana if they appear in the effect of a
  spell or ability that reads "add [mana symbol] to your mana pool" or
  something similar. (See rule 107.3e.)

In other words, the same symbols represent generic mana when they are
found in a mana cost, but colorless mana when they are found elsewhere.
Therefore, in this grammar:
  Rules for "mana_colour" include neither generic nor colorless mana.
  Rules for "mana_type" include	"colorless" but not "generic_mana_cost".
  Rules for "mana_cost" include "generic_mana_cost" which maps to the
    colorless mana symbols ("variable_mana_symbol", "mana_numeral" etc).
  Rules for "mana_symbol" include "colourless_mana_symbol" which maps to
    colourless mana symbols as above, while a non-context-free version
    maps from types or colours to symbols.


+-Other rules-+
Rules for mana abilities and all the predicates for interacting with
mana (drawing it from and adding it to mana pools etc) are also (to be)
included in this source file, which should eventually become a module-
when a standard for Prolog modules is established and can guarantee
portability of modularised source between Prologs.

+-The Spellings of Colo/ur-+
Terminals use the US spelling ("color") as is used on the cards and in
rules documents since this will make parsing painless. The British
spelling ("colour") is used for non-terminals where it is highly
unlikely to cause any trouble.
*/

% Mana colours
% 106.1a There are five colors of mana: white, blue, black, red,
%  and green.

    mana_colour --> ['White'] ; [white].
    mana_colour --> ['Blue'] ; [blue].
    mana_colour --> ['Black'] ; [black].
    mana_colour --> ['Red'] ; [red].
    mana_colour --> ['Green'] ; [green].

% Mana types
% 106.1b There are six types of mana: white, blue, black, red, green,
%   and colourless.

    mana_type --> ['White'] ; [white].
    mana_type --> ['Blue'] ; [blue].
    mana_type --> ['Black'] ; [black].
    mana_type --> ['Red'] ; [red].
    mana_type --> ['Green'] ; [green].
    mana_type --> ['Colorless'] ; [colorless].

% Symbols and costs
% 106.2. Mana is represented by mana symbols (see rule 107.4). Mana
%  symbols also represent mana costs (see rule 202).

% Mana costs.
       % Some cards have no mana cost (eg land cards).
    mana_cost --> [].
    mana_cost --> generic_mana_cost, coloured_mana_cost.

    generic_mana_cost -->
        ([] ; variable_mana_symbols),
        ([] ; mana_numerals),
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
	% Mana cost can be [], but mana can't.
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
    phyrexian_symbol --> ['{P}']. % See CompRules: 107.4g

    % Snow mana
    snow_mana_symbol --> ['{S}'].

% Numerals
    % The first clause is here to allow some
    %  generation of numerical mana costs. Obviously,
    %  it will only generate 0-mana costs. Expand if
    %  necessary, remove if not needed.
    mana_numeral --> ['{0}'], !. % green cut
    % For mana numerals, we need to open up the atom
    %  and get the juicy bit inside. Er. The number code.
    % This takes a bit of doing. This way is a little condensed:
    mana_numeral --> [N],
	{ atom_codes(N, [123 | Codes]) , % [123] = "{"
	numeric_string(Codes, []) }, !. % green cut

    numeric_string --> [125]. % "{"
    numeric_string --> [N],
	{N >= 48, N =< 57}, numeric_string. % [1-9]


% Non context-free rules.

% Mana symbols by colour and type
    mana_symbol(Type) --> ['{W}'], {Type = 'White' ; Type = white}.
    mana_symbol(Type) --> ['{U}'], {Type = 'Blue' ; Type = blue}.
    mana_symbol(Type) --> ['{B}'], {Type = 'Black' ; Type = black}.
    mana_symbol(Type) --> ['{R}'], {Type = 'Red' ; Type = red}.
    mana_symbol(Type) --> ['{G}'], {Type = 'Green' ; Type = green}.
    % Next, one type matches two clases of symbol, so it's faster to
    %  make the comparison at the head of the clause.
    mana_symbol('Colorless') -->  variable_mana_symbol ; mana_numeral.
    mana_symbol(colorless) -->  variable_mana_symbol ; mana_numeral.
	% Think of this as "symbols for each colour or type of mana".

/* Usage:
Find the symbol of a colour or type of mana:
?- phrase(mana_colour, [Colour]), phrase(mana_symbol(Colour), [Symbol]).
Colour = 'White',
Symbol = '{W}' ;
Colour = white,
Symbol = '{W}' ;
...

Find the colour or type of mana a symbol represents:
?- phrase(mana_type, [Type]), phrase(mana_symbol(Type), ['{X}']).
Type = 'Colorless' ;
Type = colorless ;
...
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%players > mana_pool/2 19/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% refactored ?/01/12
    /* mana_pool(Player, Mana) */
    % A player's mana pool

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
*/
    :- dynamic mana_pool/2.

    % Mana Pools
    mana_pool('Player 1', ['{X}'-0, '{W}'-0, '{U}'-0, '{B}'-0, '{R}'-0, '{G}'-0]).
    mana_pool('Glee-min', ['{X}'-0, '{W}'-0, '{U}'-0, '{B}'-0, '{R}'-0, '{G}'-0]).


%%%%%%%%%%%%%%player_actions>mana_cost/3 07/02/2011
%%%%%%%%%%%%%%%%%%%%%%%
    /* mana_cost(+Cost) */
    % Cost: a list of symbols.
    % Checks that the Cost is a mana cost.

    mana_cost(Cost):-
	phrase(mana_cost, Cost).


%%%%%%%%%%%%%%players > add_mana/3 21/01/11
%%%%%%%%%%%%%%%%%%%%%%%
%Corrects: improves performance (by using diff_append).
    /* add_mana(Mana_amount, Mana_pool, New_pool) */
   % Adds an amount of mana to a player's pool.
   % Not responsible for saving the New_pool.

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
ToDo: Rename to, or interface by add_mana.

Mana_amount: a list of mana symbols headed by a number and sorted in the
  order:
    Number-W-U-B-R-G
Mana_pool: a player's mana pool:
    mana_pool(Player, Mana_pool).
New_pool: the player's mana pool, increased by the Mana_amount. Each
  mana symbol in the input adds one mana of that colour to the pool. The
  number at the head is added to the colourless mana in the pool.
For example, the mana amount:
    [2, '{R}','{G}']
  - will add two colourless, one red and one green mana to a pool.
*/

    % Merge diff_append.pl to predicates libary, then comment this line out.
    :- ensure_loaded('diff_append.pl').

%%%%%%%%%%%%%%mana_addition/4 (1) 04/01/12

    mana_addition([Colourless | Mana], ['{X}'-Amount | Pool], Acc, Temp):-
	number(Colourless), !, %red cut
	New_amount is Amount + Colourless,
	mana_addition(Mana, ['{X}'-New_amount | Pool], Acc, Temp).

%%%%%%%%%%%%%%mana_addition/4 (2) 04/01/12

    mana_addition([Symbol | Mana], [Symbol-Value | Pool], Acc , Temp):-
	New_value is Value + 1,
	mana_addition(Mana, [Symbol-New_value | Pool], Acc, Temp).

%%%%%%%%%%%%%%mana_addition/4 (3) 04/01/12

    mana_addition(Mana, [Symbol-Value | Pool], Acc, Temp):-
	diff_append(Acc, [Symbol-Value|Y]-Y, New_acc),
	mana_addition(Mana, Pool, New_acc , Temp).

%%%%%%%%%%%%%%mana_addition/4 (0) 04/01/12

    mana_addition([], _, _Pool,
		  ['{X}'-_C,'{W}'-_W,'{U}'-_U,'{B}'-_B,'{R}'-_R,'{G}'-_G]).

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
Usage:
Add an amount of mana to a player's pool:
[?- notrace, mana_pool(Player1, Pool), mana_addition([1, '{W}', '{R}', '{R}', '{R}'], Pool, X-X, X), !.
Player1 = 'Player 1',
Pool = ['{X}'-0, '{W}'-0, '{U}'-0, '{B}'-0, '{R}'-0, '{G}'-0],
X = ['{X}'-1, '{W}'-1, '{U}'-0, '{B}'-0, '{R}'-3, '{G}'-0].

Assert/1 the new pool to the knolwedge-base separately, if needed.
*/






