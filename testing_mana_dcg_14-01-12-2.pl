% This only defines mana, not costs. For example,
%  a "P" with colourless background can be in rules text
%  to mean any phyrexian mana.

/*
	mana --> mana_symbol_string.
	mana --> mana_numeral.
	mana --> mana_numeral, mana_symbol_string.
        mana --> mana_symbol_variable.
        mana_symbol_string --> mana_symbol.
        mana_symbol_string --> mana_symbol_variable, mana_symbol.
	mana_symbol_string --> mana_symbol, mana_symbol_string.
*/

	%Note: mana cost can be [], but mana can't.
	mana --> generic_mana.
        mana --> coloured_mana.
	mana --> generic_mana, coloured_mana.

        generic_mana --> variable_mana_symbols.
        generic_mana --> mana_numerals.
        generic_mana --> snow_mana_symbols.
        generic_mana --> variable_mana_symbols, mana_numerals.

        variable_mana_symbols --> variable_mana_symbol.
        variable_mana_symbols --> variable_mana_symbol, variable_mana_symbols.

        mana_numerals --> mana_numeral.
        mana_numerals --> mana_numeral, mana_numerals.

        snow_mana_symbols --> snow_mana_symbol.
        snow_mana_symbols --> snow_mana_symbol, snow_mana_symbols.

        % Different types of mana symbols may be ordered, eg
	%  phyrexian mana always coming first, but that is not
        %  strictly defined in the rules, so it's treated as a
	%  by-convention rule. The order of the coloured_mana_symbol
	%  rules will impose an order on generated mana strings, but
        %  any ordering will be accepted as input.
	coloured_mana --> coloured_mana_symbol.
        coloured_mana --> coloured_mana_symbol, coloured_mana.

        coloured_mana_symbol --> phyrexian_mana_symbol.
        coloured_mana_symbol --> hybrid_mana_symbol.
        coloured_mana_symbol --> monocoloured_hybrid_mana_symbol.
        coloured_mana_symbol --> primary_coloured_mana_symbol.

        % mana: terminals
        % Coloured mana:
	% Primary mana colours
	primary_coloured_mana_symbol --> ['{W}'].
	primary_coloured_mana_symbol --> ['{U}'].
	primary_coloured_mana_symbol --> ['{B}'].
	primary_coloured_mana_symbol --> ['{R}'].
	primary_coloured_mana_symbol --> ['{G}'].
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
	% Variable mana symbol
	variable_mana_symbol --> ['{X}'].

        % Snow mana
	snow_mana_symbol --> ['{S}'].

	mana_numeral --> [C], { number(C), C >= 0 }.
	% ^ but query with phrase(mana, [<number>])!!


      mana(X) --> {phrase(mana, [X])}, [X].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%players > mana_pool/2 19/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%13-01-12 Corrects: Colourless --> Generic
	/* mana_pool(Player, Mana) */
	% Where Mana == [Generic, White, Blue, Black, Red, Green]

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	Mana quantities should be of the type:
	 '{<number>'} (generic) or
	 '{W}' (white) or
	 '{U}' (blue) or
	 '{B}' (black) or
	 '{R}' (red) or
	 '{G}' (green)
	So for example, the cost two, red, black, black
	should look like:
	  ['{2}','{R}','{B}','{B}']
	This should then be turned into a mana_quantity:
	  ['{X}'-2, '{W}'-0, '{U}'-0, '{B}'-2, '{R}'-1, '{G}'-0]
	I'll need to deal separately with the problem of converting
	   '{X}'-2 to '{2}' and vv.
*/

	% Mana Pools
%	mana_pool('Player 1', [g-0, w-0, u-0, b-0, r-0, g-0]).
%	mana_pool('Glee-min', [g-0, w-0, u-0, b-0, r-0, g-0]).
	mana_pool('Player 1', ['{X}'-0, '{W}'-0, '{U}'-0, '{B}'-0, '{R}'-0, '{G}'-0]).
	mana_pool('Glee-min', ['{X}'-0, '{W}'-0, '{U}'-0, '{B}'-0, '{R}'-0, '{G}'-0]).

%%%%%%%%%%%%%%player_actions>mana_cost/3 07/02/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* mana_cost(+Cost) */
	% Checks that a cost is a mana cost.

	mana_cost(Cost):-
		phrase(mana, Cost).




%%%%%%%%%%%%%%players > add_mana/3 21/01/11
%%%%%%%%%%%%%%%%%%%%%%%
%Corrects: time complexity
	/* add_mana(Mana_list, Mana_pool, New_pool) */
	% Adds the mana to the player's pool.

%%%%%%%%%%%%%%add_mana/3 (1) 21/01/11

	% Boundary condition...
	add_mana([], New_pool, New_pool).

%%%%%%%%%%%%%%add_mana/3 (2) 21/01/11

	% For each symbol in the mana list, add 1 to the appropriate value
	%  in the mana pool. Numbers (representing colourless mana) are
	%  added wholesale.

/*%OK
	add_mana([Generic | Mana], [c-Amount | Pool], New_pool):-
		number(Generic),
		New_amount is Amount + Generic,
		add_mana(Mana, [c-New_amount | Pool], New_pool).
*/

%%%%%%%%%%%%%%add_mana/3 (2) 04/01/12

	add_mana([Generic | Mana], ['{X}'-Amount | Pool], New_pool):-
		number(Generic), !, %red cut
		New_amount is Amount + Generic,
		add_mana(Mana, ['{X}'-New_amount | Pool], New_pool).

	add_mana([Symbol | Rest], ['{X}'-X,'{W}'-W,'{U}'-U,'{B}'-B,'{R}'-R,'{G}'-G], New_pool):-
		(Symbol = '{W}' -> W1 is W + 1; W1 is W),
		(Symbol = '{U}' -> U1 is U + 1; U1 is U),
		(Symbol = '{B}' -> B1 is B + 1; B1 is B),
		(Symbol = '{R}' -> R1 is R + 1; R1 is R),
		(Symbol = '{G}' -> G1 is G + 1; G1 is G),
		add_mana(Rest, ['{X}'-X,'{W}'-W1,'{U}'-U1,'{B}'-B1,'{R}'-R1,'{G}'-G1], New_pool).


	mana_addition([Colourless | Mana], ['{X}'-Amount | Pool], Acc, Temp):-
	    number(Colourless), !, %red cut
	    New_amount is Amount + Colourless,
	    mana_addition(Mana, ['{X}'-New_amount | Pool], Acc, Temp).
       % OK, now, first test, then remove the "X" from the pool and add just numbers. Which means
       %  you'll have to do some string manip. Yay!

/*        mana_addition([Symbol1 | Mana], [Symbol2-Value | Pool], Acc , Temp):-
	    pair_addition(Symbol1, Symbol2-Value, New_value),
	    mana_addition(Mana, [Symbol2-New_value | Pool], Acc, Temp).
*/
        mana_addition([Symbol | Mana], [Symbol-Value | Pool], Acc , Temp):-
	    New_value is Value + 1,
	    mana_addition(Mana, [Symbol-New_value | Pool], Acc, Temp).

        mana_addition(Mana, [Symbol-Value | Pool], Acc, Temp):-
	    diff_append(Acc, [Symbol-Value|Y]-Y, New_acc),
	    mana_addition(Mana, Pool, New_acc , Temp).

	 mana_addition([], _, _Pool, ['{X}'-_X,'{W}'-_W,'{U}'-_U,'{B}'-_B,'{R}'-_R,'{G}'-_G]).

/*        pair_addition(Symbol, Symbol-Value, New_value):-
	        New_value is Value + 1, !.
*/


/*
Remember this:
?- test(A,B), diff_append(A-B, [a-1|C]-C, X-Y).
A = [test, a-1|Y],
B = [a-1|Y],
C = Y,
X = [test, a-1|Y].
So you need to keep your mana pools as diff lists, and
get your mana strings as mana(Symbol, Rest) predicates. I think.

For example, you can define:
      mana(X) --> {phrase(mana, [X])}, [X].

And then do (something like) this:

?- phrase(mana('{X}'), Y).
Y = ['{X}'] ;
false.

?- mana('{X}', X, Y).
X = ['{X}'|Y] ;
false.

?- mana('{X}', X, Y), mana('{R}', A, B), diff_append(X-Y, A-B, Z-[]).
X = ['{X}', '{R}'],
Y = A, A = ['{R}'],
B = [],
Z = ['{X}', '{R}'] .

?- mana('{X}', X, Y), mana('{R}', A, B), diff_append(X-Y, A-[], Z-[]).
X = ['{X}', '{R}'|B],
Y = A, A = ['{R}'|B],
Z = ['{X}', '{R}'|B] .

?- mana('{X}', X, Y), mana('{R}', A, B), diff_append(X-Y, A-B, Z-C).
X = ['{X}', '{R}'|C],
Y = A, A = ['{R}'|C],
B = C,
Z = ['{X}', '{R}'|C]

This still means that you need an operation to create a diff list for
each element you want to append, and another to create a normal list
from the diff list generated by the append (or some logic to make the
last append generate a normal list). However, you can do the first part
as a batch operation when you create a mana string to pass to the
add_mana predicate:

  mana_string([]) --> [].
  mana_string([Symbol | Mana]) -->
	{mana(Symbol, X, _Y)},
	[X],
	mana_string(Mana).

?- phrase(mana_string(['{X}', '{R}']), X).
X = [['{X}'|_G4267], ['{R}'|_G4288]] .

pair_addition will have to change to reflect this:

        pair_addition([Symbol | _], Symbol-Value, New_value):-
	        New_value is Value + 1, !.


Let's see again:
?- Acc = [X]-X, diff_append(Acc, [a-1|Y]-Y, Z), diff_append(Z, [b-2|A]-A, Z1).
Acc = [[a-1, b-2|A]]-[a-1, b-2|A],
X = [a-1, b-2|A],
Y = [b-2|A],
Z = [[a-1, b-2|A]]-[b-2|A],
Z1 = [[a-1, b-2|A]]-A.

The useful insightisthat the diff_append generates as output a
difference list. So you can stick it directly to the next call. Also you
can start it with a not-exactly-difference list: [X]-X, which I don't
think is exactly the empty diff list.
Actually, you can start it with X-X too, it seems to have the same
output.


%  accumulator(Accumulator) -->  [Accumulator].


        mana_addition([Symbol1 | Mana], [Symbol2-Value | Pool], Acc , Temp):-
	    pair_addition(Symbol1, Symbol2-Value, New_value),
	    mana_addition(Mana, [Symbol2-New_value | Pool], Acc, Temp).

        mana_addition(Mana, [Symbol-Value | Pool], Acc, Temp):-
	    diff_append(Acc, [Symbol-Value|Y]-Y, New_acc),
%	    diff_append([Acc]-_X, [Symbol-Value|Y]-Y, New_acc-_A),
	    mana_addition(Mana, Pool, New_acc , Temp).

	 mana_addition([], _, _Pool, ['{X}'-_X,'{W}'-_W,'{U}'-_U,'{B}'-_B,'{R}'-_R,'{G}'-_G]).
%	 mana_addition([], _, Pool, Pool).

To understand the above, first check this out:
?- mana_addition([a,a,a,a,b,b,b,b,c,c,c,c,c], [a-0,b-0,c-0], X-X, Y).
X = [a-4, b-4, c-5|_G1203],
Y = [a-4, b-4, c-5|_G1203]-_G1203

Since X returns a diff list you can map it to a normal list:
?- mana_addition([a,a,a,a,b,b,b,b,c,c,c,c,c], [a-0,b-0,c-0], X-X, Y), X = [a-A, b-B, c-C].
X = [a-4, b-4, c-5],
Y = [a-4, b-4, c-5]-[],
A = B, B = 4,
C = 5 .

So you can do the mapping directly, at the output arg. Which I reckon,
saves you an additional operation to unify the output with a cleanup
pattern later.
 */



/*
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

Note that the predicate above is still an O(n) operation, but most of
the time it would probably save you a whole lot of n, because you would
only add one or two symbols most of the time, and you wouldn't have to
go through the whole mana pool-list for each of them. On the other hand,
that's what my new predicate does, and with a sing-op append which is
equivalent to an assert, I think... and it doesn't really need to do the
retract.

A further option is to use the assoc module and save the mana pool as a
balanced tree, which then makes all operations on it O(log n) time,
apparently. The problem with this is that to make it readable you then
need to turn it back into a list, so you have to add two operations to
everything you do: assoc_to_list and list_to_assoc.
Well... won't this still be better than any list processing? You would
never have to go through an O(n) op, your time would never be worse than
O(log n)?
*/























