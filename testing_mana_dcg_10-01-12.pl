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
/*	add_mana([Symbol | Rest], [c-C, w-W, u-U, b-B, r-R, g-G], New_pool):-
		(number(Symbol) -> C1 is C + Symbol; C1 is C),
		(Symbol == w -> W1 is W + 1; W1 is W),
		(Symbol == u -> U1 is U + 1; U1 is U),
		(Symbol == b -> B1 is B + 1; B1 is B),
		(Symbol == r -> R1 is R + 1; R1 is R),
		(Symbol == g -> G1 is G + 1; G1 is G),
		remove(Symbol, [Symbol | Rest], Rest),
		add_mana(Rest, [c-C1, w-W1, u-U1, b-B1, r-R1, g-G1], New_pool),!.
*/

/*	add_mana([Symbol | Rest], Pool, New_pool):-
		member(Symbol-Amount, Pool),
		New_amount is Amount + 1,
		replace().
*/

/*%OK
	add_mana([Generic | Mana], [c-Amount | Pool], New_pool):-
		number(Generic),
		New_amount is Amount + Generic,
		add_mana(Mana, [c-New_amount | Pool], New_pool).
*/
/*	add_mana([Symbol1 | Mana], [Symbol2-Amount | [Symbol3 | Pool]], New_pool):-
	        (  Symbol1 = Symbol2 -> New_amount is Amount + 1,
		add_mana(Mana, [Symbol2-New_amount | [Symbol3 | Pool]], New_pool));
		add_mana( Mana, [Symbol3 | Pool], New_pool ).
*/

/*
	add_mana([Generic | Mana], [c-Amount | Pool], New_pool):-
		number(Generic),
		New_amount is Amount + Generic,
		add_mana(Mana, [c-New_amount | Pool], New_pool).

	add_mana([Symbol1 | Mana], [Symbol2-Amount | Pool], New_pool):-
	        Symbol1 = Symbol2,
		New_amount is Amount + 1,
		add_mana(Mana, [Symbol2-New_amount | Pool], New_pool).

	add_mana([_ | [Symbol | Mana ]], [_ | [Symbol-Amount | Pool]], New_pool):-
	        true,
		add_mana([Symbol | Mana], [Symbol-Amount | Pool], New_pool).
*/





%	add_mana([], Pool, [_Rest|Pool]).
/*	add_mana([Generic | Mana], [c-Amount | Pool], New_pool):-
		number(Generic),
		New_amount is Amount + Generic,
		add_mana(Mana, [c-New_amount | Pool], New_pool).
	add_mana([Symbol | Mana], [Symbol-Amount | Pool], New_pool):-
		New_amount is Amount + 1,
		add_mana(Mana, [Symbol-New_amount | Pool], New_pool).

	add_mana([_Symbol | [Symbol | Mana ]], [_Symbol-_Amount | [Symbol-Amount | Pool]], New_pool):-
		add_mana([Symbol | Mana], [Symbol-Amount | Pool], New_pool).

	add_mana([Symbol | Mana], [Symbol-Amount | Pool], New_pool):-
		add_mana(Mana, Pool, [Symbol-Amount | New_pool]).
*/

/*List = [a-1, b-2, c-3], list_to_assoc(List, Assoc), map_assoc(succ, Assoc, Assoc1),assoc_to_list(Assoc1, List1).	*/

/*        pair_addition(Symbol, Symbol-Value, New_value):-
	        New_value is Value + 1, !.
	pair_addition(_, _-Value, Value).
	% wrong key format?
*/

/*%Works, not happy.
        add_mana([Symbol | Mana], Pool, Temp):-
	    member(Symbol-Value, Pool), !,
	    New_value is Value + 1,
	    list_to_assoc(Pool, Pool_assoc),
	    put_assoc(Symbol, Pool_assoc, New_value, New_assoc),
	    assoc_to_list(New_assoc, New_pool),
	    add_mana(Mana, New_pool, Temp).

       add_mana([Symbol | _Mana], _Pool, _Temp):-
           write('Wrong symbol in mana string ': Symbol), nl, !, fail.
*/

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
















