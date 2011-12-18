% First saved: 17/02/2011
% Last saved: 13/04/2011

% Doing:
%	adding gets(X,Y) to modifiers.
%	still fixing damage and objects...
%	  Done redoing everything (here) to use obect_handle
%	  and object_state internally, not passing objects around
% Todo
%
% NOTES:
% Past done:
%	added atom_word_list
% 	Objects overhual
%	changed atom_to_list; go back to previous stable predicates.pl
%	  if there's trouble.
%	fixed make_unique (was creating copies of cards)
%	added list_difference
%	added flatten_list
% 	added deconstruct and assemble
%	added damage predicates


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
%%%%              Next                 %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%next/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* next(?Current, ?Next, ?List) */
	% Find the next or previous element in a list
	%   or generate a list from elements.

	next(Current, Next, [Current, Next|_]).
	next(Current, Next, [H|T]):-
		next(Current, Next, T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%           Compare Lists           %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%compare_lists/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* compare_lists(+List_1, +List_2) */
	% True if each element in List_1 is in List_2
	%  exactly as many times.

	compare_lists(List_1, List_2):-
		compare_lists(List_1, List_1, List_2, List_2).

	compare_lists([],_, [], _).
	compare_lists([H1 | T1], List_1, [H2 | T2], List_2):-
		member(H1, List_2),
		member(H2, List_1),
		compare_lists(T1, List_1, T2, List_2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%          List Difference          %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%list_difference/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* list_difference(+List_1, +List_2, -Difference) */
	% Returns the difference of two lists (as in sets)

	list_difference(List, List, []).
	list_difference(L1, L2, Diff):-
		list_difference(L1, L2, [], Diff_1),
		list_difference(L2, L1, [], Diff_2),
		append(Diff_1, Diff_2, Diff).

	list_difference(List, List, Diff, Diff).
	list_difference([], _List, Diff, Diff).
	list_difference([H | T], L2, Temp, Diff):-
		\+ member(H, L2),
		append(Temp, [H], New),
		list_difference(T, L2, New, Diff).
	list_difference([_ | T], L2, Temp, Diff):-
		list_difference(T, L2, Temp, Diff).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%           Flatten List            %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%flatten_list/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* flatten_list(+List, -Flat) */
	% from https://sites.google.com/site/prologsite/prolog-problems/1

	flatten_list(X,[X]) :-
		\+ list(X).
	flatten_list([],[]).
	flatten_list([X|Xs],Zs) :-
		flatten_list(X,Y), flatten_list(Xs,Ys), append(Y,Ys,Zs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%           Permutation             %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%permutation/2 14/04/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* permutation(-Permutation, +List) */
	% Permutation is a permutation of List
	% Taken from Sterling and Shapiro (1986) pg. 113

	permutation([],[]).
	permutation([Z|Zs], Xs):-
		remove(Z,Xs,Ys),
		permutation(Zs, Ys).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              Maths                %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%floor/3 13/04/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* floor(+Dividend,+Divisor,-Quotient) */
	% Performs division of Dividedn by Divisor, generating
	% an integer Quotient (always rounding _up_)

	ceiling(X, Y, Z):-
		A is X mod Y,
		A = 0,
		Z is X / Y, !.
	ceiling(X, Y, Z):-
		Z is X // Y + 1, !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ceiling/3 19/04/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* floor(+Dividend,+Divisor,-Quotient) */
	% Performs division of Dividedn by Divisor, generating
	% an integer Quotient (always rounding _down_)

	floor(X, Y, Z):-
		A is X mod Y,
		A = 0,
		Z is X / Y, !.
	floor(X, Y, Z):-
		Z is X // Y - 1, !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%sum/3 13/04/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* sum(+Numbers, [], -Sum) */
	% Sums up a list of numbers.

	sum([], Sum, Sum).
	sum([Num|List], Temp, Sum):-
		New_temp is Num + Temp,
		sum(List, New_temp, Sum).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%round/3 19/04/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* round(X, Y) */
	% Rounds a number up if its fractional part is more than 0.5
	%  and down otherwise.

/*
	round(X, X):-         %Swi, 04/06/11, def added in comp layer.
		integer(X).
	round(X, Y):-
		A is fp(X),
		B is int(X),
		( A >= 0.5 -> Y is B+1 ;
		Y is B ).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%             Atom - list           %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%atom_list/2 28/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/*  atom_list(?Atom, ?List) */
	% Converts between an atom and a list of atoms or numbers

%%%%%%%%%%%%%%atom_list/2 (1) 28/02/11

	% convert from an atom to a List
	%  of atomic characters (not charcodes!)
	atom_to_list(Atom, List):-
		type(Atom, 3), type(List, 0),
		%   	atom ^	variable ^
		atom_codes(Atom, Chars),
		to_list(Chars, _Temp_list, List), !.

%%%%%%%%%%%%%%atom_list/2 (2) 28/02/11

	% Convert from a list of characters
	%   to an Atom
	atom_to_list(Atom, List):-
		type(Atom, 0), type(List, 6),
		% variable ^	   list ^
		to_chars(List, [], Chars),
		atom_codes(Atom, Chars), !.


%%%%%%%%%%%%%%to_list/3 28/02/11
%%%%%%%%%%%%%%%%%%%%%%%

	% Turn the List of character codes
	%  to a list of atomic characters
	to_list([], List, List).
	to_list([Head | Tail], Temp, List):-
		number_code(Head),
		numbers([Head | Tail], [], Numbers, Rest),
		number_codes(Number, Numbers),
		append(Temp, [Number], New_temp),
		to_list(Rest, New_temp, List).
	to_list([Head | Tail], Temp, List):-
		atom_codes(Atomic, [Head]),
		append(Temp, [Atomic], New_temp),
		to_list(Tail, New_temp, List).


%%%%%%%%%%%%%%numbers/4 28/02/11
%%%%%%%%%%%%%%%%%%%%%%%

	% A sequence of character codes of numeric digits
	%  is turned into a single number (eg, 50,51 to 23)
	numbers([], Number, Number, []).
	numbers([Head | Tail], Temp_num, Number, Rest):-
		number_code(Head),
		append(Temp_num, [Head], New_num),
		numbers(Tail, New_num, Number, Rest).
	numbers(Rest, Number, Number, Rest).

	% Number char codes.
	number_code(Character) :-
		Character =< 0'9,
		Character >= 0'0.


%%%%%%%%%%%%%%to_chars/4 28/02/11
%%%%%%%%%%%%%%%%%%%%%%%

	% Turn the list of atomic characters
	%  to a list of character codes.
	to_chars([], Chars, Chars).
	to_chars([Head | Tail], Temp, Chars):- % done 03/04/11
		number(Head),
		number_codes(Head, Char),
		append(Temp, Char, New_temp),
		to_chars(Tail, New_temp, Chars).
	to_chars([Head | Tail], Temp, Chars):-
		atom_codes(Head, Char),
		append(Temp, Char, New_temp),
		to_chars(Tail, New_temp, Chars).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%          Atom -> word-list        %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%atom_word_list/3 31/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* atom_word_list(+Word_list, [], Atomic_forms) */
	% Converts a list of atomic words to a list of atoms for printing
	% Expand to convert the other way around and handle strings

%%%%%%%%%%%%%%atomic_abilities/3 (0) 31/03/11

	atom_word_list([], Atomic, Atomic).

%%%%%%%%%%%%%%atomic_abilities/3 (1) 31/03/11

	atom_word_list([Ability | Abilities], Temp, Atomic):-
		split_atom(Ability, [], Split),
		atom_to_list(Atom, Split),
		append(Temp, [Atom], New_temp),
		atom_word_list(Abilities, New_temp, Atomic).


%%%%%%%%%%%%%%split_atom/3 31/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* split_atom(Ability, [], Atomic_form) */
	% Converts an ability word-list to an atomic form

%%%%%%%%%%%%%%split_atom/3 (0) 31/03/11

	split_atom([], Atom, Atom).

%%%%%%%%%%%%%%split_atom/3 (1) 01/04/11

	% Add a full stop at the end!
	split_atom([Word | []], Temp, Atom):-
		length(Word, 1),
		( type(Word, 3) -> atom_codes(Word, Chars);
		% ^ The word is an atom
		type(Word, 1) -> number_codes(Word, Chars) ),
		% ^ The word is a number
		append(Chars, [46], Full),
		atom_codes(Stop, Full),
		append(Temp, [Stop], New_temp),
		split_atom([], New_temp, Atom).

%%%%%%%%%%%%%%split_atom/3 (2) 01/04/11

	% Add spaces between words
	split_atom([Word | Ability], Temp, Atom):-
		( type(Word, 3) -> atom_codes(Word, Chars);
		type(Word, 1) -> number_codes(Word, Chars) ),
		append(Chars, [32], Full),
		atom_codes(Spaced, Full),
		append(Temp, [Spaced], New_temp),
		split_atom(Ability, New_temp, Atom).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%             Objects               %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%deconstruct/3 30/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* object_map(?Object, ?State, ?Map) */
	% Converts between a Map: [Name - Id]
	%  and an MGL Object: copy(Name, Id, State)
	%  optionally returning its State list.

	object_map(Object, State, Map):-
		Map = [Name - Id],
		zone(_Zone, Objects),
		member(Object, Objects),
		Object =.. [object, Name - Id, State].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%object_handle/2 30/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* object_handle(?Object, ?Handle) */
	% Converts between an MGL object: objec(Name, Id, State)
	%  and its Handle: Name - Id.
	% Use to identify an Object in a zone.

%%%%%%%%%%%%%%object_handle/2 (1) 30/03/11

	% The Object is in a shared zone
	object_handle(Object, Handle):-
		Handle = Name - Id,
		zone(_Zone, Objects),
		member(Object, Objects),
		Object =.. [object, Name - Id, _State].

%%%%%%%%%%%%%%object_handle/2 (2) 10/04/11

	% The object is in a partition
/*	object_handle(Object, Handle):-
		Handle = Name - Id,
		zone(_Player, _Partition, Objects)->
		member(Object, Objects),
		Object =.. [object, Name - Id, _State].
	% Added this on 10/04 because I noticed it wasn't looking in
	%  partitions. Now it's causing problems in choose_targets
	%  and who knows what else. Temp fix below.
*/

	% Temporarily use to find objects in non-shared zones.
	object_handle_0(Object, Handle):-
		Handle = Name - Id,
		zone(_Player, _Partition, Objects),
		member(Object, Objects),
		Object =.. [object, Name - Id, _State].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%object_state/2 30/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* object_state(?Object, ?State) */
	% Returns an Object's state list (or finds all Objects with a given
	%  State list.
	% Use to query an Object's state.

	object_state(Handle, State):-
		object_handle(Object, Handle),
		Object =.. [object, _Name - _Id, State].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%object_template/3 30/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* object_template(?Handle, ?Field, ?Info) */
	% Returns information about the Object's card-template.
	% Use to find an Object's characteristics, like mana cost, P/T etc.

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	Field: the name of a field in a card/1 Characteristics list:
	card([card_name 	Name,
		mana_cost 	Cost,
		type_line 	[Supertype, Type, Subtype], % are lists
		text_box 	Text,
		power		Power,
		toughness	Toughness,
		loyalty 	Loyalty,
		state		State]
	)
	Each field name is a prefix operator; Prolog's internal represantation
	  of the field is: field_name(Field_value). The call below works
	  because Win-Prolog allows variable functor names. Other Prologs
	  will do the same thing with the "univ" operator:
		X =.. [Field, Info]
	The really cool thing with this is that the value of the field
	  can be a rule, so that it can then be called directly, with a call
	  like:
		object_template(Object, Field, Info),
		Info.
*/

	object_template(Handle, Field, Info):-
		Handle = Name-_Id,
		card(Characteristics),
		member(card_name Name, Characteristics),
%		member( Field(Info), Characteristics)
		Data =.. [Field, Info],
		member(Data, Characteristics).

	object_template(Object, Field, Info):-
		Object =.. [_,Name-_,_],
		card(Characteristics),
		member(card_name Name, Characteristics),
%		member( Field(Info), Characteristics)
		Data =.. [Field, Info],
		member(Data, Characteristics).


%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*	This is a general form of the predicate above, that will
	  allow an instance of any class to be queried for the value of
	  one of its fields, that can be a rule or a fact. The class
	  will need a field: instance Name to store its name. An even
	  more general form can leave the field-name operator unbound.

	:- op(100, fx, instance).
	class(Class, Instance, Field, Value):-
		Instance =..[_,Name-_,_],
		Class(Fields),
		member(instance Name, Fields),
		member(Field(Value), Fields).

	OOP - like inheritance hierarchies can be observed via predicates
	  that restrict querying of an object's fields to predicates
	  that are defined in its class, or a parent class.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%object_controller/3 30/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* object_controller(+Object, -Controller) */
	% Returns an Object's Controller, ie the player whose Partition
	%  the Object is in.
	% Use to find the controller of an object

	object_controller(Handle, Controller):-
		object_handle(Object, Handle),
		zone(Controller, _Zone, Permanents),
		member(Object, Permanents).


/*	object_controller(Object, Controller):-
		zone(Controller, _Zone, Permanents),
		member(Object, Permanents).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%             Make Unique           %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	:- dynamic(object_ids/2).
	:- dynamic(object/3).

	object_ids([], [0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%make_unique/2 04/02/2011
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* make_unique(+Card, -Copy) */
	% Generates copies of cards with unique ids.


%%%%%%%%%%%%%%make_unique/2 (1) 04/02/2011

/*	make_unique(Name, object : Name - Id / State):-
		\+ object_ids(Name, _),
		Id = 1, State = [],
		assert(object_ids(Name, [Id])).*/

	make_unique(Name, object(Name - Id, State)):-
		\+ object_ids(Name, _),
		Id = 1, State = [],
		assert(object_ids(Name, [Id])).

%%%%%%%%%%%%%%make_unique/2 (2) 04/02/2011

/*	make_unique(Name, object: Name - Id / State):-
		object_ids(Name, Ids),
		member(Id1, Ids),
		Id is Id1 + 1, State = [],
		append([Id], Ids, New_ids),
		retractall(object_ids(Name, _)),
		assert(object_ids(Name, New_ids)). */

	make_unique(Name, object(Name - Id, State)):-
		object_ids(Name, Ids),
		member(Id1, Ids),
		Id is Id1 + 1, State = [],
		append([Id], Ids, New_ids),
		retractall(object_ids(Name, _)),
		assert(object_ids(Name, New_ids)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%             Creature              %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%creature/8 03-04-11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* creature(?Handle, ?State, ?Casting_cost, ?Type_line,
		?Abilities, ?Power, ?Toughness, ?Controller) */
	% Queries a creature object for its characteristics,
	%  State-list and controller

	creature(Creature, State, Cost, Types, Abilities, Power, Toughness, Controller):-
		object_template(Creature, type_line, [_, ['Creature'], _]),
		object_state(Creature, State),
		object_template(Creature, mana_cost, Cost),
		object_template(Creature, type_line, Types),
		object_template(Creature, text_box, Abilities),
		power_toughness(Creature, State, Power, Toughness), !,
		% ^ Something was causing uncontrolled backtracking
		%  during output_combatants/X so I added the cut.
		%  I removed again on 13/04 and seems OK, but watch.
		% Added back on 26/04 >_<
		object_controller(Creature, Controller).


%%%%%%%%%%%%%%power_toughness/4 03-04-11
%%%%%%%%%%%%%%%%%%%%%%%
	/* power_toughness(+Object, +State, -Power, -Toughness) */
	% Returns a creature's power and toughness, applying any modifiers,
	%  from counters or "gets" effects.

	power_toughness(Object, State, Power, Toughness):-
		object_template(Object, power, [P]),
		object_template(Object, toughness, [T]),
		( member(counter(Counter_type, Counter_quantity), State) ->
			modifier(Counter_type, Counter_quantity, P, T, Counter_P, Counter_T);
			Counter_P = P, Counter_T = T
		),
		( member(gets(Gets_type, _Duration), State) ->
			modifier(Gets_type, [], Counter_P, Counter_T, Gets_P, Gets_T);
			Gets_P = Counter_P, Gets_T = Counter_T
		),
		Power = Gets_P, Toughness = Gets_T.


%%%%%%%%%%%%%%modifier/6 03-04-11
%%%%%%%%%%%%%%%%%%%%%%%
	/* modifier(+Type, +Quantity, +Power, +Tougness,
		-Modified_Power, -Modified_Toughness) */
	% Applies a modifier to a creature's power and toughness.

	% "gets" effect is already a list in MGL but we re-convert it here.
	% Do something!
	modifier(Type, [], Power, Toughness, Modified_P, Modified_T):-
		atom_to_list(Type, List),
		List = [Prefix_P,X,/,Prefix_T,Y],
%		Modified_P is Prefix_P(Power, X), %Swi, 18/06/11
%		Modified_T is Prefix_T(Toughness, Y)
		Power_mod =.. [Prefix_P, Power, X],
		Modified_P is Power_mod,
		Toughness_mod =.. [Prefix_T, Toughness, Y],
       		Modified_T is Toughness_mod.
                % ^ This sort of thing results in something like:
                % Modified_P is +(1,1), if Power_mod =.. [+, 1, 1]

	modifier(Type, Quantity, Power, Toughness, Modified_P, Modified_T):-
		atom_to_list(Type, List),
		List = [Prefix_P,X,/,Prefix_T,Y],
		Total_X is X * Quantity,
%		Modified_P is Prefix_P(Power, Total_X), % Swi,18/06/11
		Power_mod =.. [Prefix_P, Power, Total_X],
		Modified_P is Power_mod,
		Total_T is Y * Quantity,
%		Modified_T is Prefix_T(Toughness, Total_T)
		Toughness_mod =.. [Prefix_T, Toughness, Total_T],
		Modified_T is Toughness_mod.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%               Damage              %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%damage/3 30/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* damage(+Source_handle, +Recipient_handle, +Amount) */
	% Deals with damage to creatures, players and planeswalkers
	% Source: Name - Id; Recipient: Name-Id or Player

	% Changing from: deal_damage/3, remember to call-graph users.
	% Also remember to update calls passing Bestower as [Name - Id]
	damage(Source, Recipient, Damage):-
		damage_is_dealt(Source, Recipient, Damage, New_damage),
		damage_results(Source, Recipient, New_damage, Event),
		damage_event(Event).


%%%%%%%%%%%%%%damage_is_dealt/4 30/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* damage_is_dealt(+Source, +Recipient, +Damage, -Modified) */
	% Damage is dealt, as modified by effects that interact with it
	% Obviously, not yet impelemented.

	% Temp >
	damage_is_dealt(_Source, _Recipient, Damage, Damage):-
		damage_replacement_effects,
		damage_prevention_effects,
		damage_triggered_abilities.

	damage_prevention_effects.
	damage_replacement_effects.
	damage_triggered_abilities.


%%%%%%%%%%%%%%damage_results/4 30/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* damage_results(+Source, +Recipient, +Damage, -Event) */
	% Damage is transformed into its results

	damage_results(Source, Recipient, Damage, Event):-
		infect(Source, Recipient, Damage, Infect),
		wither(Source, Recipient, Damage, Infect, Wither),
		lifelink(Source, Recipient, Damage, Wither, Lifelink),
		damage(Source, Recipient, Damage, Lifelink, Event),
		damage_replacement_effects.
		% ^ more of those! This time they affect the results of dmg

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	damage_results is likely to change a lot, as I implement
	  more of the Comp rules (esp. continuous effects and triggered
	  abilities). Particularly, the way effects are applied to and
	  modify damage and its results is going to have to be implemented
	  more rigorously.
*/


%%%%%%%%%%%%%%infect/4 30/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* infect(+Source, +Recipient, +Damage, -Event) */
	% Calculates the results of damage from a Source with Infect.

%%%%%%%%%%%%%%infect/4 (1) 30/03/11

	% Damage to a player from a source with Infect causes them to gain
	%  that many poison counters
	infect(Source, Recipient, Damage, [Event]):-
		player(Recipient),
		object_template(Source, text_box, Abilities),
		member(['Infect'], Abilities),
		Event =.. [gain_poison, Recipient, Damage, _Poison].

%%%%%%%%%%%%%%infect/4 (2) 30/03/11

	% Damage to a creature from a source with Infect causes that many
	%  -1/-1 counters to be put on that creature.
	infect(Source, Recipient, Damage, [Event]):-
		creature(Recipient,_,_,_,_,_,_,Controller),
		object_template(Source, text_box, Abilities),
		member(['Infect'], Abilities),
		Event =.. [put_counter, Recipient, Controller, '-1/-1', Damage].

%%%%%%%%%%%%%%infect/4 (0) 30/03/11

	% Damage from a source without Infect does not result in infection.
	infect(_, _, _, []).


%%%%%%%%%%%%%%wither/4 30/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* wither(+Source, +Recipient, +Damage, +Infect, -Event) */
	% Calculates the results of damage from a Source with Wither.

%%%%%%%%%%%%%%wither/4 (1) 30/03/11

	% Damage to a creature from a source with Infect causes that many
	%  -1/-1 counters to be put on that creature.
	wither(Source, Recipient, Damage, [], [Event]):-
		creature(Recipient,_,_,_,_,_,_,Controller),
		object_template(Source, text_box, Abilities),
		member(['Wither'], Abilities),
		Event =.. [put_counter, Recipient, Controller, '-1/-1', Damage].
	% If the 4th argument, Infect is not [], then the source has Infect
	%  and the recipient is already taking damage as -1/-1 counters.

%%%%%%%%%%%%%%wither/4 (0) 30/03/11

	% Damage from a source without Wither does not result in withering.
	wither(_, _, _, Event, Event).


%%%%%%%%%%%%%%lifelink/4 30/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* lifelink(+Source, +Recipient, +Damage, +Wither, -Event) */
	% Calculates the results of damage from a Source with Lifelink.

%%%%%%%%%%%%%%lifelink/4 (1) 30/03/11

	% Damage from a source with Lifelink also causes that source's
	%  controller to gain that much life.
	lifelink(Source, _Recipient, Damage, Wither, Event):-
		object_template(Source, text_box, Abilities),
		member(['Lifelink'], Abilities),
		object_controller(Source, Controller),
		Lifegain =.. [gain_life, Controller, Damage, _New_life],
		append(Wither, [Lifegain], Event).

%%%%%%%%%%%%%%lifelink/4 (0) 30/03/11

	% Damage from a source without lifelink does not result in lifegain.
	lifelink(_, _, _, Event, Event).


%%%%%%%%%%%%%%damage/5 30/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/*damage(+Source, +Recipient, +Amount, +Lifelink, -Event)*/
	% Calculates the results of normal damage

%%%%%%%%%%%%%%damage/5 (1) 30/03/11

	% The Source does not have Infect or Wither: damage
	%  results in a damage amount marked on a creature,
	%  loss of Loyalty or loss of Life.
	damage(Source, Recipient, Damage, Lifelink, Event):-
		\+ member(gain_poison(Recipient,Damage,_Poison), Lifelink),
		\+ member(put_counter(Recipient, _Controller, '-1/-1', Damage), Lifelink),
		Deal =.. [deal_damage, Source, Recipient, Damage],
		append(Lifelink, [Deal], Event).
	% Case: no Infect, no Wither; Recipeint: creature or player

%%%%%%%%%%%%%%damage/5 (2) 30/03/11

	% The Source does not have Infect: damage to players results
	%  in loss of Life.
	damage(Source, Recipient, Damage, Lifelink, Event):-
		player(Recipient),
		\+ member(gain_poison(Recipient,Damage,_), Lifelink),
		Deal =.. [deal_damage, Source, Recipient, Damage],
		append(Lifelink, [Deal], Event).
	% Case: no Infect; Recipient: player

%%%%%%%%%%%%%%damage/5 (3) 30/03/11

	% The Source has Infect or Wither: damage to creatures
	%  or players does not result in damage being marked on them
	%  or loss of Life.
	damage(_Source, _Recipient, _Damage, Event, Event):-
		member(gain_poison(_,_,_), Event);
		member(put_counter(_, _,_,_), Event).
	% Case: Infect or Wither; Recipient: creature (both) or player (Infect)


%%%%%%%%%%%%%%damage_event/1 30/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* damage_event(+Event) */
	% The damage event occurs

	damage_event([]).
	damage_event([Event | Rest]):-
		Event,
		damage_event(Rest).
	% I think only two results will fire:
	%  either a counter and lifegain, or
	%  damage and lifegain, but not both
	%  counters and damage together


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%deal_damage/3 05/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* deal_damage(+Bestower, +Recipient, +Damage) */
	% Deals an amount of Damage to a Recipient: a player,
	%   planeswalker or creature.

%%%%%%%%%%%%%%deal_damage/3 (1) 30/03/11

	% Damage dealt to a creature is marked on the creature

	deal_damage(Source, Recipient, Damage):-
		creature(Recipient, State, _Cc,_Ty,_Ab,_P,_T, Controller),
		(member(damage(Amount), State); Amount = 0 ),
		update_damage(Recipient, Controller, Amount, Damage),
		output(deal_damage, [creature, Source, Recipient, Damage]).

%%%%%%%%%%%%%%deal_damage/3 (2) 05/03/11

	% Damage to a player causes loss of life.
	deal_damage(Source, Recipient, Damage):-
		player(Recipient),
		lose_life(Recipient, Damage, _Remaining),
		output(deal_damage, [player, Source, Recipient, Damage]).


%%%%%%%%%%%%%%update_damage/5 05/03/11
%%%%%%%%%%%%%%%%%%%%%%%
	/*update_damage(+Creature, +Controller, +Existing, +New)*/
	% Updates the damage on a creature.

	update_damage(Creature, Controller, Existing_dmg, New_dmg):-
		Total is Existing_dmg + New_dmg,
		(remove_state(Creature, damage(Existing_dmg), Object);
		Object = Creature),
		add_state(Object, damage(Total), Final_state),
		change_state(Creature, 'Battlefield', Controller, Final_state).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lethal_damage/2 05/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/*lethal_damage(+Creature, +Damage)*/
	% True when the a creature has lethal damage on it.

/*	lethal_damage(Creature, Damage):-
		object_handle(Creature, Handle),
		object_state(Handle, State),
		\+ member(damage(_Damage), State).
*/

%%%%%%%%%%%%%%lethal_damage/3 (1) 05/03/11

	lethal_damage(Creature, Damage):-
		object_handle(Creature, Handle),
		creature(Handle, State, _Cc,_Ty,_Ab,_P,Toughness,_Ctr),
		% Local cut or creatures with no damage crash and burn
		%  (removed on?). On 26/04, creature/6 caused problems with
		%  "gets" effects.
		member(damage(Damage), State),
		Damage >= Toughness.

%%%%%%%%%%%%%%lethal_damage/3 (2) 05/03/11

	% Reference is an Object
	lethal_damage(Creature, Damage):-
		creature(Creature, State, _Cc,_Ty,_Ab,_P,Toughness,_Ctr),
		member(damage(Damage), State),
		Damage >= Toughness.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%            Switches               %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%prompt_switches/2 03/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* prompt_switches(+Atoms, -Switches) */
	% Takes in a list of Atoms and returns a map of Atom - switch pairs
	%  and a list of Atoms with their switches enclosed in square brackets
	%  ie a list of Switches.

	prompt_switches(Atoms, [Map, Switches]):-
		map_switches(Atoms, Map),
		prompt_switches(Map, [], Switches), !.

%%%%%%%%%%%%%%prompt_switches/3 03/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* prompt_switches(+Atoms, [], -Switches) */
	% Business goals of prompt_switches/2

%%%%%%%%%%%%%%prompt_switches/3 (0) 03/02/11

	prompt_switches([], Switches, Switches).

%%%%%%%%%%%%%%prompt_switches/3 (1) 03/02/11

	prompt_switches(Atoms, Temp, Switches):-
		Atoms= [Atom - Switch | Rest_atoms],
		( type(Atom, 3) -> atom_codes(Atom, Atom_codes);
		type(Atom, 1) -> number_codes(Atom, Atom_codes) ),
		( type(Switch, 3) -> atom_codes(Switch, [Switch_codes]);
		type(Switch, 1) -> number_codes(Switch, Switch_codes) ),
		([SWitch_codes] = Switch_codes; SWitch_codes = Switch_codes ),
		% ^ Oh Prolog... why dost thou hurtst me thusly? 0_o@
		bracket(SWitch_codes, Atom_codes, Bracketed),
		append(Temp, [Bracketed], New_temp),
		prompt_switches(Rest_atoms, New_temp, Switches).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%bracket/3 03/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/*  bracket(+Switch, +Atom, -Bracketed) */
	% Encloses the given Switch in square brackets.

%%%%%%%%%%%%%%bracket/3 (1) 03/02/11

	bracket(Switch, Atom, Bracketed):-
		member(Switch, Atom, Position),
		insert_at(91, Atom, Position, Result),
		P1 is Position + 2,
		insert_at(93, Result, P1, Result_2),
		atom_codes(Bracketed, Result_2).

%%%%%%%%%%%%%%bracket/3 (2) 03/02/11

	bracket(Switch, Atom, Bracketed):-
		append(Atom, [91, Switch, 93], Codes),
		atom_codes(Bracketed, Codes).


%%%%%%%%%%%%%%insert_at/4 03/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* insert_at(?Element, ?List, +Position, ?Result).*/

	insert_at(X,L,K,R):-
		remove_at(X,R,K,L).

%%%%%%%%%%%%%%remove_at/4 03/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* remove_at(?Element, ?List, +Position, ?Result) */

	remove_at(X,[X|Xs],1,Xs).
	remove_at(X,[Y|Xs],K,[Y|Ys]) :- K > 1,
	   K1 is K - 1, remove_at(X,Xs,K1,Ys).

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	insert_at and remove_at are
	  taken from "99 Prolog problems,
	  at http://sites.google.com/site/prologsite/home
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%map_switches/2 03/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* map_switches(+Atoms, -Map) */
	% Takes in a list of Atoms and returns a map of
	%  'Atom - Switches' pairs.

	map_switches(Atoms, Map):-
		switches(Atoms, Switches),
		map_switches(Atoms, Switches, [], Map).


%%%%%%%%%%%%%%map_switches/4 03/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* map_switches(+Atoms, Switches, [], -Map) */
	% Business goal for top-level goal map_switches/2.

%%%%%%%%%%%%%%map_switches/4 (0) 03/02/11

	map_switches([], [], Map, Map).

%%%%%%%%%%%%%%map_switches/4 (1) 03/02/11

	map_switches(Atoms, Switches, Temp, Map):-
		Atoms = [Atom | Rest_atoms],
		Switches = [Switch | Rest_switches],
		append(Temp, [Atom - Switch], New_temp),
		map_switches(Rest_atoms, Rest_switches, New_temp, Map).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%switches/2 03/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* switches(+Atoms, -Switches) */
	% Takes in a list of Atoms and returns a list of unique Switches
	% as character codes

	switches(Atoms, Switches):-
		switches(Atoms, [], Codes),
		atom_codes(Chars, Codes),
		atom_to_list(Chars, Switches), !.

%%%%%%%%%%%%%%switches/3 03/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* switches(+Atoms, [], -Switches) */
	% Generates the list of Switches' character codes.

%%%%%%%%%%%%%%switches/3 (0) 03/02/11

	switches([], Switches, Switches).

%%%%%%%%%%%%%%switches/3 (1) 03/02/11

	switches([Atom | Rest], [], Temp):-
		( type(Atom, 3) -> atom_codes(Atom, Chars);
		type(Atom, 1) -> number_codes(Atom, Chars) ),
		member(Char, Chars),
		append([], [Char], New_switches),
		switches(Rest, New_switches, Temp).

%%%%%%%%%%%%%%switches/3 (2) 03/02/11

	switches([Atom | Rest], Switches, Temp):-
		( type(Atom, 3) -> atom_codes(Atom, Chars);
		type(Atom, 1) -> number_codes(Atom, Chars) ),
		member(Char, Chars),
		not member(Char, Switches),
		append(Switches, [Char], New_switches),
		switches(Rest, New_switches, Temp).

%%%%%%%%%%%%%%switches/3 (3) 03/02/11

	switches([Atom | Rest], Switches, Temp):-
		( type(Atom, 3) -> atom_codes(Atom, Chars);
		type(Atom, 1) -> number_codes(Atom, Chars) ),
		member(Char, Chars),
		Char >= 0'a, Char =< 0'z,
		Upper_char is Char - 32,
		not member(Upper_char, Switches),
		append(Switches, [Upper_char], New_switches),
		switches(Rest, New_switches, Temp).

%%%%%%%%%%%%%%switches/3 (4) 03/02/11

	switches([_Atom | Rest], Switches, Temp):-
		(setof(Char, integer_bound(0'a, Char, 0'z), Codes);
		setof(Char, integer_bound(0'A, Char, 0'Z), Codes)),
		member(Char, Codes),
		not member(Char, Switches),
		append(Switches, [Char], New_switches),
		switches(Rest, New_switches, Temp).

%%%%%%%%%%%%%%switches/3 (5) 03/02/11

	switches(_Atoms, _Switches, _Temp):-
		write(`It seems I'm out of letters to assign as switches :(`), nl, !.




















