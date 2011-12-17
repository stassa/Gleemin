
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%         Swi Compatibility         %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	:- set_prolog_flag(backquoted_string,true).
	% Swi LPA backtic string compatibility.
	:- style_check(-singleton).
	% Stop Swi warning on singleton vars.
	:- style_check(-discontiguous).
	% Stop Swi warning on discontiguous clauses.

     :-op(900, fy, not).
     not(Goal):-
	\+ Goal.

     one(Goal):- once(Goal).

     list(Term):- is_list(Term).

     removeall(Element,List1,List2):-
	          delete(List1,Element,List2).

     remove(Element, List, Rest):-
                    select(Element,List,Rest).

     member(Element, List, Position):-
	          nth1(Position, List, Element).


% From: http://www.syntax-example.com/Code/prolog-find-sublists-given-list-3152.aspx

     sublist([],[]).

     sublist([Head|Sublist],[Head|Tail]):-
          sublist(Sublist,Tail).

     sublist(Sublist,[_|Tail]):-
          sublist(Sublist,Tail).



     integer_bound(Lower, Number, Upper):-
          between(Lower, Upper, Number).


     % Should work as long as Prolog iso
     %  flag is set to false
     len(Term, Length):-
	(  atom(Term) ; string(Term) ),
	atom_length(Term, Length),!.
        % Cut for determinism.

     len(Term, Length):-
	is_list(Term),
	length(Term, Length),!.

     len(Term, Length):-
	compound(Term),
	Term =.. List,
	length(List, Length),!.

     len(Term, Length):-
	number(Term),
	number_chars(Term, List),
	length(List, Length).



     number_atom(Number, Atom):-
	atom_number(Atom, Number).

/*
      list_read(Input):-
	get_input(_Char, [], Input).

      get_input(Char, Input, Input):-
           \+ var(Char),
	   Char = 10.
      get_input(Char, Temp, Input):-
	get(Char),
	append(Temp,[Char],New_temp),
	get_input(Char, New_temp, Input).
*/

% This does more or less the same as LPA:fread(a,0,-1,Input)
/*     list_read(Input):-
          read_line_to_codes(current_input, Codes),
	  atom_codes(Input, Codes).
*/

/*
Not needed...
     list_read(Input, List):-
	read_line_to_codes(current_input, Input ),
	codes(Input, [], List).

     codes([], List, List).
     codes([Code|Rest], Temp, List):-
	char_code(Atom, Code),
	append(Temp, [Atom], New_temp),
	codes(Rest, New_temp, List).
*/

% This is missing the last two type checks from LPA: concjunction
%  and disjunction, since they are absent from Swi.
% Using numbers like this is a bit hard to read (though you get used
%  to it eventually) so I'd better turn them to atoms at some point,
%  eg, 2 --> float etc. Currently there are many calls to type/2 in the
%  source so it would only cause more errors.

     type([], 5):- !.

     type(Term, Type):-
	(
	(   Type = 0 -> var(Term));
	(   Type = 1 -> integer(Term));
	(   Type = 2 -> float(Term));
	(   Type = 3 -> atom(Term));
	(   Type = 4 -> string(Term));
	(   Type = 5 -> Term = []);
	(   Type = 6 -> is_list(Term));
	(   Type = 7 -> compound(Term))), !.

     type(Term, Type):-
	(   var(Term) -> Type = 0 );
	(   integer(Term) -> Type = 1 );
	(   float(Term) -> Type = 2 );
	(   atom(Term) -> Type = 3 );
	(   string(Term) -> Type = 4 );
	(   is_list(Term) -> Type = 6 );
	(   compound(Term) -> Type = 7 ).


       invert_case(Lower, Upper):-
	       var(Upper) -> upcase_atom(Lower, Upper);
	       var(Lower) -> downcase_atom(Upper, Lower);
	       Lower = Upper.














