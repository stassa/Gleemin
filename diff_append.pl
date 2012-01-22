    diff_append(As-Bs, Bs-Cs, As-Cs).
/*
Usage:
?- diff_append([a,b|X]-X,[c,d]-[], Y-[]).
X = [c, d],
Y = [a, b, c, d]

Eg: diff_append([the, bird | X]-X, [flies, high]-[], Full).
Eg: diff_append([Article, Noun | X]-X, [Verb,Determiner]-[], [the,bird,flies,high]-[]).

Generate a diff list from a single element (which is easy to turn
into a diff-list):

?- diff_append(X-X, [a|Y]-Y, Z-A).
X = [a|A],
Y = A,
Z = [a|A].

% remember that dcgs create diff lists:

    diff_list --> [a,b,c].

?- diff_list(A,B).
A = [a, b, c|B]

?- diff_list(A,B), diff_append(A-B, [d|C]-C, X-Y).
A = [a, b, c, d|Y],
B = [d|Y],
C = Y,
X = [a, b, c, d|Y].
*/

    diff_list([]) --> [].
    diff_list([H|T]) --> [H], diff_list(T).

/*
Usage:

?- diff_list([a,b,c],A, B).
A = [a, b, c|B].

7 ?- diff_list([a,b,c],A, B), diff_append(A-B, [d,e,f|Y]-[], Z-[]).
A = [a, b, c, d, e, f|Y],
B = [d, e, f|Y],
Z = [a, b, c, d, e, f|Y].

8 ?- diff_list([a,b,c],A, B), diff_append(A-B, [d,e,f]-[], Z-[]).
A = [a, b, c, d, e, f],
B = [d, e, f],
Z = [a, b, c, d, e, f].

But this is O(N). So it doesn't save you that much time. Is there no
easy way to create a diff list from an ordinary list?
*/

    dcg_append(X) --> [X].

/*
Usage:
    dcg_append([X|Z], [Y|Z], Z).

Where X is a difference list with tail Z and Z is any list. For
example:

?- dcg_append([a,b,c|Y], [X|Y], [d,e,f]).
Y = [d, e, f],
X = [a, b, c, d, e, f].

You don't call it with phrase/2 or /3 then- the idea is to use the dcg
rule expansion to unify the difference list at the input with the list
at the last argument.

Note that this is not more efficient than diff_append: the dcg rule must
first be expanded, which of course takes some additional time. In fact,
it looks like it takes quite some time to do it...

    */

/*
This should be efficient _if_ dcg rule expansion is efficient. Which I
bet it isn't.

*/

    dcglst(L) --> L.

/*
Usage:
?- dcglst([a,b,c], A, B).
A = [a, b, c|B].
*/

/*
This no good:

21 ?- L = [a,b,c], Diff =.. [., L, Y].
L = [a, b, c],
Diff = [[a, b, c]|Y].

*/






