% First saved: 10/01/11
% Last saved:  10/04/11
%
%	Operators for Card database etc.
%
% Doing:
%
% Todo
%
% NOTES:


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%          M:tG operators           %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	% card database operators
	:- op(100, fx, card_name).
	:- op(100, fx, card_copy).	% unique ID, not used
	:- op(100, fx, mana_cost).
	:- op(100, fx, illustration). % for the GUI
	:- op(100, fx, expansion). 	% just in case
	:- op(100, fx, type_line).
	:- op(100, fx, text_box).
	:- op(100, fx, power).
	:- op(100, fx, toughness).
	:- op(100, fx, loyalty).
	:- op(100, fx, state).		% Status and damage, counters etc, not currently used.

	% inference engine operators
	:- op(910, fx, if).
	:- op(905, xfy, then).
	:- op(860, xf, present).
	:- op(740, xf, identified).
	:- op(740, xf, eliminated).
	:- op(760, xfy, with).
	:- op(710, xf, certainty).
	:- op(900, xfy, or).
	:- op(900, xfy, and).
	:- op(500, yfx, blocks).

	% ability operators. Not currently used
/*%	:- op(100, fx, keyword).
	:- op(100, fx, modes).		% "mode" is a built-in operator
	:- op(100, fx, condition).	% For triggered abilities
	:- op(100, fx, cost).
	:- op(100, fx, recipients).	% targets or not
	:- op(100, fx, effect).
	:- op(100, fx, duration).
	%:- op(1000, xfx, [when, whenever, at]).
	%:- op(1000, yfx, [:, -]).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%     Operators Implementations     %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	% Conjunction
	and(A,B):- A,B.
	% Disjunction
	or(A,B):- A;B.






