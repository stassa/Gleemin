% First saved: 24/02/2011
% Last saved: 28/04/2011
%
%	Status:
%
%
%
% Doing:
%	Major overhaul: Adding definitions of targeted_ability, 
%		see notes in abilities syntaxt.txt [28/04/11]
%	Implementing more effects:
%		Done Destroy target X
%		Done Destroy target X or Y 
%			Done syntax (also for "and"); could generalise.
%		Done Destroy target <condition> X 
%	noticed that burn effect sends only name of source
%	(also needs Id). The reason is I'm not actually associating
%	the source of an effect with the effect (this/1 does recognise
%	a permanent with an ability that refers to itself, so use it)
% Todo
%	Fix targeting (uncontrolled backtracking)
%	  and update to current version again. 
%
% NOTES:
%	Handles both abilities and effects.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%          Abilities Syntax         %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ability 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*	An ability may specify a set of recipients:
	 game entities affected by the ability 
	 (ie, their state is modified by the ability)
	Rrecipients are not necessarily targets- abiliies
	 can affect game entities without targeting them. 
	Some abilities (notably, mana abilities) don't 
	  specify any affected entities. Their effects
	  usually determine the game entities affected. 
	  For example, a mana ability will affect its controller
	  and that player's mana pool and First Strike will affect
	  the rules of combat. Those recipients are not explicitly
	  referenced in the abilities' (grammatical) definitions, 
	  although the Comp. Rules will usually implicitly name 
	  all such recipients. 
*/
	ability --> ability(_Recipients).

	ability(Recipients) --> spell_ability(Recipients).
	ability(Recipients) --> activated_ability(Recipients).
	ability(Recipients) --> triggered_ability(Recipients).
	ability(Recipients) --> static_ability(Recipients).
	ability(Recipients) --> ability_word, [-], ability(Recipients).
	ability(Recipients) --> keyword_ability(Recipients).
	ability --> keyword_ability.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%spell_ability 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	spell_ability(Recipients) --> effect(Recipients).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%activated_ability 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	activated_ability(Recipients) --> cost, [:], effect(Recipients).
	activated_ability(Recipients) --> cost, [:], effect(Recipients), activation_instructions.
	activated_ability --> mana_ability. % mana abilities don't have recipients as such
	%%activated_ability --> loyalty_ability.
	activated_ability(Recipients) --> keyword_ability(Recipients).
	% ^ This will need further definition- not all keyword abilities
	%  are activated.  Basically, each keyword ability that is an 
	%  activated ability should be defined separately with its 
	%  activated form. 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%triggered_ability 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	% triggered_ability: non-terminals
	triggered_ability(Recipients) --> trigger_condition, [','], effect(Recipients).
	triggered_ability(_Recipients) --> mana_ability.
	triggered_ability(Recipients) --> keyword_ability(Recipients).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%static_ability 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	static_ability(Recipients) --> keyword_ability(Recipients).
	static_ability(Recipients) --> effect(Recipients).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ability_word 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	ability_word --> ['Channel'].
	ability_word --> ['Chroma'].
	ability_word --> ['Domain'].
	ability_word --> ['Grandeur'].
	ability_word --> ['Hellbent'].
	ability_word --> ['Imprint'].
	ability_word --> ['Kinship'].
	ability_word --> ['Landfall'].
	ability_word --> ['Metalcraft'].
	ability_word --> ['Radiance'].
	ability_word --> ['Sweep'].
	ability_word --> ['Threshold'].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%keyword_ability 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	keyword_ability(_X) --> keyword_ability. 

	keyword_ability --> ['Deathtouch'].
	keyword_ability --> ['Defender'].
	keyword_ability --> ['Defender'].
	keyword_ability --> ['Double Strike'].
	keyword_ability --> ['Enchant'].
	keyword_ability --> ['Equip'].
	keyword_ability --> ['First Strike'].
	keyword_ability --> ['Flash'].
	keyword_ability --> ['Flying'].
	keyword_ability --> ['Haste'].
	keyword_ability --> ['Intimidate'].
	keyword_ability --> ['Landwalk'].
	keyword_ability --> ['Lifelink'].
	keyword_ability --> ['Protection'].
	keyword_ability --> ['Reach'].
	keyword_ability --> ['Shroud'].
	keyword_ability --> ['Trample'].
	keyword_ability --> ['Vigilance'].
	keyword_ability --> ['Banding'].
	keyword_ability --> ['Rampage'].
	keyword_ability --> ['Cumulative Upkeep'].
	keyword_ability --> ['Flanking'].
	keyword_ability --> ['Phasing'].
	keyword_ability --> ['Buyback'].
	keyword_ability --> ['Shadow'].
	keyword_ability --> ['Cycling'].
	keyword_ability --> ['Echo'].
	keyword_ability --> ['Horsemanship'].
	keyword_ability --> ['Fading'].
	keyword_ability --> ['Kicker'].
	keyword_ability --> ['Flashback'].
	keyword_ability --> ['Madness'].
	keyword_ability --> ['Fear'].
	keyword_ability --> ['Morph'].
	keyword_ability --> ['Amplify'].
	keyword_ability --> ['Provoke'].
	keyword_ability --> ['Storm'].
	keyword_ability --> ['Affinity'].
	keyword_ability --> ['Entwine'].
	keyword_ability --> ['Modular'].
	keyword_ability --> ['Bushido'].
	keyword_ability --> ['Soulshift'].
	keyword_ability --> ['Splice'].
	keyword_ability --> ['Offering'].
	keyword_ability --> ['Ninjutsu'].
	keyword_ability --> ['Epic'].
	keyword_ability --> ['Convoke'].
	keyword_ability --> ['Dredge'].
	keyword_ability --> ['Transmute'].
	keyword_ability --> ['Bloodthirst'].
	keyword_ability --> ['Haunt'].
	keyword_ability --> ['Replicate'].
	keyword_ability --> ['Forecast'].
	keyword_ability --> ['Graft'].
	keyword_ability --> ['Recover'].
	keyword_ability --> ['Ripple'].
	keyword_ability --> ['Split Second'].
	keyword_ability --> ['Suspend'].
	keyword_ability --> ['Vanishing'].
	keyword_ability --> ['Absorb'].
	keyword_ability --> ['Aura Swap'].
	keyword_ability --> ['Delve'].
	keyword_ability --> ['Fortify'].
	keyword_ability --> ['Frenzy'].
	keyword_ability --> ['Gravestorm'].
	keyword_ability --> ['Poisonous'].
	keyword_ability --> ['Transfigure'].
	keyword_ability --> ['Champion'].
	keyword_ability --> ['Changeling'].
	keyword_ability --> ['Evoke'].
	keyword_ability --> ['Hideaway'].
	keyword_ability --> ['Prowl'].
	keyword_ability --> ['Reinforce'].
	keyword_ability --> ['Conspire'].
	keyword_ability --> ['Persist'].
	keyword_ability --> ['Wither'].
	keyword_ability --> ['Retrace'].
	keyword_ability --> ['Devour'].
	keyword_ability --> ['Exalted'].
	keyword_ability --> ['Unearth'].
	keyword_ability --> ['Cascade'].
	keyword_ability --> ['Annihilator'].
	keyword_ability --> ['Level Up'].
	keyword_ability --> ['Rebound'].
	keyword_ability --> ['Totem Armor'].
	keyword_ability --> ['Infect'].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%mana_ability 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	mana_ability --> [mana].
%	mana_ability --> [add,Mana,'to your mana pool'].
	%mana_ability --> ['add'],mana,['to', 'your', 'mana', 'pool'].

/*	mana_ability --> cost, [:], ['add'],mana,['to', 'your', 'mana', 'pool'].
	% Triggered mana ability definitions are probably not correct, see 605.1b
	mana_ability --> trigger_word, trigger_event, ['add'],mana,['to', 'your', 'mana', 'pool'].
	mana_ability --> trigger_word, trigger_event(X), ['add'],mana,['to', 'your', 'mana', 'pool'].
*/	

/*	mana_ability --> cost, [:], ['add'],mana,['to'], player(Player), ['mana', 'pool'].
	% Triggered mana ability definitions are probably not correct, see 605.1b
	mana_ability --> trigger_word, trigger_event, ['add'],mana,['to'], player(Player), ['mana', 'pool'].
	mana_ability --> trigger_word, trigger_event(X), ['add'],mana,['to'], player(Player), ['mana', 'pool'].
*/

	mana_ability --> cost, [:], [add],mana,[to], [your, mana, pool].
	% Triggered mana ability definitions are probably not correct, see 605.1b
	mana_ability --> trigger_word, trigger_event, ['add'],mana,['to'], your(X), ['mana', 'pool'].
	mana_ability --> trigger_word, trigger_event(X), ['add'],mana,['to'], your(Y), ['mana', 'pool'].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%loyalty_ability 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%%loyalty_ability --> [loyalty Loyalty].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%cost 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	cost --> mana_cost.
	cost --> [tap].
	% ^ Will need to deal with "tap a permanent you control" type
	%  of costs!
	cost --> mana_cost, [tap].
	cost --> [tap], mana_cost.

	mana_cost --> mana.
%	mana_cost --> mana(X).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%condition 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	% condition: non-terminals
	trigger_condition --> trigger_word, trigger_event(_X).
	trigger_condition --> trigger_word, trigger_event.
	trigger_condition --> trigger_word, trigger_event, [if], condition.
	% ^ ie, "When/Whenever/At [trigger event], if [condition], [effect]."

	% condition: terminals
	trigger_word --> ['When'];['Whenever'];['At'].
	%^ needs further disambiguation, otherwise it may accept 
	%  "At enters the battlefield".

	trigger_event(X) --> [X], ['enters', 'the', 'Battlefield'].
	trigger_event --> [trigger, event, placeholder].

	condition --> target(X), [controls], {player(X)}.
	%condition --> [condition, placeholder].
	/*| ?- phrase(triggered_ability, 
		['When',trigger,event,placeholder,
		if,condition, placeholder,(','),
		'Tap',target,'Creature'] ).
	yes*/
% Not sure about those above, try phrase(triggered_ability, X). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%effect 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	effect(Target) --> return(Target).
	effect(Target) --> return(Target,Type).
	%effect(Target) --> tap(Target, Type).
	%effect(All) --> tap(All, Type).
	effect(Target) --> tap(Target, Type, Keyword).
	effect(Target) --> untap(Target,Type).
	%effect(All) --> untap(All, Type).
	effect(Target) --> destroy(Target,Type).
	%effect(All) --> destroy(All,Type).
	effect(Target) --> destroy(_nosuch, Target).
	effect(Target) --> destroy(Target,_nosuch).
	effect([Target1,Target2]) --> destroy(Target1, Type1, Target2, Type2). 
	effect(All) --> regenerate(All,Type).
	effect(All) --> exile(All,Type).
	effect(All) --> deals_to_all(Source,Damage,All,Type).
	effect(Each) --> deals_to_each(Source, Damage, Each, Type).
	effect(Each) --> deals_to_each(Source,Damage,Each,Type1,Type2).
	effect(Target) --> deals(Source,Damage,[],Target).
	effect(Target) --> deals(Source,Damage,Target,[]).
	effect(Target) --> deals(Source,Damage,Target,Type).
	effect(Target_player) --> deals(Source,Damage,Target_player).
	effect(Target) -->  gets(Target,Sign1,P,Sign2,B).
	effect(This) --> gains(This).
	effect(Target) --> gains(Target,Type).
	effect([Player,Permanent]) --> gains(Player, Permanent, Type).
	effect([You,Permanent]) --> gain(You,Permanent,Type).
	effect(Target) --> gains(Target,Counters,_termination).
	effect(You) --> gain(You,Counters,_termination).
	effect(Target) --> put(Amount,Sign1,P,/,Sign2,T,Target,Type).


	% Use to find/ validate an effect's target(s).
%	effect --> effect(_Target).
/*	effect(Target) --> effect(Target,_Y).
	effect(Target) --> effect(Target,_Y,_Z).
	effect(Target) --> effect(_X,_Y,Target,_A).
	effect(Target) --> effect(_X,_Y,_Z,Target).
	effect(Target) --> effect(_X,_Y,Target,_A,_B).
	effect(Target) -->  effect(Target,_Y,_Z,_A,_B).
	effect(Target) --> effect(_X,_Y,_Z,_A,_B,_C,Target,_E).
*/
	% Use to determine what is an effect
/*	effect --> effect(_X,_Y).
	effect --> effect(_X,_Y,_Z).
	effect --> effect(_X,_Y,_Z,_A).
	effect --> effect(_X,_Y,_Z,_A,_B).
	effect --> effect(_X,_Y,_Z,_A,_B,_C,_D,_E).
*/
/*	effect(X,Y) --> return(X,Y).
	effect(X) --> return(X).
	effect(X,Y) --> tap(X,Y).
	effect(X,Y,Z) --> tap(X,Y,Z).
	effect(X,Y) --> untap(X,Y).
	effect(X,Y) --> destroy(X,Y).
	effect(X,Y) --> regenerate(X,Y).
	effect(X,Y) --> exile(X,Y).
	effect(X,Y,Z,A) --> deals(X,Y,Z,A).
	effect(X,Y,Z) --> deals(X,Y,Z).
	effect(X,A,B,C,D) --> gets(X,A,B,C,D).
	effect(X) --> gains(X).
	effect(X,Y) --> gains(X,Y).
	effect(X,Y,Z) --> gains(X,Y,Z).
	effect(X,Y,Z) --> gain(X,Y,Z).
	effect(X,Y,A,S,Z,B,D,E) --> put(X,Y,A,S,Z,B,D,E).	
*/
	% effects

	% effect(Target) --> return(Target)
	return(X) --> (['Return', target] ; [return, target]), 
		permanent(X), [to, its, 'owner''s', hand].
	/*| ?- phrase(return(X), Z).
	X = copy('Goblin Balloon Brigade',1,[]) ,
	Z = ['Return',target,permanent,to,its,'owner''s',hand] ;*/

	% effect(Target) --> return(Target,Type).
	return(X,Y) --> (['Return'] ; [return]),  
		target(X,Y), [to, its, 'owner''s', hand], {permanent_Type(Y)}.
		%{Y = 'Artifact';
		%Y = 'Creature';
		%Y = 'Enchantment';
		%Y = 'Land';
		%Y = 'Planeswalker'}.
	/*| ?- phrase(return(X,Y), Z).
	X = 'Goblin Balloon Brigade' ,
	Y = 'Creature' ,
	Z = ['Return',target,'Creature',to,its,'owner''s',hand] ; */


	% Tap target permanent of a specified type with a given keyword ability
	% effect(Target) --> tap(Target, Type, Keyword).
	tap(X,Y,Z) --> (['Tap'];[tap]), target(X,Y,Z), (condition ; []),{permanent_Type(Y)}.
	% Tap target permanent of a specified type
	% effect(Target) --> effect(Target, Type).
	tap(X,Y) --> (['Tap'];[tap]), target(X,Y), (condition ; []),{permanent_Type(Y)}.
	% Tap all of a type of permanent
	% eeffect(All) --> tap(All, Type).
	tap(X,Y) --> (['Tap'];[tap]), all(X,Y), (condition ; []).
	% ^ No need to specify permanents here (it's done in "all").

	% Untap target permanent of a specified type
	%effect(Target) --> untap(Target,Type).
	untap(X,Y) --> (['Untap'];[untap]), target(X,Y), {permanent_Type(Y)}.
	% Untap all
	% effect(Target) --> untap(All, Type).
	untap(X,Y) --> (['Untap'];[untap]), all(X,Y).

	% Destroy target permanent ...
	% effect(Target) --> destroy(Target,Type).
	destroy(X,Y) --> (['Destroy'];[destroy]), target(X,Y), {permanent_Type(Y)}.
	% Destroy all...
	% effect(Target) --> destroy(All,Type).
	destroy(X,Y) --> (['Destroy'];[destroy]), all(X,Y).

	% effect(Target) --> destroy(_nosuch, Target).
	destroy(X,Y) --> (['Destroy'];[destroy]), [target,X,or], target(Y),
		{ permanent_Type(X), phrase(target(Y), [Z]), permanent_Type(Z) }.
		% ^ Will have problem with complex types. Try member(T,Z), permanent_Type(T)
		%  but causes uncontrolled backtracking
	% effect(Target) --> destroy(Target,_nosuch).
	destroy(X,Y) --> (['Destroy'];[destroy]), target(X), ([or,Y] ; [or,target,Y] ), 
		{ permanent_Type(Y), phrase(target(X), [Z]), permanent_Type(Z) }.
	/*| ?- phrase(destroy(X,Y), ['Destroy',target,'Artifact',or,target,'Land']).
	X = 'Artifact' ,
	Y = object('Mountain' - 2,[]) ; no
	| ?- phrase(destroy(X,Y), ['Destroy',target,'Creature',or,'Land']).
	X = object('Goblin Balloon Brigade' - 1,[]) ,
	Y = 'Land' ;
	X = 'Creature' ,
	Y = object('Mountain' - 2,[]) ; no */

/*
	^ That can't be done, for example, like this:
		destroy(X,Y,Z,A) --> destroy(X,Y), [or], destroy_more(Z,A). 
		destroy_more(X,Y),[destroy, target] --> destroy(X,Y). 
		destroy_more(X,Y),['Destroy', target] --> destroy(X,Y). 
	- because "or" should be true even if only one target of the 
	  specified type is present. Just putting an "or" between two 
	  clauses that both require a target won't do. On the other 
	  hand, I believe that's the way to do "and".
	I should probably define a general-case "and" and "or" terminal.  
*/	
	% effect([Target1,Target2]) --> destroy(Target1, Type1, Target2, Type2). 
	destroy(X,Y,Z,A) --> destroy(X,Y), [and], destroy_more(Z,A), { Y\=A }. 
	destroy_more(X,Y),[destroy] --> destroy(X,Y). 
	destroy_more(X,Y),['Destroy'] --> destroy(X,Y). 
	/*| ?- phrase(destroy(X,Y,Z,A), B).
	X = object('Goblin Balloon Brigade' - 1,[]) ,
	Y = 'Creature' ,
	Z = object('Mountain' - 2,[]) ,
	A = 'Land' ,
	B = ['Destroy',target,'Creature',and,target,'Land'] ;*/


	% Regenerate...
	% effect(Target) --> regenerate(Target,Type).
	regenerate(X,Y) --> (['Regenerate'];[regenerate]), target(X,Y), {permanent_Type(Y)}.
	% ...all
	% effect(All) --> regenerate(All,Type).
	regenerate(X,Y) --> (['Regenerate'];[regenerate]), all(X,Y).

	% Exile... 
	% effect(Target) --> exile(Target,Type).
	exile(X,Y) --> (['Exile'];[exile]), target(X,Y), {permanent_Type(Y)}.
	% ...all
	% effect(All) --> exile(All,Type). Should be each
	exile(X,Y) --> (['Exile'];[exile]), all(X,Y).

	
	% Damage to all Z of type A - this doesn't seem to be used in the Oracle
	% effect(All) --> deals_to_all(Source,Damage,All,Type).
	deals_to_all(X,Y,Z,A) --> /*{Z \= []},*/ [X], [deals], [Y], [damage, to], all(Z,A), 
		{ (A =  creatures ; A = planeswalkers ; A = players)}.

	% X deals Y Damage to each Z of type A
	% effect(All) --> deals_to_each(Source, Damage, All, Type).
	deals_to_each(X,Y,Z,A) --> 
		[X], [deals], [Y], [damage, to], each(Z,A), 
		{ (A =  creature ; A = planeswalker ; A = player)}.
	/*| ?- phrase(deals_to_each('Pyroclasm',2,Z,creature), Effect). 
	Z = [object('Goblin Piker' - 1,[])] ,
	Effect = ['Pyroclasm',deals,2,damage,to,each,creature] ; */

	% X deals Y Damage to each Z of type A and each Z of type B
	% effect(All) --> deals_to_each(Source,Damage,All,Type1,Type2).
	deals_to_each(X,Y,Z,A,B) --> 
		[X], [deals], [Y], [damage, to], each(Z,A,B). 
		{ (A =  creature ; A = planeswalker ; A = player),
		(B = creature ; B = planeswalker ; B = player)	}.
	/*| ?- phrase(deals_to_each('Earthquake',Y,Z,creature,player), Effect). 
	Y = _ ,
	Z = [object('Goblin Piker' - 1,[]),'Player 1','Glee-min'] ,
	Effect = ['Earthquake',deals,Y,damage,to,each,creature,and,each,player] ; */


	% Deal damage to a target creature or Planeswalker, or a target player
	% X: source; Y: damage amount; Z: creature; A: player.
	% effect(Target) --> deals(Source,Damage,Target,[]).
	% effect(Target) --> deals(Source,Damage,[],Target).
	deals(X,Y,Z,[]) --> [X], deals_to(Y,Z), [or, player]. % to target creature
	deals(X,Y,[],A) --> [X], [deals,Y,damage,to,target,'Creature',or], deals_or(A). %or target player

	deals_to(Y,Z) --> [deals], [Y], [damage, to], target(Z,B), 
		%{ (B =  'Creature' ; B = 'Planeswalker')}. % Planeswalkers are actually player types!
	{ (B =  'Creature' ; B = 'Planeswalker')}.

	deals_or(Z), [target] --> target(Z), 
		{ player(Z) }.
	/* | ?- phrase(deals('Bolt',3,[],'Player 1'), B).
	B = ['Bolt',deals,3,damage,to,target,creature,or,player] ;
	no
	| ?- phrase(deals('Bolt',3,[],'Player 2'), B).
	B = ['Bolt',deals,3,damage,to,target,creature,or,player] ;
	no
	| ?- phrase(deals('Bolt',3,copy('Goblin Piker',1,[]),[]), B).
	B = ['Bolt',deals,3,damage,to,target,'Creature',or,player] ;
	no */

	% Incorrectly implemented: Planeswalkers
	% A source deals damage to a target creature or Planeswlker 
	% effect(Target) --> deals(Source,Damage,Target,Type).
	deals(X,Y,Z) --> [X], [deals], [Y], [damage, to], target(Z,B), 
		{ (B =  'Creature' ; B = 'Planeswalker')}. 

	% A source deals damage to a target player
	% effect(Target_player) --> deals(Source,Damage,Target_player).
	deals(X,Y,Z) --> [X], [deals], [Y], [damage, to], target(Z), 
		{ player(Z) }.
	% Try also source(X), [deals], amount(X), ... etc. 

	% Creature buff: 
	%Target creature gets +3/+3 until end of turn
	% effect(Target) --> gets(Target,Sign1,P,Sign2,B).
	gets(X,A,B,C,D) --> target(X,Y), [gets,A,B,/,C,D],([until,end,of,turn];[]), 
		{ (Y =  'Creature')}. 
	/*| ?- phrase(gets(X,+,1,+,1), Z).
	X = copy('Goblin Piker',1,[]) ,
	Z = [target,'Creature',gets,+,1,/,+,1,until,end,of,turn] ;*/
	% Nope, I don't think that ^looks ^ very fetching. 
	/*
	Remember you can do: 
	| ?- A = +, B =1, C is A(B).
	A = + ,
	B = C = 1

	| ?- A = +, B =1, C is A(B,2).
	A = + ,
	B = 1 ,
	C = 3

	Also, you should be using atom_list to compose the 
	symbols (+,-,/,etc) in to an atom, but a) atom_list
	throws a tantrum with, eg: atom_to_list(X, [+,1,+,1])
	(because it doesn't like non-atomic numbers) and b) 
	if you keep them as atoms you will have to re-numberize
	them later for evaluation of the effect. 
	*/

	% Ability graft: 

	%This permanent gains <keyword> until end of turn
	% effect(This) --> gains(This).
	gains(X) --> this(X), [gains],keyword_ability,([until,end,of,turn];[]). 
	/*| ?- phrase(gains(X), Z).
	X = copy('Goblin Balloon Brigade',1,[]) ,
	Z = ['Goblin Balloon Brigade',gains,'Flying',until,end,of,turn] ; */

	%Target permanent gains <keyword> until end of turn
	% effect(Target) --> gains(Target,Type).
	gains(X,Y) --> target(X,Y), [gains],keyword_ability,([until,end,of,turn];[]), 
		{ permanent_Type(Y)}. 
	/*| ?- phrase(gains(X,Y), Z).
	X = copy('Goblin Balloon Brigade',1,[]) ,
	Y = 'Creature' ,
	Z = [target,'Creature',gains,'Deathtouch',until,end,of,turn] ; */
	% ^ Not all abilities can be grafted on all permanent types. 
	% Determine which ones can on what types, with
	%  keyword_ability = Kw1 ; Kw2 ; Kw3... etc. 

	% Target player gains control of target permanent (until end of turn). 
	% effect([Player,Permanent]) --> gains(Player, Permanent, Type).
	gains(X,Y,Z) --> target(X), [gains,control,of], target(Y,Z), ([until,end,of,turn] ; []),
		{ player(X), permanent_Type(Z)}.
	/*| ?- phrase(gains(X,Y,A), Z).
	X = 'Player 1' ,
	Y = copy('Island',3,[]) ,
	A = 'Land' ,
	Z = [target,player,gains,control,of,target,'Land',until,end,of,turn] ;	*/

	% effect([You,Permanent]) --> gain(You,Permanent,Type).
	gain(X,Y,Z), [you] --> you(X), (['Gain'];[gain]),[control,of], target(Y,Z), ([until,end,of,turn]; []),
		{ permanent_Type(Z)}.
	/*| ?- phrase(gain(X,Y,Z), A).
	X = 'Player 1' ,
	Y = copy('Goblin Piker',1,[]) ,
	Z = 'Creature' ,
	A = ['Gain',control,of,target,'Creature',until,end,of,turn] ; */

	% effect(Target) --> gains(Target,Counters,_termination).
	% A player gains poison counters
	gains(X,Y,Z) --> target(X), [gains,A,poison,Z],
		{ player(X), (Y = 1, A = a, Z = counter ; 
		Y \= 1, A = Y, Z = counters) }.
	/*| ?- phrase(gain(X,2,Z), A).
	X = 'Player 1' ,
	Z = counters ,
	A = [target,player,gains,2,poison,counters] ; */
	% ^ instantiating Y to a number distinguishes from other
	%  gains/3 clauses.

	% It could even happen to you!
	% effect(You) --> gain(You,Counters,_termination).
	gain(X,Y,Z),[you] --> you(X), [gain,A,poison,Z],
		{ player(X), (Y = 1, A = a, Z = counter;
		 Y \= 1, A = Y, Z = counters) }.


	% Put X: a/amount,Y: sign,A: value, S: slahs or [], 
	% Z: sign, B: value, C: counter/counters
	% eg: Put 3 +1/-3 counters on target creature. 
	% effect(Target) --> put(Amount,Sign1,P,/,Sign2,T,Target,Type).
	put(X,Y,A,S,Z,B,D,E) --> [put,M,Y,A,S,Z,B,C,on], target(D,E),
		{X = 1, M = a, C = counter ; M = X, C = counters,
		permanent_Type(E)}.
	/*| ?- phrase(put(1,[],[],[],[],audition,X,Y), D).
	X = copy('Goblin Piker',1,[]) ,
	Y = 'Creature' ,
	D = [put,a,[],[],[],[],audition,counter,on,target,'Creature'] 
	| ?- phrase(put(2,+,1,/,+,1,X,Y), D).
	X = copy('Goblin Piker',1,[]) ,
	Y = 'Creature' ,
	D = [put,2,+,1,/,+,1,counters,on,target,'Creature'] 
	*/ %^ Still needs some work... :\


%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
%	effect --> ['Choose one - '], effect(X), [;], [or], effect(Y).
	effect --> mode, effect(X), [;], [or], effect(Y).
	%effect(Player) --> mode(Player), effect(X), [;], [or], effect(Y).
	% ^ backstack full
	mode(Player) --> player(Player), [' chooses one -'].
	mode --> ['Choose one - '] ; ['Choose two - '] ; ['Choose one  or both- '].
%	effect(X) --> ['Choose one'], effect(X).
*/

/* Keep this: 
	deals(X,Y,Z,A) --> [X], deals_to(Y,Z), [or], [X], deals_or(Y,A).
	deals_to(Y,Z) --> [deals], [Y], [damage, to], target(Z,B), 
		{ (B =  'Creature' ; B = 'Planeswalker')}.
	deals_or(Y,Z) --> [deals], [Y], [damage, to], target(Z), 
		{ player(Z) }.
Because:
| ?- phrase(deals('Bolt',3,Target,Other), Z).
Target = copy('Goblin Piker',1,[]) ,
Other = 'Player 1' ,
Z = ['Bolt',deals,3,damage,to,target,'Creature',or,'Bolt',deals,3,damage,to,target,player] ;
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%activation_instructions 27/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%activation_instructions --> [with], keyword_ability.
	% ^ what, really? 
	activation_instructions --> [placeholder].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%all 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	% All permanents of a specified type 
	all(X,Y) --> [all, Y], 
		{ (	Y = artifacts, T = 'Artifact'; 
			Y = creatures, T = 'Creature';		  
			Y = enchantments, T = 'Enchantment';
			Y = lands, T = 'Land';
			Y = planeswalkers, T = 'Planeswalker';
			Y = permanents	
			% Btw, this simply replaces the singular, capitalised form 
			%  with the plural, just for some grammatical correctness.
		),
		findall(Z, 
			(zone('Battlefield', Cards),
			 Z = object(Name -_Id,_State),
			member(Z, Cards),  
			check_type(Name, _, [T], _)),
		X) }.
	/*| ?- phrase(all(Y,creatures), X). 
	Y = [copy('Goblin Piker',1,[]),copy('Birds of Paradise',1,[])] ,
	X = [all,creatures] ;*/
	% Instantiate Y to "permanents" to return all permanents. 
	% Leave undefined, and all permanent types are returned
	%  in successive backtracking.

	% Do: Extend this to cover all types in any zone. 

	% All players
	all(X,Y) --> [all, players], 
		{Y = players, findall(Z, player(Z), X)}.
	/*| ?- phrase(all(X,players), Effect). 
	X = ['Player 1','Glee-min'] ,
	Effect = [all,players]*/

	% I'm not sure if "all X and all players" appears anywhere.
	all(X,Y,Z) --> ([all] ; ['All']), [Y, and, all, Z], 
		{((	((Y = artifacts ; Z = artifacts), T = 'Artifact'); 
			((Y = creatures ; Z = creatures), T = 'Creature');	
			((Y = enchantments ; Z = enchantments), T = 'Enchantment');
			((Y = lands ; Z = lands), T = 'Land');
			((Y = planeswalkers ; Z = planeswalkers), T = 'Planeswalker');
			(Y = permanents ; Z = permanents)	
		),
		findall(A, 
			(zone('Battlefield', Cards),
			 A = object(Name -_Id,_State),
			member(A, Cards),  
			check_type(Name, _, [T], _)),
		X1)) , ( (Y = players ; Z = players), 
		findall(B, player(B), X2) ),
		append(X1, X2, X) }.
	/*| ?- phrase(all(X,Y,Z), Effect). 
	X = [object('Goblin Piker' - 1,[]),'Player 1','Glee-min'] ,
	Y = creatures ,
	Z = players ,
	Effect = [all,creatures,and,all,players] ; ;*/	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%each 05/04/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% This and all are really the same except for number (artifact/artifacts etc). 
	%  It's just that it'll look a bit ugly to disambiguate between them.
	% I should generalise number of type atoms later, 
	%  but for now it's clearer like this.

	% Each permanent of a specified type 
	each(X,Y) --> ([each];['Each']), [Y], 
		{ (	Y = artifact, T = 'Artifact'; 
			Y = creature, T = 'Creature';		  
			Y = enchantment, T = 'Enchantment';
			Y = land, T = 'Land';
			Y = planeswalker, T = 'Planeswalker';
			Y = permanent	
			% Btw, this simply replaces the singular, capitalised form 
			%  with the plural, just for some grammatical correctness.
		),
		findall(Z, 
			(zone('Battlefield', Cards),
			 Z = object(Name -_Id,_State),
			member(Z, Cards),  
			check_type(Name, _, [T], _)),
		X) }.
	/*| ?- phrase(each(X,land), Effect). 
	X = [object('Mountain' - 6,[tapped]),object('Mountain' - 1,[])] ,
	Effect = [each,land] ; */

	% Each player
	each(X,Y) --> ([each];['Each']), [player], 
		{ Y = player, findall(Z, player(Z), X)}.
	/*| ?- phrase(each(X,player), Effect). 
	X = ['Player 1','Glee-min'] ,
	Y = player ,
	Effect = ['Each',player] */

	% Each permanent of a specified type and each player
	each(X,Y,Z) --> ([each] ; ['Each']), [Y, and, each, Z], 
		{((	((Y = artifact ; Z = artifact), T = 'Artifact'); 
			((Y = creature ; Z = creature), T = 'Creature');	
			((Y = enchantment ; Z = enchantment), T = 'Enchantment');
			((Y = land ; Z = land), T = 'Land');
			((Y = planeswalker ; Z = planeswalker), T = 'Planeswalker');
			(Y = permanent ; Z = permanent)
		),
		findall(A, 
			(zone('Battlefield', Cards),
			 A = object(Name -_Id,_State),
			member(A, Cards),  
			check_type(Name, _, [T], _)),
		X1)) , ( 
			((Y = player ; Z = player), 
		findall(B, player(B), X2) ),
		append(X1, X2, X)) }.
	/*| ?- phrase(each(X,creature,player), Effect). 
	X = [object('Goblin Piker' - 1,[]),'Player 1','Glee-min'] ,
	Effect = [each,creature,and,each,player] ; */


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%target 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
	114.2. Only permanents are legal targets for spells and abilities, 
	unless a spell or ability 
	  (a) specifies that it can target an object in another zone 
	  or a player, 
	  (b) targets an object that can't exist on the battlefield, 
	  such as a spell or ability, or 
	  (c) targets a zone.
*/

	target --> target(X).
	target --> target(X,Y).
	target(X),[target] --> target(X,_Y). % used in, eg "(target Type1 or) Type2"
	% ^ eg: | ?- phrase(target(object('Goblin Balloon Brigade' - 1,[])), ['Land']). no
	% | ?- phrase(target(X), ['Land']). X = object('Mountain' - 2,[]) ;
	target --> target(X,Y,Z).
	%target(X,Y) --> target(X), ([or]; [and] ; [',']), target(Y).
	%target --> target, target. % not too useful.
	% eg: phrase(target,[target,'Creature',and,target,player]).
	% 	phrase(target,[target,'Creature',or,player]).


	target(X,Y) --> [target], condition(X), [Y], 
		{ object_handle(X, Name-_Id), 
		check_type(Name, _, [Y], _) }. 

	condition(X) --> [tapped], 
		{ object_handle(X, Handle), 
		object_state(Handle, State), 
		member(tapped, State)  }. 
	/*| ?- phrase(destroy(X,Y),['Destroy',target,tapped,'Creature']). 
	X = object('Prodigal Pyromancer' - 3,[tapped]) ,
	Y = 'Creature' */


	% A target can be a permanent of a specified type,
	%  with a keyword ability.
	target(X,Y,Z) --> [target, Y], [with,Z], %keyword_ability(Z), 
		{zone('Battlefield', Cards),
		 X = object(Name -_Id,_State),
		member(X, Cards),  
		check_type(Name, _, [Y], _),
		card([card_name Name, _, _, text_box Text_box, _, _, _, _]),
		member(A, Text_box) ,phrase(keyword_ability(A),A), 
		[Z] = A}. %remove brackets
	/*| ?- phrase(target(X,Y,Z), A).
	X = copy('Air Servant',1,[]) ,
	Y = 'Creature' ,
	Z = 'Flying' ,
	A = [target,'Creature',with,'Flying'] */

		%^ Temp patch until I fix BoP Mana ability 
		%  (it clashes with determine_ability/2 
		/*determine_abilities(Name, Abilities),
		member(Z,Abilities)}.*/ 

	% A target can be a permanent of a specified type.
	target(X,Y) --> ([target];['Target']), [Y], 
		{zone('Battlefield', Cards),
		 X = object(Name -_Id,_State),
		member(X, Cards),  
		check_type(Name, _, [T], _) , 
		invert_case(Y, T)}. % This does something weird...
	/*| ?- phrase(target(X,Y), Z).
	X = copy('Goblin Piker',1,[]) ,
	Y = creature ,
	Z = [target,creature] ;*/ % Alternative to >

	% A target can be a permanent of a certain type.
	target(X) --> ([target];['Target']), [Y],
		{zone('Battlefield', Cards),
		 X = object(Name -_Id,_State),
		member(X, Cards),  
		check_type(Name, _, [Y], _)}.
	/*| ?- phrase(target(X), Z ).
	X = 'Goblin Balloon Brigade' ,
	Z = [target,'Creature'] ;
	X = 'Mountain' ,
	Z = [target,'Land'] ; */

	% A target can be a permanent of a type other than one specified.
%	target(X) --> [target, non, - ,Y], object(X), 
	target(X) --> ([target];['Target']), [non, - ,Y], object(X), 
		{ X = object(Name -_Id,_State),
		\+ check_type(Name, _, [Y], _) }.
	/* | ?- phrase(target(X), [target, non, - ,'Land',permanent]).
	X = copy('Goblin Balloon Brigade',1,[]) ; */

	% A target can be an object, reporting its type and 
	%  object class (permanent or spell, basically)
%	target(X) --> [target, Y], object(X,Y).
	target(X) --> ([target];['Target']), [Y], object(X,Y).
	/*| ?- phrase(target(X), Z).
	X = copy('Goblin Balloon Brigade',1,[]) ,
	Z = [target,['Creature'],spell] ;
		% ^ Good start, but then... >
	X = copy('Mountain',1,[tapped]) ,
	Z = [target,['Land'],permanent] ;*/

	% A target can be any object, reporting its object class
	%  (permanent, spell etc)
%	target(X) --> [target], object(X).
	target(X) --> ([target];['Target']), object(X).
	/*| ?- phrase(target(X), A).
	X = copy('Goblin Balloon Brigade',1,[]) ,
	A = [target,spell] ;
	X = copy('Mountain',1,[tapped]) ,
	A = [target,permanent] ; 
	| ?- phrase(target(copy('Goblin Balloon Brigade',1,[])), A).
	A = [target,spell] ;
	| ?- phrase(target(X), [target,spell]).
	X = copy('Goblin Balloon Brigade',1,[]) */

	% A target can be a player: 
%	target(X) --> [target], player(X).
	target(X) --> ([target];['Target']), player(X).
	/*| ?- phrase(target(X), Y). 
	X = 'Player 1' ,
	Y = [target,player] 
	| ?- phrase(target('Player 2'), [target,player]). 
	yes*/
	% Note you could also call the non-DCG player/1 directly, 
	%  but it's useful to identify what the DCG player(X) is too. 
	% Same for all X's 

	% A target can be a partition
%	target(X) --> [target, X], 
	target(X) --> ([target];['Target']), [X], 
			{zone(X), 
			(\+ X='Stack', \+ X='Battlefield', \+ X='Exile') }.
	/*| ?- phrase(target(X), Y).
	X = 'Library' ,
	Y = [target,'Library'] ;*/

	% A target can be a partition, naming the player.
%	target(X,Y) --> [target,'player''s',Y], 
	target(X,Y) --> ([target];['Target']),['player''s',Y], 
			{ player(X), zone(Y), 
			(\+ Y='Stack', \+ Y='Battlefield', \+ Y='Exile') }. 
	/*| ?- phrase(target(X,Y), A).
	X = 'Player 1' ,
	Y = 'Library' ,
	A = [target,'player''s','Library'] 
	| ?- phrase(target('Player 2','Hand'), [target,'player''s','Hand']).
	yes*/ 

%	target(X,Y,Z) --> [target], zone(X,Y,Z).
	target(X,Y,Z) --> ([target];['Target']), zone(X,Y,Z).
	/*| ?- phrase(target(X,Y,Z), A).
	X = 'Player 1' ,
	Y = 'Hand' ,
	Z = ['Prodigal Pyromancer','Prodigal Pyromancer','Goblin Balloon Brigade','Mountain','Lava Axe','Vulshok Berserker'] ,
	A = [target,'Hand'] ;*/
	% ^ alternative way to say "target zone", plus player/conents info. 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%zone 12/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	% A player's partitions
	zone(X,Y) --> player(X),['s'],zone(X,Y,Z), [Z], 
			{ \+ Y='Stack', \+ Y='Battlefield', \+ Y='Exile' }. 
	/*| ?- phrase(zone(X,Y), Z). 
	X = 'Player 1' ,
	Y = 'Library' ,
	Z = [player,s,'Library',[]] ;
	| ?- phrase(zone('Player 1','Library'), [_,_,_,D]). 
	D = [] ; % Power!!
	| ?- phrase(zone(X,Y), [player,s,'Stack',D]). 
	no*/ 
	
	% A target can be a shared zone. Wait. Can it? No, it can't. 
	% "the" is not a target (nor is "a"). 
	zone(X,Y) --> [the,X], {zone(X,Y)}.
	/*X = 'Stack' ,
	Y = [] ,
	A = [the,'Stack'] ;*/

	% A player's partitions, naming "you": _your_ partition. 
	zone(X,Y) --> [your],zone(X,Y,Z), [Z], 
			{player(X),
			 \+ Y='Stack', \+ Y='Battlefield', \+ Y='Exile' }. 
	/*| ?- phrase(zone(X,Y), Z).
	X = 'Player 1' ,
	Y = 'Hand' ,
	Z = [your,'Hand',['Prodigal Pyromancer','Prodigal Pyromancer',
	'Goblin Balloon Brigade','Mountain','Lava Axe','Vulshok Berserker']] */

	% Partitions by player
	zone(X,Y,Z) --> [Y], {zone(X,Y,Z)}, 
			{ \+ Y='Stack', \+ Y='Battlefield', \+ Y='Exile' }.
	/*| ?- phrase(zone(X,Y,Z), A). 
	X = 'Player 1' ,
	Y = 'Library' ,
	Z = [] ,
	A = ['Library'] ;
	| ?- phrase(zone(X,'Exile',Z), A). 
	no	*/ % ^ used with "target zone" etc.

	% Shared zones
	zone(X,Y) --> [X], {zone(X,Y)}.
	/*| ?- phrase(zone(X,Y), A). 
	X = 'Exile' ,
	Y = [] ,
	A = ['Exile']*/
	% ^ Use?


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%player 12/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	% A player is a player
	player(X) --> [player], {player(X)}.
	/*| ?- phrase(player('Player 1'), X). 
	X = [player]
	| ?- phrase(player('Player 1'), [player]). 
	yes	*/

	% Called with effect(X), [you] --> player(X)... 
	%  to remove the explicit "you" before, eg "Gain control"...
	you(X) --> [you], {player(X)}.
	/*| ?- phrase(you(X), Y). 
	X = 'Player 1' ,
	Y = [you] ;*/

	your(X) --> [your], {player(X)}.
	% ^ untested and caused problems.

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	"You" and "your" are not really players, rather an Object's controller: 109.5
	 Also of course it is used to identify a player's zones, but 
	  I can't find that explicitly declared in the rules. 
	Also, it's used to find a player's mana pool, life total and so on. 
	  So I should define them like players, or pointers to players. 
	Doing so caused problems- check notes on 14-03-11-2

	A player can also be a Planeswalker! 
	  I'm not implementing them yet, so it's fudged, above 
	 (especially where things target "creature or player" etc.

*/
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%object 12/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*	109.1. An object is an ability on the stack, 
	a card, a copy of a card, a token, a spell, 
	a permanent, or an emblem.
*/

	% An object of a specific type
	object(X,Y) --> object(X), 
		{ X = object(Name -_Id,_State),
		check_type(Name, _, [Y], _)}.
	/* |?- phrase(object(X,Y), A).
	X = copy('Goblin Balloon Brigade',1,[]) ,
	Y = 'Creature' ,
	A = [spell] ;
	X = copy('Mountain',1,[tapped]) ,
	Y = 'Land' ,
	A = [permanent] ;*/
	% ^ hm. Again not quite correct MGL, always 
	%  "Creature Spell" is OK, but "Land Peramanent? Hm. Yeah, 
	%  OK, but not what I wanted. 

	% An object is a spell on the Stack
	object(X) --> spell(X).  
%	spell(X) --> [spell],
	spell(X) --> ([spell];[ability]), % not tested
		{ X = object(_Name -_Id,_State),
		zone('Stack', Objects),
		member(X, Objects) }.
	/*| ?- phrase(spell(X), A).
	X = copy('Goblin Balloon Brigade',1,[]) ,
	A = [spell] ;*/

	% An object is a permanent, or token on the Battlefield
	object(X) --> permanent(X).	%[X], 
	permanent(X) --> [permanent], 
		{ X = object(_Name -_Id,_State),
		zone('Battlefield', Objects),
		member(X, Objects) }. 
	/* | ?- phrase(permanent(X), A).
	X = copy('Mountain',1,[tapped]) ,
	A = [permanent] ; */

	% An object is an emblem in the Command zone)
	object(X) --> emblem(X).
	emblem(X) --> [emblem], 
		{ X = object(_Name -_Id,_State),
		zone('Command', Objects),
		member(X, Objects) }. 
	/* no emblems -> tests yet*/

	% An object is a card in a zone
	% (other than the Battlefield or Stack)
	object(X,Y,Z) --> card(X,Y,Z).
	% Call as for card/3 is not correct MGL. 

	% An object that is a spell can refer to itself 
	%  by its card name, if its card name is part of one of its abilities. 
	this(X), [spell] --> object(X), [Name], 
		{X = object(Name - _Id, _State), 
		card([card_name Name, _, _, text_box Text, _, _, _, _]),
		member(Ability, Text),
		member(Name, Ability) }. 
	% ^ Should use determine_abilities/2

	% An object that is an ability on the Stack can refer to itself
	%  by its card name, if its card name is part of its text.
/*	this(X), [ability] --> object(X), [Name], 
		{X = object(Name - _Id, _State), 
		card([card_name Name, _, _, text_box Text, _, _, _, _]),
		member(Ability, Text),
		member(Name, Ability) }. 
*/	% ^ Not like this- later.

	% An object that is a permanent can refer to itself 
	%  by its card name, if its card name is part of one of its abilities. 
	this(X), [permanent] --> object(X), [Name], 
		{X = object(Name - _Id, _State), 
		card([card_name Name, _, _, text_box Text, _, _, _, _]),
		member(Ability, Text),
		member(Name, Ability) }. 
		/*| ?- phrase(this(X), Z).
		X = copy('Goblin Balloon Brigade',1,[]) ,
		Z = ['Goblin Balloon Brigade'] ; */

	% An object that is a card can have an ability that refers to 
	%  a permanent or a spell with its card name, if its card name 
	%  is part of one of its abilities. The card is in a zone, other
	%  than the stack (but not just in the database). 
	%this(Name) --> [Name], 
	this(Name) --> card(Name),
		 {card([card_name Name, _, _, text_box Text, _, _, _, _]),
		member(Ability, Text),
		member(Name, Ability) }. 
		/*| ?- phrase(this(X), Y).
		X = 'Aether Adept' ,
		Y = ['Aether Adept'] ;
		...
		X = 'Pyroclasm' ,
		Y = ['Pyroclasm'] ;
		no*/
	% ^ This will return all the names of cards in hands, libraries, etc, 
	%  that have an ability that mentions the spell's name, 
	%  eg "When Aether Adept enters the Battlefield..." 
	%  or "Lightning Bolt deals..."
	% Here because other this(X) clauses only find spells or permanents 
	%  already on the Stack or Battlefield and determine_abilities needs
	%  to be able to recognise this sort of ability even before a card
	%  is on the Stack or Battlefield. 

	% The source of an ability can be an object 
	%  and it can refer to itself. 
	source(X) --> object(X,Y).
	source(X) --> object(X).
	source(X) --> object(X,Y,Z).
	source(X), [spell] --> this(X).
	source(X), [permanent] --> this(X).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%card 12/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*  108.2a In the text of spells or abilities, the term "card" 
	is used only to refer to a card that's not on the battlefield 
	or on the stack, such as a creature card in a player's hand. 
	For more information, see section 4, "Zones."
*/

	% A card in a zone (that is not the Battlefield or the Stack)
	card(X) --> [X], 
		{ zone(Z, Y, Cards),
		member(X, Cards),
		player(Z),
		Y \= 'Battlefield',
		Y \= 'Stack'  }.
	/*| ?- phrase(card(X), Y).
	X = 'Island' ,
	Y = ['Island'] ;	*/
	% ^ This will return only cards in hands, libraries etc
	%  not cards in the database that are not in one of a player's 
	%  zones. 

% Hm. Those below are not quite correct MGL... 

	% A card in a zone (that is not the Battlefield or the Stack)
	card(X,Y,Z) --> [X,Y,Z], 
		{ zone(Z, Y, Cards),
		member(X, Cards),
		player(Z),
		Y \= 'Battlefield',
		Y \= 'Stack'  }.
	/*| ?- phrase(card(X,Y,Z), A).
	X = 'Prodigal Pyromancer' ,
	Y = 'Hand' ,
	Z = 'Player 1' ,
	A = ['Prodigal Pyromancer','Hand','Player 1'] ;*/
	% Use to target cards in players' hands, 
	%  libraries etc. 

	% A card in a zone, of a certain type, eg, a creature card in 
	%  a player's hand. 
	% card(Name, Zone, Player, Supertype, Type, Subtype).
	card(X,Y,Z,A,B,C) --> [X,Y,Z], 
		{ zone(Z, Y, Cards),		
		( member(X, Cards) ; 
		member(object(X -_Id,_State), Cards) ),		
		player(Z),
		Y \= 'Battlefield',
		Y \= 'Stack',
		check_type(X, A, B, C)  }.
	/* | ?- phrase(card(X,Y,Z,A,B,C), D).
	X = 'Mountain' ,
	Y = 'Library' ,
	Z = 'Player 1' ,
	A = ['Basic'] ,
	B = ['Land'] ,
	C = ['Mountain'] ,
	D = ['Mountain','Library','Player 1'] ; */
	% Note this won't find any copy/3 Objects in Gys or the Exile. 
	% Useful to search libraries and hands though. 
	% OK, now it will: 
	/*| ?- phrase(card(X,Y,Z,A,B,C), D).
	X = 'Goblin Balloon Brigade' ,
	Y = 'Graveyard' ,
	Z = 'Player 1' ,
	A = [] ,
	B = ['Creature'] ,
	C = ['Goblin','Warrior'] ,
	D = ['Goblin Balloon Brigade','Graveyard','Player 1'] */

/* Check this: 

	| ?- phrase(card('Goblin Balloon Brigade',Y,Z,A,B,C), D).
	Y = 'Graveyard' ,
	Z = 'Player 1' ,
	A = [] ,
	B = ['Creature'] ,
	C = ['Goblin','Warrior'] ,
	D = ['Goblin Balloon Brigade','Graveyard','Player 1'] ;

	Y = 'Hand' ,
	Z = 'Player 1' ,
	A = [] ,
	B = ['Creature'] ,
	C = ['Goblin','Warrior'] ,
	D = ['Goblin Balloon Brigade','Hand','Player 1'] ;

	Y = 'Library' ,
	Z = 'Player 1' ,
	A = [] ,
	B = ['Creature'] ,
	C = ['Goblin','Warrior'] ,
	D = ['Goblin Balloon Brigade','Library','Player 1'] ;

	Y = 'Library' ,
	Z = 'Player 1' ,
	A = [] ,
	B = ['Creature'] ,
	C = ['Goblin','Warrior'] ,
	D = ['Goblin Balloon Brigade','Library','Player 1'] ;

	All cards named 'Goblin Balloon Brigade' in a player's hand, 
	graveyard and library.
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%mana 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	xxy(Y) --> [X], {string_to_list(X, Y)}.
%  	phrase(xxy([23,g,r]), ['23gr']).

%	mana(X) --> [Y], {string_to_list(X,Y)}.
	% ^ Y is the list form of string X

%	mana --> [C], {mana_cost(C)}.
	% OK, but this won't generate mana strings... 

	% mana: non-terminals
	mana --> symbol_string. 
	mana --> colourless_mana. 
	mana --> colourless_mana, symbol_string. 
	symbol_string --> mana_symbol. 
	symbol_string --> mana_symbol, symbol_string. 

	% mana: terminals
	mana_symbol --> [w] ; [u] ; [b] ; [r] ; [g]. 
	colourless_mana --> [C], { number(C) }.
	% ^ but query with phrase(mana, [<number>])!!


%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	OK; test with: 
	phrase(mana_ability, [add,w,to,your,mana,pool]).
	phrase(mana_ability, [add,1,to,your,mana,pool]).
	phrase(mana_ability, [add,w,u,r,g,g,g,to,your,mana,pool]).
	phrase(mana_ability, [add,10000,w,to,your,mana,pool]).
	phrase(mana_ability, [add,125787,w,u,r,g,g,g,to,your,mana,pool]).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%     Effects & Abilities rules     %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%determine_abilities/2 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* determine_abilities(+Card, ?Abilities) */
	% Returns a card's abilities, each in a separate list. 

	determine_abilities(Card, Abilities):-
		card([card_name Card, _, _, text_box Text_box, _, _, _, _]),		
		determine_abilities(Text_box, [], Abilities).


%%%%%%%%%%%%%%determine_abilities/3 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%determine_abilities/3 (0) 25/02/11

	determine_abilities([], Abilities, Abilities).

%%%%%%%%%%%%%%determine_abilities/3 (1) 25/02/11

	determine_abilities(Text_box, Temp, Abilities):- 
		member(Ability, Text_box), 
		%phrase(ability, Ability), 
		remove(Ability, Text_box, New_text_box), 
		append(Temp, [Ability], New_temp),
		determine_abilities(New_text_box, New_temp, Abilities).

/*
700.2. A spell or ability is modal if it has two or more options 
preceded by "Choose one  -- ," 
"Choose two  -- ," 
"Choose one or both  -- ," 
or "[a specified player] chooses one  -- ." 
Each of those options is a mode.
*/


%%%%%%%%%%%%%%mana_ability/3 20/02/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* mana_ability(+Permanent, ?Ability, ?Mana) */
	% One mana ability of a permanent and the Mana it generates

	mana_ability(Permanent, Ability, Mana):- 
		card([card_name Permanent, _, _, text_box Abilities, _, _, _, _]),
		member(Ability, Abilities),
		phrase(mana_ability, Ability), 
		Ability = [_Cost, :, 'add', Mana, 'to', 'your', 'mana', 'pool'].

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	Mana abilities must be callable independently of 
	  activate_abilities/3, since the same timing rules
	  don't necessarily apply and their Effects don't 
	  use the stack. 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%determine_cost/2 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* determine_abilities(+Spell, +Ability, ?Cost) */
	% Returns the mana cost of a spell or the activation cost of one 
	%  ability of a permanent. 

	% The cost is a mana cost. 
	determine_cost(Spell, [], [Cost]):-
		card([card_name Spell, mana_cost Cost, _, _, _, _, _, state State]).
	% Needs to deal with variable (X) and hybrid (g/r) costs. 

	% The cost is an activation cost 
	determine_cost([], Ability, Cost):-
		sublist(Cost, Ability),
		phrase(cost, Cost).
	% ^ Merh. This found that Cost = [tap, u] (on backtracking 
	% after finding Cost = tap). Maybe I should define cost more 
	%  rigorously, as what comes after a colon... 

	% Yep, see? These two determine a mana cost differently; one is a 
	%  mana string: 2341ubgrw, the other is a list of mana symbols: 
	%  2341u,b,g,r,w. That's because I have a new definition in here
	% I should  bring it in line with the older one, I think it's more
	%  convenient. 

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	Note that for activated abilities, this will return the whole
	  cost. Further processing should sepearate individual costs and 
	  attempt to satisfy them in turn.
	I decided to develop this program because I knew it would give me 
	  the chance to say things like that. 
	Yes, ha ha. I changed this in activate_abilities, will update this
	  when there's time. 
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%additional_cost/2 27/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
	% The Spell has an additional cost
	additional_cost(Spell, [], Cost):-
		card([card_name Spell, _, _, text_box Abilities, _, _, _, _]),
		sublist(Cost, Abilities),
		phrase(additional_cost, Cost).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%alternative_cost/2 27/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
	determine_cost(Spell, [], Cost):-
		card([card_name Spell, _, _, text_box Abilities, _, _, _, _]),
		sublist(Cost, Abilities),
		phrase(additional_cost, Cost).
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%choose_targets/4 04/04/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* choose_target(+Controller, +Object, +Ability, -Updated) */
	% Takes in the user's choice of target, checks it for validity 
	%  and adds it to the state of the ability or spell. 

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	For now, only one target is chosen per ability
	Also, not tested with spells that have multiple abilities, 
	  though it should handle them, in theory (hence the recursion). 
*/

%%%%%%%%%%%%%%choose_targets/4 (0) 04/04/2011

	% The Object has no abilities, or all its 
	%  abilities have been processed. 
	choose_targets(_Player, _Object, [], _Play).

%%%%%%%%%%%%%%choose_targets/4 (1) 04/04/2011
	
	% The Object is a spell on the stack; 
	% its first Ability is not a spell ability 
	choose_targets(Player, Object, Text_box, Play):- 
		object_handle(Object, Name-_Id), 
		\+ phrase(ability, Name), 
		% ^ The object is not an ability on the stack
		member(Ability, Text_box), 
		\+ phrase(spell_ability(_), Ability), 
		remove(Ability, Text_box, Rest_box), 
		choose_targets(Player, Object, Rest_box, Play). 

%%%%%%%%%%%%%%choose_targets/4 (2) 04/04/2011

	% The Object is a spell on the stack and its current ability is 
	%  a targeted spell ability, or the Object is an ability on the Stack 
	choose_targets(Player, Object, Abilities, Play):- 
		member(Ability, Abilities), 
		choose_targets(Player, Ability, Target),
		update_object(Player, Object, Target, Updated, Play), 
		remove(Ability, Abilities, Rest_box),
		choose_targets(Player, Updated, Rest_box, Play). 


%%%%%%%%%%%%%%choose_targets/3 04/04/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* choose_targets(+Player, +Ability, -Targets) */
	% Lets the player choose targets for an Ability 

	choose_targets(Player, Ability, Targets):- 
		ability_targets(_Player, Ability, Map, Switches, Identified), 
		choose_targets(Player, Ability, Map, Switches, Identified, Targets).


%%%%%%%%%%%%%%ability_targets/5 04/04/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* ability_targets(+Player, +Ability, -Map, -Switches, -Identified) */
	% Finds all legal targets for an Ability 

	ability_targets(_Player, Ability, Map, Switches, Identified):- 
		findall(TArget, 
					( phrase(ability(Target), Ability, []), 
					(object_handle(Target, Name-Id)->
					TArget = [Name,Id]; 
					player(Target), TArget = Target;
					zone(Target), TArget = Target )),
				Targets), 
		findall(Target, (member([Target, Id], Targets);
				 member(Target, Targets), 
					\+ Target = [_Name,_Id]),
				Names), 
		append([cancel], Names, Full), 
		prompt_switches(Full, Switched), 
		Switched = [Map, Switches], 
		identify_object(Map, Targets, 1, [], Identified).


/*
| ?- phrase(effect(Tagret), ['Prodigal Pyromancer',deals,1,damage,to,target,'Creature',or,player],[]).
Tagret = object('Prodigal Pyromancer' - 3,[]) ;
*/

%%%%%%%%%%%%%%choose_targets/6 04/04/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* choose_targets(+Player, +Ability, +Map, +Switchs, +Identified, -Targets) */
	% Handles player input for choose_targets/3

	choose_targets(Player, Ability, Map, Switches, Identified, Targets):-  
		Context = [Ability, Map, Switches, Identified],
		input(choose_targets, Context, Input) ->  
		atom_chars(Char, [Input]), 
		map_targets(Char, Map, Identified, Targets);
		choose_targets(Player, Ability, Map, Switches, Identified, Targets).
	% Once this works, it needs to be upgraded to string_input, so that 
	%  you can choose any number of targets- then you need to add them to 
	%  the object's state separately. I mean recursively of curse, I mean course.


%%%%%%%%%%%%%%ability_targets/4 04/04/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* map_targets(+Switch, +Map, +Identified, -Targets) */
	% Identifies targets from their Name-Switch map and their
	%  Name-Switch, Id list. 

%%%%%%%%%%%%%%ability_targets/4 (0) 04/04/2011

	% The Player has chosen to cancel targeting
	map_targets(Switch, Map, _Identified, []):- 
		member(cancel - Switch , Map). 

%%%%%%%%%%%%%%ability_targets/4 (1) 04/04/2011

	% The target is an MGL Object: object(Name-Id, State)
	map_targets(Switch, Map, Identified, Target):- 
		member(Name - Switch , Map), 
		% ^ From the Switch we find the name of the target's Name
		member([Name - Switch, Id], Identified), 
		% ^ From the Name and Switch we find the object's Id .
		object_handle(Target, Name-Id).

%%%%%%%%%%%%%%ability_targets/4 (2) 04/04/2011

	% The target is a player
	map_targets(Switch, Map, Identified, Player):- 
		member(Player - Switch , Map), 
		member([Player - Switch], Identified), 
		player(Player).

%%%%%%%%%%%%%%ability_targets/4 (3) 04/04/2011

	% The target is a zone
	map_targets(Switch, Map, Identified, Zone):- 
		member(Zone - Switch , Map), 
		member([Zone - Switch], Identified), 
		zone(Zone, _Objects).


%%%%%%%%%%%%%%update_object/5 04/04/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* update_object(+Player, +Object, +Target, +Updated, -Play) */
	% Adds a state, target(Target) to the State-list of Object.

%%%%%%%%%%%%%%update_object/5 (0) 04/04/2011

	% Targeting cancelled- don't update the object. 
	update_object(_Player, _Object, [], _Updated, 99). 

%%%%%%%%%%%%%%update_object/5 (1) 04/04/2011

	% Update the object's State with the targeting information
	update_object(Player, Object, Target, Updated, _Play):- 
		(object_handle(Target, Handle) -> TArget = Handle ;
		player(Target)-> TArget = Target  ),
		add_state(Object, target(TArget), Updated),
		change_state(Object, 'Stack', Player, Updated). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%generate_effect/2 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* generate_effect(+Object, +Player) */

%%%%%%%%%%%%%%generate_effect/2 (1) 22/04/11

	% Temp: permanents will need to have their spell abilities checked
	%  while being cast. 
	generate_effect(Object, Player):- 
		Object = object(Name - Id, _State),
		permanent_type(Name),
		move_to_zone(Player, Object, 'Stack', 'Battlefield'), 
		( creature(Object,_,Abilities,_,_,_) ->  
		(member(['Haste'], Abilities) ;
		add_state(Object, 'summoning sickness', Updated), 
		change_state(Object, 'Battlefield', Player, Updated)) ; true ),
		output(generate_effect,[permanent, Name-Id]).

%%%%%%%%%%%%%%generate_effect/2 (2) 22/04/11

	% Not a permanent- creating spell. 
	generate_effect(Object, Player):- 
		\+ permanent_type(Object),
		% Object : object(Name - Id, State)
		%   where member(target(Target), State).
		determine_effect(Object, Effect, Target), 
		gen_effect(Effect, Target),
		% tab(25), write(`Effect: ` - Object), nl,
		Object = object(Name - _Id, _State),
		(phrase(ability, Name) -> remove_from_zone(Player, Object, 'Stack'); 
		move_to_zone(Player, Object, 'Stack', 'Graveyard')),
		output(generate_effect,[non_permanent, Name]).


%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	The first clause implements summonign sickness naively.
	Any permanent may acquire a creature type at any point
	  during the game. It should still be subject to 
	  summoning_sickness.
*/


%%%%%%%%%%%%%%determine_effect/3 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* determine_effect(+Object, -Effect, -Recipients) */
	% Determines the Effect that must be generated by an Object on 
	%  the Stack and the Recipients to be affected by it. 

%%%%%%%%%%%%%%determine_effect/3 (1) 25/02/11

	% The Object is a spell on the Stack, it has an ability with a 
	%  targeted effect and its target is in its State
	determine_effect(Object, Effect, Target):- 
		Object = object(Name - _Id, State), 
		%  eg State: [ target('Maritime Guard' - 1) ]
		member(target(Target), State),
		% ^ The target's State could have "untargettable" in, check later
		determine_abilities(Name, Abilities), 
		member(Ability, Abilities), % Only its first ability! Refine.
		(object_handle(TArget, Target); player(Target), TArget = Target ), 
		sublist(Effect, Ability), 
		phrase(effect(TArget), Effect).

%%%%%%%%%%%%%%determine_effect/3 (1) 04/04/11

	% The Object is a targeted ability on the stack
	determine_effect(Object, Effect, Target):- 
		object_handle(Object, Ability - Id), 
		object_state(Ability-Id, State), 
		%  eg State: [ target(object('Maritime Guard' - 1, [])) ]
		member(target(Target), State),
		(object_handle(TArget, Target); player(Target), TArget = Target ), 
		sublist(Effect, Ability), 
		phrase(effect(TArget), Effect).


/*	determine_effect(Object, Effect, Target):- 
		object_handle(Object, Ability - Id), 
		object_state(Ability-Id, State), 
		%  eg State: [ target(object('Maritime Guard' - 1, [])) ]
		member(target(Target), State),
		(object_handle(TArget, Target); player(Target), TArget = Target ), 
		phrase(effect(TArget), Ability, Effect).
*/


%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	 The call to sublist/2 -> phrase/2 doesn't check the 
	  validity of a target, as in choose_targets- rather, 
	  it associates an effect with a subset of any targets 
	  stored in the object's State list. An ability may have 
	  more than one effect each with a different set of targets.
	Abilities with more than one target will have to be dealt with
	  separately.
*/


%%%%%%%%%%%%%%gen_effect/2 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%gen_effect/2 (1) 05/04/11

	% "Bounce" effect.
	gen_effect(Effect, Target):- 
		( object_handle(TArget, Target) ; player(Target), TArget = Target ), 
		Effect = ['Return',target,_Permanent_type,to,its,'owner''s',hand],
		owned_by(TArget, Owner),
		move_to_zone(Owner, TArget, 'Battlefield', 'Hand').

%%%%%%%%%%%%%%gen_effect/2 (2) 06/04/11

	% "Burn" effect
	gen_effect(Effect, Target):- 
		( object_handle(TArget, Target) ; player(Target), TArget = Target ), 
		%Effect = [Source,deals,Damage,damage,to,target,'Creature',or,player]
		( phrase(deals(Source,Damage,TArget,[]), Effect); % target Creature
		phrase(deals(Source,Damage,[],TArget), Effect); % or target player
		%Effect = [Source,deals,Damage,damage,to,target,player]
		phrase(deals(Source,Damage,TArget), Effect)), 
		damage(Source, Target, Damage).
	% Lava Axe stopped working...

%%%%%%%%%%%%%%gen_effect/2 (3) 06/04/11

	% Not tested
/*	% Damage to each/all object(s) of a specified type, or each player
	gen_effect(Effect, Targets):- 
		(phrase(deals_to_each(Source,Damage,Targets,_Type), Effect);
		phrase(deals_to_all(Source,Damage,Targets,_TYpe), Effect)), 
		damage_many(Source, Damage, Targets).

	damage_many(_Source, _Damage, []).
 	damage_many(Source, Damage, [Target | Targets]):- 
		damage(Source, Target, Damage),
		damage_many(Source, Damage, Targets).
*/

%%%%%%%%%%%%%%gen_effect/2 (4) 28/04/11

	% Destroy target permanent.

	gen_effect(Effect, Target):- 
		phrase(destroy(_X,_Y), Effect), 
		% ^ Oh yes you can. Update the rest too.
		object_handle(Object, Target),
		owned_by(Object, Owner),
		move_to_zone(Owner, Object, 'Battlefield', 'Graveyard').

%%%%%%%%%%%%%%gen_effect/2 (5) 05/04/11

	% "Gets" (buff) effect
	gen_effect(Effect, Target):- 
		Effect = [target,'Creature',gets,Prefix_P,X,/,Prefix_T,Y,until,end,of,turn], 
		object_controller(Target, Controller), 
		buff_debuff(Target, Controller, [Prefix_P,X,/,Prefix_T,Y], 'until end of turn').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%permanent_Type/1 13/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	% This should replace the permanent_type in 
	%  cards.pl, since that doesn't make much sense 
	%  (in view of check_type) and nothing uses it anyway.
	permanent_Type(X):- 
		X = 'Artifact';
		X = 'Creature';
		X = 'Enchantment';
		X = 'Land';
		X = 'Planeswalker'.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%invert_case/2 13/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* invert_case/2(?Lower, ?Upper) */
	% Does what it says on the tin- just a shell 
	%  to make sure lwrupr does not fail when both 
	%  variables are bound (in which case this will not
	%  really do anything. 

	invert_case(Lower, Upper):- 
		type(Upper, 0), lwrupr(Lower, Upper); 
		type(Lower, 0), lwrupr(Lower, Upper);
		%true. 
		Lower = Upper.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              Notes                %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%             Section               %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%predicate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%child predicate
%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%clause

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* _*/

% Notes saved for player/you stuff in 15/03/11

%		( creature(Object,_,Abilities,_,_,_), 
%		member(['Haste'], Abilities) -> true ;
%		add_state(Object, 'summoning sickness', Updated), 
%		change_state(Object, 'Battlefield', Player, Updated) ),

/*
| ?- Aba = aba, write(Aba) ~> X, fread( a, 0, -1, A ) <~ X.
Aba = A = aba ,
X = `aba`

*/

%	target(X,Y,Z) --> [target], object(X,Y,Z).
	% ^ Not quite correct MGL; correct object(X,Y,Z) and try again. 	

/* Implement but correctly (add abilities on the Stack to objects)
	% An object is an ability on the Stack
	object(X) --> [X], 
		{zone('Stack', Objects), 
		member(X, Objects), 
		phrase(ability, X)}.
*/

/*
	spell_ability(Recipients),[target,Type] --> effect(Recipients,Type), target(Recipients,Type).

	return(_X,Y) --> (['Return', target, Y] ; [return, target, Y]),
		[to, its, 'owner''s', hand], {permanent_Type(Y)}.
*/


/*
More note-taking. You can do this:
| ?- phrase(effect(Tagret), ['Prodigal Pyromancer',deals,1,damage,to,target,'Creature',or,player],X).
Tagret = 'Prodigal Pyromancer' ,
X = [or,player] ;

Tagret = object('Prodigal Pyromancer' - 3,[]) ,
X = [] ;

Tagret = X = [] ;

Tagret = X = [] ;

Tagret = X = [] ;

Tagret = 'Player 1' ,
X = [] ;

Tagret = 'Glee-min' ,
X = [] ;

no

But you prefer to do this: 

| ?- phrase(effect(Tagret), ['Prodigal Pyromancer',deals,1,damage,to,target,'Creature',or,player],[]).
Tagret = object('Prodigal Pyromancer' - 3,[]) ;

Tagret = [] ;

Tagret = [] ;

Tagret = [] ;

Tagret = 'Player 1' ;

Tagret = 'Glee-min' ;

no

	( You need Prodigal on the Battlefield, else its ability 
	  is not valid, because it's not there to have it)

*/


/* Riiight. You can do this instead: 
	| ?- phrase(effect(X), ['Destroy',target,'Land'],Y).
	X = object('Mountain' - 1,[tapped]) ,
	Y = [] ;

	X = object('Mountain' - 2,[tapped]) ,
	Y = [] ;

	X = object('Mountain' - 3,[tapped]) ,
	Y = [] ;

	Or this: 

	| ?- phrase(effect(X,Y), ['Destroy',target,'Land'],Z).
	X = object('Mountain' - 1,[tapped]) ,
	Y = 'Land' ,
	Z = [] ;

	X = object('Mountain' - 2,[tapped]) ,
	Y = 'Land' ,
	Z = [] ;

	X = object('Mountain' - 3,[tapped]) ,
	Y = 'Land' ,
	Z = [] ;
*/


