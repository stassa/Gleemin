
if X-'Mountain' present and X >= 1 then 'Pistachio' eliminated with 40:40 certainty.

/*
:- op(500, yfx, blocks).

	heuristic_evaluation(Blocking_set, Value):-
		findall(Power, (member(_Blocker blocks Attacker , Blocking_set),
				Attacker:Power/_Toughness),
			Attackers_power), % Total attack power
		sum(Attackers_power, 0, Attack_power),
		findall(Blocker1 : Attacker1,
				( member(Blocker1 blocks Attacker1 , Blocking_set),
				Attacker1:Power1/_T,
				Blocker1:_Power/Toughness1,
				Toughness1 > Power1),
			Blockers), % All surviving blockers
		length(Blockers, Survivors),
		Value = Attack_power/Survivors.
*/
