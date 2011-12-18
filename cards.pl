% First saved: 10/01/11
% Last saved:  23/04/11
%
%	This is really Objects, though only card-based ones (not abilities)
%
% Doing: 
%	Done adding some tutorial cards
%	Done adding card_characteristics
%	* Added notes to check_type, creature_type
%	change mana abilities syntax to pure MGL.
%	putting each ability in a separate list. Done?
%	noticed card templates have state (might be useful)
%	adding rules text
% Todo
%
% NOTES:
%	add/remove/change_state must use the new object predicates, 
%	  but currently everything and their little sister uses them so
% 	  I won't update them yet. I added the correct forms though and 
%	  any new predicates (or old ones that I redo) should use those.
%	Same goes for tap_untap. 
%	Done added check_type pred
%	Capitalisation in abilities is not strictly Oracle- like.


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
%%%%           Cards Facts             %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	:- ensure_loaded('mtg_operators.pl').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%supertypes/1 03/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/*  supertypes(+Supertypes) */
	% Lists known supertypes

	supertypes(['Basic', 'Legendary', 'Ongoing', 'Snow', 'World']). 

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	Supertypes are _not_ formally connected with specific 
	types or subtypes. 
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%card_types/1 03/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/*  card_types(+Type) */ 
	% Lists known card Types

	% Known card types. 
	card_types([
			'Artifact',
			'Creature',
			'Enchantment',
			'Instant',
			'Land',
			'Plane',
			'Planeswalker',
			'Scheme',
			'Sorcery',
			'Tribal',
			'Vanguard'
			]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%subtypes/1 03/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/*  subtypes(+Type, +Subtypes) */
	% Lists known Subtypes of known Types

%%%%%%%%%%%%%%subtype/2 (1) 03/02/11

	% Artifact subtypes
	subtypes('Artifact', 
			[
				'Equipment',
				'Contraption',
				'Fortification'
			]
		).

%%%%%%%%%%%%%%subtype/2 (2) 03/02/11

	% Enchantment subtypes 
	subtypes('Enchantment', 
			[
				'Aura',
				'Shrine'
			]
		).

%%%%%%%%%%%%%%subtype/2 (3) 03/02/11

	% Land subtypes
	subtypes('Land', 
			[
				'Desert',
				'Forest',
				'Island',
				'Lair',
				'Locus',
				'Mine',
				'Mountain',
				'Plains',
				'Power-Plant',
				'Swamp',
				'Tower',
				'Urza''s'
			]
		).

%%%%%%%%%%%%%%subtype/2 (4) 03/02/11

	% Planeswalker subtypes
	subtypes('Planeswalker', 
			[
				'Ajani',
				'Bolas',
				'Chandra',
				'Elspeth',
				'Garruk',
				'Gideon',
				'Jace',
				'Koth',
				'Liliana',
				'Nissa',
				'Sarkhan',
				'Sorin',
				'Tezzeret',
				'Venser'
			]
		).

%%%%%%%%%%%%%%subtype/2 (5) 03/02/11	

	% Sorcery subtypes
	subtypes('Sorcery',
			 [
				'Arcane', 
				'Trap'
			]
		).

%%%%%%%%%%%%%%subtype/2 (6) 03/02/11

	% Instant subtypes
	subtypes('Instant',
			 [
				'Arcane', 
				'Trap'
			]
		).

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	Instant and Sorcery types share their subtypes, 
	 which are called spell types. 
	I should represent this with a rule. 
*/

%%%%%%%%%%%%%%subtype/2 (7) 03/02/11

	% Creature subtypes
	subtypes('Creature', 
			[
				'Advisor', 
				'Ally', 
				'Angel',
				'Anteater',
				'Antelope', 
				'Ape', 
				'Archer', 
				'Archon', 
				'Artificer', 
				'Assassin', 
				'Assembly'-
				'Worker', 
				'Atog', 
				'Aurochs', 
				'Avatar', 
				'Badger', 
				'Barbarian', 
				'Basilisk', 
				'Bat', 
				'Bear', 
				'Beast', 
				'Beeble', 
				'Berserker', 
				'Bird', 
				'Blinkmoth', 
				'Boar', 
				'Bringer', 
				'Brushwagg', 
				'Camarid', 
				'Camel', 
				'Caribou', 
				'Carrier', 
				'Cat', 
				'Centaur', 
				'Cephalid', 
				'Chimera', 
				'Citizen', 
				'Cleric', 
				'Cockatrice', 
				'Construct', 
				'Coward', 
				'Crab', 
				'Crocodile', 
				'Cyclops', 
				'Dauthi', 
				'Demon', 
				'Deserter', 
				'Devil', 
				'Djinn', 
				'Dragon', 
				'Drake', 
				'Dreadnought', 
				'Drone', 
				'Druid', 
				'Dryad', 
				'Dwarf', 
				'Efreet', 
				'Elder', 
				'Eldrazi', 
				'Elemental', 
				'Elephant', 
				'Elf', 
				'Elk', 
				'Eye', 
				'Faerie', 
				'Ferret', 
				'Fish', 
				'Flagbearer', 
				'Fox', 
				'Frog', 
				'Fungus', 
				'Gargoyle', 
				'Giant', 
				'Gnome', 
				'Goat', 
				'Goblin', 
				'Golem', 
				'Gorgon', 
				'Graveborn', 
				'Griffin', 
				'Hag', 
				'Harpy', 
				'Hellion', 
				'Hippo', 
				'Hippogriff', 
				'Homarid', 
				'Homunculus', 
				'Horror', 
				'Horse', 
				'Hound', 
				'Human', 
				'Hydra', 
				'Hyena', 
				'Illusion', 
				'Imp', 
				'Incarnation', 
				'Insect', 
				'Jellyfish', 
				'Juggernaut', 
				'Kavu', 
				'Kirin', 
				'Kithkin', 
				'Knight', 
				'Kobold', 
				'Kor', 
				'Kraken', 
				'Lammasu', 
				'Leech', 
				'Leviathan', 
				'Lhurgoyf', 
				'Licid', 
				'Lizard', 
				'Manticore', 
				'Masticore', 
				'Mercenary', 
				'Merfolk', 
				'Metathran', 
				'Minion', 
				'Minotaur', 
				'Monger', 
				'Mongoose', 
				'Monk', 
				'Moonfolk', 
				'Mutant', 
				'Myr', 
				'Mystic', 
				'Nautilus', 
				'Nephilim', 
				'Nightmare', 
				'Nightstalker', 
				'Ninja', 
				'Noggle', 
				'Nomad', 
				'Octopus', 
				'Ogre', 
				'Ooze', 
				'Orb', 
				'Orc', 
				'Orgg', 
				'Ouphe', 
				'Ox', 
				'Oyster', 
				'Pegasus', 
				'Pentavite', 
				'Pest', 
				'Phelddagrif', 
				'Phoenix', 
				'Pincher', 
				'Pirate', 
				'Plant', 
				'Prism', 
				'Rabbit', 
				'Rat', 
				'Rebel', 
				'Reflection', 
				'Rhino', 
				'Rigger', 
				'Rogue', 
				'Salamander', 
				'Samurai', 
				'Sand', 
				'Saproling', 
				'Satyr', 
				'Scarecrow', 
				'Scorpion', 
				'Scout', 
				'Serf', 
				'Serpent', 
				'Shade', 
				'Shaman', 
				'Shapeshifter', 
				'Sheep', 
				'Siren', 
				'Skeleton', 
				'Slith', 
				'Sliver', 
				'Slug', 
				'Snake', 
				'Soldier', 
				'Soltari', 
				'Spawn', 
				'Specter', 
				'Spellshaper', 
				'Sphinx', 
				'Spider', 
				'Spike', 
				'Spirit', 
				'Splinter', 
				'Sponge', 
				'Squid', 
				'Squirrel', 
				'Starfish', 
				'Surrakar', 
				'Survivor', 
				'Tetravite', 
				'Thalakos', 
				'Thopter', 
				'Thrull', 
				'Treefolk', 
				'Triskelavite', 
				'Troll', 
				'Turtle', 
				'Unicorn', 
				'Vampire', 
				'Vedalken', 
				'Viashino', 
				'Volver', 
				'Wall', 
				'Warrior', 
				'Weird', 
				'Whale', 
				'Wizard', 
				'Wolf', 
				'Wolverine', 
				'Wombat', 
				'Worm', 
				'Wraith', 
				'Wurm', 
				'Yeti', 
				'Zombie', 
				'Zubera'
			]
		).
	% Yes, this is my really long list of Mt:G creature subtypes. 
	% I would like to thank Notepad++ for its option to record macros. 

%%%%%%%%%%%%%%subtype/2 (8) 03/02/11

	% Tribal subtypes.
	subtypes('Tribal', 
			[
				'Advisor', 
				'Ally', 
				'Angel',
				'Anteater',
				'Antelope', 
				'Ape', 
				'Archer', 
				'Archon', 
				'Artificer', 
				'Assassin', 
				'Assembly'-
				'Worker', 
				'Atog', 
				'Aurochs', 
				'Avatar', 
				'Badger', 
				'Barbarian', 
				'Basilisk', 
				'Bat', 
				'Bear', 
				'Beast', 
				'Beeble', 
				'Berserker', 
				'Bird', 
				'Blinkmoth', 
				'Boar', 
				'Bringer', 
				'Brushwagg', 
				'Camarid', 
				'Camel', 
				'Caribou', 
				'Carrier', 
				'Cat', 
				'Centaur', 
				'Cephalid', 
				'Chimera', 
				'Citizen', 
				'Cleric', 
				'Cockatrice', 
				'Construct', 
				'Coward', 
				'Crab', 
				'Crocodile', 
				'Cyclops', 
				'Dauthi', 
				'Demon', 
				'Deserter', 
				'Devil', 
				'Djinn', 
				'Dragon', 
				'Drake', 
				'Dreadnought', 
				'Drone', 
				'Druid', 
				'Dryad', 
				'Dwarf', 
				'Efreet', 
				'Elder', 
				'Eldrazi', 
				'Elemental', 
				'Elephant', 
				'Elf', 
				'Elk', 
				'Eye', 
				'Faerie', 
				'Ferret', 
				'Fish', 
				'Flagbearer', 
				'Fox', 
				'Frog', 
				'Fungus', 
				'Gargoyle', 
				'Giant', 
				'Gnome', 
				'Goat', 
				'Goblin', 
				'Golem', 
				'Gorgon', 
				'Graveborn', 
				'Griffin', 
				'Hag', 
				'Harpy', 
				'Hellion', 
				'Hippo', 
				'Hippogriff', 
				'Homarid', 
				'Homunculus', 
				'Horror', 
				'Horse', 
				'Hound', 
				'Human', 
				'Hydra', 
				'Hyena', 
				'Illusion', 
				'Imp', 
				'Incarnation', 
				'Insect', 
				'Jellyfish', 
				'Juggernaut', 
				'Kavu', 
				'Kirin', 
				'Kithkin', 
				'Knight', 
				'Kobold', 
				'Kor', 
				'Kraken', 
				'Lammasu', 
				'Leech', 
				'Leviathan', 
				'Lhurgoyf', 
				'Licid', 
				'Lizard', 
				'Manticore', 
				'Masticore', 
				'Mercenary', 
				'Merfolk', 
				'Metathran', 
				'Minion', 
				'Minotaur', 
				'Monger', 
				'Mongoose', 
				'Monk', 
				'Moonfolk', 
				'Mutant', 
				'Myr', 
				'Mystic', 
				'Nautilus', 
				'Nephilim', 
				'Nightmare', 
				'Nightstalker', 
				'Ninja', 
				'Noggle', 
				'Nomad', 
				'Octopus', 
				'Ogre', 
				'Ooze', 
				'Orb', 
				'Orc', 
				'Orgg', 
				'Ouphe', 
				'Ox', 
				'Oyster', 
				'Pegasus', 
				'Pentavite', 
				'Pest', 
				'Phelddagrif', 
				'Phoenix', 
				'Pincher', 
				'Pirate', 
				'Plant', 
				'Prism', 
				'Rabbit', 
				'Rat', 
				'Rebel', 
				'Reflection', 
				'Rhino', 
				'Rigger', 
				'Rogue', 
				'Salamander', 
				'Samurai', 
				'Sand', 
				'Saproling', 
				'Satyr', 
				'Scarecrow', 
				'Scorpion', 
				'Scout', 
				'Serf', 
				'Serpent', 
				'Shade', 
				'Shaman', 
				'Shapeshifter', 
				'Sheep', 
				'Siren', 
				'Skeleton', 
				'Slith', 
				'Sliver', 
				'Slug', 
				'Snake', 
				'Soldier', 
				'Soltari', 
				'Spawn', 
				'Specter', 
				'Spellshaper', 
				'Sphinx', 
				'Spider', 
				'Spike', 
				'Spirit', 
				'Splinter', 
				'Sponge', 
				'Squid', 
				'Squirrel', 
				'Starfish', 
				'Surrakar', 
				'Survivor', 
				'Tetravite', 
				'Thalakos', 
				'Thopter', 
				'Thrull', 
				'Treefolk', 
				'Triskelavite', 
				'Troll', 
				'Turtle', 
				'Unicorn', 
				'Vampire', 
				'Vedalken', 
				'Viashino', 
				'Volver', 
				'Wall', 
				'Warrior', 
				'Weird', 
				'Whale', 
				'Wizard', 
				'Wolf', 
				'Wolverine', 
				'Wombat', 
				'Worm', 
				'Wraith', 
				'Wurm', 
				'Yeti', 
				'Zombie', 
				'Zubera'
			]
		).

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	And, unfortunately, Creature and Tribal types share their 
	  very many subtypes. The fact that those types are common 
	  can equally be expressed by a rule... but facts may also
	  be needed seperately. 
*/

%%%%%%%%%%%%%%subtype/2 (9) 03/02/11

/*
Alara, Arkhos, Bolas's Meditation Realm, Dominaria, Equilor, Iquatana, 
Ir, Kaldheim, Kamigawa, Karsus, Lorwyn, Luvion, Mercadia, 
Mirrodin, Moag, Muraganda, Phyrexia, Pyrulea, Rabiah, 
Rath, Ravnica, Segovia, Serra's Realm, Shadowmoor, 
Shandalar, Ulgrotha, Valla, Wildfire, and Zendikar
*/

	% Plane types, aka Planar types.
	subtypes('Plane', [
				'Alara',
				'Arkhos',
				'Bolas'' Meditation Realm', 
				'Dominaria',
				'Equilor',
				'Iquatana',
				'Ir', 
				'Kaldheim',
				'Kamigawa',
				'Karsus',
				'Lorwyn',
				'Luvion',
				'Mercadia', 
				'Mirrodin',
				'Moag',
				'Muraganda',
				'Phyrexia',
				'Pyrulea',
				'Rabiah',
				'Rath', 
				'Ravnica',
				'Segovia',
				'Serra''s Realm', 
				'Shadowmoor',
				'Shandalar', 
				'Ulgrotha',
				'Valla',
				'Wildfire',
				'Zendikar'
			]
		).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%card/1 04/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/*	card([
		card_name 		Name, 
		mana_cost 		Cost,
		type_line 		[Supertype, Type, Subtype], % are lists
		text_box 		Text,
		power 		Power, 
		toughness 		Toughness, 
		loyalty 		Loyalty 
		state			[[Status] | Other]
		]).	*/
	% Lists the parts of a card
	% Query with: 
		% card([Name, Cost, Type, Text, P, T, Loyalty, State]).
		% card([Name | _]).
		% card([card_name Name | _]).
	% etc.


%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	Update: state is not needed for card/1 instances, it's needed for object/3 
	  ones. I will leave this here for now because everything else is 
	  using it like that, but I should fix it. On the other hand, it may 
	  be useful as a starting state (for "comes into play tapped" etc).
	state is a list of Status = [Tapped, Flipped, Facing, Phased]
	  (where 
		Tapped : 'tapped' or 'untapped'
		Flipped: 'flipped' or 'unflipped'
		Facing: 'face_up' or 'face_down'
		Phased: 'phased_in' or 'phased_out')
	  followed by any number of other states the card can acquire during 
	  the game, eg. damage, counters, destroyed, attacking/blocking, 
	  regenerating etc etc. 
	All permanents normally start in: 
	  state [['untapped', 'unflipped', 'face_up', 'phased_in']]
	Any new state element added will probably have to be a tuple 
	  eg, damage(Amount), counter(Type) etc. 
	Finding the value of a particulare state element of a card can be 
	  done with member/2, eg: 
		state State, member(damage(Amount), State), process(Amount)
	  or for Status specifically:
		state State = [Tapped, Flipped, Facing, Phased | Rest],
			process(Tapped)
	Query with: 
	  card([card_name Name, _, _, _, _, _, _, state State]).
	  card([card_name Name, _, _, _, _, _, _, state [Status|_]]).
	  card([card_name Name, _, _, _, _, _, _, state [[Tapped |_]|_]]).
	  card([card_name Name, _, _, _, _, _, _, state [[Tapped, _, _, Phased]|_]]).
	etc.

	Abilities must be in MGL turned into a comma-separated list of 
	  Edinburgh-syntax Prolog atoms. Each ability must be in a separate
	  sub-list of the text_box field, eg: text_box [[Ability,1] [Ability,2]]. 
	Some MGL terms need to be capitalised to be recognised by Gleemin: 
		Type information ('Creature', not creature)
		Zone information ('Battlefield', not battlefield)
		"When", "Whenever" and "At" (always at the start of sentences?)
*/

/* Template
	card([
		card_name 		'', 
		mana_cost 		'',
		%illustration 	''
		type_line 		[[], [''], ['']],
		text_box		[],
		power 		[], 
		toughness 		[], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

*/

	% Land cards.
/*	card([
		card_name 		'Swamplains', 
		mana_cost 		0,
		%illustration 	'Plains'
		type_line 		[['Basic'], ['Land'], ['Plains']],
		%text_box 		[add_mana('tap', w, 'you'), add_mana('tap', b, 'you')],
		text_box 		[add_mana(['1', 'tap'], ['add', w, 'to your mana pool']), 
						add_mana(['tap', '1'], ['add', b, 'to your mana pool'])],
		power 		[], 
		toughness 		[], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']]
		]).
*/

	card([
		card_name 		'Plains', 
		mana_cost 		0,
		%illustration 	'Plains'
		type_line 		[['Basic'], ['Land'], ['Plains']],
		text_box 		[[tap,:,add,w,to,your,mana,pool]],
		power 		[], 
		toughness 		[], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']]
		]).


	card([
		card_name 		'Island', 
		mana_cost 		0,
		%illustration 	'Island'
		type_line 		[['Basic'], ['Land'], ['Island']],
		text_box 		[[tap,:,add,u,to,your,mana,pool]],
		power 		[], 
		toughness 		[], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).


	card([
		card_name 		'Swamp', 
		mana_cost 		0,
		%illustration 	'Swamp'
		type_line 		[['Basic'], ['Land'], ['Swamp']],
		text_box 		[[tap,:,add,b,to,your,mana,pool]],
		power 		[], 
		toughness 		[], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Mountain', 
		mana_cost 		0,
		%illustration 	'Mountain'
		type_line 		[['Basic'], ['Land'], ['Mountain']],
		text_box 		[[tap,:,add,r,to,your,mana,pool]],
		power 		[], 
		toughness 		[], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Forest', 
		mana_cost 		0,
		%illustration 	'Forest'
		type_line 		[['Basic'], ['Land'], ['Forest']],
		text_box 		[[tap,:,add,g,to,your,mana,pool]],
		power 		[], 
		toughness 		[], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	
	% Spells: 

	card([
		card_name 		'Aether Adept', 
		mana_cost 		'1uu',
		%illustration 	'Aether Adept'
		type_line 		[[], ['Creature'], ['Human', 'Wizard']],
		text_box		[['When','Aether Adept',enters,the,'Battlefield',(','),return,target,'Creature',to,its,'owner''s',hand]],
		power 		[2], 
		toughness 		[2], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Air Servant', 
		mana_cost 		'4u',
		%illustration 	'Air Servant'
		type_line 		[[], ['Creature'], ['Elemental']],
		text_box		[['Flying'],[2,u,:,tap,target,'Creature',with,'Flying']],
		power 		[4], 
		toughness 		[3], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Armored Cancrix', 
		mana_cost 		'4u',
		%illustration 	'Armored Cancrix'
		type_line 		[[], ['Creature'], ['Crab']],
		text_box		[],
		power 		[2], 
		toughness 		[5], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Assassinate', 
		mana_cost 		'2b',
		%illustration 	'Assassinate'
		type_line 		[[], ['Sorcery'], []],
		text_box		[['Destroy',target,tapped,'Creature']],
		power 		[2], 
		toughness 		[5], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Azure Drake', 
		mana_cost 		'3u',
		%illustration 	'Azure Drake'
		type_line 		[[], ['Creature'], ['Drake']],
		text_box		[['Flying']],
		power 		[2], 
		toughness 		[4], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Birds of Paradise', 
		mana_cost 		'g',
		%illustration 	'Birds of Paradise'
		type_line 		[[], ['Creature'], ['Bird']],
		text_box		[['Flying'], [tap,:,add,one,mana,of,any,color,to,your,mana,pool]],
		power 		[0], 
		toughness 		[1], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Boomerang', 
		mana_cost 		'uu',
		%illustration 	'Boomerang'
		type_line 		[[], ['Instant'], []],
		text_box		[['Return',target,permanent,to,its,'owner''s',hand]],
		power 		[0], 
		toughness 		[0], 
		loyalty 		[], 
		state 		[[]] 
		]).

	card([
		card_name 		'Canyon Minotaur', 
		mana_cost 		'3r',
		%illustration 	'Canyon Minotaur'
		type_line 		[[], ['Creature'], ['Minotaur', 'Warrior']],
		text_box		[],
		power 		[3], 
		toughness 		[3], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Child of Night', 
		mana_cost 		'1b',
		%illustration 	'Canyon Minotaur'
		type_line 		[[], ['Creature'], ['Vampire']],
		text_box		[['Lifelink']],
		power 		[2], 
		toughness 		[1], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Demolish', 
		mana_cost 		'3r',
		%illustration 	'Demolish'
		type_line 		[[], ['Sorcery'], []],
		text_box		[['Destroy',target,artifact,or,land]],
		power 		[], 
		toughness 		[], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Flensermite', 
		mana_cost 		'1b',
		%illustration 	''
		type_line 		[[], ['Creature'], ['Gremlin']],
		text_box		[['Infect'], ['Lifelink']],
		power 		[1], 
		toughness 		[1], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Giant Growth', 
		mana_cost 		'g',
		%illustration 	'Giant Growth'
		type_line 		[[], ['Instant'], []],
		text_box		[[target,'Creature',gets,+,3,/,+,3,until,end,of,turn]],
		power 		[], 
		toughness 		[], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Giant Spider', 
		mana_cost 		'3g',
		%illustration 	'Giant Spider'
		type_line 		[[], ['Creature'], ['Spider']],
		text_box		[['Reach']],
		power 		[2], 
		toughness 		[4], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Goblin Balloon Brigade', 
		mana_cost 		'r',
		%illustration 	'Goblin Balloon Brigade'
		type_line 		[[], ['Creature'], ['Goblin', 'Warrior']],
		text_box		[[r,:,'Goblin Balloon Brigade',gains,'Flying',until,end,of,turn]], 
		power 		[1], 
		toughness 		[1], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Goblin Piker', 
		mana_cost 		'1r',
		%illustration 	'Goblin Piker'
		type_line 		[[], ['Creature'], ['Goblin', 'Warrior']],
		text_box		[],
		power 		[2], 
		toughness 		[1], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Lava Axe', 
		mana_cost 		'4r',
		%illustration 	'Lava Axe'
		type_line 		[[], ['Sorcery'], []],
		text_box		[['Lava Axe',deals,5,damage,to,target,player]],
		power 		[], 
		toughness 		[], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Lightning Bolt', 
		mana_cost 		'r',
		%illustration 	'Lightning Bolt'
		type_line 		[[], ['Instant'], []],
		text_box		[['Lightning Bolt',deals,3,damage,to,target,'Creature',or,target,player]], 
																				% ^^^^^ Swi, 29/06/11
		power 		[], 
		toughness 		[], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Llanowar Elves', 
		mana_cost 		'g',
		%illustration 	'Llanowar Elves'
		type_line 		[[], ['Creature'], ['Elf','Druid']],
		text_box		[[tap,:,add,g,to,your,mana,pool]],
		power 		[1], 
		toughness 		[1], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Maritime Guard', 
		mana_cost 		'1u',
		%illustration 	'Maritime Guard'
		type_line 		[[], ['Creature'], ['Merfolk', 'Soldier']],
		text_box		[],
		power 		[1], 
		toughness 		[3],  
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Naturalize', 
		mana_cost 		'1g',
		%illustration 	'Naturalize'
		type_line 		[[], ['Instant'], []],
		text_box		[['Destroy',target,'Artifact',or,'Enchantment']],
		power 		[], 
		toughness 		[], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Nature''s Spiral', 
		mana_cost 		'1g',
		%illustration 	'Nature''s Spiral'
		type_line 		[[], ['Sorcery'], []],
		text_box		[['Return',target,permanent,card,from,your,'Graveyard',to,your,'Hand']],
		power 		[], 
		toughness 		[], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Plummet', 
		mana_cost 		'1g',
		%illustration 	'Plummet'
		type_line 		[[], ['Instant'], []],
		text_box		[['Destroy',target,'Creature',with,'Flying']],
		power 		[], 
		toughness 		[], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Prodigal Pyromancer', 
		mana_cost 		'2r',
		%illustration 	'Prodigal Pyromancer'
		type_line 		[[], ['Creature'], ['Human', 'Wizard']],
		text_box		[[tap,:,'Prodigal Pyromancer',deals,1,damage,to,target,'Creature',or,target,player]],
																							%^^^^ Swi, 29/06/11
		power 		[1], 
		toughness 		[1], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Puncture Blast', 
		mana_cost 		'2r',
		%illustration 	''
		type_line 		[[], ['Instant'], []],
		text_box		[['Wither'], ['Puncture Blast',deals,3,damage,to,target,'Creature',or,player]],
		power 		[], 
		toughness 		[], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Pyroclasm', 
		mana_cost 		'1r',
		%illustration 	'Pyroclasm'
		type_line 		[[], ['Sorcery'], []],
		text_box		[['Pyroclasm',deals,2,damage,to,each,'Creature']],
		power 		[], 
		toughness 		[], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Runeclaw Bear', 
		mana_cost 		'1g',
		%illustration 	'Runeclaw Bear'
		type_line 		[[], ['Creature'], ['Bear']],
		text_box		[],
		power 		[2], 
		toughness 		[2], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Spined Wurm', 
		mana_cost 		'4g',
		%illustration 	'Spined Wurm'
		type_line 		[[], ['Creature'], ['Wurm']],
		text_box		[],
		power 		[5], 
		toughness 		[4], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Unsummon', 
		mana_cost 		'u',
		%illustration 	'Unsummon'
		type_line 		[[], ['Instant'], []],
		text_box		[['Return',target,'Creature',to,its,'owner''s',hand]],
		power 		[], 
		toughness 		[], 
		loyalty 		[], 
		state 		[] 
		]).

	card([
		card_name 		'Vulshok Berserker', 
		mana_cost 		'3r',
		%illustration 	''
		type_line 		[[], ['Creature'], ['Human', 'Berserker']],
		text_box		[['Haste']],
		power 		[3], 
		toughness 		[2], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).


	% Tutorial cards

	card([
		card_name 		'Air Servant+', 
		mana_cost 		u, % '4u',
		%illustration 	'Air Servant'
		type_line 		[[], ['Creature'], ['Elemental']],
		text_box		[['Flying'],[2,u,:,tap,target,'Creature',with,'Flying']],
		power 		[4], 
		toughness 		[1], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Goblin Balloon Brigade+', 
		mana_cost 		'r',
		%illustration 	'Goblin Balloon Brigade'
		type_line 		[[], ['Creature'], ['Goblin', 'Warrior']],
		text_box		[['Lifelink'], ['Infect'], ['Wither']], % used for testing!	
		power 		[1], 
		toughness 		[1], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Maritime Guard+', 
		mana_cost 		'u',	
		%illustration 	'Maritime Guard'
		type_line 		[[], ['Creature'], ['Merfolk', 'Soldier']],
		text_box		[],
		power 		[1], 
		toughness 		[1],  
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Prodigal Pyromancer+', 
		mana_cost 		'r',
		%illustration 	'Prodigal Pyromancer'
		type_line 		[[], ['Creature'], ['Human', 'Wizard']],
		text_box		[[tap,:,'Prodigal Pyromancer',deals,1,damage,to,target,'Creature',or,target,player]],
																							% ^^^^ Swi, 29/06/11
		power 		[1], 
		toughness 		[1], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Vulshok Berserker+', 
		mana_cost 		'1r',
		%illustration 	''
		type_line 		[[], ['Creature'], ['Human', 'Berserker']],
		text_box		[['Haste']],
		power 		[3], 
		toughness 		[2], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%card/2 04/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* card(+Name, +ID, ?State) */	
	% This is how permanents and objects should be represented, ie 
	%  cards on the battlefield and on the stack. 
	
	% Example: 
	%card('Plains', 1, ['tapped']).
	%card('Ajani''s Pridemate', 1, [counters('+1/+1', 2), damage(2)]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%        Cards Rules                %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%change_state/4 23/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/*change_state(+Object, + Zone, +Player, +New_Object)*/
	% Change the State of an Object
	% Call when any change of state should occur.

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	Object, New_Object : object(Name - Id, State)
*/
	change_state(Object, Zone, Player, New_Object):- 
		%Object = object(Name - Id, _State), 
		zone(Player, Zone, Partition),
		member(Object, Partition),
		remove(Object, Partition, New_partition),
		New_object = New_Object, 
		append([New_object], New_partition, Full),
		retractall(zone(Player, Zone, _)), 
		asserta(zone(Player, Zone, Full)), 
		update_shared_zone(Zone).

	change_state(Handle, Zone, Player, New_Object):- 
		%Handle = Name - Id, 
		object_handle(Object, Handle), 
		zone(Player, Zone, Partition),
		member(Object, Partition),
		remove(Object, Partition, New_partition),
		New_object = New_Object, 
		append([New_object], New_partition, Full),
		retractall(zone(Player, Zone, _)), 
		asserta(zone(Player, Zone, Full)), 
		update_shared_zone(Zone).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%add_state/3 23/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* add_state(+Object, +State_to_add, -Updated) */
	% adds a state to an Object's State list

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	Object : object(Name - Id, State)
	State_to_add, Updated : term (valid state)
*/
	add_state(Object, Addend, Updated):- 
		Object = object(Name - Id, State),
		append([Addend], State, New_state),
		Updated = object(Name - Id, New_state).

	add_state(Handle, Addend, Updated):- 
		Handle = Name-Id,
		object_state(Handle, State),
		append([Addend], State, New_state),
		Updated = object(Name - Id, New_state).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%remove_state/3 23/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* remove_state(+Object, +State_to_remove, -Updated) */
	% removes a state to an Object's State list
	% Will succeed without warning if the state is not present. 

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	Object : object(Name - Id, State)
	State_to_remove, Updated : term (must be a valid state)
*/
	remove_state(Object, Subtrahend, Updated):- 
		Object = object(Name - Id, State),
		remove(Subtrahend, State, New_state),
		Updated = object(Name - Id, New_state).

	remove_state(Handle, Subtrahend, Updated):- 
		Handle = Name-Id, 
		object_state(Handle, State),
		remove(Subtrahend, State, New_state),
		Updated = object(Name - Id, New_state).

	remove_state(Handle, _Subtrahend, Object):- 
		object_handle(Object, Handle). 

	remove_state(Object, _Subtrahend, Object).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%remove_state/3 23/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* remove_state(+Object, +State_to_remove, -Updated) */
	% removes all instances of a state to an Object's State list

	remove_state_all(Object, Subtrahend, Updated):- 
		Object = object(Name - Id, State),
		removeall(Subtrahend, State, New_state),
		Updated = object(Name - Id, New_state).

	remove_state_all(Handle, Subtrahend, Updated):- 
		Handle = Name-Id, 
		object_state(Handle, State),
		removeall(Subtrahend, State, New_state),
		Updated = object(Name - Id, New_state).


%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	List of known valid states:

	_State_			_Object type_			_Implemented_
	'tap'				permanent				?/0?/11?
	'untap'			permanent				?/0?/11?
	countered			spell	
	destroyed			permanent
	discarded			card (in Hand)
	sacrificed			permanent
	regenerating		permanent
	damage(Amount)		permanent				03/03/11
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%tap_untap/3 05/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/*  tap_untap(+Permanent, +Controller, +Command) */
	% Taps or untaps a Permanent

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	+Permanent: object(Name - Id, State)
	+Command:  'tap' or 'untap'
 
	All Permanents begin with an empty State == []. Invoking tap_untap/3 
	  on a Permanent, adds a quoted atom, 'tap' or 'untap' to its State, 
	 depending on the value of Command:
	_Command_		_Added to State_
	'tap'			'tapped'
	'untap'		'untapped'

	A Permanent is only considered tapped if it has the atom 'tap' in its
	  State. A Permanent that is not tapped is considered untapped. Any
	  predicates checking for the tapped/untapped state of a permanent
	  should check for the absence of 'tapped' rather than the presence of 
	  'untapped'. Really, 'untapped' is there only for clarity while 
	  debugging- and playing!  
*/

	% Tap or untap a Permanent controlled by Player. 
 	tap_untap(Permanent, Player, Command):-
		(Permanent = object(_Name - _Id, State); object_state(Permanent, State) ),
			(
				Command == 'tap' -> Before = 'untapped', After = 'tapped';
				Command == 'untap' -> Before = 'tapped', After = 'untapped'
			), 
		\+ member(After, State), 		
			(				
				member(Before, State)-> 
				remove_state(Permanent, Before, Updated),
				% Updated = object(Name - Id, New_state),  
				add_state(Updated, After, Full); 
				add_state(Permanent, After, Full)
			),
		change_state(Permanent, 'Battlefield', Player, Full). 


	% Total hack. attacker/3 needs the tapped state of a creature returned.
	% Fix!!!
 	tap_untap(Permanent, Player, Command, Full):-
		(Permanent = object(_Name - _Id, State); object_state(Permanent, State) ),
			(
				Command == 'tap' -> Before = 'untapped', After = 'tapped';
				Command == 'untap' -> Before = 'tapped', After = 'untapped'
			), 
		\+ member(After, State), 		
			(				
				member(Before, State)-> 
				remove_state(Permanent, Before, Updated),
				% Updated = object(Name - Id, New_state),  
				add_state(Updated, After, Full); 
				add_state(Permanent, After, Full)
			),
		change_state(Permanent, 'Battlefield', Player, Full). 


%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	Btw, for testing purposes:
	  asserta(zone( 'Player 2', 'Battlefield', [object('Plains',3,['untapped'])] )).
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%check_type/4 24/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* check_type(?Object, ?Supertype, ?Type, ?Subtype) */
	% Checks the type line of an Object, or queries the database
	%  for Objects of a given super/type/sub. 

	check_type(Object, Supertype, Type, Subtype):-
		card([card_name Object, _, type_line Type_line, _, _, _, _, _]),
		Type_line = [Supertype, Type, Subtype].

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	Object : object(Name - Id, [State])
	Supertype, Type, Subtype: ['Type']
	
	Note that an object can have a mixed type, 
	  eg, 'Artifact Creature' - 204.2b 
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%permanent_type/3 03/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	permanent_type(Object):- 
		check_type(Object, _Supertype, Type, _Subtype),
		(
			Type == ['Artifact'];
			Type == ['Creature'];
			Type == ['Enchantment'];
			Type == ['Land'];
			Type == ['Planeswalker']
		).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%creature_type/3 03/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	creature_type(Object):- 
		check_type(Object, _Supertype, ['Creature'], _Subtype).
	% This doesn't cover artifact creatures etc mixed creature
	%  types- use member/2 instead:
	%  	check_type(Object, _Supertype, Type, _Subtype),
	% 	member('Creature', Type).
	% Implement and test


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%check_subtype/3 03/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* check_subtype(?Type, ?Subtypes, ?Subtype) */
	% Check an Object's Type or Subtype 
	%  or find the known Subtypes of a Type in the database. 

	check_subtype(Type, Subtypes, Subtype):-
		subtypes(Type, Subtypes), 
		member(Subtype, Subtypes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%put_counter/4 29/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* put_counter(+Permanent, +Controller, +Type, +Quantity ) */
	% Adds Quantity counters of Type to the State of Permanent. 

	% Permanent = Name-Id
	put_counter(Permanent, Controller, Type, Quantity):- 
		object_state(Permanent, State), 
		( member(counter(Type, X), State) ; X = 0 ), 
		Total is Quantity + X,
		remove_state(Permanent, counter(Type, X), Removed), 
		add_state(Removed, counter(Type, Total), Added), 
		change_state(Permanent, 'Battlefield', Controller, Added). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%remove_counter/4 29/03/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* remove_counter(+Permanent, +Controller, +Type, +Quantity ) */
	% Removes Quantity counters of Type from the State of Permanent. 

%%%%%%%%%%%%%%remove_counter/4 (1) 29/03/11

	% All counters of a type are removed from the permanent
	remove_counter(Permanent, Controller, Type, Quantity):- 
		object_state(Permanent, State), 
		member(counter(Type, Quantity), State),
		remove_state(Permanent, counter(Type, Quantity), Removed), 
		change_state(Permanent, 'Battlefield', Controller, Removed). 

%%%%%%%%%%%%%%remove_counter/4 (2) 29/03/11

	% Some counters of Type to remain on Permanent
	remove_counter(Permanent, Controller, Type, Quantity):- 
		object_state(Permanent, State), 
		( member(counter(Type, X), State) ; X = 0 ), 
		X > Quantity, Total is X - Quantity,
		remove_state(Permanent, counter(Type, X), Removed), 
		add_state(Removed, counter(Type, Total), Added), 
		change_state(Permanent, 'Battlefield', Controller, Added). 

%%%%%%%%%%%%%%remove_counter/4 (0) 29/03/11

	% Not enough counters to remove, or something else failed.
	remove_counter(Handle, _Controller, Type, Quantity):- 
		object_handle(Object, Handle), 
		% ^ Temp fix
		write(`Failed to remove` - Quantity - Type - ` counters from ` - Object), nl.


%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	Known types of counters: 
	'+1/+1'
	'-1/-1'*

	Note:
	  a) this is a departure from the MGL interpeter's definition of 
	  counters (the interpeter should be amended 'cause this is simpler)
	  b) Counters are added individually but the total bonus or malus is 
	  calculated collectively. It's not a good idea to lump all +X/+X or 
	  -X/-X counters into one. It's trivial to calculate the full modifier
	  from the individual counters and their quantity. On the other hand, 
	  an effect may appear that changes counters' values and that would not
	  be trivial to accommodate without further hacking/ fudging. 

*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%buff_debuff/4 05/04/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* buff_debuff(+Creature, Controller, +Type, +Duration) */
	% Adds a power and/or toughness modifier to a creature's State

	% Permanent: Name-Id
	buff_debuff(Permanent, Controller, Type, Duration):- 
		%Type = [Prefix_P,X,/,Prefix_T,Y],
		atom_to_list(Modifier, Type), 
		%object_state(Permanent, State), 
		add_state(Permanent, gets(Modifier, Duration), Added),
		change_state(Permanent, 'Battlefield', Controller, Added). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%card_characteristics/1 22/04/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* card_characteristics(+Name) */
	% Nicely prints out a card's characteristics

	card_characteristics(Name):- 
	card([
		card_name 		Name, 
		mana_cost 		Cost,
		type_line 		Types,
		text_box		Abilities,
		power 		Power, 
		toughness 		Toughness, 
		loyalty 		Loyalty, 
		state 		_Nevermind
		]),
	all_types(Types, [], Type_line),  
	(Power = [] -> POwer = '-' ; [POwer] = Power ),
	(Toughness = [] -> TOughness = '-' ; [TOughness] = Toughness),
	(Loyalty = [] -> LOyalty = '-' ; [LOyalty] = Loyalty),
	atom_word_list([Type_line], [], [TypeLine]), 
	atom_word_list(Abilities, [], TextBox), 
	text_box(TextBox, [], Text_box), 
	append([['Card Name',	Name], 
		['Mana Cost',	Cost],
		['Type Line',	TypeLine]],
		Text_box, Step1), 
	append(Step1, [['Power', 	POwer], 
			['Toughness', 	TOughness], 
			['Loyalty',		LOyalty]], Full), 
	output(card_characteristics, [info, Full]). 


%%%%%%%%%%%%%%text_box/3 22/04/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* text_box(Text_box, [], Abilities) */
	% Converts a nested list of abilities to a list of 
	% ['Text Box', Ability], needed for pretty printing. 
	
	text_box([], Abilities, Abilities). 
	text_box([[ ]| Rest], Temp, Abilities):- 
		text_box(Rest, Temp, Abilities).
	text_box([Ability | Rest], Temp, Abilities):- 
		append(['Text Box'], [Ability], Full),
		append(Temp, [Full], New_temp),  
		text_box(Rest, New_temp, Abilities). 
	

%%%%%%%%%%%%%%all_types/3 22/04/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* all_types(+Type_line, [], Types) */
	% Converts the nested list of Super/Type/Sub to 
	%  a format more appropriate for printing; it also 
	%  adds a "--" between a type and subtype(s)

	all_types([], Types, Types). 
	all_types([[]| Rest], Temp, Types):- 
		all_types(Rest, Temp, Types).
	all_types([Rest], Temp, Types):-
		append([--], Rest, Full),
		append(Temp, Full, New_temp), 
		all_types([], New_temp, Types).
	all_types([Type | Rest], Temp, Types):- 
		append(Temp, Type, New_temp), 
		all_types(Rest, New_temp, Types). 




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              Notes                %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
/*

	card('Ajani''s Pridemate', 0):- 
		card_name 		'Ajani''s Pridemate', 
		card_copy 		0, 
		mana_cost 		[1, w],
		%illustration 	'Ajani''s Pridemate'
		type_line 		['Creature', 'Cat', 'Soldier'],
		text_box 		['Whenever you gain life, 
			   		you may put a +1/+1 counter 
					on Ajani''s Pridemate.'],
		power 		2, 
		toughness 		2, 
		loyalty 		[]. 


	card('Angelic Arbiter', 0):-
		card_name 		'Angelic Arbiter', 
		card_copy 		0, 
		mana_cost 		[5, w, w],
		%illustration 	'Angelic Arbiter'
		type_line 		['Creature', 'Angel'],
		text_box 		['Flying', 
					'Each opponent who cast a spell 
					this turn can''t attack with creatures.', 
					'Each opponent who attacked with a creature 
					this turn can''t cast spells.'],
		power 		5,
		toughness 		6, 
		loyalty		[]. 
*/


