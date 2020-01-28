## Items.DB and Title.DB file format

Text files that act as "databases". A | character is used as column/field seperator. Compared to the Xref.db file every line does NOT end with a seperator. 

**Items.db**  
All items with their properties - the same column can have different meaning depending on item type: Examples below shows the `Axe`, the `ElfBandQuiv` and `vsLootDrkHelm` items:

In table form:

| Column | Field name(s) | Value (Axe) | Value (ElfBandQuiv) | Value (vsLootDrkHelm)   
| ------ | ------------- | ----- | ----- | ----- |
| 0 | "ItemName" | Axe | ElfBandQuiv | vsLootDrkHelm  
| 1 | PiercingMin ||3|
| 2 | CrushingMin ||
| 3 | CuttingMin |1|
| 4 | HeatMin ||
| 5 | ColdMin ||
| 6 | ElectricMin||4|
| 7 | PoisonMin
| 8 | MagicMin
| 9 | MentalMin
| 10 | StunMin
| 11 | SpecialMin
| 12 | PiercingMax||5|
| 13 | CrushingMax|4|
| 14 | CuttingMax|6|4|
| 15 | HeatMax
| 16 | ColdMax
| 17 | ElectricMax||8|
| 18 | PoisonMax
| 19 | MagicMax
| 20 | MentalMax
| 21 | StunMax
| 22 | SpecialMax
| 23 | PiercingInv|||1
| 24 | CrushingInv|||1
| 25 | CuttingInv|||1
| 26 | HeatInv
| 27 | ColdInv
| 28 | ElectricInv
| 29 | PoisonInv
| 30 | MagicInv
| 31 | MentalInv
| 32 | StunInv|||1
| 33 | PiercingRes||2|6
| 34 | CrushingRes||3|9
| 35 | CuttingRes||3|9
| 36 | HeatRes
| 37 | ColdRes
| 38 | ElectricRes
| 39 | PoisonRes
| 40 | MagicRes
| 41 | MentalRes
| 42 | StunRes|||5
| 43 | StrengthSM
| 44 | CoordinationSM
| 45 | ConstitutionSM
| 46 | MysticismSM
| 47 | CombatSM
| 48 | StealthSM|||-3
| 49 | RestrictionSM||2|2
| 50 | AttackRecoverySM|7
| 51 | HitRecoverySM|||-4
| 52 | PerceptionSM
| 53 | CharmSM
| 54 | HealingRateSM
| 55 | RechargeRateSM
| 56 | HitPointsSM
| 57 | ManaSM
| 58 | AttackSM
| 59 | DefenseSM|||-2
| 60 | cSlotsAllowed|[Weapon]||[Helmet]
| 61 | cItemInfo|A large cutting device consisting of a sharpened wedge attached to a long handle.|A container for arrows that hangs at the waist. This quiver belt contains magical arrows that do Lightning damage|A protective shell for the head that covers everything from the neck up, providing unparalleled protection.
| 62 | cValue|2|500|6
| 63 | cWeight, cTitle
| 64 | cMagic
| 65 | cItemType|Weapon|Quiver|Armor
| 66 | \<missing>
| 67 | cSecretName
| 68 | cSecretInfo
| 69 | cInventoryImage|Axe.gif|QuiverBelt.gif|DrkHelm.gif
| 70 | cPartName|prt_Axe|prt_ElfBandQuiv|prt_DrkHelm
| 71 | cDisplayName|War Axe|Elven Quiver Belt|Great Helm|
| 72 | cInventoryHeight|104|156|52|
| 73 | cInventoryWidth|54|36|36|
| 74 | w2Handed, qFletchingColor, iMaterial|False|16777215|Metal
| 75 | wRange, qTracking|8|
| 76 | wMinStrength, qSndOther|6|
| 77 | wMinCoordination, qSndMetal|10|
| 78 | wMaxRestriction, qSndStone|45|
| 79 | wSndAttack|AxeMiss,SwordMiss|
| 80 | wSndOther|Axewfles,Axewleat|
| 81 | wSndMetal|Axewmet1,Axewmeta|

and in their raw form within the file:

`Axe|||1||||||||||4|6||||||||||||||||||||||||||||||||||||7||||||||||[Weapon]|A large cutting device consisting of a sharpened wedge attached to a long handle.|2|||Weapon||||Axe.gif|prt_Axe|War Axe|104|54|False|8|6|10|45|AxeMiss,SwordMiss|Axewfles,Axewleat|Axewmet1,Axewmeta`  

`ElfBandQuiv|3|||||4||||||5||4|||8||||||||||||||||2|3|3||||||||||||||2||||||||||||A container for arrows that hangs at the waist. This quiver belt contains magical arrows that do Lightning damage|500|||Quiver||||QuiverBelt.gif|prt_ElfBandQuiv|Elven Quiver Belt|156|36|16777215|`  

`vsLootDrkHelm|||||||||||||||||||||||1|1|1|||||||1|6|9|9|||||||5||||||-3|2||-4||||||||-2|[Helmet]|A protective shell for the head that covers everything from the neck up, providing unparalleled protection.|6|||Armor||||DrkHelm.gif|prt_DrkHelm|Great Helm|52|36|Metal`

So the prefixed names probably means c=character, w=weapon, q=quiver and i=item (Armor material) specific for the fields that have multiple purposes.  

Field 70 - **cPartName** is the key that is used to find the corresponding row in the xRef.db file - se the seperate documentation on that.

The file is used by the **PartManager**, together with the XRef.db file.

**Title.db**  
Contains "label-properties" - and these are added and removed throughout the game to act as "states" and "custom" properties - the file contains a small set of these.

Examples below shows the `Path Finding`, `Kynar`, `Engres` and `corvus` "titles":

In table form:

| Column | Field name(s) | Value (Path Finding) | Value (Kynar) | Value (Engres) | Value (Corvus)  
| ------ | ------------- | ----- | ----- | ----- | ---- |
| 0 | "Title" | Path Finding | Kynar | Engres | corvus 
| 1 | ttVisible|True|False|False|False
| 2 | ttStrength||7|3
| 3 | ttCoordination|2||11
| 4 | ttConstitution
| 5 | ttMysticism
| 6 | ttCombat||||25
| 7 | ttStealth|3|-10
| 8 | ttRestriction|-1|||-8
| 9 | ttAttackRecovery||-10|-10|-10
| 10 | ttHitRecovery||-5|-5|-5
| 11 | ttPerception
| 12 | ttCharm
| 13 | ttHealingRate||5|5|10
| 14 | ttRechargeRate||5|5
| 15 | ttHitPoints||25|15|75
| 16 | ttMana
| 17 | ttAttack||9|8|7
| 18 | ttDefense|0|3|6|5
| 19 | ttDisplayName

and in their raw form within the file:

`Path Finding|True||2||||3|-1||||||||||0`

`Kynar|False|7|||||-10||-10|-5|||5|5|25||9|3`

`Engres|False|3|11||||||-10|-5|||5|5|15||8|6`

`corvus|False|||||25||-8|-10|-5|||10||75||7|5`