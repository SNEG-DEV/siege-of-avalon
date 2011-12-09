unit SiegeTypes;

interface

const
  MaxCompanions = 5;
  //  ThresholdOfPain = 5;

  MaxItems = 2047;
  MaxTiles = 2047;
  ItemListSize = 32767;
  MaxScripts = 128;
  MaxScriptFrames = 64;
  MaxZones = 255;
  MaxLightStates = 4;
  WorkWidth = 384;
  WorkHeight = 160;
  MaxSubMaps = 127;
  MaxZoneHeight = 2048;

type
  TCastingType = ( ctCombat, ctHealing, ctDivination, ctSummoning, ctTranslocation, ctProtection, ctIllusion );

  TTargetType = ( ttNone, ttFriend, ttEnemy );

  TAIMode = ( aiNone, aiIdle, aiCombat, aiParty );

  TAIPriority = ( prNone, prAttack, prGuard, prFlee, prHide, prCast, prFollowClose, prFollowFar );

  TAIParameter = ( paNone, paAnyEnemy, paAnyAlly, paAnyPartyMember, paClosestEnemy, paStrongestEnemy,
    paWeakestEnemy, paMostMagicalEnemy, paAttacker, paSelf, paSpecificPartyMember, paPlayerTarget );

  TMaterial = ( maOther, maMetal );

  TNextAction = ( naNone, naWalk, naRun, naAttack, naCast );

  TScriptMode = ( smOnce, smRepeat, smRandom );

  TDamageRange = record
    Min : Single;
    Max : Single;
  end;

  TDamageResistance = record
    Invulnerability : Single;
    Resistance : Single;
  end;

  PDamageProfile = ^TDamageProfile;
  TDamageProfile = record
    Piercing : TDamageRange;
    Crushing : TDamageRange;
    Cutting : TDamageRange;
    Heat : TDamageRange;
    Cold : TDamageRange;
    Electric : TDamageRange;
    Poison : TDamageRange;
    Magic : TDamageRange;
    Mental : TDamageRange;
    Stun : TDamageRange;
    Special : TDamageRange;
  end;

  PDamageResistanceProfile = ^TDamageResistanceProfile;
  TDamageResistanceProfile = record
    Piercing : TDamageResistance;
    Crushing : TDamageResistance;
    Cutting : TDamageResistance;
    Heat : TDamageResistance;
    Cold : TDamageResistance;
    Electric : TDamageResistance;
    Poison : TDamageResistance;
    Magic : TDamageResistance;
    Mental : TDamageResistance;
    Stun : TDamageResistance;
  end;

  PStatModifier = ^TStatModifier;
  TStatModifier = record
    Strength : Integer;
    Coordination : Integer;
    Constitution : Integer;
    Mysticism : Integer;
    Combat : Integer;
    Stealth : Integer;
    Restriction : Integer;
    AttackRecovery : Integer;
    HitRecovery : Integer;
    Perception : Integer;
    Charm : Integer;
    HealingRate : Integer;
    RechargeRate : Integer;
    HitPoints : Integer;
    Mana : Integer;
    Attack : Integer;
    Defense : Integer;
    Visible : boolean;
    DisplayName : string;
  end;

  TDynamicWordArray = array of Word;
  TDynamicSmallIntArray = array of SmallInt;

  TFacing = ( fNW, fNN, fNE, fEE, fSE, fSS, fSW, fWW );

  TSlot = ( slLeg1, slBoot, slLeg2, slChest1, slChest2, slArm, slBelt, slChest3,
    slGauntlet, slOuter, slHelmet, slWeapon, slShield, slMisc1, slMisc2, slMisc3 );

  TSlotAllowed = set of TSlot;

  TLightSource = record
    X, Y, Z : longint;
    Intensity : double;
    Radius : integer;
  end;

  PGridInfo = ^GridInfo;
  GridInfo = packed record
    Figure : Pointer; //For collision detection
    FilterID : Smallint;
    TriggerID : Smallint;
    CollisionMask : Word;
    LineOfSightMask : Word;
    FilterMask : Word;
    TriggerMask : Word;
    Tile : array[ 0..4 ] of Word;
    Zone : array[ 0..4 ] of Byte;
    BitField : Byte; //Bit 7 denotes a diamond tile, Bit 6 is automap.
  end;

  PTileInfo = ^TileInfo;
  TileInfo = packed record
    ImageIndex : Word;
    Rows : Word;
    Columns : Word;
    Zone : Word;
    Element : Byte;
    Reserved : Byte;
  end;

  MapColumnHeaderInfo = packed record
    BaseLine : Longint;
    Top : Longint;
    Active : Boolean;
    Reserved : Byte;
  end;

  RowUpdateInfo = packed record
    Figure : Pointer; //The first figure on the row
    OverlapRow : Longint; //The last row that contains an item which could overlap this row
    DescendRow : Longint; //The first row which has an item that descends below its position to this row
    MaxHeight : Longint; //The tallest item on this row
    ItemIndex : Word; //The first item on the row
  end;

  PItemInfo = ^ItemInfo;
  ItemInfo = packed record
    Top : Longint;
    Left : Longint;
    Slope : Single;
    StripHeights : THandle;
    CollisionMasks : THandle;
    LineOfSightMasks : THandle;
    LightPoints : THandle;
    Width : Word;
    Height : Word;
    Strips : Word; //=roundup(Width/TileWidth)  Strips*Height=next Items top
    StripCount : Word;
    Used : Boolean;
    Visible : Boolean;
    AutoTransparent : Boolean;
    Vertical : Boolean;
  end;

  PItemInstanceInfo = ^ItemInstanceInfo;
  ItemInstanceInfo = packed record
    X : Longint;
    Y : Longint;
    ImageY : Word;
    Slope0 : Single;
    Slope1 : Single;
    Slope2 : Single;
    RefItem : word;
    FilterID : Smallint;
    XRayID : Smallint;
    ImageX : Word;
    Width : Word;
    Height : Word;
    VHeight : Word; //Height of region that may obscure objects behind it
    Next : Word;
    Zone : Word;
    AutoTransparent : Boolean;
    Visible : Boolean;
    Last : Boolean;
    Vertical : Boolean;
  end;

  ScriptInfo = packed record
    Frames : Word;
    FrameID : array[ 1..MaxScriptFrames ] of Word;
    Name : string[ 32 ];
    Multiplier : Word;
    Tag : Longint;
  end;

  BITMAP = record
    bmType : Longint;
    bmWidth : Longint;
    bmHeight : Longint;
    bmWidthBytes : Longint;
    bmPlanes : Integer;
    bmBitsPixel : Integer;
    bmBits : Pointer;
  end;

  TPixelFormat = ( pf555, pf565, pf888 );

  TFlicker = ( flNone, flCandle, flTorch, flFluorescent );

  TAniSpecialEffect = ( seNone, seTranslucent, seSilhouette, seInvert, seSpooky,
    seFunky, seWhite, seNegative, seStrange, seAdd, seSubtract,
    seMultiply );

implementation

end.
