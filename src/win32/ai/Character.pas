unit Character;
(*
  Siege Of Avalon : Open Source Edition

  Portions created by Digital Tome L.P. Texas USA are
  Copyright ©1999-2000 Digital Tome L.P. Texas USA
  All Rights Reserved.

  Portions created by Team SOAOS are
  Copyright (C) 2003 - Team SOAOS.

  Portions created by Steffen Nyeland are
  Copyright (C) 2019 - Steffen Nyeland.

  Contributor(s):
  Dominique Louis <Dominique@SavageSoftware.com.au>
  Steffen Nyeland

  You may retrieve the latest version of this file at:
  https://github.com/SteveNew/Siege-of-Avalon-Open-Source

  The contents of this file maybe used with permission, subject to
  the GNU Lesser General Public License Version 2.1 (the "License"); you may
  not use this file except in compliance with the License. You may
  obtain a copy of the License at https://opensource.org/licenses/LGPL-2.1

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  Description:

  Requires: Delphi 10.3.3 or later

  Revision History:
  - 13 Jul 2003 - DL: Initial Upload to CVS
  - 10 Mar 2019 - SN: Forked on GitHub
  see git repo afterwards

*)

interface

uses
  System.Classes,
  System.UITypes,
  System.Generics.Collections,
  Winapi.Windows,
  SoAOS.Map,
  SoAOS.Animation,
  SoAOS.Types,
  Vcl.Graphics,
  // Winapi.DirectDraw,
  DirectX,
  DXEffects,
  SoAOS.Graphics.Types,
  SoAOS.AI.Types,
  SoAOS.AI,
  DFX,
  Resource;

const
  MaxCompanions = 5;
  // ThresholdOfPain = 5;

procedure PlaySound(const Sounds: TDynamicSmallIntArray; X, Y: longint);
procedure PlaySingleSound(Sound: SmallInt; X, Y: longint);

type
  TMaterial = (maOther, maMetal);

  TNextAction = (naNone, naWalk, naRun, naAttack, naCast);

  TCharacter = class;
  TCompanionCharacter = class;

  TDamageRange = record
    Min: Single;
    Max: Single;
  end;

  TDamageResistance = record
    Invulnerability: Single;
    Resistance: Single;
  end;

  PDamageProfile = ^TDamageProfile;

  TDamageProfile = record
    Piercing: TDamageRange;
    Crushing: TDamageRange;
    Cutting: TDamageRange;
    Heat: TDamageRange;
    Cold: TDamageRange;
    Electric: TDamageRange;
    Poison: TDamageRange;
    Magic: TDamageRange;
    Mental: TDamageRange;
    Stun: TDamageRange;
    Special: TDamageRange;
  end;

  PDamageResistanceProfile = ^TDamageResistanceProfile;

  TDamageResistanceProfile = record
    Piercing: TDamageResistance;
    Crushing: TDamageResistance;
    Cutting: TDamageResistance;
    Heat: TDamageResistance;
    Cold: TDamageResistance;
    Electric: TDamageResistance;
    Poison: TDamageResistance;
    Magic: TDamageResistance;
    Mental: TDamageResistance;
    Stun: TDamageResistance;
  end;

  PStatModifier = ^TStatModifier;

  TStatModifier = record
    Strength: Integer;
    Coordination: Integer;
    Constitution: Integer;
    Mysticism: Integer;
    Combat: Integer;
    Stealth: Integer;
    Restriction: Integer;
    AttackRecovery: Integer;
    HitRecovery: Integer;
    Perception: Integer;
    Charm: Integer;
    HealingRate: Integer;
    RechargeRate: Integer;
    HitPoints: Integer;
    Mana: Integer;
    Attack: Integer;
    Defense: Integer;
    Visible: boolean;
    DisplayName: string;
  end;

  TEffect = class(TObject)
  private
    PlayMode: TScriptMode;
    FFrame: Word;
    FScriptFrame: Word;
    Script: TScript;
    Delay: longint;
    FrameMultiplier: Word;
    FResource: TResource;
    procedure UpdateScript;
    procedure SetResource(const Value: TResource);
  public
    tag: Integer; // For Dup Checking
    ColorR, ColorG, ColorB: Integer;
    ApplyColor: boolean;
    EffectR, EffectG, EffectB: Integer;
    SpecialEffect: TAniSpecialEffect;
    Alpha: Integer;
    UseCustom: boolean;
    StatModifier: TStatModifier;
    Resistance: TDamageResistanceProfile;
    Duration: Integer;
    AnimationDuration: Integer;
    Power: Integer;
    Keyed: boolean;
    // This effect uses the target characters frame instead of its own script
    DisableWhenDone: boolean;
    procedure Refresh(NewEffect: TEffect); virtual;
    procedure Adjust(Character: TCharacter); virtual;
    procedure RenderLocked(Figure: TAniFigure; Bits: PBITPLANE); virtual;
    procedure DoAction(const Action: string; Facing: TFacing);
    function DoFrame: boolean; virtual;
    function Hit(Source: TAniFigure; Damage: PDamageProfile): boolean; virtual;
    property Resource: TResource read FResource write SetResource;
  end;

  TAbstractObject = class(TGameObject)
  public
    constructor Create(X, Y, Z: longint);
  end;

  TSoundPlayer = class(TAbstractObject)
  private
    FSoundOn: boolean;
    FVolume: Integer;
    FFileName: string;
    Sounds: TDynamicSmallIntArray;
    Playing: boolean;
    FContinuous: boolean;
    ContinuousIndex: Integer;
    FInterval: Integer;
    MaxInterval: Integer;
    NoMaxSpecified: boolean;
    procedure SetSoundOn(const Value: boolean);
    procedure SetVolume(const Value: Integer);
    procedure SetFilename(const Value: string);
    procedure SetContinuous(const Value: boolean);
    procedure SetInterval(const Value: Integer);
  protected
    RandomInterval: boolean;
    IntervalCount: Integer;
    function GetProperty(const Name: string): string; override;
    procedure SetProperty(const Name: string; const Value: string); override;
  public
    Ambient: boolean;
    RndLocation: boolean;
    PlaybackSpeed: Integer;
    Radius: Integer;
    constructor Create(X, Y, Z: longint);
    destructor Destroy; override;
    property SoundOn: boolean read FSoundOn write SetSoundOn;
    property Volume: Integer read FVolume write SetVolume;
    procedure SaveProperties(List: TStringList); override;
    procedure Init; override;
    procedure Execute;
    property FileName: string read FFileName write SetFilename;
    property Continuous: boolean read FContinuous write SetContinuous;
    property Interval: Integer read FInterval write SetInterval;
  end;

  TPathCorner = class(TAbstractObject)
  protected
    function GetProperty(const Name: string): string; override;
    procedure SetProperty(const Name: string; const Value: string); override;
  public
    Delay: Integer;
    NextDestination: string;
    OnArrival: string;
    procedure SaveProperties(List: TStringList); override;
  end;

  TTrigger = class(TAbstractObject)
  private
    TriggerCount: Integer;
    CountDown: Integer;
    Activator: TCharacter;
    FTriggerSound: string;
    TriggerSounds: TDynamicSmallIntArray;
    procedure SetTriggerSound(const Value: string);
  protected
    function GetProperty(const Name: string): string; override;
    procedure SetProperty(const Name: string; const Value: string); override;
  public
    TriggerEnabled: boolean;
    OnTrigger: string;
    TriggerDelay: Integer;
    destructor Destroy; override;
    procedure SaveProperties(List: TStringList); override;
    procedure Trigger(Character: TCharacter);
    function Execute: boolean;
    // Returns true when trigger can be removed from list
    property TriggerSound: string read FTriggerSound write SetTriggerSound;
  end;

  TEventTimer = class(TAbstractObject)
  protected
    function GetProperty(const Name: string): string; override;
    procedure SetProperty(const Name: string; const Value: string); override;
  public
    AutoReset: boolean;
    Enabled: boolean;
    Interval: Integer;
    CountDown: Integer;
    OnTimer: string;
    TimerCount: Integer;
    procedure Execute;
    procedure SaveProperties(List: TStringList); override;
  end;

  TItem = class(TSpriteObject)
  private
    PickUpCount: Integer;
    DropCount: Integer;
  protected
    procedure SetResource(const Value: TAniResource); override;
    function GetProperty(const Name: string): string; override;
    procedure SetProperty(const Name: string; const Value: string); override;
    function GetName: string; override;
    function GetInfo: string; virtual;
  public
    SlotsAllowed: TSlotAllowed;
    Modifier: TStatModifier;
    Resistance: TDamageResistanceProfile;
    Damage: TDamageProfile;
    // Applies ony to unarmed combat (unless item is TWeapon)
    Value: Integer;
    Weight: Integer;
    Magic: Integer; // Is item magical?  If so, how much?
    Identified: boolean;
    Title: string;
    ItemName: string;
    DisplayName: string;
    ItemInfo: string;
    SecretName: string;
    SecretInfo: string;
    PartName: string;
    OnPickUp: string;
    OnDrop: string;
    InvX: Integer;
    InvY: Integer;
    InvW: Integer;
    InvH: Integer;
    Material: TMaterial;
    LayerIndex: Integer;
    InventoryImage: string;
    LayeredImage: string;
    procedure Clone(var NewObject: TObject; NewGUID: string); virtual;
    procedure Equip(Character: TCharacter); virtual;
    function CanEquip(Slot: TSlot; Character: TCharacter): boolean; virtual;
    function MeetsRequirements(Character: TCharacter): boolean; virtual;
    procedure PickUp; virtual;
    procedure Drop; virtual;
    function GetInventoryImage: IDirectDrawSurface;
    function GetInventoryShadow: IDirectDrawSurface;
    function GetIconicImage: IDirectDrawSurface;
    procedure SaveProperties(List: TStringList); override;
    procedure Init; override;
    function ShouldSave: boolean; override;
    property Info: string read GetInfo;
  end;

  TWeapon = class(TItem)
  private
    FAttackSound: string;
    FStrikeMetalSound: string;
    FStrikeLeatherSound: string;
    FRange: Integer; // Horizontal Pixels
    procedure SetAttackSound(const Value: string);
    procedure SetStrikeLeatherSound(const Value: string);
    procedure SetStrikeMetalSound(const Value: string);
    procedure SetRange(const Value: Integer);
  protected
    function GetProperty(const Name: string): string; override;
    procedure SetProperty(const Name: string; const Value: string); override;
  public
    AttackSounds: TDynamicSmallIntArray;
    StrikeMetalSounds: TDynamicSmallIntArray;
    StrikeLeatherSounds: TDynamicSmallIntArray;
    TwoHanded: boolean;
    MinStrength: Integer;
    MinCoordination: Integer;
    MaxRestriction: Integer;
    destructor Destroy; override;
    procedure Clone(var NewObject: TObject; NewGUID: string); override;
    procedure Equip(Character: TCharacter); override;
    procedure DoDamage(Source, Target: TCharacter); virtual;
    procedure GetDamage(Character: TCharacter); virtual;
    function MeetsRequirements(Character: TCharacter): boolean; override;
    procedure SaveProperties(List: TStringList); override;
    property AttackSound: string read FAttackSound write SetAttackSound;
    property StrikeMetalSound: string read FStrikeMetalSound
      write SetStrikeMetalSound;
    property StrikeLeatherSound: string read FStrikeLeatherSound
      write SetStrikeLeatherSound;
    property Range: Integer read FRange write SetRange;
  end;

  TQuiver = class(TItem)
  private
    FStrikeLeatherSound: string;
    StrikeLeatherSounds: TDynamicSmallIntArray;
    FStrikeMetalSound: string;
    StrikeMetalSounds: TDynamicSmallIntArray;
    FStrikeStoneSound: string;
    StrikeStoneSounds: TDynamicSmallIntArray;
    procedure SetStrikeLeatherSound(const Value: string);
    procedure SetStrikeMetalSound(const Value: string);
    procedure SetStrikeStoneSound(const Value: string);
  protected
    function GetProperty(const Name: string): string; override;
    procedure SetProperty(const Name: string; const Value: string); override;
  public
    FletchingColor: TColor;
    TrackingDegree: Single;
    destructor Destroy; override;
    procedure Clone(var NewObject: TObject; NewGUID: string); override;
    procedure Equip(Character: TCharacter); override;
    procedure SaveProperties(List: TStringList); override;
    property StrikeLeatherSound: string read FStrikeLeatherSound
      write SetStrikeLeatherSound;
    property StrikeMetalSound: string read FStrikeMetalSound
      write SetStrikeMetalSound;
    property StrikeStoneSound: string read FStrikeStoneSound
      write SetStrikeStoneSound;
  end;

  TLauncher = class(TWeapon)
  public
    procedure DoDamage(Source, Target: TCharacter); override;
  end;

  TBow = class(TLauncher)
  public
    constructor Create(X, Y, Z: longint; Frame: Word;
      Enabled: boolean); override;
    procedure DoDamage(Source, Target: TCharacter); override;
    procedure GetDamage(Character: TCharacter); override;
  end;

  TContainer = class(TSpriteObject)
  private
    Opening: boolean;
    FClosed: boolean;
    FPlaying: boolean;
    OpenCount: Integer;
    CloseCount: Integer;
    OpenAttemptCount: Integer;
    FOpenSound: string;
    OpenSounds: TDynamicSmallIntArray;
    FCloseSound: string;
    CloseSounds: TDynamicSmallIntArray;
    ShowObjectInventory: boolean;
    FName: string;
    FInventoryList: string;
    procedure SetCloseSound(const Value: string);
    procedure SetOpenSound(const Value: string);
    function FixMissingItems: boolean;
  protected
    function GetProperty(const Name: string): string; override;
    procedure SetProperty(const Name: string; const Value: string); override;
    function GetName: string; override;
    procedure DoFrame; override;
  public
    Inventory: TList;
    KeyRequired: boolean;
    KeyName: string;
    OnOpen: string;
    OnClose: string;
    OnOpenAttempt: string;
    GridWidth: Integer;
    GridHeight: Integer;
    AlwaysOpen: boolean;
    NoItemPlacement: boolean;
    constructor Create(X, Y, Z: longint; Frame: Word;
      Enabled: boolean); override;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    function DoAction(const Action: string): boolean; override;
    procedure Activate; override;
    procedure ScriptEnd(Sender: TObject); virtual;
    function FindFreeInventoryXY(Item: TItem): boolean;
    procedure SaveProperties(List: TStringList); override;
    procedure Init; override;
    function HasItem(const ItemName: string): boolean;
    procedure RemoveItem(const ItemName: string);
    property Closed: boolean read FClosed;
    property Playing: boolean read FPlaying;
    property OpenSound: string read FOpenSound write SetOpenSound;
    property CloseSound: string read FCloseSound write SetCloseSound;
  end;

  TDoor = class(TSpriteObject)
  private
    Opening: boolean;
    FClosed: boolean;
    FPlaying: boolean;
    OpenCount: Integer;
    CloseCount: Integer;
    OpenAttemptCount: Integer;
    FOpenSound: string;
    OpenSounds: TDynamicSmallIntArray;
    FCloseSound: string;
    CloseSounds: TDynamicSmallIntArray;
    FName: string;
    procedure SetCloseSound(const Value: string);
    procedure SetOpenSound(const Value: string);
  protected
    function GetProperty(const Name: string): string; override;
    procedure SetProperty(const Name: string; const Value: string); override;
    procedure DoFrame; override;
    procedure SetResource(const Value: TAniResource); override;
    function GetName: string; override;
  public
    PrevFrame: Integer;
    Trigger: Integer;
    KeyRequired: boolean;
    Frame1: PItemInstanceInfo;
    KeyName: string;
    OnOpen: string;
    OnClose: string;
    OnOpenAttempt: string;
    GridWidth: Integer;
    GridHeight: Integer;
    constructor Create(X, Y, Z: longint; Frame: Word;
      Enabled: boolean); override;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    function DoAction(const Action: string): boolean; override;
    procedure Activate; override;
    procedure ScriptEnd(Sender: TObject); virtual;
    procedure ChangeFrame;
    procedure SaveProperties(List: TStringList); override;
    procedure Init; override;
    property Closed: boolean read FClosed;
    property Playing: boolean read FPlaying;
    property OpenSound: string read FOpenSound write SetOpenSound;
    property CloseSound: string read FCloseSound write SetCloseSound;
  end;

  TCharacter = class(TSpriteObject)
  private
    FTarget: TSpriteObject;
    FTargetX: longint;
    FTargetY: longint;
    FStrength: Integer;
    FCoordination: Integer;
    FConstitution: Integer;
    FAttacking: boolean;
    FCasting: boolean;
    FReady: boolean;
    FCharm: Integer;
    FHealingRate: Integer;
    FMysticism: Integer;
    FHitPoints: Single;
    FRechargeRate: Integer;
    FStealth: Integer;
    FPerception: Integer;
    PerceptionFactor: Single;
    FAttackRecovery: Integer;
    FHitRecovery: Integer;
    FRecoveryCount: Integer;
    FMana: Single;
    FRestriction: Integer;
    FCombat: Integer;
    FDead: boolean;
    WasPartyMember: boolean;
    Dieing: boolean;
    FDeadCount: LongWord;
    FInventory: TList; // Find correct type
    FRange: Integer;
    Shifted: boolean;
    InitStand: boolean;
    NextTarget: TSpriteObject;
    NextTargetX: longint;
    NextTargetY: longint;
    TargetReached: boolean;
    DieCount: Integer;
    FAttackBonus: Single;
    FBowBonus: Single;
    FDefense: Single;
    FTrainingPoints: Integer;
    Avoid: TList<TAniFigure>;
    FindAgain: boolean;
    Heal: Double;
    Recharge: Double;
    ManaDrain: Single;
    FTrack: TCharacter;
    FFriends: TStringList;
    FEnemies: TStringList;
    FAttackSound: string;
    AttackSounds: TDynamicSmallIntArray;
    FDeathSound: string;
    FPainSound: string;
    FBattleCry: string;
    BattleCries: TDynamicSmallIntArray;
    FOnEquipmentChange: TNotifyEvent;
    FMoney: longint;
    FName: string;
    FFrozen: boolean;
    FCombatMode: boolean;
    FMovement: Single;
    UseAttackRecovery: boolean;
    RunExists: boolean;
    MoveMode: string;
    FWillBeDisabled: boolean;
    FInventoryList: string;
    FEquipmentNames: array [slLeg1 .. slMisc3] of string;
    AntiPathMode: boolean;
    FDeviance: Integer;
    ThresholdOfPain: Integer;
    FVision: Integer; // Sensory values are in horizontal units
    procedure SetAI( const Value : TAI );
    function GetEquipment(Slot: TSlot): TItem;
    procedure SetEquipment(Slot: TSlot; const Value: TItem);
    procedure SetCharm(const Value: Integer);
    procedure SetCombat(const Value: Integer);
    procedure SetConstitution(const Value: Integer);
    procedure SetCoordination(const Value: Integer);
    procedure SetHealingRate(const Value: Integer);
    procedure SetHitPoints(const Value: Single);
    procedure SetMana(const Value: Single);
    procedure SetMysticism(const Value: Integer);
    procedure SetPerception(const Value: Integer);
    procedure SetRechargeRate(const Value: Integer);
    procedure SetStealth(const Value: Integer);
    procedure SetStrength(const Value: Integer);
    procedure SetAttackRecovery(const Value: Integer);
    procedure SetHitRecovery(const Value: Integer);
    procedure SetAttackBonus(const Value: Single);
    procedure SetDefense(const Value: Single);
    procedure SetMovement(const Value: Single);
    procedure SetAIMode(const Value: TAIMode);
    procedure SetTrainingPoints(const Value: Integer);
    procedure SetMoney(const Value: Integer);
    procedure SetTrack(Character: TCharacter);
    function GetFriends: string;
    procedure SetFriends(const Value: string);
    function GetEnemies: string;
    procedure SetEnemies(const Value: string);
    function GetTitleList: string;
    function GetWillBeDisabled: boolean;
    procedure SetTitleList(const Value: string);
    procedure SetAttackSound(const Value: string);
    procedure SetDeathSound(const Value: string);
    procedure SetPainSound(const Value: string);
    procedure SetBattleCry(const Value: string);
    procedure SetFrozen(const Value: boolean);
    procedure SetCombatMode(const Value: boolean);
    procedure SetDead(const Value: boolean);
    procedure AddDamageBonus;
    function getVision: Integer;
    property WillBeDisabled: boolean read GetWillBeDisabled
      write FWillBeDisabled;
  protected
    FWounds: Double;
    FDrain: Double;
    FCurrentSpell: TObject; // TSpell;
    FHotKey: array [1 .. 10] of TObject; // TSpell;
    procedure Render; override;
    function GetProperty(const Name: string): string; override;
    procedure SetProperty(const Name: string; const Value: string); override;
    procedure ScriptEnd(Sender: TObject); virtual;
    procedure PathStep(Sender: TAniFigure; X, Y: longint); virtual;
    procedure ApplyModifier(Modifier: PStatModifier);
    procedure ApplyResistance(Profile: PDamageResistanceProfile);
    procedure CollideFigure(Source, Target: TAniFigure; var Stop: boolean);
    procedure CollideItem(Source: TAniFigure; var Stop: boolean);
    procedure Stop(Sender: TObject);
    procedure NoPath(Sender: TObject);
    procedure Filter(Source: TAniFigure; ID, PrevID: SmallInt);
    procedure Trigger(Source: TAniFigure; ID, PrevID: SmallInt);
    procedure DoFrame; override;
    procedure SetResource(const Value: TAniResource); override;
    function GetName: string; override;
  public
    PainSounds: TDynamicSmallIntArray;
    DeathSounds: TDynamicSmallIntArray;
    InPain: boolean;
    BaseResistance: TDamageResistanceProfile;
    BaseUnArmedDamage: TDamageProfile;
    UnArmedDamage: TDamageProfile;
    FCastRecovery: Integer;
    FAIMode: TAIMode;
    FAI : TAI;
    NewAIMode: TAIMode;
    FEquipment: array [slLeg1 .. slMisc3] of TItem;
    EquipmentLocked: array [slLeg1 .. slMisc3] of boolean;
    BaseStrength: Integer;
    BaseCoordination: Integer;
    BaseConstitution: Integer;
    BaseMysticism: Integer;
    BaseCombat: Integer;
    BaseStealth: Integer;
    BaseMovement: Single;
    BasePerception: Integer;
    BaseCharm: Integer;
    BaseHealingRate: Integer;
    BaseRechargeRate: Integer;
    BaseHitPoints: Single;
    BaseMana: Single;
    BaseAttackRecovery: Integer;
    BaseAttackBonus: Single;
    BaseDefense: Single;
    BaseHitRecovery: Integer;
    Damage: TDamageProfile;
    Resistance: TDamageResistanceProfile;
    FrameCount: LongWord;
    IdleAI: string;
    CombatAI: string;
    PartyAI: string;
    OnDie: string;
    Hearing: Integer;
    Smell: Integer;
    MysticVision: Integer;
    IsMerchant: boolean;
    BuyingDiscount: Single;
    SellingMarkup: Single;
    Alliance: string;
    PrevAlliance: string;
    Effects: TList<TEffect>;
    PrevAIMode: TAIMode;
    UseDefaultEquipment: boolean;
    Titles: TStringList;
    AutoFight: boolean;
    NextAction: TNextAction;
    Companion: array [1 .. MaxCompanions] of TCompanionCharacter;
    NoItemPlacement: boolean;
    IntendToZone: boolean;
    Looted: boolean;
    UseAllegianceOf: TCharacter;
    PartyMember: boolean;
    InterfaceLocked: boolean;
    SpawnCount: Integer;
    Spawned: boolean;
    TransitX, TransitY, TransitZ: longint;
    StandAction: string;
    // this was moved to public to give the AI access to it
    AntiPathEnabled: boolean; // Other wise I cant shut it off in the AI
    // unless the character is reloaded... ie when the map is loaded
    constructor Create(X, Y, Z: longint; Frame: Word;
      Enabled: boolean); override;
    destructor Destroy; override;
    procedure CalcStats; virtual;
    procedure Stand; virtual;
    procedure Face(X, Y: longint); virtual;
    function TakeDamage(Source: TCharacter; Damage, Stun: Single;
      UseStealth: boolean): Integer; virtual;
    procedure WalkTo(X, Y, Deviance: longint); virtual;
    procedure RunTo(X, Y, Deviance: longint); virtual;
    procedure Attack(Target: TCharacter); virtual;
    procedure AttackPoint(X, Y: longint); virtual;
    procedure Cast(Target: TSpriteObject); virtual;
    procedure CastPoint(X, Y: longint); virtual;
    procedure Die; virtual;
    function DoAction(const Action: string): boolean; override;
    procedure LoadEquipment(UseDefaults: boolean);
    procedure Approach(ATarget: TSpriteObject); virtual;
    procedure ApproachRun(ATarget: TSpriteObject); virtual;
    procedure ShiftApproach(ATarget: TSpriteObject); virtual;
    procedure ShiftApproachRun(ATarget: TSpriteObject); virtual;
    function InRange(Target: TAniFigure): boolean;
    function RangeTo(X, Y: longint): Double;
    function IsAlly(Target: TCharacter): boolean;
    function IsNeutral(Target: TCharacter): boolean;
    function IsEnemy(Target: TCharacter): boolean;
    procedure MakeAlly(Alliance: string);
    procedure MakeEnemy(Alliance: string);
    procedure MakeNeutral(Alliance: string);
    procedure AddTitle(const Title: string);
    procedure RemoveTitle(const Title: string);
    procedure AddEffect(Effect: TEffect);
    function SpellList: TStringList;
    function TitleExists(const Title: string): boolean;
    function InInventory(const ItemName: string): boolean;
    function IsWorn(const ItemName: string): boolean;
    function FindFreeInventoryXY(Item: TItem): boolean;
    procedure SaveProperties(List: TStringList); override;
    procedure Init; override;
    procedure InitAI;
    procedure DoBattleCry;
    function ShouldSave: boolean; override;
    procedure Clone(var NewObject: TObject; NewGUID: string); virtual;
    function HasItem(const ItemName: string): boolean;
    procedure RemoveItem(const ItemName: string);
    function AffectDamage(Source: TAniFigure; Damage: PDamageProfile): boolean;
    procedure ClearEquipment;
    function ValidateSpells: boolean;
    procedure SetVision(v: Integer);
    // Primary Stats
    property Strength: Integer read FStrength write SetStrength;
    property Coordination: Integer read FCoordination write SetCoordination;
    property Constitution: Integer read FConstitution write SetConstitution;
    property Perception: Integer read FPerception write SetPerception;
    property Charm: Integer read FCharm write SetCharm;
    // Skill stats
    property Mysticism: Integer read FMysticism write SetMysticism;
    property Combat: Integer read FCombat write SetCombat;
    property Stealth: Integer read FStealth write SetStealth;
    // Secondary Stats
    property Restriction: Integer read FRestriction;
    property Movement: Single read FMovement write SetMovement;
    property HealingRate: Integer read FHealingRate write SetHealingRate;
    property RechargeRate: Integer read FRechargeRate write SetRechargeRate;

    property HitPoints: Single read FHitPoints write SetHitPoints;
    property Mana: Single read FMana write SetMana;
    property AttackRecovery: Integer read FAttackRecovery
      write SetAttackRecovery;
    property HitRecovery: Integer read FHitRecovery write SetHitRecovery;

    property Wounds: Double read FWounds write FWounds;
    property Drain: Double read FDrain write FDrain;
    property Ready: boolean read FReady;
    property Equipment[Slot: TSlot]: TItem read GetEquipment write SetEquipment;
    property Inventory: TList read FInventory;
    property AI : TAI read FAI write SetAI;
    property Dead: boolean read FDead write SetDead;
    property Range: Integer read FRange;
    property AIMode: TAIMode read FAIMode write SetAIMode;
    property AttackBonus: Single read FAttackBonus write SetAttackBonus;
    property BowBonus: Single read FBowBonus;
    property Defense: Single read FDefense write SetDefense;
    property TrainingPoints: Integer read FTrainingPoints
      write SetTrainingPoints;
    property Money: Integer read FMoney write SetMoney;
    property Attacking: boolean read FAttacking;
    property Casting: boolean read FCasting;
    property RecoveryCount: Integer read FRecoveryCount;
    property Track: TCharacter read FTrack write SetTrack;
    property Friends: string read GetFriends write SetFriends;
    property Enemies: string read GetEnemies write SetEnemies;
    property TitleList: string read GetTitleList write SetTitleList;
    property OnEquipmentChange: TNotifyEvent read FOnEquipmentChange
      write FOnEquipmentChange;
    property AttackSound: string read FAttackSound write SetAttackSound;
    property DeathSound: string read FDeathSound write SetDeathSound;
    property PainSound: string read FPainSound write SetPainSound;
    property BattleCry: string read FBattleCry write SetBattleCry;
    property Name: string read GetName write FName;
    property Frozen: boolean read FFrozen write SetFrozen;
    property CombatMode: boolean read FCombatMode write SetCombatMode;
    property Target: TSpriteObject read FTarget;
    property DeadCount: LongWord read FDeadCount;
    property TargetX: longint read FTargetX;
    property TargetY: longint read FTargetY;
    property Vision: Integer read getVision;
    // Sensory - values are in horizontal pixels
  end;

  TCompanionCharacter = class(TCharacter)
  private
    Fade: Integer;
  protected
    procedure DoFrame; override;
    procedure SetResource(const Value: TAniResource); override;
  public
    Duration: Integer;
    Master: TCharacter;
    constructor Create(X, Y, Z: longint; Frame: Word;
      Enabled: boolean); override;
    destructor Destroy; override;
  end;

  TSpriteClass = class of TSpriteObject;

  TSpriteManager = class(TObject)
  private
    FCurrentIndex: Word;
    List: TList;
    SpriteCount: Integer;
  public
    constructor Create(Count: Integer);
    destructor Destroy; override;
    procedure ReAlloc;
    function NewSprite(ClassType: TSpriteClass; Resource: TAniResource;
      X, Y, Z, Frame: Integer): TSpriteObject;
    property CurentIndex: Word read FCurrentIndex;
  end;

function Perceptible(const Source: TCharacter; const Target: TSpriteObject;
  const Vision, Hearing, Smell: Double; MysticVision: Integer): boolean;
function GetPerceptibleEnemies(Source: TCharacter; Factor: Single): TStringList;
function GetNearbyEnemies(Source: TCharacter; Limit: Double): TStringList;
function GetPerceptibleAllies(Source: TCharacter; Factor: Single): TStringList;
function GetNearbyAllies(Source: TCharacter; Limit: Double): TStringList;
function GetPerceptibleDead(Source: TCharacter; Factor: Single): TStringList;
function GetPerceptibleContainers(Source: TCharacter; Factor: Single)
  : TStringList;
function GetGroup(Source: TGameObject; const GroupName: string): TStringList;
function AllDead(const GroupName: string): boolean;
function GetGUID(GUID: string): TGameObject;
function GetNearbyCharacter(Source: TCharacter; Limit: Double): TStringList;
function TransferItem(Source, Dest: TGameObject; ItemName: string;
  DropIfNoRoom: boolean): boolean;
function IsAnybodyInTheWay(Source, Dest: TGameObject; Radius: Integer): boolean;
function CalcTotalDamage(Damage: TDamageProfile;
  Resistance: TDamageResistanceProfile; F: Single; Critical: boolean): Single;
function CalcDamage(Range: TDamageRange): Single;
procedure ComputeTrajectory(Source: TAniFigure; var TargetX, TargetY: Integer;
  ErrDegree: Single);
function GetFacing(SrcX, SrcY, GridX, GridY: longint): TFacing;

const
  SoundPreloadCount = 5;

var
  Sprites: TSpriteManager;
  Player, Current: TCharacter;
  NPCList: TList;

implementation

uses
  SoAOS.Projectile,
  SoAOS.AI.Helper,
  SoAOS.Spells,
  SoAOS.Effects,
  System.SysUtils,
  DXUtil,
  SoAOS.Graphics.Draw,
  LogFile,
  Engine,
  Titles,
  Display,
  Parts,
  Sound,
  System.IniFiles,
  AI1,
  Spells,
  BasicHumanoidAI,
  UndeadAI,
  WolfAI,
  MiscAI,
  AniDemo;

// fix for bad character names

function FixCharacterName(const Value: string): string;
var
  INI: TMemINIFile;
  StrTmp: string;
begin
  INI := TMemINIFile.Create( MapPath + 'symbols.'+ Language +'.ini', TEncoding.GetEncoding(INICodepage) );

  try
    StrTmp := '#' + Value;
    result := INI.ReadString(Parse(Value, 0, '.'),
      Parse(Value, 1, '.'), StrTmp);
    if StrTmp = result then
      log.log('*** StillBadName: ' + Value);
  finally
    INI.free;
  end;
end;

function IsAnybodyInTheWay(Source, Dest: TGameObject; Radius: Integer): boolean;
var
  i: Integer;
  A, B, C, Q, D, T: Double;
  dx, dy: Double;
  List: TList;
  Test: TAniFigure;
begin
  result := false;

  dx := Dest.X - Source.X;
  dy := 2 * (Dest.Y - Source.Y);
  A := sqr(dx) + sqr(dy);
  D := sqrt(A);

  List := Game.FindInRadius(Source.X, Source.Y, D);
  if assigned(List) then
    try
      for i := 0 to List.Count - 1 do
      begin
        Test := List.items[i];
        if (Test <> Source) and (Test <> Dest) and not(Test is TProjectile) then
        begin
          if Test.Radius > 0 then
          begin
            B := 2 * (dx * (Source.X - Test.X) + 2 * dy * (Source.Y - Test.Y));
            C := sqr(Source.X - Test.X) + 4 * sqr(Source.Y - Test.Y) -
              sqr(Radius + Test.Radius);
            Q := sqr(B) - 4 * A * C;
            if (Q >= 0) then
            begin
              T := (-B - sqrt(Q)) / (2 * A);
              if (T >= 0) then
              begin
                result := true;
                break;
              end;
            end;
          end;
        end;
      end;
    finally
      List.free;
    end;
end;

procedure PlaySound(const Sounds: TDynamicSmallIntArray; X, Y: longint);
var
  D: Double;
  PanD: longint;
  Volume, Pan: Integer;
  HearingRange: longint;
const
  FailName: string = 'Character.PlaySound';
begin
  log.DebugLog(FailName);
  try

    if not assigned(Sounds) then
      exit;
    HearingRange := ScreenMetrics.HearingRange;
    if HearingRange <= 0 then
      exit;
    PanD := X - Current.X;
    D := sqrt(sqr(PanD) + 2 * sqr(Y - Current.Y));
    if D > HearingRange then
      exit;
    if D <= 1 then
    begin
      Volume := MasterSoundVolume;
      Pan := 0;
    end
    else
    begin
      Volume := round(MasterSoundVolume * (HearingRange - D) / HearingRange);
      Pan := (PanD * 10000) div HearingRange;
    end;
    if assigned(SoundLib) then
      SoundLib.PlaySound(Sounds, 0, Volume, Pan, 100);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure PlaySingleSound(Sound: SmallInt; X, Y: longint);
var
  D: Double;
  PanD: longint;
  Volume, Pan: Integer;
  HearingRange: longint;
const
  FailName: string = 'Character.PlaySingleSound';
begin
  log.DebugLog(FailName);
  try
    HearingRange := ScreenMetrics.HearingRange;
    if HearingRange <= 0 then
      exit;
    PanD := X - Current.X;
    D := sqrt(sqr(PanD) + 2 * sqr(Y - Current.Y));
    if D > HearingRange then
      exit;
    if D <= 1 then
    begin
      Volume := MasterSoundVolume;
      Pan := 0;
    end
    else
    begin
      Volume := round(MasterSoundVolume * (HearingRange - D) / HearingRange);
      Pan := (PanD * 10000) div HearingRange;
    end;
    if assigned(SoundLib) then
      SoundLib.PlaySoundByIndex(Sound, 0, Volume, Pan, 100);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function CalcDamage(Range: TDamageRange): Single;
const
  FailName: string = 'Character.CalcDamage';
begin
  result := 0;

  log.DebugLog(FailName);
  try

    result := random * (Range.Max - Range.Min) + Range.Min;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function CalcTotalDamage(Damage: TDamageProfile;
  Resistance: TDamageResistanceProfile; F: Single; Critical: boolean): Single;
var
  Points: Single;
  D: Single;
const
  FailName: string = 'Character.CalcTotalDamage';
begin
  result := 0;

  log.DebugLog(FailName);
  try

    if Critical then
    begin
      result := 1;

      with Damage do
      begin
        result := result + CalcDamage(Special);

        Points := CalcDamage(Piercing) * F;
        if Points > 0 then
        begin
          D := Points * (1 - Resistance.Piercing.Resistance);
          if D > 0 then
            result := result + D;
        end;

        Points := CalcDamage(Crushing) * F;
        if Points > 0 then
        begin
          D := Points * (1 - Resistance.Crushing.Resistance);
          if D > 0 then
            result := result + D;
        end;

        Points := CalcDamage(Cutting) * F;
        if Points > 0 then
        begin
          D := Points * (1 - Resistance.Cutting.Resistance);
          if D > 0 then
            result := result + D;
        end;

        Points := CalcDamage(Heat) * F;
        if Points > 0 then
        begin
          D := Points * (1 - Resistance.Heat.Resistance);
          if D > 0 then
            result := result + D;
        end;

        Points := CalcDamage(Cold) * F;
        if Points > 0 then
        begin
          D := Points * (1 - Resistance.Cold.Resistance);
          if D > 0 then
            result := result + D;
        end;

        Points := CalcDamage(Electric) * F;
        if Points > 0 then
        begin
          D := Points * (1 - Resistance.Electric.Resistance);
          if D > 0 then
            result := result + D;
        end;

        Points := CalcDamage(Poison) * F;
        if Points > 0 then
        begin
          D := Points * (1 - Resistance.Poison.Resistance);
          if D > 0 then
            result := result + D;
        end;

        Points := CalcDamage(Magic) * F;
        if Points > 0 then
        begin
          D := Points * (1 - Resistance.Magic.Resistance);
          if D > 0 then
            result := result + D;
        end;

        Points := CalcDamage(Mental) * F;
        if Points > 0 then
        begin
          D := Points * (1 - Resistance.Mental.Resistance);
          if D > 0 then
            result := result + D;
        end;
      end;
    end
    else
    begin
      result := 0;

      with Damage do
      begin
        result := result + CalcDamage(Special);

        Points := CalcDamage(Piercing) * F -
          Resistance.Piercing.Invulnerability;
        if Points > 0 then
        begin
          D := Points * (1 - Resistance.Piercing.Resistance);
          if D > 0 then
            result := result + D;
        end;

        Points := CalcDamage(Crushing) * F -
          Resistance.Crushing.Invulnerability;
        if Points > 0 then
        begin
          D := Points * (1 - Resistance.Crushing.Resistance);
          if D > 0 then
            result := result + D;
        end;

        Points := CalcDamage(Cutting) * F - Resistance.Cutting.Invulnerability;
        if Points > 0 then
        begin
          D := Points * (1 - Resistance.Cutting.Resistance);
          if D > 0 then
            result := result + D;
        end;

        Points := CalcDamage(Heat) * F - Resistance.Heat.Invulnerability;
        if Points > 0 then
        begin
          D := Points * (1 - Resistance.Heat.Resistance);
          if D > 0 then
            result := result + D;
        end;

        Points := CalcDamage(Cold) * F - Resistance.Cold.Invulnerability;
        if Points > 0 then
        begin
          D := Points * (1 - Resistance.Cold.Resistance);
          if D > 0 then
            result := result + D;
        end;

        Points := CalcDamage(Electric) * F -
          Resistance.Electric.Invulnerability;
        if Points > 0 then
        begin
          D := Points * (1 - Resistance.Electric.Resistance);
          if D > 0 then
            result := result + D;
        end;

        Points := CalcDamage(Poison) * F - Resistance.Poison.Invulnerability;
        if Points > 0 then
        begin
          D := Points * (1 - Resistance.Poison.Resistance);
          if D > 0 then
            result := result + D;
        end;

        Points := CalcDamage(Magic) * F - Resistance.Magic.Invulnerability;
        if Points > 0 then
        begin
          D := Points * (1 - Resistance.Magic.Resistance);
          if D > 0 then
            result := result + D;
        end;

        Points := CalcDamage(Mental) * F - Resistance.Mental.Invulnerability;
        if Points > 0 then
        begin
          D := Points * (1 - Resistance.Mental.Resistance);
          if D > 0 then
            result := result + D;
        end;
      end;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure ComputeTrajectory(Source: TAniFigure; var TargetX, TargetY: Integer;
  ErrDegree: Single);
var
  Angle: Single;
  D, dx, dy: Double;
begin
  dx := TargetX - Source.X;
  dy := 2 * (TargetY - Source.Y);
  D := sqrt(dx * dx + dy * dy);
  Angle := ATan(dx, dy) + (random - 0.5) * ErrDegree * PI / 90;
  TargetX := Source.X + round(cos(Angle) * D);
  TargetY := Source.Y + round(sin(Angle) * D / 2);
end;

function GetFacing(SrcX, SrcY, GridX, GridY: longint): TFacing;
var
  Slope: Single;
const
  FailName: string = 'Character.GetFacing';
begin
  result := fSS;

  log.DebugLog(FailName);
  try

    if (GridX = SrcX) then
    begin
      if (GridY < SrcY) then
        result := fNN
    end
    else
    begin
      Slope := (GridY - SrcY) / (GridX - SrcX);
      if (GridX < SrcX) then
      begin
        if (Slope >= -0.25) and (Slope <= 0.25) then
          result := fWW
        else if (Slope > 2) then
          result := fNN
        else if (Slope < -2) then
          result := fSS
        else if (Slope > 0) then
          result := fNW
        else
          result := fSW;
      end
      else
      begin
        if (Slope >= -0.25) and (Slope <= 0.25) then
          result := fEE
        else if (Slope > 2) then
          result := fSS
        else if (Slope < -2) then
          result := fNN
        else if (Slope > 0) then
          result := fSE
        else
          result := fNE;
      end;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function SetDamageProps(const S, Value: string;
  var Damage: TDamageProfile): boolean;
var
  C1, C2: string;
const
  FailName: string = 'Character.SetDamageProps';
begin
  result := false;

  log.DebugLog(FailName);
  try

    C1 := Parse(S, 0, '.');
    if C1 <> 'damage' then
      exit;
    C2 := Parse(S, 1, '.');

    if C2 = 'piercingmin' then
    begin
      Damage.Piercing.Min := UnFormatFP(Value);
      result := true;
    end
    else if C2 = 'piercingmax' then
    begin
      Damage.Piercing.Max := UnFormatFP(Value);
      result := true;
    end
    else if C2 = 'crushingmin' then
    begin
      Damage.Crushing.Min := UnFormatFP(Value);
      result := true;
    end
    else if C2 = 'crushingmax' then
    begin
      Damage.Crushing.Max := UnFormatFP(Value);
      result := true;
    end
    else if C2 = 'cuttingmin' then
    begin
      Damage.Cutting.Min := UnFormatFP(Value);
      result := true;
    end
    else if C2 = 'cuttingmax' then
    begin
      Damage.Cutting.Max := UnFormatFP(Value);
      result := true;
    end
    else if C2 = 'heatmin' then
    begin
      Damage.Heat.Min := UnFormatFP(Value);
      result := true;
    end
    else if C2 = 'heatmax' then
    begin
      Damage.Heat.Max := UnFormatFP(Value);
      result := true;
    end
    else if C2 = 'coldmin' then
    begin
      Damage.Cold.Min := UnFormatFP(Value);
      result := true;
    end
    else if C2 = 'coldmax' then
    begin
      Damage.Cold.Max := UnFormatFP(Value);
      result := true;
    end
    else if C2 = 'electricmin' then
    begin
      Damage.Electric.Min := UnFormatFP(Value);
      result := true;
    end
    else if C2 = 'electricmax' then
    begin
      Damage.Electric.Max := UnFormatFP(Value);
      result := true;
    end
    else if C2 = 'poisonmin' then
    begin
      Damage.Poison.Min := UnFormatFP(Value);
      result := true;
    end
    else if C2 = 'poisonmax' then
    begin
      Damage.Poison.Max := UnFormatFP(Value);
      result := true;
    end
    else if C2 = 'magicmin' then
    begin
      Damage.Magic.Min := UnFormatFP(Value);
      result := true;
    end
    else if C2 = 'magicmax' then
    begin
      Damage.Magic.Max := UnFormatFP(Value);
      result := true;
    end
    else if C2 = 'mentalmin' then
    begin
      Damage.Mental.Min := UnFormatFP(Value);
      result := true;
    end
    else if C2 = 'mentalmax' then
    begin
      Damage.Mental.Max := UnFormatFP(Value);
      result := true;
    end
    else if C2 = 'stunmin' then
    begin
      Damage.Stun.Min := UnFormatFP(Value);
      result := true;
    end
    else if C2 = 'stunmax' then
    begin
      Damage.Stun.Max := UnFormatFP(Value);
      result := true;
    end
    else if C2 = 'specialmin' then
    begin
      Damage.Special.Min := UnFormatFP(Value);
      result := true;
    end
    else if C2 = 'specialmax' then
    begin
      Damage.Special.Max := UnFormatFP(Value);
      result := true;
    end

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function GetDamageProps(const S: string; var Value: string;
  const Damage: TDamageProfile): boolean;
var
  C1, C2: string;
const
  FailName: string = 'Character.GetDamageProps';
begin
  result := false;
  Value := '';

  log.DebugLog(FailName);
  try
    C1 := Parse(S, 0, '.');
    if C1 <> 'damage' then
      exit;
    C2 := Parse(S, 1, '.');

    if C2 = 'piercingmin' then
    begin
      Value := FormatFP(Damage.Piercing.Min);
      result := true;
    end
    else if C2 = 'piercingmax' then
    begin
      Value := FormatFP(Damage.Piercing.Max);
      result := true;
    end
    else if C2 = 'crushingmin' then
    begin
      Value := FormatFP(Damage.Crushing.Min);
      result := true;
    end
    else if C2 = 'crushingmax' then
    begin
      Value := FormatFP(Damage.Crushing.Max);
      result := true;
    end
    else if C2 = 'cuttingmin' then
    begin
      Value := FormatFP(Damage.Cutting.Min);
      result := true;
    end
    else if C2 = 'cuttingmax' then
    begin
      Value := FormatFP(Damage.Cutting.Max);
      result := true;
    end
    else if C2 = 'heatmin' then
    begin
      Value := FormatFP(Damage.Heat.Min);
      result := true;
    end
    else if C2 = 'heatmax' then
    begin
      Value := FormatFP(Damage.Heat.Max);
      result := true;
    end
    else if C2 = 'coldmin' then
    begin
      Value := FormatFP(Damage.Cold.Min);
      result := true;
    end
    else if C2 = 'coldmax' then
    begin
      Value := FormatFP(Damage.Cold.Max);
      result := true;
    end
    else if C2 = 'electricmin' then
    begin
      Value := FormatFP(Damage.Electric.Min);
      result := true;
    end
    else if C2 = 'electricmax' then
    begin
      Value := FormatFP(Damage.Electric.Max);
      result := true;
    end
    else if C2 = 'poisonmin' then
    begin
      Value := FormatFP(Damage.Poison.Min);
      result := true;
    end
    else if C2 = 'poisonmax' then
    begin
      Value := FormatFP(Damage.Poison.Max);
      result := true;
    end
    else if C2 = 'magicmin' then
    begin
      Value := FormatFP(Damage.Magic.Min);
      result := true;
    end
    else if C2 = 'magicmax' then
    begin
      Value := FormatFP(Damage.Magic.Max);
      result := true;
    end
    else if C2 = 'mentalmin' then
    begin
      Value := FormatFP(Damage.Mental.Min);
      result := true;
    end
    else if C2 = 'mentalmax' then
    begin
      Value := FormatFP(Damage.Mental.Max);
      result := true;
    end
    else if C2 = 'stunmin' then
    begin
      Value := FormatFP(Damage.Stun.Min);
      result := true;
    end
    else if C2 = 'stunmax' then
    begin
      Value := FormatFP(Damage.Stun.Max);
      result := true;
    end
    else if C2 = 'specialmin' then
    begin
      Value := FormatFP(Damage.Special.Min);
      result := true;
    end
    else if C2 = 'specialmax' then
    begin
      Value := FormatFP(Damage.Special.Max);
      result := true;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure SaveDamageProps(List: TStringList; const Damage: TDamageProfile);
var
  S: string;
const
  FailName: string = 'Character.SaveDamageProps';
begin
  log.DebugLog(FailName);
  try

    S := 'damage.piercingmin=' + FormatFP(Damage.Piercing.Min);
    List.add(S);
    S := 'damage.piercingmax=' + FormatFP(Damage.Piercing.Max);
    List.add(S);
    S := 'damage.crushingmin=' + FormatFP(Damage.Crushing.Min);
    List.add(S);
    S := 'damage.crushingmax=' + FormatFP(Damage.Crushing.Max);
    List.add(S);
    S := 'damage.cuttingmin=' + FormatFP(Damage.Cutting.Min);
    List.add(S);
    S := 'damage.cuttingmax=' + FormatFP(Damage.Cutting.Max);
    List.add(S);
    S := 'damage.heatmin=' + FormatFP(Damage.Heat.Min);
    List.add(S);
    S := 'damage.heatmax=' + FormatFP(Damage.Heat.Max);
    List.add(S);
    S := 'damage.coldmin=' + FormatFP(Damage.Cold.Min);
    List.add(S);
    S := 'damage.coldmax=' + FormatFP(Damage.Cold.Max);
    List.add(S);
    S := 'damage.electricmin=' + FormatFP(Damage.Electric.Min);
    List.add(S);
    S := 'damage.electricmax=' + FormatFP(Damage.Electric.Max);
    List.add(S);
    S := 'damage.poisonmin=' + FormatFP(Damage.Poison.Min);
    List.add(S);
    S := 'damage.poisonmax=' + FormatFP(Damage.Poison.Max);
    List.add(S);
    S := 'damage.magicmin=' + FormatFP(Damage.Magic.Min);
    List.add(S);
    S := 'damage.magicmax=' + FormatFP(Damage.Magic.Max);
    List.add(S);
    S := 'damage.mentalmin=' + FormatFP(Damage.Mental.Min);
    List.add(S);
    S := 'damage.mentalmax=' + FormatFP(Damage.Mental.Max);
    List.add(S);
    S := 'damage.stunmin=' + FormatFP(Damage.Stun.Min);
    List.add(S);
    S := 'damage.stunmax=' + FormatFP(Damage.Stun.Max);
    List.add(S);
    S := 'damage.specialmin=' + FormatFP(Damage.Special.Min);
    List.add(S);
    S := 'damage.specialmax=' + FormatFP(Damage.Special.Max);
    List.add(S);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function SetResistanceProps(const S, Value: string;
  var Resistance: TDamageResistanceProfile): boolean;
var
  C1, C2: string;
const
  FailName: string = 'Character.SetResistanceProps';
begin
  result := false;

  log.DebugLog(FailName);
  try

    C1 := Parse(S, 0, '.');
    if C1 <> 'resistance' then
      exit;
    C2 := Parse(S, 1, '.');

    if C2 = 'piercinginv' then
    begin
      Resistance.Piercing.Invulnerability := UnFormatFP(Value);
      result := true;
    end
    else if C2 = 'crushinginv' then
    begin
      Resistance.Crushing.Invulnerability := UnFormatFP(Value);
      result := true;
    end
    else if C2 = 'cuttinginv' then
    begin
      Resistance.Cutting.Invulnerability := UnFormatFP(Value);
      result := true;
    end
    else if C2 = 'heatinv' then
    begin
      Resistance.Heat.Invulnerability := UnFormatFP(Value);
      result := true;
    end
    else if C2 = 'coldinv' then
    begin
      Resistance.Cold.Invulnerability := UnFormatFP(Value);
      result := true;
    end
    else if C2 = 'electricinv' then
    begin
      Resistance.Electric.Invulnerability := UnFormatFP(Value);
      result := true;
    end
    else if C2 = 'poisoninv' then
    begin
      Resistance.Poison.Invulnerability := UnFormatFP(Value);
      result := true;
    end
    else if C2 = 'magicinv' then
    begin
      Resistance.Magic.Invulnerability := UnFormatFP(Value);
      result := true;
    end
    else if C2 = 'mentalinv' then
    begin
      Resistance.Mental.Invulnerability := UnFormatFP(Value);
      result := true;
    end
    else if C2 = 'stuninv' then
    begin
      Resistance.Stun.Invulnerability := UnFormatFP(Value);
      result := true;
    end
    else if C2 = 'piercingres' then
    begin
      Resistance.Piercing.Resistance := UnFormatFP(Value) / 100;
      result := true;
    end
    else if C2 = 'crushingres' then
    begin
      Resistance.Crushing.Resistance := UnFormatFP(Value) / 100;
      result := true;
    end
    else if C2 = 'cuttingres' then
    begin
      Resistance.Cutting.Resistance := UnFormatFP(Value) / 100;
      result := true;
    end
    else if C2 = 'heatres' then
    begin
      Resistance.Heat.Resistance := UnFormatFP(Value) / 100;
      result := true;
    end
    else if C2 = 'coldres' then
    begin
      Resistance.Cold.Resistance := UnFormatFP(Value) / 100;
      result := true;
    end
    else if C2 = 'electricres' then
    begin
      Resistance.Electric.Resistance := UnFormatFP(Value) / 100;
      result := true;
    end
    else if C2 = 'poisonres' then
    begin
      Resistance.Poison.Resistance := UnFormatFP(Value) / 100;
      result := true;
    end
    else if C2 = 'magicres' then
    begin
      Resistance.Magic.Resistance := UnFormatFP(Value) / 100;
      result := true;
    end
    else if C2 = 'mentalres' then
    begin
      Resistance.Mental.Resistance := UnFormatFP(Value) / 100;
      result := true;
    end
    else if C2 = 'stunres' then
    begin
      Resistance.Stun.Resistance := UnFormatFP(Value) / 100;
      result := true;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function GetResistanceProps(const S: string; var Value: string;
  const Resistance: TDamageResistanceProfile): boolean;
var
  C1, C2: string;
const
  FailName: string = 'Character.GetResistanceProps';
begin
  Value := '';
  result := false;

  log.DebugLog(FailName);
  try

    C1 := Parse(S, 0, '.');
    if C1 <> 'resistance' then
      exit;
    C2 := Parse(S, 1, '.');

    if C2 = 'piercinginv' then
    begin
      Value := FormatFP(Resistance.Piercing.Invulnerability);
      result := true;
    end
    else if C2 = 'crushinginv' then
    begin
      Value := FormatFP(Resistance.Crushing.Invulnerability);
      result := true;
    end
    else if C2 = 'cuttinginv' then
    begin
      Value := FormatFP(Resistance.Cutting.Invulnerability);
      result := true;
    end
    else if C2 = 'heatinv' then
    begin
      Value := FormatFP(Resistance.Heat.Invulnerability);
      result := true;
    end
    else if C2 = 'coldinv' then
    begin
      Value := FormatFP(Resistance.Cold.Invulnerability);
      result := true;
    end
    else if C2 = 'electricinv' then
    begin
      Value := FormatFP(Resistance.Electric.Invulnerability);
      result := true;
    end
    else if C2 = 'poisoninv' then
    begin
      Value := FormatFP(Resistance.Poison.Invulnerability);
      result := true;
    end
    else if C2 = 'magicinv' then
    begin
      Value := FormatFP(Resistance.Magic.Invulnerability);
      result := true;
    end
    else if C2 = 'mentalinv' then
    begin
      Value := FormatFP(Resistance.Mental.Invulnerability);
      result := true;
    end
    else if C2 = 'stuninv' then
    begin
      Value := FormatFP(Resistance.Stun.Invulnerability);
      result := true;
    end
    else if C2 = 'piercingres' then
    begin
      Value := FormatFP(100 * Resistance.Piercing.Resistance);
      result := true;
    end
    else if C2 = 'crushingres' then
    begin
      Value := FormatFP(100 * Resistance.Crushing.Resistance);
      result := true;
    end
    else if C2 = 'cuttingres' then
    begin
      Value := FormatFP(100 * Resistance.Cutting.Resistance);
      result := true;
    end
    else if C2 = 'heatres' then
    begin
      Value := FormatFP(100 * Resistance.Heat.Resistance);
      result := true;
    end
    else if C2 = 'coldres' then
    begin
      Value := FormatFP(100 * Resistance.Cold.Resistance);
      result := true;
    end
    else if C2 = 'electricres' then
    begin
      Value := FormatFP(100 * Resistance.Electric.Resistance);
      result := true;
    end
    else if C2 = 'poisonres' then
    begin
      Value := FormatFP(100 * Resistance.Poison.Resistance);
      result := true;
    end
    else if C2 = 'magicres' then
    begin
      Value := FormatFP(100 * Resistance.Magic.Resistance);
      result := true;
    end
    else if C2 = 'mentalres' then
    begin
      Value := FormatFP(100 * Resistance.Mental.Resistance);
      result := true;
    end
    else if C2 = 'stunres' then
    begin
      Value := FormatFP(100 * Resistance.Stun.Resistance);
      result := true;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure SaveResistanceProps(List: TStringList;
  const Resistance: TDamageResistanceProfile);
var
  S: string;
const
  FailName: string = 'Character.SaveResistanceProps';
begin
  log.DebugLog(FailName);
  try

    S := 'resistance.piercinginv=' +
      FormatFP(Resistance.Piercing.Invulnerability);
    List.add(S);
    S := 'resistance.crushinginv=' +
      FormatFP(Resistance.Crushing.Invulnerability);
    List.add(S);
    S := 'resistance.cuttinginv=' +
      FormatFP(Resistance.Cutting.Invulnerability);
    List.add(S);
    S := 'resistance.heatinv=' + FormatFP(Resistance.Heat.Invulnerability);
    List.add(S);
    S := 'resistance.coldinv=' + FormatFP(Resistance.Cold.Invulnerability);
    List.add(S);
    S := 'resistance.electricinv=' +
      FormatFP(Resistance.Electric.Invulnerability);
    List.add(S);
    S := 'resistance.poisoninv=' + FormatFP(Resistance.Poison.Invulnerability);
    List.add(S);
    S := 'resistance.magicinv=' + FormatFP(Resistance.Magic.Invulnerability);
    List.add(S);
    S := 'resistance.mentalinv=' + FormatFP(Resistance.Mental.Invulnerability);
    List.add(S);
    S := 'resistance.stuninv=' + FormatFP(Resistance.Stun.Invulnerability);
    List.add(S);
    S := 'resistance.piercingres=' +
      FormatFP(100 * Resistance.Piercing.Resistance);
    List.add(S);
    S := 'resistance.crushingres=' +
      FormatFP(100 * Resistance.Crushing.Resistance);
    List.add(S);
    S := 'resistance.cuttingres=' +
      FormatFP(100 * Resistance.Cutting.Resistance);
    List.add(S);
    S := 'resistance.heatres=' + FormatFP(100 * Resistance.Heat.Resistance);
    List.add(S);
    S := 'resistance.coldres=' + FormatFP(100 * Resistance.Cold.Resistance);
    List.add(S);
    S := 'resistance.electricres=' +
      FormatFP(100 * Resistance.Electric.Resistance);
    List.add(S);
    S := 'resistance.poisonres=' + FormatFP(100 * Resistance.Poison.Resistance);
    List.add(S);
    S := 'resistance.magicres=' + FormatFP(100 * Resistance.Magic.Resistance);
    List.add(S);
    S := 'resistance.mentalres=' + FormatFP(100 * Resistance.Mental.Resistance);
    List.add(S);
    S := 'resistance.stunres=' + FormatFP(100 * Resistance.Stun.Resistance);
    List.add(S);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

{ TCharacter }

function TCharacter.GetName: string;
const
  FailName: string = 'TCharacter.GetName';
begin
  log.DebugLog(FailName);
  try

    result := FName;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.SetFrozen(const Value: boolean);
const
  FailName: string = 'TCharacter.SetFrozen';
begin
  log.DebugLog(FailName);
  try

    if Value <> FFrozen then
    begin
      FFrozen := Value;
      inherited Stop;
      Stand;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.SetCombatMode(const Value: boolean);
const
  FailName: string = 'TCharacter.SetCombatMode';
begin
  log.DebugLog(FailName);
  try

    if Value <> FCombatMode then
    begin
      FCombatMode := Value;
      CalcStats;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.SetDead(const Value: boolean);
const
  FailName: string = 'TCharacter.SetDead';
begin
  log.DebugLog(FailName);
  try

    FDead := Value;
    if FDead then
      MouseRect := Rect(24, CenterY - Radius div 2, Width - 24,
        CenterY + Radius div 4);

    if Spawned then
      AddEffect(TBodyRotEffect.Create);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.SetResource(const Value: TAniResource);
var
  Script: TScript;
const
  FailName: string = 'TCharacter.SetResource';
  Action = 'Run';
begin
  log.DebugLog(FailName);
  try

    inherited;

    RunExists := ActionExists(Action);

    if RunExists and (Resource is TCharacterResource) and
      assigned(TCharacterResource(Resource).NakedResource) then
    begin
      Script := TCharacterResource(Resource).NakedResource.Script
        [Action + Facing.ToString];
      if not assigned(Script) then
        Script := TCharacterResource(Resource).NakedResource.Script[Action];
      RunExists := assigned(Script);
    end;

    if Resource is TCharacterResource then
      LoadProperties(TCharacterResource(Resource).Defaults);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.AddDamageBonus;
var
  i: TSlot;
begin
  for i := slLeg1 to slMisc3 do
  begin
    if (i <> slWeapon) and assigned(Equipment[i]) and
      not(Equipment[i] is TQuiver) then
    begin
      Damage.Piercing.Min := Damage.Piercing.Min + Equipment[i]
        .Damage.Piercing.Min;
      Damage.Piercing.Max := Damage.Piercing.Max + Equipment[i]
        .Damage.Piercing.Max;
      Damage.Crushing.Min := Damage.Crushing.Min + Equipment[i]
        .Damage.Crushing.Min;
      Damage.Crushing.Max := Damage.Crushing.Max + Equipment[i]
        .Damage.Crushing.Max;
      Damage.Cutting.Min := Damage.Cutting.Min + Equipment[i]
        .Damage.Cutting.Min;
      Damage.Cutting.Max := Damage.Cutting.Max + Equipment[i]
        .Damage.Cutting.Max;
      Damage.Heat.Min := Damage.Heat.Min + Equipment[i].Damage.Heat.Min;
      Damage.Heat.Max := Damage.Heat.Max + Equipment[i].Damage.Heat.Max;
      Damage.Cold.Min := Damage.Cold.Min + Equipment[i].Damage.Cold.Min;
      Damage.Cold.Max := Damage.Cold.Max + Equipment[i].Damage.Cold.Max;
      Damage.Electric.Min := Damage.Electric.Min + Equipment[i]
        .Damage.Electric.Min;
      Damage.Electric.Max := Damage.Electric.Max + Equipment[i]
        .Damage.Electric.Max;
      Damage.Poison.Min := Damage.Poison.Min + Equipment[i].Damage.Poison.Min;
      Damage.Poison.Max := Damage.Poison.Max + Equipment[i].Damage.Poison.Max;
      Damage.Magic.Min := Damage.Magic.Min + Equipment[i].Damage.Magic.Min;
      Damage.Magic.Max := Damage.Magic.Max + Equipment[i].Damage.Magic.Max;
      Damage.Mental.Min := Damage.Mental.Min + Equipment[i].Damage.Mental.Min;
      Damage.Mental.Max := Damage.Mental.Max + Equipment[i].Damage.Mental.Max;
      Damage.Stun.Min := Damage.Stun.Min + Equipment[i].Damage.Stun.Min;
      Damage.Stun.Max := Damage.Stun.Max + Equipment[i].Damage.Stun.Max;
      Damage.Special.Min := Damage.Special.Min + Equipment[i]
        .Damage.Special.Min;
      Damage.Special.Max := Damage.Special.Max + Equipment[i]
        .Damage.Special.Max;
    end;
  end;
end;

procedure TCharacter.CalcStats;
var
  i: TSlot;
  j: Integer;
  F: Single;
const
  FailName: string = 'TCharacter.CalcStats';
begin
  log.DebugLog(FailName);
  try

    if Loading then
      exit;

    FMovement := BaseMovement;
    FStrength := BaseStrength;
    FCoordination := BaseCoordination;
    FConstitution := BaseConstitution;
    FMysticism := BaseMysticism;
    FCombat := BaseCombat;
    FStealth := BaseStealth;
    FRestriction := 0;
    FAttackRecovery := BaseAttackRecovery;
    FHitRecovery := BaseHitRecovery;
    FPerception := BasePerception;
    FCharm := BaseCharm;
    FHealingRate := BaseHealingRate;
    FRechargeRate := BaseRechargeRate;
    FHitPoints := BaseHitPoints;
    FMana := BaseMana;
    FAttackBonus := BaseAttackBonus;
    FDefense := BaseDefense;

    Resistance := BaseResistance;
    UnArmedDamage := BaseUnArmedDamage;

    for i := slLeg1 to slMisc3 do
    begin
      if assigned(FEquipment[i]) then
      begin
        if i = slWeapon then
        begin
          if CombatMode then
            FEquipment[i].Equip(Self);
        end
        else
          FEquipment[i].Equip(Self);
      end;
    end;

    for j := 0 to Titles.Count - 1 do
    begin
      if assigned(Titles.objects[j]) then
        ApplyModifier(PStatModifier(Titles.objects[j]));
    end;

    ColorR := 0;
    ColorG := 0;
    ColorB := 0;
    for j := 0 to Effects.Count - 1 do
      Effects.items[j].Adjust(Self);

    FAttackBonus := FAttackBonus + FStrength / 5 + FCoordination / 2 + FCombat -
      FRestriction / 10;
    FDefense := FDefense + FCoordination / 2 + FCombat - FRestriction / 10 + 10;
    FBowBonus := FAttackBonus + FCoordination + FPerception / 2 + FCombat / 4 -
      FRestriction / 5;

    if FCombatMode and assigned(FEquipment[slWeapon]) and
      (FEquipment[slWeapon] is TWeapon) then
    begin
      if FEquipment[slWeapon] is TBow then
      begin
        if assigned(FEquipment[slBelt]) then
        begin
          if FEquipment[slBelt] is TQuiver then
          begin
            TWeapon(FEquipment[slWeapon]).Damage :=
              TQuiver(FEquipment[slBelt]).Damage;
          end;
        end;
      end;
      TWeapon(FEquipment[slWeapon]).GetDamage(Self);
      // Add unarmed damage
      FRange := TWeapon(FEquipment[slWeapon]).Range;
    end
    else
    begin // Unarmed
      FRange := 4;
      Damage := UnArmedDamage;
      F := Strength / 10;
      Damage.Piercing.Min := Damage.Piercing.Min * F;
      Damage.Piercing.Max := Damage.Piercing.Max * F;
      Damage.Crushing.Min := Damage.Crushing.Min * F;
      Damage.Crushing.Max := Damage.Crushing.Max * F;
      Damage.Cutting.Min := Damage.Cutting.Min * F;
      Damage.Cutting.Max := Damage.Cutting.Max * F;
      Damage.Stun.Min := Damage.Stun.Min * F;
      Damage.Stun.Max := Damage.Stun.Max * F;
    end;
    FHitPoints := FHitPoints + FConstitution / 2;
    FMana := FMana + FMysticism / 2 + FConstitution / 4;
    FAttackRecovery := FAttackRecovery - (FCoordination + FStrength) div 10 +
      FRestriction div 10;
    if FAttackRecovery < 0 then
      FAttackRecovery := 0;
    if FAttackRecovery > 63 then
      FAttackRecovery := 63;
    FHitRecovery := FHitRecovery - FConstitution div 4;
    if FHitRecovery < 0 then
      FHitRecovery := 0;
    Heal := FHitPoints * FHealingRate * FConstitution / 1000000;
    Recharge := FMana * FRechargeRate * FConstitution / 500000;
    PerceptionFactor := sqrt(FPerception / 10);
    Speed := FMovement;
    ThresholdOfPain := round(FHitPoints / 5);
  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

constructor TCharacter.Create(X, Y, Z: Integer; Frame: Word; Enabled: boolean);
const
  FailName: string = 'TCharacter.Create';
begin
  log.DebugLog(FailName);
  try

    inherited;
    BaseStrength := 7;
    BaseCoordination := 7;
    BaseConstitution := 7;
    BaseMysticism := 5;
    BaseCombat := 5;
    BaseStealth := 5;
    BasePerception := 10;
    BaseCharm := 10;
    BaseHealingRate := 10;
    BaseRechargeRate := 10;
    BaseHitPoints := 20;
    BaseMana := 10;
    BaseAttackRecovery := 12;
    BaseHitRecovery := 0;
    TrainingPoints := 0;
    FVision := 400;
    Hearing := 80;
    Smell := 40;
    MysticVision := 0;
    FWounds := 0;
    FDrain := 0;
    FMoney := 0;
    BuyingDiscount := 0.75;
    SellingMarkup := 1.25;
    BaseUnArmedDamage.Crushing.Min := 0;
    BaseUnArmedDamage.Crushing.Max := 2;
    OnScriptEnd := ScriptEnd;
    OnPathStep := PathStep;
    OnCollideFigure := CollideFigure;
    OnCollideItem := CollideItem;
    OnStop := Stop;
    OnNoPath := NoPath;
    OnTrigger := Trigger;
    OnFilter := Filter;
    FInventory := TList.Create;
    Titles := TStringList.Create;
    FFriends := TStringList.Create;
    FFriends.Sorted := true;
    FFriends.Duplicates := dupIgnore;
    FEnemies := TStringList.Create;
    FEnemies.Sorted := true;
    FEnemies.Duplicates := dupIgnore;
    Effects := TList<TEffect>.Create;
    CalcStats;
    FrameCount := random(10);
    FCombatMode := true;
    FReady := true;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

destructor TCharacter.Destroy;
var
  i: Integer;
const
  FailName: string = 'TCharacter.Destroy';
begin
  log.DebugLog(FailName);
  try
    AI.free;
    FInventory.free;
    if assigned(Avoid) then
      Avoid.free;
    FFriends.free;
    FEnemies.free;
    Effects.free;
    for i := 0 to Titles.Count - 1 do
    begin
      if assigned(Titles.objects[i]) then
      begin
        PStatModifier(Titles.objects[i]).DisplayName := '';
        Dispose(PStatModifier(Titles.objects[i]));
      end;
    end;
    Titles.free;
    if assigned(SoundLib) then
    begin
      SoundLib.FreeSound(AttackSounds);
      SoundLib.FreeSound(DeathSounds);
      SoundLib.FreeSound(PainSounds);
      SoundLib.FreeSound(BattleCries);
    end;
    inherited;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TCharacter.FindFreeInventoryXY(Item: TItem): boolean;
const
  MaxWidth = 12;
  MaxHeight = 14;
var
  iX, iY, i, j: Integer;
  Inv: array [0 .. MaxWidth - 1, 0 .. MaxHeight - 1] of boolean;
  Found: boolean;
const
  FailName: string = 'TCharacter.FindFreeInventoryXY';
begin
  result := false;

  log.DebugLog(FailName);
  try

    if not assigned(Item) then
      exit;

    ZeroMemory(@Inv, sizeof(Inv));

    // Set current inventory slots to true
    for i := 0 to Inventory.Count - 1 do
    begin
      with TItem(Inventory.items[i]) do
      begin
        for iX := InvX to InvX + InvW - 1 do
        begin
          for iY := InvY to InvY + InvH - 1 do
          begin
            Inv[iX, iY] := true;
          end;
        end;
      end;
    end;

    // Find a spot for the item
    for i := 0 to MaxWidth - Item.InvW + 1 do
    begin
      for j := 0 to MaxHeight - Item.InvH + 1 do
      begin
        Found := true;
        for iX := i to i + Item.InvW - 1 do
        begin
          for iY := j to j + Item.InvH - 1 do
          begin
            if (iX >= MaxWidth) or (iY >= MaxHeight) then
            begin
              Found := false;
              break;
            end
            else if Inv[iX, iY] then
            begin
              Found := false;
              break;
            end;
          end;
          if not Found then
            break;
        end;
        if Found then
        begin
          Item.InvX := i;
          Item.InvY := j;
          result := true;
          exit;
        end;
      end;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.Die;
var
  event: string;
const
  FailName: string = 'TCharacter.Die';
begin
  log.DebugLog(FailName);
  try

    inherited Stop;
    Dead := true;
    FTarget := nil;
    if assigned(Avoid) then
      Avoid.free;
    Avoid := nil;
    FAttacking := false;
    AutoFight := false;
    FCasting := false;
    NextAction := naNone;
    FReady := false;

    WasPartyMember := (NPCList.IndexOf(Self) >= 0);
    if WasPartyMember and (Self <> Player) then
      frmMain.RemoveFromParty(Self);

    if random < 0.125 then
    begin
      if ActionExists('DeathSpin') then
      begin
        if not inherited DoAction('DeathSpin') then
          Frame := 0;
      end
      else
      begin
        if not inherited DoAction('Death') then
          Frame := 0;
      end;
    end
    else
    begin
      if not inherited DoAction('Death') then
        Frame := 0;
    end;

    Dieing := true;
    if assigned(AI) then
    begin
      AI.WasKilled(nil);
    end
    else
    begin
      PlaySound(DeathSounds, X, Y);
    end;
    AIMode := ainone;
{$IFDEF AILog}
    log.log(GUID + ' dies');
{$ENDIF}
    Inc(DieCount);
    event := 'OnDie[' + IntToStr(DieCount) + ']';
    if PropertyExists(event) then
    begin
      RunScript(Self, Properties[event]);
    end
    else
    begin
      RunScript(Self, OnDie);
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.DoFrame;
var
  Roll, ToHit, F: Single;
  Total, Stun: Single;
  event: string;
  i: Integer;
  Changed: boolean;
  NewDamage: TDamageProfile;
const
  FailName: string = 'TCharacter.DoFrame';
begin
  log.DebugLog(FailName);
  try

    inherited;
    Inc(FrameCount);
    if FAIMode <> NewAIMode then
      InitAI;

    Changed := false;
    for i := Effects.Count - 1 downto 0 do
    begin
      if Effects[i].DoFrame then
      begin
        if Effects[i].DisableWhenDone then
        begin
          Effects[ i ].free;
          // Effects.ExtractAt(i);
          Effects.Delete( i );
          Enabled := false;
          exit;
        end
        else
        begin
          Effects[i].free;
          Effects.Delete(i);
          Changed := true;
        end;
      end;
    end;
    if Changed then
      CalcStats;

    if FDead then
    begin
      Inc(FDeadCount);
      if Speed > 0.0625 then
        Speed := Speed / 1.125
      else if Speed > 0 then
      begin
        Speed := 0;
        inherited Stop;
      end;
    end
    else
    begin
      FDrain := FDrain - Recharge;
      if FDrain < 0 then
        FDrain := 0;
      FWounds := FWounds - Heal;
      if FWounds < 0 then
        FWounds := 0;

      if FRecoveryCount > 0 then
      begin
        Dec(FRecoveryCount);
        if FRecoveryCount = 0 then
        begin
          FAttacking := false;
          FCasting := false;
          if NextAction = naAttack then
          begin
            FReady := true;
            if assigned(NextTarget) then
            begin
              if TCharacter(NextTarget).Dead then
              begin
                FReady := true;
                Stand;
              end
              else
              begin
                if InRange(NextTarget) then
                  Attack(TCharacter(NextTarget))
                else
                  Approach(TSpriteObject(NextTarget));
              end;
            end
            else
            begin
              AttackPoint(NextTargetX, NextTargetY);
            end;
          end
          else if NextAction = naCast then
          begin
            FReady := true;
            if assigned(NextTarget) then
              Cast(TCharacter(NextTarget))
            else
              CastPoint(NextTargetX, NextTargetY);
          end
          else if NextAction = naWalk then
          begin
            FReady := true;
            if assigned(NextTarget) then
              Approach(TSpriteObject(NextTarget))
            else
              WalkTo(NextTargetX, NextTargetY, 64);
          end
          else if NextAction = naRun then
          begin
            FReady := true;
            if assigned(NextTarget) then
              ApproachRun(TSpriteObject(NextTarget))
            else
              RunTo(NextTargetX, NextTargetY, 64);
          end
          else
          begin
            FReady := true;
            Stand;
          end;
        end;
      end
      else if InitStand then
      begin
        FAttacking := false;
        AutoFight := false;
        FCasting := false;
        NextAction := naNone;
        if StandAction = '' then
          DoAction('Stand')
        else
          DoAction(StandAction);
        FrameMultiplier := FrameMultiplier + random(FrameMultiplier div 2) -
          FrameMultiplier div 4;
        // Delay := -random(FrameMultiplier) - 1;
        InitStand := false;
        FReady := true;
      end
      else if FAttacking then
      begin
        if CombatMode and assigned(Equipment[slWeapon]) and
          (Equipment[slWeapon] is TBow) and assigned(Equipment[slBelt]) and
          (Equipment[slBelt] is TQuiver) then
        begin
          if (ScriptFrame = TCharacterResource(Resource).ReleaseFrame) and
            (Delay = 0) then
          begin // Use ReleaseFrame if Bow
            PlaySound(TWeapon(FEquipment[slWeapon]).AttackSounds, X, Y);
            if assigned(FTarget) then
            begin
              if Equipment[slWeapon] is TWeapon then
                TWeapon(Equipment[slWeapon]).DoDamage(Self, TCharacter(FTarget))
            end
            else
            begin
              if Equipment[slWeapon] is TWeapon then
                TWeapon(Equipment[slWeapon]).DoDamage(Self, nil)
            end;
          end;
        end
        else
        begin
          if (ScriptFrame = 5) and (Delay = 0) then
          begin
            if CombatMode and assigned(FEquipment[slWeapon]) and
              (FEquipment[slWeapon] is TWeapon) then
              PlaySound(TWeapon(FEquipment[slWeapon]).AttackSounds, X, Y)
            else
              PlaySound(AttackSounds, X, Y);
          end;
          if (ScriptFrame = TCharacterResource(Resource).ContactFrame) and
            (Delay = 0) then
          begin
            if assigned(FTarget) then
            begin
              if InRange(FTarget) then
              begin
                if CombatMode and assigned(Equipment[slWeapon]) then
                begin
                  if Equipment[slWeapon] is TWeapon then
                    TWeapon(Equipment[slWeapon])
                      .DoDamage(Self, TCharacter(FTarget));
                end
                else
                begin
                  Roll := random(40);
                  ToHit := Roll + AttackBonus;
{$IFDEF AILog}
                  log.log(GUID + ' strikes at ' + FTarget.GUID + ' and...');
                  log.log('  needs: ' + floattostr(TCharacter(FTarget)
                    .Defense));
                  log.log('    got: ' + floattostr(ToHit) + ' (' +
                    floattostr(Roll) + ')');
{$ENDIF}
                  if Roll < 1 then
                  begin
{$IFDEF AILog}
                    log.log(GUID + ' misses');
{$ENDIF}
                    TCharacter(FTarget).TakeDamage(Self, 0, 0, false);
                  end
                  else if (Roll > 35) or (ToHit > TCharacter(FTarget).Defense)
                  then
                  begin
                    NewDamage := Damage;
                    if not TCharacter(FTarget).AffectDamage(Self, @NewDamage)
                    then
                    begin
                      if TCharacter(FTarget).Track = Self then
                        F := 1
                      else
                      begin
                        F := 1;
                        // F:=Stealth/10;
                        // if F<1 then F:=1;
                      end;
                      // if ToHit > TCharacter(FTarget).Defense then begin
                      // F:=F*(1+(ToHit-TCharacter(FTarget).Defense)/40);
                      // if F<1 then F:=1;
                      // end;
                      if (Roll > 35) then
                      begin
{$IFDEF AILog}
                        log.log(GUID + ' scores a critical hit');
{$ENDIF}
                        if (Self = Current) then
                          F := F * 2;
                        Total := CalcTotalDamage(NewDamage,
                          TCharacter(FTarget).Resistance, F, true);
                      end
                      else
                      begin
                        Total := CalcTotalDamage(NewDamage,
                          TCharacter(FTarget).Resistance, F, false);
                      end;
{$IFDEF AILog}
                      log.log(TCharacter(FTarget).GUID + ' takes ' +
                        IntToStr(round(Total)) +
                        ' points of damage from ' + GUID);
{$ENDIF}
                      Stun := CalcDamage(NewDamage.Stun) * F -
                        TCharacter(FTarget).Resistance.Stun.Invulnerability;
                      if Stun > 0 then
                        Stun := Stun *
                          (1 - TCharacter(FTarget).Resistance.Stun.Resistance);
                      TCharacter(FTarget).TakeDamage(Self, Total, Stun, false);
                    end;
                  end
                  else
                  begin
{$IFDEF AILog}
                    log.log(GUID + ' misses');
{$ENDIF}
                    TCharacter(FTarget).TakeDamage(Self, 0, 0, false);
                  end;
                end;
              end;
            end
            else
            begin
              if CombatMode and assigned(Equipment[slWeapon]) then
                if Equipment[slWeapon] is TWeapon then
                  TWeapon(Equipment[slWeapon]).DoDamage(Self, nil);
            end;
          end;
        end;
      end
      else if FCasting then
      begin
        if (ScriptFrame = TCharacterResource(Resource).ReleaseFrame) and
          (Delay = 0) then
        begin // Change to Cast
          if assigned(CurrentSpell) then
          begin
            if CurrentSpell.Cast(Self, FTarget) then
            begin
              FDrain := FDrain + ManaDrain;
            end;
          end;
        end;
      end
      else if TargetReached then
      begin
        TargetReached := false;
        if assigned(FTarget) then
        begin
          if FTarget is TCharacter then
          begin
            if TCharacter(FTarget).Dead then
            begin
              AutoFight := false;
              if not TCharacter(FTarget).Dieing and not CombatMode and
                not InterfaceLocked and not(FTarget.Frame = 0) and
                not TCharacter(FTarget).WillBeDisabled then
              begin
                frmMain.BeginLoot(Current, FTarget);
              end;
              FTarget := nil;
            end
            else if TCharacter(FTarget).IsEnemy(Current) then
            begin
              Face(FTarget.X, FTarget.Y);
              AutoFight := (Self = Current);
              Attack(TCharacter(FTarget));
            end
            else
            begin
              i := NPCList.IndexOf(FTarget);
              if i >= 0 then
              begin
                if assigned(TCharacter(NPCList.items[i]).FAI) and
                  (TCharacter(NPCList.items[i]).FAI is TPartyAI) and
                  not CombatMode and not InterfaceLocked then
                begin
                  frmMain.BeginObjInventory(Current, NPCList.items[i]);
                end;
                FTarget := nil;
              end
              else
              begin
                if ((TCharacter(FTarget).AIMode = aiIdle) or
                  (TCharacter(FTarget).AIMode = ainone)) and not CombatMode and
                  not InterfaceLocked then
                // Only allow activate on non combative characters
                  TSpriteObject(FTarget).Activate;
                FTarget := nil;
              end;
            end;
          end
          else if (FTarget is TItem) and not InterfaceLocked then
          begin
            if FindFreeInventoryXY(TItem(FTarget)) then
            begin
              TItem(FTarget).PickUp;
              Inventory.add(FTarget);
              TItem(FTarget).Enabled := false;
              TItem(FTarget).LayeredImage :=
                PartManager.GetImageFile(TItem(FTarget).PartName,
                TCharacterResource(Resource).NakedName);
              TItem(FTarget).Resource := PartManager.GetLayerResource
                (TItem(FTarget).LayeredImage);
            end
            else
              Say(FullInvMsg, cTalkWhiteColor);
            FTarget := nil;
          end
          else if (FTarget is TContainer) and not InterfaceLocked then
          begin
            if TContainer(FTarget).Closed then
            begin
              if TContainer(FTarget).KeyRequired then
              begin
                if InInventory(TContainer(FTarget).KeyName) then
                begin
                  TContainer(FTarget).Activate;
                end
                else
                begin
                  Inc(TContainer(FTarget).OpenAttemptCount);
                  event := 'OnOpenAttempt[' +
                    IntToStr(TContainer(FTarget).OpenAttemptCount) + ']';
                  if TContainer(FTarget).PropertyExists(event) then
                    RunScript(FTarget, TContainer(FTarget).Properties[event])
                  else
                    RunScript(FTarget, TContainer(FTarget).OnOpenAttempt);
                end;
              end
              else
              begin
                TContainer(FTarget).Activate;
              end;
            end
            else
            begin
              if Shifted then
                TContainer(FTarget).Activate
              else
              begin
                if TContainer(FTarget).FixMissingItems then
                  frmMain.BeginObjInventory(Current, FTarget);
              end;
            end;
            FTarget := nil;
          end
          else if (FTarget is TDoor) and not InterfaceLocked then
          begin
            if TDoor(FTarget).Closed then
            begin
              if TDoor(FTarget).KeyRequired then
              begin
                if InInventory(TDoor(FTarget).KeyName) then
                begin
                  TDoor(FTarget).Activate;
                end
                else
                begin
                  Inc(TDoor(FTarget).OpenAttemptCount);
                  event := 'OnOpenAttempt[' +
                    IntToStr(TDoor(FTarget).OpenAttemptCount) + ']';
                  if TDoor(FTarget).PropertyExists(event) then
                    RunScript(FTarget, TDoor(FTarget).Properties[event])
                  else
                    RunScript(FTarget, TDoor(FTarget).OnOpenAttempt);
                end;
              end
              else
              begin
                TDoor(FTarget).Activate;
              end;
            end
            else
            begin
              if Shifted then
                TDoor(FTarget).Activate;
            end;
            FTarget := nil;
          end
          else
          begin
            if not InterfaceLocked then
              TSpriteObject(FTarget).Activate;
            FTarget := nil;
          end;
        end;
      end
      else
      begin
        if assigned(FTarget) then
        begin
          if (FTarget.X <> FTargetX) or (FTarget.Y <> FTargetY) then
          begin
            FindAgain := false;
            Speed := FMovement;
            AntiPathMode := AntiPathEnabled and
              (((X < Game.OffsetX) and (Self.X < Game.OffsetX)) or
              ((Y < Game.OffsetY) and (Self.Y < Game.OffsetY)) or
              ((X > Game.RightX) and (Self.X > Game.RightX)) or
              ((Y > Game.BottomY) and (Self.Y > Game.BottomY)));
            if AntiPathMode then
            begin
              FDeviance := PathDeviance;
              MoveTo(FTarget.X, FTarget.Y, Z);
              PathStep(Self, X, Y);
            end
            else
            begin
              FindPathTo(FTarget.X, FTarget.Y, Avoid, PathDeviance);
            end;
            FTargetX := FTarget.X;
            FTargetY := FTarget.Y;
          end;
        end;
      end;
      if FindAgain then
      begin
        FindAgain := false;
        Speed := FMovement;
        AntiPathMode := AntiPathEnabled and
          (((X < Game.OffsetX) and (Self.X < Game.OffsetX)) or
          ((Y < Game.OffsetY) and (Self.Y < Game.OffsetY)) or
          ((X > Game.RightX) and (Self.X > Game.RightX)) or
          ((Y > Game.BottomY) and (Self.Y > Game.BottomY)));
        if AntiPathMode then
        begin
          FDeviance := PathDeviance;
          MoveTo(PathDestX, PathDestY, Z);
          PathStep(Self, X, Y);
        end
        else
        begin
          FindPathTo(PathDestX, PathDestY, Avoid, PathDeviance);
        end;
        InitStand := false;
      end;

      if FReady and (RecoveryCount = 0) and not FFrozen then
      begin
        if assigned(AI) then
        begin
          AI.Execute;
        end;
      end;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.Face(X, Y: Integer);
const
  FailName: string = 'TCharacter.Face';
begin
  log.DebugLog(FailName);
  try

    FFacing := GetFacing(Self.X, Self.Y, X, Y);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TCharacter.GetEquipment(Slot: TSlot): TItem;
begin
  result := FEquipment[Slot];
end;

procedure TCharacter.SetEquipment(Slot: TSlot; const Value: TItem);
var
  i: Integer;
const
  FailName: string = 'TCharacter.SetEquipment';
begin
  log.DebugLog(FailName);
  try

    FEquipment[Slot] := Value;

    if assigned(Value) then
    begin
      i := Inventory.IndexOf(Value);
      if i >= 0 then
        Inventory.Delete(i);

      // Make sure we have the current version of the part for this character
      if assigned(Resource) then
        Value.LayeredImage := PartManager.GetImageFile(Value.PartName,
          TCharacterResource(Resource).NakedName);
    end;

    CalcStats;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TCharacter.ValidateSpells: boolean;
var
  i: Integer;
begin
  result := true;
  if assigned(CurrentSpell) then
  begin
    if not TitleExists(CurrentSpell.Name) then
    begin
      CurrentSpell := nil;
      result := false;
    end;
  end;
  for i := 1 to 10 do
  begin
    if assigned(HotKey[i]) then
    begin
      if not TitleExists(HotKey[i].Name) then
      begin
        HotKey[i] := nil;
        result := false;
      end;
    end;
  end;
end;

procedure TCharacter.Stand;
const
  FailName: string = 'TCharacter.Stand';
begin
  log.DebugLog(FailName);
  try

    FTarget := nil;
    InitStand := true; // Stand on the next frame unless another walk is started

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TCharacter.TakeDamage(Source: TCharacter; Damage, Stun: Single;
  UseStealth: boolean): Integer;
var
  event: string;
  D, dx, dy: Double;
  sX, sY: longint;
  ApparentSource: TCharacter;
  F: Single;
  Recovery: Integer;
const
  FailName: string = 'TCharacter.TakeDamage';
begin
  result := 0;
  log.DebugLog(FailName);
  try

    if FDead then
      exit;
    if (Damage > ThresholdOfPain) or (Stun > 0) then
    begin
      inherited Stop;
      if assigned(FAI) then
        TAI(FAI).OnStop;
    end;

    if UseStealth then
    begin
      ApparentSource := nil;
      if assigned(Source) then
      begin
        if Source.Stealth <= 10 then
          ApparentSource := Source
        else
        begin
          F := 10 / Source.Stealth;
          if random < F then
            ApparentSource := Source;
        end;
      end;
    end
    else
    begin
      ApparentSource := Source
    end;

    if (BaseHitPoints > 0) then
      FWounds := FWounds + Damage;
    if (FWounds >= FHitPoints) and (BaseHitPoints > 0) then
    begin
      inherited Stop;
      FTarget := nil;
      FAttacking := false;
      FCasting := false;
      FReady := false;
      Dead := true;
      WasPartyMember := (NPCList.IndexOf(Self) >= 0);
      if WasPartyMember and (Self <> Player) then
        frmMain.RemoveFromParty(Self);
      FRecoveryCount := 0;
      if random < 0.125 then
      begin
        if ActionExists('DeathSpin') then
          Enabled := inherited DoAction('DeathSpin')
        else
          Enabled := inherited DoAction('Death');
      end
      else
        Enabled := inherited DoAction('Death');
      if Enabled then
      begin
        Dieing := true;
        if assigned(Source) then
        begin
          if assigned(Resource) and (Resource is TCharacterResource) and
            (TCharacterResource(Resource).DeathSlide) then
            if HitPoints > 0 then
            begin
              dx := X - Source.X;
              dy := Y - Source.Y;
              D := sqrt(sqr(dx) + sqr(dy));
              if D > 0 then
              begin
                Speed := 4 * Damage / HitPoints;
                // damage done vs total hit points
                if Speed > 16 then
                  Speed := 16;
                sX := round(8 * dx / D);
                sY := round(8 * dy / D);
                Move(X + sX, Y + sY, Z);
              end;
            end;
        end;
      end;
      if assigned(AI) then
      begin
        AI.WasAttacked(ApparentSource, Damage);
        AI.WasKilled(ApparentSource);
      end
      else
      begin
        PlaySound(DeathSounds, X, Y);
      end;
      AIMode := ainone;
{$IFDEF AILog}
      log.log(GUID + ' dies');
{$ENDIF}
      Inc(DieCount);
      event := 'OnDie[' + IntToStr(DieCount) + ']';
      if PropertyExists(event) then
        RunScript(Self, Properties[event])
      else
        RunScript(Self, OnDie);
    end
    else
    begin
      if (Damage = 0) and (Stun > 0) then
      begin // Took stun damage only i.e. the hold spell
        if DoAction('Pain') then
        begin
          Recovery := FHitRecovery + round(Stun);
          if Recovery > FRecoveryCount then
            FRecoveryCount := Recovery;
          result := Recovery;
          FTarget := nil;
          FAttacking := false;
          FCasting := false;
          FReady := false;
        end
        else
        begin
          Stand;
          FRecoveryCount := 0;
        end;
      end
      else if (Damage > ThresholdOfPain) then
      begin
        if random(3) = 0 then // JAH
          if DoAction('Pain') then
          begin
            Recovery := FHitRecovery + round(Stun);
            if Recovery > FRecoveryCount then
              FRecoveryCount := Recovery;
            result := Recovery;
            FTarget := nil;
            FAttacking := false;
            if assigned(CurrentSpell) then // JAH
            begin // Interupt casting
              if CurrentSpell.Interupted then
                FCasting := false;
            end;
            FReady := false;
          end
          else
          begin
            Stand;
            FRecoveryCount := 0;
          end;
      end;

      if (AIMode = ainone) and (Self <> Current) then
      begin
        AIMode := aiCombat;
        Track := ApparentSource;
      end;
      if assigned(AI) then
      begin
        AI.WasAttacked(ApparentSource, Damage);
      end
      else
      begin
        if (Damage > ThresholdOfPain) or (Stun > 0) then
        begin
          if not InPain then
          begin
            InPain := true;
            PlaySound(PainSounds, X, Y);
          end;
        end;
      end;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.WalkTo(X, Y, Deviance: Integer);
const
  FailName: string = 'TCharacter.WalkTo';
begin
  log.DebugLog(FailName);
  try

    if FFrozen then
      exit;
    if FReady then
    begin
      NextAction := naNone;
      Movement := TResource(Resource).Speed;
      MoveMode := 'Walk';
      FDeviance := Deviance;
      if assigned(Avoid) then
        Avoid.Clear;
      // By retaining this list, the character will be smarter, but will
      if (X <> Self.X) or (Y <> Self.Y) then
      begin // increasingly slow performance.
        FTarget := nil;
        AntiPathMode := AntiPathEnabled and
          (((X < Game.OffsetX) and (Self.X < Game.OffsetX)) or
          ((Y < Game.OffsetY) and (Self.Y < Game.OffsetY)) or
          ((X > Game.RightX) and (Self.X > Game.RightX)) or
          ((Y > Game.BottomY) and (Self.Y > Game.BottomY)));
        if AntiPathMode then
        begin
          FDeviance := Deviance;
          MoveTo(X, Y, Z);
          PathStep(Self, X, Y);
        end
        else
        begin
          FindPathTo(X, Y, Avoid, Deviance);
        end;
        InitStand := false;
      end;
    end
    else
    begin
      NextAction := naWalk;
      NextTarget := nil;
      NextTargetX := X;
      NextTargetY := Y
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.RunTo(X, Y, Deviance: Integer);
const
  FailName: string = 'TCharacter.RunTo';
begin
  log.DebugLog(FailName);
  try

    if not RunExists then
    begin
      WalkTo(X, Y, Deviance);
      exit;
    end;

    if FFrozen then
      exit;
    if FReady then
    begin
      NextAction := naNone;
      Movement := TResource(Resource).RunSpeed;
      MoveMode := 'Run';
      FDeviance := Deviance;
      if assigned(Avoid) then
        Avoid.Clear;
      // By retaining this list, the character will be smarter, but will
      if (X <> Self.X) or (Y <> Self.Y) then
      begin // increasingly slow performance.
        FTarget := nil;
        AntiPathMode := AntiPathEnabled and
          (((X < Game.OffsetX) and (Self.X < Game.OffsetX)) or
          ((Y < Game.OffsetY) and (Self.Y < Game.OffsetY)) or
          ((X > Game.RightX) and (Self.X > Game.RightX)) or
          ((Y > Game.BottomY) and (Self.Y > Game.BottomY)));
        // AntiPathMode:=true;
        if AntiPathMode then
        begin
          MoveTo(X, Y, Z);
          PathStep(Self, X, Y);
        end
        else
        begin
          FindPathTo(X, Y, Avoid, Deviance);
        end;
        InitStand := false;
      end;
    end
    else
    begin
      NextAction := naRun;
      NextTarget := nil;
      NextTargetX := X;
      NextTargetY := Y
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.ScriptEnd(Sender: TObject);
const
  FailName: string = 'TCharacter.ScriptEnd';
begin
  log.DebugLog(FailName);
  try

    if FAttacking then
    begin
      if UseAttackRecovery then
        FRecoveryCount := FAttackRecovery + 1
      else
        FRecoveryCount := 3; // 9;
    end
    else if FCasting then
    begin
      FRecoveryCount := FCastRecovery + 1;
    end
    else if FDead then
    begin
      Dieing := false;
    end
    else if InPain then
    begin
      InPain := false;
      FReady := (FRecoveryCount <= 0);
      if FReady then
        InitStand := true;
    end
    else
    begin
      FReady := true;
      InitStand := true;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.PathStep(Sender: TAniFigure; X, Y: longint);
const
  FailName: string = 'TCharacter.PathStep';
var
  OldDelay: longint;
begin
  log.DebugLog(FailName);
  try

    FFacing := GetFacing(Self.X, Self.Y, X, Y);
    OldDelay := Delay;
    if OldDelay < 0 then
      OldDelay := 0;
    PlayScript(MoveMode + Facing.ToString, ScriptFrame, smRepeat);
    Delay := OldDelay;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.Attack(Target: TCharacter);
const
  FailName: string = 'TCharacter.Attack';
begin
  log.DebugLog(FailName);
  try

    if FFrozen then
      exit;
    FCasting := false;

    if FReady then
    begin
      inherited Stop;
      Face(Target.X, Target.Y);
      if CombatMode and assigned(FEquipment[slWeapon]) and
        (FEquipment[slWeapon] is TBow) and assigned(FEquipment[slBelt]) and
        (FEquipment[slBelt] is TQuiver) then
      begin
        if DoAction('BowAttack') then
        begin
          FTargetX := Target.X;
          FTargetY := Target.Y;
          FAttacking := true;
          UseAttackRecovery := true;
          if Target.Dead then
          begin
            AutoFight := false;
            NextAction := naNone;
          end;
          if AutoFight then
          begin
            NextAction := naAttack;
            NextTarget := Target;
            NextTargetX := Target.X;
            NextTargetY := Target.Y;
          end
          else
            NextAction := naNone;
          FReady := false;
          InitStand := false;
          FTarget := Target;
        end
        else
        begin
          Stand;
        end;
      end
      else
      begin
        if DoAction('Attack') then
        begin
          { if CombatMode and assigned(FEquipment[slWeapon]) and (FEquipment[slWeapon] is TWeapon) then
            PlaySound(TWeapon(FEquipment[slWeapon]).AttackSounds,X,Y)
            else
            PlaySound(AttackSounds,X,Y); }
          FTargetX := Target.X;
          FTargetY := Target.Y;
          FAttacking := true;
          UseAttackRecovery := false;
          if Target.Dead then
          begin
            AutoFight := false;
            NextAction := naNone;
          end;
          if AutoFight then
          begin
            NextAction := naAttack;
            NextTarget := Target;
            NextTargetX := Target.X;
            NextTargetY := Target.Y;
          end
          else
            NextAction := naNone;
          FReady := false;
          InitStand := false;
          FTarget := Target;
        end
        else
        begin
          Stand;
        end;
      end;
    end
    else
    begin
      NextAction := naAttack;
      NextTarget := Target;
      NextTargetX := Target.X;
      NextTargetY := Target.Y
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.AttackPoint(X, Y: Integer);
const
  FailName: string = 'TCharacter.AttackPoint';
begin
  log.DebugLog(FailName);
  try

    if FFrozen then
      exit;
    FCasting := false;
    if FReady then
    begin
      inherited Stop;
      Face(X, Y);
      FTarget := nil;
      if CombatMode and assigned(FEquipment[slWeapon]) and
        (FEquipment[slWeapon] is TBow) and assigned(FEquipment[slBelt]) and
        (FEquipment[slBelt] is TQuiver) then
      begin
        if DoAction('BowAttack') then
        begin
          FTargetX := X;
          FTargetY := Y;
          FAttacking := true;
          UseAttackRecovery := true;
          NextAction := naNone;
          FReady := false;
        end
        else
        begin
          Stand;
        end;
      end
      else
      begin
        if DoAction('Attack') then
        begin
          { if CombatMode and assigned(FEquipment[slWeapon]) and (FEquipment[slWeapon] is TWeapon) then
            PlaySound(TWeapon(FEquipment[slWeapon]).AttackSounds,self.X,self.Y)
            else
            PlaySound(AttackSounds,X,Y); }
          FTargetX := X;
          FTargetY := Y;
          FAttacking := true;
          UseAttackRecovery := false;
          NextAction := naNone;
          FReady := false;
        end
        else
        begin
          Stand;
        end;
      end;
    end
    else
    begin
      NextAction := naAttack;
      NextTarget := nil;
      NextTargetX := X;
      NextTargetY := Y;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.ApplyModifier(Modifier: PStatModifier);
const
  FailName: string = 'TCharacter.ApplyModifier';
begin
  log.DebugLog(FailName);
  try

    Inc(FStrength, Modifier.Strength);
    Inc(FCoordination, Modifier.Coordination);
    Inc(FConstitution, Modifier.Constitution);
    Inc(FMysticism, Modifier.Mysticism);
    Inc(FCombat, Modifier.Combat);
    Inc(FStealth, Modifier.Stealth);
    Inc(FRestriction, Modifier.Restriction);
    Inc(FAttackRecovery, Modifier.AttackRecovery);
    Inc(FHitRecovery, Modifier.HitRecovery);
    Inc(FPerception, Modifier.Perception);
    Inc(FCharm, Modifier.Charm);
    Inc(FHealingRate, Modifier.HealingRate);
    Inc(FRechargeRate, Modifier.RechargeRate);
    FHitPoints := FHitPoints + Modifier.HitPoints;
    FMana := FMana + Modifier.Mana;
    FAttackBonus := FAttackBonus + Modifier.Attack;
    FDefense := FDefense + Modifier.Defense;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.ApplyResistance(Profile: PDamageResistanceProfile);

  procedure ResCalc(var A: TDamageResistance; const B: TDamageResistance);
  begin
    A.Invulnerability := A.Invulnerability + B.Invulnerability;
    A.Resistance := A.Resistance + B.Resistance - (A.Resistance * B.Resistance);
  end;

const
  FailName: string = 'TCharacter.ApplyResistance';
begin
  log.DebugLog(FailName);
  try

    ResCalc(Resistance.Piercing, Profile.Piercing);
    ResCalc(Resistance.Crushing, Profile.Crushing);
    ResCalc(Resistance.Cutting, Profile.Cutting);
    ResCalc(Resistance.Heat, Profile.Heat);
    ResCalc(Resistance.Cold, Profile.Cold);
    ResCalc(Resistance.Electric, Profile.Electric);
    ResCalc(Resistance.Poison, Profile.Poison);
    ResCalc(Resistance.Magic, Profile.Magic);
    ResCalc(Resistance.Mental, Profile.Mental);
    ResCalc(Resistance.Stun, Profile.Stun);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.CollideFigure(Source, Target: TAniFigure;
  var Stop: boolean);
var
  i: Integer;
  event: string;
const
  FailName: string = 'TCharacter.CollideFigure';
begin
  log.DebugLog(FailName);
  try

    if FDead then
    begin
      Stop := true;
      exit;
    end;

    if TSpriteObject(Target).OnCollide <> '' then
    begin
      Inc(TSpriteObject(Target).CollideCount);
      event := 'OnCollide[' + IntToStr(TSpriteObject(Target)
        .CollideCount) + ']';
      if TSpriteObject(Target).PropertyExists(event) then
        RunScript(Self, TSpriteObject(Target).Properties[event])
      else
        RunScript(Self, TSpriteObject(Target).OnCollide);
    end;

    if Target = FTarget then
    begin
      Stop := true;
      TargetReached := true;
    end
    else
    begin
      if Target is TItem then
      begin
      end
      else if Target is TProjectile then
      begin
      end
      else if Target is TCharacter then
      begin
        if TCharacter(Target).Dead then
        begin
        end
        else
        begin
          if PartyMember and TCharacter(Target).PartyMember then
          else if AntiPathMode then
          else
          begin
            Stop := true;
            FindAgain := true;
            if assigned(FAI) then
              FindAgain := not AI.OnCollideFigure(Target);
            if FindAgain then
            begin
              if assigned(Avoid) then
              begin
                i := Avoid.IndexOf(Target);
                if i = -1 then
                  Avoid.add(Target);
              end
              else
              begin
                Avoid := TList<TAniFigure>.Create;
                Avoid.add(Target);
              end;
            end;
          end;
        end;
      end
      else if Target is TDoor then
      begin
        Stop := TDoor(Target).Closed;
        if assigned(AI) then
          AI.OnCollideObject(Target);
        FindAgain := false;
      end
      else
      begin
        if AntiPathMode then
        begin
        end
        else
        begin
          Stop := true;
          if assigned(AI) then
            FindAgain := not AI.OnCollideFigure(Target)
          else
            FindAgain := true;
          if assigned(Avoid) then
          begin
            i := Avoid.IndexOf(Target);
            if i = -1 then
              Avoid.add(Target);
          end
          else
          begin
            Avoid := TList<TAniFigure>.Create;
            Avoid.add(Target);
          end;
        end;
      end;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.CollideItem(Source: TAniFigure; var Stop: boolean);
const
  FailName: string = 'TCharacter.CollideItem';
begin
  log.DebugLog(FailName);
  try

    if AntiPathMode then
    begin
      Stop := false;
      AntiPathMode := false;
      FindPathTo(DestX, DestY, Avoid, FDeviance);
    end
    else
    begin
      Stop := true;
      if FDead then
        exit;
      if assigned(AI) then
        AI.OnCollideItem;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.Stop(Sender: TObject);
const
  FailName: string = 'TCharacter.Stop';
begin
  log.DebugLog(FailName);
  try

    if FDead then
      exit;
    if FindAgain then
      exit;
    FAttacking := false;
    FCasting := false;
    NextAction := naNone;
    AutoFight := false;
    if StandAction = '' then
      DoAction('Stand')
    else
      DoAction(StandAction);

    if FRecoveryCount > 0 then
      exit; // Hold was recovering after push was cast
    // FRecoveryCount := 0;          // vs walk in place?
    FReady := true;
    if assigned(AI) then
      AI.OnStop;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.NoPath(Sender: TObject);
const
  FailName: string = 'TCharacter.NoPath';
begin
  log.DebugLog(FailName);
  try

    if FindAgain then
      exit;
    FAttacking := false;
    FCasting := false;
    FTarget := nil;
    NextAction := naNone;
    AutoFight := false;
    FRecoveryCount := 0;
    if StandAction = '' then
      DoAction('Stand')
    else
      DoAction(StandAction);
    FReady := true;
    if assigned(AI) then
      AI.OnNoPath;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.Trigger(Source: TAniFigure; ID, PrevID: SmallInt);
var
  i: Integer;
const
  FailName: string = 'TCharacter.Trigger';
begin
  log.DebugLog(FailName);
  try

    if ID <= 0 then
      exit;
    i := ID - 1;
    if (i < FigureInstances.Count) and assigned(FigureInstances.objects[i]) and
      (FigureInstances.objects[i] is TTrigger) then
    begin
      TTrigger(FigureInstances.objects[i]).Trigger(Self);
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.Filter(Source: TAniFigure; ID, PrevID: SmallInt);
const
  FailName: string = 'TCharacter.Filter';
begin
  log.DebugLog(FailName);
  try

    if Self = View.KeyFigure then
    begin
      View.ItemMask := ID;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.Approach(ATarget: TSpriteObject);
var
  D: Double;
const
  FailName: string = 'TCharacter.Approach';
begin
  log.DebugLog(FailName);
  try

    IntendToZone := false;
    Shifted := false;
    if FReady then
    begin
      NextAction := naNone;
      D := sqrt(sqr(ATarget.X - StepX) + sqr(2 * (ATarget.Y - StepY)));
      if D <= ATarget.Radius + Radius then
        TargetReached := true
      else
        WalkTo(ATarget.X, ATarget.Y, 64);
      FTarget := ATarget;
      FTargetX := FTarget.X;
      FTargetY := FTarget.Y;
    end
    else
    begin
      NextAction := naWalk;
      NextTarget := ATarget;
      NextTargetX := ATarget.X;
      NextTargetY := ATarget.Y
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.ApproachRun(ATarget: TSpriteObject);
var
  D: Double;
const
  FailName: string = 'TCharacter.ApproachRun';
begin
  log.DebugLog(FailName);
  try

    if not RunExists then
    begin
      Approach(ATarget);
      exit;
    end;

    IntendToZone := false;
    Shifted := false;
    if FReady then
    begin
      NextAction := naNone;
      D := sqrt(sqr(ATarget.X - StepX) + sqr(2 * (ATarget.Y - StepY)));
      if D <= ATarget.Radius + Radius then
        TargetReached := true
      else
        RunTo(ATarget.X, ATarget.Y, 64);
      FTarget := ATarget;
      FTargetX := FTarget.X;
      FTargetY := FTarget.Y;
    end
    else
    begin
      NextAction := naRun;
      NextTarget := ATarget;
      NextTargetX := ATarget.X;
      NextTargetY := ATarget.Y
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.ShiftApproach(ATarget: TSpriteObject);
const
  FailName: string = 'TCharacter.ShiftApproach';
begin
  log.DebugLog(FailName);
  try

    Approach(ATarget);
    Shifted := true;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.ShiftApproachRun(ATarget: TSpriteObject);
const
  FailName: string = 'TCharacter.ShiftApproachRun';
begin
  log.DebugLog(FailName);
  try

    ApproachRun(ATarget);
    Shifted := true;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.SaveProperties(List: TStringList);
var
  S: string;
  i: Integer;
const
  FailName: string = 'TCharacter.SaveProperties';
begin
  log.DebugLog(FailName);
  try

    S := 'strength=' + IntToStr(BaseStrength);
    List.add(S);
    S := 'coordination=' + IntToStr(BaseCoordination);
    List.add(S);
    S := 'constitution=' + IntToStr(BaseConstitution);
    List.add(S);
    S := 'mysticism=' + IntToStr(BaseMysticism);
    List.add(S);
    S := 'combat=' + IntToStr(BaseCombat);
    List.add(S);
    S := 'stealth=' + IntToStr(BaseStealth);
    List.add(S);
    S := 'perception=' + IntToStr(BasePerception);
    List.add(S);
    S := 'charm=' + IntToStr(BaseCharm);
    List.add(S);
    S := 'healingrate=' + IntToStr(BaseHealingRate);
    List.add(S);
    S := 'rechargerate=' + IntToStr(BaseRechargeRate);
    List.add(S);
    S := 'hitpoints=' + FormatFP(BaseHitPoints);
    List.add(S);
    S := 'mana=' + FormatFP(BaseMana);
    List.add(S);
    S := 'wounds=' + FormatFP(FWounds);
    List.add(S);
    S := 'drain=' + FormatFP(FDrain);
    List.add(S);
    S := 'AttackRecovery=' + IntToStr(BaseAttackRecovery);
    List.add(S);
    S := 'HitRecovery=' + IntToStr(BaseHitRecovery);
    List.add(S);
    S := 'ondie=' + OnDie;
    List.add(S);
    S := 'idleai=' + IdleAI;
    List.add(S);
    S := 'combatai=' + CombatAI;
    List.add(S);
    S := 'partyai=' + PartyAI;
    List.add(S);
    S := 'vision=' + IntToStr(FVision);
    List.add(S);
    S := 'hearing=' + IntToStr(Hearing);
    List.add(S);
    S := 'smell=' + IntToStr(Smell);
    List.add(S);
    S := 'mysticvision=' + IntToStr(MysticVision);
    List.add(S);
    S := 'charactername=' + Self.Name;
    List.add(S);
    S := 'alliance=' + Self.Alliance;
    List.add(S);
    S := 'prevalliance=' + Self.PrevAlliance;
    List.add(S);
    S := 'myfriends=' + Self.Friends;
    List.add(S);
    S := 'myenemies=' + Self.Enemies;
    List.add(S);
    S := 'frozen=' + BoolToStr(Frozen, True);
    List.add(S);
    S := 'partymember=' + BoolToStr(PartyMember, True);
    List.add(S);
    S := 'willbedisabled=' + BoolToStr(WillBeDisabled, True);
    List.add(S);
    S := 'combatmode=' + BoolToStr(CombatMode, True);
    List.add(S);
    S := 'ismerchant=' + BoolToStr(IsMerchant, True);
    List.add(S);
    S := 'buyingdiscount=' + FormatFP(BuyingDiscount);
    List.add(S);
    S := 'sellingmarkup=' + FormatFP(SellingMarkup);
    List.add(S);
    S := 'moneyamount=' + IntToStr(Money);
    List.add(S);
    S := 'looted=' + BoolToStr(Looted, True);
    List.add(S);
    S := 'titles=' + Self.TitleList;
    List.add(S);
    S := 'trainingpoints=' + IntToStr(TrainingPoints);
    List.add(S);
    S := 'attacksounds=' + AttackSound;
    List.add(S);
    S := 'deathsounds=' + DeathSound;
    List.add(S);
    S := 'painsounds=' + PainSound;
    List.add(S);
    S := 'battlecry=' + BattleCry;
    List.add(S);
    S := 'dead=' + BoolToStr(FDead, True);
    List.add(S);
    S := 'deadcount=' + IntToStr(FDeadCount);
    List.add(S);
    S := 'diecount=' + IntToStr(DieCount);
    List.add(S);
    SaveResistanceProps(List, BaseResistance);
    SaveDamageProps(List, BaseUnArmedDamage);
    if assigned(HotKey[1]) then
    begin
      S := 'hotkey1=' + HotKey[1].Name;
      List.add(S);
    end;
    if assigned(HotKey[2]) then
    begin
      S := 'hotkey2=' + HotKey[2].Name;
      List.add(S);
    end;
    if assigned(HotKey[3]) then
    begin
      S := 'hotkey3=' + HotKey[3].Name;
      List.add(S);
    end;
    if assigned(HotKey[4]) then
    begin
      S := 'hotkey4=' + HotKey[4].Name;
      List.add(S);
    end;
    if assigned(HotKey[5]) then
    begin
      S := 'hotkey5=' + HotKey[5].Name;
      List.add(S);
    end;
    if assigned(HotKey[6]) then
    begin
      S := 'hotkey6=' + HotKey[6].Name;
      List.add(S);
    end;
    if assigned(HotKey[7]) then
    begin
      S := 'hotkey7=' + HotKey[7].Name;
      List.add(S);
    end;
    if assigned(HotKey[8]) then
    begin
      S := 'hotkey8=' + HotKey[8].Name;
      List.add(S);
    end;
    if assigned(HotKey[9]) then
    begin
      S := 'hotkey9=' + HotKey[9].Name;
      List.add(S);
    end;
    if assigned(HotKey[10]) then
    begin
      S := 'hotkey10=' + HotKey[10].Name;
      List.add(S);
    end;
    if assigned(CurrentSpell) then
    begin
      S := 'currentspell=' + CurrentSpell.Name;
      List.add(S);
    end;
    if NewAIMode = aiCombat then
      S := 'AIMode=Combat'
    else if NewAIMode = aiIdle then
      S := 'AIMode=Idle'
    else if NewAIMode = aiParty then
      S := 'AIMode=Party'
    else
      S := 'AIMode=None';
    List.add(S);
    if PrevAIMode = aiCombat then
      S := 'PrevAIMode=Combat'
    else if PrevAIMode = aiIdle then
      S := 'PrevAIMode=Idle'
    else if PrevAIMode = aiParty then
      S := 'PrevAIMode=Party'
    else
      S := 'PrevAIMode=None';
    List.add(S);
    if assigned(Equipment[slLeg1]) then
    begin
      S := 'equipment.leg1=' + Equipment[slLeg1].ItemName;
      List.add(S);
    end;
    if assigned(Equipment[slBoot]) then
    begin
      S := 'equipment.boot=' + Equipment[slBoot].ItemName;
      List.add(S);
    end;
    if assigned(Equipment[slLeg2]) then
    begin
      S := 'equipment.leg2=' + Equipment[slLeg2].ItemName;
      List.add(S);
    end;
    if assigned(Equipment[slChest1]) then
    begin
      S := 'equipment.chest1=' + Equipment[slChest1].ItemName;
      List.add(S);
    end;
    if assigned(Equipment[slChest2]) then
    begin
      S := 'equipment.chest2=' + Equipment[slChest2].ItemName;
      List.add(S);
    end;
    if assigned(Equipment[slArm]) then
    begin
      S := 'equipment.arm=' + Equipment[slArm].ItemName;
      List.add(S);
    end;
    if assigned(Equipment[slBelt]) then
    begin
      S := 'equipment.belt=' + Equipment[slBelt].ItemName;
      List.add(S);
    end;
    if assigned(Equipment[slChest3]) then
    begin
      S := 'equipment.chest3=' + Equipment[slChest3].ItemName;
      List.add(S);
    end;
    if assigned(Equipment[slGauntlet]) then
    begin
      S := 'equipment.gauntlet=' + Equipment[slGauntlet].ItemName;
      List.add(S);
    end;
    if assigned(Equipment[slOuter]) then
    begin
      S := 'equipment.outer=' + Equipment[slOuter].ItemName;
      List.add(S);
    end;
    if assigned(Equipment[slHelmet]) then
    begin
      S := 'equipment.helmet=' + Equipment[slHelmet].ItemName;
      List.add(S);
    end;
    if assigned(Equipment[slWeapon]) then
    begin
      S := 'equipment.weapon=' + Equipment[slWeapon].ItemName;
      List.add(S);
    end;
    if assigned(Equipment[slShield]) then
    begin
      S := 'equipment.shield=' + Equipment[slShield].ItemName;
      List.add(S);
    end;
    if assigned(Equipment[sltabar]) then
    begin
      S := 'equipment.tabar=' + Equipment[sltabar].ItemName;
      List.add(S);
    end;
    if assigned(Equipment[slMisc1]) then
    begin
      S := 'equipment.misc1=' + Equipment[slMisc1].ItemName;
      List.add(S);
    end;
    if assigned(Equipment[slMisc2]) then
    begin
      S := 'equipment.misc2=' + Equipment[slMisc2].ItemName;
      List.add(S);
    end;
    if assigned(Equipment[slMisc3]) then
    begin
      S := 'equipment.misc3=' + Equipment[slMisc3].ItemName;
      List.add(S);
    end;
    S := '';
    if EquipmentLocked[slLeg1] then
      S := S + 'leg1,';
    if EquipmentLocked[slBoot] then
      S := S + 'boot,';
    if EquipmentLocked[slLeg2] then
      S := S + 'leg2,';
    if EquipmentLocked[slChest1] then
      S := S + 'chest1,';
    if EquipmentLocked[slChest2] then
      S := S + 'chest2,';
    if EquipmentLocked[slArm] then
      S := S + 'arm,';
    if EquipmentLocked[slBelt] then
      S := S + 'belt,';
    if EquipmentLocked[slChest3] then
      S := S + 'chest3,';
    if EquipmentLocked[slGauntlet] then
      S := S + 'gauntlet,';
    if EquipmentLocked[slOuter] then
      S := S + 'outer,';
    if EquipmentLocked[slHelmet] then
      S := S + 'helmet,';
    if EquipmentLocked[slWeapon] then
      S := S + 'weapon,';
    if EquipmentLocked[slShield] then
      S := S + 'shield,';
    if EquipmentLocked[sltabar] then
      S := S + 'tabar,';
    if EquipmentLocked[slMisc1] then
      S := S + 'misc1,';
    if EquipmentLocked[slMisc2] then
      S := S + 'misc2,';
    if EquipmentLocked[slMisc3] then
      S := S + 'misc3,';
    if S <> '' then
    begin
      S := 'lockedequipment=' + copy(S, 1, length(S) - 1);
      List.add(S);
    end;
    if Inventory.Count = 0 then
    begin
      S := 'inventory.listofitems='
    end
    else
    begin
      S := 'inventory.listofitems=' + TItem(Inventory.items[0]).ItemName;
      for i := 1 to Inventory.Count - 1 do
        S := S + ',' + TItem(Inventory.items[i]).ItemName;
    end;
    List.add(S);

    inherited;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TCharacter.GetProperty(const Name: string): string;
var
  S: string;
  i: Integer;
const
  FailName: string = 'TCharacter.GetProperty';
begin
  log.DebugLog(FailName);
  try

    S := LowerCase(Name);
    if S = 'strength' then
      result := IntToStr(Strength)
    else if S = 'coordination' then
      result := IntToStr(Coordination)
    else if S = 'constitution' then
      result := IntToStr(Constitution)
    else if S = 'mysticism' then
      result := IntToStr(Mysticism)
    else if S = 'combat' then
      result := IntToStr(Combat)
    else if S = 'stealth' then
      result := IntToStr(Stealth)
    else if S = 'perception' then
      result := IntToStr(Perception)
    else if S = 'charm' then
      result := IntToStr(Charm)
    else if S = 'healingrate' then
      result := IntToStr(HealingRate)
    else if S = 'rechargerate' then
      result := IntToStr(RechargeRate)
    else if S = 'hitpoints' then
      result := FormatFP(HitPoints)
    else if S = 'mana' then
      result := FormatFP(Mana)
    else if S = 'wounds' then
      result := FormatFP(FWounds)
    else if S = 'drain' then
      result := FormatFP(FDrain)
    else if S = 'attackrecovery' then
      result := IntToStr(AttackRecovery)
    else if S = 'hitrecovery' then
      result := IntToStr(HitRecovery)
    else if S = 'ondie' then
      result := OnDie
    else if S = 'idleai' then
      result := IdleAI
    else if S = 'combatai' then
      result := CombatAI
    else if S = 'partyai' then
      result := PartyAI
    else if S = 'vision' then
      result := IntToStr(FVision)
    else if S = 'hearing' then
      result := IntToStr(Hearing)
    else if S = 'smell' then
      result := IntToStr(Smell)
    else if S = 'mysticvision' then
      result := IntToStr(MysticVision)
    else if S = 'charactername' then
      result := Self.Name
    else if S = 'alliance' then
      result := Self.Alliance
    else if S = 'prevalliance' then
      result := Self.PrevAlliance
    else if S = 'myfriends' then
      result := Self.Friends
    else if S = 'myenemies' then
      result := Self.Enemies
    else if S = 'frozen' then
      result := BoolToStr(Frozen, True)
    else if S = 'partymember' then
      result := BoolToStr(PartyMember, True)
    else if S = 'willbedisabled' then
      result := BoolToStr(WillBeDisabled, True)
    else if S = 'combatmode' then
      result := BoolToStr(CombatMode, True)
    else if S = 'ismerchant' then
      result := BoolToStr(IsMerchant, True)
    else if S = 'buyingdiscount' then
      result := FormatFP(BuyingDiscount)
    else if S = 'sellingmarkup' then
      result := FormatFP(SellingMarkup)
    else if S = 'moneyamount' then
      result := IntToStr(Money)
    else if S = 'looted' then
      result := BoolToStr(Looted, True)
    else if S = 'titles' then
      result := Self.TitleList
    else if S = 'trainingpoints' then
      result := IntToStr(FTrainingPoints)
    else if S = 'dead' then
      result := BoolToStr(FDead, True)
    else if S = 'attacksounds' then
      result := AttackSound
    else if S = 'deathsounds' then
      result := DeathSound
    else if S = 'painsounds' then
      result := PainSound
    else if S = 'battlecry' then
      result := BattleCry
    else if S = 'partycount' then
      result := IntToStr(NPCList.Count - 1)
    else if S = 'currentspell' then
      result := CurrentSpell.Name
    else if GetResistanceProps(S, result, BaseResistance) then
    else if GetDamageProps(S, result, BaseUnArmedDamage) then
    else if S = 'aimode' then
    begin
      if AIMode = aiCombat then
        result := 'Combat'
      else if AIMode = aiIdle then
        result := 'Idle'
      else if AIMode = aiParty then
        result := 'Party'
      else
        result := '';
    end
    else if S = 'prevaimode' then
    begin
      if PrevAIMode = aiCombat then
        result := 'Combat'
      else if PrevAIMode = aiIdle then
        result := 'Idle'
      else if PrevAIMode = aiParty then
        result := 'Party'
      else
        result := '';
    end
    else if S = 'lockedequipment' then
    begin
      result := '';
      if EquipmentLocked[slLeg1] then
        result := result + 'leg1,';
      if EquipmentLocked[slBoot] then
        result := result + 'boot,';
      if EquipmentLocked[slLeg2] then
        result := result + 'leg2,';
      if EquipmentLocked[slChest1] then
        result := result + 'chest1,';
      if EquipmentLocked[slChest2] then
        result := result + 'chest2,';
      if EquipmentLocked[slArm] then
        result := result + 'arm,';
      if EquipmentLocked[slBelt] then
        result := result + 'belt,';
      if EquipmentLocked[slChest3] then
        result := result + 'chest3,';
      if EquipmentLocked[slGauntlet] then
        result := result + 'gauntlet,';
      if EquipmentLocked[slOuter] then
        result := result + 'outer,';
      if EquipmentLocked[slHelmet] then
        result := result + 'helmet,';
      if EquipmentLocked[slWeapon] then
        result := result + 'weapon,';
      if EquipmentLocked[slShield] then
        result := result + 'shield,';
      if EquipmentLocked[sltabar] then
        result := result + 'tabar,';
      if EquipmentLocked[slMisc1] then
        result := result + 'misc1,';
      if EquipmentLocked[slMisc2] then
        result := result + 'misc2,';
      if EquipmentLocked[slMisc3] then
        result := result + 'misc3,';
      if result <> '' then
        result := copy(result, 1, length(result) - 1);
    end
    else if S = 'equipment.leg1' then
      result := Equipment[slLeg1].ItemName
    else if S = 'equipment.boot' then
      result := Equipment[slBoot].ItemName
    else if S = 'equipment.leg2' then
      result := Equipment[slLeg2].ItemName
    else if S = 'equipment.chest1' then
      result := Equipment[slChest1].ItemName
    else if S = 'equipment.chest2' then
      result := Equipment[slChest2].ItemName
    else if S = 'equipment.arm' then
      result := Equipment[slArm].ItemName
    else if S = 'equipment.belt' then
      result := Equipment[slBelt].ItemName
    else if S = 'equipment.chest3' then
      result := Equipment[slChest3].ItemName
    else if S = 'equipment.gauntlet' then
      result := Equipment[slGauntlet].ItemName
    else if S = 'equipment.outer' then
      result := Equipment[slOuter].ItemName
    else if S = 'equipment.helmet' then
      result := Equipment[slHelmet].ItemName
    else if S = 'equipment.weapon' then
      result := Equipment[slWeapon].ItemName
    else if S = 'equipment.shield' then
      result := Equipment[slShield].ItemName
    else if S = 'equipment.tabar' then
      result := Equipment[sltabar].ItemName
    else if S = 'equipment.misc1' then
      result := Equipment[slMisc1].ItemName
    else if S = 'equipment.misc2' then
      result := Equipment[slMisc2].ItemName
    else if S = 'equipment.misc3' then
      result := Equipment[slMisc3].ItemName
    else if S = 'inventory.listofitems' then
    begin
      if Inventory.Count = 0 then
        result := ''
      else
      begin
        result := TItem(Inventory.items[0]).ItemName;
        for i := 1 to Inventory.Count - 1 do
          result := result + ',' + TItem(Inventory.items[i]).ItemName;
      end;
    end
    else
      result := inherited GetProperty(Name);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TCharacter.getVision: Integer;
begin
  result := Trunc(FVision * ScreenMetrics.VisibilityFactor);
end;

procedure TCharacter.SetProperty(const Name, Value: string);
var
  S: string;
  i: Integer;
  L: Integer;
  NoProp: boolean;
const
  FailName: string = 'TCharacter.SetProperty';
begin
  log.DebugLog(FailName);
  try

    NoProp := false;
    S := LowerCase(Name);
    L := length(S);
    case L of
      4:
        begin
          if S = 'mana' then
            Mana := UnFormatFP(Value)
          else if S = 'dead' then
            Dead := (LowerCase(Value) = 'true')
          else
            NoProp := true;
        end;
      5:
        begin
          if S = 'charm' then
            Charm := StrToInt(Value)
          else if S = 'drain' then
            FDrain := UnFormatFP(Value)
          else if S = 'ondie' then
          begin
            if not LoadingFromSaveFile then
              OnDie := Value;
          end
          else if S = 'smell' then
            Self.Smell := StrToInt(Value)
          else
            NoProp := true;
        end;
      6:
        begin
          if S = 'combat' then
            Combat := StrToInt(Value)
          else if S = 'wounds' then
            FWounds := UnFormatFP(Value)
          else if S = 'idleai' then
            IdleAI := Value
          else if S = 'vision' then
            Self.FVision := StrToInt(Value)
          else if S = 'frozen' then
            Self.Frozen := (LowerCase(Value) = 'true')
          else if S = 'titles' then
            Self.TitleList := Value
          else if S = 'looted' then
          begin
            Self.Looted := (LowerCase(Value) = 'true');
            if Self.Looted then
              Self.FMoney := 0;
            // Wipes out default money loaded from resource file
          end
          else if S = 'aimode' then
          begin
            S := LowerCase(Value);
            if (S = 'combat') or (S = 'combatai') then
              NewAIMode := aiCombat
            else if (S = 'idle') or (S = 'idleai') then
              NewAIMode := aiIdle
            else if (S = 'party') or (S = 'partyai') then
              NewAIMode := aiParty
            else
              NewAIMode := ainone;
          end
          else
            NoProp := true;
        end;
      7:
        begin
          if S = 'stealth' then
            Stealth := StrToInt(Value)
          else if S = 'partyai' then
            PartyAI := Value
          else if S = 'hearing' then
            Self.Hearing := StrToInt(Value)
          else if S = 'hotkey1' then
          begin
            i := AllSpellList.IndexOf(Value);
            if i < 0 then
              HotKey[1] := nil
            else
              HotKey[1] := TSpell(AllSpellList.objects[i]);
          end
          else if S = 'hotkey2' then
          begin
            i := AllSpellList.IndexOf(Value);
            if i < 0 then
              HotKey[2] := nil
            else
              HotKey[2] := TSpell(AllSpellList.objects[i]);
          end
          else if S = 'hotkey3' then
          begin
            i := AllSpellList.IndexOf(Value);
            if i < 0 then
              HotKey[3] := nil
            else
              HotKey[3] := TSpell(AllSpellList.objects[i]);
          end
          else if S = 'hotkey4' then
          begin
            i := AllSpellList.IndexOf(Value);
            if i < 0 then
              HotKey[4] := nil
            else
              HotKey[4] := TSpell(AllSpellList.objects[i]);
          end
          else if S = 'hotkey5' then
          begin
            i := AllSpellList.IndexOf(Value);
            if i < 0 then
              HotKey[5] := nil
            else
              HotKey[5] := TSpell(AllSpellList.objects[i]);
          end
          else if S = 'hotkey6' then
          begin
            i := AllSpellList.IndexOf(Value);
            if i < 0 then
              HotKey[6] := nil
            else
              HotKey[6] := TSpell(AllSpellList.objects[i]);
          end
          else if S = 'hotkey7' then
          begin
            i := AllSpellList.IndexOf(Value);
            if i < 0 then
              HotKey[7] := nil
            else
              HotKey[7] := TSpell(AllSpellList.objects[i]);
          end
          else if S = 'hotkey8' then
          begin
            i := AllSpellList.IndexOf(Value);
            if i < 0 then
              HotKey[8] := nil
            else
              HotKey[8] := TSpell(AllSpellList.objects[i]);
          end
          else if S = 'hotkey9' then
          begin
            i := AllSpellList.IndexOf(Value);
            if i < 0 then
              HotKey[9] := nil
            else
              HotKey[9] := TSpell(AllSpellList.objects[i]);
          end
          else
            NoProp := true;
        end;
      8:
        begin
          if S = 'strength' then
            Strength := StrToInt(Value)
          else if S = 'combatai' then
            CombatAI := Value
          else if S = 'alliance' then
            Self.Alliance := Value
          else if S = 'diecount' then
            DieCount := StrToInt(Value)
          else if S = 'hotkey10' then
          begin
            i := AllSpellList.IndexOf(Value);
            if i < 0 then
              HotKey[10] := nil
            else
              HotKey[10] := TSpell(AllSpellList.objects[i]);
          end
          else
            NoProp := true;
        end;
      9:
        begin
          if S = 'mysticism' then
            Mysticism := StrToInt(Value)
          else if S = 'hitpoints' then
            HitPoints := UnFormatFP(Value)
          else if S = 'myfriends' then
            Self.Friends := Value
          else if S = 'myenemies' then
            Self.Enemies := Value
          else if S = 'battlecry' then
            BattleCry := Value
          else if S = 'deadcount' then
            FDeadCount := StrToInt(Value)
          else
            NoProp := true;
        end;
      10:
        begin
          if S = 'perception' then
            Perception := StrToInt(Value)
          else if S = 'combatmode' then
            Self.CombatMode := (LowerCase(Value) = 'true')
          else if S = 'ismerchant' then
            Self.IsMerchant := (LowerCase(Value) = 'true')
          else if S = 'painsounds' then
            PainSound := Value
          else if S = 'prevaimode' then
          begin
            S := LowerCase(Value);
            if (S = 'combat') or (S = 'combatai') then
              PrevAIMode := aiCombat
            else if (S = 'idle') or (S = 'idleai') then
              PrevAIMode := aiIdle
            else if (S = 'party') or (S = 'partyai') then
              PrevAIMode := aiParty
            else
              PrevAIMode := ainone;
          end
          else
            NoProp := true;
        end;
      11:
        begin
          if S = 'healingrate' then
            HealingRate := StrToInt(Value)
          else if S = 'moneyamount' then
            Self.Money := StrToInt(Value)
          else if S = 'deathsounds' then
            DeathSound := Value
          else if S = 'hitrecovery' then
            HitRecovery := StrToInt(Value)
          else if S = 'partymember' then
            Self.PartyMember := (LowerCase(Value) = 'true')
          else
            NoProp := true;
        end;
      12:
        begin
          if S = 'coordination' then
            Coordination := StrToInt(Value)
          else if S = 'constitution' then
            Constitution := StrToInt(Value)
          else if S = 'rechargerate' then
            RechargeRate := StrToInt(Value)
          else if S = 'attacksounds' then
            AttackSound := Value
          else if S = 'prevalliance' then
            Self.PrevAlliance := Value
          else if S = 'mysticvision' then
            Self.MysticVision := StrToInt(Value)
          else if S = 'intendtozone' then
            Self.IntendToZone := (LowerCase(Value) = 'true')
          else if S = 'currentspell' then
          begin
            i := AllSpellList.IndexOf(Value);
            if i < 0 then
              CurrentSpell := nil
            else
              CurrentSpell := TSpell(AllSpellList.objects[i]);

          end
          else
            NoProp := true;
        end;
      13:
        begin
          if S = 'charactername' then
          begin
            // Fix missing character name symbol replacements
            if (length(Value) >= 13) and
              (LowerCase(copy(Value, 1, 13)) = 'charactername') then
            begin
              log.log('** Attempting to fix bad name: ' + Value);
              Self.Name := FixCharacterName(Value);
              log.log('** Result: ' + Self.Name);
            end
            else
              Self.Name := Value;
          end
          else if S = 'sellingmarkup' then
            Self.SellingMarkup := UnFormatFP(Value)
          else if S = 'equipment.arm' then
          begin
            FEquipmentNames[slArm] := Value
          end
          else
            NoProp := true;
        end;
      14:
        begin
          if S = 'attackrecovery' then
            AttackRecovery := StrToInt(Value)
          else if S = 'buyingdiscount' then
            Self.BuyingDiscount := UnFormatFP(Value)
          else if S = 'trainingpoints' then
            Self.TrainingPoints := StrToInt(Value)
          else if S = 'equipment.leg1' then
          begin
            FEquipmentNames[slLeg1] := Value
          end
          else if S = 'equipment.boot' then
          begin
            FEquipmentNames[slBoot] := Value
          end
          else if S = 'equipment.leg2' then
          begin
            FEquipmentNames[slLeg2] := Value
          end
          else if S = 'equipment.belt' then
          begin
            FEquipmentNames[slBelt] := Value
          end
          else if S = 'willbedisabled' then
          begin
            Self.WillBeDisabled := (LowerCase(Value) = 'true')
          end
          else
            NoProp := true;
        end;
      15:
        begin
          if S = 'equipment.outer' then
          begin
            FEquipmentNames[slOuter] := Value
          end
          else if S = 'equipment.tabar' then
          begin
            FEquipmentNames[sltabar] := Value
          end
          else if S = 'equipment.misc1' then
          begin
            FEquipmentNames[slMisc1] := Value
          end
          else if S = 'equipment.misc2' then
          begin
            FEquipmentNames[slMisc2] := Value
          end
          else if S = 'equipment.misc3' then
          begin
            FEquipmentNames[slMisc3] := Value
          end
          else if S = 'lockedequipment' then
          begin
            // for j := slLeg1 to slMisc3 do
            // EquipmentLocked[ j ] := false;
            S := LowerCase(Value);
            EquipmentLocked[slLeg1] := S.Contains('leg1');
            EquipmentLocked[slBoot] := S.Contains('boot');
            EquipmentLocked[slLeg2] := S.Contains('leg2');
            EquipmentLocked[slChest1] := S.Contains('chest1');
            EquipmentLocked[slChest2] := S.Contains('chest2');
            EquipmentLocked[slArm] := S.Contains('arm');
            EquipmentLocked[slBelt] := S.Contains('belt');
            EquipmentLocked[slChest3] := S.Contains('chest3');
            EquipmentLocked[slGauntlet] := S.Contains('gauntlet');
            EquipmentLocked[slOuter] := S.Contains('outer');
            EquipmentLocked[slHelmet] := S.Contains('helmet');
            EquipmentLocked[slWeapon] := S.Contains('weapon');
            EquipmentLocked[slShield] := S.Contains('shield');
            EquipmentLocked[sltabar] := S.Contains('tabar');
            EquipmentLocked[slMisc1] := S.Contains('misc1');
            EquipmentLocked[slMisc2] := S.Contains('misc2');
            EquipmentLocked[slMisc3] := S.Contains('misc3');
          end
          else
            NoProp := true;
        end;
      16:
        begin
          if S = 'equipment.chest1' then
          begin
            FEquipmentNames[slChest1] := Value
          end
          else if S = 'equipment.chest2' then
          begin
            FEquipmentNames[slChest2] := Value
          end
          else if S = 'equipment.chest3' then
          begin
            FEquipmentNames[slChest3] := Value
          end
          else if S = 'equipment.helmet' then
          begin
            FEquipmentNames[slHelmet] := Value
          end
          else if S = 'equipment.weapon' then
          begin
            FEquipmentNames[slWeapon] := Value
          end
          else if S = 'equipment.shield' then
          begin
            FEquipmentNames[slShield] := Value
          end
          else
            NoProp := true;
        end;
      18:
        begin
          if S = 'equipment.gauntlet' then
          begin
            FEquipmentNames[slGauntlet] := Value
          end
          else
            NoProp := true;
        end;
      21:
        begin
          if S = 'inventory.listofitems' then
          begin
            FInventoryList := Value;
          end
          else
            NoProp := true;
        end
    else
      NoProp := true;
    end;

    if NoProp then
    begin
      if SetResistanceProps(S, Value, BaseResistance) then
      else if SetDamageProps(S, Value, BaseUnArmedDamage) then
      else
        inherited;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.LoadEquipment(UseDefaults: boolean);
var
  i: TSlot;
  i1, j: Integer;
  S: string;
  NewItem: TItem;
const
  FailName: string = 'TCharacter.LoadEquipment';
begin
  log.DebugLog(FailName);
  try

    for i := slLeg1 to slMisc3 do
    begin
      if FEquipmentNames[i] <> '' then
      begin
        if assigned(Resource) then
          Equipment[i] := PartManager.LoadItem(FEquipmentNames[i],
            TCharacterResource(Resource).NakedName);
        FEquipmentNames[i] := '';
      end;

      if assigned(Equipment[i]) then
      begin
        // Load Resource
        if not assigned(Equipment[i].Resource) then
        begin
          Equipment[i].Resource := PartManager.GetLayerResource
            (Equipment[i].LayeredImage);
          if FigureInstances.IndexOfObject(Equipment[i]) < 0 then
          begin
            Equipment[i].GUID := '';
            j := FigureInstances.add(Equipment[i].GUID);
            FigureInstances.objects[j] := Equipment[i];
          end;
        end;
      end
      else if UseDefaults and (TCharacterResource(Resource).Equipment[i] <> '')
      then
      begin
        if assigned(Resource) then
        begin
          S := TCharacterResource(Resource).Equipment[i];
          Equipment[i] := PartManager.LoadItem(S, TCharacterResource(Resource)
            .NakedName);
        end;

        // Load Resource
        if assigned(Equipment[i]) then
        begin
          Equipment[i].Resource := PartManager.GetLayerResource
            (Equipment[i].LayeredImage);
          Equipment[i].GUID := '';
          j := FigureInstances.add(Equipment[i].GUID);
          FigureInstances.objects[j] := Equipment[i];
        end;
      end;
    end;

    if FInventoryList <> '' then
    begin
      if assigned(Resource) then
      begin
        i1 := 0;
        S := Parse(FInventoryList, i1, ',');
        while S <> '' do
        begin
          NewItem := PartManager.LoadItem(S, TCharacterResource(Resource)
            .NakedName);
          if assigned(NewItem) then
          begin
            if NoItemPlacement or FindFreeInventoryXY(NewItem) then
            begin
              Inventory.add(NewItem);
              NewItem.Enabled := false;
              NewItem.Resource := PartManager.GetLayerResource
                (NewItem.LayeredImage);
              NewItem.GUID := '';
              j := FigureInstances.add(NewItem.GUID);
              FigureInstances.objects[j] := NewItem;
            end
            else
            begin
              NewItem.free;
              log.log('*** Error: Item (' + S +
                ') could not be added to inventory');
            end;
          end
          else
          begin
            log.log('*** Error: Item (' + S + ') could not be loaded');
          end;
          Inc(i1);
          S := Parse(FInventoryList, i1, ',');
        end;
      end;
      FInventoryList := '';
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TCharacter.DoAction(const Action: string): boolean;
var
  S: string;
  Script: TScript;
  Multiplier, MultiplierFrame: Word;
  Variation: Integer;
const
  FailName: string = 'TCharacter.DoAction';
begin
  result := false;

  log.DebugLog(FailName);
  try

    S := LowerCase(Action);
    if S = 'attack' then
    begin
      if assigned(Resource) and (Resource is TCharacterResource) and
        (TCharacterResource(Resource).AttackVariations > 0) then
        Variation := random(TCharacterResource(Resource).AttackVariations) + 1
      else
        Variation := 1;
      S := Action + IntToStr(Variation) + Facing.ToString;
      Script := Resource.Script[S];
      if assigned(Script) then
      begin
        if Script.Frames > 0 then
        begin
          Multiplier := (FAttackRecovery div Script.Frames) + 2;
          MultiplierFrame := (FAttackRecovery mod Script.Frames) + 1;
          Delay := -1;
          PlayScript(S, 1, smOnce, Multiplier, MultiplierFrame, -1);
          result := true;
        end;
      end
      else
      begin
        S := Action + IntToStr(Variation);
        Script := Resource.Script[S];
        if assigned(Script) then
        begin
          if Script.Frames > 0 then
          begin
            Multiplier := (FAttackRecovery div Script.Frames) + 2;
            MultiplierFrame := (FAttackRecovery mod Script.Frames) + 1;
            Delay := -1;
            PlayScript(S, 1, smOnce, Multiplier, MultiplierFrame, -1);
            result := true;
          end;
        end;
      end;
    end
    else if S = 'death' then
    begin
      Die;
    end
    else
      result := inherited DoAction(Action);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.SetCharm(const Value: Integer);
begin
  if Value <> BaseCharm then
  begin
    BaseCharm := Value;
    CalcStats;
  end;
end;

procedure TCharacter.SetCombat(const Value: Integer);
begin
  if Value <> BaseCombat then
  begin
    BaseCombat := Value;
    CalcStats;
  end;
end;

procedure TCharacter.SetConstitution(const Value: Integer);
begin
  if Value <> BaseConstitution then
  begin
    BaseConstitution := Value;
    CalcStats;
  end;
end;

procedure TCharacter.SetCoordination(const Value: Integer);
begin
  if Value <> BaseCoordination then
  begin
    BaseCoordination := Value;
    CalcStats;
  end;
end;

procedure TCharacter.SetHealingRate(const Value: Integer);
begin
  if Value <> BaseHealingRate then
  begin
    BaseHealingRate := Value;
    CalcStats;
  end;
end;

procedure TCharacter.SetHitPoints(const Value: Single);
begin
  if Value <> BaseHitPoints then
  begin
    BaseHitPoints := Value;
    CalcStats;
  end;
end;

procedure TCharacter.SetMana(const Value: Single);
begin
  if Value <> BaseMana then
  begin
    BaseMana := Value;
    CalcStats;
  end;
end;

procedure TCharacter.SetMysticism(const Value: Integer);
begin
  if Value <> BaseMysticism then
  begin
    BaseMysticism := Value;
    CalcStats;
  end;
end;

procedure TCharacter.SetPerception(const Value: Integer);
begin
  if Value <> BasePerception then
  begin
    BasePerception := Value;
    CalcStats;
  end;
end;

procedure TCharacter.SetRechargeRate(const Value: Integer);
begin
  if Value <> BaseRechargeRate then
  begin
    BaseRechargeRate := Value;
    CalcStats;
  end;
end;

procedure TCharacter.SetStealth(const Value: Integer);
begin
  if Value <> BaseStealth then
  begin
    BaseStealth := Value;
    CalcStats;
  end;
end;

procedure TCharacter.SetStrength(const Value: Integer);
begin
  if Value <> BaseStrength then
  begin
    BaseStrength := Value;
    CalcStats;
  end;
end;

procedure TCharacter.SetAttackRecovery(const Value: Integer);
begin
  if Value <> BaseAttackRecovery then
  begin
    BaseAttackRecovery := Value;
    CalcStats;
  end;
end;

procedure TCharacter.SetHitRecovery(const Value: Integer);
begin
  if Value <> BaseHitRecovery then
  begin
    BaseHitRecovery := Value;
    CalcStats;
  end;
end;

procedure TCharacter.SetAttackBonus(const Value: Single);
begin
  if Value <> BaseAttackBonus then
  begin
    BaseAttackBonus := Value;
    CalcStats;
  end;
end;

procedure TCharacter.SetDefense(const Value: Single);
begin
  if Value <> BaseDefense then
  begin
    BaseDefense := Value;
    CalcStats;
  end;
end;

procedure TCharacter.SetMovement(const Value: Single);
begin
  if Value <> BaseMovement then
  begin
    BaseMovement := Value;
    CalcStats;
  end;
end;

procedure TCharacter.Render;
const
  Width = 32;
  Height = 6;
var
  X, Y: longint;
  BltFx: TDDBLTFX;
  Ratio: Single;
  BarWidth: Integer;
  pr, pr0: TRect;
begin
  inherited;
  if Highlighted and not FDead and assigned(AI) and Current.IsEnemy(Self) then
  begin
    X := View.Left + PosX + (Self.Width - Width) div 2;
    Y := View.Top + PosY - Height;
    BltFx.dwSize := sizeof(BltFx);
    BltFx.dwFillColor := 0;
    pr := Rect(X, Y, X + Width, Y + Height);
    pr0 := Rect(0, 0, 0, 0);
    lpDDSBack.Blt(@pr, nil, @pr0, DDBLT_COLORFILL + DDBLT_WAIT, @BltFx);
    if (BaseHitPoints > 0) and (HitPoints > 0) then
    begin
      Ratio := (HitPoints - Wounds) / HitPoints;
      if Ratio > 1 then
        Ratio := 1;
    end
    else
      Ratio := 1;
    BarWidth := round((Width - 1) * Ratio);
    if PixelFormat = pf555 then
      BltFx.dwFillColor := Trunc($18 * (1 - Ratio)) shl 10 +
        Trunc($F * Ratio) shl 5
    else
      BltFx.dwFillColor := Trunc($18 * (1 - Ratio)) shl 11 +
        Trunc($1F * Ratio) shl 5;
    pr := Rect(X + 1, Y + 1, X + BarWidth, Y + Height - 1);
    pr0 := Rect(0, 0, 0, 0);
    lpDDSBack.Blt(@pr, nil, @pr0, DDBLT_COLORFILL + DDBLT_WAIT, @BltFx);
  end;
end;

procedure TCharacter.SetAI(const Value: TAI);
const
  FailName : string = 'TCharacter.SetAI';
begin
  try

    if Assigned( FAI ) then
      FAI.Free;
    FAI := Value;
    if Assigned( FAI ) then
    begin
      FAI.Character := Self;
      if not Loading then
        FAI.Init;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TCharacter.SetAIMode(const Value: TAIMode);
begin
  NewAIMode := Value;
end;

procedure TCharacter.Cast(Target: TSpriteObject);
const
  FailName: string = 'TCharacter.Cast';
begin
  log.DebugLog(FailName);
  try

    if FFrozen then
      exit;
    FAttacking := false;
    AutoFight := false;

    if FReady then
    begin
      inherited Stop;
      NextAction := naNone;
      Face(Target.X, Target.Y);
      if not assigned(CurrentSpell) then
      begin
        Stand;
        exit;
      end;
      ManaDrain := CurrentSpell.Drain(Self);
      if Drain + ManaDrain > Mana then
      begin
        Stand;
        exit; // Not enough mana
      end;
      if DoAction('Cast') then
      begin
        if not CurrentSpell.SoundInCast then
          CurrentSpell.PlaySound(X, Y);
        CurrentSpell.Casting(Self);
        FTarget := Target;
        FTargetX := Target.X;
        FTargetY := Target.Y;
        FCasting := true;
        FReady := false;
        InitStand := false;
      end
      else
      begin
        Stand;
      end;
    end
    else
    begin
      NextAction := naCast;
      NextTarget := Target;
      NextTargetX := Target.X;
      NextTargetY := Target.Y
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.CastPoint(X, Y: Integer);
const
  FailName: string = 'TCharacter.CastPoint';
begin
  log.DebugLog(FailName);
  try

    if FFrozen then
      exit;
    FAttacking := false;
    AutoFight := false;
    if FReady then
    begin
      inherited Stop;
      NextAction := naNone;
      Face(X, Y + Height div 2);
      FTarget := nil;
      if not assigned(CurrentSpell) then
      begin
        Stand;
        exit;
      end;
      ManaDrain := CurrentSpell.Drain(Self);
      if Drain + ManaDrain > Mana then
      begin
        Stand;
        exit; // Not enough mana
      end;
      if DoAction('Cast') then
      begin
        if not CurrentSpell.SoundInCast then
          CurrentSpell.PlaySound(Self.X, Self.Y);
        CurrentSpell.Casting(Self);
        FTargetX := X;
        FTargetY := Y;
        FCasting := true;
        FReady := false;
      end
      else
      begin
        Stand;
      end;
    end
    else
    begin
      NextAction := naCast;
      NextTarget := nil;
      NextTargetX := X;
      NextTargetY := Y;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.SetTrack(Character: TCharacter);
const
  FailName: string = 'TCharacter.SetTrack';
begin
  log.DebugLog(FailName);
  try

    FTrack := Character;
    if assigned(FAI) then
      AI.TrackChanged;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.SetTrainingPoints(const Value: Integer);
const
  FailName: string = 'TCharacter.SetTrainingPoints';
begin
  log.DebugLog(FailName);
  try

    Inc(FTrainingPoints, Value);
    if FTrainingPoints < 0 then
      FTrainingPoints := 0;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.SetVision(v: Integer);
begin
  FVision := v;
end;

procedure TCharacter.SetMoney(const Value: Integer);
const
  FailName: string = 'TCharacter.SetMoney';
begin
  log.DebugLog(FailName);
  try

    if PartyMember and assigned(Player) then
    begin
      Inc(Player.FMoney, Value);
      if Player.FMoney < 0 then
        Player.FMoney := 0;
    end
    else
    begin
      Inc(FMoney, Value);
      if FMoney < 0 then
        FMoney := 0;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.DoBattleCry;
const
  FailName: string = 'TCharacter.DoBattleCry';
begin
  log.DebugLog(FailName);
  try

    PlaySound(BattleCries, X, Y);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TCharacter.GetWillBeDisabled: boolean;
var
  i: Integer;
begin
  result := FWillBeDisabled;

  if not result then
  begin
    for i := 0 to Effects.Count - 1 do
    begin
      if Effects[i].DisableWhenDone then
      begin
        result := true;
        break;
      end;
    end;
    FWillBeDisabled := result;
  end;
end;

function TCharacter.ShouldSave: boolean;
begin
  result := inherited ShouldSave;

  if result then
  begin
    result := not Spawned;
  end;

  if result then
  begin
    if PartyMember then
    begin
      if CreatedFromLvlFile then
      begin
        result := true;
      end
      else
      begin
        result := false;
      end;
    end;
  end;

  // if result then result:=(self<>Player);   //Causes partymembers to be loaded twice - duplicating items in their inventory
  // if result then result:=not PartyMember; //Causes duplication of party members on originating map
end;

function TCharacter.InRange(Target: TAniFigure): boolean;
var
  D: Double;
const
  FailName: string = 'TCharacter.InRange';
begin
  result := false;;
  log.DebugLog(FailName);
  try

    D := sqrt(sqr(Target.X - X) + sqr(2 * (Target.Y - Y)));
    result := (D <= Target.Radius + Radius + Range);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TCharacter.RangeTo(X, Y: longint): Double;
var
  A, B: Double;
const
  FailName: string = 'TCharacter.RangeTo';
begin
  result := 0;
  log.DebugLog(FailName);
  try

    A := X - Self.X;
    B := 2 * (Y - Self.Y);
    result := sqrt(sqr(A) + sqr(B));

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.AddEffect(Effect: TEffect);
var
  i: Integer;
  Found: boolean;
const
  FailName: string = 'TCharacter.AddEffect';
begin
  log.DebugLog(FailName);
  try

    Found := false;
    if Effect.tag <> 0 then
    begin
      for i := 0 to Effects.Count - 1 do
      begin
        if Effects[i].tag = Effect.tag then
        begin
          if Effect.tag < 0 then
          begin
            Effects[i].Refresh(Effect);
            Effect.free;
          end
          else
          begin
            Effects[i].free;
            Effects[i] := Effect;
          end;
          Found := true;
          break;
        end;
      end;
    end;
    if not Found then
      Effects.add(Effect);
    CalcStats;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.AddTitle(const Title: string);
var
  Modifier: PStatModifier;
  i: Integer;
const
  FailName: string = 'TCharacter.AddTitle';
begin
  log.DebugLog(FailName);
  try

    if (Titles.IndexOf(Title) < 0) then
    begin
      Modifier := TitlesManager.GetStatModifier(Title);
      i := Titles.add(Title);
      Titles.objects[i] := TObject(Modifier);
      CalcStats;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.RemoveTitle(const Title: string);
var
  i: Integer;
const
  FailName: string = 'TCharacter.RemoveTitle';
begin
  log.DebugLog(FailName);
  try

    if Title = '' then
      exit;
    i := Titles.IndexOf(Title);

    if i >= 0 then
    begin
      if assigned(Titles.objects[i]) then
        Dispose(PStatModifier(Titles.objects[i]));
      Titles.Delete(i);
    end;
    CalcStats;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TCharacter.TitleExists(const Title: string): boolean;
var
  i: TSlot;
const
  FailName: string = 'TCharacter.TitleExists';
begin
  result := false;

  log.DebugLog(FailName);
  try

    result := (Titles.IndexOf(LowerCase(Title)) >= 0);

    if not result then
    begin
      for i := slLeg1 to slMisc3 do
      begin
        if assigned(Equipment[i]) then
        begin
          if AnsiSameText(Equipment[i].Title, Title) then
          begin
            result := true;
            break;
          end;
        end;
      end;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.SetTitleList(const Value: string);
var
  S: string;
  i: Integer;
const
  FailName: string = 'TCharacter.SetTitleList';
begin
  log.DebugLog(FailName);
  try

    for i := 0 to Titles.Count - 1 do
    begin
      if assigned(Titles.objects[i]) then
        Dispose(PStatModifier(Titles.objects[i]));
    end;
    Titles.Clear;

    i := 0;
    S := Parse(Value, i, ',');
    while S <> '' do
    begin
      AddTitle(S);
      Inc(i);
      S := Parse(Value, i, ',');
    end;
    CalcStats;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TCharacter.GetTitleList: string;
var
  i: Integer;
const
  FailName: string = 'TCharacter.GetTitleList';
begin
  log.DebugLog(FailName);
  try

    if Titles.Count = 0 then
      result := ''
    else
    begin
      result := Titles.strings[0];
      for i := 1 to Titles.Count - 1 do
        result := result + ',' + Titles.strings[i];
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TCharacter.InInventory(const ItemName: string): boolean;
var
  i: Integer;
  j: TSlot;
  S: string;
const
  FailName: string = 'TCharacter.InInventory';
begin
  result := false;

  log.DebugLog(FailName);
  try

    S := LowerCase(ItemName);
    for i := 0 to Inventory.Count - 1 do
    begin
      if LowerCase(TItem(Inventory.items[i]).ItemName) = S then
      begin
        result := true;
        exit;
      end;
    end;

    for j := slLeg1 to slMisc3 do
    begin
      if assigned(Equipment[j]) then
      begin
        if LowerCase(Equipment[j].ItemName) = S then
        begin
          result := true;
          exit;
        end;
      end;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TCharacter.IsWorn(const ItemName: string): boolean;
var
  j: TSlot;
  S: string;
const
  FailName: string = 'TCharacter.InInventory';
begin
  result := false;

  log.DebugLog(FailName);
  try

    S := LowerCase(ItemName);
    for j := slLeg1 to slMisc3 do
    begin
      if assigned(Equipment[j]) then
      begin
        if LowerCase(Equipment[j].ItemName) = S then
        begin
          result := true;
          exit;
        end;
      end;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TCharacter.IsAlly(Target: TCharacter): boolean;
const
  FailName: string = 'TCharacter.IsAlly';
begin
  result := false;

  log.DebugLog(FailName);
  try

    if not assigned(Target) then
      exit;
    if assigned(UseAllegianceOf) then
    begin
      if Target.Alliance = '' then
        result := false
      else
        result := UseAllegianceOf.FFriends.IndexOf(Target.Alliance) >= 0;

      if not result and (Alliance <> '') then
        result := Target.FFriends.IndexOf(UseAllegianceOf.Alliance) >= 0
    end
    else
    begin
      if Target.Alliance = '' then
        result := false
      else
        result := FFriends.IndexOf(Target.Alliance) >= 0;

      if not result and (Alliance <> '') then
        result := Target.FFriends.IndexOf(Alliance) >= 0;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TCharacter.IsEnemy(Target: TCharacter): boolean;
const
  FailName: string = 'TCharacter.IsEnemy';
begin
  result := false;

  log.DebugLog(FailName);
  try

    if not assigned(Target) then
      exit;
    if assigned(UseAllegianceOf) then
    begin
      if Target.Alliance = '' then
        result := false
      else
        result := UseAllegianceOf.FEnemies.IndexOf(Target.Alliance) >= 0;

      if not result and (Alliance <> '') then
        result := Target.FEnemies.IndexOf(UseAllegianceOf.Alliance) >= 0;
    end
    else
    begin
      if Target.Alliance = '' then
        result := false
      else
        result := FEnemies.IndexOf(Target.Alliance) >= 0;

      if not result and (Alliance <> '') then
        result := Target.FEnemies.IndexOf(Alliance) >= 0;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TCharacter.IsNeutral(Target: TCharacter): boolean;
const
  FailName: string = 'TCharacter.IsNeutral';
begin
  result := false;

  log.DebugLog(FailName);
  try

    if not assigned(Target) then
      exit;
    if assigned(UseAllegianceOf) then
    begin
      if Target.Alliance = '' then
        result := true
      else
        result := (UseAllegianceOf.FEnemies.IndexOf(Target.Alliance) < 0) and
          (UseAllegianceOf.FFriends.IndexOf(Target.Alliance) < 0);

      if result and (Alliance <> '') then
        result := (Target.FEnemies.IndexOf(UseAllegianceOf.Alliance) < 0) and
          (Target.FFriends.IndexOf(UseAllegianceOf.Alliance) < 0);
    end
    else
    begin
      if Target.Alliance = '' then
        result := true
      else
        result := (FEnemies.IndexOf(Target.Alliance) < 0) and
          (FFriends.IndexOf(Target.Alliance) < 0);

      if result and (Alliance <> '') then
        result := (Target.FEnemies.IndexOf(Alliance) < 0) and
          (Target.FFriends.IndexOf(Alliance) < 0);
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TCharacter.GetFriends: string;
var
  i: Integer;
const
  FailName: string = 'TCharacter.GetFriends';
begin
  log.DebugLog(FailName);
  try

    if FFriends.Count = 0 then
      result := ''
    else
    begin
      result := FFriends.strings[0];
      for i := 1 to FFriends.Count - 1 do
        result := result + ',' + FFriends.strings[i];
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.SetFriends(const Value: string);
var
  S: string;
  i: Integer;
const
  FailName: string = 'TCharacter.SetFriends';
begin
  log.DebugLog(FailName);
  try

    FFriends.Clear;

    i := 0;
    S := Parse(Value, i, ',');
    while S <> '' do
    begin
      FFriends.add(S);
      Inc(i);
      S := Parse(Value, i, ',');
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TCharacter.GetEnemies: string;
var
  i: Integer;
const
  FailName: string = 'TCharacter.GetEnemies';
begin
  log.DebugLog(FailName);
  try

    if FEnemies.Count = 0 then
      result := ''
    else
    begin
      result := FEnemies.strings[0];
      for i := 1 to FEnemies.Count - 1 do
        result := result + ',' + FEnemies.strings[i];
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.SetEnemies(const Value: string);
var
  S: string;
  i: Integer;
const
  FailName: string = 'TCharacter.SetEnemies';
begin
  log.DebugLog(FailName);
  try

    FEnemies.Clear;

    i := 0;
    S := Parse(Value, i, ',');
    while S <> '' do
    begin
      FEnemies.add(S);
      Inc(i);
      S := Parse(Value, i, ',');
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.MakeAlly(Alliance: string);
var
  i: Integer;
const
  FailName: string = 'TCharacter.MakeAlly';
begin
  log.DebugLog(FailName);
  try

    if Alliance = '' then
      exit;
    i := FEnemies.IndexOf(Alliance);
    if i >= 0 then
      FEnemies.Delete(i);
    FFriends.add(Alliance);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.MakeEnemy(Alliance: string);
var
  i: Integer;
const
  FailName: string = 'TCharacter.MakeEnemy';
begin
  log.DebugLog(FailName);
  try

    if Alliance = '' then
      exit;
    i := FFriends.IndexOf(Alliance);
    if i >= 0 then
      FFriends.Delete(i);
    FEnemies.add(Alliance);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.MakeNeutral(Alliance: string);
var
  i: Integer;
const
  FailName: string = 'TCharacter.MakeNeutral';
begin
  log.DebugLog(FailName);
  try

    if Alliance = '' then
      exit;
    i := FFriends.IndexOf(Alliance);
    if i >= 0 then
      FFriends.Delete(i);
    i := FEnemies.IndexOf(Alliance);
    if i >= 0 then
      FEnemies.Delete(i);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TCharacter.SpellList: TStringList;
const
  FailName: string = 'TCharacter.SpellList';
var
  i, j: Integer;
begin
  log.DebugLog(FailName);

  result := TStringList.Create;
  try
    for i := 0 to AllSpellList.Count - 1 do
    begin
      if TitleExists(AllSpellList.strings[i]) then
      begin
        j := result.add(AllSpellList.strings[i]);
        result.objects[j] := AllSpellList.objects[i];
      end;
    end;

    result.Sorted := true;
  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.SetAttackSound(const Value: string);
var
  SoundCount: Integer;
const
  FailName: string = 'TCharacter.SetAttackSound';
begin
  log.DebugLog(FailName);
  try

    if assigned(SoundLib) then
    begin
      if assigned(AttackSounds) then
        SoundLib.FreeSound(AttackSounds);
      AttackSounds := SoundLib.OpenSound(Value, 1, SoundCount);
    end;
    FAttackSound := Value;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.SetDeathSound(const Value: string);
var
  SoundCount: Integer;
const
  FailName: string = 'TCharacter.SetDeathSound';
begin
  log.DebugLog(FailName);
  try

    if assigned(SoundLib) then
    begin
      if assigned(DeathSounds) then
        SoundLib.FreeSound(DeathSounds);
      DeathSounds := SoundLib.OpenSound(Value, 1, SoundCount);
    end;
    FDeathSound := Value;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.SetPainSound(const Value: string);
var
  SoundCount: Integer;
const
  FailName: string = 'TCharacter.SetPainSound';
begin
  log.DebugLog(FailName);
  try

    if assigned(SoundLib) then
    begin
      if assigned(PainSounds) then
        SoundLib.FreeSound(PainSounds);
      PainSounds := SoundLib.OpenSound(Value, 1, SoundCount);
    end;
    FPainSound := Value;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.SetBattleCry(const Value: string);
var
  SoundCount: Integer;
const
  FailName: string = 'TCharacter.SetBattleCry';
begin
  log.DebugLog(FailName);
  try

    if assigned(SoundLib) then
    begin
      if assigned(BattleCries) then
        SoundLib.FreeSound(BattleCries);
      BattleCries := SoundLib.OpenSound(Value, 1, SoundCount);
    end;
    FBattleCry := Value;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.Init;
var
  Script: TScript;
const
  FailName: string = 'TCharacter.Init';
begin
  log.DebugLog(FailName);
  try
    LoadEquipment(UseDefaultEquipment);
    if FDead then
    begin
      Script := Resource.Script['Death' + Facing.ToString];
      if assigned(Script) then
        Frame := Script.FrameID[Script.Frames]
      else
      begin
        Script := Resource.Script['Death'];
        if assigned(Script) then
          Frame := Script.FrameID[Script.Frames]
        else
          Frame := 1;
      end;
    end
    else
    begin
      if StandAction = '' then
        Script := Resource.Script['Stand' + Facing.ToString]
      else
        Script := Resource.Script[StandAction + Facing.ToString];
      if assigned(Script) then
        Frame := Script.FrameID[Script.Frames]
      else
      begin
        if StandAction = '' then
          Script := Resource.Script['Stand']
        else
          Script := Resource.Script[StandAction];
        if assigned(Script) then
          Frame := Script.FrameID[Script.Frames]
        else
          Frame := 1;
      end;
      Stand;
    end;

    AntiPathEnabled := not TitleExists('NoAntiPath');

    CalcStats;

    if WillBeDisabled then
      Enabled := false;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCharacter.InitAI;
var
  S: string;
begin
  FAIMode := NewAIMode;
  if FAIMode = aiIdle then
    S := IdleAI
  else if FAIMode = aiCombat then
    S := CombatAI
  else if FAIMode = aiParty then
    S := PartyAI
  else
    S := '';

  if S = '' then
    AI := nil
  else
  begin
    AI := AssignAI1(S);
    if not assigned(AI) then
      AI := AssignHumanoidAI(S);
    if not assigned(AI) then
      AI := AssignUndeadAI(S);
    if not assigned(AI) then
      AI := AssignWolfAI(S);
    if not assigned(AI) then
      AI := AssignMiscAI(S);
  end;
end;

procedure TCharacter.Clone(var NewObject: TObject; NewGUID: string);
var
  i: Integer;
  k: TSlot;
  p: PStatModifier;
begin
  NewObject := TCharacter.Create(X, Y, Z, Frame, Enabled);

  TCharacter(NewObject).Spawned := true;
  TCharacter(NewObject).Resource := Resource;
  TCharacter(NewObject).GUID := NewGUID;
  TCharacter(NewObject).Width := Width;
  TCharacter(NewObject).Height := Height;
  TCharacter(NewObject).CenterX := CenterX;
  TCharacter(NewObject).CenterY := CenterY;
  TCharacter(NewObject).Radius := Radius;
  TCharacter(NewObject).Highlightable := Highlightable;
  TCharacter(NewObject).Highlighted := false;
  TCharacter(NewObject).UseLineOfSight := UseLineOfSight;
  TCharacter(NewObject).UseLighting := UseLighting;
  TCharacter(NewObject).AutoTransparent := false;
  TCharacter(NewObject).Visible := Visible;
  if TResource(Resource).FrameHeight >= 100 then
    TCharacter(NewObject).MouseRect := Rect(CenterX - Radius, 20,
      CenterX + Radius, TResource(Resource).FrameHeight - 20)
  else
    TCharacter(NewObject).MouseRect := Rect(CenterX - Radius, 0,
      CenterX + Radius, TResource(Resource).FrameHeight);
  TCharacter(NewObject).GroupName := GroupName;
  TCharacter(NewObject).SpecialEffect := SpecialEffect;
  TCharacter(NewObject).MaskHeight := MaskHeight;
  TCharacter(NewObject).OnActivate := OnActivate;
  TCharacter(NewObject).OnCollide := OnCollide;
  TCharacter(NewObject).Alpha := Alpha;
  TCharacter(NewObject).UnMoveable := UnMoveable;
  TCharacter(NewObject).ColorR := ColorR;
  TCharacter(NewObject).ColorG := ColorG;
  TCharacter(NewObject).ColorB := ColorB;
  TCharacter(NewObject).FDead := FDead;
  TCharacter(NewObject).FTrainingPoints := FTrainingPoints;
  TCharacter(NewObject).FFriends.text := FFriends.text;
  TCharacter(NewObject).FEnemies.text := FEnemies.text;
  TCharacter(NewObject).AttackSound := FAttackSound;
  TCharacter(NewObject).DeathSound := FDeathSound;
  TCharacter(NewObject).PainSound := FPainSound;
  TCharacter(NewObject).BattleCry := FBattleCry;
  TCharacter(NewObject).FMoney := FMoney;
  TCharacter(NewObject).FName := FName;
  TCharacter(NewObject).FFrozen := FFrozen;
  TCharacter(NewObject).FCombatMode := FCombatMode;
  TCharacter(NewObject).UseAttackRecovery := UseAttackRecovery;
  TCharacter(NewObject).RunExists := RunExists;
  TCharacter(NewObject).BaseResistance := BaseResistance;
  TCharacter(NewObject).BaseUnArmedDamage := BaseUnArmedDamage;
  for k := slLeg1 to slMisc3 do
  begin
    if assigned(FEquipment[k]) then
      FEquipment[k].Clone(TObject(TCharacter(NewObject).FEquipment[k]), '');
  end;
  TCharacter(NewObject).BaseStrength := BaseStrength;
  TCharacter(NewObject).BaseCoordination := BaseCoordination;
  TCharacter(NewObject).BaseConstitution := BaseConstitution;
  TCharacter(NewObject).BaseMysticism := BaseMysticism;
  TCharacter(NewObject).BaseCombat := BaseCombat;
  TCharacter(NewObject).BaseStealth := BaseStealth;
  TCharacter(NewObject).BaseMovement := BaseMovement;
  TCharacter(NewObject).BasePerception := BasePerception;
  TCharacter(NewObject).BaseCharm := BaseCharm;
  TCharacter(NewObject).BaseHealingRate := BaseHealingRate;
  TCharacter(NewObject).BaseRechargeRate := BaseRechargeRate;
  TCharacter(NewObject).BaseHitPoints := BaseHitPoints;
  TCharacter(NewObject).BaseMana := BaseMana;
  TCharacter(NewObject).BaseAttackRecovery := BaseAttackRecovery;
  TCharacter(NewObject).BaseHitRecovery := BaseHitRecovery;
  TCharacter(NewObject).BaseAttackBonus := BaseAttackBonus;
  TCharacter(NewObject).BaseDefense := BaseDefense;
  TCharacter(NewObject).FrameCount := FrameCount;
  TCharacter(NewObject).IdleAI := IdleAI;
  TCharacter(NewObject).CombatAI := CombatAI;
  TCharacter(NewObject).PartyAI := PartyAI;
  TCharacter(NewObject).OnDie := OnDie;
  TCharacter(NewObject).FVision := FVision;
  TCharacter(NewObject).Hearing := Hearing;
  TCharacter(NewObject).Smell := Smell;
  TCharacter(NewObject).MysticVision := MysticVision;
  TCharacter(NewObject).IsMerchant := IsMerchant;
  TCharacter(NewObject).BuyingDiscount := BuyingDiscount;
  TCharacter(NewObject).SellingMarkup := SellingMarkup;
  TCharacter(NewObject).Alliance := Alliance;
  TCharacter(NewObject).FProperties.text := FProperties.text;
  TCharacter(NewObject).Titles.text := Titles.text;
  for i := 0 to Titles.Count - 1 do
  begin
    if assigned(Titles.objects[i]) then
    begin
      new(p);
      p^ := PStatModifier(Titles.objects[i])^;
      TCharacter(NewObject).Titles.objects[i] := TObject(p);
    end;
  end;
  TCharacter(NewObject).UseAllegianceOf := UseAllegianceOf;
  TCharacter(NewObject).InterfaceLocked := InterfaceLocked;
  TCharacter(NewObject).AIMode := NewAIMode;
  TCharacter(NewObject).CalcStats;

  i := FigureInstances.add(TCharacter(NewObject).GUID);
  FigureInstances.objects[i] := NewObject;
  Game.AddFigure(TCharacter(NewObject));
end;

function TCharacter.HasItem(const ItemName: string): boolean;
var
  i: Integer;
  j: TSlot;
  S: string;
begin
  result := false;
  S := LowerCase(ItemName);
  for i := 0 to Inventory.Count - 1 do
  begin
    if LowerCase(TItem(Inventory.items[i]).ItemName) = S then
    begin
      result := true;
      exit;
    end;
  end;

  for j := slLeg1 to slMisc3 do
  begin
    if not(assigned(Equipment[j])) then
      continue;
    if LowerCase(Equipment[j].ItemName) = S then
    begin
      result := true;
      exit;
    end;
  end;

end;

procedure TCharacter.RemoveItem(const ItemName: string);
var
  i: Integer;
  j: TSlot;
  S: string;
begin
  S := LowerCase(ItemName);
  for i := 0 to Inventory.Count - 1 do
  begin
    if LowerCase(TItem(Inventory.items[i]).ItemName) = S then
    begin
      TItem(Inventory.items[i]).Enabled := false;
      Inventory.Delete(i);
      exit;
    end;
  end;

  for j := slLeg1 to slMisc3 do
  begin
    if not(assigned(Equipment[j])) then
      continue;
    if LowerCase(Equipment[j].ItemName) = S then
    begin
      Equipment[j].Enabled := false;
      Equipment[j] := nil;
      exit;
    end;
  end;

end;

function TCharacter.AffectDamage(Source: TAniFigure;
  Damage: PDamageProfile): boolean;
var
  i: Integer;
begin
  result := false;
  for i := 0 to Effects.Count - 1 do
    result := result or Effects[i].Hit(Source, Damage);
end;

procedure TCharacter.ClearEquipment;
var
  j: TSlot;
begin
  for j := slLeg1 to slMisc3 do
    FEquipmentNames[j] := '';
end;

{ TWeapon }

procedure TWeapon.Clone(var NewObject: TObject; NewGUID: string);
begin
  if not assigned(NewObject) then
    NewObject := TWeapon.Create(X, Y, Z, 1, false);

  TWeapon(NewObject).AttackSound := FAttackSound;
  TWeapon(NewObject).StrikeMetalSound := FStrikeMetalSound;
  TWeapon(NewObject).StrikeLeatherSound := FStrikeLeatherSound;
  TWeapon(NewObject).FRange := FRange; // Horizontal Pixels
  TWeapon(NewObject).TwoHanded := TwoHanded;
  TWeapon(NewObject).MinStrength := MinStrength;
  TWeapon(NewObject).MinCoordination := MinCoordination;
  TWeapon(NewObject).MaxRestriction := MaxRestriction;

  inherited Clone(NewObject, NewGUID);
end;

destructor TWeapon.Destroy;
const
  FailName: string = 'TWeapon.Destroy';
begin
  log.DebugLog(FailName);
  try

    if assigned(SoundLib) then
    begin
      SoundLib.FreeSound(AttackSounds);
      SoundLib.FreeSound(StrikeMetalSounds);
      SoundLib.FreeSound(StrikeLeatherSounds);
    end;
    inherited;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TWeapon.DoDamage(Source, Target: TCharacter);
var
  Total, Stun: Single;
  Roll, ToHit, F: Single;
  NewDamage: TDamageProfile;
const
  FailName: string = 'TWeapon.DoDamage';
begin
  log.DebugLog(FailName);
  try

    if assigned(Target) then
    begin
      Roll := random(40);
      // if Source=Player then Roll:=40;
      ToHit := Roll + Source.AttackBonus;
{$IFDEF AILog}
      log.log(Source.GUID + ' strikes at ' + Target.GUID + ' and...');
      log.log('  needs: ' + floattostr(Target.Defense));
      log.log('    got: ' + floattostr(ToHit) + ' (' + floattostr(Roll) + ')');
{$ENDIF}
      if Roll < 1 then
      begin
{$IFDEF AILog}
        log.log(Source.GUID + ' misses');
{$ENDIF}
        Target.TakeDamage(Source, 0, 0, false);
      end
      else if (Roll > 35) or (ToHit > Target.Defense) then
      begin
        NewDamage := Source.Damage;
        if Target.AffectDamage(Source, @NewDamage) then
          exit;
        if Target.Track = Source then
          F := 1
        else
        begin
          F := 1;
          // F:=Source.Stealth/10;
          // if F<1 then F:=1;
        end;
        // if ToHit > Target.Defense then begin
        // F:=F*(1+(ToHit-Target.Defense)/40);
        // if F<1 then F:=1;
        // end;
        if (Roll > 35) then
        begin
{$IFDEF AILog}
          log.log(Source.GUID + ' scores a critical hit');
{$ENDIF}
          if (Source = Current) then
            F := F * 2;
          Total := CalcTotalDamage(NewDamage, Target.Resistance, F, true);
        end
        else
        begin
          Total := CalcTotalDamage(NewDamage, Target.Resistance, F, false);
        end;
{$IFDEF AILog}
        log.log(Target.GUID + ' takes ' + IntToStr(round(Total)) +
          ' points of damage from ' + Source.GUID);
{$ENDIF}
        Stun := CalcDamage(NewDamage.Stun) * F -
          Target.Resistance.Stun.Invulnerability;
        if Stun > 0 then
          Stun := Stun * (1 - Target.Resistance.Stun.Resistance);
        Target.TakeDamage(Source, Total, Stun, false);
        if assigned(Target.Equipment[slChest3]) and
          (Target.Equipment[slChest3].Material = maMetal) then
          PlaySound(StrikeMetalSounds, Source.X, Source.Y)
        else if assigned(Target.Equipment[slChest2]) and
          (Target.Equipment[slChest2].Material = maMetal) then
          PlaySound(StrikeMetalSounds, Source.X, Source.Y)
        else if assigned(Target.Equipment[slChest1]) and
          (Target.Equipment[slChest1].Material = maMetal) then
          PlaySound(StrikeMetalSounds, Source.X, Source.Y)
        else
          PlaySound(StrikeLeatherSounds, Source.X, Source.Y);
      end
      else
      begin
{$IFDEF AILog}
        log.log(Source.GUID + ' misses');
{$ENDIF}
        Target.TakeDamage(Source, 0, 0, false);
      end;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TWeapon.Equip(Character: TCharacter);
const
  FailName: string = 'TWeapon.Equip';
begin
  log.DebugLog(FailName);
  try

    Character.ApplyModifier(@Modifier);
    Character.ApplyResistance(@Resistance);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TWeapon.GetDamage(Character: TCharacter);
var
  D, F: Single;
const
  FailName: string = 'TWeapon.GetDamage';
begin
  log.DebugLog(FailName);
  try

    F := Character.Strength / 10;
    if (Character.Strength < MinStrength) and (MinStrength > 0) then
    begin
      D := Character.Strength / MinStrength;
      F := F * D;
    end;
    if (Character.Coordination < MinCoordination) and (MinCoordination > 0) then
    begin
      D := Character.Coordination / MinCoordination;
      F := F * D;
    end;
    if (Character.Restriction > MaxRestriction) and (MaxRestriction >= 0) then
    begin
      D := 1 / (1 + Character.Restriction - MaxRestriction);
      F := F * D;
    end;

    Character.Damage := Damage;
    Character.AddDamageBonus;
    Character.Damage.Piercing.Min := Character.Damage.Piercing.Min * F;
    Character.Damage.Piercing.Max := Character.Damage.Piercing.Max * F;
    Character.Damage.Crushing.Min := Character.Damage.Crushing.Min * F;
    Character.Damage.Crushing.Max := Character.Damage.Crushing.Max * F;
    Character.Damage.Cutting.Min := Character.Damage.Cutting.Min * F;
    Character.Damage.Cutting.Max := Character.Damage.Cutting.Max * F;
    Character.Damage.Stun.Min := Character.Damage.Stun.Min * F;
    Character.Damage.Stun.Max := Character.Damage.Stun.Max * F;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TWeapon.GetProperty(const Name: string): string;
var
  S: string;
const
  FailName: string = 'TWeapon.GetProperty';
begin
  log.DebugLog(FailName);
  try

    S := LowerCase(Name);
    if S = 'twohanded' then
      result := BoolToStr(TwoHanded, True)
    else if S = 'range' then
      result := IntToStr(Range)
    else if S = 'minstrength' then
      result := IntToStr(MinStrength)
    else if S = 'mincoordination' then
      result := IntToStr(MinCoordination)
    else if S = 'maxrestriction' then
      result := IntToStr(MaxRestriction)
    else if S = 'maxrestriction' then
      result := IntToStr(MaxRestriction)
    else if S = 'attacksound' then
      result := AttackSound
    else if S = 'strikeleathersound' then
      result := StrikeLeatherSound
    else if S = 'strikemetalsound' then
      result := StrikeMetalSound
    else
      result := inherited GetProperty(Name);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TWeapon.MeetsRequirements(Character: TCharacter): boolean;
begin
  result := true;
end;

procedure TWeapon.SaveProperties(List: TStringList);
var
  S: string;
const
  FailName: string = 'TWeapon.SaveProperties';
begin
  log.DebugLog(FailName);
  try

    S := 'twohanded=' + BoolToStr(TwoHanded, True);
    List.add(S);
    S := 'range=' + IntToStr(Range);
    List.add(S);
    S := 'minstrength=' + IntToStr(MinStrength);
    List.add(S);
    S := 'mincoordination=' + IntToStr(MinCoordination);
    List.add(S);
    S := 'maxrestriction=' + IntToStr(MaxRestriction);
    List.add(S);
    S := 'attacksound=' + AttackSound;
    List.add(S);
    S := 'strikeleathersound=' + StrikeLeatherSound;
    List.add(S);
    S := 'strikemetalsound=' + StrikeMetalSound;
    List.add(S);
    inherited;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TWeapon.SetAttackSound(const Value: string);
var
  SoundCount: Integer;
const
  FailName: string = 'TWeapon.SetAttackSound';
begin
  log.DebugLog(FailName);
  try

    if assigned(SoundLib) then
    begin
      if assigned(AttackSounds) then
        SoundLib.FreeSound(AttackSounds);
      AttackSounds := SoundLib.OpenSound(Value, 1, SoundCount);
    end;
    FAttackSound := Value;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TWeapon.SetProperty(const Name, Value: string);
var
  S: string;
  L: Integer;
  NoProp: boolean;
const
  FailName: string = 'TWeapon.SetProperty';
begin
  log.DebugLog(FailName);
  try

    NoProp := false;
    S := LowerCase(Name);
    L := length(S);
    case L of
      5:
        begin
          if S = 'range' then
            Range := StrToInt(Value)
          else
            NoProp := true;
        end;
      9:
        begin
          if S = 'twohanded' then
            TwoHanded := (LowerCase(Value) = 'true')
          else
            NoProp := true;
        end;
      11:
        begin
          if S = 'minstrength' then
            MinStrength := StrToInt(Value)
          else if S = 'attacksound' then
            AttackSound := Value
          else
            NoProp := true;
        end;
      14:
        begin
          if S = 'maxrestriction' then
            MaxRestriction := StrToInt(Value)
          else
            NoProp := true;
        end;
      15:
        begin
          if S = 'mincoordination' then
            MinCoordination := StrToInt(Value)
          else
            NoProp := true;
        end;
      16:
        begin
          if S = 'strikemetalsound' then
            StrikeMetalSound := Value
          else
            NoProp := true;
        end;
      18:
        begin
          if S = 'strikeleathersound' then
            StrikeLeatherSound := Value
          else
            NoProp := true;
        end
    else
      NoProp := true;
    end;

    if NoProp then
      inherited;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TWeapon.SetRange(const Value: Integer);
begin
  FRange := Value;
  if FRange < 4 then
    FRange := 4;
end;

procedure TWeapon.SetStrikeLeatherSound(const Value: string);
var
  SoundCount: Integer;
const
  FailName: string = 'TWeapon.SetStrikeLeatherSound';
begin
  log.DebugLog(FailName);
  try

    if assigned(SoundLib) then
    begin
      if assigned(StrikeLeatherSounds) then
        SoundLib.FreeSound(StrikeLeatherSounds);
      StrikeLeatherSounds := SoundLib.OpenSound(Value, 1, SoundCount);
    end;
    FStrikeLeatherSound := Value;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TWeapon.SetStrikeMetalSound(const Value: string);
var
  SoundCount: Integer;
const
  FailName: string = 'TWeapon.SetStrikeMetalSound';
begin
  log.DebugLog(FailName);
  try

    if assigned(SoundLib) then
    begin
      if assigned(StrikeMetalSounds) then
        SoundLib.FreeSound(StrikeMetalSounds);
      StrikeMetalSounds := SoundLib.OpenSound(Value, 1, SoundCount);
    end;
    FStrikeMetalSound := Value;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

{ TContainer }

constructor TContainer.Create(X, Y, Z: Integer; Frame: Word; Enabled: boolean);
const
  FailName: string = 'TContainer.SetStrikeMetalSound';
begin
  log.DebugLog(FailName);
  try

    inherited;
    Inventory := TList.Create;
    OnScriptEnd := ScriptEnd;
    FClosed := true;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TContainer.Activate;
const
  FailName: string = 'TContainer.Activate';
begin
  log.DebugLog(FailName);
  try

    inherited;
    if FPlaying then
      exit;
    if FClosed then
      Open
    else
      Close;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TContainer.Close;
var
  event: string;
const
  FailName: string = 'TContainer.Close';
begin
  log.DebugLog(FailName);
  try

    if AlwaysOpen then
      exit;
    if FPlaying then
      exit;
    if FClosed then
      exit;
    FPlaying := true;
    FClosed := true;
    inherited DoAction('Close');
    PlaySound(CloseSounds, X, Y);

    Inc(CloseCount);
    event := 'OnClose[' + IntToStr(CloseCount) + ']';
    if PropertyExists(event) then
      RunScript(Self, Properties[event])
    else
      RunScript(Self, OnClose);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TContainer.Open;
var
  event: string;
  S: string;
const
  FailName: string = 'TContainer.Open';
begin
  log.DebugLog(FailName);
  try

    if FPlaying then
      exit;
    if not AlwaysOpen then
    begin
      if not FClosed then
        exit;
    end;
    PlaySound(OpenSounds, X, Y);
    if inherited DoAction('Open') then
    begin
      Opening := true;
      FPlaying := true;
    end
    else
    begin
      FClosed := false;

      Inc(OpenCount);
      event := 'OnOpen[' + IntToStr(OpenCount) + ']';
      if PropertyExists(event) then
      begin
        S := Properties[event];
        if S = '' then
          ShowObjectInventory := true
        else
          RunScript(Self, S);
      end
      else
      begin
        if OnOpen = '' then
          ShowObjectInventory := true
        else
          RunScript(Self, OnOpen);
      end;
    end

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TContainer.ScriptEnd(Sender: TObject);
var
  event: string;
  S: string;
const
  FailName: string = 'TContainer.ScriptEnd';
begin
  log.DebugLog(FailName);
  try

    FPlaying := false;
    if Opening then
    begin
      Opening := false;
      FClosed := false;

      Inc(OpenCount);
      event := 'OnOpen[' + IntToStr(OpenCount) + ']';
      if PropertyExists(event) then
      begin
        S := Properties[event];
        if S = '' then
          ShowObjectInventory := true
        else
          RunScript(Self, S);
      end
      else
      begin
        if OnOpen = '' then
          ShowObjectInventory := true
        else
          RunScript(Self, OnOpen);
      end;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TContainer.GetProperty(const Name: string): string;
var
  S: string;
  i: Integer;
const
  FailName: string = 'TContainer.GetProperty';
begin
  log.DebugLog(FailName);
  try

    S := LowerCase(Name);
    if S = 'keyname' then
      result := KeyName
    else if S = 'name' then
      result := Self.Name
    else if S = 'closed' then
      result := BoolToStr(FClosed, True)
    else if S = 'onopen' then
      result := OnOpen
    else if S = 'onclose' then
      result := OnClose
    else if S = 'onopenattempt' then
      result := OnOpenAttempt
    else if S = 'gridwidth' then
      result := IntToStr(GridWidth)
    else if S = 'gridheight' then
      result := IntToStr(GridHeight)
    else if S = 'closecount' then
      result := IntToStr(CloseCount)
    else if S = 'opencount' then
      result := IntToStr(OpenCount)
    else if S = 'openattemptcount' then
      result := IntToStr(OpenAttemptCount)
    else if S = 'alwaysopen' then
      result := BoolToStr(AlwaysOpen, True)
    else if S = 'opensounds' then
      result := OpenSound
    else if S = 'closesounds' then
      result := CloseSound
    else if S = 'inventory.listofitems' then
    begin
      if Inventory.Count = 0 then
        result := ''
      else
      begin
        result := TItem(Inventory.items[0]).ItemName;
        for i := 1 to Inventory.Count - 1 do
          result := result + ',' + TItem(Inventory.items[i]).ItemName;
      end;
    end
    else
      result := inherited GetProperty(Name);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TContainer.SetProperty(const Name, Value: string);
var
  S: string;
  L: Integer;
  NoProp: boolean;
const
  FailName: string = 'TContainer.SetProperty';
begin
  log.DebugLog(FailName);
  try

    NoProp := false;
    S := LowerCase(Name);
    L := length(S);
    case L of
      4:
        begin
          if S = 'name' then
          begin
            // Fix missing character name symbol replacements
            if (length(Value) >= 13) and
              (LowerCase(copy(Value, 1, 13)) = 'charactername') then
            begin
              log.log('** Attempting to fix bad name: ' + Value);
              Self.FName := FixCharacterName(Value);
              log.log('** Result: ' + Self.FName);
            end
            else
              Self.FName := Value;
          end
          else
            NoProp := true;
        end;
      6:
        begin
          if S = 'closed' then
            FClosed := (LowerCase(Value) = 'true')
          else if S = 'onopen' then
          begin
            if not LoadingFromSaveFile then
              OnOpen := Value;
          end
          else
            NoProp := true;
        end;
      7:
        begin
          if S = 'keyname' then
          begin
            KeyName := Value;
            KeyRequired := (Value <> '');
          end
          else if S = 'onclose' then
          begin
            if not LoadingFromSaveFile then
              OnClose := Value;
          end
          else
            NoProp := true;
        end;
      9:
        begin
          if S = 'gridwidth' then
            GridWidth := StrToInt(Value)
          else if S = 'opencount' then
            OpenCount := StrToInt(Value)
          else
            NoProp := true;
        end;
      10:
        begin
          if S = 'gridheight' then
            GridHeight := StrToInt(Value)
          else if S = 'closecount' then
            CloseCount := StrToInt(Value)
          else if S = 'opensounds' then
            OpenSound := Value
          else if S = 'alwaysopen' then
            AlwaysOpen := (LowerCase(Value) = 'true')
          else
            NoProp := true;
        end;
      11:
        begin
          if S = 'closesounds' then
            CloseSound := Value
          else
            NoProp := true;
        end;
      13:
        begin
          if S = 'onopenattempt' then
          begin
            if not LoadingFromSaveFile then
              OnOpenAttempt := Value;
          end
          else
            NoProp := true;
        end;
      16:
        begin
          if S = 'openattemptcount' then
            OpenAttemptCount := StrToInt(Value)
          else
            NoProp := true;
        end;
      21:
        begin
          if S = 'inventory.listofitems' then
          begin
            FInventoryList := Value;
          end
          else
            NoProp := true;
        end
    else
      begin
        NoProp := true;
      end;
    end;

    if NoProp then
      inherited;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TContainer.DoAction(const Action: string): boolean;
var
  S: string;
const
  FailName: string = 'TContainer.DoAction';
begin
  result := false;

  log.DebugLog(FailName);
  try

    result := true;
    S := LowerCase(Action);
    if S = 'open' then
      Open
    else if S = 'close' then
      Close
    else
      result := inherited DoAction(Action);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TContainer.FindFreeInventoryXY(Item: TItem): boolean;
const
  MaxWidth = 32;
  MaxHeight = 32;
var
  iX, iY, i, j: Integer;
  Inv: array [0 .. MaxWidth - 1, 0 .. MaxHeight - 1] of boolean;
  Found: boolean;
  W, H: Integer;
const
  FailName: string = 'TContainer.FindFreeInventoryXY';
begin
  result := false;

  log.DebugLog(FailName);
  try

    if not assigned(Item) then
      exit;

    W := GridWidth * 2;
    H := GridHeight * 2;
    ZeroMemory(@Inv, sizeof(Inv));

    // Set current inventory slots to true
    for i := 0 to Inventory.Count - 1 do
    begin
      with TItem(Inventory.items[i]) do
      begin
        for iX := InvX to InvX + InvW - 1 do
        begin
          for iY := InvY to InvY + InvH - 1 do
          begin
            Inv[iX, iY] := true;
          end;
        end;
      end;
    end;

    // Find a spot for the item
    for i := 0 to W - Item.InvW + 1 do
    begin
      for j := 0 to H - Item.InvH + 1 do
      begin
        Found := true;
        for iX := i to i + Item.InvW - 1 do
        begin
          for iY := j to j + Item.InvH - 1 do
          begin
            if (iX >= W) or (iY >= H) then
            begin
              Found := false;
              break;
            end
            else if Inv[iX, iY] then
            begin
              Found := false;
              break;
            end;
          end;
          if not Found then
            break;
        end;
        if Found then
        begin
          Item.InvX := i;
          Item.InvY := j;
          result := true;
          exit;
        end;
      end;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

destructor TContainer.Destroy;
const
  FailName: string = 'TContainer.Destroy';
begin
  log.DebugLog(FailName);
  try

    Inventory.free;
    if assigned(SoundLib) then
    begin
      SoundLib.FreeSound(OpenSounds);
      SoundLib.FreeSound(CloseSounds);
    end;
    inherited;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TContainer.SaveProperties(List: TStringList);
var
  S: string;
  i: Integer;
const
  FailName: string = 'TContainer.SaveProperties';
begin
  log.DebugLog(FailName);
  try

    S := 'keyname=' + KeyName;
    List.add(S);
    S := 'name=' + Self.Name;
    List.add(S);
    S := 'closed=' + BoolToStr(FClosed, True);
    List.add(S);
    S := 'onopen=' + OnOpen;
    List.add(S);
    S := 'onclose=' + OnClose;
    List.add(S);
    S := 'onopenattempt=' + OnOpenAttempt;
    List.add(S);
    S := 'gridwidth=' + IntToStr(GridWidth);
    List.add(S);
    S := 'gridheight=' + IntToStr(GridHeight);
    List.add(S);
    S := 'closecount=' + IntToStr(CloseCount);
    List.add(S);
    S := 'opencount=' + IntToStr(OpenCount);
    List.add(S);
    S := 'openattemptcount=' + IntToStr(OpenAttemptCount);
    List.add(S);
    S := 'alwaysopen=' + BoolToStr(AlwaysOpen, True);
    List.add(S);
    S := 'opensounds=' + OpenSound;
    List.add(S);
    S := 'closesounds=' + CloseSound;
    List.add(S);
    if Inventory.Count = 0 then
      S := 'inventory.listofitems='
    else
    begin
      S := 'inventory.listofitems=' + TItem(Inventory.items[0]).ItemName;
      for i := 1 to Inventory.Count - 1 do
        S := S + ',' + TItem(Inventory.items[i]).ItemName;
    end;
    List.add(S);
    inherited;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TContainer.SetCloseSound(const Value: string);
var
  SoundCount: Integer;
const
  FailName: string = 'TContainer.SetCloseSound';
begin
  log.DebugLog(FailName);
  try

    if assigned(SoundLib) then
    begin
      if assigned(CloseSounds) then
        SoundLib.FreeSound(CloseSounds);
      CloseSounds := SoundLib.OpenSound(Value, 1, SoundCount);
    end;
    FCloseSound := Value;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TContainer.SetOpenSound(const Value: string);
var
  SoundCount: Integer;
const
  FailName: string = 'TContainer.SetOpenSound';
begin
  log.DebugLog(FailName);
  try

    if assigned(SoundLib) then
    begin
      if assigned(OpenSounds) then
        SoundLib.FreeSound(OpenSounds);
      OpenSounds := SoundLib.OpenSound(Value, 1, SoundCount);
    end;
    FOpenSound := Value;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TContainer.Init;
var
  Script: TScript;
  i, j: Integer;
  S: string;
  NewItem: TItem;
const
  FailName: string = 'TContainer.Init';
begin
  log.DebugLog(FailName);
  try

    if FInventoryList <> '' then
    begin
      i := 0;
      S := Parse(FInventoryList, i, ',');
      while S <> '' do
      begin
        NewItem := PartManager.LoadItem(S, '');
        if assigned(NewItem) then
        begin
          if NoItemPlacement or FindFreeInventoryXY(NewItem) then
          begin
            Inventory.add(NewItem);
            NewItem.Enabled := false;
            NewItem.Resource := PartManager.GetLayerResource
              (NewItem.LayeredImage);
            NewItem.GUID := '';
            j := FigureInstances.add(NewItem.GUID);
            FigureInstances.objects[j] := NewItem;
          end
          else
          begin
            NewItem.free;
            log.log('*** Error: Item (' + S +
              ') could not be added to inventory for ' + GUID);
          end;
        end
        else
        begin
          log.log('*** Error: Item (' + S +
            ') could not be loaded for ' + GUID);
        end;
        Inc(i);
        S := Parse(FInventoryList, i, ',');
      end;
      FInventoryList := '';
    end;

    if Closed then
    begin
      Script := Resource.Script['Close' + Facing.ToString];
      if assigned(Script) then
        Frame := Script.FrameID[Script.Frames]
      else
      begin
        Script := Resource.Script['Close'];
        if assigned(Script) then
          Frame := Script.FrameID[Script.Frames]
        else
          Frame := 1;
      end;
    end
    else
    begin
      Script := Resource.Script['Open' + Facing.ToString];
      if assigned(Script) then
        Frame := Script.FrameID[Script.Frames]
      else
      begin
        Script := Resource.Script['Open'];
        if assigned(Script) then
          Frame := Script.FrameID[Script.Frames]
        else
          Frame := 1;
      end;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TContainer.GetName: string;
const
  FailName: string = 'TContainer.GetName';
begin
  log.DebugLog(FailName);
  try

    result := FName;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TContainer.DoFrame;
const
  FailName: string = 'TContainer.DoFrame';
begin
  log.DebugLog(FailName);
  try

    inherited;
    if ShowObjectInventory then
    begin

      if FixMissingItems then
        frmMain.BeginObjInventory(Current, Self);
      ShowObjectInventory := false;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TContainer.FixMissingItems: boolean;
var
  i: Integer;
  ZeroCount: Integer;
begin
  // Code to repair missing items bug
  result := true;
  ZeroCount := 0;
  for i := 0 to Inventory.Count - 1 do
  begin
    if (TItem(Inventory.items[i]).InvX = 0) and
      (TItem(Inventory.items[i]).InvY = 0) then
      Inc(ZeroCount);
  end;
  if ZeroCount > 1 then
  begin
    for i := Inventory.Count - 2 downto 0 do
    begin
      if not FindFreeInventoryXY(TItem(Inventory.items[i])) then
      begin
        result := false;
        TItem(Inventory.items[i]).SetPos(X, Y, Z);
        TItem(Inventory.items[i]).Enabled := true;
        Inventory.Delete(i);
      end;
    end;
  end;
  if not result then
  begin
    Say(ChestMsg, cTalkRedColor);
  end;
end;

function TContainer.HasItem(const ItemName: string): boolean;
var
  i: Integer;
  S: string;
begin
  result := false;
  S := LowerCase(ItemName);
  for i := 0 to Inventory.Count - 1 do
  begin
    if LowerCase(TItem(Inventory.items[i]).ItemName) = S then
    begin
      result := true;
      exit;
    end;
  end;
end;

procedure TContainer.RemoveItem(const ItemName: string);
var
  i: Integer;
  S: string;
begin
  S := LowerCase(ItemName);
  for i := 0 to Inventory.Count - 1 do
  begin
    if LowerCase(TItem(Inventory.items[i]).ItemName) = S then
    begin
      TItem(Inventory.items[i]).Enabled := false;
      Inventory.Delete(i);
      exit;
    end;
  end;
end;

{ TDoor }

constructor TDoor.Create(X, Y, Z: Integer; Frame: Word; Enabled: boolean);
const
  FailName: string = 'TDoor.Create';
begin
  log.DebugLog(FailName);
  try

    inherited;
    Visible := false;
    OnScriptEnd := ScriptEnd;
    FClosed := true;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TDoor.Activate;
const
  FailName: string = 'TDoor.Activate';
begin
  log.DebugLog(FailName);
  try

    inherited;
    if FPlaying then
      exit;
    if FClosed then
      Open
    else
      Close;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TDoor.Close;
var
  event: string;
const
  FailName: string = 'TDoor.Close';
begin
  log.DebugLog(FailName);
  try

    if FPlaying then
      exit;
    if FClosed then
      exit;
    FPlaying := true;
    FClosed := true;
    inherited DoAction('Close');
    PlaySound(CloseSounds, X, Y);

    Inc(CloseCount);
    event := 'OnClose[' + IntToStr(CloseCount) + ']';
    if PropertyExists(event) then
      RunScript(Self, Properties[event])
    else
      RunScript(Self, OnClose);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TDoor.DoAction(const Action: string): boolean;
var
  S: string;
const
  FailName: string = 'TDoor.DoAction';
begin
  result := false;

  log.DebugLog(FailName);
  try

    result := true;
    S := LowerCase(Action);
    if S = 'open' then
      Open
    else if S = 'close' then
      Close
    else
      result := inherited DoAction(Action);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TDoor.Open;
const
  FailName: string = 'TDoor.Open';
begin
  log.DebugLog(FailName);
  try

    if FPlaying then
      exit;
    if not FClosed then
      exit;
    FPlaying := true;
    Opening := true;
    PlaySound(OpenSounds, X, Y);
    inherited DoAction('Open');

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TDoor.GetProperty(const Name: string): string;
var
  S: string;
const
  FailName: string = 'TDoor.GetProperty';
begin
  log.DebugLog(FailName);
  try

    S := LowerCase(Name);
    if S = 'keyname' then
      result := KeyName
    else if S = 'closed' then
      result := BoolToStr(FClosed, True)
    else if S = 'onopen' then
      result := OnOpen
    else if S = 'onclose' then
      result := OnClose
    else if S = 'onopenattempt' then
      result := OnOpenAttempt
    else if S = 'closecount' then
      result := IntToStr(CloseCount)
    else if S = 'opencount' then
      result := IntToStr(OpenCount)
    else if S = 'openattemptcount' then
      result := IntToStr(OpenAttemptCount)
    else if S = 'opensounds' then
      result := OpenSound
    else if S = 'closesounds' then
      result := CloseSound
    else
      result := inherited GetProperty(Name);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TDoor.SetProperty(const Name, Value: string);
var
  S: string;
  L: Integer;
  NoProp: boolean;
const
  FailName: string = 'TDoor.SetProperty';
begin
  log.DebugLog(FailName);
  try

    NoProp := false;
    S := LowerCase(Name);
    L := length(S);
    case L of
      6:
        begin
          if S = 'closed' then
            FClosed := (LowerCase(Value) = 'true')
          else if S = 'onopen' then
          begin
            if not LoadingFromSaveFile then
              OnOpen := Value;
          end
          else
            NoProp := true;
        end;
      7:
        begin
          if S = 'keyname' then
          begin
            KeyName := Value;
            KeyRequired := (Value <> '');
          end
          else if S = 'onclose' then
          begin
            if not LoadingFromSaveFile then
              OnClose := Value;
          end
          else
            NoProp := true;
        end;
      9:
        begin
          if S = 'opencount' then
            OpenCount := StrToInt(Value)
          else
            NoProp := true;
        end;
      10:
        begin
          if S = 'closecount' then
            CloseCount := StrToInt(Value)
          else if S = 'opensounds' then
            OpenSound := Value
          else
            NoProp := true;
        end;
      11:
        begin
          if S = 'closesounds' then
            CloseSound := Value
          else
            NoProp := true;
        end;
      13:
        begin
          if S = 'onopenattempt' then
          begin
            if not LoadingFromSaveFile then
              OnOpenAttempt := Value;
          end
          else
            NoProp := true;
        end;
      16:
        begin
          if S = 'openattemptcount' then
            OpenAttemptCount := StrToInt(Value)
          else
            NoProp := true;
        end
    else
      begin
        NoProp := true;
      end;
    end;

    if NoProp then
      inherited;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TDoor.ChangeFrame;
var
  pFrame: PItemInstanceInfo;
  i: Integer;
const
  FailName: string = 'TDoor.ChangeFrame';
begin
  log.DebugLog(FailName);
  try

    // Log.Log('ChangeFrame: '+inttostr(Frame)+','+inttostr(PrevFrame));
    if Resource is TDoorResource then
    begin
      if PrevFrame <> 0 then
      begin
        pFrame := Frame1;
        Inc(pFrame, (PrevFrame - 1) * TDoorResource(Resource).Strips);
        for i := 1 to TDoorResource(Resource).Strips do
        begin
          pFrame.Visible := false;
          Inc(pFrame);
        end;
      end;
      if Frame <> 0 then
      begin
        pFrame := Frame1;
        Inc(pFrame, (Frame - 1) * TDoorResource(Resource).Strips);
        for i := 1 to TDoorResource(Resource).Strips do
        begin
          pFrame.Visible := true;
          Inc(pFrame);
        end;
      end;
      if assigned(View) then
        View.ForceRefresh := true;
    end
    else
      log.log('*** Error: Invalid door resource');
    PrevFrame := Frame;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TDoor.ScriptEnd(Sender: TObject);
var
  event: string;
const
  FailName: string = 'TDoor.ScriptEnd';
begin
  log.DebugLog(FailName);
  try

    FPlaying := false;
    if Opening then
    begin
      Opening := false;
      FClosed := false;
      Inc(OpenCount);
      event := 'OnOpen[' + IntToStr(OpenCount) + ']';
      if PropertyExists(event) then
        RunScript(Self, Properties[event])
      else
        RunScript(Self, OnOpen);
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TDoor.DoFrame;
const
  FailName: string = 'TDoor.DoFrame';
begin
  log.DebugLog(FailName);
  try

    Visible := Highlighted;
    if Frame <> PrevFrame then
      ChangeFrame;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TDoor.SaveProperties(List: TStringList);
var
  S: string;
const
  FailName: string = 'TDoor.SaveProperties';
begin
  log.DebugLog(FailName);
  try

    S := 'keyname=' + KeyName;
    List.add(S);
    S := 'closed=' + BoolToStr(FClosed, True);
    List.add(S);
    S := 'onopen=' + OnOpen;
    List.add(S);
    S := 'onclose=' + OnClose;
    List.add(S);
    S := 'onopenattempt=' + OnOpenAttempt;
    List.add(S);
    S := 'closecount=' + IntToStr(CloseCount);
    List.add(S);
    S := 'opencount=' + IntToStr(OpenCount);
    List.add(S);
    S := 'openattemptcount=' + IntToStr(OpenAttemptCount);
    List.add(S);
    S := 'opensounds=' + OpenSound;
    List.add(S);
    S := 'closesounds=' + CloseSound;
    List.add(S);
    inherited;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TDoor.SetCloseSound(const Value: string);
var
  SoundCount: Integer;
const
  FailName: string = 'TDoor.SetCloseSound';
begin
  log.DebugLog(FailName);
  try

    if assigned(SoundLib) then
    begin
      if assigned(CloseSounds) then
        SoundLib.FreeSound(CloseSounds);
      CloseSounds := SoundLib.OpenSound(Value, 1, SoundCount);
    end;
    FCloseSound := Value;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TDoor.SetOpenSound(const Value: string);
var
  SoundCount: Integer;
const
  FailName: string = 'TDoor.SetOpenSound';
begin
  log.DebugLog(FailName);
  try

    if assigned(SoundLib) then
    begin
      if assigned(OpenSounds) then
        SoundLib.FreeSound(OpenSounds);
      OpenSounds := SoundLib.OpenSound(Value, 1, SoundCount);
    end;
    FOpenSound := Value;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

destructor TDoor.Destroy;
const
  FailName: string = 'TDoor.Destroy';
begin
  log.DebugLog(FailName);
  try

    if assigned(SoundLib) then
    begin
      SoundLib.FreeSound(OpenSounds);
      SoundLib.FreeSound(CloseSounds);
    end;
    inherited;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TDoor.Init;
var
  Script: TScript;
  k: Integer;
  pFrame: PItemInstanceInfo;
const
  FailName: string = 'TDoor.Init';
begin
  log.DebugLog(FailName);
  try

    if not assigned(Frame1) then
    begin // Prevent a load game from creating a duplicate door
      if Resource is TDoorResource then
      begin
        for k := 0 to TResource(Resource).FrameCount - 1 do
        begin
          if k = 0 then
            Frame1 := GameMap.AddItem(TDoorResource(Resource).ItemZone,
              TDoorResource(Resource).ItemIndex + k, X - CenterX,
              Y + Height - CenterY, Z, 0, false)
          else
            GameMap.AddItem(TDoorResource(Resource).ItemZone,
              TDoorResource(Resource).ItemIndex + k, X - CenterX,
              Y + Height - CenterY, Z, 0, false);
        end;
      end;
    end;

    if assigned(Frame1) then
    begin
      pFrame := Frame1;
      for k := 1 to TResource(Resource).FrameCount *
        TDoorResource(Resource).Strips do
      begin
        pFrame.Visible := false;
        Inc(pFrame);
      end;
    end;

    if Closed then
    begin
      Script := Resource.Script['Close' + Facing.ToString];
      if assigned(Script) then
        Frame := Script.FrameID[Script.Frames]
      else
      begin
        Script := Resource.Script['Close'];
        if assigned(Script) then
          Frame := Script.FrameID[Script.Frames]
        else
          Frame := 1;
      end;
    end
    else
    begin
      Script := Resource.Script['Open' + Facing.ToString];
      if assigned(Script) then
        Frame := Script.FrameID[Script.Frames]
      else
      begin
        Script := Resource.Script['Open'];
        if assigned(Script) then
          Frame := Script.FrameID[Script.Frames]
        else
          Frame := 1;
      end;
    end;

    ChangeFrame;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TDoor.SetResource(const Value: TAniResource);
const
  FailName: string = 'TDoor.SetResource';
begin
  log.DebugLog(FailName);
  try

    inherited;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TDoor.GetName: string;
const
  FailName: string = 'TDoor.GetName';
begin
  log.DebugLog(FailName);
  try

    result := FName;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

{ TItem }

function TItem.CanEquip(Slot: TSlot; Character: TCharacter): boolean;
const
  FailName: string = 'TItem.CanEquip';
begin
  result := false;

  log.DebugLog(FailName);
  try

    result := (Slot in SlotsAllowed) and MeetsRequirements(Character);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TItem.Clone(var NewObject: TObject; NewGUID: string);
var
  i: Integer;
begin
  if not assigned(NewObject) then
    NewObject := TItem.Create(X, Y, Z, 1, false);

  TItem(NewObject).Resource := Resource;
  TItem(NewObject).GUID := NewGUID;
  TItem(NewObject).Width := Width;
  TItem(NewObject).Height := Height;
  TItem(NewObject).CenterX := CenterX;
  TItem(NewObject).CenterY := CenterY;
  TItem(NewObject).Radius := Radius;
  TItem(NewObject).Highlightable := Highlightable;
  TItem(NewObject).Highlighted := false;
  TItem(NewObject).UseLineOfSight := UseLineOfSight;
  TItem(NewObject).UseLighting := UseLighting;
  TItem(NewObject).AutoTransparent := false;
  TItem(NewObject).Visible := Visible;
  TItem(NewObject).MouseRect := MouseRect;
  TItem(NewObject).GroupName := GroupName;
  TItem(NewObject).SpecialEffect := SpecialEffect;
  TItem(NewObject).MaskHeight := MaskHeight;
  TItem(NewObject).OnActivate := OnActivate;
  TItem(NewObject).OnCollide := OnCollide;
  TItem(NewObject).Alpha := Alpha;
  TItem(NewObject).UnMoveable := UnMoveable;
  TItem(NewObject).ColorR := ColorR;
  TItem(NewObject).ColorG := ColorG;
  TItem(NewObject).ColorB := ColorB;
  TItem(NewObject).SlotsAllowed := SlotsAllowed;
  TItem(NewObject).Modifier := Modifier;
  TItem(NewObject).Resistance := Resistance;
  TItem(NewObject).Damage := Damage;
  // Applies ony to unarmed combat (unless item is TWeapon)
  TItem(NewObject).Value := Value;
  TItem(NewObject).Weight := Weight;
  TItem(NewObject).Magic := Magic; // Is item magical?  If so, how much?
  TItem(NewObject).Identified := Identified;
  TItem(NewObject).Title := Title;
  TItem(NewObject).ItemName := ItemName;
  TItem(NewObject).DisplayName := DisplayName;
  TItem(NewObject).ItemInfo := ItemInfo;
  TItem(NewObject).SecretName := SecretName;
  TItem(NewObject).SecretInfo := SecretInfo;
  TItem(NewObject).PartName := PartName;
  TItem(NewObject).OnPickUp := OnPickUp;
  TItem(NewObject).OnDrop := OnDrop;
  TItem(NewObject).InvX := InvX;
  TItem(NewObject).InvY := InvY;
  TItem(NewObject).InvW := InvW;
  TItem(NewObject).InvH := InvH;
  TItem(NewObject).Material := Material;
  TItem(NewObject).LayerIndex := LayerIndex;
  TItem(NewObject).InventoryImage := InventoryImage;
  TItem(NewObject).LayeredImage := LayeredImage;

  i := FigureInstances.add(TItem(NewObject).GUID);
  FigureInstances.objects[i] := NewObject;
  Game.AddFigure(TItem(NewObject));
end;

procedure TItem.Drop;
var
  event: string;
const
  FailName: string = 'TItem.Drop';
begin
  log.DebugLog(FailName);
  try

    if not assigned(Resource) then
    begin
      LayeredImage := PartManager.GetImageFile(PartName,
        'HumanMaleLayers\BaseHumanMale.gif');
      Resource := PartManager.GetLayerResource(LayeredImage);
    end;

    if ActionExists('Default') then
      DoAction('Default')
    else
    begin
      if Resource is TLayerResource then
      begin
        Frame := TLayerResource(Resource).ItemFrame + 1;
      end;
    end;

    Inc(DropCount);
    event := 'OnDrop[' + IntToStr(DropCount) + ']';
    if PropertyExists(event) then
      RunScript(Self, Properties[event])
    else
      RunScript(Self, OnDrop);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TItem.Equip(Character: TCharacter);
const
  FailName: string = 'TItem.Equip';
begin
  log.DebugLog(FailName);
  try

    Character.ApplyModifier(@Modifier);
    Character.ApplyResistance(@Resistance);

    Character.UnArmedDamage.Piercing.Min := Character.UnArmedDamage.Piercing.Min
      + Damage.Piercing.Min;
    Character.UnArmedDamage.Piercing.Max := Character.UnArmedDamage.Piercing.Max
      + Damage.Piercing.Max;
    Character.UnArmedDamage.Crushing.Min := Character.UnArmedDamage.Crushing.Min
      + Damage.Crushing.Min;
    Character.UnArmedDamage.Crushing.Max := Character.UnArmedDamage.Crushing.Max
      + Damage.Crushing.Max;
    Character.UnArmedDamage.Cutting.Min := Character.UnArmedDamage.Cutting.Min +
      Damage.Cutting.Min;
    Character.UnArmedDamage.Cutting.Max := Character.UnArmedDamage.Cutting.Max +
      Damage.Cutting.Max;
    Character.UnArmedDamage.Heat.Min := Character.UnArmedDamage.Heat.Min +
      Damage.Heat.Min;
    Character.UnArmedDamage.Heat.Max := Character.UnArmedDamage.Heat.Max +
      Damage.Heat.Max;
    Character.UnArmedDamage.Cold.Min := Character.UnArmedDamage.Cold.Min +
      Damage.Cold.Min;
    Character.UnArmedDamage.Cold.Max := Character.UnArmedDamage.Cold.Max +
      Damage.Cold.Max;
    Character.UnArmedDamage.Electric.Min := Character.UnArmedDamage.Electric.Min
      + Damage.Electric.Min;
    Character.UnArmedDamage.Electric.Max := Character.UnArmedDamage.Electric.Max
      + Damage.Electric.Max;
    Character.UnArmedDamage.Poison.Min := Character.UnArmedDamage.Poison.Min +
      Damage.Poison.Min;
    Character.UnArmedDamage.Poison.Max := Character.UnArmedDamage.Poison.Max +
      Damage.Poison.Max;
    Character.UnArmedDamage.Magic.Min := Character.UnArmedDamage.Magic.Min +
      Damage.Magic.Min;
    Character.UnArmedDamage.Magic.Max := Character.UnArmedDamage.Magic.Max +
      Damage.Magic.Max;
    Character.UnArmedDamage.Mental.Min := Character.UnArmedDamage.Mental.Min +
      Damage.Mental.Min;
    Character.UnArmedDamage.Mental.Max := Character.UnArmedDamage.Mental.Max +
      Damage.Mental.Max;
    Character.UnArmedDamage.Stun.Min := Character.UnArmedDamage.Stun.Min +
      Damage.Stun.Min;
    Character.UnArmedDamage.Stun.Max := Character.UnArmedDamage.Stun.Max +
      Damage.Stun.Max;
    Character.UnArmedDamage.Special.Min := Character.UnArmedDamage.Special.Min +
      Damage.Special.Min;
    Character.UnArmedDamage.Special.Max := Character.UnArmedDamage.Special.Max +
      Damage.Special.Max;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TItem.GetIconicImage: IDirectDrawSurface;
var
  ColorMatch: Integer;
  W, H: Integer;
  ddsd: TDDSurfaceDesc;
  Bits: BITPLANE;
  S: string;
  NewResource: TResource;
const
  FailName: string = 'TItem.GetIconicImage';
begin
  log.DebugLog(FailName);
  try

    result := nil;
    W := GroundListWidth;
    H := GroundListHeight;
    result := DDGetSurface(lpDD, W, H, cTransparent, true, ColorMatch);
    if assigned(result) then
    begin
      if assigned(Resource) then
      begin
        NewResource := TResource(Resource);
      end
      else
      begin
        S := PartManager.GetImageFile(PartName,
          'HumanMaleLayers\BaseHumanMale.gif');
        NewResource := PartManager.GetLayerResource(S);
      end;
      if assigned(NewResource) then
      begin
        ddsd.dwSize := sizeof(ddsd);
        if result.Lock(nil, ddsd, DDLOCK_WAIT, 0) = DD_OK then
        begin
          try
            Bits.bitsPtr := ddsd.lpSurface;
            Bits.bitsWdh := W;
            Bits.bitsHgh := H;
            Bits.bitsFmt := dfx_pixelformat;
            Bits.bitsPitch := ddsd.lPitch;
            Bits.BaseX := NewResource.CenterX - W div 2;
            Bits.BaseY := NewResource.CenterY - H div 2;
            NewResource.RenderLocked(nil, @Bits);
          finally
            result.Unlock(nil);
          end;
        end;
      end;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TItem.GetInfo: string;
const
  FailName: string = 'TItem.GetInfo';
begin
  log.DebugLog(FailName);
  try

    if Identified and (SecretInfo <> '') then
      result := SecretInfo
    else
      result := ItemInfo

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TItem.GetInventoryImage: IDirectDrawSurface;
var
  ColorMatch: Integer;
  ddsd: TDDSurfaceDesc;
  Bits: BITPLANE;
  InvResource: TResource;
  W, H: Integer;
const
  FailName: string = 'TItem.GetInventoryImage';
begin
  log.DebugLog(FailName);
  try

    result := nil;
    if (InventoryImage = '') or (InvW = 0) or (InvH = 0) then
      exit;
    InvResource := LoadArtResource(InventoryImage);
    try
      if assigned(InvResource) and (InvResource is TInventoryResource) then
      begin
        W := InvW * 18;
        H := InvH * 26;
        result := DDGetSurface(lpDD, W, H, cTransparent, true, ColorMatch);
        if assigned(result) then
        begin
          ddsd.dwSize := sizeof(ddsd);
          if result.Lock(nil, ddsd, DDLOCK_WAIT, 0) = DD_OK then
          begin
            try
              Bits.bitsPtr := ddsd.lpSurface;
              Bits.bitsWdh := W;
              Bits.bitsHgh := H;
              Bits.bitsFmt := dfx_pixelformat;
              Bits.bitsPitch := ddsd.lPitch;
              Bits.BaseX := 0;
              Bits.BaseY := 0;
              InvResource.RenderLocked(nil, @Bits);
            finally
              result.Unlock(nil);
            end;
          end;
        end;
      end
      else
      begin
        log.log('*** Error: Inventory image (' + InventoryImage +
          ') could not be loaded');
      end;
    finally
      InvResource.free;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TItem.GetInventoryShadow: IDirectDrawSurface;
var
  ColorMatch: Integer;
  W, H: Integer;
  ddsd: TDDSurfaceDesc;
  Bits: BITPLANE;
  InvResource: TResource;
const
  FailName: string = 'TItem.GetInventoryShadow';
begin
  log.DebugLog(FailName);
  try

    result := nil;
    if InvW = 0 then
      W := 18
    else
      W := InvW * 18;
    if InvH = 0 then
      H := 26
    else
      H := InvH * 26;
    result := DDGetSurface(lpDD, W, H, cTransparent, true, ColorMatch);
    if assigned(result) then
    begin
      ddsd.dwSize := sizeof(ddsd);
      if result.Lock(nil, ddsd, DDLOCK_WAIT, 0) = DD_OK then
      begin
        try
          Bits.bitsPtr := ddsd.lpSurface;
          Bits.bitsWdh := W;
          Bits.bitsHgh := H;
          Bits.bitsFmt := dfx_pixelformat;
          Bits.bitsPitch := ddsd.lPitch;
          Bits.BaseX := 0;
          Bits.BaseY := 0;
          if InventoryImage <> '' then
          begin
            InvResource := LoadArtResource(InventoryImage);
            try
              if assigned(InvResource) then
              begin
                if InvResource is TInventoryResource then
                begin
                  TInventoryResource(InvResource)
                    .RenderShadowLocked(nil, @Bits);
                end;
              end;
            finally
              InvResource.free;
            end;
          end;
        finally
          result.Unlock(nil);
        end;
      end;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TItem.GetName: string;
const
  FailName: string = 'TItem.GetName';
begin
  log.DebugLog(FailName);
  try

    if Identified and (SecretName <> '') then
      result := SecretName
    else
      result := DisplayName;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TItem.GetProperty(const Name: string): string;
var
  S: string;
const
  FailName: string = 'TItem.GetProperty';
begin
  log.DebugLog(FailName);
  try

    S := LowerCase(Name);
    if S = 'itemname' then
      result := ItemName
    else if S = 'iteminfo' then
      result := ItemInfo
    else if S = 'displayname' then
      result := DisplayName
    else if S = 'partname' then
      result := PartName
    else if S = 'inventoryimage' then
      result := InventoryImage
    else if S = 'layeredimage' then
      result := LayeredImage
    else if S = 'invx' then
      result := IntToStr(InvX)
    else if S = 'invy' then
      result := IntToStr(InvY)
    else if S = 'invw' then
      result := IntToStr(InvW)
    else if S = 'invh' then
      result := IntToStr(InvH)
    else if S = 'layerindex' then
      result := IntToStr(LayerIndex)
    else if S = 'identified' then
      result := BoolToStr(Identified, True)
    else if S = 'secretname' then
      result := SecretName
    else if S = 'secretinfo' then
      result := SecretInfo
    else if S = 'magic' then
      result := IntToStr(Magic)
    else if S = 'weight' then
      result := IntToStr(Weight)
    else if S = 'title' then
      result := Title
    else if S = 'value' then
      result := IntToStr(Self.Value)
    else if S = 'onpickup' then
      result := OnPickUp
    else if S = 'ondrop' then
      result := OnDrop
      // Modifiers
    else if S = 'modifier.strength' then
      result := IntToStr(Modifier.Strength)
    else if S = 'modifier.coordination' then
      result := IntToStr(Modifier.Coordination)
    else if S = 'modifier.constitution' then
      result := IntToStr(Modifier.Constitution)
    else if S = 'modifier.mysticism' then
      result := IntToStr(Modifier.Mysticism)
    else if S = 'modifier.combat' then
      result := IntToStr(Modifier.Combat)
    else if S = 'modifier.stealth' then
      result := IntToStr(Modifier.Stealth)
    else if S = 'modifier.restriction' then
      result := IntToStr(Modifier.Restriction)
    else if S = 'modifier.attackrecovery' then
      result := IntToStr(Modifier.AttackRecovery)
    else if S = 'modifier.hitrecovery' then
      result := IntToStr(Modifier.HitRecovery)
    else if S = 'modifier.perception' then
      result := IntToStr(Modifier.Perception)
    else if S = 'modifier.charm' then
      result := IntToStr(Modifier.Charm)
    else if S = 'modifier.healingrate' then
      result := IntToStr(Modifier.HealingRate)
    else if S = 'modifier.rechargerate' then
      result := IntToStr(Modifier.RechargeRate)
    else if S = 'modifier.hitpoints' then
      result := IntToStr(Modifier.HitPoints)
    else if S = 'modifier.mana' then
      result := IntToStr(Modifier.Mana)
      // Resistance
    else if GetResistanceProps(S, result, Resistance) then
      // Damage
    else if GetDamageProps(S, result, Damage) then
    else if S = 'slot' then
    begin
      result := '';
      if slLeg1 in SlotsAllowed then
        result := result + '[Leg1]';
      if slBoot in SlotsAllowed then
        result := result + '[Boot]';
      if slLeg2 in SlotsAllowed then
        result := result + '[Leg2]';
      if slChest1 in SlotsAllowed then
        result := result + '[Chest1]';
      if slChest2 in SlotsAllowed then
        result := result + '[Chest2]';
      if slArm in SlotsAllowed then
        result := result + '[Arm]';
      if slBelt in SlotsAllowed then
        result := result + '[Belt]';
      if slChest3 in SlotsAllowed then
        result := result + '[Chest3]';
      if slGauntlet in SlotsAllowed then
        result := result + '[Gauntlet]';
      if slOuter in SlotsAllowed then
        result := result + '[Outer]';
      if slHelmet in SlotsAllowed then
        result := result + '[Helmet]';
      if slWeapon in SlotsAllowed then
        result := result + '[Weapon]';
      if slShield in SlotsAllowed then
        result := result + '[Shield]';
      if sltabar in SlotsAllowed then
        result := result + '[tabar]';
      if slMisc1 in SlotsAllowed then
        result := result + '[Misc1]';
      if slMisc2 in SlotsAllowed then
        result := result + '[Misc2]';
      if slMisc3 in SlotsAllowed then
        result := result + '[Misc3]';
    end
    else
      result := inherited GetProperty(Name);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TItem.Init;
const
  FailName: string = 'TItem.Init';
begin
  log.DebugLog(FailName);
  try

    if not DoAction('Default') then
    begin
      if Resource is TLayerResource then
        Frame := TLayerResource(Resource).ItemFrame + 1;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TItem.MeetsRequirements(Character: TCharacter): boolean;
begin
  result := true;
end;

procedure TItem.PickUp;
var
  event: string;
const
  FailName: string = 'TItem.PickUp';
begin
  log.DebugLog(FailName);
  try

    Inc(PickUpCount);
    event := 'OnPickUp[' + IntToStr(PickUpCount) + ']';
    if PropertyExists(event) then
      RunScript(Self, Properties[event])
    else
      RunScript(Self, OnPickUp);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TItem.SaveProperties(List: TStringList);
var
  S: string;
const
  FailName: string = 'TItem.SaveProperties';
begin
  log.DebugLog(FailName);
  try

    S := 'itemname=' + ItemName;
    List.add(S);
    S := 'iteminfo=' + ItemInfo;
    List.add(S);
    S := 'displayname=' + DisplayName;
    List.add(S);
    S := 'partname=' + PartName;
    List.add(S);
    S := 'inventoryimage=' + InventoryImage;
    List.add(S);
    S := 'layeredimage=' + LayeredImage;
    List.add(S);
    S := 'invx=' + IntToStr(InvX);
    List.add(S);
    S := 'invy=' + IntToStr(InvY);
    List.add(S);
    S := 'invw=' + IntToStr(InvW);
    List.add(S);
    S := 'invh=' + IntToStr(InvH);
    List.add(S);
    S := 'layerindex=' + IntToStr(LayerIndex);
    List.add(S);
    S := 'identified=' + BoolToStr(Identified, True);
    List.add(S);
    S := 'secretname=' + SecretName;
    List.add(S);
    S := 'secretinfo=' + SecretInfo;
    List.add(S);
    S := 'magic=' + IntToStr(Magic);
    List.add(S);
    S := 'weight=' + IntToStr(Weight);
    List.add(S);
    S := 'title=' + Title;
    List.add(S);
    S := 'value=' + IntToStr(Self.Value);
    List.add(S);
    S := 'onpickup=' + OnPickUp;
    List.add(S);
    S := 'ondrop=' + OnDrop;
    List.add(S);
    // Modifiers
    S := 'modifier.strength=' + IntToStr(Modifier.Strength);
    List.add(S);
    S := 'modifier.coordination=' + IntToStr(Modifier.Coordination);
    List.add(S);
    S := 'modifier.constitution=' + IntToStr(Modifier.Constitution);
    List.add(S);
    S := 'modifier.mysticism=' + IntToStr(Modifier.Mysticism);
    List.add(S);
    S := 'modifier.combat=' + IntToStr(Modifier.Combat);
    List.add(S);
    S := 'modifier.stealth=' + IntToStr(Modifier.Stealth);
    List.add(S);
    S := 'modifier.restriction=' + IntToStr(Modifier.Restriction);
    List.add(S);
    S := 'modifier.attackrecovery=' + IntToStr(Modifier.AttackRecovery);
    List.add(S);
    S := 'modifier.hitrecovery=' + IntToStr(Modifier.HitRecovery);
    List.add(S);
    S := 'modifier.perception=' + IntToStr(Modifier.Perception);
    List.add(S);
    S := 'modifier.charm=' + IntToStr(Modifier.Charm);
    List.add(S);
    S := 'modifier.healingrate=' + IntToStr(Modifier.HealingRate);
    List.add(S);
    S := 'modifier.rechargerate=' + IntToStr(Modifier.RechargeRate);
    List.add(S);
    S := 'modifier.hitpoints=' + IntToStr(Modifier.HitPoints);
    List.add(S);
    S := 'modifier.mana=' + IntToStr(Modifier.Mana);
    List.add(S);
    // Resistance
    SaveResistanceProps(List, Resistance);
    // Damage
    SaveDamageProps(List, Damage);
    S := 'slot=';
    if slLeg1 in SlotsAllowed then
      S := S + '[Leg1]';
    if slBoot in SlotsAllowed then
      S := S + '[Boot]';
    if slLeg2 in SlotsAllowed then
      S := S + '[Leg2]';
    if slChest1 in SlotsAllowed then
      S := S + '[Chest1]';
    if slChest2 in SlotsAllowed then
      S := S + '[Chest2]';
    if slArm in SlotsAllowed then
      S := S + '[Arm]';
    if slBelt in SlotsAllowed then
      S := S + '[Belt]';
    if slChest3 in SlotsAllowed then
      S := S + '[Chest3]';
    if slGauntlet in SlotsAllowed then
      S := S + '[Gauntlet]';
    if slOuter in SlotsAllowed then
      S := S + '[Outer]';
    if slHelmet in SlotsAllowed then
      S := S + '[Helmet]';
    if slWeapon in SlotsAllowed then
      S := S + '[Weapon]';
    if slShield in SlotsAllowed then
      S := S + '[Shield]';
    if sltabar in SlotsAllowed then
      S := S + '[tabar]';
    if slMisc1 in SlotsAllowed then
      S := S + '[Misc1]';
    if slMisc2 in SlotsAllowed then
      S := S + '[Misc2]';
    if slMisc3 in SlotsAllowed then
      S := S + '[Misc3]';
    List.add(S);
    inherited;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TItem.SetProperty(const Name, Value: string);
var
  S: string;
  NoProp: boolean;
  L: longint;
const
  FailName: string = 'TItem.SetProperty';
begin
  log.DebugLog(FailName);
  try

    NoProp := false;
    S := LowerCase(Name);
    L := length(S);
    case L of
      4:
        begin
          if S = 'invx' then
            InvX := StrToInt(Value)
          else if S = 'invy' then
            InvY := StrToInt(Value)
          else if S = 'invw' then
            InvW := StrToInt(Value)
          else if S = 'invh' then
            InvH := StrToInt(Value)
          else if S = 'slot' then
          begin
            S := Value.ToLower;
            SlotsAllowed := [];
            if S.Contains('[leg1]') then
              SlotsAllowed := SlotsAllowed + [slLeg1];
            if S.Contains('[boot]') then
              SlotsAllowed := SlotsAllowed + [slBoot];
            if S.Contains('[leg2]') then
              SlotsAllowed := SlotsAllowed + [slLeg2];
            if S.Contains('[chest1]') then
              SlotsAllowed := SlotsAllowed + [slChest1];
            if S.Contains('[chest2]') then
              SlotsAllowed := SlotsAllowed + [slChest2];
            if S.Contains('[arm]') then
              SlotsAllowed := SlotsAllowed + [slArm];
            if S.Contains('[belt]') then
              SlotsAllowed := SlotsAllowed + [slBelt];
            if S.Contains('[chest3]') then
              SlotsAllowed := SlotsAllowed + [slChest3];
            if S.Contains('[gauntlet]') then
              SlotsAllowed := SlotsAllowed + [slGauntlet];
            if S.Contains('[outer]') then
              SlotsAllowed := SlotsAllowed + [slOuter];
            if S.Contains('[helmet]') then
              SlotsAllowed := SlotsAllowed + [slHelmet];
            if S.Contains('[weapon]') then
              SlotsAllowed := SlotsAllowed + [slWeapon];
            if S.Contains('[shield]') then
              SlotsAllowed := SlotsAllowed + [slShield];
            if S.Contains('[tabar]') then
              SlotsAllowed := SlotsAllowed + [sltabar];
            if S.Contains('[misc1]') then
              SlotsAllowed := SlotsAllowed + [slMisc1];
            if S.Contains('[misc2]') then
              SlotsAllowed := SlotsAllowed + [slMisc2];
            if S.Contains('[misc3]') then
              SlotsAllowed := SlotsAllowed + [slMisc3];
          end
          else
            NoProp := true;
        end;
      5:
        begin
          if S = 'magic' then
            Magic := StrToInt(Value)
          else if S = 'value' then
            Self.Value := StrToInt(Value)
          else if S = 'title' then
            Title := Value
          else
            NoProp := true;
        end;
      6:
        begin
          if S = 'weight' then
            Weight := StrToInt(Value)
          else
            NoProp := true;
        end;
      8:
        begin
          if S = 'itemname' then
            ItemName := Value
          else if S = 'iteminfo' then
            ItemInfo := Value
          else if S = 'partname' then
            PartName := Value
          else if S = 'onpickup' then
          begin
            if not LoadingFromSaveFile then
              OnPickUp := Value;
          end
          else
            NoProp := true;
        end;
      10:
        begin
          if S = 'layerindex' then
            LayerIndex := StrToInt(Value)
          else if S = 'identified' then
            Identified := (LowerCase(Value) = 'true')
          else if S = 'secretname' then
            SecretName := Value
          else if S = 'secretinfo' then
            SecretInfo := Value
          else
            NoProp := true;
        end;
      11:
        begin
          if S = 'displayname' then
            DisplayName := Value
          else
            NoProp := true;
        end;
      12:
        begin
          if S = 'layeredimage' then
            LayeredImage := Value
          else
            NoProp := true;
        end;
      13:
        begin
          if S = 'modifier.mana' then
            Modifier.Mana := StrToInt(Value)
          else
            NoProp := true;
        end;
      14:
        begin
          if S = 'inventoryimage' then
            InventoryImage := Value
          else if S = 'modifier.charm' then
            Modifier.Charm := StrToInt(Value)
          else
            NoProp := true;
        end;
      15:
        begin
          if S = 'modifier.combat' then
            Modifier.Combat := StrToInt(Value)
          else
            NoProp := true;
        end;
      16:
        begin
          if S = 'modifier.stealth' then
            Modifier.Stealth := StrToInt(Value)
          else
            NoProp := true;
        end;
      17:
        begin
          if S = 'modifier.strength' then
            Modifier.Strength := StrToInt(Value)
          else
            NoProp := true;
        end;
      18:
        begin
          if S = 'modifier.mysticism' then
            Modifier.Mysticism := StrToInt(Value)
          else if S = 'modifier.hitpoints' then
            Modifier.HitPoints := StrToInt(Value)
          else
            NoProp := true;
        end;
      19:
        begin
          if S = 'modifier.perception' then
            Modifier.Perception := StrToInt(Value)
          else
            NoProp := true;
        end;
      20:
        begin
          if S = 'modifier.restriction' then
            Modifier.Restriction := StrToInt(Value)
          else if S = 'modifier.hitrecovery' then
            Modifier.HitRecovery := StrToInt(Value)
          else if S = 'modifier.healingrate' then
            Modifier.HealingRate := StrToInt(Value)
          else
            NoProp := true;
        end;
      21:
        begin
          if S = 'modifier.coordination' then
            Modifier.Coordination := StrToInt(Value)
          else if S = 'modifier.constitution' then
            Modifier.Constitution := StrToInt(Value)
          else if S = 'modifier.rechargerate' then
            Modifier.RechargeRate := StrToInt(Value)
          else
            NoProp := true;
        end;
      23:
        begin
          if S = 'modifier.attackrecovery' then
            Modifier.AttackRecovery := StrToInt(Value)
          else
            NoProp := true;
        end
    else
      NoProp := true;
    end;

    if NoProp then
    begin
      if SetResistanceProps(S, Value, Resistance) then
      else if SetDamageProps(S, Value, Damage) then
      else
        inherited;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TItem.SetResource(const Value: TAniResource);
const
  FailName: string = 'TItem.SetResource';
begin
  log.DebugLog(FailName);
  try

    inherited;

    Self.MouseRect := Rect(CenterX - Radius, CenterY - Radius, CenterX + Radius,
      CenterY + Radius);

    if assigned(Resource) and (Resource is TLayerResource) then
      Frame := TLayerResource(Resource).ItemFrame + 1;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TItem.ShouldSave: boolean;
begin
  result := Enabled;
end;

{ TLauncher }

procedure TLauncher.DoDamage(Source, Target: TCharacter);
var
  NewProjectile: TProjectile;
const
  FailName: string = 'TLauncher.DoDamage';
begin
  log.DebugLog(FailName);
  try

    NewProjectile := TProjectile(Sprites.NewSprite(TProjectile, nil, Source.X,
      Source.Y, Source.Height div 2 - 16, 1));
    if assigned(NewProjectile) then
    begin
      NewProjectile.Damage := Source.Damage;
      NewProjectile.HitIncidental := 0;
      NewProjectile.HitTarget := 1;
      NewProjectile.Launch(Source, Target, Source.FTargetX, Source.FTargetY);
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

{ TAbstractObject }

constructor TAbstractObject.Create;
begin
  inherited Create(X, Y, Z, 0, false);
end;

//{ TGameObject }
//
//constructor TGameObject.Create(X, Y, Z: longint; Frame: Word; Enabled: boolean);
//const
//  FailName: string = 'TGameObject.Create';
//begin
//  log.DebugLog(FailName);
//  try
//
//    inherited Create(X, Y, Z, Frame, Enabled);
//    FProperties := TStringList.Create;
//    FProperties.Duplicates := dupIgnore;
//
//  except
//    on E: Exception do
//      log.log(FailName, E.Message, []);
//  end;
//end;
//
//destructor TGameObject.Destroy;
//const
//  FailName: string = 'TGameObject.Destroy';
//begin
//  log.DebugLog(FailName);
//  try
//
//    FProperties.free;
//    inherited;
//
//  except
//    on E: Exception do
//      log.log(FailName, E.Message, []);
//  end;
//end;
//
//function TGameObject.GetProperty(const Name: string): string;
//var
//  S: string;
//const
//  FailName: string = 'TGameObject.GetProperty';
//begin
//  log.DebugLog(FailName);
//  try
//
//    S := LowerCase(Name);
//    if S = 'guid' then
//      result := GUID
//    else if S = 'groupname' then
//      result := GroupName
//    else if S = 'loadcount' then
//      result := IntToStr(LoadCount)
//    else if S = 'onload' then
//      result := OnLoad
//    else
//      result := FProperties.Values[Name];
//
//  except
//    on E: Exception do
//      log.log(FailName, E.Message, []);
//  end;
//end;
//
//procedure TGameObject.Init;
//begin
//
//end;
//
//procedure TGameObject.LoadProperties(const List: TStringList);
//var
//  S, S0, S1: string;
//  StrTmp: string;
//  i, j, L: Integer;
//  INI: TINIFile;
//const
//  FailName: string = 'TGameObject.LoadProperties';
//begin
//  log.DebugLog(FailName);
//  try
//
//    Loading := true;
//    INI := nil;
//    try
//      FProperties.Clear;
//      for i := 0 to List.Count - 1 do
//      begin
//        S := List.strings[i];
//        j := Pos('=', S);
//        if (j > 0) { and (j<length(S)) } then
//        begin // This caused blank properties not to overwrite defaults
//          if (j < length(S)) and (S[j + 1] = '#') then
//          begin
//            if not assigned(INI) then
//              INI := TINIFile.Create(MapPath + 'symbols.ini');
//            S0 := copy(S, j + 1, length(S) - j);
//            S1 := Parse(S0, 1, '#');
//            StrTmp := S1;
//            L := length(S1);
//            S1 := INI.ReadString(Parse(S1, 0, '.'), Parse(S1, 1, '.'),
//              '#' + S1);
//            if StrTmp = S1 then
//              log.log('** BadName: ' + S1);
//            SetProperty(copy(S, 1, j - 1),
//              S1 + copy(S0, L + 3, length(S0) - L - 2));
//          end
//          else
//          begin
//            SetProperty(copy(S, 1, j - 1), copy(S, j + 1, length(S) - j));
//          end;
//        end;
//      end;
//    finally
//      Loading := false;
//      INI.free;
//    end;
//
//  except
//    on E: Exception do
//      log.log(FailName, E.Message, []);
//  end;
//end;
//
//function TGameObject.PropertyExists(const Name: string): boolean;
//const
//  FailName: string = 'TGameObject.PropertyExists';
//begin
//  result := false;
//
//  log.DebugLog(FailName);
//  try
//
//    result := FProperties.IndexOfName(Name) >= 0;
//
//  except
//    on E: Exception do
//      log.log(FailName, E.Message, []);
//  end;
//end;
//
//procedure TGameObject.SaveProperties(List: TStringList);
//var
//  i: Integer;
//  S: string;
//const
//  FailName: string = 'TGameObject.SaveProperties';
//begin
//  log.DebugLog(FailName);
//  try
//
//    S := 'guid=' + GUID;
//    List.add(S);
//    S := 'groupname=' + GroupName;
//    List.add(S);
//    S := 'onload=' + OnLoad;
//    List.add(S);
//    S := 'loadcount=' + IntToStr(LoadCount);
//    List.add(S);
//    S := 'position=' + IntToStr(X) + ',' + IntToStr(Y) + ',' + IntToStr(Z);
//    List.add(S);
//    for i := 0 to FProperties.Count - 1 do
//    begin
//      List.add(FProperties.strings[i]);
//    end;
//
//  except
//    on E: Exception do
//      log.log(FailName, E.Message, []);
//  end;
//end;
//
//procedure TGameObject.SetProperty(const Name, Value: string);
//var
//  S: string;
//  L: Integer;
//  NoProp: boolean;
//const
//  FailName: string = 'TGameObject.SetProperty';
//begin
//  log.DebugLog(FailName);
//  try
//
//    NoProp := false;
//    S := LowerCase(Name);
//    L := length(S);
//    case L of
//      4:
//        begin
//          if S = 'guid' then
//            GUID := Value
//          else
//            NoProp := true;
//        end;
//      6:
//        begin
//          if S = 'onload' then
//          begin
//            if not LoadingFromSaveFile then
//              OnLoad := Value;
//          end
//          else
//            NoProp := true;
//        end;
//      8:
//        begin
//          if S = 'position' then
//          begin
//            FX := StrToIntDef(Parse(Value, 0, ','), 0);
//            FY := StrToIntDef(Parse(Value, 1, ','), 0);
//            FZ := StrToIntDef(Parse(Value, 2, ','), 0);
//          end
//          else
//            NoProp := true;
//        end;
//      9:
//        begin
//          if S = 'groupname' then
//            GroupName := Value
//          else if S = 'loadcount' then
//            LoadCount := StrToInt(Value)
//          else
//            NoProp := true;
//        end
//    else
//      begin
//        NoProp := true;
//      end;
//    end;
//
//    if NoProp then
//      FProperties.Values[Name] := Value;
//
//  except
//    on E: Exception do
//      log.log(FailName, E.Message, []);
//  end;
//end;
//
//procedure TGameObject.DoLoad;
//var
//  event: string;
//const
//  FailName: string = 'TGameObject.DoLoad';
//begin
//  log.DebugLog(FailName);
//  try
//
//    Inc(LoadCount);
//    event := 'OnLoad[' + IntToStr(LoadCount) + ']';
//    if PropertyExists(event) then
//      RunScript(Self, Properties[event])
//    else
//      RunScript(Self, OnLoad);
//
//  except
//    on E: Exception do
//      log.log(FailName, E.Message, []);
//  end;
//end;

//{ TSpriteObject }
//
//procedure TSpriteObject.Activate;
//var
//  event: string;
//const
//  FailName: string = 'TSpriteObject.Activate';
//begin
//  log.DebugLog(FailName);
//  try
//
//    Inc(ActivateCount);
//    event := 'OnActivate[' + IntToStr(ActivateCount) + ']';
//    if PropertyExists(event) then
//      RunScript(Self, Properties[event])
//    else
//      RunScript(Self, OnActivate);
//
//  except
//    on E: Exception do
//      log.log(FailName, E.Message, []);
//  end;
//end;
//
//constructor TSpriteObject.Create(X, Y, Z: longint; Frame: Word;
//  Enabled: boolean);
//const
//  FailName: string = 'TSpriteObject.Create';
//begin
//  log.DebugLog(FailName);
//  try
//
//    inherited Create(X, Y, Z, Frame, Enabled);
//    FFacing := fSS;
//
//  except
//    on E: Exception do
//      log.log(FailName, E.Message, []);
//  end;
//end;
//
//destructor TSpriteObject.Destroy;
//const
//  FailName: string = 'TSpriteObject.Destroy';
//begin
//  log.DebugLog(FailName);
//  try
//
//    if assigned(MsgImage) then
//      MsgImage := nil;
//
//    inherited;
//
//  except
//    on E: Exception do
//      log.log(FailName, E.Message, []);
//  end;
//end;
//
//function TSpriteObject.DoAction(const Action: string): boolean;
//var
//  S: string;
//  Script: TScript;
//const
//  FailName: string = 'TSpriteObject.DoAction';
//begin
//  result := false;
//
//  log.DebugLog(FailName);
//  try
//
//    if not assigned(Resource) then
//    begin
//      result := false;
//      exit;
//    end;
//    result := true;
//    Delay := -1;
//    S := LowerCase(Action);
//    if S = 'activate' then
//      Activate
//    else
//    begin
//      S := Action + Facing.ToString;
//      Script := Resource.Script[S];
//      if assigned(Script) then
//      begin
//        if Script.tag = scrLoop then
//          PlayScript(S, 1, smRepeat)
//        else if Script.tag = scrRandom then
//          PlayScript(S, 1, smRandom)
//        else
//          PlayScript(S, 1, smOnce);
//      end
//      else
//      begin
//        Script := Resource.Script[Action];
//        if assigned(Script) then
//        begin
//          if Script.tag = scrLoop then
//            PlayScript(Action, 1, smRepeat)
//          else if Script.tag = scrRandom then
//            PlayScript(Action, 1, smRandom)
//          else
//            PlayScript(Action, 1, smOnce);
//        end
//        else
//        begin
//          result := false;
//        end;
//      end;
//    end;
//
//  except
//    on E: Exception do
//      log.log(FailName, E.Message, []);
//  end;
//end;
//
//function TSpriteObject.GetProperty(const Name: string): string;
//var
//  S: string;
//const
//  FailName: string = 'TSpriteObject.GetProperty';
//begin
//  log.DebugLog(FailName);
//  try
//
//    S := LowerCase(Name);
//    if S = 'onactivate' then
//      result := OnActivate
//    else if S = 'facing' then
//      result := Facing.ToString
//    else if S = 'enabled' then
//      result := FmtBool(Enabled)
//    else if S = 'unmoveable' then
//      result := FmtBool(UnMoveable)
//    else if S = 'oncollide' then
//      result := OnCollide
//    else if S = 'activatecount' then
//      result := IntToStr(ActivateCount)
//    else if S = 'collidecount' then
//      result := IntToStr(CollideCount)
//    else
//      result := inherited GetProperty(Name);
//
//  except
//    on E: Exception do
//      log.log(FailName, E.Message, []);
//  end;
//end;
//
//procedure TSpriteObject.SetProperty(const Name: string; const Value: string);
//var
//  S, S1: string;
//  i, j, k, L: longint;
//  NoProp: boolean;
//const
//  FailName: string = 'TSpriteObject.SetProperty';
//begin
//  log.DebugLog(FailName);
//  try
//
//    NoProp := false;
//    S := LowerCase(Name);
//    L := length(S);
//    case L of
//      5:
//        begin
//          if S = 'frame' then
//            Frame := StrToInt(Value)
//          else
//            NoProp := true;
//        end;
//      6:
//        begin
//          if S = 'facing' then
//          begin
//            S1 := LowerCase(Value);
//            if (S1 = 'se') then
//              FFacing := fSE
//            else if (S1 = 'ee') then
//              FFacing := fEE
//            else if (S1 = 'ne') then
//              FFacing := fNE
//            else if (S1 = 'nn') then
//              FFacing := fNN
//            else if (S1 = 'nw') then
//              FFacing := fNW
//            else if (S1 = 'ww') then
//              FFacing := fWW
//            else if (S1 = 'sw') then
//              FFacing := fSW
//            else
//              FFacing := fSS;
//          end
//          else
//            NoProp := true;
//        end;
//      7:
//        begin
//          if S = 'enabled' then
//            Enabled := (LowerCase(Value) = 'true')
//          else
//            NoProp := true;
//        end;
//      8:
//        begin
//          if S = 'resource' then
//          else if S = 'position' then
//          begin
//            i := StrToIntDef(Parse(Value, 0, ','), 0);
//            j := StrToIntDef(Parse(Value, 1, ','), 0);
//            k := StrToIntDef(Parse(Value, 2, ','), 0);
//            SetPos(i, j, k);
//          end
//          else
//            NoProp := true;
//        end;
//      9:
//        begin
//          if S = 'oncollide' then
//          begin
//            if not LoadingFromSaveFile then
//              OnCollide := Value;
//          end
//          else
//            NoProp := true;
//        end;
//      10:
//        begin
//          if S = 'onactivate' then
//          begin
//            if not LoadingFromSaveFile then
//              OnActivate := Value;
//          end
//          else if S = 'unmoveable' then
//            UnMoveable := (LowerCase(Value) = 'true')
//          else
//            NoProp := true;
//        end;
//      12:
//        begin
//          if S = 'collidecount' then
//            CollideCount := StrToInt(Value)
//          else
//            NoProp := true;
//        end;
//      13:
//        begin
//          if S = 'activatecount' then
//            ActivateCount := StrToInt(Value)
//          else
//            NoProp := true;
//        end
//    else
//      begin
//        NoProp := true;
//      end;
//    end;
//
//    if NoProp then
//      inherited;
//
//  except
//    on E: Exception do
//      log.log(FailName, E.Message, []);
//  end;
//end;
//
//procedure TSpriteObject.SetResource(const Value: TAniResource);
//const
//  FailName: string = 'TSpriteObject.SetResource';
//begin
//  log.DebugLog(FailName);
//  try
//
//    inherited;
//    if Value is TResource then
//    begin
//      with Value as TResource do
//      begin
//        Self.Width := FrameWidth;
//        Self.Height := FrameHeight;
//        Self.FrameMultiplier := FrameMultiplier;
//        Self.CenterX := CenterX;
//        Self.CenterY := CenterY;
//        Self.Radius := Radius;
//        Self.Speed := Speed;
//        Self.Alpha := Alpha;
//        Self.SpecialEffect := SpecialEffect;
//        Self.MaskHeight := FrameHeight;
//        Self.UseLighting := UseLighting;
//        Self.Highlightable := Highlightable;
//        if FrameHeight >= 100 then
//          Self.MouseRect := Rect(CenterX - Radius, 20, CenterX + Radius,
//            FrameHeight - 20)
//        else
//          Self.MouseRect := Rect(CenterX - Radius, 0, CenterX + Radius,
//            FrameHeight);
//      end;
//    end;
//
//  except
//    on E: Exception do
//      log.log(FailName, E.Message, []);
//  end;
//end;
//
//function TSpriteObject.ActionExists(const Action: string): boolean;
//var
//  Script: TScript;
//const
//  FailName: string = 'TSpriteObject.ActionExists';
//begin
//  result := false;
//
//  log.DebugLog(FailName);
//  try
//
//    if not assigned(Resource) then
//    begin
//      result := false;
//      exit;
//    end;
//    Script := Resource.Script[Action + Facing.ToString];
//    if not assigned(Script) then
//      Script := Resource.Script[Action];
//    result := assigned(Script);
//
//  except
//    on E: Exception do
//      log.log(FailName, E.Message, []);
//  end;
//end;
//
//procedure TSpriteObject.Say(const Msg: string; Color: TColor);
//var
//  BM: TBitmap;
//  R: TRect;
//  i: Integer;
//const
//  FailName: string = 'TSpriteObject.Say';
//begin
//  // TODO: Cleanup
//  log.DebugLog(FailName);
//  try
//
//    if Msg = '' then
//    begin
//      MsgDuration := 0;
//      if MsgDuration = 0 then
//      begin
//        MsgImage := nil;
//        i := SayList.IndexOf(Self);
//        if i >= 0 then
//          SayList.Delete(i);
//      end;
//      exit;
//    end;
//    MsgDuration := 100;
//    BM := TBitmap.Create;
//    R := Rect(0, 0, Width * 2, 0);
//    DrawText(BM.Canvas.Handle, PChar(Msg), -1, R, DT_CALCRECT or DT_CENTER or
//      DT_NOCLIP or DT_NOPREFIX or DT_WORDBREAK);
//    MsgWidth := R.Right;
//    MsgHeight := R.Bottom;
//    BM.Width := MsgWidth;
//    BM.Height := MsgHeight;
//    SetTextColor(BM.Canvas.Handle, ColorToRGB(Color));
//    SetBkMode(BM.Canvas.Handle, TRANSPARENT);
//    PatBlt(BM.Canvas.Handle, 0, 0, MsgWidth, MsgHeight, BLACKNESS);
//    DrawText(BM.Canvas.Handle, PChar(Msg), -1, R, DT_CENTER or DT_NOCLIP or
//      DT_NOPREFIX or DT_WORDBREAK);
//    MsgImage := SoAOS_DX_SurfaceFromBMP(BM, clBlack);
//    BM.free;
//    SayList.Insert(0, Self);
//
//  except
//    on E: Exception do
//      log.log(FailName, E.Message, []);
//  end;
//end;
//
//procedure TSpriteObject.UpdateSay;
//var
//  X0, i: Integer;
//  // pr : TRect;
//const
//  FailName: string = 'TSpriteObject.UpdateSay';
//begin
//  log.DebugLog(FailName);
//  try
//
//    inherited;
//    if MsgDuration > 0 then
//    begin
//      X0 := PosX + CenterX - MsgWidth div 2;
//      // Windows 8/10 Fix, Crash at Mapedges appearently - from gondur branch
//      DrawAlpha(lpDDSBack, Rect(X0, PosY - MsgHeight, X0 + MsgWidth, PosY),
//        Rect(0, 0, MsgWidth, MsgHeight), MsgImage, true, 255);
//      // pr := Rect( 0, 0, MsgWidth, MsgHeight );
//      // lpDDSBack.BltFast( X0, PosY - MsgHeight, MsgImage, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
//      Dec(MsgDuration);
//
//      if MsgDuration = 0 then
//      begin
//        MsgImage := nil;
//        i := SayList.IndexOf(Self);
//        if i >= 0 then
//          SayList.Delete(i);
//      end;
//    end;
//
//  except
//    on E: Exception do
//      log.log(FailName, E.Message, []);
//  end;
//end;
//
//procedure TSpriteObject.SetFacing(const Value: TFacing);
//const
//  FailName: string = 'TSpriteObject.SetFacing';
//begin
//  log.DebugLog(FailName);
//  try
//
//    FFacing := Value;
//
//  except
//    on E: Exception do
//      log.log(FailName, E.Message, []);
//  end;
//end;
//
//procedure TSpriteObject.SaveProperties(List: TStringList);
//var
//  S: string;
//const
//  FailName: string = 'TSpriteObject.SaveProperties';
//begin
//  log.DebugLog(FailName);
//  try
//
//    S := 'enabled=' + FmtBool(Enabled);
//    List.add(S);
//    S := 'onactivate=' + OnActivate;
//    List.add(S);
//    S := 'facing=' + Facing.ToString;
//    List.add(S);
//    S := 'frame=' + IntToStr(Frame);
//    List.add(S);
//    S := 'unmoveable=' + FmtBool(UnMoveable);
//    List.add(S);
//    S := 'oncollide=' + OnCollide;
//    List.add(S);
//    S := 'activatecount=' + IntToStr(ActivateCount);
//    List.add(S);
//    S := 'collidecount=' + IntToStr(CollideCount);
//    List.add(S);
//    if assigned(Resource) then
//    begin
//      S := 'resource=' + TResource(Resource).FileName;
//      List.add(S);
//    end;
//    inherited;
//
//  except
//    on E: Exception do
//      log.log(FailName, E.Message, []);
//  end;
//end;
//
//procedure TSpriteObject.Init;
//const
//  FailName: string = 'TSpriteObject.Init';
//begin
//  log.DebugLog(FailName);
//  try
//
//    DoAction('Default');
//
//  except
//    on E: Exception do
//      log.log(FailName, E.Message, []);
//  end;
//end;
//
//function TSpriteObject.GetName: string;
//const
//  FailName: string = 'TSpriteObject.GetName';
//begin
//  log.DebugLog(FailName);
//  try
//
//    result := '';
//
//  except
//    on E: Exception do
//      log.log(FailName, E.Message, []);
//  end;
//end;
//
//function TSpriteObject.ShouldSave: boolean;
//begin
//  // result:=Enabled;
//  result := true;
//end;

{ TBow }

constructor TBow.Create(X, Y, Z: Integer; Frame: Word; Enabled: boolean);
begin
  inherited;
end;

procedure TBow.DoDamage(Source, Target: TCharacter);
var
  NewProjectile: TProjectile;
  R: Integer;
  D, SinT, CosT: Single;
  X1, Y1, X2, Y2: Integer;
  Z1, Z2: Integer;
  ToHit: Single;
  TargetX, TargetY: longint;
  Angle: Single;
  Roll, F: Single;
const
  FailName: string = 'TBow.DoDamage';
begin
  log.DebugLog(FailName);
  try
    if MinStrength = 0 then
      exit;
    if not(Source.FEquipment[slBelt] is TQuiver) then
      exit;
    Z1 := Source.Height div 2 - 16;
    if assigned(Target) then
      Z2 := 0
    else
      Z2 := Z1;
    TargetX := Source.FTargetX;
    TargetY := Source.FTargetY + Z2;

    D := sqrt(sqr(TargetX - Source.X) + sqr(2 * (TargetY - Source.Y)));
    if D = 0 then
      exit;
    NewProjectile := TProjectile(Sprites.NewSprite(TArrow, nil, Source.X,
      Source.Y, Z1, 0));
    if assigned(NewProjectile) then
    begin
      if assigned(Source.FTarget) then
      begin
        if TCharacter(Source.FTarget).Track = Source then
          F := 1
        else
        begin
          F := Source.Stealth / 10;
          if F < 1 then
            F := 1;
        end;
      end
      else
      begin
        F := 1;
      end;

      Roll := random(40);
      ToHit := Source.BowBonus;
      if Roll > 35 then
      begin
        if (Source = Current) then
          F := F * 2;
        NewProjectile.Critical := true;
      end
      else
      begin
        if ToHit < 1 then
          Angle := 10
        else
          Angle := 175 / ToHit;
        if Angle > 10 then
          Angle := 10;
        ComputeTrajectory(Source, TargetX, TargetY, Angle);
      end;

      if F < 1 then
        F := 1;
      NewProjectile.DamageFactor := F;
      NewProjectile.Width := 64;
      NewProjectile.Height := 32;
      NewProjectile.CenterX := NewProjectile.Width div 2;
      NewProjectile.CenterY := NewProjectile.Height div 2;
      NewProjectile.UseLighting := true;
      NewProjectile.UseLineOfSight := true;
      NewProjectile.Damage := Source.Damage;
      NewProjectile.Radius := 1;
      NewProjectile.Speed := MinStrength * 2.5;
      NewProjectile.UseStealth := true;

      if ToHit <= 10 then
        NewProjectile.HitIncidental := 1
      else
        NewProjectile.HitIncidental := 10 / ToHit;
      NewProjectile.HitTarget := 1;

      if NewProjectile.Speed < 1 then
        NewProjectile.Speed := 1;
      NewProjectile.Duration :=
        Trunc((Range * MinStrength / 10) / NewProjectile.Speed) + 1;
      NewProjectile.TrackingDegree := TQuiver(Source.FEquipment[slBelt])
        .TrackingDegree;
      TArrow(NewProjectile).FletchingColor := TQuiver(Source.FEquipment[slBelt])
        .FletchingColor;
      R := (TArrow(NewProjectile).Width - 4) div 2;
      SinT := (TargetY - Source.Y) / D;
      CosT := (TargetX - Source.X) / D;
      X2 := round(R * CosT);
      Y2 := round(R * SinT);
      X1 := NewProjectile.Height - X2;
      Y1 := NewProjectile.Height div 2 - Y2;
      Inc(X2, NewProjectile.Height);
      Inc(Y2, NewProjectile.Height div 2);
      TArrow(NewProjectile).BM := TBitmap.Create;
      TArrow(NewProjectile).BM.Width := NewProjectile.Width;
      TArrow(NewProjectile).BM.Height := NewProjectile.Height;
      TArrow(NewProjectile).RLE := TRLESprite.Create;
      TArrow(NewProjectile).Draw(X1, Y1, X2, Y2, R, SinT, CosT);

      if length(TQuiver(Source.FEquipment[slBelt]).StrikeLeatherSounds) > 0 then
        TArrow(NewProjectile).StrikeLeatherSound :=
          TQuiver(Source.FEquipment[slBelt]).StrikeLeatherSounds
          [random(length(TQuiver(Source.FEquipment[slBelt])
          .StrikeLeatherSounds))];
      if length(TQuiver(Source.FEquipment[slBelt]).StrikeMetalSounds) > 0 then
        TArrow(NewProjectile).StrikeMetalSound :=
          TQuiver(Source.FEquipment[slBelt]).StrikeMetalSounds
          [random(length(TQuiver(Source.FEquipment[slBelt])
          .StrikeMetalSounds))];
      if length(TQuiver(Source.FEquipment[slBelt]).StrikeStoneSounds) > 0 then
        TArrow(NewProjectile).StrikeWallSound :=
          TQuiver(Source.FEquipment[slBelt]).StrikeStoneSounds
          [random(length(TQuiver(Source.FEquipment[slBelt])
          .StrikeStoneSounds))];
      NewProjectile.Launch(Source, Target, TargetX, TargetY);
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TBow.GetDamage(Character: TCharacter);
var
  D, F: Single;
const
  FailName: string = 'TBow.GetDamage';
begin
  log.DebugLog(FailName);
  try

    F := MinStrength / 10;
    if (Character.Coordination < MinCoordination) and (MinCoordination > 0) then
    begin
      D := Character.Coordination / MinCoordination;
      F := F * D;
    end;
    if (Character.Restriction > MaxRestriction) and (MaxRestriction >= 0) then
    begin
      D := 1 / (1 + Character.Restriction - MaxRestriction);
      F := F * D;
    end;

    Character.Damage := Damage;
    Character.Damage.Piercing.Min := Character.Damage.Piercing.Min * F;
    Character.Damage.Piercing.Max := Character.Damage.Piercing.Max * F;
    Character.Damage.Crushing.Min := Character.Damage.Crushing.Min * F;
    Character.Damage.Crushing.Max := Character.Damage.Crushing.Max * F;
    Character.Damage.Cutting.Min := Character.Damage.Cutting.Min * F;
    Character.Damage.Cutting.Max := Character.Damage.Cutting.Max * F;
    Character.Damage.Stun.Min := Character.Damage.Stun.Min * F;
    Character.Damage.Stun.Max := Character.Damage.Stun.Max * F;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

{ TQuiver }

procedure TQuiver.Clone(var NewObject: TObject; NewGUID: string);
begin
  if not assigned(NewObject) then
    NewObject := TQuiver.Create(X, Y, Z, 1, false);

  TQuiver(NewObject).StrikeLeatherSound := FStrikeLeatherSound;
  TQuiver(NewObject).StrikeMetalSound := FStrikeMetalSound;
  TQuiver(NewObject).StrikeStoneSound := FStrikeStoneSound;
  TQuiver(NewObject).FletchingColor := FletchingColor;
  TQuiver(NewObject).TrackingDegree := TrackingDegree;

  inherited Clone(NewObject, NewGUID);
end;

destructor TQuiver.Destroy;
const
  FailName: string = 'TQuiver.Destroy';
begin
  log.DebugLog(FailName);
  try

    if assigned(SoundLib) then
    begin
      SoundLib.FreeSound(StrikeLeatherSounds);
      SoundLib.FreeSound(StrikeMetalSounds);
      SoundLib.FreeSound(StrikeStoneSounds);
    end;

    inherited;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TQuiver.Equip(Character: TCharacter);
const
  FailName: string = 'TQuiver.Equip';
begin
  log.DebugLog(FailName);
  try

    Character.ApplyModifier(@Modifier);
    Character.ApplyResistance(@Resistance);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TQuiver.GetProperty(const Name: string): string;
var
  S: string;
const
  FailName: string = 'TQuiver.GetProperty';
begin
  log.DebugLog(FailName);
  try

    S := LowerCase(Name);
    if S = 'fletchingcolor' then
      result := IntToStr(FletchingColor)
    else if S = 'trackingdegree' then
      result := FormatFP(TrackingDegree)
    else if S = 'strikeleathersound' then
      result := StrikeLeatherSound
    else if S = 'strikemetalsound' then
      result := StrikeMetalSound
    else if S = 'strikestonesound' then
      result := StrikeStoneSound
    else
      result := inherited GetProperty(Name);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TQuiver.SaveProperties(List: TStringList);
var
  S: string;
const
  FailName: string = 'TQuiver.SaveProperties';
begin
  log.DebugLog(FailName);
  try

    S := 'fletchingcolor=' + IntToStr(FletchingColor);
    List.add(S);
    S := 'trackingdegree=' + FormatFP(TrackingDegree);
    List.add(S);
    S := 'strikeleathersound=' + StrikeLeatherSound;
    List.add(S);
    S := 'strikemetalsound=' + StrikeMetalSound;
    List.add(S);
    S := 'strikestonesound=' + StrikeStoneSound;
    List.add(S);
    inherited;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TQuiver.SetProperty(const Name, Value: string);
var
  S: string;
const
  FailName: string = 'TQuiver.SetProperty';
begin
  log.DebugLog(FailName);
  try

    S := LowerCase(Name);
    if S = 'fletchingcolor' then
      FletchingColor := StrToInt(Value)
    else if S = 'trackingdegree' then
      TrackingDegree := UnFormatFP(Value)
    else if S = 'strikeleathersound' then
      StrikeLeatherSound := Value
    else if S = 'strikemetalsound' then
      StrikeMetalSound := Value
    else if S = 'strikestonesound' then
      StrikeStoneSound := Value
    else
      inherited;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TQuiver.SetStrikeLeatherSound(const Value: string);
var
  SoundCount: Integer;
const
  FailName: string = 'TQuiver.SetStrikeLeatherSound';
begin
  log.DebugLog(FailName);
  try

    if assigned(SoundLib) then
    begin
      if assigned(StrikeLeatherSounds) then
        SoundLib.FreeSound(StrikeLeatherSounds);
      StrikeLeatherSounds := SoundLib.OpenSound(Value, 1, SoundCount);
    end;
    FStrikeLeatherSound := Value;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TQuiver.SetStrikeMetalSound(const Value: string);
var
  SoundCount: Integer;
const
  FailName: string = 'TQuiver.SetStrikeMetalSound';
begin
  log.DebugLog(FailName);
  try

    if assigned(SoundLib) then
    begin
      if assigned(StrikeMetalSounds) then
        SoundLib.FreeSound(StrikeMetalSounds);
      StrikeMetalSounds := SoundLib.OpenSound(Value, 1, SoundCount);
    end;
    FStrikeMetalSound := Value;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TQuiver.SetStrikeStoneSound(const Value: string);
var
  SoundCount: Integer;
const
  FailName: string = 'TQuiver.SetStrikeStoneSound';
begin
  log.DebugLog(FailName);
  try

    if assigned(SoundLib) then
    begin
      if assigned(StrikeStoneSounds) then
        SoundLib.FreeSound(StrikeStoneSounds);
      StrikeStoneSounds := SoundLib.OpenSound(Value, 1, SoundCount);
    end;
    FStrikeStoneSound := Value;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

{ TTrigger }

destructor TTrigger.Destroy;
const
  FailName: string = 'TTrigger.Destroy';
begin
  log.DebugLog(FailName);
  try

    if assigned(SoundLib) then
    begin
      SoundLib.FreeSound(TriggerSounds);
    end;
    inherited;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TTrigger.Execute;
var
  event: string;
const
  FailName: string = 'TTrigger.Execute';
begin
  result := false;

  log.DebugLog(FailName);
  try

    Dec(CountDown);
    if CountDown > 0 then
      result := false
    else
    begin
      result := true;
      Inc(TriggerCount);
      event := 'OnTrigger[' + IntToStr(TriggerCount) + ']';
      if PropertyExists(event) then
        RunScript(Activator, Properties[event])
      else
        RunScript(Activator, OnTrigger);
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TTrigger.GetProperty(const Name: string): string;
var
  S: string;
const
  FailName: string = 'TTrigger.GetProperty';
begin
  log.DebugLog(FailName);
  try

    S := LowerCase(Name);
    if S = 'triggerenabled' then
      result := BoolToStr(TriggerEnabled, True)
    else if S = 'ontrigger' then
      result := OnTrigger
    else if S = 'triggerdelay' then
      result := IntToStr(TriggerDelay)
    else if S = 'triggercount' then
      result := IntToStr(TriggerCount)
    else if S = 'triggersound' then
      result := TriggerSound
    else
      result := inherited GetProperty(Name);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TTrigger.SaveProperties(List: TStringList);
var
  S: string;
const
  FailName: string = 'TTrigger.SaveProperties';
begin
  log.DebugLog(FailName);
  try

    S := 'triggerenabled=' + BoolToStr(TriggerEnabled, True);
    List.add(S);
    S := 'ontrigger=' + OnTrigger;
    List.add(S);
    S := 'triggerdelay=' + IntToStr(TriggerDelay);
    List.add(S);
    S := 'triggercount=' + IntToStr(TriggerCount);
    List.add(S);
    S := 'triggersound=' + TriggerSound;
    List.add(S);
    inherited;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TTrigger.SetProperty(const Name, Value: string);
var
  S: string;
const
  FailName: string = 'TTrigger.SetProperty';
begin
  log.DebugLog(FailName);
  try

    S := LowerCase(Name);
    if S = 'triggerenabled' then
      TriggerEnabled := (LowerCase(Value) = 'true')
    else if S = 'ontrigger' then
    begin
      if not LoadingFromSaveFile then
        OnTrigger := Value;
    end
    else if S = 'triggerdelay' then
      TriggerDelay := StrToInt(Value)
    else if S = 'triggercount' then
      TriggerCount := StrToInt(Value)
    else if S = 'triggersound' then
      TriggerSound := Value
    else
      inherited;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TTrigger.SetTriggerSound(const Value: string);
var
  SoundCount: Integer;
const
  FailName: string = 'TTrigger.SetTriggerSound';
begin
  log.DebugLog(FailName);
  try

    if assigned(SoundLib) then
    begin
      if assigned(TriggerSounds) then
        SoundLib.FreeSound(TriggerSounds);
      TriggerSounds := SoundLib.OpenSound(Value, 1, SoundCount);
    end;
    FTriggerSound := Value;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TTrigger.Trigger(Character: TCharacter);
var
  event: string;
const
  FailName: string = 'TTrigger.Trigger';
begin
  log.DebugLog(FailName);
  try
    if not TriggerEnabled then
      exit;
    if Character.InterfaceLocked then
      exit;
    if CountDown > 0 then
      exit;
    if TriggerDelay = 0 then
    begin
      Inc(TriggerCount);
      event := 'OnTrigger[' + IntToStr(TriggerCount) + ']';
      if PropertyExists(event) then
        RunScript(Character, Properties[event])
      else
        RunScript(Character, OnTrigger);
    end
    else
    begin
      Activator := Character;
      CountDown := TriggerDelay;
      ActiveTriggers.add(Self);
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

{ TSoundPlayer }

constructor TSoundPlayer.Create(X, Y, Z: Integer);
const
  FailName: string = 'TSoundPlayer.Create';
begin
  log.DebugLog(FailName);
  try

    inherited;
    FSoundOn := true;
    FVolume := 80;
    PlaybackSpeed := 100;
    RandomInterval := true;
    Radius := 400;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

destructor TSoundPlayer.Destroy;
const
  FailName: string = 'TSoundPlayer.PlaySound';
begin
  log.DebugLog(FailName);
  try

    if assigned(SoundLib) then
    begin
      SoundLib.FreeSound(Sounds);
    end;
    inherited;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TSoundPlayer.Execute;
var
  D: Double;
  PanD: longint;
  Vol, Pan: Integer;

  function CalcVolPan: boolean;
  var
    AdjustedVolume: Integer;
    HearingRange: longint;
  begin
    result := false;
    if (MasterSoundVolume = 0) or (FVolume = 0) then
      exit;
    AdjustedVolume := (MasterSoundVolume * FVolume) div 100;
    if Ambient then
    begin
      Vol := AdjustedVolume;
      Pan := 0;
    end
    else if RndLocation then
    begin
      Vol := random(AdjustedVolume) + 1;
      Pan := random(20001) - 10000;
    end
    else
    begin
      HearingRange := Radius + (ScreenMetrics.HearingRange-400);
      if HearingRange <= 0 then
        exit;
      PanD := X - Current.X;
      D := sqrt(sqr(PanD) + 2 * sqr(Y - Current.Y));
      if D > HearingRange then
        exit;
      if D <= 1 then
      begin
        Vol := AdjustedVolume;
        Pan := 0;
      end
      else
      begin
        Vol := round(AdjustedVolume * (HearingRange - D) / HearingRange);
        Pan := (PanD * 10000) div HearingRange;
      end;
    end;
    result := true;
  end;

const
  FailName: string = 'TSoundPlayer.Execute';
begin
  log.DebugLog(FailName);
  try

    if not assigned(SoundLib) then
      exit;
    if not FSoundOn then
      exit;
    if FContinuous then
    begin
      if Playing then
      begin
        if CalcVolPan then
        begin
          SoundLib.SetSound(ContinuousIndex, PlaybackSpeed, Vol, Pan);
        end
        else
        begin
          SoundLib.StopSound(ContinuousIndex);
          Playing := false;
        end;
      end
      else
      begin
        if assigned(Sounds) then
        begin
          if CalcVolPan then
          begin
            ContinuousIndex := SoundLib.PlaySound(Sounds, 1, Vol, Pan,
              PlaybackSpeed);
            Playing := true;
          end;
        end;
      end;
    end
    else
    begin
      if IntervalCount <= 0 then
      begin
        if RandomInterval then
        begin
          if Interval <= 0 then
            IntervalCount := 45 - random(30)
          else
            IntervalCount := Interval + random(MaxInterval - Interval);
        end
        else
        begin
          if Interval <= 0 then
            IntervalCount := 30
          else
            IntervalCount := Interval;
        end;
      end
      else
      begin
        Dec(IntervalCount);
        if IntervalCount = 0 then
        begin
          if assigned(Sounds) then
          begin
            if CalcVolPan then
            begin
              SoundLib.PlaySound(Sounds, 0, Vol, Pan, PlaybackSpeed);
            end;
          end;
        end;
      end;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TSoundPlayer.GetProperty(const Name: string): string;
var
  S: string;
const
  FailName: string = 'TSoundPlayer.GetProperty';
begin
  log.DebugLog(FailName);
  try

    S := LowerCase(Name);
    if S = 'on' then
      result := BoolToStr(SoundOn, True)
    else if S = 'selfplay' then
    begin
      if RandomInterval then
        result := 'RndInterval'
      else
        result := 'FixedInterval';
    end
    else if S = 'volume' then
      result := IntToStr(Volume)
    else if S = 'intervalperiod' then
      result := IntToStr(Interval) + '-' + IntToStr(MaxInterval)
    else if S = 'useasambientfill' then
      result := BoolToStr(Ambient, True)
    else if S = 'audiofile' then
      result := FileName
    else if S = 'continuous' then
      result := BoolToStr(Continuous, True)
    else if S = 'rndlocation' then
      result := BoolToStr(RndLocation, True)
    else if S = 'playbackspeed' then
      result := IntToStr(PlaybackSpeed)
    else if S = 'radius' then
      result := IntToStr(Radius)
    else
      result := inherited GetProperty(Name);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TSoundPlayer.Init;
begin
  inherited;

  if NoMaxSpecified then
  begin
    if RandomInterval then
    begin
      MaxInterval := Interval + Interval div 2;
      Interval := Interval div 2;
    end;
    NoMaxSpecified := false;
  end;
end;

procedure TSoundPlayer.SaveProperties(List: TStringList);
var
  S: string;
const
  FailName: string = 'TSoundPlayer.SaveProperties';
begin
  log.DebugLog(FailName);
  try

    S := 'on=' + BoolToStr(SoundOn, True);
    List.add(S);
    if RandomInterval then
      S := 'selfplay=RndInterval'
    else
      S := 'selfplay=FixedInterval';
    List.add(S);
    S := 'volume=' + IntToStr(Volume);
    List.add(S);
    S := 'intervalperiod=' + IntToStr(Interval) + '-' + IntToStr(MaxInterval);
    List.add(S);
    S := 'useasambientfill=' + BoolToStr(Ambient, True);
    List.add(S);
    S := 'audiofile=' + FileName;
    List.add(S);
    S := 'continuous=' + BoolToStr(Continuous, True);
    List.add(S);
    S := 'rndlocation=' + BoolToStr(RndLocation, True);
    List.add(S);
    S := 'playbackspeed=' + IntToStr(PlaybackSpeed);
    List.add(S);
    S := 'radius=' + IntToStr(Radius);
    List.add(S);
    inherited;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TSoundPlayer.SetContinuous(const Value: boolean);
const
  FailName: string = 'TSoundPlayer.SetContinuous';
begin
  log.DebugLog(FailName);
  try

    FContinuous := Value;
    if FContinuous then
    begin
      Playing := false;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TSoundPlayer.SetFilename(const Value: string);
var
  SoundCount: Integer;
const
  FailName: string = 'TSoundPlayer.SetFilename';
begin
  log.DebugLog(FailName);
  try

    if assigned(SoundLib) then
    begin
      if assigned(Sounds) then
        SoundLib.FreeSound(Sounds);
      Sounds := SoundLib.OpenSound(Value, 1, SoundCount);
    end;
    FFileName := Value;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TSoundPlayer.SetInterval(const Value: Integer);
const
  FailName: string = 'TSoundPlayer.SetInterval';
begin
  log.DebugLog(FailName);
  try

    FInterval := Value;
    IntervalCount := random(Interval);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TSoundPlayer.SetProperty(const Name, Value: string);
var
  S: string;
const
  FailName: string = 'TSoundPlayer.SetProperty';
begin
  log.DebugLog(FailName);
  try

    S := LowerCase(Name);
    if S = 'on' then
      SoundOn := (LowerCase(Value) = 'true')
    else if S = 'selfplay' then
      RandomInterval := (LowerCase(Value) = 'rndinterval')
    else if S = 'volume' then
      Volume := StrToInt(Value)
    else if S = 'intervalperiod' then
    begin
      if Value.Contains('-') then
      begin
        Interval := StrToInt(Parse(Value, 0, '-'));
        MaxInterval := StrToInt(Parse(Value, 1, '-'));
      end
      else
      begin
        NoMaxSpecified := true;
        Interval := StrToInt(Value);
        MaxInterval := Interval;
      end;
    end
    else if S = 'useasambientfill' then
      Ambient := (LowerCase(Value) = 'true')
    else if S = 'audiofile' then
      FileName := Value
    else if S = 'continuous' then
      Continuous := (LowerCase(Value) = 'true')
    else if S = 'rndlocation' then
      RndLocation := (LowerCase(Value) = 'true')
    else if S = 'playbackspeed' then
      PlaybackSpeed := StrToInt(Value)
    else if S = 'radius' then
      Radius := StrToInt(Value)
    else
      inherited;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TSoundPlayer.SetSoundOn(const Value: boolean);
const
  FailName: string = 'TSoundPlayer.SetSoundOn';
begin
  log.DebugLog(FailName);
  try

    FSoundOn := Value;
    if assigned(SoundLib) then
    begin
      if not FSoundOn and FContinuous and Playing then
      begin
        SoundLib.StopSound(ContinuousIndex);
      end;
    end;
    Playing := false;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TSoundPlayer.SetVolume(const Value: Integer);
const
  FailName: string = 'TSoundPlayer.SetVolume';
begin
  log.DebugLog(FailName);
  try

    FVolume := Value;
    if assigned(SoundLib) then
    begin
      if FContinuous and Playing then
      begin
        SoundLib.SetSound(ContinuousIndex, PlaybackSpeed, FVolume, 0);
      end;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function Perceptible(const Source: TCharacter; const Target: TSpriteObject;
  const Vision, Hearing, Smell: Double; MysticVision: Integer): boolean;
var
  D, Dv: Double;
  dx, dy: Integer;
  S: Single;
const
  FailName: string = 'Character.Perceptible';
begin
  result := false;

  log.DebugLog(FailName);
  try

    D := Target.Distance - Target.Radius;
    result := (D <= MysticVision);
    if result then
      exit;

    if Target is TCharacter then
    begin
      D := D * TCharacter(Target).Stealth / 10;
    end;
    result := (D <= Smell) or (D <= Hearing);
    if not result then
    begin
      if Target is TCharacter then
      begin
        if TCharacter(Target).LightIndex <= 0 then
        begin
          result := false;
          exit;
        end;
        S := 2 * sqrt(TCharacter(Target).LightIndex);
        Dv := D / S;
        result := (Dv <= Vision);
        if result then
        begin
          case Source.Facing of
            fSS:
              begin
                if Target.Y < Source.Y then
                begin
                  dy := 2 * (Source.Y - Target.Y);
                  dx := Target.X - Source.X;
                  S := dx / dy;
                  result := (S <= 1) and (S >= -1);
                end;
              end;
            fSE:
              result := (Target.X >= Source.X) or (Target.Y >= Source.Y);
            fEE:
              begin
                if Target.X < Source.X then
                begin
                  dx := Target.X - Source.X;
                  dy := 2 * (Source.Y - Target.Y);
                  S := dy / dx;
                  result := (S <= 1) and (S >= -1);
                end;
              end;
            fNE:
              result := (Target.X >= Source.X) or (Target.Y <= Source.Y);
            fNN:
              begin
                if Target.Y > Source.Y then
                begin
                  dy := 2 * (Source.Y - Target.Y);
                  dx := Target.X - Source.X;
                  S := dx / dy;
                  result := (S <= 1) and (S >= -1);
                end;
              end;
            fNW:
              result := (Target.X <= Source.X) and (Target.Y <= Source.Y);
            fWW:
              begin
                if Target.X > Source.X then
                begin
                  dx := Target.X - Source.X;
                  dy := 2 * (Source.Y - Target.Y);
                  S := dy / dx;
                  result := (S <= 1) and (S >= -1);
                end;
              end;
            fSW:
              result := (Target.X <= Source.X) or (Target.Y >= Source.Y);
          end;
        end;
      end;
    end;
    if result then
      result := Game.LineOfSight(Source.X, Source.Y, Target.X, Target.Y);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function GetGUID(GUID: string): TGameObject;
var
  i: Integer;
  S: string;
const
  FailName: string = 'Character.GetGUID';
begin
  result := nil;

  log.DebugLog(FailName);
  try

    S := LowerCase(GUID);
    i := FigureInstances.IndexOf(S);
    if i >= 0 then
      result := TGameObject(FigureInstances.objects[i]);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function GetNearbyCharacter(Source: TCharacter; Limit: Double): TStringList;
var
  i, j: Integer;
  List: TList;
const
  FailName: string = 'Character.GetNearbyCharacter';
begin
  result := nil;

  log.DebugLog(FailName);
  try

    List := Game.FindInRadius(Source.X, Source.Y, Limit);
    try
      if assigned(List) then
      begin
        for i := 0 to List.Count - 1 do
        begin
          if TAniFigure(List.items[i]) is TCharacter then
          begin
            if (List.items[i] <> Source) and not TCharacter(List.items[i]).Dead
            then
            begin
              if not assigned(result) then
                result := TStringList.Create;
              j := result.add(TSpriteObject(List.items[i]).GUID);
              result.objects[j] := List.items[i];
            end;
          end;
        end;
      end;
    finally
      List.free;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TransferItem(Source, Dest: TGameObject; ItemName: string;
  DropIfNoRoom: boolean): boolean;
var
  Inventory: TList;
  S: string;
  Item: TItem;
  i, j: Integer;
  k: TSlot;
const
  FailName: string = 'Character.TransferItem';
begin
  result := true;

  log.DebugLog(FailName);
  try

    if (Source is TCharacter) then
      Inventory := TCharacter(Source).Inventory
    else if (Source is TContainer) then
      Inventory := TContainer(Source).Inventory
    else
      exit;

    S := LowerCase(ItemName);
    Item := nil;
    j := 0;
    for i := 0 to Inventory.Count - 1 do
    begin
      if LowerCase(TItem(Inventory.items[i]).ItemName) = S then
      begin
        Item := TItem(Inventory.items[i]);
        j := i;
        break;
      end;
    end;

    if not assigned(Item) and (Source is TCharacter) then
    begin
      for k := slLeg1 to slMisc3 do
      begin
        if assigned(TCharacter(Source).Equipment[k]) then
        begin
          if LowerCase(TCharacter(Source).Equipment[k].ItemName) = S then
          begin
            Item := TCharacter(Source).Equipment[k];
            if (Dest is TCharacter) then
            begin
              if TCharacter(Dest).FindFreeInventoryXY(Item) then
              begin
                TCharacter(Source).Equipment[k] := nil;
                TCharacter(Dest).Inventory.add(Item);
                Item.LayeredImage := PartManager.GetImageFile(Item.PartName,
                  TCharacterResource(TCharacter(Dest).Resource).NakedName);
                Item.Resource := PartManager.GetLayerResource
                  (Item.LayeredImage);
              end
              else
              begin
                result := false;
              end;
            end
            else if (Dest is TContainer) then
            begin
              if TContainer(Dest).FindFreeInventoryXY(Item) then
              begin
                TCharacter(Source).Equipment[k] := nil;
                TContainer(Dest).Inventory.add(Item);
              end
              else
              begin
                result := false;
              end;
            end;
            exit;
          end;
        end;
      end;
    end;

    if assigned(Item) then
    begin
      if (Dest is TCharacter) then
      begin
        if TCharacter(Dest).FindFreeInventoryXY(Item) then
        begin
          Inventory.Delete(j);
          TCharacter(Dest).Inventory.add(Item);
          Item.LayeredImage := PartManager.GetImageFile(Item.PartName,
            TCharacterResource(TCharacter(Dest).Resource).NakedName);
          Item.Resource := PartManager.GetLayerResource(Item.LayeredImage);
        end
        else if Dest = Current then
        begin
          result := false;
          if DropIfNoRoom then
          begin
            Inventory.Delete(j);
            Item.LayeredImage := PartManager.GetImageFile(Item.PartName,
              TCharacterResource(TCharacter(Dest).Resource).NakedName);
            Item.Resource := PartManager.GetLayerResource(Item.LayeredImage);
            Item.SetPos(Dest.X, Dest.Y, Dest.Z);
            TCharacter(Dest).Say(FullInvMsg, cTalkWhiteColor);
            Item.Enabled := true;
            Item.Drop;
          end;
        end;
      end
      else if (Dest is TContainer) then
      begin
        if TContainer(Dest).FindFreeInventoryXY(Item) then
        begin
          Inventory.Delete(j);
          TContainer(Dest).Inventory.add(Item);
        end
        else
        begin
          result := false;
        end;
      end;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function GetPerceptibleEnemies(Source: TCharacter; Factor: Single): TStringList;
var
  i, j: Integer;
  List: TList;
  Limit: Double;
  lVision, Hearing, Smell, F: Double;
const
  FailName: string = 'Character.GetPerceptibleEnemies';
begin
  result := nil;

  log.DebugLog(FailName);
  try
    F := Source.PerceptionFactor * Factor;
    lVision := Source.Vision * F;
    Hearing := Source.Hearing * F;
    Smell := Source.Smell * F;
    Limit := lVision;
    if Hearing > Limit then
      Limit := Hearing;
    if Smell > Limit then
      Limit := Smell;
    if Source.MysticVision > Limit then
      Limit := Source.MysticVision;

    List := Game.FindInRadius(Source.X, Source.Y, Limit);
    try
      if assigned(List) then
      begin
        for i := 0 to List.Count - 1 do
        begin
          if TAniFigure(List.items[i]) is TCharacter then
          begin
            if not TCharacter(List.items[i]).Dead and
              Source.IsEnemy(TCharacter(List.items[i])) then
            begin
              if Perceptible(Source, TSpriteObject(List.items[i]), lVision,
                Hearing, Smell, Source.MysticVision) then
              begin
                if not assigned(result) then
                  result := TStringList.Create;
                j := result.add(TSpriteObject(List.items[i]).GUID);
                result.objects[j] := List.items[i];
              end;
            end;
          end;
        end;
      end;
    finally
      List.free;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function GetNearbyEnemies(Source: TCharacter; Limit: Double): TStringList;
var
  i, j: Integer;
  List: TList;
const
  FailName: string = 'Character.GetNearbyEnemies';
begin
  result := nil;

  log.DebugLog(FailName);
  try

    List := Game.FindInRadius(Source.X, Source.Y, Limit);
    try
      if assigned(List) then
      begin
        for i := 0 to List.Count - 1 do
        begin
          if TAniFigure(List.items[i]) is TCharacter then
          begin
            if not TCharacter(List.items[i]).Dead and
              (Source.IsEnemy(TCharacter(List.items[i])) or
              TCharacter(List.items[i]).IsEnemy(Source)) then
            begin
              if not assigned(result) then
                result := TStringList.Create;
              j := result.add(TSpriteObject(List.items[i]).GUID);
              result.objects[j] := List.items[i];
            end;
          end;
        end;
      end;
    finally
      List.free;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function GetPerceptibleAllies(Source: TCharacter; Factor: Single): TStringList;
var
  i, j: Integer;
  List: TList;
  Limit: Double;
  lVision, Hearing, Smell, F: Double;
const
  FailName: string = 'Character.GetPerceptibleAllies';
begin
  result := nil;

  log.DebugLog(FailName);
  try

    F := Source.PerceptionFactor * Factor;
    lVision := Source.Vision * F;
    Hearing := Source.Hearing * F;
    Smell := Source.Smell * F;
    Limit := lVision;
    if Hearing > Limit then
      Limit := Hearing;
    if Smell > Limit then
      Limit := Smell;
    if Source.MysticVision > Limit then
      Limit := Source.MysticVision;

    List := Game.FindInRadius(Source.X, Source.Y, Limit);
    try
      if assigned(List) then
      begin
        for i := 0 to List.Count - 1 do
        begin
          if TAniFigure(List.items[i]) is TCharacter then
          begin
            if (List.items[i] <> Source) and not TCharacter(List.items[i])
              .Dead and Source.IsAlly(TCharacter(List.items[i])) then
            begin
              if Perceptible(Source, TSpriteObject(List.items[i]), lVision,
                Hearing, Smell, Source.MysticVision) then
              begin
                if not assigned(result) then
                  result := TStringList.Create;
                j := result.add(TSpriteObject(List.items[i]).GUID);
                result.objects[j] := List.items[i];
              end;
            end;
          end;
        end;
      end;
    finally
      List.free;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function GetNearbyAllies(Source: TCharacter; Limit: Double): TStringList;
var
  i, j: Integer;
  List: TList;
const
  FailName: string = 'Character.GetNearbyAllies';
begin
  result := nil;

  log.DebugLog(FailName);
  try

    List := Game.FindInRadius(Source.X, Source.Y, Limit);
    try
      if assigned(List) then
      begin
        for i := 0 to List.Count - 1 do
        begin
          if TAniFigure(List.items[i]) is TCharacter then
          begin
            if (List.items[i] <> Source) and not TCharacter(List.items[i])
              .Dead and (Source.IsAlly(TCharacter(List.items[i])) or
              TCharacter(List.items[i]).IsAlly(Source)) then
            begin
              if not assigned(result) then
                result := TStringList.Create;
              j := result.add(TSpriteObject(List.items[i]).GUID);
              result.objects[j] := List.items[i];
            end;
          end;
        end;
      end;
    finally
      List.free;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function GetPerceptibleDead(Source: TCharacter; Factor: Single): TStringList;
var
  i, j: Integer;
  List: TList;
  Limit: Double;
  lVision, Hearing, Smell, F: Double;
const
  FailName: string = 'Character.GetPerceptibleDead';
begin
  result := nil;

  log.DebugLog(FailName);
  try

    F := Source.PerceptionFactor * Factor;
    lVision := Source.Vision * F;
    Hearing := Source.Hearing * F;
    Smell := Source.Smell * F;
    Limit := lVision;
    if Hearing > Limit then
      Limit := Hearing;
    if Smell > Limit then
      Limit := Smell;
    if Source.MysticVision > Limit then
      Limit := Source.MysticVision;

    List := Game.FindInRadius(Source.X, Source.Y, Limit);
    try
      if assigned(List) then
      begin
        for i := 0 to List.Count - 1 do
        begin
          if TAniFigure(List.items[i]) is TCharacter then
          begin
            if TCharacter(List.items[i]).Dead then
            begin
              if Perceptible(Source, TSpriteObject(List.items[i]), lVision,
                Hearing, Smell, Source.MysticVision) then
              begin
                if not assigned(result) then
                  result := TStringList.Create;
                j := result.add(TSpriteObject(List.items[i]).GUID);
                result.objects[j] := List.items[i];
              end;
            end;
          end;
        end;
      end;
    finally
      List.free;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function GetPerceptibleContainers(Source: TCharacter; Factor: Single)
  : TStringList;
var
  i, j: Integer;
  List: TList;
  Limit: Double;
  lVision, Hearing, Smell, F: Double;
const
  FailName: string = 'Character.GetPerceptibleContainers';
begin
  result := nil;

  log.DebugLog(FailName);
  try

    F := Source.PerceptionFactor * Factor;
    lVision := Source.Vision * F;
    Hearing := Source.Hearing * F;
    Smell := Source.Smell * F;
    Limit := lVision;
    if Hearing > Limit then
      Limit := Hearing;
    if Smell > Limit then
      Limit := Smell;
    if Source.MysticVision > Limit then
      Limit := Source.MysticVision;

    List := Game.FindInRadius(Source.X, Source.Y, Limit);
    try
      if assigned(List) then
      begin
        for i := 0 to List.Count - 1 do
        begin
          if TAniFigure(List.items[i]) is TContainer then
          begin
            if Perceptible(Source, TSpriteObject(List.items[i]), lVision,
              Hearing, Smell, Source.MysticVision) then
            begin
              if not assigned(result) then
                result := TStringList.Create;
              j := result.add(TSpriteObject(List.items[i]).GUID);
              result.objects[j] := List.items[i];
            end;
          end;
        end;
      end;
    finally
      List.free;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function GetGroup(Source: TGameObject; const GroupName: string): TStringList;
var
  S: string;
  i, j: Integer;
const
  FailName: string = 'Character.GetGroup';
begin
  result := nil;

  log.DebugLog(FailName);
  try

    if GroupName = '' then
      exit;
    S := LowerCase(GroupName);
    for i := 0 to FigureInstances.Count - 1 do
    begin
      if assigned(FigureInstances.objects[i]) then
      begin
        if LowerCase(TGameObject(FigureInstances.objects[i]).GroupName) = S then
        begin
          if (((FigureInstances.objects[i] is TCharacter) and
            not TCharacter(FigureInstances.objects[i]).Dead) or
            not(FigureInstances.objects[i] is TCharacter)) and
            (FigureInstances.objects[i] <> Source) then
          begin
            if not assigned(result) then
              result := TStringList.Create;
            j := result.add(FigureInstances.strings[i]);
            result.objects[j] := FigureInstances.objects[i];
          end;
        end;
      end;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function AllDead(const GroupName: string): boolean;
var
  S: string;
  i: Integer;
const
  FailName: string = 'Character.AllDead';
begin
  result := true;

  log.DebugLog(FailName);
  try

    if GroupName = '' then
      exit;
    S := LowerCase(GroupName);
    for i := 0 to FigureInstances.Count - 1 do
    begin
      if assigned(FigureInstances.objects[i]) then
      begin
        if TGameObject(FigureInstances.objects[i]).Enabled then
        begin
          if LowerCase(TGameObject(FigureInstances.objects[i]).GroupName) = S
          then
          begin
            if ((FigureInstances.objects[i] is TCharacter) and
              not(TCharacter(FigureInstances.objects[i]).Dead or
              TCharacter(FigureInstances.objects[i]).Dieing)) then
            begin
              result := false;
              exit;
            end;
          end;
        end;
      end;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

{ TSpriteManager }

constructor TSpriteManager.Create(Count: Integer);
var
  i: Integer;
  Sprite: TSpriteObject;
const
  FailName: string = 'TSpriteManager.Create';
begin
  log.DebugLog(FailName);
  try

    SpriteCount := Count;
    List := TList.Create;
    List.capacity := SpriteCount;
    for i := 1 to SpriteCount do
    begin
      Sprite := TSpriteObject.Create(0, 0, 0, 0, false);
      Game.AddFigure(Sprite);
      List.add(Sprite);
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

destructor TSpriteManager.Destroy;
const
  FailName: string = 'TSpriteManager.Destroy';
begin
  log.DebugLog(FailName);
  try

    List.free;
    inherited;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TSpriteManager.NewSprite(ClassType: TSpriteClass;
  Resource: TAniResource; X, Y, Z, Frame: Integer): TSpriteObject;
var
  StartIndex: Integer;
  Tail1, Tail2: TProjectile;
const
  FailName: string = 'TSpriteManager.NewSprite';
begin
  result := nil;

  log.DebugLog(FailName);
  try

    StartIndex := FCurrentIndex;
    while TAniFigure(List.items[FCurrentIndex]).Enabled do
    begin
      Inc(FCurrentIndex);
      if (FCurrentIndex >= SpriteCount) then
        FCurrentIndex := 0;
      if FCurrentIndex = StartIndex then
        break; // We've gone all the way around. Now pick the oldest.
    end;
    // We're going to rely on the assumption that the indexes in Game.FigureList
    // match the indexes in List.
    if (TAniFigure(List.items[FCurrentIndex]) is TProjectile) then
    begin
      Tail1 := TProjectile(List.items[FCurrentIndex]).TrailedBy;
      while assigned(Tail1) do
      begin
        if Tail1.Enabled then
        begin
          Tail1.Enabled := false;
          Game.DisableFigure(Tail1);
        end;
        Tail2 := Tail1.TrailedBy;
        Tail1.TrailedBy := nil;
        Tail1 := Tail2;
      end;
    end;

    result := ClassType.Create(X, Y, Z, Frame, true);
    result.Resource := Resource;
    Game.ReplaceFigure(FCurrentIndex, result);
    List.items[FCurrentIndex] := result;

    Inc(FCurrentIndex);
    if (FCurrentIndex >= SpriteCount) then
      FCurrentIndex := 0;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TSpriteManager.ReAlloc;
var
  i: Integer;
  Sprite: TSpriteObject;
const
  FailName: string = 'TSpriteManager.ReAlloc';
begin
  log.DebugLog(FailName);
  try

    List.Clear;
    for i := 1 to SpriteCount do
    begin
      Sprite := TSpriteObject.Create(0, 0, 0, 0, false);
      Game.AddFigure(Sprite);
      List.add(Sprite);
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

{ TPathCorner }

function TPathCorner.GetProperty(const Name: string): string;
var
  S: string;
const
  FailName: string = 'TPathCorner.GetProperty';
begin
  log.DebugLog(FailName);
  try

    S := LowerCase(Name);
    if S = 'nextdestination' then
      result := NextDestination
    else if S = 'onarrival' then
      result := OnArrival
    else if S = 'delay' then
      result := IntToStr(Delay)
    else
      result := inherited GetProperty(name);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TPathCorner.SaveProperties(List: TStringList);
var
  S: string;
const
  FailName: string = 'TPathCorner.SaveProperties';
begin
  log.DebugLog(FailName);
  try

    S := 'nextdestination=' + NextDestination;
    List.add(S);
    S := 'delay=' + IntToStr(Delay);
    List.add(S);
    S := 'onarrival=' + OnArrival;
    List.add(S);
    inherited;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TPathCorner.SetProperty(const Name, Value: string);
var
  S: string;
const
  FailName: string = 'TPathCorner.SetProperty';
begin
  log.DebugLog(FailName);
  try

    S := LowerCase(Name);
    if S = 'nextdestination' then
      NextDestination := Value
    else if S = 'onarrival' then
    begin
      if not LoadingFromSaveFile then
        OnArrival := Value;
    end
    else if S = 'delay' then
      Delay := StrToInt(Value)
    else
      inherited;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

{ TEffect }

procedure TEffect.Adjust(Character: TCharacter);
const
  FailName: string = 'TEffect.Adjust';
begin
  log.DebugLog(FailName);
  try

    Character.ApplyModifier(@StatModifier);
    Character.ApplyResistance(@Resistance);
    if ApplyColor then
    begin
      Character.ColorR := ColorR;
      Character.ColorG := ColorG;
      Character.ColorB := ColorB;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TEffect.DoAction(const Action: string; Facing: TFacing);
var
  S: string;
const
  FailName: string = 'TEffect.DoAction';
begin
  log.DebugLog(FailName);
  try

    if assigned(Resource) then
    begin
      S := Action + Facing.ToString;
      Script := Resource.Script[S];
      if not assigned(Script) then
        Script := Resource.Script[Action];
      if assigned(Script) then
      begin
        if Script.tag = scrLoop then
          PlayMode := smRepeat
        else if Script.tag = scrRandom then
          PlayMode := smRandom
        else
          PlayMode := smOnce;
        FrameMultiplier := Script.Multiplier;
        FScriptFrame := 1;
        Delay := -1;
      end;
    end
    else
      Script := nil;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

// Return value of true means this object is ready to be destroyed.

function TEffect.DoFrame: boolean;
const
  FailName: string = 'TEffect.DoFrame';
begin
  result := true;

  log.DebugLog(FailName);
  try

    if AnimationDuration > 0 then
      Dec(AnimationDuration);
    if Duration > 0 then
      Dec(Duration);
    if (AnimationDuration > 0) or (Duration > 0) then
    begin
      if (AnimationDuration > 0) then
        UpdateScript;
      result := false;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TEffect.Hit(Source: TAniFigure; Damage: PDamageProfile): boolean;
begin
  result := false;
end;

procedure TEffect.Refresh(NewEffect: TEffect);
begin

end;

procedure TEffect.RenderLocked(Figure: TAniFigure; Bits: PBITPLANE);
var
  SrcBlend, DstBlend: Integer;
const
  FailName: string = 'TEffect.RenderLocked';
begin
  log.DebugLog(FailName);
  try

    if assigned(Resource) and (AnimationDuration > 0) then
    begin
      if Keyed then
      begin
        Resource.RenderLocked(Figure, Bits);
      end
      else
      begin
        if UseCustom then
        begin
          case SpecialEffect of
            seTranslucent:
              begin
                DstBlend := 100 - Alpha;
                SrcBlend := Alpha;
              end;
            seAdd:
              begin
                DstBlend := 100;
                SrcBlend := Alpha;
              end;
          else
            begin
              DstBlend := 0;
              SrcBlend := 100;
            end;
          end;
          Resource.RLE.DrawColorize(FFrame - 1,
            Figure.CenterX - Resource.FrameWidth div 2,
            (Figure.Height - Resource.FrameHeight) div 2, Bits, EffectR,
            EffectG, EffectB, SrcBlend, DstBlend);
        end
        else
        begin
          case Resource.SpecialEffect of
            seTranslucent:
              begin
                DstBlend := 100 - Resource.Alpha;
                SrcBlend := Resource.Alpha;
              end;
            seAdd:
              begin
                DstBlend := 100;
                SrcBlend := Resource.Alpha;
              end;
          else
            begin
              DstBlend := 0;
              SrcBlend := 100;
            end;
          end;
          Resource.RLE.DrawBlend(FFrame - 1,
            Figure.CenterX - Resource.FrameWidth div 2,
            (Figure.Height - Resource.FrameHeight) div 2, Bits, SrcBlend,
            DstBlend);
        end;
      end;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TEffect.SetResource(const Value: TResource);
begin
  FResource := Value;
  if assigned(FResource) then
    FResource.LoadGraphic;
end;

procedure TEffect.UpdateScript;
const
  FailName: string = 'TEffect.UpdateScript';
begin
  log.DebugLog(FailName);
  try

    if assigned(Script) then
    begin
      Inc(Delay);
      if Delay = 0 then
      begin
        FFrame := Script.FrameID[FScriptFrame];
      end
      else if (Delay >= FrameMultiplier) then
      begin
        Delay := 0;
        if (PlayMode = smRandom) then
        begin
          FScriptFrame := random(Script.Frames) + 1;
          FFrame := Script.FrameID[FScriptFrame];
        end
        else
        begin
          if (FScriptFrame < Script.Frames) then
          begin
            Inc(FScriptFrame);
            FFrame := Script.FrameID[FScriptFrame];
          end
          else
          begin
            if (PlayMode = smRepeat) then
            begin
              FScriptFrame := 1;
              FFrame := Script.FrameID[1];
            end
            else
            begin
              FFrame := Script.FrameID[Script.Frames];
              FScriptFrame := 0;
            end;
          end;
        end;
      end;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

{ TEventTimer }

procedure TEventTimer.Execute;
var
  event: string;
begin
  if Enabled then
  begin
    Dec(CountDown);
    if CountDown <= 0 then
    begin
      Inc(TimerCount);
      event := 'OnTimer[' + IntToStr(TimerCount) + ']';
      if PropertyExists(event) then
      begin
        RunScript(Self, Properties[event]);
      end
      else
      begin
        RunScript(Self, OnTimer);
      end;

      CountDown := Interval;
      Enabled := AutoReset;
    end;
  end;
end;

function TEventTimer.GetProperty(const Name: string): string;
var
  S: string;
const
  FailName: string = 'TEventTimer.GetProperty';
begin
  log.DebugLog(FailName);
  try

    S := LowerCase(Name);
    if S = 'interval' then
      result := IntToStr(Interval)
    else if S = 'enabled' then
      result := BoolToStr(Enabled, True)
    else if S = 'autoreset' then
      result := BoolToStr(AutoReset, True)
    else if S = 'countdown' then
      result := IntToStr(CountDown)
    else if S = 'timercount' then
      result := IntToStr(TimerCount)
    else if S = 'ontimer' then
      result := OnTimer
    else
      result := inherited GetProperty(Name);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TEventTimer.SaveProperties(List: TStringList);
var
  S: string;
const
  FailName: string = 'TEventTimer.SaveProperties';
begin
  log.DebugLog(FailName);
  try

    S := 'interval=' + IntToStr(Interval);
    List.add(S);
    S := 'enabled=' + BoolToStr(Enabled, True);
    List.add(S);
    S := 'autoreset=' + BoolToStr(AutoReset, True);
    List.add(S);
    S := 'countdown=' + IntToStr(CountDown);
    List.add(S);
    S := 'timercount=' + IntToStr(TimerCount);
    List.add(S);
    S := 'ontimer=' + OnTimer;
    List.add(S);
    inherited;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TEventTimer.SetProperty(const Name, Value: string);
var
  S: string;
  L: Integer;
  NoProp: boolean;
const
  FailName: string = 'TEventTimer.SetProperty';
begin
  log.DebugLog(FailName);
  try

    NoProp := false;
    S := LowerCase(Name);
    L := length(S);
    case L of
      7:
        begin
          if S = 'enabled' then
            Enabled := (LowerCase(Value) = 'true')
          else if S = 'ontimer' then
          begin
            if not LoadingFromSaveFile then
              OnTimer := Value
          end
          else
            NoProp := true;
        end;
      8:
        begin
          if S = 'interval' then
            Interval := StrToInt(Value)
          else
            NoProp := true;
        end;
      9:
        begin
          if S = 'autoreset' then
            AutoReset := (LowerCase(Value) = 'true')
          else if S = 'countdown' then
            CountDown := StrToInt(Value)
          else
            NoProp := true;
        end;
      10:
        begin
          if S = 'timercount' then
            TimerCount := StrToInt(Value)
          else
            NoProp := true;
        end;
    end;

    if NoProp then
      inherited;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

{ TCompanionCharacter }

constructor TCompanionCharacter.Create(X, Y, Z: Integer; Frame: Word;
  Enabled: boolean);
const
  FailName: string = 'TCompanionCharacter.Create';
begin
  log.DebugLog(FailName);
  try

    inherited;
    Fade := -40;
    Frozen := true;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TCompanionCharacter.DoFrame;
var
  i: Integer;
const
  FailName: string = 'TCompanionCharacter.DoFrame';
begin
  log.DebugLog(FailName);
  try

    if Fade < 0 then
    begin
      Inc(Fade);
      Alpha := (100 * (40 + Fade)) div 40;
      if Fade = 0 then
      begin
        SpecialEffect := seNone;
        Highlightable := true;
        Frozen := false;
      end;
    end
    else if Fade > 0 then
    begin
      Dec(Fade);
      Alpha := (100 * Fade) div 40;
      if Fade = 0 then
      begin
        Enabled := false;
        if assigned(Master) then
        begin
          for i := 1 to MaxCompanions do
          begin
            if Master.Companion[i] = Self then
              Master.Companion[i] := nil;
          end;
        end;
        for i := 0 to Game.FigureList.Count - 1 do
        begin
          if TObject(Game.FigureList.items[i]) is TCharacter then
          begin
            if TCharacter(Game.FigureList.items[i]).Track = Self then
              TCharacter(Game.FigureList.items[i]).Track := nil;
          end;
        end;
        if Game.KeyFigure = Self then
          frmMain.ChangeFocus(Master);
      end;
    end
    else
    begin
      if Duration > 0 then
      begin
        Dec(Duration);
        if (Duration <= 0) then
        begin
          Fade := 40;
          SpecialEffect := seTranslucent;
          Highlightable := false;
        end;
      end;
    end;
    if Enabled then
      inherited;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

destructor TCompanionCharacter.Destroy;
var
  i: Integer;
begin
  for i := 0 to Game.FigureList.Count - 1 do
  begin
    if TObject(Game.FigureList.items[i]) is TCharacter then
    begin
      if TCharacter(Game.FigureList.items[i]).Track = Self then
        TCharacter(Game.FigureList.items[i]).Track := nil;
    end;
  end;

  inherited;
end;

procedure TCompanionCharacter.SetResource(const Value: TAniResource);
const
  FailName: string = 'TCompanionCharacter.SetResource';
begin
  log.DebugLog(FailName);
  try

    inherited;
    SpecialEffect := seTranslucent;
    Alpha := 0;
    Highlightable := false;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

end.
