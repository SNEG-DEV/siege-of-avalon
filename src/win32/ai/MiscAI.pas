unit MiscAI;
{******************************************************************************}
{                                                                              }
{               Siege Of Avalon : Open Source Edition                          }
{               -------------------------------------                          }
{                                                                              }
{ Portions created by Digital Tome L.P. Texas USA are                          }
{ Copyright ©1999-2000 Digital Tome L.P. Texas USA                             }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Portions created by Team SOAOS are                                           }
{ Copyright (C) 2003 - Team SOAOS.                                             }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                            }
{                                                                              }
{                                                                              }
{                                                                              }
{ You may retrieve the latest version of this file at the SOAOS project page : }
{   http://www.sourceforge.com/projects/soaos                                  }
{                                                                              }
{ The contents of this file maybe used with permission, subject to             }
{ the GNU Lesser General Public License Version 2.1 (the "License"); you may   }
{ not use this file except in compliance with the License. You may             }
{ obtain a copy of the License at                                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Software distributed under the License is distributed on an                  }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{ implied. See the License for the specific language governing                 }
{ rights and limitations under the License.                                    }
{                                                                              }
{ Description                                                                  }
{ -----------                                                                  }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   DirectX Runtime libraris on Win32                                          }
{   They are available from...                                                 }
{   http://www.microsoft.com.                                                  }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{   July    13 2003 - DL : Initial Upload to CVS                               }
{                                                                              }
{******************************************************************************}
{*****************************************************************************}
{ Digital Tome Game Engine System                                             }
{                                                                             }
{ Copyright ©1999-2000 Digital Tome L.P. Texas USA as an unpublished work.    }
{ STRICTLY CONFIDENTIAL AND PROPRIETARY PRIVATE PROPERTY                      }
{ Not for public release/use.                                                 }
{                                                                             }
{ AI1                                                                         }
{                                                                             }
{ This is a SAMPLE "Stand-Alone Extension Artificial Intelligence" module     }
{ for the Digital Tome Game Engine System. As an AI consultant you are        }
{ authorized to use this specific file as an example/template for creating    }
{ the various modules you have been contracted to submit for possible         }
{ inclusion in our games. You should rename this (as AI1 is taken) then       }
{ begin creating your own additional features and actions for our review.     }
{                                                                             }
{*****************************************************************************}

{$INCLUDE Anigrp30cfg.inc}

interface

uses
  Classes,
  SysUtils,
  Character,
  Engine,
  Anigrp30,
  LogFile,
  Dialogs,
  Resource,
  Graphics,
  Math,
  strfunctions;

type
  TIdleDuty = ( idStand, idMeander, idGuard, idbusy );
  TMainStat = ( msStrength, msHitPoints, msCombat, msMysticism, msMana );
  TSFXDelayType = ( dtFixed, dtRandom );

  TCompanion = class( TPartyAI )
  private
    Delay : Integer;
    ShotCounter : integer;
    BuffCount : integer;
    MaxBuffs : integer;
    MaxShots : integer;
    BlockedShot : integer;

    AttackFrame : integer;
    GuardFrame : integer;
    FCombative : Boolean;
    Fighting : Boolean;
    FFighter : Boolean;
    FScout : Boolean;
    FMage : Boolean;

    FMeleeRanged : Boolean;
    FMeleeAggressive : Boolean;
    FMeleeDefensive : Boolean;
    FMagicAggressive : Boolean;
    FMagicDefensive : Boolean;

    FHoldAggressive : Boolean;
    FHoldDefensive : Boolean;
    FHoldandRun : Boolean;

    FHealFirst : Boolean;
 //   fStayClose: Boolean;
    Walking : Boolean;
    ReadyToAttack : Boolean;
//    FCaster: Boolean;
    bMove : Boolean;
    NewFrame : Integer;
    CirclePoint : Integer;
    NukeCounter : Integer;
  //  RunOrFight: Boolean;
    RunAway : Boolean;
    CastTimes : Integer;
    IDistance : Integer;
    Stopped : boolean;
    Spells : TStringList;
    oSpellBook : string;
    dSpellBook : string;

    procedure GuardPlayer;
    procedure FollowLeader;
    procedure AttackMelee;
    procedure FightBack;
    procedure AttackMeleeRanged;
    procedure AttackCaster;
    function HealParty : boolean;
    procedure MoveAway;
    procedure BattleTactic;
    procedure BuffParty;


  protected
    procedure OnNoPath; override;
    procedure OnStop; override;
    procedure WasAttacked( Source : TAniFigure; Damage : Single ); override;
    function OnCollideFigure( Target : TAniFigure ) : boolean; override;

    procedure SetMeleeRanged( value : Boolean );
    procedure SetMeleeAggressive( value : Boolean );
    procedure SetMeleeDefensive( value : Boolean );
    procedure SetMagicAggressive( value : Boolean );
    procedure SetMagicDefensive( value : Boolean );

    procedure SetHoldAggressive( value : Boolean );
    procedure SetHoldDefensive( value : Boolean );
    procedure SetHoldandRun( value : Boolean );

  public
    destructor Destroy; override;
    property Combative : Boolean read FCombative write FCombative;
   //roperty Caster: Boolean read FCaster write FCaster;
    property Healfirst : Boolean read FHealfirst write FHealfirst;
//    property StayClose: Boolean read fStayClose write fStayClose;
    property MeleeRanged : Boolean read FMeleeRanged write SetMeleeRanged;
    property MeleeAggressive : Boolean read FMeleeAggressive write SetMeleeAggressive;
    property MeleeDefensive : Boolean read FMeleeDefensive write SetMeleeDefensive;
    property MagicAggressive : Boolean read FMagicAggressive write SetMagicAggressive;
    property MagicDefensive : Boolean read FMagicDefensive write SetMagicDefensive;

    property HoldAggressive : Boolean read FHoldAggressive write SetHoldAggressive;
    property HoldDefensive : Boolean read FHoldDefensive write SetHoldDefensive;
    property HoldandRun : Boolean read FHoldandRun write SetHoldandRun;

    procedure Init; override;
    procedure Execute; override;
    function InRange( Target : TAniFigure ) : Boolean;
  end;

  TMeleeTraining = class( TAI )
  private
    Walking : Boolean;
    ReadyToAttack : Boolean;
    //RunAway: Boolean;
    Delay : integer;
    CollideCount : integer;
    waiting : boolean;
    //Soft
    procedure FindNextTarget;
    procedure Attack;
  protected
    procedure OnStop; override;
    procedure WasAttacked( Source : TAniFigure; Damage : Single ); override;
    function OnCollideFigure( Target : TAniFigure ) : Boolean; override;
    procedure OnNoPath; override;
  public
    destructor Destroy; override;
    procedure Execute; override;
  end;

  TMeleeSparing = class( TAI )
  private
    Walking : Boolean;
    ReadyToAttack : Boolean;
    iTimeToRun : integer;
    Delay : integer;
    //Soft
    procedure Attack;
  protected
    procedure OnStop; override;
    procedure WasAttacked( Source : TAniFigure; Damage : Single ); override;
    function OnCollideFigure( Target : TAniFigure ) : Boolean; override;
    procedure OnNoPath; override;
  public
    procedure Init; override;
    procedure Execute; override;
  end;

  TMeleePratice = class( TAI )
  private
    Delay : integer;
    PartyTotal : integer;
    Partylist : TstringList;
  protected
    procedure WasAttacked( Source : TAniFigure; Damage : Single ); override;
    procedure WasKilled( Source : TAniFigure ); override;
  public
    destructor Destroy; override;
    procedure Init; override;
    procedure Execute; override;
  end;

  TRitual = class( TAI )
  private
    Delay : integer;
    strSpell : string;
    iSpellCount : Integer;
  protected
  public
    destructor Destroy; override;
    procedure Init; override;
    procedure Execute; override;
  end;

  TEnviromentDamage = class( TAI )
  private
    Interval : integer;
  protected
  public
    destructor Destroy; override;
    procedure Init; override;
    procedure Execute; override;
  end;

  TRunScript = class( TAI )
  private
    Interval : integer;
    NewFrame : integer;
    strScript : string;
  protected
  public
    destructor Destroy; override;
    procedure Init; override;
    procedure Execute; override;
  end;

  TGuardDog = class( TAI )
  private
    FMaster : TCharacter;
    GroupList : TStringList;
    Delay : Integer;
    FCombative : Boolean;
    Fighting : Boolean;
    FFighter : Boolean;
    FScout : Boolean;
    FMage : Boolean;
    OldStand : string;
    NewStand : string;
    StandInterval : integer;

    //Sounds
    IdleSFXDelay : integer;
    iSFXDelayCount : integer;
    PlayIdleSFX : boolean;
    bPlaySFXMetal : boolean;
    bPlaySFXAttack : boolean;
    bPlaySFXOther : boolean;
    SFXDelayType : TSFXDelayType;

    FHealFirst : Boolean;
    fStayClose : Boolean;
    Walking : Boolean;
    ReadyToAttack : Boolean;
    FCaster : Boolean;
    bMove : Boolean;
    CirclePoint : Integer;
    NukeCounter : Integer;
    RunOrFight : Boolean;
    RunAway : Boolean;
    CastTimes : Integer;
    IDistance : Integer;
    Stopped : boolean;
    Spells : TStringList;
    oSpellBook : string;
    strDisguise : string;
    tmpEnemies : string;
    procedure GuardMaster;
    procedure FollowLeader;
    procedure AttackMelee;
    procedure AttackCaster;
    procedure HealMaster;
    procedure HealSelf;
    procedure MoveAway;
    procedure BattleTactic;
    procedure PlaySounds;

  protected
    procedure OnNoPath; override;
    procedure OnStop; override;
    procedure WasAttacked( Source : TAniFigure; Damage : Single ); override;
    function OnCollideFigure( Target : TAniFigure ) : boolean; override;

  public
    destructor Destroy; override;
    property Combative : Boolean read FCombative write FCombative;
    property Caster : Boolean read FCaster write FCaster;
    property Healfirst : Boolean read FHealfirst write FHealfirst;
    property StayClose : Boolean read fStayClose write fStayClose;

    procedure Init; override;
    procedure Execute; override;
  end;

  TRandomChestLoot = class( TAI )
  private
  protected
  public
    procedure Init; override;
  end;

  TClonePlayer = class( TAI )
  private
  protected
  public
    procedure Init; override;
  end;

  TObelisk = class( TAI )
  private
  protected
  public
    procedure Clicked; override;
  end;

  TPriortyCompanion = class( TPartyAI )
  private
    Delay : Integer;
    FCombative : Boolean;
    Friendly : TCharacter;
    FriendsList : TStringList;
    Fighting : Boolean;
    FHealFirst : Boolean;
    fStayClose : Boolean;
    Walking : Boolean;
    ReadyToAttack : Boolean;
    FCaster : Boolean;
    bMove : Boolean;
    CirclePoint : Integer;
    NukeCounter : Integer;
    RunOrFight : Boolean;
    RunAway : Boolean;
    CastTimes : Integer;
    IDistance : Integer;
    Spells : TStringList;
    oSpellBook : string;
    dSpellBook : string;

    procedure GuardPlayer;
    procedure FollowPlayer( Distance : Integer );
    procedure AttackMelee;
    procedure AttackCaster;
    procedure HealPlayer;
    procedure MoveAway;
    procedure BattleTactic;
    procedure FindFriendly;

  protected
    procedure OnNoPath; override;
    procedure OnStop; override;
    procedure WasAttacked( Source : TAniFigure; Damage : Single ); override;
  public
    destructor Destroy; override;
    property Combative : Boolean read FCombative write FCombative;
    property Caster : Boolean read FCaster write FCaster;
    property Healfirst : Boolean read FHealfirst write FHealfirst;
    property StayClose : Boolean read fStayClose write fStayClose;

    procedure Init; override;
    procedure Execute; override;
    function OnCollideFigure( Target : TAniFigure ) : Boolean; override;
  end;

  TWorms = class( TAI )
  private
    Delay : Integer;
    Fighting : Boolean;
    ReadyToAttack : Boolean;
    Revealed : Boolean;
    AttackDelay : Integer;

    procedure AttackMelee;
//    procedure BattleTactic;
    procedure FindTarget;
  protected
    procedure OnNoPath; override;
    procedure OnStop; override;
    procedure WasAttacked( Source : TAniFigure; Damage : Single ); override;
  public
    destructor Destroy; override;
    procedure Init; override;
    procedure Execute; override;
  end;

  TWatchDog = class( TAI )
  private
    Delay : Integer;
    FCombative : Boolean;
    iLeash : Integer;
    CenterX : Integer;
    CenterY : Integer;
    strTitle : string;
    IdleDuty : string;
    Fighting : Boolean;
    Walking : Boolean;
    ReadyToAttack : Boolean;
    FCaster : Boolean;
    bMove : Boolean;
    CirclePoint : Integer;
    NukeCounter : Integer;
    RunOrFight : Boolean;
    RunAway : Boolean;
    CastTimes : Integer;
    IDistance : Integer;
    Spells : TStringList;
    oSpellBook : string;

    procedure AttackMelee;
    procedure AttackCaster;
    procedure MoveAway;
    procedure BattleTatic;
    procedure GuardDog;
    procedure Meander;
  protected
    procedure OnNoPath; override;
    procedure OnStop; override;
    procedure WasAttacked( Source : TAniFigure; Damage : Single ); override;
  public
    destructor Destroy; override;
    property Combative : Boolean read FCombative write FCombative;
    property Caster : Boolean read FCaster write Fcaster;
    procedure Init; override;
    procedure Execute; override;
    function OnCollideFigure( Target : TAniFigure ) : Boolean; override;
  end;

  TDrunk = class( TAI )
  private
    Delay : Integer;
    FCombative : Boolean;
    bHarassing : Boolean;
    bShutUp : Boolean;
    iLeash : Integer;
    CenterX : Integer;
    CenterY : Integer;
    Fighting : Boolean;
    Walking : Boolean;
    ReadyToAttack : Boolean;
    FCaster : Boolean;
    bMove : Boolean;
    CirclePoint : Integer;
    NukeCounter : Integer;
    RunOrFight : Boolean;
    RunAway : Boolean;
    CastTimes : Integer;
    IDistance : Integer;
    Spells : TStringList;
    oSpellBook : string;

    procedure AttackMelee;
    procedure AttackCaster;
    procedure MoveAway;
    procedure BattleTatic;
    procedure DetectChar;
//    procedure Meander;
  protected
    procedure OnNoPath; override;
    procedure OnStop; override;
    procedure WasAttacked( Source : TAniFigure; Damage : Single ); override;
  public
    property Combative : Boolean read FCombative write FCombative;
    property Caster : Boolean read FCaster write Fcaster;
    procedure Init; override;
    procedure Execute; override;
    function OnCollideFigure( Target : TAniFigure ) : Boolean; override;
  end;

  TCommanderCombat = class( TAI )
  private
    TimeToDie : Integer;
    RunAwayTime : Integer;
    FBaseCourage : Integer;
    FBonusCourage : Integer;
    Walking : Boolean;
    Delay : Integer;
    TimeToAttack : Integer;
    MainStat : string;
    OrigPartyTot : Integer;
    OrdersGiven : Boolean;
    WaitingToKill : Boolean;
    CollideCount : Integer;
    bRunAway : Boolean;
    procedure Attack;
    procedure FindTarget;
    procedure Run;
    procedure Wait;

  protected
    procedure OnStop; override;
    procedure OnNoPath; override;
    procedure WasAttacked( Source : TAniFigure; Damage : Single ); override;
    procedure WasKilled( Source : TAniFigure ); override;

  public
    procedure Init; override;
    property BonusCourage : Integer read FBonusCourage write FBonusCourage;
    property BaseCourage : Integer read FBaseCourage write FBaseCourage;
    function OnCollideFigure( Target : TAniFigure ) : Boolean; override;
    procedure Execute; override;
    procedure NotifyOfDeath( Source : TAniFigure ); override;
  end;

  TOrcIdle = class( TAI )
  private
    Walking : Boolean;
    FFighting : Boolean;
    Delay : Integer;
    CenterX : Integer;
    CenterY : Integer;
    //Soft
    iLeash : Integer;
    IdleDuty : TIdleDuty;
    bCombative : Boolean;
    bTalk : Boolean;
    //temp
    Point1X : Integer;
    Point1Y : Integer;
    Point2X : Integer;
    Point2Y : Integer;
    atStart : Boolean;

  protected
    procedure OnStop; override;
    procedure WasAttacked( Source : TAniFigure; Damage : Single ); override;
  public
    procedure Init; override;
    property Fighting : Boolean read FFighting write FFighting;
    procedure Follow( Source, Target : TAniFigure ); override;
    procedure Execute; override;
    function OnCollideFigure( Target : TAniFigure ) : Boolean; override;
  end;

  TScoutIdle = class( TAI )
  private
    Walking : Boolean;
    Delay : Integer;
    CenterX : Integer;
    CenterY : Integer;
    MyGroup : TStringList;
    MyContainers : TStringList;
    MyPathCorners : TStringList;
    CurrentPath : TGameObject;
    ReturnPath : TGameObject;
    CollideCount : Integer;
    //Soft
    iLeash : Integer;
    IdleDuty : TIdleDuty;
    bCombative : Boolean;
    ReturnName : string;
    //temp

    procedure Meander;
    procedure FindTarget;
    procedure WalkPath;
    procedure GotoFriends;
    procedure GetPath( ToX, ToY : Integer );
    procedure Wait;

  protected
    procedure OnStop; override;
    procedure OnNoPath; override;
    procedure WasAttacked( Source : TAniFigure; Damage : Single ); override;
  public
    destructor Destroy; override;
    procedure Init; override;
    procedure Execute; override;
    function OnCollideFigure( Target : TAniFigure ) : Boolean; override;
  end;

function AssignMiscAI( AIName : string ) : TAI;
function RangeTest( Target, Source : TAniFigure; iDist : Integer ) : Boolean;
function GetFacing( SrcX, SrcY, TargetX, TargetY : Longint ) : Extended;

implementation
uses
  anidemo,
  BasicHumanoidAI;
const
  PI = 3.1415926535;
  pi2 = 2 * PI;

function AssignMiscAI( AIName : string ) : TAI;
var
  S : string;
const
  FailName : string = 'MiscAI.AssignMiscAI';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  Result := nil;
  try
    S := LowerCase( AIName );
    if ( S = 'companion' ) then
      Result := TCompanion.Create
    else if ( S = 'commandercombat' ) then
      Result := TCommanderCombat.Create
    else if ( S = 'orcidle' ) then
      Result := TOrcIdle.Create
    else if ( S = 'scoutidle' ) then
      Result := TScoutIdle.Create
    else if ( S = 'watchdog' ) then
    begin
      Result := TWatchDog.Create
    end
    else if ( S = 'drunk' ) then
    begin
      Result := TDrunk.Create
    end
    else if ( S = 'worm' ) then
    begin
      Result := TWorms.Create
    end
    else if ( S = 'randomchestloot' ) then
    begin
      Result := TRandomChestLoot.Create
    end
    else if ( S = 'guarddog' ) then
    begin
      Result := TGuardDog.Create
    end
    else if ( S = 'ritual' ) then
    begin
      Result := TRitual.Create;
    end
    else if ( S = 'humanoidmeleetraining' ) then
      Result := TMeleeTraining.Create
    else if ( S = 'humanoidmeleesparing' ) then
      Result := TMeleeSparing.Create
    else if ( S = 'humanoidmeleepratice' ) then
      Result := TMeleePratice.Create
    else if ( S = 'enviromentdamage' ) then
      Result := TEnviromentDamage.Create
    else if ( S = 'obelisk' ) then
      Result := TObelisk.Create
    else if ( S = 'runscript' ) then
      Result := TRunScript.Create
    else if ( S = 'cloneplayer' ) then
      Result := TCloneplayer.Create
    else
      Result := nil;


  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function RangeTest( Target, Source : TAniFigure; iDist : Integer ) : Boolean;
var
  D : Double;
const
  FailName : string = 'MiscAI.RangeTest';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  Result := False;
  try
    D := sqrt( sqr( Target.X - Source.X ) + sqr( 2 * ( Target.Y - Source.Y ) ) );
    if D <= iDist then
      Result := True;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function GetFacing( SrcX, SrcY, TargetX, TargetY : Longint ) : Extended;
var
  Slope : Single;
begin
  Result := 2;

  try

    if ( TargetX = SrcX ) then
    begin
      if ( TargetY < SrcY ) then
        Result := 2
    end
    else
    begin
      Slope := ( TargetY - SrcY ) / ( TargetX - SrcX );
      if ( TargetX < SrcX ) then
      begin
        if ( Slope >= -0.25 ) and ( Slope <= 0.25 ) then
          Result := 1
        else if ( Slope > 2 ) then
          Result := 2
        else if ( Slope < -2 ) then
          Result := 2
        else if ( Slope > 0 ) then
          Result := 1.5
        else
          Result := 1.5;
      end
      else
      begin
        if ( Slope >= -0.25 ) and ( Slope <= 0.25 ) then
          Result := 1
        else if ( Slope > 2 ) then
          Result := 2
        else if ( Slope < -2 ) then
          Result := 2
        else if ( Slope > 0 ) then
          Result := 1.5
        else
          Result := 1.5;
      end;
    end;

  except
    on E : Exception do
      Log.log( 'Error Humanoid GetFacing: ' + E.Message );
  end;
end;

{ TCompanion }

procedure TCompanion.Execute;
const
  FailName : string = 'MiscAI.TCompanion.Execute';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;

   { if Assigned(Character.track) and not(Character.track.Dead) then
       Character.say(Character.track.guid, clwhite);


    if Assigned(Character.track) and Character.track.Dead then
       character.say('Track Dead', clwhite);

    if Not(Assigned(Character.track)) then
      character.say('No Track', clwhite);
    }

    if not ( Assigned( FLeader ) ) then
    begin
      FLeader := Player;
    end;

    if assigned( character.track ) then
      if ( character.track = player ) or ( character.track = character ) or Character.track.Dead then
      begin
        character.track := nil;
        Fighting := false;
      end;

    if not ( Character.combatMode ) then
    begin
      Character.track := nil;
      fighting := false;
      BuffCount := 0;
    end;

    if BlockedShot >= 4 then
    begin
      Character.track := nil;
      fighting := false;
      BlockedShot := 0;
    end;


    if Character.CombatMode then
      if not ( Fighting ) or Character.track.Dead then
        if not ( FHoldAggressive ) and not ( FHoldDefensive ) and not ( FHoldandRun ) then
          GuardPlayer; //look for something to kill

    if ( Delay > 0 ) and not ( Walking ) then
    begin
      Dec( Delay );
      Exit;
    end;

    if CirclePoint > 535 then
      CirclePoint := Random( 360 ) + 180;

    if RunAway and not ( walking ) then
    begin
    //    player.say('runaway',clgreen);

      MoveAway; //get away from current attacker
      exit;
    end;

    if ( FMagicAggressive or FMagicDefensive or FMeleeRanged ) and Assigned( Character.Track ) and not ( walking ) then
    begin //maintain my current distance
      if Character.IsEnemy( Character.Track ) and ( TCharacter( Character.track ).track = Character ) then
      begin //I hate him and he hates me
        if RangeTest( Character.Track, Character, iDistance ) and ( Random( 2 ) = 0 ) then
        begin
      //    player.say('running cause Im scared', clpurple);
          MoveAway;
          Exit;
        end
      end;
    end;

    if not ( FHoldAggressive or FHoldDefensive or FHoldandRun ) and not ( Fighting ) and not ( assigned( Character.track ) ) then
      FollowLeader; //will follow the player till attacked or find a target

    if ( FHoldAggressive or FHoldDefensive or FHoldandRun ) and not ( Character.combatMode ) then
      FollowLeader; //will still follow player till attacker or player goes into combatmode



    if Assigned( Character.Track ) and Fighting and Character.CombatMode then
    begin //Melee group
      if FMeleeAggressive or FMeleeDefensive then
        AttackMelee;
      if FMeleeRanged then
        AttackMeleeRanged;
    end;

    if ( FMagicAggressive or FMagicDefensive ) and Assigned( Character.Track ) and Fighting and not ( walking ) and Character.CombatMode then
    begin //caster group

      if FMagicDefensive then
        if not ( HealParty ) then
          if BuffCount < MaxBuffs then
            BuffParty
          else
          begin
            AttackCaster;
          end;

      if FMagicAggressive then
        AttackCaster;
    end;

    if FHoldAggressive and Fighting then
    begin //pansie group
      FightBack;
    end;

    if FHoldDefensive and Fighting then
    begin //pansie group
      FightBack;
    end;


  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TCompanion.BuffParty;
var
  Spell : string;
const
  FailName : string = 'MiscAI.TCompanion.BuffParty';

begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Spell := strTokenAt( dSPellBook, ',', Random( StrTokenCount( dSpellBook, ',' ) ) );
    if Spells.IndexOf( spell ) <> -1 then
    begin
      Character.CurrentSpell := TSpell( Spells.Objects[ Spells.IndexOf( spell ) ] );

      if Assigned( Character.CurrentSpell ) then
        character.Cast( NPCList.Items[ Random( NpCList.Count ) ] );
    end;
    inc( BuffCount );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;


function TCompanion.HealParty : boolean;
var
  iLoop : integer;
const
  FailName : string = 'MiscAI.TCompanion.HealPlayer';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  Result := false;

  try

    if Character.CurrentSpell <> TSpell( Spells.Objects[ Spells.IndexOf( 'Heal' ) ] ) then
      Character.CurrentSpell := TSpell( Spells.Objects[ Spells.IndexOf( 'Heal' ) ] );

    for iLoop := 0 to NPCList.Count - 1 do
    begin
      if TCharacter( NPCList.Items[ iLoop ] ).wounds > ( TCharacter( NPCList.Items[ iLoop ] ).HitPoints * 0.50 ) then
      begin
        character.Cast( NPCList.Items[ iLoop ] );
        Result := true;
        break;
      end;
    end;


  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;


procedure TCompanion.Init;
var
  S : string;
  i : integer;
const
  FailName : string = 'MiscAI.TCompanion.Init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try


    AttackFrame := 0;
    GuardFrame := 0;
    ShotCounter := 0;
    BlockedShot := 0;
    MaxShots := Random( 15 ) + 15;

    BuffCount := 0;
    MaxBuffs := NPCList.count + Random( 4 ) + 1;

    Spells := character.SpellList;
    CirclePoint := Random( 360 ) + 180;
    NukeCounter := 0;
    CastTimes := Random( 15 ) + 15;
    character.MakeAlly( 'party' );

    FFighter := character.TitleExists( 'Fighter' );
    FScout := character.TitleExists( 'Scout' );
    FMage := character.TitleExists( 'Mage' );

    Character.Vision := 0;
    character.Hearing := 0;
    Character.Smell := 0;
    Character.MysticVision := 300;

    SetMeleeRanged( character.TitleExists( 'MeleeRanged' ) );
    SetMeleeAggressive( character.TitleExists( 'MeleeAggressive' ) );
    SetMeleeDefensive( character.TitleExists( 'MeleeDefensive' ) );
    SetMagicAggressive( character.TitleExists( 'MagicAggressive' ) );
    SetMagicDefensive( character.TitleExists( 'MagicDefensive' ) );

    SetHoldAggressive( character.TitleExists( 'HoldAggressive' ) );
    SetHoldDefensive( character.TitleExists( 'HoldDefensive' ) );
    SetHoldandRun( character.TitleExists( 'HoldandRun' ) );

    if not ( FMeleeRanged ) and not ( FMeleeAggressive ) and not ( FMeleeDefensive ) and
      not ( FMagicAggressive ) and not ( FMagicDefensive ) and not ( FHoldAggressive ) and
      not ( FHoldDefensive ) and not ( FHoldandRun ) then
      SetMeleeDefensive( true ); //Default Setting

    if character.TitleExists( 'Flame' ) then
      oSpellBook := oSpellBook + 'flame,';
    if character.TitleExists( 'Frost' ) then
      oSpellBook := oSpellBook + 'frost,';
    if character.TitleExists( 'Charge' ) then
      oSpellBook := oSpellBook + 'charge,';
    if character.TitleExists( 'Hold' ) then
      oSpellBook := oSpellBook + 'hold,';
    if character.TitleExists( 'Shrapnel' ) then
      oSpellBook := oSpellBook + 'shrapnel,';
    if character.TitleExists( 'Great Hand' ) then
      oSpellBook := oSpellBook + 'great hand,';
    if character.TitleExists( 'Shock' ) then
      oSpellBook := oSpellBook + 'shock,';
    if character.TitleExists( 'Great Wolf' ) then
      oSpellBook := oSpellBook + 'Great Wolf,';
    if character.TitleExists( 'Blizzard' ) then
      oSpellBook := oSpellBook + 'Blizzard,';
    if character.TitleExists( 'Flame Strike' ) then
      oSpellBook := oSpellBook + 'flame strike,';
    if character.TitleExists( 'Forget' ) then
      oSpellBook := oSpellBook + 'forget,';


    if character.TitleExists( 'Aura of Iron' ) then
      dSpellBook := dSpellBook + 'Aura of Iron,';
    if character.TitleExists( 'Aura of Steel' ) then
      dSpellBook := dSpellBook + 'Aura of Steel,';
    if character.TitleExists( 'Shadow' ) then
      dSpellBook := dSpellBook + 'Shadow,';
    if character.TitleExists( 'Protection from Fire' ) then
      dSpellBook := dSpellBook + 'Protection from Fire,';
    if character.TitleExists( 'Protection from Cold' ) then
      dSpellBook := dSpellBook + 'Protection from Cold,';
    if character.TitleExists( 'Protection from Lightning' ) then
      dSpellBook := dSpellBook + 'Protection from Lightning,';
    if character.TitleExists( 'Protection from Magic' ) then
      dSpellBook := dSpellBook + 'Protection from Magic,';
    if character.TitleExists( 'Protection from All' ) then
      dSpellBook := dSpellBook + 'Protection from All,';
    if character.TitleExists( 'Deflect' ) then
      dSpellBook := dSpellBook + 'Deflect,';
    if character.TitleExists( 'Mana Thief' ) then
      dSpellBook := dSpellBook + 'Mana Thief,';

    StrStripLast( oSpellBook );
    StrStripLast( dSpellBook );

  //  if character.TitleExists('HealFirst') then
  //  begin
  //    Character.AddTitle('Heal');
  //  end;

    character.Properties[ 'idleduty' ] := 'stand';

    S := Character.Properties[ 'distance' ];
    try
      if S = '' then
        iDistance := 175
      else
        iDistance := StrToInt( S );
    except
      iDistance := 175;
    end;

    if character.TitleExists( 'Combative' ) then
    begin
        //  character.RechargeRate := player.RechargeRate + i;
        //  character.HealingRate := player.HealingRate + i;

          //companion is a mage
      if FMage then
      begin
        S := LowerCase( Character.Properties[ 'BalanceWithPlayer' ] );
        try
          if ( S <> '' ) and ( s <> '0' ) then
          begin
            i := StrToInt( s );
            if i <> 0 then
            begin
              if player.TitleExists( 'Apprentice' ) then
              begin
                character.Mysticism := ( ( player.Mysticism * 3 ) div 4 ) + i;
                character.perception := ( ( player.perception * 3 ) div 4 ) + i;
                character.Coordination := ( ( player.Coordination * 3 ) div 4 ) + i;
                character.HitPoints := ( ( player.Perception * 2 ) );
              end;
              if player.TitleExists( 'Hunter' ) then
              begin
                character.Mysticism := ( ( player.Stealth * 3 ) div 4 ) + i;
                character.perception := ( ( player.Coordination * 3 ) div 4 ) + i;
                character.Coordination := ( ( player.Coordination * 3 ) div 4 ) + i;
                character.HitPoints := ( ( player.strength * 2 ) );
              end;
              if player.TitleExists( 'Squire' ) then
              begin
                character.Mysticism := ( ( player.Combat * 3 ) div 4 ) + i;
                character.perception := ( ( player.Strength * 3 ) div 4 ) + i;
                character.Coordination := ( ( player.Strength * 3 ) div 4 ) + i;
                character.HitPoints := ( ( player.Coordination * 2 ) );
              end;
            end;
          end;
        except
        end;
      end;

          //Companion is a Scout
      if FScout then
      begin
        S := LowerCase( Character.Properties[ 'BalanceWithPlayer' ] );
        try
          if ( S <> '' ) and ( s <> '0' ) then
          begin
            i := StrToInt( s );
            if i <> 0 then
            begin
              if player.TitleExists( 'Apprentice' ) then
              begin
                character.Combat := ( ( ( player.mysticism * 3 ) div 4 ) + i );
                character.strength := ( ( player.perception * 6 ) div 10 ) + i;
                character.Coordination := ( ( player.perception * 2 ) div 3 ) + i;
                character.Stealth := player.Mysticism + i;
                character.HitPoints := ( ( player.Perception * 2 ) );
              end;
              if player.TitleExists( 'Hunter' ) then
              begin
                character.Combat := player.combat + i;
                character.strength := ( ( player.Strength * 6 ) div 10 ) + i;
                character.Coordination := ( ( player.Coordination * 2 ) div 3 ) + i;
                character.Stealth := player.Stealth + i;
                character.HitPoints := ( ( player.strength * 2 ) );
              end;
              if player.TitleExists( 'Squire' ) then
              begin
                character.Combat := ( ( ( player.combat * 3 ) div 4 ) + i );
                character.strength := ( ( player.Coordination * 6 ) div 10 ) + i;
                character.Coordination := ( ( player.strength * 2 ) div 3 ) + i;
                character.Stealth := player.Combat + i;
                character.HitPoints := ( ( player.Coordination * 2 ) );
              end;
            end;
          end;
        except
        end;
      end;
          //Companion is a fighter
      if FFighter then
      begin
        S := LowerCase( Character.Properties[ 'BalanceWithPlayer' ] );
        try
          if ( S <> '' ) and ( s <> '0' ) then
          begin
            i := StrToInt( s );
            if i <> 0 then
            begin
              if player.TitleExists( 'Apprentice' ) then
              begin
                character.Combat := ( ( ( player.Mysticism * 3 ) div 4 ) + i );
                character.strength := ( ( player.perception * 3 ) div 4 ) + i;
                character.HitPoints := ( ( player.Perception * 2 ) );
                character.Coordination := ( ( player.Coordination * 2 ) div 3 ) + i;
                              //   Character.AttackRecovery := Character.AttackRecovery + (player.attackRecovery div i);
              end;
              if player.TitleExists( 'Hunter' ) then
              begin
                character.Combat := ( ( ( player.Stealth * 3 ) div 4 ) + i );
                character.strength := ( ( player.Coordination * 3 ) div 4 ) + i;
                character.HitPoints := ( ( player.Coordination * 2 ) );
                character.Coordination := ( ( player.Coordination * 3 ) div 4 ) + i;
                           //      Character.AttackRecovery := Character.AttackRecovery + (player.attackRecovery div i);
              end;

              if player.TitleExists( 'Squire' ) then
              begin
                character.Combat := ( ( player.Combat * 3 ) div 4 ) + i;
                character.strength := ( ( player.Strength * 3 ) div 4 ) + i;
                character.Coordination := ( ( player.Coordination * 2 ) div 3 ) + i;
                          //       Character.AttackRecovery := Character.AttackRecovery + (player.attackRecovery div i);
                character.HitPoints := ( ( player.strength * 2 ) );
              end;
            end;
          end;
        except
        end;
      end;
    end;

    S := Character.Properties[ 'iSpeed' ];
    try
      if ( S <> '' ) and ( s <> '0' ) then
        TCharacterResource( character.Resource ).Speed := StrToInt( S );
    except
    end;

    S := LowerCase( Character.Properties[ 'Moveable' ] );
    try
      if S = '' then
        bMove := True
      else if S = 'false' then
        bMove := False
      else
        bMove := True;
    except
      bMove := True;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;


procedure TCompanion.SetMeleeRanged( value : Boolean );
begin
  if Value then
  begin
    FMeleeRanged := true;
    FMeleeAggressive := false;
    FMeleeDefensive := false;
    FMagicAggressive := false;
    FMagicDefensive := false;
    FHoldAggressive := false;
    FHoldDefensive := false;
    FHoldandRun := false;
    character.AddTitle( 'MeleeRanged' );
    character.RemoveTitle( 'MeleeAggressive' );
    character.RemoveTitle( 'MeleeDefensive' );
    character.RemoveTitle( 'MagicAggressive' );
    character.RemoveTitle( 'MagicDefensive' );
    character.RemoveTitle( 'HoldAggressive' );
    character.RemoveTitle( 'HoldDefensive' );
    character.RemoveTitle( 'HoldandRun' );
  end;
end;

procedure TCompanion.SetMeleeAggressive( value : Boolean );
begin
  if Value then
  begin
    FMeleeRanged := false;
    FMeleeAggressive := true;
    FMeleeDefensive := false;
    FMagicAggressive := false;
    FMagicDefensive := false;
    FHoldAggressive := false;
    FHoldDefensive := false;
    FHoldandRun := false;
    character.RemoveTitle( 'MeleeRanged' );
    character.AddTitle( 'MeleeAggressive' );
    character.RemoveTitle( 'MeleeDefensive' );
    character.RemoveTitle( 'MagicAggressive' );
    character.RemoveTitle( 'MagicDefensive' );
    character.RemoveTitle( 'HoldAggressive' );
    character.RemoveTitle( 'HoldDefensive' );
    character.RemoveTitle( 'HoldandRun' );
  end;
end;

procedure TCompanion.SetMeleeDefensive( value : Boolean );
begin
  if Value then
  begin
    FMeleeRanged := false;
    FMeleeAggressive := false;
    FMeleeDefensive := true;
    FMagicAggressive := false;
    FMagicDefensive := false;
    FHoldAggressive := false;
    FHoldDefensive := false;
    FHoldandRun := false;
    character.RemoveTitle( 'MeleeRanged' );
    character.RemoveTitle( 'MeleeAggressive' );
    character.AddTitle( 'MeleeDefensive' );
    character.RemoveTitle( 'MagicAggressive' );
    character.RemoveTitle( 'MagicDefensive' );
    character.RemoveTitle( 'HoldAggressive' );
    character.RemoveTitle( 'HoldDefensive' );
    character.RemoveTitle( 'HoldandRun' );
  end;

end;

procedure TCompanion.SetMagicAggressive( value : Boolean );
begin
  if Value then
  begin
    FMeleeRanged := false;
    FMeleeAggressive := false;
    FMeleeDefensive := false;
    FMagicAggressive := true;
    FMagicDefensive := false;
    FHoldAggressive := false;
    FHoldDefensive := false;
    FHoldandRun := false;
    character.RemoveTitle( 'MeleeRanged' );
    character.RemoveTitle( 'MeleeAggressive' );
    character.RemoveTitle( 'MeleeDefensive' );
    character.AddTitle( 'MagicAggressive' );
    character.RemoveTitle( 'MagicDefensive' );
    character.RemoveTitle( 'HoldAggressive' );
    character.RemoveTitle( 'HoldDefensive' );
    character.RemoveTitle( 'HoldandRun' );
  end;

end;

procedure TCompanion.SetMagicDefensive( value : Boolean );
begin
  if Value then
  begin
    FMeleeRanged := false;
    FMeleeAggressive := false;
    FMeleeDefensive := false;
    FMagicAggressive := false;
    FMagicDefensive := true;
    FHoldAggressive := false;
    FHoldDefensive := false;
    FHoldandRun := false;
    character.RemoveTitle( 'MeleeRanged' );
    character.RemoveTitle( 'MeleeAggressive' );
    character.RemoveTitle( 'MeleeDefensive' );
    character.RemoveTitle( 'MagicAggressive' );
    character.AddTitle( 'MagicDefensive' );
    character.RemoveTitle( 'HoldAggressive' );
    character.RemoveTitle( 'HoldDefensive' );
    character.RemoveTitle( 'HoldandRun' );
  end;
end;

procedure TCompanion.SetHoldAggressive( value : Boolean );
begin
  if Value then
  begin
    FMeleeRanged := false;
    FMeleeAggressive := false;
    FMeleeDefensive := false;
    FMagicAggressive := false;
    FMagicDefensive := false;
    FHoldAggressive := true;
    FHoldDefensive := false;
    FHoldandRun := false;
    character.RemoveTitle( 'MeleeRanged' );
    character.RemoveTitle( 'MeleeAggressive' );
    character.RemoveTitle( 'MeleeDefensive' );
    character.RemoveTitle( 'MagicAggressive' );
    character.RemoveTitle( 'MagicDefensive' );
    character.AddTitle( 'HoldAggressive' );
    character.RemoveTitle( 'HoldDefensive' );
    character.RemoveTitle( 'HoldandRun' );
  end;
end;

procedure TCompanion.SetHoldDefensive( value : Boolean );
begin
  if Value then
  begin
    FMeleeRanged := false;
    FMeleeAggressive := false;
    FMeleeDefensive := false;
    FMagicAggressive := false;
    FMagicDefensive := false;
    FHoldAggressive := false;
    FHoldDefensive := true;
    FHoldandRun := false;
    character.RemoveTitle( 'MeleeRanged' );
    character.RemoveTitle( 'MeleeAggressive' );
    character.RemoveTitle( 'MeleeDefensive' );
    character.RemoveTitle( 'MagicAggressive' );
    character.RemoveTitle( 'MagicDefensive' );
    character.RemoveTitle( 'HoldAggressive' );
    character.AddTitle( 'HoldDefensive' );
    character.RemoveTitle( 'HoldandRun' );
  end;

end;

procedure TCompanion.SetHoldandRun( value : Boolean );
begin
  if Value then
  begin
    FMeleeRanged := false;
    FMeleeAggressive := false;
    FMeleeDefensive := false;
    FMagicAggressive := false;
    FMagicDefensive := false;
    FHoldAggressive := false;
    FHoldDefensive := false;
    FHoldandRun := true;
    character.RemoveTitle( 'MeleeRanged' );
    character.RemoveTitle( 'MeleeAggressive' );
    character.RemoveTitle( 'MeleeDefensive' );
    character.RemoveTitle( 'MagicAggressive' );
    character.RemoveTitle( 'MagicDefensive' );
    character.RemoveTitle( 'HoldAggressive' );
    character.RemoveTitle( 'HoldDefensive' );
    character.AddTitle( 'HoldandRun' );
  end;

end;

function TCompanion.OnCollideFigure( Target : TAniFigure ) : boolean;
const
  FailName : string = 'TCompanion.OnCollideFigure';
var
  r : Integer;
  T : single;
  X, Y : Integer;

begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  Result := False;
  try
    if assigned( target ) then
      if Target = Current then
      begin
        r := random( 150 );
        T := pi2 * random( 360 ) / 360;
        X := round( r * cos( T ) ) + FLeader.X + 10;
        Y := round( r * sin( T ) / 2 ) + FLeader.Y + 10;
        Character.WalkTo( X, Y, 4 );
      end;

  except
    on E : Exception do
      Log.log( 'Error TCompanion CollideFigure: ' + E.Message );
  end;
end;


procedure TCompanion.OnStop;
const
  FailName : string = 'MiscAI.TCompanion.OnStop';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    walking := False;
    if Assigned( Character.Track ) then
      Character.Face( Character.Track.X, Character.Track.Y )
    else
    begin
      character.Face( Player.x, Player.y );
      character.stand;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TCompanion.OnNoPath;
const
  FailName : string = 'MiscAI.TCompanion.OnNoPath';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    walking := False;

    if Assigned( Character.Track ) then
    begin
      Character.track := nil;
      fighting := false;
    end;


    //  Character.Face(Character.Track.X, Character.Track.Y);
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TCompanion.WasAttacked( Source : TAniFigure; Damage : Single );
const
  FailName : string = 'MiscAI.TCompanion.WasAttacked';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
    Delay := 0;
    if Source is TCharacter then
      if Character.IsEnemy( TCharacter( Source ) ) then
      begin
        Character.Track := TCharacter( Source );

        if FHoldAggressive or FMeleeAggressive or FMeleeDefensive then
          Fighting := True;

        if FMagicAggressive or FMagicDefensive or FMeleeRanged then
        begin
          if Damage > ( ( Character.HitPoints - Character.wounds ) * 0.10 ) then
            RunAway := True;
//          Fighting := false;
        end;

        if FHoldAggressive and ( Character.wounds > ( Character.HitPoints * 0.25 ) ) then
        begin
          RunAway := True;
//           walking := false;
//           Fighting := false;
        end;


        if FHoldDefensive and ( Character.wounds > ( Character.HitPoints * 0.50 ) ) then
        begin
          RunAway := True;
//           walking := false;
//           Fighting := false;
        end;

        if FHoldandRun then
        begin
          RunAway := True;
 //          Fighting := false;
        end;
      end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;


procedure TCompanion.GuardPlayer;
var
  List : TStringList;

const
  FailName : string = 'MiscAI.TCompanion.GuardPlayer';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    //watch for good guys
    if GuardFrame = 0 then
      GuardFrame := FrameCount + 40;

    if ( frameCount > GuardFrame ) then
    begin
      GuardFrame := 0;
      List := GetPerceptibleEnemies( Character, 1 );
      if Assigned( List ) then
      begin
        delay := 0;
        if List.Count = 1 then
          Character.Track := TCharacter( List.Objects[ 0 ] )
        else
          Character.Track := TCharacter( List.Objects[ random( List.Count ) ] );
        Fighting := True;
        List.Free;
      end
      else
        Fighting := False;
    end;
    if Assigned( Character.track ) then
    begin
      if ( Character.RangeTo( Character.Track.X, Character.Track.Y ) > 300 ) or not ( Game.LineOfCollision( Character.x, character.y, Character.track.x, Character.track.y ) ) then
      begin
        Character.Track := nil;
        fighting := false;
        Character.RunTo( Player.x, Player.y, 4 );
        walking := true;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TCompanion.FollowLeader;
var
  r : Integer;
  T : single;
  X, Y : Integer;
  iRange : Double;
const
  FailName : string = 'MiscAI.BTCompanion.FollowPlayer';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if NewFrame = 0 then
      NewFrame := FrameCount + 16;

    BuffCount := 0;
    Character.track := nil;
    fighting := false;


    if ( frameCount > NewFrame ) then
    begin //stay close to the leader
      NewFrame := 0;
      iRange := ( character.Rangeto( Player.x, Player.y ) );
      if ( iRange > 160 ) then
      begin
        if ( iRange > 300 ) then
        begin
          case Fleader.Facing of
            fNE, fEE, fSE :
              begin
                if not ( game.LineOfCollision( FLeader.x - 50, FLeader.y, FLeader.X, FLeader.y ) ) then
                  Character.RunTo( FLeader.X, FLeader.Y, 48 )
                else
                  Character.RunTo( FLeader.X - 50, FLeader.Y, 48 );
              end;
            fNW, fWW, fSW :
              begin
                if not ( game.LineOfCollision( FLeader.x + 50, FLeader.y, FLeader.X, FLeader.y ) ) then
                  Character.RunTo( FLeader.X, FLeader.Y, 48 )
                else
                  Character.RunTo( FLeader.X + 50, FLeader.Y, 48 );
              end;
            fSS :
              begin
                if not ( game.LineOfCollision( FLeader.x, FLeader.y + 50, FLeader.X, FLeader.y ) ) then
                  Character.RunTo( FLeader.X, FLeader.Y, 48 )
                else
                  Character.RunTo( FLeader.X, FLeader.Y + 50, 48 );
              end;
            fNN :
              begin
                if not ( game.LineOfCollision( FLeader.x, FLeader.y - 50, FLeader.X, FLeader.y ) ) then
                  Character.RunTo( FLeader.X, FLeader.Y, 48 )
                else
                  Character.RunTo( FLeader.X, FLeader.Y - 50, 48 );
              end;
          end;
          exit;
        end;


        if character.PartyMember then
        begin
          case Player.Facing of
            fNE, fEE, fSE :
              begin
                case NPCList.IndexOf( Character ) of
                  0 :
                    if not ( game.LineOfCollision( Player.x - 50, Player.y, Player.X, Player.y ) ) then
                      Character.RunTo( Player.X, Player.Y, 48 )
                    else
                      Character.RunTo( Player.X - 50, Player.Y, 48 );
                  1 :
                    if not ( game.LineOfCollision( Player.x - 50, Player.y + 50, Player.X, Player.y ) ) then
                      Character.RunTo( Player.X, Player.Y, 48 )
                    else
                      Character.RunTo( Player.X - 50, Player.Y + 50, 48 );
                  2 :
                    if not ( game.LineOfCollision( Player.x - 50, Player.y - 50, Player.X, Player.y ) ) then
                      Character.RunTo( Player.X, Player.Y, 48 )
                    else
                      Character.RunTo( Player.X - 50, Player.Y - 50, 48 );
                  3 :
                    if not ( game.LineOfCollision( Player.x - 50, Player.y, Player.X, Player.y ) ) then
                      Character.RunTo( Player.X, Player.Y, 48 )
                    else
                      Character.RunTo( Player.X - 50, Player.Y, 48 );
                  4 :
                    if not ( game.LineOfCollision( Player.x - 85, Player.y, Player.X, Player.y ) ) then
                      Character.RunTo( Player.X, Player.Y, 48 )
                    else
                      Character.RunTo( Player.X - 85, Player.Y, 48 );
                end;
              end;
            fNW, fWW, fSW :
              begin
                case NPCList.IndexOf( Character ) of
                  0 :
                    if not ( game.LineOfCollision( Player.x + 50, Player.y, Player.X, Player.y ) ) then
                      Character.RunTo( Player.X, Player.Y, 48 )
                    else
                      Character.RunTo( Player.X + 50, Player.Y, 48 );
                  1 :
                    if not ( game.LineOfCollision( Player.x + 50, Player.y + 50, Player.X, Player.y ) ) then
                      Character.RunTo( Player.X, Player.Y, 48 )
                    else
                      Character.RunTo( Player.X + 50, Player.Y + 50, 48 );
                  2 :
                    if not ( game.LineOfCollision( Player.x + 50, Player.y - 50, Player.X, Player.y ) ) then
                      Character.RunTo( Player.X, Player.Y, 48 )
                    else
                      Character.RunTo( Player.X + 50, Player.Y - 50, 48 );
                  3 :
                    if not ( game.LineOfCollision( Player.x + 50, Player.y, Player.X, Player.y ) ) then
                      Character.RunTo( Player.X, Player.Y, 48 )
                    else
                      Character.RunTo( Player.X + 50, Player.Y, 48 );
                  4 :
                    if not ( game.LineOfCollision( Player.x + 85, Player.y, Player.X, Player.y ) ) then
                      Character.RunTo( Player.X, Player.Y, 48 )
                    else
                      Character.RunTo( Player.X + 85, Player.Y, 48 );
                end;
              end;
            fSS :
              begin
                case NPCList.IndexOf( Character ) of
                  0 :
                    if not ( game.LineOfCollision( Player.x, Player.y + 50, Player.X, Player.y ) ) then
                      Character.RunTo( Player.X, Player.Y, 48 )
                    else
                      Character.RunTo( Player.X, Player.Y + 50, 48 );
                  1 :
                    if not ( game.LineOfCollision( Player.x - 50, Player.y - 50, Player.X, Player.y ) ) then
                      Character.RunTo( Player.X, Player.Y, 48 )
                    else
                      Character.RunTo( Player.X - 50, Player.Y - 50, 48 );
                  2 :
                    if not ( game.LineOfCollision( Player.x + 50, Player.y - 50, Player.X, Player.y ) ) then
                      Character.RunTo( Player.X, Player.Y, 48 )
                    else
                      Character.RunTo( Player.X + 50, Player.Y - 50, 48 );
                  3 :
                    if not ( game.LineOfCollision( Player.x, Player.y - 50, Player.X, Player.y ) ) then
                      Character.RunTo( Player.X, Player.Y, 48 )
                    else
                      Character.RunTo( Player.X, Player.Y - 50, 48 );
                  4 :
                    if not ( game.LineOfCollision( Player.x, Player.y - 85, Player.X, Player.y ) ) then
                      Character.RunTo( Player.X, Player.Y, 48 )
                    else
                      Character.RunTo( Player.X, Player.Y - 85, 48 );
                end;
              end;
            fNN :
              begin
                case NPCList.IndexOf( Character ) of
                  0 :
                    if not ( game.LineOfCollision( Player.x, Player.y - 50, Player.X, Player.y ) ) then
                      Character.RunTo( Player.X, Player.Y, 48 )
                    else
                      Character.RunTo( Player.X, Player.Y - 50, 48 );
                  1 :
                    if not ( game.LineOfCollision( Player.x - 50, Player.y + 50, Player.X, Player.y ) ) then
                      Character.RunTo( Player.X, Player.Y, 48 )
                    else
                      Character.RunTo( Player.X - 50, Player.Y + 50, 48 );
                  2 :
                    if not ( game.LineOfCollision( Player.x + 50, Player.y + 50, Player.X, Player.y ) ) then
                      Character.RunTo( Player.X, Player.Y, 48 )
                    else
                      Character.RunTo( Player.X + 50, Player.Y + 50, 48 );
                  3 :
                    if not ( game.LineOfCollision( Player.x, Player.y + 50, Player.X, Player.y ) ) then
                      Character.RunTo( Player.X, Player.Y, 48 )
                    else
                      Character.RunTo( Player.X, Player.Y + 50, 48 );
                  4 :
                    if not ( game.LineOfCollision( Player.x, Player.y + 85, Player.X, Player.y ) ) then
                      Character.RunTo( Player.X, Player.Y, 48 )
                    else
                      Character.RunTo( Player.X, Player.Y + 85, 48 );
                end;
              end;
          end;
        end
        else
          Character.RunTo( FLeader.X + ( random( 220 ) - 110 ), FLeader.Y + ( random( 220 ) - 110 ), 48 );

        Stopped := false;
      end;
    end;
  except
    on E : Exception do
      Log.log( 'Error Companion FollowPlayer: ' + E.Message );
  end;
end;

procedure TCompanion.AttackMelee;
const
  FailName : string = 'MiscAI.TCompanion.AttackMelee';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if Walking then
    begin
      try
        if AttackFrame = 0 then
          AttackFrame := FrameCount + 10;

        if ( frameCount > AttackFrame ) then
        begin
          AttackFrame := 0;
          if ( character.X <> character.StartX ) and ( character.Y <> character.StartY ) then
          begin
            character.track := nil;
            fighting := false;
            exit;
          end;
          if FMeleeDefensive and ( character.RangeTo( Player.x, Player.y ) < 300 ) then
          begin //dont get to far away;
            Character.Track := nil;
            fighting := false;
            Character.RunTo( Player.x, Player.y, 4 );
          end
          else if FMeleeAggressive and not ( Character.InRange( Character.Track ) ) then
          begin //go get the bastard
            Character.RunTo( Character.Track.X, Character.Track.Y, 4 );
          end
          else
            walking := False;
        end;
      except
        on E : Exception do
          Log.log( 'Error Companion AttackMelee1: ' + E.Message );
      end;

    end;


    if ReadyToAttack then
    begin //hit him then
      try
        Character.Attack( Character.Track );
        ReadyToAttack := False;
        Walking := False;
      except
        on E : Exception do
          Log.log( 'Error Companion AttackMelee2: ' + E.Message );
      end;
    end;


    if not ( Walking ) and not ( ReadyToAttack ) then
    begin
      try
        if Assigned( Character.Track ) then
        begin
          if TCharacter( Character.Track ).Dead or not ( TCharacter( Character.Track ).enabled ) then
          begin //forget about him and go back to the player
            Character.Track := nil;
            Fighting := False;
            Character.RunTo( Player.x, Player.y, 4 );
          end
          else
          begin
            if Character.InRange( Character.Track ) then
            begin //are we there yet?
              Delay := 0;
              readyToAttack := True;
            end
            else
            begin //well go get him
              Delay := 0;
              Walking := True;
              Character.RunTo( Character.Track.X, Character.Track.Y, 48 );
            end;
          end;
        end;
      except
        on E : Exception do
          Log.log( 'Error Companion AttackMelee3: ' + E.Message );
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TCompanion.FightBack;
const
  FailName : string = 'MiscAI.TCompanion.FightBack';
begin
  try
    if walking then
      exit;

    if Assigned( Character.Track ) then
    begin
      if TCharacter( Character.Track ).Dead or not ( TCharacter( Character.Track ).enabled ) then
      begin //forget about him
        Character.Track := nil;
        Fighting := False;
      end
      else if Character.InRange( Character.Track ) then
      begin //are we there yet?
        Character.Attack( Character.Track );
      end;
    end;
  except
    on E : Exception do
      Log.log( 'Error Companion FightBack: ' + E.Message );
  end;
end;

function TCompanion.InRange( Target : TAniFigure ) : Boolean;
var
  D : Double;
const
  FailName : string = 'THumanoidArcherCombat.InRange';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  Result := False;
  try
    D := sqrt( sqr( Target.X - Character.X ) + sqr( 2 * ( Target.Y - Character.Y ) ) );
    Result := ( D <= Target.Radius + Character.Radius + Character.Range ) and ( Game.LineOfSight( Character.X, Character.Y, Target.X, Target.Y ) );
  except on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;


procedure TCompanion.AttackMeleeRanged;
const
  FailName : string = 'MiscAI.TCompanion.AttackMelee';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if walking then
      exit;
    if TCharacter( Character.Track ).Dead or not ( TCharacter( Character.Track ).enabled ) then
    begin
      Character.Track := nil;
      fighting := false;
      Character.RunTo( Player.x, Player.y, 4 );
      ShotCounter := 0;
      walking := true;
    end
    else
    begin
     // if (Character.RangeTo(Character.Track.X, Character.Track.Y) > 51) and (Random(3) = 0) then
     // begin
      if ( ShotCounter < MaxShots ) then
      begin //make sure I have a shot
        if not ( IsAnybodyInTheWay( character, TCharacter( Character.track ), 1 ) ) or ( Random( 3 ) = 0 ) then
        begin
          Inc( ShotCounter );
          Character.Face( Character.Track.x, Character.Track.y );
                          //if Character.Inrange(Character.Track) then
          if InRange( Character.Track ) then
          begin
            Character.Attack( Character.Track );
          end
          else
          begin
                     // Find Better position to shoot
            Character.RunTo( Character.Track.X, Character.Track.Y, 64 )
          end;
        end
        else
        begin
//                 player.say('blocked',clred);
          Inc( BlockedShot );
          BattleTactic;
        end;
      end
      else
      begin
//          player.say('move on',clblue);
        BattleTactic;
      end;
   //   end
   //   else
   //   begin
  //        if random(2) = 1 then
  //           BattleTactic
  //        else
   //          if InRange(Character.Track) then
   //             Character.Attack(Character.Track);
    //  end;
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidArcher Attack: ' + E.Message );

  end;
end;


procedure TCompanion.BattleTactic;
var
  r : Integer;
  T : single;
  X, Y : Integer;
const
  FailName : string = 'THumanoidArcherCombat.BattleTactic';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    ShotCounter := 0;
    NukeCounter := 0;

    if not Walking then
    begin
      Walking := True;
      inc( CirclePoint, 45 );
      r := iDistance;
      T := pi2 * CirclePoint / 360;
      X := round( r * cos( T ) ) + TCharacter( Character.Track ).X;
      Y := round( r * sin( T ) / 2 ) + TCharacter( Character.Track ).Y;

      Character.WalkTo( X, Y, 48 );
    end;
  except
    on E : Exception do
      Log.log( 'Error Companion BattleTactic: ' + E.Message );

  end;
end;

procedure TCompanion.AttackCaster;
var
  spell : string;
const
  FailName : string = 'MiscAI.TCompanion.AttackCaster';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if walking then
      exit;
    if TCharacter( Character.Track ).Dead or not ( TCharacter( Character.Track ).enabled ) then
    begin
      Character.Track := nil;
      fighting := False;
      NukeCounter := 0;
      if BuffCount > 0 then
        dec( BuffCount );
      Character.RunTo( Player.x, Player.y, 4 );
      walking := true;
    end
    else
    begin
      if not ( RangeTest( Character.Track, Character, 50 ) ) then
      begin
        if ( NukeCounter < CastTimes ) then
        begin
          if not ( IsAnybodyInTheWay( character, TCharacter( Character.track ), 1 ) ) or ( random( 3 ) = 0 ) then
          begin
            try
              if bMove then
                Inc( NukeCounter );

              Character.Face( Character.Track.x, Character.Track.y );
              Spell := strTokenAt( oSPellBook, ',', Random( StrTokenCount( oSpellBook, ',' ) ) );
              if Spells.IndexOf( spell ) <> -1 then
              begin
                if Character.currentSpell <> TSpell( Spells.Objects[ Spells.IndexOf( spell ) ] ) then
                  Character.CurrentSpell := TSpell( Spells.Objects[ Spells.IndexOf( spell ) ] );
                if ( character.Mana - character.Drain ) >= Character.CurrentSpell.Drain( Character ) then
                  character.Cast( Character.Track )
                else
                  Delay := Random( 360 ) + 120;
              end;
            except
              on E : Exception do
                Log.log( 'Error Companion AttackCaster1: ' + E.Message );
            end;
          end
          else
          begin
            Inc( BlockedShot );
            BattleTactic;
          end;
        end
        else
          BattleTactic;

      end
      else if random( 2 ) = 1 then
      begin
        try
          Character.Face( Character.Track.x, Character.Track.y );
          if Character.currentSpell <> TSpell( Spells.Objects[ Spells.IndexOf( 'Push' ) ] ) then
            Character.CurrentSpell := TSpell( Spells.Objects[ Spells.IndexOf( 'Push' ) ] );
          if ( character.Mana - character.Drain ) >= Character.CurrentSpell.Drain( Character ) then
            character.Cast( Character.Track )
          else if Character.Inrange( Character.Track ) then
            Character.Attack( Character.Track );
        except
          on E : Exception do
            Log.log( 'Error Companion AttackCaster2: ' + E.Message );
        end;
      end
      else
      begin
        try
            //RunOrFight := False;
          if Character.Inrange( Character.Track ) then
            Character.Attack( Character.Track );
        except
          on E : Exception do
            Log.log( 'Error Companion AttackCaster3: ' + E.Message );
        end;
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TCompanion.MoveAway;
const
  FailName : string = 'MiscAI.TCompanion.MoveAway';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Walking := True;
    if Assigned( Character.Track ) then
      Character.Face( Character.Track.X, Character.Track.Y );

    if Pos( 'E', character.FacingString ) <> 0 then
      Character.RunTo( Character.X - 200, Character.Y + random( 400 ) - 200, 48 )
    else if Pos( 'W', character.FacingString ) <> 0 then
      Character.RunTo( Character.X + 200, Character.Y + random( 400 ) - 200, 48 )
    else if Pos( 'SS', character.FacingString ) <> 0 then
      Character.RunTo( Character.X + random( 400 ) - 200, Character.Y - 200, 48 )
    else
      Character.RunTo( Character.X + random( 400 ) - 200, Character.Y + 200, 48 );

    RunAway := False;
  except
    on E : Exception do
      Log.log( 'Error Companion MoveAway: ' + E.Message );
  end;
end;

destructor TCompanion.Destroy;
const
  FailName : string = 'MiscAI.TCompanion.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
    if Assigned( Spells ) then
      Spells.Free;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;
{*************************************************************************************}

{TGuardDog}

procedure TGuardDog.Execute;
const
  FailName : string = 'MiscAI.TGuardDog.Execute';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
    //Temp find player stuff
 //   if assigned(character.track) then
 //   character.say('Tracking '+ character.track.GUID, clblue);
    if StandInterval > 10 then
      if ( FrameCount mod StandInterval ) = 0 then
      begin
        case Random( 2 ) of
          0 : Character.StandAction := newStand;
          1 : Character.StandAction := oldStand;
        end;
      end;

    if assigned( character.track ) then
      if ( character.track = FMaster ) or ( character.track = character ) or Character.track.Dead then
      begin
        character.track := nil;
        Fighting := false;
      end;

    if not ( Fighting ) and PlayIdleSFX then
      PlaySounds;


  //  if Not(Character.combatMode) and Assigned(character.track) then
  //  begin
  //    Character.track := nil;
  //    fighting := false;
  //  end;

    if not ( assigned( character.track ) ) and ( character.TitleExists( 'Combative' ) ) and not ( Fighting ) and Assigned( FMaster ) then
      GuardMaster;


    if ( Delay > 0 ) and not ( Walking ) then
    begin
      Dec( Delay );
      Exit;

    end;


    if CirclePoint > 535 then
      CirclePoint := Random( 360 ) + 180;


    if Caster and RunAway and not ( Walking ) then
      MoveAway;

    if FCaster and not ( RunOrFight ) and Assigned( Character.Track ) and not ( walking ) then
    begin
      if Character.IsEnemy( Character.Track ) then
      begin
        if RangeTest( Character.Track, Character, iDistance ) then
        begin
          MoveAway;
          RunOrFight := True;
          Exit;
        end
      end;
    end;

    if FHealFirst and Assigned( FMaster ) and FCaster and not ( walking ) then
      if FMaster.Wounds > ( FMaster.HitPoints * 0.50 ) then
        HealMaster;

    if character.TitleExists( 'Heal' ) then
      if Character.Wounds > ( Character.HitPoints * 0.50 ) then
        HealSelf;



    if not ( Fighting ) and Assigned( FMaster ) then
      FollowLeader;

    if not ( FMage ) and Assigned( Character.Track ) and Fighting then
      AttackMelee;

    if FMage and FCaster and Assigned( Character.Track ) and Fighting and not ( Character.Casting ) and not ( walking ) then
    begin
      if Character.Track = FMaster then
        // Character.Track := nil
      else
        AttackCaster;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TGuardDog.PlaySounds;
begin

  case SFXDelayType of
    dtFixed :
      begin
        if iSFXDelayCount = 0 then
        begin
          Dec( iSFXDelayCount );
          if Assigned( character.Equipment[ slMisc1 ] ) then
            if character.Equipment[ slMisc1 ] is TWeapon then
            begin
              if bPlaySFXMetal and bPlaySFXAttack and bPlaySFXOther then
              begin

                case Random( 3 ) of
                  0 : PlaySound( TWeapon( character.Equipment[ slMisc1 ] ).StrikeLeatherSounds, character.X, character.Y );
                  1 : PlaySound( TWeapon( character.Equipment[ slMisc1 ] ).StrikeMetalSounds, character.X, character.Y );
                  2 : PlaySound( TWeapon( character.Equipment[ slMisc1 ] ).AttackSounds, character.X, character.Y );
                end;
              end
              else if bPlaySFXMetal and bPlaySFXAttack then
              begin
                case Random( 2 ) of
                  0 : PlaySound( TWeapon( character.Equipment[ slMisc1 ] ).StrikeMetalSounds, character.X, character.Y );
                  1 : PlaySound( TWeapon( character.Equipment[ slMisc1 ] ).AttackSounds, character.X, character.Y );
                end;
              end
              else if bPlaySFXMetal and bPlaySFXOther then
              begin
                case Random( 2 ) of
                  0 : PlaySound( TWeapon( character.Equipment[ slMisc1 ] ).StrikeLeatherSounds, character.X, character.Y );
                  1 : PlaySound( TWeapon( character.Equipment[ slMisc1 ] ).StrikeMetalSounds, character.X, character.Y );
                end;
              end
              else if bPlaySFXAttack and bPlaySFXOther then
              begin
                case Random( 2 ) of
                  0 : PlaySound( TWeapon( character.Equipment[ slMisc1 ] ).StrikeLeatherSounds, character.X, character.Y );
                  1 : PlaySound( TWeapon( character.Equipment[ slMisc1 ] ).AttackSounds, character.X, character.Y );
                end;
              end
              else if bPlaySFXMetal then
              begin
                PlaySound( TWeapon( character.Equipment[ slMisc1 ] ).StrikeMetalSounds, character.X, character.Y );
              end
              else if bPlaySFXAttack then
              begin
                PlaySound( TWeapon( character.Equipment[ slMisc1 ] ).AttackSounds, character.X, character.Y );
              end
              else if bPlaySFXOther then
              begin
                PlaySound( TWeapon( character.Equipment[ slMisc1 ] ).StrikeLeatherSounds, character.X, character.Y );
              end;
            end;
          if IdleSFXDelay > -1 then
            iSFXDelayCount := IdleSFXDelay;
        end
        else if iSFXDelayCount > -1 then
          Dec( iSFXDelayCount );
      end;
    dtRandom :
      begin
        if iSFXDelayCount = 0 then
        begin
          Dec( iSFXDelayCount );
          if Assigned( character.Equipment[ slMisc1 ] ) then
            if character.Equipment[ slMisc1 ] is TWeapon then
            begin
              if bPlaySFXMetal and bPlaySFXAttack and bPlaySFXOther then
              begin
                case Random( 3 ) of
                  0 : PlaySound( TWeapon( character.Equipment[ slMisc1 ] ).StrikeLeatherSounds, character.X, character.Y );
                  1 : PlaySound( TWeapon( character.Equipment[ slMisc1 ] ).StrikeMetalSounds, character.X, character.Y );
                  2 : PlaySound( TWeapon( character.Equipment[ slMisc1 ] ).AttackSounds, character.X, character.Y );
                end;
              end
              else if bPlaySFXMetal and bPlaySFXAttack then
              begin
                case Random( 2 ) of
                  0 : PlaySound( TWeapon( character.Equipment[ slMisc1 ] ).StrikeMetalSounds, character.X, character.Y );
                  1 : PlaySound( TWeapon( character.Equipment[ slMisc1 ] ).AttackSounds, character.X, character.Y );
                end;
              end
              else if bPlaySFXMetal and bPlaySFXOther then
              begin
                case Random( 2 ) of
                  0 : PlaySound( TWeapon( character.Equipment[ slMisc1 ] ).StrikeLeatherSounds, character.X, character.Y );
                  1 : PlaySound( TWeapon( character.Equipment[ slMisc1 ] ).StrikeMetalSounds, character.X, character.Y );
                end;
              end
              else if bPlaySFXAttack and bPlaySFXOther then
              begin
                case Random( 2 ) of
                  0 : PlaySound( TWeapon( character.Equipment[ slMisc1 ] ).StrikeLeatherSounds, character.X, character.Y );
                  1 : PlaySound( TWeapon( character.Equipment[ slMisc1 ] ).AttackSounds, character.X, character.Y );
                end;
              end
              else if bPlaySFXMetal then
              begin
                PlaySound( TWeapon( character.Equipment[ slMisc1 ] ).StrikeMetalSounds, character.X, character.Y );
              end
              else if bPlaySFXAttack then
              begin
                PlaySound( TWeapon( character.Equipment[ slMisc1 ] ).AttackSounds, character.X, character.Y );
              end
              else if bPlaySFXOther then
              begin
                PlaySound( TWeapon( character.Equipment[ slMisc1 ] ).StrikeLeatherSounds, character.X, character.Y );
              end;
            end;
          if IdleSFXDelay > 0 then
            iSFXDelayCount := Random( IdleSFXDelay ) + 20;
        end
        else if iSFXDelayCount > -1 then
          Dec( iSFXDelayCount );
      end;
  end;
end;


procedure TGuardDog.HealMaster;
const
  FailName : string = 'MiscAI.TGuardDog.HealFMaster';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if Character.CurrentSpell <> TSpell( Spells.Objects[ Spells.IndexOf( 'Heal' ) ] ) then
      Character.CurrentSpell := TSpell( Spells.Objects[ Spells.IndexOf( 'Heal' ) ] );

    if ( character.Mana - character.Drain ) >= Character.CurrentSpell.Drain( Character ) then
      character.Cast( FMaster );
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TGuardDog.HealSelf;
const
  FailName : string = 'MiscAI.TGuardDog.HealFMaster';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if Character.CurrentSpell <> TSpell( Spells.Objects[ Spells.IndexOf( 'Heal' ) ] ) then
      Character.CurrentSpell := TSpell( Spells.Objects[ Spells.IndexOf( 'Heal' ) ] );

    if ( character.Mana - character.Drain ) >= Character.CurrentSpell.Drain( Character ) then
      character.Cast( Character );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TGuardDog.Init;
var
  S : string;
  i, j : integer;
const
  FailName : string = 'MiscAI.TGuardDog.Init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  StandInterval := -1;
  try
    if character.GroupName <> '' then
      GroupList := GetGroup( Character, Character.GroupName );

    if Assigned( GroupList ) then
    begin
      for i := 0 to GroupList.count - 1 do
        if ( GroupList.objects[ i ] is TCharacter ) then
        begin
          FMaster := TCharacter( GroupList.Objects[ i ] );
          Break;
        end;
    end;
    Spells := character.SpellList;
    CirclePoint := Random( 360 ) + 180;
    NukeCounter := 0;
    CastTimes := Random( 3 ) + 1;
   // character.MakeAlly(FMaster.Alliance);

    FFighter := character.TitleExists( 'Fighter' );
    FScout := character.TitleExists( 'Scout' );
    FMage := character.TitleExists( 'Mage' );

    FCombative := character.TitleExists( 'Combative' );
    FCaster := character.TitleExists( 'Caster' );
    FHealfirst := character.TitleExists( 'HealFirst' );
    FStayClose := character.TitleExists( 'StayClose' );

    if character.TitleExists( 'Flame' ) then
      oSpellBook := oSpellBook + 'flame,';
    if character.TitleExists( 'Frost' ) then
      oSpellBook := oSpellBook + 'frost,';
    if character.TitleExists( 'Charge' ) then
      oSpellBook := oSpellBook + 'charge,';
    if character.TitleExists( 'Hold' ) then
      oSpellBook := oSpellBook + 'hold,';
    if character.TitleExists( 'Shrapnel' ) then
      oSpellBook := oSpellBook + 'shrapnel,';

    StrStripLast( oSpellBook );

    if character.TitleExists( 'HealFirst' ) then
    begin
      Character.AddTitle( 'Heal' );

    end;

    S := LowerCase( Character.Properties[ 'tmpEnemies' ] );
    try
      if S <> '' then
        Character.MakeEnemy( s );
    except

    end;

    S := LowerCase( Character.Properties[ 'NewStand' ] );
    try
      if s <> '' then
      begin
        if character.StandAction = '' then
          OldStand := 'stand'
        else
          OldStand := character.StandAction;
        NewStand := s;
        character.StandAction := s;
        StandInterval := Random( 400 ) + 500;
      end
    except
    end;

    S := LowerCase( Character.Properties[ 'disguise' ] );
    try
      if S = '' then
        strdisguise := ''
      else
      begin
        strdisguise := s;
        if Pos( 'party', Lowercase( Character.Enemies ) ) <> 0 then
        begin
          tmpEnemies := Character.Enemies;
          Character.Properties[ 'tmpEnemies' ] := Character.Enemies;
          if ( Player.IsWorn( strdisguise ) ) and ( NPCList.count < 2 ) then
          begin
            for j := 0 to NPCList.count - 1 do
            begin
              TCharacter( NPCList[ j ] ).MakeNeutral( Character.Alliance );
            end;
          end;
        end;
      end;
    except
      strdisguise := '';
    end;

    S := Character.Properties[ 'Distance' ];
    try
      if S = '' then
        iDistance := 175
      else
        iDistance := StrToInt( S );
    except
      iDistance := 175;
    end;


    if character.TitleExists( 'Combative' ) then
    begin
        //  character.RechargeRate := player.RechargeRate + i;
        //  character.HealingRate := player.HealingRate + i;

          //companion is a mage
      if FMage then
      begin
        S := LowerCase( Character.Properties[ 'BalanceWithFMaster' ] );
        try
          if ( S <> '' ) and ( s <> '0' ) then
          begin
            i := StrToInt( s );
            if i <> 0 then
            begin
              if player.TitleExists( 'Apprentice' ) then
              begin
                Character.Wounds := 0;
                Character.Drain := 0;
                character.Mysticism := ( ( player.Mysticism * 3 ) div 4 ) + i;
                character.perception := ( ( player.perception * 3 ) div 4 ) + i;
                character.Coordination := ( ( player.Coordination * 3 ) div 4 ) + i;
                character.HitPoints := ( ( player.Perception * 2 ) );
              end;
              if player.TitleExists( 'Hunter' ) then
              begin
                Character.Wounds := 0;
                Character.Drain := 0;
                character.Mysticism := ( ( player.Stealth * 3 ) div 4 ) + i;
                character.perception := ( ( player.Coordination * 3 ) div 4 ) + i;
                character.Coordination := ( ( player.Coordination * 3 ) div 4 ) + i;
                character.HitPoints := ( ( player.strength * 2 ) );
              end;
              if player.TitleExists( 'Squire' ) then
              begin
                Character.Wounds := 0;
                Character.Drain := 0;
                character.Mysticism := ( ( player.Combat * 3 ) div 4 ) + i;
                character.perception := ( ( player.Strength * 3 ) div 4 ) + i;
                character.Coordination := ( ( player.Strength * 3 ) div 4 ) + i;
                character.HitPoints := ( ( player.Coordination * 2 ) );
              end;
            end;
          end;
        except
        end;
      end;

          //GuardDog is a Scout
      if FScout then
      begin
        S := LowerCase( Character.Properties[ 'BalanceWithFMaster' ] );
        try
          if ( S <> '' ) and ( s <> '0' ) then
          begin
            i := StrToInt( s );
            if i <> 0 then
            begin
              if player.TitleExists( 'Apprentice' ) then
              begin
                character.Combat := ( ( ( player.mysticism * 3 ) div 4 ) + i );
                character.strength := ( ( player.perception * 6 ) div 10 ) + i;
                character.Coordination := ( ( player.perception * 2 ) div 3 ) + i;
                character.Stealth := player.Mysticism + i;
                character.HitPoints := ( ( player.Perception * 2 ) );
              end;
              if player.TitleExists( 'Hunter' ) then
              begin
                character.Combat := player.combat + i;
                character.strength := ( ( player.Strength * 6 ) div 10 ) + i;
                character.Coordination := ( ( player.Coordination * 2 ) div 3 ) + i;
                character.Stealth := player.Stealth + i;
                character.HitPoints := ( ( player.strength * 2 ) );
              end;
              if player.TitleExists( 'Squire' ) then
              begin
                character.Combat := ( ( ( player.combat * 3 ) div 4 ) + i );
                character.strength := ( ( player.Coordination * 6 ) div 10 ) + i;
                character.Coordination := ( ( player.strength * 2 ) div 3 ) + i;
                character.Stealth := player.Combat + i;
                character.HitPoints := ( ( player.Coordination * 2 ) );

              end;
            end;
          end;
        except
        end;
      end;
          //GuardDog is a fighter
      if character.TitleExists( 'Fighter' ) then
      begin
        S := LowerCase( Character.Properties[ 'BalanceWithFMaster' ] );
        try
          if ( S <> '' ) and ( s <> '0' ) then
          begin
            i := StrToInt( s );
            if i <> 0 then
            begin
              if player.TitleExists( 'Apprentice' ) then
              begin
                Character.Wounds := 0;
                Character.Drain := 0;
                character.Combat := ( ( ( player.Mysticism * 3 ) div 4 ) + i );
                character.strength := ( ( player.perception * 3 ) div 4 ) + i;
                character.HitPoints := ( ( player.Perception * 2 ) );
                character.Coordination := ( ( player.Coordination * 2 ) div 3 ) + i;
                              //   Character.AttackRecovery := Character.AttackRecovery + (player.attackRecovery div i);
              end;
              if player.TitleExists( 'Hunter' ) then
              begin
                Character.Wounds := 0;
                Character.Drain := 0;
                character.Combat := ( ( ( player.Stealth * 3 ) div 4 ) + i );
                character.strength := ( ( player.Coordination * 3 ) div 4 ) + i;
                character.HitPoints := ( ( player.Coordination * 2 ) );
                character.Coordination := ( ( player.Coordination * 3 ) div 4 ) + i;
                           //      Character.AttackRecovery := Character.AttackRecovery + (player.attackRecovery div i);
              end;

              if player.TitleExists( 'Squire' ) then
              begin
                Character.Wounds := 0;
                Character.Drain := 0;
                character.Combat := ( ( player.Combat * 3 ) div 4 ) + i;
                character.strength := ( ( player.Strength * 3 ) div 4 ) + i;
                character.Coordination := ( ( player.Coordination * 2 ) div 3 ) + i;
                          //       Character.AttackRecovery := Character.AttackRecovery + (player.attackRecovery div i);
                character.HitPoints := ( ( player.strength * 2 ) );
              end;
            end;
          end;
        except
        end;
      end;
    end;


    S := LowerCase( Character.Properties[ 'Transparent' ] );
    try
      if S <> '100' then
      begin
        Character.Alpha := StrToInt( s );
        Character.SpecialEffect := seAdd;
      end;
    except
    end;
    S := LowerCase( Character.Properties[ 'PlayIdleSFX' ] );
    try
      if S = '' then
        PlayIdleSFX := false
      else if S = 'true' then
        PlayIdleSFX := true
      else
        PlayIdleSFX := false;
    except
      PlayIdleSFX := false;
    end;

    S := LowerCase( Character.Properties[ 'IdleSFXDelay' ] );
    try
      if S = '' then
        IdleSFXDelay := 0
      else
      begin
        iSFXDelayCount := StrToInt( s );
        IdleSFXDelay := StrToInt( s );
      end;
    except
      IdleSFXDelay := 0;
    end;

    S := LowerCase( Character.Properties[ 'PlaySFXMetal' ] );
    try
      if S = '' then
        bPlaySFXMetal := false
      else if S = 'true' then
        bPlaySFXMetal := true
      else
        bPlaySFXMetal := False;
    except
      bPlaySFXMetal := false;
    end;
    S := LowerCase( Character.Properties[ 'PlaySFXAttack' ] );
    try
      if S = '' then
        bPlaySFXAttack := false
      else if S = 'true' then
        bPlaySFXAttack := true
      else
        bPlaySFXAttack := False;
    except
      bPlaySFXAttack := false;
    end;

    S := LowerCase( Character.Properties[ 'PlaySFXOther' ] );
    try
      if S = '' then
        bPlaySFXOther := false
      else if S = 'true' then
        bPlaySFXOther := true
      else
        bPlaySFXOther := False;
    except
      bPlaySFXOther := false;
    end;

    S := LowerCase( Character.Properties[ 'SFXDelayType' ] );
    try
      if S = '' then
        SFXDelayType := dtRandom
      else if S = 'fixed' then
        SFXDelayType := dtFixed
      else
        SFXDelayType := dtRandom;
    except
      SFXDelayType := dtRandom;
    end;

    S := Character.Properties[ 'iSpeed' ];
    try
      if ( S <> '' ) and ( s <> '0' ) then
        TCharacterResource( character.Resource ).Speed := StrToInt( S );
    except
    end;

    S := LowerCase( Character.Properties[ 'Moveable' ] );
    try
      if S = '' then
        bMove := True
      else if S = 'false' then
        bMove := False
      else
        bMove := True;
    except
      bMove := True;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TGuardDog.OnCollideFigure( Target : TAniFigure ) : boolean;
const
  FailName : string = 'TGuardDog.OnCollideFigure';
var
  r : Integer;
  T : single;
  X, Y : Integer;

begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  Result := False;
  try
    if assigned( target ) then
      if Target = FMaster then
      begin
        r := random( 150 );
        T := pi2 * random( 360 ) / 360;
        X := round( r * cos( T ) ) + FMaster.X + 10;
        Y := round( r * sin( T ) / 2 ) + FMaster.Y + 10;
        Character.WalkTo( X, Y, 4 );
      end;

  except
    on E : Exception do
      Log.log( 'Error TGuardDog CollideFigure: ' + E.Message );
  end;
end;


procedure TGuardDog.OnStop;
const
  FailName : string = 'MiscAI.TGuardDog.OnStop';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    character.stand;

    walking := False;
    if Assigned( Character.Track ) then
      Character.Face( Character.Track.X, Character.Track.Y )
    else
    begin
      character.Face( FMaster.x, FMaster.y );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TGuardDog.OnNoPath;
const
  FailName : string = 'MiscAI.TGuardDog.OnNoPath';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    walking := False;

    if Assigned( Character.Track ) then
      Character.Face( Character.Track.X, Character.Track.Y );
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TGuardDog.WasAttacked( Source : TAniFigure; Damage : Single );
var
  FriendList : TStringList;
  istealth : integer;
  J : integer;

const
  FailName : string = 'MiscAI.TGuardDog.WasAttacked';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
    Delay := 0;
    if Source is TCharacter then
      if ( Character.IsEnemy( TCharacter( Source ) ) ) and ( TCharacter( Source ) <> FMaster ) then
      begin
        //Character.Track := TCharacter(Source);
        if not ( Character.IsAlly( TCharacter( Source ) ) ) then
        begin
          if Current.Stealth < 1 then
            istealth := ( 1 div 100 )
          else
            iStealth := ( TCharacter( Source ).stealth div 100 );

          if not ( character.RangeTo( TCharacter( Source ).x, TCharacter( Source ).y ) < ( ( round( character.Vision * 1.5 ) - ( round( character.Vision * 1.5 ) * iStealth ) ) * GetFacing( character.x, character.y, TCharacter( Source ).x, TCharacter( Source ).y ) ) ) then
          begin
            Character.Face( Source.x, Source.y );
            exit;
          end;

          character.StandAction := '';
          StandInterval := -1;

          character.Track := TCharacter( Source );

          if ( TCharacter( Source ).PartyMember ) or ( TCharacter( Source ) = current ) then
          begin
            Player.MakeEnemy( Character.Alliance );
          end;
          TCharacter( Source ).MakeEnemy( Character.Alliance );


          FriendList := GetPerceptibleAllies( Character, 1 );
                   //ach a bad guy... tell all my friends
          if Assigned( FriendList ) then
          begin

            for j := 0 to FriendList.Count - 1 do
            begin
              if Assigned( TCharacter( FriendList.Objects[ j ] ).AI ) then
                if TCharacter( FriendList.Objects[ j ] ).AiMode <> AiCombat then
                begin
                  if LowerCase( TCharacter( FriendList.Objects[ j ] ).IdleAI ) = 'humanoididle' then
                    THumanoidIdle( TCharacter( FriendList.Objects[ j ] ).ai ).strdisguise := '';

                  if ( TCharacter( Source ).PartyMember ) or ( TCharacter( Source ) = current ) then
                  begin
                    TCharacter( FriendList.Objects[ j ] ).MakeEnemy( 'party' );
                    TCharacter( FriendList.Objects[ j ] ).track := TCharacter( NPCList[ Random( NPCList.count ) ] );
                  end
                  else
                  begin
                    TCharacter( FriendList.Objects[ j ] ).MakeEnemy( TCharacter( source ).alliance );
                    TCharacter( FriendList.Objects[ j ] ).track := TCharacter( source );
                  end;
                  TCharacter( FriendList.Objects[ j ] ).AiMode := AiCombat;
                end;
            end;
            friendList.Free;
          end;
        end;
        if Caster then
          RunAway := True
        else
          Fighting := True;

      end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;


procedure TGuardDog.GuardMaster;
var
  List : TStringList;
  FriendList : TStringList;
  t : Single;
  NewX, NewY : Integer;
  iStealth, j : integer;
const
  FailName : string = 'MiscAI.TGuardDog.GuardFMaster';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    //watch for good guys
    if ( FrameCount mod 40 ) = 0 then
    begin

      if ( LowerCase( character.Enemies ) = 'party' ) or ( lowerCase( tmpEnemies ) = 'party' ) then
      begin

        if Current.Stealth < 1 then
          istealth := ( 1 div 100 )
        else
          iStealth := ( current.stealth div 100 );
        if ( strdisguise <> '' ) and Character.IsEnemy( player ) then
          if not ( character.RangeTo( Current.x, Current.y ) < ( ( character.Vision - ( character.Vision * iStealth ) ) * GetFacing( character.x, character.y, current.x, current.y ) ) ) or
            not ( game.LineOfSight( character.x, character.y, current.x, current.y ) ) then
          begin
            if ( Player.IsWorn( strdisguise ) ) and ( NPCList.count < 2 ) then
            begin
              Character.MakeNeutral( 'party' );
            end;
          end;

        if ( strdisguise <> '' ) and Character.IsEnemy( player ) then
          strdisguise := '';
        if character.RangeTo( Current.x, Current.y ) < ( ( character.Vision - ( character.Vision * iStealth ) ) * GetFacing( character.x, character.y, current.x, current.y ) ) then
        begin
          if game.LineOfSight( character.x, character.y, current.x, current.y ) then
          begin
            if strdisguise <> '' then
            begin
              if Player.IsWorn( strdisguise ) and ( NPCList.count < 2 ) then
              begin
                Fighting := False;
                exit;
              end
              else if Pos( 'party', Lowercase( tmpEnemies ) ) <> 0 then
              begin
                Character.MakeEnemy( 'party' );
              end;
            end;


            character.StandAction := '';
            StandInterval := -1;

            if Player.IsWorn( strdisguise ) and ( NPCList.count >= 2 ) then
              character.track := TCharacter( NPCList[ NPCList.count - 1 ] )
            else
              character.track := TCharacter( NPCList[ Random( NPCList.count ) ] );

            Fighting := True;

            FriendList := GetPerceptibleAllies( Character, 1 );
                 //ach a bad guy... tell all my friends
            if Assigned( FriendList ) then
            begin

              for j := 0 to FriendList.Count - 1 do
              begin
                if Assigned( TCharacter( FriendList.Objects[ j ] ).AI ) then
                  if TCharacter( FriendList.Objects[ j ] ).AiMode <> AiCombat then
                  begin
                    THumanoidIdle( TCharacter( FriendList.Objects[ j ] ).ai ).strdisguise := '';
                    if LowerCase( TCharacter( FriendList.Objects[ j ] ).IdleAI ) <> 'guarddog' then
                    begin
                      TCharacter( FriendList.Objects[ j ] ).MakeEnemy( 'party' );
                      TCharacter( FriendList.Objects[ j ] ).AiMode := AiCombat;
                    end;
                    TCharacter( FriendList.Objects[ j ] ).track := TCharacter( NPCList[ Random( NPCList.count ) ] );
                  end;
              end;
              friendList.Free;
            end;
          end;
        end
      end
      else
      begin
        List := GetPerceptibleEnemies( Character, 2 );
        if Assigned( List ) then
        begin
          delay := 0;
          if List.Count = 1 then
            Character.Track := TCharacter( List.Objects[ 0 ] )
          else
            Character.Track := TCharacter( List.Objects[ random( List.Count ) ] );
          Fighting := True;
          List.Free;
          if FCaster then
          begin
            walking := True;
            t := 0.25;
            NewX := FMaster.x + round( t * ( Character.Track.x - FMaster.x ) );
            NewY := FMaster.y + round( t * ( Character.Track.y - FMaster.y ) );
            Character.runTo( NewX, NewY, 4 );
          end;
        end
        else
        begin
          Fighting := False;

        end;
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TGuardDog.FollowLeader;
var
  r : Integer;
  T : Single;
  X, Y : Integer;

const
  FailName : string = 'MiscAI.BTGuardDog.FollowFMaster';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    if ( character.Rangeto( FMaster.x, FMaster.y ) > 160 ) then
    begin //stay close to the leader
      if ( FrameCount mod 10 ) = 0 then
      begin
        Character.walkTo( FMaster.X + ( random( 220 ) - 110 ), FMaster.Y + ( random( 220 ) - 110 ), 48 );
        Stopped := false;
      end;
        {  if Pos('E',FFMaster.FacingString)<>0 then
             Character.WalkTo(FFMaster.X - (random(100)+20), FFMaster.Y+ random(100)+20, 4)
          else
          if Pos('W',FFMaster.FacingString)<>0 then
             Character.WalkTo(FFMaster.X + (random(100)+20), FFMaster.Y +  (random(100)+20), 4)
          else
          if Pos('SS',FFMaster.FacingString)<>0 then
             Character.WalkTo(FFMaster.X +  (random(100)+20), FFMaster.Y - (random(100)+20), 4)
          else
              Character.WalkTo(FFMaster.X +  (random(100)+20), FFMaster.Y + (random(100)+20), 4);}
    end
    else
    begin //walk around a bit but stay close
{      if (FrameCount mod 360) = 0 then
      begin  }
      if not Stopped then
      begin
        if character.Rangeto( FMaster.x, FMaster.y ) < 320 then
        begin
        end
        else
        begin
          r := random( 10 ) + 60;
          T := sqrt( sqr( FMaster.X - Character.X ) + sqr( 2 * ( FMaster.Y - Character.Y ) ) );
          if T > 0 then
          begin
            X := round( r * ( Character.X - FMaster.X ) / T ) + FMaster.X;
            Y := round( r * 2 * ( Character.Y - FMaster.Y ) / T ) + FMaster.Y;
            Character.WalkTo( X, Y, 16 );
          end;
        end;
        Stopped := true;
      end;
    end;
  except
    on E : Exception do
      Log.log( 'Error GuardDog FollowFMaster: ' + E.Message );
  end;
end;

procedure TGuardDog.AttackMelee;
const
  FailName : string = 'MiscAI.TGuardDog.AttackMelee';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if OldStand <> '' then
      character.StandAction := oldStand;
    StandInterval := 1;
    if Walking then
    begin

      try
        if ( FrameCount mod 10 ) = 0 then
        begin

          if ( character.X = character.StartX ) and ( character.Y = character.StartY ) then
          begin
            character.track := nil;
            fighting := false;

            exit;
          end;
           //  if StayClose and (character.RangeTo(FMaster.x, FMaster.y) < 300) then
           //    Character.Track := nil
           //  else
          if not ( Character.InRange( Character.Track ) ) then
          begin
            Character.RunTo( Character.Track.X, Character.Track.Y, 48 );
          end
          else
            walking := False;
        end;
      except
        on E : Exception do
          Log.log( 'Error GuardDog AttackMelee1: ' + E.Message );
      end;

    end;

    if ReadyToAttack then
    begin
      try
        character.Face( Character.Track.x, Character.Track.y );

//        if Not(IsAnybodyInTheWay(character,TCharacter(Character.track), 20)) then
        Character.Attack( Character.Track );

        ReadyToAttack := False;

        Walking := False;
      except
        on E : Exception do
          Log.log( 'Error GuardDog AttackMelee2: ' + E.Message );
      end;
    end
    else if not ( walking ) then
    begin

      try
        if Assigned( Character.Track ) then
        begin
          if TCharacter( Character.Track ).Dead then
          begin
            Character.Track := nil;
            Fighting := False;

          end
          else
          begin
            if Character.InRange( Character.Track ) then
            begin
              Delay := 0;
              readyToAttack := True;
            end
            else
            begin
              Delay := 0;
              Walking := True;
              Character.RunTo( Character.Track.X, Character.Track.Y, 48 );
            end;
          end;
        end;
      except
        on E : Exception do
          Log.log( 'Error GuardDog AttackMelee3: ' + E.Message );
      end;

    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TGuardDog.BattleTactic;
var
  r : Integer;
  T : single;
  X, Y : Integer;
const
  FailName : string = 'MiscAI.TGuardDog.BattleTactic';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if not Walking then
    begin
      Walking := True;
      NukeCounter := 0;
      Inc( CirclePoint, 45 );
      r := 300;
      T := pi2 * CirclePoint / 360;
      X := round( r * cos( T ) ) + Character.Track.X;
      Y := round( r * sin( T ) / 2 ) + Character.Track.Y;
      Character.WalkTo( X, Y, 16 );
    end;
  except
    on E : Exception do
      Log.log( 'Error GuardDog BattleTactic: ' + E.Message );
  end;
end;

procedure TGuardDog.AttackCaster;
var
  spell : string;
const
  FailName : string = 'MiscAI.TGuardDog.AttackCaster';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if TCharacter( Character.Track ).Dead then
    begin
     // Character.Track := nil;
     // fighting := False;
    end
    else
    begin
      if not ( RangeTest( Character.Track, Character, 50 ) ) then
      begin
        if ( NukeCounter < CastTimes ) and not ( IsAnybodyInTheWay( character, TCharacter( Character.track ), 10 ) ) then
        begin
          try
            if bMove then
              Inc( NukeCounter );
            //line of sight test here.
            Character.Face( Character.Track.x, Character.Track.y );
            Spell := strTokenAt( oSPellBook, ',', Random( StrTokenCount( oSpellBook, ',' ) ) );
            if Character.currentSpell <> TSpell( Spells.Objects[ Spells.IndexOf( spell ) ] ) then
              Character.CurrentSpell := TSpell( Spells.Objects[ Spells.IndexOf( spell ) ] );
            if ( character.Mana - character.Drain ) >= Character.CurrentSpell.Drain( Character ) then
              character.Cast( Character.Track )
            else
              Delay := Random( 360 ) + 120;
          except
            on E : Exception do
              Log.log( 'Error GuardDog AttackCaster1: ' + E.Message );
          end;

        end
        else
          BattleTactic;

      end
      else if random( 2 ) = 1 then
      begin
        try
          Character.Face( Character.Track.x, Character.Track.y );
          if Character.currentSpell <> TSpell( Spells.Objects[ Spells.IndexOf( 'Push' ) ] ) then
            Character.CurrentSpell := TSpell( Spells.Objects[ Spells.IndexOf( 'Push' ) ] );
          if ( character.Mana - character.Drain ) >= Character.CurrentSpell.Drain( Character ) then
            character.Cast( Character.Track )
          else if Character.Inrange( Character.Track ) then
            Character.Attack( Character.Track );
        except
          on E : Exception do
            Log.log( 'Error GuardDog AttackCaster2: ' + E.Message );
        end;
      end
      else
      begin
        try
          RunOrFight := False;
          if Character.Inrange( Character.Track ) then
            Character.Attack( Character.Track );
        except
          on E : Exception do
            Log.log( 'Error GuardDog AttackCaster3: ' + E.Message );
        end;
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TGuardDog.MoveAway;
const
  FailName : string = 'MiscAI.TGuardDog.MoveAway';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Walking := True;
    if Assigned( Character.Track ) then
      Character.Face( Character.Track.X, Character.Track.Y );

    if Pos( 'E', character.FacingString ) <> 0 then
      Character.RunTo( Character.X - 100, Character.Y + random( 200 ) - 100, 4 )
    else if Pos( 'W', character.FacingString ) <> 0 then
      Character.RunTo( Character.X + 100, Character.Y + random( 200 ) - 100, 4 )
    else if Pos( 'SS', character.FacingString ) <> 0 then
      Character.RunTo( Character.X + random( 200 ) - 100, Character.Y - 100, 4 )
    else
      Character.RunTo( Character.X + random( 200 ) - 100, Character.Y + 100, 4 );

    RunAway := False;
  except
    on E : Exception do
      Log.log( 'Error GuardDog MoveAway: ' + E.Message );
  end;
end;

destructor TGuardDog.Destroy;
const
  FailName : string = 'MiscAI.TGuardDog.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
    if Assigned( Spells ) then
      Spells.Free;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;
{*************************************************************************************}


{ TCommanderCombat }

procedure TCommanderCombat.Execute;
const
  FailName : string = 'MiscAI.TCommanderCombat.Execute';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
    if bRunaway then
      Run;
    if ( FrameCount mod 160 ) = 0 then
    begin
      walking := False;
    end;

    if not walking then
    begin
      if Delay > 0 then
      begin
        Dec( Delay );
        Exit;
      end;
    end;

    if Walking and Assigned( Character.Track ) then
    begin
      if ( FrameCount mod 10 ) = 0 then
        if not ( Character.InRange( Character.Track ) ) then
        begin
          walking := True;
          Character.WalkTo( Character.Track.X, Character.Track.Y, 16 );
        end;
    end;
    if ( not Walking ) then
      if ( Assigned( Character.Track ) ) and ( not WaitingToKill ) then
        Attack
      else
        FindTarget;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TCommanderCombat.Attack;
const
  FailName : string = 'MiscAI.TCommanderCombat.Attack';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if Character.Track is TCharacter and TCharacter( Character.Track ).Dead then
    begin
      Character.Track := nil;
      WaitingToKill := True;
    end
    else
    begin
      if Character.InRange( Character.Track ) then
      begin
        character.Face( Character.Track.x, Character.Track.y );
        Character.Attack( Character.Track );
        Walking := False;
      end
      else
      begin
        Character.WalkTo( Character.Track.X, Character.Track.Y, 16 );
        Walking := True;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TCommanderCombat.FindTarget;
var
  List : TStringList;
  iLoop : Integer;
  FList : TStringList;
  iFriend, iEnemy : Integer;

const
  FailName : string = 'MiscAI.TCommanderCombat.FindTarget';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if ( ( FrameCount mod 40 ) = 0 ) and WaitingToKill and OrdersGiven then
    begin
      //See if There is anyone who needs killing
      List := GetPerceptibleEnemies( Character, 1.5 );
      if Assigned( List ) then
      begin
        for iLoop := 0 to List.Count - 1 do
        begin
          if ( TCharacter( List.Objects[ iLoop ] ).Wounds / TCharacter( List.Objects[ iLoop ] ).Hitpoints ) * 100 >= TimeToAttack then
          begin
            Character.Track := TCharacter( List.Objects[ iLoop ] );
            Character.Say( 'Die Wimp!', clWhite );
            WaitingToKill := False;
            Break;
          end;
        end;
        List.Free;
      end
      else
        Character.AIMode := aiIdle;
    end;
    if not OrdersGiven then
    begin
      if ( FrameCount mod 40 ) = 0 then
      begin
        iEnemy := 0;
        List := GetPerceptibleEnemies( Character, 1.25 );
        if Assigned( List ) then
        begin
          FList := GetPerceptibleAllies( Character, 1.5 );
          if Assigned( FList ) then
          begin
            WaitingToKill := True;
            for iFriend := 0 to FList.Count - 1 do
            begin //Assign each ally a diff target
              if Assigned( TCharacter( FList.Objects[ iFriend ] ).AI ) then
                if TCharacter( FList.Objects[ iFriend ] ).AiMode = aiIdle then
                  TAI( TCharacter( FList.Objects[ iFriend ] ).AI ).Follow( Character, TCharacter( List.Objects[ iEnemy ] ) )
                else if TCharacter( FList.Objects[ iFriend ] ).AiMode = aiCombat then
                  if Assigned( TCharacter( FList.Objects[ iFriend ] ).AI ) then
                    TAI( TCharacter( FList.Objects[ iFriend ] ).AI ).CallToArms( Character, TCharacter( List.Objects[ iEnemy ] ) );
              if iEnemy = ( List.Count - 1 ) then
                iEnemy := 0
              else
                Inc( iEnemy );
            end;
            FList.Free;
          end
          else
          begin
            //ReadyToAttack:=True;
            WaitingToKill := False;
            Character.Track := TCharacter( List.Objects[ Random( List.Count ) ] );
          end;
          List.Free;
          OrdersGiven := True;
        end;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TCommanderCombat.Run;
var
  List : TStringList;
  iLoop : Integer;

const
  FailName : string = 'MiscAI.TCommanderCombat.Run';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if Assigned( Character.Track ) then
    begin
      Character.Face( Character.Track.X, Character.Track.Y );
    end;
    if walking then
      if Pos( 'E', character.FacingString ) <> 0 then
        Character.WalkTo( Character.X - 250, Character.Y + random( 500 ) - 250, 16 )
      else if Pos( 'W', character.FacingString ) <> 0 then
        Character.WalkTo( Character.X + 250, Character.Y + random( 500 ) - 250, 16 )
      else if Pos( 'SS', character.FacingString ) <> 0 then
        Character.WalkTo( Character.X + random( 500 ) - 250, Character.Y - 250, 16 )
      else
        Character.WalkTo( Character.X + random( 500 ) - 250, Character.Y + 250, 16 );
    //bRunaway := False;

    Walking := True;
    Delay := random( 240 );
    //while running away if I see a friend tell him to help me
    List := GetPerceptibleAllies( Character, 1.5 );
    if Assigned( List ) then
    begin
      for iLoop := 0 to List.Count - 1 do
      begin
        if Assigned( TCharacter( List.Objects[ iLoop ] ).AI ) then
        begin
          if TCharacter( List.Objects[ iLoop ] ).AiMode = aiIdle then
            TAI( TCharacter( List.Objects[ iLoop ] ).AI ).Follow( Character, Character.Track )
          else if TCharacter( List.Objects[ iLoop ] ).AiMode = aiCombat then
            TAI( TCharacter( List.Objects[ iLoop ] ).AI ).CallToArms( character, Character.Track );
        end;
      end;
      Character.Say( 'All Attack!!', clRed );
      List.Free;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TCommanderCombat.Init;
var
  S : string;
  List : TStringList;
  iLoop : Integer;
const
  FailName : string = 'MiscAI.TCommanderCombat.Init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    S := Character.Properties[ 'RunAwayTime' ];
    try
      if S = '' then
        RunAwayTime := 25
      else
        RunAwayTime := StrToInt( S );
    except
      RunAwayTime := 25;
    end;

    S := Character.Properties[ 'TimeToDie' ];
    try
      if S = '' then
        TimeToDie := 90
      else
        TimeToDie := StrToInt( S );
    except
      TimeToDie := 90;
    end;

    S := LowerCase( Character.Properties[ 'MainStat' ] );
    try
      if S = '' then
        MainStat := 'strength'
      else
        MainStat := S;
    except
      MainStat := 'strength';
    end;

    S := Character.Properties[ 'TimeToAttack' ];
    try
      if S = '' then
        TimeToAttack := 20
      else
        TimeToAttack := StrToInt( S );
    except
      TimeToAttack := 20;
    end;
    if Character.GroupName <> '' then
    begin
      List := GetGroup( Character, Character.GroupName );
      if Assigned( List ) then
      begin
        OrigPartyTot := 0;
        for iLoop := 0 to List.Count - 1 do
        begin
          if ( TCharacter( List.Objects[ iLoop ] ) is TCharacter ) and ( List.Objects[ iLoop ] <> Character ) then
            Inc( OrigPartyTot );
        end;
        List.Free;
      end;
    end;

    Delay := random( 60 );
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TCommanderCombat.Wait;
const
  FailName : string = 'MiscAI.TCommanderCombat.Wait';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Character.WalkTo( Character.X + random( 80 ) - 40, Character.Y + random( 40 ) - 20, 16 );
    CollideCount := 0;
    Walking := True;
    Delay := random( 10 ) + 10;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TCommanderCombat.OnCollideFigure( Target : TAniFigure ) : Boolean;
const
  FailName : string = 'MiscAI.TCommanderCombat.OnCollideFigure';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  Result := False;
  try
    if Target = Character.Track then
    begin
      Attack;
      Result := True;
    end
    else
    begin
      if Target is TCharacter then
      begin
        if Character.IsEnemy( TCharacter( Target ) ) then
        begin
          Character.Track := TCharacter( Target );
          Result := True;
        end
        else if Assigned( Character.Track ) and not ( Character.InRange( Character.Track ) ) then
        begin
          Inc( CollideCount );
          if ( CollideCount > 5 ) then
          begin
            Character.Stand;
            Wait;
            Result := True;
          end;
        end;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TCommanderCombat.WasAttacked( Source : TAniFigure; Damage : Single );
const
  FailName : string = 'MiscAI.TCommanderCombat.WasAttacked';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Delay := 0;
    if Source is TCharacter then
      character.Face( TCharacter( Source ).x, TCharacter( Source ).y );

    if Source is TCharacter then
    begin
      if ( ( Character.Wounds / Character.HitPoints ) * 100 >= RunAwayTime ) and ( ( Character.Wounds / Character.HitPoints ) > ( TCharacter( Source ).Wounds / TCharacter( Source ).Hitpoints ) ) then
      begin
        Character.Track := TCharacter( Source );
        bRunAway := True;
      end
      else
      begin
        if ( Source is TCharacter ) and ( Character.Track = nil ) then
          if Character.IsEnemy( TCharacter( Source ) ) then
            Character.Track := TCharacter( Source );
      end;
    end;
    inherited;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TCommanderCombat.OnNoPath;
const
  FailName : string = 'MiscAI.TCommanderCombat.OnNoPath';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Walking := False;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TCommanderCombat.OnStop;
const
  FailName : string = 'MiscAI.TCommanderCombat.OnStop';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Walking := False;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TCommanderCombat.WasKilled( Source : TAniFigure );
var
  List : TStringList;
  iLoop : Integer;
const
  FailName : string = 'MiscAI.TCommanderCombat.WasKilled';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if Source is TCharacter then
      character.Face( TCharacter( Source ).x, TCharacter( Source ).y );

    //Tell everyone I died so they can save themselves
    List := GetPerceptibleAllies( Character, 1.5 );
    if Assigned( List ) then
    begin
      for iLoop := 0 to List.Count - 1 do
      begin
        if Assigned( TCharacter( List.Objects[ iLoop ] ).AI ) then
          if TCharacter( List.Objects[ iLoop ] ).AiMode = aiCombat then
            TAI( TCharacter( List.Objects[ iLoop ] ).AI ).NotifyOfDeath( character );
      end;
      List.Free;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TCommanderCombat.NotifyOfDeath( Source : TAniFigure );
var
  EnemyList : TStringList;
  FriendList : TStringList;
  iLoop : Integer;
const
  FailName : string = 'MiscAI.TCommanderCombat.NotifyOfDeath';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    FriendList := GetPerceptibleAllies( Character, 1.5 );
    EnemyList := GetPerceptibleEnemies( Character, 1.25 );
    if Assigned( FriendList ) and Assigned( EnemyList ) then
    begin
      if ( FriendList.Count < EnemyList.Count ) and ( FriendList.Count > 2 ) then
      begin
        Character.Say( 'ReGroup!', clYellow );
        for iLoop := 0 to FriendList.Count - 1 do
        begin
          if Assigned( TCharacter( FriendList.Objects[ iLoop ] ).AI ) then
            if TCharacter( FriendList.Objects[ iLoop ] ).AiMode = aiCombat then
              TAI( TCharacter( FriendList.Objects[ iLoop ] ).AI ).Regroup( Character, Character.X + Random( 200 ), Character.Y + Random( 200 ) );
        end;
        OrdersGiven := False;
      end;
      FriendList.Free;
      EnemyList.Free;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;
{****************************************************************************************}
{ TOrcIdle }

procedure TOrcIdle.Execute;
var
  r : Integer;
  T : single;
  X, Y : Integer;
  List : TStringList;
  FriendList : TStringList;
  i, j : Integer;

const
  FailName : string = 'MiscAI.TOrcIdle.Execute';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
    if ( FrameCount mod 160 ) = 0 then
      walking := False;

    if bCombative then
      if ( FrameCount mod 40 ) = 0 then
      begin
        List := GetPerceptibleEnemies( Character, 1.5 );
        if Assigned( List ) then
        begin
          //find a live target
          for i := 0 to List.Count - 1 do
          begin
            if not ( TCharacter( List.Objects[ i ] ).Dead ) then
            begin
              FriendList := GetPerceptibleAllies( Character, 1 );
              //ach a bad guy... tell all my friends
              if Assigned( FriendList ) then
              begin
                for j := 0 to FriendList.Count - 1 do
                begin
                  if Assigned( TCharacter( List.Objects[ j ] ).AI ) then
                    if TCharacter( FriendList.Objects[ j ] ).AiMode <> AiCombat then
                    begin
                      TCharacter( FriendList.Objects[ j ] ).AiMode := AiCombat;
                      TAI( TCharacter( FriendList.Objects[ j ] ).AI ).CallToArms( Character, TCharacter( List.Objects[ i ] ) );
                    end;
                end;
                friendList.Free;
              end;
              //its time to get to work
              character.AIMode := aiCombat;
              Break;
            end;
          end;
          List.Free;
        end;
      end;

    if ( ( FrameCount mod 60 ) = 0 ) and Fighting then
    begin
      Character.Face( Character.Track.x, Character.Track.y );
      case random( 4 ) of
        0 : Character.Say( 'Get out of way!', clRed );
        1 : Character.Say( 'You in way! Move!', clRed );
      end;
      Character.DoAction( 'Attack1' );
      walking := False;
    end;

    if not ( Walking ) and ( Delay > 0 ) then
    begin
      Dec( Delay );
      Exit;
    end;

    if not Walking then
    begin
      case IdleDuty of
        IdMeander :
          begin //wander
            case random( 10 ) of //From Mike
              2 :
                begin //found a chest
                  List := GetPerceptibleContainers( Character, 1.5 );
                  if Assigned( List ) then
                  begin
                    Character.Track := nil;
                    if List.Count = 1 then
                      Character.Track := TCharacter( List.Objects[ 0 ] )
                    else
                      Character.Track := TCharacter( List.Objects[ Random( List.Count ) ] );

                    Character.Approach( Character.Track );
                    Walking := True;
                    if TContainer( Character.Track ).Closed then
                      Character.Say( 'Lets see whats in this', clWhite )
                    else
                      Character.Say( 'Now who left this open?', clWhite );

                    Delay := Random( 140 ) + 100;
                    List.Free;
                    Fighting := False;
                  end;
                end;
              5 :
                begin //loot corpse
                  List := GetPerceptibleDead( Character, 1.5 );
                  if Assigned( List ) then
                  begin
                    Character.Track := nil;
                    if List.Count = 1 then
                      Character.Track := TCharacter( List.Objects[ 0 ] )
                    else
                      Character.Track := TCharacter( List.Objects[ Random( List.Count ) ] );
                    Character.WalkTo( Character.Track.X, Character.Track.Y, 64 );
                    walking := True;
                    Character.Say( 'Wont be needin this no more', clWhite );
                    Delay := Random( 140 ) + 100;
                    List.Free;
                    Fighting := False;
                  end;
                end;

              8 :
                begin //walk to an allie... maybe start a fight
                  List := GetPerceptibleAllies( Character, 1.5 );
                  if Assigned( List ) then
                  begin
                    Character.Track := nil;
                    if List.Count = 1 then
                      Character.Track := TCharacter( List.Objects[ 0 ] )
                    else
                      Character.Track := TCharacter( List.Objects[ Random( List.Count ) ] );
                    Character.WalkTo( Character.Track.X, Character.Track.Y, 64 );
                    Walking := True;
                    Delay := Random( 160 );
                    List.Free;
                  end;
                end;

            else
              begin //wonder around
                //Pick A random direction
                Character.Track := nil;
                r := random( iLeash );
                T := pi2 * random( 360 ) / 360;
                X := round( r * cos( T ) ) + CenterX;
                Y := round( r * sin( T ) ) + CenterY;
                Character.walkTo( X, Y, 64 );
                character.say( '', clBlack ); //clear text
                delay := Random( 200 ) + 200;
                Walking := True;
                Fighting := False;
              end;
            end;
          end;

        idGuard :
          begin //guard duty
            //Pathcorner stuff here
            if atStart then
              character.WalkTo( Point2X, Point2Y, 64 )
            else
              character.WalkTo( Point1X, Point1Y, 64 );
            atStart := not ( atStart );
            Delay := random( 300 );
            walking := True;
            fighting := False;
          end;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TOrcIdle.Init;
var
  S : string;
const
  FailName : string = 'MiscAI.TOrcIdle.Init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    CenterX := Character.X;
    CenterY := Character.Y;
    S := LowerCase( Character.Properties[ 'IdleDuty' ] );
    try
      if S = '' then
        IdleDuty := idMeander
      else if S = 'idmeander' then
        IdleDuty := idMeander
      else if S = 'idstand' then
        IdleDuty := idStand
      else if S = 'idguard' then
        IdleDuty := idGuard
      else
        IdleDuty := idMeander;
    except
      IdleDuty := idMeander;
    end;

    S := LowerCase( Character.Properties[ 'Combative' ] );
    try
      if S = '' then
        bCombative := True
      else if S = 'false' then
        bCombative := False
      else
        bCombative := True;
    except
      bCombative := True;
    end;

    S := LowerCase( Character.Properties[ 'Talk' ] );
    try
      if S = '' then
        bTalk := True
      else if S = 'false' then
        bTalk := False
      else
        bTalk := True;
    except
      bTalk := True;
    end;

    S := Character.Properties[ 'LeashLength' ];
    try
      if S = '' then
        iLeash := 0
      else
        iLeash := StrToInt( S );
    except
      iLeash := 0;
    end;

    if IdleDuty = idGuard then
    begin
      //pathcorner stuff here
      Point1X := Character.x;
      Point1Y := character.y;
      Point2X := character.x + random( 500 ) - 250;
      Point2Y := character.y + random( 250 ) - 125;
      AtStart := True;

    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TOrcIdle.Follow( Source, Target : TAniFigure );
const
  FailName : string = 'MiscAI.TOrcIdle.Follow';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    character.WalkTo( Target.x, Target.y, 64 );
    Walking := True;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TOrcIdle.OnStop;
const
  FailName : string = 'MiscAI.TOrcIdle.OnStop';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Walking := False;

    if IdleDuty = idGuard then
      case Random( 18 ) of
        6 :
          character.say( 'Guard duty sucks', clWhite );
        12 :
          character.Say( 'who goes there?', clWhite );
        17 :
          character.Say( 'Did you hear something?', clWhite );
      end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TOrcIdle.WasAttacked( Source : TAniFigure; Damage : Single );
const
  FailName : string = 'MiscAI.TOrcIdle.WasAttacked';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if Source is TCharacter then
      character.Face( TCharacter( Source ).x, TCharacter( Source ).y );

    if Source is TCharacter then
    begin
      character.AIMode := aiCombat;
    end;
    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TOrcIdle.OnCollideFigure( Target : TAniFigure ) : Boolean;
const
  FailName : string = 'MiscAI.TOrcIdle.OnCollideFigure';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  Result := False;
  try
    if Target is TCharacter then
      if Character.IsEnemy( TCharacter( Target ) ) and not ( Tcharacter( Target ).dead ) then
      begin
        case random( 3 ) of
          0 :
            begin
              Character.Face( Target.x, Target.y );
              Character.Stand;
              Walking := False;
              Delay := Random( 300 );
              Result := True;

            end;
          2 :
            begin
              Walking := False;
              Fighting := True;
              Delay := Random( 160 ) + 100;
              Character.Track := Tcharacter( Target );
              Result := True;
            end;
        end;

      end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

(************************************************************************)
{ TScoutIdle }

procedure TScoutIdle.Execute;
const
  FailName : string = 'MiscAI.TScoutIdle.Execute';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
    if not ( Assigned( Character.Track ) ) and bCombative then
    begin
      FindTarget;
    end
    else
    begin //found enemy go back to others
      if Assigned( Character.Track ) then
      begin
        GotoFriends;
      end;
    end;

    if not ( Walking ) then
    begin
      if ( Delay > 0 ) then
      begin
        Dec( Delay );
        Exit;
      end;
      if not Assigned( Character.Track ) then
        case IdleDuty of
          idMeander :
            begin
              Meander;
            end;
          idGuard :
            WalkPath;
          idStand :
            Exit;
        end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TScoutIdle.GetPath( ToX, ToY : Integer );
var
  DiffX : Integer;
  DiffY : Integer;
const
  FailName : string = 'MiscAI.TScoutIdle.GetPath';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    DiffX := ToX - Character.X;
    DiffY := ToY - Character.Y;
    if Abs( DiffX ) > 1019 then
    begin
      if DiffX < 0 then
        ToX := Character.X - 1019
      else
        ToX := Character.X + 1019;
    end;
    if Abs( DiffY ) > 509 then
    begin
      if DiffY < 0 then
        ToY := Character.Y - 509
      else
        ToY := Character.Y + 509;

    end;
    Character.Walkto( ToX, ToY, 24 );
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TScoutIdle.FindTarget;
var
  List : TStringList;
const
  FailName : string = 'MiscAI.TScoutIdle.FindTarget';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    //watch for bad guys
    if ( FrameCount mod 40 ) = 0 then
    begin
      List := GetPerceptibleEnemies( Character, 1.5 );
      if Assigned( List ) then
      begin
        Character.Track := TCharacter( List.Objects[ 0 ] );
        delay := 0;
        List.Free;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TScoutIdle.GotoFriends;
var
  List : TStringList;
  iLoop : Integer;
const
  FailName : string = 'MiscAI.TScoutIdle.GotoFriends';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if ( FrameCount mod 40 ) = 0 then
    begin
      //see if friends are around
      List := GetPerceptibleAllies( Character, 1.6 );
      if Assigned( List ) then
      begin
        for iLoop := 0 to List.Count - 1 do
        begin
          if Assigned( TCharacter( List.Objects[ iLoop ] ).AI ) then
          begin
            if TCharacter( List.Objects[ iLoop ] ).AIMode = aiIdle then
              TAI( TCharacter( List.Objects[ iLoop ] ).AI ).Follow( Character, Character.Track );
            if TCharacter( List.Objects[ iLoop ] ).AIMode = aiCombat then
              TAI( TCharacter( List.Objects[ iLoop ] ).AI ).CallToArms( Character, Character.Track );
          end;
        end;
        List.Free;
        Character.AIMode := aiCombat;
      end
      else
      begin
        //find Pathpoint to walk to based on GUID that is assigned to SoftProp-'ReturnPath'
        begin
          if not Assigned( ReturnPath ) then
          begin //Already Have Destination
            if ReturnName <> '' then
              ReturnPath := GetGuid( ReturnName );
            if Assigned( ReturnPath ) then
            begin
              walking := True;
              GetPath( ReturnPath.X, ReturnPath.Y );
              Delay := StrToInt( ReturnPath.Properties[ 'Delay' ] );
            end;
          end
          else
          begin //walk to Destination
            walking := True;
            GetPath( ReturnPath.X, ReturnPath.Y );
            Delay := StrToInt( ReturnPath.Properties[ 'Delay' ] );
          end;
        end;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TScoutIdle.WalkPath;
const
  FailName : string = 'MiscAI.TScoutIdle.WalkPath';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    //Pathcorner stuff here
    if Assigned( MyPathCorners ) then
    begin
      if MyPathCorners.Count > 0 then
      begin
        if not Assigned( CurrentPath ) then
        begin //Already Have Destination
          CurrentPath := TGameObject( MyPathCorners.Objects[ random( MyPathCorners.Count ) ] );
          if Assigned( CurrentPath ) then
          begin
            walking := True;
            Character.WalkTo( CurrentPath.X, CurrentPath.Y, 16 );
            Delay := StrToInt( CurrentPath.Properties[ 'Delay' ] );
          end;
        end
        else
        begin //walk to Destination
          walking := True;
          Character.WalkTo( CurrentPath.X, CurrentPath.Y, 16 );
          Delay := StrToInt( CurrentPath.Properties[ 'Delay' ] );
        end;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TScoutIdle.Meander;
var
  r : Integer;
  T : single;
  X, Y : Integer;
const
  FailName : string = 'MiscAI.TScoutIdle.Meander';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    //Pick A random direction
    Character.Track := nil;
    r := random( 500 );
    T := pi2 * random( 360 ) / 360;
    X := round( r * cos( T ) ) + CenterX;
    Y := round( r * sin( T ) ) + CenterY;
    Character.walkTo( X, Y, 16 );
    character.say( '', clBlack ); //clear text
    delay := Random( 200 ) + 200;
    Walking := True;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TScoutIdle.Init;
var
  S : string;
  i, j : Integer;
const
  FailName : string = 'MiscAI.TScoutIdle.Init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Walking := False;
    CenterX := Character.X;
    CenterY := Character.Y;

    S := Character.Properties[ 'LeashLength' ];
    try
      if S = '' then
        iLeash := 0
      else
        iLeash := StrToInt( S );
    except
      iLeash := 0;
    end;

    S := LowerCase( Character.Properties[ 'IdleDuty' ] );
    try
      if S = '' then
        IdleDuty := idMeander
      else if S = 'meander' then
        IdleDuty := idMeander
      else if S = 'stand' then
        IdleDuty := idStand
      else if S = 'guard' then
        IdleDuty := idGuard
      else
        IdleDuty := idMeander;
    except
      IdleDuty := idMeander;

    end;
    S := LowerCase( Character.Properties[ 'Combative' ] );
    try
      if S = '' then
        bCombative := True
      else if S = 'false' then
        bCombative := False
      else
        bCombative := True;
    except
      bCombative := True;
    end;

    S := Character.Properties[ 'ReturnPath' ];
    try
      if S = '' then
        ReturnName := ''
      else
        ReturnName := S;
    except
      ReturnName := '';
    end;

    if Character.GroupName <> '' then
      MyGroup := GetGroup( Character, Character.GroupName );
    if Assigned( MyGroup ) then
    begin
      MyContainers := TStringList.Create;
      MyPathCorners := TStringList.Create;
      for i := 0 to MyGroup.Count - 1 do
      begin
        if ( MyGroup.Objects[ i ] is TContainer ) then
        begin
          j := MyContainers.Add( MyGroup.Strings[ i ] );
          MyContainers.Objects[ j ] := MyGroup.Objects[ i ];
        end
        else if ( MyGroup.Objects[ i ] is TPathCorner ) then
        begin
          j := MyPathCorners.Add( MyGroup.Strings[ i ] );
          MyPathCorners.Objects[ j ] := MyGroup.Objects[ i ];
        end;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TScoutIdle.OnNoPath;
const
  FailName : string = 'MiscAI.TScoutIdle.OnNoPath';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Walking := False;
    if Assigned( CurrentPath ) then
      CurrentPath := nil;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TScoutIdle.Wait;
const
  FailName : string = 'MiscAI.TScoutIdle.Wait';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Character.WalkTo( Character.X + random( 80 ) - 40, Character.Y + random( 40 ) - 20, 16 );
    CollideCount := 0;
    Walking := True;
    Delay := random( 10 ) + 10;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TScoutIdle.OnStop;
const
  FailName : string = 'MiscAI.TScoutIdle.OnStop';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    Walking := False;
    if Assigned( CurrentPath ) then
    begin
      if TPathCorner( CurrentPath ).NextDestination = '' then
        CurrentPath := nil
      else
      begin
        CurrentPath := GetGUID( TPathCorner( CurrentPath ).NextDestination );
        if not ( CurrentPath is TPathCorner ) then
          CurrentPath := nil;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TScoutIdle.WasAttacked( Source : TAniFigure; Damage : Single );
const
  FailName : string = 'MiscAI.TScoutIdle.WasAttacked';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
    if Source is TCharacter then
      character.Face( TCharacter( Source ).x, TCharacter( Source ).y );

    if Character.IsAlly( TCharacter( Source ) ) then
      Character.Track := TCharacter( Source );
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TScoutIdle.OnCollideFigure( Target : TAniFigure ) : Boolean;
const
  FailName : string = 'MiscAI.TScoutIdle.OnCollideFigure';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  Result := False;
  try
    if Target is TCharacter then
    begin
      if Character.IsEnemy( TCharacter( Target ) ) then
      begin
        Character.Track := TCharacter( Target );
        Result := True;
      end
      else if Assigned( Character.Track ) and not ( Character.InRange( Character.Track ) ) then
      begin
        Inc( CollideCount );
        if ( CollideCount > 5 ) then
        begin
          Character.Stand;
          Wait;
          Result := True;
        end;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{*************************************************************************************}

procedure TWatchDog.Execute;
const
  FailName : string = 'MiscAI.TWatchDog.Execute';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;

    if ( FCombative ) and not ( Fighting ) then
      GuardDog;

    if ( Delay > 0 ) and not ( Walking ) then
    begin
      Dec( Delay );
      Exit;
    end;

    if CirclePoint > 535 then
      CirclePoint := Random( 360 ) + 180;

    if Caster and RunAway and not ( Walking ) then
      MoveAway;

    if Caster and not ( RunOrFight ) and Assigned( Character.Track ) and not ( walking ) then
    begin
      if RangeTest( Character.Track, Character, iDistance ) then
      begin
        MoveAway;
        RunOrFight := True;
        Exit;
      end;
    end;

    if ( IdleDuty <> 'stand' ) and not ( walking ) and not ( Fighting ) and not ( Assigned( character.track ) ) then
      Meander;

    if not ( Caster ) and Assigned( Character.Track ) and Fighting then
      AttackMelee;

    if Caster and Assigned( Character.Track ) and Fighting and not ( Character.Casting ) and not ( walking ) then
    begin
      if Character.Track = Player then
        Character.Track := nil;
      AttackCaster;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TWatchDog.Init;
var
  S : string;
const
  FailName : string = 'MiscAI.TWatchDog.Init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    CirclePoint := Random( 360 ) + 180;
    NukeCounter := 0;
    CastTimes := Random( 3 ) + 1;
    CenterX := Character.X;
    CenterY := Character.Y;

    S := Character.Properties[ 'Distance' ];
    try
      if S = '' then
        iDistance := 175
      else
        iDistance := StrToInt( S );
    except
      iDistance := 175;
    end;

    S := LowerCase( Character.Properties[ 'IdleDuty' ] );
    try
      if S = '' then
        IdleDuty := 'stand'
      else if S = 'meander' then
        IdleDuty := s
      else if S = 'busy' then
        IdleDuty := 'meander'
      else
        IdleDuty := 'stand'
    except
      IdleDuty := 'stand';
    end;

    S := Character.Properties[ 'WatchedTitle' ];
    try
      if S = '' then
        strTitle := 'harasser;killdad;killmom'
      else
        strTitle := S;
    except
      strTitle := 'harasser;killdad;killmom';
    end;

    S := LowerCase( Character.Properties[ 'Combative' ] );
    try
      if S = '' then
        FCombative := True
      else if S = 'false' then
        FCombative := False
      else
        FCombative := True;
    except
      FCombative := True;
    end;

    S := LowerCase( Character.Properties[ 'Caster' ] );
    try
      if S = '' then
        FCaster := False
      else if S = 'true' then
        FCaster := True
      else
        FCaster := False;
    except
      FCaster := False;
    end;

    S := Character.Properties[ 'LeashLength' ];
    try

      if S = '' then
        iLeash := 50
      else
        iLeash := StrToInt( S );
    except
      iLeash := 50;
    end;

    S := LowerCase( Character.Properties[ 'Moveable' ] );
    try
      if S = '' then
        bMove := True
      else if S = 'false' then
        bMove := False
      else
        bMove := True;
    except
      bMove := True;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TWatchDog.OnStop;
const
  FailName : string = 'MiscAI.TWatchDog.OnStop';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    walking := False;
    if Assigned( Character.Track ) then
      Character.Face( Character.Track.X, Character.Track.Y );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TWatchDog.OnNoPath;
const
  FailName : string = 'MiscAI.TWatchDog.OnNoPath';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    walking := False;
    if Assigned( Character.Track ) then
      Character.Face( Character.Track.X, Character.Track.Y );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TWatchDog.WasAttacked( Source : TAniFigure; Damage : Single );
const
  FailName : string = 'MiscAI.TWatchDog.WasAttacked';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
    if Source is TCharacter then
      character.Face( TCharacter( Source ).x, TCharacter( Source ).y );

    if Source is TCharacter then
    begin
      Character.Track := TCharacter( Source );

      if Caster then
        RunAway := True
      else
        Fighting := True;
      delay := 0;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TWatchDog.OnCollideFigure( Target : TAniFigure ) : Boolean;
begin
  Result := True;
  //     if strTitle <> '' then
  //     if TCharacter(Target).TitleExists(strTitle) then
  //     begin
  //        Character.Track := TCharacter(target);
  //        Fighting := true;
  //        character.Say('I told you to leave her alone!', clred);
  //     end;
  //     if TCharacter(Target) = Character.Track then
  //        walking := false;

end;

procedure TWatchDog.GuardDog;
const
  FailName : string = 'MiscAI.TWatchDog.GuardDog';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    //watch for good guys

    if ( FrameCount mod 10 ) = 0 then
    begin
      if ( strTitle <> '' ) and not ( Player.Dead ) then
      begin
        if Player.TitleExists( StrTokenAt( strTitle, ';', 0 ) ) then
        begin
          Character.Track := Player;
          Fighting := True;
          delay := 0;
          character.Say( 'Don' + #39 + 't you guys ever learn!', clRed );
          Exit;
        end;

        if Player.TitleExists( StrTokenAt( strTitle, ';', 1 ) ) then
        begin
          Character.Track := Player;
          Fighting := True;
          delay := 0;
          character.Say( 'Hello my name is Inigo Montoya!' + #10 + 'You kill my Father! Prepare to die!', clRed );
          Exit;
        end;
        if Player.TitleExists( StrTokenAt( strTitle, ';', 2 ) ) then
        begin
          Character.Track := Player;
          Fighting := True;
          delay := 0;
          character.Say( 'Hello my name is Inigo Montoya!' + #10 + 'You kill my Mother! Prepare to die!', clRed );
          Exit;
        end;
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TWatchDog.AttackMelee;
const
  FailName : string = 'MiscAI.TWatchDog.AttackMelee';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if Walking then
    begin
      if ( FrameCount mod 10 ) = 0 then
        if not ( Character.InRange( Character.Track ) ) then
          Character.WalkTo( Character.Track.X, Character.Track.Y, 64 )
        else
          walking := False;
    end;

    if ReadyToAttack then
    begin
      character.Face( Character.Track.x, Character.Track.y );
      Character.Attack( Character.Track );
      ReadyToAttack := False;
      Walking := False;
    end
    else if not ( Walking ) then
    begin
      if Assigned( Character.Track ) then
      begin
        if TCharacter( Character.Track ).Dead then
        begin
          Character.Track := nil;
          Fighting := False;
        end
        else if Character.InRange( Character.Track ) then
        begin
          Delay := 0;
          readyToAttack := True;
        end
        else
        begin
          Walking := True;
          Delay := 0;
          Character.WalkTo( Character.Track.X, Character.Track.Y, 64 );
        end;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TWatchDog.BattleTatic;
var
  r : Integer;
  T : single;
  X, Y : Integer;
const
  FailName : string = 'MiscAI.TWatchDog.BattleTatic';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if not Walking then
    begin
      Walking := True;
      NukeCounter := 0;
      Inc( CirclePoint, 45 );
      r := 300;
      T := pi2 * CirclePoint / 360;
      X := round( r * cos( T ) ) + Character.Track.X;
      Y := round( r * sin( T ) / 2 ) + Character.Track.Y;
      Character.WalkTo( X, Y, 16 );
    end;
  except
    on E : Exception do
      Log.log( 'Error WatchDog BattleTatic: ' + E.Message );
  end;
end;

procedure TWatchDog.AttackCaster;
var
  spell : string;

const
  FailName : string = 'MiscAI.TWatchDog.AttackCaster';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if TCharacter( Character.Track ).Dead then
    begin
      Character.Track := nil;
      fighting := False;
    end
    else
    begin
      if not ( RangeTest( Character.Track, Character, 50 ) ) then
      begin
        if NukeCounter < CastTimes then
        begin
          if bMove then
            Inc( NukeCounter );
          //line of sight test here.
          Character.Face( Character.Track.x, Character.Track.y );
          Spell := strTokenAt( oSPellBook, ',', Random( StrTokenCount( oSpellBook, ',' ) ) );
          if Character.currentSpell <> TSpell( Spells.Objects[ Spells.IndexOf( spell ) ] ) then
            Character.CurrentSpell := TSpell( Spells.Objects[ Spells.IndexOf( spell ) ] );
          if ( character.Mana - character.Drain ) >= Character.CurrentSpell.Drain( Character ) then
            character.Cast( Character.Track )
          else
            Delay := Random( 360 ) + 120;
        end
        else
          BattleTatic;

      end
      else if random( 2 ) = 1 then
      begin
        Character.Face( Character.Track.x, Character.Track.y );
        if Character.currentSpell <> TSpell( Spells.Objects[ Spells.IndexOf( 'Push' ) ] ) then
          Character.CurrentSpell := TSpell( Spells.Objects[ Spells.IndexOf( 'Push' ) ] );
        if ( character.Mana - character.Drain ) >= Character.CurrentSpell.Drain( Character ) then
          character.Cast( Character.Track )
        else if Character.Inrange( Character.Track ) then
          Character.Attack( Character.Track );
      end
      else
      begin
        RunOrFight := False;
        if Character.Inrange( Character.Track ) then
          Character.Attack( Character.Track );
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TWatchDog.MoveAway;
const
  FailName : string = 'MiscAI.TWatchDog.MoveAway';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Walking := True;
    if Assigned( Character.Track ) then
      Character.Face( Character.Track.X, Character.Track.Y );

    if Pos( 'E', character.FacingString ) <> 0 then
      Character.WalkTo( Character.X - 100, Character.Y + random( 200 ) - 100, 64 )
    else if Pos( 'W', character.FacingString ) <> 0 then
      Character.WalkTo( Character.X + 100, Character.Y + random( 200 ) - 100, 64 )
    else if Pos( 'SS', character.FacingString ) <> 0 then
      Character.WalkTo( Character.X + random( 200 ) - 100, Character.Y - 100, 64 )
    else
      Character.WalkTo( Character.X + random( 200 ) - 100, Character.Y + 100, 64 );

    RunAway := False;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TWatchDog.Meander;
var
  r : Integer;
  T : single;
  X, Y : Integer;

const
  FailName : string = 'MiscAI.TWatchDog.Meander';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    //Pick A random direction
    Walking := True;
    if iLeash <> 0 then
    begin
      r := random( iLeash );
      T := pi2 * random( 360 ) / 360;
      X := round( r * cos( T ) ) + CenterX;
      Y := round( r * sin( T ) ) + CenterY;
    end
    else
    begin
      X := random( 200 ) + 50;
      Y := random( 200 ) + 50;
    end;
    Character.walkTo( X, Y, 16 );
    character.say( '...', clBlack ); //clear text
    delay := Random( 200 ) + 200;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{*************************************************************************************}

procedure TDrunk.Execute;
var
  Effect : TEffect;

const
  FailName : string = 'MiscAI.TDrunk.Execute';
begin

  if ( Delay > 0 ) then
  begin
    Dec( Delay );
    Exit;
  end;

  Effect := TEffect.Create;
  Effect.Resource := LoadArtResource( 'engine\WeaponProjectiles\vortex.gif' );
 // Effect.Resource:=LoadArtResource('engine\spells\summonreceive.gif');
  Effect.AnimationDuration := 8 * ( Effect.Resource.FrameMultiplier + 6 );
  Effect.Power := Character.Mysticism * 5;
  Effect.DoAction( 'Default', Character.FacingString );

  character.AddEffect( Effect );
  delay := 50;

  exit;
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;

    if ( ( FrameCount mod 500 ) = 0 ) then
      bHarassing := False;

    if not ( bShutUp ) and not ( bHarassing ) and not ( Fighting ) then
      DetectChar;

    if ( Delay > 0 ) and not ( Walking ) then
    begin
      Dec( Delay );
      Exit;
    end;

    if CirclePoint > 535 then
      CirclePoint := Random( 360 ) + 180;

    if RunAway and not ( Walking ) then
      MoveAway;

    if Caster and not ( RunOrFight ) and Assigned( Character.Track ) and not ( walking ) then
    begin
      if RangeTest( Character.Track, Character, iDistance ) then
      begin
        MoveAway;
        RunOrFight := True;
        Exit;
      end;
    end;

    //  if Not(walking) and Not(Fighting) and Not(Assigned(character.track))then
    //     Meander;

    if not ( Caster ) and Assigned( Character.Track ) and Fighting then
      AttackMelee;

    if Caster and Assigned( Character.Track ) and Fighting and not ( Character.Casting ) and not ( walking ) then
    begin
      if Character.Track = Player then
        Character.Track := nil;
      AttackCaster;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TDrunk.Init;
var
  S : string;
const
  FailName : string = 'MiscAI.TDrunk.Init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  delay := 50;
  exit;
  try
    CirclePoint := Random( 360 ) + 180;
    NukeCounter := 0;
    CastTimes := Random( 3 ) + 1;
    CenterX := Character.X;
    CenterY := Character.Y;

    S := Character.Properties[ 'Distance' ];
    try
      if S = '' then
        iDistance := 175
      else
        iDistance := StrToInt( S );
    except
      iDistance := 175;
    end;

    S := LowerCase( Character.Properties[ 'Combative' ] );
    try
      if S = '' then
        FCombative := True
      else if S = 'false' then
        FCombative := False
      else
        FCombative := True;
    except
      FCombative := True;
    end;

    S := LowerCase( Character.Properties[ 'Caster' ] );
    try
      if S = '' then
        FCaster := False
      else if S = 'true' then
        FCaster := True
      else
        FCaster := False;
    except
      FCaster := False;
    end;

    S := Character.Properties[ 'LeashLength' ];
    try

      if S = '' then
        iLeash := 50
      else
        iLeash := StrToInt( S );
    except
      iLeash := 50;
    end;

    S := LowerCase( Character.Properties[ 'Moveable' ] );
    try
      if S = '' then
        bMove := True
      else if S = 'false' then
        bMove := False
      else
        bMove := True;
    except
      bMove := True;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TDrunk.OnStop;
const
  FailName : string = 'MiscAI.TDrunk.OnStop';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    walking := False;
    if Assigned( Character.Track ) then
      Character.Face( Character.Track.X, Character.Track.Y );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TDrunk.OnNoPath;
const
  FailName : string = 'MiscAI.TDrunk.OnNoPath';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    walking := False;
    if Assigned( Character.Track ) then
      Character.Face( Character.Track.X, Character.Track.Y );
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;

end;

procedure TDrunk.WasAttacked( Source : TAniFigure; Damage : Single );
const
  FailName : string = 'MiscAI.TDrunk.WasAttacked';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;

    if Source is TCharacter then
    begin
      Character.Track := TCharacter( Source );
      if ( Character.Wounds / Character.HitPoints ) * 100 >= 25 then
      begin
        fighting := False;
        Character.say( 'Ok ok! Stop! I get the point!', clRed );
        bShutUp := True;
        bHarassing := True;
        player.AddTitle( 'drunk' );
        character.Properties[ 'IdleDuty' ] := 'stand';
        character.IdleAI := 'HumanoidIdle';
        Exit;
      end;

      if Caster then
        RunAway := True
      else
        Fighting := True;
      delay := 0;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TDrunk.OnCollideFigure( Target : TAniFigure ) : Boolean;
begin
  Result := True;
  //     if strTitle <> '' then
  //     if TCharacter(Target).TitleExists(strTitle) then
  //     begin
  //        Character.Track := TCharacter(target);
  //        Fighting := true;
  //        character.Say('I told you to leave her alone!', clred);
  //     end;
  //     if TCharacter(Target) = Character.Track then
  //        walking := false;

end;

procedure TDrunk.DetectChar;
var
  List : TStringList;

const
  FailName : string = 'MiscAI.TDrunk.DetectChar';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    //watch for good guys
    if ( FrameCount mod 10 ) = 0 then
    begin
      List := GetNearbyCharacter( Character, 150 );

      if Assigned( List ) then
      begin
        bHarassing := True;
        if List.Count = 1 then
          Character.Track := TCharacter( List.Objects[ 0 ] )
        else
          Character.Track := TCharacter( List.Objects[ random( List.Count ) ] );

        if Character.Track = Player then
        begin
          case Random( 2 ) of
            0 : character.Say( 'Hey buddy, I saw her first!', clWhite );
            1 : character.Say( 'She' + #39 + 'll come around. ' + #10 + 'No woman can resist this for long!', clWhite );
          end;
        end
        else
        begin
          case Random( 5 ) of
            0 : Character.say( 'C' + #39 + 'mere, sugar! Daddy' + #39 + 's got a present for you!', clWhite );
            1 : Character.say( 'How ' + #39 + 'bout you give me a little kiss!', clWhite );
            2 : Character.say( 'How ' + #39 + 'bout I  make your night honey!', clWhite );
            3 : Character.say( 'So when do you get off?', clWhite );
            4 : Character.say( 'Hey baby, I can make you see stars!', clWhite );
          end;
          case Random( 6 ) of
            0 : Tcharacter( Character.Track ).say( 'Leave me alone, you knave!', clYellow );
            1 : Tcharacter( Character.Track ).say( 'Ugh! You are the foulest creature here!', clYellow );
            2 : Tcharacter( Character.Track ).say( 'With you? Never!', clYellow );
            3 : Tcharacter( Character.Track ).say( 'I' + #39 + 'd rather kiss an orc!', clYellow );
            4 : Tcharacter( Character.Track ).say( 'Me and you? Never!', clYellow );
          end;

        end;
        List.Free;
      end
      else
        bHarassing := False;

    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TDrunk.AttackMelee;
const
  FailName : string = 'MiscAI.TDrunk.AttackMelee';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if Walking then
    begin
      if ( FrameCount mod 10 ) = 0 then
        if not ( Character.InRange( Character.Track ) ) then
          Character.WalkTo( Character.Track.X, Character.Track.Y, 64 )
        else
          walking := False;
    end;

    if ReadyToAttack then
    begin
      character.Face( Character.Track.x, Character.Track.y );
      Character.Attack( Character.Track );
      ReadyToAttack := False;
      Walking := False;
    end
    else if not ( Walking ) then
    begin
      if Assigned( Character.Track ) then
      begin
        if TCharacter( Character.Track ).Dead then
        begin
          Character.Track := nil;
          Fighting := False;
        end
        else if Character.InRange( Character.Track ) then
        begin
          Delay := 0;
          readyToAttack := True;
        end
        else
        begin
          Walking := True;
          Delay := 0;
          Character.WalkTo( Character.Track.X, Character.Track.Y, 64 );
        end;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TDrunk.BattleTatic;
var
  r : Integer;
  T : single;
  X, Y : Integer;
const
  FailName : string = 'MiscAI.TDrunk.BattleTatic';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if not Walking then
    begin
      Walking := True;
      NukeCounter := 0;
      Inc( CirclePoint, 45 );
      r := 300;
      T := pi2 * CirclePoint / 360;
      X := round( r * cos( T ) ) + Character.Track.X;
      Y := round( r * sin( T ) / 2 ) + Character.Track.Y;
      Character.WalkTo( X, Y, 16 );
    end;
  except
    on E : Exception do
      Log.log( 'Error WatchDog BattleTatic: ' + E.Message );
  end;
end;

procedure TDrunk.AttackCaster;
var
  spell : string;
const
  FailName : string = 'MiscAI.TDrunk.AttackCaster';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if TCharacter( Character.Track ).Dead then
    begin
      Character.Track := nil;
      fighting := False;
    end
    else
    begin
      if not ( RangeTest( Character.Track, Character, 50 ) ) then
      begin
        if NukeCounter < CastTimes then
        begin
          if bMove then
            Inc( NukeCounter );
          //line of sight test here.
          Character.Face( Character.Track.x, Character.Track.y );
          Spell := strTokenAt( oSPellBook, ',', Random( StrTokenCount( oSpellBook, ',' ) ) );
          if Character.currentSpell <> TSpell( Spells.Objects[ Spells.IndexOf( Spell ) ] ) then
            Character.CurrentSpell := TSpell( Spells.Objects[ Spells.IndexOf( Spell ) ] );
          if ( character.Mana - character.Drain ) >= Character.CurrentSpell.Drain( Character ) then
            character.Cast( Character.Track )
          else
            Delay := Random( 360 ) + 120;
        end
        else
          BattleTatic;

      end
      else if random( 2 ) = 1 then
      begin
        Character.Face( Character.Track.x, Character.Track.y );
        if Character.currentSpell <> TSpell( Spells.Objects[ Spells.IndexOf( 'Push' ) ] ) then
          Character.CurrentSpell := TSpell( Spells.Objects[ Spells.IndexOf( 'Push' ) ] );
        if ( character.Mana - character.Drain ) >= Character.CurrentSpell.Drain( Character ) then
          character.Cast( Character.Track )
        else if Character.Inrange( Character.Track ) then
          Character.Attack( Character.Track );
      end
      else
      begin
        RunOrFight := False;
        if Character.Inrange( Character.Track ) then
          Character.Attack( Character.Track );
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TDrunk.MoveAway;
const
  FailName : string = 'MiscAI.TDrunk.MoveAway';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Walking := True;

    if Assigned( Character.Track ) then
      Character.Face( Character.Track.X, Character.Track.Y );

    if Pos( 'E', character.FacingString ) <> 0 then
      Character.WalkTo( Character.X - 100, Character.Y + random( 200 ) - 100, 64 )
    else if Pos( 'W', character.FacingString ) <> 0 then
      Character.WalkTo( Character.X + 100, Character.Y + random( 200 ) - 100, 64 )
    else if Pos( 'SS', character.FacingString ) <> 0 then
      Character.WalkTo( Character.X + random( 200 ) - 100, Character.Y - 100, 64 )
    else
      Character.WalkTo( Character.X + random( 200 ) - 100, Character.Y + 100, 64 );

    // RunAway := False;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{procedure TDrunk.Meander;
var
  r: Integer;
  T: Single;
  X, Y: Integer;

const
  FailName: string = 'MiscAI.TDrunk.Meander';
begin
{$IFDEF DODEBUG}
{  if (CurrDbgLvl >= DbgLvlSevere) then
    Log.LogEntry(FailName);
{$ENDIF}
{  try
    //Pick A random direction
    Walking := True;
    if iLeash <> 0 then begin
      r := random(iLeash);
      T := pi2 * random(360) / 360;
      X := Round(r * cos(T)) + CenterX;
      Y := Round(r * sin(T)) + CenterY;
    end
    else begin
      X := random(200) + 50;
      Y := random(200) + 50;
    end;
    Character.walkTo(X, Y, 16);
    character.say('...', clBlack);      //clear text
    delay := Random(200) + 200;
  except
    on E: Exception do Log.log(FailName, E.Message, []);
  end;
end;            }

destructor TWatchDog.Destroy;
const
  FailName : string = 'MiscAI.TWatchDog.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
    if Assigned( Spells ) then
      Spells.Free;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TScoutIdle.Destroy;
const
  FailName : string = 'MiscAI.TScoutIdle.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
    if Assigned( MyGroup ) then
      MyGroup.Free;
    if Assigned( MyContainers ) then
      MyContainers.Free;
    if Assigned( MyPathCorners ) then
      MyPathCorners.Free;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;
{*************************************************************************************}

{ TWorms }

procedure TWorms.AttackMelee;
var
  x, y : Integer;
const
  FailName : string = 'MiscAI.TWorms.AttackMelee';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if ReadyToAttack then
    begin
      try
        character.Face( Character.Track.x, Character.Track.y );
        Character.Attack( Character.Track );
        ReadyToAttack := False;
        delay := ( AttackDelay - TCharacter( Character.Track ).Combat );
        if Delay < 0 then
          delay := 0;
      except
        on E : Exception do
          Log.log( 'Error Worms AttackMelee2: ' + E.Message );
      end;
    end
    else
    begin
      try
        if Assigned( Character.Track ) then
        begin
          if TCharacter( Character.Track ).Dead or not ( TCharacter( Character.Track ).enabled ) then
          begin
            Character.Track := nil;
            Fighting := False;
          end
          else if Character.InRange( Character.Track ) then
          begin
            if Revealed then
              readyToAttack := True
            else
            begin
              character.Face( Character.Track.x, Character.Track.y );
              Character.DoAction( 'Reveal' );
              Revealed := True;
              delay := 20;
            end;
          end
          else
          begin
            if Revealed then
            begin
              Character.DoAction( 'hide' );
              Revealed := False;
              delay := 20;
            end
            else
            begin
              character.Frame := 0;
              X := ( random( 64 ) - 32 );
              //      if (x < 16) and (x > -16) then x := 16;
              y := ( random( 64 ) - 32 );
              //      if (y < 16) and (y > -16) then y := 16;

              Character.SetPos( Character.Track.X + x, Character.Track.Y + y, 0 );
            end;
          end;
        end;
      except
        on E : Exception do
          Log.log( 'Error Worms AttackMelee3: ' + E.Message );
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{procedure TWorms.BattleTactic;
begin

end; }

destructor TWorms.Destroy;
begin

end;

procedure TWorms.Execute;
const
  FailName : string = 'MiscAI.TWorms.Execute';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if ( Delay > 0 ) then
    begin
      Dec( Delay );
      Exit;
    end;

    if not ( Fighting ) and not ( Assigned( Character.track ) ) then
      FindTarget;

    if Assigned( Character.Track ) and Fighting then
      AttackMelee;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TWorms.FindTarget;
var
  iStealth : integer;
const
  FailName : string = 'MiscAI.TWorms.FindTarget';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if ( FrameCount mod 40 ) = 0 then
    begin
      if Current.Stealth < 1 then
        istealth := ( 1 div 100 )
      else
        iStealth := ( current.stealth div 100 );
      if character.RangeTo( Current.x, Current.y ) < ( ( character.Vision - ( character.Vision * iStealth ) ) * GetFacing( character.x, character.y, current.x, current.y ) ) then
        if game.LineOfSight( character.x, character.y, current.x, current.y ) then
        begin
          character.track := Current;
          Fighting := true;
        end
        else
          fighting := false;

    {  List := GetPerceptibleEnemies(Character, 1);
      if Assigned(List) then
      begin
        if List.Count = 1 then
          Character.Track := TCharacter(List.Objects[0])
        else
          Character.Track := TCharacter(List.Objects[random(List.Count)]);
        Fighting := True;
        List.Free;
      end
      else
        Fighting := False;}
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TWorms.Init;
var
  s : string;
  i : Integer;
const
  FailName : string = 'MiscAI.TWorms.Init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Revealed := False;
    character.Frame := 0;
    S := Character.Properties[ 'AttackDelay' ];
    try
      if ( S = '' ) or ( s = '0' ) then
        AttackDelay := 32
      else
        AttackDelay := StrToInt( S );
    except
      AttackDelay := 32;
    end;

    S := LowerCase( Character.Properties[ 'BalanceWithPlayer' ] );
    try
      if ( S <> '' ) and ( s <> '0' ) then
      begin
        i := StrToInt( s );
        if i >= 0 then
        begin
          if player.TitleExists( 'Apprentice' ) then
            character.Combat := player.Mysticism + i;
          if player.TitleExists( 'Hunter' ) then
            character.Combat := player.Stealth + i;
          if player.TitleExists( 'Squire' ) then
            character.Combat := player.Combat + i;
        end;
      end;
    except
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TWorms.OnNoPath;
begin

end;

procedure TWorms.OnStop;
begin

end;

procedure TWorms.WasAttacked( Source : TAniFigure; Damage : Single );
begin

end;

{***************************************************************}
{ TPriortyCompanion }

procedure TPriortyCompanion.Execute;

const
  FailName : string = 'MiscAI.TPriortyCompanion.Execute';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
    //Temp find player stuff
  //  if not (Assigned(FPlayer)) then begin
  //    FPlayer := Player;
  //  end;

    if ( Delay > 0 ) and not ( Walking ) then
    begin
      Dec( Delay );
      Exit;
    end;

    case TPartyAI( character.AI ).Priority[ 1 ] of
      prAttack, prGuard :
        begin
          if ( FCombative ) and not ( Fighting ) and Assigned( Player ) then
            GuardPlayer;
          Exit;
        end;

      prCast :
        begin
          Character.CurrentSpell := TPartyAI( character.AI ).SpellToCast[ 1 ];
          case TPartyAI( character.AI ).Parameter[ 1 ] of
            paAnyAlly, paAnyPartyMember :
              begin
                FindFriendly;
                if Assigned( Character.CurrentSpell ) and Assigned( Friendly ) then
                  Character.Cast( Friendly );
              end;
            paWeakestEnemy, paMostMagicalEnemy, paAnyEnemy, paClosestEnemy, paStrongestEnemy :
              begin
                GuardPlayer;
                if Assigned( Character.CurrentSpell ) and Assigned( Character.Track ) then
                  Character.Cast( Character.Track );
              end;
            paAttacker :
              begin
                if Assigned( Character.CurrentSpell ) and Assigned( Character.Track ) then
                  Character.Cast( Character.Track );
              end;
            paSelf :
              begin
                if Assigned( Character.CurrentSpell ) then
                  Character.Cast( Character );
              end;
          end;


        end;
      prFollowClose :
        begin
          if not ( Fighting ) and Assigned( FLeader ) then
            iDistance := 120;
        end;
      prFollowFar :
        begin
          if not ( Fighting ) and Assigned( FLeader ) then
            iDistance := 240;
        end;
    end;

    //TPartyAI(character.AI).Priority[1] := prAttack;


    if ( FCombative ) and not ( Fighting ) and Assigned( Player ) then
      GuardPlayer;

    if not ( Fighting ) and Assigned( FLeader ) then
      FollowPlayer( IDistance );


    if CirclePoint > 535 then
      CirclePoint := Random( 360 ) + 180;

    if Caster and RunAway and not ( Walking ) then
      MoveAway;

    if Caster and not ( RunOrFight ) and Assigned( Character.Track ) and not ( walking ) then
    begin
      if Character.IsEnemy( Character.Track ) then
      begin
        if RangeTest( Character.Track, Character, iDistance ) then
        begin
          MoveAway;
          RunOrFight := True;
          Exit;
        end
      end;
    end;

    if FHealFirst and Assigned( Player ) and Caster and not ( walking ) then
      if Player.Wounds > ( Player.HitPoints * 0.75 ) then
        HealPlayer;

    if not ( Caster ) and Assigned( Character.Track ) and Fighting then
      AttackMelee;

    if Caster and Assigned( Character.Track ) and Fighting and not ( Character.Casting ) and not ( walking ) then
    begin
      if Character.Track = Player then
        Character.Track := nil;
      AttackCaster;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TPriortyCompanion.HealPlayer;
const
  FailName : string = 'MiscAI.TPriortyCompanion.HealPlayer';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if Character.CurrentSpell <> TSpell( Spells.Objects[ Spells.IndexOf( 'Healing' ) ] ) then
      Character.CurrentSpell := TSpell( Spells.Objects[ Spells.IndexOf( 'Healing' ) ] );

    if ( character.Mana - character.Drain ) >= Character.CurrentSpell.Drain( Character ) then
      character.Cast( Player );
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TPriortyCompanion.Init;
var
  S : string;
const
  FailName : string = 'MiscAI.TPriortyCompanion.Init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Spells := character.SpellList;
    CirclePoint := Random( 360 ) + 180;
    NukeCounter := 0;
    CastTimes := Random( 3 ) + 1;

    Character.AddTitle( 'Flame' );
    Character.AddTitle( 'Shock' );
    Character.AddTitle( 'Push' );
    Character.AddTitle( 'Shrapnel' );
    Character.AddTitle( 'Frost' );
    Character.AddTitle( 'Heal' );
    Character.AddTitle( 'Charge' );
    Character.AddTitle( 'Summon Rat' );
    Character.AddTitle( 'Summon Wolf' );
    Character.AddTitle( 'Protection from Fire' );
    Character.AddTitle( 'Protection from Cold' );
    Character.AddTitle( 'Protection from Lightning' );
    Character.AddTitle( 'Protection from Poison' );
    Character.AddTitle( 'Protection from Magic' );
    Character.AddTitle( 'Protection from All' );
    Character.AddTitle( 'Aura of Iron' );
    Character.AddTitle( 'Aura of Steel' );
    Character.AddTitle( 'Shadow' );
    Character.AddTitle( 'Hold' );

    S := Character.Properties[ 'Distance' ];
    try
      if S = '' then
        iDistance := 175
      else
        iDistance := StrToInt( S );
    except
      iDistance := 175;
    end;

    S := LowerCase( Character.Properties[ 'Combative' ] );
    try
      if S = '' then
        FCombative := True
      else if S = 'false' then
        FCombative := False
      else
        FCombative := True;
    except
      FCombative := False;
    end;

    S := LowerCase( Character.Properties[ 'Caster' ] );
    try
      if S = '' then
        FCaster := False
      else if S = 'true' then
        FCaster := True
      else
        FCaster := False;
    except
      FCaster := False;
    end;

    S := LowerCase( Character.Properties[ 'SpellBook' ] );
    try
      if S = '' then
        oSpellBook := 'frost,shock,shrapnel,flame'
      else
        oSpellBook := s
    except
      oSpellBook := 'frost,shock,shrapnel,flame';
    end;

    S := LowerCase( Character.Properties[ 'SpellBook' ] );
    try
      if S = '' then
        dSpellBook := 'push,heal,protection'
      else
        dSpellBook := s
    except
      dSpellBook := 'push,heal,protection';
    end;

    S := Character.Properties[ 'iSpeed' ];
    try
      if ( S <> '' ) and ( s <> '0' ) then
        TCharacterResource( character.Resource ).Speed := StrToInt( S );
    except
    end;

    S := LowerCase( Character.Properties[ 'Moveable' ] );
    try
      if S = '' then
        bMove := True
      else if S = 'false' then
        bMove := False
      else
        bMove := True;
    except
      bMove := True;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TPriortyCompanion.OnStop;
const
  FailName : string = 'MiscAI.TPriortyCompanion.OnStop';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    walking := False;
    if Assigned( Character.Track ) then
      Character.Face( Character.Track.X, Character.Track.Y );
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TPriortyCompanion.OnNoPath;
const
  FailName : string = 'MiscAI.TPriortyCompanion.OnNoPath';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    walking := False;
    if Assigned( Character.Track ) then
      Character.Face( Character.Track.X, Character.Track.Y );
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TPriortyCompanion.WasAttacked( Source : TAniFigure; Damage : Single );
const
  FailName : string = 'MiscAI.TPriortyCompanion.WasAttacked';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
    Delay := 0;
    if Source is TCharacter then
      if Character.IsEnemy( TCharacter( Source ) ) then
      begin
        Character.Track := TCharacter( Source );

        if Caster then
          RunAway := True
        else
          Fighting := True;

      end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TPriortyCompanion.OnCollideFigure( Target : TAniFigure ) : Boolean;
const
  FailName : string = 'MiscAI.TPriortyCompanion.OnCollideFigure';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  Result := False;
  try
    if Assigned( Character.Track ) then
      if TCharacter( Target ) = Character.Track then
        walking := False;

    // if TCharacter(Target).Alliance = Character.Alliance then
    //    Result := true;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TPriortyCompanion.GuardPlayer;
var
  List : TStringList;
  t : Single;
  NewX, NewY : Integer;

const
  FailName : string = 'MiscAI.TPriortyCompanion.GuardPlayer';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    //watch for good guys
    if ( FrameCount mod 40 ) = 0 then
    begin
      List := GetPerceptibleEnemies( Character, 3 );
      if Assigned( List ) then
      begin
        delay := 0;
        if List.Count = 1 then
          Character.Track := TCharacter( List.Objects[ 0 ] )
        else
          Character.Track := TCharacter( List.Objects[ random( List.Count ) ] );
        Fighting := True;
        List.Free;
        if FCaster then
        begin
          walking := True;
          t := 0.25;
          NewX := Player.x + round( t * ( Character.Track.x - Player.x ) );
          NewY := Player.y + round( t * ( Character.Track.y - Player.y ) );
          Character.WalkTo( NewX, NewY, 4 );
        end;
      end
      else
        Fighting := False;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TPriortyCompanion.FollowPlayer( Distance : integer );
var
  r : Integer;
  T : single;
  X, Y : Integer;

const
  FailName : string = 'MiscAI.TPriortyCompanion.FollowPlayer';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if character.Rangeto( FLeader.x, FLeader.y ) > Distance then
    begin //stay close to the player
      if ( FrameCount mod 40 ) = 0 then
        Character.RunTo( FLeader.X + ( random( 240 ) - 120 ), FLeader.Y + ( random( 240 ) - 120 ), 4 )

        {  if Pos('E',FPlayer.FacingString)<>0 then
             Character.WalkTo(FPlayer.X - (random(100)+20), FPlayer.Y+ random(100)+20, 4)
          else
          if Pos('W',FPlayer.FacingString)<>0 then
             Character.WalkTo(FPlayer.X + (random(100)+20), FPlayer.Y +  (random(100)+20), 4)
          else
          if Pos('SS',FPlayer.FacingString)<>0 then
             Character.WalkTo(FPlayer.X +  (random(100)+20), FPlayer.Y - (random(100)+20), 4)
          else
              Character.WalkTo(FPlayer.X +  (random(100)+20), FPlayer.Y + (random(100)+20), 4);}
    end
    else
    begin //walk around a but but stay close
      if ( FrameCount mod 360 ) = 0 then
      begin
        r := random( 150 );
        T := pi2 * random( 360 ) / 360;
        X := round( r * cos( T ) ) + FLeader.X + 10;
        Y := round( r * sin( T ) / 2 ) + FLeader.Y + 10;
        Character.WalkTo( X, Y, 4 );
      end;
    end;
  except
    on E : Exception do
      Log.log( 'Error Companion FollowPlayer: ' + E.Message );
  end;
end;

procedure TPriortyCompanion.AttackMelee;
const
  FailName : string = 'MiscAI.TPriortyCompanion.AttackMelee';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if Walking then
    begin
      try
        if ( FrameCount mod 10 ) = 0 then
          if not ( Character.InRange( Character.Track ) ) then
            Character.WalkTo( Character.Track.X, Character.Track.Y, 4 )
          else
            walking := False;
      except
        on E : Exception do
          Log.log( 'Error Companion AttackMelee1: ' + E.Message );
      end;

    end;

    if ReadyToAttack then
    begin
      try
        character.Face( Character.Track.x, Character.Track.y );
        Character.Attack( Character.Track );
        ReadyToAttack := False;
        Walking := False;
      except
        on E : Exception do
          Log.log( 'Error Companion AttackMelee2: ' + E.Message );
      end;
    end
    else if not ( Walking ) then
    begin
      try
        if Assigned( Character.Track ) then
        begin
          if TCharacter( Character.Track ).Dead then
          begin
            Character.Track := nil;
            Fighting := False;
          end
          else if Character.InRange( Character.Track ) then
          begin
            Delay := 0;
            readyToAttack := True;
          end
          else
          begin
            Delay := 0;
            Walking := True;
            Character.WalkTo( Character.Track.X, Character.Track.Y, 4 );
          end;
        end;
      except
        on E : Exception do
          Log.log( 'Error Companion AttackMelee3: ' + E.Message );
      end;

    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TPriortyCompanion.BattleTactic;
var
  r : Integer;
  T : single;
  X, Y : Integer;
const
  FailName : string = 'MiscAI.TPriortyCompanion.BattleTatic';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if not Walking then
    begin
      Walking := True;
      NukeCounter := 0;
      Inc( CirclePoint, 45 );
      r := 300;
      T := pi2 * CirclePoint / 360;
      X := round( r * cos( T ) ) + Character.Track.X;
      Y := round( r * sin( T ) / 2 ) + Character.Track.Y;
      Character.WalkTo( X, Y, 16 );
    end;
  except
    on E : Exception do
      Log.log( 'Error Companion BattleTatic: ' + E.Message );
  end;
end;

procedure TPriortyCompanion.AttackCaster;
var
  spell : string;
const
  FailName : string = 'MiscAI.TPriortyCompanion.AttackCaster';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if TCharacter( Character.Track ).Dead then
    begin
      Character.Track := nil;
      fighting := False;
    end
    else
    begin
      if not ( RangeTest( Character.Track, Character, 50 ) ) then
      begin
        if NukeCounter < CastTimes then
        begin
          try
            if bMove then
              Inc( NukeCounter );
            //line of sight test here.
            Character.Face( Character.Track.x, Character.Track.y );
            Spell := strTokenAt( oSPellBook, ',', Random( StrTokenCount( oSpellBook, ',' ) ) );
            if Character.currentSpell <> TSpell( Spells.Objects[ Spells.IndexOf( spell ) ] ) then
              Character.CurrentSpell := TSpell( Spells.Objects[ Spells.IndexOf( spell ) ] );
            if ( character.Mana - character.Drain ) >= Character.CurrentSpell.Drain( Character ) then
              character.Cast( Character.Track )
            else
              Delay := Random( 360 ) + 120;
          except
            on E : Exception do
              Log.log( 'Error Companion AttackCaster1: ' + E.Message );
          end;

        end
        else
          BattleTactic;

      end
      else if random( 2 ) = 1 then
      begin
        try
          Character.Face( Character.Track.x, Character.Track.y );
          if Character.currentSpell <> TSpell( Spells.Objects[ Spells.IndexOf( 'Push' ) ] ) then
            Character.CurrentSpell := TSpell( Spells.Objects[ Spells.IndexOf( 'Push' ) ] );
          if ( character.Mana - character.Drain ) >= Character.CurrentSpell.Drain( Character ) then
            character.Cast( Character.Track )
          else if Character.Inrange( Character.Track ) then
            Character.Attack( Character.Track );
        except
          on E : Exception do
            Log.log( 'Error Companion AttackCaster2: ' + E.Message );
        end;
      end
      else
      begin
        try
          RunOrFight := False;
          if Character.Inrange( Character.Track ) then
            Character.Attack( Character.Track );
        except
          on E : Exception do
            Log.log( 'Error Companion AttackCaster3: ' + E.Message );
        end;
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TPriortyCompanion.MoveAway;
const
  FailName : string = 'MiscAI.TPriortyCompanion.MoveAway';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Walking := True;
    if Assigned( Character.Track ) then
      Character.Face( Character.Track.X, Character.Track.Y );

    if Pos( 'E', character.FacingString ) <> 0 then
      Character.WalkTo( Character.X - 100, Character.Y + random( 200 ) - 100, 4 )
    else if Pos( 'W', character.FacingString ) <> 0 then
      Character.WalkTo( Character.X + 100, Character.Y + random( 200 ) - 100, 4 )
    else if Pos( 'SS', character.FacingString ) <> 0 then
      Character.WalkTo( Character.X + random( 200 ) - 100, Character.Y - 100, 4 )
    else
      Character.WalkTo( Character.X + random( 200 ) - 100, Character.Y + 100, 4 );

    RunAway := False;
  except
    on E : Exception do
      Log.log( 'Error Companion MoveAway: ' + E.Message );
  end;
end;

destructor TPriortyCompanion.Destroy;
const
  FailName : string = 'MiscAI.TPriortyCompanion.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
    if Assigned( Spells ) then
      Spells.Free;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TPriortyCompanion.FindFriendly;
var
  i : integer;
const
  FailName : string = 'TPriortyCompanion.FindFriendly';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    if not Assigned( FriendsList ) then
      FriendsList := GetPerceptibleAllies( Character, 2 );
    if assigned( FriendsList ) then
    begin //find someone to heal
      if FriendsList.Count = 1 then
      begin
        if FriendsList.objects[ 0 ] is TCharacter then
          if not ( TCharacter( FriendsList.objects[ 0 ] ).dead ) then
            if TCharacter( FriendsList.objects[ 0 ] ).Wounds > ( TCharacter( FriendsList.objects[ 0 ] ).HitPoints * 0.75 ) then
              Friendly := TCharacter( FriendsList.objects[ 0 ] );
      end
      else
      begin
        for i := 0 to FriendsList.Count - 1 do
        begin
          if FriendsList.objects[ i ] is TCharacter then
            if not ( TCharacter( FriendsList.objects[ i ] ).dead ) then
              if TCharacter( FriendsList.objects[ i ] ).Wounds > ( TCharacter( FriendsList.objects[ i ] ).HitPoints * 0.75 ) then
              begin
                Friendly := TCharacter( FriendsList.objects[ i ] );
                break;
              end;
        end;
      end;
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidCaster FindFriendly: ' + E.Message );

  end;
end;

{*************************************************************************************}

{ TRandomChestLoot }

procedure TRandomChestLoot.Init;
var
  s, sLoot : string;
  GUID : string;
  i, j : integer;

begin

  if LowerCase( Character.Properties[ 'NewLoot' ] ) <> 'true' then
  begin
    GUID := LowerCase( Character.Properties[ 'RandomLootGUID' ] );
    S := LowerCase( Character.Properties[ 'RandomLoot' ] );
    try
      if S <> '' then
      begin
        sLoot := s;
        j := StrTokenCount( sLoot, ',' );
        for i := 0 to StrToInt( Character.Properties[ 'RandomLootCount' ] ) - 1 do
        begin
          s := StrTokenAt( sLoot, ',', Random( j ) );
          if not ( character.HasItem( s ) ) then
          begin
            RunScript( Character, GUID + '.additem(' + s + ')' );
            RunScript( Character, 'additem(' + s + ')' );
          end;
        end;
        Character.Properties[ 'NewLoot' ] := 'true';
      end;
    except
    end;
  end;

end;
(**************************************************************************************)
{ TRitual }

destructor TRitual.Destroy;
const
  FailName : string = 'THumanoidRitual.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TRitual.Execute;
const
  FailName : string = 'THumanoidRitual.Execute';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    try
      if not ( Character.TitleExists( '02atstone' ) ) then
        exit;
      if Delay > 0 then
      begin
        dec( Delay );
        character.say( strSpell, clred );
        exit;
      end;

      if iSPellCount = 10 then
      begin
        character.doaction( 'death' );
        RunScript( Character, 'Targetable200008110049155730.doaction(death)' );
        RunScript( Character, 'Lich200008250075954760.doaction(death)' );
      end;

      case iSpellCount of
        0 : StrSpell := 'Shabakaar al necrohilisu ma khapek!';
        1 : StrSpell := 'By the energies of light,';
        2 : StrSpell := 'I lose the bindings that hold this soul.';
        3 : StrSpell := 'Erukaahil restes abaka ne shahala-zes!';
        4 : StrSpell := 'Too long it has resisted its fate,';
        5 : StrSpell := 'unholy energies giving life beyond life.';
        6 : StrSpell := 'Tevawadak inestium garanajiik useeliopen!';
        7 : StrSpell := 'With my life I displace the unlife within,';
        8 : StrSpell := 'my spirit shall shepherd this soul to its end.';
        9 : StrSpell := 'Aaarallesso mekanesta derimevelious Nu!';
      end;

      Inc( iSPellCount );
      delay := random( 64 );

    except
      on E : Exception do
        Log.log( 'Error HumanoidRitual Execute: ' + E.Message );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TRitual.Init;
const
  FailName : string = 'THumanoidRitual.Init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    iSpellCount := 0;
    Delay := 0;
    strSpell := '';
  except
    on E : Exception do
      Log.log( 'Error HumanoidRitual Init: ' + E.Message );
  end;

  Delay := random( 40 );
end;

{ TMeleeTraining }

procedure TMeleeTraining.Execute;
const
  FailName : string = 'TMeleeTraining.Execute';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    try
      if Delay > 0 then
      begin
        dec( Delay );
        exit;
      end;

      //follow traget a little better
      if Walking and Assigned( Character.track ) then
      begin
        if ( frameCount mod 10 ) = 0 then
          if not ( Character.InRange( Character.Track ) ) then
            Character.WalkTo( ( Character.Track ).X, ( Character.Track ).Y, 64 );
      end;

      if not ( Assigned( Character.Track ) ) then
        FindNextTarget
      else
        Attack;
    except
      on E : Exception do
        Log.log( 'Error Melee Execute: ' + E.Message );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TMeleeTraining.FindNextTarget;
var
  List : TStringList;
const
  FailName : string = 'TMeleeTraining.FindNextTarget';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    if ( FrameCount mod 40 ) = 0 then
    begin
      List := GetPerceptibleEnemies( Character, 2 );
      if assigned( List ) then
      begin
        if List.Count = 1 then
          Character.Track := TCharacter( List.objects[ 0 ] )
        else
          Character.Track := TCharacter( List.objects[ random( List.count ) ] );
        list.free;
      end;
    end;
  except
    on E : Exception do
      Log.log( 'Error Melee FindNextTarget: ' + E.Message );
  end;

end;


procedure TMeleeTraining.Attack;
const
  FailName : string = 'TMeleeTraining.Attack';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if not ( TCharacter( Character.Track ).dead ) then
    try
      if ReadyToAttack then
      begin
        character.Face( Character.Track.x, Character.Track.y );
        case random( 5 ) of
          0 :
            begin
              Character.DoAction( 'Attack1' );
              if random( 3 ) = 0 then
                PlaySound( TWeapon( character.Equipment[ slWeapon ] ).AttackSounds, character.X, character.Y );
              if random( 3 ) = 0 then
                PlaySound( TWeapon( character.Equipment[ slWeapon ] ).StrikeLeatherSounds, character.X, character.Y );
            end;
          1 :
            begin
              Character.DoAction( 'Attack2' );
              if random( 3 ) = 0 then
                PlaySound( TWeapon( character.Equipment[ slWeapon ] ).AttackSounds, character.X, character.Y );
              if random( 3 ) = 0 then
                PlaySound( TWeapon( character.Equipment[ slWeapon ] ).StrikeLeatherSounds, character.X, character.Y );
            end;
          2 :
            begin
              Character.DoAction( 'Attack3' );
              if random( 3 ) = 0 then
                PlaySound( TWeapon( character.Equipment[ slWeapon ] ).AttackSounds, character.X, character.Y );
              if random( 3 ) = 0 then
                PlaySound( TWeapon( character.Equipment[ slWeapon ] ).StrikeLeatherSounds, character.X, character.Y );
            end;
          3 :
            begin
              Character.DoAction( 'Attack4' );
              if random( 3 ) = 0 then
                PlaySound( TWeapon( character.Equipment[ slWeapon ] ).AttackSounds, character.X, character.Y );
              if random( 3 ) = 0 then
                PlaySound( TWeapon( character.Equipment[ slWeapon ] ).StrikeLeatherSounds, character.X, character.Y );
            end;
          4 : Character.DoAction( 'Pain' );
        end;



        Delay := random( 10 ) + 10;
        ReadyToAttack := false;
        Walking := false;
      end
      else if not ( Walking ) then
      begin
        if assigned( Character.Track ) then
        begin
          if Character.InRange( Character.Track ) then
          begin
            readyToAttack := true;
            delay := random( 10 ) + 10;
          end
          else
          begin
            Character.WalkTo( Character.Track.X, Character.Track.Y, 64 );
            Walking := true;
          end;
        end;
      end;
    except
      on E : Exception do
        Log.log( 'Error Melee Attack: ' + E.Message );

    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TMeleeTraining.OnCollideFigure( Target : TAniFigure ) : boolean;
const
  FailName : string = 'TMeleeTraining.OnCollideFigure';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  Result := False;

  try
    if Target = Character.Track then
    begin
      ReadyToAttack := True;
      Result := True;
    end
    else
    begin
      if Target is TCharacter then
      begin
        if Character.IsEnemy( TCharacter( Target ) ) then
        begin
          Character.Track := TCharacter( Target );
          ReadyToAttack := True;
          Result := True;
        end
        else if assigned( Character.Track ) and not ( Character.InRange( Character.Track ) ) then
        begin
          inc( CollideCount );
          if ( CollideCount > 3 ) then
          begin
            Character.Stand;
            Waiting := true;
            result := true;
            delay := random( 140 );
          end;
        end;
      end;
    end;
  except
    on E : Exception do
      Log.log( 'Error Melee CollideFigure: ' + E.Message );
  end;

end;

procedure TMeleeTraining.OnNoPath;
const
  FailName : string = 'TMeleeTraining.OnNoPath';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    Walking := False;
    if Assigned( Character.Track ) then
      Character.Face( Character.Track.X, Character.track.Y );
  except
    on E : Exception do
      Log.log( 'Error Melee NoPath: ' + E.Message );
  end;


end;

procedure TMeleeTraining.OnStop;
const
  FailName : string = 'TMeleeTraining.OnStop';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    try
      Walking := false;
      if Assigned( Character.Track ) then
        Character.Face( Character.Track.X, Character.track.Y );
    except
      on E : Exception do
        Log.log( 'Error Melee Stop: ' + E.Message );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TMeleeTraining.WasAttacked( Source : TAniFigure; Damage : single );
const
  FailName : string = 'TMeleeTraining.WasAttacked';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    try
      if Source is TCharacter then
        character.Face( TCharacter( Source ).x, TCharacter( Source ).y );

    except
      on E : Exception do
        Log.log( 'Error Melee WasAttacked: ' + E.Message );
    end;

    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

(*************************************************************************)

destructor TMeleeTraining.Destroy;
const
  FailName : string = 'TMeleeTraining.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );

{$ENDIF}
  try

    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TMeleePratice }

destructor TMeleePratice.Destroy;
const
  FailName : string = 'TMeleePratice.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    if Assigned( Partylist ) then
      Partylist.free;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TMeleePratice.Execute;
var
  iLoop : integer;
  strAction : string;
  facing : string;
const
  FailName : string = 'TMeleePratice.Execute';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    try
      if Delay > 0 then
      begin
        dec( Delay );
        exit;
      end;

      case random( 3 ) of
        0 : Facing := 'SE';
        1 : Facing := 'EE';
        2 : Facing := 'SW';
      end;

      case random( 4 ) of
        0 : strAction := 'Attack1';
        1 : strAction := 'Attack2';
        2 : strAction := 'Attack3';
        3 : strAction := 'Attack4';
      end;

      Character.DoAction( strAction );
      for iLoop := 0 to PartyList.count - 1 do
      begin
        if Facing = 'SE' then
          if not ( TCharacter( PartyList.Objects[ iLoop ] ).dead ) then
            TCharacter( PartyList.Objects[ iLoop ] ).Facing := FSE;
        if Facing = 'EE' then
          if not ( TCharacter( PartyList.Objects[ iLoop ] ).dead ) then
            TCharacter( PartyList.Objects[ iLoop ] ).Facing := FEE;
        if Facing = 'SW' then
          if not ( TCharacter( PartyList.Objects[ iLoop ] ).dead ) then
            TCharacter( PartyList.Objects[ iLoop ] ).Facing := FSW;

        if not ( TCharacter( PartyList.Objects[ iLoop ] ).dead ) then
          TCharacter( PartyList.Objects[ iLoop ] ).DoAction( strAction );

        PlaySound( TWeapon( Tcharacter( PartyList.Objects[ iLoop ] ).Equipment[ slWeapon ] ).AttackSounds, Tcharacter( PartyList.Objects[ iLoop ] ).X, Tcharacter( PartyList.Objects[ iLoop ] ).Y );

      end;
      delay := random( 80 );

    except
      on E : Exception do
        Log.log( 'Error MeleePractice Execute: ' + E.Message );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TMeleePratice.Init;
const
  FailName : string = 'TMeleePratice.Init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    PartyTotal := 1;
    Partylist := GetGroup( Character, Character.GroupName );
    if Assigned( PartyList ) then
    begin
      PartyTotal := Partylist.Count;
    end;

  except
    on E : Exception do
      Log.log( 'Error Melee Init: ' + E.Message );
  end;

  Delay := random( 40 );
end;

(**************************************************************************************)

procedure TMeleePratice.WasAttacked( Source : TAniFigure; Damage : Single );
var
  FriendList : TStringList;
  istealth : integer;
  J : integer;
  x : longint;
  y : longint;
begin
//  Delay := 0;
  try

    if Source is TCharacter then
      character.Face( TCharacter( Source ).x, TCharacter( Source ).y );

    if Source is TCharacter then
    begin
      if not ( Character.IsAlly( TCharacter( Source ) ) ) then
      begin
        if Current.Stealth < 1 then
          istealth := ( 1 div 100 )
        else
          iStealth := ( TCharacter( Source ).stealth div 100 );

        if not ( character.RangeTo( TCharacter( Source ).x, TCharacter( Source ).y ) < ( ( round( character.Vision * 1.5 ) - ( round( character.Vision * 1.5 ) * iStealth ) ) * GetFacing( character.x, character.y, TCharacter( Source ).x, TCharacter( Source ).y ) ) ) then
        begin
          Character.Face( Source.x, Source.y );
          exit;
        end;

        character.Track := TCharacter( Source );

        if ( TCharacter( Source ).PartyMember ) or ( TCharacter( Source ) = current ) then
        begin
          Player.MakeEnemy( Character.Alliance );
        end;
        TCharacter( Source ).MakeEnemy( Character.Alliance );

        character.AIMode := aiCombat;


        FriendList := GetPerceptibleAllies( Character, 1 );
             //ach a bad guy... tell all my friends
        if Assigned( FriendList ) then
        begin

          for j := 0 to FriendList.Count - 1 do
          begin
            if Assigned( TCharacter( FriendList.Objects[ j ] ).AI ) then
              if TCharacter( FriendList.Objects[ j ] ).AiMode <> AiCombat then
              begin
                if LowerCase( TCharacter( FriendList.Objects[ j ] ).IdleAI ) = 'humanoididle' then
                  THumanoidIdle( TCharacter( FriendList.Objects[ j ] ).ai ).strdisguise := '';

                if ( TCharacter( Source ).PartyMember ) or ( TCharacter( Source ) = current ) then
                begin
                  TCharacter( FriendList.Objects[ j ] ).MakeEnemy( 'party' );
                  TCharacter( FriendList.Objects[ j ] ).track := TCharacter( NPCList[ Random( NPCList.count ) ] );
                end
                else
                begin
                  TCharacter( FriendList.Objects[ j ] ).MakeEnemy( TCharacter( source ).alliance );
                  TCharacter( FriendList.Objects[ j ] ).track := TCharacter( source );
                end;
                TCharacter( FriendList.Objects[ j ] ).AiMode := AiCombat;
              end;
          end;
          friendList.Free;
        end;

      end;
    end
    else
    begin
      X := random( 100 ) + 50;
      Y := random( 100 ) + 50;
      Character.walkTo( X, Y, 16 );
    end
  except
    on E : Exception do
      Log.log( 'Error MeleePratice WasAttacked: ' + E.Message );
  end;

  inherited;

end;

procedure TMeleePratice.WasKilled( Source : TAniFigure );
var
  FriendList : TStringList;
  iStealth : integer;
  J : integer;

begin
  inherited;
  try
      //     Delay := 0;
      //doenst really work
    if Source is TCharacter then
      character.Face( TCharacter( Source ).x, TCharacter( Source ).y );
    if not ( Character.IsAlly( TCharacter( Source ) ) ) then
    begin
      if Current.Stealth < 1 then
        istealth := ( 1 div 100 )
      else
        iStealth := ( TCharacter( Source ).stealth div 100 );

      if not ( character.RangeTo( TCharacter( Source ).x, TCharacter( Source ).y ) < ( ( round( character.Vision * 1.5 ) - ( round( character.Vision * 1.5 ) * iStealth ) ) * GetFacing( character.x, character.y, TCharacter( Source ).x, TCharacter( Source ).y ) ) ) then
      begin
        exit;
      end;


      FriendList := GetPerceptibleAllies( Character, 1 );
       //ach a bad guy... tell all my friends
      if Assigned( FriendList ) then
      begin

        for j := 0 to FriendList.Count - 1 do
        begin
          if Assigned( TCharacter( FriendList.Objects[ j ] ).AI ) then
            if TCharacter( FriendList.Objects[ j ] ).AiMode <> AiCombat then
            begin
              if LowerCase( TCharacter( FriendList.Objects[ j ] ).IdleAI ) = 'humanoididle' then
                THumanoidIdle( TCharacter( FriendList.Objects[ j ] ).ai ).strdisguise := '';

              if ( TCharacter( Source ).PartyMember ) or ( TCharacter( Source ) = current ) then
              begin
                TCharacter( FriendList.Objects[ j ] ).MakeEnemy( 'party' );
                TCharacter( FriendList.Objects[ j ] ).track := TCharacter( NPCList[ Random( NPCList.count ) ] );
              end
              else
              begin
                TCharacter( FriendList.Objects[ j ] ).MakeEnemy( TCharacter( source ).alliance );
                TCharacter( FriendList.Objects[ j ] ).track := TCharacter( source );
              end;
              TCharacter( FriendList.Objects[ j ] ).AiMode := AiCombat;
            end;
        end;
        friendList.Free;
      end;
    end;
  except
    on E : Exception do
      Log.log( 'Error MeleePratice WasKilled: ' + E.Message );
  end;

end;

{ TMeleeSparing }

procedure TMeleeSparing.Execute;
const
  FailName : string = 'TMeleeSparing.Execute';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    try
      if Delay > 0 then
      begin
        dec( Delay );
        exit;
      end;

      if Assigned( Character.Track ) then
        Attack;

    except
      on E : Exception do
        Log.log( 'Error MeleeSparing Execute: ' + E.Message );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;



procedure TMeleeSparing.Attack;
const
  FailName : string = 'TMeleeSparing.Attack';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    if ReadyToAttack then
    begin
      character.Face( Character.Track.x, Character.Track.y );
      Character.Attack( Character.Track );
      ReadyToAttack := false;
      Walking := false;
    end
    else if not ( Walking ) then
    begin
      if assigned( Character.Track ) then
      begin
        if TCharacter( Character.Track ).Dead then
          Character.Track := nil
        else
        begin
          if Character.InRange( Character.Track ) then
          begin
            readyToAttack := true;
            delay := ( 25 - TCharacter( Character.Track ).Combat );
            if Delay < 0 then
              delay := 0;

          end
          else
          begin
            Character.MakeNeutral( 'party' );
            player.CombatMode := false;
            character.AIMode := aiNone;
            exit;
          end;
        end;
      end;
    end;
  except
    on E : Exception do
      Log.log( 'Error Melee Attack: ' + E.Message );

  end;

end;




procedure TMeleeSparing.Init;
var
  S : string;
const
  FailName : string = 'TMeleeSparing.Init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    Character.track := player;
    character.combat := player.combat;
    character.Strength := player.strength;
    character.Coordination := player.Coordination;

    S := Character.Properties[ 'TimeToRun' ];
    try
      if S = '' then
        iTimeToRun := 75
      else
        iTimeToRun := StrToInt( S );
    except
      iTimeToRun := 75;
    end;



  except
    on E : Exception do
      Log.log( 'Error Melee Init: ' + E.Message );
  end;

  Delay := random( 40 );
end;


function TMeleeSparing.OnCollideFigure( Target : TAniFigure ) : boolean;
const
  FailName : string = 'TMeleeSparing.OnCollideFigure';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  Result := False;

  try
    if Target = Character.Track then
    begin
      ReadyToAttack := True;
      Result := True;
    end
    else
    begin
      if Target is TCharacter then
      begin
        if Character.IsEnemy( TCharacter( Target ) ) then
        begin
          Character.Track := TCharacter( Target );
          ReadyToAttack := True;
          Result := True;
        end
      end;
    end;
  except
    on E : Exception do
      Log.log( 'Error Melee CollideFigure: ' + E.Message );
  end;

end;

procedure TMeleeSparing.OnNoPath;
const
  FailName : string = 'TMeleeSparing.OnNoPath';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    Walking := False;
    if Assigned( Character.Track ) then
      Character.Face( Character.Track.X, Character.track.Y );
  except
    on E : Exception do
      Log.log( 'Error Melee NoPath: ' + E.Message );
  end;


end;

procedure TMeleeSparing.OnStop;
const
  FailName : string = 'TMeleeSparing.OnStop';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    Walking := false;
    if Assigned( Character.Track ) then
      Character.Face( Character.Track.X, Character.track.Y );
  except
    on E : Exception do
      Log.log( 'Error Melee Stop: ' + E.Message );
  end;


end;

procedure TMeleeSparing.WasAttacked( Source : TAniFigure; Damage : single );
const
  FailName : string = 'TMeleeSparing.WasAttacked';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if Source is TCharacter then
      character.Face( TCharacter( Source ).x, TCharacter( Source ).y );

    try
      if Source is TCharacter then
      begin
        if Character.IsEnemy( TCharacter( Source ) ) then
          if ( Character.Wounds / Character.HitPoints ) * 100 >= iTimeToRun then
          begin
            Character.say( 'Ok ok! Thats enough training for now.', clred );
            Character.MakeNeutral( 'player' );
            character.AIMode := aiNone;
            exit;
          end
      end;
    except
      on E : Exception do
        Log.log( 'Error MeleeSparing WasAttacked: ' + E.Message );
    end;

    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;


{ TEnviromentDamage }

destructor TEnviromentDamage.Destroy;
begin

end;

procedure TEnviromentDamage.Execute;
var
  Total : Single;
  iLoop : integer;
begin
  inherited;
  if ( frameCount mod Interval ) = 0 then
  begin
    for iLoop := 0 to NPCList.Count - 1 do
    begin
      Total := CalcTotalDamage( Character.Damage, TCharacter( NPCList[ iLoop ] ).Resistance, 1, false );
      TCharacter( NPCList[ iLoop ] ).TakeDamage( TCharacter( NPCList[ iLoop ] ), Total, 0, false );
    end;
  end;
end;

procedure TEnviromentDamage.Init;
var
  s : string;
begin
  S := Character.Properties[ 'LeashLength' ];
  try

    if S = '' then
      Interval := 166
    else
      Interval := StrToInt( S );
  except
    Interval := 166;
  end;

end;

{ TObliesk }

procedure TObelisk.Clicked;
var
  i : integer;
begin
  try
    if current.combatMode then
      if not ( Character.IsEnemy( current ) ) then
      begin
        Current.CombatMode := false;
        for i := 0 to NPCList.count - 1 do
        begin
          TCharacter( NPCList.items[ i ] ).CombatMode := Current.CombatMode;
          frmMain.PaintCharacterOnBorder( TSpriteObject( NPCList.items[ i ] ), i );
        end;
      end;
  except
    on E : Exception do
      Log.log( 'Error Obelisk Clicked: ' + E.Message );
  end;


end;

{ TRunScript }

destructor TRunScript.Destroy;
begin

end;

procedure TRunScript.Execute;
begin
  inherited;
  try
    if NewFrame = 0 then
      NewFrame := FrameCount + Interval;

    if ( frameCount > NewFrame ) then
    begin
      NewFrame := 0;
//        log.log('RunScript: ' + strScript);
      if strScript <> '' then
        RunScript( Character, strScript );
    end;
  except
    on E : Exception do
      Log.log( 'Error Runscript Execute: ' + E.Message );
  end;
end;

procedure TRunScript.Init;
var
  s : string;
begin
  NewFrame := 0;
  S := LowerCase( Character.Properties[ 'Script' ] );
  try
    if S = '' then
      strScript := ''
    else
      strScript := s;
  except
    strScript := '';
  end;

  S := Character.Properties[ 'LeashLength' ];
  try
    if S = '' then
      Interval := 166
    else
      Interval := StrToInt( S );
  except
    Interval := 166;
  end;

end;

{ TClonePlayer }

procedure TClonePlayer.Init;
var
  NewGuard : TCharacter;
  Effect : TEffect;

begin
  try

    Player.Clone( TObject( NewGuard ), 'MIPlayer' );

    NewGuard.SetPos( Character.X, Character.Y, Character.z );
    TCharacterResource( NewGuard.Resource ).CenterY := 101;
    //  NewGuard.Face(Character.Track.x, character.track.y);
    //  NewGuard.Facing := Character.Facing;
    //  NewGuard.SpecialEffect := seNone;
    //  NewGuard.Highlightable := true;
    //  NewGuard.alpha := 75;
    //  NewGuard.SpecialEffect := seTranslucent;   //   NewGuard.HitPoints := 1;



    NewGuard.Alliance := 'ahoul';
    NewGuard.Name := '';
     // NewGuard.CombatAI := 'HumanoidCasterCombat';
    NewGuard.Properties[ 'CombatMode' ] := 'True';
    NewGuard.Properties[ 'MyFriends' ] := 'ahoul';
    NewGuard.Properties[ 'MyEnemies' ] := 'party';
    NewGuard.Properties[ 'EquipmentLocked' ] := 'true';
    NewGuard.Properties[ 'Transparent' ] := '75';
    NewGuard.Properties[ 'AttackDelay' ] := '3';
    NewGuard.Properties[ 'BaseCourage' ] := '6';
    NewGuard.Properties[ 'TimeToRun' ] := '90';
    NewGuard.Properties[ 'Distance' ] := '175';
    NewGuard.Properties[ 'HealFirst' ] := 'False';
    NewGuard.Properties[ 'CanStop' ] := 'True';
    NewGuard.Properties[ 'Combative' ] := 'True';
    NewGuard.Properties[ 'IdleDuty' ] := 'Stand';
    NewGuard.Mana := 500;
    NewGuard.Vision := 300;
    NewGuard.Properties[ 'DeathSounds' ] := 'malekill3,malekill5,malekill7';
    NewGuard.Properties[ 'PainSounds' ] := 'malegrunt2,malegrunt4,malegrunt6';
    NewGuard.GroupName := 'ahoul';
    NewGuard.Properties[ 'OSpellBook' ] := 'invis,buff,mirrorimage';
    NewGuard.Properties[ 'DSpellBook' ] := 'protection from all,deflect,mana thief,heal,aura of steel';
    NewGuard.Properties[ 'SummonGUID' ] := 'Undead2C6E8D18592D106';
    NewGuard.addTitle( 'Reflect' );
    NewGuard.addtitle( 'Deflectfirst' );
    NewGuard.Addtitle( 'IgnoreStealth' );
    NewGuard.Properties[ 'BalanceWithPlayer' ] := '7';
    NewGuard.OnDie := 'doeffect(Fadeaway)';
    NewGuard.Properties[ 'lockedequipment' ] := 'true';

    NewGuard.AIMode := AIIdle;
    NewGuard.CombatAI := 'HumanoidCasterCombat';
    NewGuard.IdleAI := 'HumanoidIdle';
    NewGuard.AI := THumanoidIdle.Create;
    NewGuard.RemoveTitle( 'fireball' );
    NewGuard.RemoveTitle( 'freeze' );
    NewGuard.Track := current;
    NewGuard.Hitpoints := NewGuard.Hitpoints / 2;
    NewGuard.Mysticism := NewGuard.Mysticism div 2;
    NewGuard.enabled := true;


    Effect := TProtectMeEffect.create;
    Player.AddEffect( Effect );

  except
    on E : Exception do
      Log.log( 'Error Clone Player: ' + E.Message );
  end;
end;

end.
