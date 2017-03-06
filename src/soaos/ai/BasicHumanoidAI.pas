unit BasicHumanoidAI;
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
{ NOTE: The source in this file has been auto-formatted by Jim Shiflett       }
{ using DelForExp, which will eventually be used on ALL Digital Tome code.    }
{*****************************************************************************}

interface

{$INCLUDE Anigrp30cfg.inc}

uses
  Windows,
  Classes,
  SysUtils,
  Graphics,
  Character,
  Anigrp30,
  strFunctions,
  Resource,
  digifx,
  anidemo,
  spells1;

type
  TIdleDuty = ( idStand, idMeander, idGuard, idbusy, idConverse, idAction );
  TSFXDelayType = ( dtFixed, dtRandom );
  TMainStat = ( msStrength, msHitPoints, msCombat, msMysticism, msMana );

  THumanoidIdle = class( TAI )
  private
    StuckTracker : integer;
    OldStand : string;
    NewStand : string;
    StandInterval : integer;
    Walking : Boolean;
    PathDev : integer;
    Delay : Integer;
    CenterX : Integer;
    CenterY : Integer;
    GroupList : TStringList;
    ContainersList : TStringList;
    PathCornersList : TStringList;
    CurrentPath : TGameObject;
    PathStart : TGameObject;
    PathEnd : TGameObject;
    bHarassing : boolean;
    bShutUp : boolean;
    bHitDoor : boolean;
    ReadyToTalk : boolean;
    PlayStartScr : boolean;
    PlayEndScr : boolean;
    ArrivalScr : string;
    Halo : Boolean;
    TalkToMeResource : TResource;
    TalkToMeTitles : string;
    TalkToMeTitle : string;
    TalkToMeCount : integer;
    bTalkToMe : boolean;
    WorkOffScreen : Boolean;
    //Soft
    MeanderDelay : integer;
    FGlowEffectResource : TResource;
    FSpellEffectResource : TResource;
    CombatSay : string;
    strCnvScr : string;
    strPathStart : string;
    strPathEnd : string;
    bCanStop : boolean;
    iLeash : Integer;
    IdleDuty : TIdleDuty;
    bCombative : Boolean;
    bTalk : Boolean;
    Facing : string;
    CircleDelay : integer;
    //Sounds
    IdleSFXDelay : integer;
    iSFXDelayCount : integer;
    PlayIdleSFX : boolean;
    bPlaySFXMetal : boolean;
    bPlaySFXAttack : boolean;
    bPlaySFXOther : boolean;
    SFXDelayType : TSFXDelayType;

    strPlayerSay : string;
    strFriendSay : string;
    strNeutralSay : string;
    strEnemySay : string;
    strPlayerRsp : string;
    strFriendRsp : string;
    strNeutralRsp : string;
    strEnemyRsp : string;
    tmpEnemies : string;
    strAction : string;
    procedure FindTarget;
    procedure Meander;
    procedure WalkPath;
    procedure Work;
    procedure DetectChar;
    procedure Talk;
    procedure PlaySounds;
    procedure HaloEffect;
    procedure TalkToMeEffect;

  protected

    procedure OnStop; override;
    procedure WasAttacked( Source : TAniFigure; Damage : Single ); override;
    procedure OnNoPath; override;
    function OnCollideFigure( Target : TAniFigure ) : boolean; override;
    function OnCollideObject( Target : TAniFigure ) : Boolean; override;
    procedure WasKilled( Source : TAniFigure ); override;
  public
    strdisguise : string;
    destructor Destroy; override;
    procedure Clicked; override;
    procedure Init; override;
    procedure Follow( Source, Target : TAniFigure ); override;
    procedure Execute; override;
  end;



  THumanoidCombat = class( TAI )
  private
    FBonusCourage : integer;
    FBaseCourage : integer;

  protected
    procedure WasAttacked( Source : TAniFigure; Damage : Single ); override;
    procedure WasKilled( Source : TAniFigure ); override;
  public
    destructor Destroy; override;
    property BonusCourage : Integer read FBonusCourage write FBonusCourage;
    property BaseCourage : Integer read FBaseCourage write FBaseCourage;
  end;


  THumanoidMeleeCombat = class( THumanoidCombat )
  private
    Walking : Boolean;
    ReadyToAttack : Boolean;
    CirclePoint : integer;
    RunForIt : Boolean;
    CombatSay : string;
    FSpellEffectResource : TResource;
    //RunAway: Boolean;
    Delay : integer;
    PartyTotal : integer;
    CollideCount : integer;
    waiting : boolean;
    OldRadius : integer;
    NewRadius : integer;

    //Soft
    iTimeToRun : Integer;
    bTakeOrders : Boolean;
    FBaseCourage : Integer;
    FBonusCourage : integer;
    AttackDelay : integer;
    iDamageShield : integer;
    AllowRun : Boolean;
    procedure RunAway;
    procedure FindNextTarget;
    procedure Attack;
    procedure TellFriends;
  protected
    procedure OnStop; override;
    procedure WasAttacked( Source : TAniFigure; Damage : Single ); override;
    procedure WasKilled( Source : TAniFigure ); override;
    function OnCollideObject( Target : TAniFigure ) : Boolean; override;
    function OnCollideFigure( Target : TAniFigure ) : Boolean; override;
    procedure OnNoPath; override;
  public
    destructor Destroy; override;
    procedure Init; override;
    property BonusCourage : Integer read FBonusCourage write FBonusCourage;
    property BaseCourage : Integer read FBaseCourage write FBaseCourage;
    procedure Execute; override;
    procedure CallToArms( Source, Target : TAniFigure ); override;
    procedure Regroup( Source : TAniFigure; NewX, NewY : Integer ); override;
    procedure NotifyOfDeath( Source : TAniFigure ); override;
  end;

  THumanoidArcherCombat = class( THumanoidCombat )
  private
    Walking : boolean;
    FriendsList : TStringList;
    CirclePoint : integer;
    Delay : integer;
    PartyTotal : integer;
    ShotCounter : integer;
    RunOrFight : boolean;
    RunAway : Boolean;
    MaxShots : integer;
    //Soft
    AttackDelay : integer;
    iTimeToRun : integer;
    bTakeOrders : Boolean;
    fBaseCourage : integer;
    fBonusCourage : integer;
    iDistance : integer;
    bMove : boolean;
    AllowRun : Boolean;
    procedure MoveAway;
    procedure Attack;
    procedure FindTarget;
    procedure BattleTactic;
  protected
    procedure OnStop; override;
    procedure WasAttacked( Source : TAniFigure; Damage : single ); override;
    procedure WasKilled( Source : TAniFigure ); override;
    function OnCollideFigure( Target : TAniFigure ) : boolean; override;
    function OnCollideObject( Target : TAniFigure ) : Boolean; override;
    procedure OnNoPath; override;
  public
    destructor Destroy; override;
    procedure Init; override;
    property BonusCourage : Integer read FBonusCourage write FBonusCourage;
    property BaseCourage : Integer read FBaseCourage write FBaseCourage;
    procedure Execute; override;
    procedure CallToArms( Source, Target : TAniFigure ); override;
    procedure Regroup( Source : TAniFigure; NewX, NewY : Integer ); override;
    procedure NotifyOfDeath( Source : TAniFigure ); override;
    function InRange( Target : TAniFigure ) : Boolean;
  end;


  THumanoidCasterCombat = class( THumanoidCombat )
  private
    Walking : boolean;
    mirror : boolean;
    Friendly : TCharacter;
    FriendsList : TStringList;
    CirclePoint : integer;
    Delay : integer;
    PauseAndExit : integer;
    PartyTotal : integer;
    NukeCounter : integer;
    RunOrFight : boolean;
    RunAway : Boolean;
    CastTimes : integer;
    Spells : TStringList;
    MySummons : TList;
    oSpellBook : string;
    dSpellBook : string;
    HoldCast : boolean;
    FReceiveResource : TResource;
    //FCastResource: TResource;
    FSpellEffectResource : TResource;
    CastEffect : TResource;
    FSummonSpawnSource : TCharacter;
    FSummonGUID : string;
    //Soft
    iTimeToRun : integer;
    bTakeOrders : Boolean;
    fBaseCourage : integer;
    fBonusCourage : integer;
    iDistance : integer;
    bHealFirst : boolean;
    bMove : boolean;
    iDamageShield : integer;
    AllowRun : boolean;
    procedure MoveAway;
    procedure Attack;
    procedure CastHeal;
    procedure FindFriendly;
    procedure FindTarget;
    procedure BattleTactic;
    procedure SummonGuards;
    procedure MirrorImage;
    procedure CastInvis;
    procedure SummonHellHound;
    procedure BuffAllies;
  protected
    procedure OnStop; override;
    procedure WasAttacked( Source : TAniFigure; Damage : single ); override;
    procedure WasKilled( Source : TAniFigure ); override;
    function OnCollideFigure( Target : TAniFigure ) : boolean; override;
    function OnCollideObject( Target : TAniFigure ) : Boolean; override;
    procedure OnNoPath; override;
  public
    destructor Destroy; override;
    procedure Init; override;
    property BonusCourage : Integer read FBonusCourage write FBonusCourage;
    property BaseCourage : Integer read FBaseCourage write FBaseCourage;
    procedure Execute; override;
    procedure CallToArms( Source, Target : TAniFigure ); override;
    procedure Regroup( Source : TAniFigure; NewX, NewY : Integer ); override;
    procedure NotifyOfDeath( Source : TAniFigure ); override;
    function InRange( Target : TAniFigure ) : Boolean;
  end;


  THumanoidHeroCombat = class( TAI )
  private
    Walking : Boolean;
    Delay : integer;
    PartyTotal : integer;
    GoodCollideCount : integer;
    BadCollideCount : integer;
//    FSpellEffectResource: TResource;
    //Soft
    AttackDelay : integer;
    FBaseCourage : Integer;
    FBonusCourage : integer;
    bTakeOrders : Boolean;
    iDistance : integer;
    MainStat : TMainStat;
    AllowRun : boolean;
    procedure Attack;
    procedure FindTarget;
    procedure RunAway;

  protected
    procedure OnNoPath; override;
    procedure OnStop; override;
    procedure WasAttacked( Source : TAniFigure; Damage : Single ); override;
    procedure WasKilled( Source : TAniFigure ); override;
    function OnCollideObject( Target : TAniFigure ) : Boolean; override;
    function OnCollideFigure( Target : TAniFigure ) : Boolean; override;
  public
    destructor Destroy; override;
    procedure Init; override;
    property BonusCourage : Integer read FBonusCourage write FBonusCourage;
    property BaseCourage : Integer read FBaseCourage write FBaseCourage;
    procedure Execute; override;
    procedure CallToArms( Source, Target : TAniFigure ); override;
    procedure Regroup( Source : TAniFigure; NewX, NewY : Integer ); override;
    procedure NotifyOfDeath( Source : TAniFigure ); override;
  end;

  THumanoidHunterCombat = class( THumanoidCombat )
  private
    Walking : Boolean;
    ReadyToAttack : Boolean;
    bRunAway : Boolean;
    Delay : integer;
    PartyTotal : integer;
    CollideCount : integer;
    RangeAttack : boolean;
    bHasBow : Boolean;
    AttackDelay : integer;
//    FSpellEffectResource: TResource;
    //Soft
    FBaseCourage : Integer;
    FBonusCourage : integer;
    bTakeOrders : Boolean;
    iDistance : integer;
    MainStat : TMainStat;
    procedure RunAway;
    procedure Attack;
    procedure FindTarget;
  protected
    procedure OnStop; override;
    procedure WasAttacked( Source : TAniFigure; Damage : Single ); override;
    procedure WasKilled( Source : TAniFigure ); override;
    function OnCollideFigure( Target : TAniFigure ) : Boolean; override;
    function OnCollideObject( Target : TAniFigure ) : Boolean; override;
    procedure OnNoPath; override;
  public
    destructor Destroy; override;
    procedure Init; override;
    property BonusCourage : Integer read FBonusCourage write FBonusCourage;
    property BaseCourage : Integer read FBaseCourage write FBaseCourage;
    procedure Execute; override;
    procedure CallToArms( Source, Target : TAniFigure ); override;
    procedure Regroup( Source : TAniFigure; NewX, NewY : Integer ); override;
    procedure NotifyOfDeath( Source : TAniFigure ); override;
  end;

  THaloEffect = class( TEffect )
  private
    Points : integer;
    PointList : pointer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Adjust( Character : TCharacter ); override;
    procedure RenderLocked( Figure : TAniFigure; Bits : PBITPLANE ); override;
    function DoFrame : boolean; override;
  end;

  TInvisEffect = class( TEffect )
  private
    FCharacter : TCharacter;
  public
    destructor Destroy; override;
    procedure Adjust( Character : TCharacter ); override;
    function DoFrame : boolean; override;
  end;

  TColorMeEffect = class( TEffect )
  private
  public
    Red : integer;
    Green : integer;
    Blue : integer;
    UseColor : Boolean;
    destructor Destroy; override;
    procedure Adjust( Character : TCharacter ); override;
    function DoFrame : boolean; override;
  end;

  TProtectMeEffect = class( TEffect )
  private
  public
    destructor Destroy; override;
    procedure Adjust( Character : TCharacter ); override;
    function DoFrame : boolean; override;
  end;

  TShieldSpellEffect = class( TEffect )
  private
    FCharacter : TCharacter;
  public
    destructor Destroy; override;
    constructor Create;
    procedure Adjust( Character : TCharacter ); override;
    function DoFrame : boolean; override;
  end;

  TGlowEffect = class( TEffect )
  private
    FCharacter : TCharacter;
  public
    destructor Destroy; override;
    constructor Create;
    procedure Adjust( Character : TCharacter ); override;
    procedure RenderLocked( Figure : TAniFigure; Bits : PBITPLANE ); override;
    function DoFrame : boolean; override;
  end;

  TPulse = class( TEffect )
  private
    FCharacter : TCharacter;
    Red : integer;
    Blue : integer;
    Green : integer;
    RedDelta : integer;
    BlueDelta : integer;
    GreenDelta : integer;
  public
    RedMaster : integer;
    BlueMaster : integer;
    GreenMaster : integer;
    constructor Create;
    procedure Adjust( Character : TCharacter ); override;
    function DoFrame : boolean; override;
  end;

function AssignHumanoidAI( AIName : string ) : TAI;
function RangeTest( Target, Source : TAniFigure; iDist : integer ) : boolean;
function LineOfSight( Character, Track : TAniFigure ) : boolean;
function GetFacing( SrcX, SrcY, TargetX, TargetY : Longint ) : Extended;
function GetSpellEffect( const s : string ) : TResource;

var
  SummonCount : integer;

implementation

uses engine,
  LogFile,
  Effects,
  Spells;

const
  PI = 3.1415926535;
  pi2 = 2 * PI;
  clRed = TColor( $0000FF );
  clWhite = TColor( $FFFFFF );

function AssignHumanoidAI( AIName : string ) : TAI;
var
  S : string;
const
  FailName : string = 'BasicHumanoidAI.AssignHumanoidAI';
begin
  Result := nil;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    S := LowerCase( AIName );

    if ( S = 'humanoididle' ) then
      Result := THumanoidIdle.Create
    else if ( S = 'humanoidmeleecombat' ) then
      Result := THumanoidMeleeCombat.Create
    else if ( S = 'humanoidarchercombat' ) then
      Result := THumanoidArcherCombat.Create
    else if ( S = 'humanoidherocombat' ) then
      Result := THumanoidheroCombat.Create
    else if ( S = 'humanoidhuntercombat' ) then
      Result := THumanoidHunterCombat.Create
    else if ( S = 'humanoidcastercombat' ) then
      Result := THumanoidCasterCombat.Create
    else if ( S = 'humanoidcombat' ) then
      Result := THumanoidCombat.Create


  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function GetSpellEffect( const s : string ) : TResource;
var
  i : integer;
begin
  Result := FireEffect;

  for i := AllCastResourceList.Count - 1 downto 0 do
  begin
    if LowerCase( TResource( AllCastResourceList.Items[ i ] ).filename ) = 'engine\spells\' + s then
    begin
      Result := TResource( AllCastResourceList.Items[ i ] );
      break;
    end;
  end;
end;

function RangeTest( Target, Source : TAniFigure; iDist : integer ) : boolean;
var
  D : Double;
const
  FailName : string = 'BasicHumanoidAI.RangeTest';
begin
  Result := false;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Result := false;
    D := sqrt( sqr( Target.X - Source.X ) + sqr( 2 * ( Target.Y - Source.Y ) ) );
    if D <= iDist then
      Result := true;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;

end;

function LineOfSight( Character, Track : TAniFigure ) : boolean;
begin
  result := true;
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



{ THumanoidIdle }

procedure THumanoidIdle.Execute;
const
  FailName : string = 'THumanoidIdle.Execute';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;

    if not ( character.OnScreen ) and not ( WorkOffScreen ) then
      exit;

  // if (Character.Alpha < 200) then
  //   Inc(Character.Alpha,2)
  // else
  //      Character.SpecialEffect:=seNone;

    if StandInterval > 10 then
      if ( FrameCount mod StandInterval ) = 0 then
      begin
        case Random( 2 ) of
          0 : Character.StandAction := newStand;
          1 : Character.StandAction := oldStand;
        end;
      end;

    if TalkToMeCount > 0 then
      TalkToMeEffect;

    if PlayIdleSFX then
      PlaySounds;

    if Halo then
      HaloEffect;

    if bCombative then
      FindTarget;
    try
      if ArrivalScr <> '' then
      begin
        RunScript( Character, ArrivalScr );
        ArrivalScr := '';
        exit;
      end;
    except
      on E : Exception do
        Log.log( 'Error HumanoidIdle Execute1: ' + E.Message );
    end;

    try
      if PlayStartScr then
      begin
        RunScript( Character, strPathStart );
        PlayStartScr := false;
        exit;
      end;
    except
      on E : Exception do
        Log.log( 'Error HumanoidIdle Execute2: ' + E.Message );
    end;
    try
      if PlayEndScr then
      begin
        RunScript( Character, strPathEnd );
        PlayEndScr := false;
        exit;
      end;
    except
      on E : Exception do
        Log.log( 'Error HumanoidIdle Execute3: ' + E.Message );
    end;

    if ( FrameCount mod 390 ) = 0 then
      walking := false;
    //If Assigned(Track) then fight;

    if bTalk and not ( bShutUp ) and not ( bHarassing ) then
      DetectChar;

    if Walking and ( IdleDuty = idConverse ) then
    begin
      if ( frameCount mod 10 ) = 0 then
        if not ( Character.InRange( Current ) ) then
          Character.WalkTo( Current.X, Current.Y, 64 );
    end;

    if ( StuckTracker = 0 ) and Assigned( CurrentPath ) then
      if TPathCorner( CurrentPath ).NextDestination = '' then
        CurrentPath := nil
      else
        CurrentPath := GetGUID( TPathCorner( CurrentPath ).NextDestination );


    if not walking then
    begin
      if Delay > 0 then
      begin
        dec( Delay );
        exit;
      end;
      Dec( stuckTracker );
      case IdleDuty of
        idMeander :
          Meander;
        idGuard :
          WalkPath;
        idBusy :
          Work;
        idConverse :
          Talk;
        idAction :
          begin
            Character.DoAction( strAction );
            Delay := Random( iLeash );
            if Delay < 16 then
              Delay := 16;
            iSFXDelayCount := 13;
            if strCnvScr <> '' then
              RunScript( Character, strCnvScr );
          end;
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure THumanoidIdle.TalkToMeEffect;
var
  Effect : TEffect;
  i : integer;
begin
  try
    if ( CircleDelay > 0 ) then
    begin
      Dec( CircleDelay );
    end
    else
    begin
      for i := 0 to TalkToMeCount - 1 do
      begin
        if not ( Character.TitleExists( StrTokenAt( TalkToMeTitle, '|', i ) ) ) then
        begin
          if Assigned( TalkToMeResource ) then
          begin
            Effect := TEffect.Create;
            Effect.Resource := TalkToMeResource;
            Effect.AnimationDuration := 8 * TalkToMeResource.FrameMultiplier;
            Effect.Power := Character.Mysticism;
            Effect.DoAction( 'Default', Character.FacingString );
            Character.AddEffect( Effect );
            Break;
          end;
        end;
      end;
      Circledelay := 6;
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidIdle TalkToMe: ' + E.Message );
  end;

end;

procedure THumanoidIdle.HaloEffect;
var
  Effect : TEffect;
begin
  try
    Effect := THaloEffect.Create;
    Effect.Duration := 0;
    character.AddEffect( Effect );
    Halo := false;
  except
    on E : Exception do
      Log.log( 'Error HumanoidIdle HaloEffect: ' + E.Message );
  end;

end;

procedure THumanoidIdle.PlaySounds;
begin
  try
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
  except
    on E : Exception do
      Log.log( 'Error HumanoidIdle PlaySFX: ' + E.Message );
  end;

end;

procedure THumanoidIdle.DetectChar;
var
  List : TStringList;
  tmp : integer;
  i : integer;
const
  FailName : string = 'THumanoidIdle.DetectChar';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    tmp := 150;
   //watch for good guys
    if ( FrameCount mod 10 ) = 0 then
    begin
      try
        List := GetNearbyCharacter( Character, 150 );

        if Assigned( List ) then
        begin
          bHarassing := true;
          try
            while List.Count > 1 do
            begin
              dec( tmp );
              for i := 0 to List.Count - 1 do
                if TCharacter( List.objects[ i ] ).distance > tmp then
                begin
                  list.Delete( i );
                  break;
                end;
            end;
            Character.Track := TCharacter( List.objects[ 0 ] );
          except
            on E : Exception do
              Log.log( 'Error HumanoidIdle DetectChar1: ' + E.Message );
          end;

          try
            if Character.Track = Current then
            begin
              if StrPlayerSay <> '' then
                character.Say( StrTokenAt( StrPlayerSay, ',', Random( StrTokenCount( StrPlayerSay, ',' ) ) ), clred );
              if StrPlayerRsp <> '' then
                Current.Say( StrTokenAt( StrPlayerRsp, ',', Random( StrTokenCount( StrPlayerRsp, ',' ) ) ), clred );
            end;
          except
            on E : Exception do
              Log.log( 'Error HumanoidIdle DetectChar2: ' + E.Message );
          end;

          try
            if ( Character.IsAlly( Character.Track ) ) and ( Character.Track <> Current ) then
            begin
              if StrFriendSay <> '' then
                character.Say( StrTokenAt( StrFriendSay, ',', Random( StrTokenCount( StrFriendSay, ',' ) ) ), clred );
              if StrFriendRsp <> '' then
                Current.Say( StrTokenAt( StrFriendRsp, ',', Random( StrTokenCount( StrFriendRsp, ',' ) ) ), clred );
            end;
          except
            on E : Exception do
              Log.log( 'Error HumanoidIdle DetectChar3: ' + E.Message );
          end;
          try
            if ( Character.IsNeutral( Character.Track ) ) and ( Character.Track <> Current ) then
            begin
              if StrNeutralSay <> '' then
                character.Say( StrTokenAt( StrNeutralSay, ',', Random( StrTokenCount( StrNeutralSay, ',' ) ) ), clred );
              if StrNeutralRsp <> '' then
                Current.Say( StrTokenAt( StrNeutralRsp, ',', Random( StrTokenCount( StrNeutralRsp, ',' ) ) ), clred );
            end;
          except
            on E : Exception do
              Log.log( 'Error HumanoidIdle DetectChar4: ' + E.Message );
          end;
          try
            if ( Character.IsEnemy( Character.Track ) ) and ( Character.Track <> Current ) then
            begin
              if StrEnemySay <> '' then
                character.Say( StrTokenAt( StrEnemySay, ',', Random( StrTokenCount( StrEnemySay, ',' ) ) ), clred );
              if StrEnemyRsp <> '' then
                Current.Say( StrTokenAt( StrEnemyRsp, ',', Random( StrTokenCount( StrEnemyRsp, ',' ) ) ), clred );
            end;
          except
            on E : Exception do
              Log.log( 'Error HumanoidIdle DetectChar5: ' + E.Message );
          end;

          list.free;
        end
        else
          bHarassing := false;
      except
        on E : Exception do
          Log.log( 'Error HumanoidIdle DetectChar6: ' + E.Message );
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;


procedure THumanoidIdle.FindTarget;
var
  List : TStringList;
  FriendList : TStringList;
  J : integer;
  iStealth : integer;
const
  FailName : string = 'THumanoidIdle.FindTarget';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
//  exit;
    if ( FrameCount mod 33 ) = 0 then
    begin
      try
        if current.Dead then
          exit;

        if ( LowerCase( character.Enemies ) = 'party' ) or ( lowerCase( tmpEnemies ) = 'party' ) then
        begin
          if character.TitleExists( 'IgnoreStealth' ) then
            istealth := ( 1 div 100 )
          else if Current.Stealth < 1 then
            istealth := ( 1 div 100 )
          else
            iStealth := ( current.stealth div 100 );

          if ( strdisguise <> '' ) and Character.IsEnemy( player ) then
            if not ( character.RangeTo( Current.x, Current.y ) < ( ( character.Vision - ( character.Vision * iStealth ) ) * GetFacing( character.x, character.y, current.x, current.y ) ) ) or
              not ( game.LineOfSight( character.x, character.y, current.x, current.y ) ) then
            begin
              if ( Player.IsWorn( strdisguise ) ) and ( NPCList.count < 2 ) then
              begin
                Character.Properties[ 'DisguiseDetected' ] := 'false';
                Character.MakeNeutral( 'party' );
              end;
            end;

        //  if (strdisguise <> '') and Character.IsEnemy(player) then
        //    strdisguise := '';

          if character.RangeTo( Current.x, Current.y ) < ( ( character.Vision - ( character.Vision * iStealth ) ) * GetFacing( character.x, character.y, current.x, current.y ) ) then
          begin
            if game.LineOfSight( character.x, character.y, current.x, current.y ) then
            begin
              if strdisguise <> '' then
              begin
                if Player.IsWorn( strdisguise ) and ( NPCList.count < 2 ) then
                  exit
                else if Pos( 'party', Lowercase( tmpEnemies ) ) <> 0 then
                begin
                  Character.MakeEnemy( 'party' );
                  Character.Properties[ 'DisguiseDetected' ] := 'true';
                end;
              end;

              character.StandAction := '';
              StandInterval := -1;

              if Player.IsWorn( strdisguise ) and ( NPCList.count >= 2 ) then
                character.track := TCharacter( NPCList[ NPCList.count - 1 ] )
              else
                character.track := TCharacter( NPCList[ Random( NPCList.count ) ] );

              if character.RangeTo( character.track.x, character.track.y ) > ( ( character.Vision - ( character.Vision * iStealth ) ) * GetFacing( character.x, character.y, character.track.x, character.track.y ) ) then
              begin
                character.track := current;
              end;

              character.AIMode := aiCombat;

              if CombatSay <> '' then
                Character.Say( CombatSay, clRed );

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

            if List.Count = 1 then
              Character.Track := TCharacter( List.objects[ 0 ] )
            else
              Character.Track := TCharacter( List.objects[ random( List.count ) ] );
            character.AIMode := aiCombat;

            if CombatSay <> '' then
              Character.Say( CombatSay, clRed );
            list.free;
          end;
        end;
      except
        on E : Exception do
          Log.log( 'Error HumanoidIdle FindTarget: ' + E.Message );
      end;

    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure THumanoidIdle.Talk;
const
  FailName : string = 'THumanoidIdle.Talk';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if ReadyToTalk then
    begin
      try
        character.Face( current.x, current.y );
        RunScript( Character, strCnvScr );
        idleDuty := idGuard;
        TCharacterResource( character.Resource ).speed := 4;
        Walking := false;
      except
        on E : Exception do
          Log.log( 'Error HumanoidIdle Talk1: ' + E.Message );

      end;
    end
    else if not ( Walking ) then
    begin
      if not ( Current.Dead ) then
      begin
        try
          if not ( Character.InRange( Current ) ) then
          begin
            Character.WalkTo( Current.X, Current.Y, 64 );
            Walking := true;
          end;
        except
          on E : Exception do
            Log.log( 'Error HumanoidIdle Talk2: ' + E.Message );
        end;

      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;


procedure THumanoidIdle.Meander;
var
  r : Integer;
  T : single;
  X, Y : Integer;
const
  FailName : string = 'THumanoidIdle.Meander';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    //Pick A random direction
    try

      if iLeash <> 0 then
      begin
        r := random( iLeash );
        T := pi2 * random( 360 ) / 360;
        X := round( r * cos( T ) ) + CenterX;
        Y := round( r * sin( T ) ) + CenterY;
      end
      else
      begin
        exit;
        X := random( 200 ) + 50 + CenterX;
        Y := random( 200 ) + 50 + CenterY;
      end;
      Walking := True;
      Character.walkTo( X, Y, 16 );
      if MeanderDelay = 0 then
        delay := Random( 200 ) + 200
      else
        delay := MeanderDelay;

    except
      on E : Exception do
        Log.log( 'Error HumanoidIdle Meander: ' + E.Message );
    end;


  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure THumanoidIdle.WalkPath;
const
  FailName : string = 'THumanoidIdle.WalkPath';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if not ( assigned( GroupList ) ) then
      exit;
    //Pathcorner stuff here
    if PathCornersList.Count > 0 then
    begin
      if not assigned( CurrentPath ) then
      begin //Already Have Destination
        try
          CurrentPath := TGameObject( PathCornersList.objects[ random( PathCornersList.count ) ] );
          if assigned( CurrentPath ) then
          begin
            walking := True;
            Facing := CurrentPath.Properties[ 'Facing' ];
            Character.WalkTo( CurrentPath.X, CurrentPath.Y, PathDev );
            Delay := StrToInt( CurrentPath.Properties[ 'Delay' ] );
          end;
        except
          on E : Exception do
            Log.log( 'Error HumanoidIdle WalkPath1: ' + E.Message );
        end;

      end
      else
      begin //walk to Destination
        try
          walking := True;
          Facing := CurrentPath.Properties[ 'Facing' ];
          Character.WalkTo( CurrentPath.X, CurrentPath.Y, PathDev );
          Delay := StrToInt( CurrentPath.Properties[ 'Delay' ] );
        except
          on E : Exception do
            Log.log( 'Error HumanoidIdle WalkPaht2: ' + E.Message );
        end;
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure THumanoidIdle.Work;
var
  tmp : Integer;
const
  FailName : string = 'THumanoidIdle.Work';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if not ( assigned( GroupList ) ) then
      exit;
    if Assigned( PathCornerSList ) then
      if PathCornersList.Count > 0 then
      begin
        try
          if not assigned( CurrentPath ) then
            CurrentPath := TGameObject( PathCornersList.objects[ random( PathCornersList.count ) ] );
          if Assigned( CurrentPath ) then
          begin
            case Random( 6 ) of
              5 :
                begin
                  if ContainersList.Count > 0 then
                  begin
                    tmp := random( ContainersList.count );
                    Walking := True;
                    Character.Approach( TSpriteObject( ContainersList.Objects[ tmp ] ) );
                  end;
                end;
              0..4 :
                begin
                  Walking := True;
                  Facing := CurrentPath.Properties[ 'Facing' ];
                  Character.WalkTo( CurrentPath.X, CurrentPath.Y, 16 );
                end;
            end;
            Delay := StrToInt( CurrentPath.Properties[ 'Delay' ] );
          end;
        except
          on E : Exception do
            Log.log( 'Error HumanoidIdle Work1: ' + E.Message );
        end;
      end
      else
      begin //no PathCorners in Group

        if Assigned( GroupList ) then
        begin
          try
            tmp := random( GroupList.count );
            if ( GroupList.Objects[ tmp ] is TSpriteObject ) then
              if ( GroupList.Objects[ tmp ] <> Character ) then
              begin
                if ( GroupList.Objects[ tmp ] is TContainer ) then
                begin
                  walking := true;
                  Character.Approach( TSpriteObject( GroupList.Objects[ tmp ] ) )
                end
                else if not ( ( GroupList.Objects[ tmp ] is TCharacter ) ) then
                begin
                  walking := true;
                  Character.WalkTo( TSpriteObject( GroupList.Objects[ tmp ] ).X, TSpriteObject( GroupList.Objects[ tmp ] ).Y, 16 );
                end;
                delay := Random( 200 ) + 200;
              end;
          except
            on E : Exception do
              Log.log( 'Error HumanoidIdle Work2: ' + E.Message );
          end;
        end;
      end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure THumanoidIdle.Init;
var
  S : string;
  sLoot : string;
  i, j : Integer;
  sStart : string;
  sEnd : string;
  Effect : TEffect;
const
  FailName : string = 'THumanoidIdle.Init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
 //  Halo := true;
 // Character.SpecialEffect:=seAdd;
 // Character.Alpha:=75;
 // Character.SpecialEffect:=seTranslucent;
 // Character.Alpha:=20;

    RunScript( character, character.OnLoad );

    //The AI is no longer being called when characters are idle off screen
    //so the Antipath code is no longer necessary for most characters
    //The following code prevents the very common accourance of two character
    //passing through one another just as they come onto the screen
    if not ( Character.TitleExists( 'NoAntiPath' ) ) then
    begin
      character.AntiPathEnabled := false;
      character.addtitle( 'NoAntiPath' );
    end;

    TalkToMeCount := 0;
    bTalkToMe := false;
    Circledelay := -1;
    StuckTracker := 300;
    CenterX := Character.X;
    CenterY := Character.Y;
    bHitDoor := false;
    PathDev := 64;
    PlayStartScr := false;
    PlayEndScr := false;
    TCharacterResource( character.Resource ).speed := 4;
    iSFXDelayCount := -1;
    StandInterval := -1;
    MeanderDelay := 0;
  //Say this when you see an enemy
    S := LowerCase( Character.Properties[ 'EnemySay' ] );
    try
      if S = '' then
        strEnemySay := ''
      else
        strEnemySay := s;
    except
      strEnemySay := '';
    end;

  // say this when you see a neutral
    S := LowerCase( Character.Properties[ 'NeutralSay' ] );
    try
      if S = '' then
        strNeutralSay := ''
      else
        strNeutralSay := s;
    except
      strNeutralSay := '';
    end;

    S := Character.Properties[ 'iSpeed' ];
    try
      if ( S <> '' ) and ( s <> '0' ) then
        TCharacterResource( character.Resource ).Speed := StrToInt( S );

    except
    end;

    S := Character.Properties[ 'MeanderDelay' ];
    try
      if ( S <> '' ) and ( s <> '0' ) then
        MeanderDelay := StrToInt( s );
    except
    end;
     //say this when you see a friend
    S := LowerCase( Character.Properties[ 'FriendSay' ] );
    try
      if S = '' then
        strFriendSay := ''
      else
        strFriendSay := s;
    except
      strFriendSay := '';
    end;

  //say this when you see the player
    S := LowerCase( Character.Properties[ 'PlayerSay' ] );
    try
      if S = '' then
        strPlayerSay := ''
      else
        strPlayerSay := s;
    except
      strPlayerSay := '';
    end;

  // enemy responce to your say
    S := LowerCase( Character.Properties[ 'EnemyRsp' ] );
    try
      if S = '' then
        strEnemyRsp := ''
      else
        strEnemyRsp := s;
    except
      strEnemyRsp := '';
    end;

    if Character.TitleExists( 'WorkOffScreen' ) then
    begin
      workOffScreen := true;
    end
    else
      workOffScreen := false;

    if Character.TitleExists( 'NoHighLight' ) then
      Character.Highlightable := false;

  // Neutral responce to your say
    S := LowerCase( Character.Properties[ 'NeutralRsp' ] );
    try
      if S = '' then
        strNeutralRsp := ''
      else
        strNeutralRsp := s;
    except
      strNeutralRsp := '';
    end;

  // friend responce to your say
    S := LowerCase( Character.Properties[ 'FriendRsp' ] );
    try
      if S = '' then
        strFriendRsp := ''
      else
        strFriendRsp := s;
    except
      strFriendRsp := '';
    end;

  // player responce to your say
    S := LowerCase( Character.Properties[ 'PlayerRsp' ] );
    try
      if S = '' then
        strPlayerRsp := ''
      else
        strPlayerRsp := s;
    except
      strPlayerRsp := '';
    end;
  //general say
    S := Character.Properties[ 'Say' ];
    try
      if S = '' then
        CombatSay := ''
      else
        CombatSay := S;
    except
      CombatSay := '';
    end;

    S := LowerCase( Character.Properties[ 'GlowEffect' ] );
    if ( s = 'true' ) then
    begin
      FGlowEffectResource := GetSpellEffect( 'glow.gif' );
      Character.AddTitle( 'cancelspelleffect' );
      Effect := TGlowEffect.Create;
      Effect.Resource := FGLowEffectResource;
      Effect.Duration := 0;
      Effect.AnimationDuration := Effect.Resource.FrameCount * Effect.Resource.FrameMultiplier;
      Character.AddEffect( Effect );
      Effect := nil;

    end;

    S := LowerCase( Character.Properties[ 'IdleEffect' ] );
    if ( s <> '' ) then
    begin
      if s = 'pulse' then
      begin
        Effect := TPulse.Create;
        TPulse( Effect ).BlueMaster := StrToInt( Character.Properties[ 'ColorBlue' ] );
        TPulse( Effect ).GreenMaster := StrToInt( Character.Properties[ 'ColorGreen' ] );
        TPulse( Effect ).RedMaster := StrToInt( Character.Properties[ 'ColorRed' ] );
        Character.AddTitle( 'cancelspelleffect' );
        Character.AddEffect( Effect );
        Effect := nil;
      end
      else
      begin
        FSpellEffectResource := GetSpellEffect( s );
        Character.AddTitle( 'cancelspelleffect' );
        Effect := TShieldSpellEffect.Create;
        Effect.Resource := FSpellEffectResource;
        Effect.Duration := 0;
        Effect.AnimationDuration := Effect.Resource.FrameCount * Effect.Resource.FrameMultiplier;
        Character.AddEffect( Effect );
        Effect := nil;
      end;
    end;




    //Just set some inital stats that will get me past a surprise attack
    S := LowerCase( Character.Properties[ 'BalanceWithPlayer' ] );
    try
      if ( S <> '' ) and ( s <> '0' ) then
      begin
        i := StrToInt( s );
        if i >= 0 then
        begin
          if player.TitleExists( 'Apprentice' ) then
          begin
            character.Wounds := 0;
            Character.Drain := 0;
            if character.BaseHitPoints > 0 then
              character.HitPoints := ( ( player.Perception * 2 ) );

            if character.Resistance.Heat.Invulnerability < ( player.mysticism div 10 ) then
              character.Resistance.Heat.Invulnerability := ( player.mysticism div 10 );
            if character.Resistance.Cold.Invulnerability < ( player.mysticism div 10 ) then
              character.Resistance.Cold.Invulnerability := ( player.mysticism div 10 );
            if character.Resistance.Electric.Invulnerability < ( player.mysticism div 10 ) then
              character.Resistance.Electric.Invulnerability := ( player.mysticism div 10 );
            if character.Resistance.Magic.Invulnerability < ( player.mysticism div 10 ) then
              character.Resistance.Magic.Invulnerability := ( player.mysticism div 10 );
            if character.Resistance.Poison.Invulnerability < ( player.mysticism div 10 ) then
              character.Resistance.Poison.Invulnerability := ( player.mysticism div 10 );
            if character.Resistance.Mental.Invulnerability < ( player.mysticism div 10 ) then
              character.Resistance.Mental.Invulnerability := ( player.mysticism div 10 );

          end;
          if player.TitleExists( 'Hunter' ) then
          begin
            character.Wounds := 0;
            Character.Drain := 0;
            if character.BaseHitPoints > 0 then
              character.HitPoints := ( ( player.Coordination * 2 ) );
            if character.Resistance.Heat.Invulnerability < ( player.mysticism div 20 ) then
              character.Resistance.Heat.Invulnerability := ( player.mysticism div 20 );
            if character.Resistance.Cold.Invulnerability < ( player.mysticism div 20 ) then
              character.Resistance.Cold.Invulnerability := ( player.mysticism div 20 );
            if character.Resistance.Electric.Invulnerability < ( player.mysticism div 20 ) then
              character.Resistance.Electric.Invulnerability := ( player.mysticism div 20 );
            if character.Resistance.Magic.Invulnerability < ( player.mysticism div 20 ) then
              character.Resistance.Magic.Invulnerability := ( player.mysticism div 20 );
            if character.Resistance.Poison.Invulnerability < ( player.mysticism div 20 ) then
              character.Resistance.Poison.Invulnerability := ( player.mysticism div 20 );
            if character.Resistance.Mental.Invulnerability < ( player.mysticism div 20 ) then
              character.Resistance.Mental.Invulnerability := ( player.mysticism div 20 );

          end;

          if player.TitleExists( 'Squire' ) then
          begin
            character.Wounds := 0;
            Character.Drain := 0;
            if character.BaseHitPoints > 0 then
              character.HitPoints := ( ( player.strength * 2 ) );
            if character.Resistance.Heat.Invulnerability < ( player.mysticism div 20 ) then
              character.Resistance.Heat.Invulnerability := ( player.mysticism div 20 );
            if character.Resistance.Cold.Invulnerability < ( player.mysticism div 20 ) then
              character.Resistance.Cold.Invulnerability := ( player.mysticism div 20 );
            if character.Resistance.Electric.Invulnerability < ( player.mysticism div 20 ) then
              character.Resistance.Electric.Invulnerability := ( player.mysticism div 20 );
            if character.Resistance.Magic.Invulnerability < ( player.mysticism div 20 ) then
              character.Resistance.Magic.Invulnerability := ( player.mysticism div 20 );
            if character.Resistance.Poison.Invulnerability < ( player.mysticism div 20 ) then
              character.Resistance.Poison.Invulnerability := ( player.mysticism div 20 );
            if character.Resistance.Mental.Invulnerability < ( player.mysticism div 20 ) then
              character.Resistance.Mental.Invulnerability := ( player.mysticism div 20 );

          end;

          if character.Resistance.Heat.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Heat.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Cold.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Cold.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Electric.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Electric.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Magic.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Magic.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Poison.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Poison.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Mental.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Mental.Resistance := ( player.mysticism / 200 );
        end;
      end;
    except
    end;


  // This script is activate when idle duty is set to action or converse
    S := LowerCase( Character.Properties[ 'Script' ] );
    try
      if S = '' then
        strCnvScr := ''
      else
        strCnvScr := s;

    except
      strCnvScr := '';
    end;


  // Used for tracking the player when he is deguised
    S := LowerCase( Character.Properties[ 'tmpEnemies' ] );
    try
      if S <> '' then
        Character.MakeEnemy( s );

    except
    end;

  // used to make the character only create random loot once
    if LowerCase( Character.Properties[ 'NewLoot' ] ) <> 'true' then
    begin
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
              RunScript( Character, 'additem(' + s + ')' );
          end;
          Character.Properties[ 'NewLoot' ] := 'true';
        end;
      except
      end;
    end;

  // used to fool enemies. should be set to the name of an item
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
            if Character.Properties[ 'DisguiseDetected' ] <> 'true' then
              Character.MakeNeutral( 'party' );

            for j := 0 to NPCList.count - 1 do
              TCharacter( NPCList[ j ] ).MakeNeutral( Character.Alliance );
          end;
        end;
      end;
    except
      strdisguise := '';
    end;

  // script to run when the character starts walking a path
    S := Character.Properties[ 'PathStartScr' ];
    try
      if S = '' then
        strPathStart := ''
      else
        strPathStart := s;
    except
      strPathStart := '';
    end;

  // script to run when the character completes a path
    S := Character.Properties[ 'PathEndScr' ];
    try
      if S = '' then
        strPathEnd := ''
      else
        strPathEnd := s;
    except
      strPathEnd := '';
    end;

  // guid of the first pathcorner in a path. when this pathcorner is
  // reached PathStartScr is run
    S := LowerCase( Character.Properties[ 'PathStart' ] );
    try
      if S = '' then
        sStart := ''
      else
        sStart := s;
    except
      sStart := '';
    end;

  // guid of the last pathcorner in a path. when this pathcorner is
  // reached PathEndScr is run
    S := LowerCase( Character.Properties[ 'PathEnd' ] );
    try
      if S = '' then
        sEnd := ''
      else
        sEnd := s;
    except
      sEnd := '';
    end;

  //idle action like hammer or stand
    S := LowerCase( Character.Properties[ 'Action' ] );
    try
      if S = '' then
        strAction := 'stand'
      else
        strAction := s;
    except
      strAction := 'stand';
    end;

  //things to do
    S := lowerCase( Character.Properties[ 'IdleDuty' ] );
    try
      if S = '' then
        IdleDuty := idStand
      else if S = 'meander' then
        IdleDuty := idMeander
      else if S = 'busy' then
        IdleDuty := idBusy
      else if S = 'stand' then
        IdleDuty := idStand
      else if S = 'guard' then
        IdleDuty := idGuard
      else if S = 'converse' then
        IdleDuty := idConverse
      else if S = 'action' then
        IdleDuty := idAction
      else
        IdleDuty := idMeander;

    except
      IdleDuty := idMeander;
    end;

  // how far I can wonder from my start point
    S := Character.Properties[ 'LeashLength' ];
    try

      if S = '' then
        iLeash := 0
      else
        iLeash := StrToInt( S );
    except
      iLeash := 0;
    end;

  // should I watch for enemies?
    S := LowerCase( Character.Properties[ 'Combative' ] );
    try
      if S = '' then
        bCombative := true
      else if S = 'false' then
        bCombative := False
      else
        bCombative := true;
    except
      bCombative := true;
    end;

  // can I be stopped from doing what I am doing
    S := LowerCase( Character.Properties[ 'CanStop' ] );
    try
      if S = '' then
        bCanStop := true
      else if S = 'false' then
        bCanStop := False
      else
        bCanStop := true;
    except
      bCanStop := true;
    end;

  //can I make comments about what I am doing.. never used
    S := LowerCase( Character.Properties[ 'AllowTalk' ] );
    try
      if S = '' then
        bTalk := true
      else if S = 'false' then
        bTalk := False
      else
        bTalk := true;
    except
      bTalk := true;
    end;

  // Look for IdleSFX to play
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
  //delay between SFX
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

  //where to play the sfx from see item DTSoundRing
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

  //where to play the sfx from see item DTSoundRing
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
  //where to play the sfx from see item DTSoundRing
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

  // SFX interval type
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

  // draw this character transparent
    S := LowerCase( Character.Properties[ 'Transparent' ] );
    try
      if S <> '100' then
      begin
        Character.Alpha := StrToInt( s );
        Character.SpecialEffect := seTranslucent;
      end;
    except
    end;

  // draw this character transparent
    S := LowerCase( Character.Properties[ 'UseColor' ] );
    try
      if S = 'true' then
      begin
        Effect := TColorMeEffect.create;
        Effect.ColorR := StrToInt( Character.Properties[ 'ColorRed' ] );
        Effect.ColorG := StrToInt( Character.Properties[ 'ColorGreen' ] );
        Effect.ColorB := StrToInt( Character.Properties[ 'ColorBlue' ] );
        Effect.ApplyColor := true;
        Character.AddEffect( Effect );
      end;
    except
    end;

  //randomize my money
    S := LowerCase( Character.Properties[ 'MoneyAmount' ] );
    try
      if s <> '' then
      begin
        Character.Properties[ 'MoneyAmount' ] := '-' + Character.Properties[ 'MoneyAmount' ];
        Character.Properties[ 'MoneyAmount' ] := IntToStr( Random( StrToInt( s ) ) );
      end
    except
    end;

  // replace the default stand action. exmaple would be making the dogs sit.
    S := LowerCase( Character.Properties[ 'NewStand' ] );
    try
      if s <> '' then
      begin
        if character.StandAction <> '' then
          OldStand := character.StandAction
        else
          OldStand := 'Stand';

        NewStand := strTokenAt( s, ',', 0 );
        character.StandAction := strTokenAt( s, ',', 0 );
        if strTokenAt( s, ',', 1 ) = '' then
          StandInterval := Random( 400 ) + 500
        else
          StandInterval := StrToInt( strTokenAt( s, ',', 1 ) );

      end
    except
    end;


  // indicator that the player should talk to this character next
    S := LowerCase( Character.Properties[ 'TalkToMe' ] );
    try
      if ( S <> '' ) and TalkToMe and FileExists( ArtPath + 'engine\weaponprojectiles\mageblueball.pox' ) then
      begin
//          TalkToMeTitles := s;
        TalkToMeCount := StrTokenCount( s, '|' );

        for i := 0 to TalkToMeCount - 1 do
        begin
          if player.TitleExists( StrTokenAt( StrTokenAt( s, '|', i ), ':', 0 ) ) then
            TalkToMeTitles := TalkToMeTitles + ( StrTokenAt( StrTokenAt( s, '|', i ), ':', 1 ) ) + '|';
        end;
        StrStripLast( TalkToMeTitles );
        TalkToMeCount := StrTokenCount( TalkToMeTitles, '|' );

        for i := 0 to TalkToMeCount - 1 do
        begin
          if not ( Character.TitleExists( StrTokenAt( TalkToMeTitles, '|', i ) ) ) then
            TalkToMeTitle := TalkToMeTitle + StrTokenAt( TalkToMeTitles, '|', i ) + '|';
        end;
        StrStripLast( TalkToMeTitle );
        TalkToMeCount := StrTokenCount( TalkToMeTitle, '|' );

        TalkToMeResource := LoadArtResource( 'engine\weaponprojectiles\mageblueball.gif' );
        TalkToMeResource.Alpha := 100;
        TalkToMeResource.SpecialEffect := seadd;
        TalkToMeResource.DrawShadow := false;
        Circledelay := 1;
      end;
    except
    end;



    try //find all my friends and pathcorners
      if character.GroupName <> '' then
        GroupList := GetGroup( Character, Character.GroupName );
    except
      on E : Exception do
        Log.log( 'Error HumanoidIdle Init1: ' + E.Message );
    end;

    if assigned( GroupList ) then
    begin
      if GroupList.Count <> 0 then
      try
        ContainersList := TStringList.create;
        PathCornersList := TStringList.Create;
        for i := 0 to GroupList.count - 1 do
        begin
          if ( GroupList.objects[ i ] is TContainer ) then
          begin
            j := ContainersList.Add( GroupList.Strings[ i ] );
            ContainersList.Objects[ j ] := GroupList.Objects[ i ];
          end
          else if ( GroupList.objects[ i ] is TPathCorner ) then
          begin
            j := PathCornersList.Add( GroupList.Strings[ i ] );
            PathCornersList.Objects[ j ] := GroupList.Objects[ i ];
          end;
        end;
        if ( sStart <> '' ) and ( PathCornersList.count <> 0 ) then
        begin
          PathStart := TPathCorner( PathCornersList.Objects[ PathCornersList.indexOf( sStart ) ] );
          CurrentPath := PathStart;
        end;
        if ( sEnd <> '' ) and ( PathCornersList.count <> 0 ) then
          PathEnd := TPathCorner( PathCornersList.Objects[ PathCornersList.indexOf( sEnd ) ] );
      except
        on E : Exception do
          Log.log( 'Error HumanoidIdle Init2: ' + E.Message );
      end;

    end;


  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure THumanoidIdle.Follow( Source, Target : TAniFigure );
const
  FailName : string = 'THumanoidIdle.Follow';
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
      Log.log( 'Error HumanoidIdle Follow: ' + E.Message );
  end;

end;

procedure THumanoidIdle.OnNoPath;
const
  FailName : string = 'THumanoidIdle.OnNoPath';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    Walking := False;
    if Assigned( currentPath ) then
    begin
      if TPathCorner( CurrentPath ).NextDestination = '' then
        CurrentPath := nil
      else
      begin
        try
          CurrentPath := GetGUID( TPathCorner( CurrentPath ).NextDestination );
        except
          on E : Exception do
            Log.log( 'Error HumanoidIdle NoPath1: ' + E.Message );
        end;

      end;
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidIdle NoPath2: ' + E.Message );
  end;

end;

procedure THumanoidIdle.OnStop;
const
  FailName : string = 'THumanoidIdle.OnStop';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    Walking := False;
    if not ( bHitDoor ) then
    begin
      if assigned( CurrentPath ) then
      begin
        if ( Facing <> '' ) then
        begin
          if Facing = 'SS' then
            character.Facing := FSS;
          if Facing = 'SE' then
            character.Facing := FSE;
          if Facing = 'EE' then
            character.Facing := FEE;
          if Facing = 'NE' then
            character.Facing := FNE;
          if Facing = 'NN' then
            character.Facing := FNN;
          if Facing = 'NW' then
            character.Facing := FNW;
          if Facing = 'WW' then
            character.Facing := FWW;
          if Facing = 'SW' then
            character.Facing := FSW;
          character.DoAction( 'Stand' );
          Facing := '';
        end;
        if character.RangeTo( CurrentPath.x, CurrentPath.y ) < 75 then
        begin
          try
            if Assigned( PathStart ) and ( strPathStart <> '' ) then
              if PathStart = CurrentPath then
                PlayStartScr := true;
            if Assigned( PathEnd ) and ( strPathEnd <> '' ) then
              if PathEnd = CurrentPath then
                PlayEndScr := true;

            if TPathCorner( CurrentPath ).OnArrival <> '' then
              ArrivalScr := TPathCorner( CurrentPath ).OnArrival;

            if TPathCorner( CurrentPath ).NextDestination = '' then
              CurrentPath := nil
            else
            begin
              CurrentPath := GetGUID( TPathCorner( CurrentPath ).NextDestination );
            end;

            StuckTracker := 300;
          except
            on E : Exception do
              Log.log( 'Error HumanoidIdle OnStop: ' + E.Message );
          end;
        end;
      end;
    end;

    bHitDoor := false;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure THumanoidIdle.WasAttacked( Source : TAniFigure; Damage : Single );
var
  FriendList : TStringList;
  istealth : integer;
  J : integer;
  x : longint;
  y : longint;
const
  FailName : string = 'THumanoidIdle.WasAttacked';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

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
        character.StandAction := '';
        StandInterval := -1;

        character.Track := TCharacter( Source );

        if ( TCharacter( Source ).PartyMember ) or ( TCharacter( Source ) = current ) then
        begin
          if ( strdisguise <> '' ) then
            strdisguise := '';

          Character.MakeEnemy( TCharacter( Source ).Alliance );
        end;
        TCharacter( Source ).MakeEnemy( Character.Alliance );

        character.AIMode := aiCombat;

        if CombatSay <> '' then
          Character.Say( CombatSay, clRed );

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
      Walking := True;
      Character.walkTo( X, Y, 16 );
    end;


  except
    on E : Exception do
      Log.log( 'Error HumanoidIdle WasAttacked: ' + E.Message );
  end;

  inherited;
end;

function THumanoidIdle.OnCollideFigure( Target : TAniFigure ) : boolean;
const
  FailName : string = 'THumanoidIdle.OnCollideFigure';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  Result := False;
  try
    if assigned( target ) then
      if Target is TCharacter then
      begin
        if not ( IdleDuty = idConverse ) and ( bCanStop ) and not ( Character.IsEnemy( TCharacter( Target ) ) ) and not ( Tcharacter( Target ).dead ) then
        begin
          case random( 2 ) of
            0 :
              begin
                Character.Face( Target.x, Target.y );
                Character.Stand;
                Walking := false;
                Delay := Random( 120 );
                Result := true;
              end;
          end;
        end;
        if IdleDuty = idConverse then
        begin
          ReadyToTalk := true;
          Result := true;
        end;
      end;

  except
    on E : Exception do
      Log.log( 'Error HumanoidIdle CollideFigure: ' + E.Message );
  end;
end;


procedure THumanoidIdle.WasKilled( Source : TAniFigure );
var
  FriendList : TStringList;
  iStealth : integer;
  J : integer;
  Effect : TEffect;
  s : string;
const
  FailName : string = 'THumanoidIdle.WasKilled';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  inherited;
  try
      //     Delay := 0;
      //doenst really work
    if Assigned( Source ) then
    begin
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

        if Character.Friends <> '' then
        begin
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
      end;
    end;
    S := LowerCase( Character.Properties[ 'DeathEffect' ] );
    if ( s <> '' ) then
    begin
      if s = 'pulse' then
      begin
        Effect := TPulse.Create;
        TPulse( Effect ).BlueMaster := StrToInt( Character.Properties[ 'ColorBlue' ] );
        TPulse( Effect ).GreenMaster := StrToInt( Character.Properties[ 'ColorGreen' ] );
        TPulse( Effect ).RedMaster := StrToInt( Character.Properties[ 'ColorRed' ] );
        Character.AddEffect( Effect );
        Effect := nil;
      end
      else
      begin
        Effect := TEffect.Create;
        if LowerCase( strTokenAt( s, '|', 1 ) ) = 'true' then
          Effect.DisableWhenDone := true;

        Effect.Resource := GetSpellEffect( strTokenAt( s, '|', 0 ) );
        Effect.AnimationDuration := Effect.Resource.FrameCount * Effect.Resource.FrameMultiplier;
        Effect.DoAction( 'Default', Character.FacingString );
        Character.AddEffect( Effect );
        Effect := nil;

      end;
    end;



  except
    on E : Exception do
      Log.log( 'Error HumanoidIdle WasKilled: ' + E.Message );
  end;

end;


function THumanoidIdle.OnCollideObject( Target : TAniFigure ) : Boolean;
const
  FailName : string = 'THumanoidIdle.OnCollideObject';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  result := false;
  try
    if Target is TDoor then
    begin
             //  result := true;
      bHitDoor := true;
      if ( TDoor( Target ).closed ) and ( TDoor( Target ).keyname = '' ) then
        TDoor( Target ).open;
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidIdle CollideObject: ' + E.Message );
  end;


end;

procedure THumanoidIdle.Clicked;
var
  i : integer;
const
  FailName : string = 'THumanoidIdle.Clicked';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    walking := false;
    delay := 200;
    character.stand;
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
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor THumanoidIdle.Destroy;
const
  FailName : string = 'THumanoidIdle.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if OldStand <> '' then
      character.StandAction := OldStand;

    inherited;

    if Assigned( GroupList ) then
      GroupList.free;
    if Assigned( ContainersList ) then
      ContainersList.free;
    if Assigned( PathCornersList ) then
      PathCornersList.free;
    if Assigned( TalkToMeResource ) then
      TalkToMeResource.free;
//    if assigned(Effect) then
//       Effect.free;


  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;
(**************************************************************************************)

{ THumanoidMeleeCombat }

procedure THumanoidMeleeCombat.Execute;
var
  r : Integer;
  T : single;
  X, Y : Integer;

const
  FailName : string = 'THumanoidMeleeCombat.Execute';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  inherited;
  try
  //    if (FrameCount Mod 20) = 0 then
  //     character.say(Character.Properties['ColorRed'] + ','+ Character.Properties['ColorGreen']+
  //     ','+Character.Properties['ColorBlue'], clwhite);


    if CirclePoint > 535 then
      CirclePoint := Random( 360 ) + 180;

    if Delay > 0 then
    begin
      dec( Delay );
      exit;
    end;
    if not ( Walking ) and RunForIt then
      RunAway;

    if assigned( character.track ) then
      if ( character.track = character ) or not ( character.IsEnemy( character.track ) ) then
        character.track := nil;

      //follow traget a little better
    if Walking and Assigned( Character.track ) then
    begin
      if ( frameCount mod 10 ) = 0 then
        if not ( Character.InRange( Character.Track ) ) then
        begin
          if ( character.RangeTo( ( Character.Track ).X, ( Character.Track ).Y ) > 260 ) and AllowRun then
            Character.RunTo( ( Character.Track ).X, ( Character.Track ).Y, 64 )
          else
            Character.WalkTo( ( Character.Track ).X, ( Character.Track ).Y, 64 );
        end;
    end;

    if Waiting then
    begin
      Waiting := false;
      if ( random( 2 ) = 0 ) and Assigned( Character.Track ) then
      begin
        inc( CirclePoint, 45 );
        r := 100;
        T := pi2 * CirclePoint / 360;
        X := round( r * cos( T ) ) + Character.Track.X;
        Y := round( r * sin( T ) / 2 ) + Character.Track.Y;
        Character.WalkTo( X, Y, 16 );
        Walking := True;
      end;
      CollideCount := 0;
      Delay := Random( 32 ) + 32;
      exit;
    end;

    if not ( Assigned( Character.Track ) ) then
      FindNextTarget
    else
      Attack;

    if ( FrameCount mod 15 ) = 0 then
      TellFriends;

  except
    on E : Exception do
      Log.log( 'Error HumanoidMelee Execute: ' + E.Message );
  end;


end;

procedure THumanoidMeleeCombat.TellFriends;
var
  FriendList : TStringList;
  j : integer;
begin
  if not ( Character.IsEnemy( character.track ) ) then
    exit;
  FriendList := GetPerceptibleAllies( Character, 1 );
     //ach a bad guy... tell all my friends
  if Assigned( FriendList ) then
  begin

    for j := 0 to FriendList.Count - 1 do
    begin
      if Assigned( TCharacter( FriendList.Objects[ j ] ).AI ) then
        if TCharacter( FriendList.Objects[ j ] ).AiMode <> AiCombat then
        begin
          if ( NPCList.IndexOf( TCharacter( FriendList.Objects[ j ] ) ) = -1 ) and ( TCharacter( FriendList.Objects[ j ] ).AIMode <> AIParty ) then
          begin
            if LowerCase( TCharacter( FriendList.Objects[ j ] ).IdleAI ) = 'humanoididle' then
              THumanoidIdle( TCharacter( FriendList.Objects[ j ] ).ai ).strdisguise := '';
            if LowerCase( TCharacter( FriendList.Objects[ j ] ).IdleAI ) <> 'guarddog' then
            begin
              TCharacter( FriendList.Objects[ j ] ).MakeEnemy( 'party' );
              TCharacter( FriendList.Objects[ j ] ).AiMode := AiCombat;
            end;
            TCharacter( FriendList.Objects[ j ] ).track := TCharacter( NPCList[ Random( NPCList.count ) ] );
          end;
        end;
    end;
    friendList.Free;
  end;
end;

procedure THumanoidMeleeCombat.FindNextTarget;
var
  List : TStringList;
const
  FailName : string = 'THumanoidMeleeCombat.FindNextTarget';
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

        if CombatSay <> '' then
          Character.Say( CombatSay, clRed );

        list.free;
      end
      else
        Character.AIMode := AIIdle;
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidMelee FindNextTarget: ' + E.Message );
  end;

end;

procedure THumanoidMeleeCombat.CallToArms( Source, Target : TAniFigure );
const
  FailName : string = 'THumanoidMeleeCombat.CallToArms';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if bTakeOrders then
      Character.Track := TCharacter( Target );
  except
    on E : Exception do
      Log.log( 'Error HumanoidMelee CallToArms: ' + E.Message );
  end;

end;

procedure THumanoidMeleeCombat.Attack;
const
  FailName : string = 'THumanoidMeleeCombat.Attack';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
 //  character.say('Found target', clwhite);
  try
    if ReadyToAttack then
    begin
      character.Face( Character.Track.x, Character.Track.y );
      Character.Attack( Character.Track );
      Character.Radius := OldRadius;
      ReadyToAttack := false;
      Walking := false;
    end
    else if not ( Walking ) then
    begin
      if assigned( Character.Track ) and ( character.track is TCHaracter ) then
      begin
        if TCharacter( Character.Track ).Dead or not ( TCharacter( Character.Track ).enabled ) then
        begin
          Character.Track := nil;
          Character.Radius := OldRadius;
        end
        else
        begin
          if Character.InRange( Character.Track ) then
          begin
            readyToAttack := true;
            Character.Radius := OldRadius;
            delay := ( AttackDelay - TCharacter( Character.Track ).Combat );
            if Delay < 0 then
              delay := 0;
          end
          else
          begin
            Character.WalkTo( Character.Track.X, Character.Track.Y, 64 );
             // TCharacterResource(character.Resource).speed := TCharacterResource(character.Resource).speed + 0.5;
            Walking := true;
            Character.Radius := NewRadius;
          end;
        end;
      end;
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidMelee Attack: ' + E.Message );

  end;

end;

procedure THumanoidMeleeCombat.RunAway;
var
  List : TStringList;
  ramPick : integer;
const
  FailName : string = 'THumanoidMeleeCombat.RunAway';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    Walking := True;
        //Better runAway code
    if assigned( Character.track ) then
    begin
      Character.Face( Character.Track.X, Character.track.Y );
    end; //From Mike
    case Character.Facing of
      fNE, fEE, fSE :
        begin
          if AllowRun then
            Character.RunTo( Character.X - 150, Character.Y + random( 300 ) - 150, 64 )
          else
            Character.WalkTo( Character.X - 150, Character.Y + random( 300 ) - 150, 64 );
        end;
      fNW, fWW, fSW :
        begin
          if AllowRun then
            Character.RunTo( Character.X + 150, Character.Y + random( 300 ) - 150, 64 )
          else
            Character.WalkTo( Character.X + 150, Character.Y + random( 300 ) - 150, 64 );
        end;
      fSS :
        begin
          if AllowRun then
            Character.RunTo( Character.X + random( 300 ) - 150, Character.Y - 150, 64 )
          else
            Character.WalkTo( Character.X + random( 300 ) - 150, Character.Y - 150, 64 );
        end;

      fNN :
        begin
          if AllowRun then
            Character.RunTo( Character.X + random( 300 ) - 150, Character.Y + 150, 64 )
          else
            Character.WalkTo( Character.X + random( 300 ) - 150, Character.Y + 150, 64 )
        end;
    end;
        {if Pos('E',character.FacingString)<>0 then
           Character.WalkTo(Character.X - 150, Character.Y+ random(300)- 150, 128)
        else
        if Pos('W',character.FacingString)<>0 then
           Character.WalkTo(Character.X + 150, Character.Y + random(300)- 150, 128)
        else
        if Pos('SS',character.FacingString)<>0 then
           Character.WalkTo(Character.X + random(300) - 150, Character.Y - 150, 128)
        else
            Character.WalkTo(Character.X + random(300) - 150, Character.Y + 150, 128);}
    ReadyToAttack := false;
    Character.Track := nil;
        //Character.AIMode := AIIdle;
    Walking := false;
    Delay := random( 120 );
        //RunForIt:= False;
        //Keep running?
    if ( Character.Wounds / Character.HitPoints ) * 100 >= iTimeToRun then
    begin
      case FBaseCourage of
        0 :
          if ( Random( 99 ) < 80 ) then
            RunForIt := True;
        1 :
          if ( Random( 99 ) < 70 ) then
            RunForIt := True;
        2 :
          if ( Random( 99 ) < 60 ) then
            RunForIt := True;
        3 :
          if ( Random( 99 ) < 50 ) then
            RunForIt := True;
        4 :
          if ( Random( 99 ) < 40 ) then
            RunForIt := True;
        5 :
          if ( Random( 99 ) < 30 ) then
            RunForIt := True;
        6 :
          if ( Random( 99 ) < 20 ) then
            RunForIt := True;
        7 :
          if ( Random( 99 ) < 10 ) then
            RunForIt := True;
      end;
    end;
  except on E : Exception do
      Log.log( 'Error HumanoidMelee RunAway: ' + E.Message );
  end;
  try
        //while running away if I see a friend tell him to help me
    List := GetPerceptibleAllies( Character, 1 );
    if Assigned( List ) then
    begin

      if List.Count = 1 then
      begin
        if Assigned( TCharacter( List.Objects[ 0 ] ).AI ) then
          TAI( TCharacter( List.Objects[ 0 ] ).AI ).CallToArms( Character, Character.Track )
      end
      else
      begin
               //ramPick := random(List.count);
        for rampick := 0 to List.Count - 1 do
        begin
          if Assigned( TCharacter( List.Objects[ RamPick ] ).AI ) then
            TAI( TCharacter( List.Objects[ RamPick ] ).AI ).CallToArms( Character, Character.Track );
        end;
      end;
      List.Free;
    end;

  except
    on E : Exception do
      Log.log( 'Error HumanoidMelee RunAway: ' + E.Message );

  end;

end;


procedure THumanoidMeleeCombat.Regroup( Source : TAniFigure; NewX, NewY : Integer );
const
  FailName : string = 'THumanoidMeleeCombat.Regroup';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    if bTakeOrders then
    begin
      Walking := true;
      Character.WalkTo( NewX, NewY, 64 );
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidMelee Regroup: ' + E.Message );
  end;

end;

procedure THumanoidMeleeCombat.Init;
var
  S : string;
  list : TstringList;
  i : integer;
const
  FailName : string = 'THumanoidMeleeCombat.Init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    OldRadius := Character.Radius;
    NewRadius := Character.Radius + 2;


    CollideCount := 0;
    CirclePoint := Random( 360 ) + 180;
    S := Character.Properties[ 'TimeToRun' ];
    try
      if S = '' then
        iTimeToRun := 75
      else
        iTimeToRun := StrToInt( S );
    except
      iTimeToRun := 75;
    end;

    S := Character.Properties[ 'Say' ];
    try
      if S = '' then
        CombatSay := ''
      else
        CombatSay := S;
    except
      CombatSay := '';
    end;

    S := LowerCase( Character.Properties[ 'AllowRun' ] );
    try
      if S = '' then
        AllowRun := true
      else if S = 'false' then
        AllowRun := False
      else
        AllowRun := true;
    except
      AllowRun := true;
    end;

    S := Character.Properties[ 'iSpeed' ];
    try
      if ( S <> '' ) and ( s <> '0' ) then
        TCharacterResource( character.Resource ).Speed := StrToInt( S );
    except
    end;

    S := Character.Properties[ 'BaseCourage' ];
    try
      if S = '' then
        FBaseCourage := 5
      else
        FBaseCourage := StrToInt( S );
    except
      FBaseCourage := 5;
    end;

    S := Character.Properties[ 'BonusCourage' ];
    try
      if S = '' then
        FBonusCourage := 0
      else
        FBonusCourage := StrToInt( S );
    except
      FBonusCourage := 0;
    end;

    S := LowerCase( Character.Properties[ 'TakeOrders' ] );
    try
      if S = '' then
        bTakeOrders := true
      else if S = 'false' then
        bTakeOrders := False
      else
        bTakeOrders := true;
    except
      bTakeOrders := true;
    end;

    S := Character.Properties[ 'AttackDelay' ];
    try
      if ( S = '' ) or ( s = '0' ) then
        AttackDelay := 30
      else
        AttackDelay := StrToInt( S );
    except
      AttackDelay := 30;
    end;

    S := LowerCase( Character.Properties[ 'BalanceWithPlayer' ] );
    try
      if ( S <> '' ) and ( s <> '0' ) then
      begin
        i := StrToInt( s );
        if i >= 0 then
        begin
          if player.TitleExists( 'Apprentice' ) then
          begin
            character.Wounds := 0;
            Character.Drain := 0;
            character.Combat := ( ( ( player.Mysticism * 3 ) div 4 ) + i );
            character.strength := ( ( player.perception * 3 ) div 4 ) + i;
            if not ( Character.TitleExists( 'NoHP' ) ) then
              if character.BaseHitPoints > 0 then
                character.HitPoints := ( ( player.Perception * 2 ) * NPCList.Count );

            character.Coordination := ( ( player.Coordination * 2 ) div 3 ) + i;
            Character.AttackRecovery := Character.AttackRecovery + ( player.attackRecovery div i );

            if character.Resistance.Heat.Invulnerability < ( player.mysticism div 10 ) then
              character.Resistance.Heat.Invulnerability := ( player.mysticism div 10 );
            if character.Resistance.Cold.Invulnerability < ( player.mysticism div 10 ) then
              character.Resistance.Cold.Invulnerability := ( player.mysticism div 10 );
            if character.Resistance.Electric.Invulnerability < ( player.mysticism div 10 ) then
              character.Resistance.Electric.Invulnerability := ( player.mysticism div 10 );
            if character.Resistance.Magic.Invulnerability < ( player.mysticism div 10 ) then
              character.Resistance.Magic.Invulnerability := ( player.mysticism div 10 );
            if character.Resistance.Poison.Invulnerability < ( player.mysticism div 10 ) then
              character.Resistance.Poison.Invulnerability := ( player.mysticism div 10 );
            if character.Resistance.Mental.Invulnerability < ( player.mysticism div 10 ) then
              character.Resistance.Mental.Invulnerability := ( player.mysticism div 10 );

            if character.Resistance.Heat.Resistance < ( player.mysticism / 200 ) then
              character.Resistance.Heat.Resistance := ( player.mysticism / 200 );
            if character.Resistance.Cold.Resistance < ( player.mysticism / 200 ) then
              character.Resistance.Cold.Resistance := ( player.mysticism / 200 );
            if character.Resistance.Electric.Resistance < ( player.mysticism / 200 ) then
              character.Resistance.Electric.Resistance := ( player.mysticism / 200 );
            if character.Resistance.Magic.Resistance < ( player.mysticism / 200 ) then
              character.Resistance.Magic.Resistance := ( player.mysticism / 200 );
            if character.Resistance.Poison.Resistance < ( player.mysticism / 200 ) then
              character.Resistance.Poison.Resistance := ( player.mysticism / 200 );
            if character.Resistance.Mental.Resistance < ( player.mysticism / 200 ) then
              character.Resistance.Mental.Resistance := ( player.mysticism / 200 );
          end;
          if player.TitleExists( 'Hunter' ) then
          begin
            character.Wounds := 0;
            Character.Drain := 0;
            character.Combat := ( ( ( player.Stealth * 3 ) div 4 ) + i );
            character.strength := ( ( player.Coordination * 3 ) div 4 ) + i;
            if not ( Character.TitleExists( 'NoHP' ) ) then
              if character.BaseHitPoints > 0 then
                character.HitPoints := ( ( player.Coordination * 2 ) * NPCList.Count );
            character.Coordination := ( ( player.Coordination * 3 ) div 4 ) + i;
            Character.AttackRecovery := Character.AttackRecovery + ( player.attackRecovery div i );

            if character.Resistance.Heat.Invulnerability < ( player.stealth div 20 ) then
              character.Resistance.Heat.Invulnerability := ( player.stealth div 20 );
            if character.Resistance.Cold.Invulnerability < ( player.stealth div 20 ) then
              character.Resistance.Cold.Invulnerability := ( player.stealth div 20 );
            if character.Resistance.Electric.Invulnerability < ( player.stealth div 20 ) then
              character.Resistance.Electric.Invulnerability := ( player.stealth div 20 );
            if character.Resistance.Magic.Invulnerability < ( player.stealth div 20 ) then
              character.Resistance.Magic.Invulnerability := ( player.stealth div 20 );

            if character.Resistance.Heat.Resistance < ( player.mysticism / 200 ) then
              character.Resistance.Heat.Resistance := ( player.mysticism / 200 );
            if character.Resistance.Cold.Resistance < ( player.mysticism / 200 ) then
              character.Resistance.Cold.Resistance := ( player.mysticism / 200 );
            if character.Resistance.Electric.Resistance < ( player.mysticism / 200 ) then
              character.Resistance.Electric.Resistance := ( player.mysticism / 200 );
            if character.Resistance.Magic.Resistance < ( player.mysticism / 200 ) then
              character.Resistance.Magic.Resistance := ( player.mysticism / 200 );
          end;

          if player.TitleExists( 'Squire' ) then
          begin
            character.Wounds := 0;
            Character.Drain := 0;
            character.Combat := ( ( player.Combat * 3 ) div 4 ) + i;
            character.strength := ( ( player.Strength * 3 ) div 4 ) + i;
            character.Coordination := ( ( player.Coordination * 2 ) div 3 ) + i;
            Character.AttackRecovery := Character.AttackRecovery + ( player.attackRecovery div i );
            if not ( Character.TitleExists( 'NoHP' ) ) then
              if character.BaseHitPoints > 0 then
                character.HitPoints := ( ( player.strength * 2 ) * NPCList.Count );

            if character.Resistance.Heat.Invulnerability < ( player.combat div 20 ) then
              character.Resistance.Heat.Invulnerability := ( player.combat div 20 );
            if character.Resistance.Cold.Invulnerability < ( player.combat div 20 ) then
              character.Resistance.Cold.Invulnerability := ( player.combat div 20 );
            if character.Resistance.Electric.Invulnerability < ( player.combat div 20 ) then
              character.Resistance.Electric.Invulnerability := ( player.combat div 20 );
            if character.Resistance.Magic.Invulnerability < ( player.combat div 20 ) then
              character.Resistance.Magic.Invulnerability := ( player.combat div 20 );

            if character.Resistance.Heat.Resistance < ( player.mysticism / 200 ) then
              character.Resistance.Heat.Resistance := ( player.mysticism / 200 );
            if character.Resistance.Cold.Resistance < ( player.mysticism / 200 ) then
              character.Resistance.Cold.Resistance := ( player.mysticism / 200 );
            if character.Resistance.Electric.Resistance < ( player.mysticism / 200 ) then
              character.Resistance.Electric.Resistance := ( player.mysticism / 200 );
            if character.Resistance.Magic.Resistance < ( player.mysticism / 200 ) then
              character.Resistance.Magic.Resistance := ( player.mysticism / 200 );
          end;

        end;
      end;
    except
    end;

    S := Character.Properties[ 'DamageShield' ];
    try
      if S = '' then
        iDamageShield := 0
      else
        iDamageShield := StrToInt( S );
    except
      iDamageShield := 0;
    end;

    S := LowerCase( Character.Properties[ 'SpellEffect' ] );
    if ( s <> '' ) then
      FSpellEffectResource := GetSpellEffect( s );

    PartyTotal := 1;
    List := GetPerceptibleAllies( Character, 1.5 );
    if Assigned( List ) then
    begin
      PartyTotal := list.Count;
      list.free;
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidMelee Init: ' + E.Message );
  end;

  Delay := random( 40 );
end;


function THumanoidMeleeCombat.OnCollideFigure( Target : TAniFigure ) : boolean;
const
  FailName : string = 'THumanoidMeleeCombat.OnCollideFigure';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  Result := False;
  Character.Radius := OldRadius;

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
            collideCount := 0;
            Character.doaction( 'stand' );
            Waiting := true;
            result := true;
            delay := random( 15 );
          end;
        end;
      end;
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidMelee CollideFigure: ' + E.Message );
  end;

end;

procedure THumanoidMeleeCombat.OnNoPath;
const
  FailName : string = 'THumanoidMeleeCombat.OnNoPath';
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
      Log.log( 'Error HumanoidMelee NoPath: ' + E.Message );
  end;


end;

procedure THumanoidMeleeCombat.OnStop;
begin
  try
    Walking := false;
   // TCharacterResource(character.Resource).speed := 5;
    if Assigned( Character.Track ) then
      Character.Face( Character.Track.X, Character.track.Y );
  except
    on E : Exception do
      Log.log( 'Error HumanoidMelee Stop: ' + E.Message );
  end;


end;

procedure THumanoidMeleeCombat.WasAttacked( Source : TAniFigure; Damage : single );
var
  x : longint;
  y : longint;
  Effect : TEffect;

const
  FailName : string = 'THumanoidMeleeCombat.WasAttacked';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if Source is TCharacter then
      character.Face( TCharacter( Source ).x, TCharacter( Source ).y )
    else
    begin
      X := random( 100 ) + 50;
      Y := random( 100 ) + 50;
      Walking := True;
      Character.walkTo( X, Y, 128 );
          // Character.walkTo(X, Y, 16);
    end;

    try
      if Source is TCharacter then
      begin
        if Character.IsEnemy( TCharacter( Source ) ) then
        begin
          Character.Radius := OldRadius;
          if ( iDamageShield > 0 ) and ( character.RangeTo( TCharacter( Source ).x, TCharacter( Source ).y ) < 50 ) then
          begin
            if Assigned( FSpellEffectResource ) then
            begin
              Effect := TEffect.Create;
              Effect.Resource := FSpellEffectResource;
              Effect.AnimationDuration := 8 * FSpellEffectResource.FrameMultiplier;
              Effect.DoAction( 'Default', TCharacter( Source ).FacingString );
              Effect.ColorR := 250;
              Effect.ColorG := 250;
              Effect.ColorB := 250;
              Effect.ApplyColor := true;
              TCharacter( Source ).AddEffect( Effect );
            end;
            TCharacter( Source ).TakeDamage( Character, ( ( iDamageShield / 100 ) * Damage ), 0, False );
            Character.TakeDamage( Character, -( ( iDamageShield / 100 ) * Damage ), 0, False );
          end;

          if ( Character.Wounds / Character.HitPoints ) * 100 >= iTimeToRun then
          begin
            case FBaseCourage of
              0 :
                if ( Random( 9 ) < 8 ) then
                  RunForIt := True;
              1 :
                if ( Random( 9 ) < 7 ) then
                  RunForIt := True;
              2 :
                if ( Random( 9 ) < 6 ) then
                  RunForIt := True;
              3 :
                if ( Random( 9 ) < 5 ) then
                  RunForIt := True;
              4 :
                if ( Random( 9 ) < 4 ) then
                  RunForIt := True;
              5 :
                if ( Random( 9 ) < 3 ) then
                  RunForIt := True;
              6 :
                if ( Random( 9 ) < 2 ) then
                  RunForIt := True;
              7 :
                if ( Random( 9 ) < 1 ) then
                  RunForIt := True;
            end;
            Character.Track := TCharacter( Source );
          end
          else
          begin
            Character.Track := TCharacter( Source );
          end;
        end;
      end;
    except
      on E : Exception do
        Log.log( 'Error HumanoidMelee WasAttacked: ' + E.Message );
    end;

    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure THumanoidMeleeCombat.WasKilled( Source : TAniFigure );
var
  List : TStringList;
  iLoop : integer;
  s : string;
  Effect : TEffect;
const
  FailName : string = 'THumanoidMeleeCombat.WasKilled';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    if Source is TCharacter then
      character.Face( TCharacter( Source ).x, TCharacter( Source ).y );

    try
         //Tell everyone I died so they can save themselves
      List := GetPerceptibleAllies( Character, 1.5 );
      if Assigned( List ) then
      begin
        for iLoop := 0 to List.count - 1 do
        begin
          if Assigned( TCharacter( List.Objects[ iLoop ] ).AI ) then
            TAI( TCharacter( List.Objects[ iLoop ] ).AI ).NotifyOfDeath( character );
        end;
        list.free;
      end;
    except
      on E : Exception do
        Log.log( 'Error HumanoidMelee WasKilled: ' + E.Message );
    end;

    S := LowerCase( Character.Properties[ 'DeathEffect' ] );
    if ( s <> '' ) then
    begin
      if s = 'pulse' then
      begin
        Effect := TPulse.Create;
        TPulse( Effect ).BlueMaster := StrToInt( Character.Properties[ 'ColorBlue' ] );
        TPulse( Effect ).GreenMaster := StrToInt( Character.Properties[ 'ColorGreen' ] );
        TPulse( Effect ).RedMaster := StrToInt( Character.Properties[ 'ColorRed' ] );
        Character.AddEffect( Effect );
        Effect := nil;
      end
      else
      begin
        Effect := TEffect.Create;
        if LowerCase( strTokenAt( s, '|', 1 ) ) = 'true' then
          Effect.DisableWhenDone := true;

        Effect.Resource := GetSpellEffect( strTokenAt( s, '|', 0 ) );
        Effect.AnimationDuration := Effect.Resource.FrameCount * Effect.Resource.FrameMultiplier;
        Effect.DoAction( 'Default', Character.FacingString );
        Character.AddEffect( Effect );
        Effect := nil;

      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure THumanoidMeleeCombat.NotifyOfDeath( Source : TAniFigure );
var
  tmpPartyTotal : integer;
  list : TStringList;
const
  FailName : string = 'THumanoidMeleeCombat.NotifyOfDeath';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    Waiting := false;
    tmpPartyTotal := 0;
    List := GetPerceptibleAllies( Character, 1.5 );
    if Assigned( List ) then
    begin
      tmpPartyTotal := list.Count;
      list.free;
    end;

    if Assigned( THumanoidCombat( TCharacter( Source ).AI ) ) then
      case FBaseCourage of
        0 :
          if ( ( ( PartyTotal div 2 ) >= tmpPartyTotal ) ) and ( Random( 99 ) < 80 ) then
            RunAway;
        1 :
          if ( ( ( PartyTotal div 2 ) >= tmpPartyTotal ) or ( THumanoidCombat( TCharacter( Source ).AI ).BaseCourage > 1 ) ) and ( Random( 99 ) < 70 ) then
            RunAway;
        2 :
          if ( ( ( PartyTotal div 2 ) > tmpPartyTotal ) or ( THumanoidCombat( TCharacter( Source ).AI ).BaseCourage > 5 ) ) and ( Random( 99 ) < 60 ) then
            RunAway;
        3 :
          if ( ( ( PartyTotal div 2 ) > tmpPartyTotal ) or ( THumanoidCombat( TCharacter( Source ).AI ).BaseCourage > 5 ) ) and ( Random( 99 ) < 50 ) then
            RunAway;
        4 :
          if ( ( ( PartyTotal div 2 ) > tmpPartyTotal ) or ( THumanoidCombat( TCharacter( Source ).AI ).BaseCourage > 6 ) ) and ( Random( 99 ) < 40 ) then
            RunAway;
        5 :
          if ( ( ( PartyTotal div 2 ) > tmpPartyTotal ) or ( THumanoidCombat( TCharacter( Source ).AI ).BaseCourage > 7 ) ) and ( Random( 99 ) < 30 ) then
            RunAway;
        6 :
          if ( ( ( PartyTotal div 2 ) > tmpPartyTotal ) or ( THumanoidCombat( TCharacter( Source ).AI ).BaseCourage > 8 ) ) and ( Random( 99 ) < 20 ) then
            RunAway;
        7 :
          if ( tmpPartyTotal = 0 ) and ( Random( 99 ) < 10 ) then
            RunAway;
        8 :
          if ( tmpPartyTotal = 0 ) and ( character.wounds >= ( Character.HitPoints * 0.75 ) ) then
            RunAway;
      end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidMelee NotifyOfDeath: ' + E.Message );
  end;


end;

destructor THumanoidMeleeCombat.Destroy;
const
  FailName : string = 'THumanoidMeleeCombat.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;

    Character.Radius := OldRadius;


  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function THumanoidMeleeCombat.OnCollideObject( Target : TAniFigure ) : Boolean;
const
  FailName : string = 'THumanoidMeleeCombat.OnCollideObject';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  result := false;
  try
    if Target is TDoor then
    begin
      if ( TDoor( Target ).closed ) and ( TDoor( Target ).keyname = '' ) then
        TDoor( Target ).open;
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidMeleeCombat CollideObject: ' + E.Message );
  end;
end;
(**********************************************************************************)

{ THumanoidArcherCombat }


procedure THumanoidArcherCombat.CallToArms( Source, Target : TAniFigure );
const
  FailName : string = 'THumanoidArcherCombat.CallToArms';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    if bTakeOrders then
    begin
      if Assigned( TCharacter( Target ) ) then
        if Character.IsEnemy( TCharacter( Target ) ) then
          Character.Track := TCharacter( Target );
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidArcher CallToArms: ' + E.Message );
  end;

end;

procedure THumanoidArcherCombat.Execute;
const
  FailName : string = 'THumanoidArcherCombat.Execute';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    try
      if ( ( FrameCount mod 160 ) = 0 ) and bMove then
        RunOrFight := true;

      if assigned( character.track ) then
        if ( character.track = character ) or character.IsAlly( character.track ) then
          character.track := nil;

      if CirclePoint > 535 then
        CirclePoint := Random( 360 ) + 180;


      if ( Delay > 0 ) and not ( Walking ) then
      begin
        dec( Delay );
        exit;
      end;

      if not ( walking ) and RunAway then
        MoveAway;

 { if Not(RunOrFight) and Assigned(Character.Track)and Not(walking) then
  begin
    if RangeTest(Character.Track, Character, iDistance) then
    begin
        MoveAway;
        RunOrFight := true;
        exit;
    end
  end;}

      if Assigned( Character.Track ) and not ( walking ) then
      begin
        if ( Character.Track = character ) then
          Character.Track := nil
        else
          Attack;
      end;


      if not Assigned( Character.Track ) then
        FindTarget;


    except
      on E : Exception do
        Log.log( 'Error HumanoidArcher Execute: ' + E.Message );

    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure THumanoidArcherCombat.BattleTactic;
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
    if not Walking then
    begin
      Walking := True;
      ShotCounter := 0;
      inc( CirclePoint, 45 );
      r := iDistance;
      T := pi2 * CirclePoint / 360;
      X := round( r * cos( T ) ) + TCharacter( Character.Track ).X;
      Y := round( r * sin( T ) / 2 ) + TCharacter( Character.Track ).Y;

      Character.WalkTo( X, Y, 48 );
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidArcher BattleTactic: ' + E.Message );

  end;

end;


procedure THumanoidArcherCombat.MoveAway;
const
  FailName : string = 'THumanoidArcherCombat.MoveAway';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    Walking := True;
    if assigned( Character.track ) then
      Character.Face( Character.Track.X, Character.track.Y );
    case Character.Facing of
      fNE, fEE, fSE :
        begin
          if AllowRun then
            Character.RunTo( Character.X - 100, Character.Y + random( 200 ) - 100, 64 )
          else
            Character.WalkTo( Character.X - 100, Character.Y + random( 200 ) - 100, 64 );
        end;
      fNW, fWW, fSW :
        begin
          if AllowRun then
            Character.RunTo( Character.X + 100, Character.Y + random( 200 ) - 100, 64 )
          else
            Character.WalkTo( Character.X + 100, Character.Y + random( 200 ) - 100, 64 );
        end;
      fSS :
        begin
          if AllowRun then
            Character.RunTo( Character.X + random( 200 ) - 100, Character.Y - 100, 64 )
          else
            Character.WalkTo( Character.X + random( 200 ) - 100, Character.Y - 100, 64 );
        end;

      fNN :
        begin
          if AllowRun then
            Character.RunTo( Character.X + random( 200 ) - 100, Character.Y + 100, 64 )
          else
            Character.WalkTo( Character.X + random( 200 ) - 100, Character.Y + 100, 64 )
        end;
    end;
    RunAway := False;

     //Keep running??
    if ( Character.Wounds / Character.HitPoints ) * 100 >= iTimeToRun then
    begin
      case FBaseCourage of
        0 :
          if ( Random( 99 ) < 80 ) then
            RunAway := true;
        1 :
          if ( Random( 99 ) < 70 ) then
            RunAway := true;
        2 :
          if ( Random( 99 ) < 60 ) then
            RunAway := true;
        3 :
          if ( Random( 99 ) < 50 ) then
            RunAway := true;
        4 :
          if ( Random( 99 ) < 40 ) then
            RunAway := true;
        5 :
          if ( Random( 99 ) < 30 ) then
            RunAway := true;
        6 :
          if ( Random( 99 ) < 20 ) then
            RunAway := true;
        7 :
          if ( Random( 99 ) < 10 ) then
            RunAway := true;
      end;
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidArcher MoveAway: ' + E.Message );

  end;

end;

procedure THumanoidArcherCombat.Attack;
const
  FailName : string = 'THumanoidArcherCombat.Attack';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if TCharacter( Character.Track ).Dead or not ( TCharacter( Character.Track ).enabled ) then
      Character.Track := nil
    else
    begin
      if ( Character.RangeTo( Character.Track.X, Character.Track.Y ) > 51 ) then
      begin
        if ShotCounter < MaxShots then
        begin
          if bMove then
            Inc( ShotCounter );
          Character.Face( Character.Track.x, Character.Track.y );
                  //if Character.Inrange(Character.Track) then
          if InRange( Character.Track ) then
          begin
            Character.Attack( Character.Track );
            delay := ( AttackDelay - TCharacter( Character.Track ).Combat ) + 15;
            if Delay < 0 then
              delay := 15;
          end
          else
          begin
                    // Find Better position to shoot
            if AllowRun then
              Character.RunTo( Character.Track.X, Character.Track.Y, 64 )
            else
              Character.WalkTo( Character.Track.X, Character.Track.Y, 64 );
          end;
        end
        else
          BattleTactic;
      end
      else
      begin
        if random( 2 ) = 1 then
        begin
          if bMove and RunOrFight then
            BattleTactic
                 // RunAway := true
          else
                {//if Character.Inrange(Character.Track) then} if InRange( Character.Track ) then
            begin
              Character.Attack( Character.Track );
              delay := ( AttackDelay - TCharacter( Character.Track ).Combat ) + 15;
              if Delay < 0 then
                delay := 15;
            end;
        end;
      end;
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidArcher Attack: ' + E.Message );

  end;

end;

procedure THumanoidArcherCombat.FindTarget;
var
  list : TStringList;
const
  FailName : string = 'THumanoidArcherCombat.FindTarget';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    List := GetPerceptibleEnemies( Character, 2 );
    if assigned( List ) then
    begin
      if List.Count = 1 then
        Character.Track := TCharacter( List.objects[ 0 ] )
      else
        Character.Track := TCharacter( List.objects[ random( List.count ) ] );
      list.free;
    end
    else
      character.AIMode := aiIdle;
  except
    on E : Exception do
      Log.log( 'Error HumanoidArcher FindTarget: ' + E.Message );

  end;

end;


procedure THumanoidArcherCombat.Init;
var
  S : string;
  i : integer;
const
  FailName : string = 'THumanoidArcherCombat.Init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
     //   RunOrFight := true;
    CirclePoint := Random( 360 ) + 180;
    ShotCounter := 0;
    MaxShots := Random( 3 ) + 1;
    runaway := false;
    S := Character.Properties[ 'iSpeed' ];
    try
      if ( S <> '' ) and ( s <> '0' ) then
        TCharacterResource( character.Resource ).Speed := StrToInt( S );
    except
    end;


    S := Character.Properties[ 'TimeToRun' ];
    try
      if S = '' then
        iTimeToRun := 75
      else
        iTimeToRun := StrToInt( S );
    except
      iTimeToRun := 75;
    end;

    S := LowerCase( Character.Properties[ 'BalanceWithPlayer' ] );
    try
      if ( S <> '' ) and ( s <> '0' ) then
      begin
        i := StrToInt( s );
        if i >= 0 then
        begin
          if player.TitleExists( 'Apprentice' ) then
          begin
            character.Wounds := 0;
            Character.Drain := 0;
            character.Combat := ( ( ( player.mysticism * 3 ) div 4 ) + i );
            character.strength := ( ( player.perception * 6 ) div 10 ) + i;
            character.Coordination := ( ( player.perception * 2 ) div 3 ) + i;
            character.Stealth := player.Mysticism + i;
            if not ( Character.TitleExists( 'NoHP' ) ) then
              if character.BaseHitPoints > 0 then
                character.HitPoints := ( ( player.Perception * 2 ) * NPCList.Count );
          end;
          if player.TitleExists( 'Hunter' ) then
          begin
            character.Wounds := 0;
            Character.Drain := 0;
            character.Combat := player.combat + i;
            character.strength := ( ( player.Strength * 6 ) div 10 ) + i;
            character.Coordination := ( ( player.Coordination * 2 ) div 3 ) + i;
            character.Stealth := player.Stealth + i;
            if not ( Character.TitleExists( 'NoHP' ) ) then
              if character.BaseHitPoints > 0 then
                character.HitPoints := ( ( player.Coordination * 2 ) * NPCList.Count );
          end;
          if player.TitleExists( 'Squire' ) then
          begin
            character.Wounds := 0;
            Character.Drain := 0;
            character.Combat := ( ( ( player.combat * 3 ) div 4 ) + i );
            character.strength := ( ( player.Coordination * 6 ) div 10 ) + i;
            character.Coordination := ( ( player.strength * 2 ) div 3 ) + i;
            character.Stealth := player.Combat + i;
            if not ( Character.TitleExists( 'NoHP' ) ) then
              if character.BaseHitPoints > 0 then
                character.HitPoints := ( ( player.strength * 2 ) * NPCList.Count );
          end;


          if character.Resistance.Heat.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Heat.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Cold.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Cold.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Electric.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Electric.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Magic.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Magic.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Poison.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Poison.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Mental.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Mental.Resistance := ( player.mysticism / 200 );

        end;
      end;
    except
    end;

    S := Character.Properties[ 'AttackDelay' ];
    try
      if ( S = '' ) or ( s = '0' ) then
        AttackDelay := 25
      else
        AttackDelay := StrToInt( S );
    except
      AttackDelay := 25;
    end;

    S := Character.Properties[ 'BaseCourage' ];
    try
      if S = '' then
        FBaseCourage := 1
      else
        FBaseCourage := StrToInt( S );
    except
      FBaseCourage := 1;
    end;


    S := LowerCase( Character.Properties[ 'TakeOrders' ] );
    try
      if S = '' then
        bTakeOrders := true
      else if S = 'false' then
        bTakeOrders := False
      else
        bTakeOrders := true;
    except
      bTakeOrders := true;
    end;

    S := LowerCase( Character.Properties[ 'Moveable' ] );
    try
      if S = '' then
        bMove := true
      else if S = 'false' then
        bMove := False
      else
        bMove := true;
    except
      bMove := true;
    end;
            //bMove :=false;
    S := LowerCase( Character.Properties[ 'AllowRun' ] );
    try
      if S = '' then
        AllowRun := true
      else if S = 'false' then
        AllowRun := False
      else
        AllowRun := true;
    except
      AllowRun := true;
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

    PartyTotal := 1;
    if character.GroupName <> '' then
      FriendsList := GetGroup( Character, Character.GroupName );
    if not Assigned( FriendsList ) then
    begin
      FriendsList := GetPerceptibleAllies( Character, 1 );
      if assigned( FriendsList ) then
      begin
        PartyTotal := Friendslist.Count;
        FriendsList.Free;
        FriendsList := nil;
      end;
    end;

    if Assigned( FriendsList ) then
    begin
      PartyTotal := Friendslist.Count;
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidArcher Init: ' + E.Message );

  end;

  Delay := random( 60 );

end;

procedure THumanoidArcherCombat.NotifyOfDeath( Source : TAniFigure );
var
  tmpPartyTotal : integer;
  list : TStringList;
const
  FailName : string = 'THumanoidArcherCombat.NotifyOfDeath';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    tmpPartyTotal := 0;
    List := GetPerceptibleAllies( Character, 1.5 );
    if Assigned( List ) then
    begin
      tmpPartyTotal := list.Count;
      list.free;
    end;
    if Assigned( THumanoidCombat( TCharacter( Source ).AI ) ) then
      case FBaseCourage of
        0 :
          if ( ( ( PartyTotal div 2 ) >= tmpPartyTotal ) ) and ( Random( 99 ) < 80 ) then
            MoveAway;
        1 :
          if ( ( ( PartyTotal div 2 ) >= tmpPartyTotal ) or ( THumanoidCombat( TCharacter( Source ).AI ).BaseCourage > 1 ) ) and ( Random( 99 ) < 70 ) then
            MoveAway;
        2 :
          if ( ( ( PartyTotal div 2 ) > tmpPartyTotal ) or ( THumanoidCombat( TCharacter( Source ).AI ).BaseCourage > 5 ) ) and ( Random( 99 ) < 60 ) then
            MoveAway;
        3 :
          if ( ( ( PartyTotal div 2 ) > tmpPartyTotal ) or ( THumanoidCombat( TCharacter( Source ).AI ).BaseCourage > 5 ) ) and ( Random( 99 ) < 50 ) then
            MoveAway;
        4 :
          if ( ( ( PartyTotal div 2 ) > tmpPartyTotal ) or ( THumanoidCombat( TCharacter( Source ).AI ).BaseCourage > 6 ) ) and ( Random( 99 ) < 40 ) then
            MoveAway;
        5 :
          if ( ( ( PartyTotal div 2 ) > tmpPartyTotal ) or ( THumanoidCombat( TCharacter( Source ).AI ).BaseCourage > 7 ) ) and ( Random( 99 ) < 30 ) then
            MoveAway;
        6 :
          if ( ( ( PartyTotal div 2 ) > tmpPartyTotal ) or ( THumanoidCombat( TCharacter( Source ).AI ).BaseCourage > 8 ) ) and ( Random( 99 ) < 20 ) then
            MoveAway;
        7 :
          if ( tmpPartyTotal = 0 ) and ( Random( 99 ) < 10 ) then
            MoveAway;
        8 :
          if ( tmpPartyTotal = 0 ) and ( character.wounds >= ( Character.HitPoints * 0.75 ) ) then
            MoveAway;
      end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidArcher NotifyOfDeath: ' + E.Message );

  end;

end;

function THumanoidArcherCombat.OnCollideFigure(
  Target : TAniFigure ) : boolean;
const
  FailName : string = 'THumanoidArcherCombat.OnCollideFigure';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  Result := False;
  try
    if Target = Character.Track then
    begin
      RunAway := True;
      Result := True;
    end
    else
    begin
      if Target is TCharacter then
      begin
        if Character.IsEnemy( TCharacter( Target ) ) then
        begin
          Character.Track := TCharacter( Target );
          RunAway := True;
          Result := True;
        end;
      end;
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidArcher CollideFigure: ' + E.Message );

  end;

end;

procedure THumanoidArcherCombat.OnNoPath;
const
  FailName : string = 'THumanoidArcherCombat.OnNoPath';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    RunOrFight := False;
    Walking := False;
    if Assigned( Character.Track ) then
      Character.Face( Character.Track.X, Character.track.Y );
  except
    on E : Exception do
      Log.log( 'Error HumanoidArcher NoPath: ' + E.Message );

  end;

end;

procedure THumanoidArcherCombat.OnStop;
const
  FailName : string = 'THumanoidArcherCombat.OnStop';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    if ( character.X <> character.StartX ) and ( character.Y <> character.StartY ) then
      RunOrFight := true
    else
      RunOrFight := false;

    Walking := false;
    if Assigned( Character.Track ) then
      Character.Face( Character.Track.X, Character.track.Y );
  except
    on E : Exception do
      Log.log( 'Error HumanoidArcher Stop: ' + E.Message );

  end;

end;

procedure THumanoidArcherCombat.Regroup( Source : TAniFigure; NewX, NewY : Integer );
const
  FailName : string = 'THumanoidArcherCombat.Regroup';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    if bTakeOrders then
    begin
      if Assigned( Character.Track ) then
        Character.Track := nil;

      Character.WalkTo( NewX, NewY, 64 );
      Walking := true;
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidArcher Regroup: ' + E.Message );

  end;

end;

procedure THumanoidArcherCombat.WasAttacked( Source : TAniFigure; Damage : single );
var
  x : longint;
  y : longint;

const
  FailName : string = 'THumanoidArcherCombat.WasAttacked';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if Source is TCharacter then
      character.Face( TCharacter( Source ).x, TCharacter( Source ).y )
    else
    begin
      X := random( 100 ) + 50;
      Y := random( 100 ) + 50;
      Walking := True;
      Character.walkTo( X, Y, 16 );
    end;

    try
      if Assigned( Source ) then
        if Source <> character then
        begin
          if Character.IsEnemy( TCharacter( Source ) ) then
          begin
            if ( Character.Wounds / Character.HitPoints ) * 100 >= iTimeToRun then
            begin
              case FBaseCourage of
                0 :
                  if ( Random( 99 ) < 80 ) then
                    RunAway := true;
                1 :
                  if ( Random( 99 ) < 70 ) then
                    RunAway := true;
                2 :
                  if ( Random( 99 ) < 60 ) then
                    RunAway := true;
                3 :
                  if ( Random( 99 ) < 50 ) then
                    RunAway := true;
                4 :
                  if ( Random( 99 ) < 40 ) then
                    RunAway := true;
                5 :
                  if ( Random( 99 ) < 30 ) then
                    RunAway := true;
                6 :
                  if ( Random( 99 ) < 20 ) then
                    RunAway := true;
                7 :
                  if ( Random( 99 ) < 10 ) then
                    RunAway := true;
              end;
              Character.Track := TCharacter( Source );
            end
            else
            begin
              Character.Track := TCharacter( Source );
            end;
          end;
        end;
    except
      on E : Exception do
        Log.log( 'Error HumanoidArcher WasAttacked: ' + E.Message );
    end;

    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure THumanoidArcherCombat.WasKilled( Source : TAniFigure );
var
  List : TStringList;
  iLoop : integer;
const
  FailName : string = 'THumanoidArcherCombat.WasKilled';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if Source is TCharacter then
      character.Face( TCharacter( Source ).x, TCharacter( Source ).y );

    try
     //Tell everyone I died so they can save themselves
      List := GetPerceptibleAllies( Character, 1.5 );
      if Assigned( List ) then
      begin
        for iLoop := 0 to List.count - 1 do
        begin
          if Assigned( TCharacter( List.Objects[ iLoop ] ).AI ) then
            TAI( TCharacter( List.Objects[ iLoop ] ).AI ).NotifyOfDeath( character );
        end;
        list.free;
      end;
    except
      on E : Exception do
        Log.log( 'Error HumanoidArcher WasKilled: ' + E.Message );

    end;


  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor THumanoidArcherCombat.Destroy;
const
  FailName : string = 'THumanoidArcherCombat.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    if Assigned( FriendsList ) then
      FriendsList.free;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

(**************************************************************************************)


function THumanoidArcherCombat.OnCollideObject(
  Target : TAniFigure ) : Boolean;
const
  FailName : string = 'THumanoidArcherCombat.OnCollideObject';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  result := false;
  try
    if Target is TDoor then
    begin
      if ( TDoor( Target ).closed ) and ( TDoor( Target ).keyname = '' ) then
        TDoor( Target ).open;
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidArcherCombat CollideObject: ' + E.Message );
  end;
end;

function THumanoidArcherCombat.InRange( Target : TAniFigure ) : Boolean;
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
{****************************************************************}
{ THumanoidCasterCombat }

procedure THumanoidCasterCombat.CallToArms( Source, Target : TAniFigure );
var
  spell : string;
const
  FailName : string = 'THumanoidCasterCombat.CallToArms';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    if bTakeOrders then
    begin
      Character.Track := TCharacter( Target );
      Spell := strTokenAt( oSPellBook, ',', Random( StrTokenCount( oSpellBook, ',' ) ) );
      if Spells.indexOf( Spell ) <> -1 then
      begin
        if Character.CurrentSpell <> TSpell( Spells.Objects[ Spells.indexOf( Spell ) ] ) then
          Character.CurrentSpell := TSpell( Spells.Objects[ Spells.indexOf( Spell ) ] );
      end;
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidCaster CallToArms: ' + E.Message );
  end;

end;

procedure THumanoidCasterCombat.Execute;
const
  FailName : string = 'THumanoidCasterCombat.Execute';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
   // exit;
    try
   //   character.say(IntToStr(round(character.hitpoints)),clwhite);
      if PauseAndExit <> -1 then
      begin
        dec( PauseAndExit );
        if Character.TitleExists( 'cancelspelleffect' ) then
          Character.RemoveTitle( 'cancelspelleffect' );
        if Character.TitleExists( 'cancelinvis' ) then
          Character.RemoveTitle( 'cancelinvis' );
        if PauseAndExit = 0 then
          character.AIMode := aiIdle;
        exit;
      end;

      if ( FrameCount mod 160 ) = 0 then
        RunOrFight := true;


      if assigned( character.track ) then
        if ( character.track = character ) or character.IsAlly( character.track ) then
          character.track := nil;

      if CirclePoint > 535 then
        CirclePoint := Random( 360 ) + 180;


      if ( Delay > 0 ) and not ( Walking ) then
      begin
        dec( Delay );
        exit;
      end;

      if not ( walking ) and RunAway then
        MoveAway;

{  if Not(RunOrFight) and Assigned(Character.Track)and Not(walking) then
  begin
    if Character.IsEnemy(TCharacter(Character.Track)) then
    begin
       if character.rangeto(Character.Track.x, Character.Track.y)<iDistance then
       begin
            if character.InRange(Character.Track) then

            else
            begin
                 MoveAway;
                 RunOrFight := true;
                 exit;
            end;
       end
    end;
  end;
 }
      if bHealFirst and Assigned( Friendly ) and not ( walking ) then
        castHeal
      else if Assigned( Character.Track ) and not ( walking ) then
      begin
        if ( Character.Track = character ) then
          Character.Track := nil
        else
          Attack;
      end;


      if not Assigned( Character.Track ) then
        FindTarget;

      if Assigned( Friendly ) then
        if ( Friendly.Wounds < ( Friendly.HitPoints * 0.75 ) ) or Friendly.dead then
          Friendly := nil;

      if bHealFirst and not Assigned( Friendly ) then
        FindFriendly;

    except
      on E : Exception do
        Log.log( 'Error HumanoidCaster Execute: ' + E.Message );

    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure THumanoidCasterCombat.SummonGuards;
var
  Effect : TEffect;
  Effect2 : TEffect;
  NewGuard : TCharacter;
  GroupList : TStringList;
begin
  try

    if character.TitleExists( 'cancelinvis' ) then
      character.removetitle( 'cancelinvis' );

    GroupList := GetGroup( Character, 'SummonedGuards' );
    if Assigned( GroupList ) then
    begin
      if GroupList.count >= 5 then
      begin
        Grouplist.Free;
        exit;
      end;
      Grouplist.Free;
    end;
    if not ( Assigned( Character.track ) ) then
      exit;
    if not ( Assigned( FSummonSpawnSource ) ) then
      exit;

    Effect := TEffect.Create;
    Effect.Resource := SummonEffect;
    Effect.AnimationDuration := 8 * SummonEffect.FrameMultiplier;
    Effect.DoAction( 'Default', Character.FacingString );
    Character.AddEffect( Effect );
    Character.DoAction( 'Cast' );

    FSummonSpawnSource.Clone( TObject( NewGuard ), 'SG' + IntToStr( ( Random( 24 ) + 1 ) * ( Random( 9 ) + 1 ) ) );
    NewGuard.SetPos( Character.track.X + ( Random( 150 ) - 100 ) + 50, Character.track.Y + ( Random( 150 ) - 100 ) + 50, Character.z );
    NewGuard.HitPoints := 20;
    NewGuard.SpecialEffect := seNone;
    NewGuard.Highlightable := true;
    NewGuard.alpha := 100;
    NewGuard.HitPoints := 1;
    NewGuard.Face( Character.Track.x, character.track.y );
    NewGuard.Track := Character.track;
    NewGuard.enabled := true;
    NewGuard.GroupName := 'SummonedGuards';
    NewGuard.Properties[ 'equipmentlocked' ] := 'true';

    if Assigned( character.Equipment[ slMisc1 ] ) then
      if character.Equipment[ slMisc1 ] is TWeapon then
        PlaySound( TWeapon( character.Equipment[ slMisc1 ] ).StrikeLeatherSounds, NewGuard.X, NewGuard.Y );


    Effect2 := TEffect.Create;
    Effect2.Resource := SummonResource;
    Effect2.AnimationDuration := 8 * SummonResource.FrameMultiplier;
    Effect2.DoAction( 'Default', NewGuard.FacingString );

    if LowerCase( NewGuard.Properties[ 'UseColor' ] ) = 'true' then
    begin
      Effect2.ColorR := StrToInt( NewGuard.Properties[ 'ColorRed' ] );
      Effect2.ColorG := StrToInt( NewGuard.Properties[ 'ColorGreen' ] );
      Effect2.ColorB := StrToInt( NewGuard.Properties[ 'ColorBlue' ] );
    end
    else
    begin
      Effect2.ColorR := -100;
      Effect2.ColorG := -100;
      Effect2.ColorB := -100;
    end;

    Effect2.ApplyColor := true;
    Effect2.Alpha := Effect.Resource.Alpha;
    Effect2.SpecialEffect := seAdd;
    Effect2.DisableWhenDone := true;
    Effect2.Duration := 900;


    NewGuard.AddEffect( Effect2 );
    MySummons.Add( NewGuard );
    NewGuard := nil;

  except
    on E : Exception do
      Log.log( 'Error HumanoidCaster Summon: ' + E.Message );
  end;
end;

procedure THumanoidCasterCombat.SummonHellHound;
var
  Effect : TEffect;
  Effect2 : TEffect;
  NewSummon : TCharacter;
  GroupList : TStringList;
begin
  try
    if character.TitleExists( 'cancelinvis' ) then
      character.removetitle( 'cancelinvis' );

    GroupList := GetGroup( Character, 'HellHounds' );
    if Assigned( GroupList ) then
    begin
      if GroupList.count >= 5 then
      begin
        Grouplist.Free;
        exit;
      end;
      Grouplist.Free;
    end;

         //cast effect
    if not ( Assigned( Character.track ) ) then
      exit;
    Effect := TEffect.Create;
    Effect.Resource := SummonEffect;
    Effect.AnimationDuration := 8 * SummonEffect.FrameMultiplier;
    Effect.DoAction( 'Default', Character.FacingString );
    Character.AddEffect( Effect );
    Character.DoAction( 'Cast' );

    NewSummon := TCharacter( Sprites.NewSprite( TCharacter, WolfResource, Character.track.X + ( Random( 150 ) - 100 ) + 50, Character.track.Y + ( Random( 150 ) - 100 ) + 50, 0, 1 ) );
    with NewSummon do
    begin
      addtitle( 'flame' );
      addtitle( 'flame strike' );
      Properties[ 'oSpellBook' ] := 'flame strike,flame';
      Properties[ 'dSpellBook' ] := 'protection from cold';
      Properties[ 'BalanceWithPlayer' ] := '5';
      Properties[ 'OnDie' ] := 'doeffect(fadeaway)';
      Properties[ 'TimeToRun' ] := '99';
      Properties[ 'Distance' ] := '90';
      Properties[ 'BaseCourage' ] := '9';
      Properties[ 'DeathSounds' ] := 'HHoundDeath';
          //  Properties['DamageShield'] := '10';
          //  Properties['SpellEffect'] := 'ProtectionReceive(Fire).gif';
      Properties[ 'PainSounds' ] := 'HHoundHit1,HHoundHit2,HHoundHit3';
      Properties[ 'CastEffect' ] := 'ProtectionReceive(Fire).gif';
      GUID := 'HH' + IntToStr( Random( 25 ) * Random( 10 ) );
      Resistance.heat.Resistance := 100;
      Resistance.Cold.Resistance := -100;
      GroupName := 'HellHounds';
     // Resistance.heat.Invulnerability := 100;
      Damage.heat.Min := 1;
      Damage.heat.Max := 10;

      vision := 300;
      Mana := 100;
      Name := 'a hell hound';
      Alliance := 'summon';
      MakeEnemy( 'party' );
      Track := Character.track;
      IdleAI := 'HumanoidIdle';
      CombatAI := 'HumanoidCasterCombat';
      AIMode := aiCombat;
      AI := THumanoidCasterCombat.Create;

    end;
    TCharacterResource( NewSummon.Resource ).UseCastAnimation := false;

    if Assigned( character.Equipment[ slMisc1 ] ) then
      if character.Equipment[ slMisc1 ] is TWeapon then
        PlaySound( TWeapon( character.Equipment[ slMisc1 ] ).StrikeLeatherSounds, NewSummon.X, NewSummon.Y );

         //summon and color
    Effect2 := TEffect.Create;
    Effect2.Resource := SummonResource;
    Effect2.AnimationDuration := 8 * SummonResource.FrameMultiplier;
    Effect2.DoAction( 'Default', NewSummon.FacingString );
         //red
    Effect2.ColorR := 0;
    Effect2.ColorG := -200;
    Effect2.ColorB := -200;
    Effect2.ApplyColor := true;
    Effect2.Alpha := Effect.Resource.Alpha;
    Effect2.SpecialEffect := seadd;
    Effect2.DisableWhenDone := true;
    Effect2.Duration := Character.Mysticism * 50;
    NewSummon.AddEffect( Effect2 );

    MySummons.Add( NewSummon );
    NewSummon := nil;


  except
    on E : Exception do
      Log.log( 'Error HumanoidCaster Hell Hound: ' + E.Message );
  end;
end;

procedure THumanoidCasterCombat.MirrorImage;
var
  Effect : TEffect;
  Effect2 : TEffect;
  Effect3 : TEffect;
  S : string;
  NewGuard : TCharacter;
  i : integer;
begin
  try
    if character.TitleExists( 'cancelinvis' ) then
      character.removetitle( 'cancelinvis' );


    Character.DoAction( 'Cast' );
    Effect := TEffect.Create;
    Effect.Resource := SummonResource;
    Effect.AnimationDuration := 8 * SummonResource.FrameMultiplier;
    Effect.DoAction( 'Default', character.FacingString );
    Effect.Alpha := Effect.Resource.Alpha;
    Effect.SpecialEffect := seAdd;
    Character.AddEffect( Effect );
    if Assigned( character.Equipment[ slMisc1 ] ) then
      if character.Equipment[ slMisc1 ] is TWeapon then
        PlaySound( TWeapon( character.Equipment[ slMisc1 ] ).StrikeLeatherSounds, character.X, character.Y );

    for i := 0 to 2 do
    begin
      Character.Clone( TObject( NewGuard ), 'MI' + IntToStr( Random( 25 ) * Random( 10 ) ) );
      case i of
        0 : NewGuard.SetPos( Character.X, Character.Y + 50, Character.z );
        1 : NewGuard.SetPos( Character.X + 50, Character.Y, Character.z );
        2 : NewGuard.SetPos( Character.X - 50, Character.Y, Character.z );
      end;
      NewGuard.Face( Character.Track.x, character.track.y );
      NewGuard.Facing := Character.Facing;
      NewGuard.SpecialEffect := seNone;
      NewGuard.Highlightable := true;
      NewGuard.alpha := 100;
      NewGuard.HitPoints := 1;
      NewGuard.GroupName := 'mirror';
      NewGuard.Properties[ 'BalanceWithPlayer' ] := '0';
      NewGuard.Properties[ 'SummonGUID' ] := '';
      NewGuard.Properties[ 'oSpellBook' ] := 'frost,flame,buff';
      NewGuard.Properties[ 'equipmentlocked' ] := 'true';
      NewGuard.OnDie := 'doeffect(Fadeaway)';
      NewGuard.Track := Character.track;
      NewGuard.Alliance := 'summon';

      // draw this character transparent
      S := LowerCase( Character.Properties[ 'Transparent' ] );
      try
        if S <> '100' then
        begin
          NewGuard.Alpha := StrToInt( s );
          NewGuard.SpecialEffect := seTranslucent;
        end;
      except
      end;

    // draw this character transparent
      S := LowerCase( Character.Properties[ 'UseColor' ] );
      try
        if S = 'true' then
        begin
          Effect3 := TColorMeEffect.create;
          Effect3.ColorR := StrToInt( NewGuard.Properties[ 'ColorRed' ] );
          Effect3.ColorG := StrToInt( NewGuard.Properties[ 'ColorGreen' ] );
          Effect3.ColorB := StrToInt( NewGuard.Properties[ 'ColorBlue' ] );
          Effect3.ApplyColor := true;
          NewGuard.AddEffect( Effect3 );
        end;
      except
      end;
      NewGuard.enabled := true;

      if Assigned( character.Equipment[ slMisc1 ] ) then
        if character.Equipment[ slMisc1 ] is TWeapon then
          PlaySound( TWeapon( character.Equipment[ slMisc1 ] ).StrikeLeatherSounds, NewGuard.X, NewGuard.Y );

      Effect2 := TEffect.Create;
      Effect2.Resource := SummonResource;
      Effect2.AnimationDuration := 8 * SummonResource.FrameMultiplier;
      Effect2.DoAction( 'Default', NewGuard.FacingString );
      Effect2.Alpha := Effect.Resource.Alpha;
      Effect2.SpecialEffect := seAdd;
      Effect2.DisableWhenDone := true;
      Effect2.Duration := 1500;
      NewGuard.AddEffect( Effect2 );
      MySummons.Add( NewGuard );
      NewGuard := nil;
    end;

  except
    on E : Exception do
      Log.log( 'Error HumanoidCaster Mirror: ' + E.Message );
  end;
end;


procedure THumanoidCasterCombat.BattleTactic;
var
  r : Integer;
  T : single;
  X, Y : Integer;
const
  FailName : string = 'THumanoidCasterCombat.BattleTactic';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    if not Walking and bMove then
    begin
      NukeCounter := 0;
      inc( CirclePoint, 45 );
      r := iDistance; //300
      T := pi2 * CirclePoint / 360;
      X := round( r * cos( T ) ) + TCharacter( Character.Track ).X;
      Y := round( r * sin( T ) / 2 ) + TCharacter( Character.Track ).Y;
      Walking := True;
      Character.WalkTo( X, Y, 48 );
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidCaster BattleTactic: ' + E.Message );

  end;

end;

procedure THumanoidCasterCombat.CastInvis;
var
  Effect : TEffect;
  Effect2 : TEffect;
begin
  try
    if character.TitleExists( 'cancelinvis' ) then
      exit;
    if Assigned( SummonResource ) then
    begin
//           Character.DoAction('Cast');
      Effect := TEffect.Create;
      Effect.Resource := SummonResource;
      Effect.AnimationDuration := 8 * SummonResource.FrameMultiplier;
      Effect.Alpha := Effect.Resource.Alpha;
      Effect.SpecialEffect := seAdd;
      Effect.DoAction( 'Default', character.FacingString );

      Character.AddEffect( Effect );

      Effect2 := TInvisEffect.Create;
      Effect2.Duration := character.Mysticism * 10;
      character.AddEffect( Effect2 );
    //jah  Delay := Random(90);
      RunAway := true;
      Character.DoAction( 'Cast' );
      delay := 32;

    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidIdle HaloEffect: ' + E.Message );
  end;

end;

procedure THumanoidCasterCombat.CastHeal;
const
  FailName : string = 'THumanoidCasterCombat.CastHeal';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    if Walking then
      exit;
    if ( TCharacter( Friendly ).Dead ) or Character.IsEnemy( TCharacter( Friendly ) ) then
      Friendly := nil
    else
    begin
      if character.TitleExists( 'cancelinvis' ) then
        character.removetitle( 'cancelinvis' );

      if Spells.indexOf( 'heal' ) <> -1 then
      begin
        if Character.currentSpell <> TSpell( Spells.Objects[ Spells.indexOf( 'heal' ) ] ) then
          Character.CurrentSpell := TSpell( Spells.Objects[ Spells.indexOf( 'heal' ) ] );

        Character.Face( Friendly.x, Friendly.y );

        if ( character.Mana - character.Drain ) >= Character.CurrentSpell.Drain( Character ) then
          character.Cast( Friendly )
      //  else
      //    Delay := Random(360) + 120;
      end;
    end
  except
    on E : Exception do
      Log.log( 'Error HumanoidCaster CastHeal: ' + E.Message );
  end;

end;


procedure THumanoidCasterCombat.MoveAway;
const
  FailName : string = 'THumanoidCasterCombat.MoveAway';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    Walking := True;
    if assigned( Character.track ) then
      Character.Face( Character.Track.X, Character.track.Y );
    case Character.Facing of
      fNE, fEE, fSE :
        begin
          if AllowRun then
            Character.RunTo( Character.X - 100, Character.Y + random( 200 ) - 100, 64 )
          else
            Character.WalkTo( Character.X - 100, Character.Y + random( 200 ) - 100, 64 );
        end;
      fNW, fWW, fSW :
        begin
          if AllowRun then
            Character.RunTo( Character.X + 100, Character.Y + random( 200 ) - 100, 64 )
          else
            Character.WalkTo( Character.X + 100, Character.Y + random( 200 ) - 100, 64 );
        end;
      fSS :
        begin
          if AllowRun then
            Character.RunTo( Character.X + random( 200 ) - 100, Character.Y - 100, 64 )
          else
            Character.WalkTo( Character.X + random( 200 ) - 100, Character.Y - 100, 64 );
        end;

      fNN :
        begin
          if AllowRun then
            Character.RunTo( Character.X + random( 200 ) - 100, Character.Y + 100, 64 )
          else
            Character.WalkTo( Character.X + random( 200 ) - 100, Character.Y + 100, 64 )
        end;
    end;

    RunAway := False;
     //Keep running??
    if ( Character.Wounds / Character.HitPoints ) * 100 >= iTimeToRun then
    begin

      case FBaseCourage of
        0 :
          if ( Random( 99 ) < 80 ) then
            RunAway := true;
        1 :
          if ( Random( 99 ) < 70 ) then
            RunAway := true;
        2 :
          if ( Random( 99 ) < 60 ) then
            RunAway := true;
        3 :
          if ( Random( 99 ) < 50 ) then
            RunAway := true;
        4 :
          if ( Random( 99 ) < 40 ) then
            RunAway := true;
        5 :
          if ( Random( 99 ) < 30 ) then
            RunAway := true;
        6 :
          if ( Random( 99 ) < 20 ) then
            RunAway := true;
        7 :
          if ( Random( 99 ) < 10 ) then
            RunAway := true;
      end;
      if Assigned( Character.currentSpell ) then
        Delay := character.CurrentSpell.Recovery( Character );
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidCaster MoveAway: ' + E.Message );

  end;

end;

procedure THumanoidCasterCombat.Attack;
var
  spell : string;
  Effect : TEffect;
const
  FailName : string = 'THumanoidCasterCombat.Attack';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if TCharacter( Character.Track ).Dead or not ( TCharacter( Character.Track ).enabled ) then
      Character.Track := nil
    else
    begin
      if ( character.rangeto( Character.Track.x, Character.Track.y ) > 50 ) or ( random( 2 ) = 1 ) then
      begin
        if NukeCounter < CastTimes then
        begin
          if bMove then
          begin
            Inc( NukeCounter );
//                 character.say('new nuke count',clwhite);
          end;

          Character.Face( Character.Track.x, Character.Track.y );

          if HoldCast then
            Spell := strTokenAt( oSPellBook, ',', Random( StrTokenCount( oSpellBook, ',' ) ) )
          else
          begin
            HoldCast := true;
            Spell := 'Hold';
          end;
          if Spell = 'hellhound' then
          begin
            SummonHellHound;
            Delay := 32;
            exit;
          end;

          if Spell = 'summonguards' then
          begin
            SummonGuards;
            Delay := 20;
            exit;
          end;
          if Spell = 'invis' then
          begin
            castInvis;
            exit;
          end;

          if Spell = 'mirrorimage' then
          begin
            if alldead( 'mirror' ) then
            begin
              mirror := true;
              MirrorImage;
              Delay := 20;
            end;
            exit;
          end;

          if Spell = 'buff' then
          begin
            BuffAllies;
            exit;
          end;
          //line of sight test here.
          if game.LineOfSight( character.x, character.y, TCharacter( character.track ).x, TCharacter( character.track ).y ) then
           //InRange(Character.Track) then
          begin
            if not ( IsAnybodyInTheWay( character, TCharacter( Character.track ), 20 ) ) then
            begin
                 //NukeCounter := 3;
              if character.TitleExists( 'cancelinvis' ) then
              begin
                if ( Spell = 'invis' ) then
                  exit
                else
                  character.removetitle( 'cancelinvis' );
              end;

              if Spells.indexOf( Spell ) <> -1 then
              begin
                if Character.currentSpell <> TSpell( Spells.Objects[ Spells.indexOf( Spell ) ] ) then
                  Character.CurrentSpell := TSpell( Spells.Objects[ Spells.indexOf( Spell ) ] );
                if ( ( character.Mana - character.Drain ) >= Character.CurrentSpell.Drain( Character ) ) then
                begin
                  if Assigned( CastEffect ) then
                  begin
                    character.doaction( 'cast' );
                    Effect := TEffect.Create;
                    Effect.Resource := CastEffect;
                    Effect.AnimationDuration := 8 * CastEffect.FrameMultiplier;
                    Effect.Power := Character.Mysticism;
                    Effect.DoAction( 'Default', Character.FacingString );
                    Character.AddEffect( Effect );
                  end;
                  character.Cast( Character.Track );
                //  delay := 16
                end
                else
                begin
                    //JAH
//                    character.say('No Mana', clwhite);

                  if bMove then
                    RunAway := true;
                //  Delay := Random(360) + 120;
                end;
              end
              else
              begin
                      //JAH
//                     character.say('No Spell', clwhite);

                BuffAllies;
                BattleTactic;
              end;
            end
            else
            begin
                    //JAH
//                    character.say('In the way', clwhite);

              BuffAllies;
              BattleTactic;
            end;
          end;
        end
        else
        begin
                //JAH
//                character.say('Nuke Counter', clwhite);

          NukeCounter := 0;
              //  BuffAllies;
          BattleTactic;
        end;
      end
      else
      begin
        //JAH
//        character.say('Range Too', clwhite);

        if random( 2 ) = 1 then
        begin
          Character.Face( Character.Track.x, Character.Track.y );
          if not ( character.TitleExists( 'NoPush' ) ) then
          begin
            if ( Spells.indexOf( 'Push' ) <> -1 ) and ( character.alliance <> 'summon' ) then
            begin
              if Character.currentSpell <> TSpell( Spells.Objects[ Spells.indexOf( 'Push' ) ] ) then
                Character.CurrentSpell := TSpell( Spells.Objects[ Spells.indexOf( 'Push' ) ] );

              if ( character.Mana - character.Drain ) >= Character.CurrentSpell.Drain( Character ) then
              begin
                if Assigned( CastEffect ) then
                begin
                  character.doaction( 'cast' );
                  Effect := TEffect.Create;
                  Effect.Resource := PushEffect;
                  Effect.AnimationDuration := 8 * PushEffect.FrameMultiplier;
                  Effect.DoAction( 'Default', Character.FacingString );
                  Character.AddEffect( Effect );
                end;
                character.Cast( Character.Track );
                Delay := 64; //dont cast push so much
              end
              else
              begin
                if RunOrFight and bMove then
                  runaway := true
                else if Character.Inrange( Character.Track ) then
                  Character.Attack( Character.Track );
              end;
            end
            else
            begin
              if Character.Inrange( Character.Track ) then
                Character.Attack( Character.Track )
            end
          end
          else
          begin
            if ( Spells.indexOf( 'Blades' ) <> -1 ) and ( character.alliance <> 'summon' ) then
            begin
              if Character.currentSpell <> TSpell( Spells.Objects[ Spells.indexOf( 'Blades' ) ] ) then
                Character.CurrentSpell := TSpell( Spells.Objects[ Spells.indexOf( 'Blades' ) ] );

              if ( character.Mana - character.Drain ) >= Character.CurrentSpell.Drain( Character ) then
              begin
                if Assigned( CastEffect ) then
                begin
                  character.doaction( 'cast' );
                  Effect := TEffect.Create;
                  Effect.Resource := RotatingBlades;
                  Effect.AnimationDuration := 8 * PushEffect.FrameMultiplier;
                  Effect.DoAction( 'Default', Character.FacingString );
                  Character.AddEffect( Effect );
                end;
                character.Cast( Character.Track );
                Delay := 10; //dont cast push so much
                RunAway := true
              end
              else
              begin
                if RunOrFight and bMove then
                  runaway := true
                else if Character.Inrange( Character.Track ) then
                  Character.Attack( Character.Track );
              end;
            end
            else
            begin
              if Character.Inrange( Character.Track ) then
                Character.Attack( Character.Track );
              RunAway := true;
            end
          end
        end
        else
        begin
          if RunOrFight and bMove then
            RunAway := true
          else
            {//if Character.Inrange(Character.Track) then} if InRange( Character.Track ) then
              Character.Attack( Character.Track );
        end;
      end;
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidCaster Attack: ' + E.Message );

  end;
end;

procedure THumanoidCasterCombat.BuffAllies;
var
  spell : string;
  Effect : TEffect;
  Allies : TStringList;
  Alley : integer;
begin
  try
    Spell := strTokenAt( DSPellBook, ',', Random( StrTokenCount( DSpellBook, ',' ) ) );
    if ( Spells.indexOf( Spell ) = -1 ) or ( LowerCase( Spell ) = 'none' ) then
      exit;

    if character.TitleExists( 'cancelinvis' ) then
    begin
      if ( Spell = 'invis' ) then
        exit
      else
        character.removetitle( 'cancelinvis' );
    end;


    if Character.currentSpell <> TSpell( Spells.Objects[ Spells.indexOf( Spell ) ] ) then
      Character.CurrentSpell := TSpell( Spells.Objects[ Spells.indexOf( Spell ) ] );

    if ( ( character.Mana - character.Drain ) >= Character.CurrentSpell.Drain( Character ) ) then
    begin
      if Assigned( CastEffect ) then
      begin
        character.doaction( 'cast' );
        Effect := TEffect.Create;
        Effect.Resource := CastEffect;
        Effect.AnimationDuration := 8 * CastEffect.FrameMultiplier;
        Effect.Power := Character.Mysticism;
        Effect.DoAction( 'Default', Character.FacingString );
        Character.AddEffect( Effect );
      end;
      Allies := GetPerceptibleAllies( Character, 1.5 );

      if assigned( Allies ) then
      begin
        Alley := Random( Allies.count );
        if Allies.objects[ Alley ] is TCharacter then
          if not ( TCharacter( Allies.objects[ Alley ] ).dead ) then
            if TCharacter( Allies.objects[ Alley ] ).Alliance <> 'summon' then
            begin
              character.Cast( TCharacter( Allies.objects[ Alley ] ) );
              Allies.free;
              exit;
            end;
        character.Cast( Character );
        Allies.free;
      end
      else
        character.Cast( Character );

    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidCaster BuffAllies: ' + E.Message );
  end;
end;

procedure THumanoidCasterCombat.FindFriendly;
var
  i : integer;
const
  FailName : string = 'THumanoidCasterCombat.FindFriendly';
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

procedure THumanoidCasterCombat.FindTarget;
var
  list : TStringList;
const
  FailName : string = 'THumanoidCasterCombat.FindTarget';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    List := GetPerceptibleEnemies( Character, 2 );
    if assigned( List ) then
    begin
      if List.Count = 1 then
        Character.Track := TCharacter( List.objects[ 0 ] )
      else
        Character.Track := TCharacter( List.objects[ random( List.count ) ] );
      list.free;
      Character.CurrentSpell := nil;
    end
    else
      PauseAndExit := 32;
  except
    on E : Exception do
      Log.log( 'Error HumanoidCaster FindTarget: ' + E.Message );

  end;

end;


procedure THumanoidCasterCombat.Init;
var
  S : string;
  i : integer;
  Effect : TEffect;
const
  FailName : string = 'THumanoidCasterCombat.Init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    PauseAndExit := -1;
    RunOrFight := true;
    CirclePoint := Random( 360 ) + 180;
    NukeCounter := 0;
    CastTimes := Random( 4 ) + 2;
    MySummons := TList.Create;
    character.AddTitle( 'Flame' );
    character.AddTitle( 'Shock' );
    character.AddTitle( 'Push' );
    character.AddTitle( 'Shrapnel' );
    character.AddTitle( 'Frost' );
    character.AddTitle( 'Blizzard' );
    character.AddTitle( 'Flame Strike' );
    character.AddTitle( 'Heal' );
    character.AddTitle( 'Healing' );
    character.AddTitle( 'Charge' );
    character.AddTitle( 'Hold' );
    character.AddTitle( 'Aura of Iron' );
    character.AddTitle( 'Aura of Steel' );
    character.AddTitle( 'Shadow' );
    character.AddTitle( 'Protection from Fire' );
    character.AddTitle( 'Protection from Cold' );
    character.AddTitle( 'Protection from Lightning' );
    character.AddTitle( 'Protection from Poison' );
    character.AddTitle( 'Protection from Magic' );
    character.AddTitle( 'Protection from All' );
    character.AddTitle( 'Deflect' );
    character.AddTitle( 'Mana Thief' );
    character.AddTitle( 'Great Hand' );
    character.AddTitle( 'Great Wolf' );
    character.addtitle( 'blades' );

    Spells := character.SpellList;


  //  character.AddTitle('Reflect');

    S := Character.Properties[ 'iSpeed' ];
    try
      if ( S <> '' ) and ( s <> '0' ) then
        TCharacterResource( character.Resource ).Speed := StrToInt( S );
    except
    end;

    S := Character.Properties[ 'TimeToRun' ];
    try
      if S = '' then
        iTimeToRun := 75
      else
        iTimeToRun := StrToInt( S );
    except
      iTimeToRun := 75;
    end;

    S := LowerCase( Character.Properties[ 'SpellEffect' ] );
    if ( s <> '' ) then
    begin
      if s = 'pulse' then
      begin
        Effect := TPulse.Create;
        TPulse( Effect ).BlueMaster := StrToInt( Character.Properties[ 'ColorBlue' ] );
        TPulse( Effect ).GreenMaster := StrToInt( Character.Properties[ 'ColorGreen' ] );
        TPulse( Effect ).RedMaster := StrToInt( Character.Properties[ 'ColorRed' ] );
        Character.AddTitle( 'cancelspelleffect' );
        Character.AddEffect( Effect );
        Effect := nil;
      end
      else
      begin
        FSpellEffectResource := GetSpellEffect( s );
        Character.AddTitle( 'cancelspelleffect' );
        Effect := TShieldSpellEffect.Create;
        Effect.Resource := FSpellEffectResource;
        Effect.Duration := 0;
        Effect.AnimationDuration := Effect.Resource.FrameCount * Effect.Resource.FrameMultiplier;
        Character.AddEffect( Effect );
        Effect := nil;
      end;
    end;

    S := LowerCase( Character.Properties[ 'CastEffect' ] );
    if ( s <> '' ) then
    begin
      CastEffect := GetSpellEffect( s );

//      CastEffect := LoadArtResource('engine\spells\' + s);
//            CastEffect.Alpha := 100;
//      CastEffect.SpecialEffect := seadd;
//      CastEffect.DrawShadow := false;
//      PushEffect := LoadArtResource('engine\spells\PushCast(LVL3).gif');
//      PushEffect.Alpha := 50;
//      PushEffect.SpecialEffect := seadd;
//      PushEffect.DrawShadow := false;

    end;

    S := LowerCase( Character.Properties[ 'BalanceWithPlayer' ] );
    try
      if ( S <> '' ) and ( s <> '0' ) then
      begin
        i := StrToInt( s );
        if i >= 0 then
        begin
          if player.TitleExists( 'Apprentice' ) then
          begin
            character.Wounds := 0;
            Character.Drain := 0;
            character.Mysticism := ( ( player.Mysticism * 3 ) div 4 ) + i;
            character.perception := ( ( player.perception * 3 ) div 4 ) + i;
            character.Coordination := ( ( player.Coordination * 3 ) div 4 ) + i;
            if not ( Character.TitleExists( 'NoHP' ) ) then
            begin
              if character.BaseHitPoints > 0 then
                if character.HitPoints > ( ( player.Perception * 2 ) ) then
                  character.HitPoints := ( ( player.Perception * 2 ) * NPCList.Count );
            end;

            character.combat := ( ( player.combat * 3 ) div 4 );
            Character.HitRecovery := -5;
            Character.AttackRecovery := Character.AttackRecovery + ( player.attackRecovery div i );

          end;
          if player.TitleExists( 'Hunter' ) then
          begin
            character.Wounds := 0;
            Character.Drain := 0;
            character.Mysticism := ( ( player.Stealth * 3 ) div 4 ) + i;
            character.perception := ( ( player.Coordination * 3 ) div 4 ) + i;
            character.Coordination := ( ( player.Coordination * 3 ) div 4 ) + i;
            if not ( Character.TitleExists( 'NoHP' ) ) then
            begin
              if character.BaseHitPoints > 0 then
                character.HitPoints := ( ( player.strength * 2 ) * NPCList.Count );
            end;

            character.combat := ( ( player.combat * 3 ) div 4 );

          end;
          if player.TitleExists( 'Squire' ) then
          begin

            character.Wounds := 0;
            Character.Drain := 0;
            character.Mysticism := ( ( player.Combat * 3 ) div 4 ) + i;
            character.perception := ( ( player.Strength * 3 ) div 4 ) + i;
            character.Coordination := ( ( player.Strength * 3 ) div 4 ) + i;
            character.combat := ( ( player.combat * 3 ) div 4 );
            if not ( Character.TitleExists( 'NoHP' ) ) then
            begin
              if character.BaseHitPoints > 0 then
                character.HitPoints := ( ( player.Coordination * 2 ) * NPCList.Count );
            end;
          end;



          if character.Resistance.Heat.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Heat.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Cold.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Cold.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Electric.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Electric.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Magic.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Magic.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Poison.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Poison.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Mental.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Mental.Resistance := ( player.mysticism / 200 );

        end;
      end;
    except
    end;

    S := Character.Properties[ 'BaseCourage' ];
    try
      if S = '' then
        FBaseCourage := 1
      else
        FBaseCourage := StrToInt( S );
    except
      FBaseCourage := 1;
    end;


    S := LowerCase( Character.Properties[ 'TakeOrders' ] );
    try
      if S = '' then
        bTakeOrders := true
      else if S = 'false' then
        bTakeOrders := False
      else
        bTakeOrders := true;
    except
      bTakeOrders := true;
    end;

    S := LowerCase( Character.Properties[ 'AllowRun' ] );
    try
      if S = '' then
        AllowRun := true
      else if S = 'false' then
        AllowRun := False
      else
        AllowRun := true;
    except
      AllowRun := true;
    end;


    S := LowerCase( Character.Properties[ 'oSpellBook' ] );
    try
      if S = '' then
        oSpellBook := 'frost,shrapnel,flame,charge,buff,hold'
      else
        oSpellBook := s
    except
      oSpellBook := 'frost,shrapnel,flame,charge,buff,hold';
    end;

    if Pos( 'hold', OSpellBook ) = 0 then
      HoldCast := true;



    S := LowerCase( Character.Properties[ 'SummonGUId' ] );
    try
      if S = '' then
        FSummonGuid := ''
      else
      begin

        FSummonGuid := s;
        Character.addtitle( 'SummonGuards' );
      end;

    except
      FSummonGuid := '';
    end;

{    if FileExists(ArtPath + 'engine\spells\SummonReceive.pox') and FileExists(ArtPath + 'engine\spells\SummonCast.pox') then
    begin
      FSummonResource := LoadArtResource('engine\spells\SummonReceive.gif');
      FSummonResource.Alpha := 75;
      FSummonResource.SpecialEffect := seadd;
      FSummonResource.DrawShadow := false;
      FSummonCastResource := LoadArtResource('engine\spells\SummonCast.gif');
      FSummonCastResource.Alpha := 75;
      FSummonCastResource.SpecialEffect := seadd;
      FSummonCastResource.DrawShadow := false;
    end;
}
    if Character.TitleExists( 'SummonGuards' ) then
    begin
      if Assigned( SummonResource ) then
      begin

        FriendsList := GetGroup( Character, Character.GroupName );
        if Assigned( FriendsList ) then
        begin
          for i := 0 to FriendsList.count - 1 do
          begin
            if FriendsList.Objects[ i ] is TCharacter then
              if LowerCase( TCharacter( FriendsList.Objects[ i ] ).GUID ) = LowerCase( FSummonGuid ) then
              begin
                FSummonSpawnSource := TCharacter( FriendsList.Objects[ i ] );
                Break;
              end;
          end;
          FriendsList.Free;
          FriendsList := nil;
        end;
        if not ( Assigned( FSummonSpawnSource ) ) then
          character.removetitle( 'SummonGuards' )
        else
          oSpellBook := oSpellBook + ',summonguards';

      end
    end;

    S := LowerCase( Character.Properties[ 'dSpellBook' ] );
    try
      if S = '' then
        dSpellBook := 'protection from all,aura of iron'
      else
        dSpellBook := s
    except
      dSpellBook := 'protection from all,aura of iron';
    end;


    S := LowerCase( Character.Properties[ 'Moveable' ] );
    try
      if S = '' then
        bMove := true
      else if S = 'false' then
        bMove := False
      else
        bMove := true;
    except
      bMove := true;
    end;

    S := LowerCase( Character.Properties[ 'HealFirst' ] );
    try
      if S = '' then
        bHealFirst := true
      else if S = 'false' then
        bHealFirst := False
      else
        bHealFirst := true;

    except
      bHealFirst := true;
    end;
        //JAH FixME
    bHealFirst := False;


    S := Character.Properties[ 'Distance' ];
    try
      if S = '' then
        iDistance := 175
      else
        iDistance := StrToInt( S );
    except
      iDistance := 175;
    end;

    S := Character.Properties[ 'DamageShield' ];
    try
      if S = '' then
        iDamageShield := 0
      else
        iDamageShield := StrToInt( S );
    except
      iDamageShield := 0;
    end;

    PartyTotal := 1;
    if character.GroupName <> '' then
      FriendsList := GetGroup( Character, Character.GroupName );
    if Assigned( FriendsList ) then
      PartyTotal := Friendslist.Count
    else
    begin
      FriendsList := GetPerceptibleAllies( Character, 1 );
      if assigned( FriendsList ) then
      begin
        PartyTotal := Friendslist.Count;
        FriendsList.Free;
        FriendsList := nil;
      end
    end;

    if Character.Alliance <> 'summon' then
    begin
      S := strTokenAt( DSPellBook, ',', Random( StrTokenCount( DSpellBook, ',' ) ) );
      if Spells.indexOf( S ) <> -1 then
      begin
        if character.titleExists( 'deflectfirst' ) and ( Spells.indexOf( 'deflect' ) <> -1 ) then
          Character.CurrentSpell := TSpell( Spells.Objects[ Spells.indexOf( 'deflect' ) ] )
        else
          Character.CurrentSpell := TSpell( Spells.Objects[ Spells.indexOf( S ) ] );
        if ( ( character.Mana - character.Drain ) >= Character.CurrentSpell.Drain( Character ) ) then
        begin

          if Assigned( CastEffect ) then
          begin
            Effect := TEffect.Create;
            Effect.Resource := CastEffect;
            Effect.AnimationDuration := 8 * CastEffect.FrameMultiplier;
            Effect.Power := Character.Mysticism;
            Effect.DoAction( 'Default', Character.FacingString );
            Character.AddEffect( Effect );
            Character.doaction( 'cast' );
          end;
          if character.TitleExists( 'cancelinvis' ) then
            character.removetitle( 'cancelinvis' );
          character.Cast( Character );
        end;
      end;
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidCaster Init: ' + E.Message );

  end;

  //Delay := random(60);

end;



procedure THumanoidCasterCombat.NotifyOfDeath( Source : TAniFigure );
var
  tmpPartyTotal : integer;
  list : TStringList;
const
  FailName : string = 'THumanoidCasterCombat.NotifyOfDeath';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    tmpPartyTotal := 0;
    List := GetPerceptibleAllies( Character, 1.5 );
    if Assigned( List ) then
    begin
      tmpPartyTotal := list.Count;
      list.free;
    end;
    if Assigned( THumanoidCombat( TCharacter( Source ).AI ) ) then
      case FBaseCourage of
        0 :
          if ( ( ( PartyTotal div 2 ) >= tmpPartyTotal ) ) and ( Random( 99 ) < 80 ) then
            MoveAway;
        1 :
          if ( ( ( PartyTotal div 2 ) >= tmpPartyTotal ) or ( THumanoidCombat( TCharacter( Source ).AI ).BaseCourage > 1 ) ) and ( Random( 99 ) < 70 ) then
            MoveAway;
        2 :
          if ( ( ( PartyTotal div 2 ) > tmpPartyTotal ) or ( THumanoidCombat( TCharacter( Source ).AI ).BaseCourage > 5 ) ) and ( Random( 99 ) < 60 ) then
            MoveAway;
        3 :
          if ( ( ( PartyTotal div 2 ) > tmpPartyTotal ) or ( THumanoidCombat( TCharacter( Source ).AI ).BaseCourage > 5 ) ) and ( Random( 99 ) < 50 ) then
            MoveAway;
        4 :
          if ( ( ( PartyTotal div 2 ) > tmpPartyTotal ) or ( THumanoidCombat( TCharacter( Source ).AI ).BaseCourage > 6 ) ) and ( Random( 99 ) < 40 ) then
            MoveAway;
        5 :
          if ( ( ( PartyTotal div 2 ) > tmpPartyTotal ) or ( THumanoidCombat( TCharacter( Source ).AI ).BaseCourage > 7 ) ) and ( Random( 99 ) < 30 ) then
            MoveAway;
        6 :
          if ( ( ( PartyTotal div 2 ) > tmpPartyTotal ) or ( THumanoidCombat( TCharacter( Source ).AI ).BaseCourage > 8 ) ) and ( Random( 99 ) < 20 ) then
            MoveAway;
        7 :
          if ( tmpPartyTotal = 0 ) and ( Random( 99 ) < 10 ) then
            MoveAway;
        8 :
          if ( tmpPartyTotal = 0 ) and ( character.wounds >= ( Character.HitPoints * 0.75 ) ) then
            MoveAway;
      end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidCaster NotifyOfDeath: ' + E.Message );

  end;

end;

function THumanoidCasterCombat.OnCollideFigure(
  Target : TAniFigure ) : boolean;
const
  FailName : string = 'THumanoidCasterCombat.OnCollideFigure';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  Result := False;
  try
    if Target = Character.Track then
    begin
//        RunAway := True;
      Result := True;
    end
    else
    begin
      if Target is TCharacter then
      begin
        if Character.IsEnemy( TCharacter( Target ) ) then
        begin
          Character.Track := TCharacter( Target );
  //          RunAway := True;
          Result := True;
        end;
      end;
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidCaster CollideFigure: ' + E.Message );

  end;

end;

procedure THumanoidCasterCombat.OnNoPath;
const
  FailName : string = 'THumanoidCasterCombat.OnNoPath';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    RunOrFight := False;
    Walking := False;
    bmove := false;
  //  if Assigned(Character.Track) then
  //     Character.Face(Character.Track.X,Character.track.Y);
  except
    on E : Exception do
      Log.log( 'Error HumanoidCaster NoPath: ' + E.Message );

  end;

end;

procedure THumanoidCasterCombat.OnStop;
const
  FailName : string = 'THumanoidCasterCombat.OnStop';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    if ( character.X <> character.StartX ) and ( character.Y <> character.StartY ) then
      RunOrFight := true
    else
      RunOrFight := false;

    Walking := false;
     // if Assigned(Character.Track) then
     //    Character.Face(Character.Track.X,Character.track.Y);
  except
    on E : Exception do
      Log.log( 'Error HumanoidCaster Stop: ' + E.Message );

  end;

end;

procedure THumanoidCasterCombat.Regroup( Source : TAniFigure; NewX, NewY : Integer );
const
  FailName : string = 'THumanoidCasterCombat.Regroup';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    if bTakeOrders then
    begin
      if Assigned( Character.Track ) then
        Character.Track := nil;

      Character.WalkTo( NewX, NewY, 64 );
      Walking := true;
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidCaster Regroup: ' + E.Message );

  end;

end;

procedure THumanoidCasterCombat.WasAttacked( Source : TAniFigure; Damage : single );
var
  x : longint;
  y : longint;
  Effect : TEffect;
const
  FailName : string = 'THumanoidCasterCombat.WasAttacked';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if Source is TCharacter then
      character.Face( TCharacter( Source ).x, TCharacter( Source ).y )
    else
    begin
      X := random( 100 ) + 50;
      Y := random( 100 ) + 50;
      Walking := True;
      Character.walkTo( X, Y, 16 );
    end;

    try
      if Assigned( Source ) then
        if Source <> character then
        begin
          if Character.IsEnemy( TCharacter( Source ) ) then
          begin
            if ( ( ( TCharacter( Source ) ).Properties[ 'DamageShield' ] = '0' ) or ( ( TCharacter( Source ) ).Properties[ 'DamageShield' ] = '' ) ) and ( iDamageShield > 0 ) and ( character.RangeTo( TCharacter( Source ).x, TCharacter( Source ).y ) < 50 ) then
            begin
              if Assigned( FSpellEffectResource ) then
              begin
                Effect := TEffect.Create;
                Effect.Resource := FSpellEffectResource;
                Effect.AnimationDuration := 8 * FSpellEffectResource.FrameMultiplier;
                Effect.ColorR := 250;
                Effect.ColorG := 250;
                Effect.ColorB := 250;
                Effect.ApplyColor := true;
                     // Effect.Power:=Character.Mysticism;
                Effect.DoAction( 'Default', TCharacter( Source ).FacingString );
                TCharacter( Source ).AddEffect( Effect );
              end;
              TCharacter( Source ).TakeDamage( Character, ( ( iDamageShield / 100 ) * Damage ), 0, False );
              Character.TakeDamage( Character, -( ( iDamageShield / 100 ) * Damage ), 0, False );
            end;

            if ( Character.Wounds / Character.HitPoints ) * 100 >= iTimeToRun then
            begin
              case FBaseCourage of
                0 :
                  if ( Random( 99 ) < 80 ) then
                    RunAway := true;
                1 :
                  if ( Random( 99 ) < 70 ) then
                    RunAway := true;
                2 :
                  if ( Random( 99 ) < 60 ) then
                    RunAway := true;
                3 :
                  if ( Random( 99 ) < 50 ) then
                    RunAway := true;
                4 :
                  if ( Random( 99 ) < 40 ) then
                    RunAway := true;
                5 :
                  if ( Random( 99 ) < 30 ) then
                    RunAway := true;
                6 :
                  if ( Random( 99 ) < 20 ) then
                    RunAway := true;
                7 :
                  if ( Random( 99 ) < 10 ) then
                    RunAway := true;
              end;

              if Spells.indexOf( 'heal' ) <> -1 then
              begin
                if Character.currentSpell <> TSpell( Spells.Objects[ Spells.indexOf( 'heal' ) ] ) then
                  Character.CurrentSpell := TSpell( Spells.Objects[ Spells.indexOf( 'heal' ) ] );
                if ( character.Mana - character.Drain ) >= Character.CurrentSpell.Drain( Character ) then
                  character.Cast( Character )
              end;

              Character.Track := TCharacter( Source );
            end
            else
            begin
              Character.Track := TCharacter( Source );
            end;
          end;
        end;
    except
      on E : Exception do
        Log.log( 'Error HumanoidCaster WasAttacked: ' + E.Message );
    end;

    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure THumanoidCasterCombat.WasKilled( Source : TAniFigure );
var
  List : TStringList;
  iLoop : integer;
  s : string;
  Effect : TEffect;
const
  FailName : string = 'THumanoidCasterCombat.WasKilled';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  character.removeTitle( 'cancelspelleffect' );
  if character.TitleExists( 'cancelinvis' ) then
    character.removetitle( 'cancelinvis' );

  try
    if Source is TCharacter then
      character.Face( TCharacter( Source ).x, TCharacter( Source ).y );
     //Tell everyone I died so they can save themselves
    List := GetPerceptibleAllies( Character, 1.5 );
    if Assigned( List ) then
    begin
      for iLoop := 0 to List.count - 1 do
      begin
        if Assigned( TCharacter( List.Objects[ iLoop ] ).AI ) then
          TAI( TCharacter( List.Objects[ iLoop ] ).AI ).NotifyOfDeath( character );
      end;
      list.free;
    end;
    if Assigned( MySummons ) then
    begin
      for iLoop := 0 to MySummons.count - 1 do
        if Assigned( MySummons.items[ iLoop ] ) then
          if TCharacter( MySummons.items[ iLoop ] ).enabled then
            if not ( TCharacter( MySummons.items[ iLoop ] ).dead ) then
              TCharacter( MySummons.items[ iLoop ] ).doaction( 'death' );
      MySummons.Clear;
      MySummons.free;
    end;

    S := LowerCase( Character.Properties[ 'DeathEffect' ] );
    if ( s <> '' ) then
    begin
      if s = 'pulse' then
      begin
        Effect := TPulse.Create;
        TPulse( Effect ).BlueMaster := StrToInt( Character.Properties[ 'ColorBlue' ] );
        TPulse( Effect ).GreenMaster := StrToInt( Character.Properties[ 'ColorGreen' ] );
        TPulse( Effect ).RedMaster := StrToInt( Character.Properties[ 'ColorRed' ] );
        Character.AddEffect( Effect );
        Effect := nil;
      end
      else
      begin
        Effect := TEffect.Create;
        if LowerCase( strTokenAt( s, '|', 1 ) ) = 'true' then
          Effect.DisableWhenDone := true;

        Effect.Resource := GetSpellEffect( strTokenAt( s, '|', 0 ) );
        Effect.AnimationDuration := Effect.Resource.FrameCount * Effect.Resource.FrameMultiplier;
        Effect.DoAction( 'Default', Character.FacingString );
        Character.AddEffect( Effect );
        Effect := nil;

      end;
    end;


  except
    on E : Exception do
      Log.log( 'Error HumanoidCaster WasKilled: ' + E.Message );

  end;

end;


(**************************************************************************************)

destructor THumanoidCasterCombat.Destroy;
const
  FailName : string = 'THumanoidCasterCombat.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    if Assigned( FriendsList ) then
      FriendsList.free;
//Should not free resources in AI
    if Assigned( FReceiveResource ) then
      FReceiveResource.free;
 //   if Assigned(CastEffect) then
//      CastEffect.free;
//    if Assigned(PushEffect) then
//      PushEffect.free;
{    if Assigned(FSummonResource) then
      FSummonResource.free;
    if Assigned(FSummonCastResource) then
      FSummonCastResource.free;
}
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function THumanoidCasterCombat.OnCollideObject(
  Target : TAniFigure ) : Boolean;
const
  FailName : string = 'THumanoidCasterCombat.OnCollideObject';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  result := false;
  try
    if Target is TDoor then
    begin
      if ( TDoor( Target ).closed ) and ( TDoor( Target ).keyname = '' ) then
        TDoor( Target ).open;
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidCasterCombat CollideObject: ' + E.Message );
  end;

end;

function THumanoidCasterCombat.InRange( Target : TAniFigure ) : Boolean;
var
  D : Double;
const
  FailName : string = 'THumanoidCasterCombat.InRange';
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

{ THumanoidHeroCombat }

procedure THumanoidHeroCombat.Execute;
const
  FailName : string = 'THumanoidHeroCombat.Execute';
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

      if Walking and Assigned( Character.track ) then
      begin
        if ( frameCount mod 10 ) = 0 then
          if not ( Character.InRange( Character.Track ) ) then
            if ( character.RangeTo( ( Character.Track ).X, ( Character.Track ).Y ) > 160 ) and AllowRun then
              Character.RunTo( ( Character.Track ).X, ( Character.Track ).Y, 64 )
            else
              Character.WalkTo( ( Character.Track ).X, ( Character.Track ).Y, 64 );
      end;

      if not ( Assigned( Character.Track ) ) then
        FindTarget
      else
        Attack;
    except
      on E : Exception do
        Log.log( 'Error HumanoidHero Execute: ' + E.Message );
    end;



  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure THumanoidHeroCombat.Attack;
const
  FailName : string = 'THumanoidHeroCombat.Attack';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    if TCharacter( Character.Track ).Dead then
    begin
      Character.Track := nil;
    end
    else if Character.InRange( Character.Track ) then
    begin
      character.Face( Character.Track.x, Character.Track.y );
      Character.Attack( Character.Track );
      delay := ( AttackDelay - TCharacter( Character.Track ).Combat );
      if Delay < 0 then
        delay := 0;
    end
    else
    begin
      if not ( Walking ) then
      begin
        Character.WalkTo( Character.Track.X, Character.Track.Y, 64 );
        Walking := true;
      end;
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidHero Attack: ' + E.Message );
  end;

end;

procedure THumanoidHeroCombat.FindTarget;
var
  List : TStringList;
  tmp, i : integer;
const
  FailName : string = 'THumanoidHeroCombat.FindTarget';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    tmp := 0;
    List := GetPerceptibleEnemies( Character, 1.5 );
    if assigned( List ) then
    begin
      if List.Count = 1 then
        Character.Track := TCharacter( List.objects[ 0 ] )
      else
      begin //really hard part
        case MainStat of
          msStrength :
            while List.Count > 1 do
            begin
              inc( tmp );
              for i := 0 to List.Count - 1 do
                if TCharacter( List.objects[ i ] ).strength < tmp then
                begin
                  list.Delete( i );
                  break;
                end;
            end;

          msHitPoints :
            while List.Count > 1 do
            begin
              inc( tmp );
              for i := 0 to List.Count - 1 do
                if TCharacter( List.objects[ i ] ).HitPoints < tmp then
                begin
                  list.Delete( i );
                  break;
                end;
            end;
          msCombat :
            while List.Count > 1 do
            begin
              inc( tmp );
              for i := 0 to List.Count - 1 do
                if TCharacter( List.objects[ i ] ).Combat < tmp then
                begin
                  list.Delete( i );
                  break;
                end;
            end;
          msMysticism :
            while List.Count > 1 do
            begin
              inc( tmp );
              for i := 0 to List.Count - 1 do
                if TCharacter( List.objects[ i ] ).Mysticism < tmp then
                begin
                  list.Delete( i );
                  break;
                end;
            end;
          msMana :
            while List.Count > 1 do
            begin
              inc( tmp );
              for i := 0 to List.Count - 1 do
                if TCharacter( List.objects[ i ] ).Mana < tmp then
                begin
                  list.Delete( i );
                  break;
                end;
            end;

        end;
        if List.Count <> 0 then
          Character.Track := TCharacter( List.objects[ 0 ] );
        Character.Say( 'Lets get to work', clWhite );

        if Assigned( Character.Track ) then
          if TCharacter( Character.Track ).Name <> '' then
            Character.Say( Character.Track.Name + ' your ass is mine!', clwhite )

      end;
      list.free;
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidHero FindTarget: ' + E.Message );
  end;

end;

procedure THumanoidHeroCombat.RunAway;
const
  FailName : string = 'THumanoidHeroCombat.RunAway';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    if assigned( Character.track ) then
      Character.Face( Character.Track.X, Character.track.Y );
    if Pos( 'E', character.FacingString ) <> 0 then
      Character.WalkTo( Character.X - 150, Character.Y + random( 300 ) - 150, 64 )
    else if Pos( 'W', character.FacingString ) <> 0 then
      Character.WalkTo( Character.X + 150, Character.Y + random( 300 ) - 150, 64 )
    else if Pos( 'SS', character.FacingString ) <> 0 then
      Character.WalkTo( Character.X + random( 300 ) - 150, Character.Y - 150, 64 )
    else
      Character.WalkTo( Character.X + random( 300 ) - 150, Character.Y + 150, 64 );
    Walking := true;
    Delay := random( 120 );
  except
    on E : Exception do
      Log.log( 'Error HumanoidHero RunAway: ' + E.Message );
  end;

end;

procedure THumanoidHeroCombat.CallToArms( Source, Target : TAniFigure );
const
  FailName : string = 'THumanoidHeroCombat.CallToArms';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    if bTakeOrders then
      Character.Track := TCharacter( Target );
  except
    on E : Exception do
      Log.log( 'Error HumanoidHero CallToArms: ' + E.Message );
  end;

end;

procedure THumanoidHeroCombat.Regroup( Source : TAniFigure; NewX, NewY : Integer );
const
  FailName : string = 'THumanoidHeroCombat.Regroup';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    exit;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;


procedure THumanoidHeroCombat.Init;
var
  S : string;
  i : integer;
  list : TstringList;
const
  FailName : string = 'THumanoidHeroCombat.Init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    GoodCollideCount := 0;
    BadCollideCount := 0;

    S := Character.Properties[ 'iSpeed' ];
    try
      if ( S <> '' ) and ( s <> '0' ) then
        TCharacterResource( character.Resource ).Speed := StrToInt( S );
    except
    end;

    S := Character.Properties[ 'AttackDelay' ];
    try
      if ( S = '' ) or ( s = '0' ) then
        AttackDelay := 25
      else
        AttackDelay := StrToInt( S );
    except
      AttackDelay := 25;
    end;
    S := LowerCase( Character.Properties[ 'AllowRun' ] );
    try
      if S = '' then
        AllowRun := true
      else if S = 'false' then
        AllowRun := False
      else
        AllowRun := true;
    except
      AllowRun := true;
    end;

    S := LowerCase( Character.Properties[ 'BalanceWithPlayer' ] );
    try
      if ( S <> '' ) and ( s <> '0' ) then
      begin
        i := StrToInt( s );
        if i >= 0 then
        begin
          if player.TitleExists( 'Apprentice' ) then
          begin
            character.Wounds := 0;
            Character.Drain := 0;
                    //    if character.Combat < (((player.mysticism* 3) div 4) +i) then
            character.Combat := ( ( ( player.mysticism * 3 ) div 4 ) + i );
                    //    if character.strength < ((player.perception div 2) +i) then
            character.strength := ( player.perception div 2 ) + i;
                   //     if character.hitpoints < ((player.Perception *2)) then
            if character.BaseHitPoints > 0 then
              character.HitPoints := ( ( player.Perception * 2 ) );

          end;
          if player.TitleExists( 'Hunter' ) then
          begin
            character.Wounds := 0;
            Character.Drain := 0;
                    //    if character.Combat < (((player.stealth* 3) div 4) +i) then
            character.Combat := ( ( ( player.stealth * 3 ) div 4 ) + i );
                       // if character.strength < ((player.Coordination div 2) +i) then
            character.strength := ( player.Coordination div 2 ) + i;
                  //      if character.hitpoints < ((player.strength *2)) then
            if character.BaseHitPoints > 0 then
              character.HitPoints := ( ( player.strength * 2 ) );

          end;
          if player.TitleExists( 'Squire' ) then
          begin
            character.Wounds := 0;
            Character.Drain := 0;
                    //    if character.Combat < (((player.combat* 3) div 4) +i) then
            character.Combat := ( ( ( player.combat * 3 ) div 4 ) + i );
                       // if character.strength < ((player.Strength div 2) +i) then
            character.strength := ( player.Strength div 2 ) + i;
                     //   if character.hitpoints < ((player.Coordination *2)) then
            if character.BaseHitPoints > 0 then
              character.HitPoints := ( ( player.Coordination * 2 ) );
          end;


          if character.Resistance.Heat.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Heat.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Cold.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Cold.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Electric.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Electric.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Magic.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Magic.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Poison.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Poison.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Mental.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Mental.Resistance := ( player.mysticism / 200 );

        end;
      end;
    except
    end;

    S := LowerCase( Character.Properties[ 'TakeOrders' ] );
    try
      if S = '' then
        bTakeOrders := true
      else if S = 'false' then
        bTakeOrders := False
      else
        bTakeOrders := true;
    except
      bTakeOrders := true;
    end;

    S := Character.Properties[ 'BaseCourage' ];
    try
      if S = '' then
        FBaseCourage := 1
      else
        FBaseCourage := StrToInt( S );
    except
      FBaseCourage := 1;
    end;

    S := Character.Properties[ 'BonusCourage' ];
    try
      if S = '' then
        FBonusCourage := 0
      else
        FBonusCourage := StrToInt( S );
    except
      FBonusCourage := 0;
    end;


    S := Character.Properties[ 'Distance' ];
    try
      if S = '' then
        iDistance := 100
      else
        iDistance := StrToInt( S );
    except
      iDistance := 100;
    end;

    S := LowerCase( Character.Properties[ 'MainStat' ] );
    try
      if S = '' then
        MainStat := msCombat
      else if S = 'strength' then
        MainStat := msStrength
      else if S = 'hitpoints' then
        MainStat := msHitPoints
      else if S = 'combat' then
        MainStat := msCombat
      else if S = 'mana' then
        MainStat := msMana
      else if S = 'mysticism' then
        MainStat := msMysticism
      else
        MainStat := msCombat
    except
      MainStat := msCombat
    end;

    PartyTotal := 1;
    List := GetPerceptibleAllies( Character, 1.5 );
    if Assigned( List ) then
    begin
      PartyTotal := List.Count;
      list.free;
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidHero Init: ' + E.Message );
  end;

  Delay := random( 60 );
end;


function THumanoidHeroCombat.OnCollideFigure( Target : TAniFigure ) : boolean;
var
  List : TStringList;
  iLoop : integer;
const
  FailName : string = 'THumanoidHeroCombat.OnCollideFigure';
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
    else if assigned( Character.Track ) then
      if not ( Character.InRange( Character.Track ) ) then
      begin
        if Character.IsEnemy( TCharacter( Target ) ) then //keep running into good guys
        begin
          inc( GoodCollideCount );
          if ( GoodCollideCount > 3 ) and Assigned( Character.track ) then
          begin
            Character.say( 'Ok you die now!', clred );
            Character.Face( target.x, target.y );

            Character.Track := TCharacter( target );
            GoodCollideCount := 0;
          end;
        end
        else
        begin
          inc( BadCollideCount );
          if ( BadCollideCount > 5 ) then
          begin
            Character.Stand;
            Character.Face( Character.Track.x, Character.Track.y );
            Character.Say( 'Get out of my way', clred );
            List := GetPerceptibleAllies( Character, 2.5 );
            if Assigned( List ) then
            begin
              for iLoop := 0 to List.count - 1 do
              begin
                TCharacter( List.Objects[ iLoop ] ).stand;
                if Pos( 'E', TCharacter( List.Objects[ iLoop ] ).FacingString ) <> 0 then
                begin
                  if Assigned( TCharacter( List.Objects[ iLoop ] ).AI ) then
                    TAI( TCharacter( List.Objects[ iLoop ] ).AI ).regroup( character, TCharacter( List.Objects[ iLoop ] ).X - 150, TCharacter( List.Objects[ iLoop ] ).Y + random( 300 ) - 150 )
                end
                else if Pos( 'W', TCharacter( List.Objects[ iLoop ] ).FacingString ) <> 0 then
                begin
                  if Assigned( TCharacter( List.Objects[ iLoop ] ).AI ) then
                    TAI( TCharacter( List.Objects[ iLoop ] ).AI ).regroup( character, TCharacter( List.Objects[ iLoop ] ).X + 150, TCharacter( List.Objects[ iLoop ] ).Y + random( 300 ) - 150 )
                end
                else if Pos( 'SS', TCharacter( List.Objects[ iLoop ] ).FacingString ) <> 0 then
                begin
                  if Assigned( TCharacter( List.Objects[ iLoop ] ).AI ) then
                    TAI( TCharacter( List.Objects[ iLoop ] ).AI ).regroup( character, TCharacter( List.Objects[ iLoop ] ).X + random( 300 ) - 150, TCharacter( List.Objects[ iLoop ] ).Y - 150 )
                end
                else if Assigned( TCharacter( List.Objects[ iLoop ] ).AI ) then
                  TAI( TCharacter( List.Objects[ iLoop ] ).AI ).regroup( character, TCharacter( List.Objects[ iLoop ] ).X + random( 300 ) - 150, TCharacter( List.Objects[ iLoop ] ).Y + 150 );
              end;
              list.free;
            end;
            BadCollideCount := 0;
            result := true;
            delay := 20;
          end;
        end;
      end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidHero CollideFigure: ' + E.Message );
  end;

end;

procedure THumanoidHeroCombat.OnStop;
const
  FailName : string = 'THumanoidHeroCombat.OnStop';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  Walking := false;
end;

procedure THumanoidHeroCombat.OnNoPath;
const
  FailName : string = 'THumanoidHeroCombat.OnNoPath';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    Walking := false;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure THumanoidHeroCombat.WasAttacked( Source : TAniFigure; Damage : single );
const
  FailName : string = 'THumanoidHeroCombat.WasAttacked';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    if Source is TCharacter then
      character.Face( TCharacter( Source ).x, TCharacter( Source ).y );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure THumanoidHeroCombat.WasKilled( Source : TAniFigure );
var
  List : TStringList;
  iLoop : integer;
const
  FailName : string = 'THumanoidHeroCombat.WasKilled';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if Source is TCharacter then
      character.Face( TCharacter( Source ).x, TCharacter( Source ).y );

    try
         //Tell everyone I died so they can save themselves
      List := GetPerceptibleAllies( Character, 1.5 );
      if Assigned( List ) then
      begin
        for iLoop := 0 to List.count - 1 do
        begin
          if Assigned( TCharacter( List.Objects[ iLoop ] ).AI ) then
            TAI( TCharacter( List.Objects[ iLoop ] ).AI ).NotifyOfDeath( character );
        end;
        list.free;
      end;
    except
      on E : Exception do
        Log.log( 'Error HumanoidHero WasKilled: ' + E.Message );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;



procedure THumanoidHeroCombat.NotifyOfDeath( Source : TAniFigure );
var
  tmpPartyTotal : integer;
  list : TStringList;
const
  FailName : string = 'THumanoidHeroCombat.NotifyOfDeath';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    tmpPartyTotal := 0;
    List := GetPerceptibleAllies( Character, 1.5 );
    if Assigned( List ) then
    begin
      tmpPartyTotal := list.Count;
      list.free;
    end;
    case FBaseCourage of
      0 :
        if ( ( ( PartyTotal div 2 ) >= tmpPartyTotal ) ) and ( Random( 99 ) < 80 ) then
          RunAway;
      1 :
        if ( ( ( PartyTotal div 2 ) >= tmpPartyTotal ) or ( THumanoidCombat( TCharacter( Source ).AI ).BaseCourage > 1 ) ) and ( Random( 99 ) < 70 ) then
          RunAway;
      2 :
        if ( ( ( PartyTotal div 2 ) > tmpPartyTotal ) or ( THumanoidCombat( TCharacter( Source ).AI ).BaseCourage > 5 ) ) and ( Random( 99 ) < 60 ) then
          RunAway;
      3 :
        if ( ( ( PartyTotal div 2 ) > tmpPartyTotal ) or ( THumanoidCombat( TCharacter( Source ).AI ).BaseCourage > 5 ) ) and ( Random( 99 ) < 50 ) then
          RunAway;
      4 :
        if ( ( ( PartyTotal div 2 ) > tmpPartyTotal ) or ( THumanoidCombat( TCharacter( Source ).AI ).BaseCourage > 6 ) ) and ( Random( 99 ) < 40 ) then
          RunAway;
      5 :
        if ( ( ( PartyTotal div 2 ) > tmpPartyTotal ) or ( THumanoidCombat( TCharacter( Source ).AI ).BaseCourage > 7 ) ) and ( Random( 99 ) < 30 ) then
          RunAway;
      6 :
        if ( ( ( PartyTotal div 2 ) > tmpPartyTotal ) or ( THumanoidCombat( TCharacter( Source ).AI ).BaseCourage > 8 ) ) and ( Random( 99 ) < 20 ) then
          RunAway;
      7 :
        if ( tmpPartyTotal = 0 ) and ( Random( 99 ) < 10 ) then
          RunAway;
      8 :
        if ( tmpPartyTotal = 0 ) and ( character.wounds >= ( Character.HitPoints * 0.75 ) ) then
          RunAway;
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidHero NotifyOfDeath: ' + E.Message );
  end;

end;

(**************************************************************************************)

destructor THumanoidHeroCombat.Destroy;
const
  FailName : string = 'THumanoidHeroCombat.Destroy';
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

function THumanoidHeroCombat.OnCollideObject( Target : TAniFigure ) : Boolean;
const
  FailName : string = 'THumanoidHeroCombat.OnCollideObject';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  result := false;
  try
    if Target is TDoor then
    begin
      if ( TDoor( Target ).closed ) and ( TDoor( Target ).keyname = '' ) then
        TDoor( Target ).open;
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidHeroCombat CollideObject: ' + E.Message );
  end;

end;

{ THumanoidHunterCombat }

procedure THumanoidHunterCombat.Execute;
const
  FailName : string = 'THumanoidHunterCombat.Execute';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if bRunaway then
      RunAway;
    if not walking then
    begin
      if Delay > 0 then
      begin
        dec( Delay );
        exit;
      end;
    end;
    inherited;
    if ( not Walking ) then
      if ( Assigned( Character.Track ) ) then
        Attack
      else
        FindTarget;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure THumanoidHunterCombat.Runaway;
const
  FailName : string = 'THumanoidHunterCombat.Runaway';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    //Better runAway code
    if assigned( Character.track ) then
    begin
      Character.Face( Character.Track.X, Character.track.Y );
    end; //From Mike
    if Pos( 'E', character.FacingString ) <> 0 then
      Character.WalkTo( Character.X - 150, Character.Y + random( 300 ) - 150, 64 )
    else if Pos( 'W', character.FacingString ) <> 0 then
      Character.WalkTo( Character.X + 150, Character.Y + random( 300 ) - 150, 64 )
    else if Pos( 'SS', character.FacingString ) <> 0 then
      Character.WalkTo( Character.X + random( 300 ) - 150, Character.Y - 150, 64 )
    else
      Character.WalkTo( Character.X + random( 300 ) - 150, Character.Y + 150, 64 );
    Walking := true;
    ReadyToAttack := false;
    bRunAway := false;
    Delay := random( 240 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure THumanoidHunterCombat.FindTarget;
var
  List : TStringList;
  i, tmp : Integer;
const
  FailName : string = 'THumanoidHunterCombat.FindTarget';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    tmp := 0;
    if ( FrameCount mod 40 ) = 0 then
    begin
      List := GetPerceptibleEnemies( Character, 1.5 );
      if assigned( List ) then
      begin
        if List.Count = 1 then
          Character.Track := TCharacter( List.objects[ 0 ] )
        else
        begin //really hard part
          case MainStat of
            msStrength :
              while List.Count > 1 do
              begin
                inc( tmp );
                for i := 0 to List.Count - 1 do
                begin
                  if TCharacter( List.objects[ i ] ).strength < tmp then
                  begin
                    list.Delete( i );
                    break;
                  end;
                end;
              end;

            msHitPoints :
              while List.Count > 1 do
              begin
                inc( tmp );
                for i := 0 to List.Count - 1 do
                begin
                  if TCharacter( List.objects[ i ] ).strength < tmp then
                  begin
                    list.Delete( i );
                    break;
                  end;
                end;
              end;
            msCombat :
              while List.Count > 1 do
              begin
                inc( tmp );
                for i := 0 to List.Count - 1 do
                begin
                  if TCharacter( List.objects[ i ] ).strength < tmp then
                  begin
                    list.Delete( i );
                    break;
                  end;
                end;
              end;
            msMysticism :
              while List.Count > 1 do
              begin
                inc( tmp );
                for i := 0 to List.Count - 1 do
                begin
                  if TCharacter( List.objects[ i ] ).Mysticism < tmp then
                  begin
                    list.Delete( i );
                    break;
                  end;
                end;
              end;
            msMana :
              while List.Count > 1 do
              begin
                inc( tmp );
                for i := 0 to List.Count - 1 do
                begin
                  if TCharacter( List.objects[ i ] ).Mana < tmp then
                  begin
                    list.Delete( i );
                    break;
                  end;
                end;
              end;

          end;
          if List.Count <> 0 then
            Character.Track := TCharacter( List.objects[ 0 ] );
          if Assigned( Character.Track ) then
            if TCharacter( Character.Track ).Name <> '' then
              Character.Say( Character.Track.Name + ' your ass is mine!', clwhite );
          list.free;
        end;
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure THumanoidHunterCombat.Attack;
var
  i : integer;
const
  FailName : string = 'THumanoidHunterCombat.Attack';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if RangeTest( Character.Track, Character, iDistance ) then
    begin
      if RangeAttack then
      begin
        RangeAttack := False;
        if assigned( Character.Equipment[ slWeapon ] ) then
        begin
          Character.Inventory.add( Character.Equipment[ slWeapon ] );
          for i := 0 to Character.Inventory.Count - 1 do
          begin
            if TGameObject( Character.Inventory.items[ i ] ) is TWeapon then
            begin
              Character.Equipment[ slWeapon ] := TWeapon( Character.Inventory.items[ i ] );
              break;
            end;
          end;
          Character.Equipment[ slWeapon ] := nil;
        end;
         //Switch weapons here
      end;
    end
    else
    begin
      if not RangeAttack and bHasBow then
      begin
         //RangeAttack := True;
        for i := 0 to Character.Inventory.Count - 1 do
        begin
          if TGameObject( Character.Inventory.items[ i ] ) is TBow then
          begin
                  {If Character.findfreeInventoryXY(TWeapon(Character.Equipment[slWeapon]) then
                  begin }
            Character.Inventory.add( Character.Equipment[ slWeapon ] );
            Character.Equipment[ slWeapon ] := TWeapon( Character.Inventory.items[ i ] );
            bHasBow := True;
            RangeAttack := True;
                  {end
                  else //Cant switch to Bow
                  begin
                       RangeAttack := False
                       bHasBow := False; //to keep from trying to switch to bow.
                  end; }
            break;
          end;
        end;
         //Switch weapons here
      end;
    end;
    if TCharacter( Character.Track ).Dead then
    begin
      Character.Track := nil;
    end
    else
    begin
      character.Face( Character.Track.x, Character.Track.y );
      if RangeAttack then
        Character.AttackPoint( Character.Track.X, Character.Track.y + 50 )
      else
      begin
        if Character.InRange( Character.Track ) then
        begin
          Character.Attack( Character.Track );
          delay := ( AttackDelay - TCharacter( Character.Track ).Combat );
          if Delay < 0 then
            delay := 0;
        end
        else
        begin
          Delay := random( 40 );
          Character.WalkTo( Character.Track.X, Character.Track.Y, 64 );
          Walking := true;
        end;
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure THumanoidHunterCombat.CallToArms( Source, Target : TAniFigure );
const
  FailName : string = 'THumanoidHunterCombat.CallToArms';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if bTakeOrders then
      Character.Track := TCharacter( Target );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure THumanoidHunterCombat.Regroup( Source : TAniFigure; NewX, NewY : Integer );
const
  FailName : string = 'THumanoidHunterCombat.Regroup';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    exit;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;


procedure THumanoidHunterCombat.Init;
var
  S : string;
  i : integer;
  list : TstringList;
const
  FailName : string = 'THumanoidHunterCombat.Init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    RangeAttack := true;
    bHasBow := True;

    S := Character.Properties[ 'AttackDelay' ];
    try
      if ( S = '' ) or ( s = '0' ) then
        AttackDelay := 25
      else
        AttackDelay := StrToInt( S );
    except
      AttackDelay := 25;
    end;

    S := LowerCase( Character.Properties[ 'BalanceWithPlayer' ] );
    try
      if ( S <> '' ) and ( s <> '0' ) then
      begin
        i := StrToInt( s );
        if i >= 0 then
        begin
          if player.TitleExists( 'Apprentice' ) then
          begin
            character.Wounds := 0;
            Character.Drain := 0;
                     //   if character.Combat < (((player.mysticism* 3) div 4) +i) then
            character.Combat := ( ( ( player.mysticism * 3 ) div 4 ) + i );
                      //  if character.strength < ((player.perception div 2) +i) then
            character.strength := ( player.perception div 2 ) + i;
                     //   if character.Coordination < (player.perception +i) then
            character.Coordination := player.perception + i;
                    //    if character.Stealth < (player.Mysticism +i) then
            character.Stealth := player.Mysticism + i;
                     //   if character.hitpoints < ((player.Perception *2)) then
            if character.BaseHitPoints > 0 then
              character.HitPoints := ( ( player.Perception * 2 ) );
          end;
          if player.TitleExists( 'Hunter' ) then
          begin
            character.Wounds := 0;
            Character.Drain := 0;
                   //     if character.Combat < (((player.combat* 3) div 4) +i) then
            character.Combat := ( ( ( player.combat * 3 ) div 4 ) + i );
                      //  if character.strength < ((player.Strength div 2) +i) then
            character.strength := ( player.Strength div 2 ) + i;
                     //   if character.Coordination < (player.Coordination +i) then
            character.Coordination := player.Coordination + i;
                      //  if character.Stealth < (player.Stealth +i) then
            character.Stealth := player.Stealth + i;
                      //  if character.hitpoints < ((player.strength *2)) then
            if character.BaseHitPoints > 0 then
              character.HitPoints := ( ( player.strength * 2 ) );
          end;
          if player.TitleExists( 'Squire' ) then
          begin
            character.Wounds := 0;
            Character.Drain := 0;
                    //    if character.Combat < (((player.Combat* 3) div 4) +i) then
            character.Combat := ( ( ( player.combat * 3 ) div 4 ) + i );
                       // if character.strength < ((player.Coordination div 2) +i) then
            character.strength := ( player.Coordination div 2 ) + i;
                     //   if character.Coordination < (player.strength +i) then
            character.Coordination := player.strength + i;
                     //   if character.Stealth < (player.Combat +i) then
            character.Stealth := player.Combat + i;

                 //       if character.hitpoints < ((player.Coordination *2)) then
            if character.BaseHitPoints > 0 then
              character.HitPoints := ( ( player.Coordination * 2 ) );
          end;

          if character.Resistance.Heat.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Heat.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Cold.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Cold.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Electric.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Electric.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Magic.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Magic.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Poison.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Poison.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Mental.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Mental.Resistance := ( player.mysticism / 200 );

        end;
      end;
    except
    end;

    S := LowerCase( Character.Properties[ 'TakeOrders' ] );
    try
      if S = '' then
        bTakeOrders := true
      else if S = 'false' then
        bTakeOrders := False
      else
        bTakeOrders := true;
    except
      bTakeOrders := true;
    end;

    S := Character.Properties[ 'iSpeed' ];
    try
      if ( S <> '' ) and ( s <> '0' ) then
        TCharacterResource( character.Resource ).Speed := StrToInt( S );
    except
    end;

    S := Character.Properties[ 'BaseCourage' ];
    try
      if S = '' then
        FBaseCourage := 1
      else
        FBaseCourage := StrToInt( S );
    except
      FBaseCourage := 1;
    end;

    S := Character.Properties[ 'BonusCourage' ];
    try
      if S = '' then
        FBonusCourage := 0
      else
        FBonusCourage := StrToInt( S );
    except
      FBonusCourage := 0;
    end;


    S := Character.Properties[ 'Distance' ];
    try
      if S = '' then
        iDistance := 100
      else
        iDistance := StrToInt( S );
    except
      iDistance := 100;
    end;

    S := LowerCase( Character.Properties[ 'MainStat' ] );
    try
      if S = '' then
        MainStat := msCombat
      else if S = 'strength' then
        MainStat := msStrength
      else if S = 'hitpoints' then
        MainStat := msHitPoints
      else if S = 'combat' then
        MainStat := msCombat
      else if S = 'mana' then
        MainStat := msMana
      else if S = 'mysticism' then
        MainStat := msMysticism
      else
        MainStat := msCombat
    except
      MainStat := msCombat
    end;



    List := GetPerceptibleAllies( Character, 1.5 );
    if Assigned( List ) then
    begin
      PartyTotal := list.Count;
      list.free;
    end;

    Delay := random( 60 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;


function THumanoidHunterCombat.OnCollideFigure( Target : TAniFigure ) : boolean;
var
  List : TStringList;
  iLoop : integer;
const
  FailName : string = 'THumanoidHunterCombat.OnCollideFigure';
begin
  Result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if Target = Character.Track then
    begin
      ReadyToAttack := True;
      Result := True;
    end
    else if assigned( Character.Track ) and not ( Character.InRange( Character.Track ) ) then
    begin
      inc( CollideCount );
      if ( CollideCount > 5 ) then
      begin
        Character.Stand;
        Character.Face( Character.Track.x, Character.Track.y );
        Character.Say( 'Get out of my way', clred );
        List := GetPerceptibleAllies( Character, 2.5 );
        if Assigned( List ) then
        begin
          for iLoop := 0 to List.count - 1 do
          begin
            TCharacter( List.Objects[ iLoop ] ).stand;
            if Pos( 'E', TCharacter( List.Objects[ iLoop ] ).FacingString ) <> 0 then
            begin
              if Assigned( TCharacter( List.Objects[ iLoop ] ).AI ) then
                TAI( TCharacter( List.Objects[ iLoop ] ).AI ).regroup( character, TCharacter( List.Objects[ iLoop ] ).X - 150, TCharacter( List.Objects[ iLoop ] ).Y + random( 300 ) - 150 )
            end
            else if Pos( 'W', TCharacter( List.Objects[ iLoop ] ).FacingString ) <> 0 then
            begin
              if Assigned( TCharacter( List.Objects[ iLoop ] ).AI ) then
                TAI( TCharacter( List.Objects[ iLoop ] ).AI ).regroup( character, TCharacter( List.Objects[ iLoop ] ).X + 150, TCharacter( List.Objects[ iLoop ] ).Y + random( 300 ) - 150 )
            end
            else if Pos( 'SS', TCharacter( List.Objects[ iLoop ] ).FacingString ) <> 0 then
            begin
              if Assigned( TCharacter( List.Objects[ iLoop ] ).AI ) then
                TAI( TCharacter( List.Objects[ iLoop ] ).AI ).regroup( character, TCharacter( List.Objects[ iLoop ] ).X + random( 300 ) - 150, TCharacter( List.Objects[ iLoop ] ).Y - 150 )
            end
            else if Assigned( TCharacter( List.Objects[ iLoop ] ).AI ) then
              TAI( TCharacter( List.Objects[ iLoop ] ).AI ).regroup( character, TCharacter( List.Objects[ iLoop ] ).X + random( 300 ) - 150, TCharacter( List.Objects[ iLoop ] ).Y + 150 );

          end;
          list.free;
        end;
        CollideCount := 0;
        result := true;
        delay := 20;
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure THumanoidHunterCombat.OnNoPath;
const
  FailName : string = 'THumanoidHunterCombat.OnNoPath';
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

procedure THumanoidHunterCombat.OnStop;
const
  FailName : string = 'THumanoidHunterCombat.OnStop';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    Walking := false;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure THumanoidHunterCombat.WasAttacked( Source : TAniFigure; Damage : single );
const
  FailName : string = 'THumanoidHunterCombat.WasAttacked';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    if Source is TCharacter then
      character.Face( TCharacter( Source ).x, TCharacter( Source ).y );


  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure THumanoidHunterCombat.WasKilled( Source : TAniFigure );
var
  List : TStringList;
  iLoop : integer;
const
  FailName : string = 'THumanoidHunterCombat.WasKilled';
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
      for iLoop := 0 to List.count - 1 do
      begin
        if Assigned( TCharacter( List.Objects[ iLoop ] ).AI ) then
          TAI( TCharacter( List.Objects[ iLoop ] ).AI ).NotifyOfDeath( character );
      end;
      list.free;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;



procedure THumanoidHunterCombat.NotifyOfDeath( Source : TAniFigure );
var
  tmpPartyTotal : integer;
  list : TStringList;
const
  FailName : string = 'THumanoidHunterCombat.NotifyOfDeath';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    tmpPartyTotal := 0;
    List := GetPerceptibleAllies( Character, 1.5 );
    if Assigned( List ) then
    begin
      tmpPartyTotal := list.Count;
      list.free;
    end;
    case FBaseCourage of
      0 :
        if ( ( ( PartyTotal div 2 ) >= tmpPartyTotal ) ) and ( Random( 9 ) < 8 ) then
          bRunAway := true;
      1 :
        if ( ( ( PartyTotal div 2 ) >= tmpPartyTotal ) or ( THumanoidCombat( TCharacter( Source ).AI ).BaseCourage > 1 ) ) and ( Random( 9 ) < 7 ) then
          bRunAway := true;
      2 :
        if ( ( ( PartyTotal div 2 ) > tmpPartyTotal ) or ( THumanoidCombat( TCharacter( Source ).AI ).BaseCourage > 5 ) ) and ( Random( 9 ) < 6 ) then
          bRunAway := true;
      3 :
        if ( ( ( PartyTotal div 2 ) > tmpPartyTotal ) or ( THumanoidCombat( TCharacter( Source ).AI ).BaseCourage > 5 ) ) and ( Random( 9 ) < 5 ) then
          bRunAway := true;
      4 :
        if ( ( ( PartyTotal div 2 ) > tmpPartyTotal ) or ( THumanoidCombat( TCharacter( Source ).AI ).BaseCourage > 6 ) ) and ( Random( 9 ) < 4 ) then
          bRunAway := true;
      5 :
        if ( ( ( PartyTotal div 2 ) > tmpPartyTotal ) or ( THumanoidCombat( TCharacter( Source ).AI ).BaseCourage > 7 ) ) and ( Random( 9 ) < 3 ) then
          bRunAway := true;
      6 :
        if ( ( ( PartyTotal div 2 ) > tmpPartyTotal ) or ( THumanoidCombat( TCharacter( Source ).AI ).BaseCourage > 8 ) ) and ( Random( 9 ) < 2 ) then
          bRunAway := true;
      7 :
        if ( tmpPartyTotal = 0 ) and ( Random( 9 ) < 1 ) then
          bRunAway := true;
      8 :
        if ( tmpPartyTotal = 0 ) and ( character.wounds >= ( Character.HitPoints * 0.75 ) ) then
          bRunAway := true;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;
(*************************************************************************)

destructor THumanoidHunterCombat.Destroy;
const
  FailName : string = 'THumanoidHunterCombat.Destroy';
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

function THumanoidHunterCombat.OnCollideObject(
  Target : TAniFigure ) : Boolean;
const
  FailName : string = 'THumanoidHunterCombat.OnCollideObject';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  result := false;
  try
    if Target is TDoor then
    begin
      if ( TDoor( Target ).closed ) and ( TDoor( Target ).keyname = '' ) then
        TDoor( Target ).open;
    end;
  except
    on E : Exception do
      Log.log( 'Error HumanoidHunterCombat CollideObject: ' + E.Message );
  end;

end;





{ THumanoidCombat }


destructor THumanoidCombat.Destroy;
const
  FailName : string = 'THumanoidCombat.Destroy';
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

procedure THumanoidCombat.WasAttacked( Source : TAniFigure; Damage : Single );
const
  FailName : string = 'THumanoidCombat.WasAttacked';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if random( 6 ) = 0 then
      inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure THumanoidCombat.WasKilled( Source : TAniFigure );
const
  FailName : string = 'THumanoidCombat.WasKilled';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if random( 3 ) = 0 then
      inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;
(*****************************************************************************)
{ THaloEffect }

procedure THaloEffect.Adjust( Character : TCharacter );
begin
  inherited;
end;

constructor THaloEffect.Create;
var
  i : integer;
  p : ^TSwirlBead;
begin
  inherited;
  AnimationDuration := 12;
  Resource := TLightning( Lightning ).SmallResource;
  Points := 6;
  GetMem( PointList, Points * sizeof( TSwirlBead ) );
  p := PointList;
  for i := 1 to Points do
  begin
    p^.X := 0;
    p^.Y := 0;
    p^.Z := 160;
//    p^.Z:=random*10;

    p^.Angle := i;
//    p^.Angle:=random*PI2;
    p^.Rise := 2.5;
//    p^.Offset:=Round(360 div i);
    p^.Offset := 15;
    inc( p );
  end;
end;

destructor THaloEffect.Destroy;
begin
  FreeMem( PointList );
  inherited;
end;

function THaloEffect.DoFrame : boolean;
var
  i : integer;
  p : ^TSwirlBead;
begin
  result := false;
  if not result then
  begin
    p := PointList;
    for i := 1 to Points do
    begin
      p^.Angle := p^.Angle - 30 / 180;
      p^.Z := 160;
//      p^.Z:=p^.Z+p^.Rise;
      p^.X := ( 50 + p^.Offset ) * cos( p^.Angle ) / 4;
      p^.Y := ( 50 + p^.Offset ) * sin( p^.Angle ) / 4;
      inc( p );
    end;
  end;
end;

procedure THaloEffect.RenderLocked( Figure : TAniFigure; Bits : PBITPLANE );
var
  i : integer;
  p : ^TSwirlBead;
  Blend : integer;
  Color : integer;
begin
  p := PointList;
  for i := 1 to Points do
  begin
    Blend := 100;
//    Blend:=100-round(p^.Z/2);
    Color := 25 * i;
//    Color:=round(p^.Z*2);
    if Color > 200 then
      Color := 200;
    if Blend > 0 then
      Resource.RLE.DrawColorize( 5, Figure.CenterX + round( p^.X ), Figure.CenterY + round( ( p^.Y - p^.Z ) / 2 ),
        Bits, 200 - Color, Color, 50, Blend, 100 );
    inc( p );
  end;
end;

(**************************************************************************************)


procedure TInvisEffect.Adjust( Character : TCharacter );
begin
  inherited;

  if Character.SpecialEffect = seNone then
  begin
    Character.SpecialEffect := seTranslucent;
    Character.Alpha := 25;
    Character.Highlightable := false;
    Character.addtitle( 'cancelinvis' );
  end;
  FCharacter := Character;

end;

destructor TInvisEffect.Destroy;
begin
  FCharacter.SpecialEffect := seNone;
  FCharacter.Highlightable := true;
  FCharacter.alpha := 100;
  inherited;
end;

function TInvisEffect.DoFrame : boolean;
begin
  result := inherited DoFrame;
  if not ( Fcharacter.TitleExists( 'cancelinvis' ) ) then
  begin
    result := true;
  end;
end;

(**************************************************************************)

procedure TShieldSpellEffect.Adjust( Character : TCharacter );
begin
  inherited;
  Character.addtitle( 'cancelspelleffect' );
  FCharacter := Character;
end;

destructor TShieldSpellEffect.Destroy;
begin
  inherited;
end;

constructor TShieldSpellEffect.Create;
begin
  inherited;
end;

function TShieldSpellEffect.DoFrame : boolean;
begin
  result := false;
  try
  //  log.log('Jah : ' + IntToStr(AnimationDuration));

    if ( AnimationDuration = 1 ) and Assigned( Resource ) then
    begin
      AnimationDuration := Resource.FrameCount * ( Resource.FrameMultiplier );
      DoAction( 'Default', FCharacter.FacingString );
    end;

    inherited DoFrame;

    if not ( FCharacter.TitleExists( 'cancelspelleffect' ) ) or ( FCharacter.dead ) then
    begin
      AnimationDuration := 0;
      result := true;
    end;


  except
    on E : Exception do
      Log.log( 'Error TShieldSpell DoFrame: ' + E.Message );
  end;
end;

(**************************************************************************************)


procedure TColorMeEffect.Adjust( Character : TCharacter );
begin
  inherited;
end;

destructor TColorMeEffect.Destroy;
begin
  inherited;
end;

function TColorMeEffect.DoFrame : boolean;
begin
  result := false;
end;


{ TGlowEffect }

procedure TGlowEffect.Adjust( Character : TCharacter );
begin
  inherited;
  Character.addtitle( 'cancelspelleffect' );
  FCharacter := Character;
end;

constructor TGlowEffect.Create;
begin
  inherited;

  // DrawColorize(0,0,0,@Bits,RFactor,GFactor,BFactor,Alpha,100);

end;

destructor TGlowEffect.Destroy;
begin
  inherited;
end;

function TGlowEffect.DoFrame : boolean;
begin
  result := false;
  try
  //  log.log('Jah : ' + IntToStr(AnimationDuration));
    if ( AnimationDuration = 1 ) and Assigned( Resource ) then
    begin
      AnimationDuration := Resource.FrameCount * ( Resource.FrameMultiplier );
      DoAction( 'Default', FCharacter.FacingString );
    end;

    inherited DoFrame;

    if not ( FCharacter.TitleExists( 'cancelspelleffect' ) ) then
    begin
      result := true;
    end;

  except
    on E : Exception do
      Log.log( 'Error TShieldSpell DoFrame: ' + E.Message );
  end;
end;

procedure TGlowEffect.RenderLocked( Figure : TAniFigure; Bits : PBITPLANE );
begin
  inherited;

end;

{ TPulse }

procedure TPulse.Adjust( Character : TCharacter );
begin
  FCharacter := Character;
//  FCharacter.ColorB:=150;
  Red := FCharacter.ColorR;
  Blue := FCharacter.ColorB;
  Green := FCharacter.ColorG;

end;

constructor TPulse.Create;
begin
  inherited;
  RedDelta := RedMaster;
  BlueDelta := BlueMaster;
  GreenDelta := GreenMaster;
end;

function TPulse.DoFrame : boolean;
begin
  result := false;
  if RedMaster <> 0 then
  begin
    inc( Red, RedDelta );
    if Red >= 400 then
      RedDelta := -RedMaster
    else if Red <= 100 then
      RedDelta := RedMaster;
  end;

  if BlueMaster <> 0 then
  begin
    inc( Blue, BlueDelta );
    if Blue >= 400 then
      BlueDelta := -BlueMaster
    else if Blue <= 100 then
      BlueDelta := BlueMaster;
  end;

  if GreenMaster <> 0 then
  begin
    inc( Green, GreenDelta );
    if Green >= 400 then
      GreenDelta := -GreenMaster
    else if Green <= 100 then
      GreenDelta := GreenMaster;
  end;

  if {not(FCharacter.TitleExists('cancelspelleffect')) or}( FCharacter.dead ) then
  begin
    result := true;
  end;

  FCharacter.ColorR := Red;
  FCharacter.ColorB := Blue;

end;

{ TProtectMeEffect }

procedure TProtectMeEffect.Adjust( Character : TCharacter );
begin
  player.addtitle( 'protectme' );
  Player.Resistance.Cold.Resistance := 1;
  Player.Resistance.Heat.Resistance := 1;
  Player.Resistance.Cutting.Resistance := 1;
  Player.Resistance.Crushing.Resistance := 1;
  Player.Resistance.Piercing.Resistance := 1;
end;

destructor TProtectMeEffect.Destroy;
begin

end;

function TProtectMeEffect.DoFrame : boolean;
begin
  result := false;
  if not ( player.titleExists( 'protectme' ) ) then
    result := true;
end;

end.
