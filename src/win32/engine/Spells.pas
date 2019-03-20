unit Spells;
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

interface

uses
  Classes,
  Windows,
  SysUtils,
  Anigrp30,
{$IFDEF DirectX}
  DirectX,
  DXUtil,
  DXEffects,
{$ENDIF}
  digifx,
  DFX,
  Engine,
  Character,
  Resource,
  LogFile;

var
  AllSpellList : TStringList;
  AllCastResourceList : TList;
  Fireball, Frostball, Lightning, Shrapnel, Push, Healing, Charge,
    SummonRat, SummonWolf, ProtectionFire, ProtectionCold,
    ProtectionElectricity, ProtectionPoison, ProtectionMagic,
    ProtectionAll, AuraOfIron, AuraOfSteel, Shadow, Hold, DeathSpell,
    Forget, ManaThief, GreatHand, GreatWolf, FlameStrike, Blizzard,
    Reflect, Firefly : TSpell;

  FireEffect : TResource;
  PushEffect : TResource;
  FrostEffect : TResource;
  LightningEffect : TResource;
  HealEffect : TResource;
  ProtectionEffect : TResource;
  ShrapnelEffect : TResource;
  ChargeEffect : TResource;
  SummonEffect : TResource;
  HoldEffect : TResource;
  LichEffect : TResource;
  AuraEffect : TResource;
  ManaThiefEffect : TResource;
  GreatHandEffect : TResource;
  GreatWolfEffect : TResource;
  BigFire : TResource;
  BigIce : TResource;
  ReflectEffect : TResource;

type
  TFireball = class( TSpell )
  private
    Resource : TResource;
    Resource2 : TResource;
    Resource3 : TResource;
    Resource4 : TResource;
    Resource5 : TResource;
    Resource6 : TResource;
    Resource7 : TResource;
  protected
    function GetLoaded : Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function GetName : string; override;
    procedure Casting( Source : TCharacter ); override;
    function Range( Source : TCharacter ) : Integer; override;
    function Recovery( Source : TCharacter ) : Integer; override;
    function Drain( Source : TCharacter ) : Single; override;
    function Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean; override;
    function GetIconXY( Source : TCharacter ) : TPoint; override;
    function GetInfo( Source : TCharacter ) : string; override;
    procedure Clear; override;
  end;

  TFrostBall = class( TSpell )
  private
    Resource : TResource;
    SmallResource : TResource;
  protected
    function GetLoaded : Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function GetName : string; override;
    procedure Casting( Source : TCharacter ); override;
    function Range( Source : TCharacter ) : Integer; override;
    function Recovery( Source : TCharacter ) : Integer; override;
    function Drain( Source : TCharacter ) : Single; override;
    function Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean; override;
    function GetIconXY( Source : TCharacter ) : TPoint; override;
    function GetInfo( Source : TCharacter ) : string; override;
    procedure Clear; override;
  end;

  TPush = class( TSpell )
  protected
    function GetLoaded : Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function GetName : string; override;
    function Range( Source : TCharacter ) : Integer; override;
    function Recovery( Source : TCharacter ) : Integer; override;
    function Drain( Source : TCharacter ) : Single; override;
    function Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean; override;
    function GetIconXY( Source : TCharacter ) : TPoint; override;
    function GetInfo( Source : TCharacter ) : string; override;
  end;

  THeal = class( TSpell )
  private
    Resource : TResource;
  protected
    function GetLoaded : Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function GetName : string; override;
    function Range( Source : TCharacter ) : Integer; override;
    function Recovery( Source : TCharacter ) : Integer; override;
    function Drain( Source : TCharacter ) : Single; override;
    function Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean; override;
    function GetIconXY( Source : TCharacter ) : TPoint; override;
    function GetInfo( Source : TCharacter ) : string; override;
    procedure Clear; override;
  end;

  TProtection = class( TSpell )
  private
    Resource : TResource;
    MinorResistance : TDamageResistanceProfile;
    MajorResistance : TDamageResistanceProfile;
    IconXY : TPoint;
    R, G, B : integer;
    ResourceOwner : boolean;
    tag : integer;
  protected
    function GetLoaded : Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Range( Source : TCharacter ) : Integer; override;
    function Recovery( Source : TCharacter ) : Integer; override;
    function Drain( Source : TCharacter ) : Single; override;
    function Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean; override;
    function GetIconXY( Source : TCharacter ) : TPoint; override;
    function GetInfo( Source : TCharacter ) : string; override;
    procedure Clear; override;
  end;

  TProtectionFire = class( TProtection )
  public
    constructor Create; override;
    class function GetName : string; override;
  end;

  TProtectionCold = class( TProtection )
  public
    constructor Create; override;
    class function GetName : string; override;
  end;

  TProtectionElectricity = class( TProtection )
  public
    constructor Create; override;
    class function GetName : string; override;
  end;

  TProtectionPoison = class( TProtection )
  public
    constructor Create; override;
    class function GetName : string; override;
  end;

  TProtectionMagic = class( TProtection )
  public
    constructor Create; override;
    class function GetName : string; override;
  end;

  TProtectionAll = class( TProtection )
  public
    constructor Create; override;
    class function GetName : string; override;
    function GetInfo( Source : TCharacter ) : string; override;
  end;

  TAura = class( TSpell )
  private
    Resource : TResource;
    HitResource : TResource;
    Resistance : TDamageResistanceProfile;
    IconXY : TPoint;
    FDrain : single;
    ColorR, ColorG, ColorB : integer;
  protected
    function GetLoaded : Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function Range( Source : TCharacter ) : Integer; override;
    function Recovery( Source : TCharacter ) : Integer; override;
    function Drain( Source : TCharacter ) : Single; override;
    function Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean; override;
    function GetIconXY( Source : TCharacter ) : TPoint; override;
  end;

  TAuraOfIron = class( TAura )
  public
    class function GetName : string; override;
    constructor Create; override;
    function GetInfo( Source : TCharacter ) : string; override;
    procedure Clear; override;
  end;

  TAuraOfSteel = class( TAura )
  public
    class function GetName : string; override;
    constructor Create; override;
    function GetInfo( Source : TCharacter ) : string; override;
    procedure Clear; override;
  end;

  TShadow = class( TSpell )
    Resource : TResource;
    ResourceOwner : boolean;
    function GetLoaded : Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function GetName : string; override;
    function Range( Source : TCharacter ) : Integer; override;
    function Recovery( Source : TCharacter ) : Integer; override;
    function Drain( Source : TCharacter ) : Single; override;
    function Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean; override;
    function GetIconXY( Source : TCharacter ) : TPoint; override;
    function GetInfo( Source : TCharacter ) : string; override;
  end;

  THold = class( TSpell )
    Resource : TResource;
    function GetLoaded : Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function GetName : string; override;
    function Range( Source : TCharacter ) : Integer; override;
    function Recovery( Source : TCharacter ) : Integer; override;
    function Drain( Source : TCharacter ) : Single; override;
    function Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean; override;
    function GetIconXY( Source : TCharacter ) : TPoint; override;
    function GetInfo( Source : TCharacter ) : string; override;
    procedure Clear; override;
  end;

  TLightning = class( TSpell )
  private
    Resource1 : TResource;
    Resource2 : TResource;
    Resource3 : TResource;
    ReceiveResource : TResource;
  protected
    function GetLoaded : Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function GetName : string; override;
    function Range( Source : TCharacter ) : Integer; override;
    function Recovery( Source : TCharacter ) : Integer; override;
    function Drain( Source : TCharacter ) : Single; override;
    function Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean; override;
    function GetIconXY( Source : TCharacter ) : TPoint; override;
    function GetInfo( Source : TCharacter ) : string; override;
    property SmallResource : TResource read Resource1;
    property MediumResource : TResource read Resource2;
    property LargeResource : TResource read Resource3;
    procedure Clear; override;
  end;

  TShrapnel = class( TSpell )
  private
    Resource : TResource;
  protected
    function GetLoaded : Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function GetName : string; override;
    function Range( Source : TCharacter ) : Integer; override;
    function Recovery( Source : TCharacter ) : Integer; override;
    function Drain( Source : TCharacter ) : Single; override;
    function Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean; override;
    function GetIconXY( Source : TCharacter ) : TPoint; override;
    function GetInfo( Source : TCharacter ) : string; override;
    procedure Clear; override;
  end;

  TWither = class( TSpell )
  private
    Resource : TResource;
  protected
    function GetLoaded : Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function GetName : string; override;
    function Range( Source : TCharacter ) : Integer; override;
    function Recovery( Source : TCharacter ) : Integer; override;
    function Drain( Source : TCharacter ) : Single; override;
    function Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean; override;
    function GetIconXY( Source : TCharacter ) : TPoint; override;
    function GetInfo( Source : TCharacter ) : string; override;
    procedure Clear; override;
  end;

  TCharge = class( TSpell )
  private
    Resource1 : TResource;
    Resource2 : TResource;
    Resource3 : TResource;
  protected
    function GetLoaded : Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function GetName : string; override;
    function Range( Source : TCharacter ) : Integer; override;
    function Recovery( Source : TCharacter ) : Integer; override;
    function Drain( Source : TCharacter ) : Single; override;
    function Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean; override;
    function GetIconXY( Source : TCharacter ) : TPoint; override;
    function GetInfo( Source : TCharacter ) : string; override;
  end;

  TSummonRat = class( TSpell )
  private
    Resource : TResource;
  protected
    function GetLoaded : Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function GetName : string; override;
    function Range( Source : TCharacter ) : Integer; override;
    function Recovery( Source : TCharacter ) : Integer; override;
    function Drain( Source : TCharacter ) : Single; override;
    function Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean; override;
    function GetIconXY( Source : TCharacter ) : TPoint; override;
    function GetInfo( Source : TCharacter ) : string; override;
  end;

  TSummonWolf = class( TSpell )
  private
    Resource : TResource;
  protected
    function GetLoaded : Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function GetName : string; override;
    function Range( Source : TCharacter ) : Integer; override;
    function Recovery( Source : TCharacter ) : Integer; override;
    function Drain( Source : TCharacter ) : Single; override;
    function Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean; override;
    function GetIconXY( Source : TCharacter ) : TPoint; override;
    function GetInfo( Source : TCharacter ) : string; override;
  end;

  TDeathSpell = class( TSpell )
  private
    Resource : TResource;
  protected
    function GetLoaded : Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function GetName : string; override;
    function Range( Source : TCharacter ) : Integer; override;
    function Recovery( Source : TCharacter ) : Integer; override;
    function Drain( Source : TCharacter ) : Single; override;
    function Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean; override;
    function GetIconXY( Source : TCharacter ) : TPoint; override;
    function GetInfo( Source : TCharacter ) : string; override;
    procedure Clear; override;
  end;

  TForget = class( TSpell )
    Resource : TResource;
    function GetLoaded : Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function GetName : string; override;
    function Range( Source : TCharacter ) : Integer; override;
    function Recovery( Source : TCharacter ) : Integer; override;
    function Drain( Source : TCharacter ) : Single; override;
    function Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean; override;
    function GetIconXY( Source : TCharacter ) : TPoint; override;
    function GetInfo( Source : TCharacter ) : string; override;
  end;

  TManaThief = class( TSpell )
    Resource : TResource;
    function GetLoaded : Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function GetName : string; override;
    function Range( Source : TCharacter ) : Integer; override;
    function Recovery( Source : TCharacter ) : Integer; override;
    function Drain( Source : TCharacter ) : Single; override;
    function Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean; override;
    function GetIconXY( Source : TCharacter ) : TPoint; override;
    function GetInfo( Source : TCharacter ) : string; override;
    procedure Clear; override;
  end;

  TGreatHand = class( TSpell )
  private
    Resource : TResource;
  protected
    function GetLoaded : Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function GetName : string; override;
    function Range( Source : TCharacter ) : Integer; override;
    function Recovery( Source : TCharacter ) : Integer; override;
    function Drain( Source : TCharacter ) : Single; override;
    function Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean; override;
    function GetIconXY( Source : TCharacter ) : TPoint; override;
    function GetInfo( Source : TCharacter ) : string; override;
    procedure Clear; override;
  end;

  TGreatWolf = class( TSpell )
  private
    Resource : TResource;
  protected
    function GetLoaded : Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function GetName : string; override;
    function Range( Source : TCharacter ) : Integer; override;
    function Recovery( Source : TCharacter ) : Integer; override;
    function Drain( Source : TCharacter ) : Single; override;
    function Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean; override;
    function GetIconXY( Source : TCharacter ) : TPoint; override;
    function GetInfo( Source : TCharacter ) : string; override;
    procedure Clear; override;
  end;

  TFlameStrike = class( TSpell )
  private
    Resource : TResource;
  protected
    function GetLoaded : Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function GetName : string; override;
    procedure Casting( Source : TCharacter ); override;
    function Range( Source : TCharacter ) : Integer; override;
    function Recovery( Source : TCharacter ) : Integer; override;
    function Drain( Source : TCharacter ) : Single; override;
    function Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean; override;
    function GetIconXY( Source : TCharacter ) : TPoint; override;
    function GetInfo( Source : TCharacter ) : string; override;
  end;

  TBlizzard = class( TSpell )
  private
    Resource : TResource;
  protected
    function GetLoaded : Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function GetName : string; override;
    procedure Casting( Source : TCharacter ); override;
    function Range( Source : TCharacter ) : Integer; override;
    function Recovery( Source : TCharacter ) : Integer; override;
    function Drain( Source : TCharacter ) : Single; override;
    function Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean; override;
    function GetIconXY( Source : TCharacter ) : TPoint; override;
    function GetInfo( Source : TCharacter ) : string; override;
  end;

  TFirefly = class( TSpell )
  private
    Resource1 : TResource;
    Resource2 : TResource;
    Resource3 : TResource;
  protected
    function GetLoaded : Boolean; override;
  public
    constructor Create; override;
    class function GetName : string; override;
    function Range( Source : TCharacter ) : Integer; override;
    function Recovery( Source : TCharacter ) : Integer; override;
    function Drain( Source : TCharacter ) : Single; override;
    function Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean; override;
    function GetIconXY( Source : TCharacter ) : TPoint; override;
    function GetInfo( Source : TCharacter ) : string; override;
  end;

  TForgetEffect = class( TEffect )
  private
    Points : integer;
    PointList : pointer;
    FCharacter : TCharacter;
    OldAI : TAI;
    OldAIMode : TAIMode;
    Applied : boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Refresh( NewEffect : TEffect ); override;
    procedure Adjust( Character : TCharacter ); override;
    procedure RenderLocked( Figure : TAniFigure; Bits : PBITPLANE ); override;
    function DoFrame : boolean; override;
  end;

  TAuraEffect = class( TEffect )
  private
    HitResource : TResource;
    FCharacter : TCharacter;
  public
    procedure Adjust( Character : TCharacter ); override;
    function Hit( Source : TAniFigure; Damage : PDamageProfile ) : boolean; override;
  end;

  TReflectEffect = class( TEffect )
  private
    HitResource : TResource;
    FCharacter : TCharacter;
    Reflect : boolean;
  public
    procedure Adjust( Character : TCharacter ); override;
    function Hit( Source : TAniFigure; Damage : PDamageProfile ) : boolean; override;
  end;

  TThiefEffect = class( TEffect )
  private
    List : TStringList;
    FCharacter : TCharacter;
    FrameCount : integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Adjust( Character : TCharacter ); override;
    function DoFrame : boolean; override;
  end;

  TDamagingEffect = class( TEffect )
  private
    FCharacter : TCharacter;
    FrameCount : integer;
  public
    Damage : TDamageProfile;
    TriggerFrame : integer;
    Source : TCharacter;
    UseStealth : boolean;
    procedure Adjust( Character : TCharacter ); override;
    function DoFrame : boolean; override;
  end;

  TLightningProjectile = class( TProjectile )
  protected
    procedure DoFrame; override;
  public
    procedure Launch( Source : TCharacter; Target : TSpriteObject; X, Y : Longint ); override;
  end;

  TShrapnelProjectile = class( TProjectile )
  protected
    procedure DoFrame; override;
  public
    Count : integer;
    NewTrackingDegree : single;
    procedure Launch( Source : TCharacter; Target : TSpriteObject; X, Y : Longint ); override;
  end;

  TChargeProjectile = class( TProjectile )
  protected
    procedure DoFrame; override;
  end;

  TFireflyProjectile = class( TProjectile )
  private
    NewTargetX, NewTargetY : longint;
    Count : integer;
    Angle : integer;
    WanderMode : boolean;
    Direction : boolean;
    procedure Arrival( sender : TObject );
    function FindEnemies : TList;
  protected
    procedure DoFrame; override;
    procedure CollideFigure( Source, Target : TAniFigure; var Stop : Boolean ); override;
  public
    ScanRange : integer;
    procedure Launch( Source : TCharacter; Target : TSpriteObject; X, Y : Longint ); override;
  end;

  TInfernoProjectile = class;
  TInfernoCoreProjectile = class( TProjectile )
  protected
    procedure Render; override;
    procedure CollideFigure( Source, Target : TAniFigure; var Stop : Boolean ); override;
    procedure CollideItem( Source : TAniFigure; var Stop : Boolean ); override;
    procedure CollideBoundary( Source : TAniFigure ); override;
  public
    Proj1, Proj2 : TInfernoProjectile;
  end;

  TInfernoProjectile = class( TProjectile )
  protected
    procedure DoFrame; override;
  public
    Core : TInfernoCoreProjectile;
    Angle : integer;
    SpinRate : integer;
    SpinRadius : integer;
  end;

  TReflect = class( TSpell )
  private
    Resource : TResource;
    HitResource : TResource;
  protected
    function GetLoaded : Boolean; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function GetName : string; override;
    function Range( Source : TCharacter ) : Integer; override;
    function Recovery( Source : TCharacter ) : Integer; override;
    function Drain( Source : TCharacter ) : Single; override;
    function Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean; override;
    function GetIconXY( Source : TCharacter ) : TPoint; override;
    function GetInfo( Source : TCharacter ) : string; override;
  end;

function LoadSpells : boolean;
function FreeSpells : boolean;
procedure ClearSpellResources;
function MakeCastEffect( var CastEffect : TResource; const ResName : string ) : boolean;
function MakeSpell( var Spell : TSpell; SpellClass : TSpellClass ) : boolean;

implementation

uses
  AniDec30,
  AI1,
  MiscAI,
  AniDemo,
  Effects,
  Display,
  Spells1;

var
  ProtectionResource : TResource;

function MakeCastEffect( var CastEffect : TResource; const ResName : string ) : boolean;
begin
  result := false;
  Log.Log( 'Loading ' + ResName + ' cast effect...' );
  CastEffect := LoadArtResource( 'engine\spells\' + ResName + '.gif', true );
  if not assigned( CastEffect ) or not CastEffect.Loaded then
  begin
    Log.Log( ResName + ' cast effect could not be loaded' );
    Exit;
  end;
  AllCastResourceList.add( CastEffect );
  result := true;
end;

function MakeSpell( var Spell : TSpell; SpellClass : TSpellClass ) : boolean;
var
  i : integer;
  S : string;
begin
  result := false;
  S := SpellClass.GetName;
  Log.Log( 'Loading ' + S + ' resource...' );
  Spell := SpellClass.Create;
  if not Spell.Loaded then
  begin
    Log.Log( S + ' could not be loaded' );
    Exit;
  end;
  i := AllSpellList.add( S );
  AllSpellList.Objects[ i ] := Spell;
  Spell.DisplayName := ExText.GetText( S );
  Spell.InfoText := ExText.GetText( S + ' Info' );
  if Spell.DisplayName = '' then
    Spell.DisplayName := S;
  result := true;
end;

function LoadSpells : boolean;
begin
  result := false;

  AllCastResourceList := TList.create;
  if not MakeCastEffect( FireEffect, 'FireSpell' ) then
    exit;
  if not MakeCastEffect( FrostEffect, 'Frost' ) then
    exit;
  if not MakeCastEffect( LightningEffect, 'shockblu' ) then
    exit;
  if not MakeCastEffect( ShrapnelEffect, 'shrapnelcast' ) then
    exit;
  if not MakeCastEffect( ChargeEffect, 'chargespell4' ) then
    exit;
  if not MakeCastEffect( PushEffect, 'push' ) then
    exit;
  if not MakeCastEffect( HealEffect, 'healcast' ) then
    exit;
  if not MakeCastEffect( ProtectionEffect, 'charge2' ) then
    exit;
  if not MakeCastEffect( SummonEffect, 'summoncast' ) then
    exit;
  if not MakeCastEffect( HoldEffect, 'holdcast' ) then
    exit;
  MakeCastEffect( LichEffect, 'lichcast' );
  if not MakeCastEffect( AuraEffect, 'AuraSteelCast' ) then
    exit;
  MakeCastEffect( ManaThiefEffect, 'ManaThiefCast' );
  MakeCastEffect( GreatHandEffect, 'GreatHandCast' );
  MakeCastEffect( GreatWolfEffect, 'GreatWolfCast' );
  if not MakeCastEffect( BigFire, 'FireCast(LVL3)' ) then
    exit;
  BigFire.SpecialEffect := seAdd;
  if not MakeCastEffect( BigIce, 'Frost(LVL3)' ) then
    exit;
  BigIce.SpecialEffect := seAdd;
  MakeCastEffect( ReflectEffect, 'ReflectCast' );

  AllSpellList := TStringList.create;
  ExText.Open( 'Spells' );
  if not MakeSpell( Fireball, TFireball ) then
    exit;
  if not MakeSpell( Frostball, TFrostball ) then
    exit;
  if not MakeSpell( Lightning, TLightning ) then
    exit;
  if not MakeSpell( Shrapnel, TShrapnel ) then
    exit;
  if not MakeSpell( Charge, TCharge ) then
    exit;
  if not MakeSpell( Push, TPush ) then
    exit;
  if not MakeSpell( Healing, THeal ) then
    exit;
  if not MakeSpell( ProtectionFire, TProtectionFire ) then
    exit;
  if not MakeSpell( ProtectionCold, TProtectionCold ) then
    exit;
  if not MakeSpell( ProtectionElectricity, TProtectionElectricity ) then
    exit;
  if not MakeSpell( ProtectionPoison, TProtectionPoison ) then
    exit;
  if not MakeSpell( ProtectionMagic, TProtectionMagic ) then
    exit;
  if not MakeSpell( ProtectionAll, TProtectionAll ) then
    exit;
  if not MakeSpell( AuraOfIron, TAuraOfIron ) then
    exit;
  if not MakeSpell( AuraOfSteel, TAuraOfSteel ) then
    exit;
  if not MakeSpell( SummonRat, TSummonRat ) then
    exit;
  if not MakeSpell( SummonWolf, TSummonWolf ) then
    exit;
  if not MakeSpell( Shadow, TShadow ) then
    exit;
  if not MakeSpell( Hold, THold ) then
    exit;
  if not MakeSpell( Firefly, TFirefly ) then
    exit;
  MakeSpell( DeathSpell, TDeathSpell );
  if not MakeSpell( Forget, TForget ) then
    exit;
  MakeSpell( ManaThief, TManaThief );
  MakeSpell( GreatHand, TGreatHand );
  MakeSpell( GreatWolf, TGreatWolf );
  if not MakeSpell( FlameStrike, TFlameStrike ) then
    exit;
  if not MakeSpell( Blizzard, TBlizzard ) then
    exit;
  MakeSpell( Reflect, TReflect );

  if not LoadSpells1 then
    exit;

  ExText.Close;

  result := true;

end;

function FreeSpells : boolean;
var
  i : integer;
begin
  result := true;

  try
    Log.Log( 'Freeing spell resources' );
    for i := 0 to AllSpellList.count - 1 do
    begin
//        Log.Log('b'+inttostr(i));
      TSpell( AllSpellList.Objects[ i ] ).free;
    end;
    AllSpellList.free;

    Log.Log( 'Freeing spell effect resources' );
    for i := 0 to AllCastResourceList.count - 1 do
    begin
//        Log.Log('c'+inttostr(i));
      TResource( AllCastResourceList.items[ i ] ).free;
    end;
    AllCastResourceList.free;
  except
    on E : Exception do
      Log.log( 'Error Spells FreeSpells: ' + E.Message );
  end;
end;

procedure ClearSpellResources;
var
  i : integer;
begin
  try
    Log.Log( 'Clearing spell resources' );
    for i := 0 to AllSpellList.count - 1 do
    begin
      TSpell( AllSpellList.Objects[ i ] ).Clear;
    end;

    Log.Log( 'Clearing spell effect resources' );
    for i := 0 to AllCastResourceList.count - 1 do
    begin
      if TResource( AllCastResourceList.items[ i ] ).OnDemand then
      begin
        TResource( AllCastResourceList.items[ i ] ).RLE.free;
        TResource( AllCastResourceList.items[ i ] ).RLE := nil;
      end;
    end;
  except
    on E : Exception do
      Log.log( 'Error Spells ClearSpells: ' + E.Message );
  end;

end;

{ TFireball }

function TFireball.Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean;
const
  SpinRate = 14;
  SpinRadius = 16;
var
  NewProjectile : TProjectile;
  NewCore : TProjectile;
  Tail1, Tail2 : TProjectile;
  Z1, Z2, Z3 : Integer;
  Bonus, Angle : single;
  TargetX, TargetY : longint;
const
  FailName : string = 'TFireball.Cast';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := inherited Cast( Source, Target );
    if not result then
      Exit;
    Z1 := Source.Height div 2 - 16;
    if Assigned( Target ) then
      Z2 := 0
    else
      Z2 := Z1;

    z3 := Source.Height div 2;

    if Source.TitleExists( 'Inferno' ) then
    begin
      NewProjectile := TProjectile( Sprites.NewSprite( TInfernoCoreProjectile, Resource, Source.X, Source.Y, Z3, 0 ) );
      if assigned( NewProjectile ) then
      begin
        ZeroMemory( @NewProjectile.Damage, sizeof( NewProjectile.Damage ) );
        NewProjectile.Radius := 0;
        NewProjectile.Duration := Source.Constitution * 10;
        NewProjectile.TrackingDegree := Source.Coordination / 20;
        NewProjectile.Speed := 12 + Source.Coordination / 10;
        NewCore := NewProjectile;
        TargetX := Source.TargetX;
        TargetY := Source.TargetY;
        Bonus := Source.Coordination + Source.Perception / 2 + Source.Combat / 4 - Source.Restriction / 5;
        if Bonus < 10 then
          Angle := 17.5
        else
          Angle := 175 / Bonus;
        ComputeTrajectory( Source, TargetX, TargetY, Angle );
        NewProjectile.Launch( Source, Target, TargetX, TargetY + z2 );

        NewProjectile := TProjectile( Sprites.NewSprite( TInfernoProjectile, Resource, Source.X, Source.Y, Z3 + SpinRadius, 1 ) );
        if assigned( NewProjectile ) then
        begin
          NewProjectile.Magic := Source.Mysticism * 2;
          ZeroMemory( @NewProjectile.Damage, sizeof( NewProjectile.Damage ) );
          NewProjectile.Damage.Heat.Min := Source.Mysticism / 2;
          NewProjectile.Damage.Heat.Max := Source.Mysticism;
          NewProjectile.Damage.Crushing.Min := 2 * Source.Mysticism / 20;
          NewProjectile.Damage.Crushing.Max := 8 * Source.Mysticism / 20;
          NewProjectile.DamageRadius := 16;
          NewProjectile.Radius := 14;
          NewProjectile.Duration := Source.Constitution * 10;
          NewProjectile.TrackingDegree := 0;
          NewProjectile.Speed := NewCore.Speed * 2;
          TInfernoProjectile( NewProjectile ).Core := TInfernoCoreProjectile( NewCore );
          TInfernoProjectile( NewProjectile ).Angle := 0;
          TInfernoProjectile( NewProjectile ).SpinRate := SpinRate;
          TInfernoProjectile( NewProjectile ).SpinRadius := SpinRadius;
          TInfernoCoreProjectile( NewCore ).Proj1 := TInfernoProjectile( NewProjectile );

          if Source.Coordination <= 10 then
            NewProjectile.HitIncidental := 1
          else
            NewProjectile.HitIncidental := 10 / Source.Coordination;
          NewProjectile.HitTarget := 1;
          NewProjectile.UseLineOfSight := True;
          NewProjectile.GlowEffect := TGlow( Sprites.NewSprite( TGlow, nil, Source.X, Source.Y - 1, 0, 0 ) );
          NewProjectile.GlowEffect.RFactor := 100;
          NewProjectile.GlowEffect.GFactor := 50;
          NewProjectile.GlowEffect.BFactor := 25;
          NewProjectile.GlowEffect.Alpha := 100;

          Tail1 := NewProjectile;
          Tail2 := TProjectile( Sprites.NewSprite( TProjectile, Resource2, Source.X, Source.Y, z3 + SpinRadius, 1 ) );
          Tail2.Passive := true;
          Tail1.TrailedBy := Tail2;
          Tail1 := Tail2;
          Tail1.DoAction( 'Default' );
          Tail2 := TProjectile( Sprites.NewSprite( TProjectile, Resource3, Source.X, Source.Y, z3 + SpinRadius, 1 ) );
          Tail2.Passive := true;
          Tail2.GlowEffect := TGlow( Sprites.NewSprite( TGlow, nil, Source.X, Source.Y - 1, 0, 0 ) );
          Tail2.GlowEffect.RFactor := 50;
          Tail2.GlowEffect.GFactor := 25;
          Tail2.GlowEffect.BFactor := 12;
          Tail2.GlowEffect.Alpha := 50;
          Tail2.OnMove := Tail2.MoveEvent;
          Tail1.TrailedBy := Tail2;
          Tail1 := Tail2;
          Tail1.DoAction( 'Default' );
          Tail2 := TProjectile( Sprites.NewSprite( TProjectile, Resource4, Source.X, Source.Y, z3 + SpinRadius, 1 ) );
          Tail2.Passive := true;
          Tail1.TrailedBy := Tail2;
          Tail1 := Tail2;
          Tail1.DoAction( 'Default' );
          Tail2 := TProjectile( Sprites.NewSprite( TProjectile, Resource5, Source.X, Source.Y, z3 + SpinRadius, 1 ) );
          Tail2.Passive := true;
          Tail1.TrailedBy := Tail2;
          Tail1 := Tail2;
          Tail1.DoAction( 'Default' );
          Tail2 := TProjectile( Sprites.NewSprite( TProjectile, Resource6, Source.X, Source.Y, z3 + SpinRadius, 1 ) );
          Tail2.Passive := true;
          Tail1.TrailedBy := Tail2;
          Tail1 := Tail2;
          Tail1.DoAction( 'Default' );
          Tail2 := TProjectile( Sprites.NewSprite( TProjectile, Resource7, Source.X, Source.Y, z3 + SpinRadius, 1 ) );
          Tail2.Passive := true;
          Tail1.TrailedBy := Tail2;
          Tail1 := Tail2;
          Tail1.DoAction( 'Default' );
          Tail2.TrailedBy := nil;

          NewProjectile.Launch( Source, Target, TargetX, TargetY + z2 );
        end;
  {************************************************************************************}
        //Second Fireball
        NewProjectile := TProjectile( Sprites.NewSprite( TInfernoProjectile, Resource, Source.X, Source.Y, z3 - SpinRadius, 1 ) );
        if assigned( NewProjectile ) then
        begin
          NewProjectile.Magic := Source.Mysticism * 2;
          ZeroMemory( @NewProjectile.Damage, sizeof( NewProjectile.Damage ) );
          NewProjectile.Damage.Heat.Min := Source.Mysticism / 2;
          NewProjectile.Damage.Heat.Max := Source.Mysticism;
          NewProjectile.Damage.Crushing.Min := 2 * Source.Mysticism / 20;
          NewProjectile.Damage.Crushing.Max := 8 * Source.Mysticism / 20;
          NewProjectile.DamageRadius := 16;
          NewProjectile.Radius := 14;
          NewProjectile.Duration := Source.Constitution * 10;
          NewProjectile.TrackingDegree := 0;
          NewProjectile.Speed := NewCore.Speed * 2;
          TInfernoProjectile( NewProjectile ).Core := TInfernoCoreProjectile( NewCore );
          TInfernoProjectile( NewProjectile ).Angle := 180;
          TInfernoProjectile( NewProjectile ).SpinRate := SpinRate;
          TInfernoProjectile( NewProjectile ).SpinRadius := SpinRadius;
          TInfernoCoreProjectile( NewCore ).Proj2 := TInfernoProjectile( NewProjectile );

          if Source.Coordination <= 10 then
            NewProjectile.HitIncidental := 1
          else
            NewProjectile.HitIncidental := 10 / Source.Coordination;

          NewProjectile.HitTarget := 1;
          NewProjectile.UseLineOfSight := True;
          NewProjectile.GlowEffect := TGlow( Sprites.NewSprite( TGlow, nil, Source.X, Source.Y - 1, 0, 0 ) );
          NewProjectile.GlowEffect.RFactor := 100;
          NewProjectile.GlowEffect.GFactor := 50;
          NewProjectile.GlowEffect.BFactor := 25;
          NewProjectile.GlowEffect.Alpha := 100;

          Tail1 := NewProjectile;
          Tail2 := TProjectile( Sprites.NewSprite( TProjectile, Resource2, Source.X, Source.Y, z3 - SpinRadius, 1 ) );
          Tail2.Passive := true;
          Tail1.TrailedBy := Tail2;
          Tail1 := Tail2;
          Tail1.DoAction( 'Default' );
          Tail2 := TProjectile( Sprites.NewSprite( TProjectile, Resource3, Source.X, Source.Y, z3 - SpinRadius, 1 ) );
          Tail2.Passive := true;
          Tail2.GlowEffect := TGlow( Sprites.NewSprite( TGlow, nil, Source.X, Source.Y - 1, 0, 0 ) );
          Tail2.GlowEffect.RFactor := 50;
          Tail2.GlowEffect.GFactor := 25;
          Tail2.GlowEffect.BFactor := 12;
          Tail2.GlowEffect.Alpha := 50;
          Tail2.OnMove := Tail2.MoveEvent;
          Tail1.TrailedBy := Tail2;
          Tail1 := Tail2;
          Tail1.DoAction( 'Default' );
          Tail2 := TProjectile( Sprites.NewSprite( TProjectile, Resource4, Source.X, Source.Y, z3 - SpinRadius, 1 ) );
          Tail2.Passive := true;
          Tail1.TrailedBy := Tail2;
          Tail1 := Tail2;
          Tail1.DoAction( 'Default' );
          Tail2 := TProjectile( Sprites.NewSprite( TProjectile, Resource5, Source.X, Source.Y, z3 - SpinRadius, 1 ) );
          Tail2.Passive := true;
          Tail1.TrailedBy := Tail2;
          Tail1 := Tail2;
          Tail1.DoAction( 'Default' );
          Tail2 := TProjectile( Sprites.NewSprite( TProjectile, Resource6, Source.X, Source.Y, z3 - SpinRadius, 1 ) );
          Tail2.Passive := true;
          Tail1.TrailedBy := Tail2;
          Tail1 := Tail2;
          Tail1.DoAction( 'Default' );
          Tail2 := TProjectile( Sprites.NewSprite( TProjectile, Resource7, Source.X, Source.Y, z3 - SpinRadius, 1 ) );
          Tail2.Passive := true;
          Tail1.TrailedBy := Tail2;
          Tail1 := Tail2;
          Tail1.DoAction( 'Default' );
          Tail2.TrailedBy := nil;

          NewProjectile.Launch( Source, Target, TargetX, TargetY + z2 );
        end;
      end;
{**************************************************************************************}
    end
    else if Source.TitleExists( 'Fireball' ) then
    begin
      NewProjectile := TProjectile( Sprites.NewSprite( TProjectile, Resource2, Source.X, Source.Y, Z1, 1 ) );
      if assigned( NewProjectile ) then
      begin
        NewProjectile.Magic := 3 * Source.Mysticism div 22;
        ZeroMemory( @NewProjectile.Damage, sizeof( NewProjectile.Damage ) );
        NewProjectile.Damage.Heat.Min := Source.Mysticism / 1.5;
        NewProjectile.Damage.Heat.Max := Source.Mysticism * 1.5;
        NewProjectile.Damage.Crushing.Min := 2 * Source.Mysticism / 15;
        NewProjectile.Damage.Crushing.Max := 8 * Source.Mysticism / 15;
        NewProjectile.DamageRadius := 16;
        NewProjectile.Radius := 14;
        NewProjectile.Duration := Source.Constitution * 10;
        NewProjectile.TrackingDegree := Source.Coordination / 20;
        NewProjectile.Speed := 12 + Source.Coordination / 10;
        if Source.Coordination <= 10 then
          NewProjectile.HitIncidental := 1
        else
          NewProjectile.HitIncidental := 10 / Source.Coordination;
        NewProjectile.HitTarget := 1;
        NewProjectile.UseLineOfSight := True;
        NewProjectile.GlowEffect := TGlow( Sprites.NewSprite( TGlow, nil, Source.X, Source.Y - 1, 0, 0 ) );
        NewProjectile.GlowEffect.RFactor := 100;
        NewProjectile.GlowEffect.GFactor := 50;
        NewProjectile.GlowEffect.BFactor := 25;
        NewProjectile.GlowEffect.Alpha := 100;

        Tail1 := NewProjectile;
        Tail2 := TProjectile( Sprites.NewSprite( TProjectile, Resource3, Source.X, Source.Y, Z1, 1 ) );
        Tail2.Passive := true;
        Tail1.TrailedBy := Tail2;
        Tail1 := Tail2;
        Tail1.DoAction( 'Default' );
        Tail2 := TProjectile( Sprites.NewSprite( TProjectile, Resource4, Source.X, Source.Y, Z1, 1 ) );
        Tail2.Passive := true;
        Tail2.GlowEffect := TGlow( Sprites.NewSprite( TGlow, nil, Source.X, Source.Y - 1, 0, 0 ) );
        Tail2.GlowEffect.RFactor := 50;
        Tail2.GlowEffect.GFactor := 25;
        Tail2.GlowEffect.BFactor := 12;
        Tail2.GlowEffect.Alpha := 50;
        Tail2.OnMove := Tail2.MoveEvent;
        Tail1.TrailedBy := Tail2;
        Tail1 := Tail2;
        Tail1.DoAction( 'Default' );
        Tail2 := TProjectile( Sprites.NewSprite( TProjectile, Resource5, Source.X, Source.Y, Z1, 1 ) );
        Tail2.Passive := true;
        Tail1.TrailedBy := Tail2;
        Tail1 := Tail2;
        Tail1.DoAction( 'Default' );
        Tail2 := TProjectile( Sprites.NewSprite( TProjectile, Resource6, Source.X, Source.Y, Z1, 1 ) );
        Tail2.Passive := true;
        Tail1.TrailedBy := Tail2;
        Tail1 := Tail2;
        Tail1.DoAction( 'Default' );
        Tail2 := TProjectile( Sprites.NewSprite( TProjectile, Resource7, Source.X, Source.Y, Z1, 1 ) );
        Tail2.Passive := true;
        Tail1.TrailedBy := Tail2;
        Tail1 := Tail2;
        Tail1.DoAction( 'Default' );
        Tail2.TrailedBy := nil;

        TargetX := Source.TargetX;
        TargetY := Source.TargetY;
        Bonus := Source.Coordination + Source.Perception / 2 + Source.Combat / 4 - Source.Restriction / 5;
        if Bonus < 10 then
          Angle := 17.5
        else
          Angle := 175 / Bonus;
        ComputeTrajectory( Source, TargetX, TargetY, Angle );

        NewProjectile.Launch( Source, Target, TargetX, TargetY + Z2 );
      end;
    end
    else
    begin
      NewProjectile := TProjectile( Sprites.NewSprite( TProjectile, Resource3, Source.X, Source.Y, Z1, 1 ) );
      if assigned( NewProjectile ) then
      begin
        NewProjectile.Magic := Source.Mysticism;
        ZeroMemory( @NewProjectile.Damage, sizeof( NewProjectile.Damage ) );
        NewProjectile.Damage.Heat.Min := Source.Mysticism / 2;
        NewProjectile.Damage.Heat.Max := Source.Mysticism;
        NewProjectile.Damage.Crushing.Min := 2 * Source.Mysticism / 20;
        NewProjectile.Damage.Crushing.Max := 8 * Source.Mysticism / 20;
        NewProjectile.DamageRadius := 16;
        NewProjectile.Radius := 14;
        NewProjectile.Duration := Source.Constitution * 10;
        NewProjectile.TrackingDegree := Source.Coordination / 20;
        NewProjectile.Speed := 12 + Source.Coordination / 10;
        if Source.Coordination <= 10 then
          NewProjectile.HitIncidental := 1
        else
          NewProjectile.HitIncidental := 10 / Source.Coordination;
        NewProjectile.HitTarget := 1;
        NewProjectile.UseLineOfSight := True;
        NewProjectile.GlowEffect := TGlow( Sprites.NewSprite( TGlow, nil, Source.X, Source.Y - 1, 0, 0 ) );
        NewProjectile.GlowEffect.RFactor := 100;
        NewProjectile.GlowEffect.GFactor := 50;
        NewProjectile.GlowEffect.BFactor := 25;
        NewProjectile.GlowEffect.Alpha := 100;

        Tail1 := NewProjectile;
        Tail2 := TProjectile( Sprites.NewSprite( TProjectile, Resource4, Source.X, Source.Y, Z1, 1 ) );
        Tail2.Passive := true;
        Tail1.TrailedBy := Tail2;
        Tail1 := Tail2;
        Tail1.DoAction( 'Default' );
        Tail2 := TProjectile( Sprites.NewSprite( TProjectile, Resource5, Source.X, Source.Y, Z1, 1 ) );
        Tail2.Passive := true;
        Tail2.GlowEffect := TGlow( Sprites.NewSprite( TGlow, nil, Source.X, Source.Y - 1, 0, 0 ) );
        Tail2.GlowEffect.RFactor := 50;
        Tail2.GlowEffect.GFactor := 25;
        Tail2.GlowEffect.BFactor := 12;
        Tail2.GlowEffect.Alpha := 50;
        Tail2.OnMove := Tail2.MoveEvent;
        Tail1.TrailedBy := Tail2;
        Tail1 := Tail2;
        Tail1.DoAction( 'Default' );
        Tail2 := TProjectile( Sprites.NewSprite( TProjectile, Resource6, Source.X, Source.Y, Z1, 1 ) );
        Tail2.Passive := true;
        Tail1.TrailedBy := Tail2;
        Tail1 := Tail2;
        Tail1.DoAction( 'Default' );
        Tail2 := TProjectile( Sprites.NewSprite( TProjectile, Resource7, Source.X, Source.Y, Z1, 1 ) );
        Tail2.Passive := true;
        Tail1.TrailedBy := Tail2;
        Tail1 := Tail2;
        Tail1.DoAction( 'Default' );
        Tail2.TrailedBy := nil;

        TargetX := Source.TargetX;
        TargetY := Source.TargetY;
        Bonus := Source.Coordination + Source.Perception / 2 + Source.Combat / 4 - Source.Restriction / 5;
        if Bonus < 10 then
          Angle := 17.5
        else
          Angle := 175 / Bonus;
        ComputeTrajectory( Source, TargetX, TargetY, Angle );

        NewProjectile.Launch( Source, Target, TargetX, TargetY + Z2 );
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TFireball.Casting( Source : TCharacter );
var
  Effect : TEffect;
begin
  if Source.TitleExists( 'Inferno' ) and TCharacterResource( Source.Resource ).UseCastAnimation then
  begin
    Effect := TEffect.Create;
    Effect.Resource := BigFire;
    Effect.AnimationDuration := 10 * Effect.Resource.FrameMultiplier;
    Effect.Power := Source.Mysticism;
    Effect.DoAction( 'Default', Source.FacingString );
    Source.AddEffect( Effect );
  end;
end;

procedure TFireball.Clear;
begin
  Resource.RLE.free; Resource.RLE := nil;
  Resource2.RLE.free; Resource2.RLE := nil;
  Resource3.RLE.free; Resource3.RLE := nil;
  Resource4.RLE.free; Resource4.RLE := nil;
  Resource5.RLE.free; Resource5.RLE := nil;
  Resource6.RLE.free; Resource6.RLE := nil;
  Resource7.RLE.free; Resource7.RLE := nil;
end;

constructor TFireball.Create;
const
  FailName : string = 'TFireball.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    CastingType := ctCombat;
    TargetType := ttEnemy;
    CastEffect := FireEffect;
    Resource := LoadArtResource( 'engine\weaponprojectiles\FireBallExplode.gif', true );
    Resource2 := LoadArtResource( 'engine\weaponprojectiles\FireBall2(LVL3).gif', true );
    Resource3 := LoadArtResource( 'engine\weaponprojectiles\FireBall3(LVL3).gif', true );
    Resource4 := LoadArtResource( 'engine\weaponprojectiles\FireBall4(LVL3).gif', true );
    Resource5 := LoadArtResource( 'engine\weaponprojectiles\FireBall5(LVL3).gif', true );
    Resource6 := LoadArtResource( 'engine\weaponprojectiles\FireBall4(LVL2).gif', true );
    Resource7 := LoadArtResource( 'engine\weaponprojectiles\FireBall5(LVL2).gif', true );
    Resource2.SpecialEffect := seAdd;
    Resource3.SpecialEffect := seAdd;
    Resource4.SpecialEffect := seAdd;
    Resource5.SpecialEffect := seAdd;
    Resource6.SpecialEffect := seAdd;
    Resource7.SpecialEffect := seAdd;
    Resource.DrawShadow := false;
    SoundInCast := false;
    LoadCastSounds( 'FireCast' );
    Interupted := false;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TFireball.Destroy;
const
  FailName : string = 'TFireball.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    Resource.Free;
    Resource2.free;
    Resource3.free;
    Resource4.free;
    Resource5.free;
    Resource6.free;
    Resource7.free;
    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TFireball.Drain( Source : TCharacter ) : Single;
const
  FailName : string = 'TFireball.Drain';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 5 + round( Source.Restriction / 10 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TFireball.GetIconXY( Source : TCharacter ) : TPoint;
const
  FailName : string = 'TFireball.GetIconXY';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if Source.TitleExists( 'Inferno' ) then
    begin
      result.X := 8 * 32;
      result.Y := 32;
    end
    else if Source.TitleExists( 'Fireball' ) then
    begin
      result.X := 8 * 32;
      result.Y := 0;
    end
    else
    begin
      result.X := 8 * 32;
      result.Y := 0;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TFireball.GetInfo( Source : TCharacter ) : string;
begin
  if Source.TitleExists( 'Inferno' ) then
  begin
    result := InfoText;
    Replace( result, 'a', inttostr( round( Source.Mysticism / 1.5 ) ) + '-' + inttostr( round( Source.Mysticism * 1.5 ) ) );
    Replace( result, 'b', inttostr( round( 2 * Source.Mysticism / 15 ) ) + '-' + inttostr( round( 8 * Source.Mysticism / 15 ) ) );
  end
  else if Source.TitleExists( 'Fireball' ) then
  begin
    result := InfoText;
    Replace( result, 'a', inttostr( round( Source.Mysticism / 1.5 ) ) + '-' + inttostr( round( Source.Mysticism * 1.5 ) ) );
    Replace( result, 'b', inttostr( round( 2 * Source.Mysticism / 15 ) ) + '-' + inttostr( round( 8 * Source.Mysticism / 15 ) ) );
  end
  else
  begin
    result := InfoText;
    Replace( result, 'a', inttostr( round( Source.Mysticism / 2 ) ) + '-' + inttostr( Source.Mysticism ) );
    Replace( result, 'b', inttostr( round( 2 * Source.Mysticism / 20 ) ) + '-' + inttostr( round( 8 * Source.Mysticism / 20 ) ) );
  end;
end;

function TFireball.GetLoaded : Boolean;
const
  FailName : string = 'TFireball.GetLoaded';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := Resource.Loaded and Resource2.Loaded and Resource3.Loaded and
      Resource4.Loaded and Resource5.Loaded and Resource6.Loaded and
      Resource7.Loaded;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

class function TFireball.GetName : string;
begin
  result := 'Flame';
end;

function TFireball.Range( Source : TCharacter ) : Integer;
const
  FailName : string = 'TFireball.Range';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := Round( Source.Constitution * 10 * ( 12 + Source.Coordination / 10 ) );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TFireball.Recovery( Source : TCharacter ) : Integer;
const
  FailName : string = 'TFireball.Recovery';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 40 - ( ( Source.Constitution + Source.Mysticism ) div 2 );
    if result < 0 then
      result := 0;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TFrostball }

function TFrostball.Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean;
var
  NewProjectile : TProjectile;
  Z1, Z2 : Integer;
  Bonus, Angle : single;
  TargetX, TargetY : longint;
  Tail1, Tail2 : TProjectile;
const
  FailName : string = 'TFrostball.Cast';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := inherited Cast( Source, Target );
    if not result then
      Exit;
    Z1 := Source.Height div 2 - 16;
    if Assigned( Target ) then
      Z2 := 0
    else
      Z2 := Z1;
    if Source.TitleExists( 'Deepfreeze' ) then
    begin
      NewProjectile := TProjectile( Sprites.NewSprite( TProjectile, Resource, Source.X, Source.Y, Z1, 1 ) );
      if assigned( NewProjectile ) then
      begin
        NewProjectile.Magic := Source.Mysticism * 2;
        ZeroMemory( @NewProjectile.Damage, sizeof( NewProjectile.Damage ) );
        NewProjectile.Damage.Cold.Min := Source.Mysticism;
        NewProjectile.Damage.Cold.Max := Source.Mysticism * 2;
        NewProjectile.DamageRadius := 8;
        NewProjectile.Radius := 14;
        NewProjectile.Duration := Source.Mysticism * 10;
        NewProjectile.TrackingDegree := 2 + Source.Mysticism / 15;
        NewProjectile.Speed := 6 + Source.Mysticism / 30;
        if Source.Mysticism + Source.Coordination <= 10 then
          NewProjectile.HitIncidental := 1
        else
          NewProjectile.HitIncidental := 10 / ( Source.Mysticism + Source.Coordination );
        NewProjectile.HitTarget := 1;
        NewProjectile.UseLineOfSight := True;
        NewProjectile.Alpha := 75;
        NewProjectile.GlowEffect := TGlow( Sprites.NewSprite( TGlow, nil, Source.X, Source.Y - 1, 0, 0 ) );
        NewProjectile.GlowEffect.RFactor := 0;
        NewProjectile.GlowEffect.GFactor := 25;
        NewProjectile.GlowEffect.BFactor := 100;
        NewProjectile.GlowEffect.Alpha := 50;

        Tail1 := NewProjectile;
        Tail2 := TProjectile( Sprites.NewSprite( TProjectile, Resource, Source.X, Source.Y, Z1, 1 ) );
        Tail2.Passive := true;
        Tail1.TrailedBy := Tail2;
        Tail2.Alpha := 55;
        Tail1 := Tail2;
        Tail1.DoAction( 'Default' );
        Tail2 := TProjectile( Sprites.NewSprite( TProjectile, Resource, Source.X, Source.Y, Z1, 1 ) );
        Tail2.Passive := true;
        Tail2.GlowEffect := TGlow( Sprites.NewSprite( TGlow, nil, Source.X, Source.Y - 1, 0, 0 ) );
        Tail2.GlowEffect.RFactor := 0;
        Tail2.GlowEffect.GFactor := 25;
        Tail2.GlowEffect.BFactor := 100;
        Tail2.GlowEffect.Alpha := 45;
        Tail2.OnMove := Tail2.MoveEvent;
        Tail1.TrailedBy := Tail2;
        Tail2.Alpha := 35;
        Tail1 := Tail2;
        Tail1.DoAction( 'Default' );
        Tail2 := TProjectile( Sprites.NewSprite( TProjectile, Resource, Source.X, Source.Y, Z1, 1 ) );
        Tail2.Passive := true;
        Tail1.TrailedBy := Tail2;
        Tail2.Alpha := 15;
        Tail1 := Tail2;
        Tail1.DoAction( 'Default' );
        Tail1.TrailedBy := nil;

        NewProjectile.Launch( Source, Target, Source.TargetX, Source.TargetY + Z2 );
      end;
    end
    else if Source.TitleExists( 'Freeze' ) then
    begin
      NewProjectile := TProjectile( Sprites.NewSprite( TProjectile, Resource, Source.X, Source.Y, Z1, 1 ) );
      if assigned( NewProjectile ) then
      begin
        NewProjectile.Magic := 3 * Source.Mysticism div 2;
        ZeroMemory( @NewProjectile.Damage, sizeof( NewProjectile.Damage ) );
        NewProjectile.Damage.Cold.Min := Source.Mysticism / 2;
        NewProjectile.Damage.Cold.Max := Source.Mysticism * 2;
        NewProjectile.DamageRadius := 8;
        NewProjectile.Radius := 14;
        NewProjectile.Duration := Source.Mysticism * 10;
        NewProjectile.TrackingDegree := 1 + Source.Mysticism / 30;
        NewProjectile.Speed := 6 + Source.Mysticism / 30;
        if Source.Mysticism + Source.Coordination <= 10 then
          NewProjectile.HitIncidental := 1
        else
          NewProjectile.HitIncidental := 10 / ( Source.Mysticism + Source.Coordination );
        NewProjectile.HitTarget := 1;
        NewProjectile.UseLineOfSight := True;
        NewProjectile.Alpha := 75;
        NewProjectile.GlowEffect := TGlow( Sprites.NewSprite( TGlow, nil, Source.X, Source.Y - 1, 0, 0 ) );
        NewProjectile.GlowEffect.RFactor := 0;
        NewProjectile.GlowEffect.GFactor := 25;
        NewProjectile.GlowEffect.BFactor := 100;
        NewProjectile.GlowEffect.Alpha := 50;
        NewProjectile.Launch( Source, Target, Source.TargetX, Source.TargetY + Z2 );
      end;
    end
    else
    begin
      NewProjectile := TProjectile( Sprites.NewSprite( TProjectile, SmallResource, Source.X, Source.Y, Z1, 1 ) );
      if assigned( NewProjectile ) then
      begin
        NewProjectile.Magic := Source.Mysticism;
        ZeroMemory( @NewProjectile.Damage, sizeof( NewProjectile.Damage ) );
        NewProjectile.Damage.Cold.Min := 4 * Source.Mysticism / 5;
        NewProjectile.Damage.Cold.Max := Source.Mysticism;
        NewProjectile.DamageRadius := 6;
        NewProjectile.Radius := 7;
        NewProjectile.Duration := Source.Constitution * 10;
        NewProjectile.TrackingDegree := 1 + Source.Coordination / 20;
        NewProjectile.Speed := 6 + Source.Coordination / 30;
        if Source.Coordination <= 10 then
          NewProjectile.HitIncidental := 1
        else
          NewProjectile.HitIncidental := 10 / Source.Coordination;
        NewProjectile.HitTarget := 1;
        NewProjectile.UseLineOfSight := True;
        NewProjectile.Alpha := 150;
        NewProjectile.GlowEffect := TGlow( Sprites.NewSprite( TGlow, nil, Source.X, Source.Y - 1, 0, 0 ) );
        NewProjectile.GlowEffect.RFactor := 0;
        NewProjectile.GlowEffect.GFactor := 25;
        NewProjectile.GlowEffect.BFactor := 100;
        NewProjectile.GlowEffect.Alpha := 25;

        TargetX := Source.TargetX;
        TargetY := Source.TargetY;
        Bonus := Source.Coordination + Source.Perception / 2 + Source.Combat / 4 - Source.Restriction / 5;
        if Bonus < 10 then
          Angle := 17.5
        else
          Angle := 175 / Bonus;
        ComputeTrajectory( Source, TargetX, TargetY, Angle );

        NewProjectile.Launch( Source, Target, TargetX, TargetY + Z2 );
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TFrostBall.Casting( Source : TCharacter );
var
  Effect : TEffect;
begin
  if Source.TitleExists( 'Deepfreeze' ) and TCharacterResource( Source.Resource ).UseCastAnimation then
  begin
    Effect := TEffect.Create;
    Effect.Resource := BigIce;
    Effect.AnimationDuration := 10 * Effect.Resource.FrameMultiplier;
    Effect.Power := Source.Mysticism;
    Effect.DoAction( 'Default', Source.FacingString );
    Source.AddEffect( Effect );
  end;
end;

procedure TFrostBall.Clear;
begin
  Resource.RLE.free; Resource.RLE := nil;
  SmallResource.RLE.free; SmallResource.RLE := nil;
end;

constructor TFrostball.Create;
const
  FailName : string = 'TFrostball.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    CastingType := ctCombat;
    TargetType := ttEnemy;
    CastEffect := FrostEffect;
    Resource := LoadArtResource( 'engine\weaponprojectiles\FrostBall.gif', true );
    SmallResource := LoadArtResource( 'engine\weaponprojectiles\SmallFrostBall.gif', true );
    Resource.DrawShadow := false;
    SmallResource.DrawShadow := false;
    SoundInCast := true;
    LoadCastSounds( 'freezespell1,freezespell2' );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TFrostball.Destroy;
const
  FailName : string = 'TFrostball.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    Resource.Free;
    SmallResource.Free;
    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TFrostball.Drain( Source : TCharacter ) : Single;
const
  FailName : string = 'TFrostball.Drain';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 5 + round( Source.Restriction / 10 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TFrostball.GetIconXY( Source : TCharacter ) : TPoint;
const
  FailName : string = 'TFrostball.GetIconXY';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result.X := 9 * 32;
    result.Y := 0;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TFrostBall.GetInfo( Source : TCharacter ) : string;
begin
  result := InfoText;
  if Source.TitleExists( 'Deepfreeze' ) then
  begin
    Replace( result, 'a', inttostr( Source.Mysticism ) + '-' + inttostr( Source.Mysticism * 2 ) );
  end
  else if Source.TitleExists( 'Freeze' ) then
  begin
    Replace( result, 'a', inttostr( round( Source.Mysticism / 2 ) ) + '-' + inttostr( Source.Mysticism * 2 ) );
  end
  else
  begin
    Replace( result, 'a', inttostr( round( 4 * Source.Mysticism / 5 ) ) + '-' + inttostr( Source.Mysticism ) );
  end;
end;

function TFrostball.GetLoaded : Boolean;
const
  FailName : string = 'TFrostball.GetLoaded';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := Resource.Loaded and SmallResource.Loaded;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

class function TFrostBall.GetName : string;
begin
  result := 'Frost';
end;

function TFrostball.Range( Source : TCharacter ) : Integer;
const
  FailName : string = 'TFrostball.Range';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := Round( Source.Constitution * 50 * ( 6 + Source.Coordination / 30 ) );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TFrostball.Recovery( Source : TCharacter ) : Integer;
const
  FailName : string = 'TFrostball.Recovery';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 40 - ( ( Source.Constitution + Source.Mysticism ) div 2 );
    if result < 0 then
      result := 0;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TProtection }

function TProtection.Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean;
var
  Effect : TEffect;
  NewTarget : TCharacter;
  s : string;
const
  FailName : string = 'TProtection.Cast';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := False;
    if assigned( Target ) then
    begin
      if not ( Target is TCharacter ) then
        exit;
      if TCharacter( Target ).Dead then
        exit;
      NewTarget := TCharacter( Target );
    end
    else
      NewTarget := Source;

    result := inherited Cast( Source, NewTarget );
    if not result then
      Exit;

    Effect := TEffect.Create;
    Effect.Resource := Resource;
    Effect.AnimationDuration := 8 * Resource.FrameMultiplier;
    S := 'Major ' + GetName;
    if Source.TitleExists( S ) then
      Effect.Resistance := MajorResistance
    else
      Effect.Resistance := MinorResistance;
    Effect.Duration := Source.Mysticism * 50;
    Effect.Power := Source.Mysticism;
    Effect.EffectR := R;
    Effect.EffectG := G;
    Effect.EffectB := B;
    Effect.SpecialEffect := seAdd;
    Effect.UseCustom := true;
    Effect.Alpha := 100;
    Effect.tag := tag;
    Effect.DoAction( 'Default', NewTarget.FacingString );

    with NewTarget do
    begin
      AddEffect( Effect );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TProtection.Clear;
begin
  if ResourceOwner then
  begin
    Resource.RLE.Free; Resource.RLE := nil;
  end;
end;

constructor TProtection.Create;
const
  FailName : string = 'TProtection.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    CastingType := ctProtection;
    TargetType := ttFriend;
    CastEffect := ProtectionEffect;
    if not assigned( ProtectionResource ) then
    begin
      ProtectionResource := LoadArtResource( 'engine\spells\protectionreceive.gif', true );
      ProtectionResource.DrawShadow := false;
      ResourceOwner := true;
    end;
    Resource := ProtectionResource;

    SoundInCast := false;
    LoadCastSounds( 'protection' );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TProtection.Destroy;
const
  FailName : string = 'TProtection.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if ResourceOwner then
    begin
      ProtectionResource.free;
      ProtectionResource := nil;
    end;
    Resource := nil;
    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TProtection.Drain( Source : TCharacter ) : Single;
const
  FailName : string = 'TProtection.Drain';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 5 + round( Source.Restriction / 10 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TProtection.GetIconXY( Source : TCharacter ) : TPoint;
const
  FailName : string = 'TProtection.GetIconXY';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := IconXY;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TProtection.GetInfo( Source : TCharacter ) : string;
var
  S : string;
begin
  result := InfoText;
  S := 'Major ' + GetName;
  if Source.TitleExists( S ) then
  begin
    Replace( result, 'a', '75' );
  end
  else
  begin
    Replace( result, 'a', '50' );
  end;
end;

function TProtection.GetLoaded : Boolean;
const
  FailName : string = 'TProtection.GetLoaded';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if assigned( Resource ) then
      result := Resource.Loaded;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TProtection.Range( Source : TCharacter ) : Integer;
const
  FailName : string = 'TProtection.Range';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := Round( Source.Mysticism * 10 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TProtection.Recovery( Source : TCharacter ) : Integer;
const
  FailName : string = 'TProtection.Recovery';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 40 - ( ( Source.Coordination + Source.Mysticism ) div 4 );
    if result < 0 then
      result := 0;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TLightning }

function TLightning.Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean;
var
  Effect : TEffect;
  Damage : TDamageProfile;
  Total, Stun : single;
  Path : HGLOBAL;
  i, j, k, PathCount : integer;
  p : ^TPoint;
  C : TPoint;
  ForkPoint : integer;
  Z1 : integer;
  NewProjectile : TProjectile;
  UseLineOfSight : boolean;
  dX, dY : integer;
  TargetX, TargetY : longint;
  ValidTarget : boolean;
  DoDamage, NewDoDamage : boolean;
  Res : TResource;
  List : TStringList;
  NewTarget : TCharacter;
const
  FailName : string = 'TLightning.Cast';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    result := False;
    ValidTarget := assigned( Target ) and ( Target is TCharacter ) and not TCharacter( Target ).Dead;
    if ValidTarget then
    begin
      TargetX := Target.X;
      TargetY := Target.Y;
    end
    else
    begin
      TargetX := Source.TargetX;
      TargetY := Source.TargetY;
    end;

    if Source.TitleExists( 'Forked Lightning' ) then
    begin
      UseLineOfSight := Source.UseLineOfSight;
      Source.UseLineOfSight := true;
      PathCount := Game.FindPath( Source, TargetX, TargetY, Source.Coordination div 2, Path );
      Source.UseLineOfSight := UseLineOfSight;
      if PathCount > 0 then
      begin
        ZeroMemory( @Damage, sizeof( Damage ) );
        Damage.Heat.Min := 0;
        Damage.Heat.Max := Source.Mysticism / 4;
        Damage.Electric.Min := Source.Mysticism / 4;
        Damage.Electric.Max := Source.Mysticism * 2;
        Damage.Stun.Min := 0;
        Damage.Stun.Max := Source.Mysticism / 4;

        NewDoDamage := false;
        if PathCount > 40 then
        begin
          PathCount := 40;
          DoDamage := false;
        end
        else if PathCount > 10 + Source.Mysticism then
        begin
          PathCount := 10 + Source.Mysticism;
          DoDamage := false;
        end
        else
          DoDamage := true;

        if DoDamage and ValidTarget then
        begin
          Effect := TEffect.Create;
          Effect.Resource := ReceiveResource;
          Effect.AnimationDuration := 8 * ReceiveResource.FrameMultiplier;
          Effect.Power := Source.Mysticism;
          Effect.DoAction( 'Default', Target.FacingString );
          TCharacter( Target ).AddEffect( Effect );
        end;

        result := inherited Cast( Source, Target );
        if not result then
        begin
          GlobalFree( Path );
          Exit;
        end;

        Z1 := Source.Height div 2 - 16;
        Resource2.DrawShadow := false;
        p := GlobalLock( Path );
        ForkPoint := 2 * PathCount div 3;
        Res := Resource3;
        for i := 1 to PathCount do
        begin
          if i = ForkPoint then
          begin
            Res := Resource2;
            C := p^;
          end;
          if i < PathCount then
          begin
            inc( p );
            dX := p^.X;
            dY := p^.y;
            dec( p );
            dX := dX - p^.X;
            dY := dY - p^.Y;
            k := 3;
          end
          else
          begin
            dX := 0;
            dY := 0;
            k := 0;
          end;
          for j := 0 to k do
          begin
            NewProjectile := TProjectile( Sprites.NewSprite( TLightningProjectile, Res, p^.X + random( 4 ) - 2 + j * dX div 4, p^.Y + random( 4 ) - 2 + j * dY div 4, Z1 + random( 4 ) - 2, 1 ) );
            if assigned( NewProjectile ) then
            begin
              ZeroMemory( @NewProjectile.Damage, sizeof( NewProjectile.Damage ) );
              NewProjectile.SpecialEffect := seAdd;
              NewProjectile.Frame := random( 6 ) + 1;
              NewProjectile.Duration := 4;
              NewProjectile.TrackingDegree := 0;
              NewProjectile.Speed := 0.75;
              NewProjectile.HitIncidental := 1;
              NewProjectile.HitTarget := 1;
              NewProjectile.UseLineOfSight := true;
              NewProjectile.Alpha := 20;
              if ( j = 0 ) and ( ( i mod 4 ) = 0 ) then
              begin
                NewProjectile.GlowEffect := TGlow( Sprites.NewSprite( TGlow, nil, Source.X, Source.Y - 1, 0, 0 ) );
                NewProjectile.GlowEffect.RFactor := 10;
                NewProjectile.GlowEffect.GFactor := 20;
                NewProjectile.GlowEffect.BFactor := 30;
                NewProjectile.GlowEffect.Alpha := 100;
              end;
              NewProjectile.Launch( Source, nil, p^.X + Random( 20 ) - 10, p^.Y + Random( 20 ) - 10 );
            end;
          end;

          inc( p );
        end;
        GlobalUnlock( Path );
        GlobalFree( Path );

      //Second fork------------------
        List := GetNearbyCharacter( TCharacter( Target ), 100 );
        if assigned( List ) then
        try
          for i := List.count - 1 downto 0 do
          begin
            if List.Objects[ i ] = Source then
              List.Delete( i )
            else if not Source.IsEnemy( TCharacter( List.Objects[ i ] ) ) then
              List.Delete( i );
          end;
          if List.count > 0 then
          begin
            NewTarget := TCharacter( List.objects[ random( List.count ) ] );
            UseLineOfSight := NewTarget.UseLineOfSight;
            NewTarget.UseLineOfSight := true;
            PathCount := Game.FindPath( NewTarget, C.X, C.Y, Source.Coordination div 2, Path );
            NewTarget.UseLineOfSight := UseLineOfSight;
            if PathCount > 0 then
            begin
              if PathCount > 40 then
              begin
                PathCount := 40;
              end
              else if PathCount > 10 + Source.Mysticism then
              begin
                PathCount := 10 + Source.Mysticism;
              end
              else
                NewDoDamage := true;

              if NewDoDamage then
              begin
                Effect := TEffect.Create;
                Effect.Resource := ReceiveResource;
                Effect.AnimationDuration := 8 * ReceiveResource.FrameMultiplier;
                Effect.Power := Source.Mysticism;
                Effect.DoAction( 'Default', Target.FacingString );
                NewTarget.AddEffect( Effect );
              end;

              p := GlobalLock( Path );
              for i := 1 to PathCount do
              begin
                if i < PathCount then
                begin
                  inc( p );
                  dX := p^.X;
                  dY := p^.y;
                  dec( p );
                  dX := dX - p^.X;
                  dY := dY - p^.Y;
                  k := 3;
                end
                else
                begin
                  dX := 0;
                  dY := 0;
                  k := 0;
                end;
                for j := 0 to k do
                begin
                  NewProjectile := TProjectile( Sprites.NewSprite( TLightningProjectile, Res, p^.X + random( 4 ) - 2 + j * dX div 4, p^.Y + random( 4 ) - 2 + j * dY div 4, Z1 + random( 4 ) - 2, 1 ) );
                  if assigned( NewProjectile ) then
                  begin
                    ZeroMemory( @NewProjectile.Damage, sizeof( NewProjectile.Damage ) );
                    NewProjectile.SpecialEffect := seAdd;
                    NewProjectile.Frame := random( 6 ) + 1;
                    NewProjectile.Duration := 4;
                    NewProjectile.TrackingDegree := 0;
                    NewProjectile.Speed := 0.75;
                    NewProjectile.HitIncidental := 1;
                    NewProjectile.HitTarget := 1;
                    NewProjectile.UseLineOfSight := true;
                    NewProjectile.Alpha := 20;
                    if ( j = 0 ) and ( ( i mod 4 ) = 0 ) then
                    begin
                      NewProjectile.GlowEffect := TGlow( Sprites.NewSprite( TGlow, nil, Source.X, Source.Y - 1, 0, 0 ) );
                      NewProjectile.GlowEffect.RFactor := 10;
                      NewProjectile.GlowEffect.GFactor := 20;
                      NewProjectile.GlowEffect.BFactor := 30;
                      NewProjectile.GlowEffect.Alpha := 100;
                    end;
                    NewProjectile.Launch( Source, nil, p^.X + Random( 20 ) - 10, p^.Y + Random( 20 ) - 10 );
                  end;
                end;

                inc( p );
              end;
              GlobalUnlock( Path );
              GlobalFree( Path );
            end;

            if NewDoDamage then
            begin
              NewTarget.AffectDamage( Source, @Damage );
              Total := CalcTotalDamage( Damage, NewTarget.Resistance, 1, false );
              Stun := CalcDamage( Damage.Stun ) - NewTarget.Resistance.Stun.Invulnerability;
              if Stun > 0 then
                Stun := Stun * ( 1 - NewTarget.Resistance.Stun.Resistance );
              NewTarget.TakeDamage( Source, Total, Stun, false );
            end;
          end;

        finally
          List.free;
        end;
      //------------------

        if DoDamage then
        begin
          if ValidTarget then
          begin
            TCharacter( Target ).AffectDamage( Source, @Damage );
            Total := CalcTotalDamage( Damage, TCharacter( Target ).Resistance, 1, false );
            Stun := CalcDamage( Damage.Stun ) - TCharacter( Target ).Resistance.Stun.Invulnerability;
            if Stun > 0 then
              Stun := Stun * ( 1 - TCharacter( Target ).Resistance.Stun.Resistance );
            with Target as TCharacter do
            begin
              TakeDamage( Source, Total, Stun, false );
            end;
          end;
        end;
      end;
    end
    else if Source.TitleExists( 'Lightning' ) then
    begin
      UseLineOfSight := Source.UseLineOfSight;
      Source.UseLineOfSight := true;
      PathCount := Game.FindPath( Source, TargetX, TargetY, Source.Coordination div 2, Path );
      Source.UseLineOfSight := UseLineOfSight;
      if PathCount > 0 then
      begin
        if PathCount > 40 then
        begin
          PathCount := 40;
          DoDamage := false;
        end
        else if PathCount > 10 + Source.Mysticism then
        begin
          PathCount := 10 + Source.Mysticism;
          DoDamage := false;
        end
        else
          DoDamage := true;

        if DoDamage and ValidTarget then
        begin
          Effect := TEffect.Create;
          Effect.Resource := ReceiveResource;
          Effect.AnimationDuration := 8 * ReceiveResource.FrameMultiplier;
          Effect.Power := Source.Mysticism;
          Effect.DoAction( 'Default', Target.FacingString );
          TCharacter( Target ).AddEffect( Effect );
        end;

        result := inherited Cast( Source, Target );
        if not result then
        begin
          GlobalFree( Path );
          Exit;
        end;

        Z1 := Source.Height div 2 - 16;
        Resource2.DrawShadow := false;
        p := GlobalLock( Path );
        for i := 1 to PathCount do
        begin
          if i < PathCount then
          begin
            inc( p );
            dX := p^.X;
            dY := p^.y;
            dec( p );
            dX := dX - p^.X;
            dY := dY - p^.Y;
            k := 3;
          end
          else
          begin
            dX := 0;
            dY := 0;
            k := 0;
          end;
          for j := 0 to k do
          begin
            NewProjectile := TProjectile( Sprites.NewSprite( TLightningProjectile, Resource2, p^.X + random( 4 ) - 2 + j * dX div 4, p^.Y + random( 4 ) - 2 + j * dY div 4, Z1 + random( 4 ) - 2, 1 ) );
            if assigned( NewProjectile ) then
            begin
              ZeroMemory( @NewProjectile.Damage, sizeof( NewProjectile.Damage ) );
              NewProjectile.SpecialEffect := seAdd;
              NewProjectile.Frame := random( 6 ) + 1;
              NewProjectile.Duration := 4;
              NewProjectile.TrackingDegree := 0;
              NewProjectile.Speed := 0.75;
              NewProjectile.HitIncidental := 1;
              NewProjectile.HitTarget := 1;
              NewProjectile.UseLineOfSight := true;
              NewProjectile.Alpha := 20;
              if ( j = 0 ) and ( ( i mod 4 ) = 0 ) then
              begin
                NewProjectile.GlowEffect := TGlow( Sprites.NewSprite( TGlow, nil, Source.X, Source.Y - 1, 0, 0 ) );
                NewProjectile.GlowEffect.RFactor := 10;
                NewProjectile.GlowEffect.GFactor := 20;
                NewProjectile.GlowEffect.BFactor := 30;
                NewProjectile.GlowEffect.Alpha := 100;
              end;
              NewProjectile.Launch( Source, nil, p^.X + Random( 20 ) - 10, p^.Y + Random( 20 ) - 10 );
            end;
          end;

          inc( p );
        end;
        GlobalUnlock( Path );
        GlobalFree( Path );

        if DoDamage then
        begin
          ZeroMemory( @Damage, sizeof( Damage ) );
          Damage.Heat.Min := 0;
          Damage.Heat.Max := Source.Mysticism / 4;
          Damage.Electric.Min := Source.Mysticism / 4;
          Damage.Electric.Max := Source.Mysticism * 2;
          Damage.Stun.Min := 0;
          Damage.Stun.Max := Source.Mysticism / 4;

          if ValidTarget then
          begin
            TCharacter( Target ).AffectDamage( Source, @Damage );
            Total := CalcTotalDamage( Damage, TCharacter( Target ).Resistance, 1, false );
            Stun := CalcDamage( Damage.Stun ) - TCharacter( Target ).Resistance.Stun.Invulnerability;
            if Stun > 0 then
              Stun := Stun * ( 1 - TCharacter( Target ).Resistance.Stun.Resistance );
            with Target as TCharacter do
            begin
              TakeDamage( Source, Total, Stun, false );
            end;
          end;
        end;
      end;
    end
    else
    begin
      UseLineOfSight := Source.UseLineOfSight;
      Source.UseLineOfSight := true;
      PathCount := Game.FindPath( Source, TargetX, TargetY, Source.Coordination div 4, Path );
      Source.UseLineOfSight := UseLineOfSight;
      if PathCount > 0 then
      begin
        DoDamage := true;
        if PathCount > 20 then
        begin
          PathCount := 20;
          DoDamage := false;
        end;
        if PathCount > 5 + Source.Mysticism div 2 then
        begin
          PathCount := 5 + Source.Mysticism div 2;
          DoDamage := false;
        end;

        if DoDamage and ValidTarget then
        begin
          Effect := TEffect.Create;
          Effect.Resource := ReceiveResource;
          Effect.AnimationDuration := 8 * ReceiveResource.FrameMultiplier;
          Effect.Power := Source.Mysticism;
          Effect.DoAction( 'Default', Target.FacingString );
          TCharacter( Target ).AddEffect( Effect );
        end;

        result := inherited Cast( Source, Target );
        if not result then
        begin
          GlobalFree( Path );
          Exit;
        end;

        Z1 := Source.Height div 2 - 16;
        Resource1.DrawShadow := false;
        p := GlobalLock( Path );
        for i := 1 to PathCount do
        begin
          if i < PathCount then
          begin
            inc( p );
            dX := p^.X;
            dY := p^.y;
            dec( p );
            dX := dX - p^.X;
            dY := dY - p^.Y;
            k := 7;
          end
          else
          begin
            dX := 0;
            dY := 0;
            k := 0;
          end;
          for j := 0 to k do
          begin
            NewProjectile := TProjectile( Sprites.NewSprite( TLightningProjectile, Resource1, p^.X + random( 4 ) - 2 + j * dX div 8, p^.Y + random( 4 ) - 2 + j * dY div 8, Z1 + random( 4 ) - 2, 1 ) );
            if assigned( NewProjectile ) then
            begin
              ZeroMemory( @NewProjectile.Damage, sizeof( NewProjectile.Damage ) );
              NewProjectile.SpecialEffect := seAdd;
              NewProjectile.Duration := 4;
              NewProjectile.TrackingDegree := 0;
              NewProjectile.Speed := 0.75;
              NewProjectile.HitIncidental := 1;
              NewProjectile.HitTarget := 1;
              NewProjectile.UseLineOfSight := true;
              NewProjectile.Alpha := 20;
              if ( j = 0 ) and ( ( i mod 4 ) = 0 ) then
              begin
                NewProjectile.GlowEffect := TGlow( Sprites.NewSprite( TGlow, nil, Source.X, Source.Y - 1, 0, 0 ) );
                NewProjectile.GlowEffect.RFactor := 10;
                NewProjectile.GlowEffect.GFactor := 20;
                NewProjectile.GlowEffect.BFactor := 30;
                NewProjectile.GlowEffect.Alpha := 100;
              end;
              NewProjectile.Launch( Source, nil, p^.X + Random( 20 ) - 10, p^.Y + Random( 20 ) - 10 );
            end;
          end;

          inc( p );
        end;
        GlobalUnlock( Path );
        GlobalFree( Path );

        if DoDamage then
        begin
          ZeroMemory( @Damage, sizeof( Damage ) );
          Damage.Heat.Min := 0;
          Damage.Heat.Max := Source.Mysticism / 4;
          Damage.Electric.Min := 1;
          Damage.Electric.Max := Source.Mysticism * 1.5;
          Damage.Stun.Min := 0;
          Damage.Stun.Max := Source.Mysticism / 4;

          if ValidTarget then
          begin
            TCharacter( Target ).AffectDamage( Source, @Damage );
            Total := CalcTotalDamage( Damage, TCharacter( Target ).Resistance, 1, false );
            Stun := CalcDamage( Damage.Stun ) - TCharacter( Target ).Resistance.Stun.Invulnerability;
            if Stun > 0 then
              Stun := Stun * ( 1 - TCharacter( Target ).Resistance.Stun.Resistance );
            with Target as TCharacter do
            begin
              TakeDamage( Source, Total, Stun, false );
            end;
          end;
        end;
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TLightning.Clear;
begin
  Resource1.RLE.free; Resource1.RLE := nil;
  Resource2.RLE.free; Resource2.RLE := nil;
  Resource3.RLE.free; Resource3.RLE := nil;
  ReceiveResource.RLE.free; ReceiveResource.RLE := nil;
end;

constructor TLightning.Create;
const
  FailName : string = 'TLightning.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    CastingType := ctCombat;
    TargetType := ttEnemy;
    CastEffect := LightningEffect;
    Resource1 := LoadArtResource( 'engine\weaponprojectiles\smalllightning.gif', true );
    Resource2 := LoadArtResource( 'engine\weaponprojectiles\medlightning.gif', true );
    Resource3 := LoadArtResource( 'engine\weaponprojectiles\lrglightning.gif', true );
    ReceiveResource := LoadArtResource( 'engine\spells\ShockReceive.gif', true );
    LoadCastSounds( 'shock1,shock2' );
    Resource1.DrawShadow := false;
    Resource2.DrawShadow := false;
    Resource3.DrawShadow := false;
    ReceiveResource.DrawShadow := false;
    SoundInCast := false;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TLightning.Destroy;
const
  FailName : string = 'TLightning.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    Resource1.Free;
    Resource2.Free;
    Resource3.Free;
    ReceiveResource.Free;
    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TLightning.Drain( Source : TCharacter ) : Single;
const
  FailName : string = 'TLightning.Drain';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 5 + round( Source.Restriction / 10 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TLightning.GetIconXY( Source : TCharacter ) : TPoint;
const
  FailName : string = 'TLightning.GetIconXY';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result.X := 11 * 32;
    result.Y := 32;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TLightning.GetInfo( Source : TCharacter ) : string;
begin
  result := InfoText;
  if Source.TitleExists( 'Forked Lightning' ) then
  begin
    Replace( result, 'a', '2x(' + inttostr( round( Source.Mysticism / 4 ) ) + '-' + inttostr( round( Source.Mysticism * 2 ) ) + ')' );
    Replace( result, 'b', '2x(0-' + inttostr( round( Source.Mysticism / 4 ) ) + ')' );
    Replace( result, 'c', '2x(0-' + inttostr( round( Source.Mysticism / 4 ) ) + ')' );
  end
  else if Source.TitleExists( 'Lightning' ) then
  begin
    Replace( result, 'a', inttostr( round( Source.Mysticism / 4 ) ) + '-' + inttostr( round( Source.Mysticism * 2 ) ) );
    Replace( result, 'b', '0-' + inttostr( round( Source.Mysticism / 4 ) ) );
    Replace( result, 'c', '0-' + inttostr( round( Source.Mysticism / 4 ) ) );
  end
  else
  begin
    Replace( result, 'a', '1-' + inttostr( round( Source.Mysticism * 1.5 ) ) );
    Replace( result, 'b', '0-' + inttostr( round( Source.Mysticism / 4 ) ) );
    Replace( result, 'c', '0-' + inttostr( round( Source.Mysticism / 4 ) ) );
  end;
end;

function TLightning.GetLoaded : Boolean;
const
  FailName : string = 'TLightning.GetLoaded';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := Resource1.Loaded and Resource2.Loaded and Resource3.Loaded and ReceiveResource.Loaded;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

class function TLightning.GetName : string;
begin
  result := 'Shock';
end;

function TLightning.Range( Source : TCharacter ) : Integer;
const
  FailName : string = 'TLightning.Range';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if Source.TitleExists( 'Lightning' ) then
    begin
      result := ( 10 + Source.Mysticism ) * 8;
      if result > 400 then
        result := 400;
    end
    else
    begin
      result := ( 5 + Source.Mysticism ) * 4;
      if result > 100 then
        result := 100;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TLightning.Recovery( Source : TCharacter ) : Integer;
const
  FailName : string = 'TLightning.Recovery';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 40 - ( Source.Constitution div 2 );
    if result < 0 then
      result := 0;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TPush }

function TPush.Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean;
var
  R, D, dX, dY : double;
  sX, sY : longint;
  Damage : TDamageProfile;
  Total, Stun : single;
const
  FailName : string = 'TPush.Cast';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := False;
    if not assigned( Target ) then
      exit;
    if Target.UnMoveable then
      exit;
    if not ( Target is TCharacter ) then
      exit;
    if TCharacter( Target ).Dead then
      exit;
    if not Game.LineOfSight( Source.X, Source.Y, Target.X, Target.Y ) then
      exit;

    result := inherited Cast( Source, Target );
    if not result then
      Exit;

    if Target is TCharacter then
    begin
      ZeroMemory( @Damage, sizeof( Damage ) );
      Damage.Crushing.Min := Source.Mysticism / 12;
      Damage.Crushing.Max := Source.Mysticism / 10;
      if TCharacter( Target ).Strength >= 1 then
      begin
        Damage.Stun.Min := 16 + round( Source.Mysticism * Source.Constitution / TCharacter( Target ).Strength / 5 );
        Damage.Stun.Max := 16 + round( Source.Mysticism * Source.Constitution / TCharacter( Target ).Strength / 4 );
      end
      else
      begin
        Damage.Stun.Min := 16 + round( Source.Mysticism * Source.Constitution / 5 );
        Damage.Stun.Max := 16 + round( Source.Mysticism * Source.Constitution / 4 );
      end;
      TCharacter( Target ).AffectDamage( Source, @Damage );
      Total := CalcTotalDamage( Damage, TCharacter( Target ).Resistance, 1, false );
      Stun := CalcDamage( Damage.Stun ) - TCharacter( Target ).Resistance.Stun.Invulnerability;
      if Stun > 0 then
        Stun := Stun * ( 1 - TCharacter( Target ).Resistance.Stun.Resistance );

      with Target as TCharacter do
      begin
        dX := X - Source.X;
        dY := Y - Source.Y;
        D := sqrt( sqr( dX ) + sqr( dY ) );
        if D = 0 then
          exit;
        R := TakeDamage( Source, Total, Stun, true ) / 2;
        if R > 0 then
        begin
          Speed := Source.Mysticism / 5;
          R := R * Speed;
          sX := round( R * dX / D );
          sY := round( R * dY / D );
          Facing := Character.GetFacing( sX, sY, 0, 0 );
          MoveTo( X + sX, Y + sY, Z );
        end;
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

constructor TPush.Create;
const
  FailName : string = 'TPush.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    CastingType := ctCombat;
    TargetType := ttEnemy;
    CastEffect := PushEffect;
    LoadCastSounds( 'pushspell3' );
    SoundInCast := false;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TPush.Destroy;
const
  FailName : string = 'TPush.Destroy';
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

function TPush.Drain( Source : TCharacter ) : Single;
const
  FailName : string = 'TPush.Drain';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 5 + round( Source.Restriction / 10 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TPush.GetIconXY( Source : TCharacter ) : TPoint;
const
  FailName : string = 'TPush.GetIconXY';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result.X := 10 * 32;
    result.Y := 32;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TPush.GetInfo( Source : TCharacter ) : string;
begin
  result := InfoText;
  Replace( result, 'a', inttostr( 16 + round( Source.Mysticism * Source.Constitution / 5 ) ) + '-' +
    inttostr( 16 + round( Source.Mysticism * Source.Constitution / 4 ) ) );
end;

function TPush.GetLoaded : Boolean;
const
  FailName : string = 'TPush.GetLoaded';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := True;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

class function TPush.GetName : string;
begin
  result := 'Push';
end;

function TPush.Range( Source : TCharacter ) : Integer;
const
  FailName : string = 'TPush.Range';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 10 * ( Source.Constitution + Source.Mysticism );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TPush.Recovery( Source : TCharacter ) : Integer;
const
  FailName : string = 'TPush.Recovery';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ THeal }

function THeal.Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean;
var
  F, Points : single;
  Effect : TEffect;
  NewTarget : TCharacter;
  D : single;
const
  FailName : string = 'THeal.Cast';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := False;
    if assigned( Target ) then
    begin
      if not ( Target is TCharacter ) then
        exit;
      if TCharacter( Target ).Dead then
        exit;
      if not Game.LineOfSight( Source.X, Source.Y, Target.X, Target.Y ) then
        exit;
      NewTarget := TCharacter( Target );
    end
    else
      NewTarget := Source;

    result := inherited Cast( Source, NewTarget );
    if not result then
      Exit;

    Effect := TEffect.Create;
    Effect.Resource := Resource;
    Effect.AnimationDuration := 8 * Resource.FrameMultiplier;
    Effect.Power := Source.Mysticism;
    Effect.DoAction( 'Default', NewTarget.FacingString );

    if Source.TitleExists( 'Traumatic Healing' ) then
    begin
      F := ( Source.Mysticism + NewTarget.Constitution ) / 128 + ( random / 10 ) - 0.05;
      D := 25;
    end
    else if Source.TitleExists( 'Greater Healing' ) then
    begin
      F := ( Source.Mysticism + NewTarget.Constitution ) / 192 + ( random / 10 ) - 0.05;
      D := 20;
    end
    else if Source.TitleExists( 'Healing' ) then
    begin
      F := ( Source.Mysticism + NewTarget.Constitution ) / 256 + ( random / 10 ) - 0.05;
      D := 15;
    end
    else if Source.TitleExists( 'Minor Healing' ) then
    begin
      F := ( Source.Mysticism + NewTarget.Constitution ) / 320 + ( random / 10 ) - 0.05;
      D := 10;
    end
    else
    begin
      F := ( Source.Mysticism + NewTarget.Constitution ) / 384 + ( random / 10 ) - 0.05;
      D := 5;
    end;

    with NewTarget do
    begin
      if F < 0 then
        F := 0
      else if F > 1 then
        F := 1;
      Points := D + HitPoints * F;
      if Points > Wounds then
        Points := Wounds;
      Wounds := Wounds - Points;
      AddEffect( Effect );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure THeal.Clear;
begin
  Resource.RLE.Free; Resource.RLE := nil;
end;

constructor THeal.Create;
const
  FailName : string = 'THeal.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    CastingType := ctHealing;
    TargetType := ttFriend;
    CastEffect := HealEffect;
    Resource := LoadArtResource( 'engine\spells\healreceive.gif', true );
    LoadCastSounds( 'heal' );
    Resource.DrawShadow := false;
    SoundInCast := false;
    Interupted := false;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor THeal.Destroy;
const
  FailName : string = 'THeal.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    Resource.Free;
    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function THeal.Drain( Source : TCharacter ) : Single;
const
  FailName : string = 'THeal.Drain';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 10 + round( Source.Restriction / 10 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function THeal.GetIconXY( Source : TCharacter ) : TPoint;
const
  FailName : string = 'THeal.GetIconXY';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if Source.TitleExists( 'Traumatic Healing' ) then
    begin
      result.X := 16 * 32;
      result.Y := 32;
    end
    else if Source.TitleExists( 'Greater Healing' ) then
    begin
      result.X := 16 * 32;
      result.Y := 0;
    end
    else if Source.TitleExists( 'Major Healing' ) then
    begin
      result.X := 15 * 32;
      result.Y := 32;
    end
    else if Source.TitleExists( 'Minor Healing' ) then
    begin
      result.X := 15 * 32;
      result.Y := 0;
    end
    else
    begin
      result.X := 14 * 32;
      result.Y := 0;
    end

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function THeal.GetInfo( Source : TCharacter ) : string;
begin
  result := InfoText;
  if Source.TitleExists( 'Traumatic Healing' ) then
  begin
    Replace( result, 'a', '25' );
    Replace( result, 'b', inttostr( round( 100 * ( Source.Mysticism ) / 128 ) ) );
  end
  else if Source.TitleExists( 'Greater Healing' ) then
  begin
    Replace( result, 'a', '20' );
    Replace( result, 'b', inttostr( round( 100 * ( Source.Mysticism ) / 192 ) ) );
  end
  else if Source.TitleExists( 'Major Healing' ) then
  begin
    Replace( result, 'a', '15' );
    Replace( result, 'b', inttostr( round( 100 * ( Source.Mysticism ) / 256 ) ) );
  end
  else if Source.TitleExists( 'Minor Healing' ) then
  begin
    Replace( result, 'a', '10' );
    Replace( result, 'b', inttostr( round( 100 * ( Source.Mysticism ) / 320 ) ) );
  end
  else
  begin
    Replace( result, 'a', '5' );
    Replace( result, 'b', inttostr( round( 100 * ( Source.Mysticism ) / 384 ) ) );
  end
end;

function THeal.GetLoaded : Boolean;
const
  FailName : string = 'THeal.GetLoaded';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if assigned( Resource ) then
      result := Resource.Loaded;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

class function THeal.GetName : string;
begin
  result := 'Heal';
end;

function THeal.Range( Source : TCharacter ) : Integer;
const
  FailName : string = 'THeal.Range';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 10 * ( Source.Constitution + Source.Mysticism );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function THeal.Recovery( Source : TCharacter ) : Integer;
const
  FailName : string = 'THeal.Recovery';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 40 - ( Source.Constitution div 2 );
    if result < 0 then
      result := 0;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TWither }

function TWither.Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean;
var
  F, Points, Total : single;
  Effect : TEffect;
  Damage : TDamageProfile;
  D : single;
const
  FailName : string = 'TWither.Cast';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := False;
    if not assigned( Target ) then
      exit;
    if not ( Target is TCharacter ) then
      exit;
    if TCharacter( Target ).Dead then
      exit;
    if not Game.LineOfSight( Source.X, Source.Y, Target.X, Target.Y ) then
      exit;

    result := inherited Cast( Source, Target );
    if not result then
      Exit;

    Effect := TEffect.Create;
    Effect.Resource := Resource;
    Effect.AnimationDuration := 8 * Resource.FrameMultiplier;
    Effect.Power := Source.Mysticism;
    Effect.DoAction( 'Default', Target.FacingString );

    if Source.TitleExists( 'Infuse' ) then
    begin
      F := Source.Mysticism / 96 + ( random / 10 ) - 0.05;
      D := 10;
    end
    else if Source.TitleExists( 'Energy' ) then
    begin
      F := Source.Mysticism / 128 + ( random / 10 ) - 0.05;
      D := 5;
    end
    else
    begin
      F := Source.Mysticism / 160 + ( random / 10 ) - 0.05;
      D := 0;
    end;

    if F < 0 then
      F := 0
    else if F > 1 then
      F := 1;
    Points := D + TCharacter( Target ).HitPoints * F;
    ZeroMemory( @Damage, sizeof( Damage ) );
    Damage.Magic.Min := Points;
    Damage.Magic.Max := Points;
    TCharacter( Target ).AffectDamage( Source, @Damage );
    Total := CalcTotalDamage( Damage, TCharacter( Target ).Resistance, 1, false );
    TCharacter( Target ).AddEffect( Effect );
    TCharacter( Target ).TakeDamage( Source, Total, 0, true );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TWither.Clear;
begin
  Resource.RLE.free; Resource.RLE := nil;
end;

constructor TWither.Create;
const
  FailName : string = 'TWither.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    CastingType := ctCombat;
    TargetType := ttEnemy;
    CastEffect := ChargeEffect;
    Resource := LoadArtResource( 'engine\spells\charge4receive.gif', true );
    LoadCastSounds( 'Charge' );
    Resource.DrawShadow := false;
    SoundInCast := true;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TWither.Destroy;
const
  FailName : string = 'TWither.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    Resource.Free;
    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TWither.Drain( Source : TCharacter ) : Single;
const
  FailName : string = 'TWither.Drain';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 5 + round( Source.Restriction / 10 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TWither.GetIconXY( Source : TCharacter ) : TPoint;
const
  FailName : string = 'TWither.GetIconXY';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result.X := 10 * 32;
    result.Y := 0;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TWither.GetInfo( Source : TCharacter ) : string;
begin
  result := InfoText;
end;

function TWither.GetLoaded : Boolean;
const
  FailName : string = 'TWither.GetLoaded';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if assigned( Resource ) then
      result := Resource.Loaded;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

class function TWither.GetName : string;
begin
  result := 'Charge';
end;

function TWither.Range( Source : TCharacter ) : Integer;
const
  FailName : string = 'TWither.Range';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := Round( Source.Mysticism * 50 * ( 24 + Source.Mysticism / 10 ) );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TWither.Recovery( Source : TCharacter ) : Integer;
const
  FailName : string = 'TWither.Recovery';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 40 - ( ( Source.Coordination + Source.Mysticism ) div 4 );
    if result < 0 then
      result := 0;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TCharge }

function TCharge.Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean;
var
  NewProjectile : TProjectile;
  Z1, Z2 : Integer;
const
  FailName : string = 'TCharge.Cast';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := inherited Cast( Source, Target );
    if not result then
      Exit;
    Z1 := Source.Height div 2 - 16;
    if Assigned( Target ) then
      Z2 := 0
    else
      Z2 := Z1;
    if Source.TitleExists( 'Infuse' ) then
    begin
      NewProjectile := TProjectile( Sprites.NewSprite( TChargeProjectile, Resource3, Source.X, Source.Y, Z1, 1 ) );
      if assigned( NewProjectile ) then
      begin
        NewProjectile.Magic := Source.Mysticism * 2;
        ZeroMemory( @NewProjectile.Damage, sizeof( NewProjectile.Damage ) );
        NewProjectile.Damage.Heat.Min := 0;
        NewProjectile.Damage.Heat.Max := Source.Mysticism / 4;
        NewProjectile.Damage.Electric.Min := 1;
        NewProjectile.Damage.Electric.Max := Source.Mysticism / 2;
        NewProjectile.DamageRadius := 0;
        NewProjectile.Duration := Source.Constitution * 10;
        NewProjectile.TrackingDegree := Source.Coordination / 3;
        NewProjectile.Speed := 8 + Source.Coordination / 10;
        NewProjectile.SpecialEffect := seAdd;
        if Source.Coordination <= 10 then
          NewProjectile.HitIncidental := 1
        else
          NewProjectile.HitIncidental := 10 / Source.Coordination;
        NewProjectile.HitTarget := 1;
        NewProjectile.UseLineOfSight := True;
        NewProjectile.GlowEffect := TGlow( Sprites.NewSprite( TGlow, nil, Source.X, Source.Y - 1, 0, 0 ) );
        NewProjectile.GlowEffect.RFactor := 25;
        NewProjectile.GlowEffect.GFactor := 25;
        NewProjectile.GlowEffect.BFactor := 50;
        NewProjectile.GlowEffect.Alpha := 10;
        NewProjectile.Launch( Source, Target, Source.TargetX, Source.TargetY + Z2 );
      end;
    end
    else if Source.TitleExists( 'Energy' ) then
    begin
      NewProjectile := TProjectile( Sprites.NewSprite( TChargeProjectile, Resource2, Source.X, Source.Y, Z1, 1 ) );
      if assigned( NewProjectile ) then
      begin
        NewProjectile.Magic := 3 * Source.Mysticism div 2;
        ZeroMemory( @NewProjectile.Damage, sizeof( NewProjectile.Damage ) );
        NewProjectile.Damage.Heat.Min := 0;
        NewProjectile.Damage.Heat.Max := Source.Mysticism / 8;
        NewProjectile.Damage.Electric.Min := 1;
        NewProjectile.Damage.Electric.Max := Source.Mysticism / 4;
        NewProjectile.DamageRadius := 0;
        NewProjectile.Duration := Source.Constitution * 10;
        NewProjectile.TrackingDegree := Source.Coordination / 4;
        NewProjectile.Speed := 8 + Source.Coordination / 10;
        NewProjectile.SpecialEffect := seAdd;
        if Source.Coordination <= 10 then
          NewProjectile.HitIncidental := 1
        else
          NewProjectile.HitIncidental := 10 / Source.Coordination;
        NewProjectile.HitTarget := 1;
        NewProjectile.UseLineOfSight := True;
        NewProjectile.GlowEffect := TGlow( Sprites.NewSprite( TGlow, nil, Source.X, Source.Y - 1, 0, 0 ) );
        NewProjectile.GlowEffect.RFactor := 25;
        NewProjectile.GlowEffect.GFactor := 25;
        NewProjectile.GlowEffect.BFactor := 50;
        NewProjectile.GlowEffect.Alpha := 10;
        NewProjectile.Launch( Source, Target, Source.TargetX, Source.TargetY + Z2 );
      end;
    end
    else
    begin
      NewProjectile := TProjectile( Sprites.NewSprite( TChargeProjectile, Resource1, Source.X, Source.Y, Z1, 1 ) );
      if assigned( NewProjectile ) then
      begin
        NewProjectile.Magic := Source.Mysticism;
        ZeroMemory( @NewProjectile.Damage, sizeof( NewProjectile.Damage ) );
        NewProjectile.Damage.Heat.Min := 0;
        NewProjectile.Damage.Heat.Max := Source.Mysticism / 16;
        NewProjectile.Damage.Electric.Min := 1;
        NewProjectile.Damage.Electric.Max := Source.Mysticism / 8;
        NewProjectile.DamageRadius := 0;
        NewProjectile.Duration := Source.Constitution * 10;
        NewProjectile.TrackingDegree := Source.Coordination / 5;
        NewProjectile.Speed := 8 + Source.Coordination / 10;
        NewProjectile.SpecialEffect := seAdd;
        if Source.Coordination <= 10 then
          NewProjectile.HitIncidental := 1
        else
          NewProjectile.HitIncidental := 10 / Source.Coordination;
        NewProjectile.HitTarget := 1;
        NewProjectile.UseLineOfSight := True;
        NewProjectile.GlowEffect := TGlow( Sprites.NewSprite( TGlow, nil, Source.X, Source.Y - 1, 0, 0 ) );
        NewProjectile.GlowEffect.RFactor := 25;
        NewProjectile.GlowEffect.GFactor := 25;
        NewProjectile.GlowEffect.BFactor := 50;
        NewProjectile.GlowEffect.Alpha := 10;
        NewProjectile.Launch( Source, Target, Source.TargetX, Source.TargetY + Z2 );
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

constructor TCharge.Create;
const
  FailName : string = 'TCharge.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    CastingType := ctCombat;
    TargetType := ttEnemy;
    CastEffect := LightningEffect;
    Resource1 := TLightning( Lightning ).SmallResource; //LoadArtResource('engine\WeaponProjectiles\smalllightning.gif');
    Resource2 := TLightning( Lightning ).MediumResource; //LoadArtResource('engine\WeaponProjectiles\medlightning.gif');
    Resource3 := TLightning( Lightning ).LargeResource; //LoadArtResource('engine\WeaponProjectiles\lrglightning.gif');
    LoadCastSounds( 'Charge' );
//  Resource1.DrawShadow:=false;
//  Resource2.DrawShadow:=false;
//  Resource3.DrawShadow:=false;
    SoundInCast := true;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TCharge.Destroy;
const
  FailName : string = 'TCharge.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

//  Resource1.Free;
//  Resource2.Free;
//  Resource3.Free;
    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TCharge.Drain( Source : TCharacter ) : Single;
const
  FailName : string = 'TCharge.Drain';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 1;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TCharge.GetIconXY( Source : TCharacter ) : TPoint;
const
  FailName : string = 'TCharge.GetIconXY';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result.X := 10 * 32;
    result.Y := 0;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TCharge.GetInfo( Source : TCharacter ) : string;
begin
  result := InfoText;
  if Source.TitleExists( 'Infuse' ) then
  begin
    Replace( result, 'a', '1-' + inttostr( round( Source.Mysticism / 2 ) ) );
    Replace( result, 'b', '0-' + inttostr( round( Source.Mysticism / 4 ) ) );
  end
  else if Source.TitleExists( 'Energy' ) then
  begin
    Replace( result, 'a', '1-' + inttostr( round( Source.Mysticism / 4 ) ) );
    Replace( result, 'b', '0-' + inttostr( round( Source.Mysticism / 8 ) ) );
  end
  else
  begin
    Replace( result, 'a', '1-' + inttostr( round( Source.Mysticism / 8 ) ) );
    Replace( result, 'b', '0-' + inttostr( round( Source.Mysticism / 16 ) ) );
  end;
end;

function TCharge.GetLoaded : Boolean;
const
  FailName : string = 'TCharge.GetLoaded';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := Resource1.Loaded and Resource2.Loaded and Resource3.Loaded;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

class function TCharge.GetName : string;
begin
  result := 'Charge';
end;

function TCharge.Range( Source : TCharacter ) : Integer;
const
  FailName : string = 'TCharge.Range';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := Round( Source.Constitution * 10 * ( 8 + Source.Coordination / 10 ) );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TCharge.Recovery( Source : TCharacter ) : Integer;
const
  FailName : string = 'TCharge.Recovery';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 10 - ( Source.Constitution div 2 );
    if result < 0 then
      result := 0;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TSummonRat }

function TSummonRat.Cast( Source : TCharacter;
  Target : TSpriteObject ) : Boolean;
var
  Effect : TEffect;
  NewCharacter : TCompanionCharacter;
  i, CompanionSlot : integer;
const
  FailName : string = 'TSummonRat.Cast';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := False;
    if assigned( Target ) then
      exit;
    if not Game.LineOfSight( Source.X, Source.Y, Source.TargetX, Source.TargetY ) then
      exit;

    CompanionSlot := 0;
    for i := 1 to MaxCompanions do
    begin
      if not assigned( Source.Companion[ i ] ) then
      begin
        CompanionSlot := i;
        break;
      end;
    end;
    if CompanionSlot = 0 then
      exit;

    result := inherited Cast( Source, Target );
    if not result then
      Exit;

    NewCharacter := TCompanionCharacter( Sprites.NewSprite( TCompanionCharacter, RatResource, Source.TargetX, Source.TargetY, 0, 1 ) );
    if assigned( NewCharacter ) then
    begin
      Source.Companion[ CompanionSlot ] := NewCharacter;

      Effect := TEffect.Create;
      Effect.Resource := Resource;
      Effect.AnimationDuration := 8 * Resource.FrameMultiplier;
      Effect.Power := Source.Mysticism;
      Effect.DoAction( 'Default', NewCharacter.FacingString );

      with NewCharacter do
      begin
        Master := Source;
        Duration := Source.Mysticism * 50;
        Name := 'Rat';
        BaseUnArmedDamage.Piercing.Min := 1;
        BaseUnArmedDamage.Piercing.Max := 6;
        BaseUnArmedDamage.Cutting.Min := 0;
        BaseUnArmedDamage.Cutting.Max := 4;
        BaseResistance.Cutting.Invulnerability := 1;
        BaseResistance.Cutting.Resistance := 0.02;
        BaseResistance.Crushing.Invulnerability := 1;
        BaseResistance.Crushing.Resistance := 0.1;
        Strength := 10;
        Coordination := 15;
        HitPoints := 5 + Source.Mysticism / 10;
        Combat := 10 + Source.Mysticism div 5;
        AI := TMeander.Create;
        PartyAI := 'meander';
        FAIMode := aiParty;
        NewAIMode := aiParty;
        InterfaceLocked := true;
        AddEffect( Effect );
      end;
      frmMain.ChangeFocus( NewCharacter );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

constructor TSummonRat.Create;
const
  FailName : string = 'TSummonRat.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    CastingType := ctSummoning;
    TargetType := ttNone;
    CastEffect := SummonEffect;
    Resource := LoadArtResource( 'engine\spells\summonreceive.gif', true );
    Resource.DrawShadow := false;
    SoundInCast := false;
    LoadCastSounds( 'Summon' );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TSummonRat.Destroy;
const
  FailName : string = 'TSummonRat.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    Resource.Free;
    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TSummonRat.Drain( Source : TCharacter ) : Single;
const
  FailName : string = 'TSummonRat.Drain';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 5 + round( Source.Restriction / 10 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TSummonRat.GetIconXY( Source : TCharacter ) : TPoint;
const
  FailName : string = 'TSummonRat.GetIconXY';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result.X := 5 * 32;
    result.Y := 0;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TSummonRat.GetInfo( Source : TCharacter ) : string;
begin
  result := InfoText;
end;

function TSummonRat.GetLoaded : Boolean;
const
  FailName : string = 'TSummonRat.GetLoaded';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if assigned( Resource ) then
      result := Resource.Loaded;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

class function TSummonRat.GetName : string;
begin
  result := 'Summon Rat';
end;

function TSummonRat.Range( Source : TCharacter ) : Integer;
const
  FailName : string = 'TSummonRat.Range';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 200;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TSummonRat.Recovery( Source : TCharacter ) : Integer;
const
  FailName : string = 'TSummonRat.Recovery';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 20;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TSummonWolf }

function TSummonWolf.Cast( Source : TCharacter;
  Target : TSpriteObject ) : Boolean;
var
  Effect : TEffect;
  NewCharacter : TCompanionCharacter;
  i, CompanionSlot : integer;
const
  FailName : string = 'TSummonWolf.Cast';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if assigned( Target ) then
      exit;
    if not Game.LineOfSight( Source.X, Source.Y, Source.TargetX, Source.TargetY ) then
      exit;

    CompanionSlot := 0;
    for i := 1 to MaxCompanions do
    begin
      if not assigned( Source.Companion[ i ] ) then
      begin
        CompanionSlot := i;
        break;
      end;
    end;
    if CompanionSlot = 0 then
      exit;

    result := inherited Cast( Source, Target );
    if not result then
      Exit;

    NewCharacter := TCompanionCharacter( Sprites.NewSprite( TCompanionCharacter, WolfResource, Source.TargetX, Source.TargetY, 0, 1 ) );
    if assigned( NewCharacter ) then
    begin
      Source.Companion[ CompanionSlot ] := NewCharacter;

      Effect := TEffect.Create;
      Effect.Resource := Resource;
      Effect.AnimationDuration := 8 * Resource.FrameMultiplier;
      Effect.Power := Source.Mysticism;
      Effect.DoAction( 'Default', NewCharacter.FacingString );

      with NewCharacter do
      begin
        Master := Source;
        Duration := Source.Mysticism * 50;
        Name := 'Wolf';
        BaseUnArmedDamage.Piercing.Min := 1;
        BaseUnArmedDamage.Piercing.Max := 10;
        BaseUnArmedDamage.Cutting.Min := 0;
        BaseUnArmedDamage.Cutting.Max := 8;
        BaseResistance.Cutting.Invulnerability := 1;
        BaseResistance.Cutting.Resistance := 0.02;
        BaseResistance.Crushing.Invulnerability := 1;
        BaseResistance.Crushing.Resistance := 0.02;
        BaseResistance.Cold.Invulnerability := 3;
        BaseResistance.Cold.Resistance := 0.25;
        Strength := 20;
        Coordination := 15;
        Alliance := Source.Alliance;
        HitPoints := 10 + Source.Mysticism / 5;
        Combat := 10 + Source.Mysticism div 5;
        addtitle( 'MeleeAgressive' );
        AI := TCompanion.Create;
        TCompanion( AI ).Combative := true;
        TCompanion( AI ).Leader := Source;
        FAIMode := aiIdle;
        NewAIMode := aiIdle;
        AddEffect( Effect );
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

constructor TSummonWolf.Create;
const
  FailName : string = 'TSummonWolf.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    CastingType := ctSummoning;
    TargetType := ttNone;
    CastEffect := SummonEffect;
    Resource := LoadArtResource( 'engine\spells\summonreceive.gif', true );
    Resource.DrawShadow := false;
    SoundInCast := false;
    LoadCastSounds( 'Summon' );
    Interupted := false;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TSummonWolf.Destroy;
const
  FailName : string = 'TSummonWolf.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Resource.Free;
    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TSummonWolf.Drain( Source : TCharacter ) : Single;
const
  FailName : string = 'TSummonWolf.Drain';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    result := 15 + round( Source.Restriction / 10 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TSummonWolf.GetIconXY( Source : TCharacter ) : TPoint;
const
  FailName : string = 'TSummonWolf.GetIconXY';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    result.X := 5 * 32;
    result.Y := 32;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TSummonWolf.GetInfo( Source : TCharacter ) : string;
begin
  result := InfoText;
  Replace( result, 'a', inttostr( 10 + round( Source.Mysticism / 5 ) ) );
  Replace( result, 'b', '2-20' );
  Replace( result, 'c', '0-16' );
  result := 'Summons a ' + inttostr( 10 + round( Source.Mysticism / 5 ) ) + ' hit point wolf';
  result := result + #13 + '2-20 Piercing, ' + '0-16 Cutting';
end;

function TSummonWolf.GetLoaded : Boolean;
const
  FailName : string = 'TSummonWolf.GetLoaded';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if assigned( Resource ) then
      result := Resource.Loaded;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

class function TSummonWolf.GetName : string;
begin
  result := 'Summon Wolf';
end;

function TSummonWolf.Range( Source : TCharacter ) : Integer;
const
  FailName : string = 'TSummonWolf.Range';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    result := 200;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TSummonWolf.Recovery( Source : TCharacter ) : Integer;
const
  FailName : string = 'TSummonWolf.Recovery';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    result := 20;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TAura }

function TAura.Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean;
var
  Effect : TEffect;
  NewTarget : TCharacter;
const
  FailName : string = 'TAura.Cast';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := False;
    if assigned( Target ) then
    begin
      if not ( Target is TCharacter ) then
        exit;
      if TCharacter( Target ).Dead then
        exit;
      NewTarget := TCharacter( Target );
    end
    else
      NewTarget := Source;

    result := inherited Cast( Source, NewTarget );
    if not result then
      Exit;

    Effect := TAuraEffect.Create;
    Effect.Resource := Resource;
    Effect.ColorR := ColorR;
    Effect.ColorG := ColorG;
    Effect.ColorB := ColorB;
    Effect.ApplyColor := true;
    Effect.Resistance := Resistance;
    Effect.AnimationDuration := 8 * Resource.FrameMultiplier;
    Effect.Duration := Source.Mysticism * 50;
    Effect.Power := Source.Mysticism;
    TAuraEffect( Effect ).HitResource := HitResource;
    Effect.tag := 20;
    Effect.DoAction( 'Default', NewTarget.FacingString );

    with NewTarget do
    begin
      AddEffect( Effect );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

constructor TAura.Create;
const
  FailName : string = 'TAura.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    CastingType := ctProtection;
    TargetType := ttFriend;
    CastEffect := AuraEffect;

    SoundInCast := true;
    LoadCastSounds( 'switchplayers' );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TAura.Destroy;
const
  FailName : string = 'TAura.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    Resource.free;
    Resource := nil;
    HitResource.free;
    HitResource := nil;
    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TAura.Drain( Source : TCharacter ) : Single;
const
  FailName : string = 'TAura.Drain';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := FDrain + round( Source.Restriction / 10 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TAura.GetIconXY( Source : TCharacter ) : TPoint;
const
  FailName : string = 'TAura.GetIconXY';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := IconXY;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TAura.GetLoaded : Boolean;
const
  FailName : string = 'TAura.GetLoaded';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := Resource.Loaded and HitResource.Loaded;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TAura.Range( Source : TCharacter ) : Integer;
const
  FailName : string = 'TAura.Range';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := Round( Source.Mysticism * 10 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TAura.Recovery( Source : TCharacter ) : Integer;
const
  FailName : string = 'TAura.Recovery';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 40 - ( ( Source.Coordination + Source.Mysticism ) div 4 );
    if result < 0 then
      result := 0;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TShadow }

function TShadow.Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean;
var
  Effect : TEffect;
  NewTarget : TCharacter;
  i : integer;
const
  FailName : string = 'TShadow.Cast';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if assigned( Target ) then
    begin
      if not ( Target is TCharacter ) then
        exit;
      if TCharacter( Target ).Dead then
        exit;
      NewTarget := TCharacter( Target );
    end
    else
      NewTarget := Source;

    result := inherited Cast( Source, NewTarget );
    if not result then
      Exit;

    Effect := TEffect.Create;
    Effect.Resource := Resource;
    Effect.ColorR := -150;
    Effect.ColorG := -150;
    Effect.ColorB := -150;
    Effect.ApplyColor := true;
    Effect.UseCustom := true;
    Effect.EffectR := 50;
    Effect.EffectG := 50;
    Effect.EffectB := 50;
    Effect.Alpha := Effect.Resource.Alpha;
    Effect.SpecialEffect := seAdd;
    Effect.StatModifier.Stealth := 50;
    Effect.AnimationDuration := 8 * Resource.FrameMultiplier;
    Effect.Duration := Source.Mysticism * 50;
    Effect.Power := Source.Mysticism;
    Effect.tag := 30;
    Effect.DoAction( 'Default', NewTarget.FacingString );

    for i := 0 to FigureInstances.count - 1 do
    begin
      if ( FigureInstances.objects[ i ] is TCharacter ) and ( TCharacter( FigureInstances.objects[ i ] ).Track = Source ) then
        TCharacter( FigureInstances.objects[ i ] ).Track := nil;
    end;

    with NewTarget do
    begin
      AddEffect( Effect );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

constructor TShadow.Create;
const
  FailName : string = 'TShadow.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    CastingType := ctIllusion;
    TargetType := ttFriend;
    CastEffect := ProtectionEffect;
    if not assigned( ProtectionResource ) then
    begin
      ProtectionResource := LoadArtResource( 'engine\spells\protectionreceive.gif', true );
      ProtectionResource.DrawShadow := false;
      ResourceOwner := true;
    end;
    Resource := ProtectionResource;

    SoundInCast := true;
    LoadCastSounds( 'healspell2' );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TShadow.Destroy;
const
  FailName : string = 'TShadow.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if ResourceOwner then
    begin
      ProtectionResource.free;
      ProtectionResource := nil;
    end;
    Resource := nil;
    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TShadow.Drain( Source : TCharacter ) : Single;
const
  FailName : string = 'TShadow.Drain';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    Drain := 15 + round( Source.Restriction / 10 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TShadow.GetIconXY( Source : TCharacter ) : TPoint;
const
  FailName : string = 'TShadow.GetIconXY';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result.X := 6 * 32;
    result.Y := 32;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TShadow.GetInfo( Source : TCharacter ) : string;
begin
  result := InfoText;
  Replace( result, 'a', '50' );
end;

function TShadow.GetLoaded : Boolean;
const
  FailName : string = 'TShadow.GetLoaded';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if assigned( Resource ) then
      result := Resource.Loaded;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

class function TShadow.GetName : string;
begin
  result := 'Shadow';
end;

function TShadow.Range( Source : TCharacter ) : Integer;
const
  FailName : string = 'TShadow.Range';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := Round( Source.Mysticism * 10 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TShadow.Recovery( Source : TCharacter ) : Integer;
const
  FailName : string = 'TShadow.Recovery';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 40 - ( ( Source.Coordination + Source.Mysticism ) div 4 );
    if result < 0 then
      result := 0;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ THold }

function THold.Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean;
var
  Effect : TEffect;
  Duration : integer;
const
  FailName : string = 'THold.Cast';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := false;
    if not assigned( Target ) then
      exit;
    if not ( Target is TCharacter ) then
      exit;

    result := inherited Cast( Source, Target );
    if not result then
      Exit;

    Duration := round( Source.Mysticism * 4 - TCharacter( Target ).Strength - TCharacter( Target ).Resistance.Magic.Invulnerability );
    if Duration > 0 then
    begin
      Duration := round( Duration * ( 1 - TCharacter( Target ).Resistance.Magic.Resistance ) );
      if Duration > 0 then
      begin

        Effect := TEffect.Create;
        Effect.Resource := Resource;
        Effect.Power := Source.Mysticism;
        Effect.tag := 50;
        Effect.DoAction( 'Run', Target.FacingString );

        Effect.AnimationDuration := TCharacter( Target ).TakeDamage( Source, 0, Duration, true );
        TCharacter( Target ).AddEffect( Effect );
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure THold.Clear;
begin
  Resource.RLE.free; Resource.RLE := nil;
end;

constructor THold.Create;
const
  FailName : string = 'THold.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    CastingType := ctTranslocation;
    TargetType := ttEnemy;
    CastEffect := HoldEffect;
    Resource := LoadArtResource( 'engine\spells\holdreceive.gif', true );
    Resource.DrawShadow := false;

    SoundInCast := true;
    LoadCastSounds( 'Hold' );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor THold.Destroy;
const
  FailName : string = 'THold.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    Resource.Free;
    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function THold.Drain( Source : TCharacter ) : Single;
const
  FailName : string = 'THold.Drain';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 5 + round( Source.Restriction / 10 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function THold.GetIconXY( Source : TCharacter ) : TPoint;
const
  FailName : string = 'THold.GetIconXY';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result.X := 4 * 32;
    result.Y := 0;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function THold.GetInfo( Source : TCharacter ) : string;
var
  Duration : integer;
begin
  Duration := round( 3 * Source.Mysticism * 4 / 100 );
  result := InfoText;
  Replace( result, 'a', inttostr( Duration ) );
end;

function THold.GetLoaded : Boolean;
const
  FailName : string = 'THold.GetLoaded';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if assigned( Resource ) then
      result := Resource.Loaded;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

class function THold.GetName : string;
begin
  result := 'Hold';
end;

function THold.Range( Source : TCharacter ) : Integer;
const
  FailName : string = 'THold.Range';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := Source.Constitution * 10;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function THold.Recovery( Source : TCharacter ) : Integer;
const
  FailName : string = 'THold.Recovery';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 10;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TDeathSpell }

function TDeathSpell.Cast( Source : TCharacter;
  Target : TSpriteObject ) : Boolean;
var
  NewProjectile : TProjectile;
  Bonus, Angle : single;
  TargetX, TargetY : longint;
const
  FailName : string = 'TDeathSpell.Cast';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := inherited Cast( Source, Target );
    if not result then
      Exit;

    NewProjectile := TProjectile( Sprites.NewSprite( TProjectile, Resource, Source.X, Source.Y, 0, 1 ) );
    if assigned( NewProjectile ) then
    begin
      ZeroMemory( @NewProjectile.Damage, sizeof( NewProjectile.Damage ) );
      NewProjectile.DamageRadius := 0;
  //  NewProjectile.Radius:=-8;
  //  NewProjectile.SpecialEffect:=seAdd;;
      NewProjectile.Alpha := 100;
      NewProjectile.TrackingDegree := 45;
      if Source.TitleExists( 'Power Death' ) then
      begin
        NewProjectile.Magic := Source.Mysticism * 10000;
        NewProjectile.Damage.Special.Min := Source.Mysticism * 10;
        NewProjectile.Damage.Special.Max := Source.Mysticism * 20;
        NewProjectile.Duration := Source.Constitution * 90;
        NewProjectile.Speed := 8;
      end
      else
      begin
        NewProjectile.Magic := Source.Mysticism * 5000;
        NewProjectile.Damage.Special.Min := Source.Mysticism / 2;
        NewProjectile.Damage.Special.Max := Source.Mysticism;
        NewProjectile.Duration := Source.Constitution * 60;
        NewProjectile.Speed := 4;
      end;
      if Source.Coordination <= 10 then
        NewProjectile.HitIncidental := 1
      else
        NewProjectile.HitIncidental := 10 / Source.Coordination;
      NewProjectile.HitTarget := 1;
      NewProjectile.UseLineOfSight := True;

      TargetX := Source.TargetX;
      TargetY := Source.TargetY;
      Bonus := Source.Coordination + Source.Perception / 2 + Source.Combat / 4 - Source.Restriction / 5;
      if Bonus < 10 then
        Angle := 17.5
      else
        Angle := 175 / Bonus;
      ComputeTrajectory( Source, TargetX, TargetY, Angle );

      NewProjectile.Launch( Source, Target, TargetX, TargetY );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TDeathSpell.Clear;
begin
  Resource.RLE.free; Resource.RLE := nil;
end;

constructor TDeathSpell.Create;
const
  FailName : string = 'TDeathSpell.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    CastingType := ctHealing;
    TargetType := ttFriend;
    CastEffect := LichEffect;
    Resource := LoadArtResource( 'engine\WeaponProjectiles\vortex.gif', true );
    LoadCastSounds( 'deathvortex' );
    if assigned( Resource ) then
      Resource.DrawShadow := false;
    SoundInCast := false;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TDeathSpell.Destroy;
const
  FailName : string = 'TDeathSpell.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    Resource.Free;
    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TDeathSpell.Drain( Source : TCharacter ) : Single;
const
  FailName : string = 'TDeathSpell.Drain';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 5 + round( Source.Restriction / 10 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TDeathSpell.GetIconXY( Source : TCharacter ) : TPoint;
const
  FailName : string = 'TDeathSpell.GetIconXY';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result.X := 12 * 32;
    result.Y := 32;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TDeathSpell.GetInfo( Source : TCharacter ) : string;
begin
  result := InfoText;
end;

function TDeathSpell.GetLoaded : Boolean;
const
  FailName : string = 'TDeathSpell.GetLoaded';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if assigned( Resource ) then
      result := Resource.Loaded;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

class function TDeathSpell.GetName : string;
begin
  result := 'Death Spell';
end;

function TDeathSpell.Range( Source : TCharacter ) : Integer;
const
  FailName : string = 'TDeathSpell.Range';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 10 * ( Source.Constitution + Source.Mysticism );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TDeathSpell.Recovery( Source : TCharacter ) : Integer;
const
  FailName : string = 'TDeathSpell.Recovery';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 40 - ( Source.Constitution div 2 );
    if result < 0 then
      result := 0;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TShrapnel }

function TShrapnel.Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean;
var
  NewProjectile : TProjectile;
  Z1, Z2 : Integer;
  i, i1, j : integer;
  List : TStringList;
const
  FailName : string = 'TShrapnel.Cast';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := inherited Cast( Source, Target );
    if not result then
      Exit;
    Z1 := Source.Height div 2 - 16;
    if Assigned( Target ) then
      Z2 := 0
    else
      Z2 := Z1;
    if Source.TitleExists( 'Flying Blades' ) then
    begin
      List := GetNearbyEnemies( Source, Source.Coordination * 24 );
      try
        for i := 0 to Source.Mysticism div 2 do
        begin
          NewProjectile := TProjectile( Sprites.NewSprite( TShrapnelProjectile, Resource, Source.X + random( 32 ) - 16, Source.Y + random( 32 ) - 16, Z1 + random( 32 ) - 16, 1 ) );
          if assigned( NewProjectile ) then
          begin
            NewProjectile.Magic := 4;
            ZeroMemory( @NewProjectile.Damage, sizeof( NewProjectile.Damage ) );
            NewProjectile.Damage.Piercing.Min := Source.Mysticism / 18;
            NewProjectile.Damage.Piercing.Max := Source.Mysticism / 6;
            NewProjectile.DamageRadius := 0;
            NewProjectile.Duration := Source.Mysticism * 10;
            NewProjectile.TrackingDegree := 0;
            NewProjectile.Speed := 16 + Source.Mysticism / 30;
            if Source.Mysticism + Source.Coordination <= 10 then
              NewProjectile.HitIncidental := 1
            else
              NewProjectile.HitIncidental := 10 / ( Source.Mysticism + Source.Coordination );
            NewProjectile.HitTarget := 1;
            NewProjectile.UseLineOfSight := True;
            i1 := 6 - Source.Coordination div 20;
            if i1 < 1 then
              i1 := 1;
            TShrapnelProjectile( NewProjectile ).Count := i1;
            TShrapnelProjectile( NewProjectile ).NewTrackingDegree := 0.0125 * Source.Coordination;
            i1 := i mod 10;
            if i1 = 0 then
              NewProjectile.Launch( Source, Target, Source.TargetX, Source.TargetY + Z2 )
            else
            begin
              if assigned( List ) then
              begin
                if i1 < 8 then
                begin
                  j := random( List.count );
                  NewProjectile.Launch( Source, TCharacter( List.Objects[ j ] ), TCharacter( List.Objects[ j ] ).X + random( 64 ) - 32, TCharacter( List.Objects[ j ] ).Y + random( 64 ) - 32 );
                end
                else
                begin
                  j := random( List.count );
                  NewProjectile.Launch( Source, TCharacter( List.Objects[ j ] ), Source.X + random( 64 ) - 32, Source.Y + random( 64 ) - 32 );
                end;
              end
              else
              begin
                if i1 < 8 then
                  NewProjectile.Launch( Source, Target, Source.TargetX + random( 64 ) - 32, Source.TargetY + random( 64 ) - 32 )
                else
                  NewProjectile.Launch( Source, Target, Source.X + random( 64 ) - 32, Source.Y + random( 64 ) - 32 )
              end;
            end;
          end;
        end;
      finally
        List.free;
      end;
    end
    else if Source.TitleExists( 'Shards' ) then
    begin
      List := GetNearbyEnemies( Source, Source.Coordination * 20 );
      try
        for i := 0 to Source.Mysticism div 3 do
        begin
          NewProjectile := TProjectile( Sprites.NewSprite( TShrapnelProjectile, Resource, Source.X + random( 32 ) - 16, Source.Y + random( 32 ) - 16, Z1 + random( 32 ) - 16, 1 ) );
          if assigned( NewProjectile ) then
          begin
            NewProjectile.Magic := 2;
            ZeroMemory( @NewProjectile.Damage, sizeof( NewProjectile.Damage ) );
            NewProjectile.Damage.Piercing.Min := Source.Mysticism / 24;
            NewProjectile.Damage.Piercing.Max := Source.Mysticism / 8;
            NewProjectile.DamageRadius := 0;
            NewProjectile.Duration := Source.Mysticism * 10;
            NewProjectile.TrackingDegree := 0;
            NewProjectile.Speed := 16 + Source.Mysticism / 30;
            if Source.Mysticism + Source.Coordination <= 10 then
              NewProjectile.HitIncidental := 1
            else
              NewProjectile.HitIncidental := 10 / ( Source.Mysticism + Source.Coordination );
            NewProjectile.HitTarget := 1;
            NewProjectile.UseLineOfSight := True;
            i1 := 6 - Source.Coordination div 20;
            if i1 < 1 then
              i1 := 1;
            TShrapnelProjectile( NewProjectile ).Count := i1;
            TShrapnelProjectile( NewProjectile ).NewTrackingDegree := 0.0125 * Source.Coordination;
            i1 := i mod 10;
            if i1 = 0 then
              NewProjectile.Launch( Source, Target, Source.TargetX, Source.TargetY + Z2 )
            else
            begin
              if assigned( List ) then
              begin
                if i1 < 6 then
                begin
                  j := random( List.count );
                  NewProjectile.Launch( Source, TCharacter( List.Objects[ j ] ), TCharacter( List.Objects[ j ] ).X + random( 64 ) - 32, TCharacter( List.Objects[ j ] ).Y + random( 64 ) - 32 );
                end
                else
                begin
                  j := random( List.count );
                  NewProjectile.Launch( Source, TCharacter( List.Objects[ j ] ), Source.X + random( 64 ) - 32, Source.Y + random( 64 ) - 32 );
                end;
              end
              else
              begin
                if i1 < 6 then
                  NewProjectile.Launch( Source, Target, Source.TargetX + random( 64 ) - 32, Source.TargetY + random( 64 ) - 32 )
                else
                  NewProjectile.Launch( Source, Target, Source.X + random( 64 ) - 32, Source.Y + random( 64 ) - 32 )
              end;
            end;
          end;
        end;
      finally
        List.free;
      end;
    end
    else
    begin
      List := GetNearbyEnemies( Source, Source.Coordination * 16 );
      try
        for i := 0 to Source.Mysticism div 5 do
        begin
          NewProjectile := TProjectile( Sprites.NewSprite( TShrapnelProjectile, Resource, Source.X + random( 32 ) - 16, Source.Y + random( 32 ) - 16, Z1 + random( 32 ) - 16, 1 ) );
          if assigned( NewProjectile ) then
          begin
            NewProjectile.Magic := 1;
            ZeroMemory( @NewProjectile.Damage, sizeof( NewProjectile.Damage ) );
            NewProjectile.Damage.Piercing.Min := Source.Mysticism / 30;
            NewProjectile.Damage.Piercing.Max := Source.Mysticism / 10;
            NewProjectile.DamageRadius := 0;
            NewProjectile.Duration := Source.Constitution * 10;
            NewProjectile.TrackingDegree := 0;
            NewProjectile.Speed := 16 + Source.Coordination / 20;
            if Source.Coordination <= 10 then
              NewProjectile.HitIncidental := 1
            else
              NewProjectile.HitIncidental := 10 / ( Source.Coordination );
            NewProjectile.HitTarget := 1;
            NewProjectile.UseLineOfSight := True;
            i1 := 6 - Source.Coordination div 20;
            if i1 < 1 then
              i1 := 1;
            TShrapnelProjectile( NewProjectile ).Count := i1;
            TShrapnelProjectile( NewProjectile ).NewTrackingDegree := 0.0125 * Source.Coordination;
            i1 := i mod 10;
            if i1 = 0 then
              NewProjectile.Launch( Source, Target, Source.TargetX, Source.TargetY + Z2 )
            else
            begin
              if assigned( List ) then
              begin
                if i1 < 4 then
                begin
                  j := random( List.count );
                  NewProjectile.Launch( Source, TCharacter( List.Objects[ j ] ), TCharacter( List.Objects[ j ] ).X + random( 64 ) - 32, TCharacter( List.Objects[ j ] ).Y + random( 64 ) - 32 );
                end
                else
                begin
                  j := random( List.count );
                  NewProjectile.Launch( Source, TCharacter( List.Objects[ j ] ), Source.X + random( 64 ) - 32, Source.Y + random( 64 ) - 32 );
                end;
              end
              else
              begin
                if i1 < 4 then
                  NewProjectile.Launch( Source, Target, Source.TargetX + random( 64 ) - 32, Source.TargetY + random( 64 ) - 32 )
                else
                  NewProjectile.Launch( Source, Target, Source.X + random( 64 ) - 32, Source.Y + random( 64 ) - 32 )
              end;
            end;
          end;
        end;
      finally
        List.free;
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TShrapnel.Clear;
begin
  Resource.RLE.free; Resource.RLE := nil;
end;

constructor TShrapnel.Create;
const
  FailName : string = 'TShrapnel.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    CastingType := ctCombat;
    TargetType := ttEnemy;
    CastEffect := ShrapnelEffect;
    Resource := LoadArtResource( 'engine\weaponprojectiles\Shrapnelball.gif', true );
    LoadCastSounds( 'ShrapCast' );
    Resource.SpecialEffect := seAdd;
    Resource.DrawShadow := false;
    SoundInCast := false;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TShrapnel.Destroy;
const
  FailName : string = 'TShrapnel.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    Resource.Free;
    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TShrapnel.Drain( Source : TCharacter ) : Single;
const
  FailName : string = 'TShrapnel.Drain';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 5 + round( Source.Restriction / 10 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TShrapnel.GetIconXY( Source : TCharacter ) : TPoint;
const
  FailName : string = 'TShrapnel.GetIconXY';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if Source.TitleExists( 'Flying Blades' ) then
    begin
      result.X := 13 * 32;
      result.Y := 32;
    end
    else if Source.TitleExists( 'Shards' ) then
    begin
      result.X := 13 * 32;
      result.Y := 32;
    end
    else
    begin
      result.X := 13 * 32;
      result.Y := 0;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TShrapnel.GetInfo( Source : TCharacter ) : string;
begin
  result := InfoText;
  if Source.TitleExists( 'Flying Blades' ) then
  begin
    Replace( result, 'a', inttostr( 1 + Source.Mysticism div 2 ) );
    Replace( result, 'b', inttostr( round( Source.Mysticism / 18 ) ) + '-' + inttostr( round( Source.Mysticism / 6 ) ) );
  end
  else if Source.TitleExists( 'Shards' ) then
  begin
    Replace( result, 'a', inttostr( 1 + Source.Mysticism div 3 ) );
    Replace( result, 'b', inttostr( round( Source.Mysticism / 24 ) ) + '-' + inttostr( round( Source.Mysticism / 8 ) ) );
  end
  else
  begin
    Replace( result, 'a', inttostr( 1 + Source.Mysticism div 5 ) );
    Replace( result, 'b', inttostr( round( Source.Mysticism / 30 ) ) + '-' + inttostr( round( Source.Mysticism / 10 ) ) );
  end;
end;

function TShrapnel.GetLoaded : Boolean;
const
  FailName : string = 'TShrapnel.GetLoaded';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if assigned( Resource ) then
      result := Resource.Loaded;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

class function TShrapnel.GetName : string;
begin
  result := 'Shrapnel';
end;

function TShrapnel.Range( Source : TCharacter ) : Integer;
const
  FailName : string = 'TShrapnel.Range';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := Round( Source.Constitution * 10 * ( 16 + Source.Coordination / 20 ) );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TShrapnel.Recovery( Source : TCharacter ) : Integer;
const
  FailName : string = 'TShrapnel.Recovery';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 40 - ( ( Source.Constitution ) div 2 );
    if result < 0 then
      result := 0;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TForget }

function TForget.Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean;
var
  Effect : TEffect;
  Duration : integer;
const
  FailName : string = 'TForget.Cast';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := false;
    if not assigned( Target ) then
      exit;
    if not ( Target is TCharacter ) then
      exit;
    if TCharacter( Target ).unmoveable = true then
      exit;
    if TCharacter( Target ).highlightable = false then
      exit;
    if not ( Source.isenemy( TCharacter( Target ) ) ) then
      exit;
    result := inherited Cast( Source, Target );
    if not result then
      Exit;


    Duration := round( Source.Mysticism * 4 - TCharacter( Target ).Resistance.Magic.Invulnerability );
    if Duration > 0 then
    begin
      Duration := round( Duration * ( 1 - TCharacter( Target ).Resistance.Magic.Resistance ) );
      if Duration > 0 then
      begin

        Effect := TForgetEffect.Create;
        Effect.Power := Source.Mysticism;
        Effect.tag := -155;
        Effect.Duration := Duration;
        TCharacter( Target ).AddEffect( Effect );
        TCharacter( Target ).Track := nil;
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

constructor TForget.Create;
const
  FailName : string = 'TForget.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    CastingType := ctIllusion;
    TargetType := ttEnemy;
    CastEffect := HoldEffect;

    SoundInCast := true;
    LoadCastSounds( 'Whispers1' );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TForget.Destroy;
const
  FailName : string = 'TForget.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

//  Resource.Free;
    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TForget.Drain( Source : TCharacter ) : Single;
const
  FailName : string = 'TForget.Drain';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 3 + round( Source.Restriction / 10 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TForget.GetIconXY( Source : TCharacter ) : TPoint;
const
  FailName : string = 'TForget.GetIconXY';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result.X := 20 * 32;
    result.Y := 0;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TForget.GetInfo( Source : TCharacter ) : string;
var
  Duration : integer;
begin
  result := InfoText;
  Duration := round( 3 * Source.Mysticism * 4 / 100 );
  Replace( result, 'a', inttostr( Duration ) );
end;

function TForget.GetLoaded : Boolean;
begin
  result := True;
end;

class function TForget.GetName : string;
begin
  result := 'Forget';
end;

function TForget.Range( Source : TCharacter ) : Integer;
const
  FailName : string = 'TForget.Range';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := Source.Constitution * 5;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TForget.Recovery( Source : TCharacter ) : Integer;
const
  FailName : string = 'TForget.Recovery';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 15;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TForgetEffect }

procedure TForgetEffect.Adjust( Character : TCharacter );
begin
  FCharacter := Character;

  if not Applied then
  begin
    if assigned( FCharacter.AI ) then
    begin
      OldAI := FCharacter.FAI;
      OldAIMode := FCharacter.FAIMode;
      FCharacter.FAI := TMeander.create;
      FCharacter.FAI.Character := FCharacter;
      FCharacter.FAI.Init;
    end;
    Applied := true;
  end;

  inherited;
end;

constructor TForgetEffect.Create;
var
  i : integer;
  p : ^TSwirlBead;
begin
  inherited;
  AnimationDuration := 80;
  Resource := TLightning( Lightning ).MediumResource;
  Points := 20;
  GetMem( PointList, Points * sizeof( TSwirlBead ) );
  p := PointList;
  for i := 1 to Points do
  begin
    p^.X := 0;
    p^.Y := 0;
    p^.Z := random * 10;
    p^.Angle := random * PI2;
    p^.Rise := 2.5 + random / 2;
    p^.Offset := random * 30 - 15;
    inc( p );
  end;
end;

destructor TForgetEffect.Destroy;
begin
  if assigned( OldAI ) then
    OldAI.free;
  FreeMem( PointList );
  inherited;
end;

function TForgetEffect.DoFrame : boolean;
var
  i : integer;
  p : ^TSwirlBead;
begin
  result := inherited DoFrame;
  if not result and ( AnimationDuration > 0 ) then
  begin
    p := PointList;
    for i := 1 to Points do
    begin
      p^.Angle := p^.Angle - 30 / 180;
      p^.Z := p^.Z + p^.Rise;
      p^.X := ( 80 + p^.Offset ) * cos( p^.Angle ) / 4;
      p^.Y := ( 80 + p^.Offset ) * sin( p^.Angle ) / 4;
      inc( p );
    end;
  end;
  if Applied and ( Duration = 0 ) then
  begin
    Applied := false;
    FCharacter.FAI.free;
    FCharacter.FAI := OldAI;
    OldAI := nil;
    FCharacter.FAIMode := OldAIMode;
  end;
end;

procedure TForgetEffect.Refresh( NewEffect : TEffect );
var
  i : integer;
  p : ^TSwirlBead;
begin
  Duration := NewEffect.Duration;
  AnimationDuration := 80;
  p := PointList;
  for i := 1 to Points do
  begin
    p^.X := 0;
    p^.Y := 0;
    p^.Z := random * 10;
    p^.Angle := random * PI2;
    p^.Rise := 2.5 + random / 2;
    p^.Offset := random * 30 - 15;
    inc( p );
  end;
end;

procedure TForgetEffect.RenderLocked( Figure : TAniFigure; Bits : PBITPLANE );
var
  i : integer;
  p : ^TSwirlBead;
  Blend : integer;
  Color : integer;
begin
  p := PointList;
  for i := 1 to Points do
  begin
    Blend := 100 - round( p^.Z / 2 );
    Color := round( p^.Z * 2 );
    if Color > 200 then
      Color := 200;
    if Blend > 0 then
      Resource.RLE.DrawColorize( 5, Figure.CenterX + round( p^.X ), Figure.CenterY + round( ( p^.Y - p^.Z ) / 2 ),
        Bits, 200 - Color, Color, 50, Blend, 100 );
    inc( p );
  end;
end;

{ TAuraEffect }

procedure TAuraEffect.Adjust( Character : TCharacter );
begin
  inherited;
  FCharacter := Character;
end;

function TAuraEffect.Hit( Source : TAniFigure; Damage : PDamageProfile ) : boolean;
var
  Effect : TEffect;
  Direction : TFacing;
  S : string;
begin
  result := false;
  Direction := Character.GetFacing( FCharacter.X, FCharacter.Y, Source.X, Source.Y );
  case Direction of
    fSS : S := 'SS';
    fSE : S := 'SE';
    fEE : S := 'EE';
    fNE : S := 'NE';
    fNN : S := 'NN';
    fNW : S := 'NW';
    fWW : S := 'WW';
    fSW : S := 'SW';
  end;

  Effect := TEffect.Create;
  Effect.Resource := HitResource;
  Effect.AnimationDuration := 8 * Resource.FrameMultiplier;
  Effect.ColorR := ColorR;
  Effect.ColorG := ColorG;
  Effect.ColorB := ColorB;
  Effect.ApplyColor := true;
  Effect.DoAction( 'Default', S );

  FCharacter.AddEffect( Effect );
end;

{ TLightningProjectile }

procedure TLightningProjectile.DoFrame;
const
  FailName : string = 'TLightningProjectile.DoFrame';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if Alpha = 20 then
      Alpha := 100
    else if Alpha = 100 then
      Alpha := 70
    else if Alpha = 70 then
      Alpha := 30;
    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TLightningProjectile.Launch( Source : TCharacter;
  Target : TSpriteObject; X, Y : Integer );
begin
  OnCollideFigure := CollideFigure;
  OnCollideItem := CollideItem;
  OnCollideBoundary := CollideBoundary;
  OnMove := MoveEvent;
  ZFix := Z;
  FSource := Source;
  FTarget := Target;
  OnScriptEnd := nil;
  Exploding := False;
  Move( X, Y, Z );

  if random > 0.65 then
    Frame := random( 6 ) + 1
  else
    Frame := 6;
end;

{ TShrapnelProjectile }

procedure TShrapnelProjectile.DoFrame;
const
  FailName : string = 'TShrapnelProjectile.DoFrame';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    dec( Count );
    if Count <= 0 then
    begin
      TrackingDegree := TrackingDegree + NewTrackingDegree;
    end;
    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TShrapnelProjectile.Launch( Source : TCharacter;
  Target : TSpriteObject; X, Y : Integer );
const
  FailName : string = 'TShrapnelProjectile.Launch';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    OnCollideFigure := CollideFigure;
    OnCollideItem := CollideItem;
    OnCollideBoundary := CollideBoundary;
    OnMove := MoveEvent;
    ZFix := Z;
    FSource := Source;
    FTarget := Target;
    OnScriptEnd := nil;
    Exploding := False;
    TrackingDegree := 0;
    Move( X, Y, Z );

    FFAcing := Character.GetFacing( Self.X, Self.Y, X, Y );
    if ActionExists( 'Cast' ) then
      DoAction( 'Cast' )
    else
      DoAction( 'Default' );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TChargeProjectile }

procedure TChargeProjectile.DoFrame;
const
  FailName : string = 'TChargeProjectile.DoFrame';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    Frame := random( 6 ) + 1;
    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TManaThief }

function TManaThief.Cast( Source : TCharacter;
  Target : TSpriteObject ) : Boolean;
var
  Effect : TEffect;
  NewTarget : TCharacter;
const
  FailName : string = 'TManaThief.Cast';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if assigned( Target ) then
    begin
      if not ( Target is TCharacter ) then
        exit;
      if TCharacter( Target ).Dead then
        exit;
      NewTarget := TCharacter( Target );
    end
    else
      NewTarget := Source;

    result := inherited Cast( Source, NewTarget );
    if not result then
      Exit;

    Effect := TThiefEffect.Create;
    Effect.Resource := Resource;
    Effect.UseCustom := true;
    Effect.EffectR := 100;
    Effect.EffectG := 100;
    Effect.EffectB := 100;
    Effect.Alpha := 85;
    Effect.SpecialEffect := seAdd;
    Effect.Duration := Source.Mysticism * 20;
    Effect.AnimationDuration := Effect.Duration;
    Effect.Power := Source.Mysticism;
    Effect.tag := 172;
    Effect.DoAction( 'Default', NewTarget.FacingString );

    with NewTarget do
    begin
      AddEffect( Effect );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TManaThief.Clear;
begin
  Resource.RLE.free; Resource.RLE := nil;
end;

constructor TManaThief.Create;
const
  FailName : string = 'TManaThief.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    CastingType := ctDivination;
    TargetType := ttFriend;
    CastEffect := ManaThiefEffect;
    Resource := LoadArtResource( 'engine\spells\ManaThiefReceive.gif', true );
    if assigned( Resource ) then
    begin
      Resource.DrawShadow := false;
    end;

    SoundInCast := false;
    LoadCastSounds( 'ManaThief' );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TManaThief.Destroy;
const
  FailName : string = 'TManaThief.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    Resource.free;
    Resource := nil;
    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TManaThief.Drain( Source : TCharacter ) : Single;
const
  FailName : string = 'TManaThief.Drain';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    Drain := 20 + round( Source.Restriction / 10 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TManaThief.GetIconXY( Source : TCharacter ) : TPoint;
const
  FailName : string = 'TManaThief.GetIconXY';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result.X := 21 * 32;
    result.Y := 32;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TManaThief.GetInfo( Source : TCharacter ) : string;
begin
  result := InfoText;
end;

function TManaThief.GetLoaded : Boolean;
const
  FailName : string = 'TManaThief.GetLoaded';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if assigned( Resource ) then
      result := Resource.Loaded;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

class function TManaThief.GetName : string;
begin
  result := 'Mana Thief';
end;

function TManaThief.Range( Source : TCharacter ) : Integer;
const
  FailName : string = 'TManaThief.Range';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := Round( Source.Mysticism * 10 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TManaThief.Recovery( Source : TCharacter ) : Integer;
const
  FailName : string = 'TManaThief.Recovery';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 40 - ( ( Source.Coordination + Source.Mysticism ) div 4 );
    if result < 0 then
      result := 0;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TThiefEffect }

procedure TThiefEffect.Adjust( Character : TCharacter );
begin
  inherited;
  FCharacter := Character;
end;

constructor TThiefEffect.Create;
begin
  inherited;
end;

destructor TThiefEffect.Destroy;
begin
  List.free;
  inherited;
end;

function TThiefEffect.DoFrame : boolean;
var
  Drain, M : single;
  i : integer;
begin
  result := inherited DoFrame;
  if not result and ( AnimationDuration > 0 ) then
  begin
    inc( FrameCount );
    if FrameCount >= 10 then
    begin
      List.free;
      List := GetNearbyCharacter( FCharacter, Power * 4 );
      FrameCount := 0;
    end;
    if assigned( List ) then
    begin
      Drain := Power / 2000;
      for i := 0 to List.count - 1 do
      begin
        M := TCharacter( List.objects[ i ] ).Mana - TCharacter( List.objects[ i ] ).Drain;
        if M > Drain then
          M := Drain;
        TCharacter( List.objects[ i ] ).Drain := TCharacter( List.objects[ i ] ).Drain + M;
        if M > FCharacter.Drain then
          M := FCharacter.Drain;
        FCharacter.Drain := FCharacter.Drain - M;
      end;
    end;
  end;
end;

{ TGreatHand }

function TGreatHand.Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean;
var
  Effect : TDamagingEffect;
const
  FailName : string = 'TGreatHand.Cast';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := False;
    if not assigned( Target ) then
      exit;
    if not ( Target is TCharacter ) then
      exit;
    if TCharacter( Target ).Dead then
      exit;
    if not Game.LineOfSight( Source.X, Source.Y, Target.X, Target.Y ) then
      exit;

    result := inherited Cast( Source, Target );
    if not result then
      Exit;

    Effect := TDamagingEffect.Create;
    Effect.Resource := Resource;
    Effect.AnimationDuration := 8 * Resource.FrameMultiplier;
    Effect.Power := Source.Mysticism;
    Effect.DoAction( 'Default', Target.FacingString );

    Effect.Damage.Crushing.Min := Source.Mysticism;
    Effect.Damage.Crushing.Max := 1.5 * Source.Mysticism;
    Effect.Source := Source;
    Effect.TriggerFrame := 12;
    Effect.UseStealth := true;
    TCharacter( Target ).AddEffect( Effect );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TGreatHand.Clear;
begin
  Resource.RLE.free; Resource.RLE := nil;
end;

constructor TGreatHand.Create;
const
  FailName : string = 'TGreatHand.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    CastingType := ctCombat;
    TargetType := ttEnemy;
    CastEffect := GreatHandEffect;
    Resource := LoadArtResource( 'engine\spells\GreatHandReceive.gif', true );
    if assigned( Resource ) then
    begin
      Resource.DrawShadow := false;
    end;

    LoadCastSounds( 'GreatHand' );
    SoundInCast := false;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TGreatHand.Destroy;
const
  FailName : string = 'TGreatHand.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    Resource.Free;
    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TGreatHand.Drain( Source : TCharacter ) : Single;
const
  FailName : string = 'TGreatHand.Drain';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 25 + round( Source.Restriction / 10 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TGreatHand.GetIconXY( Source : TCharacter ) : TPoint;
const
  FailName : string = 'TGreatHand.GetIconXY';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result.X := 19 * 32;
    result.Y := 32;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TGreatHand.GetInfo( Source : TCharacter ) : string;
begin
  result := InfoText;
  Replace( result, 'a', inttostr( Source.Mysticism ) + '-' + inttostr( round( 1.5 * Source.Mysticism ) ) );
end;

function TGreatHand.GetLoaded : Boolean;
const
  FailName : string = 'TGreatHand.GetLoaded';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if assigned( Resource ) then
      result := Resource.Loaded;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

class function TGreatHand.GetName : string;
begin
  result := 'Great Hand';
end;

function TGreatHand.Range( Source : TCharacter ) : Integer;
const
  FailName : string = 'TGreatHand.Range';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := Round( Source.Mysticism * 50 * ( 24 + Source.Mysticism / 10 ) );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TGreatHand.Recovery( Source : TCharacter ) : Integer;
const
  FailName : string = 'TGreatHand.Recovery';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 40 - ( ( Source.Coordination + Source.Mysticism ) div 4 );
    if result < 0 then
      result := 0;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TAuraOfIron }

procedure TAuraOfIron.Clear;
begin
  Resource.RLE.free; Resource.RLE := nil;
  HitResource.RLE.free; HitResource.RLE := nil;
end;

constructor TAuraOfIron.Create;
const
  FailName : string = 'TAuraOfIron.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;

    Resource := LoadArtResource( 'engine\spells\AuraIronReceive(LVL2).gif', true );
    Resource.DrawShadow := false;
    Resource.SpecialEffect := seAdd;

    HitResource := LoadArtResource( 'engine\spells\AuraIronHit(LVL2).gif', true );
    HitResource.DrawShadow := false;
    HitResource.SpecialEffect := seAdd;

    Resistance.Piercing.Resistance := 0.5;
    Resistance.Cutting.Resistance := 0.5;
    Resistance.Crushing.Resistance := 0.5;
    ColorR := 150;
    ColorG := 150;
    ColorB := 150;
    FDrain := 20;
    IconXY.X := 18 * 32;
    IconXY.Y := 0;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TAuraOfIron.GetInfo( Source : TCharacter ) : string;
begin
  result := InfoText;
  Replace( result, 'a', '50' );
end;

class function TAuraOfIron.GetName : string;
begin
  result := 'Aura of Iron';
end;

{ TAuraOfSteel }

procedure TAuraOfSteel.Clear;
begin
  Resource.RLE.free; Resource.RLE := nil;
  HitResource.RLE.free; HitResource.RLE := nil;
end;

constructor TAuraOfSteel.Create;
const
  FailName : string = 'TAuraOfSteel.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;

    Resource := LoadArtResource( 'engine\spells\AuraSteelReceive(LVL2).gif', true );
    Resource.DrawShadow := false;
    Resource.SpecialEffect := seAdd;

    HitResource := LoadArtResource( 'engine\spells\AuraSteelHit(LVL2).gif', true );
    HitResource.DrawShadow := false;
    HitResource.SpecialEffect := seAdd;

    Resistance.Piercing.Resistance := 0.75;
    Resistance.Cutting.Resistance := 0.75;
    Resistance.Crushing.Resistance := 0.75;
    ColorR := 180;
    ColorG := 180;
    ColorB := 255;
    FDrain := 30;
    IconXY.X := 18 * 32;
    IconXY.Y := 32;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TAuraOfSteel.GetInfo( Source : TCharacter ) : string;
begin
  result := InfoText;
  Replace( result, 'a', '75' );
end;

class function TAuraOfSteel.GetName : string;
begin
  result := 'Aura of Steel';
end;

{ TProtectionFire }

constructor TProtectionFire.Create;
const
  FailName : string = 'TProtection.CreateFire';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    tag := 4;
    MinorResistance.Heat.Resistance := 0.5;
    MajorResistance.Heat.Resistance := 0.75;
    IconXY.X := 0;
    IconXY.Y := 0;
    R := 100;
    G := 10;
    B := 0;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

class function TProtectionFire.GetName : string;
begin
  result := 'Protection from Fire';
end;

{ TProtectionCold }

constructor TProtectionCold.Create;
const
  FailName : string = 'TProtection.CreateCold';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    tag := 2;
    MinorResistance.Cold.Resistance := 0.5;
    MajorResistance.Cold.Resistance := 0.75;
    IconXY.X := 0;
    IconXY.Y := 32;
    R := 0;
    G := 35;
    B := 60;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

class function TProtectionCold.GetName : string;
begin
  result := 'Protection from Cold';
end;

{ TProtectionElectricity }

constructor TProtectionElectricity.Create;
const
  FailName : string = 'TProtection.CreateElectricity';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    tag := 3;
    MinorResistance.Electric.Resistance := 0.5;
    MajorResistance.Electric.Resistance := 0.75;
    IconXY.X := 32;
    IconXY.Y := 0;
    R := 110;
    G := 100;
    B := 150;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

class function TProtectionElectricity.GetName : string;
begin
  result := 'Protection from Lightning';
end;

{ TProtectionPoison }

constructor TProtectionPoison.Create;
const
  FailName : string = 'TProtection.CreatePoison';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    tag := 6;
    MinorResistance.Poison.Resistance := 0.5;
    MajorResistance.Poison.Resistance := 0.75;
    IconXY.X := 2 * 32;
    IconXY.Y := 0;
    R := 65;
    G := 75;
    B := 25;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

class function TProtectionPoison.GetName : string;
begin
  result := 'Protection from Poison';
end;

{ TProtectionMagic }

constructor TProtectionMagic.Create;
const
  FailName : string = 'TProtection.CreateMagic';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    tag := 5;
    MinorResistance.Magic.Resistance := 0.5;
    MajorResistance.Magic.Resistance := 0.75;
    IconXY.X := 32;
    IconXY.Y := 32;
    R := 100;
    G := 0;
    B := 100;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

class function TProtectionMagic.GetName : string;
begin
  result := 'Protection from Magic';
end;

{ TProtectionAll }

constructor TProtectionAll.Create;
const
  FailName : string = 'TProtection.CreateAll';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    tag := 1;
    MinorResistance.Heat.Resistance := 0.25;
    MinorResistance.Cold.Resistance := 0.25;
    MinorResistance.Electric.Resistance := 0.25;
    MinorResistance.Poison.Resistance := 0.25;
    MinorResistance.Magic.Resistance := 0.25;
    MajorResistance.Heat.Resistance := 0.5;
    MajorResistance.Cold.Resistance := 0.5;
    MajorResistance.Electric.Resistance := 0.5;
    MajorResistance.Poison.Resistance := 0.5;
    MajorResistance.Magic.Resistance := 0.5;
    IconXY.X := 2 * 32;
    IconXY.Y := 32;
    R := 100;
    G := 100;
    B := 100;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TProtectionAll.GetInfo( Source : TCharacter ) : string;
var
  S : string;
begin
  result := InfoText;
  S := 'Major ' + GetName;
  if Source.TitleExists( S ) then
  begin
    Replace( result, 'a', '50' );
  end
  else
  begin
    Replace( result, 'a', '25' );
  end;
end;

class function TProtectionAll.GetName : string;
begin
  result := 'Protection from All';
end;

{ TGreatWolf }

function TGreatWolf.Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean;
var
  Effect : TDamagingEffect;
const
  FailName : string = 'TGreatWolf.Cast';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := False;
    if not assigned( Target ) then
      exit;
    if not ( Target is TCharacter ) then
      exit;
    if TCharacter( Target ).Dead then
      exit;
    if not Game.LineOfSight( Source.X, Source.Y, Target.X, Target.Y ) then
      exit;

    result := inherited Cast( Source, Target );
    if not result then
      Exit;

    Effect := TDamagingEffect.Create;
    Effect.Resource := Resource;
    Effect.AnimationDuration := 8 * Resource.FrameMultiplier;
    Effect.Power := Source.Mysticism;
    Effect.DoAction( 'Default', Target.FacingString );

    Effect.Damage.Crushing.Min := Source.Mysticism / 3;
    Effect.Damage.Crushing.Max := 2 * Source.Mysticism / 3;
    Effect.Damage.Cutting.Min := Source.Mysticism / 3;
    Effect.Damage.Cutting.Max := 2 * Source.Mysticism / 3;
    Effect.Damage.Piercing.Min := Source.Mysticism / 3;
    Effect.Damage.Piercing.Max := 2 * Source.Mysticism / 3;
    Effect.Source := Source;
    Effect.TriggerFrame := 36;
    Effect.UseStealth := true;
    TCharacter( Target ).AddEffect( Effect );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TGreatWolf.Clear;
begin
  Resource.RLE.free; Resource.RLE := nil;
end;

constructor TGreatWolf.Create;
const
  FailName : string = 'TGreatWolf.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    CastingType := ctCombat;
    TargetType := ttEnemy;
    CastEffect := GreatWolfEffect;
    Resource := LoadArtResource( 'engine\spells\GreatWolfReceive.gif', true );
    if assigned( Resource ) then
    begin
      Resource.DrawShadow := false;
    end;

    LoadCastSounds( 'WolfC&R' );
    SoundInCast := false;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TGreatWolf.Destroy;
const
  FailName : string = 'TGreatWolf.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    Resource.Free;
    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TGreatWolf.Drain( Source : TCharacter ) : Single;
const
  FailName : string = 'TGreatWolf.Drain';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 30 + round( Source.Restriction / 10 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TGreatWolf.GetIconXY( Source : TCharacter ) : TPoint;
const
  FailName : string = 'TGreatWolf.GetIconXY';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result.X := 20 * 32;
    result.Y := 32;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TGreatWolf.GetInfo( Source : TCharacter ) : string;
begin
  result := InfoText;
  Replace( result, 'a', inttostr( round( Source.Mysticism / 3 ) ) + '-' + inttostr( round( 2 * Source.Mysticism / 3 ) ) );
  Replace( result, 'b', inttostr( round( Source.Mysticism / 3 ) ) + '-' + inttostr( round( 2 * Source.Mysticism / 3 ) ) );
  Replace( result, 'c', inttostr( round( Source.Mysticism / 3 ) ) + '-' + inttostr( round( 2 * Source.Mysticism / 3 ) ) );
end;

function TGreatWolf.GetLoaded : Boolean;
const
  FailName : string = 'TGreatWolf.GetLoaded';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if assigned( Resource ) then
      result := Resource.Loaded;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

class function TGreatWolf.GetName : string;
begin
  result := 'Great Wolf';
end;

function TGreatWolf.Range( Source : TCharacter ) : Integer;
const
  FailName : string = 'TGreatWolf.Range';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := Round( Source.Mysticism * 50 * ( 24 + Source.Mysticism / 10 ) );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TGreatWolf.Recovery( Source : TCharacter ) : Integer;
const
  FailName : string = 'TGreatWolf.Recovery';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 40 - ( ( Source.Coordination + Source.Mysticism ) div 4 );
    if result < 0 then
      result := 0;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TFlameStrike }

function TFlameStrike.Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean;
var
  NewProjectile : TProjectile;
  Damage : TDamageProfile;
  Z1 : Integer;
  Path : HGLOBAL;
  i, PathCount : integer;
  p : ^TPoint;
  UseLineOfSight : boolean;
  ValidTarget : boolean;
  Total, Stun : single;
  DoDamage : boolean;
const
  FailName : string = 'TFlameStrike.Cast';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := inherited Cast( Source, Target );
    if not result then
      Exit;

    ValidTarget := assigned( Target ) and ( Target is TCharacter ) and not TCharacter( Target ).Dead;
    UseLineOfSight := Source.UseLineOfSight;
    Source.UseLineOfSight := true;
    PathCount := Game.FindPath( Source, Source.TargetX, Source.TargetY, Source.Coordination div 4, Path );
    Source.UseLineOfSight := UseLineOfSight;

    if PathCount > 0 then
    begin

      result := inherited Cast( Source, Target );
      if not result then
      begin
        GlobalFree( Path );
        Exit;
      end;

      if PathCount > 20 then
      begin
        PathCount := 20;
        DoDamage := false;
      end
      else if PathCount > Source.Mysticism then
      begin
        PathCount := Source.Mysticism;
        DoDamage := false;
      end
      else
        DoDamage := true;

      Z1 := Source.Height div 2 - 16;
      Resource.DrawShadow := false;
      p := GlobalLock( Path );
      for i := 1 to PathCount do
      begin
//      if (i mod 2)=0 then begin
        NewProjectile := TProjectile( Sprites.NewSprite( TProjectile, Resource, p^.X + random( 4 ) - 2, p^.Y + random( 4 ) - 2, Z1 + random( 4 ) - 2, 1 ) );
        if assigned( NewProjectile ) then
        begin
          ZeroMemory( @NewProjectile.Damage, sizeof( NewProjectile.Damage ) );
          NewProjectile.Duration := 4 + i + random( 8 );
          NewProjectile.TrackingDegree := 0;
          NewProjectile.Speed := 1;
          NewProjectile.HitIncidental := 1;
          NewProjectile.HitTarget := 1;
          NewProjectile.UseLineOfSight := true;
          NewProjectile.Radius := 14;
          NewProjectile.Damage.Heat.Min := Source.Mysticism / 4;
          NewProjectile.Damage.Heat.Max := Source.Mysticism / 3;
          if ( ( i mod 4 ) = 0 ) then
          begin
            NewProjectile.GlowEffect := TGlow( Sprites.NewSprite( TGlow, nil, Source.X, Source.Y - 1, 0, 0 ) );
            NewProjectile.GlowEffect.RFactor := 100;
            NewProjectile.GlowEffect.GFactor := 50;
            NewProjectile.GlowEffect.BFactor := 25;
            NewProjectile.GlowEffect.Alpha := 100;
          end;
          NewProjectile.Launch( Source, nil, p^.X + Random( 20 ) - 10, p^.Y + Random( 20 ) - 10 ); //replace nil with target
        end;
//      end;

        inc( p );
      end;
      GlobalUnlock( Path );
      GlobalFree( Path );
      ZeroMemory( @Damage, sizeof( Damage ) );
      Damage.Heat.Min := Source.Mysticism * 0.6;
      Damage.Heat.Max := Source.Mysticism * 1.2;

      if ValidTarget and DoDamage then
      begin
        TCharacter( Target ).AffectDamage( Source, @Damage );
        Total := CalcTotalDamage( Damage, TCharacter( Target ).Resistance, 1, false );
        Stun := CalcDamage( Damage.Stun ) - TCharacter( Target ).Resistance.Stun.Invulnerability;
        if Stun > 0 then
          Stun := Stun * ( 1 - TCharacter( Target ).Resistance.Stun.Resistance );
        with Target as TCharacter do
        begin
          TakeDamage( Source, Total, Stun, false );
        end;
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TFlameStrike.Casting( Source : TCharacter );
var
  Effect : TEffect;
begin
  if TCharacterResource( Source.Resource ).UseCastAnimation then
  begin
    Effect := TEffect.Create;
    Effect.Resource := BigFire;
    Effect.AnimationDuration := 10 * Effect.Resource.FrameMultiplier;
    Effect.Power := Source.Mysticism;
    Effect.DoAction( 'Default', Source.FacingString );
    Source.AddEffect( Effect );
  end;
end;

constructor TFlameStrike.Create;
const
  FailName : string = 'TFlameStrike.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    CastingType := ctCombat;
    TargetType := ttEnemy;
    CastEffect := FireEffect;
    Resource := TFireball( Fireball ).Resource;
    SoundInCast := false;
    LoadCastSounds( 'FireCast' );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TFlameStrike.Destroy;
const
  FailName : string = 'TFlameStrike.Destroy';
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

function TFlameStrike.Drain( Source : TCharacter ) : Single;
const
  FailName : string = 'TFlameStrike.Drain';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 10 + round( Source.Restriction / 10 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TFlameStrike.GetIconXY( Source : TCharacter ) : TPoint;
const
  FailName : string = 'TFlameStrike.GetIconXY';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result.X := 8 * 32;
    result.Y := 32;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TFlameStrike.GetInfo( Source : TCharacter ) : string;
begin
  result := InfoText;
  Replace( result, 'a', inttostr( round( Source.Mysticism * 0.6 ) ) + '-' + inttostr( round( Source.Mysticism * 1.2 ) ) );
end;

function TFlameStrike.GetLoaded : Boolean;
const
  FailName : string = 'TFlameStrike.GetLoaded';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if assigned( Resource ) then
      result := Resource.Loaded;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

class function TFlameStrike.GetName : string;
begin
  result := 'Flame Strike';
end;

function TFlameStrike.Range( Source : TCharacter ) : Integer;
const
  FailName : string = 'TFlameStrike.Range';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := Round( Source.Constitution * 10 * ( 12 + Source.Coordination / 10 ) );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TFlameStrike.Recovery( Source : TCharacter ) : Integer;
const
  FailName : string = 'TFlameStrike.Recovery';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 40 - ( ( Source.Constitution + Source.Mysticism ) div 2 );
    if result < 0 then
      result := 0;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TBlizzard }

function TBlizzard.Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean;
var
  NewProjectile : TProjectile;
  Z1, Z2 : Integer;
  i : integer;
const
  FailName : string = 'TBlizzard.Cast';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := inherited Cast( Source, Target );
    if not result then
      Exit;
    Z1 := Source.Height div 2 - 16;
    if Assigned( Target ) then
      Z2 := 0
    else
      Z2 := Z1;

    for i := 0 to Source.Mysticism div 6 do
    begin
      NewProjectile := TProjectile( Sprites.NewSprite( TProjectile, Resource, Source.X + random( 32 ) - 16, Source.Y + random( 32 ) - 16, Z1 + random( 32 ) - 16, 1 ) );
      NewProjectile.Magic := 6;
      ZeroMemory( @NewProjectile.Damage, sizeof( NewProjectile.Damage ) );
      NewProjectile.Damage.Crushing.Min := 1;
      NewProjectile.Damage.Crushing.Max := 2;
      NewProjectile.Damage.Cold.Min := Source.Mysticism / 16;
      NewProjectile.Damage.Cold.Max := Source.Mysticism / 8;
      NewProjectile.DamageRadius := 0;
      NewProjectile.Duration := Source.Mysticism * 10;
      NewProjectile.TrackingDegree := Source.Mysticism / 30;
      NewProjectile.Speed := 12 + Source.Mysticism / 30;
      if Source.Mysticism + Source.Coordination <= 10 then
        NewProjectile.HitIncidental := 1
      else
        NewProjectile.HitIncidental := 10 / ( Source.Mysticism + Source.Coordination );
      NewProjectile.HitTarget := 1;
      NewProjectile.UseLineOfSight := True;
      NewProjectile.Alpha := 75;
      NewProjectile.Launch( Source, Target, Source.TargetX, Source.TargetY + Z2 );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TBlizzard.Casting( Source : TCharacter );
var
  Effect : TEffect;
begin
  if TCharacterResource( Source.Resource ).UseCastAnimation then
  begin
    Effect := TEffect.Create;
    Effect.Resource := BigIce;
    Effect.AnimationDuration := 10 * Effect.Resource.FrameMultiplier;
    Effect.Power := Source.Mysticism;
    Effect.DoAction( 'Default', Source.FacingString );
    Source.AddEffect( Effect );
  end;
end;

constructor TBlizzard.Create;
const
  FailName : string = 'TBlizzard.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    CastingType := ctCombat;
    TargetType := ttEnemy;
    CastEffect := FrostEffect;
    Resource := LoadArtResource( 'engine\weaponprojectiles\NewLightningball.gif', true );
    Resource.DrawShadow := false;
    SoundInCast := true;
    LoadCastSounds( 'freezespell1,freezespell2' );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TBlizzard.Destroy;
const
  FailName : string = 'TBlizzard.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    Resource.Free;
    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TBlizzard.Drain( Source : TCharacter ) : Single;
const
  FailName : string = 'TBlizzard.Drain';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 5 + round( Source.Restriction / 10 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TBlizzard.GetIconXY( Source : TCharacter ) : TPoint;
const
  FailName : string = 'TBlizzard.GetIconXY';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result.X := 7 * 32;
    result.Y := 0;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TBlizzard.GetInfo( Source : TCharacter ) : string;
begin
  result := InfoText;
  Replace( result, 'a', inttostr( 1 + Source.Mysticism div 6 ) );
  Replace( result, 'b', inttostr( round( Source.Mysticism / 16 ) ) + '-' + inttostr( round( Source.Mysticism / 8 ) ) );
end;

function TBlizzard.GetLoaded : Boolean;
const
  FailName : string = 'TBlizzard.GetLoaded';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if assigned( Resource ) then
      result := Resource.Loaded;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

class function TBlizzard.GetName : string;
begin
  result := 'Blizzard';
end;

function TBlizzard.Range( Source : TCharacter ) : Integer;
const
  FailName : string = 'TBlizzard.Range';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := Round( Source.Constitution * 50 * ( 6 + Source.Coordination / 30 ) );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TBlizzard.Recovery( Source : TCharacter ) : Integer;
const
  FailName : string = 'TBlizzard.Recovery';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 40 - ( ( Source.Constitution + Source.Mysticism ) div 2 );
    if result < 0 then
      result := 0;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TReflect }

function TReflect.Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean;
var
  Effect : TReflectEffect;
  NewTarget : TCharacter;
const
  FailName : string = 'TReflect.Cast';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := False;
    if assigned( Target ) then
    begin
      if not ( Target is TCharacter ) then
        exit;
      if TCharacter( Target ).Dead then
        exit;
      NewTarget := TCharacter( Target );
    end
    else
      NewTarget := Source;

    result := inherited Cast( Source, NewTarget );
    if not result then
      Exit;

    Effect := TReflectEffect.Create;
    Effect.Reflect := Source.TitleExists( 'Reflect' );
    Effect.Resource := Resource;
    Effect.AnimationDuration := 8 * Resource.FrameMultiplier;
    Effect.Duration := Source.Mysticism * 20;
    Effect.Power := Source.Mysticism * 10;
    TReflectEffect( Effect ).HitResource := HitResource;
    Effect.tag := 183;
    Effect.DoAction( 'Default', NewTarget.FacingString );

    with NewTarget do
    begin
      AddEffect( Effect );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

constructor TReflect.Create;
const
  FailName : string = 'TReflect.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    CastingType := ctProtection;
    TargetType := ttFriend;
    CastEffect := ReflectEffect;

    Resource := LoadArtResource( 'engine\spells\ReflectReceive.gif', true );
    if assigned( Resource ) then
    begin
      Resource.DrawShadow := false;
      Resource.SpecialEffect := seAdd;
    end;

    HitResource := LoadArtResource( 'engine\spells\ReflectHit.gif', true );
    if assigned( HitResource ) then
    begin
      HitResource.DrawShadow := false;
      HitResource.SpecialEffect := seAdd;
    end;

    SoundInCast := false;
    LoadCastSounds( 'reflect' );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TReflect.Destroy;
const
  FailName : string = 'TReflect.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    Resource.free;
    Resource := nil;
    HitResource.free;
    HitResource := nil;
    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TReflect.Drain( Source : TCharacter ) : Single;
const
  FailName : string = 'TReflect.Drain';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 20 + round( Source.Restriction / 10 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TReflect.GetIconXY( Source : TCharacter ) : TPoint;
const
  FailName : string = 'TReflect.GetIconXY';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result.X := 22 * 32;
    result.Y := 0;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TReflect.GetLoaded : Boolean;
const
  FailName : string = 'TReflect.GetLoaded';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if assigned( Resource ) and assigned( HitResource ) then
      result := Resource.Loaded and HitResource.Loaded;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TReflect.Range( Source : TCharacter ) : Integer;
const
  FailName : string = 'TReflect.Range';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := Round( Source.Mysticism * 10 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TReflect.Recovery( Source : TCharacter ) : Integer;
const
  FailName : string = 'TReflect.Recovery';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 40 - ( ( Source.Coordination + Source.Mysticism ) div 4 );
    if result < 0 then
      result := 0;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TReflect.GetInfo( Source : TCharacter ) : string;
begin
  result := InfoText;
end;

class function TReflect.GetName : string;
begin
  result := 'Deflect';
end;

{ TReflectEffect }

procedure TReflectEffect.Adjust( Character : TCharacter );
begin
  inherited;
  FCharacter := Character;
end;

function TReflectEffect.Hit( Source : TAniFigure; Damage : PDamageProfile ) : boolean;
var
  Effect : TEffect;
  Direction : TFacing;
  S : string;
begin
  result := false;
  if Source is TProjectile then
  begin
    if TProjectile( Source ).Magic > 0 then
    begin
      if TProjectile( Source ).Magic > Power then
      begin
        Power := 0;
        Duration := 0;
      end
      else
      begin
        Power := Power - TProjectile( Source ).Magic;
        Direction := Character.GetFacing( FCharacter.X, FCharacter.Y, Source.X, Source.Y );
        case Direction of
          fSS : S := 'SS';
          fSE : S := 'SE';
          fEE : S := 'EE';
          fNE : S := 'NE';
          fNN : S := 'NN';
          fNW : S := 'NW';
          fWW : S := 'WW';
          fSW : S := 'SW';
        end;

        Effect := TEffect.Create;
        Effect.Resource := HitResource;
        Effect.AnimationDuration := 8 * Resource.FrameMultiplier;
        Effect.DoAction( 'Default', S );

        result := true;
        if Reflect then
        begin
          TProjectile( Source ).Launch( FCharacter, TProjectile( Source ).FSource, //Reflect
            TProjectile( Source ).FSource.X, TProjectile( Source ).FSource.Y );
        end
        else
        begin
          TProjectile( Source ).Launch( FCharacter, nil, //Deflect
            TProjectile( Source ).FTarget.X, TProjectile( Source ).FTarget.Y );
        end;

        FCharacter.AddEffect( Effect );
      end;
    end;
  end;
end;

{ TDamagingEffect }

procedure TDamagingEffect.Adjust( Character : TCharacter );
begin
  inherited;
  FCharacter := Character;
end;

function TDamagingEffect.DoFrame : boolean;
var
  Stun, Total : single;
begin
  result := inherited DoFrame;
  if not result then
  begin
    inc( FrameCount );
    if FrameCount = TriggerFrame then
    begin
      FCharacter.AffectDamage( Source, @Damage );
      Total := CalcTotalDamage( Damage, FCharacter.Resistance, 1, false );
      Stun := CalcDamage( Damage.Stun ) - FCharacter.Resistance.Stun.Invulnerability;
      if Stun > 0 then
        Stun := Stun * ( 1 - FCharacter.Resistance.Stun.Resistance );
      FCharacter.TakeDamage( Source, Total, Stun, UseStealth );
    end;
  end;
end;

{ TFirefly }

function TFirefly.Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean;
var
  NewProjectile : TProjectile;
  Z1, Z2 : Integer;
  i, j, Count : integer;
  Angle : single;
  D : double;
  A : single;
  X, Y : longint;
  Tail1, Tail2 : TProjectile;
  TargetY : longint;
const
  FailName : string = 'TFirefly.Cast';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := inherited Cast( Source, Target );
    if not result then
      Exit;
    Z1 := Source.Height div 2 - 16;
    if Assigned( Target ) then
      Z2 := 0
    else
      Z2 := Z1;

    Count := Source.Mysticism div 4;
    if Count < 3 then
      Count := 3;
    Angle := pi2 / Count;
    TargetY := Source.TargetY + Z2;
    D := sqrt( sqr( Source.X - Source.TargetX ) + sqr( 2 * ( Source.Y - TargetY ) ) );
    A := ATan( Source.TargetX - Source.X, 2 * ( TargetY - Source.Y ) );
    for i := 0 to Count - 1 do
    begin
      NewProjectile := TProjectile( Sprites.NewSprite( TFireflyProjectile, Resource2, Source.X, Source.Y, Z1, 1 ) );
      if assigned( NewProjectile ) then
      begin
        NewProjectile.Magic := Source.Mysticism * 2;
        ZeroMemory( @NewProjectile.Damage, sizeof( NewProjectile.Damage ) );
        NewProjectile.Damage.Heat.Min := 0;
        NewProjectile.Damage.Heat.Max := Source.Mysticism / 8;
        NewProjectile.Damage.Magic.Min := 1;
        NewProjectile.Damage.Magic.Max := Source.Mysticism / 3;
        NewProjectile.DamageRadius := 0;
        NewProjectile.Duration := Source.Constitution * 10 + random( 100 );
        NewProjectile.TrackingDegree := 0;
        NewProjectile.Speed := 3 + random + Source.Coordination / 10;
        NewProjectile.SpecialEffect := seAdd;
        NewProjectile.HitIncidental := 1;
        NewProjectile.HitTarget := 1;
        NewProjectile.UseLineOfSight := True;
        NewProjectile.Frame := 6;
        NewProjectile.ColorR := 400;
        NewProjectile.ColorG := 192;
        NewProjectile.ColorB := 0;
        NewProjectile.UseLighting := true;
        TFireflyProjectile( NewProjectile ).ScanRange := Source.Mysticism * 4;

        Tail1 := NewProjectile;
        for j := 1 to 1 do
        begin
          Tail2 := TProjectile( Sprites.NewSprite( TProjectile, Resource1, Source.X, Source.Y, Z1, 1 ) );
          Tail2.Passive := true;
          Tail1.TrailedBy := Tail2;
          Tail2.Alpha := 55;
          Tail2.Frame := 6;
          Tail2.ColorR := NewProjectile.ColorR;
          Tail2.ColorG := NewProjectile.ColorG;
          Tail2.ColorB := NewProjectile.ColorB;
          Tail2.UseLighting := NewProjectile.UseLighting;
          Tail1 := Tail2
        end;

        Tail1.TrailedBy := nil;

        X := Source.X + round( cos( A + Angle * i ) * D );
        Y := Source.Y + round( sin( A + Angle * i ) * D / 2 );
        NewProjectile.Launch( Source, nil, X, Y );
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

constructor TFirefly.Create;
const
  FailName : string = 'TFirefly.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    CastingType := ctCombat;
    TargetType := ttEnemy;
    CastEffect := PushEffect;
    Resource1 := TLightning( Lightning ).SmallResource;
    Resource2 := TLightning( Lightning ).MediumResource;
    Resource3 := TLightning( Lightning ).LargeResource;
    LoadCastSounds( 'FireCast' );
    SoundInCast := true;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TFirefly.Drain( Source : TCharacter ) : Single;
const
  FailName : string = 'TFirefly.Drain';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 10 + round( Source.Restriction / 10 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TFirefly.GetIconXY( Source : TCharacter ) : TPoint;
const
  FailName : string = 'TFirefly.GetIconXY';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result.X := 10 * 32;
    result.Y := 0;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TFirefly.GetInfo( Source : TCharacter ) : string;
begin
  result := InfoText;
  Replace( result, 'a', '1-' + inttostr( round( Source.Mysticism / 2 ) ) );
  Replace( result, 'b', '0-' + inttostr( round( Source.Mysticism / 4 ) ) );
end;

function TFirefly.GetLoaded : Boolean;
const
  FailName : string = 'TFirefly.GetLoaded';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := Resource1.Loaded and Resource2.Loaded and Resource3.Loaded;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

class function TFirefly.GetName : string;
begin
  result := 'Firefly';
end;

function TFirefly.Range( Source : TCharacter ) : Integer;
const
  FailName : string = 'TFirefly.Range';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := Round( Source.Constitution * 10 * ( 8 + Source.Coordination / 10 ) );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TFirefly.Recovery( Source : TCharacter ) : Integer;
const
  FailName : string = 'TFirefly.Recovery';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 10 - ( Source.Constitution div 2 );
    if result < 0 then
      result := 0;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TFireflyProjectile }

procedure TFireflyProjectile.Arrival( sender : TObject );
var
  List : TList;
  i : integer;
begin
  List := FindEnemies;
  if assigned( List ) then
  begin
    i := random( List.count );
    NewTargetX := TCharacter( List.items[ i ] ).X + ( TCharacter( List.items[ i ] ).X - X );
    NewTargetY := TCharacter( List.items[ i ] ).Y + ( TCharacter( List.items[ i ] ).Y - Y );
    List.free;
    WanderMode := false;
  end
  else
  begin
    Direction := ( random < 0.5 );
    WanderMode := true;
  end;
  Count := 0;
end;

procedure TFireflyProjectile.CollideFigure( Source, Target : TAniFigure;
  var Stop : Boolean );
begin
  if Target = FSource then
    Exit;
  if Target is TCharacter then
  begin
    if not TCharacter( Target ).Dead then
    begin
      if TCharacter( FSource ).IsEnemy( TCharacter( Target ) ) then
      begin
{$IFDEF AILog}
        Log.Log( 'Projectile strikes ' + TCharacter( Target ).GUID );
{$ENDIF}
        if assigned( TCharacter( Target ).Equipment[ slChest3 ] ) and
          ( TCharacter( Target ).Equipment[ slChest3 ].Material = maMetal ) then
          PlaySingleSound( StrikeMetalSound, X, Y )
        else if assigned( TCharacter( Target ).Equipment[ slChest2 ] ) and
          ( TCharacter( Target ).Equipment[ slChest2 ].Material = maMetal ) then
          PlaySingleSound( StrikeMetalSound, X, Y )
        else if assigned( TCharacter( Target ).Equipment[ slChest1 ] ) and
          ( TCharacter( Target ).Equipment[ slChest1 ].Material = maMetal ) then
          PlaySingleSound( StrikeMetalSound, X, Y )
        else
          PlaySingleSound( StrikeLeatherSound, X, Y );
        DoDamage( TSpriteObject( Target ) );
      end;
    end;
  end
  else if Target is TDoor then
  begin
    if TDoor( Target ).Closed then
    begin
      PlaySingleSound( StrikeWallSound, X, Y );
      Stop := True;
      Disable;
    end;
  end;
end;

procedure TFireflyProjectile.DoFrame;
var
  Tail : TProjectile;
  A : single;
begin
  inherited;
  Tail := self;
  while assigned( Tail ) do
  begin
    if longint( Duration ) < Tail.Alpha then
      Tail.Alpha := Duration;
    Tail := Tail.TrailedBy;
  end;
  dec( Count );
  if Count <= 0 then
  begin
    if WanderMode then
    begin
      if Direction then
        Angle := Angle + random( 11 ) - 8
      else
        Angle := Angle + random( 11 ) - 2;
      if random < 0.05 then
        Direction := not Direction;
      A := pi * Angle / 180;
      Move( X + round( 256 * cos( A ) ), Y + round( 128 * sin( A ) ), Z );
      if ( Duration mod 15 ) = 0 then
        Arrival( self );
    end
    else
    begin
      MoveTo( NewTargetX, NewTargetY, Z );
    end;
  end;
end;

function TFireflyProjectile.FindEnemies : TList;
var
  i : Integer;
  List : TList;
begin
  result := nil;

  List := Game.FindInRadius( X, Y, ScanRange );
  try
    if Assigned( List ) then
    begin
      for i := 0 to List.Count - 1 do
      begin
        if TAniFigure( List.Items[ i ] ) is TCharacter then
        begin
          if not TCharacter( List.Items[ i ] ).Dead and
            ( FSource.IsEnemy( TCharacter( List.Items[ i ] ) ) or TCharacter( List.Items[ i ] ).IsEnemy( FSource ) ) then
          begin
            if not Assigned( result ) then
              result := TList.Create;
            result.Add( List.Items[ i ] );
          end;
        end;
      end;
    end;
  finally
    List.Free;
  end;
end;

procedure TFireflyProjectile.Launch( Source : TCharacter;
  Target : TSpriteObject; X, Y : Integer );
begin
  OnCollideItem := CollideItem;
  OnCollideFigure := CollideFigure;
  OnCollideBoundary := CollideBoundary;
  OnStop := Arrival;
  OnMove := MoveEvent;
  ZFix := Z;
  FSource := Source;
  FTarget := Target;
  OnScriptEnd := nil;
  Exploding := False;
  TrackingDegree := 0;
  MoveTo( X, Y, Z );
  WanderMode := true;
  Angle := round( 180 * ATan( X - Source.X, Y - Source.Y ) / pi );
  Direction := ( random < 0.5 );
  Count := 1000;
end;

{ TInfernoProjectile }

procedure TInfernoProjectile.DoFrame;
var
  dX, dY : double;
  D, T, A : double;
begin
  inherited;
  if not Exploding then
  begin
    if assigned( Core ) then
    begin
      inc( Angle, SpinRate );
      dX := Core.X - Core.PrevX;
      dY := Core.Y - Core.PrevY;
      D := sqrt( sqr( dX ) + sqr( dY ) );
      A := Angle * pi / 180;
      T := SpinRadius * sin( A );
      MoveTo( Core.X + round( T * dY / D ), Core.Y + round( T * dX / D ), Core.Z + round( SpinRadius * cos( A ) ) );
    end;
  end;
end;

{ TInfernoCoreProjectile }

procedure TInfernoCoreProjectile.CollideBoundary( Source : TAniFigure );
begin
  inherited;
end;

procedure TInfernoCoreProjectile.CollideFigure( Source, Target : TAniFigure;
  var Stop : Boolean );
begin
  Stop := false;
end;

procedure TInfernoCoreProjectile.CollideItem( Source : TAniFigure;
  var Stop : Boolean );
begin
  Stop := false;
end;

procedure TInfernoCoreProjectile.Render;
begin
//  inherited;
end;

end.
