unit Spells1;
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
  Engine,
  MiscAI,
  Character,
  Resource,
  Spells,
  LogFile;

var
  GreatFoot : TSpell;
  MirrorImage : TSpell;
  HealAll : TSpell;
  Blade : TSpell;
  SummonResource : TResource;
  MageGreenSpellCast : TResource;
  MageBlueSpellCast : TResource;
  MageRedSpellCast : TResource;
  MageOrangeSpellCast : TResource;
  MageYellowSpellCast : TResource;
  MagePurpleSpellCast : TResource;
  MithrasSpellCast : TResource;
  HellHoundCast : TResource;
  MageGreenSpellEffect : TResource;
  MageBlueSpellEffect : TResource;
  MageRedSpellEffect : TResource;
  MageOrangeSpellEffect : TResource;
  MageYellowSpellEffect : TResource;
  MagePurpleSpellEffect : TResource;
  MithrasSpellEffect : TResource;
  HeatProtectionRing : TResource;
  WindShearReceive : TResource;
  MagicProtectionReceive : TResource;
  MagicProtectionHit : TResource;
  Glow : TResource;
  Smoken : TResource;
  FrostDoom : TResource;
  RotatingBlades : TResource;

type
  TGreatFoot = class( TSpell )
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

  TRotatingBlades = class( TSpell )
  private
    //Resource: TResource;
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

  TMirrorImage = class( TSpell )
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

  THealAll = class( TSpell )
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


function LoadSpells1 : boolean;

implementation

uses
  BasicHumanoidAI;

function LoadSpells1 : boolean;
begin
  result := false;

  if not MakeCastEffect( SummonResource, 'SummonReceive' ) then
    exit;
  if not MakeCastEffect( MageGreenSpellCast, 'MageGreenSpellcast1' ) then
    exit;
  if not MakeCastEffect( MageBlueSpellCast, 'MageBlueSpellcast1' ) then
    exit;
  if not MakeCastEffect( MageRedSpellCast, 'MageRedSpellcast1' ) then
    exit;
  if not MakeCastEffect( MageOrangeSpellCast, 'MageOrangeSpellcast1' ) then
    exit;
  if not MakeCastEffect( MagePurpleSpellCast, 'MagePurpleSpellcast1' ) then
    exit;
  if not MakeCastEffect( MageYellowSpellCast, 'MageYellowSpellcast1' ) then
    exit;
  if not MakeCastEffect( MithrasSpellCast, 'MithrasSpellcast1' ) then
    exit;
  if not MakeCastEffect( HellHoundCast, 'ProtectionReceive(Fire)' ) then
    exit;

  if not MakeCastEffect( MageGreenSpellEffect, 'MageGreenReceive' ) then
    exit;
  if not MakeCastEffect( MageBlueSpellEffect, 'MageBlueReceive' ) then
    exit;
  if not MakeCastEffect( MageRedSpellEffect, 'MageRedReceive' ) then
    exit;
  if not MakeCastEffect( MageOrangeSpellEffect, 'MageOrangeReceive' ) then
    exit;
  if not MakeCastEffect( MagePurpleSpellEffect, 'MagePurpleReceive' ) then
    exit;
  if not MakeCastEffect( MageYellowSpellEffect, 'MageYellowReceive' ) then
    exit;
  if not MakeCastEffect( MithrasSpellEffect, 'MithrasSpellReceive' ) then
    exit;
  if not MakeCastEffect( Glow, 'Glow' ) then
    exit;
  if not MakeCastEffect( HeatProtectionRing, 'HeatProtectionRing' ) then
    exit;
  if not MakeCastEffect( WindShearReceive, 'WindShearReceive' ) then
    exit;
  if not MakeCastEffect( MagicProtectionReceive, 'MagicProtectionReceive' ) then
    exit;
  if not MakeCastEffect( Smoken, 'Smoken' ) then
    exit;
  if not MakeCastEffect( MagicProtectionHit, 'MagicProtectionHit' ) then
    exit;
  if not MakeCastEffect( FrostDoom, 'Frost(LVL3)' ) then
    exit;
  if not MakeCastEffect( RotatingBlades, 'RotatingBladesCast' ) then
    exit;
  if not MakeSpell( MirrorImage, TMirrorImage ) then
    exit;
  if not MakeSpell( HealAll, THealAll ) then
    exit;

  MakeSpell( Blade, TRotatingBlades );

  //  if not MakeSpell(GreatFoot,TGreatFoot) then exit;
  result := true;
end;

{ TGreatFoot }

function TGreatFoot.Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean;
var
  Effect : TDamagingEffect;
const
  FailName : string = 'TGreatFoot.Cast';
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

procedure TGreatFoot.Clear;
begin
  Resource.RLE.free; Resource.RLE := nil;
end;

constructor TGreatFoot.Create;
const
  FailName : string = 'TGreatFoot.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    CastingType := ctCombat;
    TargetType := ttEnemy;
    CastEffect := GreatHandEffect;
    Resource := LoadArtResource( 'engine/spells/GreatHandReceive.gif', true );
    LoadCastSounds( 'Charge' );
    Resource.DrawShadow := false;
    SoundInCast := true;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TGreatFoot.Destroy;
const
  FailName : string = 'TGreatFoot.Destroy';
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

function TGreatFoot.Drain( Source : TCharacter ) : Single;
const
  FailName : string = 'TGreatFoot.Drain';
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

function TGreatFoot.GetIconXY( Source : TCharacter ) : TPoint;
const
  FailName : string = 'TGreatFoot.GetIconXY';
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

function TGreatFoot.GetInfo( Source : TCharacter ) : string;
begin
  result := '';
end;

function TGreatFoot.GetLoaded : Boolean;
const
  FailName : string = 'TGreatFoot.GetLoaded';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := Resource.Loaded;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

class function TGreatFoot.GetName : string;
begin
  result := 'Great Foot';
end;

function TGreatFoot.Range( Source : TCharacter ) : Integer;
const
  FailName : string = 'TGreatFoot.Range';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := round( Source.Mysticism * 50 * ( 24 + Source.Mysticism / 10 ) );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TGreatFoot.Recovery( Source : TCharacter ) : Integer;
const
  FailName : string = 'TGreatFoot.Recovery';
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

function TMirrorImage.Cast( Source : TCharacter;
  Target : TSpriteObject ) : Boolean;
var
  Effect : TEffect;
  NewCharacter : TCharacter;
  i : integer;
const
  FailName : string = 'TMirrorImage.Cast';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

 // if assigned(Target) then exit;
    if not ( AllDead( 'pmirror' ) ) then
      exit;


    result := inherited Cast( Source, Target );
    if not result then
      Exit;

    for i := 0 to 2 do
    begin
      NewCharacter := nil;
      Effect := nil;

      Source.Clone( TObject( NewCharacter ), 'PMI' + IntToStr( Random( 25 ) * Random( 10 ) ) );

      Effect := TEffect.Create;
      Effect.Resource := Resource;
      Effect.AnimationDuration := 8 * Resource.FrameMultiplier;
      Effect.Power := Source.Mysticism;
      Effect.DisableWhenDone := true;
      Effect.Duration := Source.Mysticism * 10;
      Effect.DoAction( 'Default', NewCharacter.FacingString );

      case i of
        0 : NewCharacter.SetPos( Source.X, Source.Y + 50, Source.z );
        1 : NewCharacter.SetPos( Source.X + 50, Source.Y, Source.z );
        2 : NewCharacter.SetPos( Source.X - 50, Source.Y, Source.z );
      end;
      if Source.titleExists( 'Apprentice' ) then
      begin
        NewCharacter.addtitle( 'mage' );
        NewCharacter.addtitle( 'MagicDefensive' );
      end;

      if Source.titleExists( 'Hunter' ) then
      begin
        NewCharacter.addtitle( 'scout' );
        NewCharacter.addtitle( 'MeleeRanged' );
      end;

      if Source.titleExists( 'Squire' ) then
      begin
        NewCharacter.addtitle( 'fighter' );
        NewCharacter.addtitle( 'MeleeAgressive' );
      end;

      NewCharacter.addtitle( 'combative' );
      NewCharacter.addtitle( 'stayclose' );
      NewCharacter.addtitle( 'caster' );


      NewCharacter.addtitle( 'charge' );
      NewCharacter.addtitle( 'hold' );
      NewCharacter.addtitle( 'healfirst' );
      NewCharacter.addtitle( 'frost' );
      NewCharacter.addtitle( 'flame' );
      NewCharacter.addtitle( 'heal' );
      NewCharacter.addtitle( 'greater healing' );
      NewCharacter.addtitle( 'fireball' );
      NewCharacter.addtitle( 'Aura of Iron' );
      NewCharacter.addtitle( 'aura of steel' );

      NewCharacter.onDie := 'Doeffect(fadeaway)';
      NewCharacter.Properties[ 'BalanceWithPlayer' ] := '1';
      NewCharacter.Properties[ 'equipmentlocked' ] := 'true';

      NewCharacter.Highlightable := true;
      NewCharacter.GroupName := 'pmirror';
      NewCharacter.AddEffect( Effect );
      NewCharacter.Spawned := false;
      NewCharacter.Stand;
      NewCharacter.AIMode := AIParty;
      NewCharacter.CombatAI := 'HumanoidCasterCombat';
      NewCharacter.IdleAI := 'HumanoidIdle';
      NewCharacter.PartyAI := 'Companion';
      NewCharacter.CombatMode := true;
      NewCharacter.HitPoints := Source.HitPoints / 3;


    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

constructor TMirrorImage.Create;
const
  FailName : string = 'TMirrorImage.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    CastingType := ctSummoning;
    TargetType := ttNone;
    CastEffect := SummonEffect;
    Resource := LoadArtResource( 'engine/spells/summonreceive.gif', true );
    Resource.DrawShadow := false;
    SoundInCast := false;
    LoadCastSounds( 'Summon' );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TMirrorImage.Destroy;
const
  FailName : string = 'TMirrorImage.Destroy';
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

function TMirrorImage.Drain( Source : TCharacter ) : Single;
const
  FailName : string = 'TMirrorImage.Drain';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    result := Source.Mana;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TMirrorImage.GetIconXY( Source : TCharacter ) : TPoint;
const
  FailName : string = 'TMirrorImage.GetIconXY';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    result.X := 23 * 32;
    result.Y := 32;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TMirrorImage.GetInfo( Source : TCharacter ) : string;
begin
  result := InfoText;
  //result:='Summons three mirror images' +#13 + 'of the caster for a short time.';
end;

function TMirrorImage.GetLoaded : Boolean;
const
  FailName : string = 'TMirrorImage.GetLoaded';
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

class function TMirrorImage.GetName : string;
begin
  result := 'Mirror Image';
end;

function TMirrorImage.Range( Source : TCharacter ) : Integer;
const
  FailName : string = 'TMirrorImage.Range';
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

function TMirrorImage.Recovery( Source : TCharacter ) : Integer;
const
  FailName : string = 'TMirrorImage.Recovery';
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


{ THealAll }

function THealAll.Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean;
var
  Points : single;
  iLoop : integer;
  Effect : TEffect;
  NewTarget : TCharacter;
const
  FailName : string = 'THealAll.Cast';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    for iLoop := 0 to NPCList.Count - 1 do
    begin
      NewTarget := TCharacter( NPCList[ iLoop ] );
      result := False;
      result := inherited Cast( Source, NewTarget );
      if not result then
        continue;

      Effect := TEffect.Create;
      Effect.Resource := Resource;
      Effect.AnimationDuration := 8 * Resource.FrameMultiplier;
      Effect.Power := Source.Mysticism;
      Effect.DoAction( 'Default', NewTarget.FacingString );

      with NewTarget do
      begin
        Points := Source.Mysticism / 4;
        if Points > Wounds then
          Points := Wounds;
        Wounds := Wounds - Points;
        AddEffect( Effect );
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure THealAll.Clear;
begin
  Resource.RLE.Free; Resource.RLE := nil;
end;

constructor THealAll.Create;
const
  FailName : string = 'THealAll.Create';
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
    Resource := LoadArtResource( 'engine/spells/Healreceive.gif', true );
    LoadCastSounds( 'Heal' );
 // Resource.DrawShadow:=false;
    SoundInCast := false;
    Interupted := false;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor THealAll.Destroy;
const
  FailName : string = 'THealAll.Destroy';
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

function THealAll.Drain( Source : TCharacter ) : Single;
const
  FailName : string = 'THealAll.Drain';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := ( 10 * NPCList.Count ) + round( Source.Restriction / 10 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function THealAll.GetIconXY( Source : TCharacter ) : TPoint;
const
  FailName : string = 'THealAll.GetIconXY';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result.X := 16 * 32;
    result.Y := 32;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function THealAll.GetInfo( Source : TCharacter ) : string;
begin
  result := InfoText;
  Replace( result, 'a', inttostr( round( Source.Mysticism / 4 ) ) );
end;

function THealAll.GetLoaded : Boolean;
const
  FailName : string = 'THealAll.GetLoaded';
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

class function THealAll.GetName : string;
begin
  result := 'Heal All';
end;

function THealAll.Range( Source : TCharacter ) : Integer;
const
  FailName : string = 'THealAll.Range';
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

function THealAll.Recovery( Source : TCharacter ) : Integer;
const
  FailName : string = 'THealAll.Recovery';
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


{ TRotatingBlades }

function TRotatingBlades.Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean;
var
  iLoop : integer;
  List : TStringList;
const
  FailName : string = 'TRotatingBlades.Cast';
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

    List := GetNearbyCharacter( TCharacter( Source ), 50 );
    if assigned( List ) then
    begin
      for iLoop := List.count - 1 downto 0 do
      begin
        if List.Objects[ iLoop ] = TCharacter( Source ) then
          List.Delete( iLoop )
        else if not ( Source.IsEnemy( TCharacter( List.Objects[ iLoop ] ) ) ) then
          List.Delete( iLoop );

      end;
      if List.count > 0 then
      begin
        for iLoop := 0 to List.Count - 1 do
          TCharacter( List.Objects[ iLoop ] ).TakeDamage( TCharacter( Source ), ( 10 + TCharacter( Source ).mysticism / 10 ), 0, False );
      end;
    end;


  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TRotatingBlades.Clear;
begin
//  Resource.RLE.Free; Resource.RLE:=nil;
end;

constructor TRotatingBlades.Create;
const
  FailName : string = 'TRotatingBlades.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
    CastingType := ctCombat;
    TargetType := ttEnemy;
    CastEffect := RotatingBlades;
    LoadCastSounds( 'pushspell3' );
    SoundInCast := false;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TRotatingBlades.Destroy;
const
  FailName : string = 'TRotatingBlades.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

 // Resource.Free;
    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TRotatingBlades.Drain( Source : TCharacter ) : Single;
const
  FailName : string = 'TRotatingBlades.Drain';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

//  result := source.mana;
    result := 20 + round( Source.Restriction / 10 );
 // log.log('Jah ' + IntTOStr(25+round(Source.Restriction/10)));

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TRotatingBlades.GetIconXY( Source : TCharacter ) : TPoint;
const
  FailName : string = 'TRotatingBlades.GetIconXY';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    result.X := 13 * 32;
    result.Y := 32;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TRotatingBlades.GetInfo( Source : TCharacter ) : string;
begin
  result := InfoText;
  Replace( result, 'a', inttostr( 10 + round( TCharacter( Source ).mysticism / 10 ) ) );
end;

function TRotatingBlades.GetLoaded : Boolean;
const
  FailName : string = 'TRotatingBlades.GetLoaded';
begin
  result := False;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    result := true;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

class function TRotatingBlades.GetName : string;
begin
  result := 'Blades';
end;

function TRotatingBlades.Range( Source : TCharacter ) : Integer;
const
  FailName : string = 'TRotatingBlades.Range';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

//  result := 10*(Source.Constitution + Source.Mysticism);

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TRotatingBlades.Recovery( Source : TCharacter ) : Integer;
const
  FailName : string = 'TRotatingBlades.Recovery';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    result := 0;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;


end.

