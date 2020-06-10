unit SoAOS.Effects;
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

  Description: SoAOS Effect classes - was named of Effect.pas

  Requires: Delphi 10.3.3 or later

  Revision History:
  - 13 Jul 2003 - DL: Initial Upload to CVS
  - 10 Mar 2019 - SN: Forked on GitHub
  see git repo afterwards

*)

interface

uses
  System.Classes,
  System.SysUtils,
  System.Types,
  Resource,
  Character,
  SoAOS.Graphics.Types,
  SoAOS.AI.Types,
  SoAOS.AI,
  SoAOS.Animation,
  Spells;

type
  TSwirl = class( TEffect )
  private
    Points : integer;
    PointList : pointer;
    FCharacter : TCharacter;
    FAlpha : integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Adjust( Character : TCharacter ); override;
    procedure RenderLocked( Figure : TAniFigure; Bits : PBITPLANE ); override;
    function DoFrame : boolean; override;
  end;

  TSpirit = class( TEffect )
  private
    Z : integer;
    Blend : integer;
  public
    constructor Create;
    procedure Adjust( Character : TCharacter ); override;
    procedure RenderLocked( Figure : TAniFigure; Bits : PBITPLANE ); override;
    function DoFrame : boolean; override;
  end;

  TFadeAway = class( TEffect )
  private
    FCharacter : TCharacter;
  public
    constructor Create;
    procedure Adjust( Character : TCharacter ); override;
    function DoFrame : boolean; override;
  end;

  TDeathPulse = class( TEffect )
  private
    FCharacter : TCharacter;
    Red : integer;
    RedDelta : integer;
    Blue : integer;
    BlueDelta : integer;
  public
    constructor Create;
    procedure Adjust( Character : TCharacter ); override;
    function DoFrame : boolean; override;
  end;

  TBurningRam = class( TEffect )
  private
    LowRes, HighRes : TResource;
    LowCount, HighCount : integer;
    LowFrame, HighFrame : integer;
    Sound : TSoundPlayer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Adjust( Character : TCharacter ); override;
    procedure RenderLocked( Figure : TAniFigure; Bits : PBITPLANE ); override;
    function DoFrame : boolean; override;
  end;

  TBodyRotEffect = class( TEffect )
  private
    FCharacter : TCharacter;
    Facing : integer;
    Frame : integer;
    Fade : integer;
  public
    constructor Create;
    procedure Adjust( Character : TCharacter ); override;
    procedure RenderLocked( Figure : TAniFigure; Bits : PBITPLANE ); override;
    function DoFrame : boolean; override;
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
    FCharacter : TCharacter;
  public
    HitResource : TResource;
    procedure Adjust( Character : TCharacter ); override;
    function Hit( Source : TAniFigure; Damage : PDamageProfile ) : boolean; override;
  end;

  TReflectEffect = class( TEffect )
  private
    FCharacter : TCharacter;
  public
    HitResource : TResource;
    Reflect : boolean;
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


type
  TSwirlBead = record
    X, Y, Z : single;
    Angle : single;
    Rise : single;
    Offset : single;
  end;

function GetNamedEffect( const Name : string ) : TEffect;

implementation

uses
  SoAOS.Types,
  SoAOS.AI.Helper,
  SoAOS.Projectile,
  AI1,
  Engine;

function GetNamedEffect( const Name : string ) : TEffect;
var
  S : string;
begin
  S := lowercase( Name );
  if S = 'swirl' then
    result := TSwirl.create
  else if S = 'spirit' then
    result := TSpirit.create
  else if S = 'fadeaway' then
    result := TFadeAway.create
  else if S = 'deathpulse' then
    result := TDeathPulse.create
  else if S = 'burningram' then
    result := TBurningRam.create
  else
    result := nil;
end;

{ TSwirl }

procedure TSwirl.Adjust( Character : TCharacter );
begin
  if Character.SpecialEffect = seNone then
  begin
    Character.SpecialEffect := seAdd;
    Character.Alpha := 100;
  end;
  FCharacter := Character;
  FAlpha := Character.Alpha * 2;
end;

constructor TSwirl.Create;
var
  i : integer;
  p : ^TSwirlBead;
begin
  inherited;
  AnimationDuration := 800;
  Resource := TLightning( Lightning ).SmallResource;
  Points := 80;
  DisableWhenDone := true;
  GetMem( PointList, Points * sizeof( TSwirlBead ) );
  p := PointList;
  for i := 1 to Points do
  begin
    p^.X := 0;
    p^.Y := 0;
    p^.Z := random * 10;
    p^.Angle := random * c2PI;
    p^.Rise := 0.25 + random / 2;
    p^.Offset := random * 30 - 15;
    inc( p );
  end;
end;

destructor TSwirl.Destroy;
begin
  FreeMem( PointList );
  inherited;
end;

function TSwirl.DoFrame : boolean;
var
  i : integer;
  p : ^TSwirlBead;
begin
  result := inherited DoFrame;
  if not result then
  begin
    p := PointList;
    for i := 1 to Points do
    begin
      p^.Angle := p^.Angle + 30 / 180;
      p^.Z := p^.Z + p^.Rise;
      p^.X := ( p^.Z + p^.Offset ) * cos( p^.Angle ) / 4;
      p^.Y := ( p^.Z + p^.Offset ) * sin( p^.Angle ) / 4;
      inc( p );
    end;
    if FAlpha > 0 then
      dec( FAlpha );
    FCharacter.Alpha := FAlpha div 2;
  end;
end;

procedure TSwirl.RenderLocked( Figure : TAniFigure; Bits : PBITPLANE );
var
  i : integer;
  p : ^TSwirlBead;
  Blend : integer;
begin
  p := PointList;
  for i := 1 to Points do
  begin
    Blend := 100 - round( p^.Z / 2 );
    if Blend > 0 then
      Resource.RLE.DrawColorize( 5, Figure.CenterX + round( p^.X ), Figure.CenterY + round( ( p^.Y - p^.Z ) / 2 ),
        Bits, round( p^.Z * 2 ), 50, 100, Blend, 100 );
    inc( p );
  end;
end;

{ TSpirit }

procedure TSpirit.Adjust( Character : TCharacter );
begin

end;

constructor TSpirit.Create;
begin
  inherited;
  AnimationDuration := 64;
  Blend := 64;
end;

function TSpirit.DoFrame : boolean;
begin
  result := inherited DoFrame;
  if not result then
  begin
    dec( Blend, 1 );
    inc( Z, 1 );
  end;
end;

procedure TSpirit.RenderLocked( Figure : TAniFigure; Bits : PBITPLANE );
begin
  if ( Figure.Resource is TCharacterResource ) and assigned( TCharacterResource( Figure.Resource ).NakedResource ) then
    TCharacterResource( Figure.Resource ).NakedResource.RLE.DrawColorize( ord( TSpriteObject( Figure ).Facing ) * 4, 0, -Z,
      Bits, 50, 200, 100, Blend, 100 )
  else
    TResource( Figure.Resource ).RLE.DrawColorize( ord( TSpriteObject( Figure ).Facing ) * 4, 0, -Z,
      Bits, 50, 200, 100, Blend, 100 );

end;

{ TFadeAway }

procedure TFadeAway.Adjust( Character : TCharacter );
begin
  if Character.SpecialEffect = seNone then
  begin
    Character.SpecialEffect := seAdd;
    Character.Alpha := 100;
  end;
  FCharacter := Character;
end;

constructor TFadeAway.Create;
begin
  inherited;
  AnimationDuration := 50;
  DisableWhenDone := true;
end;

function TFadeAway.DoFrame : boolean;
begin
  result := inherited DoFrame;
  if not result then
  begin
    if FCharacter.Alpha >= 2 then
      dec( FCharacter.Alpha, 2 );
  end;
end;

{ TDeathPulse }

procedure TDeathPulse.Adjust( Character : TCharacter );
begin
  FCharacter := Character;
//  FCharacter.ColorB:=150;
  Red := FCharacter.ColorR;
  Blue := FCharacter.ColorB;
end;

constructor TDeathPulse.Create;
begin
  inherited;
  RedDelta := 15;
  BlueDelta := 10;
end;

function TDeathPulse.DoFrame : boolean;
begin
  result := false;

  inc( Red, RedDelta );
  if Red >= 400 then
    RedDelta := -15
  else if Red <= 100 then
    RedDelta := 15;

  inc( Blue, BlueDelta );
  if Blue >= 400 then
    BlueDelta := -10
  else if Blue <= 100 then
  begin
    BlueDelta := 10;
    result := not FCharacter.InInventory( 'EarthStone' );
  end;

  FCharacter.ColorR := Red;
  FCharacter.ColorB := Blue;
end;

{ TBurningRam }

procedure TBurningRam.Adjust( Character : TCharacter );
begin
  if not assigned( Sound ) then
  begin
    Sound := TSoundPlayer.create( Character.X, Character.Y, Character.Z );
    Sound.Continuous := true;
    Sound.Filename := 'fire';
    Sounds.add( Sound );
  end;
end;

constructor TBurningRam.Create;
begin
  inherited;
  AnimationDuration := 5000;
  LowRes := LoadArtResource( 'engine\fx\lowfire.pox' );
  HighRes := LoadArtResource( 'engine\fx\hifire.pox' );
end;

destructor TBurningRam.Destroy;
var
  i : integer;
begin
  LowRes.Free;
  HighRes.Free;
  if assigned( Sound ) then
  begin
    i := Sounds.indexOf( Sound );
    if i >= 0 then
      Sounds.delete( i );
    Sound.free;
  end;
  inherited;
end;

function TBurningRam.DoFrame : boolean;
begin
  result := inherited DoFrame;
  if result then
    Sound.SoundOn := false;
end;

procedure TBurningRam.RenderLocked( Figure : TAniFigure; Bits : PBITPLANE );
var
  R, G, B : integer;
  Blend : integer;
begin
  if assigned( HighRes ) and ( AnimationDuration < 4000 ) and ( AnimationDuration > 1000 ) then
  begin
    inc( HighCount );
    if HighCount > 0 then
    begin
      HighCount := 0;
      inc( HighFrame );
      if HighFrame > 6 then
        HighFrame := 0;
    end;
    if AnimationDuration > 2500 then
    begin
      Blend := 100 * ( 4000 - AnimationDuration ) div 1500;
      R := 75 + 50 * ( 4000 - AnimationDuration ) div 1500;
      G := 50 + 50 * ( AnimationDuration - 2500 ) div 1500;
      B := 25;
    end
    else
    begin
      Blend := 100 * ( AnimationDuration - 1000 ) div 1500;
      R := 75 + 50 * ( AnimationDuration - 1000 ) div 1500;
      G := 50 + 50 * ( 2500 - AnimationDuration ) div 1500;
      B := 25;
    end;
    HighRes.RLE.DrawColorize( HighFrame, 0, 0, Bits, R, G, B, Blend, 100 );
  end;
  if assigned( LowRes ) then
  begin
    inc( LowCount );
    if LowCount > 1 then
    begin
      LowCount := 0;
      inc( LowFrame );
      if LowFrame > 6 then
        LowFrame := 0;
    end;
    if AnimationDuration > 2500 then
    begin
      if AnimationDuration > 4900 then
        Blend := ( 5000 - AnimationDuration )
      else
        Blend := 100;
      R := 75 + 25 * ( AnimationDuration - 2500 ) div 2500;
      G := 75 - 25 * ( AnimationDuration - 2500 ) div 2500;
      B := 25 * ( 5000 - AnimationDuration ) div 2500;
    end
    else
    begin
      if AnimationDuration < 100 then
        Blend := AnimationDuration
      else
        Blend := 100;
      if AnimationDuration < 80 then
        Sound.Volume := AnimationDuration;
      R := 75;
      G := 75 - 25 * ( 2500 - AnimationDuration ) div 2500;
      B := 25;
    end;
    LowRes.RLE.DrawColorize( LowFrame, 0, 0, Bits, R, G, B, Blend, 100 );
  end;

end;

{ TBodyRotEffect }

procedure TBodyRotEffect.Adjust( Character : TCharacter );
begin
  inherited;
  if not assigned( FCharacter ) then
  begin
    FCharacter := Character;
    Facing := ord( FCharacter.Facing ) * 3;
    Frame := -1;
    DisableWhenDone := true;
    AnimationDuration := 3300;
  end;
end;

constructor TBodyRotEffect.Create;
begin
  inherited;
  if not assigned( BodyRotResource ) then
  begin
    BodyRotResource := LoadArtResource( 'engine\fx\CorpseRot.gif' );
  end;
end;

function TBodyRotEffect.DoFrame : boolean;
begin
  result := inherited DoFrame;
  if not Result then
  begin
    if ( ( AnimationDuration - 100 ) mod 800 ) = 0 then
    begin
      inc( Frame );
      Fade := 100;
      if Frame = 0 then
      begin
        if FCharacter.SpecialEffect <> seTranslucent then
        begin
          FCharacter.SpecialEffect := seTranslucent;
          FCharacter.Alpha := 100;
        end;
        if Fade < FCharacter.Alpha then
          FCharacter.Alpha := Fade;
      end;
    end
    else if Fade > 0 then
    begin
      dec( Fade );
      if ( Frame = 0 ) then
      begin
        if ( Fade = 0 ) then
          FCharacter.Frame := 0
        else
        begin
          if Fade < FCharacter.Alpha then
            FCharacter.Alpha := Fade;
        end;
      end;
    end;
  end;
end;

procedure TBodyRotEffect.RenderLocked( Figure : TAniFigure; Bits : PBITPLANE );
var
  R, G, B : integer;
begin
  if assigned( BodyRotResource ) then
  begin
    if Figure.UseLighting then
    begin
      R := 100 * Figure.LightR div 255;
      G := 100 * Figure.LightG div 255;
      B := 100 * Figure.LightB div 255;
      if Fade > 0 then
      begin
        if Frame = 0 then
        begin
          BodyRotResource.RLE.DrawColorize( Facing + Frame, 0, 0, Bits, R, G, B, 100 - Fade, Fade );
        end
        else if Frame < 3 then
        begin
          BodyRotResource.RLE.DrawColorize( Facing + Frame - 1, 0, 0, Bits, R, G, B, Fade, 100 - Fade );
          BodyRotResource.RLE.DrawColorize( Facing + Frame, 0, 0, Bits, R, G, B, 100 - Fade, Fade );
        end
        else
        begin
          BodyRotResource.RLE.DrawColorize( Facing + Frame - 1, 0, 0, Bits, R, G, B, Fade, 100 - Fade );
        end;
      end
      else if Frame >= 0 then
      begin
        BodyRotResource.RLE.DrawColorize( Facing + Frame, 0, 0, Bits, R, G, B, 100, 0 );
      end;
    end
    else
    begin
      if Fade > 0 then
      begin
        if Frame = 0 then
        begin
          BodyRotResource.RLE.DrawBlend( Facing + Frame, 0, 0, Bits, 100 - Fade, Fade );
        end
        else if Frame < 3 then
        begin
          BodyRotResource.RLE.DrawBlend( Facing + Frame - 1, 0, 0, Bits, Fade, 100 - Fade );
          BodyRotResource.RLE.DrawBlend( Facing + Frame, 0, 0, Bits, 100 - Fade, Fade );
        end
        else
        begin
          BodyRotResource.RLE.DrawBlend( Facing + Frame - 1, 0, 0, Bits, Fade, 100 - Fade );
        end;
      end
      else if Frame >= 0 then
      begin
        BodyRotResource.RLE.Draw( Facing + Frame, 0, 0, Bits );
      end;
    end;
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
      OldAI := FCharacter.AI;
      OldAIMode := FCharacter.FAIMode;
      FCharacter.AI := TMeander.create;
      FCharacter.AI.Character := FCharacter;
      FCharacter.AI.Init;
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
    p^.Angle := random * c2PI;
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
    FCharacter.AI.free;
    FCharacter.AI := OldAI;
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
    p^.Angle := random * c2PI;
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
begin
  result := false;
  Direction := Character.GetFacing( FCharacter.X, FCharacter.Y, Source.X, Source.Y );

  Effect := TEffect.Create;
  Effect.Resource := HitResource;
  Effect.AnimationDuration := 8 * Resource.FrameMultiplier;
  Effect.ColorR := ColorR;
  Effect.ColorG := ColorG;
  Effect.ColorB := ColorB;
  Effect.ApplyColor := true;
  Effect.DoAction( 'Default', Direction );

  FCharacter.AddEffect( Effect );
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

        Effect := TEffect.Create;
        Effect.Resource := HitResource;
        Effect.AnimationDuration := 8 * Resource.FrameMultiplier;
        Effect.DoAction( 'Default', Direction );

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

end.
