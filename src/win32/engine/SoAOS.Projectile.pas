unit SoAOS.Projectile;
(*
  Siege Of Avalon : Open Source Edition

  Portions created by Steffen Nyeland are
  Copyright (C) 2020 - Steffen Nyeland.

  Contributor(s):
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

  Description: SoAOS Projectile classes - was part of Character.pas

  Requires: Delphi 10.3.3 or later

  Revision History:
  - May 2020 - SN: Initial Commit to Git
  see git repo afterwards

*)

interface

uses
  Winapi.Windows,
  System.Classes,
  System.SysUtils,
  System.UITypes,
  Vcl.Graphics,
  SoAOS.Animation,
  Character,
  Resource,
  LogFile,
  DFX,
  Engine;

type
  TGlow = class;

  TProjectile = class( TSpriteObject )
  protected
    ZFix : Integer;
    Exploding : Boolean;
    procedure DoDamage( Target : TSpriteObject ); virtual;
    procedure CollideFigure( Source, Target : TAniFigure; var Stop : Boolean ); virtual;
    procedure CollideItem( Source : TAniFigure; var Stop : Boolean ); virtual;
    procedure CollideBoundary( Source : TAniFigure ); virtual;
    procedure DoFrame; override;
    procedure ExplodeEnd( Sender : TObject ); virtual;
    procedure Disable; virtual;
  public
    DamageFactor : single;
    Critical : boolean;
    FSource : TCharacter;
    FTarget : TSpriteObject;
    Damage : TDamageProfile;
    HitTarget : single;
    HitIncidental : single;
    TrackingDegree : Single;
    DamageRadius : Integer;
    Duration : LongWord;
    UseStealth : boolean;
    GlowEffect : TGlow;
    TrailedBy : TProjectile;
    Passive : boolean;
    StrikeLeatherSound : SmallInt;
    StrikeMetalSound : SmallInt;
    StrikeWallSound : SmallInt;
    Magic : integer; //How magical is this projectile
    constructor Create( X, Y, Z : Longint; Frame : Word; Enabled : Boolean ); override;
    procedure Launch( Source : TCharacter; Target : TSpriteObject; X, Y : Longint ); virtual;
    procedure MoveEvent( Sender : TObject ); virtual;
  end;

  TGlow = class( TProjectile )
  protected
    procedure Render; override;
    procedure DoDamage( Target : TSpriteObject ); override;
    procedure CollideFigure( Source, Target : TAniFigure; var Stop : Boolean ); override;
    procedure CollideItem( Source : TAniFigure; var Stop : Boolean ); override;
    procedure CollideBoundary( Source : TAniFigure ); override;
    procedure DoFrame; override;
    procedure ExplodeEnd( Sender : TObject ); override;
    procedure Disable; override;
  public
    RFactor : integer;
    GFactor : integer;
    BFactor : integer;
    constructor Create( X, Y, Z : Longint; Frame : Word; Enabled : Boolean ); override;
    procedure MoveEvent( Sender : TObject ); override;
  end;

  TArrow = class( TProjectile )
  private
    PrevSlopeX : Double;
    PrevSlopeY : Double;
  protected
    procedure Render; override;
    procedure Disable; override;
  public
    FletchingColor : TColor;
    BM : TBitmap;
    RLE : TRLESprite;
    procedure Draw( X1, Y1, X2, Y2, R : Integer; SinT, CosT : Single ); virtual;
    destructor Destroy; override;
    procedure Launch( Source : TCharacter; Target : TSpriteObject; X, Y : Longint ); override;
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

implementation

uses
  SoAOS.Map,
  SoAOS.Graphics.Types,
  DirectX;

{ TProjectile }

procedure TProjectile.CollideFigure( Source, Target : TAniFigure;
  var Stop : Boolean );
var
  Hit : boolean;
const
  FailName : string = 'TProjectile.CollideFigure';
begin
  Log.DebugLog( FailName );
  try

    if Target = FSource then
      Exit; //Don't allow projectiles to collide with their creator
    if Target is TCharacter then
    begin
      if not TCharacter( Target ).Dead then
      begin
        if not assigned( FTarget ) then
        begin
          if TCharacter( FSource ).IsAlly( TCharacter( Target ) ) then
            Hit := ( Random < HitIncidental )
          else
            Hit := ( Random < HitTarget );
        end
        else
        begin
          if ( Target = FTarget ) then
          begin
            Hit := ( Random < ( HitTarget + TrackingDegree / 15 ) );
            TrackingDegree := 0;
          end
          else
            Hit := ( Random < HitIncidental );
        end;
        if Hit then
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
          if ActionExists( 'Explode' ) then
          begin
            FTarget := TSpriteObject( Target );
            Exploding := True;
            OnScriptEnd := ExplodeEnd;
            DoAction( 'Explode' );
          end
          else
          begin
            DoDamage( TSpriteObject( Target ) );
            Disable;
          end;
        end;
      end;
    end
    else if Target is TDoor then
    begin
      if TDoor( Target ).Closed then
      begin
        PlaySingleSound( StrikeWallSound, X, Y );
        Stop := True;
        if ActionExists( 'Explode' ) then
        begin
          FTarget := nil;
          Exploding := True;
          OnScriptEnd := ExplodeEnd;
          DoAction( 'Explode' );
        end
        else
        begin
          DoDamage( nil );
          Disable;
        end;
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TProjectile.CollideItem( Source : TAniFigure;
  var Stop : Boolean );
const
  FailName : string = 'TProjectile.CollideItem';
begin
  Log.DebugLog( FailName );
  try

    Stop := True;
    PlaySingleSound( StrikeWallSound, X, Y );
    if ActionExists( 'Explode' ) then
    begin
{$IFDEF AILog}
      Log.Log( 'Projectile strikes wall' );
{$ENDIF}
      Exploding := True;
      FTarget := nil;
      OnScriptEnd := ExplodeEnd;
      DoAction( 'Explode' );
    end
    else
    begin
      Disable;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

constructor TProjectile.Create( X, Y, Z : Longint; Frame : Word; Enabled : Boolean );
begin
  inherited;
  DamageFactor := 1;
  GlowEffect := nil;
end;

procedure TProjectile.Launch( Source : TCharacter; Target : TSpriteObject; X, Y : Longint );
const
  FailName : string = 'TProjectile.Launch';
begin
  Log.DebugLog( FailName );
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
//  if Assigned(Target) then
//    Move(Target.X, Target.Y, Target.Z + ZFix)
//  else
    Move( X, Y, Z );

    FFAcing := GetFacing( Self.X, Self.Y, X, Y );
    if ActionExists( 'Cast' ) then
      DoAction( 'Cast' )
    else
      DoAction( 'Default' );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TProjectile.CollideBoundary( Source : TAniFigure );
const
  FailName : string = 'TProjectile.CollideBoundary';
begin
  Log.DebugLog( FailName );
  try

    Disable;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TProjectile.DoDamage( Target : TSpriteObject );
var
  Total, Stun : Single;
  List : TStringList;
  i : Integer;
  D, F : Single;
  NewDamage : TDamageProfile;
const
  FailName : string = 'TProjectile.DoDamage';
begin
  Log.DebugLog( FailName );
  try

    if DamageRadius > 0 then
    begin
      List := GetCharactersInRadius( X, Y, Radius + DamageRadius );
      if Assigned( List ) then
      begin
        for i := 0 to List.Count - 1 do
        begin
          if List.Objects[ i ] <> FSource then
          begin
            NewDamage := Damage;
            if not TCharacter( List.Objects[ i ] ).AffectDamage( self, @NewDamage ) then
            begin
              D := TCharacter( List.Objects[ i ] ).Distance - TCharacter( List.Objects[ i ] ).Radius - Radius;
              if D < 0 then
                D := 0;
              F := ( 1 - ( D / DamageRadius ) ) * DamageFactor;
              if ( FTarget = Target ) and ( List.Objects[ i ] = Target ) then
                Total := CalcTotalDamage( NewDamage, TCharacter( TCharacter( List.Objects[ i ] ) ).Resistance, F, Critical )
              else
                Total := CalcTotalDamage( NewDamage, TCharacter( TCharacter( List.Objects[ i ] ) ).Resistance, F, false );
              if Total > 0 then
              begin
{$IFDEF AILog}
                Log.Log( TCharacter( List.Objects[ i ] ).GUID + ' takes ' + IntToStr( Round( Total ) ) + ' points of damage from a projectile' );
                if Assigned( FSource ) then
                  Log.Log( '  fired by ' + FSource.GUID );
{$ENDIF}
              end;
              Stun := CalcDamage( NewDamage.Stun ) * F - TCharacter( List.Objects[ i ] ).Resistance.Stun.Invulnerability;
              if Stun > 0 then
                Stun := Stun * ( 1 - TCharacter( List.Objects[ i ] ).Resistance.Stun.Resistance );
              TCharacter( List.Objects[ i ] ).TakeDamage( FSource, Total, Stun, UseStealth );
            end
          end;
        end;
        List.Free;
      end;
    end
    else
    begin
      if Assigned( Target ) then
      begin
        NewDamage := Damage;
        if TCharacter( Target ).AffectDamage( self, @NewDamage ) then
          exit;
        if ( FTarget = Target ) then
          Total := CalcTotalDamage( NewDamage, TCharacter( Target ).Resistance, DamageFactor, Critical )
        else
          Total := CalcTotalDamage( NewDamage, TCharacter( Target ).Resistance, DamageFactor, false );
        if Total > 0 then
        begin
{$IFDEF AILog}
          Log.Log( TCharacter( Target ).GUID + ' takes ' + IntToStr( Round( Total ) ) + ' points of damage from a projectile' );
          if Assigned( FSource ) then
            Log.Log( '  fired by ' + FSource.GUID );
{$ENDIF}
        end;
        Stun := CalcDamage( NewDamage.Stun ) - TCharacter( Target ).Resistance.Stun.Invulnerability;
        if Stun > 0 then
          Stun := Stun * ( 1 - TCharacter( Target ).Resistance.Stun.Resistance );
        TCharacter( Target ).TakeDamage( FSource, Total, Stun, UseStealth );
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TProjectile.ExplodeEnd( Sender : TObject );
const
  FailName : string = 'TProjectile.ExplodeEnd';
begin
  Log.DebugLog( FailName );
  try

    Disable;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TProjectile.Disable;
const
  FailName : string = 'TProjectile.Disable';
begin
  Log.DebugLog( FailName );
  try

    if assigned( GlowEffect ) then
    begin
      GlowEffect.enabled := false;
      GlowEffect := nil;
    end;

    if assigned( TrailedBy ) then
      TrailedBy.Disable; //This will cascade

    Enabled := False;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TProjectile.DoFrame;
var
  AX, Bx : Double;
  AY, By : Double;
  NewX, NewY : Longint;
  at, AT1, DT, T1, T2, T3 : Single;
  iX1, iY1, iZ1 : integer;
  iX2, iY2, iZ2 : integer;
  Tail : TProjectile;
const
  FailName : string = 'TProjectile.DoFrame';
begin
  Log.DebugLog( FailName );
  try

    if Exploding then
    begin
      if ( ScriptFrame = TProjectileResource( Resource ).ContactFrame ) and ( Delay = 0 ) then
      begin
        DoDamage( TSpriteObject( FTarget ) );
      end;
    end
    else if Passive then
    begin
    end
    else
    begin
      if Assigned( FTarget ) and ( TrackingDegree <> 0 ) then
      begin
        if ( FTarget is TCharacter ) and TCharacter( FTarget ).Dead then
          TrackingDegree := 0
        else
        begin
          AX := StepX;
          AY := StepY;
          Bx := FTarget.X;
          By := FTarget.Y;

          T1 := ATan( SlopeX, SlopeY );
          T2 := ATan( Bx - AX, By - AY );
          at := T2 - T1;
          if at > 0 then
          begin
            T3 := T1 + 2 * PI;
            AT1 := T2 - T3;
            if Abs( AT1 ) < Abs( at ) then
            begin
              at := AT1;
              T1 := T3;
            end;
          end
          else
          begin
            T3 := T2 + 2 * PI;
            AT1 := T3 - T1;
            if Abs( AT1 ) < Abs( at ) then
            begin
              at := AT1;
              T2 := T3;
            end;
          end;
          DT := PI * TrackingDegree / 180;
          if T2 >= T1 then
          begin
            if at > DT then
              at := DT;
            T1 := T1 + at;
          end
          else
          begin
            if at < -DT then
              at := -DT;
            T1 := T1 + at;
          end;

          NewX := Round( AX + 128 * Cos( T1 ) );
          NewY := Round( AY + 128 * Sin( T1 ) );
          Move( NewX, NewY, FTarget.Z + ZFix );
        end;
      end;

      Dec( Duration );
      if Duration <= 0 then
        Disable;
    end;

    if not Passive then
    begin
      if assigned( TrailedBy ) then
      begin
        iX1 := X;
        iY1 := Y;
        iZ1 := Z;
        Tail := TrailedBy;
        while assigned( Tail ) do
        begin
          iX2 := Tail.X;
          iY2 := Tail.Y;
          iZ2 := Tail.Z;
          Tail.SetPos( iX1, iY1, iZ1 );
          iX1 := iX2;
          iY1 := iY2;
          iZ1 := iZ2;
          Tail := Tail.TrailedBy;
        end;
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TProjectile.MoveEvent( Sender : TObject );
const
  FailName : string = 'TProjectile.MoveEvent';
begin
  Log.DebugLog( FailName );
  try

    if assigned( GlowEffect ) then
    begin
      if Exploding then
      begin
        GlowEffect.enabled := Z < 150;
        if GlowEffect.enabled then
        begin
          GlowEffect.SetPos( X, Y - 1, 0 );
          GlowEffect.Alpha := 150 - Z;
          View.TransFigure( GlowEffect );
        end;
      end
      else
      begin
        GlowEffect.enabled := Z < 100;
        if GlowEffect.enabled then
        begin
          GlowEffect.SetPos( X, Y - 1, 0 );
          GlowEffect.Alpha := 100 - Z;
          View.TransFigure( GlowEffect );
        end;
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TGlow }

procedure TGlow.CollideBoundary( Source : TAniFigure );
begin

end;

procedure TGlow.CollideFigure( Source, Target : TAniFigure;
  var Stop : Boolean );
begin

end;

procedure TGlow.CollideItem( Source : TAniFigure; var Stop : Boolean );
begin

end;

constructor TGlow.Create( X, Y, Z : Integer;
  Frame : Word; Enabled : Boolean );
const
  FailName : string = 'TGlow.Create';
begin
  Log.DebugLog( FailName );
  try

    inherited;
    Width := 111;
    Height := 60;
    CenterX := 55;
    CenterY := 30;
    Speed := 0;
    Radius := 0;
    UseLighting := false;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TGlow.Disable;
begin

end;

procedure TGlow.DoDamage( Target : TSpriteObject );
begin

end;

procedure TGlow.DoFrame;
begin

end;

procedure TGlow.ExplodeEnd( Sender : TObject );
begin

end;

procedure TGlow.MoveEvent( Sender : TObject );
begin

end;

procedure TGlow.Render;
var
  DstX1, DstY1 : Integer;
  ddsd : TDDSurfaceDesc;
  Bits : BITPLANE;
const
  FailName : string = 'TGlow.Render';
begin
  FillChar(ddsd, sizeof(ddsd), 0);
  Log.DebugLog( FailName );
  try

    DstX1 := View.Left + PosX;
    DstY1 := View.Top + PosY;

    ddsd.dwSize := SizeOf( ddsd );
    if lpDDSBack.Lock( nil, ddsd, DDLOCK_WAIT, 0 ) = DD_OK then
    begin
      try
        Bits.bitsPtr := ddsd.lpSurface;
        Bits.bitsWdh := ResWidth;
        Bits.bitsHgh := ResHeight;
        Bits.bitsFmt := dfx_pixelformat;
        Bits.bitsPitch := ddsd.lPitch;
        Bits.BaseX := -DstX1;
        Bits.BaseY := -DstY1;

        GlowImage.DrawColorize( 0, 0, 0, @Bits, RFactor, GFactor, BFactor, Alpha, 100 );
      finally
        lpDDSBack.Unlock( nil );
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TArrow }

procedure TArrow.Render;
var
  R : Integer;
  X1, Y1, X2, Y2 : Integer;
  DstX1, DstY1 : Integer;
  ddsd : TDDSurfaceDesc;
  Bits : BITPLANE;
  RFactor, GFactor, BFactor : integer;
const
  FailName : string = 'TArrow.Render';
begin
  FillChar(ddsd, sizeof(ddsd), 0);
  Log.DebugLog( FailName );
  try

    if not Visible then
      Exit;
    if TrackingDegree <> 0 then
    begin
      if ( SlopeX <> PrevSlopeX ) or ( SlopeY <> PrevSlopeY ) then
      begin
        PrevSlopeX := SlopeX;
        PrevSlopeY := SlopeY;
        R := ( width - 4 ) div 2;
        X2 := Round( R * SlopeX );
        Y2 := Round( R * SlopeY );
        X1 := Height - X2;
        Y1 := Height div 2 - Y2;
        Inc( X2, Height );
        Inc( Y2, Height div 2 );
        Draw( X1, Y1, X2, Y2, R, SlopeY, SlopeX );
      end;
    end;

    DstX1 := View.Left + PosX;
    DstY1 := View.Top + PosY;

    ddsd.dwSize := SizeOf( ddsd );
    if lpDDSBack.Lock( nil, ddsd, DDLOCK_WAIT, 0 ) = DD_OK then
    begin
      try
        Bits.bitsPtr := ddsd.lpSurface;
        Bits.bitsWdh := ResWidth;
        Bits.bitsHgh := ResHeight;
        Bits.bitsFmt := dfx_pixelformat;
        Bits.bitsPitch := ddsd.lPitch;
        Bits.BaseX := -DstX1;
        Bits.BaseY := -DstY1;

        RFactor := 100 * LightR div 255;
        GFactor := 100 * LightG div 255;
        BFactor := 100 * LightB div 255;
        RLE.DrawColorize( 0, 0, 0, @Bits, RFactor, GFactor, BFactor, 100, 0 );
      finally
        lpDDSBack.Unlock( nil );
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TArrow.Destroy;
begin
  RLE.Free;
  if Assigned( BM ) then
  begin
    BM.Free;
  end;
  inherited;
end;

procedure TArrow.Draw( X1, Y1, X2, Y2, R : Integer; SinT, CosT : Single );
var
  X0, Y0 : Integer;
const
  FailName : string = 'TArrow.Draw';
begin
  Log.DebugLog( FailName );
  try

    with BM.Canvas do
    begin
      // Beware below fix is temporary - due to issue with DDrawCompat ddraw.dll
//      Y1 := Height-Y1;
//      Y2 := Height-Y2;
      //

      PatBlt( Handle, 0, 0, BM.width, BM.Height, BLACKNESS );
      X0 := R - Round( ( R - 4 ) * CosT );
      // Beware below fix is temporary - due to issue with DDrawCompat ddraw.dll
//      Y0 := Height - ( R div 2 - Round( ( R - 4 ) * SinT ) );
      Y0 := R div 2 - Round( ( R - 4 ) * SinT );
      Pen.Color := FletchingColor;
      Pen.Width := 3;
      MoveTo( X0, Y0 );
      LineTo( X1, Y1 );
      Pen.Color := $204080;
      Pen.Width := 1;
      MoveTo( X1, Y1 );
      LineTo( X2, Y2 );
      Pen.Color := clSilver;
      X0 := Height + Round( ( R - 2 ) * CosT );
      // Beware below fix is temporary - due to issue with DDrawCompat ddraw.dll
//      Y0 := Height - ( Height div 2 + Round( ( R - 2 ) * SinT ) );
      Y0 := Height div 2 + Round( ( R - 2 ) * SinT );
      MoveTo( X0, Y0 );
      LineTo( X2, Y2 );
    end;
    RLE.LoadFromBitmap( BM, Width, Height, clBlack );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TArrow.Launch( Source : TCharacter; Target : TSpriteObject; X, Y : Longint );
const
  FailName : string = 'TArrow.Launch';
begin
  Log.DebugLog( FailName );
  try

    inherited;
    PrevSlopeX := SlopeX;
    PrevSlopeY := SlopeY;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TArrow.Disable;
const
  FailName : string = 'TArrow.Disable';
begin
  Log.DebugLog( FailName );
  try

    inherited;
    RLE.free;
    RLE := nil;
    BM.Free;
    BM := nil;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TLightningProjectile }

procedure TLightningProjectile.DoFrame;
const
  FailName : string = 'TLightningProjectile.DoFrame';
begin
  Log.DebugLog( FailName );
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
  Log.DebugLog( FailName );
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
  Log.DebugLog( FailName );
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
  Log.DebugLog( FailName );
  try

    Frame := random( 6 ) + 1;
    inherited;

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
