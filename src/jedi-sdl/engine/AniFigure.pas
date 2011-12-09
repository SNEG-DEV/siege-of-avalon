unit AniFigure;

interface

uses
  Classes,
  SiegeTypes,
  sdl,
  AniView,
  Anigrp30,
  CustomAniFigure;

type
  TAniFigure = class;
  
  TCollideFigureEvent = procedure( Source, Target : TAniFigure; var Stop : Boolean ) of object;

  TCollideItemEvent = procedure( Source : TAniFigure; var Stop : Boolean ) of object;

  TCollideBoundaryEvent = procedure( Source : TAniFigure ) of object;

  TTriggerEvent = procedure( Source : TAniFigure; ID, PrevID : SmallInt ) of object;

  TPathEvent = procedure( Sender : TAniFigure; X, Y : Longint ) of object;

  TAniFigure = class( TCustomAniFigure )
  private
    PlayMode : TScriptMode;
    MultiplierDelta : Smallint;
    MultiplierDeltaFrame : Word;
    FFrame : Word;
    FScriptFrame : Word;
    FView : TAniView;
    FScriptIndex : Integer;
    FEnabled : Boolean;
    FStartX : longint;
    FStartY : longint;
    FStartZ : longint;
    FPathDestX : Longint;
    FPathDestY : Longint;
    FDistance : Double;
    FPosX : Longint;
    FPosY : Longint;

    ScriptTerminated : boolean;
    FResource : TAniResource;
    FOnScreen : boolean;
    FLightIndex : single;
    function GetLightIndex : single;
    procedure SetEnabled( const Value : Boolean );
    procedure SetFrame( const Value : Word );
  public

    LightComputed : LongWord;
    Tile : PGridInfo;
    Zone : Byte;
    NextInTile : TAniFigure;
    MapOffset : Longint;
    PrevX : longint;
    PrevY : longint;
    PrevZ : longint;
    StepX : Double;
    StepY : Double;
    StepZ : Double;
    SlopeX : Double;
    SlopeY : Double;
    SlopeZ : Double;
    Terminal : Boolean;
    PathStep : Word;
    DestX : Longint;
    DestY : Longint;
    DestZ : Longint;
    PathHandle : HGLOBAL;
    PathCount : Word;
    PathDeviance : integer;
    AvoidInPath : TList;
    GotPath : Boolean;
    NextOnRow : TAniFigure;
    ViewEnabled : Boolean;
    FZ : Longint;
    FY : Longint;
    FX : Longint;
    Moving : Boolean;
    Moved : Boolean;
    NeedPath : Boolean;
    LightR : Integer;
    LightG : Integer;
    LightB : Integer;
    HighlightColor : TSDL_Color;
    Width : Longint;
    Height : Longint;
    CenterX : Longint;
    CenterY : Longint;
    Radius : Longint;
    Speed : Single;
    FrameMultiplier : Word;
    Delay : Longint;
    Highlightable : Boolean;
    Highlighted : Boolean;
    UseLineOfSight : Boolean;
    UseLighting : Boolean;
    AutoTransparent : Boolean;
    Visible : Boolean;
    MouseRect : TRect;
    OnClick : TNotifyEvent;
    OnCollideFigure : TCollideFigureEvent;
    OnCollideItem : TCollideItemEvent;
    OnCollideBoundary : TCollideBoundaryEvent;
    OnMove : TNotifyEvent;
    OnStop : TNotifyEvent;
    OnScriptEnd : TNotifyEvent;
    OnTrigger : TTriggerEvent;
    OnFilter : TTriggerEvent;
    OnPathStep : TPathEvent;
    OnNoPath : TNotifyEvent;
    constructor Create( X, Y, Z : Longint; Frame : Word; Enabled : Boolean );
    destructor Destroy; override;
    procedure UpdateScript;
    procedure Stop;
    procedure Move( X, Y, Z : Longint );
    procedure MoveTo( X, Y, Z : Longint );
    procedure SetPos( X, Y, Z : Longint );
    procedure FindPathTo( X, Y : Longint; Avoid : TList; Deviance : integer );
    procedure PlayScript( Name : string; StartIndex : Word; PlayMode : TScriptMode ); overload;
    procedure PlayScript( Name : string; StartIndex : Word; PlayMode : TScriptMode; Multiplier, DeltaFrame : Word; Delta : SmallInt ); overload;
    procedure ForceFrame( const Value : Word );
    property ScriptFrame : Word read FScriptFrame;
    property X : Longint read FX write FX;
    property Y : Longint read FY write FY;
    property Z : Longint read FZ write FZ;
    property StartX : Longint read FStartX write FStartX;
    property StartY : Longint read FStartY write FStartY;
    property StartZ : Longint read FStartZ write FStartZ;
    property PosX : Longint read FPosX write FPosX;
    property PosY : Longint read FPosY write FPosY;
    property Frame : Word read FFrame write SetFrame;
    property ScriptIndex : Integer read FScriptIndex;
    property Enabled : Boolean read FEnabled write SetEnabled;
    property PathDestX : Longint read FPathDestX;
    property PathDestY : Longint read FPathDestY;
    property Distance : Double read FDistance write FDistance;
    property View : TAniView read FView write FView;
    property LightIndex : single read GetLightIndex write FLightIndex;
    property OnScreen : boolean read FOnScreen write FOnScreen;
  end;

implementation

{ TAniFigure }

constructor TAniFigure.Create( X, Y,
  Z : Integer; Frame : Word; Enabled : Boolean );
begin
  inherited Create;
  FX := X;
  FY := Y;
  FZ := Z;
  StepX := X;
  StepY := Y;
  StepZ := Z;
  PrevX := X;
  PrevY := Y;
  PrevZ := Z;
  Moving := False;
  Terminal := False;
  Highlighted := False;
  Visible := True;
  FFrame := Frame;
  FScriptIndex := -1;
  FScriptFrame := 0;
  FEnabled := Enabled;
  ViewEnabled := Enabled;
  NextInTile := nil;
  NextOnRow := nil;
  Moved := True;
  MapOffset := -1;
end;

destructor TAniFigure.Destroy;
begin
  if ( PathHandle <> 0 ) then
  begin
    //GlobalFree( PathHandle );
    PathHandle := 0;
  end;
  inherited Destroy;
end;

function TAniFigure.GetLightIndex : single;
begin
  if LightComputed <> FView.FrameCount then
  begin
    FView.ComputeLight( self );
  end;
  result := FLightIndex;
end;

procedure TAniFigure.FindPathTo( X, Y : Integer; Avoid : TList; Deviance : integer );
begin
  FStartX := FX;
  FStartY := FY;
  FStartZ := FZ;
  Moving := False;
  Terminal := False;
  NeedPath := True;
  GotPath := False;
  if ( PathHandle <> 0 ) then
  begin
    //GlobalFree( PathHandle );
    PathHandle := 0;
  end;
  FPathDestX := X;
  FPathDestY := Y;
  SlopeZ := 0;
  AvoidInPath := Avoid;
  PathDeviance := Deviance;
end;

procedure TAniFigure.Move( X, Y, Z : Integer );
var
  D, Dx, dy, dZ : Double;
begin
  FStartX := FX;
  FStartY := FY;
  FStartZ := FZ;
  Moving := True;
  Terminal := False;
  NeedPath := False;
  GotPath := False;
  if ( PathHandle <> 0 ) then
  begin
    //GlobalFree( PathHandle );
    PathHandle := 0;
  end;
  DestX := X;
  DestY := Y;
  DestZ := Z;
  Dx := DestX - FX;
  dy := 2 * ( DestY - FY );
  dZ := DestZ - FZ;
  D := sqrt( sqr( Dx ) + sqr( dy ) + sqr( dZ ) );
  if D <> 0 then
  begin
    SlopeX := Dx / D;
    SlopeY := dy / ( 2 * D );
    SlopeZ := dZ / D;
  end;
end;

procedure TAniFigure.MoveTo( X, Y, Z : Integer );
var
  D, Dx, dy, dZ : Double;
begin
  FStartX := FX;
  FStartY := FY;
  FStartZ := FZ;
  Moving := True;
  Terminal := True;
  NeedPath := False;
  GotPath := False;
  if ( PathHandle <> 0 ) then
  begin
    //GlobalFree( PathHandle );
    PathHandle := 0;
  end;
  DestX := X;
  DestY := Y;
  DestZ := Z;
  Dx := DestX - FX;
  dy := 2 * ( DestY - FY );
  dZ := DestZ - FZ;
  D := sqrt( sqr( Dx ) + sqr( dy ) + sqr( dZ ) );
  if D <> 0 then
  begin
    SlopeX := Dx / D;
    SlopeY := dy / ( 2 * D );
    SlopeZ := dZ / D;
  end;
end;

procedure TAniFigure.PlayScript( Name : string; StartIndex : Word;
  PlayMode : TScriptMode );
var
  i : Integer;
begin
  if ( Name = '' ) then
    i := -1
  else
  begin
    i := FResource.Scripts.IndexOf( Name );
    if i >= 0 then
    begin
      if ( FrameMultiplier <> TScript( FResource.Scripts.Objects[ i ] ).Multiplier ) or ( i <> FScriptIndex ) then
      begin
        Delay := -1;
        FrameMultiplier := TScript( FResource.Scripts.Objects[ i ] ).Multiplier;
      end;
    end;
  end;
  ScriptTerminated := false;
  FScriptIndex := i;
  FScriptFrame := StartIndex;
  Self.PlayMode := PlayMode;
  MultiplierDeltaFrame := 0;
  MultiplierDelta := 0;
end;

procedure TAniFigure.PlayScript( Name : string; StartIndex : Word; PlayMode : TScriptMode; Multiplier, DeltaFrame : Word; Delta : SmallInt );
var
  i : Integer;
begin
  if ( Name = '' ) then
    i := -1
  else
  begin
    i := FResource.Scripts.IndexOf( Name );
    if i >= 0 then
    begin
      if ( FrameMultiplier <> Multiplier ) or ( i <> FScriptIndex ) then
      begin
        Delay := -1;
        FrameMultiplier := Multiplier;
        if ( StartIndex = DeltaFrame ) then
          inc( FrameMultiplier, Delta );
      end;
    end;
  end;
  ScriptTerminated := false;
  FScriptIndex := i;
  FScriptFrame := StartIndex;
  Self.PlayMode := PlayMode;
  MultiplierDeltaFrame := DeltaFrame;
  MultiplierDelta := Delta;
end;

procedure TAniFigure.SetEnabled( const Value : Boolean );
begin
  FEnabled := Value;
  if ( FEnabled ) then
    ViewEnabled := FEnabled;
end;

procedure TAniFigure.SetFrame( const Value : Word );
begin
  FFrame := Value;
  FScriptIndex := -1;
  FScriptFrame := 0;
  PlayMode := smOnce;
  Delay := 0;
end;

procedure TAniFigure.ForceFrame( const Value : Word );
begin
  FFrame := Value;
end;

procedure TAniFigure.SetPos( X, Y, Z : Integer );
begin
  FStartX := FX;
  FStartY := FY;
  FStartZ := FZ;
  Moving := False;
  Terminal := False;
  Moved := True;
  NeedPath := False;
  GotPath := False;
  if ( PathHandle <> 0 ) then
  begin
    //GlobalFree( PathHandle );
    PathHandle := 0;
  end;
  PrevX := FX;
  PrevY := FY;
  PrevZ := FZ;
  FX := X;
  FY := Y;
  FZ := Z;
  StepX := X;
  StepY := Y;
  StepZ := Z;
end;

procedure TAniFigure.Stop;
begin
  Moving := False;
  Terminal := False;
  NeedPath := False;
  GotPath := False;
  if ( PathHandle <> 0 ) then
  begin
    //GlobalFree( PathHandle );
    PathHandle := 0;
  end;
end;

procedure TAniFigure.UpdateScript;
begin
  if ScriptTerminated then
    exit;
  if ( FScriptIndex >= 0 ) then
  begin
    Inc( Delay );
    if Delay = 0 then
    begin
      FFrame := TScript( FResource.Scripts.Objects[ FScriptIndex ] ).FrameID[ FScriptFrame ];
    end
    else if ( Delay >= FrameMultiplier ) then
    begin
      Delay := 0;
      if ( PlayMode = smRandom ) then
      begin
        FScriptFrame := Random( TScript( FResource.Scripts.Objects[ FScriptIndex ] ).Frames ) + 1;
        FFrame := TScript( FResource.Scripts.Objects[ FScriptIndex ] ).FrameID[ FScriptFrame ];
      end
      else
      begin
        if ( FScriptFrame < TScript( FResource.Scripts.Objects[ FScriptIndex ] ).Frames ) then
        begin
          Inc( FScriptFrame );
          FFrame := TScript( FResource.Scripts.Objects[ FScriptIndex ] ).FrameID[ FScriptFrame ];
          if FScriptFrame = MultiplierDeltaFrame then
          begin
            inc( FrameMultiplier, MultiplierDelta );
          end;
        end
        else
        begin
          if ( PlayMode = smRepeat ) then
          begin
            FScriptFrame := 1;
            FFrame := TScript( FResource.Scripts.Objects[ FScriptIndex ] ).FrameID[ 1 ];
          end
          else
          begin
            FFrame := TScript( FResource.Scripts.Objects[ FScriptIndex ] ).FrameID[ TScript( FResource.Scripts.Objects[ FScriptIndex ] ).Frames ];
            ScriptTerminated := true;
            if Assigned( OnScriptEnd ) then
              OnScriptEnd( Self );
          end;
        end;
      end;
    end;
  end;
end;

end.

