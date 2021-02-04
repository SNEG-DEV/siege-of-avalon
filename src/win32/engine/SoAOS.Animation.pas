unit SoAOS.Animation;
(*
  Siege Of Avalon : Open Source Edition

  Portions created by Digital Tome L.P. Texas USA are
  Copyright ©1999-2000 Digital Tome L.P. Texas USA
  All Rights Reserved.

  Portions created by Team SOAOS are
  Copyright (C) 2003 - Team SOAOS.

  Portions created by Steffen Nyeland are
  Copyright (C) 2020 - Steffen Nyeland.

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

  Description: Was part of AniDec30.pas, AniGrp30.pas and Characters.pas

  Requires: Delphi 10.3.3 or later

  Revision History:
  - 13 Jul 2003 - DL: Initial Upload to CVS
  - 10 Mar 2019 - SN: Forked on GitHub
  see git repo afterwards

*)

interface

uses
  Winapi.Windows,
  System.Types,
  System.Classes,
  System.Generics.Collections,
  Vcl.Graphics,
  Vcl.Controls,
  AStar,
  // Winapi.DirectDraw,
  DirectX,
  MMTimer,

  SoAOS.Types,
  SoAOS.Map;

type
  TAniFigure = class;
  TAniView = class;
  // TSubMap = class;

  TAniSpecialEffect = (seNone, seTranslucent, seSilhouette, seInvert, seSpooky,
    seFunky, seWhite, seNegative, seStrange, seAdd, seSubtract, seMultiply);

  TExtMouseEvent = procedure(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y, MapX, MapY: Integer) of object;

  TExtMouseMoveEvent = procedure(Sender: TObject; Shift: TShiftState;
    X, Y, MapX, MapY: Integer) of object;

  TCollideFigureEvent = procedure(Source, Target: TAniFigure; var Stop: Boolean)
    of object;

  TCollideItemEvent = procedure(Source: TAniFigure; var Stop: Boolean)
    of object;

  TCollideBoundaryEvent = procedure(Source: TAniFigure) of object;

  TTriggerEvent = procedure(Source: TAniFigure; ID, PrevID: SmallInt) of object;

  TPathEvent = procedure(Sender: TAniFigure; X, Y: Longint) of object;

  TScriptMode = (smOnce, smRepeat, smRandom);

  // TSubMap = class( TAniMap )
  // public
  // X1, Y1 : Longint;
  // X2, Y2 : Longint;
  // Visible : Boolean;
  // end;

  TScript = class(TObject)
  const
    MaxScriptFrames = 64;
  public
    Frames: Word;
    FrameID: array [1 .. MaxScriptFrames] of Word;
    Multiplier: Word;
    Tag: Longint;
  end;

  TAniResource = class(TObject)
  private
    function GetScript(const Name: string): TScript;
  protected
    Scripts: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Render(Figure: TAniFigure); virtual; abstract;
    procedure EnumLightSource(Figure: TAniFigure; Index, X, Y, Z: Longint;
      Intensity: double; Radius: Integer); virtual;
    // procedure Draw( Canvas : TCanvas; X, Y : Integer; Frame : Word ); virtual; abstract;
    procedure FreeResources; virtual; abstract;
    function AddScript(const Name: string; Script: TScript): Integer;
    property Script[const Name: string]: TScript read GetScript;
  end;

  TAniFigure = class(TObject)
  private
    NextOnRow: TAniFigure;
    NextInTile: TAniFigure;
    Moving: Boolean;
    Moved: Boolean;
    MapOffset: Longint;
    Terminal: Boolean;
    NeedPath: Boolean;
    GotPath: Boolean;
    PathHandle: HGLOBAL;
    PathCount: Word;
    PathStep: Word;
    PlayMode: TScriptMode;
    MultiplierDelta: SmallInt;
    MultiplierDeltaFrame: Word;
    AvoidInPath: TList<TAniFigure>;
    ViewEnabled: Boolean;
    Zone: Byte;
    FFrame: Word;
    FScriptFrame: Word;
    FView: TAniView;
    FScriptIndex: Integer;
    FEnabled: Boolean;
    FStartX: Longint;
    FStartY: Longint;
    FStartZ: Longint;
    FPathDestX: Longint;
    FPathDestY: Longint;
    FStepX: double;
    FStepY: double;
    FStepZ: double;
    FSlopeX: double;
    FSlopeY: double;
    FSlopeZ: double;
    FDestX: Longint;
    FDestY: Longint;
    FDestZ: Longint;
    FPrevX: Longint;
    FPrevY: Longint;
    FPrevZ: Longint;
    FDistance: double;
    FPosX: Longint;
    FPosY: Longint;
    FLightIndex: single;
    LightComputed: LongWord;
    ScriptTerminated: Boolean;
    FResource: TAniResource;
    FTile: PGridInfo;
    FOnScreen: Boolean;
    function GetLightIndex: single;
    procedure UpdateScript;
    procedure SetEnabled(const Value: Boolean);
    procedure SetFrame(const Value: Word);
  protected
    FZ: Longint;
    FY: Longint;
    FX: Longint;
    PathDeviance: Integer;
    procedure SetResource(const Value: TAniResource); virtual;
    procedure Render; virtual;
    procedure EnumLightSource(Index, X, Y, Z: Longint; Intensity: double;
      Radius: Integer); virtual;
    procedure DoFrame; virtual;
  public
    LightR: Integer;
    LightG: Integer;
    LightB: Integer;
    HighlightColor: TColor;
    Width: Longint;
    Height: Longint;
    CenterX: Longint;
    CenterY: Longint;
    Radius: Longint;
    Speed: single;
    FrameMultiplier: Word;
    Delay: Longint;
    Highlightable: Boolean;
    Highlighted: Boolean;
    UseLineOfSight: Boolean;
    UseLighting: Boolean;
    AutoTransparent: Boolean;
    Visible: Boolean;
    MouseRect: TRect;
    OnClick: TNotifyEvent;
    OnCollideFigure: TCollideFigureEvent;
    OnCollideItem: TCollideItemEvent;
    OnCollideBoundary: TCollideBoundaryEvent;
    OnMove: TNotifyEvent;
    OnStop: TNotifyEvent;
    OnScriptEnd: TNotifyEvent;
    OnTrigger: TTriggerEvent;
    OnFilter: TTriggerEvent;
    OnPathStep: TPathEvent;
    OnNoPath: TNotifyEvent;
    constructor Create(X, Y, Z: Longint; Frame: Word; Enabled: Boolean);
    destructor Destroy; override;
    procedure Stop;
    procedure Move(X, Y, Z: Longint);
    procedure MoveTo(X, Y, Z: Longint);
    procedure SetPos(X, Y, Z: Longint);
    procedure FindPathTo(X, Y: Longint; Avoid: TList<TAniFigure>;
      Deviance: Integer);
    procedure PlayScript(Name: string; StartIndex: Word;
      PlayMode: TScriptMode); overload;
    procedure PlayScript(Name: string; StartIndex: Word; PlayMode: TScriptMode;
      Multiplier, DeltaFrame: Word; Delta: SmallInt); overload;
    procedure ForceFrame(const Value: Word);
    property ScriptFrame: Word read FScriptFrame;
    property X: Longint read FX;
    property Y: Longint read FY;
    property Z: Longint read FZ;
    property StartX: Longint read FStartX;
    property StartY: Longint read FStartY;
    property StartZ: Longint read FStartZ;
    property StepX: double read FStepX;
    property StepY: double read FStepY;
    property StepZ: double read FStepZ;
    property SlopeX: double read FSlopeX;
    property SlopeY: double read FSlopeY;
    property SlopeZ: double read FSlopeZ;
    property DestX: Longint read FDestX;
    property DestY: Longint read FDestY;
    property DestZ: Longint read FDestZ;
    property PrevX: Longint read FPrevX;
    property PrevY: Longint read FPrevY;
    property PrevZ: Longint read FPrevZ;
    property PosX: Longint read FPosX;
    property PosY: Longint read FPosY;
    property Frame: Word read FFrame write SetFrame;
    property ScriptIndex: Integer read FScriptIndex;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property View: TAniView read FView;
    property PathDestX: Longint read FPathDestX;
    property PathDestY: Longint read FPathDestY;
    property Distance: double read FDistance;
    property Resource: TAniResource read FResource write SetResource;
    property LightIndex: single read GetLightIndex;
    property Tile: PGridInfo read FTile;
    property OnScreen: Boolean read FOnScreen;
  end;

  TGameObject = class(TAniFigure)
  private
    LoadCount: Integer;
  protected
    function GetProperty(const Name: string): string; virtual;
    procedure SetProperty(const Name: string; const Value: string); virtual;
  public
    // New - was private
    FProperties: TStringList;
    //
    Loading: Boolean;
    GUID: string;
    GroupName: string;
    OnLoad: string;
    CreatedFromLvlFile: Boolean;
    constructor Create(X, Y, Z: Longint; Frame: Word; Enabled: Boolean);
    destructor Destroy; override;
    function PropertyExists(const Name: string): Boolean;
    procedure LoadProperties(const List: TStringList); virtual;
    procedure SaveProperties(List: TStringList); virtual;
    procedure Init; virtual;
    procedure DoLoad;
    property Properties[const Name: string]: string read GetProperty
      write SetProperty;
  end;

  TSpriteObject = class(TGameObject)
  private
    ActivateCount: Integer;

    MsgDuration: Integer;
    MsgImage: IDirectDrawSurface;
    MsgWidth: Integer;
    MsgHeight: Integer;
    procedure SetFacing(const Value: TFacing);
  protected
    FFacing: TFacing;
    procedure SetResource(const Value: TAniResource); override;
    function GetProperty(const Name: string): string; override;
    procedure SetProperty(const Name: string; const Value: string); override;
    function GetName: string; virtual;
  public
    // New - was private
    CollideCount: Integer;
    //
    SpecialEffect: TAniSpecialEffect;
    MaskHeight: Integer;
    OnActivate: string;
    OnCollide: string;
    Alpha: Integer;
    UnMoveable: Boolean;
    ColorR, ColorG, ColorB: Integer;
    constructor Create(X, Y, Z: Longint; Frame: Word;
      Enabled: Boolean); virtual;
    destructor Destroy; override;
    function ActionExists(const Action: string): Boolean;
    function DoAction(const Action: string): Boolean; virtual;
    procedure Activate; virtual;
    procedure Say(const Msg: string; Color: TColor);
    procedure UpdateSay;
    procedure Init; override;
    procedure SaveProperties(List: TStringList); override;
    function ShouldSave: Boolean; virtual;
    property Facing: TFacing read FFacing write SetFacing;
    property Name: string read GetName;
  end;

  TAniView = class(TGraphicControl)
  const
    WorkWidth = 384;
    WorkHeight = 160;
  type
    MapColumnHeaderInfo = packed record
      BaseLine: Longint;
      Top: Longint;
      Active: Boolean;
      Reserved: Byte;
    end;

    RowUpdateInfo = packed record
      Figure: Pointer; // The first figure on the row
      OverlapRow: Longint;
      // The last row that contains an item which could overlap this row
      DescendRow: Longint;
      // The first row which has an item that descends below its position to this row
      MaxHeight: Longint; // The tallest item on this row
      ItemIndex: Word; // The first item on the row
    end;

  private
    MapX: Longint;
    MapY: Longint;
    MapOffsetX: Integer;
    MapOffsetY: Integer;
    MapWidth: Longint;
    MapHeight: Longint;
    MapBitWidth: Longint;
    MapBitHeight: Longint;
    MapColumns: HGLOBAL;
    FMapColumnCount: Longint;
    MapRows: HGLOBAL;
    PixelHeight: Longint;
    Timer: TAniTimer;
    FInterval: Word;
    FActive: Boolean;
    FMap: TAniMap;
    FFRameCount: LongWord;
    lpDDSMap: IDirectDrawSurface;
    Work: IDirectDrawSurface;
    XRayImage: IDirectDrawSurface;
    XRayWidth: Integer;
    XRayHeight: Integer;
    MaxHeightTop: Longint;
    MaxHeightBottom: Longint;
    FMaxCollisionRadius: Word;
    FItemMask: Longint;
    FFigureList: TList<TAniFigure>;
    FOnMouseDown: TExtMouseEvent;
    FOnMouseUp: TExtMouseEvent;
    FOnMouseMove: TExtMouseMoveEvent;
    FOnBeforeDisplay: TNotifyEvent;
    FOnAfterDisplay: TNotifyEvent;
    FOnWaitFortimer: TNotifyEvent;
    FAStar: TAStar;
    FAStarFigure: TAniFigure;
    FAstarAvoidFigure: TList<TAniFigure>;
    FStartX: Longint;
    FStartY: Longint;
    FDrawing: Boolean;
    FLMouseButton: Boolean;
    FShowRepaint: Boolean;
    RepaintCode: Longint;
    LastTickCount: LongWord;
    MapPreCreated: Boolean;
    procedure FDrawFrame(Sender: TObject);
    procedure SetMap(const VMap: TAniMap);
    procedure InitMap;
    procedure UpdateMap;
    procedure DrawTile(GridLoc: Pointer; i, j, Layer: Integer);
    procedure CopyTile(Dest: IDirectDrawSurface; GridLoc: Pointer;
      X, Y, Layer: Integer; ClipRect: PRect);
    procedure DrawItems;
    procedure DrawItemsClip(X1, X2, Y1, Y2: Longint);
    procedure SetInterval(PInterval: Word);
    procedure SetActive(VActive: Boolean);
    procedure SetItemMask(Mask: Longint);
    procedure SetShowRepaint(const Value: Boolean);
    procedure BuildRowUpdateInfo;
    procedure DrawFigure(Figure: TAniFigure);
    procedure FRefreshMap;
    procedure RefreshRegion(X, Y, W, H: Longint);
    procedure RefreshLight(Zone: TLightZone);
    function CanMove(SrcX, SrcY, DestX, DestY: SmallInt): Boolean;
    procedure GetPath(Figure: TAniFigure);
    procedure SetAutoTransparentMask(const Value: TBitmap);
    procedure ComputeLight(Figure: TAniFigure);
  protected
    procedure Show(Value: Boolean);
    procedure Paint; override;
  public
    TempDC: HDC; // Used for swapping bitmaps for blting
    OldTempBitmap: HBITMAP; // Original Bitmap in TempDC
    OffsetX: Longint;
    OffsetY: Longint;
    CenterX: Longint;
    CenterY: Longint;
    FRightX: Longint;
    FBottomY: Longint;
    KeyFigure: TAniFigure;
    MouseOverFigure: TAniFigure;
    MouseOverHLFigure: TAniFigure;
    MouseOverTile: PGridInfo;
    MousePosition: TPoint;
    ForceRefresh: Boolean;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure InitDX(Handle: HWND; ResW, ResH, BPP: Integer);
    procedure CloseDX;
    property Active: Boolean read FActive write SetActive;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CenterView(X, Y: Longint);
    procedure DrawFrame;
    procedure RefreshMap;
    procedure FreeResources;
    procedure AddFigure(Figure: TAniFigure);
    procedure ReplaceFigure(i: Integer; Figure: TAniFigure);
    procedure MoveFigure(Figure: TAniFigure);
    procedure TransFigure(Figure: TAniFigure);
    procedure WaitForNextFrame;
    procedure PrecreateMap(W, H: Longint);
    procedure UncreateMap;
    function FindPath(Figure: TAniFigure; X2, Y2, Deviance: Longint;
      var Path: HGLOBAL): Integer;
    function FindInRadius(X, Y: Longint; Radius: single): TList;
    function LineOfSight(X1, Y1, X2, Y2: Longint): Boolean;
    function LineOfCollision(X1, Y1, X2, Y2: Longint): Boolean;
    function ClearShot(SrcX, SrcY, DstX, DstY, Radius: Longint;
      UseLineOfSight: Boolean): Boolean;
    procedure DisableFigure(Figure: TAniFigure);
    property Map: TAniMap read FMap write SetMap;
    property Canvas;
    property ItemMask: Longint read FItemMask write SetItemMask;
    property AutoTransparentMask: TBitmap write SetAutoTransparentMask;
    property Drawing: Boolean read FDrawing;
    property FRameCount: LongWord read FFRameCount;
    property RightX: Longint read FRightX;
    property BottomY: Longint read FBottomY;
  published
    property Enabled;
    property Align;
    property ShowHint;
    property Visible;
    property Interval: Word read FInterval write SetInterval default 50;
    property FigureList: TList<TAniFigure> read FFigureList;
    property OnMouseDown: TExtMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp: TExtMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseMove: TExtMouseMoveEvent read FOnMouseMove
      write FOnMouseMove;
    property OnClick;
    property OnBeforeDisplay: TNotifyEvent read FOnBeforeDisplay
      write FOnBeforeDisplay;
    property OnAfterDisplay: TNotifyEvent read FOnAfterDisplay
      write FOnAfterDisplay;
    property LMouseButton: Boolean read FLMouseButton write FLMouseButton;
    property OnWaitForTimer: TNotifyEvent read FOnWaitFortimer
      write FOnWaitFortimer;
    property ShowRepaint: Boolean read FShowRepaint write SetShowRepaint;
  end;

procedure Register;

procedure Clip(ClipX1, ClipX2: Integer; var DestX1, DestX2, SrcX1,
  SrcX2: Integer);
procedure Clip1(ClipX1, ClipX2: Integer; var DestX1, SrcX1, SrcX2: Integer);
procedure Clip2(ClipX1, ClipX2: Integer; var DestX1, SrcX1, W: Integer);

var
  lpDD: IDirectDraw;
  lpDDSFront: IDirectDrawSurface;
  lpDDSBack: IDirectDrawSurface;
  ResWidth: Integer;
  ResHeight: Integer;
  DXMode: Boolean;
  PixelFormat: TPixelFormat;
  Debug: Longint;

implementation

uses
  System.SysUtils,
  System.IniFiles,
  Vcl.Forms,
  DXUtil,
  DXEffects,
  SoAOS.Graphics.Draw,
  AniDemo,
  Engine,
  Resource,
  Character,
  LogFile;

procedure Clip(ClipX1, ClipX2: Integer; var DestX1, DestX2, SrcX1,
  SrcX2: Integer);
begin
  if (DestX1 < ClipX1) then
  begin
    Inc(SrcX1, ClipX1 - DestX1);
    DestX1 := ClipX1;
  end;
  if (DestX2 > ClipX2) then
  begin
    Dec(SrcX2, DestX2 - ClipX2);
    DestX2 := ClipX2;
  end;
end;

procedure Clip1(ClipX1, ClipX2: Integer; var DestX1, SrcX1, SrcX2: Integer);
begin
  if (DestX1 < ClipX1) then
  begin
    Inc(SrcX1, ClipX1 - DestX1);
    DestX1 := ClipX1;
  end;
  if (DestX1 + (SrcX2 - SrcX1) > ClipX2) then
  begin
    SrcX2 := SrcX1 + ClipX2 - DestX1;
  end;
end;

procedure Clip2(ClipX1, ClipX2: Integer; var DestX1, SrcX1, W: Integer);
begin
  if (DestX1 < ClipX1) then
  begin
    Dec(W, ClipX1 - DestX1);
    Inc(SrcX1, ClipX1 - DestX1);
    DestX1 := ClipX1;
  end;
  if (DestX1 + W > ClipX2) then
  begin
    W := ClipX2 - DestX1;
  end;
end;

{ AniView }

constructor TAniView.Create(AOwner: TComponent);
var
  ScreenDC: HDC;
begin
  inherited Create(AOwner);

  FInterval := 50;
  ShowRepaint := false;

  if not(csDesigning in ComponentState) then
  begin
    // TempDC
    ScreenDC := GetDC(0);
    TempDC := CreateCompatibleDC(ScreenDC);
    OldTempBitmap := SelectObject(TempDC, CreateCompatibleBitmap(TempDC, 1, 1));
    DeleteObject(SelectObject(TempDC, OldTempBitmap));
    ReleaseDC(0, ScreenDC);

    // Initailize A* object
    FAStar := TAStar.Create;
    FAStar.CanMove := CanMove;

    FFigureList := TList<TAniFigure>.Create;
  end;
end;

destructor TAniView.Destroy;
begin
  if not(csDesigning in ComponentState) then
  begin
    FigureList.Free;
    if Assigned(Timer) then
      Timer.Destroy;
    if (MapColumns <> 0) then
      GlobalFree(MapColumns);
    MapColumns := 0;
    if (MapRows <> 0) then
      GlobalFree(MapRows);
    MapRows := 0;
    if (TempDC <> 0) then
      DeleteDC(TempDC);
    TempDC := 0;
    FAStar.Free;
    FAStar := nil;
  end;
  inherited Destroy;
end;

procedure TAniView.InitDX(Handle: HWND; ResW, ResH, BPP: Integer);
var
  ddsd: TDDSurfaceDesc;
  Caps: TDDSCaps;
  BltFx: TDDBLTFX;
  C: Longint;
  pr: TRect;
  tmpDD: IDirectDraw;
begin
  // Log.Log('InitDX');
  if DXMode then
    Exit;
  ResWidth := ResW;
  ResHeight := ResH;
  DirectDrawCreate(nil, tmpDD, nil); // Prepare for DirectX 7 or newer
  try
    tmpDD.QueryInterface(IID_IDirectDraw, lpDD);
  finally
    tmpDD := nil;
  end;
  lpDD.SetCooperativeLevel(Handle, DDSCL_EXCLUSIVE or DDSCL_FULLSCREEN);
  lpDD.SetDisplayMode(ResW, ResH, BPP);

  ddsd.dwSize := SizeOf(ddsd);
  ddsd.dwFlags := DDSD_CAPS or DDSD_BACKBUFFERCOUNT;
  ddsd.dwBackBufferCount := 1;
  ddsd.ddsCaps.dwCaps := DDSCAPS_COMPLEX + DDSCAPS_FLIP +
    DDSCAPS_PRIMARYSURFACE;
  if (lpDD.CreateSurface(ddsd, lpDDSFront, nil) = DD_OK) then
  begin
    Caps.dwCaps := DDSCAPS_BACKBUFFER;
    lpDDSFront.GetAttachedSurface(Caps, lpDDSBack);
  end;
  BltFx.dwSize := SizeOf(BltFx);
  BltFx.dwFillColor := 0;
  pr := Rect(0, 0, ResWidth, ResHeight);
  lpDDSBack.Blt(@pr, nil, @pr, DDBLT_COLORFILL + DDBLT_WAIT, @BltFx);
  lpDDSFront.Flip(nil, DDFLIP_WAIT);
  BltFx.dwSize := SizeOf(BltFx);
  BltFx.dwFillColor := 0;
  pr := Rect(0, 0, ResWidth, ResHeight);
  lpDDSBack.Blt(@pr, nil, @pr, DDBLT_COLORFILL + DDBLT_WAIT, @BltFx);
  C := FindColorMatch($FFFFFF);
  if C = $FFFFFF then
    PixelFormat := pf888
  else if C > $7FFF then
    PixelFormat := pf565
  else
    PixelFormat := pf555;
  DXMode := True;
end;

procedure TAniView.CloseDX;
begin
  if not DXMode then
    Exit;
  DXMode := false;
  lpDD.RestoreDisplayMode;
  lpDDSBack := nil;
  lpDDSFront := nil;
  lpDD := nil;
end;

procedure TAniView.FreeResources;
var
  i: Integer;
begin
  if MapColumns <> 0 then
    GlobalFree(MapColumns);
  MapColumns := 0;
  if MapRows <> 0 then
    GlobalFree(MapRows);
  MapRows := 0;
  FMap := nil;
  KeyFigure := nil;
  if not MapPreCreated then
  begin
    lpDDSMap := nil;
    Work := nil;
    XRayImage := nil;
  end;

  for i := 0 to FigureList.Count - 1 do
  begin
    try
      FigureList[i].Free;
    except
      Log.Log('*** Error: object could not be freed');
    end;
  end;
  FigureList.Clear;
  MapX := 0;
  MapY := 0;
  MapOffsetX := 0;
  MapOffsetY := 0;
  MapWidth := 0;
  MapHeight := 0;
  MapBitWidth := 0;
  MapBitHeight := 0;
  FMapColumnCount := 0;
  FMaxCollisionRadius := 0;
  PixelHeight := 0;
  OffsetX := 0;
  OffsetY := 0;
  CenterX := 0;
  CenterY := 0;
  FRightX := Width;
  FBottomY := Height;

  MaxHeightTop := 0;
  MaxHeightBottom := 0;
end;

procedure TAniView.Paint;
begin
  inherited Paint;
  if csDesigning in ComponentState then
  begin
    PatBlt(Canvas.Handle, 0, 0, Width, Height, BLACKNESS);
  end;
end;

procedure TAniView.Show(Value: Boolean);
begin
  Invalidate;
end;

procedure TAniView.SetMap(const VMap: TAniMap);
begin
  FMap := VMap;
  InitMap;
end;

procedure TAniView.AddFigure(Figure: TAniFigure);
begin
  if (Figure = nil) then
    Exit;
  FigureList.Add(Figure);
  Figure.FView := Self;
  if (Figure.Radius > FMaxCollisionRadius) then
    FMaxCollisionRadius := Figure.Radius;
  if (Figure.Height - Figure.CenterY > MaxHeightTop) then
    MaxHeightTop := Figure.Height - Figure.CenterY;
  if (Figure.CenterY > MaxHeightBottom) then
    MaxHeightBottom := Figure.CenterY;
end;

procedure TAniView.ReplaceFigure(i: Integer; Figure: TAniFigure);
begin
  if (Figure = nil) then
    Exit;
  if Assigned(FigureList[i]) then
  begin
    if FigureList[i].Enabled then
      DisableFigure(FigureList[i]);
    FigureList[i].Free;
  end;
  FigureList[i] := Figure;
  Figure.FView := Self;
  if (Figure.Radius > FMaxCollisionRadius) then
    FMaxCollisionRadius := Figure.Radius;
  if (Figure.Height - Figure.CenterY > MaxHeightTop) then
    MaxHeightTop := Figure.Height - Figure.CenterY;
  if (Figure.CenterY > MaxHeightBottom) then
    MaxHeightBottom := Figure.CenterY;
end;

procedure TAniView.FDrawFrame(Sender: TObject);
begin
  if not FActive then
    Exit;
  Timer.OnTimer := nil;
  DrawFrame;
  Timer.OnTimer := FDrawFrame;
end;

procedure TAniView.WaitForNextFrame;
var
  NextTickCount: LongWord;
begin
  NextTickCount := LastTickCount + Interval;
  while GetTickCount < NextTickCount do
  begin
    if Assigned(OnWaitForTimer) then
      OnWaitForTimer(Self)
    else
      application.processmessages;
  end;
end;

procedure TAniView.DrawFrame;
var
  i, j: Longint;
  RefItem: PItemInstanceInfo;
  RowBase, RowData: ^RowUpdateInfo;
  ColumnBase, ColumnData: ^MapColumnHeaderInfo;
  NextFigure, LastFigure: TAniFigure;
  ItemIndex: Word;
  MaxRow, Y1, Y2: Longint;
  StripX, StripW: Integer;
  X, Y, T, W, H: Longint;
  ZoneItem: TZone;
  KeyX, KeyY: Longint;
  XRayActive: Boolean;
  XRayX1, XRayY1, XRayX2, XRayY2: Longint;
  XRayX1Fix, XRayX2Fix: Longint;
  InXrayZone: Boolean;
  ApplyXRay: Boolean;
  HidesCharacter: Boolean;
  GridLoc: ^GridInfo;
  Loc: Integer;
  DC: HDC;
  SrcX1, SrcY1, SrcX2, SrcY2: Integer;
  DstX1, DstY1, DstX2, DstY2: Integer;
  BltFx: TDDBLTFX;
  ddck: TDDCOLORKEY;
  pr, p2: TRect;
begin
  if FDrawing then
    Exit;
  LastTickCount := GetTickCount;
  FDrawing := True;

  // Calculate postion of key figure
  if Assigned(KeyFigure) then
  begin
    if KeyFigure.NeedPath then
      GetPath(KeyFigure);
    if KeyFigure.Moved then
      TransFigure(KeyFigure);
    if KeyFigure.Moving then
      MoveFigure(KeyFigure);
    CenterX := KeyFigure.FX;
    CenterY := KeyFigure.FY;
  end;
  OffsetX := CenterX - Width div 2;
  OffsetY := CenterY - Height div 2;
  W := FMap.BitWidth - Width - FMap.TileWidth;
  H := FMap.BitHeight - Height - FMap.TileHeight;
  if (OffsetX < FMap.TileWidth) then
    OffsetX := FMap.TileWidth;
  if (OffsetY < FMap.TileHeight) then
    OffsetY := FMap.TileHeight;
  if (OffsetX > W) then
    OffsetX := W;
  if (OffsetY > H) then
    OffsetY := H;
  FRightX := OffsetX + Width;
  FBottomY := OffsetY + Height;

  XRayActive := false;
  XRayX1 := 0;
  XRayY1 := 0;
  XRayY2 := 0;
  XRayX1Fix := 0;
  XRayX2Fix := 0;

  if Assigned(FMap) then
  begin
    RowBase := GlobalLock(MapRows);

    // Move map accordingly
    if ForceRefresh then
    begin
      RefreshMap;
      ForceRefresh := false;
    end
    else
      UpdateMap;

    // Copy map to frame buffer
    pr := Rect(MapOffsetX, MapOffsetY, MapOffsetX + Width, MapOffsetY + Height);
    lpDDSBack.BltFast(Left, Top, lpDDSMap, @pr, DDBLTFAST_NOCOLORKEY or
      DDBLTFAST_WAIT);

    // Add flicker lighting
    for i := 1 to FMap.Zones.Count - 1 do
    begin
      ZoneItem := FMap.Zones[i];
      if ZoneItem is TLightZone then
      begin
        if ZoneItem.FullRefresh then
        begin
          RefreshLight(TLightZone(ZoneItem));
        end;
      end;
    end;

    // Get mouse position
    // GetCursorPos( MousePosition );
    MousePosition := ScreenToClient(Mouse.CursorPos);
    MouseOverFigure := nil;
    MouseOverHLFigure := nil;
    MouseOverTile := nil;

    if (MousePosition.X >= Left) and (MousePosition.X < Left + Width) and
      (MousePosition.Y >= Top) and (MousePosition.Y < Top + Height) then
    begin
      X := (MousePosition.X + OffsetX) div FMap.TileWidth;
      Y := (MousePosition.Y + OffsetY) div FMap.TileHeight;
      if (X >= 0) and (Y >= 0) and (X < FMap.Width) and (Y < FMap.Height) then
      begin
        Loc := X + Y * FMap.Width;
        GridLoc := GlobalLock(FMap.MapData);
        Inc(GridLoc, Loc);
        MouseOverTile := Pointer(GridLoc);
        GlobalUnlock(FMap.MapData);
      end;
    end;

    // Calculate all figures and place on map update grid
    { for i := 0 to FigureList.Count - 1 do begin
      if (TAniFigure(FigureList.Items[i]).Enabled) then begin
      TAniFigure(FigureList.Items[i]).UpdateScript;

      if (FigureList.Items[i] <> KeyFigure) then
      if TAniFigure(FigureList.Items[i]).NeedPath then GetPath(TAniFigure(FigureList.Items[i]));
      if (FigureList.Items[i] <> KeyFigure) then
      if TAniFigure(FigureList.Items[i]).Moved then TransFigure(TAniFigure(FigureList.Items[i]));
      if (FigureList.Items[i] <> KeyFigure) then
      if TAniFigure(FigureList.Items[i]).Moving then MoveFigure(TAniFigure(FigureList.Items[i]));
      if assigned(TAniFigure(FigureList.Items[i]).OnMove) then TAniFigure(FigureList.Items[i]).OnMove(FigureList.Items[i]);

      X := TAniFigure(FigureList.Items[i]).FX - TAniFigure(FigureList.Items[i]).CenterX - OffsetX;
      W:=X+TAniFigure(FigureList.Items[i]).MouseRect.Right;
      inc(X,TAniFigure(FigureList.Items[i]).MouseRect.Left);
      Y := TAniFigure(FigureList.Items[i]).FY - TAniFigure(FigureList.Items[i]).CenterY - TAniFigure(FigureList.Items[i]).FZ - OffsetY;
      H := Y+TAniFigure(FigureList.Items[i]).MouseRect.Bottom;
      inc(Y,TAniFigure(FigureList.Items[i]).MouseRect.Top);
      if (MousePosition.X >= X) and (MousePosition.X < W) and (MousePosition.Y >= Y) and (MousePosition.Y < H) then begin
      if not Assigned(MouseOverFigure) then
      MouseOverFigure := TAniFigure(FigureList.Items[i])
      else if (TAniFigure(FigureList.Items[i]).FY > MouseOverFigure.FY) then
      MouseOverFigure := TAniFigure(FigureList.Items[i]);
      if (TAniFigure(FigureList.Items[i]).Highlightable) then begin
      if not Assigned(MouseOverHLFigure) then
      MouseOverHLFigure := TAniFigure(FigureList.Items[i])
      else if (TAniFigure(FigureList.Items[i]).FY > MouseOverHLFigure.FY) then
      MouseOverHLFigure := TAniFigure(FigureList.Items[i]);
      end;
      end;

      if (Y + H >= 0) or (Y < Height) then begin
      RowData := RowBase;
      if (TAniFigure(FigureList.Items[i]).FY > 0) then
      if (TAniFigure(FigureList.Items[i]).FY > PixelHeight) then
      Inc(RowData, PixelHeight)
      else
      Inc(RowData, TAniFigure(FigureList.Items[i]).FY);
      NextFigure := RowData^.Figure;
      RowData^.Figure := FigureList.Items[i];
      TAniFigure(FigureList.Items[i]).NextOnRow := NextFigure;
      end;

      end
      else begin
      if (TAniFigure(FigureList.Items[i]).ViewEnabled) then DisableFigure(TAniFigure(FigureList.Items[i]));
      end;
      end; }

    if Assigned(KeyFigure) and KeyFigure.AutoTransparent and Assigned(XRayImage)
    then
    begin
      XRayX1 := KeyFigure.FX - XRayWidth div 2;
      XRayY1 := KeyFigure.FY - KeyFigure.CenterY;
      XRayX2 := XRayX1 + XRayWidth;
      XRayY2 := XRayY1 + XRayHeight;
      XRayX1Fix := FMap.StripWidth * (XRayX1 div FMap.StripWidth);
      XRayX2Fix := FMap.StripWidth * (XRayX2 div FMap.StripWidth + 1);
      XRayActive := True;
      RefreshRegion(XRayX1, XRayY1, XRayWidth, XRayHeight);
    end;

    BltFx.dwSize := SizeOf(BltFx);
    BltFx.dwFillColor := FMap.ColorMatch;
    pr := Rect(WorkWidth div 2, 0, WorkWidth div 2 + XRayWidth +
      FMap.StripWidth, XRayHeight);
    p2 := Rect(320, 0, 320 + XRayWidth + FMap.StripWidth, XRayHeight);
    Work.Blt(@pr, nil, @p2, DDBLT_COLORFILL + DDBLT_WAIT, @BltFx);
    // Walk through grid and draw on frame buffer when necessary
    MaxRow := PixelHeight;
    Y := OffsetY + Height + MaxHeightBottom;
    if (Y < MaxRow) then
      MaxRow := Y;

    if (OffsetY > MaxHeightTop) then
      Y1 := OffsetY - MaxHeightTop
    else
      Y1 := 0;
    if Y1 < 0 then
      Y1 := 0;
    RowData := RowBase;
    Inc(RowData, OffsetY);

    Y := RowData^.DescendRow;
    if Y < Y1 then
      Y1 := Y;

    RowData := RowBase;
    Inc(RowData, Y1);
    ItemIndex := RowData^.ItemIndex;

    Y := OffsetY + Height;
    if Y > PixelHeight then
      Y := PixelHeight;
    RowData := RowBase;
    Inc(RowData, Y);
    Y2 := RowData^.OverlapRow;
    if MaxRow > Y2 then
      Y2 := MaxRow;

    ColumnBase := GlobalLock(MapColumns);
    ColumnData := ColumnBase;
    ZeroMemory(ColumnData, FMapColumnCount * SizeOf(MapColumnHeaderInfo));

    if (Y2 >= Y1) then
    begin
      RowData := RowBase;
      Inc(RowData, Y1);
      for i := Y1 to Y2 do
      begin
        RowData^.Figure := nil;
        Inc(RowData);
      end;

      for i := 0 to FigureList.Count - 1 do
      begin
        if FigureList[i].Enabled then
        begin
          FigureList[i].FOnScreen := false;
          FigureList[i].UpdateScript;
          if FigureList[i].ViewEnabled then
            FigureList[i].Moved := True;

          if (FigureList[i] <> KeyFigure) then
            if FigureList[i].NeedPath then
              GetPath(FigureList[i]);
          if (FigureList[i] <> KeyFigure) then
            if FigureList[i].Moved then
              TransFigure(FigureList[i]);
          if (FigureList[i] <> KeyFigure) then
            if FigureList[i].Moving then
              MoveFigure(FigureList[i]);
          if Assigned(FigureList[i].OnMove) then
            FigureList[i].OnMove(FigureList[i]);

          X := FigureList[i].FX - FigureList[i].CenterX - OffsetX;
          W := X + FigureList[i].MouseRect.Right;
          Inc(X, FigureList[i].MouseRect.Left);
          Y := FigureList[i].FY - FigureList[i].CenterY - FigureList[i].FZ
            - OffsetY;
          H := Y + FigureList[i].MouseRect.Bottom;
          Inc(Y, FigureList[i].MouseRect.Top);
          if (MousePosition.X >= X) and (MousePosition.X < W) and
            (MousePosition.Y >= Y) and (MousePosition.Y < H) then
          begin
            if not Assigned(MouseOverFigure) then
              MouseOverFigure := FigureList[i]
            else if (FigureList[i].FY > MouseOverFigure.FY) then
              MouseOverFigure := FigureList[i];
            if FigureList[i].Highlightable then
            begin
              if not Assigned(MouseOverHLFigure) then
                MouseOverHLFigure := FigureList[i]
              else if (FigureList[i].FY > MouseOverHLFigure.FY) then
                MouseOverHLFigure := FigureList[i];
            end;
          end;

          if (FigureList[i].FY >= Y1) and (FigureList[i].FY <= Y2) then
          begin
            RowData := RowBase;
            Inc(RowData, FigureList[i].FY);
            NextFigure := RowData^.Figure;
            RowData^.Figure := FigureList[i];
            FigureList[i].NextOnRow := NextFigure;
          end
          else
          begin
            FigureList[i].NextOnRow := nil;
          end;
        end
        else
        begin
          if FigureList[i].ViewEnabled then
            DisableFigure(FigureList[i]);
        end;
      end;

      RowData := RowBase;
      Inc(RowData, Y1);
      for i := Y1 to Y2 do
      begin
        if (ItemIndex > 0) then
        begin
          while (FMap.FItemList[ItemIndex].Y <= i) or (i = PixelHeight) do
          begin
            if FMap.FItemList[ItemIndex].Visible then
            begin
              if (FMap.FItemList[ItemIndex].FilterID = 0) or
                ((FMap.FItemList[ItemIndex].FilterID < 0) or
                (FMap.FItemList[ItemIndex].FilterID = FItemMask)) and
                (FMap.FItemList[ItemIndex].FilterID <> -FItemMask) then
              begin
                StripX := (FMap.FItemList[ItemIndex].X - MapX * FMap.TileWidth)
                  div FMap.StripWidth;
                ColumnData := ColumnBase;
                Y := FMap.FItemList[ItemIndex].Y - FMap.FItemList
                  [ItemIndex].VHeight;
                j := Y + FMap.FItemList[ItemIndex].VHeight;
                if (StripX >= 0) and (StripX < FMapColumnCount) then
                begin
                  InXrayZone := XRayActive and
                    (FMap.FItemList[ItemIndex].X >= XRayX1Fix) and
                    (FMap.FItemList[ItemIndex].X < XRayX2Fix) and (j > XRayY1)
                    and (Y <= XRayY2);
                  Inc(ColumnData, StripX);
                  HidesCharacter := ColumnData^.Active and
                    (Y <= ColumnData^.BaseLine);
                  if InXrayZone or HidesCharacter then
                  begin
                    X := FMap.FItemList[ItemIndex].X - OffsetX;
                    ApplyXRay := InXrayZone and FMap.FItemList[ItemIndex]
                      .AutoTransparent and
                      ((FMap.FItemList[ItemIndex].XRayID = 0) or
                      (FMap.FItemList[ItemIndex].XRayID = FItemMask));
                    if ApplyXRay then
                    begin
                      if FMap.FItemList[ItemIndex].VHeight = 0 then
                        ApplyXRay := false
                      else
                      begin
                        RefItem := @FMap.FItemList
                          [FMap.FItemList[ItemIndex].RefItem];
                        j := Round(RefItem.Slope0 * (KeyFigure.FX - RefItem.X))
                          + RefItem.Y;
                        ApplyXRay := (j > KeyFigure.FY);
                      end;
                    end;
                    Dec(Y, OffsetY);
                    ZoneItem := FMap.Zones[FMap.FItemList[ItemIndex].Zone];

                    SrcX1 := FMap.FItemList[ItemIndex].ImageX;
                    DstX1 := X;
                    SrcX2 := FMap.FItemList[ItemIndex].ImageX + FMap.FItemList
                      [ItemIndex].Width;
                    DstX2 := X + FMap.FItemList[ItemIndex].Width;
                    Clip(0, Width, DstX1, DstX2, SrcX1, SrcX2);

                    SrcY1 := FMap.FItemList[ItemIndex].ImageY;
                    DstY1 := Y;
                    SrcY2 := FMap.FItemList[ItemIndex].ImageY + FMap.FItemList
                      [ItemIndex].Height;
                    DstY2 := Y + FMap.FItemList[ItemIndex].Height;
                    Clip(0, Height, DstY1, DstY2, SrcY1, SrcY2);

                    if ZoneItem is TLightZone then
                    begin
                      if TLightZone(ZoneItem).Flicker <> flNone then
                      begin
                        H := (TLightZone(ZoneItem).States - TLightZone(ZoneItem)
                          .State) * TLightZone(ZoneItem).ItemStateOffset;
                        Dec(SrcX1, H);
                        Dec(SrcX2, H);
                      end;
                    end;
                    if ApplyXRay then
                    begin
                      if ColumnData^.Active then
                      begin
                        if (DstY2 + OffsetY > XRayY2) and
                          (ColumnData^.BaseLine > XRayY2) then
                        begin
                          Clip(ColumnData^.Top - OffsetY,
                            ColumnData^.BaseLine - OffsetY, DstY1, DstY2,
                            SrcY1, SrcY2);
                          pr := Rect(SrcX1, SrcY2 - (DstY2 + OffsetY - XRayY2),
                            SrcX2, SrcY2);
                          lpDDSBack.BltFast(Left + DstX1,
                            Top + XRayY2 - OffsetY, ZoneItem.FItemImages, @pr,
                            DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
                        end;
                        Clip(ColumnData^.Top - OffsetY, XRayY2, DstY1, DstY2,
                          SrcY1, SrcY2); // ????
                        H := XRayY1 - (DstY1 + OffsetY);
                        if (H > 0) then
                        begin
                          Y := SrcY2 - SrcY1;
                          if H > Y then
                            H := Y;
                          pr := Rect(SrcX1, SrcY1, SrcX2, SrcY1 + H);
                          lpDDSBack.BltFast(Left + DstX1, Top + DstY1,
                            ZoneItem.FItemImages, @pr, DDBLTFAST_SRCCOLORKEY or
                            DDBLTFAST_WAIT);
                        end;
                      end;
                      Clip(XRayY1 - OffsetY, XRayY2 - OffsetY, DstY1, DstY2,
                        SrcY1, SrcY2);
                      // Blt to work at 0,0 with no color key.
                      // Then blt the appropriate segment of Xray on to work with black color key.
                      // Then blt the result onto the back buffer using SRCCOLORKEY
                      pr := Rect(SrcX1, SrcY1, SrcX2, SrcY2);
                      Work.BltFast(0, 0, ZoneItem.FItemImages, @pr,
                        DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT);

                      X := DstX1 + OffsetX - XRayX1;
                      Y := DstY1 + OffsetY - XRayY1;
                      W := X + SrcX2 - SrcX1;
                      H := Y + SrcY2 - SrcY1;
                      j := 0;

                      if (W > XRayWidth) then
                        W := XRayWidth;
                      if (X < 0) then
                      begin
                        j := -X;
                        X := 0;
                      end;
                      pr := Rect(X, Y, W, H);
                      Work.BltFast(j, 0, XRayImage, @pr,
                        DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);

                      ddck.dwColorSpaceLowValue := FMap.ColorMatch;
                      ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
                      Work.SetColorKey(DDCKEY_SRCBLT, @ddck);
                      pr := Rect(0, 0, SrcX2 - SrcX1, SrcY2 - SrcY1);
                      lpDDSBack.BltFast(Left + DstX1, Top + DstY1, Work, @pr,
                        DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
                    end
                    else
                    begin
                      if ColumnData^.Active then
                      begin
                        if InXrayZone then
                        begin
                          if XRayY1 < ColumnData^.Top then
                            Y := XRayY1
                          else
                            Y := ColumnData^.Top;
                          if XRayY2 > ColumnData^.BaseLine then
                            H := XRayY2
                          else
                            H := ColumnData^.BaseLine;
                          Clip(Y - OffsetY, H - OffsetY, DstY1, DstY2,
                            SrcY1, SrcY2);
                        end
                        else
                          Clip(ColumnData^.Top - OffsetY,
                            ColumnData^.BaseLine - OffsetY, DstY1, DstY2,
                            SrcY1, SrcY2);
                      end
                      else
                        Clip(XRayY1 - OffsetY, XRayY2 - OffsetY, DstY1, DstY2,
                          SrcY1, SrcY2);
                      pr := Rect(SrcX1, SrcY1, SrcX2, SrcY2);
                      lpDDSBack.BltFast(Left + DstX1, Top + DstY1,
                        ZoneItem.FItemImages, @pr, RepaintCode); // ****
                    end;
                  end;
                end;
              end;
            end;
            ItemIndex := FMap.FItemList[ItemIndex].Next;
            if (ItemIndex = 0) then
              Break;
          end;
        end;
        if Assigned(RowData^.Figure) and (i <= MaxRow) then
        begin
          NextFigure := RowData^.Figure;
          RowData^.Figure := nil;
          while Assigned(NextFigure) do
          begin
            if NextFigure.FEnabled then
            begin
              if NextFigure.Visible then
              begin
                if (NextFigure = KeyFigure) and NextFigure.AutoTransparent and
                  Assigned(XRayImage) then
                begin
                  StripX := (XRayX1Fix - MapX * FMap.TileWidth)
                    div FMap.StripWidth;
                  StripW := (XRayX2Fix - XRayX1Fix) div FMap.StripWidth + 1;
                  T := XRayY1;
                  Y := XRayY2;
                end
                else
                begin
                  StripX := (NextFigure.FX - NextFigure.CenterX - MapX *
                    FMap.TileWidth) div FMap.StripWidth;
                  StripW := NextFigure.Width div FMap.StripWidth + 1;
                  if ((NextFigure.Width mod FMap.StripWidth) <> 0) then
                    Inc(StripW);
                  T := i - NextFigure.CenterY - NextFigure.Z;
                  Y := T + NextFigure.Height;
                end;
                ColumnData := ColumnBase;
                if (StripX < 0) then
                begin
                  Inc(StripW, StripX);
                  StripX := 0;
                end
                else
                  Inc(ColumnData, StripX);
                if (StripW > 0) then
                begin
                  for j := 1 to StripW do
                  begin
                    if (StripX + j > FMapColumnCount) then
                      Break;
                    if (ColumnData^.Active) then
                    begin
                      // if (T > ColumnData^.BaseLine) then ColumnData^.Top := T
                      // else if (T < ColumnData^.Top) then ColumnData^.Top := T;
                      if (T < ColumnData^.Top) then
                        ColumnData^.Top := T;
                      if (Y > ColumnData^.BaseLine) then
                        ColumnData^.BaseLine := Y;
                    end
                    else
                    begin
                      ColumnData^.Top := T;
                      ColumnData^.BaseLine := Y;
                      ColumnData^.Active := True;
                    end;
                    Inc(ColumnData);
                  end;
                end;
              end;
              DrawFigure(NextFigure);
            end;
            LastFigure := NextFigure;
            NextFigure := LastFigure.NextOnRow;
            LastFigure.NextOnRow := nil;
          end;
        end;
        Inc(RowData);
      end;
    end;

    SelectObject(TempDC, OldTempBitmap);
    GlobalUnlock(MapColumns);
    GlobalUnlock(MapRows);
  end;

  if Assigned(KeyFigure) and (KeyFigure.AutoTransparent) and Assigned(XRayImage)
  then
  begin
    { Work.BltFast(320+KeyFigure.FX-XRayWidth div 2-OffsetX-KeyX,0,
      XRayImage,Rect(0,0,XRayWidth,XRayHeight),DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
      ddck.dwColorSpaceLowValue:=FMap.FColorMatch;
      ddck.dwColorSpaceHighValue:=ddck.dwColorSpaceLowValue;
      Work.SetColorKey(DDCKEY_SRCBLT,ddck);
      lpDDSBack.BltFast(KeyX+left,KeyY+top,Work,Rect(WorkWidth div 2,0,WorkWidth div 2+XRayWidth+FMap.FStripWidth,XRayHeight),
      DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT); }
  end;

  // Display frame buffer
  if Assigned(FOnBeforeDisplay) then
    FOnBeforeDisplay(Self);

  lpDDSFront.Flip(nil, DDFLIP_WAIT);

  if Assigned(FOnAfterDisplay) then
    FOnAfterDisplay(Self);

  for i := 0 to FigureList.Count - 1 do
  begin
    if FigureList[i].Enabled then
      FigureList[i].DoFrame;
  end;

  Inc(FFRameCount);
  FDrawing := false;
end;

procedure TAniView.UpdateMap;
var
  NewMapX, NewMapY: Longint;
  i, j: Longint;
  MinX, MaxX: Longint;
  MapBase, p: ^GridInfo;
  ClipX1, ClipX2, ClipY1, ClipY2: Longint;
  X, Y: Longint;
  SrcX1, SrcY1: Longint;
  SrcX2, SrcY2: Longint;
  pr: TRect;
begin
  NewMapX := OffsetX div FMap.TileWidth;
  if (OffsetX < 0) then
    if ((OffsetX mod FMap.TileWidth) <> 0) then
      Dec(NewMapX);
  NewMapY := OffsetY div FMap.TileHeight;
  if (OffsetY < 0) then
    if ((OffsetY mod FMap.TileHeight) <> 0) then
      Dec(NewMapY);
  if (NewMapX <> MapX) or (NewMapY <> MapY) then
  begin
    i := MapX - NewMapX;
    j := MapY - NewMapY;
    if (i > 0) then
    begin
      SrcX1 := 0;
      X := i * FMap.TileWidth;
      SrcX2 := MapBitWidth - X;
    end
    else if (i < 0) then
    begin
      SrcX1 := -i * FMap.TileWidth;
      X := 0;
      SrcX2 := SrcX1 + MapBitWidth - SrcX1;
    end
    else
    begin
      SrcX1 := 0;
      X := 0;
      SrcX2 := MapBitWidth;
    end;

    if (j > 0) then
    begin
      SrcY1 := 0;
      Y := j * FMap.TileHeight;
      SrcY2 := MapBitHeight - Y;
    end
    else if (j < 0) then
    begin
      SrcY1 := -j * FMap.TileHeight;
      Y := 0;
      SrcY2 := SrcY1 + MapBitHeight - SrcY1;
    end
    else
    begin
      SrcY1 := 0;
      Y := 0;
      SrcY2 := MapBitHeight;
    end;

    pr := Rect(SrcX1, SrcY1, SrcX2, SrcY2);
    lpDDSMap.BltFast(X, Y, lpDDSMap, @pr, DDBLTFAST_NOCOLORKEY or
      DDBLTFAST_WAIT);
    // Draw empty Tiles and objects on mapbuffer
    if (Abs(NewMapX - MapX) > MapWidth) or (Abs(NewMapY - MapY) > MapHeight)
    then
    begin
      MapX := NewMapX;
      MapY := NewMapY;
      FRefreshMap;
    end
    else
    begin
      MapBase := GlobalLock(FMap.MapData);
      if (NewMapX < MapX) then
      begin
        for j := 1 to MapHeight do
        begin
          Y := NewMapY + j - 1;
          if (Y >= 0) and (Y < FMap.Height) then
          begin
            p := MapBase;
            Inc(p, NewMapX + Y * FMap.Width);
            for i := 1 to MapX - NewMapX do
            begin
              X := NewMapX + i - 1;
              if (X >= 0) and (X < FMap.Width) then
              begin
                DrawTile(p, i, j, 0);
                DrawTile(p, i, j, 1);
              end;
              Inc(p);
            end;
          end;
        end;
        ClipX1 := NewMapX * FMap.TileWidth;
        ClipX2 := MapX * FMap.TileWidth;
        MinX := MapX - NewMapX + 1;
        MaxX := MapWidth;
      end
      else if (NewMapX > MapX) then
      begin
        for j := 1 to MapHeight do
        begin
          Y := NewMapY + j - 1;
          if (Y >= 0) and (Y < FMap.Height) then
          begin
            p := MapBase;
            Inc(p, MapWidth + MapX + Y * FMap.Width);
            for i := MapWidth - NewMapX + MapX + 1 to MapWidth do
            begin
              X := NewMapX + i - 1;
              if (X >= 0) and (X < FMap.Width) then
              begin
                DrawTile(p, i, j, 0);
                DrawTile(p, i, j, 1);
              end;
              Inc(p);
            end;
          end;
        end;
        ClipX1 := (MapX + MapWidth) * FMap.TileWidth;
        ClipX2 := (NewMapX + MapWidth) * FMap.TileWidth;
        MinX := 1;
        MaxX := MapWidth - NewMapX + MapX;
      end
      else
      begin
        ClipX1 := 0;
        ClipX2 := 0;
        MinX := 1;
        MaxX := MapWidth;
      end;
      if (NewMapY < MapY) then
      begin
        for j := 1 to MapY - NewMapY do
        begin
          Y := NewMapY + j - 1;
          if (Y >= 0) and (Y < FMap.Height) then
          begin
            p := MapBase;
            Inc(p, NewMapX + MinX - 1 + (NewMapY + j - 1) * FMap.Width);
            for i := MinX to MaxX do
            begin
              X := NewMapX + i - 1;
              if (X >= 0) and (X < FMap.Width) then
              begin
                DrawTile(p, i, j, 0);
                DrawTile(p, i, j, 1);
              end;
              Inc(p);
            end;
          end;
        end;
        ClipY1 := NewMapY * FMap.TileHeight;
        ClipY2 := MapY * FMap.TileHeight;
      end
      else if (NewMapY > MapY) then
      begin
        for j := MapHeight - NewMapY + MapY + 1 to MapHeight do
        begin
          Y := NewMapY + j - 1;
          if (Y >= 0) and (Y < FMap.Height) then
          begin
            p := MapBase;
            Inc(p, NewMapX + MinX - 1 + (NewMapY + j - 1) * FMap.Width);
            for i := MinX to MaxX do
            begin
              X := NewMapX + i - 1;
              if (X >= 0) and (X < FMap.Width) then
              begin
                DrawTile(p, i, j, 0);
                DrawTile(p, i, j, 1);
              end;
              Inc(p);
            end;
          end;
        end;
        ClipY1 := (MapY + MapHeight) * FMap.TileHeight;
        ClipY2 := (NewMapY + MapHeight) * FMap.TileHeight;
      end
      else
      begin
        ClipY1 := 0;
        ClipY2 := 0;
      end;
      GlobalUnlock(FMap.MapData);
      MapX := NewMapX;
      MapY := NewMapY;
      DrawItemsClip(ClipX1, ClipX2, ClipY1, ClipY2);
    end;
  end;
  MapOffsetX := OffsetX mod FMap.TileWidth;
  if (MapOffsetX < 0) then
    Inc(MapOffsetX, FMap.TileWidth);
  MapOffsetY := OffsetY mod FMap.TileHeight;
  if (MapOffsetY < 0) then
    Inc(MapOffsetY, FMap.TileHeight);
end;

procedure TAniView.PrecreateMap(W, H: Longint);
var
  ddsd: TDDSurfaceDesc;
  ReturnCode: HRESULT;
begin
  MapPreCreated := True;
  Log.Log('Creating map buffer');
  Log.Log(inttostr(W) + ' x ' + inttostr(H));
  lpDDSMap := nil;
  ZeroMemory(@ddsd, SizeOf(ddsd));
  ddsd.dwSize := SizeOf(ddsd);
  ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
  ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_VIDEOMEMORY;
  ddsd.dwWidth := W;
  ddsd.dwHeight := H;
  ReturnCode := lpDD.CreateSurface(ddsd, lpDDSMap, nil);
  if (ReturnCode = DD_OK) then
  begin
    Log.Log('Map buffer created in VRAM');
  end
  else
  begin
    Log.Log('*** Error: Map buffer created in System RAM!');
    if ReturnCode = DDERR_INCOMPATIBLEPRIMARY then
      Log.Log('DDERR_INCOMPATIBLEPRIMARY')
    else if ReturnCode = DDERR_INVALIDCAPS then
      Log.Log('DDERR_INVALIDCAPS')
    else if ReturnCode = DDERR_INVALIDOBJECT then
      Log.Log('DDERR_INVALIDOBJECT')
    else if ReturnCode = DDERR_INVALIDPARAMS then
      Log.Log('DDERR_INVALIDPARAMS')
    else if ReturnCode = DDERR_INVALIDPIXELFORMAT then
      Log.Log('DDERR_INVALIDPIXELFORMAT')
    else if ReturnCode = DDERR_NOALPHAHW then
      Log.Log('DDERR_NOALPHAHW')
    else if ReturnCode = DDERR_NOCOOPERATIVELEVELSET then
      Log.Log('DDERR_NOCOOPERATIVELEVELSET')
    else if ReturnCode = DDERR_NODIRECTDRAWHW then
      Log.Log('DDERR_NODIRECTDRAWHW')
    else if ReturnCode = DDERR_NOEMULATION then
      Log.Log('DDERR_NOEMULATION')
    else if ReturnCode = DDERR_NOEXCLUSIVEMODE then
      Log.Log('DDERR_NOEXCLUSIVEMODE')
    else if ReturnCode = DDERR_NOFLIPHW then
      Log.Log('DDERR_NOFLIPHW')
    else if ReturnCode = DDERR_NOMIPMAPHW then
      Log.Log('DDERR_NOMIPMAPHW')
    else if ReturnCode = DDERR_NOOVERLAYHW then
      Log.Log('DDERR_NOOVERLAYHW')
    else if ReturnCode = DDERR_NOZBUFFERHW then
      Log.Log('DDERR_NOZBUFFERHW')
    else if ReturnCode = DDERR_OUTOFMEMORY then
      Log.Log('DDERR_OUTOFMEMORY')
    else if ReturnCode = DDERR_OUTOFVIDEOMEMORY then
      Log.Log('DDERR_OUTOFVIDEOMEMORY')
    else if ReturnCode = DDERR_PRIMARYSURFACEALREADYEXISTS then
      Log.Log('DDERR_PRIMARYSURFACEALREADYEXISTS')
    else if ReturnCode = DDERR_UNSUPPORTEDMODE then
      Log.Log('DDERR_UNSUPPORTEDMODE');

    ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
    lpDD.CreateSurface(ddsd, lpDDSMap, nil);
  end;

  Log.Log('Creating work buffer');
  Work := nil;
  ZeroMemory(@ddsd, SizeOf(ddsd));
  ddsd.dwSize := SizeOf(ddsd);
  ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
  ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_VIDEOMEMORY;
  ddsd.dwWidth := WorkWidth;
  ddsd.dwHeight := WorkHeight;
  if (lpDD.CreateSurface(ddsd, Work, nil) <> DD_OK) then
  begin
    ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
    lpDD.CreateSurface(ddsd, Work, nil);
  end;
end;

procedure TAniView.UncreateMap;
begin
  if MapPreCreated then
  begin
    MapPreCreated := false;
    lpDDSMap := nil;
    Work := nil;
    XRayImage := nil;
  end;
end;

procedure TAniView.InitMap;
var
  W, H: Longint;
  ddsd: TDDSurfaceDesc;
  ddck: TDDCOLORKEY;
  ReturnCode: HRESULT;
begin
  if (FMap = nil) then
    Exit;
  // TZone(FMap.Zones[1]).ExportTiles('f:\zone1tiles.bmp');
  // TZone(FMap.Zones[2]).ExportTiles('f:\zone2tiles.bmp');
  { TZone(FMap.Zones[0]).ExportItems('f:\zone0items.bmp');
    TZone(FMap.Zones[1]).ExportItems('f:\zone1items.bmp');
    TZone(FMap.Zones[2]).ExportItems('f:\zone2items.bmp');
    TZone(FMap.Zones[3]).ExportItems('f:\zone3items.bmp');
    TZone(FMap.Zones[4]).ExportItems('f:\zone4items.bmp');
    TZone(FMap.Zones[5]).ExportItems('f:\zone5items.bmp');
    TZone(FMap.Zones[6]).ExportItems('f:\zone6items.bmp');
    TZone(FMap.Zones[7]).ExportItems('f:\zone7items.bmp');
    TZone(FMap.Zones[8]).ExportItems('f:\zone8items.bmp'); }
  { TZone(FMap.Zones[9]).ExportItems('f:\zone9items.bmp');
    TZone(FMap.Zones[10]).ExportItems('f:\zone10items.bmp');
    TZone(FMap.Zones[11]).ExportItems('f:\zone11items.bmp');
    TZone(FMap.Zones[12]).ExportItems('f:\zone12items.bmp');
    TZone(FMap.Zones[13]).ExportItems('f:\zone13items.bmp');
    TZone(FMap.Zones[14]).ExportItems('f:\zone14items.bmp');
    TZone(FMap.Zones[15]).ExportItems('f:\zone15items.bmp');
    TZone(FMap.Zones[16]).ExportItems('f:\zone16items.bmp');
    TZone(FMap.Zones[17]).ExportItems('f:\zone17items.bmp');
    TZone(FMap.Zones[18]).ExportItems('f:\zone18items.bmp');
    TZone(FMap.Zones[19]).ExportItems('f:\zone19items.bmp'); }
  // TZone(FMap.Zones[1]).ExportTiles('f:\zone1tiles.bmp');
  // TZone(FMap.Zones[2]).ExportTiles('f:\zone2tiles.bmp');
  // TZone(FMap.Zones[3]).ExportTiles('f:\zone3tiles.bmp');
  // TZone(FMap.Zones[1]).ExportItems('f:\zone1items.bmp');
  // TZone(FMap.Zones[2]).ExportItems('f:\zone2items.bmp');
  // TZone(FMap.Zones[3]).ExportItems('f:\zone3items.bmp');

  MapWidth := (Width div FMap.TileWidth) + 2;
  MapHeight := (Height div FMap.TileHeight) + 2;
  MapBitWidth := MapWidth * FMap.TileWidth;
  MapBitHeight := MapHeight * FMap.TileHeight;
  W := MapBitWidth;
  H := MapBitHeight;

  if not MapPreCreated then
  begin
    Log.Log('Creating map buffer');
    Log.Log(inttostr(W) + ' x ' + inttostr(H));
    lpDDSMap := nil;
    ZeroMemory(@ddsd, SizeOf(ddsd));
    ddsd.dwSize := SizeOf(ddsd);
    ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
    ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_VIDEOMEMORY;
    ddsd.dwWidth := W;
    ddsd.dwHeight := H;
    ReturnCode := lpDD.CreateSurface(ddsd, lpDDSMap, nil);
    if (ReturnCode = DD_OK) then
    begin
      Log.Log('Map buffer created in VRAM');
    end
    else
    begin
      Log.Log('*** Error: Map buffer created in System RAM!');
      if ReturnCode = DDERR_INCOMPATIBLEPRIMARY then
        Log.Log('DDERR_INCOMPATIBLEPRIMARY')
      else if ReturnCode = DDERR_INVALIDCAPS then
        Log.Log('DDERR_INVALIDCAPS')
      else if ReturnCode = DDERR_INVALIDOBJECT then
        Log.Log('DDERR_INVALIDOBJECT')
      else if ReturnCode = DDERR_INVALIDPARAMS then
        Log.Log('DDERR_INVALIDPARAMS')
      else if ReturnCode = DDERR_INVALIDPIXELFORMAT then
        Log.Log('DDERR_INVALIDPIXELFORMAT')
      else if ReturnCode = DDERR_NOALPHAHW then
        Log.Log('DDERR_NOALPHAHW')
      else if ReturnCode = DDERR_NOCOOPERATIVELEVELSET then
        Log.Log('DDERR_NOCOOPERATIVELEVELSET')
      else if ReturnCode = DDERR_NODIRECTDRAWHW then
        Log.Log('DDERR_NODIRECTDRAWHW')
      else if ReturnCode = DDERR_NOEMULATION then
        Log.Log('DDERR_NOEMULATION')
      else if ReturnCode = DDERR_NOEXCLUSIVEMODE then
        Log.Log('DDERR_NOEXCLUSIVEMODE')
      else if ReturnCode = DDERR_NOFLIPHW then
        Log.Log('DDERR_NOFLIPHW')
      else if ReturnCode = DDERR_NOMIPMAPHW then
        Log.Log('DDERR_NOMIPMAPHW')
      else if ReturnCode = DDERR_NOOVERLAYHW then
        Log.Log('DDERR_NOOVERLAYHW')
      else if ReturnCode = DDERR_NOZBUFFERHW then
        Log.Log('DDERR_NOZBUFFERHW')
      else if ReturnCode = DDERR_OUTOFMEMORY then
        Log.Log('DDERR_OUTOFMEMORY')
      else if ReturnCode = DDERR_OUTOFVIDEOMEMORY then
        Log.Log('DDERR_OUTOFVIDEOMEMORY')
      else if ReturnCode = DDERR_PRIMARYSURFACEALREADYEXISTS then
        Log.Log('DDERR_PRIMARYSURFACEALREADYEXISTS')
      else if ReturnCode = DDERR_UNSUPPORTEDMODE then
        Log.Log('DDERR_UNSUPPORTEDMODE');

      ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
      lpDD.CreateSurface(ddsd, lpDDSMap, nil);
    end;
  end;

  ddck.dwColorSpaceLowValue := FMap.ColorMatch;
  ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
  lpDDSMap.SetColorKey(DDCKEY_SRCBLT, @ddck);

  ddck.dwColorSpaceLowValue := FMap.ColorMatch;
  ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
  lpDDSBack.SetColorKey(DDCKEY_SRCBLT, @ddck);

  if not MapPreCreated then
  begin
    Log.Log('Creating work buffer');
    Work := nil;
    ZeroMemory(@ddsd, SizeOf(ddsd));
    ddsd.dwSize := SizeOf(ddsd);
    ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
    ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_VIDEOMEMORY;
    ddsd.dwWidth := WorkWidth;
    ddsd.dwHeight := WorkHeight;
    if (lpDD.CreateSurface(ddsd, Work, nil) <> DD_OK) then
    begin
      ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
      lpDD.CreateSurface(ddsd, Work, nil);
    end;
  end;

  Log.Log('MapColumns');
  if (MapColumns <> 0) then
    GlobalFree(MapColumns);
  MapColumns := 0;
  FMapColumnCount := MapWidth shl 2;
  MapColumns := GlobalAlloc(GPTR, FMapColumnCount *
    SizeOf(MapColumnHeaderInfo));
  Log.Log('MapRows');
  if (MapRows <> 0) then
    GlobalFree(MapRows);
  MapRows := 0;
  PixelHeight := (FMap.Height + 32) * FMap.TileHeight;
  // 32 added to correct off-bottom drawing errors
  MapRows := GlobalAlloc(GPTR, (PixelHeight + FMap.TileHeight) *
    SizeOf(RowUpdateInfo));
  Log.Log('BuildRowUpdateInfo');

  BuildRowUpdateInfo;
  MapX := OffsetX div FMap.TileWidth;
  MapY := OffsetY div FMap.TileHeight;
  Log.Log('Assignment Complete');
end;

procedure TAniView.RefreshRegion(X, Y, W, H: Longint);
var
  i, j: Integer;
  MapBase, p: ^GridInfo;
  Layer: Integer;
  X1, Y1: Longint;
  X2, Y2: Longint;
  XA, YA: Longint;
  MinX, MaxX: Longint;
  MaxY, MaxRow: Longint;
  RowBase, RowData: ^RowUpdateInfo;
  R: TRect;
begin
  MinX := (X div FMap.StripWidth) * FMap.StripWidth;
  MaxX := ((X + W) div FMap.StripWidth + 1) * FMap.StripWidth;

  X1 := X div FMap.TileWidth;
  if X1 < 0 then
    X1 := 0;
  Y1 := Y div FMap.TileHeight;
  if Y1 < 0 then
    Y1 := 0;
  X2 := (X + W) div FMap.TileWidth;
  if X2 >= FMap.Width then
    X2 := FMap.Width - 1;
  Y2 := (Y + H) div FMap.TileHeight;
  if Y2 >= FMap.Height then
    Y2 := FMap.Height - 1;
  R.Left := MinX - OffsetX + Left;
  R.Right := MaxX - OffsetX + Left;
  R.Top := Y - OffsetY + Top;
  R.Bottom := Y + H - OffsetY + Top;
  MapBase := GlobalLock(FMap.MapData);
  for Layer := 0 to 1 do
  begin
    for j := Y1 to Y2 do
    begin
      YA := j * FMap.TileHeight - OffsetY;
      p := MapBase;
      Inc(p, X1 + j * FMap.Width);
      for i := X1 to X2 do
      begin
        XA := i * FMap.TileWidth - OffsetX;
        CopyTile(lpDDSBack, p, XA + Left, YA + Top, Layer, @R);
        Inc(p);
      end;
    end;
  end;
  GlobalUnlock(FMap.MapData);

  // Update items (items with descenders)
  RowBase := GlobalLock(MapRows);
  RowData := RowBase;
  if (Y > 0) then
    Inc(RowData, Y);
  i := RowData^.DescendRow;
  RowData := RowBase;
  Inc(RowData, i);
  i := RowData^.ItemIndex;
  if (i = 0) then
  begin
    GlobalUnlock(MapRows);
    Exit;
  end;

  MaxY := Y + H;
  if (MaxY > PixelHeight) then
    MaxRow := PixelHeight
  else
  begin
    Inc(RowBase, MaxY);
    MaxRow := RowBase^.OverlapRow;
  end;

  X1 := R.Left;
  X2 := R.Right;
  Y1 := R.Top;
  Y2 := R.Bottom;

  while (i <> 0) do
  begin
    if (FMap.FItemList[i].Y > MaxRow) and (FMap.FItemList[i].Y <= PixelHeight)
    then
      Break;
    if FMap.FItemList[i].Visible then
    begin
      if (FMap.FItemList[i].X >= MinX) and (FMap.FItemList[i].X < MaxX) then
      begin
        if (FMap.FItemList[i].FilterID = 0) or
          ((FMap.FItemList[i].FilterID < 0) or
          (FMap.FItemList[i].FilterID = FItemMask)) and
          (FMap.FItemList[i].FilterID <> -FItemMask) then
        begin

          R.Left := FMap.FItemList[i].ImageX;
          R.Right := FMap.FItemList[i].ImageX + FMap.FItemList[i].Width;
          R.Top := FMap.FItemList[i].ImageY + FMap.FItemList[i].VHeight;
          R.Bottom := FMap.FItemList[i].ImageY + FMap.FItemList[i].Height;

          XA := FMap.FItemList[i].X - OffsetX + Left;
          YA := FMap.FItemList[i].Y - OffsetY + Top;
          Clip1(X1, X2, XA, R.Left, R.Right);
          Clip1(Y1, Y2, YA, R.Top, R.Bottom);

          lpDDSBack.BltFast(XA, YA, FMap.Zones[FMap.FItemList[i].Zone]
            .FItemImages, @R, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
        end;
      end;
    end;
    i := FMap.FItemList[i].Next;
  end;
end;

procedure TAniView.RefreshLight(Zone: TLightZone);
var
  i, j: Integer;
  MapBase, p: ^GridInfo;
  Layer: Integer;
  X, Y, W, H: Longint;
  Index: Word;
  SrcX, SrcY: Longint;
  MaxY: Longint;
  SrcY1, SrcY2: Longint;
  SrcX1, SrcX2: Longint;
  YA, SrcY1a, SrcY2a: Longint;
  DstH: Longint;
  Offset: Integer;
  NewState: Integer;
  ZoneX, ZoneY: Integer;
  pr: TRect;
begin
  case Zone.Flicker of
    flFluorescent:
      begin
        if (Zone.StateDuration <= 0) then
        begin
          if Zone.State = Zone.States then
          begin
            NewState := 1;
          end
          else
          begin
            NewState := Zone.States;
          end;
          if random(2) = 0 then
          begin
            Zone.StateDuration := random(50) + 5;
          end
          else
          begin
            Zone.StateDuration := 1;
          end;
        end
        else
        begin
          Dec(Zone.StateDuration);
          NewState := Zone.State;
        end;
      end;
    flCandle:
      begin
        if (Zone.StateDuration <= 0) then
        begin
          if Zone.State = Zone.States then
          begin
            NewState := random(3) + 1;
            if random(8) = 0 then
            begin
              Zone.StateDuration := random(10) + 5;
              Zone.Blinking := false;
            end
            else
            begin
              Zone.StateDuration := random(2) + 4;
            end;
          end
          else
          begin
            if Zone.Blinking or (random(32) = 0) then
            begin
              NewState := Zone.States;
              Zone.StateDuration := random(2) + 4;
              Zone.Blinking := True;
            end
            else
            begin
              NewState := random(3) + 1;
              Zone.StateDuration := random(10) + 5;
            end;
          end;
        end
        else
        begin
          Dec(Zone.StateDuration);
          NewState := Zone.State;
        end;
      end;
    flTorch:
      begin
        if (Zone.StateDuration <= 0) then
        begin
          NewState := random(3) + 1;
          Zone.StateDuration := random(8) + 1;
        end
        else
        begin
          Dec(Zone.StateDuration);
          NewState := Zone.State;
        end;
      end;
  else
    NewState := Zone.State;
  end;

  Offset := (Zone.States - NewState) * Zone.TileStateOffset;
  MapBase := GlobalLock(FMap.MapData);
  for Layer := 0 to 1 do
  begin
    for j := Zone.Y1 to Zone.Y2 do
    begin
      Y := (j - Zone.Y1) * FMap.TileHeight;
      p := MapBase;
      Inc(p, Zone.X1 + j * FMap.Width);
      for i := Zone.X1 to Zone.X2 do
      begin
        if FMap.Zones[p^.Zone[Layer]] = Zone then
        begin
          X := (i - Zone.X1) * FMap.TileWidth;
          Index := p^.Tile[Layer];
          if (Index <> $FFFF) then
          begin
            Dec(Index, Offset);
            SrcX := (Index div Zone.TileMaxColumnIndex) * FMap.TileWidth;
            SrcY := (Index mod Zone.TileMaxColumnIndex) * FMap.TileHeight;
            if (Layer = 0) then
            begin
              pr := Rect(SrcX, SrcY, SrcX + FMap.TileWidth,
                SrcY + FMap.TileHeight);
              Work.BltFast(X, Y, Zone.FTileImages, @pr, DDBLTFAST_NOCOLORKEY or
                DDBLTFAST_WAIT);
            end
            else
            begin
              pr := Rect(SrcX, SrcY, SrcX + FMap.TileWidth,
                SrcY + FMap.TileHeight);
              Work.BltFast(X, Y, Zone.FTileImages, @pr, DDBLTFAST_SRCCOLORKEY or
                DDBLTFAST_WAIT);
            end;
          end;
        end;
        Inc(p);
      end;
    end;
  end;
  GlobalUnlock(FMap.MapData);

  ZoneX := Zone.X1 * FMap.TileWidth - OffsetX;
  ZoneY := Zone.Y1 * FMap.TileHeight - OffsetY;
  W := (Zone.X2 - Zone.X1 + 1) * FMap.TileWidth;
  H := (Zone.Y2 - Zone.Y1 + 1) * FMap.TileHeight;

  Offset := (Zone.States - NewState) * Zone.ItemStateOffset;
  MaxY := MapHeight * FMap.TileHeight;
  if Assigned(Zone) then
  begin
    for i := 0 to Zone.Items.Count - 1 do
    begin
      if ItemInstanceInfo(Zone.Items[i]^).Visible and
        (ItemInstanceInfo(Zone.Items[i]^).FilterID = 0) or
        ((ItemInstanceInfo(Zone.Items[i]^).FilterID < 0) or
        (ItemInstanceInfo(Zone.Items[i]^).FilterID = FItemMask)) and
        (ItemInstanceInfo(Zone.Items[i]^).FilterID <> -FItemMask) then
      begin

        X := ItemInstanceInfo(Zone.Items[i]^).X - OffsetX;
        Y := ItemInstanceInfo(Zone.Items[i]^).Y -
          ItemInstanceInfo(Zone.Items[i]^).VHeight - OffsetY;
        DstH := Y + ItemInstanceInfo(Zone.Items[i]^).Height;
        if (Y < 0) then
        begin
          SrcY1 := ItemInstanceInfo(Zone.Items[i]^).ImageY - Y;
          SrcY2 := ItemInstanceInfo(Zone.Items[i]^).ImageY +
            ItemInstanceInfo(Zone.Items[i]^).Height;
          if (DstH > MapBitHeight) then
            Dec(SrcY2, DstH - MaxY);
          Y := 0;
        end
        else
        begin
          SrcY1 := ItemInstanceInfo(Zone.Items[i]^).ImageY;
          SrcY2 := ItemInstanceInfo(Zone.Items[i]^).ImageY +
            ItemInstanceInfo(Zone.Items[i]^).Height;
          if (DstH > MapBitHeight) then
            Dec(SrcY2, DstH - MaxY);
        end;

        SrcX1 := ItemInstanceInfo(Zone.Items[i]^).ImageX - Offset;
        SrcX2 := ItemInstanceInfo(Zone.Items[i]^).ImageX +
          ItemInstanceInfo(Zone.Items[i]^).Width - Offset;

        YA := Y;
        SrcY1a := SrcY1;
        SrcY2a := SrcY2;

        Clip1(ZoneY, ZoneY + H, YA, SrcY1a, SrcY2a);
        pr := Rect(SrcX1, SrcY1a, SrcX2, SrcY2a);
        Work.BltFast(X - ZoneX, YA - ZoneY, Zone.FItemImages, @pr,
          DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);

        // also clip x against edge of screen
        Clip1(Y, ZoneY, Y, SrcY1, SrcY2);
        Clip1(0, Width, X, SrcX1, SrcX2);
        pr := Rect(SrcX1, SrcY1, SrcX2, SrcY2);
        lpDDSBack.BltFast(X + Left, Y + Top, Zone.FItemImages, @pr,
          DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
      end;
    end;
  end;
  Zone.State := NewState;

  SrcX := 0;
  SrcY := 0;
  if (ZoneX < 0) then
  begin
    Dec(SrcX, ZoneX);
    Inc(W, ZoneX);
    ZoneX := 0;
  end;
  if (ZoneY < 0) then
  begin
    Dec(SrcY, ZoneY);
    Inc(H, ZoneY);
    ZoneY := 0;
  end;
  if (ZoneX + W > Width) then
  begin
    W := Width - ZoneX;
  end;
  if (ZoneY + H > Height) then
  begin
    H := Height - ZoneY;
  end;

  pr := Rect(ZoneX + Left, ZoneY + Top, ZoneX + Left + W, ZoneY + Top + H);
  Work.BltFast(SrcX, SrcY, lpDDSBack, @pr, DDBLTFAST_SRCCOLORKEY or
    DDBLTFAST_WAIT);
  pr := Rect(SrcX, SrcY, SrcX + W, SrcY + H);
  lpDDSBack.BltFast(ZoneX + Left, ZoneY + Top, Work, @pr,
    DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT);
end;

procedure TAniView.FRefreshMap;
var
  i, j: Integer;
  MapBase, p: ^GridInfo;
  Layer: Integer;
  X, Y: Longint;
begin
  // Log.Log('Refresh Map a');
  MapBase := GlobalLock(FMap.MapData);
  for Layer := 0 to 1 do
  begin
    for j := 1 to MapHeight do
    begin
      Y := MapY + j - 1;
      if (Y >= 0) and (Y < FMap.Height) then
      begin
        p := MapBase;
        Inc(p, MapX + Y * FMap.Width);
        for i := 1 to MapWidth do
        begin
          X := MapX + i - 1;
          if (X >= 0) and (X < FMap.Width) then
          begin
            // Log.Log('Refresh Map b');
            DrawTile(p, i, j, Layer);
            // Log.Log('Refresh Map c');
          end;
          Inc(p);
        end;
      end;
    end;
  end;
  // Log.Log('Refresh Map d');
  GlobalUnlock(FMap.MapData);
  // Log.Log('Refresh Map e');
  DrawItems;
  // Log.Log('Refresh Map f');
end;

procedure TAniView.RefreshMap;
begin
  MapX := OffsetX div FMap.TileWidth;
  if (OffsetX < 0) then
    if ((OffsetX mod FMap.TileWidth) <> 0) then
      Dec(MapX);
  MapY := OffsetY div FMap.TileHeight;
  if (OffsetY < 0) then
    if ((OffsetY mod FMap.TileHeight) <> 0) then
      Dec(MapY);

  MapOffsetX := OffsetX mod FMap.TileWidth;
  if (MapOffsetX < 0) then
    Inc(MapOffsetX, FMap.TileWidth);
  MapOffsetY := OffsetY mod FMap.TileHeight;
  if (MapOffsetY < 0) then
    Inc(MapOffsetY, FMap.TileHeight);
  FRefreshMap;
end;

procedure TAniView.DrawItems;
var
  i: Word;
  MinX, MinY, MaxX, MaxY: Longint;
  RowBase, RowData: ^RowUpdateInfo;
  MaxRow: Longint;
  X, Y: Longint;
  ZoneItem: TZone;
  SrcY1, SrcY2: Longint;
  MapH, DstH: Longint;
  pr: TRect;
begin
  MinY := MapY * FMap.TileHeight;
  if (MinY > PixelHeight) then
    Exit;
  MinX := MapX * FMap.TileWidth;
  MaxY := (MapY + MapHeight) * FMap.TileHeight;
  MaxX := (MapX + MapWidth) * FMap.TileWidth;
  MapH := MapBitHeight;
  RowBase := GlobalLock(MapRows);
  RowData := RowBase;
  if (MinY > 0) then
    Inc(RowData, MinY);
  i := RowData^.DescendRow;
  RowData := RowBase;
  Inc(RowData, i);
  i := RowData^.ItemIndex;
  if (MaxY > PixelHeight) then
    MaxRow := PixelHeight
  else
  begin
    Inc(RowBase, MaxY);
    MaxRow := RowBase^.OverlapRow;
  end;
  GlobalUnlock(MapRows);

  while (i <> 0) do
  begin
    if (FMap.FItemList[i].Y > MaxRow) and (FMap.FItemList[i].Y <= PixelHeight)
    then
      Break;
    if FMap.FItemList[i].Visible then
    begin
      if (FMap.FItemList[i].FilterID = 0) or
        ((FMap.FItemList[i].FilterID < 0) or
        (FMap.FItemList[i].FilterID = FItemMask)) and
        (FMap.FItemList[i].FilterID <> -FItemMask) then
      begin
        Y := FMap.FItemList[i].Y - FMap.FItemList[i].VHeight;
        if (FMap.FItemList[i].X < MaxX) and
          (Y + FMap.FItemList[i].Height >= MinY) then
        begin
          if (FMap.FItemList[i].X + FMap.FItemList[i].Width > MinX) and
            (Y < MaxY) then
          begin
            X := FMap.FItemList[i].X - MinX;
            Dec(Y, MinY);
            ZoneItem := FMap.Zones[FMap.FItemList[i].Zone];
            DstH := Y + FMap.FItemList[i].Height;
            if (Y < 0) then
            begin
              SrcY1 := FMap.FItemList[i].ImageY - Y;
              SrcY2 := FMap.FItemList[i].ImageY + FMap.FItemList[i].Height;
              if (DstH > MapH) then
                Dec(SrcY2, DstH - MapH);
              Y := 0;
            end
            else
            begin
              SrcY1 := FMap.FItemList[i].ImageY;
              SrcY2 := FMap.FItemList[i].ImageY + FMap.FItemList[i].Height;
              if (DstH > MapH) then
                Dec(SrcY2, DstH - MapH);
            end;

            if ZoneItem is TLightZone then
            begin
              if not ZoneItem.FullRefresh then
              begin
                pr := Rect(FMap.FItemList[i].ImageX, SrcY1,
                  FMap.FItemList[i].ImageX + FMap.FItemList[i].Width, SrcY2);
                lpDDSMap.BltFast(X, Y, ZoneItem.FItemImages, @pr,
                  DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
              end;
            end
            else
            begin
              pr := Rect(FMap.FItemList[i].ImageX, SrcY1,
                FMap.FItemList[i].ImageX + FMap.FItemList[i].Width, SrcY2);
              lpDDSMap.BltFast(X, Y, ZoneItem.FItemImages, @pr,
                DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
            end;
          end;
        end;
      end;
    end;
    i := FMap.FItemList[i].Next;
  end;
end;

procedure TAniView.DrawItemsClip(X1, X2, Y1, Y2: Longint);
var
  i: Word;
  MinX, MinY, MaxX, MaxY: Longint;
  X3, Y3, Y: Longint;
  SrcX, SrcY, W, W1, H, DstX, DstY: Longint;
  RowBase, RowData: ^RowUpdateInfo;
  MaxRow: Longint;
  ZoneItem: TZone;
  SrcY1, SrcY2: Longint;
  MapH, DstH: Longint;
  pr: TRect;
begin
  MinY := MapY * FMap.TileHeight;
  if (MinY > PixelHeight) then
    Exit;
  MinX := MapX * FMap.TileWidth;
  MaxY := (MapY + MapHeight) * FMap.TileHeight;
  MaxX := (MapX + MapWidth) * FMap.TileWidth;
  MapH := MapBitHeight;
  RowBase := GlobalLock(MapRows);
  RowData := RowBase;
  if (MinY > 0) then
    Inc(RowData, MinY);
  i := RowData^.DescendRow;
  RowData := RowBase;
  Inc(RowData, i);
  i := RowData^.ItemIndex;
  if (i = 0) then
  begin
    GlobalUnlock(MapRows);
    Exit;
  end;
  if (MaxY > PixelHeight) then
    MaxRow := PixelHeight
  else
  begin
    Inc(RowBase, MaxY);
    MaxRow := RowBase^.OverlapRow;
  end;
  GlobalUnlock(MapRows);

  while (i <> 0) do
  begin
    if (FMap.FItemList[i].Y > MaxRow) and (FMap.FItemList[i].Y <= PixelHeight)
    then
      Break;
    if FMap.FItemList[i].Visible then
    begin
      if (FMap.FItemList[i].FilterID = 0) or
        ((FMap.FItemList[i].FilterID < 0) or
        (FMap.FItemList[i].FilterID = FItemMask)) and
        (FMap.FItemList[i].FilterID <> -FItemMask) then
      begin
        if (FMap.FItemList[i].X < MaxX) then
        begin
          Y := FMap.FItemList[i].Y - FMap.FItemList[i].VHeight + FMap.FItemList
            [i].Height;
          if (Y >= MinY) then
          begin
            W := FMap.FItemList[i].Width;
            W1 := W;
            H := FMap.FItemList[i].Height;
            X3 := FMap.FItemList[i].X + W;
            Y3 := FMap.FItemList[i].Y - FMap.FItemList[i].VHeight;
            ZoneItem := FMap.Zones[FMap.FItemList[i].Zone];
            if (X3 > MinX) and (Y3 < MaxY) then
            begin
              if (X3 > X1) and (FMap.FItemList[i].X < X2) then
              begin
                // Perform clipping
                if (FMap.FItemList[i].X <= X1) then
                begin
                  SrcX := FMap.FItemList[i].ImageX + W - X3 + X1;
                  DstX := X1 - MinX;
                  if (X3 < X2) then
                    W := X3 - X1
                  else
                    W := X2 - X1;
                end
                else
                begin
                  SrcX := FMap.FItemList[i].ImageX;
                  DstX := FMap.FItemList[i].X - MinX;
                  W := X2 - FMap.FItemList[i].X;
                  if (W > FMap.FItemList[i].Width) then
                    W := FMap.FItemList[i].Width;
                end;
                DstY := Y3 - MinY;
                DstH := DstY + H;
                if (DstY < 0) then
                begin
                  SrcY1 := FMap.FItemList[i].ImageY - DstY;
                  SrcY2 := FMap.FItemList[i].ImageY + H;
                  if (DstH > MapH) then
                    Dec(SrcY2, DstH - MapH);
                  DstY := 0;
                end
                else
                begin
                  SrcY1 := FMap.FItemList[i].ImageY;
                  SrcY2 := FMap.FItemList[i].ImageY + H;
                  if (DstH > MapH) then
                    Dec(SrcY2, DstH - MapH);
                end;

                if SrcY2 > SrcY1 then
                begin
                  if ZoneItem is TLightZone then
                  begin
                    if not ZoneItem.FullRefresh then
                    begin
                      pr := Rect(SrcX, SrcY1, SrcX + W, SrcY2);
                      lpDDSMap.BltFast(DstX, DstY, ZoneItem.FItemImages, @pr,
                        DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
                    end;
                  end
                  else
                  begin
                    pr := Rect(SrcX, SrcY1, SrcX + W, SrcY2);
                    lpDDSMap.BltFast(DstX, DstY, ZoneItem.FItemImages, @pr,
                      DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
                  end;
                end;
              end;

              if (Y > Y1) and (Y3 < Y2) then
              begin
                if (Y3 <= Y1) then
                begin
                  SrcY := FMap.FItemList[i].ImageY + Y1 - Y3;
                  DstY := Y1 - MinY;
                  if (Y < Y2) then
                    H := Y - Y1
                  else
                    H := Y2 - Y1;
                end
                else
                begin
                  SrcY := FMap.FItemList[i].ImageY;
                  DstY := Y3 - MinY;
                  H := Y2 - Y3;
                  if (H > FMap.FItemList[i].Height) then
                    H := FMap.FItemList[i].Height;
                end;
                DstX := FMap.FItemList[i].X - MinX;
                DstH := DstY + H;
                if (DstY < 0) then
                begin
                  SrcY1 := SrcY - DstY;
                  SrcY2 := SrcY + H;
                  if (DstH > MapH) then
                    Dec(SrcY2, DstH - MapH);
                  DstY := 0;
                end
                else
                begin
                  SrcY1 := SrcY;
                  SrcY2 := SrcY + H;
                  if (DstH > MapH) then
                    Dec(SrcY2, DstH);
                end;

                if W > 0 then
                begin
                  if ZoneItem is TLightZone then
                  begin
                    if not ZoneItem.FullRefresh then
                    begin
                      pr := Rect(FMap.FItemList[i].ImageX, SrcY1,
                        FMap.FItemList[i].ImageX + W1, SrcY2);
                      lpDDSMap.BltFast(DstX, DstY, ZoneItem.FItemImages, @pr,
                        DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
                    end;
                  end
                  else
                  begin
                    pr := Rect(FMap.FItemList[i].ImageX, SrcY1,
                      FMap.FItemList[i].ImageX + W1, SrcY2);
                    lpDDSMap.BltFast(DstX, DstY, ZoneItem.FItemImages, @pr,
                      DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
                  end;
                end;
              end;

            end;
          end;
        end;
      end;
    end;
    i := FMap.FItemList[i].Next;
  end;
end;

procedure TAniView.DrawTile(GridLoc: Pointer; i, j, Layer: Integer);
var
  X, Y: Integer;
begin
  X := (i - 1) * FMap.TileWidth;
  Y := (j - 1) * FMap.TileHeight;
  CopyTile(lpDDSMap, GridLoc, X, Y, Layer, nil);
  PGridInfo(GridLoc).BitField := PGridInfo(GridLoc).BitField or $40;
  // This space has been viewed on screen
end;

procedure TAniView.CopyTile(Dest: IDirectDrawSurface; GridLoc: Pointer;
  X, Y, Layer: Integer; ClipRect: PRect);
var
  Index: Word;
  SrcX, SrcY: Longint;
  DstX, DstY: Longint;
  p: ^GridInfo;
  ZoneTile: TZone;
  HalfWidth, HalfHeight: Integer;
  // BltFx : TDDBLTFX;
  SrcX2, SrcY2: Longint;
  Offset: Integer;
{$IFDEF DEBUG}
  Mask, k: Word;
  DC: HDC;
{$ENDIF}
  pr: TRect;
begin
  p := GridLoc;
  Index := p^.Tile[Layer];
  if (Layer = 1) and ((p^.BitField and $80) <> 0) then
  begin
    HalfWidth := FMap.TileWidth div 2;
    HalfHeight := FMap.TileHeight div 2;

    if (Index <> $FFFF) then
    begin
      ZoneTile := FMap.Zones[p^.Zone[1]];
      SrcX := (Index div ZoneTile.TileMaxColumnIndex) * FMap.TileWidth;
      SrcY := (Index mod ZoneTile.TileMaxColumnIndex) * FMap.TileHeight +
        HalfHeight;
      SrcX2 := SrcX + FMap.TileWidth;
      SrcY2 := SrcY + HalfHeight;
      DstX := X;
      DstY := Y;
      if Assigned(ClipRect) then
      begin
        Clip1(ClipRect.Left, ClipRect.Right, DstX, SrcX, SrcX2);
        Clip1(ClipRect.Top, ClipRect.Bottom, DstY, SrcY, SrcY2);
      end;
      if (SrcX2 > SrcX) and (SrcY2 > SrcY) then
      begin
        pr := Rect(SrcX, SrcY, SrcX2, SrcY2);
        Dest.BltFast(DstX, DstY, ZoneTile.FTileImages, @pr,
          DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
      end;
    end;

    if (p^.Tile[2] <> $FFFF) then
    begin
      ZoneTile := FMap.Zones[p^.Zone[2]];
      SrcX := (p^.Tile[2] div ZoneTile.TileMaxColumnIndex) * FMap.TileWidth;
      SrcY := (p^.Tile[2] mod ZoneTile.TileMaxColumnIndex) * FMap.TileHeight;
      SrcX2 := SrcX + HalfWidth;
      SrcY2 := SrcY + FMap.TileHeight;
      DstX := X + HalfWidth;
      DstY := Y;
      if Assigned(ClipRect) then
      begin
        Clip1(ClipRect.Left, ClipRect.Right, DstX, SrcX, SrcX2);
        Clip1(ClipRect.Top, ClipRect.Bottom, DstY, SrcY, SrcY2);
      end;
      if (SrcX2 > SrcX) and (SrcY2 > SrcY) then
      begin
        pr := Rect(SrcX, SrcY, SrcX2, SrcY2);
        Dest.BltFast(DstX, DstY, ZoneTile.FTileImages, @pr,
          DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
      end;
    end;

    if (p^.Tile[3] <> $FFFF) then
    begin
      ZoneTile := FMap.Zones[p^.Zone[3]];
      SrcX := (p^.Tile[3] div ZoneTile.TileMaxColumnIndex) * FMap.TileWidth;
      SrcY := (p^.Tile[3] mod ZoneTile.TileMaxColumnIndex) * FMap.TileHeight;
      SrcX2 := SrcX + FMap.TileWidth;
      SrcY2 := SrcY + HalfHeight;
      DstX := X;
      DstY := Y + HalfHeight;
      if Assigned(ClipRect) then
      begin
        Clip1(ClipRect.Left, ClipRect.Right, DstX, SrcX, SrcX2);
        Clip1(ClipRect.Top, ClipRect.Bottom, DstY, SrcY, SrcY2);
      end;
      if (SrcX2 > SrcX) and (SrcY2 > SrcY) then
      begin
        pr := Rect(SrcX, SrcY, SrcX2, SrcY2);
        Dest.BltFast(DstX, DstY, ZoneTile.FTileImages, @pr,
          DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
      end;
    end;

    if (p^.Tile[4] <> $FFFF) then
    begin
      ZoneTile := FMap.Zones[p^.Zone[4]];
      SrcX := (p^.Tile[4] div ZoneTile.TileMaxColumnIndex) * FMap.TileWidth +
        HalfWidth;
      SrcY := (p^.Tile[4] mod ZoneTile.TileMaxColumnIndex) * FMap.TileHeight;
      SrcX2 := SrcX + HalfWidth;
      SrcY2 := SrcY + FMap.TileHeight;
      DstX := X;
      DstY := Y;
      if Assigned(ClipRect) then
      begin
        Clip1(ClipRect.Left, ClipRect.Right, DstX, SrcX, SrcX2);
        Clip1(ClipRect.Top, ClipRect.Bottom, DstY, SrcY, SrcY2);
      end;
      if (SrcX2 > SrcX) and (SrcY2 > SrcY) then
      begin
        pr := Rect(SrcX, SrcY, SrcX2, SrcY2);
        Dest.BltFast(DstX, DstY, ZoneTile.FTileImages, @pr,
          DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
      end;
    end;
  end
  else
  begin
    if (Index = $FFFF) then
      Exit;
    ZoneTile := FMap.Zones[p^.Zone[Layer]];
    if ZoneTile is TLightZone then
    begin
      if ZoneTile.FullRefresh then
      begin
        if Dest <> lpDDSMap then
        begin
          // if Layer = 1 then
          // Exit;
          //
          // DstX := X + FMap.FTileWidth;
          // DstY := Y + FMap.FTileHeight;
          // if Assigned( ClipRect ) then
          // begin
          // if ( X < ClipRect.Left ) then
          // X := ClipRect.Left;
          // if ( DstX > ClipRect.Right ) then
          // DstX := ClipRect.Right;
          // if ( Y < ClipRect.Top ) then
          // Y := ClipRect.Top;
          // if ( DstY > ClipRect.Bottom ) then
          // DstY := ClipRect.Bottom;
          // end;
          // BltFx.dwSize := SizeOf( BltFx );
          // BltFx.dwFillColor := FMap.FColorMatch;
          // pr :=  Rect( X, Y, DstX, DstY );
          // Dest.Blt( @pr, nil, @pr, DDBLT_COLORFILL + DDBLT_WAIT, @BltFx );
          // Exit;
          // end
          // else
          // begin
          Offset := (TLightZone(ZoneTile).States - TLightZone(ZoneTile).State) *
            TLightZone(ZoneTile).TileStateOffset;
          Dec(Index, Offset);
        end;
      end;
    end;

    SrcX := (Index div ZoneTile.TileMaxColumnIndex) * FMap.TileWidth;
    SrcY := (Index mod ZoneTile.TileMaxColumnIndex) * FMap.TileHeight;
    if (Layer = 0) then
    begin
      SrcX2 := SrcX + FMap.TileWidth;
      SrcY2 := SrcY + FMap.TileHeight;
      DstX := X;
      DstY := Y;
      if Assigned(ClipRect) then
      begin
        Clip1(ClipRect.Left, ClipRect.Right, DstX, SrcX, SrcX2);
        Clip1(ClipRect.Top, ClipRect.Bottom, DstY, SrcY, SrcY2);
        if (SrcX2 <= SrcX) or (SrcY2 <= SrcY) then
          Exit;
      end;
      pr := Rect(SrcX, SrcY, SrcX2, SrcY2);
      Dest.BltFast(DstX, DstY, ZoneTile.FTileImages, @pr,
        DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT);
    end
    else
    begin
      SrcX2 := SrcX + FMap.TileWidth;
      SrcY2 := SrcY + FMap.TileHeight;
      DstX := X;
      DstY := Y;
      if Assigned(ClipRect) then
      begin
        Clip1(ClipRect.Left, ClipRect.Right, DstX, SrcX, SrcX2);
        Clip1(ClipRect.Top, ClipRect.Bottom, DstY, SrcY, SrcY2);
        if (SrcX2 <= SrcX) or (SrcY2 <= SrcY) then
          Exit;
      end;
      pr := Rect(SrcX, SrcY, SrcX2, SrcY2);
      Dest.BltFast(DstX, DstY, ZoneTile.FTileImages, @pr,
        DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
    end;
  end;

{$IFDEF DEBUG}
  if Dest = lpDDSMap then
  begin
    Dest.GetDC(DC);
    try
      if (p^.CollisionMask <> 0) then
      begin
        Mask := p^.LineOfSightMask;
        for k := 0 to 3 do
        begin
          if ((Mask and 1) = 1) then
            PatBlt(DC, X + k * 16, Y + 24, 16, 8, WHITENESS);
          Mask := Mask shr 1;
        end;
        for k := 0 to 3 do
        begin
          if ((Mask and 1) = 1) then
            PatBlt(DC, X + k * 16, Y + 16, 16, 8, WHITENESS);
          Mask := Mask shr 1;
        end;
        for k := 0 to 3 do
        begin
          if ((Mask and 1) = 1) then
            PatBlt(DC, X + k * 16, Y + 8, 16, 8, WHITENESS);
          Mask := Mask shr 1;
        end;
        for k := 0 to 3 do
        begin
          if ((Mask and 1) = 1) then
            PatBlt(DC, X + k * 16, Y, 16, 8, WHITENESS);
          Mask := Mask shr 1;
        end;
      end;
    finally
      Dest.ReleaseDC(DC);
    end;
  end;
{$ENDIF}
{$IFDEF DEBUG}
  if Dest = MapBuffer.Canvas.Handle then
  begin
    MapBuffer.Canvas.Brush.Style := bsClear;
    Rectangle(Dest, X, Y, X + FMap.FTileWidth + 1, Y + FMap.FTileHeight + 1);
    MapBuffer.Canvas.Brush.Style := bsSolid;
    if (p^.CollisionMask <> 0) then
    begin
      Mask := p^.LineOfSightMask;
      for k := 0 to 3 do
      begin
        if ((Mask and 1) = 1) then
          PatBlt(Dest, X + k * 16, Y + 24, 16, 8, WHITENESS);
        Mask := Mask shr 1;
      end;
      for k := 0 to 3 do
      begin
        if ((Mask and 1) = 1) then
          PatBlt(Dest, X + k * 16, Y + 16, 16, 8, WHITENESS);
        Mask := Mask shr 1;
      end;
      for k := 0 to 3 do
      begin
        if ((Mask and 1) = 1) then
          PatBlt(Dest, X + k * 16, Y + 8, 16, 8, WHITENESS);
        Mask := Mask shr 1;
      end;
      for k := 0 to 3 do
      begin
        if ((Mask and 1) = 1) then
          PatBlt(Dest, X + k * 16, Y, 16, 8, WHITENESS);
        Mask := Mask shr 1;
      end;
    end;
  end;
{$ENDIF}
end;

procedure TAniView.CenterView(X, Y: Longint);
begin
  KeyFigure := nil;
  CenterX := X;
  CenterY := Y;
end;

procedure TAniView.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FLMouseButton := Button = mbLeft;
  if Assigned(FOnMouseDown) then
  begin
    inherited MouseDown(Button, Shift, X, Y);
    OnMouseDown(Self, Button, Shift, X, Y, X + OffsetX, Y + OffsetY);
  end;
end;

procedure TAniView.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  FLMouseButton := false;
  if Assigned(FOnMouseUp) then
  begin
    inherited MouseUp(Button, Shift, X, Y);
    OnMouseUp(Self, Button, Shift, X, Y, X + OffsetX, Y + OffsetY);
  end;
end;

procedure TAniView.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseMove) then
  begin
    inherited MouseMove(Shift, X, Y);
    OnMouseMove(Self, Shift, X, Y, X + OffsetX, Y + OffsetY);
  end;
end;

procedure TAniView.BuildRowUpdateInfo;
var
  RowBase, RowData: ^RowUpdateInfo;
  i, j: Longint;
  Y, ItemY: Longint;
  MaxHeight: Integer;
  ItemIndex: Word;
  MaxRow: Longint;
begin
  RowBase := GlobalLock(MapRows);
  RowData := RowBase;
  Y := 0;
  MaxHeight := 0;
  i := FMap.FirstItem;
  while (i > 0) do
  begin
    ItemY := FMap.FItemList[i].Y;
    if ItemY < 0 then
    begin
    end
    else if (ItemY = Y) then
    begin
      if (FMap.FItemList[i].VHeight > MaxHeight) then
        MaxHeight := FMap.FItemList[i].VHeight;
    end
    else
    begin
      RowData^.MaxHeight := MaxHeight;
      if (ItemY >= PixelHeight) then
      begin
        RowData := RowBase;
        Inc(RowData, PixelHeight);
        MaxHeight := FMap.FItemList[i].VHeight - ItemY + PixelHeight - 1;
      end
      else
      begin
        Inc(RowData, ItemY - Y);
        MaxHeight := FMap.FItemList[i].VHeight;
      end;
      Y := ItemY;
      RowData^.ItemIndex := i;
    end;
    i := FMap.FItemList[i].Next;
  end;
  RowData^.MaxHeight := MaxHeight;

  MaxRow := PixelHeight + FMap.TileHeight;
  RowData := RowBase;
  for i := 0 to MaxRow - 1 do
  begin
    RowData^.DescendRow := i;
    Inc(RowData);
  end;

  i := FMap.FirstItem;
  while (i > 0) do
  begin
    Y := FMap.FItemList[i].Y - FMap.FItemList[i].VHeight + FMap.FItemList
      [i].Height;
    if (Y >= MaxRow) then
      Y := MaxRow - 1;
    if (Y >= 0) then
    begin
      j := FMap.FItemList[i].Y + 1;
      if j < 0 then
        j := 0;
      RowData := RowBase;
      Inc(RowData, j);
      for j := j to Y do
      begin
        if (FMap.FItemList[i].Y < RowData^.DescendRow) then
        begin
          if FMap.FItemList[i].Y < 0 then
            RowData^.DescendRow := 0
          else
            RowData^.DescendRow := FMap.FItemList[i].Y;
        end;
        Inc(RowData);
      end;
    end;
    i := FMap.FItemList[i].Next;
  end;

  i := PixelHeight;
  j := i;
  Inc(RowBase, i);
  RowData := RowBase;
  ItemIndex := RowData^.ItemIndex;
  Y := j - RowData^.MaxHeight + 1;
  while (i >= 0) do
  begin
    if (RowBase^.ItemIndex = 0) then
      RowBase^.ItemIndex := ItemIndex
    else
      ItemIndex := RowBase^.ItemIndex;
    while (i < Y) and (j > 1) do
    begin
      Dec(RowData);
      Dec(j);
      Y := j - RowData^.MaxHeight + 1;
    end;
    RowBase^.OverlapRow := j;
    Dec(i);
    Dec(RowBase);
  end;
  GlobalUnlock(MapRows);
end;

function TAniView.CanMove(SrcX, SrcY, DestX, DestY: SmallInt): Boolean;
var
  X, Y: Longint;
  DestX1, DestY1: Longint;
  R, R2, D2: Longint;
  X1, Y1, X2, Y2: Longint;
  XL: Longint;
  GridBase, GridLoc: ^GridInfo;
  CollisionMask: Word;
  cx, cy: Longint;
  CellWidth, CellHeight: Integer;
  i, j: Integer;
  Dx, dy: Integer;
  ScanY: Longint;
  A, B: double;
begin
  CellWidth := FMap.StripWidth shr 1;
  CellHeight := FMap.StripHeight shr 1;
  DestX1 := FStartX + DestX * CellWidth;
  DestY1 := FStartY + DestY * CellHeight;

  if Assigned(FAstarAvoidFigure) then
  begin
    for i := 0 to FAstarAvoidFigure.Count - 1 do
    begin
      A := sqr(FAstarAvoidFigure[i].Radius + FAStarFigure.Radius) +
        sqr(FMap.StripWidth); // StripWidth is the fudge factor
      B := sqr(FAstarAvoidFigure[i].StepX - DestX1) +
        sqr(2 * (FAstarAvoidFigure[i].StepY - DestY1));
      // if (B<A) then begin
      // A:=sqr(StepX-(FStartX+SrcX*CellWidth)+CellWidth)+sqr(2*(StepY-(FStartY+SrcY*CellHeight))+CellHeight);
      if (B < A) then
      begin // This will allow movement if the figure is moving away from the target
        Result := false; // even though it is still inside the radius.
        Exit;
      end;
      // end;
    end;
  end;

  R := FAStarFigure.Radius + CellWidth + 2; // 2 added for round off error
  R2 := R * R;
  X1 := DestX1 - R; // if (X1<0) then X1:=0;
  X2 := DestX1 + R; // if (X2>=FMap.FBitWidth) then X2:=FMap.FBitWidth-1;
  Y1 := DestY1 - R; // if (Y1<0) then Y1:=0;
  Y2 := DestY1 + R; // if (Y2>=FMap.FBitheight) then Y2:=FMap.FBitHeight-1;
  if (X1 < FMap.TileWidth - CellWidth) or
    (X2 >= FMap.BitWidth + FMap.TileWidth + CellWidth) or
    (Y1 < FMap.TileHeight - CellWidth) or
    (Y2 >= FMap.BitHeight + FMap.TileHeight + CellWidth) then
  begin
    Result := false;
    Exit;
  end;

  GridBase := GlobalLock(FMap.MapData);

  XL := X1 div FMap.TileWidth;

  for Y := Y1 div FMap.TileHeight to Y2 div FMap.TileHeight do
  begin
    if (Y >= 0) and (Y < FMap.Height) then
    begin
      ScanY := (Y + 1) * FMap.TileHeight - CellHeight;
      GridLoc := GridBase;
      Inc(GridLoc, Y * FMap.Width + XL);
      for X := XL to X2 div FMap.TileWidth do
      begin
        if (X >= 0) and (X < FMap.Width) then
        begin
          if FAStarFigure.UseLineOfSight then
            CollisionMask := GridLoc^.LineOfSightMask
          else
            CollisionMask := GridLoc^.CollisionMask;
          if (CollisionMask <> 0) then
          begin
            for j := 0 to 3 do
            begin
              if ((CollisionMask and $F) <> 0) then
              begin
                cy := ScanY - j * FMap.StripHeight;
                for i := 0 to 3 do
                begin
                  if ((CollisionMask and 1) = 1) then
                  begin
                    cx := X * FMap.TileWidth + i * FMap.StripWidth + CellWidth;
                    Dx := (DestX1 - cx);
                    dy := 2 * (DestY1 - cy);
                    D2 := Dx * Dx + dy * dy;
                    if (D2 < R2) then
                    begin
                      Result := false;
                      Exit;
                    end;
                  end;
                  CollisionMask := CollisionMask shr 1;
                end;
              end
              else
                CollisionMask := CollisionMask shr 4;
            end;
          end;
        end;
        Inc(GridLoc);
      end;
    end;
  end;
  GlobalUnlock(FMap.MapData);
  Result := True;
end;

function TAniView.FindPath(Figure: TAniFigure; X2, Y2, Deviance: Longint;
  var Path: HGLOBAL): Integer;
var
  p: ^TPoint;
  Dx, dy: Longint;
  CellWidth, CellHeight: Integer;
  i: Integer;
begin
  Result := 0;
  CellWidth := FMap.StripWidth shr 1;
  CellHeight := FMap.StripHeight shr 1;
  Dx := Round(2 * (X2 - Figure.StepX) / FMap.StripWidth);
  dy := Round(2 * (Y2 - Figure.StepY) / FMap.StripHeight);
  if (Dx = 0) and (dy = 0) then
    Exit;
  if (Dx <= MaxSearch) and (Dx >= MinSearch) and (dy <= MaxSearch) and
    (dy >= MinSearch) then
  begin
    FAStarFigure := Figure;
    FAstarAvoidFigure := nil;
    FStartX := CellWidth * Trunc(2 * Figure.StepX / FMap.StripWidth) +
      (FMap.StripWidth shr 2);
    FStartY := CellHeight * Trunc(2 * Figure.StepY / FMap.StripHeight) +
      (FMap.StripHeight shr 2);
    FAStar.Deviance := Deviance;
    Result := FAStar.FindJaggedPath(0, 0, Dx, dy, Path);
    if Result > 0 then
    begin
      p := GlobalLock(Path);
      for i := 1 to Result do
      begin
        p^.X := p^.X * CellWidth + FStartX;
        p^.Y := p^.Y * CellHeight + FStartY;
        Inc(p);
      end;
      GlobalUnlock(Path);
    end;
  end;
end;

procedure TAniView.GetPath(Figure: TAniFigure);
var
  Dx, dy: Longint;
  D: double;
  pBase, p: ^TPoint;
  CellWidth, CellHeight: Integer;
  i: Integer;
begin
  Figure.NeedPath := false;
  CellWidth := FMap.StripWidth shr 1;
  CellHeight := FMap.StripHeight shr 1;
  Dx := Round(2 * (Figure.FPathDestX - Figure.StepX) / FMap.StripWidth);
  dy := Round(2 * (Figure.FPathDestY - Figure.StepY) / FMap.StripHeight);
  if (Dx = 0) and (dy = 0) then
  begin
    Figure.GotPath := false;
    if Assigned(Figure.OnNoPath) then
      Figure.OnNoPath(Figure);
    Exit;
  end;
  if (Dx <= MaxSearch) and (Dx >= MinSearch) and (dy <= MaxSearch) and
    (dy >= MinSearch) then
  begin
    FAStarFigure := Figure;
    FAstarAvoidFigure := Figure.AvoidInPath;
    FStartX := CellWidth * Trunc(2 * Figure.StepX / FMap.StripWidth) +
      (FMap.StripWidth shr 2);
    FStartY := CellHeight * Trunc(2 * Figure.StepY / FMap.StripHeight) +
      (FMap.StripHeight shr 2);

    FAStar.Deviance := Figure.PathDeviance;
    if (not Assigned(FAstarAvoidFigure)) or (FAstarAvoidFigure.Count = 0) then
    begin
      if not CanMove(Dx, dy, Dx, dy) then
      begin
        FAStar.Deviance := 16;
      end;
    end;

    Figure.PathCount := FAStar.FindPath(0, 0, Dx, dy, Figure.PathHandle);
    if (Figure.PathCount > 0) and (Figure.PathHandle <> 0) then
    begin
      pBase := GlobalLock(Figure.PathHandle);
      p := pBase;
{$IFDEF DEBUG}
      RefreshMap;
      MapBuffer.Canvas.Brush.Color := $FF00;
      MapBuffer.Canvas.Ellipse(Figure.FX - OffsetX + MapOffsetX - Figure.Radius,
        Figure.FY - OffsetY + MapOffsetY - Figure.Radius div 2,
        Figure.FX - OffsetX + MapOffsetX + Figure.Radius,
        Figure.FY - OffsetY + MapOffsetY + Figure.Radius div 2);
      MapBuffer.Canvas.Brush.Color := 255;
{$ENDIF}
      for i := 1 to Figure.PathCount do
      begin
        p^.X := p^.X * CellWidth + FStartX;
        p^.Y := p^.Y * CellHeight + FStartY;
{$IFDEF DEBUG}
        MapBuffer.Canvas.Ellipse(p^.X - OffsetX + MapOffsetX - Figure.Radius,
          p^.Y - OffsetY + MapOffsetY - Figure.Radius div 2,
          p^.X - OffsetX + MapOffsetX + Figure.Radius,
          p^.Y - OffsetY + MapOffsetY + Figure.Radius div 2);
{$ENDIF}
        Inc(p);
      end;
      Figure.FDestX := pBase^.X;
      Figure.FDestY := pBase^.Y;
      Figure.FDestZ := Figure.FZ;
      Figure.GotPath := True;
      Figure.Terminal := True;
      Figure.Moving := True;
      Figure.PathStep := 1;
      Dx := Figure.DestX - Figure.FX;
      dy := 2 * (Figure.DestY - Figure.FY);
      D := sqrt(sqr(Dx) + sqr(dy));
      if D <> 0 then
      begin
        Figure.FSlopeX := Dx / D;
        Figure.FSlopeY := dy / (2 * D);
      end;
      GlobalUnlock(Figure.PathHandle);
      if Assigned(Figure.OnPathStep) then
        Figure.OnPathStep(Figure, Figure.DestX, Figure.DestY);
    end
    else
    begin
      Figure.GotPath := false;
      if Assigned(Figure.OnNoPath) then
        Figure.OnNoPath(Figure);
    end;
  end
  else
  begin
    Figure.GotPath := false;
    if Assigned(Figure.OnNoPath) then
      Figure.OnNoPath(Figure);
  end;
end;

procedure TAniView.MoveFigure(Figure: TAniFigure);
const
  Tol = 1 / 256;
var
  Dx, dy, dZ, D: double;
  DestX, DestY, DestZ: double;
  Gx, Gy: Integer;
  Gx2, Gy2: Integer;
  R, G: double;
  p: array [0 .. 3] of TPoint;
{$IFDEF DEBUG}
  p1: array [0 .. 3] of TPoint;
{$ENDIF}
  Top, Left, Right, Bottom, Temp: Integer;
  XL1, YL1, XL2, YL2: Longint;
  XR1, YR1, XR2, YR2: Longint;
  X, Y, XL, XR, ScanY, TileRow, TileRowOffset: Longint;
  dXL, dYL, dXR, dYR: Longint;
  ModeL, ModeR: Boolean;
  GridBase, GridLoc: ^GridInfo;
  CollisionMask: Word;
  A, B, C, Q, T, T1, T2: double;
  cx, cy: Integer;
  i, j: Integer;
  edge: Longint;
  Stop: Boolean;
  InitSeq: Boolean;
  OldTriggerID: SmallInt;
  OldFilterID: SmallInt;
  Point: ^TPoint;
  S: single;
  StepX, StepY: double;
  NextFigure, TempFigure: TAniFigure;
  RemX, RemY: Integer;
  BitMask: Word;
  XRayOn: Boolean;
begin
  Top := 0;
  Bottom := 0;
  Dx := Figure.DestX - Figure.StepX;
  dy := 2 * (Figure.DestY - Figure.StepY);
  dZ := Figure.DestZ - Figure.StepZ;
  D := sqrt(sqr(Dx) + sqr(dy) + sqr(dZ));
  if ((D <= 0) or (D <= Figure.Speed)) and Figure.Terminal then
  begin
    if (Figure.GotPath) then
    begin
      DestZ := Figure.StepZ;
      if (Figure.PathStep >= Figure.PathCount) then
      begin
        DestX := Figure.DestX;
        DestY := Figure.DestY;
        Figure.Moving := false;
        if Assigned(Figure.OnStop) then
          Figure.OnStop(Figure);
      end
      else
      begin
        Point := GlobalLock(Figure.PathHandle);
        Inc(Point, Figure.PathStep);
        S := Figure.Speed;
        repeat
          if (Figure.PathStep >= Figure.PathCount) then
            Break;
          StepX := Figure.DestX;
          StepY := Figure.DestY;
          S := S - D;
          Figure.FDestX := Point^.X;
          Figure.FDestY := Point^.Y;
          Dx := Figure.DestX - StepX;
          dy := 2 * (Figure.DestY - StepY);
          D := sqrt(sqr(Dx) + sqr(dy));
          Inc(Figure.PathStep);
          Inc(Point);
        until (S <= D);
        GlobalUnlock(Figure.PathHandle);
        if (S <= D) then
        begin
          Figure.FSlopeX := Dx / D;
          Figure.FSlopeY := dy / (2 * D);
          DestX := StepX + S * Figure.SlopeX;
          DestY := StepY + S * Figure.SlopeY;
          DestZ := Figure.DestZ;
          if Assigned(Figure.OnPathStep) then
            Figure.OnPathStep(Figure, Figure.DestX, Figure.DestY);
        end
        else
        begin
          DestX := Figure.DestX;
          DestY := Figure.DestY;
          Figure.Moving := false;
          if Assigned(Figure.OnStop) then
            Figure.OnStop(Figure);
        end;
      end;
    end
    else
    begin
      DestX := Figure.DestX;
      DestY := Figure.DestY;
      DestZ := Figure.DestZ;
      Figure.Moving := false;
      if Assigned(Figure.OnStop) then
        Figure.OnStop(Figure);
    end;
  end
  else
  begin
    DestX := Figure.StepX + Figure.Speed * Figure.SlopeX;
    DestY := Figure.StepY + Figure.Speed * Figure.SlopeY;
    DestZ := Figure.StepZ + Figure.Speed * Figure.SlopeZ;
  end;

  if Assigned(FMap) then
  begin
    // Collision detection
    GridBase := GlobalLock(FMap.MapData);
    GridLoc := GridBase;
    if (Figure.MapOffset >= 0) then
    begin
      // Remove figure from collision chain
      Inc(GridLoc, (Figure.MapOffset));
      NextFigure := GridLoc^.Figure;

      RemX := Figure.FPrevX mod FMap.TileWidth;
      RemY := FMap.TileHeight - (Figure.FPrevY mod FMap.TileHeight) - 1;
      BitMask := 1 shl ((RemY div FMap.StripHeight) * 4 +
        (RemX div FMap.StripWidth));
      if (GridLoc^.TriggerMask and BitMask) > 0 then
        OldTriggerID := GridLoc^.TriggerID
      else
        OldTriggerID := 0;
      if (GridLoc^.FilterMask and BitMask) > 0 then
        OldFilterID := GridLoc^.FilterID
      else
        OldFilterID := 0;

      if (NextFigure = Figure) then
      begin
        GridLoc^.Figure := Figure.NextInTile;
      end
      else
      begin
        TempFigure := NextFigure;
        while Assigned(NextFigure) do
        begin
          NextFigure := NextFigure.NextInTile;
          if (NextFigure = Figure) then
          begin
            TempFigure.NextInTile := NextFigure.NextInTile;
            Break;
          end;
          TempFigure := NextFigure;
        end;
      end;
      Figure.NextInTile := nil;
    end
    else
    begin
      OldTriggerID := 0;
      OldFilterID := 0;
    end;

    Dx := DestX - Figure.StepX;
    dy := 2 * (DestY - Figure.StepY);
    dZ := DestZ - Figure.StepZ;
    if (Dx <> 0) or (dy <> 0) then
    begin
      D := sqrt(sqr(Dx) + sqr(dy));
      T := 1;
      // Collision with map boundaries
      if (Dx <> 0) then
      begin
        edge := Figure.Radius + FMap.TileWidth;
        if (DestX < edge) then
        begin
          T1 := (edge - Figure.StepX) / Dx;
          if (T1 < T) then
            T := T1;
        end
        else
        begin
          edge := FMap.BitWidth - Figure.Radius - FMap.TileWidth;
          if (DestX > edge) then
          begin
            T1 := (edge - Figure.StepX) / Dx;
            if (T1 < T) then
              T := T1;
          end;
        end;
      end;
      if (dy <> 0) then
      begin
        edge := Figure.Radius + FMap.TileHeight;
        if (DestY < edge) then
        begin
          T1 := (edge - Figure.StepY) / dy;
          if (T1 < T) then
            T := T1;
        end
        else
        begin
          edge := FMap.BitHeight - Figure.Radius - FMap.TileHeight;
          if (DestY > edge) then
          begin
            T1 := (edge - Figure.StepY) / dy;
            if (T1 < T) then
              T := T1;
          end;
        end;
      end;
      if (T < 1) then
      begin
        if Assigned(Figure.OnCollideBoundary) then
          Figure.OnCollideBoundary(Figure);
      end;

      if Assigned(Figure.OnCollideFigure) then
      begin
        // Collisions with other figures
        InitSeq := false;
        R := Figure.Radius + FMaxCollisionRadius;
        G := R / D;
        Gx := Round(G * Dx);
        Gy := Round(G * dy);
        Gx2 := Gx div 2;
        Gy2 := Gy div 2;

        X := Round(Figure.StepX);
        Y := Round(Figure.StepY);
        p[0].X := X - Gy;
        p[0].Y := Y + Gx2;
        p[1].X := X + Gy;
        p[1].Y := Y - Gx2;
        X := Round(DestX);
        Y := Round(DestY);
        p[2].X := X + Gy + Gx;
        p[2].Y := Y - Gx2 + Gy2;
        p[3].X := X - Gy + Gx;
        p[3].Y := Y + Gx2 + Gy2;

        { p1[0].X:=p[0].X-OffsetX;
          p1[0].Y:=p[0].Y-OffsetY;
          p1[1].X:=p[1].X-OffsetX;
          p1[1].Y:=p[1].Y-OffsetY;
          p1[2].X:=p[2].X-OffsetX;
          p1[2].Y:=p[2].Y-OffsetY;
          p1[3].X:=p[3].X-OffsetX;
          p1[3].Y:=p[3].Y-OffsetY;
          FrameBuffer.canvas.Pen.color:=clBlack;
          Polygon(FrameBuffer.canvas.handle,p1[0],4); }

        if (p[0].Y < p[1].Y) then
          Top := 0
        else
          Top := 1;
        if (p[2].Y < p[Top].Y) then
          Top := 2;
        if (p[3].Y < p[Top].Y) then
          Top := 3;
        Bottom := (Top + 2) mod 4;
        Left := (Top + 1) mod 4;
        Right := (Top + 3) mod 4;
        if (p[Left].X > p[Right].X) then
        begin
          Temp := Left;
          Left := Right;
          Right := Temp;
        end;
        // Roundoff error
        Dec(p[Top].Y);
        Dec(p[Left].X);
        Inc(p[Bottom].Y);
        Inc(p[Right].X);

        if (p[Top].Y < 0) then
          TileRow := 0
        else
          TileRow := p[Top].Y div FMap.TileHeight;
        ScanY := (TileRow + 1) * FMap.TileHeight;
        TileRowOffset := TileRow * FMap.Width;

        XL1 := p[Top].X;
        YL1 := p[Top].Y;
        XL2 := p[Left].X;
        YL2 := p[Left].Y;
        XR1 := XL1;
        YR1 := YL1;
        XR2 := p[Right].X;
        YR2 := p[Right].Y;
        dXL := XL2 - XL1;
        dYL := YL2 - YL1;
        dXR := XR2 - XR1;
        dYR := YR2 - YR1;
        ModeL := True;
        ModeR := True;
        while (ScanY < FMap.BitHeight) do
        begin
          if (ModeL) then
          begin
            if (ScanY > p[Left].Y) then
            begin
              XL1 := p[Bottom].X;
              YL1 := p[Bottom].Y;
              dXL := XL2 - XL1;
              dYL := YL2 - YL1;
              ModeL := false;
              XL := XL2 div FMap.TileWidth;
            end
            else
              XL := (XL1 + dXL * (ScanY - YL1) div dYL) div FMap.TileWidth;
          end
          else
            XL := (XL1 + dXL * (ScanY - FMap.TileHeight - YL1) div dYL)
              div FMap.TileWidth;

          if (ModeR) then
          begin
            if (ScanY > p[Right].Y) then
            begin
              XR1 := p[Bottom].X;
              YR1 := p[Bottom].Y;
              dXR := XR2 - XR1;
              dYR := YR2 - YR1;
              ModeR := false;
              XR := XR2 div FMap.TileWidth;
            end
            else
              XR := (XR1 + dXR * (ScanY - YR1) div dYR) div FMap.TileWidth;
          end
          else
            XR := (XR1 + dXR * (ScanY - FMap.TileHeight - YR1) div dYR)
              div FMap.TileWidth;

          if (XL < 0) then
            XL := 0;
          if (XR >= FMap.Width) then
            XR := FMap.Width - 1;
          GridLoc := GridBase;
          Inc(GridLoc, TileRowOffset);
          Inc(GridLoc, XL);
          for X := XL to XR do
          begin
            // (*whew*) We've established there could be a collision, now let's find out
            // if there really is, and if so, where?
            NextFigure := GridLoc^.Figure;
            while Assigned(NextFigure) do
            begin
              if NextFigure.Radius > 0 then
              begin
                A := sqr(Dx) + sqr(dy);
                B := 2 * (Dx * (Figure.StepX - NextFigure.StepX) + 2 * dy *
                  (Figure.StepY - NextFigure.StepY));
                C := sqr(Figure.StepX - NextFigure.StepX) + 4 *
                  sqr(Figure.StepY - NextFigure.StepY) -
                  sqr(Figure.Radius + NextFigure.Radius);
                Q := sqr(B) - 4 * A * C;
                if (Q >= 0) then
                begin
                  T1 := (-B - sqrt(Q)) / (2 * A);
                  if (T1 < 1) and (T1 >= -Tol) then
                  begin
                    Stop := false;
                    Figure.OnCollideFigure(Figure, NextFigure, Stop);
                    if (Stop) then
                      if (T1 < T) then
                        T := T1;
                  end;
                end;
              end;
              NextFigure := NextFigure.NextInTile;
            end;
            Inc(GridLoc);
          end;
          if (ScanY >= p[Bottom].Y) then
            Break;
          Inc(TileRowOffset, FMap.Width);
          Inc(ScanY, FMap.TileHeight);
        end;
      end
      else
        InitSeq := True;

      // Collisions with map objects
      if Assigned(Figure.OnCollideItem) then
      begin
        T2 := T;
        R := Figure.Radius;
        G := R / D;
        Gx := Round(G * Dx);
        Gy := Round(G * dy);
        Gx2 := Gx div 2;
        Gy2 := Gy div 2;

        X := Round(Figure.StepX);
        Y := Round(Figure.StepY);
        p[0].X := X - Gy;
        p[0].Y := Y + Gx2;
        p[1].X := X + Gy;
        p[1].Y := Y - Gx2;
        X := Round(DestX);
        Y := Round(DestY);
        p[2].X := X + Gy + Gx;
        p[2].Y := Y - Gx2 + Gy2;
        p[3].X := X - Gy + Gx;
        p[3].Y := Y + Gx2 + Gy2;

        { p1[0].X:=p[0].X-OffsetX;
          p1[0].Y:=p[0].Y-OffsetY;
          p1[1].X:=p[1].X-OffsetX;
          p1[1].Y:=p[1].Y-OffsetY;
          p1[2].X:=p[2].X-OffsetX;
          p1[2].Y:=p[2].Y-OffsetY;
          p1[3].X:=p[3].X-OffsetX;
          p1[3].Y:=p[3].Y-OffsetY;
          FrameBuffer.canvas.Pen.color:=clBlack;
          Polygon(FrameBuffer.canvas.handle,p1[0],4); }

        if (InitSeq) then
        begin
          if (p[0].Y < p[1].Y) then
            Top := 0
          else
            Top := 1;
          if (p[2].Y < p[Top].Y) then
            Top := 2;
          if (p[3].Y < p[Top].Y) then
            Top := 3;
          Bottom := (Top + 2) mod 4;
          Left := (Top + 1) mod 4;
          Right := (Top + 3) mod 4;
          if (p[Left].X > p[Right].X) then
          begin
            Temp := Left;
            Left := Right;
            Right := Temp;
          end;
        end;
        // Roundoff error
        Dec(p[Top].Y);
        Dec(p[Left].X);
        Inc(p[Bottom].Y);
        Inc(p[Right].X);

        if (p[Top].Y < 0) then
          TileRow := 0
        else
          TileRow := p[Top].Y div FMap.TileHeight;
        ScanY := (TileRow + 1) * FMap.TileHeight;
        TileRowOffset := TileRow * FMap.Width;

        XL1 := p[Top].X;
        YL1 := p[Top].Y;
        XL2 := p[Left].X;
        YL2 := p[Left].Y;
        XR1 := XL1;
        YR1 := YL1;
        XR2 := p[Right].X;
        YR2 := p[Right].Y;
        dXL := XL2 - XL1;
        dYL := YL2 - YL1;
        dXR := XR2 - XR1;
        dYR := YR2 - YR1;
        ModeL := True;
        ModeR := True;
        while (ScanY < FMap.BitHeight) do
        begin
          if (ModeL) then
          begin
            if (ScanY > p[Left].Y) then
            begin
              XL1 := p[Bottom].X;
              YL1 := p[Bottom].Y;
              dXL := XL2 - XL1;
              dYL := YL2 - YL1;
              ModeL := false;
              XL := XL2 div FMap.TileWidth;
            end
            else
              XL := (XL1 + dXL * (ScanY - YL1) div dYL) div FMap.TileWidth;
          end
          else
            XL := (XL1 + dXL * (ScanY - FMap.TileHeight - YL1) div dYL)
              div FMap.TileWidth;

          if (ModeR) then
          begin
            if (ScanY > p[Right].Y) then
            begin
              XR1 := p[Bottom].X;
              YR1 := p[Bottom].Y;
              dXR := XR2 - XR1;
              dYR := YR2 - YR1;
              ModeR := false;
              XR := XR2 div FMap.TileWidth;
            end
            else
              XR := (XR1 + dXR * (ScanY - YR1) div dYR) div FMap.TileWidth;
          end
          else
            XR := (XR1 + dXR * (ScanY - FMap.TileHeight - YR1) div dYR)
              div FMap.TileWidth;

          if (XL < 0) then
            XL := 0;
          if (XR >= FMap.Width) then
            XR := FMap.Width - 1;
          GridLoc := GridBase;
          Inc(GridLoc, TileRowOffset);
          Inc(GridLoc, XL);
          for X := XL to XR do
          begin
            if Figure.UseLineOfSight then
              CollisionMask := GridLoc^.LineOfSightMask
            else
              CollisionMask := GridLoc^.CollisionMask;
            if (CollisionMask <> 0) then
            begin
              // (*whew*) We've established there could be a collision, now let's find out
              // if there really is, and if so, where?
              for j := 1 to 4 do
              begin
                cy := ScanY - (j - 1) * FMap.StripHeight -
                  (FMap.StripHeight shr 1);
                for i := 1 to 4 do
                begin
                  if ((CollisionMask and 1) = 1) then
                  begin
                    cx := X * FMap.TileWidth + (i - 1) * FMap.StripWidth +
                      (FMap.StripWidth shr 1);
                    A := sqr(Dx) + sqr(dy);
                    B := 2 * (Dx * (Figure.StepX - cx) + 2 * dy *
                      (Figure.StepY - cy));
                    C := sqr(Figure.StepX - cx) + 4 * sqr(Figure.StepY - cy) -
                      sqr(Figure.Radius + (FMap.StripWidth shr 1));
                    Q := sqr(B) - 4 * A * C;
                    if (Q >= 0) then
                    begin
                      T1 := (-B - sqrt(Q)) / (2 * A);
                      if (T1 > -Tol) and (T1 < T2) then
                        T2 := T1;
                    end;
                  end;
                  CollisionMask := CollisionMask shr 1;
                end;
              end;
            end;
            Inc(GridLoc);
          end;
          if (ScanY >= p[Bottom].Y) then
            Break;
          Inc(TileRowOffset, FMap.Width);
          Inc(ScanY, FMap.TileHeight);
        end;

        if (T2 < T) then
        begin
          Stop := false;
          Figure.OnCollideItem(Figure, Stop);
          if (Stop) then
            T := T2;
        end;
      end;

      if (not Figure.Moved) then
      begin // This clause allows an event to call SetPos
        // Figure has hit something, perform movement calculations
        if (T < 1) then
        begin
          Figure.Moving := false;
          DestX := Figure.StepX + Dx * T;
          DestY := Figure.StepY + dy * T / 2;
          DestZ := Figure.StepZ + dZ * T;
          if Assigned(Figure.OnStop) then
            Figure.OnStop(Figure);
        end;
      end;
    end;

    if (not Figure.Moved) then
    begin // This if allows an event to call SetPos
      Figure.FStepX := DestX;
      Figure.FStepY := DestY;
      Figure.FStepZ := DestZ;
      Figure.FPrevX := Figure.FX;
      Figure.FPrevY := Figure.FY;
      Figure.FPrevZ := Figure.FZ;
      Figure.FX := Round(DestX);
      Figure.FY := Round(DestY);
      Figure.FZ := Round(DestZ);
      Figure.MapOffset := (Figure.FY div FMap.TileHeight) * FMap.Width +
        (Figure.FX div FMap.TileWidth);
      GridLoc := GridBase;
      Inc(GridLoc, (Figure.MapOffset));
      NextFigure := GridLoc^.Figure;
      GridLoc^.Figure := Figure;
      Figure.NextInTile := NextFigure;
      Figure.Zone := GridLoc^.Zone[0];
      Figure.FTile := PGridInfo(GridLoc);
      if (GridLoc^.TriggerID <> OldTriggerID) then
      begin
        if Assigned(Figure.OnTrigger) then
        begin
          if (GridLoc^.TriggerID = 0) or (GridLoc^.TriggerMask = $FFFF) then
          begin
            Figure.OnTrigger(Figure, GridLoc^.TriggerID, OldTriggerID);
          end
          else
          begin
            RemX := Figure.FX div FMap.TileWidth;
            RemY := FMap.TileHeight - (Figure.FY mod FMap.TileHeight);
            BitMask := 1 shl ((RemY div FMap.StripHeight) * 4 +
              (RemX div FMap.StripWidth));
            if (GridLoc^.TriggerMask and BitMask) > 0 then
              Figure.OnTrigger(Figure, GridLoc^.TriggerID, OldTriggerID);
          end;

        end;
      end;
      if (GridLoc^.FilterID <> OldFilterID) then
      begin
        if Assigned(Figure.OnFilter) then
        begin
          if (GridLoc^.FilterID = 0) or (GridLoc^.FilterMask = $FFFF) then
          begin
            Figure.OnFilter(Figure, GridLoc^.FilterID, OldFilterID);
          end
          else
          begin
            RemX := Figure.FX div FMap.TileWidth;
            RemY := FMap.TileHeight - (Figure.FY mod FMap.TileHeight);
            BitMask := 1 shl ((RemY div FMap.StripHeight) * 4 +
              (RemX div FMap.StripWidth));
            if (GridLoc^.FilterMask and BitMask) > 0 then
              Figure.OnFilter(Figure, GridLoc^.FilterID, OldFilterID);
          end;
        end;
      end;
      // Figure.FPrevX := Figure.FX;
      // Figure.FPrevY := Figure.FY;
      // Figure.FPrevZ := Figure.FZ;
    end;
    GlobalUnlock(FMap.MapData);
  end;
end;

function TAniView.ClearShot(SrcX, SrcY, DstX, DstY, Radius: Longint;
  UseLineOfSight: Boolean): Boolean;
const
  Tol = 1 / 256;
var
  Dx, dy, dZ, D: double;
  DestX, DestY, DestZ: double;
  Gx, Gy: Integer;
  Gx2, Gy2: Integer;
  R, G: double;
  p: array [0 .. 3] of TPoint;
  Top, Left, Right, Bottom, Temp: Integer;
  XL1, YL1, XL2, YL2: Longint;
  XR1, YR1, XR2, YR2: Longint;
  X, Y, XL, XR, ScanY, TileRow, TileRowOffset: Longint;
  dXL, dYL, dXR, dYR: Longint;
  ModeL, ModeR: Boolean;
  GridBase, GridLoc: ^GridInfo;
  CollisionMask: Word;
  A, B, C, Q, T, T1: double;
  cx, cy: Integer;
  i, j: Integer;
  edge: Longint;
  Stop: Boolean;
  Point: ^TPoint;
  NextFigure, TempFigure: TAniFigure;
begin
  Dx := DestX - SrcX;
  dy := 2 * (DestY - SrcY);
  D := sqrt(sqr(Dx) + sqr(dy));
  if D <= 0 then
  begin
    Result := True;
    Exit;
  end;

  if Assigned(FMap) then
  begin
    // Collision detection
    GridBase := GlobalLock(FMap.MapData);
    try
      if (Dx <> 0) or (dy <> 0) then
      begin
        D := sqrt(sqr(Dx) + sqr(dy));
        T := 1;
        // Collision with map boundaries
        if (Dx <> 0) then
        begin
          edge := Radius + FMap.TileWidth;
          if (DestX < edge) then
          begin
            T1 := (edge - SrcX) / Dx;
            if (T1 < T) then
            begin
              Result := false;
              Exit;
            end;
          end
          else
          begin
            edge := FMap.BitWidth - Radius - FMap.TileWidth;
            if (DestX > edge) then
            begin
              T1 := (edge - SrcX) / Dx;
              if (T1 < T) then
              begin
                Result := false;
                Exit;
              end;
            end;
          end;
        end;
        if (dy <> 0) then
        begin
          edge := Radius + FMap.TileHeight;
          if (DestY < edge) then
          begin
            T1 := (edge - SrcY) / dy;
            if (T1 < T) then
            begin
              Result := false;
              Exit;
            end;
          end
          else
          begin
            edge := FMap.BitHeight - Radius - FMap.TileHeight;
            if (DestY > edge) then
            begin
              T1 := (edge - SrcY) / dy;
              if (T1 < T) then
              begin
                Result := false;
                Exit;
              end;
            end;
          end;
        end;

        // Collisions with other figures
        R := Radius + FMaxCollisionRadius;
        G := R / D;
        Gx := Round(G * Dx);
        Gy := Round(G * dy);
        Gx2 := Gx div 2;
        Gy2 := Gy div 2;

        X := SrcX;
        Y := SrcY;
        p[0].X := X - Gy;
        p[0].Y := Y + Gx2;
        p[1].X := X + Gy;
        p[1].Y := Y - Gx2;
        X := Round(DestX);
        Y := Round(DestY);
        p[2].X := X + Gy + Gx;
        p[2].Y := Y - Gx2 + Gy2;
        p[3].X := X - Gy + Gx;
        p[3].Y := Y + Gx2 + Gy2;

        if (p[0].Y < p[1].Y) then
          Top := 0
        else
          Top := 1;
        if (p[2].Y < p[Top].Y) then
          Top := 2;
        if (p[3].Y < p[Top].Y) then
          Top := 3;
        Bottom := (Top + 2) mod 4;
        Left := (Top + 1) mod 4;
        Right := (Top + 3) mod 4;
        if (p[Left].X > p[Right].X) then
        begin
          Temp := Left;
          Left := Right;
          Right := Temp;
        end;
        // Roundoff error
        Dec(p[Top].Y);
        Dec(p[Left].X);
        Inc(p[Bottom].Y);
        Inc(p[Right].X);

        if (p[Top].Y < 0) then
          TileRow := 0
        else
          TileRow := p[Top].Y div FMap.TileHeight;
        ScanY := (TileRow + 1) * FMap.TileHeight;
        TileRowOffset := TileRow * FMap.Width;

        XL1 := p[Top].X;
        YL1 := p[Top].Y;
        XL2 := p[Left].X;
        YL2 := p[Left].Y;
        XR1 := XL1;
        YR1 := YL1;
        XR2 := p[Right].X;
        YR2 := p[Right].Y;
        dXL := XL2 - XL1;
        dYL := YL2 - YL1;
        dXR := XR2 - XR1;
        dYR := YR2 - YR1;
        ModeL := True;
        ModeR := True;
        while (ScanY < FMap.BitHeight) do
        begin
          if (ModeL) then
          begin
            if (ScanY > p[Left].Y) then
            begin
              XL1 := p[Bottom].X;
              YL1 := p[Bottom].Y;
              dXL := XL2 - XL1;
              dYL := YL2 - YL1;
              ModeL := false;
              XL := XL2 div FMap.TileWidth;
            end
            else
              XL := (XL1 + dXL * (ScanY - YL1) div dYL) div FMap.TileWidth;
          end
          else
            XL := (XL1 + dXL * (ScanY - FMap.TileHeight - YL1) div dYL)
              div FMap.TileWidth;

          if (ModeR) then
          begin
            if (ScanY > p[Right].Y) then
            begin
              XR1 := p[Bottom].X;
              YR1 := p[Bottom].Y;
              dXR := XR2 - XR1;
              dYR := YR2 - YR1;
              ModeR := false;
              XR := XR2 div FMap.TileWidth;
            end
            else
              XR := (XR1 + dXR * (ScanY - YR1) div dYR) div FMap.TileWidth;
          end
          else
            XR := (XR1 + dXR * (ScanY - FMap.TileHeight - YR1) div dYR)
              div FMap.TileWidth;

          if (XL < 0) then
            XL := 0;
          if (XR >= FMap.Width) then
            XR := FMap.Width - 1;
          GridLoc := GridBase;
          Inc(GridLoc, TileRowOffset);
          Inc(GridLoc, XL);
          for X := XL to XR do
          begin
            NextFigure := GridLoc^.Figure;
            while Assigned(NextFigure) do
            begin
              if NextFigure.Radius > 0 then
              begin
                A := sqr(Dx) + sqr(dy);
                B := 2 * (Dx * (SrcX - NextFigure.StepX) + 2 * dy *
                  (SrcY - NextFigure.StepY));
                C := sqr(SrcX - NextFigure.StepX) + 4 *
                  sqr(SrcY - NextFigure.StepY) -
                  sqr(Radius + NextFigure.Radius);
                Q := sqr(B) - 4 * A * C;
                if (Q >= 0) then
                begin
                  T1 := (-B - sqrt(Q)) / (2 * A);
                  if (T1 < 1) and (T1 >= -Tol) then
                  begin
                    Result := false;
                    Exit;
                  end;
                end;
              end;
              NextFigure := NextFigure.NextInTile;
            end;
            Inc(GridLoc);
          end;
          if (ScanY >= p[Bottom].Y) then
            Break;
          Inc(TileRowOffset, FMap.Width);
          Inc(ScanY, FMap.TileHeight);
        end;

        // Collisions with map objects
        R := Radius;
        G := R / D;
        Gx := Round(G * Dx);
        Gy := Round(G * dy);
        Gx2 := Gx div 2;
        Gy2 := Gy div 2;

        X := SrcX;
        Y := SrcY;
        p[0].X := X - Gy;
        p[0].Y := Y + Gx2;
        p[1].X := X + Gy;
        p[1].Y := Y - Gx2;
        X := Round(DestX);
        Y := Round(DestY);
        p[2].X := X + Gy + Gx;
        p[2].Y := Y - Gx2 + Gy2;
        p[3].X := X - Gy + Gx;
        p[3].Y := Y + Gx2 + Gy2;

        // Roundoff error
        Dec(p[Top].Y);
        Dec(p[Left].X);
        Inc(p[Bottom].Y);
        Inc(p[Right].X);

        if (p[Top].Y < 0) then
          TileRow := 0
        else
          TileRow := p[Top].Y div FMap.TileHeight;
        ScanY := (TileRow + 1) * FMap.TileHeight;
        TileRowOffset := TileRow * FMap.Width;

        XL1 := p[Top].X;
        YL1 := p[Top].Y;
        XL2 := p[Left].X;
        YL2 := p[Left].Y;
        XR1 := XL1;
        YR1 := YL1;
        XR2 := p[Right].X;
        YR2 := p[Right].Y;
        dXL := XL2 - XL1;
        dYL := YL2 - YL1;
        dXR := XR2 - XR1;
        dYR := YR2 - YR1;
        ModeL := True;
        ModeR := True;
        while (ScanY < FMap.BitHeight) do
        begin
          if (ModeL) then
          begin
            if (ScanY > p[Left].Y) then
            begin
              XL1 := p[Bottom].X;
              YL1 := p[Bottom].Y;
              dXL := XL2 - XL1;
              dYL := YL2 - YL1;
              ModeL := false;
              XL := XL2 div FMap.TileWidth;
            end
            else
              XL := (XL1 + dXL * (ScanY - YL1) div dYL) div FMap.TileWidth;
          end
          else
            XL := (XL1 + dXL * (ScanY - FMap.TileHeight - YL1) div dYL)
              div FMap.TileWidth;

          if (ModeR) then
          begin
            if (ScanY > p[Right].Y) then
            begin
              XR1 := p[Bottom].X;
              YR1 := p[Bottom].Y;
              dXR := XR2 - XR1;
              dYR := YR2 - YR1;
              ModeR := false;
              XR := XR2 div FMap.TileWidth;
            end
            else
              XR := (XR1 + dXR * (ScanY - YR1) div dYR) div FMap.TileWidth;
          end
          else
            XR := (XR1 + dXR * (ScanY - FMap.TileHeight - YR1) div dYR)
              div FMap.TileWidth;

          if (XL < 0) then
            XL := 0;
          if (XR >= FMap.Width) then
            XR := FMap.Width - 1;
          GridLoc := GridBase;
          Inc(GridLoc, TileRowOffset);
          Inc(GridLoc, XL);
          for X := XL to XR do
          begin
            if UseLineOfSight then
              CollisionMask := GridLoc^.LineOfSightMask
            else
              CollisionMask := GridLoc^.CollisionMask;
            if (CollisionMask <> 0) then
            begin
              for j := 1 to 4 do
              begin
                cy := ScanY - (j - 1) * FMap.StripHeight -
                  (FMap.StripHeight shr 1);
                for i := 1 to 4 do
                begin
                  if ((CollisionMask and 1) = 1) then
                  begin
                    cx := X * FMap.TileWidth + (i - 1) * FMap.StripWidth +
                      (FMap.StripWidth shr 1);
                    A := sqr(Dx) + sqr(dy);
                    B := 2 * (Dx * (SrcX - cx) + 2 * dy * (SrcY - cy));
                    C := sqr(SrcX - cx) + 4 * sqr(SrcY - cy) -
                      sqr(Radius + (FMap.StripWidth shr 1));
                    Q := sqr(B) - 4 * A * C;
                    if (Q >= 0) then
                    begin
                      T1 := (-B - sqrt(Q)) / (2 * A);
                      if (T1 > -Tol) and (T1 < T) then
                      begin
                        Result := false;
                        Exit;
                      end;
                    end;
                  end;
                  CollisionMask := CollisionMask shr 1;
                end;
              end;
            end;
            Inc(GridLoc);
          end;
          if (ScanY >= p[Bottom].Y) then
            Break;
          Inc(TileRowOffset, FMap.Width);
          Inc(ScanY, FMap.TileHeight);
        end;
      end;
    finally
      GlobalUnlock(FMap.MapData);
    end;
  end;
  Result := True;
end;

procedure TAniView.DisableFigure(Figure: TAniFigure);
var
  GridBase, GridLoc: ^GridInfo;
  NextFigure, TempFigure: TAniFigure;
begin
  if Assigned(FMap) then
  begin
    // Collision detection
    GridBase := GlobalLock(FMap.MapData);
    GridLoc := GridBase;
    if (Figure.MapOffset >= 0) then
    begin
      // Remove figure from collision chain
      Inc(GridLoc, (Figure.MapOffset));
      NextFigure := GridLoc^.Figure;
      if (NextFigure = Figure) then
      begin
        GridLoc^.Figure := Figure.NextInTile;
      end
      else
      begin
        TempFigure := NextFigure;
        while Assigned(NextFigure) do
        begin
          NextFigure := NextFigure.NextInTile;
          if (NextFigure = Figure) then
          begin
            TempFigure.NextInTile := NextFigure.NextInTile;
            Break;
          end;
          TempFigure := NextFigure;
        end;
      end;
      Figure.NextInTile := nil;
    end;
    GlobalUnlock(FMap.MapData);
  end;
  Figure.ViewEnabled := false;
end;

procedure TAniView.TransFigure(Figure: TAniFigure);
var
  GridBase, GridLoc: ^GridInfo;
  OldTriggerID: SmallInt;
  OldFilterID: SmallInt;
  NextFigure, TempFigure: TAniFigure;
  RemX, RemY: Integer;
  BitMask: Word;
begin
  Figure.Moved := false;
  if Assigned(FMap) then
  begin
    GridBase := GlobalLock(FMap.MapData);
    GridLoc := GridBase;
    if (Figure.MapOffset >= 0) then
    begin
      // Remove figure from collision chain
      Inc(GridLoc, (Figure.MapOffset));
      NextFigure := GridLoc^.Figure;
      RemX := Figure.FPrevX mod FMap.TileWidth;
      RemY := FMap.TileHeight - (Figure.FPrevY mod FMap.TileHeight) - 1;
      BitMask := 1 shl ((RemY div FMap.StripHeight) * 4 +
        (RemX div FMap.StripWidth));
      if (GridLoc^.TriggerMask and BitMask) > 0 then
        OldTriggerID := GridLoc^.TriggerID
      else
        OldTriggerID := 0;
      if (GridLoc^.FilterMask and BitMask) > 0 then
        OldFilterID := GridLoc^.FilterID
      else
        OldFilterID := 0;
      if (NextFigure = Figure) then
      begin
        GridLoc^.Figure := Figure.NextInTile;
      end
      else
      begin
        TempFigure := NextFigure;
        while Assigned(NextFigure) do
        begin
          NextFigure := NextFigure.NextInTile;
          if (NextFigure = Figure) then
          begin
            TempFigure.NextInTile := NextFigure.NextInTile;
            Break;
          end;
          TempFigure := NextFigure;
        end;
      end;
      Figure.NextInTile := nil;
    end
    else
    begin
      OldTriggerID := 0;
      OldFilterID := 0;
    end;

    Figure.MapOffset := (Figure.FY div FMap.TileHeight) * FMap.Width +
      (Figure.FX div FMap.TileWidth);
    if (Figure.MapOffset < 0) then
      Figure.MapOffset := 0;
    if Figure.MapOffset >= FMap.Height * FMap.Width then
      Figure.MapOffset := FMap.Height * FMap.Width - 1;
    GridLoc := GridBase;
    Inc(GridLoc, (Figure.MapOffset));
    NextFigure := GridLoc^.Figure;
    GridLoc^.Figure := Figure;
    Figure.NextInTile := NextFigure;
    Figure.Zone := GridLoc^.Zone[0];
    Figure.FTile := PGridInfo(GridLoc);
    if (GridLoc^.TriggerID <> OldTriggerID) then
    begin
      if Assigned(Figure.OnTrigger) then
      begin
        if (GridLoc^.TriggerID = 0) or (GridLoc^.TriggerMask = $FFFF) then
        begin
          Figure.OnTrigger(Figure, GridLoc^.TriggerID, OldTriggerID);
        end
        else
        begin
          RemX := Figure.FX div FMap.TileWidth;
          RemY := FMap.TileHeight - (Figure.FY mod FMap.TileHeight);
          BitMask := 1 shl ((RemY div FMap.StripHeight) * 4 +
            (RemX div FMap.StripWidth));
          if (GridLoc^.TriggerMask and BitMask) > 0 then
            Figure.OnTrigger(Figure, GridLoc^.TriggerID, OldTriggerID);
        end;

      end;
    end;
    if (GridLoc^.FilterID <> OldFilterID) then
    begin
      if Assigned(Figure.OnFilter) then
      begin
        if (GridLoc^.FilterID = 0) or (GridLoc^.FilterMask = $FFFF) then
        begin
          Figure.OnFilter(Figure, GridLoc^.FilterID, OldFilterID);
        end
        else
        begin
          RemX := Figure.FX div FMap.TileWidth;
          RemY := FMap.TileHeight - (Figure.FY mod FMap.TileHeight);
          BitMask := 1 shl ((RemY div FMap.StripHeight) * 4 +
            (RemX div FMap.StripWidth));
          if (GridLoc^.FilterMask and BitMask) > 0 then
            Figure.OnFilter(Figure, GridLoc^.FilterID, OldFilterID);
        end;
      end;
    end;
    Figure.FPrevX := Figure.FX;
    Figure.FPrevY := Figure.FY;
    Figure.FPrevZ := Figure.FZ;
    GlobalUnlock(FMap.MapData);
  end;
end;

procedure TAniView.ComputeLight(Figure: TAniFigure);
var
  i, j: Integer;
  X1, Y1, Z1: Longint;
  IL1, D: double;
  R1, G1, B1: double;
  RL, GL, BL: Integer;
  Test: TLightZone;
begin
  Figure.LightComputed := FRameCount;
  R1 := FMap.LightR;
  G1 := FMap.LightG;
  B1 := FMap.LightB;
  if (FMap.Zones[Figure.Zone] is TLightZone) then
  begin
    j := 0;
    for i := 0 to TLightZone(FMap.Zones[Figure.Zone]).OverlapZones.Count - 1 do
    begin
      Test := TLightZone(FMap.Zones[Figure.Zone]).OverlapZones[i];
      X1 := sqr(Test.FlickerX[Test.State] - Figure.FX);
      Y1 := sqr((Test.FlickerY[Test.State] - Figure.FY) * 2);
      Z1 := sqr(Test.FlickerZ[Test.State] - (Figure.Height div 2));
      D := sqrt(X1 + Y1 + Z1) / Test.FlickerRadius[Test.State];
      if D <= 1 then
      begin
        if FMap.LineOfSight(Test.FlickerX[Test.State],
          Test.FlickerY[Test.State], Figure.FX, Figure.FY) then
        begin
          RL := Test.Color and $FF;
          GL := Test.Color and $FF00 shr 8;
          BL := Test.Color and $FF0000 shr 16;
          IL1 := (1 - D) * Test.FlickerIntensity[Test.State] / 100;
          Figure.EnumLightSource(j, Test.FlickerX[Test.State],
            Test.FlickerY[Test.State], Test.FlickerZ[Test.State], IL1,
            Test.FlickerRadius[Test.State]);
          Inc(j);
          R1 := R1 + IL1 * RL;
          G1 := G1 + IL1 * GL;
          B1 := B1 + IL1 * BL;
        end;
      end;
    end;
  end;
  Figure.LightR := Round(R1);
  Figure.LightG := Round(G1);
  Figure.LightB := Round(B1);
  if (R1 >= G1) and (R1 >= B1) then
    Figure.FLightIndex := R1 / 255
  else if (G1 >= B1) then
    Figure.FLightIndex := G1 / 255
  else
    Figure.FLightIndex := B1 / 255;
end;

procedure TAniView.DrawFigure(Figure: TAniFigure);
begin
  Figure.FPosX := Figure.FX - Figure.CenterX - OffsetX;
  if (Figure.FPosX + Figure.Width < Left) or (Figure.FPosX >= Left + Width) then
    Exit;
  Figure.FPosY := Figure.FY - Figure.CenterY - Figure.FZ - OffsetY;
  if (Figure.UseLighting) and FMap.UseLighting then
  begin
    ComputeLight(Figure);
  end
  else
  begin
    Figure.LightR := 255;
    Figure.LightG := 255;
    Figure.LightB := 255;
  end;
  Figure.FOnScreen := True;
  Figure.Render;
end;

procedure TAniView.SetInterval(PInterval: Word);
begin
  FInterval := PInterval;
  if Assigned(Timer) then
    Timer.Interval := PInterval;
end;

//
procedure TAniView.SetActive(VActive: Boolean);
begin
  FActive := VActive;
  if FActive then
  begin
    if not Assigned(Timer) then
    begin
      Timer := TAniTimer.Create(nil);
      Timer.Interval := FInterval;
      Timer.TimerPriority := tpNormal;
      Timer.Resolution := 1;
    end;
    Timer.OnTimer := FDrawFrame;
    Timer.Enabled := FActive;
  end
  else
  begin
    if Assigned(Timer) then
    begin
      Timer.OnTimer := nil;
      Timer.Enabled := FActive;
    end;
  end;
end;

procedure TAniView.SetItemMask(Mask: Longint);
begin
  if (FItemMask <> Mask) then
  begin
    FItemMask := Mask;
    FRefreshMap;
  end;
end;

procedure TAniView.SetShowRepaint(const Value: Boolean);
begin
  FShowRepaint := Value;
  if FShowRepaint then
    RepaintCode := DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT
  else
    RepaintCode := DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT;
end;

procedure TAniView.SetAutoTransparentMask(const Value: TBitmap);
begin
  // TODO: Refactor
  if Assigned(Value) then
  begin
    XRayImage := nil;
    XRayImage := SoAOS_DX_SurfaceFromBMP(Value, clBlack);
    XRayWidth := Value.Width;
    XRayHeight := Value.Height;
  end
  else
  begin
    XRayImage := nil;
    XRayWidth := 0;
    XRayHeight := 0;
  end;
end;

function TAniView.FindInRadius(X, Y: Longint; Radius: single): TList;
var
  i: Integer;
  Dx, dy: Longint;
  R, hR: single;
begin
  Result := nil;
  for i := 0 to FigureList.Count - 1 do
  begin
    if FigureList[i].Enabled then
    begin
      R := Radius + FigureList[i].Radius;
      Dx := FigureList[i].X - X;
      if (Dx <= R) and (Dx >= -R) then
      begin
        dy := FigureList[i].Y - Y;
        hR := R / 2;
        if (dy <= hR) and (dy >= -hR) then
        begin
          FigureList[i].FDistance := sqrt(sqr(Dx) + sqr(2 * dy));
          if FigureList[i].Distance <= R then
          begin
            if not Assigned(Result) then
              Result := TList.Create;
            Result.Add(FigureList[i])
          end;
        end;
      end;
    end;
  end;
end;

function TAniView.LineOfSight(X1, Y1, X2, Y2: Longint): Boolean;
begin
  if Assigned(FMap) then
    Result := FMap.LineOfSight(X1, Y1, X2, Y2)
  else
    Result := True;
end;

function TAniView.LineOfCollision(X1, Y1, X2, Y2: Longint): Boolean;
begin
  if Assigned(FMap) then
    Result := FMap.LineOfCollision(X1, Y1, X2, Y2)
  else
    Result := True;
end;

procedure Register;
begin
  RegisterComponents('Animation', [TAniView, TAniMap]);
  // RegisterComponents('Animation', [TAniView, TAniFigure, TAniMap]);
  // RegisterComponents('Animation', [TAniView, TAniFigure, TAniMap]);
  // RegisterPropertyEditor(TypeInfo(TAbout), TAniFigure, 'About', TAbout);
  // RegisterPropertyEditor(TypeInfo(TAbout), TAniMap, 'About', TAbout);
  // RegisterPropertyEditor(TypeInfo(TAbout), TAniView, 'About', TAbout);
  // RegisterComponentEditor(TAniFigure, TAniFigureEditor);
  // RegisterComponentEditor(TAniMap, TAniMapEditor);
end;

{ TAniFigure }

constructor TAniFigure.Create(X, Y, Z: Integer; Frame: Word; Enabled: Boolean);
begin
  inherited Create;
  FX := X;
  FY := Y;
  FZ := Z;
  FStepX := X;
  FStepY := Y;
  FStepZ := Z;
  FPrevX := X;
  FPrevY := Y;
  FPrevZ := Z;
  Moving := false;
  Terminal := false;
  Highlighted := false;
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
  if (PathHandle <> 0) then
  begin
    GlobalFree(PathHandle);
    PathHandle := 0;
  end;
  inherited Destroy;
end;

function TAniFigure.GetLightIndex: single;
begin
  if Assigned(FView.FMap) and (LightComputed <> FView.FRameCount) then
  begin
    FView.ComputeLight(Self);
  end;
  Result := FLightIndex;
end;

procedure TAniFigure.FindPathTo(X, Y: Integer; Avoid: TList<TAniFigure>;
  Deviance: Integer);
begin
  FStartX := FX;
  FStartY := FY;
  FStartZ := FZ;
  Moving := false;
  Terminal := false;
  NeedPath := True;
  GotPath := false;
  if (PathHandle <> 0) then
  begin
    GlobalFree(PathHandle);
    PathHandle := 0;
  end;
  FPathDestX := X;
  FPathDestY := Y;
  FSlopeZ := 0;
  AvoidInPath := Avoid;
  PathDeviance := Deviance;
end;

procedure TAniFigure.Move(X, Y, Z: Integer);
var
  D, Dx, dy, dZ: double;
begin
  FStartX := FX;
  FStartY := FY;
  FStartZ := FZ;
  Moving := True;
  Terminal := false;
  NeedPath := false;
  GotPath := false;
  if (PathHandle <> 0) then
  begin
    GlobalFree(PathHandle);
    PathHandle := 0;
  end;
  FDestX := X;
  FDestY := Y;
  FDestZ := Z;
  Dx := DestX - FX;
  dy := 2 * (DestY - FY);
  dZ := DestZ - FZ;
  D := sqrt(sqr(Dx) + sqr(dy) + sqr(dZ));
  if D <> 0 then
  begin
    FSlopeX := Dx / D;
    FSlopeY := dy / (2 * D);
    FSlopeZ := dZ / D;
  end;
end;

procedure TAniFigure.MoveTo(X, Y, Z: Integer);
var
  D, Dx, dy, dZ: double;
begin
  FStartX := FX;
  FStartY := FY;
  FStartZ := FZ;
  Moving := True;
  Terminal := True;
  NeedPath := false;
  GotPath := false;
  if (PathHandle <> 0) then
  begin
    GlobalFree(PathHandle);
    PathHandle := 0;
  end;
  FDestX := X;
  FDestY := Y;
  FDestZ := Z;
  Dx := DestX - FX;
  dy := 2 * (DestY - FY);
  dZ := DestZ - FZ;
  D := sqrt(sqr(Dx) + sqr(dy) + sqr(dZ));
  if D <> 0 then
  begin
    FSlopeX := Dx / D;
    FSlopeY := dy / (2 * D);
    FSlopeZ := dZ / D;
  end;
end;

procedure TAniFigure.PlayScript(Name: string; StartIndex: Word;
  PlayMode: TScriptMode);
var
  i: Integer;
begin
  if (Name = '') then
    i := -1
  else
  begin
    i := Resource.Scripts.IndexOf(Name);
    if i >= 0 then
    begin
      if (FrameMultiplier <> TScript(Resource.Scripts.Objects[i]).Multiplier) or
        (i <> FScriptIndex) then
      begin
        Delay := -1;
        FrameMultiplier := TScript(Resource.Scripts.Objects[i]).Multiplier;
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

procedure TAniFigure.PlayScript(Name: string; StartIndex: Word;
  PlayMode: TScriptMode; Multiplier, DeltaFrame: Word; Delta: SmallInt);
var
  i: Integer;
begin
  if (Name = '') then
    i := -1
  else
  begin
    i := Resource.Scripts.IndexOf(Name);
    if i >= 0 then
    begin
      if (FrameMultiplier <> Multiplier) or (i <> FScriptIndex) then
      begin
        Delay := -1;
        FrameMultiplier := Multiplier;
        if (StartIndex = DeltaFrame) then
          Inc(FrameMultiplier, Delta);
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

procedure TAniFigure.Render;
begin
  if Assigned(Resource) then
    Resource.Render(Self);
end;

procedure TAniFigure.EnumLightSource(Index, X, Y, Z: Longint; Intensity: double;
  Radius: Integer);
begin
  if Assigned(Resource) then
    Resource.EnumLightSource(Self, Index, X, Y, Z, Intensity, Radius);
end;

procedure TAniFigure.DoFrame;
begin

end;

procedure TAniFigure.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
  if (FEnabled) then
    ViewEnabled := FEnabled;
end;

procedure TAniFigure.SetFrame(const Value: Word);
begin
  FFrame := Value;
  FScriptIndex := -1;
  FScriptFrame := 0;
  PlayMode := smOnce;
  Delay := 0;
end;

procedure TAniFigure.ForceFrame(const Value: Word);
begin
  FFrame := Value;
end;

procedure TAniFigure.SetPos(X, Y, Z: Integer);
begin
  FStartX := FX;
  FStartY := FY;
  FStartZ := FZ;
  Moving := false;
  Terminal := false;
  Moved := True;
  NeedPath := false;
  GotPath := false;
  if (PathHandle <> 0) then
  begin
    GlobalFree(PathHandle);
    PathHandle := 0;
  end;
  FPrevX := FX;
  FPrevY := FY;
  FPrevZ := FZ;
  FX := X;
  FY := Y;
  FZ := Z;
  FStepX := X;
  FStepY := Y;
  FStepZ := Z;
end;

procedure TAniFigure.SetResource(const Value: TAniResource);
begin
  FResource := Value;
end;

procedure TAniFigure.Stop;
begin
  Moving := false;
  Terminal := false;
  NeedPath := false;
  GotPath := false;
  if (PathHandle <> 0) then
  begin
    GlobalFree(PathHandle);
    PathHandle := 0;
  end;
end;

procedure TAniFigure.UpdateScript;
begin
  if ScriptTerminated then
    Exit;
  if (FScriptIndex >= 0) then
  begin
    Inc(Delay);
    if Delay = 0 then
    begin
      FFrame := TScript(Resource.Scripts.Objects[FScriptIndex])
        .FrameID[FScriptFrame];
    end
    else if (Delay >= FrameMultiplier) then
    begin
      Delay := 0;
      if (PlayMode = smRandom) then
      begin
        FScriptFrame := random(TScript(Resource.Scripts.Objects[FScriptIndex])
          .Frames) + 1;
        FFrame := TScript(Resource.Scripts.Objects[FScriptIndex])
          .FrameID[FScriptFrame];
      end
      else
      begin
        if (FScriptFrame < TScript(Resource.Scripts.Objects[FScriptIndex])
          .Frames) then
        begin
          Inc(FScriptFrame);
          FFrame := TScript(Resource.Scripts.Objects[FScriptIndex])
            .FrameID[FScriptFrame];
          if FScriptFrame = MultiplierDeltaFrame then
          begin
            Inc(FrameMultiplier, MultiplierDelta);
          end;
        end
        else
        begin
          if (PlayMode = smRepeat) then
          begin
            FScriptFrame := 1;
            FFrame := TScript(Resource.Scripts.Objects[FScriptIndex])
              .FrameID[1];
          end
          else
          begin
            FFrame := TScript(Resource.Scripts.Objects[FScriptIndex])
              .FrameID[TScript(Resource.Scripts.Objects[FScriptIndex]).Frames];
            ScriptTerminated := True;
            if Assigned(OnScriptEnd) then
              OnScriptEnd(Self);
          end;
        end;
      end;
    end;
  end;
end;

{ TAniResource }

constructor TAniResource.Create;
begin
  inherited;
  Scripts := TStringList.Create;
end;

destructor TAniResource.Destroy;
var
  i: Integer;
begin
  for i := 0 to Scripts.Count - 1 do
    TScript(Scripts.Objects[i]).Free;
  Scripts.Free;
  FreeResources;
  inherited;
end;

procedure TAniResource.EnumLightSource(Figure: TAniFigure;
  Index, X, Y, Z: Longint; Intensity: double; Radius: Integer);
begin

end;

function TAniResource.AddScript(const Name: string; Script: TScript): Integer;
begin
  Result := Scripts.Add(Name);
  Scripts.Objects[Result] := Script;
end;

function TAniResource.GetScript(const Name: string): TScript;
var
  i: Integer;
begin
  i := Scripts.IndexOf(Name);
  if i >= 0 then
    Result := TScript(Scripts.Objects[i])
  else
    Result := nil;
end;

// { TImageSheet }
//
// constructor TImageSheet.Create;
// begin
// inherited;
// end;
//
/// /        procedure TImageSheet.Draw( Canvas : TCanvas; X, Y : Integer; Frame : Word );
/// /        begin
/// /
/// /        end;
//
// procedure TImageSheet.FreeResources;
// begin
// Picture := nil;
// end;
//
// function TImageSheet.GetFrames : Longint;
// begin
// Result := FramesWide * FramesHigh;
// end;
//
// procedure TImageSheet.Render( Figure : TAniFigure );
// var
// SrcX, SrcY : Longint;
// SrcX1, SrcY1, SrcX2, SrcY2 : Integer;
// DstX1, DstY1, DstX2, DstY2 : Integer;
// pr : TRect;
// begin
// if ( Figure.Frame = 0 ) or not Figure.Visible then
// Exit;
//
// SrcX := FrameWidth * ( ( Figure.Frame - 1 ) mod FramesWide );
// SrcY := FrameHeight * ( ( Figure.Frame - 1 ) div FramesWide );
//
// SrcX1 := SrcX;
// SrcX2 := SrcX1 + Figure.Width;
// DstX1 := Figure.View.Left + Figure.PosX;
// DstX2 := DstX1 + Figure.Width;
// Clip( Figure.View.Left, Figure.View.Left + Figure.View.Width, DstX1, DstX2, SrcX1, SrcX2 );
// SrcY1 := SrcY;
// SrcY2 := SrcY1 + Figure.Height;
// DstY1 := Figure.View.Top + Figure.PosY;
// DstY2 := DstY1 + Figure.Height;
// Clip( Figure.View.Top, Figure.View.Top + Figure.View.Height, DstY1, DstY2, SrcY1, SrcY2 );
// pr := Rect( SrcX1, SrcY1, SrcX2, SrcY2 );
// lpDDSBack.BltFast( DstX1, DstY1, Picture, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
// end;
//
// procedure TImageSheet.SetImage( const Value : IDirectDrawSurface );
// begin
// Picture := nil;
// Picture := Value;
// end;

{ TSpriteObject }

procedure TSpriteObject.Activate;
var
  event: string;
const
  FailName: string = 'TSpriteObject.Activate';
begin
  log.DebugLog(FailName);
  try

    Inc(ActivateCount);
    event := 'OnActivate[' + IntToStr(ActivateCount) + ']';
    if PropertyExists(event) then
      RunScript(Self, Properties[event])
    else
      RunScript(Self, OnActivate);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

constructor TSpriteObject.Create(X, Y, Z: longint; Frame: Word;
  Enabled: boolean);
const
  FailName: string = 'TSpriteObject.Create';
begin
  log.DebugLog(FailName);
  try

    inherited Create(X, Y, Z, Frame, Enabled);
    FFacing := fSS;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

destructor TSpriteObject.Destroy;
const
  FailName: string = 'TSpriteObject.Destroy';
begin
  log.DebugLog(FailName);
  try

    if assigned(MsgImage) then
      MsgImage := nil;

    inherited;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TSpriteObject.DoAction(const Action: string): boolean;
var
  S: string;
  Script: TScript;
const
  FailName: string = 'TSpriteObject.DoAction';
begin
  result := false;

  log.DebugLog(FailName);
  try

    if not assigned(Resource) then
    begin
      result := false;
      exit;
    end;
    result := true;
    Delay := -1;
    S := LowerCase(Action);
    if S = 'activate' then
      Activate
    else
    begin
      S := Action + Facing.ToString;
      Script := Resource.Script[S];
      if assigned(Script) then
      begin
        if Script.tag = scrLoop then
          PlayScript(S, 1, smRepeat)
        else if Script.tag = scrRandom then
          PlayScript(S, 1, smRandom)
        else
          PlayScript(S, 1, smOnce);
      end
      else
      begin
        Script := Resource.Script[Action];
        if assigned(Script) then
        begin
          if Script.tag = scrLoop then
            PlayScript(Action, 1, smRepeat)
          else if Script.tag = scrRandom then
            PlayScript(Action, 1, smRandom)
          else
            PlayScript(Action, 1, smOnce);
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

function TSpriteObject.GetProperty(const Name: string): string;
var
  S: string;
const
  FailName: string = 'TSpriteObject.GetProperty';
begin
  log.DebugLog(FailName);
  try

    S := LowerCase(Name);
    if S = 'onactivate' then
      result := OnActivate
    else if S = 'facing' then
      result := Facing.ToString
    else if S = 'enabled' then
      result := BoolToStr(Enabled, True)
    else if S = 'unmoveable' then
      result := BoolToStr(UnMoveable, True)
    else if S = 'oncollide' then
      result := OnCollide
    else if S = 'activatecount' then
      result := IntToStr(ActivateCount)
    else if S = 'collidecount' then
      result := IntToStr(CollideCount)
    else
      result := inherited GetProperty(Name);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TSpriteObject.SetProperty(const Name: string; const Value: string);
var
  S, S1: string;
  i, j, k, L: longint;
  NoProp: boolean;
const
  FailName: string = 'TSpriteObject.SetProperty';
begin
  log.DebugLog(FailName);
  try

    NoProp := false;
    S := LowerCase(Name);
    L := length(S);
    case L of
      5:
        begin
          if S = 'frame' then
            Frame := StrToInt(Value)
          else
            NoProp := true;
        end;
      6:
        begin
          if S = 'facing' then
          begin
            S1 := LowerCase(Value);
            if (S1 = 'se') then
              FFacing := fSE
            else if (S1 = 'ee') then
              FFacing := fEE
            else if (S1 = 'ne') then
              FFacing := fNE
            else if (S1 = 'nn') then
              FFacing := fNN
            else if (S1 = 'nw') then
              FFacing := fNW
            else if (S1 = 'ww') then
              FFacing := fWW
            else if (S1 = 'sw') then
              FFacing := fSW
            else
              FFacing := fSS;
          end
          else
            NoProp := true;
        end;
      7:
        begin
          if S = 'enabled' then
            Enabled := (LowerCase(Value) = 'true')
          else
            NoProp := true;
        end;
      8:
        begin
          if S = 'resource' then
          else if S = 'position' then
          begin
            i := StrToIntDef(Parse(Value, 0, ','), 0);
            j := StrToIntDef(Parse(Value, 1, ','), 0);
            k := StrToIntDef(Parse(Value, 2, ','), 0);
            SetPos(i, j, k);
          end
          else
            NoProp := true;
        end;
      9:
        begin
          if S = 'oncollide' then
          begin
            if not LoadingFromSaveFile then
              OnCollide := Value;
          end
          else
            NoProp := true;
        end;
      10:
        begin
          if S = 'onactivate' then
          begin
            if not LoadingFromSaveFile then
              OnActivate := Value;
          end
          else if S = 'unmoveable' then
            UnMoveable := (LowerCase(Value) = 'true')
          else
            NoProp := true;
        end;
      12:
        begin
          if S = 'collidecount' then
            CollideCount := StrToInt(Value)
          else
            NoProp := true;
        end;
      13:
        begin
          if S = 'activatecount' then
            ActivateCount := StrToInt(Value)
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

procedure TSpriteObject.SetResource(const Value: TAniResource);
const
  FailName: string = 'TSpriteObject.SetResource';
begin
  log.DebugLog(FailName);
  try

    inherited;
    if Value is TResource then
    begin
      with Value as TResource do
      begin
        Self.Width := FrameWidth;
        Self.Height := FrameHeight;
        Self.FrameMultiplier := FrameMultiplier;
        Self.CenterX := CenterX;
        Self.CenterY := CenterY;
        Self.Radius := Radius;
        Self.Speed := Speed;
        Self.Alpha := Alpha;
        Self.SpecialEffect := SpecialEffect;
        Self.MaskHeight := FrameHeight;
        Self.UseLighting := UseLighting;
        Self.Highlightable := Highlightable;
        if FrameHeight >= 100 then
          Self.MouseRect := Rect(CenterX - Radius, 20, CenterX + Radius,
            FrameHeight - 20)
        else
          Self.MouseRect := Rect(CenterX - Radius, 0, CenterX + Radius,
            FrameHeight);
      end;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TSpriteObject.ActionExists(const Action: string): boolean;
var
  Script: TScript;
const
  FailName: string = 'TSpriteObject.ActionExists';
begin
  result := false;

  log.DebugLog(FailName);
  try

    if not assigned(Resource) then
    begin
      result := false;
      exit;
    end;
    Script := Resource.Script[Action + Facing.ToString];
    if not assigned(Script) then
      Script := Resource.Script[Action];
    result := assigned(Script);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TSpriteObject.Say(const Msg: string; Color: TColor);
var
  BM: TBitmap;
  R: TRect;
  i: Integer;
const
  FailName: string = 'TSpriteObject.Say';
begin
  // TODO: Cleanup
  log.DebugLog(FailName);
  try

    if Msg = '' then
    begin
      MsgDuration := 0;
      if MsgDuration = 0 then
      begin
        MsgImage := nil;
        i := SayList.IndexOf(Self);
        if i >= 0 then
          SayList.Delete(i);
      end;
      exit;
    end;
    MsgDuration := 100;
    BM := TBitmap.Create;
    R := Rect(0, 0, Width * 2, 0);
    DrawText(BM.Canvas.Handle, PChar(Msg), -1, R, DT_CALCRECT or DT_CENTER or
      DT_NOCLIP or DT_NOPREFIX or DT_WORDBREAK);
    MsgWidth := R.Right;
    MsgHeight := R.Bottom;
    BM.Width := MsgWidth;
    BM.Height := MsgHeight;
    SetTextColor(BM.Canvas.Handle, ColorToRGB(Color));
    SetBkMode(BM.Canvas.Handle, TRANSPARENT);
    PatBlt(BM.Canvas.Handle, 0, 0, MsgWidth, MsgHeight, BLACKNESS);
    DrawText(BM.Canvas.Handle, PChar(Msg), -1, R, DT_CENTER or DT_NOCLIP or
      DT_NOPREFIX or DT_WORDBREAK);
    MsgImage := SoAOS_DX_SurfaceFromBMP(BM, clBlack);
    BM.free;
    SayList.Insert(0, Self);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TSpriteObject.UpdateSay;
var
  X0, i: Integer;
  // pr : TRect;
const
  FailName: string = 'TSpriteObject.UpdateSay';
begin
  log.DebugLog(FailName);
  try

    inherited;
    if MsgDuration > 0 then
    begin
      X0 := PosX + CenterX - MsgWidth div 2;
      // Windows 8/10 Fix, Crash at Mapedges appearently - from gondur branch
      DrawAlpha(lpDDSBack, Rect(X0, PosY - MsgHeight, X0 + MsgWidth, PosY),
        Rect(0, 0, MsgWidth, MsgHeight), MsgImage, true, 255);
      // pr := Rect( 0, 0, MsgWidth, MsgHeight );
      // lpDDSBack.BltFast( X0, PosY - MsgHeight, MsgImage, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      Dec(MsgDuration);

      if MsgDuration = 0 then
      begin
        MsgImage := nil;
        i := SayList.IndexOf(Self);
        if i >= 0 then
          SayList.Delete(i);
      end;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TSpriteObject.SetFacing(const Value: TFacing);
const
  FailName: string = 'TSpriteObject.SetFacing';
begin
  log.DebugLog(FailName);
  try

    FFacing := Value;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TSpriteObject.SaveProperties(List: TStringList);
var
  S: string;
const
  FailName: string = 'TSpriteObject.SaveProperties';
begin
  log.DebugLog(FailName);
  try

    S := 'enabled=' + BoolToStr(Enabled, True);
    List.add(S);
    S := 'onactivate=' + OnActivate;
    List.add(S);
    S := 'facing=' + Facing.ToString;
    List.add(S);
    S := 'frame=' + IntToStr(Frame);
    List.add(S);
    S := 'unmoveable=' + BoolToStr(UnMoveable, True);
    List.add(S);
    S := 'oncollide=' + OnCollide;
    List.add(S);
    S := 'activatecount=' + IntToStr(ActivateCount);
    List.add(S);
    S := 'collidecount=' + IntToStr(CollideCount);
    List.add(S);
    if assigned(Resource) then
    begin
      S := 'resource=' + TResource(Resource).FileName;
      List.add(S);
    end;
    inherited;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TSpriteObject.Init;
const
  FailName: string = 'TSpriteObject.Init';
begin
  log.DebugLog(FailName);
  try

    DoAction('Default');

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TSpriteObject.GetName: string;
const
  FailName: string = 'TSpriteObject.GetName';
begin
  log.DebugLog(FailName);
  try

    result := '';

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TSpriteObject.ShouldSave: boolean;
begin
  // result:=Enabled;
  result := true;
end;

{ TGameObject }

constructor TGameObject.Create(X, Y, Z: longint; Frame: Word; Enabled: boolean);
const
  FailName: string = 'TGameObject.Create';
begin
  log.DebugLog(FailName);
  try

    inherited Create(X, Y, Z, Frame, Enabled);
    FProperties := TStringList.Create;
    FProperties.Duplicates := dupIgnore;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

destructor TGameObject.Destroy;
const
  FailName: string = 'TGameObject.Destroy';
begin
  log.DebugLog(FailName);
  try

    FProperties.free;
    inherited;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TGameObject.GetProperty(const Name: string): string;
var
  S: string;
const
  FailName: string = 'TGameObject.GetProperty';
begin
  log.DebugLog(FailName);
  try

    S := LowerCase(Name);
    if S = 'guid' then
      result := GUID
    else if S = 'groupname' then
      result := GroupName
    else if S = 'loadcount' then
      result := IntToStr(LoadCount)
    else if S = 'onload' then
      result := OnLoad
    else
      result := FProperties.Values[Name];

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TGameObject.Init;
begin

end;

procedure TGameObject.LoadProperties(const List: TStringList);
var
  S, S0, S1: string;
  StrTmp: string;
  i, j, L: Integer;
  INI: TMemINIFile;
const
  FailName: string = 'TGameObject.LoadProperties';
begin
  log.DebugLog(FailName);
  try

    Loading := true;
    INI := nil;
    try
      FProperties.Clear;
      for i := 0 to List.Count - 1 do
      begin
        S := List.strings[i];
        j := Pos('=', S);
        if (j > 0) { and (j<length(S)) } then
        begin // This caused blank properties not to overwrite defaults
          if (j < length(S)) and (S[j + 1] = '#') then
          begin
            if not assigned(INI) then
              INI := TMemINIFile.Create( MapPath + Language + '\symbols.ini', TEncoding.GetEncoding(INICodepage) );

            S0 := copy(S, j + 1, length(S) - j);
            S1 := Parse(S0, 1, '#');
            StrTmp := S1;
            L := length(S1);
            S1 := INI.ReadString(Parse(S1, 0, '.'), Parse(S1, 1, '.'),
              '#' + S1);
            if StrTmp = S1 then
              log.log('** BadName: ' + S1);
            SetProperty(copy(S, 1, j - 1),
              S1 + copy(S0, L + 3, length(S0) - L - 2));
          end
          else
          begin
            SetProperty(copy(S, 1, j - 1), copy(S, j + 1, length(S) - j));
          end;
        end;
      end;
    finally
      Loading := false;
      INI.free;
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

function TGameObject.PropertyExists(const Name: string): boolean;
const
  FailName: string = 'TGameObject.PropertyExists';
begin
  result := false;

  log.DebugLog(FailName);
  try

    result := FProperties.IndexOfName(Name) >= 0;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TGameObject.SaveProperties(List: TStringList);
var
  i: Integer;
  S: string;
const
  FailName: string = 'TGameObject.SaveProperties';
begin
  log.DebugLog(FailName);
  try

    S := 'guid=' + GUID;
    List.add(S);
    S := 'groupname=' + GroupName;
    List.add(S);
    S := 'onload=' + OnLoad;
    List.add(S);
    S := 'loadcount=' + IntToStr(LoadCount);
    List.add(S);
    S := 'position=' + IntToStr(X) + ',' + IntToStr(Y) + ',' + IntToStr(Z);
    List.add(S);
    for i := 0 to FProperties.Count - 1 do
    begin
      List.add(FProperties.strings[i]);
    end;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TGameObject.SetProperty(const Name, Value: string);
var
  S: string;
  L: Integer;
  NoProp: boolean;
const
  FailName: string = 'TGameObject.SetProperty';
begin
  log.DebugLog(FailName);
  try

    NoProp := false;
    S := LowerCase(Name);
    L := length(S);
    case L of
      4:
        begin
          if S = 'guid' then
            GUID := Value
          else
            NoProp := true;
        end;
      6:
        begin
          if S = 'onload' then
          begin
            if not LoadingFromSaveFile then
              OnLoad := Value;
          end
          else
            NoProp := true;
        end;
      8:
        begin
          if S = 'position' then
          begin
            FX := StrToIntDef(Parse(Value, 0, ','), 0);
            FY := StrToIntDef(Parse(Value, 1, ','), 0);
            FZ := StrToIntDef(Parse(Value, 2, ','), 0);
          end
          else
            NoProp := true;
        end;
      9:
        begin
          if S = 'groupname' then
            GroupName := Value
          else if S = 'loadcount' then
            LoadCount := StrToInt(Value)
          else
            NoProp := true;
        end
    else
      begin
        NoProp := true;
      end;
    end;

    if NoProp then
      FProperties.Values[Name] := Value;

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

procedure TGameObject.DoLoad;
var
  event: string;
const
  FailName: string = 'TGameObject.DoLoad';
begin
  log.DebugLog(FailName);
  try

    Inc(LoadCount);
    event := 'OnLoad[' + IntToStr(LoadCount) + ']';
    if PropertyExists(event) then
      RunScript(Self, Properties[event])
    else
      RunScript(Self, OnLoad);

  except
    on E: Exception do
      log.log(FailName, E.Message, []);
  end;
end;

end.
