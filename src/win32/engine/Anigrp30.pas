unit Anigrp30;
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

{$INCLUDE Anigrp30cfg.inc}

interface

uses
  Windows,
  Classes,
  SysUtils,
  Forms,
  Graphics,
  Controls,
  ExtCtrls,
  Dialogs,
  AniDec30,
  AStar,
  MMTimer,
{$IFDEF DirectX}
  DirectX,
  DXUtil,
  DXEffects,
{$ENDIF}
  LogFile;

type
  TAniMap = class;
  TAniFigure = class;
  TAniView = class;
  TSubMap = class;

  TFlicker = ( flNone, flCandle, flTorch, flFluorescent );

  TAniSpecialEffect = ( seNone, seTranslucent, seSilhouette, seInvert, seSpooky,
    seFunky, seWhite, seNegative, seStrange, seAdd, seSubtract,
    seMultiply );

  TExtMouseEvent = procedure( Sender : TAniView; Button : TMouseButton;
    Shift : TShiftState; X, Y : Integer; MapX, MapY : Integer ) of object;

  TExtMouseMoveEvent = procedure( Sender : TAniView;
    Shift : TShiftState; X, Y : Integer; MapX, mapY : Integer ) of object;

  TCollideFigureEvent = procedure( Source, Target : TAniFigure; var Stop : Boolean ) of object;

  TCollideItemEvent = procedure( Source : TAniFigure; var Stop : Boolean ) of object;

  TCollideBoundaryEvent = procedure( Source : TAniFigure ) of object;

  TTriggerEvent = procedure( Source : TAniFigure; ID, PrevID : SmallInt ) of object;

  TPathEvent = procedure( Sender : TAniFigure; X, Y : Longint ) of object;

  TScriptMode = ( smOnce, smRepeat, smRandom );

  TZone = class( TObject )
  private
    //    Filename          :string;
    FIndex : Integer;
    //Tile Information
    FTileBitWidth : Longint;
    FTileBitHeight : Longint;
    FTileMaxIndex : Word;
    FTileMaxColumnIndex : Word;
    FTileMem : ^TileInfo;
    //Item Information
    FItemBitWidth : Longint;
    FItemBitHeight : Longint;
    FItemMem : ^ItemInfo;
    FItemColumn : integer;
    FItemColumnBitHeight : integer;
    FMap : TAniMap;
{$IFDEF DirectX}
    TilesInVideo : Boolean;
    ItemsInVideo : Boolean;
    FullRefresh : boolean;
    FTileImages : IDirectDrawSurface;
    FItemImages : IDirectDrawSurface;
{$ENDIF}
{$IFNDEF DirectX}
    FTileMasks : HBITMAP;
    FTileImages : HBITMAP;
    FItemMasks : HBITMAP;
    FItemImages : HBITMAP;
{$ENDIF}
    function GetTile( i : Integer ) : TileInfo;
    procedure SetTile( i : Integer; Tile : TileInfo );
    function GetItem( i : Integer ) : ItemInfo;
    procedure SetItem( i : Integer; Item : ItemInfo );
    property Tile[ i : Integer ] : TileInfo read GetTile write SetTile;
    property Item[ i : Integer ] : ItemInfo read GetItem write SetItem;
  public
    X1 : Longint;
    Y1 : Longint;
    X2 : Longint;
    Y2 : Longint;
    Cached : Boolean;
    Loaded : boolean;
    constructor Create( Map : TAniMap );
    destructor Destroy; override;
    procedure Release;
    procedure Restore;
{$IFDEF DirectX}
    procedure DefineTile( Index : Word; Image : IDirectDrawSurface; Color : TColor );
    function DefineItem( Index : Word; Image : IDirectDrawSurface; const StripHeights, CollisionMasks, LineOfSightMasks, LightPoints : array of Word; Color : TColor; Slope : Single; Visible, AutoTransparent, Vertical : boolean ) : PItemInfo;
    procedure MoveToVideo;
{$ENDIF}
{$IFNDEF DirectX}
    procedure DefineTile( Index : Word; Image, Mask : TBitmap; Color : TColor );
    function DefineItem( Index : Word; BITMAP : TBitmap; const StripHeights, CollisionMasks, LineOfSightMasks, LightPoints : array of Word; Color : TColor; Slope : Single; Visible, AutoTransparent, Vertical : boolean ) : PItemInfo;
{$ENDIF}
    procedure DisposeDef;
    class procedure Skip( Stream : TStream ); virtual;
    procedure SaveToStream( Stream : TStream; SaveImage : boolean ); virtual;
    procedure LoadFromStream( Stream : TStream ); virtual;
    procedure LoadTileCustomData( Stream : TStream ); virtual;
    procedure SaveTileCustomData( Stream : TStream ); virtual;
    procedure LoadItemCustomData( Stream : TStream ); virtual;
    procedure SaveItemCustomData( Stream : TStream ); virtual;
    procedure ExportTiles( Filename : string );
    procedure ExportItems( Filename : string );
    property Index : Integer read FIndex;
  end;

  TLightZone = class( TZone )
  private
    OverlapZones : TList;
    Items : TList;
    States : Integer;
    State : Integer;
    TileStateOffset : Longint;
    ItemStateOffset : Longint;
    FlickerX : array[ 1..MaxLightStates ] of Longint;
    FlickerY : array[ 1..MaxLightStates ] of Longint;
    FlickerZ : array[ 1..MaxLightStates ] of Longint;
    FlickerRadius : array[ 1..MaxLightStates ] of Longint;
    FlickerIntensity : array[ 1..MaxLightStates ] of Longint;
    AddColumn : boolean;
{$IFDEF DirectX}
    Blinking : Boolean;
    StateDuration : Integer;
    procedure AddStrip( Image : IDirectDrawSurface; var NewX, NewY : word );
{$ENDIF}
{$IFNDEF DirectX}
    procedure AddStrip( Image, Mask : TBitmap; var NewX, NewY : word );
{$ENDIF}
    procedure NewTileState;
    procedure NewItemState;
  public
    Color : TColor;
    Intensity : integer;
    Radius : Longint;
    Flicker : TFlicker;
    X : Longint;
    Y : Longint;
    Z : Longint;
    constructor Create( Map : TAniMap );
    destructor Destroy; override;
    procedure LoadTileCustomData( Stream : TStream ); override;
    procedure SaveTileCustomData( Stream : TStream ); override;
    procedure LoadItemCustomData( Stream : TStream ); override;
    procedure SaveItemCustomData( Stream : TStream ); override;
  end;

  TAniMap = class( TComponent )
  private
    FWidth : Longint;
    FHeight : Longint;
    FBitWidth : Longint;
    FBitHeight : Longint;
    FMapData : HGLOBAL;
    FTransparentColor : TColor;
{$IFDEF DirectX}
    FColorMatch : word;
    NeedColorMatch : boolean;
{$ENDIF}
    FAmbientColor : TColor;
    FAmbientIntensity : integer;
    LightR : double;
    LightG : double;
    LightB : double;
    FUseLighting : Boolean;
    FUseAmbientOnly : Boolean;
    FTileSize : Word;
    FTileHeight : Word;
    FTileWidth : Word;
    FStripWidth : Word;
    FStripHeight : Word;
    //Item Instance Information
    FItemList : array[ 1..ItemListSize ] of ItemInstanceInfo;
    FirstItem : Word;
    LastItem : Word;
    //    SubMaps           :TList;
    procedure SetWidth( VWidth : Longint );
    procedure SetHeight( VHeight : Longint );
    procedure SetTileSize( Size : Word );
    procedure SetAmbientColor( Color : TColor );
    procedure SetAmbientIntensity( Value : Integer );
    function GetZoneCount : word;
    procedure SetTransparentColor( Color : TColor );
    function GetColorMatch : word;
  protected
  public
    Zones : TList;
    constructor Create( AOwner : TComponent ); override;
    destructor Destroy; override;
    function AddItem( Zone, ItemIndex : Word; X, Y, Z : Longint; FilterID : Smallint;
      Collidable : Boolean ) : PItemInstanceInfo;
    procedure Clear;
    procedure FreeResources;
    procedure FreeDefinitions;
    function GetTile( X, Y : Longint ) : PGridInfo;
    procedure SetTile( X, Y, Layer : Longint; Zone, Index : Word );
    procedure SetDiamondTile( X, Y : Longint; ZoneTop, TileTop, ZoneRight, TileRight, ZoneBottom, TileBottom, ZoneLeft, TileLeft : Word );
    procedure SetCollisionMask( X, Y : Longint; CollisionMask : Word );
    procedure SetLineOfSightMask( X, Y : Longint; LineOfSightMask : Word );
    procedure SetTrigger( X, Y : Longint; TriggerID : SmallInt ); overload;
    procedure SetTrigger( X, Y : Longint; TriggerID : SmallInt; TriggerMask : Word ); overload;
    procedure SetFilter( X, Y : Longint; FilterID : SmallInt ); overload;
    procedure SetFilter( X, Y : Longint; FilterID : SmallInt; FilterMask : Word ); overload;
{$IFDEF DirectX}
    procedure DefineTile( Zone, Index : Word; Image : IDirectDrawSurface );
    function DefineItem( Zone, Index : Word; Image : IDirectDrawSurface; const StripHeights, CollisionMasks, LineOfSightMasks, LightPoints : array of Word; Slope : Single; Visible, AutoTransparent, Vertical : Boolean ) : PItemInfo;
{$ENDIF}
{$IFNDEF DirectX}
    procedure DefineTile( Zone, Index : Word; BITMAP : TBitmap );
    function DefineItem( Zone, Index : Word; BITMAP : TBitmap; const StripHeights, CollisionMasks, LineOfSightMasks, LightPoints : array of Word; Slope : Single; Visible, AutoTransparent, Vertical : Boolean ) : PItemInfo;
{$ENDIF}
    procedure Sort;
    procedure RenderMap;
    procedure MoveZoneToVideo( Index : integer );
    function GetZoneMemoryUsage( Index : integer ) : longint;
    function AddZone : Word;
    function AddLightZone : Word;
    function AddLight( Color : TColor; Intensity : Integer; Radius : Longint; Flicker : TFlicker; X, Y, Z : Longint ) : Word;
    function LineOfSight( X1, Y1, X2, Y2 : Longint ) : Boolean;
    function LineOfCollision( X1, Y1, X2, Y2 : Longint ) : Boolean;
    function GetItemIndex( Item : PItemInstanceInfo ) : word;
    function GetItemAddress( Index : word ) : PItemInstanceInfo;
    procedure SaveMapKnownInfo( Stream : TStream );
    procedure LoadMapKnownInfo( Stream : TStream );
    procedure SaveToStream( Stream : TStream );
    procedure LoadFromStream( Stream : TStream );
    property TileWidth : Word read FTileWidth;
    property TileHeight : Word read FTileHeight;
    property ItemCount : word read LastItem;
    property ColorMatch : word read GetColorMatch;
  published
    property Width : Longint read FWidth write SetWidth default 10;
    property Height : Longint read FHeight write SetHeight default 20;
    property TileSize : Word read FTileSize write SetTileSize default 4;
    property TransparentColor : TColor read FTransparentColor write SetTransparentColor default clFuchsia;
    property AmbientIntensity : Integer read FAmbientIntensity write SetAmbientIntensity;
    property AmbientColor : TColor read FAmbientColor write SetAmbientColor;
    property UseLighting : Boolean read FUseLighting write FUseLighting;
    property UseAmbientOnly : Boolean read FUseAmbientOnly write FUseAmbientOnly;
    property ZoneCount : word read GetZoneCount;
  end;

  TSubMap = class( TAniMap )
  public
    X1, Y1 : Longint;
    X2, Y2 : Longint;
    Visible : Boolean;
  end;

  TScript = class( TObject )
  public
    Frames : Word;
    FrameID : array[ 1..MaxScriptFrames ] of Word;
    Multiplier : Word;
    Tag : Longint;
  end;

  TAniResource = class( TObject )
  private
    function GetScript( const Name : string ) : TScript;
  protected
    Scripts : TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Render( Figure : TAniFigure ); virtual; abstract;
    procedure EnumLightSource( Figure : TAniFigure; Index, X, Y, Z : longint; Intensity : double; Radius : integer ); virtual;
    procedure Draw( Canvas : TCanvas; X, Y : Integer; Frame : Word ); virtual; abstract;
    procedure FreeResources; virtual; abstract;
    function AddScript( const Name : string; Script : TScript ) : Integer;
    property Script[ const Name : string ] : TScript read GetScript;
  end;

  TAniFigure = class( TObject )
  private
    NextOnRow : TAniFigure;
    NextInTile : TAniFigure;
    Moving : Boolean;
    Moved : Boolean;
    MapOffset : Longint;
    Terminal : Boolean;
    NeedPath : Boolean;
    GotPath : Boolean;
    PathHandle : HGLOBAL;
    PathCount : Word;
    PathStep : Word;
    PlayMode : TScriptMode;
    MultiplierDelta : Smallint;
    MultiplierDeltaFrame : Word;
    AvoidInPath : TList;
    ViewEnabled : Boolean;
    Zone : Byte;
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
    FStepX : Double;
    FStepY : Double;
    FStepZ : Double;
    FSlopeX : Double;
    FSlopeY : Double;
    FSlopeZ : Double;
    FDestX : Longint;
    FDestY : Longint;
    FDestZ : Longint;
    FPrevX : longint;
    FPrevY : longint;
    FPrevZ : longint;
    FDistance : Double;
    FPosX : Longint;
    FPosY : Longint;
    FLightIndex : single;
    LightComputed : LongWord;
    ScriptTerminated : boolean;
    FResource : TAniResource;
    FTile : PGridInfo;
    FOnScreen : boolean;
    function GetLightIndex : single;
    procedure UpdateScript;
    procedure SetEnabled( const Value : Boolean );
    procedure SetFrame( const Value : Word );
  protected
    FZ : Longint;
    FY : Longint;
    FX : Longint;
    PathDeviance : integer;
    procedure SetResource( const Value : TAniResource ); virtual;
    procedure Render; virtual;
    procedure EnumLightSource( Index, X, Y, Z : longint; Intensity : double; Radius : integer ); virtual;
    procedure DoFrame; virtual;
  public
    LightR : Integer;
    LightG : Integer;
    LightB : Integer;
    HighlightColor : TColor;
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
    procedure Stop;
    procedure Move( X, Y, Z : Longint );
    procedure MoveTo( X, Y, Z : Longint );
    procedure SetPos( X, Y, Z : Longint );
    procedure FindPathTo( X, Y : Longint; Avoid : TList; Deviance : integer );
    procedure PlayScript( Name : string; StartIndex : Word; PlayMode : TScriptMode ); overload;
    procedure PlayScript( Name : string; StartIndex : Word; PlayMode : TScriptMode; Multiplier, DeltaFrame : Word; Delta : SmallInt ); overload;
    procedure ForceFrame( const Value : Word );
    property ScriptFrame : Word read FScriptFrame;
    property X : Longint read FX;
    property Y : Longint read FY;
    property Z : Longint read FZ;
    property StartX : Longint read FStartX;
    property StartY : Longint read FStartY;
    property StartZ : Longint read FStartZ;
    property StepX : Double read FStepX;
    property StepY : Double read FStepY;
    property StepZ : Double read FStepZ;
    property SlopeX : Double read FSlopeX;
    property SlopeY : Double read FSlopeY;
    property SlopeZ : Double read FSlopeZ;
    property DestX : Longint read FDestX;
    property DestY : Longint read FDestY;
    property DestZ : Longint read FDestZ;
    property PrevX : Longint read FPrevX;
    property PrevY : Longint read FPrevY;
    property PrevZ : Longint read FPrevZ;
    property PosX : Longint read FPosX;
    property PosY : Longint read FPosY;
    property Frame : Word read FFrame write SetFrame;
    property ScriptIndex : Integer read FScriptIndex;
    property Enabled : Boolean read FEnabled write SetEnabled;
    property View : TAniView read FView;
    property PathDestX : Longint read FPathDestX;
    property PathDestY : Longint read FPathDestY;
    property Distance : Double read FDistance;
    property Resource : TAniResource read FResource write SetResource;
    property LightIndex : single read GetLightIndex;
    property Tile : PGridInfo read FTile;
    property OnScreen : boolean read FOnScreen;
  end;

  TImageSheet = class( TAniResource )
  private
{$IFDEF DirectX}
    Picture : IDirectDrawSurface;
    procedure SetImage( const Value : IDirectDrawSurface );
{$ENDIF}
{$IFNDEF DirectX}
    Picture : HBITMAP;
    Mask : HBITMAP;
    procedure SetImage( const Value : TBitmap );
{$ENDIF}
    function GetFrames : Longint;
  public
    FrameWidth : Longint;
    FrameHeight : Longint;
    FramesWide : Longint;
    FramesHigh : Longint;
    TransparentColor : TColor;
    constructor Create;
    procedure Draw( Canvas : TCanvas; X, Y : Integer; Frame : Word ); override;
    procedure FreeResources; override;
    procedure Render( Figure : TAniFigure ); override;
    property Frames : Longint read GetFrames;
    property Image : IDirectDrawSurface write SetImage;
  end;

  TAniView = class( TGraphicControl )
  private
    MapX : Longint;
    MapY : Longint;
    MapOffsetX : Integer;
    MapOffsetY : Integer;
    MapWidth : Longint;
    MapHeight : Longint;
    MapBitWidth : Longint;
    MapBitHeight : Longint;
    MapColumns : HGLOBAL;
    FMapColumnCount : Longint;
    MapRows : HGLOBAL;
    PixelHeight : Longint;
    Timer : TAniTimer;
    FInterval : Word;
    FActive : Boolean;
    FMap : TAniMap;
    FFRameCount : LongWord;
{$IFNDEF DirectX}
    MapBuffer : TBitmap; //MapBuffer represents the fixed portion of the map
    Work : TBitmap;
    XRayImage : TBitmap;
{$ENDIF}
{$IFDEF DirectX}
    lpDDSMap : IDirectDrawSurface;
    Work : IDirectDrawSurface;
    XRayImage : IDirectDrawSurface;
{$ENDIF}
    XRayWidth : Integer;
    XRayHeight : Integer;
    MaxHeightTop : Longint;
    MaxHeightBottom : Longint;
    FMaxCollisionRadius : Word;
    FItemMask : Longint;
    FFigureList : TList;
    FOnMouseDown : TExtMouseEvent;
    FOnMouseUp : TExtMouseEvent;
    FOnMouseMove : TExtMouseMoveEvent;
    FOnBeforeDisplay : TNotifyEvent;
    FOnAfterDisplay : TNotifyEvent;
    FOnWaitFortimer : TNotifyEvent;
    FAStar : TAStar;
    FAStarFigure : TAniFigure;
    FAstarAvoidFigure : TList;
    FStartX : Longint;
    FStartY : Longint;
    FDrawing : Boolean;
    FLMouseButton : boolean;
    FShowRepaint : boolean;
    RepaintCode : longint;
    LastTickCount : longword;
    MapPreCreated : boolean;
    procedure FDrawFrame( Sender : TObject );
    procedure SetMap( const VMap : TAniMap );
    procedure InitMap;
    procedure UpdateMap;
    procedure DrawTile( GridLoc : Pointer; i, j, Layer : Integer );
{$IFDEF DirectX}
    procedure CopyTile( Dest : IDirectDrawSurface; GridLoc : Pointer; X, Y, Layer : Integer; ClipRect : PRect );
{$ENDIF}
{$IFNDEF DirectX}
    procedure CopyTile( Dest : HDC; GridLoc : Pointer; X, Y, Layer : Integer; ClipRect : PRect );
{$ENDIF}
    procedure DrawItems;
    procedure DrawItemsClip( X1, X2, Y1, Y2 : Longint );
    procedure SetInterval( PInterval : Word );
    procedure SetActive( VActive : Boolean );
    procedure SetItemMask( Mask : Longint );
    procedure SetShowRepaint( const Value : Boolean );
    procedure BuildRowUpdateInfo;
    procedure DrawFigure( Figure : TAniFigure );
    procedure FRefreshMap;
    procedure RefreshRegion( X, Y, W, H : Longint );
{$IFDEF DirectX}
    procedure RefreshLight( Zone : TLightZone );
{$ENDIF}
    function CanMove( SrcX, SrcY, DestX, DestY : Smallint ) : Boolean;
    procedure GetPath( Figure : TAniFigure );
    procedure SetAutoTransparentMask( const Value : TBitmap );
    procedure ComputeLight( Figure : TAniFigure );
  protected
    procedure Show( Value : Boolean );
    procedure Paint; override;
  public
    TempDC : HDC; //Used for swapping bitmaps for blting
    OldTempBitmap : HBITMAP; //Original Bitmap in TempDC
    OffsetX : Longint;
    OffsetY : Longint;
    CenterX : Longint;
    CenterY : Longint;
    FRightX : longint;
    FBottomY : longint;
{$IFNDEF DirectX}
    FrameBuffer : TBitmap; //FrameDC is where the frame is prepared before display
{$ENDIF}
    KeyFigure : TAniFigure;
    MouseOverFigure : TAniFigure;
    MouseOverHLFigure : TAniFigure;
    MouseOverTile : PGridInfo;
    MousePosition : TPoint;
    ForceRefresh : Boolean;
    constructor Create( AOwner : TComponent ); override;
    destructor Destroy; override;
    procedure InitDX( Handle : HWND; ResW, ResH, BPP : Integer );
    procedure CloseDX;
    property Active : Boolean read FActive write SetActive;
    procedure MouseDown( Button : TMouseButton; Shift : TShiftState; X, Y : Integer ); override;
    procedure MouseUp( Button : TMouseButton; Shift : TShiftState; X, Y : Integer ); override;
    procedure MouseMove( Shift : TShiftState; X, Y : Integer ); override;
    procedure CenterView( X, Y : Longint );
    procedure DrawFrame;
    procedure RefreshMap;
    procedure FreeResources;
    procedure AddFigure( Figure : TAniFigure );
    procedure ReplaceFigure( i : integer; Figure : TAniFigure );
    procedure MoveFigure( Figure : TAniFigure );
    procedure TransFigure( Figure : TAniFigure );
    procedure WaitForNextFrame;
    procedure PrecreateMap( W, H : longint );
    procedure UncreateMap;
    function FindPath( Figure : TAniFigure; X2, Y2, Deviance : Longint; var Path : HGLOBAL ) : integer;
    function FindInRadius( X, Y : Longint; Radius : Single ) : TList;
    function LineOfSight( X1, Y1, X2, Y2 : Longint ) : Boolean;
    function LineOfCollision( X1, Y1, X2, Y2 : Longint ) : Boolean;
    function ClearShot( SrcX, Srcy, DstX, DstY, Radius : longint; UseLineOfSight : boolean ) : boolean;
    procedure DisableFigure( Figure : TAniFigure );
    property Map : TAniMap read FMap write SetMap;
    property Canvas;
    property ItemMask : Longint read FItemMask write SetItemMask;
    property AutoTransparentMask : TBitmap write SetAutoTransparentMask;
    property Drawing : Boolean read FDrawing;
    property FRameCount : LongWord read FFRameCount;
    property RightX : longint read FRightX;
    property BottomY : longint read FBottomY;
  published
    property Enabled;
    property Align;
    property ShowHint;
    property Visible;
    property Interval : Word read FInterval write SetInterval default 50;
    property FigureList : TList read FFigureList;
    property OnMouseDown : TExtMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp : TExtMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseMove : TExtMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnClick;
    property OnBeforeDisplay : TNotifyEvent read FOnBeforeDisplay write FOnBeforeDisplay;
    property OnAfterDisplay : TNotifyEvent read FOnAfterDisplay write FOnAfterDisplay;
    property LMouseButton : boolean read FLMouseButton write FLMousebutton;
    property OnWaitForTimer : TNotifyEvent read FOnWaitForTimer write FOnWaitForTimer;
    property ShowRepaint : boolean read FShowRepaint write SetShowRepaint;
  end;

procedure Register;
function MakeScript( const Frames : array of Word ) : ScriptInfo;
function FindColorMatch( Color : TColor ) : word;

var
{$IFDEF DirectX}
  lpDD : IDirectDraw;
  lpDDSFront : IDirectDrawSurface;
  lpDDSBack : IDirectDrawSurface;
  ResWidth : Integer;
  ResHeight : Integer;
  DXMode : Boolean;
  PixelFormat : TPixelFormat;
{$ENDIF}
  Debug : Longint;

implementation

uses
  Character;

function MakeScript( const Frames : array of Word ) : ScriptInfo;
var
  i, j : Integer;
begin
  j := 0;
  for i := Low( Frames ) to High( Frames ) do
  begin
    Inc( j );
    Result.FrameID[ j ] := Frames[ i ];
  end;
  Result.Frames := j;
end;

function FindColorMatch( Color : TColor ) : word;
begin
  result := DDColorMatch( lpDDSBack, Color );
end;

//------------------------------------------------------------------------------
//AniMap Component

constructor TAniMap.Create( AOwner : TComponent );
var
  GridSize, i : Longint;
  GridLoc : ^GridInfo;
  NewZone : TZone;
begin
  inherited Create( AOwner );
  FWidth := 10;
  FHeight := 20;
  FTileSize := 4;
  FTileWidth := FTileSize * 16;
  FTileHeight := FTileSize * 8;
  FBitWidth := FWidth * FTileWidth;
  FBitHeight := FHeight * FTileHeight;
  FStripWidth := FTileWidth shr 2;
  FStripHeight := FTileHeight shr 2;
  FTransparentColor := clFuchsia;
  NeedColorMatch := true;
  if not ( csDesigning in ComponentState ) then
  begin
    if ( FWidth > 0 ) and ( FHeight > 0 ) then
    begin
      GridSize := FWidth * FHeight;
      FMapData := GlobalAlloc( GPTR, GridSize * SizeOf( GridInfo ) );
      GridLoc := GlobalLock( FMapData );
      for i := 1 to GridSize do
      begin
        GridLoc^.Tile[ 0 ] := $FFFF;
        GridLoc^.Tile[ 1 ] := $FFFF;
        Inc( GridLoc );
      end;
      GlobalUnlock( FMapData );
    end;
    Zones := TList.Create;
    NewZone := TZone.Create( Self );
    Zones.Add( NewZone );
  end;
end;

destructor TAniMap.Destroy;
var
  i : Integer;
begin
//  if not (csDesigning in ComponentState) then begin
  if ( FMapData <> 0 ) then
  begin
    GlobalFree( FMapData );
    FMapData := 0;
  end;
  for i := 0 to Zones.Count - 1 do
    TZone( Zones.Items[ i ] ).Free;
  Zones.Free;
//  end;
  inherited Destroy;
end;

procedure TAniMap.SaveToStream( Stream : TStream );
var
  GridLoc : ^GridInfo;
  MemSize : longint;
begin
  Stream.write( FirstItem, sizeof( FirstItem ) );
  Stream.write( LastItem, sizeof( LastItem ) );
  Stream.write( FItemList[ 1 ], LastItem * sizeof( ItemInstanceInfo ) );
  Stream.write( FWidth, sizeof( FWidth ) );
  Stream.write( FHeight, sizeof( FHeight ) );
  MemSize := FWidth * FHeight * sizeof( GridInfo );
  GridLoc := GlobalLock( FMapData );
  Stream.write( GridLoc^, MemSize );
end;

procedure TAniMap.LoadFromStream( Stream : TStream );
var
  GridLoc : ^GridInfo;
  MemSize : longint;
begin
  Stream.read( FirstItem, sizeof( FirstItem ) );
  Stream.read( LastItem, sizeof( LastItem ) );
  Stream.read( FItemList[ 1 ], LastItem * sizeof( ItemInstanceInfo ) );
  Stream.read( FWidth, sizeof( FWidth ) );
  Stream.read( FHeight, sizeof( FHeight ) );
  FBitWidth := FWidth * FTileWidth;
  FBitHeight := FHeight * FTileHeight;

  MemSize := FWidth * FHeight * sizeof( GridInfo );
  if ( FMapData <> 0 ) then
  begin
    GlobalFree( FMapData );
    FMapData := 0;
  end;
  FMapData := GlobalAlloc( GMEM_FIXED, MemSize );
  GridLoc := GlobalLock( FMapData );
  Stream.read( GridLoc^, MemSize );
end;

function TAniMap.GetItemIndex( Item : PItemInstanceInfo ) : word;
begin
  result := 1 + ( longword( Item ) - longword( @FItemList[ 1 ] ) ) div sizeof( ItemInstanceInfo );
end;

function TAniMap.GetItemAddress( Index : word ) : PItemInstanceInfo;
begin
  result := @FItemList[ Index ];
end;

procedure TAniMap.FreeResources;
var
  i : Integer;
begin
  for i := 0 to Zones.Count - 1 do
  begin
{$IFDEF DirectX}
    TZone( Zones.Items[ i ] ).FTileImages := nil;
    TZone( Zones.Items[ i ] ).FItemImages := nil;
{$ENDIF}
{$IFNDEF DirectX}
    DeleteObject( TZone( Zones.Items[ i ] ).FTileImages );
    TZone( Zones.Items[ i ] ).FTileImages := 0;
    DeleteObject( TZone( Zones.Items[ i ] ).FTileMasks );
    TZone( Zones.Items[ i ] ).FTileMasks := 0;
    DeleteObject( TZone( Zones.Items[ i ] ).FItemImages );
    TZone( Zones.Items[ i ] ).FItemImages := 0;
    DeleteObject( TZone( Zones.Items[ i ] ).FItemMasks );
    TZone( Zones.Items[ i ] ).FItemMasks := 0;
{$ENDIF}
    TZone( Zones.Items[ i ] ).Cached := False;
    TZone( Zones.Items[ i ] ).FTileBitWidth := 0;
    TZone( Zones.Items[ i ] ).FTileBitHeight := 0;
    TZone( Zones.Items[ i ] ).FTileMaxIndex := 0;
    TZone( Zones.Items[ i ] ).FTileMaxColumnIndex := 0;
    TZone( Zones.Items[ i ] ).FItemBitWidth := 0;
    TZone( Zones.Items[ i ] ).FItemBitHeight := 0;
    TZone( Zones.Items[ i ] ).FItemColumn := 0;
    TZone( Zones.Items[ i ] ).FItemColumnBitHeight := 0;
  end;
end;

procedure TAniMap.FreeDefinitions;
var
  i : Integer;
begin
  for i := 0 to Zones.Count - 1 do
  begin
    TZone( Zones.Items[ i ] ).DisposeDef;
  end;
end;

procedure TAniMap.Clear;
var
  i : Longint;
//  GridSize: longint;
//  GridLoc: ^GridInfo;
  NewZone : TZone;
begin
  if ( FMapData <> 0 ) then
  begin
    GlobalFree( FMapData );
    FMapData := 0;
{    if (FWidth > 0) and (FHeight > 0) then begin
      GridSize := FWidth * FHeight;
      GridLoc := GlobalLock(FMapData);
      for i := 1 to GridSize do begin
        GridLoc^.Tile[0] := $FFFF;
        GridLoc^.Tile[1] := $FFFF;
        GridLoc^.CollisionMask := 0;
        GridLoc^.LineOfSightMask := 0;
        GridLoc^.Figure := nil;
        Inc(GridLoc);
      end;
      GlobalUnlock(FMapData);
    end; }
  end;
  FreeResources;
  for i := 0 to Zones.count - 1 do
    TZone( Zones.items[ i ] ).free;
  Zones.Clear;
  NewZone := TZone.Create( Self );
  Zones.Add( NewZone );
  FirstItem := 0;
  LastItem := 0;
end;

procedure TAniMap.SetWidth( VWidth : Longint );
var
  i, GridSize : Longint;
  GridLoc : ^GridInfo;
begin
  FWidth := VWidth;
  FBitWidth := FWidth * FTileWidth;
  if ( FMapData <> 0 ) then
  begin
    GlobalFree( FMapData );
    FMapData := 0;
  end;
  if ( FWidth > 0 ) and ( FHeight > 0 ) then
  begin
    GridSize := FWidth * FHeight;
    FMapData := GlobalAlloc( GPTR, GridSize * SizeOf( GridInfo ) );
    GridLoc := GlobalLock( FMapData );
    for i := 1 to GridSize do
    begin
      GridLoc^.Tile[ 0 ] := $FFFF;
      GridLoc^.Tile[ 1 ] := $FFFF;
      Inc( GridLoc );
    end;
    GlobalUnlock( FMapData );
  end;
end;

procedure TAniMap.SetHeight( VHeight : Longint );
var
  i, GridSize : Longint;
  GridLoc : ^GridInfo;
begin
  FHeight := VHeight;
  FBitHeight := FHeight * FTileHeight;
  if ( FMapData <> 0 ) then
  begin
    GlobalFree( FMapData );
    FMapData := 0;
  end;
  if ( FWidth > 0 ) and ( FHeight > 0 ) then
  begin
    GridSize := FWidth * FHeight;
    FMapData := GlobalAlloc( GPTR, GridSize * SizeOf( GridInfo ) );
    GridLoc := GlobalLock( FMapData );
    for i := 1 to GridSize do
    begin
      GridLoc^.Tile[ 0 ] := $FFFF;
      GridLoc^.Tile[ 1 ] := $FFFF;
      Inc( GridLoc );
    end;
    GlobalUnlock( FMapData );
  end;
end;

procedure TAniMap.SetAmbientColor( Color : TColor );
begin
  FAmbientColor := ColorToRGB( Color );
  LightR := ( FAmbientColor and $FF ) * ( FAmbientIntensity / 100 );
  LightG := ( ( FAmbientColor and $FF00 ) shr 8 ) * ( FAmbientIntensity / 100 );
  LightB := ( ( FAmbientColor and $FF0000 ) shr 16 ) * ( FAmbientIntensity / 100 );
end;

procedure TAniMap.SetAmbientIntensity( Value : Integer );
begin
  FAmbientIntensity := Value;
  LightR := round( ( FAmbientColor and $FF ) * ( FAmbientIntensity / 100 ) );
  LightG := round( ( ( FAmbientColor and $FF00 ) shr 8 ) * ( FAmbientIntensity / 100 ) );
  LightB := round( ( ( FAmbientColor and $FF0000 ) shr 16 ) * ( FAmbientIntensity / 100 ) );
end;

function TAniMap.GetZoneMemoryUsage( Index : integer ) : longint;
begin
  with TZone( Zones.items[ Index ] ) do
  begin
    result := FItemBitWidth * FItemBitHeight + FTileBitWidth * FTileBitHeight;
//Log.Log(inttostr(FItemBitWidth)+'x'+inttostr(FitemBitHeight));
  end;
end;

procedure TAniMap.MoveZoneToVideo( Index : integer );
begin
  if not ( TZone( Zones.items[ Index ] ).TilesInVideo and TZone( Zones.items[ Index ] ).ItemsInVideo ) then
    TZone( Zones.items[ Index ] ).MoveToVideo;
end;

function TAniMap.GetZoneCount : word;
begin
  result := Zones.count;
end;

procedure TAniMap.SetTransparentColor( Color : TColor );
begin
  FTransparentColor := Color;
  if DXMode then
  begin
    FColorMatch := FindColorMatch( Color );
    NeedColorMatch := false;
  end
  else
  begin
    NeedColorMatch := true;
  end;
end;

function TAniMap.GetColorMatch : word;
begin
  if NeedColorMatch then
  begin
    FColorMatch := FindColorMatch( FTransparentColor );
    NeedColorMatch := false;
  end;

  result := FColorMatch;
end;

function TAniMap.AddZone : Word;
var
  NewZone : TZone;
begin
  NewZone := TZone.Create( Self );
  NewZone.FIndex := Zones.Add( NewZone );
  Result := NewZone.Index;
end;

function TAniMap.AddLightZone : Word;
var
  NewZone : TZone;
begin
  NewZone := TLightZone.Create( Self );
  NewZone.FIndex := Zones.Add( NewZone );
  Result := NewZone.Index;
end;

function TAniMap.AddLight( Color : TColor; Intensity : Integer; Radius : Longint; Flicker : TFlicker; X, Y, Z : Longint ) : Word;
var
  NewZone : TLightZone;
  R2 : Longint;
begin
  Result := 0;
  if Radius <= 0 then
    Exit;
  NewZone := TLightZone.Create( Self );
  NewZone.Color := ColorToRGB( Color );
  NewZone.Intensity := Intensity;
  NewZone.Radius := Radius;
  NewZone.Flicker := Flicker;
  NewZone.State := 1;
  NewZone.X := X;
  NewZone.Y := Y;
  NewZone.Z := Z;
  NewZone.FIndex := Zones.Add( NewZone );
  NewZone.X1 := ( X - Radius ) div FTileWidth;
  if ( Radius mod FTileWidth ) = 0 then
    dec( NewZone.X1 );
  if NewZone.X1 < 0 then
    NewZone.X1 := 0;
  NewZone.X2 := ( X + Radius ) div FTileWidth;
  if NewZone.X2 >= width then
    NewZone.X2 := width - 1;

  R2 := Radius div 2;
  NewZone.Y1 := ( Y - R2 ) div FTileHeight;
  if ( R2 mod FTileHeight ) = 0 then
    dec( NewZone.Y1 );
  if NewZone.Y1 < 0 then
    NewZone.Y1 := 0;
  NewZone.Y2 := ( Y + R2 ) div FTileHeight;
  if NewZone.Y2 >= Height then
    NewZone.Y2 := Height - 1;

  case NewZone.Flicker of
{$IFDEF DirectX}
    flCandle :
      begin
        NewZone.States := 4;
        NewZone.FlickerX[ 1 ] := NewZone.X;
        NewZone.FlickerY[ 1 ] := NewZone.Y;
        NewZone.FlickerZ[ 1 ] := NewZone.Z;
        NewZone.FlickerRadius[ 1 ] := NewZone.Radius;
        NewZone.FlickerIntensity[ 1 ] := NewZone.Intensity;
        NewZone.FlickerX[ 2 ] := NewZone.X + random( 5 ) - 2;
        NewZone.FlickerY[ 2 ] := NewZone.Y + random( 5 ) - 2;
        NewZone.FlickerZ[ 2 ] := NewZone.Z + random( 2 );
        NewZone.FlickerRadius[ 2 ] := NewZone.Radius - 4;
        NewZone.FlickerIntensity[ 2 ] := 15 * NewZone.Intensity div 16;
        NewZone.FlickerX[ 3 ] := NewZone.X + random( 5 ) - 2;
        NewZone.FlickerY[ 3 ] := NewZone.Y + random( 5 ) - 2;
        NewZone.FlickerZ[ 3 ] := NewZone.Z + random( 4 );
        NewZone.FlickerRadius[ 3 ] := NewZone.Radius - 8;
        NewZone.FlickerIntensity[ 3 ] := 14 * NewZone.Intensity div 16;
        NewZone.FlickerX[ 4 ] := NewZone.X + random( 5 ) - 2;
        NewZone.FlickerY[ 4 ] := NewZone.Y + random( 5 ) - 2;
        NewZone.FlickerZ[ 4 ] := NewZone.Z + random( 4 );
        NewZone.FlickerRadius[ 4 ] := NewZone.Radius - 16;
        NewZone.FlickerIntensity[ 4 ] := 13 * NewZone.Intensity div 16;
      end;
    flTorch :
      begin
        NewZone.States := 3;
        NewZone.FlickerX[ 1 ] := NewZone.X;
        NewZone.FlickerY[ 1 ] := NewZone.Y;
        NewZone.FlickerZ[ 1 ] := NewZone.Z;
        NewZone.FlickerRadius[ 1 ] := NewZone.Radius;
        NewZone.FlickerIntensity[ 1 ] := NewZone.Intensity;
        NewZone.FlickerX[ 2 ] := NewZone.X + random( 9 ) - 4;
        NewZone.FlickerY[ 2 ] := NewZone.Y + random( 9 ) - 4;
        NewZone.FlickerZ[ 2 ] := NewZone.Z + random( 4 );
        NewZone.FlickerRadius[ 2 ] := NewZone.Radius - 8;
        NewZone.FlickerIntensity[ 2 ] := 3 * NewZone.Intensity div 4;
        NewZone.FlickerX[ 3 ] := NewZone.X + random( 9 ) - 4;
        NewZone.FlickerY[ 3 ] := NewZone.Y + random( 9 ) - 4;
        NewZone.FlickerZ[ 3 ] := NewZone.Z + random( 4 );
        NewZone.FlickerRadius[ 3 ] := NewZone.Radius - 16;
        NewZone.FlickerIntensity[ 3 ] := NewZone.Intensity div 2;
      end;
    flFluorescent :
      begin
        NewZone.States := 2;
        NewZone.FlickerX[ 1 ] := NewZone.X;
        NewZone.FlickerY[ 1 ] := NewZone.Y;
        NewZone.FlickerZ[ 1 ] := NewZone.Z;
        NewZone.FlickerRadius[ 1 ] := NewZone.Radius;
        NewZone.FlickerIntensity[ 1 ] := NewZone.Intensity;
        NewZone.FlickerX[ 2 ] := NewZone.X;
        NewZone.FlickerY[ 2 ] := NewZone.Y;
        NewZone.FlickerZ[ 2 ] := NewZone.Z;
        NewZone.FlickerRadius[ 2 ] := NewZone.Radius;
        NewZone.FlickerIntensity[ 2 ] := 0;
      end;
{$ENDIF}
    flNone :
      begin
        NewZone.States := 1;
        NewZone.FlickerX[ 1 ] := NewZone.X;
        NewZone.FlickerY[ 1 ] := NewZone.Y;
        NewZone.FlickerZ[ 1 ] := NewZone.Z;
        NewZone.FlickerRadius[ 1 ] := NewZone.Radius;
        NewZone.FlickerIntensity[ 1 ] := NewZone.Intensity;
      end;
  else
    begin
      NewZone.States := 1;
      NewZone.FlickerX[ 1 ] := NewZone.X;
      NewZone.FlickerY[ 1 ] := NewZone.Y;
      NewZone.FlickerZ[ 1 ] := NewZone.Z;
      NewZone.FlickerRadius[ 1 ] := NewZone.Radius;
      NewZone.FlickerIntensity[ 1 ] := NewZone.Intensity;
    end;
  end;

  Result := NewZone.Index;
end;

procedure TAniMap.SaveMapKnownInfo( Stream : TStream );
var
  GridSize, i : Longint;
  GridLoc : ^GridInfo;
  Bits : longword;
begin
  if ( FWidth > 0 ) and ( FHeight > 0 ) then
  begin
    GridSize := FWidth * FHeight;
    GridLoc := GlobalLock( FMapData );
    Bits := 0;
    for i := 1 to GridSize do
    begin
      Bits := Bits shl 1;
      if ( GridLoc^.BitField and $40 ) <> 0 then
        Bits := Bits or 1;
      if ( i mod 32 ) = 0 then
      begin
        Stream.write( Bits, sizeof( Bits ) );
        Bits := 0;
      end;
      Inc( GridLoc );
    end;
    GlobalUnlock( FMapData );
  end;
end;

procedure TAniMap.LoadMapKnownInfo( Stream : TStream );
var
  GridSize, i : Longint;
  GridLoc : ^GridInfo;
  Bits : longword;
begin
  if ( FWidth > 0 ) and ( FHeight > 0 ) then
  begin
    GridSize := FWidth * FHeight;
    GridLoc := GlobalLock( FMapData );
    Bits := 0;
    for i := 1 to GridSize do
    begin
      if ( i mod 32 ) = 1 then
      begin
        Stream.Read( Bits, sizeof( Bits ) );
      end;
      if ( Bits and $80000000 ) <> 0 then
        GridLoc^.BitField := GridLoc^.BitField or $40;
      Bits := Bits shl 1;
      Inc( GridLoc );
    end;
    GlobalUnlock( FMapData );
  end;
end;

procedure TAniMap.RenderMap;
var
  GridBase, p : ^GridInfo;
  ZoneTile : TZone;
  Index : Word;
  tX, tY : word;
  SrcX, SrcY : Longint;
  X1, Y1, Z1 : longint;
  X2, Y2 : Longint;
  i, j, k, m : Longint;
  RL, GL, BL : word;
  R1, G1, B1 : word;
  D : Double;
  IL1 : integer;
  Overlap, OverlapTile, LightZones : TList;
  CurrentZone, Test : TLightZone;
  Zone : Word;
  CurrentIndex : Integer;
  OVERLAPPED : Boolean;
  X, Y : Longint;
  NewTileIndex, ItemCount : Word;
  HasLayer1 : Boolean;
  Layer : Integer;
  A, A1, A2, Slope : Single;
  State : Integer;
  ZoneX, ZoneY, ZoneZ : Longint;
  ZoneIntensity : double;
  ZoneRadius : Longint;
  HalfWidth, HalfHeight : Integer;
  SortedZones : TList;
  ColorMatch : word;
  DblColorMatch : longword;
  BltFx : DDBLTFX;
  ddsd : TDDSurfaceDesc;
  C16 : word;
  p16 : ^word;
  TempSurface : IDirectDrawSurface;
  LightR1, LightG1, LightB1 : word;
  Pitch : longint;
  TimeCount : longword;
begin
  if not UseLighting then
    Exit;

  LightR1 := round( LightR );
  LightG1 := round( LightG );
  LightB1 := round( LightB );

  HalfWidth := FTileWidth div 2;
  HalfHeight := FTileHeight div 2;
  GridBase := GlobalLock( FMapData );

  TransparentColor := ColorToRGB( FTransparentColor );
  ColorMatch := self.ColorMatch;
  DblColorMatch := ColorMatch or ( DblColorMatch shl 16 );

  if not FUseAmbientOnly then
  begin
    OverlapTile := TList.Create;
    LightZones := TList.Create;
    //Make sure flicker lighting is evaluated last
    SortedZones := TList.create;
    for Zone := 0 to Zones.Count - 1 do
    begin
      ZoneTile := Zones.Items[ Zone ];
      if ZoneTile is TLightZone then
      begin
        if TLightZone( ZoneTile ).States = 1 then
          SortedZones.add( ZoneTile );
      end
      else
        SortedZones.add( ZoneTile );
    end;
    for Zone := 0 to Zones.Count - 1 do
    begin
      ZoneTile := Zones.Items[ Zone ];
      if ZoneTile is TLightZone then
      begin
        if TLightZone( ZoneTile ).States > 1 then
          SortedZones.add( ZoneTile );
      end;
    end;

    //Apply lighting to tiles
    Log.Log( 'Start Tiles' );
    TimeCount := GetTickCount;
    for Zone := 1 to SortedZones.Count - 1 do
    begin
      ZoneTile := SortedZones.Items[ Zone ];
      if ZoneTile is TLightZone then
      begin
        CurrentZone := SortedZones.Items[ Zone ];
        LightZones.Add( CurrentZone );
        Overlap := TList.Create;
        Overlap.Add( currentZone );
        NewTileIndex := 0;
        CurrentIndex := -1;
        for i := 1 to SortedZones.Count - 1 do
        begin
          if ( i <> Zone ) then
          begin
            ZoneTile := SortedZones.Items[ i ];
            if ZoneTile is TLightZone then
            begin
              Test := SortedZones.Items[ i ];
              if ( Test.X1 <= CurrentZone.X2 ) and ( Test.X2 >= CurrentZone.X1 ) and
                ( Test.Y1 <= CurrentZone.Y2 ) and ( Test.Y2 > CurrentZone.Y1 ) then
              begin
                if i <= Zone then
                  CurrentIndex := Overlap.Add( Test )
                else
                  Overlap.Add( Test );
              end;
            end;
          end;
          TLightZone( CurrentZone ).OverlapZones := Overlap;
        end;

        TempSurface := DDGetSurface( lpDD, FTileWidth, FTileHeight, TransparentColor, false );
        for State := 1 to CurrentZone.States do
        begin
          HasLayer1 := False;
          for Layer := 0 to 1 do
          begin
            if ( Layer = 0 ) or ( ( Layer = 1 ) and HasLayer1 ) then
            begin
              for Y := CurrentZone.Y1 to CurrentZone.Y2 do
              begin
                p := GridBase;
                Inc( p, Y * FWidth + CurrentZone.X1 );
                for X := CurrentZone.X1 to CurrentZone.X2 do
                begin
                  OverlapTile.Clear;
                  OVERLAPPED := False;
                  for i := 1 to Overlap.Count - 1 do
                  begin
                    Test := Overlap.Items[ i ];
                    if ( Test.X1 < X + 1 ) and ( Test.X2 >= X ) and
                      ( Test.Y1 < Y + 1 ) and ( Test.Y2 >= Y ) then
                    begin
                      OverlapTile.Add( Test );
                      if i > CurrentIndex then
                      begin
                        OVERLAPPED := True;
                        Break;
                      end;
                    end;
                  end;
                  if not OVERLAPPED then
                  begin
                    HasLayer1 := HasLayer1 or ( p^.Tile[ 1 ] <> $FFFF );
                    if ( ( p^.BitField and $80 ) <> 0 ) then
                      HasLayer1 := HasLayer1 or ( p^.Tile[ 2 ] <> $FFFF ) or ( p^.Tile[ 3 ] <> $FFFF ) or ( p^.Tile[ 4 ] <> $FFFF );
                    Index := p^.Tile[ Layer ];
                    if ( Index <> $FFFF ) or ( ( ( p^.BitField and $80 ) <> 0 ) and ( Layer = 1 ) ) then
                    begin
                      OverlapTile.Add( CurrentZone );
                      ZoneTile := Zones.Items[ p^.Zone[ Layer ] ];
                      if ( ( ( p^.BitField and $80 ) <> 0 ) and ( Layer = 1 ) ) then
                      begin
                        BltFx.dwSize := SizeOf( BltFx );
                        BltFx.dwFillColor := ColorMatch;
                        TempSurface.Blt( Rect( 0, 0, FTileWidth, FTileHeight ), nil, Rect( 0, 0, FTileWidth, FTileHeight ), DDBLT_COLORFILL + DDBLT_WAIT, BltFx );
                        if ( Index <> $FFFF ) then
                        begin
                          SrcX := ( Index div ZoneTile.FTileMaxColumnIndex ) * FTileWidth;
                          SrcY := ( Index mod ZoneTile.FTileMaxColumnIndex ) * FTileHeight + HalfHeight;
                          TempSurface.BltFast( 0, 0, ZoneTile.FTileImages, Rect( SrcX, SrcY, SrcX + FTileWidth, SrcY + HalfHeight ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
                        end;
                        Index := p^.Tile[ 2 ];
                        if ( Index <> $FFFF ) then
                        begin
                          SrcX := ( Index div ZoneTile.FTileMaxColumnIndex ) * FTileWidth;
                          SrcY := ( Index mod ZoneTile.FTileMaxColumnIndex ) * FTileHeight;
                          ZoneTile := Zones.Items[ p^.Zone[ 2 ] ];
                          TempSurface.BltFast( HalfWidth, 0, ZoneTile.FTileImages, Rect( SrcX, SrcY, SrcX + HalfWidth, SrcY + FTileHeight ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
                        end;
                        Index := p^.Tile[ 3 ];
                        if ( Index <> $FFFF ) then
                        begin
                          SrcX := ( Index div ZoneTile.FTileMaxColumnIndex ) * FTileWidth;
                          SrcY := ( Index mod ZoneTile.FTileMaxColumnIndex ) * FTileHeight;
                          ZoneTile := Zones.Items[ p^.Zone[ 3 ] ];
                          TempSurface.BltFast( 0, HalfHeight, ZoneTile.FTileImages, Rect( SrcX, SrcY, SrcX + FTileWidth, SrcY + HalfHeight ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
                        end;
                        Index := p^.Tile[ 4 ];
                        if ( Index <> $FFFF ) then
                        begin
                          SrcX := ( Index div ZoneTile.FTileMaxColumnIndex ) * FTileWidth + HalfWidth;
                          SrcY := ( Index mod ZoneTile.FTileMaxColumnIndex ) * FTileHeight;
                          ZoneTile := Zones.Items[ p^.Zone[ 4 ] ];
                          TempSurface.BltFast( 0, 0, ZoneTile.FTileImages, Rect( SrcX, SrcY, SrcX + HalfWidth, SrcY + FTileHeight ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
                        end;
                      end
                      else
                      begin
                        SrcX := ( Index div ZoneTile.FTileMaxColumnIndex ) * FTileWidth;
                        SrcY := ( Index mod ZoneTile.FTileMaxColumnIndex ) * FTileHeight;
                        TempSurface.BltFast( 0, 0, ZoneTile.FTileImages, Rect( SrcX, SrcY, SrcX + FTileWidth, SrcY + FTileHeight ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
                      end;

                      //Render Tile
                      ddsd.dwSize := SizeOf( ddsd );
                      if TempSurface.Lock( nil, ddsd, DDLOCK_WAIT, 0 ) = DD_OK then
                      begin
                        try
                          for j := 0 to FTileHeight - 1 do
                          begin
                            Y2 := j + Y * FTileHeight;
                            p16 := ddsd.lpSurface;
                            inc( PChar( p16 ), j * ddsd.lPitch );
                            for i := 0 to FTileWidth - 1 do
                            begin
                              X2 := i + X * FTileWidth;
                              C16 := p16^;
                              if C16 <> ColorMatch then
                              begin
                                R1 := LightR1;
                                G1 := LightG1;
                                B1 := LightB1;
                                for k := 0 to OverlapTile.Count - 1 do
                                begin
                                  Test := OverlapTile.Items[ k ];
                                  if Test = CurrentZone then
                                  begin
                                    ZoneX := Test.FlickerX[ State ];
                                    ZoneY := Test.FlickerY[ State ];
                                    ZoneZ := Test.FlickerZ[ State ];
                                    ZoneRadius := Test.FlickerRadius[ State ];
                                    ZoneIntensity := Test.FlickerIntensity[ State ] / 100;
                                  end
                                  else
                                  begin
                                    ZoneX := Test.X;
                                    ZoneY := Test.Y;
                                    ZoneZ := Test.Z;
                                    ZoneRadius := Test.Radius;
                                    ZoneIntensity := Test.Intensity / 100;
                                  end;
                                  if ZoneIntensity > 0 then
                                  begin
                                    RL := Test.Color and $FF;
                                    GL := Test.Color and $FF00 shr 8;
                                    BL := Test.Color and $FF0000 shr 16;
                                    X1 := sqr( ZoneX - X2 );
                                    Y1 := sqr( ( ZoneY - Y2 ) * 2 );
                                    Z1 := sqr( ZoneZ );
                                    D := sqrt( X1 + Y1 + Z1 ) / ZoneRadius;
                                    if D <= 1 then
                                    begin
                                      if LineOfCollision( ZoneX, ZoneY, X2, Y2 ) then
                                      begin
                                        IL1 := trunc( ( 1 - D ) * ZoneIntensity * 256 );
                                        inc( R1, ( IL1 * RL ) shr 8 );
                                        inc( G1, ( IL1 * GL ) shr 8 );
                                        inc( B1, ( IL1 * BL ) shr 8 );
                                      end;
                                    end;
                                  end;
                                end;
                                if ( R1 <> $FF ) or ( G1 <> $FF ) or ( B1 <> $FF ) then
                                begin
                                  if PixelFormat = pf555 then
                                    asm
                                    push    EBX
                                    mov     BX,C16
                                    mov     AL,BL
                                    and     EAX,$1F
                                    mul     B1
                                    test    EAX,$FFFFE000
                                    jz      @@StoreBlue
                                    mov     CX,$1F
                                    jmp     @@Green
                                  @@StoreBlue:
                                    shr     AX,8
                                    mov     CX,AX
                                  @@Green:
                                    mov     AX,BX
                                    shr     AX,5
                                    and     EAX,$1F  //*
                                    mul     G1
                                    test    EAX,$FFFFE000 //*
                                    jz      @@StoreGreen
                                    or      CX,$3E0 //*
                                    jmp     @@Red
                                  @@StoreGreen:
                                    shr     AX,3    //*
                                    and     AX,$3E0 //*
                                    or      CX,AX
                                  @@Red:
                                    xor     EAX,EAX
                                    mov     AH,BH
                                    shr     AX,10 //*
                                    mul     R1
                                    test    EAX,$FFFFE000
                                    jz      @@StoreRed
                                    or      CH,$F8
                                    jmp     @@Continue
                                  @@StoreRed:
                                    shl     AH,2 //*
                                    or      CH,AH
                                  @@Continue:
                                    mov     EAX,p16
                                    mov     [EAX],CX
                                    pop     EBX
                                    end
                                  else
                                    asm
                                    push    EBX
                                    mov     BX,C16
                                    mov     AL,BL
                                    and     EAX,$1F
                                    mul     B1
                                    test    EAX,$FFFFE000
                                    jz      @@StoreBlue
                                    mov     CX,$1F
                                    jmp     @@Green
                                  @@StoreBlue:
                                    shr     AX,8
                                    mov     CX,AX
                                  @@Green:
                                    mov     AX,BX
                                    shr     AX,5
                                    and     EAX,$3F  //*
                                    mul     G1
                                    test    EAX,$FFFFC000 //*
                                    jz      @@StoreGreen
                                    or      CX,$7E0 //*
                                    jmp     @@Red
                                  @@StoreGreen:
                                    shr     AX,3    //*
                                    and     AX,$7E0 //*
                                    or      CX,AX
                                  @@Red:
                                    xor     EAX,EAX
                                    mov     AH,BH
                                    shr     AX,11 //*
                                    mul     R1
                                    test    EAX,$FFFFE000
                                    jz      @@StoreRed
                                    or      CH,$F8
                                    jmp     @@Continue
                                  @@StoreRed:
                                    shl     AH,3 //*
                                    or      CH,AH
                                  @@Continue:
                                    mov     EAX,p16
                                    mov     [EAX],CX
                                    pop     EBX
                                    end;
                                end;
                              end;
                              inc( p16 );
                            end;
                          end;
                        finally
                          TempSurface.UnLock( nil );
                        end;
                      end;

                      if ( State = CurrentZone.States ) then
                      begin //only update grid on last state
                        p^.Zone[ Layer ] := CurrentZone.Index;
                        p^.Tile[ Layer ] := NewTileIndex;
                        if ( Layer = 1 ) then
                          p^.BitField := p^.BitField and $7F; //This is no longer a diamond tile
                      end;
                      Inc( NewTileIndex );
                      CurrentZone.DefineTile( NewTileIndex, TempSurface, FTransparentColor );
                    end;
                  end;
                  Inc( p );
                end; //X
              end; //Y
            end; //Has layer
          end; //Layer loop
          CurrentZone.NewTileState;
        end; //State loop
        TempSurface := nil;

      end;
    end;
    GlobalUnlock( FMapData );
    SortedZones.free;
    TimeCount := GetTickCount - TimeCount;
    Log.Log( 'End Tiles: ' + inttostr( TimeCount ) );

    //Apply lighting to items
    Log.Log( 'Start Items' );
    TimeCount := GetTickCount;
    ItemCount := 0;
    m := FStripWidth div 2;
    if ( LightZones.Count > 0 ) then
    begin
      for State := 1 to MaxLightStates do
      begin
        i := FirstItem;
        while ( i <> 0 ) do
        begin
          ZoneTile := Zones.Items[ FItemList[ i ].Zone ];
          X := FItemList[ i ].X div FTileWidth;
          Y := FItemList[ i ].Y div FTileHeight;
          OverlapTile.Clear;
          for j := 0 to LightZones.Count - 1 do
          begin
            Test := LightZones[ j ];
            if Test.States >= State then
            begin
              if ( X >= Test.X1 ) and ( X <= Test.X2 ) and
                ( Y >= Test.Y1 ) and ( Y <= Test.Y2 ) then
              begin
                OverlapTile.Add( Test );
              end;
            end;
          end;
          if OverlapTile.Count > 0 then
          begin
            Zone := TZone( OverlapTile[ OverlapTile.Count - 1 ] ).Index;
            TempSurface := DDGetSurface( lpDD, FStripWidth, FItemList[ i ].Height, TransparentColor, false );
            TempSurface.BltFast( 0, 0, ZoneTile.FItemImages,
              Rect( FItemList[ i ].ImageX, FItemList[ i ].ImageY, FItemList[ i ].ImageX + FStripWidth,
              FItemList[ i ].ImageY + FItemList[ i ].Height ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
            ddsd.dwSize := SizeOf( ddsd );
            if TempSurface.Lock( nil, ddsd, DDLOCK_WAIT, 0 ) = DD_OK then
            begin
              try
                for X := 0 to FStripWidth - 1 do
                begin
                  X2 := FItemList[ i ].X + X;
                  if ( X = 0 ) then
                  begin
                    Slope := FItemList[ i ].Slope1;
                    A := ArcTan( Slope );
                    if ( Slope > 0 ) then
                    begin
                      A2 := A;
                      A1 := A + PI;
                    end
                    else
                    begin
                      A2 := 2 * PI + A;
                      A1 := A2 - PI;
                    end;
                  end
                  else if ( X = m ) then
                  begin
                    Slope := FItemList[ i ].Slope2;
                    A := ArcTan( Slope );
                    if ( Slope > 0 ) then
                    begin
                      A2 := A;
                      A1 := A + PI;
                    end
                    else
                    begin
                      A2 := 2 * PI + A;
                      A1 := A2 - PI;
                    end;
                  end;
                  p16 := ddsd.lpSurface;
                  inc( p16, X );
                  if FItemList[ i ].Vertical then
                    Y2 := FItemList[ i ].Y + Round( ( X - m ) * Slope ) //If vertical, Y2 remains constant
                  else //so only calculate once
                    Y2 := 0;
                  for Y := 0 to FItemList[ i ].Height - 1 do
                  begin
                    if not FItemList[ i ].Vertical then
                      Y2 := FItemList[ i ].Y - FItemList[ i ].VHeight + Y;
                    C16 := p16^;
                    if C16 <> ColorMatch then
                    begin
                      R1 := LightR1;
                      G1 := LightG1;
                      B1 := LightB1;
                      for j := 0 to OverlapTile.Count - 1 do
                      begin
                        Test := OverlapTile.Items[ j ];
                        if Test = Zones.Items[ Zone ] then
                        begin
                          ZoneX := Test.FlickerX[ State ];
                          ZoneY := Test.FlickerY[ State ];
                          ZoneZ := Test.FlickerZ[ State ];
                          ZoneRadius := Test.FlickerRadius[ State ];
                          ZoneIntensity := Test.FlickerIntensity[ State ] / 100;
                        end
                        else
                        begin
                          ZoneX := Test.X;
                          ZoneY := Test.Y;
                          ZoneZ := Test.Z;
                          ZoneRadius := Test.Radius;
                          ZoneIntensity := Test.Intensity / 100;
                        end;
                        if ZoneIntensity > 0 then
                        begin
                          A := ATan( X2 - ZoneX, Y2 - ZoneY );
                          if ( ( A2 >= A1 ) and ( ( A > A1 ) and ( A < A2 ) ) ) or ( ( A2 < A1 ) and ( ( A < A2 ) or ( A > A1 ) ) ) then
                          begin
                            RL := Test.Color and $FF;
                            GL := Test.Color and $FF00 shr 8;
                            BL := Test.Color and $FF0000 shr 16;
                            X1 := sqr( ZoneX - X2 );
                            Y1 := sqr( ( ZoneY - Y2 ) * 2 );
                            if FItemList[ i ].Vertical then
                              Z1 := sqr( Y2 - FItemList[ i ].Y + FItemList[ i ].VHeight - Y - ZoneZ - 1 )
                            else
                              Z1 := sqr( ZoneZ );
                            D := sqrt( X1 + Y1 + Z1 ) / ZoneRadius;
                            if D <= 1 then
                            begin
                              if LineOfCollision( ZoneX, ZoneY, X2, Y2 + 4 ) then
                              begin
                                IL1 := trunc( ( 1 - D ) * ZoneIntensity * 256 );
                                inc( R1, ( IL1 * RL ) shr 8 );
                                inc( G1, ( IL1 * GL ) shr 8 );
                                inc( B1, ( IL1 * BL ) shr 8 );
                              end;
                            end;
                          end;
                        end;
                      end;
                      if ( R1 <> $FF ) or ( G1 <> $FF ) or ( B1 <> $FF ) then
                      begin
                        if PixelFormat = pf555 then
                          asm
                          push    EBX
                          mov     BX,C16
                          mov     AL,BL
                          and     EAX,$1F
                          mul     B1
                          test    EAX,$FFFFE000
                          jz      @@StoreBlue
                          mov     CX,$1F
                          jmp     @@Green
                        @@StoreBlue:
                          shr     AX,8
                          mov     CX,AX
                        @@Green:
                          mov     AX,BX
                          shr     AX,5
                          and     EAX,$1F  //*
                          mul     G1
                          test    EAX,$FFFFE000 //*
                          jz      @@StoreGreen
                          or      CX,$3E0 //*
                          jmp     @@Red
                        @@StoreGreen:
                          shr     AX,3    //*
                          and     AX,$3E0 //*
                          or      CX,AX
                        @@Red:
                          xor     EAX,EAX
                          mov     AH,BH
                          shr     AX,10 //*
                          mul     R1
                          test    EAX,$FFFFE000
                          jz      @@StoreRed
                          or      CH,$F8
                          jmp     @@Continue
                        @@StoreRed:
                          shl     AH,2 //*
                          or      CH,AH
                        @@Continue:
                          mov     EAX,p16
                          mov     [EAX],CX
                          pop     EBX
                          end
                        else
                          asm
                          push    EBX
                          mov     BX,C16
                          mov     AL,BL
                          and     EAX,$1F
                          mul     B1
                          test    EAX,$FFFFE000
                          jz      @@StoreBlue
                          mov     CX,$1F
                          jmp     @@Green
                        @@StoreBlue:
                          shr     AX,8
                          mov     CX,AX
                        @@Green:
                          mov     AX,BX
                          shr     AX,5
                          and     EAX,$3F  //*
                          mul     G1
                          test    EAX,$FFFFC000 //*
                          jz      @@StoreGreen
                          or      CX,$7E0 //*
                          jmp     @@Red
                        @@StoreGreen:
                          shr     AX,3    //*
                          and     AX,$7E0 //*
                          or      CX,AX
                        @@Red:
                          xor     EAX,EAX
                          mov     AH,BH
                          shr     AX,11 //*
                          mul     R1
                          test    EAX,$FFFFE000
                          jz      @@StoreRed
                          or      CH,$F8
                          jmp     @@Continue
                        @@StoreRed:
                          shl     AH,3 //*
                          or      CH,AH
                        @@Continue:
                          mov     EAX,p16
                          mov     [EAX],CX
                          pop     EBX
                          end;
                      end;
                    end;
                    inc( PChar( p16 ), ddsd.lPitch );
                  end;
                end;
              finally
                TempSurface.UnLock( nil );
              end;
            end;
            if ( State = TLightZone( Zones.Items[ Zone ] ).States ) then
            begin
              TLightZone( Zones[ Zone ] ).AddStrip( TempSurface, FItemList[ i ].ImageX, FItemList[ i ].ImageY );
              FItemList[ i ].Zone := Zone;
            end
            else
            begin
              TLightZone( Zones[ Zone ] ).AddStrip( TempSurface, tX, tY );
            end;
            Inc( ItemCount );
            TempSurface := nil;
          end;
          i := FItemList[ i ].Next;
        end;
        for j := 0 to LightZones.Count - 1 do
        begin
          Test := LightZones[ j ];
          if Test.States >= State then
          begin
            Test.NewItemState;
          end;
        end;
      end;
    end;
    OverlapTile.Free;
    TimeCount := GetTickCount - TimeCount;
    Log.Log( 'End Items: ' + inttostr( TimeCount ) );

    //Construct item list for all light zones
    if LightZones.Count > 0 then
    begin
      i := FirstItem;
      while ( i <> 0 ) do
      begin
        ZoneTile := Zones.Items[ FItemList[ i ].Zone ];
        if ZoneTile is TLightZone then
        begin
          Test := TLightZone( ZoneTile );
          if not Assigned( Test.Items ) then
            Test.Items := TList.Create;
          Test.Items.Add( @FItemList[ i ] );
        end;
        i := FItemList[ i ].Next;
      end;
    end;
  end;

  if ( LightR <> $FF ) or ( LightG <> $FF ) or ( LightB <> $FF ) then
  begin
    //Render ambient color in all zones
    Log.Log( 'Start Ambient' );
    TimeCount := GetTickCount;
    for Zone := 0 to Zones.Count - 1 do
    begin
      ZoneTile := Zones.Items[ Zone ];
      if not ( ZoneTile is TLightZone ) then
      begin
        //Do tiles first
        if Assigned( ZoneTile.FTileImages ) then
        begin
          ddsd.dwSize := SizeOf( ddsd );
          if ZoneTile.FTileImages.Lock( nil, ddsd, DDLOCK_WAIT, 0 ) = DD_OK then
          begin
            try
              j := ZoneTile.FTileBitHeight;
              i := ZoneTile.FTileBitWidth;
              p16 := ddsd.lpSurface;
              Pitch := ddsd.lPitch;
              if ( i > 0 ) and ( j > 0 ) then
              begin
                if PixelFormat = pf555 then
                  asm
                  push    EBX
                  push    ESI
                  push    EDI

                  mov     ECX,j
                @@OuterLoop:
                  push    ECX
                  dec     ECX
                  mov     EAX,Pitch
                  mul     ECX
                  mov     ESI,p16
                  add     ESI,EAX

                  mov     EDI,i
                @@InnerLoop:
                  mov     EBX,[ESI]
                  cmp     EBX,DblColorMatch
                  je      @@Next2
                  mov     ECX,EBX
                  cmp     BX,ColorMatch
                  je      @@Next1
                  mov     AL,BL
                  and     EAX,$1F
                  mul     LightB1
                  test    EAX,$FFFFE000
                  jz      @@StoreBlue1
                  mov     CX,$1F
                  jmp     @@Green1
                @@StoreBlue1:
                  shr     AX,8
                  mov     CX,AX
                @@Green1:
                  mov     AX,BX
                  shr     AX,5
                  and     EAX,$1F  //*
                  mul     LightG1
                  test    EAX,$FFFFE000 //*
                  jz      @@StoreGreen1
                  or      CX,$3E0 //*
                  jmp     @@Red1
                @@StoreGreen1:
                  shr     AX,3    //*
                  and     AX,$3E0 //*
                  or      CX,AX
                @@Red1:
                  xor     EAX,EAX
                  mov     AH,BH
                  shr     AX,10 //*
                  mul     LightR1
                  test    EAX,$FFFFE000
                  jz      @@StoreRed1
                  or      CH,$F8
                  jmp     @@Next1
                @@StoreRed1:
                  shl     AH,2 //*
                  or      CH,AH

                @@Next1:
                  rol     ECX,16
                  rol     EBX,16
                  cmp     BX,ColorMatch
                  je      @@Continue
                  mov     AL,BL
                  and     EAX,$1F
                  mul     LightB1
                  test    EAX,$FFFFE000
                  jz      @@StoreBlue2
                  mov     CX,$1F
                  jmp     @@Green2
                @@StoreBlue2:
                  shr     AX,8
                  mov     CX,AX
                @@Green2:
                  mov     AX,BX
                  shr     AX,5
                  and     EAX,$1F  //*
                  mul     LightG1
                  test    EAX,$FFFFE000 //*
                  jz      @@StoreGreen2
                  or      CX,$3E0 //*
                  jmp     @@Red2
                @@StoreGreen2:
                  shr     AX,3    //*
                  and     AX,$3E0 //*
                  or      CX,AX
                @@Red2:
                  xor     EAX,EAX
                  mov     AH,BH
                  shr     AX,10 //*
                  mul     LightR1
                  test    EAX,$FFFFE000
                  jz      @@StoreRed2
                  or      CH,$F8
                  jmp     @@Continue
                @@StoreRed2:
                  shl     AH,2 //*
                  or      CH,AH
                @@Continue:
                  ror     ECX,16
                  mov     [ESI],ECX
                @@Next2:
                  add     ESI,4
                  sub     EDI,2
                  jnz     @@InnerLoop

                  pop     ECX
                  dec     ECX
                  jnz     @@OuterLoop

                  pop     EDI
                  pop     ESI
                  pop     EBX
                  end
                else
                  asm
                  push    EBX
                  push    ESI
                  push    EDI

                  mov     ECX,j
                @@OuterLoop:
                  push    ECX
                  dec     ECX
                  mov     EAX,Pitch
                  mul     ECX
                  mov     ESI,p16
                  add     ESI,EAX

                  mov     EDI,i
                @@InnerLoop:
                  mov     EBX,[ESI]
                  cmp     EBX,DblColorMatch
                  je      @@Next2
                  mov     ECX,EBX
                  cmp     BX,ColorMatch
                  je      @@Next1
                  mov     AL,BL
                  and     EAX,$1F
                  mul     LightB1
                  test    EAX,$FFFFE000
                  jz      @@StoreBlue1
                  mov     CX,$1F
                  jmp     @@Green1
                @@StoreBlue1:
                  shr     AX,8
                  mov     CX,AX
                @@Green1:
                  mov     AX,BX
                  shr     AX,5
                  and     EAX,$3F  //*
                  mul     LightG1
                  test    EAX,$FFFFC000 //*
                  jz      @@StoreGreen1
                  or      CX,$7E0 //*
                  jmp     @@Red1
                @@StoreGreen1:
                  shr     AX,3    //*
                  and     AX,$7E0 //*
                  or      CX,AX
                @@Red1:
                  xor     EAX,EAX
                  mov     AH,BH
                  shr     AX,11 //*
                  mul     LightR1
                  test    EAX,$FFFFE000
                  jz      @@StoreRed1
                  or      CH,$F8
                  jmp     @@Next1
                @@StoreRed1:
                  shl     AH,3 //*
                  or      CH,AH

                @@Next1:
                  rol     ECX,16
                  rol     EBX,16
                  cmp     BX,ColorMatch
                  je      @@Continue
                  mov     AL,BL
                  and     EAX,$1F
                  mul     LightB1
                  test    EAX,$FFFFE000
                  jz      @@StoreBlue2
                  mov     CX,$1F
                  jmp     @@Green2
                @@StoreBlue2:
                  shr     AX,8
                  mov     CX,AX
                @@Green2:
                  mov     AX,BX
                  shr     AX,5
                  and     EAX,$3F  //*
                  mul     LightG1
                  test    EAX,$FFFFC000 //*
                  jz      @@StoreGreen2
                  or      CX,$7E0 //*
                  jmp     @@Red2
                @@StoreGreen2:
                  shr     AX,3    //*
                  and     AX,$7E0 //*
                  or      CX,AX
                @@Red2:
                  xor     EAX,EAX
                  mov     AH,BH
                  shr     AX,11 //*
                  mul     LightR1
                  test    EAX,$FFFFE000
                  jz      @@StoreRed2
                  or      CH,$F8
                  jmp     @@Continue
                @@StoreRed2:
                  shl     AH,3 //*
                  or      CH,AH
                @@Continue:
                  ror     ECX,16
                  mov     [ESI],ECX
                @@Next2:
                  add     ESI,4
                  sub     EDI,2
                  jnz     @@InnerLoop

                  pop     ECX
                  dec     ECX
                  jnz     @@OuterLoop

                  pop     EDI
                  pop     ESI
                  pop     EBX
                  end;
              end;
            finally
              ZoneTile.FTileImages.UnLock( nil );
            end;
          end;
        end;

        //Do Items
        if Assigned( ZoneTile.FItemImages ) then
        begin
          ddsd.dwSize := SizeOf( ddsd );
          if ZoneTile.FItemImages.Lock( nil, ddsd, DDLOCK_WAIT, 0 ) = DD_OK then
          begin
            try
              j := ZoneTile.FItemBitHeight;
              i := ZoneTile.FItemBitWidth;
              p16 := ddsd.lpSurface;
              Pitch := ddsd.lPitch;
              if ( i > 0 ) and ( j > 0 ) then
              begin
                if PixelFormat = pf555 then
                  asm
                  push    EBX
                  push    ESI
                  push    EDI

                  mov     ECX,j
                @@OuterLoop:
                  push    ECX
                  dec     ECX
                  mov     EAX,Pitch
                  mul     ECX
                  mov     ESI,p16
                  add     ESI,EAX

                  mov     EDI,i
                @@InnerLoop:
                  mov     EBX,[ESI]
                  cmp     EBX,DblColorMatch
                  je      @@Next2
                  mov     ECX,EBX
                  cmp     BX,ColorMatch
                  je      @@Next1
                  mov     AL,BL
                  and     EAX,$1F
                  mul     LightB1
                  test    EAX,$FFFFE000
                  jz      @@StoreBlue1
                  mov     CX,$1F
                  jmp     @@Green1
                @@StoreBlue1:
                  shr     AX,8
                  mov     CX,AX
                @@Green1:
                  mov     AX,BX
                  shr     AX,5
                  and     EAX,$1F  //*
                  mul     LightG1
                  test    EAX,$FFFFE000 //*
                  jz      @@StoreGreen1
                  or      CX,$3E0 //*
                  jmp     @@Red1
                @@StoreGreen1:
                  shr     AX,3    //*
                  and     AX,$3E0 //*
                  or      CX,AX
                @@Red1:
                  xor     EAX,EAX
                  mov     AH,BH
                  shr     AX,10 //*
                  mul     LightR1
                  test    EAX,$FFFFE000
                  jz      @@StoreRed1
                  or      CH,$F8
                  jmp     @@Next1
                @@StoreRed1:
                  shl     AH,2 //*
                  or      CH,AH

                @@Next1:
                  rol     ECX,16
                  rol     EBX,16
                  cmp     BX,ColorMatch
                  je      @@Continue
                  mov     AL,BL
                  and     EAX,$1F
                  mul     LightB1
                  test    EAX,$FFFFE000
                  jz      @@StoreBlue2
                  mov     CX,$1F
                  jmp     @@Green2
                @@StoreBlue2:
                  shr     AX,8
                  mov     CX,AX
                @@Green2:
                  mov     AX,BX
                  shr     AX,5
                  and     EAX,$1F  //*
                  mul     LightG1
                  test    EAX,$FFFFE000 //*
                  jz      @@StoreGreen2
                  or      CX,$3E0 //*
                  jmp     @@Red2
                @@StoreGreen2:
                  shr     AX,3    //*
                  and     AX,$3E0 //*
                  or      CX,AX
                @@Red2:
                  xor     EAX,EAX
                  mov     AH,BH
                  shr     AX,10 //*
                  mul     LightR1
                  test    EAX,$FFFFE000
                  jz      @@StoreRed2
                  or      CH,$F8
                  jmp     @@Continue
                @@StoreRed2:
                  shl     AH,2 //*
                  or      CH,AH
                @@Continue:
                  ror     ECX,16
                  mov     [ESI],ECX
                @@Next2:
                  add     ESI,4
                  sub     EDI,2
                  jnz     @@InnerLoop

                  pop     ECX
                  dec     ECX
                  jnz     @@OuterLoop

                  pop     EDI
                  pop     ESI
                  pop     EBX
                  end
                else
                  asm
                  push    EBX
                  push    ESI
                  push    EDI

                  mov     ECX,j
                @@OuterLoop:
                  push    ECX
                  dec     ECX
                  mov     EAX,Pitch
                  mul     ECX
                  mov     ESI,p16
                  add     ESI,EAX

                  mov     EDI,i
                @@InnerLoop:
                  mov     EBX,[ESI]
                  cmp     EBX,DblColorMatch
                  je      @@Next2
                  mov     ECX,EBX
                  cmp     BX,ColorMatch
                  je      @@Next1
                  mov     AL,BL
                  and     EAX,$1F
                  mul     LightB1
                  test    EAX,$FFFFE000
                  jz      @@StoreBlue1
                  mov     CX,$1F
                  jmp     @@Green1
                @@StoreBlue1:
                  shr     AX,8
                  mov     CX,AX
                @@Green1:
                  mov     AX,BX
                  shr     AX,5
                  and     EAX,$3F  //*
                  mul     LightG1
                  test    EAX,$FFFFC000 //*
                  jz      @@StoreGreen1
                  or      CX,$7E0 //*
                  jmp     @@Red1
                @@StoreGreen1:
                  shr     AX,3    //*
                  and     AX,$7E0 //*
                  or      CX,AX
                @@Red1:
                  xor     EAX,EAX
                  mov     AH,BH
                  shr     AX,11 //*
                  mul     LightR1
                  test    EAX,$FFFFE000
                  jz      @@StoreRed1
                  or      CH,$F8
                  jmp     @@Next1
                @@StoreRed1:
                  shl     AH,3 //*
                  or      CH,AH

                @@Next1:
                  rol     ECX,16
                  rol     EBX,16
                  cmp     BX,ColorMatch
                  je      @@Continue
                  mov     AL,BL
                  and     EAX,$1F
                  mul     LightB1
                  test    EAX,$FFFFE000
                  jz      @@StoreBlue2
                  mov     CX,$1F
                  jmp     @@Green2
                @@StoreBlue2:
                  shr     AX,8
                  mov     CX,AX
                @@Green2:
                  mov     AX,BX
                  shr     AX,5
                  and     EAX,$3F  //*
                  mul     LightG1
                  test    EAX,$FFFFC000 //*
                  jz      @@StoreGreen2
                  or      CX,$7E0 //*
                  jmp     @@Red2
                @@StoreGreen2:
                  shr     AX,3    //*
                  and     AX,$7E0 //*
                  or      CX,AX
                @@Red2:
                  xor     EAX,EAX
                  mov     AH,BH
                  shr     AX,11 //*
                  mul     LightR1
                  test    EAX,$FFFFE000
                  jz      @@StoreRed2
                  or      CH,$F8
                  jmp     @@Continue
                @@StoreRed2:
                  shl     AH,3 //*
                  or      CH,AH
                @@Continue:
                  ror     ECX,16
                  mov     [ESI],ECX
                @@Next2:
                  add     ESI,4
                  sub     EDI,2
                  jnz     @@InnerLoop

                  pop     ECX
                  dec     ECX
                  jnz     @@OuterLoop

                  pop     EDI
                  pop     ESI
                  pop     EBX
                  end;
              end;
            finally
              ZoneTile.FItemImages.UnLock( nil );
            end;
          end;
        end;
      end;
    end;
    TimeCount := GetTickCount - TimeCount;
    Log.Log( 'End Ambient: ' + inttostr( TimeCount ) );
  end;

  if not FUseAmbientOnly then
  begin
    //Update flickering light zone items such that they can be updated correctly without
    //re-rendering the entire map.
    for i := 0 to LightZones.Count - 1 do
    begin
      Test := LightZones[ i ];
      Test.State := Test.States;
      if ( Test.Flicker <> flNone ) then
      begin
        Test.MoveToVideo;
        Test.FullRefresh := Test.TilesInVideo and Test.ItemsInVideo;
      end;
    end;
    LightZones.Free;
  end;
end;

function TAniMap.LineOfSight( X1, Y1, X2, Y2 : Longint ) : Boolean;
var
  GridBase, p : ^GridInfo;
  i, i1, i2 : Longint;
  j, k : Longint;
  Dx, dy : Longint;
  X, Y : Longint;
  sX, sY, s : Longint;
  R : Double;
  Offset : Longint;
  Mask : Word;
  W, H : Integer;
  a, a2 : Double;
  b, b2 : Double;
  c2 : Double;
  d2 : Double;
  R2 : Double;
  Pass : Boolean;
  StripX, StripY : Longint;
begin
  //This routine does not check for visibility within the starting tile.
  //To do so would cause a number of special cases and would slow performance.
  //Frankly, if you're standing in the same tile visibilty should not be a problem anyway.
  GridBase := GlobalLock( FMapDAta );
  Dx := X2 - X1;
  dy := Y2 - Y1;
  W := FStripWidth div 2;
  H := FStripHeight div 2;
  StripX := ( X2 div FStripWidth ) * FStripWidth + W;
  StripY := ( Y2 div FStripHeight ) * FStripHeight + H;
  R2 := sqr( W );

  if ( Dx <> 0 ) then
  begin
    if ( X1 < X2 ) then
    begin
      i1 := X1 div FTileWidth + 1;
      i2 := X2 div FTileWidth;
      Offset := 0;
    end
    else
    begin
      i1 := X2 div FTileWidth + 1;
      i2 := X1 div FTileWidth;
      Offset := -1;
    end;
    X := i1 * FTileWidth;
    for i := i1 to i2 do
    begin
      R := ( X - X1 ) / Dx;
      Y := Y1 + Round( R * dy );
      j := Y div FTileHeight;
      if ( Y mod FTileHeight ) = 0 then
        if ( Y2 < Y1 ) then
          Dec( j );
      if j >= 0 then
      begin
        p := GridBase;
        Inc( p, j * FWidth + i + Offset );
        if ( p^.LineOfSightMask <> 0 ) then
        begin
          k := j * FTileHeight;
          Mask := p^.LineOfSightMask;
          for s := 0 to 15 do
          begin
            if ( Mask and 1 ) = 1 then
            begin
              sX := X + ( s mod 4 ) * FStripWidth + W + Offset * FTileWidth;
              if ( Offset < 0 ) then
                Pass := ( sX > X2 )
              else
                Pass := ( sX <= X2 );
              if Pass then
              begin
                sY := k + ( 3 - ( s div 4 ) ) * FStripHeight + H;
                if ( Y2 < Y1 ) then
                  Pass := sY > Y2 - H
                else
                  Pass := sY <= Y2 + H;
                if Pass then
                begin
                  if ( sX <> StripX ) or ( sY <> StripY ) then
                  begin
                    if Dx = 0 then
                    begin
                      d2 := sqr( X2 - sX );
                    end
                    else if dy = 0 then
                    begin
                      d2 := sqr( 2 * ( Y2 - sY ) );
                    end
                    else
                    begin
                      a := 2 * ( sY - ( dy * ( sX - X1 ) / Dx + Y1 ) );
                      b := sX - ( Dx * ( sY - Y1 ) / dy + X1 );
                      a2 := sqr( a );
                      b2 := sqr( b );
                      c2 := a2 + b2;
                      if c2 = 0 then
                      begin
                        GlobalUnlock( FMapData );
                        Result := False;
                        Exit;
                      end;
                      d2 := a2 * b2 / c2;
                    end;
                    if d2 <= r2 then
                    begin
                      GlobalUnlock( FMapData );
                      Result := False;
                      Exit;
                    end;
                  end;
                end;
              end;
            end;
            Mask := Mask shr 1;
          end;
        end;
      end;
      Inc( X, FTileWidth );
    end;
  end;

  if ( dy <> 0 ) then
  begin
    if ( Y1 < Y2 ) then
    begin
      i1 := Y1 div FTileHeight + 1;
      i2 := Y2 div FTileHeight;
      Offset := 0;
    end
    else
    begin
      i1 := Y2 div FTileHeight + 1;
      i2 := Y1 div FTileHeight;
      Offset := -FWidth;
    end;
    Y := i1 * FTileHeight;
    for i := i1 to i2 do
    begin
      R := ( Y - Y1 ) / dy;
      X := X1 + Round( R * Dx );
      j := X div FTileWidth;
      if ( X mod FTileWidth ) = 0 then
        if ( X2 < X1 ) then
          Dec( j );
      if ( j >= 0 ) then
      begin
        p := GridBase;
        Inc( p, i * FWidth + j + Offset );
        if ( p^.LineOfSightMask <> 0 ) then
        begin

          k := j * FTileWidth;
          Mask := p^.LineOfSightMask;
          for s := 0 to 15 do
          begin
            if ( Mask and 1 ) = 1 then
            begin
              sY := Y + ( 3 - ( s div 4 ) ) * FStripHeight + H;
              if ( Offset < 0 ) then
              begin
                Dec( sY, FTileHeight );
                Pass := ( sY > Y2 );
              end
              else
              begin
                Pass := ( sY <= Y2 );
              end;
              if Pass then
              begin
                sX := k + ( s mod 4 ) * FStripWidth + W;
                if ( X2 < X1 ) then
                  Pass := sX > X2 - W
                else
                  Pass := sX <= X2 + W;
                if Pass then
                begin
                  if ( sX <> StripX ) or ( sY <> StripY ) then
                  begin
                    if Dx = 0 then
                    begin
                      d2 := sqr( X2 - sX );
                    end
                    else if dy = 0 then
                    begin
                      d2 := sqr( 2 * ( Y2 - sY ) );
                    end
                    else
                    begin
                      a := 2 * ( sY - ( dy * ( sX - X1 ) / Dx + Y1 ) );
                      b := sX - ( Dx * ( sY - Y1 ) / dy + X1 );
                      a2 := sqr( a );
                      b2 := sqr( b );
                      c2 := a2 + b2;
                      if c2 = 0 then
                      begin
                        GlobalUnlock( FMapData );
                        Result := False;
                        Exit;
                      end;
                      d2 := a2 * b2 / c2;
                    end;
                    if d2 <= r2 then
                    begin
                      GlobalUnlock( FMapData );
                      Result := False;
                      Exit;
                    end;
                  end;
                end;
              end;
            end;
            Mask := Mask shr 1;
          end;
        end;
      end;
      Inc( Y, FTileHeight );
    end;
  end;

  GlobalUnlock( FMapData );
  Result := True;
end;

function TAniMap.LineOfCollision( X1, Y1, X2, Y2 : Longint ) : Boolean;
var
  GridBase, p : ^GridInfo;
  i, i1, i2 : Longint;
  j, k : Longint;
  Dx, dy : Longint;
  X, Y : Longint;
  sX, sY, s : Longint;
  R : Double;
  Offset : Longint;
  Mask : Word;
  W, H : Integer;
  a, a2 : Double;
  b, b2 : Double;
  c2 : Double;
  d2 : Double;
  R2 : Double;
  Pass : Boolean;
  StripX, StripY : Longint;
begin
  //This routine does not check for visibility within the starting tile.
  //To do so would cause a number of special cases and would slow performance.
  //Frankly, if you're standing in the same tile visibilty should not be a problem anyway.
  GridBase := GlobalLock( FMapDAta );
  Dx := X2 - X1;
  dy := Y2 - Y1;
  W := FStripWidth div 2;
  H := FStripHeight div 2;
  StripX := ( X2 div FStripWidth ) * FStripWidth + W;
  StripY := ( Y2 div FStripHeight ) * FStripHeight + H;
  R2 := sqr( W );

  if ( Dx <> 0 ) then
  begin
    if ( X1 < X2 ) then
    begin
      i1 := X1 div FTileWidth + 1;
      i2 := X2 div FTileWidth;
      Offset := 0;
    end
    else
    begin
      i1 := X2 div FTileWidth + 1;
      i2 := X1 div FTileWidth;
      Offset := -1;
    end;
    X := i1 * FTileWidth;
    for i := i1 to i2 do
    begin
      R := ( X - X1 ) / Dx;
      Y := Y1 + Round( R * dy );
      j := Y div FTileHeight;
      if ( Y mod FTileHeight ) = 0 then
        if ( Y2 < Y1 ) then
          Dec( j );
      if j >= 0 then
      begin
        p := GridBase;
        Inc( p, j * FWidth + i + Offset );
        if ( p^.CollisionMask <> 0 ) then
        begin
          k := j * FTileHeight;
          Mask := p^.CollisionMask;
          for s := 0 to 15 do
          begin
            if ( Mask and 1 ) = 1 then
            begin
              sX := X + ( s mod 4 ) * FStripWidth + W + Offset * FTileWidth;
              if ( Offset < 0 ) then
                Pass := ( sX > X2 )
              else
                Pass := ( sX <= X2 );
              if Pass then
              begin
                sY := k + ( 3 - ( s div 4 ) ) * FStripHeight + H;
                if ( Y2 < Y1 ) then
                  Pass := sY > Y2 - H
                else
                  Pass := sY <= Y2 + H;
                if Pass then
                begin
                  if ( sX <> StripX ) or ( sY <> StripY ) then
                  begin
                    if Dx = 0 then
                    begin
                      d2 := sqr( X2 - sX );
                    end
                    else if dy = 0 then
                    begin
                      d2 := sqr( 2 * ( Y2 - sY ) );
                    end
                    else
                    begin
                      a := 2 * ( sY - ( dy * ( sX - X1 ) / Dx + Y1 ) );
                      b := sX - ( Dx * ( sY - Y1 ) / dy + X1 );
                      a2 := sqr( a );
                      b2 := sqr( b );
                      c2 := a2 + b2;
                      if c2 = 0 then
                      begin
                        GlobalUnlock( FMapData );
                        Result := False;
                        Exit;
                      end;
                      d2 := a2 * b2 / c2;
                    end;
                    if d2 <= r2 then
                    begin
                      GlobalUnlock( FMapData );
                      Result := False;
                      Exit;
                    end;
                  end;
                end;
              end;
            end;
            Mask := Mask shr 1;
          end;
        end;
      end;
      Inc( X, FTileWidth );
    end;
  end;

  if ( dy <> 0 ) then
  begin
    if ( Y1 < Y2 ) then
    begin
      i1 := Y1 div FTileHeight + 1;
      i2 := Y2 div FTileHeight;
      Offset := 0;
    end
    else
    begin
      i1 := Y2 div FTileHeight + 1;
      i2 := Y1 div FTileHeight;
      Offset := -FWidth;
    end;
    Y := i1 * FTileHeight;
    for i := i1 to i2 do
    begin
      R := ( Y - Y1 ) / dy;
      X := X1 + Round( R * Dx );
      j := X div FTileWidth;
      if ( X mod FTileWidth ) = 0 then
        if ( X2 < X1 ) then
          Dec( j );
      if ( j >= 0 ) then
      begin
        p := GridBase;
        Inc( p, i * FWidth + j + Offset );
        if ( p^.CollisionMask <> 0 ) then
        begin

          k := j * FTileWidth;
          Mask := p^.CollisionMask;
          for s := 0 to 15 do
          begin
            if ( Mask and 1 ) = 1 then
            begin
              sY := Y + ( 3 - ( s div 4 ) ) * FStripHeight + H;
              if ( Offset < 0 ) then
              begin
                Dec( sY, FTileHeight );
                Pass := ( sY > Y2 );
              end
              else
              begin
                Pass := ( sY <= Y2 );
              end;
              if Pass then
              begin
                sX := k + ( s mod 4 ) * FStripWidth + W;
                if ( X2 < X1 ) then
                  Pass := sX > X2 - W
                else
                  Pass := sX <= X2 + W;
                if Pass then
                begin
                  if ( sX <> StripX ) or ( sY <> StripY ) then
                  begin
                    if Dx = 0 then
                    begin
                      d2 := sqr( X2 - sX );
                    end
                    else if dy = 0 then
                    begin
                      d2 := sqr( 2 * ( Y2 - sY ) );
                    end
                    else
                    begin
                      a := 2 * ( sY - ( dy * ( sX - X1 ) / Dx + Y1 ) );
                      b := sX - ( Dx * ( sY - Y1 ) / dy + X1 );
                      a2 := sqr( a );
                      b2 := sqr( b );
                      c2 := a2 + b2;
                      if c2 = 0 then
                      begin
                        GlobalUnlock( FMapData );
                        Result := False;
                        Exit;
                      end;
                      d2 := a2 * b2 / c2;
                    end;
                    if d2 <= r2 then
                    begin
                      GlobalUnlock( FMapData );
                      Result := False;
                      Exit;
                    end;
                  end;
                end;
              end;
            end;
            Mask := Mask shr 1;
          end;
        end;
      end;
      Inc( Y, FTileHeight );
    end;
  end;

  GlobalUnlock( FMapData );
  Result := True;
end;

{$IFDEF DirectX}

function TAniMap.DefineItem( Zone, Index : Word; Image : IDirectDrawSurface; const StripHeights, CollisionMasks, LineOfSightMasks, LightPoints : array of Word; Slope : Single; Visible, AutoTransparent, Vertical : Boolean ) : PItemInfo;
begin
  if ( Zone >= Zones.Count ) then
  begin
    Result := nil;
    Exit;
  end;
  Result := TZone( Zones.Items[ Zone ] ).DefineItem( Index, Image, StripHeights, CollisionMasks, LineOfSightMasks, LightPoints, FTransparentColor, Slope, Visible, AutoTransparent, Vertical );
end;
{$ENDIF}

{$IFNDEF DirectX}

function TAniMap.DefineItem( Zone, Index : Word; Image : TBitmap; const StripHeights, CollisionMasks, LineOfSightMasks, LightPoints : array of Word; Slope : Single; Visible, AutoTransparent, Vertical : Boolean ) : PItemInfo;
begin
  if ( Zone >= Zones.Count ) then
  begin
    Result := nil;
    Exit;
  end;
  Result := TZone( Zones.Items[ Zone ] ).DefineItem( Index, Image, StripHeights, CollisionMasks, LineOfSightMasks, LightPoints, FTransparentColor, Slope, Visible, AutoTransparent, Vertical );
end;
{$ENDIF}

{$IFDEF DirectX}

procedure TAniMap.DefineTile( Zone, Index : Word; Image : IDirectDrawSurface );
begin
  if ( Zone >= Zones.Count ) then
    Exit;
  TZone( Zones.Items[ Zone ] ).DefineTile( Index, Image, FTransparentColor );
end;
{$ENDIF}

{$IFNDEF DirectX}

procedure TAniMap.DefineTile( Zone, Index : Word; BITMAP : TBitmap );
begin
  if ( Zone >= Zones.Count ) then
    Exit;
  TZone( Zones.Items[ Zone ] ).DefineTile( Index, BITMAP, nil, FTransparentColor );
end;
{$ENDIF}

function TAniMap.GetTile( X, Y : Longint ) : PGridInfo;
var
  GridLoc : ^GridInfo;
  Loc : Integer;
begin
  if ( FMapData <> 0 ) then
  begin
    if ( X < FWidth ) and ( Y < FHeight ) then
    begin
      Loc := X + Y * FWidth;
      GridLoc := GlobalLock( FMapData );
      Inc( GridLoc, Loc );
      Result := pointer( GridLoc );
      GlobalUnlock( FMapData );
    end
    else
    begin
      Result := nil;
    end;
  end
  else
  begin
    Result := nil;
  end;
end;

procedure TAniMap.SetDiamondTile( X, Y : Longint; ZoneTop, TileTop, ZoneRight, TileRight, ZoneBottom, TileBottom, ZoneLeft, TileLeft : Word );
var
  GridLoc, p : ^GridInfo;
  Loc : Longint;
begin
  if ( FMapData <> 0 ) then
  begin
    if ( X < FWidth ) and ( Y < FHeight ) then
    begin
      GridLoc := GlobalLock( FMapData );
      Loc := X + Y * FWidth;
      p := GridLoc;
      Inc( p, Loc );
      if TileTop = 0 then
      begin
        p^.Zone[ 1 ] := 0;
        p^.Tile[ 1 ] := $FFFF;
      end
      else
      begin
        p^.Zone[ 1 ] := ZoneTop;
        p^.Tile[ 1 ] := TZone( Zones.Items[ ZoneTop ] ).Tile[ TileTop ].ImageIndex;
      end;
      if TileRight = 0 then
      begin
        p^.Zone[ 2 ] := 0;
        p^.Tile[ 2 ] := $FFFF;
      end
      else
      begin
        p^.Zone[ 2 ] := ZoneRight;
        p^.Tile[ 2 ] := TZone( Zones.Items[ ZoneRight ] ).Tile[ TileRight ].ImageIndex;
      end;
      if TileBottom = 0 then
      begin
        p^.Zone[ 3 ] := 0;
        p^.Tile[ 3 ] := $FFFF;
      end
      else
      begin
        p^.Zone[ 3 ] := ZoneBottom;
        p^.Tile[ 3 ] := TZone( Zones.Items[ ZoneBottom ] ).Tile[ TileBottom ].ImageIndex;
      end;
      if TileLeft = 0 then
      begin
        p^.Zone[ 4 ] := 0;
        p^.Tile[ 4 ] := $FFFF;
      end
      else
      begin
        p^.Zone[ 4 ] := ZoneLeft;
        p^.Tile[ 4 ] := TZone( Zones.Items[ ZoneLeft ] ).Tile[ TileLeft ].ImageIndex;
      end;
      p^.BitField := p^.BitField or $80; //Set high bit to denote a diamond tile
      GlobalUnlock( FMapData );
    end
  end;
end;

procedure TAniMap.SetTile( X, Y, Layer : Longint; Zone, Index : Word );
var
  GridLoc, p : ^GridInfo;
  Loc : Longint;
  i, j : Integer;
begin
  if ( FMapData <> 0 ) then
  begin
    GridLoc := GlobalLock( FMapData );
    if ( Index = 0 ) then
    begin
      if ( X < FWidth ) and ( Y < FHeight ) then
      begin
        Loc := X + Y * FWidth;
        p := GridLoc;
        Inc( p, Loc );
        p^.Zone[ Layer ] := 0;
        p^.Tile[ Layer ] := $FFFF;
        if ( Layer ) = 1 then
          p^.BitField := p^.BitField and $7F; //Turn off high bit to denote rect tile
      end;
    end
    else
    begin
      for i := 0 to TZone( Zones.Items[ Zone ] ).Tile[ Index ].Columns - 1 do
      begin
        if ( X + i < FWidth ) then
        begin
          for j := 0 to TZone( Zones.Items[ Zone ] ).Tile[ Index ].Rows - 1 do
          begin
            if ( Y + j < FHeight ) then
            begin
              Loc := ( X + i ) + ( Y + j ) * FWidth;
              p := GridLoc;
              Inc( p, Loc );
              p^.Zone[ Layer ] := Zone;
              p^.Tile[ Layer ] := TZone( Zones.Items[ Zone ] ).Tile[ Index ].ImageIndex +
                i * TZone( Zones.Items[ Zone ] ).Tile[ Index ].Rows + j;
              if ( Layer ) = 1 then
                p^.BitField := p^.BitField and $7F; //Turn off high bit to denote rect tile
            end;
          end;
        end;
      end;
    end;
    GlobalUnlock( FMapData );
  end;
end;

procedure TAniMap.SetCollisionMask( X, Y : Integer; CollisionMask : Word );
var
  GridLoc, p : ^GridInfo;
  Loc : Longint;
begin
  if ( FMapData <> 0 ) then
  begin
    if ( X < FWidth ) and ( Y < FHeight ) then
    begin
      GridLoc := GlobalLock( FMapData );
      Loc := X + Y * FWidth;
      p := GridLoc;
      Inc( p, Loc );
      p^.CollisionMask := CollisionMask;
      GlobalUnlock( FMapData );
    end;
  end;
end;

procedure TAniMap.SetLineOfSightMask( X, Y : Integer; LineOfSightMask : Word );
var
  GridLoc, p : ^GridInfo;
  Loc : Longint;
begin
  if ( FMapData <> 0 ) then
  begin
    if ( X < FWidth ) and ( Y < FHeight ) then
    begin
      GridLoc := GlobalLock( FMapData );
      Loc := X + Y * FWidth;
      p := GridLoc;
      Inc( p, Loc );
      p^.LineOfSightMask := LineOfSightMask;
      GlobalUnlock( FMapData );
    end;
  end;
end;

procedure TAniMap.SetTrigger( X, Y : Integer; TriggerID : SmallInt );
var
  GridLoc, p : ^GridInfo;
  Loc : Longint;
begin
  if ( FMapData <> 0 ) then
  begin
    if ( X < FWidth ) and ( Y < FHeight ) then
    begin
      GridLoc := GlobalLock( FMapData );
      Loc := X + Y * FWidth;
      p := GridLoc;
      Inc( p, Loc );
      p^.TriggerID := TriggerID;
      p^.TriggerMask := $FFFF;
      GlobalUnlock( FMapData );
    end;
  end;
end;

procedure TAniMap.SetTrigger( X, Y : Integer; TriggerID : SmallInt; TriggerMask : word );
var
  GridLoc, p : ^GridInfo;
  Loc : Longint;
begin
  if ( FMapData <> 0 ) then
  begin
    if ( X < FWidth ) and ( Y < FHeight ) then
    begin
      GridLoc := GlobalLock( FMapData );
      Loc := X + Y * FWidth;
      p := GridLoc;
      Inc( p, Loc );
      p^.TriggerID := TriggerID;
      p^.TriggerMask := TriggerMask;
      GlobalUnlock( FMapData );
    end;
  end;
end;

procedure TAniMap.SetFilter( X, Y : Integer; FilterID : SmallInt );
var
  GridLoc, p : ^GridInfo;
  Loc : Longint;
begin
  if ( FMapData <> 0 ) then
  begin
    if ( X < FWidth ) and ( Y < FHeight ) then
    begin
      GridLoc := GlobalLock( FMapData );
      Loc := X + Y * FWidth;
      p := GridLoc;
      Inc( p, Loc );
      p^.FilterID := FilterID;
      p^.FilterMask := $FFFF;
      GlobalUnlock( FMapData );
    end;
  end;
end;

procedure TAniMap.SetFilter( X, Y : Integer; FilterID : SmallInt; FilterMask : Word );
var
  GridLoc, p : ^GridInfo;
  Loc : Longint;
begin
  if ( FMapData <> 0 ) then
  begin
    if ( X < FWidth ) and ( Y < FHeight ) then
    begin
      GridLoc := GlobalLock( FMapData );
      Loc := X + Y * FWidth;
      p := GridLoc;
      Inc( p, Loc );
      p^.FilterID := FilterID;
      p^.FilterMask := FilterMask;
      GlobalUnlock( FMapData );
    end;
  end;
end;

function TAniMap.AddItem( Zone, ItemIndex : Word; X, Y, Z : Longint; FilterID : Smallint;
  Collidable : Boolean ) : PItemInstanceInfo;
var
  Strip, VStrip : Integer;
  i, j, k : integer;
  SrcX, SrcY, SrcW : Longint;
  StripData : ^Word;
  GridBase, GridLoc : ^GridInfo;
  Strips, Rows : Integer;
  SrcMaskBase, SrcMask : ^Word;
  BitMask : Word;
  ZoneItem : TZone;
  First : Integer;
  RefItem, RefDelta, Delta : integer;
  InitDelta : boolean;
begin

  if ( X < 0 ) and ( ( X mod FStripWidth ) <> 0 ) then
    X := ( ( X div FStripWidth ) - 1 ) * FStripWidth
  else
    X := ( X div FStripWidth ) * FStripWidth;
  if ( Y < 0 ) and ( ( Y mod FStripHeight ) <> 0 ) then
    Y := ( ( Y div FStripHeight ) - 1 ) * FStripHeight
  else
    Y := ( Y div FStripHeight ) * FStripHeight;
  GridBase := GlobalLock( FMapData );

  ZoneItem := Zones.Items[ Zone ];
  with ZoneItem as TZone do
  begin

    //Apply Collision Mask
    if Collidable then
    begin
      if ( Item[ ItemIndex ].CollisionMasks <> 0 ) then
      begin
        SrcMaskBase := GlobalLock( Item[ ItemIndex ].CollisionMasks );
        if Assigned( SrcMaskBase ) then
        begin
          Strips := Item[ ItemIndex ].Strips shl 2;
          Rows := Item[ ItemIndex ].Height div FStripHeight;
          if ( ( Item[ ItemIndex ].Height mod FStripHeight ) <> 0 ) then
            Inc( Rows );
          for j := 0 to Rows - 1 do
          begin
            k := Y - ( j + 1 ) * FStripHeight;
            if k >= 0 then
            begin
              Y1 := k div FTileHeight;
              if ( Y1 < FHeight ) then
              begin
                Y2 := ( Y div FStripHeight ) - j - 1;
                for i := 0 to Strips - 1 do
                begin
                  SrcMask := SrcMaskBase;
                  Inc( SrcMask, ( j shr 2 ) * Item[ ItemIndex ].Strips + ( i shr 2 ) );
                  BitMask := 1 shl ( ( i mod 4 ) + ( ( j mod 4 ) ) * 4 );
                  if ( ( SrcMask^ and BitMask ) = BitMask ) then
                  begin
                    k := X + i * FStripWidth;
                    if k >= 0 then
                    begin
                      X1 := k div FTileWidth;
                      if ( X1 < FWidth ) then
                      begin
                        X2 := ( X div FStripWidth ) + i;
                        GridLoc := GridBase;
                        Inc( GridLoc, X1 + Y1 * FWidth );
                        BitMask := 1 shl ( ( X2 mod 4 ) + ( 3 - ( Y2 mod 4 ) ) * 4 );
                        GridLoc^.CollisionMask := GridLoc^.CollisionMask or BitMask;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
        GlobalUnlock( Item[ ItemIndex ].CollisionMasks );
      end;
    end;

    //Apply Line of Sight Mask
    if Collidable then
    begin
      if ( Item[ ItemIndex ].LineOfSightMasks <> 0 ) then
      begin
        SrcMaskBase := GlobalLock( Item[ ItemIndex ].LineOfSightMasks );
        if Assigned( SrcMaskBase ) then
        begin
          Strips := Item[ ItemIndex ].Strips shl 2;
          Rows := Item[ ItemIndex ].Height div FStripHeight;
          if ( ( Item[ ItemIndex ].Height mod FStripHeight ) <> 0 ) then
            Inc( Rows );
          for j := 0 to Rows - 1 do
          begin
            k := Y - ( j + 1 ) * FStripHeight;
            if k >= 0 then
            begin
              Y1 := k div FTileHeight;
              if ( Y1 < FHeight ) then
              begin
                Y2 := ( Y div FStripHeight ) - j - 1;
                for i := 0 to Strips - 1 do
                begin
                  SrcMask := SrcMaskBase;
                  Inc( SrcMask, ( j shr 2 ) * Item[ ItemIndex ].Strips + ( i shr 2 ) );
                  BitMask := 1 shl ( ( i mod 4 ) + ( ( j mod 4 ) ) * 4 );
                  if ( ( SrcMask^ and BitMask ) = BitMask ) then
                  begin
                    k := X + i * FStripWidth;
                    if k >= 0 then
                    begin
                      X1 := k div FTileWidth;
                      if ( X1 < FWidth ) then
                      begin
                        X2 := ( X div FStripWidth ) + i;
                        GridLoc := GridBase;
                        Inc( GridLoc, X1 + Y1 * FWidth );
                        BitMask := 1 shl ( ( X2 mod 4 ) + ( 3 - ( Y2 mod 4 ) ) * 4 );
                        GridLoc^.LineOfSightMask := GridLoc^.LineOfSightMask or BitMask;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
        GlobalUnlock( Item[ ItemIndex ].LineOfSightMasks );
      end;
    end;

    //Divide the image into strips and add each strip to the items list as a seperate item
    if LastItem >= ItemListSize then
    begin
      Log.Log( '*** Error: number of items exceeds maximum' );
      result := nil;
      exit;
    end;
    First := LastItem + 1;
    InitDelta := false;
    Result := @FItemList[ First ];
    RefDelta := 0;
    RefItem := First;
    for Strip := 1 to Item[ ItemIndex ].Strips do
    begin
      SrcY := Item[ ItemIndex ].Top + ( Strip - 1 ) * Item[ ItemIndex ].Height;
      for VStrip := 0 to 3 do
      begin
        SrcX := Item[ ItemIndex ].Left + VStrip * FStripWidth;
        SrcW := ( Strip - 1 ) * FTileWidth + VStrip * FStripWidth;
        if ( SrcW < Item[ ItemIndex ].Width ) then
        begin
          Inc( LastItem );
          if LastItem <= ItemListSize then
          begin
            FItemList[ LastItem ].ImageX := SrcX;
            FItemList[ LastItem ].ImageY := SrcY;
            if ( SrcW + FStripWidth > Item[ ItemIndex ].Width ) then
              FItemList[ LastItem ].Width := Item[ ItemIndex ].Width - SrcW
            else
              FItemList[ LastItem ].Width := FStripWidth;
            FItemList[ LastItem ].Height := Item[ ItemIndex ].Height;
            FItemList[ LastItem ].Zone := Zone;
            FItemList[ LastItem ].FilterID := FilterID;
            FItemList[ LastItem ].XRayID := 0;
            FItemList[ LastItem ].Slope0 := Item[ ItemIndex ].Slope;
            FItemList[ LastItem ].Visible := Item[ ItemIndex ].Visible;
            FItemList[ LastItem ].AutoTransparent := Item[ ItemIndex ].AutoTransparent;
            FItemList[ LastItem ].Vertical := Item[ ItemIndex ].Vertical;
            FItemList[ LastItem ].Last := False;
            FItemList[ LastItem ].Next := 0;

            if ( Item[ ItemIndex ].StripHeights <> 0 ) then
            begin
              StripData := GlobalLock( Item[ ItemIndex ].StripHeights );
              Inc( StripData, ( ( Strip - 1 ) shl 2 ) + VStrip );
              FItemList[ LastItem ].VHeight := StripData^ + Z;
              GlobalUnlock( Item[ ItemIndex ].StripHeights );
            end;

            FItemList[ LastItem ].X := X + SrcW;
            FItemList[ LastItem ].Y := Y - FItemList[ LastItem ].Height + FItemList[ LastItem ].VHeight;

            if FItemList[ LastItem ].VHeight <> 0 then
            begin
              Delta := FItemList[ LastItem ].Y - round( SrcW * FItemList[ LastItem ].Slope0 );
              if InitDelta then
              begin
                if Delta < RefDelta then
                begin
                  RefDelta := Delta;
                  RefItem := LastItem
                end;
              end
              else
              begin
                RefDelta := Delta;
                RefItem := LastItem;
                InitDelta := true;
              end;
            end;

            if ( Item[ ItemIndex ].LightPoints <> 0 ) then
            begin
              StripData := GlobalLock( Item[ ItemIndex ].LightPoints );
              Inc( StripData, ( ( Strip - 1 ) shl 2 ) + VStrip );
              FItemList[ LastItem ].Slope1 := smallint( StripData^ ) / FStripWidth;
              FItemList[ LastItem ].Slope2 := FItemList[ LastItem ].Slope1;
              GlobalUnlock( Item[ ItemIndex ].LightPoints );
            end;


            //Keep Items sorted by VY     //Replaced by BuildRowUpdateInfo
           { if (FirstItem = 0) then begin
              FirstItem := LastItem;
              FItemList[LastItem].Next := 0;
            end
            else begin
              i := FirstItem;
              j := 0;
              while (i <> 0) do begin
                if (FItemList[i].Y >= FItemList[LastItem].Y) then begin
                  if (j = 0) then
                    FirstItem := LastItem
                  else
                    FItemList[j].Next := LastItem;
                  FItemList[LastItem].Next := i;
                  Break;
                end;
                j := i;
                i := FItemList[i].Next;
              end;
              if (i = 0) then begin
                FItemList[j].Next := LastItem;
                FItemList[LastItem].Next := 0;
              end;
            end;  }
          end;
        end;
      end;
    end;

    for i := First to LastItem do
    begin
      FItemList[ i ].RefItem := RefItem;
    end;

    FItemList[ LastItem ].Last := True;
  end;
  GlobalUnlock( FMapData );
end;

procedure TAniMap.SetTileSize( Size : Word );
begin
  FTileSize := Size;
  FTileWidth := FTileSize * 16;
  FTileHeight := FTileSize * 8;
  FStripWidth := FTileWidth shr 2;
  FStripHeight := FTileHeight shr 2;
  FBitWidth := FWidth * FTileWidth;
  FBitHeight := FHeight * FTileHeight;
end;

//------------------------------------------------------------------------------
//AniView Component

constructor TAniView.Create( AOwner : TComponent );
var
  ScreenDC : HDC;
{$IFDEF SHAREWARE}
  Msg : string;
{$ENDIF}
begin
  inherited Create( AOwner );

  FInterval := 50;
  ShowRepaint := false;

  if not ( csDesigning in ComponentState ) then
  begin
    //Built-in Timer;
{    Timer := TAniTimer.Create(AOwner);
    Timer.Interval := FInterval;
    Timer.OnTimer := nil;               //Must set active property
    Timer.resolution := 1;
    Timer.TimerPriority := tpNormal;  }

{$IFNDEF DirectX}
    //FrameDC
    FrameBuffer := TBitmap.Create;

    //MapDC
    MapBuffer := TBitmap.Create;

    //Work
    Work := TBitmap.Create;
    Work.width := WorkWidth;
    Work.Height := WorkHeight;
{$ENDIF}

    //TempDC
    ScreenDC := GetDC( 0 );
    TempDC := CreateCompatibleDC( ScreenDC );
    OldTempBitmap := SelectObject( TempDC, CreateCompatibleBitmap( TempDC, 1, 1 ) );
    DeleteObject( SelectObject( TempDC, OldTempBitmap ) );
    ReleaseDC( 0, ScreenDC );

    //Initailize A* object
    FAStar := TAStar.Create;
    FAStar.CanMove := CanMove;

    FFigureList := TList.Create;
  end;
end;

destructor TAniView.Destroy;
begin
  if not ( csDesigning in ComponentState ) then
  begin
    FigureList.Free;
    if Assigned( Timer ) then
      Timer.Destroy;
    if ( MapColumns <> 0 ) then
      GlobalFree( MapColumns );
    MapColumns := 0;
    if ( MapRows <> 0 ) then
      GlobalFree( MapRows );
    MapRows := 0;
    if ( TempDC <> 0 ) then
      DeleteDC( TempDC );
    TempDC := 0;
    FAStar.Free;
    FAStar := nil;
  end;
  inherited Destroy;
end;

procedure TAniView.InitDX( Handle : HWND; ResW, ResH, BPP : Integer );
{$IFDEF DirectX}
var
  ddsd : DDSurfaceDesc;
  Caps : DDSCaps;
  BltFx : DDBLTFX;
  C : longint;
{$ENDIF}
begin
{$IFDEF DirectX}
//Log.Log('InitDX');
  if DXMode then
    Exit;
//Log.Log('2');
  ResWidth := ResW;
//Log.Log('3');
  ResHeight := ResH;
//Log.Log('4');
  DirectDrawCreate( nil, lpDD, nil );
//Log.Log('5');
  lpDD.SetCooperativeLevel( Handle, DDSCL_EXCLUSIVE or DDSCL_FULLSCREEN );
//Log.Log('6');
  lpDD.SetDisplayMode( ResW, ResH, BPP );
//Log.Log('7');

  ddsd.dwSize := SizeOf( ddsd );
//Log.Log('8');
  ddsd.dwFlags := DDSD_CAPS or DDSD_BACKBUFFERCOUNT;
//Log.Log('9');
  ddsd.dwBackBufferCount := 1;
//Log.Log('10');
  ddsd.ddsCaps.dwCaps := DDSCAPS_COMPLEX + DDSCAPS_FLIP + DDSCAPS_PRIMARYSURFACE;
//Log.Log('11');
  if ( lpdd.CreateSurface( ddsd, lpDDSFront, nil ) = DD_OK ) then
  begin
//Log.Log('12');
    Caps.dwCaps := DDSCAPS_BACKBUFFER;
//Log.Log('13');
    lpDDSFront.GetAttachedSurface( Caps, lpDDSBack );
//Log.Log('14');
  end;
  BltFx.dwSize := SizeOf( BltFx );
//Log.Log('15');
  BltFx.dwFillColor := 0;
//Log.Log('16');
  lpDDSBack.Blt( Rect( 0, 0, ResWidth, ResHeight ), nil, Rect( 0, 0, ResWidth, ResHeight ), DDBLT_COLORFILL + DDBLT_WAIT, BltFx );
//Log.Log('17');
  lpDDSFront.Flip( nil, DDFLIP_WAIT );
//Log.Log('18');
  BltFx.dwSize := SizeOf( BltFx );
//Log.Log('19');
  BltFx.dwFillColor := 0;
//Log.Log('20');
  lpDDSBack.Blt( Rect( 0, 0, ResWidth, ResHeight ), nil, Rect( 0, 0, ResWidth, ResHeight ), DDBLT_COLORFILL + DDBLT_WAIT, BltFx );
//Log.Log('21');
  C := FindColorMatch( $FFFFFF );
//Log.Log('22');
  if C = $FFFFFF then
    PixelFormat := pf888
  else if C > $7FFF then
    PixelFormat := pf565
  else
    PixelFormat := pf555;
//Log.Log('23');
  DXMode := True;
//Log.Log('24');
{$ENDIF}
end;

procedure TAniView.CloseDX;
begin
{$IFDEF DirectX}
  if not DXMode then
    Exit;
  DXMode := False;
  lpdd.RestoreDisplayMode;
  lpDDSBack := nil;
  lpDDSFront := nil;
  lpDD := nil;
{$ENDIF}
end;

procedure TAniView.FreeResources;
var
  i : Integer;
begin
  if MapColumns <> 0 then
    GlobalFree( MapColumns );
  MapColumns := 0;
  if MapRows <> 0 then
    GlobalFree( MapRows );
  MapRows := 0;
  FMap := nil;
  KeyFigure := nil;
{$IFDEF DirectX}
  if not MapPreCreated then
  begin
    lpDDSMap := nil;
    Work := nil;
    XRayImage := nil;
  end;
{$ENDIF}
{$IFNDEF DirectX}
  FrameBuffer.Free;
  FrameBuffer := nil;
  MapBuffer.Free;
  MapBuffer := nil;
  Work.Free;
  Work := nil;
  XRayImage.Free;
  XRayImage := nil;
{$ENDIF}
  for i := 0 to FigureList.Count - 1 do
  begin
    try
      TAniFigure( FigureList.Items[ i ] ).Free;
    except
      Log.Log( '*** Error: object could not be freed' );
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
{$IFDEF DirectX}
  if csDesigning in ComponentState then
  begin
    PatBlt( Canvas.Handle, 0, 0, Width, Height, BLACKNESS );
  end;
{$ENDIF}
{$IFNDEF DirectX}
  if Assigned( FrameBuffer ) then
  begin
    if ( FrameBuffer.Width <> Width ) or ( FrameBuffer.Height <> Height ) then
    begin
      FrameBuffer.width := Width;
      FrameBuffer.Height := Height;
      PatBlt( Framebuffer.Canvas.Handle, 0, 0, Width, Height, BLACKNESS );
      InitMap;
    end;
    BitBlt( Canvas.Handle, 0, 0, Width, Height, FrameBuffer.Canvas.Handle, 0, 0, SRCCOPY );
  end
  else
  begin
    PatBlt( Canvas.Handle, 0, 0, Width, Height, BLACKNESS );
  end;
{$ENDIF}
end;

procedure TAniView.Show( Value : Boolean );
begin
  Invalidate;
end;

procedure TAniView.SetMap( const VMap : TAniMap );
begin
  FMap := VMap;
  InitMap;
end;

procedure TAniView.AddFigure( Figure : TAniFigure );
begin
  if ( Figure = nil ) then
    Exit;
  FigureList.Add( Figure );
  Figure.FView := Self;
  if ( Figure.Radius > FMaxCollisionRadius ) then
    FMaxCollisionRadius := Figure.Radius;
  if ( Figure.Height - Figure.CenterY > MaxHeightTop ) then
    MaxHeightTop := Figure.Height - Figure.CenterY;
  if ( Figure.CenterY > MaxHeightBottom ) then
    MaxHeightBottom := Figure.CenterY;
end;

procedure TAniView.ReplaceFigure( i : integer; Figure : TAniFigure );
begin
  if ( Figure = nil ) then
    Exit;
  if assigned( FigureList.items[ i ] ) then
  begin
    if TAniFigure( FigureList.items[ i ] ).Enabled then
      DisableFigure( TAniFigure( FigureList.items[ i ] ) );
    TAniFigure( FigureList.items[ i ] ).Free;
  end;
  FigureList.items[ i ] := Figure;
  Figure.FView := Self;
  if ( Figure.Radius > FMaxCollisionRadius ) then
    FMaxCollisionRadius := Figure.Radius;
  if ( Figure.Height - Figure.CenterY > MaxHeightTop ) then
    MaxHeightTop := Figure.Height - Figure.CenterY;
  if ( Figure.CenterY > MaxHeightBottom ) then
    MaxHeightBottom := Figure.CenterY;
end;


procedure TAniView.FDrawFrame( Sender : TObject );
begin
  if not FActive then
    Exit;
  Timer.OnTimer := nil;
  DrawFrame;
  Timer.OnTimer := FDrawFrame;
end;

procedure TAniView.WaitForNextFrame;
var
  NextTickCount : longword;
begin
  NextTickCount := LastTickCount + Interval;
  while GetTickCount < NextTickCount do
  begin
    if assigned( OnWaitFortimer ) then
      OnWaitFortimer( self )
    else
      application.processmessages;
  end;
end;

procedure TAniView.DrawFrame;
var
  i, j : Longint;
  RefItem : PItemInstanceInfo;
  RowBase, RowData : ^RowUpdateInfo;
  ColumnBase, ColumnData : ^MapColumnHeaderInfo;
  NextFigure, LastFigure : TAniFigure;
  ItemIndex : Word;
  MaxRow, Y1, Y2 : Longint;
  StripX, StripW : Integer;
  X, Y, T, W, H : Longint;
  ZoneItem : TZone;
  KeyX, KeyY : Longint;
  XRayActive : Boolean;
  XRayX1, XRayY1, XRayX2, XRayY2 : Longint;
  XRayX1Fix, XRayX2Fix : Longint;
  InXrayZone : Boolean;
  ApplyXRay : Boolean;
  HidesCharacter : Boolean;
  GridLoc : ^GridInfo;
  Loc : Integer;
{$IFNDEF DirectX}
  H1 : Longint;
{$ENDIF}
{$IFDEF DirectX}
  DC : HDC;
  SrcX1, SrcY1, SrcX2, SrcY2 : Integer;
  DstX1, DstY1, DstX2, DstY2 : Integer;
  BltFx : DDBLTFX;
  ddck : DDCOLORKEY;
{$ENDIF}
begin
  if FDrawing then
    exit;
  LastTickCount := GetTickCount;
  FDrawing := True;

  //Calculate postion of key figure
  if Assigned( KeyFigure ) then
  begin
    if KeyFigure.NeedPath then
      GetPath( KeyFigure );
    if KeyFigure.Moved then
      TransFigure( KeyFigure );
    if KeyFigure.Moving then
      MoveFigure( KeyFigure );
    CenterX := KeyFigure.FX;
    CenterY := KeyFigure.FY;
  end;
  OffsetX := CenterX - Width div 2;
  OffsetY := CenterY - Height div 2;
  W := FMap.FBitWidth - Width - FMap.FTileWidth;
  H := FMap.FBitHeight - Height - FMap.FTileHeight;
  if ( OffsetX < FMap.FTileWidth ) then
    OffsetX := FMap.FTileWidth;
  if ( OffsetY < FMap.FTileHeight ) then
    OffsetY := FMap.FTileHeight;
  if ( OffsetX > W ) then
    OffsetX := W;
  if ( OffsetY > H ) then
    OffsetY := H;
  FRightX := OffsetX + Width;
  FBottomY := OffsetY + Height;

  XRayActive := False;
  XRayX1 := 0;
  XRayY1 := 0;
  XRayY2 := 0;
  XRayX1Fix := 0;
  XRayX2Fix := 0;

  if Assigned( FMap ) then
  begin
    RowBase := GlobalLock( MapRows );

    //Move map accordingly
    if ForceRefresh then
    begin
      RefreshMap;
      ForceRefresh := False;
    end
    else
      UpdateMap;

    //Copy map to frame buffer
{$IFDEF DirectX}
    lpDDSBack.BltFast( Left, Top, lpDDSMap, Rect( MapOffsetX, MapOffsetY, MapOffsetX + Width, MapOffsetY + Height ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );

    //Add flicker lighting
    for i := 1 to FMap.Zones.Count - 1 do
    begin
      ZoneItem := FMap.Zones.Items[ i ];
      if ZoneItem is TLightZone then
      begin
        if ZoneItem.FullRefresh then
        begin
          RefreshLight( TLightZone( ZoneItem ) );
        end;
      end;
    end;
{$ENDIF}

{$IFNDEF DirectX}
    BitBlt( FrameBuffer.Canvas.Handle, 0, 0, width, Height, MapBuffer.Canvas.Handle, MapOffsetX, MapOffsetY, SRCCOPY );
{$ENDIF}

    //Get mouse position
    GetCursorPos( MousePosition );
    MousePosition := ScreenToClient( MousePosition );
    MouseOverFigure := nil;
    MouseOverHLFigure := nil;
    MouseOverTile := nil;

    if ( MousePosition.X >= Left ) and ( MousePosition.X < Left + Width ) and ( MousePosition.Y >= Top ) and ( MousePosition.Y < Top + Height ) then
    begin
      X := ( MousePosition.X + OffsetX ) div FMap.FTileWidth;
      Y := ( MousePosition.Y + OffsetY ) div FMap.FTileHeight;
      if ( X >= 0 ) and ( Y >= 0 ) and ( X < FMap.FWidth ) and ( Y < Fmap.FHeight ) then
      begin
        Loc := X + Y * FMap.FWidth;
        GridLoc := GlobalLock( FMap.FMapData );
        Inc( GridLoc, Loc );
        MouseOverTile := pointer( GridLoc );
        GlobalUnlock( Fmap.FMapData );
      end;
    end;

    //Calculate all figures and place on map update grid
{    for i := 0 to FigureList.Count - 1 do begin
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
    end;     }

    if Assigned( KeyFigure ) and KeyFigure.AutoTransparent and Assigned( XRayImage ) then
    begin
      XRayX1 := KeyFigure.FX - XRayWidth div 2;
      XRayY1 := KeyFigure.FY - KeyFigure.CenterY;
      XRayX2 := XRayX1 + XRayWidth;
      XRayY2 := XRayY1 + XRayHeight;
      XRayX1Fix := FMap.FStripWidth * ( XRayX1 div FMap.FStripWidth );
      XRayX2Fix := FMap.FStripWidth * ( XRayX2 div FMap.FStripWidth + 1 );
      XRayActive := True;
      RefreshRegion( XRayX1, XRayY1, XRayWidth, XRayHeight );
    end;

{$IFDEF DirectX}
    BltFx.dwSize := SizeOf( BltFx );
    BltFx.dwFillColor := FMap.FColorMatch;
    Work.Blt( Rect( WorkWidth div 2, 0, WorkWidth div 2 + XRayWidth + FMap.FStripWidth, XRayHeight ), nil, Rect( 320, 0, 320 + XRayWidth + FMap.FStripWidth, XRayHeight ), DDBLT_COLORFILL + DDBLT_WAIT, BltFx );
{$ENDIF}
{$IFNDEF DirectX}
    PatBlt( Work.Canvas.Handle, WorkWidth div 2, 0, XRayWidth + FMap.FStripWidth, XRayHeight, BLACKNESS );
{$ENDIF}
    //Walk through grid and draw on frame buffer when necessary
    MaxRow := PixelHeight;
    Y := OffsetY + Height + MaxHeightBottom;
    if ( Y < MaxRow ) then
      MaxRow := Y;

    if ( OffsetY > MaxHeightTop ) then
      Y1 := OffsetY - MaxHeightTop
    else
      Y1 := 0;
    if Y1 < 0 then
      Y1 := 0;
    RowData := RowBase;
    Inc( RowData, OffsetY );

    Y := RowData^.DescendRow;
    if Y < Y1 then
      Y1 := Y;

    RowData := RowBase;
    Inc( RowData, Y1 );
    ItemIndex := RowData^.ItemIndex;

    Y := OffsetY + Height;
    if Y > PixelHeight then
      Y := PixelHeight;
    RowData := RowBase;
    Inc( RowData, Y );
    Y2 := RowData^.OverlapRow;
    if MaxRow > Y2 then
      Y2 := MaxRow;

    ColumnBase := GlobalLock( MapColumns );
    ColumnData := ColumnBase;
    ZeroMemory( ColumnData, FMapColumnCount * sizeof( MapColumnHeaderInfo ) );

    if ( Y2 >= Y1 ) then
    begin
      RowData := RowBase;
      Inc( RowData, Y1 );
      for i := Y1 to Y2 do
      begin
        RowData^.Figure := nil;
        inc( RowData );
      end;

      for i := 0 to FigureList.Count - 1 do
      begin
        if ( TAniFigure( FigureList.Items[ i ] ).Enabled ) then
        begin
          TAniFigure( FigureList.Items[ i ] ).FOnScreen := false;
          TAniFigure( FigureList.Items[ i ] ).UpdateScript;
          if TAniFigure( FigureList.Items[ i ] ).ViewEnabled then
            TAniFigure( FigureList.Items[ i ] ).Moved := true;

          if ( FigureList.Items[ i ] <> KeyFigure ) then
            if TAniFigure( FigureList.Items[ i ] ).NeedPath then
              GetPath( TAniFigure( FigureList.Items[ i ] ) );
          if ( FigureList.Items[ i ] <> KeyFigure ) then
            if TAniFigure( FigureList.Items[ i ] ).Moved then
              TransFigure( TAniFigure( FigureList.Items[ i ] ) );
          if ( FigureList.Items[ i ] <> KeyFigure ) then
            if TAniFigure( FigureList.Items[ i ] ).Moving then
              MoveFigure( TAniFigure( FigureList.Items[ i ] ) );
          if assigned( TAniFigure( FigureList.Items[ i ] ).OnMove ) then
            TAniFigure( FigureList.Items[ i ] ).OnMove( FigureList.Items[ i ] );

          X := TAniFigure( FigureList.Items[ i ] ).FX - TAniFigure( FigureList.Items[ i ] ).CenterX - OffsetX;
          W := X + TAniFigure( FigureList.Items[ i ] ).MouseRect.Right;
          inc( X, TAniFigure( FigureList.Items[ i ] ).MouseRect.Left );
          Y := TAniFigure( FigureList.Items[ i ] ).FY - TAniFigure( FigureList.Items[ i ] ).CenterY - TAniFigure( FigureList.Items[ i ] ).FZ - OffsetY;
          H := Y + TAniFigure( FigureList.Items[ i ] ).MouseRect.Bottom;
          inc( Y, TAniFigure( FigureList.Items[ i ] ).MouseRect.Top );
          if ( MousePosition.X >= X ) and ( MousePosition.X < W ) and ( MousePosition.Y >= Y ) and ( MousePosition.Y < H ) then
          begin
            if not Assigned( MouseOverFigure ) then
              MouseOverFigure := TAniFigure( FigureList.Items[ i ] )
            else if ( TAniFigure( FigureList.Items[ i ] ).FY > MouseOverFigure.FY ) then
              MouseOverFigure := TAniFigure( FigureList.Items[ i ] );
            if ( TAniFigure( FigureList.Items[ i ] ).Highlightable ) then
            begin
              if not Assigned( MouseOverHLFigure ) then
                MouseOverHLFigure := TAniFigure( FigureList.Items[ i ] )
              else if ( TAniFigure( FigureList.Items[ i ] ).FY > MouseOverHLFigure.FY ) then
                MouseOverHLFigure := TAniFigure( FigureList.Items[ i ] );
            end;
          end;

          if ( TAniFigure( FigureList.Items[ i ] ).FY >= Y1 ) and ( TAniFigure( FigureList.Items[ i ] ).FY <= Y2 ) then
          begin
            RowData := RowBase;
            Inc( RowData, TAniFigure( FigureList.Items[ i ] ).FY );
            NextFigure := RowData^.Figure;
            RowData^.Figure := FigureList.Items[ i ];
            TAniFigure( FigureList.Items[ i ] ).NextOnRow := NextFigure;
          end
          else
          begin
            TAniFigure( FigureList.Items[ i ] ).NextOnRow := nil;
          end;
        end
        else
        begin
          if ( TAniFigure( FigureList.Items[ i ] ).ViewEnabled ) then
            DisableFigure( TAniFigure( FigureList.Items[ i ] ) );
        end;
      end;

      RowData := RowBase;
      Inc( RowData, Y1 );
      for i := Y1 to Y2 do
      begin
        if ( ItemIndex > 0 ) then
        begin
          while ( FMap.FItemList[ ItemIndex ].Y <= i ) or ( i = PixelHeight ) do
          begin
            if FMap.FItemList[ ItemIndex ].Visible then
            begin
              if ( FMap.FItemList[ ItemIndex ].FilterID = 0 ) or ( ( FMap.FItemList[ ItemIndex ].FilterID < 0 ) or
                ( FMap.FItemList[ ItemIndex ].FilterID = FItemMask ) ) and
                ( FMap.FItemList[ ItemIndex ].FilterID <> -FItemMask ) then
              begin
                StripX := ( FMap.FItemList[ ItemIndex ].X - MapX * FMap.FTileWidth ) div FMap.FStripWidth;
                ColumnData := ColumnBase;
                Y := FMap.FItemList[ ItemIndex ].Y - FMap.FItemList[ ItemIndex ].VHeight;
                j := Y + FMap.FItemList[ ItemIndex ].VHeight;
                if ( StripX >= 0 ) and ( StripX < FMapColumnCount ) then
                begin
                  InXRayZone := XRayActive and ( FMap.FItemList[ ItemIndex ].X >= XRayX1Fix ) and
                    ( FMap.FItemList[ ItemIndex ].X < XRayX2Fix ) and ( j > XRayY1 ) and ( Y <= XRayY2 );
                  Inc( ColumnData, StripX );
                  HidesCharacter := ColumnData^.Active and ( Y <= ColumnData^.BaseLine );
                  if InXRayZone or HidesCharacter then
                  begin
                    X := FMap.FItemList[ ItemIndex ].X - OffsetX;
                    ApplyXRay := InXRayZone and FMap.FItemList[ ItemIndex ].AutoTransparent and
                      ( ( FMap.FItemList[ ItemIndex ].XRayID = 0 ) or ( FMap.FItemList[ ItemIndex ].XRayID = FItemMask ) );
                    if ApplyXRay then
                    begin
                      if FMap.FItemList[ ItemIndex ].VHeight = 0 then
                        ApplyXRay := false
                      else
                      begin
                        RefItem := @FMap.FItemList[ FMap.FItemList[ ItemIndex ].RefItem ];
                        j := Round( RefItem.Slope0 * ( KeyFigure.FX - RefItem.X ) ) + RefItem.Y;
                        ApplyXRay := ( j > KeyFigure.FY );
                      end;
                    end;
                    Dec( Y, OffsetY );
                    ZoneItem := TZone( FMap.Zones.Items[ FMap.FItemList[ ItemIndex ].Zone ] );
{$IFDEF DirectX}
                    SrcX1 := FMap.FItemList[ ItemIndex ].ImageX;
                    DstX1 := X;
                    SrcX2 := FMap.FItemList[ ItemIndex ].ImageX + FMap.FItemList[ ItemIndex ].Width;
                    DstX2 := X + FMap.FItemList[ ItemIndex ].Width;
                    Clip( 0, width, DstX1, DstX2, SrcX1, SrcX2 );

                    SrcY1 := FMap.FItemList[ ItemIndex ].ImageY;
                    DstY1 := Y;
                    SrcY2 := FMap.FItemList[ ItemIndex ].ImageY + FMap.FItemList[ ItemIndex ].Height;
                    DstY2 := Y + FMap.FItemList[ ItemIndex ].Height;
                    Clip( 0, Height, DstY1, DstY2, SrcY1, SrcY2 );

                    if ZoneItem is TLightZone then
                    begin
                      if TLightZone( ZoneItem ).Flicker <> flNone then
                      begin
                        H := ( TLightZone( ZoneItem ).States - TLightZone( ZoneItem ).State ) * TLightZone( ZoneItem ).ItemStateOffset;
                        dec( SrcX1, H );
                        dec( SrcX2, H );
                      end;
                    end;
                    if ApplyXRay then
                    begin
                      if ColumnData^.Active then
                      begin
                        if ( DstY2 + OffsetY > XRayY2 ) and ( ColumnData^.BaseLine > XRayY2 ) then
                        begin
                          Clip( ColumnData^.Top - OffsetY, ColumnData^.BaseLine - OffsetY, DstY1, DstY2, SrcY1, SrcY2 );
                          lpDDSBack.BltFast( left + DstX1, top + XRayY2 - OffsetY, ZoneItem.FItemImages,
                            Rect( SrcX1, SrcY2 - ( DstY2 + OffsetY - XRayY2 ), SrcX2, SrcY2 ),
                            DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
                        end;
                        Clip( ColumnData^.Top - OffsetY, XRayY2, DstY1, DstY2, SrcY1, SrcY2 ); //????
                        H := XRayY1 - ( DstY1 + OffsetY );
                        if ( H > 0 ) then
                        begin
                          Y := SrcY2 - SrcY1;
                          if H > Y then
                            H := Y;
                          lpDDSBack.BltFast( left + DstX1, top + DstY1, ZoneItem.FItemImages,
                            Rect( SrcX1, SrcY1, SrcX2, SrcY1 + H ),
                            DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
                        end;
                      end;
                      Clip( XRayY1 - OffsetY, XRayY2 - OffsetY, DstY1, DstY2, SrcY1, SrcY2 );
                      //Blt to work at 0,0 with no color key.
                      //Then blt the appropriate segment of Xray on to work with black color key.
                      //Then blt the result onto the back buffer using SRCCOLORKEY
                      Work.BltFast( 0, 0, ZoneItem.FItemImages,
                        Rect( SrcX1, SrcY1, SrcX2, SrcY2 ),
                        DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );

                      X := DstX1 + OffsetX - XRayX1;
                      Y := DstY1 + OffsetY - XRayY1;
                      W := X + SrcX2 - SrcX1;
                      H := Y + SrcY2 - SrcY1;
                      j := 0;

                      if ( W > XRayWidth ) then
                        W := XRayWidth;
                      if ( X < 0 ) then
                      begin
                        j := -X;
                        X := 0;
                      end;

                      Work.BltFast( j, 0, XRayImage,
                        Rect( X, Y, W, H ),
                        DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

                      ddck.dwColorSpaceLowValue := FMap.FColorMatch;
                      ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
                      Work.SetColorKey( DDCKEY_SRCBLT, ddck );
                      lpDDSBack.BltFast( Left + DstX1, Top + DstY1, Work, Rect( 0, 0, SrcX2 - SrcX1, SrcY2 - SrcY1 ),
                        DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
                    end
                    else
                    begin
                      if ColumnData^.Active then
                      begin
                        if InXRayZone then
                        begin
                          if XRayY1 < ColumnData^.Top then
                            Y := XRayY1
                          else
                            Y := ColumnData^.Top;
                          if XRayY2 > ColumnData^.BaseLine then
                            H := XRayY2
                          else
                            H := ColumnData^.BaseLine;
                          Clip( Y - OffsetY, H - OffsetY, DstY1, DstY2, SrcY1, SrcY2 );
                        end
                        else
                          Clip( ColumnData^.Top - OffsetY, ColumnData^.BaseLine - OffsetY, DstY1, DstY2, SrcY1, SrcY2 );
                      end
                      else
                        Clip( XRayY1 - OffsetY, XRayY2 - OffsetY, DstY1, DstY2, SrcY1, SrcY2 );
                      lpDDSBack.BltFast( Left + DstX1, Top + DstY1, ZoneItem.FItemImages, Rect( SrcX1, SrcY1, SrcX2, SrcY2 ),
                        RepaintCode ); //****
                    end;
{$ENDIF}
{$IFNDEF DirectX}
                    if ApplyXRay then
                    begin
                      if ( FMap.FItemList[ ItemIndex ].Y > XRayY2 ) then
                      begin
                        j := FMap.FItemList[ ItemIndex ].ImageY + FMap.FItemList[ ItemIndex ].VHeight - FMap.FItemList[ ItemIndex ].Y + XRayY2;
                        H1 := FMap.FItemList[ ItemIndex ].Height - j + FMap.FItemList[ ItemIndex ].ImageY;
                        SelectObject( TempDC, ZoneItem.FItemMasks );
                        BitBlt( FrameBuffer.Canvas.Handle, X, XRayY2 - OffsetY, FMap.FItemList[ ItemIndex ].Width, H1, TempDC,
                          FMap.FItemList[ ItemIndex ].ImageX, j, SRCAND );
                        SelectObject( TempDC, ZoneItem.FItemImages );
                        BitBlt( FrameBuffer.Canvas.Handle, X, XRayY2 - OffsetY, FMap.FItemList[ ItemIndex ].Width, H1, TempDC,
                          FMap.FItemList[ ItemIndex ].ImageX, j, SRCPAINT );
                      end;
                      if ( Y + OffsetY < XRayY1 ) then
                      begin
                        H1 := XRayY1 - ( Y + OffsetY );
                        SelectObject( TempDC, ZoneItem.FItemMasks );
                        BitBlt( FrameBuffer.Canvas.Handle, X, Y, FMap.FItemList[ ItemIndex ].Width, H1, TempDC,
                          FMap.FItemList[ ItemIndex ].ImageX, FMap.FItemList[ ItemIndex ].ImageY, SRCAND );
                        SelectObject( TempDC, ZoneItem.FItemImages );
                        BitBlt( FrameBuffer.Canvas.Handle, X, Y, FMap.FItemList[ ItemIndex ].Width, H1, TempDC,
                          FMap.FItemList[ ItemIndex ].ImageX, FMap.FItemList[ ItemIndex ].ImageY, SRCPAINT );
                      end;
                      X := WorkWidth div 2 + X - XRayX1Fix + OffsetX;
                      Y := Y - XRayY1 + OffsetY;
                      SelectObject( TempDC, ZoneItem.FItemMasks );
                      BitBlt( Work.Canvas.Handle, X, Y, FMap.FItemList[ ItemIndex ].Width, H, TempDC,
                        FMap.FItemList[ ItemIndex ].ImageX, FMap.FItemList[ ItemIndex ].ImageY, SRCAND );
                      SelectObject( TempDC, ZoneItem.FItemImages );
                      BitBlt( Work.Canvas.Handle, X, Y, FMap.FItemList[ ItemIndex ].Width, H, TempDC,
                        FMap.FItemList[ ItemIndex ].ImageX, FMap.FItemList[ ItemIndex ].ImageY, SRCPAINT );
                    end
                    else
                    begin
                      SelectObject( TempDC, ZoneItem.FItemMasks );
                      BitBlt( FrameBuffer.Canvas.Handle, X, Y, FMap.FItemList[ ItemIndex ].Width, H, TempDC,
                        FMap.FItemList[ ItemIndex ].ImageX, FMap.FItemList[ ItemIndex ].ImageY, SRCAND );
                      SelectObject( TempDC, ZoneItem.FItemImages );
                      BitBlt( FrameBuffer.Canvas.Handle, X, Y, FMap.FItemList[ ItemIndex ].Width, H, TempDC,
                        FMap.FItemList[ ItemIndex ].ImageX, FMap.FItemList[ ItemIndex ].ImageY, SRCPAINT );
                      //PatBlt(FrameBuffer.Canvas.handle,X,Y,FMap.FItemList[ItemIndex].Width,H,BLACKNESS);
                    end;
{$ENDIF}
                  end;
                end;
              end;
            end;
            ItemIndex := FMap.FItemList[ ItemIndex ].Next;
            if ( ItemIndex = 0 ) then
              Break;
          end;
        end;
        if Assigned( RowData^.Figure ) and ( i <= MaxRow ) then
        begin
          NextFigure := RowData^.Figure;
          RowData^.Figure := nil;
          while Assigned( NextFigure ) do
          begin
            if NextFigure.FEnabled then
            begin
              if NextFigure.Visible then
              begin
                if ( NextFigure = KeyFigure ) and TAniFigure( NextFigure ).AutoTransparent and Assigned( XRayImage ) then
                begin
                  StripX := ( XRayX1Fix - MapX * FMap.FTileWidth ) div FMap.FStripWidth;
                  StripW := ( XRayX2Fix - XRayX1Fix ) div FMap.FStripWidth + 1;
                  T := XRayY1;
                  Y := XRayY2;
                end
                else
                begin
                  StripX := ( NextFigure.FX - NextFigure.CenterX - MapX * FMap.FTileWidth ) div FMap.FStripWidth;
                  StripW := NextFigure.Width div Fmap.FStripWidth + 1;
                  if ( ( NextFigure.Width mod Fmap.FStripWidth ) <> 0 ) then
                    Inc( StripW );
                  T := i - NextFigure.CenterY - NextFigure.Z;
                  Y := T + NextFigure.Height;
                end;
                ColumnData := ColumnBase;
                if ( StripX < 0 ) then
                begin
                  Inc( StripW, StripX );
                  StripX := 0;
                end
                else
                  Inc( ColumnData, StripX );
                if ( StripW > 0 ) then
                begin
                  for j := 1 to StripW do
                  begin
                    if ( StripX + j > FMapColumnCount ) then
                      Break;
                    if ( ColumnData^.Active ) then
                    begin
                     // if (T > ColumnData^.BaseLine) then ColumnData^.Top := T
                     // else if (T < ColumnData^.Top) then ColumnData^.Top := T;
                      if ( T < ColumnData^.Top ) then
                        ColumnData^.Top := T;
                      if ( Y > ColumnData^.BaseLine ) then
                        ColumnData^.BaseLine := Y;
                    end
                    else
                    begin
                      ColumnData^.Top := T;
                      ColumnData^.BaseLine := Y;
                      ColumnData^.Active := True;
                    end;
                    Inc( ColumnData );
                  end;
                end;
              end;
              DrawFigure( NextFigure );
            end;
            LastFigure := NextFigure;
            NextFigure := LastFigure.NextOnRow;
            LastFigure.NextOnRow := nil;
          end;
        end;
        Inc( RowData );
      end;
    end;

    SelectObject( TempDC, OldTempBitmap );
    GlobalUnlock( MapColumns );
    GlobalUnlock( MapRows );
  end;

  if Assigned( KeyFigure ) and ( KeyFigure.AutoTransparent ) and Assigned( XRayImage ) then
  begin
{$IFNDEF DirectX}
    BitBlt( Work.Canvas.Handle, 320 + KeyFigure.FX - XRAyWidth div 2 - OffsetX - KeyX, 0,
      XRayWidth, XRayHeight, XRayImage.Canvas.Handle, 0, 0, SRCAND );
    BitBlt( FrameBuffer.Canvas.Handle, KeyX, KeyY,
      XRayWidth + FMap.FStripWidth, XRayHeight,
      Work.Canvas.Handle, WorkWidth div 2, 0, SRCPAINT );
{$ENDIF}
{$IFDEF DirectX}
    {    Work.BltFast(320+KeyFigure.FX-XRayWidth div 2-OffsetX-KeyX,0,
          XRayImage,Rect(0,0,XRayWidth,XRayHeight),DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
        ddck.dwColorSpaceLowValue:=FMap.FColorMatch;
        ddck.dwColorSpaceHighValue:=ddck.dwColorSpaceLowValue;
        Work.SetColorKey(DDCKEY_SRCBLT,ddck);
        lpDDSBack.BltFast(KeyX+left,KeyY+top,Work,Rect(WorkWidth div 2,0,WorkWidth div 2+XRayWidth+FMap.FStripWidth,XRayHeight),
          DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);   }
{$ENDIF}
  end;

  //Display frame buffer
  if Assigned( FOnBeforeDisplay ) then
    FOnBeforeDisplay( Self );

{$IFDEF DirectX}
  lpDDSFront.Flip( nil, DDFLIP_WAIT );
{$ENDIF}

{$IFNDEF DirectX}
  BitBlt( Canvas.Handle, 0, 0, width, Height, FrameBuffer.Canvas.Handle, 0, 0, SRCCOPY );
{$ENDIF}

  if Assigned( FOnAfterDisplay ) then
    FOnAfterDisplay( Self );

  for i := 0 to FigureList.Count - 1 do
  begin
    if TAniFigure( FigureList.Items[ i ] ).Enabled then
      TAniFigure( FigureList.Items[ i ] ).DoFrame;
  end;

  Inc( FFrameCount );
  FDrawing := False;
end;

procedure TAniView.UpdateMap;
var
  NewMapX, NewMapY : Longint;
  i, j : Longint;
  MinX, MaxX : Longint;
  MapBase, p : ^GridInfo;
  ClipX1, ClipX2, ClipY1, ClipY2 : Longint;
  X, Y : Longint;
{$IFDEF DirectX}
  SrcX1, SrcY1 : Longint;
  SrcX2, SrcY2 : Longint;
{$ENDIF}
begin
  NewMapX := OffsetX div FMap.FTileWidth;
  if ( OffsetX < 0 ) then
    if ( ( OffsetX mod FMap.FTileWidth ) <> 0 ) then
      Dec( NewMapX );
  NewMapY := OffsetY div FMap.FTileHeight;
  if ( OffsetY < 0 ) then
    if ( ( OffsetY mod FMap.FTileHeight ) <> 0 ) then
      Dec( NewMapY );
  if ( NewMapX <> MapX ) or ( NewMapY <> MapY ) then
  begin
{$IFDEF DirectX}
    i := MapX - NewMapX;
    j := MapY - NewMapY;
    if ( i > 0 ) then
    begin
      SrcX1 := 0;
      X := i * FMap.FTileWidth;
      SrcX2 := MapBitWidth - X;
    end
    else if ( i < 0 ) then
    begin
      SrcX1 := -i * FMap.FTileWidth;
      X := 0;
      SrcX2 := SrcX1 + MapBitWidth - SrcX1;
    end
    else
    begin
      SrcX1 := 0;
      X := 0;
      SrcX2 := MapBitWidth;
    end;

    if ( j > 0 ) then
    begin
      SrcY1 := 0;
      Y := j * FMap.FTileHeight;
      SrcY2 := MapBitHeight - Y;
    end
    else if ( j < 0 ) then
    begin
      SrcY1 := -j * FMap.FTileHeight;
      Y := 0;
      SrcY2 := SrcY1 + MapBitHeight - SrcY1;
    end
    else
    begin
      SrcY1 := 0;
      Y := 0;
      SrcY2 := MapBitHeight;
    end;

    lpDDSMap.BltFast( X, Y, lpDDSMap, Rect( SrcX1, SrcY1, SrcX2, SrcY2 ),
      DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
{$ENDIF}
{$IFNDEF DirectX}
    BitBlt( MapBuffer.Canvas.Handle, ( MapX - NewMapX ) * FMap.FTileWidth, ( MapY - NewMapY ) * FMap.FTileHeight,
      MapBitWidth, MapBitHeight, MapBuffer.Canvas.Handle, 0, 0, SRCCOPY );
{$ENDIF}
    //Draw empty Tiles and objects on mapbuffer
    if ( Abs( NewMapX - MapX ) > MapWidth ) or ( Abs( NewMapY - MapY ) > MapHeight ) then
    begin
      MapX := NewMapX;
      MapY := NewMapY;
      FRefreshMap;
    end
    else
    begin
      MapBase := GlobalLock( FMap.FMapData );
      if ( NewMapX < MapX ) then
      begin
        for j := 1 to MapHeight do
        begin
          Y := NewMapY + j - 1;
          if ( Y >= 0 ) and ( Y < FMap.FHeight ) then
          begin
            p := MapBase;
            Inc( p, NewMapX + Y * FMap.FWidth );
            for i := 1 to MapX - NewMapX do
            begin
              X := NewMapX + i - 1;
              if ( X >= 0 ) and ( X < FMap.FWidth ) then
              begin
                DrawTile( p, i, j, 0 );
                DrawTile( p, i, j, 1 );
              end;
              Inc( p );
            end;
          end;
        end;
        ClipX1 := NewMapX * FMap.FTileWidth;
        ClipX2 := MapX * FMap.FTileWidth;
        MinX := MapX - NewMapX + 1;
        MaxX := MapWidth;
      end
      else if ( NewMapX > MapX ) then
      begin
        for j := 1 to MapHeight do
        begin
          Y := NewMapY + j - 1;
          if ( Y >= 0 ) and ( Y < FMap.FHeight ) then
          begin
            p := MapBase;
            Inc( p, MapWidth + MapX + Y * FMap.FWidth );
            for i := MapWidth - NewMapX + MapX + 1 to MapWidth do
            begin
              X := NewMapX + i - 1;
              if ( X >= 0 ) and ( X < FMap.FWidth ) then
              begin
                DrawTile( p, i, j, 0 );
                DrawTile( p, i, j, 1 );
              end;
              Inc( p );
            end;
          end;
        end;
        ClipX1 := ( MapX + MapWidth ) * FMap.FTileWidth;
        ClipX2 := ( NewMapX + MapWidth ) * FMap.FTileWidth;
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
      if ( NewMapY < MapY ) then
      begin
        for j := 1 to MapY - NewMapY do
        begin
          Y := NewMapY + j - 1;
          if ( Y >= 0 ) and ( Y < FMap.FHeight ) then
          begin
            p := MapBase;
            Inc( p, NewMapX + MinX - 1 + ( NewMapY + j - 1 ) * FMap.FWidth );
            for i := MinX to MaxX do
            begin
              X := NewMapX + i - 1;
              if ( X >= 0 ) and ( X < FMap.FWidth ) then
              begin
                DrawTile( p, i, j, 0 );
                DrawTile( p, i, j, 1 );
              end;
              Inc( p );
            end;
          end;
        end;
        ClipY1 := NewMapY * FMap.FTileHeight;
        ClipY2 := MapY * FMap.FTileHeight;
      end
      else if ( NewMapY > MapY ) then
      begin
        for j := MapHeight - NewMapY + MapY + 1 to MapHeight do
        begin
          Y := NewMapY + j - 1;
          if ( Y >= 0 ) and ( Y < FMap.FHeight ) then
          begin
            p := MapBase;
            Inc( p, NewMapX + MinX - 1 + ( NewMapY + j - 1 ) * FMap.FWidth );
            for i := MinX to MaxX do
            begin
              X := NewMapX + i - 1;
              if ( X >= 0 ) and ( X < FMap.FWidth ) then
              begin
                DrawTile( p, i, j, 0 );
                DrawTile( p, i, j, 1 );
              end;
              Inc( p );
            end;
          end;
        end;
        ClipY1 := ( MapY + MapHeight ) * FMap.FTileHeight;
        ClipY2 := ( NewMapY + MapHeight ) * FMap.FTileheight;
      end
      else
      begin
        ClipY1 := 0;
        ClipY2 := 0;
      end;
      GlobalUnlock( FMap.FMapData );
      MapX := NewMapX;
      MapY := NewMapY;
      DrawItemsClip( ClipX1, ClipX2, ClipY1, ClipY2 );
    end;
  end;
  MapOffsetX := OffsetX mod FMap.FTileWidth;
  if ( MapOffsetX < 0 ) then
    Inc( MapOffsetX, FMap.FTileWidth );
  MapOffsetY := OffsetY mod FMap.FTileHeight;
  if ( MapOffsetY < 0 ) then
    Inc( MapOffsetY, FMap.FTileHeight );
end;

procedure TAniView.PreCreateMap( W, H : longint );
var
  ddsd : DDSurfaceDesc;
  ReturnCode : HRESULT;
begin
  MapPreCreated := true;
  Log.Log( 'Creating map buffer' );
  Log.Log( inttostr( W ) + ' x ' + inttostr( H ) );
  lpDDSMap := nil;
  ZeroMemory( @ddsd, SizeOf( ddsd ) );
  ddsd.dwSize := SizeOf( ddsd );
  ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
  ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_VIDEOMEMORY;
  ddsd.dwWidth := W;
  ddsd.dwHeight := H;
  ReturnCode := lpdd.CreateSurface( ddsd, lpDDSMap, nil );
  if ( ReturnCode = DD_OK ) then
  begin
    Log.Log( 'Map buffer created in VRAM' );
  end
  else
  begin
    Log.Log( '*** Error: Map buffer created in System RAM!' );
    if ReturnCode = DDERR_INCOMPATIBLEPRIMARY then
      Log.Log( 'DDERR_INCOMPATIBLEPRIMARY' )
    else if ReturnCode = DDERR_INVALIDCAPS then
      Log.Log( 'DDERR_INVALIDCAPS' )
    else if ReturnCode = DDERR_INVALIDOBJECT then
      Log.Log( 'DDERR_INVALIDOBJECT' )
    else if ReturnCode = DDERR_INVALIDPARAMS then
      Log.Log( 'DDERR_INVALIDPARAMS' )
    else if ReturnCode = DDERR_INVALIDPIXELFORMAT then
      Log.Log( 'DDERR_INVALIDPIXELFORMAT' )
    else if ReturnCode = DDERR_NOALPHAHW then
      Log.Log( 'DDERR_NOALPHAHW' )
    else if ReturnCode = DDERR_NOCOOPERATIVELEVELSET then
      Log.Log( 'DDERR_NOCOOPERATIVELEVELSET' )
    else if ReturnCode = DDERR_NODIRECTDRAWHW then
      Log.Log( 'DDERR_NODIRECTDRAWHW' )
    else if ReturnCode = DDERR_NOEMULATION then
      Log.Log( 'DDERR_NOEMULATION' )
    else if ReturnCode = DDERR_NOEXCLUSIVEMODE then
      Log.Log( 'DDERR_NOEXCLUSIVEMODE' )
    else if ReturnCode = DDERR_NOFLIPHW then
      Log.Log( 'DDERR_NOFLIPHW' )
    else if ReturnCode = DDERR_NOMIPMAPHW then
      Log.Log( 'DDERR_NOMIPMAPHW' )
    else if ReturnCode = DDERR_NOOVERLAYHW then
      Log.Log( 'DDERR_NOOVERLAYHW' )
    else if ReturnCode = DDERR_NOZBUFFERHW then
      Log.Log( 'DDERR_NOZBUFFERHW' )
    else if ReturnCode = DDERR_OUTOFMEMORY then
      Log.Log( 'DDERR_OUTOFMEMORY' )
    else if ReturnCode = DDERR_OUTOFVIDEOMEMORY then
      Log.Log( 'DDERR_OUTOFVIDEOMEMORY' )
    else if ReturnCode = DDERR_PRIMARYSURFACEALREADYEXISTS then
      Log.Log( 'DDERR_PRIMARYSURFACEALREADYEXISTS' )
    else if ReturnCode = DDERR_UNSUPPORTEDMODE then
      Log.Log( 'DDERR_UNSUPPORTEDMODE' );

    ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
    lpdd.CreateSurface( ddsd, lpDDSMap, nil );
  end;

  Log.Log( 'Creating work buffer' );
  Work := nil;
  ZeroMemory( @ddsd, SizeOf( ddsd ) );
  ddsd.dwSize := SizeOf( ddsd );
  ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
  ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_VIDEOMEMORY;
  ddsd.dwWidth := WorkWidth;
  ddsd.dwHeight := WorkHeight;
  if ( lpdd.CreateSurface( ddsd, Work, nil ) <> DD_OK ) then
  begin
    ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
    lpdd.CreateSurface( ddsd, Work, nil );
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
  W, H : Longint;
{$IFDEF DirectX}
  ddsd : DDSurfaceDesc;
  ddck : DDCOLORKEY;
  ReturnCode : HRESULT;
{$ENDIF}
begin
  if ( FMap = nil ) then
    Exit;
//TZone(FMap.Zones[1]).ExportTiles('f:\zone1tiles.bmp');
//TZone(FMap.Zones[2]).ExportTiles('f:\zone2tiles.bmp');
{TZone(FMap.Zones[0]).ExportItems('f:\zone0items.bmp');
TZone(FMap.Zones[1]).ExportItems('f:\zone1items.bmp');
TZone(FMap.Zones[2]).ExportItems('f:\zone2items.bmp');
TZone(FMap.Zones[3]).ExportItems('f:\zone3items.bmp');
TZone(FMap.Zones[4]).ExportItems('f:\zone4items.bmp');
TZone(FMap.Zones[5]).ExportItems('f:\zone5items.bmp');
TZone(FMap.Zones[6]).ExportItems('f:\zone6items.bmp');
TZone(FMap.Zones[7]).ExportItems('f:\zone7items.bmp');
TZone(FMap.Zones[8]).ExportItems('f:\zone8items.bmp'); }
{TZone(FMap.Zones[9]).ExportItems('f:\zone9items.bmp');
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
//TZone(FMap.Zones[1]).ExportTiles('f:\zone1tiles.bmp');
//TZone(FMap.Zones[2]).ExportTiles('f:\zone2tiles.bmp');
//TZone(FMap.Zones[3]).ExportTiles('f:\zone3tiles.bmp');
//TZone(FMap.Zones[1]).ExportItems('f:\zone1items.bmp');
//TZone(FMap.Zones[2]).ExportItems('f:\zone2items.bmp');
//TZone(FMap.Zones[3]).ExportItems('f:\zone3items.bmp');

  MapWidth := ( width div FMap.FTileWidth ) + 2;
  MapHeight := ( Height div FMap.FTileHeight ) + 2;
  MapBitWidth := MapWidth * FMap.FTileWidth;
  MapBitHeight := MapHeight * FMap.FTileHeight;
  W := MapBitWidth;
  H := MapBitHeight;
{$IFDEF DirectX}
  if not MapPreCreated then
  begin
    Log.Log( 'Creating map buffer' );
    Log.Log( inttostr( W ) + ' x ' + inttostr( H ) );
    lpDDSMap := nil;
    ZeroMemory( @ddsd, SizeOf( ddsd ) );
    ddsd.dwSize := SizeOf( ddsd );
    ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
    ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_VIDEOMEMORY;
    ddsd.dwWidth := W;
    ddsd.dwHeight := H;
    ReturnCode := lpdd.CreateSurface( ddsd, lpDDSMap, nil );
    if ( ReturnCode = DD_OK ) then
    begin
      Log.Log( 'Map buffer created in VRAM' );
    end
    else
    begin
      Log.Log( '*** Error: Map buffer created in System RAM!' );
      if ReturnCode = DDERR_INCOMPATIBLEPRIMARY then
        Log.Log( 'DDERR_INCOMPATIBLEPRIMARY' )
      else if ReturnCode = DDERR_INVALIDCAPS then
        Log.Log( 'DDERR_INVALIDCAPS' )
      else if ReturnCode = DDERR_INVALIDOBJECT then
        Log.Log( 'DDERR_INVALIDOBJECT' )
      else if ReturnCode = DDERR_INVALIDPARAMS then
        Log.Log( 'DDERR_INVALIDPARAMS' )
      else if ReturnCode = DDERR_INVALIDPIXELFORMAT then
        Log.Log( 'DDERR_INVALIDPIXELFORMAT' )
      else if ReturnCode = DDERR_NOALPHAHW then
        Log.Log( 'DDERR_NOALPHAHW' )
      else if ReturnCode = DDERR_NOCOOPERATIVELEVELSET then
        Log.Log( 'DDERR_NOCOOPERATIVELEVELSET' )
      else if ReturnCode = DDERR_NODIRECTDRAWHW then
        Log.Log( 'DDERR_NODIRECTDRAWHW' )
      else if ReturnCode = DDERR_NOEMULATION then
        Log.Log( 'DDERR_NOEMULATION' )
      else if ReturnCode = DDERR_NOEXCLUSIVEMODE then
        Log.Log( 'DDERR_NOEXCLUSIVEMODE' )
      else if ReturnCode = DDERR_NOFLIPHW then
        Log.Log( 'DDERR_NOFLIPHW' )
      else if ReturnCode = DDERR_NOMIPMAPHW then
        Log.Log( 'DDERR_NOMIPMAPHW' )
      else if ReturnCode = DDERR_NOOVERLAYHW then
        Log.Log( 'DDERR_NOOVERLAYHW' )
      else if ReturnCode = DDERR_NOZBUFFERHW then
        Log.Log( 'DDERR_NOZBUFFERHW' )
      else if ReturnCode = DDERR_OUTOFMEMORY then
        Log.Log( 'DDERR_OUTOFMEMORY' )
      else if ReturnCode = DDERR_OUTOFVIDEOMEMORY then
        Log.Log( 'DDERR_OUTOFVIDEOMEMORY' )
      else if ReturnCode = DDERR_PRIMARYSURFACEALREADYEXISTS then
        Log.Log( 'DDERR_PRIMARYSURFACEALREADYEXISTS' )
      else if ReturnCode = DDERR_UNSUPPORTEDMODE then
        Log.Log( 'DDERR_UNSUPPORTEDMODE' );

      ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
      lpdd.CreateSurface( ddsd, lpDDSMap, nil );
    end;
  end;

  if FMap.NeedColorMatch then
  begin
    FMap.FColorMatch := FindColorMatch( FMap.FTransparentColor );
    FMap.NeedColorMatch := false;
  end;
  ddck.dwColorSpaceLowValue := FMap.FColorMatch;
  ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
  lpDDSMap.SetColorKey( DDCKEY_SRCBLT, ddck );

  ddck.dwColorSpaceLowValue := FMap.FColorMatch;
  ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
  lpDDSBack.SetColorKey( DDCKEY_SRCBLT, ddck );

  if not MapPreCreated then
  begin
    Log.Log( 'Creating work buffer' );
    Work := nil;
    ZeroMemory( @ddsd, SizeOf( ddsd ) );
    ddsd.dwSize := SizeOf( ddsd );
    ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
    ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_VIDEOMEMORY;
    ddsd.dwWidth := WorkWidth;
    ddsd.dwHeight := WorkHeight;
    if ( lpdd.CreateSurface( ddsd, Work, nil ) <> DD_OK ) then
    begin
      ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
      lpdd.CreateSurface( ddsd, Work, nil );
    end;
  end;
{$ENDIF}
{$IFNDEF DirectX}
  MapBuffer.width := W;
  MapBuffer.Height := H;
{$ENDIF}
  Log.Log( 'MapColumns' );
  if ( MapColumns <> 0 ) then
    GlobalFree( MapColumns );
  MapColumns := 0;
  FMapColumnCount := MapWidth shl 2;
  MapColumns := GlobalAlloc( GPTR, FMapColumnCount * SizeOf( MapColumnHeaderInfo ) );
  Log.Log( 'MapRows' );
  if ( MapRows <> 0 ) then
    GlobalFree( MapRows );
  MapRows := 0;
  PixelHeight := ( FMap.FHeight + 32 ) * FMap.FTileHeight; //32 added to correct off-bottom drawing errors
  MapRows := GlobalAlloc( GPTR, ( PixelHeight + FMap.FTileHeight ) * SizeOf( RowUpdateInfo ) );
  Log.Log( 'BuildRowUpdateInfo' );

  BuildRowUpdateInfo;
  MapX := OffsetX div FMap.FTileWidth;
  MapY := OffsetY div FMap.FTileheight;
  Log.Log( 'Assignment Complete' );
end;

procedure TAniView.RefreshRegion( X, Y, W, H : Longint );
var
  i, j : Integer;
  MapBase, p : ^GridInfo;
  Layer : Integer;
  X1, Y1 : Longint;
  X2, Y2 : Longint;
  XA, YA : Longint;
  MinX, MaxX : Longint;
  MaxY, MaxRow : longint;
  RowBase, RowData : ^RowUpdateInfo;
  R : TRect;
begin
  MinX := ( X div FMap.FStripWidth ) * FMap.FStripWidth;
  MaxX := ( ( X + W ) div FMap.FStripWidth + 1 ) * FMap.FStripWidth;

  X1 := X div FMap.FTileWidth;
  if X1 < 0 then
    X1 := 0;
  Y1 := Y div FMap.FTileHeight;
  if Y1 < 0 then
    Y1 := 0;
  X2 := ( X + W ) div FMap.FTileWidth;
  if X2 >= FMap.FWidth then
    X2 := FMap.FWidth - 1;
  Y2 := ( Y + H ) div FMap.FTileHeight;
  if Y2 >= FMap.FHeight then
    Y2 := FMap.FHeight - 1;
{$IFDEF DirectX}
  R.Left := MinX - OffsetX + Left;
  R.Right := MaxX - OffsetX + Left;
  R.Top := Y - OffsetY + Top;
  R.Bottom := Y + H - OffsetY + Top;
{$ENDIF}
{$IFNDEF DirectX}
  R.Left := MinX - OffsetX;
  R.Right := MaxX - OffsetX;
  R.Top := Y - OffsetY;
  R.Bottom := Y + H - OffsetY;
{$ENDIF}
  MapBase := GlobalLock( FMap.FMapData );
  for Layer := 0 to 1 do
  begin
    for j := Y1 to Y2 do
    begin
      YA := j * FMap.FTileHeight - OffsetY;
      p := MapBase;
      Inc( p, X1 + j * FMap.FWidth );
      for i := X1 to X2 do
      begin
        XA := i * FMap.FTileWidth - OffsetX;
{$IFDEF DIRECTX}
        CopyTile( lpDDSBack, p, XA + Left, YA + Top, Layer, @R );
{$ENDIF}
{$IFNDEF DIRECTX}
        CopyTile( FrameBuffer.Canvas.Handle, p, XA, YA, Layer, @R );
{$ENDIF}
        Inc( p );
      end;
    end;
  end;
  GlobalUnlock( FMap.FMapData );

  //Update items (items with descenders)
  RowBase := GlobalLock( MapRows );
  RowData := RowBase;
  if ( Y > 0 ) then
    Inc( RowData, Y );
  i := RowData^.DescendRow;
  RowData := RowBase;
  Inc( RowData, i );
  i := RowData^.ItemIndex;
  if ( i = 0 ) then
  begin
    GlobalUnlock( MapRows );
    Exit;
  end;

  MaxY := Y + H;
  if ( MaxY > PixelHeight ) then
    MaxRow := PixelHeight
  else
  begin
    Inc( RowBase, MaxY );
    MaxRow := RowBase^.OverlapRow;
  end;

  X1 := R.Left;
  X2 := R.Right;
  Y1 := R.Top;
  Y2 := R.Bottom;

  while ( i <> 0 ) do
  begin
    if ( FMap.FItemList[ i ].Y > MaxRow ) and ( FMap.FItemList[ i ].Y <= PixelHeight ) then
      Break;
    if FMap.FItemList[ i ].Visible then
    begin
      if ( FMap.FItemList[ i ].X >= MinX ) and ( FMap.FItemList[ i ].X < MaxX ) then
      begin
        if ( FMap.FItemList[ i ].FilterID = 0 ) or ( ( FMap.FItemList[ i ].FilterID < 0 ) or
          ( FMap.FItemList[ i ].FilterID = FItemMask ) ) and
          ( FMap.FItemList[ i ].FilterID <> -FItemMask ) then
        begin

          R.Left := FMap.FItemList[ i ].ImageX;
          R.Right := FMap.FItemList[ i ].ImageX + FMap.FItemList[ i ].Width;
          R.Top := FMap.FItemList[ i ].ImageY + FMap.FItemList[ i ].VHeight;
          R.Bottom := FMap.FItemList[ i ].ImageY + FMap.FItemList[ i ].Height;

          XA := FMap.FItemList[ i ].X - OffsetX + Left;
          YA := FMap.FItemList[ i ].Y - OffsetY + Top;
          Clip1( X1, X2, XA, R.Left, R.Right );
          Clip1( Y1, Y2, YA, R.Top, R.Bottom );

          lpDDSBack.BltFast( XA, YA, TZone( FMap.Zones.Items[ FMap.FItemList[ i ].Zone ] ).FItemImages,
            R, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        end;
      end;
    end;
    i := FMap.FItemList[ i ].Next;
  end;
end;

{$IFDEF DIRECTX} //Flickering lights are only supported in DirectX

procedure TAniView.RefreshLight( Zone : TLightZone );
var
  i, j : Integer;
  MapBase, p : ^GridInfo;
  Layer : Integer;
  X, Y, W, H : Longint;
  Index : Word;
  SrcX, SrcY : Longint;
  MaxY : Longint;
  SrcY1, SrcY2 : Longint;
  SrcX1, SrcX2 : longint;
  Ya, SrcY1a, SrcY2a : Longint;
  DstH : Longint;
  Offset : Integer;
  NewState : Integer;
  ZoneX, ZoneY : integer;
begin
  case Zone.Flicker of
    flFluorescent :
      begin
        if ( Zone.StateDuration <= 0 ) then
        begin
          if Zone.State = Zone.States then
          begin
            NewState := 1;
          end
          else
          begin
            NewState := Zone.States;
          end;
          if random( 2 ) = 0 then
          begin
            Zone.StateDuration := random( 50 ) + 5;
          end
          else
          begin
            Zone.StateDuration := 1;
          end;
        end
        else
        begin
          Dec( Zone.StateDuration );
          NewState := Zone.State;
        end;
      end;
    flCandle :
      begin
        if ( Zone.StateDuration <= 0 ) then
        begin
          if Zone.State = Zone.States then
          begin
            NewState := random( 3 ) + 1;
            if random( 8 ) = 0 then
            begin
              Zone.StateDuration := random( 10 ) + 5;
              Zone.Blinking := False;
            end
            else
            begin
              Zone.StateDuration := random( 2 ) + 4;
            end;
          end
          else
          begin
            if Zone.Blinking or ( random( 32 ) = 0 ) then
            begin
              NewState := Zone.States;
              Zone.StateDuration := random( 2 ) + 4;
              Zone.Blinking := True;
            end
            else
            begin
              NewState := random( 3 ) + 1;
              Zone.StateDuration := random( 10 ) + 5;
            end;
          end;
        end
        else
        begin
          Dec( Zone.StateDuration );
          NewState := Zone.State;
        end;
      end;
    flTorch :
      begin
        if ( Zone.StateDuration <= 0 ) then
        begin
          NewState := random( 3 ) + 1;
          Zone.StateDuration := random( 8 ) + 1;
        end
        else
        begin
          Dec( Zone.StateDuration );
          NewState := Zone.State;
        end;
      end;
  else
    NewState := Zone.State;
  end;

  Offset := ( Zone.States - NewState ) * Zone.TileStateOffset;
  MapBase := GlobalLock( FMap.FMapData );
  for Layer := 0 to 1 do
  begin
    for j := Zone.Y1 to Zone.Y2 do
    begin
      Y := ( j - Zone.Y1 ) * FMap.FTileHeight;
      p := MapBase;
      Inc( p, Zone.X1 + j * FMap.FWidth );
      for i := Zone.X1 to Zone.X2 do
      begin
        if FMap.Zones.Items[ p^.Zone[ Layer ] ] = Zone then
        begin
          X := ( i - Zone.X1 ) * FMap.FTileWidth;
          Index := p^.Tile[ Layer ];
          if ( Index <> $FFFF ) then
          begin
            Dec( Index, Offset );
            SrcX := ( Index div Zone.FTileMaxColumnIndex ) * FMap.FTileWidth;
            SrcY := ( Index mod Zone.FTileMaxColumnIndex ) * FMap.FTileHeight;
            if ( Layer = 0 ) then
            begin
              Work.BltFast( X, Y, Zone.FTileImages, Rect( SrcX, SrcY,
                SrcX + FMap.FTileWidth, SrcY + FMap.FTileHeight ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
            end
            else
            begin
              Work.BltFast( X, Y, Zone.FTileImages, Rect( SrcX, SrcY,
                SrcX + FMap.FTileWidth, SrcY + FMap.FTileheight ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
            end;
          end;
        end;
        Inc( p );
      end;
    end;
  end;
  GlobalUnlock( FMap.FMapData );

  ZoneX := Zone.X1 * FMap.FTileWidth - OffsetX;
  ZoneY := Zone.Y1 * FMap.FTileHeight - OffsetY;
  W := ( Zone.X2 - Zone.X1 + 1 ) * FMap.FTileWidth;
  H := ( Zone.Y2 - Zone.Y1 + 1 ) * FMap.FTileHeight;

  Offset := ( Zone.States - NewState ) * Zone.ItemStateOffset;
  MaxY := MapHeight * FMap.FTileHeight;
  if Assigned( Zone.Items ) then
  begin
    for i := 0 to Zone.Items.Count - 1 do
    begin
      if ItemInstanceInfo( Zone.Items.Items[ i ]^ ).Visible and ( ItemInstanceInfo( Zone.Items.Items[ i ]^ ).FilterID = 0 ) or
        ( ( ItemInstanceInfo( Zone.Items.Items[ i ]^ ).FilterID < 0 ) or
        ( ItemInstanceInfo( Zone.Items.Items[ i ]^ ).FilterID = FItemMask ) ) and
        ( ItemInstanceInfo( Zone.Items.Items[ i ]^ ).FilterID <> -FItemMask ) then
      begin

        X := ItemInstanceInfo( Zone.Items.Items[ i ]^ ).X - OffsetX;
        Y := ItemInstanceInfo( Zone.Items.Items[ i ]^ ).Y - ItemInstanceInfo( Zone.Items.Items[ i ]^ ).VHeight - OffsetY;
        DstH := Y + ItemInstanceInfo( Zone.Items.Items[ i ]^ ).Height;
        if ( Y < 0 ) then
        begin
          SrcY1 := ItemInstanceInfo( Zone.Items.Items[ i ]^ ).ImageY - Y;
          SrcY2 := ItemInstanceInfo( Zone.Items.Items[ i ]^ ).ImageY + ItemInstanceInfo( Zone.Items.Items[ i ]^ ).Height;
          if ( DstH > MapBitHeight ) then
            Dec( SrcY2, DstH - MaxY );
          Y := 0;
        end
        else
        begin
          SrcY1 := ItemInstanceInfo( Zone.Items.Items[ i ]^ ).ImageY;
          SrcY2 := ItemInstanceInfo( Zone.Items.Items[ i ]^ ).ImageY + ItemInstanceInfo( Zone.Items.Items[ i ]^ ).Height;
          if ( DstH > MapBitHeight ) then
            Dec( SrcY2, DstH - MaxY );
        end;

        SrcX1 := ItemInstanceInfo( Zone.Items.Items[ i ]^ ).ImageX - Offset;
        SrcX2 := ItemInstanceInfo( Zone.Items.Items[ i ]^ ).ImageX + ItemInstanceInfo( Zone.Items.Items[ i ]^ ).Width - Offset;

        Ya := Y;
        SrcY1a := SrcY1;
        SrcY2a := SrcY2;

        Clip1( ZoneY, ZoneY + H, Ya, SrcY1a, SrcY2a );
        Work.BltFast( X - ZoneX, Ya - ZoneY, Zone.FItemImages, Rect( SrcX1, SrcY1a, SrcX2, SrcY2a ),
          DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

        //also clip x against edge of screen
        Clip1( Y, ZoneY, Y, SrcY1, SrcY2 );
        Clip1( 0, Width, X, SrcX1, SrcX2 );
        lpDDSBack.BltFast( X + Left, Y + Top, Zone.FItemImages, Rect( SrcX1, SrcY1, SrcX2, SrcY2 ),
          DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end;
    end;
  end;
  Zone.State := NewState;

  SrcX := 0;
  SrcY := 0;
  if ( ZoneX < 0 ) then
  begin
    Dec( SrcX, ZoneX );
    Inc( W, ZoneX );
    ZoneX := 0;
  end;
  if ( ZoneY < 0 ) then
  begin
    Dec( SrcY, ZoneY );
    Inc( H, ZoneY );
    ZoneY := 0;
  end;
  if ( ZoneX + W > Width ) then
  begin
    W := Width - ZoneX;
  end;
  if ( ZoneY + H > Height ) then
  begin
    H := Height - ZoneY;
  end;

  Work.BltFast( SrcX, SrcY, lpDDSBack, Rect( ZoneX + Left, ZoneY + Top, ZoneX + Left + W, ZoneY + Top + H ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
  lpDDSBack.BltFast( ZoneX + Left, ZoneY + Top, Work, Rect( SrcX, SrcY, SrcX + W, SrcY + H ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
end;
{$ENDIF}

procedure TAniView.FRefreshMap;
var
  i, j : Integer;
  MapBase, p : ^GridInfo;
  Layer : Integer;
  X, Y : Longint;
begin
//Log.Log('Refresh Map a');
  MapBase := GlobalLock( FMap.FMapData );
  for Layer := 0 to 1 do
  begin
    for j := 1 to MapHeight do
    begin
      Y := MapY + j - 1;
      if ( Y >= 0 ) and ( Y < FMap.FHeight ) then
      begin
        p := MapBase;
        Inc( p, MapX + Y * FMap.FWidth );
        for i := 1 to MapWidth do
        begin
          X := MapX + i - 1;
          if ( X >= 0 ) and ( X < FMap.FWidth ) then
          begin
//Log.Log('Refresh Map b');
            DrawTile( p, i, j, Layer );
//Log.Log('Refresh Map c');
          end;
          Inc( p );
        end;
      end;
    end;
  end;
//Log.Log('Refresh Map d');
  GlobalUnlock( FMap.FMapData );
//Log.Log('Refresh Map e');
  DrawItems;
//Log.Log('Refresh Map f');
end;

procedure TAniView.RefreshMap;
begin
  MapX := OffsetX div FMap.FTileWidth;
  if ( OffsetX < 0 ) then
    if ( ( OffsetX mod FMap.FTileWidth ) <> 0 ) then
      Dec( MapX );
  MapY := OffsetY div FMap.FTileHeight;
  if ( OffsetY < 0 ) then
    if ( ( OffsetY mod FMap.FTileHeight ) <> 0 ) then
      Dec( MapY );

  MapOffsetX := OffsetX mod FMap.FTileWidth;
  if ( MapOffsetX < 0 ) then
    Inc( MapOffsetX, FMap.FTileWidth );
  MapOffsetY := OffsetY mod FMap.FTileHeight;
  if ( MapOffsetY < 0 ) then
    Inc( MapOffsetY, FMap.FTileHeight );
  FRefreshMap;
end;

procedure TAniView.DrawItems;
var
  i : Word;
  MinX, MinY, MaxX, MaxY : Longint;
  RowBase, RowData : ^RowUpdateInfo;
  MaxRow : Longint;
  X, Y : Longint;
  ZoneItem : TZone;
{$IFDEF DirectX}
  SrcY1, SrcY2 : Longint;
  MapH, DstH : Longint;
{$ENDIF}
begin
  MinY := MapY * FMap.FTileHeight;
  if ( MinY > PixelHeight ) then
    Exit;
  MinX := MapX * FMap.FTileWidth;
  MaxY := ( MapY + MapHeight ) * FMap.FTileHeight;
  MaxX := ( MapX + MapWidth ) * FMap.FTileWidth;
{$IFDEF DirectX}
  MapH := MapBitHeight;
{$ENDIF}
  RowBase := GlobalLock( MapRows );
  RowData := RowBase;
  if ( MinY > 0 ) then
    Inc( RowData, MinY );
  i := RowData^.DescendRow;
  RowData := RowBase;
  Inc( RowData, i );
  i := RowData^.ItemIndex;
  if ( MaxY > PixelHeight ) then
    MaxRow := PixelHeight
  else
  begin
    Inc( RowBase, MaxY );
    MaxRow := RowBase^.OverlapRow;
  end;
  GlobalUnlock( MapRows );

  while ( i <> 0 ) do
  begin
    if ( FMap.FItemList[ i ].Y > MaxRow ) and ( FMap.FItemList[ i ].Y <= PixelHeight ) then
      Break;
    if FMap.FItemList[ i ].Visible then
    begin
      if ( FMap.FItemList[ i ].FilterID = 0 ) or ( ( FMap.FItemList[ i ].FilterID < 0 ) or
        ( FMap.FItemList[ i ].FilterID = FItemMask ) ) and
        ( FMap.FItemList[ i ].FilterID <> -FItemMask ) then
      begin
        Y := FMap.FItemList[ i ].Y - FMap.FItemList[ i ].VHeight;
        if ( FMap.FItemList[ i ].X < MaxX ) and ( Y + FMap.FItemList[ i ].Height >= MinY ) then
        begin
          if ( FMap.FItemList[ i ].X + FMap.FItemList[ i ].Width > MinX ) and ( Y < MaxY ) then
          begin
            X := FMap.FItemList[ i ].X - MinX;
            dec( Y, MinY );
            ZoneItem := TZone( FMap.Zones.Items[ FMap.FItemList[ i ].Zone ] );
{$IFDEF DirectX}
            DstH := Y + FMap.FItemList[ i ].Height;
            if ( Y < 0 ) then
            begin
              SrcY1 := FMap.FItemList[ i ].ImageY - Y;
              SrcY2 := FMap.FItemList[ i ].ImageY + FMap.FItemList[ i ].Height;
              if ( DstH > MapH ) then
                Dec( SrcY2, DstH - MapH );
              Y := 0;
            end
            else
            begin
              SrcY1 := FMap.FItemList[ i ].ImageY;
              SrcY2 := FMap.FItemList[ i ].ImageY + FMap.FItemList[ i ].Height;
              if ( DstH > MapH ) then
                Dec( SrcY2, DstH - MapH );
            end;

            if ZoneItem is TLightZone then
            begin
              if not ZoneItem.FullRefresh then
              begin
                lpDDSMap.BltFast( X, Y, ZoneItem.FItemImages, Rect( FMap.FItemList[ i ].ImageX, SrcY1,
                  FMap.FItemList[ i ].ImageX + FMap.FItemList[ i ].Width, SrcY2 ),
                  DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
              end;
            end
            else
            begin
              lpDDSMap.BltFast( X, Y, ZoneItem.FItemImages, Rect( FMap.FItemList[ i ].ImageX, SrcY1,
                FMap.FItemList[ i ].ImageX + FMap.FItemList[ i ].Width, SrcY2 ),
                DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
            end;
{$ENDIF}
{$IFNDEF DirectX}
            SelectObject( TempDC, ZoneItem.FItemMasks );
            BitBlt( MapBuffer.Canvas.Handle, X, Y, FMap.FItemList[ i ].Width, FMap.FItemList[ i ].Height,
              TempDC, FMap.FItemList[ i ].ImageX, FMap.FItemList[ i ].ImageY, SRCAND );
            SelectObject( TempDC, ZoneItem.FItemImages );
            BitBlt( MapBuffer.Canvas.Handle, X, Y, FMap.FItemList[ i ].Width, FMap.FItemList[ i ].Height,
              TempDC, FMap.FItemList[ i ].ImageX, FMap.FItemList[ i ].ImageY, SRCPAINT );
{$ENDIF}
          end;
        end;
      end;
    end;
    i := FMap.FItemList[ i ].Next;
  end;
{$IFNDEF DirectX}
  SelectObject( TempDC, OldTempBitmap );
{$ENDIF}
end;

procedure TAniView.DrawItemsClip( X1, X2, Y1, Y2 : Longint );
var
  i : Word;
  MinX, MinY, MaxX, MaxY : Longint;
  X3, Y3, Y : Longint;
  SrcX, SrcY, W, W1, H, DstX, DstY : Longint;
  RowBase, RowData : ^RowUpdateInfo;
  MaxRow : Longint;
  ZoneItem : TZone;
{$IFDEF DirectX}
  SrcY1, SrcY2 : Longint;
  MapH, DstH : Longint;
{$ENDIF}
begin
  MinY := MapY * FMap.FTileheight;
  if ( MinY > PixelHeight ) then
    Exit;
  MinX := MapX * FMap.FTileWidth;
  MaxY := ( MapY + MapHeight ) * FMap.FTileHeight;
  MaxX := ( MapX + MapWidth ) * FMap.FTileWidth;
{$IFDEF DirectX}
  MapH := MapBitHeight;
{$ENDIF}
  RowBase := GlobalLock( MapRows );
  RowData := RowBase;
  if ( MinY > 0 ) then
    Inc( RowData, MinY );
  i := RowData^.DescendRow;
  RowData := RowBase;
  Inc( RowData, i );
  i := RowData^.ItemIndex;
  if ( i = 0 ) then
  begin
    GlobalUnlock( MapRows );
    Exit;
  end;
  if ( MaxY > PixelHeight ) then
    MaxRow := PixelHeight
  else
  begin
    Inc( RowBase, MaxY );
    MaxRow := RowBase^.OverlapRow;
  end;
  GlobalUnlock( MapRows );

  while ( i <> 0 ) do
  begin
    if ( FMap.FItemList[ i ].Y > MaxRow ) and ( FMap.FItemList[ i ].Y <= PixelHeight ) then
      Break;
    if FMap.FItemList[ i ].Visible then
    begin
      if ( FMap.FItemList[ i ].FilterID = 0 ) or ( ( FMap.FItemList[ i ].FilterID < 0 ) or
        ( FMap.FItemList[ i ].FilterID = FItemMask ) ) and
        ( FMap.FItemList[ i ].FilterID <> -FItemMask ) then
      begin
        if ( FMap.FItemList[ i ].X < MaxX ) then
        begin
          Y := FMap.FItemList[ i ].Y - FMap.FItemList[ i ].VHeight + FMap.FItemList[ i ].Height;
          if ( Y >= MinY ) then
          begin
            W := FMap.FItemList[ i ].Width;
            W1 := W;
            H := FMap.FItemList[ i ].Height;
            X3 := FMap.FItemList[ i ].X + W;
            Y3 := FMap.FItemList[ i ].Y - FMap.FItemList[ i ].VHeight;
            ZoneItem := TZone( FMap.Zones.Items[ FMap.FItemList[ i ].Zone ] );
            if ( X3 > MinX ) and ( Y3 < MaxY ) then
            begin
              if ( X3 > X1 ) and ( FMap.FItemList[ i ].X < X2 ) then
              begin
                //Perform clipping
                if ( FMap.FItemList[ i ].X <= X1 ) then
                begin
                  SrcX := FMap.FItemList[ i ].ImageX + W - X3 + X1;
                  DstX := X1 - MinX;
                  if ( X3 < X2 ) then
                    W := X3 - X1
                  else
                    W := X2 - X1;
                end
                else
                begin
                  SrcX := FMap.FItemList[ i ].ImageX;
                  DstX := FMap.FItemList[ i ].X - MinX;
                  W := X2 - FMap.FItemList[ i ].X;
                  if ( W > FMap.FItemList[ i ].Width ) then
                    W := FMap.FItemList[ i ].Width;
                end;
                DstY := Y3 - MinY;
{$IFDEF DirectX}
                DstH := DstY + H;
                if ( DstY < 0 ) then
                begin
                  SrcY1 := FMap.FItemList[ i ].ImageY - DstY;
                  SrcY2 := FMap.FItemList[ i ].ImageY + H;
                  if ( DstH > MapH ) then
                    Dec( SrcY2, DstH - MapH );
                  DstY := 0;
                end
                else
                begin
                  SrcY1 := FMap.FItemList[ i ].ImageY;
                  SrcY2 := FMap.FItemList[ i ].ImageY + H;
                  if ( DstH > MapH ) then
                    Dec( SrcY2, DstH - MapH );
                end;

                if SrcY2 > SrcY1 then
                begin
                  if ZoneItem is TLightZone then
                  begin
                    if not ZoneItem.FullRefresh then
                    begin
                      lpDDSMap.BltFast( DstX, DstY, ZoneItem.FItemImages, Rect( SrcX, SrcY1,
                        SrcX + W, SrcY2 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
                    end;
                  end
                  else
                  begin
                    lpDDSMap.BltFast( DstX, DstY, ZoneItem.FItemImages, Rect( SrcX, SrcY1,
                      SrcX + W, SrcY2 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
                  end;
                end;
{$ENDIF}
{$IFNDEF DirectX}
                SelectObject( TempDC, ZoneItem.FItemMasks );
                BitBlt( MapBuffer.Canvas.Handle, DstX, DstY, W, H, TempDC, SrcX, FMap.FItemList[ i ].ImageY, SRCAND );
                SelectObject( TempDC, ZoneItem.FItemImages );
                BitBlt( MapBuffer.Canvas.Handle, DstX, DstY, W, H, TempDC, SrcX, FMap.FItemList[ i ].ImageY, SRCPAINT );
{$ENDIF}
              end;

              if ( Y > Y1 ) and ( Y3 < Y2 ) then
              begin
                if ( Y3 <= Y1 ) then
                begin
                  SrcY := FMap.FItemList[ i ].ImageY + Y1 - Y3;
                  DstY := Y1 - MinY;
                  if ( Y < Y2 ) then
                    H := Y - Y1
                  else
                    H := Y2 - Y1;
                end
                else
                begin
                  SrcY := FMap.FItemList[ i ].ImageY;
                  DstY := Y3 - MinY;
                  H := Y2 - Y3;
                  if ( H > FMap.FItemList[ i ].Height ) then
                    H := FMap.FItemList[ i ].Height;
                end;
                DstX := FMap.FItemList[ i ].X - MinX;
{$IFDEF DirectX}
                DstH := DstY + H;
                if ( DstY < 0 ) then
                begin
                  SrcY1 := SrcY - DstY;
                  SrcY2 := SrcY + H;
                  if ( DstH > MapH ) then
                    Dec( SrcY2, DstH - MapH );
                  DstY := 0;
                end
                else
                begin
                  SrcY1 := SrcY;
                  SrcY2 := SrcY + H;
                  if ( DstH > MapH ) then
                    Dec( SrcY2, DstH );
                end;

                if W > 0 then
                begin
                  if ZoneItem is TLightZone then
                  begin
                    if not ZoneItem.FullRefresh then
                    begin
                      lpDDSMap.BltFast( DstX, DstY, ZoneItem.FItemImages, Rect( FMap.FItemList[ i ].ImageX, SrcY1,
                        FMap.FItemList[ i ].ImageX + W1, SrcY2 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
                    end;
                  end
                  else
                  begin
                    lpDDSMap.BltFast( DstX, DstY, ZoneItem.FItemImages, Rect( FMap.FItemList[ i ].ImageX, SrcY1,
                      FMap.FItemList[ i ].ImageX + W1, SrcY2 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
                  end;
                end;
{$ENDIF}
{$IFNDEF DirectX}
                SelectObject( TempDC, ZoneItem.FItemMasks );
                BitBlt( MapBuffer.Canvas.Handle, DstX, DstY, W1, H, TempDC, FMap.FItemList[ i ].ImageX, SrcY, SRCAND );
                SelectObject( TempDC, ZoneItem.FItemImages );
                BitBlt( MapBuffer.Canvas.Handle, DstX, DstY, W1, H, TempDC, FMap.FItemList[ i ].ImageX, SrcY, SRCPAINT );
{$ENDIF}
              end;

            end;
          end;
        end;
      end;
    end;
    i := FMap.FItemList[ i ].Next;
  end;
{$IFNDEF DirectX}
  SelectObject( TempDC, OldTempBitmap );
{$ENDIF}
end;

procedure TAniView.DrawTile( GridLoc : Pointer; i, j, Layer : Integer );
var
  X, Y : Integer;
begin
  X := ( i - 1 ) * FMap.FTileWidth;
  Y := ( j - 1 ) * FMap.FTileHeight;
{$IFDEF DirectX}
  CopyTile( lpDDSMap, GridLoc, X, Y, Layer, nil );
{$ENDIF}
{$IFNDEF DirectX}
  CopyTile( MapBuffer.Canvas.Handle, GridLoc, X, Y, Layer, nil );
{$ENDIF}
  PGridInfo( GridLoc ).BitField := PGridInfo( GridLoc ).BitField or $40; //This space has been viewed on screen
end;

{$IFDEF DirectX}

procedure TAniView.CopyTile( Dest : IDirectDrawSurface; GridLoc : Pointer; X, Y, Layer : Integer; ClipRect : PRect );
{$ENDIF}
{$IFNDEF DirectX}
  procedure TAniView.CopyTile( Dest : HDC; GridLoc : Pointer; X, Y, Layer : Integer; ClipRect : PRect );
  {$ENDIF}
  var
    Index : Word;
    SrcX, SrcY : Longint;
    DstX, DstY : Longint;
    p : ^GridInfo;
    ZoneTile : TZone;
    HalfWidth, HalfHeight : Integer;
  {$IFDEF DirectX}
    BltFx : DDBLTFX;
    SrcX2, SrcY2 : Longint;
    Offset : Integer;
  {$ENDIF}
  {$IFNDEF DirectX}
    W, H : Integer;
  {$ENDIF}
  {$IFDEF DEBUG}
    Mask, k : Word;
    DC : HDC;
  {$ENDIF}
  begin
    p := GridLoc;
    Index := p^.Tile[ Layer ];
    if ( Layer = 1 ) and ( ( p^.BitField and $80 ) <> 0 ) then
    begin
{$IFNDEF DirectX}
      HalfWidth := FMap.FTileWidth div 2;
      HalfHeight := FMap.FTileHeight div 2;

      if ( Index <> $FFFF ) then
      begin
        ZoneTile := TZone( FMap.Zones.Items[ p^.Zone[ 1 ] ] );
        SrcX := ( Index div ZoneTile.FTileMaxColumnIndex ) * ZoneTile.FTileWidth;
        SrcY := ( Index mod ZoneTile.FTileMaxColumnIndex ) * ZoneTile.FTileHeight + HalfHeight;
        DstX := X;
        DstY := Y;
        W := FMap.FTileWidth;
        H := HalfHeight;
        if Assigned( ClipRect ) then
        begin
          Clip2( ClipRect.Left, ClipRect.Right, DstX, SrcX, W );
          Clip2( ClipRect.Top, ClipRect.Bottom, DstY, SrcY, H );
        end;
        if ( W > 0 ) and ( H > 0 ) then
        begin
          SelectObject( TempDC, ZoneTile.FTileMasks );
          BitBlt( Dest, DstX, DstY, W, H, TempDC, SrcX, SrcY, SRCAND );
          SelectObject( TempDC, ZoneTile.FTileImages );
          BitBlt( Dest, DstX, DstY, W, H, TempDC, SrcX, SrcY, SRCPAINT );
        end;
      end;

      if ( p^.Tile[ 2 ] <> $FFFF ) then
      begin
        ZoneTile := TZone( FMap.Zones.Items[ p^.Zone[ 2 ] ] );
        SrcX := ( p^.Tile[ 2 ] div ZoneTile.FTileMaxColumnIndex ) * ZoneTile.FTileWidth;
        SrcY := ( p^.Tile[ 2 ] mod ZoneTile.FTileMaxColumnIndex ) * ZoneTile.FTileHeight;
        DstX := X + HalfWidth;
        DstY := Y;
        W := HalfWidth;
        H := FMap.FTileHeight;
        if Assigned( ClipRect ) then
        begin
          Clip2( ClipRect.Left, ClipRect.Right, DstX, SrcX, W );
          Clip2( ClipRect.Top, ClipRect.Bottom, DstY, SrcY, H );
        end;
        if ( W > 0 ) and ( H > 0 ) then
        begin
          SelectObject( TempDC, ZoneTile.FTileMasks );
          BitBlt( Dest, DstX, DstY, W, H, TempDC, SrcX, SrcY, SRCAND );
          SelectObject( TempDC, ZoneTile.FTileImages );
          BitBlt( Dest, DstX, DstY, W, H, TempDC, SrcX, SrcY, SRCPAINT );
        end;
      end;

      if ( p^.Tile[ 3 ] <> $FFFF ) then
      begin
        ZoneTile := TZone( FMap.Zones.Items[ p^.Zone[ 3 ] ] );
        SrcX := ( p^.Tile[ 3 ] div ZoneTile.FTileMaxColumnIndex ) * ZoneTile.FTileWidth;
        SrcY := ( p^.Tile[ 3 ] mod ZoneTile.FTileMaxColumnIndex ) * ZoneTile.FTileHeight;
        DstX := X;
        DstY := Y + HalfHeight;
        W := FMap.FTileWidth;
        H := HalfHeight;
        if Assigned( ClipRect ) then
        begin
          Clip2( ClipRect.Left, ClipRect.Right, DstX, SrcX, W );
          Clip2( ClipRect.Top, ClipRect.Bottom, DstY, SrcY, H );
        end;
        if ( W > 0 ) and ( H > 0 ) then
        begin
          SelectObject( TempDC, ZoneTile.FTileMasks );
          BitBlt( Dest, DstX, DstY, W, H, TempDC, SrcX, SrcY, SRCAND );
          SelectObject( TempDC, ZoneTile.FTileImages );
          BitBlt( Dest, DstX, DstY, W, H, TempDC, SrcX, SrcY, SRCPAINT );
        end;
      end;

      if ( p^.Tile[ 4 ] <> $FFFF ) then
      begin
        ZoneTile := TZone( FMap.Zones.Items[ p^.Zone[ 4 ] ] );
        SrcX := ( p^.Tile[ 4 ] div ZoneTile.FTileMaxColumnIndex ) * ZoneTile.FTileWidth + HalfWidth;
        SrcY := ( p^.Tile[ 4 ] mod ZoneTile.FTileMaxColumnIndex ) * ZoneTile.FTileHeight;
        DstX := X;
        DstY := Y;
        W := HalfWidth;
        H := FMap.FTileHeight;
        if Assigned( ClipRect ) then
        begin
          Clip2( ClipRect.Left, ClipRect.Right, DstX, SrcX, W );
          Clip2( ClipRect.Top, ClipRect.Bottom, DstY, SrcY, H );
        end;
        if ( W > 0 ) and ( H > 0 ) then
        begin
          SelectObject( TempDC, ZoneTile.FTileMasks );
          BitBlt( Dest, DstX, DstY, W, H, TempDC, SrcX, SrcY, SRCAND );
          SelectObject( TempDC, ZoneTile.FTileImages );
          BitBlt( Dest, DstX, DstY, W, H, TempDC, SrcX, SrcY, SRCPAINT );
        end;
      end;
{$ENDIF}
{$IFDEF DirectX}
      HalfWidth := FMap.FTileWidth div 2;
      HalfHeight := FMap.FTileHeight div 2;

      if ( Index <> $FFFF ) then
      begin
        ZoneTile := TZone( FMap.Zones.Items[ p^.Zone[ 1 ] ] );
        SrcX := ( Index div ZoneTile.FTileMaxColumnIndex ) * FMap.FTileWidth;
        SrcY := ( Index mod ZoneTile.FTileMaxColumnIndex ) * FMap.FTileHeight + HalfHeight;
        SrcX2 := SrcX + FMap.FTileWidth;
        SrcY2 := SrcY + HalfHeight;
        DstX := X;
        DstY := Y;
        if Assigned( ClipRect ) then
        begin
          Clip1( ClipRect.Left, ClipRect.Right, DstX, SrcX, SrcX2 );
          Clip1( ClipRect.Top, ClipRect.Bottom, DstY, SrcY, SrcY2 );
        end;
        if ( SrcX2 > SrcX ) and ( SrcY2 > SrcY ) then
        begin
          Dest.BltFast( DstX, DstY, ZoneTile.FTileImages, Rect( SrcX, SrcY,
            SrcX2, SrcY2 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        end;
      end;

      if ( p^.Tile[ 2 ] <> $FFFF ) then
      begin
        ZoneTile := TZone( FMap.Zones.Items[ p^.Zone[ 2 ] ] );
        SrcX := ( p^.Tile[ 2 ] div ZoneTile.FTileMaxColumnIndex ) * FMap.FTileWidth;
        SrcY := ( p^.Tile[ 2 ] mod ZoneTile.FTileMaxColumnIndex ) * FMap.FTileHeight;
        SrcX2 := SrcX + HalfWidth;
        SrcY2 := SrcY + FMap.FTileHeight;
        DstX := X + HalfWidth;
        DstY := Y;
        if Assigned( ClipRect ) then
        begin
          Clip1( ClipRect.Left, ClipRect.Right, DstX, SrcX, SrcX2 );
          Clip1( ClipRect.Top, ClipRect.Bottom, DstY, SrcY, SrcY2 );
        end;
        if ( SrcX2 > SrcX ) and ( SrcY2 > SrcY ) then
        begin
          Dest.BltFast( DstX, DstY, ZoneTile.FTileImages, Rect( SrcX, SrcY,
            SrcX2, SrcY2 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        end;
      end;

      if ( p^.Tile[ 3 ] <> $FFFF ) then
      begin
        ZoneTile := TZone( FMap.Zones.Items[ p^.Zone[ 3 ] ] );
        SrcX := ( p^.Tile[ 3 ] div ZoneTile.FTileMaxColumnIndex ) * FMap.FTileWidth;
        SrcY := ( p^.Tile[ 3 ] mod ZoneTile.FTileMaxColumnIndex ) * FMap.FTileHeight;
        SrcX2 := SrcX + FMap.FTileWidth;
        SrcY2 := SrcY + HalfHeight;
        DstX := X;
        DstY := Y + HalfHeight;
        if Assigned( ClipRect ) then
        begin
          Clip1( ClipRect.Left, ClipRect.Right, DstX, SrcX, SrcX2 );
          Clip1( ClipRect.Top, ClipRect.Bottom, DstY, SrcY, SrcY2 );
        end;
        if ( SrcX2 > SrcX ) and ( SrcY2 > SrcY ) then
        begin
          Dest.BltFast( DstX, DstY, ZoneTile.FTileImages, Rect( SrcX, SrcY,
            SrcX2, SrcY2 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        end;
      end;

      if ( p^.Tile[ 4 ] <> $FFFF ) then
      begin
        ZoneTile := TZone( FMap.Zones.Items[ p^.Zone[ 4 ] ] );
        SrcX := ( p^.Tile[ 4 ] div ZoneTile.FTileMaxColumnIndex ) * FMap.FTileWidth + HalfWidth;
        SrcY := ( p^.Tile[ 4 ] mod ZoneTile.FTileMaxColumnIndex ) * FMap.FTileHeight;
        SrcX2 := SrcX + HalfWidth;
        SrcY2 := SrcY + FMap.FTileHeight;
        DstX := X;
        DstY := Y;
        if Assigned( ClipRect ) then
        begin
          Clip1( ClipRect.Left, ClipRect.Right, DstX, SrcX, SrcX2 );
          Clip1( ClipRect.Top, ClipRect.Bottom, DstY, SrcY, SrcY2 );
        end;
        if ( SrcX2 > SrcX ) and ( SrcY2 > SrcY ) then
        begin
          Dest.BltFast( DstX, DstY, ZoneTile.FTileImages, Rect( SrcX, SrcY,
            SrcX2, SrcY2 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        end;
      end;
{$ENDIF}
    end
    else
    begin
      if ( Index = $FFFF ) then
        Exit;
      ZoneTile := TZone( FMap.Zones.Items[ p^.Zone[ Layer ] ] );
{$IFDEF DirectX}
      if ZoneTile is TLightZone then
      begin
        if ZoneTile.FullRefresh then
        begin
          if Dest = lpDDSMap then
          begin
            if Layer = 1 then
              Exit;
            DstX := X + FMap.FTileWidth;
            DstY := Y + FMap.FTileHeight;
            if Assigned( ClipRect ) then
            begin
              if ( X < ClipRect.Left ) then
                X := ClipRect.Left;
              if ( DstX > ClipRect.Right ) then
                DstX := ClipRect.Right;
              if ( Y < ClipRect.Top ) then
                Y := ClipRect.Top;
              if ( DstY > ClipRect.Bottom ) then
                DstY := ClipRect.Bottom;
            end;
            BltFx.dwSize := SizeOf( BltFx );
            BltFx.dwFillColor := FMap.FColorMatch;
            Dest.Blt( Rect( X, Y, DstX, DstY ), nil, Rect( X, Y, DstX, DstY ), DDBLT_COLORFILL + DDBLT_WAIT, BltFx );
            Exit;
          end
          else
          begin
            Offset := ( TLightZone( ZoneTile ).States - TLightZone( ZoneTile ).State ) * TLightZone( ZoneTile ).TileStateOffset;
            Dec( Index, Offset );
          end;
        end;
      end;
{$ENDIF}
      SrcX := ( Index div ZoneTile.FTileMaxColumnIndex ) * FMap.FTileWidth;
      SrcY := ( Index mod ZoneTile.FTileMaxColumnIndex ) * FMap.FTileHeight;
      if ( Layer = 0 ) then
      begin
{$IFDEF DirectX}
        SrcX2 := SrcX + FMap.FTileWidth;
        SrcY2 := SrcY + FMap.FTileHeight;
        DstX := X;
        DstY := Y;
        if Assigned( ClipRect ) then
        begin
          Clip1( ClipRect.Left, ClipRect.Right, DstX, SrcX, SrcX2 );
          Clip1( ClipRect.Top, ClipRect.Bottom, DstY, SrcY, SrcY2 );
          if ( SrcX2 <= SrcX ) or ( SrcY2 <= SrcY ) then
            Exit;
        end;
        Dest.BltFast( DstX, DstY, ZoneTile.FTileImages, Rect( SrcX, SrcY,
          SrcX2, SrcY2 ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
{$ENDIF}
{$IFNDEF DirectX}
        DstX := X;
        DstY := Y;
        W := FMap.FTileWidth;
        H := FMap.FTileHeight;
        if Assigned( ClipRect ) then
        begin
          Clip2( ClipRect.Left, ClipRect.Right, DstX, SrcX, W );
          Clip2( ClipRect.Top, ClipRect.Bottom, DstY, SrcY, H );
          if ( W <= 0 ) or ( H <= 0 ) then
            Exit;
        end;
        SelectObject( TempDC, ZoneTile.FTileImages );
        BitBlt( Dest, DstX, DstY, W, H, TempDC, SrcX, SrcY, SRCCOPY );
{$ENDIF}
      end
      else
      begin
{$IFDEF DirectX}
        SrcX2 := SrcX + FMap.FTileWidth;
        SrcY2 := SrcY + FMap.FTileHeight;
        DstX := X;
        DstY := Y;
        if Assigned( ClipRect ) then
        begin
          Clip1( ClipRect.Left, ClipRect.Right, DstX, SrcX, SrcX2 );
          Clip1( ClipRect.Top, ClipRect.Bottom, DstY, SrcY, SrcY2 );
          if ( SrcX2 <= SrcX ) or ( SrcY2 <= SrcY ) then
            Exit;
        end;
        Dest.BltFast( DstX, DstY, ZoneTile.FTileImages, Rect( SrcX, SrcY,
          SrcX2, SrcY2 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
{$ENDIF}
{$IFNDEF DirectX}
        DstX := X;
        DstY := Y;
        W := FMap.FTileWidth;
        H := FMap.FTileHeight;
        if Assigned( ClipRect ) then
        begin
          Clip2( ClipRect.Left, ClipRect.Right, DstX, SrcX, W );
          Clip2( ClipRect.Top, ClipRect.Bottom, DstY, SrcY, H );
          if ( W <= 0 ) or ( H <= 0 ) then
            Exit;
        end;
        SelectObject( TempDC, ZoneTile.FTileMasks );
        BitBlt( Dest, DstX, DstY, W, H, TempDC, SrcX, SrcY, SRCAND );
        SelectObject( TempDC, ZoneTile.FTileImages );
        BitBlt( Dest, DstX, DstY, W, H, TempDC, SrcX, SrcY, SRCPAINT );
{$ENDIF}
      end;
    end;

{$IFDEF DEBUG}
    if Dest = lpDDSMap then
    begin
      Dest.GetDC( DC );
      try
        if ( p^.CollisionMask <> 0 ) then
        begin
          Mask := p^.LineOfSightMask;
          for k := 0 to 3 do
          begin
            if ( ( Mask and 1 ) = 1 ) then
              PatBlt( DC, X + k * 16, Y + 24, 16, 8, WHITENESS );
            Mask := Mask shr 1;
          end;
          for k := 0 to 3 do
          begin
            if ( ( Mask and 1 ) = 1 ) then
              PatBlt( DC, X + k * 16, Y + 16, 16, 8, WHITENESS );
            Mask := Mask shr 1;
          end;
          for k := 0 to 3 do
          begin
            if ( ( Mask and 1 ) = 1 ) then
              PatBlt( DC, X + k * 16, Y + 8, 16, 8, WHITENESS );
            Mask := Mask shr 1;
          end;
          for k := 0 to 3 do
          begin
            if ( ( Mask and 1 ) = 1 ) then
              PatBlt( DC, X + k * 16, Y, 16, 8, WHITENESS );
            Mask := Mask shr 1;
          end;
        end;
      finally
        Dest.ReleaseDC( DC );
      end;
    end;
{$ENDIF}


{$IFDEF DEBUG}
    if Dest = MapBuffer.Canvas.Handle then
    begin
      MapBuffer.Canvas.Brush.Style := bsClear;
      Rectangle( Dest, X, Y, X + FMap.FTileWidth + 1, Y + FMap.FTileHeight + 1 );
      MapBuffer.Canvas.Brush.Style := bsSolid;
      if ( p^.CollisionMask <> 0 ) then
      begin
        Mask := p^.LineOfSightMask;
        for k := 0 to 3 do
        begin
          if ( ( Mask and 1 ) = 1 ) then
            PatBlt( Dest, X + k * 16, Y + 24, 16, 8, WHITENESS );
          Mask := Mask shr 1;
        end;
        for k := 0 to 3 do
        begin
          if ( ( Mask and 1 ) = 1 ) then
            PatBlt( Dest, X + k * 16, Y + 16, 16, 8, WHITENESS );
          Mask := Mask shr 1;
        end;
        for k := 0 to 3 do
        begin
          if ( ( Mask and 1 ) = 1 ) then
            PatBlt( Dest, X + k * 16, Y + 8, 16, 8, WHITENESS );
          Mask := Mask shr 1;
        end;
        for k := 0 to 3 do
        begin
          if ( ( Mask and 1 ) = 1 ) then
            PatBlt( Dest, X + k * 16, Y, 16, 8, WHITENESS );
          Mask := Mask shr 1;
        end;
      end;
    end;
{$ENDIF}
  end;

  procedure TAniView.CenterView( X, Y : Longint );
  begin
    KeyFigure := nil;
    CenterX := X;
    CenterY := Y;
  end;

  procedure TAniView.MouseDown( Button : TMouseButton; Shift : TShiftState; X, Y : Integer );
  begin
    FLMousebutton := Button = mbLeft;
    if Assigned( FOnMouseDown ) then
    begin
      inherited MouseDown( Button, Shift, X, Y );
      OnMouseDown( Self, Button, Shift, X, Y, X + OffsetX, Y + OffsetY );
    end;
  end;

  procedure TAniView.MouseUp( Button : TMouseButton; Shift : TShiftState; X, Y : Integer );
  begin
    FLMousebutton := False;
    if Assigned( FOnMouseUp ) then
    begin
      inherited MouseUp( Button, Shift, X, Y );
      OnMouseUp( Self, Button, Shift, X, Y, X + OffsetX, Y + OffsetY );
    end;
  end;

  procedure TAniView.MouseMove( Shift : TShiftState; X, Y : Integer );
  begin
    if Assigned( FOnMouseMove ) then
    begin
      inherited MouseMove( Shift, X, Y );
      OnMouseMove( Self, Shift, X, Y, X + OffsetX, Y + OffsetY );
    end;
  end;

  procedure TAniMap.Sort;
  var
    i, j, Y : integer;
    MinY, MaxY : longint;
    pBase, p : ^longint;
    MemSize : longint;
  begin
  //Sort ItemList
    if LastItem > 0 then
    begin
      MinY := FItemList[ 1 ].Y;
      MaxY := MinY;
      for i := 2 to LastItem do
      begin
        if FItemList[ i ].Y < MinY then
          MinY := FItemList[ i ].Y
        else if FItemList[ i ].Y > MaxY then
          MaxY := FItemList[ i ].Y;
      end;
      MemSize := ( MaxY - MinY + 1 ) * sizeof( longint );
      GetMem( pBase, MemSize );
      try
        ZeroMemory( pBase, MemSize );
        for i := LastItem downto 1 do
        begin
          Y := FItemList[ i ].Y - MinY;
          p := pBase;
          inc( p, Y );
          FItemList[ i ].Next := p^;
          p^ := i;
        end;

        p := pBase;
        FirstItem := p^; //we know the first one is valid
        j := FirstItem;
        while FItemList[ j ].Next <> 0 do
        begin
          j := FItemList[ j ].Next;
        end;
        for i := 1 to ( MaxY - MinY ) do
        begin
          inc( p );
          if p^ <> 0 then
          begin
            FItemList[ j ].Next := p^;
            j := p^;
            while FItemList[ j ].Next <> 0 do
            begin
              j := FItemList[ j ].Next;
            end;
          end;
        end;
      finally
        FreeMem( pBase );
      end;
    end;
  end;

  procedure TAniView.BuildRowUpdateInfo;
  var
    RowBase, RowData : ^RowUpdateInfo;
    i, j : Longint;
    Y, ItemY : Longint;
    MaxHeight : Integer;
    ItemIndex : Word;
    MaxRow : Longint;
  begin
    RowBase := GlobalLock( MapRows );
    RowData := RowBase;
    Y := 0;
    MaxHeight := 0;
    i := FMap.FirstItem;
    while ( i > 0 ) do
    begin
      ItemY := FMap.FItemList[ i ].Y;
      if ItemY < 0 then
      begin
      end
      else if ( ItemY = Y ) then
      begin
        if ( FMap.FItemList[ i ].VHeight > MaxHeight ) then
          MaxHeight := FMap.FItemList[ i ].VHeight;
      end
      else
      begin
        RowData^.MaxHeight := MaxHeight;
        if ( ItemY >= PixelHeight ) then
        begin
          RowData := RowBase;
          Inc( RowData, PixelHeight );
          MaxHeight := FMap.FItemList[ i ].VHeight - ItemY + PixelHeight - 1;
        end
        else
        begin
          Inc( RowData, ItemY - Y );
          MaxHeight := FMap.FItemList[ i ].VHeight;
        end;
        Y := ItemY;
        RowData^.ItemIndex := i;
      end;
      i := FMap.FItemList[ i ].Next;
    end;
    RowData^.MaxHeight := MaxHeight;

    MaxRow := PixelHeight + FMap.FTileHeight;
    RowData := RowBase;
    for i := 0 to MaxRow - 1 do
    begin
      RowData^.DescendRow := i;
      Inc( RowData );
    end;

    i := FMap.FirstItem;
    while ( i > 0 ) do
    begin
      Y := FMap.FItemList[ i ].Y - FMap.FItemList[ i ].VHeight + FMap.FItemList[ i ].Height;
      if ( Y >= MaxRow ) then
        Y := MaxRow - 1;
      if ( Y >= 0 ) then
      begin
        j := FMap.FItemList[ i ].Y + 1;
        if j < 0 then
          j := 0;
        RowData := RowBase;
        Inc( RowData, j );
        for j := j to Y do
        begin
          if ( FMap.FItemList[ i ].Y < RowData^.DescendRow ) then
          begin
            if FMap.FItemList[ i ].Y < 0 then
              RowData^.DescendRow := 0
            else
              RowData^.DescendRow := FMap.FItemList[ i ].Y;
          end;
          Inc( RowData );
        end;
      end;
      i := FMap.FItemList[ i ].Next;
    end;

    i := PixelHeight;
    j := i;
    Inc( RowBase, i );
    RowData := RowBase;
    ItemIndex := RowData^.ItemIndex;
    Y := j - RowData^.MaxHeight + 1;
    while ( i >= 0 ) do
    begin
      if ( RowBase^.ItemIndex = 0 ) then
        RowBase^.ItemIndex := ItemIndex
      else
        ItemIndex := RowBase^.ItemIndex;
      while ( i < Y ) and ( J > 1 ) do
      begin
        Dec( RowData );
        Dec( j );
        Y := j - RowData^.MaxHeight + 1;
      end;
      RowBase^.OverlapRow := j;
      Dec( i );
      Dec( RowBase );
    end;
    GlobalUnlock( MapRows );
  end;

  function TAniView.CanMove( SrcX, SrcY, DestX, DestY : Smallint ) : Boolean;
  var
    X, Y : Longint;
    DestX1, DestY1 : Longint;
    R, R2, D2 : Longint;
    X1, Y1, X2, Y2 : Longint;
    XL : Longint;
    GridBase, GridLoc : ^GridInfo;
    CollisionMask : Word;
    cx, cy : Longint;
    CellWidth, CellHeight : Integer;
    i, j : Integer;
    Dx, dy : Integer;
    ScanY : Longint;
    A, B : Double;
  begin
    CellWidth := FMap.FStripWidth shr 1;
    CellHeight := FMap.FStripHeight shr 1;
    DestX1 := FStartX + DestX * CellWidth;
    DestY1 := FStartY + DestY * CellHeight;

    if Assigned( FAstarAvoidFigure ) then
    begin
      for i := 0 to FAstarAvoidFigure.Count - 1 do
      begin
        A := sqr( TAniFigure( FAstarAvoidFigure.Items[ i ] ).Radius + FAStarFigure.Radius ) + sqr( FMap.FStripWidth ); //StripWidth is the fudge factor
        B := sqr( TAniFigure( FAstarAvoidFigure.Items[ i ] ).StepX - DestX1 ) + sqr( 2 * ( TAniFigure( FAstarAvoidFigure.Items[ i ] ).StepY - DestY1 ) );
      //        if (B<A) then begin
      //          A:=sqr(StepX-(FStartX+SrcX*CellWidth)+CellWidth)+sqr(2*(StepY-(FStartY+SrcY*CellHeight))+CellHeight);
        if ( B < A ) then
        begin //This will allow movement if the figure is moving away from the target
          Result := False; //even though it is still inside the radius.
          Exit;
        end;
      //        end;
      end;
    end;

    R := FAStarFigure.Radius + CellWidth + 2; //2 added for round off error
    R2 := R * R;
    X1 := DestX1 - R; //if (X1<0) then X1:=0;
    X2 := DestX1 + R; //if (X2>=FMap.FBitWidth) then X2:=FMap.FBitWidth-1;
    Y1 := DestY1 - R; //if (Y1<0) then Y1:=0;
    Y2 := DestY1 + R; //if (Y2>=FMap.FBitheight) then Y2:=FMap.FBitHeight-1;
    if ( X1 < FMap.FTileWidth - CellWidth ) or ( X2 >= FMap.FBitWidth + FMap.FTileWidth + CellWidth ) or
      ( Y1 < FMap.FTileHeight - CellWidth ) or ( Y2 >= FMap.FBitheight + FMap.FTileHeight + CellWidth ) then
    begin
      Result := False;
      Exit;
    end;

    GridBase := GlobalLock( FMap.FMapData );

    XL := X1 div FMap.FTileWidth;

    for Y := Y1 div FMap.FTileHeight to Y2 div FMap.FTileHeight do
    begin
      if ( Y >= 0 ) and ( Y < FMap.FHeight ) then
      begin
        ScanY := ( Y + 1 ) * FMap.FTileHeight - CellHeight;
        GridLoc := GridBase;
        Inc( GridLoc, Y * FMap.FWidth + XL );
        for X := XL to X2 div FMap.FTileWidth do
        begin
          if ( X >= 0 ) and ( X < FMap.FWidth ) then
          begin
            if FAStarFigure.UseLineOfSight then
              CollisionMask := GridLoc^.LineOfSightMask
            else
              CollisionMask := GridLoc^.CollisionMask;
            if ( CollisionMask <> 0 ) then
            begin
              for j := 0 to 3 do
              begin
                if ( ( CollisionMask and $F ) <> 0 ) then
                begin
                  cy := ScanY - j * FMap.FStripHeight;
                  for i := 0 to 3 do
                  begin
                    if ( ( CollisionMask and 1 ) = 1 ) then
                    begin
                      cx := X * FMap.FTileWidth + i * FMap.FStripWidth + CellWidth;
                      Dx := ( DestX1 - cx );
                      dy := 2 * ( DestY1 - cy );
                      D2 := Dx * Dx + dy * dy;
                      if ( D2 < R2 ) then
                      begin
                        Result := False;
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
          Inc( GridLoc );
        end;
      end;
    end;
    GlobalUnlock( FMap.FMapData );
    Result := True;
  end;

  function TAniView.FindPath( Figure : TAniFigure; X2, Y2, Deviance : Longint; var Path : HGLOBAL ) : integer;
  var
    p : ^TPoint;
    dx, dy : Longint;
    CellWidth, CellHeight : Integer;
    i : integer;
  begin
    result := 0;
    CellWidth := FMap.FStripWidth shr 1;
    CellHeight := FMap.FStripHeight shr 1;
    Dx := Round( 2 * ( X2 - Figure.StepX ) / FMap.FStripWidth );
    dy := Round( 2 * ( Y2 - Figure.StepY ) / FMap.FStripHeight );
    if ( dx = 0 ) and ( dy = 0 ) then
      Exit;
    if ( Dx <= MaxSearch ) and ( Dx >= MinSearch ) and ( dy <= MaxSearch ) and ( dy >= MinSearch ) then
    begin
      FAStarFigure := Figure;
      FAStarAvoidFigure := nil;
      FStartX := CellWidth * Trunc( 2 * Figure.StepX / FMap.FStripWidth ) + ( FMap.FStripWidth shr 2 );
      FStartY := CellHeight * Trunc( 2 * Figure.StepY / FMap.FStripHeight ) + ( FMap.FStripHeight shr 2 );
      FAStar.Deviance := Deviance;
      result := FAStar.FindJaggedPath( 0, 0, dx, dy, Path );
      if result > 0 then
      begin
        p := GlobalLock( Path );
        for i := 1 to result do
        begin
          p^.X := p^.X * CellWidth + FStartX;
          p^.Y := p^.Y * CellHeight + FStartY;
          Inc( p );
        end;
        GlobalUnlock( Path );
      end;
    end;
  end;

  procedure TAniView.GetPath( Figure : TAniFigure );
  var
    Dx, dy : Longint;
    D : Double;
    pBase, p : ^TPoint;
    CellWidth, CellHeight : Integer;
    i : Integer;
  begin
    Figure.NeedPath := False;
    CellWidth := FMap.FStripWidth shr 1;
    CellHeight := FMap.FStripHeight shr 1;
    Dx := Round( 2 * ( Figure.FPathDestX - Figure.StepX ) / FMap.FStripWidth );
    dy := Round( 2 * ( Figure.FPathDestY - Figure.StepY ) / FMap.FStripHeight );
    if ( Dx = 0 ) and ( dy = 0 ) then
    begin
      Figure.GotPath := False;
      if Assigned( Figure.OnNoPath ) then
        Figure.OnNoPath( Figure );
      Exit;
    end;
    if ( Dx <= MaxSearch ) and ( Dx >= MinSearch ) and ( dy <= MaxSearch ) and ( dy >= MinSearch ) then
    begin
      FAStarFigure := Figure;
      FAStarAvoidFigure := Figure.AvoidInPath;
      FStartX := CellWidth * Trunc( 2 * Figure.StepX / FMap.FStripWidth ) + ( FMap.FStripWidth shr 2 );
      FStartY := CellHeight * Trunc( 2 * Figure.StepY / FMap.FStripHeight ) + ( FMap.FStripHeight shr 2 );

      FAStar.Deviance := Figure.PathDeviance;
      if ( not Assigned( FAstarAvoidFigure ) ) or ( FAstarAvoidFigure.Count = 0 ) then
      begin
        if not CanMove( dx, dy, dx, dy ) then
        begin
          FAStar.Deviance := 16;
        end;
      end;

      Figure.PathCount := FAStar.FindPath( 0, 0, Dx, dy, Figure.PathHandle );
      if ( Figure.PathCount > 0 ) and ( Figure.PathHandle <> 0 ) then
      begin
        pBase := GlobalLock( Figure.PathHandle );
        p := pBase;
{$IFDEF DEBUG}
        RefreshMap;
        MapBuffer.Canvas.Brush.Color := $FF00;
        MapBuffer.Canvas.Ellipse( Figure.FX - OffsetX + MapOffsetX - Figure.Radius,
          Figure.FY - OffsetY + MapOffsetY - Figure.Radius div 2,
          Figure.FX - OffsetX + MapOffsetX + Figure.Radius,
          Figure.FY - OffsetY + MapOffsetY + Figure.Radius div 2 );
        MapBuffer.Canvas.Brush.Color := 255;
{$ENDIF}
        for i := 1 to Figure.PathCount do
        begin
          p^.X := p^.X * CellWidth + FStartX;
          p^.Y := p^.Y * CellHeight + FStartY;
{$IFDEF DEBUG}
          MapBuffer.Canvas.Ellipse( p^.X - OffsetX + MapOffsetX - Figure.Radius,
            p^.Y - OffsetY + MapOffsetY - Figure.Radius div 2,
            p^.X - OffsetX + MapOffsetX + Figure.Radius,
            p^.Y - OffsetY + MapOffsetY + Figure.Radius div 2 );
{$ENDIF}
          Inc( p );
        end;
{$IFNDEF DirectX}
        MapBuffer.Canvas.Brush.Color := $FFFFFF;
{$ENDIF}
        Figure.FDestX := pBase^.X;
        Figure.FDestY := pBase^.Y;
        Figure.FDestZ := Figure.FZ;
        Figure.GotPath := True;
        Figure.Terminal := True;
        Figure.Moving := True;
        Figure.PathStep := 1;
        Dx := Figure.DestX - Figure.FX;
        dy := 2 * ( Figure.DestY - Figure.FY );
        D := sqrt( sqr( Dx ) + sqr( dy ) );
        if D <> 0 then
        begin
          Figure.FSlopeX := Dx / D;
          Figure.FSlopeY := dy / ( 2 * D );
        end;
        GlobalUnlock( Figure.PathHandle );
        if Assigned( Figure.OnPathStep ) then
          Figure.OnPathStep( Figure, Figure.DestX, Figure.DestY );
      end
      else
      begin
        Figure.GotPath := False;
        if Assigned( Figure.OnNoPath ) then
          Figure.OnNoPath( Figure );
      end;
    end
    else
    begin
      Figure.GotPath := False;
      if Assigned( Figure.OnNoPath ) then
        Figure.OnNoPath( Figure );
    end;
  end;

  procedure TAniView.MoveFigure( Figure : TAniFigure );
  const
    Tol = 1 / 256;
  var
    Dx, dy, dZ, D : Double;
    DestX, DestY, DestZ : Double;
    Gx, Gy : Integer;
    Gx2, Gy2 : Integer;
    R, G : Double;
    p : array[ 0..3 ] of TPoint;
  {$IFDEF DEBUG}
    p1 : array[ 0..3 ] of TPoint;
  {$ENDIF}
    Top, Left, Right, Bottom, Temp : Integer;
    XL1, YL1, XL2, YL2 : Longint;
    XR1, YR1, XR2, YR2 : Longint;
    X, Y, XL, XR, ScanY, TileRow, TileRowOffset : Longint;
    dXL, dYL, dXR, dYR : Longint;
    ModeL, ModeR : Boolean;
    GridBase, GridLoc : ^GridInfo;
    CollisionMask : Word;
    A, B, C, Q, T, T1, T2 : Double;
    cx, cy : Integer;
    i, j : Integer;
    edge : Longint;
    Stop : Boolean;
    InitSeq : Boolean;
    OldTriggerID : SmallInt;
    OldFilterID : SmallInt;
    Point : ^TPoint;
    S : Single;
    StepX, StepY : Double;
    NextFigure, TempFigure : TAniFigure;
    RemX, RemY : integer;
    BitMask : word;
  begin
    Top := 0;
    Bottom := 0;
    Dx := Figure.DestX - Figure.StepX;
    dy := 2 * ( Figure.DestY - Figure.StepY );
    dZ := Figure.DestZ - Figure.StepZ;
    D := sqrt( sqr( Dx ) + sqr( dy ) + sqr( dZ ) );
    if ( ( D <= 0 ) or ( D <= Figure.Speed ) ) and Figure.Terminal then
    begin
      if ( Figure.GotPath ) then
      begin
        DestZ := Figure.StepZ;
        if ( Figure.PathStep >= Figure.PathCount ) then
        begin
          DestX := Figure.DestX;
          DestY := Figure.DestY;
          Figure.Moving := False;
          if Assigned( Figure.OnStop ) then
            Figure.OnStop( Figure );
        end
        else
        begin
          Point := GlobalLock( Figure.PathHandle );
          Inc( Point, Figure.PathStep );
          S := Figure.Speed;
          repeat
            if ( Figure.PathStep >= Figure.PathCount ) then
              Break;
            StepX := Figure.DestX;
            StepY := Figure.DestY;
            S := S - D;
            Figure.FDestX := Point^.X;
            Figure.FDestY := Point^.Y;
            Dx := Figure.DestX - StepX;
            dy := 2 * ( Figure.DestY - StepY );
            D := sqrt( sqr( Dx ) + sqr( dy ) );
            Inc( Figure.PathStep );
            Inc( Point );
          until ( S <= D );
          GlobalUnlock( Figure.PathHandle );
          if ( S <= D ) then
          begin
            Figure.FSlopeX := Dx / D;
            Figure.FSlopeY := dy / ( 2 * D );
            DestX := StepX + S * Figure.SlopeX;
            DestY := StepY + S * Figure.SlopeY;
            DestZ := Figure.DestZ;
            if Assigned( Figure.OnPathStep ) then
              Figure.OnPathStep( Figure, Figure.DestX, Figure.DestY );
          end
          else
          begin
            DestX := Figure.DestX;
            DestY := Figure.DestY;
            Figure.Moving := False;
            if Assigned( Figure.OnStop ) then
              Figure.OnStop( Figure );
          end;
        end;
      end
      else
      begin
        DestX := Figure.DestX;
        DestY := Figure.DestY;
        DestZ := Figure.DestZ;
        Figure.Moving := False;
        if Assigned( Figure.OnStop ) then
          Figure.OnStop( Figure );
      end;
    end
    else
    begin
      DestX := Figure.StepX + Figure.Speed * Figure.SlopeX;
      DestY := Figure.StepY + Figure.Speed * Figure.SlopeY;
      DestZ := Figure.StepZ + Figure.Speed * Figure.SlopeZ;
    end;

    if Assigned( FMap ) then
    begin
    //Collision detection
      GridBase := GlobalLock( FMap.FMapData );
      GridLoc := GridBase;
      if ( Figure.MapOffset >= 0 ) then
      begin
      //Remove figure from collision chain
        Inc( GridLoc, ( Figure.MapOffset ) );
        NextFigure := GridLoc^.Figure;

        RemX := Figure.FPrevX mod FMap.FTileWidth;
        RemY := FMap.FTileHeight - ( Figure.FPrevY mod FMap.FTileHeight ) - 1;
        BitMask := 1 shl ( ( RemY div FMap.FStripHeight ) * 4 + ( RemX div FMap.FStripWidth ) );
        if ( GridLoc^.TriggerMask and BitMask ) > 0 then
          OldTriggerID := GridLoc^.TriggerID
        else
          OldTriggerID := 0;
        if ( GridLoc^.FilterMask and BitMask ) > 0 then
          OldFilterID := GridLoc^.FilterID
        else
          OldFilterID := 0;

        if ( NextFigure = Figure ) then
        begin
          GridLoc^.Figure := Figure.NextInTile;
        end
        else
        begin
          TempFigure := NextFigure;
          while Assigned( NextFigure ) do
          begin
            NextFigure := NextFigure.NextInTile;
            if ( NextFigure = Figure ) then
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
      dy := 2 * ( DestY - Figure.StepY );
      dZ := DestZ - Figure.StepZ;
      if ( Dx <> 0 ) or ( dy <> 0 ) then
      begin
        D := sqrt( sqr( Dx ) + sqr( dy ) );
        T := 1;
      //Collision with map boundaries
        if ( Dx <> 0 ) then
        begin
          edge := Figure.Radius + FMap.FTileWidth;
          if ( DestX < edge ) then
          begin
            T1 := ( edge - Figure.StepX ) / Dx;
            if ( T1 < T ) then
              T := T1;
          end
          else
          begin
            edge := FMap.FBitWidth - Figure.Radius - FMap.FTileWidth;
            if ( DestX > edge ) then
            begin
              T1 := ( edge - Figure.StepX ) / Dx;
              if ( T1 < T ) then
                T := T1;
            end;
          end;
        end;
        if ( dy <> 0 ) then
        begin
          edge := Figure.Radius + FMap.FTileHeight;
          if ( DestY < edge ) then
          begin
            T1 := ( edge - Figure.StepY ) / dy;
            if ( T1 < T ) then
              T := T1;
          end
          else
          begin
            edge := FMap.FBitHeight - Figure.Radius - FMap.FTileHeight;
            if ( DestY > edge ) then
            begin
              T1 := ( edge - Figure.StepY ) / dy;
              if ( T1 < T ) then
                T := T1;
            end;
          end;
        end;
        if ( T < 1 ) then
        begin
          if Assigned( Figure.OnCollideBoundary ) then
            Figure.OnCollideBoundary( Figure );
        end;

        if Assigned( Figure.OnCollideFigure ) then
        begin
        //Collisions with other figures
          InitSeq := False;
          R := Figure.Radius + FMaxCollisionRadius;
          G := R / D;
          Gx := Round( G * Dx );
          Gy := Round( G * dy );
          Gx2 := Gx div 2;
          Gy2 := Gy div 2;

          X := Round( Figure.StepX );
          Y := Round( Figure.StepY );
          p[ 0 ].X := X - Gy;
          p[ 0 ].Y := Y + Gx2;
          p[ 1 ].X := X + Gy;
          p[ 1 ].Y := Y - Gx2;
          X := Round( DestX );
          Y := Round( DestY );
          p[ 2 ].X := X + Gy + Gx;
          p[ 2 ].Y := Y - Gx2 + Gy2;
          p[ 3 ].X := X - Gy + Gx;
          p[ 3 ].Y := Y + Gx2 + Gy2;

        {p1[0].X:=p[0].X-OffsetX;
        p1[0].Y:=p[0].Y-OffsetY;
        p1[1].X:=p[1].X-OffsetX;
        p1[1].Y:=p[1].Y-OffsetY;
        p1[2].X:=p[2].X-OffsetX;
        p1[2].Y:=p[2].Y-OffsetY;
        p1[3].X:=p[3].X-OffsetX;
        p1[3].Y:=p[3].Y-OffsetY;
        FrameBuffer.canvas.Pen.color:=clBlack;
        Polygon(FrameBuffer.canvas.handle,p1[0],4);}

          if ( p[ 0 ].Y < p[ 1 ].Y ) then
            Top := 0
          else
            Top := 1;
          if ( p[ 2 ].Y < p[ Top ].Y ) then
            Top := 2;
          if ( p[ 3 ].Y < p[ Top ].Y ) then
            Top := 3;
          Bottom := ( Top + 2 ) mod 4;
          Left := ( Top + 1 ) mod 4;
          Right := ( Top + 3 ) mod 4;
          if ( p[ Left ].X > p[ Right ].X ) then
          begin
            Temp := Left;
            Left := Right;
            Right := Temp;
          end;
        //Roundoff error
          Dec( p[ Top ].Y );
          Dec( p[ Left ].X );
          Inc( p[ Bottom ].Y );
          Inc( p[ Right ].X );

          if ( p[ Top ].Y < 0 ) then
            TileRow := 0
          else
            TileRow := p[ Top ].Y div FMap.FTileHeight;
          ScanY := ( TileRow + 1 ) * FMap.FTileHeight;
          TileRowOffset := TileRow * Fmap.FWidth;

          XL1 := p[ Top ].X;
          YL1 := p[ Top ].Y;
          XL2 := p[ Left ].X;
          YL2 := p[ Left ].Y;
          XR1 := XL1;
          YR1 := YL1;
          XR2 := p[ Right ].X;
          YR2 := p[ Right ].Y;
          dXL := XL2 - XL1;
          dYL := YL2 - YL1;
          dXR := XR2 - XR1;
          dYR := YR2 - YR1;
          ModeL := True;
          ModeR := True;
          while ( ScanY < FMap.FBitHeight ) do
          begin
            if ( ModeL ) then
            begin
              if ( ScanY > p[ Left ].Y ) then
              begin
                XL1 := p[ Bottom ].X;
                YL1 := p[ Bottom ].Y;
                dXL := XL2 - XL1;
                dYL := YL2 - YL1;
                ModeL := False;
                XL := XL2 div FMap.FTileWidth;
              end
              else
                XL := ( XL1 + dXL * ( ScanY - YL1 ) div dYL ) div FMap.FTileWidth;
            end
            else
              XL := ( XL1 + dXL * ( ScanY - FMap.FTileHeight - YL1 ) div dYL ) div FMap.FTileWidth;

            if ( ModeR ) then
            begin
              if ( ScanY > p[ Right ].Y ) then
              begin
                XR1 := p[ Bottom ].X;
                YR1 := p[ Bottom ].Y;
                dXR := XR2 - XR1;
                dYR := YR2 - YR1;
                ModeR := False;
                XR := XR2 div FMap.FTileWidth;
              end
              else
                XR := ( XR1 + dXR * ( ScanY - YR1 ) div dYR ) div FMap.FTileWidth;
            end
            else
              XR := ( XR1 + dXR * ( ScanY - FMap.FTileHeight - YR1 ) div dYR ) div FMap.FTileWidth;

            if ( XL < 0 ) then
              XL := 0;
            if ( XR >= FMap.FWidth ) then
              XR := FMap.FWidth - 1;
            GridLoc := GridBase;
            Inc( GridLoc, TileRowOffset );
            Inc( GridLoc, XL );
            for X := XL to XR do
            begin
            //(*whew*) We've established there could be a collision, now let's find out
            //if there really is, and if so, where?
              NextFigure := GridLoc^.Figure;
              while Assigned( NextFigure ) do
              begin
                if NextFigure.Radius > 0 then
                begin
                  A := sqr( Dx ) + sqr( dy );
                  B := 2 * ( Dx * ( Figure.StepX - NextFigure.StepX ) + 2 * dy * ( Figure.StepY - NextFigure.StepY ) );
                  C := sqr( Figure.StepX - NextFigure.StepX ) + 4 * sqr( Figure.StepY - NextFigure.StepY ) -
                    sqr( Figure.Radius + NextFigure.Radius );
                  Q := sqr( B ) - 4 * A * C;
                  if ( Q >= 0 ) then
                  begin
                    T1 := ( -B - Sqrt( Q ) ) / ( 2 * A );
                    if ( T1 < 1 ) and ( T1 >= -Tol ) then
                    begin
                      Stop := False;
                      Figure.OnCollideFigure( Figure, NextFigure, Stop );
                      if ( Stop ) then
                        if ( T1 < T ) then
                          T := T1;
                    end;
                  end;
                end;
                NextFigure := NextFigure.NextInTile;
              end;
              Inc( GridLoc );
            end;
            if ( ScanY >= p[ Bottom ].Y ) then
              Break;
            Inc( TileRowOffset, FMap.FWidth );
            Inc( ScanY, FMap.FTileHeight );
          end;
        end
        else
          InitSeq := True;

      //Collisions with map objects
        if Assigned( Figure.OnCollideItem ) then
        begin
          T2 := T;
          R := Figure.Radius;
          G := R / D;
          Gx := Round( G * Dx );
          Gy := Round( G * dy );
          Gx2 := Gx div 2;
          Gy2 := Gy div 2;

          X := Round( Figure.StepX );
          Y := Round( Figure.StepY );
          p[ 0 ].X := X - Gy;
          p[ 0 ].Y := Y + Gx2;
          p[ 1 ].X := X + Gy;
          p[ 1 ].Y := Y - Gx2;
          X := Round( DestX );
          Y := Round( DestY );
          p[ 2 ].X := X + Gy + Gx;
          p[ 2 ].Y := Y - Gx2 + Gy2;
          p[ 3 ].X := X - Gy + Gx;
          p[ 3 ].Y := Y + Gx2 + Gy2;

        {p1[0].X:=p[0].X-OffsetX;
        p1[0].Y:=p[0].Y-OffsetY;
        p1[1].X:=p[1].X-OffsetX;
        p1[1].Y:=p[1].Y-OffsetY;
        p1[2].X:=p[2].X-OffsetX;
        p1[2].Y:=p[2].Y-OffsetY;
        p1[3].X:=p[3].X-OffsetX;
        p1[3].Y:=p[3].Y-OffsetY;
        FrameBuffer.canvas.Pen.color:=clBlack;
        Polygon(FrameBuffer.canvas.handle,p1[0],4);}

          if ( InitSeq ) then
          begin
            if ( p[ 0 ].Y < p[ 1 ].Y ) then
              Top := 0
            else
              Top := 1;
            if ( p[ 2 ].Y < p[ Top ].Y ) then
              Top := 2;
            if ( p[ 3 ].Y < p[ Top ].Y ) then
              Top := 3;
            Bottom := ( Top + 2 ) mod 4;
            Left := ( Top + 1 ) mod 4;
            Right := ( Top + 3 ) mod 4;
            if ( p[ Left ].X > p[ Right ].X ) then
            begin
              Temp := Left;
              Left := Right;
              Right := Temp;
            end;
          end;
        //Roundoff error
          Dec( p[ Top ].Y );
          Dec( p[ Left ].X );
          Inc( p[ Bottom ].Y );
          Inc( p[ Right ].X );

          if ( p[ Top ].Y < 0 ) then
            TileRow := 0
          else
            TileRow := p[ Top ].Y div FMap.FTileHeight;
          ScanY := ( TileRow + 1 ) * FMap.FTileHeight;
          TileRowOffset := TileRow * Fmap.FWidth;

          XL1 := p[ Top ].X;
          YL1 := p[ Top ].Y;
          XL2 := p[ Left ].X;
          YL2 := p[ Left ].Y;
          XR1 := XL1;
          YR1 := YL1;
          XR2 := p[ Right ].X;
          YR2 := p[ Right ].Y;
          dXL := XL2 - XL1;
          dYL := YL2 - YL1;
          dXR := XR2 - XR1;
          dYR := YR2 - YR1;
          ModeL := True;
          ModeR := True;
          while ( ScanY < FMap.FBitHeight ) do
          begin
            if ( ModeL ) then
            begin
              if ( ScanY > p[ Left ].Y ) then
              begin
                XL1 := p[ Bottom ].X;
                YL1 := p[ Bottom ].Y;
                dXL := XL2 - XL1;
                dYL := YL2 - YL1;
                ModeL := False;
                XL := XL2 div FMap.FTileWidth;
              end
              else
                XL := ( XL1 + dXL * ( ScanY - YL1 ) div dYL ) div FMap.FTileWidth;
            end
            else
              XL := ( XL1 + dXL * ( ScanY - FMap.FTileHeight - YL1 ) div dYL ) div FMap.FTileWidth;

            if ( ModeR ) then
            begin
              if ( ScanY > p[ Right ].Y ) then
              begin
                XR1 := p[ Bottom ].X;
                YR1 := p[ Bottom ].Y;
                dXR := XR2 - XR1;
                dYR := YR2 - YR1;
                ModeR := False;
                XR := XR2 div FMap.FTileWidth;
              end
              else
                XR := ( XR1 + dXR * ( ScanY - YR1 ) div dYR ) div FMap.FTileWidth;
            end
            else
              XR := ( XR1 + dXR * ( ScanY - FMap.FTileHeight - YR1 ) div dYR ) div FMap.FTileWidth;

            if ( XL < 0 ) then
              XL := 0;
            if ( XR >= FMap.FWidth ) then
              XR := FMap.FWidth - 1;
            GridLoc := GridBase;
            Inc( GridLoc, TileRowOffset );
            Inc( GridLoc, XL );
            for X := XL to XR do
            begin
              if Figure.UseLineOfSight then
                CollisionMask := GridLoc^.LineOfSightMask
              else
                CollisionMask := GridLoc^.CollisionMask;
              if ( CollisionMask <> 0 ) then
              begin
              //(*whew*) We've established there could be a collision, now let's find out
              //if there really is, and if so, where?
                for j := 1 to 4 do
                begin
                  cy := ScanY - ( j - 1 ) * FMap.FStripHeight - ( FMap.FStripHeight shr 1 );
                  for i := 1 to 4 do
                  begin
                    if ( ( CollisionMask and 1 ) = 1 ) then
                    begin
                      cx := X * FMap.FTileWidth + ( i - 1 ) * FMap.FStripWidth + ( FMap.FStripWidth shr 1 );
                      A := sqr( Dx ) + sqr( dy );
                      B := 2 * ( Dx * ( Figure.StepX - cx ) + 2 * dy * ( Figure.StepY - cy ) );
                      C := sqr( Figure.StepX - cx ) + 4 * sqr( Figure.StepY - cy ) -
                        sqr( Figure.Radius +
                        ( FMap.FStripWidth shr 1 ) );
                      Q := sqr( B ) - 4 * A * C;
                      if ( Q >= 0 ) then
                      begin
                        T1 := ( -B - Sqrt( Q ) ) / ( 2 * A );
                        if ( T1 > -Tol ) and ( T1 < T2 ) then
                          T2 := T1;
                      end;
                    end;
                    CollisionMask := CollisionMask shr 1;
                  end;
                end;
              end;
              Inc( GridLoc );
            end;
            if ( ScanY >= p[ Bottom ].Y ) then
              Break;
            Inc( TileRowOffset, FMap.FWidth );
            Inc( ScanY, FMap.FTileHeight );
          end;

          if ( T2 < T ) then
          begin
            Stop := False;
            Figure.OnCollideItem( Figure, Stop );
            if ( Stop ) then
              T := T2;
          end;
        end;

        if ( not Figure.Moved ) then
        begin //This clause allows an event to call SetPos
        //Figure has hit something, perform movement calculations
          if ( T < 1 ) then
          begin
            Figure.Moving := False;
            DestX := Figure.StepX + Dx * T;
            DestY := Figure.StepY + dy * T / 2;
            DestZ := Figure.StepZ + dZ * T;
            if Assigned( Figure.OnStop ) then
              Figure.OnStop( Figure );
          end;
        end;
      end;

      if ( not Figure.Moved ) then
      begin //This if allows an event to call SetPos
        Figure.FStepX := DestX;
        Figure.FStepY := DestY;
        Figure.FStepZ := DestZ;
        Figure.FPrevX := Figure.FX;
        Figure.FPrevY := Figure.FY;
        Figure.FPrevZ := Figure.FZ;
        Figure.FX := Round( DestX );
        Figure.FY := Round( DestY );
        Figure.FZ := Round( DestZ );
        Figure.MapOffset := ( Figure.FY div FMap.FTileHeight ) * FMap.FWidth + ( Figure.FX div FMap.FTileWidth );
        GridLoc := GridBase;
        Inc( GridLoc, ( Figure.MapOffset ) );
        NextFigure := GridLoc^.Figure;
        GridLoc^.Figure := Figure;
        Figure.NextInTile := NextFigure;
        Figure.Zone := GridLoc^.Zone[ 0 ];
        Figure.FTile := PGridInfo( GridLoc );
        if ( GridLoc^.TriggerID <> OldTriggerID ) then
        begin
          if Assigned( Figure.OnTrigger ) then
          begin
            if ( GridLoc^.TriggerID = 0 ) or ( GridLoc^.TriggerMask = $FFFF ) then
            begin
              Figure.OnTrigger( Figure, GridLoc^.TriggerID, OldTriggerID );
            end
            else
            begin
              RemX := Figure.FX div FMap.FTileWidth;
              RemY := FMap.FTileHeight - ( Figure.FY mod FMap.FTileHeight );
              BitMask := 1 shl ( ( RemY div FMap.FStripHeight ) * 4 + ( RemX div FMap.FStripWidth ) );
              if ( GridLoc^.TriggerMask and BitMask ) > 0 then
                Figure.OnTrigger( Figure, GridLoc^.TriggerID, OldTriggerID );
            end;

          end;
        end;
        if ( GridLoc^.FilterID <> OldFilterID ) then
        begin
          if Assigned( Figure.OnFilter ) then
          begin
            if ( GridLoc^.FilterID = 0 ) or ( GridLoc^.FilterMask = $FFFF ) then
            begin
              Figure.OnFilter( Figure, GridLoc^.FilterID, OldFilterID );
            end
            else
            begin
              RemX := Figure.FX div FMap.FTileWidth;
              RemY := FMap.FTileHeight - ( Figure.FY mod FMap.FTileHeight );
              BitMask := 1 shl ( ( RemY div FMap.FStripHeight ) * 4 + ( RemX div FMap.FStripWidth ) );
              if ( GridLoc^.FilterMask and BitMask ) > 0 then
                Figure.OnFilter( Figure, GridLoc^.FilterID, OldFilterID );
            end;
          end;
        end;
   //   Figure.FPrevX := Figure.FX;
   //   Figure.FPrevY := Figure.FY;
   //   Figure.FPrevZ := Figure.FZ;
      end;
      GlobalUnlock( FMap.FMapData );
    end;
  end;

  function TAniView.ClearShot( SrcX, Srcy, DstX, DstY, Radius : longint; UseLineOfSight : boolean ) : boolean;
  const
    Tol = 1 / 256;
  var
    Dx, dy, dZ, D : Double;
    DestX, DestY, DestZ : Double;
    Gx, Gy : Integer;
    Gx2, Gy2 : Integer;
    R, G : Double;
    p : array[ 0..3 ] of TPoint;
    Top, Left, Right, Bottom, Temp : Integer;
    XL1, YL1, XL2, YL2 : Longint;
    XR1, YR1, XR2, YR2 : Longint;
    X, Y, XL, XR, ScanY, TileRow, TileRowOffset : Longint;
    dXL, dYL, dXR, dYR : Longint;
    ModeL, ModeR : Boolean;
    GridBase, GridLoc : ^GridInfo;
    CollisionMask : Word;
    A, B, C, Q, T, T1 : Double;
    cx, cy : Integer;
    i, j : Integer;
    edge : Longint;
    Stop : Boolean;
    Point : ^TPoint;
    NextFigure, TempFigure : TAniFigure;
  begin
    Dx := DestX - SrcX;
    dy := 2 * ( DestY - SrcY );
    D := sqrt( sqr( Dx ) + sqr( dy ) );
    if D <= 0 then
    begin
      result := true;
      exit;
    end;

    if Assigned( FMap ) then
    begin
    //Collision detection
      GridBase := GlobalLock( FMap.FMapData );
      try
        if ( Dx <> 0 ) or ( dy <> 0 ) then
        begin
          D := sqrt( sqr( Dx ) + sqr( dy ) );
          T := 1;
        //Collision with map boundaries
          if ( Dx <> 0 ) then
          begin
            edge := Radius + FMap.FTileWidth;
            if ( DestX < edge ) then
            begin
              T1 := ( edge - SrcX ) / Dx;
              if ( T1 < T ) then
              begin
                result := false;
                exit;
              end;
            end
            else
            begin
              edge := FMap.FBitWidth - Radius - FMap.FTileWidth;
              if ( DestX > edge ) then
              begin
                T1 := ( edge - SrcX ) / Dx;
                if ( T1 < T ) then
                begin
                  result := false;
                  exit;
                end;
              end;
            end;
          end;
          if ( dy <> 0 ) then
          begin
            edge := Radius + FMap.FTileHeight;
            if ( DestY < edge ) then
            begin
              T1 := ( edge - SrcY ) / dy;
              if ( T1 < T ) then
              begin
                result := false;
                exit;
              end;
            end
            else
            begin
              edge := FMap.FBitHeight - Radius - FMap.FTileHeight;
              if ( DestY > edge ) then
              begin
                T1 := ( edge - SrcY ) / dy;
                if ( T1 < T ) then
                begin
                  result := false;
                  exit;
                end;
              end;
            end;
          end;

        //Collisions with other figures
          R := Radius + FMaxCollisionRadius;
          G := R / D;
          Gx := Round( G * Dx );
          Gy := Round( G * dy );
          Gx2 := Gx div 2;
          Gy2 := Gy div 2;

          X := SrcX;
          Y := SrcY;
          p[ 0 ].X := X - Gy;
          p[ 0 ].Y := Y + Gx2;
          p[ 1 ].X := X + Gy;
          p[ 1 ].Y := Y - Gx2;
          X := Round( DestX );
          Y := Round( DestY );
          p[ 2 ].X := X + Gy + Gx;
          p[ 2 ].Y := Y - Gx2 + Gy2;
          p[ 3 ].X := X - Gy + Gx;
          p[ 3 ].Y := Y + Gx2 + Gy2;

          if ( p[ 0 ].Y < p[ 1 ].Y ) then
            Top := 0
          else
            Top := 1;
          if ( p[ 2 ].Y < p[ Top ].Y ) then
            Top := 2;
          if ( p[ 3 ].Y < p[ Top ].Y ) then
            Top := 3;
          Bottom := ( Top + 2 ) mod 4;
          Left := ( Top + 1 ) mod 4;
          Right := ( Top + 3 ) mod 4;
          if ( p[ Left ].X > p[ Right ].X ) then
          begin
            Temp := Left;
            Left := Right;
            Right := Temp;
          end;
        //Roundoff error
          Dec( p[ Top ].Y );
          Dec( p[ Left ].X );
          Inc( p[ Bottom ].Y );
          Inc( p[ Right ].X );

          if ( p[ Top ].Y < 0 ) then
            TileRow := 0
          else
            TileRow := p[ Top ].Y div FMap.FTileHeight;
          ScanY := ( TileRow + 1 ) * FMap.FTileHeight;
          TileRowOffset := TileRow * Fmap.FWidth;

          XL1 := p[ Top ].X;
          YL1 := p[ Top ].Y;
          XL2 := p[ Left ].X;
          YL2 := p[ Left ].Y;
          XR1 := XL1;
          YR1 := YL1;
          XR2 := p[ Right ].X;
          YR2 := p[ Right ].Y;
          dXL := XL2 - XL1;
          dYL := YL2 - YL1;
          dXR := XR2 - XR1;
          dYR := YR2 - YR1;
          ModeL := True;
          ModeR := True;
          while ( ScanY < FMap.FBitHeight ) do
          begin
            if ( ModeL ) then
            begin
              if ( ScanY > p[ Left ].Y ) then
              begin
                XL1 := p[ Bottom ].X;
                YL1 := p[ Bottom ].Y;
                dXL := XL2 - XL1;
                dYL := YL2 - YL1;
                ModeL := False;
                XL := XL2 div FMap.FTileWidth;
              end
              else
                XL := ( XL1 + dXL * ( ScanY - YL1 ) div dYL ) div FMap.FTileWidth;
            end
            else
              XL := ( XL1 + dXL * ( ScanY - FMap.FTileHeight - YL1 ) div dYL ) div FMap.FTileWidth;

            if ( ModeR ) then
            begin
              if ( ScanY > p[ Right ].Y ) then
              begin
                XR1 := p[ Bottom ].X;
                YR1 := p[ Bottom ].Y;
                dXR := XR2 - XR1;
                dYR := YR2 - YR1;
                ModeR := False;
                XR := XR2 div FMap.FTileWidth;
              end
              else
                XR := ( XR1 + dXR * ( ScanY - YR1 ) div dYR ) div FMap.FTileWidth;
            end
            else
              XR := ( XR1 + dXR * ( ScanY - FMap.FTileHeight - YR1 ) div dYR ) div FMap.FTileWidth;

            if ( XL < 0 ) then
              XL := 0;
            if ( XR >= FMap.FWidth ) then
              XR := FMap.FWidth - 1;
            GridLoc := GridBase;
            Inc( GridLoc, TileRowOffset );
            Inc( GridLoc, XL );
            for X := XL to XR do
            begin
              NextFigure := GridLoc^.Figure;
              while Assigned( NextFigure ) do
              begin
                if NextFigure.Radius > 0 then
                begin
                  A := sqr( Dx ) + sqr( dy );
                  B := 2 * ( Dx * ( SrcX - NextFigure.StepX ) + 2 * dy * ( SrcY - NextFigure.StepY ) );
                  C := sqr( SrcX - NextFigure.StepX ) + 4 * sqr( SrcY - NextFigure.StepY ) -
                    sqr( Radius + NextFigure.Radius );
                  Q := sqr( B ) - 4 * A * C;
                  if ( Q >= 0 ) then
                  begin
                    T1 := ( -B - Sqrt( Q ) ) / ( 2 * A );
                    if ( T1 < 1 ) and ( T1 >= -Tol ) then
                    begin
                      result := false;
                      exit;
                    end;
                  end;
                end;
                NextFigure := NextFigure.NextInTile;
              end;
              Inc( GridLoc );
            end;
            if ( ScanY >= p[ Bottom ].Y ) then
              Break;
            Inc( TileRowOffset, FMap.FWidth );
            Inc( ScanY, FMap.FTileHeight );
          end;

        //Collisions with map objects
          R := Radius;
          G := R / D;
          Gx := Round( G * Dx );
          Gy := Round( G * dy );
          Gx2 := Gx div 2;
          Gy2 := Gy div 2;

          X := SrcX;
          Y := SrcY;
          p[ 0 ].X := X - Gy;
          p[ 0 ].Y := Y + Gx2;
          p[ 1 ].X := X + Gy;
          p[ 1 ].Y := Y - Gx2;
          X := Round( DestX );
          Y := Round( DestY );
          p[ 2 ].X := X + Gy + Gx;
          p[ 2 ].Y := Y - Gx2 + Gy2;
          p[ 3 ].X := X - Gy + Gx;
          p[ 3 ].Y := Y + Gx2 + Gy2;

        //Roundoff error
          Dec( p[ Top ].Y );
          Dec( p[ Left ].X );
          Inc( p[ Bottom ].Y );
          Inc( p[ Right ].X );

          if ( p[ Top ].Y < 0 ) then
            TileRow := 0
          else
            TileRow := p[ Top ].Y div FMap.FTileHeight;
          ScanY := ( TileRow + 1 ) * FMap.FTileHeight;
          TileRowOffset := TileRow * Fmap.FWidth;

          XL1 := p[ Top ].X;
          YL1 := p[ Top ].Y;
          XL2 := p[ Left ].X;
          YL2 := p[ Left ].Y;
          XR1 := XL1;
          YR1 := YL1;
          XR2 := p[ Right ].X;
          YR2 := p[ Right ].Y;
          dXL := XL2 - XL1;
          dYL := YL2 - YL1;
          dXR := XR2 - XR1;
          dYR := YR2 - YR1;
          ModeL := True;
          ModeR := True;
          while ( ScanY < FMap.FBitHeight ) do
          begin
            if ( ModeL ) then
            begin
              if ( ScanY > p[ Left ].Y ) then
              begin
                XL1 := p[ Bottom ].X;
                YL1 := p[ Bottom ].Y;
                dXL := XL2 - XL1;
                dYL := YL2 - YL1;
                ModeL := False;
                XL := XL2 div FMap.FTileWidth;
              end
              else
                XL := ( XL1 + dXL * ( ScanY - YL1 ) div dYL ) div FMap.FTileWidth;
            end
            else
              XL := ( XL1 + dXL * ( ScanY - FMap.FTileHeight - YL1 ) div dYL ) div FMap.FTileWidth;

            if ( ModeR ) then
            begin
              if ( ScanY > p[ Right ].Y ) then
              begin
                XR1 := p[ Bottom ].X;
                YR1 := p[ Bottom ].Y;
                dXR := XR2 - XR1;
                dYR := YR2 - YR1;
                ModeR := False;
                XR := XR2 div FMap.FTileWidth;
              end
              else
                XR := ( XR1 + dXR * ( ScanY - YR1 ) div dYR ) div FMap.FTileWidth;
            end
            else
              XR := ( XR1 + dXR * ( ScanY - FMap.FTileHeight - YR1 ) div dYR ) div FMap.FTileWidth;

            if ( XL < 0 ) then
              XL := 0;
            if ( XR >= FMap.FWidth ) then
              XR := FMap.FWidth - 1;
            GridLoc := GridBase;
            Inc( GridLoc, TileRowOffset );
            Inc( GridLoc, XL );
            for X := XL to XR do
            begin
              if UseLineOfSight then
                CollisionMask := GridLoc^.LineOfSightMask
              else
                CollisionMask := GridLoc^.CollisionMask;
              if ( CollisionMask <> 0 ) then
              begin
                for j := 1 to 4 do
                begin
                  cy := ScanY - ( j - 1 ) * FMap.FStripHeight - ( FMap.FStripHeight shr 1 );
                  for i := 1 to 4 do
                  begin
                    if ( ( CollisionMask and 1 ) = 1 ) then
                    begin
                      cx := X * FMap.FTileWidth + ( i - 1 ) * FMap.FStripWidth + ( FMap.FStripWidth shr 1 );
                      A := sqr( Dx ) + sqr( dy );
                      B := 2 * ( Dx * ( SrcX - cx ) + 2 * dy * ( SrcY - cy ) );
                      C := sqr( SrcX - cx ) + 4 * sqr( SrcY - cy ) -
                        sqr( Radius +
                        ( FMap.FStripWidth shr 1 ) );
                      Q := sqr( B ) - 4 * A * C;
                      if ( Q >= 0 ) then
                      begin
                        T1 := ( -B - Sqrt( Q ) ) / ( 2 * A );
                        if ( T1 > -Tol ) and ( T1 < T ) then
                        begin
                          result := false;
                          exit;
                        end;
                      end;
                    end;
                    CollisionMask := CollisionMask shr 1;
                  end;
                end;
              end;
              Inc( GridLoc );
            end;
            if ( ScanY >= p[ Bottom ].Y ) then
              Break;
            Inc( TileRowOffset, FMap.FWidth );
            Inc( ScanY, FMap.FTileHeight );
          end;
        end;
      finally
        GlobalUnlock( FMap.FMapData );
      end;
    end;
    result := true;
  end;

  procedure TAniView.DisableFigure( Figure : TAniFigure );
  var
    GridBase, GridLoc : ^GridInfo;
    NextFigure, TempFigure : TAniFigure;
  begin
    if Assigned( FMap ) then
    begin
    //Collision detection
      GridBase := GlobalLock( FMap.FMapData );
      GridLoc := GridBase;
      if ( Figure.MapOffset >= 0 ) then
      begin
      //Remove figure from collision chain
        Inc( GridLoc, ( Figure.MapOffset ) );
        NextFigure := GridLoc^.Figure;
        if ( NextFigure = Figure ) then
        begin
          GridLoc^.Figure := Figure.NextInTile;
        end
        else
        begin
          TempFigure := NextFigure;
          while Assigned( NextFigure ) do
          begin
            NextFigure := NextFigure.NextInTile;
            if ( NextFigure = Figure ) then
            begin
              TempFigure.NextInTile := NextFigure.NextInTile;
              Break;
            end;
            TempFigure := NextFigure;
          end;
        end;
        Figure.NextInTile := nil;
      end;
      GlobalUnlock( FMap.FMapData );
    end;
    Figure.ViewEnabled := False;
  end;

  procedure TAniView.TransFigure( Figure : TAniFigure );
  var
    GridBase, GridLoc : ^GridInfo;
    OldTriggerID : SmallInt;
    OldFilterID : SmallInt;
    NextFigure, TempFigure : TAniFigure;
    RemX, RemY : integer;
    BitMask : word;
  begin
    Figure.Moved := False;
    if Assigned( FMap ) then
    begin
      GridBase := GlobalLock( FMap.FMapData );
      GridLoc := GridBase;
      if ( Figure.MapOffset >= 0 ) then
      begin
      //Remove figure from collision chain
        Inc( GridLoc, ( Figure.MapOffset ) );
        NextFigure := GridLoc^.Figure;
        RemX := Figure.FPrevX mod FMap.FTileWidth;
        RemY := FMap.FTileHeight - ( Figure.FPrevY mod FMap.FTileHeight ) - 1;
        BitMask := 1 shl ( ( RemY div FMap.FStripHeight ) * 4 + ( RemX div FMap.FStripWidth ) );
        if ( GridLoc^.TriggerMask and BitMask ) > 0 then
          OldTriggerID := GridLoc^.TriggerID
        else
          OldTriggerID := 0;
        if ( GridLoc^.FilterMask and BitMask ) > 0 then
          OldFilterID := GridLoc^.FilterID
        else
          OldFilterID := 0;
        if ( NextFigure = Figure ) then
        begin
          GridLoc^.Figure := Figure.NextInTile;
        end
        else
        begin
          TempFigure := NextFigure;
          while Assigned( NextFigure ) do
          begin
            NextFigure := NextFigure.NextInTile;
            if ( NextFigure = Figure ) then
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

      Figure.MapOffset := ( Figure.FY div FMap.FTileHeight ) * FMap.FWidth + ( Figure.FX div FMap.FTileWidth );
      if ( Figure.MapOffset < 0 ) then
        Figure.MapOffset := 0;
      if Figure.MapOffset >= FMap.Height * FMap.Width then
        Figure.MapOffset := FMap.Height * FMap.Width - 1;
      GridLoc := GridBase;
      Inc( GridLoc, ( Figure.MapOffset ) );
      NextFigure := GridLoc^.Figure;
      GridLoc^.Figure := Figure;
      Figure.NextInTile := NextFigure;
      Figure.Zone := GridLoc^.Zone[ 0 ];
      Figure.FTile := PGridInfo( GridLoc );
      if ( GridLoc^.TriggerID <> OldTriggerID ) then
      begin
        if Assigned( Figure.OnTrigger ) then
        begin
          if ( GridLoc^.TriggerID = 0 ) or ( GridLoc^.TriggerMask = $FFFF ) then
          begin
            Figure.OnTrigger( Figure, GridLoc^.TriggerID, OldTriggerID );
          end
          else
          begin
            RemX := Figure.FX div FMap.FTileWidth;
            RemY := FMap.FTileHeight - ( Figure.FY mod FMap.FTileHeight );
            BitMask := 1 shl ( ( RemY div FMap.FStripHeight ) * 4 + ( RemX div FMap.FStripWidth ) );
            if ( GridLoc^.TriggerMask and BitMask ) > 0 then
              Figure.OnTrigger( Figure, GridLoc^.TriggerID, OldTriggerID );
          end;

        end;
      end;
      if ( GridLoc^.FilterID <> OldFilterID ) then
      begin
        if Assigned( Figure.OnFilter ) then
        begin
          if ( GridLoc^.FilterID = 0 ) or ( GridLoc^.FilterMask = $FFFF ) then
          begin
            Figure.OnFilter( Figure, GridLoc^.FilterID, OldFilterID );
          end
          else
          begin
            RemX := Figure.FX div FMap.FTileWidth;
            RemY := FMap.FTileHeight - ( Figure.FY mod FMap.FTileHeight );
            BitMask := 1 shl ( ( RemY div FMap.FStripHeight ) * 4 + ( RemX div FMap.FStripWidth ) );
            if ( GridLoc^.FilterMask and BitMask ) > 0 then
              Figure.OnFilter( Figure, GridLoc^.FilterID, OldFilterID );
          end;
        end;
      end;
      Figure.FPrevX := Figure.FX;
      Figure.FPrevY := Figure.FY;
      Figure.FPrevZ := Figure.FZ;
      GlobalUnlock( FMap.FMapData );
    end;
  end;

  procedure TAniView.ComputeLight( Figure : TAniFigure );
  var
    i, j : Integer;
    X1, Y1, Z1 : Longint;
    IL1, D : Double;
    R1, G1, B1 : Double;
    RL, GL, BL : Integer;
    Test : TLightZone;
  begin
    Figure.LightComputed := FrameCount;
    R1 := FMap.LightR;
    G1 := FMap.LightG;
    B1 := FMap.LightB;
    if ( TZone( FMap.Zones.Items[ Figure.Zone ] ) is TLightZone ) then
    begin
      j := 0;
      for i := 0 to TLightZone( FMap.Zones.Items[ Figure.Zone ] ).OverlapZones.Count - 1 do
      begin
        Test := TLightZone( FMap.Zones.Items[ Figure.Zone ] ).OverlapZones.Items[ i ];
        X1 := sqr( Test.FlickerX[ Test.State ] - Figure.FX );
        Y1 := sqr( ( Test.FlickerY[ Test.State ] - Figure.FY ) * 2 );
        Z1 := sqr( Test.FlickerZ[ Test.State ] - ( Figure.Height div 2 ) );
        D := sqrt( X1 + Y1 + Z1 ) / Test.FlickerRadius[ Test.State ];
        if D <= 1 then
        begin
          if FMap.LineOfSight( Test.FlickerX[ Test.State ], Test.FlickerY[ Test.State ], Figure.FX, Figure.FY ) then
          begin
            RL := Test.Color and $FF;
            GL := Test.Color and $FF00 shr 8;
            BL := Test.Color and $FF0000 shr 16;
            IL1 := ( 1 - D ) * Test.FlickerIntensity[ Test.State ] / 100;
            Figure.EnumLightSource( j, Test.FlickerX[ Test.State ], Test.FlickerY[ Test.State ], Test.FlickerZ[ Test.State ], IL1, Test.FlickerRadius[ Test.State ] );
            inc( j );
            R1 := R1 + IL1 * RL;
            G1 := G1 + IL1 * GL;
            B1 := B1 + IL1 * BL;
          end;
        end;
      end;
    end;
    Figure.LightR := Round( R1 );
    Figure.LightG := Round( G1 );
    Figure.LightB := Round( B1 );
    if ( R1 >= G1 ) and ( R1 >= B1 ) then
      Figure.FLightIndex := R1 / 255
    else if ( G1 >= B1 ) then
      Figure.FLightIndex := G1 / 255
    else
      Figure.FLightIndex := B1 / 255;
  end;

  procedure TAniView.DrawFigure( Figure : TAniFigure );
  begin
    Figure.FPosX := Figure.FX - Figure.CenterX - OffsetX;
    if ( Figure.FPosX + Figure.Width < Left ) or ( Figure.FPosX >= Left + Width ) then
      exit;
    Figure.FPosY := Figure.FY - Figure.CenterY - Figure.FZ - OffsetY;
    if ( Figure.UseLighting ) and FMap.UseLighting then
    begin
      ComputeLight( Figure );
    end
    else
    begin
      Figure.LightR := 255;
      Figure.LightG := 255;
      Figure.LightB := 255;
    end;
    Figure.FOnScreen := true;
    Figure.Render;
  end;

  procedure TAniView.SetInterval( PInterval : Word );
  begin
    FInterval := PInterval;
    if Assigned( Timer ) then
      Timer.Interval := PInterval;
  end;

  procedure TAniView.SetActive( VActive : Boolean );
  begin
    FActive := VActive;
    if FActive then
    begin
      if not Assigned( Timer ) then
      begin
        Timer := TAniTimer.create( nil );
        Timer.Interval := FInterval;
        Timer.TimerPriority := tpNormal;
        Timer.Resolution := 1;
      end;
      Timer.OnTimer := FDrawFrame;
      Timer.enabled := FActive;
    end
    else
    begin
      if Assigned( Timer ) then
      begin
        Timer.OnTimer := nil;
        Timer.enabled := FActive;
      end;
    end;
  end;

  procedure TAniView.SetItemMask( Mask : Longint );
  begin
    if ( FItemMask <> Mask ) then
    begin
      FItemMask := Mask;
      FRefreshMap;
    end;
  end;

  procedure TAniView.SetShowRepaint( const Value : Boolean );
  begin
    FShowRepaint := Value;
    if FShowRepaint then
      RepaintCode := DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT
    else
      RepaintCode := DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT;
  end;

  procedure TAniView.SetAutoTransparentMask( const Value : TBitmap );
  begin
    if Assigned( Value ) then
    begin
{$IFNDEF DirectX}
      XRayImage.Free;
      XRayImage := TBitmap.Create;
      XRayImage.Assign( Value );
{$ENDIF}
{$IFDEF DirectX}
      XRayImage := nil;
      XRayImage := DDGetImage( lpDD, Value, clBlack, True );
{$ENDIF}
      XRayWidth := Value.width;
      XRayHeight := Value.Height;
    end
    else
    begin
{$IFNDEF DirectX}
      XRayImage.Free;
{$ENDIF}
      XRayImage := nil;
      XRayWidth := 0;
      XRayHeight := 0;
    end;
  end;

  function TAniView.FindInRadius( X, Y : Longint; Radius : Single ) : TList;
  var
    i : integer;
    Dx, dy : Longint;
    R, hR : Single;
  begin
    Result := nil;
    for i := 0 to FigureList.Count - 1 do
    begin
      if TAniFigure( FigureList.Items[ i ] ).Enabled then
      begin
        R := Radius + TAniFigure( FigureList.Items[ i ] ).Radius;
        Dx := TAniFigure( FigureList.Items[ i ] ).X - X;
        if ( Dx <= R ) and ( Dx >= -R ) then
        begin
          dy := TAniFigure( FigureList.Items[ i ] ).Y - Y;
          hR := R / 2;
          if ( dy <= hR ) and ( dy >= -hR ) then
          begin
            TAniFigure( FigureList.Items[ i ] ).FDistance := sqrt( sqr( Dx ) + sqr( 2 * dy ) );
            if TAniFigure( FigureList.Items[ i ] ).Distance <= R then
            begin
              if not Assigned( Result ) then
                Result := TList.Create;
              Result.Add( TAniFigure( FigureList.Items[ i ] ) )
            end;
          end;
        end;
      end;
    end;
  end;

  function TAniView.LineOfSight( X1, Y1, X2, Y2 : Longint ) : Boolean;
  begin
    if Assigned( FMap ) then
      Result := FMap.LineOfSight( X1, Y1, X2, Y2 )
    else
      Result := True;
  end;

  function TAniView.LineOfCollision( X1, Y1, X2, Y2 : Longint ) : Boolean;
  begin
    if Assigned( FMap ) then
      Result := FMap.LineOfCollision( X1, Y1, X2, Y2 )
    else
      Result := True;
  end;

  procedure Register;
  begin
    RegisterComponents( 'Animation', [ TAniView, TAniMap ] );
  //  RegisterComponents('Animation', [TAniView, TAniFigure, TAniMap]);
  //  RegisterComponents('Animation', [TAniView, TAniFigure, TAniMap]);
  //  RegisterPropertyEditor(TypeInfo(TAbout), TAniFigure, 'About', TAbout);
  //  RegisterPropertyEditor(TypeInfo(TAbout), TAniMap, 'About', TAbout);
  //  RegisterPropertyEditor(TypeInfo(TAbout), TAniView, 'About', TAbout);
  //  RegisterComponentEditor(TAniFigure, TAniFigureEditor);
  //  RegisterComponentEditor(TAniMap, TAniMapEditor);
  end;

{ TZone }

  constructor TZone.Create( Map : TAniMap );
  begin
    inherited Create;
    FMap := Map;
    FTileBitWidth := Map.FTileWidth;
    FItemBitWidth := Map.FTileWidth;
    FTileMaxColumnIndex := MaxZoneHeight div Map.FTileHeight;
  end;

  destructor TZone.Destroy;
  begin
    DisposeDef;
{$IFDEF DirectX}
    FTileImages := nil;
    FItemImages := nil;
{$ENDIF}
{$IFNDEF DirectX}
    DeleteObject( FTileImages );
    FTileImages := 0;
    DeleteObject( FTileMasks );
    FTileMasks := 0;
    DeleteObject( FItemImages );
    FItemImages := 0;
    DeleteObject( FItemMasks );
    FItemMasks := 0;
{$ENDIF}
    Cached := False;
    inherited;
  end;

  function TZone.GetTile( i : Integer ) : TileInfo;
  var
    P : ^TileInfo;
  begin
    if ( i > 0 ) and ( i <= MaxTiles ) then
    begin
      P := Pointer( FTileMem );
      Inc( P, i - 1 );
      Result := P^;
    end;
  end;

  procedure TZone.SetTile( i : Integer; Tile : TileInfo );
  var
    P : ^TileInfo;
  begin
    if ( i > 0 ) and ( i <= MaxTiles ) then
    begin
      if ( FTileMem = nil ) then
      begin
        GetMem( FTileMem, SizeOf( TileInfo ) * MaxTiles );
        ZeroMemory( FTileMem, SizeOf( TileInfo ) * MaxTiles );
      end;
      P := Pointer( FTileMem );
      Inc( P, i - 1 );
      P^ := Tile;
    end;
  end;

  procedure TZone.DisposeDef;
  var
    i : Integer;
  begin
    if Assigned( FItemMem ) then
    begin
      for i := 1 to MaxItems do
      begin
        if Item[ i ].Used then
        begin
          if ( Item[ i ].StripHeights <> 0 ) then
          begin
            GlobalFree( Item[ i ].StripHeights );
          end;
          if ( Item[ i ].CollisionMasks <> 0 ) then
          begin
            GlobalFree( Item[ i ].CollisionMasks );
          end;
          if ( Item[ i ].LineOfSightMasks <> 0 ) then
          begin
            GlobalFree( Item[ i ].LineOfSightMasks );
          end;
          if ( Item[ i ].LightPoints <> 0 ) then
          begin
            GlobalFree( Item[ i ].LightPoints );
          end;
        end;
      end;
    end;

    if Assigned( FTileMem ) then
      FreeMem( FTileMem );
    FTileMem := nil;
    if Assigned( FItemMem ) then
      FreeMem( FItemMem );
    FItemMem := nil;
  end;

  function TZone.GetItem( i : Integer ) : ItemInfo;
  var
    p : ^ItemInfo;
  begin
    if ( i > 0 ) and ( i <= MaxItems ) then
    begin
      p := Pointer( FItemMem );
      Inc( p, i - 1 );
      Result := p^;
    end;
  end;

  procedure TZone.SetItem( i : Integer; Item : ItemInfo );
  var
    p : ^ItemInfo;
    j : Integer;
  begin
    if ( i > 0 ) and ( i <= MaxItems ) then
    begin
      if ( FItemMem = nil ) then
      begin
        GetMem( FItemMem, SizeOf( ItemInfo ) * MaxItems );
        ZeroMemory( FItemMem, SizeOf( ItemInfo ) * MaxItems );
        p := Pointer( FItemMem );
        for j := 1 to MaxItems do
        begin
          p^.Used := False;
          Inc( p );
        end;
      end;
      p := Pointer( FItemMem );
      Inc( p, i - 1 );
      if ( p^.CollisionMasks <> 0 ) then
        GlobalFree( p^.CollisionMasks );
      if ( p^.LineOfSightMasks <> 0 ) then
        GlobalFree( p^.LineOfSightMasks );
      if ( p^.StripHeights <> 0 ) then
        GlobalFree( p^.StripHeights );
      if ( p^.LightPoints <> 0 ) then
        GlobalFree( p^.LightPoints );
      p^ := Item;
      p^.Used := True;
    end;
  end;

  procedure TZone.Release;
  begin
{$IFDEF DirectX}
    FTileImages := nil;
    FItemImages := nil;
{$ENDIF}
{$IFNDEF DirectX}
    DeleteObject( FTileImages );
    FTileImages := 0;
    DeleteObject( FTileMasks );
    FTileMasks := 0;
    DeleteObject( FItemImages );
    FItemImages := 0;
    DeleteObject( FItemMasks );
    FItemMasks := 0;
{$ENDIF}
    Cached := True;
  end;

  procedure TZone.Restore;
  begin
    Cached := True;
  end;

{$IFDEF DirectX}
  function TZone.DefineItem( Index : Word; Image : IDirectDrawSurface; const StripHeights,
    CollisionMasks, LineOfSightMasks, LightPoints : array of Word; Color : TColor; Slope : Single; Visible,
    AutoTransparent, Vertical : Boolean ) : PItemInfo;
  {$ENDIF}
  {$IFNDEF DirectX}
    function TZone.DefineItem( Index : Word; Image : TBitmap; const StripHeights,
      CollisionMasks, LineOfSightMasks, LightPoints : array of Word; Color : TColor; Slope : Single; Visible,
      AutoTransparent, Vertical : Boolean ) : PItemInfo;
    {$ENDIF}
    var
      SrcDC : HDC;
      Picture, Mask : HBITMAP;
      NewBitWidth, NewBitHeight : Longint;
      i, X : Integer;
      StripData, TileData : ^Word;
      Strips, Rows, Tiles : Integer;
      W, H : integer;
    {$IFDEF DirectX}
      NewItemImages : IDirectDrawSurface;
      ddsd : DDSurfaceDesc;
      ddck : DDCOLORKEY;
//  BltFx: DDBLTFX;
      SrcX1, SrcX2, SrcY1, SrcY2 : integer;
      Bitmap : TBitmap;
    {$ENDIF}
    {$IFNDEF DirectX}
      ScreenDC, DestDC : HDC;
      NewItemImages, NewItemMasks : HBITMAP;
      OldSrcBM, OldDestBM : HBITMAP;
    {$ENDIF}
      NewItem : ItemInfo;
    begin
      if Loaded then
      begin
        result := nil;
        exit;
      end;

      NewItem.StripHeights := 0;
      NewItem.CollisionMasks := 0;
      NewItem.LineOfSightMasks := 0;
      NewItem.LightPoints := 0;
      if ( Image = nil ) then
      begin
        NewItem.Width := 0;
        NewItem.Height := 0;
        NewItem.Top := 0;
        NewItem.Left := 0;
        NewItem.Strips := 0;
        NewItem.StripCount := 0;
        Item[ Index ] := NewItem;
      end
      else
      begin
  {$IFDEF DirectX}
        GetSurfaceDims( W, H, Image );
  {$ENDIF}
  {$IFNDEF DirectX}
        W := Image.Width;
        H := Image.Height;
  {$ENDIF}
        NewItem.Width := W;
        NewItem.Height := H;
        NewItem.Strips := W div FMap.FTileWidth;
        NewItem.StripCount := W div FMap.FStripWidth;
        NewItem.Slope := Slope;
        NewItem.AutoTransparent := AutoTransparent;
        NewItem.Vertical := Vertical;
        NewItem.Visible := Visible;
        if ( ( W mod FMap.FTileWidth ) <> 0 ) then
          Inc( NewItem.Strips );
        if ( ( W mod FMap.FStripWidth ) <> 0 ) then
          Inc( NewItem.StripCount );
        NewBitWidth := ( FItemColumn + 1 ) * FMap.FTileWidth;
        NewBitHeight := FItemColumnBitHeight + NewItem.Strips * NewItem.Height;
        if NewBitHeight > MaxZoneHeight then
        begin
          inc( FItemColumn );
          FItemColumnBitHeight := 0;
          NewBitHeight := FItemColumnBitHeight + NewItem.Strips * NewItem.Height;
          inc( NewBitWidth, FMap.FTileWidth );
        end;
        if NewBitWidth < FItemBitWidth then
          NewBitWidth := FItemBitWidth;
        if NewBitHeight < FItemBitHeight then
          NewBitHeight := FItemBitHeight;
        X := FItemColumn * FMap.FTileWidth;
        NewItem.Top := FItemColumnBitHeight;
        NewItem.Left := X;

  {$IFNDEF DirectX}
        CreateMask( Picture, Mask, Image, ColorToRGB( Color ) );
  {$ENDIF}

    //Strip Height Data
        if ( High( StripHeights ) < 0 ) then
        begin
          Log.Log( 'Generating default depth anchors' );
  {$IFDEF DirectX}
          Bitmap := TBitmap.create;
          Bitmap.width := W;
          Bitmap.height := H;
          Image.GetDC( SrcDC );
          BitBlt( Bitmap.canvas.handle, 0, 0, W, H, SrcDC, 0, 0, SRCCOPY );
          Image.ReleaseDC( SrcDC );
          CreateMask( Picture, Mask, Bitmap, ColorToRGB( Color ) );
          Bitmap.free;
  {$ENDIF}
          GetStripHeights( NewItem.StripHeights, Mask, NewItem.Width, NewItem.Height, FMap.FStripWidth );
  {$IFDEF DirectX}
          DeleteObject( Picture );
          DeleteObject( Mask );
  {$ENDIF}
        end
        else
        begin
          Strips := NewItem.Strips shl 2;
          NewItem.StripHeights := GlobalAlloc( GHND, Strips * SizeOf( Word ) );
          StripData := GlobalLock( NewItem.StripHeights );
          for i := 0 to High( StripHeights ) do
          begin
            if ( i >= Strips ) then
              Break;
            StripData^ := StripHeights[ i ];
            Inc( StripData );
          end;
          GlobalUnlock( NewItem.StripHeights );
        end;

    //Collision Data
        if ( High( CollisionMasks ) >= 0 ) then
        begin
          Rows := NewItem.Height div FMap.FTileHeight;
          if ( ( NewItem.Height mod FMap.FTileHeight ) <> 0 ) then
            Inc( Rows );
          Tiles := Rows * NewItem.Strips;
          NewItem.CollisionMasks := GlobalAlloc( GHND, Tiles * SizeOf( Word ) );
          TileData := GlobalLock( NewItem.CollisionMasks );
          for i := 0 to High( CollisionMasks ) do
          begin
            if ( i >= Tiles ) then
              Break;
            TileData^ := CollisionMasks[ i ];
            Inc( TileData );
          end;
          GlobalUnlock( NewItem.CollisionMasks );
        end;

    //Line of Sight Data
        if ( High( LineOfSightMasks ) >= 0 ) then
        begin
          Rows := NewItem.Height div FMap.FTileHeight;
          if ( ( NewItem.Height mod FMap.FTileHeight ) <> 0 ) then
            Inc( Rows );
          Tiles := Rows * NewItem.Strips;
          NewItem.LineOfSightMasks := GlobalAlloc( GHND, Tiles * SizeOf( Word ) );
          TileData := GlobalLock( NewItem.LineOfSightMasks );
          for i := 0 to High( LineOfSightMasks ) do
          begin
            if ( i >= Tiles ) then
              Break;
            TileData^ := LineOfSightMasks[ i ];
            Inc( TileData );
          end;
          GlobalUnlock( NewItem.LineOfSightMasks );
        end;

    //Light Points
        Strips := NewItem.Strips shl 2;
        NewItem.LightPoints := GlobalAlloc( GHND, Strips * SizeOf( Word ) );
        StripData := GlobalLock( NewItem.LightPoints );
        for i := 0 to High( LightPoints ) - 1 do
        begin
          if ( i >= Strips ) then
            Break;
          StripData^ := word( LightPoints[ i + 1 ] - LightPoints[ i ] );
          Inc( StripData );
        end;
        GlobalUnlock( NewItem.LightPoints );

  {$IFDEF DirectX}
        if ( NewBitWidth > FItemBitWidth ) or ( NewBitHeight > FItemBitHeight ) then
        begin
//Log.Log('Resize Zone Items: '+inttostr(FIndex));
//Log.Log(inttostr(NewBitWidth)+'x'+inttostr(NewBitHeight));
//      inc(NewBitWidth,FMap.FTileWidth);
          ddsd.dwSize := SizeOf( ddsd );
          ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
          ddsd.dwWidth := NewBitWidth;
          ddsd.dwHeight := NewBitHeight;
          if ItemsInVideo then
          begin
            ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_VIDEOMEMORY;
            if lpdd.CreateSurface( ddsd, NewItemImages, nil ) <> DD_OK then
            begin
              ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
              lpdd.CreateSurface( ddsd, NewItemImages, nil );
              ItemsInVideo := false;
            end;
          end
          else
          begin
            ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
            lpdd.CreateSurface( ddsd, NewItemImages, nil );
          end;

     { BltFx.dwSize := SizeOf(BltFx);
      BltFx.dwFillColor := FMap.ColorMatch;
      NewItemImages.Blt(Rect(0, 0, NewBitWidth, NewBitHeight), nil,
        Rect(0, 0, NewBitWidth, NewBitHeight), DDBLT_COLORFILL + DDBLT_WAIT, BltFx); }

          if Assigned( FItemImages ) then
          begin
            NewItemImages.BltFast( 0, 0, FItemImages, Rect( 0, 0, FItemBitWidth, FItemBitHeight ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
          end;
          ddck.dwColorSpaceLowValue := FMap.ColorMatch;
          ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
          NewItemImages.SetColorKey( DDCKEY_SRCBLT, ddck );

          FItemImages := nil;
          FItemImages := NewItemImages;

          if NewBitWidth > FItemBitWidth then
            FItemBitWidth := NewBitWidth;
          if NewBitHeight > FItemBitHeight then
            FItemBitHeight := NewBitHeight;
        end;

        for i := 1 to NewItem.Strips do
        begin
          SrcX1 := ( i - 1 ) * FMap.FTileWidth;
          SrcY1 := 0;
          SrcX2 := SrcX1 + FMap.FTileWidth;
          SrcY2 := SrcY1 + NewItem.Height;
          if SrcX2 > W then
            SrcX2 := W;
          if SrcY2 > H then
            SrcY2 := H;
          FItemImages.BltFast( X, FItemColumnBitHeight + ( i - 1 ) * NewItem.Height, Image,
            Rect( SrcX1, SrcY1, SrcX2, SrcY2 ),
            DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
        end;

        FItemColumnBitHeight := FItemColumnBitHeight + NewItem.Strips * NewItem.Height;
  {$ENDIF}
  {$IFNDEF DirectX}
        ScreenDC := GetDC( 0 );
        SrcDC := CreateCompatibleDC( ScreenDC );
        DestDC := CreateCompatibleDC( ScreenDC );
        NewItemImages := CreateCompatibleBitmap( ScreenDC, NewBitWidth, NewBitHeight );
        NewItemMasks := CreateCompatibleBitmap( DestDC, NewBitWidth, NewBitHeight );
        OldDestBM := SelectObject( DestDC, NewItemImages );
        if ( FItemImages <> 0 ) then
        begin
          OldSrcBM := SelectObject( SrcDC, FItemImages );
          BitBlt( DestDC, 0, 0, FItemBitWidth, FItemBitHeight, SrcDC, 0, 0, SRCCOPY );
          SelectObject( SrcDC, Picture );
        end
        else
          OldSrcBM := SelectObject( SrcDC, Picture );
        for i := 1 to NewItem.Strips do
          BitBlt( DestDC, X, FItemColumnBitHeight + ( i - 1 ) * NewItem.Height,
            FTileWidth, NewItem.Height, SrcDC, ( i - 1 ) * FTileWidth, 0, SRCCOPY );

        SelectObject( DestDC, NewItemMasks );
        if ( FItemMasks <> 0 ) then
        begin
          SelectObject( SrcDC, FItemMasks );
          BitBlt( DestDC, 0, 0, FItemBitWidth, FItemBitHeight, SrcDC, 0, 0, SRCCOPY );
        end;
        SelectObject( SrcDC, Mask );
        for i := 1 to NewItem.Strips do
          BitBlt( DestDC, X, FItemColumnBitHeight + ( i - 1 ) * NewItem.Height,
            FTileWidth, NewItem.Height, SrcDC, ( i - 1 ) * FTileWidth, 0, SRCCOPY );

        SelectObject( SrcDC, OldSrcBM );
        SelectObject( DestDC, OldDestBM );
        DeleteObject( FItemImages );
        DeleteObject( FItemMasks );
        DeleteObject( Picture );
        DeleteObject( Mask );
        DeleteDC( SrcDC );
        DeleteDC( DestDC );
        ReleaseDC( 0, ScreenDC );
        FItemImages := NewItemImages;
        FItemMasks := NewItemMasks;

        FItemBitWidth := NewBitWidth;
        FItemColumnBitHeight := FItemColumnBitHeight + NewItem.Strips * NewItem.Height;
        if NewBitHeight > FItemBitHeight then
          FItemBitHeight := NewBitHeight;
  {$ENDIF}
        Item[ Index ] := NewItem;
      end;
      Result := Pointer( FItemMem );
      Inc( Result, Index - 1 );
    end;

  {$IFDEF DirectX}
    procedure TZone.DefineTile( Index : Word; Image : IDirectDrawSurface; Color : TColor );
    {$ENDIF}
    {$IFNDEF DirectX}
      procedure TZone.DefineTile( Index : Word; Image, Mask : TBitmap; Color : TColor );
      {$ENDIF}
      var
        NewMaxIndex : Word;
        NewBitWidth, NewBitHeight : Longint;
        i, j, X, Y : Integer;
        NewTile : TileInfo;
        W, H : integer;
      {$IFDEF DirectX}
        NewTileImages : IDirectDrawSurface;
        ddsd : DDSurfaceDesc;
        ddck : DDCOLORKEY;
        BltFx : DDBLTFX;
        SrcX1, SrcX2, SrcY1, SrcY2 : integer;
      {$ENDIF}
      {$IFNDEF DirectX}
        DestDC : HDC;
        SrcDC : HDC;
        Picture, Mask : HBITMAP;
        ScreenDC : HDC;
        OldSrcBM, OldDestBM : HBITMAP;
        NewTileImages, NewTileMasks : HBITMAP;
      {$ENDIF}
      begin
        if Loaded then
          exit;

        if ( Image = nil ) then
        begin
          NewTile.ImageIndex := 0;
          NewTile.Rows := 0;
          NewTile.Columns := 0;
          NewTile.Element := 0;
          Tile[ Index ] := NewTile;
        end
        else
        begin
    {$IFDEF DirectX}
          GetSurfaceDims( W, H, Image );
    {$ENDIF}
    {$IFNDEF DirectX}
          W := Image.Width;
          H := Image.Height;
    {$ENDIF}
          NewTile.ImageIndex := FTileMaxIndex;
          NewTile.Columns := W div FMap.FTileWidth;
          if ( ( W mod FMap.FTileWidth ) <> 0 ) then
            Inc( NewTile.Columns );
          NewTile.Rows := H div FMap.FTileHeight;
          if ( ( H mod FMap.FTileHeight ) <> 0 ) then
            Inc( NewTile.Rows );
          NewMaxIndex := FTileMaxIndex + NewTile.Columns * NewTile.Rows;
          NewBitWidth := ( ( ( NewMaxIndex - 1 ) div FTileMaxColumnIndex ) + 1 ) * FMap.FTileWidth;
          if NewBitWidth > FMap.FTileWidth then
            NewBitHeight := MaxZoneHeight
          else
            NewBitHeight := NewMaxIndex * FMap.FTileHeight;
          X := ( ( FTileMaxIndex - 1 ) div FTileMaxColumnIndex ) * FMap.FTileWidth;
          Y := ( FTileMaxIndex mod FTileMaxColumnIndex ) * FMap.FTileHeight;
          if ( ( FTileMaxIndex mod FTileMaxColumnIndex ) = 0 ) and ( FTileMaxIndex > 0 ) then
            inc( X, FMap.FTileWidth );
          Tile[ Index ] := NewTile;
    {$IFDEF DirectX}
          if ( NewBitWidth > FTileBitWidth ) or ( NewBitHeight > FTileBitHeight ) then
          begin
//      Log.Log('Resize Zone Tiles');
//Log.Log(inttostr(NewBitWidth)+' '+inttostr(FTileBitWidth));
//Log.Log(inttostr(NewBitHeight)+' '+inttostr(FTileBitHeight));
            ddsd.dwSize := SizeOf( ddsd );
            ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
            ddsd.dwWidth := NewBitWidth;
            ddsd.dwHeight := NewBitHeight;
            if TilesInVideo then
            begin
              ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_VIDEOMEMORY;
              if lpdd.CreateSurface( ddsd, NewTileImages, nil ) <> DD_OK then
              begin
                ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
                lpdd.CreateSurface( ddsd, NewTileImages, nil );
                TilesInVideo := false;
              end;
            end
            else
            begin
              ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
              lpdd.CreateSurface( ddsd, NewTileImages, nil );
            end;

            BltFx.dwSize := SizeOf( BltFx );
            BltFx.dwFillColor := FMap.ColorMatch;
            NewTileImages.Blt( Rect( 0, 0, NewBitWidth, NewBitHeight ), nil,
              Rect( 0, 0, NewBitWidth, NewBitHeight ), DDBLT_COLORFILL + DDBLT_WAIT, BltFx );
            if Assigned( FTileImages ) then
            begin
              NewTileImages.BltFast( 0, 0, FTileImages, Rect( 0, 0, FTileBitWidth, FTileBitHeight ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
            end;

            ddck.dwColorSpaceLowValue := FMap.ColorMatch;
            ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
            NewTileImages.SetColorKey( DDCKEY_SRCBLT, ddck );

            FTileImages := nil;
            FTileImages := NewTileImages;

            if NewBitWidth > FTileBitWidth then
              FTileBitWidth := NewBitWidth;
            if NewBitHeight > FTileBitHeight then
              FTileBitHeight := NewBitHeight;
          end;

          for i := 0 to NewTile.Columns - 1 do
          begin
            for j := 0 to NewTile.Rows - 1 do
            begin
              if Y >= MaxZoneHeight then
              begin
                inc( X, FMap.FTileWidth );
                Y := 0;
              end;
              SrcX1 := i * FMap.FTileWidth;
              SrcY1 := j * FMap.FTileHeight;
              SrcX2 := SrcX1 + FMap.FTileWidth;
              SrcY2 := SrcY1 + FMap.FTileHeight;
              if SrcX2 > W then
                SrcX2 := W;
              if SrcY2 > H then
                SrcY2 := H;
              FTileImages.BltFast( X, Y, Image, Rect( SrcX1, SrcY1, SrcX2, SrcY2 ),
                DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
              inc( Y, FMap.FTileHeight );
            end;
          end;

    {$ENDIF}
    {$IFNDEF DirectX}
          if Assigned( MaskBM ) then
          begin
            Picture := BITMAP.Handle;
            Mask := MaskBM.Handle;
          end
          else
            CreateMask( Picture, Mask, BITMAP, ColorToRGB( Color ) );
          ScreenDC := GetDC( 0 );
          SrcDC := CreateCompatibleDC( ScreenDC );
          DestDC := CreateCompatibleDC( ScreenDC );
          NewTileImages := CreateCompatibleBitmap( ScreenDC, FTileBitWidth, NewBitHeight );
          NewTileMasks := CreateCompatibleBitmap( DestDC, FTileBitWidth, NewBitHeight );
          OldDestBM := SelectObject( DestDC, NewTileImages );
          if ( FTileImages <> 0 ) then
          begin
            OldSrcBM := SelectObject( SrcDC, FTileImages );
            PatBlt( DestDC, 0, FTileBitHeight, FTileBitWidth, NewBitHeight - FTileBitHeight, BLACKNESS );
            BitBlt( DestDC, 0, 0, FTileBitWidth, FTileBitHeight, SrcDC, 0, 0, SRCCOPY );
            SelectObject( SrcDC, Picture );
          end
          else
            OldSrcBM := SelectObject( SrcDC, Picture );
    //This is now messed up  (GDI only)
          for i := 1 to Tile[ Index ].Columns do
            BitBlt( DestDC, 0, FTileBitHeight + ( ( i - 1 ) * Tile[ Index ].Rows ) * FTileHeight,
              FTileBitWidth, Tile[ Index ].Rows * FTileHeight, SrcDC, ( i - 1 ) * FTileBitWidth, 0, SRCCOPY );

          SelectObject( DestDC, NewTileMasks );
          if ( FTileMasks <> 0 ) then
          begin
            SelectObject( SrcDC, FTileMasks );
            PatBlt( DestDC, 0, FTileBitHeight, FTileBitWidth, NewBitHeight - FTileBitHeight, WHITENESS );
            BitBlt( DestDC, 0, 0, FTileBitWidth, FTileBitHeight, SrcDC, 0, 0, SRCCOPY );
          end;
          SelectObject( SrcDC, Mask );
          for i := 1 to Tile[ Index ].Columns do
            BitBlt( DestDC, 0, FTileBitHeight + ( ( i - 1 ) * Tile[ Index ].Rows ) * FTileHeight,
              FTileBitWidth, Tile[ Index ].Rows * FTileHeight, SrcDC, ( i - 1 ) * FTileBitWidth, 0, SRCCOPY );

          SelectObject( SrcDC, OldSrcBM );
          SelectObject( DestDC, OldDestBM );
          DeleteObject( FTileImages );
          DeleteObject( FTileMasks );
          if not Assigned( MaskBM ) then
          begin
            DeleteObject( Picture );
            DeleteObject( Mask );
          end;
          DeleteDC( SrcDC );
          DeleteDC( DestDC );
          ReleaseDC( 0, ScreenDC );
          FTileImages := NewTileImages;
          FTileMasks := NewTileMasks;
          FTileBitWidth := NewBitWidth;
          FTileBitHeight := NewBitHeight;
    {$ENDIF}
          FTileMaxIndex := NewMaxIndex;
        end;
      end;

      procedure TZone.ExportTiles( Filename : string );
      var
        BM : TBitmap;
        DC : HDC;
      begin
        try
          if assigned( FTileImages ) then
          begin
            BM := TBitmap.create;
            try
              BM.width := FTileBitWidth;
              BM.Height := FTileBitHeight;
              FTileImages.GetDC( DC );
              BitBlt( BM.canvas.handle, 0, 0, FTileBitWidth, FTileBitHeight, DC, 0, 0, SRCCOPY );
              FTileImages.ReleaseDC( DC );
              BM.SaveToFile( Filename );
            finally
              BM.free;
            end;
          end;
        except
        end;
      end;

      procedure TZone.ExportItems( Filename : string );
      var
        BM : TBitmap;
        DC : HDC;
      begin
        try
          if assigned( FItemImages ) then
          begin
            BM := TBitmap.create;
            try
              BM.width := FItemBitWidth;
              BM.Height := FItemBitHeight;
              FItemImages.GetDC( DC );
              BitBlt( BM.canvas.handle, 0, 0, FItemBitWidth, FItemBitHeight, DC, 0, 0, SRCCOPY );
              FItemImages.ReleaseDC( DC );
              BM.SaveToFile( Filename );
            finally
              BM.free;
            end;
          end;
        except
        end;
      end;

      procedure TZone.SaveToStream( Stream : TStream; SaveImage : boolean );
      var
        ddsd : TDDSurfaceDesc;
        L : longint;
        i : integer;
        p : ^byte;
        MemSize : longint;
        P0, P1 : longint;
      begin
//  Log.Log('Saving tiles '+IntToStr(FTileBitWidth)+'x'+IntToStr(FTileBitHeight));
        Stream.write( FTileBitWidth, sizeof( FTileBitWidth ) );
        Stream.write( FTileBitHeight, sizeof( FTileBitHeight ) );
        L := 0;
        Stream.write( L, sizeof( L ) );

        if SaveImage and ( FTileBitWidth > 0 ) and ( FTileBitHeight > 0 ) then
        begin
          ddsd.dwSize := SizeOf( ddsd );
          if FTileImages.Lock( nil, ddsd, DDLOCK_WAIT, 0 ) = DD_OK then
          begin
            try
              P0 := Stream.position;
              SaveTileCustomData( Stream );
              MemSize := FTileBitWidth * 2;
              L := MemSize * FTileBitHeight + Stream.Position - P0;
              P1 := Stream.Position;
              Stream.Position := P0 - sizeof( L );
              Stream.write( L, sizeof( L ) );
              Stream.Position := P1;
              p := ddsd.lpSurface;
              for i := 1 to FTileBitHeight do
              begin
                Stream.Write( p^, MemSize );
                inc( p, ddsd.lPitch );
              end;
            finally
              FTileImages.Unlock( nil );
            end;
          end;
        end;

//  Log.Log('Saving items '+IntToStr(FItembitWidth)+'x'+IntToStr(FItemBitHeight));
        Stream.write( FItemBitWidth, sizeof( FItembitWidth ) );
        Stream.write( FItemBitHeight, sizeof( FItemBitHeight ) );
        L := 0;
        Stream.write( L, sizeof( L ) );

        if SaveImage and ( FItemBitWidth > 0 ) and ( FItemBitHeight > 0 ) then
        begin
          ddsd.dwSize := SizeOf( ddsd );
          if FItemImages.Lock( nil, ddsd, DDLOCK_WAIT, 0 ) = DD_OK then
          begin
            try
              P0 := Stream.position;
              SaveItemCustomData( Stream );
              MemSize := FItembitWidth * 2;
              L := MemSize * FItemBitHeight + Stream.Position - P0;
              P1 := Stream.Position;
              Stream.Position := P0 - sizeof( L );
              Stream.write( L, sizeof( L ) );
              Stream.Position := P1;
              p := ddsd.lpSurface;
              for i := 1 to FItemBitHeight do
              begin
                Stream.Write( p^, MemSize );
                inc( p, ddsd.lPitch );
              end;
            finally
              FItemImages.Unlock( nil );
            end;
          end
        end;
      end;

      class procedure TZone.Skip( Stream : TStream );
      var
        L : longint;
      begin
        Stream.read( L, sizeof( L ) );
        Stream.read( L, sizeof( L ) );
        Stream.read( L, sizeof( L ) );
        Stream.seek( L, soFromCurrent );
        Stream.read( L, sizeof( L ) );
        Stream.read( L, sizeof( L ) );
        Stream.read( L, sizeof( L ) );
        Stream.seek( L, soFromCurrent );
      end;

      procedure TZone.LoadTileCustomData( Stream : TStream );
      begin
        Stream.read( FTileMaxIndex, sizeof( FTileMaxIndex ) );
        Stream.read( FTileMaxColumnIndex, sizeof( FTileMaxColumnIndex ) );
        Stream.read( X1, sizeof( X1 ) );
        Stream.read( Y1, sizeof( Y1 ) );
        Stream.read( X2, sizeof( X2 ) );
        Stream.read( Y2, sizeof( Y2 ) );
      end;

      procedure TZone.SaveTileCustomData( Stream : TStream );
      begin
        Stream.write( FTileMaxIndex, sizeof( FTileMaxIndex ) );
        Stream.write( FTileMaxColumnIndex, sizeof( FTileMaxColumnIndex ) );
        Stream.write( X1, sizeof( X1 ) );
        Stream.write( Y1, sizeof( Y1 ) );
        Stream.write( X2, sizeof( X2 ) );
        Stream.write( Y2, sizeof( Y2 ) );
      end;

      procedure TZone.LoadItemCustomData( Stream : TStream );
      begin
        Stream.read( FItemColumn, sizeof( FItemColumn ) );
        Stream.read( FItemColumnBitHeight, sizeof( FItemColumnBitHeight ) );
      end;

      procedure TZone.SaveItemCustomData( Stream : TStream );
      begin
        Stream.write( FItemColumn, sizeof( FItemColumn ) );
        Stream.write( FItemColumnBitHeight, sizeof( FItemColumnBitHeight ) );
      end;

      procedure TZone.LoadFromStream( Stream : TStream );
      var
        L : longint;
        ddsd : DDSurfaceDesc;
        ddck : DDCOLORKEY;
        BltFx : DDBLTFX;
        i : integer;
        p : ^byte;
        MemSize : longint;
      begin
        if Loaded then
          exit;
        Stream.read( FTileBitWidth, sizeof( FTileBitWidth ) );
        Stream.read( FTileBitHeight, sizeof( FTileBitHeight ) );
        Stream.read( L, sizeof( L ) );
//  Log.Log('Sizing tiles to '+IntToStr(FTileBitWidth)+'x'+IntToStr(FTileBitHeight));

        if ( FTileBitWidth > 0 ) and ( FTileBitHeight > 0 ) then
        begin
          FTileImages := nil;
          ddsd.dwSize := SizeOf( ddsd );
          ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
          ddsd.dwWidth := FTileBitWidth;
          ddsd.dwHeight := FTileBitHeight;
{    if TilesInVideo then begin
      ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_VIDEOMEMORY;
      if lpdd.CreateSurface(ddsd, FTileImages, nil) <> DD_OK then begin
        ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
        lpdd.CreateSurface(ddsd, FTileImages, nil);
        TilesInVideo:=false;
      end;
    end
    else begin   }
          ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
          lpdd.CreateSurface( ddsd, FTileImages, nil );
//    end;

          BltFx.dwSize := SizeOf( BltFx );
          BltFx.dwFillColor := FMap.ColorMatch;
          FTileImages.Blt( Rect( 0, 0, FTileBitWidth, FTileBitHeight ), nil,
            Rect( 0, 0, FTileBitWidth, FTileBitHeight ), DDBLT_COLORFILL + DDBLT_WAIT, BltFx );

          ddck.dwColorSpaceLowValue := FMap.ColorMatch;
          ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
          FTileImages.SetColorKey( DDCKEY_SRCBLT, ddck );
        end;

        if L > 0 then
        begin
          LoadTileCustomData( Stream );
          if Assigned( FTileImages ) then
          begin
            ddsd.dwSize := SizeOf( ddsd );
            if FTileImages.Lock( nil, ddsd, DDLOCK_WAIT, 0 ) = DD_OK then
            begin
              try
//          Log.Log('Loading tiles from cache (zone '+inttostr(index)+')');
                MemSize := FTileBitWidth * 2;
                p := ddsd.lpSurface;
                for i := 1 to FTileBitHeight do
                begin
                  Stream.read( p^, MemSize );
                  inc( p, ddsd.lPitch );
                end;
              finally
                FTileImages.Unlock( nil );
              end;
            end;
            Loaded := true;
          end;
        end;

        Stream.read( FItemBitWidth, sizeof( FItemBitWidth ) );
        Stream.read( FItemBitHeight, sizeof( FItemBitHeight ) );
        Stream.read( L, sizeof( L ) );
//  Log.Log('Sizing items to '+IntToStr(FItemBitWidth)+'x'+IntToStr(FItemBitHeight));

        if ( FItemBitWidth > 0 ) and ( FItemBitHeight > 0 ) then
        begin
          FItemImages := nil;
          ddsd.dwSize := SizeOf( ddsd );
          ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
          ddsd.dwWidth := FItemBitWidth;
          ddsd.dwHeight := FItemBitHeight;
{    if ItemsInVideo then begin
      ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_VIDEOMEMORY;
      if lpdd.CreateSurface(ddsd, FItemImages, nil) <> DD_OK then begin
        ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
        lpdd.CreateSurface(ddsd, FItemImages, nil);
        ItemsInVideo:=false;
      end;
    end
    else begin  }
          ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
          lpdd.CreateSurface( ddsd, FItemImages, nil );
//    end;

          ddck.dwColorSpaceLowValue := FMap.ColorMatch;
          ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
          FItemImages.SetColorKey( DDCKEY_SRCBLT, ddck );
        end;

        if L > 0 then
        begin
          LoadItemCustomData( Stream );
          if assigned( FItemImages ) then
          begin
            ddsd.dwSize := SizeOf( ddsd );
            if FItemImages.Lock( nil, ddsd, DDLOCK_WAIT, 0 ) = DD_OK then
            begin
              try
//          Log.Log('Loading items from cache (zone '+inttostr(index)+')');
                MemSize := FItembitWidth * 2;
                p := ddsd.lpSurface;
                for i := 1 to FItemBitHeight do
                begin
                  Stream.read( p^, MemSize );
                  inc( p, ddsd.lPitch );
                end;
              finally
                FItemImages.Unlock( nil );
              end;
            end;
            Loaded := true;
          end;
        end;
        FullRefresh := TilesInVideo and ItemsInVideo;
      end;

    {$IFDEF DirectX}

      procedure TZone.MoveToVideo;
      var
        NewTileImages : IDirectDrawSurface;
        NewItemImages : IDirectDrawSurface;
        ddsd : DDSurfaceDesc;
        ddck : DDCOLORKEY;
        i, Rem : integer;
        L : longword;
      const
        SectionHeight = 256;
      begin
        if not ItemsInVideo then
        begin
          if FItemBitHeight = 0 then
          begin
            ItemsInVideo := True;
          end
          else
          begin
            ddsd.dwSize := SizeOf( ddsd );
            ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
            ddsd.dwWidth := FItemBitWidth;
            ddsd.dwHeight := FItemBitHeight;
            ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_VIDEOMEMORY;
            if lpdd.CreateSurface( ddsd, NewItemImages, nil ) = DD_OK then
            begin
              for i := 0 to FItemBitHeight div SectionHeight - 1 do
              begin
                NewItemImages.BltFast( 0, i * SectionHeight, FItemImages, Rect( 0, i * SectionHeight, FItemBitWidth, ( i + 1 ) * SectionHeight ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
              end;
              Rem := FItemBitHeight mod SectionHeight;
              if Rem > 0 then
              begin
                NewItemImages.BltFast( 0, FItemBitHeight - Rem, FItemImages, Rect( 0, FItemBitHeight - Rem, FItemBitWidth, FItemBitHeight ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
              end;

              FItemImages := nil;
              FItemImages := NewItemImages;
              ddck.dwColorSpaceLowValue := FMap.ColorMatch;
              ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
              FItemImages.SetColorKey( DDCKEY_SRCBLT, ddck );
              ItemsInVideo := True;
            end
            else
            begin
              if not ( self is TLightZone ) then
              begin //Poke the zone to reduce stuttering
                if FItemImages.Lock( nil, ddsd, DDLOCK_WAIT, 0 ) = DD_OK then
                begin
                  L := longint( ddsd.lpSurface^ );
                  longint( ddsd.lpSurface^ ) := L;
                  FItemImages.Unlock( nil );
                end;
              end;
            end;
          end;
          if ItemsInVideo then
            Log.Log( 'Zone ' + IntToStr( Index ) + ' items moved to video' )
          else
            Log.Log( 'Zone ' + IntToStr( Index ) + ' items could not be moved to video' );
        end;

        if not TilesInVideo then
        begin
          if ( FTileBitHeight = 0 ) then
          begin
            TilesInVideo := true;
          end
          else
          begin
            ddsd.dwSize := SizeOf( ddsd );
            ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
            ddsd.dwWidth := FTileBitWidth;
            ddsd.dwHeight := FTileBitHeight;
            ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_VIDEOMEMORY;
            if lpdd.CreateSurface( ddsd, NewTileImages, nil ) = DD_OK then
            begin
              for i := 0 to FTileBitHeight div SectionHeight - 1 do
              begin
                NewTileImages.BltFast( 0, i * SectionHeight, FTileImages, Rect( 0, i * SectionHeight, FTileBitWidth, ( i + 1 ) * SectionHeight ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
              end;
              Rem := FTileBitHeight mod SectionHeight;
              if Rem > 0 then
              begin
                NewTileImages.BltFast( 0, FTileBitHeight - Rem, FTileImages, Rect( 0, FTileBitHeight - Rem, FTileBitWidth, FTileBitHeight ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
              end;

              FTileImages := nil;
              FTileImages := NewTileImages;
              ddck.dwColorSpaceLowValue := FMap.ColorMatch;
              ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
              FTileImages.SetColorKey( DDCKEY_SRCBLT, ddck );
              TilesInVideo := true;
            end
            else
            begin
              if not ( self is TLightZone ) then
              begin //Poke the zone to reduce stuttering
                if FTileImages.Lock( nil, ddsd, DDLOCK_WAIT, 0 ) = DD_OK then
                begin
                  L := longint( ddsd.lpSurface^ );
                  longint( ddsd.lpSurface^ ) := L;
                  FTileImages.Unlock( nil );
                end;
              end;
            end;
          end;
          if TilesInVideo then
            Log.Log( 'Zone ' + IntToStr( Index ) + ' tiles moved to video' )
          else
            Log.Log( 'Zone ' + IntToStr( Index ) + ' tiles could not be moved to video' );
        end;
      end;
    {$ENDIF}

{ TLightZone }

      constructor TLightZone.Create( Map : TAniMap );
      begin
        inherited;
        FItemBitWidth := Map.FStripWidth;
      end;

    {$IFDEF DirectX}
      procedure TLightZone.AddStrip( Image : IDirectDrawSurface; var NewX, NewY : word );
      {$ENDIF}
      {$IFNDEF DirectX}
        procedure TLightZone.AddStrip( Image, Mask : TBitmap; var NewX, NewY : word );
        {$ENDIF}
        var
          NewBitWidth, NewBitHeight : Longint;
          W, H : integer;
        {$IFDEF DirectX}
          NewItemImages : IDirectDrawSurface;
          ddsd : DDSurfaceDesc;
          ddck : DDCOLORKEY;
//  BltFx: DDBLTFX;
        {$ENDIF}
        {$IFNDEF DirectX}
          ScreenDC, SrcDC, DestDC : HDC;
          NewItemImages, NewItemMasks : HBITMAP;
          OldSrcBM, OldDestBM : HBITMAP;
        {$ENDIF}
        begin
          if ( Image = nil ) then
            Exit;
      {$IFDEF DirectX}
          GetSurfaceDims( W, H, Image );
      {$ENDIF}
      {$IFNDEF DirectX}
          W := Image.Width;
          H := Image.Height;
      {$ENDIF}
          if AddColumn or ( FItemColumnBitHeight + H > MaxZoneHeight ) then
          begin
            AddColumn := false;
            inc( FItemColumn );
            NewBitWidth := ( FItemColumn + 1 ) * FMap.FStripWidth;
            FItemColumnBitHeight := 0;
            NewBitHeight := H;
          end
          else
          begin
            NewBitWidth := FItemBitWidth;
            NewBitHeight := FItemColumnBitHeight + H;
          end;
          if NewBitWidth < FItemBitWidth then
            NewBitWidth := FItemBitWidth;
          if NewBitHeight < FItemBitHeight then
            NewBitHeight := FItemBitHeight;

          NewX := FItemColumn * FMap.FStripWidth;
          NewY := FItemColumnBitHeight;

      {$IFDEF DirectX}
          if ( NewBitHeight > FItemBitHeight ) or ( NewBitWidth > FItemBitWidth ) then
          begin
            ddsd.dwSize := SizeOf( ddsd );
            ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
            ddsd.dwWidth := NewBitWidth;
            ddsd.dwHeight := NewBitHeight;
            ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
            lpdd.CreateSurface( ddsd, NewItemImages, nil );

  {  BltFx.dwSize := SizeOf(BltFx);
    BltFx.dwFillColor := FMap.ColorMatch;
    NewItemImages.Blt(Rect(0, 0, NewBitWidth, NewBitHeight), nil,
      Rect(0, 0, NewBitWidth, NewBitHeight), DDBLT_COLORFILL + DDBLT_WAIT, BltFx);  }

            if Assigned( FItemImages ) then
            begin
              NewItemImages.BltFast( 0, 0, FItemImages, Rect( 0, 0, FItemBitWidth, FItemBitHeight ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
            end;
            NewItemImages.BltFast( NewX, NewY, Image, Rect( 0, 0, W, H ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );

            ddck.dwColorSpaceLowValue := FMap.ColorMatch;
            ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
            NewItemImages.SetColorKey( DDCKEY_SRCBLT, ddck );

            FItemImages := nil;
            FItemImages := NewItemImages;
            if NewBitWidth > FItemBitWidth then
              FItemBitWidth := NewBitWidth;
            FItemBitHeight := NewBitHeight;
          end
          else
          begin
            FItemImages.BltFast( NewX, NewY, Image, Rect( 0, 0, W, H ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
          end;
      {$ENDIF}
      {$IFNDEF DirectX}
          ScreenDC := GetDC( 0 );
          SrcDC := CreateCompatibleDC( ScreenDC );
          DestDC := CreateCompatibleDC( ScreenDC );
          NewItemImages := CreateCompatibleBitmap( ScreenDC, FItemBitWidth, NewBitHeight );
          ReleaseDC( 0, ScreenDC );
          NewItemMasks := CreateCompatibleBitmap( DestDC, FItemBitWidth, NewBitHeight );
          OldDestBM := SelectObject( DestDC, NewItemImages );
          if ( FItemImages <> 0 ) then
          begin
            OldSrcBM := SelectObject( SrcDC, FItemImages );
            BitBlt( DestDC, 0, 0, FItemBitWidth, FItemBitHeight, SrcDC, 0, 0, SRCCOPY );
            SelectObject( SrcDC, OldSrcBM );
          end;
          BitBlt( DestDC, NewX, NewY, FStripWidth, BITMAP.Height, BITMAP.Canvas.Handle, 0, 0, SRCCOPY );

          SelectObject( DestDC, NewItemMasks );
          if ( FItemMasks <> 0 ) then
          begin
            OldSrcBM := SelectObject( SrcDC, FItemMasks );
            BitBlt( DestDC, 0, 0, FItemBitWidth, FItemBitHeight, SrcDC, 0, 0, SRCCOPY );
            SelectObject( SrcDC, OldSrcBM );
          end;
          DeleteDC( SrcDC );
          BitBlt( DestDC, NewX, NewY, FStripWidth, BITMAP.Height, Mask.Canvas.Handle, 0, 0, SRCCOPY );

          SelectObject( DestDC, OldDestBM );
          DeleteDC( DestDC );
          DeleteObject( FItemImages );
          DeleteObject( FItemMasks );
          FItemImages := NewItemImages;
          FItemMasks := NewItemMasks;
          FItemBitWidth := NewBitWidth;
          FItemBitHeight := NewBitHeight;
      {$ENDIF}

          inc( FItemColumnBitHeight, H );
        end;

        procedure TLightZone.NewTileState;
        begin
          if TileStateOffset = 0 then
          begin
            TileStateOffset := FTileMaxIndex;
          end;
        end;

        procedure TLightZone.NewItemState;
        begin
          AddColumn := true;
          if ItemStateOffset = 0 then
          begin
            ItemStateOffset := FItemBitWidth;
          end;
        end;

        destructor TLightZone.Destroy;
        begin
          OverlapZones.Free;
          Items.Free;
          inherited;
        end;

        procedure TLightZone.LoadTileCustomData( Stream : TStream );
        var
          i, L : longint;
        begin
          if assigned( OverlapZones ) then
          begin
            OverlapZones.free;
            OverlapZones := nil;
          end;
          Stream.Read( L, sizeof( L ) );
          if L > 0 then
          begin
            OverlapZones := TList.create;
            OverlapZones.capacity := L;
            for i := 0 to L - 1 do
            begin
              Stream.Read( L, sizeof( L ) );
              OverlapZones.add( FMap.Zones[ L ] );
            end;
          end;

          if assigned( Items ) then
          begin
            Items.free;
            Items := nil;
          end;
          Stream.Read( L, sizeof( L ) );
          if L > 0 then
          begin
            Items := TList.create;
            Items.capacity := L;
            for i := 0 to L - 1 do
            begin
              Stream.Read( L, sizeof( L ) );
              Items.add( @FMap.FItemList[ L ] );
            end;
          end;

          Stream.Read( States, sizeof( States ) );
          Stream.Read( State, sizeof( State ) );
          Stream.Read( TileStateOffset, sizeof( TileStateOffset ) );
          Stream.Read( ItemStateOffset, sizeof( ItemStateOffset ) );
          Stream.Read( FlickerX, sizeof( FlickerX ) );
          Stream.Read( FlickerY, sizeof( FlickerY ) );
          Stream.Read( FlickerZ, sizeof( FlickerZ ) );
          Stream.Read( FlickerRadius, sizeof( FlickerRadius ) );
          Stream.Read( FlickerIntensity, sizeof( FlickerIntensity ) );
          Stream.Read( Color, sizeof( Color ) );
          Stream.Read( Intensity, sizeof( Intensity ) );
          Stream.Read( Radius, sizeof( Radius ) );
          Stream.Read( Flicker, sizeof( Flicker ) );
          Stream.Read( X, sizeof( X ) );
          Stream.Read( Y, sizeof( Y ) );
          Stream.Read( Z, sizeof( Z ) );
          if Flicker <> flNone then
          begin
            TilesInVideo := true;
            ItemsInVideo := true;
          end;

          inherited;
        end;

        procedure TLightZone.SaveTileCustomData( Stream : TStream );
        var
          i, L : longint;
        begin
          if assigned( OverlapZones ) then
          begin
            L := OverlapZones.count;
            Stream.write( L, sizeof( L ) );
            for i := 0 to L - 1 do
            begin
              L := TZone( OverlapZones.items[ i ] ).Index;
              Stream.write( L, sizeof( L ) );
            end;
          end
          else
          begin
            L := 0;
            Stream.write( L, sizeof( L ) );
          end;

          if assigned( Items ) then
          begin
            L := Items.count;
            Stream.write( L, sizeof( L ) );
            for i := 0 to L - 1 do
            begin
              L := FMap.GetItemIndex( PItemInstanceInfo( Items.items[ i ] ) );
              Stream.write( L, sizeof( L ) );
            end;
          end
          else
          begin
            L := 0;
            Stream.write( L, sizeof( L ) );
          end;

          Stream.write( States, sizeof( States ) );
          Stream.write( State, sizeof( State ) );
          Stream.write( TileStateOffset, sizeof( TileStateOffset ) );
          Stream.write( ItemStateOffset, sizeof( ItemStateOffset ) );
          Stream.write( FlickerX, sizeof( FlickerX ) );
          Stream.write( FlickerY, sizeof( FlickerY ) );
          Stream.write( FlickerZ, sizeof( FlickerZ ) );
          Stream.write( FlickerRadius, sizeof( FlickerRadius ) );
          Stream.write( FlickerIntensity, sizeof( FlickerIntensity ) );
          Stream.write( Color, sizeof( Color ) );
          Stream.write( Intensity, sizeof( Intensity ) );
          Stream.write( Radius, sizeof( Radius ) );
          Stream.write( Flicker, sizeof( Flicker ) );
          Stream.write( X, sizeof( X ) );
          Stream.write( Y, sizeof( Y ) );
          Stream.write( Z, sizeof( Z ) );

          inherited;
        end;

        procedure TLightZone.LoadItemCustomData( Stream : TStream );
        begin
          inherited;
        end;

        procedure TLightZone.SaveItemCustomData( Stream : TStream );
        begin
          inherited;
        end;

{ TAniFigure }

        constructor TAniFigure.Create( X, Y,
          Z : Integer; Frame : Word; Enabled : Boolean );
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
            GlobalFree( PathHandle );
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
            GlobalFree( PathHandle );
            PathHandle := 0;
          end;
          FPathDestX := X;
          FPathDestY := Y;
          FSlopeZ := 0;
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
            GlobalFree( PathHandle );
            PathHandle := 0;
          end;
          FDestX := X;
          FDestY := Y;
          FDestZ := Z;
          Dx := DestX - FX;
          dy := 2 * ( DestY - FY );
          dZ := DestZ - FZ;
          D := sqrt( sqr( Dx ) + sqr( dy ) + sqr( dZ ) );
          if D <> 0 then
          begin
            FSlopeX := Dx / D;
            FSlopeY := dy / ( 2 * D );
            FSlopeZ := dZ / D;
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
            GlobalFree( PathHandle );
            PathHandle := 0;
          end;
          FDestX := X;
          FDestY := Y;
          FDestZ := Z;
          Dx := DestX - FX;
          dy := 2 * ( DestY - FY );
          dZ := DestZ - FZ;
          D := sqrt( sqr( Dx ) + sqr( dy ) + sqr( dZ ) );
          if D <> 0 then
          begin
            FSlopeX := Dx / D;
            FSlopeY := dy / ( 2 * D );
            FSlopeZ := dZ / D;
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
            i := Resource.Scripts.IndexOf( Name );
            if i >= 0 then
            begin
              if ( FrameMultiplier <> TScript( Resource.Scripts.Objects[ i ] ).Multiplier ) or ( i <> FScriptIndex ) then
              begin
                Delay := -1;
                FrameMultiplier := TScript( Resource.Scripts.Objects[ i ] ).Multiplier;
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
            i := Resource.Scripts.IndexOf( Name );
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

        procedure TAniFigure.Render;
        begin
          if Assigned( Resource ) then
            Resource.Render( Self );
        end;

        procedure TAniFigure.EnumLightSource( Index, X, Y, Z : longint; Intensity : double; Radius : integer );
        begin
          if Assigned( Resource ) then
            Resource.EnumLightSource( Self, Index, X, Y, Z, Intensity, Radius );
        end;

        procedure TAniFigure.DoFrame;
        begin

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
            GlobalFree( PathHandle );
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

        procedure TAniFigure.SetResource( const Value : TAniResource );
        begin
          FResource := Value;
        end;

        procedure TAniFigure.Stop;
        begin
          Moving := False;
          Terminal := False;
          NeedPath := False;
          GotPath := False;
          if ( PathHandle <> 0 ) then
          begin
            GlobalFree( PathHandle );
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
              FFrame := TScript( Resource.Scripts.Objects[ FScriptIndex ] ).FrameID[ FScriptFrame ];
            end
            else if ( Delay >= FrameMultiplier ) then
            begin
              Delay := 0;
              if ( PlayMode = smRandom ) then
              begin
                FScriptFrame := Random( TScript( Resource.Scripts.Objects[ FScriptIndex ] ).Frames ) + 1;
                FFrame := TScript( Resource.Scripts.Objects[ FScriptIndex ] ).FrameID[ FScriptFrame ];
              end
              else
              begin
                if ( FScriptFrame < TScript( Resource.Scripts.Objects[ FScriptIndex ] ).Frames ) then
                begin
                  Inc( FScriptFrame );
                  FFrame := TScript( Resource.Scripts.Objects[ FScriptIndex ] ).FrameID[ FScriptFrame ];
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
                    FFrame := TScript( Resource.Scripts.Objects[ FScriptIndex ] ).FrameID[ 1 ];
                  end
                  else
                  begin
                    FFrame := TScript( Resource.Scripts.Objects[ FScriptIndex ] ).FrameID[ TScript( Resource.Scripts.Objects[ FScriptIndex ] ).Frames ];
                    ScriptTerminated := true;
                    if Assigned( OnScriptEnd ) then
                      OnScriptEnd( Self );
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
          i : Integer;
        begin
          for i := 0 to Scripts.Count - 1 do
            TScript( Scripts.Objects[ i ] ).Free;
          Scripts.Free;
          FreeResources;
          inherited;
        end;

        procedure TAniResource.EnumLightSource( Figure : TAniFigure; Index, X, Y, Z : longint; Intensity : double; Radius : integer );
        begin

        end;

        function TAniResource.AddScript( const Name : string; Script : TScript ) : Integer;
        begin
          Result := Scripts.Add( Name );
          Scripts.Objects[ Result ] := Script;
        end;

        function TAniResource.GetScript( const Name : string ) : TScript;
        var
          i : Integer;
        begin
          i := Scripts.IndexOf( Name );
          if i >= 0 then
            Result := TScript( Scripts.Objects[ i ] )
          else
            Result := nil;
        end;

{ TImageSheet }

        constructor TImageSheet.Create;
        begin
          inherited;
        end;

        procedure TImageSheet.Draw( Canvas : TCanvas; X, Y : Integer; Frame : Word );
        begin

        end;

        procedure TImageSheet.FreeResources;
        begin
      {$IFDEF DirectX}
          Picture := nil;
      {$ENDIF}
      {$IFNDEF DirectX}
          DeleteObject( Picture );
          Picture := 0;
          DeleteObject( Mask );
          Mask := 0;
      {$ENDIF}
        end;

        function TImageSheet.GetFrames : Longint;
        begin
          Result := FramesWide * FramesHigh;
        end;

        procedure TImageSheet.Render( Figure : TAniFigure );
        var
          SrcX, SrcY : Longint;
        {$IFDEF DirectX}
          SrcX1, SrcY1, SrcX2, SrcY2 : Integer;
          DstX1, DstY1, DstX2, DstY2 : Integer;
        {$ENDIF}
        begin
          if ( Figure.Frame = 0 ) or not Figure.Visible then
            Exit;

          SrcX := FrameWidth * ( ( Figure.Frame - 1 ) mod FramesWide );
          SrcY := FrameHeight * ( ( Figure.Frame - 1 ) div FramesWide );

      {$IFDEF DirectX}
          SrcX1 := SrcX;
          SrcX2 := SrcX1 + Figure.Width;
          DstX1 := Figure.View.Left + Figure.PosX;
          DstX2 := DstX1 + Figure.Width;
          Clip( Figure.View.Left, Figure.View.Left + Figure.View.Width, DstX1, DstX2, SrcX1, SrcX2 );
          SrcY1 := SrcY;
          SrcY2 := SrcY1 + Figure.Height;
          DstY1 := Figure.View.Top + Figure.PosY;
          DstY2 := DstY1 + Figure.Height;
          Clip( Figure.View.Top, Figure.View.Top + Figure.View.Height, DstY1, DstY2, SrcY1, SrcY2 );
          lpDDSBack.BltFast( DstX1, DstY1, Picture,
            Rect( SrcX1, SrcY1, SrcX2, SrcY2 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      {$ENDIF}
      {$IFNDEF DirectX}
          SelectObject( Figure.FView.TempDC, Mask );
          BitBlt( Figure.FView.FrameBuffer.Canvas.Handle, Figure.PosX, Figure.PosY, Figure.Width, Figure.Height,
            Figure.FView.TempDC, SrcX, SrcY, SRCAND );
          SelectObject( Figure.FView.TempDC, Picture );
          BitBlt( Figure.FView.FrameBuffer.Canvas.Handle, Figure.PosX, Figure.PosY, Figure.Width, Figure.Height,
            Figure.FView.TempDC, SrcX, SrcY, SRCPAINT );
          SelectObject( Figure.FView.TempDC, Figure.FView.OldTempBitmap );
      {$ENDIF}
        end;

      {$IFDEF DirectX}

        procedure TImageSheet.SetImage( const Value : IDirectDrawSurface );
        {$ENDIF}
        {$IFNDEF DirectX}
          procedure TImageSheet.SetImage( const Value : TBitmap );
          {$ENDIF}
          begin
        {$IFDEF DirectX}
            Picture := nil;
            Picture := Value;
        {$ENDIF}
        {$IFNDEF DirectX}
            if not Assigned( Value ) then
            begin
              DeleteObject( Picture );
              Picture := 0;
              DeleteObject( Mask );
              Mask := 0;
            end
            else
            begin
              CreateMask( Picture, Mask, Value, ColorToRGB( TransparentColor ) );
            end;
        {$ENDIF}
          end;

end.
