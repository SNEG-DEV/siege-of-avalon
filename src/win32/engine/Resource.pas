unit Resource;
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
  System.Types,
  System.SysUtils,
  System.IOUtils,
  System.UITypes,
//  Vcl.Graphics,
  SoAOS.Types,
  SoAOS.Animation,
  SoAOS.Map,
  System.IniFiles,
//  Winapi.DirectDraw,
  DirectX,
  DXUtil,
  DXEffects,
  DFX,
  SoAOS.Graphics.Types,
  LogFile;

type
  TDynamicSmallIntArray = TArray<SmallInt>;

  TSlot = ( slLeg1, slBoot, slLeg2, slChest1, slChest2, slArm, slBelt, slChest3,
    slGauntlet, slOuter, slHelmet, slWeapon, slShield, slTabar, slMisc1, slMisc2, slMisc3 );

  TSlotAllowed = set of TSlot;

  TLightSource = record
    X, Y, Z : longint;
    Intensity : double;
    Radius : integer;
  end;

  TStringIniFile = class( TCustomIniFile )
  private
    FSections : TStringList;
    FData : AnsiString;
    function AddSection( const Section : string ) : TStrings;
    procedure LoadValues;
  public
    FileName : string;
    constructor Create( const Data : AnsiString );
    destructor Destroy; override;
    procedure Clear;
    procedure DeleteKey( const Section, Ident : string ); override;
    procedure EraseSection( const Section : string ); override;
    procedure GetStrings( List : TStrings );
    procedure ReadSection( const Section : string; Strings : TStrings ); override;
    procedure ReadSections( Strings : TStrings ); override;
    procedure ReadSectionValues( const Section : string; Strings : TStrings ); override;
    function ReadString( const Section, Ident, Default : string ) : string; override;
    procedure SetStrings( List : TStrings );
    procedure UpdateFile; override;
    procedure WriteString( const Section, Ident, Value : string ); override;
    property Data : AnsiString read FData;
  end;

  TResource = class( TAniResource )
  private
    FScriptMax : Integer;
    Picture : TBitPlane;
    Lights : array[ 1..8 ] of TLightSource;
    LightCount : integer;
    FReload : boolean;
    procedure LoadAction( INI : TStringIniFile; const Action : AnsiString );
    procedure LoadScript( const S, Name : string; Multiplier : Word );
    procedure SetReload( Value : boolean ); virtual;
    function GetFrameCount : integer;
  public
    Loaded : Boolean;
    FrameWidth : Integer;
    FrameHeight : Integer;
    FrameMultiplier : Integer;
    SpecialEffect : TAniSpecialEffect;
    Radius : Integer;
    CenterX : Integer;
    CenterY : Integer;
    Speed : Single;
    RunSpeed : Single;
    DeathSlide : Boolean;
    Alpha : Integer;
    UseLighting : Boolean;
    Vertical : Boolean;
    Highlightable : Boolean;
    DrawShadow : boolean;
    ComplexShadow : boolean;
    ShadowColor : TColor;
    RLE : TRLESprite;
    OnDemand : boolean;
    Filename : String;
    procedure EnumLightSource( Figure : TAniFigure; Index, X, Y, Z : longint; Intensity : double; Radius : integer ); override;
    procedure LoadData( INI : TStringINIFile ); virtual;
//    procedure Draw( Canvas : TCanvas; X, Y : Integer; Frame : Word ); override;
    procedure FreeResources; override;
    procedure Render( Figure : TAniFigure ); override;
    procedure RenderLocked( Figure : TAniFigure; Bits : PBITPLANE ); virtual;
    function MemSize : longint; virtual;
    procedure LoadGraphic;
    property ScriptMax : Integer read FScriptMax;
    property FrameCount : integer read GetFrameCount;
    property Reload : boolean read FReload write SetReload;
  end;

  TLayerResource = class( TResource )
  private
    LinkPath : string;
    procedure SetReload( Value : boolean ); override;
  public
    ItemFrame : integer;
    LinkedResource : TResource;
    BackLayer : array[ 0..383 ] of boolean;
    procedure RenderLocked( Figure : TAniFigure; Bits : PBITPLANE ); override;
    procedure LoadData( INI : TStringINIFile ); override;
  end;

  TInventoryResource = class( TResource )
  public
    procedure LoadData( INI : TStringINIFile ); override;
    procedure RenderLocked( Figure : TAniFigure; Bits : PBITPLANE ); override;
    procedure RenderShadowLocked( Figure : TAniFigure; Bits : PBITPLANE );
  end;

  TCastResource = class( TResource )
  public
    procedure RenderLocked( Figure : TAniFigure; Bits : PBITPLANE ); override;
  end;

  TCharacterResource = class( TResource )
  private
    FContactFrame : Integer;
    FReleaseFrame : Integer;
    FCastFrame : Integer;
    FAttackVariations : integer;
    procedure SetReload( Value : boolean ); override;
  public
    Defaults : TStringList;
    Layered : boolean;
    NakedName : AnsiString;
    HeadName : AnsiString;
    NakedResource : TLayerResource;
    HeadResource : TLayerResource;
    UseCastAnimation : Boolean;
    UseDefaultPants : ^TLayerResource;
    Female : boolean;
    Equipment : array[ slLeg1..slMisc3 ] of AnsiString;
    constructor Create;
    destructor Destroy; override;
    procedure Render( Figure : TAniFigure ); override;
    procedure RenderLocked( Figure : TAniFigure; Bits : PBITPLANE ); override;
    procedure LoadData( INI : TStringINIFile ); override;
    procedure FreeResources; override;
    property ContactFrame : Integer read FContactFrame;
    property ReleaseFrame : Integer read FReleaseFrame;
    property CastFrame : Integer read FCastFrame;
    property AttackVariations : Integer read FAttackVariations;
  end;

  TProjectileResource = class( TResource )
  private
    FContactFrame : Integer;
  public
    procedure LoadData( INI : TStringINIFile ); override;
    property ContactFrame : Integer read FContactFrame;
  end;

  TStaticResource = class( TResource )
  private
    Data : AnsiString;
    function GetImage( ImageIndex : Integer ) : IDirectDrawSurface;
    procedure GetImage1( ImageIndex : Integer; Surface : IDirectDrawSurface; W : integer );
  public
    procedure LoadData( INI : TStringINIFile ); override;
    function Define( Map : TAniMap; Zone : byte; Index : word ) : integer; virtual;
    property Image[ ImageIndex : Integer ] : IDirectDrawSurface read GetImage;
  end;

  TDoorResource = class( TStaticResource )
  public
    Strips : Integer;
    ItemIndex : Integer;
    ItemZone : Integer;
    procedure LoadData( INI : TStringINIFile ); override;
    function Define( Map : TAniMap; Zone : byte; Index : Word ) : integer; override;
    procedure RenderLocked( Figure : TAniFigure; Bits : PBITPLANE ); override;
  end;

  TTileResource = class( TStaticResource )
  public
    INI : TStringINIFile;
    destructor Destroy; override;
    procedure LoadData( INI : TStringINIFile ); override;
    function Define( Map : TAniMap; Zone : byte; Index : word ) : integer; override;
  end;

const
  RenderWidth = 8;
  scrLoop = 1;
  scrDie = 2;
  scrRandom = 3;
  Angles = 8;

  LayerPath =  'engine\layeredimages\';
  InventoryPath = 'engine\inventoryimages\';

var
  ResourcePath : string;
  TilePath : string;
  SoundPath : string;
  ItemDB : string;
  XRefDB : string;
  TitlesDB : string;

{ TODO -cMove : Redo as stringhelper in SoAOS.StrUtils }
function Parse( const S : String; Index : integer; ParseChar : Char ) : string;
//function GetFile( const FileName : string; var BM : TBitmap; var INI : TStringIniFile; var FrameCount : Integer ) : Boolean;
function LoadResource( const Filename : string ) : TResource; overload;
function LoadResource( const Filename : string; OnDemand : boolean ) : TResource; overload;
function LoadArtResource( const ResourceFile : string ) : TResource; overload;
function LoadArtResource( const ResourceFile : string; OnDemand : boolean ) : TResource; overload;
function GetFileDate( cFile : string ) : TDateTime;
function NakedName( const FileName: string ): string;

implementation

uses
  Character,
  SoAOS.AI.Helper,
  Engine,
  Parts;

function Parse( const S : String; Index : integer; ParseChar : Char ) : string;
begin
  result := '';
  var arr: TArray<string> := S.Split([ParseChar]);
  if Length(arr)-1>=Index then
    result := arr[Index];
end;

function GetFileDate( cFile : string ) : TDateTime;
var
  TStream : TFileStream;
const
  FailName : string = 'Resource.GetFileDate';
begin
  Result := -1;

  Log.DebugLog(FailName);
  try

    if not TFile.Exists( cFile ) then
    begin
      exit;
    end;
    try
      TStream := TFileStream.Create( cFile, fmShareDenyNone );
      try
        Result := FileDateToDateTime( FileGetDate( TStream.Handle ) );
      finally
        TStream.Free;
      end;
    except
      Result := -1;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function LoadArtResource( const ResourceFile : string ) : TResource;
begin
  result := LoadArtResource( ResourceFile, false );
end;

function LoadArtResource( const ResourceFile : string; OnDemand : boolean ) : TResource;
begin
  result := LoadResource( ResourcePath + ResourceFile, OnDemand );
  if assigned( result ) then
    result.Filename := ResourceFile;
end;

function LoadResource( const Filename : string ) : TResource;
begin
  result := LoadResource( Filename, false );
end;

function LoadResource( const Filename : string; OnDemand : boolean ) : TResource;
var
  POXFile : string;
  INI : TStringINIFile;
  Stream : TMemoryStream;
  S : AnsiString;
  TextOnly : boolean;
  L : longword;
  M : array[ 1..2 ] of AnsiChar;
  EOB, BB : word;
const
  FailName : string = 'Resource.LoadResource';
begin
  result := nil;
  Log.DebugLog(FailName);
  try
    EOB := $4242;
    POXFile := ChangeFileExt( Filename, '.pox' );
    if not TFile.Exists( POXFile ) then
      exit;
    //Load POX
    Stream := TMemoryStream.create;
    try
      Stream.LoadFromFile( POXFile );
      TextOnly := false;
      Stream.Read( L, sizeof( L ) );
      if ( L <> $41584F50 ) then
        exit;
      Stream.Read( M, sizeof( M ) );
      Stream.Read( BB, sizeof( BB ) ); //CRLF
      if ( M = #83#84 ) then
      begin //ST
        result := TStaticResource.Create;
        Stream.Read( L, sizeof( L ) );
      end
      else if ( M = #67#67 ) then
      begin //CC
        result := TCharacterResource.Create;
        Stream.Read( L, sizeof( L ) );
        TCharacterResource( result ).Layered := false;
      end
      else if ( M = #76#67 ) then
      begin //LC
        result := TCharacterResource.Create;
        L := Stream.Size - Stream.Position;
        TCharacterResource( result ).Layered := true;
        TextOnly := true;
      end
      else if ( M = #68#83 ) then
      begin //DS
        result := TDoorResource.Create;
        Stream.Read( L, sizeof( L ) );
      end
      else if ( M = #84#84 ) then
      begin //TT
        result := TTileResource.Create;
        Stream.Read( L, sizeof( L ) );
      end
      else if ( M = #80#82 ) then
      begin //PR
        result := TProjectileResource.Create;
        Stream.Read( L, sizeof( L ) );
      end
      else if ( M = #83#67 ) then
      begin //SC
        result := TCastResource.Create;
        Stream.Read( L, sizeof( L ) );
      end
      else if ( M = #76#76 ) then
      begin //LL
        result := TLayerResource.Create;
        TLayerResource( result ).LinkPath := Filename;
        Stream.Read( L, sizeof( L ) );
      end
      else if ( M = #73#73 ) then
      begin //II
        result := TInventoryResource.Create;
        Stream.Read( L, sizeof( L ) );
      end
      else if ( M = #83#80 ) then
      begin //SP
        result := TResource.Create;
        Stream.Read( L, sizeof( L ) );
      end
      else
        exit;
      SetLength( S, L );
      Stream.Read( S[ 1 ], L );
      INI := TStringINIFile.Create( S );
      result.LoadData( INI );
      INI.free;

      if TextOnly then
      begin
        result.Loaded := true;
        result.Reload := true;
      end
      else if OnDemand then
      begin
        result.OnDemand := true;
        result.Loaded := true;
        result.Reload := true;
      end
      else
      begin
        Stream.Read( BB, sizeof( BB ) );
        if BB = EOB then
        begin
          result.RLE := TRLESprite.create;
          result.RLE.LoadFromStream( Stream );
          result.Loaded := true;
          result.Reload := true;
        end
        else
        begin
          result.free;
          result := nil;
          exit;
        end;
      end;
    finally
      Stream.free;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message+ #13#10+E.StackTrace, [ ] );
  end;
end;

procedure LoadArray( S : string; var A : TArray<Word> );
begin
  var arr: TArray<string> := S.Split([',']);
  setLength(A, Length(arr));
  for var i: integer := 0 to Length(arr)-1 do
    A[i] := StrToIntDef(arr[i], 0);
end;

function NakedName( const Filename : string ) : string;
var
  POXFile : string;
  INI : TStringINIFile;
  Stream : TMemoryStream;
  S : AnsiString;
  L : longword;
  M : array[ 1..2 ] of AnsiChar;
  BB : word;
const
  FailName : string = 'Resource.NakedName';
begin
  Result := '';
  Log.DebugLog(FailName);
  try
    POXFile := ChangeFileExt( Filename, '.pox' );
    if not TFile.Exists( POXFile ) then
      exit;
    //Load POX
    Stream := TMemoryStream.create;
    try
      Stream.LoadFromFile( POXFile );
      Stream.Read( L, sizeof( L ) );
      if ( L <> $41584F50 ) then
        exit;
      Stream.Read( M, sizeof( M ) );
      Stream.Read( BB, sizeof( BB ) ); //CRLF
      if ( M = #76#67 ) then
        L := Stream.Size - Stream.Position
      else
        exit;
      SetLength( S, L );
      Stream.Read( S[ 1 ], L );
      INI := TStringINIFile.Create( S );
      Result := ChangeFileExt( INI.ReadString('Layers', 'naked', ''), '.pox' );
      INI.free;
    finally
      Stream.free;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message+ #13#10+E.StackTrace, [ ] );
  end;
end;


{ TResource }

procedure TResource.LoadScript( const S, Name : string; Multiplier : Word );
var
  NewScript : TScript;
const
  FailName : string = 'TResource.LoadScript';
begin
  Log.DebugLog(FailName);
  try
    NewScript := TScript.Create;
    NewScript.Multiplier := Multiplier;
    var i: integer := 0;
    var arr: TArray<string> := S.Split([',']);
    for var C: string in arr do
    begin
      if ( C = 'loop' ) then
      begin
        NewScript.Tag := scrLoop;
        NewScript.Frames := i;
        AddScript( Name, NewScript );
        Break;
      end
      else if ( C = 'random' ) then
      begin
        NewScript.Tag := scrRandom;
        NewScript.Frames := i;
        AddScript( Name, NewScript );
        Break;
      end
      else if C = 'die' then
      begin
        NewScript.Tag := scrDie;
        NewScript.Frames := i;
        AddScript( Name, NewScript );
        Break;
      end
      else if ( C = 'end' ) or ( C = '' ) then
      begin
        NewScript.Tag := 0;
        NewScript.Frames := i;
        AddScript( Name, NewScript );
        Break;
      end
      else
      begin
        Inc( i );
        NewScript.FrameID[ i ] := StrToInt( C );
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TResource.LoadAction( INI : TStringIniFile; const Action : AnsiString );
var
  S0, S1, S2, S3, S4, S5, S6, S7, S8 : AnsiString;
  Group : string;
  Multiplier : Word;
const
  FailName : string = 'TResource.LoadAction';
begin
  Log.DebugLog(FailName);
  try
  
    Group := 'Action ' + Action;
    Multiplier := INI.ReadInteger( Group, 'FrameMultiplier', FrameMultiplier );
    S0 := AnsiString( AnsiLowerCase( INI.ReadString( Group, 'Frames', '' ) ) );
    S1 := AnsiString( AnsiLowerCase( INI.ReadString( Group, 'SSFrames', '' ) ) );
    S2 := AnsiString( AnsiLowerCase( INI.ReadString( Group, 'SEFrames', '' ) ) );
    S3 := AnsiString( AnsiLowerCase( INI.ReadString( Group, 'EEFrames', '' ) ) );
    S4 := AnsiString( AnsiLowerCase( INI.ReadString( Group, 'NEFrames', '' ) ) );
    S5 := AnsiString( AnsiLowerCase( INI.ReadString( Group, 'NNFrames', '' ) ) );
    S6 := AnsiString( AnsiLowerCase( INI.ReadString( Group, 'NWFrames', '' ) ) );
    S7 := AnsiString( AnsiLowerCase( INI.ReadString( Group, 'WWFrames', '' ) ) );
    S8 := AnsiString( AnsiLowerCase( INI.ReadString( Group, 'SWFrames', '' ) ) );
    if S0 <> '' then
      LoadScript( S0, Action, Multiplier );
    if S1 <> '' then
      LoadScript( S1, Action + 'SS', Multiplier );
    if S2 <> '' then
      LoadScript( S2, Action + 'SE', Multiplier );
    if S3 <> '' then
      LoadScript( S3, Action + 'EE', Multiplier );
    if S4 <> '' then
      LoadScript( S4, Action + 'NE', Multiplier );
    if S5 <> '' then
      LoadScript( S5, Action + 'NN', Multiplier );
    if S6 <> '' then
      LoadScript( S6, Action + 'NW', Multiplier );
    if S7 <> '' then
      LoadScript( S7, Action + 'WW', Multiplier );
    if S8 <> '' then
      LoadScript( S8, Action + 'SW', Multiplier );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TResource.LoadData( INI : TStringIniFile );
var
  S : string;
  i : Integer;
  Actions : TStringList;
const
  FailName : string = 'TResource.LoadData';
begin
  Log.DebugLog(FailName);
  try

    FrameWidth := INI.ReadInteger( 'Header', 'ImageWidth', 96 );
    FrameHeight := INI.ReadInteger( 'Header', 'ImageHeight', 86 );
    S := LowerCase( INI.ReadString( 'Header', 'Blend', '' ) );
    if ( S = 'add' ) then
      SpecialEffect := seAdd
    else if ( S = 'subtract' ) or ( S = 'sub' ) then
      SpecialEffect := seSubtract
    else if ( S = 'alpha' ) then
      SpecialEffect := seTranslucent
    else
      SpecialEffect := seNone;
    Alpha := INI.ReadInteger( 'Header', 'BlendAmount', 100 );
    S := LowerCase( INI.ReadString( 'Header', 'UseLighting', '' ) );
    if ( S = 'none' ) then
    begin
      UseLighting := False;
      Vertical := false;
    end
    else
    begin
      UseLighting := True;
      Vertical := ( S = 'vert' );
    end;
    S := LowerCase( INI.ReadString( 'Header', 'Highlightable', '' ) );
    if ( S = 'yes' ) then
      Highlightable := True
    else
      Highlightable := False;
    S := LowerCase( INI.ReadString( 'Header', 'Shadow', '' ) );
    if ( S = 'no' ) or ( S = 'none' ) then
    begin
      DrawShadow := False;
      ComplexShadow := false;
    end
    else
    begin
      DrawShadow := True;
      ComplexShadow := ( S <> 'simple' );
    end;
    ShadowColor := INI.ReadInteger( 'Header', 'ShadowColor', 0 );
    Radius := INI.ReadInteger( 'Header', 'CollisionRadius', 16 );
    FrameMultiplier := INI.ReadInteger( 'Header', 'FrameMultiplier', 1 );
    CenterX := INI.ReadInteger( 'Header', 'CollisionOffset', FrameWidth div 2 );
    CenterY := INI.ReadInteger( 'Header', 'CollisionHeight', FrameHeight - 10 );
    Speed := UnFormatFP( INI.ReadString( 'Header', 'Speed', '5.0' ) );
    RunSpeed := Speed;

    S := INI.ReadString( 'Action Walk', 'MovementPerFrame', '' );
    if S <> '' then
      Speed := UnFormatFP( S );

    S := INI.ReadString( 'Action Run', 'MovementPerFrame', '' );
    if S <> '' then
      RunSpeed := UnFormatFP( S );

    S := INI.ReadString( 'Action Death', 'MovementPerFrame', '' );
    if S = '-1' then
      DeathSlide := false
    else
      DeathSlide := true;

    S := INI.ReadString( 'Header', 'Actions', '' );
    Actions := TStringList.Create;
    Actions.CommaText := S;
    for i := 0 to Actions.Count - 1 do
    begin
      LoadAction( INI, AnsiString( Actions.Strings[ i ] ) );
    end;
    Actions.Free;

    Picture := TBitPlane.Create( FrameWidth, FrameHeight );
    Picture.KeyColor := cTransparent;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

//procedure TResource.Draw( Canvas : TCanvas; X, Y : Integer; Frame : Word );
//begin
//
//end;

procedure TResource.EnumLightSource( Figure : TAniFigure; Index, X, Y, Z : longint; Intensity : double; Radius : integer );
begin
  if Index >= 8 then
    exit;
  LightCount := Index + 1;
  Lights[ LightCount ].X := X;
  Lights[ LightCount ].Y := Y;
  Lights[ LightCount ].Z := Z;
  Lights[ LightCount ].Intensity := Intensity;
  Lights[ LightCount ].Radius := Radius;
end;

procedure TResource.FreeResources;
const
  FailName : string = 'TResource.FreeResources';
begin
  Log.DebugLog(FailName);
  try
    Picture.free;
    Picture := nil;
    RLE.free;
    RLE := nil;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TResource.SetReload( Value : boolean );
begin
  FReload := Value;
end;

procedure TResource.Render( Figure : TAniFigure );
var
  DstX1, DstY1 : Integer;
  ddsd : TDDSurfaceDesc;
  Bits : BITPLANE;
begin
  if ( Figure.Frame = 0 ) or not Figure.Visible then
    Exit;

  DstX1 := Figure.View.Left + Figure.PosX;
  DstY1 := Figure.View.Top + Figure.PosY;

  if PlotShadows and DrawShadow then
  begin
    if not ComplexShadow then
      DrawSub( lpDDSBack, Rect( DstX1 + Figure.CenterX - 37, DstY1 + Figure.CenterY - 20, DstX1 + Figure.CenterX + 37, DstY1 + Figure.CenterY + 20 ),
        Rect( 0, 0, 74, 40 ), ShadowImage, True, 255 )
    else if ( BaseLightType = 3 ) then
      DrawSub( lpDDSBack, Rect( DstX1 + Figure.CenterX - 37, DstY1 + Figure.CenterY - 20, DstX1 + Figure.CenterX + 37, DstY1 + Figure.CenterY + 20 ),
        Rect( 0, 0, 74, 40 ), ShadowImage, True, 128 );
  end;

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
      RenderLocked( Figure, @Bits );
    finally
      lpDDSBack.Unlock( nil );
    end;
  end;
end;

function TResource.GetFrameCount : integer;
begin
  if assigned( RLE ) then
    result := RLE.Frames
  else
    result := 0;
end;

procedure TResource.LoadGraphic;
var
  Stream : TFileStream;
  L : longword;
  EOB, BB : word;
begin
  if assigned( RLE ) then
    exit;
  EOB := $4242;
  Stream := TFileStream.create( ResourcePath + ChangeFileExt( Filename, '.pox' ), fmOpenRead or fmShareCompat );
  try
    Stream.Read( L, 4 );
    if ( L <> $41584F50 ) then
      exit;
    Stream.Seek(4, soFromCurrent);
    Stream.Read( L, 4);
    Stream.Seek(L, soFromCurrent);
    Stream.Read( BB, 2 );
    if BB = EOB then
    begin
      RLE := TRLESprite.create;
      RLE.LoadFromStream( Stream );
    end;
  finally
    Stream.free;
  end;
end;

procedure TResource.RenderLocked( Figure : TAniFigure; Bits : PBITPLANE );
var
  SrcBlend, DstBlend : integer;
  i, j, A : integer;
  ShadowFrame : integer;
  ShadowScript : integer;
  dX, dY : longint;
  D, D1, D2 : single;
  R, G, B : integer;
begin
  if not assigned( RLE ) then
  begin
    if OnDemand then
    begin
      LoadGraphic;
      if not assigned( RLE ) then
        exit;
    end
    else
      exit;
  end;

{    if (MaskHeight < DstY2 - DstY1) then begin
      lpDDSBack.BltFast(DstX1, DstY1, Picture,
        Rect(SrcX1, SrcY1, SrcX2, MaskHeight), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
      DrawAlpha(lpDDSBack, Rect(DstX1, View.Top + Y + MaskHeight, DstX2, DstY2), Rect(SrcX1, MaskHeight, SrcX2, SrcY2),
        Picture, True, Alpha);
    end   }

  if PlotShadows then
  begin
    if ComplexShadow then
    begin
      if Figure.ScriptIndex >= 0 then
      begin
        for i := 1 to LightCount do
        begin
          dX := Figure.X - Lights[ i ].X;
          dY := Lights[ i ].Y - Figure.Y;
          if ( dX = 0 ) and ( dY = 0 ) then
          begin
          end
          else
          begin
            A := round( ATan( dY, dX ) * 180 / PI );
            if ( A >= 23 ) and ( A < 68 ) then
              ShadowScript := Figure.ScriptIndex + 1
            else if ( A >= 68 ) and ( A < 113 ) then
              ShadowScript := Figure.ScriptIndex + 2
            else if ( A >= 113 ) and ( A < 158 ) then
              ShadowScript := Figure.ScriptIndex + 3
            else if ( A >= 158 ) and ( A < 203 ) then
              ShadowScript := Figure.ScriptIndex + 4
            else if ( A >= 203 ) and ( A < 248 ) then
              ShadowScript := Figure.ScriptIndex + 5
            else if ( A >= 248 ) and ( A < 293 ) then
              ShadowScript := Figure.ScriptIndex + 6
            else if ( A >= 293 ) and ( A < 338 ) then
              ShadowScript := Figure.ScriptIndex + 7
            else
              ShadowScript := Figure.ScriptIndex;

            if ( ShadowScript div 8 ) > ( Figure.ScriptIndex div 8 ) then
              dec( ShadowScript, 8 );
            ShadowFrame := TScript( Scripts.objects[ ShadowScript ] ).FrameID[ Figure.ScriptFrame ] - 1;

            Picture.Clear;
            RLE.DrawMono( ShadowFrame, 0, 0, Picture.Bits, ShadowColor );

            Picture.Bits.BaseX := Figure.CenterX - Bits.BaseX;
            Picture.Bits.BaseY := Figure.CenterY + Figure.Z - Bits.BaseY;
            j := round( Lights[ i ].Intensity * 50 );
            D1 := sqrt( sqr( dx ) + sqr( dY ) );
            if D1 > 0 then
            begin
              if ( Lights[ i ].Z > Figure.Height ) then
                D := D1 / Lights[ i ].Radius
              else
              begin
                D2 := Lights[ i ].Z / Figure.Height;
                D := ( 1 - D2 ) + D2 * D1 / Lights[ i ].Radius;
              end;
              Picture.DrawShadow( 0, 0, Bits, j, 100 - j, A, round( ( 50 + 50 * abs( dx ) / D1 ) * D ) );
            end;
            Picture.Bits.BaseX := 0;
            Picture.Bits.BaseY := 0;
          end;
        end;
      end;
    end;
  end;
  LightCount := 0;

  case TSpriteObject( Figure ).SpecialEffect of
    seTranslucent :
      begin
        DstBlend := 100 - TSpriteObject( Figure ).Alpha;
        SrcBlend := TSpriteObject( Figure ).Alpha;
      end;
    seAdd :
      begin
        DstBlend := 100;
        SrcBlend := TSpriteObject( Figure ).Alpha;
      end;
  else
    begin
      DstBlend := 0;
      SrcBlend := 100;
    end;
  end;

  R := Figure.LightR + TSpriteObject( Figure ).ColorR;
  G := Figure.LightG + TSpriteObject( Figure ).ColorG;
  B := Figure.LightB + TSpriteObject( Figure ).ColorB;
  if R < 0 then
    R := 0;
  if G < 0 then
    G := 0;
  if B < 0 then
    B := 0;

  if Figure.Highlighted and assigned( Picture ) then
  begin
    Picture.Clear;
    if Figure.UseLighting then
      RLE.DrawColorize( Figure.Frame - 1, 0, 0, Picture.Bits,
        100 * R div 255,
        100 * G div 255,
        100 * B div 255, 100, 0 )
    else
      RLE.Draw( Figure.Frame - 1, 0, 0, Picture.Bits );
    if DstBlend = 0 then
      Picture.DrawOutline( 0, 0, Bits, Figure.HighlightColor, true )
    else
    begin
      Picture.DrawOutline( 0, 0, Bits, Figure.HighlightColor, false );
      Picture.DrawBlend( 0, 0, Bits, SrcBlend, DstBlend );
    end;
  end
  else
  begin
    if Figure.UseLighting then
      RLE.DrawColorize( Figure.Frame - 1, 0, 0, Bits,
        100 * R div 255,
        100 * G div 255,
        100 * B div 255, SrcBlend, DstBlend )
    else
    begin
      RLE.DrawBlend( Figure.Frame - 1, 0, 0, Bits, SrcBlend, DstBlend );
    end;
  end;
end;

function TResource.MemSize : longint;
begin
  if assigned( RLE ) then
    result := RLE.MemSize
  else
    result := 0;
end;

{ TCharacterResource }

constructor TCharacterResource.Create;
const
  FailName : string = 'TCharacterResource.Create';
begin
  Log.DebugLog(FailName);
  try
    inherited;
    Defaults := TStringList.Create;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TCharacterResource.Destroy;
const
  FailName : string = 'TCharacterResource.Destroy';
begin
  Log.DebugLog(FailName);
  try
    Defaults.Free;
    inherited;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TCharacterResource.LoadData( INI : TStringIniFile );
const
  FailName : string = 'TCharacterResource.LoadData';
begin
  Log.DebugLog(FailName);
  try
    inherited;
    FContactFrame := INI.ReadInteger( 'Action Attack1', 'TriggerFrame', 1 );
    FReleaseFrame := INI.ReadInteger( 'Action BowAttack', 'TriggerFrame', 1 );
    FCastFrame := INI.ReadInteger( 'Action Cast', 'TriggerFrame', 1 );
    Equipment[ slLeg1 ] := AnsiString( INI.ReadString( 'Layers', 'leg1', '' ) );
    Equipment[ slBoot ] := AnsiString( INI.ReadString( 'Layers', 'boot', '' ) );
    Equipment[ slLeg2 ] := AnsiString( INI.ReadString( 'Layers', 'leg2', '' ) );
    Equipment[ slChest1 ] := AnsiString( INI.ReadString( 'Layers', 'chest1', '' ) );
    Equipment[ slChest2 ] := AnsiString( INI.ReadString( 'Layers', 'chest2', '' ) );
    Equipment[ slArm ] := AnsiString( INI.ReadString( 'Layers', 'arm', '' ) );
    Equipment[ slBelt ] := AnsiString( INI.ReadString( 'Layers', 'belt', '' ) );
    Equipment[ slChest3 ] := AnsiString( INI.ReadString( 'Layers', 'chest3', '' ) );
    Equipment[ slGauntlet ] := AnsiString( INI.ReadString( 'Layers', 'gauntlet', '' ) );
    Equipment[ slOuter ] := AnsiString( INI.ReadString( 'Layers', 'outer', '' ) );
    Equipment[ slHelmet ] := AnsiString( INI.ReadString( 'Layers', 'helmet', '' ) );
    Equipment[ slWeapon ] := AnsiString( INI.ReadString( 'Layers', 'weapon', '' ) );
    Equipment[ slShield ] := AnsiString( INI.ReadString( 'Layers', 'shield', '' ) );
    Equipment[ sltabar ] := AnsiString( INI.ReadString( 'Layers', 'tabar', '' ) );
    Equipment[ slMisc1 ] := AnsiString( INI.ReadString( 'Layers', 'misc1', '' ) );
    Equipment[ slMisc2 ] := AnsiString( INI.ReadString( 'Layers', 'misc2', '' ) );
    Equipment[ slMisc3 ] := AnsiString( INI.ReadString( 'Layers', 'misc3', '' ) );

    FAttackVariations := 1;
    while INI.SectionExists( 'Action Attack' + IntToStr( FAttackVariations ) ) do
      inc( FAttackVariations );
    dec( FAttackVariations );

    NakedName := AnsiString( INI.ReadString( 'Layers', 'naked', '' ) );
    if NakedName <> '' then
    begin
      NakedResource := PartManager.GetLayerResource( NakedName );
    end;
    if lowercase( NakedName ) = 'humanmalelayers\basehumanmale.gif' then
      UseDefaultPants := @DefaultPants
    else if lowercase( NakedName ) = 'humanfemale2layers\basehumanfemale.gif' then
      UseDefaultPants := @FemDefaultPants
    else if lowercase( NakedName ) = 'elfmalelayers\baseelf.gif' then
      UseDefaultPants := @ElfDefaultPants
    else
      UseDefaultPants := nil;
    Female := lowercase( NakedName ).IndexOf( 'female' ) > 0;

    HeadName := AnsiString( INI.ReadString( 'Layers', 'head', '' ) );
    if HeadName <> '' then
    begin
      HeadResource := PartManager.GetLayerResource( HeadName );
    end;

    UseCastAnimation := ( lowercase( INI.ReadString( 'Header', 'UseCastAnimation', '' ) ) <> 'false' );

    INI.ReadSectionValues( 'Properties', Defaults );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TCharacterResource.SetReload( Value : boolean );
begin
  FReload := Value;
  if Value then
  begin
    if assigned( NakedResource ) then
      NakedResource.FReload := true;
    if assigned( HeadResource ) then
      HeadResource.FReload := true;
  end;
end;

procedure TCharacterResource.Render( Figure : TAniFigure );
var
  DstX1, DstY1 : Integer;
  ddsd : TDDSurfaceDesc;
  Bits : BITPLANE;
  i : integer;
begin
  if not Figure.Visible then
    exit;
  if ( Figure.Frame = 0 ) then
  begin
    DstX1 := Figure.View.Left + Figure.PosX;
    DstY1 := Figure.View.Top + Figure.PosY;

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

        with TCharacter( Figure ) do
        begin
          if UseCastAnimation and Casting and ( RecoveryCount = 0 ) then
            CurrentSpell.CastEffect.RenderLocked( Figure, @Bits );
          for i := 0 to Effects.count - 1 do
            Effects[ i ].RenderLocked( Figure, @Bits );
        end;
      finally
        lpDDSBack.Unlock( nil );
      end;
    end;
    Exit;
  end;

  DstX1 := Figure.View.Left + Figure.PosX;
  DstY1 := Figure.View.Top + Figure.PosY;

  if PlotShadows and DrawShadow then
  begin
    if not ComplexShadow then
      DrawSub( lpDDSBack, Rect( DstX1 + Figure.CenterX - 37, DstY1 + Figure.CenterY - 20, DstX1 + Figure.CenterX + 37, DstY1 + Figure.CenterY + 20 ),
        Rect( 0, 0, 74, 40 ), ShadowImage, True, 255 )
    else if ( BaseLightType = 3 ) then
      DrawSub( lpDDSBack, Rect( DstX1 + Figure.CenterX - 37, DstY1 + Figure.CenterY - 20, DstX1 + Figure.CenterX + 37, DstY1 + Figure.CenterY + 20 ),
        Rect( 0, 0, 74, 40 ), ShadowImage, True, 128 );
  end;

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
      RenderLocked( Figure, @Bits );

      with TCharacter( Figure ) do
      begin
        if UseCastAnimation and Casting and ( RecoveryCount = 0 ) then
          CurrentSpell.CastEffect.RenderLocked( Figure, @Bits );
        for i := 0 to Effects.count - 1 do
          Effects[ i ].RenderLocked( Figure, @Bits );
      end;
    finally
      lpDDSBack.Unlock( nil );
    end;
  end;
end;

procedure TCharacterResource.RenderLocked( Figure : TAniFigure;
  Bits : PBITPLANE );
var
  SrcBlend, DstBlend : integer;
  RFactor, GFactor, BFactor : integer;
  i, j, A : integer;
  ShadowFrame : integer;
  ShadowScript : integer;
  dX, dY : longint;
  D, D1, D2 : single;
  p : pointer;

  procedure DrawParts( MyBits : PBITPLANE );
  var
    i : integer;
    R, G, B : integer;
  begin
    i := Figure.Frame - 1;

    R := Figure.LightR + TSpriteObject( Figure ).ColorR;
    G := Figure.LightG + TSpriteObject( Figure ).ColorG;
    B := Figure.LightB + TSpriteObject( Figure ).ColorB;
    if R < 0 then
      R := 0;
    if G < 0 then
      G := 0;
    if B < 0 then
      B := 0;
    if Figure.UseLighting then
    begin
      RFactor := 100 * R div 255;
      GFactor := 100 * G div 255;
      BFactor := 100 * B div 255;
    end
    else
    begin
      RFactor := 100;
      GFactor := 100;
      BFactor := 100;
    end;

    if Female and Bikini then
    begin
      if assigned( TCharacter( Figure ).FEquipment[ slShield ] ) and
        assigned( TCharacter( Figure ).FEquipment[ slShield ].Resource ) then
      begin
        if TLayerResource( TCharacter( Figure ).FEquipment[ slShield ].Resource ).BackLayer[ i ] then
          TResource( TCharacter( Figure ).FEquipment[ slShield ].Resource ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
        if assigned( TLayerResource( TCharacter( Figure ).FEquipment[ slShield ].Resource ).LinkedResource ) then
          TLayerResource( TCharacter( Figure ).FEquipment[ slShield ].Resource ).LinkedResource.RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      end;
      if TCharacter( Figure ).CombatMode and assigned( TCharacter( Figure ).FEquipment[ slWeapon ] ) and
        assigned( TCharacter( Figure ).FEquipment[ slWeapon ].Resource ) then
      begin
        if TLayerResource( TCharacter( Figure ).FEquipment[ slWeapon ].Resource ).BackLayer[ i ] then
          TResource( TCharacter( Figure ).FEquipment[ slWeapon ].Resource ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
        if assigned( TLayerResource( TCharacter( Figure ).FEquipment[ slWeapon ].Resource ).LinkedResource ) then
          TLayerResource( TCharacter( Figure ).FEquipment[ slWeapon ].Resource ).LinkedResource.RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      end;
      if assigned( TCharacter( Figure ).FEquipment[ sltabar ] ) and
        assigned( TCharacter( Figure ).FEquipment[ sltabar ].Resource ) then
      begin
        if TLayerResource( TCharacter( Figure ).FEquipment[ sltabar ].Resource ).BackLayer[ i ] then
          TResource( TCharacter( Figure ).FEquipment[ sltabar ].Resource ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
        if assigned( TLayerResource( TCharacter( Figure ).FEquipment[ sltabar ].Resource ).LinkedResource ) then
          TLayerResource( TCharacter( Figure ).FEquipment[ sltabar ].Resource ).LinkedResource.RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      end;
      if assigned( TCharacter( Figure ).FEquipment[ slHelmet ] ) and
        assigned( TCharacter( Figure ).FEquipment[ slHelmet ].Resource ) then
      begin
        if TLayerResource( TCharacter( Figure ).FEquipment[ slHelmet ].Resource ).BackLayer[ i ] then
          TResource( TCharacter( Figure ).FEquipment[ slHelmet ].Resource ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
        if assigned( TLayerResource( TCharacter( Figure ).FEquipment[ slHelmet ].Resource ).LinkedResource ) then
          TLayerResource( TCharacter( Figure ).FEquipment[ slHelmet ].Resource ).LinkedResource.RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      end;
      if assigned( TCharacter( Figure ).FEquipment[ slGauntlet ] ) and
        assigned( TCharacter( Figure ).FEquipment[ slGauntlet ].Resource ) then
      begin
        if TLayerResource( TCharacter( Figure ).FEquipment[ slGauntlet ].Resource ).BackLayer[ i ] then
          TResource( TCharacter( Figure ).FEquipment[ slGauntlet ].Resource ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
        if assigned( TLayerResource( TCharacter( Figure ).FEquipment[ slGauntlet ].Resource ).LinkedResource ) then
          TLayerResource( TCharacter( Figure ).FEquipment[ slGauntlet ].Resource ).LinkedResource.RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      end;
      if assigned( TCharacter( Figure ).FEquipment[ slBelt ] ) and
        assigned( TCharacter( Figure ).FEquipment[ slBelt ].Resource ) then
      begin
        if TLayerResource( TCharacter( Figure ).FEquipment[ slBelt ].Resource ).BackLayer[ i ] then
          TResource( TCharacter( Figure ).FEquipment[ slBelt ].Resource ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
        if assigned( TLayerResource( TCharacter( Figure ).FEquipment[ slBelt ].Resource ).LinkedResource ) then
          TLayerResource( TCharacter( Figure ).FEquipment[ slBelt ].Resource ).LinkedResource.RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      end;
      if assigned( TCharacter( Figure ).FEquipment[ slArm ] ) and
        assigned( TCharacter( Figure ).FEquipment[ slArm ].Resource ) then
      begin
        if TLayerResource( TCharacter( Figure ).FEquipment[ slArm ].Resource ).BackLayer[ i ] then
          TResource( TCharacter( Figure ).FEquipment[ slArm ].Resource ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
        if assigned( TLayerResource( TCharacter( Figure ).FEquipment[ slArm ].Resource ).LinkedResource ) then
          TLayerResource( TCharacter( Figure ).FEquipment[ slArm ].Resource ).LinkedResource.RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      end;
      if assigned( TCharacter( Figure ).FEquipment[ slBoot ] ) and
        assigned( TCharacter( Figure ).FEquipment[ slBoot ].Resource ) then
      begin
        if TLayerResource( TCharacter( Figure ).FEquipment[ slBoot ].Resource ).BackLayer[ i ] then
          TResource( TCharacter( Figure ).FEquipment[ slBoot ].Resource ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
        if assigned( TLayerResource( TCharacter( Figure ).FEquipment[ slBoot ].Resource ).LinkedResource ) then
          TLayerResource( TCharacter( Figure ).FEquipment[ slBoot ].Resource ).LinkedResource.RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      end;

      NakedResource.RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );

      if assigned( TCharacter( Figure ).FEquipment[ slBoot ] ) and
        assigned( TCharacter( Figure ).FEquipment[ slBoot ].Resource ) and
        not TLayerResource( TCharacter( Figure ).FEquipment[ slBoot ].Resource ).BackLayer[ i ] then
        TResource( TCharacter( Figure ).FEquipment[ slBoot ].Resource ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      if assigned( TCharacter( Figure ).FEquipment[ slArm ] ) and
        assigned( TCharacter( Figure ).FEquipment[ slArm ].Resource ) and
        not TLayerResource( TCharacter( Figure ).FEquipment[ slArm ].Resource ).BackLayer[ i ] then
        TResource( TCharacter( Figure ).FEquipment[ slArm ].Resource ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      if assigned( TCharacter( Figure ).FEquipment[ slBelt ] ) and
        assigned( TCharacter( Figure ).FEquipment[ slBelt ].Resource ) and
        not TLayerResource( TCharacter( Figure ).FEquipment[ slBelt ].Resource ).BackLayer[ i ] then
        TResource( TCharacter( Figure ).FEquipment[ slBelt ].Resource ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );

      if assigned( HeadResource ) then
        HeadResource.RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );

      if assigned( TCharacter( Figure ).FEquipment[ slHelmet ] ) and
        assigned( TCharacter( Figure ).FEquipment[ slHelmet ].Resource ) and
        not TLayerResource( TCharacter( Figure ).FEquipment[ slHelmet ].Resource ).BackLayer[ i ] then
        TResource( TCharacter( Figure ).FEquipment[ slHelmet ].Resource ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );

      if TCharacter( Figure ).CombatMode and assigned( TCharacter( Figure ).FEquipment[ slWeapon ] ) and
        assigned( TCharacter( Figure ).FEquipment[ slWeapon ].Resource ) and
        not TLayerResource( TCharacter( Figure ).FEquipment[ slWeapon ].Resource ).BackLayer[ i ] then
        TResource( TCharacter( Figure ).FEquipment[ slWeapon ].Resource ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      if assigned( TCharacter( Figure ).FEquipment[ slShield ] ) and
        assigned( TCharacter( Figure ).FEquipment[ slShield ].Resource ) and
        not TLayerResource( TCharacter( Figure ).FEquipment[ slShield ].Resource ).BackLayer[ i ] then
        TResource( TCharacter( Figure ).FEquipment[ slShield ].Resource ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
    end
    else
    begin
      p := TCharacter( Figure ).FEquipment[ slShield ];
      if assigned( p ) and assigned( TItem( p ).Resource ) then
      begin
        P := TItem( p ).Resource;
        if TLayerResource( p ).BackLayer[ i ] then
          TResource( p ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
        if assigned( TLayerResource( p ).LinkedResource ) then
          TLayerResource( p ).LinkedResource.RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      end;
      if TCharacter( Figure ).CombatMode then
      begin
        p := TCharacter( Figure ).FEquipment[ slWeapon ];
        if assigned( p ) and assigned( TItem( p ).Resource ) then
        begin
          P := TItem( p ).Resource;
          if TLayerResource( p ).BackLayer[ i ] then
            TResource( p ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
          if assigned( TLayerResource( p ).LinkedResource ) then
            TLayerResource( p ).LinkedResource.RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
        end;
      end;
      p := TCharacter( Figure ).FEquipment[ slHelmet ];
      if assigned( p ) and assigned( TItem( p ).Resource ) then
      begin
        P := TItem( p ).Resource;
        if TLayerResource( p ).BackLayer[ i ] then
          TResource( p ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
        if assigned( TLayerResource( p ).LinkedResource ) then
          TLayerResource( p ).LinkedResource.RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      end;
      p := TCharacter( Figure ).FEquipment[ sltabar ];
      if assigned( p ) and assigned( TItem( p ).Resource ) then
      begin
        P := TItem( p ).Resource;
        if TLayerResource( p ).BackLayer[ i ] then
          TResource( p ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
        if assigned( TLayerResource( p ).LinkedResource ) then
          TLayerResource( p ).LinkedResource.RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      end;
      p := TCharacter( Figure ).FEquipment[ slOuter ];
      if assigned( p ) and assigned( TItem( p ).Resource ) then
      begin
        P := TItem( p ).Resource;
        if TLayerResource( p ).BackLayer[ i ] then
          TResource( p ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
        if assigned( TLayerResource( p ).LinkedResource ) then
          TLayerResource( p ).LinkedResource.RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      end;
      p := TCharacter( Figure ).FEquipment[ slGauntlet ];
      if assigned( p ) and assigned( TItem( p ).Resource ) then
      begin
        P := TItem( p ).Resource;
        if TLayerResource( p ).BackLayer[ i ] then
          TResource( p ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
        if assigned( TLayerResource( p ).LinkedResource ) then
          TLayerResource( p ).LinkedResource.RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      end;
      p := TCharacter( Figure ).FEquipment[ slChest3 ];
      if assigned( p ) and assigned( TItem( p ).Resource ) then
      begin
        P := TItem( p ).Resource;
        if TLayerResource( p ).BackLayer[ i ] then
          TResource( p ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
        if assigned( TLayerResource( p ).LinkedResource ) then
          TLayerResource( p ).LinkedResource.RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      end;
      p := TCharacter( Figure ).FEquipment[ slBelt ];
      if assigned( p ) and assigned( TItem( p ).Resource ) then
      begin
        P := TItem( p ).Resource;
        if TLayerResource( p ).BackLayer[ i ] then
          TResource( p ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
        if assigned( TLayerResource( p ).LinkedResource ) then
          TLayerResource( p ).LinkedResource.RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      end;
      p := TCharacter( Figure ).FEquipment[ slArm ];
      if assigned( p ) and assigned( TItem( p ).Resource ) then
      begin
        P := TItem( p ).Resource;
        if TLayerResource( p ).BackLayer[ i ] then
          TResource( p ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
        if assigned( TLayerResource( p ).LinkedResource ) then
          TLayerResource( p ).LinkedResource.RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      end;
      p := TCharacter( Figure ).FEquipment[ slChest2 ];
      if assigned( p ) and assigned( TItem( p ).Resource ) then
      begin
        P := TItem( p ).Resource;
        if TLayerResource( p ).BackLayer[ i ] then
          TResource( p ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
        if assigned( TLayerResource( p ).LinkedResource ) then
          TLayerResource( p ).LinkedResource.RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      end;
      p := TCharacter( Figure ).FEquipment[ slChest1 ];
      if assigned( p ) and assigned( TItem( p ).Resource ) then
      begin
        P := TItem( p ).Resource;
        if TLayerResource( p ).BackLayer[ i ] then
          TResource( p ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
        if assigned( TLayerResource( p ).LinkedResource ) then
          TLayerResource( p ).LinkedResource.RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      end;
      p := TCharacter( Figure ).FEquipment[ slLeg2 ];
      if assigned( p ) and assigned( TItem( p ).Resource ) then
      begin
        P := TItem( p ).Resource;
        if TLayerResource( p ).BackLayer[ i ] then
          TResource( p ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
        if assigned( TLayerResource( p ).LinkedResource ) then
          TLayerResource( p ).LinkedResource.RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      end;
      p := TCharacter( Figure ).FEquipment[ slBoot ];
      if assigned( p ) and assigned( TItem( p ).Resource ) then
      begin
        P := TItem( p ).Resource;
        if TLayerResource( p ).BackLayer[ i ] then
          TResource( p ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
        if assigned( TLayerResource( p ).LinkedResource ) then
          TLayerResource( p ).LinkedResource.RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      end;
      p := TCharacter( Figure ).FEquipment[ slLeg1 ];
      if assigned( p ) and assigned( TItem( p ).Resource ) then
      begin
        P := TItem( p ).Resource;
        if TLayerResource( p ).BackLayer[ i ] then
          TResource( p ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
        if assigned( TLayerResource( p ).LinkedResource ) then
          TLayerResource( p ).LinkedResource.RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      end;

      NakedResource.RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );

      if assigned( TCharacter( Figure ).FEquipment[ slLeg1 ] ) and
        assigned( TCharacter( Figure ).FEquipment[ slLeg1 ].Resource ) and
        not TLayerResource( TCharacter( Figure ).FEquipment[ slLeg1 ].Resource ).BackLayer[ i ] then
      begin
        TResource( TCharacter( Figure ).FEquipment[ slLeg1 ].Resource ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      end
      else
      begin
        if assigned( UseDefaultPants ) and assigned( UseDefaultPants^ ) then
          UseDefaultPants^.RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      end;
      if assigned( TCharacter( Figure ).FEquipment[ slBoot ] ) and
        assigned( TCharacter( Figure ).FEquipment[ slBoot ].Resource ) and
        not TLayerResource( TCharacter( Figure ).FEquipment[ slBoot ].Resource ).BackLayer[ i ] then
        TResource( TCharacter( Figure ).FEquipment[ slBoot ].Resource ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      if assigned( TCharacter( Figure ).FEquipment[ slLeg2 ] ) and
        assigned( TCharacter( Figure ).FEquipment[ slLeg2 ].Resource ) and
        not TLayerResource( TCharacter( Figure ).FEquipment[ slLeg2 ].Resource ).BackLayer[ i ] then
        TResource( TCharacter( Figure ).FEquipment[ slLeg2 ].Resource ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      if assigned( TCharacter( Figure ).FEquipment[ slChest1 ] ) and
        assigned( TCharacter( Figure ).FEquipment[ slChest1 ].Resource ) and
        not TLayerResource( TCharacter( Figure ).FEquipment[ slChest1 ].Resource ).BackLayer[ i ] then
        TResource( TCharacter( Figure ).FEquipment[ slChest1 ].Resource ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      if assigned( TCharacter( Figure ).FEquipment[ slChest2 ] ) and
        assigned( TCharacter( Figure ).FEquipment[ slChest2 ].Resource ) and
        not TLayerResource( TCharacter( Figure ).FEquipment[ slChest2 ].Resource ).BackLayer[ i ] then
        TResource( TCharacter( Figure ).FEquipment[ slChest2 ].Resource ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      if assigned( TCharacter( Figure ).FEquipment[ slArm ] ) and
        assigned( TCharacter( Figure ).FEquipment[ slArm ].Resource ) and
        not TLayerResource( TCharacter( Figure ).FEquipment[ slArm ].Resource ).BackLayer[ i ] then
        TResource( TCharacter( Figure ).FEquipment[ slArm ].Resource ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      if assigned( TCharacter( Figure ).FEquipment[ slBelt ] ) and
        assigned( TCharacter( Figure ).FEquipment[ slBelt ].Resource ) and
        not TLayerResource( TCharacter( Figure ).FEquipment[ slBelt ].Resource ).BackLayer[ i ] then
        TResource( TCharacter( Figure ).FEquipment[ slBelt ].Resource ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      if assigned( TCharacter( Figure ).FEquipment[ slChest3 ] ) and
        assigned( TCharacter( Figure ).FEquipment[ slChest3 ].Resource ) and
        not TLayerResource( TCharacter( Figure ).FEquipment[ slChest3 ].Resource ).BackLayer[ i ] then
        TResource( TCharacter( Figure ).FEquipment[ slChest3 ].Resource ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      if assigned( TCharacter( Figure ).FEquipment[ slGauntlet ] ) and
        assigned( TCharacter( Figure ).FEquipment[ slGauntlet ].Resource ) and
        not TLayerResource( TCharacter( Figure ).FEquipment[ slGauntlet ].Resource ).BackLayer[ i ] then
        TResource( TCharacter( Figure ).FEquipment[ slGauntlet ].Resource ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      if assigned( TCharacter( Figure ).FEquipment[ slOuter ] ) and
        assigned( TCharacter( Figure ).FEquipment[ slOuter ].Resource ) and
        not TLayerResource( TCharacter( Figure ).FEquipment[ slOuter ].Resource ).BackLayer[ i ] then
        TResource( TCharacter( Figure ).FEquipment[ slOuter ].Resource ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      if assigned( TCharacter( Figure ).FEquipment[ sltabar ] ) and
        assigned( TCharacter( Figure ).FEquipment[ sltabar ].Resource ) and
        not TLayerResource( TCharacter( Figure ).FEquipment[ sltabar ].Resource ).BackLayer[ i ] then
        TResource( TCharacter( Figure ).FEquipment[ sltabar ].Resource ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      
      if assigned( HeadResource ) then
        HeadResource.RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );

      if assigned( TCharacter( Figure ).FEquipment[ slHelmet ] ) and
        assigned( TCharacter( Figure ).FEquipment[ slHelmet ].Resource ) and
        not TLayerResource( TCharacter( Figure ).FEquipment[ slHelmet ].Resource ).BackLayer[ i ] then
        TResource( TCharacter( Figure ).FEquipment[ slHelmet ].Resource ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );

      if TCharacter( Figure ).CombatMode and assigned( TCharacter( Figure ).FEquipment[ slWeapon ] ) and
        assigned( TCharacter( Figure ).FEquipment[ slWeapon ].Resource ) and
        not TLayerResource( TCharacter( Figure ).FEquipment[ slWeapon ].Resource ).BackLayer[ i ] then
        TResource( TCharacter( Figure ).FEquipment[ slWeapon ].Resource ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
      if assigned( TCharacter( Figure ).FEquipment[ slShield ] ) and
        assigned( TCharacter( Figure ).FEquipment[ slShield ].Resource ) and
        not TLayerResource( TCharacter( Figure ).FEquipment[ slShield ].Resource ).BackLayer[ i ] then
        TResource( TCharacter( Figure ).FEquipment[ slShield ].Resource ).RLE.DrawColorize( i, 0, 0, MyBits, RFactor, GFactor, BFactor, 100, 0 );
    end;
  end;

begin
  if Layered then
  begin
    if PlotShadows then
    begin
      if ComplexShadow then
      begin
        ShadowFrame := Figure.Frame - 1;
        if BaseLightType = 0 then
        begin
          Picture.Clear;
          NakedResource.RLE.DrawMono( ShadowFrame, 0, 0, Picture.Bits, ShadowColor );
          with Figure as TCharacter do
          begin
            if assigned( Equipment[ slShield ] ) and assigned( Equipment[ slShield ].Resource ) then
              TResource( Equipment[ slShield ].Resource ).RLE.DrawMono( ShadowFrame, 0, 0, Picture.Bits, ShadowColor );
            if TCharacter( Figure ).CombatMode and assigned( Equipment[ slWeapon ] ) and assigned( Equipment[ slWeapon ].Resource ) then
              TResource( Equipment[ slWeapon ].Resource ).RLE.DrawMono( ShadowFrame, 0, 0, Picture.Bits, ShadowColor );
            if assigned( Equipment[ slOuter ] ) and assigned( Equipment[ slOuter ].Resource ) then
            begin
              TResource( Equipment[ slOuter ].Resource ).RLE.DrawMono( ShadowFrame, 0, 0, Picture.Bits, ShadowColor );
              if assigned( TLayerResource( Equipment[ slOuter ].Resource ).LinkedResource ) then
                TLayerResource( Equipment[ slOuter ].Resource ).LinkedResource.RLE.DrawMono( ShadowFrame, 0, 0, Picture.Bits, ShadowColor );
            end;
                        if assigned( Equipment[ sltabar ] ) and assigned( Equipment[ sltabar ].Resource ) then
            begin
              TResource( Equipment[ sltabar ].Resource ).RLE.DrawMono( ShadowFrame, 0, 0, Picture.Bits, ShadowColor );
              if assigned( TLayerResource( Equipment[ sltabar ].Resource ).LinkedResource ) then
                TLayerResource( Equipment[ sltabar ].Resource ).LinkedResource.RLE.DrawMono( ShadowFrame, 0, 0, Picture.Bits, ShadowColor );
            end;
          end;
          Picture.Bits.BaseX := Figure.CenterX - Bits.BaseX;
          Picture.Bits.BaseY := Figure.CenterY + Figure.Z - Bits.BaseY;
          Picture.DrawShadow( 0, 0, Bits, 50, 50, 340, 54 );
          Picture.Bits.BaseX := 0;
          Picture.Bits.BaseY := 0;
        end
        else if BaseLightType = 1 then
        begin
          Picture.Clear;
          NakedResource.RLE.DrawMono( ShadowFrame, 0, 0, Picture.Bits, ShadowColor );
          with Figure as TCharacter do
          begin
            if assigned( Equipment[ slShield ] ) and assigned( Equipment[ slShield ].Resource ) then
              TResource( Equipment[ slShield ].Resource ).RLE.DrawMono( ShadowFrame, 0, 0, Picture.Bits, ShadowColor );
            if TCharacter( Figure ).CombatMode and assigned( Equipment[ slWeapon ] ) and assigned( Equipment[ slWeapon ].Resource ) then
              TResource( Equipment[ slWeapon ].Resource ).RLE.DrawMono( ShadowFrame, 0, 0, Picture.Bits, ShadowColor );
            if assigned( Equipment[ slOuter ] ) and assigned( Equipment[ slOuter ].Resource ) then
            begin
              TResource( Equipment[ slOuter ].Resource ).RLE.DrawMono( ShadowFrame, 0, 0, Picture.Bits, ShadowColor );
              if assigned( TLayerResource( Equipment[ slOuter ].Resource ).LinkedResource ) then
                TLayerResource( Equipment[ slOuter ].Resource ).LinkedResource.RLE.DrawMono( ShadowFrame, 0, 0, Picture.Bits, ShadowColor );
            end;
            if assigned( Equipment[ sltabar ] ) and assigned( Equipment[ sltabar ].Resource ) then
            begin
              TResource( Equipment[ sltabar ].Resource ).RLE.DrawMono( ShadowFrame, 0, 0, Picture.Bits, ShadowColor );
              if assigned( TLayerResource( Equipment[ sltabar ].Resource ).LinkedResource ) then
                TLayerResource( Equipment[ sltabar ].Resource ).LinkedResource.RLE.DrawMono( ShadowFrame, 0, 0, Picture.Bits, ShadowColor );
            end;
          end;
          Picture.Bits.BaseX := Figure.CenterX - Bits.BaseX;
          Picture.Bits.BaseY := Figure.CenterY + Figure.Z - Bits.BaseY;
          Picture.DrawShadow( 0, 0, Bits, 50, 50, 0, 50 );
          Picture.Bits.BaseX := 0;
          Picture.Bits.BaseY := 0;
        end
        else if BaseLightType = 2 then
        begin
          Picture.Clear;
          NakedResource.RLE.DrawMono( ShadowFrame, 0, 0, Picture.Bits, ShadowColor );
          with Figure as TCharacter do
          begin
            if assigned( Equipment[ slShield ] ) and assigned( Equipment[ slShield ].Resource ) then
              TResource( Equipment[ slShield ].Resource ).RLE.DrawMono( ShadowFrame, 0, 0, Picture.Bits, ShadowColor );
            if TCharacter( Figure ).CombatMode and assigned( Equipment[ slWeapon ] ) and assigned( Equipment[ slWeapon ].Resource ) then
              TResource( Equipment[ slWeapon ].Resource ).RLE.DrawMono( ShadowFrame, 0, 0, Picture.Bits, ShadowColor );
            if assigned( Equipment[ slOuter ] ) and assigned( Equipment[ slOuter ].Resource ) then
            begin
              TResource( Equipment[ slOuter ].Resource ).RLE.DrawMono( ShadowFrame, 0, 0, Picture.Bits, ShadowColor );
              if assigned( TLayerResource( Equipment[ slOuter ].Resource ).LinkedResource ) then
                TLayerResource( Equipment[ slOuter ].Resource ).LinkedResource.RLE.DrawMono( ShadowFrame, 0, 0, Picture.Bits, ShadowColor );
            end;
            if assigned( Equipment[ sltabar ] ) and assigned( Equipment[ sltabar ].Resource ) then
            begin
              TResource( Equipment[ sltabar ].Resource ).RLE.DrawMono( ShadowFrame, 0, 0, Picture.Bits, ShadowColor );
              if assigned( TLayerResource( Equipment[ sltabar ].Resource ).LinkedResource ) then
                TLayerResource( Equipment[ sltabar ].Resource ).LinkedResource.RLE.DrawMono( ShadowFrame, 0, 0, Picture.Bits, ShadowColor );
            end;
          end;
          Picture.Bits.BaseX := Figure.CenterX - Bits.BaseX;
          Picture.Bits.BaseY := Figure.CenterY + Figure.Z - Bits.BaseY;
          Picture.DrawShadow( 0, 0, Bits, 50, 50, 20, 54 );
          Picture.Bits.BaseX := 0;
          Picture.Bits.BaseY := 0;
        end;


        if Figure.ScriptIndex >= 0 then
        begin
          for i := 1 to LightCount do
          begin
            dX := Figure.X - Lights[ i ].X;
            dY := Lights[ i ].Y - Figure.Y;
            if ( dX = 0 ) and ( dY = 0 ) then
            begin
            end
            else
            begin
              A := round( ATan( dY, dX ) * 180 / PI );
              if ( A >= 23 ) and ( A < 68 ) then
                ShadowScript := Figure.ScriptIndex + 1
              else if ( A >= 68 ) and ( A < 113 ) then
                ShadowScript := Figure.ScriptIndex + 2
              else if ( A >= 113 ) and ( A < 158 ) then
                ShadowScript := Figure.ScriptIndex + 3
              else if ( A >= 158 ) and ( A < 203 ) then
                ShadowScript := Figure.ScriptIndex + 4
              else if ( A >= 203 ) and ( A < 248 ) then
                ShadowScript := Figure.ScriptIndex + 5
              else if ( A >= 248 ) and ( A < 293 ) then
                ShadowScript := Figure.ScriptIndex + 6
              else if ( A >= 293 ) and ( A < 338 ) then
                ShadowScript := Figure.ScriptIndex + 7
              else
                ShadowScript := Figure.ScriptIndex;

              if ( ShadowScript div 8 ) > ( Figure.ScriptIndex div 8 ) then
                dec( ShadowScript, 8 );
              ShadowFrame := TScript( Scripts.objects[ ShadowScript ] ).FrameID[ Figure.ScriptFrame ] - 1;

              Picture.Clear;
              NakedResource.RLE.DrawMono( ShadowFrame, 0, 0, Picture.Bits, ShadowColor );
              with Figure as TCharacter do
              begin
                if assigned( Equipment[ slShield ] ) and assigned( Equipment[ slShield ].Resource ) then
                  TResource( Equipment[ slShield ].Resource ).RLE.DrawMono( ShadowFrame, 0, 0, Picture.Bits, ShadowColor );
                if TCharacter( Figure ).CombatMode and assigned( Equipment[ slWeapon ] ) and assigned( Equipment[ slWeapon ].Resource ) then
                  TResource( Equipment[ slWeapon ].Resource ).RLE.DrawMono( ShadowFrame, 0, 0, Picture.Bits, ShadowColor );
                if assigned( Equipment[ slOuter ] ) and assigned( Equipment[ slOuter ].Resource ) then
                begin
                  TResource( Equipment[ slOuter ].Resource ).RLE.DrawMono( ShadowFrame, 0, 0, Picture.Bits, ShadowColor );
                  if assigned( TLayerResource( Equipment[ slOuter ].Resource ).LinkedResource ) then
                    TLayerResource( Equipment[ slOuter ].Resource ).LinkedResource.RLE.DrawMono( ShadowFrame, 0, 0, Picture.Bits, ShadowColor );
                end;
                if assigned( Equipment[ sltabar ] ) and assigned( Equipment[ sltabar ].Resource ) then
                begin
                  TResource( Equipment[ sltabar ].Resource ).RLE.DrawMono( ShadowFrame, 0, 0, Picture.Bits, ShadowColor );
                  if assigned( TLayerResource( Equipment[ sltabar ].Resource ).LinkedResource ) then
                    TLayerResource( Equipment[ sltabar ].Resource ).LinkedResource.RLE.DrawMono( ShadowFrame, 0, 0, Picture.Bits, ShadowColor );
                end;
              end;

              Picture.Bits.BaseX := Figure.CenterX - Bits.BaseX;
              Picture.Bits.BaseY := Figure.CenterY + Figure.Z - Bits.BaseY;
              j := round( Lights[ i ].Intensity * 50 );
              D1 := sqrt( sqr( dx ) + sqr( dY ) );
              if D1 > 0 then
              begin
                if ( Lights[ i ].Z > Figure.Height ) then
                  D := D1 / Lights[ i ].Radius
                else
                begin
                  D2 := Lights[ i ].Z / Figure.Height;
                  D := ( 1 - D2 ) + D2 * D1 / Lights[ i ].Radius;
                end;
                Picture.DrawShadow( 0, 0, Bits, j, 100 - j, A, round( ( 50 + 50 * abs( dx ) / D1 ) * D ) );
              end;
              Picture.Bits.BaseX := 0;
              Picture.Bits.BaseY := 0;
            end;
          end;
        end;
      end;
    end;
    LightCount := 0;

    case TSpriteObject( Figure ).SpecialEffect of
      seTranslucent :
        begin
          DstBlend := 100 - TSpriteObject( Figure ).Alpha;
          SrcBlend := TSpriteObject( Figure ).Alpha;
        end;
      seAdd :
        begin
          DstBlend := 100;
          SrcBlend := TSpriteObject( Figure ).Alpha;
        end;
    else
      begin
        DstBlend := 0;
        SrcBlend := 100;
      end;
    end;

    if Figure.Highlighted then
    begin
      Picture.Clear;
      DrawParts( Picture.Bits );

      if DstBlend = 0 then
        Picture.DrawOutline( 0, 0, Bits, Figure.HighlightColor, true )
      else
      begin
        Picture.DrawOutline( 0, 0, Bits, Figure.HighlightColor, false );
        Picture.DrawBlend( 0, 0, Bits, SrcBlend, DstBlend );
      end;
    end
    else if ( DstBlend <> 0 ) or ( SrcBlend <> 100 ) then
    begin
      Picture.Clear;
      DrawParts( Picture.Bits );
      Picture.DrawBlend( 0, 0, Bits, SrcBlend, DstBlend );
    end
    else
    begin
      DrawParts( Bits );
    end;
  end
  else
    inherited;
end;

procedure TCharacterResource.FreeResources;
begin
  inherited;
end;

{ TStringINIFile }

constructor TStringIniFile.Create( const Data : AnsiString );
begin
  FSections := TStringList.Create;
  FData := Data;
  LoadValues;
end;

destructor TStringIniFile.Destroy;
begin
  if FSections <> nil then
    Clear;
  FSections.Free;
end;

function TStringIniFile.AddSection( const Section : string ) : TStrings;
begin
  Result := TStringList.Create;
  try
    FSections.AddObject( Section, Result );
  except
    Result.Free;
  end;
end;

procedure TStringIniFile.Clear;
var
  I : Integer;
begin
  for I := 0 to FSections.Count - 1 do
    TStrings( FSections.Objects[ I ] ).Free;
  FSections.Clear;
end;

procedure TStringIniFile.DeleteKey( const Section, Ident : string );
var
  I, J : Integer;
  Strings : TStrings;
begin
  I := FSections.IndexOf( Section );
  if I >= 0 then
  begin
    Strings := TStrings( FSections.Objects[ I ] );
    J := Strings.IndexOfName( Ident );
    if J >= 0 then
      Strings.Delete( J );
  end;
end;

procedure TStringIniFile.EraseSection( const Section : string );
var
  I : Integer;
begin
  I := FSections.IndexOf( Section );
  if I >= 0 then
  begin
    TStrings( FSections.Objects[ I ] ).Free;
    FSections.Delete( I );
  end;
end;

procedure TStringIniFile.GetStrings( List : TStrings );
var
  I, J : Integer;
  Strings : TStrings;
begin
  List.BeginUpdate;
  try
    for I := 0 to FSections.Count - 1 do
    begin
      List.Add( '[' + FSections[ I ] + ']' );
      Strings := TStrings( FSections.Objects[ I ] );
      for J := 0 to Strings.Count - 1 do
        List.Add( Strings[ J ] );
      List.Add( '' );
    end;
  finally
    List.EndUpdate;
  end;
end;

procedure TStringIniFile.LoadValues;
var
  List : TStringList;
begin
  if ( FData <> '' ) then
  begin
    List := TStringList.Create;
    try
      List.Text := FData;
      SetStrings( List );
    finally
      List.Free;
    end;
  end
  else
    Clear;
end;

procedure TStringIniFile.ReadSection( const Section : string;
  Strings : TStrings );
var
  I, J : Integer;
  SectionStrings : TStrings;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    I := FSections.IndexOf( Section );
    if I >= 0 then
    begin
      SectionStrings := TStrings( FSections.Objects[ I ] );
      for J := 0 to SectionStrings.Count - 1 do
        Strings.Add( SectionStrings.Names[ J ] );
    end;
  finally
    Strings.EndUpdate;
  end;
end;

procedure TStringIniFile.ReadSections( Strings : TStrings );
begin
  Strings.Assign( FSections );
end;

procedure TStringIniFile.ReadSectionValues( const Section : string;
  Strings : TStrings );
var
  I : Integer;
begin
  Strings.BeginUpdate;
  try
    Strings.Clear;
    I := FSections.IndexOf( Section );
    if I >= 0 then
      Strings.Assign( TStrings( FSections.Objects[ I ] ) );
  finally
    Strings.EndUpdate;
  end;
end;

function TStringIniFile.ReadString( const Section, Ident,
  Default : string ) : string;
var
  I : Integer;
  Strings : TStrings;
begin
  I := FSections.IndexOf( Section );
  if I >= 0 then
  begin
    Strings := TStrings( FSections.Objects[ I ] );
    I := Strings.IndexOfName( Ident );
    if I >= 0 then
    begin
      Result := Copy( Strings[ I ], Length( Ident ) + 2, Maxint );
      Exit;
    end;
  end;
  Result := Default;
end;

procedure TStringIniFile.SetStrings( List : TStrings );
var
  I : Integer;
  S : string;
  Strings : TStrings;
begin
  Clear;
  Strings := nil;
  for I := 0 to List.Count - 1 do
  begin
    S := Trim( List[ I ] );
    if ( S <> '' ) and ( S[ 1 ] <> ';' ) then
      if ( S[ 1 ] = '[' ) and ( S[ Length( S ) ] = ']' ) then
        Strings := AddSection( Copy( S, 2, Length( S ) - 2 ) )
      else if Strings <> nil then
        Strings.Add( S );
  end;
end;

procedure TStringIniFile.UpdateFile;
var
  List : TStringList;
begin
  if ( FileName <> '' ) then
  begin
    List := TStringList.Create;
    try
      GetStrings( List );
      List.SaveToFile( FileName );
    finally
      List.Free;
    end;
  end;
end;

procedure TStringIniFile.WriteString( const Section, Ident, Value : string );
var
  I : Integer;
  S : string;
  Strings : TStrings;
begin
  I := FSections.IndexOf( Section );
  if I >= 0 then
    Strings := TStrings( FSections.Objects[ I ] )
  else
    Strings := AddSection( Section );
  S := Ident + '=' + Value;
  I := Strings.IndexOfName( Ident );
  if I >= 0 then
    Strings[ I ] := S
  else
    Strings.Add( S );
end;

{ TProjectileResource }

procedure TProjectileResource.LoadData( INI : TStringIniFile );
begin
  inherited;
  FContactFrame := INI.ReadInteger( 'Action Explode', 'TriggerFrame', 1 );
end;

{ TDoorResource }

procedure TDoorResource.LoadData( INI : TStringINIFile );
var
  S : string;
  i : Integer;
  Actions : TStringList;
const
  FailName : string = 'TDoorResource.LoadData';
begin
  Log.DebugLog(FailName);
  try
    inherited;
    FrameWidth := INI.ReadInteger( 'Header', 'ImageWidth', 96 );
    FrameHeight := INI.ReadInteger( 'Header', 'ImageHeight', 86 );
    S := LowerCase( INI.ReadString( 'Header', 'Blend', '' ) );
    Radius := INI.ReadInteger( 'Header', 'CollisionRadius', 16 );
    FrameMultiplier := INI.ReadInteger( 'Header', 'FrameMultiplier', 1 );
    CenterX := INI.ReadInteger( 'Header', 'CollisionOffset', FrameWidth div 2 );
    CenterY := INI.ReadInteger( 'Header', 'CollisionHeight', FrameHeight - 10 );

    S := INI.ReadString( 'Header', 'Actions', '' );
    Actions := TStringList.Create;
    Actions.CommaText := S;
    for i := 0 to Actions.Count - 1 do
    begin
      LoadAction( INI, AnsiString( Actions.Strings[ i ] ) );
    end;
    Actions.Free;

    S := LowerCase( INI.ReadString( 'Header', 'Highlightable', '' ) );
    if ( S = 'yes' ) then
      Highlightable := True
    else
      Highlightable := False;

    Picture := TBitPlane.Create( FrameWidth, FrameHeight );
    Picture.KeyColor := cTransparent;

    DrawShadow := false;
    ComplexShadow := false;

    UseLighting := false;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TDoorResource.Define( Map : TAniMap; Zone : byte; Index : Word ) : integer;
var
  S : string;
  CollisionMask, DepthAnchors, LineOfSightMask, LightPoints : TArray<Word>;
  Slope, Angle : Single;
  AutoTransparent : Boolean;
  pItem : PItemInfo;
  MultiImage : boolean;
  INI : TStringINIFile;
  ImageIndex : integer;
  BM : IDirectDrawSurface;
  ddck : TDDCOLORKEY;
  BltFx : TDDBLTFX;
  W : integer;
  ColorMatch : integer;
  pr : TRect;
const
  FailName : string = 'TDoorResource.Define';
begin
  result := Index;

  CollisionMask := nil;
  DepthAnchors := nil;
  LineOfSightMask := nil;
  LightPoints := nil;

  Log.DebugLog(FailName);
  try
    if ( FrameWidth mod 4 ) = 0 then
      W := FrameWidth
    else
      W := FrameWidth + 4 - ( FrameWidth mod 4 );
    BM := DDGetSurface( lpDD, W, FrameHeight, cTransparent, false, ColorMatch );

    ddck.dwColorSpaceLowValue := ColorMatch;
    ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;

    BltFx.dwSize := SizeOf( BltFx );
    BltFx.dwFillColor := ColorMatch;

    INI := TStringINIFile.create( Data );
    S := lowercase( INI.ReadString( 'Header', 'GameClassType', '' ) );
    MultiImage := ( S = 'multiimage' ) or ( FrameCount > 1 );
    for ImageIndex := 0 to FrameCount - 1 do
    begin
      if MultiImage then
      begin
        S := INI.ReadString( 'DepthAnchors', 'Frame' + IntToStr( ImageIndex + 1 ), '' );
        if ( S <> '' ) and ( S <> 'XX' ) then
          LoadArray( S, DepthAnchors );
        S := INI.ReadString( 'LightPoints', 'Frame' + IntToStr( ImageIndex + 1 ), '' );
        if ( S <> '' ) and ( S <> 'XX' ) then
          LoadArray( S, LightPoints );
        S := INI.ReadString( 'XRayable', 'Frame' + IntToStr( ImageIndex + 1 ), '' );
      end
      else
      begin
        S := INI.ReadString( 'Header', 'DepthAnchors', '' );
        if ( S <> '' ) and ( S <> 'XX' ) then
          LoadArray( S, DepthAnchors );
        S := INI.ReadString( 'Header', 'LightPoints', '' );
        if ( S <> '' ) and ( S <> 'XX' ) then
          LoadArray( S, LightPoints );
        S := INI.ReadString( 'Header', 'XRayable', '' );
      end;
      Slope := 0; //Evaluate XRaySlope
      AutoTransparent := False;
      if ( LowerCase( S ) = 'no' ) or ( S = '' ) then
      else
      begin
        try
          Angle := UnFormatFP( S );
          if ( Angle <= 360 ) and ( Angle >= 0 ) then
          begin
            if Angle <> 90 then
            begin
              Angle := -PI * Angle / 180;
              Slope := Sin( Angle ) / Cos( Angle ) / 2;
              AutoTransparent := True;
            end;
          end;
        except
        end;
      end;
      if ImageIndex > 0 then
      begin
        pr := Rect( 0, 0, W, FrameHeight );
        BM.Blt( @pr, nil, @pr, DDBLT_COLORFILL + DDBLT_WAIT, @BltFx );
      end;
      GetImage1( ImageIndex, BM, W );
      inc( Index );
      pItem := Map.DefineItem( Zone, Index, BM, DepthAnchors, CollisionMask, LineOfSightMask, LightPoints, Slope, False, AutoTransparent, Vertical );
      if ( ImageIndex = 0 ) then
      begin
        Strips := pItem.StripCount;
        ItemIndex := Index;
        ItemZone := Zone;
      end;
      CollisionMask := nil;
      DepthAnchors := nil;
      LineOfSightMask := nil;
      LightPoints := nil;
    end;
    INI.free;
    BM := nil;
    result := Index;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TDoorResource.RenderLocked( Figure : TAniFigure; Bits : PBITPLANE );
begin
  if Figure.Highlighted and assigned( Picture ) then
  begin
    Picture.Clear;
    RLE.DrawMono( TDoor( Figure ).PrevFrame - 1, 0, 0, Picture.Bits, 0 );
    Picture.DrawOutline( 0, 0, Bits, Figure.HighlightColor, false );
  end;
end;

{ TStaticResource }

function TStaticResource.Define( Map : TAniMap; Zone : byte; Index : word ) : integer;
var
  S : string;
  CollisionMask, LineOfSightMask, DepthAnchors, LightPoints : TArray<Word>;
  Slope, Angle : Single;
  AutoTransparent : Boolean;
  ImageIndex : integer;
  BM : IDirectDrawSurface;
  ddck : TDDCOLORKEY;
  BltFx : TDDBLTFX;
  INI : TStringINIFile;
  MultiImage : boolean;
  W : integer;
  ColorMatch : integer;
  pr : TRect;
const
  FailName : string = 'TStaticResource.Define';
begin
  result := Index;
  CollisionMask := nil;
  DepthAnchors := nil;
  LineOfSightMask := nil;
  LightPoints := nil;

  Log.DebugLog(FailName);
  try

    if ( FrameWidth mod 4 ) = 0 then
      W := FrameWidth
    else
      W := FrameWidth + 4 - ( FrameWidth mod 4 );
    BM := DDGetSurface( lpDD, W, FrameHeight, cTransparent, false, ColorMatch );

    ddck.dwColorSpaceLowValue := ColorMatch;
    ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;

    BltFx.dwSize := SizeOf( BltFx );
    BltFx.dwFillColor := ColorMatch;

    INI := TStringINIFile.create( Data );
    S := lowercase( INI.ReadString( 'Header', 'GameClassType', '' ) );
    MultiImage := ( S = 'multiimage' ) or ( FrameCount > 1 );
    for ImageIndex := 0 to FrameCount - 1 do
    begin
      if MultiImage then
      begin
        S := INI.ReadString( 'CollisionMasks', 'Frame' + IntToStr( ImageIndex + 1 ), '' );
        if ( S <> '' ) and ( S <> 'XX' ) then
          LoadArray( S, CollisionMask );
        S := INI.ReadString( 'LineOfSightMasks', 'Frame' + IntToStr( ImageIndex + 1 ), '' );
        if ( S <> '' ) and ( S <> 'XX' ) then
          LoadArray( S, LineOfSightMask );
        S := INI.ReadString( 'DepthAnchors', 'Frame' + IntToStr( ImageIndex + 1 ), '' );
        if ( S <> '' ) and ( S <> 'XX' ) then
          LoadArray( S, DepthAnchors );
        S := INI.ReadString( 'LightPoints', 'Frame' + IntToStr( ImageIndex + 1 ), '' );
        if ( S <> '' ) and ( S <> 'XX' ) then
          LoadArray( S, LightPoints );
        S := INI.ReadString( 'XRayable', 'Frame' + IntToStr( ImageIndex + 1 ), '' );
      end
      else
      begin
        S := INI.ReadString( 'Header', 'CollisionMask', '' );
        if ( S <> '' ) and ( S <> 'XX' ) then
          LoadArray( S, CollisionMask );
        S := INI.ReadString( 'Header', 'LineOfSightMask', '' );
        if ( S <> '' ) and ( S <> 'XX' ) then
          LoadArray( S, LineOfSightMask );
        S := INI.ReadString( 'Header', 'DepthAnchors', '' );
        if ( S <> '' ) and ( S <> 'XX' ) then
          LoadArray( S, DepthAnchors );
        S := INI.ReadString( 'Header', 'LightPoints', '' );
        if ( S <> '' ) and ( S <> 'XX' ) then
          LoadArray( S, LightPoints );
        S := INI.ReadString( 'Header', 'XRayable', '' );
      end;
      Slope := 0; //Evaluate XRaySlope
      AutoTransparent := False;
      if ( LowerCase( S ) = 'no' ) or ( S = '' ) then
      else
      begin
        try
          Angle := UnFormatFP( S );
          if ( Angle <= 360 ) and ( Angle >= 0 ) then
          begin
            if Angle <> 90 then
            begin
              Angle := -PI * Angle / 180;
              Slope := Sin( Angle ) / Cos( Angle ) / 2;
              AutoTransparent := True;
            end;
          end;
        except
        end;
      end;
      if ImageIndex > 0 then
      begin
        pr := Rect( 0, 0, W, FrameHeight );
        BM.Blt( @pr, nil, @pr, DDBLT_COLORFILL + DDBLT_WAIT, @BltFx );
      end;
      GetImage1( ImageIndex, BM, W );
      inc( Index );
      Map.DefineItem( Zone, Index, BM, DepthAnchors, CollisionMask, LineOfSightMask, LightPoints, Slope, True, AutoTransparent, Vertical );
      CollisionMask := nil;
      DepthAnchors := nil;
      LineOfSightMask := nil;
      LightPoints := nil;
    end;
    INI.free;
    BM := nil;
    result := Index;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TStaticResource.GetImage1( ImageIndex : Integer; Surface : IDirectDrawSurface; W : integer );
var
  Bits : BITPLANE;
  ddsd : TDDSurfaceDesc;
const
  FailName : string = 'TStaticResource.GetImage1';
begin
  Log.DebugLog(FailName);
  try
    ddsd.dwSize := SizeOf( ddsd );
    if Surface.Lock( nil, ddsd, DDLOCK_WAIT, 0 ) = DD_OK then
    begin
      try
        Bits.bitsPtr := ddsd.lpSurface;
        Bits.bitsWdh := W;
        Bits.bitsHgh := FrameHeight;
        Bits.bitsFmt := dfx_pixelformat;
        Bits.bitsPitch := ddsd.lPitch;
        Bits.BaseX := 0;
        Bits.BaseY := 0;
        RLE.Draw( ImageIndex, 0, 0, @Bits );
      finally
        Surface.Unlock( nil );
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TStaticResource.GetImage( ImageIndex : Integer ) : IDirectDrawSurface;
var
  Bits : BITPLANE;
  W : integer;
  ColorMatch : integer;
  ddsd : TDDSurfaceDesc;
const
  FailName : string = 'TStaticResource.GetImage';
begin
  Log.DebugLog(FailName);
  try

    if ( FrameWidth mod 4 ) = 0 then
      W := FrameWidth
    else
      W := FrameWidth + 4 - ( FrameWidth mod 4 );
    result := DDGetSurface( lpDD, W, FrameHeight, cTransparent, false, ColorMatch );

    ddsd.dwSize := SizeOf( ddsd );
    if result.Lock( nil, ddsd, DDLOCK_WAIT, 0 ) = DD_OK then
    begin
      try
        Bits.bitsPtr := ddsd.lpSurface;
        Bits.bitsWdh := W;
        Bits.bitsHgh := FrameHeight;
        Bits.bitsFmt := dfx_pixelformat;
        Bits.bitsPitch := ddsd.lPitch;
        Bits.BaseX := 0;
        Bits.BaseY := 0;
        RLE.Draw( ImageIndex, 0, 0, @Bits );
      finally
        result.Unlock( nil );
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TStaticResource.LoadData( INI : TStringINIFile );
var
  S : string;
const
  FailName : string = 'TStaticResource.LoadData';
begin
  Log.DebugLog(FailName);
  try

    FrameWidth := INI.ReadInteger( 'Header', 'ImageWidth', 0 );
    FrameHeight := INI.ReadInteger( 'Header', 'ImageHeight', 0 );

    S := LowerCase( INI.ReadString( 'Header', 'UseLighting', '' ) );
    if ( S = 'none' ) then
    begin
      UseLighting := False;
      Vertical := false;
    end
    else
    begin
      UseLighting := True;
      Vertical := ( S = 'vert' );
    end;

    Data := INI.Data;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TCastResource }

procedure TCastResource.RenderLocked( Figure : TAniFigure; Bits : PBITPLANE );
var
  Frame : integer;
begin
  if not assigned( RLE ) then
  begin
    if OnDemand then
    begin
      LoadGraphic;
      if not assigned( RLE ) then
        exit;
    end
    else
      exit;
  end;

  Frame := Ord( TCharacter( Figure ).Facing ) * 8 + Figure.ScriptFrame - 1;
  RLE.DrawBlend( Frame, 0, 0, Bits, 100, 100 );
end;

{ TTileResource }

function TTileResource.Define( Map : TAniMap; Zone : byte; Index : word ) : integer;
var
  TileBM : IDirectDrawSurface;
  ddck : TDDCOLORKEY;
  BltFx : TDDBLTFX;
  i : Integer;
  W : integer;
  ColorMatch : integer;
  pr: TRect;
const
  FailName : string = 'TTileResource.Define';
begin
  result := Index;
  Log.DebugLog(FailName);
  try

    if ( FrameWidth mod 4 ) = 0 then
      W := FrameWidth
    else
      W := FrameWidth + 4 - ( FrameWidth mod 4 );
    TileBM := DDGetSurface( lpDD, W, FrameHeight, cTransparent, false, ColorMatch );

    ddck.dwColorSpaceLowValue := ColorMatch;
    ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;

    BltFx.dwSize := SizeOf( BltFx );
    BltFx.dwFillColor := ColorMatch;

    Result := Index + FrameCount;
    for i := 0 to FrameCount - 1 do
    begin
      if i > 0 then
      begin
        pr := Rect( 0, 0, W, FrameHeight );
        TileBM.Blt( @pr, nil, @pr, DDBLT_COLORFILL + DDBLT_WAIT, @BltFx );
      end;
      GetImage1( i, TileBM, W );
      Map.DefineTile( Zone, Index + i, TileBM );
    end;
    TileBM := nil;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TTileResource.Destroy;
const
  FailName : string = 'TTileResource.Destroy';
begin
  Log.DebugLog(FailName);
  try
    INI.free;
    inherited;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TTileResource.LoadData( INI : TStringINIFile );
const
  FailName : string = 'TTileResource.LoadData';
begin
  Log.DebugLog(FailName);
  try
    inherited;
    self.INI := TStringINIFile.create( Data );
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TLayerResource }

procedure TLayerResource.SetReload( Value : boolean );
begin
  FReload := Value;
  if Value and Assigned( Linkedresource ) then
    Linkedresource.FReload := true;
end;

procedure TLayerResource.RenderLocked( Figure : TAniFigure; Bits : PBITPLANE );
begin
  if not assigned( RLE ) then
  begin
    if OnDemand then
    begin
      LoadGraphic;
      if not assigned( RLE ) then
        exit;
    end
    else
      exit;
  end;

  if not assigned( Figure ) then
  begin
    RLE.Draw( ItemFrame, 0, 0, Bits );
  end
  else if Figure is TCharacter then
  begin
    if Figure.UseLighting then
      RLE.DrawColorize( Figure.Frame - 1, 0, 0, Bits,
        100 * ( Figure.LightR + TSpriteObject( Figure ).ColorR ) div 255,
        100 * ( Figure.LightG + TSpriteObject( Figure ).ColorG ) div 255,
        100 * ( Figure.LightB + TSpriteObject( Figure ).ColorB ) div 255, 100, 0 )
    else
      RLE.Draw( Figure.Frame - 1, 0, 0, Bits );
  end
  else
    inherited;
end;

procedure TLayerResource.LoadData( INI : TStringINIFile );
var
  S, S1 : string;
  arr: TArray<string>;
  i, j : integer;
const
  FailName : string = 'TLayerResource.LoadData';
begin
  Log.DebugLog(FailName);
  try

    inherited;
    ItemFrame := INI.ReadInteger( 'Header', 'GameImageFrame', 191 );
    S := INI.ReadString( 'Header', 'LinkedLayerFile', '' );
    if S <> '' then
    begin
      S1 := ExtractFilePath( LinkPath );
      S1 := copy( S1, 1, length( S1 ) - 1 );
      S1 := ExtractFileName( S1 ) + '\' + S;
      LinkedResource := PartManager.GetLayerResource( S1 );
    end;
    LinkPath := '';
    S := INI.ReadString( 'Header', 'LayeredFramesToBack', '' );
    arr := S.Split([',']);
    for i := 0 to Length(arr)-1 do
    begin
      j := StrToIntDef( arr[i], -1 );
      if j >= 0 then
        BackLayer[ j ] := True;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TInventoryResource }

procedure TInventoryResource.LoadData( INI : TStringINIFile );
const
  FailName : string = 'TInventoryResource.LoadData';
begin
  Log.DebugLog(FailName);
  try
    FrameWidth := INI.ReadInteger( 'Header', 'ImageWidth', 96 );
    FrameHeight := INI.ReadInteger( 'Header', 'ImageHeight', 86 );
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TInventoryResource.RenderLocked( Figure : TAniFigure; Bits : PBITPLANE );
begin
  RLE.Draw( 0, 0, 0, Bits );
end;

procedure TInventoryResource.RenderShadowLocked( Figure : TAniFigure; Bits : PBITPLANE );
begin
  RLE.Draw( 1, 0, 0, Bits );
end;

end.
