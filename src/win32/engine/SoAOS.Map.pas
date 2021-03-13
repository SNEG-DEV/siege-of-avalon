unit SoAOS.Map;
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

  Description: Was part of AniDec30.pas and AniGrp30.pas

  Requires: Delphi 10.3.3 or later

  Revision History:
  - 13 Jul 2003 - DL: Initial Upload to CVS
  - 10 Mar 2019 - SN: Forked on GitHub
  see git repo afterwards

*)

interface

uses
  Winapi.Windows,
  System.UITypes,
  System.Classes,
  System.Generics.Collections,
  Vcl.Graphics,
  DirectX,
  LogFile;

const
  MaxLightStates = 4;

type
  PGridInfo = ^GridInfo;

  GridInfo = packed record
    Figure: Pointer; // For collision detection
    FilterID: Smallint;
    TriggerID: Smallint;
    CollisionMask: Word;
    LineOfSightMask: Word;
    FilterMask: Word;
    TriggerMask: Word;
    Tile: array [0 .. 4] of Word;
    Zone: array [0 .. 4] of Byte;
    BitField: Byte; // Bit 7 denotes a diamond tile, Bit 6 is automap.
  end;

  PTileInfo = ^TileInfo;

  TileInfo = packed record
    ImageIndex: Word;
    Rows: Word;
    Columns: Word;
    Zone: Word;
    Element: Byte;
    Reserved: Byte;
  end;

  PItemInfo = ^ItemInfo;

  ItemInfo = packed record
    Top: Longint;
    Left: Longint;
    Slope: Single;
    StripHeights: HGLOBAL;
    CollisionMasks: HGLOBAL;
    LineOfSightMasks: HGLOBAL;
    LightPoints: HGLOBAL;
    Width: Word;
    Height: Word;
    Strips: Word; // =roundup(Width/TileWidth)  Strips*Height=next Items top
    StripCount: Word;
    Used: Boolean;
    Visible: Boolean;
    AutoTransparent: Boolean;
    Vertical: Boolean;
  end;

  PItemInstanceInfo = ^ItemInstanceInfo;

  ItemInstanceInfo = packed record
    X: Longint;
    Y: Longint;
    ImageY: Word;
    Slope0: Single;
    Slope1: Single;
    Slope2: Single;
    RefItem: Word;
    FilterID: Smallint;
    XRayID: Smallint;
    ImageX: Word;
    Width: Word;
    Height: Word;
    VHeight: Word; // Height of region that may obscure objects behind it
    Next: Word;
    Zone: Word;
    AutoTransparent: Boolean;
    Visible: Boolean;
    Last: Boolean;
    Vertical: Boolean;
  end;

type
  TPixelFormat = (pf555, pf565, pf888);

  TFlicker = (flNone, flCandle, flTorch, flFluorescent);

  TAniMap = class;

  TZone = class(TObject)
  const
    MaxItems = 2047;
    MaxTiles = 2047;
    MaxZoneHeight = 2048;
  private
    // Filename          :string;
    FIndex: Integer;
    // Tile Information
    FTileBitWidth: Longint;
    FTileBitHeight: Longint;
    FTileMaxIndex: Word;
    FTileMaxColumnIndex: Word;
    FTileMem: ^TileInfo;
    // Item Information
    FItemBitWidth: Longint;
    FItemBitHeight: Longint;
    FItemMem: ^ItemInfo;
    FItemColumn: Integer;
    FItemColumnBitHeight: Integer;
    FMap: TAniMap;
    TilesInVideo: Boolean;
    ItemsInVideo: Boolean;

    function GetTile(i: Integer): TileInfo;
    procedure SetTile(i: Integer; Tile: TileInfo);
    function GetItem(i: Integer): ItemInfo;
    procedure SetItem(i: Integer; Item: ItemInfo);
    property Tile[i: Integer]: TileInfo read GetTile write SetTile;
    property Item[i: Integer]: ItemInfo read GetItem write SetItem;
  public
    X1: Longint;
    Y1: Longint;
    X2: Longint;
    Y2: Longint;
    Loaded: Boolean;
    // New - was private
    FullRefresh: Boolean;
    FItemImages: IDirectDrawSurface;
    FTileImages: IDirectDrawSurface;
    //
    constructor Create(Map: TAniMap);
    destructor Destroy; override;
    procedure Release;
    procedure DefineTile(Index: Word; Image: IDirectDrawSurface; Color: TColor);
    function DefineItem(Index: Word; Image: IDirectDrawSurface;
      const StripHeights, CollisionMasks, LineOfSightMasks,
      LightPoints: array of Word; Color: TColor; Slope: Single;
      Visible, AutoTransparent, Vertical: Boolean): PItemInfo;
    procedure MoveToVideo;
    procedure DisposeDef;
    class procedure Skip(Stream: TStream); virtual;
    procedure SaveToStream(Stream: TStream; SaveImage: Boolean); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure LoadTileCustomData(Stream: TStream); virtual;
    procedure SaveTileCustomData(Stream: TStream); virtual;
    procedure LoadItemCustomData(Stream: TStream); virtual;
    procedure SaveItemCustomData(Stream: TStream); virtual;
    procedure ExportTiles(Filename: string);
    procedure ExportItems(Filename: string);
    property Index: Integer read FIndex;
    // New
    property TileMaxColumnIndex: Word read FTileMaxColumnIndex;
  end;

  TLightZone = class(TZone)
  private
    AddColumn: Boolean;
    procedure AddStrip(Image: IDirectDrawSurface; var NewX, NewY: Word);
    procedure NewTileState;
    procedure NewItemState;
  public
    Color: TColor;
    Intensity: Integer;
    Radius: Longint;
    Flicker: TFlicker;
    X: Longint;
    Y: Longint;
    Z: Longint;
    // New - was private
    FlickerX: array [1 .. MaxLightStates] of Longint;
    FlickerY: array [1 .. MaxLightStates] of Longint;
    FlickerZ: array [1 .. MaxLightStates] of Longint;
    FlickerRadius: array [1 .. MaxLightStates] of Longint;
    FlickerIntensity: array [1 .. MaxLightStates] of Longint;
    OverlapZones: TList<TLightZone>;
    States: Integer;
    State: Integer;
    ItemStateOffset: Longint;
    TileStateOffset: Longint;
    StateDuration: Integer;
    Blinking: Boolean;
    Items: TList;
    //
    constructor Create(Map: TAniMap);
    destructor Destroy; override;
    procedure LoadTileCustomData(Stream: TStream); override;
    procedure SaveTileCustomData(Stream: TStream); override;
    procedure LoadItemCustomData(Stream: TStream); override;
    procedure SaveItemCustomData(Stream: TStream); override;
  end;

  TAniMap = class(TComponent)
  const
    ItemListSize = 32767;
  private
    FWidth: Longint;
    FHeight: Longint;
    FBitWidth: Longint;
    FBitHeight: Longint;
    FMapData: HGLOBAL;
    FTransparentColor: TColor;
    FColorMatch: Word;
    NeedColorMatch: Boolean;
    FAmbientColor: TColor;
    FAmbientIntensity: Integer;

    FUseLighting: Boolean;
    FUseAmbientOnly: Boolean;
    FTileSize: Word;
    FTileHeight: Word;
    FTileWidth: Word;
    FStripWidth: Word;
    FStripHeight: Word;

    // SubMaps           :TList;
    procedure SetWidth(VWidth: Longint);
    procedure SetHeight(VHeight: Longint);
    procedure SetTileSize(Size: Word);
    procedure SetAmbientColor(Color: TColor);
    procedure SetAmbientIntensity(Value: Integer);
    function GetZoneCount: Word;
    procedure SetTransparentColor(Color: TColor);
    function GetColorMatch: Word;
  protected
  public
    Zones: TList<TZone>;
    // New - was private
    LightR: double;
    LightG: double;
    LightB: double;
    // Item Instance Information
    FItemList: array [1 .. ItemListSize] of ItemInstanceInfo;
    FirstItem: Word;
    LastItem: Word;
    //
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddItem(Zone, ItemIndex: Word; X, Y, Z: Longint;
      FilterID: Smallint; Collidable: Boolean): PItemInstanceInfo;
    procedure Clear;
    procedure FreeResources;
    procedure FreeDefinitions;
    function GetTile(X, Y: Longint): PGridInfo;
    procedure SetTile(X, Y, Layer: Longint; Zone, Index: Word);
    procedure SetDiamondTile(X, Y: Longint; ZoneTop, TileTop, ZoneRight,
      TileRight, ZoneBottom, TileBottom, ZoneLeft, TileLeft: Word);
    procedure SetCollisionMask(X, Y: Longint; CollisionMask: Word);
    procedure SetLineOfSightMask(X, Y: Longint; LineOfSightMask: Word);
    procedure SetTrigger(X, Y: Longint; TriggerID: Smallint); overload;
    procedure SetTrigger(X, Y: Longint; TriggerID: Smallint;
      TriggerMask: Word); overload;
    procedure SetFilter(X, Y: Longint; FilterID: Smallint); overload;
    procedure SetFilter(X, Y: Longint; FilterID: Smallint;
      FilterMask: Word); overload;
    procedure DefineTile(Zone, Index: Word; Image: IDirectDrawSurface);
    function DefineItem(Zone, Index: Word; Image: IDirectDrawSurface;
      const StripHeights, CollisionMasks, LineOfSightMasks,
      LightPoints: array of Word; Slope: Single;
      Visible, AutoTransparent, Vertical: Boolean): PItemInfo;
    procedure Sort;
    procedure RenderMap;
    procedure MoveZoneToVideo(Index: Integer);
    function GetZoneMemoryUsage(Index: Integer): Longint;
    function AddZone: Word;
    function AddLightZone: Word;
    function AddLight(Color: TColor; Intensity: Integer; Radius: Longint;
      Flicker: TFlicker; X, Y, Z: Longint): Word;
    function LineOfSight(X1, Y1, X2, Y2: Longint): Boolean;
    function LineOfCollision(X1, Y1, X2, Y2: Longint): Boolean;
    function GetItemIndex(Item: PItemInstanceInfo): Word;
    function GetItemAddress(Index: Word): PItemInstanceInfo;
    procedure SaveMapKnownInfo(Stream: TStream);
    procedure LoadMapKnownInfo(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);
    property TileWidth: Word read FTileWidth;
    property TileHeight: Word read FTileHeight;
    property ItemCount: Word read LastItem;
    property ColorMatch: Word read GetColorMatch;
    // New
    property BitWidth: Longint read FBitWidth;
    property BitHeight: Longint read FBitHeight;
    property MapData: HGLOBAL read FMapData;
    property StripWidth: Word read FStripWidth;
    property StripHeight: Word read FStripHeight;
  published
    property Width: Longint read FWidth write SetWidth default 10;
    property Height: Longint read FHeight write SetHeight default 20;
    property TileSize: Word read FTileSize write SetTileSize default 4;
    property TransparentColor: TColor read FTransparentColor
      write SetTransparentColor default clFuchsia;
    property AmbientIntensity: Integer read FAmbientIntensity
      write SetAmbientIntensity;
    property AmbientColor: TColor read FAmbientColor write SetAmbientColor;
    property UseLighting: Boolean read FUseLighting write FUseLighting;
    property UseAmbientOnly: Boolean read FUseAmbientOnly write FUseAmbientOnly;
    property ZoneCount: Word read GetZoneCount;
  end;

procedure CreateMask(var Picture, Mask: HBITMAP; BITMAP: TBitmap;
  Color: TColor);

procedure GetStripHeights(var StripHeights: HGLOBAL; Mask: HBITMAP;
  W, H, StripWidth: Word);

function ATan(X, Y: Single): Single;

function FindColorMatch(Color: TColor): Word;

implementation

uses
  System.SysUtils,
  SoAOS.Graphics.Draw,
  SoAOS.Animation,
  DXUtil;

procedure CreateMask(var Picture, Mask: HBITMAP; BITMAP: TBitmap;
  Color: TColor);
var
  TempBitmap: TBitmap;
  DC, MaskDC: HDC;
  OldPicture, OldMask: HBITMAP;
  OldPalette: HPALETTE;
  ScreenDC: HDC;
begin
  if (Picture <> 0) then
    DeleteObject(Picture);
  Picture := 0;
  if (Mask <> 0) then
    DeleteObject(Mask);
  Mask := 0;

  TempBitmap := TBitmap.Create;
  TempBitmap.Assign(BITMAP);
  TempBitmap.TRANSPARENT := True;
  TempBitmap.TransparentMode := tmFixed;
  TempBitmap.TransparentColor := Color;
  TempBitmap.Canvas.Pen.Color := clBlack;
  ScreenDC := GetDC(0);

  DC := CreateCompatibleDC(ScreenDC);
  OldPalette := SelectPalette(DC, TempBitmap.Palette, False);
  MaskDC := CreateCompatibleDC(ScreenDC);
  Picture := CreateCompatibleBitmap(ScreenDC, BITMAP.Width, BITMAP.Height);
  ReleaseDC(0, ScreenDC);
  Mask := TempBitmap.ReleaseMaskHandle;

  OldMask := SelectObject(MaskDC, Mask);
  OldPicture := SelectObject(DC, Picture);
  BitBlt(DC, 0, 0, BITMAP.Width, BITMAP.Height, BITMAP.Canvas.Handle, 0,
    0, SRCCOPY);
  PatBlt(MaskDC, 0, 0, BITMAP.Width, BITMAP.Height, DSTINVERT);
  BitBlt(DC, 0, 0, BITMAP.Width, BITMAP.Height, MaskDC, 0, 0, SRCAND);
  PatBlt(MaskDC, 0, 0, BITMAP.Width, BITMAP.Height, DSTINVERT);

  SelectPalette(DC, OldPalette, False);
  SelectObject(MaskDC, OldMask);
  SelectObject(DC, OldPicture);

  TempBitmap.Free;

  DeleteDC(MaskDC);
  DeleteDC(DC);
end;

procedure GetStripHeights(var StripHeights: HGLOBAL; Mask: HBITMAP;
  W, H, StripWidth: Word);
var
  bmi: ^TBitmapInfo;
  ghBitmapInfo: HGLOBAL;
  DC: HDC;
  RowSize: Longint;
  hBits: HGLOBAL;
  BitsBase, Bits: ^Byte;
  BitOffset, ByteOffset: Integer;
  Strips: Integer;
  i, j, k: Integer;
  BytesCovered: Integer;
  BitMask, EndBits: Byte;
  MaxBit: Word;
  StripData: ^Word;
const
  FailName: string = 'AniDec30.GetStripHeights';
begin
  Log.DebugLog(FailName);
  try

    RowSize := W div 8;
    if ((W mod 8) <> 0) then
      Inc(RowSize);
    if ((RowSize mod 4) <> 0) then
      Inc(RowSize, 4 - (RowSize mod 4));
    Strips := W div StripWidth;
    if ((W mod StripWidth) <> 0) then
      Inc(Strips);
    StripHeights := GlobalAlloc(GHND, Strips * SizeOf(Word));
    StripData := GlobalLock(StripHeights);
    hBits := GlobalAlloc(GPTR, H * RowSize);
    BitsBase := GlobalLock(hBits);
    ghBitmapInfo := GlobalAlloc(GPTR, SizeOf(TBitmapInfoHeader) + 1024);
    bmi := GlobalLock(ghBitmapInfo);
    bmi^.bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
    bmi^.bmiHeader.biPlanes := 1;
    bmi^.bmiHeader.biWidth := W;
    bmi^.bmiHeader.biHeight := H;
    bmi^.bmiHeader.biBitCount := 1;
    bmi^.bmiHeader.biCompression := BI_RGB;
    DC := GetDC(0);
    GetDIBits(DC, Mask, 0, H, BitsBase, bmi^, DIB_RGB_COLORS);
    ReleaseDC(0, DC);

    for i := 1 to Strips do
    begin
      ByteOffset := ((i - 1) * StripWidth) div 8;
      BitOffset := ((i - 1) * StripWidth) mod 8;
      BytesCovered := ((StripWidth + BitOffset) div 8);
      if (((StripWidth + BitOffset) mod 8) <> 0) then
        Inc(BytesCovered);
      MaxBit := 0;
      for j := 1 to BytesCovered do
      begin
        BitMask := $FF;
        if (j = 1) then
        begin
          if (StripWidth < 8) then
          begin
            BitMask := not((1 shl StripWidth) - 1);
          end;
          BitMask := BitMask shr BitOffset;
        end;
        if (j = BytesCovered) then
        begin
          if (((StripWidth + BitOffset) mod 8) <> 0) then
          begin
            EndBits := not(1 shl (8 - ((StripWidth + BitOffset) mod 8)) - 1);
            BitMask := BitMask and EndBits;
          end;
        end;
        if (i = Strips) then
        begin
          if ((ByteOffset + j) * 8 > W) then
          begin
            if ((ByteOffset + j) * 8 > W + 8) then
              BitMask := 0
            else
            begin
              EndBits := not(1 shl (8 - (W mod 8)) - 1);
              BitMask := BitMask and EndBits;
            end;
          end;
        end;
        Bits := BitsBase;
        Inc(Bits, ByteOffset + j - 1);
        for k := H downto 1 do
        begin
          if ((Bits^ and BitMask) <> BitMask) then
          begin
            if (k > MaxBit) then
              MaxBit := k;
            Break;
          end;
          Inc(Bits, RowSize);
        end;
      end;
      StripData^ := MaxBit;
      Inc(StripData);
    end;

    GlobalUnlock(hBits);
    GlobalFree(hBits);
    GlobalUnlock(ghBitmapInfo);
    GlobalFree(ghBitmapInfo);
    GlobalUnlock(StripHeights);

  except
    on E: Exception do
      Log.Log(FailName, E.Message, []);
  end;
end;

function ATan(X, Y: Single): Single;
begin
  if (X = 0) then
  begin
    if (Y >= 0) then
      Result := PI / 2
    else
      Result := 3 * PI / 2;
  end
  else if (X > 0) then
  begin
    if (Y >= 0) then
      Result := ArcTan(Y / X)
    else
      Result := ArcTan(Y / X) + 2 * PI;
  end
  else
  begin
    Result := ArcTan(Y / X) + PI;
  end;
  if Result < 0 then
    Result := Result + 2 * PI;
end;

function FindColorMatch(Color: TColor): Word;
begin
  Result := SoAOS_DX_ColorMatch(lpDDSBack, Color);
end;

{ TZone }

constructor TZone.Create(Map: TAniMap);
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
  FTileImages := nil;
  FItemImages := nil;
  inherited;
end;

function TZone.GetTile(i: Integer): TileInfo;
var
  P: ^TileInfo;
begin
  if (i > 0) and (i <= MaxTiles) then
  begin
    P := Pointer(FTileMem);
    Inc(P, i - 1);
    Result := P^;
  end;
end;

procedure TZone.SetTile(i: Integer; Tile: TileInfo);
var
  P: ^TileInfo;
begin
  if (i > 0) and (i <= MaxTiles) then
  begin
    if (FTileMem = nil) then
    begin
      GetMem(FTileMem, SizeOf(TileInfo) * MaxTiles);
      ZeroMemory(FTileMem, SizeOf(TileInfo) * MaxTiles);
    end;
    P := Pointer(FTileMem);
    Inc(P, i - 1);
    P^ := Tile;
  end;
end;

procedure TZone.DisposeDef;
var
  i: Integer;
begin
  if Assigned(FItemMem) then
  begin
    for i := 1 to MaxItems do
    begin
      if Item[i].Used then
      begin
        if (Item[i].StripHeights <> 0) then
        begin
          GlobalFree(Item[i].StripHeights);
        end;
        if (Item[i].CollisionMasks <> 0) then
        begin
          GlobalFree(Item[i].CollisionMasks);
        end;
        if (Item[i].LineOfSightMasks <> 0) then
        begin
          GlobalFree(Item[i].LineOfSightMasks);
        end;
        if (Item[i].LightPoints <> 0) then
        begin
          GlobalFree(Item[i].LightPoints);
        end;
      end;
    end;
  end;

  if Assigned(FTileMem) then
    FreeMem(FTileMem);
  FTileMem := nil;
  if Assigned(FItemMem) then
    FreeMem(FItemMem);
  FItemMem := nil;
end;

function TZone.GetItem(i: Integer): ItemInfo;
var
  P: ^ItemInfo;
begin
  if (i > 0) and (i <= MaxItems) then
  begin
    P := Pointer(FItemMem);
    Inc(P, i - 1);
    Result := P^;
  end;
end;

procedure TZone.SetItem(i: Integer; Item: ItemInfo);
var
  P: ^ItemInfo;
  j: Integer;
begin
  if (i > 0) and (i <= MaxItems) then
  begin
    if (FItemMem = nil) then
    begin
      GetMem(FItemMem, SizeOf(ItemInfo) * MaxItems);
      ZeroMemory(FItemMem, SizeOf(ItemInfo) * MaxItems);
      P := Pointer(FItemMem);
      for j := 1 to MaxItems do
      begin
        P^.Used := False;
        Inc(P);
      end;
    end;
    P := Pointer(FItemMem);
    Inc(P, i - 1);
    if (P^.CollisionMasks <> 0) then
      GlobalFree(P^.CollisionMasks);
    if (P^.LineOfSightMasks <> 0) then
      GlobalFree(P^.LineOfSightMasks);
    if (P^.StripHeights <> 0) then
      GlobalFree(P^.StripHeights);
    if (P^.LightPoints <> 0) then
      GlobalFree(P^.LightPoints);
    P^ := Item;
    P^.Used := True;
  end;
end;

procedure TZone.Release;
begin
  FTileImages := nil;
  FItemImages := nil;
end;

function TZone.DefineItem(Index: Word; Image: IDirectDrawSurface;
  const StripHeights, CollisionMasks, LineOfSightMasks,
  LightPoints: array of Word; Color: TColor; Slope: Single;
  Visible, AutoTransparent, Vertical: Boolean): PItemInfo;
var
  SrcDC: HDC;
  Picture, Mask: HBITMAP;
  NewBitWidth, NewBitHeight: Longint;
  i, X: Integer;
  StripData, TileData: ^Word;
  Strips, Rows, Tiles: Integer;
  W, H: Integer;
  NewItemImages: IDirectDrawSurface;
  ddsd: TDDSurfaceDesc;
  ddck: TDDCOLORKEY;
  // BltFx: DDBLTFX;
  SrcX1, SrcX2, SrcY1, SrcY2: Integer;
  BITMAP: TBitmap;
  NewItem: ItemInfo;
  pr: TRect;
begin
  FillChar(ddsd, sizeof(ddsd), 0);
  if Loaded then
  begin
    Result := nil;
    exit;
  end;

  NewItem.StripHeights := 0;
  NewItem.CollisionMasks := 0;
  NewItem.LineOfSightMasks := 0;
  NewItem.LightPoints := 0;
  if (Image = nil) then
  begin
    NewItem.Width := 0;
    NewItem.Height := 0;
    NewItem.Top := 0;
    NewItem.Left := 0;
    NewItem.Strips := 0;
    NewItem.StripCount := 0;
    Item[Index] := NewItem;
  end
  else
  begin
    GetSurfaceDims(W, H, Image);
    NewItem.Width := W;
    NewItem.Height := H;
    NewItem.Strips := W div FMap.FTileWidth;
    NewItem.StripCount := W div FMap.FStripWidth;
    NewItem.Slope := Slope;
    NewItem.AutoTransparent := AutoTransparent;
    NewItem.Vertical := Vertical;
    NewItem.Visible := Visible;
    if ((W mod FMap.FTileWidth) <> 0) then
      Inc(NewItem.Strips);
    if ((W mod FMap.FStripWidth) <> 0) then
      Inc(NewItem.StripCount);
    NewBitWidth := (FItemColumn + 1) * FMap.FTileWidth;
    NewBitHeight := FItemColumnBitHeight + NewItem.Strips * NewItem.Height;
    if NewBitHeight > MaxZoneHeight then
    begin
      Inc(FItemColumn);
      FItemColumnBitHeight := 0;
      NewBitHeight := FItemColumnBitHeight + NewItem.Strips * NewItem.Height;
      Inc(NewBitWidth, FMap.FTileWidth);
    end;
    if NewBitWidth < FItemBitWidth then
      NewBitWidth := FItemBitWidth;
    if NewBitHeight < FItemBitHeight then
      NewBitHeight := FItemBitHeight;
    X := FItemColumn * FMap.FTileWidth;
    NewItem.Top := FItemColumnBitHeight;
    NewItem.Left := X;

    // Strip Height Data
    if (High(StripHeights) < 0) then
    begin
      Log.Log('Generating default depth anchors');
      BITMAP := TBitmap.Create;
      BITMAP.Width := W;
      BITMAP.Height := H;
      Image.GetDC(SrcDC);
      BitBlt(BITMAP.Canvas.Handle, 0, 0, W, H, SrcDC, 0, 0, SRCCOPY);
      Image.ReleaseDC(SrcDC);
      CreateMask(Picture, Mask, BITMAP, ColorToRGB(Color));
      BITMAP.Free;
      GetStripHeights(NewItem.StripHeights, Mask, NewItem.Width, NewItem.Height,
        FMap.FStripWidth);
      DeleteObject(Picture);
      DeleteObject(Mask);
    end
    else
    begin
      Strips := NewItem.Strips shl 2;
      NewItem.StripHeights := GlobalAlloc(GHND, Strips * SizeOf(Word));
      StripData := GlobalLock(NewItem.StripHeights);
      for i := 0 to High(StripHeights) do
      begin
        if (i >= Strips) then
          Break;
        StripData^ := StripHeights[i];
        Inc(StripData);
      end;
      GlobalUnlock(NewItem.StripHeights);
    end;

    // Collision Data
    if (High(CollisionMasks) >= 0) then
    begin
      Rows := NewItem.Height div FMap.FTileHeight;
      if ((NewItem.Height mod FMap.FTileHeight) <> 0) then
        Inc(Rows);
      Tiles := Rows * NewItem.Strips;
      NewItem.CollisionMasks := GlobalAlloc(GHND, Tiles * SizeOf(Word));
      TileData := GlobalLock(NewItem.CollisionMasks);
      for i := 0 to High(CollisionMasks) do
      begin
        if (i >= Tiles) then
          Break;
        TileData^ := CollisionMasks[i];
        Inc(TileData);
      end;
      GlobalUnlock(NewItem.CollisionMasks);
    end;

    // Line of Sight Data
    if (High(LineOfSightMasks) >= 0) then
    begin
      Rows := NewItem.Height div FMap.FTileHeight;
      if ((NewItem.Height mod FMap.FTileHeight) <> 0) then
        Inc(Rows);
      Tiles := Rows * NewItem.Strips;
      NewItem.LineOfSightMasks := GlobalAlloc(GHND, Tiles * SizeOf(Word));
      TileData := GlobalLock(NewItem.LineOfSightMasks);
      for i := 0 to High(LineOfSightMasks) do
      begin
        if (i >= Tiles) then
          Break;
        TileData^ := LineOfSightMasks[i];
        Inc(TileData);
      end;
      GlobalUnlock(NewItem.LineOfSightMasks);
    end;

    // Light Points
    Strips := NewItem.Strips shl 2;
    NewItem.LightPoints := GlobalAlloc(GHND, Strips * SizeOf(Word));
    StripData := GlobalLock(NewItem.LightPoints);
    for i := 0 to High(LightPoints) - 1 do
    begin
      if (i >= Strips) then
        Break;
      StripData^ := Word(LightPoints[i + 1] - LightPoints[i]);
      Inc(StripData);
    end;
    GlobalUnlock(NewItem.LightPoints);

    if (NewBitWidth > FItemBitWidth) or (NewBitHeight > FItemBitHeight) then
    begin
      // Log.Log('Resize Zone Items: '+inttostr(FIndex));
      // Log.Log(inttostr(NewBitWidth)+'x'+inttostr(NewBitHeight));
      // inc(NewBitWidth,FMap.FTileWidth);
      ddsd.dwSize := SizeOf(ddsd);
      ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
      ddsd.dwWidth := NewBitWidth;
      ddsd.dwHeight := NewBitHeight;
      if ItemsInVideo then
      begin
        ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_VIDEOMEMORY;
        if lpdd_CreateSurface(ddsd, NewItemImages, nil) <> DD_OK then
        begin
          ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
          lpdd_CreateSurface(ddsd, NewItemImages, nil);
          ItemsInVideo := False;
        end;
      end
      else
      begin
        ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
        lpdd_CreateSurface(ddsd, NewItemImages, nil);
      end;

      { BltFx.dwSize := SizeOf(BltFx);
        BltFx.dwFillColor := FMap.ColorMatch;
        NewItemImages.Blt(Rect(0, 0, NewBitWidth, NewBitHeight), nil,
        Rect(0, 0, NewBitWidth, NewBitHeight), DDBLT_COLORFILL + DDBLT_WAIT, BltFx); }

      if Assigned(FItemImages) then
      begin
        pr := Rect(0, 0, FItemBitWidth, FItemBitHeight);
        NewItemImages.BltFast(0, 0, FItemImages, @pr, DDBLTFAST_NOCOLORKEY or
          DDBLTFAST_WAIT);
      end;
      ddck.dwColorSpaceLowValue := FMap.ColorMatch;
      ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
      NewItemImages.SetColorKey(DDCKEY_SRCBLT, @ddck);

      FItemImages := nil;
      FItemImages := NewItemImages;

      if NewBitWidth > FItemBitWidth then
        FItemBitWidth := NewBitWidth;
      if NewBitHeight > FItemBitHeight then
        FItemBitHeight := NewBitHeight;
    end;

    for i := 1 to NewItem.Strips do
    begin
      SrcX1 := (i - 1) * FMap.FTileWidth;
      SrcY1 := 0;
      SrcX2 := SrcX1 + FMap.FTileWidth;
      SrcY2 := SrcY1 + NewItem.Height;
      if SrcX2 > W then
        SrcX2 := W;
      if SrcY2 > H then
        SrcY2 := H;
      pr := Rect(SrcX1, SrcY1, SrcX2, SrcY2);
      FItemImages.BltFast(X, FItemColumnBitHeight + (i - 1) * NewItem.Height,
        Image, @pr, DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT);
    end;

    FItemColumnBitHeight := FItemColumnBitHeight + NewItem.Strips *
      NewItem.Height;
    Item[Index] := NewItem;
  end;
  Result := Pointer(FItemMem);
  Inc(Result, Index - 1);
end;

procedure TZone.DefineTile(Index: Word; Image: IDirectDrawSurface;
  Color: TColor);
var
  NewMaxIndex: Word;
  NewBitWidth, NewBitHeight: Longint;
  i, j, X, Y: Integer;
  NewTile: TileInfo;
  W, H: Integer;
  NewTileImages: IDirectDrawSurface;
  ddsd: TDDSurfaceDesc;
  ddck: TDDCOLORKEY;
  BltFx: TDDBLTFX;
  SrcX1, SrcX2, SrcY1, SrcY2: Integer;
  pr: TRect;
begin
  FillChar(ddsd, sizeof(ddsd), 0);
  if Loaded then
    exit;

  if (Image = nil) then
  begin
    NewTile.ImageIndex := 0;
    NewTile.Rows := 0;
    NewTile.Columns := 0;
    NewTile.Element := 0;
    Tile[Index] := NewTile;
  end
  else
  begin
    GetSurfaceDims(W, H, Image);
    NewTile.ImageIndex := FTileMaxIndex;
    NewTile.Columns := W div FMap.FTileWidth;
    if ((W mod FMap.FTileWidth) <> 0) then
      Inc(NewTile.Columns);
    NewTile.Rows := H div FMap.FTileHeight;
    if ((H mod FMap.FTileHeight) <> 0) then
      Inc(NewTile.Rows);
    NewMaxIndex := FTileMaxIndex + NewTile.Columns * NewTile.Rows;
    NewBitWidth := (((NewMaxIndex - 1) div FTileMaxColumnIndex) + 1) *
      FMap.FTileWidth;
    if NewBitWidth > FMap.FTileWidth then
      NewBitHeight := MaxZoneHeight
    else
      NewBitHeight := NewMaxIndex * FMap.FTileHeight;
    // X := ( ( FTileMaxIndex - 1 ) div FTileMaxColumnIndex ) * FMap.FTileWidth; // original - seems to cause pink artifacts
    X := (FTileMaxIndex div FTileMaxColumnIndex) * FMap.FTileWidth;
    // fix by rucksacksepp
    Y := (FTileMaxIndex mod FTileMaxColumnIndex) * FMap.FTileHeight;
    // if ( ( FTileMaxIndex mod FTileMaxColumnIndex ) = 0 ) and ( FTileMaxIndex > 0 ) then // original - seems to cause pink artifacts
    // TODO: Check if below condition ever ends up true - otherwise remove.
    if ((FTileMaxIndex mod FTileMaxColumnIndex) < 0) and (FTileMaxIndex > 0)
    then // fix by rucksacksepp
      Inc(X, FMap.FTileWidth);
    Tile[Index] := NewTile;
    if (NewBitWidth > FTileBitWidth) or (NewBitHeight > FTileBitHeight) then
    begin
      // Log.Log('Resize Zone Tiles');
      // Log.Log(inttostr(NewBitWidth)+' '+inttostr(FTileBitWidth));
      // Log.Log(inttostr(NewBitHeight)+' '+inttostr(FTileBitHeight));
      ddsd.dwSize := SizeOf(ddsd);
      ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
      ddsd.dwWidth := NewBitWidth;
      ddsd.dwHeight := NewBitHeight;
      if TilesInVideo then
      begin
        ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_VIDEOMEMORY;
        if lpdd_CreateSurface(ddsd, NewTileImages, nil) <> DD_OK then
        begin
          ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
          lpdd_CreateSurface(ddsd, NewTileImages, nil);
          TilesInVideo := False;
        end;
      end
      else
      begin
        ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
        lpdd_CreateSurface(ddsd, NewTileImages, nil);
      end;

      BltFx.dwSize := SizeOf(BltFx);
      BltFx.dwFillColor := FMap.ColorMatch;
      pr := Rect(0, 0, NewBitWidth, NewBitHeight);
      NewTileImages.Blt(@pr, nil, @pr, DDBLT_COLORFILL + DDBLT_WAIT, @BltFx);
      if Assigned(FTileImages) then
      begin
        pr := Rect(0, 0, FTileBitWidth, FTileBitHeight);
        NewTileImages.BltFast(0, 0, FTileImages, @pr, DDBLTFAST_NOCOLORKEY or
          DDBLTFAST_WAIT);
      end;

      ddck.dwColorSpaceLowValue := FMap.ColorMatch;
      ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
      NewTileImages.SetColorKey(DDCKEY_SRCBLT, @ddck);

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
          Inc(X, FMap.FTileWidth);
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
        pr := Rect(SrcX1, SrcY1, SrcX2, SrcY2);
        FTileImages.BltFast(X, Y, Image, @pr, DDBLTFAST_SRCCOLORKEY or
          DDBLTFAST_WAIT);
        Inc(Y, FMap.FTileHeight);
      end;
    end;

    FTileMaxIndex := NewMaxIndex;
  end;
end;

procedure TZone.ExportTiles(Filename: string);
var
  BM: TBitmap;
  DC: HDC;
begin
  try
    if Assigned(FTileImages) then
    begin
      BM := TBitmap.Create;
      try
        BM.Width := FTileBitWidth;
        BM.Height := FTileBitHeight;
        FTileImages.GetDC(DC);
        BitBlt(BM.Canvas.Handle, 0, 0, FTileBitWidth, FTileBitHeight, DC, 0,
          0, SRCCOPY);
        FTileImages.ReleaseDC(DC);
        BM.SaveToFile(Filename);
      finally
        BM.Free;
      end;
    end;
  except
  end;
end;

procedure TZone.ExportItems(Filename: string);
var
  BM: TBitmap;
  DC: HDC;
begin
  try
    if Assigned(FItemImages) then
    begin
      BM := TBitmap.Create;
      try
        BM.Width := FItemBitWidth;
        BM.Height := FItemBitHeight;
        FItemImages.GetDC(DC);
        BitBlt(BM.Canvas.Handle, 0, 0, FItemBitWidth, FItemBitHeight, DC, 0,
          0, SRCCOPY);
        FItemImages.ReleaseDC(DC);
        BM.SaveToFile(Filename);
      finally
        BM.Free;
      end;
    end;
  except
  end;
end;

procedure TZone.SaveToStream(Stream: TStream; SaveImage: Boolean);
var
  ddsd: TDDSurfaceDesc;
  L: Longint;
  i: Integer;
  P: ^Byte;
  MemSize: Longint;
  P0, P1: Longint;
begin
  FillChar(ddsd, sizeof(ddsd), 0);
  // Log.Log('Saving tiles '+IntToStr(FTileBitWidth)+'x'+IntToStr(FTileBitHeight));
  Stream.write(FTileBitWidth, SizeOf(FTileBitWidth));
  Stream.write(FTileBitHeight, SizeOf(FTileBitHeight));
  L := 0;
  Stream.write(L, SizeOf(L));

  if SaveImage and (FTileBitWidth > 0) and (FTileBitHeight > 0) then
  begin
    ddsd.dwSize := SizeOf(ddsd);
    if FTileImages.Lock(nil, ddsd, DDLOCK_WAIT, 0) = DD_OK then
    begin
      try
        P0 := Stream.position;
        SaveTileCustomData(Stream);
        MemSize := FTileBitWidth * 2;
        L := MemSize * FTileBitHeight + Stream.position - P0;
        P1 := Stream.position;
        Stream.position := P0 - SizeOf(L);
        Stream.write(L, SizeOf(L));
        Stream.position := P1;
        P := ddsd.lpSurface;
        for i := 1 to FTileBitHeight do
        begin
          Stream.write(P^, MemSize);
          Inc(P, ddsd.lPitch);
        end;
      finally
        FTileImages.Unlock(nil);
      end;
    end;
  end;

  // Log.Log('Saving items '+IntToStr(FItembitWidth)+'x'+IntToStr(FItemBitHeight));
  Stream.write(FItemBitWidth, SizeOf(FItemBitWidth));
  Stream.write(FItemBitHeight, SizeOf(FItemBitHeight));
  L := 0;
  Stream.write(L, SizeOf(L));

  if SaveImage and (FItemBitWidth > 0) and (FItemBitHeight > 0) then
  begin
    ddsd.dwSize := SizeOf(ddsd);
    if FItemImages.Lock(nil, ddsd, DDLOCK_WAIT, 0) = DD_OK then
    begin
      try
        P0 := Stream.position;
        SaveItemCustomData(Stream);
        MemSize := FItemBitWidth * 2;
        L := MemSize * FItemBitHeight + Stream.position - P0;
        P1 := Stream.position;
        Stream.position := P0 - SizeOf(L);
        Stream.write(L, SizeOf(L));
        Stream.position := P1;
        P := ddsd.lpSurface;
        for i := 1 to FItemBitHeight do
        begin
          Stream.write(P^, MemSize);
          Inc(P, ddsd.lPitch);
        end;
      finally
        FItemImages.Unlock(nil);
      end;
    end
  end;
end;

class procedure TZone.Skip(Stream: TStream);
var
  L: Longint;
begin
  Stream.read(L, SizeOf(L));
  Stream.read(L, SizeOf(L));
  Stream.read(L, SizeOf(L));
  Stream.seek(L, soFromCurrent);
  Stream.read(L, SizeOf(L));
  Stream.read(L, SizeOf(L));
  Stream.read(L, SizeOf(L));
  Stream.seek(L, soFromCurrent);
end;

procedure TZone.LoadTileCustomData(Stream: TStream);
begin
  Stream.read(FTileMaxIndex, SizeOf(FTileMaxIndex));
  Stream.read(FTileMaxColumnIndex, SizeOf(FTileMaxColumnIndex));
  Stream.read(X1, SizeOf(X1));
  Stream.read(Y1, SizeOf(Y1));
  Stream.read(X2, SizeOf(X2));
  Stream.read(Y2, SizeOf(Y2));
end;

procedure TZone.SaveTileCustomData(Stream: TStream);
begin
  Stream.write(FTileMaxIndex, SizeOf(FTileMaxIndex));
  Stream.write(FTileMaxColumnIndex, SizeOf(FTileMaxColumnIndex));
  Stream.write(X1, SizeOf(X1));
  Stream.write(Y1, SizeOf(Y1));
  Stream.write(X2, SizeOf(X2));
  Stream.write(Y2, SizeOf(Y2));
end;

procedure TZone.LoadItemCustomData(Stream: TStream);
begin
  Stream.read(FItemColumn, SizeOf(FItemColumn));
  Stream.read(FItemColumnBitHeight, SizeOf(FItemColumnBitHeight));
end;

procedure TZone.SaveItemCustomData(Stream: TStream);
begin
  Stream.write(FItemColumn, SizeOf(FItemColumn));
  Stream.write(FItemColumnBitHeight, SizeOf(FItemColumnBitHeight));
end;

procedure TZone.LoadFromStream(Stream: TStream);
var
  L: Longint;
  ddsd: TDDSurfaceDesc;
  ddck: TDDCOLORKEY;
  BltFx: TDDBLTFX;
  i: Integer;
  P: ^Byte;
  MemSize: Longint;
  pr: TRect;
begin
  FillChar(ddsd, sizeof(ddsd), 0);
  if Loaded then
    exit;
  Stream.read(FTileBitWidth, SizeOf(FTileBitWidth));
  Stream.read(FTileBitHeight, SizeOf(FTileBitHeight));
  Stream.read(L, SizeOf(L));
  // Log.Log('Sizing tiles to '+IntToStr(FTileBitWidth)+'x'+IntToStr(FTileBitHeight));

  if (FTileBitWidth > 0) and (FTileBitHeight > 0) then
  begin
    FTileImages := nil;
    ddsd.dwSize := SizeOf(ddsd);
    ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
    ddsd.dwWidth := FTileBitWidth;
    ddsd.dwHeight := FTileBitHeight;
    { if TilesInVideo then begin
      ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_VIDEOMEMORY;
      if lpdd.CreateSurface(ddsd, FTileImages, nil) <> DD_OK then begin
      ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
      lpdd.CreateSurface(ddsd, FTileImages, nil);
      TilesInVideo:=false;
      end;
      end
      else begin }
    ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
    lpdd_CreateSurface(ddsd, FTileImages, nil);
    // end;

    BltFx.dwSize := SizeOf(BltFx);
    BltFx.dwFillColor := FMap.ColorMatch;
    pr := Rect(0, 0, FTileBitWidth, FTileBitHeight);
    FTileImages.Blt(@pr, nil, @pr, DDBLT_COLORFILL + DDBLT_WAIT, @BltFx);

    ddck.dwColorSpaceLowValue := FMap.ColorMatch;
    ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
    FTileImages.SetColorKey(DDCKEY_SRCBLT, @ddck);
  end;

  if L > 0 then
  begin
    LoadTileCustomData(Stream);
    if Assigned(FTileImages) then
    begin
      ddsd.dwSize := SizeOf(ddsd);
      if FTileImages.Lock(nil, ddsd, DDLOCK_WAIT, 0) = DD_OK then
      begin
        try
          // Log.Log('Loading tiles from cache (zone '+inttostr(index)+')');
          MemSize := FTileBitWidth * 2;
          P := ddsd.lpSurface;
          for i := 1 to FTileBitHeight do
          begin
            Stream.read(P^, MemSize);
            Inc(P, ddsd.lPitch);
          end;
        finally
          FTileImages.Unlock(nil);
        end;
      end;
      Loaded := True;
    end;
  end;

  Stream.read(FItemBitWidth, SizeOf(FItemBitWidth));
  Stream.read(FItemBitHeight, SizeOf(FItemBitHeight));
  Stream.read(L, SizeOf(L));
  // Log.Log('Sizing items to '+IntToStr(FItemBitWidth)+'x'+IntToStr(FItemBitHeight));

  if (FItemBitWidth > 0) and (FItemBitHeight > 0) then
  begin
    FItemImages := nil;
    ddsd.dwSize := SizeOf(ddsd);
    ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
    ddsd.dwWidth := FItemBitWidth;
    ddsd.dwHeight := FItemBitHeight;
    { if ItemsInVideo then begin
      ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_VIDEOMEMORY;
      if lpdd.CreateSurface(ddsd, FItemImages, nil) <> DD_OK then begin
      ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
      lpdd.CreateSurface(ddsd, FItemImages, nil);
      ItemsInVideo:=false;
      end;
      end
      else begin }
    ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
    lpdd_CreateSurface(ddsd, FItemImages, nil);
    // end;

    ddck.dwColorSpaceLowValue := FMap.ColorMatch;
    ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
    FItemImages.SetColorKey(DDCKEY_SRCBLT, @ddck);
  end;

  if L > 0 then
  begin
    LoadItemCustomData(Stream);
    if Assigned(FItemImages) then
    begin
      ddsd.dwSize := SizeOf(ddsd);
      if FItemImages.Lock(nil, ddsd, DDLOCK_WAIT, 0) = DD_OK then
      begin
        try
          // Log.Log('Loading items from cache (zone '+inttostr(index)+')');
          MemSize := FItemBitWidth * 2;
          P := ddsd.lpSurface;
          for i := 1 to FItemBitHeight do
          begin
            Stream.read(P^, MemSize);
            Inc(P, ddsd.lPitch);
          end;
        finally
          FItemImages.Unlock(nil);
        end;
      end;
      Loaded := True;
    end;
  end;
  FullRefresh := TilesInVideo and ItemsInVideo;
end;

procedure TZone.MoveToVideo;
var
  NewTileImages: IDirectDrawSurface;
  NewItemImages: IDirectDrawSurface;
  ddsd: TDDSurfaceDesc;
  ddck: TDDCOLORKEY;
  i, Rem: Integer;
  L: longword;
  pr: TRect;
const
  SectionHeight = 256;
begin
  FillChar(ddsd, sizeof(ddsd), 0);
  if not ItemsInVideo then
  begin
    if FItemBitHeight = 0 then
    begin
      ItemsInVideo := True;
    end
    else
    begin
      ddsd.dwSize := SizeOf(ddsd);
      ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
      ddsd.dwWidth := FItemBitWidth;
      ddsd.dwHeight := FItemBitHeight;
      ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_VIDEOMEMORY;
      if lpdd_CreateSurface(ddsd, NewItemImages, nil) = DD_OK then
      begin
        for i := 0 to FItemBitHeight div SectionHeight - 1 do
        begin
          pr := Rect(0, i * SectionHeight, FItemBitWidth,
            (i + 1) * SectionHeight);
          NewItemImages.BltFast(0, i * SectionHeight, FItemImages, @pr,
            DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT);
        end;
        Rem := FItemBitHeight mod SectionHeight;
        if Rem > 0 then
        begin
          pr := Rect(0, FItemBitHeight - Rem, FItemBitWidth, FItemBitHeight);
          NewItemImages.BltFast(0, FItemBitHeight - Rem, FItemImages, @pr,
            DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT);
        end;

        FItemImages := nil;
        FItemImages := NewItemImages;
        ddck.dwColorSpaceLowValue := FMap.ColorMatch;
        ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
        FItemImages.SetColorKey(DDCKEY_SRCBLT, @ddck);
        ItemsInVideo := True;
      end
      else
      begin
        if not(self is TLightZone) then
        begin // Poke the zone to reduce stuttering
          if FItemImages.Lock(nil, ddsd, DDLOCK_WAIT, 0) = DD_OK then
          begin
            L := Longint(ddsd.lpSurface^);
            Longint(ddsd.lpSurface^) := L;
            FItemImages.Unlock(nil);
          end;
        end;
      end;
    end;
    if ItemsInVideo then
      Log.Log('Zone ' + IntToStr(Index) + ' items moved to video')
    else
      Log.Log('Zone ' + IntToStr(Index) + ' items could not be moved to video');
  end;

  if not TilesInVideo then
  begin
    if (FTileBitHeight = 0) then
    begin
      TilesInVideo := True;
    end
    else
    begin
      ddsd.dwSize := SizeOf(ddsd);
      ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
      ddsd.dwWidth := FTileBitWidth;
      ddsd.dwHeight := FTileBitHeight;
      ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_VIDEOMEMORY;
      if lpdd_CreateSurface(ddsd, NewTileImages, nil) = DD_OK then
      begin
        for i := 0 to FTileBitHeight div SectionHeight - 1 do
        begin
          pr := Rect(0, i * SectionHeight, FTileBitWidth,
            (i + 1) * SectionHeight);
          NewTileImages.BltFast(0, i * SectionHeight, FTileImages, @pr,
            DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT);
        end;
        Rem := FTileBitHeight mod SectionHeight;
        if Rem > 0 then
        begin
          pr := Rect(0, FTileBitHeight - Rem, FTileBitWidth, FTileBitHeight);
          NewTileImages.BltFast(0, FTileBitHeight - Rem, FTileImages, @pr,
            DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT);
        end;

        FTileImages := nil;
        FTileImages := NewTileImages;
        ddck.dwColorSpaceLowValue := FMap.ColorMatch;
        ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
        FTileImages.SetColorKey(DDCKEY_SRCBLT, @ddck);
        TilesInVideo := True;
      end
      else
      begin
        if not(self is TLightZone) then
        begin // Poke the zone to reduce stuttering
          if FTileImages.Lock(nil, ddsd, DDLOCK_WAIT, 0) = DD_OK then
          begin
            L := Longint(ddsd.lpSurface^);
            Longint(ddsd.lpSurface^) := L;
            FTileImages.Unlock(nil);
          end;
        end;
      end;
    end;
    if TilesInVideo then
      Log.Log('Zone ' + IntToStr(Index) + ' tiles moved to video')
    else
      Log.Log('Zone ' + IntToStr(Index) + ' tiles could not be moved to video');
  end;
end;

{ TLightZone }

constructor TLightZone.Create(Map: TAniMap);
begin
  inherited;
  FItemBitWidth := Map.FStripWidth;
end;

procedure TLightZone.AddStrip(Image: IDirectDrawSurface; var NewX, NewY: Word);
var
  NewBitWidth, NewBitHeight: Longint;
  W, H: Integer;
  NewItemImages: IDirectDrawSurface;
  ddsd: TDDSurfaceDesc;
  ddck: TDDCOLORKEY;
  // BltFx: DDBLTFX;
  pr: TRect;
begin
  FillChar(ddsd, sizeof(ddsd), 0);
  if (Image = nil) then
    exit;
  GetSurfaceDims(W, H, Image);
  if AddColumn or (FItemColumnBitHeight + H > MaxZoneHeight) then
  begin
    AddColumn := False;
    Inc(FItemColumn);
    NewBitWidth := (FItemColumn + 1) * FMap.FStripWidth;
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

  if (NewBitHeight > FItemBitHeight) or (NewBitWidth > FItemBitWidth) then
  begin
    ddsd.dwSize := SizeOf(ddsd);
    ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
    ddsd.dwWidth := NewBitWidth;
    ddsd.dwHeight := NewBitHeight;
    ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
    lpdd_CreateSurface(ddsd, NewItemImages, nil);

    { BltFx.dwSize := SizeOf(BltFx);
      BltFx.dwFillColor := FMap.ColorMatch;
      NewItemImages.Blt(Rect(0, 0, NewBitWidth, NewBitHeight), nil,
      Rect(0, 0, NewBitWidth, NewBitHeight), DDBLT_COLORFILL + DDBLT_WAIT, BltFx); }

    if Assigned(FItemImages) then
    begin
      pr := Rect(0, 0, FItemBitWidth, FItemBitHeight);
      NewItemImages.BltFast(0, 0, FItemImages, @pr, DDBLTFAST_NOCOLORKEY or
        DDBLTFAST_WAIT);
    end;
    pr := Rect(0, 0, W, H);
    NewItemImages.BltFast(NewX, NewY, Image, @pr, DDBLTFAST_NOCOLORKEY or
      DDBLTFAST_WAIT);

    ddck.dwColorSpaceLowValue := FMap.ColorMatch;
    ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
    NewItemImages.SetColorKey(DDCKEY_SRCBLT, @ddck);

    FItemImages := nil;
    FItemImages := NewItemImages;
    if NewBitWidth > FItemBitWidth then
      FItemBitWidth := NewBitWidth;
    FItemBitHeight := NewBitHeight;
  end
  else
  begin
    pr := Rect(0, 0, W, H);
    FItemImages.BltFast(NewX, NewY, Image, @pr, DDBLTFAST_NOCOLORKEY or
      DDBLTFAST_WAIT);
  end;

  Inc(FItemColumnBitHeight, H);
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
  AddColumn := True;
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

procedure TLightZone.LoadTileCustomData(Stream: TStream);
var
  i, L: Longint;
begin
  if Assigned(OverlapZones) then
  begin
    OverlapZones.Free;
    OverlapZones := nil;
  end;
  Stream.read(L, SizeOf(L));
  if L > 0 then
  begin
    OverlapZones := TList<TLightZone>.Create;
    OverlapZones.Capacity := L;
    for i := 0 to L - 1 do
    begin
      Stream.read(L, SizeOf(L));
      OverlapZones.Add(TLightZone(FMap.Zones[L]));
    end;
  end;

  if Assigned(Items) then
  begin
    Items.Free;
    Items := nil;
  end;
  Stream.read(L, SizeOf(L));
  if L > 0 then
  begin
    Items := TList.Create;
    Items.Capacity := L;
    for i := 0 to L - 1 do
    begin
      Stream.read(L, SizeOf(L));
      Items.Add(@FMap.FItemList[L]);
    end;
  end;

  Stream.read(States, SizeOf(States));
  Stream.read(State, SizeOf(State));
  Stream.read(TileStateOffset, SizeOf(TileStateOffset));
  Stream.read(ItemStateOffset, SizeOf(ItemStateOffset));
  Stream.read(FlickerX, SizeOf(FlickerX));
  Stream.read(FlickerY, SizeOf(FlickerY));
  Stream.read(FlickerZ, SizeOf(FlickerZ));
  Stream.read(FlickerRadius, SizeOf(FlickerRadius));
  Stream.read(FlickerIntensity, SizeOf(FlickerIntensity));
  Stream.read(Color, SizeOf(Color));
  Stream.read(Intensity, SizeOf(Intensity));
  Stream.read(Radius, SizeOf(Radius));
  Stream.read(Flicker, SizeOf(Flicker));
  Stream.read(X, SizeOf(X));
  Stream.read(Y, SizeOf(Y));
  Stream.read(Z, SizeOf(Z));
  if Flicker <> flNone then
  begin
    TilesInVideo := True;
    ItemsInVideo := True;
  end;

  inherited;
end;

procedure TLightZone.SaveTileCustomData(Stream: TStream);
var
  i, L: Longint;
begin
  if Assigned(OverlapZones) then
  begin
    L := OverlapZones.Count;
    Stream.write(L, SizeOf(L));
    for i := 0 to L - 1 do
    begin
      L := OverlapZones[i].Index;
      Stream.write(L, SizeOf(L));
    end;
  end
  else
  begin
    L := 0;
    Stream.write(L, SizeOf(L));
  end;

  if Assigned(Items) then
  begin
    L := Items.Count;
    Stream.write(L, SizeOf(L));
    for i := 0 to L - 1 do
    begin
      L := FMap.GetItemIndex(PItemInstanceInfo(Items[i]));
      Stream.write(L, SizeOf(L));
    end;
  end
  else
  begin
    L := 0;
    Stream.write(L, SizeOf(L));
  end;

  Stream.write(States, SizeOf(States));
  Stream.write(State, SizeOf(State));
  Stream.write(TileStateOffset, SizeOf(TileStateOffset));
  Stream.write(ItemStateOffset, SizeOf(ItemStateOffset));
  Stream.write(FlickerX, SizeOf(FlickerX));
  Stream.write(FlickerY, SizeOf(FlickerY));
  Stream.write(FlickerZ, SizeOf(FlickerZ));
  Stream.write(FlickerRadius, SizeOf(FlickerRadius));
  Stream.write(FlickerIntensity, SizeOf(FlickerIntensity));
  Stream.write(Color, SizeOf(Color));
  Stream.write(Intensity, SizeOf(Intensity));
  Stream.write(Radius, SizeOf(Radius));
  Stream.write(Flicker, SizeOf(Flicker));
  Stream.write(X, SizeOf(X));
  Stream.write(Y, SizeOf(Y));
  Stream.write(Z, SizeOf(Z));

  inherited;
end;

procedure TLightZone.LoadItemCustomData(Stream: TStream);
begin
  inherited;
end;

procedure TLightZone.SaveItemCustomData(Stream: TStream);
begin
  inherited;
end;

{ TAniMap }

constructor TAniMap.Create(AOwner: TComponent);
var
  GridSize, i: Longint;
  GridLoc: ^GridInfo;
  NewZone: TZone;
begin
  inherited Create(AOwner);
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
  NeedColorMatch := True;
  if not(csDesigning in ComponentState) then
  begin
    if (FWidth > 0) and (FHeight > 0) then
    begin
      GridSize := FWidth * FHeight;
      FMapData := GlobalAlloc(GPTR, GridSize * SizeOf(GridInfo));
      GridLoc := GlobalLock(FMapData);
      for i := 1 to GridSize do
      begin
        GridLoc^.Tile[0] := $FFFF;
        GridLoc^.Tile[1] := $FFFF;
        Inc(GridLoc);
      end;
      GlobalUnlock(FMapData);
    end;
    Zones := TList<TZone>.Create;
    NewZone := TZone.Create(self);
    Zones.Add(NewZone);
  end;
end;

destructor TAniMap.Destroy;
var
  i: Integer;
begin
  if (FMapData <> 0) then
  begin
    GlobalFree(FMapData);
    FMapData := 0;
  end;
  for i := 0 to Zones.Count - 1 do
    Zones[i].Free;
  Zones.Free;
  inherited Destroy;
end;

procedure TAniMap.SaveToStream(Stream: TStream);
var
  GridLoc: ^GridInfo;
  MemSize: Longint;
begin
  Stream.write(FirstItem, SizeOf(FirstItem));
  Stream.write(LastItem, SizeOf(LastItem));
  Stream.write(FItemList[1], LastItem * SizeOf(ItemInstanceInfo));
  Stream.write(FWidth, SizeOf(FWidth));
  Stream.write(FHeight, SizeOf(FHeight));
  MemSize := FWidth * FHeight * SizeOf(GridInfo);
  GridLoc := GlobalLock(FMapData);
  Stream.write(GridLoc^, MemSize);
end;

procedure TAniMap.LoadFromStream(Stream: TStream);
var
  GridLoc: ^GridInfo;
  MemSize: Longint;
begin
  Stream.read(FirstItem, SizeOf(FirstItem));
  Stream.read(LastItem, SizeOf(LastItem));
  Stream.read(FItemList[1], LastItem * SizeOf(ItemInstanceInfo));
  Stream.read(FWidth, SizeOf(FWidth));
  Stream.read(FHeight, SizeOf(FHeight));
  FBitWidth := FWidth * FTileWidth;
  FBitHeight := FHeight * FTileHeight;

  MemSize := FWidth * FHeight * SizeOf(GridInfo);
  if (FMapData <> 0) then
  begin
    GlobalFree(FMapData);
    FMapData := 0;
  end;
  FMapData := GlobalAlloc(GMEM_FIXED, MemSize);
  GridLoc := GlobalLock(FMapData);
  Stream.read(GridLoc^, MemSize);
end;

function TAniMap.GetItemIndex(Item: PItemInstanceInfo): Word;
begin
  Result := 1 + (longword(Item) - longword(@FItemList[1]))
    div SizeOf(ItemInstanceInfo);
end;

function TAniMap.GetItemAddress(Index: Word): PItemInstanceInfo;
begin
  Result := @FItemList[Index];
end;

procedure TAniMap.FreeResources;
var
  i: Integer;
begin
  for i := 0 to Zones.Count - 1 do
  begin
    Zones[i].FTileImages := nil;
    Zones[i].FItemImages := nil;
    Zones[i].FTileBitWidth := 0;
    Zones[i].FTileBitHeight := 0;
    Zones[i].FTileMaxIndex := 0;
    Zones[i].FTileMaxColumnIndex := 0;
    Zones[i].FItemBitWidth := 0;
    Zones[i].FItemBitHeight := 0;
    Zones[i].FItemColumn := 0;
    Zones[i].FItemColumnBitHeight := 0;
  end;
end;

procedure TAniMap.FreeDefinitions;
var
  i: Integer;
begin
  for i := 0 to Zones.Count - 1 do
  begin
    Zones[i].DisposeDef;
  end;
end;

procedure TAniMap.Clear;
var
  i: Longint;
  NewZone: TZone;
begin
  if (FMapData <> 0) then
  begin
    GlobalFree(FMapData);
    FMapData := 0;
  end;
  FreeResources;
  for i := 0 to Zones.Count - 1 do
    Zones[i].Free;
  Zones.Clear;
  NewZone := TZone.Create(self);
  Zones.Add(NewZone);
  FirstItem := 0;
  LastItem := 0;
end;

procedure TAniMap.SetWidth(VWidth: Longint);
var
  i, GridSize: Longint;
  GridLoc: ^GridInfo;
begin
  FWidth := VWidth;
  FBitWidth := FWidth * FTileWidth;
  if (FMapData <> 0) then
  begin
    GlobalFree(FMapData);
    FMapData := 0;
  end;
  if (FWidth > 0) and (FHeight > 0) then
  begin
    GridSize := FWidth * FHeight;
    FMapData := GlobalAlloc(GPTR, GridSize * SizeOf(GridInfo));
    GridLoc := GlobalLock(FMapData);
    for i := 1 to GridSize do
    begin
      GridLoc^.Tile[0] := $FFFF;
      GridLoc^.Tile[1] := $FFFF;
      Inc(GridLoc);
    end;
    GlobalUnlock(FMapData);
  end;
end;

procedure TAniMap.Sort;
var
  i, j, Y: Integer;
  MinY, MaxY: Longint;
  pBase, P: ^Longint;
  MemSize: Longint;
begin
  // Sort ItemList
  if LastItem > 0 then
  begin
    MinY := FItemList[1].Y;
    MaxY := MinY;
    for i := 2 to LastItem do
    begin
      if FItemList[i].Y < MinY then
        MinY := FItemList[i].Y
      else if FItemList[i].Y > MaxY then
        MaxY := FItemList[i].Y;
    end;
    MemSize := (MaxY - MinY + 1) * SizeOf(Longint);
    GetMem(pBase, MemSize);
    try
      ZeroMemory(pBase, MemSize);
      for i := LastItem downto 1 do
      begin
        Y := FItemList[i].Y - MinY;
        P := pBase;
        Inc(P, Y);
        FItemList[i].Next := P^;
        P^ := i;
      end;

      P := pBase;
      FirstItem := P^; // we know the first one is valid
      j := FirstItem;
      while FItemList[j].Next <> 0 do
      begin
        j := FItemList[j].Next;
      end;
      for i := 1 to (MaxY - MinY) do
      begin
        Inc(P);
        if P^ <> 0 then
        begin
          FItemList[j].Next := P^;
          j := P^;
          while FItemList[j].Next <> 0 do
          begin
            j := FItemList[j].Next;
          end;
        end;
      end;
    finally
      FreeMem(pBase);
    end;
  end;
end;

procedure TAniMap.SetHeight(VHeight: Longint);
var
  i, GridSize: Longint;
  GridLoc: ^GridInfo;
begin
  FHeight := VHeight;
  FBitHeight := FHeight * FTileHeight;
  if (FMapData <> 0) then
  begin
    GlobalFree(FMapData);
    FMapData := 0;
  end;
  if (FWidth > 0) and (FHeight > 0) then
  begin
    GridSize := FWidth * FHeight;
    FMapData := GlobalAlloc(GPTR, GridSize * SizeOf(GridInfo));
    GridLoc := GlobalLock(FMapData);
    for i := 1 to GridSize do
    begin
      GridLoc^.Tile[0] := $FFFF;
      GridLoc^.Tile[1] := $FFFF;
      Inc(GridLoc);
    end;
    GlobalUnlock(FMapData);
  end;
end;

procedure TAniMap.SetAmbientColor(Color: TColor);
begin
  FAmbientColor := ColorToRGB(Color);
  LightR := (FAmbientColor and $FF) * (FAmbientIntensity / 100);
  LightG := ((FAmbientColor and $FF00) shr 8) * (FAmbientIntensity / 100);
  LightB := ((FAmbientColor and $FF0000) shr 16) * (FAmbientIntensity / 100);
end;

procedure TAniMap.SetAmbientIntensity(Value: Integer);
begin
  FAmbientIntensity := Value;
  LightR := round((FAmbientColor and $FF) * (FAmbientIntensity / 100));
  LightG := round(((FAmbientColor and $FF00) shr 8) *
    (FAmbientIntensity / 100));
  LightB := round(((FAmbientColor and $FF0000) shr 16) *
    (FAmbientIntensity / 100));
end;

function TAniMap.GetZoneMemoryUsage(Index: Integer): Longint;
begin
  with Zones[Index] do
  begin
    Result := FItemBitWidth * FItemBitHeight + FTileBitWidth * FTileBitHeight;
    // Log.Log(inttostr(FItemBitWidth)+'x'+inttostr(FitemBitHeight));
  end;
end;

procedure TAniMap.MoveZoneToVideo(Index: Integer);
begin
  if not(Zones[Index].TilesInVideo and Zones[Index].ItemsInVideo) then
    Zones[Index].MoveToVideo;
end;

function TAniMap.GetZoneCount: Word;
begin
  Result := Zones.Count;
end;

procedure TAniMap.SetTransparentColor(Color: TColor);
begin
  FTransparentColor := Color;
  if DXMode then
  begin
    FColorMatch := FindColorMatch(Color);
    NeedColorMatch := False;
  end
  else
  begin
    NeedColorMatch := True;
  end;
end;

function TAniMap.GetColorMatch: Word;
begin
  if NeedColorMatch then
  begin
    FColorMatch := FindColorMatch(FTransparentColor);
    NeedColorMatch := False;
  end;

  Result := FColorMatch;
end;

function TAniMap.AddZone: Word;
var
  NewZone: TZone;
begin
  NewZone := TZone.Create(self);
  NewZone.FIndex := Zones.Add(NewZone);
  Result := NewZone.Index;
end;

function TAniMap.AddLightZone: Word;
var
  NewZone: TZone;
begin
  NewZone := TLightZone.Create(self);
  NewZone.FIndex := Zones.Add(NewZone);
  Result := NewZone.Index;
end;

function TAniMap.AddLight(Color: TColor; Intensity: Integer; Radius: Longint;
  Flicker: TFlicker; X, Y, Z: Longint): Word;
var
  NewZone: TLightZone;
  R2: Longint;
begin
  Result := 0;
  if Radius <= 0 then
    exit;
  NewZone := TLightZone.Create(self);
  NewZone.Color := ColorToRGB(Color);
  NewZone.Intensity := Intensity;
  NewZone.Radius := Radius;
  NewZone.Flicker := Flicker;
  NewZone.State := 1;
  NewZone.X := X;
  NewZone.Y := Y;
  NewZone.Z := Z;
  NewZone.FIndex := Zones.Add(NewZone);
  NewZone.X1 := (X - Radius) div FTileWidth;
  if (Radius mod FTileWidth) = 0 then
    dec(NewZone.X1);
  if NewZone.X1 < 0 then
    NewZone.X1 := 0;
  NewZone.X2 := (X + Radius) div FTileWidth;
  if NewZone.X2 >= Width then
    NewZone.X2 := Width - 1;

  R2 := Radius div 2;
  NewZone.Y1 := (Y - R2) div FTileHeight;
  if (R2 mod FTileHeight) = 0 then
    dec(NewZone.Y1);
  if NewZone.Y1 < 0 then
    NewZone.Y1 := 0;
  NewZone.Y2 := (Y + R2) div FTileHeight;
  if NewZone.Y2 >= Height then
    NewZone.Y2 := Height - 1;

  case NewZone.Flicker of
    flCandle:
      begin
        NewZone.States := 4;
        NewZone.FlickerX[1] := NewZone.X;
        NewZone.FlickerY[1] := NewZone.Y;
        NewZone.FlickerZ[1] := NewZone.Z;
        NewZone.FlickerRadius[1] := NewZone.Radius;
        NewZone.FlickerIntensity[1] := NewZone.Intensity;
        NewZone.FlickerX[2] := NewZone.X + random(5) - 2;
        NewZone.FlickerY[2] := NewZone.Y + random(5) - 2;
        NewZone.FlickerZ[2] := NewZone.Z + random(2);
        NewZone.FlickerRadius[2] := NewZone.Radius - 4;
        NewZone.FlickerIntensity[2] := 15 * NewZone.Intensity div 16;
        NewZone.FlickerX[3] := NewZone.X + random(5) - 2;
        NewZone.FlickerY[3] := NewZone.Y + random(5) - 2;
        NewZone.FlickerZ[3] := NewZone.Z + random(4);
        NewZone.FlickerRadius[3] := NewZone.Radius - 8;
        NewZone.FlickerIntensity[3] := 14 * NewZone.Intensity div 16;
        NewZone.FlickerX[4] := NewZone.X + random(5) - 2;
        NewZone.FlickerY[4] := NewZone.Y + random(5) - 2;
        NewZone.FlickerZ[4] := NewZone.Z + random(4);
        NewZone.FlickerRadius[4] := NewZone.Radius - 16;
        NewZone.FlickerIntensity[4] := 13 * NewZone.Intensity div 16;
      end;
    flTorch:
      begin
        NewZone.States := 3;
        NewZone.FlickerX[1] := NewZone.X;
        NewZone.FlickerY[1] := NewZone.Y;
        NewZone.FlickerZ[1] := NewZone.Z;
        NewZone.FlickerRadius[1] := NewZone.Radius;
        NewZone.FlickerIntensity[1] := NewZone.Intensity;
        NewZone.FlickerX[2] := NewZone.X + random(9) - 4;
        NewZone.FlickerY[2] := NewZone.Y + random(9) - 4;
        NewZone.FlickerZ[2] := NewZone.Z + random(4);
        NewZone.FlickerRadius[2] := NewZone.Radius - 8;
        NewZone.FlickerIntensity[2] := 3 * NewZone.Intensity div 4;
        NewZone.FlickerX[3] := NewZone.X + random(9) - 4;
        NewZone.FlickerY[3] := NewZone.Y + random(9) - 4;
        NewZone.FlickerZ[3] := NewZone.Z + random(4);
        NewZone.FlickerRadius[3] := NewZone.Radius - 16;
        NewZone.FlickerIntensity[3] := NewZone.Intensity div 2;
      end;
    flFluorescent:
      begin
        NewZone.States := 2;
        NewZone.FlickerX[1] := NewZone.X;
        NewZone.FlickerY[1] := NewZone.Y;
        NewZone.FlickerZ[1] := NewZone.Z;
        NewZone.FlickerRadius[1] := NewZone.Radius;
        NewZone.FlickerIntensity[1] := NewZone.Intensity;
        NewZone.FlickerX[2] := NewZone.X;
        NewZone.FlickerY[2] := NewZone.Y;
        NewZone.FlickerZ[2] := NewZone.Z;
        NewZone.FlickerRadius[2] := NewZone.Radius;
        NewZone.FlickerIntensity[2] := 0;
      end;
    flNone:
      begin
        NewZone.States := 1;
        NewZone.FlickerX[1] := NewZone.X;
        NewZone.FlickerY[1] := NewZone.Y;
        NewZone.FlickerZ[1] := NewZone.Z;
        NewZone.FlickerRadius[1] := NewZone.Radius;
        NewZone.FlickerIntensity[1] := NewZone.Intensity;
      end;
  else
    begin
      NewZone.States := 1;
      NewZone.FlickerX[1] := NewZone.X;
      NewZone.FlickerY[1] := NewZone.Y;
      NewZone.FlickerZ[1] := NewZone.Z;
      NewZone.FlickerRadius[1] := NewZone.Radius;
      NewZone.FlickerIntensity[1] := NewZone.Intensity;
    end;
  end;

  Result := NewZone.Index;
end;

procedure TAniMap.SaveMapKnownInfo(Stream: TStream);
var
  GridSize, i: Longint;
  GridLoc: ^GridInfo;
  Bits: longword;
begin
  if (FWidth > 0) and (FHeight > 0) then
  begin
    GridSize := FWidth * FHeight;
    GridLoc := GlobalLock(FMapData);
    Bits := 0;
    for i := 1 to GridSize do
    begin
      Bits := Bits shl 1;
      if (GridLoc^.BitField and $40) <> 0 then
        Bits := Bits or 1;
      if (i mod 32) = 0 then
      begin
        Stream.write(Bits, SizeOf(Bits));
        Bits := 0;
      end;
      Inc(GridLoc);
    end;
    GlobalUnlock(FMapData);
  end;
end;

procedure TAniMap.LoadMapKnownInfo(Stream: TStream);
var
  GridSize, i: Longint;
  GridLoc: ^GridInfo;
  Bits: longword;
begin
  if (FWidth > 0) and (FHeight > 0) then
  begin
    GridSize := FWidth * FHeight;
    GridLoc := GlobalLock(FMapData);
    Bits := 0;
    for i := 1 to GridSize do
    begin
      if (i mod 32) = 1 then
      begin
        Stream.read(Bits, SizeOf(Bits));
      end;
      if (Bits and $80000000) <> 0 then
        GridLoc^.BitField := GridLoc^.BitField or $40;
      Bits := Bits shl 1;
      Inc(GridLoc);
    end;
    GlobalUnlock(FMapData);
  end;
end;

procedure TAniMap.RenderMap;
var
  GridBase, P: ^GridInfo;
  ZoneTile: TZone;
  Index: Word;
  tX, tY: Word;
  SrcX, SrcY: Longint;
  X1, Y1, Z1: Longint;
  X2, Y2: Longint;
  i, j, k, m: Longint;
  RL, GL, BL: Word;
  R1, G1, B1: Word;
  D: double;
  IL1: Integer;
  Overlap: TList<TLightZone>;
  OverlapTile: TList<TLightZone>;
  LightZones: TList<TLightZone>;
  CurrentZone, Test: TLightZone;
  Zone: Word;
  CurrentIndex: Integer;
  OVERLAPPED: Boolean;
  X, Y: Longint;
  NewTileIndex, ItemCount: Word;
  HasLayer1: Boolean;
  Layer: Integer;
  A, A1, A2, Slope: Single;
  State: Integer;
  ZoneX, ZoneY, ZoneZ: Longint;
  ZoneIntensity: double;
  ZoneRadius: Longint;
  HalfWidth, HalfHeight: Integer;
  SortedZones: TList;
  ColorMatch: Word;
  DblColorMatch: longword;
  BltFx: TDDBLTFX;
  ddsd: TDDSurfaceDesc;
  C16: Word;
  p16: ^Word;
  TempSurface: IDirectDrawSurface;
  LightR1, LightG1, LightB1: Word;
  Pitch: Longint;
  TimeCount: longword;
  pr: TRect;
begin
  FillChar(ddsd, sizeof(ddsd), 0);
  if not UseLighting then
    exit;

  LightR1 := round(LightR);
  LightG1 := round(LightG);
  LightB1 := round(LightB);

  HalfWidth := FTileWidth div 2;
  HalfHeight := FTileHeight div 2;
  GridBase := GlobalLock(FMapData);

  TransparentColor := ColorToRGB(FTransparentColor);
  ColorMatch := self.ColorMatch;
  DblColorMatch := ColorMatch or (DblColorMatch shl 16);

  if not FUseAmbientOnly then
  begin
    OverlapTile := TList<TLightZone>.Create;
    LightZones := TList<TLightZone>.Create;
    // Make sure flicker lighting is evaluated last
    SortedZones := TList.Create;
    for Zone := 0 to Zones.Count - 1 do
    begin
      ZoneTile := Zones[Zone];
      if ZoneTile is TLightZone then
      begin
        if TLightZone(ZoneTile).States = 1 then
          SortedZones.Add(ZoneTile);
      end
      else
        SortedZones.Add(ZoneTile);
    end;
    for Zone := 0 to Zones.Count - 1 do
    begin
      ZoneTile := Zones[Zone];
      if ZoneTile is TLightZone then
      begin
        if TLightZone(ZoneTile).States > 1 then
          SortedZones.Add(ZoneTile);
      end;
    end;

    // Apply lighting to tiles
    Log.Log('Start Tiles');
    TimeCount := GetTickCount;
    for Zone := 1 to SortedZones.Count - 1 do
    begin
      ZoneTile := SortedZones[Zone];
      if ZoneTile is TLightZone then
      begin
        CurrentZone := SortedZones[Zone];
        LightZones.Add(CurrentZone);
        Overlap := TList<TLightZone>.Create;
        Overlap.Add(CurrentZone);
        NewTileIndex := 0;
        CurrentIndex := -1;
        for i := 1 to SortedZones.Count - 1 do
        begin
          if (i <> Zone) then
          begin
            ZoneTile := SortedZones[i];
            if ZoneTile is TLightZone then
            begin
              Test := SortedZones[i];
              if (Test.X1 <= CurrentZone.X2) and (Test.X2 >= CurrentZone.X1) and
                (Test.Y1 <= CurrentZone.Y2) and (Test.Y2 > CurrentZone.Y1) then
              begin
                if i <= Zone then
                  CurrentIndex := Overlap.Add(Test)
                else
                  Overlap.Add(Test);
              end;
            end;
          end;
          CurrentZone.OverlapZones := Overlap;
        end;
        TempSurface := DDGetSurface(lpdd, FTileWidth, FTileHeight,
          TransparentColor, False);
        for State := 1 to CurrentZone.States do
        begin
          HasLayer1 := False;
          for Layer := 0 to 1 do
          begin
            if (Layer = 0) or ((Layer = 1) and HasLayer1) then
            begin
              for Y := CurrentZone.Y1 to CurrentZone.Y2 do
              begin
                P := GridBase;
                Inc(P, Y * FWidth + CurrentZone.X1);
                for X := CurrentZone.X1 to CurrentZone.X2 do
                begin
                  OverlapTile.Clear;
                  OVERLAPPED := False;
                  for i := 1 to Overlap.Count - 1 do
                  begin
                    Test := Overlap[i];
                    if (Test.X1 < X + 1) and (Test.X2 >= X) and
                      (Test.Y1 < Y + 1) and (Test.Y2 >= Y) then
                    begin
                      OverlapTile.Add(Test);
                      if i > CurrentIndex then
                      begin
                        OVERLAPPED := True;
                        Break;
                      end;
                    end;
                  end;
                  if not OVERLAPPED then
                  begin
                    HasLayer1 := HasLayer1 or (P^.Tile[1] <> $FFFF);
                    if ((P^.BitField and $80) <> 0) then
                      HasLayer1 := HasLayer1 or (P^.Tile[2] <> $FFFF) or
                        (P^.Tile[3] <> $FFFF) or (P^.Tile[4] <> $FFFF);
                    Index := P^.Tile[Layer];
                    if (Index <> $FFFF) or
                      (((P^.BitField and $80) <> 0) and (Layer = 1)) then
                    begin
                      OverlapTile.Add(CurrentZone);
                      ZoneTile := Zones[P^.Zone[Layer]];
                      if (((P^.BitField and $80) <> 0) and (Layer = 1)) then
                      begin
                        BltFx.dwSize := SizeOf(BltFx);
                        BltFx.dwFillColor := ColorMatch;
                        pr := Rect(0, 0, FTileWidth, FTileHeight);
                        TempSurface.Blt(@pr, nil, @pr,
                          DDBLT_COLORFILL + DDBLT_WAIT, @BltFx);
                        if (Index <> $FFFF) then
                        begin
                          SrcX := (Index div ZoneTile.FTileMaxColumnIndex) *
                            FTileWidth;
                          SrcY := (Index mod ZoneTile.FTileMaxColumnIndex) *
                            FTileHeight + HalfHeight;
                          pr := Rect(SrcX, SrcY, SrcX + FTileWidth,
                            SrcY + HalfHeight);
                          TempSurface.BltFast(0, 0, ZoneTile.FTileImages, @pr,
                            DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
                        end;
                        Index := P^.Tile[2];
                        if (Index <> $FFFF) then
                        begin
                          SrcX := (Index div ZoneTile.FTileMaxColumnIndex) *
                            FTileWidth;
                          SrcY := (Index mod ZoneTile.FTileMaxColumnIndex) *
                            FTileHeight;
                          ZoneTile := Zones[P^.Zone[2]];
                          pr := Rect(SrcX, SrcY, SrcX + HalfWidth,
                            SrcY + FTileHeight);
                          TempSurface.BltFast(HalfWidth, 0,
                            ZoneTile.FTileImages, @pr, DDBLTFAST_SRCCOLORKEY or
                            DDBLTFAST_WAIT);
                        end;
                        Index := P^.Tile[3];
                        if (Index <> $FFFF) then
                        begin
                          SrcX := (Index div ZoneTile.FTileMaxColumnIndex) *
                            FTileWidth;
                          SrcY := (Index mod ZoneTile.FTileMaxColumnIndex) *
                            FTileHeight;
                          ZoneTile := Zones[P^.Zone[3]];
                          pr := Rect(SrcX, SrcY, SrcX + FTileWidth,
                            SrcY + HalfHeight);
                          TempSurface.BltFast(0, HalfHeight,
                            ZoneTile.FTileImages, @pr, DDBLTFAST_SRCCOLORKEY or
                            DDBLTFAST_WAIT);
                        end;
                        Index := P^.Tile[4];
                        if (Index <> $FFFF) then
                        begin
                          SrcX := (Index div ZoneTile.FTileMaxColumnIndex) *
                            FTileWidth + HalfWidth;
                          SrcY := (Index mod ZoneTile.FTileMaxColumnIndex) *
                            FTileHeight;
                          ZoneTile := Zones[P^.Zone[4]];
                          pr := Rect(SrcX, SrcY, SrcX + HalfWidth,
                            SrcY + FTileHeight);
                          TempSurface.BltFast(0, 0, ZoneTile.FTileImages, @pr,
                            DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
                        end;
                      end
                      else
                      begin
                        SrcX := (Index div ZoneTile.FTileMaxColumnIndex) *
                          FTileWidth;
                        SrcY := (Index mod ZoneTile.FTileMaxColumnIndex) *
                          FTileHeight;
                        pr := Rect(SrcX, SrcY, SrcX + FTileWidth,
                          SrcY + FTileHeight);
                        TempSurface.BltFast(0, 0, ZoneTile.FTileImages, @pr,
                          DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
                      end;
                      // Render Tile
                      ddsd.dwSize := SizeOf(ddsd);
                      if TempSurface.Lock(nil, ddsd, DDLOCK_WAIT, 0) = DD_OK
                      then
                      begin
                        try
                          for j := 0 to FTileHeight - 1 do
                          begin
                            Y2 := j + Y * FTileHeight;
                            p16 := ddsd.lpSurface;
                            Inc(PAnsiChar(p16), j * ddsd.lPitch);
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
                                  Test := OverlapTile[k];
                                  if Test = CurrentZone then
                                  begin
                                    ZoneX := Test.FlickerX[State];
                                    ZoneY := Test.FlickerY[State];
                                    ZoneZ := Test.FlickerZ[State];
                                    ZoneRadius := Test.FlickerRadius[State];
                                    ZoneIntensity := Test.FlickerIntensity
                                      [State] / 100;
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
                                    X1 := sqr(ZoneX - X2);
                                    Y1 := sqr((ZoneY - Y2) * 2);
                                    Z1 := sqr(ZoneZ);
                                    D := sqrt(X1 + Y1 + Z1) / ZoneRadius;
                                    if D <= 1 then
                                    begin
                                      if LineOfCollision(ZoneX, ZoneY, X2, Y2)
                                      then
                                      begin
                                        IL1 := trunc
                                        ((1 - D) * ZoneIntensity * 256);
                                        Inc(R1, (IL1 * RL) shr 8);
                                        Inc(G1, (IL1 * GL) shr 8);
                                        Inc(B1, (IL1 * BL) shr 8);
                                      end;
                                    end;
                                  end;
                                end;
                                if (R1 <> $FF) or (G1 <> $FF) or (B1 <> $FF)
                                then
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
                                      and     EAX,$1F  // *
                                      mul     G1
                                      test    EAX,$FFFFE000 // *
                                      jz      @@StoreGreen
                                      or      CX,$3E0 // *
                                      jmp     @@Red
                                    @@StoreGreen:
                                      shr     AX,3    // *
                                      and     AX,$3E0 // *
                                      or      CX,AX
                                    @@Red:
                                      xor     EAX,EAX
                                      mov     AH,BH
                                      shr     AX,10 // *
                                      mul     R1
                                      test    EAX,$FFFFE000
                                      jz      @@StoreRed
                                      or      CH,$F8
                                      jmp     @@Continue
                                    @@StoreRed:
                                      shl     AH,2 // *
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
                                      and     EAX,$3F  // *
                                      mul     G1
                                      test    EAX,$FFFFC000 // *
                                      jz      @@StoreGreen
                                      or      CX,$7E0 // *
                                      jmp     @@Red
                                    @@StoreGreen:
                                      shr     AX,3    // *
                                      and     AX,$7E0 // *
                                      or      CX,AX
                                    @@Red:
                                      xor     EAX,EAX
                                      mov     AH,BH
                                      shr     AX,11 // *
                                      mul     R1
                                      test    EAX,$FFFFE000
                                      jz      @@StoreRed
                                      or      CH,$F8
                                      jmp     @@Continue
                                    @@StoreRed:
                                      shl     AH,3 // *
                                      or      CH,AH
                                    @@Continue:
                                      mov     EAX,p16
                                      mov     [EAX],CX
                                      pop     EBX
                                    end;
                                end;
                              end;
                              Inc(p16);
                            end;
                          end;
                        finally
                          TempSurface.Unlock(nil);
                        end;
                      end;

                      if (State = CurrentZone.States) then
                      begin // only update grid on last state
                        P^.Zone[Layer] := CurrentZone.Index;
                        P^.Tile[Layer] := NewTileIndex;
                        if (Layer = 1) then
                          P^.BitField := P^.BitField and $7F;
                        // This is no longer a diamond tile
                      end;
                      Inc(NewTileIndex);
                      CurrentZone.DefineTile(NewTileIndex, TempSurface,
                        FTransparentColor);
                    end;
                  end;
                  Inc(P);
                end; // X
              end; // Y
            end; // Has layer
          end; // Layer loop
          CurrentZone.NewTileState;
        end; // State loop
        TempSurface := nil;

      end;
    end;
    GlobalUnlock(FMapData);
    SortedZones.Free;
    TimeCount := GetTickCount - TimeCount;
    Log.Log('End Tiles: ' + IntToStr(TimeCount));

    // Apply lighting to items
    Log.Log('Start Items');
    TimeCount := GetTickCount;
    ItemCount := 0;
    m := FStripWidth div 2;
    if (LightZones.Count > 0) then
    begin
      for State := 1 to MaxLightStates do
      begin
        i := FirstItem;
        while (i <> 0) do
        begin
          ZoneTile := Zones[FItemList[i].Zone];
          X := FItemList[i].X div FTileWidth;
          Y := FItemList[i].Y div FTileHeight;
          OverlapTile.Clear;
          for j := 0 to LightZones.Count - 1 do
          begin
            Test := LightZones[j];
            if Test.States >= State then
            begin
              if (X >= Test.X1) and (X <= Test.X2) and (Y >= Test.Y1) and
                (Y <= Test.Y2) then
              begin
                OverlapTile.Add(Test);
              end;
            end;
          end;
          if OverlapTile.Count > 0 then
          begin
            Zone := OverlapTile[OverlapTile.Count - 1].Index;
            TempSurface := DDGetSurface(lpdd, FStripWidth, FItemList[i].Height,
              TransparentColor, False);
            pr := Rect(FItemList[i].ImageX, FItemList[i].ImageY,
              FItemList[i].ImageX + FStripWidth, FItemList[i].ImageY +
              FItemList[i].Height);
            TempSurface.BltFast(0, 0, ZoneTile.FItemImages, @pr,
              DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
            ddsd.dwSize := SizeOf(ddsd);
            if TempSurface.Lock(nil, ddsd, DDLOCK_WAIT, 0) = DD_OK then
            begin
              try
                for X := 0 to FStripWidth - 1 do
                begin
                  X2 := FItemList[i].X + X;
                  if (X = 0) then
                  begin
                    Slope := FItemList[i].Slope1;
                    A := ArcTan(Slope);
                    if (Slope > 0) then
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
                  else if (X = m) then
                  begin
                    Slope := FItemList[i].Slope2;
                    A := ArcTan(Slope);
                    if (Slope > 0) then
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
                  Inc(p16, X);
                  if FItemList[i].Vertical then
                    Y2 := FItemList[i].Y + round((X - m) * Slope)
                    // If vertical, Y2 remains constant
                  else // so only calculate once
                    Y2 := 0;
                  for Y := 0 to FItemList[i].Height - 1 do
                  begin
                    if not FItemList[i].Vertical then
                      Y2 := FItemList[i].Y - FItemList[i].VHeight + Y;
                    C16 := p16^;
                    if C16 <> ColorMatch then
                    begin
                      R1 := LightR1;
                      G1 := LightG1;
                      B1 := LightB1;
                      for j := 0 to OverlapTile.Count - 1 do
                      begin
                        Test := OverlapTile[j];
                        if Test = Zones[Zone] then
                        begin
                          ZoneX := Test.FlickerX[State];
                          ZoneY := Test.FlickerY[State];
                          ZoneZ := Test.FlickerZ[State];
                          ZoneRadius := Test.FlickerRadius[State];
                          ZoneIntensity := Test.FlickerIntensity[State] / 100;
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
                          A := ATan(X2 - ZoneX, Y2 - ZoneY);
                          if ((A2 >= A1) and ((A > A1) and (A < A2))) or
                            ((A2 < A1) and ((A < A2) or (A > A1))) then
                          begin
                            RL := Test.Color and $FF;
                            GL := Test.Color and $FF00 shr 8;
                            BL := Test.Color and $FF0000 shr 16;
                            X1 := sqr(ZoneX - X2);
                            Y1 := sqr((ZoneY - Y2) * 2);
                            if FItemList[i].Vertical then
                              Z1 := sqr(Y2 - FItemList[i].Y + FItemList[i]
                                .VHeight - Y - ZoneZ - 1)
                            else
                              Z1 := sqr(ZoneZ);
                            D := sqrt(X1 + Y1 + Z1) / ZoneRadius;
                            if D <= 1 then
                            begin
                              if LineOfCollision(ZoneX, ZoneY, X2, Y2 + 4) then
                              begin
                                IL1 := trunc((1 - D) * ZoneIntensity * 256);
                                Inc(R1, (IL1 * RL) shr 8);
                                Inc(G1, (IL1 * GL) shr 8);
                                Inc(B1, (IL1 * BL) shr 8);
                              end;
                            end;
                          end;
                        end;
                      end;
                      if (R1 <> $FF) or (G1 <> $FF) or (B1 <> $FF) then
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
                            and     EAX,$1F  // *
                            mul     G1
                            test    EAX,$FFFFE000 // *
                            jz      @@StoreGreen
                            or      CX,$3E0 // *
                            jmp     @@Red
                          @@StoreGreen:
                            shr     AX,3    // *
                            and     AX,$3E0 // *
                            or      CX,AX
                          @@Red:
                            xor     EAX,EAX
                            mov     AH,BH
                            shr     AX,10 // *
                            mul     R1
                            test    EAX,$FFFFE000
                            jz      @@StoreRed
                            or      CH,$F8
                            jmp     @@Continue
                          @@StoreRed:
                            shl     AH,2 // *
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
                            and     EAX,$3F  // *
                            mul     G1
                            test    EAX,$FFFFC000 // *
                            jz      @@StoreGreen
                            or      CX,$7E0 // *
                            jmp     @@Red
                          @@StoreGreen:
                            shr     AX,3    // *
                            and     AX,$7E0 // *
                            or      CX,AX
                          @@Red:
                            xor     EAX,EAX
                            mov     AH,BH
                            shr     AX,11 // *
                            mul     R1
                            test    EAX,$FFFFE000
                            jz      @@StoreRed
                            or      CH,$F8
                            jmp     @@Continue
                          @@StoreRed:
                            shl     AH,3 // *
                            or      CH,AH
                          @@Continue:
                            mov     EAX,p16
                            mov     [EAX],CX
                            pop     EBX
                          end;
                      end;
                    end;
                    Inc(PAnsiChar(p16), ddsd.lPitch);
                  end;
                end;
              finally
                TempSurface.Unlock(nil);
              end;
            end;
            if (State = TLightZone(Zones[Zone]).States) then
            begin
              TLightZone(Zones[Zone]).AddStrip(TempSurface, FItemList[i].ImageX,
                FItemList[i].ImageY);
              FItemList[i].Zone := Zone;
            end
            else
            begin
              TLightZone(Zones[Zone]).AddStrip(TempSurface, tX, tY);
            end;
            Inc(ItemCount);
            TempSurface := nil;
          end;
          i := FItemList[i].Next;
        end;
        for j := 0 to LightZones.Count - 1 do
        begin
          Test := LightZones[j];
          if Test.States >= State then
          begin
            Test.NewItemState;
          end;
        end;
      end;
    end;
    OverlapTile.Free;
    TimeCount := GetTickCount - TimeCount;
    Log.Log('End Items: ' + IntToStr(TimeCount));

    // Construct item list for all light zones
    if LightZones.Count > 0 then
    begin
      i := FirstItem;
      while (i <> 0) do
      begin
        ZoneTile := Zones[FItemList[i].Zone];
        if ZoneTile is TLightZone then
        begin
          Test := TLightZone(ZoneTile);
          if not Assigned(Test.Items) then
            Test.Items := TList.Create;
          Test.Items.Add(@FItemList[i]);
        end;
        i := FItemList[i].Next;
      end;
    end;
  end;

  if (LightR <> $FF) or (LightG <> $FF) or (LightB <> $FF) then
  begin
    // Render ambient color in all zones
    Log.Log('Start Ambient');
    TimeCount := GetTickCount;
    for Zone := 0 to Zones.Count - 1 do
    begin
      ZoneTile := Zones[Zone];
      if not(ZoneTile is TLightZone) then
      begin
        // Do tiles first
        if Assigned(ZoneTile.FTileImages) then
        begin
          ddsd.dwSize := SizeOf(ddsd);
          if ZoneTile.FTileImages.Lock(nil, ddsd, DDLOCK_WAIT, 0) = DD_OK then
          begin
            try
              j := ZoneTile.FTileBitHeight;
              i := ZoneTile.FTileBitWidth;
              p16 := ddsd.lpSurface;
              Pitch := ddsd.lPitch;
              if (i > 0) and (j > 0) then
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
                    and     EAX,$1F  // *
                    mul     LightG1
                    test    EAX,$FFFFE000 // *
                    jz      @@StoreGreen1
                    or      CX,$3E0 // *
                    jmp     @@Red1
                  @@StoreGreen1:
                    shr     AX,3    // *
                    and     AX,$3E0 // *
                    or      CX,AX
                  @@Red1:
                    xor     EAX,EAX
                    mov     AH,BH
                    shr     AX,10 // *
                    mul     LightR1
                    test    EAX,$FFFFE000
                    jz      @@StoreRed1
                    or      CH,$F8
                    jmp     @@Next1
                  @@StoreRed1:
                    shl     AH,2 // *
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
                    and     EAX,$1F  // *
                    mul     LightG1
                    test    EAX,$FFFFE000 // *
                    jz      @@StoreGreen2
                    or      CX,$3E0 // *
                    jmp     @@Red2
                  @@StoreGreen2:
                    shr     AX,3    // *
                    and     AX,$3E0 // *
                    or      CX,AX
                  @@Red2:
                    xor     EAX,EAX
                    mov     AH,BH
                    shr     AX,10 // *
                    mul     LightR1
                    test    EAX,$FFFFE000
                    jz      @@StoreRed2
                    or      CH,$F8
                    jmp     @@Continue
                  @@StoreRed2:
                    shl     AH,2 // *
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
                    and     EAX,$3F  // *
                    mul     LightG1
                    test    EAX,$FFFFC000 // *
                    jz      @@StoreGreen1
                    or      CX,$7E0 // *
                    jmp     @@Red1
                  @@StoreGreen1:
                    shr     AX,3    // *
                    and     AX,$7E0 // *
                    or      CX,AX
                  @@Red1:
                    xor     EAX,EAX
                    mov     AH,BH
                    shr     AX,11 // *
                    mul     LightR1
                    test    EAX,$FFFFE000
                    jz      @@StoreRed1
                    or      CH,$F8
                    jmp     @@Next1
                  @@StoreRed1:
                    shl     AH,3 // *
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
                    and     EAX,$3F  // *
                    mul     LightG1
                    test    EAX,$FFFFC000 // *
                    jz      @@StoreGreen2
                    or      CX,$7E0 // *
                    jmp     @@Red2
                  @@StoreGreen2:
                    shr     AX,3    // *
                    and     AX,$7E0 // *
                    or      CX,AX
                  @@Red2:
                    xor     EAX,EAX
                    mov     AH,BH
                    shr     AX,11 // *
                    mul     LightR1
                    test    EAX,$FFFFE000
                    jz      @@StoreRed2
                    or      CH,$F8
                    jmp     @@Continue
                  @@StoreRed2:
                    shl     AH,3 // *
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
              ZoneTile.FTileImages.Unlock(nil);
            end;
          end;
        end;

        // Do Items
        if Assigned(ZoneTile.FItemImages) then
        begin
          ddsd.dwSize := SizeOf(ddsd);
          if ZoneTile.FItemImages.Lock(nil, ddsd, DDLOCK_WAIT, 0) = DD_OK then
          begin
            try
              j := ZoneTile.FItemBitHeight;
              i := ZoneTile.FItemBitWidth;
              p16 := ddsd.lpSurface;
              Pitch := ddsd.lPitch;
              if (i > 0) and (j > 0) then
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
                    and     EAX,$1F  // *
                    mul     LightG1
                    test    EAX,$FFFFE000 // *
                    jz      @@StoreGreen1
                    or      CX,$3E0 // *
                    jmp     @@Red1
                  @@StoreGreen1:
                    shr     AX,3    // *
                    and     AX,$3E0 // *
                    or      CX,AX
                  @@Red1:
                    xor     EAX,EAX
                    mov     AH,BH
                    shr     AX,10 // *
                    mul     LightR1
                    test    EAX,$FFFFE000
                    jz      @@StoreRed1
                    or      CH,$F8
                    jmp     @@Next1
                  @@StoreRed1:
                    shl     AH,2 // *
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
                    and     EAX,$1F  // *
                    mul     LightG1
                    test    EAX,$FFFFE000 // *
                    jz      @@StoreGreen2
                    or      CX,$3E0 // *
                    jmp     @@Red2
                  @@StoreGreen2:
                    shr     AX,3    // *
                    and     AX,$3E0 // *
                    or      CX,AX
                  @@Red2:
                    xor     EAX,EAX
                    mov     AH,BH
                    shr     AX,10 // *
                    mul     LightR1
                    test    EAX,$FFFFE000
                    jz      @@StoreRed2
                    or      CH,$F8
                    jmp     @@Continue
                  @@StoreRed2:
                    shl     AH,2 // *
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
                    and     EAX,$3F  // *
                    mul     LightG1
                    test    EAX,$FFFFC000 // *
                    jz      @@StoreGreen1
                    or      CX,$7E0 // *
                    jmp     @@Red1
                  @@StoreGreen1:
                    shr     AX,3    // *
                    and     AX,$7E0 // *
                    or      CX,AX
                  @@Red1:
                    xor     EAX,EAX
                    mov     AH,BH
                    shr     AX,11 // *
                    mul     LightR1
                    test    EAX,$FFFFE000
                    jz      @@StoreRed1
                    or      CH,$F8
                    jmp     @@Next1
                  @@StoreRed1:
                    shl     AH,3 // *
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
                    and     EAX,$3F  // *
                    mul     LightG1
                    test    EAX,$FFFFC000 // *
                    jz      @@StoreGreen2
                    or      CX,$7E0 // *
                    jmp     @@Red2
                  @@StoreGreen2:
                    shr     AX,3    // *
                    and     AX,$7E0 // *
                    or      CX,AX
                  @@Red2:
                    xor     EAX,EAX
                    mov     AH,BH
                    shr     AX,11 // *
                    mul     LightR1
                    test    EAX,$FFFFE000
                    jz      @@StoreRed2
                    or      CH,$F8
                    jmp     @@Continue
                  @@StoreRed2:
                    shl     AH,3 // *
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
              ZoneTile.FItemImages.Unlock(nil);
            end;
          end;
        end;
      end;
    end;
    TimeCount := GetTickCount - TimeCount;
    Log.Log('End Ambient: ' + IntToStr(TimeCount));
  end;

  if not FUseAmbientOnly then
  begin
    // Update flickering light zone items such that they can be updated correctly without
    // re-rendering the entire map.
    for i := 0 to LightZones.Count - 1 do
    begin
      Test := LightZones[i];
      Test.State := Test.States;
      if (Test.Flicker <> flNone) then
      begin
        Test.MoveToVideo;
        Test.FullRefresh := Test.TilesInVideo and Test.ItemsInVideo;
      end;
    end;
    LightZones.Free;
  end;
end;

function TAniMap.LineOfSight(X1, Y1, X2, Y2: Longint): Boolean;
var
  GridBase, P: ^GridInfo;
  i, i1, i2: Longint;
  j, k: Longint;
  Dx, dy: Longint;
  X, Y: Longint;
  sX, sY, s: Longint;
  R: double;
  Offset: Longint;
  Mask: Word;
  W, H: Integer;
  A, A2: double;
  b, b2: double;
  c2: double;
  d2: double;
  R2: double;
  Pass: Boolean;
  StripX, StripY: Longint;
begin
  // This routine does not check for visibility within the starting tile.
  // To do so would cause a number of special cases and would slow performance.
  // Frankly, if you're standing in the same tile visibilty should not be a problem anyway.
  GridBase := GlobalLock(FMapData);
  Dx := X2 - X1;
  dy := Y2 - Y1;
  W := FStripWidth div 2;
  H := FStripHeight div 2;
  StripX := (X2 div FStripWidth) * FStripWidth + W;
  StripY := (Y2 div FStripHeight) * FStripHeight + H;
  R2 := sqr(W);

  if (Dx <> 0) then
  begin
    if (X1 < X2) then
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
      R := (X - X1) / Dx;
      Y := Y1 + round(R * dy);
      j := Y div FTileHeight;
      if (Y mod FTileHeight) = 0 then
        if (Y2 < Y1) then
          dec(j);
      if j >= 0 then
      begin
        P := GridBase;
        Inc(P, j * FWidth + i + Offset);
        if (P^.LineOfSightMask <> 0) then
        begin
          k := j * FTileHeight;
          Mask := P^.LineOfSightMask;
          for s := 0 to 15 do
          begin
            if (Mask and 1) = 1 then
            begin
              sX := X + (s mod 4) * FStripWidth + W + Offset * FTileWidth;
              if (Offset < 0) then
                Pass := (sX > X2)
              else
                Pass := (sX <= X2);
              if Pass then
              begin
                sY := k + (3 - (s div 4)) * FStripHeight + H;
                if (Y2 < Y1) then
                  Pass := sY > Y2 - H
                else
                  Pass := sY <= Y2 + H;
                if Pass then
                begin
                  if (sX <> StripX) or (sY <> StripY) then
                  begin
                    if Dx = 0 then
                    begin
                      d2 := sqr(X2 - sX);
                    end
                    else if dy = 0 then
                    begin
                      d2 := sqr(2 * (Y2 - sY));
                    end
                    else
                    begin
                      A := 2 * (sY - (dy * (sX - X1) / Dx + Y1));
                      b := sX - (Dx * (sY - Y1) / dy + X1);
                      A2 := sqr(A);
                      b2 := sqr(b);
                      c2 := A2 + b2;
                      if c2 = 0 then
                      begin
                        GlobalUnlock(FMapData);
                        Result := False;
                        exit;
                      end;
                      d2 := A2 * b2 / c2;
                    end;
                    if d2 <= R2 then
                    begin
                      GlobalUnlock(FMapData);
                      Result := False;
                      exit;
                    end;
                  end;
                end;
              end;
            end;
            Mask := Mask shr 1;
          end;
        end;
      end;
      Inc(X, FTileWidth);
    end;
  end;

  if (dy <> 0) then
  begin
    if (Y1 < Y2) then
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
      R := (Y - Y1) / dy;
      X := X1 + round(R * Dx);
      j := X div FTileWidth;
      if (X mod FTileWidth) = 0 then
        if (X2 < X1) then
          dec(j);
      if (j >= 0) then
      begin
        P := GridBase;
        Inc(P, i * FWidth + j + Offset);
        if (P^.LineOfSightMask <> 0) then
        begin

          k := j * FTileWidth;
          Mask := P^.LineOfSightMask;
          for s := 0 to 15 do
          begin
            if (Mask and 1) = 1 then
            begin
              sY := Y + (3 - (s div 4)) * FStripHeight + H;
              if (Offset < 0) then
              begin
                dec(sY, FTileHeight);
                Pass := (sY > Y2);
              end
              else
              begin
                Pass := (sY <= Y2);
              end;
              if Pass then
              begin
                sX := k + (s mod 4) * FStripWidth + W;
                if (X2 < X1) then
                  Pass := sX > X2 - W
                else
                  Pass := sX <= X2 + W;
                if Pass then
                begin
                  if (sX <> StripX) or (sY <> StripY) then
                  begin
                    if Dx = 0 then
                    begin
                      d2 := sqr(X2 - sX);
                    end
                    else if dy = 0 then
                    begin
                      d2 := sqr(2 * (Y2 - sY));
                    end
                    else
                    begin
                      A := 2 * (sY - (dy * (sX - X1) / Dx + Y1));
                      b := sX - (Dx * (sY - Y1) / dy + X1);
                      A2 := sqr(A);
                      b2 := sqr(b);
                      c2 := A2 + b2;
                      if c2 = 0 then
                      begin
                        GlobalUnlock(FMapData);
                        Result := False;
                        exit;
                      end;
                      d2 := A2 * b2 / c2;
                    end;
                    if d2 <= R2 then
                    begin
                      GlobalUnlock(FMapData);
                      Result := False;
                      exit;
                    end;
                  end;
                end;
              end;
            end;
            Mask := Mask shr 1;
          end;
        end;
      end;
      Inc(Y, FTileHeight);
    end;
  end;

  GlobalUnlock(FMapData);
  Result := True;
end;

function TAniMap.LineOfCollision(X1, Y1, X2, Y2: Longint): Boolean;
var
  GridBase, P: ^GridInfo;
  i, i1, i2: Longint;
  j, k: Longint;
  Dx, dy: Longint;
  X, Y: Longint;
  sX, sY, s: Longint;
  R: double;
  Offset: Longint;
  Mask: Word;
  W, H: Integer;
  A, A2: double;
  b, b2: double;
  c2: double;
  d2: double;
  R2: double;
  Pass: Boolean;
  StripX, StripY: Longint;
begin
  // This routine does not check for visibility within the starting tile.
  // To do so would cause a number of special cases and would slow performance.
  // Frankly, if you're standing in the same tile visibilty should not be a problem anyway.
  GridBase := GlobalLock(FMapData);
  Dx := X2 - X1;
  dy := Y2 - Y1;
  W := FStripWidth div 2;
  H := FStripHeight div 2;
  StripX := (X2 div FStripWidth) * FStripWidth + W;
  StripY := (Y2 div FStripHeight) * FStripHeight + H;
  R2 := sqr(W);

  if (Dx <> 0) then
  begin
    if (X1 < X2) then
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
      R := (X - X1) / Dx;
      Y := Y1 + round(R * dy);
      j := Y div FTileHeight;
      if (Y mod FTileHeight) = 0 then
        if (Y2 < Y1) then
          dec(j);
      if j >= 0 then
      begin
        P := GridBase;
        Inc(P, j * FWidth + i + Offset);
        if (P^.CollisionMask <> 0) then
        begin
          k := j * FTileHeight;
          Mask := P^.CollisionMask;
          for s := 0 to 15 do
          begin
            if (Mask and 1) = 1 then
            begin
              sX := X + (s mod 4) * FStripWidth + W + Offset * FTileWidth;
              if (Offset < 0) then
                Pass := (sX > X2)
              else
                Pass := (sX <= X2);
              if Pass then
              begin
                sY := k + (3 - (s div 4)) * FStripHeight + H;
                if (Y2 < Y1) then
                  Pass := sY > Y2 - H
                else
                  Pass := sY <= Y2 + H;
                if Pass then
                begin
                  if (sX <> StripX) or (sY <> StripY) then
                  begin
                    if Dx = 0 then
                    begin
                      d2 := sqr(X2 - sX);
                    end
                    else if dy = 0 then
                    begin
                      d2 := sqr(2 * (Y2 - sY));
                    end
                    else
                    begin
                      A := 2 * (sY - (dy * (sX - X1) / Dx + Y1));
                      b := sX - (Dx * (sY - Y1) / dy + X1);
                      A2 := sqr(A);
                      b2 := sqr(b);
                      c2 := A2 + b2;
                      if c2 = 0 then
                      begin
                        GlobalUnlock(FMapData);
                        Result := False;
                        exit;
                      end;
                      d2 := A2 * b2 / c2;
                    end;
                    if d2 <= R2 then
                    begin
                      GlobalUnlock(FMapData);
                      Result := False;
                      exit;
                    end;
                  end;
                end;
              end;
            end;
            Mask := Mask shr 1;
          end;
        end;
      end;
      Inc(X, FTileWidth);
    end;
  end;

  if (dy <> 0) then
  begin
    if (Y1 < Y2) then
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
      R := (Y - Y1) / dy;
      X := X1 + round(R * Dx);
      j := X div FTileWidth;
      if (X mod FTileWidth) = 0 then
        if (X2 < X1) then
          dec(j);
      if (j >= 0) then
      begin
        P := GridBase;
        Inc(P, i * FWidth + j + Offset);
        if (P^.CollisionMask <> 0) then
        begin

          k := j * FTileWidth;
          Mask := P^.CollisionMask;
          for s := 0 to 15 do
          begin
            if (Mask and 1) = 1 then
            begin
              sY := Y + (3 - (s div 4)) * FStripHeight + H;
              if (Offset < 0) then
              begin
                dec(sY, FTileHeight);
                Pass := (sY > Y2);
              end
              else
              begin
                Pass := (sY <= Y2);
              end;
              if Pass then
              begin
                sX := k + (s mod 4) * FStripWidth + W;
                if (X2 < X1) then
                  Pass := sX > X2 - W
                else
                  Pass := sX <= X2 + W;
                if Pass then
                begin
                  if (sX <> StripX) or (sY <> StripY) then
                  begin
                    if Dx = 0 then
                    begin
                      d2 := sqr(X2 - sX);
                    end
                    else if dy = 0 then
                    begin
                      d2 := sqr(2 * (Y2 - sY));
                    end
                    else
                    begin
                      A := 2 * (sY - (dy * (sX - X1) / Dx + Y1));
                      b := sX - (Dx * (sY - Y1) / dy + X1);
                      A2 := sqr(A);
                      b2 := sqr(b);
                      c2 := A2 + b2;
                      if c2 = 0 then
                      begin
                        GlobalUnlock(FMapData);
                        Result := False;
                        exit;
                      end;
                      d2 := A2 * b2 / c2;
                    end;
                    if d2 <= R2 then
                    begin
                      GlobalUnlock(FMapData);
                      Result := False;
                      exit;
                    end;
                  end;
                end;
              end;
            end;
            Mask := Mask shr 1;
          end;
        end;
      end;
      Inc(Y, FTileHeight);
    end;
  end;

  GlobalUnlock(FMapData);
  Result := True;
end;

function TAniMap.DefineItem(Zone, Index: Word; Image: IDirectDrawSurface;
  const StripHeights, CollisionMasks, LineOfSightMasks,
  LightPoints: array of Word; Slope: Single;
  Visible, AutoTransparent, Vertical: Boolean): PItemInfo;
begin
  if (Zone >= Zones.Count) then
  begin
    Result := nil;
    exit;
  end;
  Result := Zones[Zone].DefineItem(Index, Image, StripHeights, CollisionMasks,
    LineOfSightMasks, LightPoints, FTransparentColor, Slope, Visible,
    AutoTransparent, Vertical);
end;

procedure TAniMap.DefineTile(Zone, Index: Word; Image: IDirectDrawSurface);
begin
  if (Zone >= Zones.Count) then
    exit;
  Zones[Zone].DefineTile(Index, Image, FTransparentColor);
end;

function TAniMap.GetTile(X, Y: Longint): PGridInfo;
var
  GridLoc: ^GridInfo;
  Loc: Integer;
begin
  if (FMapData <> 0) then
  begin
    if (X < FWidth) and (Y < FHeight) then
    begin
      Loc := X + Y * FWidth;
      GridLoc := GlobalLock(FMapData);
      Inc(GridLoc, Loc);
      Result := Pointer(GridLoc);
      GlobalUnlock(FMapData);
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

procedure TAniMap.SetDiamondTile(X, Y: Longint; ZoneTop, TileTop, ZoneRight,
  TileRight, ZoneBottom, TileBottom, ZoneLeft, TileLeft: Word);
var
  GridLoc, P: ^GridInfo;
  Loc: Longint;
begin
  if (FMapData <> 0) then
  begin
    if (X < FWidth) and (Y < FHeight) then
    begin
      GridLoc := GlobalLock(FMapData);
      Loc := X + Y * FWidth;
      P := GridLoc;
      Inc(P, Loc);
      if TileTop = 0 then
      begin
        P^.Zone[1] := 0;
        P^.Tile[1] := $FFFF;
      end
      else
      begin
        P^.Zone[1] := ZoneTop;
        P^.Tile[1] := Zones[ZoneTop].Tile[TileTop].ImageIndex;
      end;
      if TileRight = 0 then
      begin
        P^.Zone[2] := 0;
        P^.Tile[2] := $FFFF;
      end
      else
      begin
        P^.Zone[2] := ZoneRight;
        P^.Tile[2] := Zones[ZoneRight].Tile[TileRight].ImageIndex;
      end;
      if TileBottom = 0 then
      begin
        P^.Zone[3] := 0;
        P^.Tile[3] := $FFFF;
      end
      else
      begin
        P^.Zone[3] := ZoneBottom;
        P^.Tile[3] := Zones[ZoneBottom].Tile[TileBottom].ImageIndex;
      end;
      if TileLeft = 0 then
      begin
        P^.Zone[4] := 0;
        P^.Tile[4] := $FFFF;
      end
      else
      begin
        P^.Zone[4] := ZoneLeft;
        P^.Tile[4] := Zones[ZoneLeft].Tile[TileLeft].ImageIndex;
      end;
      P^.BitField := P^.BitField or $80;
      // Set high bit to denote a diamond tile
      GlobalUnlock(FMapData);
    end
  end;
end;

procedure TAniMap.SetTile(X, Y, Layer: Longint; Zone, Index: Word);
var
  GridLoc, P: ^GridInfo;
  Loc: Longint;
  i, j: Integer;
begin
  if (FMapData <> 0) then
  begin
    GridLoc := GlobalLock(FMapData);
    if (Index = 0) then
    begin
      if (X < FWidth) and (Y < FHeight) then
      begin
        Loc := X + Y * FWidth;
        P := GridLoc;
        Inc(P, Loc);
        P^.Zone[Layer] := 0;
        P^.Tile[Layer] := $FFFF;
        if (Layer) = 1 then
          P^.BitField := P^.BitField and $7F;
        // Turn off high bit to denote rect tile
      end;
    end
    else
    begin
      for i := 0 to Zones[Zone].Tile[Index].Columns - 1 do
      begin
        if (X + i < FWidth) then
        begin
          for j := 0 to Zones[Zone].Tile[Index].Rows - 1 do
          begin
            if (Y + j < FHeight) then
            begin
              Loc := (X + i) + (Y + j) * FWidth;
              P := GridLoc;
              Inc(P, Loc);
              P^.Zone[Layer] := Zone;
              P^.Tile[Layer] := Zones[Zone].Tile[Index].ImageIndex + i *
                Zones[Zone].Tile[Index].Rows + j;
              if (Layer) = 1 then
                P^.BitField := P^.BitField and $7F;
              // Turn off high bit to denote rect tile
            end;
          end;
        end;
      end;
    end;
    GlobalUnlock(FMapData);
  end;
end;

procedure TAniMap.SetCollisionMask(X, Y: Integer; CollisionMask: Word);
var
  GridLoc, P: ^GridInfo;
  Loc: Longint;
begin
  if (FMapData <> 0) then
  begin
    if (X < FWidth) and (Y < FHeight) then
    begin
      GridLoc := GlobalLock(FMapData);
      Loc := X + Y * FWidth;
      P := GridLoc;
      Inc(P, Loc);
      P^.CollisionMask := CollisionMask;
      GlobalUnlock(FMapData);
    end;
  end;
end;

procedure TAniMap.SetLineOfSightMask(X, Y: Integer; LineOfSightMask: Word);
var
  GridLoc, P: ^GridInfo;
  Loc: Longint;
begin
  if (FMapData <> 0) then
  begin
    if (X < FWidth) and (Y < FHeight) then
    begin
      GridLoc := GlobalLock(FMapData);
      Loc := X + Y * FWidth;
      P := GridLoc;
      Inc(P, Loc);
      P^.LineOfSightMask := LineOfSightMask;
      GlobalUnlock(FMapData);
    end;
  end;
end;

procedure TAniMap.SetTrigger(X, Y: Integer; TriggerID: Smallint);
var
  GridLoc, P: ^GridInfo;
  Loc: Longint;
begin
  if (FMapData <> 0) then
  begin
    if (X < FWidth) and (Y < FHeight) then
    begin
      GridLoc := GlobalLock(FMapData);
      Loc := X + Y * FWidth;
      P := GridLoc;
      Inc(P, Loc);
      P^.TriggerID := TriggerID;
      P^.TriggerMask := $FFFF;
      GlobalUnlock(FMapData);
    end;
  end;
end;

procedure TAniMap.SetTrigger(X, Y: Integer; TriggerID: Smallint;
  TriggerMask: Word);
var
  GridLoc, P: ^GridInfo;
  Loc: Longint;
begin
  if (FMapData <> 0) then
  begin
    if (X < FWidth) and (Y < FHeight) then
    begin
      GridLoc := GlobalLock(FMapData);
      Loc := X + Y * FWidth;
      P := GridLoc;
      Inc(P, Loc);
      P^.TriggerID := TriggerID;
      P^.TriggerMask := TriggerMask;
      GlobalUnlock(FMapData);
    end;
  end;
end;

procedure TAniMap.SetFilter(X, Y: Integer; FilterID: Smallint);
var
  GridLoc, P: ^GridInfo;
  Loc: Longint;
begin
  if (FMapData <> 0) then
  begin
    if (X < FWidth) and (Y < FHeight) then
    begin
      GridLoc := GlobalLock(FMapData);
      Loc := X + Y * FWidth;
      P := GridLoc;
      Inc(P, Loc);
      P^.FilterID := FilterID;
      P^.FilterMask := $FFFF;
      GlobalUnlock(FMapData);
    end;
  end;
end;

procedure TAniMap.SetFilter(X, Y: Integer; FilterID: Smallint;
  FilterMask: Word);
var
  GridLoc, P: ^GridInfo;
  Loc: Longint;
begin
  if (FMapData <> 0) then
  begin
    if (X < FWidth) and (Y < FHeight) then
    begin
      GridLoc := GlobalLock(FMapData);
      Loc := X + Y * FWidth;
      P := GridLoc;
      Inc(P, Loc);
      P^.FilterID := FilterID;
      P^.FilterMask := FilterMask;
      GlobalUnlock(FMapData);
    end;
  end;
end;

function TAniMap.AddItem(Zone, ItemIndex: Word; X, Y, Z: Longint;
  FilterID: Smallint; Collidable: Boolean): PItemInstanceInfo;
var
  Strip, VStrip: Integer;
  i, j, k: Integer;
  SrcX, SrcY, SrcW: Longint;
  StripData: ^Word;
  GridBase, GridLoc: ^GridInfo;
  Strips, Rows: Integer;
  SrcMaskBase, SrcMask: ^Word;
  BitMask: Word;
  ZoneItem: TZone;
  First: Integer;
  RefItem, RefDelta, Delta: Integer;
  InitDelta: Boolean;
begin

  if (X < 0) and ((X mod FStripWidth) <> 0) then
    X := ((X div FStripWidth) - 1) * FStripWidth
  else
    X := (X div FStripWidth) * FStripWidth;
  if (Y < 0) and ((Y mod FStripHeight) <> 0) then
    Y := ((Y div FStripHeight) - 1) * FStripHeight
  else
    Y := (Y div FStripHeight) * FStripHeight;
  GridBase := GlobalLock(FMapData);

  ZoneItem := Zones[Zone];
  with ZoneItem as TZone do
  begin

    // Apply Collision Mask
    if Collidable then
    begin
      if (Item[ItemIndex].CollisionMasks <> 0) then
      begin
        SrcMaskBase := GlobalLock(Item[ItemIndex].CollisionMasks);
        if Assigned(SrcMaskBase) then
        begin
          Strips := Item[ItemIndex].Strips shl 2;
          Rows := Item[ItemIndex].Height div FStripHeight;
          if ((Item[ItemIndex].Height mod FStripHeight) <> 0) then
            Inc(Rows);
          for j := 0 to Rows - 1 do
          begin
            k := Y - (j + 1) * FStripHeight;
            if k >= 0 then
            begin
              Y1 := k div FTileHeight;
              if (Y1 < FHeight) then
              begin
                Y2 := (Y div FStripHeight) - j - 1;
                for i := 0 to Strips - 1 do
                begin
                  SrcMask := SrcMaskBase;
                  Inc(SrcMask, (j shr 2) * Item[ItemIndex].Strips + (i shr 2));
                  BitMask := 1 shl ((i mod 4) + ((j mod 4)) * 4);
                  if ((SrcMask^ and BitMask) = BitMask) then
                  begin
                    k := X + i * FStripWidth;
                    if k >= 0 then
                    begin
                      X1 := k div FTileWidth;
                      if (X1 < FWidth) then
                      begin
                        X2 := (X div FStripWidth) + i;
                        GridLoc := GridBase;
                        Inc(GridLoc, X1 + Y1 * FWidth);
                        BitMask := 1 shl ((X2 mod 4) + (3 - (Y2 mod 4)) * 4);
                        GridLoc^.CollisionMask :=
                          GridLoc^.CollisionMask or BitMask;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
        GlobalUnlock(Item[ItemIndex].CollisionMasks);
      end;
    end;

    // Apply Line of Sight Mask
    if Collidable then
    begin
      if (Item[ItemIndex].LineOfSightMasks <> 0) then
      begin
        SrcMaskBase := GlobalLock(Item[ItemIndex].LineOfSightMasks);
        if Assigned(SrcMaskBase) then
        begin
          Strips := Item[ItemIndex].Strips shl 2;
          Rows := Item[ItemIndex].Height div FStripHeight;
          if ((Item[ItemIndex].Height mod FStripHeight) <> 0) then
            Inc(Rows);
          for j := 0 to Rows - 1 do
          begin
            k := Y - (j + 1) * FStripHeight;
            if k >= 0 then
            begin
              Y1 := k div FTileHeight;
              if (Y1 < FHeight) then
              begin
                Y2 := (Y div FStripHeight) - j - 1;
                for i := 0 to Strips - 1 do
                begin
                  SrcMask := SrcMaskBase;
                  Inc(SrcMask, (j shr 2) * Item[ItemIndex].Strips + (i shr 2));
                  BitMask := 1 shl ((i mod 4) + ((j mod 4)) * 4);
                  if ((SrcMask^ and BitMask) = BitMask) then
                  begin
                    k := X + i * FStripWidth;
                    if k >= 0 then
                    begin
                      X1 := k div FTileWidth;
                      if (X1 < FWidth) then
                      begin
                        X2 := (X div FStripWidth) + i;
                        GridLoc := GridBase;
                        Inc(GridLoc, X1 + Y1 * FWidth);
                        BitMask := 1 shl ((X2 mod 4) + (3 - (Y2 mod 4)) * 4);
                        GridLoc^.LineOfSightMask :=
                          GridLoc^.LineOfSightMask or BitMask;
                      end;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
        GlobalUnlock(Item[ItemIndex].LineOfSightMasks);
      end;
    end;

    // Divide the image into strips and add each strip to the items list as a seperate item
    if LastItem >= ItemListSize then
    begin
      Log.Log('*** Error: number of items exceeds maximum');
      Result := nil;
      exit;
    end;
    First := LastItem + 1;
    InitDelta := False;
    Result := @FItemList[First];
    RefDelta := 0;
    RefItem := First;
    for Strip := 1 to Item[ItemIndex].Strips do
    begin
      SrcY := Item[ItemIndex].Top + (Strip - 1) * Item[ItemIndex].Height;
      for VStrip := 0 to 3 do
      begin
        SrcX := Item[ItemIndex].Left + VStrip * FStripWidth;
        SrcW := (Strip - 1) * FTileWidth + VStrip * FStripWidth;
        if (SrcW < Item[ItemIndex].Width) then
        begin
          Inc(LastItem);
          if LastItem <= ItemListSize then
          begin
            FItemList[LastItem].ImageX := SrcX;
            FItemList[LastItem].ImageY := SrcY;
            if (SrcW + FStripWidth > Item[ItemIndex].Width) then
              FItemList[LastItem].Width := Item[ItemIndex].Width - SrcW
            else
              FItemList[LastItem].Width := FStripWidth;
            FItemList[LastItem].Height := Item[ItemIndex].Height;
            FItemList[LastItem].Zone := Zone;
            FItemList[LastItem].FilterID := FilterID;
            FItemList[LastItem].XRayID := 0;
            FItemList[LastItem].Slope0 := Item[ItemIndex].Slope;
            FItemList[LastItem].Visible := Item[ItemIndex].Visible;
            FItemList[LastItem].AutoTransparent :=
              Item[ItemIndex].AutoTransparent;
            FItemList[LastItem].Vertical := Item[ItemIndex].Vertical;
            FItemList[LastItem].Last := False;
            FItemList[LastItem].Next := 0;

            if (Item[ItemIndex].StripHeights <> 0) then
            begin
              StripData := GlobalLock(Item[ItemIndex].StripHeights);
              Inc(StripData, ((Strip - 1) shl 2) + VStrip);
              FItemList[LastItem].VHeight := StripData^ + Z;
              GlobalUnlock(Item[ItemIndex].StripHeights);
            end;

            FItemList[LastItem].X := X + SrcW;
            FItemList[LastItem].Y := Y - FItemList[LastItem].Height +
              FItemList[LastItem].VHeight;

            if FItemList[LastItem].VHeight <> 0 then
            begin
              Delta := FItemList[LastItem].Y -
                round(SrcW * FItemList[LastItem].Slope0);
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
                InitDelta := True;
              end;
            end;

            if (Item[ItemIndex].LightPoints <> 0) then
            begin
              StripData := GlobalLock(Item[ItemIndex].LightPoints);
              Inc(StripData, ((Strip - 1) shl 2) + VStrip);
              FItemList[LastItem].Slope1 := Smallint(StripData^) / FStripWidth;
              FItemList[LastItem].Slope2 := FItemList[LastItem].Slope1;
              GlobalUnlock(Item[ItemIndex].LightPoints);
            end;

            // Keep Items sorted by VY     //Replaced by BuildRowUpdateInfo
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
              end; }
          end;
        end;
      end;
    end;

    for i := First to LastItem do
    begin
      FItemList[i].RefItem := RefItem;
    end;

    FItemList[LastItem].Last := True;
  end;
  GlobalUnlock(FMapData);
end;

procedure TAniMap.SetTileSize(Size: Word);
begin
  FTileSize := Size;
  FTileWidth := FTileSize * 16;
  FTileHeight := FTileSize * 8;
  FStripWidth := FTileWidth shr 2;
  FStripHeight := FTileHeight shr 2;
  FBitWidth := FWidth * FTileWidth;
  FBitHeight := FHeight * FTileHeight;
end;

end.
