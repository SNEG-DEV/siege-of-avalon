unit Zone;

interface

uses
  Classes,
  sdl,
  SiegeTypes,
  AniMap;

type
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
    FTilesInVideo : Boolean;
    FItemsInVideo : Boolean;
    FTileImages : PSDL_Surface;
    FItemImages : PSDL_Surface;
    function GetTile( i : Integer ) : TileInfo;
    procedure SetTile( i : Integer; Tile : TileInfo );
    function GetItem( i : Integer ) : ItemInfo;
    procedure SetItem( i : Integer; Item : ItemInfo );
  public
    FullRefresh : boolean;
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
    procedure DefineTile( Index : Word; Image : PSDL_Surface; Color : TSDL_Color );
    function DefineItem( Index : Word; Image : PSDL_Surface; const StripHeights, CollisionMasks, LineOfSightMasks, LightPoints : array of Word; Color : TSDL_Color; Slope : Single; Visible, AutoTransparent, Vertical : boolean ) : PItemInfo;
    procedure MoveToVideo;
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
    property Index : Integer read FIndex write FIndex;
    property TileImages : PSDL_Surface read FTileImages write FTileImages;
    property ItemImages : PSDL_Surface read FItemImages write FItemImages;
    property TileBitWidth : Integer read FTileBitWidth write FTileBitWidth;
    property TileBitHeight : Integer read FTileBitHeight write FTileBitHeight;
    property TileMaxIndex : Word read FTileMaxIndex write FTileMaxIndex;
    property TileMaxColumnIndex : Word read FTileMaxColumnIndex write FTileMaxColumnIndex;
    property ItemBitWidth : Integer read FItemBitWidth write FItemBitWidth;
    property ItemBitHeight : Integer read FItemBitHeight write FItemBitHeight;
    property ItemColumn : Integer read FItemColumn write FItemColumn;
    property ItemColumnBitHeight : Integer read FItemColumnBitHeight write FItemColumnBitHeight;
    property TilesInVideo : Boolean read FTilesInVideo write FTilesInVideo;
    property ItemsInVideo : Boolean read FItemsInVideo write FItemsInVideo;
    property Tile[ i : Integer ] : TileInfo read GetTile write SetTile;
    property Item[ i : Integer ] : ItemInfo read GetItem write SetItem;
  end;

  TLightZone = class( TZone )
  private
    AddColumn : boolean;
  public
    TileStateOffset : Longint;
    Blinking : Boolean;
    StateDuration : Integer;
    ItemStateOffset : Longint;
    Items : TList;
    OverlapZones : TList;
    FlickerX : array[ 1..MaxLightStates ] of Longint;
    FlickerY : array[ 1..MaxLightStates ] of Longint;
    FlickerZ : array[ 1..MaxLightStates ] of Longint;
    FlickerRadius : array[ 1..MaxLightStates ] of Longint;
    FlickerIntensity : array[ 1..MaxLightStates ] of Longint;
    State : Integer;
    States : Integer;
    Color : TSDL_Color;
    Intensity : integer;
    Radius : Longint;
    Flicker : TFlicker;
    X : Longint;
    Y : Longint;
    Z : Longint;
    procedure NewTileState;
    procedure AddStrip( Image : PSDL_Surface; var NewX, NewY : word );
    procedure NewItemState;
    constructor Create( Map : TAniMap );
    destructor Destroy; override;
    procedure LoadTileCustomData( Stream : TStream ); override;
    procedure SaveTileCustomData( Stream : TStream ); override;
    procedure LoadItemCustomData( Stream : TStream ); override;
    procedure SaveItemCustomData( Stream : TStream ); override;
  end;

implementation

uses
  SysUtils,
  logger;

{ TZone }

constructor TZone.Create( Map : TAniMap );
begin
  inherited Create;
  FMap := Map;
  FTileBitWidth := Map.TileWidth;
  FItemBitWidth := Map.TileWidth;
  FTileMaxColumnIndex := MaxZoneHeight div Map.TileHeight;
end;

destructor TZone.Destroy;
begin
  DisposeDef;
  FTileImages := nil;
  FItemImages := nil;
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
      FillChar( FTileMem, SizeOf( TileInfo ) * MaxTiles, 0 );
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
          // TODO : GlobalFree( Item[ i ].StripHeights );
        end;
        if ( Item[ i ].CollisionMasks <> 0 ) then
        begin
          // TODO : GlobalFree( Item[ i ].CollisionMasks );
        end;
        if ( Item[ i ].LineOfSightMasks <> 0 ) then
        begin
          // TODO : GlobalFree( Item[ i ].LineOfSightMasks );
        end;
        if ( Item[ i ].LightPoints <> 0 ) then
        begin
          // TODO : GlobalFree( Item[ i ].LightPoints );
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
      FillChar( FItemMem, SizeOf( ItemInfo ) * MaxItems, 0 );
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
      // TODO : GlobalFree( p^.CollisionMasks );
      if ( p^.LineOfSightMasks <> 0 ) then
      // TODO : GlobalFree( p^.LineOfSightMasks );
        if ( p^.StripHeights <> 0 ) then
      // TODO : GlobalFree( p^.StripHeights );
          if ( p^.LightPoints <> 0 ) then
      // TODO : GlobalFree( p^.LightPoints );
            p^ := Item;
    p^.Used := True;
  end;
end;

procedure TZone.Release;
begin
  FTileImages := nil;
  FItemImages := nil;
  Cached := True;
end;

procedure TZone.Restore;
begin
  Cached := True;
end;

function TZone.DefineItem( Index : Word; Image : PSDL_Surface; const StripHeights,
  CollisionMasks, LineOfSightMasks, LightPoints : array of Word; Color : TSDL_Color; Slope : Single; Visible,
  AutoTransparent, Vertical : Boolean ) : PItemInfo;
var
  SrcDC : PSDL_Surface;
  Picture, Mask : PSDL_Surface;
  NewBitWidth, NewBitHeight : Longint;
  i, X : Integer;
  StripData, TileData : ^Word;
  Strips, Rows, Tiles : Integer;
  W, H : integer;
  NewItemImages : PSDL_Surface;
  SrcX1, SrcX2, SrcY1, SrcY2 : integer;
  Bitmap : PSDL_Surface;
  NewItem : ItemInfo;
begin
  // TODO :
  (* if Loaded then
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
    GetSurfaceDims( W, H, Image );
    NewItem.Width := W;
    NewItem.Height := H;
    NewItem.Strips := W div FMap.TileWidth;
    NewItem.StripCount := W div FMap.FStripWidth;
    NewItem.Slope := Slope;
    NewItem.AutoTransparent := AutoTransparent;
    NewItem.Vertical := Vertical;
    NewItem.Visible := Visible;
    if ( ( W mod FMap.TileWidth ) <> 0 ) then
      Inc( NewItem.Strips );
    if ( ( W mod FMap.FStripWidth ) <> 0 ) then
      Inc( NewItem.StripCount );
    NewBitWidth := ( FItemColumn + 1 ) * FMap.TileWidth;
    NewBitHeight := FItemColumnBitHeight + NewItem.Strips * NewItem.Height;
    if NewBitHeight > MaxZoneHeight then
    begin
      inc( FItemColumn );
      FItemColumnBitHeight := 0;
      NewBitHeight := FItemColumnBitHeight + NewItem.Strips * NewItem.Height;
      inc( NewBitWidth, FMap.TileWidth );
    end;
    if NewBitWidth < FItemBitWidth then
      NewBitWidth := FItemBitWidth;
    if NewBitHeight < FItemBitHeight then
      NewBitHeight := FItemBitHeight;
    X := FItemColumn * FMap.TileWidth;
    NewItem.Top := FItemColumnBitHeight;
    NewItem.Left := X;

    //Strip Height Data
    if ( High( StripHeights ) < 0 ) then
    begin
      Log.Log( 'Generating default depth anchors' );
      Bitmap := TBitmap.create;
      Bitmap.width := W;
      Bitmap.height := H;
      Image.GetDC( SrcDC );
      BitBlt( Bitmap.canvas.handle, 0, 0, W, H, SrcDC, 0, 0, SRCCOPY );
      Image.ReleaseDC( SrcDC );
      CreateMask( Picture, Mask, Bitmap, ColorToRGB( Color ) );
      Bitmap.free;
      GetStripHeights( NewItem.StripHeights, Mask, NewItem.Width, NewItem.Height, FMap.FStripWidth );
      DeleteObject( Picture );
      DeleteObject( Mask );
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
      Rows := NewItem.Height div FMap.TileHeight;
      if ( ( NewItem.Height mod FMap.TileHeight ) <> 0 ) then
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
      Rows := NewItem.Height div FMap.TileHeight;
      if ( ( NewItem.Height mod FMap.TileHeight ) <> 0 ) then
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

    if ( NewBitWidth > FItemBitWidth ) or ( NewBitHeight > FItemBitHeight ) then
    begin
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
      WrapperBlt( NewItemImages, Rect(0, 0, NewBitWidth, NewBitHeight), nil,
        Rect(0, 0, NewBitWidth, NewBitHeight), DDBLT_COLORFILL + DDBLT_WAIT, BltFx); }

      if Assigned( FItemImages ) then
      begin
        WrapperBltFast( NewItemImages, 0, 0, FItemImages, Rect( 0, 0, FItemBitWidth, FItemBitHeight ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
      end;
      ddck.dwColorSpaceLowValue := FMap.ColorMatch;
      ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
      NewItemImages.SetColorKey( DDCKEY_SRCBLT, {$IFNDEF DX5}@{$ENDIF}ddck );

      FItemImages := nil;
      FItemImages := NewItemImages;

      if NewBitWidth > FItemBitWidth then
        FItemBitWidth := NewBitWidth;
      if NewBitHeight > FItemBitHeight then
        FItemBitHeight := NewBitHeight;
    end;

    for i := 1 to NewItem.Strips do
    begin
      SrcX1 := ( i - 1 ) * FMap.TileWidth;
      SrcY1 := 0;
      SrcX2 := SrcX1 + FMap.TileWidth;
      SrcY2 := SrcY1 + NewItem.Height;
      if SrcX2 > W then
        SrcX2 := W;
      if SrcY2 > H then
        SrcY2 := H;
      WrapperBltFast( FItemImages, X, FItemColumnBitHeight + ( i - 1 ) * NewItem.Height, Image,
        Rect( SrcX1, SrcY1, SrcX2, SrcY2 ),
        DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
    end;

    FItemColumnBitHeight := FItemColumnBitHeight + NewItem.Strips * NewItem.Height;
    Item[ Index ] := NewItem;
  end;
  Result := Pointer( FItemMem );
  Inc( Result, Index - 1 );*)
end;

procedure TZone.DefineTile( Index : Word; Image : PSDL_Surface; Color : TSDL_Color );
var
  NewMaxIndex : Word;
  NewBitWidth, NewBitHeight : Longint;
  i, j, X, Y : Integer;
  NewTile : TileInfo;
  W, H : integer;
  NewTileImages : PSDL_Surface;
    //TODO : ddsd : TDDSurfaceDesc;
    //TODO : ddck : TDDCOLORKEY;
    //TODO : BltFx : TDDBLTFX;
  SrcX1, SrcX2, SrcY1, SrcY2 : integer;
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
      //TODO : GetSurfaceDims( W, H, Image );
    NewTile.ImageIndex := FTileMaxIndex;
    NewTile.Columns := W div FMap.TileWidth;
    if ( ( W mod FMap.TileWidth ) <> 0 ) then
      Inc( NewTile.Columns );
    NewTile.Rows := H div FMap.TileHeight;
    if ( ( H mod FMap.TileHeight ) <> 0 ) then
      Inc( NewTile.Rows );
    NewMaxIndex := FTileMaxIndex + NewTile.Columns * NewTile.Rows;
    NewBitWidth := ( ( ( NewMaxIndex - 1 ) div FTileMaxColumnIndex ) + 1 ) * FMap.TileWidth;
    if NewBitWidth > FMap.TileWidth then
      NewBitHeight := MaxZoneHeight
    else
      NewBitHeight := NewMaxIndex * FMap.TileHeight;
    X := ( ( FTileMaxIndex - 1 ) div FTileMaxColumnIndex ) * FMap.TileWidth;
    Y := ( FTileMaxIndex mod FTileMaxColumnIndex ) * FMap.TileHeight;
    if ( ( FTileMaxIndex mod FTileMaxColumnIndex ) = 0 ) and ( FTileMaxIndex > 0 ) then
      inc( X, FMap.TileWidth );
    Tile[ Index ] := NewTile;
    if ( NewBitWidth > FTileBitWidth ) or ( NewBitHeight > FTileBitHeight ) then
    begin
//      Log.Log('Resize Zone Tiles');
//Log.Log(inttostr(NewBitWidth)+' '+inttostr(FTileBitWidth));
//Log.Log(inttostr(NewBitHeight)+' '+inttostr(FTileBitHeight));
        //TODO : ddsd.dwSize := SizeOf( ddsd );
        //TODO : ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
        //TODO : ddsd.dwWidth := NewBitWidth;
        //TODO : ddsd.dwHeight := NewBitHeight;
      if TilesInVideo then
      begin
          //TODO : ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_VIDEOMEMORY;
          //TODO : if lpdd.CreateSurface( ddsd, NewTileImages, nil ) <> DD_OK then
          //TODO :  begin
          //TODO :   ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
          //TODO :   lpdd.CreateSurface( ddsd, NewTileImages, nil );
          //TODO :   TilesInVideo := false;
          //TODO : end;
      end
      else
      begin
          //TODO : ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
          //TODO : lpdd.CreateSurface( ddsd, NewTileImages, nil );
      end;

        //TODO : BltFx.dwSize := SizeOf( BltFx );
        //TODO : BltFx.dwFillColor := FMap.ColorMatch;
        //TODO : WrapperBlt( NewTileImages, Rect( 0, 0, NewBitWidth, NewBitHeight ), nil,
        //TODO :   Rect( 0, 0, NewBitWidth, NewBitHeight ), DDBLT_COLORFILL + DDBLT_WAIT, BltFx );
      if Assigned( FTileImages ) then
      begin
          //TODO : WrapperBltFast( NewTileImages, 0, 0, FTileImages, Rect( 0, 0, FTileBitWidth, FTileBitHeight ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
      end;

        //TODO : ddck.dwColorSpaceLowValue := FMap.ColorMatch;
        //TODO : ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
        //TODO : NewTileImages.SetColorKey( DDCKEY_SRCBLT, {$IFNDEF DX5}@{$ENDIF}ddck );

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
          inc( X, FMap.TileWidth );
          Y := 0;
        end;
        SrcX1 := i * FMap.TileWidth;
        SrcY1 := j * FMap.TileHeight;
        SrcX2 := SrcX1 + FMap.TileWidth;
        SrcY2 := SrcY1 + FMap.TileHeight;
        if SrcX2 > W then
          SrcX2 := W;
        if SrcY2 > H then
          SrcY2 := H;
          // TODO : WrapperBltFast( FTileImages, X, Y, Image, Rect( SrcX1, SrcY1, SrcX2, SrcY2 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        inc( Y, FMap.TileHeight );
      end;
    end;

    FTileMaxIndex := NewMaxIndex;
  end;
end;

procedure TZone.ExportTiles( Filename : string );
var
  BM : PSDL_Surface;
    // TODO : DC : HDC;
begin
  try
    if assigned( FTileImages ) then
    begin
        // TODO : BM := TBitmap.create;
      try
          // TODO : BM.width := FTileBitWidth;
          // TODO : BM.Height := FTileBitHeight;
          // TODO : FTileImages.GetDC( DC );
          // TODO : BitBlt( BM.canvas.handle, 0, 0, FTileBitWidth, FTileBitHeight, DC, 0, 0, SRCCOPY );
          // TODO : FTileImages.ReleaseDC( DC );
          // TODO : BM.SaveToFile( Filename );
      finally
          // TODO : BM.free;
      end;
    end;
  except
  end;
end;

procedure TZone.ExportItems( Filename : string );
var
  BM : PSDL_Surface;
    // TODO : DC : HDC;
begin
  try
    if assigned( FItemImages ) then
    begin
        // TODO : BM := TBitmap.create;
      try
          // TODO : BM.width := FItemBitWidth;
          // TODO : BM.Height := FItemBitHeight;
          // TODO : FItemImages.GetDC( DC );
          // TODO : BitBlt( BM.canvas.handle, 0, 0, FItemBitWidth, FItemBitHeight, DC, 0, 0, SRCCOPY );
          // TODO : FItemImages.ReleaseDC( DC );
          // TODO : BM.SaveToFile( Filename );
      finally
          // TODO : BM.free;
      end;
    end;
  except
  end;
end;

procedure TZone.SaveToStream( Stream : TStream; SaveImage : boolean );
var
    // TODO : ddsd : TDDSurfaceDesc;
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
      // TODO : ddsd.dwSize := SizeOf( ddsd );
      {if FTileImages.Lock( nil, ddsd, DDLOCK_WAIT, 0 ) = DD_OK then
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
      end;}
  end;

//  Log.Log('Saving items '+IntToStr(FItembitWidth)+'x'+IntToStr(FItemBitHeight));
  Stream.write( FItemBitWidth, sizeof( FItembitWidth ) );
  Stream.write( FItemBitHeight, sizeof( FItemBitHeight ) );
  L := 0;
  Stream.write( L, sizeof( L ) );

  if SaveImage and ( FItemBitWidth > 0 ) and ( FItemBitHeight > 0 ) then
  begin
      // TODO : ddsd.dwSize := SizeOf( ddsd );
      {if FItemImages.Lock( nil, ddsd, DDLOCK_WAIT, 0 ) = DD_OK then
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
      end}
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
    // TODO : ddsd : TDDSurfaceDesc;
    // TODO : ddck : TDDCOLORKEY;
    // TODO : BltFx : TDDBLTFX;
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
      // TODO : ddsd.dwSize := SizeOf( ddsd );
      // TODO : ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
      // TODO : ddsd.dwWidth := FTileBitWidth;
      // TODO : ddsd.dwHeight := FTileBitHeight;
      // TODO : ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
      // TODO : lpdd.CreateSurface( ddsd, FTileImages, nil );

      // TODO : BltFx.dwSize := SizeOf( BltFx );
      // TODO : BltFx.dwFillColor := FMap.ColorMatch;
      // TODO : WrapperBlt( FTileImages, Rect( 0, 0, FTileBitWidth, FTileBitHeight ), nil, Rect( 0, 0, FTileBitWidth, FTileBitHeight ), DDBLT_COLORFILL + DDBLT_WAIT, BltFx );

      // TODO : ddck.dwColorSpaceLowValue := FMap.ColorMatch;
      // TODO : ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
      // TODO : FTileImages.SetColorKey( DDCKEY_SRCBLT, {$IFNDEF DX5}@{$ENDIF}ddck );
  end;

  if L > 0 then
  begin
    LoadTileCustomData( Stream );
    if Assigned( FTileImages ) then
    begin
        // TODO : ddsd.dwSize := SizeOf( ddsd );
        {if FTileImages.Lock( nil, ddsd, DDLOCK_WAIT, 0 ) = DD_OK then
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
        end;}
      Loaded := true;
    end;
  end;

  Stream.read( FItemBitWidth, sizeof( FItemBitWidth ) );
  Stream.read( FItemBitHeight, sizeof( FItemBitHeight ) );
  Stream.read( L, sizeof( L ) );
//  Log.Log('Sizing items to '+IntToStr(FItemBitWidth)+'x'+IntToStr(FItemBitHeight));

  if ( FItemBitWidth > 0 ) and ( FItemBitHeight > 0 ) then
  begin
      // TODO : FItemImages := nil;
      (*ddsd.dwSize := SizeOf( ddsd );
      ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
      ddsd.dwWidth := FItemBitWidth;
      ddsd.dwHeight := FItemBitHeight;
      ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
      lpdd.CreateSurface( ddsd, FItemImages, nil );

      ddck.dwColorSpaceLowValue := FMap.ColorMatch;
      ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
      FItemImages.SetColorKey( DDCKEY_SRCBLT, {$IFNDEF DX5}@{$ENDIF}ddck );*)
  end;

  if L > 0 then
  begin
    LoadItemCustomData( Stream );
    if assigned( FItemImages ) then
    begin
        // TODO : ddsd.dwSize := SizeOf( ddsd );
        {if FItemImages.Lock( nil, ddsd, DDLOCK_WAIT, 0 ) = DD_OK then
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
        end;}
      Loaded := true;
    end;
  end;
  FullRefresh := TilesInVideo and ItemsInVideo;
end;

procedure TZone.MoveToVideo;
var
  NewTileImages : PSDL_Surface;
  NewItemImages : PSDL_Surface;
  // TODO : ddsd : TDDSurfaceDesc;
  // TODO : ddck : TDDCOLORKEY;
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
      // TODO :
      (*ddsd.dwSize := SizeOf( ddsd );
      ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
      ddsd.dwWidth := FItemBitWidth;
      ddsd.dwHeight := FItemBitHeight;
      ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_VIDEOMEMORY;
      if lpdd.CreateSurface( ddsd, NewItemImages, nil ) = DD_OK then
      begin
        for i := 0 to FItemBitHeight div SectionHeight - 1 do
        begin
          WrapperBltFast( NewItemImages, 0, i * SectionHeight, FItemImages, Rect( 0, i * SectionHeight, FItemBitWidth, ( i + 1 ) * SectionHeight ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
        end;
        Rem := FItemBitHeight mod SectionHeight;
        if Rem > 0 then
        begin
          WrapperBltFast( NewItemImages, 0, FItemBitHeight - Rem, FItemImages, Rect( 0, FItemBitHeight - Rem, FItemBitWidth, FItemBitHeight ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
        end;

        FItemImages := nil;
        FItemImages := NewItemImages;
        ddck.dwColorSpaceLowValue := FMap.ColorMatch;
        ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
        FItemImages.SetColorKey( DDCKEY_SRCBLT, {$IFNDEF DX5}@{$ENDIF}ddck );
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
      end;*)
    end;
    if ItemsInVideo then
      Log.LogStatus( 'Zone ' + IntToStr( Index ) + ' items moved to video', 'TZone.MoveToVideo' )
    else
      Log.LogStatus( 'Zone ' + IntToStr( Index ) + ' items could not be moved to video', 'TZone.MoveToVideo' );
  end;

  if not TilesInVideo then
  begin
    if ( FTileBitHeight = 0 ) then
    begin
      TilesInVideo := true;
    end
    else
    begin
      // TODO :
      (*ddsd.dwSize := SizeOf( ddsd );
      ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
      ddsd.dwWidth := FTileBitWidth;
      ddsd.dwHeight := FTileBitHeight;
      ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_VIDEOMEMORY;
      if lpdd.CreateSurface( ddsd, NewTileImages, nil ) = DD_OK then
      begin
        for i := 0 to FTileBitHeight div SectionHeight - 1 do
        begin
          WrapperBltFast( NewTileImages, 0, i * SectionHeight, FTileImages, Rect( 0, i * SectionHeight, FTileBitWidth, ( i + 1 ) * SectionHeight ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
        end;
        Rem := FTileBitHeight mod SectionHeight;
        if Rem > 0 then
        begin
          WrapperBltFast( NewTileImages, 0, FTileBitHeight - Rem, FTileImages, Rect( 0, FTileBitHeight - Rem, FTileBitWidth, FTileBitHeight ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
        end;

        FTileImages := nil;
        FTileImages := NewTileImages;
        ddck.dwColorSpaceLowValue := FMap.ColorMatch;
        ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
        FTileImages.SetColorKey( DDCKEY_SRCBLT, {$IFNDEF DX5}@{$ENDIF}ddck );
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
      end;*)
    end;
    if TilesInVideo then
      Log.LogStatus( 'Zone ' + IntToStr( Index ) + ' tiles moved to video', 'TZone.MoveToZone' )
    else
      Log.LogStatus( 'Zone ' + IntToStr( Index ) + ' tiles could not be moved to video', 'TZone.MoveToZone' );
  end;
end;

{ TLightZone }

constructor TLightZone.Create( Map : TAniMap );
begin
  inherited;
  FItemBitWidth := Map.StripWidth;
end;

procedure TLightZone.AddStrip( Image : PSDL_Surface; var NewX, NewY : word );
var
  NewBitWidth, NewBitHeight : Longint;
  W, H : integer;
  NewItemImages : PSDL_Surface;
  // TODO : ddsd : TDDSurfaceDesc;
  // TODO : ddck : TDDCOLORKEY;
begin
  if ( Image = nil ) then
    Exit;
  // TODO : GetSurfaceDims( W, H, Image );


  if AddColumn or ( FItemColumnBitHeight + H > MaxZoneHeight ) then
  begin
    AddColumn := false;
    inc( FItemColumn );
    NewBitWidth := ( FItemColumn + 1 ) * FMap.StripWidth;
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

  NewX := FItemColumn * FMap.StripWidth;
  NewY := FItemColumnBitHeight;

  if ( NewBitHeight > FItemBitHeight ) or ( NewBitWidth > FItemBitWidth ) then
  begin
    // TODO :
    {ddsd.dwSize := SizeOf( ddsd );
    ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
    ddsd.dwWidth := NewBitWidth;
    ddsd.dwHeight := NewBitHeight;
    ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
    lpdd.CreateSurface( ddsd, NewItemImages, nil );}

  {  BltFx.dwSize := SizeOf(BltFx);
    BltFx.dwFillColor := FMap.ColorMatch;
    WrapperBlt( NewItemImages, Rect(0, 0, NewBitWidth, NewBitHeight), nil,
      Rect(0, 0, NewBitWidth, NewBitHeight), DDBLT_COLORFILL + DDBLT_WAIT, BltFx);  }

    if Assigned( FItemImages ) then
    begin
      // TODO : WrapperBltFast( NewItemImages, 0, 0, FItemImages, Rect( 0, 0, FItemBitWidth, FItemBitHeight ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
    end;
    // TODO : WrapperBltFast( NewItemImages, NewX, NewY, Image, Rect( 0, 0, W, H ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );

    // TODO : ddck.dwColorSpaceLowValue := FMap.ColorMatch;
    // TODO : ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
    // TODO : NewItemImages.SetColorKey( DDCKEY_SRCBLT, {$IFNDEF DX5}@{$ENDIF}ddck );

    FItemImages := nil;
    FItemImages := NewItemImages;
    if NewBitWidth > FItemBitWidth then
      FItemBitWidth := NewBitWidth;
    FItemBitHeight := NewBitHeight;
  end
  else
  begin
    // TODO : WrapperBltFast( FItemImages, NewX, NewY, Image, Rect( 0, 0, W, H ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
  end;
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
      Items.add( @FMap.ItemList[ L ] );
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

end.

