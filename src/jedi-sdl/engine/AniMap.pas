unit AniMap;

interface

uses
  Classes,
  sdl,
  SiegeTypes;

type
  TAniMap = class( TObject )
  private
    FWidth : Longint;
    FHeight : Longint;
    FMapData : HGLOBAL;
    FTransparentColor : TSDL_Color;
    FColorMatch : word;
    FAmbientColor : TSDL_Color;
    FAmbientIntensity : integer;
    FUseLighting : Boolean;
    FUseAmbientOnly : Boolean;
    FTileSize : Word;

    LastItem : Word;
    //    SubMaps           :TList;
    procedure SetWidth( VWidth : Longint );
    procedure SetHeight( VHeight : Longint );
    procedure SetTileSize( Size : Word );
    procedure SetAmbientColor( Color : TSDL_Color );
    procedure SetAmbientIntensity( Value : Integer );
    function GetZoneCount : word;
    procedure SetTransparentColor( Color : TSDL_Color );
    function GetColorMatch : word;
  protected
  public
    LightR : double;
    LightG : double;
    LightB : double;
    FirstItem : Word;
    NeedColorMatch : boolean;
    StripWidth : Word;
    StripHeight : Word;
    TileHeight : Word;
    TileWidth : Word;
    BitWidth : Longint;
    BitHeight : Longint;
    Zones : TList;
    //Item Instance Information
    ItemList : array[ 1..ItemListSize ] of ItemInstanceInfo;
    constructor Create;
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
    procedure DefineTile( Zone, Index : Word; Image : PSDL_Surface );
    function DefineItem( Zone, Index : Word; Image : PSDL_Surface; const StripHeights, CollisionMasks, LineOfSightMasks, LightPoints : array of Word; Slope : Single; Visible, AutoTransparent, Vertical : Boolean ) : PItemInfo;
    procedure Sort;
    procedure RenderMap;
    procedure MoveZoneToVideo( Index : integer );
    function GetZoneMemoryUsage( Index : integer ) : longint;
    function AddZone : Word;
    function AddLightZone : Word;
    function AddLight( Color : TSDL_Color; Intensity : Integer; Radius : Longint; Flicker : TFlicker; X, Y, Z : Longint ) : Word;
    function LineOfSight( X1, Y1, X2, Y2 : Longint ) : Boolean;
    function LineOfCollision( X1, Y1, X2, Y2 : Longint ) : Boolean;
    function GetItemIndex( Item : PItemInstanceInfo ) : word;
    function GetItemAddress( Index : word ) : PItemInstanceInfo;
    procedure SaveMapKnownInfo( Stream : TStream );
    procedure LoadMapKnownInfo( Stream : TStream );
    procedure SaveToStream( Stream : TStream );
    procedure LoadFromStream( Stream : TStream );
    property ItemCount : word read LastItem;
    property ColorMatch : word read GetColorMatch;
  published
    property Width : Longint read FWidth write SetWidth default 10;
    property Height : Longint read FHeight write SetHeight default 20;
    property TileSize : Word read FTileSize write SetTileSize default 4;
    property TransparentColor : TSDL_Color read FTransparentColor write SetTransparentColor;
    property AmbientIntensity : Integer read FAmbientIntensity write SetAmbientIntensity;
    property AmbientColor : TSDL_Color read FAmbientColor write SetAmbientColor;
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

implementation

uses
  Zone;

constructor TAniMap.Create;
var
  GridSize, i : Longint;
  GridLoc : ^GridInfo;
  NewZone : TZone;
begin
  inherited;
  FWidth := 10;
  FHeight := 20;
  FTileSize := 4;
  TileWidth := FTileSize * 16;
  TileHeight := FTileSize * 8;
  BitWidth := FWidth * TileWidth;
  BitHeight := FHeight * TileHeight;
  StripWidth := TileWidth shr 2;
  StripHeight := TileHeight shr 2;
  // clFuchsia
  FTransparentColor.r := 0;
  FTransparentColor.g := 255;
  FTransparentColor.b := 255;
  NeedColorMatch := true;
  if ( FWidth > 0 ) and ( FHeight > 0 ) then
  begin
    GridSize := FWidth * FHeight;
    // TODO : FMapData := GlobalAlloc( GPTR, GridSize * SizeOf( GridInfo ) );
    // TODO : GridLoc := GlobalLock( FMapData );
    for i := 1 to GridSize do
    begin
      GridLoc^.Tile[ 0 ] := $FFFF;
      GridLoc^.Tile[ 1 ] := $FFFF;
      Inc( GridLoc );
    end;
    // TODO : GlobalUnlock( FMapData );
  end;
  Zones := TList.Create;
  NewZone := TZone.Create( Self );
  Zones.Add( NewZone );
end;

destructor TAniMap.Destroy;
var
  i : Integer;
begin
//  if not (csDesigning in ComponentState) then begin
  if ( FMapData <> 0 ) then
  begin
    // TODO : GlobalFree( FMapData );
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
  Stream.write( ItemList[ 1 ], LastItem * sizeof( ItemInstanceInfo ) );
  Stream.write( FWidth, sizeof( FWidth ) );
  Stream.write( FHeight, sizeof( FHeight ) );
  MemSize := FWidth * FHeight * sizeof( GridInfo );
  // TODO : GridLoc := GlobalLock( FMapData );
  Stream.write( GridLoc^, MemSize );
end;

procedure TAniMap.LoadFromStream( Stream : TStream );
var
  GridLoc : ^GridInfo;
  MemSize : longint;
begin
  Stream.read( FirstItem, sizeof( FirstItem ) );
  Stream.read( LastItem, sizeof( LastItem ) );
  Stream.read( ItemList[ 1 ], LastItem * sizeof( ItemInstanceInfo ) );
  Stream.read( FWidth, sizeof( FWidth ) );
  Stream.read( FHeight, sizeof( FHeight ) );
  BitWidth := FWidth * TileWidth;
  BitHeight := FHeight * TileHeight;

  MemSize := FWidth * FHeight * sizeof( GridInfo );
  if ( FMapData <> 0 ) then
  begin
    // TODO : GlobalFree( FMapData );
    FMapData := 0;
  end;
  // TODO : FMapData := GlobalAlloc( GMEM_FIXED, MemSize );
  // TODO : GridLoc := GlobalLock( FMapData );
  Stream.read( GridLoc^, MemSize );
end;

function TAniMap.GetItemIndex( Item : PItemInstanceInfo ) : word;
begin
  result := 1 + ( longword( Item ) - longword( @ItemList[ 1 ] ) ) div sizeof( ItemInstanceInfo );
end;

function TAniMap.GetItemAddress( Index : word ) : PItemInstanceInfo;
begin
  result := @ItemList[ Index ];
end;

procedure TAniMap.FreeResources;
var
  i : Integer;
begin
  for i := 0 to Zones.Count - 1 do
  begin
    TZone( Zones.Items[ i ] ).TileImages := nil;
    TZone( Zones.Items[ i ] ).ItemImages := nil;
    TZone( Zones.Items[ i ] ).Cached := False;
    TZone( Zones.Items[ i ] ).TileBitWidth := 0;
    TZone( Zones.Items[ i ] ).TileBitHeight := 0;
    TZone( Zones.Items[ i ] ).TileMaxIndex := 0;
    TZone( Zones.Items[ i ] ).TileMaxColumnIndex := 0;
    TZone( Zones.Items[ i ] ).ItemBitWidth := 0;
    TZone( Zones.Items[ i ] ).ItemBitHeight := 0;
    TZone( Zones.Items[ i ] ).ItemColumn := 0;
    TZone( Zones.Items[ i ] ).ItemColumnBitHeight := 0;
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
    // TODO : GlobalFree( FMapData );
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
  BitWidth := FWidth * TileWidth;
  if ( FMapData <> 0 ) then
  begin
    // TODO : GlobalFree( FMapData );
    FMapData := 0;
  end;
  if ( FWidth > 0 ) and ( FHeight > 0 ) then
  begin
    GridSize := FWidth * FHeight;
    // TODO : FMapData := GlobalAlloc( GPTR, GridSize * SizeOf( GridInfo ) );
    // TODO : GridLoc := GlobalLock( FMapData );
    for i := 1 to GridSize do
    begin
      GridLoc^.Tile[ 0 ] := $FFFF;
      GridLoc^.Tile[ 1 ] := $FFFF;
      Inc( GridLoc );
    end;
    // TODO : GlobalUnlock( FMapData );
  end;
end;

procedure TAniMap.SetHeight( VHeight : Longint );
var
  i, GridSize : Longint;
  GridLoc : ^GridInfo;
begin
  FHeight := VHeight;
  BitHeight := FHeight * TileHeight;
  if ( FMapData <> 0 ) then
  begin
    // TODO : GlobalFree( FMapData );
    FMapData := 0;
  end;
  if ( FWidth > 0 ) and ( FHeight > 0 ) then
  begin
    GridSize := FWidth * FHeight;
    // TODO : FMapData := GlobalAlloc( GPTR, GridSize * SizeOf( GridInfo ) );
    // TODO : GridLoc := GlobalLock( FMapData );
    for i := 1 to GridSize do
    begin
      GridLoc^.Tile[ 0 ] := $FFFF;
      GridLoc^.Tile[ 1 ] := $FFFF;
      Inc( GridLoc );
    end;
    // TODO : GlobalUnlock( FMapData );
  end;
end;

procedure TAniMap.SetAmbientColor( Color : TSDL_Color );
begin
  FAmbientColor.r := Color.r;
  FAmbientColor.g := Color.g;
  FAmbientColor.b := Color.b;
  LightR := ( FAmbientColor.r ) * ( FAmbientIntensity / 100 );
  LightG := ( ( FAmbientColor.g ) shr 8 ) * ( FAmbientIntensity / 100 );
  LightB := ( ( FAmbientColor.b ) shr 16 ) * ( FAmbientIntensity / 100 );
end;

procedure TAniMap.SetAmbientIntensity( Value : Integer );
begin
  FAmbientIntensity := Value;
  LightR := round( ( FAmbientColor.r ) * ( FAmbientIntensity / 100 ) );
  LightG := round( ( ( FAmbientColor.g ) shr 8 ) * ( FAmbientIntensity / 100 ) );
  LightB := round( ( ( FAmbientColor.b ) shr 16 ) * ( FAmbientIntensity / 100 ) );
end;

function TAniMap.GetZoneMemoryUsage( Index : integer ) : longint;
begin
  with TZone( Zones.items[ Index ] ) do
  begin
    result := ItemBitWidth * ItemBitHeight + TileBitWidth * TileBitHeight;
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

procedure TAniMap.SetTransparentColor( Color : TSDL_Color );
begin
  FTransparentColor := Color;
  // TODO : FColorMatch := FindColorMatch( Color );
  NeedColorMatch := false;
end;

function TAniMap.GetColorMatch : word;
begin
  if NeedColorMatch then
  begin
    // TODO  FColorMatch := FindColorMatch( FTransparentColor );
    NeedColorMatch := false;
  end;

  result := FColorMatch;
end;

function TAniMap.AddZone : Word;
var
  NewZone : TZone;
begin
  NewZone := TZone.Create( Self );
  NewZone.Index := Zones.Add( NewZone );
  Result := NewZone.Index;
end;

function TAniMap.AddLightZone : Word;
var
  NewZone : TZone;
begin
  NewZone := TLightZone.Create( Self );
  NewZone.Index := Zones.Add( NewZone );
  Result := NewZone.Index;
end;

function TAniMap.AddLight( Color : TSDL_Color; Intensity : Integer; Radius : Longint; Flicker : TFlicker; X, Y, Z : Longint ) : Word;
var
  NewZone : TLightZone;
  R2 : Longint;
begin
  Result := 0;
  if Radius <= 0 then
    Exit;
  NewZone := TLightZone.Create( Self );
  NewZone.Color.r := Color.r;
  NewZone.Color.g := Color.g;
  NewZone.Color.b := Color.b;
  NewZone.Intensity := Intensity;
  NewZone.Radius := Radius;
  NewZone.Flicker := Flicker;
  NewZone.State := 1;
  NewZone.X := X;
  NewZone.Y := Y;
  NewZone.Z := Z;
  NewZone.Index := Zones.Add( NewZone );
  NewZone.X1 := ( X - Radius ) div TileWidth;
  if ( Radius mod TileWidth ) = 0 then
    dec( NewZone.X1 );
  if NewZone.X1 < 0 then
    NewZone.X1 := 0;
  NewZone.X2 := ( X + Radius ) div TileWidth;
  if NewZone.X2 >= width then
    NewZone.X2 := width - 1;

  R2 := Radius div 2;
  NewZone.Y1 := ( Y - R2 ) div TileHeight;
  if ( R2 mod TileHeight ) = 0 then
    dec( NewZone.Y1 );
  if NewZone.Y1 < 0 then
    NewZone.Y1 := 0;
  NewZone.Y2 := ( Y + R2 ) div TileHeight;
  if NewZone.Y2 >= Height then
    NewZone.Y2 := Height - 1;

  case NewZone.Flicker of
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
    // TODO : GridLoc := GlobalLock( FMapData );
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
    // TODO : GlobalUnlock( FMapData );
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
    // TODO : GridLoc := GlobalLock( FMapData );
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
    // TODO : GlobalUnlock( FMapData );
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
  // TODO : BltFx : TDDBLTFX;
  // TODO : ddsd : TDDSurfaceDesc;
  C16 : word;
  p16 : ^word;
  TempSurface : PSDL_Surface;
  LightR1, LightG1, LightB1 : word;
  Pitch : longint;
  TimeCount : longword;
begin
  if not UseLighting then
    Exit;

  LightR1 := round( LightR );
  LightG1 := round( LightG );
  LightB1 := round( LightB );

  HalfWidth := TileWidth div 2;
  HalfHeight := TileHeight div 2;
  // TODO : GridBase := GlobalLock( FMapData );

  {TransparentColor.r := FTransparentColor.r;
  TransparentColor.g := FTransparentColor.g;
  TransparentColor.b := FTransparentColor.b;}
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
    // TODO : TimeCount := GetTickCount;
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

        // TODO : TempSurface := DDGetSurface( lpDD, FTileWidth, FTileHeight, TransparentColor, false );
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
                        // TODO : BltFx.dwSize := SizeOf( BltFx );
                        // TODO : BltFx.dwFillColor := ColorMatch;
                        // TODO : WrapperBlt( TempSurface, Rect( 0, 0, FTileWidth, FTileHeight ), nil, Rect( 0, 0, FTileWidth, FTileHeight ), DDBLT_COLORFILL + DDBLT_WAIT, BltFx );
                        if ( Index <> $FFFF ) then
                        begin
                          SrcX := ( Index div ZoneTile.TileMaxColumnIndex ) * TileWidth;
                          SrcY := ( Index mod ZoneTile.TileMaxColumnIndex ) * TileHeight + HalfHeight;
                          // TODO : WrapperBltFast( TempSurface, 0, 0, ZoneTile.FTileImages, Rect( SrcX, SrcY, SrcX + FTileWidth, SrcY + HalfHeight ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
                        end;
                        Index := p^.Tile[ 2 ];
                        if ( Index <> $FFFF ) then
                        begin
                          SrcX := ( Index div ZoneTile.TileMaxColumnIndex ) * TileWidth;
                          SrcY := ( Index mod ZoneTile.TileMaxColumnIndex ) * TileHeight;
                          ZoneTile := Zones.Items[ p^.Zone[ 2 ] ];
                          // TODO : WrapperBltFast( TempSurface, HalfWidth, 0, ZoneTile.FTileImages, Rect( SrcX, SrcY, SrcX + HalfWidth, SrcY + FTileHeight ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
                        end;
                        Index := p^.Tile[ 3 ];
                        if ( Index <> $FFFF ) then
                        begin
                          SrcX := ( Index div ZoneTile.TileMaxColumnIndex ) * TileWidth;
                          SrcY := ( Index mod ZoneTile.TileMaxColumnIndex ) * TileHeight;
                          ZoneTile := Zones.Items[ p^.Zone[ 3 ] ];
                          // TODO : WrapperBltFast( TempSurface, 0, HalfHeight, ZoneTile.FTileImages, Rect( SrcX, SrcY, SrcX + FTileWidth, SrcY + HalfHeight ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
                        end;
                        Index := p^.Tile[ 4 ];
                        if ( Index <> $FFFF ) then
                        begin
                          SrcX := ( Index div ZoneTile.TileMaxColumnIndex ) * TileWidth + HalfWidth;
                          SrcY := ( Index mod ZoneTile.TileMaxColumnIndex ) * TileHeight;
                          ZoneTile := Zones.Items[ p^.Zone[ 4 ] ];
                          // TODO : WrapperBltFast( TempSurface, 0, 0, ZoneTile.FTileImages, Rect( SrcX, SrcY, SrcX + HalfWidth, SrcY + FTileHeight ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
                        end;
                      end
                      else
                      begin
                        SrcX := ( Index div ZoneTile.TileMaxColumnIndex ) * TileWidth;
                        SrcY := ( Index mod ZoneTile.TileMaxColumnIndex ) * TileHeight;
                        // TODO : WrapperBltFast( TempSurface, 0, 0, ZoneTile.FTileImages, Rect( SrcX, SrcY, SrcX + FTileWidth, SrcY + FTileHeight ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
                      end;

                      //Render Tile
                      // TODO : ddsd.dwSize := SizeOf( ddsd );
                      // TODO : if TempSurface.Lock( nil, ddsd, DDLOCK_WAIT, 0 ) = DD_OK then
                      begin
                        try
                          for j := 0 to TileHeight - 1 do
                          begin
                            Y2 := j + Y * TileHeight;
                            // TODO : p16 := ddsd.lpSurface;
                            // TODO : inc( PChar( p16 ), j * ddsd.lPitch );
                            for i := 0 to TileWidth - 1 do
                            begin
                              X2 := i + X * TileWidth;
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
                                    RL := Test.Color.r;
                                    GL := Test.Color.g shr 8;
                                    BL := Test.Color.b shr 16;
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
                                  // TODO :
                                  {if PixelFormat = pf555 then
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
                                    end;}
                                end;
                              end;
                              inc( p16 );
                            end;
                          end;
                        finally
                          // TODO : TempSurface.UnLock( nil );
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
    // TODO : GlobalUnlock( FMapData );
    SortedZones.free;
    // TODO : TimeCount := GetTickCount - TimeCount;

    //Apply lighting to items
    // TODO : TimeCount := GetTickCount;
    ItemCount := 0;
    m := StripWidth div 2;
    if ( LightZones.Count > 0 ) then
    begin
      for State := 1 to MaxLightStates do
      begin
        i := FirstItem;
        while ( i <> 0 ) do
        begin
          ZoneTile := Zones.Items[ ItemList[ i ].Zone ];
          X := ItemList[ i ].X div TileWidth;
          Y := ItemList[ i ].Y div TileHeight;
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
            // TODO :
            {TempSurface := DDGetSurface( lpDD, FStripWidth, ItemList[ i ].Height, TransparentColor, false );
            WrapperBltFast( TempSurface, 0, 0, ZoneTile.FItemImages,
            Rect( ItemList[ i ].ImageX, ItemList[ i ].ImageY, ItemList[ i ].ImageX + FStripWidth,
            ItemList[ i ].ImageY + ItemList[ i ].Height ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
            ddsd.dwSize := SizeOf( ddsd );
            if TempSurface.Lock( nil, ddsd, DDLOCK_WAIT, 0 ) = DD_OK then
            begin
              try
                for X := 0 to FStripWidth - 1 do
                begin
                  X2 := ItemList[ i ].X + X;
                  if ( X = 0 ) then
                  begin
                    Slope := ItemList[ i ].Slope1;
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
                    Slope := ItemList[ i ].Slope2;
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
                  if ItemList[ i ].Vertical then
                    Y2 := ItemList[ i ].Y + Round( ( X - m ) * Slope ) //If vertical, Y2 remains constant
                  else //so only calculate once
                    Y2 := 0;
                  for Y := 0 to ItemList[ i ].Height - 1 do
                  begin
                    if not ItemList[ i ].Vertical then
                      Y2 := ItemList[ i ].Y - ItemList[ i ].VHeight + Y;
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
                            if ItemList[ i ].Vertical then
                              Z1 := sqr( Y2 - ItemList[ i ].Y + ItemList[ i ].VHeight - Y - ZoneZ - 1 )
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
            end;}
            if ( State = TLightZone( Zones.Items[ Zone ] ).States ) then
            begin
              TLightZone( Zones[ Zone ] ).AddStrip( TempSurface, ItemList[ i ].ImageX, ItemList[ i ].ImageY );
              ItemList[ i ].Zone := Zone;
            end
            else
            begin
              TLightZone( Zones[ Zone ] ).AddStrip( TempSurface, tX, tY );
            end;
            Inc( ItemCount );
            TempSurface := nil;
          end;
          i := ItemList[ i ].Next;
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
    // TODO : TimeCount := GetTickCount - TimeCount;

    //Construct item list for all light zones
    if LightZones.Count > 0 then
    begin
      i := FirstItem;
      while ( i <> 0 ) do
      begin
        ZoneTile := Zones.Items[ ItemList[ i ].Zone ];
        if ZoneTile is TLightZone then
        begin
          Test := TLightZone( ZoneTile );
          if not Assigned( Test.Items ) then
            Test.Items := TList.Create;
          Test.Items.Add( @ItemList[ i ] );
        end;
        i := ItemList[ i ].Next;
      end;
    end;
  end;

  if ( LightR <> $FF ) or ( LightG <> $FF ) or ( LightB <> $FF ) then
  begin
    //Render ambient color in all zones
    // TODO : TimeCount := GetTickCount;
    for Zone := 0 to Zones.Count - 1 do
    begin
      ZoneTile := Zones.Items[ Zone ];
      if not ( ZoneTile is TLightZone ) then
      begin
        //Do tiles first
        if Assigned( ZoneTile.TileImages ) then
        begin
          // TODO :
          {ddsd.dwSize := SizeOf( ddsd );
          if ZoneTile.TileImages.Lock( nil, ddsd, DDLOCK_WAIT, 0 ) = DD_OK then
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
          end;}
        end;

        //Do Items
        if Assigned( ZoneTile.ItemImages ) then
        begin
          // TODO :
          {ddsd.dwSize := SizeOf( ddsd );
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
          end;}
        end;
      end;
    end;
    // TODO : TimeCount := GetTickCount - TimeCount;
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
  // TODO : GridBase := GlobalLock( FMapDAta );
  Dx := X2 - X1;
  dy := Y2 - Y1;
  W := StripWidth div 2;
  H := StripHeight div 2;
  StripX := ( X2 div StripWidth ) * StripWidth + W;
  StripY := ( Y2 div StripHeight ) * StripHeight + H;
  R2 := sqr( W );

  if ( Dx <> 0 ) then
  begin
    if ( X1 < X2 ) then
    begin
      i1 := X1 div TileWidth + 1;
      i2 := X2 div TileWidth;
      Offset := 0;
    end
    else
    begin
      i1 := X2 div TileWidth + 1;
      i2 := X1 div TileWidth;
      Offset := -1;
    end;
    X := i1 * TileWidth;
    for i := i1 to i2 do
    begin
      R := ( X - X1 ) / Dx;
      Y := Y1 + Round( R * dy );
      j := Y div TileHeight;
      if ( Y mod TileHeight ) = 0 then
        if ( Y2 < Y1 ) then
          Dec( j );
      if j >= 0 then
      begin
        p := GridBase;
        Inc( p, j * FWidth + i + Offset );
        if ( p^.LineOfSightMask <> 0 ) then
        begin
          k := j * TileHeight;
          Mask := p^.LineOfSightMask;
          for s := 0 to 15 do
          begin
            if ( Mask and 1 ) = 1 then
            begin
              sX := X + ( s mod 4 ) * StripWidth + W + Offset * TileWidth;
              if ( Offset < 0 ) then
                Pass := ( sX > X2 )
              else
                Pass := ( sX <= X2 );
              if Pass then
              begin
                sY := k + ( 3 - ( s div 4 ) ) * StripHeight + H;
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
                        // TODO : GlobalUnlock( FMapData );
                        Result := False;
                        Exit;
                      end;
                      d2 := a2 * b2 / c2;
                    end;
                    if d2 <= r2 then
                    begin
                      // TODO : GlobalUnlock( FMapData );
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
      Inc( X, TileWidth );
    end;
  end;

  if ( dy <> 0 ) then
  begin
    if ( Y1 < Y2 ) then
    begin
      i1 := Y1 div TileHeight + 1;
      i2 := Y2 div TileHeight;
      Offset := 0;
    end
    else
    begin
      i1 := Y2 div TileHeight + 1;
      i2 := Y1 div TileHeight;
      Offset := -FWidth;
    end;
    Y := i1 * TileHeight;
    for i := i1 to i2 do
    begin
      R := ( Y - Y1 ) / dy;
      X := X1 + Round( R * Dx );
      j := X div TileWidth;
      if ( X mod TileWidth ) = 0 then
        if ( X2 < X1 ) then
          Dec( j );
      if ( j >= 0 ) then
      begin
        p := GridBase;
        Inc( p, i * FWidth + j + Offset );
        if ( p^.LineOfSightMask <> 0 ) then
        begin

          k := j * TileWidth;
          Mask := p^.LineOfSightMask;
          for s := 0 to 15 do
          begin
            if ( Mask and 1 ) = 1 then
            begin
              sY := Y + ( 3 - ( s div 4 ) ) * StripHeight + H;
              if ( Offset < 0 ) then
              begin
                Dec( sY, TileHeight );
                Pass := ( sY > Y2 );
              end
              else
              begin
                Pass := ( sY <= Y2 );
              end;
              if Pass then
              begin
                sX := k + ( s mod 4 ) * StripWidth + W;
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
                        // TODO : GlobalUnlock( FMapData );
                        Result := False;
                        Exit;
                      end;
                      d2 := a2 * b2 / c2;
                    end;
                    if d2 <= r2 then
                    begin
                      // TODO : GlobalUnlock( FMapData );
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
      Inc( Y, TileHeight );
    end;
  end;

  // TODO : GlobalUnlock( FMapData );
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
  // TODO : GridBase := GlobalLock( FMapDAta );
  Dx := X2 - X1;
  dy := Y2 - Y1;
  W := StripWidth div 2;
  H := StripHeight div 2;
  StripX := ( X2 div StripWidth ) * StripWidth + W;
  StripY := ( Y2 div StripHeight ) * StripHeight + H;
  R2 := sqr( W );

  if ( Dx <> 0 ) then
  begin
    if ( X1 < X2 ) then
    begin
      i1 := X1 div TileWidth + 1;
      i2 := X2 div TileWidth;
      Offset := 0;
    end
    else
    begin
      i1 := X2 div TileWidth + 1;
      i2 := X1 div TileWidth;
      Offset := -1;
    end;
    X := i1 * TileWidth;
    for i := i1 to i2 do
    begin
      R := ( X - X1 ) / Dx;
      Y := Y1 + Round( R * dy );
      j := Y div TileHeight;
      if ( Y mod TileHeight ) = 0 then
        if ( Y2 < Y1 ) then
          Dec( j );
      if j >= 0 then
      begin
        p := GridBase;
        Inc( p, j * FWidth + i + Offset );
        if ( p^.CollisionMask <> 0 ) then
        begin
          k := j * TileHeight;
          Mask := p^.CollisionMask;
          for s := 0 to 15 do
          begin
            if ( Mask and 1 ) = 1 then
            begin
              sX := X + ( s mod 4 ) * StripWidth + W + Offset * TileWidth;
              if ( Offset < 0 ) then
                Pass := ( sX > X2 )
              else
                Pass := ( sX <= X2 );
              if Pass then
              begin
                sY := k + ( 3 - ( s div 4 ) ) * StripHeight + H;
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
                        // TODO : GlobalUnlock( FMapData );
                        Result := False;
                        Exit;
                      end;
                      d2 := a2 * b2 / c2;
                    end;
                    if d2 <= r2 then
                    begin
                      // TODO : GlobalUnlock( FMapData );
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
      Inc( X, TileWidth );
    end;
  end;

  if ( dy <> 0 ) then
  begin
    if ( Y1 < Y2 ) then
    begin
      i1 := Y1 div TileHeight + 1;
      i2 := Y2 div TileHeight;
      Offset := 0;
    end
    else
    begin
      i1 := Y2 div TileHeight + 1;
      i2 := Y1 div TileHeight;
      Offset := -FWidth;
    end;
    Y := i1 * TileHeight;
    for i := i1 to i2 do
    begin
      R := ( Y - Y1 ) / dy;
      X := X1 + Round( R * Dx );
      j := X div TileWidth;
      if ( X mod TileWidth ) = 0 then
        if ( X2 < X1 ) then
          Dec( j );
      if ( j >= 0 ) then
      begin
        p := GridBase;
        Inc( p, i * FWidth + j + Offset );
        if ( p^.CollisionMask <> 0 ) then
        begin

          k := j * TileWidth;
          Mask := p^.CollisionMask;
          for s := 0 to 15 do
          begin
            if ( Mask and 1 ) = 1 then
            begin
              sY := Y + ( 3 - ( s div 4 ) ) * StripHeight + H;
              if ( Offset < 0 ) then
              begin
                Dec( sY, TileHeight );
                Pass := ( sY > Y2 );
              end
              else
              begin
                Pass := ( sY <= Y2 );
              end;
              if Pass then
              begin
                sX := k + ( s mod 4 ) * StripWidth + W;
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
                        // TODO : GlobalUnlock( FMapData );
                        Result := False;
                        Exit;
                      end;
                      d2 := a2 * b2 / c2;
                    end;
                    if d2 <= r2 then
                    begin
                      // TODO : GlobalUnlock( FMapData );
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
      Inc( Y, TileHeight );
    end;
  end;

  // TODO : GlobalUnlock( FMapData );
  Result := True;
end;

function TAniMap.DefineItem( Zone, Index : Word; Image : PSDL_Surface; const StripHeights, CollisionMasks, LineOfSightMasks, LightPoints : array of Word; Slope : Single; Visible, AutoTransparent, Vertical : Boolean ) : PItemInfo;
begin
  if ( Zone >= Zones.Count ) then
  begin
    Result := nil;
    Exit;
  end;
  Result := TZone( Zones.Items[ Zone ] ).DefineItem( Index, Image, StripHeights, CollisionMasks, LineOfSightMasks, LightPoints, FTransparentColor, Slope, Visible, AutoTransparent, Vertical );
end;

procedure TAniMap.DefineTile( Zone, Index : Word; Image : PSDL_Surface );
begin
  if ( Zone >= Zones.Count ) then
    Exit;
  TZone( Zones.Items[ Zone ] ).DefineTile( Index, Image, FTransparentColor );
end;

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
      // TODO : GridLoc := GlobalLock( FMapData );
      Inc( GridLoc, Loc );
      Result := pointer( GridLoc );
      // TODO : GlobalUnlock( FMapData );
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
      // TODO : GridLoc := GlobalLock( FMapData );
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
      // TODO : GlobalUnlock( FMapData );
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
    // TODO : GridLoc := GlobalLock( FMapData );
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
    // TODO : GlobalUnlock( FMapData );
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
      // TODO : GridLoc := GlobalLock( FMapData );
      Loc := X + Y * FWidth;
      p := GridLoc;
      Inc( p, Loc );
      p^.CollisionMask := CollisionMask;
      // TODO : GlobalUnlock( FMapData );
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
      // TODO : GridLoc := GlobalLock( FMapData );
      Loc := X + Y * FWidth;
      p := GridLoc;
      Inc( p, Loc );
      p^.LineOfSightMask := LineOfSightMask;
      // TODO : GlobalUnlock( FMapData );
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
      // TODO : GridLoc := GlobalLock( FMapData );
      Loc := X + Y * FWidth;
      p := GridLoc;
      Inc( p, Loc );
      p^.TriggerID := TriggerID;
      p^.TriggerMask := $FFFF;
      // TODO : GlobalUnlock( FMapData );
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
      // TODO : GridLoc := GlobalLock( FMapData );
      Loc := X + Y * FWidth;
      p := GridLoc;
      Inc( p, Loc );
      p^.TriggerID := TriggerID;
      p^.TriggerMask := TriggerMask;
      // TODO : GlobalUnlock( FMapData );
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
      // TODO : GridLoc := GlobalLock( FMapData );
      Loc := X + Y * FWidth;
      p := GridLoc;
      Inc( p, Loc );
      p^.FilterID := FilterID;
      p^.FilterMask := $FFFF;
      // TODO : GlobalUnlock( FMapData );
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
      // TODO : GridLoc := GlobalLock( FMapData );
      Loc := X + Y * FWidth;
      p := GridLoc;
      Inc( p, Loc );
      p^.FilterID := FilterID;
      p^.FilterMask := FilterMask;
      // TODO : GlobalUnlock( FMapData );
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

  if ( X < 0 ) and ( ( X mod StripWidth ) <> 0 ) then
    X := ( ( X div StripWidth ) - 1 ) * StripWidth
  else
    X := ( X div StripWidth ) * StripWidth;
  if ( Y < 0 ) and ( ( Y mod StripHeight ) <> 0 ) then
    Y := ( ( Y div StripHeight ) - 1 ) * StripHeight
  else
    Y := ( Y div StripHeight ) * StripHeight;
  // TODO : GridBase := GlobalLock( FMapData );

  ZoneItem := Zones.Items[ Zone ];
  with ZoneItem as TZone do
  begin

    //Apply Collision Mask
    if Collidable then
    begin
      if ( Item[ ItemIndex ].CollisionMasks <> 0 ) then
      begin
        // TODO : SrcMaskBase := GlobalLock( Item[ ItemIndex ].CollisionMasks );
        if Assigned( SrcMaskBase ) then
        begin
          Strips := Item[ ItemIndex ].Strips shl 2;
          Rows := Item[ ItemIndex ].Height div StripHeight;
          if ( ( Item[ ItemIndex ].Height mod StripHeight ) <> 0 ) then
            Inc( Rows );
          for j := 0 to Rows - 1 do
          begin
            k := Y - ( j + 1 ) * StripHeight;
            if k >= 0 then
            begin
              Y1 := k div TileHeight;
              if ( Y1 < FHeight ) then
              begin
                Y2 := ( Y div StripHeight ) - j - 1;
                for i := 0 to Strips - 1 do
                begin
                  SrcMask := SrcMaskBase;
                  Inc( SrcMask, ( j shr 2 ) * Item[ ItemIndex ].Strips + ( i shr 2 ) );
                  BitMask := 1 shl ( ( i mod 4 ) + ( ( j mod 4 ) ) * 4 );
                  if ( ( SrcMask^ and BitMask ) = BitMask ) then
                  begin
                    k := X + i * StripWidth;
                    if k >= 0 then
                    begin
                      X1 := k div TileWidth;
                      if ( X1 < FWidth ) then
                      begin
                        X2 := ( X div StripWidth ) + i;
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
        // TODO : GlobalUnlock( Item[ ItemIndex ].CollisionMasks );
      end;
    end;

    //Apply Line of Sight Mask
    if Collidable then
    begin
      if ( Item[ ItemIndex ].LineOfSightMasks <> 0 ) then
      begin
        // TODO : SrcMaskBase := GlobalLock( Item[ ItemIndex ].LineOfSightMasks );
        if Assigned( SrcMaskBase ) then
        begin
          Strips := Item[ ItemIndex ].Strips shl 2;
          Rows := Item[ ItemIndex ].Height div StripHeight;
          if ( ( Item[ ItemIndex ].Height mod StripHeight ) <> 0 ) then
            Inc( Rows );
          for j := 0 to Rows - 1 do
          begin
            k := Y - ( j + 1 ) * StripHeight;
            if k >= 0 then
            begin
              Y1 := k div TileHeight;
              if ( Y1 < FHeight ) then
              begin
                Y2 := ( Y div StripHeight ) - j - 1;
                for i := 0 to Strips - 1 do
                begin
                  SrcMask := SrcMaskBase;
                  Inc( SrcMask, ( j shr 2 ) * Item[ ItemIndex ].Strips + ( i shr 2 ) );
                  BitMask := 1 shl ( ( i mod 4 ) + ( ( j mod 4 ) ) * 4 );
                  if ( ( SrcMask^ and BitMask ) = BitMask ) then
                  begin
                    k := X + i * StripWidth;
                    if k >= 0 then
                    begin
                      X1 := k div TileWidth;
                      if ( X1 < FWidth ) then
                      begin
                        X2 := ( X div StripWidth ) + i;
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
        // TODO : GlobalUnlock( Item[ ItemIndex ].LineOfSightMasks );
      end;
    end;

    //Divide the image into strips and add each strip to the items list as a seperate item
    if LastItem >= ItemListSize then
    begin
      result := nil;
      exit;
    end;
    First := LastItem + 1;
    InitDelta := false;
    Result := @ItemList[ First ];
    RefDelta := 0;
    RefItem := First;
    for Strip := 1 to Item[ ItemIndex ].Strips do
    begin
      SrcY := Item[ ItemIndex ].Top + ( Strip - 1 ) * Item[ ItemIndex ].Height;
      for VStrip := 0 to 3 do
      begin
        SrcX := Item[ ItemIndex ].Left + VStrip * StripWidth;
        SrcW := ( Strip - 1 ) * TileWidth + VStrip * StripWidth;
        if ( SrcW < Item[ ItemIndex ].Width ) then
        begin
          Inc( LastItem );
          if LastItem <= ItemListSize then
          begin
            ItemList[ LastItem ].ImageX := SrcX;
            ItemList[ LastItem ].ImageY := SrcY;
            if ( SrcW + StripWidth > Item[ ItemIndex ].Width ) then
              ItemList[ LastItem ].Width := Item[ ItemIndex ].Width - SrcW
            else
              ItemList[ LastItem ].Width := StripWidth;
            ItemList[ LastItem ].Height := Item[ ItemIndex ].Height;
            ItemList[ LastItem ].Zone := Zone;
            ItemList[ LastItem ].FilterID := FilterID;
            ItemList[ LastItem ].XRayID := 0;
            ItemList[ LastItem ].Slope0 := Item[ ItemIndex ].Slope;
            ItemList[ LastItem ].Visible := Item[ ItemIndex ].Visible;
            ItemList[ LastItem ].AutoTransparent := Item[ ItemIndex ].AutoTransparent;
            ItemList[ LastItem ].Vertical := Item[ ItemIndex ].Vertical;
            ItemList[ LastItem ].Last := False;
            ItemList[ LastItem ].Next := 0;

            if ( Item[ ItemIndex ].StripHeights <> 0 ) then
            begin
              // TODO : StripData := GlobalLock( Item[ ItemIndex ].StripHeights );
              Inc( StripData, ( ( Strip - 1 ) shl 2 ) + VStrip );
              ItemList[ LastItem ].VHeight := StripData^ + Z;
              // TODO : GlobalUnlock( Item[ ItemIndex ].StripHeights );
            end;

            ItemList[ LastItem ].X := X + SrcW;
            ItemList[ LastItem ].Y := Y - ItemList[ LastItem ].Height + ItemList[ LastItem ].VHeight;

            if ItemList[ LastItem ].VHeight <> 0 then
            begin
              Delta := ItemList[ LastItem ].Y - round( SrcW * ItemList[ LastItem ].Slope0 );
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
              // TODO : StripData := GlobalLock( Item[ ItemIndex ].LightPoints );
              Inc( StripData, ( ( Strip - 1 ) shl 2 ) + VStrip );
              ItemList[ LastItem ].Slope1 := smallint( StripData^ ) / StripWidth;
              ItemList[ LastItem ].Slope2 := ItemList[ LastItem ].Slope1;
              // TODO : GlobalUnlock( Item[ ItemIndex ].LightPoints );
            end;


            //Keep Items sorted by VY     //Replaced by BuildRowUpdateInfo
           { if (FirstItem = 0) then begin
              FirstItem := LastItem;
              ItemList[LastItem].Next := 0;
            end
            else begin
              i := FirstItem;
              j := 0;
              while (i <> 0) do begin
                if (ItemList[i].Y >= ItemList[LastItem].Y) then begin
                  if (j = 0) then
                    FirstItem := LastItem
                  else
                    ItemList[j].Next := LastItem;
                  ItemList[LastItem].Next := i;
                  Break;
                end;
                j := i;
                i := ItemList[i].Next;
              end;
              if (i = 0) then begin
                ItemList[j].Next := LastItem;
                ItemList[LastItem].Next := 0;
              end;
            end;  }
          end;
        end;
      end;
    end;

    for i := First to LastItem do
    begin
      ItemList[ i ].RefItem := RefItem;
    end;

    ItemList[ LastItem ].Last := True;
  end;
  // TODO : GlobalUnlock( FMapData );
end;

procedure TAniMap.SetTileSize( Size : Word );
begin
  FTileSize := Size;
  TileWidth := FTileSize * 16;
  TileHeight := FTileSize * 8;
  StripWidth := TileWidth shr 2;
  StripHeight := TileHeight shr 2;
  BitWidth := FWidth * TileWidth;
  BitHeight := FHeight * TileHeight;
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
    MinY := ItemList[ 1 ].Y;
    MaxY := MinY;
    for i := 2 to LastItem do
    begin
      if ItemList[ i ].Y < MinY then
        MinY := ItemList[ i ].Y
      else if ItemList[ i ].Y > MaxY then
        MaxY := ItemList[ i ].Y;
    end;
    MemSize := ( MaxY - MinY + 1 ) * sizeof( longint );
    GetMem( pBase, MemSize );
    try
      FillChar( pBase, MemSize, 0 );
      for i := LastItem downto 1 do
      begin
        Y := ItemList[ i ].Y - MinY;
        p := pBase;
        inc( p, Y );
        ItemList[ i ].Next := p^;
        p^ := i;
      end;

      p := pBase;
      FirstItem := p^; //we know the first one is valid
      j := FirstItem;
      while ItemList[ j ].Next <> 0 do
      begin
        j := ItemList[ j ].Next;
      end;
      for i := 1 to ( MaxY - MinY ) do
      begin
        inc( p );
        if p^ <> 0 then
        begin
          ItemList[ j ].Next := p^;
          j := p^;
          while ItemList[ j ].Next <> 0 do
          begin
            j := ItemList[ j ].Next;
          end;
        end;
      end;
    finally
      FreeMem( pBase );
    end;
  end;

end;

end.

 