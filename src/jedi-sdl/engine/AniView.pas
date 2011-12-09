unit AniView;

interface

uses
  Classes,
  sdl,
  sdlwindow,
  SiegeInterfaces,
  AniMap,
  AStar,
  CustomAniFigure,
  SiegeTypes,
  Zone;

type
  TAniView = class;

  TExtMouseEvent = procedure( aSender : TAniView; aButton : Integer;
    aShift : TSDLMod; aMousePos : TPoint; aMapX, aMapY : Integer ) of object;

  TExtMouseMoveEvent = procedure( aSender : TAniView;
    Shift : TSDLMod; aMousePos : TPoint; aMapX, aMapY : Integer ) of object;

  TAniView = class( TSimpleSoAInterface )
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
    //TODO : Timer : TAniTimer;
    FInterval : Word;
    FActive : Boolean;
    FMap : TAniMap;
    FFRameCount : LongWord;
    lpDDSMap : PSDL_Surface;
    Work : PSDL_Surface;
    XRayImage : PSDL_Surface;
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
    FAStarFigure : TCustomAniFigure;
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
    procedure CopyTile( Dest : PSDL_Surface; GridLoc : Pointer; X, Y, Layer : Integer; ClipRect : PRect );
    procedure DrawItems;
    procedure DrawItemsClip( X1, X2, Y1, Y2 : Longint );
    procedure SetInterval( PInterval : Word );
    procedure SetActive( VActive : Boolean );
    procedure SetItemMask( Mask : Longint );
    procedure SetShowRepaint( const Value : Boolean );
    procedure BuildRowUpdateInfo;
    procedure DrawFigure( Figure : TCustomAniFigure );
    procedure FRefreshMap;
    procedure RefreshRegion( X, Y, W, H : Longint );
    procedure RefreshLight( Zone : TLightZone );
    function CanMove( SrcX, SrcY, DestX, DestY : Smallint ) : Boolean;
    procedure GetPath( Figure : TCustomAniFigure );
    procedure SetAutoTransparentMask( const Value : PSDL_Surface );
  protected
    procedure Show( Value : Boolean );
    procedure Paint;
  public
    //TODO : TempDC : HDC; //Used for swapping bitmaps for blting
    //TODO : OldTempBitmap : HBITMAP; //Original Bitmap in TempDC
    OffsetX : Longint;
    OffsetY : Longint;
    CenterX : Longint;
    CenterY : Longint;
    FRightX : longint;
    FBottomY : longint;
    KeyFigure : TCustomAniFigure;
    MouseOverFigure : TCustomAniFigure;
    MouseOverHLFigure : TCustomAniFigure;
    MouseOverTile : PGridInfo;
    MousePosition : TPoint;
    ForceRefresh : Boolean;
    constructor Create( const aMainWindow : TSDL2DWindow );
    destructor Destroy; override;
    //TODO : procedure InitDX( Handle : HWND; ResW, ResH, BPP : Integer );
    //TODO : procedure CloseDX;
    procedure ComputeLight( Figure : TCustomAniFigure );
    property Active : Boolean read FActive write SetActive;
    procedure MouseDown( Button : Integer; Shift : TSDLMod; MousePos : TPoint ); override;
    procedure MouseUp( Button : Integer; Shift : TSDLMod; MousePos : TPoint ); override;
    procedure MouseMove( Shift : TSDLMod; CurrentPos : TPoint; RelativePos : TPoint ); override;
    procedure CenterView( X, Y : Longint );
    procedure DrawFrame;
    procedure RefreshMap;
    procedure FreeResources;
    procedure AddFigure( Figure : TCustomAniFigure );
    procedure ReplaceFigure( i : integer; Figure : TCustomAniFigure );
    procedure MoveFigure( Figure : TCustomAniFigure );
    procedure TransFigure( Figure : TCustomAniFigure );
    procedure WaitForNextFrame;
    procedure PrecreateMap( W, H : longint );
    procedure UncreateMap;
    function FindPath( Figure : TCustomAniFigure; X2, Y2, Deviance : Longint; var Path : HGLOBAL ) : integer;
    function FindInRadius( X, Y : Longint; Radius : Single ) : TList;
    function LineOfSight( X1, Y1, X2, Y2 : Longint ) : Boolean;
    function LineOfCollision( X1, Y1, X2, Y2 : Longint ) : Boolean;
    function ClearShot( SrcX, Srcy, DstX, DstY, Radius : longint; UseLineOfSight : boolean ) : boolean;
    procedure DisableFigure( Figure : TCustomAniFigure );
    property Map : TAniMap read FMap write SetMap;
    property ItemMask : Longint read FItemMask write SetItemMask;
    property Drawing : Boolean read FDrawing;
    property FRameCount : LongWord read FFRameCount;
    property RightX : longint read FRightX;
    property BottomY : longint read FBottomY;
  published
    property Interval : Word read FInterval write SetInterval default 50;
    property FigureList : TList read FFigureList;
    property OnMouseDown : TExtMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseUp : TExtMouseEvent read FOnMouseUp write FOnMouseUp;
    property OnMouseMove : TExtMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnBeforeDisplay : TNotifyEvent read FOnBeforeDisplay write FOnBeforeDisplay;
    property OnAfterDisplay : TNotifyEvent read FOnAfterDisplay write FOnAfterDisplay;
    property LMouseButton : boolean read FLMouseButton write FLMousebutton;
    property OnWaitForTimer : TNotifyEvent read FOnWaitForTimer write FOnWaitForTimer;
    property ShowRepaint : boolean read FShowRepaint write SetShowRepaint;
  end;

implementation

uses
  logger,
  AniFigure;

//------------------------------------------------------------------------------
//AniView Component

constructor TAniView.Create( const aMainWindow : TSDL2DWindow );
begin
  inherited;

  FInterval := 50;
  ShowRepaint := false;

    // TODO Built-in Timer;
{    Timer := TAniTimer.Create(AOwner);
    Timer.Interval := FInterval;
    Timer.OnTimer := nil;               //Must set active property
    Timer.resolution := 1;
    Timer.TimerPriority := tpNormal;  }

    //TempDC
    // TODO :
    {OldTempBitmap := SelectObject( TempDC, CreateCompatibleBitmap( TempDC, 1, 1 ) );
    DeleteObject( SelectObject( TempDC, OldTempBitmap ) );
    ReleaseDC( 0, ScreenDC );}

    //Initailize A* object
  FAStar := TAStar.Create;
  FAStar.CanMove := CanMove;

  FFigureList := TList.Create;
end;

destructor TAniView.Destroy;
begin
  FigureList.Free;
  // TODO
  {if Assigned( Timer ) then
    Timer.Destroy;
  if ( MapColumns <> 0 ) then
    GlobalFree( MapColumns );
  MapColumns := 0;
  if ( MapRows <> 0 ) then
    GlobalFree( MapRows );
  MapRows := 0;
  if ( TempDC <> 0 ) then
    DeleteDC( TempDC );
  TempDC := 0;}
  FAStar.Free;
  FAStar := nil;
  inherited Destroy;
end;

{procedure TAniView.InitDX( Handle : HWND; ResW, ResH, BPP : Integer );
var
  ddsd : TDDSurfaceDesc;
  Caps : TDDSCaps;
  BltFx : TDDBLTFX;
  C : longint;
begin
  if DXMode then
    Exit;

  ResWidth := ResW;

  ResHeight := ResH;

  DirectDrawCreate( nil, lpDD, nil );

  lpDD.SetCooperativeLevel( Handle, DDSCL_EXCLUSIVE or DDSCL_FULLSCREEN );

  lpDD.SetDisplayMode( ResW, ResH, BPP );

  ddsd.dwSize := SizeOf( ddsd );

  ddsd.dwFlags := DDSD_CAPS or DDSD_BACKBUFFERCOUNT;
  ddsd.dwBackBufferCount := 1;

  ddsd.ddsCaps.dwCaps := DDSCAPS_COMPLEX + DDSCAPS_FLIP + DDSCAPS_PRIMARYSURFACE;

  if ( lpdd.CreateSurface( ddsd, lpDDSFront, nil ) = DD_OK ) then
  begin

    Caps.dwCaps := DDSCAPS_BACKBUFFER;

    lpDDSFront.GetAttachedSurface( Caps, lpDDSBack );

  end;
  BltFx.dwSize := SizeOf( BltFx );

  BltFx.dwFillColor := 0;

  // TODO WrapperBlt( lpDDSBack, Rect( 0, 0, ResWidth, ResHeight ), nil, Rect( 0, 0, ResWidth, ResHeight ), DDBLT_COLORFILL + DDBLT_WAIT, BltFx );

  lpDDSFront.Flip( nil, DDFLIP_WAIT );

  BltFx.dwSize := SizeOf( BltFx );

  BltFx.dwFillColor := 0;

  // TODO WrapperBlt( lpDDSBack, Rect( 0, 0, ResWidth, ResHeight ), nil, Rect( 0, 0, ResWidth, ResHeight ), DDBLT_COLORFILL + DDBLT_WAIT, BltFx );

  C := FindColorMatch( $FFFFFF );

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
  DXMode := False;
  lpdd.RestoreDisplayMode;
  lpDDSBack := nil;
  lpDDSFront := nil;
  lpDD := nil;
end;}

procedure TAniView.FreeResources;
var
  i : Integer;
begin
  if MapColumns <> 0 then
    //GlobalFree( MapColumns );
  MapColumns := 0;
  if MapRows <> 0 then
    //GlobalFree( MapRows );
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
    TAniFigure( FigureList.Items[ i ] ).Free;
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
  // TODO FRightX := FWidth;
  // TODO FBottomY := FHeight;

  MaxHeightTop := 0;
  MaxHeightBottom := 0;
end;

procedure TAniView.Paint;
begin
  //inherited Paint;

  //PatBlt( Canvas.Handle, 0, 0, Width, Height, BLACKNESS );
end;

procedure TAniView.Show( Value : Boolean );
begin
  //Invalidate;
end;

procedure TAniView.SetMap( const VMap : TAniMap );
begin
  FMap := VMap;
  InitMap;
end;

procedure TAniView.AddFigure( Figure : TCustomAniFigure );
begin
  if ( Figure = nil ) then
    Exit;
  FigureList.Add( Figure );
  TAniFigure( Figure ).View := Self;
  if ( TAniFigure( Figure ).Radius > FMaxCollisionRadius ) then
    FMaxCollisionRadius := TAniFigure( Figure ).Radius;
  if ( TAniFigure( Figure ).Height - TAniFigure( Figure ).CenterY > MaxHeightTop ) then
    MaxHeightTop := TAniFigure( Figure ).Height - TAniFigure( Figure ).CenterY;
  if ( TAniFigure( Figure ).CenterY > MaxHeightBottom ) then
    MaxHeightBottom := TAniFigure( Figure ).CenterY;
end;

procedure TAniView.ReplaceFigure( i : integer; Figure : TCustomAniFigure );
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
  TAniFigure( Figure ).View := Self;
  if ( TAniFigure( Figure ).Radius > FMaxCollisionRadius ) then
    FMaxCollisionRadius := TAniFigure( Figure ).Radius;
  if ( TAniFigure( Figure ).Height - TAniFigure( Figure ).CenterY > MaxHeightTop ) then
    MaxHeightTop := TAniFigure( Figure ).Height - TAniFigure( Figure ).CenterY;
  if ( TAniFigure( Figure ).CenterY > MaxHeightBottom ) then
    MaxHeightBottom := TAniFigure( Figure ).CenterY;
end;


procedure TAniView.FDrawFrame( Sender : TObject );
begin
  if not FActive then
    Exit;
  // TODO Timer.OnTimer := nil;
  DrawFrame;
  // TODO Timer.OnTimer := FDrawFrame;
end;

procedure TAniView.WaitForNextFrame;
var
  NextTickCount : longword;
begin
  NextTickCount := LastTickCount + Interval;
  while SDL_GetTicks < NextTickCount do
  begin
    if assigned( OnWaitFortimer ) then
      OnWaitFortimer( self );
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
  //DC : HDC;
  SrcX1, SrcY1, SrcX2, SrcY2 : Integer;
  DstX1, DstY1, DstX2, DstY2 : Integer;
  //BltFx : TDDBLTFX;
  //ddck : TDDCOLORKEY;
begin
  if FDrawing then
    exit;
  LastTickCount := SDL_GetTicks;
  FDrawing := True;

  //Calculate postion of key figure
  if Assigned( KeyFigure ) then
  begin
    if TAniFigure( KeyFigure ).NeedPath then
      GetPath( TAniFigure( KeyFigure ) );
    if TAniFigure( KeyFigure ).Moved then
      TransFigure( TAniFigure( KeyFigure ) );
    if TAniFigure( KeyFigure ).Moving then
      MoveFigure( TAniFigure( KeyFigure ) );
    CenterX := TAniFigure( KeyFigure ).FX;
    CenterY := TAniFigure( KeyFigure ).FY;
  end;
  // TODO OffsetX := CenterX - Width div 2;
  // TODO OffsetY := CenterY - Height div 2;
  // TODO W := FMap.BitWidth - Width - FMap.TileWidth;
  // TODO H := FMap.BitHeight - Height - FMap.TileHeight;
  if ( OffsetX < FMap.TileWidth ) then
    OffsetX := FMap.TileWidth;
  if ( OffsetY < FMap.TileHeight ) then
    OffsetY := FMap.TileHeight;
  if ( OffsetX > W ) then
    OffsetX := W;
  if ( OffsetY > H ) then
    OffsetY := H;
  // TODO FRightX := OffsetX + Width;
  // TODO FBottomY := OffsetY + Height;

  XRayActive := False;
  XRayX1 := 0;
  XRayY1 := 0;
  XRayY2 := 0;
  XRayX1Fix := 0;
  XRayX2Fix := 0;

  if Assigned( FMap ) then
  begin
    //RowBase := GlobalLock( MapRows );

    //Move map accordingly
    if ForceRefresh then
    begin
      RefreshMap;
      ForceRefresh := False;
    end
    else
      UpdateMap;

    //Copy map to frame buffer
    // TODO// TODO WrapperBltFast( lpDDSBack, Left, Top, lpDDSMap, Rect( MapOffsetX, MapOffsetY, MapOffsetX + Width, MapOffsetY + Height ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );

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

    //Get mouse position
    SDL_GetMouseState( MousePosition.x, MousePosition.y );
    // TODO MousePosition := ScreenToClient( MousePosition );
    MouseOverFigure := nil;
    MouseOverHLFigure := nil;
    MouseOverTile := nil;

   // TODO  if ( MousePosition.X >= Left ) and ( MousePosition.X < Left + Width ) and ( MousePosition.Y >= Top ) and ( MousePosition.Y < Top + Height ) then
    begin
      X := ( MousePosition.X + OffsetX ) div FMap.TileWidth;
      Y := ( MousePosition.Y + OffsetY ) div FMap.TileHeight;
      if ( X >= 0 ) and ( Y >= 0 ) and ( X < FMap.Width ) and ( Y < Fmap.Height ) then
      begin
        Loc := X + Y * FMap.Width;
        // TODO GridLoc := GlobalLock( FMap.FMapData );
        Inc( GridLoc, Loc );
        MouseOverTile := pointer( GridLoc );
        // TODO GlobalUnlock( Fmap.FMapData );
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

    if Assigned( KeyFigure )
    and TAniFigure( KeyFigure ).AutoTransparent
    and Assigned( XRayImage ) then
    begin
      XRayX1 := TAniFigure( KeyFigure ).FX - XRayWidth div 2;
      XRayY1 := TAniFigure( KeyFigure ).FY - TAniFigure( KeyFigure ).CenterY;
      XRayX2 := XRayX1 + XRayWidth;
      XRayY2 := XRayY1 + XRayHeight;
      XRayX1Fix := FMap.StripWidth * ( XRayX1 div FMap.StripWidth );
      XRayX2Fix := FMap.StripWidth * ( XRayX2 div FMap.StripWidth + 1 );
      XRayActive := True;
      RefreshRegion( XRayX1, XRayY1, XRayWidth, XRayHeight );
    end;

    // TODO BltFx.dwSize := SizeOf( BltFx );
    // TODO BltFx.dwFillColor := FMap.FColorMatch;
    // TODO WrapperBlt( Work, Rect( WorkWidth div 2, 0, WorkWidth div 2 + XRayWidth + FMap.StripWidth, XRayHeight ), nil, Rect( 320, 0, 320 + XRayWidth + FMap.StripWidth, XRayHeight ), DDBLT_COLORFILL + DDBLT_WAIT, BltFx );

    //Walk through grid and draw on frame buffer when necessary
    MaxRow := PixelHeight;
    // TODO Y := OffsetY + Height + MaxHeightBottom;
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

    // TODO Y := OffsetY + Height;
    if Y > PixelHeight then
      Y := PixelHeight;
    RowData := RowBase;
    Inc( RowData, Y );
    Y2 := RowData^.OverlapRow;
    if MaxRow > Y2 then
      Y2 := MaxRow;

    // TODO ColumnBase := GlobalLock( MapColumns );
    ColumnData := ColumnBase;
    FillChar( ColumnData, FMapColumnCount * sizeof( MapColumnHeaderInfo ), 0 );

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
          TAniFigure( FigureList.Items[ i ] ).OnScreen := false;
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
            else if ( TAniFigure( FigureList.Items[ i ] ).FY > TAniFigure( MouseOverFigure ).FY ) then
              MouseOverFigure := TAniFigure( FigureList.Items[ i ] );
            if ( TAniFigure( FigureList.Items[ i ] ).Highlightable ) then
            begin
              if not Assigned( MouseOverHLFigure ) then
                MouseOverHLFigure := TAniFigure( FigureList.Items[ i ] )
              else if ( TAniFigure( FigureList.Items[ i ] ).FY > TAniFigure( MouseOverHLFigure ).FY ) then
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
          while ( FMap.ItemList[ ItemIndex ].Y <= i ) or ( i = PixelHeight ) do
          begin
            if FMap.ItemList[ ItemIndex ].Visible then
            begin
              if ( FMap.ItemList[ ItemIndex ].FilterID = 0 ) or ( ( FMap.ItemList[ ItemIndex ].FilterID < 0 ) or
                ( FMap.ItemList[ ItemIndex ].FilterID = FItemMask ) ) and
                ( FMap.ItemList[ ItemIndex ].FilterID <> -FItemMask ) then
              begin
                StripX := ( FMap.ItemList[ ItemIndex ].X - MapX * FMap.TileWidth ) div FMap.StripWidth;
                ColumnData := ColumnBase;
                Y := FMap.ItemList[ ItemIndex ].Y - FMap.ItemList[ ItemIndex ].VHeight;
                j := Y + FMap.ItemList[ ItemIndex ].VHeight;
                if ( StripX >= 0 ) and ( StripX < FMapColumnCount ) then
                begin
                  InXRayZone := XRayActive and ( FMap.ItemList[ ItemIndex ].X >= XRayX1Fix ) and
                    ( FMap.ItemList[ ItemIndex ].X < XRayX2Fix ) and ( j > XRayY1 ) and ( Y <= XRayY2 );
                  Inc( ColumnData, StripX );
                  HidesCharacter := ColumnData^.Active and ( Y <= ColumnData^.BaseLine );
                  if InXRayZone or HidesCharacter then
                  begin
                    X := FMap.ItemList[ ItemIndex ].X - OffsetX;
                    ApplyXRay := InXRayZone and FMap.ItemList[ ItemIndex ].AutoTransparent and
                      ( ( FMap.ItemList[ ItemIndex ].XRayID = 0 ) or ( FMap.ItemList[ ItemIndex ].XRayID = FItemMask ) );
                    if ApplyXRay then
                    begin
                      if FMap.ItemList[ ItemIndex ].VHeight = 0 then
                        ApplyXRay := false
                      else
                      begin
                        RefItem := @FMap.ItemList[ FMap.ItemList[ ItemIndex ].RefItem ];
                        j := Round( RefItem.Slope0 * ( TAniFigure( KeyFigure ).FX - RefItem.X ) ) + RefItem.Y;
                        ApplyXRay := ( j > TAniFigure( KeyFigure ).FY );
                      end;
                    end;
                    Dec( Y, OffsetY );
                    ZoneItem := TZone( FMap.Zones.Items[ FMap.ItemList[ ItemIndex ].Zone ] );

                    SrcX1 := FMap.ItemList[ ItemIndex ].ImageX;
                    DstX1 := X;
                    SrcX2 := FMap.ItemList[ ItemIndex ].ImageX + FMap.ItemList[ ItemIndex ].Width;
                    DstX2 := X + FMap.ItemList[ ItemIndex ].Width;
                    // TODO Clip( 0, width, DstX1, DstX2, SrcX1, SrcX2 );

                    SrcY1 := FMap.ItemList[ ItemIndex ].ImageY;
                    DstY1 := Y;
                    SrcY2 := FMap.ItemList[ ItemIndex ].ImageY + FMap.ItemList[ ItemIndex ].Height;
                    DstY2 := Y + FMap.ItemList[ ItemIndex ].Height;
                    // TODO Clip( 0, Height, DstY1, DstY2, SrcY1, SrcY2 );

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
                          // TODO Clip( ColumnData^.Top - OffsetY, ColumnData^.BaseLine - OffsetY, DstY1, DstY2, SrcY1, SrcY2 );
                          // TODO// TODO WrapperBltFast( lpDDSBack, left + DstX1, top + XRayY2 - OffsetY, ZoneItem.FItemImages,
                          // TODO   Rect( SrcX1, SrcY2 - ( DstY2 + OffsetY - XRayY2 ), SrcX2, SrcY2 ),
                          // TODO   DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
                        end;
                        // TODO Clip( ColumnData^.Top - OffsetY, XRayY2, DstY1, DstY2, SrcY1, SrcY2 ); //????
                        H := XRayY1 - ( DstY1 + OffsetY );
                        if ( H > 0 ) then
                        begin
                          Y := SrcY2 - SrcY1;
                          if H > Y then
                            H := Y;
                          // TODO// TODO WrapperBltFast( lpDDSBack, left + DstX1, top + DstY1, ZoneItem.FItemImages,
                            // TODO Rect( SrcX1, SrcY1, SrcX2, SrcY1 + H ),
                            // TODO DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
                        end;
                      end;
                      // TODO Clip( XRayY1 - OffsetY, XRayY2 - OffsetY, DstY1, DstY2, SrcY1, SrcY2 );
                      //Blt to work at 0,0 with no color key.
                      //Then blt the appropriate segment of Xray on to work with black color key.
                      //Then blt the result onto the back buffer using SRCCOLORKEY
                      // TODO// TODO WrapperBltFast( Work, 0, 0, ZoneItem.FItemImages,
                        // TODO Rect( SrcX1, SrcY1, SrcX2, SrcY2 ),
                        // TODO DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );

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

                      // TODO// TODO WrapperBltFast( Work, j, 0, XRayImage,
                        // TODO Rect( X, Y, W, H ),
                        // TODO DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

                      // TODO ddck.dwColorSpaceLowValue := FMap.FColorMatch;
                      // TODO ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
                      // TODO Work.SetColorKey( DDCKEY_SRCBLT, {$IFNDEF DX5}@{$ENDIF}ddck );
                      // TODO// TODO WrapperBltFast( lpDDSBack, Left + DstX1, Top + DstY1, Work, Rect( 0, 0, SrcX2 - SrcX1, SrcY2 - SrcY1 ),
                        // TODO DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
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
                          // TODO Clip( Y - OffsetY, H - OffsetY, DstY1, DstY2, SrcY1, SrcY2 );
                        end
                        else
                          // TODO Clip( ColumnData^.Top - OffsetY, ColumnData^.BaseLine - OffsetY, DstY1, DstY2, SrcY1, SrcY2 );
                      end
                      else
                        // TODO Clip( XRayY1 - OffsetY, XRayY2 - OffsetY, DstY1, DstY2, SrcY1, SrcY2 );
                      // TODO// TODO WrapperBltFast( lpDDSBack, Left + DstX1, Top + DstY1, ZoneItem.FItemImages, Rect( SrcX1, SrcY1, SrcX2, SrcY2 ), RepaintCode ); //****
                    end;
                  end;
                end;
              end;
            end;
            ItemIndex := FMap.ItemList[ ItemIndex ].Next;
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
            if NextFigure.Enabled then
            begin
              if NextFigure.Visible then
              begin
                if ( NextFigure = KeyFigure ) and TAniFigure( NextFigure ).AutoTransparent and Assigned( XRayImage ) then
                begin
                  StripX := ( XRayX1Fix - MapX * FMap.TileWidth ) div FMap.StripWidth;
                  StripW := ( XRayX2Fix - XRayX1Fix ) div FMap.StripWidth + 1;
                  T := XRayY1;
                  Y := XRayY2;
                end
                else
                begin
                  StripX := ( NextFigure.FX - NextFigure.CenterX - MapX * FMap.TileWidth ) div FMap.StripWidth;
                  StripW := NextFigure.Width div FMap.StripWidth + 1;
                  if ( ( NextFigure.Width mod FMap.StripWidth ) <> 0 ) then
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

    // TODO SelectObject( TempDC, OldTempBitmap );
    // TODO GlobalUnlock( MapColumns );
    // TODO GlobalUnlock( MapRows );
  end;

  if Assigned( KeyFigure )
  and ( TAniFigure( KeyFigure ).AutoTransparent )
  and Assigned( XRayImage ) then
  begin

    {    // TODO WrapperBltFast( Work, 320+KeyFigure.FX-XRayWidth div 2-OffsetX-KeyX,0,
          XRayImage,Rect(0,0,XRayWidth,XRayHeight),DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
        ddck.dwColorSpaceLowValue:=FMap.FColorMatch;
        ddck.dwColorSpaceHighValue:=ddck.dwColorSpaceLowValue;
        Work.SetColorKey(DDCKEY_SRCBLT,ddck);
        // TODO WrapperBltFast( lpDDSBack, KeyX+left,KeyY+top,Work,Rect(WorkWidth div 2,0,WorkWidth div 2+XRayWidth+FMap.StripWidth,XRayHeight),
          DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);   }
  end;

  //Display frame buffer
  if Assigned( FOnBeforeDisplay ) then
    FOnBeforeDisplay( Self );

  // TODO lpDDSFront.Flip( nil, DDFLIP_WAIT );

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
  SrcX1, SrcY1 : Longint;
  SrcX2, SrcY2 : Longint;
begin
  NewMapX := OffsetX div FMap.TileWidth;
  if ( OffsetX < 0 ) then
    if ( ( OffsetX mod FMap.TileWidth ) <> 0 ) then
      Dec( NewMapX );
  NewMapY := OffsetY div FMap.TileHeight;
  if ( OffsetY < 0 ) then
    if ( ( OffsetY mod FMap.TileHeight ) <> 0 ) then
      Dec( NewMapY );
  if ( NewMapX <> MapX ) or ( NewMapY <> MapY ) then
  begin
    i := MapX - NewMapX;
    j := MapY - NewMapY;
    if ( i > 0 ) then
    begin
      SrcX1 := 0;
      X := i * FMap.TileWidth;
      SrcX2 := MapBitWidth - X;
    end
    else if ( i < 0 ) then
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

    if ( j > 0 ) then
    begin
      SrcY1 := 0;
      Y := j * FMap.TileHeight;
      SrcY2 := MapBitHeight - Y;
    end
    else if ( j < 0 ) then
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

    // TODO// TODO WrapperBltFast( lpDDSMap, X, Y, lpDDSMap, Rect( SrcX1, SrcY1, SrcX2, SrcY2 ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );

    //Draw empty Tiles and objects on mapbuffer
    if ( Abs( NewMapX - MapX ) > MapWidth ) or ( Abs( NewMapY - MapY ) > MapHeight ) then
    begin
      MapX := NewMapX;
      MapY := NewMapY;
      FRefreshMap;
    end
    else
    begin
      // TODO MapBase := GlobalLock( FMap.FMapData );
      if ( NewMapX < MapX ) then
      begin
        for j := 1 to MapHeight do
        begin
          Y := NewMapY + j - 1;
          if ( Y >= 0 ) and ( Y < FMap.Height ) then
          begin
            p := MapBase;
            Inc( p, NewMapX + Y * FMap.Width );
            for i := 1 to MapX - NewMapX do
            begin
              X := NewMapX + i - 1;
              if ( X >= 0 ) and ( X < FMap.Width ) then
              begin
                DrawTile( p, i, j, 0 );
                DrawTile( p, i, j, 1 );
              end;
              Inc( p );
            end;
          end;
        end;
        ClipX1 := NewMapX * FMap.TileWidth;
        ClipX2 := MapX * FMap.TileWidth;
        MinX := MapX - NewMapX + 1;
        MaxX := MapWidth;
      end
      else if ( NewMapX > MapX ) then
      begin
        for j := 1 to MapHeight do
        begin
          Y := NewMapY + j - 1;
          if ( Y >= 0 ) and ( Y < FMap.Height ) then
          begin
            p := MapBase;
            Inc( p, MapWidth + MapX + Y * FMap.Width );
            for i := MapWidth - NewMapX + MapX + 1 to MapWidth do
            begin
              X := NewMapX + i - 1;
              if ( X >= 0 ) and ( X < FMap.Width ) then
              begin
                DrawTile( p, i, j, 0 );
                DrawTile( p, i, j, 1 );
              end;
              Inc( p );
            end;
          end;
        end;
        ClipX1 := ( MapX + MapWidth ) * FMap.TileWidth;
        ClipX2 := ( NewMapX + MapWidth ) * FMap.TileWidth;
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
          if ( Y >= 0 ) and ( Y < FMap.Height ) then
          begin
            p := MapBase;
            Inc( p, NewMapX + MinX - 1 + ( NewMapY + j - 1 ) * FMap.Width );
            for i := MinX to MaxX do
            begin
              X := NewMapX + i - 1;
              if ( X >= 0 ) and ( X < FMap.Width ) then
              begin
                DrawTile( p, i, j, 0 );
                DrawTile( p, i, j, 1 );
              end;
              Inc( p );
            end;
          end;
        end;
        ClipY1 := NewMapY * FMap.TileHeight;
        ClipY2 := MapY * FMap.TileHeight;
      end
      else if ( NewMapY > MapY ) then
      begin
        for j := MapHeight - NewMapY + MapY + 1 to MapHeight do
        begin
          Y := NewMapY + j - 1;
          if ( Y >= 0 ) and ( Y < FMap.Height ) then
          begin
            p := MapBase;
            Inc( p, NewMapX + MinX - 1 + ( NewMapY + j - 1 ) * FMap.Width );
            for i := MinX to MaxX do
            begin
              X := NewMapX + i - 1;
              if ( X >= 0 ) and ( X < FMap.Width ) then
              begin
                DrawTile( p, i, j, 0 );
                DrawTile( p, i, j, 1 );
              end;
              Inc( p );
            end;
          end;
        end;
        ClipY1 := ( MapY + MapHeight ) * FMap.TileHeight;
        ClipY2 := ( NewMapY + MapHeight ) * FMap.TileHeight;
      end
      else
      begin
        ClipY1 := 0;
        ClipY2 := 0;
      end;
      // TODO GlobalUnlock( FMap.FMapData );
      MapX := NewMapX;
      MapY := NewMapY;
      DrawItemsClip( ClipX1, ClipX2, ClipY1, ClipY2 );
    end;
  end;
  MapOffsetX := OffsetX mod FMap.TileWidth;
  if ( MapOffsetX < 0 ) then
    Inc( MapOffsetX, FMap.TileWidth );
  MapOffsetY := OffsetY mod FMap.TileHeight;
  if ( MapOffsetY < 0 ) then
    Inc( MapOffsetY, FMap.TileHeight );
end;

procedure TAniView.PreCreateMap( W, H : longint );
var
  // TODO ddsd : TDDSurfaceDesc;
  ReturnCode : HRESULT;
begin
  MapPreCreated := true;
  lpDDSMap := nil;
  // TODO FilChar( @ddsd, SizeOf( ddsd ), 0 );
  {ddsd.dwSize := SizeOf( ddsd );
  ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
  ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_VIDEOMEMORY;
  ddsd.dwWidth := W;
  ddsd.dwHeight := H;
  ReturnCode := lpdd.CreateSurface( ddsd, lpDDSMap, nil );}
  // TODO if ( ReturnCode = DD_OK ) then
  {begin
    Log.Log( 'Map buffer created in VRAM' );
  end
   TODO else
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
  end;}
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
  // TODO ddsd : TDDSurfaceDesc;
  // TODO ddck : TDDCOLORKEY;
  ReturnCode : HRESULT;
begin
  if ( FMap = nil ) then
    Exit;
{TZone(FMap.Zones[1]).ExportTiles('f:/zone1tiles.bmp');
TZone(FMap.Zones[2]).ExportTiles('f:/zone2tiles.bmp');
TZone(FMap.Zones[0]).ExportItems('f:/zone0items.bmp');
TZone(FMap.Zones[1]).ExportItems('f:/zone1items.bmp');
TZone(FMap.Zones[2]).ExportItems('f:/zone2items.bmp');
TZone(FMap.Zones[3]).ExportItems('f:/zone3items.bmp');
TZone(FMap.Zones[4]).ExportItems('f:/zone4items.bmp');
TZone(FMap.Zones[5]).ExportItems('f:/zone5items.bmp');
TZone(FMap.Zones[6]).ExportItems('f:/zone6items.bmp');
TZone(FMap.Zones[7]).ExportItems('f:/zone7items.bmp');
TZone(FMap.Zones[8]).ExportItems('f:/zone8items.bmp');
TZone(FMap.Zones[9]).ExportItems('f:/zone9items.bmp');
TZone(FMap.Zones[10]).ExportItems('f:/zone10items.bmp');
TZone(FMap.Zones[11]).ExportItems('f:/zone11items.bmp');
TZone(FMap.Zones[12]).ExportItems('f:/zone12items.bmp');
TZone(FMap.Zones[13]).ExportItems('f:/zone13items.bmp');
TZone(FMap.Zones[14]).ExportItems('f:/zone14items.bmp');
TZone(FMap.Zones[15]).ExportItems('f:/zone15items.bmp');
TZone(FMap.Zones[16]).ExportItems('f:/zone16items.bmp');
TZone(FMap.Zones[17]).ExportItems('f:/zone17items.bmp');
TZone(FMap.Zones[18]).ExportItems('f:/zone18items.bmp');
TZone(FMap.Zones[19]).ExportItems('f:/zone19items.bmp');
TZone(FMap.Zones[1]).ExportTiles('f:/zone1tiles.bmp');
TZone(FMap.Zones[2]).ExportTiles('f:/zone2tiles.bmp');
TZone(FMap.Zones[3]).ExportTiles('f:/zone3tiles.bmp');
TZone(FMap.Zones[1]).ExportItems('f:/zone1items.bmp');
TZone(FMap.Zones[2]).ExportItems('f:/zone2items.bmp');
TZone(FMap.Zones[3]).ExportItems('f:/zone3items.bmp');}

  // TODO MapWidth := ( width div FMap.TileWidth ) + 2;
  // TODO MapHeight := ( Height div FMap.TileHeight ) + 2;
  MapBitWidth := MapWidth * FMap.TileWidth;
  MapBitHeight := MapHeight * FMap.TileHeight;
  W := MapBitWidth;
  H := MapBitHeight;
  if not MapPreCreated then
  begin
    // TODO Log.Log( 'Creating map buffer' );
    // TODO Log.Log( inttostr( W ) + ' x ' + inttostr( H ) );
    lpDDSMap := nil;
    // TODO ZeroMemory( @ddsd, SizeOf( ddsd ) );
    // TODO ddsd.dwSize := SizeOf( ddsd );
    // TODO ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
    // TODO ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_VIDEOMEMORY;
    // TODO ddsd.dwWidth := W;
    // TODO ddsd.dwHeight := H;
    // TODO ReturnCode := lpdd.CreateSurface( ddsd, lpDDSMap, nil );
    {if ( ReturnCode = DD_OK ) then
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
    end; }
  end;

  if FMap.NeedColorMatch then
  begin
    // TODO FMap.ColorMatch := FindColorMatch( FMap.FTransparentColor );
    FMap.NeedColorMatch := false;
  end;
  // TODO ddck.dwColorSpaceLowValue := FMap.FColorMatch;
  // TODO ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
  // TODO lpDDSMap.SetColorKey( DDCKEY_SRCBLT, {$IFNDEF DX5}@{$ENDIF}ddck );

  // TODO ddck.dwColorSpaceLowValue := FMap.FColorMatch;
  // TODO ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
  // TODO pDDSBack.SetColorKey( DDCKEY_SRCBLT, {$IFNDEF DX5}@{$ENDIF}ddck );

  if not MapPreCreated then
  begin
    // TODO  Log.Log( 'Creating work buffer' );
    Work := nil;
    // TODO ZeroMemory( @ddsd, SizeOf( ddsd ) );
    {ddsd.dwSize := SizeOf( ddsd );
    ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
    ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_VIDEOMEMORY;
    ddsd.dwWidth := WorkWidth;
    ddsd.dwHeight := WorkHeight;
    if ( lpdd.CreateSurface( ddsd, Work, nil ) <> DD_OK ) then
    begin
      ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
      lpdd.CreateSurface( ddsd, Work, nil );
    end;}
  end;

  // TODO Log.Log( 'MapColumns' );
  if ( MapColumns <> 0 ) then
    // TODO GlobalFree( MapColumns );
  MapColumns := 0;
  FMapColumnCount := MapWidth shl 2;
  // TODO MapColumns := GlobalAlloc( GPTR, FMapColumnCount * SizeOf( MapColumnHeaderInfo ) );
  // TODO Log.Log( 'MapRows' );
  if ( MapRows <> 0 ) then
    // TODO GlobalFree( MapRows );
  MapRows := 0;
  PixelHeight := ( FMap.Height + 32 ) * FMap.TileHeight; //32 added to correct off-bottom drawing errors
  // TODO MapRows := GlobalAlloc( GPTR, ( PixelHeight + FMap.TileHeight ) * SizeOf( RowUpdateInfo ) );
  // TODO Log.Log( 'BuildRowUpdateInfo' );

  BuildRowUpdateInfo;
  MapX := OffsetX div FMap.TileWidth;
  MapY := OffsetY div FMap.TileHeight;
  // TODO Log.Log( 'Assignment Complete' );
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
  MinX := ( X div FMap.StripWidth ) * FMap.StripWidth;
  MaxX := ( ( X + W ) div FMap.StripWidth + 1 ) * FMap.StripWidth;

  X1 := X div FMap.TileWidth;
  if X1 < 0 then
    X1 := 0;
  Y1 := Y div FMap.TileHeight;
  if Y1 < 0 then
    Y1 := 0;
  X2 := ( X + W ) div FMap.TileWidth;
  if X2 >= FMap.Width then
    X2 := FMap.Width - 1;
  Y2 := ( Y + H ) div FMap.TileHeight;
  if Y2 >= FMap.Height then
    Y2 := FMap.Height - 1;
  // TODO R.Left := MinX - OffsetX + Left;
  // TODO R.Right := MaxX - OffsetX + Left;
  // TODO R.Top := Y - OffsetY + Top;
  // TODO R.Bottom := Y + H - OffsetY + Top;
  // TODO MapBase := GlobalLock( FMap.FMapData );
  for Layer := 0 to 1 do
  begin
    for j := Y1 to Y2 do
    begin
      YA := j * FMap.TileHeight - OffsetY;
      p := MapBase;
      Inc( p, X1 + j * FMap.Width );
      for i := X1 to X2 do
      begin
        XA := i * FMap.TileWidth - OffsetX;
        // TODO CopyTile( lpDDSBack, p, XA + Left, YA + Top, Layer, @R );
        Inc( p );
      end;
    end;
  end;
  // TODO GlobalUnlock( FMap.FMapData );

  //Update items (items with descenders)
  // TODO RowBase := GlobalLock( MapRows );
  RowData := RowBase;
  if ( Y > 0 ) then
    Inc( RowData, Y );
  i := RowData^.DescendRow;
  RowData := RowBase;
  Inc( RowData, i );
  i := RowData^.ItemIndex;
  if ( i = 0 ) then
  begin
    // TODO GlobalUnlock( MapRows );
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
    if ( FMap.ItemList[ i ].Y > MaxRow ) and ( FMap.ItemList[ i ].Y <= PixelHeight ) then
      Break;
    if FMap.ItemList[ i ].Visible then
    begin
      if ( FMap.ItemList[ i ].X >= MinX ) and ( FMap.ItemList[ i ].X < MaxX ) then
      begin
        if ( FMap.ItemList[ i ].FilterID = 0 ) or ( ( FMap.ItemList[ i ].FilterID < 0 ) or
          ( FMap.ItemList[ i ].FilterID = FItemMask ) ) and
          ( FMap.ItemList[ i ].FilterID <> -FItemMask ) then
        begin

          R.Left := FMap.ItemList[ i ].ImageX;
          R.Right := FMap.ItemList[ i ].ImageX + FMap.ItemList[ i ].Width;
          R.Top := FMap.ItemList[ i ].ImageY + FMap.ItemList[ i ].VHeight;
          R.Bottom := FMap.ItemList[ i ].ImageY + FMap.ItemList[ i ].Height;

          // TODO XA := FMap.ItemList[ i ].X - OffsetX + Left;
          // TODO YA := FMap.ItemList[ i ].Y - OffsetY + Top;
          // TODO Clip1( X1, X2, XA, R.Left, R.Right );
          // TODO Clip1( Y1, Y2, YA, R.Top, R.Bottom );

          // TODO// TODO WrapperBltFast( lpDDSBack, XA, YA, TZone( FMap.Zones.Items[ FMap.ItemList[ i ].Zone ] ).FItemImages, R, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        end;
      end;
    end;
    i := FMap.ItemList[ i ].Next;
  end;
end;

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
  // TODO MapBase := GlobalLock( FMap.MapData );
  for Layer := 0 to 1 do
  begin
    for j := Zone.Y1 to Zone.Y2 do
    begin
      Y := ( j - Zone.Y1 ) * FMap.TileHeight;
      p := MapBase;
      Inc( p, Zone.X1 + j * FMap.Width );
      for i := Zone.X1 to Zone.X2 do
      begin
        if FMap.Zones.Items[ p^.Zone[ Layer ] ] = Zone then
        begin
          X := ( i - Zone.X1 ) * FMap.TileWidth;
          Index := p^.Tile[ Layer ];
          if ( Index <> $FFFF ) then
          begin
            Dec( Index, Offset );
            SrcX := ( Index div Zone.TileMaxColumnIndex ) * FMap.TileWidth;
            SrcY := ( Index mod Zone.TileMaxColumnIndex ) * FMap.TileHeight;
            if ( Layer = 0 ) then
            begin
              // TODO// TODO WrapperBltFast( Work, X, Y, Zone.FTileImages, Rect( SrcX, SrcY, SrcX + FMap.TileWidth, SrcY + FMap.TileHeight ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
            end
            else
            begin
              // TODO// TODO WrapperBltFast( Work, X, Y, Zone.FTileImages, Rect( SrcX, SrcY, SrcX + FMap.TileWidth, SrcY + FMap.TileHeight ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
            end;
          end;
        end;
        Inc( p );
      end;
    end;
  end;
  // TODO GlobalUnlock( FMap.MapData );

  ZoneX := Zone.X1 * FMap.TileWidth - OffsetX;
  ZoneY := Zone.Y1 * FMap.TileHeight - OffsetY;
  W := ( Zone.X2 - Zone.X1 + 1 ) * FMap.TileWidth;
  H := ( Zone.Y2 - Zone.Y1 + 1 ) * FMap.TileHeight;

  Offset := ( Zone.States - NewState ) * Zone.ItemStateOffset;
  MaxY := MapHeight * FMap.TileHeight;
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

        // TODO Clip1( ZoneY, ZoneY + H, Ya, SrcY1a, SrcY2a );
        // TODO// TODO WrapperBltFast( Work, X - ZoneX, Ya - ZoneY, Zone.FItemImages, Rect( SrcX1, SrcY1a, SrcX2, SrcY2a ),          DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

        //also clip x against edge of screen
        // TODO Clip1( Y, ZoneY, Y, SrcY1, SrcY2 );
        // TODO Clip1( 0, Width, X, SrcX1, SrcX2 );
        // TODO// TODO WrapperBltFast( lpDDSBack, X + Left, Y + Top, Zone.FItemImages, Rect( SrcX1, SrcY1, SrcX2, SrcY2 ),         DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
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
  // TODO from { to }
  {if ( ZoneX + W > Width ) then
  begin
    W := Width - ZoneX;
  end;
  if ( ZoneY + H > Height ) then
  begin
    H := Height - ZoneY;
  end;

  // TODO WrapperBltFast( Work, SrcX, SrcY, lpDDSBack, Rect( ZoneX + Left, ZoneY + Top, ZoneX + Left + W, ZoneY + Top + H ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
  // TODO WrapperBltFast( lpDDSBack, ZoneX + Left, ZoneY + Top, Work, Rect( SrcX, SrcY, SrcX + W, SrcY + H ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );}
end;

procedure TAniView.FRefreshMap;
var
  i, j : Integer;
  MapBase, p : ^GridInfo;
  Layer : Integer;
  X, Y : Longint;
begin
  //Log.Log('Refresh Map a');
  // TODO MapBase := GlobalLock( FMap.FMapData );
  for Layer := 0 to 1 do
  begin
    for j := 1 to MapHeight do
    begin
      Y := MapY + j - 1;
      if ( Y >= 0 ) and ( Y < FMap.Height ) then
      begin
        p := MapBase;
        Inc( p, MapX + Y * FMap.Width );
        for i := 1 to MapWidth do
        begin
          X := MapX + i - 1;
          if ( X >= 0 ) and ( X < FMap.Width ) then
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
  // TODO GlobalUnlock( FMap.FMapData );
//Log.Log('Refresh Map e');
// TODO   DrawItems;
//Log.Log('Refresh Map f');
end;

procedure TAniView.RefreshMap;
begin
  MapX := OffsetX div FMap.TileWidth;
  if ( OffsetX < 0 ) then
    if ( ( OffsetX mod FMap.TileWidth ) <> 0 ) then
      Dec( MapX );
  MapY := OffsetY div FMap.TileHeight;
  if ( OffsetY < 0 ) then
    if ( ( OffsetY mod FMap.TileHeight ) <> 0 ) then
      Dec( MapY );

  MapOffsetX := OffsetX mod FMap.TileWidth;
  if ( MapOffsetX < 0 ) then
    Inc( MapOffsetX, FMap.TileWidth );
  MapOffsetY := OffsetY mod FMap.TileHeight;
  if ( MapOffsetY < 0 ) then
    Inc( MapOffsetY, FMap.TileHeight );
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
  SrcY1, SrcY2 : Longint;
  MapH, DstH : Longint;
begin
  MinY := MapY * FMap.TileHeight;
  if ( MinY > PixelHeight ) then
    Exit;
  MinX := MapX * FMap.TileWidth;
  MaxY := ( MapY + MapHeight ) * FMap.TileHeight;
  MaxX := ( MapX + MapWidth ) * FMap.TileWidth;
  MapH := MapBitHeight;
  // TODO RowBase := GlobalLock( MapRows );
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
  // TODO GlobalUnlock( MapRows );

  while ( i <> 0 ) do
  begin
    if ( FMap.ItemList[ i ].Y > MaxRow ) and ( FMap.ItemList[ i ].Y <= PixelHeight ) then
      Break;
    if FMap.ItemList[ i ].Visible then
    begin
      if ( FMap.ItemList[ i ].FilterID = 0 ) or ( ( FMap.ItemList[ i ].FilterID < 0 ) or
        ( FMap.ItemList[ i ].FilterID = FItemMask ) ) and
        ( FMap.ItemList[ i ].FilterID <> -FItemMask ) then
      begin
        Y := FMap.ItemList[ i ].Y - FMap.ItemList[ i ].VHeight;
        if ( FMap.ItemList[ i ].X < MaxX ) and ( Y + FMap.ItemList[ i ].Height >= MinY ) then
        begin
          if ( FMap.ItemList[ i ].X + FMap.ItemList[ i ].Width > MinX ) and ( Y < MaxY ) then
          begin
            X := FMap.ItemList[ i ].X - MinX;
            dec( Y, MinY );
            ZoneItem := TZone( FMap.Zones.Items[ FMap.ItemList[ i ].Zone ] );
            DstH := Y + FMap.ItemList[ i ].Height;
            if ( Y < 0 ) then
            begin
              SrcY1 := FMap.ItemList[ i ].ImageY - Y;
              SrcY2 := FMap.ItemList[ i ].ImageY + FMap.ItemList[ i ].Height;
              if ( DstH > MapH ) then
                Dec( SrcY2, DstH - MapH );
              Y := 0;
            end
            else
            begin
              SrcY1 := FMap.ItemList[ i ].ImageY;
              SrcY2 := FMap.ItemList[ i ].ImageY + FMap.ItemList[ i ].Height;
              if ( DstH > MapH ) then
                Dec( SrcY2, DstH - MapH );
            end;

            if ZoneItem is TLightZone then
            begin
              if not ZoneItem.FullRefresh then
              begin
                // TODO// TODO WrapperBltFast( lpDDSMap, X, Y, ZoneItem.FItemImages, Rect( FMap.ItemList[ i ].ImageX, SrcY1,                  FMap.ItemList[ i ].ImageX + FMap.ItemList[ i ].Width, SrcY2 ),                  DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
              end;
            end
            else
            begin
              // TODO// TODO WrapperBltFast( lpDDSMap, X, Y, ZoneItem.FItemImages, Rect( FMap.ItemList[ i ].ImageX, SrcY1,                FMap.ItemList[ i ].ImageX + FMap.ItemList[ i ].Width, SrcY2 ),                DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
            end;
          end;
        end;
      end;
    end;
    i := FMap.ItemList[ i ].Next;
  end;
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
  SrcY1, SrcY2 : Longint;
  MapH, DstH : Longint;
begin
  MinY := MapY * FMap.TileHeight;
  if ( MinY > PixelHeight ) then
    Exit;
  MinX := MapX * FMap.TileWidth;
  MaxY := ( MapY + MapHeight ) * FMap.TileHeight;
  MaxX := ( MapX + MapWidth ) * FMap.TileWidth;
  MapH := MapBitHeight;
  // TODO   RowBase := GlobalLock( MapRows );
  RowData := RowBase;
  if ( MinY > 0 ) then
    Inc( RowData, MinY );
  i := RowData^.DescendRow;
  RowData := RowBase;
  Inc( RowData, i );
  i := RowData^.ItemIndex;
  if ( i = 0 ) then
  begin
    // TODO GlobalUnlock( MapRows );
    Exit;
  end;
  if ( MaxY > PixelHeight ) then
    MaxRow := PixelHeight
  else
  begin
    Inc( RowBase, MaxY );
    MaxRow := RowBase^.OverlapRow;
  end;
  // TODO GlobalUnlock( MapRows );

  while ( i <> 0 ) do
  begin
    if ( FMap.ItemList[ i ].Y > MaxRow ) and ( FMap.ItemList[ i ].Y <= PixelHeight ) then
      Break;
    if FMap.ItemList[ i ].Visible then
    begin
      if ( FMap.ItemList[ i ].FilterID = 0 ) or ( ( FMap.ItemList[ i ].FilterID < 0 ) or
        ( FMap.ItemList[ i ].FilterID = FItemMask ) ) and
        ( FMap.ItemList[ i ].FilterID <> -FItemMask ) then
      begin
        if ( FMap.ItemList[ i ].X < MaxX ) then
        begin
          Y := FMap.ItemList[ i ].Y - FMap.ItemList[ i ].VHeight + FMap.ItemList[ i ].Height;
          if ( Y >= MinY ) then
          begin
            W := FMap.ItemList[ i ].Width;
            W1 := W;
            H := FMap.ItemList[ i ].Height;
            X3 := FMap.ItemList[ i ].X + W;
            Y3 := FMap.ItemList[ i ].Y - FMap.ItemList[ i ].VHeight;
            ZoneItem := TZone( FMap.Zones.Items[ FMap.ItemList[ i ].Zone ] );
            if ( X3 > MinX ) and ( Y3 < MaxY ) then
            begin
              if ( X3 > X1 ) and ( FMap.ItemList[ i ].X < X2 ) then
              begin
                //Perform clipping
                if ( FMap.ItemList[ i ].X <= X1 ) then
                begin
                  SrcX := FMap.ItemList[ i ].ImageX + W - X3 + X1;
                  DstX := X1 - MinX;
                  if ( X3 < X2 ) then
                    W := X3 - X1
                  else
                    W := X2 - X1;
                end
                else
                begin
                  SrcX := FMap.ItemList[ i ].ImageX;
                  DstX := FMap.ItemList[ i ].X - MinX;
                  W := X2 - FMap.ItemList[ i ].X;
                  if ( W > FMap.ItemList[ i ].Width ) then
                    W := FMap.ItemList[ i ].Width;
                end;
                DstY := Y3 - MinY;
                DstH := DstY + H;
                if ( DstY < 0 ) then
                begin
                  SrcY1 := FMap.ItemList[ i ].ImageY - DstY;
                  SrcY2 := FMap.ItemList[ i ].ImageY + H;
                  if ( DstH > MapH ) then
                    Dec( SrcY2, DstH - MapH );
                  DstY := 0;
                end
                else
                begin
                  SrcY1 := FMap.ItemList[ i ].ImageY;
                  SrcY2 := FMap.ItemList[ i ].ImageY + H;
                  if ( DstH > MapH ) then
                    Dec( SrcY2, DstH - MapH );
                end;

                if SrcY2 > SrcY1 then
                begin
                  if ZoneItem is TLightZone then
                  begin
                    if not ZoneItem.FullRefresh then
                    begin
                      // TODO// TODO WrapperBltFast( lpDDSMap, DstX, DstY, ZoneItem.FItemImages, Rect( SrcX, SrcY1,                        SrcX + W, SrcY2 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
                    end;
                  end
                  else
                  begin
                    // TODO// TODO WrapperBltFast( lpDDSMap, DstX, DstY, ZoneItem.FItemImages, Rect( SrcX, SrcY1,                    SrcX + W, SrcY2 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
                  end;
                end;
              end;

              if ( Y > Y1 ) and ( Y3 < Y2 ) then
              begin
                if ( Y3 <= Y1 ) then
                begin
                  SrcY := FMap.ItemList[ i ].ImageY + Y1 - Y3;
                  DstY := Y1 - MinY;
                  if ( Y < Y2 ) then
                    H := Y - Y1
                  else
                    H := Y2 - Y1;
                end
                else
                begin
                  SrcY := FMap.ItemList[ i ].ImageY;
                  DstY := Y3 - MinY;
                  H := Y2 - Y3;
                  if ( H > FMap.ItemList[ i ].Height ) then
                    H := FMap.ItemList[ i ].Height;
                end;
                DstX := FMap.ItemList[ i ].X - MinX;
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
                      // TODO// TODO WrapperBltFast( lpDDSMap, DstX, DstY, ZoneItem.FItemImages, Rect( FMap.ItemList[ i ].ImageX, SrcY1,                        FMap.ItemList[ i ].ImageX + W1, SrcY2 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
                    end;
                  end
                  else
                  begin
                    // TODO// TODO WrapperBltFast( lpDDSMap, DstX, DstY, ZoneItem.FItemImages, Rect( FMap.ItemList[ i ].ImageX, SrcY1,                      FMap.ItemList[ i ].ImageX + W1, SrcY2 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
                  end;
                end;
              end;

            end;
          end;
        end;
      end;
    end;
    i := FMap.ItemList[ i ].Next;
  end;
end;

procedure TAniView.DrawTile( GridLoc : Pointer; i, j, Layer : Integer );
var
  X, Y : Integer;
begin
  X := ( i - 1 ) * FMap.TileWidth;
  Y := ( j - 1 ) * FMap.TileHeight;

  CopyTile( lpDDSMap, GridLoc, X, Y, Layer, nil );


  PGridInfo( GridLoc ).BitField := PGridInfo( GridLoc ).BitField or $40; //This space has been viewed on screen
end;

procedure TAniView.CopyTile( Dest : PSDL_Surface; GridLoc : Pointer; X, Y, Layer : Integer; ClipRect : PRect );

var
  Index : Word;
  SrcX, SrcY : Longint;
  DstX, DstY : Longint;
  p : ^GridInfo;
  ZoneTile : TZone;
  HalfWidth, HalfHeight : Integer;
  // TODO BltFx : TDDBLTFX;
  SrcX2, SrcY2 : Longint;
  Offset : Integer;
begin
  p := GridLoc;
  Index := p^.Tile[ Layer ];
  if ( Layer = 1 ) and ( ( p^.BitField and $80 ) <> 0 ) then
  begin
    HalfWidth := FMap.TileWidth div 2;
    HalfHeight := FMap.TileHeight div 2;

    if ( Index <> $FFFF ) then
    begin
      ZoneTile := TZone( FMap.Zones.Items[ p^.Zone[ 1 ] ] );
      SrcX := ( Index div ZoneTile.TileMaxColumnIndex ) * FMap.TileWidth;
      SrcY := ( Index mod ZoneTile.TileMaxColumnIndex ) * FMap.TileHeight + HalfHeight;
      SrcX2 := SrcX + FMap.TileWidth;
      SrcY2 := SrcY + HalfHeight;
      DstX := X;
      DstY := Y;
      if Assigned( ClipRect ) then
      begin
        // TODO Clip1( ClipRect.Left, ClipRect.Right, DstX, SrcX, SrcX2 );
        // TODO Clip1( ClipRect.Top, ClipRect.Bottom, DstY, SrcY, SrcY2 );
      end;
      if ( SrcX2 > SrcX ) and ( SrcY2 > SrcY ) then
      begin
        // TODO// TODO WrapperBltFast( Dest, DstX, DstY, ZoneTile.FTileImages, Rect( SrcX, SrcY,     SrcX2, SrcY2 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end;
    end;

    if ( p^.Tile[ 2 ] <> $FFFF ) then
    begin
      ZoneTile := TZone( FMap.Zones.Items[ p^.Zone[ 2 ] ] );
      SrcX := ( p^.Tile[ 2 ] div ZoneTile.TileMaxColumnIndex ) * FMap.TileWidth;
      SrcY := ( p^.Tile[ 2 ] mod ZoneTile.TileMaxColumnIndex ) * FMap.TileHeight;
      SrcX2 := SrcX + HalfWidth;
      SrcY2 := SrcY + FMap.TileHeight;
      DstX := X + HalfWidth;
      DstY := Y;
      if Assigned( ClipRect ) then
      begin
        // TODO Clip1( ClipRect.Left, ClipRect.Right, DstX, SrcX, SrcX2 );
        // TODO Clip1( ClipRect.Top, ClipRect.Bottom, DstY, SrcY, SrcY2 );
      end;
      if ( SrcX2 > SrcX ) and ( SrcY2 > SrcY ) then
      begin
        // TODO WrapperBltFast( Dest, DstX, DstY, ZoneTile.FTileImages, Rect( SrcX, SrcY,  SrcX2, SrcY2 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end;
    end;

    if ( p^.Tile[ 3 ] <> $FFFF ) then
    begin
      ZoneTile := TZone( FMap.Zones.Items[ p^.Zone[ 3 ] ] );
      SrcX := ( p^.Tile[ 3 ] div ZoneTile.TileMaxColumnIndex ) * FMap.TileWidth;
      SrcY := ( p^.Tile[ 3 ] mod ZoneTile.TileMaxColumnIndex ) * FMap.TileHeight;
      SrcX2 := SrcX + FMap.TileWidth;
      SrcY2 := SrcY + HalfHeight;
      DstX := X;
      DstY := Y + HalfHeight;
      if Assigned( ClipRect ) then
      begin
        // TODO Clip1( ClipRect.Left, ClipRect.Right, DstX, SrcX, SrcX2 );
        // TODO Clip1( ClipRect.Top, ClipRect.Bottom, DstY, SrcY, SrcY2 );
      end;
      if ( SrcX2 > SrcX ) and ( SrcY2 > SrcY ) then
      begin
        // TODO WrapperBltFast( Dest, DstX, DstY, ZoneTile.FTileImages, Rect( SrcX, SrcY, SrcX2, SrcY2 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end;
    end;

    if ( p^.Tile[ 4 ] <> $FFFF ) then
    begin
      ZoneTile := TZone( FMap.Zones.Items[ p^.Zone[ 4 ] ] );
      SrcX := ( p^.Tile[ 4 ] div ZoneTile.TileMaxColumnIndex ) * FMap.TileWidth + HalfWidth;
      SrcY := ( p^.Tile[ 4 ] mod ZoneTile.TileMaxColumnIndex ) * FMap.TileHeight;
      SrcX2 := SrcX + HalfWidth;
      SrcY2 := SrcY + FMap.TileHeight;
      DstX := X;
      DstY := Y;
      if Assigned( ClipRect ) then
      begin
        // TODO Clip1( ClipRect.Left, ClipRect.Right, DstX, SrcX, SrcX2 );
        // TODO Clip1( ClipRect.Top, ClipRect.Bottom, DstY, SrcY, SrcY2 );
      end;
      if ( SrcX2 > SrcX ) and ( SrcY2 > SrcY ) then
      begin
        // TODO WrapperBltFast( Dest, DstX, DstY, ZoneTile.FTileImages, Rect( SrcX, SrcY, SrcX2, SrcY2 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end;
    end;
  end
  else
  begin
    if ( Index = $FFFF ) then
      Exit;
    ZoneTile := TZone( FMap.Zones.Items[ p^.Zone[ Layer ] ] );
    if ZoneTile is TLightZone then
    begin
      if ZoneTile.FullRefresh then
      begin
        if Dest = lpDDSMap then
        begin
          if Layer = 1 then
            Exit;
          DstX := X + FMap.TileWidth;
          DstY := Y + FMap.TileHeight;
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
          // TODO BltFx.dwSize := SizeOf( BltFx );
          // TODO BltFx.dwFillColor := FMap.FColorMatch;
          // TODO WrapperBlt( Dest, Rect( X, Y, DstX, DstY ), nil, Rect( X, Y, DstX, DstY ), DDBLT_COLORFILL + DDBLT_WAIT, BltFx );
          Exit;
        end
        else
        begin
          Offset := ( TLightZone( ZoneTile ).States - TLightZone( ZoneTile ).State ) * TLightZone( ZoneTile ).TileStateOffset;
          Dec( Index, Offset );
        end;
      end;
    end;
    SrcX := ( Index div ZoneTile.TileMaxColumnIndex ) * FMap.TileWidth;
    SrcY := ( Index mod ZoneTile.TileMaxColumnIndex ) * FMap.TileHeight;
    if ( Layer = 0 ) then
    begin
      SrcX2 := SrcX + FMap.TileWidth;
      SrcY2 := SrcY + FMap.TileHeight;
      DstX := X;
      DstY := Y;
      if Assigned( ClipRect ) then
      begin
        // TODO Clip1( ClipRect.Left, ClipRect.Right, DstX, SrcX, SrcX2 );
        // TODO Clip1( ClipRect.Top, ClipRect.Bottom, DstY, SrcY, SrcY2 );
        if ( SrcX2 <= SrcX ) or ( SrcY2 <= SrcY ) then
          Exit;
      end;
      // TODO WrapperBltFast( Dest, DstX, DstY, ZoneTile.FTileImages, Rect( SrcX, SrcY, SrcX2, SrcY2 ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
    end
    else
    begin
      SrcX2 := SrcX + FMap.TileWidth;
      SrcY2 := SrcY + FMap.TileHeight;
      DstX := X;
      DstY := Y;
      if Assigned( ClipRect ) then
      begin
        // TODO Clip1( ClipRect.Left, ClipRect.Right, DstX, SrcX, SrcX2 );
        // TODO Clip1( ClipRect.Top, ClipRect.Bottom, DstY, SrcY, SrcY2 );
        if ( SrcX2 <= SrcX ) or ( SrcY2 <= SrcY ) then
          Exit;
      end;
      // TODO WrapperBltFst( Dest, DstX, DstY, ZoneTile.FTileImages, Rect( SrcX, SrcY,   SrcX2, SrcY2 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    end;
  end;
end;

procedure TAniView.CenterView( X, Y : Longint );
begin
  KeyFigure := nil;
  CenterX := X;
  CenterY := Y;
end;

procedure TAniView.MouseDown( Button : Integer; Shift : TSDLMod; MousePos : TPoint );
begin
  // TODO FLMousebutton := Button = mbLeft;
  if Assigned( FOnMouseDown ) then
  begin
    inherited MouseDown( Button, Shift, MousePos );
    OnMouseDown( Self, Button, Shift, MousePos, MousePos.X + OffsetX, MousePos.Y + OffsetY );
  end;
end;

procedure TAniView.MouseUp( Button : Integer; Shift : TSDLMod; MousePos : TPoint );
begin
  FLMousebutton := False;
  if Assigned( FOnMouseUp ) then
  begin
    inherited MouseUp( Button, Shift, MousePos );
    OnMouseUp( Self, Button, Shift, MousePos, MousePos.X + OffsetX, MousePos.Y + OffsetY );
  end;
end;

procedure TAniView.MouseMove( Shift : TSDLMod; CurrentPos : TPoint; RelativePos : TPoint );
begin
  if Assigned( FOnMouseMove ) then
  begin
    inherited MouseMove( Shift, CurrentPos, RelativePos );
    OnMouseMove( Self, Shift, CurrentPos, CurrentPos.X + OffsetX, CurrentPos.Y + OffsetY );
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
  // TODO RowBase := GlobalLock( MapRows );
  RowData := RowBase;
  Y := 0;
  MaxHeight := 0;
  i := FMap.FirstItem;
  while ( i > 0 ) do
  begin
    ItemY := FMap.ItemList[ i ].Y;
    if ItemY < 0 then
    begin
    end
    else if ( ItemY = Y ) then
    begin
      if ( FMap.ItemList[ i ].VHeight > MaxHeight ) then
        MaxHeight := FMap.ItemList[ i ].VHeight;
    end
    else
    begin
      RowData^.MaxHeight := MaxHeight;
      if ( ItemY >= PixelHeight ) then
      begin
        RowData := RowBase;
        Inc( RowData, PixelHeight );
        MaxHeight := FMap.ItemList[ i ].VHeight - ItemY + PixelHeight - 1;
      end
      else
      begin
        Inc( RowData, ItemY - Y );
        MaxHeight := FMap.ItemList[ i ].VHeight;
      end;
      Y := ItemY;
      RowData^.ItemIndex := i;
    end;
    i := FMap.ItemList[ i ].Next;
  end;
  RowData^.MaxHeight := MaxHeight;

  MaxRow := PixelHeight + FMap.TileHeight;
  RowData := RowBase;
  for i := 0 to MaxRow - 1 do
  begin
    RowData^.DescendRow := i;
    Inc( RowData );
  end;

  i := FMap.FirstItem;
  while ( i > 0 ) do
  begin
    Y := FMap.ItemList[ i ].Y - FMap.ItemList[ i ].VHeight + FMap.ItemList[ i ].Height;
    if ( Y >= MaxRow ) then
      Y := MaxRow - 1;
    if ( Y >= 0 ) then
    begin
      j := FMap.ItemList[ i ].Y + 1;
      if j < 0 then
        j := 0;
      RowData := RowBase;
      Inc( RowData, j );
      for j := j to Y do
      begin
        if ( FMap.ItemList[ i ].Y < RowData^.DescendRow ) then
        begin
          if FMap.ItemList[ i ].Y < 0 then
            RowData^.DescendRow := 0
          else
            RowData^.DescendRow := FMap.ItemList[ i ].Y;
        end;
        Inc( RowData );
      end;
    end;
    i := FMap.ItemList[ i ].Next;
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
  // TODO GlobalUnlock( MapRows );
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
  CellWidth := FMap.StripWidth shr 1;
  CellHeight := FMap.StripHeight shr 1;
  DestX1 := FStartX + DestX * CellWidth;
  DestY1 := FStartY + DestY * CellHeight;

  if Assigned( FAstarAvoidFigure ) then
  begin
    for i := 0 to FAstarAvoidFigure.Count - 1 do
    begin
      A := sqr( TAniFigure( FAstarAvoidFigure.Items[ i ] ).Radius + TAniFigure( FAStarFigure ).Radius ) + sqr( FMap.StripWidth ); //StripWidth is the fudge factor
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

  R := TAniFigure( FAStarFigure ).Radius + CellWidth + 2; //2 added for round off error
  R2 := R * R;
  X1 := DestX1 - R; //if (X1<0) then X1:=0;
  X2 := DestX1 + R; //if (X2>=FMap.FBitWidth) then X2:=FMap.FBitWidth-1;
  Y1 := DestY1 - R; //if (Y1<0) then Y1:=0;
  Y2 := DestY1 + R; //if (Y2>=FMap.FBitheight) then Y2:=FMap.FBitHeight-1;
  if ( X1 < FMap.TileWidth - CellWidth ) or ( X2 >= FMap.BitWidth + FMap.TileWidth + CellWidth ) or
    ( Y1 < FMap.TileHeight - CellWidth ) or ( Y2 >= FMap.Bitheight + FMap.TileHeight + CellWidth ) then
  begin
    Result := False;
    Exit;
  end;

  // TODO GridBase := GlobalLock( FMap.FMapData );

  XL := X1 div FMap.TileWidth;

  for Y := Y1 div FMap.TileHeight to Y2 div FMap.TileHeight do
  begin
    if ( Y >= 0 ) and ( Y < FMap.Height ) then
    begin
      ScanY := ( Y + 1 ) * FMap.TileHeight - CellHeight;
      GridLoc := GridBase;
      Inc( GridLoc, Y * FMap.Width + XL );
      for X := XL to X2 div FMap.TileWidth do
      begin
        if ( X >= 0 ) and ( X < FMap.Width ) then
        begin
          if TAniFigure( FAStarFigure ).UseLineOfSight then
            CollisionMask := GridLoc^.LineOfSightMask
          else
            CollisionMask := GridLoc^.CollisionMask;
          if ( CollisionMask <> 0 ) then
          begin
            for j := 0 to 3 do
            begin
              if ( ( CollisionMask and $F ) <> 0 ) then
              begin
                cy := ScanY - j * FMap.StripHeight;
                for i := 0 to 3 do
                begin
                  if ( ( CollisionMask and 1 ) = 1 ) then
                  begin
                    cx := X * FMap.TileWidth + i * FMap.StripWidth + CellWidth;
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
  // TODO GlobalUnlock( FMap.FMapData );
  Result := True;
end;

function TAniView.FindPath( Figure : TCustomAniFigure; X2, Y2, Deviance : Longint; var Path : HGLOBAL ) : integer;
var
  p : ^TPoint;
  dx, dy : Longint;
  CellWidth, CellHeight : Integer;
  i : integer;
begin
  result := 0;
  CellWidth := FMap.StripWidth shr 1;
  CellHeight := FMap.StripHeight shr 1;
  Dx := Round( 2 * ( X2 - TAniFigure( Figure ).StepX ) / FMap.StripWidth );
  dy := Round( 2 * ( Y2 - TAniFigure( Figure ).StepY ) / FMap.StripHeight );
  if ( dx = 0 ) and ( dy = 0 ) then
    Exit;
  if ( Dx <= MaxSearch ) and ( Dx >= MinSearch ) and ( dy <= MaxSearch ) and ( dy >= MinSearch ) then
  begin
    FAStarFigure := Figure;
    FAStarAvoidFigure := nil;
    FStartX := CellWidth * Trunc( 2 * TAniFigure( Figure ).StepX / FMap.StripWidth ) + ( FMap.StripWidth shr 2 );
    FStartY := CellHeight * Trunc( 2 * TAniFigure( Figure ).StepY / FMap.StripHeight ) + ( FMap.StripHeight shr 2 );
    FAStar.Deviance := Deviance;
    result := FAStar.FindJaggedPath( 0, 0, dx, dy, Path );
    if result > 0 then
    begin
      // TODO p := GlobalLock( Path );
      for i := 1 to result do
      begin
        p^.X := p^.X * CellWidth + FStartX;
        p^.Y := p^.Y * CellHeight + FStartY;
        Inc( p );
      end;
      // TODO GlobalUnlock( Path );
    end;
  end;
end;

procedure TAniView.GetPath( Figure : TCustomAniFigure );
var
  Dx, dy : Longint;
  D : Double;
  pBase, p : ^TPoint;
  CellWidth, CellHeight : Integer;
  i : Integer;
begin
  TAniFigure( Figure ).NeedPath := False;
  CellWidth := FMap.StripWidth shr 1;
  CellHeight := FMap.StripHeight shr 1;
  Dx := Round( 2 * ( TAniFigure( Figure ).PathDestX - TAniFigure( Figure ).StepX ) / FMap.StripWidth );
  dy := Round( 2 * ( TAniFigure( Figure ).PathDestY - TAniFigure( Figure ).StepY ) / FMap.StripHeight );
  if ( Dx = 0 ) and ( dy = 0 ) then
  begin
    TAniFigure( Figure ).GotPath := False;
    if Assigned( TAniFigure( Figure ).OnNoPath ) then
      TAniFigure( Figure ).OnNoPath( Figure );
    Exit;
  end;
  if ( Dx <= MaxSearch ) and ( Dx >= MinSearch ) and ( dy <= MaxSearch ) and ( dy >= MinSearch ) then
  begin
    FAStarFigure := Figure;
    FAStarAvoidFigure := TAniFigure( Figure ).AvoidInPath;
    FStartX := CellWidth * Trunc( 2 * TAniFigure( Figure ).StepX / FMap.StripWidth ) + ( FMap.StripWidth shr 2 );
    FStartY := CellHeight * Trunc( 2 * TAniFigure( Figure ).StepY / FMap.StripHeight ) + ( FMap.StripHeight shr 2 );

    FAStar.Deviance := TAniFigure( Figure ).PathDeviance;
    if ( not Assigned( FAstarAvoidFigure ) ) or ( FAstarAvoidFigure.Count = 0 ) then
    begin
      if not CanMove( dx, dy, dx, dy ) then
      begin
        FAStar.Deviance := 16;
      end;
    end;

    TAniFigure( Figure ).PathCount := FAStar.FindPath( 0, 0, Dx, dy, TAniFigure( Figure ).PathHandle );
    if ( TAniFigure( Figure ).PathCount > 0 ) and ( TAniFigure( Figure ).PathHandle <> 0 ) then
    begin
      // TODO pBase := GlobalLock( TAniFigure( Figure ).PathHandle );
      p := pBase;

      for i := 1 to TAniFigure( Figure ).PathCount do
      begin
        p^.X := p^.X * CellWidth + FStartX;
        p^.Y := p^.Y * CellHeight + FStartY;
        Inc( p );
      end;
      TAniFigure( Figure ).DestX := pBase^.X;
      TAniFigure( Figure ).DestY := pBase^.Y;
      TAniFigure( Figure ).DestZ := TAniFigure( Figure ).FZ;
      TAniFigure( Figure ).GotPath := True;
      TAniFigure( Figure ).Terminal := True;
      TAniFigure( Figure ).Moving := True;
      TAniFigure( Figure ).PathStep := 1;
      Dx := TAniFigure( Figure ).DestX - TAniFigure( Figure ).FX;
      dy := 2 * ( TAniFigure( Figure ).DestY - TAniFigure( Figure ).FY );
      D := sqrt( sqr( Dx ) + sqr( dy ) );
      if D <> 0 then
      begin
        TAniFigure( Figure ).SlopeX := Dx / D;
        TAniFigure( Figure ).SlopeY := dy / ( 2 * D );
      end;
      // TODO GlobalUnlock( TAniFigure( Figure ).PathHandle );
      if Assigned( TAniFigure( Figure ).OnPathStep ) then
        TAniFigure( Figure ).OnPathStep( TAniFigure( Figure ), TAniFigure( Figure ).DestX, TAniFigure( Figure ).DestY );
    end
    else
    begin
      TAniFigure( Figure ).GotPath := False;
      if Assigned( TAniFigure( Figure ).OnNoPath ) then
        TAniFigure( Figure ).OnNoPath( TAniFigure( Figure ) );
    end;
  end
  else
  begin
    TAniFigure( Figure ).GotPath := False;
    if Assigned( TAniFigure( Figure ).OnNoPath ) then
      TAniFigure( Figure ).OnNoPath( TAniFigure( Figure ) );
  end;
end;

procedure TAniView.MoveFigure( Figure : TCustomAniFigure );
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
  Dx := TAniFigure( Figure ).DestX - TAniFigure( Figure ).StepX;
  dy := 2 * ( TAniFigure( Figure ).DestY - TAniFigure( Figure ).StepY );
  dZ := TAniFigure( Figure ).DestZ - TAniFigure( Figure ).StepZ;
  D := sqrt( sqr( Dx ) + sqr( dy ) + sqr( dZ ) );
  if ( ( D <= 0 ) or ( D <= TAniFigure( Figure ).Speed ) ) and TAniFigure( Figure ).Terminal then
  begin
    if ( TAniFigure( Figure ).GotPath ) then
    begin
      DestZ := TAniFigure( Figure ).StepZ;
      if ( TAniFigure( Figure ).PathStep >= TAniFigure( Figure ).PathCount ) then
      begin
        DestX := TAniFigure( Figure ).DestX;
        DestY := TAniFigure( Figure ).DestY;
        TAniFigure( Figure ).Moving := False;
        if Assigned( TAniFigure( Figure ).OnStop ) then
          TAniFigure( Figure ).OnStop( TAniFigure( Figure ) );
      end
      else
      begin
        // TODO Point := GlobalLock( Figure.PathHandle );
        Inc( Point, TAniFigure( Figure ).PathStep );
        S := TAniFigure( Figure ).Speed;
        repeat
          if ( TAniFigure( Figure ).PathStep >= TAniFigure( Figure ).PathCount ) then
            Break;
          StepX := TAniFigure( Figure ).DestX;
          StepY := TAniFigure( Figure ).DestY;
          S := S - D;
          TAniFigure( Figure ).DestX := Point^.X;
          TAniFigure( Figure ).DestY := Point^.Y;
          Dx := TAniFigure( Figure ).DestX - StepX;
          dy := 2 * ( TAniFigure( Figure ).DestY - StepY );
          D := sqrt( sqr( Dx ) + sqr( dy ) );
          Inc( TAniFigure( Figure ).PathStep );
          Inc( Point );
        until ( S <= D );
        // TODO GlobalUnlock( TAniFigure( Figure ).PathHandle );
        if ( S <= D ) then
        begin
          TAniFigure( Figure ).SlopeX := Dx / D;
          TAniFigure( Figure ).SlopeY := dy / ( 2 * D );
          DestX := StepX + S * TAniFigure( Figure ).SlopeX;
          DestY := StepY + S * TAniFigure( Figure ).SlopeY;
          DestZ := TAniFigure( Figure ).DestZ;
          if Assigned( TAniFigure( Figure ).OnPathStep ) then
            TAniFigure( Figure ).OnPathStep( TAniFigure( Figure ), TAniFigure( Figure ).DestX, TAniFigure( Figure ).DestY );
        end
        else
        begin
          DestX := TAniFigure( Figure ).DestX;
          DestY := TAniFigure( Figure ).DestY;
          TAniFigure( Figure ).Moving := False;
          if Assigned( TAniFigure( Figure ).OnStop ) then
            TAniFigure( Figure ).OnStop( TAniFigure( Figure ) );
        end;
      end;
    end
    else
    begin
      DestX := TAniFigure( Figure ).DestX;
      DestY := TAniFigure( Figure ).DestY;
      DestZ := TAniFigure( Figure ).DestZ;
      TAniFigure( Figure ).Moving := False;
      if Assigned( TAniFigure( Figure ).OnStop ) then
        TAniFigure( Figure ).OnStop( Figure );
    end;
  end
  else
  begin
    DestX := TAniFigure( Figure ).StepX + TAniFigure( Figure ).Speed * TAniFigure( Figure ).SlopeX;
    DestY := TAniFigure( Figure ).StepY + TAniFigure( Figure ).Speed * TAniFigure( Figure ).SlopeY;
    DestZ := TAniFigure( Figure ).StepZ + TAniFigure( Figure ).Speed * TAniFigure( Figure ).SlopeZ;
  end;

  if Assigned( FMap ) then
  begin
    //Collision detection
    // TODO GridBase := GlobalLock( FMap.MapData );
    GridLoc := GridBase;
    if ( TAniFigure( Figure ).MapOffset >= 0 ) then
    begin
      //Remove figure from collision chain
      Inc( GridLoc, ( TAniFigure( Figure ).MapOffset ) );
      NextFigure := GridLoc^.Figure;

      RemX := TAniFigure( Figure ).PrevX mod FMap.TileWidth;
      RemY := FMap.TileHeight - ( TAniFigure( Figure ).PrevY mod FMap.TileHeight ) - 1;
      BitMask := 1 shl ( ( RemY div FMap.StripHeight ) * 4 + ( RemX div FMap.StripWidth ) );
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
        GridLoc^.Figure := TAniFigure( Figure ).NextInTile;
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
      TAniFigure( Figure ).NextInTile := nil;
    end
    else
    begin
      OldTriggerID := 0;
      OldFilterID := 0;
    end;

    Dx := DestX - TAniFigure( Figure ).StepX;
    dy := 2 * ( DestY - TAniFigure( Figure ).StepY );
    dZ := DestZ - TAniFigure( Figure ).StepZ;
    if ( Dx <> 0 ) or ( dy <> 0 ) then
    begin
      D := sqrt( sqr( Dx ) + sqr( dy ) );
      T := 1;
      //Collision with map boundaries
      if ( Dx <> 0 ) then
      begin
        edge := TAniFigure( Figure ).Radius + FMap.TileWidth;
        if ( DestX < edge ) then
        begin
          T1 := ( edge - TAniFigure( Figure ).StepX ) / Dx;
          if ( T1 < T ) then
            T := T1;
        end
        else
        begin
          edge := FMap.BitWidth - TAniFigure( Figure ).Radius - FMap.TileWidth;
          if ( DestX > edge ) then
          begin
            T1 := ( edge - TAniFigure( Figure ).StepX ) / Dx;
            if ( T1 < T ) then
              T := T1;
          end;
        end;
      end;
      if ( dy <> 0 ) then
      begin
        edge := TAniFigure( Figure ).Radius + FMap.TileHeight;
        if ( DestY < edge ) then
        begin
          T1 := ( edge - TAniFigure( Figure ).StepY ) / dy;
          if ( T1 < T ) then
            T := T1;
        end
        else
        begin
          edge := FMap.BitHeight - TAniFigure( Figure ).Radius - FMap.TileHeight;
          if ( DestY > edge ) then
          begin
            T1 := ( edge - TAniFigure( Figure ).StepY ) / dy;
            if ( T1 < T ) then
              T := T1;
          end;
        end;
      end;
      if ( T < 1 ) then
      begin
        if Assigned( TAniFigure( Figure ).OnCollideBoundary ) then
          TAniFigure( Figure ).OnCollideBoundary( TAniFigure( Figure ) );
      end;

      if Assigned( TAniFigure( Figure ).OnCollideFigure ) then
      begin
        //Collisions with other figures
        InitSeq := False;
        R := TAniFigure( Figure ).Radius + FMaxCollisionRadius;
        G := R / D;
        Gx := Round( G * Dx );
        Gy := Round( G * dy );
        Gx2 := Gx div 2;
        Gy2 := Gy div 2;

        X := Round( TAniFigure( Figure ).StepX );
        Y := Round( TAniFigure( Figure ).StepY );
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
          TileRow := p[ Top ].Y div FMap.TileHeight;
        ScanY := ( TileRow + 1 ) * FMap.TileHeight;
        TileRowOffset := TileRow * FMap.Width;

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
        while ( ScanY < FMap.BitHeight ) do
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
              XL := XL2 div FMap.TileWidth;
            end
            else
              XL := ( XL1 + dXL * ( ScanY - YL1 ) div dYL ) div FMap.TileWidth;
          end
          else
            XL := ( XL1 + dXL * ( ScanY - FMap.TileHeight - YL1 ) div dYL ) div FMap.TileWidth;

          if ( ModeR ) then
          begin
            if ( ScanY > p[ Right ].Y ) then
            begin
              XR1 := p[ Bottom ].X;
              YR1 := p[ Bottom ].Y;
              dXR := XR2 - XR1;
              dYR := YR2 - YR1;
              ModeR := False;
              XR := XR2 div FMap.TileWidth;
            end
            else
              XR := ( XR1 + dXR * ( ScanY - YR1 ) div dYR ) div FMap.TileWidth;
          end
          else
            XR := ( XR1 + dXR * ( ScanY - FMap.TileHeight - YR1 ) div dYR ) div FMap.TileWidth;

          if ( XL < 0 ) then
            XL := 0;
          if ( XR >= FMap.Width ) then
            XR := FMap.Width - 1;
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
                B := 2 * ( Dx * ( TAniFigure( Figure ).StepX - NextFigure.StepX ) + 2 * dy * ( TAniFigure( Figure ).StepY - NextFigure.StepY ) );
                C := sqr( TAniFigure( Figure ).StepX - NextFigure.StepX ) + 4 * sqr( TAniFigure( Figure ).StepY - NextFigure.StepY ) -
                  sqr( TAniFigure( Figure ).Radius + NextFigure.Radius );
                Q := sqr( B ) - 4 * A * C;
                if ( Q >= 0 ) then
                begin
                  T1 := ( -B - Sqrt( Q ) ) / ( 2 * A );
                  if ( T1 < 1 ) and ( T1 >= -Tol ) then
                  begin
                    Stop := False;
                    TAniFigure( Figure ).OnCollideFigure( TAniFigure( Figure ), NextFigure, Stop );
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
          Inc( TileRowOffset, FMap.Width );
          Inc( ScanY, FMap.TileHeight );
        end;
      end
      else
        InitSeq := True;

      //Collisions with map objects
      if Assigned( TAniFigure( Figure ).OnCollideItem ) then
      begin
        T2 := T;
        R := TAniFigure( Figure ).Radius;
        G := R / D;
        Gx := Round( G * Dx );
        Gy := Round( G * dy );
        Gx2 := Gx div 2;
        Gy2 := Gy div 2;

        X := Round( TAniFigure( Figure ).StepX );
        Y := Round( TAniFigure( Figure ).StepY );
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
          TileRow := p[ Top ].Y div FMap.TileHeight;
        ScanY := ( TileRow + 1 ) * FMap.TileHeight;
        TileRowOffset := TileRow * FMap.Width;

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
        while ( ScanY < FMap.BitHeight ) do
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
              XL := XL2 div FMap.TileWidth;
            end
            else
              XL := ( XL1 + dXL * ( ScanY - YL1 ) div dYL ) div FMap.TileWidth;
          end
          else
            XL := ( XL1 + dXL * ( ScanY - FMap.TileHeight - YL1 ) div dYL ) div FMap.TileWidth;

          if ( ModeR ) then
          begin
            if ( ScanY > p[ Right ].Y ) then
            begin
              XR1 := p[ Bottom ].X;
              YR1 := p[ Bottom ].Y;
              dXR := XR2 - XR1;
              dYR := YR2 - YR1;
              ModeR := False;
              XR := XR2 div FMap.TileWidth;
            end
            else
              XR := ( XR1 + dXR * ( ScanY - YR1 ) div dYR ) div FMap.TileWidth;
          end
          else
            XR := ( XR1 + dXR * ( ScanY - FMap.TileHeight - YR1 ) div dYR ) div FMap.TileWidth;

          if ( XL < 0 ) then
            XL := 0;
          if ( XR >= FMap.Width ) then
            XR := FMap.Width - 1;
          GridLoc := GridBase;
          Inc( GridLoc, TileRowOffset );
          Inc( GridLoc, XL );
          for X := XL to XR do
          begin
            if TAniFigure( Figure ).UseLineOfSight then
              CollisionMask := GridLoc^.LineOfSightMask
            else
              CollisionMask := GridLoc^.CollisionMask;
            if ( CollisionMask <> 0 ) then
            begin
              //(*whew*) We've established there could be a collision, now let's find out
              //if there really is, and if so, where?
              for j := 1 to 4 do
              begin
                cy := ScanY - ( j - 1 ) * FMap.StripHeight - ( FMap.StripHeight shr 1 );
                for i := 1 to 4 do
                begin
                  if ( ( CollisionMask and 1 ) = 1 ) then
                  begin
                    cx := X * FMap.TileWidth + ( i - 1 ) * FMap.StripWidth + ( FMap.StripWidth shr 1 );
                    A := sqr( Dx ) + sqr( dy );
                    B := 2 * ( Dx * ( TAniFigure( Figure ).StepX - cx ) + 2 * dy * ( TAniFigure( Figure ).StepY - cy ) );
                    C := sqr( TAniFigure( Figure ).StepX - cx ) + 4 * sqr( TAniFigure( Figure ).StepY - cy ) -
                      sqr( TAniFigure( Figure ).Radius +
                      ( FMap.StripWidth shr 1 ) );
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
          Inc( TileRowOffset, FMap.Width );
          Inc( ScanY, FMap.TileHeight );
        end;

        if ( T2 < T ) then
        begin
          Stop := False;
          TAniFigure( Figure ).OnCollideItem( TAniFigure( Figure ), Stop );
          if ( Stop ) then
            T := T2;
        end;
      end;

      if ( not TAniFigure( Figure ).Moved ) then
      begin //This clause allows an event to call SetPos
        //Figure has hit something, perform movement calculations
        if ( T < 1 ) then
        begin
          TAniFigure( Figure ).Moving := False;
          DestX := TAniFigure( Figure ).StepX + Dx * T;
          DestY := TAniFigure( Figure ).StepY + dy * T / 2;
          DestZ := TAniFigure( Figure ).StepZ + dZ * T;
          if Assigned( TAniFigure( Figure ).OnStop ) then
            TAniFigure( Figure ).OnStop( Figure );
        end;
      end;
    end;

    if ( not TAniFigure( Figure ).Moved ) then
    begin //This if allows an event to call SetPos
      TAniFigure( Figure ).StepX := DestX;
      TAniFigure( Figure ).StepY := DestY;
      TAniFigure( Figure ).StepZ := DestZ;
      TAniFigure( Figure ).PrevX := TAniFigure( Figure ).FX;
      TAniFigure( Figure ).PrevY := TAniFigure( Figure ).FY;
      TAniFigure( Figure ).PrevZ := TAniFigure( Figure ).FZ;
      TAniFigure( Figure ).FX := Round( DestX );
      TAniFigure( Figure ).FY := Round( DestY );
      TAniFigure( Figure ).FZ := Round( DestZ );
      TAniFigure( Figure ).MapOffset := ( TAniFigure( Figure ).FY div FMap.TileHeight ) * FMap.Width + ( TAniFigure( Figure ).FX div FMap.TileWidth );
      GridLoc := GridBase;
      Inc( GridLoc, ( TAniFigure( Figure ).MapOffset ) );
      NextFigure := GridLoc^.Figure;
      GridLoc^.Figure := Figure;
      TAniFigure( Figure ).NextInTile := NextFigure;
      TAniFigure( Figure ).Zone := GridLoc^.Zone[ 0 ];
      TAniFigure( Figure ).Tile := PGridInfo( GridLoc );
      if ( GridLoc^.TriggerID <> OldTriggerID ) then
      begin
        if Assigned( TAniFigure( Figure ).OnTrigger ) then
        begin
          if ( GridLoc^.TriggerID = 0 ) or ( GridLoc^.TriggerMask = $FFFF ) then
          begin
            TAniFigure( Figure ).OnTrigger( TAniFigure( Figure ), GridLoc^.TriggerID, OldTriggerID );
          end
          else
          begin
            RemX := TAniFigure( Figure ).FX div FMap.TileWidth;
            RemY := FMap.TileHeight - ( TAniFigure( Figure ).FY mod FMap.TileHeight );
            BitMask := 1 shl ( ( RemY div FMap.StripHeight ) * 4 + ( RemX div FMap.StripWidth ) );
            if ( GridLoc^.TriggerMask and BitMask ) > 0 then
              TAniFigure( Figure ).OnTrigger( TAniFigure( Figure ), GridLoc^.TriggerID, OldTriggerID );
          end;

        end;
      end;
      if ( GridLoc^.FilterID <> OldFilterID ) then
      begin
        if Assigned( TAniFigure( Figure ).OnFilter ) then
        begin
          if ( GridLoc^.FilterID = 0 ) or ( GridLoc^.FilterMask = $FFFF ) then
          begin
            TAniFigure( Figure ).OnFilter( TAniFigure( Figure ), GridLoc^.FilterID, OldFilterID );
          end
          else
          begin
            RemX := TAniFigure( Figure ).FX div FMap.TileWidth;
            RemY := FMap.TileHeight - ( TAniFigure( Figure ).FY mod FMap.TileHeight );
            BitMask := 1 shl ( ( RemY div FMap.StripHeight ) * 4 + ( RemX div FMap.StripWidth ) );
            if ( GridLoc^.FilterMask and BitMask ) > 0 then
              TAniFigure( Figure ).OnFilter( TAniFigure( Figure ), GridLoc^.FilterID, OldFilterID );
          end;
        end;
      end;
   //   Figure.FPrevX := Figure.FX;
   //   Figure.FPrevY := Figure.FY;
   //   Figure.FPrevZ := Figure.FZ;
    end;
    // TODO GlobalUnlock( FMap.FMapData );
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
    // TODO GridBase := GlobalLock( FMap.FMapData );
    try
      if ( Dx <> 0 ) or ( dy <> 0 ) then
      begin
        D := sqrt( sqr( Dx ) + sqr( dy ) );
        T := 1;
        //Collision with map boundaries
        if ( Dx <> 0 ) then
        begin
          edge := Radius + FMap.TileWidth;
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
            edge := FMap.BitWidth - Radius - FMap.TileWidth;
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
          edge := Radius + FMap.TileHeight;
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
            edge := FMap.BitHeight - Radius - FMap.TileHeight;
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
          TileRow := p[ Top ].Y div FMap.TileHeight;
        ScanY := ( TileRow + 1 ) * FMap.TileHeight;
        TileRowOffset := TileRow * FMap.Width;

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
        while ( ScanY < FMap.BitHeight ) do
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
              XL := XL2 div FMap.TileWidth;
            end
            else
              XL := ( XL1 + dXL * ( ScanY - YL1 ) div dYL ) div FMap.TileWidth;
          end
          else
            XL := ( XL1 + dXL * ( ScanY - FMap.TileHeight - YL1 ) div dYL ) div FMap.TileWidth;

          if ( ModeR ) then
          begin
            if ( ScanY > p[ Right ].Y ) then
            begin
              XR1 := p[ Bottom ].X;
              YR1 := p[ Bottom ].Y;
              dXR := XR2 - XR1;
              dYR := YR2 - YR1;
              ModeR := False;
              XR := XR2 div FMap.TileWidth;
            end
            else
              XR := ( XR1 + dXR * ( ScanY - YR1 ) div dYR ) div FMap.TileWidth;
          end
          else
            XR := ( XR1 + dXR * ( ScanY - FMap.TileHeight - YR1 ) div dYR ) div FMap.TileWidth;

          if ( XL < 0 ) then
            XL := 0;
          if ( XR >= FMap.Width ) then
            XR := FMap.Width - 1;
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
          Inc( TileRowOffset, FMap.Width );
          Inc( ScanY, FMap.TileHeight );
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
          TileRow := p[ Top ].Y div FMap.TileHeight;
        ScanY := ( TileRow + 1 ) * FMap.TileHeight;
        TileRowOffset := TileRow * FMap.Width;

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
        while ( ScanY < FMap.BitHeight ) do
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
              XL := XL2 div FMap.TileWidth;
            end
            else
              XL := ( XL1 + dXL * ( ScanY - YL1 ) div dYL ) div FMap.TileWidth;
          end
          else
            XL := ( XL1 + dXL * ( ScanY - FMap.TileHeight - YL1 ) div dYL ) div FMap.TileWidth;

          if ( ModeR ) then
          begin
            if ( ScanY > p[ Right ].Y ) then
            begin
              XR1 := p[ Bottom ].X;
              YR1 := p[ Bottom ].Y;
              dXR := XR2 - XR1;
              dYR := YR2 - YR1;
              ModeR := False;
              XR := XR2 div FMap.TileWidth;
            end
            else
              XR := ( XR1 + dXR * ( ScanY - YR1 ) div dYR ) div FMap.TileWidth;
          end
          else
            XR := ( XR1 + dXR * ( ScanY - FMap.TileHeight - YR1 ) div dYR ) div FMap.TileWidth;

          if ( XL < 0 ) then
            XL := 0;
          if ( XR >= FMap.Width ) then
            XR := FMap.Width - 1;
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
                cy := ScanY - ( j - 1 ) * FMap.StripHeight - ( FMap.StripHeight shr 1 );
                for i := 1 to 4 do
                begin
                  if ( ( CollisionMask and 1 ) = 1 ) then
                  begin
                    cx := X * FMap.TileWidth + ( i - 1 ) * FMap.StripWidth + ( FMap.StripWidth shr 1 );
                    A := sqr( Dx ) + sqr( dy );
                    B := 2 * ( Dx * ( SrcX - cx ) + 2 * dy * ( SrcY - cy ) );
                    C := sqr( SrcX - cx ) + 4 * sqr( SrcY - cy ) -
                      sqr( Radius +
                      ( FMap.StripWidth shr 1 ) );
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
          Inc( TileRowOffset, FMap.Width );
          Inc( ScanY, FMap.TileHeight );
        end;
      end;
    finally
      // TODO GlobalUnlock( FMap.FMapData );
    end;
  end;
  result := true;
end;

procedure TAniView.DisableFigure( Figure : TCustomAniFigure );
var
  GridBase, GridLoc : ^GridInfo;
  NextFigure, TempFigure : TAniFigure;
begin
  if Assigned( FMap ) then
  begin
    //Collision detection
    // TODO GridBase := GlobalLock( FMap.FMapData );
    GridLoc := GridBase;
    if ( TAniFigure( Figure ).MapOffset >= 0 ) then
    begin
      //Remove figure from collision chain
      Inc( GridLoc, ( TAniFigure( Figure ).MapOffset ) );
      NextFigure := GridLoc^.Figure;
      if ( NextFigure = Figure ) then
      begin
        GridLoc^.Figure := TAniFigure( Figure ).NextInTile;
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
      TAniFigure( Figure ).NextInTile := nil;
    end;
    // TODO GlobalUnlock( FMap.FMapData );
  end;
  TAniFigure( Figure ).ViewEnabled := False;
end;

procedure TAniView.TransFigure( Figure : TCustomAniFigure );
var
  GridBase, GridLoc : ^GridInfo;
  OldTriggerID : SmallInt;
  OldFilterID : SmallInt;
  NextFigure, TempFigure : TAniFigure;
  RemX, RemY : integer;
  BitMask : word;
begin
  TAniFigure( Figure ).Moved := False;
  if Assigned( FMap ) then
  begin
    // TODO GridBase := GlobalLock( FMap.FMapData );
    GridLoc := GridBase;
    if ( TAniFigure( Figure ).MapOffset >= 0 ) then
    begin
      //Remove figure from collision chain
      Inc( GridLoc, ( TAniFigure( Figure ).MapOffset ) );
      NextFigure := GridLoc^.Figure;
      RemX := TAniFigure( Figure ).PrevX mod FMap.TileWidth;
      RemY := FMap.TileHeight - ( TAniFigure( Figure ).PrevY mod FMap.TileHeight ) - 1;
      BitMask := 1 shl ( ( RemY div FMap.StripHeight ) * 4 + ( RemX div FMap.StripWidth ) );
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
        GridLoc^.Figure := TAniFigure( Figure ).NextInTile;
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
      TAniFigure( Figure ).NextInTile := nil;
    end
    else
    begin
      OldTriggerID := 0;
      OldFilterID := 0;
    end;

    TAniFigure( Figure ).MapOffset := ( TAniFigure( Figure ).FY div FMap.TileHeight ) * FMap.Width + ( TAniFigure( Figure ).FX div FMap.TileWidth );
    if ( TAniFigure( Figure ).MapOffset < 0 ) then
      TAniFigure( Figure ).MapOffset := 0;
    if TAniFigure( Figure ).MapOffset >= FMap.Height * FMap.Width then
      TAniFigure( Figure ).MapOffset := FMap.Height * FMap.Width - 1;
    GridLoc := GridBase;
    Inc( GridLoc, ( TAniFigure( Figure ).MapOffset ) );
    NextFigure := GridLoc^.Figure;
    GridLoc^.Figure := Figure;
    TAniFigure( Figure ).NextInTile := NextFigure;
    TAniFigure( Figure ).Zone := GridLoc^.Zone[ 0 ];
    TAniFigure( Figure ).Tile := PGridInfo( GridLoc );
    if ( GridLoc^.TriggerID <> OldTriggerID ) then
    begin
      if Assigned( TAniFigure( Figure ).OnTrigger ) then
      begin
        if ( GridLoc^.TriggerID = 0 ) or ( GridLoc^.TriggerMask = $FFFF ) then
        begin
          TAniFigure( Figure ).OnTrigger( TAniFigure( Figure ), GridLoc^.TriggerID, OldTriggerID );
        end
        else
        begin
          RemX := TAniFigure( Figure ).FX div FMap.TileWidth;
          RemY := FMap.TileHeight - ( TAniFigure( Figure ).FY mod FMap.TileHeight );
          BitMask := 1 shl ( ( RemY div FMap.StripHeight ) * 4 + ( RemX div FMap.StripWidth ) );
          if ( GridLoc^.TriggerMask and BitMask ) > 0 then
            TAniFigure( Figure ).OnTrigger( TAniFigure( Figure ), GridLoc^.TriggerID, OldTriggerID );
        end;

      end;
    end;
    if ( GridLoc^.FilterID <> OldFilterID ) then
    begin
      if Assigned( TAniFigure( Figure ).OnFilter ) then
      begin
        if ( GridLoc^.FilterID = 0 ) or ( GridLoc^.FilterMask = $FFFF ) then
        begin
          TAniFigure( Figure ).OnFilter( TAniFigure( Figure ), GridLoc^.FilterID, OldFilterID );
        end
        else
        begin
          RemX := TAniFigure( Figure ).FX div FMap.TileWidth;
          RemY := FMap.TileHeight - ( TAniFigure( Figure ).FY mod FMap.TileHeight );
          BitMask := 1 shl ( ( RemY div FMap.StripHeight ) * 4 + ( RemX div FMap.StripWidth ) );
          if ( GridLoc^.FilterMask and BitMask ) > 0 then
            TAniFigure( Figure ).OnFilter( TAniFigure( Figure ), GridLoc^.FilterID, OldFilterID );
        end;
      end;
    end;
    TAniFigure( Figure ).PrevX := TAniFigure( Figure ).FX;
    TAniFigure( Figure ).PrevY := TAniFigure( Figure ).FY;
    TAniFigure( Figure ).PrevZ := TAniFigure( Figure ).FZ;
    // TODO GlobalUnlock( FMap.MapData );
  end;
end;

procedure TAniView.ComputeLight( Figure : TCustomAniFigure );
var
  i, j : Integer;
  X1, Y1, Z1 : Longint;
  IL1, D : Double;
  R1, G1, B1 : Double;
  RL, GL, BL : Integer;
  Test : TLightZone;
begin
  TAniFigure( Figure ).LightComputed := FrameCount;
  R1 := FMap.LightR;
  G1 := FMap.LightG;
  B1 := FMap.LightB;
  if ( TZone( FMap.Zones.Items[ TAniFigure( Figure ).Zone ] ) is TLightZone ) then
  begin
    j := 0;
    for i := 0 to TLightZone( FMap.Zones.Items[ TAniFigure( Figure ).Zone ] ).OverlapZones.Count - 1 do
    begin
      Test := TLightZone( FMap.Zones.Items[ TAniFigure( Figure ).Zone ] ).OverlapZones.Items[ i ];
      X1 := sqr( Test.FlickerX[ Test.State ] - TAniFigure( Figure ).FX );
      Y1 := sqr( ( Test.FlickerY[ Test.State ] - TAniFigure( Figure ).FY ) * 2 );
      Z1 := sqr( Test.FlickerZ[ Test.State ] - ( TAniFigure( Figure ).Height div 2 ) );
      D := sqrt( X1 + Y1 + Z1 ) / Test.FlickerRadius[ Test.State ];
      if D <= 1 then
      begin
        if FMap.LineOfSight( Test.FlickerX[ Test.State ], Test.FlickerY[ Test.State ], TAniFigure( Figure ).FX, TAniFigure( Figure ).FY ) then
        begin
          // TODO
          {RL := Test.Color and $FF;
          GL := Test.Color and $FF00 shr 8;
          BL := Test.Color and $FF0000 shr 16;}
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
  TAniFigure( Figure ).LightR := Round( R1 );
  TAniFigure( Figure ).LightG := Round( G1 );
  TAniFigure( Figure ).LightB := Round( B1 );
  if ( R1 >= G1 ) and ( R1 >= B1 ) then
    TAniFigure( Figure ).LightIndex := R1 / 255
  else if ( G1 >= B1 ) then
    TAniFigure( Figure ).LightIndex := G1 / 255
  else
    TAniFigure( Figure ).LightIndex := B1 / 255;
end;

procedure TAniView.DrawFigure( Figure : TCustomAniFigure );
begin
  TAniFigure( Figure ).PosX := TAniFigure( Figure ).FX - TAniFigure( Figure ).CenterX - OffsetX;
  // tODO
  {if ( TAniFigure( Figure ).PosX + TAniFigure( Figure ).Width < Left ) or ( TAniFigure( Figure ).PosX >= Left + Width ) then
    exit;}
  TAniFigure( Figure ).PosY := TAniFigure( Figure ).FY - TAniFigure( Figure ).CenterY - TAniFigure( Figure ).FZ - OffsetY;
  if ( TAniFigure( Figure ).UseLighting ) and FMap.UseLighting then
  begin
    ComputeLight( Figure );
  end
  else
  begin
    TAniFigure( Figure ).LightR := 255;
    TAniFigure( Figure ).LightG := 255;
    TAniFigure( Figure ).LightB := 255;
  end;
  TAniFigure( Figure ).OnScreen := true;
  TAniFigure( Figure ).Render;
end;

procedure TAniView.SetInterval( PInterval : Word );
begin
  FInterval := PInterval;
  // TODO
  { if Assigned( Timer ) then
    Timer.Interval := PInterval;}
end;

procedure TAniView.SetActive( VActive : Boolean );
begin
  FActive := VActive;
  if FActive then
  begin
    // TODO
    { if not Assigned( Timer ) then
    begin
      Timer := TAniTimer.create( nil );
      Timer.Interval := FInterval;
      Timer.TimerPriority := tpNormal;
      Timer.Resolution := 1;
    end;
    Timer.OnTimer := FDrawFrame;
    Timer.enabled := FActive; }
  end
  else
  begin
    // TODO
    { if Assigned( Timer ) then
    begin
      Timer.OnTimer := nil;
      Timer.enabled := FActive;
    end; }
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
    // TODO RepaintCode := DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT
  else
    // TODO RepaintCode := DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT;
end;

procedure TAniView.SetAutoTransparentMask( const Value : PSDL_Surface );
begin
  if Assigned( Value ) then
  begin
    XRayImage := nil;
    { TODO
    XRayImage := DDGetImage( lpDD, Value, clBlack, True );
    XRayWidth := Value.width;
    XRayHeight := Value.Height;}
  end
  else
  begin
    { TODO
    XRayImage := nil;
    XRayWidth := 0;
    XRayHeight := 0; }
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
          TAniFigure( FigureList.Items[ i ] ).Distance := sqrt( sqr( Dx ) + sqr( 2 * dy ) );
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

end.

