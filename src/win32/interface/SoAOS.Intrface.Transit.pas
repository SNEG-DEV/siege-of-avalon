unit SoAOS.Intrface.Transit;
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

  Description: Fast transit screen - partly refactored Transit.pas

  Requires: Delphi 10.3.3 or later

  Revision History:
  - 13 Jul 2003 - DL: Initial Upload to CVS
  - 10 Mar 2019 - SN: Forked on GitHSub
  see git repo afterwards

*)

interface

uses
  System.Types,
  System.Classes,
  System.Generics.Collections,
  Vcl.Controls,
  Vcl.Forms,
//  Winapi.DirectDraw,
  DirectX,
  Display,
  SoAOS.Animation,
  MMTimer;

type
  TTransit = class;

  TTransitPoint = class( TObject )
  strict private
    FRegion: TRect;
    FBlend: Integer;
    FImage: IDirectDrawSurface;
    FIndex: Integer;
    FMapName: string;
    FEnabled: boolean;
    FMapResult: string;
  public
    constructor Create(const AOwner: TTransit; const x, y: Integer; const Image, MapName: string);
    constructor CreateDefault(const AOwner: TTransit; const x, y, w, h: Integer);
    destructor Destroy; override;
    procedure Reset;
    procedure Update;
    procedure ShowInRed;
    property Index: integer read FIndex;
    property MapName: string read FMapName;
    property MapResult: string read FMapResult;
    property Enabled: Boolean read FEnabled;
    property Region: TRect read FRegion;
    property Image: IDirectDrawSurface read FImage;
    property Blend: Integer read FBlend;
  end;

  THotspot = class( TObject )
  strict private
    FRegion : TRect;
    FGrayImage : IDirectDrawSurface;
    FRedImage : IDirectDrawSurface;
    FEnabled : boolean;
    FDest : TObjectList<TTransitPoint>;
  public
    constructor Create( const AOwner : TTransit; const x, y: Integer; const redImage, grayImage: string );
    procedure CreateOuterBailey( const AOwner : TTransit );
    procedure CreateInnerBailey( const AOwner : TTransit );
    procedure CreateOuterKeep( const AOwner : TTransit );
    procedure CreateInnerKeep( const AOwner : TTransit );
    procedure CreatePeasantsBailey( const AOwner : TTransit );
    procedure CreateSouthgate( const AOwner : TTransit );
    destructor Destroy; override;
    property Enabled: Boolean read FEnabled;
    property Region: TRect read FRegion;
    property Dest: TObjectList<TTransitPoint> read FDest;
    property RedImage: IDirectDrawSurface read FRedImage;
    property GrayImage: IDirectDrawSurface read FGrayImage;
  end;

  TTransit = class( TDisplay )
  private
    FOnClose : TNotifyEvent;
    DXBack : IDirectDrawSurface;
    DXDirty : IDirectDrawSurface;
    Background : IDirectDrawSurface;
    MouseOverBack : boolean;
    HotspotList : TObjectList<THotspot>;
    Selected : THotspot;
    Timer : TAniTimer;
    Locked : boolean;
    DirtyRect : TRect;
    MsgBack : string;
    MsgProceed : string;
    procedure TimerEvent( Sender : TObject );
    procedure TimerCloseEvent( Sender : TObject );
    function MapInList( const MapName : string ) : integer;
    procedure FormMouseDown( Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer );
    procedure FormMouseMove( Sender : TObject; Shift : TShiftState; X, Y : Integer );
  protected
    procedure MouseDown( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer ); override;
    procedure MouseMove( Sender : TObject;
      Shift : TShiftState; X, Y, GridX, GridY : Integer ); override;
  public
    DefaultName : string;
    DefaultMap : string;
    DefaultScene : string;
    DefaultStartingPoint : string;
    DefaultTransition : string;
    Targets : string;
    DefaultTransit : TTransitPoint;
    MapsAvailable : TStringList;
    frmMain : TForm;
    ResultIndex : integer;
    ResultMapName : string;
    Cancelled : boolean;
    procedure Paint; override;
    procedure Init; override;
    procedure Release; override;
    property OnClose : TNotifyEvent read FOnClose write FOnClose;
  end;

implementation

uses
  SoAOS.Types,
  SoAOS.Graphics.Draw,
  SoAOS.Graphics.Types,
  SoAOS.Intrface.Text,
  AniDemo,
  System.SysUtils,
  Gametext,
  Engine,
  DXEffects,
  DXUtil,
  DFX,
  LogFile;

{ TTransit }

procedure TTransit.Init;
const
  FailName : string = 'TTransit.Init';
var
  NewHotspot : THotspot;
begin
  Log.DebugLog(FailName);
  try
    Locked := false;
    ResultIndex := -1;
    Cancelled := false;
    inherited;

    frmMain.OnMouseDown := FormMouseDown;
    frmMain.OnMouseMove := FormMouseMove;

    X1 := 0;
    Y1 := 0;
    X2 := 800;
    Y2 := 600;

    ExText.Open( 'Transit' );
    MsgProceed := ExText.GetText( 'Message0' );
    MsgBack := ExText.GetText( 'Message1' );
    ExText.Close;

    MouseOverBack := false;
    MouseCursor.Cleanup;
    MouseCursor.PlotDirty := false;
    Selected := nil;
    Timer := TAniTimer.create( nil );
    Timer.enabled := false;
    Timer.Interval := 25;
    Timer.OnTimer := TimerEvent;

    HotspotList := TObjectList<THotspot>.Create;

    NewHotspot := THotspot.Create( Self, 261, 301, 'InnerBaileyAvailable.bmp', 'InnerBaileyUnavailable.bmp' );
    NewHotspot.CreateInnerBailey( Self );
    HotspotList.Add( NewHotspot );

    NewHotspot := THotspot.Create( Self, 276, 361, 'OuterKeepAvailable.bmp', '' );
    NewHotspot.CreateOuterKeep( Self );
    HotspotList.Add( NewHotspot );

    NewHotspot := THotspot.Create( Self, 266, 501, 'SouthGateAvailable.bmp', 'SouthGateUnavailable.bmp' );
    NewHotspot.CreateSouthGate( Self );
    HotspotList.Add( NewHotspot );

    NewHotspot := THotspot.Create( Self, 259, 463, 'OuterBaileyAvailable.bmp', '' );
    NewHotspot.CreateOuterBailey( Self );
    HotspotList.Add( NewHotspot );

    NewHotspot := THotspot.Create( Self, 250, 251, 'InnerKeepAvailable.bmp', 'InnerKeepUnavailable.bmp' );
    NewHotspot.CreateInnerKeep( Self );
    HotspotList.Add( NewHotspot );

    NewHotspot := THotspot.Create( Self, 373, 464, 'Peasant''sBaileyAvailable.bmp', 'Peasant''sBaileyUnavailable.bmp' );
    NewHotspot.CreatePeasantsBailey( Self );
    HotspotList.Add( NewHotspot );

    NewHotspot := THotspot.Create( Self, 377, 295, '', 'CattleBaileyOffLimits.bmp' );
    HotspotList.Add( NewHotspot );

    NewHotspot := THotspot.Create( Self, 263, 191, '', 'NobleBaileyOffLimits.bmp' );
    HotspotList.Add( NewHotspot );

    NewHotspot := THotspot.Create( Self, 158, 197, '', 'TrainingBaileyOffLimits.bmp' );
    HotspotList.Add( NewHotspot );

    NewHotspot := THotspot.Create( Self, 159, 299, '', 'Soldier''sBaileyOffLimits.bmp' );
    HotspotList.Add( NewHotspot );

    NewHotspot := THotspot.Create( Self, 154, 464, '', 'Merchant''sBaileyOffLimits.bmp' );
    HotspotList.Add( NewHotspot );

    NewHotspot := THotspot.Create( Self, 46, 438, '', 'Wharf''sBaileyOffLimits.bmp' );
    HotspotList.Add( NewHotspot );

    DefaultTransit := TTransitPoint.CreateDefault( Self, 606, 528, 174, 50 );
    
    MapsAvailable := nil;

    DirtyRect := Rect( 440, 545, 440 + 150, 545 + 30 );

    DXBack := DDGetSurface( lpDD, DirtyRect.Width, DirtyRect.Height, cInvisColor, False );
    DXDirty := DDGetSurface( lpDD, DirtyRect.Width, DirtyRect.Height, cInvisColor, False );

    Background := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'TransitDefault.bmp', cInvisColor );
    try
      if Assigned( Background ) then
      begin
        DXBack.BltFast( 0, 0, Background, @DirtyRect, DDBLTFAST_WAIT );
        DXDirty.BltFast( 0, 0, Background, @DirtyRect, DDBLTFAST_WAIT );
      end;
    except
    end;

    pText.LoadDarkFontGraphic( 'inventory' );
    pText.PlotDarkText2( DXBack, MsgBack, 0, 0, 240 );
    pText.PlotDarkText2( DXDirty, MsgBack, 0, 0, 128 );

    Paint;
    MouseCursor.Cleanup;
    SoAOS_DX_BltFastWaitXY( lpDDSFront, Rect( 0, 0, ScreenMetrics.ScreenWidth, ScreenMetrics.ScreenHeight ) );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

function TTransit.MapInList( const MapName : string ) : integer;
var
  S, S1 : string;
  i, j : integer;
begin
  result := -1;
  if assigned( MapsAvailable ) then
  begin
    S := lowercase( MapName ) + '|';
    for i := 0 to MapsAvailable.count - 1 do
    begin
      S1 := lowercase( MapsAvailable.strings[ i ] );
      j := Pos( S, S1 );
      if ( j = 1 ) or ( j = 2 ) then
      begin
        result := i;
        exit;
      end;
    end;
  end;
end;

procedure TTransit.MouseDown( Sender : TObject; Button : TMouseButton;
  Shift : TShiftState; X, Y, GridX, GridY : integer );
const
  FailName : string = 'TTransit.MouseDown';
var
  LastSelected : THotspot;
  hotSpot : THotSpot;
  selDest : TTransitPoint;
begin
  Log.DebugLog(FailName);
  try
    if Locked then
      Exit;

    inherited;
  //if InBound then Close;
    if X < 600 then
    begin
      if ptInRect( DirtyRect, point( x, y ) ) then
      begin
        Cancelled := true;
        Close;
      end
      else
      begin
        LastSelected := Selected;
        Selected := nil;
        for hotSpot in HotspotList do
        begin
          if ptInRect( hotSpot.Region, point( x, y ) ) then
          begin
            if hotSpot.Enabled then
              Selected := hotSpot;
            break;
          end;
        end;
        if Selected <> LastSelected then
        begin
          if Assigned( Selected ) then
          begin
            if Assigned( Selected.Dest ) then
            begin
              for selDest in Selected.Dest do
                selDest.Reset;
              Paint;
              MouseCursor.Cleanup;
              SoAOS_DX_BltFastWaitXY( lpDDSFront, Rect( 0, 0, ScreenMetrics.ScreenWidth, ScreenMetrics.ScreenHeight ) );
              Timer.tag := 0;
              Timer.enabled := true;
            end
            else
            begin
              Timer.enabled := false;
              Paint;
            end;
          end
          else
          begin
            Timer.enabled := false;
            Paint;
          end;
        end;
      end;
    end
    else
    begin
      if DefaultTransit.Enabled and ptInRect( DefaultTransit.Region, point( x, y ) ) then
      begin
      //Proceed to default destination point
        ResultIndex := DefaultTransit.Index;
        ResultMapName := DefaultTransit.MapResult;
        DefaultTransit.ShowInRed;
        MouseCursor.Cleanup;
        SoAOS_DX_BltFront;
        Locked := True;
        Timer.enabled := False;
        Timer.Interval := 500;
        Timer.OnTimer := TimerCloseEvent;
        Timer.enabled := True;
        Exit;
      end
      else if assigned( Selected ) then
      begin
        for selDest in Selected.Dest do
        begin
          if selDest.Enabled  and  ptInRect( selDest.Region, Point( x, y ) ) then
          begin
          //We have a destination point
            ResultIndex := selDest.Index;
            ResultMapName := selDest.MapResult;
            selDest.ShowInRed;
            MouseCursor.Cleanup;
            SoAOS_DX_BltFront;
            Locked := True;

            Timer.Enabled := False;
            Timer.Interval := 500;
            Timer.OnTimer := TimerCloseEvent;
            Timer.Enabled := True;

            Exit;
          end;
        end;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //MouseDown

procedure TTransit.MouseMove( Sender : TObject;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer );
const
  FailName : string = 'TTransit.MouseMove';
var
  pr : TRect;
begin
  Log.DebugLog(FailName);
  try
    if Locked then
      exit;

    if ptInRect( DirtyRect, point( x, y ) ) then
    begin
      if not MouseOverBack then
      begin
      //lpDDSFront.Flip(nil, DDFLIP_WAIT);
        MouseCursor.Cleanup;
        SoAOS_DX_BltFastWaitXY( lpDDSFront, Rect( 0, 0, ScreenMetrics.ScreenWidth, ScreenMetrics.ScreenHeight ) );
        if assigned( DXBack ) then
        begin
          pr := Rect( 0, 0, DirtyRect.Width, DirtYrect.Height );
          lpDDSBack.bltFast( DirtyRect.Left, DirtyRect.Top, DXBack, @pr, DDBLTFAST_WAIT );
        end;
        lpDDSFront.Flip( nil, DDFLIP_WAIT );
        MouseCursor.PlotDirty := false;
        MouseOverBack := true;
      end;
    end
    else
    begin
      if MouseOverBack then
      begin
     //clean up
        MouseCursor.Cleanup;
        SoAOS_DX_BltFastWaitXY( lpDDSFront, Rect( 0, 0, ScreenMetrics.ScreenWidth, ScreenMetrics.ScreenHeight ) );
        if assigned( DXDirty ) then
        begin
          pr := Rect( 0, 0, DirtyRect.Width, DirtYrect.Height );
          lpDDSBack.bltFast( DirtyRect.Left, DirtyRect.Top, DXDirty, @pr, DDBLTFAST_WAIT );
        end;
        lpDDSFront.Flip( nil, DDFLIP_WAIT );
        MouseCursor.PlotDirty := false;
        MouseOverBack := false;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //MouseMove

procedure TTransit.Paint;
var
  pr : TRect;
  selDest : TTransitPoint;
  hotSpot : THotspot;
begin
  pr := Rect( 0, 0, 800, 600 );  //NOHD
  lpDDSBack.BltFast( 0, 0, Background, @pr, DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
  if assigned( DXDirty ) then
  begin
    pr := Rect( 0, 0, DirtyRect.Width, DirtYrect.Height );
    lpDDSBack.bltFast( DirtyRect.Left, DirtyRect.Top, DXDirty, @pr, DDBLTFAST_WAIT );
  end;
  for hotSpot in HotspotList do
  begin
    if Assigned( hotSpot.RedImage ) and ( hotSpot = Selected ) then
    begin
      pr := Rect( 0, 0, hotSpot.Region.Width, hotSpot.Region.Height );
      lpDDSBack.BltFast( hotSpot.Region.Left, hotSpot.Region.Top, hotSpot.RedImage, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    end
    else if Assigned( hotSpot.GrayImage ) and not hotSpot.Enabled then
    begin
      pr := Rect( 0, 0, hotSpot.Region.Width, hotSpot.Region.Height );
      lpDDSBack.BltFast( hotSpot.Region.Left, hotSpot.Region.Top, hotSpot.GrayImage, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    end
  end;

  if Assigned( Selected ) then
  begin
    if Assigned( Selected.Dest ) then
    begin
      for selDest in Selected.Dest do
      begin
        lpDDSBack.BltFast( selDest.Region.Left, selDest.Region.Top, Background, @selDest.Region, DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
        DrawAlpha( lpDDSBack, selDest.Region, Rect( 0, 0, selDest.Region.Width, selDest.Region.Height ), selDest.Image, True, selDest.Blend );
      end;
    end;
  end;

  DrawAlpha( lpDDSBack, DefaultTransit.Region, Rect( 0, 0, DefaultTransit.Region.Width, DefaultTransit.Region.Height ),
      DefaultTransit.Image, true, 220 );
  
  lpDDSFront.Flip( nil, DDFLIP_WAIT );
  MouseCursor.PlotDirty := false;
end;

procedure TTransit.TimerEvent( Sender : TObject );
var
  i : integer;
begin
  if Assigned( Selected ) then
  begin
    if Assigned( Selected.Dest ) then
    begin
      for i := 0 to Selected.Dest.count - 1 do
      begin
        with Selected.Dest[ i ] do
        begin
          lpDDSBack.BltFast( Region.Left, Region.Top, Background,
            @Region, DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
          Selected.Dest[ i ].Update;
        end;
      end;
    end;
  end;
  Timer.tag := Timer.tag + 1;
  if Timer.tag >= 15 then
    Timer.enabled := false;
  MouseCursor.Cleanup;
  lpDDSFront.Flip( nil, DDFLIP_WAIT );
  MouseCursor.PlotDirty := false;
end;

procedure TTransit.Release;
begin
  Background := nil;
  DXBack := nil;
  DXDirty := nil;
  FreeAndNil(DefaultTransit);
  Timer.Enabled := False;
  Timer.OnTimer := nil;
  FreeAndNil(Timer);
  FreeAndNil(HotspotList);
  inherited;
end;

procedure TTransit.FormMouseDown( Sender : TObject; Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer );
begin
  MouseDown( nil, Button, Shift, X, Y, 0, 0 );
end;

procedure TTransit.FormMouseMove( Sender : TObject; Shift : TShiftState; X,
  Y : Integer );
begin
  MouseMove( nil, Shift, X, Y, 0, 0 );
end;

procedure TTransit.TimerCloseEvent( Sender : TObject );
begin
  Close;
end;

{ THotspot }

constructor THotspot.Create( const AOwner : TTransit; const x, y: Integer; const redImage, grayImage: string );
var
  width, height : Integer;
begin
  inherited Create;
//  Region.Right := X + Region.Right - Region.Left;
//  Region.Top := Y + Region.Bottom - Region.Top;
  FRegion.Left := x;
  FRegion.Bottom := y;

  if redImage <> '' then
  begin
    FRedImage := SoAOS_DX_LoadBMP( InterfacePath + redImage, cInvisColor, width, height );
    FRegion.Right := FRegion.Left + width;
    FRegion.Top := FRegion.Bottom - height;
  end;

  if grayImage <> '' then
  begin
    FGrayImage := SoAOS_DX_LoadBMP( InterfacePath + grayImage, cInvisColor, width, height );
    FRegion.Right := FRegion.Left + width;
    FRegion.Top := FRegion.Bottom - height;
  end;
end;

procedure THotspot.CreateInnerBailey( const AOwner : TTransit );
var
  NewTransitPoint : TTransitPoint;
begin
  FDest := TObjectList<TTransitPoint>.Create;
  NewTransitPoint := TTransitPoint.Create( AOwner, 631, 173, 'CutInrBailey.bmp', 'IBailey01' );
  FDest.Add( NewTransitPoint );
  FEnabled := FEnabled or NewTransitPoint.Enabled;
end;

procedure THotspot.CreateInnerKeep( const AOwner : TTransit );
var
  NewTransitPoint : TTransitPoint;
begin
  FDest := TObjectList<TTransitPoint>.Create;
  NewTransitPoint := TTransitPoint.Create( AOwner, 605, 427, 'CutInrKepBas.bmp', 'KeepBsmt' );
  FDest.Add( NewTransitPoint );
  FEnabled := FEnabled or NewTransitPoint.Enabled;

  NewTransitPoint := TTransitPoint.Create( AOwner, 701, 426, 'CutInrKepCave.bmp', 'caves01' );
  FDest.Add( NewTransitPoint );
  FEnabled := FEnabled or NewTransitPoint.Enabled;

  NewTransitPoint := TTransitPoint.Create( AOwner, 627, 205, 'CutInrKepLev1.bmp', 'IKeep01' );
  FDest.Add( NewTransitPoint );
  FEnabled := FEnabled or NewTransitPoint.Enabled;

  NewTransitPoint := TTransitPoint.Create( AOwner, 628, 17, 'CutInrKepLev2.bmp', 'IKeep02' );
  FDest.Add( NewTransitPoint );
  FEnabled := FEnabled or NewTransitPoint.Enabled;
end;

procedure THotspot.CreateOuterBailey( const AOwner : TTransit );
var
  NewTransitPoint : TTransitPoint;
begin
  FDest := TObjectList<TTransitPoint>.Create;
  NewTransitPoint := TTransitPoint.Create( AOwner, 627, 151, 'CutOuterBailey.bmp', 'OB1b' );
  FDest.Add( NewTransitPoint );
  FEnabled := FEnabled or NewTransitPoint.Enabled;
end;

procedure THotspot.CreateOuterKeep( const AOwner : TTransit );
var
  NewTransitPoint : TTransitPoint;
begin
  FDest := TObjectList<TTransitPoint>.Create;
  NewTransitPoint := TTransitPoint.Create( AOwner, 622, 418, 'CutOutrKepBas1.bmp', 'Okeepsub1b' );
  FDest.Add( NewTransitPoint );
  FEnabled := FEnabled or NewTransitPoint.Enabled;

  NewTransitPoint := TTransitPoint.Create( AOwner, 622, 470, 'CutOutrKepBas2.bmp', 'okeepsub2' );
  FDest.Add( NewTransitPoint );
  FEnabled := FEnabled or NewTransitPoint.Enabled;

  NewTransitPoint := TTransitPoint.Create( AOwner, 644, 278, 'CutOutrKepLev1.bmp', 'OKeepL1');
  FDest.Add( NewTransitPoint );
  FEnabled := FEnabled or NewTransitPoint.Enabled;

  NewTransitPoint := TTransitPoint.Create( AOwner, 643, 149, 'CutOutrKepLev2.bmp', 'OKeepL2');
  FDest.Add( NewTransitPoint );
  FEnabled := FEnabled or NewTransitPoint.Enabled;

  NewTransitPoint := TTransitPoint.Create( AOwner, 640, 18, 'CutOutrKepLev3.bmp', 'OKeepL3' );
  FDest.Add( NewTransitPoint );
  FEnabled := FEnabled or NewTransitPoint.Enabled;
end;

procedure THotspot.CreatePeasantsBailey( const AOwner : TTransit );
var
  NewTransitPoint : TTransitPoint;
begin
  FDest := TObjectList<TTransitPoint>.Create;
  NewTransitPoint := TTransitPoint.Create( AOwner, 610, 45, 'CutPeasantsBailey.bmp', '?' );
  Dest.Add( NewTransitPoint );
  FEnabled := FEnabled or NewTransitPoint.Enabled;
end;

procedure THotspot.CreateSouthgate( const AOwner : TTransit );
var
  NewTransitPoint : TTransitPoint;
begin
  FDest := TObjectList<TTransitPoint>.Create;
  NewTransitPoint := TTransitPoint.Create( AOwner, 616, 471, 'CutSouthGateBas.bmp', 'southgatesub1' );
  FDest.Add( NewTransitPoint );
  FEnabled := FEnabled or NewTransitPoint.Enabled;

  NewTransitPoint := TTransitPoint.Create( AOwner, 611, 292, 'CutSouthGateLev1.bmp', 'Southgate1b' );
  FDest.Add( NewTransitPoint );
  FEnabled := FEnabled or NewTransitPoint.Enabled;

  NewTransitPoint := TTransitPoint.Create( AOwner, 640, 177, 'CutSouthGateLev2.bmp', 'Southgate2' );
  FDest.Add( NewTransitPoint );
  FEnabled := FEnabled or NewTransitPoint.Enabled;

  NewTransitPoint := TTransitPoint.Create( AOwner, 641, 57, 'CutSouthGateLev3.bmp', 'Southgate3' );
  FDest.Add( NewTransitPoint );
  FEnabled := FEnabled or NewTransitPoint.Enabled;
end;

destructor THotspot.Destroy;
begin
  FGrayImage := nil;
  FRedImage := nil;
  if Assigned( FDest ) then
    FDest.Free;
  inherited;
end;

{ TTransitPoint }

constructor TTransitPoint.Create(const AOwner: TTransit; const x, y: Integer; const Image, MapName: string);
var
  width, height : Integer;
begin
  inherited Create;
  FRegion.Left := x;
  FRegion.Top := y;
  if image <> '' then
  begin
    FImage := SoAOS_DX_LoadBMP( InterfaceLanguagePath + image, cInvisColor, width, height );
    FRegion.Right := Region.Left + width;
    FRegion.Bottom := Region.Top + height;
  end;
  FMapName := mapName;
  FIndex := AOwner.MapInList( mapName );
  FEnabled := ( FIndex >= 0 );
  if ( FIndex >= 0 ) and assigned( AOwner.MapsAvailable ) then
    FMapResult := AOwner.MapsAvailable.strings[ FIndex ];
end;

constructor TTransitPoint.CreateDefault(const AOwner: TTransit; const x, y, w, h: Integer);
begin
  inherited Create;
  FRegion.Left := x;
  FRegion.Top := y;
  FRegion.BottomRight := Point( x + w, y + h );
  FImage := DDGetSurface( lpDD, w, h, cInvisColor, false );
  AOwner.pText.PlotF13TextCentered( FImage, AOwner.MsgProceed, 0, FRegion.Width, 3, 255 );
  AOwner.pText.PlotF13TextCentered( FImage, AOwner.DefaultName, 0, FRegion.Width, 23, 255 );
  FMapName := AOwner.DefaultMap;
  FIndex := -1;
  FEnabled := True;
  FMapResult := '';
end;

destructor TTransitPoint.Destroy;
begin
  FImage := nil;
  inherited;
end;

procedure TTransitPoint.Reset;
begin
  FBlend := 0;
end;

procedure TTransitPoint.ShowInRed;
var
  ddsd : TDDSurfaceDesc;
  SrcBits : BITPLANE;
  DestBits : BITPLANE;
  R : TRect;
  RLE : TRLESprite;
begin
  R := Rect( Region.Left - 4, Region.Top - 4, Region.right + 4, Region.Bottom + 4 );
  FillRectAlpha( lpDDSBack, R, $2070A0, 40 );
  R := Rect( Region.Left - 3, Region.Top - 3, Region.right + 3, Region.Bottom + 3 );
  FillRectAlpha( lpDDSBack, R, $2070A0, 40 );
  R := Rect( Region.Left - 2, Region.Top - 2, Region.right + 2, Region.Bottom + 2 );
  FillRectAlpha( lpDDSBack, R, $2070A0, 40 );
  R := Rect( Region.Left - 1, Region.Top - 1, Region.right + 1, Region.Bottom + 1 );
  FillRectAlpha( lpDDSBack, R, $2070A0, 40 );

  ddsd.dwSize := SizeOf( ddsd );
  if lpDDSBack.Lock( @Region, ddsd, DDLOCK_WAIT, 0 ) = DD_OK then
  begin
    try
      DestBits.bitsPtr := ddsd.lpSurface;
      DestBits.bitsWdh := ResWidth;
      DestBits.bitsHgh := ResHeight;
      DestBits.bitsFmt := dfx_pixelformat;
      DestBits.bitsPitch := ddsd.lPitch;
      DestBits.BaseX := 0;
      DestBits.BaseY := 0;

      R := Rect( 0, 0, Region.Width, Region.Height );
      if FImage.Lock( @R, ddsd, DDLOCK_WAIT, 0 ) = DD_OK then
      begin
        try
          SrcBits.bitsPtr := ddsd.lpSurface;
          SrcBits.bitsWdh := Region.Width;
          SrcBits.bitsHgh := Region.Height;
          SrcBits.bitsFmt := dfx_pixelformat;
          SrcBits.bitsPitch := ddsd.lPitch;
          SrcBits.BaseX := 0;
          SrcBits.BaseY := 0;

          RLE := TRLESprite.Create;
          try
            RLE.LoadFromBitPlaneBits( @SrcBits, cInvisColor );
            RLE.DrawBlend( 0, 0, 0, @DestBits, 70, 50 );
          finally
            RLE.Free;
          end;

        finally
          FImage.Unlock( nil );
        end;
      end;
    finally
      lpDDSBack.Unlock( nil );
    end;
  end;
end;

procedure TTransitPoint.Update;
begin
  if FEnabled and ( FBlend < 255 ) then
    Inc( FBlend, 17 );

  if ( not FEnabled ) and ( FBlend < 85 ) then
    Inc( FBlend, 17 );

  if Assigned( FImage ) then
    DrawAlpha( lpDDSBack, FRegion, Rect( 0, 0, FRegion.Width, Region.Height ), FImage, True, FBlend );
end;

end.
