unit Transit;
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

interface

uses
  SysUtils,
  Classes,
  Controls,
  extctrls,
  Forms,
  Windows,
  Gametext,
  Graphics,
{$IFDEF DX5}
  DirectX,
{$ELSE}
  DirectDraw,
{$ENDIF}
  Anigrp30,
  Engine,
  DXEffects,
  DXUtil,
  Display,
  MMTimer,
  digifx,
  DFX,
  LogFile;

type
  TTransit = class;

  TTransitPoint = class( TObject )
  private
    Region : TRect;
    Blend : integer;
    FImage : IDirectDrawSurface;
    FIndex : integer;
    procedure SetImage( const Value : string );
    procedure SetIndex( const Value : integer );
  public
    Enabled : boolean;
    MapName : string;
    MapResult : string;
    destructor Destroy; override;
    procedure SetPos( X, Y : integer );
    procedure Reset;
    procedure Update;
    procedure ShowInRed;
    property Image : string write SetImage;
    property Index : integer read FIndex write SetIndex;
  end;

  THotspot = class( TObject )
  private
    Region : TRect;
    FGrayImage : IDirectDrawSurface;
    FRedImage : IDirectDrawSurface;
    Owner : TTransit;
    procedure SetGrayImage( const Value : string );
    procedure SetRedImage( const Value : string );
  public
    Enabled : boolean;
    Dest : TList;
    constructor Create( AOwner : TTransit );
    constructor CreateOuterBailey( AOwner : TTransit );
    constructor CreateInnerBailey( AOwner : TTransit );
    constructor CreateOuterKeep( AOwner : TTransit );
    constructor CreateInnerKeep( AOwner : TTransit );
    constructor CreatePeasantsBailey( AOwner : TTransit );
    constructor CreateSouthgate( AOwner : TTransit );
    destructor Destroy; override;
    procedure SetPos( X, Y : integer );
    property GrayImage : string write SetGrayImage;
    property RedImage : string write SetRedImage;
  end;

  TTransit = class( TDisplay )
  private
    FOnClose : TNotifyEvent;
    DXBack : IDirectDrawSurface;
    DXDirty : IDirectDrawSurface;
    Background : IDirectDrawSurface;
    MouseOverBack : boolean;
    HotspotList : TList;
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
    procedure MouseDown( Sender : TAniview; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
    procedure MouseMove( Sender : TAniview;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
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
  AniDemo;

{ TTransit }

procedure TTransit.Init;
const
  FailName : string = 'TTransit.Init';
var
  BM : TBitmap;
  NewHotspot : THotspot;
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
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

    ExText.OPen( 'Transit' );
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

    HotspotList := TList.create;

    NewHotspot := THotspot.CreateInnerBailey( self );
    NewHotspot.SetPos( 261, 301 );
    NewHotspot.RedImage := 'InnerBaileyAvailable.bmp';
    NewHotspot.GrayImage := 'InnerBaileyUnavailable.bmp';
    HotspotList.add( NewHotspot );

    NewHotspot := THotspot.CreateOuterKeep( self );
    NewHotspot.SetPos( 276, 361 );
    NewHotspot.RedImage := 'OuterKeepAvailable.bmp';
    NewHotspot.GrayImage := '';
    HotspotList.add( NewHotspot );

    NewHotspot := THotspot.CreateSouthGate( self );
    NewHotspot.SetPos( 266, 501 );
    NewHotspot.RedImage := 'SouthGateAvailable.bmp';
    NewHotspot.GrayImage := 'SouthGateUnavailable.bmp';
    HotspotList.add( NewHotspot );

    NewHotspot := THotspot.CreateOuterBailey( self );
    NewHotspot.SetPos( 259, 463 );
    NewHotspot.RedImage := 'OuterBaileyAvailable.bmp';
    NewHotspot.GrayImage := '';
    HotspotList.add( NewHotspot );

    NewHotspot := THotspot.CreateInnerKeep( self );
    NewHotspot.SetPos( 250, 251 );
    NewHotspot.RedImage := 'InnerKeepAvailable.bmp';
    NewHotspot.GrayImage := 'InnerKeepUnavailable.bmp';
    HotspotList.add( NewHotspot );

    NewHotspot := THotspot.CreatePeasantsBailey( self );
    NewHotspot.SetPos( 373, 464 );
    NewHotspot.RedImage := 'Peasant''sBaileyAvailable.bmp';
    NewHotspot.GrayImage := 'Peasant''sBaileyUnavailable.bmp';
    HotspotList.add( NewHotspot );

    NewHotspot := THotspot.create( self );
    NewHotspot.SetPos( 377, 295 );
    NewHotspot.RedImage := '';
    NewHotspot.GrayImage := 'CattleBaileyOffLimits.bmp';
    HotspotList.add( NewHotspot );

    NewHotspot := THotspot.create( self );
    NewHotspot.SetPos( 263, 191 );
    NewHotspot.RedImage := '';
    NewHotspot.GrayImage := 'NobleBaileyOffLimits.bmp';
    HotspotList.add( NewHotspot );

    NewHotspot := THotspot.create( self );
    NewHotspot.SetPos( 158, 197 );
    NewHotspot.RedImage := '';
    NewHotspot.GrayImage := 'TrainingBaileyOffLimits.bmp';
    HotspotList.add( NewHotspot );

    NewHotspot := THotspot.create( self );
    NewHotspot.SetPos( 159, 299 );
    NewHotspot.RedImage := '';
    NewHotspot.GrayImage := 'Soldier''sBaileyOffLimits.bmp';
    HotspotList.add( NewHotspot );

    NewHotspot := THotspot.create( self );
    NewHotspot.SetPos( 154, 464 );
    NewHotspot.RedImage := '';
    NewHotspot.GrayImage := 'Merchant''sBaileyOffLimits.bmp';
    HotspotList.add( NewHotspot );

    NewHotspot := THotspot.create( self );
    NewHotspot.SetPos( 46, 438 );
    NewHotspot.RedImage := '';
    NewHotspot.GrayImage := 'Wharf''sBaileyOffLimits.bmp';
    HotspotList.add( NewHotspot );

    DefaultTransit := TTransitPoint.create;
    DefaultTransit.SetPos( 606, 528 );
    DefaultTransit.Region.BottomRight := Point( 780, 578 );
    DefaultTransit.FImage := DDGetSurface( lpDD, DefaultTransit.Region.Right - DefaultTransit.Region.Left,
      DefaultTransit.Region.Bottom - DefaultTransit.Region.top, clAqua, false );
    pText.PlotF13TextCentered( DefaultTransit.FImage, MsgProceed,
      0, DefaultTransit.Region.Right - DefaultTransit.Region.Left, 3, 255 );
    pText.PlotF13TextCentered( DefaultTransit.FImage, DefaultName,
      0, DefaultTransit.Region.Right - DefaultTransit.Region.Left, 23, 255 );

    DefaultTransit.MapName := DefaultMap;
    DefaultTransit.Index := -1;
    DefaultTransit.Enabled := true;
    DefaultTransit.MapResult := '';

    MapsAvailable := nil;

    DirtyRect := rect( 440, 545, 440 + 150, 545 + 30 );

    BM := TBitmap.create;
    try
      BM.LoadFromFile( InterfacePath + 'MapBack.bmp' );
      DXBack := DDGetSurface( lpDD, DirtyRect.Right - DirtyRect.Left, DirtyRect.Bottom - DirtyRect.Top, clAqua, false );
      DXDirty := DDGetSurface( lpDD, DirtyRect.Right - DirtyRect.Left, DirtyRect.Bottom - DirtyRect.Top, clAqua, false );

      BM.LoadFromFile( InterfacePath + 'TransitDefault.bmp' );
      Background := DDGetImage( lpDD, BM, clAqua, true );
      try
        if assigned( Background ) then
        begin
          WrapperBltFast( DXBack, 0, 0, Background, DirtyRect, DDBLTFAST_WAIT );
          WrapperBltFast( DXDirty, 0, 0, Background, DirtyRect, DDBLTFAST_WAIT );
        end;
      except
      end;
    finally
      BM.free;
    end;

    pText.LoadDarkFontGraphic( 'inventory' );
    pText.PlotDarkText2( DXBack, MsgBack, 0, 0, 240 );
    pText.PlotDarkText2( DXDirty, MsgBack, 0, 0, 128 );

    Paint;
    MouseCursor.Cleanup;
    WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
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

procedure TTransit.MouseDown( Sender : TAniview; Button : TMouseButton;
  Shift : TShiftState; X, Y, GridX, GridY : integer );
const
  FailName : string = 'TTransit.MouseDown';
var
  LastSelected : THotspot;
  i : integer;
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if Locked then
      exit;

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
        for i := 0 to HotspotList.count - 1 do
        begin
          if ptInRect( THotspot( HotspotList.items[ i ] ).Region, point( x, y ) ) then
          begin
            if THotspot( HotspotList.items[ i ] ).Enabled then
              Selected := THotspot( HotspotList.items[ i ] );
            break;
          end;
        end;
        if Selected <> LastSelected then
        begin
          if assigned( Selected ) then
          begin
            if assigned( Selected.Dest ) then
            begin
              for i := 0 to Selected.Dest.count - 1 do
              begin
                TTransitPoint( Selected.Dest.items[ i ] ).Reset;
              end;
              Paint;
              MouseCursor.Cleanup;
              WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
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
        lpDDSFront.Flip( nil, DDFLIP_WAIT );
        WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
        MouseCursor.PlotDirty := false;
        Locked := true;
        Timer.enabled := false;
        Timer.Interval := 500;
        Timer.OnTimer := TimerCloseEvent;
        Timer.enabled := true;
        exit;
      end
      else if assigned( Selected ) then
      begin
        for i := 0 to Selected.Dest.count - 1 do
        begin
          if TTransitPoint( Selected.Dest.items[ i ] ).Enabled then
          begin
            if ptInRect( TTransitPoint( Selected.Dest.items[ i ] ).Region, point( x, y ) ) then
            begin
            //We have a destination point
              ResultIndex := TTransitPoint( Selected.Dest.items[ i ] ).Index;
              ResultMapName := TTransitPoint( Selected.Dest.items[ i ] ).MapResult;
              TTransitPoint( Selected.Dest.items[ i ] ).ShowInRed;
              MouseCursor.Cleanup;
              lpDDSFront.Flip( nil, DDFLIP_WAIT );
              WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
              MouseCursor.PlotDirty := false;
              Locked := true;
              Timer.enabled := false;
              Timer.Interval := 500;
              Timer.OnTimer := TimerCloseEvent;
              Timer.enabled := true;
              exit;
            end;
          end;
        end;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //MouseDown

procedure TTransit.MouseMove( Sender : TAniview; Shift : TShiftState; X, Y, GridX,
  GridY : integer );
const
  FailName : string = 'TTransit.MouseMove';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if Locked then
      exit;

    if ptInRect( DirtyRect, point( x, y ) ) then
    begin
      if not MouseOverBack then
      begin
      //lpDDSFront.Flip(nil, DDFLIP_WAIT);
        MouseCursor.Cleanup;
        WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
        if assigned( DXBack ) then
          WrapperBltFast( lpDDSBack, DirtyRect.Left, DirtyRect.Top, DXBack, rect( 0, 0, DirtyRect.Right - DirtyRect.Left, DirtYrect.Bottom - DirtyRect.Top ), DDBLTFAST_WAIT );
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
        WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
        if assigned( DXDirty ) then
          WrapperBltFast( lpDDSBack, DirtyRect.Left, DirtyRect.Top, DXDirty, rect( 0, 0, DirtyRect.Right - DirtyRect.Left, DirtYrect.Bottom - DirtyRect.Top ), DDBLTFAST_WAIT );
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
  i : integer;
begin
  WrapperBltFast( lpDDSBack, 0, 0, Background, Rect( 0, 0, 800, 600 ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
  if assigned( DXDirty ) then
    WrapperBltFast( lpDDSBack, DirtyRect.Left, DirtyRect.Top, DXDirty, rect( 0, 0, DirtyRect.Right - DirtyRect.Left, DirtYrect.Bottom - DirtyRect.Top ), DDBLTFAST_WAIT );
  for i := 0 to HotspotList.count - 1 do
  begin
    with THotspot( HotspotList.items[ i ] ) do
    begin
      if assigned( FRedImage ) and ( HotspotList.items[ i ] = Selected ) then
      begin
        WrapperBltFast( lpDDSBack, Region.Left, Region.Top, FRedImage,
          Rect( 0, 0, Region.Right - Region.Left, Region.Bottom - Region.Top ),
          DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end
      else if assigned( FGrayImage ) and not Enabled then
      begin
        WrapperBltFast( lpDDSBack, Region.Left, Region.Top, FGrayImage,
          Rect( 0, 0, Region.Right - Region.Left, Region.Bottom - Region.Top ),
          DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end
    end;
  end;

  if assigned( Selected ) then
  begin
    if assigned( Selected.Dest ) then
    begin
      for i := 0 to Selected.Dest.count - 1 do
      begin
        with TTransitPoint( Selected.Dest.items[ i ] ) do
        begin
          WrapperBltFast( lpDDSBack, Region.Left, Region.Top, Background,
            Region, DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
          DrawAlpha( lpDDSBack, Region, Rect( 0, 0, Region.Right - Region.Left, Region.Bottom - Region.Top ),
            FImage, true, Blend );
        end;
      end;
    end;
  end;

  with DefaultTransit do
  begin
    DrawAlpha( lpDDSBack, Region, Rect( 0, 0, Region.Right - Region.Left, Region.Bottom - Region.Top ),
      FImage, true, 220 );
  end;


  lpDDSFront.Flip( nil, DDFLIP_WAIT );
  MouseCursor.PlotDirty := false;
end;

procedure TTransit.TimerEvent( Sender : TObject );
var
  i : integer;
begin
  if assigned( Selected ) then
  begin
    if assigned( Selected.Dest ) then
    begin
      for i := 0 to Selected.Dest.count - 1 do
      begin
        with TTransitPoint( Selected.Dest.items[ i ] ) do
        begin
          WrapperBltFast( lpDDSBack, Region.Left, Region.Top, Background,
            Region, DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
          TTransitPoint( Selected.Dest.items[ i ] ).Update;
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
var
  i : integer;
begin
  Background := nil;
  DXBack := nil;
  DXDirty := nil;
  DefaultTransit.free;
  DefaultTransit := nil;
  Timer.enabled := false;
  Timer.OnTimer := nil;
  Timer.free;
  Timer := nil;
  for i := 0 to HotspotList.count - 1 do
    THotspot( HotspotList.items[ i ] ).free;
  HotspotList.free;
  HotspotList := nil;

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

constructor THotspot.Create( AOwner : TTransit );
begin
  inherited Create;
  Owner := AOwner;
end;

constructor THotspot.CreateInnerBailey( AOwner : TTransit );
var
  NewTransitPoint : TTransitPoint;
begin
  inherited Create;
  Owner := AOwner;
  Dest := TList.create;

  NewTransitPoint := TTransitPoint.create;
  NewTransitPoint.SetPos( 631, 173 );
  NewTransitPoint.Image := 'CutInrBailey.bmp';
  NewTransitPoint.MapName := 'IBailey01';
  NewTransitPoint.Index := Owner.MapInList( NewTransitPoint.MapName );
  if ( NewTransitPoint.Index >= 0 ) and assigned( Owner.MapsAvailable ) then
    NewTransitPoint.MapResult := Owner.MapsAvailable.strings[ NewTransitPoint.Index ];
  Dest.add( NewTransitPoint );
  Enabled := Enabled or NewTransitPoint.Enabled;
end;

constructor THotspot.CreateInnerKeep( AOwner : TTransit );
var
  NewTransitPoint : TTransitPoint;
begin
  inherited Create;
  Owner := AOwner;
  Dest := TList.create;

  NewTransitPoint := TTransitPoint.create;
  NewTransitPoint.SetPos( 605, 427 );
  NewTransitPoint.Image := 'CutInrKepBas.bmp';
  NewTransitPoint.MapName := 'KeepBsmt';
  NewTransitPoint.Index := Owner.MapInList( NewTransitPoint.MapName );
  if ( NewTransitPoint.Index >= 0 ) and assigned( Owner.MapsAvailable ) then
    NewTransitPoint.MapResult := Owner.MapsAvailable.strings[ NewTransitPoint.Index ];
  Dest.add( NewTransitPoint );
  Enabled := Enabled or NewTransitPoint.Enabled;

  NewTransitPoint := TTransitPoint.create;
  NewTransitPoint.SetPos( 701, 426 );
  NewTransitPoint.Image := 'CutInrKepCave.bmp';
  NewTransitPoint.MapName := 'caves01';
  NewTransitPoint.Index := Owner.MapInList( NewTransitPoint.MapName );
  if ( NewTransitPoint.Index >= 0 ) and assigned( Owner.MapsAvailable ) then
    NewTransitPoint.MapResult := Owner.MapsAvailable.strings[ NewTransitPoint.Index ];
  Dest.add( NewTransitPoint );
  Enabled := Enabled or NewTransitPoint.Enabled;

  NewTransitPoint := TTransitPoint.create;
  NewTransitPoint.SetPos( 627, 205 );
  NewTransitPoint.Image := 'CutInrKepLev1.bmp';
  NewTransitPoint.MapName := 'IKeep01';
  NewTransitPoint.Index := Owner.MapInList( NewTransitPoint.MapName );
  if ( NewTransitPoint.Index >= 0 ) and assigned( Owner.MapsAvailable ) then
    NewTransitPoint.MapResult := Owner.MapsAvailable.strings[ NewTransitPoint.Index ];
  Dest.add( NewTransitPoint );
  Enabled := Enabled or NewTransitPoint.Enabled;

  NewTransitPoint := TTransitPoint.create;
  NewTransitPoint.SetPos( 628, 17 );
  NewTransitPoint.Image := 'CutInrKepLev2.bmp';
  NewTransitPoint.MapName := 'IKeep02';
  NewTransitPoint.Index := Owner.MapInList( NewTransitPoint.MapName );
  if ( NewTransitPoint.Index >= 0 ) and assigned( Owner.MapsAvailable ) then
    NewTransitPoint.MapResult := Owner.MapsAvailable.strings[ NewTransitPoint.Index ];
  Dest.add( NewTransitPoint );
  Enabled := Enabled or NewTransitPoint.Enabled;
end;

constructor THotspot.CreateOuterBailey( AOwner : TTransit );
var
  NewTransitPoint : TTransitPoint;
begin
  inherited Create;
  Owner := AOwner;
  Dest := TList.create;

  NewTransitPoint := TTransitPoint.create;
  NewTransitPoint.SetPos( 627, 151 );
  NewTransitPoint.Image := 'CutOuterBailey.bmp';
  NewTransitPoint.MapName := 'OB1b';
  NewTransitPoint.Index := Owner.MapInList( NewTransitPoint.MapName );
  if ( NewTransitPoint.Index >= 0 ) and assigned( Owner.MapsAvailable ) then
    NewTransitPoint.MapResult := Owner.MapsAvailable.strings[ NewTransitPoint.Index ];
  Dest.add( NewTransitPoint );
  Enabled := Enabled or NewTransitPoint.Enabled;
end;

constructor THotspot.CreateOuterKeep( AOwner : TTransit );
var
  NewTransitPoint : TTransitPoint;
begin
  inherited Create;
  Owner := AOwner;
  Dest := TList.create;

  NewTransitPoint := TTransitPoint.create;
  NewTransitPoint.SetPos( 622, 418 );
  NewTransitPoint.Image := 'CutOutrKepBas1.bmp';
  NewTransitPoint.MapName := 'Okeepsub1b';
  NewTransitPoint.Index := Owner.MapInList( NewTransitPoint.MapName );
  if ( NewTransitPoint.Index >= 0 ) and assigned( Owner.MapsAvailable ) then
    NewTransitPoint.MapResult := Owner.MapsAvailable.strings[ NewTransitPoint.Index ];
  Dest.add( NewTransitPoint );
  Enabled := Enabled or NewTransitPoint.Enabled;

  NewTransitPoint := TTransitPoint.create;
  NewTransitPoint.SetPos( 622, 470 );
  NewTransitPoint.Image := 'CutOutrKepBas2.bmp';
  NewTransitPoint.MapName := 'okeepsub2';
  NewTransitPoint.Index := Owner.MapInList( NewTransitPoint.MapName );
  if ( NewTransitPoint.Index >= 0 ) and assigned( Owner.MapsAvailable ) then
    NewTransitPoint.MapResult := Owner.MapsAvailable.strings[ NewTransitPoint.Index ];
  Dest.add( NewTransitPoint );
  Enabled := Enabled or NewTransitPoint.Enabled;

  NewTransitPoint := TTransitPoint.create;
  NewTransitPoint.SetPos( 644, 278 );
  NewTransitPoint.Image := 'CutOutrKepLev1.bmp';
  NewTransitPoint.MapName := 'OKeepL1';
  NewTransitPoint.Index := Owner.MapInList( NewTransitPoint.MapName );
  if ( NewTransitPoint.Index >= 0 ) and assigned( Owner.MapsAvailable ) then
    NewTransitPoint.MapResult := Owner.MapsAvailable.strings[ NewTransitPoint.Index ];
  Dest.add( NewTransitPoint );
  Enabled := Enabled or NewTransitPoint.Enabled;

  NewTransitPoint := TTransitPoint.create;
  NewTransitPoint.SetPos( 643, 149 );
  NewTransitPoint.Image := 'CutOutrKepLev2.bmp';
  NewTransitPoint.MapName := 'OKeepL2';
  NewTransitPoint.Index := Owner.MapInList( NewTransitPoint.MapName );
  if ( NewTransitPoint.Index >= 0 ) and assigned( Owner.MapsAvailable ) then
    NewTransitPoint.MapResult := Owner.MapsAvailable.strings[ NewTransitPoint.Index ];
  Dest.add( NewTransitPoint );
  Enabled := Enabled or NewTransitPoint.Enabled;

  NewTransitPoint := TTransitPoint.create;
  NewTransitPoint.SetPos( 640, 18 );
  NewTransitPoint.Image := 'CutOutrKepLev3.bmp';
  NewTransitPoint.MapName := 'OKeepL3';
  NewTransitPoint.Index := Owner.MapInList( NewTransitPoint.MapName );
  if ( NewTransitPoint.Index >= 0 ) and assigned( Owner.MapsAvailable ) then
    NewTransitPoint.MapResult := Owner.MapsAvailable.strings[ NewTransitPoint.Index ];
  Dest.add( NewTransitPoint );
  Enabled := Enabled or NewTransitPoint.Enabled;
end;

constructor THotspot.CreatePeasantsBailey( AOwner : TTransit );
var
  NewTransitPoint : TTransitPoint;
begin
  inherited Create;
  Owner := AOwner;
  Dest := TList.create;

  NewTransitPoint := TTransitPoint.create;
  NewTransitPoint.SetPos( 610, 45 );
  NewTransitPoint.Image := 'CutPeasantsBailey.bmp';
  NewTransitPoint.MapName := '?';
  NewTransitPoint.Index := Owner.MapInList( NewTransitPoint.MapName );
  if ( NewTransitPoint.Index >= 0 ) and assigned( Owner.MapsAvailable ) then
    NewTransitPoint.MapResult := Owner.MapsAvailable.strings[ NewTransitPoint.Index ];
  Dest.add( NewTransitPoint );
  Enabled := Enabled or NewTransitPoint.Enabled;
end;

constructor THotspot.CreateSouthgate( AOwner : TTransit );
var
  NewTransitPoint : TTransitPoint;
begin
  inherited Create;
  Owner := AOwner;
  Dest := TList.create;

  NewTransitPoint := TTransitPoint.create;
  NewTransitPoint.SetPos( 616, 471 );
  NewTransitPoint.Image := 'CutSouthGateBas.bmp';
  NewTransitPoint.MapName := 'southgatesub1';
  NewTransitPoint.Index := Owner.MapInList( NewTransitPoint.MapName );
  if ( NewTransitPoint.Index >= 0 ) and assigned( Owner.MapsAvailable ) then
    NewTransitPoint.MapResult := Owner.MapsAvailable.strings[ NewTransitPoint.Index ];
  Dest.add( NewTransitPoint );
  Enabled := Enabled or NewTransitPoint.Enabled;

  NewTransitPoint := TTransitPoint.create;
  NewTransitPoint.SetPos( 611, 292 );
  NewTransitPoint.Image := 'CutSouthGateLev1.bmp';
  NewTransitPoint.MapName := 'Southgate1b';
  NewTransitPoint.Index := Owner.MapInList( NewTransitPoint.MapName );
  if ( NewTransitPoint.Index >= 0 ) and assigned( Owner.MapsAvailable ) then
    NewTransitPoint.MapResult := Owner.MapsAvailable.strings[ NewTransitPoint.Index ];
  Dest.add( NewTransitPoint );
  Enabled := Enabled or NewTransitPoint.Enabled;

  NewTransitPoint := TTransitPoint.create;
  NewTransitPoint.SetPos( 640, 177 );
  NewTransitPoint.Image := 'CutSouthGateLev2.bmp';
  NewTransitPoint.MapName := 'Southgate2';
  NewTransitPoint.Index := Owner.MapInList( NewTransitPoint.MapName );
  if ( NewTransitPoint.Index >= 0 ) and assigned( Owner.MapsAvailable ) then
    NewTransitPoint.MapResult := Owner.MapsAvailable.strings[ NewTransitPoint.Index ];
  Dest.add( NewTransitPoint );
  Enabled := Enabled or NewTransitPoint.Enabled;

  NewTransitPoint := TTransitPoint.create;
  NewTransitPoint.SetPos( 641, 57 );
  NewTransitPoint.Image := 'CutSouthGateLev3.bmp';
  NewTransitPoint.MapName := 'Southgate3';
  NewTransitPoint.Index := Owner.MapInList( NewTransitPoint.MapName );
  if ( NewTransitPoint.Index >= 0 ) and assigned( Owner.MapsAvailable ) then
    NewTransitPoint.MapResult := Owner.MapsAvailable.strings[ NewTransitPoint.Index ];
  Dest.add( NewTransitPoint );
  Enabled := Enabled or NewTransitPoint.Enabled;
end;

destructor THotspot.Destroy;
var
  i : integer;
begin
  FGrayImage := nil;
  FRedImage := nil;
  if assigned( Dest ) then
  begin
    for i := 0 to Dest.count - 1 do
      TTransitPoint( Dest.items[ i ] ).free;
    Dest.free;
  end;
  inherited;
end;

procedure THotspot.SetGrayImage( const Value : string );
var
  BM : TBitmap;
begin
  if Value = '' then
    exit;
  BM := TBitmap.create;
  try
    BM.LoadFromFile( InterfacePath + Value );
    Region.Right := Region.Left + BM.width;
    Region.Top := Region.Bottom - BM.height;
    FGrayImage := DDGetImage( lpDD, BM, clAqua, false );
  finally
    BM.free;
  end;
end;

procedure THotspot.Setpos( X, Y : integer );
begin
  Region.Right := X + Region.Right - Region.Left;
  Region.Top := Y + Region.Bottom - Region.Top;
  Region.Left := X;
  Region.Bottom := Y;
end;

procedure THotspot.SetRedImage( const Value : string );
var
  BM : TBitmap;
begin
  if Value = '' then
    exit;
  BM := TBitmap.create;
  try
    BM.LoadFromFile( InterfacePath + Value );
    Region.Right := Region.Left + BM.width;
    Region.Top := Region.Bottom - BM.height;
    FRedImage := DDGetImage( lpDD, BM, clAqua, false );
  finally
    BM.free;
  end;
end;

{ TTransitPoint }

destructor TTransitPoint.Destroy;
begin
  FImage := nil;
  inherited;
end;

procedure TTransitPoint.Reset;
begin
  Blend := 0;
end;

procedure TTransitPoint.SetImage( const Value : string );
var
  BM : TBitmap;
begin
  if Value = '' then
    exit;
  BM := TBitmap.create;
  try
    BM.LoadFromFile( InterfacePath + Value );
    Region.Right := Region.Left + BM.width;
    Region.Bottom := Region.Top + BM.height;
    FImage := DDGetImage( lpDD, BM, clAqua, false );
  finally
    BM.free;
  end;
end;

procedure TTransitPoint.SetIndex( const Value : integer );
begin
  FIndex := Value;
  Enabled := ( Value >= 0 );
end;

procedure TTransitPoint.SetPos( X, Y : integer );
begin
  Region.Right := X + Region.Right - Region.Left;
  Region.Bottom := Y + Region.Bottom - Region.Top;
  Region.Left := X;
  Region.Top := Y;
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

      R := Rect( 0, 0, Region.right - Region.Left, Region.Bottom - Region.Top );
      if FImage.Lock( @R, ddsd, DDLOCK_WAIT, 0 ) = DD_OK then
      begin
        try
          SrcBits.bitsPtr := ddsd.lpSurface;
          SrcBits.bitsWdh := Region.right - Region.Left;
          SrcBits.bitsHgh := Region.Bottom - Region.Top;
          SrcBits.bitsFmt := dfx_pixelformat;
          SrcBits.bitsPitch := ddsd.lPitch;
          SrcBits.BaseX := 0;
          SrcBits.BaseY := 0;

          RLE := TRLESprite.create;
          try
            RLE.LoadFromBitPlaneBits( @SrcBits, clAqua );

            RLE.DrawBlend( 0, 0, 0, @DestBits, 70, 50 );
          finally
            RLE.free;
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
  if Enabled then
  begin
    if Blend < 255 then
      inc( Blend, 17 );
  end
  else
  begin
    if Blend < 85 then
      inc( Blend, 17 );
  end;

  if assigned( FImage ) then
  begin
    DrawAlpha( lpDDSBack, Region, Rect( 0, 0, Region.Right - Region.Left, Region.Bottom - Region.Top ), FImage, true, Blend );
  end;
end;

end.

