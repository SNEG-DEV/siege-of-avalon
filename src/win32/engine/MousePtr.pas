unit MousePtr;
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
//  Winapi.DirectDraw,
  DirectX,
  DXUtil,
  System.Types,
  System.SysUtils,
  System.Classes,
  Vcl.Controls,
  Vcl.ExtCtrls,
  LogFile;

type
  TMousePtr = class( TObject )
  private
    DXMousePtr : IDirectDrawSurface;
    DXDirty : IDirectDrawSurface;
    MouseTimer : TTimer;
    Mpt : TPoint;
//    MouseCounter : integer;
    MouseFrame : integer;
//    MouseAnimationCycle : integer; //speed of frame change
//    StartFrame : integer;
//    PlayFrames : integer;
//    Loop : boolean;
//    OldStartFrame : integer;
//    OldSpeed : integer;
//    OldLoop : boolean;
    FPlotDirty : boolean; //plot cleanup or no?
    WAdj, HAdj, OldWAdj, OldHAdj : integer;
    FEnabled : boolean;
    procedure MouseTimerEvent( Sender : TObject );
    procedure SetPlotDirty( const Value : boolean );
    procedure SetEnabled( const Value : boolean );
  protected
  public
    DxSurface : IDirectDrawSurface; //surface to draw pointer to
    constructor Create;
    destructor Destroy; override;
//    procedure SetAnim( Frame, Frames, Speed : integer );
    procedure Cleanup;
//    procedure SetLoopAnim( Frame, Frames, Speed : integer );
    procedure SetFrame( Frame : integer );
    property PlotDirty : boolean read FPlotDirty write SetPlotDirty; //plot cleanup or no?
    property Enabled : boolean read FEnabled write SetEnabled;
  end;

implementation

uses
  SoAOS.Types,
  SoAOS.Graphics.Draw,
  SoAOS.Animation,
  AniDemo;

const
  PtrWidth = 32;
  PtrHeight = 32;
  SheetWidth = 6;

{ TMousePtr }

constructor TMousePtr.Create;
var
  pr : TRect;
  absPt: TPoint;
const
  FailName : string = 'TMousePtr.Create';
begin
  Log.DebugLog( FailName );
  try
    inherited;
    DXMousePtr := SoAOS_DX_LoadBMP( InterfacePath + 'siegecursorsheet.bmp', cTransparent );
    DXDirty := DDGetSurface( lpDD, PtrWidth, PtrHeight, cTransparent, true );
  //pre-load Dirty
    FPlotDirty := false;
    DXSurface := lpDDSFront;
    absPt := Mouse.CursorPos;
    absPt.X := absPt.X - frmMain.Left;
    absPt.Y := absPt.Y - frmMain.Top;
    mPt := TPoint.Create(absPt.X, absPt.Y);
    pr := Rect( mPt.x, mPt.y, mPt.x + PtrWidth, mPt.y + PtrHeight );
    DXDirty.BltFast( 0, 0, DXSurface, @pr, DDBLTFAST_WAIT );
    MouseTimer := TTimer.create( nil );
    MouseTimer.Enabled := false;
    MouseTimer.onTimer := MouseTimerEvent;
    MouseTimer.Interval := 10;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //Create

destructor TMousePtr.Destroy;
const
  FailName : string = 'TMousePtr.Destroy';
begin
  Log.DebugLog( FailName );
  try
    MouseTimer.enabled := false;
    MouseTimer.onTimer := nil;
    MouseTimer.free;
    DXDirty := nil;
    DXMousePtr := nil;
    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //Destroy


procedure TMousePtr.MouseTimerEvent( Sender : TObject );
var
  PrevPt, absPt : TPoint;
  X, Y : longint;
  pr : TRect;
begin
  if not FEnabled then
    exit;

  PrevPt := mPt;
  absPt := Mouse.CursorPos;
  absPt.X := absPt.X - frmMain.Left;
  absPt.Y := absPt.Y - frmMain.Top;
  mPt := TPoint.Create(absPt.X, absPt.Y);
//  if ( MouseAnimationCycle > 0 ) and ( MouseCounter > MouseAnimationCycle ) then
//  begin
//    inc( MouseFrame );
//    if MouseFrame >= PlayFrames then
//    begin
//      if Loop then
//      begin
//        MouseFrame := StartFrame;
//      end
//      else
//      begin
//        StartFrame := OldStartFrame;
//        MouseAnimationCycle := OldSpeed;
//        Loop := OldLoop;
//        MouseFrame := StartFrame;
//      end;
//    end;
//    MouseCounter := 0;
//  end;
//
//  if assigned( Sender ) then
//    inc( MouseCounter );

  OldWAdj := WAdj;
  OldHAdj := HAdj;
  WAdj := 0;
  HAdj := 0;
  if mPt.x + PtrWidth > ScreenMetrics.ScreenWidth then
    WAdj := ( mPt.x + PtrWidth ) - ScreenMetrics.ScreenWidth;
  if mPt.y + PtrHeight > ScreenMetrics.ScreenHeight then
    HAdj := ( mPt.y + PtrHeight ) - ScreenMetrics.ScreenHeight;

  if FPlotDirty then
  begin
    if ( mPt.x <> PrevPt.x ) or ( mPt.y <> PrevPt.y ) then
    begin
      pr := Rect( 0, 0, PtrWidth - OldWAdj, PtrHeight - OldHAdj );
      DXSurface.BltFast( PrevPt.x, PrevPt.y, DXDirty, @pr, DDBLTFAST_WAIT );
      pr := Rect( mPt.x, mPt.y, mPt.x + PtrWidth - WAdj, mPt.y + PtrHeight - HAdj );
      DXDirty.BltFast( 0, 0, DXSurface, @pr, DDBLTFAST_WAIT );
    end;
  end
  else
  begin
    pr := Rect( mPt.x, mPt.y, mPt.x + PtrWidth - WAdj, mPt.y + PtrHeight - HAdj );
    DXDirty.BltFast( 0, 0, DXSurface, @pr, DDBLTFAST_WAIT );
  end;

  X := ( MouseFrame mod SheetWidth ) * PtrWidth;
  Y := ( MouseFrame div SheetWidth ) * PtrHeight;
  pr := Rect( X, Y, X + PtrWidth - WAdj, Y + PtrHeight - HAdj );
  DXSurface.BltFast( mPt.x, mPt.y, DXMousePtr, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
  FPlotDirty := true;
end; //MouseTimerEvent


//procedure TMousePtr.SetAnim( Frame, Frames, Speed : integer );
//const
//  FailName : string = 'TMousePtr.SetAnim';
//begin
//  Log.DebugLog( FailName );
//  try
//    if MouseAnimationCycle > 0 then
//      exit;
//    OldStartFrame := StartFrame;
//    OldSpeed := MouseAnimationCycle;
//    OldLoop := Loop;
//    StartFrame := Frame;
//    PlayFrames := Frame + Frames;
//    MouseAnimationCycle := Speed;
//    Loop := false;
//    MouseCounter := 0;
//    MouseFrame := StartFrame;
//  except
//    on E : Exception do
//      Log.log( FailName + E.Message );
//  end;
//
//end;

//procedure TMousePtr.SetLoopAnim( Frame, Frames, Speed : integer );
//const
//  FailName : string = 'TMousePtr.SetloopAnim';
//begin
//  Log.DebugLog( FailName );
//  try
//    if MouseAnimationCycle > 0 then
//      exit;
//    StartFrame := Frame;
//    PlayFrames := Frame + Frames;
//    MouseAnimationCycle := Speed;
//    Loop := true;
//    MouseCounter := 0;
//    MouseFrame := StartFrame;
//  except
//    on E : Exception do
//      Log.log( FailName + E.Message );
//  end;
//
//end;

procedure TMousePtr.SetPlotDirty( const Value : boolean );
const
  FailName : string = 'TMousePtr.SetPlotDirty';
begin
  Log.DebugLog( FailName );
  try
    FPlotDirty := Value;
    if not FPlotDirty then
      MouseTimerEvent( nil );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TMousePtr.Cleanup;
const
  FailName : string = 'TMousePtr.Cleanup';
var
  pr : TRect;
begin
  Log.DebugLog( FailName );
  try
    if FPlotDirty then
    begin
      pr := Rect( 0, 0, PtrWidth - WAdj, PtrHeight - HAdj );
      DXSurface.BltFast( mPt.x, mPt.y, DXDirty, @pr, DDBLTFAST_WAIT );
      FPlotDirty := false;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TMousePtr.SetEnabled( const Value : boolean );
const
  FailName : string = 'TMousePtr.SetEnabled';
begin
  Log.DebugLog( FailName );
  try
    if FEnabled = Value then
      exit;

    FEnabled := Value;
    if FEnabled then
    begin
      PlotDirty := false;
      MouseTimer.enabled := True;
    end
    else
    begin
      MouseTimer.enabled := False;
      Cleanup;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TMousePtr.SetFrame( Frame : integer );
begin
//  StartFrame := Frame;
//  MouseAnimationCycle := 0;
//  Loop := false;
  MouseFrame := Frame;
end;

end.
