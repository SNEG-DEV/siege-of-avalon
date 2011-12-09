unit MousePtr;
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
{$IFDEF DirectX}
{$IFDEF DX5}
  DirectX,
{$ELSE}
  DirectDraw,
{$ENDIF}
  DXUtil,
  DXEffects,
{$ENDIF}
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  Character,
  StdCtrls,
  Anigrp30,
  logger,
  sdl;

type
  TMousePtr = class( TObject )
  private
    BMBack : TBitmap;
    DXMousePtr : PSDL_Surface;
    DXDirty : PSDL_Surface;
    MouseTimer : TTimer;
    Mpt : TPoint;
    MouseCounter : integer;
    MouseFrame : integer;
    MouseAnimationCycle : integer; //speed of frame change
    StartFrame : integer;
    PlayFrames : integer;
    Loop : boolean;
    OldStartFrame : integer;
    OldSpeed : integer;
    OldLoop : boolean;
    FPlotDirty : boolean; //plot cleanup or no?
    WAdj, HAdj, OldWAdj, OldHAdj : integer;
    FEnabled : boolean;
    procedure MouseTimerEvent( Sender : TObject );
    procedure SetPlotDirty( const Value : boolean );
    procedure SetEnabled( const Value : boolean );
  protected
  public
    DxSurface : PSDL_Surface; //surface to draw pointer to
    constructor Create;
    destructor Destroy; override;
    procedure SetAnim( Frame, Frames, Speed : integer );
    procedure Cleanup;
    procedure SetLoopAnim( Frame, Frames, Speed : integer );
    procedure SetFrame( Frame : integer );
    property PlotDirty : boolean read FPlotDirty write SetPlotDirty; //plot cleanup or no?
    property Enabled : boolean read FEnabled write SetEnabled;
  end;

implementation

uses
  //AniDemo;}
  globals;

const
  PtrWidth = 32;
  PtrHeight = 32;
  SheetWidth = 6;
{ TMousePtr }

constructor TMousePtr.Create;
var
  InvisColor : integer;
const
  FailName : string = 'TMousePtr.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
    BMBack := TBitmap.Create;
  //transparent color
    InvisColor := rgb( 255, 0, 255 );

    DXMousePtr := SDL_LoadBMP( PChar( SoASettings.InterfacePath  + '/siegecursorsheet.bmp' ) );
  //lpDDSBack.BltFast(0, 0, DXMousePtr, Rect(0, 0, PtrWidth, PtrHeight), DDBLTFAST_WAIT);

    BMBack.Free;

    //DXDirty := DDGetSurface( lpDD, PtrWidth, PtrHeight, InvisColor, true );
  //pre-load Dirty

    FPlotDirty := false;
    //DXSurface := lpDDSFront;

    {GetCursorPos( mPt );
    WrapperBltFast( DXDirty, 0, 0, DXSurface, Rect( mPt.x, mPt.y, mPt.x + PtrWidth, mPt.y + PtrHeight ), DDBLTFAST_WAIT );}



    MouseTimer := TTimer.create( nil );
//  MouseTimer:=TAniTimer.create(nil);
    MouseTimer.onTimer := MouseTimerEvent;
//  MouseTimer.TimerPriority:=tpNormal;
    MouseTimer.Interval := 10;
//  MouseTimer.resolution := 1;
    MouseTimer.enabled := false;
  except
    on E : Exception do
      Log.LogError( E.Message, FailName );
  end;

end; //Create

destructor TMousePtr.Destroy;
const
  FailName : string = 'TMousePtr.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    MouseTimer.enabled := false;
    MouseTimer.onTimer := nil;
    MouseTimer.free;
    DXDirty := nil;
    DXMousePtr := nil;
    inherited;
  except
    on E : Exception do
      Log.LogError( E.Message, FailName );
  end;

end; //Destroy


procedure TMousePtr.MouseTimerEvent( Sender : TObject );
var
  PrevPt : TPoint;
  X, Y : longint;
begin
  if not FEnabled then
    exit;

  PrevPt := mPt;
  GetCursorPos( mPt );

  if ( MouseAnimationCycle > 0 ) and ( MouseCounter > MouseAnimationCycle ) then
  begin
    inc( MouseFrame );
    if MouseFrame >= PlayFrames then
    begin
      if Loop then
      begin
        MouseFrame := StartFrame;
      end
      else
      begin
        StartFrame := OldStartFrame;
        MouseAnimationCycle := OldSpeed;
        Loop := OldLoop;
        MouseFrame := StartFrame;
      end;
    end;
    MouseCounter := 0;
  end;

  if assigned( Sender ) then
    inc( MouseCounter );

  OldWAdj := WAdj;
  OldHAdj := HAdj;
  WAdj := 0;
  HAdj := 0;
  if mPt.x + PtrWidth > 800 then
    WAdj := ( mPt.x + PtrWidth ) - 800;
      //mPt.x:=800-PtrWidth;
  if mPt.y + PtrHeight > 600 then
    HAdj := ( mPt.y + PtrHeight ) - 600;
      //mPt.y:=600-PtrHeight;

  if FPlotDirty then
  begin
    if ( mPt.x <> PrevPt.x ) or ( mPt.y <> PrevPt.y ) then
    begin
      WrapperBltFast( DXSurface, PrevPt.x, PrevPt.y, DXDirty, Rect( 0, 0, PtrWidth - OldWAdj, PtrHeight - OldHAdj ), DDBLTFAST_WAIT );
      WrapperBltFast( DXDirty, 0, 0, DXSurface, Rect( mPt.x, mPt.y, mPt.x + PtrWidth - WAdj, mPt.y + PtrHeight - HAdj ), DDBLTFAST_WAIT );
    end;
  end
  else
  begin
    WrapperBltFast( DXDirty, 0, 0, DXSurface, Rect( mPt.x, mPt.y, mPt.x + PtrWidth - WAdj, mPt.y + PtrHeight - HAdj ), DDBLTFAST_WAIT );
  end;

  X := ( MouseFrame mod SheetWidth ) * PtrWidth;
  Y := ( MouseFrame div SheetWidth ) * PtrHeight;
  WrapperBltFast( DXSurface, mPt.x, mPt.y, DXMousePtr, Rect( X, Y, X + PtrWidth - WAdj, Y + PtrHeight - HAdj ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
  FPlotDirty := true;
end; //MouseTimerEvent


procedure TMousePtr.SetAnim( Frame, Frames, Speed : integer );
const
  FailName : string = 'TMousePtr.SetAnim';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if MouseAnimationCycle > 0 then
      exit;
    OldStartFrame := StartFrame;
    OldSpeed := MouseAnimationCycle;
    OldLoop := Loop;
    StartFrame := Frame;
    PlayFrames := Frame + Frames;
    MouseAnimationCycle := Speed;
    Loop := false;
    MouseCounter := 0;
    MouseFrame := StartFrame;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TMousePtr.SetLoopAnim( Frame, Frames, Speed : integer );
const
  FailName : string = 'TMousePtr.SetloopAnim';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if MouseAnimationCycle > 0 then
      exit;
    StartFrame := Frame;
    PlayFrames := Frame + Frames;
    MouseAnimationCycle := Speed;
    Loop := true;
    MouseCounter := 0;
    MouseFrame := StartFrame;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TMousePtr.SetPlotDirty( const Value : boolean );
const
  FailName : string = 'TMousePtr.SetPlotDirty';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
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
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if FPlotDirty then
    begin
      WrapperBltFast( DXSurface, mPt.x, mPt.y, DXDirty, Rect( 0, 0, PtrWidth - WAdj, PtrHeight - HAdj ), DDBLTFAST_WAIT );
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
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
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
  StartFrame := Frame;
  MouseAnimationCycle := 0;
  Loop := false;
  MouseFrame := Frame;
end;

end.

