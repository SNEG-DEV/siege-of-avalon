unit Intro;
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
  Resource,
  StdCtrls,
  GameText,
  Display,
  Anigrp30,
  Engine,
  LogFile;


type
  TIntroRect = record
    Rect : TRect;
    Image : IDirectDrawSurface;
    Enabled : boolean;
  end;

  TIntro = class( TDisplay )
  private
    AreYouSureBoxVisible : boolean;
    DXBack : IDirectDrawSurface;
    PrevChoice : integer;
    txtMessage : array[ 0..1 ] of string;
    procedure AreYouSure;
  protected
    procedure MouseDown( Sender : TAniview; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
    procedure MouseMove( Sender : TAniview;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
    procedure KeyDown( Sender : TObject; var key : Word; Shift : TShiftState ); override;
  public
    Captions : array[ 1..8 ] of TIntroRect;
    MenuChoice : integer;
    constructor Create;
    destructor Destroy; override;
    procedure Init; override;
    procedure Release; override;
  end;

implementation

uses
  AniDemo;

const
  XFrame = 106;
  YFrame = 41;


procedure MakeRect( var Caption : TIntroRect; X, Y : integer; BM : TBitmap );
const
  FailName : string = 'Intro.MakeRect';
  W = 582;
  H = 52;
var
  DC : HDC;
  //DXTemp : IDirectDrawSurface;
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Caption.Rect := Rect( X, Y, X + W, Y + H );
    Caption.Image := DDGetSurface( lpDD, W, H, $FF04FF, false );
    Caption.Image.GetDC( DC );
    try
      BitBlt( DC, 0, 0, W, H, BM.canvas.handle, X - XFrame, Y - YFrame, SRCCOPY );
    finally
      Caption.Image.ReleaseDC( DC );
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TIntro }

constructor TIntro.Create;
const
  FailName : string = 'TIntro.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end; //Create

destructor TIntro.Destroy;
const
  FailName : string = 'TIntro.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end; //Destroy

procedure TIntro.Init;
var
  BM : TBitmap;
  DC : HDC;
  Y1 : integer;
const
  FailName : string = 'TIntro.Init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if Loaded then
      Exit;
    inherited;
    ExText.Open( 'Intro' );
    txtMessage[ 0 ] := ExText.GetText( 'Message' + inttostr( 0 ) );

    PrevChoice := 0;
    AreYouSureBoxVisible := false;
    pText.LoadFontGraphic( 'createchar' );

    BM := TBitmap.Create;
    try
      BM.LoadFromFile( InterfacePath + 'gMainMenuBlank.bmp' );
      DXBack := DDGetImage( lpDD, BM, $00FFFF00, true );

      BM.LoadFromFile( InterfacePath + LanguagePath + 'gMainMenuText.bmp' );
      DXBack.GetDC( DC );
      try
        BitBlt( DC, 106, 41, 582, 416, BM.canvas.handle, 0, 0, SRCCOPY );
      finally
        DXBack.ReleaseDC( DC );
      end;

      WrapperBltFast( lpDDSBack, 0, 0, DXBack, Rect( 0, 0, 800, 600 ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );

      BM.LoadFromFile( InterfacePath + LanguagePath + 'gMainMenuTextBttns.bmp' );
      Y1 := YFrame;
      MakeRect( Captions[ 1 ], XFrame, Y1, BM );

      inc( Y1, 52 );
      MakeRect( Captions[ 2 ], XFrame, Y1, BM );

      inc( Y1, 52 );
      MakeRect( Captions[ 3 ], XFrame, Y1, BM );

      inc( Y1, 52 );
      MakeRect( Captions[ 4 ], XFrame, Y1, BM );

      inc( Y1, 52 );
      MakeRect( Captions[ 5 ], XFrame, Y1, BM );

      inc( Y1, 52 );
      MakeRect( Captions[ 6 ], XFrame, Y1, BM );

      inc( Y1, 52 );
      MakeRect( Captions[ 7 ], XFrame, Y1, BM );

      inc( Y1, 52 );
      MakeRect( Captions[ 8 ], XFrame, Y1, BM );

    finally
      BM.Free;
    end;

    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end; //Init


procedure TIntro.KeyDown( Sender : TObject; var key : Word; Shift : TShiftState );
begin
    //pFloater.init;
end;

procedure TIntro.MouseDown( Sender : TAniview; Button : TMouseButton; Shift : TShiftState; X, Y, GridX, GridY : integer );
var
  i : integer;
const
  FailName : string = 'TIntro.MouseDown';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    MenuChoice := 0;

    if AreYouSureBoxVisible = false then
    begin
      for i := 1 to 8 do
      begin
        if Captions[ i ].enabled and ( X >= Captions[ i ].Rect.Left ) and ( Y >= Captions[ i ].Rect.Top ) and ( X < Captions[ i ].Rect.Right ) and ( Y < Captions[ i ].Rect.Bottom ) then
        begin
          MenuChoice := i;
          break;
        end;
      end;
      if MenuChoice = 7 then
      begin //they chose exit- display AreYouSure Box
        AreYouSure;
      end
      else if MenuChoice > 0 then
      begin
        Close;
      end;
    end
    else
    begin //check for clicks on Yes/No in AreYouSurebox
      if PtInRect( rect( 303, 456, 352, 484 ), point( x, y ) ) then
      begin //Yes pressed- quit game
        MenuChoice := 7;
        Close;
      end
      else if PtInRect( rect( 438, 456, 488, 484 ), point( x, y ) ) then
      begin //No pressed- just show screen
        AreYouSureBoxVisible := false;
        WrapperBltFast( lpDDSBack, 0, 0, DXBack, rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT ); //clear screen
        lpDDSFront.Flip( nil, DDFLIP_WAIT );
        WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
        MouseCursor.PlotDirty := false;
      end; //endif PtInRect
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;


procedure TIntro.MouseMove( Sender : TAniview; Shift : TShiftState; X, Y,
  GridX, GridY : integer );
var
  Choice : integer;
  i : integer;
const
  FailName : string = 'TIntro.MouseMove';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Choice := 0;
    if AreYouSureBoxVisible = false then
    begin
      for i := 1 to 8 do
      begin
        if Captions[ i ].enabled and ( X >= Captions[ i ].Rect.Left ) and ( Y >= Captions[ i ].Rect.Top ) and ( X < Captions[ i ].Rect.Right ) and ( Y < Captions[ i ].Rect.Bottom ) then
        begin
          Choice := i;
          if Choice <> PrevChoice then
          begin
            WrapperBltFast( lpDDSBack, 0, 0, DXBack, Rect( 0, 0, 800, 600 ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
            WrapperBltFast( lpDDSBack, Captions[ i ].Rect.Left, Captions[ i ].Rect.Top, Captions[ i ].Image,
              Rect( 0, 0, Captions[ i ].Rect.Right - Captions[ i ].Rect.Left, Captions[ i ].Rect.Bottom - Captions[ i ].Rect.Top ),
              DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
            lpDDSFront.Flip( nil, DDFLIP_WAIT );
            MouseCursor.PlotDirty := false;
          end;
          break;
        end;
      end;

      if ( Choice = 0 ) and ( Choice <> PrevChoice ) then
      begin
        WrapperBltFast( lpDDSBack, 0, 0, DXBack, Rect( 0, 0, 800, 600 ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
        lpDDSFront.Flip( nil, DDFLIP_WAIT );
        MouseCursor.PlotDirty := false;
      end;
      PrevChoice := Choice;
    end; //endif areyousure

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TIntro.AreYouSure;
var
  BM : TBitmap;
  DXBorders : IDirectDrawSurface;
  InvisColor : integer;
  nRect : TRect;
const
  FailName : string = 'TIntro.AreYouSure ';
begin

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    BM := TBitmap.Create;
  //transparent color
    InvisColor := $00FFFF00;

    BM.LoadFromFile( InterfacePath + LanguagePath + 'ldChooseBox.bmp' );
    DXBorders := DDGetImage( lpDD, BM, InvisColor, False );
    nRect := Captions[ 7 ].Rect; //Exit

    WrapperBltFast( lpDDSBack, 0, 0, DXBack, rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
    WrapperBltFast( lpDDSBack, 400 - BM.width div 2, nRect.top + 32, DXBorders, Rect( 0, 0, BM.width, BM.Height ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

    DXBorders := nil;

    pText.PlotTextBlock( txtMessage[ 0 ], 400 - BM.width div 2 + 23, 400 - BM.width div 2 + 281, nRect.top + 52, 240 );

    AreYouSureBoxVisible := true;

    BM.Free;

    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end; //AreYouSure


procedure TIntro.Release;
var
  i : integer;
const
  FailName : string = 'TIntro.Release';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    ExText.close;
    for i := 1 to 8 do
    begin
      Captions[ i ].Image := nil;
    end;

    inherited;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;


end.

