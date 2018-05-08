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
  DirectX,
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

//mit gMainMenuTextBttns.bmp
(*procedure MakeRect( var Caption : TIntroRect; X, Y : integer; BM : TBitmap );
const
  FailName : string = 'Intro.MakeRect';
  W = 582;
  H = 52;
var
  DC : HDC;
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Caption.Rect := Rect( X, Y, X + W, Y + H );
    Caption.Image := DDGetSurface( lpDD, W, H, $FF00FF, true );
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
end;*)

//Neues Spiel
procedure MakeRect1( var Caption : TIntroRect; X, Y : integer; BM : TBitmap );
const
  FailName : string = 'Intro.MakeRect';
  W = 285;
  H = 47;
var
  DC : HDC;
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Caption.Rect := Rect( 255, 49, 255 + W, 49 + H );
    Caption.Image := DDGetSurface( lpDD, W, H, $FF00FF, true );
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

//Laden
procedure MakeRect2( var Caption : TIntroRect; X, Y : integer; BM : TBitmap );
const
  FailName : string = 'Intro.MakeRect';
  W = 150;
  H = 56;
var
  DC : HDC;
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Caption.Rect := Rect( 323, 94, 323 + W, 94 + H );
    Caption.Image := DDGetSurface( lpDD, W, H, $FF00FF, true );
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

//Speichern
procedure MakeRect3( var Caption : TIntroRect; X, Y : integer; BM : TBitmap );
const
  FailName : string = 'Intro.MakeRect';
  W = 215;
  H = 45;
var
  DC : HDC;
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Caption.Rect := Rect( 294, 148, 294 + W, 148 + H );
    Caption.Image := DDGetSurface( lpDD, W, H, $FF00FF, true );
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

//Optionen
procedure MakeRect4( var Caption : TIntroRect; X, Y : integer; BM : TBitmap );
const
  FailName : string = 'Intro.MakeRect';
  W = 234;
  H = 59;
var
  DC : HDC;
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Caption.Rect := Rect( 284, 191, 284+ W, 191 + H );
    Caption.Image := DDGetSurface( lpDD, W, H, $FF00FF, true );
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

//Homepage
procedure MakeRect5( var Caption : TIntroRect; X, Y : integer; BM : TBitmap );
const
  FailName : string = 'Intro.MakeRect';
  W = 274;
  H = 45;
var
  DC : HDC;
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Caption.Rect := Rect( 260, 254, 260 + W, 254 + H );
    Caption.Image := DDGetSurface( lpDD, W, H, $FF00FF, true );
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

//Herausgeber
procedure MakeRect6( var Caption : TIntroRect; X, Y : integer; BM : TBitmap );
const
  FailName : string = 'Intro.MakeRect';
  W = 214;
  H = 62;
var
  DC : HDC;
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Caption.Rect := Rect( 294, 297, 294 + W, 297 + H - 5);
    Caption.Image := DDGetSurface( lpDD, W, H, $FF00FF, true );
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

//Beenden
procedure MakeRect7( var Caption : TIntroRect; X, Y : integer; BM : TBitmap );
const
  FailName : string = 'Intro.MakeRect';
  W = 199;
  H = 45;
var
  DC : HDC;
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Caption.Rect := Rect( 300, 353, 300 + W, 353 + H );
    Caption.Image := DDGetSurface( lpDD, W, H, $FF00FF, true );
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

//Fortsetzen
procedure MakeRect8( var Caption : TIntroRect; X, Y : integer; BM : TBitmap );
const
  FailName : string = 'Intro.MakeRect';
  W = 395;
  H = 47;
var
  DC : HDC;
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Caption.Rect := Rect( 197, 400, 197 + W, 400 + H );
    Caption.Image := DDGetSurface( lpDD, W, H, $FF00FF, true );
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
      BM.LoadFromFile( InterfacePath + 'gMainMenu.bmp' );
      DXBack := DDGetImage( lpDD, BM, $00FFFF00, true );

      //für gMainMenuTextBttns.bmp
      {BM.LoadFromFile( InterfacePath + 'gMainMenublank.bmp' );
      DXBack := DDGetImage( lpDD, BM, $00FFFF00, true );

      BM.LoadFromFile( InterfacePath + 'gMainMenuText.bmp' );
      DXBack.GetDC( DC );
      try
        BitBlt( DC, 106, 41, 582, 416, BM.canvas.handle, 0, 0, SRCCOPY );
      finally
        DXBack.ReleaseDC( DC );
      end;}

      lpDDSBack.BltFast( 0, 0, DXBack, Rect( 0, 0, 800, 600 ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );

      //mit gMainMenuTextBttns.bmp
      {BM.LoadFromFile( InterfacePath + 'gMainMenuTextBttns.bmp' );
      MakeRect1( Captions[ 1 ], XFrame, Y1, BM );
      inc( Y1, 52 );
      MakeRect2( Captions[ 2 ], XFrame, Y1, BM );
      inc( Y1, 52 );
      MakeRect3( Captions[ 3 ], XFrame, Y1, BM );
      inc( Y1, 52 );
      MakeRect4( Captions[ 4 ], XFrame, Y1, BM );
      inc( Y1, 52 );
      MakeRect5( Captions[ 5 ], XFrame, Y1, BM );
      inc( Y1, 52 );
      MakeRect6( Captions[ 6 ], XFrame, Y1, BM );
      inc( Y1, 52 );
      MakeRect7( Captions[ 7 ], XFrame, Y1, BM );
      inc( Y1, 52 );
      MakeRect8( Captions[ 8 ], XFrame, Y1, BM );}

      Y1 := YFrame;
      BM.LoadFromFile( InterfacePath + 'gnew.bmp' );
      MakeRect1( Captions[ 1 ], XFrame, Y1, BM );
      BM.LoadFromFile( InterfacePath + 'gload.bmp' );
      MakeRect2( Captions[ 2 ], XFrame, Y1, BM );
      BM.LoadFromFile( InterfacePath + 'gsave.bmp' );
      MakeRect3( Captions[ 3 ], XFrame, Y1, BM );
      BM.LoadFromFile( InterfacePath + 'goptions.bmp' );
      MakeRect4( Captions[ 4 ], XFrame, Y1, BM );
      BM.LoadFromFile( InterfacePath + 'gupdate.bmp' );
      MakeRect5( Captions[ 5 ], XFrame, Y1, BM );
      BM.LoadFromFile( InterfacePath + 'gcredits.bmp' );
      MakeRect6( Captions[ 6 ], XFrame, Y1, BM );
      BM.LoadFromFile( InterfacePath + 'gexit.bmp' );
      MakeRect7( Captions[ 7 ], XFrame, Y1, BM );
      BM.LoadFromFile( InterfacePath + 'gResume.bmp' );
      MakeRect8( Captions[ 8 ], XFrame, Y1, BM );

    finally
      BM.Free;
    end;

    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    lpDDSBack.BltFast( 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
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
    if captions[ 1 ].enabled and ( X >= Captions[ 1 ].Rect.Left ) and ( Y >= Captions[ 1 ].Rect.Top ) and ( X < Captions[ 1 ].Rect.Right ) and ( Y < Captions[ 1 ].Rect.Bottom ) then
    begin
    MenuChoice := 1;
    end;
    if captions[ 2 ].enabled and ( X >= Captions[ 2 ].Rect.Left ) and ( Y >= Captions[ 2 ].Rect.Top ) and ( X < Captions[ 2 ].Rect.Right ) and ( Y < Captions[ 2 ].Rect.Bottom ) then
    begin
    MenuChoice := 2;
    end;
    if captions[ 3 ].enabled and ( X >= Captions[ 3 ].Rect.Left ) and ( Y >= Captions[ 3 ].Rect.Top ) and ( X < Captions[ 3 ].Rect.Right ) and ( Y < Captions[ 3 ].Rect.Bottom ) then
    begin
    MenuChoice := 3;
    end;
    if captions[ 4 ].enabled and ( X >= Captions[ 4 ].Rect.Left ) and ( Y >= Captions[ 4 ].Rect.Top ) and ( X < Captions[ 4 ].Rect.Right ) and ( Y < Captions[ 4 ].Rect.Bottom ) then
    begin
    MenuChoice := 4;
    end;
    if captions[ 5 ].enabled and ( X >= Captions[ 5 ].Rect.Left ) and ( Y >= Captions[ 5 ].Rect.Top ) and ( X < Captions[ 5 ].Rect.Right ) and ( Y < Captions[ 5 ].Rect.Bottom ) then
    begin
    MenuChoice := 5;
    end;
    if captions[ 6 ].enabled and ( X >= Captions[ 6 ].Rect.Left ) and ( Y >= Captions[ 6 ].Rect.Top ) and ( X < Captions[ 6 ].Rect.Right ) and ( Y < Captions[ 6 ].Rect.Bottom ) then
    begin
    MenuChoice := 6;
    end;
    if captions[ 7 ].enabled and ( X >= Captions[ 7 ].Rect.Left ) and ( Y >= Captions[ 7 ].Rect.Top ) and ( X < Captions[ 7 ].Rect.Right ) and ( Y < Captions[ 7 ].Rect.Bottom ) then
    begin
     AreYouSure;
    end;
    if captions[ 8 ].enabled and ( X >= Captions[ 8 ].Rect.Left ) and ( Y >= Captions[ 8 ].Rect.Top ) and ( X < Captions[ 8 ].Rect.Right ) and ( Y < Captions[ 8 ].Rect.Bottom ) then
    begin
    MenuChoice := 8;
    end;
    if (MenuChoice > 0) and (MenuChoice <> 7) then
    begin
    Close;
    end;
    end
    else
    if PtInRect( rect( 303, 456, 352, 484 ), point( x, y ) ) then
      begin //Sind sie Sicher? Ja
        MenuChoice := 7;
        Close;
      end
      else if PtInRect( rect( 438, 456, 488, 484 ), point( x, y ) ) then
      begin //Sind sie sicher? Nein
        AreYouSureBoxVisible := false;
        lpDDSBack.BltFast( 0, 0, DXBack, rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT ); //clear screen
        lpDDSFront.Flip( nil, DDFLIP_WAIT );
        lpDDSBack.BltFast( 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
        MouseCursor.PlotDirty := false;
      end;

     //mit gMainMenuTextBttns.bmp
    {for i := 1 to 8 do
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
        lpDDSBack.BltFast( 0, 0, DXBack, rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT ); //clear screen
        lpDDSFront.Flip( nil, DDFLIP_WAIT );
        lpDDSBack.BltFast( 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
        MouseCursor.PlotDirty := false;
      end; //endif PtInRect
    end;}
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
    if captions[ 1 ].enabled and ( X >= Captions[ 1 ].Rect.Left ) and ( Y >= Captions[ 1 ].Rect.Top ) and ( X < Captions[ 1 ].Rect.Right ) and ( Y < Captions[ 1 ].Rect.Bottom ) then
    begin
    choice := 1;
    if Choice <> PrevChoice then
    begin
    lpDDSBack.BltFast( 0, 0, DXBack, Rect( 0, 0, 800, 600 ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
    lpDDSBack.BltFast( Captions[ 1 ].Rect.Left, Captions[ 1 ].Rect.Top, Captions[ 1 ].Image,
    Rect( 0, 0, Captions[ 1 ].Rect.Right - Captions[ 1 ].Rect.Left, Captions[ 1 ].Rect.Bottom - Captions[ 1 ].Rect.Top ),
    DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    MouseCursor.PlotDirty := false;
    end;
    end;
    if captions[ 2 ].enabled and ( X >= Captions[ 2 ].Rect.Left ) and ( Y >= Captions[ 2 ].Rect.Top ) and ( X < Captions[ 2 ].Rect.Right ) and ( Y < Captions[ 2 ].Rect.Bottom ) then
    begin
    Choice := 2;
    if Choice <> PrevChoice then
    begin
    lpDDSBack.BltFast( 0, 0, DXBack, Rect( 0, 0, 800, 600 ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
    lpDDSBack.BltFast( Captions[ 2 ].Rect.Left, Captions[ 2 ].Rect.Top, Captions[ 2 ].Image,
    Rect( 0, 0, Captions[ 2 ].Rect.Right - Captions[ 2 ].Rect.Left, Captions[ 2 ].Rect.Bottom - Captions[ 2 ].Rect.Top ),
    DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    MouseCursor.PlotDirty := false;
    end;
    end;
    if captions[ 3 ].enabled and ( X >= Captions[ 3 ].Rect.Left ) and ( Y >= Captions[ 3 ].Rect.Top ) and ( X < Captions[ 3 ].Rect.Right ) and ( Y < Captions[ 3 ].Rect.Bottom ) then
    begin
    Choice := 3;
    if Choice <> PrevChoice then
    begin
    lpDDSBack.BltFast( 0, 0, DXBack, Rect( 0, 0, 800, 600 ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
    lpDDSBack.BltFast( Captions[ 3 ].Rect.Left, Captions[ 3 ].Rect.Top, Captions[ 3 ].Image,
    Rect( 0, 0, Captions[ 3 ].Rect.Right - Captions[ 3 ].Rect.Left, Captions[ 3 ].Rect.Bottom - Captions[ 3 ].Rect.Top ),
    DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    MouseCursor.PlotDirty := false;
    end;
    end;
    if captions[ 4 ].enabled and ( X >= Captions[ 4 ].Rect.Left ) and ( Y >= Captions[ 4 ].Rect.Top ) and ( X < Captions[ 4 ].Rect.Right ) and ( Y < Captions[ 4 ].Rect.Bottom ) then
    begin
    Choice := 4;
    if Choice <> PrevChoice then
    begin
    lpDDSBack.BltFast( 0, 0, DXBack, Rect( 0, 0, 800, 600 ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
    lpDDSBack.BltFast( Captions[ 4 ].Rect.Left, Captions[ 4 ].Rect.Top, Captions[ 4 ].Image,
    Rect( 0, 0, Captions[ 4 ].Rect.Right - Captions[ 4 ].Rect.Left, Captions[ 4 ].Rect.Bottom - Captions[ 4 ].Rect.Top ),
    DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    MouseCursor.PlotDirty := false;
    end;
    end;
    if captions[ 5 ].enabled and ( X >= Captions[ 5 ].Rect.Left ) and ( Y >= Captions[ 5 ].Rect.Top ) and ( X < Captions[ 5 ].Rect.Right ) and ( Y < Captions[ 5 ].Rect.Bottom ) then
    begin
    Choice := 5;
    if Choice <> PrevChoice then
    begin
    lpDDSBack.BltFast( 0, 0, DXBack, Rect( 0, 0, 800, 600 ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
    lpDDSBack.BltFast( Captions[ 5 ].Rect.Left, Captions[ 5 ].Rect.Top, Captions[ 5 ].Image,
    Rect( 0, 0, Captions[ 5 ].Rect.Right - Captions[ 5 ].Rect.Left, Captions[ 5 ].Rect.Bottom - Captions[ 5 ].Rect.Top ),
    DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    MouseCursor.PlotDirty := false;
    end;
    end;
    if captions[ 6 ].enabled and ( X >= Captions[ 6 ].Rect.Left ) and ( Y >= Captions[ 6 ].Rect.Top ) and ( X < Captions[ 6 ].Rect.Right ) and ( Y < Captions[ 6 ].Rect.Bottom ) then
    begin
    Choice := 6;
    if Choice <> PrevChoice then
    begin
    lpDDSBack.BltFast( 0, 0, DXBack, Rect( 0, 0, 800, 600 ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
    lpDDSBack.BltFast( Captions[ 6 ].Rect.Left, Captions[ 6 ].Rect.Top, Captions[ 6 ].Image,
    Rect( 0, 0, Captions[ 6 ].Rect.Right - Captions[ 6 ].Rect.Left, Captions[ 6 ].Rect.Bottom - Captions[ 6 ].Rect.Top ),
    DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    MouseCursor.PlotDirty := false;
    end;
    end;
    if captions[ 7 ].enabled and ( X >= Captions[ 7 ].Rect.Left ) and ( Y >= Captions[ 7 ].Rect.Top ) and ( X < Captions[ 7 ].Rect.Right ) and ( Y < Captions[ 7 ].Rect.Bottom ) then
    begin
    Choice := 7;
    if Choice <> PrevChoice then
    begin
    lpDDSBack.BltFast( 0, 0, DXBack, Rect( 0, 0, 800, 600 ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
    lpDDSBack.BltFast( Captions[ 7 ].Rect.Left, Captions[ 7 ].Rect.Top, Captions[ 7 ].Image,
    Rect( 0, 0, Captions[ 7 ].Rect.Right - Captions[ 7 ].Rect.Left, Captions[ 7 ].Rect.Bottom - Captions[ 7 ].Rect.Top ),
    DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    MouseCursor.PlotDirty := false;
    end;
    end;
    if captions[ 8 ].enabled and ( X >= Captions[ 8 ].Rect.Left ) and ( Y >= Captions[ 8 ].Rect.Top ) and ( X < Captions[ 8 ].Rect.Right ) and ( Y < Captions[ 8 ].Rect.Bottom ) then
    begin
    Choice := 8;
    if Choice <> PrevChoice then
    begin
    lpDDSBack.BltFast( 0, 0, DXBack, Rect( 0, 0, 800, 600 ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
    lpDDSBack.BltFast( Captions[ 8 ].Rect.Left, Captions[ 8 ].Rect.Top, Captions[ 8 ].Image,
    Rect( 0, 0, Captions[ 8 ].Rect.Right - Captions[ 8 ].Rect.Left, Captions[ 8 ].Rect.Bottom - Captions[ 8 ].Rect.Top ),
    DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    MouseCursor.PlotDirty := false;
    end;
 end;

     //mit gMainMenuTextBttns.bmp
      {for i := 1 to 8 do
      begin
        if Captions[ i ].enabled and ( X >= Captions[ i ].Rect.Left ) and ( Y >= Captions[ i ].Rect.Top ) and ( X < Captions[ i ].Rect.Right ) and ( Y < Captions[ i ].Rect.Bottom ) then
        begin
          Choice := i;
          if Choice <> PrevChoice then
          begin
            lpDDSBack.BltFast( 0, 0, DXBack, Rect( 0, 0, 800, 600 ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
            lpDDSBack.BltFast( Captions[ i ].Rect.Left, Captions[ i ].Rect.Top, Captions[ i ].Image,
              Rect( 0, 0, Captions[ i ].Rect.Right - Captions[ i ].Rect.Left, Captions[ i ].Rect.Bottom - Captions[ i ].Rect.Top ),
              DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
            lpDDSFront.Flip( nil, DDFLIP_WAIT );
            MouseCursor.PlotDirty := false;
          end;
          break;
        end;
      end;  }

      if ( Choice = 0 ) and ( Choice <> PrevChoice ) then
      begin
        lpDDSBack.BltFast( 0, 0, DXBack, Rect( 0, 0, 800, 600 ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
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

    BM.LoadFromFile( InterfacePath + 'ldChooseBox.bmp' );
    DXBorders := DDGetImage( lpDD, BM, InvisColor, False );
    nRect := Captions[ 7 ].Rect; //Exit

    lpDDSBack.BltFast( 0, 0, DXBack, rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
    lpDDSBack.BltFast( 400 - BM.width div 2, nRect.top + 32, DXBorders, Rect( 0, 0, BM.width, BM.Height ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

    DXBorders := nil;

    pText.PlotTextBlock( txtMessage[ 0 ], 400 - BM.width div 2 + 23, 400 - BM.width div 2 + 281, nRect.top + 52, 240 );

    AreYouSureBoxVisible := true;

    BM.Free;

    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    lpDDSBack.BltFast( 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
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
