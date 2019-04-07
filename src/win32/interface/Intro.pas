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

interface

uses
{$IFDEF DirectX}
  DirectX,
  DXUtil,
{$ENDIF}
  Winapi.Windows,
  System.SysUtils,
  System.Types,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
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
  SoAOS.Types,
  SoAOS.Graphics.Draw,
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
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Caption.Rect := Rect( X, Y, X + W, Y + H );
    Caption.Image := DDGetSurface( lpDD, W, H, cInvisIntro, true );
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
  pr : TRect;
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
      DXBack := SoAOS_DX_SurfaceFromBMP( BM, cInvisColor );

      BM.LoadFromFile( InterfacePath + 'gMainMenuText.bmp' );
      DXBack.GetDC( DC );
      try
        BitBlt( DC, 106, 41, 582, 416, BM.canvas.handle, 0, 0, SRCCOPY );
      finally
        DXBack.ReleaseDC( DC );
      end;

      pr := Rect( 0, 0, 800, 600 ); //NOHD
      lpDDSBack.BltFast( 0, 0, DXBack, @pr, DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );

      BM.LoadFromFile( InterfacePath + 'gMainMenuTextBttns.bmp' );
      Y1 := YFrame;
      MakeRect( Captions[ 1 ], XFrame, Y1, BM );  // New game

      inc( Y1, 52 );
      MakeRect( Captions[ 2 ], XFrame, Y1, BM );  // Load

      inc( Y1, 52 );
      MakeRect( Captions[ 3 ], XFrame, Y1, BM );  // Save

      inc( Y1, 52 );
      MakeRect( Captions[ 4 ], XFrame, Y1, BM );  // Options

      inc( Y1, 52 );
      MakeRect( Captions[ 5 ], XFrame, Y1, BM );  // History

      inc( Y1, 52 );
      MakeRect( Captions[ 6 ], XFrame, Y1, BM );  // Credits

      inc( Y1, 52 );
      MakeRect( Captions[ 7 ], XFrame, Y1, BM );  // Exit

      inc( Y1, 52 );
      MakeRect( Captions[ 8 ], XFrame, Y1, BM );  // Resume

    finally
      BM.Free;
    end;

    SoAOS_DX_BltFront;
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
  pr : TRect;
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
        pr := Rect( 0, 0, 800, 600 );  //NOHD
        lpDDSBack.BltFast( 0, 0, DXBack, @pr, DDBLTFAST_WAIT ); //clear screen
        SoAOS_DX_BltFront;
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
  pr : TRect;
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
            pr := Rect( 0, 0, 800, 600 ); //NOHD
            lpDDSBack.BltFast( 0, 0, DXBack, @pr, DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
            pr := Rect( 0, 0, Captions[ i ].Rect.Right - Captions[ i ].Rect.Left, Captions[ i ].Rect.Bottom - Captions[ i ].Rect.Top );
            lpDDSBack.BltFast( Captions[ i ].Rect.Left, Captions[ i ].Rect.Top, Captions[ i ].Image, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
            lpDDSFront.Flip( nil, DDFLIP_WAIT );
            MouseCursor.PlotDirty := false;
          end;
          break;
        end;
      end;

      if ( Choice = 0 ) and ( Choice <> PrevChoice ) then
      begin
        pr := Rect( 0, 0, 800, 600 ); //NOHD
        lpDDSBack.BltFast( 0, 0, DXBack, @pr, DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
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
  width, height : Integer;
  DXBorders : IDirectDrawSurface;
  nRect : TRect;
  pr : TRect;
const
  FailName : string = 'TIntro.AreYouSure ';
begin

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    DXBorders := SoAOS_DX_LoadBMP( InterfacePath + 'ldChooseBox.bmp', cInvisColor, width, height );
    nRect := Captions[ 7 ].Rect; //Exit

    pr := Rect( 0, 0, 800, 600 ); //NOHD
    lpDDSBack.BltFast( 0, 0, DXBack, @pr, DDBLTFAST_WAIT );
    pr := Rect( 0, 0, width, height );
    lpDDSBack.BltFast( 400 - width div 2, nRect.top + 32, DXBorders, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

    DXBorders := nil;

    pText.PlotTextBlock( txtMessage[ 0 ], 400 - width div 2 + 23, 400 - width div 2 + 281, nRect.top + 52, 240 );

    AreYouSureBoxVisible := true;

    SoAOS_DX_BltFront;
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
