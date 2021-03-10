unit SoAOS.Intrface.Dialogs.MainMenu;
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

  Description: Main Menu Dialog - was Intro.pas - a lot more clean-up is coming

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
  Winapi.Windows,
  System.SysUtils,
  System.Types,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  SoAOS.Intrface.Dialogs,
  GameText,
  Engine,
  SoAOS.Animation,
  LogFile;

type
  TIntroRect = record
    Rect : TRect;
    Image : IDirectDrawSurface;
    Enabled : boolean;
  end;

  TIntro = class( TDialog )
  private
    AreYouSureBoxVisible : boolean;
    DXBack : IDirectDrawSurface;
    PrevChoice : integer;
    txtMessage : array[ 0..1 ] of string;
    procedure AreYouSure;
    function GetNoRect: TRect;
    function GetYesRect: TRect;
  protected
    procedure MouseDown( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer ); override;
    procedure MouseMove( Sender : TObject;
      Shift : TShiftState; X, Y, GridX, GridY : Integer ); override;
    procedure KeyDown( Sender : TObject; var key : Word; Shift : TShiftState ); override;
  public
    Captions : array[ 1..8 ] of TIntroRect;
    MenuChoice : integer;
    constructor Create;
    destructor Destroy; override;
    procedure Init; override;
    procedure Release; override;
    property YesRect: TRect read GetYesRect;
    property NoRect: TRect read GetNoRect;
  end;

implementation

uses
  SoAOS.Types,
  SoAOS.Intrface.Text,
  SoAOS.Graphics.Draw,
  AniDemo;

const
  XFrame = 106;

procedure MakeRect( var Caption : TIntroRect; X, Y, YOffset : integer; BM : TBitmap );
const
  FailName : string = 'Intro.MakeRect';
  W = 582;
  H = 52;
var
  DC : HDC;
begin
  Log.DebugLog( FailName );
  try
    Caption.Rect := Rect( X, Y, X + W, Y + H );
    Caption.Image := DDGetSurface( lpDD, W, H, BM.Canvas.Pixels[1,1], true ); // The asset released has a "color" issue - this hack solves it
    Caption.Image.GetDC( DC );
    try
      BitBlt( DC, 0, 0, W, H, BM.canvas.handle, 0, Y - YOffset, SRCCOPY );
    finally
      Caption.Image.ReleaseDC( DC );
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure MakeOldRect( var Caption : TIntroRect; X, Y : integer; BM : TBitmap );
const
  FailName : string = 'Intro.MakeOldRect';
var
  DC: HDC;
  W, H: Integer;
begin
  Log.DebugLog( FailName );
  W := BM.Width;
  H := BM.Height;
  try
    Caption.Rect := Rect( X, Y, X + W, Y + H );
    Caption.Image := DDGetSurface( lpDD, W, H, cTransparent, true );
    Caption.Image.GetDC( DC );
    try
      BitBlt( DC, 0, 0, W, H, BM.canvas.handle, 0, 0, SRCCOPY );
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
  Log.DebugLog( FailName );
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
  Log.DebugLog( FailName );
  try
    inherited;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TIntro.GetNoRect: TRect;
var
  y: Integer;
begin
  // Dynamic due to Russian menu variation.
  y := Captions[ 7 ].Rect.Top + 110;
  Result := ApplyOffset( Rect( 437, y, 489, y + 31 ) );
end;

function TIntro.GetYesRect: TRect;
var
  y: Integer;
begin
  // Dynamic due to Russian menu variation.
  y := Captions[ 7 ].Rect.Top + 110;
  Result := ApplyOffset( Rect( 303, y, 355, y + 31 ) );
end;

procedure TIntro.Init;
var
  BM : TBitmap;
  DC : HDC;
  Y1, YWidth, YOffset : integer;
  pr : TRect;
const
  FailName : string = 'TIntro.Init';
begin
  // Does calculate an offset based on screen vs menu bitmap - but might move to screenMetrics
  // For now are the caption rects in SD coords
  Log.DebugLog( FailName );
  try
    if Loaded then
      Exit;
    inherited;
    ExText.Open( 'Intro' );
    txtMessage[ 0 ] := ExText.GetText( 'Message' + inttostr( 0 ) );

    PrevChoice := 0;
    AreYouSureBoxVisible := false;
    pText.LoadFontGraphic( 'createchar' );

    { TODO -cUI : Currently old, new multilanguage, and russian menu implementations - pick one }
    BM := TBitmap.Create;
    try
      if FileExists( InterfaceLanguagePath + 'gMainMenuBlank.bmp' ) then   // multi language setup
      begin
        DXBack := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'gMainMenuBlank.bmp', cInvisColor, DlgWidth, DlgHeight );
        BM.LoadFromFile( InterfaceLanguagePath + 'gMainMenuText.bmp' );
        DXBack.GetDC( DC );
        try
          if Language = 'russian' then
            BitBlt( DC, 106, 31, 582, 440, BM.canvas.handle, 0, 0, SRCCOPY )
          else
            BitBlt( DC, 106, 41, 582, 416, BM.canvas.handle, 0, 0, SRCCOPY );
        finally
          DXBack.ReleaseDC( DC );
        end;
      end
      else
      begin // original 2001 setup
        DXBack := SoAOS_DX_LoadBMP( InterfacePath + 'gMainMenu.bmp', cInvisColor, DlgWidth, DlgHeight );
      end;

      pr := Rect( 0, 0, DlgWidth, DlgHeight );
      lpDDSBack.BltFast( Offset.X, Offset.Y, DXBack, @pr, DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );

      if FileExists( InterfaceLanguagePath + 'gMainMenuTextBttns.bmp' ) then
      begin
        BM.LoadFromFile( InterfaceLanguagePath + 'gMainMenuTextBttns.bmp' );
        if Language = 'russian' then
        begin
          YWidth := 55;
          YOffset := 30;
        end
        else
        begin
          YWidth := 52;
          YOffset := 41;
        end;
        Y1 := YOffset;
        MakeRect( Captions[ 1 ], XFrame, Y1, YOffset, BM );  // New game
        inc( Y1, YWidth );
        MakeRect( Captions[ 2 ], XFrame, Y1, YOffset, BM );  // Load
        inc( Y1, YWidth );
        MakeRect( Captions[ 3 ], XFrame, Y1, YOffset, BM );  // Save
        inc( Y1, YWidth );
        MakeRect( Captions[ 4 ], XFrame, Y1, YOffset, BM );  // Options
        inc( Y1, YWidth );
        MakeRect( Captions[ 5 ], XFrame, Y1, YOffset, BM );  // History
        inc( Y1, YWidth );
        MakeRect( Captions[ 6 ], XFrame, Y1, YOffset, BM );  // Credits
        inc( Y1, YWidth );
        MakeRect( Captions[ 7 ], XFrame, Y1, YOffset, BM );  // Exit
        inc( Y1, YWidth );
        MakeRect( Captions[ 8 ], XFrame, Y1, YOffset, BM );  // Resume
      end
      else
      begin
        BM.LoadFromFile( InterfacePath + 'gNEW.bmp' );
        MakeOldRect( Captions[ 1 ], 255, 49, BM );  // New game
        BM.LoadFromFile( InterfacePath + 'gLOAD.bmp' );
        MakeOldRect( Captions[ 2 ], 323, 94, BM );  // Load
        BM.LoadFromFile( InterfacePath + 'gSAVE.bmp' );
        MakeOldRect( Captions[ 3 ], 294, 148, BM );  // Save
        BM.LoadFromFile( InterfacePath + 'gOPTIONS.bmp' );
        MakeOldRect( Captions[ 4 ], 284, 191, BM );  // Options
        BM.LoadFromFile( InterfacePath + 'gUPDATE.bmp' );
        MakeOldRect( Captions[ 5 ], 260, 254, BM );  // History
        BM.LoadFromFile( InterfacePath + 'gCREDITS.bmp' );
        MakeOldRect( Captions[ 6 ], 294, 297, BM );  // Credits
        BM.LoadFromFile( InterfacePath + 'gEXIT.bmp' );
        MakeOldRect( Captions[ 7 ], 300, 353, BM );  // Exit
        BM.LoadFromFile( InterfacePath + 'gRESUME.bmp' );
        MakeOldRect( Captions[ 8 ], 197, 400, BM );  // Resume
      end;

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

procedure TIntro.MouseDown( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer );
var
  i : integer;
  pr : TRect;
const
  FailName : string = 'TIntro.MouseDown';
begin
  Log.DebugLog( FailName );
  try
    MenuChoice := 0;

    if AreYouSureBoxVisible = false then
    begin
      // Compensate for higher resolution against caption rects
      Dec(X, Offset.X);
      Dec(Y, Offset.Y);
      for i := 1 to 8 do
      begin
        if Captions[ i ].Enabled and Captions[ i ].Rect.Contains( Point( X, Y ) ) then
        begin
          MenuChoice := i;
          Break;
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
      if YesRect.Contains( Point( x, y ) ) then
      begin //Yes pressed- quit game
        MenuChoice := 7;
        Close;
      end
      else if NoRect.Contains( Point( x, y ) ) then
      begin //No pressed- just show screen
        AreYouSureBoxVisible := false;
        pr := Rect( 0, 0, DlgWidth, DlgHeight );
        lpDDSBack.BltFast( Offset.X, Offset.Y, DXBack, @pr, DDBLTFAST_WAIT ); //clear screen
        SoAOS_DX_BltFront;
      end; //endif PtInRect
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TIntro.MouseMove( Sender : TObject;
      Shift : TShiftState; X, Y, GridX, GridY : Integer );
var
  Choice : integer;
  i : integer;
  pr : TRect;
const
  FailName : string = 'TIntro.MouseMove';
begin
  Log.DebugLog( FailName );
  try
    Choice := 0;
    if AreYouSureBoxVisible = false then
    begin
      // Compensate for higher resolution against caption rects
      Dec(X, Offset.X);
      Dec(Y, Offset.Y);
      for i := 1 to 8 do
      begin
        if Captions[ i ].Enabled and Captions[ i ].Rect.Contains( Point( X, Y ) ) then
        begin
          Choice := i;
          if Choice <> PrevChoice then
          begin
            pr := Rect( 0, 0, DlgWidth, DlgHeight );
            lpDDSBack.BltFast( Offset.X, Offset.Y, DXBack, @pr, DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
            pr := Rect( 0, 0, Captions[ i ].Rect.Width, Captions[ i ].Rect.Height );
            lpDDSBack.BltFast( Offset.X + Captions[ i ].Rect.Left, Offset.Y + Captions[ i ].Rect.Top, Captions[ i ].Image, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
            lpDDSFront.Flip( nil, DDFLIP_WAIT );
            MouseCursor.PlotDirty := false;
          end;
          break;
        end;
      end;

      if ( Choice = 0 ) and ( Choice <> PrevChoice ) then
      begin
        pr := Rect( 0, 0, DlgWidth, DlgHeight );
        lpDDSBack.BltFast( Offset.X, Offset.Y, DXBack, @pr, DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
        lpDDSFront.Flip( nil, DDFLIP_WAIT );
        MouseCursor.PlotDirty := false;
      end;
      PrevChoice := Choice;
    end;

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

  Log.DebugLog( FailName );
  try
    DXBorders := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'ldChooseBox.bmp', cInvisColor, width, height );
    nRect := Captions[ 7 ].Rect; //Exit

    pr := Rect( 0, 0, DlgWidth, DlgHeight );
    lpDDSBack.BltFast( Offset.X, Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
    pr := Rect( 0, 0, width, height );
    lpDDSBack.BltFast( (ScreenMetrics.ScreenWidth - width) div 2, nRect.top + 32 + Offset.Y, DXBorders, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

    DXBorders := nil;

    pText.PlotTextBlock( txtMessage[ 0 ], (ScreenMetrics.ScreenWidth - width) div 2 + 23, (ScreenMetrics.ScreenWidth - width ) div 2 + 281, nRect.top + 52 + Offset.Y, 240 );

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
  Log.DebugLog( FailName );
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
