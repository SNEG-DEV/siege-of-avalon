unit GameText;
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
  DXEffects,
  System.Types,
  System.Classes,
  System.SysUtils,
  System.IOUtils,
  Logfile;

type
  Alphabet = record
    sx : integer;
    sy : integer;
    sw : integer;
    sh : integer;
    AdjPrev : integer; //How much to adjust for previous char
    AdjNext : integer; //How much to adjust for next char
    AdjTop : integer;
  end;

  TGameText = class( TObject )
  private
    DXSurface : IDirectDrawSurface;
    DXDarkSurface : IDirectDrawSurface;
    DXTinySurface : IDirectDrawSurface;
    DX13Surface : IDirectDrawSurface;
    DXGoldSurface : IDirectDrawSurface;
    DXMegaTinySurface : IDirectDrawSurface;
    Letter : array[ 32..255 ] of Alphabet;
    DarkLetter : array[ 32..255 ] of Alphabet;
    TinyLetter : array[ 32..255 ] of Alphabet;
    F13Letter : array[ 32..255 ] of Alphabet;
    MegaTinyLetter : array[ 32..255 ] of Alphabet;
    procedure LoadText;
  public
    constructor Create;
    destructor Destroy; override;
    procedure PlotText( Sentence : string; X, Y, Alpha : integer );
    procedure PlotText2( DX : IDirectDrawSurface; Sentence : string; X, Y, Alpha : integer );
    procedure PlotDarkText( Sentence : string; X, Y, Alpha : integer );
    procedure PlotDarkText2( DX : IDirectDrawSurface; Sentence : string; X, Y, Alpha : integer );
    procedure PlotTinyText( Sentence : string; X, Y, Alpha : integer );
    procedure PlotTinyText2( DX : IDirectDrawSurface; Sentence : string; X, Y, Alpha : integer );
    function PlotTinyTextBlock( Sentence : string; X, X2, Y, Alpha : integer ) : integer;
    procedure PlotMegaTinyText( DX : IDirectDrawSurface; Sentence : string; X, Y, Alpha : integer );
    procedure PlotF13Text( DX : IDirectDrawSurface; Sentence : string; X, Y, Alpha : integer );
    procedure PlotF13TextCentered( DX : IDirectDrawSurface; Sentence : string; X, X2, Y, Alpha : integer );
    function PlotTextCentered( Sentence : string; X, X2, Y, Alpha : integer ) : boolean;
    function PlotTextCentered2( DX : IDirectDrawSurface; Sentence : string; X, X2, Y, Alpha : integer ) : boolean;
    function PlotDarkTextCentered( Sentence : string; X, X2, Y, Alpha : integer ) : boolean;
    function PlotDarkTextCentered2( DX : IDirectDrawSurface; Sentence : string; X, X2, Y, Alpha : integer ) : boolean;
    function PlotTextBlock( Sentence : string; X, X2, Y, Alpha : integer ) : integer;
    procedure PlotGoldText( DX : IDirectDrawSurface; Sentence : string; X, Y, Alpha : integer );
    function PlotGoldTextCentered( DX : IDirectDrawSurface; Sentence : string; X, X2, Y, Alpha : integer ) : boolean;
    function PlotGoldTextBlock( Sentence : string; X, X2, Y, Alpha : integer ) : integer;
    function TextBlockHeight( Sentence : string; X, X2, Y : integer ) : integer;
    procedure BreakTextIntoAStringList( const Sentence : string; const daList : TStringList; x1, x2 : integer );
    function PlotF13Block( DX : IDirectDrawSurface; Sentence : string; X, X2, Y, Alpha : integer ) : integer;
    procedure LoadFontGraphic( ScreenName : string );
    procedure LoadDarkFontGraphic( ScreenName : string );
    procedure LoadTinyFontGraphic;
    procedure UnLoadTinyFontGraphic;
    procedure Load13Graphic;
    procedure UnLoad13Graphic;
    procedure LoadMegaTinyFontGraphic;
    procedure UnLoadMegaTinyFontGraphic;
    procedure LoadGoldFontGraphic;
    procedure UnLoadGoldFontGraphic;
    function TextLength( Sentence : string ) : integer;
    function TinyTextLength( Sentence : string ) : integer;
    procedure PlotSquishedText( Sentence : string; X, Y, Alpha : integer );
    function PlotTextBlockAroundBox( Sentence : string; X, X2, Y, Alpha : integer; cRect : TRect ) : integer;

    procedure WriteText( Sentence : string; X, Y, FontSize : Integer );

  end;

implementation

uses
  Winapi.Windows,
  VCL.Graphics,
  SoAOS.Types,
  SoAOS.Graphics.Draw,
  SoAOS.Animation,
  AniDemo;

{ TGameText }

constructor TGameText.Create;
const
  FailName : string = 'TGameText.Create';
begin
  Log.DebugLog(FailName);
  try
    LoadText;
    LoadFontGraphic( 'Inventory' );
    LoadDarkFontGraphic( 'Inventory' );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

destructor TGameText.Destroy;
const
  FailName : string = 'TGameText.destroy';
begin
  Log.DebugLog(FailName);
  try
    if assigned( DXSurface ) then
      DXSurface := nil;
    if assigned( DXDarkSurface ) then
      DXDarkSurface := nil;
    if assigned( DXTinySurface ) then
      DXTinySurface := nil;
    if assigned( DXMegaTinySurface ) then
      DXMegaTinySurface := nil;
    if assigned( DX13Surface ) then
      DX13Surface := nil;
    if assigned( DXGoldSurface ) then
      DXGoldSurface := nil;
    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

procedure TGameText.WriteText(Sentence: string; X, Y, FontSize: Integer);
var
  BM: TBitmap;
  R: TRect;
  MsgImage: IDirectDrawSurface;
  MsgWidth: Integer;
  MsgHeight: Integer;
  dpiForm, dpiSystem: cardinal;
begin
  try
    if TOSVersion.Major=10 then
    begin
      dpiForm := GetDpiForWindow(frmMain.Handle);
      dpiSystem := GetDpiForSystem;
      FontSize := Trunc(FontSize * (dpiForm/dpiSystem));
    end
    else
      FontSize := Trunc(FontSize/frmMain.ScaleFactor);
  except
  end;
  BM := TBitmap.Create;
  try
    BM.Canvas.Font.Name:='BlackChancery';
    BM.Canvas.Font.Size := FontSize;
    R := Rect(0, 0, 300, 30);  // Width
    DrawText(BM.Canvas.Handle, PWideChar(Sentence), -1, R, DT_CALCRECT or DT_CENTER or
      DT_NOCLIP or DT_NOPREFIX or DT_WORDBREAK);
    MsgWidth := R.Right;
    MsgHeight := R.Bottom;
    BM.Width := MsgWidth;
    BM.Height := MsgHeight;
    SetTextColor(BM.Canvas.Handle, $000082BD); //  ColorToRGB(clWhite)
    SetBkMode(BM.Canvas.Handle, TRANSPARENT);
    PatBlt(BM.Canvas.Handle, 0, 0, MsgWidth, MsgHeight, BLACKNESS);
    DrawText(BM.Canvas.Handle, PWideChar(Sentence), -1, R, DT_CENTER or DT_NOCLIP or
      DT_NOPREFIX or DT_WORDBREAK);
    MsgImage := SoAOS_DX_SurfaceFromBMP(BM, clBlack);
    DrawAlpha(lpDDSBack, Rect(X, Y, X + MsgWidth, Y + 30), Rect(0, 0, MsgWidth, MsgHeight), MsgImage, true, 245);

//      pr := Rect( Letter[ j ].sx, Letter[ j ].sy, Letter[ j ].sx + Letter[ j ].sw, Letter[ j ].sy + Letter[ j ].sh );
//      lpDDSBack.BltFast( X + XStart + Letter[ j ].AdjPrev, Y + Letter[ j ].AdjTop, DXSurface, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

  finally
    BM.Free;
  end;
end;

procedure TGameText.LoadFontGraphic( ScreenName : string );
var
  fileName : String;
const
  FailName : string = 'TGameText.LoadOfntGraphic';
begin
  Log.DebugLog(FailName);
  try

    if Assigned( DXSurface ) then
      DXSurface := nil;
    if Lowercase( Screenname ) = 'inventory' then
      fileName := 'fntInvFont.bmp'
    else if Lowercase( Screenname ) = 'statistics' then
      fileName := 'fntStatFont.bmp'
    else if Lowercase( Screenname ) = 'createchar' then
      fileName := 'fntGoldFont.bmp';

    DXSurface := SoAOS_DX_LoadBMP( InterfaceLanguagePath + fileName, cInvisColor );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TGameText.LoadFontGraphic

procedure TGameText.LoadDarkFontGraphic( ScreenName : string );
const
  FailName : string = 'TGameText.LoadDarkFontGraphic';
begin
  Log.DebugLog(FailName);
  try

    if assigned( DXDarkSurface ) then
      exit;
    DXDarkSurface := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'fntBoldFont.bmp', cInvisColor );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TGameText.LoadDarkFontGraphic

procedure TGameText.LoadTinyFontGraphic( );
begin
  if assigned( DXTinySurface ) then
    exit;
  DXTinySurface := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'fntTinyFont.bmp', cInvisColor );
end; //TGameText.LoadTinyFontGraphic

procedure TGameText.UnloadTinyFontGraphic( );
begin;
  if assigned( DXTinySurface ) then
    DXTinySurface := nil;
end; //TGameText.UnloadTinyFontGraphic

procedure TGameText.Load13Graphic( );
begin
  if assigned( DX13Surface ) then
    exit;
  DX13Surface := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'fnt13.bmp', cInvisColor );
end; //TGameText.Load13Graphic

procedure TGameText.Unload13Graphic( );
begin;
  if assigned( DX13Surface ) then
    DX13Surface := nil;
end; //TGameText.Unload13Graphic

procedure TGameText.LoadMegaTinyFontGraphic( );
begin
  if assigned( DXMegaTinySurface ) then
    exit;
  DXMegaTinySurface := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'fntMegaTinyFont.bmp', cInvisColor );
end; //TGameText.LoadMegaTinyFontGraphic

procedure TGameText.UnloadMegaTinyFontGraphic( );
begin;
  if assigned( DXMegaTinySurface ) then
    DXMegaTinySurface := nil;
end; //TGameText.UnloadTinyFontGraphic

procedure TGameText.LoadText;
var
  F : TextFile;
  i : integer;
const
  FailName : string = 'TGameText.LoadText';
begin
  Log.DebugLog(FailName);
  try

    if TFile.Exists( InterfaceLanguagePath + 'fntAlphaCoords.dat' ) then
    begin
      AssignFile( F, InterfaceLanguagePath + 'fntAlphaCoords.dat' );
      Reset( F );
      i := 32;
      while not Eof( F ) do
      begin
        Read( F, Letter[ i ].sx, Letter[ i ].sy, Letter[ i ].sw, Letter[ i ].sh, Letter[ i ].AdjPrev, Letter[ i ].AdjNext, Letter[ i ].AdjTop );
        i := i + 1;
        if i > 255 then
          break;
      end;
      CloseFile( F );
    end
    else
    begin
      AssignFile( F, InterfacePath + 'fnterror.txt' );
      Rewrite( F );
      write( F, 'Didnt find file AlphaCoords.dat' );
      CloseFile( F );
    end;
  //And Now the Dark Font
    if TFile.Exists( InterfaceLanguagePath + 'fntDarkAlphaCoords.dat' ) then
    begin
      AssignFile( F, InterfaceLanguagePath + 'fntDarkAlphaCoords.dat' );
      Reset( F );
      i := 32;
      while not Eof( F ) do
      begin
        Read( F, DarkLetter[ i ].sx, DarkLetter[ i ].sy, DarkLetter[ i ].sw, DarkLetter[ i ].sh, DarkLetter[ i ].AdjPrev, DarkLetter[ i ].AdjNext, DarkLetter[ i ].AdjTop );
        i := i + 1;
        if i > 255 then
          break;
      end;
      CloseFile( F );
    end
    else
    begin
      AssignFile( F, InterfacePath + 'fnterror.txt' );
      Rewrite( F );
      write( F, 'Didnt find file DarkAlphaCoords' );
      CloseFile( F );
    end;
  //And Now the Tiny Font
    if TFile.Exists( InterfaceLanguagePath + 'fntTinyCoords.dat' ) then
    begin
      AssignFile( F, InterfaceLanguagePath + 'fntTinyCoords.dat' );
      Reset( F );
      i := 32;
      while not Eof( F ) do
      begin
        Read( F, TinyLetter[ i ].sx, TinyLetter[ i ].sy, TinyLetter[ i ].sw, TinyLetter[ i ].sh, TinyLetter[ i ].AdjPrev, TinyLetter[ i ].AdjNext, TinyLetter[ i ].AdjTop );
        i := i + 1;
        if i > 255 then
          break;
      end;
      CloseFile( F );
    end
    else
    begin
      AssignFile( F, InterfacePath + 'fnterror.txt' );
      Rewrite( F );
      write( F, 'Didnt find file TinyCoords' );
      CloseFile( F );
    end;
  //And Now the Mega Tiny Font
    if TFile.Exists( InterfaceLanguagePath + 'fntMegaTinyCoords.dat' ) then
    begin
      AssignFile( F, InterfaceLanguagePath + 'fntMegaTinyCoords.dat' );
      Reset( F );
      i := 32;
      while not Eof( F ) do
      begin
        Read( F, MegaTinyLetter[ i ].sx, MegaTinyLetter[ i ].sy, MegaTinyLetter[ i ].sw, MegaTinyLetter[ i ].sh, MegaTinyLetter[ i ].AdjPrev, MegaTinyLetter[ i ].AdjNext, MegaTinyLetter[ i ].AdjTop );
        i := i + 1;
        if i > 255 then
          break;
      end;
      CloseFile( F );
    end
    else
    begin
      AssignFile( F, InterfacePath + 'fnterror.txt' );
      Rewrite( F );
      write( F, 'Didnt find file MegaTinyCoords' );
      CloseFile( F );
    end;
  //And Now the 13 Tiny Font
    if TFile.Exists( InterfaceLanguagePath + 'fnt13Coords.dat' ) then
    begin
      AssignFile( F, InterfaceLanguagePath + 'fnt13Coords.dat' );
      Reset( F );
      i := 32;
      while not Eof( F ) do
      begin
        Read( F, F13Letter[ i ].sx, F13Letter[ i ].sy, F13Letter[ i ].sw, F13Letter[ i ].sh, F13Letter[ i ].AdjPrev, F13Letter[ i ].AdjNext, F13Letter[ i ].AdjTop );
        i := i + 1;
        if i > 255 then
          break;
      end;
      CloseFile( F );
    end
    else
    begin
      AssignFile( F, InterfacePath + 'fnterror.txt' );
      Rewrite( F );
      write( F, 'Didnt find file fnt13Coords' );
      CloseFile( F );
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TGameText.LoadText

procedure TGameText.PlotDarkText2( DX : IDirectDrawSurface; Sentence : string; X, Y, Alpha : integer );
var
  i : integer;
  j : integer;
  XStart : integer;
  AnsiCodePoints: AnsiString; // The bitmap font is depending on an Ansi position - not its representation
const
  FailName : string = 'TGameText.PLotDarkText';
begin
  Log.DebugLog(FailName);
  try
    AnsiCodePoints := AnsiString(Sentence);
    XStart := 0;
    for i := 1 to Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      DrawAlpha( DX, Rect( X + XStart + DarkLetter[ j ].AdjPrev, Y + DarkLetter[ j ].AdjTop, X + XStart + DarkLetter[ j ].sw + DarkLetter[ j ].AdjPrev, Y + DarkLetter[ j ].AdjTop + DarkLetter[ j ].sh ), Rect( DarkLetter[ j ].sx, DarkLetter[ j ].sy, DarkLetter[ j ].sx + DarkLetter[ j ].sw, DarkLetter[ j ].sy + DarkLetter[ j ].sh ), DXDarkSurface, true, Alpha );
      XStart := XStart + DarkLetter[ j ].sw + DarkLetter[ j ].AdjPrev + DarkLetter[ j ].AdjNext;
      XStart := XStart - 1;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TGameText.PlotDarkText

procedure TGameText.PlotTinyText2( DX : IDirectDrawSurface; Sentence : string; X, Y, Alpha : integer );
var
  i : integer;
  j : integer;
  XStart : integer;
  AnsiCodePoints: AnsiString; // The bitmap font is depending on an Ansi position - not its representation
const
  FailName : string = 'TGameText.PlotTinyText';
begin
  Log.DebugLog(FailName);
  try
    AnsiCodePoints := AnsiString(Sentence);
    XStart := 0;
    for i := 1 to Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      DrawAlpha( DX, Rect( X + XStart + TinyLetter[ j ].AdjPrev, Y + TinyLetter[ j ].AdjTop, X + XStart + TinyLetter[ j ].sw + TinyLetter[ j ].AdjPrev, Y + TinyLetter[ j ].AdjTop + TinyLetter[ j ].sh ), Rect( TinyLetter[ j ].sx, TinyLetter[ j ].sy, TinyLetter[ j ].sx + TinyLetter[ j ].sw, TinyLetter[ j ].sy + TinyLetter[ j ].sh ), DXTinySurface, true, Alpha );
      XStart := XStart + TinyLetter[ j ].sw + TinyLetter[ j ].AdjPrev + TinyLetter[ j ].AdjNext;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TGameText.PlotTinyText

procedure TGameText.PlotTinyText( Sentence : string; X, Y, Alpha : integer );
begin
  PlotTinyText2( lpDDSBack, Sentence, X, Y, Alpha );
end;

procedure TGameText.PlotMegaTinyText( DX : IDirectDrawSurface; Sentence : string; X, Y, Alpha : integer );
var
  i : integer;
  j : integer;
  XStart : integer;
  AnsiCodePoints: AnsiString; // The bitmap font is depending on an Ansi position - not its representation
const
  FailName : string = 'TGameText.PlotMegaTinyText';
begin
  Log.DebugLog(FailName);
  try
    AnsiCodePoints := AnsiString(Sentence);
    XStart := 0;
    for i := 1 to Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      DrawAlpha( DX, Rect( X + XStart + MegaTinyLetter[ j ].AdjPrev, Y + MegaTinyLetter[ j ].AdjTop, X + XStart + MegaTinyLetter[ j ].sw + MegaTinyLetter[ j ].AdjPrev, Y + MegaTinyLetter[ j ].AdjTop + MegaTinyLetter[ j ].sh ), Rect( MegaTinyLetter[ j ].sx, MegaTinyLetter[ j ].sy, MegaTinyLetter[ j ].sx + MegaTinyLetter[ j ].sw, MegaTinyLetter[ j ].sy + MegaTinyLetter[ j ].sh ), DXMegaTinySurface, true, Alpha );
      XStart := XStart + MegaTinyLetter[ j ].sw + MegaTinyLetter[ j ].AdjPrev + MegaTinyLetter[ j ].AdjNext;
    end; //end for
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TGameText.PlotMegaTinyText

procedure TGameText.PlotF13Text( DX : IDirectDrawSurface; Sentence : string; X, Y, Alpha : integer );
var
  i : integer;
  j : integer;
  XStart : integer;
  AnsiCodePoints: AnsiString; // The bitmap font is depending on an Ansi position - not its representation
const
  FailName : string = 'TGameText.PlotF13Text';
begin
  Log.DebugLog(FailName);
  try
    AnsiCodePoints := AnsiString(Sentence);
    XStart := 0;
    for i := 1 to Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      DrawAlpha( DX, Rect( X + XStart + F13Letter[ j ].AdjPrev, Y + F13Letter[ j ].AdjTop, X + XStart + F13Letter[ j ].sw + F13Letter[ j ].AdjPrev, Y + F13Letter[ j ].AdjTop + F13Letter[ j ].sh ), Rect( F13Letter[ j ].sx, F13Letter[ j ].sy, F13Letter[ j ].sx + F13Letter[ j ].sw, F13Letter[ j ].sy + F13Letter[ j ].sh ), DX13Surface, true, Alpha );
      XStart := XStart + F13Letter[ j ].sw + F13Letter[ j ].AdjPrev + F13Letter[ j ].AdjNext;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TGameText.PlotF13Text

procedure TGameText.PlotText( Sentence : string; X, Y, Alpha : integer );
var
  i : integer;
  j : integer;
  XStart : integer;
  pr : TRect;
  AnsiCodePoints: AnsiString; // The bitmap font is depending on an Ansi position - not its representation
const
  FailName : string = 'TGameText.Plottext';
begin
  Log.DebugLog(FailName);
  try
    AnsiCodePoints := AnsiString(Sentence);
    XStart := 0;
    for i := 1 to Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      if Alpha > 0 then
        DrawAlpha( lpDDSBack, Rect( X + XStart + Letter[ j ].AdjPrev, Y + Letter[ j ].AdjTop, X + XStart + Letter[ j ].sw + Letter[ j ].AdjPrev, Y + Letter[ j ].AdjTop + Letter[ j ].sh ), Rect( Letter[ j ].sx, Letter[ j ].sy, Letter[ j ].sx + Letter[ j ].sw, Letter[ j ].sy + Letter[ j ].sh ), DXSurface, true, Alpha )
      else
      begin
        pr := Rect( Letter[ j ].sx, Letter[ j ].sy, Letter[ j ].sx + Letter[ j ].sw, Letter[ j ].sy + Letter[ j ].sh );
        lpDDSBack.BltFast( X + XStart + Letter[ j ].AdjPrev, Y + Letter[ j ].AdjTop, DXSurface, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end;
      XStart := XStart + Letter[ j ].sw + Letter[ j ].AdjPrev + Letter[ j ].AdjNext;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TGameText.PlotText

procedure TGameText.PlotText2( DX : IDirectDrawSurface; Sentence : string; X, Y, Alpha : integer );
var
  i : integer;
  j : integer;
  XStart : integer;
  pr : TRect;
  AnsiCodePoints: AnsiString; // The bitmap font is depending on an Ansi position - not its representation
const
  FailName : string = 'TGameText.PlotText2';
begin
  Log.DebugLog(FailName);
  try
    AnsiCodePoints := AnsiString(Sentence);
    XStart := 0;
    for i := 1 to Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      if Alpha > 0 then
        DrawAlpha( DX, Rect( X + XStart + Letter[ j ].AdjPrev, Y + Letter[ j ].AdjTop, X + XStart + Letter[ j ].sw + Letter[ j ].AdjPrev, Y + Letter[ j ].AdjTop + Letter[ j ].sh ), Rect( Letter[ j ].sx, Letter[ j ].sy, Letter[ j ].sx + Letter[ j ].sw, Letter[ j ].sy + Letter[ j ].sh ), DXSurface, true, Alpha )
      else
      begin
        pr := Rect( Letter[ j ].sx, Letter[ j ].sy, Letter[ j ].sx + Letter[ j ].sw, Letter[ j ].sy + Letter[ j ].sh );
        DX.BltFast( X + XStart + Letter[ j ].AdjPrev, Y + Letter[ j ].AdjTop, DXSurface, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end;
      XStart := XStart + Letter[ j ].sw + Letter[ j ].AdjPrev + Letter[ j ].AdjNext;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TGameText.PlotText2

procedure TGameText.PlotSquishedText( Sentence : string; X, Y, Alpha : integer );
var
  i : integer;
  j : integer;
  XStart : integer;
  pr : TRect;
  AnsiCodePoints: AnsiString; // The bitmap font is depending on an Ansi position - not its representation
const
  FailName : string = 'TGameText.PlotSquishedText';
begin
  Log.DebugLog(FailName);
  try
    AnsiCodePoints := AnsiString(Sentence);
    XStart := 0;
    for i := 1 to Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      if Alpha > 0 then
        DrawAlpha( lpDDSBack, Rect( X + XStart + Letter[ j ].AdjPrev, Y + Letter[ j ].AdjTop, X + XStart + Letter[ j ].sw + Letter[ j ].AdjPrev, Y + Letter[ j ].AdjTop + Letter[ j ].sh ), Rect( Letter[ j ].sx, Letter[ j ].sy, Letter[ j ].sx + Letter[ j ].sw, Letter[ j ].sy + Letter[ j ].sh ), DXSurface, true, Alpha )
      else
      begin
        pr := Rect( Letter[ j ].sx, Letter[ j ].sy, Letter[ j ].sx + Letter[ j ].sw, Letter[ j ].sy + Letter[ j ].sh );
        lpDDSBack.BltFast( X + XStart + Letter[ j ].AdjPrev, Y + Letter[ j ].AdjTop, DXSurface, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end;
      XStart := XStart + Letter[ j ].sw + Letter[ j ].AdjPrev + Letter[ j ].AdjNext - 1;
    end; //wend
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TGameText.PlotSquishedText


function TGameText.PlotTextCentered( Sentence : string; X, X2, Y, Alpha : integer ) : boolean;
//Plots a line of text centered between X and X2
var
  i : integer;
  j : integer;
  XStart : integer;
  TheLength : integer;
  ThereWasRoom : boolean;
  pr : TRect;
  AnsiCodePoints: AnsiString; // The bitmap font is depending on an Ansi position - not its representation
const
  FailName : string = 'TGameText.PlotTextCentered';
begin
  Log.DebugLog(FailName);
  Result := false;
  try
    AnsiCodePoints := AnsiString(Sentence);
    TheLength := 0;
    for i := 1 to length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      TheLength := TheLength + Letter[ j ].sw + Letter[ j ].AdjPrev + Letter[ j ].AdjNext;
    end; //wend
    ThereWasRoom := True;
    XStart := ( ( X2 - X ) - TheLength ) div 2; //center the line of text
    if XStart < X then
    begin //there wasn't enough space for the entire line
      ThereWasRoom := false;
    end;

    for i := 1 to Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      if Alpha > 0 then
        DrawAlpha( lpDDSBack, Rect( X + XStart + Letter[ j ].AdjPrev, Y + Letter[ j ].AdjTop, X + XStart + Letter[ j ].sw + Letter[ j ].AdjPrev, Y + Letter[ j ].AdjTop + Letter[ j ].sh ), Rect( Letter[ j ].sx, Letter[ j ].sy, Letter[ j ].sx + Letter[ j ].sw, Letter[ j ].sy + Letter[ j ].sh ), DXSurface, true, Alpha )
      else
      begin
        pr := Rect( Letter[ j ].sx, Letter[ j ].sy, Letter[ j ].sx + Letter[ j ].sw, Letter[ j ].sy + Letter[ j ].sh );
        lpDDSBack.BltFast( X + XStart + Letter[ j ].AdjPrev, Y + Letter[ j ].AdjTop, DXSurface, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end;
      XStart := XStart + Letter[ j ].sw + Letter[ j ].AdjPrev + Letter[ j ].AdjNext;
    end; //wend

    Result := ThereWasRoom;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TGameText.PlotTextCentered


function TGameText.PlotTextCentered2( DX : IDirectDrawSurface; Sentence : string; X, X2, Y, Alpha : integer ) : boolean;
//Plots a line of text centered between X and X2
var
  i : integer;
  j : integer;
  XStart : integer;
  TheLength : integer;
  ThereWasRoom : boolean;
  pr : TRect;
  AnsiCodePoints: AnsiString; // The bitmap font is depending on an Ansi position - not its representation
const
  FailName : string = 'TGameText.PlotTextCentered2';
begin
  Log.DebugLog(FailName);
  Result := false;
  try
    AnsiCodePoints := AnsiString(Sentence);
    TheLength := 0;
    for i := 1 to length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      TheLength := TheLength + Letter[ j ].sw + Letter[ j ].AdjPrev + Letter[ j ].AdjNext;
    end; //wend

    ThereWasRoom := True;
    XStart := ( ( X2 - X ) - TheLength ) div 2; //center the line of text
    if XStart < X then
    begin //there wasn't enough space for the entire line
      ThereWasRoom := false;
    end;

    for i := 1 to Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      if Alpha > 0 then
        DrawAlpha( DX, Rect( X + XStart + Letter[ j ].AdjPrev, Y + Letter[ j ].AdjTop, X + XStart + Letter[ j ].sw + Letter[ j ].AdjPrev, Y + Letter[ j ].AdjTop + Letter[ j ].sh ), Rect( Letter[ j ].sx, Letter[ j ].sy, Letter[ j ].sx + Letter[ j ].sw, Letter[ j ].sy + Letter[ j ].sh ), DXSurface, true, Alpha )
      else
      begin
        pr := Rect( Letter[ j ].sx, Letter[ j ].sy, Letter[ j ].sx + Letter[ j ].sw, Letter[ j ].sy + Letter[ j ].sh );
        DX.BltFast( X + XStart + Letter[ j ].AdjPrev, Y + Letter[ j ].AdjTop, DXSurface, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end;
      XStart := XStart + Letter[ j ].sw + Letter[ j ].AdjPrev + Letter[ j ].AdjNext;
    end; //wend

    Result := ThereWasRoom;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TGameText.PlotTextCentered2

function TGameText.PlotDarkTextCentered2( DX : IDirectDrawSurface; Sentence : string; X, X2, Y, Alpha : integer ) : boolean;
//Plots a line of text centered between X and X2
var
  i : integer;
  j : integer;
  XStart : integer;
  TheLength : integer;
  ThereWasRoom : boolean;
  pr : TRect;
  AnsiCodePoints: AnsiString; // The bitmap font is depending on an Ansi position - not its representation
const
  FailName : string = 'TGameText.PlotDarktextCentered2';
begin
  Log.DebugLog(FailName);
  Result := false;
  try
    AnsiCodePoints := AnsiString(Sentence);
    TheLength := 0;

    for i := 1 to length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      TheLength := TheLength + DarkLetter[ j ].sw + DarkLetter[ j ].AdjPrev + DarkLetter[ j ].AdjNext - 1;
    end; //wend

    ThereWasRoom := True;
    XStart := ( ( X2 - X ) - TheLength ) div 2; //center the line of text
    if XStart < X then
    begin //there wasn't enough space for the entire line
      ThereWasRoom := false;
    end;

    for i := 1 to Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      if Alpha > 0 then
        DrawAlpha( DX, Rect( X + XStart + DarkLetter[ j ].AdjPrev, Y + DarkLetter[ j ].AdjTop, X + XStart + DarkLetter[ j ].sw + DarkLetter[ j ].AdjPrev, Y + DarkLetter[ j ].AdjTop + DarkLetter[ j ].sh ), Rect( DarkLetter[ j ].sx, DarkLetter[ j ].sy, DarkLetter[ j ].sx + DarkLetter[ j ].sw, DarkLetter[ j ].sy + DarkLetter[ j ].sh ), DXDarkSurface, true, Alpha )
      else
      begin
        pr := Rect( DarkLetter[ j ].sx, DarkLetter[ j ].sy, DarkLetter[ j ].sx + DarkLetter[ j ].sw, DarkLetter[ j ].sy + DarkLetter[ j ].sh );
        DX.BltFast( X + XStart + DarkLetter[ j ].AdjPrev, Y + DarkLetter[ j ].AdjTop, DXDarkSurface, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end;
      XStart := XStart + DarkLetter[ j ].sw + DarkLetter[ j ].AdjPrev + DarkLetter[ j ].AdjNext - 1;
    end; //wend

    Result := ThereWasRoom;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TGameText.PlotDarkTextCentered

function TGameText.PlotTextBlock( Sentence : string; X, X2, Y, Alpha : integer ) : integer;
//Plots a line of text centered between X and X2
var
  i : integer;
  j : integer;
  k : integer;
  NL : integer;
  XStart : integer;
  TheLength : integer;
  LastSpace, PrevLastSpace : integer;
  LineBreak : array[ 0..50 ] of integer;
  pr : TRect;
  AnsiCodePoints: AnsiString; // The bitmap font is depending on an Ansi position - not its representation
const
  FailName : string = 'TGameText.PlotTextBlock';
begin
  Log.DebugLog(FailName);
  Result := 0;
  try
    AnsiCodePoints := AnsiString(Sentence);
    i := 1;
    k := 0;
    TheLength := 0;
    LastSpace := 0;
    PrevLastSpace := 0;
    LineBreak[ 0 ] := 9999; //in case there are no line breaks, we initalize to an absurdly high number
    while i <= Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );

      if j = 13 then
      begin
        //debugPlot(j);
        i := i + 1;
        TheLength := 0;
      end
      else if ( j < 32 ) or ( j > 255 ) then
      begin
        i := i + 1;
      end
      else
      begin
        if ( j = 32 ) then
        begin //if its a space or linefeed
          LastSpace := i;
        end;
        TheLength := TheLength + Letter[ j ].sw + Letter[ j ].AdjPrev + Letter[ j ].AdjNext;
        if ( TheLength > ( X2 - X ) ) and ( LastSpace > PrevLastSpace ) then
        begin //time to break the line - if we havent had a space yet, we jsut keep going to avoid a lockup
          LineBreak[ k ] := LastSpace;
          k := k + 1;
          TheLength := 0;
          i := LastSpace + 1;
          LastSpace := PrevLastSpace;
        end
        else
          i := i + 1;
      end;

    end; //wend
    LineBreak[ k ] := 9999; //set the last linebreak absurdly high - we have to initialize this last break

    XStart := 0;
    k := 0;
    NL := 0;
    for i := 1 to Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      if j = 13 then
      begin
        XStart := 0;
        NL := NL + 1;
      end
      else if ( j < 32 ) or ( j > 255 ) then
      begin
      end
      else if i = LineBreak[ k ] then
      begin //we've hit a space at a line break
        k := k + 1; //so inc the index to the next break, and skip this space plot
        XStart := 0;
      end
      else
      begin
        if Alpha > 0 then
          DrawAlpha( lpDDSBack, Rect( X + XStart + Letter[ j ].AdjPrev, Y + k * 22 + NL * 22 + Letter[ j ].AdjTop, X + XStart + Letter[ j ].sw + Letter[ j ].AdjPrev, Y + Letter[ j ].AdjTop + Letter[ j ].sh + k * 22 + NL * 22 ), Rect( Letter[ j ].sx, Letter[ j ].sy, Letter[ j ].sx + Letter[ j ].sw, Letter[ j ].sy + Letter[ j ].sh ), DXSurface, true, Alpha )
        else
        begin
          pr := Rect( Letter[ j ].sx, Letter[ j ].sy, Letter[ j ].sx + Letter[ j ].sw, Letter[ j ].sy + Letter[ j ].sh );
          lpDDSBack.BltFast( X + XStart + Letter[ j ].AdjPrev, Y + k * 22 + Letter[ j ].AdjTop, DXSurface, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        end;
        XStart := XStart + Letter[ j ].sw + Letter[ j ].AdjPrev + Letter[ j ].AdjNext;
      end;
    end; //endfor

    Result := k + 1; //return the number of lines we run through
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TGameText.PlotTextBlock

function TGameText.TextBlockHeight( Sentence : string; X, X2, Y : integer ) : integer;
//Plots a line of text centered between X and X2
var
  i : integer;
  j : integer;
  k : integer;
  XStart : integer;
  TheLength : integer;
  LastSpace, PrevLastSpace : integer;
  LineBreak : array[ 0..50 ] of integer;
  AnsiCodePoints: AnsiString; // The bitmap font is depending on an Ansi position - not its representation
const
  FailName : string = 'TGameText.PlotTextBlock';
begin
  Log.DebugLog(FailName);
  Result := 0;
  try
    AnsiCodePoints := AnsiString(Sentence);
    i := 1;
    k := 0;
    TheLength := 0;
    LastSpace := 0;
    PrevLastSpace := 0;
    LineBreak[ 0 ] := 9999; //in case there are no line breaks, we initalize to an absurdly high number
    while i <= Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );

      if j = 13 then
      begin
        //debugPlot(j);
        i := i + 1;
        TheLength := 0;
      end
      else if ( j < 32 ) or ( j > 255 ) then
      begin
        i := i + 1;
      end
      else
      begin
        if ( j = 32 ) then
        begin //if its a space or linefeed
          LastSpace := i;
        end;
        TheLength := TheLength + Letter[ j ].sw + Letter[ j ].AdjPrev + Letter[ j ].AdjNext;
        if ( TheLength > ( X2 - X ) ) and ( LastSpace > PrevLastSpace ) then
        begin //time to break the line - if we havent had a space yet, we jsut keep going to avoid a lockup
          LineBreak[ k ] := LastSpace;
          k := k + 1;
          TheLength := 0;
          i := LastSpace + 1;
          LastSpace := PrevLastSpace;
        end
        else
          i := i + 1;
      end;

    end; //wend
    LineBreak[ k ] := 9999; //set the last linebreak absurdly high - we have to initialize this last break

    XStart := 0;
    k := 0;
    for i := 1 to Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      if j = 13 then
      begin
        XStart := 0;
      end
      else if ( j < 32 ) or ( j > 255 ) then
      begin
      end
      else if i = LineBreak[ k ] then
      begin //we've hit a space at a line break
        k := k + 1; //so inc the index to the next break, and skip this space plot
        XStart := 0;
      end
      else
      begin
        XStart := XStart + Letter[ j ].sw + Letter[ j ].AdjPrev + Letter[ j ].AdjNext;
      end;
    end; //endfor

    Result := k + 1; //return the number of lines we run through
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TGameText.TextBlockHeight


function TGameText.PlotF13Block( DX : IDirectDrawSurface; Sentence : string; X, X2, Y, Alpha : integer ) : integer;
//Plots a line of text centered between X and X2
var
  i : integer;
  j : integer;
  k : integer;
  NL : integer;
  XStart : integer;
  TheLength : integer;
  LastSpace, PrevLastSpace : integer;
  LineBreak : array[ 0..50 ] of integer;
  pr : TRect;
  AnsiCodePoints: AnsiString; // The bitmap font is depending on an Ansi position - not its representation
const
  FailName : string = 'TGameText.Plotf13block';
begin
  Log.DebugLog(FailName);
  Result := 0;
  try
    AnsiCodePoints := AnsiString(Sentence);
    i := 1;
    k := 0;
    TheLength := 0;
    LastSpace := 0;
    PrevLastSpace := 0;
    LineBreak[ 0 ] := 9999; //in case there are no line breaks, we initalize to an absurdly high number
    while i <= Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );

      if j = 13 then
      begin
        //debugPlot(j);
        i := i + 1;
        TheLength := 0;
      end
      else if ( j < 32 ) or ( j > 255 ) then
      begin
        i := i + 1;
      end
      else
      begin
        if ( j = 32 ) then
        begin //if its a space or linefeed
          LastSpace := i;
        end;
        TheLength := TheLength + F13Letter[ j ].sw + F13Letter[ j ].AdjPrev + F13Letter[ j ].AdjNext;
        if ( TheLength > ( X2 - X ) ) and ( LastSpace > PrevLastSpace ) then
        begin //time to break the line - if we havent had a space yet, we jsut keep going to avoid a lockup
          LineBreak[ k ] := LastSpace;
          k := k + 1;
          TheLength := 0;
          i := LastSpace + 1;
          LastSpace := PrevLastSpace;
        end
        else
          i := i + 1;
      end;

    end; //wend
    LineBreak[ k ] := 9999; //set the last linebreak absurdly high - we have to initialize this last break

    XStart := 0;
    k := 0;
    NL := 0;
    for i := 1 to Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      if j = 13 then
      begin
        XStart := 0;
        NL := NL + 1;
      end
      else if ( j < 32 ) or ( j > 255 ) then
      begin
      end
      else if i = LineBreak[ k ] then
      begin //we've hit a space at a line break
        k := k + 1; //so inc the index to the next break, and skip this space plot
        XStart := 0;
      end
      else
      begin
        if Alpha > 0 then
          DrawAlpha( DX, Rect( X + XStart + F13Letter[ j ].AdjPrev, Y + k * 13 + NL * 13 + F13Letter[ j ].AdjTop, X + XStart + F13Letter[ j ].sw + F13Letter[ j ].AdjPrev, Y + F13Letter[ j ].AdjTop + F13Letter[ j ].sh + k * 13 + NL * 13 ), Rect( F13Letter[ j ].sx, F13Letter[ j ].sy, F13Letter[ j ].sx + F13Letter[ j ].sw, F13Letter[ j ].sy + F13Letter[ j ].sh ), DX13Surface, true, Alpha )
        else
        begin
          pr := Rect( F13Letter[ j ].sx, F13Letter[ j ].sy, F13Letter[ j ].sx + F13Letter[ j ].sw, F13Letter[ j ].sy + F13Letter[ j ].sh );
          DX.BltFast( X + XStart + F13Letter[ j ].AdjPrev, Y + k * 13 + F13Letter[ j ].AdjTop, DX13Surface, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        end;
        XStart := XStart + F13Letter[ j ].sw + F13Letter[ j ].AdjPrev + F13Letter[ j ].AdjNext;
      end;
    end; //endfor

    Result := k + 1; //return the number of lines we run through
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TGameText.PlotF13Block

function TGameText.PlotTextBlockAroundBox( Sentence : string; X, X2, Y, Alpha : integer; cRect : TRect ) : integer;
//Plots a line of text centered between X and X2
var
  i : integer;
  j : integer;
  k : integer;
  NL : integer;
  XStart : integer;
  TheLength : integer;
  LastSpace : integer;
  LineBreak : array[ 0..500 ] of integer;
  rRect : TRect;
  pr : TRect;
  AnsiCodePoints: AnsiString; // The bitmap font is depending on an Ansi position - not its representation
const
  FailName : string = 'TGameText.PlotTextAroundBlock';
begin
  Log.DebugLog(FailName);
  Result := 0;
  try
    AnsiCodePoints := AnsiString(Sentence);
    i := 1;
    k := 0;
    TheLength := 0;
    LastSpace := 0;
    LineBreak[ 0 ] := 9999; //in case there are no line breaks, we initalize to an absurdly high number

    NL := 0;
    while i <= Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );

      if ( j = 13 ) then
      begin
        i := i + 1;
        TheLength := 0;
        NL := NL + 1;
      end
      else if ( j < 32 ) or ( j > 255 ) then
      begin
        i := i + 1;
      end
      else if ( TheLength = 0 ) and IntersectRect( rRect, cRect, rect( X + TheLength, K * 22 + NL * 22 + Y, X + TheLength + Letter[ j ].sw + Letter[ j ].AdjPrev + Letter[ j ].AdjNext, K * 22 + NL * 22 + Y + 22 ) ) then
      begin
        while IntersectRect( rRect, cRect, rect( X + TheLength, K * 22 + NL * 22 + Y, X + TheLength + Letter[ j ].sw + Letter[ j ].AdjPrev + Letter[ j ].AdjNext, K * 22 + NL * 22 + Y + 22 ) ) and ( TheLength < ( X2 - X ) ) do
        begin
          TheLength := TheLength + 1;
        end;
        i := i + 1;
      end
      else if IntersectRect( rRect, cRect, rect( X + TheLength, K * 22 + NL * 22 + Y, X + TheLength + Letter[ j ].sw + Letter[ j ].AdjPrev + Letter[ j ].AdjNext, K * 22 + NL * 22 + 22 + Y ) ) then
      begin
        LineBreak[ k ] := LastSpace;
        k := k + 1;
        TheLength := 0;
        i := LastSpace + 1;
      end
      else
      begin
        if ( j = 32 ) then
        begin //if its a space or linefeed
          LastSpace := i;
        end;
        TheLength := TheLength + Letter[ j ].sw + Letter[ j ].AdjPrev + Letter[ j ].AdjNext;
        if ( TheLength > ( X2 - X ) ) and ( LastSpace > 0 ) then
        begin //time to break the line - if we havent had a space yet, we jsut keep going to avoid a lockup
          LineBreak[ k ] := LastSpace;
          k := k + 1;
          TheLength := 0;
          i := LastSpace + 1;
        end
        else
          i := i + 1;
      end;

    end; //wend
    LineBreak[ k ] := 9999; //set the last linebreak absurdly high - we have to initialize this last break

    XStart := 0;
    k := 0;
    NL := 0;
    for i := 1 to Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      if j = 13 then
      begin
        XStart := 0;
        NL := NL + 1;
      end
      else if ( j < 32 ) or ( j > 255 ) then
      begin
      end
      else if i = LineBreak[ k ] then
      begin //we've hit a space at a line break
        k := k + 1; //so inc the index to the next break, and skip this space plot
        XStart := 0;
      end
      else
      begin
        //if XStart < 300 then begin
        while IntersectRect( rRect, cRect, rect( X + XStart, K * 22 + NL * 22 + Y, X + XStart + Letter[ j ].sw + Letter[ j ].AdjPrev + Letter[ j ].AdjNext, K * 22 + NL * 22 + Y + 22 ) ) and ( XStart < X2 ) do
        begin
          XStart := XStart + 1;
        end;
        //end;
        if Alpha > 0 then
          DrawAlpha( lpDDSBack, Rect( X + XStart + Letter[ j ].AdjPrev, Y + k * 22 + NL * 22 + Letter[ j ].AdjTop, X + XStart + Letter[ j ].sw + Letter[ j ].AdjPrev, Y + Letter[ j ].AdjTop + Letter[ j ].sh + k * 22 + NL * 22 ), Rect( Letter[ j ].sx, Letter[ j ].sy, Letter[ j ].sx + Letter[ j ].sw, Letter[ j ].sy + Letter[ j ].sh ), DXSurface, true, Alpha )
        else
        begin
          pr := Rect( Letter[ j ].sx, Letter[ j ].sy, Letter[ j ].sx + Letter[ j ].sw, Letter[ j ].sy + Letter[ j ].sh );
          lpDDSBack.BltFast( X + XStart + Letter[ j ].AdjPrev, Y + k * 22 + Letter[ j ].AdjTop, DXSurface, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        end;
        XStart := XStart + Letter[ j ].sw + Letter[ j ].AdjPrev + Letter[ j ].AdjNext;
      end;
    end; //endfor

    Result := k + 1; //return the number of lines we run through
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TGameText.PlotTextBlockAroundBox

function TGameText.TextLength( Sentence : string ) : integer;
var
  i : integer;
  j : integer;
  XStart : integer;
  AnsiCodePoints: AnsiString; // The bitmap font is depending on an Ansi position - not its representation
const
  FailName : string = 'TGameText.TextLength';
begin
  Log.DebugLog(FailName);
  Result := 0;
  try
    AnsiCodePoints := AnsiString(Sentence);
    XStart := 0;
    for i := 1 to Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      XStart := XStart + Letter[ j ].sw + Letter[ j ].AdjPrev + Letter[ j ].AdjNext;
    end; //wend

    Result := XStart;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TGameText.TextLength

function TGameText.TinyTextLength( Sentence : string ) : integer;
var
  i : integer;
  j : integer;
  XStart : integer;
  AnsiCodePoints: AnsiString; // The bitmap font is depending on an Ansi position - not its representation
const
  FailName : string = 'TGameText.TinyTextlength';
begin
  Log.DebugLog(FailName);
  Result := 0;
  try
    AnsiCodePoints := AnsiString(Sentence);
    XStart := 0;
    for i := 1 to Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      XStart := XStart + TinyLetter[ j ].sw + TinyLetter[ j ].AdjPrev + TinyLetter[ j ].AdjNext;
    end; //wend

    Result := XStart;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TGameText.TinyTextLength


procedure TGameText.BreakTextIntoAStringList( const Sentence : string; const daList : TStringList; X1, X2 : integer );
//Plots a line of text centered between X and X2
var
  i : integer;
  j : integer;
  k : integer;
  XStart : integer;
  TheLength : integer;
  LastSpace : integer;
  LineBreak : array[ 0..50 ] of integer;
  daString : RawByteString;
  bytes: RawByteString;
const
  FailName : string = 'TGameText.BreakTextIntoAStringList';
begin
  Log.DebugLog(FailName);
  try
    i := 1;
    k := 0;
    TheLength := 0;
    LastSpace := 0;
    LineBreak[ 0 ] := 9999; //in case there are no line breaks, we initalize to an absurdly high number
    bytes := Sentence;
    while i <= Length( bytes ) do
    begin
      j := ord( bytes[ i ] );

      if j = 13 then
      begin
        //debugPlot(j);
        i := i + 1;
        TheLength := 0;
      end
      else if ( j < 32 ) or ( j > 255 ) then
      begin
        i := i + 1;
      end
      else
      begin
        if ( j = 32 ) then
        begin //if its a space or linefeed
          LastSpace := i;
        end;
        TheLength := TheLength + Letter[ j ].sw + Letter[ j ].AdjPrev + Letter[ j ].AdjNext;
        if ( TheLength > ( X2 - X1 ) ) and ( LastSpace > 0 ) then
        begin //time to break the line - if we havent had a space yet, we jsut keep going to avoid a lockup
          LineBreak[ k ] := LastSpace;
          k := k + 1;
          TheLength := 0;
          i := LastSpace + 1;
        end
        else
          i := i + 1;
      end;

    end; //wend
    LineBreak[ k ] := 9999; //set the last linebreak absurdly high - we have to initialize this last break

    daString := '';
    XStart := 0;
    k := 0;
    for i := 1 to Length( bytes ) do
    begin
      j := ord( bytes[ i ] );
      if j = 13 then
      begin
        daList.add( daString );
        XStart := 0;
        //NL:=NL+1;
        daString := '';
      end
      else if ( j < 32 ) or ( j > 255 ) then
      begin
      end
      else if i = LineBreak[ k ] then
      begin //we've hit a space at a line break
        daList.add( daString );
        k := k + 1; //so inc the index to the next break, and skip this space plot
        XStart := 0;
        daString := '';
      end
      else
      begin
        daString := daString + AnsiChar( j );
        XStart := XStart + Letter[ j ].sw + Letter[ j ].AdjPrev + Letter[ j ].AdjNext;
      end;
    end; //endfor
    daList.add( daString );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //BreakTextIntoAStringList

function TGameText.PlotDarkTextCentered( Sentence : string; X, X2, Y, Alpha : integer ) : boolean;
begin
  result := PlotDarkTextCentered2( lpDDSBack, Sentence, X, X2, Y, Alpha );
end;

procedure TGameText.PlotDarkText( Sentence : string;
  X, Y, Alpha : integer );
begin
  PlotDarkText2( lpDDSBack, Sentence, X, Y, Alpha );
end;

procedure TGameText.PlotF13TextCentered( DX : IDirectDrawSurface;
  Sentence : string; X, X2, Y, Alpha : integer );
//Plots a line of text centered between X and X2
var
  i : integer;
  j : integer;
  XStart : integer;
  TheLength : integer;
  pr : TRect;
  AnsiCodePoints: AnsiString; // The bitmap font is depending on an Ansi position - not its representation
const
  FailName : string = 'TGameText.PlotTextCentered2';
begin
  Log.DebugLog(FailName);
  try
    AnsiCodePoints := AnsiString(Sentence);
    TheLength := 0;
  //while (Sentence[i] <> #0) do begin
    for i := 1 to length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      TheLength := TheLength + F13Letter[ j ].sw + F13Letter[ j ].AdjPrev + F13Letter[ j ].AdjNext;
    end; //wend

    XStart := ( ( X2 - X ) - TheLength ) div 2; //center the line of text

    for i := 1 to Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      if Alpha > 0 then
        DrawAlpha( DX, Rect( X + XStart + F13Letter[ j ].AdjPrev, Y + F13Letter[ j ].AdjTop, X + XStart + F13Letter[ j ].sw + F13Letter[ j ].AdjPrev, Y + F13Letter[ j ].AdjTop + F13Letter[ j ].sh ), Rect( F13Letter[ j ].sx, F13Letter[ j ].sy, F13Letter[ j ].sx + F13Letter[ j ].sw, F13Letter[ j ].sy + F13Letter[ j ].sh ), DX13Surface, true, Alpha )
      else
      begin
        pr := Rect( F13Letter[ j ].sx, F13Letter[ j ].sy, F13Letter[ j ].sx + F13Letter[ j ].sw, F13Letter[ j ].sy + F13Letter[ j ].sh );
        DX.BltFast( X + XStart + F13Letter[ j ].AdjPrev, Y + F13Letter[ j ].AdjTop, DXSurface, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end;
      XStart := XStart + F13Letter[ j ].sw + F13Letter[ j ].AdjPrev + F13Letter[ j ].AdjNext;
    end; //wend

  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

procedure TGameText.LoadGoldFontGraphic;
begin
  if assigned( DXGoldSurface ) then
    exit;
  DXGoldSurface := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'fntTinyGold.bmp', cInvisColor );
end;

procedure TGameText.PlotGoldText( DX : IDirectDrawSurface; Sentence : string;
  X, Y, Alpha : integer );
var
  i : integer;
  j : integer;
  XStart : integer;
  pr : TRect;
  AnsiCodePoints: AnsiString; // The bitmap font is depending on an Ansi position - not its representation
const
  FailName : string = 'TGameText.PlotText2';
begin
  Log.DebugLog(FailName);
  try
    AnsiCodePoints := AnsiString(Sentence);
    XStart := 0;
    for i := 1 to Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      if Alpha > 0 then
        DrawAlpha( DX, Rect( X + XStart + TinyLetter[ j ].AdjPrev, Y + TinyLetter[ j ].AdjTop, X + XStart + TinyLetter[ j ].sw + TinyLetter[ j ].AdjPrev, Y + TinyLetter[ j ].AdjTop + TinyLetter[ j ].sh ), Rect( TinyLetter[ j ].sx, TinyLetter[ j ].sy, TinyLetter[ j ].sx + TinyLetter[ j ].sw, TinyLetter[ j ].sy + TinyLetter[ j ].sh ), DXGoldSurface, true, Alpha )
      else
      begin
        pr := Rect( TinyLetter[ j ].sx, TinyLetter[ j ].sy, TinyLetter[ j ].sx + TinyLetter[ j ].sw, TinyLetter[ j ].sy + TinyLetter[ j ].sh );
        DX.BltFast( X + XStart + TinyLetter[ j ].AdjPrev, Y + TinyLetter[ j ].AdjTop, DXGoldSurface, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end;
      XStart := XStart + TinyLetter[ j ].sw + TinyLetter[ j ].AdjPrev + TinyLetter[ j ].AdjNext;
    end; //wend
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

function TGameText.PlotGoldTextBlock( Sentence : string; X, X2, Y,
  Alpha : integer ) : integer;
//Plots a line of text centered between X and X2
var
  i : integer;
  j : integer;
  k : integer;
  NL : integer;
  XStart : integer;
  TheLength : integer;
  LastSpace, PrevLastSpace : integer;
  LineBreak : array[ 0..50 ] of integer;
  pr : TRect;
  AnsiCodePoints: AnsiString; // The bitmap font is depending on an Ansi position - not its representation
const
  FailName : string = 'TGameText.PlotTextBlock';
begin
  Log.DebugLog(FailName);
  Result := 0;
  try
    AnsiCodePoints := AnsiString(Sentence);
    i := 1;
    k := 0;
    TheLength := 0;
    LastSpace := 0;
    PrevLastSpace := 0;
    LineBreak[ 0 ] := 9999; //in case there are no line breaks, we initalize to an absurdly high number
    while i <= Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );

      if j = 13 then
      begin
        //debugPlot(j);
        i := i + 1;
        TheLength := 0;
      end
      else if ( j < 32 ) or ( j > 255 ) then
      begin
        i := i + 1;
      end
      else
      begin
        if ( j = 32 ) then
        begin //if its a space or linefeed
          LastSpace := i;
        end;
        TheLength := TheLength + TinyLetter[ j ].sw + TinyLetter[ j ].AdjPrev + TinyLetter[ j ].AdjNext;
        if ( TheLength > ( X2 - X ) ) and ( LastSpace > PrevLastSpace ) then
        begin //time to break the line - if we havent had a space yet, we jsut keep going to avoid a lockup
          LineBreak[ k ] := LastSpace;
          k := k + 1;
          TheLength := 0;
          i := LastSpace + 1;
          LastSpace := PrevLastSpace;
        end
        else
          i := i + 1;
      end;

    end; //wend
    LineBreak[ k ] := 9999; //set the last linebreak absurdly high - we have to initialize this last break

    XStart := 0;
    k := 0;
    NL := 0;
    for i := 1 to Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      if j = 13 then
      begin
        XStart := 0;
        NL := NL + 1;
      end
      else if ( j < 32 ) or ( j > 255 ) then
      begin
      end
      else if i = LineBreak[ k ] then
      begin //we've hit a space at a line break
        k := k + 1; //so inc the index to the next break, and skip this space plot
        XStart := 0;
      end
      else
      begin
        if Alpha > 0 then
          DrawAlpha( lpDDSBack, Rect( X + XStart + TinyLetter[ j ].AdjPrev, Y + k * 22 + NL * 22 + TinyLetter[ j ].AdjTop, X + XStart + TinyLetter[ j ].sw + TinyLetter[ j ].AdjPrev, Y + TinyLetter[ j ].AdjTop + TinyLetter[ j ].sh + k * 22 + NL * 22 ), Rect( TinyLetter[ j ].sx, TinyLetter[ j ].sy, TinyLetter[ j ].sx + TinyLetter[ j ].sw, TinyLetter[ j ].sy + TinyLetter[ j ].sh ), DXGoldSurface, true, Alpha )
        else
        begin
          pr := Rect( TinyLetter[ j ].sx, TinyLetter[ j ].sy, TinyLetter[ j ].sx + TinyLetter[ j ].sw, TinyLetter[ j ].sy + TinyLetter[ j ].sh );
          lpDDSBack.BltFast( X + XStart + TinyLetter[ j ].AdjPrev, Y + k * 22 + TinyLetter[ j ].AdjTop, DXGoldSurface, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        end;
        XStart := XStart + TinyLetter[ j ].sw + TinyLetter[ j ].AdjPrev + TinyLetter[ j ].AdjNext;
      end;
    end; //endfor

    Result := k + 1; //return the number of lines we run through
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

function TGameText.PlotGoldTextCentered( DX : IDirectDrawSurface;
  Sentence : string; X, X2, Y, Alpha : integer ) : boolean;
//Plots a line of text centered between X and X2
var
  i : integer;
  j : integer;
  XStart : integer;
  TheLength : integer;
  ThereWasRoom : boolean;
  pr : TRect;
  AnsiCodePoints: AnsiString; // The bitmap font is depending on an Ansi position - not its representation
const
  FailName : string = 'TGameText.PlotTextCentered2';
begin
  Log.DebugLog(FailName);
  Result := false;
  try
    AnsiCodePoints := AnsiString(Sentence);
    TheLength := 0;
  //while (Sentence[i] <> #0) do begin
    for i := 1 to length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      TheLength := TheLength + TinyLetter[ j ].sw + TinyLetter[ j ].AdjPrev + TinyLetter[ j ].AdjNext;
    end; //wend

    ThereWasRoom := True;
    XStart := ( ( X2 - X ) - TheLength ) div 2; //center the line of text
    if XStart < X then
    begin //there wasn't enough space for the entire line
      ThereWasRoom := false;
    end;

    for i := 1 to Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      if Alpha > 0 then
        DrawAlpha( DX, Rect( X + XStart + TinyLetter[ j ].AdjPrev, Y + TinyLetter[ j ].AdjTop, X + XStart + TinyLetter[ j ].sw + TinyLetter[ j ].AdjPrev, Y + TinyLetter[ j ].AdjTop + TinyLetter[ j ].sh ), Rect( TinyLetter[ j ].sx, TinyLetter[ j ].sy, TinyLetter[ j ].sx + TinyLetter[ j ].sw, TinyLetter[ j ].sy + TinyLetter[ j ].sh ), DXGoldSurface, true, Alpha )
      else
      begin
        pr := Rect( TinyLetter[ j ].sx, TinyLetter[ j ].sy, TinyLetter[ j ].sx + TinyLetter[ j ].sw, TinyLetter[ j ].sy + TinyLetter[ j ].sh );
        DX.BltFast( X + XStart + TinyLetter[ j ].AdjPrev, Y + TinyLetter[ j ].AdjTop, DXGoldSurface, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end;
      XStart := XStart + TinyLetter[ j ].sw + TinyLetter[ j ].AdjPrev + TinyLetter[ j ].AdjNext;
    end; //wend

    Result := ThereWasRoom;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

procedure TGameText.UnLoadGoldFontGraphic;
begin
//  if assigned( DXGoldSurface ) then
//    DXGoldSurface := nil;
end;

function TGameText.PlotTinyTextBlock( Sentence : string; X, X2, Y,
  Alpha : integer ) : integer;
//Plots a line of text centered between X and X2
var
  i : integer;
  j : integer;
  k : integer;
  NL : integer;
  XStart : integer;
  TheLength : integer;
  LastSpace, PrevLastSpace : integer;
  LineBreak : array[ 0..50 ] of integer;
  pr : TRect;
  AnsiCodePoints: AnsiString; // The bitmap font is depending on an Ansi position - not its representation
const
  FailName : string = 'TGameText.PlotTextBlock';
begin
  Log.DebugLog(FailName);
  Result := 0;
  try
    AnsiCodePoints := AnsiString(Sentence);
    i := 1;
    k := 0;
    TheLength := 0;
    LastSpace := 0;
    PrevLastSpace := 0;
    LineBreak[ 0 ] := 9999; //in case there are no line breaks, we initalize to an absurdly high number
    while i <= Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );

      if j = 13 then
      begin
        //debugPlot(j);
        i := i + 1;
        TheLength := 0;
      end
      else if ( j < 32 ) or ( j > 255 ) then
      begin
        i := i + 1;
      end
      else
      begin
        if ( j = 32 ) then
        begin //if its a space or linefeed
          LastSpace := i;
        end;
        TheLength := TheLength + TinyLetter[ j ].sw + TinyLetter[ j ].AdjPrev + TinyLetter[ j ].AdjNext;
        if ( TheLength > ( X2 - X ) ) and ( LastSpace > PrevLastSpace ) then
        begin //time to break the line - if we havent had a space yet, we jsut keep going to avoid a lockup
          LineBreak[ k ] := LastSpace;
          k := k + 1;
          TheLength := 0;
          i := LastSpace + 1;
          LastSpace := PrevLastSpace;
        end
        else
          i := i + 1;
      end;

    end; //wend
    LineBreak[ k ] := 9999; //set the last linebreak absurdly high - we have to initialize this last break

    XStart := 0;
    k := 0;
    NL := 0;
    for i := 1 to Length( AnsiCodePoints ) do
    begin
      j := integer( AnsiCodePoints[ i ] );
      if j = 13 then
      begin
        XStart := 0;
        NL := NL + 1;
      end
      else if ( j < 32 ) or ( j > 255 ) then
      begin
      end
      else if i = LineBreak[ k ] then
      begin //we've hit a space at a line break
        k := k + 1; //so inc the index to the next break, and skip this space plot
        XStart := 0;
      end
      else
      begin
        if Alpha > 0 then
          DrawAlpha( lpDDSBack, Rect( X + XStart + TinyLetter[ j ].AdjPrev, Y + k * 18 + NL * 18 + TinyLetter[ j ].AdjTop, X + XStart + TinyLetter[ j ].sw + TinyLetter[ j ].AdjPrev, Y + TinyLetter[ j ].AdjTop + TinyLetter[ j ].sh + k * 18 + NL * 18 ), Rect( TinyLetter[ j ].sx, TinyLetter[ j ].sy, TinyLetter[ j ].sx + TinyLetter[ j ].sw, TinyLetter[ j ].sy + TinyLetter[ j ].sh ), DXTinySurface, true, Alpha )
        else
        begin
          pr := Rect( TinyLetter[ j ].sx, TinyLetter[ j ].sy, TinyLetter[ j ].sx + TinyLetter[ j ].sw, TinyLetter[ j ].sy + TinyLetter[ j ].sh );
          lpDDSBack.BltFast( X + XStart + TinyLetter[ j ].AdjPrev, Y + k * 18 + TinyLetter[ j ].AdjTop, DXTinySurface, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        end;
        XStart := XStart + TinyLetter[ j ].sw + TinyLetter[ j ].AdjPrev + TinyLetter[ j ].AdjNext;
      end;
    end; //endfor

    Result := k + 1; //return the number of lines we run through
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

end.
