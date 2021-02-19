unit LogScreen;
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
  System.SysUtils,
  System.IOUtils,
  System.Types,
  System.Classes,
  Vcl.Controls,
  GameText,
  Display,
  Engine,
  System.IniFiles,
  LogFile,
  SoAOS.Animation,
  Resource;

type
  TLogScreen = class( TDisplay )
  private
    //Bitmap stuff
    DXBack : IDirectDrawSurface;
    DXBackToGame : IDirectDrawSurface; //Back To Game highlight
    DXPrev : IDirectDrawSurface;
    DXNext : IDirectDrawSurface;
    DXPrev2 : IDirectDrawSurface;
    DXNext2 : IDirectDrawSurface;
    MaxPages : integer;
    INI : TMemIniFile;
    IniFileFound : boolean;
    FileName : string;
    TitleName : string;
    Title : string;
    DontPlotText : boolean;
    FirstItem : array[ 0..1000 ] of integer; //index of stringlist for starting item on each page (Max 100 pages)
    txtMessage : array[ 0..1 ] of string;
    procedure ShowText( Page : integer );
  protected
    procedure MouseDown( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer ); override;
    procedure MouseMove( Sender : TObject;
      Shift : TShiftState; X, Y, GridX, GridY : Integer ); override;
  public
    PageNumber : integer;
    LogInfo : TStringList;
    constructor Create;
    destructor Destroy; override;
    procedure Init; override;
    procedure Release; override;
  end;

  TQuestLog = class( TLogScreen )
  public
    constructor Create;
  end;

  TAdvLog = class( TLogScreen )
  public
    constructor Create;
  end;

implementation

uses
  SoAOS.Types,
  SoAOS.Intrface.Text,
  SoAOS.Graphics.Draw,
  AniDemo;

{ TLogScreen }

constructor TLogScreen.Create;
const
  FailName : string = 'TLogScreen.Create';
begin
  Log.DebugLog( FailName );
  try
    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //Create

destructor TLogScreen.Destroy;
const
  FailName : string = 'TLogScreen.Destroy';
begin
  Log.DebugLog( FailName );
  try

    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //Destroy

procedure TLogScreen.Init;
var
  DXBorder : IDirectDrawSurface;
  i, width, height : integer;
  pr : TRect;
const
  FailName : string = 'TLogScreen.init';
begin
  Log.DebugLog( FailName );
  try

    if Loaded then
      Exit;
    inherited;
    MouseCursor.Cleanup;

    pText.LoadFontGraphic( 'inventory' ); //load the inventory font graphic in

    ExText.Open( 'LogScreen' );
    for i := 0 to 1 do
      txtMessage[ i ] := ExText.GetText( 'Message' + inttostr( i ) );

    Title := ExText.GetText( TitleName );

    FirstItem[ 0 ] := 0;

    if TFile.Exists( FileName ) then
    begin
      INI := TMeminifile.create( FileName );
      IniFileFound := true;
    end
    else
    begin
      IniFileFound := false;
      log.log( '***ERROR ' + FileName + ' not found' );
    end;

  //if (MaxPages < 1) or (MaxPages > 1000) then begin //figure out how many pages

    if LogInfo.count > 0 then
    begin
      MaxPages := 1000;
      DontPlotText := true;
      i := 0;
      while i < MaxPages do
      begin
        ShowText( i );
        inc( i );
      end;

      if ( PageNumber < 0 ) or ( PageNumber > MaxPages ) then
      begin
        PageNumber := MaxPages;
      end;
      DontPlotText := false;
    end
    else
    begin
      MaxPages := 0;
      PageNumber := MaxPages;
    end;
  //end;

    pr := Rect( 0, 0, ResWidth, ResHeight );
    lpDDSBack.BltFast( 0, 0, lpDDSFront, @pr, DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;

  //ShowText(PageNumber); //to see if we have more than 1 page

    if IniFileFound and ( MaxPages > 0 ) then
    begin //(LogInfo.count > 15) then begin
      DXPrev := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'logPrevious.bmp', cTransparent );
      DXPrev2 := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'logPrevious2.bmp', cTransparent );
      DXNext := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'logNext.bmp', cTransparent );
      DXNext2 := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'logNext2.bmp', cTransparent );
    end;

    DXBackToGame := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'obInvBackToGame.bmp', cInvisColor );
    DXBack := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'LogScreen.bmp', cTransparent, width, height );
    pr := Rect( 0, 0, width, height );
    lpDDSBack.BltFast( 0, 0, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

  //Now for the Alpha'ed edges
    DXBorder := SoAOS_DX_LoadBMP( InterfacePath + 'obInvRightShadow.bmp', cInvisColor, width, height );
    DrawSub( lpDDSBack, Rect( 659, 0, 659 + width, height ), Rect( 0, 0, width, height ), DXBorder, True, 150 );

    DXBorder := nil;

    DXBorder := SoAOS_DX_LoadBMP( InterfacePath + 'obInvBottomShadow.bmp', cInvisColor, width, height );
    DrawSub( lpDDSBack, Rect( 0, 456, width, 456 + height ), Rect( 0, 0, width, height ), DXBorder, True, 150 );

    DXBorder := nil; //release DXBorder

    if IniFileFound and ( MaxPages > 0 ) then
    begin //(LogInfo.count > 15) then begin
      pr := Rect( 0, 0, 86, 29 );
      lpDDSBack.BltFast( 400, 424, DXPrev, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      pr := Rect( 0, 0, 62, 27 );
      lpDDSBack.BltFast( 500, 424, DXNext, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    end;

    pText.PlotText( Title, 5, 5, 240 );

    ShowText( PageNumber );
    pText.plotText( txtMessage[ 0 ] + inttostr( PageNumber + 1 ) + txtMessage[ 1 ] + inttostr( MaxPages + 1 ), 20, 424, 240 );

    SoAOS_DX_BltFront;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //Init


procedure TLogScreen.MouseDown( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer );
const
  FailName : string = 'TLogScreen.Mousedown';
var
  pr : TRect;
begin
  Log.DebugLog( FailName );
  try

    if IniFileFound and ( MaxPages > 0 ) then
    begin //(LogInfo.count > 15) then begin
      if PtinRect( rect( 400, 424, 400 + 86, 424 + 29 ), point( X, Y ) ) then
      begin //over prev
        if PageNumber > 0 then
        begin
          PageNumber := PageNumber - 1;
          pr := Rect( 0, 40, 650, 415 );
          lpDDSBack.BltFast( 0, 40, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
          pr := Rect( 20, 424, 350, 450 );
          lpDDSBack.BltFast( 20, 424, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
          pText.plotText( txtMessage[ 0 ] + inttostr( PageNumber + 1 ) + txtMessage[ 1 ] + inttostr( MaxPages + 1 ), 20, 424, 240 );
          ShowText( PageNumber );
        end;
      end;
      if PtinRect( rect( 500, 424, 500 + 86, 424 + 29 ), point( X, Y ) ) then
      begin //over next
         //Get the mex number of pages
        { if (LogInfo.count mod 15) > 0 then  //if there's an extra few items add a page
            i:=(LogInfo.count div 15)
         else
            i:=(LogInfo.count div 15)-1;
         if PageNumber < i then begin }
        if PageNumber < MaxPages then
        begin
          PageNumber := PageNumber + 1;
          pr := Rect( 0, 40, 650, 415 );
          lpDDSBack.BltFast( 0, 40, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
          pr := Rect( 20, 424, 350, 450 );
          lpDDSBack.BltFast( 20, 424, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
          pText.plotText( txtMessage[ 0 ] + inttostr( PageNumber + 1 ) + txtMessage[ 1 ] + inttostr( MaxPages + 1 ), 20, 424, 240 );
          ShowText( PageNumber );
        end;

      end;
    end;
    if PtinRect( rect( 588, 407, 588 + 77, 412 + 54 ), point( X, Y ) ) then
    begin //over back button
      Close;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //MouseDown

procedure TLogScreen.MouseMove( Sender : TObject;
      Shift : TShiftState; X, Y, GridX, GridY : Integer );
const
  FailName : string = 'TLogScreen.mouseMove';
var
  pr : TRect;
begin
  Log.DebugLog( FailName );
  try
    pr := Rect( 588, 407, 588 + 77, 407 + 54 );
    lpDDSBack.BltFast( 588, 407, DXBack, @pr, DDBLTFAST_WAIT );
    if IniFileFound and ( MaxPages > 0 ) then
    begin //(LogInfo.count > 15) then begin
      pr := Rect( 0, 0, 86, 29 );
      lpDDSBack.BltFast( 400, 424, DXPrev, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      pr := Rect( 0, 0, 62, 27 );
      lpDDSBack.BltFast( 500, 424, DXNext, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    end;
    if IniFileFound and ( MaxPages > 0 ) then
    begin //(LogInfo.count > 15) then begin
      if PtinRect( rect( 400, 424, 400 + 86, 424 + 29 ), point( X, Y ) ) then
      begin //over prev
        pr := Rect( 0, 0, 86, 29 );
        lpDDSBack.BltFast( 400, 424, DXPrev2, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end;
      if PtinRect( rect( 500, 424, 500 + 86, 424 + 29 ), point( X, Y ) ) then
      begin //over next
        pr := Rect( 0, 0, 62, 27 );
        lpDDSBack.BltFast( 500, 424, DXNext2, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end;
    end;
    if PtinRect( rect( 588, 407, 588 + 77, 412 + 54 ), point( X, Y ) ) then
    begin //over back button
      //plot highlighted back to game
      pr := Rect( 0, 0, 77, 54 );
      lpDDSBack.BltFast( 588, 407, DXBackToGame, @pr, DDBLTFAST_WAIT );
    end;

    SoAOS_DX_BltFront;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //MouseMove

procedure TLogScreen.ShowText( Page : integer );
var
  i, BlockHeight, Y : integer;
  LineCount : integer;
  S : string;
const
  FailName : string = 'TLogScreen.ShowText';
begin
  Log.DebugLog( FailName );
  try
  //DebugPrint(inttostr(LogInfo.count-1));
    if IniFileFound then
    begin
      Y := 40;
      LineCount := 0;
    //DebugPrint(inttostr(LogInfo.count-1));
      for i := FirstItem[ Page ] to LogInfo.count - 1 do
      begin
        if LineCount < 15 then
        begin
          S := INI.readString( 'Quests', LogInfo.Strings[ i ], '' );
          if S <> '' then
          begin
            BlockHeight := pText.TextBlockHeight( S, 20, 640, 0 );
            if BlockHeight + LineCount < 15 then
            begin //we have room
              if DontPlotText = false then
                pText.PlotTextBlock( S, 20, 640, Y, 240 );
              Y := Y + BlockHeight * 25; //Y:=Y+25;
              if BlockHeight = 1 then
                Y := Y + 6; //single lines come out too close- Kludge
              LineCount := LineCount + BlockHeight; //inc(LineCount);
              if i = LogInfo.count - 1 then
              begin
                MaxPages := Page; //if this is the last item, the is the last page
                   //debugPrint('hit max page'+inttostr(MaxPages));
                   //debugprint('loginfo-1='+inttostr(LogInfo.count-1));
              end;
            end
            else
            begin //we dont have room
              LineCount := 15;
              FirstItem[ Page + 1 ] := i; //set the next pages first item
                //DebugPrint('page:'+inttostr(Page+1) + ' item:' + inttostr(i));
            end; //endif BlockHeight
          end
          else
          begin
            if i = LogInfo.count - 1 then
            begin
              MaxPages := Page; //if this is the last item, the is the last page
            end;
          end; //end if
        end;
      end; //end for
    end; //endif
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //ShowText

procedure TLogScreen.Release;
const
  FailName : string = 'TLogScreen.Release';
begin
  Log.DebugLog( FailName );
  try
    ExText.close;
    DXBack := nil;
    DXBackToGame := nil;
    if assigned( DXPrev ) then
      DXPrev := nil;
    if assigned( DXNext ) then
      DXNext := nil;
    if assigned( DXPrev2 ) then
      DXPrev2 := nil;
    if assigned( DXNext2 ) then
      DXNext2 := nil;
    INI.free;
    INI := nil;
    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

{ TQuestLog }

constructor TQuestLog.Create;
begin
  inherited;
  TitleName := 'Message3';
  FileName := ResourcePath + 'conversations\'+Language+'\Quests.lst';
end;

{ TAdvLog }

constructor TAdvLog.Create;
begin
  inherited;
  TitleName := 'Message2';
  FileName := ResourcePath + 'conversations\'+Language+'\Adventures.lst';
end;

end.
