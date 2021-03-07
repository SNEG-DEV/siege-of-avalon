unit SoAOS.Intrface.Dialogs.Journal;
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

  Description: Journal/History Dialog - was Journal.pas - a lot more clean-up is coming

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
  System.SysUtils,
  System.IOUtils,
  System.Types,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Imaging.jpeg,
  System.IniFiles,
  SoAOS.Intrface.Dialogs,
  Resource,
  GameText,
  AdventureLog,
  LogFile,
  SoAOS.Animation,
  Engine;

type

  TJournal = class( TDialog )
  private
    //Bitmap stuff
    DXBack : IDirectDrawSurface;
    DXPic : IDirectDrawSurface;
    PicWidth : integer;
    PicHeight : integer;
    LogText : AnsiString;
    CurrentLogIndex : integer;
    txtMessage : array[ 0..1 ] of string;
    procedure ShowText;
    procedure FormMouseDown( Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer );
    procedure FormMouseMove( Sender : TObject; Shift : TShiftState; X, Y : Integer );
  protected
    procedure MouseDown( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer ); override;
    procedure MouseMove( Sender : TObject;
      Shift : TShiftState; X, Y, GridX, GridY : Integer ); override;
    procedure MouseUp( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer ); override;
  public
    frmMain : TForm;
    //LogFile: string;
    StartLogIndex : integer;
    PicXY : TPoint;
    JournalLog : TAdventureLog;
    procedure Init; override;
    procedure Release; override;
  end;

implementation

uses
  SoAOS.Types,
  SoAOS.Intrface.Text,
  SoAOS.Graphics.Draw,
  AniDemo;

{ TJournal }

procedure TJournal.Init;
var
  i : integer;
  Ini : TIniFile;
  pr : TRect;
const
  FailName : string = 'TJournal.init';
begin
  Log.DebugLog(FailName);
  try

    if Loaded then
      Exit;
    inherited;
    ExText.Open( 'Journal' );
    for i := 0 to 1 do
      txtMessage[ i ] := ExText.GetText( 'Message' + inttostr( i ) );

    MouseCursor.Cleanup;
    pr := Rect( 0, 0, ResWidth, ResHeight );
    lpDDSBack.BltFast( 0, 0, lpDDSFront, @pr, DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;
    if JournalLog.LogFileList.count - 1 > StartLogIndex then
      inc( StartLogIndex );

  //JournalLog.create:=TAdventureLog.create;
  //JournalLog.LogDirectory:=ExtractFilePath(Application.ExeName) + 'Journal\';
    INI := TIniFile.Create( SiegeINIFile );

    if ( INI.readinteger( 'Settings', 'JournalFont', 0 ) = 1 ) and TDirectory.Exists( ResourcePath + 'journalalt\'  + Language ) then
      JournalLog.LogDirectory := ResourcePath + 'journalalt\' + Language + '\'
    else
      JournalLog.LogDirectory := ResourcePath + 'journal\' + Language + '\';

    ini.free;
  //LogText:=JournalLog.ReadLogByName(LogFile);
  //Show Latest Log
  //LogText:=JournalLog.ReadLogByIndex(JournalLog.LogFileList.count-1);
  //LogText:=ReadALog(ExtractFilePath(Application.ExeName) + 'Journal\' + LogFile);

    CurrentLogIndex := StartLogIndex; //JournalLog.LogFileList.count-1;
    if CurrentLogIndex >= JournalLog.LogFileList.count then
      CurrentLogIndex := JournalLog.LogFileList.count - 1;
  //Set mouse events for form
    frmMain.OnMouseDown := FormMouseDown;
    frmMain.OnMouseMove := FormMouseMove;

    pText.LoadFontGraphic( 'inventory' ); //load the statistics font graphic in

    DXBack := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'Journal.bmp', cInvisColor, DlgWidth, DlgHeight );

  {//Now for the Alpha'ed edges
  BMBack.LoadFromFile(ExtractFilePath(Application.ExeName) + 'Options\Dialog-Shadow.bmp');
  DXBorders := DDGetImage(lpDD, BMBack, InvisColor, False);
  DrawSub(lpDDSBack, Rect(296+XAdj, 175+YAdj, 296+XAdj + BMBack.Width, 175+YAdj+BMBack.Height), Rect(0, 0, BMBack.Width, BMBack.Height), DXBorders, True, 128);

  DXBorders:=nil;
  }

    ShowText;
    SoAOS_DX_BltFront;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //Init

procedure TJournal.FormMouseDown( Sender : TObject; Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer );
var
  pr1, pr2, pr3: TRect;
const
  FailName : string = 'TJournal.formmousedown';
begin
  try
  //check for clicks
    pr1 := ApplyOffset( Rect( 582, 575, 659, 596 ) );
    pr2 := ApplyOffset( Rect( 681, 575, 721, 596 ) );
    pr3 := ApplyOffset( Rect( 746, 575, 786, 596 ) );

    if pr1.Contains( Point( x, y ) ) then
    begin //prev
      if CurrentLogIndex > 0 then
      begin
        CurrentLogIndex := CurrentLogIndex - 1;
        ShowText;
      end;
    end
    else if pr2.Contains( Point( x, y ) ) then
    begin //next
      if CurrentLogIndex < JournalLog.LogFileList.count - 1 then
      begin
        CurrentLogIndex := CurrentLogIndex + 1;
        ShowText;
      end;
    end
    else if pr3.Contains( Point( x, y ) ) then
    begin //exit
      Close;
      frmMain := nil;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //FormMouseDown

procedure TJournal.FormMouseMove( Sender : TObject; Shift : TShiftState; X, Y : Integer );
const
  FailName : string = 'TJournal.formmousemove';
begin
  try
  //clean up
{  lpDDSBack.BltFast(297,571,DXBack,rect(297,571,798,593),DDBLTFAST_WAIT);

  //Plot buttons
  pText.PlotText('Previous',300,570,240);
  pText.PlotText('Next',450,570,240);
  pText.PlotText('Exit',740,570,240);

  //plot rollovers
  if ptinRect(rect(298,571,374,592),point(x,y)) and (CurrentLogIndex > 0) then begin //prev
     pText.PlotDarkText('Previous',300,570,240);
  end
  else if ptinRect(rect(447,571,497,592),point(x,y)) and (CurrentLogIndex < JournalLog.LogFileList.count-1) then begin  //next
     pText.PlotDarkText('Next',450,570,240);
  end
  else if ptinRect(rect(738,571,780,592),point(x,y)) then begin  //exit
     pText.PlotDarkText('Exit',740,570,240);
  end;

  lpDDSFront_Flip(nil, DDFLIP_WAIT);
  lpDDSBack.BltFast(0, 0, lpDDSFront, Rect(0, 0, 800, 600), DDBLTFAST_WAIT);
  }
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //FormMouseMove

procedure TJournal.MouseDown( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer );
var
  pr1, pr2, pr3: TRect;
begin
  try
    pr1 := ApplyOffset( Rect( 582, 575, 659, 596 ) );
    pr2 := ApplyOffset( Rect( 681, 575, 721, 596 ) );
    pr3 := ApplyOffset( Rect( 746, 575, 786, 596 ) );
  //check for clicks
    if pr1.Contains( Point( x, y ) ) then
    begin //prev
      if CurrentLogIndex > 0 then
      begin
        CurrentLogIndex := CurrentLogIndex - 1;
        ShowText;
      end;
    end
    else if pr2.Contains( Point( x, y ) ) then
    begin //next
      if CurrentLogIndex < JournalLog.LogFileList.count - 1 then
      begin
        CurrentLogIndex := CurrentLogIndex + 1;
        ShowText;
      end;
    end
    else if pr3.Contains( Point( x, y ) ) then
    begin //exit
      Close;
      frmMain := nil;
    end;
  except
    on E : Exception do
     exit;
  end;
end; //MouseDown

procedure TJournal.MouseMove( Sender : TObject;
      Shift : TShiftState; X, Y, GridX, GridY : Integer );
begin

end; //MouseMove

procedure TJournal.MouseUp( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer );
begin

end;


procedure TJournal.Release;
const
  FailName : string = 'TJournal.release';
begin
  Log.DebugLog(FailName);
  try
    ExText.close;
//   StartLogIndex:=JournalLog.LogFileList.count-1;
    DXBack := nil;
   //DXPic:=nil;
    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;


procedure TJournal.ShowText;
var
  BM : TBitmap;
  width, height : Integer;
  PicName : string;
  jpg : TJPEGImage;
  pr : TRect;
const
  FailName : string = 'TJournal.showtext';
begin
  Log.DebugLog(FailName);
  try

    if CurrentLogIndex > StartLogIndex then
      StartLogIndex := CurrentLogIndex;
  //clear screen
    pr := Rect( 0, 0, DlgWidth, DlgHeight );
    lpDDSBack.BltFast( Offset.X, Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
  //Plot buttons
  //pText.PlotText('Previous',300,570,240);
  //pText.PlotText('Next',450,570,240);
  //pText.PlotText('Exit',740,570,240);

  //Show Latest Log
    LogText := JournalLog.ReadLogByIndex( CurrentLogIndex );

//TODO: JPEG code should go - or be interchangable with BMP (and PNG)
  //get the pic, if it exists
    if ( CurrentLogIndex >= 0 ) and ( CurrentLogIndex < JournalLog.LogFileList.count ) then
    begin
      if TFile.Exists( JournalLog.LogDirectory + ChangeFileExt( JournalLog.LogFileList.strings[ CurrentLogIndex ], '.bmp' ) ) then
      begin
        PicName := JournalLog.LogDirectory + ChangeFileExt( JournalLog.LogFileList.strings[ CurrentLogIndex ], '.bmp' );
        if LogText <> '' then
          PicXY := point( 20, 20 )
        else
          PicXY := point( 0, 0 );
        DXPic := SoAOS_DX_LoadBMP( PicName, cInvisColor, width, height );
        PicWidth := width;
        PicHeight := height;
        pr := Rect( 0, 0, width, Height );
        lpDDSBack.BltFast( PicXY.X + Offset.X, PicXY.Y + Offset.Y, DXPic, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        if LogText <> '' then
          pText.PlotTextBlockAroundBox( Logtext, 50, 750, 50, 240, rect( PicXY.X, PicXY.Y, PicXY.X + PicWidth + 20, PicXY.Y + PicHeight + 20 ) );
      end
      else if TFile.Exists( JournalLog.LogDirectory + ChangeFileExt( JournalLog.LogFileList.strings[ CurrentLogIndex ], '.jpg' ) ) then
      begin
        PicName := JournalLog.LogDirectory + ChangeFileExt( JournalLog.LogFileList.strings[ CurrentLogIndex ], '.jpg' );
        if LogText <> '' then
          PicXY := point( 20, 20 )
        else
          PicXY := point( 0, 0 );

        BM := TBitmap.create;
        try
          jpg := TJPEGImage.create;
          jpg.LoadFromFile( picname );

          BM.width := jpg.width;
          BM.Height := jpg.height;
          Bm.Canvas.Draw( 0, 0, jpg );

          jpg.Free;

          DXPic := SoAOS_DX_SurfaceFromBMP( BM, cInvisColor );
          PicWidth := BM.width;
          PicHeight := BM.height;
        finally
          BM.Free;
        end;

        pr := Rect( 0, 0, BM.width, BM.Height );
        lpDDSBack.BltFast( PicXY.X + Offset.X, PicXY.Y + Offset.Y, DXPic, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        if LogText <> '' then
          pText.PlotTextBlockAroundBox( Logtext, 50, 750, 50, 240, rect( PicXY.X, PicXY.Y, PicXY.X + PicWidth + 20, PicXY.Y + PicHeight + 20 ) );
      end
      else if LogText <> '' then
        pText.PlotTextBlock( Logtext, 50, 750, 50, 240 );
    end
    else
      pText.PlotTextBlock( Logtext, 50, 750, 50, 240 );

    if not NoPageNumbers then
      pText.plotText( txtMessage[ 0 ] + intToStr( CurrentLogIndex + 1 ) + txtMessage[ 1 ] + IntToStr( JournalLog.LogFileList.count ), 81 + Offset.X, 575 + Offset.Y, 255 );
    SoAOS_DX_BltFront;

    DXPic := nil;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //ShowText


end.
