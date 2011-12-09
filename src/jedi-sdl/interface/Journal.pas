unit Journal;
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
  jpeg,
  iniFiles,
  FileCtrl,
  ExtCtrls,
  Character,
  Resource,
  StdCtrls,
  GameText,
  Display,
  Anigrp30,
  AdventureLog,
  LogFile,
  Engine;

type
  TJournal = class( TDisplay )
  private
    //Bitmap stuff
    BMBack : TBitmap;
    DXBack : IDirectDrawSurface;
    DXPic : IDirectDrawSurface;
    PicWidth : integer;
    PicHeight : integer;
    LogText : string;
    CurrentLogIndex : integer;
    txtMessage : array[ 0..1 ] of string;
    procedure ShowText;
    procedure FormMouseDown( Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer );
    procedure FormMouseMove( Sender : TObject; Shift : TShiftState; X, Y : Integer );
  protected
    procedure MouseDown( Sender : TAniview; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
    procedure MouseMove( Sender : TAniview;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
    procedure MouseUp( Sender : TAniview; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
  public
    frmMain : TForm;
    //LogFile: string;
    StartLogIndex : integer;
    PicXY : TPoint;
    JournalLog : TAdventureLog;
    constructor Create;
    destructor Destroy; override;
    procedure Init; override;
    procedure Release; override;
  end;
implementation
uses
  AniDemo;
{ TJournal }

constructor TJournal.Create;
const
  FailName : string = 'TJournal.create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //Create

destructor TJournal.Destroy;
const
  FailName : string = 'TJournal.destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //Destroy

procedure TJournal.Init;
var
  InvisColor : integer;
  i : integer;
  Ini : TIniFile;
const
  FailName : string = 'TJournal.init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if Loaded then
      Exit;
    inherited;
    ExText.Open( 'Journal' );
    for i := 0 to 1 do
      txtMessage[ i ] := ExText.GetText( 'Message' + inttostr( i ) );

    MouseCursor.Cleanup;
    WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, ResWidth, ResHeight ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;
    if JournalLog.LogFileList.count - 1 > StartLogIndex then
      inc( StartLogIndex );

  //JournalLog.create:=TAdventureLog.create;
  //JournalLog.LogDirectory:=ExtractFilePath(Application.ExeName) + 'Journal/';
    INI := TIniFile.Create( DefaultPath + 'siege.ini' );

    if ( INI.readinteger( 'Settings', 'JournalFont', 0 ) = 1 ) and DirectoryExists( ArtPath + 'journalalt' ) then
      JournalLog.LogDirectory := ArtPath + 'journalalt/'
    else
      JournalLog.LogDirectory := ArtPath + 'journal/';

    ini.free;
  //LogText:=JournalLog.ReadLogByName(LogFile);
  //Show Latest Log
  //LogText:=JournalLog.ReadLogByIndex(JournalLog.LogFileList.count-1);
  //LogText:=ReadALog(ExtractFilePath(Application.ExeName) + 'Journal/' + LogFile);

    CurrentLogIndex := StartLogIndex; //JournalLog.LogFileList.count-1;
    if CurrentLogIndex >= JournalLog.LogFileList.count then
      CurrentLogIndex := JournalLog.LogFileList.count - 1;
  //Set mouse events for form
    frmMain.OnMouseDown := FormMouseDown;
    frmMain.OnMouseMove := FormMouseMove;

    pText.LoadFontGraphic( 'inventory' ); //load the statistics font graphic in
    BMBack := TBitmap.Create;
  //transparent color
    InvisColor := $00FFFF00;

    BMBack.LoadFromFile( InterfacePath + 'Journal.bmp' );
    DXBack := DDGetImage( lpDD, BMBack, InvisColor, False );
  //WrapperBltFast( lpDDSBack, 0, 0, DXBack, Rect(0, 0, BMBack.width, BMBack.Height), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);

  {//Now for the Alpha'ed edges
  BMBack.LoadFromFile(ExtractFilePath(Application.ExeName) + 'Options/Dialog-Shadow.bmp');
  DXBorders := DDGetImage(lpDD, BMBack, InvisColor, False);
  DrawSub(lpDDSBack, Rect(296+XAdj, 175+YAdj, 296+XAdj + BMBack.Width, 175+YAdj+BMBack.Height), Rect(0, 0, BMBack.Width, BMBack.Height), DXBorders, True, 128);

  DXBorders:=nil;
  }
    BMBack.Free;

    ShowText;
    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //Init

procedure TJournal.FormMouseDown( Sender : TObject; Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer );
const
  FailName : string = 'TJournal.formmousedown';
begin
  try
  //check for clicks
    if ptinRect( rect( 582, 575, 659, 596 ), point( x, y ) ) then
    begin //prev
      if CurrentLogIndex > 0 then
      begin
        CurrentLogIndex := CurrentLogIndex - 1;
        ShowText;
      end;
    end
    else if ptinRect( rect( 681, 575, 721, 596 ), point( x, y ) ) then
    begin //next
      if CurrentLogIndex < JournalLog.LogFileList.count - 1 then
      begin
        CurrentLogIndex := CurrentLogIndex + 1;
        ShowText;
      end;
    end
    else if ptinRect( rect( 746, 575, 786, 596 ), point( x, y ) ) then
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
{  WrapperBltFast( lpDDSBack, 297,571,DXBack,rect(297,571,798,593),DDBLTFAST_WAIT);

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

  lpDDSFront.Flip(nil, DDFLIP_WAIT);
  WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect(0, 0, 800, 600), DDBLTFAST_WAIT);
  }
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //FormMouseMove

procedure TJournal.MouseDown( Sender : TAniview; Button : TMouseButton; Shift : TShiftState; X, Y, GridX, GridY : integer );
begin

end; //MouseDown

procedure TJournal.MouseMove( Sender : TAniview; Shift : TShiftState; X, Y, GridX, GridY : integer );
begin

end; //MouseMove

procedure TJournal.MouseUp( Sender : TAniview; Button : TMouseButton; Shift : TShiftState; X, Y, GridX, GridY : integer );
begin

end;


procedure TJournal.Release;
const
  FailName : string = 'TJournal.release';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
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
  PicName : string;
  jpg : TJPEGImage;

const
  FailName : string = 'TJournal.showtext';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if CurrentLogIndex > StartLogIndex then
      StartLogIndex := CurrentLogIndex;
  //clear screen
    WrapperBltFast( lpDDSBack, 0, 0, DXBack, Rect( 0, 0, 800, 600 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
  //Plot buttons
  //pText.PlotText('Previous',300,570,240);
  //pText.PlotText('Next',450,570,240);
  //pText.PlotText('Exit',740,570,240);

  //Show Latest Log
    LogText := JournalLog.ReadLogByIndex( CurrentLogIndex );

    BM := TBitmap.create;
  //get the pic, if it exists
    if ( CurrentLogIndex >= 0 ) and ( CurrentLogIndex < JournalLog.LogFileList.count ) then
    begin
      if FileExists( JournalLog.LogDirectory + ChangeFileExt( JournalLog.LogFileList.strings[ CurrentLogIndex ], '.bmp' ) ) then
      begin
        PicName := JournalLog.LogDirectory + ChangeFileExt( JournalLog.LogFileList.strings[ CurrentLogIndex ], '.bmp' );
        if LogText <> '' then
          PicXY := point( 20, 20 )
        else
          PicXY := point( 0, 0 );
        BM.LoadFromFile( PicName );
        DXPic := DDGetImage( lpDD, BM, $00FFFF00, False );
        PicWidth := BM.width;
        PicHeight := BM.height;
        WrapperBltFast( lpDDSBack, PicXY.X, PicXY.Y, DXPic, Rect( 0, 0, BM.width, BM.Height ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        if LogText <> '' then
          pText.PlotTextBlockAroundBox( Logtext, 50, 750, 50, 240, rect( PicXY.X, PicXY.Y, PicXY.X + PicWidth + 20, PicXY.Y + PicHeight + 20 ) );
      end
      else if FileExists( JournalLog.LogDirectory + ChangeFileExt( JournalLog.LogFileList.strings[ CurrentLogIndex ], '.jpg' ) ) then
      begin
        PicName := JournalLog.LogDirectory + ChangeFileExt( JournalLog.LogFileList.strings[ CurrentLogIndex ], '.jpg' );
        if LogText <> '' then
          PicXY := point( 20, 20 )
        else
          PicXY := point( 0, 0 );

        jpg := TJPEGImage.create;
        jpg.LoadFromFile( picname );

        BM.width := jpg.width;
        BM.Height := jpg.height;
        Bm.Canvas.Draw( 0, 0, jpg );

        jpg.Free;

        DXPic := DDGetImage( lpDD, BM, $00FFFF00, False );
        PicWidth := BM.width;
        PicHeight := BM.height;
        WrapperBltFast( lpDDSBack, PicXY.X, PicXY.Y, DXPic, Rect( 0, 0, BM.width, BM.Height ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        if LogText <> '' then
          pText.PlotTextBlockAroundBox( Logtext, 50, 750, 50, 240, rect( PicXY.X, PicXY.Y, PicXY.X + PicWidth + 20, PicXY.Y + PicHeight + 20 ) );
      end
      else if LogText <> '' then
        pText.PlotTextBlock( Logtext, 50, 750, 50, 240 );
    end
    else
      pText.PlotTextBlock( Logtext, 50, 750, 50, 240 );

    if not NoPageNumbers then
      pText.plotText( txtMessage[ 0 ] + intToStr( CurrentLogIndex + 1 ) + txtMessage[ 1 ] + IntToStr( JournalLog.LogFileList.count ), 81, 575, 255 );
    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;
    BM.Free;
    DXPic := nil;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //ShowText


end.

