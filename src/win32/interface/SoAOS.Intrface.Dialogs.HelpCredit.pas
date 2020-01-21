unit SoAOS.Intrface.Dialogs.HelpCredit;
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

  Description: Help, Credits and "Death" Dialog - shows the bmp file given - was ShowGraphic.pas - a lot more clean-up is coming

  Requires: Delphi 10.3.3 or later

  Revision History:
  - 13 Jul 2003 - DL: Initial Upload to CVS
  - 10 Mar 2019 - SN: Forked on GitHub
  see git repo afterwards

*)

interface

uses
{$IFDEF DirectX}
  DirectX,
  DXUtil,
{$ENDIF}
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  System.Types,
  Vcl.Controls,
  Vcl.Forms,
  SoAOS.Intrface.Dialogs,
  Anigrp30,
  Music;
  
type
  TShowGraphic = class( TDialog )
  private
    DXBack : IDirectDrawSurface;
    procedure FormMouseDown( Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer );
  protected
    procedure MouseDown( Sender : TAniview; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
    procedure KeyDown( Sender : TObject; var key : Word; Shift : TShiftState ); override;
  public
    frmMain : TForm;
    FileName : string;
    MusicFileName : AnsiString;
    pMusic : TMusic;
    procedure Init; override;
    procedure Release; override;
  end;

implementation

uses
  SoAOS.Types,
  SoAOS.Graphics.Draw,
  AniDemo,
  Resource,
  Engine,
  LogFile;

{ TShowGraphic }

procedure TShowGraphic.Init;
var
  pr : TRect;
const
  FailName : string = 'TShowGraphic.init';
begin
  Log.DebugLog(FailName);
  try
    if Loaded then
      Exit;
    inherited;
    MouseCursor.Cleanup;
    pr := Rect( 0, 0, ResWidth, ResHeight );
    lpDDSBack.BltFast( 0, 0, lpDDSFront, @pr, DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;

    frmMain.OnMouseDown := FormMouseDown;

    DXBack := SoAOS_DX_LoadBMP( InterfacePath + FileName, cInvisColor, DlgWidth, DlgHeight );
    pr := Rect( 0, 0, DlgWidth, DlgHeight );
    lpDDSBack.BltFast( Offset.X, Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );

    SoAOS_DX_BltFront;

    if assigned( pMusic ) then
    begin
      if TFile.Exists( SoundPath + 'Theme\' + MusicFileName ) then
      begin
        pMusic.OpenThisSong( AnsiString ( SoundPath + 'Theme\' + MusicFileName ) );
        pMusic.PlayThisSong;
        pMusic.SetSongVolume( 99 );
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //Init


procedure TShowGraphic.KeyDown( Sender : TObject; var key : Word; Shift : TShiftState );
const
  FailName : string = 'TShowGraphic.keydown';
begin
  Log.DebugLog(FailName);
  try
    if assigned( pMusic ) then
      pMusic.PauseThisSong;
    Close;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //keyDown

procedure TShowGraphic.MouseDown( Sender : TAniview; Button : TMouseButton; Shift : TShiftState; X, Y, GridX, GridY : integer );
const
  FailName : string = 'TShowGraphic.mousedown';
begin
  Log.DebugLog(FailName);
  try
    if assigned( pMusic ) then
      pMusic.PauseThisSong;
    Close;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //MouseDown

procedure TShowGraphic.FormMouseDown( Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer );
const
  FailName : string = 'TShowGraphic.formmousedown';
begin
  Log.DebugLog(FailName);
  try
    if assigned( pMusic ) then
      pMusic.PauseThisSong;
    Close;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //FormMouseDown

procedure TShowGraphic.Release;
const
  FailName : string = 'TShowGraphic.release';
begin
  Log.DebugLog(FailName);
  try
    DXBack := nil;
    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

end.
