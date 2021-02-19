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
//  Winapi.DirectDraw,
  DirectX,
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  System.Types,
  Vcl.Controls,
  Vcl.Forms,
  SoAOS.Intrface.Dialogs,
  SoAOS.Animation,
  Music;
  
type
  TShowGraphic = class( TDialog )
  strict private
    DXBack : IDirectDrawSurface;
    pMusic : TMusic;
    FMusicFileName : string; // was AnsiString;
    FBMPFileName : string;
    procedure FormMouseDown( Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer );
    procedure SetMusicFileName(const Value: string);
    procedure SetBMPFileName(const Value: string);
  protected
    procedure MouseDown( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer ); override;
    procedure KeyDown( Sender : TObject; var key : Word; Shift : TShiftState ); override;
  public
    frmMain : TForm;
    procedure Init; override;
    procedure Release; override;
    property BMPFileName : string write SetBMPFileName;
    property MusicFileName : string write SetMusicFileName;
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
begin
  if Loaded then
    Exit;
  inherited;
  MouseCursor.Cleanup;
  pr := Rect( 0, 0, ResWidth, ResHeight );
  lpDDSBack.BltFast( 0, 0, lpDDSFront, @pr, DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
  MouseCursor.PlotDirty := false;

  frmMain.OnMouseDown := FormMouseDown; //TODO: Use Callback

  DXBack := SoAOS_DX_LoadBMP( InterfaceLanguagePath + FBMPFileName, cInvisColor, DlgWidth, DlgHeight );
  pr := Rect( 0, 0, DlgWidth, DlgHeight );
  lpDDSBack.BltFast( Offset.X, Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );

  SoAOS_DX_BltFront;

  if Assigned( pMusic ) then
  begin
    if TFile.Exists( SoundPath + 'Theme\' + FMusicFileName ) then
    begin
      pMusic.OpenThisSong( AnsiString ( SoundPath + 'Theme\' + FMusicFileName ) );
      pMusic.PlayThisSong;
      pMusic.SetSongVolume( MasterMusicVolume );
    end;
  end;
end; //Init

procedure TShowGraphic.KeyDown( Sender : TObject; var key : Word; Shift : TShiftState );
begin
  if Assigned( pMusic ) then
    pMusic.PauseThisSong;
  Close;
end; //keyDown

procedure TShowGraphic.MouseDown( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer );
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
begin
  DXBack := nil;
  inherited;
end;

procedure TShowGraphic.SetBMPFileName(const Value: string);
begin
  FBMPFileName := Value;
end;

procedure TShowGraphic.SetMusicFileName(const Value: string);
begin
  FMusicFileName := Value;
  if Value='' then
    pMusic := nil
  else
    pMusic := MusicLib;
end;

end.
