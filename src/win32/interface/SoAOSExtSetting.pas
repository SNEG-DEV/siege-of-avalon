unit SoAOSExtSetting;
(*
  Siege Of Avalon : Open Source Edition

  Portions created by Steffen Nyeland are
  Copyright (C) 2020 - Steffen Nyeland.

  Contributor(s):
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

  Description: Game launcher settings

  Requires: Delphi 10.3.3 or later

  Revision History:
  - 10 Jan 2020 - SN: Initial Commit to Git
  see git repo afterwards

*)

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Imaging.pngimage;

const
  cNoLanguage = 'default';

type
  TfrmLaunchSetting = class(TForm)
    imgPage1: TImage;
    tmrScroll: TTimer;
    LinkLabel: TLinkLabel;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    lblLanguage: TStaticText;
    procedure FormCreate(Sender: TObject);
    procedure tmrScrollTimer(Sender: TObject);
    procedure TxtScrollLeft;
    procedure TxtScrollRight;
    procedure imgPage1Click(Sender: TObject);
    procedure Done(r: integer);
    procedure FormDestroy(Sender: TObject);
    procedure LinkLabelLinkClick(Sender: TObject; const Link: string;
      LinkType: TSysLinkType);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FLanguages: TStringList;
    FCurrentLanguage: string;
    FCurrentLanguageIdx: Integer;
    FScrollDirLeft: Boolean;
    FScrollText: string;
    FInterfacePath: string;

    Support720p: Boolean;
    Support1080p: Boolean;
  public
    class function Execute: TModalResult;
  end;

var
  frmLaunchSetting: TfrmLaunchSetting;

implementation

uses
  System.IOUtils,
  System.IniFiles,
  Winapi.ShellAPI;

{$R *.dfm}

function LoadResourceFontByID( ResourceID : Integer; ResType: PChar ) : Boolean;
var
  ResStream : TResourceStream;
  FontsCount : DWORD;
begin
  ResStream := TResourceStream.CreateFromID(hInstance, ResourceID, ResType);
  try
    Result := (AddFontMemResourceEx(ResStream.Memory, ResStream.Size, nil, @FontsCount) <> 0);
  finally
    ResStream.Free;
  end;
end;

procedure TfrmLaunchSetting.TxtScrollLeft;
begin
  Dec(FCurrentLanguageIdx);
  if FCurrentLanguageIdx=-1 then
    FCurrentLanguageIdx := FLanguages.Count-1;
  FCurrentLanguage := FLanguages[FCurrentLanguageIdx];
  FScrollText := FCurrentLanguage.PadLeft(30,' ');
  FScrollDirLeft := True;
  tmrScroll.Enabled := True;
end;

procedure TfrmLaunchSetting.TxtScrollRight;
begin
  Inc(FCurrentLanguageIdx);
  if FCurrentLanguageIdx=FLanguages.Count then
    FCurrentLanguageIdx := 0;
  FCurrentLanguage := FLanguages[FCurrentLanguageIdx];
  FScrollText := FCurrentLanguage.PadRight(30,' ');
  FScrollDirLeft := False;
  tmrScroll.Enabled := True;
end;

procedure TfrmLaunchSetting.Done(r: integer);
var
  INI: TIniFile;
  languagePath, fsrc, fdest: string;
  files: TArray<string>;
begin
  INI := TIniFile.Create(ChangeFileExt(Application.ExeName,'.ini'));
  try

    try
      if (FCurrentLanguage <> cNoLanguage) then
      begin
        if TDirectory.Exists(TPath.Combine(FInterfacePath, FCurrentLanguage)) then
          INI.WriteString('Settings', 'LanguagePath', FCurrentLanguage)
        else
        begin
          INI.WriteString('Settings', 'LanguagePath', '');
          FCurrentLanguage := cNoLanguage;
        end;
      end;
      INI.WriteInteger('Settings', 'ScreenResolution', r);
      INI.UpdateFile;
    except
      on EIniFileException do
      begin
        RaiseLastOsError;
      end;
    end;

    if FCurrentLanguage <> cNoLanguage then
    begin
      languagePath := IncludeTrailingPathDelimiter(TPath.Combine(FInterfacePath, FCurrentLanguage));
      files := TDirectory.GetFiles(languagePath);
      for fsrc in files do
      begin
        fdest := TPath.Combine( FInterfacePath, TPath.GetFileName( fsrc ) );
        TFile.Copy( fsrc, fdest, True);
      end;
      INI.WriteString( 'Settings', 'ItemDB', 'ArtLib/Resources/Database/'+FCurrentLanguage+'/Items.DB' );
//      INI.WriteString( 'Settings', 'XRefDB', 'ArtLib/Resources/Database/'+FCurrentLanguage+'/xref.db' );
      INI.WriteString( 'Settings', 'TitlesDB', 'ArtLib/Resources/Database/'+FCurrentLanguage+'/Title.db' );
    end;

  finally
    INI.Free;
  end;
  ModalResult := mrOk;
end;

class function TfrmLaunchSetting.Execute: TModalResult;
var
  F: TfrmLaunchSetting;
begin
  F := TfrmLaunchSetting.Create(nil);
  try
    Result := F.ShowModal;
  finally
    F.Free;
  end;
end;

procedure TfrmLaunchSetting.FormCreate(Sender: TObject);
var
  INI: TIniFile;
  lInterfacePath: string;
  dir: string;
begin
  FLanguages := TStringList.Create(dupIgnore, True, False);
  if LoadResourceFontByID(1, RT_FONT) then
    Self.Font.Name := 'BlackChancery';
  SendMessageTimeout(HWND_BROADCAST, WM_FONTCHANGE, 0, 0, SMTO_NORMAL, 100, nil);
  Application.ProcessMessages;
  INI := TIniFile.Create(ChangeFileExt(Application.ExeName,'.ini'));
  try
    FCurrentLanguage := INI.ReadString('Settings', 'LanguagePath', cNoLanguage);
    if FCurrentLanguage='' then
      FCurrentLanguage := cNoLanguage;
    lInterfacePath := INI.ReadString('Settings', 'Interface', 'Interface');
  finally
    INI.Free;
  end;
  FInterfacePath := IncludeTrailingPathDelimiter(TPath.GetFullPath(lInterfacePath));
  for dir in TDirectory.GetDirectories(FInterfacePath) do
    FLanguages.Add(Copy(dir, dir.LastIndexOf(PathDelim)+2));
  if FLanguages.Count=0 then // no languages - other than english
    FLanguages.Add(cNoLanguage);
  FCurrentLanguageIdx := FLanguages.IndexOf(FCurrentLanguage);
  if FCurrentLanguageIdx=-1 then
    FCurrentLanguageIdx := 0;
  for var i: integer := 0 to screen.MonitorCount-1 do
  begin
    var mon: TMonitor := screen.Monitors[i];
    if mon.Primary then
    begin
      Support1080p := (mon.Height >= 1080);
      Support720p := (mon.Height >= 720);
    end;
  end;
end;

procedure TfrmLaunchSetting.FormDestroy(Sender: TObject);
begin
  FLanguages.Free;
end;

procedure TfrmLaunchSetting.FormShow(Sender: TObject);
begin
  lblLanguage.Font.Name := 'BlackChancery';
  lblLanguage.Caption := FLanguages[FCurrentLanguageIdx];
 // lblLanguageCaption.Font.Name := 'BlackChancery';
end;

procedure TfrmLaunchSetting.imgPage1Click(Sender: TObject);
var
  lInterfacePath: string;
begin
  lInterfacePath := FInterfacePath;
  if FCurrentLanguage <> cNoLanguage then
    lInterfacePath := IncludeTrailingPathDelimiter(TPath.Combine(FInterfacePath, FCurrentLanguage));
  if Rect(321,283,342,304).Contains(imgPage1.ScreenToClient(Mouse.cursorpos)) then
    TxtScrollLeft;
  if Rect(455,283,476,304).Contains(imgPage1.ScreenToClient(Mouse.cursorpos)) then
    TxtScrollRight;
  if Rect(100,327,186,380).Contains(imgPage1.ScreenToClient(Mouse.cursorpos)) then
    Done(600);
  if Rect(241,327,327,380).Contains(imgPage1.ScreenToClient(Mouse.cursorpos)) then
    if Support720p then Done(720)
    else ShowMessage('The current resolution of primary monitor does not support 720p/HD.');
  if Rect(373,327,459,380).Contains(imgPage1.ScreenToClient(Mouse.cursorpos)) then
    if Support1080p then Done(1080)
    else ShowMessage('The current resolution of primary monitor does not support 1080p/FullHD.');
end;

procedure TfrmLaunchSetting.LinkLabelLinkClick(Sender: TObject;
  const Link: string; LinkType: TSysLinkType);
begin
  ShellExecute(0, nil, PChar(Link), nil, nil, 1);
end;

procedure TfrmLaunchSetting.tmrScrollTimer(Sender: TObject);
begin
  //TODO: Redo this - since DX has partly been initialized controls do not behave correctly
  if FScrollDirLeft then
  begin
    if FScrollText[2]<>' ' then
      tmrScroll.Enabled := False;
    FScrollText := copy(FScrollText, 2);
    lblLanguage.Caption := copy(FScrollText, 1, Length(FCurrentLanguage));
  end
  else
  begin
    if FScrollText[Length(FScrollText)-1]<>' ' then
      tmrScroll.Enabled := False;
    FScrollText := copy(FScrollText, 1, Length(FScrollText)-1);
    lblLanguage.Caption := copy(FScrollText, Length(FScrollText)-Length(FCurrentLanguage));
  end;
end;

end.
