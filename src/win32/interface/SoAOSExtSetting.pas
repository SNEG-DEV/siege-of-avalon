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
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    lblLanguage: TStaticText;
    StaticText3: TStaticText;
    cmbMonitors: TComboBox;
    stMonitor: TStaticText;
    imgSD: TImage;
    imgHD: TImage;
    imgFullHD: TImage;
    WindowedMode: TCheckBox;
    StaticText4: TStaticText;
    procedure FormCreate(Sender: TObject);
    procedure tmrScrollTimer(Sender: TObject);
    procedure TxtScrollLeft;
    procedure TxtScrollRight;
    procedure imgPage1Click(Sender: TObject);
    procedure Done(r: integer; windowed: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StaticText3Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure cmbMonitorsChange(Sender: TObject);
    procedure StaticText4Click(Sender: TObject);
    procedure imgSDClick(Sender: TObject);
    procedure imgHDClick(Sender: TObject);
    procedure imgFullHDClick(Sender: TObject);
  private
    { Private declarations }
    FLanguages: TStringList;
    FCurrentLanguage: string;
    FCurrentLanguageIdx: Integer;
    FScrollDirLeft: Boolean;
    FScrollText: string;
    FInterfacePath: string;

    monitorCnt: Integer;
    devices: TStringList;
    CurrentDeviceName: string;

    function AppHookFunc(var Message : TMessage) : Boolean;
    procedure SetResolutionSupport(lpszDeviceName: LPCWSTR);
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

function TfrmLaunchSetting.AppHookFunc(var Message: TMessage): Boolean;
begin
  Result := False;
  if Message.Msg = WM_SYSCOMMAND then
  begin
    PostMessage(Handle, WM_CLOSE, 0, 0);
    Result := True;
  end;
end;

procedure TfrmLaunchSetting.cmbMonitorsChange(Sender: TObject);
begin
  CurrentDeviceName := devices.ValueFromIndex[cmbMonitors.ItemIndex];
  SetResolutionSupport(PWideChar(CurrentDeviceName));
end;

procedure TfrmLaunchSetting.Done(r: integer; windowed: Boolean);
var
  INI: TIniFile;
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
      INI.WriteBool('Settings', 'Windowed', windowed);
      INI.WriteString('Settings', 'DeviceName', CurrentDeviceName);
      INI.UpdateFile;
    except
      on EIniFileException do
      begin
        RaiseLastOsError;
      end;
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

procedure TfrmLaunchSetting.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := True;
  if ModalResult=mrNone then
    ModalResult := mrCancel;
end;

procedure TfrmLaunchSetting.FormCreate(Sender: TObject);
var
  INI: TIniFile;
  lInterfacePath: string;
  dir: string;
  I, prim: Integer;

  devName: string;
  DisplayDevice: TDisplayDevice;
  iDevNum: DWORD;
begin
  Application.HookMainWindow(AppHookFunc);

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
    WindowedMode.Checked := INI.ReadBool('Settings', 'Windowed', False);
    CurrentDeviceName := INI.ReadString('Settings', 'DeviceName', '');
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

  monitorCnt := Screen.MonitorCount;
  cmbMonitors.Visible := monitorCnt > 1;
  stMonitor.Visible := cmbMonitors.Visible;

  if monitorCnt > 1 then
  begin
    prim := 0;
// DeviceDrivers
    devices := TStringList.Create;
    devices.NameValueSeparator := '=';

    DisplayDevice.cb := SizeOf(DisplayDevice);
    for iDevNum := 0 to monitorCnt-1 do
      if EnumDisplayDevices(NIL, iDevNum, DisplayDevice, 0) then
      begin
        devName := displayDevice.DeviceName;
        EnumDisplayDevices(PChar(devName), 0, displayDevice, 0);
        devices.Add('Display '+(iDevNum+1).ToString+' - '+string(DisplayDevice.DeviceString) + '=' + devName);
        if devName=CurrentDeviceName then
          prim := iDevNum;
      end;

    for I := 0 to devices.Count-1 do
      cmbMonitors.Items.Add(devices.KeyNames[i]);

    cmbMonitors.ItemIndex := prim;
    CurrentDeviceName := devices.ValueFromIndex[cmbMonitors.ItemIndex];
  end
  else
    CurrentDeviceName := '';

  SetResolutionSupport(PWideChar(CurrentDeviceName));
end;

procedure TfrmLaunchSetting.FormDestroy(Sender: TObject);
begin
  FLanguages.Free;
  devices.Free;
  Application.UnHookMainWindow(AppHookFunc);
end;

procedure TfrmLaunchSetting.FormShow(Sender: TObject);
begin
  lblLanguage.Font.Name := 'BlackChancery';
  lblLanguage.Caption := FLanguages[FCurrentLanguageIdx];
 // lblLanguageCaption.Font.Name := 'BlackChancery';
end;

procedure TfrmLaunchSetting.imgFullHDClick(Sender: TObject);
begin
  Done(1080, WindowedMode.Checked);
end;

procedure TfrmLaunchSetting.imgHDClick(Sender: TObject);
begin
  Done(720, WindowedMode.Checked);
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
end;

procedure TfrmLaunchSetting.imgSDClick(Sender: TObject);
begin
  Done(600, WindowedMode.Checked);
end;

procedure TfrmLaunchSetting.SetResolutionSupport(lpszDeviceName: LPCWSTR);
var
  iModeNum: DWORD;
  lpDevMode: TDeviceMode;
begin
  imgSD.Visible := False;
  imgHD.Visible := False;
  imgFullHD.Visible := False;
  iModeNum := 0;
  if lpszDeviceName='' then
    lpszDeviceName := nil;
  while EnumDisplaySettings(lpszDeviceName, iModeNum, lpDevMode) do
  begin
    if (lpDevMode.dmPelsWidth = 800) and (lpDevMode.dmPelsHeight = 600) then
      imgSD.Visible := True;
    if (lpDevMode.dmPelsWidth = 1280) and (lpDevMode.dmPelsHeight = 720) then
      imgHD.Visible := True;
    if (lpDevMode.dmPelsWidth = 1920) and (lpDevMode.dmPelsHeight = 1080) then
      imgFullHD.Visible := True;
    Inc( iModeNum );
  end;
end;

procedure TfrmLaunchSetting.StaticText3Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmLaunchSetting.StaticText4Click(Sender: TObject);
begin
  WindowedMode.Checked := not WindowedMode.Checked;
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
