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
  cNoLanguage = 'Default';

type
  TfrmLaunchSetting = class(TForm)
    imgPage1: TImage;
    tmrScroll: TTimer;
    lblLanguage: TStaticText;
    StaticText3: TStaticText;
    lblMonitor: TStaticText;
    lblResolution: TStaticText;
    imgFullscreen: TImage;
    procedure FormCreate(Sender: TObject);
    procedure tmrScrollTimer(Sender: TObject);
    procedure imgPage1Click(Sender: TObject);
    procedure Done(r: integer; windowed: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StaticText3Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure imgFullscreenClick(Sender: TObject);
  private
    { Private declarations }
    FLanguages: TStringList;
    FCurrentLanguage: string;
    FCurrentLanguageIdx: Integer;

    FMonitors: TStringList;
    FCurrentDevice: string;
    FCurrentDeviceIdx: Integer;

    FResolutions: TStringList;
    FCurrentResolution: string;
    FCurrentResolutionIdx: Integer;

    FScrollDirLeft: Boolean;
    FScrollText: string;
    FScrollFullText: string;
    FScrollControl: TStaticText;

    FInterfacePath: string;

    monitorCnt: Integer;

    function AppHookFunc(var Message : TMessage) : Boolean;
    procedure SetResolutionSupport(lpszDeviceName: LPCWSTR);
    function ScrollText(const goLeft: boolean; var idx: integer; const list: TStringList; const control: TStaticText): string;
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

function TfrmLaunchSetting.AppHookFunc(var Message: TMessage): Boolean;
begin
  Result := False;
  if Message.Msg = WM_SYSCOMMAND then
  begin
    PostMessage(Handle, WM_CLOSE, 0, 0);
    Result := True;
  end;
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
      INI.WriteString('Settings', 'DeviceName', FCurrentDevice);
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
  prim: Integer;

  devName: string;
  DisplayDevice: TDisplayDevice;
  iDevNum: DWORD;
  langStr: string;
begin
  Application.HookMainWindow(AppHookFunc);

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
    imgFullscreen.Visible := not INI.ReadBool('Settings', 'Windowed', False);
    FCurrentDevice := INI.ReadString('Settings', 'DeviceName', '');
    FCurrentResolution := INI.ReadString('Settings', 'ScreenResolution', '600');
  finally
    INI.Free;
  end;

  FLanguages := TStringList.Create(dupIgnore, True, False);
  FInterfacePath := IncludeTrailingPathDelimiter(TPath.GetFullPath(lInterfacePath));
  for dir in TDirectory.GetDirectories(FInterfacePath) do
  begin
    langStr := AnsiLowerCase(Copy(dir, dir.LastIndexOf(PathDelim)+2));
    FLanguages.Add(AnsiUpperCase(langStr[1])+copy(LangStr, 2));
  end;
  if FLanguages.Count=0 then // no languages - other than english
    FLanguages.Add(cNoLanguage);
  FCurrentLanguageIdx := FLanguages.IndexOf(FCurrentLanguage);
  if FCurrentLanguageIdx=-1 then
    FCurrentLanguageIdx := 0;

  FMonitors := TStringList.Create();
  FMonitors.NameValueSeparator := '=';
  monitorCnt := Screen.MonitorCount;
  prim := 0;
// DeviceDrivers
  DisplayDevice.cb := SizeOf(DisplayDevice);
  for iDevNum := 0 to monitorCnt-1 do
    if EnumDisplayDevices(NIL, iDevNum, DisplayDevice, 0) then
    begin
      devName := displayDevice.DeviceName;
      EnumDisplayDevices(PChar(devName), 0, displayDevice, 0);
      FMonitors.Add('Display '+(iDevNum+1).ToString+' - '+string(DisplayDevice.DeviceString) + '=' + devName);
      if devName=FCurrentDevice then
        prim := iDevNum;
    end;
  FCurrentDeviceIdx := prim;
  FCurrentDevice := FMonitors.ValueFromIndex[FCurrentDeviceIdx];

  FResolutions := TStringList.Create(dupIgnore, True, False);
  SetResolutionSupport(PWideChar(FCurrentDevice));
end;

procedure TfrmLaunchSetting.FormDestroy(Sender: TObject);
begin
  FLanguages.Free;
  FMonitors.Free;
  FResolutions.Free;
  Application.UnHookMainWindow(AppHookFunc);
end;

procedure TfrmLaunchSetting.FormShow(Sender: TObject);
begin
  lblLanguage.Font.Name := 'BlackChancery';
  lblLanguage.Caption := FLanguages[FCurrentLanguageIdx];
  lblResolution.Font.Name := 'BlackChancery';
  lblResolution.Caption := FResolutions.KeyNames[FCurrentResolutionIdx];
  lblMonitor.Font.Name := 'BlackChancery';
  lblMonitor.Caption := FMonitors.KeyNames[FCurrentDeviceIdx];
end;

procedure TfrmLaunchSetting.imgFullscreenClick(Sender: TObject);
begin
  imgFullscreen.Visible := not imgFullscreen.Visible;
  SetResolutionSupport(PWideChar(FCurrentDevice));
end;

procedure TfrmLaunchSetting.imgPage1Click(Sender: TObject);
var
  lInterfacePath: string;
begin
  lInterfacePath := FInterfacePath;
  if FCurrentLanguage <> cNoLanguage then
    lInterfacePath := IncludeTrailingPathDelimiter(TPath.Combine(FInterfacePath, FCurrentLanguage));
  // Language
  if Rect(199,333,214,348).Contains(imgPage1.ScreenToClient(Mouse.cursorpos)) then
    FCurrentLanguage := ScrollText(True, FCurrentLanguageIdx, FLanguages, lblLanguage);
  if Rect(300,333,315,348).Contains(imgPage1.ScreenToClient(Mouse.cursorpos)) then
    FCurrentLanguage := ScrollText(False, FCurrentLanguageIdx, FLanguages, lblLanguage);

  // Resolution
  if Rect(199,267,214,282).Contains(imgPage1.ScreenToClient(Mouse.cursorpos)) then
    FCurrentResolution := ScrollText(True, FCurrentResolutionIdx, FResolutions, lblResolution);
  if Rect(400,267,415,282).Contains(imgPage1.ScreenToClient(Mouse.cursorpos)) then
    FCurrentResolution := ScrollText(False, FCurrentResolutionIdx, FResolutions, lblResolution);

  // Monitor
  if Rect(199,234,214,249).Contains(imgPage1.ScreenToClient(Mouse.cursorpos)) then
  begin
    FCurrentDevice := ScrollText(True, FCurrentDeviceIdx, FMonitors, lblMonitor);
    SetResolutionSupport(PWideChar(FCurrentDevice));
  end;
  if Rect(493,234,508,249).Contains(imgPage1.ScreenToClient(Mouse.cursorpos)) then
  begin
    FCurrentDevice := ScrollText(False, FCurrentDeviceIdx, FMonitors, lblMonitor);
    SetResolutionSupport(PWideChar(FCurrentDevice));
  end;

  // Fullscreen
  if Rect(219,295,244,320).Contains(imgPage1.ScreenToClient(Mouse.cursorpos)) then
  begin
    imgFullscreen.Visible := not imgFullscreen.Visible;
    SetResolutionSupport(PWideChar(FCurrentDevice));
  end;

  // Let's Play
  if Rect(410,325,522,387).Contains(imgPage1.ScreenToClient(Mouse.cursorpos)) then
  begin
    Done(FCurrentResolution.ToInteger, not imgFullscreen.Visible);
  end;
end;

function TfrmLaunchSetting.ScrollText(const goLeft: boolean; var idx: integer;
  const list: TStringList; const control: TStaticText): string;
begin
  if goLeft then
  begin
    Inc(idx);
    if idx = list.Count then
      idx := 0;
  end
  else
  begin
    Dec(idx);
    if idx = -1 then
      idx := list.Count-1;
  end;

  FScrollFullText := list[idx];
  if Pos('=', FScrollFullText)>0 then
  begin
    Result := list.ValueFromIndex[idx];
    FScrollFullText := list.KeyNames[idx];
  end
  else
    Result := FScrollFullText;

  if goLeft then
    FScrollText := FScrollFullText.PadLeft(FScrollFullText.Length*3 ,' ')
  else
    FScrollText := FScrollFullText.PadRight(FScrollFullText.Length*3,' ');

  FScrollDirLeft := goLeft;
  FScrollControl := control;
  tmrScroll.Enabled := True;
end;

procedure TfrmLaunchSetting.SetResolutionSupport(lpszDeviceName: LPCWSTR);
var
  iModeNum: DWORD;
  lpDevMode: TDeviceMode;
  i: Integer;
begin
  FResolutions.Clear;
  iModeNum := 0;
  if lpszDeviceName='' then
    lpszDeviceName := nil;
  while EnumDisplaySettings(lpszDeviceName, iModeNum, lpDevMode) do
  begin
    if imgFullscreen.Visible then // exact resolution needed.
    begin
      if (lpDevMode.dmPelsWidth = 800) and (lpDevMode.dmPelsHeight = 600) then
        FResolutions.Add('800 x 600 (Original)=600');
      if (lpDevMode.dmPelsWidth = 1280) and (lpDevMode.dmPelsHeight = 720) then
        FResolutions.Add('1280 x 720 (HD)=720');
      if (lpDevMode.dmPelsWidth = 1920) and (lpDevMode.dmPelsHeight = 1080) then
        FResolutions.Add('1920 x 1080 (FullHD)=1080');
      Inc( iModeNum );
    end
    else
    begin
      if (lpDevMode.dmPelsWidth >= 800) and (lpDevMode.dmPelsHeight >= 600) then
        FResolutions.Add('800 x 600 (Original)=600');
      if (lpDevMode.dmPelsWidth >= 1280) and (lpDevMode.dmPelsHeight >= 720) then
        FResolutions.Add('1280 x 720 (HD)=720');
      if (lpDevMode.dmPelsWidth >= 1920) and (lpDevMode.dmPelsHeight >= 1080) then
        FResolutions.Add('1920 x 1080 (FullHD)=1080');
      Inc( iModeNum );
    end;
  end;

  FCurrentResolutionIdx := 0;
  for i := 0 to FResolutions.Count-1 do
  begin
    if FResolutions.ValueFromIndex[i]=FCurrentResolution then
      FCurrentResolutionIdx := i;
  end;

  FCurrentResolution := FResolutions.ValueFromIndex[FCurrentResolutionIdx];
  lblResolution.Caption := FResolutions.KeyNames[FCurrentResolutionIdx];
end;

procedure TfrmLaunchSetting.StaticText3Click(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmLaunchSetting.tmrScrollTimer(Sender: TObject);
begin
  //TODO: Redo this - since DX has partly been initialized controls do not behave correctly
  if FScrollDirLeft then
  begin
    if FScrollText[2]<>' ' then
      tmrScroll.Enabled := False;
    FScrollText := copy(FScrollText, 2);
    FScrollControl.Caption := copy(FScrollText, 1, Length(FScrollFullText));
  end
  else
  begin
    if FScrollText[Length(FScrollText)-1]<>' ' then
      tmrScroll.Enabled := False;
    FScrollText := copy(FScrollText, 1, Length(FScrollText)-1);
    FScrollControl.Caption := copy(FScrollText, Length(FScrollText)-Length(FScrollFullText));
  end;
end;

end.
