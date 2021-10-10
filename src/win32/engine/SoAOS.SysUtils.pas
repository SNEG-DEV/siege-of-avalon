unit SoAOS.SysUtils;
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

  Description: SoAOS generic system

  Requires: Delphi 10.3.3 or later

  Revision History:
  - 1 Jan 2020 - SN: Initial Commit to Git
  see git repo afterwards

*)

interface

  function IsRunningUnderWINE(out WineVer: string): Boolean;
  function IsMFAvailable: Boolean;

implementation

uses
  Winapi.Windows;

function IsRunningUnderWINE(out WineVer: string): boolean;
var
  dll: HMODULE;
  get_WINE_version: function: PAnsiChar;
begin
  Result := False;
  dll := LoadLibrary('ntdll.dll');
  if dll<>0 then
  begin
    @get_WINE_version := GetProcAddress(dll, 'wine_get_version');
    if Assigned(@get_WINE_version) then
    begin
      WineVer := get_WINE_version();
      Result := True;
    end;
    FreeLibrary(dll);
  end;
end;

function IsMFAvailable: Boolean;
var
  dll: HMODULE;
begin
  Result := False;
  dll := LoadLibrary('mfplay.dll');
  if dll<>0 then
  begin
    Result := True;
    FreeLibrary(dll);
  end;
end;

end.
