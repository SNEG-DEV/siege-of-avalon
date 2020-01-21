unit SoAOS.Intrface.Dialogs;
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

  Description: Adding offset handling to current TDisplay used by centered menues

  Requires: Delphi 10.3.3 or later

  Revision History:
  - 21 Jan 2020 - SN: Initial Commit to Git
  see git repo afterwards

*)

interface

uses
  System.Types,

  Display;

type
  TDialog = class( TDisplay )
  private
    function GetOffset: TPoint;
  public
    DlgWidth: Integer;
    DlgHeight: Integer;
    property Offset: TPoint read GetOffset;
  end;

implementation

uses
  AniDemo;

{ TDialog }

function TDialog.GetOffset: TPoint;
begin
  Result := TPoint.Create((ScreenMetrics.ScreenWidth - DlgWidth) div 2, (ScreenMetrics.ScreenHeight - DlgHeight) div 2);
end;

end.
