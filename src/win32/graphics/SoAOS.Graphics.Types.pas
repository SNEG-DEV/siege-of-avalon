unit SoAOS.Graphics.Types;
(*
  Siege Of Avalon : Open Source Edition

  Portions created by Stefen Nyeland are
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

  Description: SoAOS Graphics types

  Requires: Delphi 10.3.3 or later

  Revision History:
  - 22 Apr 2019 - SN: Initial Commit to Git
  see git repo afterwards

*)
interface

uses
  System.Types;

type
  PBITPLANE = ^BITPLANE;
  BITPLANE = record
    bitsPtr : PBYTE;
    bitsWdh : DWORD;
    bitsHgh : DWORD;
    bitsFmt : DWORD;
    bitsPitch : DWORD;
    BaseX : integer;
    BaseY : integer;
  end;

implementation

end.
