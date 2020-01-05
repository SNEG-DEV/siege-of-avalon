unit SoAOS.StrUtils;
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

  Description: SoAOS specific string helpers - token parsing/handling

  Requires: Delphi 10.3.3 or later

  Revision History:
  - 1 Jan 2020 - SN: Initial Commit to Git
  see git repo afterwards

*)

interface

uses
  System.Classes,
  System.SysUtils;

type
  TTokenString = type String;

  //TODO: find better names when scope of use is clearer
  TTokenString_Helper = record helper for TTokenString
    function PipeToken(Index: integer): string; inline; // | delimiter
    function PipeTokenTS(Index: integer): TTokenString; inline; // | delimiter
    function CommaToken(Index: integer): string; inline;  // , delimiter
    function RandomToken: string; inline; // , delimiter
    function SemiToken(Index: integer): string; inline;  // ; delimiter
    function ColonToken(Index: integer): string; inline;  // : delimiter
    function gtToken(Index: integer): string; inline;  // > delimiter
    function ltToken(Index: integer): string; inline;  // < delimiter
    function TokenCount(Seperator: Char): integer; inline;
  end;

implementation

{ TTokenString_Helper }

function TTokenString_Helper.PipeToken(Index: integer): string;
var
  arr: TArray<string>;
begin
  Result := '';
  arr := string(Self).Split(['|']);
  if Length(arr)-1>=Index then
    Result := arr[Index];
end;

function TTokenString_Helper.PipeTokenTS(Index: integer): TTokenString;
var
  arr: TArray<string>;
begin
  Result := '';
  arr := string(Self).Split(['|']);
  if Length(arr)-1>=Index then
    Result := arr[Index];
end;

function TTokenString_Helper.ColonToken(Index: integer): string;
var
  arr: TArray<string>;
begin
  Result := '';
  arr := string(Self).Split([':']);
  if Length(arr)-1>=Index then
    Result := arr[Index];
end;

function TTokenString_Helper.CommaToken(Index: integer): string;
var
  arr: TArray<string>;
begin
  Result := '';
  arr := string(Self).Split([',']);
  if Length(arr)-1>=Index then
    Result := arr[Index];
end;

function TTokenString_Helper.gtToken(Index: integer): string;
var
  arr: TArray<string>;
begin
  Result := '';
  arr := string(Self).Split(['>']);
  if Length(arr)-1>=Index then
    Result := arr[Index];
end;

function TTokenString_Helper.ltToken(Index: integer): string;
var
  arr: TArray<string>;
begin
  Result := '';
  arr := string(Self).Split(['<']);
  if Length(arr)-1>=Index then
    Result := arr[Index];
end;

function TTokenString_Helper.RandomToken: string;
var
  arr: TArray<string>;
begin
  arr := string(Self).Split([',']);
  Result := arr[Random(Length(arr))];
end;

function TTokenString_Helper.SemiToken(Index: integer): string;
var
  arr: TArray<string>;
begin
  Result := '';
  arr := string(Self).Split([';']);
  if Length(arr)-1>=Index then
    Result := arr[Index];
end;

function TTokenString_Helper.TokenCount(Seperator: Char): integer;
var
  arr: TArray<string>;
begin
  arr := string(Self).Split([Seperator]);
  Result := Length(arr);
end;

end.
