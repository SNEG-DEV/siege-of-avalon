unit SoAOS.StackTrace;
(*
  Siege Of Avalon : Open Source Edition

  Portions created by Steffen Nyeland are
  Copyright (C) 2021 - Steffen Nyeland.

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

  Description: StackTrace Hooks - utilizing the Jedi Projects JclDebug

  See JCL MPL license info included in jcl source folder - only a part of the full JCL is included.

  Requires: Delphi 10.3.3 or later
*)

interface

uses
	System.SysUtils, System.Classes, JclDebug;

implementation

function GetExceptionStackInfoProc(P: PExceptionRecord): Pointer;
var
	LLines: TStringList;
	LText: String;
	LResult: PChar;
	jcl_sil: TJclStackInfoList;
begin
	LLines := TStringList.Create;
	try
		jcl_sil:=TJclStackInfoList.Create(False, 7, p.ExceptAddr, False, nil, nil);
		try
			jcl_sil.AddToStrings(LLines, true, true, true, true);
		finally
			FreeAndNil(jcl_sil);
		end;
		LText := LLines.Text;
		LResult := StrAlloc(Length(LText));
		StrCopy(LResult, PChar(LText));
		Result := LResult;
	finally
		LLines.Free;
	end;
end;

function GetStackInfoStringProc(Info: Pointer): string;
begin
	Result := string(PChar(Info));
end;

procedure CleanUpStackInfoProc(Info: Pointer);
begin
	StrDispose(PChar(Info));
end;

initialization

if JclStartExceptionTracking then
begin
	Exception.GetExceptionStackInfoProc := GetExceptionStackInfoProc;
	Exception.GetStackInfoStringProc := GetStackInfoStringProc;
	Exception.CleanUpStackInfoProc := CleanUpStackInfoProc;
end;

finalization

if JclExceptionTrackingActive then
begin
	Exception.GetExceptionStackInfoProc := nil;
	Exception.GetStackInfoStringProc := nil;
	Exception.CleanUpStackInfoProc := nil;
	JclStopExceptionTracking;
end;

end.
