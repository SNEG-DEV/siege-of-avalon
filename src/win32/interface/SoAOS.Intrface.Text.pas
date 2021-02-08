unit SoAOS.Intrface.Text;
(*
  Siege Of Avalon : Open Source Edition

  Portions created by Digital Tome L.P. Texas USA are
  Copyright ©1999-2000 Digital Tome L.P. Texas USA
  All Rights Reserved.

  Portions created by Team SOAOS are
  Copyright (C) 2003 - Team SOAOS.

  Portions created by Steffen Nyeland are
  Copyright (C) 2020 - Steffen Nyeland.

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

  Description: Interface to text.ini in Interface folder - which contains localized UI messages

  TODO: Should not use initialize and finalize - should just be read into memory at startup - less I/O

  Dual usage - The GameText uses the bitmap fonts which are ANSI mapped - and the TTF popups needs the encoded text.

  Requires: Delphi 10.3.3 or later

  Revision History:
  - 21 Jan 2020 - SN: Initial Commit to Git
  see git repo afterwards

*)

interface

uses
  System.SysUtils,
  System.Classes,
  System.IniFiles;

type
  TExternalizer = class( TObject )
  private
    INI : TMemINIFile;
    FSection : string;
    Encoded : boolean;
  public
    destructor Destroy; override;
    procedure Open( const Section : string );
    procedure OpenEncoded( const Section : string );
    procedure Close;
    function GetText( const ID : string ) : String;
    procedure GetSection( Strings : TStrings );
  end;

var
  ExText : TExternalizer;

implementation

uses
  AniDemo;

{ TExternalizer }

procedure TExternalizer.Close;
begin
  if Assigned( INI ) then
    FreeAndNil(INI);
end;

destructor TExternalizer.Destroy;
begin
  INI.Free;
  inherited;
end;

procedure TExternalizer.GetSection(Strings: TStrings);
begin
  INI.ReadSectionValues( FSection, Strings );
end;

function TExternalizer.GetText(const ID: string): String;
begin
  if Assigned( INI ) then
    Result := AnsiDequotedStr(INI.ReadString( FSection, ID, '' ), '"')
  else
    Result := '';
end;

procedure TExternalizer.Open(const Section: string);
begin
  FSection := Section;
  if Assigned( INI) and Encoded then
    FreeAndNil(INI);
  if not Assigned( INI ) then
  begin
    INI := TMemIniFile.Create( InterfacePath + 'text.ini' );
    Encoded := False;
  end;
end;

procedure TExternalizer.OpenEncoded(const Section: string);
begin
  FSection := Section;
  if Assigned( INI) and (not Encoded) then
    FreeAndNil(INI);
  if not Assigned( INI ) then
  begin
    INI := TMemIniFile.Create( InterfacePath + 'text.ini', TEncoding.GetEncoding(INICodepage) );
    Encoded := True;
  end;
end;

initialization
  begin
    ExText := TExternalizer.create;
  end;

finalization
  begin
    ExText.free;
  end;

end.
