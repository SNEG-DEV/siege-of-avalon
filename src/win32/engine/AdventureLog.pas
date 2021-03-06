unit AdventureLog;
(*
  Siege Of Avalon : Open Source Edition

  Portions created by Digital Tome L.P. Texas USA are
  Copyright ?1999-2000 Digital Tome L.P. Texas USA
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

  Description:

  Requires: Delphi 10.3.3 or later

  Revision History:
  - 13 Jul 2003 - DL: Initial Upload to CVS
  - 10 Mar 2019 - SN: Forked on GitHub
  see git repo afterwards

*)

interface

uses
  System.Classes;

type
//  TAboutAdventureLogProperty = class( TPropertyEditor )
//  public
//    procedure Edit; override;
//    function GetAttributes : TPropertyAttributes; override;
//    function GetValue : string; override;
//  end;

  TAdventureLog = class( TObject )
  private
//    FAbout : TAboutAdventureLogProperty;
    FAdventureLog : TStrings;
    FLogDateList : TStringList;
    FLogDirectory : string;
  protected
    { Protected declarations }
  public
    constructor Create;
    destructor Destroy; override;
    function AddLogEntry( LogFile : string ) : integer;
    function ReadLogByName( LogFile : string ) : AnsiString;
    function ReadLogByIndex( LogIndex : integer ) : AnsiString;
    procedure DeleteLogEntry( LogFile : string );
    procedure Clear;
//  published
//    property About : TAboutAdventureLogProperty read FAbout write FAbout;
    property LogFileList : TStringList read FLogDateList write FLogDateList;
    property LogDirectory : string read FLogDirectory write FLogDirectory;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  LogFile;

//procedure TAboutAdventureLogProperty.Edit;
//begin
//  Application.MessageBox( 'Adventure Log for The Game Master' + #39 + 's Table. (c) 1999 Random Features, Inc.',
//    'TAdventureLog component version 1.0', MB_OK + MB_ICONINFORMATION );
//end;

//function TAboutAdventureLogProperty.GetAttributes : TPropertyAttributes;
//begin
//  Result := [ paMultiSelect, paDialog, paReadOnly ];
//end;

//function TAboutAdventureLogProperty.GetValue : string;
//begin
//  Result := '(about)';
//end;


constructor TAdventureLog.Create;
const
  FailName : string = 'TAdventureLog.Create';
begin
  Log.DebugLog(FailName);
  try

    inherited Create;
    FLogDateList := TStringList.Create;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TAdventureLog.Destroy;
begin
  inherited Destroy;
  FAdventureLog.Free;
  FAdventureLog := nil;
  FLogDateList.free;
  FLogDateList := nil;
end;

function TAdventureLog.AddLogEntry( LogFile : string ) : integer;
var
  i : integer;
const
  FailName : string = 'TAdventureLog.AddLogEntry';
begin
  result := -1;
  Log.DebugLog(FailName);
  try

    i := FLogDateList.IndexOf( LogFile );
    if i = -1 then
      result := FLogDateList.Add( LogFile );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TAdventureLog.ReadLogByName( LogFile : string ) : AnsiString;
var
  MyStream : TFileStream;
  iSize : integer;
  MyString : AnsiString;
const
  FailName : string = 'TAdventureLog.ReadLogByName';
begin
  Log.DebugLog(FailName);
  try

    if not ( TFile.Exists( FLogDirectory + LogFile ) ) then
    begin
      result := '';
      exit;
    end;
    MyStream := TFileStream.Create( FLogDirectory + LogFile, fmOpenRead );
    iSize := MyStream.Size;
    SetLength( MyString, iSize );
    MyStream.Read( MyString[ 1 ], iSize );
    MyStream.free;
    result := MyString;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TAdventureLog.ReadLogByIndex( LogIndex : integer ) : AnsiString;
var
  MyStream : TFileStream;
  iSize : integer;
  MyString : AnsiString;
const
  FailName : string = 'TAdventureLog.ReadLogByIndex';
begin
  Log.DebugLog(FailName);
  try

    if ( LogIndex >= 0 ) and ( LogIndex < FLogDateList.Count ) then
    begin
      if FLogDateList.strings[ LogIndex ] <> '' then
      begin
        if not ( TFile.Exists( FLogDirectory + FLogDateList.strings[ LogIndex ] ) ) then
        begin
          result := '';
          exit;
        end;
        MyStream := TFileStream.Create( FLogDirectory + FLogDateList.strings[ LogIndex ], fmOpenRead );
        iSize := MyStream.Size;
        SetLength( MyString, iSize );
        MyStream.Read( MyString[ 1 ], iSize );
        MyStream.free;
        result := MyString;
      end;
    end
    else
    begin
      result := '';
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TAdventureLog.DeleteLogEntry( LogFile : String );
const
  FailName : string = 'TAdventureLog.DeleteLogEntry';
begin
  Log.DebugLog(FailName);
  try

    FLogDateList.Delete( FLogDateList.IndexOf( LogFile ) );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TAdventureLog.Clear;
const
  FailName : string = 'TAdventureLog.Clear';
begin
  Log.DebugLog(FailName);
  try

    FLogDateList.Clear;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

end.
