unit AdventureLog;
{******************************************************************************}
{                                                                              }
{               Siege Of Avalon : Open Source Edition                          }
{               -------------------------------------                          }
{                                                                              }
{ Portions created by Digital Tome L.P. Texas USA are                          }
{ Copyright ©1999-2000 Digital Tome L.P. Texas USA                             }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Portions created by Team SOAOS are                                           }
{ Copyright (C) 2003 - Team SOAOS.                                             }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                            }
{                                                                              }
{                                                                              }
{                                                                              }
{ You may retrieve the latest version of this file at the SOAOS project page : }
{   http://www.sourceforge.com/projects/soaos                                  }
{                                                                              }
{ The contents of this file maybe used with permission, subject to             }
{ the GNU Lesser General Public License Version 2.1 (the "License"); you may   }
{ not use this file except in compliance with the License. You may             }
{ obtain a copy of the License at                                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Software distributed under the License is distributed on an                  }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{ implied. See the License for the specific language governing                 }
{ rights and limitations under the License.                                    }
{                                                                              }
{ Description                                                                  }
{ -----------                                                                  }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   DirectX Runtime libraris on Win32                                          }
{   They are available from...                                                 }
{   http://www.microsoft.com.                                                  }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{   July    13 2003 - DL : Initial Upload to CVS                               }
{                                                                              }
{******************************************************************************}

{$INCLUDE Anigrp30cfg.inc}

interface

uses
  Classes,
  SysUtils,
  logger;

type
  {TAboutAdventureLogProperty = class(TPropertyEditor)
 public
  procedure Edit; override;
  function GetAttributes: TPropertyAttributes; override;
  function GetValue: string; override;
 end;}

  TAdventureLog = class( TObject )
  private
    //FAbout: TAboutAdventureLogProperty;
    FAdventureLog : TStrings;
    FLogDateList : TStringList;
    FLogDirectory : string;
  protected
    { Protected declarations }
  public
    constructor Create;
    destructor Destroy; override;
    function AddLogEntry( LogFile : string ) : integer;
    function ReadLogByName( LogFile : string ) : string;
    function ReadLogByIndex( LogIndex : integer ) : string;
    procedure DeleteLogEntry( LogFile : string );
    procedure Clear;
  published
    //property About: TAboutAdventureLogProperty read FAbout write FAbout;
    property LogFileList : TStringList read FLogDateList write FLogDateList;
    property LogDirectory : string read FLogDirectory write FLogDirectory;
  end;

implementation

{procedure TAboutAdventureLogProperty.Edit;
begin
 Application.MessageBox('Adventure Log for The Game Master'+#39+'s Table. (c) 1999 Random Features, Inc.',
                                       'TAdventureLog component version 1.0', MB_OK+ MB_ICONINFORMATION);
end;

function TAboutAdventureLogProperty.GetAttributes: TPropertyAttributes;
begin
 Result := [paMultiSelect, paDialog, paReadOnly];
end;

function TAboutAdventureLogProperty.GetValue: string;
begin
 Result := '(about)';
end;}


constructor TAdventureLog.Create;
const
  FailName : string = 'TAdventureLog.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited Create;
    FLogDateList := TStringList.Create;
  except
    on E : Exception do
      Log.LogError( E.Message, FailName );
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
  try

    i := FLogDateList.IndexOf( LogFile );
    if i = -1 then
      result := FLogDateList.Add( LogFile );

  except
    on E : Exception do
      Log.LogError( E.Message, FailName );
  end;
end;

function TAdventureLog.ReadLogByName( LogFile : string ) : string;
var
  MyStream : TFileStream;
  iSize : integer;
  MyString : string;
const
  FailName : string = 'TAdventureLog.ReadLogByName';
begin
  try
    if not ( fileExists( FLogDirectory + LogFile ) ) then
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
      Log.LogError( E.Message, FailName );
  end;
end;

function TAdventureLog.ReadLogByIndex( LogIndex : integer ) : string;
var
  MyStream : TFileStream;
  iSize : integer;
  MyString : string;
const
  FailName : string = 'TAdventureLog.ReadLogByIndex';
begin
  try

    if ( LogIndex >= 0 ) and ( LogIndex < FLogDateList.Count ) then
    begin
      if FLogDateList.strings[ LogIndex ] <> '' then
      begin
        if not ( fileExists( FLogDirectory + FLogDateList.strings[ LogIndex ] ) ) then
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
      Log.LogError( E.Message, FailName );
  end;
end;

procedure TAdventureLog.DeleteLogEntry( LogFile : string );
const
  FailName : string = 'TAdventureLog.DeleteLogEntry';
begin
  try

    FLogDateList.Delete( FLogDateList.IndexOf( LogFile ) );

  except
    on E : Exception do
      Log.LogError( E.Message, FailName );
  end;
end;

procedure TAdventureLog.Clear;
const
  FailName : string = 'TAdventureLog.Clear';
begin
  try

    FLogDateList.Clear;

  except
    on E : Exception do
      Log.LogError( E.Message, FailName );
  end;
end;

end.

