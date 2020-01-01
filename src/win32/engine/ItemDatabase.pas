unit ItemDatabase;
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

interface

uses
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  LogFile;

type
  TStringDatabase = class( TObject )
  private
    FDataBase : TStringList;
    FItemList : TStringList;
    FFileName : string;
    FItemName : string;
    DataString : AnsiString;
    function GetFields( FieldPos : integer ) : string;
  public
    constructor Create( const Filename : string );
    destructor Destroy; override;
    function FindRecord( const Key : string ) : boolean;
    property ItemName : string read FItemName;
    property FileName : string read FFileName;
    property Fields[ FieldPos : integer ] : string read GetFields;
  end;

implementation

uses
  SoAOS.StrUtils;

{ TStringDatabase }

constructor TStringDatabase.Create( const Filename : string );
var
  iLoop : integer;
begin
  FFileName := Filename;
  FDataBase := TStringList.Create;
  FItemList := TStringList.Create;
  if TFile.Exists( FFileName ) then
  begin
    FDataBase.LoadFromFile( FFileName );
    for iLoop := 0 to FDatabase.Count - 1 do
    begin
      FItemList.Add( TTokenString( AnsiString( FDatabase.Strings[ iLoop ] )).PipeToken( 0 ) {+ '=' + IntToStr(iLoop)} );
    end;
  end;

end;

destructor TStringDatabase.Destroy;
begin
  if Assigned( FDatabase ) then
  begin
    FDataBase.free;
    FDataBase := nil;
  end;

  if Assigned( FItemList ) then
  begin
    FItemList.free;
    FItemList := nil;
  end;
  inherited Destroy;
end;

function TStringDatabase.FindRecord( const Key : string ) : boolean;
var
  i : integer;
begin
  i := FItemList.IndexOf( Key );
  if i >= 0 then
  begin
    DataString := AnsiString( FDatabase.Strings[ i ] );
    result := true;
  end
  else
  begin
    DataString := '';
    result := false;
  end;
end;

function TStringDatabase.GetFields( FieldPos : integer ) : string;
const
  FailName : string = 'TStringDatabase.GetFields';
begin
  Log.DebugLog(FailName);
  try
    Result := TTokenString(DataString).PipeToken( FieldPos );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

end.
