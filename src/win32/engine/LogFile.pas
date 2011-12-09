unit LogFile;
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
  Classes,
  SysUtils,
  Windows;

type
  TLog = class( TFileStream )
  public
    iErr : Integer; // Convenient temporary error value storage
    CurrDbgGroup : Word; // obsolete, forcibly overwritten herein
    constructor Create( FileName : string );
    procedure Log( const FailName : string; const Msg : string; const Args : array of const ); overload;
    procedure Log( const Msg : string ); overload;
    procedure LogEntry( const FailName : string );
    procedure Comment( const Msg : string ); overload;
    // TLog.Msg is a less verbose message than a full TLog.Log entry (no timestamp)
    procedure Msg( const FailName : string; const Msg : string; const Args : array of const );
    function VersionInfo( const InfoStr, defaultStr : string ) : string;
    procedure Flush;
  private
    RealCurrDbgLvl, // Set by Command Line Parser to current Debug Level of Detail
      RealCurrDbgGroup : Word; // Set by Command Line Parser to current Debug Interest Group
    MEMORYSTATUS : TMemoryStatus;
    procedure OutputLogHeader;
    procedure ParseCommandLine;
  end;

const
  // Define "Level of Detail" for the debug log file, higher number means more text in the log file
  DbgLvlAbort = 0; // Output only those critical errors that might abort the game
  DbgLvlSevere = 1; // Output important information, uses more time/diskspace
  DbgLvlWarn = 2; // Level where all "FailName" dumps start happening
  DbgLvlAll = 3; // Output EVERYTHING you know about the program, disk space be damned.

  // Define "Log Flush" option for use when game crashes without useful log entries.
  DbgFlushLog = 4; // When this bit is true then we flush the log cache with each write

  // Define "Groupings" of debug information as bit mask value subset of DEBUG value
  DbgAudio = 8; // track sound system problems
  DbgAI = 16; // track Atrificial Intelegence problems???
  DbgRender = 32; // track normal GDI/DirectX Game Render problems
  DbgInterface = 64; // track problems when user is working with an Interface Screen
  DbgLoadup = 128; // track problems during program load/init (otherwise wait till game is really "running")
  DbgOnline = 256; // track problems that happen when game is accessing Internet services/programs/features
  DbgSaveLoad = 512; // track problems with file/game Save/Load actions

var
  Log : TLog;
  CurrDbgLvl : Integer;

implementation

uses String32,
  Forms,
  Engine,
  IniFiles;

{ TLog }

// NOTE! Accessing VersionInfo can cause WriteError on Write Protected Networked Removable Media

function TLog.VersionInfo( const InfoStr, defaultStr : string ) : string;
{ Legal InfoStr values are:
'CompanyName', 'FileDescription', 'FileVersion', 'InternalName',
'LegalCopyright', 'LegalTradeMarks', 'OriginalFilename',
'ProductName', 'ProductVersion', 'Comments'
}
var
  S : string;
  n, Len : DWORD;
  Buf,
    Value : PChar;
begin
  S := Application.ExeName;
  n := GetFileVersionInfoSize( PChar( S ), n );
  VersionInfo := defaultStr;
  if n > 0 then
  begin
    Buf := AllocMem( n );
    try
      GetFileVersionInfo( PChar( S ), 0, n, Buf );
      if VerQueryValue( Buf, PChar( 'StringFileInfo\040904E4\' + InfoStr ), Pointer( Value ), Len ) then
        VersionInfo := Value;
    finally
      FreeMem( Buf, n );
    end;
  end;
end;

function GetIniDebugValue( ) : Integer;
var
  INI : TIniFile;
begin
  INI := TIniFile.Create( ExtractFilePath( Application.ExeName ) + 'siege.ini' );
  try
    Result := INI.ReadInteger( 'Settings', 'Debug', 0 );
  finally
    INI.Free;
  end;
end;

procedure TLog.OutputLogHeader;
var
  S : string;
begin

  S := ExtractFileName( Application.ExeName ) + #13#10
    + 'Copyright ©1999-2000 Digital Tome L.P. Texas USA' + #13#10
    + 'Portions Copyright ©1999-2000 Digital Tome, Inc. ' + #13#10
    + 'All Rights Reserved' + #13#10 + #13#10;
  Write( S[ 1 ], Length( S ) );

  if RealCurrDbgLvl > 0 then
  begin
    S := 'FileVersion: ' + VersionInfo( 'FileVersion', 'VersionError' ) + #13#10;
    Write( S[ 1 ], Length( S ) );

    S := 'ProductVersion: ' + VersionInfo( 'ProductVersion', 'VersionError' ) + #13#10;
    Write( S[ 1 ], Length( S ) );

    S := 'Comments: ' + VersionInfo( 'Comments', 'No Comments' ) + #13#10 + #13#10;
    Write( S[ 1 ], Length( S ) );

    S := 'Commandline: ' + #13#10 + CmdLine + #13#10 + #13#10;
    Write( S[ 1 ], Length( S ) );
  end;

  MEMORYSTATUS.dwLength := SizeOf( MEMORYSTATUS );
  GlobalMemoryStatus( MEMORYSTATUS );
  with MEMORYSTATUS do
  begin
    // Size of MemoryStatus record
    S := IntToStr( dwLength ) + ' = Size of ''MemoryStatus'' record' + #13#10;

    // Per-Cent of Memory in use by your system
    S := S + IntToStr( dwMemoryLoad ) + '% memory in use' + #13#10;

    // The amount of Total Physical memory allocated to your system.
    S := S + IntToStr( dwTotalPhys ) + ' Total Physical Memory in bytes' + #13#10;

    // The amount available of physical memory in your system.
    S := S + IntToStr( dwAvailPhys ) + ' Available Physical Memory in bytes' + #13#10;

    // The amount of Total Bytes allocated to your page file.
    S := S + IntToStr( dwTotalPageFile ) + ' Total Bytes of Paging File' + #13#10;

    // The amount of available bytes in your page file.
    S := S + IntToStr( dwAvailPageFile ) + ' Available bytes in paging file' + #13#10;

    // The amount of Total bytes allocated to this program (generally 2 gigabytes of virtual space).
    S := S + IntToStr( dwTotalVirtual ) + ' User Bytes of Address space' + #13#10;

    // The amount of avalable bytes that is left to your program to use.
    S := S + IntToStr( dwAvailVirtual ) + ' Available User bytes of address space' + #13#10;

    // Nice little block seperator
    S := S + #13#10 + ' ===== End Of Header =====' + #13#10 + #13#10;
    Write( S[ 1 ], Length( S ) );
  end; { with }
end;

procedure TLog.ParseCommandLine;
const
  FailName : string = 'TLog.ParseCommandLine';
var
  i : Integer;
  w : Word;
  sTemp : string;
  DebugSet, BadParam : Boolean;
begin
  BadParam := False;
  DebugSet := False;
  w := 0;
  for i := 1 to ParamCount do
  begin
    sTemp := Trim( UpperCase( ParamStr( i ) ) );
    if ( Pos( 'DEBUG=', sTemp ) > 0 ) then
    begin
      DebugSet := True;
      ParseString( sTemp, '=' ); // crop off the prefix part
      Trim( sTemp );
      if Length( sTemp ) < 1 then
      begin
        BadParam := True;
        Break;
      end
      else
      begin
        if IsDigit( sTemp[ 1 ] ) or ( sTemp[ 1 ] = '$' ) then
        try
          w := StrToInt( sTemp );
        except
          BadParam := True;
        end
        else if CompareText( sTemp, 'ALL' ) = 0 then
          w := $FFFF
        else
          BadParam := True;
      end;
      Break;
    end;
  end; {for i := 1}
  if BadParam then
    w := 0;
  if w = 0 then
    w := GetIniDebugValue;
  RealCurrDbgGroup := w and $FFFC;
  RealCurrDbgLvl := w and 3;
  sTemp := '';
  if BadParam then
    sTemp := FailName + ' Invalid Debug Commandline Parameter ' + sTemp + #13#10#13#10 + CmdLine;
  if DebugSet or ( w > 0 ) then
    FmtStr( sTemp, FailName + ' Debug Set, CurrDbgGroup = %x, RealCurrDbgLvl = %d' + #13#10#13#10, [ RealCurrDbgGroup, RealCurrDbgLvl ] );
  if Length( sTemp ) > 0 then
    Write( sTemp[ 1 ], Length( sTemp ) );
end;

constructor TLog.Create( FileName : string );
begin
  try
    inherited Create( FileName, fmCreate or fmShareDenyWrite );
  except
    Abort;
  end;
  ParseCommandLine;
  OutputLogHeader;
end;

procedure TLog.Log( const FailName : string; const Msg : string; const Args : array of const );
var
  S : string;
begin
  CurrDbgLvl := RealCurrDbgLvl;
  CurrDbgGroup := RealCurrDbgGroup;
  S := TimeToStr( Time ) + ' ' + Format( '%8.8d', [ Game.FrameCount ] ) + ': ' + FailName + ' ' + Format( Msg, Args ) + #13#10;
  Write( S[ 1 ], Length( S ) );
  if ( RealCurrDbgGroup and DbgFlushLog ) <> 0 then
    FlushFileBuffers( Handle );
end;

procedure TLog.Msg( const FailName : string; const Msg : string; const Args : array of const );
var
  S : string;
begin
  CurrDbgLvl := RealCurrDbgLvl;
  CurrDbgGroup := RealCurrDbgGroup;
  S := FailName + ' ' + Format( Msg, Args ) + #13#10;
  Write( S[ 1 ], Length( S ) );
  if ( RealCurrDbgGroup and DbgFlushLog ) <> 0 then
    FlushFileBuffers( Handle );
end;

procedure TLog.LogEntry( const FailName : string );
var
  S : string;
begin
  CurrDbgLvl := RealCurrDbgLvl;
  CurrDbgGroup := RealCurrDbgGroup;
  S := TimeToStr( Time ) + ' ' + Format( '%8.8d', [ Game.FrameCount ] ) + ': === ' + FailName + ' ===' + #13#10;
  Write( S[ 1 ], Length( S ) );
  if ( RealCurrDbgGroup and DbgFlushLog ) <> 0 then
    FlushFileBuffers( Handle );
end;

procedure TLog.Log( const Msg : string );
var
  S : string;
begin
  CurrDbgLvl := RealCurrDbgLvl;
  CurrDbgGroup := RealCurrDbgGroup;
  S := TimeToStr( Time ) + ' ' + Format( '%8.8d', [ Game.FrameCount ] ) + ': ' + Msg + #13#10;
  Write( S[ 1 ], Length( S ) );
  if ( RealCurrDbgGroup and DbgFlushLog ) <> 0 then
    FlushFileBuffers( Handle );
end;

procedure TLog.Comment( const Msg : string );
var
  S : string;
begin
  if length( Msg ) > 0 then
  begin
    S := Msg + #13#10;
    Write( S[ 1 ], Length( S ) );
  end;
  if ( RealCurrDbgGroup and DbgFlushLog ) <> 0 then
    FlushFileBuffers( Handle );
end;

procedure TLog.Flush;
begin
  FlushFileBuffers( Handle );
end;

end.

