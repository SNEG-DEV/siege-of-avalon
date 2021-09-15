unit LogFile;
(*
  Siege Of Avalon : Open Source Edition

  Portions created by Digital Tome L.P. Texas USA are
  Copyright ©1999-2000 Digital Tome L.P. Texas USA
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
  System.Classes,
  System.IOUtils,
  System.SysUtils,
  Winapi.Windows;

type
  TLog = class( TFileStream )
  private
    RealCurrDbgLvl, // Set by Command Line Parser to current Debug Level of Detail
    RealCurrDbgGroup : Word; // Set by Command Line Parser to current Debug Interest Group
    MEMORYSTATUS : TMemoryStatus;
    procedure OutputLogHeader;
    procedure ParseCommandLine;
  public
    iErr : Integer; // Convenient temporary error value storage
    CurrDbgGroup : Word; // obsolete, forcibly overwritten herein
    constructor Create( FileName : string );
    procedure Log( const FailName : string; const Msg : string; const Args : array of const ); overload;
    procedure Log( const FailName : string; const Msg : string; const StackTrace : string; const Args : array of const ); overload;
    procedure Log( const Msg : string ); overload;
    procedure LogEntry( const FailName : string );
    procedure DebugLog( const FailName : string );
    procedure Comment( const Msg : string ); overload;
    // TLog.Msg is a less verbose message than a full TLog.Log entry (no timestamp)
    procedure Msg( const FailName : string; const Msg : string; const Args : array of const );
    function VersionInfo( const InfoStr, defaultStr : string ) : string;
    procedure Flush;
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

uses
  System.StrUtils,
  Engine,
  System.IniFiles;

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
  S := ParamStr(0);
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
  INI := TIniFile.Create( SiegeINIFile );
  try
    Result := INI.ReadInteger( 'Settings', 'Debug', 0 );
  finally
    INI.Free;
  end;
end;

procedure TLog.OutputLogHeader;
var
  S : AnsiString;
//  B : TBytes;
begin

  S := AnsiString( ExtractFileName( ParamStr(0) ) + #13#10
    + 'Copyright ©1999-2000 Digital Tome L.P. Texas USA' + #13#10
    + 'Portions Copyright ©1999-2000 Digital Tome, Inc. ' + #13#10
    + 'All Rights Reserved' + #13#10 + #13#10 );
//  b := TEncoding.UTF8.GetBytes(s);
// S := TEncoding.UTF8.GetString(b);
  Write( S[1], Length( S ) );

  if RealCurrDbgLvl > 0 then
  begin
    S := AnsiString( 'FileVersion: ' + VersionInfo( 'FileVersion', 'VersionError' ) + #13#10 );
//    b := TEncoding.UTF8.GetBytes(s);
    Write( S[1], Length( S ) );

    S := AnsiString( 'ProductVersion: ' + VersionInfo( 'ProductVersion', 'VersionError' ) + #13#10 );
//    b := TEncoding.UTF8.GetBytes(s);
    Write( S[1], Length( S ) );

    S := AnsiString( 'Comments: ' + VersionInfo( 'Comments', 'No Comments' ) + #13#10 + #13#10 );
//    b := TEncoding.UTF8.GetBytes(s);
    Write( S[1], Length( S ) );

    S := AnsiString( 'Commandline: ' + #13#10 + CmdLine + #13#10 + #13#10 );
//    b := TEncoding.UTF8.GetBytes(s);
    Write( S[1], Length( S ) );
  end;

  MEMORYSTATUS.dwLength := SizeOf( MEMORYSTATUS );
  GlobalMemoryStatus( MEMORYSTATUS );
  with MEMORYSTATUS do
  begin
    // Size of MemoryStatus record
    S := AnsiString( IntToStr( dwLength ) + ' = Size of ''MemoryStatus'' record' + #13#10 );

    // Per-Cent of Memory in use by your system
    S := S + AnsiString( IntToStr( dwMemoryLoad ) + '% memory in use' + #13#10 );

    // The amount of Total Physical memory allocated to your system.
    S := S + AnsiString( IntToStr( dwTotalPhys ) + ' Total Physical Memory in bytes' + #13#10 );

    // The amount available of physical memory in your system.
    S := S + AnsiString( IntToStr( dwAvailPhys ) + ' Available Physical Memory in bytes' + #13#10 );

    // The amount of Total Bytes allocated to your page file.
    S := S + AnsiString( IntToStr( dwTotalPageFile ) + ' Total Bytes of Paging File' + #13#10 );

    // The amount of available bytes in your page file.
    S := S + AnsiString( IntToStr( dwAvailPageFile ) + ' Available bytes in paging file' + #13#10 );

    // The amount of Total bytes allocated to this program (generally 2 gigabytes of virtual space).
    S := S + AnsiString( IntToStr( dwTotalVirtual ) + ' User Bytes of Address space' + #13#10 );

    // The amount of avalable bytes that is left to your program to use.
    S := S + AnsiString( IntToStr( dwAvailVirtual ) + ' Available User bytes of address space' + #13#10 );

    // Nice little block seperator
    S := S + AnsiString( #13#10 + ' ===== End Of Header =====' + #13#10 + #13#10 );
//    b := TEncoding.UTF8.GetBytes(s);
    Write( S[1], Length( S ) );
  end; { with }
end;

procedure TLog.ParseCommandLine;
const
  FailName : string = 'TLog.ParseCommandLine';
var
  i : Integer;
  w : Integer;
  sTemp : AnsiString;
  DebugSet, BadParam : Boolean;
begin
  BadParam := False;
  DebugSet := False;
  w := 0;
  for i := 1 to ParamCount do
  begin
    sTemp := AnsiString( Trim( ParamStr( i ) ) );
    if StartsText( 'DEBUG=', sTemp ) then  // Only DEBUG param seems valid
    begin
      DebugSet := True;
      sTemp := AnsiString( ReplaceText( sTemp, 'DEBUG=', '' ) );
      if Length( sTemp ) < 1 then
      begin
        BadParam := True;
        Break;
      end
      else
      begin
        if TryStrToInt( sTemp, w ) then
          BadParam := False
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
    sTemp := AnsiString( FailName + ' Invalid Debug Commandline Parameter ' + sTemp + #13#10#13#10 + CmdLine );
  if DebugSet or ( w > 0 ) then
    sTemp := AnsiString( Format( FailName + ' Debug Set, CurrDbgGroup = %x, RealCurrDbgLvl = %d' + #13#10#13#10, [ RealCurrDbgGroup, RealCurrDbgLvl ] ) );
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

procedure TLog.DebugLog(const FailName: string);
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    LogEntry( FailName );
{$ENDIF}
end;

procedure TLog.Log( const FailName : string; const Msg : string; const Args : array of const );
var
  S : AnsiString;
//  b : UTF8String; // TBytes;
begin
  CurrDbgLvl := RealCurrDbgLvl;
  CurrDbgGroup := RealCurrDbgGroup;
  if Assigned(Game) then
    S := AnsiString( TimeToStr( Time ) + ' ' + Format( '%8.8d', [ Game.FrameCount ] ) + ': ' + FailName + ' ' + Format( Msg, Args ) + #13#10 )
  else
    S := AnsiString( TimeToStr( Time ) + ' ' + 'Before Init' + ': ' + FailName + ' ' + Format( Msg, Args ) + #13#10 );
//  b := TEncoding.UTF8.GetBytes(s);
  Write( S[1], Length( S ) );
  if ( RealCurrDbgGroup and DbgFlushLog ) <> 0 then
    FlushFileBuffers( Handle );
end;

procedure TLog.Msg( const FailName : string; const Msg : string; const Args : array of const );
var
  S : AnsiString;
//  b : TBytes;
begin
  CurrDbgLvl := RealCurrDbgLvl;
  CurrDbgGroup := RealCurrDbgGroup;
  S := AnsiString( FailName + ' ' + Format( Msg, Args ) + #13#10 );
//  b := TEncoding.UTF8.GetBytes(s);
  Write( S[1], Length( S ) );
  if ( RealCurrDbgGroup and DbgFlushLog ) <> 0 then
    FlushFileBuffers( Handle );
end;

procedure TLog.LogEntry( const FailName : string );
var
  S : AnsiString;
//  b : TBytes;
begin
  CurrDbgLvl := RealCurrDbgLvl;
  CurrDbgGroup := RealCurrDbgGroup;
  S := AnsiString( TimeToStr( Time ) + ' ' + Format( '%8.8d', [ Game.FrameCount ] ) + ': === ' + FailName + ' ===' + #13#10 );
//  b := TEncoding.UTF8.GetBytes(s);
  Write( S[1], Length( S ) );
  if ( RealCurrDbgGroup and DbgFlushLog ) <> 0 then
    FlushFileBuffers( Handle );
end;

procedure TLog.Log( const Msg : string );
var
  S : AnsiString;
//  b : TBytes;
begin
  CurrDbgLvl := RealCurrDbgLvl;
  CurrDbgGroup := RealCurrDbgGroup;
  if Game <> nil then
    S := AnsiString( TimeToStr( Time ) + ' ' + Format( '%8.8d', [ Game.FrameCount ] ) + ': ' + Msg + #13#10 )
  else
    S := AnsiString( TimeToStr( Time ) + ' ' + Format( '%8.8d', [ -1 ] ) + ': ' + Msg + #13#10 );
//  b := TEncoding.UTF8.GetBytes(s);
  Write( S[1], Length( S ) );
  if ( RealCurrDbgGroup and DbgFlushLog ) <> 0 then
    FlushFileBuffers( Handle );
end;

procedure TLog.Log(const FailName, Msg, StackTrace: string; const Args: array of const);
var
  S : AnsiString;
begin
  CurrDbgLvl := RealCurrDbgLvl;
  CurrDbgGroup := RealCurrDbgGroup;
  if Game <> nil then
    S := AnsiString( TimeToStr( Time ) + ' ' + Format( '%8.8d', [ Game.FrameCount ] ) + ': ' + Msg + sLineBreak + StackTrace )
  else
    S := AnsiString( TimeToStr( Time ) + ' ' + Format( '%8.8d', [ -1 ] ) + ': ' + Msg + sLineBreak + StackTrace );
//  b := TEncoding.UTF8.GetBytes(s);
  Write( S[1], Length( S ) );
  if ( RealCurrDbgGroup and DbgFlushLog ) <> 0 then
    FlushFileBuffers( Handle );
end;

procedure TLog.Comment( const Msg : string );
var
  S : AnsiString;
//  B : TBytes;
begin
  if length( Msg ) > 0 then
  begin
    S := AnsiString( Msg + #13#10 );
//    b := TEncoding.UTF8.GetBytes(s);
    Write( S[1], Length( S ) );
  end;
  if ( RealCurrDbgGroup and DbgFlushLog ) <> 0 then
    FlushFileBuffers( Handle );
end;

procedure TLog.Flush;
begin
  FlushFileBuffers( Handle );
end;

end.

