unit Security;
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
{*****************************************************************************
 Digital Tome Game Engine

 Copyright ©1999-2001 Digital Tome L.P. Texas USA
 Not for public release/use.

 This WAS a "dirty little stop-gap" solution to pretending that we have a
 real Anti-Piracy system in the game. This still does not support the highly
 secure system implemented for us by Dariusz, but does have a little more code
 to support the WEB.DE online Registration system for Blackstar Interactive GmbH
 in Germany. It now also can be in the "uses" of the
 Siege-of-Avalon.com/scripts/Login.exe program for serial number validation.
 Now supports Online Registration of DigitalTome/USA/Domestic sales like
 that done for Blackstar.
 NOTE: The references herein to "fake" registration has to do with the codes
 created before the online validation was supported. It isn't really "fake"
 any more, they do now serve a real (albeit lame) purpose.

*****************************************************************************}

interface

uses
  Windows,
  SysUtils,
  Forms,
  Classes,
  IniFiles;

var
  ChapterAuthorizeMask : Int64;

const
  SFPCallBack1Val : Integer = -2; // DO NOT CHANGE! JShiflett
  SFPCallBack2Val : Integer = -2; // DO NOT CHANGE! JShiflett

function GetChapterAuthorizeMask( INIFilename : string ) : Boolean;
function StringToCRC( CRCString : AnsiString ) : LongWord;
function IsDTRegSystem( sNumber : string ) : Boolean;
function CharVal( const C : Char ) : Integer;
function CleanSerialNumber( SerialNumber : string ) : string;
function ComputeAuthValue( key, ID : string ) : string;
function ExtractChapterNumber( const sNumber : string ) : Integer;
function SetChapterNumber( const Number : Integer ) : string;
function IsSerialValid( SerialNumber : AnsiString ) : Boolean;

implementation

uses
  String32;

var
  VolSerial : DWORD;
  VolSerialString : AnsiString;

const
  Authorize6Chapters = Int64( $1F ); // Value needed to let game play Chapters 1 through 6
  AuthorizeChapter20 = Int64( $40000 ); // Value needed to approve "Chapter 20", used for Spain Siege CD

  // === SUPER-SECRET CD-COPY-PREVENTION HOOKS. DO NOT CHANGE! JShiflett

procedure SFPCallBack1;
begin
  SFPCallBack1Val := Integer( GetDriveType( 'c:\' ) );
end;

procedure SFPCallBack2;
var
  Buffer : array[ 0..MAX_PATH - 1 ] of Char;
begin
  SFPCallBack2Val := Integer( GetCurrentDirectory( SizeOf( Buffer ), Buffer ) );
end;

function SFPLoopBack1 : Boolean;
begin
  Result := SFPCallBack1Val = Integer( GetDriveType( 'c:\' ) );
end;

function SFPLoopBack2 : Boolean;
var
  Buffer : array[ 0..MAX_PATH - 1 ] of Char;
begin
  Result := SFPCallBack2Val = Integer( GetCurrentDirectory( SizeOf( Buffer ), Buffer ) );
end;

exports
  SFPCallBack1 Name 'SFINIT0',
  SFPCallBack2 Name 'SFINIT1',
  SFPLoopBack1 Name 'SFLB_0',
  SFPLoopBack1 Name 'SFLB_1';

// === END OF SUPER-SECRET CD-COPY-PREVENTION HOOKS.

function IsDTRegSystem( sNumber : string ) : Boolean;
begin
  Result := False;
  if Length( sNumber ) < 2 then
    Exit;
  Result := sNumber[ 2 ] in [ '0'..'9' ]; // old-style "fake" registration number
end;

function CharVal( const C : Char ) : Integer;
begin
  CharVal := Ord( C ) - Ord( '0' );
end;

function CleanSerialNumber( SerialNumber : string ) : string;
var
  idx : Integer;
begin
  SerialNumber := Trim( SerialNumber );
  idx := 1;
  while idx < Length( SerialNumber ) do
  begin
    while not ( SerialNumber[ idx ] in [ '0'..'9', 'a'..'z', 'A'..'Z' ] ) do
      Delete( SerialNumber, idx, 1 );
    Inc( idx, 1 );
  end;
  Result := SerialNumber;
end;

function ComputeAuthValue( key, ID : string ) : string;
var
  val1, val2, val3 : Int64;
  Tmp : string;
begin
  if Length( key ) < 15 then
    Tmp := '0'
  else
    Tmp := Copy( key, Length( key ) - 8, Length( key ) );
  val1 := StrToInt64( Tmp );
  val2 := StrToInt64( ID );
  val3 := ( val1 + val2 ) div 3;
  ComputeAuthValue := IntToStr( val3 );
end;

function ExtractChapterNumber( const sNumber : string ) : Integer;
begin
  Result := 0;
  if Length( sNumber ) >= 4 then
    Result := StrToInt( Copy( sNumber, 4, 1 ) + Copy( sNumber, 3, 1 ) );
end;

function SetChapterNumber( const Number : Integer ) : string;
begin
  try
    Result := IntToStr( Number );
    if Number > 9 then
      Result := '00' + Result[ 2 ] + Result[ 1 ]
    else
      Result := '00' + Result[ 1 ] + '0';
  except
    Result := '';
  end;
end;

procedure EnableRegisterButtonOnLoader;
var
  datFileName : string;
  datFile : TIniFile;
begin
  datFileName := ExtractFilePath( Application.ExeName ) + 'DTLoader.dat';
  datFile := TIniFile.Create( datFileName );
  try
    try
      datFile.WriteString( 'Digital', 'ShowReg', 'True' );
    except
    end;
  finally
    datFile.Free;
  end;
end;

function IsSerialValid( SerialNumber : AnsiString ) : Boolean;
var
  SumSet1, SumSet2, i : Integer;
begin
  Result := True;
  SumSet1 := 0;
  SumSet2 := 0;
  SerialNumber := CleanSerialNumber( SerialNumber ); // probably redundant
  if Length( SerialNumber ) <> 15 then
  begin
    Result := False;
    Exit;
  end;
  try
    // '10' is the ID for Siege of Avalon, or '1a' for Blackstar versions
    if ( SerialNumber[ 1 ] <> '1' ) or ( ( SerialNumber[ 2 ] <> '0' ) and ( SerialNumber[ 2 ] <> 'a' ) ) then
      Result := False;

    // '64' is the maximum legal Chapters for Siege of Avalon, digits swapped
    i := ExtractChapterNumber( SerialNumber );
    if ( i < 1 ) or ( i > 64 ) then
      Result := False;

    // if (CompareStr(SerialNumber, '101000000000000') = 0) then Result := False;
    if ( CompareStr( '000000000', Copy( SerialNumber, 7, 9 ) ) = 0 ) then
      Result := False; // block illegal number

    if not Result then
      Exit;

    // Prepare to validate 2 internal checksums
    for i := 7 to 10 do
      SumSet1 := SumSet1 + StrToInt( Copy( SerialNumber, i, 1 ) );
    for i := 11 to 14 do // We overlooked the LSD of the number in "fake" version
      SumSet2 := SumSet2 + StrToInt( Copy( SerialNumber, i, 1 ) );

    // Normalize and validate extra checksum of newer version numbers
    if not IsDTRegSystem( SerialNumber ) then
    begin // new version for Blackstar
      SerialNumber[ 5 ] := Char( Ord( SerialNumber[ 5 ] ) - Ord( 'Q' ) + Ord( '0' ) ); // remove ASCII offset
      SerialNumber[ 6 ] := Char( Ord( SerialNumber[ 6 ] ) - Ord( 'Q' ) + Ord( '0' ) );
      i := ( CharVal( SerialNumber[ 3 ] ) + CharVal( SerialNumber[ 4 ] )
        + CharVal( SerialNumber[ 5 ] ) + CharVal( SerialNumber[ 6 ] ) ) mod ( 10 );
      if i <> CharVal( SerialNumber[ 15 ] ) then
        Result := False;
    end;

    // Validate 2 internal checksums
    if ( Sumset1 mod ( 5 ) ) <> CharVal( SerialNumber[ 5 ] ) then
      Result := False;
    if ( Sumset2 mod ( 6 ) ) <> CharVal( SerialNumber[ 6 ] ) then
      Result := False;

  except
    Result := False;
  end;
end;

function GetVolumeID( DriveChar : Char; var VolSerial : DWORD ) : string;
var
  OldErrorMode : Integer;
  NotUsed, VolFlags : DWORD;
  Buf : array[ 0..MAX_PATH ] of Char;
begin
  OldErrorMode := SetErrorMode( SEM_FAILCRITICALERRORS );
  try
    Buf[ 0 ] := #0;
    if GetVolumeInformation( PChar( DriveChar + ':\' ), Buf, DWORD( SizeOf( Buf ) ),
      @VolSerial, NotUsed, VolFlags, nil, 0 ) then
      SetString( Result, Buf, StrLen( Buf ) )
    else
      Result := '';
  finally
    SetErrorMode( OldErrorMode );
  end;
end;

function KeyIsValid( SerialNumber, KeyString : AnsiString ) : Boolean;
begin // Do IsSerialValid before you call this
  try
    Result := 0 = ( AnsiCompareStr( KeyString, IntToStr( StringToCRC( SerialNumber + VolSerialString ) ) ) );
  except
    Result := False;
  end;
end;

function RegIsValid( SerialNumber, KeyString : AnsiString ) : Boolean;
begin // Do IsSerialValid before you call this
  try
    Result := 0 = ( AnsiCompareStr( KeyString, ComputeAuthValue( SerialNumber, VolSerialString ) ) );
  except
    Result := False;
  end;
end;

procedure UpdateChapterAuthorizeMask( SerialNumber : AnsiString );
var // Do IsSerialValid and KeyIsValid before you call this
  i : Integer;
  cam : Int64;
begin
  try
    i := ExtractChapterNumber( SerialNumber );
    if i < 1 then
      Exit;
    if i = 1 then
    begin // This is the original subscription setting
      ChapterAuthorizeMask := ChapterAuthorizeMask or Authorize6Chapters; // Authorize Chapters 2 to 6
      Exit;
    end;

    Dec( i ); // Chapter 1 is always free, don't count it.
    cam := Int64( 1 ) shl Int64( i - 1 );
    ChapterAuthorizeMask := ChapterAuthorizeMask or cam;
  except
  end;
end;

function GetChapterAuthorizeMask( INIFilename : string ) : Boolean;
var
  SerialNumber, KeyString,
    SerialName, KeyName : AnsiString;
  INI : TMemIniFile; // Harder to hack than TIniFile
  List : TStringList;
  i, j : Integer;
  S : string;
  RKeyMismatch : Boolean;
  // FoundOldRegVersion: Boolean;
  // Year, Month, Day: Word;

  (*
  function IsReadyToForceUpdate: Boolean;
  begin
    Result := False;
    // DecodeDate(Now, Year, Month, Day);  //TIMEBOMB - Hardcoded Arbitrary Date!
    // if (Year > 2001) then begin
    i := INI.ReadInteger('Keys', 'Var', 0);
    Inc(i);
    if i > 15 then i := 0;              // game will attempt to update itself every 15 times it is run
    INI.WriteInteger('Keys', 'Var', i);
    if i = 0 then Result := True;
    // end;
  end;
  *)

  procedure ProcessCDChapters; // Handle Chapters installed from a retail CD
  var
    ChapterNumber : Integer;
    ChapterNumStr : string;
  begin
    for ChapterNumber := 2 to 64 do
    begin
      ChapterNumStr := Trim( IntToStr( ChapterNumber ) );
      // If sold Online at WEB.DE then it MUST be registered so bypass this trick
      if INI.ValueExists( 'Versions', 'SoACH' + ChapterNumStr + '_biW' ) then
        Continue;
      // If sold on Blackstar CD then no registration needed so trick the authorization mask
      if INI.ValueExists( 'Versions', 'SoACH' + ChapterNumStr + '_bi' ) or
        INI.ValueExists( 'Versions', 'SoACH' + ChapterNumStr + '_biE' ) then
        UpdateChapterAuthorizeMask( SetChapterNumber( ChapterNumber ) );
    end;
    // If sold on Global Star Anthology CD then no registration needed.
    if INI.ValueExists( 'Versions', 'SoACD' ) and
      INI.ValueExists( 'InstallPaths', 'SoACD' ) and
      INI.ValueExists( 'Chapters', 'Chapter 2' ) and
      INI.ValueExists( 'Chapters', 'Chapter 3' ) and
      INI.ValueExists( 'Chapters', 'Chapter 4' ) and
      INI.ValueExists( 'Chapters', 'Chapter 5' ) then
      ChapterAuthorizeMask := ChapterAuthorizeMask or Authorize6Chapters;
  end;

begin
  //??? TODO: Support "grace period" while waiting for formal new version registration, maybe.
  Result := True;
  // FoundOldRegVersion := False;
  ChapterAuthorizeMask := 0;
  RKeyMismatch := False;
  GetVolumeID( 'C', VolSerial );
  VolSerialString := IntToStr( VolSerial );

  // CD-COPY-PREVENTION! DO NOT CHANGE! Used for Spanish Anthology CD. JShiflett
  if ( SFPCallBack1Val <> -2 ) and ( SFPCallBack2Val <> -2 ) then
  begin
    if not ( SFPLoopBack1 and SFPLoopBack2 ) then
    begin
      Result := False; // Illegal copy CD, abort.
      Exit;
    end;
    // Legal CD, authorize game maps, including special maps for this CD
    ChapterAuthorizeMask := AuthorizeChapter20 + Authorize6Chapters;
    Exit;
  end;
  // END OF CD-COPY-PREVENTION! DO NOT CHANGE! JShiflett

  INI := nil;
  INI := TMemIniFile.Create( INIFilename ); // Access to the 'siege.ini' file
  try
    if Length( VolSerialString ) < 4 then // GetFakeVolSerialString
      VolSerialString := INI.ReadString( 'Keys', 'RKey', '0' )
    else
    begin
      RKeyMismatch := CompareStr( VolSerialString, INI.ReadString( 'Keys', 'RKey', '0' ) ) <> 0;
    end;

    List := TStringList.Create;
    try
      // Authorize Chapters installed from a retail CD
      if not RKeyMismatch then
        ProcessCDChapters;

      // Process registration keys for downloaded Chapters, if any
      INI.ReadSectionValues( 'Keys', List );
      for i := 0 to List.Count - 1 do
      begin
        S := Trim( LowerCase( List.Strings[ i ] ) );
        if Pos( 'serial', S ) = 1 then
        begin
          S := Trim( List.Strings[ i ] ); // no case changing on actual test string
          j := Pos( '=', S );
          if j > 0 then
          begin
            SerialNumber := Copy( S, j + 1, Length( S ) - j );
            SerialNumber := CleanSerialNumber( SerialNumber );
            if Length( SerialNumber ) < 15 then
              Continue;

            // Compute value to save in SIEGE.INI [Keys] Key__=xxxx
            KeyString := IntToStr( StringToCRC( SerialNumber + VolSerialString ) );
            SerialName := Copy( S, 1, j - 1 );
            Keyname := 'Key' + Copy( SerialName, 7, Length( Serialname ) - 6 );
            S := Trim( List.Values[ KeyName ] );

            // Authorize Chapters installed from an online purchase
            if IsSerialValid( SerialNumber ) then
            begin
              if S = '' then
              begin // brand this SIEGE.INI to this computer
                INI.WriteString( 'Keys', KeyName, KeyString );
                S := KeyString;
              end;

              if KeyIsValid( SerialNumber, S ) then
              begin
                Keyname := 'RegID' + Copy( SerialName, 7, Length( Serialname ) - 6 );
                S := Trim( List.Values[ KeyName ] );
                if ( S <> '' ) and RegIsValid( SerialNumber, S ) then
                  UpdateChapterAuthorizeMask( SerialNumber )
                else
                begin
                  EnableRegisterButtonOnLoader;
                  INI.WriteString( 'Keys', KeyName, '' );
                end;
                {end;}
              end;
            end
            else
              EnableRegisterButtonOnLoader; // Assure user can enter new Registration Numbers
          end;
        end;
      end;
    finally
      // if FoundOldRegVersion then Result := not IsReadyToForceUpdate; // Nasty trick
      List.Free;
    end;
  finally
    if Assigned( INI ) then
      INI.UpdateFile; // Required for MemIni files
    INI.Free;
  end;
end;

function StringToCRC( CRCString : AnsiString ) : LongWord;
var
  table : array[ 0..255 ] of LongWord; // CRC table.
  halfi : ^LongWord; // Pointer to CRC of i / 2.
  crc : LongWord; // Current CRC.
  BufPtr : ^Byte; // Pointer to walk through buffer.
  i, x, Loop : Integer;

const
  polynomial = $00102100; // CCITT spec value

type
  dwordrec =
    record
    Lo, Hi : Word
  end;

  function LowW( DWORD : Longint ) : Word;
  begin
    LowW := ( dwordrec( DWORD ) ).Lo
  end;

  function HiW( DWORD : Longint ) : Word;
  begin
    HiW := ( dwordrec( DWORD ) ).Hi
  end;

begin
  crc := 0;
  CRCString := CRCString + #0; // append a NULL

  // Generate a CRC lookup table for faster calculation. Static tables are easy to hack.
  halfi := @table;
  table[ 0 ] := 0;
  for Loop := 0 to 127 do
  begin
    i := Loop * 2;
    if ( Hi( HiW( halfi^ ) ) and $80 ) = $80 then
    begin
      table[ i + 1 ] := halfi^ shl 1;
      table[ i ] := table[ i + 1 ] xor polynomial;
    end
    else
    begin
      table[ i ] := halfi^ shl 1;
      table[ i + 1 ] := table[ i ] xor polynomial;
    end;
    Inc( halfi );
  end;

  // Compute CRC value from input string
  BufPtr := @CRCString[ 1 ];
  for x := 1 to Length( CRCString ) do
  begin
{$R-}
    crc := ( crc shl 8 ) xor table[ Hi( HiW( crc ) ) xor BufPtr^ ];
    Inc( BufPtr );
  end;
  StringToCRC := crc;
end;

end.
