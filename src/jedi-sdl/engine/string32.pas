unit String32;
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
  Windows,
  SysUtils,
  Classes;

//function IsNormal(cTex: string): boolean;  //
function IsDigit( ch : Char ) : Boolean;
function IsUpper( ch : Char ) : Boolean;
function IsLower( ch : Char ) : Boolean;
function ToUpper( ch : Char ) : Char;
function ToLower( ch : Char ) : Char;
function Proper( const s : string ) : string;
function LTrim( const s : string ) : string;
function RTrim( const S : string ) : string;
//function Trim(const S: string): string;      //now included
function RightStr( const S : string; Size : Word ) : string;
function LeftStr( const S : string; Size : Word ) : string;
function MidStr( const S : string; Pos, Size : Word ) : string;
function InStr( const S, L : string ) : Integer;
function PadR( const s : string; n : Integer ) : string;
function Quoted( cString : string ) : string;
function ReplaceAll( S, Old, New : string; caseSen : Boolean ) : string;
function GetFileDate( cFile : string ) : string;
function BooleanToYesNo( b : Boolean ) : string;
function YesNoToBoolean( s : string ) : Boolean;
function LongFileNameToShort( f : string ) : string;
function LongDirectoryToShort( d : string ) : string;
procedure WriteLog( cFile, cMsg : string );
function ParseString( var s : string; delimeter : string ) : string;
function iif( b : Boolean; ifTrue, ifFalse : Variant ) : Variant;
function StringsEqual( s1, s2 : string ) : Boolean;
function AddRightSlash( cDir : string ) : string;

//versioning stuff, Delphi3
procedure GetBuildInfo( var V1, V2, V3, V4 : Word );
function GetBuildInfoString : string;

implementation

function IsDigit( ch : Char ) : Boolean;
begin
  Result := ch in [ '0'..'9' ];
end;

function IsUpper( ch : Char ) : Boolean;
// To determine if the character is an uppercase letter.
begin
  Result := ch in [ 'A'..'Z' ];
end;

function IsLower( ch : Char ) : Boolean;
// To determine if the character is an lowercase letter.
begin
  Result := ch in [ 'a'..'z' ];
end;

function ToUpper( ch : Char ) : Char;
// Changes a character to an uppercase letter.
begin
  Result := Chr( Ord( ch ) and $DF );
end;

function ToLower( ch : Char ) : Char;
// Changes a character to a lowercase letter.
begin
  Result := Chr( Ord( ch ) or $20 );
end;

function Proper( const s : string ) : string;
// Capitalizes first letter of every word in s if delimited with spaces
var
  i : Integer;
  CapitalizeNextLetter : Boolean;
begin
  Result := LowerCase( s );
  CapitalizeNextLetter := True;
  for i := 1 to Length( Result ) do
  begin
    if CapitalizeNextLetter and IsLower( Result[ i ] ) then
      Result[ i ] := ToUpper( Result[ i ] );
    CapitalizeNextLetter := Result[ i ] = ' ';
  end;
end;

function RTrim( const S : string ) : string;
begin
  Result := TrimRight( s )
end;

function LTrim( const S : string ) : string;
begin
  Result := TrimLeft( s )
end;

{Supresses trailing blanks in a string.}
{function RTrim(const s: string): string;
var
  i: integer;
begin
  i := Length(s);
  while (I > 0) and (s[i] <= ' ') do Dec(i);
  Result := Copy(s, 1, i);
end;}

{Removes the leading spaces from a string.}
{function LTrim(const S: string): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  Result := Copy(S, I, Maxint);
end;}

{ Removes leading and trailing whitespace from s}
{function Trim(const S: string): string;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then Result := '' else
  begin
    while S[L] <= ' ' do Dec(L);
    Result := Copy(S, I, L - I + 1);
  end;
end;}

function RightStr( const S : string; Size : Word ) : string;
var
  l : Integer;
begin
  l := Length( s );
  if Size > l then
    Size := l;
  RightStr := Copy( S, l - Size + 1, Size )
end {RightStr};

function LeftStr( const S : string; Size : Word ) : string;
begin
  LeftStr := Copy( S, 1, Size )
end {LeftStr};

function MidStr( const S : string; Pos, Size : Word ) : string;
var
  l : Integer;
begin
  l := Length( s );
  if Size > l then
    Size := l;
  MidStr := Copy( S, Pos, Size )
end {MidStr};

function InStr( const S, L : string ) : Integer;
var
  i, Len : Integer;
  Done, Found : Boolean;
begin

  Result := 0;
  if Length( s ) = 0 then
    Exit;

  Len := Length( l );
  Done := False;
  Found := False;
  i := 1;
  repeat
    if MidStr( s, i, Len ) = L then
      Found := True
    else
    begin
      Inc( i );
      Done := i > Length( s );
    end;
  until Done or Found;
  if Found then
    Result := i;

end;

function PadR( const s : string; n : Integer ) : string;
var
  i : Integer;
  spc : string;
begin
  spc := '';
  for i := 1 to n do
    spc := spc + ' ';
  Result := LeftStr( s + spc, n );
end;

function Quoted( cString : string ) : string;
begin
  Result := '"' + cString + '" ';
end;

function GetFileDate( cFile : string ) : string;
var
  TStream : TFileStream;
begin
  Result := '1/31/97';
  TStream := TFileStream.Create( cFile, fmShareDenyNone );
  try
    Result := DateToStr( FileDateToDateTime( FileGetDate( TStream.Handle ) ) );
  finally
    TStream.Free;
  end;
end;

{function IsNormal(cTex: string): boolean;
var c :char;
begin
  result := false;
  if length(cTex) = 0 then exit;
  c    := cTex[1];
  if (c in ['A'..'Z', 'a'..'z', '0'..'9']) and
     (cTex <> 'clip') and (cTex <> 'trigger') then begin
     result := true;
  end;
end;}

function ReplaceAll( S, Old, New : string; caseSen : Boolean ) : string;
var
  P, x : Smallint;
  tmpstr : string;
begin
  P := 1;
  if not caseSen then
    Old := AnsiLowerCase( Old );
  while P < Length( S ) do
  begin
    tmpstr := Copy( S, P, Length( S ) - P + 1 );
    if not caseSen then
      tmpstr := AnsiLowerCase( tmpstr );
    x := Pos( Old, tmpstr );
    if x > 0 then
    begin
      Delete( S, P + x - 1, Length( Old ) );
      Insert( New, S, P + x - 1 );
      P := P + x - 1 + Length( New );
    end
    else
      Inc( P );
  end;
  Result := S;
end;

procedure GetBuildInfo( var V1, V2, V3, V4 : Word );
var
  VerInfoSize : DWORD;
  VerInfo : Pointer;
  VerValueSize : DWORD;
  VerValue : PVSFixedFileInfo;
  Dummy : DWORD;
begin
  VerInfoSize := GetFileVersionInfoSize( PChar( ParamStr( 0 ) ), Dummy );
  GetMem( VerInfo, VerInfoSize );
  GetFileVersionInfo( PChar( ParamStr( 0 ) ), 0, VerInfoSize, VerInfo );
  VerQueryValue( VerInfo, '\', Pointer( VerValue ), VerValueSize );
  with VerValue^ do
  begin
    V1 := dwFileVersionMS shr 16;
    V2 := dwFileVersionMS and $FFFF;
    V3 := dwFileVersionLS shr 16;
    V4 := dwFileVersionLS and $FFFF;
  end;
  FreeMem( VerInfo, VerInfoSize );
end;

function GetBuildInfoString : string;
var
  V1, V2, V3, V4 : Word;
begin
  GetBuildInfo( V1, V2, V3, V4 );
  //Result := Format('%d.%d.%d.%d', [V1, V2, V3, V4]);
  Result := Format( '%d.%d.%d', [ V1, V2, V3 ] );
end;

function BooleanToYesNo( b : Boolean ) : string;
begin
  if b then
    Result := 'Y'
  else
    Result := 'N';
end;

function YesNoToBoolean( s : string ) : Boolean;
begin
  Result := not ( LowerCase( LeftStr( s, 1 ) ) = 'n' );
end;

function LongFileNameToShort( f : string ) : string;
var
  oDum : TFileStream;
  cTemp : array[ 0..259 ] of Char;
  bMadeFile : Boolean;
begin
  //file must exist or it fails! (stupid)
  if not FileExists( f ) then
  begin
    oDum := TFileStream.Create( f, fmCreate );
    oDum.WriteBuffer( 'xxx', 3 );
    oDum.Free;
    bMadeFile := True;
  end
  else
    bMadeFile := False;

  FillChar( cTemp, 260, 0 );
  GetShortPathName( @f[ 1 ], cTemp, 260 );
  Result := LowerCase( StrPas( cTemp ) );

  if bMadeFile then
    DeleteFile( PChar( f ) );
end;

function LongDirectoryToShort( d : string ) : string;
var
  cTemp : array[ 0..259 ] of Char;
  oDum : TFileStream;
begin
  //file must exist or it fails! (stupid)
  if ( RightStr( d, 1 ) <> '\'  )
  or ( RightStr( d, 1 ) <> '/'  ) then
    d := d + '/';
  d := d + 'x.x';
  oDum := TFileStream.Create( d, fmCreate );
  oDum.WriteBuffer( 'xxx', 3 );
  oDum.Free;

  FillChar( cTemp, 260, 0 );
  GetShortPathName( @d[ 1 ], cTemp, 260 );
  DeleteFile( PChar( d ) );
  d := LowerCase( ExtractFilePath( StrPas( cTemp ) ) );
  if RightStr( d, 1 ) = '\' then
    d := LeftStr( d, Length( d ) - 1 );
  Result := d;
end;

procedure WriteLog( cFile, cMsg : string );
var
  t : TStringList;
begin
  t := TStringList.Create;
  if FileExists( cFile ) then
    t.LoadFromFile( cFile );
  t.Add( cMsg );
  t.SaveToFile( cFile );
  t.Free;
end;

function ParseString( var s : string; delimeter : string ) : string;
var
  iPos : Integer;
begin
  if s = '' then
  begin
    Result := '';
  end
  else
  begin
    iPos := InStr( s, delimeter );
    if iPos > 0 then
    begin
      Result := Trim( LeftStr( s, iPos - 1 ) );
      s := Trim( MidStr( s, iPos + 1, Length( s ) ) );
    end
    else
    begin
      Result := s;
      s := '';
    end;
  end;
end;

function iif( b : Boolean; ifTrue, ifFalse : Variant ) : Variant;
//not really a string function, but oh well
begin
  if b then
    Result := ifTrue
  else
    Result := ifFalse
end;

function StringsEqual( s1, s2 : string ) : Boolean;
//NOT case sensitive;
begin
  Result := CompareText( s1, s2 ) = 0
end;

function AddRightSlash( cDir : string ) : string;
begin
  Result := cDir;
  if ( RightStr( Result, 1 ) <> '\' )
  or ( RightStr( Result, 1 ) <> '/' ) then
    Result := Result + '/';
end;

end.



