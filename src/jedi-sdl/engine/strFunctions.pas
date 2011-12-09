unit strFunctions;
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
  classes,
  IniFiles;

type
  TSROption = ( srWord, srCase, srAll );
  TSROptions = set of TsrOption;


function strUpper( const S : string ) : string;
function strEncrypt( const S : string; Key : Word ) : string;
function strDecrypt( const S : string; Key : Word ) : string;
function strLastCh( const S : string ) : Char;
procedure strStripLast( var S : string );
procedure strStripFirst( var S : string );
procedure strSearchReplace( var S : string; const Source, Dest : string; Options : TSRoptions );
function strReplace( const S : string; C : Char; const Replace : string ) : string;
function strContains( const S1, S2 : string ) : Boolean;
function strToken( var S : string; Seperator : Char ) : string;
function strTokenCount( S : string; Seperator : Char ) : Integer;
function strTokenAt( const S : string; Seperator : Char; At : Integer ) : string;
procedure strTokenToStrings( S : string; Seperator : Char; List : TStrings );
function strTokenFromStrings( Seperator : Char; List : TStrings ) : string;
function strLeft( const S : string; Len : Integer ) : string;
function strRight( const S : string; Len : Integer ) : string;
function strPadChL( const S : string; C : Char; Len : Integer ) : string;
function strPadChR( const S : string; C : Char; Len : Integer ) : string;
function strPadChC( const S : string; C : Char; Len : Integer ) : string;
function strPadL( const S : string; Len : Integer ) : string;
function strPadR( const S : string; Len : Integer ) : string;
function strPadC( const S : string; Len : Integer ) : string;
procedure AppendTextToFile( fileName : TFileName; const msg : string );
function strChrCount( S : string; Seperator : Char ) : Integer;
procedure CopyMemIniFile( Src, Dest : TMemIniFile );
procedure CopyIniFile( Src : TMemIniFIle; Dest : TIniFile );


const
  NULL = #0;
  BACKSPACE = #8;
  TAB = #9;
  LF = #10;
  CR = #13;
  EOF_ = #26;
  ESC = #27;
  BLANK = #32;
  SPACE = BLANK;
  C1 = 52845;
  C2 = 22719;
  CRLF : PChar = CR + LF;

implementation

function strEncrypt( const S : string; Key : Word ) : string;
var
  I : Integer;
begin
  SetLength( Result, Length( S ) );
  for I := 1 to Length( S ) do
  begin
    Result[ I ] := Char( Ord( S[ I ] ) xor ( Key shr 8 ) );
    Key := ( Ord( Result[ I ] ) + Key ) * C1 + C2;
  end;
end;

function strDecrypt( const S : string; Key : Word ) : string;
var
  I : Integer;
begin
  SetLength( Result, Length( S ) );
  for I := 1 to Length( S ) do
  begin
    Result[ I ] := char( Ord( S[ I ] ) xor ( Key shr 8 ) );
    Key := ( Ord( S[ I ] ) + Key ) * C1 + C2;
  end;
end;

function strLastCh( const S : string ) : Char;
begin
  Result := S[ Length( S ) ];
end;

procedure strStripLast( var S : string );
begin
  if Length( S ) > 0 then
    Delete( S, Length( S ), 1 );
end;

procedure strStripFirst( var S : string );
begin
  if Length( S ) > 0 then
    Delete( S, 1, 1 );
end;


procedure strSearchReplace( var S : string; const Source, Dest : string; Options : TSRoptions );
var
  hs, hs1, hs2, hs3 : string;
var
  i, j : integer;

begin
  if srCase in Options then
  begin
    hs := s;
    hs3 := source;
  end
  else
  begin
    hs := StrUpper( s );
    hs3 := StrUpper( Source );
  end;
  hs1 := '';
  I := pos( hs3, hs );
  j := length( hs3 );
  while i > 0 do
  begin
    delete( hs, 1, i + j - 1 );
    hs1 := Hs1 + copy( s, 1, i - 1 );
    delete( s, 1, i - 1 );
    hs2 := copy( s, 1, j );
    delete( s, 1, j );
    if ( not ( srWord in Options ) )
      or ( pos( s[ 1 ], ' .,:;-#''+*?=)(/&%$§"!{[]}\~<>|' ) > 0 ) then
    begin

      hs1 := hs1 + dest;
    end
    else
    begin
      hs1 := hs1 + hs2;
    end;
    if srall in options then
      I := pos( hs3, hs )
    else
      i := 0;
  end;
  s := hs1 + s;
end;

function strUpper( const S : string ) : string;
begin
  Result := AnsiUpperCase( S );
end;

function strReplace( const S : string; C : Char; const Replace : string ) : string;
var
  i : Integer;
begin
  Result := '';
  for i := Length( S ) downto 1 do
    if S[ i ] = C then
      Result := Replace + Result
    else
      Result := S[ i ] + Result;
end;

function strContains( const S1, S2 : string ) : Boolean;
begin
  Result := Pos( S1, S2 ) > 0;
end;

function strToken( var S : string; Seperator : Char ) : string;
var
  I : Word;
begin
  I := Pos( Seperator, S );
  if I <> 0 then
  begin
    Result := System.Copy( S, 1, I - 1 );
    System.Delete( S, 1, I );
  end
  else
  begin
    Result := S;
    S := '';
  end;
end;

function strTokenCount( S : string; Seperator : Char ) : Integer;
begin
  Result := 0;
  while S <> '' do
  begin
    StrToken( S, Seperator );
    Inc( Result );
  end;
end;

function strTokenAt( const S : string; Seperator : Char; At : Integer ) : string;
var
  j, i : Integer;
begin
  Result := '';
  j := 1;
  i := 0;
  while ( i <= At ) and ( j <= Length( S ) ) do
  begin
    if S[ j ] = Seperator then
      Inc( i )
    else if i = At then
      Result := Result + S[ j ];
    Inc( j );
  end;
end;

procedure strTokenToStrings( S : string; Seperator : Char; List : TStrings );
var
  Token : string;
begin
  List.Clear;
  Token := strToken( S, Seperator );
  while Token <> '' do
  begin
    List.Add( Token );
    Token := strToken( S, Seperator );
    if Token = '' then
      Token := strToken( S, Seperator );
  end;
end;

function strTokenFromStrings( Seperator : Char; List : TStrings ) : string;
var
  i : Integer;
begin
  Result := '';
  for i := 0 to List.Count - 1 do
    if Result <> '' then
      Result := Result + Seperator + List[ i ]
    else
      Result := List[ i ];
end;

function strLeft( const S : string; Len : Integer ) : string;
begin
  Result := Copy( S, 1, Len );
end;

function strRight( const S : string; Len : Integer ) : string;
begin
  if Len >= Length( S ) then
    Result := S
  else
    Result := Copy( S, Succ( Length( S ) ) - Len, Len );
end;

function strPadChL( const S : string; C : Char; Len : Integer ) : string;
begin
  Result := S;
  while Length( Result ) < Len do
    Result := C + Result;
end;

function strPadChR( const S : string; C : Char; Len : Integer ) : string;
begin
  Result := S;
  while Length( Result ) < Len do
    Result := Result + C;
end;

function strPadChC( const S : string; C : Char; Len : Integer ) : string;
begin
  Result := S;
  while Length( Result ) < Len do
  begin
    Result := Result + C;
    if Length( Result ) < Len then
      Result := C + Result;
  end;
end;

function strPadL( const S : string; Len : Integer ) : string;
begin
  Result := strPadChL( S, BLANK, Len );
end;

function strPadC( const S : string; Len : Integer ) : string;
begin
  Result := strPadChC( S, BLANK, Len );
end;


function strPadR( const S : string; Len : Integer ) : string;
begin
  Result := strPadChR( S, BLANK, Len );
end;


procedure AppendTextToFile( fileName : TFileName; const msg : string );
var
  Output : TextFile;
begin
  if Length( fileName ) = 0 then
    exit; // If there is no filename, exit

  AssignFile( Output, fileName ); // Get our handle
  if not FileExists( fileName ) then // If the file not there
    ReWrite( Output ); //   create it
  Append( Output ); // set the file up for append
  Writeln( Output, msg ); // write our msg
  CloseFile( Output ); // close the file
end;

function strChrCount( S : string; Seperator : Char ) : Integer;
begin
  Result := 0;
  while Pos( Seperator, S ) > 0 do
  begin
    S[ Pos( Seperator, S ) ] := '+';
    Inc( Result );
  end;
end;

procedure CopyMemIniFile( Src, Dest : TMemIniFile );
var
  mySectionsList : TStrings;
  myValuesList : TStrings;
  i : integer;
  j : integer;
begin
  mySectionsList := TStringList.Create;
  myValuesList := TStringList.Create;

  Src.ReadSections( mySectionsList );
  for i := 0 to mySectionsList.Count - 1 do
  begin
    src.ReadSectionValues( mySectionsList.Strings[ i ], myValuesList );
    for j := 0 to myValuesList.count - 1 do
    begin
      Dest.WriteString( mySectionsList.Strings[ i ], StrTokenAt( myValuesList.Strings[ j ], '=', 0 ), Src.ReadString( mySectionsList.Strings[ i ], StrTokenAt( myValuesList.Strings[ j ], '=', 0 ), '' ) );
    end;
    myValuesList.Clear;
  end;
end;

procedure CopyIniFile( Src : TMemIniFile; Dest : TIniFIle );
var
  mySectionsList : TStrings;
  myValuesList : TStrings;
  i : integer;
  j : integer;
begin
  mySectionsList := TStringList.Create;
  myValuesList := TStringList.Create;

  Src.ReadSections( mySectionsList );
  for i := 0 to mySectionsList.Count - 1 do
  begin
    src.ReadSectionValues( mySectionsList.Strings[ i ], myValuesList );
    for j := 0 to myValuesList.count - 1 do
    begin
      Dest.WriteString( mySectionsList.Strings[ i ], StrTokenAt( myValuesList.Strings[ j ], '=', 0 ), Src.ReadString( mySectionsList.Strings[ i ], StrTokenAt( myValuesList.Strings[ j ], '=', 0 ), '' ) );
    end;
    myValuesList.Clear;
  end;
end;



end.

