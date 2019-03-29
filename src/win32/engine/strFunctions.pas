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
  System.Classes;

type
  TSROption = ( srWord, srCase, srAll );
  TSROptions = set of TsrOption;

procedure strStripLast( var S : string );
procedure strStripFirst( var S : string );
function strToken( var S : string; Seperator : Char ) : string;
function strTokenCount( S : string; Seperator : Char ) : Integer;
function strTokenAt( const S : string; Seperator : Char; At : Integer ) : string;
procedure strTokenToStrings( S : string; Seperator : Char; List : TStrings );
function strLeft( const S : string; Len : Integer ) : string;

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

implementation

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

function strLeft( const S : string; Len : Integer ) : string;
begin
  Result := Copy( S, 1, Len );
end;

end.
