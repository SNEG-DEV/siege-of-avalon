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

function IsDigit( ch : Char ) : Boolean;
function IsLower( ch : Char ) : Boolean;
function ToUpper( ch : Char ) : Char;
function RightStr( const S : string; Size : Word ) : string;
function LeftStr( const S : string; Size : Word ) : string;
function MidStr( const S : string; Pos, Size : Word ) : string;
function InStr( const S, L : string ) : Integer;
function ParseString( var s : string; delimeter : string ) : string;

implementation

uses
  SysUtils;

function IsDigit( ch : Char ) : Boolean;
begin
  Result := ch in [ '0'..'9' ];
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

end.

