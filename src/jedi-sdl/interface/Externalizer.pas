unit Externalizer;
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
{   SDL ( http://www.libsdl.org ) and DirectX ( http://www.microsoft.com )     }
{   Runtime libraris on Win32 and just SDL ( http://www.libsdl.org ) shared    }
{   objects or their equivalents on Linux and other Unixes                     }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{   Should compile with Delphi, Kylix and FreePascal on Win32 and Linux for    }
{   starters and FreeBSD and MacOS X etc there after.                          }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{   September   23 2004 - DL : Initial Creation                                }
{                                                                              }
{
  $Log: Externalizer.pas,v $
  Revision 1.3  2005/06/02 22:51:54  savage
  More Cross-Platform additions and amendments

  Revision 1.2  2005/06/01 21:42:41  savage
  More Linux compatability fixes.

  Revision 1.1  2004/09/30 22:49:20  savage
  Initial Game Interface units.


}
{******************************************************************************}


interface

uses
  Classes,
  IniFiles;

type
  TExternalizer = class( TObject )
  private
    INI : TIniFile;
    FSection : string;
  public
    destructor Destroy; override;
    procedure Open( const Section : string );
    procedure Close;
    function GetText( const ID : string ) : string;
    procedure GetSection( Strings : TStrings );
  end;

implementation

uses
  xplatformutils,
  globals;

{ TExternalizer }
procedure TExternalizer.Close;
begin
  if assigned( INI ) then
  begin
    INI.free;
    INI := nil;
  end;
end;

destructor TExternalizer.Destroy;
begin
  INI.free;
  inherited;
end;

procedure TExternalizer.GetSection(Strings: TStrings);
begin
  INI.ReadSectionValues( FSection, Strings );
end;

function TExternalizer.GetText(const ID: string): string;
begin
  if assigned( INI ) then
    result := INI.ReadString( FSection, ID, '' )
  else
    result := '';
end;

procedure TExternalizer.Open(const Section: string);
begin
  FSection := Section;
  if not assigned( INI ) then
    INI := TIniFile.create( SoASettings.InterfacePath + DIR_SEP + SoASettings.LanguagePath + DIR_SEP + 'Text.ini' );
end;

initialization

finalization

end.
 