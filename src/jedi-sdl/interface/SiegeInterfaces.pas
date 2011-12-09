unit SiegeInterfaces;
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
{   September   11 2004 - DL : Initial Creation                                }
{                                                                              }
{
  $Log: SiegeInterfaces.pas,v $
  Revision 1.2  2005/05/06 08:06:12  savage
  removed spacing

  Revision 1.1  2004/09/30 22:49:20  savage
  Initial Game Interface units.


}
{******************************************************************************}

interface

uses
  sdl,
  sdlgameinterface;

type
  TSimpleSoAInterface = class( TGameInterface )
  private

  public
    DXBack : PSDL_Surface;
    procedure FreeSurfaces; override;
    procedure Render; override;
  end;



implementation

{ TSimpleSoAInterface }

procedure TSimpleSoAInterface.FreeSurfaces;
begin
  SDL_FreeSurface( DXBack );
  inherited;
end;

procedure TSimpleSoAInterface.Render;
var
  Rect : TSDL_Rect;
begin
  Rect.x := ( MainWindow.DisplaySurface.w - DXBack.w ) shr 1;
  Rect.y := ( MainWindow.DisplaySurface.h - DXBack.h ) shr 1;
  Rect.w := DXBack.w;
  Rect.h := DXBack.h;
  SDL_BlitSurface( DXBack, nil, MainWindow.DisplaySurface, @Rect );
end;

end.
