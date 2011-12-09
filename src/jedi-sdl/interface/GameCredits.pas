unit GameCredits;
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
{   starter and FreeBSD and MacOS X etc there after.                           }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{   September   30 2004 - DL : Initial Creation                                }
{                                                                              }
{
  $Log: GameCredits.pas,v $
  Revision 1.3  2005/05/13 12:33:14  savage
  Various Changes and bug fixes. Main work on the NewGame screen.

  Revision 1.2  2005/05/10 14:12:46  savage
  Latest Enhancments and bug fixes

  Revision 1.1  2004/09/30 22:49:20  savage
  Initial Game Interface units.


}
{******************************************************************************}

interface

uses
  sdl,
  SiegeInterfaces;

type
  TGameCredits = class( TSimpleSoAInterface )
  public
    procedure FreeSurfaces; override;
    procedure LoadSurfaces; override;
    procedure Render; override;
    procedure MouseDown( Button : Integer; Shift : TSDLMod; CurrentPos : TPoint ); override;
    procedure MouseMove( Shift : TSDLMod; CurrentPos : TPoint; RelativePos : TPoint ); override;
    procedure KeyDown( var Key : TSDLKey; Shift : TSDLMod; unicode : UInt16 ); override;
  end;

implementation

uses
  SysUtils,
  logger,
  sdlaudiomixer,
  globals,
  GameMainMenu;

{ TGameCredits }

procedure TGameCredits.FreeSurfaces;
begin
  GameAudio.MusicManager.TrackNames['credits'].Stop;
  inherited;
end;

procedure TGameCredits.KeyDown( var Key : TSDLKey; Shift : TSDLMod;
  unicode : UInt16 );
begin
  inherited;
  MainWindow.Rendering := false;
end;

procedure TGameCredits.LoadSurfaces;
const
  FailName : string = 'TGameCredits.LoadSurfaces';
  Flags : Cardinal = SDL_SRCCOLORKEY or SDL_RLEACCEL or SDL_HWACCEL;
begin
  inherited;
  try
    DXBack := SDL_LoadBMP( PChar( SoASettings.InterfacePath + '/' + 'CreditsScreen.bmp' ) );
    SDL_SetColorKey( DXBack, Flags, SDL_MapRGB( DXBack.format, 0, 255, 255 ) );

    NextGameInterface := TMainMenu;

    // Queue Music and Action
    GameAudio.MusicManager.Add( TSDLMusic.Create( SoASettings.SoundPath + '/Theme/exCARLibur.mp3' ), 'credits' );
    GameAudio.MusicManager.TrackNames['credits'].Volume := SoASettings.MusicVolume;
    GameAudio.MusicManager.TrackNames['credits'].Play;
  except
    on E: Exception do
      Log.LogError( E.Message, FailName );
  end;
end;

procedure TGameCredits.MouseDown( Button : Integer; Shift : TSDLMod; CurrentPos : TPoint );
begin
  inherited;
  MainWindow.Rendering := false;
end;

procedure TGameCredits.MouseMove( Shift : TSDLMod; CurrentPos, RelativePos : TPoint );
begin
  inherited;

end;

procedure TGameCredits.Render;
begin
  inherited;

end;

end.
