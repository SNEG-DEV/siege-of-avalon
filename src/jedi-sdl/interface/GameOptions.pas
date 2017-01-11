unit GameOptions;
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
  $Log: GameOptions.pas,v $
  Revision 1.6  2005/06/02 22:51:54  savage
  More Cross-Platform additions and amendments

  Revision 1.5  2005/05/25 23:15:42  savage
  Latest Changes

  Revision 1.4  2005/05/13 12:33:15  savage
  Various Changes and bug fixes. Main work on the NewGame screen.

  Revision 1.3  2005/05/11 13:38:29  savage
  Fix for GameOption Sliders and tidying up the GameJournal and AdventureLog files.

  Revision 1.2  2004/10/17 18:36:05  savage
  Game Options now include language selection

  Revision 1.1  2004/09/30 22:49:20  savage
  Initial Game Interface units.


}
{******************************************************************************}

interface

uses
  Classes,
  sdl,
  SiegeInterfaces;

type
  TMouseOverGameOptions = ( moNone, moContinue, moSound, moMusic, moShadowsOn, moShadowsOff, moEnglish, moSpanish, moGerman, moSpells );

  TLanguageSelected = ( lsEnglish, lsSpanish, lsGerman );
  
  TGameOptions = class( TSimpleSoAInterface )
  private
    DXContinue, DXVolumeSlider, DXBackHighlight : PSDL_Surface;
    DxLangEng, DxLangSpa, DxLangGer : PSDL_Surface;
    DxTextMessage : array[ 0..4 ] of PSDL_Surface;
    SpellList : TStringList;
    MouseOverOptions : TMouseOverGameOptions;
    ShadowsOn : Boolean;
    LanguageSelected : TLanguageSelected;
    EngRect, SpaRect, GerRect : TSDL_Rect;
    SoundRect, MusicRect, ShadowOnRect, ShadowOffRect, ContinueRect : TSDL_Rect;
    SpellsRect : TSDL_Rect;
    SoundPos, MusicPos : integer;
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
  globals,
  GameMainMenu,
  sdlgameinterface;

{ TGameOptions }

procedure TGameOptions.FreeSurfaces;
var
  i : integer;
begin
  SDL_FreeSurface( DXContinue );
  SDL_FreeSurface( DXVolumeSlider );
  SDL_FreeSurface( DXBackHighlight );
  SDL_FreeSurface( DxLangEng );
  SDL_FreeSurface( DxLangSpa );
  SDL_FreeSurface( DxLangGer );
  for i := Low( DxTextMessage ) to High( DxTextMessage ) do
    SDL_FreeSurface( DxTextMessage[ i ] );

  ExText.Close;

  SpellList.Free;
  inherited;
end;

procedure TGameOptions.KeyDown( var Key : TSDLKey; Shift : TSDLMod; unicode : UInt16 );
begin
  inherited;
  MainWindow.Rendering := false;
end;

procedure TGameOptions.LoadSurfaces;
var
  i : integer;
  Flags : Cardinal;
  C : TSDL_Color;
begin
  inherited;
  Flags := SDL_SRCCOLORKEY or SDL_RLEACCEL or SDL_HWACCEL;

  SpellList := TStringList.Create;

  ExText.Open( 'Options' );
  C.r := 231;
  C.g := 156;
  C.b := 0;
  GameFont.ForeGroundColour := C;
  C.r := 0;
  C.g := 0;
  C.b := 0;
  GameFont.BackGroundColour := C;
  {if SoASettings.UseSmallFont then
    GameFont.FontSize := 13
  else}
  GameFont.FontSize := 18;

  for i := Low( DxTextMessage ) to High( DxTextMessage ) do
  begin
    DxTextMessage[ i ] := GameFont.DrawText( ExText.GetText( 'Message' + IntToStr( i ) ) );
    SDL_SetColorKey( DxTextMessage[ i ], Flags, SDL_MapRGB( DxTextMessage[ i ].format, 0, 0, 0 ) );
  end;

  //TODO : { if Character <> nil then SpellList := Character.SpellList; }

  DXContinue := SDL_LoadBMP( PChar( SoASettings.InterfacePath + DIR_SEP + SoASettings.LanguagePath + DIR_SEP + 'opContinue.bmp' ) );
  SDL_SetColorKey( DXContinue, Flags, SDL_MapRGB( DXContinue.format, 0, 255, 255 ) );

  DXVolumeSlider := SDL_LoadBMP( PChar( SoASettings.InterfacePath + DIR_SEP + 'opVolume.bmp' ) );
  SDL_SetColorKey( DXVolumeSlider, Flags, SDL_MapRGB( DXVolumeSlider.format, 0, 255, 255 ) );

  DXBack := SDL_LoadBMP( PChar( SoASettings.InterfacePath + DIR_SEP + SoASettings.LanguagePath + DIR_SEP + 'options.bmp' ) );
  SDL_SetColorKey( DXBack, Flags, SDL_MapRGB( DXBack.format, 0, 255, 255 ) );

  DxLangEng := SDL_LoadBMP( PChar( SoASettings.InterfacePath + DIR_SEP + 'english.bmp' ) );
  SDL_SetColorKey( DxLangEng, Flags, SDL_MapRGB( DxLangEng.format, 0, 255, 255 ) );

  DxLangSpa := SDL_LoadBMP( PChar( SoASettings.InterfacePath + DIR_SEP + 'spanish.bmp' ) );
  SDL_SetColorKey( DxLangSpa, Flags, SDL_MapRGB( DxLangSpa.format, 0, 255, 255 ) );

  DxLangGer := SDL_LoadBMP( PChar( SoASettings.InterfacePath + DIR_SEP + 'german.bmp' ) );
  SDL_SetColorKey( DxLangGer, Flags, SDL_MapRGB( DxLangGer.format, 0, 255, 255 ) );

  // Setup Rects
  if SoASettings.LanguagePath = 'english' then
    LanguageSelected := lsEnglish;
  EngRect.x := 363;
  EngRect.y := 135;
  EngRect.w := DxLangEng.w shr 1;
  EngRect.h := DxLangEng.h;

  if SoASettings.LanguagePath = 'spanish' then
    LanguageSelected := lsSpanish;

  SpaRect.x := 468;
  SpaRect.y := 135;
  SpaRect.w := DxLangSpa.w shr 1;
  SpaRect.h := DxLangSpa.h;

  if SoASettings.LanguagePath = 'german' then
    LanguageSelected := lsGerman;

  GerRect.x := 573;
  GerRect.y := 135;
  GerRect.w := DxLangGer.w shr 1;
  GerRect.h := DxLangGer.h;

  SoundRect.x := 100;
  SoundRect.y := 88;
  SoundRect.w := 230;
  SoundRect.h := 35;

  MusicRect.x := 100;
  MusicRect.y := 172;
  MusicRect.w := 230;
  MusicRect.h := 35;

  ShadowOnRect.x := 557;
  ShadowOnRect.y := 71;
  ShadowOnRect.w := 20;
  ShadowOnRect.h := 20;

  ShadowOffRect.x := 628;
  ShadowOffRect.y := 71;
  ShadowOffRect.w := 20;
  ShadowOffRect.h := 20;

  ContinueRect.x := 500;
  ContinueRect.y := 450;
  ContinueRect.w := 200;
  ContinueRect.h := 45;

  SpellsRect.x := 0;
  SpellsRect.y := 0;
  SpellsRect.w := 200;
  SpellsRect.h := 45;
  DXBackHighlight := SDL_CreateRGBSurface( SDL_SWSURFACE, SpellsRect.w, SpellsRect.h,
    MainWindow.DisplaySurface.format.BitsPerPixel, MainWindow.DisplaySurface.format.RMask, MainWindow.DisplaySurface.format.GMask,
    MainWindow.DisplaySurface.format.BMask, MainWindow.DisplaySurface.format.AMask );
  SDL_FillRect( DXBackHighlight, @SpellsRect, SDL_MapRGB( MainWindow.DisplaySurface.format, 255, 255, 0 ) );
  SpellsRect.x := 500;
  SpellsRect.y := 450;

  // Load Selections from SoASettings
  ShadowsOn := SoASettings.ShadowsOn;
  SoundPos := Round( ( ( SoASettings.SoundVolume * SoundRect.w ) / 255 ) + SoundRect.x );
  MusicPos := Round( ( ( SoASettings.MusicVolume * MusicRect.w ) / 255 ) + MusicRect.x );

  NextGameInterface := TMainMenu; // TODO : Change this to something more appropriate
end;

procedure TGameOptions.MouseDown( Button : Integer; Shift : TSDLMod; CurrentPos : TPoint );
begin
  inherited;
  if PointIsInRect( CurrentPos, SoundRect.x, SoundRect.y, SoundRect.x + SoundRect.w, SoundRect.y + SoundRect.h ) then //Sound Volume
  begin
    SoundPos := CurrentPos.x;
    if SoundPos < 116 then
      SoundPos := 116;
    if SoundPos > 316 then
      SoundPos := 316;
  end
  else if PointIsInRect( CurrentPos, MusicRect.x, MusicRect.y, MusicRect.x + MusicRect.w, MusicRect.y + MusicRect.h ) then //Music Volume
  begin
    MusicPos := CurrentPos.x;
    if MusicPos < 116 then
      MusicPos := 116;
    if MusicPos > 316 then
      MusicPos := 316;
  end;

  if PointIsInRect( CurrentPos, ShadowOnRect.x, ShadowOnRect.y, ShadowOnRect.x + ShadowOnRect.w, ShadowOnRect.y + ShadowOnRect.h ) then
  begin //yes
    ShadowsOn := true;
  end
  else if PointIsInRect( CurrentPos, ShadowOffRect.x, ShadowOffRect.y, ShadowOffRect.x + ShadowOffRect.w, ShadowOffRect.y + ShadowOffRect.h ) then
  begin //no
    ShadowsOn := false;
  end
  else if PointIsInRect( CurrentPos, EngRect.x, EngRect.y, EngRect.x + EngRect.w, EngRect.y + EngRect.w ) then
  begin
    LanguageSelected := lsEnglish
  end
  else if PointIsInRect( CurrentPos, SpaRect.x, SpaRect.y, SpaRect.x + SpaRect.w, SpaRect.y + SpaRect.w ) then
  begin
    LanguageSelected := lsSpanish
  end
  else if PointIsInRect( CurrentPos, GerRect.x, GerRect.y, GerRect.x + GerRect.w, GerRect.y + GerRect.w ) then
  begin
    LanguageSelected := lsGerman
  end
  else if PointIsInRect( CurrentPos, ContinueRect.x, ContinueRect.y, ContinueRect.x + ContinueRect.w, ContinueRect.y + ContinueRect.h ) then
  begin // Continue
    // Save Selections to SoASettings
    SoASettings.ShadowsOn := ShadowsOn;
    SoASettings.SoundVolume := Round( ( ( SoundPos - SoundRect.x ) / SoundRect.w ) * 255 );
    SoASettings.MusicVolume := Round( ( ( MusicPos - MusicRect.x ) / MusicRect.w ) * 255 );
    case LanguageSelected of
      lsEnglish :
      begin
        SoASettings.LanguagePath := 'english';
      end;

      lsSpanish :
      begin
        SoASettings.LanguagePath := 'spanish';
      end;

      lsGerman :
      begin
        SoASettings.LanguagePath := 'german';
      end;
    end;
    MainWindow.Rendering := false;
  end;
end;

procedure TGameOptions.MouseMove( Shift : TSDLMod; CurrentPos, RelativePos : TPoint );
begin
  inherited;

  MouseOverOptions := moNone;
  if PointIsInRect( CurrentPos, ContinueRect.x, ContinueRect.y, ContinueRect.x + ContinueRect.w, ContinueRect.y + ContinueRect.h ) then
    MouseOverOptions := moContinue
  else if PointIsInRect( CurrentPos, SoundRect.x, SoundRect.y, SoundRect.x + SoundRect.w, SoundRect.y + SoundRect.h ) then
    MouseOverOptions := moSound
  else if PointIsInRect( CurrentPos, MusicRect.x, MusicRect.y, MusicRect.x + MusicRect.w, MusicRect.y + MusicRect.h ) then
    MouseOverOptions := moMusic
  else if PointIsInRect( CurrentPos, ShadowOnRect.x, ShadowOnRect.y, ShadowOnRect.x + ShadowOnRect.w, ShadowOnRect.y + ShadowOnRect.h ) then
    MouseOverOptions := moShadowsOn
  else if PointIsInRect( CurrentPos, ShadowOffRect.x, ShadowOffRect.y, ShadowOffRect.x + ShadowOffRect.w, ShadowOffRect.y + ShadowOffRect.h ) then
    MouseOverOptions := moShadowsOff
  else if PointIsInRect( CurrentPos, EngRect.x, EngRect.y, EngRect.x + EngRect.w, EngRect.y + EngRect.h ) then
    MouseOverOptions := moEnglish
  else if PointIsInRect( CurrentPos, SpaRect.x, SpaRect.y, SpaRect.x + SpaRect.w, SpaRect.y + SpaRect.h ) then
    MouseOverOptions := moSpanish
  else if PointIsInRect( CurrentPos, GerRect.x, GerRect.y, GerRect.x + GerRect.w, GerRect.y + GerRect.h ) then
    MouseOverOptions := moGerman
  else if PointIsInRect( CurrentPos, SpellsRect.x, SpellsRect.y, SpellsRect.x + SpellsRect.w, SpellsRect.y + SpellsRect.h ) then
    MouseOverOptions := moSpells;
end;

procedure TGameOptions.Render;
var
  Rect, SrcRect : TSDL_Rect;
begin
  inherited;
  case  MouseOverOptions of
    moContinue:
    begin
      Rect.x := 400;
      Rect.y := 450;
      Rect.w := 300;
      Rect.h := 45;
      SDL_BlitSurface( DXContinue, nil, MainWindow.DisplaySurface, @Rect );
    end;

    moSound:
    begin
      Rect.x := ( MainWindow.DisplaySurface.w - DxTextMessage[ 0 ].w ) shr 1;
      Rect.y := 516;
      Rect.w := DxTextMessage[ 0 ].w;
      Rect.h := DxTextMessage[ 0 ].h;
      SDL_BlitSurface( DxTextMessage[ 0 ], nil, MainWindow.DisplaySurface, @Rect );
    end;

    moMusic:
    begin
      Rect.x := ( MainWindow.DisplaySurface.w - DxTextMessage[ 1 ].w ) shr 1;
      Rect.y := 516;
      Rect.w := DxTextMessage[ 1 ].w;
      Rect.h := DxTextMessage[ 1 ].h;
      SDL_BlitSurface( DxTextMessage[ 1 ], nil, MainWindow.DisplaySurface, @Rect );
    end;

    moShadowsOn, moShadowsOff:
    begin
      Rect.x := ( MainWindow.DisplaySurface.w - DxTextMessage[ 2 ].w ) shr 1;
      Rect.y := 516;
      Rect.w := DxTextMessage[ 2 ].w;
      Rect.h := DxTextMessage[ 2 ].h;
      SDL_BlitSurface( DxTextMessage[ 2 ], nil, MainWindow.DisplaySurface, @Rect );
    end;

    moSpells:
    begin
      {if Character = nil then
      begin
        Rect.x := ( MainWindow.DisplaySurface.w - DxTextMessage[ 3 ].w ) shr 1;
        Rect.y := 516;
        Rect.w := DxTextMessage[ 3 ].w;
        Rect.h := DxTextMessage[ 3 ].h;
        SDL_BlitSurface( DxTextMessage[ 3 ], nil, MainWindow.DisplaySurface, @Rect )
      end
      else}
      begin
        Rect.x := ( MainWindow.DisplaySurface.w - DxTextMessage[ 4 ].w ) shr 1;
        Rect.y := 516;
        Rect.w := DxTextMessage[ 4 ].w;
        Rect.h := DxTextMessage[ 4 ].h;
        SDL_BlitSurface( DxTextMessage[ 4 ], nil, MainWindow.DisplaySurface, @Rect )
      end
    end;
  end;

  if ShadowsOn then
  begin
    Rect.x := 560;
    Rect.y := 75;
    Rect.w := 12;
    Rect.h := 12;
    SDL_FillRect( MainWindow.DisplaySurface, @Rect, SDL_MapRGB( MainWindow.DisplaySurface.format, 255, 255, 0 ) );
  end
  else
  begin
    Rect.x := 632;
    Rect.y := 75;
    Rect.w := 12;
    Rect.h := 12;
    SDL_FillRect( MainWindow.DisplaySurface, @Rect, SDL_MapRGB( MainWindow.DisplaySurface.format, 255, 255, 0 ) );
  end;

  case LanguageSelected of
    lsEnglish :
    begin
      SrcRect.x := ( DxLangEng.w shr 1 );
      SrcRect.y := 0;
      SrcRect.w := DxLangEng.w shr 1;
      SrcRect.h := DxLangEng.h;
      SDL_BlitSurface( DxLangEng, @SrcRect, MainWindow.DisplaySurface, @EngRect );

      SrcRect.x := 0;
      SrcRect.y := 0;
      SrcRect.w := DxLangSpa.w shr 1;
      SrcRect.h := DxLangSpa.h;
      SDL_BlitSurface( DxLangSpa, @SrcRect, MainWindow.DisplaySurface, @SpaRect );

      SrcRect.x := 0;
      SrcRect.y := 0;
      SrcRect.w := DxLangGer.w shr 1;
      SrcRect.h := DxLangGer.h;
      SDL_BlitSurface( DxLangGer, @SrcRect, MainWindow.DisplaySurface, @GerRect );
    end;

    lsSpanish :
    begin
      SrcRect.x := 0;
      SrcRect.y := 0;
      SrcRect.w := DxLangEng.w shr 1;
      SrcRect.h := DxLangEng.h;
      SDL_BlitSurface( DxLangEng, @SrcRect, MainWindow.DisplaySurface, @EngRect );

      SrcRect.x := ( DxLangSpa.w shr 1 );
      SrcRect.y := 0;
      SrcRect.w := DxLangSpa.w shr 1;
      SrcRect.h := DxLangSpa.h;
      SDL_BlitSurface( DxLangSpa, @SrcRect, MainWindow.DisplaySurface, @SpaRect );

      SrcRect.x := 0;
      SrcRect.y := 0;
      SrcRect.w := DxLangGer.w shr 1;
      SrcRect.h := DxLangGer.h;
      SDL_BlitSurface( DxLangGer, @SrcRect, MainWindow.DisplaySurface, @GerRect );
    end;

    lsGerman :
    begin
      SrcRect.x := 0;
      SrcRect.y := 0;
      SrcRect.w := DxLangEng.w shr 1;
      SrcRect.h := DxLangEng.h;
      SDL_BlitSurface( DxLangEng, @SrcRect, MainWindow.DisplaySurface, @EngRect );

      SrcRect.x := 0;
      SrcRect.y := 0;
      SrcRect.w := DxLangSpa.w shr 1;
      SrcRect.h := DxLangSpa.h;
      SDL_BlitSurface( DxLangSpa, @SrcRect, MainWindow.DisplaySurface, @SpaRect );

      SrcRect.x := ( DxLangGer.w shr 1 );
      SrcRect.y := 0;
      SrcRect.w := DxLangGer.w shr 1;
      SrcRect.h := DxLangGer.h;
      SDL_BlitSurface( DxLangGer, @SrcRect, MainWindow.DisplaySurface, @GerRect );
    end;

  end;

  //Sound FX
  Rect.x := 116;
  Rect.y := 92;
  Rect.w := SoundPos - 115;
  Rect.h := 13;
  SDL_FillRect( MainWindow.DisplaySurface, @Rect, SDL_MapRGB( MainWindow.DisplaySurface.format, 255, 255, 0 ) );


  Rect.x := SoundPos - ( DXVolumeSlider.w shr 1 );
  Rect.y := 103;
  Rect.w := DXVolumeSlider.w;
  Rect.h := DXVolumeSlider.h;
  SDL_BlitSurface( DXVolumeSlider, nil, MainWindow.DisplaySurface, @Rect );


  //Music
  Rect.x := 116;
  Rect.y := 175;
  Rect.w := MusicPos - 115;
  Rect.h := 13;
  SDL_FillRect( MainWindow.DisplaySurface, @Rect, SDL_MapRGB( MainWindow.DisplaySurface.format, 255, 255, 0 ) );

  Rect.x := MusicPos - ( DXVolumeSlider.w shr 1 );
  Rect.y := 184;
  Rect.w := DXVolumeSlider.w;
  Rect.h := DXVolumeSlider.h;
  SDL_BlitSurface( DXVolumeSlider, nil, MainWindow.DisplaySurface, @Rect );

  // Needed for spell selection
  //SDL_SetAlpha( DXBackHighlight, SDL_RLEACCEL or SDL_SRCALPHA, 64 );
  //SDL_BlitSurface( DXBackHighlight, nil, MainWindow.DisplaySurface, @SpellsRect );
end;

end.
