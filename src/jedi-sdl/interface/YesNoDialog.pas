unit YesNoDialog;
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
  $Log: YesNoDialog.pas,v $
  Revision 1.4  2005/06/02 22:51:54  savage
  More Cross-Platform additions and amendments

  Revision 1.3  2005/06/01 20:24:27  savage
  Fix for Linux case sensitivity issues

  Revision 1.2  2005/05/15 14:17:17  savage
  Fade our background when dialog has the focus.

  Revision 1.1  2004/10/06 22:48:46  savage
  Changes required to make use of YesNoDialog

  Revision 1.1  2004/09/30 22:49:20  savage
  Initial Game Interface units.


}
{******************************************************************************}

interface

uses
  sdl,
  sdlgameinterface,
  SiegeInterfaces;

type
  TDialogResult = ( drYes, drNo );
  
  TYesNoDialog = class( TSimpleSoAInterface )
  private
    DxTextMessage, DxChooseBox : PSDL_Surface;
    FQuestionText: string;
    FDialogResult: TDialogResult;
    procedure SetDialogResult(const Value: TDialogResult);
  public
    property QuestionText : string read FQuestionText write FQuestionText;
    property DialogResult : TDialogResult read FDialogResult write SetDialogResult;
    procedure FreeSurfaces; override;
    procedure LoadSurfaces; override;
    procedure Render; override;
    procedure MouseDown( Button : Integer; Shift : TSDLMod; CurrentPos : TPoint ); override;
    procedure MouseMove( Shift : TSDLMod; CurrentPos : TPoint; RelativePos : TPoint ); override;
    procedure KeyDown( var Key : TSDLKey; Shift : TSDLMod; unicode : UInt16 ); override;
  end;

implementation

uses
  xplatformutils,
  globals,
  GameMainMenu;

{ TYesNoDialog }

procedure TYesNoDialog.FreeSurfaces;
begin
  SDL_FreeSurface( DxTextMessage );
  SDL_FreeSurface( DxChooseBox );
  inherited;
end;

procedure TYesNoDialog.KeyDown( var Key : TSDLKey; Shift : TSDLMod; unicode : UInt16 );
begin
  inherited;
  case Key of
    SDLK_RETURN, SDLK_KP_ENTER :
      begin
        DialogResult := drYes;
      end;

    SDLK_ESCAPE :
      begin
        DialogResult := drNo;
      end;
  end;
end;

procedure TYesNoDialog.LoadSurfaces;
const
  Flags : Cardinal= SDL_SRCCOLORKEY or SDL_RLEACCEL or SDL_HWACCEL;
var
  C : TSDL_Color;
  WindowRect : TSDL_Rect;
begin
  inherited;

  WindowRect.x := 0;
  WindowRect.y := 0;
  WindowRect.w := MainWindow.DisplaySurface.w;
  WindowRect.h := MainWindow.DisplaySurface.h;

  DXBack := SDL_CreateRGBSurface( SDL_SWSURFACE, WindowRect.w, WindowRect.h,
      MainWindow.DisplaySurface.format.BitsPerPixel, MainWindow.DisplaySurface.format.RMask, MainWindow.DisplaySurface.format.GMask,
      MainWindow.DisplaySurface.format.BMask, MainWindow.DisplaySurface.format.AMask );

  SDL_FillRect( DXBack, @WindowRect, SDL_MapRGB( MainWindow.DisplaySurface.format, 128, 128, 128 ) );

  SDL_SetAlpha( DXBack, SDL_RLEACCEL or SDL_SRCALPHA, 16 );

  DxChooseBox := SDL_LoadBMP( PChar( SoASettings.InterfacePath + DIR_SEP + SoASettings.LanguagePath + DIR_SEP + 'ldChoosebox.bmp' ) );
  SDL_SetColorKey( DxChooseBox, Flags, SDL_MapRGB( DXBack.format, 0, 255, 255 ) );

  C.r := 231;
  C.g := 156;
  C.b := 0;
  GameFont.ForeGroundColour := C;
  C.r := 0;
  C.g := 0;
  C.b := 0;
  GameFont.BackGroundColour := C;
  GameFont.FontSize := 18;

  DxTextMessage := GameFont.DrawText( FQuestionText, 261, 55 );
  SDL_SetColorKey( DxTextMessage, Flags, SDL_MapRGB( DxTextMessage.format, 0, 0, 0 ) );
end;

procedure TYesNoDialog.MouseDown( Button : Integer; Shift : TSDLMod; CurrentPos : TPoint );
begin
  inherited;
  if PointIsInRect( CurrentPos, 304, 318, 357, 350 ) then
  begin //Yes pressed- quit game
    DialogResult := drYes;
  end
  else if PointIsInRect( CurrentPos, 440, 318, 492, 350 ) then
  begin //No pressed- just show screen
    DialogResult := drNo;
  end;
end;

procedure TYesNoDialog.MouseMove( Shift : TSDLMod; CurrentPos,
  RelativePos : TPoint );
begin
  inherited;

end;

procedure TYesNoDialog.Render;
var
  Rect : TSDL_Rect;
begin
  inherited;
  Rect.x := ( ( MainWindow.DisplaySurface.w - DxTextMessage.w ) shr 1 ) - 20;
  Rect.y := ( ( MainWindow.DisplaySurface.h - DxTextMessage.h ) shr 1 ) - 30;
  Rect.w := DxChooseBox.w;
  Rect.h := DxChooseBox.h;  
  SDL_BlitSurface( DxChooseBox,  nil, MainWindow.DisplaySurface, @Rect );
  
  Rect.x := ( MainWindow.DisplaySurface.w - DxTextMessage.w ) shr 1;
  Rect.y := ( ( MainWindow.DisplaySurface.h - DxTextMessage.h ) shr 1 ) - 20;
  Rect.w := DxTextMessage.w;
  Rect.h := DxTextMessage.h;
  SDL_BlitSurface( DxTextMessage, nil, MainWindow.DisplaySurface, @Rect );
end;

procedure TYesNoDialog.SetDialogResult(const Value: TDialogResult);
begin
  FDialogResult := Value;
  MainWindow.Rendering := false;
end;

end.
