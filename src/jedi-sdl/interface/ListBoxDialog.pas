unit ListBoxDialog;
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
  $Log: ListBoxDialog.pas,v $
  Revision 1.2  2005/06/02 22:51:54  savage
  More Cross-Platform additions and amendments

  Revision 1.1  2005/05/06 08:18:55  savage
  ListBoxDialog used in New GUI

  Revision 1.1  2004/10/06 22:48:46  savage
  Changes required to make use of YesNoDialog

  Revision 1.1  2004/09/30 22:49:20  savage
  Initial Game Interface units.


}
{******************************************************************************}

interface

uses
  Classes,
  sdl,
  sdlwindow,
  sdlgameinterface,
  SiegeInterfaces;

type
  TListItem = record
    FInfo : PSDL_Surface;
    FText : PSDL_Surface;
  end;

  TListBoxDialog = class( TSimpleSoAInterface )
  private
    FSelectedIndex: integer;
    FX, FY: integer;
    procedure SetSelectedIndex(const Value: integer);
  public
    ListItems: array of TListItem;
    constructor Create( const aMainWindow : TSDL2DWindow; aX, aY : integer ); reintroduce;
    property SelectedIndex : integer read FSelectedIndex write SetSelectedIndex;
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

{ TListBoxDialog }

constructor TListBoxDialog.Create(const aMainWindow: TSDL2DWindow; aX, aY: integer);
begin
  inherited Create( aMainWindow );
  FX := aX;
  FY := aY; 
end;

procedure TListBoxDialog.FreeSurfaces;
begin

  inherited;
end;

procedure TListBoxDialog.KeyDown( var Key : TSDLKey; Shift : TSDLMod; unicode : UInt16 );
begin
  inherited;
  case Key of
    SDLK_RETURN, SDLK_KP_ENTER :
      begin
        SelectedIndex := 0;
      end;

    SDLK_ESCAPE :
      begin
        SelectedIndex := -1;
      end;
  end;
  MainWindow.Rendering := false;
end;

procedure TListBoxDialog.LoadSurfaces;
var
  Flags : Cardinal;
begin
  inherited;
  Flags := SDL_SRCCOLORKEY or SDL_RLEACCEL or SDL_HWACCEL;

  DXBack := SDL_LoadBMP( PChar( SoASettings.InterfacePath + DIR_SEP + SoASettings.LanguagePath + DIR_SEP + 'chaChooseBox.bmp' ) );
  SDL_SetColorKey( DXBack, Flags, SDL_MapRGB( DXBack.format, 0, 255, 255 ) );
end;

procedure TListBoxDialog.MouseDown( Button : Integer; Shift : TSDLMod; CurrentPos : TPoint );
begin
  inherited;
  // Clicked on 1 option
  if PointIsInRect( CurrentPos, 304, 318, 357, 350 ) then
  begin 
    SelectedIndex := 0;
  end
  // Clicked on 2 option
  else if PointIsInRect( CurrentPos, 440, 318, 492, 350 ) then
  begin
    SelectedIndex := 1;
  end
  // Clicked on 3 option
  else if PointIsInRect( CurrentPos, 440, 318, 492, 350 ) then
  begin
    SelectedIndex := 2;
  end
  // Clicked on 4 option
  else if PointIsInRect( CurrentPos, 440, 318, 492, 350 ) then
  begin
    SelectedIndex := 3;
  end
  // Clicked OK
  else if PointIsInRect( CurrentPos, 465, 59, 465 + 123, 59 + 181 ) then
  begin
    
  end;
end;

procedure TListBoxDialog.MouseMove( Shift : TSDLMod; CurrentPos, RelativePos : TPoint );
begin
  inherited;

end;

procedure TListBoxDialog.Render;
var
  InfoRect, TextRect, lRect : TSDL_Rect;
  i : integer;
begin
  lRect.x := FX;
  lRect.y := FY;
  lRect.w := DXBack.w;
  lRect.h := DXBack.h;
  SDL_BlitSurface( DXBack, nil, MainWindow.DisplaySurface, @lRect );

  TextRect.y := FY + 10;
  for i := Low( ListItems ) to High( ListItems ) do
  begin
    TextRect.x := FX + 20;
    TextRect.y := TextRect.y + ListItems[ i ].FText.h + 2;
    TextRect.w := ListItems[ i ].FText.w;
    TextRect.h := ListItems[ i ].FText.h;

    SDL_BlitSurface( ListItems[ i ].FText, nil, MainWindow.DisplaySurface, @TextRect );
  end;

  InfoRect.x := 488;
  InfoRect.y := 158;
  InfoRect.w := ListItems[ 0 ].FInfo.w;
  InfoRect.h := ListItems[ 0 ].FInfo.h;

  SDL_BlitSurface( ListItems[ 0 ].FInfo, nil, MainWindow.DisplaySurface, @InfoRect );
end;

procedure TListBoxDialog.SetSelectedIndex(const Value: integer);
begin
  FSelectedIndex := Value;
  MainWindow.Rendering := false;
end;

end.
