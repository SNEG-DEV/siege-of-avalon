unit GameJournal;
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
  $Log: GameJournal.pas,v $
  Revision 1.6  2005/06/01 20:24:27  savage
  Fix for Linux case sensitivity issues

  Revision 1.5  2005/05/25 23:15:42  savage
  Latest Changes

  Revision 1.4  2005/05/13 12:33:15  savage
  Various Changes and bug fixes. Main work on the NewGame screen.

  Revision 1.3  2005/05/11 13:38:29  savage
  Fix for GameOption Sliders and tidying up the GameJournal and AdventureLog files.

  Revision 1.2  2005/05/10 14:12:48  savage
  Latest Enhancments and bug fixes

  Revision 1.1  2004/09/30 22:49:20  savage
  Initial Game Interface units.


}
{******************************************************************************}

interface

uses
  sdl,
  SiegeInterfaces,
  AdventureLog;

type
  TGameJournal = class( TSimpleSoAInterface )
  private
    CurrentLogIndex, StartLogIndex : integer;
    JournalLog : TAdventureLog;
    TextMessage : array[ 0..1 ] of string;
    DxPageNumbers : PSDL_Surface;
    procedure NextPage;
    procedure PreviousPage;
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
  {$IFDEF ver120}
  FileCtrl,
  {$ENDIF}
  Classes,
  SysUtils,
  logger,
  globals,
  GameMainMenu;

{ TGameJournal }

procedure TGameJournal.FreeSurfaces;
begin
  JournalLog.Free;
  
  ExText.Close;
  inherited;
end;

procedure TGameJournal.KeyDown( var Key : TSDLKey; Shift : TSDLMod;
  unicode : UInt16 );
const
  FailName : string = 'TGameJournal.KeyDown';
begin
  inherited;
  try
    case Key of
      SDLK_ESCAPE :
        begin
          NextGameInterface := TMainMenu;
          MainWindow.Rendering := false;
        end;

      SDLK_RIGHT, SDLK_PAGEDOWN :
      begin
        NextPage;
      end;

      SDLK_LEFT, SDLK_PAGEUP :
      begin
        PreviousPage;
      end;
    end;
  except
    on E: Exception do
      Log.LogError( E.Message, FailName );
  end;
end;

procedure TGameJournal.LoadSurfaces;
const
  FailName : string = 'TGameJournal.LoadSurfaces';
  Flags : Cardinal = SDL_SRCCOLORKEY or SDL_RLEACCEL or SDL_HWACCEL;
var
  i : integer;
  List : TStringList;
  C : TSDL_Color;
begin
  inherited;
  try
    StartLogIndex := -1;

    ExText.Open( 'Journal' );
    for i := 0 to 1 do
    begin
      TextMessage[ i ] := ExText.GetText( 'Message' + inttostr( i )  );
    end;

    JournalLog := TAdventureLog.Create;
    List := TStringList.Create;
    try
      List.CommaText := SoASettings.History;
      for i := 0 to List.Count - 1 do
      begin
        JournalLog.AddLogEntry( List.Strings[ i ] + '.jrn' );
      end;
    finally
      List.Free;
    end;

    if JournalLog.LogFileList.count - 1 > StartLogIndex then
      inc( StartLogIndex );

    if ( SoASettings.JournalFont = 1 ) and DirectoryExists( SoASettings.ArtPath + '/JournalAlt/' ) then
      JournalLog.LogDirectory := SoASettings.ArtPath + '/JournalAlt/'
    else
      JournalLog.LogDirectory := SoASettings.ArtPath + '/Journal/';

    CurrentLogIndex := StartLogIndex; //JournalLog.LogFileList.count-1;
    if CurrentLogIndex >= JournalLog.LogFileList.count then
        CurrentLogIndex := JournalLog.LogFileList.count - 1;

    DXBack := SDL_LoadBMP( PChar( JournalLog.LogDirectory + ChangeFileExt( JournalLog.LogFileList.strings[ CurrentLogIndex ], '.bmp' ) ) );

    C.r := 0;
    C.g := 0;
    C.b := 0;
    GameFont.ForeGroundColour := C;
    C.r := 168;
    C.g := 144;
    C.b := 112;
    GameFont.BackGroundColour := C;
    {if SoASettings.UseSmallFont then
      GameFont.FontSize := 13
    else}
    GameFont.FontSize := 18;
    
    DXPagenumbers := GameFont.DrawText( TextMessage[ 0 ] + intToStr( CurrentLogIndex + 1 ) + TextMessage[ 1 ] + IntToStr( JournalLog.LogFileList.count ) );
    SDL_SetColorKey( DXPagenumbers, Flags, SDL_MapRGB( DXPagenumbers.format, 168, 144, 112 ) );
  except
    on E: Exception do
      Log.LogError( E.Message, FailName );
  end;
end;

procedure TGameJournal.MouseDown( Button : Integer; Shift : TSDLMod; CurrentPos : TPoint );
const
  FailName : string = 'TGameJournal.MouseDown';
  Flags : Cardinal = SDL_SRCCOLORKEY or SDL_RLEACCEL or SDL_HWACCEL;
begin
  inherited;
  try
    if PointIsInRect( CurrentPos, 582, 575, 659, 596 ) then
    begin //prev
      PreviousPage;
    end
    else if PointIsInRect( CurrentPos, 681, 575, 721, 596 ) then
    begin //next
      NextPage;
    end
    else if PointIsInRect( CurrentPos, 746, 575, 786, 596 ) then
    begin //exit
      NextGameInterface := TMainMenu;
      MainWindow.Rendering := false;
    end;
  except
    on E: Exception do
      Log.LogError( E.Message, FailName );
  end;
end;

procedure TGameJournal.MouseMove( Shift : TSDLMod; CurrentPos, RelativePos : TPoint );
const
  FailName : string = 'TGameJournal.MouseMove';
begin
  inherited;

end;

procedure TGameJournal.NextPage;
const
  FailName : string = 'TGameJournal.NextPage';
  Flags : Cardinal = SDL_SRCCOLORKEY or SDL_RLEACCEL or SDL_HWACCEL;
begin
  try
    if CurrentLogIndex < JournalLog.LogFileList.count - 1 then
    begin
      Inc( CurrentLogIndex );
      SDL_FreeSurface( DXBack );
      DXBack := SDL_LoadBMP( PChar( JournalLog.LogDirectory + ChangeFileExt( JournalLog.LogFileList.strings[ CurrentLogIndex ], '.bmp' ) ) );
      SDL_FreeSurface( DXPagenumbers );
      DXPagenumbers := GameFont.DrawText( TextMessage[ 0 ] + intToStr( CurrentLogIndex + 1 ) + TextMessage[ 1 ] + IntToStr( JournalLog.LogFileList.count ) );
      SDL_SetColorKey( DXPagenumbers, Flags, SDL_MapRGB( DXPagenumbers.format, 168, 144, 112 ) );
    end;
  except
    on E: Exception do
      Log.LogError( E.Message, FailName );
  end;
end;

procedure TGameJournal.PreviousPage;
const
  FailName : string = 'TGameJournal.PreviousPage';
  Flags : Cardinal = SDL_SRCCOLORKEY or SDL_RLEACCEL or SDL_HWACCEL;
begin
  try
    if CurrentLogIndex > 0 then
    begin
      Dec( CurrentLogIndex );
      SDL_FreeSurface( DXBack );
      DXBack := SDL_LoadBMP( PChar( JournalLog.LogDirectory + ChangeFileExt( JournalLog.LogFileList.strings[ CurrentLogIndex ], '.bmp' ) ) );
      SDL_FreeSurface( DXPagenumbers );
      DXPagenumbers := GameFont.DrawText( TextMessage[ 0 ] + intToStr( CurrentLogIndex + 1 ) + TextMessage[ 1 ] + IntToStr( JournalLog.LogFileList.count ) );
      SDL_SetColorKey( DXPagenumbers, Flags, SDL_MapRGB( DXPagenumbers.format, 168, 144, 112 ) );
    end;
  except
    on E: Exception do
      Log.LogError( E.Message, FailName );
  end;
end;

procedure TGameJournal.Render;
const
  FailName : string = 'TGameJournal.Render';
var
  Rect : TSDL_Rect;
begin
  inherited;
  try
    if not SoASettings.NoPageNumbers then
    begin
      Rect.x := 20;
      Rect.y := 570;
      Rect.w := DxPageNumbers.w;
      Rect.h := DxPageNumbers.h;
      SDL_BlitSurface( DxPageNumbers, nil, MainWindow.DisplaySurface, @Rect );
    end;
  except
    on E: Exception do
      Log.LogError( E.Message, FailName );
  end;
end;

end.
