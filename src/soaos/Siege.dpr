program Siege;
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

uses
  Forms,
  Windows,
  SysUtils,
  IniFiles,
  Controls,
  AniDemo in 'engine\AniDemo.pas' {frmMain},
  Loader in 'engine\Loader.pas',
  Resource in 'engine\Resource.pas',
  Engine in 'engine\Engine.pas',
  AdventureLog in 'engine\AdventureLog.pas',
  DFX in 'graphics\DFX.pas',
  GameText in 'engine\GameText.pas',
  Character in 'ai\Character.pas',
  AI1 in 'ai\AI1.pas',
  UndeadAI in 'ai\UndeadAI.pas',
  BasicHumanoidAI in 'AI\BasicHumanoidAI.pas',
  MiscAI in 'ai\MiscAI.pas',
  WolfAI in 'ai\WolfAI.pas',
  Anigrp30 in 'engine\Anigrp30.pas',
  AniDec30 in 'engine\AniDec30.pas',
  String32 in 'engine\string32.pas',
  DXEffects in 'graphics\DXEffects.pas',
  DXUtil in 'graphics\DXUtil.pas',
  LogFile in 'Engine\LogFile.pas',
  AStar in 'ai\AStar.pas',
  Inventory in 'interface\Inventory.pas',
  Display in 'interface\Display.pas',
  CharCreation in 'interface\CharCreation.pas',
  Award in 'interface\Award.pas',
  Parts in 'engine\Parts.pas',
  digifx in 'graphics\digifx.pas',
  DirectX in 'graphics\DirectX.pas',
  DXRender in 'graphics\DXRender.pas',
  Converse in 'interface\Converse.pas',
  Titles in 'engine\Titles.pas',
  Map in 'interface\Map.pas',
  Journal in 'interface\Journal.pas',
  LoaderBox in 'engine\LoaderBox.pas',
  Effects in 'engine\Effects.pas',
  Spells in 'engine\Spells.pas',
  Spells1 in 'engine\Spells1.pas',
  Intro in 'interface\Intro.pas',
  SaveFile in 'engine\SaveFile.pas',
  Security in 'engine\Security.pas',
  ItemDatabase in 'engine\ItemDatabase.pas',
  AddKickNPC in 'interface\AddKickNPC.pas',
  LoadGame in 'interface\LoadGame.pas',
  LootCorpse in 'interface\LootCorpse.pas',
  LogScreen in 'interface\LogScreen.pas',
  Merchant in 'interface\Merchant.pas',
  Sound in 'sound\Sound.pas',
  Midi in 'sound\Midi.pas',
  MP3 in 'sound\MP3.pas',
  Music in 'sound\Music.pas',
  NPCBehavior in 'interface\NPCBehavior.pas',
  ShowGraphic in 'interface\Showgraphic.pas',
  MousePtr in 'engine\MousePtr.pas',
  ObjInventory in 'interface\ObjInventory.pas',
  OpenAnim in 'interface\OpenAnim.pas',
  Options in 'interface\Options.pas',
  Scroll in 'engine\Scroll.pas',
  Statistics in 'interface\Statistics.pas',
  strFunctions in 'engine\strFunctions.pas',
  Transit in 'interface\Transit.pas';


{$R *.RES}

const
  MUTEXNAME = 'DigitalTomeSiegeOfAvalon';

var
  hMutex : THandle;
  zAppName : array[ 0..512 ] of Char;
  zCurDir : array[ 0..255 ] of Char;
  WorkDir : string;
  MovieSwitches : string;
  STARTUPINFO : TStartupInfo;
  ProcessInfo : TProcessInformation;
  SiegeIni : TIniFile;

procedure PlayOpeningMovie;
begin
  SiegeIni := nil;
  SiegeIni := TIniFile.Create( ExtractFilePath( Application.ExeName ) + 'siege.ini' );
  try

    OpeningMovie := SiegeIni.ReadString( 'Settings', 'MoviePath', ExtractFilePath( Application.ExeName ) + 'Movies' ) + '\' + SiegeIni.ReadString( 'Settings', 'OpeningMovie', 'siegeopening.bik' );
    ClosingMovie := SiegeIni.ReadString( 'Settings', 'MoviePath', ExtractFilePath( Application.ExeName ) + 'Movies' ) + '\' + SiegeIni.ReadString( 'Settings', 'ClosingMovie', 'siegeclosing.bik' );
    MovieSwitches := UpperCase( SiegeIni.ReadString( 'Settings', 'MovieSwitches', '/R/C/U1/I102/D9/B0' ) );

    Screen.Cursor := crNone;
    Application.ProcessMessages;

    if FileExists( OpeningMovie ) and ( LowerCase( SiegeIni.ReadString( 'Settings', 'ShowIntro', 'true' ) ) = 'true' ) then
    begin
      //Begin the opening Movie
      StrPCopy( zAppName, ExtractFilePath( Application.ExeName ) + 'BinkPlay.exe' + ' ' + OpeningMovie + ' ' + MovieSwitches + '/P' );
      GetDir( 0, WorkDir );
      StrPCopy( zCurDir, WorkDir );
      FillChar( STARTUPINFO, SizeOf( STARTUPINFO ), #0 );
      STARTUPINFO.cb := SizeOf( STARTUPINFO );

      STARTUPINFO.dwFlags := STARTF_USESHOWWINDOW;
      STARTUPINFO.wShowWindow := 1;
      if CreateProcess( nil, zAppName, nil, nil, False, CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS
        , nil, nil, STARTUPINFO, ProcessInfo ) then
        WaitForSingleObject( ProcessInfo.hProcess, INFINITE );
    end;
  finally
    if Assigned( SiegeIni ) then
      SiegeIni.Free;
    SiegeIni := nil;
  end;

  zAppName := '';
  WorkDir := '';
  zCurDir := '';
  Screen.Cursor := crDefault;
end;

procedure PlayClosingMovie;
begin
  //Begin the closing Movie
  Screen.Cursor := crNone;
  Application.ProcessMessages;
  if FileExists( ClosingMovie ) and bPlayClosingMovie then
  begin
    StrPCopy( zAppName, ExtractFilePath( Application.ExeName ) + 'BinkPlay.exe' + ' ' + ClosingMovie + ' ' + MovieSwitches );
    GetDir( 0, WorkDir );
    StrPCopy( zCurDir, WorkDir );
    FillChar( STARTUPINFO, SizeOf( STARTUPINFO ), #0 );
    STARTUPINFO.cb := SizeOf( STARTUPINFO );

    STARTUPINFO.dwFlags := STARTF_USESHOWWINDOW;
    STARTUPINFO.wShowWindow := 1;
    if CreateProcess( nil, zAppName, nil, nil, False, CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS
      , nil, nil, STARTUPINFO, ProcessInfo ) then
      WaitForSingleObject( ProcessInfo.hProcess, INFINITE );
  end;
  Screen.Cursor := crDefault;
end;

begin

  // A means of assuring that only one copy of game runs at a time, but does it REALLY work? What is runtime error 216?
  hMutex := OpenMutex( MUTEX_ALL_ACCESS, False, MUTEXNAME );
  if hMutex <> 0 then
  begin
    CloseHandle( hMutex );
    Exit;
  end;
  hMutex := CreateMutex( nil, True, PChar( MUTEXNAME ) );

  PlayOpeningMovie;
  bPlayClosingMovie := False; // Game must force to true to show closing movie

  Application.Initialize;
  Application.HelpFile := 'help.htm';
  Application.Title := 'Siege of Avalon';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;

  ReleaseMutex( hMutex );
  CloseHandle( hMutex );

  PlayClosingMovie;

end.

