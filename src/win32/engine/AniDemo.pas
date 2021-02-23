unit AniDemo;
(*
  Siege Of Avalon : Open Source Edition

  Portions created by Digital Tome L.P. Texas USA are
  Copyright �1999-2000 Digital Tome L.P. Texas USA
  All Rights Reserved.

  Portions created by Team SOAOS are
  Copyright (C) 2003 - Team SOAOS.

  Portions created by Steffen Nyeland are
  Copyright (C) 2019 - Steffen Nyeland.

  Contributor(s):
  Dominique Louis <Dominique@SavageSoftware.com.au>
  Steffen Nyeland

  You may retrieve the latest version of this file at:
  https://github.com/SteveNew/Siege-of-Avalon-Open-Source

  The contents of this file maybe used with permission, subject to
  the GNU Lesser General Public License Version 2.1 (the "License"); you may
  not use this file except in compliance with the License. You may
  obtain a copy of the License at https://opensource.org/licenses/LGPL-2.1

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  Description:

  Requires: Delphi 10.3.3 or later

  Revision History:
  - 13 Jul 2003 - DL: Initial Upload to CVS
  - 10 Mar 2019 - SN: Forked on GitHub
  see git repo afterwards

*)

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.IOUtils,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  System.IniFiles,
  LogFile,
  Spells,
//  Winapi.DirectDraw,
  DirectX,
  DXUtil,
  DXEffects,
  SoAOS.Types,
  SoAOS.AI.Types,
  SoAOS.Intrface.Popup,
  SoAOS.Map,
  Character,
  Loader,
  Resource,
  Display,
  Inventory,
  Converse,
  Gametext,
  Statistics,
  ObjInventory,
  Map,
  Merchant,
  AdventureLog,
  SoAOS.Spells,
  SoAOS.Intrface.Dialogs.Journal,
  SoAOS.Intrface.Dialogs.NewCharacter,
  SoAOS.Intrface.Dialogs.Mainmenu,
  SoAOS.Intrface.Dialogs.Options,
  NPCBehavior,
  LoaderBox,
  Award,
  SoAOS.Intrface.Dialogs.LoadSaveGame,
  LootCorpse,
  OpenAnim,
  SoAOS.Intrface.Dialogs.HelpCredit,
  LogScreen,
  SoAOS.Intrface.Transit,
  SoAOS.Animation,
  AddKickNPC;

const
  WM_StartMainMenu = WM_USER + 1;
  WM_StartOptions = WM_USER + 2;
  WM_StartLoad = WM_USER + 3;
  WM_StartSave = WM_USER + 4;
  WM_StartNew = WM_USER + 5;
  WM_StartIntro = WM_USER + 6;
  WM_StartCredits = WM_USER + 7;
  WM_Done = WM_USER + 8;
  WM_StartTimer = WM_USER + 9;
  WM_EndTimer = WM_USER + 10;
  WM_StartTransit = WM_USER + 11;

var
  DlgProgress : TLoaderBox;
  InterfacePath : string;
  InterfaceLanguagePath : string;
  MapPath : AnsiString;
  Language : string;
  DeviceDriverIndex : Integer;
  INICodepage : Integer;
  MaxPartyMembers : Integer;
  bPlayClosingMovie : Boolean;
  OpeningMovie : string;
  ClosingMovie : string;

type
  TfrmMain = class( TForm )
    Timer2 : TTimer;
    Timer3 : TTimer;
    procedure FormShow( Sender : TObject );
    procedure Timer1Timer( Sender : TObject );
    procedure AniView1MouseDown( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer );
    procedure AniView1AfterDisplay( Sender : TObject );
    procedure AniView1BeforeDisplay( Sender : TObject );
    procedure FormDestroy( Sender : TObject );
    procedure FormCreate( Sender : TObject );
    procedure FormMouseDown( Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer );
    procedure Timer2Timer( Sender : TObject );
    procedure WMStartMainMenu( var Message : TWMNoParams ); message WM_StartMainMenu;
    procedure WMStartOptions( var Message : TWMNoParams ); message WM_StartOptions;
    procedure WMStartNew( var Message : TWMNoParams ); message WM_StartNew;
    procedure WMStartLoad( var Message : TWMNoParams ); message WM_StartLoad;
    procedure WMStartSave( var Message : TWMNoParams ); message WM_StartSave;
    procedure WMStartIntro( var Message : TWMNoParams ); message WM_StartIntro;
    procedure WMStartCredits( var Message : TWMNoParams ); message WM_StartCredits;
    procedure WMDone( var Message : TWMNoParams ); message WM_Done;
    procedure WMSetFocus( var Message : TMessage ); message WM_SETFOCUS;
    procedure WMKillFocus( var Message : TMessage ); message WM_KILLFOCUS;
    procedure WMStartTimer( var Message : TWMNoParams ); message WM_StartTimer;
    procedure WMStartTransit( var Message : TWMNoParams ); message WM_StartTransit;
    procedure FormMouseMove( Sender : TObject; Shift : TShiftState; X,
      Y : Integer );
    procedure Timer3Timer( Sender : TObject );
    procedure ShowEnding;
    procedure ShowHistroy;
    procedure CloseEnding( Sender : TObject );
    procedure CloseHistory( Sender : TObject );
  private
    AdventureLog1 : TAdventureLog;
    HistoryLog : TAdventureLog;
    SoundCount : LongWord;
    Seconds : LongWord;

    Mana : Double;
    Drain : Double;
    Life : Double;
    Wounds : Double;
    FSpellBarActive : Boolean;
    LoadNewLevel : Integer;
    NewLVLFile : string;
    NewScene : string;
    NewStartingPoint : string;
    Spells : array[ 0..35 ] of TSpell;
    NoFadeIn : Boolean;
    NextSong : string;
    SongCounter : Longint;
    SoundOK : Boolean;
//    Spinner : Integer;
    NewGame : Boolean;

    OverlayR : IDirectDrawSurface;
    ManaEmpty : IDirectDrawSurface;
    LifeEmpty : IDirectDrawSurface;
    SpellBar : IDirectDrawSurface;
    SpellGlyphs : IDirectDrawSurface;
    NoSpellIcon : IDirectDrawSurface;
    HelpBox : IDirectDrawSurface;
    FCurrentTheme : string;

    imgGlow : TBitmap;
    imgHelp : TBitmap;
    imgCombat : TBitmap;
    imgAutoTransparent : TBitmap;
    imgSidebar : TBitmap;
    imgSpellBar : TBitmap;


    LastFileSaved : string;
    UseVideoRAM : Boolean;
    UseTimer : Boolean;
    QuickMessage : string;
    PrevQuickMessage : string;

    QuickMessageCount : Integer;
    Initialized : Boolean;
    NeedToReload : Boolean;
    KeepTravelList : string;
    Transitionscreen : string;
    DefaultTransition : string;
    LastSpellName : string;

    PrevTriggerID : Smallint;
    Zonable : Boolean;

    FActive : Boolean;
    InTimerLoop : Boolean;
    Interval : Integer;
    NewPartyMember : TCharacter;
    SongDuration : Integer;

    MapName : string;
    trNewFile, trSceneName, trStartingPoint, trTransition, trTargetList : string;
    Popup : TPopup;


    procedure OpenDialog( Dialog : TDisplay; CloseProcedure : TNotifyEvent );
    procedure CloseCreateDialog( Sender : TObject );
    procedure CloseIntroDialog( Sender : TObject );
    procedure CloseOptions( Sender : TObject );
    procedure CloseLoad( Sender : TObject );
    procedure CloseSave( Sender : TObject );
    procedure CloseIntroJournal( Sender : TObject );
    procedure CloseRosterDialog( Sender : TObject );
    procedure CloseTransit( Sender : TObject );
    procedure SaveOptions;
    procedure SetCurrentTheme( const Value : string );
    procedure AppException( Sender : TObject; E : Exception );
    procedure DrawHealthBars;
    procedure CloseShow( Sender : TObject );
    procedure AppActivate( Sender : TObject );
    procedure AppDeactivate( Sender : TObject );
    function ShouldRun( X, Y : Longint ) : Boolean;
    function IsOnZoneTile( Figure : TAniFigure ) : Boolean;
    procedure DrawRosterGuy( Character : TCharacter; X, Y : Integer );
    procedure SetActive( const Value : Boolean );
  public
    DeathScreen : string;
    DisableConsole : Boolean;
    SoundTimer : TTimer;
    FadeIn, FadeOut : Integer;
    XRayOn : Boolean;
    Paused : Boolean;
    MouseMessage : string;
    HLFigure : TAniFigure;
    OverlayB : IDirectDrawSurface;
    PauseImage : IDirectDrawSurface;
    imgBottomBar : TBitmap;
    NPCHealthBltFx : TDDBLTFX;
    NPCManaBltFx : TDDBLTFX;
    NPCBarXCoord : array[ 1..4 ] of Integer;
    DoNotRestartTimer : Boolean;
    ScreenShot : TBitmap;
    procedure CloseAllDialogs( Sender : TObject );
    procedure DrawSpellGlyphs;
    procedure DrawCurrentSpell;
    procedure FreeAll;
    procedure BeginConverse( ObjectRef : TGameObject; Conversation : string );
    procedure BeginInventory( Character : TCharacter );
    procedure BeginMerchant( Character : TCharacter );
    procedure BeginLoot( Character : TCharacter; OtherObj : TSpriteObject );
    procedure BeginObjInventory( Character : TCharacter; OtherObj : TSpriteObject );
    procedure BeginStatistics( Character : TCharacter );
    procedure BeginMap( Character : TCharacter );
    procedure BeginTitles( Character : TCharacter );
    procedure BeginNPC( Character : TCharacter );
    procedure BeginJournal;
    procedure BeginOptions( Character : TCharacter );
    procedure BeginHelp;
    procedure BeginQuestLog;
    procedure BeginAdvLog;
    procedure BeginDeath;
    procedure BeginRoster( Character : TCharacter );
    procedure BeginTransit( const NewFile, SceneName, StartingPoint, Transition, TargetList : string );
    procedure AddToParty( Figure : TAniFigure );
    procedure RemoveFromParty( Figure : TAniFigure );
    procedure ChangeFocus( Figure : TAniFigure );
    procedure PaintCharacterOnBorder( Figure : TSpriteObject; Slot : Integer );
    procedure AddLogEntry( const FileName : string );
    procedure AddQuest( const Entry : string );
    procedure AddAdventure( const Entry : string );
    procedure ClearLogGraphic;
    procedure ClearQuestGraphic;
    procedure ClearAdventureGraphic;
    procedure ShowQuickMessage( const Msg : string; Time : Integer );
    procedure ShowMouseMessage( const Msg : string );
    procedure AssignMarch;
    function LoadResources : Boolean;
    function ClearResources( PreserveResourceList : Boolean ) : Boolean;
    procedure ClearOnDemandResources;
    procedure LoadNewMap( const NewFile, SceneName, StartingPoint, Transition : string );
    procedure LoadNewMapFile;
    function LoadMapFile( New, FullLoad : Boolean ) : Boolean;
    function FindMap( const FileName : string ) : string;
    procedure InventoryDraw( Sender : TObject );
    function SaveGame : Boolean;
    function LoadGame( FullLoad : Boolean ) : Boolean;
    procedure PlaceNPCList;
    procedure CueTune( const FileList : string; Instant : Boolean );
    procedure ClearOverlayB;
    procedure SaveAGame( const Name : string );
    property CurrentTheme : string read FCurrentTheme write SetCurrentTheme;
    property Active : Boolean read FActive write SetActive;
    property SpellBarActive : Boolean read FSpellBarActive write FSpellBarActive;
  end;

var
  SetAppExStyleCount : Integer;
  ScreenMetrics : TScreenResolutionData;
  DlgRect : TDialogData;
  DlgMap : TMap;
  DlgRoster : TAddKickNPC;
  DlgTitles : TAward;
  DlgQuestLog : TQuestLog;
  DlgJournal : TJournal;
  DlgAdvLog : TAdvLog;
  DlgOptions : TOptions;
  DlgInventory : TInventory;
  DlgStatistics : TStatistics;
  DlgShow : TShowGraphic;
  DlgTransit : TTransit;

  AdjustedPartyHitPoints : Boolean;
  AdjustedCompanionAI : Boolean;  // Rucksacksepp's improved Companion AI - default to true
  frmMain : TfrmMain;

const
  TempGame = '~Temp';

procedure ForceNotReadOnly( const FileName : string );

implementation

uses
  SoAOS.Graphics.Draw,
  SoAOS.Graphics.Types,
  SoAOS.AI,
  SoAOS.AI.Helper,
  SoAOS.Intrface.KeyEvents,
  System.Types,
  DFX,
  Parts,
  Sound,
  Music,
  MP3,
  Engine,
  MousePtr,
  SaveFile;

{$R *.DFM}

var
  DlgConverse : TConverseBox;
  DlgLoot : TLootCorpse;
  DlgMerchant : TMerchant;
  DlgObjInventory : TObjInventory;


  DlgNPC : TNPCBehavior;
  DlgLoad : TLoadGame;
  DlgCreation : TCreation;
  DlgIntro : TIntro;
  DlgText : TGameText;

procedure TfrmMain.CloseAllDialogs( Sender : TObject );
var
  i : Integer;
const
  FailName : string = 'Main.CloseAllDialogs';
begin
  Log.DebugLog(FailName);
  try

    Game.LMouseButton := False;
    if ( Sender = DlgConverse ) then
    begin
      if DlgMerchant.Loaded then
      begin
        Exit;
      end
      else if Assigned( NewPartyMember ) and DlgConverse.Loaded then
      begin
        DlgConverse.Release;
        BeginRoster( NewPartyMember );
        NewPartyMember := nil;
        Exit;
      end;
    end;
    if DlgInventory.Loaded then
    begin
      DlgInventory.Release;
      i := NPCList.IndexOf( DlgInventory.Character );
      if i >= 0 then
        PaintCharacterOnBorder( DlgInventory.Character, i );
      if not DlgInventory.Character.ValidateSpells then
      begin
        DrawCurrentSpell;
      end;
    end;
    if DlgMerchant.Loaded then
    begin
      DlgMerchant.Release;
      i := NPCList.IndexOf( DlgInventory.Character );
      if i >= 0 then
        PaintCharacterOnBorder( DlgInventory.Character, i );
      if not DlgMerchant.Character.ValidateSpells then
      begin
        DrawCurrentSpell;
      end;
    end;
    if DlgLoot.Loaded then
    begin
      DlgLoot.Release;
      if not DlgLoot.Character.ValidateSpells then
      begin
        DrawCurrentSpell;
      end;
    end;
    if DlgObjInventory.Loaded then
    begin
      DlgObjInventory.Release;
      if not DlgObjInventory.Character.ValidateSpells then
      begin
        DrawCurrentSpell;
      end;
    end;
    if DlgStatistics.Loaded then
      DlgStatistics.Release;
    if DlgConverse.Loaded then
      DlgConverse.Release;
    if DlgMap.Loaded then
      DlgMap.Release;
    if DlgShow.Loaded then
      DlgShow.Release;
    if DlgTitles.Loaded then
      DlgTitles.Release;
    if DlgQuestLog.Loaded then
      DlgQuestLog.Release;
    if DlgAdvLog.Loaded then
      DlgAdvLog.Release;
    if DlgNPC.Loaded then
      DlgNPC.Release;
    if DlgRoster.Loaded then
      DlgRoster.Release;
    if DlgJournal.Loaded then
      DlgJournal.Release;
    if DlgProgress.Loaded then
      DlgProgress.Release;
    if DlgLoad.Loaded then
      DlgLoad.Release;
    if DlgIntro.Loaded then
      DlgIntro.Release;
    if DlgCreation.Loaded then
      DlgCreation.Release;
    if DlgTransit.Loaded then
      DlgTransit.Release;
    if DlgOptions.Loaded then
    begin
      DlgOptions.Release;
      SaveOptions;
    end;

    Game.OnMouseDown := AniView1MouseDown;
    Game.OnMouseMove := nil;
    Game.OnMouseUp := nil;
    OnKeyDown := TKeyEvent.FormKeyDown;
    OnMouseDown := FormMouseDown;
    OnMouseMove := FormMouseMove;
    DisableConsole := False;
    if ( Sender <> DlgConverse ) then
      ShowQuickMessage( '', 1 );
    if DoNotRestartTimer then
      DoNotRestartTimer := False
    else
      Active := True;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.OpenDialog( Dialog : TDisplay; CloseProcedure : TNotifyEvent );
const
  FailName : string = 'Main.OpenDialog';
begin
  Log.DebugLog(FailName);
  try

    Active := False;
    Paused := False;

    if DlgInventory.Loaded then
      DlgInventory.Release;
    if DlgMerchant.Loaded then
      DlgMerchant.Release;
    if DlgLoot.Loaded then
      DlgLoot.Release;
    if DlgObjInventory.Loaded then
      DlgObjInventory.Release;
    if DlgStatistics.Loaded then
      DlgStatistics.Release;
    if DlgMap.Loaded then
      DlgMap.Release;
    if DlgTitles.Loaded then
      DlgTitles.Release;
    if DlgQuestLog.Loaded then
      DlgQuestLog.Release;
    if DlgAdvLog.Loaded then
      DlgAdvLog.Release;
    if DlgNPC.Loaded then
      DlgNPC.Release;
    if DlgRoster.Loaded then
      DlgRoster.Release;
    if DlgJournal.Loaded then
      DlgJournal.Release;
    if DlgOptions.Loaded then
      DlgOptions.Release;
    if DlgLoad.Loaded then
      DlgLoad.Release;
    if DlgProgress.Loaded then
      DlgProgress.Release;
    if DlgConverse.Loaded then
      DlgConverse.Release;
    if DlgIntro.Loaded then
      DlgIntro.Release;
    if DlgShow.Loaded then
      DlgShow.Release;
    if DlgCreation.Loaded then
      DlgCreation.Release;
    if DlgTransit.Loaded then
      DlgTransit.Release;
    Game.OnMouseDown := nil;
    Game.OnMouseMove := nil;
    Game.OnMouseUp := nil;
    OnKeyDown := nil;
    Dialog.OldKeyDown := TKeyEvent.FormKeyDown;
    Dialog.pText := DlgText;
    Dialog.OnClose := CloseProcedure;
    MouseCursor.SetFrame( 37 );
    PrevTriggerID := -1;
    Dialog.Init;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

//Create new game

procedure TfrmMain.CloseCreateDialog( Sender : TObject );
var
  i, j : Integer;
  INI : TIniFile;
const
  FailName : string = 'Main.CloseCreateDialog';
begin
  Log.DebugLog(FailName);
  try

    TDisplay( Sender ).Release;
    Game.OnMouseDown := AniView1MouseDown;
    Game.OnMouseMove := nil;
    Game.OnMouseUp := nil;
    OnKeyDown := TKeyEvent.FormKeyDown;
    OnMouseDown := FormMouseDown;
    OnMouseMove := FormMouseMove;

    if TCreation( Sender ).Cancel then
    begin
      with Sender as TCreation do
      begin
        for i := 1 to 4 do
        begin
          Shirt[ i ].Free;
          Shirt[ i ] := nil;
          Pants[ i ].Free;
          Pants[ i ] := nil;
        end;
        FreeAndNil(boots);
      end;

      for i := 0 to FigureInstances.Count - 1 do
      begin
        if Assigned( FigureInstances.Objects[ i ] ) then
          TGameObject( FigureInstances.Objects[ i ] ).Free;
      end;
      FigureInstances.Clear;

      if Assigned( Player.Resource ) then
        Player.Resource.Free;
      Player.Free;
      Player := nil;
      PostMessage( Handle, WM_StartMainMenu, 0, 0 ); //Restart the intro
      Exit;
    end;

    ForceNotReadOnly( GamesPath + TempGame + '.sav' );
    ForceNotReadOnly( GamesPath + TempGame + '.idx' );
    ForceNotReadOnly( GamesPath + TempGame + '.map' );

    try
      DeleteFile( GamesPath + TempGame + '.sav' );
    except
    end;
    try
      DeleteFile( GamesPath + TempGame + '.idx' );
    except
    end;
    try
      DeleteFile( GamesPath + TempGame + '.map' );
    except
    end;

    GameName := 'NewGame';

    with Sender as TCreation do
    begin
      Player.Equipment[ slChest1 ] := SelectedShirt;
      Player.Equipment[ slLeg1 ] := SelectedPants;
      Player.Equipment[ slBoot ] := SelectedBoots;
      if Assigned( SelectedHair ) then
      begin
        TCharacterResource( Player.Resource ).HeadName := AnsiString( ExtractFilePath( TCharacterResource( Player.Resource ).NakedName ) + ChangeFileExt( ExtractFileName( SelectedHair.FileName ), '.gif' ));
        TCharacterResource( Player.Resource ).HeadResource := TLayerResource( SelectedHair );
      end
      else
      begin
        TCharacterResource( Player.Resource ).HeadName := '';
        TCharacterResource( Player.Resource ).HeadResource := nil;
      end;

      for i := 1 to 4 do
      begin
        if SelectedShirt = Shirt[ i ] then
        begin
          j := FigureInstances.Add( '' );
          FigureInstances.Objects[ j ] := SelectedShirt;
        end
        else
          Shirt[ i ].Free;

        if SelectedPants = Pants[ i ] then
        begin
          j := FigureInstances.Add( '' );
          FigureInstances.Objects[ j ] := SelectedPants;
        end
        else
          Pants[ i ].Free;
      end;

      j := FigureInstances.Add( '' );
      FigureInstances.Objects[ j ] := SelectedBoots;

      Player.Properties[ 'HeadLayer' ] := TCharacterResource( Player.Resource ).HeadName;

      if SelectedTraining = 0 then
      begin
        Player.AddTitle( 'Squire' );
        Player.AddTitle( 'Siegeexe' );
      end
      else if SelectedTraining = 1 then
      begin
        Player.AddTitle( 'Hunter' );
        Player.AddTitle( 'Siegeexe' );
      end
      else if SelectedTraining = 2 then
      begin
        Player.AddTitle( 'Apprentice' );
        Player.AddTitle( 'Charge' );
        Player.AddTitle( 'Siegeexe' );
      end;

      if AllSpells then
      begin
        for i := 0 to AllSpellList.Count - 1 do
        begin
          Player.AddTitle( AllSpellList.Strings[ i ] );
        end;
      end;

      Player.Alliance := 'Party';
    end;

    TravelList.Clear;
    NPCList.Clear;
    NPCList.Add( Player );

    INI := TIniFile.Create( SiegeINIFile );
    try
      if Player.AttackSound = '' then
        Player.AttackSound := INI.ReadString( 'Character', 'AttackSounds', '' );
      if Player.PainSound = '' then
        Player.PainSound := INI.ReadString( 'Character', 'PainSounds', '' );
      if Player.DeathSound = '' then
        Player.DeathSound := INI.ReadString( 'Character', 'DeathSounds', '' );
      if Player.BattleCry = '' then
        Player.BattleCry := INI.ReadString( 'Character', 'BattleCry', '' );
      // Need to set/get the correct "Player" name not the nakedname resource
      INI.WriteString('Character', 'Resource', ChangeFileExt( ExtractFileName( TCharacterResource( Player.Resource ).FileName ), '' )  );
    finally
      INI.Free;
    end;

    CurrentStartingPoint := 'Start';
    Player.GUID := 'Player';
    i := FigureInstances.Add( Player.GUID );
    FigureInstances.Objects[ i ] := Player;
    i := Figures.Add( TResource( Player.Resource ).FileName );
    Figures.Objects[ i ] := Player.Resource;

    Player.CombatMode := False;
    Player.Money := 5;

    //Player.Speed:=10;

    TransitionScreen := '';
    DeathScreen := '';
    MaxPartyMembers := ScreenMetrics.PartyMemberSlots; // Original 2
    if not LoadMapFile( True, False ) then
      Exit;

    LastFileSaved := '';

    Log.Log( 'Start level' );
    DisableConsole := False;
    NewGame := False;
    Game.ForceRefresh := True;
    FSpellbarActive := False;
    ClearOverlayB;
    BeginJournal;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.SaveOptions;
var
  INI : TIniFile;
const
  FailName : string = 'Main.SaveOptions';
begin
  Log.DebugLog(FailName);
  try

    MasterSoundVolume := DlgOptions.SoundVolume;
    MasterMusicVolume := DlgOptions.MusicVolume;
    if Assigned( MusicLib ) then
      MusicLib.SetSongVolume( MasterMusicVolume );
    PlotShadows := DlgOptions.PlotShadows;
    if MasterSoundVolume > 100 then
      MasterSoundVolume := 100
    else if MasterSoundVolume < 0 then
      MasterSoundVolume := 0;
    if MasterMusicVolume > 100 then
      MasterMusicVolume := 100
    else if MasterMusicVolume < 0 then
      MasterMusicVolume := 0;
    INI := TIniFile.Create( SiegeINIFile );
    try
      INI.WriteInteger( 'Settings', 'SoundVolume', MasterSoundVolume );
      INI.WriteInteger( 'Settings', 'MusicVolume', MasterMusicVolume );
      INI.WriteInteger( 'Settings', 'Shadows', Integer( PlotShadows ) );
    finally
      INI.Free;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.CloseOptions( Sender : TObject );
const
  FailName : string = 'Main.CloseOptions';
begin
  Log.DebugLog(FailName);
  try

    SaveOptions;
    DlgOptions.Release;
    PostMessage( Handle, WM_StartMainMenu, 0, 0 ); //Restart the intro

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.CloseShow( Sender : TObject );
const
  FailName : string = 'Main.CloseShow';
begin
  Log.DebugLog(FailName);
  try

    DlgShow.Release;
    PostMessage( Handle, WM_StartMainMenu, 0, 0 ); //Restart the intro

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.FormShow( Sender : TObject );
var
  INI : TIniFile;
  INILanguage : TIniFile;
  ShowIntro : Boolean;
  rtString : string;
  PopupEnabled : Boolean;
const
  FailName : string = 'Main.FormShow';
begin
  Log.DebugLog(FailName);
  try
    Log.Log( 'Start Siege' );
    Log.flush;
    // Setting all paths and file references - move into record or class - SoAOSPath struct
    // Start with defaults and missing - OS agnostic
    AppPath := ExtractFilePath( Application.ExeName );
    SiegeINIFile := AppPath + 'siege.ini';

    MapPath := AnsiString(IncludeTrailingPathDelimiter(TPath.Combine(AppPath, 'maps')));
    GamesPath := IncludeTrailingPathDelimiter(TPath.Combine(AppPath, 'games'));
    ResourcePath := IncludeTrailingPathDelimiter(TPath.Combine(AppPath, 'ArtLib/Resources'));
    TilePath := IncludeTrailingPathDelimiter(TPath.Combine(AppPath, 'ArtLib/Tiles'));
    SoundPath := IncludeTrailingPathDelimiter(TPath.Combine(ResourcePath, 'Audio'));
    InterfacePath := IncludeTrailingPathDelimiter(TPath.Combine(AppPath, 'Interface'));
    ItemDB := IncludeTrailingPathDelimiter(TPath.Combine(ResourcePath, 'Database/items.db'));
    XRefDB := IncludeTrailingPathDelimiter(TPath.Combine(AppPath, 'Database/xref.db'));
    TitlesDB := IncludeTrailingPathDelimiter(TPath.Combine(AppPath, 'Database/titles.db'));

//    StartFile := MapPath+'OKeepL2.lvl';

    DisableConsole := True;

    NewGame := True;

    INI := TIniFile.Create( SiegeINIFile );
    try
      ResourcePath := TPath.GetFullPath(IncludeTrailingPathDelimiter( INI.ReadString( 'Settings', 'ArtPath', ResourcePath ) ) );
      Log.Log( 'ArtPath=' + ResourcePath );
      Log.flush;

      TilePath := TPath.GetFullPath(IncludeTrailingPathDelimiter( INI.ReadString( 'Settings', 'TilePath', TilePath ) ) );
      Log.Log( 'TilePath=' + TilePath );
      Log.flush;

      SoundPath := TPath.GetFullPath(IncludeTrailingPathDelimiter( INI.ReadString( 'Settings', 'SoundPath', SoundPath ) ) );
      Log.Log( 'SoundPath=' + SoundPath );
      Log.flush;

      Language := INI.ReadString('Settings', 'LanguagePath', 'english');
      InterfacePath := TPath.GetFullPath(IncludeTrailingPathDelimiter( INI.ReadString( 'Settings', 'Interface', InterfacePath )));
      InterfaceLanguagePath := IncludeTrailingPathDelimiter( TPath.Combine( InterfacePath, Language ));

      Log.Log( 'InterfacePath=' + InterfacePath );
      Log.flush;

      PlotScreenRes := INI.ReadInteger( 'Settings', 'ScreenResolution', 600 );

      case PlotScreenRes of
         600 : ScreenMetrics := cOriginal;
         720 : ScreenMetrics := cHD;
        1080 : ScreenMetrics := cFullHD;
        else
        begin
          PlotScreenRes := 600;
          INI.WriteInteger( 'Settings', 'ScreenResolution', 600 );
          ScreenMetrics := cOriginal;
        end;
      end;

      if FileExists( InterfaceLanguagePath + 'gMainMenuBlank.bmp' ) then
        DlgRect := cMultilingualDialogs
      else
        DlgRect := cClassicDialogs;

      AdjustedPartyHitPoints := ( LowerCase( INI.ReadString( 'Settings', 'AdjustedPartyHitPoints', 'false' ) ) = 'true' );
      AdjustedCompanionAI := ( INI.ReadString( 'Settings', 'AdjustedPartyHitPoints', 'true' ).ToLower = 'true' );

      Log.Log( 'Set Bounds' );
      Log.flush;
      Game.SetBounds( 0, 0, ScreenMetrics.GameWidth, ScreenMetrics.GameHeight ); // 703, 511

      Log.Log( 'Mode=DX' );
      Log.flush;

      Log.Log( 'Read Settings' );
      Log.flush;

      GetChapters( INI );

      DeviceDriverIndex := INI.ReadInteger('Settings', 'DeviceDriverIndex', 0);

      // Quick PL and RU Symbol.ini hacks - until better way found
      if Language = 'russian' then
        INICodePage := 1251
      else if Language = 'polish' then
        INICodePage := 1250
      else
        INICodePage := 1252;

      SiegeINILanguageFile :=  AppPath + 'siege.'+Language+'.ini';

      INILanguage := TIniFile.Create(SiegeINILanguageFile);
      try
        ItemDB := TPath.GetFullPath(INILanguage.ReadString( 'Settings', 'ItemDB', ItemDB ));
        TitlesDB := TPath.GetFullPath(INILanguage.ReadString( 'Settings', 'TitlesDB', TitlesDB ));
        UseSmallFont := ( LowerCase( INI.ReadString( 'Settings', 'UseSmallFont', '' ) ) = 'true' );
      finally
        INILanguage.Free;
      end;

      XRefDB := TPath.GetFullPath(INI.ReadString( 'Settings', 'XRefDB', XRefDB ));

      Log.Log( 'Item DB=' + ItemDB );
      Log.Log( 'XRef DB=' + XRefDB );
      Log.Log( 'Titles DB=' + TitlesDB );
      Log.flush;

      TalkToMe := ( LowerCase( INI.ReadString( 'Settings', 'TalkToMe', '' ) ) = 'true' );

      UseDirectSound := False;

      MapPath := AnsiString( TPath.GetFullPath(IncludeTrailingPathDelimiter( INI.ReadString( 'Settings', 'MapPath', MapPath ) ) ) );
      Log.Log( 'MapPath=' + MapPath );
      Log.flush;
    
      DefaultTransition := INI.ReadString( 'Settings', 'Transition', '' );
      if DefaultTransition <> '' then
        DefaultTransition := InterfacePath + DefaultTransition + '.bmp';
      Log.Log( 'DefaultTransition=' + DefaultTransition );
      Log.flush;

      GlobalBrightness := INI.ReadInteger( 'Settings', 'Brightness', 0 );
      INI.WriteInteger( 'Settings', 'Brightness', GlobalBrightness );
      Log.Log( 'Brightness=' + IntToStr( GlobalBrightness ) );
      Log.flush;

      Game.ShowRepaint := ( LowerCase( INI.ReadString( 'Settings', 'ShowRepaint', '' ) ) = 'true' );
      if Game.ShowRepaint then
        Log.Log( 'ShowRepaint=true' );

      Log.Log( 'Create Globals' );
      Log.flush;
      CreateGlobals;
      if LowerCase( INI.ReadString( 'Settings', 'sound', '' ) ) <> 'false' then
      begin
        INI.WriteString( 'Settings', 'sound', 'true' );
        Log.Log( 'Sound enabled' );
        Log.flush;
        Log.Log( 'Initializing sound' );
        Log.flush;

        rtString := LowerCase( INI.ReadString( 'Settings', 'UseDirectSound', '' ) );
        if rtString = '' then
        begin //no setting found - check for SB live
          if ( Pos( 'live', LowerCase( FSOUND_GetDriverName( 0 ) ) ) = 0 ) and ( Pos( 'live', LowerCase( FSOUND_GetDriverName( 1 ) ) ) = 0 ) then
          begin
            UseDirectSound := True;
          end
          else
          begin
            UseDirectSound := False; //SB live
          end;
        end
        else if rtString = 'true' then
          UseDirectSound := True;

        SoundLib := TSound.Create( Handle );
        if LowerCase( INI.ReadString( 'Settings', 'music', '' ) ) <> 'false' then
        begin
          INI.WriteString( 'Settings', 'music', 'true' );
          Log.Log( 'Music enabled' );
          Log.flush;
          Log.Log( 'Initializing music' );
          Log.flush;
          MusicLib := TMusic.Create( Handle );
        end
        else
        begin
          INI.WriteString( 'Settings', 'music', 'false' );
          Log.Log( 'Music disabled' );
          Log.flush;
        end;
      end
      else
      begin
        INI.WriteString( 'Settings', 'sound', 'false' );
        INI.WriteString( 'Settings', 'music', 'false' );
        Log.Log( 'Sound disabled' );
        Log.flush;
        Log.Log( 'Music disabled' );
        Log.flush;
      end;
      //MusicLib.OpenThisSong(SoundPath+'theme\canyon'+'.mp3');
      //MusicLib.PlayThisSong;

      if DaSoundCardAvailable then
        Log.Log( 'Sound card available' )
      else
        Log.Log( 'Sound card not available' );
      Log.flush;

      SoundOK := DaSoundCardAvailable and ( Assigned( SoundLib ) or Assigned( MusicLib ) );
      Log.Log( 'Initializing event timer' );
      Log.flush;
      SoundTimer := TTimer.Create( nil );
      SoundTimer.Interval := 100;

      MasterSoundVolume := INI.ReadInteger( 'Settings', 'SoundVolume', 50 );
      MasterMusicVolume := INI.ReadInteger( 'Settings', 'MusicVolume', 50 );
      PlotShadows := ( INI.ReadInteger( 'Settings', 'Shadows', 1 ) <> 0 );
      if PlotShadows then
      begin
        INI.WriteInteger( 'Settings', 'Shadows', 1 );
        Log.Log( 'PlotShadows enabled' );
      end
      else
      begin
        INI.WriteInteger( 'Settings', 'Shadows', 0 );
      end;

      UseTimer := ( LowerCase( INI.ReadString( 'Settings', 'UseTimer', '' ) ) = 'true' );
      if UseTimer then
      begin
        INI.WriteString( 'Settings', 'UseTimer', 'true' );
        Log.Log( 'UseTimer enabled' );
      end
      else
      begin
        INI.WriteString( 'Settings', 'UseTimer', 'false' );
      end;
      Interval := INI.ReadInteger( 'Settings', 'Interval', 30 );
      Log.Log( 'Interval=' + IntToStr( Interval ) );
      UseVideoRAM := ( LowerCase( INI.ReadString( 'Settings', 'UseVideo', '' ) ) = 'true' );
      if UseVideoRAM then
      begin
        INI.WriteString( 'Settings', 'UseVideo', 'true' );
        Log.Log( 'UseVideoRAM enabled' );
      end
      else
      begin
        INI.WriteString( 'Settings', 'UseVideo', 'false' )
      end;
      AllSpells := ( LowerCase( INI.ReadString( 'Settings', 'AllSpells', '' ) ) = 'true' );
      if AllSpells then
        Log.Log( 'AllSpells enabled' );

      Bikini := ( lowercase( INI.ReadString( 'Settings', 'Bikini', '' ) ) = 'true' );
      NoPageNumbers := ( LowerCase( INI.ReadString( 'Settings', 'NoPageNumbers', '' ) ) = 'true' );
      NoTransit := ( LowerCase( INI.ReadString( 'Settings', 'NoTransit', '' ) ) = 'true' );
      GameMap.UseLighting := ( LowerCase( INI.ReadString( 'Settings', 'Lighting', '' ) ) <> 'false' );
      if GameMap.UseLighting then
      begin
        INI.WriteString( 'Settings', 'Lighting', 'true' );
        Log.Log( 'UseLighting enabled' );
      end
      else
      begin
        INI.WriteString( 'Settings', 'Lighting', 'false' );
      end;
      GameMap.UseAmbientOnly := ( LowerCase( INI.ReadString( 'Settings', 'AmbientOnly', '' ) ) = 'true' );
      Log.flush;

      if ( INI.ReadInteger( 'Settings', 'JournalFont', 0 ) = 1 ) and TDirectory.Exists( ResourcePath + 'journalalt\'  + Language ) then
        AdventureLog1.LogDirectory := ResourcePath + 'journalalt\' + Language + '\'
      else
        AdventureLog1.LogDirectory := ResourcePath + 'journal\' + Language + '\';

      //cue intro music
      Log.Log( 'Start event timer' );
      Log.flush;
      SoundTimer.OnTimer := Timer1Timer;
      SoundTimer.Enabled := True; //Start sound timer

      ShowIntro := ( LowerCase( INI.ReadString( 'Settings', 'ShowIntro', 'true' ) ) = 'true' );

      PopupEnabled := ( LowerCase( INI.ReadString( 'Settings', 'Popup', 'true' ) ) = 'true' );

    finally
      INI.Free;
    end;

    Log.Log( 'Initializing DX...' );
    Log.flush;
    Game.InitDX( Handle, ScreenMetrics.ScreenWidth, ScreenMetrics.ScreenHeight, ScreenMetrics.BPP );  // 800, 600, 16
    Game.PreCreateMap( ScreenMetrics.PreMapWidth, ScreenMetrics.PreMapHeight );  // 768, 544
    Log.Log( 'DX initialization complete' );
    Log.flush;
    if PixelFormat = pf555 then
      Log.Log( 'Using 555 Driver' )
    else if PixelFormat = pf565 then
      Log.Log( 'Using 565 Driver' )
    else if PixelFormat = pf888 then
      Log.Log( 'Using 888 Driver' );
    Log.flush;

    Log.Log( 'Loading cursor' );
    Log.flush;
    MouseCursor := TMousePtr.Create;
    Screen.Cursor := crNone;

    Log.Log( 'Initializing DFX...' );
    Log.flush;
    DFXInit( AppPath );

    Log.Log( 'Loading resources...' );
    Log.flush;
    if not LoadResources then
    begin
      Log.Log( 'Load failed...' );
      Log.flush;
      //Error Message
      FreeAll;
      Close;
      Exit;
    end;

    NPCHealthBltFx.dwSize := SizeOf( TDDBLTFX );
    NPCHealthBltFx.dwFillColor := SoAOS_DX_ColorMatch( lpDDSFront, cHealthColor );

    NPCManaBltFx.dwSize := SizeOf( TDDBLTFX );
    NPCManaBltFx.dwFillColor := SoAOS_DX_ColorMatch( lpDDSFront, cManaColor );

    NPCBarXCoord[ 1 ] := 67;
    NPCBarXCoord[ 2 ] := 153;
    NPCBarXCoord[ 3 ] := 235;
    NPCBarXCoord[ 4 ] := 312;

    if PopupEnabled then
      Popup := TPopup.Create;

    if NeedToReload then
    begin
      Log.Log( 'Reloading game=' + GameName );
      Log.Log( LVLFile );
      Log.Log( CurrentScene );
      TravelList.Text := KeepTravelList;
      KeepTravelList := '';
      TransitionScreen := '';
      DeathScreen := '';
      MaxPartyMembers := ScreenMetrics.PartyMemberSlots; // Original 2
      if not LoadMapFile( False, True ) then
        Exit;
      DisableConsole := False;
      NewGame := False;
      Game.ForceRefresh := True;
      CloseAllDialogs( nil );
      MouseCursor.Enabled := True;
    end
    else
    begin
      if ShowIntro and not Initialized then
        PostMessage( Handle, WM_StartIntro, 0, 0 )
      else
        PostMessage( Handle, WM_StartMainMenu, 0, 0 );
    end;

    Log.Log( 'Load complete' );
    Log.flush;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.Timer1Timer( Sender : TObject );
var
  i : Integer;
  S : string;
const
  FailName : string = 'Main.Timer1Timer';
begin
  try

    SoundTimer.OnTimer := nil;

    if SoundOK then
    begin
      if Active then
      begin
        for i := 0 to Sounds.Count - 1 do
        begin
          if TObject( Sounds.Items[ i ] ) is TSoundPlayer then
          begin
            with TSoundPlayer( Sounds.Items[ i ] ) do
            begin
              if Continuous or ( SoundCount = 0 ) then
                Execute;
            end;
          end
          else if TObject( Sounds.Items[ i ] ) is TEventTimer then
          begin
            with TEventTimer( Sounds.Items[ i ] ) do
            begin
              if SoundCount = 0 then
                Execute;
            end;
          end;
        end;
        Inc( SoundCount );
        if SoundCount >= 10 then
        begin
          Inc( Seconds );
          SoundCount := 0;
        end;

        Inc( SongCounter );
        if SongCounter > SongDuration then
        begin
          CueTune( '', False );
          SongCounter := -30;
        end
        else if SongCounter = 0 then
        begin
          S := Themes.Values[ CurrentTheme ];
          CueTune( S, False );
        end;
      end;

      if Assigned( MusicLib ) then
      begin
        if ( FadeOut > 0 ) then
        begin
          Dec( FadeOut, 1 );
          MusicLib.SetSongVolume( FadeOut );
          if FadeOut = 0 then
          begin
            MusicLib.PauseThisSong;
            if NextSong <> '' then
            begin
              MusicLib.OpenThisSong( AnsiString( SoundPath + NextSong + '.mp3' ) );
              FadeIn := MasterMusicVolume + 5;
            end;
          end;
        end
        else if ( FadeIn > 0 ) then
        begin
          if NoFadeIn then
          begin
            if FadeIn = MasterMusicVolume + 5 then
            begin
              MusicLib.SetSongVolume( 0 );
              Dec( FadeIn, 1 );
            end
            else if FadeIn = MasterMusicVolume + 1 then
            begin
              MusicLib.SetSongVolume( MasterMusicVolume );
              Dec( FadeIn, 1 );
            end
            else if FadeIn > MasterMusicVolume then
              Dec( FadeIn, 1 )
            else
            begin
              MusicLib.PlayThisSong;
              MusicLib.SetSongVolume( MasterMusicVolume );
              FadeIn := 0;
            end;
          end
          else
          begin
            if FadeIn = MasterMusicVolume + 5 then
              MusicLib.SetSongVolume( 0 )
            else if FadeIn >= MasterMusicVolume then
            else
              MusicLib.SetSongVolume( MasterMusicVolume - FadeIn );
            if FadeIn = MasterMusicVolume then
            begin
              MusicLib.PlayThisSong;
              MusicLib.SetSongVolume( 0 )
            end;
            Dec( FadeIn, 1 );
          end;
        end;
      end;
    end
    else
    begin
      if Active then
      begin
        for i := 0 to Sounds.Count - 1 do
        begin
          if TObject( Sounds.Items[ i ] ) is TEventTimer then
          begin
            with TEventTimer( Sounds.Items[ i ] ) do
            begin
              if SoundCount = 0 then
                Execute;
            end;
          end;
        end;
      end;
    end;

    SoundTimer.OnTimer := Timer1Timer;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.AniView1MouseDown( Sender : TObject; Button : TMouseButton;
  Shift : TShiftState; X, Y, GridX, GridY : Integer );
const
  FailName : string = 'Main.AniView1MouseDown';
begin
  try
    if not Active then
      Exit;
    if not Assigned( Current ) then
      Exit;
    if Current.Frozen then
      Exit;

    Current.AutoFight := False;
    Current.NextAction := naNone;

    if Button = mbLeft then
    begin
      {if (X>=120) and (X<179) and (Y>=490) and (Y<509) and not SpellBarActive then begin
        if DlgRoster.Loaded then
          CloseAllDialogs(DlgRoster)
        else begin
          DoNotRestartTimer:=true;
          CloseAllDialogs(DlgRoster);
          BeginRoster(nil);
        end;
        exit;
      end; }

      if Assigned( Game.MouseOverHLFigure ) then
      begin
        if ( ssShift in Shift ) then
        begin
          if ( Game.MouseOverHLFigure is TCharacter ) and not TCharacter( Game.MouseOverHLFigure ).Dead then
          begin
            Current.AutoFight := True;
            Current.Attack( TCharacter( Game.MouseOverHLFigure ) );
          end
          else
            Current.AttackPoint( GridX, GridY );
        end
        else if ( ssCtrl in Shift ) then
        begin
          if ( Game.MouseOverHLFigure is TCharacter ) and TCharacter( Game.MouseOverHLFigure ).Dead and IsOnZoneTile( Game.MouseOverHLFigure ) then
          begin
            BeginLoot( Current, TCharacter( Game.MouseOverHLFigure ) );
          end
          else if ShouldRun( GridX, GridY ) then
            Current.ShiftApproachRun( TSpriteObject( Game.MouseOverHLFigure ) )
          else
            Current.ShiftApproach( TSpriteObject( Game.MouseOverHLFigure ) );
          if ( Game.MouseOverHLFigure is TCharacter ) and Assigned( TCharacter( Game.MouseOverHLFigure ).AI ) then
            TCharacter( Game.MouseOverHLFigure ).AI.Clicked;
        end
        else
        begin
          if ( Game.MouseOverHLFigure is TDoor ) and not TDoor( Game.MouseOverHLFigure ).Closed then
          begin
            //          Current.IntendToZone:=Zonable;
            Current.IntendToZone := True;
            if ShouldRun( GridX, GridY ) then
              Current.RunTo( GridX, GridY, 64 )
            else
              Current.WalkTo( GridX, GridY, 64 );
          end
          else if ( Game.MouseOverHLFigure is TCharacter ) then
          begin
            if Current.CombatMode and not TCharacter( Game.MouseOverHLFigure ).Dead and
              Current.IsEnemy( TCharacter( Game.MouseOverHLFigure ) ) and
              Current.InRange( Game.MouseOverHLFigure ) then
            begin
              Current.AutoFight := True;
              Current.Attack( TCharacter( Game.MouseOverHLFigure ) );
            end
            else
            begin
              if ( Game.MouseOverHLFigure is TCharacter ) and TCharacter( Game.MouseOverHLFigure ).Dead and IsOnZoneTile( Game.MouseOverHLFigure ) then
              begin
                BeginLoot( Current, TCharacter( Game.MouseOverHLFigure ) );
              end
              else if ShouldRun( GridX, GridY ) then
                Current.ApproachRun( TSpriteObject( Game.MouseOverHLFigure ) )
              else
                Current.Approach( TSpriteObject( Game.MouseOverHLFigure ) );
              if Assigned( TCharacter( Game.MouseOverHLFigure ).AI ) then
                TCharacter( Game.MouseOverHLFigure ).AI.Clicked;
            end;
          end
          else
          begin
            if ShouldRun( GridX, GridY ) then
              Current.ApproachRun( TSpriteObject( Game.MouseOverHLFigure ) )
            else
              Current.Approach( TSpriteObject( Game.MouseOverHLFigure ) );
          end;
        end;
      end
      else
      begin
        if ( ssShift in Shift ) then
          Current.AttackPoint( GridX, GridY )
        else
        begin
          //        Current.IntendToZone:=Zonable;
          Current.IntendToZone := True;
          if ShouldRun( GridX, GridY ) then
            Current.RunTo( GridX, GridY, 64 )
          else
            Current.WalkTo( GridX, GridY, 64 );
        end;
      end;
    end
    else if Button = mbRight then
    begin
      if Assigned( Game.MouseOverHLFigure ) then
      begin
        if ( Game.MouseOverHLFigure is TSpriteobject ) then
          Current.Cast( TCharacter( Game.MouseOverHLFigure ) )
        else
          Current.CastPoint( GridX, GridY );
      end
      else
      begin
        Current.CastPoint( GridX, GridY );
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;


procedure TfrmMain.AniView1AfterDisplay( Sender : TObject );
var
  i, j : Integer;
  GridX, GridY : Longint;
  S : string;
const
  FailName : string = 'Main.AniView1AfterDisplay';
begin
  Log.DebugLog(FailName);
  try
    if Assigned( Game.MouseOverTile ) then
    begin
      if Game.MouseOverTile.TriggerID <> PrevTriggerID then
      begin
        Zonable := False;

        if Game.MouseOverTile.TriggerID > 0 then
        begin
          i := Game.MouseOverTile.TriggerID - 1;
          if ( i < FigureInstances.Count ) and Assigned( FigureInstances.Objects[ i ] ) and ( FigureInstances.Objects[ i ] is TTrigger ) then
          begin
            j := Pos( 'loadmap(', LowerCase( TTrigger( FigureInstances.Objects[ i ] ).OnTrigger ) );
            if (j > 0) and TTrigger( FigureInstances.Objects[ i ] ).TriggerEnabled then
            begin
              S := Parse( AnsiString( Copy( TTrigger( FigureInstances.Objects[ i ] ).OnTrigger, j + 8, Length( TTrigger( FigureInstances.Objects[ i ] ).OnTrigger ) - j - 7 )), 0, ',' );
              if TFile.Exists( FindMap( S ) ) then
              begin
                MouseCursor.SetFrame( 1 );
                Zonable := True;
              end;
            end;
          end;
        end
        else
          MouseCursor.SetFrame( 0 );
        PrevTriggerID := Game.MouseOverTile.TriggerID;
      end;
    end
    else
    begin
      if PrevTriggerID <> -1 then
      begin
        MouseCursor.SetFrame( 37 );
        PrevTriggerID := -1;
      end;
    end;

    MouseCursor.PlotDirty := False;
    if Assigned( Game.MouseOverHLFigure ) and ( Game.MouseOverHLFigure is TDoor ) then
    begin
      with Game.MouseOverHLFigure as TDoor do
      begin
        Highlighted := Closed or ( GetKeyState( VK_CONTROL ) < 0 ); //Only highlight doors if ctrl key down
        if Highlighted then
        begin
          if Game.MouseOverHLFigure <> HLFigure then
          begin
            if Assigned( HLFigure ) then
              HLFigure.Highlighted := False;
            HighlightColor := clPurple;
            HLFigure := Game.MouseOverHLFigure;
          end;
        end;
      end;
    end
    else
    begin
      if Game.MouseOverHLFigure <> HLFigure then
      begin
        if Assigned( HLFigure ) then
          HLFigure.Highlighted := False;
        HLFigure := nil;
        if Assigned( Game.MouseOverHLFigure ) then
        begin
          if Game.MouseOverHLFigure is TCharacter then
          begin
            if Assigned( Current ) then
            begin
              if not Current.CombatMode or not TCharacter( Game.MouseOverHLFigure ).Dead then
              begin
                if Current.IsEnemy( TCharacter( Game.MouseOverHLFigure ) ) then
                begin
                  Game.MouseOverHLFigure.HighlightColor := clRed;
                  TCharacter( Current ).Track := TCharacter( Game.MouseOverHLFigure );
                end
                else if Current.IsAlly( TCharacter( Game.MouseOverHLFigure ) ) then
                  Game.MouseOverHLFigure.HighlightColor := clGreen
                else
                  Game.MouseOverHLFigure.HighlightColor := clYellow;
                Game.MouseOverHLFigure.Highlighted := True;
                HLFigure := Game.MouseOverHLFigure;
              end;
            end
            else
              Game.MouseOverHLFigure.HighlightColor := clYellow;
          end
          else if Game.MouseOverHLFigure is TItem then
          begin
            i := TItem( Game.MouseOverHLFigure ).Magic;
            if i > 255 then
              i := 255;
            Game.MouseOverHLFigure.HighlightColor := $FF0000 + ( i shl 8 ) + i;
            Game.MouseOverHLFigure.Highlighted := True;
            HLFigure := Game.MouseOverHLFigure;
          end
          else
          begin
            Game.MouseOverHLFigure.HighlightColor := clPurple;
            Game.MouseOverHLFigure.Highlighted := True;
            HLFigure := Game.MouseOverHLFigure;
          end;
        end;
      end;
    end;

    for i := ActiveTriggers.Count - 1 downto 0 do
    begin
      if TTrigger( ActiveTriggers[ i ] ).Execute then
        ActiveTriggers.Delete( i );
    end;

    for i := SpawnList.Count - 1 downto 0 do
    begin
      if TCharacter( SpawnList.Items[ i ] ).SpawnCount > 0 then
      begin
        Dec( TCharacter( SpawnList.Items[ i ] ).SpawnCount );
      end
      else
      begin
        TCharacter( SpawnList.Items[ i ] ).Enabled := True;
        SpawnList.Delete( i );
      end;
    end;

    if ( MouseCursor.Enabled ) and Assigned( Current ) and Game.LMouseButton and not Assigned( Current.Target ) and ( GetKeyState( VK_SHIFT ) >= 0 ) and ( ( Game.FrameCount mod 8 ) = 0 ) then
    begin
      GridX := Game.MousePosition.X + Game.OffsetX;
      GridY := Game.MousePosition.Y + Game.OffsetY;
      if ShouldRun( GridX, GridY ) then
        Current.RunTo( GridX, GridY, 64 )
      else
        Current.WalkTo( GridX, GridY, 64 );
    end;

    if Player.DeadCount > 100 then
    begin
      Active := False;
      NewGame := True;
      ClearResources( True );
      Sprites.Realloc;
      BeginDeath;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.DrawHealthBars;
var
  i : Integer;
  HpDistance, ManaDistance : Double;
  pr, pr0 : TRect;
const
  FailName : string = 'Main.DrawHealthBars';
begin
  Log.DebugLog(FailName);
  try

    if Mana > 0 then
      i := Round( 80 * ( Drain / Mana ) )
    else
      i := 80;
    if i > 80 then
      i := 80
    else if i < 0 then
      i := 0;
    pr := Rect( 0, 0, 78, i );

    lpDDSBack.BltFast( ScreenMetrics.ManaEmptyX, 134, ManaEmpty, @pr, DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
    if Life > 0 then
      i := Round( 107 * ( Wounds / Life ) )
    else
      i := 107;
    if i > 107 then
      i := 107
    else if i < 0 then
      i := 0;
    pr := Rect( 0, 0, 52, i );

    lpDDSBack.BltFast( ScreenMetrics.LifeEmptyX, 248, LifeEmpty, @pr, DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );  //SD

    if not FSpellbarActive then
    begin
      for i := 1 to NPCList.Count - 1 do
      begin
        HpDistance := TCharacter( NPCList[ i ] ).HitPoints - TCharacter( NPCList[ i ] ).Wounds;
        if HPDistance > TCharacter( NPCList[ i ] ).HitPoints then
          HPDistance := TCharacter( NPCList[ i ] ).HitPoints
        else if HPDistance < 0 then
          HPDistance := 0;

        ManaDistance := TCharacter( NPCList[ i ] ).Mana - TCharacter( NPCList[ i ] ).Drain;
        if ManaDistance > TCharacter( NPCList[ i ] ).Mana then
          ManaDistance := TCharacter( NPCList[ i ] ).Mana
        else if ManaDistance < 0 then
          ManaDistance := 0;

        HPDistance := HPDistance * ( 66 / TCharacter( NPCList[ i ] ).HitPoints );
        ManaDistance := ManaDistance * ( 66 / TCharacter( NPCList[ i ] ).Mana );

        pr := Rect( NPCBarXCoord[ i ], ScreenMetrics.NPCBarY - Round( HPDistance ), NPCBarXCoord[ i ] + 5, ScreenMetrics.NPCBarY );
        pr0 := Rect( 0, 0, 0, 0 );
        lpDDSBack.Blt( @pr, nil, @pr0, DDBLT_COLORFILL + DDBLT_WAIT, @NPCHealthBltFx );

        pr := Rect( NPCBarXCoord[ i ] + 7, ScreenMetrics.NPCBarY - Round( ManaDistance ), NPCBarXCoord[ i ] + 7 + 5, ScreenMetrics.NPCBarY );
        lpDDSBack.Blt( @pr, nil, @pr0, DDBLT_COLORFILL + DDBLT_WAIT, @NPCManaBltFx );
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.AniView1BeforeDisplay( Sender : TObject );
var
  i : Integer;
  pr : TRect;
const
  FailName : string = 'Main.AniView1BeforeDisplay';
begin
  Log.DebugLog(FailName);
  try

    for i := SayList.Count - 1 downto 0 do
      TSpriteObject( SayList.Items[ i ] ).UpdateSay;

    if FSpellBarActive then
    begin
      pr := Rect( 0, 0, ScreenMetrics.ScreenWidth, 114 );
      lpDDSBack.BltFast( 0, ScreenMetrics.SpellBarY, SpellBar, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      pr := Rect( 0, 0, 195, 59 );
      lpDDSBack.BltFast( 250, ScreenMetrics.HelpBoxY, HelpBox, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    end
    else
    begin
      if Assigned( HLFigure ) and ( HLFigure is TSpriteObject ) then
        ShowMouseMessage( TSpriteObject( HLFigure ).Name )
      else
        ShowMouseMessage( '' );
      pr := Rect( 0, 0, ScreenMetrics.ScreenWidth, 114 );
      lpDDSBack.BltFast( 0, ScreenMetrics.SpellBarY, OverlayB, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    end;
    pr := Rect( 0, 0, 117, ScreenMetrics.SpellBarY );
    lpDDSBack.BltFast( ScreenMetrics.SpellBarX, 0, OverlayR, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

    if Assigned( Player ) then
    begin
      //  if (Mana<>TCharacter(Game.KeyFigure).Mana) or (Drain<>TCharacter(Game.KeyFigure).Drain) then begin
      Mana := Player.Mana;
      Drain := Player.Drain;
      //  end;

      //  if (Life<>TCharacter(Game.KeyFigure).HitPoints) or (Wounds<>TCharacter(Game.KeyFigure).Wounds) then begin
      Life := Player.HitPoints;
      Wounds := Player.Wounds;
      //  end;
    end;

    DrawHealthBars;

    if Assigned( Popup ) then
      Popup.Draw;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.FormDestroy( Sender : TObject );
var
  ExStyle : Integer;
const
  FailName : string = 'Main.FormDestroy';
begin
  Log.DebugLog(FailName);
  try

    Log.Log( 'Shutting down application' );
    try
      DeleteFile( GamesPath + TempGame + '.sav' );
    except
    end;
    try
      DeleteFile( GamesPath + TempGame + '.idx' );
    except
    end;
    try
      DeleteFile( GamesPath + TempGame + '.map' );
    except
    end;

    Dec( SetAppExStyleCount );
    if SetAppExStyleCount = 0 then
    begin
      ExStyle := GetWindowLong( Application.Handle, GWL_EXSTYLE );
      ExStyle := ExStyle and ( not WS_EX_TOOLWINDOW );
      SetWindowLong( Application.Handle, GWL_EXSTYLE, ExStyle );
    end;

    imgHelp.Free;
    imgCombat.Free;
    imgAutoTransparent.Free;
    imgSidebar.Free;
    imgSpellBar.Free;
    imgBottomBar.Free;

    AdventureLog1.free;
    HistoryLog.free;

    Application.OnException := nil;
    Application.OnActivate := nil;
    Application.OnDeactivate := nil;
    Log.Log( 'Console destroyed' );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.FormCreate( Sender : TObject );
var
  ExStyle : Integer;
begin
  Scaled := False;
  BorderStyle := bsNone;
  BorderIcons := [];
  FormStyle := fsStayOnTop;

  imgHelp := TBitmap.Create;
  imgCombat := TBitmap.Create;
  imgAutoTransparent := TBitmap.Create;
  imgSidebar := TBitmap.Create;
  imgSpellBar := TBitmap.Create;
  imgBottomBar := TBitmap.Create;

  ScreenMetrics := cOriginal; // cOriginal or  cHD or cFullHD - TBA
  AdjustedPartyHitPoints := False; // True = Rucksacksepp's HP adjusted by number of PartyMembers

  Application.OnException := AppException;
  Application.OnActivate := AppActivate;
  Application.OnDeactivate := AppDeactivate;

  Inc( SetAppExStyleCount );
  ExStyle := GetWindowLong( Application.Handle, GWL_EXSTYLE );
  ExStyle := ExStyle or WS_EX_TOOLWINDOW;
  SetWindowLong( Application.Handle, GWL_EXSTYLE, ExStyle );

  AdventureLog1 := TAdventureLog.create;
  HistoryLog := TAdventureLog.create;

  OnKeyDown := TKeyEvent.FormKeyDown;

  Game := TAniView.Create( frmMain );
  Game.Parent := frmMain;
  Game.Height := ScreenMetrics.GameHeight; // 511; In HD there was a diff 991 <-> 997?
  Game.Left := 0;
  Game.LMouseButton := False;
  Game.ShowRepaint := False;
  Game.Width := ScreenMetrics.GameWidth;  // 703
  Game.OnAfterDisplay := AniView1AfterDisplay;
  Game.OnBeforeDisplay := AniView1BeforeDisplay;
  Game.OnMouseDown := AniView1MouseDown;

  GameMap := TAniMap.Create( frmMain );
  GameMap.AmbientColor := cBlackBackground;
  GameMap.AmbientIntensity := 0;
  GameMap.Height := 400;
  GameMap.TransparentColor := cTransparent;
  GameMap.UseAmbientOnly := False;
  GameMap.UseLighting := True;
  GameMap.Width := 200;
end;

procedure TfrmMain.AppException( Sender : TObject; E : Exception );
begin
  if Assigned( Log ) then
  begin
    Log.log( '*** Error Application: ' + E.Message );
    Log.Flush;
  end;
  if Assigned( lpDD ) then
    lpDD.FlipToGDISurface;
  MessageBox( Handle, PChar(E.Message), 'Failure', MB_OK );
  Application.Terminate;
end;

procedure TfrmMain.BeginConverse( ObjectRef : TGameObject; Conversation : string );
const
  FailName : string = 'Main.BeginConverse';
begin
  Log.DebugLog(FailName);
  try

    if DlgConverse.Loaded then
      Exit; //We cannot have more than one conversation going
    DisableConsole := True;
    DlgConverse.ObjectRef := ObjectRef;
    DlgConverse.Conversation := Conversation;
    OpenDialog( DlgConverse, CloseAllDialogs );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.BeginInventory( Character : TCharacter );
var
  List : TList;
  i : Integer;
const
  FailName : string = 'Main.BeginInventory';
begin
  Log.DebugLog(FailName);
  try

    if Assigned( Character ) and not Character.InterfaceLocked then
    begin
      DlgInventory.Character := Character;
      DlgInventory.GroundList.Clear;
      DlgInventory.DrawGuy := InventoryDraw;
      //    DlgInventory.Locked:=(Current<>Player);

      List := Game.FindInRadius( Character.X, Character.Y, 64 );
      try
        if Assigned( List ) then
        begin
          for i := 0 to List.Count - 1 do
          begin
            if ( TAniFigure( List.Items[ i ] ) is TItem ) then
            begin
              if TItem( List.Items[ i ] ).Enabled then
              begin
                DlgInventory.GroundList.Add( List.Items[ i ] );
              end;
            end;
          end;
        end;
      finally
        List.Free;
      end;

      OpenDialog( DlgInventory, CloseAllDialogs );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.BeginMerchant( Character : TCharacter );
var
  List : TList;
  i : Integer;
const
  FailName : string = 'Main.BeginMerchant';
begin
  Log.DebugLog(FailName);
  try

    if Assigned( Character ) and not Character.InterfaceLocked then
    begin
      DlgMerchant.Merchant := Character;
      DlgMerchant.Character := Current;
      DlgMerchant.GroundList.Clear;
      DlgMerchant.DrawGuy := InventoryDraw;
      DlgMerchant.Locked := ( Current <> Player );

      List := Game.FindInRadius( Character.X, Character.Y, 64 );
      try
        if Assigned( List ) then
        begin
          for i := 0 to List.Count - 1 do
          begin
            if TAniFigure( List.Items[ i ] ) is TItem then
            begin
              if TItem( List.Items[ i ] ).Enabled then
              begin
                DlgMerchant.GroundList.Add( List.Items[ i ] );
              end;
            end;
          end;
        end;
      finally
        List.Free;
      end;

      OpenDialog( DlgMerchant, CloseAllDialogs );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.BeginObjInventory( Character : TCharacter; OtherObj : TSpriteObject );
var
  List : TList;
  i : Integer;
const
  FailName : string = 'Main.BeginObjInventory';
begin
  Log.DebugLog(FailName);
  try

    if Assigned( Character ) and Assigned( OtherObj ) and not Character.InterfaceLocked then
    begin
      DlgObjInventory.Character := Character;
      DlgObjInventory.OtherOb := OtherObj;
      DlgObjInventory.GroundList.Clear;

      List := Game.FindInRadius( Character.X, Character.Y, 64 );
      try
        if Assigned( List ) then
        begin
          for i := 0 to List.Count - 1 do
          begin
            if TAniFigure( List.Items[ i ] ) is TItem then
            begin
              if TItem( List.Items[ i ] ).Enabled then
              begin
                DlgObjInventory.GroundList.Add( List.Items[ i ] );
              end;
            end;
          end;
        end;
      finally
        List.Free;
      end;

      OpenDialog( DlgObjInventory, CloseAllDialogs );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.BeginLoot( Character : TCharacter; OtherObj : TSpriteObject );
var
  List : TList;
  i : Integer;
  Money : Integer;
const
  FailName : string = 'Main.BeginLoot';
begin
  Log.DebugLog(FailName);
  try

    if Assigned( Character ) and Assigned( OtherObj ) and ( OtherObj is TCharacter ) and not Character.InterfaceLocked then
    begin
      Money := TCharacter( OtherObj ).Money; //Funds transfer
      TCharacter( OtherObj ).Money := -Money;
      Character.Money := Money;
      TCharacter( OtherObj ).Looted := True;

      DlgLoot.Character := Character;
      DlgLoot.OtherOb := OtherObj;
      DlgLoot.Locked := LowerCase( OtherObj.Properties[ 'EquipmentLocked' ] ) = 'true';
      DlgLoot.GroundList.Clear;

      List := Game.FindInRadius( Character.X, Character.Y, 64 );
      try
        if Assigned( List ) then
        begin
          for i := 0 to List.Count - 1 do
          begin
            if TAniFigure( List.Items[ i ] ) is TItem then
            begin
              if TItem( List.Items[ i ] ).Enabled then
              begin
                DlgLoot.GroundList.Add( List.Items[ i ] );
              end;
            end;
          end;
        end;
      finally
        List.Free;
      end;

      OpenDialog( DlgLoot, CloseAllDialogs );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.BeginStatistics( Character : TCharacter );
const
  FailName : string = 'Main.BeginStatistics';
begin
  Log.DebugLog(FailName);
  try

    if Assigned( Character ) and not Character.InterfaceLocked then
    begin
      DlgStatistics.Character := Character;
      OpenDialog( DlgStatistics, CloseAllDialogs );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.BeginMap( Character : TCharacter );
const
  FailName : string = 'Main.BeginMap';
begin
  Log.DebugLog(FailName);
  try

    if Assigned( Character ) then
    begin
      DlgMap.Character := Character;
      DlgMap.CharacterList := NPCList;
      DlgMap.Map := GameMap;
      DlgMap.MapName := MapName;
      OpenDialog( DlgMap, CloseAllDialogs );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.BeginHelp;
const
  FailName : string = 'Main.BeginHelp';
begin
  Log.DebugLog(FailName);
  try

    DlgShow.frmMain := Self;
    DlgShow.BMPFileName := 'HelpScreen(telekeneticDuck).bmp';
    DlgShow.MusicFileName := '';
    OpenDialog( DlgShow, CloseAllDialogs );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.BeginTitles( Character : TCharacter );
const
  FailName : string = 'Main.BeginTitles';
begin
  Log.DebugLog(FailName);
  try

    if Assigned( Character ) and not Character.InterfaceLocked then
    begin
      DlgTitles.Character := Character;
      OpenDialog( DlgTitles, CloseAllDialogs );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.BeginNPC( Character : TCharacter );
const
  FailName : string = 'Main.BeginNPC';
begin
  Log.DebugLog(FailName);
  try

    Exit;
    if Assigned( Character ) and not Character.InterfaceLocked then
    begin
      if Character.AI is TPartyAI then
      begin
        DlgNPC.CharAI := TPartyAI( Character.AI );
        OpenDialog( DlgNPC, CloseAllDialogs );
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.BeginJournal;
const
  FailName : string = 'Main.BeginJournal';
begin
  Log.DebugLog(FailName);
  try

    ClearLogGraphic;
    DisableConsole := True;
    DlgJournal.frmMain := Self;
    DlgJournal.JournalLog := AdventureLog1;
    OpenDialog( DlgJournal, CloseAllDialogs );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.BeginOptions( Character : TCharacter );
const
  FailName : string = 'Main.BeginOptions';
begin
  Log.DebugLog(FailName);
  try

    if Assigned( Character ) then
    begin
      DlgOptions.Character := Character;
      DlgOptions.SoundVolume := MasterSoundVolume;
      DlgOptions.MusicVolume := MasterMusicVolume;
      DlgOptions.PlotShadows := PlotShadows;
      DlgOptions.IconDX := SpellGlyphs;
      OpenDialog( DlgOptions, CloseAllDialogs );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.FreeAll;
const
  FailName : string = 'Main.FreeAll';
begin
  Log.DebugLog(FailName);
  try

    if Assigned( SoundTimer ) then
    begin
      SoundTimer.OnTimer := nil;
      SoundTimer.Enabled := False;
      SoundTimer.Free;
    end;
    Active := False;
    Game.Enabled := False;
    Log.Log( 'Freeing resources' );
    ClearResources( False );
    Game.UncreateMap;
    Log.Log( 'Freeing globals' );
    FreeGlobals;
    ScreenShot.Free;
    Log.Log( 'Freeing dialogs' );
    DlgConverse.Free;
    //Log.Log('a');
    DlgInventory.Free;
    DlgInventory := nil;
    //Log.Log('a1');
    DlgMerchant.Free;
    DlgMerchant := nil;
    //Log.Log('a2');
    DlgObjInventory.Free;
    DlgObjInventory := nil;
    //Log.Log('a3');
    DlgStatistics.Free;
    DlgStatistics := nil;
    //Log.Log('a4');
    DlgMap.Free;
    DlgMap := nil;
    //Log.Log('a5');
    DlgNPC.Free;
    DlgNPC := nil;
    //Log.Log('a6');
    DlgJournal.Free;
    DlgJournal := nil;
    //Log.Log('a7');
    DlgOptions.Free;
    DlgOptions := nil;
    ///Log.Log('a8');
    DlgLoad.Free;
    DlgLoad := nil;
    //Log.Log('a9');
    DlgProgress.Free;
    DlgProgress := nil;
    //Log.Log('a10');
    DlgText.Free;
    DlgText := nil;
    //Log.Log('a11');
    DlgIntro.Free;
    DlgIntro := nil;
    //Log.Log('a12');
    DlgCreation.Free;
    DlgCreation := nil;
    //Log.Log('a13');
    DlgLoot.Free;
    DlgLoot := nil;
    //Log.Log('a14');
    DlgTitles.Free;
    DlgTitles := nil;
    //Log.Log('a15');
    DlgShow.Free;
    DlgShow := nil;
    //Log.Log('a16');
    DlgQuestLog.Free;
    DlgQuestLog := nil;
    //Log.Log('a17');
    DlgAdvLog.Free;
    DlgAdvLog := nil;
    //Log.Log('a18');
    DlgRoster.Free;
    DlgRoster := nil;
    //Log.Log('a19');
    DlgTransit.Free;
    DlgTransit := nil;

    FreeSpells;

    Log.Log( 'Freeing console' );
    OverlayB := nil;
    OverlayR := nil;
    ManaEmpty := nil;
    LifeEmpty := nil;
    SpellBar := nil;
    SpellGlyphs := nil;
    ShadowImage := nil;
    NoSpellIcon := nil;
    HelpBox := nil;
    PauseImage := nil;
    Game.AutoTransparentMask := nil;
    GlowImage.Free;

    Popup.Free;
    MouseCursor.Free;
    Log.Log( 'Terminating DFX' );
    DFXShutdown;
    Log.Log( 'Terminating DX...' );
    Game.CloseDX;
    Log.Log( 'DX terminated' );
    if ( Seconds > 0 ) then
    begin
      Log.Log( 'Frame Rate=' + IntToStr( Game.FrameCount div Seconds ) );
    end;

    Screen.Cursor := crDefault;

    Log.Log( 'Debug=' + IntToStr( Debug ) );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.FormMouseDown( Sender : TObject; Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer );
var
  i : Integer;
  pr : TRect;
  Ymin1, Ymin2, Ymax, Yorg : Integer;
const
  FailName : string = 'Main.FormMouseDown';
begin
  try

    if DisableConsole then
      Exit;
    if Game.Enabled then
    begin
      if FSpellBarActive then
      begin
        //TODO: Refactor - code smell below - spell Rect logic - should be added to screenMetrics
        if ScreenMetrics.IniIdent='HD' then
        begin
          Ymin1 := 606;
          Ymin2 := 639;
          Ymax := 715;
          Yorg := 640;
        end
        else if ScreenMetrics.IniIdent='FullHD' then
        begin
          Ymin1 := 966;
          Ymin2 := 999;
          Ymax := 1075;
          Yorg := 1000;
        end
        else
        begin
          Ymin1 := 486;
          Ymin2 := 519;
          Ymax := 595;
          Yorg := 520;
        end;

        if ( X >= 0 ) and ( X < 696 ) and ( Y >= Ymin1 ) and ( Y < Ymax ) then
        begin
          if ( X >= 9 ) and ( X < 693 ) and ( Y >= Ymin2 ) and ( Y < Ymax ) then
          begin
            i := ( X - 9 ) div 37 + 18 * ( ( Y - Yorg ) div 36 );
            if ( i >= 0 ) and ( i < 36 ) then
            begin
              if Assigned( Spells[ i ] ) then
              begin
                Current.CurrentSpell := Spells[ i ];
                DrawCurrentSpell;
              end;
            end;
          end;
          FSpellBarActive := False;
          Exit;
        end;
      end
      else
      begin
        if ScreenMetrics.popStatsRect.Contains( Point(x, y) ) then
        begin
          i := 0;
          if i < NPCList.Count then
          begin
            if Button = mbLeft then
            begin
              if NPCList.Items[ i ] = Current then
              begin
                if DlgStatistics.Loaded then
                  CloseAllDialogs( DlgStatistics )
                else
                begin
                  DoNotRestartTimer := True;
                  CloseAllDialogs( DlgStatistics );
                  BeginStatistics( Current );
                end;
              end
              else
              begin
                if DlgStatistics.Loaded then
                begin
                  DoNotRestartTimer := True;
                  CloseAllDialogs( DlgStatistics );
                  ChangeFocus( NPCList.Items[ i ] );
                  pr := Rect( 16, 0, 117, 120 );          // 699 and 1819
                  lpDDSFront.BltFast( ScreenMetrics.StatsX, 0, OverlayR, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
                  BeginStatistics( Current );
                end
                else if DlgInventory.Loaded then
                begin
                  DoNotRestartTimer := True;
                  CloseAllDialogs( DlgStatistics );
                  ChangeFocus( NPCList.Items[ i ] );
                  pr := Rect( 16, 0, 117, 120 );
                  lpDDSFront.BltFast( ScreenMetrics.StatsX, 0, OverlayR, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
                  BeginInventory( Current );
                end
                else if Active then
                  ChangeFocus( NPCList.Items[ i ] );
              end;
            end
            else if Button = mbRight then
            begin
              BeginNPC( NPCList.Items[ i ] );
            end;
          end;
          Exit;
        end
        else
        begin  // Teammates
          i := -1;
          if ScreenMetrics.popParty1Rect.Contains( Point(x, y) ) then i := 1
          else if ScreenMetrics.popParty2Rect.Contains( Point(x, y) ) then i := 2;

          if ( MaxPartyMembers=4 ) then //HD always has 4 slots
          begin
            if ScreenMetrics.popParty3Rect.Contains( Point(x, y) ) then i := 3
            else if ScreenMetrics.popParty4Rect.Contains( Point(x, y) ) then i := 4;
          end
          else if ScreenMetrics.popStatsRect.Contains( Point(x, y) ) then i := 0;

          {      if i=0 then begin
                  if Button=mbLeft then begin
                    if NPCList.items[i]=Current then begin
                      if DlgStatistics.Loaded then
                        CloseAllDialogs(DlgStatistics)
                      else begin
                        DoNotRestartTimer:=true;
                        CloseAllDialogs(DlgStatistics);
                        BeginStatistics(Current);
                      end;
                    end;
                  end;
                end  }
          if i >= 0 then
          begin
            if i < NPCList.Count then
            begin
              if Button = mbLeft then
              begin
                if NPCList.Items[ i ] = Current then
                begin
                  if DlgStatistics.Loaded then
                    CloseAllDialogs( DlgStatistics )
                  else
                  begin
                    DoNotRestartTimer := True;
                    CloseAllDialogs( DlgStatistics );
                    BeginStatistics( Current );
                  end;
                end
                else
                begin
                  if DlgStatistics.Loaded then
                  begin
                    DoNotRestartTimer := True;
                    CloseAllDialogs( DlgStatistics );
                    ChangeFocus( NPCList.Items[ i ] );
                    pr := Rect( 0, 12, 326, 114 );
                    lpDDSFront.BltFast( 0, ScreenMetrics.StatsY, OverlayB, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
                    pr := Rect( 16, 0, 117, 120 );
                    lpDDSFront.BltFast( ScreenMetrics.StatsX, 0, OverlayR, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
                    BeginStatistics( Current );
                  end
                  else if DlgInventory.Loaded then
                  begin
                    DoNotRestartTimer := True;
                    CloseAllDialogs( DlgStatistics );
                    ChangeFocus( NPCList.Items[ i ] );
                    pr := Rect( 0, 12, 326, 114 );
                    lpDDSFront.BltFast( 0, ScreenMetrics.StatsY, OverlayB, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
                    pr := Rect( 16, 0, 117, 120 );
                    lpDDSFront.BltFast( ScreenMetrics.StatsX, 0, OverlayR, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
                    BeginInventory( Current );
                  end
                  else if Active then
                    ChangeFocus( NPCList.Items[ i ] );
                end;
              end
              else if Button = mbRight then
              begin
                BeginNPC( NPCList.Items[ i ] );
              end;
            end;
            Exit;
          end;
        end;
      end;

      if Assigned( Current ) then
      begin
        if ScreenMetrics.popInventoryRect.Contains( Point(x, y) ) then
        begin
          if DlgInventory.Loaded then
            CloseAllDialogs( DlgInventory )
          else
          begin
            DoNotRestartTimer := True;
            CloseAllDialogs( DlgInventory );
            BeginInventory( Current );
          end;
        end
        else if ScreenMetrics.popMapRect.Contains( Point(x, y) ) then
        begin
          if DlgMap.Loaded then
            CloseAllDialogs( DlgMap )
          else
          begin
            DoNotRestartTimer := True;
            CloseAllDialogs( DlgMap );
            BeginMap( Current );
          end;
        end
        else if ScreenMetrics.popJournalRect.Contains( Point(x, y) ) then
        begin
          if DlgJournal.Loaded then
            CloseAllDialogs( DlgJournal )
          else
          begin
            DoNotRestartTimer := True;
            CloseAllDialogs( DlgJournal );
            BeginJournal;
          end;
        end
        else if ScreenMetrics.popAdventureRect.Contains( Point(x, y) ) then
        begin
          if DlgAdvLog.Loaded then
            CloseAllDialogs( DlgAdvLog )
          else
          begin
            DoNotRestartTimer := True;
            CloseAllDialogs( DlgAdvLog );
            BeginAdvLog;
          end;
        end
        else if ScreenMetrics.popQuestRect.Contains( Point(x, y) ) then
        begin
          if DlgQuestLog.Loaded then
            CloseAllDialogs( DlgQuestLog )
          else
          begin
            DoNotRestartTimer := True;
            CloseAllDialogs( DlgQuestLog );
            BeginQuestLog;
          end;
        end
        else if ScreenMetrics.popAwardsRect.Contains( Point(x, y) ) then
        begin
          if DlgTitles.Loaded then
            CloseAllDialogs( DlgTitles )
          else
          begin
            DoNotRestartTimer := True;
            CloseAllDialogs( DlgTitles );
            BeginTitles( Current );
          end;
        end
        else if ScreenMetrics.popRosterRect.Contains( Point(x, y) ) then
        begin
          if DlgRoster.Loaded then
            CloseAllDialogs( DlgRoster )
          else
          begin
            ChangeFocus( player );
            DoNotRestartTimer := True;
            CloseAllDialogs( DlgRoster );
            BeginRoster( nil );
          end;
          Exit;
        end
        else if ScreenMetrics.popSpellRect.Contains( Point(x, y) ) then
        begin
          FSpellBarActive := not FSpellBarActive;
          if FSpellBarActive then
            DrawSpellGlyphs;
        end;
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.FormMouseMove( Sender : TObject; Shift : TShiftState; X,
  Y : Integer );
var
  S, Info : string;
  i : Integer;
  DC : HDC;
  Ymin1, Ymin2, Ymax, Yorg : Integer;
const
  FailName : string = 'Main.FormMouseMove';
begin
  try
    if FSpellBarActive then
    begin
      //TODO: Refactor - code smell below - I even added more stupidity
      S := '';
      Info := '';  // Game info area
      if ScreenMetrics.IniIdent='HD' then
      begin
        Ymin1 := 606;
        Ymin2 := 639;
        Ymax := 715;
        Yorg := 640;
      end
      else if ScreenMetrics.IniIdent='FullHD' then
      begin
        Ymin1 := 966;
        Ymin2 := 999;
        Ymax := 1075;
        Yorg := 1000;
      end
      else
      begin
        Ymin1 := 486;
        Ymin2 := 519;
        Ymax := 595;
        Yorg := 520;
      end;

      if ( X >= 0 ) and ( X < 696 ) and ( Y >= Ymin1 ) and ( Y < Ymax ) then
      begin
        if ( X >= 9 ) and ( X < 693 ) and ( Y >= Ymin2 ) and ( Y < Ymax ) then
        begin
          i := ( X - 9 ) div 37 + 18 * ( ( Y - Yorg ) div 36 );
          if ( i >= 0 ) and ( i < 36 ) then
          begin
            if Assigned( Spells[ i ] ) then
            begin
              S := Spells[ i ].DisplayName;
              Info := Spells[ i ].GetInfo( Current );
            end;
          end;
        end;
      end;
      if S <> LastSpellName then
      begin
        HelpBox.GetDC( DC );
        try
          BitBlt( DC, 0, 0, imgHelp.width, imgHelp.Height, imgHelp.Canvas.Handle, 0, 0, SRCCOPY );
        finally
          HelpBox.ReleaseDC( DC );
        end;
        DlgText.PlotF13Text( HelpBox, S, 10, 5, 170 );
        LastSpellName := S;
        DlgText.PlotF13Block( HelpBox, Info, 20, 205, 20, 170 );
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.ChangeFocus( Figure : TAniFigure );
var
  i : Integer;
const
  FailName : string = 'Main.ChangeFocus';
begin
  Log.DebugLog(FailName);
  try

    FSpellBarActive := False; //This prevents the player from choosing a spell for the wrong character

    Current.Highlightable := True;
    Current.AIMode := aiParty;
    Current.InitAI;
    i := NPCList.IndexOf( Current );
    Current := TCharacter( Figure );
    Log.Log( 'Change focus to ' + Current.Name );
    AssignMarch;
    if i >= 0 then
      PaintCharacterOnBorder( TSpriteObject( NPCList.Items[ i ] ), i );
    i := NPCList.IndexOf( Current );
    if i >= 0 then
      PaintCharacterOnBorder( TSpriteObject( NPCList.Items[ i ] ), i );
    DrawCurrentSpell;

    Game.KeyFigure := Current;
    Current.AutoTransparent := XRayOn;
    Current.Highlightable := False;
    Current.AIMode := aiNone;
    Current.Track := nil;
    Current.Stand;
    Current.Visible := True;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.AddToParty( Figure : TAniFigure );
var
  i : Integer;
const
  FailName : string = 'Main.AddToParty';
begin
  Log.DebugLog(FailName);
  try

    if not ( Figure is TCharacter ) then
      Exit;
    i := NPCList.IndexOf( Figure );
    if i >= 0 then
      Exit;

    if NPCList.Count <= MaxPartyMembers then
    begin
      TCharacter( Figure ).PrevAIMode := TCharacter( Figure ).AIMode;
      TCharacter( Figure ).AIMode := aiParty;
      TCharacter( Figure ).PrevAlliance := TCharacter( Figure ).Alliance;
      TCharacter( Figure ).Alliance := Player.Alliance;
      TCharacter( Figure ).UseAllegianceOf := Player;
      TCharacter( Figure ).Track := nil;
      TCharacter( Figure ).InitAI;
      i := NPCList.Add( TCharacter( Figure ) );
      PaintCharacterOnBorder( TCharacter( Figure ), i );
      AssignMarch;
    end
    else
    begin
      if DlgConverse.Loaded then
      begin
        NewPartyMember := TCharacter( Figure );
      end
      else
      begin
        BeginRoster( TCharacter( Figure ) );
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.RemoveFromParty( Figure : TAniFigure );
var
  i, j : Integer;
const
  FailName : string = 'Main.RemoveFromParty';
begin
  Log.DebugLog(FailName);
  try

    i := NPCList.IndexOf( Figure );
    if i < 0 then
      Exit;

    TCharacter( Figure ).AIMode := aiIdle;
    TCharacter( Figure ).UseAllegianceOf := nil;
    TCharacter( Figure ).PartyMember := False;

    NPCList.Delete( i );
    AssignMarch;

    PaintCharacterOnBorder( TCharacter( Figure ), i );

    for j := i to NPCList.Count - 1 do
      PaintCharacterOnBorder( TSpriteObject( NPCList.Items[ j ] ), j );

    PaintCharacterOnBorder( nil, NPCList.Count );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.PaintCharacterOnBorder( Figure : TSpriteObject; Slot : Integer );
var
  SrcX, SrcY, DstX, DstY : Integer;
  W, H : Integer;
  Image : TBitmap;
  Surface : IDirectDrawSurface;
  DC : HDC;
  ddsd : TDDSurfaceDesc;
  Bits : BITPLANE;
  OldUseLighting : Boolean;
  OldDrawShadow : Boolean;
  OldComplexShadow : Boolean;
  OldFrame : Integer;
  BitPlane : TBitPlane;
  RLE : TRLESprite;
  OldLightR, OldLightG, OldLightB : Integer;
  OldColorR, OldColorG, OldColorB : Integer;
  OldHighighted : Boolean;
const
  FailName : string = 'Main.PaintCharacterOnBorder';
begin
  Log.DebugLog(FailName);
  try

    if Slot > 5 then
      Exit;
    W := 74;
    H := 130;
    case Slot of
      1 :
        begin
          Image := imgBottomBar;
          Surface := OverlayB;
          DstX := 34 - W div 2;
          DstY := 546 - 486 - H div 2;
        end;
      2 :
        begin
          Image := imgBottomBar;
          Surface := OverlayB;
          DstX := 116 - W div 2;
          DstY := 546 - 486 - H div 2;
        end;
      3 :
        begin
          Image := imgBottomBar;
          Surface := OverlayB;
          DstX := 200 - W div 2;
          DstY := 546 - 486 - H div 2;
        end;
      4 :
        begin
          Image := imgBottomBar;
          Surface := OverlayB;
          DstX := 280 - W div 2;
          DstY := 546 - 486 - H div 2;
        end;
      5 :
        begin
          Image := imgBottomBar;
          Surface := OverlayB;
          DstX := 492 - W div 2;
          DstY := 546 - 486 - H div 2;
        end;
    else
      begin
        Image := imgSidebar;
        Surface := OverlayR;
        DstX := 746 - 683 - W div 2;
        DstY := 66 - H div 2;
      end;
    end;

    SrcX := 0;
    SrcY := 0;

    Clip2( 0, Image.width, DstX, SrcX, W );
    Clip2( 0, Image.Height, DstY, SrcY, H );

    Surface.GetDC( DC );
    BitBlt( DC, DstX, DstY, W, H, Image.Canvas.Handle, DstX, DstY, SRCCOPY );
    if Slot = 0 then
    begin
      if Assigned( Figure ) and ( Figure is TCharacter ) then
      begin
        if TCharacter( Figure ).CombatMode then
        begin
          BitBlt( DC, 21, 0, imgCombat.width, imgCombat.Height, imgCombat.Canvas.Handle, 0, 0, SRCCOPY );
        end;
      end;
    end;
    Surface.ReleaseDC( DC );

    if not Assigned( Figure ) then
      Exit;
    if not Assigned( Figure.Resource ) then
      Exit;

    //if we actually have a figure to draw then reload W,H based on actual width/height
    W := Figure.Width;
    H := Figure.Height;
    case Slot of
      1 :
        begin
          DstX := 34 - W div 2;
          DstY := 546 - 486 - H div 2;
        end;
      2 :
        begin
          DstX := 116 - W div 2;
          DstY := 546 - 486 - H div 2;
        end;
      3 :
        begin
          DstX := 200 - W div 2;
          DstY := 546 - 486 - H div 2;
        end;
      4 :
        begin
          DstX := 280 - W div 2;
          DstY := 546 - 486 - H div 2;
        end;
      5 :
        begin
          DstX := 492 - W div 2;
          DstY := 546 - 486 - H div 2;
        end;
    else
      begin
        DstX := 746 - 683 - W div 2;
        DstY := 66 - H div 2;
      end;
    end;

    SrcX := 0;
    SrcY := 0;

    Clip2( 0, Image.width, DstX, SrcX, W );
    Clip2( 0, Image.Height, DstY, SrcY, H );

    BitPlane := TBitPlane.Create( W, H );
    try
      BitPlane.KeyColor := clFuchsia;
      BitPlane.Clear;
      if SrcX > 0 then
        BitPlane.Bits.BaseX := SrcX;
      if SrcY > 0 then
        BitPlane.Bits.BaseY := SrcY;
      OldUseLighting := Figure.UseLighting;
      OldDrawShadow := TResource( Figure.Resource ).DrawShadow;
      OldComplexShadow := TResource( Figure.Resource ).ComplexShadow;
      OldHighighted := Figure.Highlighted;
      Figure.UseLighting := ( Figure <> Current );
      OldColorR := Figure.ColorR;
      OldColorG := Figure.ColorG;
      OldColorB := Figure.ColorB;
      Figure.ColorR := 0;
      Figure.ColorG := 0;
      Figure.ColorB := 0;
      OldLightR := Figure.LightR;
      OldLightG := Figure.LightG;
      OldLightB := Figure.LightB;
      Figure.LightR := 96;
      Figure.LightG := 80;
      Figure.LightB := 32;
      TResource( Figure.Resource ).DrawShadow := False;
      TResource( Figure.Resource ).ComplexShadow := False;
      OldFrame := Figure.Frame;
      Figure.Highlighted := False;
      Figure.ForceFrame( 21 );
      TResource( Figure.Resource ).RenderLocked( Figure, BitPlane.Bits );
      Figure.UseLighting := OldUseLighting;
      TResource( Figure.Resource ).DrawShadow := OldDrawShadow;
      TResource( Figure.Resource ).ComplexShadow := OldComplexShadow;
      Figure.ForceFrame( OldFrame );
      Figure.LightR := OldLightR;
      Figure.LightG := OldLightG;
      Figure.LightB := OldLightB;
      Figure.ColorR := OldColorR;
      Figure.ColorG := OldColorG;
      Figure.ColorB := OldColorB;
      Figure.Highlighted := OldHighighted;

      RLE := TRLESprite.Create;
      try
        BitPlane.Bits.BaseX := 0;
        BitPlane.Bits.BaseY := 0;
        RLE.LoadFromBitPlane( BitPlane );

        ddsd.dwSize := SizeOf( ddsd );
        if Surface.Lock( nil, ddsd, DDLOCK_WAIT, 0 ) = DD_OK then
        begin
          try
            Bits.bitsPtr := ddsd.lpSurface;
            Bits.bitsWdh := ResWidth;
            Bits.bitsHgh := ResHeight;
            Bits.bitsFmt := dfx_pixelformat;
            Bits.bitsPitch := ddsd.lPitch;
            Bits.BaseX := -DstX;
            Bits.BaseY := -DstY;
            if Figure = Current then
              RLE.Draw( 0, 0, 0, @Bits )
            else
            begin
              if Slot = 0 then
                RLE.DrawColorize( 0, 0, 0, @Bits, 96, 80, 32, 125, 25 )
              else
                RLE.DrawBlend( 0, 0, 0, @Bits, 75, 50 );
            end;
            RLE.DrawAntiAlias( 0, 0, 0, @Bits );
          finally
            Surface.Unlock( nil );
          end;
        end;
      finally
        RLE.Free;
      end;
    finally
      BitPlane.Free;
    end;

    if Slot = 0 then
    begin
      DlgText.PlotF13Text( Surface, Figure.Name, 24, 108, 170 );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TfrmMain.LoadResources : Boolean;
var
  pr : TRect;
const
  FailName : string = 'Main.LoadResources';
begin
  Result := False;
  Log.DebugLog(FailName);
  try

    try
      Log.Log( 'Loading console...' );
      SpellGlyphs := SoAOS_DX_LoadBMP( InterfacePath + 'SpellGlyphs.bmp', cBlackBackground );
      imgCombat.LoadFromFile( InterfacePath + 'combat.bmp' );
      imgBottomBar.LoadFromFile( InterfaceLanguagePath + ScreenMetrics.bottombarFile + '.bmp' );
      OverlayB := SoAOS_DX_SurfaceFromBMP( imgBottomBar, cTransparent );

      imgSidebar.LoadFromFile( InterfaceLanguagePath + ScreenMetrics.sidebarFile + '.bmp' );
      OverlayR := SoAOS_DX_SurfaceFromBMP( imgSidebar, cTransparent );

      ManaEmpty := SoAOS_DX_LoadBMP( InterfacePath + 'mana.bmp', cBlackBackground );
      LifeEmpty := SoAOS_DX_LoadBMP( InterfacePath + 'health.bmp', cBlackBackground );

      imgSpellBar.LoadFromFile( InterfaceLanguagePath + ScreenMetrics.spellbarFile + '.bmp' );
      SpellBar := SoAOS_DX_SurfaceFromBMP( imgSpellBar, cTransparent );

      ShadowImage := SoAOS_DX_LoadBMPResource( 'shadow', cBlackBackground );
      imgAutoTransparent.LoadFromResourceName( HInstance, 'xray' );
      Game.AutoTransparentMask := imgAutoTransparent;

      NoSpellIcon := DDGetSurface( lpDD, 32, 32, clBlack, False );

      pr := Rect( 456, 64, 456 + 32, 64 + 32 );
      NoSpellIcon.BltFast( 0, 0, OverlayB, @pr, DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
      imgHelp.LoadFromFile( InterfacePath + 'spellinfo.bmp' );
      HelpBox := SoAOS_DX_LoadBMP( InterfacePath + 'spellinfo.bmp', cTransparent );
      PauseImage := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'paused.bmp', cTransparent );

      imgGlow := TBitmap.Create;
      try
        imgGlow.LoadFromResourceName( Hinstance, 'glow' );
        GlowImage := TRLESprite.Create;   // Loadfrom BMP
        GlowImage.LoadFromBitmap( imgGlow, imgGlow.width, imgGlow.Height, 0 );
      finally
        imgGlow.Free;
      end;

      Log.Log( 'Console load Complete' );

      ScreenShot := TBitmap.Create;
      ScreenShot.width := 225;
      // Beware below fix is temporary - due to issue with DDrawCompat ddraw.dll
      // ScreenShot.Height := -162;
      ScreenShot.Height := 162;

      Log.Log( 'Loading interface...' );
      DlgIntro := TIntro.Create; // Create first due to Intercase check
      DlgConverse := TConverseBox.Create;
      DlgInventory := TInventory.Create;
      DlgMerchant := TMerchant.Create;
      DlgLoot := TLootCorpse.Create;
      DlgObjInventory := TObjInventory.Create;
      DlgMap := TMap.Create;
      DlgTitles := TAward.Create;
      DlgJournal := TJournal.Create;
      DlgOptions := TOptions.Create;
      DlgLoad := TLoadGame.Create;
      DlgProgress := TLoaderBox.Create;

      DlgNPC := TNPCBehavior.Create;
      DlgShow := TShowGraphic.Create;
      DlgCreation := TCreation.Create;
      DlgText := TGameText.Create;
      DlgText.Load13Graphic;
      DlgStatistics := TStatistics.Create;
      DlgQuestLog := TQuestLog.Create;
      DlgAdvLog := TAdvLog.Create;
      DlgRoster := TAddKickNPC.Create;
      DlgTransit := TTransit.Create;

      Log.Log( 'Interface load Complete' );

      if not LoadSpells then
        Exit;

      Game.Interval := Interval;

    finally
      //    Screen.Cursor := crDefault;
    end;

    Result := True;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.LoadNewMapFile;
var
  S : string;
const
  FailName : string = 'Main.LoadNewMapFile';
begin
  Log.DebugLog(FailName);
  try

    S := GameName;
    try
      GameName := TempGame;
      Log.Log( 'SaveGame' );
      SaveGame;
      Log.Log( 'Copy files' );
      ForceNotReadOnly( GamesPath + '~End of Level.sav' );
      ForceNotReadOnly( GamesPath + '~End of Level.idx' );
      ForceNotReadOnly( GamesPath + '~End of Level.map' );
      try
        TFile.Copy( GamesPath + GameName + '.sav', GamesPath + '~End of Level.sav', True );
      except
      end;
      try
        TFile.Copy( GamesPath + GameName + '.idx', GamesPath + '~End of Level.idx', True );
      except
      end;
      try
        TFile.Copy( GamesPath + GameName + '.map', GamesPath + '~End of Level.map', True );
      except
      end;
      Log.Log( 'ClearOnDemandResources' );
      ClearOnDemandResources;
      Log.Log( 'ClearResources' );
      ClearResources( True );
      Log.Log( 'ReAlloc' );
      Sprites.Realloc;
      FSpellBarActive := False;

      LVLFile := NewLVLFile;
      CurrentScene := NewScene;
      CurrentStartingPoint := NewStartingPoint;

      Log.Log( 'LoadMapFile' );
      if not LoadMapFile( False, False ) then
        Exit;
    finally
      GameName := S;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TfrmMain.LoadMapFile( New, FullLoad : Boolean ) : Boolean;
var
  Loaded : Boolean;
  i : Integer;
  L : Longint;
  S : string;
  Stream : TFileStream;
  IgnoreDefaultObjects, IgnoreSceneObjects : Boolean;
  Level : AnsiString;
  ZoneTotal, ZoneMem : LongWord;
  Brightness : Longint;
  SceneName : string;
  TimeStamp : TDateTime;
  INILanguage : TMemIniFile;
const
  FailName : string = 'Main.LoadMapFile';
begin
  Result := True;
  Log.DebugLog(FailName);
  try

    Log.Log( '-----------------------------------------------------' );
    //  Screen.Cursor := crHourglass;
    MouseCursor.Enabled := False;
    if TransitionScreen = '' then
      S := DefaultTransition
    else
    begin
      S := ResourcePath + 'Transition\' + TransitionScreen + '.bmp';
      if not TFile.Exists( S ) then
        S := DefaultTransition;
    end;

    if TFile.Exists( S ) then
      DlgProgress.FileName := S
    else
      DlgProgress.FileName := '';

    Log.Log( 'Init' );
    DlgProgress.Init;
    try
      Game.KeyFigure := nil;
      SpawnList.Clear;
      Level := AnsiString( LowerCase( ChangeFileExt( ExtractFileName( LVLFile ), '' ) ) );

      if ( CurrentScene = '' ) or ( LowerCase( CurrentScene ) = 'default' ) then
        SceneName := 'Default Scene'
      else
        SceneName := CurrentScene;

      Log.Log( 'Loading map...' );

      IgnoreDefaultObjects := False;
      IgnoreSceneObjects := False;
      i := TravelList.IndexOf( Level );
      if i >= 0 then
        IgnoreDefaultObjects := True;
      S := Level + '|' + LowerCase( CurrentScene );
      i := TravelList.IndexOf( S );
      if i >= 0 then
        IgnoreSceneObjects := True; //was <

      Loaded := LoadMap( GameMap, LVLFile, CurrentScene, IgnoreDefaultObjects, IgnoreSceneObjects );
      if not Loaded then
      begin
        Log.Log( 'Error in load' );
        Log.Log( 'Map could not be loaded' );
        //Error Message
        FreeAll;
        Close;
        Result := False;
        Exit;
      end;
      Log.Log( 'Map load complete' );

      INILanguage := TMemIniFile.Create( SiegeINILanguageFile, TEncoding.ANSI );
      try
        MapName := INILanguage.ReadString( 'MapNames', Level, Level );
        Log.Log(Level+' ### '+MapName);
      finally
        INILanguage.Free;
      end;
      DlgProgress.SetBar( Round( DlgProgress.MaxValue * 0.81 ) );

      if not New then
      begin
        Log.Log( 'Loading scene' );
        if not LoadGame( FullLoad ) then
        begin
          FreeAll;
          Close;
          Result := False;
          Exit;
        end;
      end;
      DlgProgress.SetBar( Round( DlgProgress.MaxValue * 0.87 ) );

      DlgProgress.SetBar( Round( DlgProgress.MaxValue * 0.88 ) );

      try
        Randomize;

        DefaultPants := PartManager.GetLayerResource( 'HumanMaleLayers\BrownPants' );
//        FemDefaultPants := PartManager.GetLayerResource('HumanFemale2Layers\BrownTights');
        FemDefaultPants := nil;
        ElfDefaultPants := PartManager.GetLayerResource( 'ElfMaleLayers\ElfTightsBrown' );

        //Stuff for summoning spells
        RatResource := TCharacterResource( PartManager.GetOnDemandResource( 'SpriteObject\Character\Animals\Rat' ) );
        WolfResource := TCharacterResource( PartManager.GetOnDemandResource( 'SpriteObject\Character\Animals\Wolf1' ) );
        GolemResource := TCharacterResource( PartManager.GetOnDemandResource( 'SpriteObject\Character\Monsters\Golem\Rockgolem' ) );

        for i := 0 to NPCList.Count - 1 do
        begin
          TCharacter( NPCList.Items[ i ] ).Enabled := True;
          //        TCharacter(NPCList.items[i]).UseDefaultEquipment:=New;
          //        TCharacter(NPCList.items[i]).AIMode:=aiParty;
          //        TCharacter(NPCList.items[i]).Init;
          //        TCharacter(NPCList.items[i]).UseDefaultEquipment:=False;
        end;

        for i := 0 to FigureInstances.Count - 1 do
        begin
          if TObject( FigureInstances.Objects[ i ] ) is TGameObject then
          begin
            if TObject( FigureInstances.Objects[ i ] ) is TDoor then
            begin
              //            if not Usecache then TGameObject(FigureInstances.Objects[i]).Init;
            end
            else if TObject( FigureInstances.Objects[ i ] ) is TCharacter then
            begin
              if TCharacter( FigureInstances.Objects[ i ] ).PartyMember and TCharacter( FigureInstances.Objects[ i ] ).Enabled then
              begin
                TCharacter( FigureInstances.Objects[ i ] ).UseDefaultEquipment := New;
                TCharacter( FigureInstances.Objects[ i ] ).AIMode := aiParty;
                TCharacter( FigureInstances.Objects[ i ] ).Init;
                TCharacter( FigureInstances.Objects[ i ] ).UseDefaultEquipment := False;
              end
              else
              begin
                TCharacter( FigureInstances.Objects[ i ] ).Init;
              end;
            end
            else
            begin
              TGameObject( FigureInstances.Objects[ i ] ).Init;
            end;

          end;
        end;

        for i := 0 to FigureInstances.Count - 1 do
        begin
          if TObject( FigureInstances.Objects[ i ] ) is TSpriteObject then
          begin
            Game.AddFigure( TAniFigure( FigureInstances.Objects[ i ] ) );
            if TObject( FigureInstances.Objects[ i ] ) is TCharacter then
            begin
              if TCharacter( FigureInstances.Objects[ i ] ).Enabled then
                TCharacter( FigureInstances.Objects[ i ] ).InitAI;
            end;
          end;
        end;

        DlgProgress.SetBar( Round( DlgProgress.MaxValue * 0.86 ) );

        Log.Log( 'Freeing unused parts' );
        PartManager.ClearUnusedParts;

        DlgProgress.SetBar( Round( DlgProgress.MaxValue * 0.89 ) );

        Current := Player;
        AssignMarch;
        Game.KeyFigure := Player;
        Current.AIMode := aiNone;
        Current.Highlightable := False;
        Current.AutoTransparent := XRayOn;
        Log.Log( 'KeyFigure=' + Current.GUID );

        for i := 0 to NPCList.Count - 1 do
        begin
          PaintCharacterOnBorder( TSpriteObject( NPCList.Items[ i ] ), i );
        end;
        DrawCurrentSpell;
        Current.AutoFight := False;

        DlgProgress.SetBar( Round( DlgProgress.MaxValue * 0.9 ) );

        Log.Log( 'Freeing unused resources' );
        GameMap.FreeDefinitions;
        DlgProgress.SetBar( Round( DlgProgress.MaxValue * 0.91 ) );
        Log.Log( 'Sorting' );
        GameMap.Sort;
        DlgProgress.SetBar( Round( DlgProgress.MaxValue * 0.92 ) );
        Log.Log( 'Rendering light sources' );
        GameMap.RenderMap;
        DlgProgress.SetBar( Round( DlgProgress.MaxValue * 0.98 ) );

        S := MapPath + ChangeFileExt( ExtractFileName( LVLFile ), '.zit' );
        if TFile.Exists( S ) then
        begin
          ForceNotReadOnly( S );
          Stream := TFileStream.Create( S, fmOpenReadWrite or fmShareDenyWrite );
        end
        else
          Stream := TFileStream.Create( S, fmCreate or fmShareDenyWrite );
        try
          for i := 0 to GameMap.ZoneCount - 1 do
          begin
            GameMap.Zones[ i ].SaveToStream( Stream, False );
          end;
        finally
          Stream.Free;
        end;

        DlgProgress.SetBar( Round( DlgProgress.MaxValue * 0.99 ) );

        DlgProgress.SetBar( DlgProgress.MaxValue );

        Log.Log( 'Memory usage' );
        Log.Log( '  Map: ' + IntToStr( GameMap.Width * GameMap.Height * SizeOf( GridInfo ) ) );
        Log.Log( '  Sprites: ' + IntToStr( PartManager.MemSize ) );
        ZoneTotal := 0;
        for i := 0 to GameMap.ZoneCount - 1 do
        begin
          ZoneMem := GameMap.GetZoneMemoryUsage( i ) * 2;
          Inc( ZoneTotal, ZoneMem );
          if GameMap.Zones[ i ] is TLightZone then
            Log.Log( '  Zone ' + IntToStr( i ) + '*: ' + IntToStr( ZoneMem ) )
          else
            Log.Log( '  Zone ' + IntToStr( i ) + ': ' + IntToStr( ZoneMem ) );
        end;
        Log.Log( '  Total all zones: ' + IntToStr( ZoneTotal ) );
        Log.Log( 'Map item count=' + IntToStr( GameMap.ItemCount ) );

        Log.Log( 'Assigning map to game environment' );
        Game.Map := GameMap;
        if UseVideoRAM then
        begin
          for i := 0 to GameMap.ZoneCount - 1 do
            if not ( GameMap.Zones[ i ] is TLightZone ) then
              GameMap.MoveZoneToVideo( i );

          for i := 1 to GameMap.ZoneCount - 1 do
            if ( GameMap.Zones[ i ] is TLightZone ) then
              GameMap.MoveZoneToVideo( i );
        end;
      except
        on E : Exception do
        begin
          Log.log( FailName, E.Message, [ ] );
          Result := False;
          Log.Log( 'Could not start game because of error' );
          FreeAll;
          Close;
          Exit;
        end;
      end;
      Game.ItemMask := 0;
    finally
      if Assigned( DlgProgress ) then
        DlgProgress.Release;
    end;

    Log.Log( 'Initialization complete' );
    Log.Log( 'Setting theme' );
    if Themes.Count > 0 then
    begin
      FCurrentTheme := Themes.Names[ 0 ];
      S := Parse( AnsiString( Themes.Strings[ 0 ] ), 1, '=' );
    end
    else
    begin
      FCurrentTheme := '';
      S := '';
    end;
    {if Game.FrameCount=0 then begin
      S:='theme\canyon';
      Log.Log('Canyon');
    end;    }
    CueTune( S, False );

    //Run load script
    Log.Log( 'Running load scripts' );
    for i := 0 to FigureInstances.Count - 1 do
    begin
      if FigureInstances.Objects[ i ] is TGameObject then
      begin
        TGameObject( FigureInstances.Objects[ i ] ).DoLoad;
      end;
    end;

    MouseCursor.Enabled := True;
    Log.Log( 'Load complete' );

    if not FullLoad then
    begin
      PlaceNPCList;
      Log.Log( 'Saving "Start of Level"' );
      SaveAGame( SOLName );
    end;


    if Player.BaseHitPoints < 1 then
    begin // jrs 6Nov01 Restore norm/default base hitpoints to Player
      Log.Log( '-- FIXUP, Player.HitPoints was ' + FloatToStr( Player.HitPoints ) + ' and BaseHitPoints was ' + FloatToStr( Player.BaseHitPoints ) );
      Player.HitPoints := 20;
      Player.CalcStats; // just to be sure
      Log.Log( '-- Player.HitPoints is now ' + FloatToStr( Player.HitPoints ) + ' and BaseHitPoints is now ' + FloatToStr( Player.BaseHitPoints ) );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TfrmMain.SaveGame : Boolean;
var
  EOB : Word;
  Level : AnsiString;

  function GetPlayerData : TMemoryStream;
  var
    i, j : Integer;
    Block : TSavBlocks;
    List : TStringList;
    L : Longint;
    S : AnsiString;
    k : TSlot;
  begin
    Result := TMemoryStream.Create;
    List := TStringList.Create;
    try
      for i := 0 to NPCList.Count - 1 do
      begin
        Block := sbCharacter;
        Result.Write( Block, SizeOf( Block ) );
        List.Clear;
        TCharacter( NPCList.Items[ i ] ).SaveProperties( List );
        S := AnsiString( List.Text );
        L := Length( S );
        Result.Write( L, SizeOf( L ) );
        Result.Write( S[ 1 ], L );
        Result.Write( EOB, SizeOf( EOB ) );

        for k := slLeg1 to SlMisc3 do
        begin
          if Assigned( TCharacter( NPCList.Items[ i ] ).Equipment[ k ] ) then
          begin
            //            Log.Log('Saving equipment item '+TCharacter(NPCList.items[i]).Equipment[k].ItemName);
            Block := sbItem;
            Result.Write( Block, SizeOf( Block ) );
            List.Clear;
            TCharacter( NPCList.Items[ i ] ).Equipment[ k ].SaveProperties( List );
            S := AnsiString( List.Text );
            L := Length( S );
            Result.Write( L, SizeOf( L ) );
            Result.Write( S[ 1 ], L );
            Result.Write( EOB, SizeOf( EOB ) );
          end;
        end;

        for j := 0 to TCharacter( NPCList.Items[ i ] ).Inventory.Count - 1 do
        begin
          //          Log.Log('Saving inventory item '+TItem(TCharacter(NPCList.items[i]).Inventory.items[j]).ItemName);
          Block := sbItem;
          Result.Write( Block, SizeOf( Block ) );
          List.Clear;
          TItem( TCharacter( NPCList.Items[ i ] ).Inventory.Items[ j ] ).SaveProperties( List );
          S := AnsiString ( List.Text );
          L := Length( S );
          Result.Write( L, SizeOf( L ) );
          Result.Write( S[ 1 ], L );
          Result.Write( EOB, SizeOf( EOB ) );
        end;
      end;
    finally
      List.Free;
    end;
  end;

  function GetMpaKnownData : TMemoryStream;
  begin
    Result := TMemoryStream.Create;
    GameMap.SaveMapKnownInfo( Result );
  end;

  function GetProperties : TMemoryStream;
  var
    i, j : Integer;
    Block : TSavBlocks;
    List : TStringList;
    S : AnsiString;
    L, L1, L2 : Longint;
    Flag : Boolean;
  begin
    Result := TMemoryStream.Create;
    List := TStringList.Create;
    try
      L1 := Length( Level );
      for i := 0 to FigureInstances.Count - 1 do
      begin
        if Assigned( FigureInstances.Objects[ i ] ) then
        begin
          Flag := not ( FigureInstances.Objects[ i ] is TSpriteObject ); //Save only sprite objects
          if not Flag then
            Flag := TSpriteObject( FigureInstances.Objects[ i ] ).ShouldSave; //Save only enabled objects
          if Flag then
          begin
            if FigureInstances.Objects[ i ] is TCharacter then
              Block := scCharacter
            else if FigureInstances.Objects[ i ] is TDoor then
              Block := scDoor
            else if FigureInstances.Objects[ i ] is TContainer then
              Block := scContainer
            else if FigureInstances.Objects[ i ] is TBow then
              Block := scBow
            else if FigureInstances.Objects[ i ] is TQuiver then
              Block := scQuiver
            else if FigureInstances.Objects[ i ] is TWeapon then
              Block := scWeapon
            else if FigureInstances.Objects[ i ] is TItem then
              Block := scItem
            else if FigureInstances.Objects[ i ] is TTrigger then
              Block := scTrigger
            else if FigureInstances.Objects[ i ] is TPathCorner then
              Block := scPathCorner
            else if FigureInstances.Objects[ i ] is TSoundPlayer then
              Block := scSoundPlayer
            else if FigureInstances.Objects[ i ] is TEventTimer then
              Block := scEventTimer
            else if FigureInstances.Objects[ i ] is TSpriteObject then
              Block := scSpriteObject
            else
              Block := scAbstract;
            Result.Write( Block, SizeOf( Block ) );
            List.Clear;
            TGameObject( FigureInstances.Objects[ i ] ).SaveProperties( List );
            S := AnsiString( List.Text );
            L2 := Length( S );
            L := SizeOf( L1 ) + L1 + SizeOf( L2 ) + L2;
            Result.Write( L, SizeOf( L ) );
            Result.Write( L1, SizeOf( L1 ) );
            Result.Write( Level[ 1 ], L1 );
            Result.Write( L2, SizeOf( L2 ) );
            Result.Write( S[ 1 ], L2 );
            Result.Write( EOB, SizeOf( EOB ) );
            if Block = scContainer then
            begin
              for j := 0 to TContainer( FigureInstances.Objects[ i ] ).Inventory.Count - 1 do
              begin
                Block := siItem;
                Result.Write( Block, SizeOf( Block ) );
                List.Clear;
                TItem( TContainer( FigureInstances.Objects[ i ] ).Inventory.Items[ j ] ).SaveProperties( List );
                S := AnsiString( List.Text );
                L := Length( S );
                Result.Write( L, SizeOf( L ) );
                Result.Write( S[ 1 ], L );
                Result.Write( EOB, SizeOf( EOB ) );
              end;
            end;
          end;
        end;
      end;
    finally
      List.Free;
    end;
  end;

var
  SavFile : TSavFile;
  i : Integer;
  S : string;
  FileName : string;
const
  FailName : string = 'Main.SaveGame';
begin
  Result := False;
  Log.DebugLog(FailName);
  try
    Level := AnsiString( LowerCase( ChangeFileExt( ExtractFileName( LVLFile ), '' ) ) );

    i := TravelList.IndexOf( Level );
    if i < 0 then
      TravelList.Add( Level );
    S := Level + '|' + LowerCase( CurrentScene );
    i := TravelList.IndexOf( S );
    if i < 0 then
      TravelList.Add( S );

    EOB := EOBMarker;
    if not TDirectory.Exists( GamesPath ) then
      ForceDirectories( GamesPath );

    FileName := GamesPath + GameName + '.sav';
    ForceNotReadOnly( FileName );
    ForceNotReadOnly( ChangeFileExt( FileName, '.idx' ) );
    ForceNotReadOnly( ChangeFileExt( FileName, '.map' ) );

    SavFile := TSavFile.Create;
    try
      SavFile.Open( GamesPath + TempGame + '.sav' );
      SavFile.MapName := Level;
      SavFile.SceneName := CurrentScene;
      SavFile.CurrentMap := Level;
      SavFile.CurrentScene := CurrentScene;
      SavFile.TravelList := TravelList;
      SavFile.JournalList := AdventureLog1.LogFileList;
      SavFile.JournalIndex := DlgJournal.StartLogIndex;
      SavFile.QuestList := Quests;
      SavFile.QuestIndex := DlgQuestLog.PageNumber;
      SavFile.AdventureList := Adventures;
      SavFile.AdventureIndex := DlgAdvLog.PageNumber;
      SavFile.DeathScreen := DeathScreen;
      SavFile.MaxPartyMembers := MaxPartyMembers;
      SavFile.PartyMembers := GetPlayerData;
      SavFile.MapKnown := GetMpaKnownData;
      SavFile.Properties := GetProperties;
      SavFile.SaveAs( FileName );
      SavFile.PartyMembers.Free;
      SavFile.MapKnown.Free;
      SavFile.Properties.Free;
    finally
      SavFile.Free;
    end;
    Result := True;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TfrmMain.ClearResources( PreserveResourceList : Boolean ) : Boolean;
var
  i : Integer;
const
  FailName : string = 'Main.ClearResources';
begin
  Result := True;
  Log.DebugLog(FailName);
  try
    HLFigure := nil;

    for i := 0 to FigureInstances.Count - 1 do
    begin
      try
        if Assigned( FigureInstances.Objects[ i ] ) and ( FigureInstances.Objects[ i ] is TAbstractObject ) then
        begin
          TAbstractObject( FigureInstances.Objects[ i ] ).Free;
        end;
      except
        Result := False;
        Log.Log( '*** Error: FigureInstances.Free' );
      end;
    end;
    if not PreserveResourceList then
    begin
      for i := 0 to Figures.Count - 1 do
      begin
        try
          if Assigned( Figures.Objects[ i ] ) then
            TResource( Figures.Objects[ i ] ).Free;
        except
          Result := False;
          Log.Log( '*** Error: Figures.Free' );
        end;
      end;
      try
        Figures.Clear;
      except
        Result := False;
        Log.Log( '*** Error: Figures.Clear' );
      end;
    end;
    try
      Game.FreeResources;
    except
      Result := False;
      Log.Log( '*** Error: Game.FreeResources' );
    end;
    try
      GameMap.Clear;
    except
      Result := False;
      Log.Log( '*** Error: GameMap.Clear' );
    end;
    try
      FigureInstances.Clear;
    except
      Result := False;
      Log.Log( '*** Error: FigureInstances.Clear' );
    end;
    try
      Sounds.Clear;
    except
      Result := False;
      Log.Log( '*** Error: Sounds.Clear' );
    end;
    try
      ActiveTriggers.Clear;
    except
      Result := False;
      Log.Log( '*** Error: ActiveTriggers.Clear' );
    end;
    try
      SayList.Clear;
    except
      Result := False;
      Log.Log( '*** Error: SayList.Clear' );
    end;
    try
      Themes.Clear;
    except
      Result := False;
      Log.Log( '*** Error: Themes.Clear' );
    end;
    try
      Player := nil;
    except
      Result := False;
      Log.Log( '*** Error: Player' );
    end;
    try
      Current := nil;
    except
      Result := False;
      Log.Log( '*** Error: Current' );
    end;
    if Assigned( BodyRotResource ) then
    try
      BodyRotResource.Free;
      BodyRotResource := nil;
    except
      Result := False;
      Log.Log( '*** Error: BodyRotResource' );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TfrmMain.LoadGame( FullLoad : Boolean ) : Boolean;
var
  EOB, BB : Word;

  procedure LoadPlayerData( Stream : TStream );
  var
    NewCharacter : TCharacter;
    ItemIndex, EquipmentCount : Integer;
    k : TSlot;
    List : TStringList;
    Block : TSavBlocks;
    L, P : Longint;
    S, S1 : AnsiString;
    i, j : Integer;
  begin
    if not Assigned( Stream ) then
      Exit;
    NPCList.Clear;
    EquipmentCount := 0;
    ItemIndex := 0;
    NewCharacter := nil;
    LoadingFromSaveFile := True;
    List := TStringList.Create;
    try
      while Stream.Position < Stream.Size do
      begin
        Stream.Read( Block, SizeOf( Block ) );
        Stream.Read( L, SizeOf( L ) );
        P := Stream.Position;
        case Block of
          sbCharacter :
            begin
              if L > 0 then
              begin
                NewCharacter := TCharacter.Create( 0, 0, 0, 0, True );
                SetLength( S, L );
                Stream.Read( S[ 1 ], L );
                List.Text := S;
                S := AnsiString( AnsiLowerCase( List.Values[ 'Resource' ] ) );
                i := Pos( 'players\player', S );
                if i > 0 then
                begin
                  j := i + length( 'players\player' );
                  while j <= length( S ) do
                  begin
                    if S[ j ] = '.' then
                    begin
                      dec( j );
                      break;
                    end;
                    inc( j );
                  end;
                  S := copy( S, i, j - i + 1 ) + '.gif';
                  S1 := AnsiString (ChangeFileExt( S, '' ) );
                  i := Figures.IndexOf( S1 );
                  if i >= 0 then
                  begin
                    NewCharacter.Resource := TResource( Figures.Objects[ i ] );
                    TResource( NewCharacter.Resource ).Reload := True;
                  end
                  else
                  begin
                    NewCharacter.Resource := LoadArtResource( S );
                    i := Figures.Add( S1 );
                    Figures.Objects[ i ] := NewCharacter.Resource;
                  end;
                  S := AnsiString( List.Values[ 'HeadLayer' ] );
                  TCharacterResource( NewCharacter.Resource ).HeadName := S;
                  if S <> '' then
                    TCharacterResource( NewCharacter.Resource ).HeadResource := PartManager.GetLayerResource( S );
                end
                else
                begin
                  i := Figures.IndexOf( S );
                  if i >= 0 then
                  begin
                    NewCharacter.Resource := TResource( Figures.Objects[ i ] );
                    TResource( NewCharacter.Resource ).Reload := True;
                  end
                  else
                  begin
                    NewCharacter.Resource := LoadArtResource( S );
                    i := Figures.Add( S );
                    Figures.Objects[ i ] := NewCharacter.Resource;
                  end;
                  S := AnsiString( List.Values[ 'HeadLayer' ] );
                  TCharacterResource( NewCharacter.Resource ).HeadName := S;
                  if S <> '' then
                    TCharacterResource( NewCharacter.Resource ).HeadResource := PartManager.GetLayerResource( S );
                end;

                NewCharacter.NoItemPlacement := True;
                LoadingFromSaveFile := False;
                NewCharacter.LoadProperties( List );
                LoadingFromSaveFile := True;
                NewCharacter.LoadEquipment( False );
                NewCharacter.NoItemPlacement := False;
                NewCharacter.UseDefaultEquipment := False;
                NPCList.Add( NewCharacter );
                EquipmentCount := 0;
                j := FigureInstances.Add( NewCharacter.GUID );
                FigureInstances.Objects[ j ] := NewCharacter;

                for k := slLeg1 to SlMisc3 do
                begin
                  if Assigned( NewCharacter.Equipment[ k ] ) then
                    Inc( EquipmentCount );
                end;
                ItemIndex := 0;
              end;
            end;
          sbItem :
            begin
              if Assigned( NewCharacter ) and ( L > 0 ) then
              begin
                SetLength( S, L );
                Stream.Read( S[ 1 ], L );
                List.Text := S;
                Inc( ItemIndex );
                if ( ItemIndex > EquipmentCount ) then
                begin
                  i := ItemIndex - EquipmentCount - 1;
                  if i < NewCharacter.Inventory.Count then
                  begin
                    LoadingFromSaveFile := False;
                    TItem( NewCharacter.Inventory.Items[ i ] ).LoadProperties( List );
                    LoadingFromSaveFile := True;
                    TItem( NewCharacter.Inventory.Items[ i ] ).GUID := '';
                    TItem( NewCharacter.Inventory.Items[ i ] ).Enabled := False;
                  end;
                end
                else
                begin
                  i := 0;
                  for k := slLeg1 to SlMisc3 do
                  begin
                    if Assigned( NewCharacter.Equipment[ k ] ) then
                    begin
                      Inc( i );
                      if i = ItemIndex then
                      begin
                        LoadingFromSaveFile := False;
                        NewCharacter.Equipment[ k ].LoadProperties( List );
                        LoadingFromSaveFile := True;
                        NewCharacter.Equipment[ k ].GUID := '';
                        NewCharacter.Equipment[ k ].Enabled := False;
                        Break;
                      end;
                    end;
                  end;
                end;
              end;
            end;
        end;
        Stream.Seek( P + L, soFromBeginning );
        Stream.Read( BB, SizeOf( BB ) );
        if BB <> EOB then
        begin
          Log.Log( '*** Error:  EOB not found' );
          Exit;
        end;
      end;
    finally
      LoadingFromSaveFile := False;
      List.Free;
    end;
  end;

  procedure LoadMapKnown( Stream : TStream );
  begin
    if not Assigned( Stream ) then
      Exit;
    GameMap.LoadMapKnownInfo( Stream );
  end;

  procedure LoadProperties( Stream : TStream; const Scene : string );
  var
    ObjectRef : TGameObject;
    LastContainer : TContainer;
    LastContainerInventoryCount : Integer;
    List : TStringList;
    Block : TSavBlocks;
    L, L1, P : Longint;
    S, S1 : AnsiString;
    i, j : Integer;
    Flag : Boolean;
    GUID : string;
  begin
    if not Assigned( Stream ) then
      Exit;
    LastContainer := nil;
    LastContainerInventoryCount := 0;
    LoadingFromSaveFile := True;
    List := TStringList.Create;
    try
      while Stream.Position < Stream.Size do
      begin
        Stream.Read( Block, SizeOf( Block ) );
        Stream.Read( L, SizeOf( L ) );
        P := Stream.Position;
        case Block of
          siItem :
            begin
              if Assigned( LastContainer ) then
              begin
                SetLength( S, L );
                Stream.Read( S[ 1 ], L );
                List.Text := S;
                if LastContainerInventoryCount < LastContainer.Inventory.Count then
                begin
                  LoadingFromSaveFile := False;
                  TItem( LastContainer.Inventory.Items[ LastContainerInventoryCount ] ).LoadProperties( List );
                  LoadingFromSaveFile := True;
                  TItem( LastContainer.Inventory.Items[ LastContainerInventoryCount ] ).GUID := '';
                  TItem( LastContainer.Inventory.Items[ LastContainerInventoryCount ] ).Enabled := False;
                end;
              end;
              Inc( LastContainerInventoryCount );
            end;
        else
          begin
            LastContainer := nil;
            Flag := False;
            case Block of
              scCharacter : Flag := True;
              scDoor : Flag := True;
              scContainer : Flag := True;
              scBow : Flag := True;
              scQuiver : Flag := True;
              scWeapon : Flag := True;
              scItem : Flag := True;
              scTrigger : Flag := True;
              scPathCorner : Flag := True;
              scSoundPlayer : Flag := True;
              scSpriteObject : Flag := True;
              scAbstract : Flag := True;
            end;
            if Flag then
            begin
              Stream.Read( L1, SizeOf( L1 ) );
              if L1 = 0 then
                S := ''
              else
              begin
                SetLength( S, L1 );
                Stream.Read( S[ 1 ], L1 );
              end;
              Stream.Read( L1, SizeOf( L1 ) );
              if L1 = 0 then
                S := ''
              else
              begin
                SetLength( S, L1 );
                Stream.Read( S[ 1 ], L1 );
              end;
              List.Text := S;
              S := AnsiString( List.Values[ 'InScene' ] );
              if ( S = '' ) or ( Pos( '[' + Scene + ']', S ) <> 0 ) then
              begin
                ObjectRef := nil;
                GUID := List.Values[ 'GUID' ];
                if GUID <> '' then
                begin
                  i := FigureInstances.IndexOf( GUID );
                  if i >= 0 then
                    ObjectRef := TGameObject( FigureInstances.Objects[ i ] );
                end;
                if Assigned( ObjectRef ) then
                begin
                  //                  Log.Log('  Loading properties for existing item '+ObjectRef.GUID);
                  ObjectRef.Enabled := True;
                end
                else
                begin
                  Log.Log( '  Creating new item ' + GUID );
                  case Block of
                    scCharacter : ObjectRef := TCharacter.Create( 0, 0, 0, 0, True );
                    scDoor : ObjectRef := TDoor.Create( 0, 0, 0, 0, True );
                    scContainer : ObjectRef := TContainer.Create( 0, 0, 0, 0, True );
                    scBow : ObjectRef := TBow.Create( 0, 0, 0, 0, True );
                    scQuiver : ObjectRef := TQuiver.Create( 0, 0, 0, 0, True );
                    scWeapon : ObjectRef := TWeapon.Create( 0, 0, 0, 0, True );
                    scItem : ObjectRef := TItem.Create( 0, 0, 0, 0, True );
                    scTrigger : ObjectRef := TTrigger.Create( 0, 0, 0 );
                    scPathCorner : ObjectRef := TPathCorner.Create( 0, 0, 0 );
                    scSoundPlayer : ObjectRef := TSoundPlayer.Create( 0, 0, 0 );
                    scEventTimer : ObjectRef := TEventTimer.Create( 0, 0, 0 );
                    scSpriteObject : ObjectRef := TSpriteObject.Create( 0, 0, 0, 0, True );
                  else
                    ObjectRef := TAbstractObject.Create( 0, 0, 0 );
                  end;
                  i := FigureInstances.Add( GUID );
                  FigureInstances.Objects[ i ] := ObjectRef;
                  if ObjectRef is TSpriteObject then
                  begin
                    S := AnsiString( List.Values[ 'Resource' ] );
                    if S = '' then
                    begin
                      ObjectRef.Free;
                      ObjectRef := nil;
                      FigureInstances.Delete( i );
                    end
                    else
                    begin
                      S1 := AnsiString( ChangeFileExt( S, '' ) );
                      j := Figures.IndexOf( S1 );
                      if j >= 0 then
                      begin
                        TSpriteObject( ObjectRef ).Resource := TResource( Figures.Objects[ j ] );
                        TResource( TSpriteObject( ObjectRef ).Resource ).Reload := True;
                      end
                      else
                      begin
                        TSpriteObject( ObjectRef ).Resource := LoadArtResource( S );
                        if TSpriteObject( ObjectRef ).Resource is TDoorResource then
                          TDoorResource( TSpriteObject( ObjectRef ).Resource ).Define( GameMap, 0, 0 );

                        if Assigned( TSpriteObject( ObjectRef ).Resource ) then
                        begin
                          j := Figures.Add( S1 );
                          Figures.Objects[ j ] := TSpriteObject( ObjectRef ).Resource;
                        end
                        else
                        begin
                          ObjectRef.Free;
                          ObjectRef := nil;
                          FigureInstances.Delete( i );
                        end;
                      end;
                    end;
                  end;
                  LoadingFromSaveFile := False;
                end;
                if Assigned( ObjectRef ) then
                begin
                  if ObjectRef is TContainer then
                  begin
                    TContainer( ObjectRef ).NoItemPlacement := True;
                    ObjectRef.LoadProperties( List );
                    TContainer( ObjectRef ).NoItemPlacement := False;
                    LastContainer := TContainer( ObjectRef );
                    LastContainerInventoryCount := 0;
                  end
                  else if ObjectRef is TCharacter then
                  begin
                    TCharacter( ObjectRef ).UseDefaultEquipment := False;
                    TCharacter( ObjectRef ).ClearEquipment;
                    TCharacter( ObjectRef ).LoadProperties( List );
                    TCharacter( ObjectRef ).Enabled := TCharacter( ObjectRef ).Enabled and ( not TCharacter( ObjectRef ).PartyMember );
                  end
                  else if ObjectRef is TDoor then
                  begin
                    ObjectRef.LoadProperties( List );
                    ObjectRef.Init;
                  end
                  else
                  begin
                    ObjectRef.LoadProperties( List );
                  end;
                end;
                LoadingFromSaveFile := True;
              end;
            end;
          end;
        end;
        Stream.Seek( P + L, soFromBeginning );
        Stream.Read( BB, SizeOf( BB ) );
        if BB <> EOB then
        begin
          Log.Log( '*** Error:  EOB not found' );
          Exit;
        end;
      end;
    finally
      LoadingFromSaveFile := False;
      List.Free;
    end;
  end;

var
  FileName : string;
  SavFile : TSavFile;
const
  FailName : string = 'Main.LoadGame';
begin
  Result := False;
  Log.DebugLog(FailName);
  try

    EOB := EOBMarker;

    FileName := GamesPath + GameName + '.sav';
    ForceNotReadOnly( FileName );
    ForceNotReadOnly( ChangeFileExt( FileName, '.idx' ) );
    ForceNotReadOnly( ChangeFileExt( FileName, '.map' ) );

    SavFile := TSavFile.Create;
    try
      if FullLoad then
      begin
        SavFile.TravelList := TravelList;
        SavFile.JournalList := AdventureLog1.LogFileList;
        SavFile.QuestList := Quests;
        SavFile.AdventureList := Adventures;
      end;
      SavFile.Open( FileName );
      SavFile.CurrentMap := ChangeFileExt( ExtractFileName( LVLFile ), '' );
      SavFile.CurrentScene := CurrentScene;
      if FullLoad then
      begin
        DlgJournal.StartLogIndex := SavFile.JournalIndex;
        DlgQuestLog.PageNumber := SavFile.QuestIndex;
        DlgAdvLog.PageNumber := SavFile.AdventureIndex;
        DeathScreen := SavFile.DeathScreen;
        MaxPartyMembers := SavFile.MaxPartyMembers;
      end;
      LoadPlayerData( SavFile.PartyMembers );
      LoadMapKnown( SavFile.MapKnown );
      LoadProperties( SavFile.Properties, CurrentScene );
    finally
      SavFile.Free;
    end;

    if NPCList.Count > 0 then
      Player := NPCList.Items[ 0 ]
    else
      Player := nil;
    Result := True;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.LoadNewMap( const NewFile, SceneName,
  StartingPoint, Transition : string );
const
  FailName : string = 'Main.LoadNewMap';
var
  S : string;
begin
  Log.DebugLog(FailName);
  try
    Active := False;
    MouseCursor.Enabled := False;

    S := FindMap( NewFile );
    if not ( TFile.Exists( S ) ) then
    begin
      Log.Log( '*** Error: Map not found: ' + NewFile );
      Exit;
    end;

    TransitionScreen := Transition;
    NewLVLFile := S;

    NewScene := SceneName;
    NewStartingPoint := StartingPoint;
    LoadNewLevel := 1;
    DisableConsole := True;
    Timer2.Enabled := True;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.Timer2Timer( Sender : TObject );
const
  FailName : string = 'Main.Timer2Timer';
var
  pr: TRect;
begin
  Log.DebugLog(FailName);
  try

    if LoadNewLevel > 0 then
    begin
      pr := Rect( 0, 0, ResWidth, ResHeight );
      lpDDSBack.BltFast( 0, 0, lpDDSFront, @pr, DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
      FillRectSub( lpDDSBack, Rect( 0, 0, 703, 511 ), $202020 );
      if FSpellBarActive then
      begin
        pr := Rect( 0, 0, ScreenMetrics.ScreenWidth, 114 );
        lpDDSBack.BltFast( 0, ScreenMetrics.SpellBarY, SpellBar, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        pr := Rect( 0, 0, 195, 59 );
        lpDDSBack.BltFast( 250, ScreenMetrics.HelpBoxY, HelpBox, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end
      else
      begin
        pr := Rect( 0, 0, ScreenMetrics.ScreenWidth, 114 );
        lpDDSBack.BltFast( 0, ScreenMetrics.SpellBarY, OverlayB, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end;
      pr := Rect( 0, 0, 117, ScreenMetrics.SpellBarY );
      lpDDSBack.BltFast( ScreenMetrics.SpellBarX, 0, OverlayR, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      DrawHealthBars;
      lpDDSFront.Flip( nil, DDFLIP_WAIT );
      MouseCursor.PlotDirty := False;
      Dec( LoadNewLevel );
      if LoadNewLevel = 0 then
      begin
        Timer2.Enabled := False;
        LoadNewMapFile;
        if Application.Terminated then
          Exit;
        LoadNewLevel := -1;
        Timer2.Enabled := True;
      end;
    end
    else if LoadNewLevel < 0 then
    begin
      Inc( LoadNewLevel );
      if LoadNewLevel = 0 then
      begin
        Timer2.Enabled := False;
        DisableConsole := False;
        Log.Log( 'Start level' );
        ShowQuickMessage( '', 0 );
        Game.ForceRefresh := True;
        Active := True;
        Paused := False;
        MouseCursor.SetFrame( 37 );
        PrevTriggerID := -1;
        MouseCursor.Enabled := True;
      end;
    end
    else
      Timer2.Enabled := False;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.PlaceNPCList;
var
  List : TStringList;
  i, j : Integer;
const
  FailName : string = 'Main.PlaceNPCList';
begin
  Log.DebugLog(FailName);
  try

    if CurrentStartingPoint = '' then
    begin
      Log.Log( '*** Error: No starting point specified' );
    end
    else
    begin
      List := GetGroup( nil, CurrentStartingPoint );
      if Assigned( List ) then
      begin
        try
          j := 0;
          for i := 0 to List.Count - 1 do
          begin
            if j >= NPCList.Count then
              Break;
            if List.Objects[ i ] is TPathCorner then
            begin
              TCharacter( NPCList.Items[ j ] ).SetPos( TPathCorner( List.Objects[ i ] ).X, TPathCorner( List.Objects[ i ] ).Y, TPathCorner( List.Objects[ i ] ).Z );
              Inc( j )
            end;
          end;
        finally
          List.Free;
        end;
      end
      else
      begin
        Log.Log( '*** Error: Starting point not found: ' + CurrentStartingPoint );
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.DrawCurrentSpell;
var
  Point : TPoint;
  pr : TRect;
const
  FailName : string = 'Main.DrawCurrentSpell';
begin
  Log.DebugLog(FailName);
  try
    pr := Rect( 0, 0, 32, 32 );
    OverlayB.BltFast( ScreenMetrics.SpellX, 64, NoSpellIcon, @pr, DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );

    if Assigned( Current ) and Assigned( Current.CurrentSpell ) then
    begin
      Point := Current.CurrentSpell.GetIconXY( Current );
      DrawAlpha( OverlayB, Rect( ScreenMetrics.SpellX, 64, ScreenMetrics.SpellX + 32, 64 + 32 ), Rect( Point.X, Point.Y, Point.X + 32, Point.Y + 32 ), SpellGlyphs, False, 200 );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.DrawSpellGlyphs;
var
  i, h : Integer;
  SpellList : TStringList;
  Point : TPoint;
  DC : HDC;
  X, Y : Integer;
const
  FailName : string = 'Main.DrawSpellGlyphs';
  FailName2 : string = 'Main.DrawHelpbox';
begin
  Log.DebugLog(FailName);
  try

    SpellBar.GetDC( DC );
    try
      BitBlt( DC, 0, 0, imgSpellBar.width, imgSpellBar.Height, imgSpellBar.Canvas.Handle, 0, 0, SRCCOPY );
    finally
      SpellBar.ReleaseDC( DC );
    end;

    for i := 0 to 35 do
      Spells[ i ] := nil;

    if Assigned( Current ) then
    begin
      SpellList := Current.SpellList;
      try
        for i := 0 to SpellList.Count - 1 do
        begin
          if i < 36 then
          begin
            Spells[ i ] := TSpell( SpellList.Objects[ i ] );
            Point := TSpell( SpellList.Objects[ i ] ).GetIconXY( Current );
            X := 9 + ( i mod 18 ) * 37;
            Y := 31 + ( i div 18 ) * 36;  //HD is 33 not 31??
            if SpellList.Objects[ i ] = Current.CurrentSpell then
            begin
              FillRectAlpha( SpellBar, Rect( X - 2, Y - 2, X + 34, Y + 34 ), clMaroon, 200 );
              //SpellBar.bltfast(X, Y,Spellglyphs, Rect ( Point.X, Point.Y, Point.X + 32, Point.Y + 32 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
              DrawAlpha( SpellBar, Rect( X, Y, X + 32, Y + 32 ), Rect( Point.X, Point.Y, Point.X + 32, Point.Y + 32 ), SpellGlyphs, False, 160 );
            end
            else
              //SpellBar.bltfast(X, Y,Spellglyphs, Rect ( Point.X, Point.Y, Point.X + 32, Point.Y + 32 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
              DrawAlpha( SpellBar, Rect( X, Y, X + 32, Y + 32 ), Rect( Point.X, Point.Y, Point.X + 32, Point.Y + 32 ), SpellGlyphs, False, 200 );
            // Draw hotkey.
            for h := 0 to 9 do
              if Current.HotKey[h+1]=Spells[i] then
              begin
                DlgText.PlotF13Block( SpellBar, h.ToString, X + 28, X + 38, Y-10, 240 );
                break;
              end;
          end;
        end;
      finally
        SpellList.Free;
      end;
    end;

    //Also clear spell help
    HelpBox.GetDC( DC );
    try
      BitBlt( DC, 0, 0, imgHelp.width, imgHelp.Height, imgHelp.Canvas.Handle, 0, 0, SRCCOPY );
    finally
      HelpBox.ReleaseDC( DC );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.CueTune( const FileList : string; Instant : Boolean );
var
  SongParser : TStringList;
  i : Integer;
  S : string;
const
  FailName : string = 'Main.CueTune';
begin
  Log.DebugLog(FailName);
  try

    SongDuration := 36000;

    if Assigned( MusicLib ) then
    begin
      NoFadeIn := Instant;
      if NextSong <> '' then
      begin
        if FadeIn > 0 then
        begin
          if FadeIn >= MasterMusicVolume then
            FadeOut := MasterMusicVolume
          else
            FadeOut := MasterMusicVolume - FadeIn;
          FadeIn := 0;
        end
        else
          FadeOut := MasterMusicVolume;
      end;
      if FileList = '' then
      begin
        NextSong := '';
        FadeIn := 0;
      end
      else
      begin
        SongCounter := 0;
        SongParser := TStringList.Create;
        try
          SongParser.CommaText := FileList;
          i := random( SongParser.Count );
          NextSong := SongParser.Strings[ i ];

          S := SoundPath + NextSong + '.mp3';
          if TFile.Exists( S ) then
          begin
            SongDuration := 2400;
          end;

          {//Prevent play of MP3
          S:=SoundPath+NextSong+'.mp3';
          if TFile.Exists(S) then begin
            NextSong:='';
            i:=0;
            repeat
              NextSong:=SongParser.strings[i];
              S:=SoundPath+NextSong+'.mp3';
              inc(i);
            until (i>=SongParser.Count) or not TFile.Exists(S);
          end; }

          if NextSong <> '' then
          begin
            if FadeOut = 0 then
            begin
              MusicLib.OpenThisSong( AnsiString ( SoundPath + NextSong + '.mp3' ) );
              MusicLib.SetSongVolume( 0 );
              FadeIn := MasterMusicVolume + 5;
            end;
          end;
        finally
          SongParser.Free;
        end;
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.CloseIntroDialog( Sender : TObject );
const
  FailName : string = 'Main.CloseIntroDialog';
begin
  Log.DebugLog(FailName);
  try

    TDisplay( Sender ).Release;
    Game.OnMouseDown := AniView1MouseDown;
    Game.OnMouseMove := nil;
    Game.OnMouseUp := nil;
    OnKeyDown := TKeyEvent.FormKeyDown;
    OnMouseDown := FormMouseDown;
    OnMouseMove := FormMouseMove;

    with Sender as TIntro do
    begin
      if MenuChoice = 1 then
      begin
        PostMessage( Handle, WM_StartNew, 0, 0 );
      end
      else if MenuChoice = 2 then
      begin
        PostMessage( Handle, WM_StartLoad, 0, 0 );
      end
      else if MenuChoice = 3 then
      begin
        PostMessage( Handle, WM_StartSave, 0, 0 );
      end
      else if MenuChoice = 4 then
      begin
        PostMessage( Handle, WM_StartOptions, 0, 0 );
      end
      else if MenuChoice = 5 then
      begin
        ShowHistroy;
        // ExitCode:=65;
        // PostMessage(handle,WM_Done,0,0);
      end
      else if MenuChoice = 6 then
      begin
        PostMessage( Handle, WM_StartCredits, 0, 0 );
      end
      else if MenuChoice = 7 then
      begin
        ExitCode := 0;
        PostMessage( Handle, WM_Done, 0, 0 );
      end
      else if MenuChoice = 8 then
      begin
        Active := True;
        DisableConsole := False;
        Paused := False;
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.WMStartmainMenu( var Message : TWMNoParams );
const
  FailName : string = 'Main.WMStartmainMenu';
begin
  Log.DebugLog(FailName);
  try

    DlgIntro.pText := DlgText;
    DlgIntro.OnClose := CloseIntroDialog;
    Game.OnMouseDown := nil;
    Game.OnMouseMove := nil;
    Game.OnMouseUp := nil;
    OnKeyDown := nil;
    DlgIntro.Captions[ 1 ].Enabled := True;
    DlgIntro.Captions[ 2 ].Enabled := True;
    DlgIntro.Captions[ 3 ].Enabled := not NewGame;
    DlgIntro.Captions[ 4 ].Enabled := True;  // not NewGame;
    DlgIntro.Captions[ 5 ].Enabled := True;
    DlgIntro.Captions[ 6 ].Enabled := True;
    DlgIntro.Captions[ 7 ].Enabled := True;
    DlgIntro.Captions[ 8 ].Enabled := not NewGame;
    MouseCursor.SetFrame( 37 );
    PrevTriggerID := -1;
    DisableConsole := True;
    DlgIntro.Init;
    MouseCursor.Enabled := True;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.SetCurrentTheme( const Value : string );
var
  S : string;
const
  FailName : string = 'Main.SetCurrentTheme';
begin
  Log.DebugLog(FailName);
  try

    FCurrentTheme := Value;
    S := Themes.Values[ FCurrentTheme ];
    CueTune( S, True );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.WMStartNew( var Message : TWMNoParams );
var
  S : string;
  i : Integer;
  INI : TIniFile;
  INILanguage : TIniFile;
  List : TArray<string>; //TStringList;
  PlayerResource : string;
const
  FailName : string = 'Main.WMStartNew';

  procedure GetCommandLineLVLFile; // *** jrs CommandLine override of LVLFile
  var
    i : Integer;
    sTemp : string;
  begin
    for i := 1 to ParamCount do
    begin
      sTemp := Trim( UpperCase( ParamStr( i ) ) );
      if ( Pos( 'STARTFILE=', sTemp ) > 0 ) then
      begin
        sTemp := sTemp.Split(['='])[1].Trim;
        if Length( sTemp ) < 1 then
          Break;
        LVLFile := sTemp;
      end;
    end; {for i := 1}
  end;

begin
  Log.DebugLog(FailName);
  try

    INI := TIniFile.Create( SiegeINIFile );
    try
      PlayerResource := 'players\' + INI.ReadString( 'Character', 'Resource', 'player' ) + '.pox';
      if not TFile.Exists( ResourcePath + PlayerResource ) then
      begin
        //Message could not create character
        PostMessage( Handle, WM_StartMainMenu, 0, 0 ); //Restart the intro
        Exit;
      end;
    finally
      INI.free;
    end;

    if not NewGame then
    begin
      ClearOnDemandResources;
      ClearResources( True );
      Sprites.Realloc;
    end;

    NewGame := True;

    Player := TCharacter.Create( 400, 400, 0, 1, True );

    if AdjustedPartyHitPoints then
      Player.TrainingPoints := 10
    else
      Player.TrainingPoints := 20; //Original

    AdventureLog1.Clear;
    DlgJournal.StartLogIndex := -1;
    Quests.Clear;
    Adventures.Clear;
    INI := TIniFile.Create( SiegeINIFile );
    try
      LVLFile := INI.ReadString( 'Settings', 'StartFile', '' );
      if LVLFile = '' then
        LVLFile := INI.ReadString( 'Settings', 'TestFile', '' );
      // *** jrs Add commandline override for StartFile setting. Mostly for testing, but might have other uses
//Use      FindCmdLineSwitch('startFile', LVLFile);
//      GetCommandLineLVLFile;

      if not System.IOUtils.TPath.IsPathRooted( LVLFile ) then
        LVLFile := MapPath + System.IOUtils.TPath.GetFileName( LVLFile );
      if not TFile.Exists( LVLFile ) then
      begin // jrs
        Log.log( FailName, 'Unable to open game map file %s', [ LVLFile ] );
        Log.Flush;
        ExitCode := -2;
        PostMessage( Handle, WM_Done, 0, 0 );
      end;

      CurrentScene := '';
      //Add first log entry
    finally
      INI.Free;
    end;

    INILanguage := TIniFile.Create(SiegeINILanguageFile);
    try
      S := INILanguage.ReadString( 'Settings', 'JournalIntro', '' );
      if S <> '' then
      begin
        List := S.Split([',']);
        for i := 0 to Length(List) - 1 do
          AdventureLog1.AddLogEntry( List[ i ] + '.jrn' );
      end;
    finally
      INILanguage.Free;
    end;

    DlgCreation.Character := Player;
    DlgCreation.frmMain := Self;

    Game.OnMouseDown := nil;
    Game.OnMouseMove := nil;
    Game.OnMouseUp := nil;
    OnKeyDown := nil;
    DlgCreation.pText := DlgText;
    DlgCreation.OnClose := CloseCreateDialog;

    Player.Resource := LoadArtResource( ChangeFileExt( PlayerResource, '.gif' ) );
    Player.LoadEquipment( True );

    DlgCreation.Init;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.WMStartOptions( var Message : TWMNoParams );
const
  FailName : string = 'Main.WMStartOptions';
begin
  Log.DebugLog(FailName);
  try

    DlgOptions.SoundVolume := MasterSoundVolume;
    DlgOptions.MusicVolume := MasterMusicVolume;
    DlgOptions.PlotShadows := PlotShadows;
    if NewGame then
    begin
      DlgOptions.Character := nil;
      DlgOptions.IconDX := nil;
    end
    else
    begin
      DlgOptions.Character := Current;
      DlgOptions.IconDX := SpellGlyphs;
    end;
    Game.OnMouseDown := nil;
    Game.OnMouseMove := nil;
    Game.OnMouseUp := nil;
    OnKeyDown := nil;
    DlgOptions.OldKeyDown := TKeyEvent.FormKeyDown;
    DlgOptions.pText := DlgText;
    DlgOptions.OnClose := CloseOptions;
    MouseCursor.SetFrame( 37 );
    PrevTriggerID := -1;
    DlgOptions.Init;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.CloseLoad( Sender : TObject );
var
  S1, S2 : string;
const
  FailName : string = 'Main.CloseLoad';
begin
  Log.DebugLog(FailName);
  try

    with Sender as TLoadGame do
    begin
      Release;
      if LoadThisFile = '' then
      begin
        Game.OnMouseDown := AniView1MouseDown;
        Game.OnMouseMove := nil;
        Game.OnMouseUp := nil;
        OnKeyDown := TKeyEvent.FormKeyDown;
        OnMouseDown := FormMouseDown;
        OnMouseMove := FormMouseMove;
        PostMessage( Handle, WM_StartMainMenu, 0, 0 ); //Restart the intro
      end
      else
      begin
        Log.Log( 'Loading saved game: ' + DlgLoad.LoadThisFile );

        S1 := GamesPath + TempGame + '.sav';
        S2 := GamesPath + DlgLoad.LoadThisFile + '.sav';
        ForceNotReadOnly( S1 );
        ForceNotReadOnly( ChangeFileExt( S1, '.idx' ) );
        ForceNotReadOnly( ChangeFileExt( S1, '.map' ) );
        ForceNotReadOnly( S2 );
        ForceNotReadOnly( ChangeFileExt( S2, '.idx' ) );
        ForceNotReadOnly( ChangeFileExt( S2, '.map' ) );
        DeleteFile( S1 );
        DeleteFile( ChangeFileExt( S1, '.idx' ) );
        DeleteFile( ChangeFileExt( S1, '.map' ) );
//        try
//          TFile.Delete( S1 );
//        except
//        end;
//        try
//          TFile.Delete( ChangeFileExt( S1, '.idx' ) );
//        except
//        end;
//        try
//          TFile.Delete( ChangeFileExt( S1, '.map' ) );
//        except
//        end;

        try
          TFile.Copy( S2, S1, True );
        except
          Log.Log( 'Error: *** Could not copy ' + S2 + ' to ' + S1 );
        end;
        try
          TFile.Copy( ChangeFileExt( S2, '.idx' ), ChangeFileExt( S1, '.idx' ), True );
        except
        end;
        try
          TFile.Copy( ChangeFileExt( S2, '.map' ), ChangeFileExt( S1, '.map' ), True );
        except
        end;

        GameName := LoadThisFile;
        LVLFile := FindMap( MapName );
        CurrentScene := SceneName;
        TravelList.Clear;
        TravelList.Text := TravelBlock;

        ClearOnDemandResources;
        ClearResources( True );
        Sprites.Realloc;
        FSpellBarActive := False;
        ClearOverlayB;

        TransitionScreen := '';
        DeathScreen := '';
        MaxPartyMembers := 4;  // Was 2
        if not LoadMapFile( False, True ) then
          Exit;
        LastFileSaved := GameName;
        NewGame := False;
        Game.ForceRefresh := True;
        CloseAllDialogs( Sender );
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.CloseSave( Sender : TObject );
{var
  S1,S2: string; }
const
  FailName : string = 'Main.CloseSave';
begin
  Log.DebugLog(FailName);
  try

    with Sender as TLoadGame do
    begin
      Release;
      if LoadThisFile = '' then
      begin
        Game.OnMouseDown := AniView1MouseDown;
        Game.OnMouseMove := nil;
        Game.OnMouseUp := nil;
        OnKeyDown := TKeyEvent.FormKeyDown;
        OnMouseDown := FormMouseDown;
        OnMouseMove := FormMouseMove;
        PostMessage( Handle, WM_StartMainMenu, 0, 0 ); //Restart the intro
      end
      else
      begin
        Log.Log( 'Saving game: ' + DlgLoad.LoadThisFile );
        GameName := LoadThisFile;
        try
          {        S1:=DefaultPath+'games\'+TempGame+'.sav';
                  S2:=DefaultPath+'games\'+DlgLoad.LoadThisFile+'.sav';
                  if TFile.Exists(S1) then begin
                    try
                      if TFile.Exists(S2) then TFile.Delete(S2);
                      TFile.Copy( S1, S2, True);
                    except
                      Log.Log('Error: *** Could not copy '+S1+' to ' + S2);
                    end;
                  end;   }
          SaveGame;
          LastFileSaved := GameName;
          if Assigned( ScreenShot ) then
          begin
            Log.Log( 'Saving screenshot: ' + GamesPath + DlgLoad.LoadThisFile + '.bmp' );
            ForceNotReadOnly( GamesPath + DlgLoad.LoadThisFile + '.bmp' );
            ScreenShot.SaveToFile( GamesPath + DlgLoad.LoadThisFile + '.bmp' );
          end;
        except
          //Could not save file
          Log.Log( '*** Error: Could not save file' );
        end;
        CloseAllDialogs( Sender );
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.WMStartLoad( var Message : TWMNoParams );
const
  FailName : string = 'Main.WMStartLoad';
begin
  Log.DebugLog(FailName);
  try

    Game.OnMouseDown := nil;
    Game.OnMouseMove := nil;
    Game.OnMouseUp := nil;
    OnKeyDown := nil;
    DlgLoad.OldKeyDown := TKeyEvent.FormKeyDown;
    DlgLoad.pText := DlgText;
    DlgLoad.OnClose := CloseLoad;
    DlgLoad.frmMain := Self;
    DlgLoad.LoadFile := True;
    DlgLoad.LastSavedFile := LastFileSaved;
    MouseCursor.SetFrame( 37 );
    PrevTriggerID := -1;
    DlgLoad.Init;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.WMStartSave( var Message : TWMNoParams );
const
  FailName : string = 'Main.WMStartSave';
begin
  Log.DebugLog(FailName);
  try

    Game.OnMouseDown := nil;
    Game.OnMouseMove := nil;
    Game.OnMouseUp := nil;
    OnKeyDown := nil;
    DlgLoad.OldKeyDown := TKeyEvent.FormKeyDown;
    DlgLoad.pText := DlgText;
    DlgLoad.OnClose := CloseSave;
    DlgLoad.frmMain := Self;
    DlgLoad.LoadFile := False;
    DlgLoad.LastSavedFile := LastFileSaved;
    MouseCursor.SetFrame( 37 );
    PrevTriggerID := -1;
    DlgLoad.Init;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.WMStartIntro( var Message : TWMNoParams );
var
  DlgOpenAnim : TOpenAnim;
  INI : TIniFile;
  INILanguage : TIniFile;
  S : string;
  List : TArray<string>;
  i : Integer;
  LocalShowHistory : Boolean;
const
  FailName : string = 'Main.WMStartIntro';
begin
  Log.DebugLog(FailName);
  try
    LocalShowHistory := True;
    if not ( TFile.Exists( OpeningMovie ) ) then
    begin
      DlgOpenAnim := TOpenAnim.Create;
      with DlgOpenAnim do
      begin
        try
          pMusic := MusicLib;
          Init;
          LocalShowHistory := not Cancel;
          Release;
        finally
          Free;
        end;
      end;
    end;

    if LocalShowHistory then
    begin
      INI := TIniFile.Create( SiegeINIFile );
      try
        INILanguage := TIniFile.Create( SiegeINILanguageFile );
        try
          S := INI.ReadString( 'Settings', 'History', '' );
        finally
          INILanguage.Free;
        end;
        if LowerCase( Ini.ReadString( 'Settings', 'ShowHistory', 'false' ) ) = 'false' then
        begin
          PostMessage( Handle, WM_StartMainMenu, 0, 0 );
          Exit;
        end;
        Ini.WriteString( 'Settings', 'ShowHistory', 'false' );

        if S = '' then
        begin
          PostMessage( Handle, WM_StartMainMenu, 0, 0 );
        end
        else
        begin
          List := S.Split([',']);
          for i := 0 to Length(List) - 1 do
            AdventureLog1.AddLogEntry( List[ i ] + '.jrn' );
          DlgJournal.StartLogIndex := -1;
          DlgJournal.pText := DlgText;
          DlgJournal.OnClose := CloseIntroJournal;
          DlgJournal.frmMain := Self;
          DlgJournal.JournalLog := AdventureLog1;
          Game.OnMouseDown := nil;
          Game.OnMouseMove := nil;
          Game.OnMouseUp := nil;
          OnKeyDown := nil;
          MouseCursor.SetFrame( 37 );
          PrevTriggerID := -1;
          DlgJournal.Init;
          MouseCursor.Enabled := True;
        end;
      finally
        INI.Free;
      end;
    end
    else
    begin
      PostMessage( Handle, WM_StartMainMenu, 0, 0 );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.ShowMouseMessage( const Msg : string );
const
  tY1 = 33;
  tY2 = 55;
var
  NewMessage : Boolean;
  DC : HDC;
const
  FailName : string = 'Main.ShowMouseMessage';
begin
  try

    if QuickMessageCount > 0 then
    begin
      Dec( QuickMessageCount );
      if QuickMessageCount = 0 then
      begin
        NewMessage := True;
        QuickMessage := '';
        PrevQuickMessage := '';
      end
      else
      begin
        NewMessage := False;
      end;
    end
    else
    begin
      NewMessage := False;
    end;
    if not NewMessage then
    begin
      if QuickMessage <> PrevQuickMessage then
      begin
        NewMessage := True;
        PrevQuickMessage := QuickMessage;
      end;
    end;
    if not NewMessage then
    begin
      if Msg <> MouseMessage then
      begin
        NewMessage := True;
        MouseMessage := Msg;
      end;
    end;
    if NewMessage then
    begin
      OverlayB.GetDC( DC );
      try
        BitBlt( DC, ScreenMetrics.BottomBarX, 30, 202, 68, imgBottomBar.Canvas.Handle, ScreenMetrics.BottomBarX, 30, SRCCOPY );
      finally
        OverlayB.ReleaseDC( DC );
      end;
      DlgText.PlotF13Text( OverlayB, MouseMessage, ScreenMetrics.MouseMsgX, tY1, 170 );
      DlgText.PlotF13Block( OverlayB, QuickMessage, ScreenMetrics.MouseMsgX, ScreenMetrics.MouseMsgX + 196, tY2, 170 );
      //    DlgText.PlotText2(OverlayB,QuickMessage,tx2,ty2,170);
      //    DlgText.LoadTinyFontGraphic;
      //    DlgText.PlotTinyText2(OverlayB,QuickMessage,tx2,ty2,170);
      //    DlgText.LoadMegaTinyFontGraphic;
      //    DlgText.PlotMegaTinyText(OverlayB,QuickMessage,tx2,ty2,170);
      //    DlgText.PlotDarkText2(OverlayB,QuickMessage,tx2,ty2,170);
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.ShowQuickMessage( const Msg : string; Time : Integer );
const
  FailName : string = 'Main.ShowQuickMessage';
begin
  Log.DebugLog(FailName);
  try

    QuickMessage := Msg;
    QuickMessageCount := Time;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.InventoryDraw( Sender : TObject );
var
  i : Integer;
  HpDistance, ManaDistance : Double;
  pr, pr0 : TRect;
const
  FailName : string = 'Main.InventoryDraw';
begin
  Log.DebugLog(FailName);
  try

    if not ( Sender is TCharacter ) then
      Exit;
    i := NPCList.IndexOf( Sender );
    if i = 0 then
    begin
      PaintCharacterOnBorder( TSpriteObject( Sender ), i );
      pr := Rect( 0, 0, 117, 133 );
      lpDDSBack.BltFast( ScreenMetrics.SpellBarX, 0, OverlayR, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    end
    else if i >= 1 then
    begin
      if not FSpellBarActive then
      begin
        PaintCharacterOnBorder( Current, i );
        pr := Rect( 0, 0, ScreenMetrics.ScreenWidth, 114 );
        lpDDSBack.BltFast( 0, ScreenMetrics.SpellBarY, OverlayB, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end;

      for i := 1 to NPCList.Count - 1 do
      begin
        HpDistance := TCharacter( NPCList[ i ] ).HitPoints - TCharacter( NPCList[ i ] ).Wounds;
        if HPDistance > TCharacter( NPCList[ i ] ).HitPoints then
          HPDistance := TCharacter( NPCList[ i ] ).HitPoints
        else if HPDistance < 0 then
          HPDistance := 0;

        ManaDistance := TCharacter( NPCList[ i ] ).Mana - TCharacter( NPCList[ i ] ).Drain;
        if ManaDistance > TCharacter( NPCList[ i ] ).Mana then
          ManaDistance := TCharacter( NPCList[ i ] ).Mana
        else if ManaDistance < 0 then
          ManaDistance := 0;

        HPDistance := HPDistance * ( 66 / TCharacter( NPCList[ i ] ).HitPoints );
        ManaDistance := ManaDistance * ( 66 / TCharacter( NPCList[ i ] ).Mana );

        pr := Rect( NPCBarXCoord[ i ], ScreenMetrics.NPCBarY - Round( HPDistance ), NPCBarXCoord[ i ] + 5, ScreenMetrics.NPCBarY );
        pr0 := Rect( 0, 0, 0, 0 );
        lpDDSBack.Blt( @pr, nil, @pr0, DDBLT_COLORFILL + DDBLT_WAIT, @NPCHealthBltFx );

        pr := Rect( NPCBarXCoord[ i ] + 7, ScreenMetrics.NPCBarY - Round( ManaDistance ), NPCBarXCoord[ i ] + 7 + 5, ScreenMetrics.NPCBarY );
        pr0 := Rect( 0, 0, 0, 0 );
        lpDDSBack.Blt( @pr, nil, @pr0, DDBLT_COLORFILL + DDBLT_WAIT, @NPCManaBltFx );
      end;

    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.WMStartCredits( var Message : TWMNoParams );
const
  FailName : string = 'Main.WMStartCredits';
begin
  Log.DebugLog(FailName);
  try

    Game.OnMouseDown := nil;
    Game.OnMouseMove := nil;
    Game.OnMouseUp := nil;
    OnKeyDown := nil;
    DlgShow.OldKeyDown := TKeyEvent.FormKeyDown;
    DlgShow.pText := DlgText;
    DlgShow.OnClose := CloseShow;
    DlgShow.frmMain := Self;
    DlgShow.BMPFileName := 'CreditsScreen.bmp';
    DlgShow.MusicFileName := 'exCarlibur.mp3';
    MouseCursor.SetFrame( 37 );
    PrevTriggerID := -1;
    DlgShow.Init;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.WMKillFocus( var Message : TMessage );
begin
  //  Log.Log('Kill Focus');
  {  Game.Active:=false;
    SaveSurfaces; }
  inherited;
end;

procedure TfrmMain.WMSetFocus( var Message : TMessage );
begin
  inherited;
  //  Log.Log('Set Focus');
  {  if SurfacesAreSaved then begin
      Log.Log('Restoring');
      RestoreSurfaces;
  //    Game.Active:=true;
  //    lpDDSFront.Flip(nil, DDFLIP_WAIT);
    end;  }
end;

procedure TfrmMain.AppActivate( Sender : TObject );
const
  FailName : string = 'Main.AppActivate';
begin
  Log.DebugLog(FailName);
  try

    Log.Log( 'App Activate' );
    if Initialized then
    begin
      WindowState := wsNormal;
      SetBounds( 0, 0, ScreenMetrics.ScreenWidth, ScreenMetrics.ScreenHeight );
      Game.Enabled := True;
      FormShow( Self );
    end
    else
    begin
      Initialized := True;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.AppDeactivate( Sender : TObject );
const
  FailName : string = 'Main.AppDeactivate';
begin
  Log.DebugLog(FailName);
  try

    Log.Log( 'App Deactivate' );
    if Active then
    begin
      Active := False;
      GameName := '~AutoSave';
      SaveGame;
      NeedToReload := True;
      KeepTravelList := TravelList.Text;
    end
    else
    begin
      NeedToReload := False;
    end;

    Game.OnMouseDown := AniView1MouseDown;
    Game.OnMouseMove := nil;
    Game.OnMouseUp := nil;
    OnKeyDown := TKeyEvent.FormKeyDown;
    OnMouseDown := FormMouseDown;
    OnMouseMove := FormMouseMove;
    FreeAll;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.BeginDeath;
const
  FailName : string = 'Main.BeginDeath';
begin
  Log.DebugLog(FailName);
  try

    Game.OnMouseDown := nil;
    Game.OnMouseMove := nil;
    Game.OnMouseUp := nil;
    OnKeyDown := nil;
    DlgShow.OldKeyDown := TKeyEvent.FormKeyDown;
    DlgShow.pText := DlgText;
    DlgShow.OnClose := CloseShow;
    DlgShow.frmMain := Self;
    if DeathScreen = '' then
      DlgShow.BMPFileName := 'Death.bmp'
    else
      DlgShow.BMPFileName := DeathScreen + '.bmp';
    DlgShow.MusicFileName := 'Level1Final.mp3';
    MouseCursor.SetFrame( 37 );
    PrevTriggerID := -1;
    DlgShow.Init;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.CloseIntroJournal( Sender : TObject );
const
  FailName : string = 'Main.CloseIntroJournal';
begin
  Log.DebugLog(FailName);
  try

    if DlgJournal.Loaded then
      DlgJournal.Release;
    AdventureLog1.Clear;
    DlgJournal.StartLogIndex := -1;
    PostMessage( Handle, WM_StartMainMenu, 0, 0 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.WMDone( var Message : TWMNoParams );
begin
  //     bPlayClosingMovie := true;
  MouseCursor.Enabled := False;
  FreeAll;
  Close;
end;

function TfrmMain.ShouldRun( X, Y : Longint ) : Boolean;
var
  D2 : Double;
begin
  if Assigned( Current ) then
  begin
    try
      D2 := sqr( Current.X - X ) + sqr( 2 * ( Current.Y - Y ) );
      Result := ( D2 > 30000 );
    except
      Result := False;
    end;
  end
  else
    Result := False;
end;

procedure TfrmMain.BeginAdvLog;
const
  FailName : string = 'Main.BeginAdvLog';
begin
  Log.DebugLog(FailName);
  try

    ClearAdventureGraphic;
    DlgAdvLog.LogInfo := Adventures;
    OpenDialog( DlgAdvLog, CloseAllDialogs );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.BeginQuestLog;
const
  FailName : string = 'Main.BeginQuestLog';
begin
  Log.DebugLog(FailName);
  try

    ClearQuestGraphic;
    DlgQuestLog.LogInfo := Quests;
    OpenDialog( DlgQuestLog, CloseAllDialogs );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.BeginRoster( Character : TCharacter );
const
  FailName : string = 'Main.BeginRoster';
begin
  Log.DebugLog(FailName);
  try

    DlgRoster.Character := Character;
    DlgRoster.OnDraw := DrawRosterGuy;
    OpenDialog( DlgRoster, CloseRosterDialog );
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.CloseRosterDialog( Sender : TObject );
var
  i : Integer;
  OldCount : Integer;
begin
  OldCount := NPCList.Count - 1;
  for i := OldCount downto 1 do
  begin
    if DlgRoster.CheckBox[ i ] then
    begin
      if NPCList.Items[ i ] = Current then
      begin
        Current.Highlightable := True;
        Current := Player;
        FSpellBarActive := False; //This prevents the player from choosing a spell for the wrong character
        DrawCurrentSpell;
        Game.KeyFigure := Current;
        Current.AutoTransparent := XRayOn;
        Current.Highlightable := False;
        Current.PrevAIMode := Current.AIMode;
        Current.AIMode := aiNone;
        Current.Visible := True;
      end;
      TCharacter( NPCList.Items[ i ] ).Alliance := Current.PrevAlliance;
      TCharacter( NPCList.Items[ i ] ).AIMode := TCharacter( NPCList.Items[ i ] ).PrevAIMode;
      TCharacter( NPCList.Items[ i ] ).UseAllegianceOf := nil;
      TCharacter( NPCList.Items[ i ] ).PartyMember := False;
      NPCList.Delete( i );
    end;
  end;
  if Assigned( DlgRoster.Character ) and DlgRoster.CheckBox[ 0 ] and ( NPCList.Count <= MaxPartyMembers ) then
  begin
    DlgRoster.Character.PrevAIMode := DlgRoster.Character.AIMode;
    DlgRoster.Character.AIMode := aiParty;
    DlgRoster.Character.PrevAlliance := DlgRoster.Character.Alliance;
    DlgRoster.Character.Alliance := Player.Alliance;
    DlgRoster.Character.UseAllegianceOf := Player;
    DlgRoster.Character.InitAI;
    NPCList.Add( DlgRoster.Character );
  end;
  AssignMarch;

  for i := 0 to NPCList.Count - 1 do
    PaintCharacterOnBorder( TSpriteObject( NPCList.Items[ i ] ), i );
  for i := NPCList.Count to OldCount do
    PaintCharacterOnBorder( nil, i );

  CloseAllDialogs( DlgRoster );
end;

procedure TfrmMain.DrawRosterGuy( Character : TCharacter; X, Y : Integer );
var
  OldUseLighting : Boolean;
  OldDrawShadow : Boolean;
  OldComplexShadow : Boolean;
  OldFrame : Integer;
  OldLightR, OldLightG, OldLightB : Integer;
  OldHighighted : Boolean;
  ddsd : TDDSurfaceDesc;
  Bits : BITPLANE;
begin
  OldUseLighting := Character.UseLighting;
  OldDrawShadow := TResource( Character.Resource ).DrawShadow;
  OldComplexShadow := TResource( Character.Resource ).ComplexShadow;
  OldHighighted := Character.Highlighted;
  Character.UseLighting := True;
  OldLightR := Character.LightR;
  OldLightG := Character.LightG;
  OldLightB := Character.LightB;
  Character.LightR := 255;
  Character.LightG := 255;
  Character.LightB := 255;
  TResource( Character.Resource ).DrawShadow := False;
  TResource( Character.Resource ).ComplexShadow := False;
  OldFrame := Character.Frame;
  Character.Highlighted := False;
  Character.ForceFrame( 21 );
  ddsd.dwSize := SizeOf( ddsd );
  if lpDDSBack.Lock( nil, ddsd, DDLOCK_WAIT, 0 ) = DD_OK then
  begin
    try
      Bits.bitsPtr := ddsd.lpSurface;
      Bits.bitsWdh := ResWidth;
      Bits.bitsHgh := ResHeight;
      Bits.bitsFmt := dfx_pixelformat;
      Bits.bitsPitch := ddsd.lPitch;
      Bits.BaseX := -X;
      Bits.BaseY := -Y;
      TResource( Character.Resource ).RenderLocked( Character, @Bits );
    finally
      lpDDSBack.Unlock( nil );
    end;
  end;
  Character.UseLighting := OldUseLighting;
  TResource( Character.Resource ).DrawShadow := OldDrawShadow;
  TResource( Character.Resource ).ComplexShadow := OldComplexShadow;
  Character.ForceFrame( OldFrame );
  Character.LightR := OldLightR;
  Character.LightG := OldLightG;
  Character.LightB := OldLightB;
  Character.Highlighted := OldHighighted;
end;

procedure TfrmMain.AssignMarch;
var
  i : Integer;
  Prev : TCharacter;
begin
  Prev := Player;
  Player.PartyMember := True;
  if Current = Player then
  begin
    for i := 1 to NPCList.Count - 1 do
    begin
      TCharacter( NPCList.Items[ i ] ).PartyMember := True;
      if TCharacter( NPCList.Items[ i ] ).AI is TPartyAI then
      begin
        TPartyAI( TCharacter( NPCList.Items[ i ] ).AI ).Leader := Prev;
        TPartyAI( TCharacter( NPCList.Items[ i ] ).AI ).Index := i;
        Prev := NPCList.Items[ i ];
      end;
    end;
  end
  else
  begin
    for i := 1 to NPCList.Count - 1 do
    begin
      TCharacter( NPCList.Items[ i ] ).PartyMember := True;
      if NPCList.Items[ i ] <> Current then
      begin
        if TCharacter( NPCList.Items[ i ] ).AI is TPartyAI then
        begin
          TPartyAI( TCharacter( NPCList.Items[ i ] ).AI ).Leader := Prev;
          TPartyAI( TCharacter( NPCList.Items[ i ] ).AI ).Index := i;
        end;
      end;
    end;
  end;
end;

function TfrmMain.IsOnZoneTile( Figure : TAniFigure ) : Boolean;
var
  i : Integer;
begin
  Result := False;
  if Assigned( Figure.Tile ) then
  begin
    if Figure.Tile.TriggerID > 0 then
    begin
      i := Figure.Tile.TriggerID - 1;
      if ( i < FigureInstances.Count ) and Assigned( FigureInstances.Objects[ i ] ) and ( FigureInstances.Objects[ i ] is TTrigger ) then
      begin
        if TTrigger( FigureInstances.Objects[ i ] ).OnTrigger.ToLower.Contains( 'loadmap(' ) then
          Result := True;
      end;
    end;
  end;
end;

procedure TfrmMain.AddLogEntry( const FileName : string );
var
  DC : HDC;
const
  FailName : string = 'Main.AddLogEntry';
begin
  Log.DebugLog(FailName);
  try
    if AdventureLog1.AddLogEntry( FileName + '.jrn' ) <> -1 then
    begin
      OverlayB.GetDC( DC );
      try
        BitBlt( DC, ScreenMetrics.LogX, 74, 68, 24, imgBottomBar.Canvas.Handle, ScreenMetrics.LogX, 74, SRCCOPY );
      finally
        OverlayB.ReleaseDC( DC );
      end;
      FillRectAlpha( OverlayB, Rect( ScreenMetrics.LogX+1, 75, ScreenMetrics.LogX + 67, 74 + 23 ), $80, 32 );
      FillRectAlpha( OverlayB, Rect( ScreenMetrics.LogX, 74, ScreenMetrics.LogX + 68, 74 + 24 ), $80, 96 );
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TfrmMain.AddAdventure( const Entry : string );
var
  DC : HDC;
  i : Integer;
begin
  i := Adventures.IndexOf( Entry );
  if i >= 0 then
    Exit;
  if Adventures.Add( Entry ) < 0 then
    Exit;

  OverlayB.GetDC( DC );
  try
    BitBlt( DC, ScreenMetrics.LogX, 50, 68, 24, imgBottomBar.Canvas.Handle, ScreenMetrics.LogX, 50, SRCCOPY );
  finally
    OverlayB.ReleaseDC( DC );
  end;
  FillRectAlpha( OverlayB, Rect( ScreenMetrics.LogX+1, 51, ScreenMetrics.LogX + 67, 50 + 23 ), $80, 32 );
  FillRectAlpha( OverlayB, Rect( ScreenMetrics.LogX, 50, ScreenMetrics.LogX + 68, 50 + 24 ), $80, 96 );
end;

procedure TfrmMain.AddQuest( const Entry : string );
var
  DC : HDC;
  i : Integer;
begin
  i := Quests.IndexOf( Entry );
  if i >= 0 then
    Exit;
  Quests.Add( Entry );

  OverlayB.GetDC( DC );
  try
    BitBlt( DC, ScreenMetrics.LogX, 26, 68, 24, imgBottomBar.Canvas.Handle, ScreenMetrics.LogX, 26, SRCCOPY );
  finally
    OverlayB.ReleaseDC( DC );
  end;
  FillRectAlpha( OverlayB, Rect( ScreenMetrics.LogX+1, 27, ScreenMetrics.LogX + 67, 26 + 23 ), $80, 32 );
  FillRectAlpha( OverlayB, Rect( ScreenMetrics.LogX, 26, ScreenMetrics.LogX + 68, 26 + 24 ), $80, 96 );
end;

procedure TfrmMain.ClearAdventureGraphic;
var
  DC : HDC;
  pr : TRect;
begin
  OverlayB.GetDC( DC );
  try
    BitBlt( DC, ScreenMetrics.LogX, 50, 68, 24, imgBottomBar.Canvas.Handle, ScreenMetrics.LogX, 50, SRCCOPY );
  finally
    OverlayB.ReleaseDC( DC );
  end;
  if not FSpellBarActive then
  begin
    MouseCursor.cleanup;
    pr := Rect( ScreenMetrics.LogX, 50, ScreenMetrics.LogX + 68, 50 + 24 );
    lpDDSFront.BltFast( ScreenMetrics.LogX, ScreenMetrics.SpellBarY + 50, OverlayB, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
  end;
end;

procedure TfrmMain.ClearLogGraphic;
var
  DC : HDC;
begin
  OverlayB.GetDC( DC );
  try
    BitBlt( DC, ScreenMetrics.LogX, 74, 68, 24, imgBottomBar.Canvas.Handle, ScreenMetrics.LogX, 74, SRCCOPY );
  finally
    OverlayB.ReleaseDC( DC );
  end;
end;

procedure TfrmMain.ClearQuestGraphic;
var
  DC : HDC;
  pr : TRect;
begin
  OverlayB.GetDC( DC );
  try
    BitBlt( DC, ScreenMetrics.LogX, 26, 68, 24, imgBottomBar.Canvas.Handle, ScreenMetrics.LogX, 26, SRCCOPY );
  finally
    OverlayB.ReleaseDC( DC );
  end;
  if not FSpellBarActive then
  begin
    MouseCursor.cleanup;
    pr := Rect( ScreenMetrics.LogX, 26, ScreenMetrics.LogX + 68, 26 + 24 );
    lpDDSFront.BltFast( ScreenMetrics.LogX, ScreenMetrics.SpellBarY + 26, OverlayB, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
  end;
end;

procedure TfrmMain.WMStartTimer( var Message : TWMNoParams );
begin
  if UseTimer then
  begin
    Game.Active := True;
    Exit;
  end;

  InTimerLoop := True;
  while FActive do
  begin
    Game.WaitForNextFrame;
    Application.ProcessMessages;
    if not FActive then
      Break;
    Game.DrawFrame;
    Application.ProcessMessages;
  end;
  InTimerLoop := False;
end;

procedure TfrmMain.SetActive( const Value : Boolean );
begin
  if Value = FActive then
    Exit;
  FActive := Value;
  if UseTimer then
  begin
    if FActive then
      PostMessage( Handle, WM_StartTimer, 0, 0 )
    else
      Game.Active := FActive;
  end
  else
  begin
    if FActive then
    begin
      if not InTimerLoop then
        PostMessage( Handle, WM_StartTimer, 0, 0 );
    end;
  end;
end;

procedure TfrmMain.ClearOverlayB;
var
  DC : HDC;
begin
  OverlayB.GetDC( DC );
  try
    BitBlt( DC, 0, 0, imgBottomBar.width, imgBottomBar.Height, imgBottomBar.Canvas.Handle, 0, 0, SRCCOPY );
  finally
    OverlayB.ReleaseDC( DC );
  end;

end;

procedure TfrmMain.SaveAGame( const Name : string );
var
  TempName {,S1,S2} : string;
  DC : HDC;
  WasActive : Boolean;
begin
  WasActive := Active;
  if WasActive then
    Active := False;
  Log.Log( 'Program Save' );
  TempName := GameName;
  try
    GameName := Name;
    {    S1:=DefaultPath+'games\'+TempGame+'.sav';
        S2:=DefaultPath+'games\'+GameName+'.sav';
        if TFile.Exists(S1) then begin
          try
            if TFile.Exists(S2) then TFile.Delete(S2);
            TFile.Copy(S1, S2, True);
          except
            Log.Log('Error: *** Could not copy '+S1+' to ' + S2);
          end;
        end;    }
    if SaveGame then
    begin
      if WasActive then
      begin
        MouseCursor.Cleanup;
        lpDDSFront.GetDC( DC );
        try
          SetStretchBltMode( ScreenShot.Canvas.Handle, HALFTONE );
          StretchBlt( ScreenShot.Canvas.Handle, 0, 0, ScreenShot.width, ScreenShot.Height,
            DC, 0, 0, ScreenShot.width * 3, ScreenShot.Height * 3, SRCCOPY );
        finally
          lpDDSFront.ReleaseDC( DC );
        end;
        try
          if Assigned( ScreenShot ) then
          begin
            Log.Log( 'Saving screenshot: ' + GamesPath + GameName + '.bmp' );
            ForceNotReadOnly( GamesPath + GameName + '.bmp' );
            ScreenShot.SaveToFile( GamesPath + GameName + '.bmp' );
          end;
        except
        end;
        ShowQuickMessage( SaveMsg, 100 );
      end;
    end;
  finally
    GameName := TempName;
  end;
  if WasActive then
    Active := True;
end;

procedure TfrmMain.BeginTransit( const NewFile, SceneName, StartingPoint,
  Transition, TargetList : string );
var
  i : Integer;
begin
  DisableConsole := True;
  trNewFile := NewFile;
  trScenename := SceneName;
  trStartingPoint := StartingPoint;
  trTransition := Transition;
  trTargetList := TargetList;
  for i := 0 to NPCList.Count - 1 do
  begin
    with TCharacter( NPCList.Items[ i ] ) do
    begin
      TransitX := PrevX;
      TransitY := PrevY;
      TransitZ := PrevZ;
    end;
  end;
  PostMessage( Handle, WM_StartTransit, 0, 0 );
end;

procedure TfrmMain.CloseTransit( Sender : TObject );
var
  i : Integer;
  S, S1 : String;
  ResMap, ResGroup : string;
  Found : Boolean;
begin
  if DlgTransit.Cancelled then
  begin
    for i := 0 to NPCList.Count - 1 do
    begin
      with TCharacter( NPCList.Items[ i ] ) do
      begin
        SetPos( TransitX, TransitY, TransitZ );
        Stand;
      end;
    end;
    CloseAllDialogs( DlgTransit );
  end
  else
  begin
    CloseAllDialogs( DlgTransit );
    Found := False;

    with DlgTransit do
    begin
      if ResultIndex < 0 then
      begin
        S := FindMap( DefaultMap );
        if not ( TFile.Exists( S ) ) then
        begin
          Log.Log( '*** Error: Default map not found: ' + DefaultMap );
          Exit;
        end;
        Found := True;
        TransitionScreen := DefaultTransition;
        NewLVLFile := S;
        NewScene := DefaultScene;
        NewStartingPoint := DefaultStartingPoint;
      end
      else
      begin
        S := LowerCase( ResultMapName );
        ResMap := Parse( AnsiString ( S ), 0, '|' );
        ResGroup := Parse( AnsiString ( S ), 1, '|' );
        i := 1; //Targets has a leading '|'
        S := Parse( AnsiString ( Targets ), i, '|' );
        while S <> '' do
        begin
          S1 := Parse( AnsiString ( S ), 0, ',' );
          if S1 = ResMap then
          begin
            S1 := Parse( AnsiString ( S ), 2, ',' );
            if S1 = ResGroup then
            begin
              TransitionScreen := Parse( AnsiString ( S ), 3, ',' );
              NewLVLFile := FindMap( ResMap );
              NewScene := Parse( AnsiString ( S ), 1, ',' );
              NewStartingPoint := ResGroup;
              Found := True;
              Break;
            end;
          end;
          Inc( i );
          S := Parse( AnsiString ( Targets ), i, '|' );
        end;
      end;
    end;

    if Found then
    begin
      DisableConsole := True;
      LoadNewMapFile;
      if Application.Terminated then
        Exit;
      LoadNewLevel := -1;
      Timer2.Enabled := True;
    end;
  end;
end;

procedure TfrmMain.WMStartTransit( var Message : TWMNoParams );
var
  Width, Height : Longint;
  FileName : string;
  X, Y : Longint;

  function GetLevelDims : Boolean;
  var
    LvlStream : TFileStream;
    L, BlockSize, Position : Longint;
  begin
    Result := False;
    LvlStream := TFileStream.Create( FileName, fmOpenRead or fmShareCompat );
    LvlStream.Read( L, SizeOf( L ) ); //Key
    LvlStream.Read( L, SizeOf( L ) ); //Version
    try
      while LvlStream.Position < LvlStream.Size do
      begin
        LvlStream.Read( L, SizeOf( L ) ); //Block Type
        LvlStream.Read( BlockSize, SizeOf( BlockSize ) );
        Position := LvlStream.Position;
        Dec( BlockSize, SizeOf( Longint ) + SizeOf( Longint ) ); //Substract size of block type and size fields
        case TMapBlockTypes( L ) of
          mbMap :
            begin
              LvlStream.Read( Width, SizeOf( Width ) );
              LvlStream.Read( Height, SizeOf( Height ) );
              //Log.Log('  sbMap: '+inttostr(Width)+'x'+inttostr(Height));
              Result := True;
              Break;
            end;
        end;
        if Position + BlockSize > LvlStream.Size then
        begin
          Log.Log( '*** Error: Read past EOF' );
          Break;
        end;
        LvlStream.Seek( Position + BlockSize, soFromBeginning );
        LvlStream.Read( L, SizeOf( L ) );
        if L <> EOB then
        begin
          Log.Log( '*** Error: EOB not found' );
          Break;
        end;
      end;
    finally
      LvlStream.Free;
    end;
  end;

  function GetPathXY( Stream : TStream; const PathName : string ) : Boolean;
  var
    Block : TSavBlocks;
    L, L1, P : Longint;
    S, S0, S1 : AnsiString;
    List : TStringList;
    BB : Word;
  begin
    Result := False;
    S0 := AnsiString ( AnsiLowerCase( PathName ) );
    Stream.Position := 0;
    List := TStringList.Create;
    try
      while Stream.Position < Stream.Size do
      begin
        Stream.Read( Block, SizeOf( Block ) );
        Stream.Read( L, SizeOf( L ) );
        P := Stream.Position;
        if Block = scPathCorner then
        begin
          Stream.Read( L1, SizeOf( L1 ) );
          SetLength( S, L1 );
          Stream.Seek( L1, soFromCurrent ); //skip map name
          Stream.Read( L1, SizeOf( L1 ) );
          SetLength( S1, L1 );
          Stream.Read( S1[ 1 ], L1 );
          List.Text := S1;
          S1 := AnsiString( List.Values[ 'GroupName' ] );
          if LowerCase( S1 ) = S0 then
          begin
            S1 := AnsiString( List.Values[ 'Position' ] );
            X := StrToInt( Parse( S1, 0, ',' ) ) div GameMap.TileWidth;
            Y := StrToInt( Parse( S1, 1, ',' ) ) div GameMap.TileHeight;
            Result := True;
            Break;
          end;
        end;

        Stream.Seek( P + L, soFromBeginning );
        Stream.Read( BB, SizeOf( BB ) );
        if BB <> EOBMarker then
        begin
          Log.Log( '*** Error: EOB not found' );
          Break;
        end;
      end;
    finally
      List.Free;
    end;
  end;

var
  S, S1, Targets : string;
  i : Integer;
  SavFile : TSavFile;
  Properties, MapKnown : TMemoryStream;
  PathName : string;
  PathCornerList : TStringList;
  p1 : ^Byte;
  Offset, Bit : Longint;
  INILanguage : TIniFile;
begin
  Log.Log( 'Transit' );
  Log.Log( '  From: ' + LVLFile );
  Log.Log( '  NewFile: ' + trNewFile );
  Log.Log( '  NewScene: ' + trSceneName );
  Log.Log( '  Starting Point: ' + trStartingPoint );
  //Log.Log('  Targets: '+trTargetList);

  Targets := LowerCase( trTargetList );
  PathCornerList := TStringList.Create;
  SavFile := TSavFile.Create;
  try
    S := GamesPath + TempGame + '.sav';
    if not TFile.Exists( S ) then
      S := GamesPath + GameName + '.sav';
    if TFile.Exists( S ) then
    try
      SavFile.Open( S );

      i := 1;
      S := Parse( AnsiString ( Targets ), i, '|' );
      while S <> '' do
      begin
        S1 := Parse( AnsiString ( S ), 0, ',' );
        SavFile.CurrentMap := S1;
        Properties := SavFile.Properties;
        MapKnown := SavFile.MapKnown;
        if Assigned( Properties ) and Assigned( MapKnown ) then
        begin
          FileName := FindMap( S1 );
          if TFile.Exists( FileName ) then
          begin
            if GetLevelDims then
            begin //Returns Width,Height
              //Scan for listed path groups
              PathName := Parse( AnsiString ( S ), 2, ',' );
              if GetPathXY( Properties, PathName ) then
              begin //Returns X,Y
                if ( X >= 0 ) and ( X < Width ) and ( Y >= 0 ) and ( Y < Height ) then
                begin
                  Offset := Y * Width + X;
                  Bit := 128 shr ( Offset mod 8 );
                  Offset := Offset div 8;
                  Offset := 4 * ( Offset div 4 ) + 3 - ( Offset mod 4 );
                  p1 := MapKnown.Memory;
                  Inc( p1, Offset );
                  if ( p1^ and Bit ) <> 0 then
                  begin
                    PathCornerList.Add( S1 + '|' + PathName );
                  end;
                end
              end;
            end;
          end;
        end;
        Inc( i );
        S := Parse( AnsiString ( Targets ), i, '|' );
      end;
    except
      Log.Log( '*** Error: Transit Error' );
    end;

    if PathCornerList.Count > 0 then
    begin
      //Show transit screen
      Log.Log( '  Show transit' );
      INILanguage := TIniFile.Create( SiegeINILanguageFile );
      try
        DlgTransit.DefaultName := INILanguage.ReadString( 'MapNames', trNewFile, trNewFile );
      finally
        INILanguage.Free;
      end;
      PathCornerList.Add( trNewFile + '|' + trStartingPoint );
      DlgTransit.DefaultMap := trNewFile;
      DlgTransit.DefaultScene := trSceneName;
      DlgTransit.DefaultStartingPoint := trStartingPoint;
      DlgTransit.DefaultTransition := trTransition;
      DlgTransit.Targets := Targets + LowerCase( '|' + trNewFile + ',' + trSceneName + ',' + trStartingPoint + ',' + trTransition );
      DlgTransit.MapsAvailable := PathCornerList;
      DlgTransit.frmMain := Self;
      OpenDialog( DlgTransit, CloseTransit );
    end
    else
    begin
      Log.Log( '  Avoid transit' );
      S := FindMap( trNewFile );
      if not ( TFile.Exists( S ) ) then
      begin
        Log.Log( '*** Error: Selected map not found: ' + trNewFile );
        Exit;
      end;
      TransitionScreen := trTransition;
      NewLVLFile := S;
      NewScene := trSceneName;
      NewStartingPoint := trStartingPoint;
      LoadNewLevel := 1;
      DisableConsole := True;
      Timer2.Enabled := True;
    end;
  finally
    PathCornerList.Free;
    SavFile.Free;
  end;
end;

{procedure TForm1.WMStartTransit(var Message: TWMNoParams);
var
  Stream,LvlStream: TFileStream;
  i,j,k: integer;
  PathCornerList,MapKnownList,List: TStringList;
  Block: TSavBlocks;
  P,L,L1,BlockSize,Position: longint;
  X,Y,Offset,Bit: longint;
  S,S1,S2,Targets: string;
  Width,Height: longint;
  BlockFound: boolean;
  GridSize,MemSize: longint;
  MapKnown,p1: ^byte;
  INILanguage: TINIFile;
begin
Log.Log('Transit');
Log.Log('  From: '+LVLFile);
Log.Log('  NewFile: '+trNewFile);
Log.Log('  NewScene: '+trSceneName);
Log.Log('  Starting Point: '+trStartingPoint);
//Log.Log('Targets: '+trTargetList);

  Targets:=lowercase(trTargetList);

  PathCornerList:=TStringList.create;
  PathCornerList.sorted:=true;
  try
    MapKnownList:=TStringList.create;
    MapKnownList.sorted:=true;
    S:=DefaultPath+'games\'+TempGame+'.sav';
    if not TFile.Exists(S) then
      S:=DefaultPath+'games\'+GameName+'.sav';
//Log.Log('Look for map known and path corner blocks in save file');
//Log.Log('Scanning: '+S);
    if TFile.Exists(S) then try
      Stream:=TFileStream.create(S,fmOpenRead or fmShareCompat);
      try
        //Look for map known and path corner blocks in save file
        while Stream.Position<Stream.Size do begin
          Stream.Read(Block,sizeof(Block));
          Stream.Read(L,sizeof(L));
          P:=Stream.Position;
          if Block=sbMapKnown then begin
            Stream.Read(L1,sizeof(L1));
            SetLength(S,L1);
            Stream.Read(S[1],L1);
//Log.Log('  sbMapKnown: '+S);
            if Pos('|'+lowercase(S),Targets)>0 then begin
              i:=MapKnownList.add(S);
              MapKnownList.objects[i]:=TObject(Stream.position);
            end;
          end
          else if Block=scPathCorner then begin
            Stream.Read(L1,sizeof(L1));
            SetLength(S,L1);
            Stream.Read(S[1],L1);
            k:=Pos('|'+lowercase(S),Targets);
            if k>0 then begin
              Stream.Read(L1,sizeof(L1));
              SetLength(S1,L1);
              Stream.Read(S1[1],L1);
              List:=TStringList.create;
              List.Text:=S1;
              S1:=List.Values['GroupName'];
//Log.Log('  scPathCorner: '+S);
//Log.Log('    Group: '+S1);
              if S1<>'' then begin
                S2:=copy(Targets,k+1,length(Targets)-k);
                if lowercase(S1)=Parse(Parse(S2,0,'|'),2,',') then begin
                  S2:=S+'|'+S1;
                  if PathCornerList.IndexOf(S2)<0 then begin //Pick only the first pathcorner in the group
//Log.Log('    Adding entry: '+S2);
                    i:=PathCornerList.add(S2);
                    PathCornerList.objects[i]:=List;
                    List:=nil
                  end;
                end;
              end;
              if assigned(List) then begin
                List.free;
              end;
            end;
          end;

          Stream.Seek(P+L,soFromBeginning);
          Stream.Read(BB,sizeof(BB));
          if BB<>EOBMarker then begin
            Log.Log('*** Error: EOB not found');
            break;
          end;
        end;

        //Identify which points we have seen
        S2:='';
        BlockFound:=false;
        Width:=0;
        Height:=0;
        MapKnown:=nil;
        for i:=PathCornerList.count-1 downto 0 do begin
          S:=PathCornerList.strings[i];
          S1:=Parse(S,0,'|');  //Name of Map
          if S1<>S2 then begin
            //Load map known block from file
            BlockFound:=false;
            FreeMem(MapKnown);
            try
              //Find the map size block
              S:=FindMap(S1);
              if TFile.Exists(S) then begin
//Log.Log('Identify which points we have seen');
//Log.Log('Scanning: '+S);
                try
                  LvlStream:=TFileStream.create(S,fmOpenRead or fmShareCompat);
                  LvlStream.read(L,sizeof(L)); //Key
                  LvlStream.read(L,sizeof(L)); //Version
                  try
                    while LvlStream.Position < LvlStream.Size do begin
                      LvlStream.read(L,sizeof(L)); //Block Type
                      LvlStream.read(BlockSize,sizeof(BlockSize));
                      Position:=LvlStream.Position;
                      Dec(BlockSize,sizeof(longint)+sizeof(longint)); //Substract size of block type and size fields
                      case TMapBlockTypes(L) of
                        mbMap:
                          begin
                            LvlStream.Read(Width, sizeof(Width));
                            LvlStream.Read(Height, sizeof(Height));
//Log.Log('  sbMapKnown: '+inttostr(Width)+'x'+inttostr(Height));
                            GridSize:=Width*Height;
                            MemSize:=GridSize div 8;
                            if (GridSize mod 8)>0 then inc(MemSize);
                            GetMem(MapKnown,MemSize);
                            j:=MapKnownList.IndexOf(S1);
                            if j>=0 then begin
                              Stream.Position:=longint(MapKnownList.Objects[j]);
                              Stream.read(MapKnown^,MemSize);
                              BlockFound:=true;
                            end;
                            break;
                          end;
                      end;
                      if Position+BlockSize>LvlStream.Size then begin
                        Log.Log('*** Error: Read past EOF');
                        break;
                      end;
                      LvlStream.Seek(Position+BlockSize,soFromBeginning);
                      LvlStream.Read(L,sizeof(L));
                      if L<>EOB then begin
                        Log.Log('*** Error: EOB not found');
                        break;
                      end;
                    end;
                    S2:=S1;
                  except
                    LvlStream.free;
                    break;
                  end;
                  LvlStream.free;
                except
                end;
              end;
            except
              break;
            end;
          end;
          if BlockFound then begin
            //Get location of group of points
            S:=TStringList(PathCornerList.objects[i]).Values['Position'];
//Log.Log('  BlockFound: '+S);
            X:=StrToInt(Parse(S,0,',')) div GameMap.TileWidth;
            Y:=StrToInt(Parse(S,1,',')) div GameMap.TileHeight;
//Log.Log('    ('+inttostr(X)+','+inttostr(Y)+')');
            //Check to see if location is on a known tile
            if (X>=0) and (X<Width) and (Y>=0) and (Y<Height) then begin
              Offset:=Y*Width+X;
//Log.Log('    Offset: '+inttostr(Offset));
              Bit:=128 shr (Offset mod 8);
//Log.Log('    Bit: '+inttostr(Bit));
              Offset:=Offset div 8;
              Offset:=4*(Offset div 4)+3-(Offset mod 4);
//Log.Log('    Byte offset: '+inttostr(Offset));
              p1:=MapKnown;
              inc(p1,Offset);
//Log.Log('    @p1: '+inttostr(p1^));
              //If not, delete the point
              if (p1^ and Bit)=0 then begin
//Log.Log('    Deleting(1): '+PathCornerList.strings[i]);
                TStringList(PathCornerList.objects[i]).free;
                PathCornerList.Delete(i);
              end;
            end
            else begin
//Log.Log('    Deleting(2): '+PathCornerList.strings[i]);
              TStringList(PathCornerList.objects[i]).free;
              PathCornerList.Delete(i);
            end;
          end;
        end;
      finally
        Stream.free;
        MapKnownList.free;
      end;
    except
    end;

    if PathCornerList.count>0 then begin
      //Show transit screen
Log.Log('  Show transit');
      INILanguage := TIniFile.Create(SiegeLanaguageINIflie);
      try
        DlgTransit.DefaultName:=INILanguage.ReadString('MapNames',trNewFile,trNewFile);
      finally
        INILanguage.free;
      end;
      PathCornerList.add(trNewFile+'|'+trStartingPoint);
      DlgTransit.DefaultMap:=trNewFile;
      DlgTransit.DefaultScene:=trSceneName;
      DlgTransit.DefaultStartingPoint:=trStartingPoint;
      DlgTransit.DefaultTransition:=trTransition;
      DlgTransit.Targets:=Targets+lowercase('|'+trNewFile+','+trSceneName+','+trStartingPoint+','+trTransition);
      DlgTransit.MapsAvailable:=PathCornerList;
      DlgTransit.frmMain:=self;
      OpenDialog(DlgTransit,CloseTransit);
    end
    else begin
Log.Log('  Avoid transit');
      S:=FindMap(trNewFile);
      if not(TFile.Exists(S)) then begin
        Log.Log('*** Error: Selected map not found: '+trNewFile);
        exit;
      end;
      TransitionScreen:=trTransition;
      NewLVLFile:=S;
      NewScene:=trSceneName;
      NewStartingPoint:=trStartingPoint;
      LoadNewLevel:=1;
      DisableConsole:=true;
      Timer2.enabled:=true;
    end;
  finally
    for i:=0 to PathCornerList.count-1 do
      TStringList(PathCornerList.objects[i]).free;
    PathCornerList.free;
  end;
end;   }

function TfrmMain.FindMap( const FileName : string ) : string;
var
  S, S1 : string;
  i : Integer;
begin
  i := 0;
  S := Parse( MapPath, i, ';' );
  while S <> '' do
  begin
    if S[ Length( S ) ] <> '\' then
      S := S + '\';
    S1 := S + FileName + '.lvl';
    if TFile.Exists( S1 ) then
    begin
      Result := S1;
      Exit;
    end;
    Inc( i );
    S := Parse( MapPath, i, ';' );
  end;

  Result := MapPath + FileName + '.lvl';
end;

procedure TfrmMain.ClearOnDemandResources;
var
  i : Integer;
begin
  ClearSpellResources;

  for i := 0 to Figures.Count - 1 do
  begin
    if Assigned( Figures.Objects[ i ] ) then
    begin
      if TResource( Figures.Objects[ i ] ).OnDemand then
      begin
        TResource( Figures.Objects[ i ] ).RLE.Free;
        TResource( Figures.Objects[ i ] ).RLE := nil;
      end;
    end;
  end;
end;

procedure TfrmMain.Timer3Timer( Sender : TObject );
const
  FailName : string = 'Main.Timer3Timer';
var
  pr : TRect;
begin
  Log.DebugLog(FailName);
  try
    try
      Active := False;
      if Timer3.Tag > 0 then
      begin
        pr := Rect( 0, 0, ResWidth, ResHeight );
        lpDDSBack.BltFast( 0, 0, lpDDSFront, @pr, DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
        FillRectSub( lpDDSBack, Rect( 0, 0, 703, 511 ), $202020 );
        if FSpellBarActive then
        begin
          pr := Rect( 0, 0, ScreenMetrics.ScreenWidth, 114 );
          lpDDSBack.BltFast( 0, ScreenMetrics.SpellBarY, SpellBar, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
          pr := Rect( 0, 0, 195, 59 );
          lpDDSBack.BltFast( 250, ScreenMetrics.HelpBoxY, HelpBox, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        end
        else
        begin
          pr := Rect( 0, 0, ScreenMetrics.ScreenWidth, 114 );
          lpDDSBack.BltFast( 0, ScreenMetrics.SpellBarY, OverlayB, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        end;
        pr := Rect( 0, 0, 117, ScreenMetrics.SpellBarY );
        lpDDSBack.BltFast( ScreenMetrics.SpellBarX, 0, OverlayR, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        DrawHealthBars;
        lpDDSFront.Flip( nil, DDFLIP_WAIT );
        MouseCursor.PlotDirty := False;
        Timer3.Tag := Timer3.Tag - 1;
        if Timer3.Tag = 0 then
        begin
          Timer3.Enabled := False;
          if Application.Terminated then
            Exit;
          Timer3.Tag := -1;
          Timer3.Enabled := True;
        end;
      end
      else if Timer3.Tag < 0 then
      begin
        Timer3.Tag := Timer3.Tag + 1;
        if Timer3.Tag = 0 then
        begin
          RunScript( Player, BlackScript );
          Timer3.Enabled := False;
          DisableConsole := False;
          ShowQuickMessage( '', 0 );
          Active := True;
          Game.ForceRefresh := True;
          Paused := False;
          MouseCursor.SetFrame( 37 );
          PrevTriggerID := -1;
          MouseCursor.Enabled := True;
        end;
      end
      else
        Timer3.Enabled := False;
    except
      Timer3.Enabled := False;
      DisableConsole := False;
      ShowQuickMessage( '', 0 );
      Active := True;
      Game.ForceRefresh := True;
      Paused := False;
      MouseCursor.SetFrame( 37 );
      PrevTriggerID := -1;
      MouseCursor.Enabled := True;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;

end;

procedure TfrmMain.ShowEnding;
var
  INILanguage : TIniFile;
  S : string;
  List : TArray<string>;
  i : Integer;
begin
  AdventureLog1.Clear;
  INILanguage := TIniFile.Create( SiegeINILanguageFile );
  try
    S := INILanguage.ReadString( 'Settings', 'Ending', '' );
    if S = '' then
    begin
      PostMessage( Handle, WM_StartMainMenu, 0, 0 );
    end
    else
    begin
      List := S.Split([',']);
      for i := 0 to Length(List) - 1 do
        AdventureLog1.AddLogEntry( List[ i ] + '.jrn' );
      DlgJournal.StartLogIndex := -1;
      DlgJournal.pText := DlgText;
      DlgJournal.OnClose := CloseIntroJournal;
      DlgJournal.frmMain := Self;
      DlgJournal.JournalLog := AdventureLog1;
      Game.OnMouseDown := nil;
      Game.OnMouseMove := nil;
      Game.OnMouseUp := nil;
      OnKeyDown := nil;
      MouseCursor.SetFrame( 37 );
      PrevTriggerID := -1;
      MouseCursor.Enabled := True;
      ClearLogGraphic;
      DisableConsole := True;
      OpenDialog( DlgJournal, CloseEnding );
    end;
  finally
    INILanguage.Free;
  end;

end;

procedure TfrmMain.CloseEnding( Sender : TObject );
const
  FailName : string = 'Main.CloseEnding';
begin
  Log.DebugLog(FailName);
  try
    if DlgJournal.Loaded then
      DlgJournal.Release;
    Active := False;
    NewGame := True;
    ClearResources( True );
    Sprites.Realloc;
    Game.ForceRefresh := True;
    Game.Enabled := True;
    Game.ForceRefresh := True;

    if TFile.Exists( ClosingMovie ) then
    begin
      bPlayClosingMovie := True;
      PostMessage( Handle, WM_Done, 0, 0 );
    end
    else
      PostMessage( Handle, WM_StartMainMenu, 0, 0 ); //Restart the intro
    //  PostMessage(handle,WM_StartCredits,0,0);

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure ForceNotReadOnly( const FileName : string );
var
  Attr : Integer;
begin
  Attr := FileGetAttr( FileName );
  if Attr = -1 then
  begin
    Exit;
  end;
  if ( Attr and faReadOnly ) <> 0 then
  begin
    Attr := Attr and ( not faReadOnly );
    FileSetAttr( FileName, Attr );
  end;
end;

procedure TfrmMain.ShowHistroy;
var
  //HistoryLog: TAdventureLog;
  Ini : TIniFile;
  IniLanguage : TIniFile;
  s : string;
  i : Integer;
  List : TArray<string>;
begin
  INI := TIniFile.Create( SiegeINIFile );

  if ( INI.ReadInteger( 'Settings', 'JournalFont', 0 ) = 1 ) and TDirectory.Exists( ResourcePath + 'journalalt\' + Language) then
    HistoryLog.LogDirectory := ResourcePath + 'journalalt\' + Language + '\'
  else
    HistoryLog.LogDirectory := ResourcePath + 'journal\' + Language + '\';

  try
    IniLanguage := TIniFile.Create( SiegeINILanguageFile );
    try
      S := INILanguage.ReadString( 'Settings', 'History', '' );
    finally
      IniLanguage.Free;
    end;
    if S = '' then
    begin
      PostMessage( Handle, WM_StartMainMenu, 0, 0 );
    end
    else
    begin
      List := S.Split([',']);
      for i := 0 to Length(List) - 1 do
        HistoryLog.AddLogEntry( List[ i ] + '.jrn' );
      DlgJournal.StartLogIndex := -1;
      DlgJournal.pText := DlgText;
      DlgJournal.OnClose := CloseIntroJournal;
      DlgJournal.frmMain := Self;
      DlgJournal.JournalLog := HistoryLog;
      Game.OnMouseDown := nil;
      Game.OnMouseMove := nil;
      Game.OnMouseUp := nil;
      OnKeyDown := nil;
      MouseCursor.SetFrame( 37 );
      PrevTriggerID := -1;
      //     DlgJournal.Init;
      MouseCursor.Enabled := True;
      //ClearLogGraphic;
      DisableConsole := True;
      OpenDialog( DlgJournal, CloseHistory );

    end;
  finally
    INI.Free;
  end;

end;

procedure TfrmMain.CloseHistory( Sender : TObject );
const
  FailName : string = 'Main.CloseHistory';
begin
  Log.DebugLog(FailName);
  try
    if DlgJournal.Loaded then
      DlgJournal.Release;
    Active := False;
    Game.ForceRefresh := True;
    Game.Enabled := True;
    Game.ForceRefresh := True;

    PostMessage( Handle, WM_StartMainMenu, 0, 0 ); //Restart the intro
    //  PostMessage(handle,WM_StartCredits,0,0);

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

initialization
  begin
    Log := TLog.Create( ExtractFilePath( Application.ExeName ) + 'Siege.log' );
    //  CoInitialize(nil);
  end;

finalization
  begin
    Log.Comment( 'Terminating application...' );
    if Assigned( MusicLib ) then
    begin
      Log.Comment( 'Freeing music' );
      MusicLib.Free;
      MusicLib := nil;
    end;
    Log.Comment( 'Terminating application...' );
    if Assigned( SoundLib ) then
    begin
      Log.Comment( 'Freeing sound' );
      //    SoundLib.FreeAllSounds;
      SoundLib.Free;
      SoundLib := nil;
    end;
    Log.Comment( 'Application terminated' );
    Log.Free;
    Log := nil;
    //  CoUninitialize;
  end;

end.
