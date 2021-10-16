unit SoAOS.Intrface.KeyEvents;
(*
  Siege Of Avalon : Open Source Edition

  Portions created by Digital Tome L.P. Texas USA are
  Copyright ©1999-2000 Digital Tome L.P. Texas USA
  All Rights Reserved.

  Portions created by Team SOAOS are
  Copyright (C) 2003 - Team SOAOS.

  Portions created by Steffen Nyeland are
  Copyright (C) 2021 - Steffen Nyeland.

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

  Description: Was part of AniDemo.pas

  Requires: Delphi 10.3.3 or later

  Revision History:
  - 21 Jan 2021 - SN: Initial commit
  see git repo afterwards

*)

interface

uses
  System.SysUtils,
  System.Classes,
  Display;

type
  TKeyEvent = class
  private
    class function ToggleShow(dialog: TDisplay): Boolean;
    class procedure ToggleSpell;
    class procedure ToggleXRay;
    class procedure TwoWeapons; //Ashes of Avalon
    class procedure HealPotion; //Ashes of Avalon
    class procedure ManaPotion; //Ashes of Avalon
    class procedure AdjustGlobalBrightness(step: Integer);
    class procedure ToggleCombat;
    class procedure QuickSave;
    class procedure ScreenShot;
    class procedure ShowMenu;
    class procedure SpellHotKey(key: Word);
    class procedure SpellHotKeyPlus(key: Word);
    class procedure TravelFast;  //SoA and AoA
//    class procedure DemoOrDeath; // Testcode needs to go
  public
    class procedure TogglePause;
    class procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  end;

implementation

uses
  Winapi.Windows,
  System.IOUtils,
  System.IniFiles,
  System.Types,
  Vcl.Graphics,
  AniDemo,
  SoAOS.AI.Helper,
  SoAOS.Types,
  Character,
  Engine,
  LogFile,
  Parts, //for TwoWeapons
  Resource, //for TwoWeapons
  SoAOS.Animation,
  SoAOS.Effects,
  DirectX,
  DXEffects;

class procedure TKeyEvent.AdjustGlobalBrightness(step: Integer);
var
  newGlobalBrightness: Integer;
  INI: TINIFile;
begin
{ TODO : Brightness changes needs reload/render of map to show. }
  newGlobalBrightness := GlobalBrightness + step;
  if newGlobalBrightness>255 then newGlobalBrightness := 255;
  if newGlobalBrightness<0 then newGlobalBrightness := 0;

  if newGlobalBrightness<>GlobalBrightness then
  begin
    INI := TIniFile.Create(SiegeINIFile);
    try
      { TODO : Save and reload map/lvl - or find a way to refresh with new brightness }
      INI.WriteInteger( 'Settings', 'Brightness', newGlobalBrightness );
      GlobalBrightness := newGlobalBrightness;
      // Game.RefreshMap;
    finally
      INI.Free;
    end;
  end;
end;

//class procedure TKeyEvent.DemoOrDeath;
////var
////  i, n: Integer;
//begin
////  n := 0;
////  for i:=0 to FigureInstances.count-1 do
////  begin
////    if (FigureInstances.Objects[i] is TCharacter) and (FigureInstances.Objects[i]<>Player) and
////      Player.isAlly( TCharacter( FigureInstances.Objects[i] ) ) and not TCharacter( FigureInstances.Objects[i] ).Dead then
////    begin
////      frmMain.AddToParty(TAniFigure(FigureInstances.Objects[i]));
////      inc(n);
////      if n >= MaxPartyMembers then
////        break;
////    end;
////  end;
//
////  player.hitpoints := -1;
////  player.trainingpoints := 10000;
////  player.money := 10000;
////  player.mana := 100;
////
////  Adventures.Add('ch4-531');
////  RunScript(Player, 'player.additem(MagicalMask)');
////  RunScript(Player, 'addtitle(04maskgiven)');
//                                                                    ;
////     AddAdventure('a');
////     AddQuest('a');
////     AddLogEntry('a');
////     end
//end;

class procedure TKeyEvent.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
const
  FailName: string = 'Main.FormKeyDown';
begin
  Log.DebugLog(FailName);
  try
    if frmMain.DisableConsole then
      Exit;

    case Key of
      VK_TAB: ShowPersistentMap := not ShowPersistentMap;
      VK_ESCAPE: ShowMenu; // ESC
      VK_SPACE: ToggleCombat; // Space
      48..57: SpellHotKey(Key); // 0-9 alternative to F-keys
      65: if ToggleShow(DlgTitles) then frmMain.BeginTitles(Current); // A
      66: Current.DoBattleCry; // B
      67: if ToggleShow(DlgStatistics) then frmMain.BeginStatistics(Current); // C
//      68: DemoOrDeath; //D test code
      68: ManaPotion; //D, only AoA
      69: HealPotion; //E, only AoA
      71: ScreenShot; // G
      73: if ToggleShow(DlgInventory) then frmMain.BeginInventory(Current); // I
      74: if ToggleShow(DlgJournal) then frmMain.BeginJournal; // J
      76: if ToggleShow(DlgAdvLog) then frmMain.BeginAdvLog; // L
      77: if ToggleShow(DlgMap) then frmMain.BeginMap(Current); // M
      79: if ToggleShow(DlgOptions) then frmMain.BeginOptions(Current); // O
      80, VK_PAUSE: TogglePause; // P or Pause
      81: if ToggleShow(DlgQuestLog) then frmMain.BeginQuestLog; // Q
      82: if ToggleShow(DlgRoster) then frmMain.BeginRoster(nil); // R
      83: ToggleSpell; // S
      84: TravelFast; // T
//      87: WeaponSwitch; ; // W - Reserved for short/long distance weapons?
      88: ToggleXRay; // X
      90: TwoWeapons; //Z, only AoA
      //  90: HDZoom; // Z - Reserved for future? HD Zoom function
      114..124: SpellhotkeyPlus(key); //F3-F12,
      VK_F1: if ToggleShow(DlgShow) then frmMain.BeginHelp; // F1
      VK_F2: QuickSave; // F2
      VK_OEM_PLUS, VK_ADD: AdjustGlobalBrightness(10);
      VK_OEM_MINUS, VK_SUBTRACT: AdjustGlobalBrightness(-10);
    end;

    // TODO: Remove DEBUG keys...unless they work
    { else if (Key=114) or (Key=115) then begin     // F3 - F4 Debugmode
      if CurrDbgLvl=0 then begin
      CurrDbgLvl:=3;
      ShowQuickMessage('Debug On',150);
      end
      else begin
      CurrDbgLvl:=0;
      ShowQuickMessage('Debug Off',150);
      end;
      end }
  except
    on E: Exception do
      Log.Log(FailName, E.Message, []);
  end;
end;

//class procedure TKeyEvent.LforWhat;
//var
//  i: Integer;
//begin //L
//  player.hitpoints := -1;
//
//  for i := 0 to  Npclist.Count -1 do
//    NPCList[i].hitpoints := -1;
//
//  Player.AddTitle('Firefly');
//  Player.Frozen:=false;
//  frmMain.ShowQuickMessage('EmHmImKmXmYm0123456789',8000);
//  frmMain.ShowQuickMessage('AÄOÖUÜBßaäoöuüO',8000);
//  frmMain.ShowQuickMessage('Ab Kb Lb Qb Rb Xb Yb',8000);
//  frmMain.ShowQuickMessage('m0m1m2m3m4m5m6m78m9m',8000);
//  Player.Mysticism:=100;
//  Player.AddEffect(TBodyRotEffect.create);
//  frmMain.BeginTransit('Spawn','','Group1','','|ob1,,group2|spawn,,test');
//  DlgTransit.frmMain:= frmMain;
////  OpenDialog(DlgTransit,CloseTransit);
//  Player.Mysticism:=1000;
//
//  RunScript(Player,'player.additem(AhoulArmGuards,1234)');
//  RunScript(Player,'Targetable200008110049155730.doaction(death)');
//  RunScript(Player,'Lich200008250075954760.doaction(death)');
//  RunScript(Player,'SaveGame(baba)');
//  Player.AddEffect(GetNamedEffect('deathpulse'));
//  Player.AddEffect(TBurningRam.create);
//
//  RunScript(Player,'doaction(death);doeffect(spirit)');
//  frmMain.ShowQuickmessage( 'Suizid ist auch eine Lösung',300);
//
//  for i:=0 to FigureInstances.count-1 do
//  begin
//    if FigureInstances.objects[i] is tcharacter then
//    begin
//      if pos('88620',tcharacter(FigureInstances.objects[i]).guid)>0 then
//      begin
//        frmMain.AddToParty(tcharacter(FigureInstances.objects[i]));
//        frmMain.ShowQuickMessage('Found him!',600);
//      end;
//    end;
//  end;
//
//  RunScript(Current,'journalentry(A);adventure(B);AddQuest(quest1)');
//end;

class procedure TKeyEvent.QuickSave;
var
  TempName: string;
begin
  if frmMain.Active then
  begin
  if not player.titleexists('HardMode') then
  begin
    frmMain.Active := False;
    Log.Log('QuickSave');
    TempName := GameName;
    try
      GameName := QuickSaveName;
      if frmMain.SaveGame then
      begin
        frmMain.SaveGameScreenShot;
        try
          if Assigned(frmMain.ScreenShot) then
          begin
            Log.Log('Saving screenshot: ' + GamesPath + GameName + '.bmp');
            frmMain.ScreenShot.SaveToFile(GamesPath + GameName + '.bmp');
          end;
        except
        end;
        frmMain.ShowQuickMessage(SaveMsg, 100);
      end;
    finally
      GameName := TempName;
      frmMain.Active := True;
    end;
  end;
  end;
end;
class procedure TKeyEvent.ScreenShot;
var
  BM: TBitmap;
  i: Integer;
  S: string;
  DC: HDC;
begin
  try
    BM := TBitmap.Create;
    try
      BM.width := ResWidth;
      BM.Height := ResHeight;
      MouseCursor.Cleanup;
      lpDDSFront.GetDC(DC);
      try
        BitBlt(BM.Canvas.Handle, 0, 0, ResWidth, ResHeight, DC, 0,
          0, SRCCOPY);
      finally
        lpDDSFront.ReleaseDC(DC);
      end;
      i := Trunc(Date);
      i := i shl 12;
      repeat
        Inc(i);
        S := IncludeTrailingBackslash(TPath.GetPicturesPath) + 'SiegeScreenShot' + IntToHex(i, 8) + '.bmp';
      until not TFile.Exists(S);
      BM.PixelFormat := pf24bit;
      BM.SaveToFile(S);
    finally
      BM.Free;
    end;
  except
  end;
end;

class procedure TKeyEvent.ShowMenu;
begin
  if DlgObjInventory.Loaded then
    frmMain.CloseAllDialogs( DlgObjInventory )
  else if DlgLoot.Loaded then
    frmMain.CloseAllDialogs( DlgLoot )
  else if DlgMerchant.Loaded then
    frmMain.CloseAllDialogs( DlgMerchant )
  else if DlgStatistics.Loaded then
    frmMain.CloseAllDialogs( DlgStatistics )
  else if DlgInventory.Loaded then
    frmMain.CloseAllDialogs( DlgInventory )
  else if DlgQuestLog.Loaded then
    frmMain.CloseAllDialogs( DlgQuestLog )
  else if DlgAdvLog.Loaded then
    frmMain.CloseAllDialogs( DlgAdvLog )
  else if DlgTitles.Loaded then
    frmMain.CloseAllDialogs( Dlgtitles )
  else if DlgRoster.Loaded then
    frmMain.CloseAllDialogs( DlgRoster )
  else if DlgMap.Loaded then
    frmMain.CloseAllDialogs( DlgMap )
  else //Also possible, but closing an interface-dialogue by pressing esc
    //without going to mainmenu is a cool feature
    //frmMain.CloseAllDialogs(nil);
    begin
      if frmMain.Paused then
        TogglePause;

      if player.titleexists('HardMode') then
        Hardmode:= true
      else
        Hardmode := false;

      frmMain.Active := False;
      frmMain.SaveGameScreenShot;
      PostMessage(frmMain.Handle, WM_StartMainMenu, 0, 0); // Restart the intro
    end;
end;
class procedure TKeyEvent.SpellHotKey(key: Word);
var
  offset: Word;
begin
  offset := 0;
  if (Key >= 48) and (Key < 58) then    // 0-9
    offset := 47;

  if (offset>0) and Assigned(Current.HotKey[Key - offset]) then
  begin
    Current.CurrentSpell := Current.HotKey[Key - offset];
    frmMain.DrawCurrentSpell;
  end;
  if frmMain.SpellBarActive then
    frmMain.DrawSpellGlyphs;
end;

class procedure TKeyEvent.SpellHotKeyPlus(key : word);
begin
if Assigned( Current.HotKey[ key - 113 + 10 ] ) then
      begin
        Current.CurrentSpell := Current.HotKey[ key - 113 + 10 ];
        frmMain.DrawCurrentSpell;
      end;
      if frmMain.SpellbarActive then
        frmMain.DrawSpellGlyphs;
      (*if key = 121 then  //F10 abfangen, Damit die Menüfunktion (Kontextmenü) nicht ausgeführt wird, (nur bei ddraw.dll mit libwine.dll und wined3d.dll)
        key := 0;*)
end;

class procedure TKeyEvent.ToggleCombat;
begin
  Current.CombatMode := not Current.CombatMode;
  NPCList.SetCombatMode(Current.CombatMode);
  if Current.CombatMode then
  begin
    if Assigned(frmMain.HLFigure) then
    begin
      frmMain.HLFigure.Highlighted := False;
      frmMain.HLFigure := nil;
    end;
  end;
end;

class procedure TKeyEvent.TogglePause;
var
  i: Integer;
  DC: HDC;
  HpDistance, ManaDistance: Double;
  pr, pr0: TRect;
begin
  if frmMain.Active xor frmMain.Paused then
  begin
    frmMain.Paused := not frmMain.Paused;
    if frmMain.Paused then
    begin
      frmMain.Active := False;
      frmMain.ShowQuickMessage('', 1);
      frmMain.MouseMessage := '';
      if Assigned(frmMain.HLFigure) then
      begin
        frmMain.HLFigure.Highlighted := False;
        frmMain.HLFigure := nil;
      end;
      frmMain.OverlayB.GetDC(DC);
      try
        BitBlt( DC, ScreenMetrics.BottomBarX, 30, 202, 68, frmMain.imgBottomBar.Canvas.Handle, ScreenMetrics.BottomBarX, 30, SRCCOPY );
      finally
        frmMain.OverlayB.ReleaseDC(DC);
      end;

      DrawAlpha(frmMain.OverlayB,
        Rect(ScreenMetrics.PauseX, 53, ScreenMetrics.PauseX + 73 { imgPaused.width } ,
        53 + 23 { imgPaused.Height } ), Rect(0, 0, 73 { imgPaused.width } ,
        23 { imgPaused.Height } ), frmMain.PauseImage, True, 170);
      pr := Rect(0, 0, ScreenMetrics.ScreenWidth, 114);

// serge: the following Blt spoils the bottom of the game screen when window unfocuses
//      lpDDSFront_BltFast(0, ScreenMetrics.SpellBarY, frmMain.OverlayB, @pr,
//        DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
      if Assigned(frmMain.PauseLayer) then
      begin
        frmMain.PauseLayer.Enabled := True;
        frmMain.PauseLayer.SetPosition(Point(ScreenMetrics.PauseX, 53 + ScreenMetrics.SpellBarY));
      end;

      for i := 1 to NPCList.Count - 1 do
      begin
        HpDistance := NPCList[i].HitPoints - NPCList[i].Wounds;
        if HpDistance > NPCList[i].HitPoints then
          HpDistance := NPCList[i].HitPoints
        else if HpDistance < 0 then
          HpDistance := 0;

        ManaDistance := NPCList[i].Mana -
          NPCList[i].Drain;
        if ManaDistance > NPCList[i].Mana then
          ManaDistance := NPCList[i].Mana
        else if ManaDistance < 0 then
          ManaDistance := 0;

        HpDistance := HpDistance * (66 / NPCList[i].HitPoints);
        ManaDistance := ManaDistance * (66 / NPCList[i].Mana);
        pr := Rect(frmMain.NPCBarXCoord[i], ScreenMetrics.NPCBarY -
          Round(HpDistance), frmMain.NPCBarXCoord[i] + 5,
          ScreenMetrics.NPCBarY);

        pr0 := Rect(0, 0, 0, 0);
        lpDDSFront.Blt(@pr, nil, @pr0, DDBLT_COLORFILL + DDBLT_WAIT,
          @frmMain.NPCHealthBltFx);
        pr := Rect(frmMain.NPCBarXCoord[i] + 7, ScreenMetrics.NPCBarY -
          Round(ManaDistance), frmMain.NPCBarXCoord[i] + 7 + 5,
          ScreenMetrics.NPCBarY);

        lpDDSFront.Blt(@pr, nil, @pr0, DDBLT_COLORFILL + DDBLT_WAIT,
          @frmMain.NPCManaBltFx);
      end;
    end
    else
    begin
      frmMain.Active := True;
      if Assigned(frmMain.PauseLayer) then
      begin
        frmMain.PauseLayer.Enabled := False;
      end;
    end;
  end;
end;

class function TKeyEvent.ToggleShow(dialog: TDisplay): Boolean;
begin
  Result := not dialog.Loaded;
  if dialog.Loaded then
    frmMain.CloseAllDialogs(dialog)
  else
  begin
    frmMain.DoNotRestartTimer := True;
    frmMain.CloseAllDialogs(dialog);
  end;
end;

class procedure TKeyEvent.ToggleSpell;
begin
  frmMain.SpellBarActive := not frmMain.SpellBarActive;
  if frmMain.SpellBarActive then
    frmMain.DrawSpellGlyphs;
end;

class procedure TKeyEvent.ToggleXRay;
begin
  frmMain.XRayOn := not frmMain.XRayOn;
  if Assigned(Current) then
    TCharacter(Current).AutoTransparent := frmMain.XRayOn;
end;

class procedure TKeyEvent.TravelFast;
begin
// NoTransit
// LoadNewMap( const NewFile(lvl), SceneName, StartingPoint, Transition(bmp) : string );
  if (not NoTransit) and (not Current.Frozen) and ( current.Ready ) then //nicht FReady wenn Lähmzauber
  begin
    if modselection = TModSelection.SoA then  //SoA
    begin
    if player.titleexists('03Chapter3') and not player.titleexists('04Chapter4') then
//      RunScript(player, 'Loadmap(forest05,default,Start,ForestChpt3|#FastTransit.Default#)');
      RunScript(player, 'Loadmap(forest05,default,f05b02,ForestChpt3|#FastTransit.Default#)');  // Better spot when having party members
//    if player.titleexists('02Chapter2') then
//      RunScript(player, 'Loadmap(southgate1b,default,Levelpoint4,VillagetoSouthGate|#FastTransit.Default#)')
//    else
//      RunScript(player, 'Loadmap(okeepl2,default,Start|#FastTransit.Default#)')
    end;
    if modselection = TModSelection.AoA then //AoA
    begin
      if current.Ready then  //Otherwise possible to escape from the hold spell by pressing t
      begin
        if not player.titleexists('Schnellerwechselaus') then //Titel für spezielle Situationen im Spiel.
        begin
          if player.titleexists('chapter06') then
          runscript(player,'Loadmap(southgate1b,default,Levelpoint4|#Schnellreise.Fall5#)')
          else if player.titleexists('chapter05') then
          runscript(player,'Loadmap(southgate1b,default,Levelpoint4|#Schnellreise.Fall4#)')
          else if player.titleexists('chapter04') then
          runscript(player,'Loadmap(southgate1b,default,Levelpoint4|#Schnellreise.Fall3#)')
          else if player.titleexists('chapter03') then
          runscript(player,'Loadmap(03Wald1,default,forst,Wald|#Schnellreise.Fall2#)')
          else if player.titleexists('chapter02') then
          begin
              if player.titleexists('ImForst') then
              runscript(player,'Loadmap(Wald1,default,forst,Wald|#Schnellreise.Fall1#)')
              else
              runscript(player,'Loadmap(southgate1b,default,Levelpoint4|#Schnellreise.Fall1#)');
          end
          else if not player.titleexists('chapter02') then
          runscript(player,'Loadmap(southgate1b,default,Levelpoint4|#Schnellreise.Fall1#)');
        end;
      end;//end FReady
    end;//end modselection -AoA
  end;
end;

class procedure TKeyEvent.TwoWeapons;
var
  WeaponEquip : string;
  ShieldEquip : string;
begin
  if modselection = TModSelection.AoA then
  begin
    if DlgInventory.Loaded then
    begin
      if current.Equipment [slshield] = nil then
      begin
        if current.titleExists('EinhandschwertRechts') and ( current.Strength + 1 > TWeapon( current.Equipment[ slWeapon ] ).MinStrength ) and ( current.Coordination + 1 > TWeapon( current.Equipment[ slWeapon ] ).MinCoordination ) then
        begin
          frmMain.CloseAllDialogs( DlgInventory );
          WeaponEquip := current.Equipment[ slWeapon ].ItemName;
          current.Equipment [ slweapon ] := nil;
          current.Equipment [ slshield ] := PartManager.LoadItem( WeaponEquip + 'Shield', TCharacterResource( current.Resource ).NakedName );
          current.Equipment[ slshield ].Resource := PartManager.GetLayerResource( current.Equipment[ slshield ].LayeredImage );
          current.equipmentlocked[ slshield ] := true;
          frmMain.DoNotRestartTimer := True;       //->Inventory
          frmMain.CloseAllDialogs( DlgInventory ); //re-
          frmMain.BeginInventory( Current );       //load
        end;
      end
      else //not shield = nil
      begin
        if current.titleExists('EinhandschwertLinks') then
        begin
          frmMain.CloseAllDialogs( DlgInventory );
          ShieldEquip := current.equipment[ slshield ].itemname;
          RunScript(current,'current.removeitem(' + ShieldEquip + ')');
          WeaponEquip := StringReplace(ShieldEquip, 'shield', '', [rfIgnoreCase]);
          RunScript(current,'current.additem(' + WeaponEquip + ')');
          current.equipmentlocked[ slshield ] := false;
          //log.log(ShieldEquip);
          //log.log(WeaponEquip);
          frmMain.DoNotRestartTimer := True;       //->Inventory
          frmMain.CloseAllDialogs( DlgInventory ); //re-
          frmMain.BeginInventory( Current );       //load
        end;
      end;
    end;
  end;
end;

class procedure TKeyEvent.HealPotion;
begin
  if (Modselection=TModSelection.AoA) and Current.Ready then  //Not when e.g. Holdspell casted
    if (not DlgInventory.Loaded) then
      Current.UseHealPotion;
end;

class procedure TKeyEvent.ManaPotion;
begin
  if (Modselection=TModSelection.AoA) and Current.Ready then //Not when e.g. Holdspell casted
    if (not DlgInventory.Loaded) then
      Current.UseManaPotion;
end;

end.
