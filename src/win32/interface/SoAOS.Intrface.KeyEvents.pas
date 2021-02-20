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
    class procedure AdjustGlobalBrightness(step: Integer);
    class procedure TogglePause;
    class procedure ToggleCombat;
    class procedure QuickSave;
    class procedure ScreenShot;
    class procedure ShowMenu;
    class procedure SpellHotKey(key: Word);
    class procedure TransitBack; // Revise
    class procedure TwinWeaponToggle; // Revise

    class procedure DemoOrDeath; // Testcode addparty sample
    class procedure LforWhat; // Testcode mess sample
  public
    class procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  end;

implementation

uses
  Winapi.Windows,
  System.IOUtils,
  System.IniFiles,
  Vcl.Graphics,
  AniDemo,
  SoAOS.AI.Helper,
  Character,
  Engine,
  LogFile,
  SoAOS.Animation,
  SoAOS.Effects,
  DirectX,
  DXEffects;

class procedure TKeyEvent.AdjustGlobalBrightness(step: Integer);
var
  newGlobalBrightness: Integer;
  INI: TINIFile;
begin
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

class procedure TKeyEvent.DemoOrDeath;
//var
//  i: Integer;
begin
//  for i:=0 to FigureInstances.count-1 do
//  begin
//    if (FigureInstances.Objects[i] is TCharacter) and (FigureInstances.Objects[i]<>Player) then
//    begin
//      frmMain.AddToParty(TAniFigure(FigureInstances.Objects[i]));
//      break;
//    end;
//  end;
    // AddAdventure('a');
    // AddQuest('a');
    // AddLogEntry('a');
    // end
end;

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
      VK_ESCAPE: ShowMenu; // ESC
      VK_SPACE: ToggleCombat; // Space
      48..57: SpellHotKey(Key); // 0-9 alternative to F-keys
      65: if ToggleShow(DlgTitles) then frmMain.BeginTitles(Current); // A
      66: Current.DoBattleCry; // B
      67: if ToggleShow(DlgStatistics) then frmMain.BeginStatistics(Current); // C
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
      84: TransitBack; // T
      87: if ToggleShow(DlgAdvLog) then frmMain.BeginAdvLog; // W
      88: ToggleXRay; // X
      90: TwinWeaponToggle; // Z - FIX - German only :(
      VK_F1: if ToggleShow(DlgShow) then frmMain.BeginHelp; // F1
      VK_F2: QuickSave; // F2
      VK_OEM_PLUS, VK_ADD: AdjustGlobalBrightness(10);
      VK_OEM_MINUS, VK_SUBTRACT: AdjustGlobalBrightness(-10);
    end;

//    else if ( key = 76 ) then LforWhat  // L test code
//    else if (Key = 68) then DemoOrDeath //D test code

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

class procedure TKeyEvent.LforWhat;
//var
//  i: Integer;
begin //L
//  player.hitpoints := -1;
//
//  for i := 0 to  Npclist.Count -1 do
//    TCharacter(npclist.Items[i]).hitpoints := -1;
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
end;

class procedure TKeyEvent.QuickSave;
var
  DC: HDC;
  TempName: string;
begin
  if frmMain.Active then
  begin
    frmMain.Active := False;
    Log.Log('QuickSave');
    TempName := GameName;
    try
      GameName := QuickSaveName;
      if frmMain.SaveGame then
      begin
        MouseCursor.Cleanup;
        lpDDSFront.GetDC(DC);
        try
          SetStretchBltMode(frmMain.ScreenShot.Canvas.Handle, HALFTONE);
          StretchBlt(frmMain.ScreenShot.Canvas.Handle, 0, 0,
            frmMain.ScreenShot.width, frmMain.ScreenShot.Height, DC, 0, 0,
            frmMain.ScreenShot.width * 3, frmMain.ScreenShot.Height *
            3, SRCCOPY);
        finally
          lpDDSFront.ReleaseDC(DC);
        end;
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
var
  DC: HDC;
begin
  frmMain.Active := False;

  MouseCursor.Cleanup;
  lpDDSFront.GetDC(DC);
  try
    SetStretchBltMode(frmMain.ScreenShot.Canvas.Handle, HALFTONE);
    StretchBlt(frmMain.ScreenShot.Canvas.Handle, 0, 0,
      frmMain.ScreenShot.width, frmMain.ScreenShot.Height, DC, 0, 0,
      frmMain.ScreenShot.width * 3, frmMain.ScreenShot.Height * 3, SRCCOPY);
  finally
    lpDDSFront.ReleaseDC(DC);
  end;

  PostMessage(frmMain.Handle, WM_StartMainMenu, 0, 0); // Restart the intro
end;

class procedure TKeyEvent.SpellHotKey(key: Word);
var
  offset: Word;
begin
  offset := 0;
//  if ( key >= 114 ) and ( key < 124 ) then   // F3 - F12 spell hotkeys
//    offset := 115
//  else
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

class procedure TKeyEvent.ToggleCombat;
var
  i: Integer;
begin
  Current.CombatMode := not Current.CombatMode;
  for i := 0 to NPCList.Count - 1 do
  begin
    TCharacter(NPCList.Items[i]).CombatMode := Current.CombatMode;
    frmMain.PaintCharacterOnBorder(TSpriteObject(NPCList.Items[i]), i);
  end;
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
        BitBlt(DC, 391, 30, 202, 68, frmMain.imgBottomBar.Canvas.Handle,
          391, 30, SRCCOPY);
      finally
        frmMain.OverlayB.ReleaseDC(DC);
      end;
      DrawAlpha(frmMain.OverlayB,
        Rect(456, 53, 456 + 73 { imgPaused.width } ,
        53 + 23 { imgPaused.Height } ), Rect(0, 0, 73 { imgPaused.width } ,
        23 { imgPaused.Height } ), frmMain.PauseImage, True, 170);
      pr := Rect(0, 0, 800, 114);
      lpDDSFront.BltFast(0, 486, frmMain.OverlayB, @pr,
        DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);

      for i := 1 to NPCList.Count - 1 do
      begin
        HpDistance := TCharacter(NPCList[i]).HitPoints -
          TCharacter(NPCList[i]).Wounds;
        if HpDistance > TCharacter(NPCList[i]).HitPoints then
          HpDistance := TCharacter(NPCList[i]).HitPoints
        else if HpDistance < 0 then
          HpDistance := 0;

        ManaDistance := TCharacter(NPCList[i]).Mana -
          TCharacter(NPCList[i]).Drain;
        if ManaDistance > TCharacter(NPCList[i]).Mana then
          ManaDistance := TCharacter(NPCList[i]).Mana
        else if ManaDistance < 0 then
          ManaDistance := 0;

        HpDistance := HpDistance * (66 / TCharacter(NPCList[i]).HitPoints);
        ManaDistance := ManaDistance * (66 / TCharacter(NPCList[i]).Mana);
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

class procedure TKeyEvent.TransitBack;
begin
  if not player.titleexists('Schnellerwechselaus') then
  begin
    if player.titleexists('chapter04') then
      RunScript(player, 'Loadmap(03Wald1,default,forst,Wald|#Schnellreise.Fall3#)');
    if player.titleexists('chapter03') and not player.titleexists('chapter04') then
      RunScript(player, 'Loadmap(03Wald1,default,forst,Wald|#Schnellreise.Fall2#)');
    if player.titleexists('chapter02') then
    begin
      if not player.titleexists('chapter03') then
      begin
        if player.titleexists('ImForst') then
          RunScript(player, 'Loadmap(Wald1,default,forst,Wald|#Schnellreise.Fall1#)')
        else
          RunScript(player, 'Loadmap(southgate1b,default,Levelpoint4|#Schnellreise.Fall1#)');
      end;
    end;
    if not player.titleexists('chapter02') then
      RunScript(player, 'Loadmap(southgate1b,default,Levelpoint4|#Schnellreise.Fall1#)');
  end;
end;

class procedure TKeyEvent.TwinWeaponToggle;
begin // Z Zweischwerterstilfunktion
  if player.titleexists('geschicklichkeit1') then
  begin
    if player.titleexists('Dagger') then
    begin
      RunScript(player, 'player.additem(DaggerShield)');
      RunScript(player, 'player.removeitem(Dagger)');
    end;
    if player.titleexists('Daggershield') then
    begin
      RunScript(player, 'player.additem(Dagger)');
      RunScript(player, 'player.removeitem(DaggerShield)');
    end;
    if player.titleexists('Daggerworn') then
    begin
      RunScript(player, 'player.additem(DaggerwornShield)');
      RunScript(player, 'player.removeitem(Daggerworn)');
    end;
    if player.titleexists('Daggershieldworn') then
    begin
      RunScript(player, 'player.additem(Daggerworn)');
      RunScript(player, 'player.removeitem(DaggerwornShield)');
    end;
    if player.titleexists('DaggerFine') then
    begin
      RunScript(player, 'player.additem(DaggerFineShield)');
      RunScript(player, 'player.removeitem(DaggerFine)');
    end;
    if player.titleexists('DaggershieldFine') then
    begin
      RunScript(player, 'player.additem(DaggerFine)');
      RunScript(player, 'player.removeitem(DaggerFineShield)');
    end;
    if player.titleexists('Axe') then
    begin
      RunScript(player, 'player.additem(Axeshield)');
      RunScript(player, 'player.removeitem(Axe)');
    end;
    if player.titleexists('Axeshield') then
    begin
      RunScript(player, 'player.additem(Axe)');
      RunScript(player, 'player.removeitem(Axeshield)');
    end;
    if player.titleexists('AxeFine') then
    begin
      RunScript(player, 'player.additem(AxeFineshield)');
      RunScript(player, 'player.removeitem(AxeFine)');
    end;
    if player.titleexists('AxeshieldFine') then
    begin
      RunScript(player, 'player.additem(AxeFine)');
      RunScript(player, 'player.removeitem(AxeFineshield)');
    end;
    if player.titleexists('AxeWorn') then
    begin
      RunScript(player, 'player.additem(AxeWornshield)');
      RunScript(player, 'player.removeitem(AxeWorn)');
    end;
    if player.titleexists('AxeshieldWorn') then
    begin
      RunScript(player, 'player.additem(AxeWorn)');
      RunScript(player, 'player.removeitem(AxeWornshield)');
    end;
    if player.titleexists('Shortsword') then
    begin
      if (player.strength > 6) and (player.coordination > 11) then
      begin
        RunScript(player, 'player.additem(ShortswordShield)');
        RunScript(player, 'player.removeitem(Shortsword)');
      end;
    end;
    if player.titleexists('ShortswordShield') then
    begin
      RunScript(player, 'player.additem(Shortsword)');
      RunScript(player, 'player.removeitem(ShortswordShield)');
    end;
    if player.titleexists('Shortswordworn') then
    begin
      if (player.strength > 6) and (player.coordination > 11) then
      begin
        RunScript(player, 'player.additem(ShortswordwornShield)');
        RunScript(player, 'player.removeitem(Shortswordworn)');
      end;
    end;
    if player.titleexists('ShortswordShieldworn') then
    begin
      RunScript(player, 'player.additem(Shortswordworn)');
      RunScript(player, 'player.removeitem(ShortswordwornShield)');
    end;
  end;

  if player.titleexists('geschicklichkeit2') then
  begin
    if (player.strength > 11) and (player.coordination > 15) then
    begin
      if player.titleexists('Gladius') then
      begin
        RunScript(player, 'player.additem(GladiusswordShield)');
        RunScript(player, 'player.removeitem(Gladiussword)');
      end;
      if player.titleexists('gladiusworn') then
      begin
        RunScript(player, 'player.additem(GladiusswordwornShield)');
        RunScript(player, 'player.removeitem(Gladiusswordworn)');
      end;
      if player.titleexists('GladiusFine') then
      begin
        RunScript(player, 'player.additem(gladiusswordFineShield)');
        RunScript(player, 'player.removeitem(gladiusswordFine)');
      end;
    end;
    if player.titleexists('gladiusshield') then
    begin
      RunScript(player, 'player.additem(gladiussword)');
      RunScript(player, 'player.removeitem(gladiusswordShield)');
    end;
    if player.titleexists('gladiusshieldworn') then
    begin
      RunScript(player, 'player.additem(gladiusswordworn)');
      RunScript(player, 'player.removeitem(gladiusswordwornShield)');
    end;
    if player.titleexists('gladiusshieldFine') then
    begin
      RunScript(player, 'player.additem(gladiusswordFine)');
      RunScript(player, 'player.removeitem(gladiusswordFineShield)');
    end;
    if (player.strength > 12) and (player.coordination > 15) then
    begin
      if player.titleexists('Scimitar') then
      begin
        RunScript(player, 'player.additem(scimitarShield)');
        RunScript(player, 'player.removeitem(scimitar)');
      end;
      if player.titleexists('scimitarworn') then
      begin
        RunScript(player, 'player.additem(scimitarwornShield)');
        RunScript(player, 'player.removeitem(scimitarworn)');
      end;
      if player.titleexists('scimitarFine') then
      begin
        RunScript(player, 'player.additem(scimitarFineShield)');
        RunScript(player, 'player.removeitem(scimitarFine)');
      end;
    end;
    if player.titleexists('scimitarshield') then
    begin
      RunScript(player, 'player.additem(scimitar)');
      RunScript(player, 'player.removeitem(scimitarShield)');
    end;
    if player.titleexists('scimitarshieldworn') then
    begin
      RunScript(player, 'player.additem(scimitarworn)');
      RunScript(player, 'player.removeitem(scimitarwornShield)');
    end;
    if player.titleexists('scimitarshieldFine') then
    begin
      RunScript(player, 'player.additem(scimitarFine)');
      RunScript(player, 'player.removeitem(scimitarFineShield)');
    end;
  end;

  if player.titleexists('geschicklichkeit3') then
  begin
    if (player.strength > 14) and (player.coordination > 17) then
    begin
      if player.titleexists('Bastardsword') then
      begin
        RunScript(player, 'player.additem(BastardswordShield)');
        RunScript(player, 'player.removeitem(Bastardsword)');
      end;
      if player.titleexists('Bastardswordworn') then
      begin
        RunScript(player, 'player.additem(BastardswordwornShield)');
        RunScript(player, 'player.removeitem(Bastardswordworn)');
      end;
      if player.titleexists('BastardswordFine') then
      begin
        RunScript(player, 'player.additem(BastardswordFineShield)');
        RunScript(player, 'player.removeitem(BastardswordFine)');
      end;
    end;
    if player.titleexists('Bastardswordshield') then
    begin
      RunScript(player, 'player.additem(Bastardsword)');
      RunScript(player, 'player.removeitem(BastardswordShield)');
    end;
    if player.titleexists('Bastardswordshieldworn') then
    begin
      RunScript(player, 'player.additem(Bastardswordworn)');
      RunScript(player, 'player.removeitem(BastardswordwornShield)');
    end;
    if player.titleexists('BastardswordshieldFine') then
    begin
      RunScript(player, 'player.additem(BastardswordFine)');
      RunScript(player, 'player.removeitem(BastardswordFineShield)');
    end;
    if (player.strength > 15) and (player.coordination > 19) then
    begin
      if player.titleexists('Officersword') then
      begin
        RunScript(player, 'player.additem(OfficerswordShield)');
        RunScript(player, 'player.removeitem(Officersword)');
      end;
    end;
    if player.titleexists('Officerswordshield') then
    begin
      RunScript(player, 'player.additem(OfficerswordFine)');
      RunScript(player, 'player.removeitem(OfficerswordFineShield)');
    end;
  end;
end;

end.
