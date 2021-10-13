unit SoAOS.Intrface.Dialogs.NewCharacter;
(*
  Siege Of Avalon : Open Source Edition

  Portions created by Digital Tome L.P. Texas USA are
  Copyright ©1999-2000 Digital Tome L.P. Texas USA
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

  Description: NewPlayer dialog - was CharCreation.pas - a lot more clean-up is coming

  Notes: Beware of the offset calculation - Blt to offset coords - with src rects. Use offset rects for mousemove/down and offset coords for text.

  Requires: Delphi 10.3.3 or later

  Revision History:
  - 13 Jul 2003 - DL: Initial Upload to CVS
  - 10 Mar 2019 - SN: Forked on GitHub
  see git repo afterwards

*)

interface

uses
  // Winapi.DirectDraw,
  DirectX,

  System.Types,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,

  Character,
  Resource,
  SoAOS.Types,
  SoAOS.Animation,
  SoAOS.Intrface.Dialogs;

type
  TBoxEnum = (bxNone = -1, bxShirt = 12, bxPants = 13, bxHairColor = 14,
    bxHairStyle = 15, bxBeard = 16, bxTraining = 17);

  TCreation = class(TDialog)
  strict private // actions
    procedure selectCharLeft;
    procedure selectCharRight;
    procedure selectShirt(const val: Integer);
    procedure selectPants(const val: Integer);
    procedure selectHairColor(const val: Integer);
    procedure selectHairStyle(const val: Integer);
    procedure selectBeard(const val: Integer);
    procedure selectBase(const val: Integer); // DoA Race
    procedure selectDoAHair(const val: Integer); // DoA Hair
    procedure selectTattoo(const val: Integer); // DoA Tattoo
    function adjustStat(idx: Integer): boolean;
  private
    FOnDraw: TNotifyEvent;
    // Training modifications
    ChosenTraining: Integer;
    // Line editor stuff
    CharacterName: string; // characters name
    CaratPosition: Integer; // position in pixels
    CaratCharPosition: Integer; // position in Characters
    CaratVisible: boolean;
    // Bitmap stuff
    DXBack: IDirectDrawSurface;
    // DD surface that holds the statistics screen before blit
    DXCircle: IDirectDrawSurface; // circle used for outline
    DXRightArrow: IDirectDrawSurface;
    DXLeftArrow: IDirectDrawSurface;
    DXBox: IDirectDrawSurface;
    DXBlack: IDirectDrawSurface;
    DXContinue: IDirectDrawSurface;
    DXCancel: IDirectDrawSurface;
    DXHardmode: IDirectDrawSurface;
    InfoRect: array [0 .. 19] of TInformationRect;
    // was 35  //collision rects for information
    ArrowRect: array [0 .. 15] of TInformationRect;
    // collision rects for arrows
    StatAdjustments: array [0 .. 7] of Integer;
    // used to see if we've added points to a stat or not
    StatName: array [0 .. 1, 0 .. 11] of string;
    // base stuff - saved in case we do a cancel
    Damage: TDamageProfile;
    Resistance: TDamageResistanceProfile;
    BaseStrength: Integer;
    BaseCoordination: Integer;
    BaseConstitution: Integer;
    BasePerception: Integer;
    BaseCharm: Integer;
    BaseMysticism: Integer;
    BaseCombat: Integer;
    BaseStealth: Integer;
    TrainingPoints: Integer;
    Modifier: Integer; // Horizontal shift
    CaratTimer: TTimer;
    BoxOpen: TBoxEnum;
    LoopCounter, Spinner: Integer;
    DoAStart: boolean; // Days of Ahoul, Check if Race and Training is OK
    txtMessage: array [0 .. 105] of string;
    rLeftArrow, rRightArrow: TRect;

    Players: TStringList; // TArray<string>;
    PlayerResourceIdx: Integer;
    LayeredImage: string;

    Female: boolean;

    procedure DrawNewPlayer;
    procedure OpenBox(box: TBoxEnum);
    procedure CaratTimerEvent(Sender: TObject);
    procedure LoadBaseValues; // saves the base stats of the character
    procedure LoadNames;
    procedure CreateCollisionRects;
    // create the rects for the collision detection
    procedure ShowStats; // plots all the numbers on the screen
    // procedure DebugPlot(i: integer);
    procedure CharCreationDraw(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    function GetCancelRect: TRect;
    function GetContinueRect: TRect;

    procedure updateClothing;
    function GetPlayerININame: string;

  protected
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y, GridX, GridY: Integer); override;
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y, GridX, GridY: Integer); override;
    procedure MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState;
      X, Y, GridX, GridY: Integer); override;
    procedure KeyDown(Sender: TObject; var key: Word;
      Shift: TShiftState); override;
  private
    ixSelectedShirt: Integer; // current selected shirt color
    ixSelectedPants: Integer; // current selected pants color
    ixSelectedHair: Integer; // current selected Hair color
    ixSelectedHairStyle: Integer; // current selected Hairstyle
    ixSelectedBeard: Integer;
    ixSelectedBase: Integer; // Days of Ahoul BaseAhoul or BaseShaman
    ixSelectedTattoo: Integer; // Days of Ahoul, used instead of haircolor
    ixselectedDoAHair: Integer; // Days of Ahoul Hair or rather head
  public
    SelectedTraining: Integer;
    SelectedShirt: TItem; // current selected shirt color
    SelectedPants: TItem; // current selected pants color
    SelectedBoots: TItem; // current selected pants color
    SelectedHair: TResource; // current selected Hair color
    SelectedBase: Integer; // Days of Ahoul BaseAhoul or BaseShaman
    SelectedTattoo: TItem; // Days of Ahoul, used instead of haircolor
    AhoulRace: Integer;
    // Days of Ahoul 1 = Shaman (Mage), 2 = Ahoul (normally no mage), 3 = Halfbreed
    SelectRect: array [0 .. 20] of TSelectionRect;
    // collision rects for selectable text

    shirt: array [1 .. 4] of TItem;
    pants: array [1 .. 4] of TItem;
    hair: array [1 .. 4, 1 .. 4, 1 .. 2] of TResource;
    boots: TItem;
    DoAHair: array [1 .. 6] of TResource;
    tattoo: array [1 .. 3] of TItem; // blue,yellow, no one
    Character: TCharacter;
    Cancel: boolean; // was Cancel Pressed?
    frmMain: TForm; // we need the  form passed into handle form mouse events
    constructor Create;
    procedure Paint; override;
    procedure Init; override;
    procedure Release; override;
    property OnDraw: TNotifyEvent read FOnDraw write FOnDraw;

    property CancelRect: TRect read GetCancelRect;
    property ContinueRect: TRect read GetContinueRect;
    property PlayerININame: string read GetPlayerININame;
  end;

implementation

uses
  System.SysUtils,
  System.IOUtils,
  DXEffects,
  DFX,
  SoAOS.Graphics.Types,
  SoAOS.Graphics.Draw,
  SoAOS.Intrface.Text,
  Parts,
  Engine,
  Logfile,
  GameText,
  AniDemo;

{ TCreation }

procedure TCreation.Init;
var
  i: Integer;
  pr: TRect;
  prHarti: TRect;
  width, height: Integer;
  playpox: string;
const
  FailName: string = 'TCreation.init';
begin
  Log.DebugLog(FailName);
  try

    if Loaded then
      Exit;
    inherited;
    Spinner := 0;
    FOnDraw := CharCreationDraw;

    ExText.Open('CharCreation');
    for i := 0 to 105 do
      txtMessage[i] := ExText.GetText('Message' + inttostr(i));

    ChosenTraining := -1; // initialize training to nothing
    // Set mouse events for form
    frmMain.OnMouseDown := FormMouseDown;
    frmMain.OnMouseMove := FormMouseMove;

    Cancel := false;
    BoxOpen := bxNone;
    CaratTimer := TTimer.Create(nil);
    CaratTimer.onTimer := CaratTimerEvent;
    // CaratTimer.TimerPriority:=tpNormal;
    CaratTimer.Interval := 100;
    // CaratTimer.resolution := 1;
    LoopCounter := 0;
    CaratTimer.enabled := True;
    CaratPosition := 0;
    CaratCharPosition := 0;
    CaratVisible := True;
    CharacterName := '';
    Modifier := 270;
    // offset to move all the stats info- makes it easier to adjust
    if modselection <> TModSelection.DoA then // Not Days of Ahoul
    begin
      ixSelectedShirt := 0;
      ixSelectedPants := 0; // 4
      ixSelectedHair := 0; // 8
      ixSelectedHairStyle := 0; // 12
      AhoulRace := 0; // Zero, since no DoA (just in case)
    end
    else
    begin
      ixSelectedBase := 2;
      // 0 = BaseShaman, 1 = BaseAhoul, 2 = Mischling (=BaseShaman)
      ixSelectedPants := 0; //
      ixSelectedTattoo := 0;
      // 0 oder 8 = blue, 1 oder 9 = yellow, 2 oder 10 = no one
      ixselectedDoAHair := 0; // 13
      AhoulRace := 3; // =ixselectedbase := 2
    end;
    ixSelectedBeard := 1;
    if modselection = TModSelection.Caves then // Caves - Mage
      SelectedTraining := 2
    else
      SelectedTraining := 0;
    Hardmode := false;
    if UseSmallFont then
      pText.LoadGoldFontGraphic;
    pText.LoadFontGraphic('CreateChar'); // load the statisctics font graphic in
    LoadNames;
    LoadBaseValues;

    Players := TStringList.Create;
    for playpox in TDirectory.GetFiles(ResourcePath + 'players', '*.pox') do
    begin
      if ExtractFileName(playpox) = 'PlayerOgre.pox' then
        continue;
      if FileExists(ResourcePath + 'Engine\LayeredImages\' + NakedName(playpox))
      then
        Players.Add('players\' + ExtractFileName(playpox))
    end;
    PlayerResourceIdx := Players.IndexOf(TCharacterResource(Character.Resource)
      .Filename);
    if PlayerResourceIdx = -1 then // non-selected or selectable
      PlayerResourceIdx := 0; // first

    if Players.Count > 1 then
    begin
      rLeftArrow := Rect(100, 140, 115, 170);
      rRightArrow := Rect(260, 140, 275, 170);
    end
    else
    begin
      rLeftArrow := Rect(-100, 140, -115, 170);
      rRightArrow := Rect(-260, 140, -275, 170);
    end;
    // Load the Background Bitmap and plot it
    DXHardmode := SoAOS_DX_LoadBMP(InterfacePath + 'opyellow.bmp', cInvisColor);
    DXCircle := SoAOS_DX_LoadBMP(InterfacePath + 'chaRedOval.bmp', cInvisColor);
    DXBlack := SoAOS_DX_LoadBMP(InterfacePath + 'chaBlack.bmp', cInvisColor);
    DXBox := SoAOS_DX_LoadBMP(InterfaceLanguagePath + 'chaChooseBox.bmp',
      cInvisColor);

    DXContinue := SoAOS_DX_LoadBMP(InterfaceLanguagePath + 'chaContinue.bmp',
      cInvisColor);
    DXCancel := SoAOS_DX_LoadBMP(InterfaceLanguagePath + 'chaCancel.bmp',
      cInvisColor, width, height);

    DXBack := SoAOS_DX_LoadBMP(InterfaceLanguagePath + 'CharCreate.bmp',
      cInvisColor, DlgWidth, DlgHeight);
    if ScreenMetrics.borderFile <> '' then
    // Neu hinpinseln, da z.B. DoA ein grünes Menü hat
      lpDDSBack.BltFast(0, 0, TfrmMain(frmMain).FillBorder, nil,
        DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
    if (Players.Count > 1) and (modselection = TModSelection.SoA) then // zur Sicherheit
    begin
      pr := Rect(0, 0, 15, 30);
      DXLeftArrow := SoAOS_DX_LoadBMPResource('chaLeftArrow', cInvisColor);
      DXBack.BltFast(rLeftArrow.Left, rLeftArrow.Top, DXLeftArrow, @pr,
        DDBLTFAST_WAIT);
      DXLeftArrow := nil;

      DXRightArrow := SoAOS_DX_LoadBMPResource('chaRightArrow', cInvisColor);
      DXBack.BltFast(rRightArrow.Left, rRightArrow.Top, DXRightArrow, @pr,
        DDBLTFAST_WAIT);
      DXRightArrow := nil;

      rLeftArrow.Offset(Offset);
      rRightArrow.Offset(Offset);
    end;

    pr := Rect(0, 0, DlgWidth, DlgHeight);
    lpDDSBack.BltFast(Offset.X, Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or
      DDBLTFAST_WAIT);
    // Hardmode
    prHarti := Rect(0, 0, 12, 12);
    lpDDSBack.BltFast(Offset.X + 450, Offset.Y + 35, DXHardmode, @prHarti,
      DDBLTFAST_WAIT);
    CreateCollisionRects; // Must be called after "Offset" is ready

    ShowStats;

    Player.Resource := LoadArtResource(ChangeFileExt(Players[PlayerResourceIdx],
      '.gif'));
    Player.LoadEquipment(True);

    updateClothing;

    DrawNewPlayer;

  except
    on E: Exception do
      Log.Log(FailName + E.Message);
  end;

end; // TCreation.Init;

procedure TCreation.Release;
const
  FailName: string = 'TCreation.Release';
begin
  Log.DebugLog(FailName);
  try

    pText.UnLoadGoldFontGraphic;

    ExText.close;
    DXHardmode := nil;
    DXBox := nil;
    DXBlack := nil;
    DXContinue := nil;
    DXCancel := nil;
    DXBack := nil;
    DXCircle := nil;
    DXLeftArrow := nil;
    DXRightArrow := nil;
    CaratTimer.enabled := false;
    CaratTimer.free;
    CaratTimer := nil;

    Players.free;

    inherited;
  except
    on E: Exception do
      Log.Log(FailName + E.Message);
  end;

end;

procedure TCreation.selectBeard(const val: Integer);
begin
  ixSelectedBeard := val - 16;
  DrawAlpha(DXBack, Rect(113, 406, 261, 434), Rect(0, 0, 25, 25), DXBlack,
    false, 255);
  if val = 16 then
    pText.PlotTextCentered2(DXBack, txtMessage[3], 113, 261, 409, 250)
  else
    pText.PlotTextCentered2(DXBack, txtMessage[4], 113, 261, 409, 250);
end;

procedure TCreation.selectCharLeft;
begin
  if PlayerResourceIdx = 0 then
    PlayerResourceIdx := Players.Count - 1
  else
    Dec(PlayerResourceIdx);
  Player.Resource := LoadArtResource(ChangeFileExt(Players[PlayerResourceIdx],
    '.gif'));
  Player.LoadEquipment(True);
  updateClothing;
end;

procedure TCreation.selectCharRight;
begin
  if PlayerResourceIdx = Players.Count - 1 then
    PlayerResourceIdx := 0
  else
    Inc(PlayerResourceIdx);
  Player.Resource := LoadArtResource(ChangeFileExt(Players[PlayerResourceIdx],
    '.gif'));
  Player.LoadEquipment(True);
  updateClothing;
end;

procedure TCreation.selectHairColor(const val: Integer);
begin
  ixSelectedHair := val - 8;
  DrawAlpha(DXBack, Rect(113, 321, 261, 348), Rect(0, 0, 25, 25), DXBlack,
    false, 255);
  pText.PlotTextCentered2(DXBack, SelectRect[val].Text + txtMessage[2], 113,
    261, 324, 250)
end;

procedure TCreation.selectHairStyle(const val: Integer);
begin
  ixSelectedHairStyle := val - 12;
  DrawAlpha(DXBack, Rect(113, 363, 261, 391), Rect(0, 0, 25, 25), DXBlack,
    false, 255);
  if val < 14 then
    pText.PlotTextCentered2(DXBack, SelectRect[val].Text + txtMessage[2], 113,
      261, 366, 250)
  else
    pText.PlotTextCentered2(DXBack, SelectRect[val].Text, 113, 261, 366, 250);
end;

procedure TCreation.selectPants(const val: Integer);
begin
  ixSelectedPants := val - 4;
  DrawAlpha(DXBack, Rect(113, 278, 261, 306), Rect(0, 0, 25, 25), DXBlack,
    false, 255);
  pText.PlotTextCentered2(DXBack, SelectRect[val].Text + txtMessage[1], 113,
    261, 281, 250);
end;

procedure TCreation.selectShirt(const val: Integer);
begin
  ixSelectedShirt := val;
  DrawAlpha(DXBack, Rect(113, 236, 261, 264), Rect(0, 0, 25, 25), DXBlack,
    false, 255);
  pText.PlotTextCentered2(DXBack, SelectRect[val].Text + txtMessage[0], 113,
    261, 239, 250)
end;

procedure TCreation.selectBase(const val: Integer); // Doa, =selected shirt
begin
  ixSelectedBase := val; // AhoulRace but 1-3 and not 0-2
  if ixselectedDoAHair = 0 then // Safety, clear Box
  begin
    DrawAlpha(DXBack, Rect(113, 363, 261, 391), Rect(0, 0, 25, 25), DXBlack,
      false, 255);
    pText.PlotTextCentered2(DXBack, SelectRect[15].Text, 113, 261, 366, 250);
    // no [ val ], instead calue for Bald = 15, SelectRect... matches luckily
  end;
  // Clear selected Training
  DrawAlpha(DXBack, Rect(300, 132, 448, 160), Rect(0, 0, 25, 25), DXBlack,
    false, 255);
  DrawAlpha(DXBack, Rect(113, 236, 261, 264), Rect(0, 0, 25, 25), DXBlack,
    false, 255);
  pText.PlotTextCentered2(DXBack, txtMessage[val + 16], 113, 261, 239, 250);
  // pText.PlotTextCentered2( DXBack, SelectRect[ val ].Text, 113, 261, 239, 250 );
  // log.log( 'Val=' + inttostr(val));
  // pText.PlotTextCentered2( DXBack, txtMessage[ val + 16 ], 113, 261, 239, 250 );
  // txtmessage 16, 17, 18
end;

procedure TCreation.selectTattoo(const val: Integer);
begin
  ixSelectedTattoo := val - 8;
  DrawAlpha(DXBack, Rect(113, 321, 261, 348), Rect(0, 0, 25, 25), DXBlack,
    false, 255);
  if ixSelectedTattoo = 0 then
    pText.PlotTextCentered2(DXBack, txtMessage[15] + txtMessage[0], 113, 261,
      324, 250)
  else if ixSelectedTattoo = 1 then
    pText.PlotTextCentered2(DXBack, txtMessage[14] + txtMessage[0], 113, 261,
      324, 250)
  else
  begin
    if language = 'german' then
      pText.PlotTextCentered2(DXBack, 'Kein' + txtMessage[0], 113,
        261, 324, 250)
    else
      pText.PlotTextCentered2(DXBack, 'No' + txtMessage[0], 113, 261, 324, 250);
  end;
  // pText.PlotTextCentered2( DXBack, SelectRect[ val ].Text + txtMessage[ 0 ], 113, 261, 324, 250 );
end;

procedure TCreation.selectDoAHair(const val: Integer);
begin
  ixselectedDoAHair := val - 12;
  DrawAlpha(DXBack, Rect(113, 363, 261, 391), Rect(0, 0, 25, 25), DXBlack,
    false, 255);
  if ixselectedDoAHair = 0 then // war =12
  begin
    if AhoulRace > 1 then // No Shaman, Orkhead possible
    begin
      pText.PlotTextCentered2(DXBack, txtMessage[81], 113, 261, 366, 250);
    end
    else // Long Shaman hair possible
    begin
      pText.PlotTextCentered2(DXBack, SelectRect[val].Text, 113, 261, 366, 250);
    end;
  end
  else if ixselectedDoAHair = 1 then // war =13
    pText.PlotTextCentered2(DXBack, SelectRect[val].Text + txtMessage[3], 113,
      261, 366, 250)
  else if ixselectedDoAHair > 1 then // war >13
    pText.PlotTextCentered2(DXBack, SelectRect[val].Text, 113, 261, 366, 250);
end;
// TCreation.Release

procedure TCreation.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y, GridX, GridY: Integer);
var
  i: Integer;
  bx: TBoxEnum;
  BoxWasOpened: boolean;
  BoxClosed: boolean;
  r1, r2, r, pr: TRect;
  prHarti, prUnHarti1, prUnHarti2: TRect;
const
  FailName: string = 'TCreation.MouseDown';
begin
  Log.DebugLog(FailName);
  try

    BoxClosed := false;
    // Hardmode
    prHarti := Rect(0, 0, 12, 12);
    prUnHarti1 := Rect(450, 35, 462, 47);
    prUnHarti2 := Rect(410, 35, 422, 47);
    if ptInRect(ApplyOffset(Rect(410, 35, 422, 47)), point(X, Y)) then
    begin
      Hardmode := True;
      DrawAlpha(lpDDSBack, ApplyOffset(Rect(410, 35, 422, 47)),
        Rect(0, 0, 12, 12), DXHardmode, True, 255);
      lpDDSBack.BltFast(450 + Offset.X, 35 + Offset.Y, DXBack, @prUnHarti1,
        DDBLTFAST_WAIT);
    end;
    if ptInRect(ApplyOffset(Rect(450, 35, 462, 47)), point(X, Y)) then
    begin
      Hardmode := false;
      DrawAlpha(lpDDSBack, ApplyOffset(Rect(450, 35, 462, 47)),
        Rect(0, 0, 12, 12), DXHardmode, false, 255);
      lpDDSBack.BltFast(410 + Offset.X, 35 + Offset.Y, DXBack, @prUnHarti2,
        DDBLTFAST_WAIT);
    end;
    // Ende Hardmode
    if rLeftArrow.Contains(point(X, Y)) then
      selectCharLeft
    else if rRightArrow.Contains(point(X, Y)) then
      selectCharRight
    else if BoxOpen = bxNone then
    begin
      for i := 0 to 15 do
        if ArrowRect[i].Contains(point(X, Y)) then
        begin
          if adjustStat(i) then
            Paint;
          break;
        end;
    end;

    r1 := ApplyOffset(Rect(465, 59, 465 + 123, 59 + 181));
    r2 := ApplyOffset(Rect(279, 239 + (ord(BoxOpen) - 12) * 42, 279 + 123,
      239 + (ord(BoxOpen) - 12) * 42 + 181));
    // check for click in box
    if (r1.Contains(point(X, Y)) or r2.Contains(point(X, Y))) and
      (BoxOpen > bxNone) then
    begin // check to see if box is open
      i := 0;
      while i < 21 do
      begin
        if SelectRect[i].Contains(point(X, Y)) then
        begin // if over an item
          // set the selected for each type based on which text the user hit
          if i < 4 then
            if modselection <> TModSelection.DoA then
            begin
              selectShirt(i);
            end
            else // DoA - Rasse
            begin
              if i = 0 then
              begin
                Player.Resource :=
                  LoadArtResource(ChangeFileExt('players\player.pox', '.gif'));
                selectBase(i);
                AhoulRace := 1;
                DoAStart := false; // New Training to choose
                if ixselectedDoAHair = 0 then // Safety reason
                  ixselectedDoAHair := 3;
              end
              else if i = 1 then
              begin
                Player.Resource :=
                  LoadArtResource(ChangeFileExt('players\playerahoul.pox',
                  '.gif'));
                selectBase(i);
                AhoulRace := 2;
                DoAStart := false; // New Training to choose
                if ixselectedDoAHair = 0 then
                // Safety reason -Orkhead or Shamanlonghair only for each Race
                  ixselectedDoAHair := 3;
              end
              else if i = 2 then
              begin
                Player.Resource :=
                  LoadArtResource(ChangeFileExt('players\player.pox', '.gif'));
                selectBase(i);
                AhoulRace := 3;
                DoAStart := false; // New Training to choose
                if ixselectedDoAHair = 0 then // Safety reason
                  ixselectedDoAHair := 3;
              end;
            end
          else if i < 8 then
            if modselection <> TModSelection.DoA then
            begin
              selectPants(i);
            end
            else // DoA - only 2 pants
            begin
              if i = 4 then
                selectPants(4);
              if i = 5 then
                selectPants(5);
            end
          else if i < 12 then
            if modselection <> TModSelection.DoA then
            begin
              selectHairColor(i);
            end
            else // DoA - Tattoo
            begin
              if i = 8 then
              begin
                selectTattoo(i);
                // ixSelectedTattoo := 8;
              end
              else if i = 9 then
              begin
                selectTattoo(i);
              end
              else if i = 10 then
              begin
                selectTattoo(i);
              end;
            end
          else if i < 16 then
            if modselection <> TModSelection.DoA then
            begin
              selectHairStyle(i);
            end
            else // DoA - Hair
            begin
              selectDoAHair(i);
            end
          else if i < 18 then
            if modselection <> TModSelection.DoA then
            begin
              selectBeard(i);
            end
            else // DoA - No special Beard selectable
            begin
              Exit
            end
          else
          begin
            if ChosenTraining = 0 then
            begin
              if modselection = TModSelection.AoA then
                Character.Strength := Character.BaseStrength - 4
              else
                Character.Strength := Character.BaseStrength - 5;
              Character.Coordination := Character.BaseCoordination - 2;
              if modselection = TModSelection.AoA then
                Character.Constitution := Character.BaseConstitution - 2
              else
                Character.Constitution := Character.BaseConstitution - 3;
              Character.Perception := Character.BasePerception + 3;
              if modselection = TModSelection.AoA then
                Character.Charm := Character.BaseCharm + 1
              else
                Character.Charm := Character.BaseCharm + 3;
              Character.Mysticism := Character.BaseMysticism + 3;
              Character.Combat := Character.BaseCombat - 10;
              Character.Stealth := Character.BaseStealth - 0;
              if modselection = TModSelection.DoA then
                DoAStart := True;
            end
            else if ChosenTraining = 1 then
            begin
              Character.Strength := Character.BaseStrength - 2;
              Character.Coordination := Character.BaseCoordination - 5;
              Character.Constitution := Character.BaseConstitution - 0;
              Character.Perception := Character.BasePerception - 0;
              Character.Charm := Character.BaseCharm + 3;
              Character.Mysticism := Character.BaseMysticism + 3;
              Character.Combat := Character.BaseCombat - 0;
              Character.Stealth := Character.BaseStealth - 10;
              if modselection = TModSelection.DoA then
                DoAStart := True;
            end
            else if ChosenTraining = 2 then
            begin
              Character.Strength := Character.BaseStrength - 0;
              Character.Coordination := Character.BaseCoordination - 3;
              Character.Constitution := Character.BaseConstitution - 2;
              Character.Perception := Character.BasePerception - 2;
              Character.Charm := Character.BaseCharm + 3;
              Character.Mysticism := Character.BaseMysticism - 10;
              Character.Combat := Character.BaseCombat - 0;
              Character.Stealth := Character.BaseStealth + 3;
              if modselection = TModSelection.DoA then
                DoAStart := True;
            end;
            SelectedTraining := i - 18;
            ChosenTraining := i - 18;
            // why do this twice? Because SelectedTraining must be initalized for drawing the select box,
            // yet we must know if the picked a class or not- we dont let them leave without selecting a class.
            // This is a change, so a bit kludgy, but it's the 11th hour here at Digital Tome 6/11/00
            r := ApplyOffset(Rect(300, 132, 448, 160));
            DrawAlpha(lpDDSBack, r, Rect(0, 0, 25, 25), DXBlack, false, 255);
            if i = 18 then
            begin
              PlotTextCentered(lpDDSBack, txtMessage[5], 300, 448, 135, 250,
                UseSmallFont);
              if modselection = TModSelection.AoA then
                Character.Strength := Character.BaseStrength + 4
              else
                Character.Strength := Character.BaseStrength + 5;
              Character.Coordination := Character.BaseCoordination + 2;
              if modselection = TModSelection.AoA then
                Character.Constitution := Character.BaseConstitution + 2
              else
                Character.Constitution := Character.BaseConstitution + 3;
              Character.Perception := Character.BasePerception - 3;
              if modselection = TModSelection.AoA then
                Character.Charm := Character.BaseCharm - 1
              else
                Character.Charm := Character.BaseCharm - 3;
              Character.Mysticism := Character.BaseMysticism - 3;
              Character.Combat := Character.BaseCombat + 10;
              Character.Stealth := Character.BaseStealth + 0;
              if modselection = TModSelection.DoA then
                DoAStart := True;
            end
            else if i = 19 then
            begin
              PlotTextCentered(lpDDSBack, txtMessage[6], 300, 448, 135, 250,
                UseSmallFont);
              Character.Strength := Character.BaseStrength + 2;
              Character.Coordination := Character.BaseCoordination + 5;
              Character.Constitution := Character.BaseConstitution + 0;
              Character.Perception := Character.BasePerception + 0;
              Character.Charm := Character.BaseCharm - 3;
              Character.Mysticism := Character.BaseMysticism - 3;
              Character.Combat := Character.BaseCombat + 0;
              Character.Stealth := Character.BaseStealth + 10;
              if modselection = TModSelection.DoA then
                DoAStart := True;
            end
            else if i = 20 then
            begin
              PlotTextCentered(lpDDSBack, txtMessage[7], 300, 448, 135, 250,
                UseSmallFont);
              Character.Strength := Character.BaseStrength + 0;
              Character.Coordination := Character.BaseCoordination + 3;
              Character.Constitution := Character.BaseConstitution + 2;
              Character.Perception := Character.BasePerception + 2;
              Character.Charm := Character.BaseCharm - 3;
              Character.Mysticism := Character.BaseMysticism + 10;
              Character.Combat := Character.BaseCombat + 0;
              Character.Stealth := Character.BaseStealth - 3;
              if modselection = TModSelection.DoA then
                DoAStart := True;
            end;
            Paint;
          end;
          i := 900; // drop out of the loop
        end;
        i := i + 1;
      end; // wend

      r1 := ApplyOffset(Rect(465, 59, 465 + 123, 59 + 181));
      r2 := ApplyOffset(Rect(279, 239 + (ord(BoxOpen) - 12) * 42, 279 + 123,
        239 + (ord(BoxOpen) - 12) * 42 + 181));
      if i = 901 then
      begin // reopen and refresh new selection
        OpenBox(BoxOpen);
        DrawNewPlayer;
      end // check to see if the hit the ok button   //CHECK
      else if (r1.Contains(point(X, Y)) and (Y > 59 + 141)) or
        (r2.Contains(point(X, Y)) and (Y > 239 + (ord(BoxOpen) - 12) * 42 + 141))
      then
      begin
        // clean up any dimmed text
        pr := Rect(114, 237, 261, 439);
        lpDDSBack.BltFast(pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr,
          DDBLTFAST_WAIT);
        // clean up after old boxes
        pr := Rect(279, 239, 402, 588);
        lpDDSBack.BltFast(pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr,
          DDBLTFAST_WAIT);
        pr := Rect(465, 59, 465 + 123, 59 + 181);
        lpDDSBack.BltFast(pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr,
          DDBLTFAST_WAIT);
        BoxOpen := bxNone;
        BoxClosed := True;
      end;
    end
    else
    begin // see if we're opening the box
      BoxWasOpened := false;
      for bx := bxShirt to bxTraining do
      begin
        if InfoRect[ord(bx)].Contains(point(X, Y)) then
        begin
          OpenBox(bx);
          BoxWasOpened := True;
        end;
      end;
      if (BoxWasOpened = false) and (BoxOpen > bxNone) then
      begin // close the box -they clicked outside
        // clean up any dimmed text
        pr := Rect(114, 237, 261, 439);
        lpDDSBack.BltFast(pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr,
          DDBLTFAST_WAIT);
        // clean up after old boxes
        pr := Rect(279, 239, 402, 588);
        lpDDSBack.BltFast(pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr,
          DDBLTFAST_WAIT);
        pr := Rect(465, 59, 465 + 123, 59 + 181);
        lpDDSBack.BltFast(pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr,
          DDBLTFAST_WAIT);
        BoxOpen := bxNone;
        BoxClosed := True;
      end; // endif

    end;
    // else begin//we arent over anything else- check back button
    if (BoxOpen = bxNone) and not BoxClosed then
    begin
      if ContinueRect.Contains(point(X, Y)) then
      begin // over continue
        // The new data is already saved- if we ever write a Cancel function then we can restore values
        // Exit the screen if the length of name is 1 or greater
        if (Length(Trim(CharacterName)) > 0) and (ChosenTraining > -1) then
          if modselection <> TModSelection.DoA then // Not DoA
          begin
            Character.Name := CharacterName;
            close;
          end
          else // DoA change of Race needs new selected Training
          begin
            if not DoAStart then
              PlotTextBlock(txtMessage[4], 500, 682, 239, 240,
                UseSmallFont, True)
            else
            begin
              Character.Name := CharacterName;
              close;
            end;
          end
        else
        begin // Hasnt entered name- tell player to enter name or pick training
          if BoxOpen = bxTraining then
          begin
            pr := Rect(490, 239, 720, 500); // 682, 430
            lpDDSBack.BltFast(pr.Left + Offset.X, pr.Top + Offset.Y, DXBack,
              @pr, DDBLTFAST_WAIT);
            if (ChosenTraining > -1) then
              PlotTextBlock(txtMessage[8], 500, 682, 239, 240,
                UseSmallFont, True)
            else
              PlotTextBlock(txtMessage[9], 500, 682, 239, 240,
                UseSmallFont, True);
          end
          else
          begin
            pr := Rect(490, 160, 720, 500);
            lpDDSBack.BltFast(pr.Left + Offset.X, pr.Top + Offset.Y, DXBack,
              @pr, DDBLTFAST_WAIT);
            if (ChosenTraining > -1) then
              PlotTextBlock(txtMessage[10], 500, 682, 165, 240,
                UseSmallFont, True)
            else
              PlotTextBlock(txtMessage[11], 500, 682, 165, 240,
                UseSmallFont, True);
          end;
        end;
      end
      else if CancelRect.Contains(point(X, Y)) then
      begin // over cancel
        Cancel := True;
        close;
      end;
    end;

  except
    on E: Exception do
      Log.Log(FailName + E.Message);
  end;

end; // TCreation.MouseDown

procedure TCreation.MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y, GridX, GridY: Integer);
var
  i: Integer;
  pr: TRect;
  PlayerInfo: string;
const
  FailName: string = 'TCreation.MouseMove';
begin
  Log.DebugLog(FailName);
  try
    // Clean up continue and cancel
    pr := DlgRect.dlgNewContinueRect;
    lpDDSBack.BltFast(ContinueRect.Left, ContinueRect.Top, DXBack, @pr,
      DDBLTFAST_WAIT);
    if BoxOpen = bxNone then
    begin
      pr := DlgRect.dlgNewCancelRect;
      lpDDSBack.BltFast(CancelRect.Left, CancelRect.Top, DXBack, @pr,
        DDBLTFAST_WAIT);
    end;
    // clear text
    if ContinueRect.Contains(point(X, Y)) then
    begin // over continue
      // dont clear if over continue, we might have the you must enter name text up -kludgy
    end
    else
    begin

      if BoxOpen = bxTraining then
      begin
        pr := Rect(490, 239, 720, 500);
        lpDDSBack.BltFast(pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr,
          DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
      end
      else
      begin
        pr := Rect(490, 160, 720, 500);
        lpDDSBack.BltFast(pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr,
          DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
      end;
    end;

    if rLeftArrow.Contains(point(X, Y)) then
    begin
      if PlayerResourceIdx = 0 then
        PlayerInfo := txtMessage[105] +
          ChangeFileExt(ExtractFileName(Players[Players.Count - 1]), '')
      else
        PlayerInfo := txtMessage[105] +
          ChangeFileExt(ExtractFileName(Players[PlayerResourceIdx - 1]), '');

      if BoxOpen <> bxTraining then
        PlotTextBlock(PlayerInfo, 500, 680, 165, 240, UseSmallFont, True)
      else // Training box in the way - so print below
        PlotTextBlock(PlayerInfo, 500, 680, 239, 240, UseSmallFont, True);
    end;

    if rRightArrow.Contains(point(X, Y)) then
    begin
      if PlayerResourceIdx = (Players.Count - 1) then
        PlayerInfo := txtMessage[105] +
          ChangeFileExt(ExtractFileName(Players[0]), '')
      else
        PlayerInfo := txtMessage[105] +
          ChangeFileExt(ExtractFileName(Players[PlayerResourceIdx + 1]), '');

      if BoxOpen <> bxTraining then
        PlotTextBlock(PlayerInfo, 500, 680, 165, 240, UseSmallFont, True)
      else // Training box in the way - so print below
        PlotTextBlock(PlayerInfo, 500, 680, 239, 240, UseSmallFont, True);
    end;

    i := 0;
    if BoxOpen > bxNone then
    begin
      i := 0;
      while i < 21 do
      begin
        if SelectRect[i].Contains(point(X, Y)) then
        begin // if over an item
          if BoxOpen <> bxTraining then // little kludge here
            PlotTextBlock(SelectRect[i].Info, 500, 682, 165, 240, UseSmallFont,
              True) // Plot the info
          else
            PlotTextBlock(SelectRect[i].Info, 500, 682, 239, 240, UseSmallFont,
              True); // Plot the info
          i := 900; // drop out of the loop
        end;
        i := i + 1;
      end; // wend
      if (BoxOpen < bxTraining) and
        ApplyOffset(Rect(279, 239 + (ord(BoxOpen) - 12) * 42, 279 + 131,
        239 + (ord(BoxOpen) - 12) * 42 + 181)).Contains(point(X, Y)) then
        i := 901; // dont show any hot text under this box
    end; // endif boxopen

    if i <> 901 then
    begin
      i := 0;
      while i < 16 do
      begin
        if ArrowRect[i].Contains(point(X, Y)) then
        begin // if over an Arrow
          if BoxOpen <> bxTraining then // little kludge here
            PlotTextBlock(ArrowRect[i].Info, 500, 682, 165, 240, UseSmallFont,
              True) // Plot the info
          else
            PlotTextBlock(ArrowRect[i].Info, 500, 682, 239, 240, UseSmallFont,
              True); // Plot the info

          i := 900; // drop out of the loop
        end;
        i := i + 1;
      end; // wend
    end; // endif i <> 901

    if i < 900 then
    begin // if we aren't over an arrow check all other hot spots
      i := 0;
      while i < 18 do
      begin
        if InfoRect[i].Contains(point(X, Y)) then
        begin // if over an item
          if BoxOpen <> bxTraining then // little kludge here
            PlotTextBlock(InfoRect[i].Info, 500, 682, 165, 240, UseSmallFont,
              True) // Plot the info
          else
            PlotTextBlock(InfoRect[i].Info, 500, 682, 239, 240, UseSmallFont,
              True); // Plot the info

          i := 900; // drop out of the loop
        end;
        i := i + 1;
      end; // wend
    end; // endif i < 900

    if (i <> 901) and (BoxOpen = bxNone) then
    begin // we arent over anything else- check back button
      if ContinueRect.Contains(point(X, Y)) then
      begin // over Continue
        // plot highlighted Continue
        pr := Rect(0, 0, ContinueRect.width, ContinueRect.height);
        lpDDSBack.BltFast(ContinueRect.Left, ContinueRect.Top, DXContinue, @pr,
          DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
      end
      else if CancelRect.Contains(point(X, Y)) then
      begin // over cancel
        // plot highlighted cancel
        pr := Rect(0, 0, CancelRect.width, CancelRect.height);
        lpDDSBack.BltFast(CancelRect.Left, CancelRect.Top, DXCancel, @pr,
          DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
      end;

    end;
    MouseCursor.PlotDirty := false;
  except
    on E: Exception do
      Log.Log(FailName + E.Message);
  end;

end; // TCreation.MouseMove

procedure TCreation.MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y, GridX, GridY: Integer);
const
  FailName: string = 'TCreation.MouseUp';
begin
  Log.DebugLog(FailName);
  try
  except
    on E: Exception do
      Log.Log(FailName + E.Message);
  end;

end;

procedure TCreation.Paint;
const
  FailName: string = 'TCreation.Paint';
var
  pr: TRect;
begin
  Log.DebugLog(FailName);
  try

    // clear the back down to the text - but dont clear the info block
    pr := Rect(370, 210, 461, 435);
    lpDDSBack.BltFast(pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr,
      DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
    // replot the entire screen statistics
    ShowStats;
    PlotText(CharacterName, 310, 95, 240);
    SoAOS_DX_BltFront;
  except
    on E: Exception do
      Log.Log(FailName + E.Message);
  end;

end;

// TCreation.Paint;

procedure TCreation.CharCreationDraw(Sender: TObject);
var
  ddsd: TDDSurfaceDesc;
  Bits: BITPLANE;
  r: TRect;
  Frame: Integer;
const
  FailName: string = 'Main.CharCreationDraw';
begin
  FillChar(ddsd, sizeof(ddsd), 0);
  // Drawing Actor/Player
  Log.DebugLog(FailName);
  try

    Frame := Spinner div 70 + 20;
    ddsd.dwSize := sizeof(ddsd);
    r := ApplyOffset(Rect(114, 93, 262, 223));
    if lpDDSBack.Lock(@r, ddsd, DDLOCK_WAIT, 0) = DD_OK then
    begin
      try
        Bits.bitsPtr := ddsd.lpSurface;
        Bits.bitsWdh := ResWidth;
        Bits.bitsHgh := ResHeight;
        Bits.bitsFmt := dfx_pixelformat;
        Bits.bitsPitch := ddsd.lPitch;
        Bits.BaseX := 0;
        Bits.BaseY := 0;

        TCharacterResource(Player.Resource).NakedResource.RLE.Draw(Frame, 0,
          0, @Bits);
        if TCharacterResource(Player.Resource).NakedName <> 'HumanMaleLayers\BaseSkeleton.gif'
        then
        begin
          if Assigned(TCreation(Sender).SelectedPants) and
            Assigned(TCreation(Sender).SelectedPants.Resource) and
            Assigned(TLayerResource(TCreation(Sender)
            .SelectedPants.Resource).RLE) then
            TLayerResource(TCreation(Sender).SelectedPants.Resource)
              .RLE.Draw(Frame, 0, 0, @Bits);
          if modselection = TModSelection.DoA then // Days of Ahoul - Tattoo
            if Assigned(TCreation(Sender).SelectedTattoo) and
              Assigned(TCreation(Sender).SelectedTattoo.Resource) and
              Assigned(TLayerResource(TCreation(Sender).SelectedTattoo.Resource)
              .RLE) then
              TLayerResource(TCreation(Sender).SelectedTattoo.Resource)
                .RLE.Draw(Frame, 0, 0, @Bits);
          if modselection = TModSelection.SoA then // SoA Stiefel für male und female
          begin
            if Assigned(TCreation(Sender).SelectedBoots) and
              Assigned(TCreation(Sender).SelectedBoots.Resource) and
              Assigned(TLayerResource(Player.Equipment[slBoot].Resource).RLE)
            then
              TLayerResource(TCreation(Sender).SelectedBoots.Resource)
                .RLE.Draw(Frame, 0, 0, @Bits)
          end
          else // alle anderen haben festgelegte Stiefel in player.pox, bzw. kein female
          begin
            if Assigned(Player.Equipment[slBoot]) and
              Assigned(Player.Equipment[slBoot].Resource) and
              Assigned(TLayerResource(Player.Equipment[slBoot].Resource).RLE)
            then
              TLayerResource(Player.Equipment[slBoot].Resource)
                .RLE.Draw(Frame, 0, 0, @Bits);
          end;
          if modselection <> TModSelection.DoA then // in Days of Ahoul -> Keine Tunika
            if Assigned(TCreation(Sender).SelectedShirt) and
              Assigned(TCreation(Sender).SelectedShirt.Resource) and
              Assigned(TLayerResource(TCreation(Sender)
              .SelectedShirt.Resource).RLE) then
              TLayerResource(TCreation(Sender).SelectedShirt.Resource)
                .RLE.Draw(Frame, 0, 0, @Bits);
          if Assigned(TCreation(Sender).SelectedHair) and
            Assigned(TLayerResource(TCreation(Sender).SelectedHair).RLE) then
            TLayerResource(TCreation(Sender).SelectedHair)
              .RLE.Draw(Frame, 0, 0, @Bits);
        end;
      finally
        lpDDSBack.Unlock(nil);
      end;
    end;
    Inc(Spinner);
    if Spinner >= 280 then
      Spinner := 0;

  except
    on E: Exception do
      Log.Log(FailName, E.Message, []);
  end;
end;

constructor TCreation.Create;
begin
  // Define all rects - both info (move) and action (down)

end;

procedure TCreation.CreateCollisionRects;
var
  i: Integer;
  LineHeight: Integer;

const
  FailName: string = 'TCreation.CreateCollisonRects';
begin
  Log.DebugLog(FailName);
  try

    // Modifier:=400;
    LineHeight := 24;
    // first the ArrowRects
    for i := 0 to 7 do
    begin
      ArrowRect[i].Left := 389;
      ArrowRect[i + 8].Left := 408;
      ArrowRect[i].right := 406;
      ArrowRect[i + 8].right := 425;
      ArrowRect[i].Top := 239 + i * LineHeight;
      ArrowRect[i + 8].Top := 239 + i * LineHeight;
      ArrowRect[i].bottom := 239 + i * LineHeight + LineHeight;
      ArrowRect[i + 8].bottom := 239 + i * LineHeight + LineHeight;

      ArrowRect[i].Info := txtMessage[26] + StatName[0][i + 1] + '.';

      if i < 5 then
        ArrowRect[i + 8].Info := txtMessage[27] + StatName[0][i + 1] +
          txtMessage[28] + StatName[0][i + 1] + txtMessage[29]
      else
        ArrowRect[i + 8].Info := txtMessage[30] + StatName[0][i + 1] +
          txtMessage[31] + StatName[0][i + 1] + txtMessage[29];
      ArrowRect[i].Offset(Offset);
      ArrowRect[i + 8].Offset(Offset);
    end; // end for

    // Training points
    InfoRect[0] := InformationRect(298, 212, 457, 236, txtMessage[32]);
    // Primary->Stealth
    // Item 1 no longer used here - was 'Primary skills are your characters main traits. These skills determine your Secondary
    // skills, Resistance modifiers and Damage modifers. Training points are used to increase your Primary skills.'
    InfoRect[1] := InformationRect(-100, 0, -90, 0, '');
    for i := 2 to 9 do
      InfoRect[i] := InformationRect(289, 239 + (i - 2) * LineHeight, 457,
        239 + (i - 2) * LineHeight + LineHeight, '');

    InfoRect[2].Info := txtMessage[33];
    // 'Strength represents the physical strength of a character.  Strength affects '+
    // 'how much damage a character inflicts in battle.';
    InfoRect[3].Info := txtMessage[34];
    // 'Coordination represents how agile a character is.  Coordination affects '+
    // 'a character''s movement, recovery and resistance modifiers.';
    InfoRect[4].Info := txtMessage[35];
    // 'Constitution represents a characters physical hardiness.  Constitution affects '+
    // 'a character''s healing rate and hit points.';
    InfoRect[5].Info := txtMessage[36];
    // 'Perception represents how well a character senses the area around him.';
    // 'Perception affects a character''s...something.  Lord knows I''m stumped.';
    InfoRect[6].Info := txtMessage[37];
    // 'Charm represents a character''s personal magnetism.  Charm affects the '+
    // 'prices a character can command when buying or selling items.';
    InfoRect[7].Info := txtMessage[38];
    // 'Mysticism represents a character''s magical ability.  Mysticism affects '+
    // 'the character''s recharge rate and mana.';
    InfoRect[8].Info := txtMessage[39];
    // 'Combat represents a character''s fighting ability.  Combat affects '+
    // 'the character''s damage modifiers.';
    InfoRect[9].Info := txtMessage[40];
    // 'Stealth represents the character''s ability to move and avoid detection.';

    InfoRect[10] := InformationRect(301, 92, 448, 120, txtMessage[41]);
    // the characters name
    InfoRect[11] := InformationRect(114, 93, 262, 223, txtMessage[42]);
    // the appearance
    InfoRect[12] := InformationRect(113, 236, 281, 264, txtMessage[43]);
    // shirt color
    InfoRect[13] := InformationRect(113, 278, 281, 306, txtMessage[44]);
    // pants
    InfoRect[14] := InformationRect(113, 321, 281, 348, txtMessage[45]);
    // hair color
    InfoRect[15] := InformationRect(113, 363, 281, 391, txtMessage[46]);
    // hair style
    InfoRect[16] := InformationRect(113, 406, 281, 434, txtMessage[47]);
    // beard

    InfoRect[17] := InformationRect(300, 132, 468, 160, txtMessage[48]);
    // Fighting training places an emphasis on your '+
    // 'character''s combat ability, Scouting emphasizes your character''s stealth talents '+
    // 'and Magic emphasizes your character''s spellcasting ability.';

    for i := 0 to 17 do
      InfoRect[i].Offset(Offset);

    // Now for the selectable text
    // Shirt color        // 12 og 13
    if modselection = TModSelection.RoD then // RoD, nur 2 Shirts
    begin
      i := 0;
      SelectRect[i] := SelectionRect(279, 239 + 34 + 24 * i, 279 + 123,
        239 + 34 + 24 + 24 * i, txtMessage[49 + i], txtMessage[70 + i]);
      Inc(i);
      SelectRect[i] := SelectionRect(279, 239 + 34 + 24 * i, 279 + 123,
        239 + 34 + 24 + 24 * i, txtMessage[52], txtMessage[73]);
    end
    else
    begin
      for i := 0 to 3 do
        SelectRect[i] := SelectionRect(279, 239 + 34 + 24 * i, 279 + 123,
          239 + 34 + 24 + 24 * i, txtMessage[49 + i], txtMessage[70 + i]);
    end;
    if modselection = TModSelection.RoD then // RoD, Hose 1 = schwarz
    begin
      i := 0;
      SelectRect[4 + i] := SelectionRect(279, 239 + 42 + 34 + 24 * i, 279 + 123,
        239 + 42 + 34 + 24 + 24 * i, txtMessage[53 + i], txtMessage[79]);
      Inc(i);
      SelectRect[4 + i] := SelectionRect(279, 239 + 42 + 34 + 24 * i, 279 + 123,
        239 + 42 + 34 + 24 + 24 * i, txtMessage[53 + i], txtMessage[70 + i]);
      Inc(i);
      SelectRect[4 + i] := SelectionRect(279, 239 + 42 + 34 + 24 * i, 279 + 123,
        239 + 42 + 34 + 24 + 24 * i, txtMessage[53 + i], txtMessage[70 + i]);
    end
    else
    begin
      // Pants color
      for i := 0 to 3 do
        SelectRect[4 + i] := SelectionRect(279, 239 + 42 + 34 + 24 * i,
          279 + 123, 239 + 42 + 34 + 24 + 24 * i, txtMessage[53 + i],
          txtMessage[70 + i]);
    end;
    // Hair color
    SelectRect[8] := SelectionRect(279, 357, 402, 381, txtMessage[57],
      txtMessage[74]);
    SelectRect[9] := SelectionRect(279, 381, 402, 405, txtMessage[58],
      txtMessage[71]);
    SelectRect[10] := SelectionRect(279, 405, 402, 429, txtMessage[59],
      txtMessage[75]);
    SelectRect[11] := SelectionRect(279, 429, 402, 453, txtMessage[60],
      txtMessage[76]);
    // Hair style
    for i := 0 to 3 do
      SelectRect[12 + i] := SelectionRect(279, 399 + 24 * i, 402, 423 + 24 * i,
        txtMessage[61 + i], txtMessage[77 + i]);
    // Beard
    for i := 0 to 1 do
      SelectRect[16 + i] := SelectionRect(279, 465 + 24 * i, 402, 489 + 24 * i,
        txtMessage[65 + i], txtMessage[81 + i]);
    // Training
    for i := 0 to 2 do
      SelectRect[18 + i] := SelectionRect(465, 69 + 38 + 24 * i, 465 + 123,
        69 + 38 + 24 + 24 * i, txtMessage[67 + i], txtMessage[83 + i]);

    for i := 0 to 20 do
      SelectRect[i].Offset(Offset);
  except
    on E: Exception do
      Log.Log(FailName + E.Message);
  end;

end;

procedure TCreation.LoadNames;
const
  FailName: string = 'TCreation.LoadNames';
begin
  Log.DebugLog(FailName);
  try

    // Loads all the names we use in the Mouseover help
    StatName[0][1] := txtMessage[86]; // 'Strength';
    StatName[0][2] := txtMessage[87]; // 'Coordination';
    StatName[0][3] := txtMessage[88]; // 'Constitution';
    StatName[0][4] := txtMessage[89]; // 'Perception';
    StatName[0][5] := txtMessage[90]; // 'Charm';
    StatName[0][6] := txtMessage[91]; // 'Mysticism';
    StatName[0][7] := txtMessage[92]; // 'Combat';
    StatName[0][8] := txtMessage[93]; // 'Stealth';

    StatName[1][1] := txtMessage[94]; // 'Piercing';
    StatName[1][2] := txtMessage[95]; // 'Crushing';
    StatName[1][3] := txtMessage[96]; // 'Cutting';
    StatName[1][4] := txtMessage[97]; // 'Heat';
    StatName[1][5] := txtMessage[98]; // 'Cold';
    StatName[1][6] := txtMessage[99]; // 'Electric';
    StatName[1][7] := txtMessage[100]; // 'Poison';
    StatName[1][8] := txtMessage[101]; // 'Magic';
    StatName[1][9] := txtMessage[102]; // 'Mental';
    StatName[1][10] := txtMessage[103]; // 'Stun';
    StatName[1][11] := txtMessage[104]; // 'Special';

  except
    on E: Exception do
      Log.Log(FailName + E.Message);
  end;

end;

procedure TCreation.LoadBaseValues;
var
  i: Integer;
const
  FailName: string = 'TCreation.LoadBaseValues';
begin
  Log.DebugLog(FailName);
  try
    // we store thse values so that we can keep the player from lowering his score beyon its start
    Damage := Character.Damage;
    Resistance := Character.Resistance;
    BaseStrength := Character.BaseStrength;
    BaseCoordination := Character.BaseCoordination;
    BaseConstitution := Character.BaseConstitution;
    BasePerception := Character.BasePerception;
    BaseCharm := Character.BaseCharm;
    BaseMysticism := Character.BaseMysticism;
    BaseCombat := Character.BaseCombat;
    BaseStealth := Character.BaseStealth;
    TrainingPoints := Character.TrainingPoints;

    for i := 0 to 7 do
    begin // initialize adjustments to zero
      StatAdjustments[i] := 0;
    end;
  except
    on E: Exception do
      Log.Log(FailName + E.Message);
  end;

end; // TCreation.LoadBaseValues;

procedure TCreation.ShowStats;
var
  a, b: string;
  i, X, Alpha: Integer;
const
  FailName: string = 'TCreation.ShowStats';
begin
  Log.DebugLog(FailName);
  try
    Alpha := 240; // blend value
    X := 167 + Modifier;
    str(Character.TrainingPoints, a);
    PlotText(a, X, 213, Alpha);
    // primary stats column
    i := 239;
    str(Character.Strength, b);
    PlotText(b, X, i, Alpha);
    i := i + 24;

    str(Character.Coordination, b);
    PlotText(b, X, i, Alpha);
    i := i + 24;

    str(Character.Constitution, b);
    PlotText(b, X, i, Alpha);
    i := i + 24;

    str(Character.BasePerception, b);
    PlotText(b, X, i, Alpha);
    i := i + 24;

    str(Character.Charm, b);
    PlotText(b, X, i, Alpha);
    i := i + 24;

    str(Character.Mysticism, b);
    PlotText(b, X, i, Alpha);
    i := i + 24;

    str(Character.Combat, b);
    PlotText(b, X, i, Alpha);
    i := i + 24;

    str(Character.Stealth, b);
    PlotText(b, X, i, Alpha);

  except
    on E: Exception do
      Log.Log(FailName + E.Message);
  end;
end;

procedure TCreation.updateClothing;
var
  i: Integer;
  S: string;
begin
  Female := TCharacterResource(Player.Resource).Female;

  shirt[1] := PartManager.LoadItem('TunicBlue',
    TCharacterResource(Player.Resource).NakedName);
  shirt[2] := PartManager.LoadItem('TunicBrown',
    TCharacterResource(Player.Resource).NakedName);
  shirt[3] := PartManager.LoadItem('TunicYellow',
    TCharacterResource(Player.Resource).NakedName);
  shirt[4] := PartManager.LoadItem('TunicGreen',
    TCharacterResource(Player.Resource).NakedName);
  for i := 1 to 4 do
    shirt[i].Resource := PartManager.GetLayerResource(shirt[i].LayeredImage);
  if modselection <> TModSelection.DoA then
  begin
    pants[1] := PartManager.LoadItem('BluePants',
      TCharacterResource(Player.Resource).NakedName);
    pants[2] := PartManager.LoadItem('BrownPants',
      TCharacterResource(Player.Resource).NakedName);
    pants[3] := PartManager.LoadItem('YellowPants',
      TCharacterResource(Player.Resource).NakedName);
    pants[4] := PartManager.LoadItem('GreenPants',
      TCharacterResource(Player.Resource).NakedName);
    if modselection = TModSelection.SoA then
    begin
      boots := PartManager.LoadItem('LowLeatherBoots',
        TCharacterResource(Player.Resource).NakedName);
      boots.Resource := PartManager.GetLayerResource(boots.LayeredImage);
    end;
    for i := 1 to 4 do
      pants[i].Resource := PartManager.GetLayerResource(pants[i].LayeredImage);
  end
  else // Days of Ahoul
  begin
    pants[1] := PartManager.LoadItem('Patchworkpants1',
      TCharacterResource(Player.Resource).NakedName);
    pants[2] := PartManager.LoadItem('Patchworkpants2',
      TCharacterResource(Player.Resource).NakedName);
    for i := 1 to 2 do
      pants[i].Resource := PartManager.GetLayerResource(pants[i].LayeredImage);
  end;
  S := ExtractFilePath(TCharacterResource(Player.Resource).NakedName);

  if modselection = TModSelection.DoA then
  begin
    tattoo[1] := PartManager.LoadItem('Tatoo2',
      TCharacterResource(Player.Resource).NakedName);
    tattoo[2] := PartManager.LoadItem('Tatoo1',
      TCharacterResource(Player.Resource).NakedName);
    tattoo[3] := nil;
    for i := 1 to 2 do
      tattoo[i].Resource := PartManager.GetLayerResource
        (tattoo[i].LayeredImage);

    LayeredImage := S + 'ShamanHair2'; // Only as Shaman
    DoAHair[1] := PartManager.GetLayerResource(LayeredImage);
    LayeredImage := S + 'ShamanPonyBeardDark';
    DoAHair[2] := PartManager.GetLayerResource(LayeredImage);
    LayeredImage := S + 'ShamanPonytail';
    DoAHair[3] := PartManager.GetLayerResource(LayeredImage);
    DoAHair[4] := nil; // Glatze
    LayeredImage := S + 'AhoulHeadGreen'; // Only as Ahoul
    DoAHair[5] := PartManager.GetLayerResource(LayeredImage);
    LayeredImage := S + 'AhoulHeadTan'; // Only as Halfbreed
    DoAHair[6] := PartManager.GetLayerResource(LayeredImage);
  end
  else
  begin
    LayeredImage := PartManager.GetImageFile('prt_ShortHairBeardLight',
      TCharacterResource(Player.Resource).NakedName);
    if PartManager.NotFound then
      LayeredImage := S + 'ShortHairBeardLight';
    hair[1, 1, 1] := PartManager.GetLayerResource(LayeredImage);
    LayeredImage := PartManager.GetImageFile('prt_LongHairBeardLight',
      TCharacterResource(Player.Resource).NakedName);
    if PartManager.NotFound then
      LayeredImage := S + 'LongHairBeardLight';
    hair[1, 2, 1] := PartManager.GetLayerResource(LayeredImage);
    LayeredImage := PartManager.GetImageFile('prt_PonytailBeardLight',
      TCharacterResource(Player.Resource).NakedName);
    if PartManager.NotFound then
      LayeredImage := S + 'PonyBeardLight';
    hair[1, 3, 1] := PartManager.GetLayerResource(LayeredImage);
    LayeredImage := PartManager.GetImageFile('prt_BeardLight',
      TCharacterResource(Player.Resource).NakedName);
    if PartManager.NotFound then
      LayeredImage := S + 'BeardLight';
    hair[1, 4, 1] := PartManager.GetLayerResource(LayeredImage);

    LayeredImage := PartManager.GetImageFile('prt_ShortHairBeardDark',
      TCharacterResource(Player.Resource).NakedName);
    if PartManager.NotFound then
      LayeredImage := S + 'ShortHairBeardDark';
    hair[2, 1, 1] := PartManager.GetLayerResource(LayeredImage);
    LayeredImage := PartManager.GetImageFile('prt_LongHairBeardDark',
      TCharacterResource(Player.Resource).NakedName);
    if PartManager.NotFound then
      LayeredImage := S + 'LongHairBeardDark';
    hair[2, 2, 1] := PartManager.GetLayerResource(LayeredImage);
    LayeredImage := PartManager.GetImageFile('prt_PonytailBeardDark',
      TCharacterResource(Player.Resource).NakedName);
    if PartManager.NotFound then
      LayeredImage := S + 'PonyBeardDark';
    hair[2, 3, 1] := PartManager.GetLayerResource(LayeredImage);
    LayeredImage := PartManager.GetImageFile('prt_BeardDark',
      TCharacterResource(Player.Resource).NakedName);
    if PartManager.NotFound then
      LayeredImage := S + 'BeardDark';
    hair[2, 4, 1] := PartManager.GetLayerResource(LayeredImage);

    LayeredImage := PartManager.GetImageFile('prt_ShortHairBeardRed',
      TCharacterResource(Player.Resource).NakedName);
    if PartManager.NotFound then
      LayeredImage := S + 'ShortHairBeardRed';
    hair[3, 1, 1] := PartManager.GetLayerResource(LayeredImage);
    LayeredImage := PartManager.GetImageFile('prt_LongHairBeardRed',
      TCharacterResource(Player.Resource).NakedName);
    if PartManager.NotFound then
      LayeredImage := S + 'LongHairBeardRed';
    hair[3, 2, 1] := PartManager.GetLayerResource(LayeredImage);
    LayeredImage := PartManager.GetImageFile('prt_PonytailBeardRed',
      TCharacterResource(Player.Resource).NakedName);
    if PartManager.NotFound then
      LayeredImage := S + 'PonyBeardRed';
    hair[3, 3, 1] := PartManager.GetLayerResource(LayeredImage);
    LayeredImage := PartManager.GetImageFile('prt_BeardRed',
      TCharacterResource(Player.Resource).NakedName);
    if PartManager.NotFound then
      LayeredImage := S + 'BeardRed';
    hair[3, 4, 1] := PartManager.GetLayerResource(LayeredImage);

    LayeredImage := PartManager.GetImageFile('prt_ShortHairBeardGrey',
      TCharacterResource(Player.Resource).NakedName);
    if PartManager.NotFound then
      LayeredImage := S + 'ShortHairBeardGrey';
    hair[4, 1, 1] := PartManager.GetLayerResource(LayeredImage);
    LayeredImage := PartManager.GetImageFile('prt_LongHairBeardGrey',
      TCharacterResource(Player.Resource).NakedName);
    if PartManager.NotFound then
      LayeredImage := S + 'LongHairBeardGrey';
    hair[4, 2, 1] := PartManager.GetLayerResource(LayeredImage);
    LayeredImage := PartManager.GetImageFile('prt_PonytailBeardGrey',
      TCharacterResource(Player.Resource).NakedName);
    if PartManager.NotFound then
      LayeredImage := S + 'PonyBeardGrey';
    hair[4, 3, 1] := PartManager.GetLayerResource(LayeredImage);
    LayeredImage := PartManager.GetImageFile('prt_BeardGrey',
      TCharacterResource(Player.Resource).NakedName);
    if PartManager.NotFound then
      LayeredImage := S + 'BeardGrey';
    hair[4, 4, 1] := PartManager.GetLayerResource(LayeredImage);

    LayeredImage := PartManager.GetImageFile('prt_ShortHairLight',
      TCharacterResource(Player.Resource).NakedName);
    if PartManager.NotFound then
      LayeredImage := S + 'ShortHairLight';
    hair[1, 1, 2] := PartManager.GetLayerResource(LayeredImage);
    LayeredImage := PartManager.GetImageFile('prt_LongHairLight',
      TCharacterResource(Player.Resource).NakedName);
    if PartManager.NotFound then
      LayeredImage := S + 'LongHairLight';
    hair[1, 2, 2] := PartManager.GetLayerResource(LayeredImage);
    LayeredImage := PartManager.GetImageFile('prt_PonytailLight',
      TCharacterResource(Player.Resource).NakedName);
    if PartManager.NotFound then
      LayeredImage := S + 'PonytailLight';
    hair[1, 3, 2] := PartManager.GetLayerResource(LayeredImage);
    hair[1, 4, 2] := nil;

    LayeredImage := PartManager.GetImageFile('prt_ShortHairDark',
      TCharacterResource(Player.Resource).NakedName);
    if PartManager.NotFound then
      LayeredImage := S + 'ShortHairDark';
    hair[2, 1, 2] := PartManager.GetLayerResource(LayeredImage);
    LayeredImage := PartManager.GetImageFile('prt_LongHairDark',
      TCharacterResource(Player.Resource).NakedName);
    if PartManager.NotFound then
      LayeredImage := S + 'LongHairDark';
    hair[2, 2, 2] := PartManager.GetLayerResource(LayeredImage);
    LayeredImage := PartManager.GetImageFile('prt_PonytailDark',
      TCharacterResource(Player.Resource).NakedName);
    if PartManager.NotFound then
      LayeredImage := S + 'PonytailDark';
    hair[2, 3, 2] := PartManager.GetLayerResource(LayeredImage);
    hair[2, 4, 2] := nil;

    LayeredImage := PartManager.GetImageFile('prt_ShortHairRed',
      TCharacterResource(Player.Resource).NakedName);
    if PartManager.NotFound then
      LayeredImage := S + 'ShortHairRed';
    hair[3, 1, 2] := PartManager.GetLayerResource(LayeredImage);
    LayeredImage := PartManager.GetImageFile('prt_LongHairRed',
      TCharacterResource(Player.Resource).NakedName);
    if PartManager.NotFound then
      LayeredImage := S + 'LongHairRed';
    hair[3, 2, 2] := PartManager.GetLayerResource(LayeredImage);
    LayeredImage := PartManager.GetImageFile('prt_PonytailRed',
      TCharacterResource(Player.Resource).NakedName);
    if PartManager.NotFound then
      LayeredImage := S + 'PonytailRed';
    hair[3, 3, 2] := PartManager.GetLayerResource(LayeredImage);
    hair[3, 4, 2] := nil;

    LayeredImage := PartManager.GetImageFile('prt_ShortHairGrey',
      TCharacterResource(Player.Resource).NakedName);
    if PartManager.NotFound then
      LayeredImage := S + 'ShortHairGrey';
    hair[4, 1, 2] := PartManager.GetLayerResource(LayeredImage);
    LayeredImage := PartManager.GetImageFile('prt_LongHairGrey',
      TCharacterResource(Player.Resource).NakedName);
    if PartManager.NotFound then
      LayeredImage := S + 'LongHairGrey';
    hair[4, 2, 2] := PartManager.GetLayerResource(LayeredImage);
    LayeredImage := PartManager.GetImageFile('prt_PonytailGrey',
      TCharacterResource(Player.Resource).NakedName);
    if PartManager.NotFound then
      LayeredImage := S + 'PonytailGrey';
    hair[4, 3, 2] := PartManager.GetLayerResource(LayeredImage);
    hair[4, 4, 2] := nil;
    // TODO: Why the above ?
    hair[1, 1, 1] := PartManager.GetLayerResource(S + 'ShortHairBeardLight');
    hair[1, 2, 1] := PartManager.GetLayerResource(S + 'LongHairBeardLight');
    hair[1, 3, 1] := PartManager.GetLayerResource(S + 'PonyBeardLight');
    hair[1, 4, 1] := PartManager.GetLayerResource(S + 'BeardLight');

    hair[2, 1, 1] := PartManager.GetLayerResource(S + 'ShortHairBeardDark');
    hair[2, 2, 1] := PartManager.GetLayerResource(S + 'LongHairBeardDark');
    hair[2, 3, 1] := PartManager.GetLayerResource(S + 'PonyBeardDark');
    hair[2, 4, 1] := PartManager.GetLayerResource(S + 'BeardDark');

    hair[3, 1, 1] := PartManager.GetLayerResource(S + 'ShortHairBeardRed');
    hair[3, 2, 1] := PartManager.GetLayerResource(S + 'LongHairBeardRed');
    hair[3, 3, 1] := PartManager.GetLayerResource(S + 'PonyBeardRed');
    hair[3, 4, 1] := PartManager.GetLayerResource(S + 'BeardRed');

    hair[4, 1, 1] := PartManager.GetLayerResource(S + 'ShortHairBeardGrey');
    hair[4, 2, 1] := PartManager.GetLayerResource(S + 'LongHairBeardGrey');
    hair[4, 3, 1] := PartManager.GetLayerResource(S + 'PonyBeardGrey');
    hair[4, 4, 1] := PartManager.GetLayerResource(S + 'BeardGrey');

    hair[1, 1, 2] := PartManager.GetLayerResource(S + 'ShortHairLight');
    hair[1, 2, 2] := PartManager.GetLayerResource(S + 'LongHairLight');
    hair[1, 3, 2] := PartManager.GetLayerResource(S + 'PonyTailLight');

    hair[2, 1, 2] := PartManager.GetLayerResource(S + 'ShortHairDark');
    hair[2, 2, 2] := PartManager.GetLayerResource(S + 'LongHairDark');
    hair[2, 3, 2] := PartManager.GetLayerResource(S + 'PonyTailDark');

    hair[3, 1, 2] := PartManager.GetLayerResource(S + 'ShortHairRed');
    hair[3, 2, 2] := PartManager.GetLayerResource(S + 'LongHairRed');
    hair[3, 3, 2] := PartManager.GetLayerResource(S + 'PonyTailRed');

    hair[4, 1, 2] := PartManager.GetLayerResource(S + 'ShortHairGrey');
    hair[4, 2, 2] := PartManager.GetLayerResource(S + 'LongHairGrey');
    hair[4, 3, 2] := PartManager.GetLayerResource(S + 'PonyTailGrey');

    if Female then
      hair[1, 3, 2] := PartManager.GetLayerResource(S + 'FemHiPonytailLight')
    else
      hair[1, 3, 2] := PartManager.GetLayerResource(S + 'PonytailLight');
    hair[1, 4, 2] := nil;

    hair[2, 1, 2] := PartManager.GetLayerResource(S + 'ShortHairDark');
    hair[2, 2, 2] := PartManager.GetLayerResource(S + 'LongHairDark');
    if Female then
      hair[2, 3, 2] := PartManager.GetLayerResource(S + 'FemHiPonytailDark')
    else
      hair[2, 3, 2] := PartManager.GetLayerResource(S + 'PonytailDark');
    hair[2, 4, 2] := nil;

    hair[3, 1, 2] := PartManager.GetLayerResource(S + 'ShortHairRed');
    hair[3, 2, 2] := PartManager.GetLayerResource(S + 'LongHairRed');
    if Female then
      hair[3, 3, 2] := PartManager.GetLayerResource(S + 'FemHiPonytailRed')
    else
      hair[3, 3, 2] := PartManager.GetLayerResource(S + 'PonytailRed');
    hair[3, 4, 2] := nil;

    hair[4, 1, 2] := PartManager.GetLayerResource(S + 'ShortHairGrey');
    hair[4, 2, 2] := PartManager.GetLayerResource(S + 'LongHairGrey');
    if Female then
      hair[4, 3, 2] := PartManager.GetLayerResource(S + 'FemHiPonytailGrey')
    else
      hair[4, 3, 2] := PartManager.GetLayerResource(S + 'PonytailGrey');
    hair[4, 4, 2] := nil;
  end; // Ende nicht DoA
end;
// TCreation.ShowStats

procedure TCreation.KeyDown(Sender: TObject; var key: Word; Shift: TShiftState);
var
  i: Integer;
  a: string;
  pr: TRect;
const
  FailName: string = 'TCreation.Keydown';
begin
  Log.DebugLog(FailName);
  try
    // DebugPlot(Key);
    pr := Rect(300, 92, 448, 120);
    lpDDSBack.BltFast(300 + Offset.X, 92 + Offset.Y, DXBack, @pr,
      DDBLTFAST_WAIT);
    if key = 39 then
      key := 999; // keep the right arrow form printing Apostrophes
    if key = 222 then
      key := 39; // apostrophe
    if key = 189 then
      key := 45; // dash
    if ((key > 64) and (key < 91)) or ((key > 47) and (key < 58)) or (key = 32)
      or (key = 189) or (key = 39) or (key = 45) then
    begin
      if ((key > 64) and (key < 91)) and (Shift <> [ssShift]) then
      // make the char lowercase
        key := key + 32;
      // if (Length(CharacterName) < 13) and (pText.TextLength(CharacterName)< 130) then begin
      if pText.TextLength(CharacterName) < 120 then
      begin
        if CaratCharPosition = Length(CharacterName) then
        begin // adding a char to end of string
          CharacterName := CharacterName + char(key);
          CaratPosition := pText.TextLength(CharacterName);
          CaratCharPosition := CaratCharPosition + 1;
        end
        else
        begin // inserting a char
          CaratCharPosition := CaratCharPosition + 1;
          CharacterName := CharacterName + 'z';
          // increase the size of the string by a char
          for i := Length(CharacterName) downto CaratCharPosition do
          begin
            CharacterName[i] := CharacterName[i - 1];
          end; // end for
          CharacterName[CaratCharPosition] := char(key);
          a := CharacterName;
          SetLength(a, CaratCharPosition);
          CaratPosition := pText.TextLength(a);
        end;
      end; // endif if length< 21
    end
    else if (key = 8) then
    begin // backspace
      if CharacterName <> '' then
      begin
        if CaratCharPosition = Length(CharacterName) then
        begin // if at the end of the name
          CaratCharPosition := CaratCharPosition - 1;
          if CaratCharPosition = 0 then
            CharacterName := ''
          else
            SetLength(CharacterName, CaratCharPosition);
          CaratPosition := pText.TextLength(CharacterName);
        end
        else if CaratCharPosition > 0 then
        begin // in middle of name somewhere
          CaratCharPosition := CaratCharPosition - 1;
          for i := CaratCharPosition + 2 to Length(CharacterName) do
          begin // chop out the middle char
            CharacterName[i - 1] := CharacterName[i];
          end; // end for
          SetLength(CharacterName, Length(CharacterName) - 1);
          a := CharacterName;
          SetLength(a, CaratCharPosition);
          CaratPosition := pText.TextLength(a);
        end;

      end; // endif length
    end
    else if (key = 46) then
    begin // Delete
      if (CharacterName <> '') and (CaratCharPosition <> Length(CharacterName))
      then
      begin
        if (CaratCharPosition = 0) and (Length(CharacterName) = 1) then
          CharacterName := ''
        else
        begin
          for i := CaratCharPosition + 1 to Length(CharacterName) do
          begin
            CharacterName[i] := CharacterName[i + 1];
          end;
          SetLength(CharacterName, Length(CharacterName) - 1);
        end;
      end;
    end
    else if key = 37 then
    begin // left arrow
      if CaratCharPosition > 0 then
      begin
        CaratCharPosition := CaratCharPosition - 1;
        a := CharacterName;
        SetLength(a, CaratCharPosition);
        CaratPosition := pText.TextLength(a);
      end; // endif
    end
    else if key = 999 then
    begin // right arrow
      if CaratCharPosition < Length(CharacterName) then
      begin
        CaratCharPosition := CaratCharPosition + 1;
        a := CharacterName;
        SetLength(a, CaratCharPosition);
        CaratPosition := pText.TextLength(a);
      end; // endif
    end;

    // Character.Name:=CharacterName;
    PlotText(CharacterName, 310, 95, 240);
    // plot the Carat
    PlotText('|', CaratPosition + 310, 95, 240);
    SoAOS_DX_BltFront;
  except
    on E: Exception do
      Log.Log(FailName + E.Message);
  end;

end; // TCreation.KeyDown

function TCreation.adjustStat(idx: Integer): boolean;
var
  B1, B2, B3, B4: boolean;
begin // if over an Arrow
  Result := false;

  B1 := ((idx > 7) and (idx < 13) and (Character.TrainingPoints > 3));
  B2 := ((idx > 12) and (Character.TrainingPoints > 1));
  B3 := ((idx < 5) and (StatAdjustments[idx] > 0));
  B4 := ((idx > 4) and (idx < 8) and (StatAdjustments[idx] > 0));

  if B1 or B2 or B3 or B4 then
  begin
    if B1 then
    begin // adjust training points, keep track of training points added
      Character.TrainingPoints := -4;
      StatAdjustments[idx - 8] := StatAdjustments[idx - 8] + 1;
    end
    else if B2 then
    begin
      Character.TrainingPoints := -2;
      StatAdjustments[idx - 8] := StatAdjustments[idx - 8] + 1;
    end
    else if B3 then
    begin
      Character.TrainingPoints := +4;
      StatAdjustments[idx] := StatAdjustments[idx] - 1;
    end
    else
    begin
      Character.TrainingPoints := +2;
      StatAdjustments[idx] := StatAdjustments[idx] - 1;
    end;

    case idx of
      0:
        Character.Strength := Character.BaseStrength - 1;
      1:
        Character.Coordination := Character.BaseCoordination - 1;
      2:
        Character.Constitution := Character.BaseConstitution - 1;
      3:
        Character.Perception := Character.BasePerception - 1;
      4:
        Character.Charm := Character.BaseCharm - 1;
      5:
        Character.Mysticism := Character.BaseMysticism - 1;
      6:
        Character.Combat := Character.BaseCombat - 1;
      7:
        Character.Stealth := Character.BaseStealth - 1;

      8:
        Character.Strength := Character.BaseStrength + 1;
      9:
        Character.Coordination := Character.BaseCoordination + 1;
      10:
        Character.Constitution := Character.BaseConstitution + 1;
      11:
        Character.Perception := Character.BasePerception + 1;
      12:
        Character.Charm := Character.BaseCharm + 1;
      13:
        Character.Mysticism := Character.BaseMysticism + 1;
      14:
        Character.Combat := Character.BaseCombat + 1;
      15:
        Character.Stealth := Character.BaseStealth + 1;
    end;

    Result := True;
  end;
end;

procedure TCreation.CaratTimerEvent(Sender: TObject);
const
  FailName: string = 'TCreation.CaratTimerEvent';
var
  pr: TRect;
begin
  Log.DebugLog(FailName);
  try
    pr := Rect(300, 92, 448, 120);
    lpDDSBack.BltFast(300 + Offset.X, 92 + Offset.Y, DXBack, @pr,
      DDBLTFAST_WAIT);
    Inc(LoopCounter);
    if LoopCounter >= 5 then
    begin
      CaratVisible := (CaratVisible = false);
      LoopCounter := 0;
    end;
    if CaratVisible then
    begin
      PlotText('|', CaratPosition + 310, 95, 240);
    end;
    PlotText(CharacterName, 310, 95, 240);
    DrawNewPlayer;
    // lpDDSFront_Flip(nil, DDFLIP_WAIT);
    // lpDDSBack.BltFast(0, 0, lpDDSFront, Rect(0, 0, 800, 600), DDBLTFAST_WAIT);
  except
    on E: Exception do
      Log.Log(FailName + E.Message);
  end;

end; // TCreation.CaratTimerEvent

procedure TCreation.OpenBox(box: TBoxEnum);
var
  i: Integer;
  pr: TRect;
const
  FailName: string = 'TCreation.OpenBox';
begin
  Log.DebugLog(FailName);
  try

    // clean up any dimmed text
    pr := Rect(114, 237, 261, 439);
    lpDDSBack.BltFast(pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr,
      DDBLTFAST_WAIT);
    // clean up after old boxes
    pr := Rect(279, 239, 402, 588);
    lpDDSBack.BltFast(pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr,
      DDBLTFAST_WAIT);
    if BoxOpen = bxTraining then
    begin
      pr := Rect(465, 59, 465 + 123, 59 + 180);
      lpDDSBack.BltFast(pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr,
        DDBLTFAST_WAIT);
    end;
    // clear the selectable rects
    for i := 0 to 20 do
      SelectRect[i].enabled := false;

    if box = bxTraining then // Training
    begin
      pr := Rect(490, 160, 720, 500);
      lpDDSBack.BltFast(pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr,
        DDBLTFAST_WAIT);
      pr := Rect(0, 0, 123, 180);
      lpDDSBack.BltFast(465 + Offset.X, 59 + Offset.Y, DXBox, @pr,
        DDBLTFAST_WAIT);
      pr := Rect(0, 0, 96, 21);
      lpDDSBack.BltFast(465 + 13 + Offset.X, 69 + 38 + 24 * (SelectedTraining) +
        Offset.Y, DXCircle, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
      if modselection = TModSelection.RoD then // RoD - Kämpfer
      begin
        PlotTextCentered(lpDDSBack, txtMessage[5], 465, 465 + 123, 69 + 38, 240,
          UseSmallFont);
        // SelectRect[ 18 ].rect := rect( 465, 69 + 38 , 465 + 123, 69 + 38 + 24 );
        SelectRect[18].enabled := True;
        SelectRect[19].enabled := false;
        SelectRect[20].enabled := false;
      end
      else if modselection = TModSelection.Caves then // Caves = Magier
      begin
        PlotTextCentered(lpDDSBack, txtMessage[7], 465, 465 + 123, 69 + 86, 240,
          UseSmallFont);
        // SelectRect[ 20 ].rect := rect( 465, 69 + 38 + 24 * 2, 465 + 123, 69 + 38 + 24 + 24 * 2 );
        SelectRect[18].enabled := false;
        SelectRect[19].enabled := false;
        SelectRect[20].enabled := True;
      end
      else if modselection = TModSelection.DoA then // DoA
        if AhoulRace = 2 then // Ahoul, no Shaman or Halfbreed
        begin
          PlotTextCentered(lpDDSBack, txtMessage[5], 465, 465 + 123, 69 + 38,
            240, UseSmallFont);
          PlotTextCentered(lpDDSBack, txtMessage[6], 465, 465 + 123, 69 + 62,
            240, UseSmallFont);
          // for i := 0 to 1 do
          // SelectRect[ 18 + i ].rect := rect( 465, 69 + 38 + 24 * i, 465 + 123, 69 + 38 + 24 + 24 * i );
          SelectRect[18].enabled := True;
          SelectRect[19].enabled := True;
          SelectRect[20].enabled := false;
        end
        else if AhoulRace = 1 then // Shaman, no Ahoul or Halfbreed
        begin
          PlotTextCentered(lpDDSBack, txtMessage[7], 465, 465 + 123, 69 + 86,
            240, UseSmallFont);
          // for i := 2 to 2 do
          // SelectRect[ 18 + i ].rect := rect( 465, 69 + 38 + 24 * i, 465 + 123, 69 + 38 + 24 + 24 * i );
          SelectRect[18].enabled := false;
          SelectRect[19].enabled := false;
          SelectRect[20].enabled := True;
        end
        else // halfbreed
        begin
          PlotTextCentered(lpDDSBack, txtMessage[5], 465, 465 + 123, 69 + 38,
            240, UseSmallFont);
          PlotTextCentered(lpDDSBack, txtMessage[6], 465, 465 + 123, 69 + 62,
            240, UseSmallFont);
          PlotTextCentered(lpDDSBack, txtMessage[7], 465, 465 + 123, 69 + 86,
            240, UseSmallFont);
          for i := 0 to 2 do
            // SelectRect[ 18 + i ].rect := rect( 465, 69 + 38 + 24 * i, 465 + 123, 69 + 38 + 24 + 24 * i )
            SelectRect[18 + i].enabled := True;
        end
      else // SoA, PoA, AoA, TsK
      begin
        PlotTextCentered(lpDDSBack, txtMessage[5], 465, 465 + 123, 69 + 38, 240,
          UseSmallFont);
        PlotTextCentered(lpDDSBack, txtMessage[6], 465, 465 + 123, 69 + 62, 240,
          UseSmallFont);
        PlotTextCentered(lpDDSBack, txtMessage[7], 465, 465 + 123, 69 + 86, 240,
          UseSmallFont);
        for i := 0 to 2 do
          SelectRect[18 + i].enabled := True;
      end;
    end
    else
    begin
      for i := 12 to 16 do
      begin
        if i <> ord(box) then
          DrawAlpha(lpDDSBack, Rect(InfoRect[i].Left, InfoRect[i].Top,
            InfoRect[i].right - 20, InfoRect[i].bottom), Rect(0, 0, 25, 25),
            DXBlack, false, 200);
      end;
      // plot box
      pr := Rect(0, 0, 123, 180);
      lpDDSBack.BltFast(279 + Offset.X, 239 + (ord(box) - 12) * 42 + Offset.Y,
        DXBox, @pr, DDBLTFAST_WAIT);
      if (box = bxShirt) or (box = bxPants) then
      begin
        // now showselected shirt or pants
        pr := Rect(0, 0, 96, 21);
        if box = bxShirt then
          if modselection <> TModSelection.DoA then
            lpDDSBack.BltFast(279 + 13 + Offset.X, 239 + (ord(box) - 12) * 42 +
              34 + 24 * ixSelectedShirt + Offset.Y, DXCircle, @pr,
              DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT)
          else // Unterschied: ixselectedbase statt ixselectedshirt
            lpDDSBack.BltFast(279 + 13 + Offset.X, 239 + (ord(box) - 12) * 42 +
              34 + 24 * ixSelectedBase + Offset.Y, DXCircle, @pr,
              DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT)
        else // Box = bxpants
          lpDDSBack.BltFast(279 + 13 + Offset.X, 239 + (ord(box) - 12) * 42 + 34
            + 24 * (ixSelectedPants) + Offset.Y, DXCircle, @pr,
            DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);

        if modselection = TModSelection.RoD then // Rise of Dwarf
        begin
          if box = bxShirt then
          begin
            PlotTextCentered(nil, txtMessage[12], 279, 279 + 123,
              239 + (ord(box) - 12) * 42 + 34, 240);
            PlotTextCentered(nil, txtMessage[15], 279, 279 + 123,
              239 + (ord(box) - 12) * 42 + 58, 240);
            for i := 0 to 1 do
              // SelectRect[ ( Box - 12 ) * 4 + i ].rect := rect( 279, 239 + ( box - 12 ) * 42 + 34 + 24 * i, 279 + 123, 239 + ( box - 12 ) * 42 + 34 + 24 + 24 * i )
              SelectRect[(ord(box) - 12) * 4 + i].enabled := True;
          end
          else if box = bxPants then
          begin
            PlotTextCentered(nil, 'Schwarz', 279, 279 + 123,
              239 + (ord(box) - 12) * 42 + 34, 240);
            PlotTextCentered(nil, txtMessage[13], 279, 279 + 123,
              239 + (ord(box) - 12) * 42 + 58, 240);
            PlotTextCentered(nil, txtMessage[14], 279, 279 + 123,
              239 + (ord(box) - 12) * 42 + 82, 240);
            for i := 0 to 2 do
              // SelectRect[ ( Box - 12 ) * 4 + i ].rect := rect( 279, 239 + ( box - 12 ) * 42 + 34 + 24 * i, 279 + 123, 239 + ( box - 12 ) * 42 + 34 + 24 + 24 * i );
              SelectRect[(ord(box) - 12) * 4 + i].enabled := True;
          end;
        end // End Rise of Dwarf
        else if modselection = TModSelection.DoA then
        // Days of Ahoul - Keine Tunika, stattdessen BaseAhoul oder BaseShaman
        begin
          if box = bxShirt then
          begin
            PlotTextCentered(nil, 'Shaman', 279, 279 + 123,
              239 + (ord(box) - 12) * 42 + 34, 240);
            PlotTextCentered(nil, 'Ahoul', 279, 279 + 123,
              239 + (ord(box) - 12) * 42 + 58, 240);
            PlotTextCentered(nil, 'Mischling', 279, 279 + 123,
              239 + (ord(box) - 12) * 42 + 82, 240);
            for i := 0 to 2 do
              SelectRect[(ord(box) - 12) * 4 + i].enabled := True;
            // SelectRect[ ( Box - 12 ) * 4 + i ].rect := rect( 279, 239 + ( box - 12 ) * 42 + 34 + 24 * i, 279 + 123, 239 + ( box - 12 ) * 42 + 34 + 24 + 24 * i )
          end
          else if box = bxPants then
          begin
            PlotTextCentered(nil, txtMessage[12], 279, 279 + 123,
              239 + (ord(box) - 12) * 42 + 34, 240);
            PlotTextCentered(nil, txtMessage[13], 279, 279 + 123,
              239 + (ord(box) - 12) * 42 + 58, 240);
            for i := 0 to 1 do
              SelectRect[(ord(box) - 12) * 4 + i].enabled := True;
            // SelectRect[ ( Box - 12 ) * 4 + i ].rect := rect( 279, 239 + ( box - 12 ) * 42 + 34 + 24 * i, 279 + 123, 239 + ( box - 12 ) * 42 + 34 + 24 + 24 * i );
          end;
        end // End Days of Ahoul
        else
        begin
          PlotTextCentered(nil, txtMessage[12], 279, 279 + 123,
            239 + (ord(box) - 12) * 42 + 34, 240);
          PlotTextCentered(nil, txtMessage[13], 279, 279 + 123,
            239 + (ord(box) - 12) * 42 + 58, 240);
          PlotTextCentered(nil, txtMessage[14], 279, 279 + 123,
            239 + (ord(box) - 12) * 42 + 82, 240);
          PlotTextCentered(nil, txtMessage[15], 279, 279 + 123,
            239 + (ord(box) - 12) * 42 + 106, 240);
          for i := 0 to 3 do
            SelectRect[(ord(box) - 12) * 4 + i].enabled := True;
        end;
      end
      else if box = bxHairColor then // hair
      begin
        pr := Rect(0, 0, 96, 21);
        if modselection = TModSelection.RoD then // Rise of Dwarf
        begin
          lpDDSBack.BltFast(279 + 13 + Offset.X, 239 + (ord(box) - 12) * 42 + 34
            + 24 * (ixSelectedHair) + Offset.Y, DXCircle, @pr,
            DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
          PlotTextCentered(nil, txtMessage[16], 279, 279 + 123,
            239 + (ord(box) - 12) * 42 + 34, 240);
          PlotTextCentered(nil, txtMessage[17], 279, 279 + 123,
            239 + (ord(box) - 12) * 42 + 58, 240);
          PlotTextCentered(nil, txtMessage[18], 279, 279 + 123,
            239 + (ord(box) - 12) * 42 + 82, 240);
          for i := 0 to 2 do
            SelectRect[(ord(box) - 12) * 4 + i].enabled := True;
          // SelectRect[ ( Box - 12 ) * 4 + i ].rect := rect( 279, 239 + ( box - 12 ) * 42 + 34 + 24 * i, 279 + 123, 239 + ( box - 12 ) * 42 + 34 + 24 + 24 * i );
        end // End Rise of Dwarf
        else if modselection = TModSelection.DoA then // Days of Ahoul
        begin
          lpDDSBack.BltFast(279 + 13 + Offset.X, 239 + (ord(box) - 12) * 42 + 34
            + 24 * (ixSelectedTattoo) + Offset.Y, DXCircle, @pr,
            DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
          PlotTextCentered(nil, txtMessage[15], 279, 279 + 123,
            239 + (ord(box) - 12) * 42 + 34, 240); // blau
          PlotTextCentered(nil, txtMessage[14], 279, 279 + 123,
            239 + (ord(box) - 12) * 42 + 58, 240); // gelb
          if language = 'german' then
            PlotTextCentered(nil, 'Keins', 279, 279 + 123,
              239 + (ord(box) - 12) * 42 + 82, 240)
          else
            PlotTextCentered(nil, 'no', 279, 279 + 123, 239 + (ord(box) - 12) *
              42 + 82, 240);
          for i := 0 to 2 do
            // SelectRect[ ( Box - 12 ) * 4 + i ].rect := rect( 279, 239 + ( box - 12 ) * 42 + 34 + 24 * i, 279 + 123, 239 + ( box - 12 ) * 42 + 34 + 24 + 24 * i );
            SelectRect[(ord(box) - 12) * 4 + i].enabled := True;
        end
        else
        begin
          lpDDSBack.BltFast(279 + 13 + Offset.X, 239 + (ord(box) - 12) * 42 + 34
            + 24 * (ixSelectedHair) + Offset.Y, DXCircle, @pr,
            DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
          PlotTextCentered(nil, txtMessage[16], 279, 279 + 123,
            239 + (ord(box) - 12) * 42 + 34, 240);
          PlotTextCentered(nil, txtMessage[17], 279, 279 + 123,
            239 + (ord(box) - 12) * 42 + 58, 240);
          PlotTextCentered(nil, txtMessage[18], 279, 279 + 123,
            239 + (ord(box) - 12) * 42 + 82, 240);
          PlotTextCentered(nil, txtMessage[19], 279, 279 + 123,
            239 + (ord(box) - 12) * 42 + 106, 240);
          for i := 0 to 3 do
            SelectRect[(ord(box) - 12) * 4 + i].enabled := True;
        end;
      end
      else if box = bxHairStyle then // hairstyle
      begin
        pr := Rect(0, 0, 96, 21);
        if modselection = TModSelection.RoD then // Rise of Dwarf
        begin
          lpDDSBack.BltFast(279 + 13 + Offset.X, 239 + (ord(box) - 12) * 42 + 34
            + 24 * (ixSelectedHairStyle) + Offset.Y, DXCircle, @pr,
            DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
          PlotTextCentered(nil, txtMessage[20], 279, 279 + 123,
            239 + (ord(box) - 12) * 42 + 34, 240);
          for i := 0 to 0 do
            // SelectRect[ ( Box - 12 ) * 4 + i ].rect := rect( 279, 239 + ( box - 12 ) * 42 + 34 + 24 * i, 279 + 123, 239 + ( box - 12 ) * 42 + 34 + 24 + 24 * i );
            SelectRect[(ord(box) - 12) * 4 + i].enabled := True;
        end // End Rise of Dwarf
        else if modselection = TModSelection.DoA then // Days of Ahoul
        begin
          lpDDSBack.BltFast(279 + 13 + Offset.X, 239 + (ord(box) - 12) * 42 + 34
            + 24 * (ixselectedDoAHair) + Offset.Y, DXCircle, @pr,
            DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
          if AhoulRace > 1 then
            PlotTextCentered(nil, txtMessage[24], 279, 279 + 123,
              239 + (ord(box) - 12) * 42 + 34, 240)
          else
            PlotTextCentered(nil, txtMessage[20], 279, 279 + 123,
              239 + (ord(box) - 12) * 42 + 34, 240);
          PlotTextCentered(nil, txtMessage[21] + txtMessage[3], 279, 279 + 123,
            239 + (ord(box) - 12) * 42 + 58, 240);
          PlotTextCentered(nil, txtMessage[22], 279, 279 + 123,
            239 + (ord(box) - 12) * 42 + 82, 240);
          PlotTextCentered(nil, txtMessage[23], 279, 279 + 123,
            239 + (ord(box) - 12) * 42 + 106, 240);
          for i := 0 to 3 do
            // SelectRect[ ( Box - 12 ) * 4 + i ].rect := rect( 279, 239 + ( box - 12 ) * 42 + 34 + 24 * i, 279 + 123, 239 + ( box - 12 ) * 42 + 34 + 24 + 24 * i );
            SelectRect[(ord(box) - 12) * 4 + i].enabled := True;
        end
        else
        begin
          lpDDSBack.BltFast(279 + 13 + Offset.X, 239 + (ord(box) - 12) * 42 + 34
            + 24 * (ixSelectedHairStyle) + Offset.Y, DXCircle, @pr,
            DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
          PlotTextCentered(nil, txtMessage[20], 279, 279 + 123,
            239 + (ord(box) - 12) * 42 + 34, 240);
          PlotTextCentered(nil, txtMessage[21], 279, 279 + 123,
            239 + (ord(box) - 12) * 42 + 58, 240);
          PlotTextCentered(nil, txtMessage[22], 279, 279 + 123,
            239 + (ord(box) - 12) * 42 + 82, 240);
          PlotTextCentered(nil, txtMessage[23], 279, 279 + 123,
            239 + (ord(box) - 12) * 42 + 106, 240);
          for i := 0 to 3 do
            SelectRect[(ord(box) - 12) * 4 + i].enabled := True;
        end;
      end
      else if box = bxBeard then // beard
        if modselection <> TModSelection.DoA then
        begin
          pr := Rect(0, 0, 96, 21);
          lpDDSBack.BltFast(279 + 13 + Offset.X, 239 + (ord(box) - 12) * 42 + 34
            + 24 * (ixSelectedBeard) + 24 + Offset.Y, DXCircle, @pr,
            DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
          PlotTextCentered(nil, txtMessage[24], 279, 279 + 123,
            239 + (ord(box) - 12) * 42 + 58, 240);
          PlotTextCentered(nil, txtMessage[25], 279, 279 + 123,
            239 + (ord(box) - 12) * 42 + 82, 240);
          for i := 0 to 1 do
            SelectRect[(ord(box) - 12) * 4 + i].enabled := True;
        end;
    end; // ednif
    BoxOpen := box;
    SoAOS_DX_BltFront;
  except
    on E: Exception do
      Log.Log(FailName + E.Message);
  end;
end; // OpenBox

procedure TCreation.DrawNewPlayer;
const
  FailName: string = 'TCreation.DrawtheGuy';
var
  pr: TRect;
begin
  Log.DebugLog(FailName);
  try
    pr := Rect(110, 93, 264, 227);
    lpDDSBack.BltFast(pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr,
      DDBLTFAST_WAIT);
    SelectedShirt := shirt[ixSelectedShirt + 1];
    SelectedPants := pants[(ixSelectedPants) + 1];
    if modselection <> TModSelection.DoA then // Nicht Days of Ahoul
      SelectedHair := hair[(ixSelectedHair) + 1, (ixSelectedHairStyle) + 1,
        (ixSelectedBeard) + 1]
      // SelectedHair := hair[ ( ixSelectedHair - 8 ) + 1, ( ixSelectedHairStyle - 12 ) + 1, ( ixSelectedBeard - 16 ) + 1 ]
    else
    begin
      if (ixselectedDoAHair = 0) and (AhoulRace = 2) then
        SelectedHair := DoAHair[5] // Ahoulheadgreen, Ahoul
      else if (ixselectedDoAHair = 0) and (AhoulRace = 3) then
        SelectedHair := DoAHair[6] // Ahoulheadtan, Halfbreed
      else
        SelectedHair := DoAHair[(ixselectedDoAHair) + 1];
      SelectedTattoo := tattoo[(ixSelectedTattoo) + 1];
    end;
    if modselection = TModSelection.SoA then // SoA boots for female and male
      SelectedBoots := boots;
    if Assigned(OnDraw) then
      OnDraw(self);
    SoAOS_DX_BltFront;
  except
    on E: Exception do
      Log.Log(FailName + E.Message);
  end;
end;

// DrawTheGuy

procedure TCreation.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
const
  FailName: string = 'TCreation.FromMouseDown';
var
  pr, r1, r2: TRect;
begin
  Log.DebugLog(FailName);
  try
    r1 := ApplyOffset(Rect(318, 553, 359, 577));
    r2 := ApplyOffset(Rect(318, 510, 359, 537));
    if r1.Contains(point(X, Y)) or r2.Contains(point(X, Y)) then
    begin
      // clean up any dimmed text
      pr := Rect(114, 237, 261, 439);
      lpDDSBack.BltFast(pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr,
        DDBLTFAST_WAIT);
      // clean up after old boxes
      pr := Rect(279, 239, 402, 588);
      lpDDSBack.BltFast(pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr,
        DDBLTFAST_WAIT);
      pr := Rect(465, 59, 465 + 123, 59 + 181);
      lpDDSBack.BltFast(pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr,
        DDBLTFAST_WAIT);
      BoxOpen := bxNone;
    end;

  except
    on E: Exception do
      Log.Log(FailName + E.Message);
  end;

end; // FormMouseDown

procedure TCreation.FormMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
const
  FailName: string = 'TCreation.FormMouseMove';
var
  pr: TRect;
begin
  Log.DebugLog(FailName);
  try
    // Clean up continue and cancel
    pr := DlgRect.dlgNewContinueRect;
    lpDDSBack.BltFast(ContinueRect.Left, ContinueRect.Top, DXBack, @pr,
      DDBLTFAST_WAIT);
    if BoxOpen = bxNone then
    begin
      pr := DlgRect.dlgNewCancelRect;
      lpDDSBack.BltFast(CancelRect.Left, CancelRect.Top, DXBack, @pr,
        DDBLTFAST_WAIT);
    end;
    // clear text
    if ContinueRect.Contains(point(X, Y)) then
    begin // over continue
      // dont clear if over continue, we might have the you must enter name text up -kludgy
    end
    else
    begin
      if BoxOpen = bxTraining then
      begin
        pr := Rect(490, 239, 720, 500);
        lpDDSBack.BltFast(pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr,
          DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
      end
      else
      begin
        pr := Rect(490, 160, 720, 500);
        lpDDSBack.BltFast(pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr,
          DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
      end;
    end;
  except
    on E: Exception do
      Log.Log(FailName + E.Message);
  end;
end;

function TCreation.GetCancelRect: TRect;
begin
  Result := ApplyOffset(DlgRect.dlgNewCancelRect);
end;

function TCreation.GetContinueRect: TRect;
begin
  Result := ApplyOffset(DlgRect.dlgNewContinueRect);
end;

function TCreation.GetPlayerININame: string;
begin
  Result := ChangeFileExt(ExtractFileName(TCharacterResource(Character.Resource)
    .Filename), '');
end;

end.
