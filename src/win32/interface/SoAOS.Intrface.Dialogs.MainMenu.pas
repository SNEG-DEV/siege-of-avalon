unit SoAOS.Intrface.Dialogs.MainMenu;
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

  Description: Main Menu Dialog - was Intro.pas - a lot more clean-up is coming

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
  DXUtil,
  DXEffects, //Dim Modbox
  Winapi.Windows,
  System.SysUtils,
  System.Types,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  SoAOS.Types,
  SoAOS.Intrface.Dialogs,
  GameText,
  Engine,
  music,
  SoAOS.Animation,
  LogFile;

type
  TIntroRect = record
    Rect: TRect;
    Image: IDirectDrawSurface;
    Enabled: boolean;
  end;

  TIntro = class(TDialog)
  private
    AreYouSureBoxVisible: boolean;
    SelectModBoxNewVisible: boolean; // Mod selection box, Newgame
    SelectModBoxLoadVisible: boolean; // Mod selection box, Loadgame
    DXBack: IDirectDrawSurface;
    DXModSelect: IDirectDrawSurface; // Mod selection box
    DXModRand: IDirectDrawSurface; // Mod selection, rectangle
    DXModDim: IDirectDrawSurface; // Dim Mod if not installed
    DXHelp: IDirectDrawSurface; // HelpScreen
    HelpscreenShow: boolean;
    Story: TModSelection; // Value for modselection after pressing OK or BACK
    PrevChoice: integer;
    txtMessage: array [0 .. 1] of string;
    procedure AreYouSure;
    procedure SelectMod; // Choose Mod
    procedure SetMod;
    procedure NotSetMod;
    procedure ShowHelp; // Show Helpscreen in Mainmenu
    function GetNoRect: TRect;
    function GetYesRect: TRect;
  protected
    procedure MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y, GridX, GridY: integer); override;
    procedure MouseMove(Sender: TObject; Shift: TShiftState;
      X, Y, GridX, GridY: integer); override;
    procedure KeyDown(Sender: TObject; var key: Word;
      Shift: TShiftState); override;
  public
    Captions: array [1 .. 8] of TIntroRect;
    MenuChoice: integer;
    modloadneeded: boolean; // Anidemo.pas needs access to it. If mod get changed ->true
    constructor Create;
    destructor Destroy; override;
    procedure Init; override;
    procedure Release; override;
    property YesRect: TRect read GetYesRect;
    property NoRect: TRect read GetNoRect;
  end;

implementation

uses
  SoAOS.Intrface.Text,
  SoAOS.Graphics.Draw,
  AniDemo;

const
  XFrame = 106;

procedure MakeRect(var Caption: TIntroRect; X, Y, YOffset: integer;
  BM: TBitmap);
const
  FailName: string = 'Intro.MakeRect';
  W = 582;
  H = 52;
var
  DC: HDC;
begin
  Log.DebugLog(FailName);
  try
    Caption.Rect := Rect(X, Y, X + W, Y + H);
    Caption.Image := DDGetSurface(lpDD, W, H, BM.Canvas.Pixels[1, 1], true);
    // The asset released has a "color" issue - this hack solves it
    Caption.Image.GetDC(DC);
    try
      BitBlt(DC, 0, 0, W, H, BM.Canvas.handle, 0, Y - YOffset, SRCCOPY);
    finally
      Caption.Image.ReleaseDC(DC);
    end;
  except
    on E: Exception do
      Log.Log(FailName, E.Message, []);
  end;
end;

procedure MakeOldRect(var Caption: TIntroRect; X, Y: integer; BM: TBitmap);
const
  FailName: string = 'Intro.MakeOldRect';
var
  DC: HDC;
  W, H: integer;
begin
  Log.DebugLog(FailName);
  W := BM.Width;
  H := BM.Height;
  try
    Caption.Rect := Rect(X, Y, X + W, Y + H);
    Caption.Image := DDGetSurface(lpDD, W, H, cTransparent, true);
    Caption.Image.GetDC(DC);
    try
      BitBlt(DC, 0, 0, W, H, BM.Canvas.handle, 0, 0, SRCCOPY);
    finally
      Caption.Image.ReleaseDC(DC);
    end;
  except
    on E: Exception do
      Log.Log(FailName, E.Message, []);
  end;
end;

{ TIntro }

constructor TIntro.Create;
const
  FailName: string = 'TIntro.Create';
begin
  Log.DebugLog(FailName);
  try
    inherited;
  except
    on E: Exception do
      Log.Log(FailName, E.Message, []);
  end;
end; // Create

destructor TIntro.Destroy;
const
  FailName: string = 'TIntro.Destroy';
begin
  Log.DebugLog(FailName);
  try
    inherited;
  except
    on E: Exception do
      Log.Log(FailName, E.Message, []);
  end;
end;

function TIntro.GetNoRect: TRect;
var
  Y: integer;
begin
  // Dynamic due to Russian menu variation.
  Y := Captions[7].Rect.Top + 110;
  Result := ApplyOffset(Rect(437, Y, 489, Y + 31));
end;

function TIntro.GetYesRect: TRect;
var
  Y: integer;
begin
  // Dynamic due to Russian menu variation.
  Y := Captions[7].Rect.Top + 110;
  Result := ApplyOffset(Rect(303, Y, 355, Y + 31));
end;

procedure TIntro.Init;
var
  BM: TBitmap;
  DC: HDC;
  Y1, YWidth, YOffset: integer;
  pr: TRect;
const
  FailName: string = 'TIntro.Init';
begin
  // Does calculate an offset based on screen vs menu bitmap - but might move to screenMetrics
  // For now are the caption rects in SD coords
  Log.DebugLog(FailName);
  try
    if Loaded then
      Exit;
    inherited;
    ExText.Open('Intro');
    txtMessage[0] := ExText.GetText('Message' + inttostr(0));

    PrevChoice := 0;
    AreYouSureBoxVisible := false;
    SelectModBoxNewVisible := false;
    SelectModBoxLoadVisible := false;
    HelpscreenShow := false;
    pText.LoadFontGraphic('createchar');

    BM := TBitmap.Create;
    try
      DXBack := SoAOS_DX_LoadBMP(InterfaceLanguagePath + 'gMainMenuBlank.bmp',
        cInvisColor, DlgWidth, DlgHeight);
      BM.LoadFromFile(InterfaceLanguagePath + 'gMainMenuText.bmp');
      DXBack.GetDC(DC);
      try
        if Language = 'russian' then
          BitBlt(DC, 106, 31, 582, 440, BM.Canvas.handle, 0, 0, SRCCOPY)
        else
          BitBlt(DC, 106, 41, 582, 416, BM.Canvas.handle, 0, 0, SRCCOPY);
      finally
        DXBack.ReleaseDC(DC);
      end;

      if ScreenMetrics.borderFile <> '' then
        lpDDSBack.BltFast(0, 0, frmMain.FillBorder, nil,
          DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
      pr := Rect(0, 0, DlgWidth, DlgHeight);
      lpDDSBack.BltFast(Offset.X, Offset.Y, DXBack, @pr, DDBLTFAST_NOCOLORKEY or
        DDBLTFAST_WAIT);

      BM.LoadFromFile(InterfaceLanguagePath + 'gMainMenuTextBttns.bmp');
      if Language = 'russian' then
      begin
        YWidth := 55;
        YOffset := 30;
      end
      else
      begin
        YWidth := 52;
        YOffset := 41;
      end;
      Y1 := YOffset;
      MakeRect(Captions[1], XFrame, Y1, YOffset, BM); // New game
      inc(Y1, YWidth);
      MakeRect(Captions[2], XFrame, Y1, YOffset, BM); // Load
      inc(Y1, YWidth);
      MakeRect(Captions[3], XFrame, Y1, YOffset, BM); // Save
      inc(Y1, YWidth);
      MakeRect(Captions[4], XFrame, Y1, YOffset, BM); // Options
      inc(Y1, YWidth);
      MakeRect(Captions[5], XFrame, Y1, YOffset, BM); // History
      inc(Y1, YWidth);
      MakeRect(Captions[6], XFrame, Y1, YOffset, BM); // Credits
      inc(Y1, YWidth);
      MakeRect(Captions[7], XFrame, Y1, YOffset, BM); // Exit
      inc(Y1, YWidth);
      MakeRect(Captions[8], XFrame, Y1, YOffset, BM); // Resume

    finally
      BM.Free;
    end;

    SoAOS_DX_BltFront;
  except
    on E: Exception do
      Log.Log(FailName, E.Message, []);
  end;
end; // Init

procedure TIntro.KeyDown(Sender: TObject; var key: Word; Shift: TShiftState);
var
  pr: TRect;
begin
  if HelpscreenShow then
  begin
    if key = 27 then //esc
      HelpscreenShow := false;
    pr := Rect(0, 0, DlgWidth, DlgHeight);
    lpDDSBack.BltFast(Offset.X, Offset.Y, DXBack, @pr, DDBLTFAST_WAIT);
    // clear screen
    SoAOS_DX_BltFront;
  end;
  if AreYouSureBoxVisible then
  begin
    if key = 13 then
    begin
      MenuChoice := 7;
      Close;
    end;
    if key = 27 then
    begin
      AreYouSureBoxVisible := false;
      pr := Rect(0, 0, DlgWidth, DlgHeight);
      lpDDSBack.BltFast(Offset.X, Offset.Y, DXBack, @pr, DDBLTFAST_WAIT);
      // clear screen
      SoAOS_DX_BltFront;
    end;
  end;
end;

procedure TIntro.MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y, GridX, GridY: integer);
var
  i, j: integer;
  pr: TRect;
  pr1, pr2: TRect;
const
  k: integer = 19; // moving constant (vertical) for Modselection rectangle
  FailName: string = 'TIntro.MouseDown';
begin
  Log.DebugLog(FailName);
  try
    MenuChoice := 0;
    // Rectangles for mod selection
    pr1 := Rect(0, 0, 123, 220); // Modbox
    pr2 := Rect(0, 0, 100, 17); // Modselect
    // Show Helpscreen/Controls
    if PtInRect(ApplyOffset(Rect(590, 470, 700, 495)), point(X, Y)) and
      (AreYouSureBoxVisible = false) and (SelectModBoxNewVisible = false) and
      (SelectModBoxLoadVisible = false) then
      ShowHelp;
    if (AreYouSureBoxVisible = false) and (HelpscreenShow = false) and
      (SelectModBoxNewVisible = false) and (SelectModBoxLoadVisible = false)
    then
    begin
      // Compensate for higher resolution against caption rects
      Dec(X, Offset.X);
      Dec(Y, Offset.Y);
      for i := 1 to 8 do
      begin
        if Captions[i].Enabled and Captions[i].Rect.Contains(point(X, Y)) then
        begin
          MenuChoice := i;
          Break;
        end;
      end;
      if (MenuChoice = 1) and (Modallowed) then
      begin
        SelectMod;
        if (Story > TModSelection.TSK) or (Story < TModSelection.SoA) then
        // Just to be sure
          Story := TModSelection.SoA;
      end
      else if (MenuChoice = 2) and (Modallowed) then
      begin
        SelectMod;
        if (Story > TModSelection.TSK) or (Story < TModSelection.SoA) then
        // Just to be sure
          Story := TModSelection.SoA;
      end
      else if MenuChoice = 7 then
      begin // they chose exit- display AreYouSure Box
        AreYouSure;
      end
      else if MenuChoice > 0 then
      begin
        Close;
      end;
    end;
    if AreYouSureBoxVisible = true then
    begin // check for clicks on Yes/No in AreYouSurebox
      if YesRect.Contains(point(X, Y)) then
      begin // Yes pressed- quit game
        MenuChoice := 7;
        Close;
      end
      else if NoRect.Contains(point(X, Y)) then
      begin // No pressed- just show screen
        AreYouSureBoxVisible := false;
        pr := Rect(0, 0, DlgWidth, DlgHeight);
        lpDDSBack.BltFast(Offset.X, Offset.Y, DXBack, @pr, DDBLTFAST_WAIT);
        // clear screen
        SoAOS_DX_BltFront;
      end; // endif PtInRect
    end;
    if (SelectModBoxNewVisible = true) or (SelectModBoxLoadVisible = true) then
    begin
      if SelectModBoxNewVisible = true then // New game
      j := 0
      else //Load game
      j := 56;
      if PtInRect(ApplyOffset(Rect(350, 107 + j, 450, 238 + j)), point(X, Y)) then
      //the whole Modbox field
      begin
        for i := 0 to 6 do
        begin
          if PtInRect(ApplyOffset(Rect(350, 107 + j + k * i, 450, 124 + j + k * i)), point(X, Y)) then
          begin
            if (i = 0) and (fileexists(AppPath + 'kingdoms.ini')) then
            begin
              Story := TModSelection.TSK;
              break;
            end;
            if (i = 1) and (fileexists(AppPath + 'siege.ini')) then
            begin
              Story := TModSelection.SoA;
              break;
            end;
            if (i = 2) and (fileexists(AppPath + 'days.ini')) then
            begin
              Story := TModSelection.DoA;
              break;
            end;
            if (i = 3) and (fileexists(AppPath + 'pillars.ini')) then
            begin
              Story := TModSelection.PoA;
              break;
            end;
            if (i = 4) and (fileexists(AppPath + 'ashes.ini')) then
            begin
              Story := TModSelection.AoA;
              break;
              end;
            if (i = 5) and (fileexists(AppPath + 'caves.ini')) then
            begin
              Story := TModSelection.Caves;
              break;
            end;
            if (i = 6) and (fileexists(AppPath + 'rise.ini')) then
            begin
              Story := TModSelection.RoD;
              // Seven Kingdoms, on top of the list, but is also modselection 7
              // log.log ('Schegichte=' + inttostr(story));
              break;
            end;
          end;
        end;
        if i < 7 then //if forloop doesn't get break-command, i -> 7
        begin
        lpDDSBack.BltFast(400 - 61 + Offset.X, 81 + j + Offset.Y, DXModSelect,
          @pr1, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
        lpDDSBack.BltFast(350 + Offset.X, 107 + j + i * k + Offset.Y,
          DXModRand, @pr2, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
        SoAOS_DX_BltFront;
        end;
      end;
      if PtInRect(ApplyOffset(Rect(407, 266 + j, 462, 294 + j)), point(X, Y))
      then
      begin // Mod selected, -> OK
        SetMod;
        if SelectModBoxNewVisible then
          MenuChoice := 1
        else
          MenuChoice := 2;
        DXModSelect := nil;
        Close;
      end
      else if PtInRect(ApplyOffset(Rect(343, 266 + j, 388, 294 + j)),
        point(X, Y)) then
      begin // BACK
        NotSetMod;
      end;
    end;
  except
    on E: Exception do
      Log.Log(FailName, E.Message, []);
  end;
end;

procedure TIntro.MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y, GridX, GridY: integer);
var
  Choice: integer;
  i: integer;
  pr: TRect;
const
  FailName: string = 'TIntro.MouseMove';
begin
  Log.DebugLog(FailName);
  try
    Choice := 0;
    if (AreYouSureBoxVisible = false) and (HelpscreenShow = false) and
      (SelectModBoxNewVisible = false) and (SelectModBoxLoadVisible = false)
    then
    begin
      // Compensate for higher resolution against caption rects
      Dec(X, Offset.X);
      Dec(Y, Offset.Y);
      for i := 1 to 8 do
      begin
        if Captions[i].Enabled and Captions[i].Rect.Contains(point(X, Y)) then
        begin
          Choice := i;
          if Choice <> PrevChoice then
          begin
            if ScreenMetrics.borderFile <> '' then
              lpDDSBack.BltFast(0, 0, frmMain.FillBorder, nil,
                DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
            pr := Rect(0, 0, DlgWidth, DlgHeight);
            lpDDSBack.BltFast(Offset.X, Offset.Y, DXBack, @pr,
              DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT);
            pr := Rect(0, 0, Captions[i].Rect.Width, Captions[i].Rect.Height);
            lpDDSBack.BltFast(Offset.X + Captions[i].Rect.Left,
              Offset.Y + Captions[i].Rect.Top, Captions[i].Image, @pr,
              DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
            lpDDSFront_Flip(nil, DDFLIP_WAIT);
            MouseCursor.PlotDirty := false;
          end;
          Break;
        end;
      end;

      if (Choice = 0) and (Choice <> PrevChoice) then
      begin
        if ScreenMetrics.borderFile <> '' then
          lpDDSBack.BltFast(0, 0, frmMain.FillBorder, nil,
            DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
        pr := Rect(0, 0, DlgWidth, DlgHeight);
        lpDDSBack.BltFast(Offset.X, Offset.Y, DXBack, @pr,
          DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT);
        lpDDSFront_Flip(nil, DDFLIP_WAIT);
        MouseCursor.PlotDirty := false;
      end;
      PrevChoice := Choice;
    end;

  except
    on E: Exception do
      Log.Log(FailName, E.Message, []);
  end;
end;

procedure TIntro.AreYouSure;
var
  Width, Height: integer;
  DXBorders: IDirectDrawSurface;
  nRect: TRect;
  pr: TRect;
const
  FailName: string = 'TIntro.AreYouSure ';
begin

  Log.DebugLog(FailName);
  try
    SelectModBoxNewVisible := false;
    SelectModBoxLoadVisible := false;
    HelpscreenShow := false;
    DXBorders := SoAOS_DX_LoadBMP(InterfaceLanguagePath + 'ldChooseBox.bmp',
      cInvisColor, Width, Height);
    nRect := Captions[7].Rect; // Exit

    pr := Rect(0, 0, DlgWidth, DlgHeight);
    lpDDSBack.BltFast(Offset.X, Offset.Y, DXBack, @pr, DDBLTFAST_WAIT);
    pr := Rect(0, 0, Width, Height);
    lpDDSBack.BltFast((ScreenMetrics.ScreenWidth - Width) div 2,
      nRect.Top + 32 + Offset.Y, DXBorders, @pr, DDBLTFAST_SRCCOLORKEY or
      DDBLTFAST_WAIT);

    DXBorders := nil;

    pText.PlotTextBlock(txtMessage[0], (ScreenMetrics.ScreenWidth - Width) div 2
      + 23, (ScreenMetrics.ScreenWidth - Width) div 2 + 281,
      nRect.Top + 52 + Offset.Y, 240);

    AreYouSureBoxVisible := true;
    SoAOS_DX_BltFront;
  except
    on E: Exception do
      Log.Log(FailName, E.Message, []);
  end;
end; // AreYouSure

procedure TIntro.SelectMod; // Mod selection
var
  width1, height1: integer;
  width2, height2: integer;
  width3, height3: integer;
  pr1, pr2, pr3: TRect;
  i, j: integer;
const
  FailName: string = 'TIntro.SelectMod ';
begin
  Log.DebugLog(FailName);
  try
    if MenuChoice = 2 then //Load game
    begin
      SelectModBoxNewVisible := false;
      SelectModBoxLoadVisible := true;
      j := 56;
    end
    else //MenuChoice = 1, New game
    begin
      SelectModBoxNewVisible := true;
      SelectModBoxLoadVisible := false;
      j := 0;
    end;
    AreYouSureBoxVisible := false;
    HelpscreenShow := false;
    DXModRand := SoAOS_DX_LoadBMP(InterfacePath + 'ModSelect.bmp', cInvisColor,
      width1, height1);
    DXModSelect := SoAOS_DX_LoadBMP(InterfacePath + 'ModBox.bmp', cInvisColor,
      width2, height2);
    DXModDim := SoAOS_DX_LoadBMP(InterfacePath + 'Chablack.bmp', cInvisColor,
      width3, height3);
    //First need to dim Modbox(=DXModselect) if a mod isn't installed
    for i := 0 to 6 do
    begin
    if (i = 0) and not (fileexists(AppPath + 'kingdoms.ini')) then
    Drawalpha(DXModSelect, rect(11, 26 + 19*i, 11 + width1, 26 + 19*i + height1), Rect(0, 0, width3, height3), DXModDim, true, 192);
    if (i = 1) and not (fileexists(AppPath + 'siege.ini')) then
    Drawalpha(DXModSelect, rect(11, 26 + 19*i, 11 + width1, 26 + 19*i + height1), Rect(0, 0, width3, height3), DXModDim, true, 192);
    if (i = 2) and not (fileexists(AppPath + 'days.ini')) then
    Drawalpha(DXModSelect, rect(11, 26 + 19*i, 11 + width1, 26 + 19*i + height1), Rect(0, 0, width3, height3), DXModDim, true, 192);
    if (i = 3) and not (fileexists(AppPath + 'pillars.ini')) then
    Drawalpha(DXModSelect, rect(11, 26 + 19*i, 11 + width1, 26 + 19*i + height1), Rect(0, 0, width3, height3), DXModDim, true, 192);
    if (i = 4) and not (fileexists(AppPath + 'ashes.ini')) then
    Drawalpha(DXModSelect, rect(11, 26 + 19*i, 11 + width1, 26 + 19*i + height1), Rect(0, 0, width3, height3), DXModDim, true, 192);
    if (i = 5) and not (fileexists(AppPath + 'caves.ini')) then
    Drawalpha(DXModSelect, rect(11, 26 + 19*i, 11 + width1, 26 + 19*i + height1), Rect(0, 0, width3, height3), DXModDim, true, 192);
    if (i = 6) and not (fileexists(AppPath + 'rise.ini')) then
    Drawalpha(DXModSelect, rect(11, 26 + 19*i, 11 + width1, 26 + 19*i + height1), Rect(0, 0, width3, height3), DXModDim, true, 192);
    end;
    pr1 := Rect(0, 0, DlgWidth, DlgHeight);
    lpDDSBack.BltFast(Offset.X, Offset.Y, DXBack, @pr1, DDBLTFAST_WAIT);
    pr2 := Rect(0, 0, width2, height2);
    lpDDSBack.BltFast(400 - 61 + Offset.X, 81 + j + Offset.Y, DXModSelect, @pr2,
      DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
    pr3 := Rect(0, 0, width1, height1);
    if modselection = TModSelection.TSK then
      lpDDSBack.BltFast(350 + Offset.X, 107 + j + Offset.Y, DXModRand, @pr3,
        DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT)
    else if (modselection > TModSelection.TSK) or (modselection < TModSelection.SoA) then
      lpDDSBack.BltFast(350 + Offset.X, 107 + j + 19 + Offset.Y, DXModRand, @pr3,
        DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT)
    else
      lpDDSBack.BltFast(350 + Offset.X, 107 + j + 19 * (ord(modselection)+1) + Offset.Y,
        DXModRand, @pr3, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
    // + 19 because SoA on the second place of the selectionscreen
    SoAOS_DX_BltFront;
  except
    on E: Exception do
      Log.Log(FailName, E.Message, []);
  end;
end;

procedure TIntro.SetMod;
const
  FailName: string = 'TIntro.SetMod ';
begin
  Log.DebugLog(FailName);
  try
    //check if modselection is changed
    if modselection <> story then
      modloadneeded := true
    else
      modloadneeded := false;
    modselection := Story;
    Case modselection of
    TModSelection.SoA:
    begin
      Modname := 'Siege';
      Modgames := 'games';
      Modmaps := 'maps';
    end;
    TModSelection.DoA:
    begin
      Modname := 'Days';
      Modgames := 'daysgames';
      Modmaps := 'daysmaps';
    end;
    TModSelection.PoA:
    begin
      Modname := 'Pillars';
      Modgames := 'pillarsgames';
      Modmaps := 'pillarsmaps';
    end;
    TModSelection.AoA:
    begin
      Modname := 'Ashes';
      Modgames := 'ashesgames';
      Modmaps := 'ashesmaps';
    end;
    TModSelection.Caves:
    begin
      Modname := 'Caves';
      Modgames := 'cavesgames';
      Modmaps := 'cavesmaps';
    end;
    TModSelection.RoD:
    begin
      Modname := 'Rise';
      Modgames := 'risegames';
      Modmaps := 'risemaps';
    end;
    TModSelection.TSK:
    begin
      Modname := 'Kingdoms';
      Modgames := 'kingdomsgames';
      Modmaps := 'kingdomsmaps';
    end;
  end;
  except
    on E: Exception do
      Log.Log(FailName, E.Message, []);
  end;
end;

procedure TIntro.NotSetMod;
var
pr: Trect;
const
  FailName: string = 'TIntro.NotSetMod ';
begin
  Log.DebugLog(FailName);
  try
    pr := ApplyOffset(Rect(0, 0, 800, 600)); // Mainmenu
    SelectModBoxNewVisible := false;
    SelectModBoxLoadVisible := false;
    Story := modselection; // Upside down because nothing should change
    lpDDSBack.BltFast(0, 0, DXBack, @pr, DDBLTFAST_WAIT); // clear screen
    SoAOS_DX_BltFront;
    except
    on E: Exception do
      Log.Log(FailName, E.Message, []);
  end;
end;

procedure TIntro.ShowHelp;
var
  pr: TRect;
  width1, height1: integer;
const
  FailName: string = 'TIntro.Showhelp ';
begin
  Log.DebugLog(FailName);
  try
    SelectModBoxNewVisible := false;
    SelectModBoxLoadVisible := false;
    AreYouSureBoxVisible := false;
    DXHelp := SoAOS_DX_LoadBMP(InterfaceLanguagePath + 'Helpscreen.bmp',
      cInvisColor, width1, height1);
    pr := Rect(0, 0, width1, height1);
    lpDDSBack.BltFast(Offset.X, Offset.Y, DXHelp, @pr, DDBLTFAST_SRCCOLORKEY or
      DDBLTFAST_WAIT);
    HelpscreenShow := true;
    SoAOS_DX_BltFront;
  except
    on E: Exception do
      Log.Log(FailName, E.Message, []);
  end;
end;

procedure TIntro.Release;
var
  i: integer;
const
  FailName: string = 'TIntro.Release';
begin
  Log.DebugLog(FailName);
  try
    ExText.Close;
    for i := 1 to 8 do
    begin
      Captions[i].Image := nil;
    end;

    inherited;
  except
    on E: Exception do
      Log.Log(FailName, E.Message, []);
  end;
end;

end.
