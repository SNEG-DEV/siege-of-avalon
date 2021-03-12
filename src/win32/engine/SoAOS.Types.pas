unit SoAOS.Types;

(*
  Siege Of Avalon : Open Source Edition

  Portions created by Steffen Nyeland are
  Copyright (C) 2019 - Steffen Nyeland.

  Portions created by Rucksacksepp are
  Copyright (C) 2019 - Rucksacksepp.

  Contributor(s):
  Steffen Nyeland
  Rucksacksepp from SoAmigos forum (most HD and FullHD metrics)

  You may retrieve the latest version of this file at:
  https://github.com/SteveNew/Siege-of-Avalon-Open-Source

  The contents of this file maybe used with permission, subject to
  the GNU Lesser General Public License Version 2.1 (the "License"); you may
  not use this file except in compliance with the License. You may
  obtain a copy of the License at https://opensource.org/licenses/LGPL-2.1

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  Description: SoAOS specific color const and types

  Requires: Delphi 10.3.3 or later

  Revision History:
  - 26 Mar 2019 - SN: Initial Commit to Git
  see git repo afterwards

*)

interface

uses
  System.SysUtils,
  System.UITypes,
  System.Types;

const
/// <summary> RGB color consts to improve readbility </summary>
  cTransparent     = TColors.Fuchsia;   // Fuchsia/Magenta transparent
  cInvisColor      = TColors.Cyan;   // Aqua/Cyan transparent
  cLoadBackColor   = $00108020;   // ??
  cLoadColor       = $00CDCDCD;   // ??
  cHealthColor     = $001F1F5F;
  cManaColor       = $00B09730;
  cTalkRedColor    = TColors.Red;  // Angry
  cTalkWhiteColor  = TColors.White;
  cTalkYellowColor = TColors.Yellow;
  cTalkBlackColor  = TColors.Black; // Clear text
  cTalkBlueColor   = TColors.Blue;  // Undead
  cBlackBackground = TColors.Black;

type
//  class function AppPath: string; static;

/// <summary> Screen resolution to improve readbility, names might change when I get a clue what is needed</summary>
  TScreenResolutionData = record
    ScreenWidth: integer;
    ScreenHeight: integer;
    BPP: integer;
    Windowed: Boolean;
    PreMapWidth: integer;
    PreMapHeight: integer;
    GameWidth: integer;
    GameHeight: integer;
    HearingRange: integer;
    SpellX : integer;
  	SpellBarX : integer;
   	SpellBarY : integer;
	  StatsX : integer;  // and inventorydlg?
   	StatsY : integer;
  	HelpBoxY : integer;
  	MouseMsgX : integer;
    PauseX: integer;
  	BottomBarX : integer;
  	NPCBarY : integer;
  	ManaEmptyX : integer;
  	LifeEmptyX : integer;
  	LogX : integer;
    VisibilityFactor: double;
    CharacterMysticVision: integer;
    CharacterDistance: integer;
    CharacterRange: integer;
    CharacterReach: integer;
    CompanionRange: integer;
    PartyMemberSlots: integer;
    AStarGridDim: Integer;
    spellbarFile: string;
    sidebarFile: string;
    bottombarFile: string;
//    MagicRange: integer;
    IniIdent: string;
    popInventoryRect: TRect;
    popMapRect: TRect;
    popQuestRect: TRect;
    popAdventureRect: TRect;
    popJournalRect: TRect;
    popAwardsRect: TRect;
    popMessageRect: TRect;
    popStatsRect: TRect;
    popManaRect: TRect;
    popHealthRect: TRect;
    popSpellRect: TRect;
    popRosterRect: TRect;
    popParty1Rect: TRect;
    popParty2Rect: TRect;
    popParty3Rect: TRect;
    popParty4Rect: TRect;
  end;

/// <summary> Dialog constants - since when no-one even bothered to use the same offset when doing multilanguage version.</summary>
  TDialogData = record
    dlgNewCancelRect: TRect;
    dlgNewContinueRect: TRect;
    dlgLoadSaveTitleRect: TRect;
    dlgLoadSaveCancelRect: TRect;
    dlgLoadSaveRect: TRect;
    dlgOptContinueRect: TRect;
  end;

const
  cOriginal : TScreenResolutionData =
    (ScreenWidth : 800;
     ScreenHeight : 600;
     BPP : 16;
     PreMapWidth: 768;
     PreMapHeight: 544;
     GameWidth : 703;
     GameHeight : 511;
     HearingRange : 400;
     SpellX : 339;
	   SpellBarX : 683;
	   SpellBarY : 486;
	   StatsX : 699;
	   StatsY : 498;
	   HelpBoxY : 455;
	   MouseMsgX : 394;
     PauseX : 456;
	   BottomBarX : 391;
	   NPCBarY : 581;
	   ManaEmptyX : 699;
	   LifeEmptyX : 709;
	   LogX : 659;
     VisibilityFactor : 1.0; // Visibility factor used on Vision property
     CharacterMysticVision: 500;  // was originally 300 but improved by Rucksacksepp
     CharacterDistance: 175;
     CharacterRange: 300;
     CharacterReach: 160;
     CompanionRange: 300;
     PartyMemberSlots: 4;
     spellbarFile: 'spellbar';
     sidebarFile: 'sidebar';
     bottombarFile: 'bottombar';
     IniIdent : 'Original';
     popInventoryRect: ( Left: 726; Top: 429; Right: 772; Bottom: 473 );
     popMapRect: ( Left: 732; Top: 511; Right: 781; Bottom: 555 );
     popQuestRect: ( Left: 666; Top: 511; Right: 715; Bottom: 531 );
     popAdventureRect: ( Left: 660; Top: 535; Right: 725; Bottom: 556 );
     popJournalRect: ( Left: 663; Top: 560; Right: 722; Bottom: 578 );
     popAwardsRect: ( Left: 609; Top: 543; Right: 647; Bottom: 583 );
     popMessageRect: ( Left: 392; Top: 517; Right: 591; Bottom: 582 );
     popStatsRect: ( Left: 715; Top: 10; Right: 778; Bottom: 104 );
     popManaRect: ( Left: 708; Top: 146; Right: 765; Bottom: 203 );
     popHealthRect: ( Left: 711; Top: 258; Right: 759; Bottom: 348 );
     popSpellRect: ( Left: 337; Top: 547; Right: 371; Bottom: 582 );
     popRosterRect: ( Left: 101; Top: 491; Right: 192; Bottom: 509 );
     popParty1Rect: ( Left: 3; Top: 510; Right: 65; Bottom: 586 );
     popParty2Rect: ( Left: 80; Top: 510; Right: 151; Bottom: 586 );
     popParty3Rect: ( Left: 166; Top: 510; Right: 233; Bottom: 586 );
     popParty4Rect: ( Left: 248; Top: 510; Right: 319; Bottom: 586 );
    );
  cHD : TScreenResolutionData =
    (ScreenWidth : 1280;
     ScreenHeight : 720;
     BPP : 16;
     PreMapWidth: 1280;
     PreMapHeight: 720;
     GameWidth : 1183;
     GameHeight : 631;
     HearingRange : 450;
     SpellX : 508;
  	 SpellBarX : 1163;  // ??
	   SpellBarY : 606;
	   StatsX : 1803;
	   StatsY : 966;
	   HelpBoxY : 575;
	   MouseMsgX : 566;
     PauseX : 628;
	   BottomBarX : 564;  //??
	   NPCBarY : 701;
   	 ManaEmptyX : 1179;
   	 LifeEmptyX : 1189;
	   LogX : 1139; // ??
     VisibilityFactor : 41/30; // Visibility factor used on Vision property 410/300
     CharacterMysticVision: 800;
     CharacterDistance: 375;
     CharacterRange: 650;
     CharacterReach: 180;
     CompanionRange: 650;
     PartyMemberSlots: 4;
     spellbarFile: 'spellbarHD';
     sidebarFile: 'sidebarHD';
     bottombarFile: 'bottombarHD';
     IniIdent : 'HD';
     popInventoryRect: ( Left: 1206; Top: 429; Right: 1254; Bottom: 473 );
     popMapRect: ( Left: 1212; Top: 631; Right: 1263; Bottom: 675 );
     popQuestRect: ( Left: 1148; Top: 631; Right: 1196; Bottom: 651 );
     popAdventureRect: ( Left: 1139; Top: 655; Right: 1205; Bottom: 676 );
     popJournalRect: ( Left: 1142; Top: 680; Right: 1206; Bottom: 698 );
     popAwardsRect: ( Left: 1088; Top: 663; Right: 1127; Bottom: 703 );
     popMessageRect: ( Left: 561; Top: 637; Right: 1072; Bottom: 702 );
     popStatsRect: ( Left: 1195; Top: 10; Right: 1258; Bottom: 104 );
     popManaRect: ( Left: 1188; Top: 146; Right: 1245; Bottom: 203 );
     popHealthRect: ( Left: 1191; Top: 258; Right: 1239; Bottom: 348 );
     popSpellRect: ( Left: 506; Top: 667; Right: 541; Bottom: 702 );
     popRosterRect: ( Left: 344; Top: 659; Right: 425; Bottom: 691 );
     popParty1Rect: ( Left: 3; Top: 630; Right: 65; Bottom: 706 );
     popParty2Rect: ( Left: 80; Top: 630; Right: 151; Bottom: 706 );
     popParty3Rect: ( Left: 166; Top: 630; Right: 233; Bottom: 706 );
     popParty4Rect: ( Left: 248; Top: 630; Right: 319; Bottom: 706 );
    );
  cFullHD : TScreenResolutionData =
    (ScreenWidth : 1920;
     ScreenHeight : 1080;
     BPP : 16;
     PreMapWidth: 1920;
     PreMapHeight: 1080;
     GameWidth : 1823;
     GameHeight : 997;  // 997?
     HearingRange : 525;
     SpellX : 508;
  	 SpellBarX : 1803;
	   SpellBarY : 966;
	   StatsX : 1803;
	   StatsY : 966;
	   HelpBoxY : 935;
	   MouseMsgX : 566;
     PauseX : 628;
	   BottomBarX : 564;
	   NPCBarY : 1061;
	   ManaEmptyX : 1819;
	   LifeEmptyX : 1829;
	   LogX : 1139;
     VisibilityFactor : 9/4; // Visibility factor used on Vision property 675/300
     CharacterMysticVision: 1000;
     CharacterDistance: 775;
     CharacterRange: 950;
     CharacterReach: 200;
     CompanionRange: 900;
     PartyMemberSlots: 4;
     spellbarFile: 'spellbarFullHD';
     sidebarFile: 'sidebarFullHD';
     bottombarFile: 'bottombarFullHD';
     IniIdent : 'FullHD';
     popInventoryRect: ( Left: 1846; Top: 885; Right: 1894; Bottom: 931 );
     popMapRect: ( Left: 1852; Top: 991; Right: 1903; Bottom: 1035 );
     popQuestRect: ( Left: 1148; Top: 991; Right: 1196; Bottom: 1011 );
     popAdventureRect: ( Left: 1139; Top: 1015; Right: 1205; Bottom: 1036 );
     popJournalRect: ( Left: 1142; Top: 1040; Right: 1206; Bottom: 1058 );
     popAwardsRect: ( Left: 1088; Top: 1023; Right: 1127; Bottom: 1063 );
     popMessageRect: ( Left: 561; Top: 997; Right: 1072; Bottom: 1062 );
     popStatsRect: ( Left: 1835; Top: 10; Right: 1898; Bottom: 104 );
     popManaRect: ( Left: 1828; Top: 146; Right: 1885; Bottom: 203 );
     popHealthRect: ( Left: 1831; Top: 258; Right: 1879; Bottom: 348 );
     popSpellRect: ( Left: 506; Top: 1027; Right: 541; Bottom: 1062 );
     popRosterRect: ( Left: 344; Top: 1019; Right: 425; Bottom: 1051 );
     popParty1Rect: ( Left: 3; Top: 990; Right: 65; Bottom: 1066 );
     popParty2Rect: ( Left: 80; Top: 990; Right: 151; Bottom: 1066 );
     popParty3Rect: ( Left: 166; Top: 990; Right: 233; Bottom: 1066 );
     popParty4Rect: ( Left: 248; Top: 990; Right: 319; Bottom: 1066 );
    );

const
  cClassicDialogs : TDialogData =
    (dlgNewCancelRect: ( Left: 102; Top: 450; Right: 102 + 155; Bottom: 450 + 44 );
     dlgNewContinueRect: ( Left: 700 - 202; Top: 450; Right: 700; Bottom: 450 + 45 );  // RightAligend
     dlgLoadSaveTitleRect: ( Left: 93; Top: 12; Right: 93 + 111; Bottom: 12 + 53 );
     dlgLoadSaveCancelRect: ( Left: 95; Top: 443; Right: 95 + 165; Bottom: 443 + 58 );
     dlgLoadSaveRect: ( Left: 701 - 121; Top: 445; Right: 701; Bottom: 445 + 54 );  // RightAligned
     dlgOptContinueRect: ( Left: 700 - 198; Top: 450; Right: 700; Bottom: 450 + 45 );  // RightAligend
    );
  cMultilingualDialogs : TDialogData =
    (dlgNewCancelRect: ( Left: 100; Top: 449; Right: 100 + 300; Bottom: 449 + 45 );
     dlgNewContinueRect: ( Left: 700 - 300; Top: 449; Right: 700; Bottom: 449 + 45 );  // RightAligend
     dlgLoadSaveTitleRect: ( Left: 94; Top: 19; Right: 94 + 263; Bottom: 19 + 45 );
     dlgLoadSaveCancelRect: ( Left: 100; Top: 450; Right: 100 + 300; Bottom: 450 + 45 );
     dlgLoadSaveRect: ( Left: 700 - 300; Top: 450; Right: 700; Bottom: 450 + 45 );  // RightAligned
     dlgOptContinueRect: ( Left: 700 - 300; Top: 450; Right: 700; Bottom: 450 + 45 );  // RightAligend
    );

type
  TFacing = ( fNW, fNN, fNE, fEE, fSE, fSS, fSW, fWW );

  TFacingHelper = record helper for TFacing
    function ToString : string;
  end;

  TInformationRect = record
  private
    FRect: TRect;
//    class operator Implicit(const Value: TRect): TInformationRect;
//    class operator Implicit(const Value: TInformationRect): TRect;
  public
    Info: string;
    function Contains(const Pt: TPoint): Boolean;
    procedure Offset(const Point: TPoint);
    property Left: Integer read FRect.Left write FRect.Left;
    property Top: Integer read FRect.Top write FRect.Top;
    property Right: Integer read FRect.Right write FRect.Right;
    property Bottom: Integer read FRect.Bottom write FRect.Bottom;
  end;

  TSelectionRect = record
  private
    FRect: TRect;
//    class operator Implicit(const Value: TRect): TSelectionRect;
//    class operator Implicit(const Value: TSelectionRect): TRect;
  public
    Info: string;
    Text: string;
    Enabled: Boolean;
    function Contains(const Pt: TPoint): Boolean;
    procedure Offset(const Point: TPoint);
    property Left: Integer read FRect.Left write FRect.Left;
    property Top: Integer read FRect.Top write FRect.Top;
    property Right: Integer read FRect.Right write FRect.Right;
    property Bottom: Integer read FRect.Bottom write FRect.Bottom;
  end;

  function InformationRect(ALeft, ATop, ARight, ABottom: Integer; AInfo: string): TInformationRect;
  function SelectionRect(ALeft, ATop, ARight, ABottom: Integer; AInfo, AText: string): TSelectionRect;

implementation

{ TFacingHelper }

function TFacingHelper.ToString: string;
begin
  case self of
    fNW: result := 'NW';
    fNN: result := 'NN';
    fNE: result := 'NE';
    fEE: result := 'EE';
    fSE: result := 'SE';
    fSS: result := 'SS';
    fSW: result := 'SW';
    fWW: result := 'WW';
  end;
end;

{ TInformationRect }

function TInformationRect.Contains(const Pt: TPoint): Boolean;
begin
  Result := FRect.Contains(Pt);
end;

procedure TInformationRect.Offset(const Point: TPoint);
begin
  FRect.TopLeft.Offset(Point);
  FRect.BottomRight.Offset(Point);
end;

function InformationRect(ALeft, ATop, ARight, ABottom: Integer; AInfo: string): TInformationRect;
begin
  Result.FRect.Left := ALeft;
  Result.FRect.Top := ATop;
  Result.FRect.Right := ARight;
  Result.FRect.Bottom := ABottom;
  Result.Info := AInfo;
end;

{ TSelectionRect }

function TSelectionRect.Contains(const Pt: TPoint): Boolean;
begin
  Result := Enabled and FRect.Contains(Pt);
end;

//class operator TSelectionRect.Implicit(const Value: TRect): TSelectionRect;
//begin
//  Result.FRect := Value;
//end;
//
//class operator TSelectionRect.Implicit(const Value: TSelectionRect): TRect;
//begin
//  Result := Value.FRect;
//end;

procedure TSelectionRect.Offset(const Point: TPoint);
begin
  FRect.TopLeft.Offset(Point);
  FRect.BottomRight.Offset(Point);
end;

function SelectionRect(ALeft, ATop, ARight, ABottom: Integer; AInfo, AText: string): TSelectionRect;
begin
  Result.FRect.Left := ALeft;
  Result.FRect.Top := ATop;
  Result.FRect.Right := ARight;
  Result.FRect.Bottom := ABottom;
  Result.Info := AInfo;
  Result.Text := AText;
  Result.Enabled := False;
end;

end.
