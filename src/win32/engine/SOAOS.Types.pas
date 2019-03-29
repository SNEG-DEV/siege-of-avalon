unit SoAOS.Types;

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

type
//  class function AppPath: string; static;

/// <summary> Screen resolution to improve readbility, names might change when I get a clue what is needed</summary>
  TScreenResolutionData = record
    ScreeenWidth: integer;
    ScreenHeight: integer;
    BPP: integer;
    PreMapWidth: integer;
    PreMapHeight: integer;
    GameWidth: integer;
    GameHeight: integer;
    GameMapWidth: integer;
    GameMapHeight: integer;
    Visibility: integer;
//    MagicRange: integer;
    IniIdent: string;
  end;

const
  cOriginal : TScreenResolutionData =
    (ScreeenWidth : 800;
     ScreenHeight : 600;
     BPP : 16;
     PreMapWidth: 768;
     PreMapHeight: 544;
     GameWidth : 703;
     GameHeight : 511;
     GameMapWidth : 200;
     GameMapHeight : 400;
     Visibility : 300; // ??
     IniIdent : 'Original';
    );
  cHD : TScreenResolutionData =
    (ScreeenWidth : 1280;
     ScreenHeight : 720;
     BPP : 16;
     PreMapWidth: 1280;
     PreMapHeight: 720;
     GameWidth : 1183;
     GameHeight : 631;
     GameMapWidth : 200;  // ??
     GameMapHeight : 400; // ??
     Visibility : 300;  //??
     IniIdent : 'HD';
    );
  cFullHD : TScreenResolutionData =
    (ScreeenWidth : 1920;
     ScreenHeight : 1080;
     BPP : 16;
     PreMapWidth: 1920;
     PreMapHeight: 1080;
     GameWidth : 1823;
     GameHeight : 991;
     GameMapWidth : 200;  // ??
     GameMapHeight : 400; // ??
     Visibility : 300;  // ??
     IniIdent : 'FullHD';
    );

type
/// <summary> Keymapping to improve readbility, names might change when I get a clue what is needed
///
///  F1 = Quick Help
///  Escape Key = Back to main menu
///  "A" Key = Awards and Titles Screen
///  "C" Key = Character (Player) Control Screen
///  "I" Key = Inventory Screen for Player
///  "J" Key = Journal Screen
///  "M" Key = Map Screen view (see where you've been)
///  "O" Key = Options Screen
///  "P" Key = Pause Game
///  "S" Key = Spell Selection, pick the magic you want to use
///  "X" Key = X-Ray View on/off toggle, so you can see behind walls (may slow some computers)
/// </summary>

  TKeymapData = record
    Help : byte;
    Menu : byte;
    Award : byte;
    Character : byte;
    Inventory : byte;
    Journal : byte;
    Map : byte;
    Option : byte;
    Pause : byte;
    QuestLog : byte;
    Spell : byte;
    XRay : byte;
    Demo : byte;
    Roster : byte;
    AdventureLog : byte;
    BattleCry : byte;
    CombatToggle : byte;
    QuickSave : byte;
    Screenshot : byte;
  end;

const
  cKeyOrig : TKeymapData =
    (Help : 112;  // F1
     Menu : 27;   // Esc
     Award : 65;  // A
     Character : 67;  // C
     Inventory : 73;  // I
     Journal : 74;  // J
     Map : 77;   // M
     Option : 79;  // O
     Pause : 80;  // P
     QuestLog : 81; // Q
     Spell : 83;  // S
     XRay : 88;  // X
     Demo : 68; // Death ? D
     Roster : 82;  // R
     AdventureLog : 87; // W
     BattleCry : 66; // B
     CombatToggle : 32; // space
     QuickSave : 113;  // F2
     Screenshot : 109; // Numpad -
    );

    cKeyGerman : TKeymapData =
    (Help : $20;
     Menu : $1B;
     Award : $41;
     Character : $43;
     Inventory : $49;
     Journal : $50;
     Map : $53;
     Option : $55;
     Pause : $56;
//    Quest : byte; ??
     Spell : $59;
     XRay : $58;
    );

implementation

 
end.
