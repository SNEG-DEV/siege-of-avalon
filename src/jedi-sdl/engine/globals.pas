unit globals;

interface

uses
  sdl,
  sdlwindow,
  sdlgameinterface,
  sdltruetypefont,
  sdlaudiomixer,
  SoAPreferences,
  SiegeInterfaces,
  GameIntro,
  Externalizer,
  //Character,
  NewGame;

var
  // Global Class variables
  SoASettings : TSoAUserPreferences;
  Application : TSDL2DWindow;
  CurrentGameInterface : TGameInterfaceClass = TGameIntro;
  GameWindow :  TGameInterface;
  GameFont :  TTrueTypeFont;
  GameAudio : TSDLAudioManager;
  ExText : TExternalizer;

  // Global type variables
  bShowIntro : Boolean = true;
  bShowOuttro : Boolean = false;
  ScreenFlags : UInt32;
  bInGame : boolean = false;

  //Player
  Player, Current : TCharacter;

implementation

uses
  xplatformutils;

initialization
begin
  SoASettings := TSoAUserPreferences.Create;
  ExText := TExternalizer.create;
  GameFont :=  TTrueTypeFont.Create( SoASettings.InterfacePath + DIR_SEP + SoASettings.TTFName, [], 18 );
  GameAudio := TSDLAudioManager.Create;
end;

finalization
begin
  Player.Free;
  SoASettings.Free;
  ExText.Free;
  GameFont.Free;
  GameAudio.Free;
end;

end.
