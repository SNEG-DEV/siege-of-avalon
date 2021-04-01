//----------------------------------------------------
// © 2015 Andrey Volia
// 
// License: MIT
// Site: https://github.com/voliaandrey/steamwrapper
//----------------------------------------------------
unit SteamworksClasses;

interface

uses Steamworks, SteamworksTypes;

Type
  THTMLSurface = record
  public
    ID: integer;
    Handler: THTMLSurfaceCallbacks;
    procedure Init(UserAgent, UserCSS: AnsiString);
  end;

implementation

{ THTMLSurface }

procedure THTMLSurface.Init(UserAgent, UserCSS: AnsiString);
var s,s2: AnsiString;
begin
  s:=UserAgent; s2:=UserCSS;
  ID := SteamHTMLSurface_CreateBrowser(Handler, @s[1], @s2[1]);
end;


end.
