unit SoAPreferences;

interface

uses
  registryuserpreferences;

type
  TSoAUserPreferences = class( TRegistryUserPreferences )
  protected
    function GetSection( const Index : Integer ) : string; override;
    function GetIdentifier( const Index : Integer ) : string; override;
    function GetDefaultBoolean( const Index : Integer ) : Boolean; override;
    function GetDefaultDateTime( const Index : Integer ) : TDateTime; override;
    function GetDefaultInteger( const Index : Integer ) : Integer; override;
    function GetDefaultFloat( const Index : Integer ) : single; override;
    function GetDefaultString( const Index : Integer ) : string; override;
  public
    constructor Create( const FileName : string = '' ); reintroduce;
    property ShadowsOn : Boolean index 0 read GetBoolean write SetBoolean;
    property ScreenWidth : Integer index 1 read GetInteger write SetInteger;
    property ScreenHeight : Integer index 2 read GetInteger write SetInteger;
    property ScreenBPP : Integer index 3 read GetInteger write SetInteger;
    property FullScreen: Boolean index 4 read GetBoolean write SetBoolean;
    property ArtPath : string index 5 read GetString write SetString;
    property TilePath : string index 6 read GetString write SetString;
    property SoundPath : string index 7 read GetString write SetString;
    property ItemDB : string index 8 read GetString write SetString;
    property XRefDB : string index 9 read GetString write SetString;
    property TitlesDB : string index 10 read GetString write SetString;
    property InterfacePath : string index 11 read GetString write SetString;
    property LanguagePath : string index 12 read GetString write SetString;
    property OpeningMovie : string index 13 read GetString write SetString;
    property ClosingMovie : string index 14 read GetString write SetString;
    property JournalFont: Integer index 15 read GetInteger write SetInteger;
    property SoundVolume : Integer index 16 read GetInteger write SetInteger;
    property MusicVolume : Integer index 17 read GetInteger write SetInteger;
    property History : string index 18 read GetString write SetString;
    property MoviePath : string index 19 read GetString write SetString;
    property TTFName : string index 20 read GetString write SetString;
    property NoPageNumbers: Boolean index 21 read GetBoolean write SetBoolean;
    property ShowHistory: Boolean index 22 read GetBoolean write SetBoolean;
    property ShowIntro: Boolean index 23 read GetBoolean write SetBoolean;
    property MovieSwitches : string index 24 read GetString write SetString;
  end;


implementation

uses
  SysUtils,
  IniFiles;

// Enumerated type for easier identification of case statements. This order must match the index order of the class property to function properly 
type
  TGameSettingType = (
    gsShadowsOn,
    gsScreenWidth,
    gsScreenHeight,
    gsScreenBPP,
    gsFullScreen,
    gsArtPath,
    gsTilePath,
    gsSoundPath,
    gsItemDB,
    gsXRefDB,
    gsTitlesDB,
    gsInterfacePath,
    gsLanguagePath,
    gsOpeningMovie,
    gsClosingMovie,
    gsJournalFont,
    gsSoundVolume,
    gsMusicVolume,
    gsHistory,
    gsMoviePath,
    gsTTFName,
    gsNoPageNumbers,
    gsShowHistory,
    gsShowIntro,
    gsMovieSwitches );

{ TGameRegistryUserPreferences }
constructor TSoAUserPreferences.Create(const FileName: string);
var
  defFileName : string;
begin
  AutoSave := false;
  if FileName <> '' then
    defFileName := FileName
  else
    defFileName := ExtractFilePath( ParamStr( 0 ) ) +  'Siege.ini';

  Registry := TIniFile.Create( defFileName );
end;

function TSoAUserPreferences.GetDefaultBoolean( const Index: Integer ): Boolean;
begin
  case TGameSettingType( Index ) of
    gsShadowsOn : Result := true;
    gsFullScreen : Result := false;
    gsNoPageNumbers : Result := false;
    gsShowHistory :  Result := true;
    gsShowIntro :  Result := true;
  else
    result := false;
  end;
end;

function TSoAUserPreferences.GetDefaultDateTime( const Index: Integer ): TDateTime;
begin
  result := Now;
end;

function TSoAUserPreferences.GetDefaultFloat( const Index: Integer ): single;
begin
  case Index of
    0 : Result := 0.0;
    1 : Result := 0.0;
  else
    result := 0.0;
  end;
end;

function TSoAUserPreferences.GetDefaultInteger( const Index: Integer ): Integer;
begin
  case TGameSettingType( Index ) of
    gsScreenWidth : Result := 800;
    gsScreenHeight : Result := 600;
    gsScreenBPP : Result := 16;
    gsJournalFont : Result := 0;
    gsSoundVolume : Result := 0;
    gsMusicVolume : Result := 0;
  else
    result := 0;
  end;
end;

function TSoAUserPreferences.GetDefaultString( const Index: Integer ): string;
begin
  case TGameSettingType( Index ) of
    gsArtPath : Result := 'ArtLib/Resources/';
    gsTilePath : Result := 'ArtLib/Tiles/';
    gsSoundPath : Result := 'ArtLib/Resources/Audio/';
    gsItemDB : Result := 'ArtLib/Resources/Database/Items.DB';
    gsXRefDB : Result := 'ArtLib/Resources/Database/xref.db';
    gsTitlesDB : Result := 'ArtLib/Resources/Database/Title.db';
    gsInterfacePath : Result := 'Interface';
    gsLanguagePath : Result := 'english';
    gsOpeningMovie : Result := 'SiegeOpening.bik';
    gsClosingMovie : Result := 'SiegeClosing.bik';
    gsTTFName : Result := 'LBLACK.TTF';
    gsHistory : Result := '';
    gsMoviePath : Result := 'Movies';
    gsMovieSwitches : Result := '/R/C/U1/I102/D9/B0';
  else
    result := '';
  end;
end;

function TSoAUserPreferences.GetIdentifier( const Index : Integer ) : string;
begin
  case TGameSettingType( Index ) of
    gsShadowsOn : Result := 'Shadows';
    gsScreenWidth : Result := 'ScreenWidth';
    gsScreenHeight : Result := 'ScreenHeight';
    gsScreenBPP : Result := 'ScreenBPP';
    gsFullScreen : Result := 'FullScreen';
    gsArtPath : Result := 'ArtPath';
    gsTilePath : Result := 'TilePath';
    gsSoundPath : Result := 'SoundPath';
    gsItemDB : Result := 'ItemDB';
    gsXRefDB : Result := 'XRefDB';
    gsTitlesDB : Result := 'TitlesDB';
    gsInterfacePath : Result := 'Interface';
    gsLanguagePath : Result := 'LanguagePath';
    gsOpeningMovie : Result := 'OpeningMovie';
    gsClosingMovie : Result := 'ClosingMovie';
    gsJournalFont : Result := 'JournalFont';
    gsSoundVolume : Result := 'SoundVolume';
    gsMusicVolume : Result := 'MusicVolume';
    gsHistory : Result := 'history';
    gsMoviePath : Result := 'MoviePath';
    gsTTFName : Result := 'TTFName';
    gsNoPageNumbers : Result := 'NoPageNumbers';
    gsShowHistory : Result := 'ShowHistory';
    gsShowIntro : Result := 'ShowIntro';
    gsMovieSwitches : Result := 'MovieSwitches';
  else
    result := '';
  end;
end;

function TSoAUserPreferences.GetSection( const Index : Integer ) : string;
begin
  case TGameSettingType( Index ) of
    gsShadowsOn..gsMoviePath : Result := 'Settings';
    gsTTFName : Result := LanguagePath;
    gsNoPageNumbers : Result := 'Settings';
    gsShowHistory : Result := 'Settings';
    gsShowIntro : Result := 'Settings';
    gsMovieSwitches : Result := 'Settings';
    //gsPlayerEmail..gsPlayerHighScore : Result := 'PlayerInfo';
  else
    result := '';
  end;
end;

end.
