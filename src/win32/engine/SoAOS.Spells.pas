unit SoAOS.Spells;
(*
  Siege Of Avalon : Open Source Edition

  Portions created by Steffen Nyeland are
  Copyright (C) 2020 - Steffen Nyeland.

  Contributor(s):
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

  Description: SoAOS Spell classes - was part of Character.pas

  Requires: Delphi 10.3.3 or later

  Revision History:
  - May 2020 - SN: Initial Commit to Git
  see git repo afterwards

*)

interface

uses
  System.Types,
  SoAOS.AI.Types,
  Character,
  Resource;

type
  TSpellClass = class of TSpell;

  TSpell = class( TObject )
  private
    CastSoundCount : integer;
    CastSoundCounter : integer;
    FInfoText : string;
    procedure SetInfoText( const Value : string );
  protected
    CastSounds : TDynamicSmallIntArray;
    procedure Replace( var S : string; const Symbol, Value : string );
    function GetLoaded : Boolean; virtual;
  public
    CastEffect : TResource;
    CastingType : TCastingType;
    TargetType : TTargetType;
    SoundInCast : boolean;
    Interupted : boolean;
    DisplayName : string;
    constructor Create; virtual;
    destructor Destroy; override;
    class function GetName : string; virtual; abstract;
    function Range( Source : TCharacter ) : Integer; virtual;
    function Recovery( Source : TCharacter ) : Integer; virtual;
    function Drain( Source : TCharacter ) : Single; virtual;
    function Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean; virtual;
    function GetIconXY( Source : TCharacter ) : TPoint; virtual; abstract;
    function GetInfo( Source : TCharacter ) : string; virtual;
    procedure Casting( Source : TCharacter ); virtual;
    procedure Clear; virtual;
    procedure LoadCastSounds( const NameList : string );
    procedure PlaySound( X, Y : longint );
    property Loaded : Boolean read GetLoaded;
    property Name : string read GetName;
    property InfoText : string read FInfoText write SetInfoText;
  end;

implementation

uses
  System.SysUtils,
  SoAOS.AI.Helper,
  LogFile,
  Sound;

{ TSpell }

function TSpell.Cast( Source : TCharacter; Target : TSpriteObject ) : Boolean;
const
  FailName : string = 'TSpell.Cast';
begin
  result := False;

  Log.DebugLog( FailName );
  try

    Source.FCastRecovery := Recovery( Source );

    if SoundInCast then
      PlaySound( Source.X, Source.Y );
    result := True;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

constructor TSpell.Create;
const
  FailName : string = 'TSpell.Create';
begin
  Log.DebugLog( FailName );
  try

    inherited;
    CastSounds := nil;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TSpell.Destroy;
const
  FailName : string = 'TSpell.Destroy';
begin
  Log.DebugLog( FailName );
  try

    if assigned( SoundLib ) then
    begin
      SoundLib.FreeSound( CastSounds );
    end;
    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TSpell.Drain( Source : TCharacter ) : Single;
begin
  result := 0;
end;

function TSpell.GetInfo( Source : TCharacter ) : string;
begin
  result := InfoText;
end;

procedure TSpell.Casting( Source : TCharacter );
begin

end;

function TSpell.GetLoaded : Boolean;
begin
  result := True;
end;

procedure TSpell.LoadCastSounds( const NameList : string );
const
  FailName : string = 'TSpell.LoadCastSounds';
begin
  Log.DebugLog( FailName );
  try

    if assigned( SoundLib ) then
    begin
      if assigned( CastSounds ) then
        SoundLib.FreeSound( CastSounds );
      CastSounds := SoundLib.OpenSound( NameList, SoundPreloadCount, CastSoundCount );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TSpell.PlaySound( X, Y : longint );
var
  i : integer;
const
  FailName : string = 'TSpell.PlaySound';
begin
  Log.DebugLog( FailName );
  try

    if assigned( CastSounds ) then
    begin
      i := random( CastSoundCount );
      PlaySingleSound( CastSounds[ CastSoundCounter * CastSoundCount + i ], X, Y );
      inc( CastSoundCounter );
      if CastSoundCounter >= SoundPreloadCount then
        CastSoundCounter := 0;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TSpell.Range( Source : TCharacter ) : Integer;
begin
  result := 0;
end;

function TSpell.Recovery( Source : TCharacter ) : Integer;
begin
  result := 0;
end;

procedure TSpell.Clear;
begin

end;

procedure TSpell.Replace( var S : string; const Symbol, Value : string );
var
  i : integer;
  S1 : string;
begin
  S1 := '#' + Symbol;
  i := Pos( S1, S );
  if i > 0 then
    S := copy( S, 1, i - 1 ) + Value + copy( S, i + length( S1 ), length( S ) - i - length( S1 ) + 1 );
end;

procedure TSpell.SetInfoText( const Value : string );
begin
  FInfoText := Value.Replace('|',#13);
end;


end.
