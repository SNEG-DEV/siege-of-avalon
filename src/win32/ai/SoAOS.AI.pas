unit SoAOS.AI;
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

  Description: SoAOS main AI classes - was part of Character.pas

  Requires: Delphi 10.3.3 or later

  Revision History:
  - May 2020 - SN: Initial Commit to Git
  see git repo afterwards

*)

interface

uses
  System.Classes,
  SoAOS.AI.Types,
  SoAOS.Animation;

type
  TAI = class( TObject )
  protected
    FCharacter : TObject; // TCharacter by class helper;
    FrameCount : LongWord;
  public
    procedure TrackChanged; virtual;
    procedure OnNoPath; virtual;
    procedure OnCollideItem; virtual;
    function OnCollideObject( Target : TAniFigure ) : Boolean; virtual;
    function OnCollideFigure( Target : TAniFigure ) : Boolean; virtual;
    procedure OnStop; virtual;
    procedure WasAttacked( Source : TAniFigure; Damage : Single ); virtual;
    procedure WasKilled( Source : TAniFigure ); virtual;
    procedure Init; virtual;
    procedure Execute; virtual;
    procedure CallToArms( Source, Target : TAniFigure ); virtual;
    procedure Regroup( Source : TAniFigure; NewX, NewY : Integer ); virtual;
    procedure NotifyOfDeath( Source : TAniFigure ); virtual;
    procedure Follow( Source, Target : TAniFigure ); virtual;
    procedure Clicked; virtual;
    procedure MoveAwayAI(radius, deviance: Integer; enface: Boolean = True; run: Boolean = False);
  end;

  TPartyAI = class( TAI )
  protected
    FLeader : TObject; // TCharacter store for class helper;
    FPartyMember : array[ 1..4 ] of TObject; // TCharacter store for class helper;
    FSpellToCast : array[ 1..4 ] of TObject; // TSpell store for class helper;
  public
    Priority : array[ 1..4 ] of TAIPriority;
    Parameter : array[ 1..4 ] of TAIParameter;

    Party : TList;
    Index : integer;
    procedure AIChanged; virtual;
  end;

implementation

uses
  System.SysUtils,
  SoAOS.Types,
  SoAOS.AI.Helper,
  Character,
  LogFile,
  Sound;

{ TAI }

procedure TAI.CallToArms( Source, Target : TAniFigure );
begin

end;

procedure TAI.Clicked;
begin

end;

procedure TAI.Execute;
begin
  Inc( FrameCount );
end;

procedure TAI.Follow( Source, Target : TAniFigure );
begin

end;

procedure TAI.Init;
begin

end;

procedure TAI.MoveAwayAI(radius, deviance: Integer; enface, run: Boolean);
const
  FailName : string = 'TAI.MoveAwayAI';
begin
  Log.DebugLog( FailName );
  try
    var diameter: integer := 2*radius;
    if enface then
    begin
      if run then
      case Character.Facing of
        fNE,fEE,fSE: Character.RunTo( Character.X - radius, Character.Y + random( diameter ) - radius, deviance );
        fNW,fSW,fWW: Character.RunTo( Character.X + radius, Character.Y + random( diameter ) - radius, deviance );
        fSS: Character.RunTo( Character.X + random( diameter ) - radius, Character.Y - radius, deviance );
        fNN: Character.RunTo( Character.X + random( diameter ) - radius, Character.Y + radius, deviance );
      end
      else
      case Character.Facing of
        fNE,fEE,fSE: Character.WalkTo( Character.X - radius, Character.Y + random( diameter ) - radius, deviance );
        fNW,fSW,fWW: Character.WalkTo( Character.X + radius, Character.Y + random( diameter ) - radius, deviance );
        fSS: Character.WalkTo( Character.X + random( diameter ) - radius, Character.Y - radius, deviance );
        fNN: Character.WalkTo( Character.X + random( diameter ) - radius, Character.Y + radius, deviance );
      end;
    end
    else // Bandits, Scouts.Wait and WolfCombat.Wait - might be a stupid idea not very SOLID
    begin
      var halfradius: integer := radius div 2;
      Character.WalkTo( Character.X + random( diameter ) - radius, Character.Y + random( radius ) - halfradius, deviance );
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

procedure TAI.NotifyOfDeath( Source : TAniFigure );
begin

end;

function TAI.OnCollideFigure( Target : TAniFigure ) : Boolean;
begin
  Result := False;
end;

procedure TAI.OnCollideItem;
begin

end;

function TAI.OnCollideObject( Target : TAniFigure ) : Boolean;
begin
  Result := False;
end;

procedure TAI.OnNoPath;
begin

end;

procedure TAI.OnStop;
begin

end;

procedure TAI.Regroup( Source : TAniFigure; NewX, NewY : Integer );
begin

end;

procedure TAI.TrackChanged;
begin

end;

procedure TAI.WasAttacked( Source : TAniFigure; Damage : Single );
const
  FailName : string = 'TAI.WasAttacked';
begin
  Log.DebugLog( FailName );
  try

    if not Character.InPain then
    begin
      Character.InPain := true;
      PlaySound( Character.PainSounds, Character.X, Character.Y );
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TAI.WasKilled( Source : TAniFigure );
const
  FailName : string = 'TAI.WasKilled';
begin
  Log.DebugLog( FailName );
  try

    PlaySound( Character.DeathSounds, Character.X, Character.Y );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TPartyAI }

procedure TPartyAI.AIChanged;
begin

end;

end.
