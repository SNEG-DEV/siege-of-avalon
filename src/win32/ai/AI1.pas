unit AI1;
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

  Description:

  Requires: Delphi 10.3.3 or later

  Revision History:
  - 13 Jul 2003 - DL: Initial Upload to CVS
  - 10 Mar 2019 - SN: Forked on GitHub
  see git repo afterwards

*)

interface

uses
  Character,
  SoAOS.Animation,
  SoAOS.AI;

type
  TMeander = class( TAI ) // Wander aimlessly
  private
    Walking : Boolean;
    Delay : Integer;
    Leash : Integer;
    CenterX : Integer;
    CenterY : Integer;
  public
    procedure OnStop; override;
    procedure OnNoPath; override;
    procedure WasAttacked( Source : TAniFigure; Damage : Single ); override;
    function OnCollideFigure( Target : TAniFigure ) : Boolean; override;
  public
    procedure Init; override;
    procedure Execute; override;
  end;

  TBanditCombat = class( TAI )
  private
    Walking : Boolean;
    Waiting : Boolean;
    ReadyToAttack : Boolean;
    OldTrack : TCharacter;
    RunAway : Boolean;
    CollideCount : Integer;
    TrackX, TrackY : Longint;
    Delay, PostDelay : Integer;
  public
    procedure OnStop; override;
    procedure OnNoPath; override;
    procedure WasAttacked( Source : TAniFigure; Damage : Single ); override;
    function OnCollideFigure( Target : TAniFigure ) : Boolean; override;
  public
    procedure Init; override;
    procedure Execute; override;
  end;

  TFollowPath = class( TAI )
  private
    CurrentPath : TGameObject;
    Walking : boolean;
  public
    procedure OnStop; override;
    procedure OnNoPath; override;
  public
    procedure Init; override;
    procedure Execute; override;
  end;

function AssignAI1( AIName : string ) : TAI;

implementation

uses
  System.Classes,
  System.Types,
  System.SysUtils,
  SoAOS.AI.Types,
  SoAOS.AI.Helper,
  LogFile;

function AssignAI1( AIName : string ) : TAI;
var
  S : string;
const
  FailName : string = 'AssignAI1';
begin
  Result := nil;

  Log.DebugLog(FailName);
  try

    S := LowerCase( AIName );
    if S = 'meander' then
      Result := TMeander.Create
    else if ( S = 'banditcombat' ) or ( S = 'basicfight' ) then
      Result := TBanditCombat.Create
    else if ( S = 'followpath' ) then
      Result := TFollowPath.Create;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TMeander }

procedure TMeander.Execute;
var
  r : Integer;
  T : Single;
  X, Y : Integer;
const
  FailName : string = 'TMeander.Execute';
begin
  Log.DebugLog(FailName);
  try

    if not Walking then
    begin
      if Delay <= 0 then
      begin
        r := random( Leash );
        T := c2PI * random( 360 ) / 360;
        X := Round( r * cos( T ) ) + CenterX;
        Y := Round( r * sin( T ) / 2 ) + CenterY;
        Character.WalkTo( X, Y, 4 );
        Walking := True;
      end
      else
      begin
        Delay := Delay - 1;
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TMeander.Init;
var
  S : string;
const
  FailName : string = 'TMeander.Init';
begin
  Log.DebugLog(FailName);
  try

    S := Character.Property_[ 'CenterX' ];
    if S = '' then
    begin
      CenterX := Character.X;
      Character.Property_[ 'CenterX' ] := IntToStr( CenterX );
    end
    else
      CenterX := StrToIntDef( S, Character.X );

    S := Character.Property_[ 'CenterY' ];
    if S = '' then
    begin
      CenterY := Character.Y;
      Character.Property_[ 'CenterY' ] := IntToStr( CenterY );
    end
    else
      CenterY := StrToIntDef( S, Character.Y );

    Leash := StrToIntDef( Character.Property_[ 'LeashLength' ], 50 );
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TMeander.OnCollideFigure( Target : TAniFigure ) : Boolean;
const
  FailName : string = 'TMeander.OnCollideFigure';
begin
  Result := True;

  Log.DebugLog(FailName);
  try

    Character.Face( Target.X, Target.Y ); //This will appear as though the character
    Character.Stand; //has stopped to talk to someone or examine
    Walking := False; //an object.
    Delay := 400;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TMeander.OnNoPath;
const
  FailName : string = 'TMeander.OnNoPath';
begin
  Log.DebugLog(FailName);
  try

    Walking := False;
    Delay := 100;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TMeander.OnStop;
const
  FailName : string = 'TMeander.OnStop';
begin
  Log.DebugLog(FailName);
  try

    Walking := False;
    Delay := 100;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TMeander.WasAttacked( Source : TAniFigure; Damage : Single );
var
  x : longint;
  y : longint;
const
  FailName : string = 'TMeander.WasAttacked';
begin
  Log.DebugLog(FailName);

  try
    if Source is TCharacter then
      Character.Face( TCharacter( Source ).x, TCharacter( Source ).y );

    if Source is TCharacter then
    begin
      if not ( Character.IsAlly( TCharacter( Source ) ) ) then
      begin
        Character.Track := TCharacter( Source );
        if TCharacter( Source ).PartyMember then
          Player.MakeEnemy( Character.Alliance );
        TCharacter( Source ).MakeEnemy( Character.Alliance );
        Character.AIMode := aiCombat;
      end;
    end
    else
    begin
      X := random( 100 ) + 50;
      Y := random( 100 ) + 50;
      Walking := True;
      Character.WalkTo( X, Y, 16 );
    end
  except
    on E : Exception do
      Log.log( 'Error Meander WasAttacked: ' + E.Message );
  end;

  inherited;
end;

{ TBanditCombat }

procedure TBanditCombat.Execute;
var
  List : TStringList;
const
  FailName : string = 'TBanditCombat.Execute';
begin
  Log.DebugLog(FailName);
  try

    if Delay > 0 then
    begin
      Delay := Delay - 1;
      Exit;
    end;

    inherited;

    if RunAway then
    begin
      MoveAwayAI(250, 16, False);
      CollideCount := 0;
      Walking := True;
      OldTrack := Character.Track;
      Character.Track := nil;
      ReadyToAttack := False;
      RunAway := False;
      PostDelay := random( 70 );
    end
    else if Waiting then
    begin
      Waiting := False;
      MoveAwayAI(40, 4, False);
      CollideCount := 0;
      Walking := True;
      PostDelay := random( 10 ) + 10;
    end
    else if ReadyToAttack then
    begin
      Character.Attack( Character.Track );
      ReadyToAttack := False;
      Walking := False;
    end
    else if Walking then
    begin
      if Assigned( Character.Track ) then
      begin
        if Character.Track.Dead then
        begin
          Character.Track := nil;
          Walking := False;
        end
        else
        begin
          if ( FrameCount mod 90 ) = 0 then
          begin
            if Character.InRange( Character.Track ) then
            begin
              ReadyToAttack := True;
              PostDelay := 0;
            end
            else
            begin
              Character.Stand;
              Delay := random( 30 ) + 30;
              if Character.Track.Strength > Character.Strength * 1.5 then
                Delay := Delay + 30;
            end;
          end
          else if ( Character.Track.X <> TrackX ) or ( Character.Track.Y <> TrackY ) then
          begin
            if ( FrameCount mod 25 ) = 0 then
            begin
              if Character.InRange( Character.Track ) then
              begin
                ReadyToAttack := True;
                PostDelay := 0;
              end
              else
              begin
                Character.Stand;
                Delay := random( 10 ) + 10;
              end;
            end;
          end;
        end;
      end;
    end
    else
    begin
      if PostDelay > 0 then
      begin
        Delay := PostDelay;
        PostDelay := 0;
      end
      else
      begin
        if Assigned( Character.Track ) then
        begin
          if TCharacter( Character.Track ).Dead then
            Character.Track := nil;
        end;
        if not Assigned( Character.Track ) then
        begin
          if ( FrameCount mod 40 ) = 0 then
          begin
            List := GetPerceptibleEnemies( Character, 1 );
            try
              if Assigned( List ) then
              begin
                if List.Count = 1 then
                  Character.Track := TCharacter( List.Objects[ 0 ] )
                else
                  Character.Track := TCharacter( List.Objects[ random( List.Count ) ] );
              end
              else
              begin
                if Assigned( OldTrack ) then
                begin
                  if not ( OldTrack ).Dead then
                  begin
                    Character.Track := OldTrack;
                    OldTrack := nil;
                  end;
                end
                else
                begin
                  if ( FrameCount mod 120 ) = 0 then
                  begin
                    MoveAwayAI(80, 4, False);
                    CollideCount := 0;
                    Walking := True;
                    PostDelay := random( 120 );
                  end;
                end;
              end;
            finally
              List.free;
            end;
          end;
        end;
        if Assigned( Character.Track ) then
        begin
          if Character.InRange( Character.Track ) then
          begin
            Character.Attack( Character.Track );
          end
          else
          begin
            Character.WalkTo( Character.Track.X, Character.Track.Y, 64 );
            CollideCount := 0;
            TrackX := Character.Track.X;
            TrackY := Character.Track.Y;
            Walking := True;
          end;
        end;
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TBanditCombat.Init;
const
  FailName : string = 'TBanditCombat.Init';
begin
  Log.DebugLog(FailName);
  try

    Delay := random( 60 );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TBanditCombat.OnCollideFigure( Target : TAniFigure ) : Boolean;
const
  FailName : string = 'TBanditCombat.OnCollideFigure';
begin
  Result := False;

  Log.DebugLog(FailName);
  try

    PostDelay := 0;
    if Target = Character.Track then
    begin
      ReadyToAttack := True;
      Result := True;
    end
    else
    begin
      if Target is TCharacter then
      begin
        if Character.IsEnemy( TCharacter( Target ) ) then
        begin
          Character.Track := TCharacter( Target );
          ReadyToAttack := True;
          Result := True;
          Exit;
        end;
      end;
      if Assigned( Character.Track ) and Character.InRange( Character.Track ) then
      begin
        ReadyToAttack := True;
        Result := True;
      end
      else
      begin
        CollideCount := CollideCount + 1;
        if ( CollideCount > 1 ) then
        begin
          Character.Stand;
          Delay := Random( 20 ) + 20;
          Waiting := True;
          Result := True;
        // Log.Log('Stop and Wait'); jrs
        end;
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TBanditCombat.OnNoPath;
const
  FailName : string = 'TBanditCombat.OnNoPath';
begin
  Log.DebugLog(FailName);
  try

    Walking := False;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TBanditCombat.OnStop;
const
  FailName : string = 'TBanditCombat.OnStop';
begin
  Log.DebugLog(FailName);
  try

    Walking := False;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TBanditCombat.WasAttacked( Source : TAniFigure; Damage : Single );
const
  FailName : string = 'TBanditCombat.WasAttacked';
begin
  Log.DebugLog(FailName);
  try

    if Source is TCharacter then
    begin
      if Character.Wounds > ( Character.HitPoints / 2 ) then
      begin
        RunAway := True;
      end
      else
      begin
        Character.Track := TCharacter( Source );
      end;
    end;
    inherited;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

{ TFollowPath }

procedure TFollowPath.Execute;
const
  FailName : string = 'TFollowPath.Execute';
begin
  Log.DebugLog(FailName);
  try

    if not Walking and assigned( CurrentPath ) then
    begin
      Character.WalkTo( CurrentPath.X, CurrentPath.Y, 4 );
      Walking := true;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TFollowPath.Init;
var
  S : string;
const
  FailName : string = 'TFollowPath.Init';
begin
  Log.DebugLog(FailName);
  try

    S := Character.Property_[ 'PathCorner' ];
    CurrentPath := GetGUID( S );
    if not ( CurrentPath is TPathCorner ) then
      CurrentPath := nil;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TFollowPath.OnNoPath;
const
  FailName : string = 'TFollowPath.OnNoPath';
begin
  Log.DebugLog(FailName);
  try

    Walking := false;
    if TPathCorner( CurrentPath ).NextDestination = '' then
      CurrentPath := nil
    else
    begin
      CurrentPath := GetGUID( TPathCorner( CurrentPath ).NextDestination );
      if not ( CurrentPath is TPathCorner ) then
        CurrentPath := nil;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TFollowPath.OnStop;
const
  FailName : string = 'TFollowPath.OnStop';
begin
  Log.DebugLog(FailName);
  try

    Walking := false;
    if TPathCorner( CurrentPath ).NextDestination = '' then
      CurrentPath := nil
    else
    begin
      CurrentPath := GetGUID( TPathCorner( CurrentPath ).NextDestination );
      if not ( CurrentPath is TPathCorner ) then
        CurrentPath := nil;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

end.
