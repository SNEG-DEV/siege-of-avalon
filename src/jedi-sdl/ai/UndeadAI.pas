unit UndeadAI;
{******************************************************************************}
{                                                                              }
{               Siege Of Avalon : Open Source Edition                          }
{               -------------------------------------                          }
{                                                                              }
{ Portions created by Digital Tome L.P. Texas USA are                          }
{ Copyright ©1999-2000 Digital Tome L.P. Texas USA                             }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Portions created by Team SOAOS are                                           }
{ Copyright (C) 2003 - Team SOAOS.                                             }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                            }
{                                                                              }
{                                                                              }
{                                                                              }
{ You may retrieve the latest version of this file at the SOAOS project page : }
{   http://www.sourceforge.com/projects/soaos                                  }
{                                                                              }
{ The contents of this file maybe used with permission, subject to             }
{ the GNU Lesser General Public License Version 2.1 (the "License"); you may   }
{ not use this file except in compliance with the License. You may             }
{ obtain a copy of the License at                                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Software distributed under the License is distributed on an                  }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{ implied. See the License for the specific language governing                 }
{ rights and limitations under the License.                                    }
{                                                                              }
{ Description                                                                  }
{ -----------                                                                  }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   DirectX Runtime libraris on Win32                                          }
{   They are available from...                                                 }
{   http://www.microsoft.com.                                                  }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{   July    13 2003 - DL : Initial Upload to CVS                               }
{                                                                              }
{******************************************************************************}

{$INCLUDE Anigrp30cfg.inc}

interface

uses
  Classes,
  SysUtils,
  Character,
  Resource,
  Engine,
  Anigrp30,
  LogFile,
  Graphics,
  Math;

type
  TUndeadType = ( utSkeleton, utLich, utGhoul, utGhost );

  TMainStat = ( msStrength, msHitPoints, msCombat, msMysticism, msMana );

  TUndeadIdle = class( TAI )
  private
    Walking : Boolean;
    Delay : Integer;
    Leash : Integer;
    CenterX : Integer;
    CenterY : Integer;
    bCombative : Boolean;
    FUnDeadType : TUndeadType;
    dead : TCharacter;
    procedure FindTarget;
    procedure Eat;
    procedure Meander;
    procedure Fight;
    procedure IdleAction;
  protected
    procedure OnStop; override;
    procedure WasAttacked( Source : TAniFigure; Damage : Single ); override;
  public
    procedure Init; override;
    property UnDeadType : TUndeadType read FUnDeadType write FUnDeadType;
    procedure Follow( Source, Target : TAniFigure ); override;
    procedure Execute; override;
    function OnCollideFigure( Target : TAniFigure ) : boolean; override;
    procedure ReGroup( Source : TAniFigure; NewX, NewY : Integer ); override;
  end;

  TUndeadMeleeCombat = class( TAI )
  private
    Walking : boolean;
    bTakeOrders : boolean;
    CirclePoint : integer;
    waiting : boolean;
    realStop : boolean;
    Delay : integer;
    FUnDeadType : TUndeadType;
    CollideCount : integer;
    procedure FindTarget;
    procedure Attack;
    procedure Eat;
  protected
    procedure OnStop; override;
    procedure OnNoPath; override;
    procedure WasAttacked( Source : TAniFigure; Damage : single ); override;
    function OnCollideFigure( Target : TAniFigure ) : boolean; override;
  public
    procedure Init; override;
    property UnDeadType : TUndeadType read FUnDeadType write FUnDeadType;
    procedure Execute; override;
    procedure Regroup( Source : TAniFigure; NewX, NewY : Integer ); override;
    procedure CallToArms( Source, Target : TAniFigure ); override;

  end;

  TUndeadArcherCombat = class( TAI )
  private
    Walking : boolean;
    FriendsList : TStringList;
    CirclePoint : integer;
    Delay : integer;
    PartyTotal : integer;
    ShotCounter : integer;
    RunOrFight : boolean;
    RunAway : Boolean;
    MaxShots : integer;
    //Soft
    bTakeOrders : Boolean;
    iDistance : integer;
    bMove : boolean;
    procedure MoveAway;
    procedure Attack;
    procedure FindTarget;
    procedure BattleTatic;
  protected
    procedure OnStop; override;
    procedure WasAttacked( Source : TAniFigure; Damage : single ); override;
    procedure WasKilled( Source : TAniFigure ); override;
    function OnCollideFigure( Target : TAniFigure ) : boolean; override;
    procedure OnNoPath; override;
  public
    destructor Destroy; override;
    procedure Init; override;
    procedure Execute; override;
    procedure CallToArms( Source, Target : TAniFigure ); override;
    procedure Regroup( Source : TAniFigure; NewX, NewY : Integer ); override;
    procedure NotifyOfDeath( Source : TAniFigure ); override;
  end;

  TUndeadCasterCombat = class( TAI )
  private
    Walking : boolean;
    Friendly : TCharacter;
    FriendsList : TStringList;
    Delay : integer;
    NukeCounter : integer;
    CirclePoint : integer;
    RunAway : boolean;
    CastTimes : integer;
    //Soft
    bTakeOrders : Boolean;
    iDistance : integer;
    bHealFirst : Boolean;
    bMove : Boolean;
    procedure MoveAway;
    procedure Attack;
    procedure FindFriendly;
    procedure FindTarget;
    procedure CastHeal;
    procedure BattleTatic;
  protected
    procedure OnStop; override;
    procedure OnNoPath; override;
    procedure WasAttacked( Source : TAniFigure; Damage : single ); override;
    procedure WasKilled( Source : TAniFigure ); override;
    function OnCollideFigure( Target : TAniFigure ) : boolean; override;
  public
    destructor Destroy; override;
    procedure Init; override;
    procedure Execute; override;
    procedure CallToArms( Source, Target : TAniFigure ); override;
    procedure Regroup( Source : TAniFigure; NewX, NewY : Integer ); override;
    procedure NotifyOfDeath( Source : TAniFigure ); override;
  end;

  TUndeadCommanderCombat = class( TAI )
  private
    Walking : boolean;
    Friendly : TCharacter;
    FriendsList : TStringList;
    Delay : integer;
    NukeCounter : integer;
    CirclePoint : integer;
    RunAway : boolean;
    CastTimes : integer;
    //Soft
    bTakeOrders : Boolean;
    iDistance : integer;
    bHealFirst : Boolean;
    bMove : Boolean;
    MainStat : TMainStat;

    procedure MoveAway;
    procedure Attack;
    procedure FindFriendly;
    procedure FindTarget;
    procedure CastHeal;
    procedure BattleTatic;
  protected
    procedure OnStop; override;
    procedure OnNoPath; override;
    procedure WasAttacked( Source : TAniFigure; Damage : single ); override;
    procedure WasKilled( Source : TAniFigure ); override;
    function OnCollideFigure( Target : TAniFigure ) : boolean; override;
  public
    destructor Destroy; override;
    procedure Init; override;
    procedure Execute; override;
    procedure CallToArms( Source, Target : TAniFigure ); override;
    procedure Regroup( Source : TAniFigure; NewX, NewY : Integer ); override;
    procedure NotifyOfDeath( Source : TAniFigure ); override;
  end;

function RangeTest( Target, Source : TAniFigure; iDist : integer ) : boolean;
function AssignUndeadAI( AIName : string ) : TAI;
function GetFacing( SrcX, SrcY, TargetX, TargetY : Longint ) : Extended;

implementation

const
  pi = 3.1415926535;
  pi2 = 2 * pi;

function AssignUndeadAI( AIName : string ) : TAI;
var
  S : string;
const
  FailName : string = 'AssignUndeadAI';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  result := nil;
  try
    S := lowercase( AIName );
    if S = 'undeadidle' then
      result := TUndeadIdle.create
    else if ( S = 'undeadmeleecombat' ) then
      result := TUndeadMeleeCombat.create
    else if ( S = 'undeadarchercombat' ) then
      result := TUndeadArcherCombat.create
    else if ( S = 'undeadcastercombat' ) then
      result := TUndeadCasterCombat.create
    else if ( S = 'undeadcommander' ) then
      result := TUndeadCommanderCombat.create
    else
      result := nil;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

function RangeTest( Target, Source : TAniFigure; iDist : integer ) : boolean;
var
  D : Double;
const
  FailName : string = 'RangeTest';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  result := false;
  try
    Result := false;
    D := sqrt( sqr( Target.X - Source.X ) + sqr( 2 * ( Target.Y - Source.Y ) ) );
    if D <= iDist then
      Result := true;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

function GetFacing( SrcX, SrcY, TargetX, TargetY : Longint ) : Extended;
var
  Slope : Single;
begin
  Result := 2;

  try

    if ( TargetX = SrcX ) then
    begin
      if ( TargetY < SrcY ) then
        Result := 2
    end
    else
    begin
      Slope := ( TargetY - SrcY ) / ( TargetX - SrcX );
      if ( TargetX < SrcX ) then
      begin
        if ( Slope >= -0.25 ) and ( Slope <= 0.25 ) then
          Result := 1
        else if ( Slope > 2 ) then
          Result := 2
        else if ( Slope < -2 ) then
          Result := 2
        else if ( Slope > 0 ) then
          Result := 1.5
        else
          Result := 1.5;
      end
      else
      begin
        if ( Slope >= -0.25 ) and ( Slope <= 0.25 ) then
          Result := 1
        else if ( Slope > 2 ) then
          Result := 2
        else if ( Slope < -2 ) then
          Result := 2
        else if ( Slope > 0 ) then
          Result := 1.5
        else
          Result := 1.5;
      end;
    end;

  except
    on E : Exception do
      Log.log( 'Error Humanoid GetFacing: ' + E.Message );
  end;
end;


{ TUndeadIdle }

procedure TUndeadIdle.Execute;
const
  FailName : string = 'TUndeadIdle.Execute';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;


  //watch for the living
    if BCombative then
      FindTarget;

    if Assigned( Character.track ) then
      fight;
    if Assigned( Dead ) then
      Eat;

    if Delay > 0 then
    begin
      dec( Delay );
      exit;
    end;

    IdleAction;

   //nothing eles to do just wonder around
    case Random( 3 ) of
      0 :
        if ( not Walking ) then
          meander;
      1 :
        if Assigned( Dead ) and ( not Walking ) then
        begin
          Character.Approach( dead );
          delay := random( 100 ) + 100;
          walking := true;
        end;
      2 :
        if Assigned( Character.track ) and ( not Walking ) then
        begin
          Character.WalkTo( Character.track.x, Character.track.y, 64 );
          delay := random( 100 ) + 100;
          walking := true;
        end;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadIdle.ReGroup( Source : TAniFigure; NewX, NewY : Integer );
const
  FailName : string = 'TUndeadIdle.reGroup';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Character.WalkTo( NewX, NewY, 64 );
    Walking := true;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadIdle.Eat;
const
  FailName : string = 'TUndeadIdle.Eat';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if Character.Inrange( dead ) then
      if ( ( FrameCount mod 40 ) = 0 ) then
      begin
        Character.Face( dead.X, dead.Y );
        case random( 4 ) of
          0 : Character.Say( '*slurp*', clblue );
          1 : Character.Say( '*crunch*', clblue );
          2 : Character.Say( '*snap*', clblue );
          3 : Character.Say( '*chew*', clblue );
        end;
        case random( 2 ) of
          0 : Character.DoAction( 'Attack1' );
          1 : Character.DoAction( 'pain' );
        end;
      end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadIdle.IdleAction;
var
  List : TStringList;
const
  FailName : string = 'TUndeadIdle.IdleAction';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    case FUndeadType of
      utGhoul : //Ghouls get hungry
        begin
          List := GetPerceptibleDead( Character, 1.5 );
          if Assigned( List ) then
          begin
            if List.Count = 1 then
              Dead := TCharacter( List.Objects[ 0 ] )
            else
              Dead := TCharacter( List.Objects[ Random( List.Count ) ] );
            List.free;
          end;
        end;
    end
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadIdle.Fight;
const
  FailName : string = 'TUndeadIdle.Fight';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if Character.Inrange( Character.Track ) then
      if ( ( FrameCount mod 60 ) = 0 ) then
      begin
        Character.Face( Character.Track.x, Character.Track.y );
        Character.DoAction( 'Attack1' );
        walking := false;
      end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadIdle.Meander;
var
  r : integer;
  T : single;
  X, Y : integer;

const
  FailName : string = 'TUndeadIdle.Meander';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if ( FUndeadType <> utSkeleton ) then
    begin
      character.say( 'clear', clblack ); //clear the text
      r := random( Leash );
      T := pi2 * random( 360 ) / 360;
      X := round( r * cos( T ) ) + CenterX;
      Y := round( r * sin( T ) / 2 ) + CenterY;
      Character.WalkTo( X, Y, 64 );
      Walking := true;
      delay := random( 100 ) + 200;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;


procedure TUndeadIdle.FindTarget;
var
  iStealth : integer;

const
  FailName : string = 'TUndeadIdle.FindTarget';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if ( FrameCount mod 40 ) = 0 then
    begin
      if Current.Stealth < 1 then
        istealth := ( 1 div 100 )
      else
        iStealth := ( current.stealth div 100 );
      if character.RangeTo( Current.x, Current.y ) < ( ( character.Vision - ( character.Vision * iStealth ) ) * GetFacing( character.x, character.y, current.x, current.y ) ) then
        if game.LineOfSight( character.x, character.y, current.x, current.y ) then
        begin
          character.track := Current;
          character.AIMode := aiCombat;
        end;

{       List := GetPerceptibleEnemies(Character, 1);
       if Assigned(List) then
       begin
            //find a live target
            for i := 0 to list.count -1 do
            if Not(TCharacter(List.Objects[i]).Dead) then
              character.AIMode:=aiCombat;
            list.free;
       end;}
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;


procedure TUndeadIdle.Init;
var
  S : string;
const
  FailName : string = 'TUndeadIdle.Init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    CenterX := Character.X;
    CenterY := Character.Y;

    S := Character.Properties[ 'LeashLength' ];
    try
      if S = '' then
        Leash := 200
      else
        Leash := StrToInt( S );
    except
      Leash := 200;
    end;

    case Random( 8 ) of
      0 : character.Frame := 168;
      1 : character.Frame := 175;
      2 : character.Frame := 184;
      3 : character.Frame := 192;
      4 : character.Frame := 200;
      5 : character.Frame := 208;
      6 : character.Frame := 216;
      7 : character.Frame := 224;
    end;


    S := LowerCase( Character.Properties[ 'Combative' ] );
    try
      bCombative := true;
      if S = 'false' then
        bCombative := False

    except
      bCombative := true;
    end;


  //temp UndeadType Setting
    if lowerCase( character.Name ) = 'skeleton' then
      FUndeadType := utSkeleton;

    if lowerCase( character.Name ) = 'ghoul' then
      FUndeadType := utGhoul;

    if lowerCase( character.Name ) = 'lich' then
      FUndeadType := utLich;

    if lowerCase( character.Name ) = 'ghost' then
      FUndeadType := utGhost;

    if lowerCase( character.name ) = 'body' then
      Character.doAction( 'death' );

    S := LowerCase( Character.Properties[ 'Transparent' ] );
    try
      if S <> '100' then
      begin
        Character.Alpha := StrToInt( s );
        Character.SpecialEffect := seAdd;
      end;
    except
    end;

  //Actual UndeadType Setting
{  S:=Character.Properties['UndeadType'];
  try
    if S='' then
       FUndeadType:=utSkeleton
    else
        case StrToInt(S) of
        0: FUndeadType:=utSkeleton;
        1: FUndeadType:=utLich;
        2: FUndeadType:=utGhoul;
        3: FUndeadType:=utGhost;
        else
         FUndeadType:=utSkeleton;
        end;

  except
       FUndeadType:=utSkeleton
  end;
 }
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadIdle.Follow( Source, Target : TAniFigure );
const
  FailName : string = 'TUndeadIdle.Follow';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    character.WalkTo( Target.x, Target.y, 64 );
    Walking := True;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadIdle.WasAttacked( Source : TAniFigure; Damage : Single );
const
  FailName : string = 'TUndeadIdle.WasAttacked';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if Source is TCharacter then
    begin
      character.AIMode := aiCombat;
    end;
    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;



function TUndeadIdle.OnCollideFigure( Target : TAniFigure ) : boolean;
const
  FailName : string = 'TUndeadIdle.Oncollidefigure';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  Result := false;
  try

    if Target is TCharacter then
      if not Character.IsEnemy( TCharacter( Target ) ) then
      begin
        case random( 2 ) of
          0 :
            begin
              Walking := false;
              Delay := Random( 160 ) + 100;
              Character.Track := Tcharacter( target );
              Result := true;
            end;
        end;

      end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadIdle.OnStop;
const
  FailName : string = 'TUndeadIdle.OnStop';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Walking := false;
    if ( character.X <> character.StartX ) and ( character.Y <> character.StartY ) then
      character.doaction( 'stand' );
    // character.Frame := Random(31)+1;

  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

(*************************************************************************************)

{ TUndeadMeleeCombat }

procedure TUndeadMeleeCombat.Execute;
var
  r : Integer;
  T : Single;
  X, Y : Integer;

const
  FailName : string = 'TUndeadMeleeCombat.Execute';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if ( FrameCount mod 160 ) = 0 then
      RealStop := false;

    if assigned( character.track ) then
      if ( character.track = character ) or character.IsAlly( character.track ) then
        character.track := nil;

    if CirclePoint > 535 then
      CirclePoint := Random( 360 ) + 180;


    if Delay > 0 then
    begin
      dec( Delay );
      exit;
    end;

    inherited;

    if Walking and Assigned( Character.track ) then
    begin
      if ( frameCount mod 10 ) = 0 then
        if not ( Character.InRange( Character.Track ) ) then
          Character.WalkTo( Character.Track.X, Character.Track.Y, 64 );
    end;

    if Waiting then
    begin
      Waiting := false;
      if ( random( 2 ) = 0 ) and Assigned( Character.Track ) then
      begin
        inc( CirclePoint, 45 );
        r := 100;
        T := pi2 * CirclePoint / 360;
        X := Round( r * cos( T ) ) + TCharacter( Character.Track ).X;
        Y := Round( r * sin( T ) / 2 ) + TCharacter( Character.Track ).Y;
        Character.WalkTo( X, Y, 16 );
        Walking := True;
      end;
      CollideCount := 0;
      Delay := Random( 32 ) + 32;
      exit;
    end;


    if not ( Walking ) and Assigned( Character.track ) then
      Attack
    else
      FindTarget;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;


procedure TUndeadMeleeCombat.Eat;
const
  FailName : string = 'TUndeadMeleeCombat.Eat';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if ( Character.Inrange( Character.Track ) ) and Character.Track.Dead then
      if ( ( FrameCount mod 40 ) = 0 ) then
      begin
        Character.Face( Character.Track.X, Character.track.Y );
        case random( 4 ) of
          0 : Character.Say( '*slurp*', clblue );
          1 : Character.Say( '*crunch*', clblue );
          2 : Character.Say( '*snap*', clblue );
          3 : Character.Say( '*chew*', clblue );
        end;
        case random( 2 ) of
          0 : Character.DoAction( 'Attack1' );
          1 : Character.DoAction( 'pain' );
        end;
        if random( 90 ) < 10 then
          Character.Track := nil;
      end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadMeleeCombat.Attack;
const
  FailName : string = 'TUndeadMeleeCombat.Attack';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if TCharacter( Character.Track ).Dead then
      if FUndeadType = utGhoul then
        Eat
      else
        Character.Track := nil
    else
    begin
      if Character.InRange( Character.Track ) then
      begin
        Walking := false;
        character.Face( Character.Track.x, Character.Track.y );
        Character.Attack( Character.Track );
      end
      else if not ( RealStop ) then
      begin
        Walking := true;
        Character.WalkTo( Character.Track.X, Character.Track.Y, 64 );
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadMeleeCombat.FindTarget;
var
  List : Tstringlist;
begin
  if ( FrameCount mod 40 ) = 0 then
  begin
    List := GetPerceptibleEnemies( Character, 2 ); //undead watch really hard.. nothing better to do

    if assigned( List ) then
    begin
      if List.Count = 1 then
        Character.Track := TCharacter( List.objects[ 0 ] )
      else
        Character.Track := TCharacter( List.objects[ random( List.count ) ] );
      list.free;
    end
    else
      character.AIMode := aiIdle;
  end;

end;


procedure TUndeadMeleeCombat.ReGroup( Source : TAniFigure; NewX, NewY : Integer );
const
  FailName : string = 'TUndeadMeleeCombat.Regroup';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if bTakeOrders then
    begin
      Character.WalkTo( NewX, NewY, 64 );
      Walking := true;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadMeleeCombat.CallToArms( Source, Target : TAniFigure );
const
  FailName : string = 'TUndeadMeleeCombat.CallToArms';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if bTakeOrders then
      Character.Track := TCharacter( Target );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadMeleeCombat.Init;
var
  S : string;
  i : integer;
const
  FailName : string = 'TUndeadMeleeCombat.Init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    CirclePoint := Random( 360 ) + 180;

    Delay := random( 40 );
    CollideCount := 0;

    S := LowerCase( Character.Properties[ 'TakeOrders' ] );
    try
      bTakeOrders := true;
      if S = 'false' then
        bTakeOrders := False;

    except
      bTakeOrders := true;
    end;

    S := LowerCase( Character.Properties[ 'BalanceWithPlayer' ] );
    try
      if ( S <> '' ) and ( s <> '0' ) then
      begin
        i := StrToInt( s );
        if i >= 0 then
        begin
          if player.TitleExists( 'Apprentice' ) then
          begin
            character.Combat := ( ( ( player.Mysticism * 3 ) div 4 ) + i );
            character.strength := ( ( player.perception * 3 ) div 4 ) + i;
            character.HitPoints := ( ( player.Perception * 2 ) );
            character.Coordination := ( ( player.Coordination * 2 ) div 3 ) + i;
            Character.AttackRecovery := Character.AttackRecovery + ( player.attackRecovery div i );

            if character.Resistance.Heat.Invulnerability < ( player.mysticism div 10 ) then
              character.Resistance.Heat.Invulnerability := ( player.mysticism div 10 );
            if character.Resistance.Cold.Invulnerability < ( player.mysticism div 10 ) then
              character.Resistance.Cold.Invulnerability := ( player.mysticism div 10 );
            if character.Resistance.Electric.Invulnerability < ( player.mysticism div 10 ) then
              character.Resistance.Electric.Invulnerability := ( player.mysticism div 10 );
            if character.Resistance.Magic.Invulnerability < ( player.mysticism div 10 ) then
              character.Resistance.Magic.Invulnerability := ( player.mysticism div 10 );
            if character.Resistance.Poison.Invulnerability < ( player.mysticism div 10 ) then
              character.Resistance.Poison.Invulnerability := ( player.mysticism div 10 );
            if character.Resistance.Mental.Invulnerability < ( player.mysticism div 10 ) then
              character.Resistance.Mental.Invulnerability := ( player.mysticism div 10 );

          end;
          if player.TitleExists( 'Hunter' ) then
          begin
            character.Combat := ( ( ( player.Stealth * 3 ) div 4 ) + i );
            character.strength := ( ( player.Coordination * 3 ) div 4 ) + i;
            character.HitPoints := ( ( player.Coordination * 2 ) );
            character.Coordination := ( ( player.Coordination * 3 ) div 4 ) + i;
            Character.AttackRecovery := Character.AttackRecovery + ( player.attackRecovery div i );

            if character.Resistance.Heat.Invulnerability < ( player.mysticism div 20 ) then
              character.Resistance.Heat.Invulnerability := ( player.mysticism div 20 );
            if character.Resistance.Cold.Invulnerability < ( player.mysticism div 20 ) then
              character.Resistance.Cold.Invulnerability := ( player.mysticism div 20 );
            if character.Resistance.Electric.Invulnerability < ( player.mysticism div 20 ) then
              character.Resistance.Electric.Invulnerability := ( player.mysticism div 20 );
            if character.Resistance.Magic.Invulnerability < ( player.mysticism div 20 ) then
              character.Resistance.Magic.Invulnerability := ( player.mysticism div 20 );
            if character.Resistance.Poison.Invulnerability < ( player.mysticism div 20 ) then
              character.Resistance.Poison.Invulnerability := ( player.mysticism div 20 );
            if character.Resistance.Mental.Invulnerability < ( player.mysticism div 20 ) then
              character.Resistance.Mental.Invulnerability := ( player.mysticism div 20 );

          end;

          if player.TitleExists( 'Squire' ) then
          begin
            character.Combat := ( ( player.Combat * 3 ) div 4 ) + i;
            character.strength := ( ( player.Strength * 3 ) div 4 ) + i;
            character.Coordination := ( ( player.Coordination * 2 ) div 3 ) + i;
            Character.AttackRecovery := Character.AttackRecovery + ( player.attackRecovery div i );
            character.HitPoints := ( ( player.strength * 2 ) );

            if character.Resistance.Heat.Invulnerability < ( player.mysticism div 20 ) then
              character.Resistance.Heat.Invulnerability := ( player.mysticism div 20 );
            if character.Resistance.Cold.Invulnerability < ( player.mysticism div 20 ) then
              character.Resistance.Cold.Invulnerability := ( player.mysticism div 20 );
            if character.Resistance.Electric.Invulnerability < ( player.mysticism div 20 ) then
              character.Resistance.Electric.Invulnerability := ( player.mysticism div 20 );
            if character.Resistance.Magic.Invulnerability < ( player.mysticism div 20 ) then
              character.Resistance.Magic.Invulnerability := ( player.mysticism div 20 );
            if character.Resistance.Poison.Invulnerability < ( player.mysticism div 20 ) then
              character.Resistance.Poison.Invulnerability := ( player.mysticism div 20 );
            if character.Resistance.Mental.Invulnerability < ( player.mysticism div 20 ) then
              character.Resistance.Mental.Invulnerability := ( player.mysticism div 20 );

          end;

          if character.Resistance.Heat.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Heat.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Cold.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Cold.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Electric.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Electric.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Magic.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Magic.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Poison.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Poison.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Mental.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Mental.Resistance := ( player.mysticism / 200 );
        end;
      end;
    except
    end;


  //temp UndeadType settings
    if character.Name = 'skeleton' then
      FUndeadType := utSkeleton;

    if character.Name = 'ghoul' then
      FUndeadType := utGhoul;

    if character.Name = 'lich' then
      FUndeadType := utLich;

    if character.Name = 'ghost' then
      FUndeadType := utGhost;

 //actual undeadType settings
{  S:=Character.Properties['UndeadType'];
  try
    if S='' then
       FUndeadType:=utSkeleton
    else
        case StrToInt(S) of
        0: FUndeadType:=utSkeleton;
        1: FUndeadType:=utLich;
        2: FUndeadType:=utGhoul;
        3: FUndeadType:=utGhost;
        else
         FUndeadType:=utSkeleton;
        end;

  except
       FUndeadType:=utSkeleton
  end;
 }
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

function TUndeadMeleeCombat.OnCollideFigure( Target : TAniFigure ) : boolean;
const
  FailName : string = 'TUndeadMeleeCombat.Attack';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  Result := false;
  try
    Result := False;
    if Target = Character.Track then
    begin
      Attack;
      Result := True;
    end
    else
    begin
      if Target is TCharacter then
      begin
        if Character.IsEnemy( TCharacter( Target ) ) then
        begin
          Character.Track := TCharacter( Target );
          Result := True;
        end
        else if assigned( Character.Track ) and not ( Character.InRange( Character.Track ) ) then
        begin
          inc( CollideCount );
          if ( CollideCount > 3 ) then
          begin
            Character.doaction( 'stand' );
            waiting := true;
            result := true;
            delay := random( 40 );
          end;
        end;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadMeleeCombat.OnStop;
const
  FailName : string = 'TUndeadMeleeCombat.OnStop';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Walking := false;
    if ( character.X = character.StartX ) and ( character.Y = character.StartY ) then
    begin
      character.doaction( 'stand' );
      character.stand;
      character.Frame := Random( 31 ) + 1;
     //  if assigned(character.track) then
     //  character.Face(character.track.x, character.track.y);

      realstop := true;
    end;


  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadMeleeCombat.OnNoPath;
const
  FailName : string = 'TUndeadMeleeCombat.OnNoPath';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Walking := false;
    if ( character.X = character.StartX ) and ( character.Y = character.StartY ) then
    begin
      character.doaction( 'stand' );
      character.stand;
      character.Frame := Random( 31 ) + 1;
      // if assigned(character.track) then
      // character.Face(character.track.x, character.track.y);
      realstop := true;
    end;

 // character.Frame := Random(31)+1;

  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadMeleeCombat.WasAttacked( Source : TAniFigure; Damage : single );
const
  FailName : string = 'TUndeadMeleeCombat.WasAttacked';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if ( Source is TCharacter ) then
      if Character.IsEnemy( TCharacter( Source ) ) then
        Character.Track := TCharacter( Source );

    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

(*************************************************************************************)

{ TUndeadArcherCombat }

procedure TUndeadArcherCombat.CallToArms( Source, Target : TAniFigure );
const
  FailName : string = 'TUndeadArcherCombat.CallToArms';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if bTakeOrders then
    begin
      Character.Track := TCharacter( Target );
    //  if Character.CurrentSpell <> fireball then
    //    Character.CurrentSpell := Fireball;
    end;
  except
    on E : Exception do
      Log.log( 'Error UndeadArcher CallToArms: ' + E.Message );
  end;

end;

procedure TUndeadArcherCombat.Execute;
const
  FailName : string = 'TUndeadArcherCombat.execute';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  inherited;
  try
  //  if (FrameCount mod 160) = 0 then walking := false;

    if CirclePoint > 535 then
      CirclePoint := Random( 360 ) + 180;


    if ( Delay > 0 ) and not ( Walking ) then
    begin
      dec( Delay );
      exit;
    end;

    if not ( walking ) and RunAway then
      MoveAway;

    if not ( RunOrFight ) and Assigned( Character.Track ) and not ( walking ) then
    begin
      if Character.IsEnemy( Character.Track ) then
      begin
        if RangeTest( Character.Track, Character, iDistance ) then
        begin
          MoveAway;
          RunOrFight := true;
          exit;
        end
      end;
    end;

    if Assigned( Character.Track ) and not ( walking ) then
    begin
      if ( Character.Track = character ) then
        Character.Track := nil
      else
        Attack;
    end;


    if not Assigned( Character.Track ) then
      FindTarget;


  except
    on E : Exception do
      Log.log( 'Error UndeadArcher Execute: ' + E.Message );

  end;



end;

procedure TUndeadArcherCombat.BattleTatic;
var
  r : Integer;
  T : Single;
  X, Y : Integer;
const
  FailName : string = 'TUndeadArcherCombat.battletactic';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if not Walking then
    begin
      Walking := True;
      ShotCounter := 0;
      inc( CirclePoint, 45 );
      r := 300;
      T := pi2 * CirclePoint / 360;
      X := Round( r * cos( T ) ) + Character.Track.X;
      Y := Round( r * sin( T ) / 2 ) + Character.Track.Y;
      Character.WalkTo( X, Y, 16 );
    end;
  except
    on E : Exception do
      Log.log( 'Error UndeadArcher BattleTatic: ' + E.Message );

  end;

end;


procedure TUndeadArcherCombat.MoveAway;
const
  FailName : string = 'TUndeadArcherCombat.MoveAway';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Walking := True;
    if assigned( Character.Track ) then
      Character.Face( Character.Track.X, Character.Track.Y );
    if Pos( 'E', character.FacingString ) <> 0 then
      Character.WalkTo( Character.X - 100, Character.Y + random( 200 ) - 100, 16 )
    else if Pos( 'W', character.FacingString ) <> 0 then
      Character.WalkTo( Character.X + 100, Character.Y + random( 200 ) - 100, 16 )
    else if Pos( 'SS', character.FacingString ) <> 0 then
      Character.WalkTo( Character.X + random( 200 ) - 100, Character.Y - 100, 16 )
    else
      Character.WalkTo( Character.X + random( 200 ) - 100, Character.Y + 100, 16 );

    RunAway := False;
  except
    on E : Exception do
      Log.log( 'Error UndeadArcher MoveAway: ' + E.Message );

  end;

end;

procedure TUndeadArcherCombat.Attack;
const
  FailName : string = 'TUndeadArcherCombat.attack';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if TCharacter( Character.Track ).Dead then
      Character.Track := nil
    else
    begin
      if not ( RangeTest( Character.Track, Character, 50 ) ) then
      begin
        if ShotCounter < MaxShots then
        begin
          if bMove then
            Inc( ShotCounter );
          Character.Face( Character.Track.x, Character.Track.y );
          if Character.Inrange( Character.Track ) then
            Character.Attack( Character.Track );
        end
        else
          BattleTatic;
      end
      else
      begin
        RunOrFight := False;
                //swithc to melee
        if Character.Inrange( Character.Track ) then
          Character.Attack( Character.Track );
      end;
    end;
  except
    on E : Exception do
      Log.log( 'Error UndeadArcher Attack: ' + E.Message );

  end;

end;

procedure TUndeadArcherCombat.FindTarget;
var
  list : TStringList;
const
  FailName : string = 'TUndeadArcherCombat.FindTarget';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    List := GetPerceptibleEnemies( Character, 2 );
    if assigned( List ) then
    begin
      if List.Count = 1 then
        Character.Track := TCharacter( List.objects[ 0 ] )
      else
        Character.Track := TCharacter( List.objects[ random( List.count ) ] );
      list.free;
      Character.CurrentSpell := nil;
    end
    else
      character.AIMode := aiIdle;
  except
    on E : Exception do
      Log.log( 'Error UndeadArcher FindTarget: ' + E.Message );

  end;

end;


procedure TUndeadArcherCombat.Init;
var
  S : string;
  i : integer;
const
  FailName : string = 'TUndeadArcherCombat.Init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    CirclePoint := Random( 360 ) + 180;
    ShotCounter := 0;
    MaxShots := Random( 3 ) + 1;

    S := LowerCase( Character.Properties[ 'TakeOrders' ] );
    try
      if S = '' then
        bTakeOrders := true
      else if S = 'false' then
        bTakeOrders := False
      else
        bTakeOrders := true;
    except
      bTakeOrders := true;
    end;

    S := LowerCase( Character.Properties[ 'BalanceWithPlayer' ] );
    try
      if ( S <> '' ) and ( s <> '0' ) then
      begin
        i := StrToInt( s );
        if i >= 0 then
        begin
          if player.TitleExists( 'Apprentice' ) then
          begin
            character.Combat := player.Mysticism + i;
            if character.hitpoints < ( ( player.Perception * 2 ) + player.Mysticism ) then
              character.HitPoints := ( ( player.Perception * 2 ) + player.Mysticism );
          end;
          if player.TitleExists( 'Hunter' ) then
          begin
            character.Combat := player.Stealth + i;
            if character.hitpoints < ( ( player.strength * 2 ) + player.combat ) then
              character.HitPoints := ( ( player.strength * 2 ) + player.combat );

          end;
          if player.TitleExists( 'Squire' ) then
          begin
            character.Combat := player.Combat + i;
            if character.hitpoints < ( ( player.Coordination * 2 ) + player.stealth ) then
              character.HitPoints := ( ( player.Coordination * 2 ) + player.stealth );
          end;

          if character.Resistance.Heat.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Heat.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Cold.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Cold.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Electric.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Electric.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Magic.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Magic.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Poison.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Poison.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Mental.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Mental.Resistance := ( player.mysticism / 200 );

        end;
      end;
    except
    end;

    S := LowerCase( Character.Properties[ 'Moveable' ] );
    try
      if S = '' then
        bMove := true
      else if S = 'false' then
        bMove := False
      else
        bMove := true;
    except
      bMove := true;
    end;


    S := Character.Properties[ 'Distance' ];
    try
      if S = '' then
        iDistance := 175
      else
        iDistance := StrToInt( S );
    except
      iDistance := 175;
    end;

    PartyTotal := 1;
    if character.GroupName <> '' then
      FriendsList := GetGroup( Character, Character.GroupName );
    if Assigned( FriendsList ) then
      PartyTotal := Friendslist.Count
    else
    begin
      FriendsList := GetPerceptibleAllies( Character, 1 );
      if Assigned( FriendsList ) then
      begin
        PartyTotal := Friendslist.Count;
        FriendsList.Free;
        FriendsList := nil;
      end;
    end;
  except
    on E : Exception do
      Log.log( 'Error UndeadArcher Init: ' + E.Message );

  end;

  Delay := random( 60 );

end;

procedure TUndeadArcherCombat.NotifyOfDeath( Source : TAniFigure );
const
  FailName : string = 'TUndeadArcherCombat.NotifyOfDeath';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
  except
    on E : Exception do
      Log.log( 'Error UndeadArcher NotifyOfDeath: ' + E.Message );
  end;
end;

function TUndeadArcherCombat.OnCollideFigure(
  Target : TAniFigure ) : boolean;
const
  FailName : string = 'TUndeadArcherCombat.Oncollidefigure';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  Result := False;
  try
    if Target = Character.Track then
    begin
      RunAway := True;
      Result := True;
    end
    else
    begin
      if Target is TCharacter then
      begin
        if Character.IsEnemy( TCharacter( Target ) ) then
        begin
          Character.Track := TCharacter( Target );
          RunAway := True;
          Result := True;
        end;
      end;
    end;
  except
    on E : Exception do
      Log.log( 'Error UndeadArcher CollideFigure: ' + E.Message );

  end;

end;

procedure TUndeadArcherCombat.OnNoPath;
const
  FailName : string = 'TUndeadArcherCombat.OnNoPath';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    RunOrFight := False;
    Walking := False;
    if Assigned( Character.Track ) then
      Character.Face( Character.Track.X, Character.Track.Y );
  except
    on E : Exception do
      Log.log( 'Error UndeadArcher NoPath: ' + E.Message );

  end;

end;

procedure TUndeadArcherCombat.OnStop;
const
  FailName : string = 'TUndeadArcherCombat.OnStop';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Walking := false;
    if Assigned( Character.Track ) then
      Character.Face( Character.Track.X, Character.Track.Y );
  except
    on E : Exception do
      Log.log( 'Error UndeadArcher Stop: ' + E.Message );

  end;

end;

procedure TUndeadArcherCombat.Regroup( Source : TAniFigure; NewX, NewY : Integer );
const
  FailName : string = 'TUndeadArcherCombat.regroup';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}

  try
    if bTakeOrders then
    begin
      if Assigned( Character.Track ) then
        Character.Track := nil;

      Character.WalkTo( NewX, NewY, 64 );
      Walking := true;
    end;
  except
    on E : Exception do
      Log.log( 'Error UndeadArcher Regroup: ' + E.Message );

  end;

end;

procedure TUndeadArcherCombat.WasAttacked( Source : TAniFigure; Damage : single );
const
  FailName : string = 'TUndeadArcherCombat.WasAttacked';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if Source <> character then
    begin
      if Character.IsEnemy( TCharacter( Source ) ) then
        Character.Track := TCharacter( Source );
    end;
  except
    on E : Exception do
      Log.log( 'Error UndeadArcher WasAttacked: ' + E.Message );
  end;

  inherited;
end;

procedure TUndeadArcherCombat.WasKilled( Source : TAniFigure );
var
  List : TStringList;
  iLoop : integer;
const
  FailName : string = 'TUndeadArcherCombat.WasKilled';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
     //Tell everyone I died so they can save themselves
    List := GetPerceptibleAllies( Character, 1.5 );
    if Assigned( List ) then
    begin
      for iLoop := 0 to List.count - 1 do
      begin
        if Assigned( TCharacter( List.Objects[ iLoop ] ).AI ) then
          TAI( TCharacter( List.Objects[ iLoop ] ).AI ).NotifyOfDeath( character );
      end;
      list.free;
    end;
  except
    on E : Exception do
      Log.log( 'Error UndeadArcher WasKilled: ' + E.Message );

  end;

end;


(**************************************************************************************)


{ THumanoidCasterCombat }


procedure TUndeadCasterCombat.CallToArms( Source, Target : TAniFigure );
const
  FailName : string = 'TUndeadCasterCombat.Calltoarms';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if bTakeOrders then
    begin
      Character.Track := TCharacter( Target );
//      if not Assigned(Character.CurrentSpell) then
//        Character.CurrentSpell := Fireball;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

procedure TUndeadCasterCombat.Execute;
const
  FailName : string = 'TUndeadCasterCombat.Execute';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
//  if (FrameCount mod 160) = 0 then walking := false;

    if CirclePoint > 535 then
      CirclePoint := Random( 360 ) + 180;

    if Delay > 0 then
    begin
      dec( Delay );
      exit;
    end;

    if not ( RunAway ) and Assigned( Character.Track ) and not ( walking ) then
    begin
      if Character.IsEnemy( Character.Track ) then
      begin
        if RangeTest( Character.Track, Character, iDistance ) then
        begin
          MoveAway;
          RunAway := true;
          exit;
        end
      end;
    end;

    if not Assigned( Character.Track ) then
      FindTarget;

    if Assigned( Friendly ) then
      if ( Friendly.Wounds < ( Friendly.HitPoints * 0.75 ) ) or Friendly.dead then
        Friendly := nil;

    if bHealFirst and not Assigned( Friendly ) then
      FindFriendly;


    if bHealFirst and Assigned( Friendly ) then
      castHeal
    else if Assigned( Character.Track ) then
    begin
      if ( Character.Track = character ) then
        Character.Track := nil
      else
        Attack;
    end;

  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

procedure TUndeadCasterCombat.BattleTatic;
var
  r : Integer;
  T : Single;
  X, Y : Integer;
const
  FailName : string = 'TUndeadCasterCombat.battletactic';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if not Walking then
    begin
      NukeCounter := 0;
      inc( CirclePoint, 45 );
      r := 300;
      T := pi2 * CirclePoint / 360;
      X := Round( r * cos( T ) ) + Character.Track.X;
      Y := Round( r * sin( T ) / 2 ) + Character.Track.Y;
      Character.WalkTo( X, Y, 16 );
      Walking := True;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

procedure TUndeadCasterCombat.CastHeal;
const
  FailName : string = 'TUndeadCasterCombat.CastHeal';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if Walking then
      exit;
    if TCharacter( Friendly ).Dead then
      Friendly := nil
    else
    begin
//      if Character.currentSpell <> Healing then
//        Character.CurrentSpell := Healing;
      Character.Face( Friendly.x, Friendly.y );
      if ( character.Mana - character.Drain ) >= Character.CurrentSpell.Drain( Character ) then
        character.Cast( Friendly )
      else
        Delay := Random( 360 ) + 120;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;


procedure TUndeadCasterCombat.MoveAway;
const
  FailName : string = 'TUndeadCasterCombat.moveAway';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    Walking := True;
    if assigned( Character.Track ) then
      Character.Face( Character.Track.X, Character.Track.Y );
    if Pos( 'E', character.FacingString ) <> 0 then
      Character.WalkTo( Character.X - 100, Character.Y + random( 200 ) - 100, 16 )
    else if Pos( 'W', character.FacingString ) <> 0 then
      Character.WalkTo( Character.X + 100, Character.Y + random( 200 ) - 100, 16 )
    else if Pos( 'SS', character.FacingString ) <> 0 then
      Character.WalkTo( Character.X + random( 200 ) - 100, Character.Y - 100, 16 )
    else
      Character.WalkTo( Character.X + random( 200 ) - 100, Character.Y + 100, 16 );

//     if Assigned(Character.currentSpell) then
//     Delay := character.CurrentSpell.Recovery(Character);
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

procedure TUndeadCasterCombat.Attack;
const
  FailName : string = 'TUndeadCasterCombat.Attack';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if TCharacter( Character.Track ).Dead then
      Character.Track := nil
    else
    begin
      if not ( RangeTest( Character.Track, Character, 50 ) ) then
      begin
        if NukeCounter < CastTimes then
        begin
          if bMove then
            Inc( NukeCounter );
               //line of sight test here.
          Character.Face( Character.Track.x, Character.Track.y );
//          if Character.currentSpell <> FireBall then
//            Character.CurrentSpell := FireBall;


          if ( character.Mana - character.Drain ) >= Character.CurrentSpell.Drain( Character ) then
            character.Cast( Character.Track )
          else
            Delay := Random( 360 ) + 120;
        end
        else
          BattleTatic;


      end
      else if random( 2 ) = 1 then
      begin
        Character.Face( Character.Track.x, Character.Track.y );
//          if Character.currentSpell <> Push then
//            Character.CurrentSpell := Push;
        if ( character.Mana - character.Drain ) >= Character.CurrentSpell.Drain( Character ) then
          character.Cast( Character.Track )
        else
          Character.Attack( Character.Track );
      end
      else
      begin
        RunAway := False;
        Character.Attack( Character.Track );
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

procedure TUndeadCasterCombat.FindFriendly;
var
  i : integer;
const
  FailName : string = 'TUndeadCasterCombat.FindFriendly';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if not Assigned( FriendsList ) then
      FriendsList := GetPerceptibleAllies( Character, 1 );
    if assigned( FriendsList ) then
    begin //find someone to heal
      if FriendsList.Count = 1 then
      begin
        if FriendsList.objects[ 0 ] is TCharacter then
          if not ( TCharacter( FriendsList.objects[ 0 ] ).dead ) then
            if TCharacter( FriendsList.objects[ 0 ] ).Wounds > ( TCharacter( FriendsList.objects[ 0 ] ).HitPoints * 0.75 ) then
              Friendly := TCharacter( FriendsList.objects[ 0 ] );
      end
      else
      begin
        for i := 0 to FriendsList.Count - 1 do
        begin
          if FriendsList.objects[ i ] is TCharacter then
            if not ( TCharacter( FriendsList.objects[ i ] ).dead ) then
              if TCharacter( FriendsList.objects[ i ] ).Wounds > ( TCharacter( FriendsList.objects[ i ] ).HitPoints * 0.75 ) then
              begin
                Friendly := TCharacter( FriendsList.objects[ i ] );
                break;
              end;
        end;
      end;
    end;


  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

procedure TUndeadCasterCombat.FindTarget;
var
  list : TStringList;
begin
  List := GetPerceptibleEnemies( Character, 2 );
  if assigned( List ) then
  begin
    if List.Count = 1 then
      Character.Track := TCharacter( List.objects[ 0 ] )
    else
      Character.Track := TCharacter( List.objects[ random( List.count ) ] );
    list.free;
    Character.CurrentSpell := nil;
  end
  else
    character.AIMode := aiIdle;

end;

procedure TUndeadCasterCombat.Init;
var
  S : string;
  i : integer;
const
  FailName : string = 'TUndeadCasterCombat.Init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    CirclePoint := Random( 360 ) + 180;
    NukeCounter := 0;
    CastTimes := Random( 3 ) + 1;
    S := LowerCase( Character.Properties[ 'Moveable' ] );
    character.AddTitle( 'Flame' );
    character.AddTitle( 'Push' );
    character.AddTitle( 'Frost' );
    character.AddTitle( 'Heal' );
    character.AddTitle( 'Charge' );


    try
      if S = '' then
        bMove := true
      else if S = 'false' then
        bMove := False
      else
        bMove := true;
    except
      bMove := true;
    end;

    S := LowerCase( Character.Properties[ 'BalanceWithPlayer' ] );
    try
      if ( S <> '' ) and ( s <> '0' ) then
      begin
        i := StrToInt( s );
        if i >= 0 then
        begin
          if player.TitleExists( 'Apprentice' ) then
          begin
            character.Mysticism := player.Mysticism + i;
            if character.hitpoints < ( ( player.Perception * 2 ) + player.Mysticism ) then
              character.HitPoints := ( ( player.Perception * 2 ) + player.Mysticism );
          end;
          if player.TitleExists( 'Hunter' ) then
          begin
            character.Mysticism := player.Stealth + i;
            if character.hitpoints < ( ( player.strength * 2 ) + player.combat ) then
              character.HitPoints := ( ( player.strength * 2 ) + player.combat );

          end;
          if player.TitleExists( 'Squire' ) then
          begin
            character.Mysticism := player.Combat + i;
            if character.hitpoints < ( ( player.Coordination * 2 ) + player.stealth ) then
              character.HitPoints := ( ( player.Coordination * 2 ) + player.stealth );
          end;

          if character.Resistance.Heat.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Heat.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Cold.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Cold.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Electric.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Electric.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Magic.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Magic.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Poison.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Poison.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Mental.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Mental.Resistance := ( player.mysticism / 200 );

        end;
      end;
    except
    end;

    S := LowerCase( Character.Properties[ 'HealFirst' ] );
    try
      if S = '' then
        bHealFirst := true
      else if S = 'false' then
        bHealFirst := False
      else
        bHealFirst := true;

    except
      bHealFirst := true;
    end;



    S := LowerCase( Character.Properties[ 'TakeOrders' ] );
    try
      if S = '' then
        bTakeOrders := true
      else if S = 'false' then
        bTakeOrders := False
      else
        bTakeOrders := true;
    except
      bTakeOrders := true;
    end;


    S := Character.Properties[ 'Distance' ];
    try
      if S = '' then
        iDistance := 175
      else
        iDistance := StrToInt( S );
    except
      iDistance := 175;
    end;

    if character.GroupName <> '' then
      FriendsList := GetGroup( Character, Character.GroupName );

    Delay := random( 60 );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

procedure TUndeadCasterCombat.NotifyOfDeath( Source : TAniFigure );
begin
//another one bites the dust
end;

function TUndeadCasterCombat.OnCollideFigure( Target : TAniFigure ) : boolean;
const
  FailName : string = 'TUndeadCasterCombat.OnCollideFigure';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  Result := False;
  try
    Result := False;
    if Target = Character.Track then
    begin
      MoveAway;
      Result := True;
    end
    else
    begin
      if Target is TCharacter then
      begin
        if Character.IsEnemy( TCharacter( Target ) ) then
        begin
          Character.Track := TCharacter( Target );
          MoveAway;
          Result := True;
        end;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadCasterCombat.OnStop;
const
  FailName : string = 'TUndeadCasterCombat.OnStop';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Walking := false;
    if ( character.X <> character.StartX ) and ( character.Y <> character.StartY ) then
      character.Frame := Random( 31 ) + 1;
    if Assigned( Character.Track ) then
      Character.Face( Character.Track.X, Character.Track.Y );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadCasterCombat.OnNoPath;
const
  FailName : string = 'TUndeadCasterCombat.OnNoPath';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    RunAway := False;
 // character.Frame := Random(31)+1;
    Walking := false;
    if Assigned( Character.Track ) then
      Character.Face( Character.Track.X, Character.Track.Y );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadCasterCombat.Regroup( Source : TAniFigure; NewX, NewY : Integer );
const
  FailName : string = 'TUndeadCasterCombat.regroup';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if bTakeOrders then
    begin
      Character.WalkTo( NewX, NewY, 64 );
      Walking := true;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadCasterCombat.WasAttacked( Source : TAniFigure;
  Damage : single );
const
  FailName : string = 'TUndeadCasterCombat.WasAttacked';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if Source <> character then
    begin
      if Character.IsEnemy( TCharacter( Source ) ) then
      begin
        Character.Track := TCharacter( Source );
   //   MoveAway;
        Friendly := nil;
      end
      else
      begin
        Character.Track := TCharacter( Source );
     // Character.Attack(Character.Track);
      end;
    end;
    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

procedure TUndeadCasterCombat.WasKilled( Source : TAniFigure );
var
  List : TStringList;
  iLoop : integer;
const
  FailName : string = 'TUndeadCasterCombat.WasKilled';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

   //Tell everyone I died so they can save themselves
    List := GetPerceptibleAllies( Character, 1.5 );
    if Assigned( List ) then
    begin
      for iLoop := 0 to List.count - 1 do
      begin
        if Assigned( TCharacter( List.Objects[ iLoop ] ).AI ) then
          TAI( TCharacter( List.Objects[ iLoop ] ).AI ).NotifyOfDeath( character );
      end;
      list.free;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

procedure TUndeadCommanderCombat.Execute;
const
  FailName : string = 'TUndeadCommanderCombat.Execute';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
//  if (FrameCount mod 160) = 0 then walking := false;

    if CirclePoint > 535 then
      CirclePoint := Random( 360 ) + 180;

    if Delay > 0 then
    begin
      dec( Delay );
      exit;
    end;

    if not ( RunAway ) and Assigned( Character.Track ) and not ( walking ) then
    begin
      if Character.IsEnemy( Character.Track ) then
      begin
        if RangeTest( Character.Track, Character, iDistance ) then
        begin
          MoveAway;
          RunAway := true;
          exit;
        end
      end;
    end;

    if not Assigned( Character.Track ) then
      FindTarget;

    if Assigned( Friendly ) then
      if ( Friendly.Wounds < ( Friendly.HitPoints * 0.75 ) ) or Friendly.dead then
        Friendly := nil;

    if bHealFirst and not Assigned( Friendly ) then
      FindFriendly;


    if bHealFirst and Assigned( Friendly ) then
      castHeal
    else if Assigned( Character.Track ) then
    begin
      if ( Character.Track = character ) then
        Character.Track := nil
      else
        Attack;
    end;

  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadCommanderCombat.CallToArms( Source, Target : TAniFigure );
const
  FailName : string = 'TUndeadCommanderCombat.CallToArms';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if bTakeOrders then
    begin
      Character.Track := TCharacter( Target );
//      if not Assigned(Character.CurrentSpell) then
//        Character.CurrentSpell := Fireball;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadCommanderCombat.BattleTatic;
var
  r : Integer;
  T : Single;
  X, Y : Integer;
const
  FailName : string = 'TUndeadCommanderCombat.Battletactic';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if not Walking then
    begin
      NukeCounter := 0;
      inc( CirclePoint, 45 );
      r := 300;
      T := pi2 * CirclePoint / 360;
      X := Round( r * cos( T ) ) + Character.Track.X;
      Y := Round( r * sin( T ) / 2 ) + Character.Track.Y;
      Character.WalkTo( X, Y, 16 );
      Walking := True;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadCommanderCombat.CastHeal;
const
  FailName : string = 'TUndeadCommanderCombat.castHeal';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if Walking then
      exit;
    if TCharacter( Friendly ).Dead then
      Friendly := nil
    else
    begin
//      if Character.currentSpell <> Healing then
//        Character.CurrentSpell := Healing;
      Character.Face( Friendly.x, Friendly.y );
      if ( character.Mana - character.Drain ) >= Character.CurrentSpell.Drain( Character ) then
        character.Cast( Friendly )
      else
        Delay := Random( 360 ) + 120;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;


procedure TUndeadCommanderCombat.MoveAway;
const
  FailName : string = 'TUndeadCommanderCombat.MoveAway';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    Walking := True;
    if assigned( Character.Track ) then
      Character.Face( Character.Track.X, Character.Track.Y );
    if Pos( 'E', character.FacingString ) <> 0 then
      Character.WalkTo( Character.X - 100, Character.Y + random( 200 ) - 100, 16 )
    else if Pos( 'W', character.FacingString ) <> 0 then
      Character.WalkTo( Character.X + 100, Character.Y + random( 200 ) - 100, 16 )
    else if Pos( 'SS', character.FacingString ) <> 0 then
      Character.WalkTo( Character.X + random( 200 ) - 100, Character.Y - 100, 16 )
    else
      Character.WalkTo( Character.X + random( 200 ) - 100, Character.Y + 100, 16 );

//     if Assigned(Character.currentSpell) then
//     Delay := character.CurrentSpell.Recovery(Character);
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadCommanderCombat.Attack;
const
  FailName : string = 'TUndeadCommanderCombat.Attack';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if TCharacter( Character.Track ).Dead then
      Character.Track := nil
    else
    begin
      if not ( RangeTest( Character.Track, Character, 50 ) ) then
      begin
        if NukeCounter < CastTimes then
        begin
          if bMove then
            Inc( NukeCounter );
               //line of sight test here.
          Character.Face( Character.Track.x, Character.Track.y );
//          if Character.currentSpell <> FireBall then
//            Character.CurrentSpell := FireBall;
          if ( character.Mana - character.Drain ) >= Character.CurrentSpell.Drain( Character ) then
          begin
            // log.log('MEC- Cast Fireball'); jrs
            character.Cast( Character.Track );
          end
          else
            Delay := Random( 360 ) + 120;
        end
        else
          BattleTatic;


      end
      else if random( 2 ) = 1 then
      begin
        Character.Face( Character.Track.x, Character.Track.y );
//          if Character.currentSpell <> Push then
//            Character.CurrentSpell := Push;
        if ( character.Mana - character.Drain ) >= Character.CurrentSpell.Drain( Character ) then
        begin
            // log.log('MEC- Cast Push'); jrs
          character.Cast( Character.Track );
        end
        else
          Character.Attack( Character.Track );
      end
      else
      begin
        RunAway := False;
        Character.Attack( Character.Track );
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadCommanderCombat.FindFriendly;
var
  i : integer;
const
  FailName : string = 'TUndeadCommanderCombat.FindFriendly';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if not Assigned( FriendsList ) then
      FriendsList := GetPerceptibleAllies( Character, 1 );
    if assigned( FriendsList ) then
    begin //find someone to heal
      if FriendsList.Count = 1 then
      begin
        if FriendsList.objects[ 0 ] is TCharacter then
          if not ( TCharacter( FriendsList.objects[ 0 ] ).dead ) then
            if TCharacter( FriendsList.objects[ 0 ] ).Wounds > ( TCharacter( FriendsList.objects[ 0 ] ).HitPoints * 0.75 ) then
              Friendly := TCharacter( FriendsList.objects[ 0 ] );
      end
      else
      begin
        for i := 0 to FriendsList.Count - 1 do
        begin
          if FriendsList.objects[ i ] is TCharacter then
            if not ( TCharacter( FriendsList.objects[ i ] ).dead ) then
              if TCharacter( FriendsList.objects[ i ] ).Wounds > ( TCharacter( FriendsList.objects[ i ] ).HitPoints * 0.75 ) then
              begin
                Friendly := TCharacter( FriendsList.objects[ i ] );
                break;
              end;
        end;
      end;
    end;


  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadCommanderCombat.FindTarget;
var
  list : TStringList;
  tmp, i : integer;
const
  FailName : string = 'TUndeadCommanderCombat.FindTarget';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    tmp := 0;
    List := GetPerceptibleEnemies( Character, 2 );
    if assigned( List ) then
    begin
      if List.Count = 1 then
        Character.Track := TCharacter( List.objects[ 0 ] )
      else
      begin //really hard part
        case MainStat of
          msStrength :
            while List.Count > 1 do
            begin
              inc( tmp );
              for i := 0 to List.Count - 1 do
                if TCharacter( List.objects[ i ] ).strength < tmp then
                begin
                  list.Delete( i );
                  break;
                end;
            end;

          msHitPoints :
            while List.Count > 1 do
            begin
              inc( tmp );
              for i := 0 to List.Count - 1 do
                if TCharacter( List.objects[ i ] ).strength < tmp then
                begin
                  list.Delete( i );
                  break;
                end;
            end;
          msCombat :
            while List.Count > 1 do
            begin
              inc( tmp );
              for i := 0 to List.Count - 1 do
                if TCharacter( List.objects[ i ] ).strength < tmp then
                begin
                  list.Delete( i );
                  break;
                end;
            end;
          msMysticism :
            while List.Count > 1 do
            begin
              inc( tmp );
              for i := 0 to List.Count - 1 do
                if TCharacter( List.objects[ i ] ).Mysticism < tmp then
                begin
                  list.Delete( i );
                  break;
                end;
            end;
          msMana :
            while List.Count > 1 do
            begin
              inc( tmp );
              for i := 0 to List.Count - 1 do
                if TCharacter( List.objects[ i ] ).Mana < tmp then
                begin
                  list.Delete( i );
                  break;
                end;
            end;

        end;
        if List.Count <> 0 then
        begin
          // log.log('MEC- Attack ' + TCharacter(List.objects[0]).Name); jrs
          Character.Track := TCharacter( List.objects[ 0 ] );
        end;
      end;
      list.free;
      Character.CurrentSpell := nil;
    end
    else
      character.AIMode := aiIdle;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadCommanderCombat.Init;
var
  S : string;
  i : integer;
const
  FailName : string = 'TUndeadCommanderCombat.Init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    CirclePoint := Random( 360 ) + 180;
    NukeCounter := 0;
    CastTimes := Random( 3 ) + 1;
    S := LowerCase( Character.Properties[ 'Moveable' ] );
    try
      if S = '' then
        bMove := true
      else if S = 'false' then
        bMove := False
      else
        bMove := true;
    except
      bMove := true;
    end;

    S := LowerCase( Character.Properties[ 'HealFirst' ] );
    try
      if S = '' then
        bHealFirst := true
      else if S = 'false' then
        bHealFirst := False
      else
        bHealFirst := true;

    except
      bHealFirst := true;
    end;

    S := LowerCase( Character.Properties[ 'BalanceWithPlayer' ] );
    try
      if ( S <> '' ) and ( s <> '0' ) then
      begin
        i := StrToInt( s );
        if i >= 0 then
        begin
          if player.TitleExists( 'Apprentice' ) then
          begin
            character.Combat := player.Mysticism + i;
            if character.hitpoints < ( ( player.Perception * 2 ) + player.Mysticism ) then
              character.HitPoints := ( ( player.Perception * 2 ) + player.Mysticism );
          end;
          if player.TitleExists( 'Hunter' ) then
          begin
            character.Combat := player.Stealth + i;
            if character.hitpoints < ( ( player.strength * 2 ) + player.combat ) then
              character.HitPoints := ( ( player.strength * 2 ) + player.combat );

          end;
          if player.TitleExists( 'Squire' ) then
          begin
            character.Combat := player.Combat + i;
            if character.hitpoints < ( ( player.Coordination * 2 ) + player.stealth ) then
              character.HitPoints := ( ( player.Coordination * 2 ) + player.stealth );
          end;

          if character.Resistance.Heat.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Heat.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Cold.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Cold.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Electric.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Electric.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Magic.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Magic.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Poison.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Poison.Resistance := ( player.mysticism / 200 );
          if character.Resistance.Mental.Resistance < ( player.mysticism / 200 ) then
            character.Resistance.Mental.Resistance := ( player.mysticism / 200 );

        end;
      end;
    except
    end;


    S := LowerCase( Character.Properties[ 'TakeOrders' ] );
    try
      if S = '' then
        bTakeOrders := true
      else if S = 'false' then
        bTakeOrders := False
      else
        bTakeOrders := true;
    except
      bTakeOrders := true;
    end;

    S := LowerCase( Character.Properties[ 'MainStat' ] );
    try
      if S = '' then
        MainStat := msCombat
      else if S = 'strength' then
        MainStat := msStrength
      else if S = 'hitpoints' then
        MainStat := msHitPoints
      else if S = 'combat' then
        MainStat := msCombat
      else if S = 'mana' then
        MainStat := msMana
      else if S = 'mysticism' then
        MainStat := msMysticism
      else
        MainStat := msCombat
    except
      MainStat := msCombat
    end;

    S := Character.Properties[ 'Distance' ];
    try
      if S = '' then
        iDistance := 175
      else
        iDistance := StrToInt( S );
    except
      iDistance := 175;
    end;

    if character.GroupName <> '' then
      FriendsList := GetGroup( Character, Character.GroupName );

    Delay := random( 60 );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadCommanderCombat.NotifyOfDeath( Source : TAniFigure );
begin
//another one bites the dust
end;

function TUndeadCommanderCombat.OnCollideFigure( Target : TAniFigure ) : boolean;
const
  FailName : string = 'TUndeadCommanderCombat.OnCollideFigure';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  Result := False;
  try

    if Target = Character.Track then
    begin
      MoveAway;
      Result := True;
    end
    else
    begin
      if Target is TCharacter then
      begin
        if Character.IsEnemy( TCharacter( Target ) ) then
        begin
          Character.Track := TCharacter( Target );
          MoveAway;
          Result := True;
        end;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadCommanderCombat.OnStop;
const
  FailName : string = 'TUndeadCommanderCombat.OnStop';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    Walking := false;
    if Assigned( Character.Track ) then
      Character.Face( Character.Track.X, Character.Track.Y );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadCommanderCombat.OnNoPath;
const
  FailName : string = 'TUndeadCommanderCombat.onNoPath';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    RunAway := False;
    Walking := false;
    if Assigned( Character.Track ) then
      Character.Face( Character.Track.X, Character.Track.Y );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadCommanderCombat.Regroup( Source : TAniFigure; NewX, NewY : Integer );
const
  FailName : string = 'TUndeadCommanderCombat.Regroup';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if bTakeOrders then
    begin
      Character.WalkTo( NewX, NewY, 64 );
      Walking := true;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadCommanderCombat.WasAttacked( Source : TAniFigure; Damage : single );
var
  List : TStringList;
  iLoop : integer;
const
  FailName : string = 'TUndeadCommanderCombat.WasAttacked';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if Source <> character then
    begin
      if Character.IsEnemy( TCharacter( Source ) ) then
      begin
        Character.Track := TCharacter( Source );
      //Being Attacked- Get Friends to help
        List := GetPerceptibleAllies( Character, 1.25 );
        if Assigned( List ) then
        begin
          if List.Count >= 3 then //Pick first 3 in the list
          begin
            for iLoop := 0 to 2 do
            begin
              if assigned( TCharacter( List.Objects[ iLoop ] ).AI ) then
              begin
                if TCharacter( List.Objects[ iLoop ] ).AIMode = aiIdle then
                  TAI( TCharacter( List.Objects[ iLoop ] ).AI ).Follow( Character, Character.Track )
                else if TCharacter( List.Objects[ iLoop ] ).AIMode = aiCombat then
                  TAI( TCharacter( List.Objects[ iLoop ] ).AI ).CallToArms( Character, Character.Track );
              end;
            end;
          end
          else // Only have 1 or 2 friends-Get Everyone
          begin
            for iLoop := 0 to List.Count - 1 do
            begin
              if assigned( TCharacter( List.Objects[ iLoop ] ).AI ) then
              begin
                if TCharacter( List.Objects[ iLoop ] ).AIMode = aiIdle then
                  TAI( TCharacter( List.Objects[ iLoop ] ).AI ).Follow( Character, Character.Track )
                else if TCharacter( List.Objects[ iLoop ] ).AIMode = aiCombat then
                  TAI( TCharacter( List.Objects[ iLoop ] ).AI ).CallToArms( Character, Character.Track );
              end;
            end;
          end;
        end;
   //   MoveAway;
        Friendly := nil;
        list.free;
      end
      else
      begin
     //Character.Track:=TCharacter(Source);
     // Character.Attack(Character.Track);
      end;
    end;
    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TUndeadCommanderCombat.WasKilled( Source : TAniFigure );
var
  List : TStringList;
  iLoop : integer;
const
  FailName : string = 'TUndeadCommanderCombat.WasKilled';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

   //Tell everyone I died so they can save themselves
    List := GetPerceptibleAllies( Character, 1.5 );
    if Assigned( List ) then
    begin
      for iLoop := 0 to List.count - 1 do
      begin
        if Assigned( TCharacter( List.Objects[ iLoop ] ).AI ) then
          TAI( TCharacter( List.Objects[ iLoop ] ).AI ).NotifyOfDeath( character );
      end;
      list.free;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;


destructor TUndeadArcherCombat.Destroy;
const
  FailName : string = 'TUndeadArcher.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
    if Assigned( FriendsList ) then
      FriendsList.free;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

destructor TUndeadCasterCombat.Destroy;
const
  FailName : string = 'TUndeadCasterCombat.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
    if Assigned( FriendsList ) then
      FriendsList.free;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

destructor TUndeadCommanderCombat.Destroy;
const
  FailName : string = 'TUndeadCommanderCombat.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
    if Assigned( FriendsList ) then
      FriendsList.free;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

end.

