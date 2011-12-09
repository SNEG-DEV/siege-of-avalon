unit Engine;
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
  Anigrp30,
  AniDec30,
  ExtCtrls,
  Windows,
  Math,
  SysUtils,
  INIFiles,
  DirectX,
  LogFile,
  MMSystem,
  Graphics,
  Resource,
  Titles,
  DFX,
  MousePtr;

procedure CreateGlobals;
procedure FreeGlobals;
function GetCharactersInRadius( X, Y : longint; Radius : single ) : TStringList;
procedure RunScript( Me : TObject; Script : string );
procedure Converse( ObjectRef : TObject; Conversation : string );
function FormatFP( D : double ) : string;
function UnFormatFP( S : string ) : double;
procedure CheckCache;
procedure GetChapters( INI : TINIFile );
function SymbolReplacement( const Script : string ) : string;

const
  PI = 3.1415926535;
  pi2 = 2 * PI;

var
  Game : TAniView;
  GameMap : TAniMap;
  Figures, FigureInstances : TStringList;
  Sounds : TList;
  ActiveTriggers : TList;
  SayList : TList;
  ShadowImage : IDirectDrawSurface;
  GlowImage : TRLESprite;
  BaseLightType : longint;
  DefaultPath : string;
  CachePath : string;
  MapKnown : boolean;
  Themes : TStringList;
  GameName : string;
  LVLFile : string;
  CurrentScene : string;
  CurrentStartingPoint : string;
  TravelList : TStringList;
  PlotShadows : boolean;
  DefaultPants : TLayerResource;
  FemDefaultPants : TLayerResource;
  ElfDefaultPants : TLayerResource;
  RatResource : TCharacterResource;
  WolfResource : TCharacterResource;
  GIFToPOX : boolean;
  AllSpells : boolean;
  Bikini : boolean;
  Quests : TStringList;
  Adventures : TStringList;
  MouseCursor : TMousePtr;
  ReadCache : boolean;
  WriteCache : boolean;
  GlobalBrightness : longint;
  UseDirectSound : boolean;
  MaxCacheSize : int64;
  LoadingFromSaveFile : boolean;
  SpawnList : TList;
  Chapters : int64;
  TalkToMe : boolean;
  BodyRotResource : TResource;
  NoPageNumbers : boolean;
  UseSmallFont : boolean;
  NoTransit : boolean;
  SaveMsg : string;
  FullInvMsg : string;
  ChestMsg : string;
  SOLName : string;
  QuickSave : string;
  BlackScript : string;
implementation

uses
  AniDemo,
  Character,
  Parts,
  Effects,
  Display,
  Spells1,
  music;

type
  TCacheInfo = record
    Name : ShortString;
    Size : longint;
    Date : TDateTime;
  end;

const
  MaxScriptEntry = 40;
  Precision = 1000000;

var
  ScriptEntryCount : integer;

procedure CreateGlobals;
const
  FailName : string = 'Engine.CreateGlobals';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    Sprites := TSpriteManager.create( 400 );
    Figures := TStringList.create;
    Figures.capacity := 256;
    FigureInstances := TStringList.create;
    FigureInstances.capacity := 1024;
    Sounds := TList.create;
    ActiveTriggers := TList.create;
    PartManager := TPartManager.Create( ItemDB, XRefDB );
    TitlesManager := TTitlesDB.create( TitlesDB );
    NPCList := TList.create;
    NPCList.capacity := 5;
    Themes := TStringList.create;
    Themes.Sorted := true;
    TravelList := TStringList.Create;
    TravelList.Sorted := true;
    Quests := TStringList.create;
    Adventures := TStringList.Create;
    SayList := TList.Create;
    SpawnList := TList.create;
    ExText.Open( 'Engine' );
    SaveMsg := ExText.GetText( 'Save' );
    if SaveMsg = '' then
      SaveMsg := 'Game Saved';
    FullInvMsg := ExText.GetText( 'Full' );
    if FullInvMsg = '' then
      FullInvMsg := 'I cannot carry anymore';
    ChestMsg := ExText.GetText( 'Chest' );
    if ChestMsg = '' then
      ChestMsg := '>>> Previously missing items placed on ground <<<';
    SOLName := ExText.GetText( 'SOL' );
    if SOLName = '' then
      SOLName := 'Start of Level';
    QuickSave := ExText.GetText( 'QuickSave' );
    if QuickSave = '' then
      QuickSave := 'QuickSave';
    ExtExt.Close;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure FreeGlobals;
const
  FailName : string = 'Engine.FreeGlobals';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    Sprites.free; Sprites := nil;
    Figures.free; Figures := nil;
    FigureInstances.free; FigureInstances := nil;
    Sounds.free; Sounds := nil;
    ActiveTriggers.free; ActiveTriggers := nil;
    PartManager.free; PartManager := nil;
    TitlesManager.Free; TitlesManager := nil;
    NPCList.free; NPCList := nil;
    Themes.free; Themes := nil;
    TravelList.free; TravelList := nil;
    Quests.Free; Quests := nil;
    Adventures.Free; Adventures := nil;
    SayList.Free; SayList := nil;
    SpawnList.free; SpawnList := nil;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function GetCharactersInRadius( X, Y : longint; Radius : single ) : TStringList;
var
  i, j : integer;
  List : TList;
const
  FailName : string = 'Engine.GetCharactersInRadius';
begin
  result := nil;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    List := Game.FindInRadius( X, Y, Radius );
    if assigned( List ) then
    begin
      for i := 0 to List.count - 1 do
      begin
        if TAniFigure( List.items[ i ] ) is TCharacter then
        begin
          if not assigned( result ) then
            Result := TStringList.create;
          j := result.add( TCharacter( List.items[ i ] ).GUID );
          result.objects[ j ] := List.items[ i ];
        end;
      end;
    end;
    List.free;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure GetChapters( INI : TINIFile );
var
  List : TStringList;
  i : integer;
  S, Key, Value : string;
  KeyIndex : int64;
begin
  Chapters := 1;
  List := TStringList.create;
  try
    INI.ReadSectionValues( 'Chapters', List );
    for i := 0 to List.count - 1 do
    begin
      try
        S := List.Strings[ i ];
        Key := Parse( S, 0, '=' );
        Value := Parse( S, 1, '=' );
        if ( lowercase( copy( Key, 1, 8 ) ) = 'chapter ' ) and ( lowercase( Value ) = 'available' ) then
        begin
          KeyIndex := StrToInt( copy( Key, 9, length( Key ) - 8 ) ) - 1;
          Chapters := Chapters or ( 1 shl KeyIndex );
        end;
      except
      end;
    end;
  finally
    List.free;
  end;
end;

function FormatFP( D : double ) : string;
var
  L : int64;
begin
  L := trunc( D );
  result := inttostr( L ) + '.' + Inttostr( trunc( ( D - L ) * Precision ) );
end;

function UnFormatFP( S : string ) : double;
var
  i : integer;
  S1, S2 : string;
begin
  i := Pos( '.', S );
  if i > 0 then
  begin
    if i = 1 then
      S1 := '0'
    else
      S1 := copy( S, 1, i - 1 );
    S2 := copy( S, i + 1, Length( S ) - i );
    if Length( S2 ) > 6 then
      S2 := copy( S2, 1, 6 );
    if Length( S2 ) = 0 then
      result := StrToInt64( S1 )
    else
      result := StrToInt64( S1 ) + ( StrToInt64( S2 ) / Power( 10, Length( S2 ) ) );
  end
  else
    result := StrToInt64( S );
end;

function SymbolReplacement( const Script : string ) : string;
var
  S, S0, S1, S2 : string;
  i, j : integer;
  INI : TINIFile;
begin
  INI := nil;
  result := Script;
  i := Pos( '#', result );
  while ( i > 0 ) do
  begin
    S := Parse( result, 1, '#' );
    j := Length( S );
    if not assigned( INI ) then
      INI := TINIFile.create( DefaultPath + 'maps\symbols.ini' );
    S1 := Parse( S, 0, '.' );
    S2 := Parse( S, 1, '.' );
    S0 := INI.ReadString( S1, S2, '' );
    result := Copy( result, 1, i - 1 ) + S0 + Copy( result, i + j + 2, Length( result ) - i - j - 1 );
    i := Pos( '#', result );
  end;
  if assigned( INI ) then
    INI.free;

  i := Pos( #13#10, result ); //Strip CRLFs
  while ( i > 0 ) do
  begin
    result := Copy( result, 1, i - 1 ) + Copy( result, i + 2, Length( result ) - i - 2 );
    i := Pos( #13#10, result );
  end;

  i := Pos( #13, result ); //Strip CRs
  while ( i > 0 ) do
  begin
    result := Copy( result, 1, i - 1 ) + Copy( result, i + 1, Length( result ) - i - 1 );
    i := Pos( #13#10, result );
  end;

  i := Pos( #9, result ); //Strip Tabs
  while ( i > 0 ) do
  begin
    result := Copy( result, 1, i - 1 ) + Copy( result, i + 1, Length( result ) - i - 1 );
    i := Pos( #13#10, result );
  end;
end;

procedure RunScript( Me : TObject; Script : string );
var
  h, i, j, k : integer;
  r : Integer;
  T : single;
  X, Y : Integer;
  iLoop : integer;
  S0, S1, S2, S3, S4, S5, Command, Token, ObjectName : string;
  ObjectRef : TGameObject;
  Parms : string;
  Event : string;
  Effect : TEffect;
  IfFailed : boolean;
  IfLevel, NewIfLevel : integer;
  Object1 : TGameObject;
  List, Group : TStringList;
  NewEffect : TEffect;
  NewItem : TItem;
  NewCharacter : TCharacter;
const
  FailName : string = 'Engine.RunScript.';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    try
      if Script = '' then
        exit;
      if ScriptEntryCount > MaxScriptEntry then
      begin
        log.log( 'ERROR: Runscript exceeded maxscript' );
        exit; //Prevent infinite loop
      end;
      inc( ScriptEntryCount );

      Script := SymbolReplacement( Script );
      IfLevel := 0;
      i := 0;
      Command := Trim( Parse( Script, i, ';' ) );
      while Command <> '' do
      begin
        IfFailed := false;
        j := Pos( '(', Command );
        if ( j > 0 ) then
        begin
          Parms := Trim( Copy( Command, j + 1, length( Command ) - j ) );
          j := Pos( ')', Parms );
          if ( j > 0 ) then
            Parms := Trim( Copy( Parms, 1, j - 1 ) );
        end
        else
          Parms := '';
        Token := lowercase( Trim( Parse( Command, 0, '(' ) ) );
        j := Pos( '.', Token );
        if j > 0 then
        begin
          ObjectName := Trim( Copy( Token, 1, j - 1 ) );
          Token := Trim( Copy( Token, j + 1, length( Token ) - j ) );
          ObjectRef := nil;
          if ObjectName = 'player' then
            ObjectRef := Player
          else if ObjectName = 'current' then
            ObjectRef := Current
          else
          begin
            j := FigureInstances.IndexOf( ObjectName );
            if j >= 0 then
              ObjectRef := TGameObject( FigureInstances.objects[ j ] );
          end;
        end
        else
        begin
          ObjectName := '';
          ObjectRef := TGameObject( Me );
        end;
        if Token = 'doaction' then
        begin
          if assigned( ObjectRef ) then
          begin
            if ObjectRef is TSpriteObject then
            begin
              TSpriteObject( ObjectRef ).DoAction( Parms );
            end;
          end;
        end
        else if Token = 'doeffect' then
        begin
          if assigned( ObjectRef ) then
          begin
            if ObjectRef is TCharacter then
            begin
              NewEffect := GetNamedEffect( Parms );
              if assigned( NewEffect ) then
              begin
                TCharacter( ObjectRef ).AddEffect( NewEffect );
              end;
            end;
          end;
        end
        else if Token = 'causeevent' then
        begin
          Event := 'On' + Parms;
          if ObjectName = '' then
          begin
            for j := 0 to FigureInstances.count - 1 do
            begin
              ObjectRef := TGameObject( FigureInstances.objects[ j ] );
              if ObjectRef is TSpriteObject then
              begin
                if TSpriteObject( ObjectRef ).PropertyExists( Event ) then
                  RunScript( ObjectRef, TSpriteObject( ObjectRef ).Properties[ Event ] );
              end;
            end;
          end
          else
          begin
            if assigned( ObjectRef ) then
            begin
              if ObjectRef is TSpriteObject then
              begin
                if TSpriteObject( ObjectRef ).PropertyExists( Event ) then
                  RunScript( ObjectRef, TSpriteObject( ObjectRef ).Properties[ Event ] );
              end;
            end;
          end;
        end
        else if Token = 'ifprop' then
        begin
          IfFailed := true;
          if assigned( ObjectRef ) then
          begin
            S1 := Parse( Parms, 1, '=' );
            if S1 = '' then
            begin
              S1 := Parse( Parms, 1, '>' );
              if S1 = '' then
              begin
                S1 := Parse( Parms, 1, '<' );
                if S1 = '' then
                begin
                end
                else
                begin
                  S2 := Parse( Parms, 0, '<' );
                  IfFailed := StrToFloat( ObjectRef.Properties[ S2 ] ) >= StrToFloat( S1 );
                end;
              end
              else
              begin
                S2 := Parse( Parms, 0, '>' );
                IfFailed := StrToFloat( ObjectRef.Properties[ S2 ] ) <= StrToFloat( S1 );
              end;
            end
            else
            begin
              S2 := Parse( Parms, 0, '=' );
              IfFailed := lowercase( ObjectRef.Properties[ S2 ] ) <> lowercase( S1 );
            end;
          end;
          if not IfFailed then
            inc( IfLevel );
        end
        else if Token = 'ifnotprop' then
        begin
          IfFailed := false;
          if assigned( ObjectRef ) then
          begin
            S1 := Parse( Parms, 1, '=' );
            if S1 = '' then
            begin
              S1 := Parse( Parms, 1, '>' );
              if S1 = '' then
              begin
                S1 := Parse( Parms, 1, '<' );
                if S1 = '' then
                begin
                end
                else
                begin
                  S2 := Parse( Parms, 0, '<' );
                  IfFailed := StrToFloat( ObjectRef.Properties[ S2 ] ) < StrToFloat( S1 );
                end;
              end
              else
              begin
                S2 := Parse( Parms, 0, '>' );
                IfFailed := StrToFloat( ObjectRef.Properties[ S2 ] ) > StrToFloat( S1 );
              end;
            end
            else
            begin
              S2 := Parse( Parms, 0, '=' );
              IfFailed := lowercase( ObjectRef.Properties[ S2 ] ) = lowercase( S1 );
            end;
          end;
          if not IfFailed then
            inc( IfLevel );
        end
        else if Token = 'addtitle' then
        begin
          if assigned( ObjectRef ) then
          begin
            if ObjectRef is TCharacter then
            begin
              TCharacter( ObjectRef ).AddTitle( Parms );
            end;
          end;
        end
        else if Token = 'removetitle' then
        begin
          if assigned( ObjectRef ) then
          begin
            if ObjectRef is TCharacter then
            begin
              TCharacter( ObjectRef ).RemoveTitle( Parms );
            end;
          end;
        end
        else if Token = 'iftitle' then
        begin
          IfFailed := true;
          if assigned( ObjectRef ) then
          begin
            if ObjectRef is TCharacter then
            begin
              IfFailed := not TCharacter( ObjectRef ).TitleExists( Parms );
            end;
          end;
          if not IfFailed then
            inc( IfLevel );
        end
        else if Token = 'ifnottitle' then
        begin
          IfFailed := false;
          if assigned( ObjectRef ) then
          begin
            if ObjectRef is TCharacter then
            begin
              IfFailed := TCharacter( ObjectRef ).TitleExists( Parms );
            end;
          end;
          if not IfFailed then
            inc( IfLevel );
        end
        else if Token = 'ifplayer' then
        begin
          IfFailed := not ( ObjectRef = Player );
          if not IfFailed then
            inc( IfLevel );
        end
        else if Token = 'ifnotplayer' then
        begin
          IfFailed := ( ObjectRef = Player );
          if not IfFailed then
            inc( IfLevel );
        end
        else if Token = 'ifcurrent' then
        begin
          IfFailed := not ( ObjectRef = Current );
          if not IfFailed then
            inc( IfLevel );
        end
        else if Token = 'ifnotcurrent' then
        begin
          IfFailed := ( ObjectRef = Current );
          if not IfFailed then
            inc( IfLevel );
        end
        else if Token = 'ifalldead' then
        begin
          IfFailed := not AllDead( Parms );
          if not IfFailed then
            inc( IfLevel );
        end
        else if Token = 'ifnotalldead' then
        begin
          IfFailed := AllDead( Parms );
          if not IfFailed then
            inc( IfLevel );
        end
        else if Token = 'ifcombatmode' then
        begin
          IfFailed := not Current.CombatMode;
          if not IfFailed then
            inc( IfLevel );
        end
        else if Token = 'ifnotcombatmode' then
        begin
          IfFailed := Current.CombatMode;
          if not IfFailed then
            inc( IfLevel );
        end
        else if Token = 'ifparty' then
        begin
          IfFailed := not ( NPCList.count > 1 );
          if not IfFailed then
            inc( IfLevel );
        end
        else if Token = 'ifnoparty' then
        begin
          IfFailed := not ( NPCList.count < 2 );
          if not IfFailed then
            inc( IfLevel );
        end
        else if Token = 'ifinparty' then
        begin
          IfFailed := not ( NPCList.IndexOf( ObjectRef ) > 0 );
          if not IfFailed then
            inc( IfLevel );
        end
        else if Token = 'ifnotinparty' then
        begin
          IfFailed := ( NPCList.IndexOf( ObjectRef ) > 0 );
          if not IfFailed then
            inc( IfLevel );
        end
        else if Token = 'ifpartycount' then
        begin
          IfFailed := not ( StrToInt( Parms ) = NPCList.count );
          if not IfFailed then
            inc( IfLevel );
        end
        else if Token = 'fadetoblack' then
        begin
          frmMain.Active := False;
          frmMain.Timer3.Interval := 200;
          frmMain.Timer3.tag := 5;
          MouseCursor.Enabled := false;
          BlackScript := Parms;
          frmMain.Timer3.enabled := true;
        end
        else if ( Token = 'loadmap' ) or ( Token = 'loadmapext' ) then
        begin
          if Current.IntendToZone then
          begin
            S0 := Parse( Parms, 0, '|' );
            S1 := Parse( S0, 0, ',' );
            S2 := Parse( S0, 1, ',' );
            S3 := Parse( S0, 2, ',' );
            S4 := Parse( S0, 3, ',' );
            k := Pos( '|', Parms );
            if ( k = 0 ) or NoTransit then
              frmMain.LoadNewMap( S1, S2, S3, S4 )
            else
            begin
              S5 := copy( Parms, k, length( Parms ) - k + 1 );
              frmMain.BeginTransit( S1, S2, S3, S4, S5 );
            end;
          end;
        end
        else if Token = 'enableconsole' then
        begin
          frmMain.DisableConsole := false;
        end
        else if Token = 'disableconsole' then
        begin
          frmMain.DisableConsole := true;
          frmMain.OnKeyDown := nil;
          Game.OnMouseDown := nil;

          Game.OnMouseUp := nil;
          Game.OnMouseMove := nil;
          MouseCursor.Enabled := false;

    //      Game.ForceRefresh:=true;
    //      Game.Enabled := false;
        end
        else if Token = 'addtoparty' then
        begin
          if TCharacter( ObjectRef ).BaseHitPoints < 0 then
          begin // jrs 6Nov01 Restore norm/default base hitpoints to all party members
            Log.Log( '-- FIXUP, Party.HitPoints was ' + FloatToStr( TCharacter( ObjectRef ).HitPoints ) + ' and BaseHitPoints was ' + FloatToStr( TCharacter( ObjectRef ).BaseHitPoints ) );
            TCharacter( ObjectRef ).HitPoints := 20;
            TCharacter( ObjectRef ).CalcStats; // just to be sure
            Log.Log( '-- Party.HitPoints is now ' + FloatToStr( TCharacter( ObjectRef ).HitPoints ) + ' and BaseHitPoints is now ' + FloatToStr( TCharacter( ObjectRef ).BaseHitPoints ) );
          end;
          frmMain.AddToParty( ObjectRef );
        end
        else if Token = 'removefromparty' then
        begin
          if NPCList.Count > 1 then
          begin
            frmMain.ChangeFocus( player );
            frmMain.RemoveFromParty( ObjectRef );
          end;
        end
        else if Token = 'removeallpartymembers' then
        begin
          if NPCList.Count > 1 then
          begin
            frmMain.ChangeFocus( player );
            while NPCList.Count > 1 do
              if NPCList.Items[ 1 ] <> player then
              begin
                TCharacter( NPCList.Items[ 1 ] ).Alliance := '';
                frmMain.RemoveFromParty( NPCList.Items[ 1 ] );
              end;
          end;
        end
        else if Token = 'freezeparty' then
        begin
          if NPCList.Count > 1 then
          begin
            frmMain.ChangeFocus( player );
            for iLoop := 0 to NPCList.Count - 1 do
              if NPCList.Items[ iLoop ] <> player then
              begin
                TCharacter( NPCList.Items[ iLoop ] ).Frozen := true;
              end;
          end
        end
        else if Token = 'unfreezeparty' then
        begin
          for iLoop := 0 to NPCList.Count - 1 do
            if NPCList.Items[ iLoop ] <> player then
            begin
              TCharacter( NPCList.Items[ iLoop ] ).Frozen := false;
            end;
        end
        else if Token = 'savegame' then
        begin
          frmMain.SaveAGame( Parms );
        end
        else if ( Token = 'setproperty' ) or ( Token = 'setprop' ) then
        begin
          if assigned( ObjectRef ) then
          begin
            S0 := Parse( Parms, 0, '=' );
            S1 := Parse( Parms, 1, '=' );
            ObjectRef.Properties[ S0 ] := S1;
          end;
        end
        else if ( Token = 'meander' ) then
        begin
          if assigned( ObjectRef ) then
          begin
            if ObjectRef is TCharacter then
              if StrToInt( Parms ) <> 0 then
              begin
                r := random( StrToInt( Parms ) );
                T := pi2 * random( 360 ) / 360;
                X := round( r * cos( T ) ) + TCharacter( ObjectRef ).X;
                Y := round( r * sin( T ) ) + TCharacter( ObjectRef ).Y;
                TCharacter( ObjectRef ).walkTo( X, Y, 16 );
              end;
          end;
        end
        else if ( Token = 'adjustspeed' ) then
        begin
          if assigned( ObjectRef ) then
          begin
            TCharacterResource( TCharacter( ObjectRef ).Resource ).speed := StrToInt( Parms );
          end;
        end
        else if ( Token = 'adjusthealth' ) then
        begin
          if assigned( ObjectRef ) then
          begin
            TCharacter( ObjectRef ).TakeDamage( TCharacter( ObjectRef ), StrToInt( Parms ), 0, False );
           // TCharacter(ObjectRef).Wounds := TCharacter(ObjectRef).Wounds + StrToInt(Parms);
          end;
        end
        else if ( Token = 'adjustmaxhitpoints' ) then
        begin
          if assigned( ObjectRef ) then
          begin
            TCharacter( ObjectRef ).Hitpoints := TCharacter( ObjectRef ).Hitpoints + StrToInt( Parms );
            if TCharacter( ObjectRef ).Hitpoints < 1 then
              TCharacter( ObjectRef ).Hitpoints := 1;
          end;
        end
        else if ( Token = 'adjustmana' ) then
        begin
          if assigned( ObjectRef ) then
          begin
            TCharacter( ObjectRef ).drain := TCharacter( ObjectRef ).drain + StrToInt( Parms );
            if TCharacter( ObjectRef ).drain < 0 then
              TCharacter( ObjectRef ).drain := 0;
            if TCharacter( ObjectRef ).drain > TCharacter( ObjectRef ).Mana then
              TCharacter( ObjectRef ).drain := TCharacter( ObjectRef ).Mana;
          end;
        end
        else if ( Token = 'setgroupproperty' ) or ( Token = 'setgroupprop' ) then
        begin
          S2 := Parse( Parms, 0, ',' );
          S3 := Parse( Parms, 1, ',' );
          S0 := Parse( S3, 0, '=' );
          S1 := Parse( S3, 1, '=' );
          List := GetGroup( nil, S2 );
          if assigned( List ) then
          begin
            for k := 0 to List.count - 1 do
              TGameObject( List.objects[ k ] ).Properties[ S0 ] := S1;
          end;
        end
        else if Token = 'say' then
        begin
          if assigned( ObjectRef ) then
          begin
            if ObjectRef is TSpriteObject then
            begin
              TSpriteObject( ObjectRef ).Say( Parms, clWhite );
            end;
          end;
        end
        else if Token = 'converse' then
        begin
          Converse( ObjectRef, Parms );
        end
        else if Token = 'merchant' then
        begin
          if ObjectRef is TCharacter then
            frmMain.BeginMerchant( TCharacter( ObjectRef ) );
        end
        else if Token = 'cleartrack' then
        begin
          if ObjectRef is TCharacter then
            TCharacter( ObjectRef ).track := nil; ;
        end
        else if Token = 'changetheme' then
        begin
          frmMain.CurrentTheme := Parms;
        end
        else if Token = 'playmp3' then
        begin
          if Assigned( MusicLib ) then
          begin
            MusicLib.OpenThisSong( SoundPath + 'theme\' + Parms );
            MusicLib.PlayThisSong;
            frmMain.SoundTimer.Enabled := false;
          end;
        end
        else if Token = 'endmp3' then
        begin
          if Assigned( MusicLib ) then
          begin
            frmMain.SoundTimer.Enabled := false;
          end;
        end

        else if Token = 'setdeathscreen' then
        begin
          frmMain.DeathScreen := Parms;
        end
        else if Token = 'showending' then
        begin
          frmMain.ShowEnding;
        end
        else if Token = 'showmessage' then
        begin
          S1 := Parse( Parms, 0, ',' ); //Message
          S2 := Parse( Parms, 1, ',' ); //Time
          try
            frmMain.ShowQuickMessage( S1, StrToInt( S2 ) );
          except
          end;
        end
        else if Token = 'makeenemy' then
        begin
          if assigned( ObjectRef ) then
          begin
            if ObjectRef is TCharacter then
            begin
              TCharacter( ObjectRef ).MakeEnemy( Parms );
            end;
          end;
        end
        else if Token = 'makefriend' then
        begin
          if assigned( ObjectRef ) then
          begin
            if ObjectRef is TCharacter then
            begin
              TCharacter( ObjectRef ).MakeAlly( Parms );
            end;
          end;
        end
        else if Token = 'makeneutral' then
        begin
          if assigned( ObjectRef ) then
          begin
            if ObjectRef is TCharacter then
            begin
              TCharacter( ObjectRef ).MakeNeutral( Parms );
            end;
          end;
        end
        else if Token = 'alert' then
        begin
          if assigned( ObjectRef ) then
          begin
            if ObjectRef is TCharacter then
            begin
              S1 := Parse( Parms, 0, ',' ); //Group to Alert
              List := GetGroup( nil, S1 );
              if assigned( List ) then
              begin
                for k := 0 to List.count - 1 do
                begin
                  if List.Objects[ k ] is TCharacter then
                  begin
                    TCharacter( List.Objects[ k ] ).Track := TCharacter( ObjectRef );
                  end;
                end;
              end;
            end;
          end;
        end
        else if Token = 'spawn' then
        begin
          S1 := Parse( Parms, 0, ',' ); //Number of characters
          S2 := Parse( Parms, 1, ',' ); //PathCorner Group
          S3 := Parse( Parms, 2, ',' ); //Base GUID
          S4 := Parse( Parms, 3, ',' ); //Interval
          S5 := Parse( Parms, 4, ',' ); //Character group to clone from
          h := StrToInt( S4 );
          List := GetGroup( nil, S2 );
          if assigned( List ) then
          try
            Group := GetGroup( nil, S5 );
            if assigned( Group ) then
            try
              for j := Group.count - 1 downto 0 do
              begin
                if not ( Group.Objects[ j ] is TCharacter ) then
                  Group.Delete( j );
              end;
              for j := 1 to strtoint( S1 ) do
              begin
                k := random( Group.count );
                TCharacter( Group.Objects[ k ] ).Clone( TObject( NewCharacter ), S3 + inttostr( j ) );
                k := random( List.count );
                NewCharacter.SetPos( TGameObject( List.objects[ k ] ).X, TGameObject( List.objects[ k ] ).Y, TGameObject( List.objects[ k ] ).z );
                SpawnList.add( NewCharacter );
                NewCharacter.SpawnCount := ( j - 1 ) * h;
              end;
            finally
              Group.free;
            end;
          finally
            List.free;
          end;
        end
        else if Token = 'giveitem' then
        begin
          if assigned( ObjectRef ) then
          begin
            S1 := Parse( Parms, 0, ',' ); //Item Name
            S2 := Parse( Parms, 1, ',' ); //Recipient
            Object1 := GetGUID( S2 );
            if assigned( Object1 ) then
              TransferItem( ObjectRef, Object1, S1, true );
          end;
        end
        else if Token = 'takeitem' then
        begin
          if assigned( ObjectRef ) then
          begin
            S1 := Parse( Parms, 0, ',' ); //Item Name
            S2 := Parse( Parms, 1, ',' ); //Giver
            Object1 := GetGUID( S2 );
            if assigned( Object1 ) then
              TransferItem( Object1, ObjectRef, S1, true );
          end;
        end
        else if Token = 'ifgiveitem' then
        begin
          IfFailed := true;
          if assigned( ObjectRef ) then
          begin
            S1 := Parse( Parms, 0, ',' ); //Item Name
            S2 := Parse( Parms, 1, ',' ); //Recipient
            Object1 := GetGUID( S2 );
            if assigned( Object1 ) then
              IfFailed := not TransferItem( ObjectRef, Object1, S1, false );
          end;
          if not IfFailed then
            inc( IfLevel );
        end
        else if Token = 'iftakeitem' then
        begin
          IfFailed := true;
          if assigned( ObjectRef ) then
          begin
            S1 := Parse( Parms, 0, ',' ); //Item Name
            S2 := Parse( Parms, 1, ',' ); //Giver
            Object1 := GetGUID( S2 );
            if assigned( Object1 ) then
              IfFailed := not TransferItem( Object1, ObjectRef, S1, false );
          end;
          if not IfFailed then
            inc( IfLevel );
        end
        else if Token = 'removeitem' then
        begin
          if assigned( ObjectRef ) then
          begin
            S1 := Parse( Parms, 0, ',' ); //Item Name
            if ObjectRef is TCharacter then
            begin
              TCharacter( ObjectRef ).RemoveItem( S1 );
            end
            else if ObjectRef is TContainer then
            begin
              TContainer( ObjectRef ).RemoveItem( S1 );
            end;
          end;
        end
        else if Token = 'ifpartymember' then
        begin
          IfFailed := true;
          if assigned( ObjectRef ) then
          begin
            if ObjectRef is TCharacter then
            begin
              IfFailed := not TCharacter( ObjectRef ).PartyMember;
            end;
          end;
          if not IfFailed then
            inc( IfLevel );
        end
        else if Token = 'ifnotpartymember' then
        begin
          IfFailed := false;
          if assigned( ObjectRef ) then
          begin
            if ObjectRef is TCharacter then
            begin
              IfFailed := TCharacter( ObjectRef ).PartyMember;
            end;
          end;
          if not IfFailed then
            inc( IfLevel );
        end
        else if Token = 'ifhasitem' then
        begin
          IfFailed := true;
          if assigned( ObjectRef ) then
          begin
            if ObjectRef is TCharacter then
            begin
              IfFailed := not TCharacter( ObjectRef ).HasItem( Parms );
            end
            else if ObjectRef is TContainer then
            begin
              IfFailed := not TContainer( ObjectRef ).HasItem( Parms );
            end;
          end;
          if not IfFailed then
            inc( IfLevel );
        end
        else if Token = 'ifnothasitem' then
        begin
          IfFailed := false;
          if assigned( ObjectRef ) then
          begin
            if ObjectRef is TCharacter then
            begin
              IfFailed := TCharacter( ObjectRef ).HasItem( Parms );
            end
            else if ObjectRef is TContainer then
            begin
              IfFailed := TContainer( ObjectRef ).HasItem( Parms );
            end;
          end;
          if not IfFailed then
            inc( IfLevel );
        end
        else if Token = 'ifchapterexists' then
        begin
          IfFailed := true;
          try
            S1 := Parse( Parms, 0, ',' );
            IfFailed := ( int64( 1 shl ( strtoint( S1 ) - 1 ) ) and Chapters ) = 0;
          except
          end;
          if not IfFailed then
            inc( IfLevel );
        end
        else if Token = 'ifnotchapterexists' then
        begin
          IfFailed := false;
          try
            S1 := Parse( Parms, 0, ',' );
            IfFailed := ( int64( 1 shl ( strtoint( S1 ) - 1 ) ) and Chapters ) <> 0;
          except
          end;
          if not IfFailed then
            inc( IfLevel );
        end
        else if Token = 'setmaxparty' then
        begin
          try
            MaxPartyMembers := strtoint( Parms );
          except
          end;
        end
        else if Token = 'journalentry' then
        begin
          frmMain.AddLogEntry( Parms );
        end
        else if Token = 'addquest' then
        begin
          frmMain.AddQuest( Parms );
        end
        else if Token = 'removequest' then
        begin
          j := Quests.IndexOf( Parms );
          if j >= 0 then
            Quests.Delete( j );
        end
        else if Token = 'adventure' then
        begin
          frmMain.AddAdventure( Parms );
        end
        else if Token = 'additem' then
        begin
          S1 := Parse( Parms, 0, ',' ); //Item Name
          S2 := Parse( Parms, 1, ',' ); //GUID for new item
          if ObjectRef is TCharacter then
          begin
            NewItem := PartManager.LoadItem( S1, TCharacterResource( TCharacter( ObjectRef ).Resource ).NakedName );
            if assigned( NewItem ) then
            begin
              j := FigureInstances.add( S2 );
              FigureInstances.objects[ j ] := NewItem;
              NewItem.Resource := PartManager.GetLayerResource( NewItem.LayeredImage );
              Game.AddFigure( NewItem );
              if TCharacter( ObjectRef ).FindFreeInventoryXY( NewItem ) then
              begin
                TCharacter( ObjectRef ).Inventory.Add( NewItem );
                NewItem.Enabled := False;
              end
              else
              begin
                NewItem.SetPos( ObjectRef.X, ObjectRef.Y, ObjectRef.Z );
                NewItem.Enabled := True;
                NewItem.Init;
              end;
            end;
          end
          else if ObjectRef is TContainer then
          begin
            NewItem := PartManager.LoadItem( S1, '' );
            if assigned( NewItem ) then
            begin
              j := FigureInstances.add( S2 );
              FigureInstances.objects[ j ] := NewItem;
              NewItem.Resource := PartManager.GetLayerResource( NewItem.LayeredImage );
              Game.AddFigure( NewItem );
              if TContainer( ObjectRef ).FindFreeInventoryXY( NewItem ) then
              begin
                TContainer( ObjectRef ).Inventory.Add( NewItem );
                NewItem.Enabled := False;
              end
              else
              begin
                NewItem.SetPos( ObjectRef.X, ObjectRef.Y, ObjectRef.Z );
                NewItem.Enabled := True;
                NewItem.Init;
              end;
            end;
          end;
        end
        else if Token = 'moveto' then
        begin
          if assigned( ObjectRef ) and ( ObjectRef is TSpriteObject ) then
          begin
            Object1 := GetGUID( Parms );
            if assigned( Object1 ) then
            begin
              if ObjectRef is TCharacter then
                TCharacter( ObjectRef ).WalkTo( TGameObject( Object1 ).X, TGameObject( Object1 ).Y, 64 )
              else if ( ObjectRef is TGameObject ) then
                TSpriteObject( ObjectRef ).FindPathTo( TGameObject( Object1 ).X, TGameObject( Object1 ).Y, nil, 64 );
            end;
          end;
        end
        else if Token = 'teleport' then
        begin
          List := GetGroup( nil, Parms );
          if assigned( List ) then
          begin
            try
              j := 0;
              for k := 0 to List.count - 1 do
              begin
                if k >= NPCList.count then
                  break;
                if List.objects[ k ] is TPathCorner then
                begin
                  TCharacter( NPCList.items[ j ] ).SetPos( TPathCorner( List.objects[ k ] ).X, TPathCorner( List.objects[ k ] ).Y, TPathCorner( List.objects[ k ] ).Z );
                  TCharacter( NPCList.items[ j ] ).Stand;
                  inc( j );
                end;
              end;
            finally
              List.free;
            end;
          end
        end
        else if Token = 'moveparty' then
        begin
          List := GetGroup( nil, Parms );
          if assigned( List ) then
          begin
            try
              j := 0;
              for k := 0 to List.count - 1 do
              begin
                if k >= NPCList.count then
                  break;
                if List.objects[ k ] is TPathCorner then
                begin
                  if TCharacter( NPCList.items[ j ] ) <> player then
                  begin
                    TCharacter( NPCList.items[ j ] ).SetPos( TPathCorner( List.objects[ k ] ).X, TPathCorner( List.objects[ k ] ).Y, TPathCorner( List.objects[ k ] ).Z );
                    TCharacter( NPCList.items[ j ] ).Stand;
                  end;
                  inc( j );
                end;
              end;
            finally
              List.free;
            end;
          end
        end
        else if Token = 'setpos' then
        begin
          if assigned( ObjectRef ) then
          begin
            Object1 := GetGUID( Parms );
            if assigned( Object1 ) and ( Object1 is TGameObject ) then
            begin
              TSpriteObject( ObjectRef ).SetPos( TGameObject( Object1 ).X, TGameObject( Object1 ).Y, TSpriteObject( ObjectRef ).Z );
              if ObjectRef is TCharacter then
                TCharacter( ObjectRef ).Stand;
            end;
          end;
        end
        else if Token = 'reinit' then
        begin
          if assigned( ObjectRef ) and ( ObjectRef is TCharacter ) then
          begin
            TCharacter( ObjectRef ).AI.Init;
          end;
        end
        else if Token = 'invis' then
        begin
          if assigned( ObjectRef ) and ( ObjectRef is TCharacter ) then
          begin
            TCharacter( ObjectRef ).Alpha := 1;
            TCharacter( ObjectRef ).SpecialEffect := seTranslucent;
          end;
        end
        else if Token = 'cast' then
        begin
          if assigned( ObjectRef ) and ( ObjectRef is TCharacter ) then
          begin
            if assigned( TCharacter( ObjectRef ).CurrentSpell ) then
            begin
              Object1 := GetGUID( Parms );
              if assigned( Object1 ) and ( Object1 is TSpriteObject ) then
                TCharacter( Objectref ).Cast( TSpriteObject( Object1 ) );
            end;
          end;
        end
        else if Token = 'endif' then
        begin
          dec( IfLevel );
          if IfLevel < 0 then
            exit;
        end
        else if Token = 'else' then
        begin
          dec( IfLevel );
          if IfLevel < 0 then
            exit;
          NewIfLevel := IfLevel;
          repeat
            inc( i );
            Command := lowercase( Trim( Parse( Script, i, ';' ) ) );
            if Command = '' then
              exit
            else
            begin
              S1 := Parse( Command, 1, '.' );
              if S1 = '' then
                S1 := Command;
              if Copy( S1, 1, 2 ) = 'if' then
                inc( NewIfLevel )
              else if S1 = 'endif' then
                dec( NewIfLevel );
            end;
          until ( NewIfLevel = IfLevel ) and ( S1 = 'endif' );
        end;

        if IfFailed then
        begin
          NewIfLevel := IfLevel + 1;
          repeat
            inc( i );
            Command := lowercase( Trim( Parse( Script, i, ';' ) ) );
            if Command = '' then
              exit
            else
            begin
              S1 := Parse( Command, 1, '.' );
              if S1 = '' then
                S1 := Command;
              if Copy( S1, 1, 2 ) = 'if' then
                inc( NewIfLevel )
              else if S1 = 'endif' then
                dec( NewIfLevel );
            end;
          until ( ( NewIfLevel = IfLevel ) and ( S1 = 'endif' ) ) or ( ( NewIfLevel = IfLevel + 1 ) and ( S1 = 'else' ) );
          if S1 <> 'else' then
            inc( IfLevel );
        end;
        inc( i );
        Command := Trim( Parse( Script, i, ';' ) );
      end;

    except
      on E : Exception do
        Log.log( FailName, E.Message, [ ] );
    end;
  finally
    dec( ScriptEntryCount );
  end;

end;

procedure Converse( ObjectRef : TObject; Conversation : string );
const
  FailName : string = 'Engine.Converse.';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    frmMain.BeginConverse( TGameObject( ObjectRef ), Conversation );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function GetFileList( const cDir, cExt : string ) : TStringList;
var
  found : integer;
  SearchRec : TSearchRec;
  sDir, sExt : string;
begin
  if cDir[ length( cDir ) ] = '\' then
    sDir := copy( cDir, 1, length( cDir ) - 1 )
  else
    sDir := cDir;
  sExt := lowercase( cExt );
  if sExt[ 1 ] <> '.' then
    sExt := '.' + sExt;

  result := TStringList.create;
  found := FindFirst( sDir + '\*' + sExt, 0, SearchRec );
  while found = 0 do
  begin
    result.add( SearchRec.Name );
    found := FindNext( SearchRec );
  end;
  FindClose( SearchRec );
end;

procedure CheckCache;
var
  F : file;
//  INI: TINIFile;
  FileList : TStringList;
  TotalSize : int64;
  Dir : string;
  i, j : integer;
  L : longint;
//  L64: int64;
  CacheList, pList, pList1, pList2 : ^TCacheInfo;
  CacheItem : TCacheInfo;
  Count : integer;
begin
  Dir := DefaultPath + 'cache\';
  FileList := GetFileList( Dir, '.zit' );
  try
    Count := FileList.count;
    GetMem( CacheList, Count * sizeof( TCacheInfo ) );
    pList := CacheList;
    TotalSize := 0;
    //Calculate TotalSize, Load Name and Size fields
    for i := 0 to Count - 1 do
    begin
      pList^.Name := ChangeFileExt( FileList.strings[ i ], '' );


      AssignFile( F, Dir + FileList.strings[ i ] );
      try
        Reset( F, 1 );
        L := filesize( F );
        CloseFile( F );
      except
        L := 0;
      end;

      AssignFile( F, Dir + ChangeFileExt( FileList.strings[ i ], '.pit' ) );
      try
        Reset( F, 1 );
        L := L + filesize( F );
        CloseFile( F );
      except
      end;

      AssignFile( F, Dir + ChangeFileExt( FileList.strings[ i ], '.dit' ) );
      try
        Reset( F, 1 );
        L := L + filesize( F );
        CloseFile( F );
      except
      end;

      AssignFile( F, Dir + ChangeFileExt( FileList.strings[ i ], '.cit' ) );
      try
        Reset( F, 1 );
        L := L + filesize( F );
        if filesize( F ) > 12 then
        begin
          Seek( F, 12 );
          BlockRead( F, pList^.Date, sizeof( TDateTime ) );
        end
        else
        begin
          pList^.Date := 0;
        end;
        CloseFile( F );
      except
      end;
//Log.Log(pList^.Name+' - '+DateTimeToStr(pList^.Date)+' ('+inttostr(L)+')');

      pList^.Size := L;
      TotalSize := TotalSize + L;
      inc( pList );
    end;
  finally
    FileList.free;
  end;

  if TotalSize > MaxCacheSize then
  begin
    Log.Log( 'Clearing cache...' );
    //Load Date field
//    INI:=TINIFile.create(DefaultPath + 'siege.ini');

//    try
    {  pList:=CacheList;
      for i:=1 to Count do begin
        S:=INI.ReadString('Cache',pList^.Name,'');
        try
          L64:=StrToInt64(S);
        except
          L64:=0;
        end;
        pList^.Date:=TDateTime(addr(L64)^);
        inc(pList);
      end;    }

      //Sort by Date
    pList := CacheList;
    for i := 1 to Count - 1 do
    begin
      pList1 := pList;
      pList2 := pList;
      for j := i + 1 to Count do
      begin
        inc( pList1 );
        if pList1.Date < pList2.Date then
          pList2 := pList1;
      end;
      if pList2 <> pList then
      begin
        CacheItem := pList^;
        pList^ := pList2^;
        pList2^ := CacheItem;
      end;
      inc( pList );
    end;

    pList := CacheList;
    while ( TotalSize > MaxCacheSize ) do
    begin
      Log.Log( '  ' + pList^.Name );
      try
        DeleteFile( Dir + pList^.Name + '.zit' );
      except
      end;
      try
        DeleteFile( Dir + pList^.Name + '.pit' );
      except
      end;
      try
        DeleteFile( Dir + pList^.Name + '.dit' );
      except
      end;
      try
        DeleteFile( Dir + pList^.Name + '.cit' );
      except
      end;
      dec( TotalSize, pList^.Size );
//        INI.DeleteKey('Cache',pList^.Name);
      inc( pList );
    end;
//    finally
//      INI.free
//    end;
  end;
  FreeMem( CacheList );
end;

end.
