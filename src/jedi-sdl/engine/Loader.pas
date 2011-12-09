unit Loader;
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

interface

{$INCLUDE Anigrp30cfg.inc}

uses
  Classes,
  Windows,
  Graphics,
  SysUtils,
  Forms,
  INIFiles,
  Anigrp30,
  AniDec30,
{$IFDEF DirectX}
{$IFDEF DX5}
  DirectX,
{$ELSE}
  DirectDraw,
{$ENDIF}
  DXUtil,
{$ENDIF}
  Character,
  Resource,
  Engine,
  ItemDatabase,
  LogFile;

type
  TMapBlockTypes = ( mbHeader, mbMap, mbRectResourceList, mbDiamResourceList,
    mbObjResourceList, mbLayer0, mbLayer1, mbLayerDiamond, mbLayerTag, mbObject,
    mbScene, mbZones, mbSceneKnown, mbTheme, mbChapt, mbAltObjResourceList,
    mbAltRectResourceList, mbAltDiamResourceList, mbBB1, mbBB2 );

  TDiamondQuadrants = ( dqCenter, dqOE, dqNE, dqON, dqNW, dqOW, dqSW, dqOS,
    dqSE, dqIE, dqIN, dqIW, dqIS );

//These are used for reading the map file               ___2____
  TDTileInfo = packed record //    |\     /|    Rectangular tiles are divided
    Slice : array[ 1..4 ] of word; //  3 |  \ /  | 1  into 4 slices to support diamond
    Element : array[ 1..4 ] of byte; //    |  / \  |    tiles as shown.
    Variation : array[ 1..4 ] of byte; //    |/_____\|
  end; //        4

const
  Key = 5765587;
  EOB = $A0A0;

function LoadMap( var Map : TAniMap; const Filename, Scene : string; IgnoreDefaultSprites, IgnoreSceneSprites : boolean ) : boolean;
function GetlevelCode( const Filename : string ) : int64;

implementation

uses
  Parts,
  AniDemo{$IFDEF VER150},
  Variants{$ENDIF};

function LoadMap( var Map : TAniMap; const Filename, Scene : string; IgnoreDefaultSprites, IgnoreSceneSprites : boolean ) : boolean;
var
  SceneName : string;
  SceneCount : integer;
  SceneIndex : integer;
  Stream : TFileStream;
  Version : longint;
  L, BlockSize : longint;
  FillIndex : longint;
  FillVarLimit : longint;
  Width, Height : longint;
  Position : longint;
  S : string;
  RNames : TStringList;
  DNames : TStringList;
  ONames : TStringList;
  RIndex, DIndex, OIndex, ZIndex : variant;
  DVariations : variant;
  TileIndex : integer;
  INI : TStringIniFile;
  X, Y : longint;
  i, j : integer;
  Resource : TResource;
  Zones : TStringList;
  Zone : integer;
  UseCache : boolean;
  CacheFileA, CacheFileB : string;
  StaticObject : boolean;
  NewObject : boolean;
  DumpMode : boolean;
  LoadCount : integer;
  StartObjectBlocks : boolean;

  procedure ReadHeaderBlock;
  var
    L : longint;
  const
    FailName : string = 'Loader.readHeaderBlock';
  begin
{$IFDEF DODEBUG}
    if ( CurrDbgLvl >= DbgLvlSevere ) then
      Log.LogEntry( FailName );
{$ENDIF}
    try
      Log.Log( 'Reading header block' );
      Stream.Read( L, sizeof( L ) );
      Map.TileSize := L;
    except
      on E : Exception do
        Log.log( FailName + E.Message );
    end;
  end;

  procedure ReadMapHeaderBlock;
  var
    L, k : longint;
    i, j : longint;
  const
    FailName : string = 'Loader.ReadMapheaderBlock';
  begin
{$IFDEF DODEBUG}
    if ( CurrDbgLvl >= DbgLvlSevere ) then
      Log.LogEntry( FailName );
{$ENDIF}
    try

      Log.Log( 'Reading map header block' );
      Stream.Read( Width, sizeof( Width ) );
      Stream.Read( Height, sizeof( Height ) );
      Map.Width := Width;
      Map.Height := Height;
      Log.Log( '  Map Size=' + IntToStr( Width ) + 'x' + IntToStr( Height ) );

    //Random # seed and FillIndex
      Stream.read( L, sizeof( L ) );
      RandSeed := L;
      Log.Log( '  Seed=' + IntToStr( L ) );
      Stream.read( FillIndex, sizeof( FillIndex ) );
      inc( FillIndex );
      Stream.read( FillVarLimit, sizeof( FillVarLimit ) );
      dec( FillVarLimit );
      L := RIndex[ FillIndex ];

      for j := 0 to Height - 1 do
      begin
        for i := 0 to Width - 1 do
        begin
          k := Random( FillVarLimit );
          Map.SetTile( i, j, 0, 1, L + k );
        end;
      end;
    except
      on E : Exception do
        Log.log( FailName + E.Message );
    end;
  end;

  procedure ReadResourceList( FResources : TStringList );
  var
    cText : string;
    L : longint;
  const
    FailName : string = 'Loader.ReadResourcesList';
  begin
{$IFDEF DODEBUG}
    if ( CurrDbgLvl >= DbgLvlSevere ) then
      Log.LogEntry( FailName );
{$ENDIF}
    try
      Log.Log( 'Reading resource list' );
      FResources.Clear;
    //get size
      Stream.Read( L, sizeof( L ) );
      if L > 0 then
      begin
        SetLength( cText, L );
        Stream.Read( cText[ 1 ], L );
        FResources.Text := cText;
      end;
    except
      on E : Exception do
        Log.log( FailName + E.Message );
    end;
  end;

  procedure ReadLayer0Block;
  var
    i, j : longint;
    iMapPos : longint;
    iPos : word;
    MaxPos : longint;
    W : word;
    V : byte;
  const
    FailName : string = 'Loader.ReadLayer0Block';
  begin
{$IFDEF DODEBUG}
    if ( CurrDbgLvl >= DbgLvlSevere ) then
      Log.LogEntry( FailName );
{$ENDIF}
    try

      Log.Log( 'Reading layer 0 data block' );
      MaxPos := Position + BlockSize;
      while Stream.Position < MaxPos do
      begin
        Stream.Read( iMapPos, sizeof( iMapPos ) );
        i := iMapPos mod Width;
        j := iMapPos div Width;
        Stream.Read( iPos, sizeof( iPos ) ); //read local-based index
        inc( iPos );
        Stream.Read( W, sizeof( W ) ); //Offset
        Stream.Read( V, sizeof( V ) ); //Variation

        if ( W = 0 ) then
          Map.SetTile( i, j, 0, 1, RIndex[ iPos ] + V - 1 );
      end;
    except
      on E : Exception do
        Log.log( FailName + E.Message );
    end;
  end;

  procedure ReadLayer1Block;
  var
    i, j : longint;
    iMapPos : longint;
    iPos : word;
    MaxPos : longint;
    W : word;
    V : byte;
  const
    FailName : string = 'Loader.ReadLayer1Block';
  begin
{$IFDEF DODEBUG}
    if ( CurrDbgLvl >= DbgLvlSevere ) then
      Log.LogEntry( FailName );
{$ENDIF}
    try
      Log.Log( 'Reading layer 1 data block' );
      MaxPos := Position + BlockSize;
      while Stream.Position < MaxPos do
      begin
        Stream.Read( iMapPos, sizeof( iMapPos ) );
        i := iMapPos mod Width;
        j := iMapPos div Width;
        Stream.Read( iPos, sizeof( iPos ) ); //read local-based index
        inc( iPos );
        Stream.Read( W, sizeof( W ) ); //Offset
        Stream.Read( V, sizeof( V ) ); //Variation

        if W = 0 then
          Map.SetTile( i, j, 1, 1, RIndex[ iPos ] + V - 1 );
      end;
    except
      on E : Exception do
        Log.log( FailName + E.Message );
    end;
  end;

  procedure ReadDiamondBlock;
  var
    iMapPos : longint;
    iLoop : longint;
    k, iSlice : byte;
    iPos : word;
    aDiam : TDTileInfo;
    MaxPos : longint;
    i, j : longint;
    TSlice, RSlice, BSlice, LSlice : word;
    B : byte;
  const
    FailName : string = 'Loader.readDiamondBlock';
  begin
{$IFDEF DODEBUG}
    if ( CurrDbgLvl >= DbgLvlSevere ) then
      Log.LogEntry( FailName );
{$ENDIF}
    try

      Log.Log( 'Reading diamond data block' );
      MaxPos := Position + BlockSize;
      while Stream.Position < MaxPos do
      begin
        Stream.Read( iMapPos, sizeof( iMapPos ) );
        ZeroMemory( @aDiam, sizeof( aDiam ) );
        i := iMapPos mod Width;
        j := iMapPos div Width;
        Stream.Read( iSlice, sizeof( iSlice ) ); //how many slices filled
        for iLoop := 1 to iSlice do
        begin
          Stream.Read( k, sizeof( k ) ); //slice (1-4)
          Stream.Read( iPos, sizeof( iPos ) ); //read local-based index
          aDiam.Slice[ k ] := iPos;
          inc( iPos );
          Stream.Read( B, sizeof( B ) ); //element
          aDiam.Element[ k ] := B;
          Stream.Read( B, sizeof( B ) ); //variation
          aDiam.Variation[ k ] := B;
        end;
      //Since each element has any number of variations, I had to come up with this monstrosity
      //to track them.
        try
          if aDiam.Variation[ 2 ] = 0 then
            TSlice := 0
          else
            TSlice := DIndex[ aDiam.Slice[ 2 ] ] + DVariations[ aDiam.Slice[ 2 ], aDiam.Element[ 2 ] ][ aDiam.Variation[ 2 ] ];
          if aDiam.Variation[ 1 ] = 0 then
            RSlice := 0
          else
            RSlice := DIndex[ aDiam.Slice[ 1 ] ] + DVariations[ aDiam.Slice[ 1 ], aDiam.Element[ 1 ] ][ aDiam.Variation[ 1 ] ];
          if aDiam.Variation[ 4 ] = 0 then
            BSlice := 0
          else
            BSlice := DIndex[ aDiam.Slice[ 4 ] ] + DVariations[ aDiam.Slice[ 4 ], aDiam.Element[ 4 ] ][ aDiam.Variation[ 4 ] ];
          if aDiam.Variation[ 3 ] = 0 then
            LSlice := 0
          else
            LSlice := DIndex[ aDiam.Slice[ 3 ] ] + DVariations[ aDiam.Slice[ 3 ], aDiam.Element[ 3 ] ][ aDiam.Variation[ 3 ] ];
          Map.SetDiamondTile( i, j, 2, TSlice, 2, RSlice, 2, BSlice, 2, LSlice );
        except
        end;
      end;
    except
      on E : Exception do
        Log.log( FailName + E.Message );
    end;

  end;

  procedure ReadObjectBlock;
  var
    L : longint;
    X, Y, Z : longint;
    Index, ImageIndex : longint;
    cText : string;
    i, j, k : integer;
    Attributes : TStringList;
    NewResource : TResource;
    S, S1, S2, S3, S4, S5 : string;
    GUID : string;
    Version : integer;
    CreateEnabled : boolean;
    pBase, pItem, pRefItem : PItemInstanceInfo;
    ItemIndex : word;
    A : single;
    XRayID : Smallint;
    AutoTransparent : boolean;
    Slope : single;
    Collidable : Boolean;
    InitDelta : boolean;
    RefDelta, Delta : integer;
    InScene : string;
    Mask : word;

    procedure AddLight;
    var
      Intensity, Radius : integer;
      Color : TColor;
      FlickerType : TFlicker;
      zone : word;
      ZoneStream : TFileStream;
    const
      FailName : string = 'Loader.Addlight';
    begin
  {$IFDEF DODEBUG}
      if ( CurrDbgLvl >= DbgLvlSevere ) then
        Log.LogEntry( FailName );
  {$ENDIF}
      try

        try
          Intensity := StrToInt( Attributes.values[ 'Intensity' ] );
        except
          Intensity := 100;
        end;
        try
          Color := StrToInt( Attributes.values[ 'Color' ] );
        except
          Color := $FFFFFF;
        end;
        try
          Radius := StrToInt( Attributes.values[ 'Radius' ] );
        except
          Radius := 128;
        end;
        S := lowercase( Attributes.values[ 'Flicker' ] );
        if S = 'torch' then
          FlickerType := flTorch
        else if S = 'candle' then
          FlickerType := flCandle
        else if S = 'fluorescent' then
          FlickerType := flFluorescent
        else
          FlickerType := flNone;
        if not UseCache then
        begin
          zone := Map.AddLight( Color, Intensity, Radius, FlickerType, X, Y, Z );
    //    Log.Log('Light Zone: '+inttostr(zone));

          S := DefaultPath + 'Maps/' + ChangeFileExt( ExtractFilename( LVLFile ), '.zit' );
          if FileExists( S ) then
          try
            ZoneStream := TFileStream.Create( S, fmOpenRead or fmShareDenyWrite );
            try
              if ZoneStream.Size >= ( zone + 1 ) * 24 then
              begin //we know this data is 24 bytes long
                ZoneStream.Position := zone * 24;
                try
                  TZone( Map.Zones.items[ zone ] ).LoadFromStream( ZoneStream );
                except
                end;
              end;
            finally
              ZoneStream.free;
            end;
          except
          end;

        end;
      except
        on E : Exception do
          Log.log( FailName + E.Message );
      end;

    end;

  const
    FailName : string = 'Loader.ReadObjectBlock';
  begin
{$IFDEF DODEBUG}
    if ( CurrDbgLvl >= DbgLvlSevere ) then
      Log.LogEntry( FailName );
{$ENDIF}
    try

      if StartObjectBlocks then
      begin
        Log.Log( 'Reading object blocks' );
        StartObjectBlocks := false;
      end;
      Stream.Read( X, sizeof( X ) );
      Stream.Read( Y, sizeof( Y ) );
      Stream.Read( Z, sizeof( Z ) );

      Stream.Read( Index, sizeof( Index ) );
      inc( Index ); //local object index
      if ( Index = 0 ) then
        exit;
      Stream.Read( ImageIndex, sizeof( ImageIndex ) );

    //attr stringlist
      Attributes := nil;
      Stream.Read( L, sizeof( L ) );
      if L > 0 then
      begin
        SetLength( cText, L );
        Stream.Read( cText[ 1 ], L );
        Attributes := TStringList.Create;
        Attributes.Text := cText;

        InScene := lowercase( Attributes.values[ 'InScene' ] ); //Check to see if object is in current scene
        if ( InScene <> '' ) then
        begin
          if ( Pos( '[' + SceneName + ']', InScene ) = 0 ) then
          begin
            Attributes.free;
            Log.Log( '  Object not in scene' );
            exit;
          end;
        end;

//      for i:=0 to Attributes.Count-1 do begin
//        Log.Log('  '+Attributes.strings[i]);
//      end;
      end;
      if OIndex[ Index ] < 0 then
      begin
        if assigned( Attributes ) then
        begin //We must at least have a base class
          inc( Y, Z );
          i := -OIndex[ Index ] - 1;
//        Log.Log('  '+Figures.strings[i]+' ('+IntToStr(Index)+') at ('+IntToStr(X)+','+IntToStr(Y)+','+IntToStr(Z)+')');
          GUID := lowercase( Attributes.values[ 'GUID' ] );
          j := FigureInstances.add( GUID );
          GUID := '';
          S := Attributes.values[ 'SpriteVersion' ];
          try
            Version := StrToInt( S );
          except
            Version := 0;
          end;

          if ( IgnoreDefaultSprites and ( InScene = '' ) ) or ( IgnoreSceneSprites and ( InScene <> '' ) ) then
            CreateEnabled := Version > 0
          else
            CreateEnabled := true;

          S := lowercase( Attributes.values[ 'BaseClassAncestor' ] );
          if S = 'character' then
          begin
            FigureInstances.objects[ j ] := TCharacter.create(
              X + TResource( Figures.objects[ i ] ).CenterX,
              Y - TResource( Figures.objects[ i ] ).FrameHeight + TResource( Figures.objects[ i ] ).CenterY, Z,
              ImageIndex + 1, CreateEnabled );
            with TCharacter( FigureInstances.objects[ j ] ) do
            begin
              CreatedFromLvlFile := true;
              AIMode := aiIdle; //Some defaults
              IdleAI := 'Meander';
              CombatAI := 'BasicFight';
              PartyAI := 'Companion';
              Resource := TResource( Figures.objects[ i ] );
              MouseRect := Rect( CenterX - Radius, 24, CenterX + Radius, CenterY );
              LoadProperties( Attributes );
              UseDefaultEquipment := true;
//            Init;
            end;
          end
          else if S = 'container' then
          begin
            FigureInstances.objects[ j ] := TContainer.create(
              X + TResource( Figures.objects[ i ] ).CenterX,
              Y - TResource( Figures.objects[ i ] ).FrameHeight + TResource( Figures.objects[ i ] ).CenterY, Z,
              ImageIndex + 1, CreateEnabled );
            with TContainer( FigureInstances.objects[ j ] ) do
            begin
              CreatedFromLvlFile := true;
              Resource := TResource( Figures.objects[ i ] );
              Attributes.Sort;
              LoadProperties( Attributes );
//            Init;
            end;
          end
          else if S = 'door' then
          begin
            if TAniResource( Figures.objects[ i ] ) is TDoorResource then
            begin
              FigureInstances.objects[ j ] := TDoor.create(
                X + TResource( Figures.objects[ i ] ).CenterX,
                Y - TResource( Figures.objects[ i ] ).FrameHeight + TResource( Figures.objects[ i ] ).CenterY, Z,
                ImageIndex + 1, CreateEnabled );
              with TDoor( FigureInstances.objects[ j ] ) do
              begin
                CreatedFromLvlFile := true;
                Resource := TResource( Figures.objects[ i ] );
                LoadProperties( Attributes );
                if not UseCache then
                  Init;
              end;
            end
            else
            begin
              Log.Log( '  *** Error: Door does not have a door resource - removing instance' );
            end;
          end
          else
          begin
            if assigned( Figures.objects[ i ] ) then
            begin
              if S <> 'spriteobject' then
                Log.Log( '  *** Error: Unknown base class (' + S + ') - assuming generic sprite' );
              FigureInstances.objects[ j ] := TSpriteObject.create(
                X + TResource( Figures.objects[ i ] ).CenterX,
                Y - TResource( Figures.objects[ i ] ).FrameHeight + TResource( Figures.objects[ i ] ).CenterY, Z,
                ImageIndex + 1, CreateEnabled );
              with TSpriteObject( FigureInstances.objects[ j ] ) do
              begin
                CreatedFromLvlFile := true;
                Resource := TResource( Figures.objects[ i ] );
                LoadProperties( Attributes );
//              Init;
              end;
            end
            else
            begin
              Log.Log( '  *** Error: Unknown base class (' + S + ')' );
            end;
          end;
        end;
      end
      else if OIndex[ Index ] = 0 then
      begin
        if assigned( Attributes ) then
        begin //We must at least have a base class
          inc( Y, Z );
 //       Log.Log('  Abstract object ('+IntToStr(Index)+') at ('+IntToStr(X)+','+IntToStr(Y)+','+IntToStr(Z)+')');
          GUID := lowercase( Attributes.values[ 'GUID' ] );
          j := FigureInstances.add( GUID );
          GUID := '';

          S := lowercase( Attributes.values[ 'BaseClassAncestor' ] );
          if S = 'pathcorner' then
          begin
            FigureInstances.objects[ j ] := TPathCorner.create( X, Y, Z );
            with TPathCorner( FigureInstances.objects[ j ] ) do
            begin
              CreatedFromLvlFile := true;
              LoadProperties( Attributes );
//            Init;
            end;
          end
          else if S = 'item' then
          begin
            S := Attributes.Values[ 'ItemName' ];
            FigureInstances.objects[ j ] := PartManager.LoadItem( S, '' );
            if assigned( FigureInstances.objects[ j ] ) then
            begin
              S1 := PartManager.GetImageFile( TItem( FigureInstances.objects[ j ] ).PartName, '' );
              NewResource := PartManager.GetLayerResource( S1 );
              if assigned( NewResource ) then
              begin
                TItem( FigureInstances.objects[ j ] ).SetPos( X + NewResource.CenterX, Y - NewResource.FrameHeight + NewResource.CenterY, Z );
                with TItem( FigureInstances.objects[ j ] ) do
                begin
                  CreatedFromLvlFile := true;
                  Enabled := true;
                  Resource := NewResource;
                  LoadProperties( Attributes );
//                Init;
                end;
              end
              else
              begin
                FigureInstances.Delete( j ); //If we were unable to load the resource, delete the reference
              end;
            end;
          end
          else if S = 'light' then
          begin
            AddLight;
          end
          else if S = 'soundplayer' then
          begin
            FigureInstances.objects[ j ] := TSoundPlayer.create( X, Y, Z );
            Sounds.add( FigureInstances.objects[ j ] );
            with TSoundPlayer( FigureInstances.objects[ j ] ) do
            begin
              CreatedFromLvlFile := true;
              LoadProperties( Attributes );
            end;
          end
          else if S = 'eventtimer' then
          begin
            FigureInstances.objects[ j ] := TEventTimer.create( X, Y, Z );
            Sounds.add( FigureInstances.objects[ j ] );
            with TEventTimer( FigureInstances.objects[ j ] ) do
            begin
              CreatedFromLvlFile := true;
              LoadProperties( Attributes );
            end;
          end
          else if ( S = 'gridspace' ) or ( S = 'gscollision' ) or
            ( S = 'gsfilterid' ) or ( S = 'gsontrigger' ) then
          begin

            S := Attributes.values[ 'OnTrigger' ];
            if S = '' then
            begin
              j := 0;
            end
            else
            begin
              FigureInstances.objects[ j ] := TTrigger.create( X, Y, Z );
              with TTrigger( FigureInstances.objects[ j ] ) do
              begin
                CreatedFromLvlFile := true;
                LoadProperties( Attributes );
              end;
              inc( j ); //j is the trigger id
            end;

            S3 := Attributes.values[ 'CollisionMasks' ];
            S4 := Attributes.values[ 'FilterMasks' ];
            S5 := Attributes.values[ 'TriggerMasks' ];

            S := Attributes.values[ 'FilterID' ];
            if S = '' then
              i := 0
            else
              i := StrToInt( S );

            S := Attributes.values[ 'ListOfTiles' ];
            k := 0;
            S1 := Parse( S, k, ' ' );
            while ( S1 <> '' ) do
            begin
              S2 := Parse( S1, 0, ',' );
              X := StrToInt( Copy( S2, 2, length( S2 ) - 1 ) ) div Map.TileWidth;
              S2 := Parse( S1, 1, ',' );
              Y := StrToInt( Copy( S2, 1, length( S2 ) - 1 ) ) div Map.TileHeight;
              if j <> 0 then
              begin
                if S5 = '' then
                  Mask := $FFFF
                else
                  Mask := strtoint( Parse( S5, k, ',' ) );
                Map.SetTrigger( X, Y, j, Mask );
              end;
              if i <> 0 then
              begin
                if S4 = '' then
                  Mask := $FFFF
                else
                  Mask := strtoint( Parse( S4, k, ',' ) );
                Map.SetFilter( X, Y, i, Mask );
              end;
              if S3 <> '' then
              begin
                Mask := strtoint( Parse( S3, k, ',' ) );
                Map.SetCollisionMask( X, Y, Mask );
              end;
              inc( k );
              S1 := Parse( S, k, ' ' );
            end;
          end
          else
          begin
            Log.Log( '  *** Error: Unknown base class (' + S + ') - assuming generic abstract' );
            FigureInstances.objects[ j ] := TAbstractObject.create( X, Y, Z );
//Log.Log('*** '+inttostr(Index));
//for k:=0 to Attributes.count-1 do
//  Log.Log('  '+Attributes.Strings[k]);
            with TAbstractObject( FigureInstances.objects[ j ] ) do
            begin
              CreatedFromLvlFile := true;
              LoadProperties( Attributes );
//            Init;
            end;
          end;
        end;
      end
      else if OIndex[ Index ] > 0 then
      begin
        if UseCache then
        begin
//        Log.Log('Skipping cached item');
        end
        else
        begin
//        Log.Log('  #'+IntToStr(OIndex[Index])+' at ('+IntToStr(X)+','+IntToStr(Y)+','+IntToStr(Z)+')');
          if lowercase( Attributes.Values[ 'Collidable' ] ) = 'false' then
            Collidable := False
          else
            Collidable := True;
          if assigned( Attributes ) then
          begin
            S := Attributes.values[ 'FilterID' ];
            if S = '' then
              i := 0
            else
              i := StrToInt( S );
            pBase := Map.AddItem( ZIndex[ Index ], OIndex[ Index ] + ImageIndex, X, Y, Z, -i, Collidable );
          end
          else
            pBase := Map.AddItem( ZIndex[ Index ], OIndex[ Index ] + ImageIndex, X, Y, Z, 0, Collidable );

          AutoTransparent := pBase.AutoTransparent;
          XRayID := pBase.XRayID;
          Slope := pBase.Slope0;
          S := lowercase( Attributes.values[ 'ini.XRayable' ] );
          if ( S = '' ) then
          else if ( S = 'no' ) then
          begin
            AutoTransparent := False;
            XRayID := 0;
            Slope := 0;
          end
          else
          begin
            try
              A := UnFormatFP( S );
              if A <> 90 then
              begin
                A := -PI * A / 180;
                Slope := Sin( A ) / Cos( A ) / 2;
                AutoTransparent := True;
              end;
            except
            end;
            S := Attributes.values[ 'XRayID' ];
            if S <> '' then
              XRayID := StrToInt( S );
          end;

          pRefItem := pBase;
          InitDelta := false;
          RefDelta := 0;
          pItem := pBase;
          while True do
          begin
            pItem.AutoTransparent := AutoTransparent;
            if AutoTransparent then
              pItem.Slope0 := Slope;
            pItem.XRayID := XRayID;

            if pItem.VHeight <> 0 then
            begin
              Delta := pItem.Y - round( ( pItem.X - pBase.X ) * Slope );
              if InitDelta then
              begin
                if Delta < RefDelta then
                begin
                  RefDelta := Delta;
                  pRefItem := pItem
                end;
              end
              else
              begin
                RefDelta := Delta;
                pRefItem := pItem;
                InitDelta := true;
              end;
            end;

            if pItem.Last then
              break;
            inc( pItem );
          end;

          ItemIndex := Map.GetItemIndex( pRefItem );
          pItem := PBase;
          while True do
          begin
            pItem.RefItem := ItemIndex;
            if pItem.Last then
              break;
            inc( pItem );
          end;
        end;

      end;

      Attributes.Free;
    except
      on E : Exception do
        Log.log( FailName + E.Message );
    end;

  end;

  function LoadIndexes( S : string ) : Variant;
  var
    i, j : integer;
    N : string;
  const
    FailName : string = 'Loader.LoadIndexes';
  begin
{$IFDEF DODEBUG}
    if ( CurrDbgLvl >= DbgLvlSevere ) then
      Log.LogEntry( FailName );
{$ENDIF}
    try
      i := 0;
      repeat
        N := Parse( S, i, ',' );
        inc( i );
      until N = '';
      dec( i );
      if i > 0 then
      begin
        result := VarArrayCreate( [ 0, i ], varInteger );
        result[ 0 ] := 0;
        for j := 1 to i do
        begin
          result[ j ] := StrToInt( Parse( S, j - 1, ',' ) ) - 1;
        end;
      end
      else
        result := varEmpty;
    except
      on E : Exception do
        Log.log( FailName + E.Message );
    end;
  end;

  procedure ReadSceneBlock;
  var
    AmbientColor, AmbientIntensity : longint;
  const
    FailName : string = 'Loader.readSceneBlock';
  begin
{$IFDEF DODEBUG}
    if ( CurrDbgLvl >= DbgLvlSevere ) then
      Log.LogEntry( FailName );
{$ENDIF}
    try
      Log.Log( 'Reading scene' );
      inc( SceneCount );

      Stream.Read( AmbientColor, sizeof( AmbientColor ) );
      Stream.Read( AmbientIntensity, sizeof( AmbientIntensity ) );
      AmbientIntensity := AmbientIntensity + GlobalBrightness;

      Stream.Read( BaseLightType, sizeof( BaseLightType ) );

      Stream.Read( L, sizeof( L ) ); //size of string coming
      SetLength( S, L );
      Stream.Read( S[ 1 ], L );
      Log.Log( '  ' + S );
      S := lowercase( S );
      if ( S = lowercase( SceneName ) ) or ( ( S = 'default scene' ) and ( SceneName = '' ) ) then
      begin
        SceneIndex := SceneCount;
        Map.AmbientColor := AmbientColor;
        Map.AmbientIntensity := AmbientIntensity;
        Log.Log( 'Applying scene' );
        Log.Log( 'AmbientColor=' + IntToStr( AmbientColor ) );
        Log.Log( 'AmbientBlend=' + IntToStr( AmbientIntensity ) );
      end;
    except
      on E : Exception do
        Log.log( FailName + E.Message );
    end;
  end;

  procedure ReadSceneKnownBlock;
  var
    L : longint;
  const
    FailName : string = 'Loader.readSceneBlock';
  begin
{$IFDEF DODEBUG}
    if ( CurrDbgLvl >= DbgLvlSevere ) then
      Log.LogEntry( FailName );
{$ENDIF}
    try

      Log.Log( 'Reading map known block' );
      Stream.Read( L, sizeof( L ) ); //# of entries

      if ( SceneIndex > 0 ) and ( SceneIndex <= L ) then
      begin
        Stream.seek( ( SceneIndex - 1 ) * sizeof( longint ), soFromCurrent );
        Stream.Read( L, sizeof( L ) );
        MapKnown := ( L <> 0 );
      end;
    except
      on E : Exception do
        Log.log( FailName + E.Message );
    end;
  end;

  procedure ReadThemeBlock;
  var
    S : string;
    i, L : longint;
    List : TStringList;
    ThemeName : string;

  const
    FailName : string = 'Loader.readThemeBlock';
  begin
{$IFDEF DODEBUG}
    if ( CurrDbgLvl >= DbgLvlSevere ) then
      Log.LogEntry( FailName );
{$ENDIF}
    try

      Log.Log( 'Reading theme block' );

      Stream.Read( L, sizeof( L ) );
      if L = SceneIndex - 1 then
      begin
        Stream.Read( L, sizeof( L ) ); //Theme Name
        if L > 0 then
        begin
          SetLength( ThemeName, L );
          Stream.Read( ThemeName[ 1 ], L );
        end;

        Stream.Read( L, sizeof( L ) ); //List
        if L > 0 then
        begin
          List := TStringList.create;
          S := '';
          try
            SetLength( S, L );
            Stream.Read( S[ 1 ], L );
            List.Text := S;
            S := List.strings[ 0 ];
            for i := 1 to List.count - 1 do
            begin
              S := S + ',' + List.strings[ i ];
            end;
          finally
            List.free;
          end;
          Themes.add( ThemeName + '=' + S );
        end;
      end;
    except
      on E : Exception do
        Log.log( FailName + E.Message );
    end;
  end;

  procedure ReadZonesBlock;
  var
    i, Count, L, MaxZone : longint;
    S : string;
    z : word;
    ZoneStream : TFileStream;
    P : longint;
  const
    FailName : string = 'Loader.ReadZonesBlock';
  begin
{$IFDEF DODEBUG}
    if ( CurrDbgLvl >= DbgLvlSevere ) then
      Log.LogEntry( FailName );
{$ENDIF}
    try

      Log.Log( 'Reading zone block' );

      MaxZone := Zones.Count; //multiple zone block support - just in case
      if MaxZone < 2 then
        MaxZone := 2;
      Log.Log( '  MaxZone=' + IntToStr( MaxZone ) );

      Stream.Read( Count, sizeof( Count ) ); //how many
      Log.Log( '  Count=' + IntToStr( Count ) );

      Stream.Read( L, sizeof( L ) ); //size of string coming
      if L > 0 then
      begin
        SetLength( S, L );
        Stream.Read( S[ 1 ], L );
        Zones.Text := S;
//Log.Log('  Count='+IntToStr(Zones.Count));
//Log.Log(S);

        for i := 0 to Count - 1 do
        begin
          Stream.Read( L, sizeof( L ) );
          if L > MaxZone then
            MaxZone := L;
          Zones.objects[ i ] := TObject( L );
//        Log.Log(Zones.strings[i]+' '+IntToStr(L));
        end;
        Zones.Sorted := true;
      end;

      ZoneStream := nil;
      if UseCache then
      begin
        Log.Log( 'Loading cache' );
        try
          ZoneStream := TFileStream.Create( CacheFileA, fmOpenRead or fmShareDenyWrite );
          try
            Map.LoadFromStream( ZoneStream );
          finally
            ZoneStream.Free;
          end;
        except
        end;

        try
          ZoneStream := TFileStream.Create( CacheFileB, fmOpenRead or fmShareDenyWrite );
        except
        end;

        z := 0;
        TZone.Skip( ZoneStream );
        for i := 1 to MaxZone do
        begin
          z := Map.AddZone;
          if assigned( ZoneStream ) then
          begin
            try
              TZone( Map.Zones.items[ z ] ).LoadFromStream( ZoneStream );
            except
            end;
          end;
        end;

        if assigned( ZoneStream ) then
        begin
          P := ZoneStream.position;
          Count := 0;
          while ZoneStream.position < ZoneStream.Size do
          begin
            Map.AddLightZone;
            TZone.Skip( ZoneStream );
            inc( Count );
          end;

          ZoneStream.position := P;
          for i := 1 to Count do
          begin
            if assigned( ZoneStream ) then
            begin
              inc( z );
              try
                TZone( Map.Zones.items[ z ] ).LoadFromStream( ZoneStream );
              except
              end;
            end;
          end;
        end;
      end
      else
      begin
        S := DefaultPath + 'Maps/' + ChangeFileExt( ExtractFilename( LVLFile ), '.zit' );
        try
          if FileExists( S ) then
            ZoneStream := TFileStream.Create( S, fmOpenRead or fmShareDenyWrite )
        except
        end;

        for i := 1 to MaxZone do
        begin
          z := Map.AddZone;
          if assigned( ZoneStream ) then
          begin
            if ZoneStream.Size >= ( z + 1 ) * 24 then
            begin //we know this data is 24 bytes long
              ZoneStream.Position := z * 24;
              try
                TZone( Map.Zones.items[ z ] ).LoadFromStream( ZoneStream );
              except
              end;
            end;
          end;
        end;
      end;

      if assigned( ZoneStream ) then
        ZoneStream.free;
    except
      on E : Exception do
        Log.log( FailName + E.Message );
    end;
  end;

const
  FailName : string = 'Loader.Main';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  result := false;
  try
    Log.Log( 'Map File=' + Filename );
    if not FileExists( Filename ) then
    begin
      Log.Log( 'File not found' );
      result := false;
      exit;
    end;
    result := true;
    LoadCount := 0;
    SceneName := Scene;
    SceneCount := 0;
    SceneIndex := 0;
    Map.AmbientColor := $FFFFFF;
    Map.AmbientIntensity := 100;
    StartObjectBlocks := true;
    if ( SceneName = '' ) or ( lowercase( SceneName ) = 'default' ) then
      SceneName := 'Default Scene';
    Log.Log( 'Scene=' + SceneName );
    SceneName := lowercase( SceneName );
    CacheFileA := CachePath + ChangeFileExt( ExtractFilename( LVLFile ), '' ) + '_' + SceneName + '.pit';
    CacheFileB := CachePath + ChangeFileExt( ExtractFilename( LVLFile ), '' ) + '_' + SceneName + '.zit';
    UseCache := ReadCache and FileExists( CacheFileA ) and FileExists( CacheFileB );

    Zones := TStringList.Create;
    try
      Stream := nil;
      if UseCache then
      begin
        try
          Stream := TFileStream.Create( CacheFileB, fmOpenRead or fmShareDenyWrite )
        except
        end;

        if assigned( Stream ) then
        begin
          try
            TZone( Map.Zones.items[ 0 ] ).LoadFromStream( Stream );
          except
          end;
          Stream.free;
          Stream := nil;
        end;
      end
      else
      begin
        S := DefaultPath + 'Maps/' + ChangeFileExt( ExtractFilename( LVLFile ), '.zit' );
        try
          if FileExists( S ) then
            Stream := TFileStream.Create( S, fmOpenRead or fmShareDenyWrite )
        except
        end;

        if assigned( Stream ) then
        begin
          try
            TZone( Map.Zones.items[ 0 ] ).LoadFromStream( Stream );
          except
          end;
          Stream.free;
          Stream := nil;
        end;
      end;

      try
        Stream := TFileStream.create( Filename, fmOpenRead or fmShareCompat );
      except
        result := false;
        exit;
      end;
      try
        DlgProgress.MaxValue := round( Stream.Size * 1.25 );
        Stream.read( L, sizeof( L ) );
        if ( L <> Key ) then
          exit;
        Stream.read( Version, sizeof( Version ) );

        TileIndex := 1;

        while Stream.Position < Stream.Size do
        begin
          Stream.read( L, sizeof( L ) ); //Block Type
          Stream.read( BlockSize, sizeof( BlockSize ) ); //Block Size
          Position := Stream.Position;
          Dec( BlockSize, sizeof( longint ) + sizeof( longint ) ); //Substract size of block type and size fields
          case TMapBlockTypes( L ) of
            mbHeader : ReadHeaderBlock;
            mbMap : if not UseCache then
                ReadMapHeaderBlock;
            mbLayer0 : if not UseCache then
                ReadLayer0Block;
            mbLayer1 : if not UseCache then
                ReadLayer1Block;
            mbLayerDiamond : if not UseCache then
                ReadDiamondBlock;
            mbObject : ReadObjectBlock;
            mbRectResourceList, mbAltRectResourceList :
              begin
                if UseCache then
                begin
                  Log.Log( 'Skipping rect resources because of cache...' );
                end
                else
                begin
                  Log.Log( 'Loading rect resources...' );
                  RNames := TStringList.create;
                  ReadResourceList( RNames );
                  RNames.Insert( 0, '' );
                  RIndex := VarArrayCreate( [ 0, RNames.count ], varInteger );
                  RIndex[ 0 ] := 0;
                  RIndex[ 1 ] := TileIndex;
                  for i := 1 to RNames.count - 1 do
                  begin
                    S := RNames.Strings[ i ] + '.gif';
//                  Log.Log('  '+S);
                    try
                      Resource := LoadResource( TilePath + S );
                      if assigned( Resource ) and ( Resource is TTileResource ) then
                      begin
//                      j:=Zones.IndexOf(S);
//                      if j>=0 then Zone:=integer(Zones.objects[j])
//                      else Zone:=1;
                        Zone := 1;
                        TileIndex := TTileResource( Resource ).Define( Map, Zone, TileIndex );
                        Resource.free;
                        RIndex[ i + 1 ] := TileIndex;
                      end;
                    except
                      Log.Log( '  *** Error: Could not load ' + S );
                      RIndex[ i + 1 ] := 0;
                    end;
                  end;
                  RNames.free;
                  Log.Log( 'Rect resources complete' );
                end;
              end;
            mbDiamResourceList, mbAltDiamResourceList :
              begin
                if UseCache then
                begin
                  Log.Log( 'Skipping diamond resources because of cache...' );
                end
                else
                begin
                  Log.Log( 'Loading diamond resources...' );
                  DNames := TStringList.create;
                  ReadResourceList( DNames );
                  DIndex := VarArrayCreate( [ 0, DNames.count ], varInteger );
                  DVariations := VarArrayCreate( [ 0, DNames.Count - 1, ord( dqCenter ), ord( dqIS ) ], varVariant );
                  DIndex[ 0 ] := TileIndex;
                  for i := 0 to DNames.count - 1 do
                  begin
                    S := DNames.Strings[ i ] + '.gif';
//                  Log.Log('  '+S);
                    try
                      Resource := LoadResource( TilePath + S );
                      if assigned( Resource ) and ( Resource is TTileResource ) then
                      begin
//                      j:=Zones.IndexOf(S);
//                      if j>=0 then Zone:=integer(Zones.objects[j])
//                      else Zone:=2;
                        Zone := 2;
                        TileIndex := TTileResource( Resource ).Define( Map, Zone, TileIndex );
                        INI := TTileResource( Resource ).INI;
                        S := INI.ReadString( 'ImageList', 'Center', '' );
                        DVariations[ i, ord( dqCenter ) ] := LoadIndexes( S );
                        S := INI.ReadString( 'ImageList', 'EECornerIn', '' );
                        DVariations[ i, ord( dqIE ) ] := LoadIndexes( S );
                        S := INI.ReadString( 'ImageList', 'EECornerOut', '' );
                        DVariations[ i, ord( dqOE ) ] := LoadIndexes( S );
                        S := INI.ReadString( 'ImageList', 'NEEdge', '' );
                        DVariations[ i, ord( dqNE ) ] := LoadIndexes( S );
                        S := INI.ReadString( 'ImageList', 'NNCornerIn', '' );
                        DVariations[ i, ord( dqIN ) ] := LoadIndexes( S );
                        S := INI.ReadString( 'ImageList', 'NNCornerOut', '' );
                        DVariations[ i, ord( dqON ) ] := LoadIndexes( S );
                        S := INI.ReadString( 'ImageList', 'NWEdge', '' );
                        DVariations[ i, ord( dqNW ) ] := LoadIndexes( S );
                        S := INI.ReadString( 'ImageList', 'WWCornerIn', '' );
                        DVariations[ i, ord( dqIW ) ] := LoadIndexes( S );
                        S := INI.ReadString( 'ImageList', 'WWCornerOut', '' );
                        DVariations[ i, ord( dqOW ) ] := LoadIndexes( S );
                        S := INI.ReadString( 'ImageList', 'SWEdge', '' );
                        DVariations[ i, ord( dqSW ) ] := LoadIndexes( S );
                        S := INI.ReadString( 'ImageList', 'SSCornerIn', '' );
                        DVariations[ i, ord( dqIS ) ] := LoadIndexes( S );
                        S := INI.ReadString( 'ImageList', 'SSCornerOut', '' );
                        DVariations[ i, ord( dqOS ) ] := LoadIndexes( S );
                        S := INI.ReadString( 'ImageList', 'SEEdge', '' );
                        DVariations[ i, ord( dqSE ) ] := LoadIndexes( S );
                        Resource.Free;
                        DIndex[ i + 1 ] := TileIndex;
                      end;
                    except
                      Log.Log( '  *** Error: Could not load ' + S );
                      DIndex[ i + 1 ] := 0;
                    end;
                  end;
                  DNames.free;
                  Log.Log( 'Diamond resources complete' );
                end;
              end;
            mbObjResourceList, mbAltObjResourceList :
              begin
                Log.Log( 'Loading object resources...' );
                ONames := TStringList.create;
                ReadResourceList( ONames );

                ONames.Insert( 0, '' );
                OIndex := VarArrayCreate( [ 1, ONames.count - 1 ], varInteger );
                ZIndex := VarArrayCreate( [ 1, ONames.count - 1 ], varInteger );
                X := 0; //X is an index to map objects
                DumpMode := false;
                for i := 1 to ONames.count - 1 do
                begin

                  S := lowercase( ONames.Strings[ i ] );
                  if copy( S, 1, 7 ) = 'editor/' then
                  begin
//                  Log.Log('  Skipping '+S);
                    OIndex[ i ] := 0;
                  end
                  else
                  begin
//                  Log.Log('  '+S);
                    StaticObject := ( copy( S, 1, 13 ) = 'staticobject/' );
                    if UseCache and StaticObject then
                    begin //Prevent loading of static if cache loaded
//                    Log.Log('  Skipping '+S+' because of cache');
                      OIndex[ i ] := X + 1;
                    end
                    else
                    begin
                      if DumpMode then
                      begin
                        if not FileExists( ArtPath + S + '.pox' ) then
                          Log.Log( '*** Error: Resource does not exist ' + S );
                      end
                      else
                      begin
                        if StaticObject then
                        begin
                          Resource := LoadArtResource( S + '.gif' );
                          NewObject := true;
                          Y := 0;
                        end
                        else
                        begin
                          Y := Figures.IndexOf( ONames.Strings[ i ] );
                          if Y >= 0 then
                          begin
                            NewObject := false;
                            Resource := TResource( Figures.objects[ Y ] );
                            Resource.Reload := true;
                            Log.Log( '  Resource already loaded ' + S );
                          end
                          else
                          begin
                            NewObject := true;
                            Resource := LoadArtResource( S + '.gif' );
                          end;
                        end;
                        if assigned( Resource ) then
                        begin
                          if Resource is TDoorResource then
                          begin //Door Resource is descendant from
                            if NewObject then
                            begin
                              Y := Figures.add( ONames.Strings[ i ] ); //TStaticResource, so it must go first.
                              Figures.objects[ Y ] := Resource;
                            end;
                            j := Zones.IndexOf( S );
                            if j >= 0 then
                              Zone := integer( Zones.objects[ j ] )
                            else
                              Zone := 0;
                            ZIndex[ i ] := Zone;
                            if UseCache then
                            begin
                              TDoorResource( Resource ).ItemIndex := X;
                              TDoorResource( Resource ).ItemZone := Zone;
                            end
                            else
                            begin
                              X := TDoorResource( Resource ).Define( Map, Zone, X );
                            end;
                            OIndex[ i ] := -Y - 1;
//                          Log.Log('  Assigned to (door) #'+IntToStr(X)+IntToStr(OIndex[i]));
                          end
                          else if Resource is TStaticResource then
                          begin
                            OIndex[ i ] := X + 1;
//                          Log.Log('  Assigned to (static) #'+IntToStr(OIndex[i]));
                            j := Zones.IndexOf( S );
                            if j >= 0 then
                              Zone := integer( Zones.objects[ j ] )
                            else
                              Zone := 0;
//else Zone:=X mod 128;
                            ZIndex[ i ] := Zone;
                            X := TStaticResource( Resource ).Define( Map, Zone, X );
                            Resource.free;
                          end
                          else
                          begin
                            if NewObject then
                            begin
                              Y := Figures.add( ONames.Strings[ i ] );
                              Figures.objects[ Y ] := Resource;
                            end
                            else
                            begin
                              if Resource is TCharacterResource then
                              begin
                                if assigned( TCharacterResource( Resource ).NakedResource ) then
                                  TCharacterResource( Resource ).NakedResource.Reload := true;
                                if assigned( TCharacterResource( Resource ).HeadResource ) then
                                  TCharacterResource( Resource ).HeadResource.Reload := true;
                              end;
                            end;
                            OIndex[ i ] := -Y - 1;
//                          Log.Log('  Assigned to (sprite) #'+IntToStr(-OIndex[i]));
                          end;
                        end
                        else
                        begin
                          OIndex[ i ] := 0;
                          if s <> '' then
                          begin
                            Log.Log( '*** Error: Resource could not be loaded ' + S );
                            DumpMode := true;
                          end;
                        end;
                      end;
                    end;
                  end;
                end;
                ONames.free;
                if DumpMode then
                begin
                  Log.Log( '*** Load aborted' );
                  result := false;
                  exit;
                end;
                Log.Log( 'Object resources complete' );
              end;
            mbScene : ReadSceneBlock;
            mbZones : ReadZonesBlock;
            mbSceneKnown : ReadSceneKnownBlock;
            mbTheme : ReadThemeBlock;
            mbBB1, mbBB2 :
              begin
              end;
            mbChapt :
          else
            begin
              Log.Log( '*** Error: Unknown block: ' + IntToStr( L ) );
            end;
          end;
          if Position + BlockSize > Stream.Size then
          begin
            Log.Log( '*** Error: Read past EOF' );
            result := false;
            break;
          end;
          Stream.Seek( Position + BlockSize, soFromBeginning );
          Stream.Read( L, sizeof( L ) );
          if L <> EOB then
          begin
            Log.Log( '*** Error: EOB not found' );
            result := false;
            break;
          end;
          if ( LoadCount mod 8 ) = 0 then
            DlgProgress.SetBar( Stream.Position );
          inc( LoadCount );
     //   application.processmessages;
        end;
      finally
        Stream.free;
      end;
    finally
      Zones.free;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

function GetlevelCode( const Filename : string ) : int64;
var
  Stream : TFileStream;
  Version : longint;
  L, BlockSize : longint;
  Position : longint;
begin
  result := 0;
  try
    Stream := TFileStream.create( Filename, fmOpenRead or fmShareDenyWrite );
  except
    exit;
  end;

  try
    Stream.read( L, sizeof( L ) );
    if ( L <> Key ) then
      exit;
    Stream.read( Version, sizeof( Version ) );

    while Stream.Position < Stream.Size do
    begin
      Stream.read( L, sizeof( L ) ); //Block Type
      Stream.read( BlockSize, sizeof( BlockSize ) ); //Block Size
      Position := Stream.Position;
      Dec( BlockSize, sizeof( longint ) + sizeof( longint ) ); //Substract size of block type and size fields
      case TMapBlockTypes( L ) of
        mbChapt :
          begin
            Stream.read( result, sizeof( result ) );
            break;
          end;
      end;
      if Position + BlockSize > Stream.Size then
      begin
        break;
      end;
      Stream.Seek( Position + BlockSize, soFromBeginning );
      Stream.Read( L, sizeof( L ) );
      if L <> EOB then
      begin
        break;
      end;
    end;
  finally
    Stream.free;
  end;

end;

end.

