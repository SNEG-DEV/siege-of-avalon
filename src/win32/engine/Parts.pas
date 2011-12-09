unit Parts;
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
  Windows,
  SysUtils,
  Graphics,
  Anigrp30,
  AniDec30,
  IniFiles,
  DFX,
  digifx,
  Resource,
  Character,
  ItemDatabase,
  Engine,
  LogFile;

type
  TPartManager = class( TObject )
  private
    InvDB : TStringDatabase;
    XRefDB : TStringDatabase;
  public
    NotFound : boolean;
    constructor Create( const ItemDBPath, XRefDBPath : string );
    destructor Destroy; override;
    function GetLayerResource( const PartFile : string ) : TLayerResource;
    function GetResource( const PartFile : string ) : TResource;
    function GetOnDemandResource( const PartFile : string ) : TResource;
    function GetImageFile( const PartName, NakedName : string ) : string;
    function LoadItem( const ItemName, NakedName : string ) : TItem;
    procedure ReleaseItemDB;
    function MemSize : longint;
    procedure ClearUnusedParts;
  end;

const
  PiercingMin = 1;
  CrushingMin = 2;
  CuttingMin = 3;
  HeatMin = 4;
  ColdMin = 5;
  ElectricMin = 6;
  PoisonMin = 7;
  MagicMin = 8;
  MentalMin = 9;
  StunMin = 10;
  SpecialMin = 11;

  PiercingMax = 12;
  CrushingMax = 13;
  CuttingMax = 14;
  HeatMax = 15;
  ColdMax = 16;
  ElectricMax = 17;
  PoisonMax = 18;
  MagicMax = 19;
  MentalMax = 20;
  StunMax = 21;
  SpecialMax = 22;

  PiercingInv = 23;
  CrushingInv = 24;
  CuttingInv = 25;
  HeatInv = 26;
  ColdInv = 27;
  ElectricInv = 28;
  PoisonInv = 29;
  MagicInv = 30;
  MentalInv = 31;
  StunInv = 32;

  PiercingRes = 33;
  CrushingRes = 34;
  CuttingRes = 35;
  HeatRes = 36;
  ColdRes = 37;
  ElectricRes = 38;
  PoisonRes = 39;
  MagicRes = 40;
  MentalRes = 41;
  StunRes = 42;

  StrengthSM = 43;
  CoordinationSM = 44;
  ConstitutionSM = 45;
  MysticismSM = 46;
  CombatSM = 47;
  StealthSM = 48;
  RestrictionSM = 49;
  AttackRecoverySM = 50;
  HitRecoverySM = 51;
  PerceptionSM = 52;
  CharmSM = 53;
  HealingRateSM = 54;
  RechargeRateSM = 55;
  HitPointsSM = 56;
  ManaSM = 57;
  AttackSM = 58;
  DefenseSM = 59;

  cSlotsAllowed = 60;
  cItemInfo = 61;
  cValue = 62;
  cWeight = 63;
  cTitle = 63;
  cMagic = 64;
  cItemType = 65;
//  cItemInfo= 66;
  cSecretName = 67;
  cSecretInfo = 68;
  cInventoryImage = 69;
  cPartName = 70;
  cDisplayName = 71;
  cInventoryHeight = 72;
  cInventoryWidth = 73;

  w2Handed = 74;
  wRange = 75;
  wMinStrength = 76;
  wMinCoordination = 77;
  wMaxRestriction = 78;
  wSndAttack = 79;
  wSndOther = 80;
  wSndMetal = 81;

  qFletchingColor = 74;
  qTracking = 75;
  qSndOther = 76;
  qSndMetal = 77;
  qSndStone = 78;

  iMaterial = 74;

var
  PartManager : TPartManager;

implementation

{ TPartManager }

constructor TPartManager.Create( const ItemDBPath, XRefDBPath : string );
const
  FailName : string = 'Parts.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    InvDB := TStringDatabase.Create( ItemDBPath );
    XRefDB := TStringDatabase.Create( XRefDBPath );
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TPartManager.Destroy;
const
  FailName : string = 'Parts.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    InvDB.Free;
    XRefDB.Free;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TPartManager.GetResource( const PartFile : string ) : TResource;
var
  i : integer;
  S, S1 : string;
const
  FailName : string = 'Parts.GetResource';
begin
  Result := nil;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    S := PartFile;
    S1 := ChangeFileExt( S, '' );
    i := Figures.IndexOf( S1 );
    if i >= 0 then
    begin
      result := TResource( Figures.Objects[ i ] );
      result.Reload := true;
    end
    else
    begin
      result := TResource( LoadArtResource( S ) );
      if assigned( result ) then
      begin
        i := Figures.add( S1 );
        Figures.Objects[ i ] := result;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TPartManager.GetOnDemandResource( const PartFile : string ) : TResource;
var
  i : integer;
  S, S1 : string;
const
  FailName : string = 'Parts.GetResource';
begin
  Result := nil;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    S := PartFile;
    S1 := ChangeFileExt( S, '' );
    i := Figures.IndexOf( S1 );
    if i >= 0 then
    begin
      result := TResource( Figures.Objects[ i ] );
      result.Reload := true;
    end
    else
    begin
      result := TResource( LoadArtResource( S, true ) );
      if assigned( result ) then
      begin
        i := Figures.add( S1 );
        Figures.Objects[ i ] := result;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TPartManager.GetLayerResource( const PartFile : string ) : TLayerResource;
var
  i : integer;
  S, S1 : string;
const
  FailName : string = 'Parts.GetLayerResource';
begin
  Result := nil;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    S := LayerPath + PartFile;
    S1 := ChangeFileExt( S, '' );
    i := Figures.IndexOf( S1 );
    if i >= 0 then
    begin
      result := TLayerResource( Figures.Objects[ i ] );
      result.Reload := true;
    end
    else
    begin
      result := TLayerResource( LoadArtResource( S ) );
      if assigned( result ) then
      begin
        i := Figures.add( S1 );
        Figures.Objects[ i ] := result;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TPartManager.LoadItem( const ItemName, NakedName : string ) : TItem;
var
  SlotString : string;
  ItemType : string;
  S : string;

  function ParseDB( FieldPos : integer ) : string;
  const
    FailName : string = 'Parts.LoadItem.ParseDB';
  begin
{$IFDEF DODEBUG}
    if ( CurrDbgLvl >= DbgLvlSevere ) then
      Log.LogEntry( FailName );
{$ENDIF}
    try
      Result := InvDB.Fields[ FieldPos ];

      if Result = '' then
        Result := '0'
    except
      on E : Exception do
        Log.log( FailName, E.Message, [ ] );
    end;
  end;

  function sParseDB( FieldPos : integer ) : string;
  const
    FailName : string = 'Parts.LoadItem.sParseDB';
  begin
{$IFDEF DODEBUG}
    if ( CurrDbgLvl >= DbgLvlSevere ) then
      Log.LogEntry( FailName );
{$ENDIF}
    try
      Result := InvDB.Fields[ FieldPos ];
    except
      on E : Exception do
        Log.log( FailName, E.Message, [ ] );
    end;
  end;


const
  FailName : string = 'Parts.LoadItem';
begin
  Result := nil;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if not InvDB.FindRecord( ItemName ) then
      exit;

    ItemType := lowercase( Trim( sParseDB( cItemType ) ) );
    if ItemType = 'weapon' then
      result := TWeapon.Create( 0, 0, 0, 1, false )
    else if ItemType = 'bow' then
      result := TBow.Create( 0, 0, 0, 1, false )
    else if ItemType = 'quiver' then
      result := TQuiver.Create( 0, 0, 0, 1, false )
    else
      result := TItem.Create( 0, 0, 0, 1, false );

    result.ItemName := ItemName;
    with result do
    begin
      Damage.Piercing.Min := StrToFloat( ParseDB( PiercingMin ) );
      Damage.Crushing.Min := StrToFloat( ParseDB( CrushingMin ) );
      Damage.Cutting.Min := StrToFloat( ParseDB( CuttingMin ) );
      Damage.Heat.Min := StrToFloat( ParseDB( HeatMin ) );
      Damage.Cold.Min := StrToFloat( ParseDB( ColdMin ) );
      Damage.Electric.Min := StrToFloat( ParseDB( ElectricMin ) );
      Damage.Poison.Min := StrToFloat( ParseDB( PoisonMin ) );
      Damage.Magic.Min := StrToFloat( ParseDB( MagicMin ) );
      Damage.Mental.Min := StrToFloat( ParseDB( MentalMin ) );
      Damage.Stun.Min := StrToFloat( ParseDB( StunMin ) );
      Damage.Special.Min := StrToFloat( ParseDB( SpecialMin ) );

      Damage.Piercing.Max := StrToFloat( ParseDB( PiercingMax ) );
      Damage.Crushing.Max := StrToFloat( ParseDB( CrushingMax ) );
      Damage.Cutting.Max := StrToFloat( ParseDB( CuttingMax ) );
      Damage.Heat.Max := StrToFloat( ParseDB( HeatMax ) );
      Damage.Cold.Max := StrToFloat( ParseDB( ColdMax ) );
      Damage.Electric.Max := StrToFloat( ParseDB( ElectricMax ) );
      Damage.Poison.Max := StrToFloat( ParseDB( PoisonMax ) );
      Damage.Magic.Max := StrToFloat( ParseDB( MagicMax ) );
      Damage.Mental.Max := StrToFloat( ParseDB( MentalMax ) );
      Damage.Stun.Max := StrToFloat( ParseDB( StunMax ) );
      Damage.Special.Max := StrToFloat( ParseDB( SpecialMax ) );

      Resistance.Piercing.Invulnerability := StrToFloat( ParseDB( PiercingInv ) );
      Resistance.Crushing.Invulnerability := StrToFloat( ParseDB( CrushingInv ) );
      Resistance.Cutting.Invulnerability := StrToFloat( ParseDB( CuttingInv ) );
      Resistance.Heat.Invulnerability := StrToFloat( ParseDB( HeatInv ) );
      Resistance.Cold.Invulnerability := StrToFloat( ParseDB( ColdInv ) );
      Resistance.Electric.Invulnerability := StrToFloat( ParseDB( ElectricInv ) );
      Resistance.Poison.Invulnerability := StrToFloat( ParseDB( PoisonInv ) );
      Resistance.Magic.Invulnerability := StrToFloat( ParseDB( MagicInv ) );
      Resistance.Mental.Invulnerability := StrToFloat( ParseDB( MentalInv ) );
      Resistance.Stun.Invulnerability := StrToFloat( ParseDB( StunInv ) );

      Resistance.Piercing.Resistance := StrToFloat( ParseDB( PiercingRes ) ) / 100;
      Resistance.Crushing.Resistance := StrToFloat( ParseDB( CrushingRes ) ) / 100;
      Resistance.Cutting.Resistance := StrToFloat( ParseDB( CuttingRes ) ) / 100;
      Resistance.Heat.Resistance := StrToFloat( ParseDB( HeatRes ) ) / 100;
      Resistance.Cold.Resistance := StrToFloat( ParseDB( ColdRes ) ) / 100;
      Resistance.Electric.Resistance := StrToFloat( ParseDB( ElectricRes ) ) / 100;
      Resistance.Poison.Resistance := StrToFloat( ParseDB( PoisonRes ) ) / 100;
      Resistance.Magic.Resistance := StrToFloat( ParseDB( MagicRes ) ) / 100;
      Resistance.Mental.Resistance := StrToFloat( ParseDB( MentalRes ) ) / 100;
      Resistance.Stun.Resistance := StrToFloat( ParseDB( StunRes ) ) / 100;

      Modifier.Strength := StrToInt( ParseDB( StrengthSM ) );
      Modifier.Coordination := StrToInt( ParseDB( CoordinationSM ) );
      Modifier.Constitution := StrToInt( ParseDB( ConstitutionSM ) );
      Modifier.Mysticism := StrToInt( ParseDB( MysticismSM ) );
      Modifier.Combat := StrToInt( ParseDB( CombatSM ) );
      Modifier.Stealth := StrToInt( ParseDB( StealthSM ) );
      Modifier.Restriction := StrToInt( ParseDB( RestrictionSM ) );
      Modifier.AttackRecovery := StrToInt( ParseDB( AttackRecoverySM ) );
      Modifier.HitRecovery := StrToInt( ParseDB( HitRecoverySM ) );
      Modifier.Perception := StrToInt( ParseDB( PerceptionSM ) );
      Modifier.Charm := StrToInt( ParseDB( CharmSM ) );
      Modifier.HealingRate := StrToInt( ParseDB( HealingRateSM ) );
      Modifier.RechargeRate := StrToInt( ParseDB( RechargeRateSM ) );
      Modifier.HitPoints := StrToInt( ParseDB( HitPointsSM ) );
      Modifier.Mana := StrToInt( ParseDB( ManaSM ) );
      Modifier.Attack := StrToInt( ParseDB( AttackSM ) );
      Modifier.Defense := StrToInt( ParseDB( DefenseSM ) );

      Value := StrToInt( ParseDB( cvalue ) );
//    Weight:= StrToInt(ParseDB(cweight));
      Title := ParseDB( cTitle );
      Magic := StrToInt( ParseDB( cMagic ) );

      Identified := False;

      ItemInfo := sParseDB( cItemInfo );
      SecretName := sParseDB( cSecretName );
      SecretInfo := sParseDB( cSecretInfo );
      InventoryImage := sParseDB( cInventoryImage );
      if InventoryImage <> '' then
        InventoryImage := InventoryPath + InventoryImage;
      PartName := sParseDB( cPartName );
      DisplayName := sParseDB( cDisplayName );

      SlotsAllowed := [ ]; //clear it first;

      SlotString := LowerCase( sParseDB( cSlotsAllowed ) );

      if Pos( '[leg1]', SlotString ) <> 0 then
        SlotsAllowed := SlotsAllowed + [ slLeg1 ];
      if Pos( '[boot]', SlotString ) <> 0 then
        SlotsAllowed := SlotsAllowed + [ slBoot ];
      if Pos( '[leg2]', SlotString ) <> 0 then
        SlotsAllowed := SlotsAllowed + [ slLeg2 ];
      if Pos( '[chest1]', SlotString ) <> 0 then
        SlotsAllowed := SlotsAllowed + [ slChest1 ];
      if Pos( '[chest2]', SlotString ) <> 0 then
        SlotsAllowed := SlotsAllowed + [ slChest2 ];
      if Pos( '[arm]', SlotString ) <> 0 then
        SlotsAllowed := SlotsAllowed + [ slArm ];
      if Pos( '[belt]', SlotString ) <> 0 then
        SlotsAllowed := SlotsAllowed + [ slBelt ];
      if Pos( '[chest3]', SlotString ) <> 0 then
        SlotsAllowed := SlotsAllowed + [ slChest3 ];
      if Pos( '[gauntlet]', SlotString ) <> 0 then
        SlotsAllowed := SlotsAllowed + [ slGauntlet ];
      if Pos( '[outer]', SlotString ) <> 0 then
        SlotsAllowed := SlotsAllowed + [ slOuter ];
      if Pos( '[helmet]', SlotString ) <> 0 then
        SlotsAllowed := SlotsAllowed + [ slHelmet ];
      if Pos( '[weapon]', SlotString ) <> 0 then
        SlotsAllowed := SlotsAllowed + [ slWeapon ];
      if Pos( '[shield]', SlotString ) <> 0 then
        SlotsAllowed := SlotsAllowed + [ slShield ];
      if Pos( '[misc1]', SlotString ) <> 0 then
        SlotsAllowed := SlotsAllowed + [ slMisc1 ];
      if Pos( '[misc2]', SlotString ) <> 0 then
        SlotsAllowed := SlotsAllowed + [ slMisc2 ];
      if Pos( '[misc3]', SlotString ) <> 0 then
        SlotsAllowed := SlotsAllowed + [ slMisc3 ];

      InvW := StrToInt( ParseDB( cInventoryWidth ) ) div 18;
      InvH := StrToInt( ParseDB( cInventoryHeight ) ) div 26;

      if PartName = '' then
        PartName := 'prt_smallbag';
      LayeredImage := GetImageFile( PartName, NakedName );
    end;

    if ItemType = 'weapon' then
    begin
      TWeapon( result ).TwoHanded := ( lowercase( ParseDB( w2Handed ) ) = 'true' );
      TWeapon( result ).Range := StrToInt( ParseDB( wRange ) );
      TWeapon( result ).MinStrength := StrToInt( ParseDB( wMinStrength ) );
      TWeapon( result ).MinCoordination := StrToInt( ParseDB( wMinCoordination ) );
      TWeapon( result ).MaxRestriction := StrToInt( ParseDB( wMaxRestriction ) );
      TWeapon( result ).AttackSound := ParseDB( wSndAttack );
      TWeapon( result ).StrikeLeatherSound := ParseDB( wSndOther );
      TWeapon( result ).StrikeMetalSound := ParseDB( wSndMetal );
    end
    else if ItemType = 'bow' then
    begin
      TBow( result ).TwoHanded := ( lowercase( ParseDB( w2Handed ) ) = 'true' );
      TBow( result ).Range := StrToInt( ParseDB( wRange ) );
      TBow( result ).MinStrength := StrToInt( ParseDB( wMinStrength ) );
      TBow( result ).MinCoordination := StrToInt( ParseDB( wMinCoordination ) );
      TBow( result ).MaxRestriction := StrToInt( ParseDB( wMaxRestriction ) );
      TBow( result ).AttackSound := ParseDB( wSndAttack );
      TBow( result ).StrikeLeatherSound := ParseDB( wSndOther );
      TBow( result ).StrikeMetalSound := ParseDB( wSndMetal );
    end
    else if ItemType = 'quiver' then
    begin
      TQuiver( result ).FletchingColor := StrToInt( ParseDB( qFletchingColor ) );
      TQuiver( result ).TrackingDegree := StrToInt( ParseDB( qTracking ) );
      TQuiver( result ).StrikeLeatherSound := ParseDB( qSndOther );
      TQuiver( result ).StrikeMetalSound := ParseDB( qSndMetal );
      TQuiver( result ).StrikeStoneSound := ParseDB( qSndStone );
    end
    else
    begin
      S := lowercase( ParseDB( iMaterial ) );
      if S = 'metal' then
        result.Material := maMetal
      else
        result.Material := maOther;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TPartManager.GetImageFile( const PartName,
  NakedName : string ) : string;
var
  S, S1 : string;
  XRefIndex : integer;
const
  FailName : string = 'Parts.GetImageFile';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    result := '';
    NotFound := false;

    if XRefDB.FindRecord( 'Base' ) then
    begin
      XRefIndex := 1;
      S1 := lowercase( NakedName );
      S := XRefDB.Fields[ XRefIndex ];
      while S <> '' do
      begin
        if Pos( S1, lowercase( S ) ) > 0 then
          break;
        inc( XRefIndex );
        S := XRefDB.Fields[ XRefIndex ];
      end;
      if S <> '' then
      begin
        if XRefDB.FindRecord( PartName ) then
        begin
          Result := XRefDB.Fields[ XRefIndex ];
        end
        else
        begin
          NotFound := true;
        end;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TPartManager.ReleaseItemDB;
const
  FailName : string = 'Parts.ReleaseItemDB';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    InvDB.free;
    InvDB := nil;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TPartManager.MemSize : longint;
var
  i : integer;
const
  FailName : string = 'Parts.MemSize';
begin
  result := 0;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    for i := 0 to Figures.Count - 1 do
    begin
      if assigned( Figures.Objects[ i ] ) then
      begin
        inc( Result, TResource( Figures.Objects[ i ] ).MemSize );
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TPartManager.ClearUnusedParts;
var
  i : integer;
begin
  for i := Figures.count - 1 downto 0 do
  begin
    if assigned( Figures.objects[ i ] ) then
    begin
      if TResource( Figures.objects[ i ] ).Reload then
      begin
        TResource( Figures.objects[ i ] ).Reload := false;
      end
      else
      begin
//        Log.Log(Figures.strings[i]+' unloaded');
        TResource( Figures.objects[ i ] ).free;
        Figures.Delete( i );
      end;
    end
    else
    begin
//      Log.Log(Figures.strings[i]+' unloaded');
      Figures.Delete( i );
    end;
  end;
end;

end.
