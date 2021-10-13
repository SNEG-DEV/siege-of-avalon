unit Parts;
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
  System.Classes,
  System.SysUtils,
  Resource,
  Character,
  SoAOS.Data.DB,
  Engine,
  LogFile;

type
  TPartManager = class( TObject )
  private
    InvDB : TSoAOSItemsTable;
    XRefDB : TSoAOSXRefTable;
  public
    NotFound : boolean;
    constructor Create( const ItemDBPath, XRefDBPath : string );
    destructor Destroy; override;
    function GetLayerResource( const PartFile : string ) : TLayerResource;
    function GetResource( const PartFile : string ) : TResource;
    function GetOnDemandResource( const PartFile : string ) : TResource;
    function GetImageFile( const PartName, NakedName : string ) : string;
    function LoadItem( const ItemName, NakedName : string ) : TItem;
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
  Log.DebugLog( FailName );
  try
    InvDB := TSoAOSItemsTable.Create;
    InvDB.LoadFromFile( ItemDBPath );
    XRefDB := TSoAOSXRefTable.Create;
    XRefDB.LoadFromFile( XRefDBPath );
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TPartManager.Destroy;
const
  FailName : string = 'Parts.Destroy';
begin
  Log.DebugLog( FailName );
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

  Log.DebugLog( FailName );
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

  Log.DebugLog( FailName );
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

  Log.DebugLog( FailName );
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
const
  FailName : string = 'Parts.LoadItem';
begin
  Result := nil;
  Log.DebugLog( FailName );
  try
    if not InvDB.Locate( ItemName ) then
      exit;

    ItemType := InvDB.Fields[ cItemType ].AsString.Trim.ToLower;
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
      Damage.Piercing.Min := InvDB.Fields[ PiercingMin ].AsFloat;
      Damage.Crushing.Min := InvDB.Fields[ CrushingMin ].AsFloat;
      Damage.Cutting.Min := InvDB.Fields[ CuttingMin ].AsFloat;
      Damage.Heat.Min := InvDB.Fields[ HeatMin ].AsFloat;
      Damage.Cold.Min := InvDB.Fields[ ColdMin ].AsFloat;
      Damage.Electric.Min := InvDB.Fields[ ElectricMin ].AsFloat;
      Damage.Poison.Min := InvDB.Fields[ PoisonMin ].AsFloat;
      Damage.Magic.Min := InvDB.Fields[ MagicMin ].AsFloat;
      Damage.Mental.Min := InvDB.Fields[ MentalMin ].AsFloat;
      Damage.Stun.Min := InvDB.Fields[ StunMin ].AsFloat;
      Damage.Special.Min := InvDB.Fields[ SpecialMin ].AsFloat;

      Damage.Piercing.Max := InvDB.Fields[ PiercingMax ].AsFloat;
      Damage.Crushing.Max := InvDB.Fields[ CrushingMax ].AsFloat;
      Damage.Cutting.Max := InvDB.Fields[ CuttingMax ].AsFloat;
      Damage.Heat.Max := InvDB.Fields[ HeatMax ].AsFloat;
      Damage.Cold.Max := InvDB.Fields[ ColdMax ].AsFloat;
      Damage.Electric.Max := InvDB.Fields[ ElectricMax ].AsFloat;
      Damage.Poison.Max := InvDB.Fields[ PoisonMax ].AsFloat;
      Damage.Magic.Max := InvDB.Fields[ MagicMax ].AsFloat;
      Damage.Mental.Max := InvDB.Fields[ MentalMax ].AsFloat;
      Damage.Stun.Max := InvDB.Fields[ StunMax ].AsFloat;
      Damage.Special.Max := InvDB.Fields[ SpecialMax ].AsFloat;

      Resistance.Piercing.Invulnerability := InvDB.Fields[ PiercingInv ].AsFloat;
      Resistance.Crushing.Invulnerability := InvDB.Fields[ CrushingInv ].AsFloat;
      Resistance.Cutting.Invulnerability := InvDB.Fields[ CuttingInv ].AsFloat;
      Resistance.Heat.Invulnerability := InvDB.Fields[ HeatInv ].AsFloat;
      Resistance.Cold.Invulnerability := InvDB.Fields[ ColdInv ].AsFloat;
      Resistance.Electric.Invulnerability := InvDB.Fields[ ElectricInv ].AsFloat;
      Resistance.Poison.Invulnerability := InvDB.Fields[ PoisonInv ].AsFloat;
      Resistance.Magic.Invulnerability := InvDB.Fields[ MagicInv ].AsFloat;
      Resistance.Mental.Invulnerability := InvDB.Fields[ MentalInv ].AsFloat;
      Resistance.Stun.Invulnerability := InvDB.Fields[ StunInv ].AsFloat;

      Resistance.Piercing.Resistance := InvDB.Fields[ PiercingRes ].AsFloat / 100;
      Resistance.Crushing.Resistance := InvDB.Fields[ CrushingRes ].AsFloat / 100;
      Resistance.Cutting.Resistance := InvDB.Fields[ CuttingRes ].AsFloat / 100;
      Resistance.Heat.Resistance := InvDB.Fields[ HeatRes ].AsFloat / 100;
      Resistance.Cold.Resistance := InvDB.Fields[ ColdRes ].AsFloat / 100;
      Resistance.Electric.Resistance := InvDB.Fields[ ElectricRes ].AsFloat / 100;
      Resistance.Poison.Resistance := InvDB.Fields[ PoisonRes ].AsFloat / 100;
      Resistance.Magic.Resistance := InvDB.Fields[ MagicRes ].AsFloat / 100;
      Resistance.Mental.Resistance := InvDB.Fields[ MentalRes ].AsFloat / 100;
      Resistance.Stun.Resistance := InvDB.Fields[ StunRes ].AsFloat / 100;

      Modifier.Strength := InvDB.Fields[ StrengthSM ].AsInteger;
      Modifier.Coordination := InvDB.Fields[ CoordinationSM ].AsInteger;
      Modifier.Constitution := InvDB.Fields[ ConstitutionSM ].AsInteger;
      Modifier.Mysticism := InvDB.Fields[ MysticismSM ].AsInteger;
      Modifier.Combat := InvDB.Fields[ CombatSM ].AsInteger;
      Modifier.Stealth := InvDB.Fields[ StealthSM ].AsInteger;
      Modifier.Restriction := InvDB.Fields[ RestrictionSM ].AsInteger;
      Modifier.AttackRecovery := InvDB.Fields[ AttackRecoverySM ].AsInteger;
      Modifier.HitRecovery := InvDB.Fields[ HitRecoverySM ].AsInteger;
      Modifier.Perception := InvDB.Fields[ PerceptionSM ].AsInteger;
      Modifier.Charm := InvDB.Fields[ CharmSM ].AsInteger;
      Modifier.HealingRate := InvDB.Fields[ HealingRateSM ].AsInteger;
      Modifier.RechargeRate := InvDB.Fields[ RechargeRateSM ].AsInteger;
      Modifier.HitPoints := InvDB.Fields[ HitPointsSM ].AsInteger;
      Modifier.Mana := InvDB.Fields[ ManaSM ].AsInteger;
      Modifier.Attack := InvDB.Fields[ AttackSM ].AsInteger;
      Modifier.Defense := InvDB.Fields[ DefenseSM ].AsInteger;

      Value := InvDB.Fields[ cvalue ].AsInteger;
//    Weight:= StrToInt(ParseDB(cweight));
      Title := InvDB.Fields[ cTitle ].AsString;
      Magic := InvDB.Fields[ cMagic ].AsInteger;

      Identified := False;

      ItemInfo := InvDB.Fields[ cItemInfo ].AsString;
      SecretName := InvDB.Fields[ cSecretName ].AsString;
      SecretInfo := InvDB.Fields[ cSecretInfo ].AsString;
      InventoryImage := InvDB.Fields[ cInventoryImage ].AsString;
      if InventoryImage <> '' then
        InventoryImage := InventoryPath + InventoryImage;
      PartName := InvDB.Fields[ cPartName ].AsString;
      DisplayName := InvDB.Fields[ cDisplayName ].AsString;

      SlotsAllowed := [ ]; //clear it first;

      SlotString := InvDB.Fields[ cSlotsAllowed ].AsString.ToLower;

      if SlotString.Contains( '[leg1]' ) then SlotsAllowed := SlotsAllowed + [ slLeg1 ];
      if SlotString.Contains( '[boot]' ) then SlotsAllowed := SlotsAllowed + [ slBoot ];
      if SlotString.Contains( '[leg2]' ) then SlotsAllowed := SlotsAllowed + [ slLeg2 ];
      if SlotString.Contains( '[Tatt]' ) then SlotsAllowed := SlotsAllowed + [ slTatt ];
      if SlotString.Contains( '[chest1]' ) then SlotsAllowed := SlotsAllowed + [ slChest1 ];
      if SlotString.Contains( '[chest2]' ) then SlotsAllowed := SlotsAllowed + [ slChest2 ];
      if SlotString.Contains( '[arm]' ) then SlotsAllowed := SlotsAllowed + [ slArm ];
      if SlotString.Contains( '[belt]' ) then SlotsAllowed := SlotsAllowed + [ slBelt ];
      if SlotString.Contains( '[chest3]' ) then SlotsAllowed := SlotsAllowed + [ slChest3 ];
      if SlotString.Contains( '[gauntlet]' ) then SlotsAllowed := SlotsAllowed + [ slGauntlet ];
      if SlotString.Contains( '[outer]' ) then SlotsAllowed := SlotsAllowed + [ slOuter ];
      if SlotString.Contains( '[helmet]' ) then SlotsAllowed := SlotsAllowed + [ slHelmet ];
      if SlotString.Contains( '[weapon]' ) then SlotsAllowed := SlotsAllowed + [ slWeapon ];
      if SlotString.Contains( '[shield]' ) then SlotsAllowed := SlotsAllowed + [ slShield ];
      if SlotString.Contains( '[tabar]' ) then SlotsAllowed := SlotsAllowed + [ sltabar ];
      if SlotString.Contains( '[coif]' ) then SlotsAllowed := SlotsAllowed + [ slCoif ];
      if SlotString.Contains( '[healthpois]' ) then SlotsAllowed := SlotsAllowed + [ slhealthpois ];
      if SlotString.Contains( '[manapois]' ) then SlotsAllowed := SlotsAllowed + [ slmanapois ];
      if SlotString.Contains( '[misc1]' ) then SlotsAllowed := SlotsAllowed + [ slMisc1 ];
      if SlotString.Contains( '[misc2]' ) then SlotsAllowed := SlotsAllowed + [ slMisc2 ];
      if SlotString.Contains( '[misc3]' ) then SlotsAllowed := SlotsAllowed + [ slMisc3 ];

      InvW := InvDB.Fields[ cInventoryWidth ].AsInteger div 18;
      InvH := InvDB.Fields[ cInventoryHeight ].AsInteger div 26;

      if PartName = '' then
        PartName := 'prt_smallbag';
      LayeredImage := GetImageFile( PartName, NakedName );
    end;

    if ItemType = 'weapon' then
    begin
      TWeapon( result ).TwoHanded := InvDB.Fields[ w2Handed ].AsBoolean;
      TWeapon( result ).Range := InvDB.Fields[ wRange ].AsInteger;
      TWeapon( result ).MinStrength := InvDB.Fields[ wMinStrength ].AsInteger;
      TWeapon( result ).MinCoordination := InvDB.Fields[ wMinCoordination ].AsInteger;
      TWeapon( result ).MaxRestriction := InvDB.Fields[ wMaxRestriction ].AsInteger;
      TWeapon( result ).AttackSound := InvDB.Fields[ wSndAttack ].AsString;
      TWeapon( result ).StrikeLeatherSound := InvDB.Fields[ wSndOther ].AsString;
      TWeapon( result ).StrikeMetalSound := InvDB.Fields[ wSndMetal ].AsString;
    end
    else if ItemType = 'bow' then
    begin
      TBow( result ).TwoHanded := InvDB.Fields[ w2Handed ].AsBoolean;
      TBow( result ).Range := InvDB.Fields[ wRange ].AsInteger;
      TBow( result ).MinStrength := InvDB.Fields[ wMinStrength ].AsInteger;
      TBow( result ).MinCoordination := InvDB.Fields[ wMinCoordination ].AsInteger;
      TBow( result ).MaxRestriction := InvDB.Fields[ wMaxRestriction ].AsInteger;
      TBow( result ).AttackSound := InvDB.Fields[ wSndAttack ].AsString;
      TBow( result ).StrikeLeatherSound := InvDB.Fields[ wSndOther ].AsString;
      TBow( result ).StrikeMetalSound := InvDB.Fields[ wSndMetal ].AsString;
    end
    else if ItemType = 'quiver' then
    begin
      TQuiver( result ).FletchingColor := InvDB.Fields[ qFletchingColor ].AsInteger;
      TQuiver( result ).TrackingDegree := InvDB.Fields[ qTracking ].AsInteger;
      TQuiver( result ).StrikeLeatherSound := InvDB.Fields[ qSndOther ].AsString;
      TQuiver( result ).StrikeMetalSound := InvDB.Fields[ qSndMetal ].AsString;
      TQuiver( result ).StrikeStoneSound := InvDB.Fields[ qSndStone ].AsString;
    end
    else
    begin
      S := InvDB.Fields[ iMaterial ].AsString.ToLower;
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

function TPartManager.GetImageFile( const PartName,  NakedName : string ) : string;
const
  FailName : string = 'Parts.GetImageFile';
begin
  Log.DebugLog( FailName );
  try
    Result := '';
    if XRefDB.Locate( PartName ) then
      Result := XRefDB.FieldByName( NakedName ).AsString;
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

  Log.DebugLog( FailName );
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
