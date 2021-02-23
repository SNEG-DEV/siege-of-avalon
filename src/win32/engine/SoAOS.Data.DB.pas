unit SoAOS.Data.DB;
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

  Description: Dataset-like handling of xref.db (both versions), Title.db and Items.db

  Requires: Delphi 10.3.3 or later

  Revision History:
  - 14 Jan 2020 - SN: Initial Commit to Git
  see git repo afterwards

*)

interface

uses
  System.Generics.Collections;

type
  TSoAOSDatasetType = (dstTitel, dstXref, dstItems);

  TSoAOSField = class
  strict private
    FStrDataRead: string;
    FFieldNames: string;
    FFieldNo: Integer;
    function GetAsBoolean: Boolean;
    function GetAsInteger: Integer;
    function GetAsPOXFilename: string;
    function GetAsString: string;
    function GetFieldNames: string;
    function GetAsFloat: Single;
  public
    constructor Create(const FieldNo: Integer; const FieldNames, Value: string);
    property FieldNames: string read GetFieldNames;
    property Value: string read FStrDataRead;
    property AsString: string read GetAsString;
    property AsInteger: Integer read GetAsInteger;
    property AsFloat: Single read GetAsFloat;
    property AsBoolean: Boolean read GetAsBoolean;
    property AsPOXFilename: string read GetAsPOXFilename;
  end;

  TSoAOSFields = class(TObjectDictionary<Integer, TSoAOSField>);

  TCustomSoAOSDataset = class
  strict private
    { Private declarations }
    FFieldNames: TDictionary<string, Integer>;
    FData: TObjectDictionary<string, TSoAOSFields>;
    FCurrentKey: string;
    FCurrentDataRow: TSoAOSFields;
    FVersion: integer;
    function GetFields(Index: Integer): TSoAOSField;
  private
    FDatasetType: TSoAOSDatasetType;
  public
    { Public declarations }
    constructor Create; virtual;
    procedure LoadFromFile(const FileName: string);
    destructor Destroy; override;
    function Locate(const KeyValue: string): boolean;
    function FieldByName(const FieldName: string): TSoAOSField;
    property Key: string read FCurrentKey;
    property Version: integer read FVersion;
    property Fields[Index: Integer]: TSoAOSField read GetFields;
  end;

  TSoAOSXRefTable = class(TCustomSoAOSDataset)
  public
    constructor Create; override;
  end;

  TSoAOSItemsTable = class(TCustomSoAOSDataset)
  public
    constructor Create; override;
  end;

  TSoAOSTitleTable = class(TCustomSoAOSDataset)
  public
    constructor Create; override;
  end;

implementation

uses
  System.Classes,
  System.IOUtils,
  System.SysUtils;

{ TSoAOSField }

constructor TSoAOSField.Create(const FieldNo: Integer; const FieldNames, Value: string);
begin
  FFieldNo := FieldNo;
  FFieldNames := FieldNames;
  FStrDataRead := Value;
end;

function TSoAOSField.GetAsBoolean: Boolean;
begin
  Result := (Self<>nil) and (FStrDataRead.Length > 0) and
    ((FStrDataRead.Chars[0] = 'T') or (FStrDataRead.Chars[0] = 't') or (FStrDataRead.Chars[0] = 'Y') or (FStrDataRead.Chars[0] = 'y'));
end;

function TSoAOSField.GetAsFloat: Single;
begin
  if Self<>nil then
    Result := StrToFloatDef(FStrDataRead, 0.0)
  else
    Result := 0.0;
end;

function TSoAOSField.GetAsInteger: Integer;
begin
  if Self<>nil then
    Result := StrToIntDef(FStrDataRead, 0)
  else
    Result := 0;
end;

function TSoAOSField.GetAsPOXFilename: string;
begin
  if Self=nil then
    Result := ''
  else
    if FStrDataRead.EndsWith('.gif', True) then
      Result := ChangeFileExt(FStrDataRead, '.pox')
    else
      Result := FStrDataRead;
end;

function TSoAOSField.GetAsString: string;
begin
  if Self=nil then
    Result := ''
  else
    Result := FStrDataRead;
end;

function TSoAOSField.GetFieldNames: string;
begin
  if Self=nil then
    Result := ''
  else
    Result := FFieldNames;
end;

{ TCustomSoAOSDataset }

constructor TCustomSoAOSDataset.Create;
begin
  inherited;
  FData := TObjectDictionary<string, TSoAOSFields>.Create([doOwnsValues]);  // key, fields
  FFieldNames := TDictionary<string, Integer>.Create;
end;

destructor TCustomSoAOSDataset.Destroy;
begin
  FData.Free;
  FFieldNames.Free;
  inherited;
end;

function TCustomSoAOSDataset.FieldByName(const FieldName: string): TSoAOSField;
var
  idx: integer;
  fldn: string;
begin
  if (FDatasetType=dstXref) and (FVersion=1) then
    fldn := ExtractFileName(FieldName).ToLower
  else
    fldn := FieldName.ToLower;

  if (FCurrentDataRow<>nil) then
  begin
    if not FFieldNames.TryGetValue(fldn, idx) then  // Version 1.5 - brought by the Elves...
      idx := 1;
    if not FCurrentDataRow.TryGetValue(idx, Result) then
      Result := nil;
  end
  else
    Result := nil;
end;

function TCustomSoAOSDataset.GetFields(Index: Integer): TSoAOSField;
begin
  if (FCurrentDataRow<>nil) and not FCurrentDataRow.TryGetValue(Index, Result) then
    Result := nil;
end;

procedure TCustomSoAOSDataset.LoadFromFile(const FileName: string);
var
  rows: TStringList;
  row, col: Integer;
  rowdata, coldata, fnames: TArray<string>;
  fields: TSoAOSFields;
  fieldcount: Integer;
  dst: TSoAOSDatasetType;
  c: Integer;
  fname: string;
begin
  FData.Clear;
  FCurrentDataRow := nil;
  FFieldNames.Clear;
  FVersion := 1;
  rows := TStringList.Create;
  try
    rows.LoadFromFile(FileName, TEncoding.ANSI);
    if rows.Count<2 then
      Exit;
    coldata := rows[0].Split(['|']);
    // FFieldNames
    fieldcount := Length(coldata);
    if coldata[0]='Base' then
    begin
      // Version check
      if rows[0].Contains(',') then // multiple fieldnames per col
        FVersion := 2;
      dst := dstXref;
      // xref has an "empty" field at the end
      fieldcount := fieldcount-1;
      SetLength(coldata, fieldcount);
      // Set FFieldNames before removing it - to match Items.db and Titles.db no header
      for c := 1 to fieldcount do
      begin
        fnames := coldata[c].Split([',']);
        for fname in fnames do  // V1 has only 1 fname per col
          FFieldNames.AddOrSetValue(fname.ToLower, c);
      end;
      rows.Delete(0);
    end
    else if coldata[0]='aeriekey' then // guess but just a row
    begin
      dst := dstItems;
      // Hardcode from consts - some do have multiple names
      FFieldNames.Add('PiercingMin', 1);
      FFieldNames.Add('CrushingMin', 2);
      FFieldNames.Add('CuttingMin', 3);
      FFieldNames.Add('HeatMin', 4);
      FFieldNames.Add('ColdMin', 5);
      FFieldNames.Add('ElectricMin', 6);
      FFieldNames.Add('PoisonMin', 7);
      FFieldNames.Add('MagicMin', 8);
      FFieldNames.Add('MentalMin', 9);
      FFieldNames.Add('StunMin', 10);
      FFieldNames.Add('SpecialMin', 11);

      FFieldNames.Add('PiercingMax', 12);
      FFieldNames.Add('CrushingMax', 13);
      FFieldNames.Add('CuttingMax', 14);
      FFieldNames.Add('HeatMax', 15);
      FFieldNames.Add('ColdMax', 16);
      FFieldNames.Add('ElectricMax', 17);
      FFieldNames.Add('PoisonMax', 18);
      FFieldNames.Add('MagicMax', 19);
      FFieldNames.Add('MentalMax', 20);
      FFieldNames.Add('StunMax', 21);
      FFieldNames.Add('SpecialMax', 22);

      FFieldNames.Add('PiercingInv', 23);
      FFieldNames.Add('CrushingInv', 24);
      FFieldNames.Add('CuttingInv', 25);
      FFieldNames.Add('HeatInv', 26);
      FFieldNames.Add('ColdInv', 27);
      FFieldNames.Add('ElectricInv', 28);
      FFieldNames.Add('PoisonInv', 29);
      FFieldNames.Add('MagicInv', 30);
      FFieldNames.Add('MentalInv', 31);
      FFieldNames.Add('StunInv', 32);

      FFieldNames.Add('PiercingRes', 33);
      FFieldNames.Add('CrushingRes', 34);
      FFieldNames.Add('CuttingRes', 35);
      FFieldNames.Add('HeatRes', 36);
      FFieldNames.Add('ColdRes', 37);
      FFieldNames.Add('ElectricRes', 38);
      FFieldNames.Add('PoisonRes', 39);
      FFieldNames.Add('MagicRes', 40);
      FFieldNames.Add('MentalRes', 41);
      FFieldNames.Add('StunRes', 42);

      FFieldNames.Add('StrengthSM', 43);
      FFieldNames.Add('CoordinationSM', 44);
      FFieldNames.Add('ConstitutionSM', 45);
      FFieldNames.Add('MysticismSM', 46);
      FFieldNames.Add('CombatSM', 47);
      FFieldNames.Add('StealthSM', 48);
      FFieldNames.Add('RestrictionSM', 49);
      FFieldNames.Add('AttackRecoverySM', 50);
      FFieldNames.Add('HitRecoverySM', 51);
      FFieldNames.Add('PerceptionSM', 52);
      FFieldNames.Add('CharmSM', 53);
      FFieldNames.Add('HealingRateSM', 54);
      FFieldNames.Add('RechargeRateSM', 55);
      FFieldNames.Add('HitPointsSM', 56);
      FFieldNames.Add('ManaSM', 57);
      FFieldNames.Add('AttackSM', 58);
      FFieldNames.Add('DefenseSM', 59);

      FFieldNames.Add('cSlotsAllowed', 60);
      FFieldNames.Add('cItemInfo', 61);
      FFieldNames.Add('cValue', 62);
      FFieldNames.Add('cWeight', 63);
      FFieldNames.Add('cTitle', 63);
      FFieldNames.Add('cMagic', 64);
      FFieldNames.Add('cItemType', 65);
//  cItemInfo= 66;
      FFieldNames.Add('cSecretName', 67);
      FFieldNames.Add('cSecretInfo', 68);
      FFieldNames.Add('cInventoryImage', 69);
      FFieldNames.Add('cPartName', 70);
      FFieldNames.Add('cDisplayName', 71);
      FFieldNames.Add('cInventoryHeight', 72);
      FFieldNames.Add('cInventoryWidth', 73);

      FFieldNames.Add('w2Handed', 74);
      FFieldNames.Add('wRange', 75);
      FFieldNames.Add('wMinStrength', 76);
      FFieldNames.Add('wMinCoordination', 77);
      FFieldNames.Add('wMaxRestriction', 78);
      FFieldNames.Add('wSndAttack', 79);
      FFieldNames.Add('wSndOther', 80);
      FFieldNames.Add('wSndMetal', 81);

      FFieldNames.Add('qFletchingColor', 74);
      FFieldNames.Add('qTracking', 75);
      FFieldNames.Add('qSndOther', 76);
      FFieldNames.Add('qSndMetal', 77);
      FFieldNames.Add('qSndStone', 78);

      FFieldNames.Add('iMaterial', 74);
      fieldcount := 81;
    end
    else
    begin
      dst := dstTitel;
      // Hardcode from consts - some do have multiple names
      FFieldNames.Add('ttVisible', 1);
      FFieldNames.Add('ttStrength', 2);
      FFieldNames.Add('ttCoordination', 3);
      FFieldNames.Add('ttConstitution', 4);
      FFieldNames.Add('ttMysticism', 5);
      FFieldNames.Add('ttCombat', 6);
      FFieldNames.Add('ttStealth', 7);
      FFieldNames.Add('ttRestriction', 8);
      FFieldNames.Add('ttAttackRecovery', 9);
      FFieldNames.Add('ttHitRecovery', 10);
      FFieldNames.Add('ttPerception', 11);
      FFieldNames.Add('ttCharm', 12);
      FFieldNames.Add('ttHealingRate', 13);
      FFieldNames.Add('ttRechargeRate', 14);
      FFieldNames.Add('ttHitPoints', 15);
      FFieldNames.Add('ttMana', 16);
      FFieldNames.Add('ttAttack', 17);
      FFieldNames.Add('ttDefense', 18);
      FFieldNames.Add('ttDisplayName', 19);
      fieldcount := 19;
    end;
    if (FDatasetType <> dst) then
    begin
      if (FDatasetType=dstXref) then raise Exception.Create('Not a valid XRef .db file');
      if (FDatasetType=dstItems) then raise Exception.Create('Not a valid Items .db file');
      if (FDatasetType=dstTitel) then raise Exception.Create('Not a valid Title .db file');
    end;
    // Data
    for row := 0 to rows.Count-1 do
    begin
      rowdata := rows[row].Split(['|']);
//      if Length(rowdata)<>fieldcount then // skip when field count mismatch
//        Continue;
      fields := TSoAOSFields.Create([doOwnsValues]);
      for col := 1 to fieldcount do
        fields.Add(col, TSoAOSField.Create(col, coldata[col], rowdata[col]));
      FData.AddOrSetValue(rowdata[0].ToLower, fields);
    end;

  finally
    rows.Free;
  end;
end;

function TCustomSoAOSDataset.Locate(const KeyValue: string): boolean;
begin
  if SameText(KeyValue, FCurrentKey) and (FCurrentDataRow <> nil) then
    Result := True
  else
  begin
    Result := FData.TryGetValue(KeyValue.ToLower, FCurrentDataRow);
    if Result then FCurrentKey := KeyValue.ToLower;
  end;
end;

{ TSoAOSXRefDB }

constructor TSoAOSXRefTable.Create;
begin
  inherited;
  FDatasetType := dstXref;
end;

{ TSoAOSItemsDB }

constructor TSoAOSItemsTable.Create;
begin
  inherited;
  FDatasetType := dstItems;
end;

{ TSoAOSTitleDB }

constructor TSoAOSTitleTable.Create;
begin
  inherited;
  FDatasetType := dstTitel;
end;

end.
