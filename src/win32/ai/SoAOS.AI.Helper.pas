unit SoAOS.AI.Helper;
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

  Description: SoAOS AI, Spell, Character class helpers - break cyclic reference

  Requires: Delphi 10.3.3 or later

  Revision History:
  - May 2020 - SN: Initial Commit to Git
  see git repo afterwards

*)

interface

uses
  Character,
  SoAOS.Spells,
  SoAOS.AI;

type
  TAIClassHelper = class helper for TAI
  private
    procedure SetCharacter(Value: TCharacter);
    function GetCharacter: TCharacter;
  public
    property Character: TCharacter read GetCharacter write SetCharacter;
  end;

  TPartyAIClassHelper = class helper for TPartyAI
  private
    procedure SetLeader(Value: TCharacter);
    function GetLeader: TCharacter;
    function GetPartyMember(Index: Integer): TCharacter;
    procedure SetPartyMember(Index: Integer; const Value: TCharacter);
    function GetSpellToCast(Index: Integer): TSpell;
    procedure SetSpellToCast(Index: Integer; const Value: TSpell);
  public
    property Leader: TCharacter read GetLeader write SetLeader;
    property PartyMember[ Index: Integer ]: TCharacter read GetPartyMember write SetPartyMember;
    property SpellToCast[Index: Integer]: TSpell read GetSpellToCast write SetSpellToCast;
  end;

  TCharacterClassHelper = class helper for TCharacter
  private
    procedure SetCurrentSpell(Value: TSpell);
    function GetCurrentSpell: TSpell;
    function GetAI: TAI;
    procedure SetAI(const Value: TAI);
    function GetHotKey(Index: Integer): TSpell;
    procedure SetHotKey(Index: Integer; const Value: TSpell);
  public
    property CurrentSpell : TSpell read GetCurrentSpell write SetCurrentSpell;
    property AI: TAI read GetAI write SetAI;
    property HotKey[Index: Integer]: TSpell read GetHotKey write SetHotKey;
  end;

implementation

{ TAIClassHelper }

function TAIClassHelper.GetCharacter: TCharacter;
begin
  Result := TCharacter(FCharacter);
end;

procedure TAIClassHelper.SetCharacter(Value: TCharacter);
begin
  FCharacter := Value;
end;

{ TPartyAIClassHelper }

function TPartyAIClassHelper.GetLeader: TCharacter;
begin
  Result := TCharacter(FLeader);
end;

function TPartyAIClassHelper.GetPartyMember(Index: Integer): TCharacter;
begin
  Result := TCharacter(FPartyMember[Index]);
end;

function TPartyAIClassHelper.GetSpellToCast(Index: Integer): TSpell;
begin
  Result := TSpell(FSpellToCast[Index]);
end;

procedure TPartyAIClassHelper.SetLeader(Value: TCharacter);
begin
  FLeader := Value;
end;

procedure TPartyAIClassHelper.SetPartyMember(Index: Integer;
  const Value: TCharacter);
begin
  FPartyMember[Index] := Value;
end;

procedure TPartyAIClassHelper.SetSpellToCast(Index: Integer;
  const Value: TSpell);
begin
  FSpellToCast[Index] := Value;
end;

{ TCharacterClassHelper }

function TCharacterClassHelper.GetAI: TAI;
begin
  Result := TAI(FAI);
end;

function TCharacterClassHelper.GetCurrentSpell: TSpell;
begin
  Result := TSpell(FCurrentSpell);
end;

function TCharacterClassHelper.GetHotKey(Index: Integer): TSpell;
begin
  Result := TSpell(FHotKey[Index]);
end;

procedure TCharacterClassHelper.SetAI(const Value: TAI);
begin
  if Assigned( FAI ) then
    FAI.Free;
  FAI := Value;
  if Assigned( FAI ) then
  begin
    TAI(FAI).Character := Self;
    if not Self.Loading then
      TAI(FAI).Init;
  end;
end;

procedure TCharacterClassHelper.SetCurrentSpell(Value: TSpell);
begin
  FCurrentSpell := Value;
end;

procedure TCharacterClassHelper.SetHotKey(Index: Integer; const Value: TSpell);
begin
  FHotKey[Index] := Value;
end;

end.
