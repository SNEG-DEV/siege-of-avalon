unit Titles;
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
  ItemDatabase;

type
  TTitlesDB = class( TStringDatabase )
  public
    function GetStatModifier( const Title : string ) : PStatModifier;
  end;

const

  ttVisible = 1;
  ttStrength = 2;
  ttCoordination = 3;
  ttConstitution = 4;
  ttMysticism = 5;
  ttCombat = 6;
  ttStealth = 7;
  ttRestriction = 8;
  ttAttackRecovery = 9;
  ttHitRecovery = 10;
  ttPerception = 11;
  ttCharm = 12;
  ttHealingRate = 13;
  ttRechargeRate = 14;
  ttHitPoints = 15;
  ttMana = 16;
  ttAttack = 17;
  ttDefense = 18;
  ttDisplayName = 19;

var
  TitlesManager : TTitlesDB;

implementation

uses
  System.SysUtils,
  LogFile;

{ TTitlesDB }

function TTitlesDB.GetStatModifier( const Title : string ) : PStatModifier;

  function GetInt( FieldPos : integer ) : integer;
  const
    FailName : string = 'GetInt within GetStamodifier';
  begin
    Log.DebugLog(FailName);
    Result := 0;
    try
      try
        Result := StrToInt( Fields[ FieldPos ] );
      except
        Result := 0;
      end;
    except
      on E : Exception do
        Log.log( FailName + E.Message );
    end;

  end;

const
  FailName : string = 'TTitlesDB.GetStatModifier';
begin
  Log.DebugLog(FailName);
  result := nil;
  try

    if FindRecord( Title ) then
    begin
      new( result );
      result.Strength := GetInt( ttStrength );
      result.Coordination := GetInt( ttCoordination );
      result.Constitution := GetInt( ttConstitution );
      result.Mysticism := GetInt( ttMysticism );
      result.Combat := GetInt( ttCombat );
      result.Stealth := GetInt( ttStealth );
      result.Restriction := GetInt( ttRestriction );
      result.AttackRecovery := GetInt( ttAttackRecovery );
      result.HitRecovery := GetInt( ttHitRecovery );
      result.Perception := GetInt( ttPerception );
      result.Charm := GetInt( ttCharm );
      result.HealingRate := GetInt( ttHealingRate );
      result.RechargeRate := GetInt( ttRechargeRate );
      result.HitPoints := GetInt( ttHitPoints );
      result.Mana := GetInt( ttMana );
      result.Attack := GetInt( ttAttack );
      result.Defense := GetInt( ttDefense );
      result.Visible := lowercase( Fields[ ttVisible ] ) = 'true';
      result.DisplayName := Fields[ ttDisplayName ];
    end
    else
      result := nil;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

end.
