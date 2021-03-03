unit Scroll;
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
//  Winapi.DirectDraw,
  DirectX,
  Vcl.Forms,
  System.Classes,
  System.Types,
  System.SysUtils,
  Character,
  GameText;

type
  TScroll = class( TObject )
  private
    DescList : TStringList;
    DXSheet : IDirectDrawSurface; //Surface used for statistics
    DXFrame : IDirectDrawSurface; //Nifty roller thing for statistics
    DXDirty : IDirectDrawSurface;
    ScrollFactor : integer; //How far we've scrolled
    StatsScrollItem : TItem; //Index for scroll item
    MaxScroll : integer; //max Scroll YCoord
    txtMessage : array[ 0..34 ] of string;
    procedure ShowStatsScroll;
    procedure PlotScrollText(Mx: Integer; My: Integer; ScrollStartValue: Integer; ScrollEndValue: Integer; var i: Integer; k: Integer; txtMsg: string);
    procedure PlotScrollText2(Mx: Integer; My: Integer; ScrollStartValue: Integer; ScrollEndValue: Integer; var i: Integer; k: Integer; txtMsg: string);
    procedure PlotScrollTextPlus(Mx: Integer; My: Integer; ScrollStartValue: Integer; ScrollEndValue: Integer; var i: Integer; k: Integer; txtMsg: string);
    procedure PlotScrollTextResistance(Mx, My, Yadj, ScrollStartValue, ScrollEndValue: Integer; var i: Integer; resistance: TDamageResistance; txtMsg: string);
    procedure PlotScrollTextRange(Mx, My, Yadj, ScrollStartValue, ScrollEndValue: Integer; var i: Integer; range: TDamageRange; txtMsg: string);

  public
    pText : TGameText;
    ScrollIsShowing : Boolean;
    KeepOnScrolling : boolean;
    ScrollAmount : integer; //-1 or 1, to scroll up or down
    procedure ScrollStatsScroll;
    procedure OpenStatsScroll( TheItem : TItem ); //the stats scroll
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  SoAOS.Types,
  SoAOS.Intrface.Text,
  SoAOS.Graphics.Draw,
  AniDemo,
  Engine,
  SoAOS.Animation,
  Logfile,
  Display;

constructor TScroll.Create;
var
  i : integer;
const
  FailName : string = 'TScroll.Create';
begin
  Log.DebugLog(FailName);
  try
    inherited;
    DXDirty := SoAOS_DX_LoadBMP( InterfacePath + 'ScrollFrame.bmp', cInvisColor );
    DXFrame := SoAOS_DX_LoadBMP( InterfacePath + 'ScrollFrame.bmp', cInvisColor );
    DXSheet := SoAOS_DX_LoadBMP( InterfacePath + 'ScrollPage.bmp', cInvisColor );
    DescList := TStringlist.create;
    ExText.Open( 'Scroll' );
    for i := 0 to 34 do
      txtMessage[ i ] := ExText.GetText( 'Message' + inttostr( i ) );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TScroll.Create;

destructor TScroll.Destroy;
const
  FailName : string = 'TScroll.Destroy';
begin
  Log.DebugLog(FailName);
  try
    ExText.close;
    DescList.free;
    DxSheet := nil;
    DxFrame := nil;
    DXDirty := nil;
    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

procedure TScroll.PlotScrollText(Mx: Integer; My: Integer; ScrollStartValue: Integer; ScrollEndValue: Integer; var i: Integer; k: Integer; txtMsg: string);
var
  a: string;
begin
  if k <> 0 then
  begin
    Str(k, a);
    if ((My + 40 + i * 22) > ScrollStartValue) and ((My + 40 + i * 22) < ScrollEndValue - 24) then
    begin
      pText.PlotText(txtMsg, Mx + 50, My + 40 + i * 22, 0);
      pText.PlotText(a, Mx + 249, My + 40 + i * 22, 0);
    end;
    i := i + 1;
  end;
end;

procedure TScroll.PlotScrollText2(Mx, My, ScrollStartValue,
  ScrollEndValue: Integer; var i: Integer; k: Integer; txtMsg: string);
var
  a: string;
begin
  if k <> 0 then
  begin
    Str( k, a );
    if ( ( My + 40 + i * 22 ) > ScrollStartValue ) and ( ( My + 40 + i * 22 ) < ScrollEndValue - 24 ) then
    begin
      pText.PlotText( txtMsg, Mx + 50, My + 40 + i * 22, 0 );
      if k > 0 then
        pText.PlotText( a, Mx + 249, My + 40 + i * 22, 0 )
      else
        pText.PlotText( a, Mx + 240, My + 40 + i * 22, 0 )
    end;
    i := i + 1;
  end;
end;

procedure TScroll.PlotScrollTextRange(Mx, My, Yadj, ScrollStartValue, ScrollEndValue: Integer; var i: Integer; range: TDamageRange; txtMsg: string);
var
  a, b: string;
  k: Integer;
begin
  k := Round( range.Max );
  if k <> 0 then
  begin
    if ( ( My + 40 + YAdj + i * 22 ) > ScrollStartValue ) and ( ( My + 40 + Yadj + i * 22 ) < ScrollEndValue - 24 ) then
    begin
      Str( k, b );
      Str( Round( range.Min ), a );
      pText.PlotText( txtMsg, Mx + 50, My + 40 + YAdj + i * 22, 0 );
      pText.PlotText( a + '-' + b, Mx + 240, My + 40 + YAdj + i * 22, 0 );
    end;
    i := i + 1;
  end;
end;

procedure TScroll.PlotScrollTextResistance(Mx, My, Yadj, ScrollStartValue, ScrollEndValue: Integer; var i: Integer; resistance: TDamageResistance; txtMsg: string);
var
  a, b: string;
  k, j: Integer;
begin
  k := Round( resistance.Invulnerability );
  j := Round( resistance.Resistance * 100 );

  if ( k <> 0 ) or ( j <> 0 ) then
  begin
    if ( ( My + 40 + YAdj + i * 22 ) > ScrollStartValue ) and ( ( My + 40 + Yadj + i * 22 ) < ScrollEndValue - 24 ) then
    begin
      Str( k, a );
      Str( j, b );
      pText.PlotText( txtMsg, Mx + 50, My + 40 + YAdj + i * 22, 0 );
      pText.PlotText( a + '-' + b + '%', Mx + 240, My + 40 + YAdj + i * 22, 0 );
    end;
    i := i + 1;
  end;
end;

procedure TScroll.PlotScrollTextPlus(Mx, My, ScrollStartValue, ScrollEndValue: Integer; var i: Integer; k: Integer; txtMsg: string);
var
  a, b: string;
begin
  if k <> 0 then
  begin
    Str( k, a );
    if k > 0 then
      b := '+'
    else
      b := '';
    if ( ( My + 40 + i * 22 ) > ScrollStartValue ) and ( ( My + 40 + i * 22 ) < ScrollEndValue - 24 ) then
    begin
      pText.PlotText( txtMsg, Mx + 50, My + 40 + i * 22, 0 );
      pText.PlotText( b + a, Mx + 240, My + 40 + i * 22, 0 );
    end;
    i := i + 1;
  end;
end;

procedure TScroll.ShowStatsScroll;
var
  Mx, My : Integer;
  i, j : Integer;
  TitleY : integer;
  Yadj : integer;
  ScrollStartValue : integer; //top of scroll
  ScrollEndValue : integer; //bottom of scroll
  pr : TRect;
const
  FailName : string = 'TScroll.ShowStatsScroll';
begin
  Log.DebugLog(FailName);
  try
    ScrollIsShowing := True;
    Yadj := 0;
    Mx := 172;
    My := 13 + 45 + ScrollFactor;

    ScrollStartValue := 13 + 45;
    ScrollEndValue := ScrollStartValue + 360;
  //show the sheet
    for i := 0 to 3 do
    begin //plot 4 segements of the sheet
      pr := Rect( 0, 0, 338, 90 );
      lpDDSBack.BltFast( Mx, 13 + 45 + i * 90, DXSheet, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    end;
    MaxScroll := 0;
    if ( My + 40 > ScrollStartValue ) and ( My + 40 < ScrollEndValue ) then
    begin
      if pText.PlotTextCentered( StatsScrollItem.Name, Mx, Mx + 330, My + 40, 0 ) then
      begin
      //DebugPrint('Error- not enough room to center name of item ' + StatsScrollItem.Name);
      end;
    end;
  //if pText.PlotTextCentered('Done', Mx, Mx + 305, My + 344) then begin
  //  DebugPrint('Error- not enough room to center done ' + StatsScrollItem.Name);
  //end;
    i := 1;
{  if  (My + 70 < ScrollEndValue) then begin
     if StatsScrollItem.Info > '' then
        i := pText.PlotTextBlock(StatsScrollItem.Info, Mx + 20, Mx + 330 - 20, My + 70,0) + 1;
  end;  }
//NEW**********************
    if StatsScrollItem.Info > '' then
    begin //use the desc as a stringlist, plot it line by line
      for j := 0 to DescList.count - 1 do
      begin
        if ( ( My + 50 + i * 22 ) > ScrollStartValue ) and ( ( My + 50 + i * 22 ) < ScrollEndValue - 24 ) then
        begin
          pText.PlotText( DescList.strings[ j ], Mx + 20, My + 50 + i * 22, 0 );
        end;
        i := i + 1;
      end;
    end;
    MaxScroll := ( DescList.count * 22 );
//NEW**********************

    i := i + 2; //leave room for Title
    TitleY := i - 1; //save line to print title on if there are statstic bonuses
  //restrictions
    if StatsScrollItem is TWeapon then
    begin
      PlotScrollText(Mx, My, ScrollStartValue, ScrollEndValue, i, TWeapon( StatsScrollItem ).MinStrength, txtMessage[0]);
      PlotScrollText(Mx, My, ScrollStartValue, ScrollEndValue, i, TWeapon( StatsScrollItem ).MinCoordination, txtMessage[1]);
      PlotScrollText(Mx, My, ScrollStartValue, ScrollEndValue, i, TWeapon( StatsScrollItem ).MaxRestriction, txtMessage[2]);

      if i > TitleY + 1 then
      begin
        MaxScroll := -( ScrollFactor - ( ( My + 40 + YAdj + TitleY * 22 ) - 88 ) );
        if ( ( My + 40 + TitleY * 22 ) > ScrollStartValue ) and ( ( My + 40 + TitleY * 22 ) < ScrollEndValue - 24 ) then
        begin
          pText.PlotTextCentered( txtMessage[ 3 ], Mx, Mx + 330, My + 40 + TitleY * 22, 0 );
        end;
        i := i + 2;
        TitleY := i - 1;
        YAdj := 10;
      end;
    end; //endif weapon

  //stats bonuses
    PlotScrollTextPlus(Mx, My, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.modifier.Strength, txtMessage[ 4 ]);
    PlotScrollTextPlus(Mx, My, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.modifier.Coordination, txtMessage[ 5 ]);
    PlotScrollTextPlus(Mx, My, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.modifier.Constitution, txtMessage[ 6 ]);
    PlotScrollTextPlus(Mx, My, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.modifier.Mysticism, txtMessage[ 7 ]);
    PlotScrollTextPlus(Mx, My, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.modifier.Combat, txtMessage[ 8 ]);
    PlotScrollTextPlus(Mx, My, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.modifier.Stealth, txtMessage[ 9 ]);

    PlotScrollText2(Mx, My, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.modifier.AttackRecovery, txtMessage[ 10 ]);
    PlotScrollText2(Mx, My, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.modifier.HitRecovery, txtMessage[ 11 ]);

    PlotScrollTextPlus(Mx, My, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.modifier.Perception, txtMessage[ 12 ]);
    PlotScrollTextPlus(Mx, My, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.modifier.Charm, txtMessage[ 13 ]);
    PlotScrollTextPlus(Mx, My, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.modifier.HealingRate, txtMessage[ 14 ]);
    PlotScrollTextPlus(Mx, My, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.modifier.RechargeRate, txtMessage[ 15 ]);
    PlotScrollTextPlus(Mx, My, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.modifier.HitPoints, txtMessage[ 16 ]);
    PlotScrollTextPlus(Mx, My, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.modifier.Mana, txtMessage[ 17 ]);
    PlotScrollTextPlus(Mx, My, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.modifier.Attack, txtMessage[ 18 ]);
    PlotScrollTextPlus(Mx, My, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.modifier.Defense, txtMessage[ 19 ]);
    PlotScrollTextPlus(Mx, My, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.modifier.Restriction, txtMessage[ 20 ]);

    if i > TitleY + 1 then
    begin
      MaxScroll := -( ScrollFactor - ( ( My + 40 + YAdj + TitleY * 22 ) - 88 ) );
      if ( ( My + 40 + TitleY * 22 ) > ScrollStartValue ) and ( ( My + 40 + TitleY * 22 ) < ScrollEndValue - 24 ) then
      begin
        pText.PlotTextCentered( txtMessage[ 21 ], Mx, Mx + 330, My + 40 + TitleY * 22, 0 );
      end;
      i := i + 1;
      TitleY := i - 1;
      Yadj := Yadj + 10;
    end;
//Next is Resistances

    PlotScrollTextResistance(Mx, My, Yadj, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.resistance.Piercing, txtMessage[ 22 ]);
    PlotScrollTextResistance(Mx, My, Yadj, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.resistance.Crushing, txtMessage[ 23 ]);
    PlotScrollTextResistance(Mx, My, Yadj, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.resistance.Cutting, txtMessage[ 24 ]);
    PlotScrollTextResistance(Mx, My, Yadj, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.resistance.Heat, txtMessage[ 25 ]);
    PlotScrollTextResistance(Mx, My, Yadj, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.resistance.Cold, txtMessage[ 26 ]);
    PlotScrollTextResistance(Mx, My, Yadj, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.resistance.Electric, txtMessage[ 27 ]);
    PlotScrollTextResistance(Mx, My, Yadj, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.resistance.Poison, txtMessage[ 28 ]);
    PlotScrollTextResistance(Mx, My, Yadj, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.resistance.Magic, txtMessage[ 29 ]);
    PlotScrollTextResistance(Mx, My, Yadj, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.resistance.Mental, txtMessage[ 30 ]);
    PlotScrollTextResistance(Mx, My, Yadj, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.resistance.Stun, txtMessage[ 31 ]);

    if i > TitleY + 1 then
    begin
      MaxScroll := -( ScrollFactor - ( ( My + 40 + YAdj + TitleY * 22 ) - 88 ) );
      if ( ( My + 40 + YAdj + TitleY * 22 ) > ScrollStartValue ) and ( ( My + 40 + Yadj + TitleY * 22 ) < ScrollEndValue - 24 ) then
      begin
        pText.PlotTextCentered( txtMessage[ 32 ], Mx, Mx + 330, My + 40 + Yadj + TitleY * 22, 0 );
      end;
      i := i + 1;
      TitleY := i - 1;
      Yadj := Yadj + 10;
    end;
//Next is Damage adjustments

    PlotScrollTextRange(Mx, My, Yadj, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.damage.Piercing, txtMessage[ 22 ]);
    PlotScrollTextRange(Mx, My, Yadj, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.damage.Crushing, txtMessage[ 23 ]);
    PlotScrollTextRange(Mx, My, Yadj, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.damage.Cutting, txtMessage[ 24 ]);
    PlotScrollTextRange(Mx, My, Yadj, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.damage.Heat, txtMessage[ 25 ]);
    PlotScrollTextRange(Mx, My, Yadj, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.damage.Cold, txtMessage[ 26 ]);
    PlotScrollTextRange(Mx, My, Yadj, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.damage.Electric, txtMessage[ 27 ]);
    PlotScrollTextRange(Mx, My, Yadj, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.damage.Poison, txtMessage[ 28 ]);
    PlotScrollTextRange(Mx, My, Yadj, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.damage.Magic, txtMessage[ 29 ]);
    PlotScrollTextRange(Mx, My, Yadj, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.damage.Mental, txtMessage[ 30 ]);
    PlotScrollTextRange(Mx, My, Yadj, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.damage.Stun, txtMessage[ 31 ]);
    PlotScrollTextRange(Mx, My, Yadj, ScrollStartValue, ScrollEndValue, i, StatsScrollItem.damage.Special, txtMessage[ 33 ]);

    if i > TitleY + 1 then
    begin
      MaxScroll := -( ScrollFactor - ( ( My + 40 + YAdj + TitleY * 22 ) - 88 ) );
      if ( ( My + 40 + YAdj + TitleY * 22 ) > ScrollStartValue ) and ( ( My + 40 + Yadj + TitleY * 22 ) < ScrollEndValue - 24 ) then
      begin
        pText.PlotTextCentered( txtMessage[ 34 ], Mx, Mx + 330, My + 40 + Yadj + TitleY * 22, 0 );
      end;
    end;

//plot the cleanup for spillover text from the description
    pr := Rect( 0, 0, 338, 45 );
    lpDDSBack.BltFast( 171, 0, DXDirty, @pr, DDBLTFAST_WAIT );
//Now plot the rollers, top and bottom
    pr := Rect( 0, 0, 443, 90 );
    lpDDSBack.BltFast( 119, 13, DXFrame, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    pr := Rect( 0, 0, 443, 90 );
    lpDDSBack.BltFast( 119, 373, DXFrame, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

  //GetCursorPos(myPoint);
  //myPoint:=Game.ScreenToClient(myPoint);
  //if not PtInRect(Rect(119, 0, 600, 550),myPoint) then
    MouseCursor.Cleanup;
    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    pr := Rect( 119, 0, 600, 550 );
    lpDDSBack.BltFast( 119, 0, lpDDSFront, @pr, DDBLTFAST_WAIT );
  //if PtInRect(Rect(119, 0, 600, 550),myPoint) then
    MouseCursor.PlotDirty := false;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TScroll.ShowStatsScroll


procedure TScroll.OpenStatsScroll( TheItem : TItem );
var
  tempString : string;
  pr : TRect;
const
  FailName : string = 'TScroll.OpenStatsScroll';
begin
  Log.DebugLog(FailName);
  try
    ScrollFactor := 0;
    KeepOnScrolling := false;
    StatsScrollItem := TheItem;
   //save this portion of the screen so we can just let text run off the top
    pr := Rect( 171, 0, 509, 45 );
    DXDirty.BltFast( 0, 0, lpDDSBack, @pr, DDBLTFAST_WAIT );
    DescList.clear;
    if StatsScrollItem.Info <> '' then
      tempString := StringReplace( StatsScrollItem.Info, '<CRLF>', char( 13 ), [ rfReplaceAll ] + [ rfIgnoreCase ] );
    pText.BreakTextIntoAStringList( tempString, DescList, 172 + 20, 172 + 310 );
    ShowStatsScroll;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TScroll.OpenStatsScroll


procedure TScroll.ScrollStatsScroll;
var
  OldTime : Longword;
  Adj, TimeDif : real;
const
  FailName : string = 'TScroll.ScrollStatsScroll';
begin
  Log.DebugLog(FailName);
  try
    OldTime := TThread.GetTickCount;  // TStopWatch better
    Adj := 0;
    while KeepOnScrolling do
    begin
      TimeDif := TThread.GetTickCount - OldTime;
      Adj := Adj + 80 * ( TimeDif / 1000 );
      OldTime := TThread.GetTickCount;
      if Adj >= 1 then
      begin
        ScrollFactor := ScrollFactor + ScrollAmount * Trunc( Adj );
        if ScrollFactor < -MaxScroll then
          ScrollFactor := -MaxScroll
        else if ScrollFactor > 0 then
          ScrollFactor := 0;
        ShowStatsScroll;
        Adj := Adj - Trunc( Adj );
      end;
      Application.ProcessMessages;
    end; //wend
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TScroll.ScrollStatsScroll;

end.
