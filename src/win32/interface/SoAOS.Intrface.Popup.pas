unit SoAOS.Intrface.Popup;
(*
  Siege Of Avalon : Open Source Edition

  Portions created by Steffen Nyeland are
  Copyright (C) 2019 - Steffen Nyeland.

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

  Description: Mouse hover hints

  Requires: Delphi 10.3.3 or later
            DirectX

  Revision History:
  - 28 Dec 2019 - SN: Initial Commit to Git
  see git repo afterwards

*)

interface

uses
  System.Classes,
//  Winapi.DirectDraw,
  DirectX,
  DXUtil;

type
  TPopup = class
  private
    Surface : IDirectDrawSurface;
    Count : Integer;
    FX, FY : Integer;
    FMsgID : Integer;
    FMsgCount : Integer;
    FWidth : Integer;
    Messages : TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw;
  end;

implementation

uses
  SoAOS.Intrface.Text,
  Winapi.Windows,
  System.Types,
  Vcl.Controls,
  Vcl.Graphics,
  Engine,
  Display,
  MousePtr,
  SoAOS.Animation,
  AniDemo;

{ TPopup }

constructor TPopup.Create;
begin
  inherited;
  Messages := TStringList.Create;
  ExText.OpenEncoded( 'Popup' );
  ExText.GetSection( Messages );
  ExText.Close;
  Count := 0;
end;

destructor TPopup.Destroy;
begin
  Messages.Free;
  Surface := nil;
  inherited;
end;

procedure TPopup.Draw;
var
  DC : HDC;
  MsgID : Integer;
  Msg : String;
  BM : TBitmap;
const
  Height = 20;
begin
  Inc( Count );
  if ( Count >= 30 ) and MouseCursor.Enabled then
  begin
    Count := 0;
//    GetCursorPos( P );
    var P: TPoint := Mouse.CursorPos;
    var SpellBarHidden: boolean := not frmMain.SpellBarActive;
	//TODO: Adjust points to HD? Does disabled make a differnce?
    if ScreenMetrics.popInventoryRect.Contains(P) then MsgID := 1 //Inventory
    else if ScreenMetrics.popMapRect.Contains(P) then MsgID := 2 //Map
    else if ScreenMetrics.popQuestRect.Contains(p) then MsgID := 3 //Quest
    else if ScreenMetrics.popAdventureRect.Contains(P) then MsgID := 4 //Adventure
    else if ScreenMetrics.popJournalRect.Contains(P) then MsgID := 5 //Journal
    else if ScreenMetrics.popAwardsRect.Contains(P) and SpellBarHidden then MsgID := 6 //Awards
    else if ScreenMetrics.popMessageRect.Contains(P) and SpellBarHidden then MsgID := 7 //Message Area
    else if ScreenMetrics.popStatsRect.Contains(P) then MsgID := 8 //Player Stats
    else if ScreenMetrics.popManaRect.Contains(P) then MsgID := 9 //Mana
    else if ScreenMetrics.popHealthRect.Contains(P) then MsgID := 10 //Health
    else if ScreenMetrics.popSpellRect.Contains(P) and SpellBarHidden then MsgID := 11 //Spell
    else if ScreenMetrics.popRosterRect.Contains(P) and SpellBarHidden then MsgID := 12 //Roster
    else if ScreenMetrics.popParty1Rect.Contains(P) and SpellBarHidden then MsgID := 13 //Party Member 1
    else if ScreenMetrics.popParty2Rect.Contains(P) and SpellBarHidden then MsgID := 14 //Party Member 2
    else if ScreenMetrics.popParty3Rect.Contains(P) and SpellBarHidden then MsgID := 15 //Party Member 3
    else if ScreenMetrics.popParty4Rect.Contains(P) and SpellBarHidden then MsgID := 16 //Party Member 4
    else MsgID := 0;

    if MsgID = 0 then
    begin
      if FMsgID > 0 then
      begin
        FMsgCount := 0;
        FMsgID := 0;
        if Assigned( Surface ) then
          Surface := nil;
      end;
    end
    else if FMsgID = MsgID then
    begin
      Inc( FMsgCount );
      if FMsgCount = 2 then
      begin
        case MsgID of
          1 : Msg := Messages.Values[ 'Inventory' ];
          2 : Msg := Messages.Values[ 'Map' ];
          3 : Msg := Messages.Values[ 'Quest' ];
          4 : Msg := Messages.Values[ 'Adventure' ];
          5 : Msg := Messages.Values[ 'Journal' ];
          6 : Msg := Messages.Values[ 'Awards' ];
          7 : Msg := Messages.Values[ 'Message' ];
          8 : Msg := Messages.Values[ 'Player' ];
          9 : Msg := Messages.Values[ 'Mana' ];
          10 : Msg := Messages.Values[ 'Health' ];
          11 : Msg := Messages.Values[ 'Spell' ];
          12 : Msg := Messages.Values[ 'Roster' ];
          13 : Msg := Messages.Values[ 'Party' ];
          14 : Msg := Messages.Values[ 'Party' ];
          15 : Msg := Messages.Values[ 'Party' ];
          16 : Msg := Messages.Values[ 'Party' ];
        else
          Msg := '';
        end;

        if Msg <> '' then
        begin
          BM := TBitmap.Create;
          try
            //            BM.Canvas.Font.Name:='fixedsys';
            BM.Canvas.Font.Style := [ fsBold ];
            var R:TRect := Rect( 0, 0, 0, Height );
            DrawText( BM.Canvas.Handle, PWideChar( Msg ), Length( Msg ), R, DT_SINGLELINE or DT_CALCRECT or DT_NOPREFIX );
            Inc( R.Right, 8 );
            R.Bottom := Height;
            FWidth := R.Right;
            BM.width := FWidth;
            BM.Height := Height;
            DrawText( BM.Canvas.Handle, PWideChar( Msg ), Length( Msg ), R, DT_CENTER or DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX );

            Surface := DDGetSurface( lpDD, FWidth, Height, $C0FFE0, True );
            Surface.GetDC( DC );
            try
              BitBlt( DC, 0, 0, FWidth, Height, BM.Canvas.Handle, 0, 0, SRCAND );
            finally
              Surface.ReleaseDC( DC );
            end;
          finally
            BM.Free;
          end;
          if P.X < 200 then
            FX := P.X - 10
          else
            FX := P.X - FWidth + 10;
          FY := P.Y - Height + 10;
        end;
      end;
    end
    else
    begin
      FMsgID := MsgID;
      FMsgCount := 1;
      if Assigned( Surface ) then
        Surface := nil;
    end;
  end;

  if ( FMsgID > 0 ) and Assigned( Surface ) then
  begin
    var pr: TRect := Rect( 0, 0, FWidth, Height );
    lpDDSBack.BltFast( FX, FY, Surface, @pr, DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
  end;

end;

end.
