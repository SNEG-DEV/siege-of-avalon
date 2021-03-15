unit SoAOS.Intrface.Dialogs;
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

  Description: Adding offset handling to current TDisplay used by centered menues

  Requires: Delphi 10.3.3 or later

  Revision History:
  - 21 Jan 2020 - SN: Initial Commit to Git
  see git repo afterwards

*)

interface

uses
  System.Types,
//  Winapi.DirectDraw,
  DirectX,
  Display;

type
  TDialog = class( TDisplay )
  private
    function GetOffset: TPoint;
  public
    DlgWidth: Integer;
    DlgHeight: Integer;
    // Local functions to honor offset - should be moved to gametext as relative function
    // PlotText(msg, x, x2, y, relativepoint=nil, centered=false) bla. bla. - will happen on gametext cleanup
    function ApplyOffset(const r: TRect): TRect;
    procedure PlotText(const Sentence: string; const X, Y, Alpha: Integer);
    procedure PlotTextCentered( const DX : IDirectDrawSurface; const Sentence : string; const X1, X2, Y, Alpha : Integer; Const UseSmallFnt: Boolean = False );
    procedure PlotTextBlock( const Sentence : string; X1, X2, Y, Alpha : integer; Const UseSmallFnt: Boolean = False );
//    procedure PlotDarkText2( const DX : IDirectDrawSurface; const Sentence : string; const X, Y, Alpha : integer );
    property Offset: TPoint read GetOffset;
  end;

implementation

uses
  AniDemo;

{ TDialog }

function TDialog.ApplyOffset(const r: TRect): TRect;
begin
  Result := r;
  Result.Offset(Offset);
end;

function TDialog.GetOffset: TPoint;
begin
  Result := TPoint.Create((ScreenMetrics.ScreenWidth - DlgWidth) div 2, (ScreenMetrics.ScreenHeight - DlgHeight) div 2);
end;

//procedure TDialog.PlotDarkText2(const DX: IDirectDrawSurface;
//  const Sentence: string; const X, Y, Alpha: integer);
//begin
//  pText.PlotDarkText2( DX, Sentence, X + Offset.X, Y + Offset.Y, Alpha );
//end;

procedure TDialog.PlotText(const Sentence: string; const X, Y,
  Alpha: Integer);
begin
  pText.PlotText( Sentence, X + Offset.X, Y + Offset.Y, Alpha );
end;

procedure TDialog.PlotTextBlock(const Sentence: string; X1, X2, Y,
  Alpha: integer; const UseSmallFnt: Boolean);
begin
  if UseSmallFnt then
    pText.PlotGoldTextBlock( Sentence, X1 + Offset.X, X2 + Offset.X, Y + Offset.Y, Alpha )
  else
    pText.PlotTextBlock( Sentence, X1 + Offset.X, X2 + Offset.X, Y + Offset.Y, Alpha );
end;

procedure TDialog.PlotTextCentered(const DX: IDirectDrawSurface;
  const Sentence: string; const X1, X2, Y, Alpha: Integer;
  const UseSmallFnt: Boolean);
begin
  if UseSmallFnt then
    pText.PlotGoldTextCentered( DX, Sentence, X1 + Offset.X, X2 + Offset.X, Y + Offset.Y, Alpha )
  else
    pText.PlotTextCentered( Sentence, X1 + Offset.X, X2 + Offset.X, Y + Offset.Y, Alpha );
end;

end.
