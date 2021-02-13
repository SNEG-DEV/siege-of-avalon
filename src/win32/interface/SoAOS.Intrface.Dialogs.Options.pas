unit SoAOS.Intrface.Dialogs.Options;
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

  Description: Options Dialog - was Options.pas - a lot more clean-up is coming

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
  DXEffects,
  System.SysUtils,
  System.Types,
  System.Classes,
  Vcl.Controls,
  Vcl.ExtCtrls,
  SoAOS.Intrface.Dialogs,
  Character,
  GameText,
  Logfile,
  SoAOS.Animation,
  Engine;

type
  TOptions = class( TDialog )
  private
    //Bitmap stuff
    DXBack : IDirectDrawSurface; //DD surface that holds the statistics screen before blit
    DXContinue : IDirectDrawSurface;
    DXYellow : IDirectDrawSurface; //used to draw lines
    DXVolumeSlider : IDirectDrawSurface;
    DXVolumeShadow : IDirectDrawSurface;
    XAdj, YAdj : integer; //adjust ments for placement of the sheet
    CurrentSelectedListItem : integer;
    StartSpell : integer;
    SpellList : TStringList;
    txtMessage : array[ 0..4 ] of string;
    Timer : TTimer;
    ScrollState : integer;
    procedure TimerEvent( Sender : TObject );
    procedure PlotMenu;
    function GetContinueRect: TRect;
    procedure DrawList;
  protected
    procedure MouseDown( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : integer ); override;
    procedure MouseMove( Sender : TObject;
      Shift : TShiftState; X, Y, GridX, GridY : integer ); override;
    procedure MouseUp( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : integer ); override;
    procedure MouseWheel( Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean );
    procedure KeyDown( Sender : TObject; var key : Word; Shift : TShiftState ); override;
  public
    SoundVolume : integer;
    MusicVolume : integer;
    PlotShadows : boolean;
    Character : TCharacter;
    IconDX : IDirectDrawSurface;
    procedure Init; override;
    procedure Release; override;
    property ContinueRect: TRect read GetContinueRect;
  end;

implementation

uses
  SoAOS.Types,
  SoAOS.Spells,
  SoAOS.AI.Helper,
  SoAOS.Intrface.Text,
  SoAOS.Graphics.Draw,
  AniDemo;

{ TOptions }

procedure TOptions.DrawList;
var
  i, j, k : integer;
  pt: TPoint;
  pr: TRect;
begin
  if CurrentSelectedListItem <> -1 then
  begin
    pr := ApplyOffset( Rect( 167, ( CurrentSelectedListItem - StartSpell ) * 35 + 259, 595, ( CurrentSelectedListItem - StartSpell ) * 35 + 35 + 259 ) );
    DrawAlpha( lpDDSBack, pr, rect( 0, 0, 12, 12 ), DXYellow, True, 40 );
  end;
  if Character <> nil then
  begin
    pr := Rect( 114, 259, 663, 434 );
    lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
    j := 0;
    for i := 0 to SpellList.count - 1 do
    begin
      if ( i >= StartSpell ) and ( i < StartSpell + 5 ) then
      begin //only show 10 files
            //Show a hotkey if associated
            //  for k := 1 to 8 do
        for k := 1 to 10 do
        begin
          if TSpell( SpellList.objects[ i ] ) = Character.HotKey[ k ] then
          begin
            //  pText.PlotText( 'F' + intToStr( k + 4 ), 611, 264 + j * 35, 240 );
            pText.PlotText( intToStr( k - 1 ), 611+Offset.X, 264 + j * 35+Offset.Y, 240 );
          end;
        end;
            //Plot The Spell Icons
        pt := TSpell( SpellList.objects[ i ] ).GetIconXY( Character );
        pr := ApplyOffset( Rect( 130, 260 + j * 35, 130 + 32, 260 + j * 35 + 32 ) );
        DrawAlpha( lpDDSBack, pr, Rect( pt.x, pt.y, pt.x + 32, pt.y + 32 ), IconDX, True, 200 );
        pText.PlotText( TSpell( SpellList.Objects[ i ] ).DisplayName, 181+Offset.X, 264 + j * 35+Offset.Y, 240 );
        j := j + 1;
      end
    end; //end for
    SoAOS_DX_BltFront;
  end;
end;

function TOptions.GetContinueRect: TRect;
begin
  Result := ApplyOffset( DlgRect.dlgOptContinueRect );
end;

procedure TOptions.Init;
var
  i : integer;
  pr : TRect;
const
  FailName : string = 'TOptions.init';
begin
  Log.DebugLog(FailName);
  try

    if Loaded then
      Exit;
    inherited;
    MouseCursor.Cleanup;
    pr := Rect( 0, 0, ResWidth, ResHeight );
    lpDDSBack.BltFast( 0, 0, lpDDSFront, @pr, DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;

    ExText.Open( 'Options' );
    for i := 0 to 4 do
      txtMessage[ i ] := ExText.GetText( 'Message' + inttostr( i ) );

    if Character <> nil then
      SpellList := Character.SpellList;
  //Sheet coordinate adjustments
    XAdj := 0;
    YAdj := -20;
    CurrentSelectedListItem := -1;
    StartSpell := 0;

    pText.LoadFontGraphic( 'createchar' ); //load the GoldFont font graphic in
    if UseSmallFont then
      pText.LoadGoldFontGraphic;
    DXContinue := SoAOS_DX_LoadBMP( InterfacePath + 'opContinue.bmp', cInvisColor );
    DXYellow := SoAOS_DX_LoadBMP( InterfacePath + 'opYellow.bmp', cInvisColor );
    DXVolumeSlider := SoAOS_DX_LoadBMP( InterfacePath + 'opVolume.bmp', cInvisColor );
    DXVolumeShadow := SoAOS_DX_LoadBMP( InterfacePath + 'opVolumeShadow.bmp', cInvisColor );
    DXBack := SoAOS_DX_LoadBMP( InterfacePath + 'options.bmp', cInvisColor, DlgWidth, DlgHeight );

  //now we blit the screen to the backbuffer
    pr := Rect( 0, 0, DlgWidth, DlgHeight );
    lpDDSBack.BltFast( Offset.X, Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

    PlotMenu;

    ScrollState := 0;
    frmMain.OnMouseWheel := MouseWheel;

    Timer := TTimer.create( nil );
    Timer.onTimer := TimerEvent;
    Timer.Interval := 100;
    Timer.enabled := True;

  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //Init


procedure TOptions.PlotMenu;
var
  i, j, k : integer;
  pt : Tpoint;
  pr : TRect;
const
  FailName : string = 'TOptions.PlotMenu';
begin
  Log.DebugLog(FailName);
  try

  //clear Volume bars
    pr := Rect( 116, 92, 315, 105 );
    lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
    pr := Rect( 116, 175, 315, 188 );
    lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
  //clear menu
    pr := Rect( 114, 259, 663, 431 );
    lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
  //clear Yes/no
    pr := Rect( 556, 70, 650, 90 );
    lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
  //clear resolution
    pr := Rect( 108, 463, 313, 483 );
    lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );

    if PlotShadows then
    begin
      pr := ApplyOffset( Rect( 560, 75, 573, 86 ) );
      DrawAlpha( lpDDSBack, pr, rect( 0, 0, 12, 12 ), DXYellow, True, 255 )
    end
    else
    begin
      pr := ApplyOffset( Rect( 632, 75, 645, 86 ) );
      DrawAlpha( lpDDSBack, pr, rect( 0, 0, 12, 12 ), DXYellow, True, 255 );
    end;

  //Put in the volume
  //Sound FX
    pr := ApplyOffset( Rect( 116, 92, SoundVolume * 2 + 116, 105 ) );
    DrawAlpha( lpDDSBack, pr, rect( 0, 0, 12, 12 ), DXYellow, True, 255 );
    pr := Rect( 0, 0, 40, 30 );
    lpDDSBack.BltFast( SoundVolume * 2 + 116 - 20 + Offset.X, 103 + Offset.Y, DXVolumeSlider, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    pr := ApplyOffset( Rect( SoundVolume * 2 + 116 - 20, 103, SoundVolume * 2 + 116 + 20, 103 + 30 ) );
    DrawSub( lpDDSBack, pr, rect( 0, 0, 40, 30 ), DXVolumeShadow, True, 200 );
  //Music
    pr := ApplyOffset( Rect( 116, 175, MusicVolume * 2 + 116, 188 ) );
    DrawAlpha( lpDDSBack, pr, Rect( 0, 0, 12, 12 ), DXYellow, True, 255 );
    pr := Rect( 0, 0, 40, 30 );
    lpDDSBack.BltFast( MusicVolume * 2 + 116 - 20 + Offset.X, 184 + Offset.Y, DXVolumeSlider, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    pr := ApplyOffset( Rect( MusicVolume * 2 + 116 - 20, 184, MusicVolume * 2 + 116 + 20, 184 + 30 ) );
    DrawSub( lpDDSBack, pr, Rect( 0, 0, 40, 30 ), DXVolumeShadow, True, 200 );

  //Plot Text HighLight
    if CurrentSelectedListItem <> -1 then
    begin
      pr := ApplyOffset( Rect( 167, ( CurrentSelectedListItem - StartSpell ) * 35 + 259, 595, ( CurrentSelectedListItem - StartSpell ) * 35 + 35 + 259 ) );
      DrawAlpha( lpDDSBack, pr, rect( 0, 0, 12, 12 ), DXYellow, True, 40 );
    end;

    if Character <> nil then
    begin
      j := 0;
      for i := 0 to SpellList.count - 1 do
      begin
        if ( i >= StartSpell ) and ( i < StartSpell + 5 ) then
        begin //only show 10 files
		  for k := 1 to 10 do // Keys 0-9
          begin
            if TSpell( SpellList.objects[ i ] ) = Character.HotKey[ k ] then
            begin
              pText.PlotText( intToStr( k - 1), 611+Offset.X, 264 + j * 35+Offset.Y, 240 );
            end;
          end;
            //Plot The Spell Icons
          pt := TSpell( SpellList.objects[ i ] ).GetIconXY( Character );
            //lpDDSBack.Bltfast(0,0,IconDX,rect(pt.x,pt.y,pt.x+32,pt.y+32),DDBLTFAST_WAIT);
          DrawAlpha( lpDDSBack, rect( 130+Offset.X, 260 + j * 35+Offset.Y, 130 + 32+Offset.X, 260 + j * 35 + 32+Offset.Y ), rect( pt.x, pt.y, pt.x + 32, pt.y + 32 ), IconDX, True, 200 );
            //Plot the spell names, but make sure they fit
            //if pText.TextLength(SpellList.Strings[i]) > 215 then
            //  pText.PlotSquishedText(SpellList.Strings[i],426,145+j*35,240)
            //else
          pText.PlotText( TSpell( SpellList.Objects[ i ] ).DisplayName, 181+Offset.X, 264 + j * 35+Offset.Y, 240 );
          j := j + 1;
        end
      end; //end for
    end;
    SoAOS_DX_BltFront;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

//PlotMenu

procedure TOptions.KeyDown( Sender : TObject; var key : Word; Shift : TShiftState );
var
  i : integer;
const
  FailName : string = 'TOptions.KeyDown';
begin
  Log.DebugLog(FailName);
  try

   //was function key f3 to f12, now 0-9
    if Character <> nil then
    begin
//      if ( key > 115 ) and ( key < 124 ) and ( CurrentSelectedListItem <> -1 ) then
if ( key > 47 ) and ( key < 58 ) and ( CurrentSelectedListItem <> -1 ) then
      begin
//        Character.HotKey[ key - 115 ] := TSpell( SpellList.objects[ CurrentSelectedListItem ] );
Character.HotKey[ key - 47 ] := TSpell( SpellList.objects[ CurrentSelectedListItem ] );
           //We dont want more than one key pointing at the same spell, so if allready assigned, change to nil.
//        for i := 1 to 8 do
for i := 1 to 10 do
        begin
//          if i <> key - 115 then
if i <> key - 47 then
          begin
//            if Character.HotKey[ key - 115 ] = Character.HotKey[ i ] then
if Character.HotKey[ key - 47 ] = Character.HotKey[ i ] then
              Character.HotKey[ i ] := nil;
          end
        end;
        PlotMenu;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //KeyDown

procedure TOptions.MouseDown( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer );
var
  pr1, pr2, pr3 : TRect;
const
  FailName : string = 'TOptions.MouseDown';
begin
  Log.DebugLog(FailName);
  try
    //FX volume
    pr1 := ApplyOffset( Rect( 100, 88, 335, 125 ) );
    pr2 := ApplyOffset( Rect( 100, 171, 335, 209 ) );

    if pr1.Contains( Point( x, y ) ) and ( Shift = [ ssLeft ] ) then
    begin
      if X > (316 + Offset.X) then
        X := (316 + Offset.X)
      else if x < (116 + Offset.X) then
        X := (116 + Offset.X);
      SoundVolume := ( X - (116 + Offset.X) ) div 2;
    end //Music volume
    else if pr2.Contains( Point( x, y ) ) and ( Shift = [ ssLeft ] ) then
    begin
      if X > (316 + Offset.X) then
        X := (316 + Offset.X)
      else if x < (116 + Offset.X) then
        X := (116 + Offset.X);
      MusicVolume := ( X - (116 + Offset.X) ) div 2;
    end;

  //check for clicks in spellist
    if Character <> nil then
    begin
      pr1 := ApplyOffset( Rect( 112, 259, 665, 431 ) );
      pr2 := ApplyOffset( Rect( 670, 319, 690, 333 ) );
      pr3:= ApplyOffset( Rect( 670, 352, 690, 365 ) );

      if pr1.Contains( Point( x, y ) ) then
      begin
        CurrentSelectedListItem := ( ( ( Y - Offset.Y ) - 259 ) div 35 ) + StartSpell; //get the Index of the selected Item
        if CurrentSelectedListItem >= SpellList.Count then //don't let them select an empty slot
          CurrentSelectedListItem := -1;
      end;

      //check for scroll arrows
      if pr2.Contains( Point( x, y ) ) then
      begin //move up
        if StartSpell > 0 then
        begin
          StartSpell := StartSpell - 1;
          if CurrentSelectedListItem > StartSpell + 4 then //clear it if offscreen
            CurrentSelectedListItem := -1;
          ScrollState := -3;
        end;
      end
      else if pr3.Contains( Point( x, y ) ) then
      begin //move down
        if StartSpell + 4 < SpellList.count - 1 then
        begin
          StartSpell := StartSpell + 1;
          if CurrentSelectedListItem < StartSpell then //clear it if offscreen
            CurrentSelectedListItem := -1;
          ScrollState := 3;
        end;
      end
      else
        ScrollState := 0;
    end; //if char = nil

    pr1 := ApplyOffset( Rect( 557, 71, 576, 89 ) );
    pr2 := ApplyOffset( Rect( 628, 71, 650, 89 ) );

    if pr1.Contains( Point( x, y ) ) then
    begin //yes
      PlotShadows := true;
    end
    else if pr2.Contains( Point( x, y ) ) then
    begin //no
      PlotShadows := false;
    end
    else if ContinueRect.Contains( Point( X, Y ) ) then
    begin //over back button
      Close; //quit
    end;

  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //mouseDown

procedure TOptions.MouseMove( Sender : TObject;
      Shift : TShiftState; X, Y, GridX, GridY : Integer );
var
  i, j, k : integer;
  pt : TPoint;
  pr, pr1, pr2,
  FXRect, MusRect, ShdwRect, SpllRect : TRect;
const
  FailName : string = 'TOptions.MouseMove';
begin
  Log.DebugLog(FailName);
  try

  //Clear rollover text area
    pr := Rect( 345, 110, 690, 240 );
    lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
  //clear Volume bars
    pr := Rect( 100, 92, 336, 155 );
    lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
    pr := Rect( 100, 175, 336, 230 );
    lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
  //clear menu
    pr := Rect( 114, 259, 663, 434 );
    lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
  //clear Yes/no
    pr := Rect( 556, 70, 650, 90 );
    lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
  //clear back To Game
    pr := DlgRect.dlgOptContinueRect;
    lpDDSBack.BltFast( ContinueRect.Left, ContinueRect.Top, DXBack, @pr, DDBLTFAST_WAIT );

    if PlotShadows then
    begin
      pr := ApplyOffset( Rect( 560, 75, 573, 86 ) );
      DrawAlpha( lpDDSBack, pr, Rect( 0, 0, 12, 12 ), DXYellow, True, 255 );
    end
    else
    begin
      pr := ApplyOffset( Rect( 632, 75, 645, 86 ) );
      DrawAlpha( lpDDSBack, pr, Rect( 0, 0, 12, 12 ), DXYellow, True, 255 );
    end;

  //FX volume - we do mousedown check here as well for drag
    pr1 := ApplyOffset( Rect( 100, 88, 335, 125 ) );
    pr2 := ApplyOffset( Rect( 100, 171, 335, 209 ) );

    if pr1.Contains( Point( x, y ) ) and ( Shift = [ ssLeft ] ) then
    begin
      if X > (316 + Offset.X) then
        X := (316 + Offset.X)
      else if x < (116 + Offset.X) then
        X := (116 + Offset.X);
      SoundVolume := ( X - (116 + Offset.X) ) div 2;
    end //Music volume - we do mousedown check here
    else if pr2.Contains( Point( x, y ) ) and ( Shift = [ ssLeft ] ) then
    begin
      if X > (316 + Offset.X) then
        X := (316 + Offset.X)
      else if x < (116 + Offset.X) then
        X := (116 + Offset.X);
      MusicVolume := ( X - (116 + Offset.X) ) div 2;
    end;

  //Sound FX
    pr := ApplyOffset( Rect( 116, 92, SoundVolume * 2 + 116, 105 ) );
    DrawAlpha( lpDDSBack, pr, Rect( 0, 0, 12, 12 ), DXYellow, True, 255 );
    pr := Rect( 0, 0, 40, 30 );
    lpDDSBack.BltFast( SoundVolume * 2 + 116 - 20 + Offset.X, 103 + Offset.Y, DXVolumeSlider, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    pr := ApplyOffset( Rect( SoundVolume * 2 + 116 - 20, 103, SoundVolume * 2 + 116 + 20, 103 + 30 ) );
    DrawSub( lpDDSBack, pr, Rect( 0, 0, 40, 30 ), DXVolumeShadow, True, 200 );
  //Music
    pr := ApplyOffset( Rect( 116, 175, MusicVolume * 2 + 116, 188 ) );
    DrawAlpha( lpDDSBack, pr, Rect( 0, 0, 12, 12 ), DXYellow, True, 255 );
    pr := Rect( 0, 0, 40, 30 );
    lpDDSBack.BltFast( MusicVolume * 2 + 116 - 20 + Offset.X, 184 + Offset.Y, DXVolumeSlider, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    pr := ApplyOffset( Rect( MusicVolume * 2 + 116 - 20, 184, MusicVolume * 2 + 116 + 20, 184 + 30 ) );
    DrawSub( lpDDSBack, pr, Rect( 0, 0, 40, 30 ), DXVolumeShadow, True, 200 );
  //Plot Text HighLight
    if CurrentSelectedListItem <> -1 then
    begin
      pr := ApplyOffset( Rect( 167, ( CurrentSelectedListItem - StartSpell ) * 35 + 259, 595, ( CurrentSelectedListItem - StartSpell ) * 35 + 35 + 259 ) );
      DrawAlpha( lpDDSBack, pr, Rect( 0, 0, 12, 12 ), DXYellow, True, 40 );
    end;
    if Character <> nil then
    begin
      j := 0;
      for i := 0 to SpellList.count - 1 do
      begin
        if ( i >= StartSpell ) and ( i < StartSpell + 5 ) then
        begin //only show 10 files
            //Show a hotkey if associated
            // for k := 1 to 8 do
          for k := 1 to 10 do
          begin
            if TSpell( SpellList.objects[ i ] ) = Character.HotKey[ k ] then
            begin
            // pText.PlotText( 'F' + intToStr( k + 4 ), 611, 264 + j * 35, 240 );
              pText.PlotText( intToStr( k - 1 ), 611+Offset.X, 264 + j * 35+Offset.Y, 240 );
            end;
          end;
            //Plot The Spell Icons
          pt := TSpell( SpellList.objects[ i ] ).GetIconXY( Character );
          pr := ApplyOffset( Rect( 130, 260 + j * 35, 130 + 32, 260 + j * 35 + 32 ) );
          DrawAlpha( lpDDSBack, pr, Rect( pt.x, pt.y, pt.x + 32, pt.y + 32 ), IconDX, True, 200 );
            //Plot the spell names, but make sure they fit
            //if pText.TextLength(SpellList.Strings[i]) > 215 then
            //  pText.PlotSquishedText(SpellList.Strings[i],426,145+j*35,240)
            //else
          pText.PlotText( TSpell( SpellList.Objects[ i ] ).DisplayName, 181+Offset.X, 264 + j * 35+Offset.Y, 240 );
          j := j + 1;
        end
      end; //end for
    end;

    if ContinueRect.Contains( Point( X, Y ) ) then
    begin //over continue
      pr := Rect( 0, 0, DlgRect.dlgOptContinueRect.Width, DlgRect.dlgOptContinueRect.Height );
      lpDDSBack.BltFast( ContinueRect.Left, ContinueRect.Top, DXContinue, @pr, DDBLTFAST_WAIT );
    end
    else
    begin
      FXRect := ApplyOffset( Rect( 100, 61, 332, 126 ) );
      MusRect := ApplyOffset( Rect( 9, 144, 332, 209 ) );
      ShdwRect := ApplyOffset( Rect( 350, 61, 695, 98 ) );
      SpllRect := ApplyOffset( Rect( 101, 229, 694, 448 ) );

      if FXRect.Contains( Point( X, Y ) ) then //over SoundFX
        PlotTextBlock( txtMessage[ 0 ], 359, 670, 121, 240, UseSmallFont )
      else if MusRect.Contains( Point( X, Y ) ) then //over music
        PlotTextBlock( txtMessage[ 1 ], 359, 670, 121, 240, UseSmallFont )
      else if ShdwRect.Contains( Point( X, Y ) ) then //over Shadows
        PlotTextBlock( txtMessage[ 2 ], 359, 670, 121, 240, UseSmallFont )
      else if SpllRect.Contains( Point( X, Y ) ) then //over Spells list
        if Character = nil then
          PlotTextBlock( txtMessage[ 3 ], 359, 670, 121, 240, UseSmallFont )
        else
          PlotTextBlock( txtMessage[ 4 ], 359, 670, 121, 240, UseSmallFont );
    end;

    SoAOS_DX_BltFront;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //MouseMove


procedure TOptions.Release;

const
  FailName : string = 'TOptions.Release';
begin
  Log.DebugLog(FailName);
  try
    if assigned( Timer ) then
    begin
      Timer.enabled := false;
      Timer.free;
      Timer := nil;
    end;
    ScrollState := 0;
    frmMain.OnMouseWheel := nil;

    ExText.close;
    pText.UnloadGoldFontGraphic;
    if Character <> nil then
      SpellList.free; //free the spelllist passed here

    DXBack := nil;
    DXContinue := nil;
    DXYellow := nil;
    DXVolumeSlider := nil;
    DXVolumeShadow := nil;

    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;


procedure TOptions.TimerEvent( Sender : TObject );
var
  P : TPoint;
  pr: TRect;
begin
  if ScrollState < 0 then
  begin
//    GetCursorPos( P );
    P := Mouse.CursorPos;
    pr := ApplyOffset( Rect( 670, 319, 690, 333 ) );
    if pr.Contains( P ) then
    begin //up arrow
      if ScrollState < -1 then
        inc( ScrollState )
      else
      begin
        if StartSpell > 0 then
        begin
          StartSpell := StartSpell - 1;
          if CurrentSelectedListItem > StartSpell + 4 then //clear it if offscreen
            CurrentSelectedListItem := -1;
          DrawList;
        end;
      end;
    end
    else
      ScrollState := 0;
  end
  else if ScrollState > 0 then
  begin
    // GetCursorPos( P );
    P := Mouse.CursorPos;
    pr := ApplyOffset( Rect( 670, 352, 690, 365 ) );
    if pr.Contains( P ) then
    begin //down arrow
      if ScrollState > 1 then
        dec( ScrollState )
      else
      begin
        if StartSpell + 4 < SpellList.count - 1 then
        begin
          StartSpell := StartSpell + 1;
          if CurrentSelectedListItem < StartSpell then //clear it if offscreen
            CurrentSelectedListItem := -1;
          DrawList;
        end;
      end;
    end
    else
      ScrollState := 0;
  end;
end;

procedure TOptions.MouseUp( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : integer );
begin
  inherited;
  ScrollState := 0;
end;

procedure TOptions.MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;

  if WheelDelta<0 then
  begin //move up
    if StartSpell > 0 then
    begin
      StartSpell := StartSpell - 1;
      if CurrentSelectedListItem > StartSpell + 4 then //clear it if offscreen
        CurrentSelectedListItem := -1;
      ScrollState := -3;
      DrawList;
    end;
  end
  else if WheelDelta>0 then
  begin //move down
    if StartSpell + 4 < SpellList.count - 1 then
    begin
      StartSpell := StartSpell + 1;
      if CurrentSelectedListItem < StartSpell then //clear it if offscreen
        CurrentSelectedListItem := -1;
      ScrollState := 3;
      DrawList;
    end;
  end
  else
    ScrollState := 0;
end;

end.
