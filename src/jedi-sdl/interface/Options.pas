unit Options;
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
{$IFDEF DirectX}
{$IFDEF DX5}
  DirectX,
{$ELSE}
  DirectDraw,
{$ENDIF}
  DXUtil,
  DXEffects,
{$ENDIF}
  Windows,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  Character,
  StdCtrls,
  GameText,
  Display,
  Anigrp30,
  Logfile,
  Engine;

type
  TOptions = class( TDisplay )
  private
    //Bitmap stuff
    BMBack : TBitmap; //The inventory screen bitmap used for loading
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
  protected
    procedure MouseDown( Sender : TAniview; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
    procedure MouseMove( Sender : TAniview;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
    procedure MouseUp( Sender : TAniview; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
    procedure KeyDown( Sender : TObject; var key : Word; Shift : TShiftState ); override;
  public
    SoundVolume : integer;
    MusicVolume : integer;
    PlotShadows : boolean;
    Character : TCharacter;
    IconDX : IDirectDrawSurface;
    opContinueRect : TRect;
    constructor Create;
    destructor Destroy; override;
    procedure Init; override;
    procedure Release; override;
  end;
implementation
uses
  AniDemo;
{ TOptions }

constructor TOptions.Create;
const
  FailName : string = 'TOptions.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
    opContinueRect := Rect( 400, 450, 400 + 300, 450 + 45 );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //Create

destructor TOptions.Destroy;
const
  FailName : string = 'TOptions.Destroys';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //Destroy

procedure TOptions.Init;
var
  InvisColor : integer;
  i : integer;
const
  FailName : string = 'TOptions.init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if Loaded then
      Exit;
    inherited;
    MouseCursor.Cleanup;
    WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, ResWidth, ResHeight ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
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
    BMBack := TBitmap.Create;
  //transparent color
    InvisColor := $00FFFF00;

    BMBack.LoadFromFile( InterfacePath + 'opContinue.bmp' );
    opContinueRect.Right := BMBack.Width;
    opContinueRect.Bottom := BMBack.Height;
    DXContinue := DDGetImage( lpDD, BMBack, InvisColor, False );
    BMBack.LoadFromFile( InterfacePath + 'opYellow.bmp' );
    DXYellow := DDGetImage( lpDD, BMBack, InvisColor, False );

    BMBack.LoadFromFile( InterfacePath + 'opVolume.bmp' );
    DXVolumeSlider := DDGetImage( lpDD, BMBack, InvisColor, False );
    BMBack.LoadFromFile( InterfacePath + 'opVolumeShadow.bmp' );
    DXVolumeShadow := DDGetImage( lpDD, BMBack, InvisColor, False );

    BMBack.LoadFromFile( InterfacePath + 'options.bmp' );
    DXBack := DDGetImage( lpDD, BMBack, InvisColor, False );


  //now we blit the screen to the backbuffer
    WrapperBltFast( lpDDSBack, 0, 0, DXBack, Rect( 0, 0, BMBack.width, BMBack.Height ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

    BMBack.Free;

    PlotMenu;

    ScrollState := 0;
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
const
  FailName : string = 'TOptions.PlotMenu';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

  //clear Volume bars
    WrapperBltFast( lpDDSBack, 116, 92, DXBack, Rect( 116, 92, 315, 105 ), DDBLTFAST_WAIT );
    WrapperBltFast( lpDDSBack, 116, 175, DXBack, Rect( 116, 175, 315, 188 ), DDBLTFAST_WAIT );
  //clear menu
    WrapperBltFast( lpDDSBack, 114, 259, DXBack, rect( 114, 259, 663, 431 ), DDBLTFAST_WAIT );
  //clear Yes/no
    WrapperBltFast( lpDDSBack, 556, 70, DXBack, rect( 556, 70, 650, 90 ), DDBLTFAST_WAIT );

    if PlotShadows then
      DrawAlpha( lpDDSBack, rect( 560, 75, 573, 86 ), rect( 0, 0, 12, 12 ), DXYellow, True, 255 )
    else
      DrawAlpha( lpDDSBack, rect( 632, 75, 645, 86 ), rect( 0, 0, 12, 12 ), DXYellow, True, 255 );
  //Put in the volume
  //Sound FX
    DrawAlpha( lpDDSBack, rect( 116, 92, SoundVolume * 2 + 116, 105 ), rect( 0, 0, 12, 12 ), DXYellow, True, 255 );
    WrapperBltFast( lpDDSBack, SoundVolume * 2 + 116 - 20, 103, DXVolumeSlider, rect( 0, 0, 40, 30 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    DrawSub( lpDDSBack, rect( SoundVolume * 2 + 116 - 20, 103, SoundVolume * 2 + 116 + 20, 103 + 30 ), rect( 0, 0, 40, 30 ), DXVolumeShadow, True, 200 );
  //Music
    DrawAlpha( lpDDSBack, rect( 116, 175, MusicVolume * 2 + 116, 188 ), rect( 0, 0, 12, 12 ), DXYellow, True, 255 );
    WrapperBltFast( lpDDSBack, MusicVolume * 2 + 116 - 20, 184, DXVolumeSlider, rect( 0, 0, 40, 30 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    DrawSub( lpDDSBack, rect( MusicVolume * 2 + 116 - 20, 184, MusicVolume * 2 + 116 + 20, 184 + 30 ), rect( 0, 0, 40, 30 ), DXVolumeShadow, True, 200 );

  //Plot Text HighLight
    if CurrentSelectedListItem <> -1 then
      DrawAlpha( lpDDSBack, rect( 167, ( CurrentSelectedListItem - StartSpell ) * 35 + 259, 595, ( CurrentSelectedListItem - StartSpell ) * 35 + 35 + 259 ), rect( 0, 0, 12, 12 ), DXYellow, True, 40 );

    if Character <> nil then
    begin
      j := 0;
      for i := 0 to SpellList.count - 1 do
      begin
        if ( i >= StartSpell ) and ( i < StartSpell + 5 ) then
        begin //only show 10 files
            //Show a hotkey if associated
          for k := 1 to 8 do
          begin
            if TSpell( SpellList.objects[ i ] ) = Character.HotKey[ k ] then
            begin
              pText.PlotText( 'F' + intToStr( k + 4 ), 611, 264 + j * 35, 240 );
            end;
          end;
            //Plot The Spell Icons
          pt := TSpell( SpellList.objects[ i ] ).GetIconXY( Character );
            //WrapperBltFast( lpDDSBack, 0,0,IconDX,rect(pt.x,pt.y,pt.x+32,pt.y+32),DDBLTFAST_WAIT);
          DrawAlpha( lpDDSBack, rect( 130, 260 + j * 35, 130 + 32, 260 + j * 35 + 32 ), rect( pt.x, pt.y, pt.x + 32, pt.y + 32 ), IconDX, True, 200 );
            //Plot the spell names, but make sure they fit
            //if pText.TextLength(SpellList.Strings[i]) > 215 then
            //  pText.PlotSquishedText(SpellList.Strings[i],426,145+j*35,240)
            //else
          pText.PlotText( TSpell( SpellList.Objects[ i ] ).DisplayName, 181, 264 + j * 35, 240 );
          j := j + 1;
        end
      end; //end for
    end;
    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //PlotMenu

procedure TOptions.KeyDown( Sender : TObject; var key : Word; Shift : TShiftState );
var
  i : integer;
const
  FailName : string = 'TOptions.KeyDown';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

   //function key f5 to f12
    if Character <> nil then
    begin
      if ( key > 115 ) and ( key < 124 ) and ( CurrentSelectedListItem <> -1 ) then
      begin
        Character.HotKey[ key - 115 ] := TSpell( SpellList.objects[ CurrentSelectedListItem ] );
           //We dont want more than one key pointing at the same spell, so if allready assigned, change to nil.
        for i := 1 to 8 do
        begin
          if i <> key - 115 then
          begin
            if Character.HotKey[ key - 115 ] = Character.HotKey[ i ] then
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

procedure TOptions.MouseDown( Sender : TAniview; Button : TMouseButton; Shift : TShiftState; X, Y, GridX, GridY : integer );

const
  FailName : string = 'TOptions.MouseDown';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try


  //FX volume
    if PtInRect( Rect( 100, 88, 335, 125 ), point( x, y ) ) and ( Shift = [ ssLeft ] ) then
    begin
      if X > 316 then
        X := 316
      else if x < 116 then
        X := 116;
      SoundVolume := ( X - 116 ) div 2;
    end //Music volume
    else if PtInRect( Rect( 100, 171, 335, 209 ), point( x, y ) ) and ( Shift = [ ssLeft ] ) then
    begin
      if X > 316 then
        X := 316
      else if x < 116 then
        X := 116;
      MusicVolume := ( X - 116 ) div 2;
    end;

  //check for clicks in spellist
    if Character <> nil then
    begin
      if ptInRect( rect( 112, 259, 665, 431 ), point( x, y ) ) then
      begin
        CurrentSelectedListItem := ( ( Y - 259 ) div 35 ) + StartSpell; //get the Index of the selected Item
        if CurrentSelectedListItem >= SpellList.Count then //don't let them select an empty slot
          CurrentSelectedListItem := -1;
      end;

      //check for scroll arrows
      if ptInRect( rect( 670, 319, 690, 333 ), point( x, y ) ) then
      begin //move up
        if StartSpell > 0 then
        begin
          StartSpell := StartSpell - 1;
          if CurrentSelectedListItem > StartSpell + 4 then //clear it if offscreen
            CurrentSelectedListItem := -1;
          ScrollState := -3;
        end;
      end
      else if ptInRect( rect( 670, 352, 690, 365 ), point( x, y ) ) then
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

    if PtInRect( rect( 557, 71, 576, 89 ), point( x, y ) ) then
    begin //yes
      PlotShadows := true;
    end
    else if PtInRect( rect( 628, 71, 650, 89 ), point( x, y ) ) then
    begin //no
      PlotShadows := false;
    end
    else if PtinRect( rect( 502, 450, 502 + 198, 450 + 45 ), point( X, Y ) ) then
    begin //over back button
      Close; //quit
    end;

  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //mouseDown


procedure TOptions.MouseMove( Sender : TAniview; Shift : TShiftState; X, Y, GridX, GridY : integer );
var
  i, j, k : integer;
  pt : Tpoint;

const
  FailName : string = 'TOptions.MouseMove';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

  //Clear rollover text area
    WrapperBltFast( lpDDSBack, 351, 113, DXBack, Rect( 351, 113, 684, 206 ), DDBLTFAST_WAIT );
  //clear Volume bars
    WrapperBltFast( lpDDSBack, 100, 92, DXBack, Rect( 100, 92, 336, 155 ), DDBLTFAST_WAIT );
    WrapperBltFast( lpDDSBack, 100, 175, DXBack, Rect( 100, 175, 336, 230 ), DDBLTFAST_WAIT );
  //clear menu
    WrapperBltFast( lpDDSBack, 114, 259, DXBack, rect( 114, 259, 663, 434 ), DDBLTFAST_WAIT );
  //clear Yes/no
    WrapperBltFast( lpDDSBack, 556, 70, DXBack, rect( 556, 70, 650, 90 ), DDBLTFAST_WAIT );
  //clear back To Game
    WrapperBltFast( lpDDSBack, opContinueRect.Left, opContinueRect.Top, DXBack, Rect( opContinueRect.Left, opContinueRect.Top, opContinueRect.Left + opContinueRect.Right, opContinueRect.Top + opContinueRect.Bottom ), DDBLTFAST_WAIT );

    if PlotShadows then
      DrawAlpha( lpDDSBack, rect( 560, 75, 573, 86 ), rect( 0, 0, 12, 12 ), DXYellow, True, 255 )
    else
      DrawAlpha( lpDDSBack, rect( 632, 75, 645, 86 ), rect( 0, 0, 12, 12 ), DXYellow, True, 255 );


  //FX volume - we do mousedown check here as well for drag
    if PtInRect( Rect( 100, 88, 335, 125 ), point( x, y ) ) and ( Shift = [ ssLeft ] ) then
    begin
      if X > 316 then
        X := 316
      else if x < 116 then
        X := 116;
      SoundVolume := ( X - 116 ) div 2;
    end //Music volume - we do mousedown check here
    else if PtInRect( Rect( 100, 171, 335, 209 ), point( x, y ) ) and ( Shift = [ ssLeft ] ) then
    begin
      if X > 316 then
        X := 316
      else if x < 116 then
        X := 116;
      MusicVolume := ( X - 116 ) div 2;
    end;


  //Sound FX
    DrawAlpha( lpDDSBack, rect( 116, 92, SoundVolume * 2 + 116, 105 ), rect( 0, 0, 12, 12 ), DXYellow, True, 255 );
    WrapperBltFast( lpDDSBack, SoundVolume * 2 + 116 - 20, 103, DXVolumeSlider, rect( 0, 0, 40, 30 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    DrawSub( lpDDSBack, rect( SoundVolume * 2 + 116 - 20, 103, SoundVolume * 2 + 116 + 20, 103 + 30 ), rect( 0, 0, 40, 30 ), DXVolumeShadow, True, 200 );
  //Music
    DrawAlpha( lpDDSBack, rect( 116, 175, MusicVolume * 2 + 116, 188 ), rect( 0, 0, 12, 12 ), DXYellow, True, 255 );
    WrapperBltFast( lpDDSBack, MusicVolume * 2 + 116 - 20, 184, DXVolumeSlider, rect( 0, 0, 40, 30 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    DrawSub( lpDDSBack, rect( MusicVolume * 2 + 116 - 20, 184, MusicVolume * 2 + 116 + 20, 184 + 30 ), rect( 0, 0, 40, 30 ), DXVolumeShadow, True, 200 );

  //Plot Text HighLight
    if CurrentSelectedListItem <> -1 then
    begin
      DrawAlpha( lpDDSBack, rect( 167, ( CurrentSelectedListItem - StartSpell ) * 35 + 259, 595, ( CurrentSelectedListItem - StartSpell ) * 35 + 35 + 259 ), rect( 0, 0, 12, 12 ), DXYellow, True, 40 );
    end;
    if Character <> nil then
    begin
      j := 0;
      for i := 0 to SpellList.count - 1 do
      begin
        if ( i >= StartSpell ) and ( i < StartSpell + 5 ) then
        begin //only show 10 files
            //Show a hotkey if associated
          for k := 1 to 8 do
          begin
            if TSpell( SpellList.objects[ i ] ) = Character.HotKey[ k ] then
            begin
              pText.PlotText( 'F' + intToStr( k + 4 ), 611, 264 + j * 35, 240 );
            end;
          end;
            //Plot The Spell Icons
          pt := TSpell( SpellList.objects[ i ] ).GetIconXY( Character );
          DrawAlpha( lpDDSBack, rect( 130, 260 + j * 35, 130 + 32, 260 + j * 35 + 32 ), rect( pt.x, pt.y, pt.x + 32, pt.y + 32 ), IconDX, True, 200 );
            //Plot the spell names, but make sure they fit
            //if pText.TextLength(SpellList.Strings[i]) > 215 then
            //  pText.PlotSquishedText(SpellList.Strings[i],426,145+j*35,240)
            //else
          pText.PlotText( TSpell( SpellList.Objects[ i ] ).DisplayName, 181, 264 + j * 35, 240 );
          j := j + 1;
        end
      end; //end for
    end;

    if PtinRect( rect( opContinueRect.Left, opContinueRect.Top, opContinueRect.Left + opContinueRect.Right, opContinueRect.Top + opContinueRect.Bottom ), point( X, Y ) ) then
    begin //over continue
      WrapperBltFast( lpDDSBack, opContinueRect.Left, opContinueRect.Top, DXContinue, Rect( 0, 0, opContinueRect.Right, opContinueRect.Bottom ), DDBLTFAST_WAIT );
    end
    else
    begin
      if UseSmallFont then
      begin
        if PtinRect( rect( 100, 61, 332, 126 ), point( X, Y ) ) then //over SoundFX
          pText.PlotGoldTextBlock( txtMessage[ 0 ], 359, 670, 121, 240 )
        else if PtinRect( rect( 9, 144, 332, 209 ), point( X, Y ) ) then //over music
          pText.PlotGoldTextBlock( txtMessage[ 1 ], 359, 670, 121, 240 )
        else if PtinRect( rect( 350, 61, 695, 98 ), point( X, Y ) ) then //over Shadows
          pText.PlotGoldTextBlock( txtMessage[ 2 ], 359, 670, 121, 240 )
        else if PtinRect( rect( 101, 229, 694, 448 ), point( X, Y ) ) then //over Spells list
          if Character = nil then
            pText.PlotGoldTextBlock( txtMessage[ 3 ], 359, 670, 121, 240 )
          else
            pText.PlotGoldTextBlock( txtMessage[ 4 ], 359, 670, 121, 240 );
      end
      else
      begin
        if PtinRect( rect( 100, 61, 332, 126 ), point( X, Y ) ) then //over SoundFX
          pText.PlotTextBlock( txtMessage[ 0 ], 359, 670, 121, 240 )
        else if PtinRect( rect( 9, 144, 332, 209 ), point( X, Y ) ) then //over music
          pText.PlotTextBlock( txtMessage[ 1 ], 359, 670, 121, 240 )
        else if PtinRect( rect( 350, 61, 695, 98 ), point( X, Y ) ) then //over Shadows
          pText.PlotTextBlock( txtMessage[ 2 ], 359, 670, 121, 240 )
        else if PtinRect( rect( 101, 229, 694, 448 ), point( X, Y ) ) then //over Spells list
          if Character = nil then
            pText.PlotTextBlock( txtMessage[ 3 ], 359, 670, 121, 240 )
          else
            pText.PlotTextBlock( txtMessage[ 4 ], 359, 670, 121, 240 );
      end;
    end;


    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //MouseMove


procedure TOptions.Release;

const
  FailName : string = 'TOptions.Release';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if assigned( Timer ) then
    begin
      Timer.enabled := false;
      Timer.free;
      Timer := nil;
    end;
    ScrollState := 0;

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

  procedure DrawList;
  var
    i, j, k : integer;
    pt : TPoint;
  begin
    if CurrentSelectedListItem <> -1 then
    begin
      DrawAlpha( lpDDSBack, rect( 167, ( CurrentSelectedListItem - StartSpell ) * 35 + 259, 595, ( CurrentSelectedListItem - StartSpell ) * 35 + 35 + 259 ), rect( 0, 0, 12, 12 ), DXYellow, True, 40 );
    end;
    if Character <> nil then
    begin
      WrapperBltFast( lpDDSBack, 114, 259, DXBack, rect( 114, 259, 663, 434 ), DDBLTFAST_WAIT );
      j := 0;
      for i := 0 to SpellList.count - 1 do
      begin
        if ( i >= StartSpell ) and ( i < StartSpell + 5 ) then
        begin //only show 10 files
              //Show a hotkey if associated
          for k := 1 to 8 do
          begin
            if TSpell( SpellList.objects[ i ] ) = Character.HotKey[ k ] then
            begin
              pText.PlotText( 'F' + intToStr( k + 4 ), 611, 264 + j * 35, 240 );
            end;
          end;
              //Plot The Spell Icons
          pt := TSpell( SpellList.objects[ i ] ).GetIconXY( Character );
          DrawAlpha( lpDDSBack, rect( 130, 260 + j * 35, 130 + 32, 260 + j * 35 + 32 ), rect( pt.x, pt.y, pt.x + 32, pt.y + 32 ), IconDX, True, 200 );
          pText.PlotText( TSpell( SpellList.Objects[ i ] ).DisplayName, 181, 264 + j * 35, 240 );
          j := j + 1;
        end
      end; //end for
      lpDDSFront.Flip( nil, DDFLIP_WAIT );
      WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
      MouseCursor.PlotDirty := false;
    end;
  end;

var
  P : TPoint;
begin
  if ScrollState < 0 then
  begin
    GetCursorPos( P );
    if PtinRect( rect( 670, 319, 690, 333 ), P ) then
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
    GetCursorPos( P );
    if PtinRect( rect( 670, 352, 690, 365 ), P ) then
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

procedure TOptions.MouseUp( Sender : TAniview; Button : TMouseButton;
  Shift : TShiftState; X, Y, GridX, GridY : integer );
begin
  inherited;

  ScrollState := 0;
end;

end.

