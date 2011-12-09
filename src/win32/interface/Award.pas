unit Award;
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
  DirectX,
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
  Engine,
  Logfile;
  
type
  TAward = class( TDisplay )
  private
    //Bitmap stuff
    BMBack : TBitmap; //The inventory screen bitmap used for loading
    DXBack : IDirectDrawSurface;
    DXBackToGame : IDirectDrawSurface; //Back To Game highlight
    DXLeftGeeble : IDirectDrawSurface;
    DXRightGeeble : IDirectDrawSurface;
    DXPrev : IDirectDrawSurface;
    DXNext : IDirectDrawSurface;
    DXPrev2 : IDirectDrawSurface;
    DXNext2 : IDirectDrawSurface;
    PageNumber : integer;
    TitleCount : integer;
    txtMessage : array[ 0..17 ] of string;
    procedure ShowText( Page : integer );
  protected
    procedure MouseDown( Sender : TAniview; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
    procedure MouseMove( Sender : TAniview;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
  public
    Character : TCharacter;
    constructor Create;
    destructor Destroy; override;
    procedure Init; override;
    procedure Release; override;
  end;
implementation
uses
  AniDemo;
{ TAward }

constructor TAward.Create;
const
  FailName : string = 'TAward.Create';
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
end; //Create

destructor TAward.Destroy;
const
  FailName : string = 'TAward.Destroy';
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

procedure TAward.Init;
var
  DXBorder : IDirectDrawSurface;
  i : integer;
const
  FailName : string = 'TAward.init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if Loaded then
      Exit;
    inherited;

    ExText.Open( 'Award' );
    for i := 0 to 17 do
      txtMessage[ i ] := ExText.GetText( 'Message' + inttostr( i ) );

    MouseCursor.Cleanup;
    BMBack := TBitmap.Create;
    PageNumber := 0;

  //Get the number of visible titles for this char
    TitleCount := 0;
    for i := 0 to Character.Titles.count - 1 do
    begin
      if assigned( Character.Titles.objects[ i ] ) then
      begin
        if PStatModifier( Character.Titles.objects[ i ] ).visible = true then
        begin
          TitleCount := TitleCount + 1;
        end;
      end;
    end; //end for

    if TitleCount > 15 then
    begin
      BMBack.LoadFromFile( InterfacePath + 'logPrevious.bmp' );
      DXPrev := DDGetImage( lpDD, BMBack, rgb( 255, 0, 255 ), False );

      BMBack.LoadFromFile( InterfacePath + 'logPrevious2.bmp' );
      DXPrev2 := DDGetImage( lpDD, BMBack, rgb( 255, 0, 255 ), False );

      BMBack.LoadFromFile( InterfacePath + 'logNext.bmp' );
      DXNext := DDGetImage( lpDD, BMBack, rgb( 255, 0, 255 ), False );

      BMBack.LoadFromFile( InterfacePath + 'logNext2.bmp' );
      DXNext2 := DDGetImage( lpDD, BMBack, rgb( 255, 0, 255 ), False );
    end;

    lpDDSBack.BltFast( 0, 0, lpDDSFront, Rect( 0, 0, ResWidth, ResHeight ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;

    pText.LoadFontGraphic( 'statistics' ); //load the inventory font graphic in
    pText.LoadTinyFontGraphic;

    BMBack.LoadFromFile( InterfacePath + 'obInvBackToGame.bmp' );
    DXBackToGame := DDGetImage( lpDD, BMBack, $00FFFF00, False );

    BMBack.LoadFromFile( InterfacePath + 'LogLeftGeeble.bmp' );
    DXLeftGeeble := DDGetImage( lpDD, BMBack, rgb( 255, 0, 255 ), False );
    BMBack.LoadFromFile( InterfacePath + 'LogRightGeeble.bmp' );
    DXRightGeeble := DDGetImage( lpDD, BMBack, rgb( 255, 0, 255 ), False );


    BMBack.LoadFromFile( InterfacePath + 'LogScreen.bmp' );
    DXBack := DDGetImage( lpDD, BMBack, rgb( 255, 0, 255 ), False );

    DrawAlpha( DXBack, Rect( 0, 380, 213, 380 + 81 ), Rect( 0, 0, 213, 81 ), DXLeftGeeble, True, 80 );
    DrawAlpha( DXBack, Rect( 452, 0, 452 + 213, 81 ), Rect( 0, 0, 213, 81 ), DXRightGeeble, True, 80 );

    lpDDSBack.BltFast( 0, 0, DXBack, Rect( 0, 0, BMBack.width, BMBack.Height ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

  //Now for the Alpha'ed edges
    BMBack.LoadFromFile( InterfacePath + 'obInvRightShadow.bmp' );
    DXBorder := DDGetImage( lpDD, BMBack, $00FFFF00, False );
    DrawSub( lpDDSBack, Rect( 659, 0, 659 + BMBack.Width, BMBack.Height ), Rect( 0, 0, BMBack.Width, BMBack.Height ), DXBorder, True, 150 );

    DXBorder := nil;

    BMBack.LoadFromFile( InterfacePath + 'obInvBottomShadow.bmp' );
    DXBorder := DDGetImage( lpDD, BMBack, $00FFFF00, False );
    DrawSub( lpDDSBack, Rect( 0, 456, BMBack.Width, 456 + BMBack.Height ), Rect( 0, 0, BMBack.Width, BMBack.Height ), DXBorder, True, 150 );

    DXBorder := nil; //release DXBorder

    BMBack.Free;

    DXLeftGeeble := nil;
    DXRightGeeble := nil;

    pText.PlotText( txtMessage[ 0 ] + Character.name, 5, 5, 240 );

    if TitleCount > 15 then
    begin
      lpDDSBack.BltFast( 400, 424, DXPrev, Rect( 0, 0, 86, 29 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      lpDDSBack.BltFast( 500, 424, DXNext, Rect( 0, 0, 62, 27 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    end;


    ShowText( PageNumber );

    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    lpDDSBack.BltFast( 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //Init


procedure TAward.MouseDown( Sender : TAniview; Button : TMouseButton; Shift : TShiftState; X, Y, GridX, GridY : integer );
var
  i : integer;
const
  FailName : string = 'TAward.MouseDown';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if TitleCount > 15 then
    begin
      if PtinRect( rect( 400, 424, 400 + 86, 424 + 29 ), point( X, Y ) ) then
      begin //over prev
        if PageNumber > 0 then
        begin
          PageNumber := PageNumber - 1;
          lpDDSBack.BltFast( 0, 40, DXBack, Rect( 0, 40, 650, 415 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
          ShowText( PageNumber );
        end;
      end;
      if PtinRect( rect( 500, 424, 500 + 86, 424 + 29 ), point( X, Y ) ) then
      begin //over next
         //Get the mex number of pages
        if ( TitleCount mod 15 ) > 0 then //if there's an extra few items add a page
          i := ( TitleCount div 15 )
        else
          i := ( TitleCount div 15 ) - 1;
        if PageNumber < i then
        begin
          PageNumber := PageNumber + 1;
          lpDDSBack.BltFast( 0, 40, DXBack, Rect( 0, 40, 650, 415 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
          ShowText( PageNumber );
        end;
      end;
    end;
    if PtinRect( rect( 588, 407, 588 + 77, 412 + 54 ), point( X, Y ) ) then
    begin //over back button
      Close;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //MouseDown

procedure TAward.MouseMove( Sender : TAniview; Shift : TShiftState; X, Y, GridX, GridY : integer );
const
  FailName : string = 'TAward.MouseMove';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    lpDDSBack.BltFast( 588, 407, DXBack, Rect( 588, 407, 588 + 77, 407 + 54 ), DDBLTFAST_WAIT );
    if TitleCount > 15 then
    begin
      lpDDSBack.BltFast( 400, 424, DXPrev, Rect( 0, 0, 86, 29 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      lpDDSBack.BltFast( 500, 424, DXNext, Rect( 0, 0, 62, 27 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      if PtinRect( rect( 400, 424, 400 + 86, 424 + 29 ), point( X, Y ) ) then
      begin //over prev
        lpDDSBack.BltFast( 400, 424, DXPrev2, Rect( 0, 0, 86, 29 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end;
      if PtinRect( rect( 500, 424, 500 + 86, 424 + 29 ), point( X, Y ) ) then
      begin //over next
        lpDDSBack.BltFast( 500, 424, DXNext2, Rect( 0, 0, 62, 27 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end;
    end;
    if PtinRect( rect( 588, 407, 588 + 77, 412 + 54 ), point( X, Y ) ) then
    begin //over back button
      //plot highlighted back to game
      lpDDSBack.BltFast( 588, 407, DXBackToGame, Rect( 0, 0, 77, 54 ), DDBLTFAST_WAIT );
    end;

    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    lpDDSBack.BltFast( 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //MouseMove

procedure TAward.ShowText( Page : integer );
var
  i, Y : integer;
  LineStartCount, LineCount : integer;
  S : string;
const
  FailName : string = 'TAward.ShowText';
  Delimeter : string = '  ';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    Y := 40;
    LineCount := 0;
    LineStartCount := 0;
    for i := 0 to Character.Titles.count - 1 do
    begin
      if assigned( Character.Titles.objects[ i ] ) then
      begin
        if PStatModifier( Character.Titles.objects[ i ] ).visible = true then
        begin
          if LineStartCount < Page * 15 then
          begin
            inc( LineStartCount );
          end
          else
          begin
            if LineCount < 15 then
            begin
                  //S:=INI.readString('Quests',LogInfo.Strings[i],'');
              S := '';
              if PStatModifier( Character.Titles.objects[ i ] ).Strength <> 0 then
                S := S + ' ' + txtMessage[ 1 ] + ': ' + IntToStr( PStatModifier( Character.Titles.objects[ i ] ).Strength ) + Delimeter;
              if PStatModifier( Character.Titles.objects[ i ] ).Coordination <> 0 then
                S := S + ' ' + txtMessage[ 2 ] + ': ' + IntToStr( PStatModifier( Character.Titles.objects[ i ] ).Coordination ) + Delimeter;
              if PStatModifier( Character.Titles.objects[ i ] ).Constitution <> 0 then
                S := S + ' ' + txtMessage[ 3 ] + ': ' + IntToStr( PStatModifier( Character.Titles.objects[ i ] ).Constitution ) + Delimeter;
              if PStatModifier( Character.Titles.objects[ i ] ).Mysticism <> 0 then
                S := S + ' ' + txtMessage[ 4 ] + ': ' + IntToStr( PStatModifier( Character.Titles.objects[ i ] ).Mysticism ) + Delimeter;
              if PStatModifier( Character.Titles.objects[ i ] ).Combat <> 0 then
                S := S + ' ' + txtMessage[ 5 ] + ': ' + IntToStr( PStatModifier( Character.Titles.objects[ i ] ).Combat ) + Delimeter;
              if PStatModifier( Character.Titles.objects[ i ] ).Stealth <> 0 then
                S := S + ' ' + txtMessage[ 6 ] + ': ' + IntToStr( PStatModifier( Character.Titles.objects[ i ] ).Stealth ) + Delimeter;
              if PStatModifier( Character.Titles.objects[ i ] ).Restriction <> 0 then
                S := S + ' ' + txtMessage[ 7 ] + ': ' + IntToStr( PStatModifier( Character.Titles.objects[ i ] ).Restriction ) + Delimeter;
              if PStatModifier( Character.Titles.objects[ i ] ).AttackRecovery <> 0 then
                S := S + ' ' + txtMessage[ 8 ] + ': ' + IntToStr( PStatModifier( Character.Titles.objects[ i ] ).AttackRecovery ) + Delimeter;
              if PStatModifier( Character.Titles.objects[ i ] ).HitRecovery <> 0 then
                S := S + ' ' + txtMessage[ 9 ] + ' ' + IntToStr( PStatModifier( Character.Titles.objects[ i ] ).HitRecovery ) + Delimeter;
              if PStatModifier( Character.Titles.objects[ i ] ).Perception <> 0 then
                S := S + ' ' + txtMessage[ 10 ] + ': ' + IntToStr( PStatModifier( Character.Titles.objects[ i ] ).Perception ) + Delimeter;
              if PStatModifier( Character.Titles.objects[ i ] ).Charm <> 0 then
                S := S + ' ' + txtMessage[ 11 ] + ': ' + IntToStr( PStatModifier( Character.Titles.objects[ i ] ).Charm ) + Delimeter;
              if PStatModifier( Character.Titles.objects[ i ] ).HealingRate <> 0 then
                S := S + ' ' + txtMessage[ 12 ] + ': ' + IntToStr( PStatModifier( Character.Titles.objects[ i ] ).HealingRate ) + Delimeter;
              if PStatModifier( Character.Titles.objects[ i ] ).RechargeRate <> 0 then
                S := S + ' ' + txtMessage[ 13 ] + ': ' + IntToStr( PStatModifier( Character.Titles.objects[ i ] ).RechargeRate ) + Delimeter;
              if PStatModifier( Character.Titles.objects[ i ] ).HitPoints <> 0 then
                S := S + ' ' + txtMessage[ 14 ] + ': ' + IntToStr( PStatModifier( Character.Titles.objects[ i ] ).HitPoints ) + Delimeter;
              if PStatModifier( Character.Titles.objects[ i ] ).Mana <> 0 then
                S := S + ' ' + txtMessage[ 15 ] + ': ' + IntToStr( PStatModifier( Character.Titles.objects[ i ] ).Mana ) + Delimeter;
              if PStatModifier( Character.Titles.objects[ i ] ).Attack <> 0 then
                S := S + ' ' + txtMessage[ 16 ] + ': ' + IntToStr( PStatModifier( Character.Titles.objects[ i ] ).Attack ) + Delimeter;
              if PStatModifier( Character.Titles.objects[ i ] ).Defense <> 0 then
                S := S + ' ' + txtMessage[ 17 ] + ': ' + IntToStr( PStatModifier( Character.Titles.objects[ i ] ).Defense ) + Delimeter;

              S := Copy( S, 1, length( S ) - 1 );
              if PStatModifier( Character.Titles.objects[ i ] ).DisplayName = '' then
                pText.PlotTinyText( Character.Titles[ i ] + '   ' + S, 20, Y, 240 )
              else
                pText.PlotTinyText( PStatModifier( Character.Titles.objects[ i ] ).DisplayName + '   ' + S, 20, Y, 240 );
              Y := Y + 25;
              inc( LineCount );
            end;
          end;
        end;
      end;
    end; //end for


  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //ShowText

procedure TAward.Release;
const
  FailName : string = 'TAward.release';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    ExText.close;
    pText.UnLoadTinyFontGraphic;
    if assigned( DXPrev ) then
      DXPrev := nil;
    if assigned( DXNext ) then
      DXNext := nil;
    if assigned( DXPrev2 ) then
      DXPrev2 := nil;
    if assigned( DXNext2 ) then
      DXNext2 := nil;
    DXBack := nil;
    DXBackToGame := nil;
    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

end.
