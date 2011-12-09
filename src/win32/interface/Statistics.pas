unit Statistics;
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
  Resource,
  Logfile;
  
type
  InformationRect = record
    rect : TRect;
    info : string;
    Disabled : boolean;
  end;

  TStatistics = class( TDisplay )
  private
    BMBack : TBitmap; //The inventory screen bitmap used for loading
    DXBack : IDirectDrawSurface; //DD surface that holds the statistics screen before blit
    DXRightArrow : IDirectDrawSurface;
    DXLeftArrow : IDirectDrawSurface;
    DXBackToGame : IDirectDrawSurface;
    InfoRect : array[ 0..96 ] of InformationRect; //collision rects for information
    ArrowRect : array[ 0..15 ] of InformationRect; //collision rects for arrows
    StatAdjustments : array[ 0..7 ] of integer; //used to see if we've added points to a stat or not
    StatName : array[ 0..1, 0..11 ] of string;
    //base stuff - saved in case we do a cancel
    Damage : TDamageProfile;
    Resistance : TDamageResistanceProfile;
    BaseStrength : integer;
    BaseCoordination : integer;
    BaseConstitution : integer;
    BasePerception : integer;
    BaseCharm : integer;
    BaseMysticism : integer;
    BaseCombat : integer;
    BaseStealth : integer;
    TrainingPoints : integer;
    txtMessage : array[ 0..64 ] of string;
    procedure ArrowInfo( i : integer ); //info can change
    procedure LoadBaseValues; //saves the base stats of the character
    procedure LoadNames;
    procedure CreateCollisionRects; //create the rects for the collision detection
    procedure ShowStats; //plots all the numbers on the screen
//    procedure DebugPlot(i: integer);
  protected
    procedure MouseDown( Sender : TAniview; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
    procedure MouseMove( Sender : TAniview;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
    procedure MouseUp( Sender : TAniview; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
  public
    Character : Tcharacter;
    constructor Create;
    destructor Destroy; override;
    procedure Paint; override;
    procedure Init; override;
    procedure Release; override;
  end;


implementation
uses
  AniDemo;
const
  TRAININGJUMP : integer = 10;
  TRAININGBASE : integer = 20;


{ TStatistics }

constructor TStatistics.Create;
const
  FailName : string = 'TStatistics.create';
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

end;

destructor TStatistics.Destroy;
const
  FailName : string = 'TStatistics.Destroy';
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

end;


procedure TStatistics.Init;
var
  InvisColor : Integer; //Transparent color :RGB(0,255,255)
  DXBorder : IDirectDrawSurface;
  i : integer;
const
  FailName : string = 'TStatistics.init';
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
    lpDDSBack.BltFast( 0, 0, lpDDSFront, Rect( 0, 0, ResWidth, ResHeight ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;

    ExText.Open( 'Statistics' );
    for i := 0 to 64 do
      txtMessage[ i ] := ExText.GetText( 'Message' + inttostr( i ) );

    pText.LoadFontGraphic( 'statistics' ); //load the statisctics font graphic in
    pText.LoadTinyFontGraphic;
    LoadNames;
    CreateCollisionRects;
    LoadBaseValues;
    BMBack := TBitmap.Create;
  //transparent color
    InvisColor := $00FFFF00;
  //Load the Background Bitmap and plot it
{$IFDEF DirectX}
//  BMBack.LoadFromFile(ExtractFilePath(Application.ExeName) + '\BaseInterface.bmp');
//  DXBack := DDGetImage(lpDD, BMBack, InvisColor, False);
//  lpDDSBack.BltFast(0,0,DXBack,rect(0,0,800,600),DDBLTFAST_WAIT);
    BMBack.LoadFromFile( InterfacePath + 'staRightArrow.bmp' );
    DXRightArrow := DDGetImage( lpDD, BMBack, InvisColor, False );
    BMBack.LoadFromFile( InterfacePath + 'staLeftArrow.bmp' );
    DXLeftArrow := DDGetImage( lpDD, BMBack, InvisColor, False );
    BMBack.LoadFromFile( InterfacePath + 'staBackToGame.bmp' );
    DXBackToGame := DDGetImage( lpDD, BMBack, InvisColor, False );

    BMBack.LoadFromFile( InterfacePath + 'Statistics.bmp' );
    DXBack := DDGetImage( lpDD, BMBack, InvisColor, False );
  //Plot the arrows on the background
    for i := 0 to 15 do
    begin
      if i < 8 then
        //DXBack.BltFast(ArrowRect[i].rect.left, ArrowRect[i].rect.top, DXLeftArrow, Rect(0, 0, 20, 15), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT) //plot the highlight
        DrawAlpha( DXBack, rect( ArrowRect[ i ].rect.left, ArrowRect[ i ].rect.top, ArrowRect[ i ].rect.left + 20, ArrowRect[ i ].rect.top + 15 ), rect( 0, 0, 20, 15 ), DXLeftArrow, True, 100 )
      else
        DrawAlpha( DXBack, rect( ArrowRect[ i ].rect.left - 4, ArrowRect[ i ].rect.top, ArrowRect[ i ].rect.left - 4 + 20, ArrowRect[ i ].rect.top + 15 ), rect( 0, 0, 20, 15 ), DXRightArrow, True, 100 );
        //DXBack.BltFast(ArrowRect[i].rect.left-4, ArrowRect[i].rect.top, DXRightArrow, Rect(0, 0, 20, 15), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT); //plot the highlight
    end; //end for

    lpDDSBack.BltFast( 0, 0, DXBack, Rect( 0, 0, BMBack.width, BMBack.Height ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
  //Now for the Alpha'ed edges
    BMBack.LoadFromFile( InterfacePath + 'staRightshad.bmp' );
    DXBorder := DDGetImage( lpDD, BMBack, InvisColor, False );
    DrawSub( lpDDSBack, Rect( 647, 0, 647 + BMBack.Width, BMBack.Height ), Rect( 0, 0, BMBack.Width, BMBack.Height ), DXBorder, True, 128 );

    DXBorder := nil;

    BMBack.LoadFromFile( InterfacePath + 'staBottomshad.bmp' );
    DXBorder := DDGetImage( lpDD, BMBack, InvisColor, False );
    DrawSub( lpDDSBack, Rect( 0, 455, BMBack.Width, 455 + BMBack.Height ), Rect( 0, 0, BMBack.Width, BMBack.Height ), DXBorder, True, 128 );

    DXBorder := nil; //release DXBorder
{$ENDIF}

{$IFDEF DirectX}
  //release the bitmap
    BMBack.Free;
    ShowStats;
  //Whew! Now we flip it all to the screen
    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    lpDDSBack.BltFast( 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;
{$ENDIF}
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TStatistics.Init;

procedure TStatistics.Release;
const
  FailName : string = 'TStatistics.release';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    pText.UnloadTinyFontGraphic;
    ExText.Close;
    DXLeftArrow := nil;
    DXRightArrow := nil;
    DXBackToGame := nil;
    DXBack := nil;

    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TStatistics.Release

procedure TStatistics.MouseDown( Sender : TAniview; Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer; GridX, GridY : integer );
var
  B1, B2, B3, B4 : boolean;
  OneLessBack : boolean;
  i : integer;
  PointAdjust : integer;
const
  FailName : string = 'TStatistics.Mousedown';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    i := 0;
    while i < 16 do
    begin
      if ptInRect( ArrowRect[ i ].rect, point( X, Y ) ) then
      begin //if over an Arrow
        //B1:= ((i > 7) and (i < 13 )and(Character.TrainingPoints > 3));
        B1 := false;
        B2 := false;
        B3 := false;
        B4 := false;
        OneLessBack := false;
        PointAdjust := 0;
        if i = 8 then
        begin
          if Character.BaseStrength < TRAININGBASE then
            PointAdjust := 4
          else
            PointAdjust := ( Character.BaseStrength - TRAININGBASE ) div TRAININGJUMP + 5;
          if Character.TrainingPoints >= PointAdjust then
            B1 := true;
        end
        else if i = 9 then
        begin
          if Character.BaseCoordination < TRAININGBASE then
            PointAdjust := 4
          else
            PointAdjust := ( Character.BaseCoordination - TRAININGBASE ) div TRAININGJUMP + 5;
          if Character.TrainingPoints >= PointAdjust then
            B1 := true;
        end
        else if i = 10 then
        begin
          if Character.BaseConstitution < TRAININGBASE then
            PointAdjust := 4
          else
            PointAdjust := ( Character.BaseConstitution - TRAININGBASE ) div TRAININGJUMP + 5;
          if Character.TrainingPoints >= PointAdjust then
            B1 := true;
        end
        else if i = 11 then
        begin
          if Character.BasePerception < TRAININGBASE then
            PointAdjust := 4
          else
            PointAdjust := ( Character.BasePerception - TRAININGBASE ) div TRAININGJUMP + 5;
          if Character.TrainingPoints >= PointAdjust then
            B1 := true;
        end
        else if i = 12 then
        begin
          if Character.BaseCharm < TRAININGBASE then
            PointAdjust := 4
          else
            PointAdjust := ( Character.BaseCharm - TRAININGBASE ) div TRAININGJUMP + 5;
          if Character.TrainingPoints >= PointAdjust then
            B1 := true;
        end
        else if i = 13 then
        begin
          if Character.BaseMysticism < TRAININGBASE then
            PointAdjust := 2
          else
            PointAdjust := ( Character.BaseMysticism - TRAININGBASE ) div TRAININGJUMP + 3;
          if Character.TrainingPoints >= PointAdjust then
            B2 := true;
        end
        else if i = 14 then
        begin
          if Character.BaseCombat < TRAININGBASE then
            PointAdjust := 2
          else
            PointAdjust := ( Character.BaseCombat - TRAININGBASE ) div TRAININGJUMP + 3;
          if Character.TrainingPoints >= PointAdjust then
            B2 := true;
        end
        else if i = 15 then
        begin
          if Character.BaseStealth < TRAININGBASE then
            PointAdjust := 2
          else
            PointAdjust := ( Character.BaseStealth - TRAININGBASE ) div TRAININGJUMP + 3;
          if Character.TrainingPoints >= PointAdjust then
            B2 := true;
        end
        else if i = 0 then
        begin
          if Character.BaseStrength < TRAININGBASE then
            PointAdjust := 4
          else
            PointAdjust := ( Character.BaseStrength - TRAININGBASE ) div TRAININGJUMP + 5;
          OneLessBack := ( Character.BaseStrength >= TRAININGBASE ) and ( ( Character.BaseStrength mod TRAININGJUMP ) = 0 );
          if StatAdjustments[ i ] > 0 then
            B3 := true;
        end
        else if i = 1 then
        begin
          if Character.BaseCoordination < TRAININGBASE then
            PointAdjust := 4
          else
            PointAdjust := ( Character.BaseCoordination - TRAININGBASE ) div TRAININGJUMP + 5;
          OneLessBack := ( Character.BaseCoordination >= TRAININGBASE ) and ( ( Character.BaseCoordination mod TRAININGJUMP ) = 0 );
          if StatAdjustments[ i ] > 0 then
            B3 := true;
        end
        else if i = 2 then
        begin
          if Character.BaseConstitution < TRAININGBASE then
            PointAdjust := 4
          else
            PointAdjust := ( Character.BaseConstitution - TRAININGBASE ) div TRAININGJUMP + 5;
          OneLessBack := ( Character.BaseConstitution >= TRAININGBASE ) and ( ( Character.BaseConstitution mod TRAININGJUMP ) = 0 );
          if StatAdjustments[ i ] > 0 then
            B3 := true;
        end
        else if i = 3 then
        begin
          if Character.BasePerception < TRAININGBASE then
            PointAdjust := 4
          else
            PointAdjust := ( Character.BasePerception - TRAININGBASE ) div TRAININGJUMP + 5;
          OneLessBack := ( Character.BasePerception >= TRAININGBASE ) and ( ( Character.BasePerception mod TRAININGJUMP ) = 0 );
          if StatAdjustments[ i ] > 0 then
            B3 := true;
        end
        else if i = 4 then
        begin
          if Character.BaseCharm < TRAININGBASE then
            PointAdjust := 4
          else
            PointAdjust := ( Character.BaseCharm - TRAININGBASE ) div TRAININGJUMP + 5;
          OneLessBack := ( Character.BaseCharm >= TRAININGBASE ) and ( ( Character.BaseCharm mod TRAININGJUMP ) = 0 );
          if StatAdjustments[ i ] > 0 then
            B3 := true;
        end
        else if i = 5 then
        begin
          if Character.BaseMysticism < TRAININGBASE then
            PointAdjust := 2
          else
            PointAdjust := ( Character.BaseMysticism - TRAININGBASE ) div TRAININGJUMP + 3;
          OneLessBack := ( Character.BaseMysticism >= TRAININGBASE ) and ( ( Character.BaseMysticism mod TRAININGJUMP ) = 0 );
          if StatAdjustments[ i ] > 0 then
            B4 := true;
        end
        else if i = 6 then
        begin
          if Character.BaseCombat < TRAININGBASE then
            PointAdjust := 2
          else
            PointAdjust := ( Character.BaseCombat - TRAININGBASE ) div TRAININGJUMP + 3;
          OneLessBack := ( Character.BaseCombat >= TRAININGBASE ) and ( ( Character.BaseCombat mod TRAININGJUMP ) = 0 );
          if StatAdjustments[ i ] > 0 then
            B4 := true;
        end
        else if i = 7 then
        begin
          if Character.BaseStealth < TRAININGBASE then
            PointAdjust := 2
          else
            PointAdjust := ( Character.BaseStealth - TRAININGBASE ) div TRAININGJUMP + 3;
          OneLessBack := ( Character.BaseStealth >= TRAININGBASE ) and ( ( Character.BaseStealth mod TRAININGJUMP ) = 0 );
          if StatAdjustments[ i ] > 0 then
            B4 := true;
        end;

        //B2:= ((i > 12) and (Character.TrainingPoints > 1));
        //B3:= ((i < 5) and (StatAdjustments[i] > 0));
        //B4:= ((i > 4) and (i < 8) and (StatAdjustments[i] > 0));

        if B1 or B2 or B3 or B4 then
        begin
          if B1 then
          begin //adjust training points, keep track of training points added
            Character.TrainingPoints := -PointAdjust;
            StatAdjustments[ i - 8 ] := StatAdjustments[ i - 8 ] + 1;
          end
          else if B2 then
          begin
            Character.TrainingPoints := -PointAdjust;
            StatAdjustments[ i - 8 ] := StatAdjustments[ i - 8 ] + 1;
          end
          else if B3 then
          begin
            if OneLessBack then
              Character.TrainingPoints := PointAdjust - 1
            else
              Character.TrainingPoints := PointAdjust;
            StatAdjustments[ i ] := StatAdjustments[ i ] - 1;
          end
          else
          begin
            if OneLessBack then
              Character.TrainingPoints := PointAdjust - 1
            else
              Character.TrainingPoints := PointAdjust;
            StatAdjustments[ i ] := StatAdjustments[ i ] - 1;
          end;
          case i of
            0 : Character.Strength := Character.BaseStrength - 1;
            1 : Character.Coordination := Character.BaseCoordination - 1;
            2 : Character.Constitution := Character.BaseConstitution - 1;
            3 : Character.Perception := Character.BasePerception - 1;
            4 : Character.Charm := Character.BaseCharm - 1;
            5 : Character.Mysticism := Character.BaseMysticism - 1;
            6 : Character.Combat := Character.BaseCombat - 1;
            7 : Character.Stealth := Character.BaseStealth - 1;
            8 : Character.Strength := Character.BaseStrength + 1;
            9 : Character.Coordination := Character.BaseCoordination + 1;
            10 : Character.Constitution := Character.BaseConstitution + 1;
            11 : Character.Perception := Character.BasePerception + 1;
            12 : Character.Charm := Character.BaseCharm + 1;
            13 : Character.Mysticism := Character.BaseMysticism + 1;
            14 : Character.Combat := Character.BaseCombat + 1;
            15 : Character.Stealth := Character.BaseStealth + 1;
          end; //case
          i := 888; //drop out of loop
        end
        else
        begin
          //buzzing noise?
          i := 999; //drop out of loop
        end; //endif b1 b2 or b3
      end;
      i := i + 1;
    end; //wend
    if i = 889 then
    begin //we hit an arrow and changed a stat
      paint;
    end
    else
    begin //we arent over anything else- check back button
      if PtinRect( rect( 581, 414, 581 + 81, 414 + 57 ), point( X, Y ) ) then
      begin //over back button
         //The new data is already saved- if we ever write a Cancel function then we can restore values
         //Exit the screen
        Close;
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TStatistics.MouseDown

procedure TStatistics.MouseMove( Sender : TAniview; Shift : TShiftState; X, Y : Integer; GridX, GridY : integer );
var
  i, j : integer;
  Info, HitPointSecMsg,
    HitPointMinMsg,
    HitPointHrMsg,
    ManaSecMsg,
    ManaMinMsg,
    ManaHrMsg : string;
  Rate : double;
const
  FailName : string = 'TStatistics.MouseMove';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

     //Clean up arrows and back to game
    lpDDSBack.BltFast( 103, 105, DXBack, Rect( 103, 105, 123, 321 ), DDBLTFAST_WAIT );
    lpDDSBack.BltFast( 188, 105, DXBack, Rect( 188, 105, 210, 315 ), DDBLTFAST_WAIT );
    lpDDSBack.BltFast( 581, 414, DXBack, Rect( 581, 414, 581 + 81, 414 + 57 ), DDBLTFAST_WAIT );
   //clear text
    lpDDSBack.BltFast( 10, 338, DXBack, Rect( 10, 338, 587, 470 ), DDBLTFAST_WAIT );
    i := 0;
    j := 0;
    while i < 16 do
    begin
      if ptInRect( ArrowRect[ i ].rect, point( X, Y ) ) then
      begin //if over an Arrow
        ArrowInfo( i );
        if i < 8 then
          lpDDSBack.BltFast( ArrowRect[ i ].rect.left, ArrowRect[ i ].rect.top, DXLeftArrow, Rect( 0, 0, 20, 15 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ) //plot the highlight
        else
          lpDDSBack.BltFast( ArrowRect[ i ].rect.left - 4, ArrowRect[ i ].rect.top, DXRightArrow, Rect( 0, 0, 20, 15 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //plot the highlight
        if UseSmallFont then
          pText.PlotTinyTextBlock( ArrowRect[ i ].Info, 10, 587, 376, 240 )
        else
          pText.PlotTextBlock( ArrowRect[ i ].Info, 10, 587, 376, 240 ); //Plot the info
        i := 900; //drop out of the loop
      end;
      i := i + 1;
    end; //wend
    if i < 900 then
    begin //if we aren't over an arrow check all other hot spots
      i := 0;
      while i < 95 do
      begin
        if not InfoRect[ i ].Disabled and ptInRect( InfoRect[ i ].rect, point( X, Y ) ) then
        begin //if over an Arrow
          if UseSmallFont then
            pText.PlotTinyTextBlock( InfoRect[ i ].Info, 10, 580, 376, 240 )
          else
            pText.PlotTextBlock( InfoRect[ i ].Info, 10, 580, 376, 240 ); //Plot the info
          j := i;
          i := 900;
        end;
        i := i + 1;
      end; //wend
    end; //endif i < 900


    ExText.Open( 'Statistics' );
    HitPointSecMsg := ExText.GetText( 'HitPointSecMsg' );
    HitPointMinMsg := ExText.GetText( 'HitPointMinMsg' );
    HitPointHrMsg := ExText.GetText( 'HitPointHrMsg' );
    ManaSecMsg := ExText.GetText( 'ManaSecMsg' );
    ManaMinMsg := ExText.GetText( 'ManaMinMsg' );
    ManaHrMsg := ExText.GetText( 'ManaHrMsg' );
    ExtExt.Close;
    if HitPointSecMsg = '' then
      HitPointSecMsg := ' hit points / sec';
    if HitPointMinMsg = '' then
      HitPointMinMsg := ' hit points / min';
    if HitPointHrMsg = '' then
      HitPointHrMsg := ' hit points / hr';
    if ManaSecMsg = '' then
      ManaSecMsg := ' mana / sec';
    if ManaMinMsg = '' then
      ManaMinMsg := ' mana / min';
    if ManaHrMsg = '' then
      ManaHrMsg := ' mana / hr';

    if j = 34 then
    begin
      Rate := 100 * ( Character.HitPoints * Character.HealingRate * Character.Constitution / 1000000 ) / 3;
      if Rate > 3 then
        Info := '* ' + inttostr( round( Rate ) ) + HitPointSecMsg
      else if Rate > 0.05 then
        Info := '* ' + inttostr( round( Rate * 60 ) ) + HitPointMinMsg
      else
        Info := '* ' + inttostr( round( Rate * 3600 ) ) + HitPointHrMsg;
      if UseSmallFont then
        pText.PlotTinyTextBlock( Info, 208, 580, 338, 240 )
      else
        pText.PlotTextBlock( Info, 208, 580, 338, 240 ); //Plot the info
    end
    else if j = 35 then
    begin
      Rate := 100 * ( Character.Mana * Character.RechargeRate * Character.Constitution / 500000 ) / 3;
      if Rate > 3 then
        Info := '* ' + inttostr( round( Rate ) ) + ManaSecMsg
      else if Rate > 0.05 then
        Info := '* ' + inttostr( round( Rate * 60 ) ) + ManaMinMsg
      else
        Info := '* ' + inttostr( round( Rate * 3600 ) ) + ManaHrMsg;
      if UseSmallFont then
        pText.PlotTinyTextBlock( Info, 208, 580, 338, 240 )
      else
        pText.PlotTextBlock( Info, 208, 580, 338, 240 ); //Plot the info
    end;

    if i <> 901 then
    begin //we arent over anything else- check back button
      if PtinRect( rect( 581, 414, 581 + 81, 414 + 57 ), point( X, Y ) ) then
      begin //over back button
         //plot highlighted back to game
        lpDDSBack.BltFast( 581, 414, DXBackToGame, Rect( 0, 0, 81, 57 ), DDBLTFAST_WAIT );
      end;
    end;

    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    lpDDSBack.BltFast( 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TStatistics.MouseMove

procedure TStatistics.MouseUp( Sender : TAniview; Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer; GridX, GridY : integer );
const
  FailName : string = 'TStatistics.Mouseup';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TStatistics.Paint;
const
  FailName : string = 'TStatistics.paint';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

  //clear the back down to the text - but dont clear the info block
    lpDDSBack.BltFast( 0, 0, DXBack, Rect( 0, 0, 677, 367 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
  //replot the entire screen statistics
    ShowStats;
    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    lpDDSBack.BltFast( 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TStatistics.Paint;

procedure TStatistics.CreateCollisionRects;
var
  i, j : integer;
  LineHeight : integer;
  a : string;
const
  FailName : string = 'TStatistics.CreateCollisonrects';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    LineHeight := 24;
   //first the ArrowRects
    for i := 0 to 7 do
    begin
      ArrowRect[ i ].rect.left := 103;
      ArrowRect[ i + 8 ].rect.left := 194;
      ArrowRect[ i ].rect.right := 118;
      ArrowRect[ i + 8 ].rect.right := 207;
      if i < 5 then
      begin //there is a break in the graphic
        ArrowRect[ i ].rect.top := 106 + i * LineHeight;
        ArrowRect[ i + 8 ].rect.top := 106 + i * LineHeight;
        ArrowRect[ i ].rect.bottom := 106 + i * LineHeight + LineHeight;
        ArrowRect[ i + 8 ].rect.bottom := 106 + i * LineHeight + LineHeight;
      end
      else
      begin
        ArrowRect[ i ].rect.top := 249 + ( i - 5 ) * LineHeight;
        ArrowRect[ i + 8 ].rect.top := 249 + ( i - 5 ) * LineHeight;
        ArrowRect[ i ].rect.bottom := 249 + ( i - 5 ) * LineHeight + LineHeight;
        ArrowRect[ i + 8 ].rect.bottom := 249 + ( i - 5 ) * LineHeight + LineHeight;
      end;
      ArrowRect[ i ].info := txtMessage[ 32 ] + StatName[ 0 ][ i + 1 ] + '.';

       //  if i < 5 then
       //     ArrowRect[i+8].info:='blah'//This arrow adds training points to '+ StatName[0][i+1]+'.  It costs '+
                                 //'4 training points to raise your '+StatName[0][i+1]+' 1 point.'
       //  else
       //     ArrowRect[i+8].info:='blah';//'This arrow adds training points to '+ StatName[0][i+1]+'.  It costs '+
                                 //'2 training points to raise your '+StatName[0][i+1]+' 1 point.';

    end; //end for

   //Training points
    InfoRect[ 0 ].rect.left := 8;
    InfoRect[ 0 ].rect.top := 42;
    InfoRect[ 0 ].rect.right := 157;
    InfoRect[ 0 ].rect.bottom := 72;
    InfoRect[ 0 ].info := txtMessage[ 33 ];
   //Primary->Stealth
    for i := 1 to 9 do
    begin
      InfoRect[ i ].rect.left := 2;
      InfoRect[ i ].rect.right := 101;
      if i < 7 then
      begin //there is a break in the graphic between the first 5 primaries and the last 3
        InfoRect[ i ].rect.top := 81 + ( i - 1 ) * LineHeight;
        InfoRect[ i ].rect.bottom := 81 + ( i - 1 ) * LineHeight + LineHeight;
      end
      else
      begin
        InfoRect[ i ].rect.top := 249 + ( i - 7 ) * LineHeight;
        InfoRect[ i ].rect.bottom := 249 + ( i - 7 ) * LineHeight + LineHeight;
      end;
    end;
    InfoRect[ 1 ].info := txtMessage[ 34 ];
    InfoRect[ 2 ].info := txtMessage[ 35 ];
    InfoRect[ 3 ].info := txtMessage[ 36 ];
    InfoRect[ 4 ].info := txtMessage[ 37 ];
    InfoRect[ 5 ].info := txtMessage[ 38 ];

    InfoRect[ 6 ].info := txtMessage[ 39 ];
    InfoRect[ 7 ].info := txtMessage[ 40 ];
    InfoRect[ 8 ].info := txtMessage[ 41 ];
    InfoRect[ 9 ].info := txtMessage[ 42 ];


   //Base and adjusted columns
    i := 10;
    InfoRect[ 10 ].info := txtMessage[ 43 ];
    InfoRect[ 19 ].info := txtMessage[ 44 ];
    for j := 0 to 8 do
    begin
      InfoRect[ i ].rect.left := 118;
      InfoRect[ i ].rect.right := 157;
      InfoRect[ i + 9 ].rect.left := 157; //adj
      InfoRect[ i + 9 ].rect.right := 193; //adj
      if j < 6 then
      begin //there is a break in the graphic between the first 5 primaries and the last 3
        InfoRect[ i ].rect.top := 81 + j * LineHeight;
        InfoRect[ i ].rect.bottom := 81 + j * LineHeight + LineHeight;
        InfoRect[ i + 9 ].rect.top := 81 + j * LineHeight;
        InfoRect[ i + 9 ].rect.bottom := 81 + j * LineHeight + LineHeight;
      end
      else
      begin
        InfoRect[ i ].rect.top := 249 + ( j - 6 ) * LineHeight;
        InfoRect[ i ].rect.bottom := 249 + ( j - 6 ) * LineHeight + LineHeight;
        InfoRect[ i + 9 ].rect.top := 249 + ( j - 6 ) * LineHeight;
        InfoRect[ i + 9 ].rect.bottom := 249 + ( j - 6 ) * LineHeight + LineHeight;
      end;
      if j > 0 then
      begin
        InfoRect[ i ].info := InfoRect[ 10 ].info + '  ' + InfoRect[ j + 1 ].info; //add the base and stat string together
        InfoRect[ i + 9 ].info := InfoRect[ 19 ].info + '  ' + InfoRect[ j + 1 ].info; //add the adj and stat string together
      end;
      i := i + 1;
    end;
    i := i + 9; //add 9 more -we did the adjusted as well
   //Secondary column
    InfoRect[ i ].info := txtMessage[ 45 ];
   //start new
    InfoRect[ i + 1 ].info := txtMessage[ 46 ];
    InfoRect[ i + 2 ].info := txtMessage[ 47 ];
    InfoRect[ i + 3 ].info := txtMessage[ 48 ];
   //end new
    InfoRect[ i + 4 ].info := txtMessage[ 49 ];
    InfoRect[ i + 5 ].info := txtMessage[ 50 ];
    InfoRect[ i + 6 ].info := txtMessage[ 51 ];
    InfoRect[ i + 7 ].info := txtMessage[ 52 ];
    InfoRect[ i + 8 ].info := txtMessage[ 53 ];
    InfoRect[ i + 9 ].info := txtMessage[ 54 ];
    InfoRect[ i + 10 ].info := ''; //fix because we cut recovery

    for j := 0 to 10 do
    begin //7 do begin
      InfoRect[ i ].rect.left := 208;
      InfoRect[ i ].rect.right := 352;
      InfoRect[ i ].rect.top := 81 + j * LineHeight;
      InfoRect[ i ].rect.bottom := 81 + j * LineHeight + LineHeight;
      i := i + 1;
    end;
    InfoRect[ i ].info := txtMessage[ 55 ];
   //resistance column
    for j := 0 to 10 do
    begin
      InfoRect[ i ].rect.left := 353;
      InfoRect[ i ].rect.right := 439;
      InfoRect[ i ].rect.top := 81 + j * LineHeight;
      InfoRect[ i ].rect.bottom := 81 + j * LineHeight + LineHeight;
      if j > 0 then
        InfoRect[ i ].info := txtMessage[ 56 ] + StatName[ 1 ][ j ] + txtMessage[ 57 ] + StatName[ 1 ][ j ] + txtMessage[ 58 ] + StatName[ 1 ][ j ] + txtMessage[ 59 ];

      InfoRect[ i ].Disabled := ( j >= 9 );
      i := i + 1;
    end;
   //Invincibility and Resistance
    InfoRect[ i ].info := txtMessage[ 60 ];
    InfoRect[ i + 11 ].info := txtMessage[ 61 ];
    for j := 0 to 10 do
    begin
      InfoRect[ i ].rect.left := 439;
      InfoRect[ i ].rect.right := 478;
      InfoRect[ i + 11 ].rect.left := 478; //res
      InfoRect[ i + 11 ].rect.right := 517; //res
      InfoRect[ i ].rect.top := 81 + j * LineHeight;
      InfoRect[ i ].rect.bottom := 81 + j * LineHeight + LineHeight;
      InfoRect[ i + 11 ].rect.top := 81 + j * LineHeight;
      InfoRect[ i + 11 ].rect.bottom := 81 + j * LineHeight + LineHeight;
     {  if j > 0 then begin
             InfoRect[i].info:='The Invincibility modifier represents how much damage of a particular type '+
                     'a character repels during an attack.  This character would resist X points of '+
                     'a '+ StatName[1][j]+ ' attack.';
             InfoRect[i+11].info:='The Resistance modifier represents what percentage of damage of a particular type '+
                     'a character repels during an attack.  This character would resist X percent of '+
                     'a '+ StatName[1][j]+ ' attack.';

       end;  }
      i := i + 1;
    end;
    i := i + 11; //add 11 more -we did the resistance as well
   //finally the damage  column
    InfoRect[ i ].info := txtMessage[ 62 ];

    for j := 0 to 11 do
    begin
      InfoRect[ i ].rect.left := 518;
      InfoRect[ i ].rect.right := 588;
      InfoRect[ i ].rect.top := 81 + j * LineHeight;
      InfoRect[ i ].rect.bottom := 81 + j * LineHeight + LineHeight;
      if j > 0 then
        InfoRect[ i ].info := txtMessage[ 63 ] + StatName[ 1 ][ j ] + txtMessage[ 64 ];
      InfoRect[ i ].Disabled := ( j >= 9 );
      i := i + 1;
    end;
   //now the damage numbers column
    for j := 0 to 10 do
    begin
      InfoRect[ i ].rect.left := 588;
      InfoRect[ i ].rect.right := 646;
      InfoRect[ i ].rect.top := 105 + j * LineHeight;
      InfoRect[ i ].rect.bottom := 105 + j * LineHeight + LineHeight;
       //InfoRect[i].info:='This character will do X-X points of ' + StatName[1][j+1]+
       //                  ' damage when attacking.';
      i := i + 1;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TStatistics.CreateCollisionRects;

{procedure TStatistics.DebugPlot(i: integer);
var
 a : string;
const
  FailName: string = 'TStatistics.Debugplot';
begin
{$IFDEF DODEBUG}
{  if (CurrDbgLvl >= DbgLvlSevere) then
    Log.LogEntry(FailName);
{$ENDIF}
{try

lpDDSBack.BltFast(20,237,DXBack,Rect(20,237,20+50,237+25),DDBLTFAST_WAIT); //clean up before we plot text
str(i,a);
pText.PlotText(a,20,237,0);
except
   on E: Exception do Log.log(FailName+E.Message);
end;

end;  }

procedure TStatistics.LoadNames;
const
  FailName : string = 'TStatistics.LoadNames';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

  //Loads all the names we use in the Mouseover help
    StatName[ 0 ][ 1 ] := txtMessage[ 0 ]; //'Strength';
    StatName[ 0 ][ 2 ] := txtMessage[ 1 ]; //'Coordination';
    StatName[ 0 ][ 3 ] := txtMessage[ 2 ]; //'Constitution';
    StatName[ 0 ][ 4 ] := txtMessage[ 3 ]; //'Perception';
    StatName[ 0 ][ 5 ] := txtMessage[ 4 ]; //'Charm';
    StatName[ 0 ][ 6 ] := txtMessage[ 5 ]; //'Mysticism';
    StatName[ 0 ][ 7 ] := txtMessage[ 6 ]; //'Combat';
    StatName[ 0 ][ 8 ] := txtMessage[ 7 ]; //'Stealth';

    StatName[ 1 ][ 1 ] := txtMessage[ 8 ]; //'Piercing';
    StatName[ 1 ][ 2 ] := txtMessage[ 9 ]; //'Crushing';
    StatName[ 1 ][ 3 ] := txtMessage[ 10 ]; //'Cutting';
    StatName[ 1 ][ 4 ] := txtMessage[ 11 ]; //'Heat';
    StatName[ 1 ][ 5 ] := txtMessage[ 12 ]; //'Cold';
    StatName[ 1 ][ 6 ] := txtMessage[ 13 ]; //'Electric';
    StatName[ 1 ][ 7 ] := txtMessage[ 15 ]; //'Poison'; //Now Magic
    StatName[ 1 ][ 8 ] := txtMessage[ 17 ]; //'Magic';  //Now Stun
//  StatName[1][9]:=txtMessage[16];//'Mental';
//  StatName[1][10]:=txtMessage[17];//'Stun';
//  StatName[1][11]:=txtMessage[18];//'Special';
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TStatistics.LoadBaseValues;
var
  i : integer;
const
  FailName : string = 'TStatistics.LoadBaseValues';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
  //we store thse values so that we can keep the player from lowering his score beyon its start
    Damage := Character.Damage;
    Resistance := Character.Resistance;
    BaseStrength := Character.BaseStrength;
    BaseCoordination := Character.BaseCoordination;
    BaseConstitution := Character.BaseConstitution;
    BasePerception := Character.BasePerception;
    BaseCharm := Character.BaseCharm;
    BaseMysticism := Character.BaseMysticism;
    BaseCombat := Character.BaseCombat;
    BaseStealth := Character.BaseStealth;
    TrainingPoints := Character.TrainingPoints;

    for i := 0 to 7 do
    begin //initialize adjustments to zero
      StatAdjustments[ i ] := 0;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TStatistics.LoadBaseValues;

procedure TStatistics.ShowStats;
var
  a, b : string;
  i, Alpha : integer;
  ix : integer;
  x1, x2, x3, x4 : integer;
const
  FailName : string = 'TStatistics.Showstats';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    Alpha := 240; //blend value
    pText.PlotTextCentered( Character.Name, 120, 640, 8, Alpha );
    str( Character.TrainingPoints, a );
    pText.PlotText( a, 130, 46, Alpha );
   //primary stats column
    x1 := 118;
    x2 := 156;
    x3 := x2;
    x4 := 194;
    i := 104;
    str( Character.BaseStrength, a );
    str( Character.Strength, b );
    if StatAdjustments[ 0 ] < 1 then
    begin
      pText.PlotTextCentered( a, x1, x2, i, Alpha );
      pText.PlotTextCentered( b, x3, x4, i, Alpha );
    end
    else
    begin //darken it up
      pText.PlotDarkTextCentered( a, x1, x2, i, 240 );
      pText.PlotDarkTextCentered( b, x3, x4, i, 240 );
    end;
    i := i + 24;
    str( Character.BaseCoordination, a );
    str( Character.Coordination, b );
    if StatAdjustments[ 1 ] < 1 then
    begin
      pText.PlotTextCentered( a, x1, x2, i, Alpha );
      pText.PlotTextCentered( b, x3, x4, i, Alpha );
    end
    else
    begin //darken it up
      pText.PlotDarkTextCentered( a, x1, x2, i, 240 );
      pText.PlotDarkTextCentered( b, x3, x4, i, 240 );
    end;
    i := i + 24;
    str( Character.BaseConstitution, a );
    str( Character.Constitution, b );
    if StatAdjustments[ 2 ] < 1 then
    begin
      pText.PlotTextCentered( a, x1, x2, i, Alpha );
      pText.PlotTextCentered( b, x3, x4, i, Alpha );
    end
    else
    begin //darken it up
      pText.PlotDarkTextCentered( a, x1, x2, i, 240 );
      pText.PlotDarkTextCentered( b, x3, x4, i, 240 );
    end;
    i := i + 24;
    str( Character.BasePerception, a );
    str( Character.Perception, b );
    if StatAdjustments[ 3 ] < 1 then
    begin
      pText.PlotTextCentered( a, x1, x2, i, Alpha );
      pText.PlotTextCentered( b, x3, x4, i, Alpha );
    end
    else
    begin //darken it up
      pText.PlotDarkTextCentered( a, x1, x2, i, 240 );
      pText.PlotDarkTextCentered( b, x3, x4, i, 240 );
    end;
    i := i + 24;
    str( Character.BaseCharm, a );
    str( Character.Charm, b );
    if StatAdjustments[ 4 ] < 1 then
    begin
      pText.PlotTextCentered( a, x1, x2, i, Alpha );
      pText.PlotTextCentered( b, x3, x4, i, Alpha );
    end
    else
    begin //darken it up
      pText.PlotDarkTextCentered( a, x1, x2, i, 240 );
      pText.PlotDarkTextCentered( b, x3, x4, i, 240 );
    end;
    i := 248;
    str( Character.BaseMysticism, a );
    str( Character.Mysticism, b );
    if StatAdjustments[ 5 ] < 1 then
    begin
      pText.PlotTextCentered( a, x1, x2, i, Alpha );
      pText.PlotTextCentered( b, x3, x4, i, Alpha );
    end
    else
    begin //darken it up
      pText.PlotDarkTextCentered( a, x1, x2, i, 240 );
      pText.PlotDarkTextCentered( b, x3, x4, i, 240 );
    end;
    i := i + 24;
    str( Character.BaseCombat, a );
    str( Character.Combat, b );
    if StatAdjustments[ 6 ] < 1 then
    begin
      pText.PlotTextCentered( a, x1, x2, i, Alpha );
      pText.PlotTextCentered( b, x3, x4, i, Alpha );
    end
    else
    begin //darken it up
      pText.PlotDarkTextCentered( a, x1, x2, i, 240 );
      pText.PlotDarkTextCentered( b, x3, x4, i, 240 );
    end;
    i := i + 24;
    str( Character.BaseStealth, a );
    str( Character.Stealth, b );
    if StatAdjustments[ 7 ] < 1 then
    begin
      pText.PlotTextCentered( a, x1, x2, i, Alpha );
      pText.PlotTextCentered( b, x3, x4, i, Alpha );
    end
    else
    begin //darken it up
      pText.PlotDarkTextCentered( a, x1, x2, i, 240 );
      pText.PlotDarkTextCentered( b, x3, x4, i, 240 );
    end;


   //Secondary stats column
    x1 := 315;
    x2 := 352;
    i := 104;
   //New
    str( round( Character.AttackBonus ), a );
    pText.PlotTextCentered( a, x1, x2, i, Alpha );
    i := i + 24;
    str( round( Character.Defense ), a );
    pText.PlotTextCentered( a, x1, x2, i, Alpha );
    i := i + 24;
    str( Character.AttackRecovery, a );
    pText.PlotTextCentered( a, x1, x2, i, Alpha );
    i := i + 24;
   //end new
    str( Character.restriction, a );
    pText.PlotTextCentered( a, x1, x2, i, Alpha );
    i := i + 24;
//   str(round(Character.movement),a);
    str( round( TCharacterResource( Character.resource ).speed ), a );
    pText.PlotTextCentered( a, x1, x2, i, Alpha );
    i := i + 24;
   //str(Character.recovery,a);
   //pText.PlotText(a),327,i,Alpha);
   //i:=i+24;
//   str(Character.healingrate,a);
//   str(round(Character.HitPoints * Character.HealingRate * Character.Constitution / 1000),a);
    a := '*';
    pText.PlotTextCentered( a, x1, x2, i, Alpha );
    i := i + 24;
//   str(Character.rechargerate,a);
//   str(round(Character.Mana * Character.RechargeRate * Character.Constitution / 200),a);
    a := '*';
    pText.PlotTextCentered( a, x1, x2, i, Alpha );
    i := i + 24;
    str( round( Character.hitpoints ), a );
    pText.PlotTextCentered( a, x1, x2, i, Alpha );
    i := i + 24;
    str( round( Character.mana ), a );
    pText.PlotTextCentered( a, x1, x2, i, Alpha );

   //Resistance column
    x1 := 439;
    x2 := 478;
    x3 := x2;
    x4 := 517;
    i := 104;
    ix := 3; //ix is the number of new items we inserted before these
    str( Round( Character.resistance.Piercing.Invulnerability ), a );
    pText.PlotTextCentered( a, x1, x2, i, Alpha );
    str( Round( Character.resistance.Piercing.Resistance * 100 ), b );
    pText.PlotTextCentered( b, x3, x4, i, Alpha );
    InfoRect[ ix + 48 ].info := txtMessage[ 19 ] + a + txtMessage[ 20 ] + StatName[ 1 ][ 1 ] + txtMessage[ 21 ];
    InfoRect[ ix + 48 + 11 ].info := txtMessage[ 22 ] + b + txtMessage[ 23 ] + StatName[ 1 ][ 1 ] + txtMessage[ 21 ];

    i := i + 24;
    str( Round( Character.resistance.crushing.Invulnerability ), a );
    pText.PlotTextCentered( a, x1, x2, i, Alpha );
    str( Round( Character.resistance.crushing.Resistance * 100 ), b );
    pText.PlotTextCentered( b, x3, x4, i, Alpha );
    InfoRect[ ix + 49 ].info := txtMessage[ 19 ] + a + txtMessage[ 20 ] + StatName[ 1 ][ 2 ] + txtMessage[ 21 ];
    InfoRect[ ix + 49 + 11 ].info := txtMessage[ 22 ] + b + txtMessage[ 23 ] + StatName[ 1 ][ 2 ] + txtMessage[ 21 ];

    i := i + 24;
    str( Round( Character.resistance.cutting.Invulnerability ), a );
    pText.PlotTextCentered( a, x1, x2, i, Alpha );
    str( Round( Character.resistance.cutting.Resistance * 100 ), b );
    pText.PlotTextCentered( b, x3, x4, i, Alpha );
    InfoRect[ ix + 50 ].info := txtMessage[ 19 ] + a + txtMessage[ 20 ] + StatName[ 1 ][ 3 ] + txtMessage[ 21 ];
    InfoRect[ ix + 50 + 11 ].info := txtMessage[ 22 ] + b + txtMessage[ 23 ] + StatName[ 1 ][ 3 ] + txtMessage[ 21 ];

    i := i + 24;
    str( Round( Character.resistance.heat.Invulnerability ), a );
    pText.PlotTextCentered( a, x1, x2, i, Alpha );
    str( Round( Character.resistance.heat.Resistance * 100 ), b );
    pText.PlotTextCentered( b, x3, x4, i, Alpha );
    InfoRect[ ix + 51 ].info := txtMessage[ 19 ] + a + txtMessage[ 20 ] + StatName[ 1 ][ 4 ] + txtMessage[ 21 ];
    InfoRect[ ix + 51 + 11 ].info := txtMessage[ 22 ] + b + txtMessage[ 23 ] + StatName[ 1 ][ 4 ] + txtMessage[ 21 ];
    i := i + 24;
    str( Round( Character.resistance.cold.Invulnerability ), a );
    pText.PlotTextCentered( a, x1, x2, i, Alpha );
    str( Round( Character.resistance.cold.Resistance * 100 ), b );
    pText.PlotTextCentered( b, x3, x4, i, Alpha );
    InfoRect[ ix + 52 ].info := txtMessage[ 19 ] + a + txtMessage[ 20 ] + StatName[ 1 ][ 5 ] + txtMessage[ 21 ];
    InfoRect[ ix + 52 + 11 ].info := txtMessage[ 22 ] + b + txtMessage[ 23 ] + StatName[ 1 ][ 5 ] + txtMessage[ 21 ];
    i := i + 24;
    str( Round( Character.resistance.electric.Invulnerability ), a );
    pText.PlotTextCentered( a, x1, x2, i, Alpha );
    str( Round( Character.resistance.electric.Resistance * 100 ), b );
    pText.PlotTextCentered( b, x3, x4, i, Alpha );
    InfoRect[ ix + 53 ].info := txtMessage[ 19 ] + a + txtMessage[ 20 ] + StatName[ 1 ][ 6 ] + txtMessage[ 21 ];
    InfoRect[ ix + 53 + 11 ].info := txtMessage[ 22 ] + b + txtMessage[ 23 ] + StatName[ 1 ][ 6 ] + txtMessage[ 21 ];

    i := i + 24;
    str( Round( Character.resistance.magic.Invulnerability ), a );
    pText.PlotTextCentered( a, x1, x2, i, Alpha );
    str( Round( Character.resistance.magic.Resistance * 100 ), b );
    pText.PlotTextCentered( b, x3, x4, i, Alpha );
    InfoRect[ ix + 54 ].info := txtMessage[ 19 ] + a + txtMessage[ 20 ] + StatName[ 1 ][ 7 ] + txtMessage[ 21 ];
    InfoRect[ ix + 54 + 11 ].info := txtMessage[ 22 ] + b + txtMessage[ 23 ] + StatName[ 1 ][ 7 ] + txtMessage[ 21 ];

    i := i + 24;
    str( Round( Character.resistance.stun.Invulnerability ), a );
    pText.PlotTextCentered( a, x1, x2, i, Alpha );
    str( Round( Character.resistance.stun.Resistance * 100 ), b );
    pText.PlotTextCentered( b, x3, x4, i, Alpha );
    InfoRect[ ix + 55 ].info := txtMessage[ 19 ] + a + txtMessage[ 20 ] + StatName[ 1 ][ 8 ] + txtMessage[ 21 ];
    InfoRect[ ix + 55 + 11 ].info := txtMessage[ 22 ] + b + txtMessage[ 23 ] + StatName[ 1 ][ 8 ] + txtMessage[ 21 ];

    InfoRect[ ix + 56 ].Disabled := true;
    InfoRect[ ix + 56 + 11 ].Disabled := true;
    InfoRect[ ix + 57 ].Disabled := true;
    InfoRect[ ix + 57 + 11 ].Disabled := true;
{   i:=i+24;
   str(Round(Character.resistance.mental.Invulnerability),a);
   pText.PlotText(a,451,i,Alpha);
   str(Round(Character.resistance.mental.Resistance*100),b);
   pText.PlotText(b,490,i,Alpha);
   InfoRect[ix+56].info:=txtMessage[19]+ a +txtMessage[20]+ StatName[1][9]+ txtMessage[21];
   InfoRect[ix+56+11].info:=txtMessage[22]+ a +txtMessage[23]+ StatName[1][9]+ txtMessage[21];

   i:=i+24;
   str(Round(Character.resistance.stun.Invulnerability),a);
   pText.PlotText(a,451,i,Alpha);
   str(Round(Character.resistance.stun.Resistance*100),b);
   pText.PlotText(b,490,i,Alpha);
   InfoRect[ix+57].info:=txtMessage[19]+ a +txtMessage[20]+ StatName[1][10]+ txtMessage[21];
   InfoRect[ix+57+11].info:=txtMessage[22]+ a +txtMessage[23]+ StatName[1][10]+ txtMessage[21];  }


   //Damage column
    x1 := 588;
    x2 := 646;
    i := 104;
    str( Round( Character.damage.Piercing.Min ), a );
    str( Round( Character.damage.Piercing.Max ), b );
    pText.PlotTextCentered( a + '-' + b, x1, x2, i, Alpha );
    InfoRect[ ix + 81 ].info := txtMessage[ 24 ] + a + txtMessage[ 25 ] + b + txtMessage[ 26 ] + StatName[ 1 ][ 1 ] +
      txtMessage[ 27 ];
    i := i + 24;
    str( Round( Character.damage.crushing.Min ), a );
    str( Round( Character.damage.crushing.Max ), b );
    pText.PlotTextCentered( a + '-' + b, x1, x2, i, Alpha );
    InfoRect[ ix + 82 ].info := txtMessage[ 24 ] + a + txtMessage[ 25 ] + b + txtMessage[ 26 ] + StatName[ 1 ][ 2 ] +
      txtMessage[ 27 ];
    i := i + 24;
    str( Round( Character.damage.cutting.Min ), a );
    str( Round( Character.damage.cutting.Max ), b );
    pText.PlotTextCentered( a + '-' + b, x1, x2, i, Alpha );
    InfoRect[ ix + 83 ].info := txtMessage[ 24 ] + a + txtMessage[ 25 ] + b + txtMessage[ 26 ] + StatName[ 1 ][ 3 ] +
      txtMessage[ 27 ];
    i := i + 24;
    str( Round( Character.damage.heat.Min ), a );
    str( Round( Character.damage.heat.Max ), b );
    pText.PlotTextCentered( a + '-' + b, x1, x2, i, Alpha );
    InfoRect[ ix + 84 ].info := txtMessage[ 24 ] + a + txtMessage[ 25 ] + b + txtMessage[ 26 ] + StatName[ 1 ][ 4 ] +
      txtMessage[ 27 ];
    i := i + 24;
    str( Round( Character.damage.cold.Min ), a );
    str( Round( Character.damage.cold.Max ), b );
    pText.PlotTextCentered( a + '-' + b, x1, x2, i, Alpha );
    InfoRect[ ix + 85 ].info := txtMessage[ 24 ] + a + txtMessage[ 25 ] + b + txtMessage[ 26 ] + StatName[ 1 ][ 5 ] +
      txtMessage[ 27 ];
    i := i + 24;
    str( Round( Character.damage.electric.Min ), a );
    str( Round( Character.damage.electric.Max ), b );
    pText.PlotTextCentered( a + '-' + b, x1, x2, i, Alpha );
    InfoRect[ ix + 86 ].info := txtMessage[ 24 ] + a + txtMessage[ 25 ] + b + txtMessage[ 26 ] + StatName[ 1 ][ 6 ] +
      txtMessage[ 27 ];
    i := i + 24;
    str( Round( Character.damage.magic.Min ), a );
    str( Round( Character.damage.magic.Max ), b );
    pText.PlotTextCentered( a + '-' + b, x1, x2, i, Alpha );
    InfoRect[ ix + 87 ].info := txtMessage[ 24 ] + a + txtMessage[ 25 ] + b + txtMessage[ 26 ] + StatName[ 1 ][ 7 ] +
      txtMessage[ 27 ];
    i := i + 24;
    str( Round( Character.damage.stun.Min ), a );
    str( Round( Character.damage.stun.Max ), b );
    pText.PlotTextCentered( a + '-' + b, x1, x2, i, Alpha );
    InfoRect[ ix + 88 ].info := txtMessage[ 24 ] + a + txtMessage[ 25 ] + b + txtMessage[ 26 ] + StatName[ 1 ][ 8 ] +
      txtMessage[ 27 ];

    InfoRect[ ix + 89 ].Disabled := true;
    InfoRect[ ix + 90 ].Disabled := true;
    InfoRect[ ix + 91 ].Disabled := true;
{   i:=i+24;
   str(Round(Character.damage.mental.Min),a);
   str(Round(Character.damage.mental.Max),b);
   pText.PlotText(a+'-'+b,600,i,Alpha);
   InfoRect[ix+89].info:=txtMessage[24]+ a + txtMessage[25] + b + txtMessage[26] + StatName[1][9]+
                      txtMessage[27];
   i:=i+24;
   str(Round(Character.damage.stun.Min),a);
   str(Round(Character.damage.stun.Max),b);
   pText.PlotText(a+'-'+b,600,i,Alpha);
   InfoRect[ix+90].info:=txtMessage[24]+ a + txtMessage[25] + b + txtMessage[26] + StatName[1][10]+
                      txtMessage[27];
   i:=i+24;
   str(Round(Character.damage.special.Min),a);
   str(Round(Character.damage.special.Max),b);
   pText.PlotText(a+'-'+b,600,i,Alpha);
   InfoRect[ix+91].info:=txtMessage[24]+ a + txtMessage[25] + b + txtMessage[26] + StatName[1][11]+
                      txtMessage[27];  }
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;


procedure TStatistics.ArrowInfo( i : integer );
var
  PointAdjust : integer;

begin

  PointAdjust := 0;

  if ( i > 7 ) and ( i < 16 ) then
  begin
    if i = 8 then
    begin
      if Character.BaseStrength < TRAININGBASE then
        PointAdjust := 4
      else
        PointAdjust := ( Character.BaseStrength - TRAININGBASE ) div TRAININGJUMP + 5;
    end
    else if i = 9 then
    begin
      if Character.BaseCoordination < TRAININGBASE then
        PointAdjust := 4
      else
        PointAdjust := ( Character.BaseCoordination - TRAININGBASE ) div TRAININGJUMP + 5;
    end
    else if i = 10 then
    begin
      if Character.BaseConstitution < TRAININGBASE then
        PointAdjust := 4
      else
        PointAdjust := ( Character.BaseConstitution - TRAININGBASE ) div TRAININGJUMP + 5;
    end
    else if i = 11 then
    begin
      if Character.BasePerception < TRAININGBASE then
        PointAdjust := 4
      else
        PointAdjust := ( Character.BasePerception - TRAININGBASE ) div TRAININGJUMP + 5;
    end
    else if i = 12 then
    begin
      if Character.BaseCharm < TRAININGBASE then
        PointAdjust := 4
      else
        PointAdjust := ( Character.BaseCharm - TRAININGBASE ) div TRAININGJUMP + 5;
    end
    else if i = 13 then
    begin
      if Character.BaseMysticism < TRAININGBASE then
        PointAdjust := 2
      else
        PointAdjust := ( Character.BaseMysticism - TRAININGBASE ) div TRAININGJUMP + 3;
    end
    else if i = 14 then
    begin
      if Character.BaseCombat < TRAININGBASE then
        PointAdjust := 2
      else
        PointAdjust := ( Character.BaseCombat - TRAININGBASE ) div TRAININGJUMP + 3;
    end
    else if i = 15 then
    begin
      if Character.BaseStealth < TRAININGBASE then
        PointAdjust := 2
      else
        PointAdjust := ( Character.BaseStealth - TRAININGBASE ) div TRAININGJUMP + 3;
    end;

    ArrowRect[ i ].info := txtMessage[ 28 ] + StatName[ 0 ][ i - 8 + 1 ] + txtMessage[ 29 ] +
      intToStr( PointAdjust ) + txtMessage[ 30 ] + StatName[ 0 ][ i - 8 + 1 ] + txtMessage[ 31 ];
  end; //endif

end; //TStartisticsArrowinfo

end.
