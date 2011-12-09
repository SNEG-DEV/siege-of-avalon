unit CharCreation;
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
  Resource,
  StdCtrls,
  GameText,
  Display,
  Anigrp30,
  Engine,
  Logfile;
  
type
  InformationRect = record
    rect : TRect;
    info : string;
  end;
  SelectableRect = record
    rect : TRect;
    info : string;
    text : string;
  end;
  TCreation = class( TDisplay )
  private
    FOnDraw : TNotifyEvent;
    //Training modifications
    ChosenTraining : integer;
    //Line editor stuff
    CharacterName : string; //characters name
    CaratPosition : integer; //position in pixels
    CaratCharPosition : integer; //position in Characters
    CaratVisible : boolean;
    //Bitmap stuff
    BMBack : TBitmap; //The inventory screen bitmap used for loading
    DXBack : IDirectDrawSurface; //DD surface that holds the statistics screen before blit
    DXCircle : IDirectDrawSurface; //circle used for outline
    //DXRightArrow:  IDirectDrawSurface;
    //DXLeftArrow:  IDirectDrawSurface;
    DXBox : IDirectDrawSurface;
    DXBlack : IDirectDrawSurface;
    DXContinue : IDirectDrawSurface;
    DXCancel : IDirectDrawSurface;
    InfoRect : array[ 0..17 ] of InformationRect; //was 35  //collision rects for information
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
    Modifier : integer; //Horizontal shift
    CaratTimer : TTimer;
    BoxOpen : integer;
    LoopCounter : integer;
    txtMessage : array[ 0..104 ] of string;
    procedure DrawTheGuy;
    procedure OpenBox( box : integer );
    procedure CaratTimerEvent( Sender : TObject );
    procedure LoadBaseValues; //saves the base stats of the character
    procedure LoadNames;
    procedure CreateCollisionRects; //create the rects for the collision detection
    procedure ShowStats; //plots all the numbers on the screen
//    procedure DebugPlot(i: integer);
    procedure FormMouseDown( Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer );
    procedure FormMouseMove( Sender : TObject; Shift : TShiftState; X, Y : Integer );
  protected
    procedure MouseDown( Sender : TAniview; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
    procedure MouseMove( Sender : TAniview;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
    procedure MouseUp( Sender : TAniview; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
    procedure KeyDown( Sender : TObject; var key : Word; Shift : TShiftState ); override;
  public

    ixSelectedShirt : integer; //current selected shirt color
    ixSelectedPants : integer; //current selected pants color
    ixSelectedHair : integer; //current selected Hair color
    ixSelectedHairStyle : integer; //current selected Hairstyle
    ixSelectedBeard : integer;

    SelectedTraining : integer;
    SelectedShirt : TItem; //current selected shirt color
    SelectedPants : TItem; //current selected pants color
    SelectedHair : TResource; //current selected Hair color
    SelectRect : array[ 0..20 ] of SelectableRect; //collision rects for selectable text

    shirt : array[ 1..4 ] of TItem;
    pants : array[ 1..4 ] of TItem;
    hair : array[ 1..4, 1..4, 1..2 ] of TResource;

    Character : Tcharacter;
    Cancel : boolean; //was Cancel Pressed?
    frmMain : TForm; //we need the  form passed into handle form mouse events
    chaContinueRect : TRect;
    chaCancelRect : TRect;
    property OnDraw : TNotifyEvent read FOnDraw write FOnDraw;
    constructor Create;
    destructor Destroy; override;
    procedure Paint; override;
    procedure Init; override;
    procedure Release; override;
  end;
implementation
uses
  AniDemo;
{ TCreation }

constructor TCreation.Create;
const
  FailName : string = 'TCreation.create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
    chaContinueRect := Rect( 400, 449, 0, 0 ); // left, top, right, bottom
    chaCancelRect := Rect( 100, 449, 0, 0 );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

destructor TCreation.Destroy;
const
  FailName : string = 'TCreation.Destroy';
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


procedure TCreation.Init;
var
  InvisColor : Integer;
  i : integer;
const
  FailName : string = 'TCreation.init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if Loaded then
      Exit;
    inherited;

    ExText.Open( 'CharCreation' );
    for i := 0 to 104 do
      txtMessage[ i ] := ExText.GetText( 'Message' + inttostr( i ) );

    ChosenTraining := -1; //initialize training to nothing
  //Set mouse events for form
    frmMain.OnMouseDown := FormMouseDown;
    frmMain.OnMouseMove := FormMouseMove;

    Cancel := false;
    BoxOpen := -1;
    CaratTimer := TTimer.create( nil );
    CaratTimer.onTimer := CaratTimerEvent;
//  CaratTimer.TimerPriority:=tpNormal;
    CaratTimer.Interval := 100;
//  CaratTimer.resolution := 1;
    LoopCounter := 0;
    CaratTimer.enabled := True;
    CaratPosition := 0;
    CaratCharPosition := 0;
    CaratVisible := true;
    CharacterName := '';
    Modifier := 270; //offset to move all the stats info- makes it easier to adjust
    ixSelectedShirt := 0;
    ixSelectedPants := 4;
    ixSelectedHair := 8;
    ixSelectedHairStyle := 12;
    ixSelectedBeard := 17;
    SelectedTraining := 18;
    if UseSmallFont then
      pText.LoadGoldFontGraphic;
    pText.LoadFontGraphic( 'CreateChar' ); //load the statisctics font graphic in
    LoadNames;
    CreateCollisionRects;
    LoadBaseValues;
    BMBack := TBitmap.Create;
  //BMTemp := TBitmap.Create;
  //transparent color
    InvisColor := $00FFFF00;
  //Load the Background Bitmap and plot it

    BMBack.LoadFromFile( InterfacePath + 'chaRedOval.bmp' );
    DXCircle := DDGetImage( lpDD, BMBack, InvisColor, False );
    BMBack.LoadFromFile( InterfacePath + 'chaBlack.bmp' );
    DXBlack := DDGetImage( lpDD, BMBack, InvisColor, False );
    BMBack.LoadFromFile( InterfacePath + 'chaChooseBox.bmp' );
    DXBox := DDGetImage( lpDD, BMBack, InvisColor, False );
    BMBack.LoadFromFile( InterfacePath + 'chaContinue.bmp' );
    chaContinueRect.Right := BMBack.width;
    chaContinueRect.Bottom := BMBack.height;
    DXContinue := DDGetImage( lpDD, BMBack, InvisColor, False );
    BMBack.LoadFromFile( InterfacePath + 'chaCancel.bmp' );
    chaCancelRect.Right := BMBack.width;
    chaCancelRect.Bottom := BMBack.height;
    DXCancel := DDGetImage( lpDD, BMBack, InvisColor, False );
    BMBack.LoadFromFile( InterfacePath + 'CharCreate.bmp' );
    DXBack := DDGetImage( lpDD, BMBack, InvisColor, False );


    WrapperBltFast( lpDDSBack, 0, 0, DXBack, Rect( 0, 0, BMBack.width, BMBack.Height ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );


  //release the bitmap
    BMBack.Free;
    ShowStats;
    DrawTheGuy;
  //Whew! Now we flip it all to the screen
//  lpDDSFront.Flip(nil, DDFLIP_WAIT);
//  WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect(0, 0, 800, 600), DDBLTFAST_WAIT);

  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TCreation.Init;

procedure TCreation.Release;
const
  FailName : string = 'TCreation.Release';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    pText.UnLoadGoldFontGraphic;

    ExText.close;

    DXBox := nil;
    DXBlack := nil;
    DXContinue := nil;
    DXCancel := nil;
    DXBack := nil;
    DXCircle := nil;
    CaratTimer.enabled := false;
    CaratTimer.free;
    CaratTimer := nil;

    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TCreation.Release

procedure TCreation.MouseDown( Sender : TAniview; Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer; GridX, GridY : integer );
var
  B1, B2, B3, B4 : boolean;
  i : integer;
  BoxWasOpened : boolean;
  BoxClosed : boolean;

const
  FailName : string = 'TCreation.MouseDown';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    BoxClosed := false;
    i := 0;
    while ( i < 16 ) and ( BoxOpen = -1 ) do
    begin
      if ptInRect( ArrowRect[ i ].rect, point( X, Y ) ) then
      begin //if over an Arrow
        B1 := ( ( i > 7 ) and ( i < 13 ) and ( Character.TrainingPoints > 3 ) );
        B2 := ( ( i > 12 ) and ( Character.TrainingPoints > 1 ) );
        B3 := ( ( i < 5 ) and ( StatAdjustments[ i ] > 0 ) );
        B4 := ( ( i > 4 ) and ( i < 8 ) and ( StatAdjustments[ i ] > 0 ) );

        if B1 or B2 or B3 or B4 then
        begin
          if B1 then
          begin //adjust training points, keep track of training points added
            Character.TrainingPoints := -4;
            StatAdjustments[ i - 8 ] := StatAdjustments[ i - 8 ] + 1;
          end
          else if B2 then
          begin
            Character.TrainingPoints := -2;
            StatAdjustments[ i - 8 ] := StatAdjustments[ i - 8 ] + 1;
          end
          else if B3 then
          begin
            Character.TrainingPoints := +4;
            StatAdjustments[ i ] := StatAdjustments[ i ] - 1;
          end
          else
          begin
            Character.TrainingPoints := +2;
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
    end //check for click in box
    else if ( ptInRect( rect( 465, 59, 465 + 123, 59 + 181 ), point( x, y ) ) or ptInRect( rect( 279, 239 + ( BoxOpen - 12 ) * 42, 279 + 123, 239 + ( BoxOpen - 12 ) * 42 + 181 ), point( x, y ) ) ) and ( BoxOpen > -1 ) then
    begin //check to see if box is open
      i := 0;
      while i < 21 do
      begin
        if ptInRect( SelectRect[ i ].rect, point( X, Y ) ) then
        begin //if over an item
            //set the selected for each type based on which text the user hit
          if i < 4 then
          begin
            ixSelectedShirt := i;
            DrawAlpha( DXBack, rect( 113, 236, 261, 264 ), rect( 0, 0, 25, 25 ), DXBlack, False, 255 );
            pText.PlotTextCentered2( DXBack, SelectRect[ i ].Text + txtMessage[ 0 ], 113, 261, 239, 250 )
          end
          else if i < 8 then
          begin
            ixSelectedPants := i;
            DrawAlpha( DXBack, rect( 113, 278, 261, 306 ), rect( 0, 0, 25, 25 ), DXBlack, False, 255 );
            pText.PlotTextCentered2( DXBack, SelectRect[ i ].Text + txtMessage[ 1 ], 113, 261, 281, 250 )
          end
          else if i < 12 then
          begin
            ixSelectedHair := i;
            DrawAlpha( DXBack, rect( 113, 321, 261, 348 ), rect( 0, 0, 25, 25 ), DXBlack, False, 255 );
            pText.PlotTextCentered2( DXBack, SelectRect[ i ].Text + txtMessage[ 2 ], 113, 261, 324, 250 )
          end
          else if i < 16 then
          begin
            ixSelectedHairStyle := i;
            DrawAlpha( DXBack, rect( 113, 363, 261, 391 ), rect( 0, 0, 25, 25 ), DXBlack, False, 255 );
            if i < 14 then
              pText.PlotTextCentered2( DXBack, SelectRect[ i ].Text + txtMessage[ 2 ], 113, 261, 366, 250 )
            else
              pText.PlotTextCentered2( DXBack, SelectRect[ i ].Text, 113, 261, 366, 250 );
          end
          else if i < 18 then
          begin
            ixSelectedBeard := i;
            DrawAlpha( DXBack, rect( 113, 406, 261, 434 ), rect( 0, 0, 25, 25 ), DXBlack, False, 255 );
            if i = 16 then
              pText.PlotTextCentered2( DXBack, txtMessage[ 3 ], 113, 261, 409, 250 )
            else
              pText.PlotTextCentered2( DXBack, txtMessage[ 4 ], 113, 261, 409, 250 );
          end
          else
          begin
            if ChosenTraining = 18 then
            begin
              Character.Strength := Character.BaseStrength - 5;
              Character.Coordination := Character.BaseCoordination - 2;
              Character.Constitution := Character.BaseConstitution - 3;
              Character.Perception := Character.BasePerception + 3;
              Character.Charm := Character.BaseCharm + 3;
              Character.Mysticism := Character.BaseMysticism + 3;
              Character.Combat := Character.BaseCombat - 10;
              Character.Stealth := Character.BaseStealth - 0;
            end
            else if ChosenTraining = 19 then
            begin
              Character.Strength := Character.BaseStrength - 2;
              Character.Coordination := Character.BaseCoordination - 5;
              Character.Constitution := Character.BaseConstitution - 0;
              Character.Perception := Character.BasePerception - 0;
              Character.Charm := Character.BaseCharm + 3;
              Character.Mysticism := Character.BaseMysticism + 3;
              Character.Combat := Character.BaseCombat - 0;
              Character.Stealth := Character.BaseStealth - 10;
            end
            else if ChosenTraining = 20 then
            begin
              Character.Strength := Character.BaseStrength - 0;
              Character.Coordination := Character.BaseCoordination - 3;
              Character.Constitution := Character.BaseConstitution - 2;
              Character.Perception := Character.BasePerception - 2;
              Character.Charm := Character.BaseCharm + 3;
              Character.Mysticism := Character.BaseMysticism - 10;
              Character.Combat := Character.BaseCombat - 0;
              Character.Stealth := Character.BaseStealth + 3;
            end;
            SelectedTraining := i;
            ChosenTraining := i;
               //why do this twice? Because SelectedTraining must be initalized for drawing the select box,
               //yet we must know if the picked a class or not- we dont let them leave without selecting a class.
               //This is a change, so a bit kludgy, but it's the 11th hour here at Digital Tome 6/11/00
            DrawAlpha( lpDDSBack, rect( 300, 132, 448, 160 ), rect( 0, 0, 25, 25 ), DXBlack, False, 255 );
            if i = 18 then
            begin
              if UseSmallFont then
                pText.PlotGoldTextCentered( lpDDSBack, txtMessage[ 5 ], 300, 448, 135, 250 )
              else
                pText.PlotTextCentered( txtMessage[ 5 ], 300, 448, 135, 250 );
              Character.Strength := Character.BaseStrength + 5;
              Character.Coordination := Character.BaseCoordination + 2;
              Character.Constitution := Character.BaseConstitution + 3;
              Character.Perception := Character.BasePerception - 3;
              Character.Charm := Character.BaseCharm - 3;
              Character.Mysticism := Character.BaseMysticism - 3;
              Character.Combat := Character.BaseCombat + 10;
              Character.Stealth := Character.BaseStealth + 0;
            end
            else if i = 19 then
            begin
              if UseSmallFont then
                pText.PlotGoldTextCentered( lpDDSBack, txtMessage[ 6 ], 300, 448, 135, 250 )
              else
                pText.PlotTextCentered( txtMessage[ 6 ], 300, 448, 135, 250 );
              Character.Strength := Character.BaseStrength + 2;
              Character.Coordination := Character.BaseCoordination + 5;
              Character.Constitution := Character.BaseConstitution + 0;
              Character.Perception := Character.BasePerception + 0;
              Character.Charm := Character.BaseCharm - 3;
              Character.Mysticism := Character.BaseMysticism - 3;
              Character.Combat := Character.BaseCombat + 0;
              Character.Stealth := Character.BaseStealth + 10;
            end
            else if i = 20 then
            begin
              if UseSmallFont then
                pText.PlotGoldTextCentered( lpDDSBack, txtMessage[ 7 ], 300, 448, 135, 250 )
              else
                pText.PlotTextCentered( txtMessage[ 7 ], 300, 448, 135, 250 );
              Character.Strength := Character.BaseStrength + 0;
              Character.Coordination := Character.BaseCoordination + 3;
              Character.Constitution := Character.BaseConstitution + 2;
              Character.Perception := Character.BasePerception + 2;
              Character.Charm := Character.BaseCharm - 3;
              Character.Mysticism := Character.BaseMysticism + 10;
              Character.Combat := Character.BaseCombat + 0;
              Character.Stealth := Character.BaseStealth - 3;
            end;
            paint;
          end;
          i := 900; //drop out of the loop
        end;
        i := i + 1;
      end; //wend
      if i = 901 then
      begin //reopen and refresh new selection
        OpenBox( BoxOpen );
        DrawTheGuy;
      end //check to see if the hit the ok button
      else if ( ptInRect( rect( 465, 59, 465 + 123, 59 + 181 ), point( x, y ) ) and ( Y > 59 + 141 ) ) or ( ptInRect( rect( 279, 239 + ( BoxOpen - 12 ) * 42, 279 + 123, 239 + ( BoxOpen - 12 ) * 42 + 181 ), point( x, y ) ) and ( Y > 239 + ( BoxOpen - 12 ) * 42 + 141 ) ) then
      begin
       //clean up any dimmed text
        WrapperBltFast( lpDDSBack, 114, 237, DXBack, Rect( 114, 237, 261, 439 ), DDBLTFAST_WAIT );
       //clean up after old boxes
        WrapperBltFast( lpDDSBack, 279, 239, DXBack, Rect( 279, 239, 402, 588 ), DDBLTFAST_WAIT );
        WrapperBltFast( lpDDSBack, 465, 59, DXBack, Rect( 465, 59, 465 + 123, 59 + 181 ), DDBLTFAST_WAIT );
        BoxOpen := -1;
        BoxClosed := true;
      end;
    end
    else
    begin //see if we're opening the box
      BoxWasOpened := false;
      for i := 12 to 17 do
      begin
        if ptInRect( InfoRect[ i ].rect, point( X, Y ) ) then
        begin
          OpenBox( i );
          BoxWasOpened := true;
        end;
      end;
      if ( BoxWasOpened = false ) and ( BoxOpen > -1 ) then
      begin //close the box -they clicked outside
       //clean up any dimmed text
        WrapperBltFast( lpDDSBack, 114, 237, DXBack, Rect( 114, 237, 261, 439 ), DDBLTFAST_WAIT );
       //clean up after old boxes
        WrapperBltFast( lpDDSBack, 279, 239, DXBack, Rect( 279, 239, 402, 588 ), DDBLTFAST_WAIT );
        WrapperBltFast( lpDDSBack, 465, 59, DXBack, Rect( 465, 59, 465 + 123, 59 + 181 ), DDBLTFAST_WAIT );
        BoxOpen := -1;
        BoxClosed := true;
      end; //endif

    end;
   //else begin//we arent over anything else- check back button
    if ( BoxOpen = -1 ) and not BoxClosed then
    begin
      if PtinRect( rect( chaContinueRect.Left, chaContinueRect.Top, chaContinueRect.Left + chaContinueRect.Right, chaContinueRect.Top + chaContinueRect.Bottom ), point( X, Y ) ) then
      begin //over continue
         //The new data is already saved- if we ever write a Cancel function then we can restore values
         //Exit the screen if the length of name is 1 or greater
        if ( Length( Trim( CharacterName ) ) > 0 ) and ( ChosenTraining > -1 ) then
        begin
          Character.Name := CharacterName;
          Close;
        end
        else
        begin //Hasnt entered name- tell player to enter name or pick training
          if BoxOpen = 17 then
          begin
            WrapperBltFast( lpDDSBack, 490, 239, DXBack, Rect( 490, 239, 682, 430 ), DDBLTFAST_WAIT );
            if UseSmallFont then
            begin
              if ( ChosenTraining > -1 ) then
                pText.PlotGoldTextBlock( txtMessage[ 8 ], 500, 682, 239, 240 )
              else
                pText.PlotGoldTextBlock( txtMessage[ 9 ], 500, 682, 239, 240 );
            end
            else
            begin
              if ( ChosenTraining > -1 ) then
                pText.PlotTextBlock( txtMessage[ 8 ], 500, 682, 239, 240 )
              else
                pText.PlotTextBlock( txtMessage[ 9 ], 500, 682, 239, 240 );
            end;
          end
          else
          begin
            WrapperBltFast( lpDDSBack, 490, 160, DXBack, Rect( 490, 160, 682, 430 ), DDBLTFAST_WAIT );
            if UseSmallFont then
            begin
              if ( ChosenTraining > -1 ) then
                pText.PlotGoldTextBlock( txtMessage[ 10 ], 500, 682, 165, 240 )
              else
                pText.PlotGoldTextBlock( txtMessage[ 11 ], 500, 682, 165, 240 );
            end
            else
            begin
              if ( ChosenTraining > -1 ) then
                pText.PlotTextBlock( txtMessage[ 10 ], 500, 682, 165, 240 )
              else
                pText.PlotTextBlock( txtMessage[ 11 ], 500, 682, 165, 240 );
            end;
          end;
        end;
      end
      else if PtinRect( rect( chaCancelRect.Left, chaCancelRect.Top, chaCancelRect.Left + chaCancelRect.Right, chaCancelRect.Top + chaCancelRect.Bottom ), point( X, Y ) ) then
      begin //over cancel
        Cancel := true;
        Close;
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TCreation.MouseDown

procedure TCreation.MouseMove( Sender : TAniview; Shift : TShiftState; X, Y : Integer; GridX, GridY : integer );
var
  i : integer;
const
  FailName : string = 'TCreation.MouseMove';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
     //Clean up continue and cancel
    WrapperBltFast( lpDDSBack, chaContinueRect.Left, chaContinueRect.Top, DXBack, Rect( chaContinueRect.Left, chaContinueRect.Top, chaContinueRect.Left + chaContinueRect.Right, chaContinueRect.Top + chaContinueRect.Bottom ), DDBLTFAST_WAIT );
    if BoxOpen = -1 then
      WrapperBltFast( lpDDSBack, chaCancelRect.Left, chaCancelRect.Top, DXBack, Rect( chaCancelRect.Left, chaCancelRect.Top, chaCancelRect.Left + chaCancelRect.Right, chaCancelRect.Top + chaCancelRect.Bottom ), DDBLTFAST_WAIT );
   //clear text
    if PtinRect( rect( chaContinueRect.Left, chaContinueRect.Top, chaContinueRect.Left + chaContinueRect.Right, chaContinueRect.Top + chaContinueRect.Bottom ), point( X, Y ) ) then
    begin //over continue
      //dont clear if over continue, we might have the you must enter name text up -kludgy
    end
    else
    begin
      if BoxOpen = 17 then
        WrapperBltFast( lpDDSBack, 490, 239, DXBack, Rect( 490, 239, 682, 430 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT )
      else
        WrapperBltFast( lpDDSBack, 490, 160, DXBack, Rect( 490, 160, 682, 430 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    end;

    i := 0;
    if BoxOpen > -1 then
    begin
      i := 0;
      while i < 21 do
      begin
        if ptInRect( SelectRect[ i ].rect, point( X, Y ) ) then
        begin //if over an item
          if UseSmallFont then
          begin
            if BoxOpen <> 17 then //little kludge here
              pText.PlotGoldTextBlock( SelectRect[ i ].Info, 500, 682, 165, 240 ) //Plot the info
            else
              pText.PlotGoldTextBlock( SelectRect[ i ].Info, 500, 682, 239, 240 ); //Plot the info
          end
          else
          begin
            if BoxOpen <> 17 then //little kludge here
              pText.PlotTextBlock( SelectRect[ i ].Info, 500, 682, 165, 240 ) //Plot the info
            else
              pText.PlotTextBlock( SelectRect[ i ].Info, 500, 682, 239, 240 ); //Plot the info
          end;
          i := 900; //drop out of the loop
        end;
        i := i + 1;
      end; //wend
      if ( BoxOpen < 17 ) and ptInRect( rect( 279, 239 + ( BoxOpen - 12 ) * 42, 279 + 131, 239 + ( BoxOpen - 12 ) * 42 + 181 ), point( X, Y ) ) then
        i := 901; //dont show any hot text under this box
    end; //endif boxopen

    if i <> 901 then
    begin
      i := 0;
      while i < 16 do
      begin
        if ptInRect( ArrowRect[ i ].rect, point( X, Y ) ) then
        begin //if over an Arrow
          if UseSmallFont then
          begin
            if BoxOpen <> 17 then //little kludge here
              pText.PlotGoldTextBlock( ArrowRect[ i ].Info, 500, 682, 165, 240 ) //Plot the info
            else
              pText.PlotGoldTextBlock( ArrowRect[ i ].Info, 500, 682, 239, 240 ); //Plot the info
          end
          else
          begin
            if BoxOpen <> 17 then //little kludge here
              pText.PlotTextBlock( ArrowRect[ i ].Info, 500, 682, 165, 240 ) //Plot the info
            else
              pText.PlotTextBlock( ArrowRect[ i ].Info, 500, 682, 239, 240 ); //Plot the info
          end;

          i := 900; //drop out of the loop
        end;
        i := i + 1;
      end; //wend
    end; //endif i <> 901

    if i < 900 then
    begin //if we aren't over an arrow check all other hot spots
      i := 0;
      while i < 18 do
      begin
        if ptInRect( InfoRect[ i ].rect, point( X, Y ) ) then
        begin //if over an item
          if UseSmallFont then
          begin
            if BoxOpen <> 17 then //little kludge here
              pText.PlotGoldTextBlock( InfoRect[ i ].Info, 500, 682, 165, 240 ) //Plot the info
            else
              pText.PlotGoldTextBlock( InfoRect[ i ].Info, 500, 682, 239, 240 ); //Plot the info
          end
          else
          begin
            if BoxOpen <> 17 then //little kludge here
              pText.PlotTextBlock( InfoRect[ i ].Info, 500, 682, 165, 240 ) //Plot the info
            else
              pText.PlotTextBlock( InfoRect[ i ].Info, 500, 682, 239, 240 ); //Plot the info
          end;

          i := 900; //drop out of the loop
        end;
        i := i + 1;
      end; //wend
    end; //endif i < 900

    if ( i <> 901 ) and ( BoxOpen = -1 ) then
    begin //we arent over anything else- check back button
      if PtinRect( rect( chaContinueRect.Left, chaContinueRect.Top, chaContinueRect.Left + chaContinueRect.Right, chaContinueRect.Top + chaContinueRect.Bottom ), point( X, Y ) ) then
      begin //over Continue
         //plot highlighted Continue
        WrapperBltFast( lpDDSBack, chaContinueRect.Left, chaContinueRect.Top, DXContinue, Rect( 0, 0, chaContinueRect.Right, chaContinueRect.Bottom ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end
      else if PtinRect( rect( chaCancelRect.Left, chaCancelRect.Top, chaCancelRect.Left + chaCancelRect.Right, chaCancelRect.Top + chaCancelRect.Bottom ), point( X, Y ) ) then
      begin //over cancel
         //plot highlighted cancel
        WrapperBltFast( lpDDSBack, chaCancelRect.Left, chaCancelRect.Top, DXCancel, Rect( 0, 0, chaCancelRect.Right, chaCancelRect.Bottom ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end;

    end;

    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    if x > 600 then
      WrapperBltFast( lpDDSBack, 500, 0, lpDDSFront, Rect( 500, 0, 800, 600 ), DDBLTFAST_WAIT )
    else if x > 400 then
      WrapperBltFast( lpDDSBack, 300, 0, lpDDSFront, Rect( 300, 0, 700, 600 ), DDBLTFAST_WAIT )
    else if x > 200 then
      WrapperBltFast( lpDDSBack, 100, 0, lpDDSFront, Rect( 100, 0, 500, 600 ), DDBLTFAST_WAIT )
    else
      WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 300, 600 ), DDBLTFAST_WAIT );

  //WrapperBltFast( lpDDSBack, 101, 61, lpDDSFront, Rect(101, 61, 710, 600), DDBLTFAST_WAIT);
  //WrapperBltFast( lpDDSBack, 101, 61, lpDDSFront, Rect(101, 61, 710, 600), DDBLTFAST_WAIT);
  //MouseCursor.PlotDirty:=false;
  //WrapperBltFast( lpDDSBack, X-20, Y-20, lpDDSFront, Rect(X-20, Y-20, X+50, Y+50), DDBLTFAST_WAIT);
    MouseCursor.PlotDirty := false;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TCreation.MouseMove



procedure TCreation.MouseUp( Sender : TAniview; Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer; GridX, GridY : integer );
const
  FailName : string = 'TCreation.MouseUp';
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

procedure TCreation.Paint;
const
  FailName : string = 'TCreation.Paint';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

  //clear the back down to the text - but dont clear the info block
    WrapperBltFast( lpDDSBack, 370, 210, DXBack, Rect( 370, 210, 461, 435 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
  //replot the entire screen statistics
    ShowStats;
    pText.PlotText( CharacterName, 310, 95, 240 );
    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TCreation.Paint;

procedure TCreation.CreateCollisionRects;
var
  i, j : integer;
  LineHeight : integer;

const
  FailName : string = 'TCreation.CreateCollisonRects';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    //Modifier:=400;
    LineHeight := 24;
   //first the ArrowRects
    for i := 0 to 7 do
    begin
      ArrowRect[ i ].rect.left := 389;
      ArrowRect[ i + 8 ].rect.left := 408;
      ArrowRect[ i ].rect.right := 406;
      ArrowRect[ i + 8 ].rect.right := 425;
      ArrowRect[ i ].rect.top := 239 + i * LineHeight;
      ArrowRect[ i + 8 ].rect.top := 239 + i * LineHeight;
      ArrowRect[ i ].rect.bottom := 239 + i * LineHeight + LineHeight;
      ArrowRect[ i + 8 ].rect.bottom := 239 + i * LineHeight + LineHeight;

      ArrowRect[ i ].info := txtMessage[ 26 ] + StatName[ 0 ][ i + 1 ] + '.';

      if i < 5 then
        ArrowRect[ i + 8 ].info := txtMessage[ 27 ] + StatName[ 0 ][ i + 1 ] + txtMessage[ 28 ] + StatName[ 0 ][ i + 1 ] + txtMessage[ 29 ]
      else
        ArrowRect[ i + 8 ].info := txtMessage[ 30 ] + StatName[ 0 ][ i + 1 ] + txtMessage[ 31 ] + StatName[ 0 ][ i + 1 ] + txtMessage[ 29 ];

    end; //end for

   //Training points
    InfoRect[ 0 ].rect.left := 298;
    InfoRect[ 0 ].rect.top := 212;
    InfoRect[ 0 ].rect.right := 457;
    InfoRect[ 0 ].rect.bottom := 236;
    InfoRect[ 0 ].info := txtMessage[ 32 ];
   //Primary->Stealth
    for i := 1 to 9 do
    begin
      InfoRect[ i ].rect.left := 289;
      InfoRect[ i ].rect.right := 457;
      InfoRect[ i ].rect.top := 239 + ( i - 2 ) * LineHeight;
      InfoRect[ i ].rect.bottom := 239 + ( i - 2 ) * LineHeight + LineHeight;
    end;
    InfoRect[ 1 ].rect.left := -100; //no longer used here
    InfoRect[ 1 ].rect.right := -90;
    InfoRect[ 1 ].info := ''; //Primary skills are your characters main traits.  These skills determine '+
                     //'your Secondary skills, Resistance modifiers and Damage modifers.  Training '+
                     //'points are used to increase your Primary skills.';
    InfoRect[ 2 ].info := txtMessage[ 33 ]; //'Strength represents the physical strength of a character.  Strength affects '+
                     //'how much damage a character inflicts in battle.';
    InfoRect[ 3 ].info := txtMessage[ 34 ]; //'Coordination represents how agile a character is.  Coordination affects '+
                     //'a character''s movement, recovery and resistance modifiers.';
    InfoRect[ 4 ].info := txtMessage[ 35 ]; //'Constitution represents a characters physical hardiness.  Constitution affects '+
                     //'a character''s healing rate and hit points.';
    InfoRect[ 5 ].info := txtMessage[ 36 ]; //'Perception represents how well a character senses the area around him.';
                     //'Perception affects a character''s...something.  Lord knows I''m stumped.';
    InfoRect[ 6 ].info := txtMessage[ 37 ]; //'Charm represents a character''s personal magnetism.  Charm affects the '+
                     //'prices a character can command when buying or selling items.';
    InfoRect[ 7 ].info := txtMessage[ 38 ]; //'Mysticism represents a character''s magical ability.  Mysticism affects '+
                     //'the character''s recharge rate and mana.';
    InfoRect[ 8 ].info := txtMessage[ 39 ]; //'Combat represents a character''s fighting ability.  Combat affects '+
                     //'the character''s damage modifiers.';
    InfoRect[ 9 ].info := txtMessage[ 40 ]; //'Stealth represents the character''s ability to move and avoid detection.';


    i := 10;
   //the characters name
    InfoRect[ i ].rect.left := 301;
    InfoRect[ i ].rect.top := 92;
    InfoRect[ i ].rect.right := 448;
    InfoRect[ i ].rect.bottom := 120;
    InfoRect[ i ].info := txtMessage[ 41 ];
   //the appearance
    i := i + 1;
    InfoRect[ i ].rect.left := 102;
    InfoRect[ i ].rect.top := 64;
    InfoRect[ i ].rect.right := 281;
    InfoRect[ i ].rect.bottom := 221;
    InfoRect[ i ].info := txtMessage[ 42 ];
   //shirt color
    i := i + 1;
    InfoRect[ i ].rect.left := 113;
    InfoRect[ i ].rect.top := 236;
    InfoRect[ i ].rect.right := 281;
    InfoRect[ i ].rect.bottom := 264;
    InfoRect[ i ].info := txtMessage[ 43 ];
   //pants
    i := i + 1;
    InfoRect[ i ].rect.left := 113;
    InfoRect[ i ].rect.top := 278;
    InfoRect[ i ].rect.right := 281;
    InfoRect[ i ].rect.bottom := 306;
    InfoRect[ i ].info := txtMessage[ 44 ];
   //hair color
    i := i + 1;
    InfoRect[ i ].rect.left := 113;
    InfoRect[ i ].rect.top := 321;
    InfoRect[ i ].rect.right := 281;
    InfoRect[ i ].rect.bottom := 348;
    InfoRect[ i ].info := txtMessage[ 45 ];
   //hair style
    i := i + 1;
    InfoRect[ i ].rect.left := 113;
    InfoRect[ i ].rect.top := 363;
    InfoRect[ i ].rect.right := 281;
    InfoRect[ i ].rect.bottom := 391;
    InfoRect[ i ].info := txtMessage[ 46 ];
   //beard
    i := i + 1;
    InfoRect[ i ].rect.left := 113;
    InfoRect[ i ].rect.top := 406;
    InfoRect[ i ].rect.right := 281;
    InfoRect[ i ].rect.bottom := 434;
    InfoRect[ i ].info := txtMessage[ 47 ];
    i := i + 1;
    InfoRect[ i ].rect.left := 300;
    InfoRect[ i ].rect.top := 132;
    InfoRect[ i ].rect.right := 468;
    InfoRect[ i ].rect.bottom := 160;
    InfoRect[ i ].info := txtMessage[ 48 ]; //  Fighting training places an emphasis on your '+
                     //'character''s combat ability, Scouting emphasizes your character''s stealth talents '+
                     //'and Magic emphasizes your character''s spellcasting ability.';

   //now for the selectable text
   //Shirt color
    i := 0;
    SelectRect[ i ].rect.left := -148;
    SelectRect[ i ].rect.top := 80;
    SelectRect[ i ].rect.right := -186;
    SelectRect[ i ].rect.bottom := 101;
    SelectRect[ i ].info := txtMessage[ 49 ];
    SelectRect[ i ].text := txtMessage[ 70 ];
    i := i + 1;
    SelectRect[ i ].rect.left := -197;
    SelectRect[ i ].rect.top := 80;
    SelectRect[ i ].rect.right := -258;
    SelectRect[ i ].rect.bottom := 101;
    SelectRect[ i ].info := txtMessage[ 50 ];
    SelectRect[ i ].text := txtMessage[ 71 ];
    i := i + 1;
    SelectRect[ i ].rect.left := -269;
    SelectRect[ i ].rect.top := 80;
    SelectRect[ i ].rect.right := -320;
    SelectRect[ i ].rect.bottom := 101;
    SelectRect[ i ].info := txtMessage[ 51 ];
    SelectRect[ i ].text := txtMessage[ 72 ];
    i := i + 1;
    SelectRect[ i ].rect.left := -331;
    SelectRect[ i ].rect.top := 80;
    SelectRect[ i ].rect.right := -382;
    SelectRect[ i ].rect.bottom := 101;
    SelectRect[ i ].info := txtMessage[ 52 ];
    SelectRect[ i ].text := txtMessage[ 73 ];
   //Pants color
    i := i + 1;
    SelectRect[ i ].rect.left := -148;
    SelectRect[ i ].rect.top := 110;
    SelectRect[ i ].rect.right := -186;
    SelectRect[ i ].rect.bottom := 131;
    SelectRect[ i ].info := txtMessage[ 53 ];
    SelectRect[ i ].text := txtMessage[ 70 ];
    i := i + 1;
    SelectRect[ i ].rect.left := -197;
    SelectRect[ i ].rect.top := 110;
    SelectRect[ i ].rect.right := -258;
    SelectRect[ i ].rect.bottom := 131;
    SelectRect[ i ].info := txtMessage[ 54 ];
    SelectRect[ i ].text := txtMessage[ 71 ];
    i := i + 1;
    SelectRect[ i ].rect.left := -269;
    SelectRect[ i ].rect.top := 110;
    SelectRect[ i ].rect.right := -320;
    SelectRect[ i ].rect.bottom := 131;
    SelectRect[ i ].info := txtMessage[ 55 ];
    SelectRect[ i ].text := txtMessage[ 72 ];
    i := i + 1;
    SelectRect[ i ].rect.left := -331;
    SelectRect[ i ].rect.top := 110;
    SelectRect[ i ].rect.right := -382;
    SelectRect[ i ].rect.bottom := 131;
    SelectRect[ i ].info := txtMessage[ 56 ];
    SelectRect[ i ].text := txtMessage[ 73 ];
   //hair color
    i := i + 1;
    SelectRect[ i ].rect.left := -149;
    SelectRect[ i ].rect.top := 140;
    SelectRect[ i ].rect.right := -201;
    SelectRect[ i ].rect.bottom := 161;
    SelectRect[ i ].info := txtMessage[ 57 ];
    SelectRect[ i ].text := txtMessage[ 74 ];
    i := i + 1;
    SelectRect[ i ].rect.left := -210;
    SelectRect[ i ].rect.top := 140;
    SelectRect[ i ].rect.right := -271;
    SelectRect[ i ].rect.bottom := 161;
    SelectRect[ i ].info := txtMessage[ 58 ];
    SelectRect[ i ].text := txtMessage[ 71 ];
    i := i + 1;
    SelectRect[ i ].rect.left := -281;
    SelectRect[ i ].rect.top := 140;
    SelectRect[ i ].rect.right := -320;
    SelectRect[ i ].rect.bottom := 161;
    SelectRect[ i ].info := txtMessage[ 59 ];
    SelectRect[ i ].text := txtMessage[ 75 ];
    i := i + 1;
    SelectRect[ i ].rect.left := 329;
    SelectRect[ i ].rect.top := 140;
    SelectRect[ i ].rect.right := 373;
    SelectRect[ i ].rect.bottom := 161;
    SelectRect[ i ].info := txtMessage[ 60 ];
    SelectRect[ i ].text := txtMessage[ 76 ];
   //Hair style
    i := i + 1;
    SelectRect[ i ].rect.left := 149;
    SelectRect[ i ].rect.top := 170;
    SelectRect[ i ].rect.right := 196;
    SelectRect[ i ].rect.bottom := 191;
    SelectRect[ i ].info := txtMessage[ 61 ];
    SelectRect[ i ].text := txtMessage[ 77 ];
    i := i + 1;
    SelectRect[ i ].rect.left := 206;
    SelectRect[ i ].rect.top := 170;
    SelectRect[ i ].rect.right := 249;
    SelectRect[ i ].rect.bottom := 191;
    SelectRect[ i ].info := txtMessage[ 62 ];
    SelectRect[ i ].text := txtMessage[ 78 ];
    i := i + 1;
    SelectRect[ i ].rect.left := 259;
    SelectRect[ i ].rect.top := 170;
    SelectRect[ i ].rect.right := 331;
    SelectRect[ i ].rect.bottom := 194;
    SelectRect[ i ].info := txtMessage[ 63 ];
    SelectRect[ i ].text := txtMessage[ 79 ];
    i := i + 1;
    SelectRect[ i ].rect.left := 340;
    SelectRect[ i ].rect.top := 170;
    SelectRect[ i ].rect.right := 382;
    SelectRect[ i ].rect.bottom := 191;
    SelectRect[ i ].info := txtMessage[ 64 ];
    SelectRect[ i ].text := txtMessage[ 80 ];
   //beard
    i := i + 1;
    SelectRect[ i ].rect.left := 149;
    SelectRect[ i ].rect.top := 200;
    SelectRect[ i ].rect.right := 185;
    SelectRect[ i ].rect.bottom := 223;
    SelectRect[ i ].info := txtMessage[ 65 ];
    SelectRect[ i ].text := txtMessage[ 81 ];
    i := i + 1;
    SelectRect[ i ].rect.left := 194;
    SelectRect[ i ].rect.top := 200;
    SelectRect[ i ].rect.right := 239;
    SelectRect[ i ].rect.bottom := 223;
    SelectRect[ i ].info := txtMessage[ 66 ];
    SelectRect[ i ].text := txtMessage[ 82 ];
   //Training
    i := i + 1;
    SelectRect[ i ].rect.left := 40;
    SelectRect[ i ].rect.top := 270;
    SelectRect[ i ].rect.right := 113;
    SelectRect[ i ].rect.bottom := 293;
    SelectRect[ i ].info := txtMessage[ 67 ];
    SelectRect[ i ].text := txtMessage[ 83 ];
    i := i + 1;
    SelectRect[ i ].rect.left := 127;
    SelectRect[ i ].rect.top := 270;
    SelectRect[ i ].rect.right := 200;
    SelectRect[ i ].rect.bottom := 293;
    SelectRect[ i ].info := txtMessage[ 68 ];
    SelectRect[ i ].text := txtMessage[ 84 ];
    i := i + 1;
    SelectRect[ i ].rect.left := 216;
    SelectRect[ i ].rect.top := 270;
    SelectRect[ i ].rect.right := 270;
    SelectRect[ i ].rect.bottom := 293;
    SelectRect[ i ].info := txtMessage[ 69 ];
    SelectRect[ i ].text := txtMessage[ 85 ];
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;


end; //TCreation.CreateCollisionRects;

{procedure TCreation.DebugPlot(i: integer);
var
 a : string;
const
  FailName: string = 'TCreation.DebugPlot';
begin
{$IFDEF DODEBUG}
{  if (CurrDbgLvl >= DbgLvlSevere) then
    Log.LogEntry(FailName);
{$ENDIF}
{try

WrapperBltFast( lpDDSBack, 20,237,DXBack,Rect(20,237,20+50,237+25),DDBLTFAST_WAIT); //clean up before we plot text
str(i,a);
pText.PlotText(a,20,237,0);
except
   on E: Exception do Log.log(FailName+E.Message);
end;

end; }

procedure TCreation.LoadNames;
const
  FailName : string = 'TCreation.LoadNames';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

  //Loads all the names we use in the Mouseover help
    StatName[ 0 ][ 1 ] := txtMessage[ 86 ]; //'Strength';
    StatName[ 0 ][ 2 ] := txtMessage[ 87 ]; //'Coordination';
    StatName[ 0 ][ 3 ] := txtMessage[ 88 ]; //'Constitution';
    StatName[ 0 ][ 4 ] := txtMessage[ 89 ]; //'Perception';
    StatName[ 0 ][ 5 ] := txtMessage[ 90 ]; //'Charm';
    StatName[ 0 ][ 6 ] := txtMessage[ 91 ]; //'Mysticism';
    StatName[ 0 ][ 7 ] := txtMessage[ 92 ]; //'Combat';
    StatName[ 0 ][ 8 ] := txtMessage[ 93 ]; //'Stealth';

    StatName[ 1 ][ 1 ] := txtMessage[ 94 ]; //'Piercing';
    StatName[ 1 ][ 2 ] := txtMessage[ 95 ]; //'Crushing';
    StatName[ 1 ][ 3 ] := txtMessage[ 96 ]; //'Cutting';
    StatName[ 1 ][ 4 ] := txtMessage[ 97 ]; //'Heat';
    StatName[ 1 ][ 5 ] := txtMessage[ 98 ]; //'Cold';
    StatName[ 1 ][ 6 ] := txtMessage[ 99 ]; //'Electric';
    StatName[ 1 ][ 7 ] := txtMessage[ 100 ]; //'Poison';
    StatName[ 1 ][ 8 ] := txtMessage[ 101 ]; //'Magic';
    StatName[ 1 ][ 9 ] := txtMessage[ 102 ]; //'Mental';
    StatName[ 1 ][ 10 ] := txtMessage[ 103 ]; //'Stun';
    StatName[ 1 ][ 11 ] := txtMessage[ 104 ]; //'Special';

  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TCreation.LoadBaseValues;
var
  i : integer;
const
  FailName : string = 'TCreation.LoadBaseValues';
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

end; //TCreation.LoadBaseValues;

procedure TCreation.ShowStats;
var
  a, b : string;
  i, Alpha : integer;

const
  FailName : string = 'TCreation.ShowStats';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try


    Alpha := 240; //blend value
    str( Character.TrainingPoints, a );
    pText.PlotText( a, 167 + Modifier, 213, Alpha );
   //primary stats column
    i := 239;
    str( Character.Strength, b );
    pText.PlotText( b, 167 + Modifier, i, Alpha );
    i := i + 24;

    str( Character.Coordination, b );
    pText.PlotText( b, 167 + Modifier, i, Alpha );
    i := i + 24;

    str( Character.Constitution, b );
    pText.PlotText( b, 167 + Modifier, i, Alpha );
    i := i + 24;

    str( Character.BasePerception, b );
    pText.PlotText( b, 167 + Modifier, i, Alpha );
    i := i + 24;

    str( Character.Charm, b );
    pText.PlotText( b, 167 + Modifier, i, Alpha );
    i := i + 24;

    str( Character.Mysticism, b );
    pText.PlotText( b, 167 + Modifier, i, Alpha );
    i := i + 24;

    str( Character.Combat, b );
    pText.PlotText( b, 167 + Modifier, i, Alpha );
    i := i + 24;

    str( Character.Stealth, b );
    pText.PlotText( b, 167 + Modifier, i, Alpha );

  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TCreation.ShowStats



procedure TCreation.KeyDown( Sender : TObject; var key : Word; Shift : TShiftState );
var
  i : integer;
  a : string;
const
  FailName : string = 'TCreation.Keydown';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

     //DebugPlot(Key);
    WrapperBltFast( lpDDSBack, 300, 92, DXBack, rect( 300, 92, 448, 120 ), DDBLTFAST_WAIT );
    if Key = 39 then
      Key := 999; //keep the right arrow form printing Apostrophes
    if Key = 222 then
      Key := 39; //apostrophe
    if Key = 189 then
      Key := 45; //dash
    if ( ( Key > 64 ) and ( Key < 91 ) ) or ( ( Key > 47 ) and ( Key < 58 ) ) or ( Key = 32 ) or ( key = 189 ) or ( key = 39 ) or ( Key = 45 ) then
    begin
      if ( ( Key > 64 ) and ( Key < 91 ) ) and ( Shift <> [ ssShift ] ) then //make the char lowercase
        Key := Key + 32;
        //if (Length(CharacterName) < 13) and (pText.TextLength(CharacterName)< 130) then begin
      if pText.TextLength( CharacterName ) < 120 then
      begin
        if CaratCharPosition = Length( CharacterName ) then
        begin //adding a char to end of string
          CharacterName := CharacterName + char( Key );
          CaratPosition := pText.TextLength( CharacterName );
          CaratCharPosition := CaratCharPosition + 1;
        end
        else
        begin //inserting a char
          CaratCharPosition := CaratCharPosition + 1;
          CharacterName := CharacterName + 'z'; //increase the size of the string by a char
          for i := Length( CharacterName ) downto CaratCharPosition do
          begin
            CharacterName[ i ] := CharacterName[ i - 1 ];
          end; //end for
          CharacterName[ CaratCharPosition ] := char( Key );
          a := CharacterName;
          SetLength( a, CaratCharPosition );
          CaratPosition := pText.TextLength( a );
        end;
      end; //endif if length< 21
    end
    else if ( Key = 8 ) then
    begin //backspace
      if CharacterName <> '' then
      begin
        if CaratCharPosition = Length( CharacterName ) then
        begin //if at the end of the name
          CaratCharPosition := CaratCharPosition - 1;
          if CaratCharPosition = 0 then
            CharacterName := ''
          else
            SetLength( CharacterName, CaratCharPosition );
          CaratPosition := pText.TextLength( CharacterName );
        end
        else if CaratCharPosition > 0 then
        begin //in middle of name somewhere
          CaratCharPosition := CaratCharPosition - 1;
          for i := CaratCharPosition + 2 to Length( CharacterName ) do
          begin //chop out the middle char
            CharacterName[ i - 1 ] := CharacterName[ i ];
          end; //end for
          SetLength( CharacterName, Length( CharacterName ) - 1 );
          a := CharacterName;
          SetLength( a, CaratCharPosition );
          CaratPosition := pText.TextLength( a );
        end;

      end; //endif length
    end
    else if ( Key = 46 ) then
    begin //Delete
      if ( CharacterName <> '' ) and ( CaratCharPosition <> Length( CharacterName ) ) then
      begin
        if ( CaratCharPosition = 0 ) and ( Length( CharacterName ) = 1 ) then
          CharacterName := ''
        else
        begin
          for i := CaratCharPosition + 1 to Length( CharacterName ) do
          begin
            CharacterName[ i ] := CharacterName[ i + 1 ];
          end;
          SetLength( CharacterName, Length( CharacterName ) - 1 );
        end;
      end;
    end
    else if Key = 37 then
    begin //left arrow
      if CaratCharPosition > 0 then
      begin
        CaratCharPosition := CaratCharPosition - 1;
        a := CharacterName;
        SetLength( a, CaratCharPosition );
        CaratPosition := pText.TextLength( a );
      end; //endif
    end
    else if Key = 999 then
    begin //right arrow
      if CaratCharPosition < length( CharacterName ) then
      begin
        CaratCharPosition := CaratCharPosition + 1;
        a := CharacterName;
        SetLength( a, CaratCharPosition );
        CaratPosition := pText.TextLength( a );
      end; //endif
    end;

     //Character.Name:=CharacterName;
    pText.PlotText( CharacterName, 310, 95, 240 );
     //plot the Carat
    pText.PlotText( '|', CaratPosition + 310, 95, 240 );
    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TCreation.KeyDown

procedure TCreation.CaratTimerEvent( Sender : TObject );
const
  FailName : string = 'TCreation.CaratTimerEvent';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    WrapperBltFast( lpDDSBack, 300, 92, DXBack, rect( 300, 92, 448, 120 ), DDBLTFAST_WAIT );
    inc( LoopCounter );
    if LoopCounter >= 5 then
    begin
      CaratVisible := ( CaratVisible = false );
      LoopCounter := 0;
    end;
    if CaratVisible then
    begin
      pText.PlotText( '|', CaratPosition + 310, 95, 240 );
    end;
    pText.PlotText( CharacterName, 310, 95, 240 );
    DrawTheGuy;
//    lpDDSFront.Flip(nil, DDFLIP_WAIT);
//    WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect(0, 0, 800, 600), DDBLTFAST_WAIT);
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TCreation.CaratTimerEvent


procedure TCreation.OpenBox( box : integer );
var
  i : integer;
const
  FailName : string = 'TCreation.OpenBox';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

   //clean up any dimmed text
    WrapperBltFast( lpDDSBack, 114, 237, DXBack, Rect( 114, 237, 261, 439 ), DDBLTFAST_WAIT );
   //clean up after old boxes
    WrapperBltFast( lpDDSBack, 279, 239, DXBack, Rect( 279, 239, 402, 588 ), DDBLTFAST_WAIT );
    if BoxOpen = 17 then
      WrapperBltFast( lpDDSBack, 465, 59, DXBack, Rect( 465, 59, 465 + 123, 59 + 180 ), DDBLTFAST_WAIT );
   //clear the selectable rects
    for i := 0 to 20 do
    begin
      SelectRect[ i ].rect := rect( -100, 0, -50, 10 );
    end;
    if box = 17 then
    begin
      WrapperBltFast( lpDDSBack, 490, 160, DXBack, Rect( 490, 160, 682, 430 ), DDBLTFAST_WAIT );
      WrapperBltFast( lpDDSBack, 465, 59, DXBox, Rect( 0, 0, 123, 180 ), DDBLTFAST_WAIT );
      WrapperBltFast( lpDDSBack, 465 + 13, 69 + 38 + 24 * ( SelectedTraining - 18 ), DXCircle, Rect( 0, 0, 96, 21 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      if UseSmallFont then
        pText.PlotGoldTextCentered( lpDDSBack, txtMessage[ 5 ], 465, 465 + 123, 69 + 38, 240 )
      else
        pText.PlotTextCentered( txtMessage[ 5 ], 465, 465 + 123, 69 + 38, 240 );
      if UseSmallFont then
        pText.PlotGoldTextCentered( lpDDSBack, txtMessage[ 6 ], 465, 465 + 123, 69 + 62, 240 )
      else
        pText.PlotTextCentered( txtMessage[ 6 ], 465, 465 + 123, 69 + 62, 240 );
      if UseSmallFont then
        pText.PlotGoldTextCentered( lpDDSBack, txtMessage[ 7 ], 465, 465 + 123, 69 + 86, 240 )
      else
        pText.PlotTextCentered( txtMessage[ 7 ], 465, 465 + 123, 69 + 86, 240 );
      for i := 0 to 2 do
        SelectRect[ 18 + i ].rect := rect( 465, 69 + 38 + 24 * i, 465 + 123, 69 + 38 + 24 + 24 * i );
    end
    else
    begin
      for i := 12 to 16 do
      begin
        if i <> box then
          DrawAlpha( lpDDSBack, rect( InfoRect[ i ].rect.left, InfoRect[ i ].rect.top, InfoRect[ i ].rect.right - 20, InfoRect[ i ].rect.bottom ), rect( 0, 0, 25, 25 ), DXBlack, False, 200 );
      end;
     //plot box
      WrapperBltFast( lpDDSBack, 279, 239 + ( box - 12 ) * 42, DXBox, Rect( 0, 0, 123, 180 ), DDBLTFAST_WAIT );
      if ( Box = 12 ) or ( Box = 13 ) then
      begin
        //now showselected shirt or pants
        if Box = 12 then
          WrapperBltFast( lpDDSBack, 279 + 13, 239 + ( box - 12 ) * 42 + 34 + 24 * ixSelectedShirt, DXCircle, Rect( 0, 0, 96, 21 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT )
        else
          WrapperBltFast( lpDDSBack, 279 + 13, 239 + ( box - 12 ) * 42 + 34 + 24 * ( ixSelectedPants - 4 ), DXCircle, Rect( 0, 0, 96, 21 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

        pText.PlotTextCentered( txtMessage[ 12 ], 279, 279 + 123, 239 + ( box - 12 ) * 42 + 34, 240 );
        pText.PlotTextCentered( txtMessage[ 13 ], 279, 279 + 123, 239 + ( box - 12 ) * 42 + 58, 240 );
        pText.PlotTextCentered( txtMessage[ 14 ], 279, 279 + 123, 239 + ( box - 12 ) * 42 + 82, 240 );
        pText.PlotTextCentered( txtMessage[ 15 ], 279, 279 + 123, 239 + ( box - 12 ) * 42 + 106, 240 );
        //set coll rects
        for i := 0 to 3 do
          SelectRect[ ( Box - 12 ) * 4 + i ].rect := rect( 279, 239 + ( box - 12 ) * 42 + 34 + 24 * i, 279 + 123, 239 + ( box - 12 ) * 42 + 34 + 24 + 24 * i );
      end
      else if box = 14 then
      begin
        WrapperBltFast( lpDDSBack, 279 + 13, 239 + ( box - 12 ) * 42 + 34 + 24 * ( ixSelectedHair - 8 ), DXCircle, Rect( 0, 0, 96, 21 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        pText.PlotTextCentered( txtMessage[ 16 ], 279, 279 + 123, 239 + ( box - 12 ) * 42 + 34, 240 );
        pText.PlotTextCentered( txtMessage[ 17 ], 279, 279 + 123, 239 + ( box - 12 ) * 42 + 58, 240 );
        pText.PlotTextCentered( txtMessage[ 18 ], 279, 279 + 123, 239 + ( box - 12 ) * 42 + 82, 240 );
        pText.PlotTextCentered( txtMessage[ 19 ], 279, 279 + 123, 239 + ( box - 12 ) * 42 + 106, 240 );
        for i := 0 to 3 do
          SelectRect[ ( Box - 12 ) * 4 + i ].rect := rect( 279, 239 + ( box - 12 ) * 42 + 34 + 24 * i, 279 + 123, 239 + ( box - 12 ) * 42 + 34 + 24 + 24 * i );
      end
      else if box = 15 then
      begin
        WrapperBltFast( lpDDSBack, 279 + 13, 239 + ( box - 12 ) * 42 + 34 + 24 * ( ixSelectedHairStyle - 12 ), DXCircle, Rect( 0, 0, 96, 21 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        pText.PlotTextCentered( txtMessage[ 20 ], 279, 279 + 123, 239 + ( box - 12 ) * 42 + 34, 240 );
        pText.PlotTextCentered( txtMessage[ 21 ], 279, 279 + 123, 239 + ( box - 12 ) * 42 + 58, 240 );
        pText.PlotTextCentered( txtMessage[ 22 ], 279, 279 + 123, 239 + ( box - 12 ) * 42 + 82, 240 );
        pText.PlotTextCentered( txtMessage[ 23 ], 279, 279 + 123, 239 + ( box - 12 ) * 42 + 106, 240 );
        for i := 0 to 3 do
          SelectRect[ ( Box - 12 ) * 4 + i ].rect := rect( 279, 239 + ( box - 12 ) * 42 + 34 + 24 * i, 279 + 123, 239 + ( box - 12 ) * 42 + 34 + 24 + 24 * i );
      end
      else if box = 16 then
      begin
        WrapperBltFast( lpDDSBack, 279 + 13, 239 + ( box - 12 ) * 42 + 34 + 24 * ( ixSelectedBeard - 16 ) + 24, DXCircle, Rect( 0, 0, 96, 21 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        pText.PlotTextCentered( txtMessage[ 24 ], 279, 279 + 123, 239 + ( box - 12 ) * 42 + 58, 240 );
        pText.PlotTextCentered( txtMessage[ 25 ], 279, 279 + 123, 239 + ( box - 12 ) * 42 + 82, 240 );
        for i := 0 to 1 do
          SelectRect[ ( Box - 12 ) * 4 + i ].rect := rect( 279, 239 + ( box - 12 ) * 42 + 34 + 24 * i + 24, 279 + 123, 239 + ( box - 12 ) * 42 + 34 + 24 + 24 * i + 24 );
      end;

    end; //ednif


    BoxOpen := box;
    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //OpenBox

procedure TCreation.DrawTheGuy;
const
  FailName : string = 'TCreation.DrawtheGuy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    WrapperBltFast( lpDDSBack, 110, 93, DXBack, Rect( 110, 93, 264, 227 ), DDBLTFAST_WAIT );
    SelectedShirt := shirt[ ixSelectedShirt + 1 ];
    SelectedPants := pants[ ( ixSelectedPants - 4 ) + 1 ];
    SelectedHair := hair[ ( ixSelectedHair - 8 ) + 1, ( ixSelectedHairStyle - 12 ) + 1, ( ixSelectedBeard - 16 ) + 1 ];
    if assigned( OnDraw ) then
      OnDraw( self );
    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //DrawTheGuy

procedure TCreation.FormMouseDown( Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer );
const
  FailName : string = 'TCreation.FromMouseDown';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    if PtInRect( rect( 318, 553, 359, 577 ), point( x, y ) ) or PtInRect( rect( 318, 510, 359, 537 ), point( x, y ) ) then
    begin
       //clean up any dimmed text
      WrapperBltFast( lpDDSBack, 114, 237, DXBack, Rect( 114, 237, 261, 439 ), DDBLTFAST_WAIT );
       //clean up after old boxes
      WrapperBltFast( lpDDSBack, 279, 239, DXBack, Rect( 279, 239, 402, 588 ), DDBLTFAST_WAIT );
      WrapperBltFast( lpDDSBack, 465, 59, DXBack, Rect( 465, 59, 465 + 123, 59 + 181 ), DDBLTFAST_WAIT );
      BoxOpen := -1;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //FormMouseDown

procedure TCreation.FormMouseMove( Sender : TObject; Shift : TShiftState; X, Y : Integer );
const
  FailName : string = 'TCreation.FormMouseMove';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

     //Clean up continue and cancel
    WrapperBltFast( lpDDSBack, chaContinueRect.Left, chaContinueRect.Top, DXBack, Rect( chaContinueRect.Left, chaContinueRect.Top, chaContinueRect.Left + chaContinueRect.Right, chaContinueRect.Top + chaContinueRect.Bottom ), DDBLTFAST_WAIT );
    if BoxOpen = -1 then
      WrapperBltFast( lpDDSBack, chaCancelRect.Left, chaCancelRect.Top, DXBack, Rect( chaCancelRect.Left, chaCancelRect.Top, chaCancelRect.Left + chaCancelRect.Right, chaCancelRect.Top + chaCancelRect.Bottom ), DDBLTFAST_WAIT );
   //clear text
    if PtinRect( rect( chaContinueRect.Left, chaContinueRect.Top, chaContinueRect.Left + chaContinueRect.Right, chaContinueRect.Top + chaContinueRect.Bottom ), point( X, Y ) ) then
    begin //over continue
      //dont clear if over continue, we might have the you must enter name text up -kludgy
    end
    else
    begin
      if BoxOpen = 17 then
        WrapperBltFast( lpDDSBack, 490, 239, DXBack, Rect( 490, 239, 682, 430 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT )
      else
        WrapperBltFast( lpDDSBack, 490, 160, DXBack, Rect( 490, 160, 682, 430 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //FormMouseMove

end.

