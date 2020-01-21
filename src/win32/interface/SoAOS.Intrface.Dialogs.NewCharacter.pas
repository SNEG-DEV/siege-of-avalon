unit SoAOS.Intrface.Dialogs.NewCharacter;
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

  Description: NewPlayer dialog - was CharCreation.pas - a lot more clean-up is coming

  Notes: Beware of the offset calculation - Blt to offset coords - with src rects. Use offset rects for mousemove/down and offset coords for text.

  Requires: Delphi 10.3.3 or later

  Revision History:
  - 13 Jul 2003 - DL: Initial Upload to CVS
  - 10 Mar 2019 - SN: Forked on GitHub
  see git repo afterwards

*)

interface

uses
  DirectX,

  System.Types,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,

  Character,
  Resource,
  SoAOS.Intrface.Dialogs,
  Anigrp30;

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

  TCreation = class( TDialog )
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
    LoopCounter, Spinner : integer;
    txtMessage : array[ 0..104 ] of string;
    // Local functions to honor offset - should be moved to gametext as relative function
    // PlotText(msg, x, x2, y, relativepoint=nil, centered=false) bla. bla. - will happen on gametext cleanup
    function ApplyOffset(r: TRect): TRect;
    procedure PlotText(const Sentence: string; const X, Y, Alpha: Integer);
    procedure PlotTextCentered( const DX : IDirectDrawSurface; const Sentence : string; const X1, X2, Y, Alpha : Integer; Const UseSmallFnt: Boolean = False );
    procedure PlotTextBlock( const Sentence : string; X1, X2, Y, Alpha : integer; Const UseSmallFnt: Boolean = False );
    //
    procedure DrawNewPlayer;
    procedure OpenBox( box : integer );
    procedure CaratTimerEvent( Sender : TObject );
    procedure LoadBaseValues; //saves the base stats of the character
    procedure LoadNames;
    procedure CreateCollisionRects; //create the rects for the collision detection
    procedure ShowStats; //plots all the numbers on the screen
//    procedure DebugPlot(i: integer);
    procedure CharCreationDraw(Sender : TObject);
    procedure FormMouseDown( Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer );
    procedure FormMouseMove( Sender : TObject; Shift : TShiftState; X, Y : Integer );
    function GetCancelRect: TRect;
    function GetContinueRect: TRect;
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

    Character : TCharacter;
    Cancel : boolean; //was Cancel Pressed?
    frmMain : TForm; //we need the  form passed into handle form mouse events
    procedure Paint; override;
    procedure Init; override;
    procedure Release; override;
    property OnDraw : TNotifyEvent read FOnDraw write FOnDraw;

    property CancelRect: TRect read GetCancelRect;
    property ContinueRect: TRect read GetContinueRect;
  end;

implementation

uses
  System.SysUtils,
{$IFDEF DirectX}
  DXUtil,
  DXEffects,
  DFX,
{$ENDIF}
  SoAOS.Types,
  SoAOS.Graphics.Types,
  SoAOS.Graphics.Draw,
  SoAOS.Intrface.Text,
  Engine,
  Logfile,
  GameText,
  AniDemo;

{ TCreation }

procedure TCreation.Init;
var
  i : integer;
  pr : TRect;
  width, height: integer;
const
  FailName : string = 'TCreation.init';
begin
  Log.DebugLog( FailName );
  try

    if Loaded then
      Exit;
    inherited;
    Spinner := 0;
    FOnDraw := CharCreationDraw;

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
    LoadBaseValues;

  //Load the Background Bitmap and plot it

    DXCircle := SoAOS_DX_LoadBMP( InterfacePath + 'chaRedOval.bmp', cInvisColor );
    DXBlack := SoAOS_DX_LoadBMP( InterfacePath + 'chaBlack.bmp', cInvisColor );
    DXBox := SoAOS_DX_LoadBMP( InterfacePath + 'chaChooseBox.bmp', cInvisColor );

    DXContinue := SoAOS_DX_LoadBMP( InterfacePath + 'chaContinue.bmp', cInvisColor );
    DXCancel := SoAOS_DX_LoadBMP( InterfacePath + 'chaCancel.bmp', cInvisColor, width, height );

    DXBack := SoAOS_DX_LoadBMP( InterfacePath + 'CharCreate.bmp', cInvisColor, DlgWidth, DlgHeight );
    pr := Rect( 0, 0, DlgWidth, DlgHeight );
    lpDDSBack.BltFast( Offset.X, Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

    CreateCollisionRects;  // Must be called after "Offset" is ready

    ShowStats;
    DrawNewPlayer;

  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TCreation.Init;

procedure TCreation.Release;
const
  FailName : string = 'TCreation.Release';
begin
  Log.DebugLog( FailName );
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
  r1, r2, r, pr: TRect;
const
  FailName : string = 'TCreation.MouseDown';
begin
  Log.DebugLog( FailName );
  try

    BoxClosed := false;
    i := 0;
    while ( i < 16 ) and ( BoxOpen = -1 ) do
    begin
      if ArrowRect[ i ].rect.Contains( Point( X, Y ) ) then
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

    r1 := ApplyOffset( Rect( 465, 59, 465 + 123, 59 + 181 ) );
    r2 := ApplyOffset( Rect( 279, 239 + ( BoxOpen - 12 ) * 42, 279 + 123, 239 + ( BoxOpen - 12 ) * 42 + 181 ) );

    if i = 889 then
    begin //we hit an arrow and changed a stat
      paint;
    end //check for click in box
    else if ( r1.Contains( Point( x, y ) ) or r2.Contains( Point( x, y ) ) ) and ( BoxOpen > -1 ) then
    begin //check to see if box is open
      i := 0;
      while i < 21 do
      begin
        if SelectRect[ i ].rect.Contains( Point( X, Y ) ) then
        begin //if over an item
            //set the selected for each type based on which text the user hit
          if i < 4 then
          begin
            ixSelectedShirt := i;
            r := Rect( 113, 236, 261, 264 );
            DrawAlpha( DXBack, r, rect( 0, 0, 25, 25 ), DXBlack, False, 255 );
            pText.PlotTextCentered2( DXBack, SelectRect[ i ].Text + txtMessage[ 0 ], r.Left, r.Right, 239, 250 )
          end
          else if i < 8 then
          begin
            ixSelectedPants := i;
            r := Rect( 113, 278, 261, 306 );
            DrawAlpha( DXBack, r, rect( 0, 0, 25, 25 ), DXBlack, False, 255 );
            pText.PlotTextCentered2( DXBack, SelectRect[ i ].Text + txtMessage[ 1 ], r.Left, r.Right, 281, 250 )
          end
          else if i < 12 then
          begin
            ixSelectedHair := i;
            r := Rect( 113, 321, 261, 348 );
            DrawAlpha( DXBack, r, rect( 0, 0, 25, 25 ), DXBlack, False, 255 );
            pText.PlotTextCentered2( DXBack, SelectRect[ i ].Text + txtMessage[ 2 ], r.Left, r.Right, 324, 250 )
          end
          else if i < 16 then
          begin
            ixSelectedHairStyle := i;
            r := Rect( 113, 363, 261, 391 );
            DrawAlpha( DXBack, r, rect( 0, 0, 25, 25 ), DXBlack, False, 255 );
            if i < 14 then
              pText.PlotTextCentered2( DXBack, SelectRect[ i ].Text + txtMessage[ 2 ], r.Left, r.Right, 366, 250 )
            else
              pText.PlotTextCentered2( DXBack, SelectRect[ i ].Text, r.Left, r.Right, 366, 250 );
          end
          else if i < 18 then
          begin
            ixSelectedBeard := i;
            r := Rect( 113, 406, 261, 434 );
            DrawAlpha( DXBack, r, rect( 0, 0, 25, 25 ), DXBlack, False, 255 );
            if i = 16 then
              pText.PlotTextCentered2( DXBack, txtMessage[ 3 ], r.Left, r.Right, 409, 250 )
            else
              pText.PlotTextCentered2( DXBack, txtMessage[ 4 ], r.Left, r.Right, 409, 250 );
          end
          else
          begin
            if ChosenTraining = 18 then
            begin
              Character.Strength := Character.BaseStrength - 4;
              Character.Coordination := Character.BaseCoordination - 2;
              Character.Constitution := Character.BaseConstitution - 2;
              Character.Perception := Character.BasePerception + 3;
              Character.Charm := Character.BaseCharm + 1;
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
            r := ApplyOffset( Rect( 300, 132, 448, 160 ) );
            DrawAlpha( lpDDSBack, r, Rect( 0, 0, 25, 25 ), DXBlack, False, 255 );
            if i = 18 then
            begin
              PlotTextCentered( lpDDSBack, txtMessage[ 5 ], 300, 448, 135, 250, UseSmallFont );
              Character.Strength := Character.BaseStrength + 4;
              Character.Coordination := Character.BaseCoordination + 2;
              Character.Constitution := Character.BaseConstitution + 2;
              Character.Perception := Character.BasePerception - 3;
              Character.Charm := Character.BaseCharm - 1;
              Character.Mysticism := Character.BaseMysticism - 3;
              Character.Combat := Character.BaseCombat + 10;
              Character.Stealth := Character.BaseStealth + 0;
            end
            else if i = 19 then
            begin
              PlotTextCentered( lpDDSBack, txtMessage[ 6 ], 300, 448, 135, 250, UseSmallFont );
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
              PlotTextCentered( lpDDSBack, txtMessage[ 7 ], 300, 448, 135, 250, UseSmallFont );
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

      r1 := ApplyOffset( Rect( 465, 59, 465 + 123, 59 + 181 ) );
      r2 := ApplyOffset( Rect( 279, 239 + ( BoxOpen - 12 ) * 42, 279 + 123, 239 + ( BoxOpen - 12 ) * 42 + 181 ) );
      if i = 901 then
      begin //reopen and refresh new selection
        OpenBox( BoxOpen );
        DrawNewPlayer;
      end //check to see if the hit the ok button
      else if ( r1.Contains( Point( x, y ) ) and ( Y > 59 + 141 ) ) or ( r2.Contains( Point( x, y ) ) and ( Y > 239 + ( BoxOpen - 12 ) * 42 + 141 ) ) then
      begin
        //clean up any dimmed text
        pr := Rect( 114, 237, 261, 439 );
        lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
        //clean up after old boxes
        pr := Rect( 279, 239, 402, 588 );
        lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
        pr := Rect( 465, 59, 465 + 123, 59 + 181 );
        lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
        BoxOpen := -1;
        BoxClosed := true;
      end;
    end
    else
    begin //see if we're opening the box
      BoxWasOpened := false;
      for i := 12 to 17 do
      begin
        if InfoRect[ i ].rect.Contains( Point( X, Y ) ) then
        begin
          OpenBox( i );
          BoxWasOpened := true;
        end;
      end;
      if ( BoxWasOpened = false ) and ( BoxOpen > -1 ) then
      begin //close the box -they clicked outside
        //clean up any dimmed text
        pr := Rect( 114, 237, 261, 439 );
        lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
        //clean up after old boxes
        pr := Rect( 279, 239, 402, 588 );
        lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
        pr := Rect( 465, 59, 465 + 123, 59 + 181 );
        lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
        BoxOpen := -1;
        BoxClosed := true;
      end; //endif

    end;
   //else begin//we arent over anything else- check back button
    if ( BoxOpen = -1 ) and not BoxClosed then
    begin
      if ContinueRect.Contains( Point( X, Y ) ) then
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
            pr := Rect( 490, 239, 682, 430 );
            lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
            if ( ChosenTraining > -1 ) then
              PlotTextBlock( txtMessage[ 8 ], 500, 682, 239, 240, UseSmallFont )
            else
              PlotTextBlock( txtMessage[ 9 ], 500, 682, 239, 240, UseSmallFont );
          end
          else
          begin
            pr := Rect( 490, 160, 682, 430 );
            lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
            if ( ChosenTraining > -1 ) then
              PlotTextBlock( txtMessage[ 10 ], 500, 682, 165, 240, UseSmallFont )
            else
              PlotTextBlock( txtMessage[ 11 ], 500, 682, 165, 240, UseSmallFont );
          end;
        end;
      end
      else if CancelRect.Contains( Point( X, Y ) ) then
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
  pr : TRect;
const
  FailName : string = 'TCreation.MouseMove';
begin
  Log.DebugLog( FailName );
  try
    //Clean up continue and cancel
    pr := DlgRect.dlgNewContinueRect;
    lpDDSBack.BltFast( ContinueRect.Left, ContinueRect.Top, DXBack, @pr, DDBLTFAST_WAIT );
    if BoxOpen = -1 then
    begin
      pr := DlgRect.dlgNewCancelRect;
      lpDDSBack.BltFast( CancelRect.Left, CancelRect.Top, DXBack, @pr, DDBLTFAST_WAIT );
    end;
   //clear text
    if ContinueRect.Contains( Point( X, Y ) ) then
    begin //over continue
      //dont clear if over continue, we might have the you must enter name text up -kludgy
    end
    else
    begin

      if BoxOpen = 17 then
      begin
        pr := Rect( 490, 239, 682, 430 );
        lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end
      else
      begin
        pr := Rect( 490, 160, 682, 430 );
        lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end;
    end;

    i := 0;
    if BoxOpen > -1 then
    begin
      i := 0;
      while i < 21 do
      begin
        if SelectRect[ i ].rect.Contains( Point( X, Y ) ) then
        begin //if over an item
          if BoxOpen <> 17 then //little kludge here
            PlotTextBlock( SelectRect[ i ].Info, 500, 682, 165, 240, UseSmallFont ) //Plot the info
          else
            PlotTextBlock( SelectRect[ i ].Info, 500, 682, 239, 240, UseSmallFont ); //Plot the info
          i := 900; //drop out of the loop
        end;
        i := i + 1;
      end; //wend
      if ( BoxOpen < 17 ) and ApplyOffset( Rect( 279, 239 + ( BoxOpen - 12 ) * 42, 279 + 131, 239 + ( BoxOpen - 12 ) * 42 + 181 )).Contains( Point( X, Y ) ) then
        i := 901; //dont show any hot text under this box
    end; //endif boxopen

    if i <> 901 then
    begin
      i := 0;
      while i < 16 do
      begin
        if ArrowRect[ i ].rect.Contains( Point( X, Y ) ) then
        begin //if over an Arrow
          if BoxOpen <> 17 then //little kludge here
            PlotTextBlock( ArrowRect[ i ].Info, 500, 682, 165, 240, UseSmallFont ) //Plot the info
          else
            PlotTextBlock( ArrowRect[ i ].Info, 500, 682, 239, 240, UseSmallFont ); //Plot the info

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
        if InfoRect[ i ].rect.Contains( point( X, Y ) ) then
        begin //if over an item
          if BoxOpen <> 17 then //little kludge here
            PlotTextBlock( InfoRect[ i ].Info, 500, 682, 165, 240, UseSmallFont ) //Plot the info
          else
            PlotTextBlock( InfoRect[ i ].Info, 500, 682, 239, 240, UseSmallFont ); //Plot the info

          i := 900; //drop out of the loop
        end;
        i := i + 1;
      end; //wend
    end; //endif i < 900

    if ( i <> 901 ) and ( BoxOpen = -1 ) then
    begin //we arent over anything else- check back button
      if ContinueRect.Contains( Point( X, Y ) ) then
      begin //over Continue
         //plot highlighted Continue
        pr := Rect( 0, 0, ContinueRect.Width, ContinueRect.Height );
        lpDDSBack.BltFast( ContinueRect.Left, ContinueRect.Top, DXContinue, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end
      else if CancelRect.Contains( Point( X, Y ) ) then
      begin //over cancel
         //plot highlighted cancel
        pr := Rect( 0, 0, CancelRect.Width, CancelRect.Height );
        lpDDSBack.BltFast( CancelRect.Left, CancelRect.Top, DXCancel, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end;

    end;
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
  Log.DebugLog( FailName );
  try
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TCreation.Paint;
const
  FailName : string = 'TCreation.Paint';
var
  pr : TRect;
begin
  Log.DebugLog( FailName );
  try

  //clear the back down to the text - but dont clear the info block
    pr := Rect( 370, 210, 461, 435 );
    lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y , DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
  //replot the entire screen statistics
    ShowStats;
    PlotText( CharacterName, 310, 95, 240 );
    SoAOS_DX_BltFront;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TCreation.PlotText(const Sentence: string; const X, Y, Alpha: Integer);
begin
  pText.PlotText( Sentence, X + Offset.X, Y + Offset.Y, Alpha );
end;

procedure TCreation.PlotTextBlock( const Sentence : string; X1, X2, Y, Alpha : integer; Const UseSmallFnt: Boolean = False );
begin
  if UseSmallFnt then
    pText.PlotGoldTextBlock( Sentence, X1 + Offset.X, X2 + Offset.X, Y + Offset.Y, Alpha )
  else
    pText.PlotTextBlock( Sentence, X1 + Offset.X, X2 + Offset.X, Y + Offset.Y, Alpha );
end;

procedure TCreation.PlotTextCentered( const DX : IDirectDrawSurface; const Sentence : string; const X1, X2, Y, Alpha : Integer; Const UseSmallFnt: Boolean = False );
begin
  if UseSmallFnt then
    pText.PlotGoldTextCentered( DX, Sentence, X1 + Offset.X, X2 + Offset.X, Y + Offset.Y, Alpha )
  else
    pText.PlotTextCentered( Sentence, X1 + Offset.X, X2 + Offset.X, Y + Offset.Y, Alpha );
end;

//TCreation.Paint;

procedure TCreation.CharCreationDraw(Sender: TObject);
var
  ddsd : TDDSurfaceDesc;
  Bits : BITPLANE;
  R : TRect;
  Frame : Integer;
const
  FailName : string = 'Main.CharCreationDraw';
begin
  // Drawing Actor/Player
  Log.DebugLog(FailName);
  try

    Frame := Spinner div 70 + 20;
    ddsd.dwSize := SizeOf( ddsd );
    R := ApplyOffset( Rect( 114, 93, 262, 223 ) );
    if lpDDSBack.Lock( @R, ddsd, DDLOCK_WAIT, 0 ) = DD_OK then
    begin
      try
        Bits.bitsPtr := ddsd.lpSurface;
        Bits.bitsWdh := ResWidth;
        Bits.bitsHgh := ResHeight;
        Bits.bitsFmt := dfx_pixelformat;
        Bits.bitsPitch := ddsd.lPitch;
        Bits.BaseX := 0;
        Bits.BaseY := 0;

        TCharacterResource( Player.Resource ).NakedResource.RLE.Draw( Frame, 0, 0, @Bits );
        if Assigned( TCreation( Sender ).SelectedPants ) and
          Assigned( TCreation( Sender ).SelectedPants.Resource ) and
          Assigned( TLayerResource( TCreation( Sender ).SelectedPants.Resource ).RLE ) then
          TLayerResource( TCreation( Sender ).SelectedPants.Resource ).RLE.Draw( Frame, 0, 0, @Bits );
        if Assigned( Player.Equipment[ slBoot ] ) and
          Assigned( Player.Equipment[ slBoot ].Resource ) and
          Assigned( TLayerResource( Player.Equipment[ slBoot ].Resource ).RLE ) then
          TLayerResource( Player.Equipment[ slBoot ].Resource ).RLE.Draw( Frame, 0, 0, @Bits );
        if Assigned( TCreation( Sender ).SelectedShirt ) and
          Assigned( TCreation( Sender ).SelectedShirt.Resource ) and
          Assigned( TLayerResource( TCreation( Sender ).SelectedShirt.Resource ).RLE ) then
          TLayerResource( TCreation( Sender ).SelectedShirt.Resource ).RLE.Draw( Frame, 0, 0, @Bits );
        if Assigned( TCreation( Sender ).SelectedHair ) and
          Assigned( TLayerResource( TCreation( Sender ).SelectedHair ).RLE ) then
          TLayerResource( TCreation( Sender ).SelectedHair ).RLE.Draw( Frame, 0, 0, @Bits );
      finally
        lpDDSBack.Unlock( nil );
      end;
    end;
    Inc( Spinner );
    if Spinner >= 280 then
      Spinner := 0;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TCreation.CreateCollisionRects;
var
  i : integer;
  LineHeight : integer;

const
  FailName : string = 'TCreation.CreateCollisonRects';
begin
  Log.DebugLog( FailName );
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
      ArrowRect[ i ].rect.Offset(Offset);
      ArrowRect[ i + 8 ].rect.Offset(Offset);
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

    for i := 0 to 17 do
      InfoRect[ i ].rect.Offset(Offset);

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

    for i := 0 to 20 do
      SelectRect[ i ].rect.Offset(Offset);
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TCreation.LoadNames;
const
  FailName : string = 'TCreation.LoadNames';
begin
  Log.DebugLog( FailName );
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
  Log.DebugLog( FailName );
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
  i, x, Alpha : integer;
const
  FailName : string = 'TCreation.ShowStats';
begin
  Log.DebugLog( FailName );
  try
    Alpha := 240; //blend value
    x := 167 + Modifier;
    str( Character.TrainingPoints, a );
    PlotText( a, x, 213, Alpha );
   //primary stats column
    i := 239;
    str( Character.Strength, b );
    PlotText( b, x, i, Alpha );
    i := i + 24;

    str( Character.Coordination, b );
    PlotText( b, x, i, Alpha );
    i := i + 24;

    str( Character.Constitution, b );
    PlotText( b, x, i, Alpha );
    i := i + 24;

    str( Character.BasePerception, b );
    PlotText( b, x, i, Alpha );
    i := i + 24;

    str( Character.Charm, b );
    PlotText( b, x, i, Alpha );
    i := i + 24;

    str( Character.Mysticism, b );
    PlotText( b, x, i, Alpha );
    i := i + 24;

    str( Character.Combat, b );
    PlotText( b, x, i, Alpha );
    i := i + 24;

    str( Character.Stealth, b );
    PlotText( b, x, i, Alpha );

  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TCreation.ShowStats

procedure TCreation.KeyDown( Sender : TObject; var key : Word; Shift : TShiftState );
var
  i : integer;
  a : string;
  pr : TRect;
const
  FailName : string = 'TCreation.Keydown';
begin
  Log.DebugLog( FailName );
  try
     //DebugPlot(Key);
    pr := Rect( 300, 92, 448, 120 );
    lpDDSBack.BltFast( 300 + Offset.X, 92 + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
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
    PlotText( CharacterName, 310, 95, 240 );
     //plot the Carat
    PlotText( '|', CaratPosition + 310, 95, 240 );
    SoAOS_DX_BltFront;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TCreation.KeyDown

procedure TCreation.CaratTimerEvent( Sender : TObject );
const
  FailName : string = 'TCreation.CaratTimerEvent';
var
  pr : TRect;
begin
  Log.DebugLog( FailName );
  try
    pr := Rect( 300, 92, 448, 120 );
    lpDDSBack.BltFast( 300 + Offset.X, 92 + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
    inc( LoopCounter );
    if LoopCounter >= 5 then
    begin
      CaratVisible := ( CaratVisible = false );
      LoopCounter := 0;
    end;
    if CaratVisible then
    begin
      PlotText( '|', CaratPosition + 310, 95, 240 );
    end;
    PlotText( CharacterName, 310, 95, 240 );
    DrawNewPlayer;
//    lpDDSFront.Flip(nil, DDFLIP_WAIT);
//    lpDDSBack.BltFast(0, 0, lpDDSFront, Rect(0, 0, 800, 600), DDBLTFAST_WAIT);
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TCreation.CaratTimerEvent

function TCreation.ApplyOffset(r: TRect): TRect;
begin
  Result := r;
  Result.Offset(Offset);
end;

procedure TCreation.OpenBox( box : integer );
var
  i : integer;
  pr : TRect;
const
  FailName : string = 'TCreation.OpenBox';
begin
  Log.DebugLog( FailName );
  try

   //clean up any dimmed text
    pr := Rect( 114, 237, 261, 439 );
    lpDDSBack.BltFast( pr.Left + Offset.x, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
   //clean up after old boxes
    pr := Rect( 279, 239, 402, 588 );
    lpDDSBack.BltFast( pr.Left + Offset.x, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
    if BoxOpen = 17 then
    begin
      pr := Rect( 465, 59, 465 + 123, 59 + 180 );
      lpDDSBack.BltFast( pr.Left + Offset.x, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
    end;
   //clear the selectable rects
    for i := 0 to 20 do
      SelectRect[ i ].rect := Rect( -100, 0, -50, 10 );

    if box = 17 then
    begin
      pr := Rect( 490, 160, 682, 430 );
      lpDDSBack.BltFast( pr.Left + Offset.x, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
      pr := Rect( 0, 0, 123, 180 );
      lpDDSBack.BltFast( 465 + Offset.X, 59 + Offset.Y, DXBox, @pr, DDBLTFAST_WAIT );
      pr := Rect( 0, 0, 96, 21 );
      lpDDSBack.BltFast( 465 + 13 + Offset.X, 69 + 38 + 24 * ( SelectedTraining - 18 ) + Offset.Y, DXCircle, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      PlotTextCentered( lpDDSBack, txtMessage[ 5 ], 465, 465 + 123, 69 + 38, 240, UseSmallFont );
      PlotTextCentered( lpDDSBack, txtMessage[ 6 ], 465, 465 + 123, 69 + 62, 240, UseSmallFont );
      PlotTextCentered( lpDDSBack, txtMessage[ 7 ], 465, 465 + 123, 69 + 86, 240, UseSmallFont );
      for i := 0 to 2 do
        SelectRect[ 18 + i ].rect := ApplyOffset( Rect( 465, 69 + 38 + 24 * i, 465 + 123, 69 + 38 + 24 + 24 * i ) );
    end
    else
    begin
      for i := 12 to 16 do
      begin
        if i <> box then
          DrawAlpha( lpDDSBack, Rect( InfoRect[ i ].rect.left, InfoRect[ i ].rect.top, InfoRect[ i ].rect.right - 20, InfoRect[ i ].rect.bottom ), rect( 0, 0, 25, 25 ), DXBlack, False, 200 );
      end;
     //plot box
      pr := Rect( 0, 0, 123, 180 );
      lpDDSBack.BltFast( 279 + Offset.X, 239 + ( box - 12 ) * 42 + Offset.Y, DXBox, @pr, DDBLTFAST_WAIT );
      if ( Box = 12 ) or ( Box = 13 ) then
      begin
        //now showselected shirt or pants
        pr := Rect( 0, 0, 96, 21 );
        if Box = 12 then
          lpDDSBack.BltFast( 279 + 13 + Offset.X, 239 + ( box - 12 ) * 42 + 34 + 24 * ixSelectedShirt + Offset.Y, DXCircle, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT )
        else
          lpDDSBack.BltFast( 279 + 13 + Offset.X, 239 + ( box - 12 ) * 42 + 34 + 24 * ( ixSelectedPants - 4 ) + Offset.Y, DXCircle, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

        PlotTextCentered( nil, txtMessage[ 12 ], 279, 279 + 123, 239 + ( box - 12 ) * 42 + 34, 240 );
        PlotTextCentered( nil, txtMessage[ 13 ], 279, 279 + 123, 239 + ( box - 12 ) * 42 + 58, 240 );
        PlotTextCentered( nil, txtMessage[ 14 ], 279, 279 + 123, 239 + ( box - 12 ) * 42 + 82, 240 );
        PlotTextCentered( nil, txtMessage[ 15 ], 279, 279 + 123, 239 + ( box - 12 ) * 42 + 106, 240 );
        //set coll rects
        for i := 0 to 3 do
          SelectRect[ ( Box - 12 ) * 4 + i ].rect := ApplyOffset( Rect( 279, 239 + ( box - 12 ) * 42 + 34 + 24 * i, 279 + 123, 239 + ( box - 12 ) * 42 + 34 + 24 + 24 * i ) );
      end
      else if box = 14 then
      begin
        pr := Rect( 0, 0, 96, 21 );
        lpDDSBack.BltFast( 279 + 13 + Offset.X, 239 + ( box - 12 ) * 42 + 34 + 24 * ( ixSelectedHair - 8 ) + Offset.Y, DXCircle, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        PlotTextCentered( nil, txtMessage[ 16 ], 279, 279 + 123, 239 + ( box - 12 ) * 42 + 34, 240 );
        PlotTextCentered( nil, txtMessage[ 17 ], 279, 279 + 123, 239 + ( box - 12 ) * 42 + 58, 240 );
        PlotTextCentered( nil, txtMessage[ 18 ], 279, 279 + 123, 239 + ( box - 12 ) * 42 + 82, 240 );
        PlotTextCentered( nil, txtMessage[ 19 ], 279, 279 + 123, 239 + ( box - 12 ) * 42 + 106, 240 );
        for i := 0 to 3 do
          SelectRect[ ( Box - 12 ) * 4 + i ].rect := ApplyOffset( Rect( 279, 239 + ( box - 12 ) * 42 + 34 + 24 * i, 279 + 123, 239 + ( box - 12 ) * 42 + 34 + 24 + 24 * i ) );
      end
      else if box = 15 then
      begin
        pr := Rect( 0, 0, 96, 21 );
        lpDDSBack.BltFast( 279 + 13 + Offset.X, 239 + ( box - 12 ) * 42 + 34 + 24 * ( ixSelectedHairStyle - 12 ) + Offset.Y, DXCircle, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        PlotTextCentered( nil, txtMessage[ 20 ], 279, 279 + 123, 239 + ( box - 12 ) * 42 + 34, 240 );
        PlotTextCentered( nil, txtMessage[ 21 ], 279, 279 + 123, 239 + ( box - 12 ) * 42 + 58, 240 );
        PlotTextCentered( nil, txtMessage[ 22 ], 279, 279 + 123, 239 + ( box - 12 ) * 42 + 82, 240 );
        PlotTextCentered( nil, txtMessage[ 23 ], 279, 279 + 123, 239 + ( box - 12 ) * 42 + 106, 240 );
        for i := 0 to 3 do
          SelectRect[ ( Box - 12 ) * 4 + i ].rect := ApplyOffset( Rect( 279, 239 + ( box - 12 ) * 42 + 34 + 24 * i, 279 + 123, 239 + ( box - 12 ) * 42 + 34 + 24 + 24 * i ) );
      end
      else if box = 16 then
      begin
        pr := Rect( 0, 0, 96, 21 );
        lpDDSBack.BltFast( 279 + 13 + Offset.X, 239 + ( box - 12 ) * 42 + 34 + 24 * ( ixSelectedBeard - 16 ) + 24 + Offset.Y, DXCircle, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        PlotTextCentered( nil, txtMessage[ 24 ], 279, 279 + 123, 239 + ( box - 12 ) * 42 + 58, 240 );
        PlotTextCentered( nil, txtMessage[ 25 ], 279, 279 + 123, 239 + ( box - 12 ) * 42 + 82, 240 );
        for i := 0 to 1 do
          SelectRect[ ( Box - 12 ) * 4 + i ].Rect := ApplyOffset( Rect( 279, 239 + ( box - 12 ) * 42 + 34 + 24 * i + 24, 279 + 123, 239 + ( box - 12 ) * 42 + 34 + 24 + 24 * i + 24 ) );
      end;

    end; //ednif

    BoxOpen := box;
    SoAOS_DX_BltFront;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //OpenBox

procedure TCreation.DrawNewPlayer;
const
  FailName : string = 'TCreation.DrawtheGuy';
var
  pr : TRect;
begin
  Log.DebugLog( FailName );
  try
    pr := Rect( 110, 93, 264, 227 );
    lpDDSBack.BltFast(  pr.Left + Offset.x, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
    SelectedShirt := shirt[ ixSelectedShirt + 1 ];
    SelectedPants := pants[ ( ixSelectedPants - 4 ) + 1 ];
    SelectedHair := hair[ ( ixSelectedHair - 8 ) + 1, ( ixSelectedHairStyle - 12 ) + 1, ( ixSelectedBeard - 16 ) + 1 ];
    if assigned( OnDraw ) then
      OnDraw( self );
    SoAOS_DX_BltFront;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //DrawTheGuy

procedure TCreation.FormMouseDown( Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer );
const
  FailName : string = 'TCreation.FromMouseDown';
var
  pr, r1, r2 : TRect;
begin
  Log.DebugLog( FailName );
  try
    r1 := Rect( 318, 553, 359, 577 );
    r1.Offset(Offset);
    r2 := Rect( 318, 510, 359, 537 );
    r2.Offset(Offset);
    if r1.Contains( Point( x, y ) ) or r2.Contains( Point( x, y ) ) then
    begin
       //clean up any dimmed text
      pr := Rect( 114, 237, 261, 439 );
      lpDDSBack.BltFast( pr.Left + Offset.x, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
       //clean up after old boxes
      pr := Rect( 279, 239, 402, 588 );
      lpDDSBack.BltFast( pr.Left + Offset.x, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
      pr := Rect( 465, 59, 465 + 123, 59 + 181 );
      lpDDSBack.BltFast( pr.Left + Offset.x, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
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
var
  pr : TRect;
begin
  Log.DebugLog( FailName );
  try
    //Clean up continue and cancel
    pr := DlgRect.dlgNewContinueRect;
    lpDDSBack.BltFast( ContinueRect.Left, ContinueRect.Top, DXBack, @pr, DDBLTFAST_WAIT );
    if BoxOpen = -1 then
    begin
      pr := DlgRect.dlgNewCancelRect;
      lpDDSBack.BltFast( CancelRect.Left, CancelRect.Top, DXBack, @pr, DDBLTFAST_WAIT );
    end;
   //clear text
    if ContinueRect.Contains( Point( X, Y ) ) then
    begin //over continue
      //dont clear if over continue, we might have the you must enter name text up -kludgy
    end
    else
    begin
      if BoxOpen = 17 then
      begin
        pr := Rect( 490, 239, 682, 430 );
        lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end
      else
      begin
        pr := Rect( 490, 160, 682, 430 );
        lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

function TCreation.GetCancelRect: TRect;
begin
  Result := DlgRect.dlgNewCancelRect;
  Result.Offset(Offset);
end;

function TCreation.GetContinueRect: TRect;
begin
  Result := DlgRect.dlgNewContinueRect;
  Result.Offset(Offset);
end;

end.
