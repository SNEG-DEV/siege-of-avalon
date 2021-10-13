unit Inventory;
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
  DXUtil,
  DXEffects,
  Winapi.Windows,
  System.SysUtils,
  System.Types,
  System.Classes,
  System.Generics.Collections,
  Vcl.Controls,
  Character,
  Resource,
  GameText,
  Scroll,
  Engine,
  SoAOS.Animation,
  SoAOS.Intrface.Dialogs,
  Logfile;

type
  BodySlotCoord = record
    cx, cy : Integer; //center x and y coords
    cr : Integer; //radius out to edge of circle
    UsedBy : Integer; //index of any item in this slot
    Rect : TRect; //rectangle around circle
  end;

  pTempItems = ^TempCharItems;
  TempCharItems = record
    PItem : TItem; //pointer to the item
    InvX : Integer;
    InvY : Integer;
    W : Integer;
    H : Integer;
    //IW: integer; //icon width
    //IH: integer; //icon height
    ItemType : string; //Inventory or Equipment
    BodySlot : Integer; //where on the body is this piece?
    CharacterHadThisOnHim : boolean; //Was this on the character when he arrived?
    DXSurface : IDirectDrawSurface; //barbie graphic surface
    DXSurfaceIcon : IDirectDrawSurface; //icon graphic surface
    DXShadow : IDirectDrawSurface; //The shadow
    function Rect0 : TRect;
    function InvRect : TRect;
  end;

  TInventory = class( TDialog )
  private
    ItemList : TList<pTempItems>; //the list of items
    pInventoryItem : pTempItems; //The temporary inventory and equipment items combined
    CurrentSelectedItem : Integer; //Current Item being dragged about
    Tx, Ty : Integer; // x and y locs used with the offset of the dragged item
    DontPlotCurrentItem : boolean;
    DXBack : IDirectDrawSurface; //DD surface that holds the inventory screen before blit
    DxDirty : IDirectDrawSurface; //DD for cleanup when dragging items
    DXCircle : IDirectDrawSurface; //circle used for outline
    DXLeftArrow : IDirectDrawSurface; //Inventory left arrow
    DXRightArrow : IDirectDrawSurface; //Inventory right arrow
    DXBackToGame : IDirectDrawSurface; //Back To Game highlight
    DXBrown : IDirectDrawSurface; //Show where we can drop item in inventory
    SlotName : TStringList; //names of each slot we plot on screen
    SlotCoord : array[ 0..20 ] of BodySlotCoord; //coordinates used to generate rects
    GroundOrderList : TList<pTempItems>; //used to keep track of the order of items on the ground
    TopGroundIndex : Integer; //Index of the current top ground item
    Alpha : integer;
    DlgScroll : TScroll; //the statistics scroll;
    ShadowAlpha : integer;
    CheckForGroundDrop : boolean;
    ErrorCode : integer; //used for weapon drop, shiedl drop error messages
    PlotArray : array[ 0..11, 0..13 ] of integer; //array to show open slots; drop items
    //New language stuff
    txtCrowns : string;
    txtMessage : array[ 0..9 ] of string;
    //End New language Stuff
    procedure ContainCursor( Action : integer ); //lock cursor to screen during item drag
    function CollisionCheck( X, Y : Integer ) : Boolean;
    function GetSlotText : string;
    function TryToDropOnBody( X, Y : Integer ) : Integer;
    procedure WriteTheInventoryData;
    procedure ShowLegalSlots( Index : Integer ); //draw the red circle around legal slots
    procedure CleanUpLegalSlots; //clean up those red circles
    procedure WeaponPlacement( ItemIndex : integer ); //avoid collision between the weapon and other items on barbie
    procedure ShowOpenInventorySlots;
//    procedure DebugPlot(i: integer);
    function DropAnItem( X, Y : integer ) : boolean;
  protected
    procedure MouseDown( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer ); override;
    procedure MouseMove( Sender : TObject;
      Shift : TShiftState; X, Y, GridX, GridY : Integer ); override;
    procedure MouseUp( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer ); override;
    procedure MouseWheel( Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean );
  public
    Character : TCharacter;
    GroundList : TList<TItem>;
    Locked : Boolean;
    DrawGuy : TNotifyEvent;
    constructor Create;
    destructor Destroy; override;
    procedure Paint; override;
    procedure Init; override;
    procedure Release; override;
  end;

implementation

uses
  SoAOS.Types,
  SoAOS.Intrface.Text,
  SoAOS.Graphics.Draw,
  AniDemo;

{ TInventory }

const
  LrgMsg = 437;
  SmlMsg = 423;
  ClearTop = SmlMsg;
  ClearBottom = 476;
  ClearLeft = 20;
  ClearRight = 595;

constructor TInventory.Create;
var
  i : Integer;
const
  FailName : string = 'TInventory.Create';
begin
  Log.DebugLog(FailName);
  try
    inherited;
  //TSlot = (slHead,slUpperBody,slLowerBody,slFeet,slHands,slRFinger,slLFinger,slBelt,slNeck,slWeapon,slShield,slMisc1,slMisc2);

    SlotName := TStringList.Create;

    SlotName.Add( 'Legs1' );
    SlotCoord[ 0 ].cx := 383;
    SlotCoord[ 0 ].cy := 373;
    SlotCoord[ 0 ].cr := 30;

    SlotName.Add( 'Feet' );
    SlotCoord[ 1 ].cx := 449;
    SlotCoord[ 1 ].cy := 375;
    SlotCoord[ 1 ].cr := 40;

    SlotName.Add( 'Legs2' );
    SlotCoord[ 2 ].cx := 513;
    SlotCoord[ 2 ].cy := 375;
    SlotCoord[ 2 ].cr := 30;

    SlotName.Add( 'Chest 1' );
    SlotCoord[ 3 ].cx := 298;
    SlotCoord[ 3 ].cy := 266;
    SlotCoord[ 3 ].cr := 33;

    SlotName.Add( 'Chest 2' );
    SlotCoord[ 4 ].cx := 603;
    SlotCoord[ 4 ].cy := 266;
    SlotCoord[ 4 ].cr := 33;

    SlotName.Add( 'Arm' );
    SlotCoord[ 5 ].cx := 553;
    SlotCoord[ 5 ].cy := 121;
    SlotCoord[ 5 ].cr := 45;

    SlotName.Add( 'Belt' );
    SlotCoord[ 6 ].cx := 587;
    SlotCoord[ 6 ].cy := 320;
    SlotCoord[ 6 ].cr := 25;

    SlotName.Add( 'Chest 3' );
    SlotCoord[ 7 ].cx := 300;
    SlotCoord[ 7 ].cy := 195;
    SlotCoord[ 7 ].cr := 40;

    SlotName.Add( 'Gauntlet' );
    SlotCoord[ 8 ].cx := 611;
    SlotCoord[ 8 ].cy := 121;
    SlotCoord[ 8 ].cr := 30;

    SlotName.Add( 'Outer' );
    SlotCoord[ 9 ].cx := 599;
    SlotCoord[ 9 ].cy := 195;
    SlotCoord[ 9 ].cr := 40;

    SlotName.Add( 'Head' );
    SlotCoord[ 10 ].cx := 450;
    SlotCoord[ 10 ].cy := 54;
    SlotCoord[ 10 ].cr := 30;

    SlotName.Add( 'Weapon' );
    SlotCoord[ 11 ].cx := 379;
    SlotCoord[ 11 ].cy := 194;
    SlotCoord[ 11 ].cr := 200;

    SlotName.Add( 'Shield' );
    SlotCoord[ 12 ].cx := 520;
    SlotCoord[ 12 ].cy := 194;
    SlotCoord[ 12 ].cr := 200;

    SlotName.Add( 'tabar' );
    SlotCoord[ 13 ].cx := 450;
    SlotCoord[ 13 ].cy := 125;
    SlotCoord[ 13 ].cr := 40;

    SlotName.Add( 'Coif' );
    SlotCoord[ 14 ].cx := 395;
    SlotCoord[ 14 ].cy := 85;
    SlotCoord[ 14 ].cr := 25;

    SlotName.Add( 'Healthpois' );
    SlotCoord[ 15 ].cx := 570;
    SlotCoord[ 15 ].cy := 40;
    SlotCoord[ 15 ].cr := 21;

    SlotName.Add( 'ManaPois' );
    SlotCoord[ 16 ].cx := 620;
    SlotCoord[ 16 ].cy := 40;
    SlotCoord[ 16 ].cr := 21;

    SlotName.Add( 'Misc1' );
    SlotCoord[ 17 ].cx := 343;
    SlotCoord[ 17 ].cy := 103;
    SlotCoord[ 17 ].cr := 21;

    SlotName.Add( 'Misc2' );
    SlotCoord[ 18 ].cx := 321;
    SlotCoord[ 18 ].cy := 138;
    SlotCoord[ 18 ].cr := 21;
    //Tatt to Slot 19 because in the old code Slot 19 was buggy and always open
    SlotName.Add( 'Tatt' );
    SlotCoord[ 19 ].cx := 0;
    SlotCoord[ 19 ].cy := 0;
    SlotCoord[ 19 ].cr := 0;

    SlotName.Add( 'Misc3' );
    SlotCoord[ 20 ].cx := 363;
    SlotCoord[ 20 ].cy := 138;
    SlotCoord[ 20 ].cr := 21;

    for i := 0 to 20 do
    begin //set up the collision rects
      SlotCoord[ i ].Rect.Left := SlotCoord[ i ].cx - SlotCoord[ i ].cr;
      SlotCoord[ i ].Rect.Right := SlotCoord[ i ].cx + SlotCoord[ i ].cr;
      SlotCoord[ i ].Rect.Top := SlotCoord[ i ].cy - SlotCoord[ i ].cr;
      SlotCoord[ i ].Rect.Bottom := SlotCoord[ i ].cy + SlotCoord[ i ].cr;
      SlotCoord[ i ].UsedBy := -1;
    end;
  //Create the GroundList
    GroundList := TList<TItem>.Create;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

destructor TInventory.Destroy;
const
  FailName : string = 'TInventory.Destroy';
begin
  Log.DebugLog(FailName);
  try

    GroundList.Free;
    GroundList := nil;
    SlotName.Free;
    SlotName := nil;
    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TInventory.Init;
var
  i, width, height : Integer;
  t : TSlot; //Index for Equipment loop
  DXBorder : IDirectDrawSurface;
  WeHaveAWeaponAndThisIsTheIndex : integer; //for moving the weapon to a clear spot on barbie
  GreatestWidth, GreatestHeight : integer; //used to create the dirty rect surface
  pr : TRect;
const
  FailName : string = 'TInventory.init';
begin
  Log.DebugLog(FailName);
  try
    if Loaded then
      Exit;
    inherited;

    frmMain.OnMouseWheel := MouseWheel;

    MouseCursor.Cleanup;

    pr := Rect( 0, 0, ResWidth, ResHeight );
    lpDDSBack.BltFast( 0, 0, lpDDSFront, @pr, DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;

    ExText.Open( 'Inventory' );
    txtCrowns := ExText.GetText( 'Crowns' );
    txtMessage[ 0 ] := ExText.GetText( 'Message0' );
    txtMessage[ 1 ] := ExText.GetText( 'Message1' );
    txtMessage[ 2 ] := ExText.GetText( 'Message2' );
    txtMessage[ 3 ] := ExText.GetText( 'Message3' );
    txtMessage[ 4 ] := ExText.GetText( 'Message4' );
    txtMessage[ 5 ] := ExText.GetText( 'Message5' );
    txtMessage[ 6 ] := ExText.GetText( 'Message6' );
    txtMessage[ 7 ] := ExText.GetText( 'Message7' );
    txtMessage[ 8 ] := ExText.GetText( 'Message8' );
    txtMessage[ 9 ] := ExText.GetText( 'Message9' );

    CheckForGroundDrop := false;
    ShadowAlpha := 150; //Alpha for the Shadows -eaiser to change in one place
    ErrorCode := -1;
    DontPlotCurrentItem := false;
    DlgScroll := TScroll.create; //create the statistics scroll
    DlgScroll.pText := pText; //assign the pointer to pText;
    pText.LoadFontGraphic( 'inventory' ); //load the inventory font graphic in
    pText.LoadTinyFontGraphic;
    CurrentSelectedItem := -1; //We aren't dragging anything
    DlgScroll.ScrollIsShowing := False; //stats screen isnt showing
    Alpha := 170; //alpha value for all alphabet plots
    TopGroundIndex := 0; //initialize the top ground item to element zero of GroundList
  //Create list
    ItemList := TList<pTempItems>.Create; //create the ItemList
  //ItemList.Sorted := True;              //Indicate that it is always to be sorted
  //ItemList.Duplicates := dupAccept;     //Allow duplicates
    GroundOrderList := TList<pTempItems>.Create;
  //Clear this list
    for i := ord( slLeg1 ) to ord( slMisc3 ) do
      SlotCoord[ i ].UsedBy := -1;

  //Load path info, coords into temp objects from the Character's Inventory
    for i := 0 to Character.Inventory.Count - 1 do
    begin
      New( pInventoryItem );
      pInventoryItem.PItem := Character.Inventory.Items[ i ];
      pInventoryItem.InvX := TItem( Character.Inventory.Items[ i ] ).InvX * 18 + 20;
      pInventoryItem.InvY := TItem( Character.Inventory.Items[ i ] ).InvY * 26 + 57;
      pInventoryItem.ItemType := 'Inventory';
      pInventoryItem.CharacterHadThisOnHim := true;
      pInventoryItem.BodySlot := -1; //not anywhere on body
      pInventoryItem.W := 0;
      pInventoryItem.H := 0;
      pInventoryItem.DXSurface := nil;
      pInventoryItem.DXSurfaceIcon := nil;
      pInventoryItem.DXShadow := nil;

      ItemList.Add( pInventoryItem );
    //j := ItemList.Add(pInventoryItem.PItem.FileName);
    //ItemList.Items[j] := TObject(pInventoryItem);
    end;
  //Load path info, coords into temp objects from the Character's Equipment list
    WeHaveAWeaponAndThisIsTheIndex := -1;
    for t := slLeg1 to slMisc3 do
    begin
      if Assigned( Character.Equipment[ t ] ) then
      begin
        New( pInventoryItem );
        pInventoryItem.PItem := Character.Equipment[ t ];
        pInventoryItem.InvX := SlotCoord[ integer( t ) ].cx - ( ( TItem( Character.Equipment[ t ] ).InvW * 18 ) div 2 );
        if ( t = slWeapon ) or ( t = slShield ) then //weapons and shields 'hang' from the centerpoint - 25% up
          pInventoryItem.InvY := SlotCoord[ integer( t ) ].cy - ( pInventoryItem.pItem.InvH * 26 ) div 4
        else //All other are centered
          pInventoryItem.InvY := SlotCoord[ integer( t ) ].cy - ( ( TItem( Character.Equipment[ t ] ).InvH * 26 ) div 2 );
        pInventoryItem.BodySlot := Integer( t ); //TItem(Character.Equipment[t]).BodySlot;
        pInventoryItem.ItemType := 'Equipment';
        pInventoryItem.CharacterHadThisOnHim := true;
        pInventoryItem.W := 0;
        pInventoryItem.H := 0;
        pInventoryItem.DXSurface := nil;
        pInventoryItem.DXSurfaceIcon := nil;
        pInventoryItem.DXShadow := nil;
        i := ItemList.Add( pInventoryItem );
        if ( t = slWeapon ) then //we move the weapon about to avoid collisions with other items-they tend to be big
          WeHaveAWeaponAndThisIsTheIndex := i
      end;
    end;

  //Load Ground Items
  //Ground items make use of a Tlist to keep track of the order that they lie on the ground.
  //This StringList is sorted by filename, so this order TList is necessary, otherwise when the user dropped
  //items on the ground they would be out of dropped order if he went to pick them up- very disconcerting
    for i := 0 to GroundList.Count - 1 do
    begin
      New( pInventoryItem );
      pInventoryItem.PItem := GroundList[ i ];
      if i = 0 then
      begin
        pInventoryItem.InvX := 277; //315-pInventoryItem.pItem.width div 2;       //Only the first ground item is visible
        pInventoryItem.InvY := 386; //401-pInventoryItem.pItem.height div 2;
      end
      else
      begin
        pInventoryItem.InvX := 999; //set it offscreen so we dont see it
        pInventoryItem.InvY := 1099; //war 999, bei FullHD sonst nicht außerhalb
      end;
      pInventoryItem.ItemType := 'Ground';
      pInventoryItem.CharacterHadThisOnHim := False;
      pInventoryItem.BodySlot := -1; //not anywhere on body
      pInventoryItem.W := 0;
      pInventoryItem.H := 0;
      pInventoryItem.DXSurface := nil;
      pInventoryItem.DXSurfaceIcon := nil;
      pInventoryItem.DXShadow := nil;
      ItemList.Add( pInventoryItem );
    //j := ItemList.Add(pInventoryItem.PItem.FileName); //not sure what to do about Iconicfilename
    //ItemList.Items[j] := TObject(pInventoryItem);
      GroundOrderList.Add( pInventoryItem ); //Now we create our order list
    end;
  //Now I have to cycle through and find the positions in the list of the equipped item for my bodyslot array
  //It must be initialized, and we can't get the true index of the object until the entire list is loaded, because
  //those indexes change due to the fact that it is a sorted stringList.  Ow.
    for i := 0 to ItemList.Count - 1 do
    begin
      if ItemList[ i ].BodySlot > -1 then
        SlotCoord[ ItemList[ i ].BodySlot ].UsedBy := i;
    end;
  //Get the barbie pics for all the items, as well as the GroundIcons and Shadows
    GreatestWidth := cGroundListWidth; //we inot to ground list size - must be at least this big
    GreatestHeight := cGroundListHeight;
    for i := 0 to ItemList.Count - 1 do
    begin
      ItemList[ i ].DXSurface := ItemList[ i ].pItem.GetInventoryImage;
      ItemList[ i ].DXShadow := ItemList[ i ].pItem.GetInventoryShadow;
      ItemList[ i ].DXSurfaceIcon := ItemList[ i ].pItem.GetIconicImage;
      //pTempItems(ItemList.Items[i]).IW := pTempItems(ItemList.Items[i]).pItem.width; //icon width
      //pTempItems(ItemList.Items[i]).IH := pTempItems(ItemList.Items[i]).pItem.height;//icon height
      ItemList[ i ].W := ItemList[ i ].pItem.InvW * 18;
      ItemList[ i ].H := ItemList[ i ].pItem.InvH * 26;
      //get the Greatest barbie pic Width and Height so we know what size to make the dirty surface
      if ItemList[ i ].W > GreatestWidth then
        GreatestWidth := ItemList[ i ].W;
      if ItemList[ i ].H > GreatestHeight then
        GreatestHeight := ItemList[ i ].H;
      //New 9-07
      if not assigned( ItemList[ i ].DXSurface ) then
      begin
        ItemList[ i ].InvX := -100;
        ItemList[ i ].InvY := -100;
        ItemList[ i ].W := 10;
        ItemList[ i ].H := 10;
      end;
      //end new
    end;
  //Create the DirectRect fix surface
    DXDirty := DDGetSurface( lpDD, GreatestWidth, GreatestHeight, cInvisColor, true );

    if WeHaveAWeaponAndThisIsTheIndex > -1 then
    begin //If wielding a weapon, put it in a spot where it doesnt collide
      Tx := ItemList[ WeHaveAWeaponAndThisIsTheIndex ].InvX;
      Ty := ItemList[ WeHaveAWeaponAndThisIsTheIndex ].InvY;
      WeaponPlacement( WeHaveAWeaponAndThisIsTheIndex );
      ItemList[ WeHaveAWeaponAndThisIsTheIndex ].InvX := Tx;
      ItemList[ WeHaveAWeaponAndThisIsTheIndex ].InvY := Ty;
    end; //endif

  //Load the Background Bitmap and plot it
    DXRightArrow := SoAOS_DX_LoadBMP( InterfacePath + 'invRightArrow.bmp', cInvisColor );
    DXLeftArrow := SoAOS_DX_LoadBMP( InterfacePath + 'invLeftArrow.bmp', cInvisColor );
    DXBackToGame := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'invBackToGame.bmp', cInvisColor );
    DXCircle := SoAOS_DX_LoadBMP( InterfacePath + 'invRedCircle.bmp', cInvisColor );
    DXBrown := SoAOS_DX_LoadBMP( InterfacePath + 'merBackHighlight.bmp', cInvisColor );
    DXBack := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'inventory.bmp', cInvisColor, DlgWidth, DlgHeight );
    pr := Rect( 0, 0, DlgWidth, DlgHeight );
    lpDDSBack.BltFast( Offset.X, Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
  //Now for the Alpha'ed edges
    DXBorder := SoAOS_DX_LoadBMP( InterfacePath + 'invRightInventoryAlpha.bmp', cInvisColor, width, height );
    DrawSub( lpDDSBack, ApplyOffset( Rect( 659, 0, 659 + width, height ) ), Rect( 0, 0, width, height ), DXBorder, True, Alpha );

    DXBorder := nil;

    DXBorder := SoAOS_DX_LoadBMP( InterfacePath + 'invBottomInventoryAlpha.bmp', cInvisColor, width, height );
    DrawSub( lpDDSBack, ApplyOffset( Rect( 0, 460, width, 460 + height ) ), Rect( 0, 0, width, height ), DXBorder, True, Alpha );

    DXBorder := nil; //release DXBorder

    DontPlotCurrentItem := false;
    paint;

    PlotText( IntToStr( Character.money ) + ' ' + txtCrowns, 240 - pText.TextLength( IntToStr( Character.money ) + ' ' + txtCrowns ), 10, Alpha );
  //Whew! Now we flip it all to the screen
    MouseCursor.Cleanup;

    SoAOS_DX_BltFront;

  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //Tinventory.init

procedure TInventory.MouseDown( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer );
var
  i, j : Integer;
  pTemp : Pointer;
  B1, B2, B3, B4 : Boolean;
  rRect : TRect;
  DontDropInGround : boolean;
  pr : TRect;
const
  FailName : string = 'TInventory.mousedown';
begin
  try
    if CurrentSelectedItem = -1 then
    begin //if no piece is being dragged pick one up
      if DlgScroll.ScrollIsShowing then
      begin
        if PtInRect( ApplyOffset( rect( 119, 30, 119 + 443, 30 + 90 )), point( X, Y ) ) or
          PtInRect( ApplyOffset( rect( 119, 373, 119 + 443, 373 + 70 )), point( X, Y ) ) then //or PtInRect(rect(171,50,171+338,380),point(X,Y)) then
        begin
          if Y < 248 + Offset.Y then
          begin
            DlgScroll.ScrollAmount := 1;
            DlgScroll.KeepOnScrolling := true;
            DlgScroll.ScrollStatsScroll;
          end
          else
          begin
            DlgScroll.ScrollAmount := -1;
            DlgScroll.KeepOnScrolling := true;
            DlgScroll.ScrollStatsScroll;
          end
        end
        else
        begin
          DlgScroll.KeepOnScrolling := false;
          DlgScroll.ScrollIsShowing := False;
          Paint;
        end;
      end
      else if PtInRect( ApplyOffset( Rect( 595, 418, 668, 463) ), Point( x, y) ) then
      begin //they hit the back to button
      //WriteTheInventoryData;            //write the data back
        Close; //lose the screen
      end
      else if PtInRect( ApplyOffset( Rect( 259, 385, 277, 418) ), Point( x, y) ) then
      begin //left arrow for ground
        if GroundOrderList.Count > 1 then
        begin //get the prev item on the ground and show it
          j := TopGroundIndex;
          if j <> 0 then
          begin //if its not the first item in the list
          //replace the back from the DXBack buffer.
            pr := Rect( 275, 384, 353, 416 );
            lpDDSBack.BltFast( 275 + Offset.X, 384 + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
            GroundOrderList[ j ].InvX := 999;
            GroundOrderList[ j ].InvY := 1099;
            j := j - 1;
          //Set the coordinates of the new item and Plot it
            GroundOrderList[ j ].InvX := 277; //315-pTempItems(GroundOrderList.Items[j]).IW div 2;
            GroundOrderList[ j ].InvY := 386; //401-pTempItems(GroundOrderList.Items[j]).IH div 2;
            pr := Rect( 0, 0, cGroundListWidth, cGroundListHeight );
            lpDDSBack.BltFast( GroundOrderList[ j ].InvX + Offset.X, GroundOrderList[ j ].InvY + Offset.Y, GroundOrderList[ j ].DXSurfaceIcon, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
            TopGroundIndex := j;
          end
          else
          begin
          //making an obnoxious buzzing noise? - we cant go backwards from the first item
          end;
        end;
      end
      else if PtInRect( ApplyOffset( Rect( 353, 391, 365, 418) ), Point( x, y) ) then
      begin //right arrow for ground
        if GroundOrderList.Count > 1 then
        begin //get the Next item on the ground and show it
          j := TopGroundIndex;
          if j < ( GroundOrderList.Count - 1 ) then
          begin //if its not the last item in the list
          //replace the back from the DXBack buffer.
            pr := Rect( 275, 384, 353, 416 );
            lpDDSBack.BltFast( 275 + Offset.X, 384 + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
            GroundOrderList[ j ].InvX := 999;
            GroundOrderList[ j ].InvY := 1099;
            j := j + 1;
          //Set the coordinates of the new item and Plot it
            GroundOrderList[ j ].InvX := 277; //315-pTempItems(GroundOrderList.Items[j]).IW div 2;
            GroundOrderList[ j ].InvY := 386; //401-pTempItems(GroundOrderList.Items[j]).IH div 2;
            pr := Rect( 0, 0, cGroundListWidth, cGroundListHeight );
            lpDDSBack.BltFast( GroundOrderList[ j ].InvX + Offset.X, GroundOrderList[ j ].InvY + Offset.Y, GroundOrderList[ j ].DXSurfaceIcon, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
            TopGroundIndex := j;
          end
          else
          begin
          //making an obnoxious buzzing noise? - we cant go forwards from the last item
          end;
        end;
      end
      else if PtInRect( ApplyOffset( Rect( 275, 384, 353, 416) ), Point( x, y) ) and ( CurrentSelectedItem = -1 ) then
      begin //over the ground slot
        //If we are pulling this from the ground slot, pick a new top item
        if GroundOrderList.Count > 0 then
        begin
          CurrentSelectedItem := ItemList.IndexOf( GroundOrderList[ TopGroundIndex ] );
          if Button = mbRight then
          begin
            DlgScroll.OpenStatsScroll( ItemList[ CurrentSelectedItem ].pItem, Offset.X, Offset.Y );
            CurrentSelectedItem := -1;
          end
          else
          begin
            pr := Rect( 275, 384, 353, 416 );
            lpDDSBack.BltFast( 275 + Offset.X, 384 + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
            if GroundOrderList.Count > 1 then
            begin //get the next item on the ground and show it
              j := TopGroundIndex; //GroundOrderList.IndexOf(ItemList.Items[CurrentSelectedItem]);
              if ( j = ( GroundOrderList.Count - 1 ) ) then //if its the last item in the list
                j := 0 //set it to the first one
              else //set it to the item folowing this one
                j := j + 1;
              GroundOrderList[ j ].InvX := 277; //315-pTempItems(GroundOrderList.Items[j]).IW div 2;
              GroundOrderList[ j ].InvY := 386; //401-pTempItems(GroundOrderList.Items[j]).IH div 2;
              pr := Rect( 0, 0, cGroundListWidth, cGroundListHeight );
              lpDDSBack.BltFast( GroundOrderList[ j ].InvX + Offset.X, GroundOrderList[ j ].InvY + Offset.Y, GroundOrderList[ j ].DXSurfaceIcon, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
              pTemp := Pointer( GroundOrderList[ j ] ); //save the pointer to the new topmost item so we can do the delete and still track it
              GroundOrderList.Delete( TopGroundIndex ); //GroundOrderList.IndexOf(ItemList.Items[CurrentSelectedItem])); //remove this item from the GroundList pointer list
              TopGroundIndex := GroundOrderList.IndexOf( pTempItems( pTemp ) );
            end
            else
            begin
              GroundOrderList.Delete( TopGroundIndex ); //GroundOrderList.IndexOf(ItemList.Items[CurrentSelectedItem])); //remove this item from the GroundList pointer list
              TopGroundIndex := -1;
            end;
            ShowLegalSlots( CurrentSelectedItem ); //show the slots this item will fit in
            //Compute the coords for the floating item
            Tx := ( X - Offset.X ) - ItemList[ CurrentSelectedItem ].W div 2;
            Ty := ( Y - Offset.Y ) - ItemList[ CurrentSelectedItem ].H div 2;
            //Plot relevant text
            pr := Rect( ClearLeft, ClearTop, ClearRight, ClearBottom );
            lpDDSBack.BltFast( ClearLeft + Offset.X, ClearTop + Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot test
            if UseSmallFont then
              pText.PlotTinyTextBlock( GetSlotText, ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha )
            else
              PlotText( GetSlotText, ClearLeft, LrgMsg, Alpha );
            //save the background to the dirty DD surface based on the floating item
            pr := ApplyOffset( Rect( Tx, Ty, Tx + ItemList[ CurrentSelectedItem ].W, Ty + ItemList[ CurrentSelectedItem ].H ) );
            DXDirty.BltFast( 0, 0, lpDDSBack, @pr, DDBLTFAST_WAIT );
            //plot the item centered under the mouse pointer
            DrawSub( lpDDSBack, ApplyOffset( rect( Tx, Ty, Tx + ItemList[ CurrentSelectedItem ].W, Ty + ItemList[ CurrentSelectedItem ].H ) ), ItemList[ CurrentSelectedItem ].Rect0, ItemList[ CurrentSelectedItem ].DXShadow, True, ShadowAlpha );
            pr := ItemList[ CurrentSelectedItem ].Rect0;
            lpDDSBack.BltFast( Tx + Offset.X, Ty + Offset.Y, ItemList[ CurrentSelectedItem ].DXSurface, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
            ContainCursor( 1 );
          end; //if Button = mbRight
        end //if GroundOrderList > 0
      end
      else
      begin //try to pick something up
        i := 0;
        while ( ( i < ItemList.Count ) and ( CurrentSelectedItem = -1 ) and Assigned( DXBack ) ) do
        begin
        //find the item the mouse is down over
          if PtInRect( ApplyOffset( ItemList[ i ].InvRect ), point( x, y ) ) and
            ( ( ItemList[ i ].ItemType = 'Inventory' ) or ( ItemList[ i ].ItemType = 'Equipment' ) ) then
          begin
            if Button = mbRight then
            begin
              DlgScroll.OpenStatsScroll( ItemList[ i ].pItem, Offset.X, Offset.Y);
            end
            else if ( X > 250 + Offset.X ) and Locked then
            begin
              //dont let players pick up item from the body of an NPC
            end
            else if ( X > 250 + Offset.X ) and not ( ItemList[ i ].pItem.CanEquip( TSlot( ItemList[ i ].BodySlot ), Character ) ) then
            begin
              //dont let players pick up an object that is in an invalid slot
            end
            else if ( X > 250 + Offset.X ) and Character.EquipmentLocked[ TSlot( ItemList[ i ].BodySlot ) ] then
            begin
              //dont let players pick up a locked object
            end
            else
            begin
              CurrentSelectedItem := i; //Get the index of the selected item
            //replace the back from the DXBack buffer.
              DontPlotCurrentItem := true;
              Paint;
              DontPlotCurrentItem := false;
              if X > 250 + Offset.X then
              begin //If we're pulling this off of the body, then clear that slot
                SlotCoord[ ItemList[ i ].BodySlot ].UsedBy := -1;
                ItemList[ i ].BodySlot := -1; //JD 6/14/2000
              end;
              ShowLegalSlots( CurrentSelectedItem ); //show the slots this item will fit in
            //Compute the coords for the floating item
              Tx := ( X - Offset.X ) - ItemList[ i ].W div 2;
              Ty := ( Y - Offset.Y ) - ItemList[ i ].H div 2;

              if Tx < 0 then
                Tx := 0;
              if Ty < 0 then
                Ty := 0;
              if ( Tx + ItemList[ i ].W ) > 659 then
                Tx := 659 - ItemList[ i ].W;
              if ( Ty + ItemList[ i ].H ) > 463 then
                Ty := 463 - ItemList[ i ].H;

            //save the background to the dirty DD surface based on the floating item
              pr := ApplyOffset( Rect( Tx, Ty, Tx + ItemList[ i ].W, Ty + ItemList[ i ].H ) );
              DXDirty.BltFast( 0, 0, lpDDSBack, @pr, DDBLTFAST_WAIT );
            //plot the item centered under the mouse pointer
              DrawSub( lpDDSBack, ApplyOffset( rect( Tx, Ty, Tx + ItemList[ i ].W, Ty + ItemList[ i ].H ) ), ItemList[ i ].Rect0, ItemList[ i ].DXShadow, True, ShadowAlpha );
              pr := ItemList[ i ].Rect0;
              lpDDSBack.BltFast( Tx + Offset.X, Ty + Offset.Y, ItemList[ i ].DXSurface, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
              ContainCursor( 1 );
            //Plot relevant text
              pr := Rect( ClearLeft, ClearTop, ClearRight, ClearBottom );
              lpDDSBack.BltFast( ClearLeft + Offset.X, ClearTop + Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot test
              if UseSmallFont then
                pText.PlotTinyTextBlock( GetSlotText, ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha )
              else
                PlotText( GetSlotText, ClearLeft, LrgMsg, Alpha );
            end;
          end;
          i := i + 1;
        end; //wend
      end; //the if relating to the arrows
    end
    else
    begin //drop the piece if we can
    //cleanup
      pr := ItemList[ CurrentSelectedItem ].Rect0;
      lpDDSBack.BltFast( Tx + Offset.X, Ty + Offset.Y, DXDirty, @pr, DDBLTFAST_WAIT );
    //try to drop on ground
    //first, if it's the leg1 slot, we add this in to make it easier to drop items in leg1 and avoid the ground slot
      DontDropInGround := false;
      if ItemList[ CurrentSelectedItem ].PItem.CanEquip( slLeg1, Character ) then
      begin
        if PtinRect( ApplyOffset( SlotCoord[ 0 ].rect ), point( X, Y ) ) then
          DontDropInGround := true;
      end;
      if ItemList[ CurrentSelectedItem ].H > 106 then
      begin
        if Tx + ItemList[ CurrentSelectedItem ].W > 360 then
          DontDropInGround := true;
      end;
    //make sure it isn't a quest item- cant drop those
      if ItemList[ CurrentSelectedItem ].DXSurfaceIcon = nil then
      begin
        DontDropInGround := true;
        ErrorCode := 256;
      end;
      if ( ( DontDropInGround = false ) and ( intersectRect( rRect, rect( 276, 386, 353, 416 ), rect( Tx, Ty, Tx + ItemList[ CurrentSelectedItem ].W, Ty + ItemList[ CurrentSelectedItem ].H ) ) ) ) then
      begin
      //DebugPrint(IntToStr(TopGroundIndex));
        if GroundOrderList.Count > 0 then
        begin //If we have any ground items
          GroundOrderList[ TopGroundIndex ].InvX := 999; //put old item offscreen- no longer on top
          GroundOrderList[ TopGroundIndex ].InvY := 1099;
        //GroundOrderList.Insert(TopGroundIndex, pTempItems(ItemList.Items[CurrentSelectedItem]));
          j := GroundOrderList.Add( ItemList[ CurrentSelectedItem ] );
          GroundOrderList.Move( j, TopGroundIndex );
        end
        else
        begin //there are no items in this list - this will automatically become zero (top spot)
          GroundOrderList.Add( ItemList[ CurrentSelectedItem ] );
          TopGroundIndex := 0;
        end;

        pr := Rect( 276, 386, 353, 416 );
        lpDDSBack.BltFast( 276 + Offset.X, 386 + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
        ItemList[ CurrentSelectedItem ].InvX := 277; //315-pTempItems(ItemList.Items[CurrentSelectedItem]).IW div 2;
        ItemList[ CurrentSelectedItem ].InvY := 386; //401-pTempItems(ItemList.Items[CurrentSelectedItem]).IH div 2;
        ItemList[ CurrentSelectedItem ].BodySlot := -1;
        ItemList[ CurrentSelectedItem ].ItemType := 'Ground';
        pr := Rect( 0, 0, cGroundListWidth, cGroundListHeight );
        lpDDSBack.BltFast( ItemList[ CurrentSelectedItem ].InvX + Offset.X, ItemList[ CurrentSelectedItem ].InvY + Offset.Y,
          ItemList[ CurrentSelectedItem ].DXSurfaceIcon, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        pr := Rect( ClearLeft, ClearTop, ClearRight, ClearBottom );
        lpDDSBack.BltFast( ClearLeft + Offset.X, ClearTop + Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clear text
        CleanUpLegalSlots;
        CurrentSelectedItem := -1;
        ContainCursor( 0 );
      end
      else if PtInRect( ApplyOffset( Rect( 250, 0, 652, 438) ), Point( x, y) ) and ( Locked = false ) then
      begin //is the mouse over on the right (body side) of the screen
        i := TryToDropOnBody( X, Y );
        if i > -1 then
        begin //if we dropped in a slot successfully
          Tx := SlotCoord[ i ].cx - ( ItemList[ CurrentSelectedItem ].W div 2 );
          if ( i = 11 ) or ( i = 12 ) then
          begin //if Weapon or shield, they Hang from the center point
            Ty := SlotCoord[ i ].cy - ItemList[ CurrentSelectedItem ].H div 4;
            if ( i = 11 ) then
              WeaponPlacement( CurrentSelectedItem );
          end
          else //otherwise center the item in the slot
            Ty := SlotCoord[ i ].cy - ( ItemList[ CurrentSelectedItem ].H div 2 );
        //plot dropped item in slot
          DrawSub( lpDDSBack, ApplyOffset( rect( Tx, Ty, Tx + ItemList[ CurrentSelectedItem ].W, Ty + ItemList[ CurrentSelectedItem ].H ) ), ItemList[ CurrentSelectedItem ].Rect0, ItemList[ CurrentSelectedItem ].DXShadow, True, ShadowAlpha );
          pr := ItemList[ CurrentSelectedItem ].Rect0;
          lpDDSBack.BltFast( Tx + Offset.X, Ty + Offset.Y, ItemList[ CurrentSelectedItem ].DXSurface, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
          ItemList[ CurrentSelectedItem ].InvX := Tx; //save new locations
          ItemList[ CurrentSelectedItem ].InvY := Ty;
          ItemList[ CurrentSelectedItem ].BodySlot := i;
          ItemList[ CurrentSelectedItem ].ItemType := 'Equipment';
          ItemList[ CurrentSelectedItem ].CharacterHadThisOnHim := true;
          if SlotCoord[ i ].UsedBy > -1 then
          begin //item already there, pass it to the mouse as the new floater
          //assign Slot to dropped item and CurrentSelectedItem to the item we got from the slot; swap them
            j := SlotCoord[ i ].UsedBy;
            if not ( ItemList[ j ].pItem.CanEquip( TSlot( ItemList[ j ].BodySlot ), Character ) ) then
            begin
              //dont let players pick up an object that is in an invalid slot
            end
            else if Character.EquipmentLocked[ TSlot( ItemList[ j ].BodySlot ) ] then
            begin
              //dont let players pick up a locked object
            end
            else
            begin
              SlotCoord[ i ].UsedBy := CurrentSelectedItem;
              CurrentSelectedItem := j;
              ItemList[ CurrentSelectedItem ].BodySlot := -1;
              DontPlotCurrentItem := true;
              CleanUpLegalSlots; //clean up after old circles
              DontPlotCurrentItem := false;
              ShowLegalSlots( CurrentSelectedItem ); //show the slots this item will fit in
            //Compute the coords for the floating item
              Tx := ( X - Offset.X ) - ItemList[ CurrentSelectedItem ].W div 2;
              Ty := ( Y - Offset.Y ) - ItemList[ CurrentSelectedItem ].H div 2;
            //save the background to the dirty DD surface based on the floating item
              pr := ApplyOffset( Rect( Tx, Ty, Tx + ItemList[ CurrentSelectedItem ].W, Ty + ItemList[ CurrentSelectedItem ].H ) );
              DXDirty.BltFast( 0, 0, lpDDSBack, @pr, DDBLTFAST_WAIT );
            //plot the item centered under the mouse pointer
              DrawSub( lpDDSBack, ApplyOffset( rect( Tx, Ty, Tx + ItemList[ CurrentSelectedItem ].W, Ty + ItemList[ CurrentSelectedItem ].H ) ), ItemList[ CurrentSelectedItem ].Rect0, ItemList[ CurrentSelectedItem ].DXShadow, True, ShadowAlpha );
              if assigned( ItemList[ CurrentSelectedItem ].DXSurface ) then
              begin
                pr := ItemList[ CurrentSelectedItem ].Rect0;
                lpDDSBack.BltFast( Tx + Offset.X, Ty + Offset.Y, ItemList[ CurrentSelectedItem ].DXSurface, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
              end;
            // //assign Slot to dropped item and CurrentSelectedItem to the item we got from the slot; swap them
            // j:=SlotCoord[i].UsedBy;
            // SlotCoord[i].UsedBy:=CurrentSelectedItem;
            // CurrentSelectedItem:=j;
            // pTempItems(ItemList.Items[CurrentSelectedItem]).BodySlot:=-1;
              //Plot relevant text
              pr := Rect( 20, ClearTop, ClearRight, ClearBottom );
              lpDDSBack.BltFast( ClearLeft + Offset.X, ClearTop + Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot test
              if UseSmallFont then
                pText.PlotTinyTextBlock( GetSlotText, ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha )
              else
                PlotText( GetSlotText, ClearLeft, LrgMsg, Alpha );
            end;
          end
          else
          begin //The slot was empty, so - clear the CurrentSelectedItem item and clear the text
            pr := Rect( ClearLeft, ClearTop, ClearRight, ClearBottom );
            lpDDSBack.BltFast( ClearLeft + Offset.X, ClearTop + Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
            SlotCoord[ i ].UsedBy := CurrentSelectedItem;
            CleanUpLegalSlots;
            CurrentSelectedItem := -1;
            ContainCursor( 0 );
          end;
        end
        else
        begin //plot error message- illegal drop tried - might not ne necessary
           //clean up - this plots the objects dirty, then the new text, then saves the dirty - prevents Dirty errors
          pr := ItemList[ CurrentSelectedItem ].Rect0;
          lpDDSBack.BltFast( Tx + Offset.X, Ty + Offset.Y, DXDirty, @pr, DDBLTFAST_WAIT );
          pr := Rect( ClearLeft, ClearTop, ClearRight, ClearBottom );
          lpDDSBack.BltFast( ClearLeft + Offset.X, ClearTop + Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
          if UseSmallFont then
          begin
            if ErrorCode = 11 then //weapon error
              pText.PlotTinyTextBlock( txtMessage[ 0 ], ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha )
            else if ErrorCode = 12 then //shield error
              pText.PlotTinyTextBlock( txtMessage[ 1 ], ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha )
            else if ErrorCode = 255 then //Quest item drop on ground error
              pText.PlotTinyTextBlock( txtMessage[ 2 ], ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha );
          end
          else
          begin
            if ErrorCode = 11 then //weapon error
              PlotText( txtMessage[ 0 ], ClearLeft, LrgMsg, Alpha )
            else if ErrorCode = 12 then //shield error
              PlotText( txtMessage[ 1 ], ClearLeft, LrgMsg, Alpha )
            else if ErrorCode = 255 then //Quest item drop on ground error
              PlotText( txtMessage[ 2 ], ClearLeft, LrgMsg, Alpha );
          end;
          ErrorCode := -1;
          //save the background to the dirty DD surface based on the floating item
          pr := ApplyOffset( Rect( Tx, Ty, Tx + ItemList[ CurrentSelectedItem ].W, Ty + ItemList[ CurrentSelectedItem ].H ) );
          DXDirty.BltFast( 0, 0, lpDDSBack, @pr, DDBLTFAST_WAIT );
          DrawSub( lpDDSBack, ApplyOffset( rect( Tx, Ty, Tx + ItemList[ CurrentSelectedItem ].W, Ty + ItemList[ CurrentSelectedItem ].H ) ), ItemList[ CurrentSelectedItem ].Rect0, ItemList[ CurrentSelectedItem ].DXShadow, True, ShadowAlpha );
          pr := ItemList[ CurrentSelectedItem ].Rect0;
          lpDDSBack.BltFast( Tx + Offset.X, Ty + Offset.Y, ItemList[ CurrentSelectedItem ].DXSurface, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

        end;
      end
      else
      begin //check inventory side
        B1 := true; //((X - (pTempItems(ItemList.Items[CurrentSelectedItem]).W div 2) > 2) and (Y - (pTempItems(ItemList.Items[CurrentSelectedItem]).H div 2) > 35)); //is it on the grid?
        B2 := true; //(X < (237 - (pTempItems(ItemList.Items[CurrentSelectedItem]).W div 2 - 9))); //is it on the right side of the grid within 1 block?
        if not B2 then
        begin
          X := X - 18; //we redo this- added a forgivness factor for dropping on right edge, we need to move this back a slot
          B2 := ( X < ( 237 + Offset.X - ( ItemList[ CurrentSelectedItem ].W div 2 - 9 ) ) );
        end;
        B3 := true; //(Y < (421 - (pTempItems(ItemList.Items[CurrentSelectedItem]).H div 2 - 9))); //bottom side within 1 block
        if not B3 then
        begin
          Y := Y - 15; //we redo this- added a forgivness factor for dropping on right edge, we need to move this up a slot
          B3 := ( Y < ( 421 + Offset.Y - ( ItemList[ CurrentSelectedItem ].H div 2 ) ) );
        end;
        B4 := DropAnItem( X, Y ); //CollisionCheck(X, Y);       //does it collide with any other items already in inventory?
        if ( B1 and B2 and B3 and B4 ) then
        begin //plot the item on the grid if it fits
        //Tx := Integer((X - 14 - (pTempItems(ItemList.Items[CurrentSelectedItem]).W div 2)) div 18) * 18 + 20;
        //Ty := Integer((Y - 47 - (pTempItems(ItemList.Items[CurrentSelectedItem]).H div 2)) div 26) * 26 + 57;
          DrawSub( lpDDSBack, ApplyOffset( rect( Tx, Ty, Tx + ItemList[ CurrentSelectedItem ].W, Ty + ItemList[ CurrentSelectedItem ].H ) ), ItemList[ CurrentSelectedItem ].Rect0, ItemList[ CurrentSelectedItem ].DXShadow, True, ShadowAlpha );
          pr := ItemList[ CurrentSelectedItem ].Rect0;
          lpDDSBack.BltFast( Tx + Offset.X, Ty + Offset.Y, ItemList[ CurrentSelectedItem ].DXSurface, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
          ItemList[ CurrentSelectedItem ].InvX := Tx; //X-pTempItems(ItemList.Items[CurrentSelectedItem]).W div 2;
          ItemList[ CurrentSelectedItem ].InvY := Ty; //Y-pTempItems(ItemList.Items[CurrentSelectedItem]).H div 2;
          ItemList[ CurrentSelectedItem ].ItemType := 'Inventory';
          ItemList[ CurrentSelectedItem ].CharacterHadThisOnHim := true;
          ItemList[ CurrentSelectedItem ].BodySlot := -1;
          CleanUpLegalSlots;
          CurrentSelectedItem := -1;
          ContainCursor( 0 );
          pr := Rect( ClearLeft, ClearTop, ClearRight, ClearBottom );
          lpDDSBack.BltFast( ClearLeft + Offset.X, ClearTop + Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //erase any message on the screen
        end
        else
        begin //plot failure message
           //clean up - this plots the objects dirty, then the new text, then saves the dirty - prevents Dirty errors
          pr := ItemList[ CurrentSelectedItem ].Rect0;
          lpDDSBack.BltFast( Tx + Offset.X, Ty + Offset.Y, DXDirty, @pr, DDBLTFAST_WAIT );
          pr := Rect( ClearLeft, ClearTop, ClearRight, ClearBottom );
          lpDDSBack.BltFast( ClearLeft + Offset.X, ClearTop + Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
          if UseSmallFont then
            pText.PlotTinyTextBlock( txtMessage[ 3 ], ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha )
          else
            PlotText( txtMessage[ 3 ], ClearLeft, LrgMsg, Alpha );
          //save the background to the dirty DD surface based on the floating item
          pr := ApplyOffset( Rect( Tx, Ty, Tx + ItemList[ CurrentSelectedItem ].W, Ty + ItemList[ CurrentSelectedItem ].H ) );
          DXDirty.BltFast( 0, 0, lpDDSBack, @pr, DDBLTFAST_WAIT );
          DrawSub( lpDDSBack, ApplyOffset( rect( Tx, Ty, Tx + ItemList[ CurrentSelectedItem ].W, Ty + ItemList[ CurrentSelectedItem ].H ) ), ItemList[ CurrentSelectedItem ].Rect0, ItemList[ CurrentSelectedItem ].DXShadow, True, ShadowAlpha );
          pr := ItemList[ CurrentSelectedItem ].Rect0;
          lpDDSBack.BltFast( Tx + Offset.X, Ty + Offset.Y, ItemList[ CurrentSelectedItem ].DXSurface, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        end;
      end;
    end; //endif

    if Loaded then
    begin
      WriteTheInventoryData;
      SoAOS_DX_BltFront;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TInventory.MouseDown


procedure TInventory.MouseMove( Sender : TObject;
      Shift : TShiftState; X, Y, GridX, GridY : Integer );
var
  Tw, Th : Integer;
  i : Integer;
  pr : TRect;
const
  FailName : string = 'TInventory.MouseMove';
begin
  try //This assigned(DXBack) is here to keep the program from crashing while Im developing it
    if ( CurrentSelectedItem > -1 ) and Assigned( DXBack ) then
    begin //are we dragging an item?
    //clean up
      pr := ItemList[ CurrentSelectedItem ].Rect0;
      lpDDSBack.BltFast( Tx + Offset.X, Ty + Offset.Y, DXDirty, @pr, DDBLTFAST_WAIT );
    //Compute the coords for the floating item
      Tx := ( X - Offset.X ) - ItemList[ CurrentSelectedItem ].W div 2;
      Ty := ( Y - Offset.Y ) - ItemList[ CurrentSelectedItem ].H div 2;
      if Tx < 0 then
        Tx := 0;
      if Ty < 0 then
        Ty := 0;
      Tw := ItemList[ CurrentSelectedItem ].W;
      if ( Tx + Tw ) > 659 then
        Tx := 659 - Tw;

      Th := ItemList[ CurrentSelectedItem ].H;
      if ( Ty + Th ) > 463 then
        Ty := 463 - Th;

    //save the background to the dirty DD surface based on the floating item
      pr := ApplyOffset( Rect( Tx, Ty, Tx + Tw, Ty + Th ) );
      DXDirty.BltFast( 0, 0, lpDDSBack, @pr, DDBLTFAST_WAIT );
    //plot the item centered under the mouse pointer
      DrawSub( lpDDSBack, ApplyOffset( rect( Tx, Ty, Tx + ItemList[ CurrentSelectedItem ].W, Ty + ItemList[ CurrentSelectedItem ].H ) ), ItemList[ CurrentSelectedItem ].Rect0, ItemList[ CurrentSelectedItem ].DXShadow, True, ShadowAlpha );
      pr := ItemList[ CurrentSelectedItem ].Rect0;
      lpDDSBack.BltFast( Tx + Offset.X, Ty + Offset.Y, ItemList[ CurrentSelectedItem ].DXSurface, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      SoAOS_DX_BltFront;
    end
    else if Assigned( DXBack ) and ( DlgScroll.ScrollIsShowing = False ) then
    begin //do the rollover
      i := 0; //find the item the mouse is down over
      pr := Rect( ClearLeft, ClearTop, ClearRight, ClearBottom );
      lpDDSBack.BltFast( ClearLeft + Offset.X, ClearTop + Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
      while ( i < ItemList.Count ) and ( CurrentSelectedItem = -1 ) do
      begin
        if ptInRect( ApplyOffset( ItemList[ i ].InvRect ), point( x, y ) ) then
        begin
          CurrentSelectedItem := i; //assign it for the sake of PlotText
          if i = SlotCoord[ 11 ].UsedBy then
          begin //if Over weapon
            if UseSmallFont then
            begin
              if Character.Strength < TWeapon( ItemList[ CurrentSelectedItem ].PItem ).MinStrength then //Weapon minstrength error
                pText.PlotTinyTextBlock( txtMessage[ 4 ], ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha )
              else if Character.Coordination < TWeapon( ItemList[ CurrentSelectedItem ].PItem ).MinCoordination then //weap mincoord
                pText.PlotTinyTextBlock( txtMessage[ 5 ], ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha )
              else if Character.Restriction > TWeapon( ItemList[ CurrentSelectedItem ].PItem ).MaxRestriction then //maxRestriction
                pText.PlotTinyTextBlock( txtMessage[ 6 ], ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha )
              else
                pText.PlotTinyTextBlock( GetSlotText + txtMessage[ 7 ], ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha );
            end
            else
            begin
              if Character.Strength < TWeapon( ItemList[ CurrentSelectedItem ].PItem ).MinStrength then //Weapon minstrength error
                PlotText( txtMessage[ 4 ], ClearLeft, LrgMsg, Alpha )
              else if Character.Coordination < TWeapon( ItemList[ CurrentSelectedItem ].PItem ).MinCoordination then //weap mincoord
                PlotText( txtMessage[ 5 ], ClearLeft, LrgMsg, Alpha )
              else if Character.Restriction > TWeapon( ItemList[ CurrentSelectedItem ].PItem ).MaxRestriction then //maxRestriction
                PlotText( txtMessage[ 6 ], ClearLeft, LrgMsg, Alpha )
              else
                pText.PlotTinyTextBlock( GetSlotText + txtMessage[ 7 ], ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha );
//                pText.PlotText(GetSlotText + txtMessage[7], ClearLeft, LrgMsg,Alpha);
            end;
          end
          else
          begin
//          if UseSmallFont then
            pText.PlotTinyTextBlock( GetSlotText + txtMessage[ 7 ], ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha );
//          else
//            pText.PlotText(GetSlotText + txtMessage[7], ClearLeft, LrgMsg,Alpha);
          end;
        end;
        i := i + 1;
      end; //wend
    //If we arent over an item see if we're over the ground slot
      if PtInRect( ApplyOffset( Rect(275, 384, 353, 416) ), Point( x, y) ) and ( CurrentSelectedItem = -1 ) then
      begin //over the ground slot
        if GroundOrderList.Count > 0 then
        begin
          CurrentSelectedItem := ItemList.IndexOf( GroundOrderList[ TopGroundIndex ] );
//          if UseSmallFont then
          pText.PlotTinyTextBlock( ( GetSlotText + txtMessage[ 7 ] ), ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha );
//          else
//            pText.PlotText((GetSlotText + txtMessage[7]), ClearLeft, LrgMsg,Alpha);
        end;
      end; //endif
    //Clean up arrows and back to game
      pr := Rect( 261, 394, 261 + 14, 394 + 15 );
      lpDDSBack.BltFast( 261 + Offset.X, 394 + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
      pr := Rect( 354, 396, 354 + 11, 396 + 11 );
      lpDDSBack.BltFast( 354 + Offset.X, 396 + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
      pr := Rect( 595, 416, 595 + 74, 416 + 46 );
      lpDDSBack.BltFast( 595 + Offset.X, 416 + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
      if CurrentSelectedItem = -1 then
      begin //If we arent over an item then check arrows and back button
        if PtinRect( ApplyOffset( rect( 259, 385, 277, 418 ) ), point( X, Y ) ) then
        begin //over left arrow
          //plot highlighted arrow
          pr := Rect( 0, 0, 14, 15 );
          lpDDSBack.BltFast( 261 + Offset.X, 394 + Offset.Y, DXLeftArrow, @pr, DDBLTFAST_WAIT );
          //plot a bit of informative text
          pr := Rect( ClearLeft, ClearTop, ClearRight, ClearBottom );
          lpDDSBack.BltFast( ClearLeft + Offset.X, ClearTop + Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
          if UseSmallFont then
            pText.PlotTinyTextBlock( txtMessage[ 8 ], ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha )
          else
            PlotText( txtMessage[ 8 ], ClearLeft, LrgMsg, Alpha );
        end
        else if PtinRect( ApplyOffset( rect( 353, 391, 365, 418 ) ), point( X, Y ) ) then
        begin //over right arrow
          //plot highlighted arrow
          pr := Rect( 0, 0, 11, 11 );
          lpDDSBack.BltFast( 354 + Offset.X, 396 + Offset.Y, DXRightArrow, @pr, DDBLTFAST_WAIT );
          //plot a bit of informative text
          pr := Rect( ClearLeft, ClearTop, ClearRight, ClearBottom );
          lpDDSBack.BltFast( ClearLeft + Offset.X, ClearTop + Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
          if UseSmallFont then
            pText.PlotTinyTextBlock( txtMessage[ 9 ], ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha )
          else
            PlotText( txtMessage[ 9 ], ClearLeft, LrgMsg, Alpha );
        end
        else if PtinRect( ApplyOffset( rect( 595, 416, 595 + 74, 416 + 46 ) ), point( X, Y ) ) then
        begin //over back button
          //plot highlighted back to game
          pr := Rect( 0, 0, 74, 46 );
          lpDDSBack.BltFast( 595 + Offset.X, 416 + Offset.Y, DXBackToGame, @pr, DDBLTFAST_WAIT );
          //don't plot a bit of informative text, just clean up
          pr := Rect( ClearLeft, ClearTop, ClearRight, ClearBottom );
          lpDDSBack.BltFast( ClearLeft + Offset.X, ClearTop + Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
        end;
      end; //endif CurrentSelectedItem
      CurrentSelectedItem := -1; //deassign it

      SoAOS_DX_BltFront;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TInventory.MouseMove


procedure TInventory.MouseUp( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer );
const
  FailName : string = 'TInventory.MouseUp';
begin
  try
    DlgScroll.KeepOnScrolling := false;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TInventory.MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;

  if DlgScroll.ScrollIsShowing then
  begin
    if WheelDelta<0 then
      DlgScroll.ScrollAmount := 1
    else if WheelDelta>0 then
      DlgScroll.ScrollAmount := -1
    else
      DlgScroll.ScrollAmount := 0;
    DlgScroll.KeepOnScrolling := true;
    DlgScroll.ScrollStatsScroll;
  end;

end;

procedure TInventory.Paint;
var
  i : Integer;
  pr : TRect;
const
  FailName : string = 'TInventory.paint';
begin
  Log.DebugLog(FailName);
  try
    pr := Rect( 0, 0, 679, 476 );
    lpDDSBack.BltFast( Offset.X, Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    PlotText( IntToStr( Character.money ) + ' ' + txtCrowns, 240 - pText.TextLength( IntToStr( Character.money ) + ' ' + txtCrowns ), 10, Alpha );
  //Now plot all of the items on the grid
    for i := 0 to ItemList.Count - 1 do
    begin
      if ( DontPlotCurrentItem = false ) or ( i <> CurrentSelectedItem ) then
      begin
        if ItemList[ i ].ItemType <> 'Ground' then
        begin //if not in the ground slot
          if assigned( ItemList[ i ].DXSurface ) then
          begin
            DrawSub( lpDDSBack, ApplyOffset( ItemList[ i ].InvRect ), ItemList[ i ].Rect0, ItemList[ i ].DXShadow, True, ShadowAlpha );
            pr := ItemList[ i ].Rect0;
            lpDDSBack.BltFast( ItemList[ i ].InvX + Offset.X, ItemList[ i ].InvY + Offset.Y, ItemList[ i ].DXSurface, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
              //If player stats are too low dim weapon-New June 8 00
            if ( ItemList[ i ].BodySlot = 11 ) and ( ItemList[ i ].PItem is TWeapon ) then
            begin
              if ( Character.Strength < TWeapon( ItemList[ i ].PItem ).MinStrength ) //Weapon minstrength error
                or ( Character.Coordination < TWeapon( ItemList[ i ].PItem ).MinCoordination ) //weap mincoord
                or ( Character.Restriction > TWeapon( ItemList[ i ].PItem ).MaxRestriction ) then
              begin //maxRestriction
                DrawAlpha( lpDDSBack, ApplyOffset( ItemList[ i ].InvRect ), ItemList[ i ].InvRect, DXBack, false, 120 );
              end;
            end;
          end;
           //end new code
        end
        else
        begin //In the ground slot so plot iconic image
          if assigned( ItemList[ i ].DXSurfaceIcon ) then
          begin
            pr := Rect( 0, 0, cGroundListWidth, cGroundListHeight );
            lpDDSBack.BltFast( ItemList[ i ].InvX + Offset.X, ItemList[ i ].InvY + Offset.Y, ItemList[ i ].DXSurfaceIcon, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
          end;
        end;
      end; //endif
    end;

    SoAOS_DX_BltFront;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TInventory.Paint;

function TInventory.CollisionCheck( X, Y : Integer ) : Boolean;
var
  i : Integer;
  R1, R2, R3 : TRect; //R1 is the rect desribing the dragged item's destination R2 is a given inventory item rect, r3 is the result rect if collison (unused)
  k : Boolean;
  CollisionHasNotOccured : Boolean;

const
  FailName : string = 'TInventory.CollisionCheck';
begin
  Log.DebugLog(FailName);
  Result := false;
  try
    CollisionHasNotOccured := True;
  //first get the rectangle desribing the area where this item will land on the grid
    R1.Left := X; //Integer((X - 14 - (pTempItems(ItemList.Items[CurrentSelectedItem]).W div 2)) div 18) * 18 + 20;
    R1.Right := R1.Left + ItemList[ CurrentSelectedItem ].W;
    R1.Top := Y; //Integer((Y - 47 - (pTempItems(ItemList.Items[CurrentSelectedItem]).H div 2)) div 26) * 26 + 57;
    R1.Bottom := R1.Top + ItemList[ CurrentSelectedItem ].H;
    for i := 0 to ItemList.Count - 1 do
    begin //check where we will land vs all other inv items for collision
      if i <> CurrentSelectedItem then
      begin //if this isnt the dragged item check for collision
        R2.Left := ItemList[ i ].InvX; //stuff this inventory item into a rect
        R2.Right := R2.Left + ItemList[ i ].W;
        R2.Top := ItemList[ i ].InvY;
        R2.Bottom := R2.Top + ItemList[ i ].H;
        k := IntersectRect( R3, R2, R1 );
        if k = True then
          CollisionHasNotOccured := False; //we hit something
      end; //endif
    end; //end for
    Result := CollisionHasNotOccured;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TInventory.CollisionCheck

function TInventory.TryToDropOnBody( X, Y : Integer ) : Integer;
var
  i : Integer;
  SlotsHitIndex : array[ 0..5 ] of Integer;
  R1, R3 : TRect; //R1 is the rect desribing the dragged item's destination, r3 is the result rect if collison (unused)
  k : Boolean;
  SlotNumber : Integer;
  j, m : Integer;
  WeaponFound, ShieldFound : Boolean;
const
  FailName : string = 'TInventory.trytodroponbody';
begin
  Log.DebugLog(FailName);
  Result := -1;
  try
    SlotNumber := -1;
    j := 0;
  //first get the rectangle describing the area where this item is
    R1.Left := Integer( X  - Offset.X - ( ItemList[ CurrentSelectedItem ].W div 2 ) );
    R1.Right := R1.Left + ItemList[ CurrentSelectedItem ].W;
    R1.Top := Integer( Y  - Offset.Y - ( ItemList[ CurrentSelectedItem ].H div 2 ) );
    R1.Bottom := R1.Top + ItemList[ CurrentSelectedItem ].H;
    for i := 0 to 20 do
    begin //check where we will land vs all other inv items for collision
      k := IntersectRect( R3, SlotCoord[ i ].Rect, R1 );
      if k then
      begin
        SlotsHitIndex[ j ] := i; //we collect all the slots hit
        j := j + 1;
      end;
    end; //end for
  //Which of these slots is the center of the dragged bitmap closest to?
    if j > 0 then
    begin // if there was at least 1 collision then
      m := SlotsHitIndex[ 0 ];
      WeaponFound := False;
      ShieldFound := False;
      for i := 0 to j - 1 do
      begin
        if SlotsHitIndex[ i ] = 11 then
          WeaponFound := True;
        if SlotsHitIndex[ i ] = 12 then
          ShieldFound := True;
        if ( ( Abs( ( X - Offset.X ) - SlotCoord[ m ].cx ) + Abs( ( Y - Offset.Y ) - SlotCoord[ m ].cy ) ) > ( Abs( ( X - Offset.X ) - SlotCoord[ SlotsHitIndex[ i ] ].cx ) + Abs( ( Y - Offset.Y )- SlotCoord[ SlotsHitIndex[ i ] ].cy ) ) ) then
          m := SlotsHitIndex[ i ]; //if the second slot is close assign its index to m
      end;
    //We watch for the shield and Weapons- if its a shield or weapon, we just chunk it on in there- special preference
      if WeaponFound and ItemList[ CurrentSelectedItem ].PItem.CanEquip( TSlot( 11 ), Character ) then
      begin
        if TWeapon( ItemList[ CurrentSelectedItem ].PItem ).TwoHanded and ( SlotCoord[ 12 ].Usedby <> -1 ) then
        begin
          SlotNumber := -1;
          ErrorCode := 11;
        end
        else
        begin
          SlotNumber := 11;
        end;
      end
      else if ShieldFound and ItemList[ CurrentSelectedItem ].PItem.CanEquip( TSlot( 12 ), Character ) then
      begin
        SlotNumber := 12;
        if SlotCoord[ 11 ].UsedBy > -1 then
        begin //if weapon wielded
          if TWeapon( ItemList[ SlotCoord[ 11 ].UsedBy ].PItem ).TwoHanded then
          begin //if its two handed
            SlotNumber := -1; //if he has 2 handed weapon, canw wear shield
            ErrorCode := 12;
          end;
        end;
      end
      else if ItemList[ CurrentSelectedItem ].PItem.CanEquip( TSlot( m ), Character ) then
      begin
        if not Character.EquipmentLocked[ TSlot( m ) ] and ( not assigned( Character.Equipment[ TSlot( m ) ] ) or Character.Equipment[ TSlot( m ) ].CanEquip( TSlot( m ), Character ) ) then
          SlotNumber := m;
      end;

    end; //endif j > 0

    Result := SlotNumber;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TInventory.TryToDropOnBody

function TInventory.GetSlotText : string;
var
  Sentence : AnsiString;
  dl : integer;
  LoopCount : integer;
const
  FailName : string = 'TInventory.GetSlotText';
begin
  Log.DebugLog(FailName);
  Result := 'failure';
  try
    Sentence := AnsiString( Trim( ItemList[ CurrentSelectedItem ].PItem.Name ));
    LoopCount := 0;
    while ( pText.TextLength( Sentence ) > 300 ) and ( LoopCount < 10 ) do
    begin
      dl := LastDelimiter( ' ', Sentence );
      SetLength( Sentence, dl - 1 );
      inc( LoopCount );
    end;

    Result := Sentence;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TInventory.GetSlotText

procedure TInventory.ShowLegalSlots( Index : Integer );
var
  t : TSlot;
const
  FailName : string = 'TInventory.ShowLegalSlots';
begin
  Log.DebugLog(FailName);
  try
    for t := slLeg1 to slMisc3 do
    begin
      if ItemList[ Index ].PItem.CanEquip( t, Character ) and not Character.EquipmentLocked[ t ] then
      begin
        if ( t = slWeapon ) or ( t = slShield ) then
        begin //custom circle rect for weapon and shield
          DrawAlpha( lpDDSBack, ApplyOffset( Rect( SlotCoord[ Integer( t ) ].cx - 30, SlotCoord[ Integer( t ) ].cy - 20, SlotCoord[ Integer( t ) ].cx + 30, SlotCoord[ Integer( t ) ].cy + 40 ) ), Rect( 0, 0, 99, 99 ), DXCircle, True, 96 );
        end
        else
        begin //every other item
          DrawAlpha( lpDDSBack, ApplyOffset( SlotCoord[ Integer( t ) ].Rect ), Rect( 0, 0, 99, 99 ), DXCircle, True, 96 );
        end;
      end;
    end;
    ShowOpenInventorySlots;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TInventory.ShowLegalSlots;

procedure TInventory.CleanUpLegalSlots;
const
  FailName : string = 'TInventory.CleanUpLegalSlots';
begin
  Log.DebugLog(FailName);
  try
    paint;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TInventory.CleanUpLegalSlots;

procedure TInventory.WriteTheInventoryData;
var
  i : Integer;
  t : TSlot;
const
  FailName : string = 'TInventory.WritetheInventorydata';
begin
  Log.DebugLog(FailName);
  try
    if ( CurrentSelectedItem <> -1 ) and CheckForGroundDrop then
    begin //player is dragging an item
      if DropAnItem( 20 + Offset.X, 58  + Offset.Y ) = true then
      begin
        ItemList[ CurrentSelectedItem ].InvX := Tx;
        ItemList[ CurrentSelectedItem ].InvY := Ty;
        ItemList[ CurrentSelectedItem ].ItemType := 'Inventory';
        ItemList[ CurrentSelectedItem ].CharacterHadThisOnHim := true;
        ItemList[ CurrentSelectedItem ].BodySlot := -1;
      end
      else
      begin //failed- no room: drop it on ground
        ItemList[ CurrentSelectedItem ].ItemType := 'Ground';
      end;
      ContainCursor( 0 );
    end;
  //Clear the Characters Inventory and Equipment list
  //Character.Inventory.free; wont let me- read only - so:
  //for i:=0 to Character.Inventory.count-1  do begin
    Character.Inventory.Clear;
  //end;
  //Clear the equipment array
    for t := slLeg1 to slMisc3 do
    begin
      Character.Equipment[ t ] := nil;
    end;

  //Assign the new values
    for i := 0 to ItemList.Count - 1 do
    begin
      ItemList[ i ].PItem.InvX := ( ItemList[ i ].InvX - 20 ) div 18;
      ItemList[ i ].PItem.InvY := ( ItemList[ i ].InvY - 57 ) div 26;
    //pTempItems(ItemList.Items[i]).pItem.BodySlot:=pTempItems(ItemList.Items[i]).BodySlot;
      ItemList[ i ].PItem.Enabled := False; //this is only true if an item in on the ground
      if ItemList[ i ].ItemType = 'Inventory' then
        Character.Inventory.Add( ItemList[ i ].PItem )
      else if ItemList[ i ].ItemType = 'Equipment' then
      begin
        if ItemList[ i ].BodySlot > -1 then
        begin
          Character.Equipment[ TSlot( ItemList[ i ].BodySlot ) ] := ItemList[ i ].PItem;
        end;
      end
      else
      begin //its on the ground
      //put the item at the characters pos on the ground
        ItemList[ i ].PItem.SetPos( Character.X, Character.Y, 0 );
        ItemList[ i ].PItem.Enabled := True; //make it visible
        if ItemList[ i ].CharacterHadThisOnHim and CheckForGroundDrop then
          ItemList[ i ].PItem.Drop;
      end; //endif
    end; //endfor

    Character.CalcStats;
    if assigned( DrawGuy ) then
      DrawGuy( Character );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TInventory.WriteTheInventoryData



procedure TInventory.ContainCursor( action : integer );
var
  prRect : TRect;
const
  FailName : string = 'TInventory.ContainCursor';
begin
  Log.DebugLog(FailName);
  try
    prRect.Left := Offset.X;
    prRect.Top := Offset.Y;
    ClientToScreen(frmMain.Handle, prRect.TopLeft);
    if Action = 1 then
    begin //constrict to main inventory area
      prRect.bottom := prRect.Top + 456;
      prRect.Right := prRect.Left + 659;
      ClipCursor( @prRect ); //TODO: Windows-ism - replace
    end
    else
    begin //restore to fullscreen
//      prRect.bottom := ScreenMetrics.ScreenHeight;
//      prRect.Right := ScreenMetrics.ScreenWidth;
      ClipCursor(nil);
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TInventory.ContainCursor

procedure TInventory.WeaponPlacement( ItemIndex : integer );
var
  nx, ny, nw, nh : integer;
  nRect : TRect;
const
  FailName : string = 'TInventory.WeaponPlacment';
begin
  Log.DebugLog(FailName);
  try
    //This routine checks for collisions with the weapon and Misc3 and the Feet-
    //It moves the weapon about accordingly to avoid unsightly collisions
    nx := Tx;
    ny := Ty;
    nw := ItemList[ ItemIndex ].W;
    nh := ItemList[ ItemIndex ].H;
    if intersectRect( nRect, rect( nx, ny, nx + nw, ny + nh ), SlotCoord[ 15 ].Rect ) then
    begin
      ny := SlotCoord[ 15 ].rect.bottom + 1;
    end;
    //if intersectRect(nRect,rect(nx,ny,nx+nw,ny+nh),SlotCoord[0].Rect) then begin
    if ny + nh > SlotCoord[ 0 ].Rect.bottom - 5 then
    begin
      ny := SlotCoord[ 0 ].rect.top - ( nh + 1 );
    end;
    if intersectRect( nRect, rect( nx, ny, nx + nw, ny + nh ), SlotCoord[ 15 ].Rect ) then
    begin
      nx := SlotCoord[ 15 ].rect.right + 1;
    end;
    Tx := nx;
    Ty := ny;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TInventory.WeaponPlacement


{procedure TInventory.DebugPlot(i: integer);
var
 a : string;
const
  FailName: string = 'TInventory.DebugPlot';
begin
  Log.DebugLog(FailName);
{try
lpDDSBack.BltFast(20,237,DXBack,Rect(20,237,20+50,237+25),DDBLTFAST_WAIT); //clean up before we plot text
str(i,a);
pText.PlotText(a,20,237,0);
except
   on E: Exception do Log.log(FailName+E.Message);
end;

end;  }

procedure TInventory.Release;
var
  i : Integer;
const
  FailName : string = 'TInventory.release';
begin
  Log.DebugLog(FailName);
  try
    frmMain.OnMouseWheel := nil;
    CheckForGroundDrop := true;
    WriteTheInventoryData;
    ExText.Close;
    Locked := false;
    DlgScroll.free;
    DlgScroll := nil;
    DXBack := nil;
    DXCircle := nil;
    DXRightArrow := nil;
    DXLeftArrow := nil;
    DXBackToGame := nil;
    DxDirty := nil;
    pText.UnloadTinyFontGraphic;
    for i := 0 to ItemList.Count - 1 do
    begin
      if Assigned( ItemList[ i ].DXSurface ) then
        ItemList[ i ].DXSurface := nil;
      if Assigned( ItemList[ i ].DXSurfaceIcon ) then
        ItemList[ i ].DXSurfaceIcon := nil;
      if Assigned( ItemList[ i ].DXShadow ) then
        ItemList[ i ].DXShadow := nil;

    end;
  //ItemsList Cleanup
    if Assigned( ItemList ) then
    begin
      for i := 0 to ( ItemList.Count - 1 ) do
      begin
        pInventoryItem := ItemList[ i ];
        Dispose( pInventoryItem );
      end;
      ItemList.Free;
      ItemList := nil;
    end;
    GroundOrderList.Free;
    GroundOrderList := nil;
    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TInventory.ShowOpenInventorySlots;
var
  i, j, k, m, n : integer;
  gWidth : integer;
  gHeight : integer;
  XX, YY : integer;

const
  FailName : string = 'TInventory.ShowOpenInventorySlots';
begin
  Log.DebugLog(FailName);
  try
    gWidth := 6 * 2; //each grid is 2x2
    gHeight := 7 * 2;

   //Clear the Array
    for i := 0 to 11 do
    begin
      for j := 0 to 13 do
      begin
        PlotArray[ i, j ] := 0;
      end;
    end;

    j := 0;
    i := CurrentSelectedItem;
    while j <= ( gWidth - ItemList[ i ].pItem.InvW ) do
    begin //try to squeeze it in start upper left going to lower right
      k := 0;
      while k <= ( gHeight - ItemList[ i ].pItem.InvH ) do
      begin
        XX := ( j * 18 ) + 20; //+pTempItems(ItemList.Items[i]).W div 2;
        YY := ( k * 26 ) + 57; //+pTempItems(ItemList.Items[i]).H div 2;
        if CollisionCheck( XX, YY ) then
        begin //if it mark the array
          for m := 0 to ItemList[ i ].pItem.InvW - 1 do
          begin
            for n := 0 to ItemList[ i ].pItem.InvH - 1 do
            begin
              PlotArray[ j + m, k + n ] := 1;
            end; //n
          end; //m
        end;
        k := k + 1;
      end; //wend
      j := j + 1;
    end; //wend


   //lpDDSBack.BltFast(20,40,DXBack,rect(20,40,238,423), DDBLTFAST_WAIT);
    for i := 0 to 11 do
    begin
      for j := 0 to 13 do
      begin
        if PlotArray[ i, j ] = 0 then
          DrawAlpha( lpDDSBack, ApplyOffset( rect( i * 18 + 21, j * 26 + 57, i * 18 + 21 + 18, j * 26 + 57 + 26 ) ), rect( 0, 0, 25, 25 ), DXBrown, False, 90 );
      end;
    end;

  { for i := 0 to ItemList.Count - 1 do begin
       if CurrentSelectedItem <> i then begin
         if pTempItems(ItemList.Items[i]).ItemType = 'Inventory' then begin
             DrawSub(lpDDSBack,rect(pTempItems(ItemList.Items[i]).InvX, pTempItems(ItemList.Items[i]).InvY,pTempItems(ItemList.Items[i]).InvX+pTempItems(ItemList.Items[i]).W, pTempItems(ItemList.Items[i]).InvY+pTempItems(ItemList.Items[i]).H),Rect(0, 0, pTempItems(ItemList.Items[i]).W, pTempItems(ItemList.Items[i]).H),pTempItems(ItemList.Items[i]).DXShadow ,True,ShadowAlpha);
             lpDDSBack.BltFast(pTempItems(ItemList.Items[i]).InvX, pTempItems(ItemList.Items[i]).InvY, pTempItems(ItemList.Items[i]).DXSurface, Rect(0, 0, pTempItems(ItemList.Items[i]).W, pTempItems(ItemList.Items[i]).H), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT)
         end;
       end;
   end; }

  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TInventory.ShowOpenInventorySlots

function TInventory.DropAnItem( X, Y : integer ) : boolean;
var
  i, j : integer;
  XX, YY : Integer;
  FoundASafePlaceToDrop : boolean;
  LastLowTotal : integer;
const
  FailName : string = 'TInventory.Dropanitem';
begin
  Log.DebugLog(FailName);
  Result := false;
  try
    LastLowTotal := 9999; //initialize to insanely high number
    FoundASafePlaceToDrop := false;
   //upper left corner of floating bmp
    XX := X - Offset.X - ItemList[ CurrentSelectedItem ].W div 2;
    YY := Y - offset.Y - ItemList[ CurrentSelectedItem ].H div 2;

    for i := 0 to 11 do
    begin
      for j := 0 to 13 do
      begin
        if PlotArray[ i, j ] = 1 then
        begin
          if ( ( i * 18 + 20 + ItemList[ CurrentSelectedItem ].W ) < 238 ) and ( ( j * 26 + 57 + ItemList[ CurrentSelectedItem ].H ) < 423 ) and ( CollisionCheck( i * 18 + 20, j * 26 + 57 ) ) then
          begin
                //find the available slot closest ot the upper left corner of floating item
            if abs( XX - ( i * 18 + 20 ) ) + abs( YY - ( j * 26 + 57 ) ) < LastLowTotal then
            begin //closer to upper corner of bitmap
              LastLowTotal := abs( XX - ( i * 18 + 20 ) ) + abs( YY - ( j * 26 + 57 ) );
              Tx := i * 18 + 20;
              Ty := j * 26 + 57;
              FoundASafePlaceToDrop := true;
            end;
          end; //endif
        end; //endif plotarray
      end; //for j
    end; //for i

    Result := FoundASafePlaceToDrop;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TInventory.DropAnItem

{ TempCharItems }

function TempCharItems.InvRect: TRect;
begin
  Result := Rect(InvX, InvY, InvX + W, InvY + H);
end;

function TempCharItems.Rect0: TRect;
begin
  Result := Rect(0, 0, W, H);
end;

end.
