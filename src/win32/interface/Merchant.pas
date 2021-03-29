unit Merchant;
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
  Vcl.Controls,
  Vcl.ExtCtrls,
  Character,
  Resource,
  GameText,
  Parts,
  Scroll,
  Engine,
  SoAOS.Animation,
  SoAOS.Intrface.Dialogs,
  LogFile;

type
  pTempItems = ^TemItems;
  TemItems = record
    PItem : TItem; //pointer to the item
    InvX : Integer;
    InvY : Integer;
    W : Integer;
    H : Integer;
    BodySlot : integer;
    cRect : TRect; //collision rect ofr the box its in
    WhoHasThis : integer; //whos got this item? Left guy(1), right guy(2) (or container) or ground(3)?
    //DXSurface: IDirectDrawSurface;      //barbie graphic surface
    DXSurfaceIcon : IDirectDrawSurface; //icon graphic surface
  end;

  Arrow = record
    X, Y : integer; //upper left coordinate to plot, check for collision on scroll arrows
    DX : IDirectDrawSurface;
  end;

  TMerchant = class( TDialog )
  private
    //List stuff
    PlayerScroll : integer; //distance left inventory has scrolled
    MerchantScroll : integer; //distance right inventory has scrolled
    ScrollArrows : array[ 0..7 ] of Arrow; //Arrows to scroll inventory
    CurrentSelectedListItem : integer;
    NumberOfMerchantItems : integer;
    NumberOfPlayerItems : integer;
    //Bitmap stuff
    ItemList : TList; //the list of items
    pInventoryItem : pTempItems; //The temporary inventory and equipment items combined
    CurrentSelectedItem : Integer; //Current Item being dragged about
    Tx, Ty : Integer; // x and y locs used with the offset of the dragged item
    DXBackHighlight : IDirectDrawSurface; //so we know which item is selected for buy/sell
    DXBack : IDirectDrawSurface; //DD surface that holds the inventory screen before blit
    DxDirty : IDirectDrawSurface; //DD for cleanup when dragging items
    DXLeftArrow : IDirectDrawSurface; //Inventory left arrow
    DXRightArrow : IDirectDrawSurface; //Inventory right arrow
    DXBuyItem : IDirectDrawSurface;
    DXSellItem : IDirectDrawSurface;
    DXBackToGame : IDirectDrawSurface; //Back To Game highlight
    DXGroundBox : IDirectDrawSurface; //The Ground Box Itself
    GroundOrderList : TList; //used to keep track of the order of items on the ground
    TopGroundIndex : Integer; //Index of the current top ground item
    Alpha : integer;
    DlgScroll : TScroll; //the statistics scroll;
    GridRightMinX : integer; //The right grid's minX - used to handle drops, clicks
    GridRightMaxX : integer; //The right grid's maX
    GridRightMinY : integer; //The right grid's minY
    GridRightMaxY : integer; //The right grid's maxY
    txtMessage : array[ 0..13 ] of string;
    Timer : TTimer;
    ScrollStateLeft : integer;
    ScrollStateRight : integer;
    procedure TimerEvent( Sender : TObject );
    procedure ContainCursor( Action : integer ); //lock curson to screen during item drag
    function CollisionCheck( X, Y : Integer ) : Boolean;
    function GetSlotText : string;
    procedure BuildGrid; //Build the drop on grid for the right inv area
    procedure ShowLeftList;
    procedure ShowRightList;
    function GetSHORTSlotText( s : string ) : string;
    procedure WriteTheInventoryData;
    function ItemFitsInInventory( ItemIndex : integer ) : boolean;
    //procedure MoveAll(Source, Destination: integer); //move as much of players inventory into object2 inventory as we can
    procedure DropOnGround( ItemIndex : integer );
  protected
    procedure MouseDown( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer ); override;
    procedure MouseMove( Sender : TObject;
      Shift : TShiftState; X, Y, GridX, GridY : Integer ); override;
    procedure MouseUp( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer ); override;
  public
    Character : TCharacter; //the charachter we draw inventory from to fill left box
    Merchant : TCharacter;
    GroundList : TList; //Passed to us - list of items on the ground
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

{ TMerchant }

const
  LrgMsg = 437;
  SmlMsg = 415;
  ClearTop = SmlMsg;
  ClearBottom = 476;
  ClearLeft = 20;
  ClearRight = 570;
  BuySellAlpha = 150;

constructor TMerchant.Create;
const
  FailName : string = 'TMerchant.Create';
begin
  Log.DebugLog( FailName );
  try
    inherited;
    GroundList := TList.Create;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

destructor TMerchant.Destroy;
const
  FailName : string = 'TMerchant.destroy';
begin
  Log.DebugLog( FailName );
  try
    GroundList.Free;
    GroundList := nil;
    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

procedure TMerchant.Init;
var
  i, width, height : Integer;
  DXBorder : IDirectDrawSurface;
  t : TSlot;
  pr : TRect;
const
  FailName : string = 'TMerchant.init';
begin
  Log.DebugLog( FailName );
  try
    if Loaded then
      Exit;
    inherited;
    MouseCursor.Cleanup;
    pr := Rect( 0, 0, ResWidth, ResHeight );
    lpDDSBack.BltFast( 0, 0, lpDDSFront, @pr, DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;

    ExText.Open( 'Merchant' );
    for i := 0 to 13 do
      txtMessage[ i ] := ExText.GetText( 'Message' + inttostr( i ) );

    PlayerScroll := 0;
    MerchantScroll := 0;
    CurrentSelectedListItem := -1;
    DlgScroll := TScroll.create; //create the statistics scroll
    DlgScroll.pText := pText; //assign the pointer to pText;
    pText.LoadFontGraphic( 'inventory' ); //load the inventory font graphic in
    pText.LoadTinyFontGraphic; //load the small font graphic for use
    CurrentSelectedItem := -1; //We aren't dragging anything
    DlgScroll.ScrollIsShowing := False; //stats screen isnt showing
    Alpha := 220; //alpha value for all alphabet plots

  //We have to do this part up here in order to get coordinated from buildgrid
    DXGroundBox := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'merGroundBox.bmp', cInvisColor );
    DXRightArrow := SoAOS_DX_LoadBMP( InterfacePath + 'invRightArrow.bmp', cInvisColor );
    DXLeftArrow := SoAOS_DX_LoadBMP( InterfacePath + 'invLeftArrow.bmp', cInvisColor );
    DXBackToGame := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'obInvBackToGame.bmp', cInvisColor );
    DXBackHighlight := SoAOS_DX_LoadBMP( InterfacePath + 'merBackHighlight.bmp', cInvisColor );
{  BMBack.LoadFromFile(InterfacePath + 'merBuyItem.bmp');
  DXBuyItem := DDGetImage(lpDD, BMBack, InvisColor, False);
  BMBack.LoadFromFile(InterfacePath + 'merSellItem.bmp');
  DXSellItem := DDGetImage(lpDD, BMBack, InvisColor, False);   }
    DXBuyItem := DDGetSurface( lpDD, 192, 24, cBlackBackground, false );
    DXSellItem := DDGetSurface( lpDD, 192, 24, cBlackBackground, false );

  //Arrows Left up, right up, left down, right down, then darks
    ScrollArrows[ 0 ].DX := SoAOS_DX_LoadBMP( InterfacePath + 'meruparrowL.bmp', cInvisColor );
    ScrollArrows[ 0 ].X := 45 - 23;
    ScrollArrows[ 0 ].Y := 37;
    ScrollArrows[ 1 ].DX := SoAOS_DX_LoadBMP( InterfacePath + 'meruparrowR.bmp', cInvisColor );
    ScrollArrows[ 1 ].X := 298;
    ScrollArrows[ 1 ].Y := 37;
    ScrollArrows[ 2 ].DX := SoAOS_DX_LoadBMP( InterfacePath + 'merdownarrowL.bmp', cInvisColor );
    ScrollArrows[ 2 ].X := 45 - 23;
    ScrollArrows[ 2 ].Y := 353 - 32;
    ScrollArrows[ 3 ].DX := SoAOS_DX_LoadBMP( InterfacePath + 'merdownarrowR.bmp', cInvisColor );
    ScrollArrows[ 3 ].X := 298;
    ScrollArrows[ 3 ].Y := 353 - 32;
    ScrollArrows[ 4 ].DX := SoAOS_DX_LoadBMP( InterfacePath + 'meruparrowLD.bmp', cInvisColor );
    ScrollArrows[ 4 ].X := ScrollArrows[ 0 ].X;
    ScrollArrows[ 4 ].Y := ScrollArrows[ 0 ].Y;
    ScrollArrows[ 5 ].DX := SoAOS_DX_LoadBMP( InterfacePath + 'meruparrowRD.bmp', cInvisColor );
    ScrollArrows[ 5 ].X := ScrollArrows[ 1 ].X;
    ScrollArrows[ 5 ].Y := ScrollArrows[ 1 ].Y;
    ScrollArrows[ 6 ].DX := SoAOS_DX_LoadBMP( InterfacePath + 'merdownarrowLD.bmp', cInvisColor );
    ScrollArrows[ 6 ].X := ScrollArrows[ 2 ].X;
    ScrollArrows[ 6 ].Y := ScrollArrows[ 2 ].Y;
    ScrollArrows[ 7 ].DX := SoAOS_DX_LoadBMP( InterfacePath + 'merdownarrowRD.bmp', cInvisColor );
    ScrollArrows[ 7 ].X := ScrollArrows[ 3 ].X;
    ScrollArrows[ 7 ].Y := ScrollArrows[ 3 ].Y;
 //end of arrows

    DXBack := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'Merchant.bmp', cInvisColor, DlgWidth, DlgHeight );
    DXDirty := DDGetSurface( lpDD, cGroundListWidth, cGroundListHeight, cInvisColor, true );
  //build the left side inventory space
    BuildGrid;
  //now we blit the screen to the backbuffer
    pr := Rect( 0, 0, DlgWidth, DlgHeight );
    lpDDSBack.BltFast( Offset.X, Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
  //Now for the Alpha'ed edges
    DXBorder := SoAOS_DX_LoadBMP( InterfacePath + 'obInvRightShadow.bmp', cInvisColor, width, height );
    DrawSub( lpDDSBack, ApplyOffset( Rect( 659, 0, 659 + width, height ) ), Rect( 0, 0, width, height ), DXBorder, True, Alpha );
    DXBorder := nil;

    DXBorder := SoAOS_DX_LoadBMP( InterfacePath + 'obInvBottomShadow.bmp', cInvisColor, width, height );
    DrawSub( lpDDSBack, ApplyOffset( Rect( 0, 456, width, 456 + height ) ), Rect( 0, 0, width, height ), DXBorder, True, Alpha );
    DXBorder := nil; //release DXBorder

  //Now put the names up
    PlotText( Character.name, 43, 10, Alpha );
    PlotText( Merchant.name, 355, 10, Alpha );
    PlotText( IntToStr( Character.money ) + txtMessage[ 0 ], 297 - pText.TextLength( IntToStr( Character.money ) + txtMessage[ 0 ] ), 10, Alpha );

  //now the Buy and Sell buttons
    pr := Rect( 76, 362, 268, 386 );
    DXSellItem.BltFast( 0, 0, DXBack, @pr, DDBLTFAST_WAIT );
    pr := Rect( 385, 362, 577, 386 );
    DXBuyItem.BltFast( 0, 0, DXBack, @pr, DDBLTFAST_WAIT );
    pText.PlotTextCentered2( DXSellItem, txtMessage[ 1 ], 0, 192, 0, 255 );
    pText.PlotTextCentered2( DXBuyItem, txtMessage[ 2 ], 0, 192, 0, 255 );

    PlotTextCentered( txtMessage[ 1 ], 46, 298, 362, BuySellAlpha );
    PlotTextCentered( txtMessage[ 2 ], 355, 607, 362, BuySellAlpha );

  //Create list
    ItemList := TList.Create; //create the ItemList
    GroundOrderList := TList.Create; //and the ground orderlist
  //Load path info, coords into temp objects from the Character's Inventory
    for i := 0 to Character.Inventory.Count - 1 do
    begin
      New( pInventoryItem );
      pInventoryItem.PItem := Character.Inventory.Items[ i ];
      pInventoryItem.InvX := TItem( Character.Inventory.Items[ i ] ).InvX * 18 + 27;
      pInventoryItem.InvY := TItem( Character.Inventory.Items[ i ] ).InvY * 26 + 42;
      pInventoryItem.WhoHasThis := 1; //the character on the left; the instigating char
      pInventoryItem.BodySlot := -1; //not equipped
      ItemList.Add( pInventoryItem );
    end;
//Load path info, coords into temp objects from the Character's Equipment list
    for t := slLeg1 to slMisc3 do
    begin
      if Assigned( Character.Equipment[ t ] ) and not Character.EquipmentLocked[ t ] then
      begin
        New( pInventoryItem );
        pInventoryItem.PItem := Character.Equipment[ t ];
      //pInventoryItem.InvX := SlotCoord[integer(t)].cx-((TItem(Character.Equipment[t]).InvW*18) div 2);
      //pInventoryItem.InvY := SlotCoord[integer(t)].cy-((TItem(Character.Equipment[t]).InvH*26) div 2);
        pInventoryItem.WhoHasThis := 4; //the character on the left; he's wearing it
        pInventoryItem.BodySlot := Integer( t );
        ItemList.Add( pInventoryItem );
      end;
    end;
  //Load path info, coords into temp objects from the Merchants's Inventory
    for i := 0 to Merchant.Inventory.Count - 1 do
    begin
      New( pInventoryItem );
      pInventoryItem.PItem := Merchant.Inventory.Items[ i ];
    //pInventoryItem.InvX := TItem(Merchant.Inventory.Items[i]).InvX*18+418;
    //pInventoryItem.InvY := TItem(Merchant.Inventory.Items[i]).InvY*26+42;
      pInventoryItem.WhoHasThis := 2; //the merchant on the right;
      pInventoryItem.BodySlot := -1; //not equipped
      ItemList.Add( pInventoryItem );
    end;
  //Now the ground
    for i := 0 to GroundList.Count - 1 do
    begin
      New( pInventoryItem );
      pInventoryItem.PItem := GroundList.Items[ i ];
      if i = 0 then
      begin
        pInventoryItem.InvX := 288; //Only the first ground item is visible
        pInventoryItem.InvY := 377;
      end
      else
      begin
        pInventoryItem.InvX := 999; //set it offscreen so we dont see it
        pInventoryItem.InvY := 999;
      end;
      pInventoryItem.WhoHasThis := 3; //the ground has it
      pInventoryItem.BodySlot := -1; //not equipped
      ItemList.Add( pInventoryItem );
      GroundOrderList.Add( pInventoryItem ); //Now we create our order list
    end;

  //Get the GroundIcons
    for i := 0 to ItemList.Count - 1 do
    begin
      //pTempItems(ItemList.Items[i]).DXSurface := pTempItems(ItemList.Items[i]).pItem.GetInventoryImage;
      pTempItems( ItemList.Items[ i ] ).DXSurfaceIcon := pTempItems( ItemList.Items[ i ] ).pItem.GetIconicImage;
      pTempItems( ItemList.Items[ i ] ).W := pTempItems( ItemList.Items[ i ] ).pItem.InvW * 18;
      pTempItems( ItemList.Items[ i ] ).H := pTempItems( ItemList.Items[ i ] ).pItem.InvH * 26;
    end;

  //Now plot all of the items on the grid
    for i := 0 to ItemList.Count - 1 do
    begin
      //lpDDSBack.BltFast(pTempItems(ItemList.Items[i]).InvX, pTempItems(ItemList.Items[i]).InvY, pTempItems(ItemList.Items[i]).DXSurface, Rect(0, 0, pTempItems(ItemList.Items[i]).W, pTempItems(ItemList.Items[i]).H), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT)
      if pTempItems( ItemList.Items[ i ] ).WhoHasThis = 3 then //if  in the ground slot
  //     lpDDSBack.BltFast(pTempItems(ItemList.Items[i]).InvX, pTempItems(ItemList.Items[i]).InvY, pTempItems(ItemList.Items[i]).DXSurface, Rect(0, 0, pTempItems(ItemList.Items[i]).W, pTempItems(ItemList.Items[i]).H), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT)
  //  else //In the ground slot so plot iconic image
      begin
        pr := Rect( 0, 0, cGroundListWidth, cGroundListHeight );
        lpDDSBack.BltFast( pTempItems( ItemList.Items[ i ] ).InvX + Offset.X, pTempItems( ItemList.Items[ i ] ).InvY + Offset.Y, pTempItems( ItemList.Items[ i ] ).DXSurfaceIcon, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end;
    end;
    ShowLeftList;
    ShowRightList;

    ScrollStateLeft := 0;
    ScrollStateRight := 0;
    Timer := TTimer.create( nil );
    Timer.onTimer := TimerEvent;
    Timer.Interval := 100;
    Timer.enabled := True;

  //Whew! Now we flip it all to the screen
    SoAOS_DX_BltFront;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TMerchant.Init

procedure TMerchant.MouseDown( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer );
var
  i, j : integer;
  pTemp : Pointer;
  rRect : TRect;
  daPrice : longint;
  pr : TRect;
const
  FailName : string = 'TMerchant.MouseDown';
begin
  try
    if CurrentSelectedItem = -1 then
    begin //if no piece is being dragged pick one up
      if DlgScroll.ScrollIsShowing then
      begin
        if PtInRect( ApplyOffset( rect( 119, 30, 119 + 443, 30 + 90 )), point( X, Y ) ) or
          PtInRect( ApplyOffset( rect( 119, 373, 119 + 443, 373 + 70 )), point( X, Y ) ) then
        begin
          if Y < 248 then
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
      end //Buy
      else if PtinRect( ApplyOffset( rect( 436, 362, 436 + 87, 362 + 24 ) ), point( X, Y ) ) and ( CurrentSelectedListItem > -1 ) then
      begin //Buy
        if pTempItems( ItemList.Items[ CurrentSelectedListItem ] ).WhoHasThis = 2 then
        begin //if the highlight is on merchant
          if Character.charm < 1 then
            daPrice := Round( pTempItems( ItemList.Items[ CurrentSelectedListItem ] ).pItem.Value * ( 1 + 10 * ( Merchant.SellingMarkup - 1 ) / 1 ) )
          else
            daPrice := Round( pTempItems( ItemList.Items[ CurrentSelectedListItem ] ).pItem.Value * ( 1 + 10 * ( Merchant.SellingMarkup - 1 ) / Character.Charm ) );
          if Character.money >= daPrice then
          begin
            Character.money := -daPrice; //Character.money-(daPrice-pTempItems(ItemList.Items[CurrentSelectedListItem]).pItem.Value);
            if ItemFitsInInventory( CurrentSelectedListItem ) = false then
            begin //not enough room in inventory
              DropOnGround( CurrentSelectedListItem );
              CurrentSelectedListItem := -1;
              paint;
              pr := Rect( 20, 412, 20 + 550, 412 + 25 );
              lpDDSBack.BltFast( 20 + Offset.X, 412 + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT ); //clean up before we plot text
              PlotText( txtMessage[ 3 ], 20, 412, Alpha );
            end
            else
            begin //we did have enough room
              ItemList.move( CurrentSelectedListItem, ItemList.count - 1 ); //move it to end of list
              CurrentSelectedListItem := -1;
              paint;
            end;
          end
          else
          begin //not enough cash
              //plot a bit of informative text
            pr := Rect( 20, 412, 20 + 550, 412 + 25 );
            lpDDSBack.BltFast( 20 + Offset.X, 412 + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT ); //clean up before we plot text
            PlotText( txtMessage[ 4 ], 20, 412, Alpha );
          end;
        end; //endif
      end //Sell
      else if PtinRect( ApplyOffset( rect( 131, 361, 131 + 82, 361 + 24 ) ), point( X, Y ) ) and ( CurrentSelectedListItem > -1 ) then
      begin //sell item
        if Character.charm < 1 then
          daPrice := Round( pTempItems( ItemList.Items[ CurrentSelectedListItem ] ).pItem.Value * ( 1 + 10 * ( Merchant.BuyingDiscount - 1 ) / 1 ) )
        else
          daPrice := Round( pTempItems( ItemList.Items[ CurrentSelectedListItem ] ).pItem.Value * ( 1 + 10 * ( Merchant.BuyingDiscount - 1 ) / Character.Charm ) );
        if daPrice < 0 then
          daPrice := 0;
        if ( pTempItems( ItemList.Items[ CurrentSelectedListItem ] ).WhoHasThis = 1 ) or ( pTempItems( ItemList.Items[ CurrentSelectedListItem ] ).WhoHasThis = 4 ) then
        begin //if highlight on player
          Character.money := daPrice; //Character.money+(pTempItems(ItemList.Items[CurrentSelectedListItem]).pItem.Value-daPrice);
          pTempItems( ItemList.Items[ CurrentSelectedListItem ] ).WhoHasThis := 2; //the merchants now
          pTempItems( ItemList.Items[ CurrentSelectedListItem ] ).BodySlot := -1; //no longer on body
          ItemList.move( CurrentSelectedListItem, ItemList.count - 1 );
          CurrentSelectedListItem := -1;
          WriteTheInventoryData; //New for June 11
          paint;
        end;
      end
      else if PtInRect( ApplyOffset( Rect( 271, 375, 287, 407) ), Point( x, y ) ) then
      begin //left arrow for ground
        if GroundOrderList.Count > 1 then
        begin //get the prev item on the ground and show it
          j := TopGroundIndex;
          if j <> 0 then
          begin //if its not the first item in the list
          //replace the back from the DXBack buffer.
            pr := Rect( 287, 376, 363, 406 );
            lpDDSBack.BltFast( 287 + Offset.X, 376 + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
            pTempItems( GroundOrderList.Items[ j ] ).InvX := 999;
            pTempItems( GroundOrderList.Items[ j ] ).InvY := 999;
            j := j - 1;
          //Set the coordinates of the new item and Plot it
            pTempItems( GroundOrderList.Items[ j ] ).InvX := 288;
            pTempItems( GroundOrderList.Items[ j ] ).InvY := 377;
            pr := Rect( 0, 0, cGroundListWidth, cGroundListHeight );
            lpDDSBack.BltFast( pTempItems( GroundOrderList.Items[ j ] ).InvX + Offset.X, pTempItems( GroundOrderList.Items[ j ] ).InvY + Offset.Y, pTempItems( GroundOrderList.Items[ j ] ).DXSurfaceIcon, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
            TopGroundIndex := j;
          end
          else
          begin
          //making an obnoxious buzzing noise? - we cant go backwards from the first item
          end;
        end;
      end
      else if PtInRect( ApplyOffset( Rect( 364, 375, 376, 407) ), Point( x, y) ) then
      begin //right arrow for ground
        if GroundOrderList.Count > 1 then
        begin //get the Next item on the ground and show it
          j := TopGroundIndex;
          if j < ( GroundOrderList.Count - 1 ) then
          begin //if its not the last item in the list
          //replace the back from the DXBack buffer.
            pr := Rect( 287, 376, 363, 406 );
            lpDDSBack.BltFast( 287 + Offset.X, 376 + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
            pTempItems( GroundOrderList.Items[ j ] ).InvX := 999;
            pTempItems( GroundOrderList.Items[ j ] ).InvY := 999;
            j := j + 1;
          //Set the coordinates of the new item and Plot it
            pTempItems( GroundOrderList.Items[ j ] ).InvX := 288;
            pTempItems( GroundOrderList.Items[ j ] ).InvY := 377;
            pr := Rect( 0, 0, cGroundListWidth, cGroundListHeight );
            lpDDSBack.BltFast( pTempItems( GroundOrderList.Items[ j ] ).InvX + Offset.X, pTempItems( GroundOrderList.Items[ j ] ).InvY+Offset.Y, pTempItems( GroundOrderList.Items[ j ] ).DXSurfaceIcon, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
            TopGroundIndex := j;
          end
          else
          begin
          //making an obnoxious buzzing noise? - we cant go forwards from the last item
          end;
        end;
      end
      else if PtInRect( ApplyOffset( Rect( 287, 363, 376, 406) ), Point( x, y) ) and ( CurrentSelectedItem = -1 ) then
      begin //over the ground slot
        //If we are pulling this from the ground slot, pick a new top item
        if GroundOrderList.Count > 0 then
        begin
          CurrentSelectedItem := ItemList.IndexOf( GroundOrderList.items[ TopGroundIndex ] );
          if Button = mbRight then
          begin
            DlgScroll.OpenStatsScroll( pTempItems( ItemList.Items[ CurrentSelectedItem ] ).pItem, Offset.X, Offset.Y );
            CurrentSelectedItem := -1;
          end
          else
          begin
            pr := Rect( 287, 376, 363, 406 );
            lpDDSBack.BltFast( 287 + Offset.X, 376 + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT ); //clean the box
            if GroundOrderList.Count > 1 then
            begin //get the next item on the ground and show it
              j := GroundOrderList.IndexOf( ItemList.Items[ CurrentSelectedItem ] );
              if ( j = ( GroundOrderList.Count - 1 ) ) then //if its the last item in the list
                j := 0 //set it to the first one
              else //set it to the item folowing this one
                j := j + 1;
              pTempItems( GroundOrderList.Items[ j ] ).InvX := 288; //325-pTempItems(GroundOrderList.Items[j]).IW div 2;
              pTempItems( GroundOrderList.Items[ j ] ).InvY := 377; //391-pTempItems(GroundOrderList.Items[j]).IH div 2;
              pr := Rect( 0, 0, cGroundListWidth, cGroundListHeight );
              lpDDSBack.BltFast( pTempItems( GroundOrderList.Items[ j ] ).InvX + Offset.X, pTempItems( GroundOrderList.Items[ j ] ).InvY + Offset.Y, pTempItems( GroundOrderList.Items[ j ] ).DXSurfaceIcon, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
              pTemp := GroundOrderList.Items[ j ]; //save the pointer to the new topmost item so we can do the delete and still track it
              GroundOrderList.Delete( GroundOrderList.IndexOf( ItemList.Items[ CurrentSelectedItem ] ) ); //remove this item from the GroundList pointer list
              TopGroundIndex := GroundOrderList.IndexOf( pTempItems( pTemp ) );
            end
            else
            begin
              GroundOrderList.Delete( GroundOrderList.IndexOf( ItemList.Items[ CurrentSelectedItem ] ) ); //remove this item from the GroundList pointer list
              TopGroundIndex := 0;
            end;
            //Compute the coords for the floating item
            Tx := ( X - Offset.X ) - cGroundListWidth div 2; //pTempItems(ItemList.Items[CurrentSelectedItem]).W div 2;
            Ty := ( Y - Offset.Y ) - cGroundListHeight div 2; //pTempItems(ItemList.Items[CurrentSelectedItem]).H div 2;
            //Plot relevant text
            pr := Rect( ClearLeft, ClearTop, ClearRight, ClearBottom );
            lpDDSBack.BltFast( ClearLeft + Offset.X, ClearTop + Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot test
            if UseSmallFont then
              pText.PlotTinyTextBlock( GetSlotText, ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha )
            else
              PlotText( GetSlotText, ClearLeft, LrgMsg, Alpha );
            //save the background to the dirty DD surface based on the floating item
            pr := ApplyOffset( Rect( Tx, Ty, Tx + cGroundListWidth, Ty + cGroundListHeight ) );
            DXDirty.BltFast( 0, 0, lpDDSBack, @pr, DDBLTFAST_WAIT );
            //plot the item centered under the mouse pointer
            pr := Rect( 0, 0, cGroundListWidth, cGroundListHeight );
            lpDDSBack.BltFast( Tx + Offset.X, Ty + Offset.Y, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).DXSurfaceIcon, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
            ContainCursor( 1 );
          end; //if Button = mbRight
        end //if GroundOrderList > 0
      end
      else if PtInRect( ApplyOffset( rect( 46, 38, 298, 353 ) ), point( x, y ) ) or
        PtInRect( ApplyOffset( rect( 356, 38, 607, 353 ) ), point( x, y ) ) then
      begin ////Select an item to sell or buy
        i := 0;
        while ( i < ItemList.Count ) and ( CurrentSelectedItem = -1 ) do
        begin
          if ( pTempItems( ItemList.Items[ i ] ).WhoHasThis = 1 ) or ( pTempItems( ItemList.Items[ i ] ).WhoHasThis = 4 ) then
          begin
            if PtInRect( ApplyOffset( pTempItems( ItemList.Items[ i ] ).cRect ), point( x, y ) ) then
            begin
              CurrentSelectedItem := i; //assign it for the sake of PlotText
//                if UseSmallFont then
              pText.PlotTinyTextBlock( ( GetSlotText + txtMessage[ 5 ] ), ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha );
//                else
//                  pText.PlotText((GetSlotText + txtMessage[5]), ClearLeft,LrgMsg,Alpha);
              if Button = mbRight then
              begin
                DlgScroll.OpenStatsScroll( pTempItems( ItemList.Items[ CurrentSelectedItem ] ).pItem, Offset.X, Offset.Y );
                CurrentSelectedItem := -1;
              end
              else
              begin
                CurrentSelectedListItem := i;
                ShowLeftList;
                ShowRightList;
                CurrentSelectedItem := -1;
              end; //endif button
            end; //endif
          end //endif WhohasThis
          else if pTempItems( ItemList.Items[ i ] ).WhoHasThis = 2 then
          begin //merchant list
            if PtInRect( ApplyOffset( pTempItems( ItemList.Items[ i ] ).cRect ), point( x, y ) ) then
            begin
              CurrentSelectedItem := i; //assign it for the sake of PlotText
//                if UseSmallFont then
              pText.PlotTinyTextBlock( ( GetSlotText + txtMessage[ 5 ] ), ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha );
//                else
//                  pText.PlotText((GetSlotText + txtMessage[5]), ClearLeft,LrgMsg,Alpha);
              if Button = mbRight then
              begin
                DlgScroll.OpenStatsScroll( pTempItems( ItemList.Items[ CurrentSelectedItem ] ).pItem, Offset.X, Offset.Y );
                CurrentSelectedItem := -1;
              end
              else
              begin
                CurrentSelectedListItem := i;
                ShowLeftList;
                ShowRightList;
                CurrentSelectedItem := -1;
              end; //endif button
            end;
          end; //endif
          i := i + 1;
        end; //wend
      end
    //else if PtInRect(rect(356,38,607,353),point(x,y)) then begin ////Select an item to buy
    //end
      else
      begin //click an arrow to scroll list up or down
        i := 0;
        while i < 4 do
        begin
          if PtinRect( ApplyOffset( rect( ScrollArrows[ i ].X, ScrollArrows[ i ].Y, ScrollArrows[ i ].X + 23, ScrollArrows[ i ].Y + 32 ) ), point( X, Y ) ) then
          begin
            pr := Rect( 0, 0, 23, 32 );
            lpDDSBack.BltFast( ScrollArrows[ i ].X + Offset.X, ScrollArrows[ i ].Y + Offset.Y, ScrollArrows[ i + 4 ].DX, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
            if ( i < 2 ) and ( PlayerScroll > 0 ) then
            begin
              PlayerScroll := PlayerScroll - 1;
              ScrollStateLeft := -3;
            end
            else if ( i > 1 ) and ( PlayerScroll < ( NumberOfPlayerItems - 9 ) ) then
            begin
              PlayerScroll := PlayerScroll + 1;
              ScrollStateLeft := 3;
            end;

            ShowLeftList;
            i := 99;
               //plot a bit of informative text
            pr := Rect( ClearLeft, ClearTop, ClearRight, ClearBottom );
            lpDDSBack.BltFast( ClearLeft + Offset.X, ClearTop + Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
            if UseSmallFont then
              pText.PlotTinyTextBlock( txtMessage[ 6 ], ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha )
            else
              PlotText( txtMessage[ 6 ], ClearLeft, LrgMsg, Alpha );
          end;
          i := i + 1;
        end; //wend
        if i < 99 then
        begin //we didnt find anything on the left side arrows- check for right side
          i := 0;
          while i < 4 do
          begin
            if PtinRect( ApplyOffset( rect( ScrollArrows[ i ].X + 310, ScrollArrows[ i ].Y, ScrollArrows[ i ].X + 23 + 310, ScrollArrows[ i ].Y + 32 ) ), point( X, Y ) ) then
            begin
              pr := Rect( 0, 0, 23, 32 );
              lpDDSBack.BltFast( ScrollArrows[ i ].X + 310 + Offset.X, ScrollArrows[ i ].Y + Offset.Y, ScrollArrows[ i + 4 ].DX, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

              if ( i < 2 ) and ( MerchantScroll > 0 ) then
              begin
                MerchantScroll := MerchantScroll - 1;
                ScrollStateRight := -3;
              end
              else if ( i > 1 ) and ( MerchantScroll < ( NumberOfMerchantItems - 9 ) ) then
              begin
                MerchantScroll := MerchantScroll + 1;
                ScrollStateRight := 3;
              end;
              ShowRightList;
              i := 99;
                   //plot a bit of informative text
              pr := Rect( ClearLeft, ClearTop, ClearRight, ClearBottom );
              lpDDSBack.BltFast( ClearLeft + Offset.X, ClearTop + Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
              if UseSmallFont then
                pText.PlotTinyTextBlock( txtMessage[ 11 ], ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha )
              else
                PlotText( txtMessage[ 11 ], ClearLeft, LrgMsg, Alpha )
            end;
            i := i + 1;
          end; //wend
        end; //endif i<99
      end; //endif DlgScroll
    end
    else
    begin //drop the piece if we can
    //cleanup
      pr := Rect( 0, 0, cGroundListWidth, cGroundListHeight );
      lpDDSBack.BltFast( Tx + Offset.X, Ty + Offset.Y, DXDirty, @pr, DDBLTFAST_WAIT );
    //try to drop on ground
      if intersectRect( rRect, rect( 287, 376, 363, 406 ), rect( Tx, Ty, Tx + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, Ty + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ) ) then
      begin
        DropOnGround( CurrentSelectedItem );
        CurrentSelectedItem := -1;
        ContainCursor( 0 );
      end
      else if PtInRect( ApplyOffset( rect( 46, 38, 298, 353 ) ), point( x, y ) ) then
      begin //try to drop it in player inventory
        i := CurrentSelectedItem; //We have to do this- CurrentSelectedItem gets re-initialized in Collisiondetect
        if ItemFitsInInventory( i ) = false then
        begin //not enough room in inventory
          DropOnGround( i );
          CurrentSelectedItem := -1;
           //paint;
          pr := Rect( 20, 412, 20 + 550, 412 + 25 );
          lpDDSBack.BltFast( 20 + Offset.X, 412 + Offset.X, DXBack, @pr, DDBLTFAST_WAIT ); //clean up before we plot text
          PlotText( txtMessage[ 3 ], 20, 412, Alpha );
        end
        else
        begin //we did have enough room
          ItemList.move( i, ItemList.count - 1 ); //move it to end of list
          CurrentSelectedItem := -1;
          paint;
        end;
        ContainCursor( 0 );
      end;

    end; //endif

    SoAOS_DX_BltFront;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TMerchant.MouseDown

procedure TMerchant.MouseMove( Sender : TObject;
      Shift : TShiftState; X, Y, GridX, GridY : Integer );
var
  Tw, Th : Integer;
  i : Integer;
  pr : TRect;
const
  FailName : string = 'TMerchant.MouseDown';
begin
  try
    //This assigned(DXBack) is here to keep the program from crashing while Im developing it
    if ( CurrentSelectedItem > -1 ) and Assigned( DXBack ) then
    begin //are we dragging an item?
    //clean up
      pr := Rect( 0, 0, cGroundListWidth, cGroundListHeight );
      lpDDSBack.BltFast( Tx + Offset.X, Ty + Offset.Y, DXDirty, @pr, DDBLTFAST_WAIT );
    //Compute the coords for the floating item
      Tx := ( X - Offset.X ) - cGroundListWidth div 2;
      Ty := ( Y - Offset.Y ) - cGroundListHeight div 2;
      if Tx < 0 then
        Tx := 0;
      if Ty < 0 then
        Ty := 0;
      Tw := cGroundListWidth;
      if ( Tx + Tw ) > 659 then
        Tx := 659 - Tw;

      Th := cGroundListHeight;
      if ( Ty + Th ) > 463 then
        Ty := 463 - Th;

    //save the background to the dirty DD surface based on the floating item
      pr := ApplyOffset( Rect( Tx, Ty, Tx + Tw, Ty + Th ) );
      DXDirty.BltFast( 0, 0, lpDDSBack, @pr, DDBLTFAST_WAIT );
    //plot the item centered under the mouse pointer
      pr := Rect( 0, 0, cGroundListWidth, cGroundListHeight );
      lpDDSBack.BltFast( Tx + Offset.X , Ty + Offset.Y, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).DXSurfaceIcon, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      SoAOS_DX_BltFront;
    end
    else if Assigned( DXBack ) and ( DlgScroll.ScrollIsShowing = False ) then
    begin //do the rollover
      i := 0; //find the item the mouse is down over
      pr := Rect( ClearLeft, ClearTop, ClearRight, ClearBottom );
      lpDDSBack.BltFast( ClearLeft + Offset.X, ClearTop + Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
      while ( i < ItemList.Count ) and ( CurrentSelectedItem = -1 ) do
      begin
        if ( pTempItems( ItemList.Items[ i ] ).WhoHasThis = 1 ) or ( pTempItems( ItemList.Items[ i ] ).WhoHasThis = 4 ) then
        begin
          if PtInRect( ApplyOffset( pTempItems( ItemList.Items[ i ] ).cRect ), point( x, y ) ) then
          begin
            CurrentSelectedItem := i; //assign it for the sake of PlotText
//            if UseSmallFont then
            pText.PlotTinyTextBlock( ( GetSlotText + txtMessage[ 5 ] ), ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha );
//            else
//              pText.PlotText((GetSlotText + txtMessage[5]), ClearLeft,LrgMsg,Alpha);
          end;
        end
        else if pTempItems( ItemList.Items[ i ] ).WhoHasThis = 2 then
        begin //merchant list
          if PtInRect( ApplyOffset( pTempItems( ItemList.Items[ i ] ).cRect ), point( x, y ) ) then
          begin
            CurrentSelectedItem := i; //assign it for the sake of PlotText
//            if UseSmallFont then
            pText.PlotTinyTextBlock( ( GetSlotText + txtMessage[ 5 ] ), ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha );
//            else
//              pText.PlotText((GetSlotText + txtMessage[5]), ClearLeft,LrgMsg,Alpha);
          end;
        end; //endif
        i := i + 1;
      end; //wend
    //If we arent over an item see if we're over the ground slot
      if PtInRect( ApplyOffset( Rect( 287, 363, 363, 406 ) ), Point( x, y ) ) and ( CurrentSelectedItem = -1 ) then
      begin //over the ground slot
        if GroundOrderList.Count > 0 then
        begin
          CurrentSelectedItem := ItemList.IndexOf( GroundOrderList.items[ TopGroundIndex ] );
//          if UseSmallFont then
          pText.PlotTinyTextBlock( ( GetSlotText + txtMessage[ 5 ] ), ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha );
//          else
//            pText.PlotText((GetSlotText + txtMessage[5]), ClearLeft,LrgMsg,Alpha);
        end;
      end; //endif
    //Clean up arrows and back to game
    //lpDDSBack.BltFast(300, 194, DXBack, Rect(300, 194, 348, 250), DDBLTFAST_WAIT);
    //Plot the arrows cleanup
      for i := 0 to 3 do
      begin
        pr := Rect( ScrollArrows[ i ].X, ScrollArrows[ i ].Y, ScrollArrows[ i ].X + 23, ScrollArrows[ i ].Y + 32 );
        lpDDSBack.BltFast( ScrollArrows[ i ].X + Offset.X, ScrollArrows[ i ].Y + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
        pr := Rect( ScrollArrows[ i ].X + 310, ScrollArrows[ i ].Y, ScrollArrows[ i ].X + 23 + 310, ScrollArrows[ i ].Y + 32 );
        lpDDSBack.BltFast( ScrollArrows[ i ].X + 310 + Offset.X, ScrollArrows[ i ].Y + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
      end; //end for
    //ground arrows back to game cleanup
      pr := Rect( 271, 385, 271 + 15, 385 + 20 );
      lpDDSBack.BltFast( 271 + Offset.X, 385 + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
      pr := Rect( 364, 385, 364 + 12, 385 + 20 );
      lpDDSBack.BltFast( 364 + Offset.X, 385 + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
      pr := Rect( 588, 407, 588 + 77, 407 + 54 );
      lpDDSBack.BltFast( 588 + Offset.X, 407 + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
    //clean up buy and sell
      pr := Rect( 385, 362, 385 + 192, 362 + 24 );
      lpDDSBack.BltFast( 385 + Offset.X, 362 + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
      pr := Rect( 76, 362, 76 + 192, 362 + 24 );
      lpDDSBack.BltFast( 76 + Offset.X, 362 + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
    //now replot the text
      PlotTextCentered( txtMessage[ 1 ], 46, 298, 362, BuySellAlpha );
      PlotTextCentered( txtMessage[ 2 ], 355, 607, 362, BuySellAlpha );
    //Clean up secondary message line
      if not ( PtInRect( ApplyOffset( Rect( 436, 362, 436 + 87, 362 + 24 ) ), point( x, y ) ) or
        PtInRect( ApplyOffset( rect( 46, 38, 298, 353 ) ), point( x, y ) ) ) then
      begin
        pr := Rect( 20, 412, 20 + 550, 412 + 23 );
        lpDDSBack.BltFast( 20 + Offset.X, 412 + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
      end;

      if CurrentSelectedItem = -1 then
      begin //If we arent over an item then check arrows and back button
        if PtinRect( ApplyOffset( rect( 271, 375, 287, 407 ) ), point( X, Y ) ) then
        begin //over left arrow
          //plot highlighted arrow
          pr := Rect( 0, 0, 14, 15 );
          lpDDSBack.BltFast( 272 + Offset.X, 385 + Offset.Y, DXLeftArrow, @pr, DDBLTFAST_WAIT );
          //plot a bit of informative text
          pr := Rect( ClearLeft, ClearTop, ClearRight, ClearBottom );
          lpDDSBack.BltFast( ClearLeft + Offset.X, ClearTop + Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
          if UseSmallFont then
            pText.PlotTinyTextBlock( txtMessage[ 7 ], ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha )
          else
            PlotText( txtMessage[ 7 ], ClearLeft, LrgMsg, Alpha );
        end
        else if PtinRect( ApplyOffset( rect( 364, 375, 376, 407 ) ), point( X, Y ) ) then
        begin //over right arrow
          //plot highlighted arrow
          pr := Rect( 0, 0, 11, 11 );
          lpDDSBack.BltFast( 365 + Offset.X, 387 + Offset.Y, DXRightArrow, @pr, DDBLTFAST_WAIT );
          //plot a bit of informative text
          pr := Rect( ClearLeft, ClearTop, ClearRight, ClearBottom );
          lpDDSBack.BltFast( ClearLeft + Offset.X, ClearTop + Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
          if UseSmallFont then
            pText.PlotTinyTextBlock( txtMessage[ 8 ], ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha )
          else
            PlotText( txtMessage[ 8 ], ClearLeft, LrgMsg, Alpha );
        end
        else if PtinRect( ApplyOffset( rect( 588, 407, 588 + 77, 412 + 54 ) ), point( X, Y ) ) then
        begin //over back button
          //plot highlighted back to game
          pr := Rect( 0, 0, 77, 54 );
          lpDDSBack.BltFast( 588 + Offset.X, 407 + Offset.Y, DXBackToGame, @pr, DDBLTFAST_WAIT );
          //don't plot a bit of informative text, just clean up
          pr := Rect( ClearLeft, ClearTop, ClearRight, ClearBottom );
          lpDDSBack.BltFast( ClearLeft + Offset.X, ClearTop + Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
        end
        else if PtinRect( ApplyOffset( rect( 436, 362, 436 + 87, 362 + 24 ) ), point( X, Y ) ) then
        begin //over buy item
          //plot highlighted BuyItem
          pr := Rect( 0, 0, 192, 24 );
          lpDDSBack.BltFast( 385 + Offset.X, 362 + Offset.Y, DXBuyItem, @pr, DDBLTFAST_WAIT );
          //plot a bit of informative text, then clean up
          pr := Rect( ClearLeft, ClearTop, ClearRight, ClearBottom );
          lpDDSBack.BltFast( ClearLeft + Offset.X, ClearTop + Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
          if UseSmallFont then
            pText.PlotTinyTextBlock( txtMessage[ 9 ], ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha )
          else
            PlotText( txtMessage[ 9 ], ClearLeft, LrgMsg, Alpha );
        end
        else if PtinRect( ApplyOffset( rect( 131, 361, 131 + 82, 361 + 24 ) ), point( X, Y ) ) then
        begin //over sell item
          //plot highlighted Sell Item
          pr := Rect( 0, 0, 192, 24 );
          lpDDSBack.BltFast( 76 + Offset.X, 362 + Offset.Y, DXSellItem, @pr, DDBLTFAST_WAIT );
          //plot a bit of informative text, then clean up
          pr := Rect( ClearLeft, ClearTop, ClearRight, ClearBottom );
          lpDDSBack.BltFast( ClearLeft + Offset.X, ClearTop + Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
          if UseSmallFont then
            pText.PlotTinyTextBlock( txtMessage[ 10 ], ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha )
          else
            PlotText( txtMessage[ 10 ], ClearLeft, LrgMsg, Alpha );
        end
        else
        begin //if PtinRect(rect(300,225,347,247),point(X,Y)) then begin //over left ALL arrow
          //plot highlighted arrow
          i := 0;
          while i < 4 do
          begin
            if PtinRect( ApplyOffset( rect( ScrollArrows[ i ].X, ScrollArrows[ i ].Y, ScrollArrows[ i ].X + 23, ScrollArrows[ i ].Y + 32 ) ), point( X, Y ) ) then
            begin
              pr := Rect( 0, 0, 23, 32 );
              lpDDSBack.BltFast( ScrollArrows[ i ].X  + Offset.X, ScrollArrows[ i ].Y  + Offset.Y, ScrollArrows[ i + 4 ].DX, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
              i := 99;
                 //plot a bit of informative text
              pr := Rect( ClearLeft, ClearTop, ClearRight, ClearBottom );
              lpDDSBack.BltFast( ClearLeft + Offset.X, ClearTop + Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
              if UseSmallFont then
                pText.PlotTinyTextBlock( txtMessage[ 6 ], ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha )
              else
                PlotText( txtMessage[ 6 ], ClearLeft, LrgMsg, Alpha )
            end;
            i := i + 1;
          end; //wend
          if i < 99 then
          begin //we didnt find anything on the left side arrows- check for right side
            i := 0;
            while i < 4 do
            begin
              if PtinRect( ApplyOffset( rect( ScrollArrows[ i ].X + 310, ScrollArrows[ i ].Y, ScrollArrows[ i ].X + 23 + 310, ScrollArrows[ i ].Y + 32 ) ), point( X, Y ) ) then
              begin
                pr := Rect( 0, 0, 23, 32 );
                lpDDSBack.BltFast( ScrollArrows[ i ].X + 310 + Offset.X, ScrollArrows[ i ].Y + Offset.Y, ScrollArrows[ i + 4 ].DX, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
                i := 99;
                     //plot a bit of informative text
                pr := Rect( ClearLeft, ClearTop, ClearRight, ClearBottom );
                lpDDSBack.BltFast( ClearLeft + Offset.X, ClearTop + Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
                if UseSmallFont then
                  pText.PlotTinyTextBlock( txtMessage[ 11 ], ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha )
                else
                  PlotText( txtMessage[ 11 ], ClearLeft, LrgMsg, Alpha )
              end;
              i := i + 1;
            end; //wend
          end; //endif i<99
        end
      end; //endif CurrentSelectedItem

      CurrentSelectedItem := -1; //deassign it
      SoAOS_DX_BltFront;
    end;

  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TMerchant.MouseMove

procedure TMerchant.MouseUp( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer );
const
  FailName : string = 'TMerchant.MouseUp';
begin
  try
    DlgScroll.KeepOnScrolling := false;
    ScrollStateLeft := 0;
    ScrollStateRight := 0;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

procedure TMerchant.Paint;
var
  i : Integer;
  pr : TRect;
const
  FailName : string = 'TMerchant.Paint';
begin
  Log.DebugLog( FailName );
  try
    pr := Rect( 0, 0, 679, 476 );
    lpDDSBack.BltFast( Offset.X, Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
  //Now plot all of the items on the grid(s), and ground slots
    for i := 0 to ItemList.Count - 1 do
    begin
      if pTempItems( ItemList.Items[ i ] ).WhoHasThis = 3 then //if in ground slot plot icon
      begin
        pr := Rect( 0, 0, cGroundListWidth, cGroundListHeight );
        lpDDSBack.BltFast( pTempItems( ItemList.Items[ i ] ).InvX + Offset.X, pTempItems( ItemList.Items[ i ] ).InvY + Offset.Y, pTempItems( ItemList.Items[ i ] ).DXSurfaceIcon, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      end;
    // else
    //    lpDDSBack.BltFast(pTempItems(ItemList.Items[i]).InvX, pTempItems(ItemList.Items[i]).InvY, pTempItems(ItemList.Items[i]).DXSurface, Rect(0, 0, pTempItems(ItemList.Items[i]).W, pTempItems(ItemList.Items[i]).H), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT)
    end;
    ShowLeftList;
    ShowRightList;
  //Now put the names up
    PlotText( Character.name, 43, 10, Alpha );
    PlotText( Merchant.name, 355, 10, Alpha );
    PlotText( IntToStr( Character.money ) + txtMessage[ 0 ], 297 - pText.TextLength( IntToStr( Character.money ) + txtMessage[ 0 ] ), 10, Alpha );
  //now the Buy and Sell buttons
    PlotTextCentered( txtMessage[ 1 ], 46, 298, 362, BuySellAlpha );
    PlotTextCentered( txtMessage[ 2 ], 355, 607, 362, BuySellAlpha );

    SoAOS_DX_BltFront;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TMerchant.Paint;

function TMerchant.CollisionCheck( X, Y : Integer ) : Boolean;
var
  i : Integer;
  R1, R2, R3 : TRect; //R1 is the rect desribing the dragged item's destination R2 is a given inventory item rect, r3 is the result rect if collison (unused)
  k : Boolean;
  CollisionHasNotOccured : Boolean;

const
  FailName : string = 'TMerchant.CollisionCheck';
begin
  Log.DebugLog( FailName );
  result := true;
  try
    CollisionHasNotOccured := True;
  //first get the rectangle desribing the area where this item will land on the grid
    R1.Left := Integer( ( X - 18 - ( pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W div 2 ) ) div 18 ) * 18 + 27;
    R1.Top := Integer( ( Y - 32 - ( pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H div 2 ) ) div 26 ) * 26 + 42;
    R1.Right := R1.Left + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W;
    R1.Bottom := R1.Top + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H;

    for i := 0 to ItemList.Count - 1 do
    begin //check where we will land vs all other inv items for collision
      if ( i <> CurrentSelectedItem ) and ( pTempItems( ItemList.Items[ i ] ).WhoHasThis = 1 ) then
      begin //if this isnt the dragged item check for collision
        R2.Left := pTempItems( ItemList.Items[ i ] ).InvX; //stuff this inventory item into a rect
        R2.Right := R2.Left + pTempItems( ItemList.Items[ i ] ).W;
        R2.Top := pTempItems( ItemList.Items[ i ] ).InvY;
        R2.Bottom := R2.Top + pTempItems( ItemList.Items[ i ] ).H;
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
end; //TMerchant.CollisionCheck


function TMerchant.ItemFitsInInventory( ItemIndex : integer ) : boolean;
var
  j, k : integer;
  gWidth : integer;
  gHeight : integer;

const
  FailName : string = 'TMerchant.ItemFitsInInventory';
begin
  Log.DebugLog( FailName );
  result := false;
  try
    gWidth := 6 * 2; //each grid is 2x2
    gHeight := 7 * 2;
    k := 0;
    j := 0;
    while j <= ( gWidth - pTempItems( ItemList.Items[ ItemIndex ] ).pItem.InvW ) do
    begin //try to squeeze it in start upper left going to lower right
      k := 0;
      while k <= ( gHeight - pTempItems( ItemList.Items[ ItemIndex ] ).pItem.InvH ) do
      begin
        CurrentSelectedItem := ItemIndex;
        if CollisionCheck( ( j * 18 ) + 27 + pTempItems( ItemList.Items[ ItemIndex ] ).W div 2, ( k * 26 ) + 42 + pTempItems( ItemList.Items[ ItemIndex ] ).H div 2 ) then
        begin //if it fits, stick it in there
          pTempItems( ItemList.Items[ ItemIndex ] ).InvX := j * 18 + 27;
          pTempItems( ItemList.Items[ ItemIndex ] ).InvY := k * 26 + 42;
          pTempItems( ItemList.Items[ ItemIndex ] ).WhoHasThis := 1; //destination character/container
          k := 99;
          j := 99; //kick out- we've placed it
        end;

        k := k + 1;
      end; //wend
      j := j + 1;
    end; //wend

    CurrentSelectedItem := -1; //clear it
   //paint;  //refresh the screen
    if k > 99 then
      Result := True
    else
      Result := False;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TMerchant.ItemFitsInInventory



function TMerchant.GetSlotText : string;
var
  Sentence : string;
  LoopCount, dl : integer;
const
  FailName : string = 'TMerchant.GetSlotText';
begin
  Log.DebugLog( FailName );
  result := 'failed';
  try
    Sentence := pTempItems( ItemList.Items[ CurrentSelectedItem ] ).PItem.Name;
    Sentence := trim( Sentence );
    LoopCount := 0;
    while ( pText.TextLength( Sentence ) > 300 ) and ( LoopCount < 10 ) do
    begin
      dl := LastDelimiter( ' ', Sentence );
      SetLength( Sentence, dl - 1 );
      inc( LoopCount );
    end;

  //Sentence := pTempItems(ItemList.Items[CurrentSelectedItem]).PItem.Name;
    Result := ( Sentence );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TMerchant.GetSlotText

function TMerchant.GetSHORTSlotText( S : string ) : string;
var
  Sentence : string;
  LoopCount, dl : integer;
const
  FailName : string = 'TMerchant.GetSHORTSlotText';
begin
  Log.DebugLog( FailName );
  result := 'failed';
  try
    Sentence := S;
    Sentence := trim( Sentence );
    LoopCount := 0;
    while ( pText.TinyTextLength( Sentence ) > 170 ) and ( LoopCount < 10 ) do
    begin
      dl := LastDelimiter( ' ', Sentence );
      SetLength( Sentence, dl - 1 );
      inc( LoopCount );
    end;
    if ( LoopCount > 0 ) and ( pText.TinyTextLength( Sentence ) < 165 ) then
    begin //we truncated
      Sentence := Sentence + '...';
    end;
  //Sentence := pTempItems(ItemList.Items[CurrentSelectedItem]).PItem.Name;
    Result := ( Sentence );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TMerchant.GetSHORTSlotText


procedure TMerchant.WriteTheInventoryData;
var
  i : Integer;
  t : TSlot;
const
  FailName : string = 'TMerchant.Writetheinventorydata';
begin
  Log.DebugLog( FailName );
  try
  //Clear the Characters/Merchant Inventory
    Character.Inventory.Clear;
    Merchant.Inventory.Clear;
  //end;
  //Clear the equipment array
    for t := slLeg1 to slMisc3 do
    begin
      if not Character.EquipmentLocked[ t ] then
        Character.Equipment[ t ] := nil;
    end;

  //Assign the new values
    for i := 0 to ItemList.Count - 1 do
    begin
      if ( pTempItems( ItemList.Items[ i ] ).WhoHasThis = 1 ) or ( pTempItems( ItemList.Items[ i ] ).WhoHasThis = 4 ) then
      begin
        pTempItems( ItemList.Items[ i ] ).PItem.InvX := ( pTempItems( ItemList.Items[ i ] ).InvX - 27 ) div 18;
        pTempItems( ItemList.Items[ i ] ).PItem.InvY := ( pTempItems( ItemList.Items[ i ] ).InvY - 42 ) div 26;
        pTempItems( ItemList.Items[ i ] ).PItem.Enabled := False; //this is only true if an item in on the ground

       //Make sure part has correct resource for base type
        pTempItems( ItemList.Items[ i ] ).PItem.LayeredImage := PartManager.GetImageFile( pTempItems( ItemList.Items[ i ] ).PItem.PartName, TCharacterResource( Character.Resource ).NakedName );
        pTempItems( ItemList.Items[ i ] ).PItem.Resource := PartManager.GetLayerResource( pTempItems( ItemList.Items[ i ] ).PItem.LayeredImage );

        if pTempItems( ItemList.Items[ i ] ).WhoHasThis = 1 then
          Character.Inventory.Add( pTempItems( ItemList.Items[ i ] ).PItem )
        else if pTempItems( ItemList.Items[ i ] ).WhoHasThis = 4 then //equiped item
          Character.Equipment[ TSlot( pTempItems( ItemList.Items[ i ] ).BodySlot ) ] := pTempItems( ItemList.Items[ i ] ).PItem

      end
      else if pTempItems( ItemList.Items[ i ] ).WhoHasThis = 2 then
      begin
       //pTempItems(ItemList.Items[i]).PItem.InvX := (pTempItems(ItemList.Items[i]).InvX-GridRightMinX) div 18;
       //pTempItems(ItemList.Items[i]).PItem.InvY := (pTempItems(ItemList.Items[i]).InvY-GridRightMinY) div 26;
        pTempItems( ItemList.Items[ i ] ).PItem.Enabled := False; //this is only true if an item in on the ground
        Merchant.Inventory.Add( pTempItems( ItemList.Items[ i ] ).PItem )
      end
      else
      begin //its on the ground- WhoHasThis=3
      //put the item at the characters pos on the ground
        pTempItems( ItemList.Items[ i ] ).PItem.SetPos( Character.X, Character.Y, 0 );
        pTempItems( ItemList.Items[ i ] ).PItem.Enabled := True; //make it visible
      end; //endif
    end; //endfor

    if assigned( DrawGuy ) then
      DrawGuy( Character );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TMerchant.WriteTheInventoryData

procedure TMerchant.ContainCursor( action : integer );
var
  prRect : TRect;
const
  FailName : string = 'TMerchant.ContainCursor';
begin
  Log.DebugLog( FailName );
  try
    prRect.Left := Offset.X;
    prRect.Top := Offset.Y;
    ClientToScreen(frmMain.Handle, prRect.TopLeft);
    if Action = 1 then
    begin //restore to fullscreen
      prRect.bottom := prRect.Top + 478;
      prRect.Right := prRect.Left + 640;
      ClipCursor( @prRect ); //TODO: Windows-ism - replace
    end
    else
    begin //constrict to main inventory area
      ClipCursor(nil);
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TMerchant.ContainCursor

procedure TMerchant.BuildGrid;
var
  i, j : integer;
  DXGrid : IDirectDrawSurface; //DD surface holding our chunk O' grid to draw right grid with
  pr : TRect;
const
  FailName : string = 'TMerchant.BuildGrid';
begin
  Log.DebugLog( FailName );
  try
  //Load the grid graphic, and draw the left inventory area before we blit the screen to the backbuffer
    DXGrid := SoAOS_DX_LoadBMP( InterfacePath + 'merGrid.bmp', cInvisColor );

  //if OtherOb is TCharacter then begin
    for j := 0 to 8 do
    begin
      for i := 0 to 6 do
      begin
        DrawAlpha( DXBack, rect( ( 607 - 36 ) - i * 36, ( 352 - 35 ) - j * 35, ( 607 - 36 ) - i * 36 + 38, ( 352 - 35 ) - j * 35 + 37 ), rect( 0, 0, 38, 37 ), DXGrid, True, 160 );
        DrawAlpha( DXBack, rect( ( 297 - 36 ) - i * 36, ( 352 - 35 ) - j * 35, ( 297 - 36 ) - i * 36 + 38, ( 352 - 35 ) - j * 35 + 37 ), rect( 0, 0, 38, 37 ), DXGrid, True, 160 );
      end;
    end;
     //Player Grid on right
    GridRightMinX := 417;
    GridRightMaxX := 597 + 36;
    GridRightMinY := 40;
    GridRightMaxY := 352 + 52;
    DXGrid := nil; //dont need it anymore
  //plot the groundbox
    pr := Rect( 0, 0, 110, 54 );
    DXBack.BltFast( 269, 359, DXGroundBox, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
  //Plot the arrows
    for i := 0 to 3 do
    begin
      pr := Rect( 0, 0, 23, 32 );
      DXBack.BltFast( ScrollArrows[ i ].X, ScrollArrows[ i ].Y, ScrollArrows[ i ].DX, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      DXBack.BltFast( ScrollArrows[ i ].X + 310, ScrollArrows[ i ].Y, ScrollArrows[ i ].DX, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    end; //end for
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TMerchant.BuildGrid;


procedure TMerchant.ShowLeftList;
var
  i, j, k : integer;
  Cost : integer;
  pr : TRect;
const
  FailName : string = 'TMerchant.ShowLeftList';
begin
  Log.DebugLog( FailName );
  try
    j := 0;
    k := 1;
    pr := Rect( 46, 38, 298, 352 );
    lpDDSBack.BltFast( 46 + Offset.X, 38 + Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    for i := 0 to ItemList.Count - 1 do
    begin //dont show quest items in list
      if ( ( pTempItems( ItemList.Items[ i ] ).WhoHasThis = 1 ) or ( ( pTempItems( ItemList.Items[ i ] ).WhoHasThis = 4 ) and ( Locked = false ) ) ) and ( pTempItems( ItemList.Items[ i ] ).DXSurfaceIcon <> nil ) then
      begin //if in player list icon
        if ( k > PlayerScroll ) and ( k < PlayerScroll + 10 ) then
        begin //show 10 items
           //Set up the collision rect
          pTempItems( ItemList.Items[ i ] ).cRect.left := 46;
          pTempItems( ItemList.Items[ i ] ).cRect.top := j * 35 + 38;
          pTempItems( ItemList.Items[ i ] ).cRect.right := 298;
          pTempItems( ItemList.Items[ i ] ).cRect.bottom := pTempItems( ItemList.Items[ i ] ).cRect.top + 36;
          if CurrentSelectedListItem = i then //plot the highlight
            DrawAlpha( lpDDSBack, ApplyOffset( pTempItems( ItemList.Items[ CurrentSelectedListItem ] ).cRect ), rect( 0, 0, 25, 25 ), DXBackHighlight, False, 75 );
            pr := Rect( 0, 0, cGroundListWidth, cGroundListHeight );
          lpDDSBack.BltFast( 46 + Offset.X, j * 35 + 38 + 3 + Offset.Y, pTempItems( ItemList.Items[ i ] ).DXSurfaceIcon, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
           //plot the name
           //pText.PlotTinyText(pTempItems(ItemList.Items[i]).pItem.name,126,j*35+38-3,250);
          PlotTinyText( GetSHORTSlotText( pTempItems( ItemList.Items[ i ] ).pItem.name ), 126, j * 35 + 38 - 3, 250 );
           //If player is wearing this item, say so
          if pTempItems( ItemList.Items[ i ] ).BodySlot > -1 then
          begin //if hes wearing it, say so
            if pTempItems( ItemList.Items[ i ] ).BodySlot = 11 then
            begin //its a weapon
              PlotTinyText( txtMessage[ 12 ], 126, j * 35 + 38 - 3 + 18, 250 );
            end
            else
            begin //an item, or armour
              PlotTinyText( txtMessage[ 13 ], 126, j * 35 + 38 - 3 + 18, 250 )
            end;
          end;
           //Plot the sale value
          if Character.charm < 1 then
            Cost := Round( pTempItems( ItemList.Items[ i ] ).pItem.Value * ( 1 + 10 * ( Merchant.BuyingDiscount - 1 ) / 1 ) )
          else
            Cost := Round( pTempItems( ItemList.Items[ i ] ).pItem.Value * ( 1 + 10 * ( Merchant.BuyingDiscount - 1 ) / Character.Charm ) );
          if Cost < 0 then
            Cost := 0;
//           Cost:= Round(pTempItems(ItemList.Items[i]).pItem.Value*Merchant.BuyingDiscount);
          PlotTinyText( IntToStr( Cost ) + txtMessage[ 0 ], 297 - pText.TinyTextLength( IntToStr( Cost ) + txtMessage[ 0 ] ), j * 35 + 38 - 3 + 18, 250 );
          j := j + 1;
        end
        else
        begin //make sure the collision rect is not available
          pTempItems( ItemList.Items[ i ] ).cRect.left := -10;
          pTempItems( ItemList.Items[ i ] ).cRect.right := -5;
        end;
        k := k + 1;
      end;
    end; //end for
    NumberOfPlayerItems := k - 1;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TMerchant.ShowLeftList

procedure TMerchant.ShowRightList;
var
  i, j, k : integer;
  Cost : integer;
  UniqueItem : boolean; //index of added item to UniqueItemList
  UniqueItemList : TStringList;
  pr : TRect;
const
  FailName : string = 'TMerchant.ShowRightList';
begin
  Log.DebugLog( FailName );
  try
    UniqueItemList := TStringList.create; //create this string list to make sure we only show one of each kind of item
    UniqueItemList.sorted := true;
    UniqueItemList.duplicates := dupError;
    j := 0;
    k := 1;
    pr := Rect( 356, 38, 608, 352 );
    lpDDSBack.BltFast( 356 + Offset.X, 38 + Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    NumberOfMerchantItems := 0;
    for i := 0 to ItemList.Count - 1 do
    begin
      if pTempItems( ItemList.Items[ i ] ).WhoHasThis = 2 then
      begin //if in Merchant list icon
        //Make sure we dont have one of these already displayed
        UniqueItem := True;
        try
          UniqueItemList.add( pTempItems( ItemList.Items[ i ] ).pItem.ItemName );
        except
          UniqueItem := false;
        end;
        if ( k > MerchantScroll ) and ( k < MerchantScroll + 10 ) and ( UniqueItem ) and ( TItem( pTempItems( ItemList.Items[ i ] ).pItem ).Value > 0 ) then
        begin //show 10 items
           //Set up the collision rect
          pTempItems( ItemList.Items[ i ] ).cRect.left := 356;
          pTempItems( ItemList.Items[ i ] ).cRect.top := j * 35 + 38;
          pTempItems( ItemList.Items[ i ] ).cRect.right := 608;
          pTempItems( ItemList.Items[ i ] ).cRect.bottom := pTempItems( ItemList.Items[ i ] ).cRect.top + 36;
          if CurrentSelectedListItem = i then //plot the highlight
            DrawAlpha( lpDDSBack, ApplyOffset( pTempItems( ItemList.Items[ CurrentSelectedListItem ] ).cRect ), rect( 0, 0, 25, 25 ), DXBackHighlight, False, 75 );
          pr := Rect( 0, 0, cGroundListWidth, cGroundListHeight );
          lpDDSBack.BltFast( 356 + Offset.X, j * 35 + 38 + 3 + Offset.Y, pTempItems( ItemList.Items[ i ] ).DXSurfaceIcon, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
           //pText.PlotTinyText(pTempItems(ItemList.Items[i]).pItem.name,80+356,j*35+38-3,250);
          PlotTinyText( GetSHORTSlotText( pTempItems( ItemList.Items[ i ] ).pItem.name ), 80 + 356, j * 35 + 38 - 3, 250 );
           //Plot the sale value
          if Character.charm < 1 then
            Cost := Round( pTempItems( ItemList.Items[ i ] ).pItem.Value * ( 1 + 10 * ( Merchant.SellingMarkup - 1 ) / 1 ) )
          else
            Cost := Round( pTempItems( ItemList.Items[ i ] ).pItem.Value * ( 1 + 10 * ( Merchant.SellingMarkup - 1 ) / Character.Charm ) );
//           Cost:= Round(pTempItems(ItemList.Items[i]).pItem.Value*Merchant.SellingMarkup);
          PlotTinyText( IntToStr( Cost ) + txtMessage[ 0 ], 607 - pText.TinyTextLength( IntToStr( Cost ) + txtMessage[ 0 ] ), j * 35 + 38 - 3 + 18, 250 );
          j := j + 1;
           //k:=k+1;
        end
        else
        begin //make sure the collision rect is not available
          pTempItems( ItemList.Items[ i ] ).cRect.left := -10;
          pTempItems( ItemList.Items[ i ] ).cRect.right := -5;
        end;
        if UniqueItem and ( TItem( pTempItems( ItemList.Items[ i ] ).pItem ).Value > 0 ) then
        begin
          inc( NumberOfMerchantItems );
          k := k + 1;
        end;
      end;
    end; //end for
  //NumberOfMerchantItems:=k-1;
    UniqueItemList.free;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TMerchant.ShowRightList


procedure TMerchant.DropOnGround( ItemIndex : integer );
const
  FailName : string = 'TMerchant.DropOnground';
var
  pr : TRect;
begin
  Log.DebugLog( FailName );
  try
    if GroundOrderList.Count > 0 then
    begin //If we have any ground items
      pTempItems( GroundOrderList.Items[ TopGroundIndex ] ).InvX := 999; //put old item offscreen- no longer on top
      pTempItems( GroundOrderList.Items[ TopGroundIndex ] ).InvY := 999;
      GroundOrderList.Insert( TopGroundIndex, pTempItems( ItemList.Items[ ItemIndex ] ) );
    end
    else
    begin //there are no items in this list - this will automatically become zero (top spot)
      GroundOrderList.Add( pTempItems( ItemList.Items[ ItemIndex ] ) );
      TopGroundIndex := 0;
    end;
    pr := Rect( 287, 376, 363, 406 );
    lpDDSBack.BltFast( 287 + Offset.X, 376 + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT ); //clean out the Ground box
    pTempItems( ItemList.Items[ ItemIndex ] ).InvX := 288;
    pTempItems( ItemList.Items[ ItemIndex ] ).InvY := 377;
    pTempItems( ItemList.Items[ ItemIndex ] ).WhoHasThis := 3; //ground
    pr := Rect( 0, 0, cGroundListWidth, cGroundListHeight );
    lpDDSBack.BltFast( pTempItems( ItemList.Items[ ItemIndex ] ).InvX + Offset.X, pTempItems( ItemList.Items[ ItemIndex ] ).InvY + Offset.Y,
      pTempItems( ItemList.Items[ ItemIndex ] ).DXSurfaceIcon, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    pr := Rect( ClearLeft, ClearTop, ClearRight, ClearBottom );
    lpDDSBack.BltFast( ClearLeft + Offset.X, ClearTop + Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clear text
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TMerchant.DropOnGround

procedure TMerchant.Release;
var
  i : integer;
const
  FailName : string = 'TMerchant.release';
begin
  Log.DebugLog( FailName );
  try
    if assigned( Timer ) then
    begin
      Timer.enabled := false;
      Timer.free;
      Timer := nil;
    end;
    ScrollStateLeft := 0;
    ScrollStateRight := 0;

    ExText.Close;

    WriteTheInventoryData;

    pText.UnLoadTinyFontGraphic; //free the tinyfont graphic
    Locked := false;
    DlgScroll.free;
    DlgScroll := nil;
    DXBack := nil;
    DXRightArrow := nil;
    DXLeftArrow := nil;
    DXBuyItem := nil;
    DXSellItem := nil;
    DXBackToGame := nil;
    DXBackHighlight := nil;
    DxDirty := nil;
    DXGroundBox := nil;
    for i := 0 to 7 do
    begin
      ScrollArrows[ i ].DX := nil;
    end;
  //ItemList Barbie pic surface cleanup
 // for i := 0 to ItemList.Count - 1 do begin
    //if Assigned(pTempItems(ItemList.Items[i]).DXSurface) then
 //     pTempItems(ItemList.Items[i]).DXSurface := nil;
 // end;

  //ItemsList Cleanup
    if Assigned( ItemList ) then
    begin
      for i := 0 to ( ItemList.Count - 1 ) do
      begin
        pInventoryItem := pTempItems( ItemList.Items[ i ] );
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
end; //TMerchant.Release


procedure TMerchant.TimerEvent( Sender : TObject );
var
  P : TPoint;
  i : integer;
  pr : TRect;
begin
  if ScrollStateLeft <> 0 then
  begin
    P := frmMain.ScreenToClient(Mouse.CursorPos);
    i := 0;
    while i < 4 do
    begin
      if PtinRect( ApplyOffset( rect( ScrollArrows[ i ].X, ScrollArrows[ i ].Y, ScrollArrows[ i ].X + 23, ScrollArrows[ i ].Y + 32 ) ), P ) then
      begin
        pr := Rect( 0, 0, 23, 32 );
        lpDDSBack.BltFast( ScrollArrows[ i ].X + Offset.X , ScrollArrows[ i ].Y + Offset.Y, ScrollArrows[ i + 4 ].DX, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        if ( i < 2 ) and ( PlayerScroll > 0 ) then
        begin
          if ScrollStateLeft > 0 then
          begin
            ScrollStateLeft := 0;
            break;
          end
          else if ScrollStateLeft < -1 then
          begin
            inc( ScrollStateLeft );
            break;
          end;
          PlayerScroll := PlayerScroll - 1;
        end
        else if ( i > 1 ) and ( PlayerScroll < ( NumberOfPlayerItems - 9 ) ) then
        begin
          if ScrollStateLeft < 0 then
          begin
            ScrollStateLeft := 0;
            break;
          end
          else if ScrollStateLeft > 1 then
          begin
            dec( ScrollStateLeft );
            break;
          end;
          PlayerScroll := PlayerScroll + 1;
        end;

        ShowLeftList;
           //plot a bit of informative text
        pr := Rect( ClearLeft, ClearTop, ClearRight, ClearBottom );
        lpDDSBack.BltFast( ClearLeft + Offset.X, ClearTop + Offset.X, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
        if UseSmallFont then
          pText.PlotTinyTextBlock( txtMessage[ 6 ], ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha )
        else
          PlotText( txtMessage[ 6 ], ClearLeft, LrgMsg, Alpha );
        SoAOS_DX_BltFront;
        break;
      end;
      i := i + 1;
    end; //wend
    if i = 4 then
      ScrollStateLeft := 0;
    exit;
  end;

  if ScrollStateRight <> 0 then
  begin
    P := frmMain.ScreenToClient(Mouse.CursorPos);
    i := 0;
    while i < 4 do
    begin
      if PtinRect( ApplyOffset( rect( ScrollArrows[ i ].X + 310, ScrollArrows[ i ].Y, ScrollArrows[ i ].X + 23 + 310, ScrollArrows[ i ].Y + 32 ) ), P ) then
      begin
        pr := Rect( 0, 0, 23, 32 );
        lpDDSBack.BltFast( ScrollArrows[ i ].X + 310 + Offset.X, ScrollArrows[ i ].Y + Offset.Y, ScrollArrows[ i + 4 ].DX, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

        if ( i < 2 ) and ( MerchantScroll > 0 ) then
        begin
          if ScrollStateRight > 0 then
          begin
            ScrollStateRight := 0;
            break;
          end
          else if ScrollStateRight < -1 then
          begin
            inc( ScrollStateRight );
            break;
          end;
          MerchantScroll := MerchantScroll - 1;
        end
        else if ( i > 1 ) and ( MerchantScroll < ( NumberOfMerchantItems - 9 ) ) then
        begin
          if ScrollStateRight < 0 then
          begin
            ScrollStateRight := 0;
            break;
          end
          else if ScrollStateRight > 1 then
          begin
            dec( ScrollStateRight );
            break;
          end;
          MerchantScroll := MerchantScroll + 1;
        end;
        ShowRightList;
        pr := Rect( ClearLeft, ClearTop, ClearRight, ClearBottom );
        lpDDSBack.BltFast( ClearLeft + Offset.X, ClearTop + Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
        if UseSmallFont then
          pText.PlotTinyTextBlock( txtMessage[ 11 ], ClearLeft + Offset.X, ClearRight + Offset.X, SmlMsg + Offset.Y, Alpha )
        else
          PlotText( txtMessage[ 11 ], ClearLeft, LrgMsg, Alpha );

        SoAOS_DX_BltFront;
        break;
      end;
      i := i + 1;
    end; //wend
    if i = 4 then
      ScrollStateRight := 0;
  end;
end;

end.
