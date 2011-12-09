unit Merchant;
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
  Statistics,
  Parts,
  Scroll,
  Anigrp30,
  Engine,
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

  TMerchant = class( TDisplay )
  private
    //List stuff
    PlayerScroll : integer; //distance left inventory has scrolled
    MerchantScroll : integer; //distance right inventory has scrolled
    ScrollArrows : array[ 0..7 ] of Arrow; //Arrows to scroll inventory
    CurrentSelectedListItem : integer;
    NumberOfMerchantItems : integer;
    NumberOfPlayerItems : integer;
    //Bitmap stuff
    BMBack : TBitmap; //The inventory screen bitmap used for loading
    ItemList : TList; //the list of items
    pInventoryItem : pTempItems; //The temporary inventory and equipment items combined
    CurrentSelectedItem : Integer; //Current Item being dragged about
    Tx, Ty : Integer; // x and y locs used with the offset of the dragged item
{$IFDEF DirectX}
    DXBackHighlight : IDirectDrawSurface; //so we know which item is selected for buy/sell
    DXBack : IDirectDrawSurface; //DD surface that holds the inventory screen before blit
    DxDirty : IDirectDrawSurface; //DD for cleanup when dragging items
    DXLeftArrow : IDirectDrawSurface; //Inventory left arrow
    DXRightArrow : IDirectDrawSurface; //Inventory right arrow
    DXBuyItem : IDirectDrawSurface;
    DXSellItem : IDirectDrawSurface;
    DXBackToGame : IDirectDrawSurface; //Back To Game highlight
    DXGroundBox : IDirectDrawSurface; //The Ground Box Itself
{$ENDIF}
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
    procedure MouseDown( Sender : TAniview; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
    procedure MouseMove( Sender : TAniview;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
    procedure MouseUp( Sender : TAniview; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
  public
    Character : TCharacter; //the charachter we draw inventory from to fill left box
    Merchant : TCharacter;
    GroundList : TList; //Passed to us - list of items on the ground
    Locked : Bool;
    DrawGuy : TNotifyEvent;
    constructor Create;
    destructor Destroy; override;
    procedure Paint; override;
    procedure Init; override;
    procedure Release; override;
  end;
implementation
uses
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
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
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
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
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
  InvisColor : Integer; //Transparent color :RGB(0,255,255)
  i : Integer;
  DXBorder : IDirectDrawSurface;
  t : TSlot;
const
  FailName : string = 'TMerchant.init';
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
    BMBack := TBitmap.Create;
  //transparent color
    InvisColor := $00FFFF00;

  //We have to do this part up here in order to get coordinated from buildgrid
    BMBack.LoadFromFile( InterfacePath + 'merGroundBox.bmp' );
    DXGroundBox := DDGetImage( lpDD, BMBack, InvisColor, False );
    BMBack.LoadFromFile( InterfacePath + 'invRightArrow.bmp' );
    DXRightArrow := DDGetImage( lpDD, BMBack, InvisColor, False );
    BMBack.LoadFromFile( InterfacePath + 'invLeftArrow.bmp' );
    DXLeftArrow := DDGetImage( lpDD, BMBack, InvisColor, False );
    BMBack.LoadFromFile( InterfacePath + 'obInvBackToGame.bmp' );
    DXBackToGame := DDGetImage( lpDD, BMBack, InvisColor, False );
    BMBack.LoadFromFile( InterfacePath + 'merBackHighlight.bmp' );
    DXBackHighlight := DDGetImage( lpDD, BMBack, InvisColor, False );
{  BMBack.LoadFromFile(InterfacePath + 'merBuyItem.bmp');
  DXBuyItem := DDGetImage(lpDD, BMBack, InvisColor, False);
  BMBack.LoadFromFile(InterfacePath + 'merSellItem.bmp');
  DXSellItem := DDGetImage(lpDD, BMBack, InvisColor, False);   }
    DXBuyItem := DDGetSurface( lpDD, 192, 24, clBlack, false );
    DXSellItem := DDGetSurface( lpDD, 192, 24, clBlack, false );

  //Arrows Left up, right up, left down, right down, then darks
    BMBack.LoadFromFile( InterfacePath + 'meruparrowL.bmp' );
    ScrollArrows[ 0 ].DX := DDGetImage( lpDD, BMBack, InvisColor, False );
    ScrollArrows[ 0 ].X := 45 - 23;
    ScrollArrows[ 0 ].Y := 37;
    BMBack.LoadFromFile( InterfacePath + 'meruparrowR.bmp' );
    ScrollArrows[ 1 ].DX := DDGetImage( lpDD, BMBack, InvisColor, False );
    ScrollArrows[ 1 ].X := 298;
    ScrollArrows[ 1 ].Y := 37;
    BMBack.LoadFromFile( InterfacePath + 'merdownarrowL.bmp' );
    ScrollArrows[ 2 ].DX := DDGetImage( lpDD, BMBack, InvisColor, False );
    ScrollArrows[ 2 ].X := 45 - 23;
    ScrollArrows[ 2 ].Y := 353 - 32;
    BMBack.LoadFromFile( InterfacePath + 'merdownarrowR.bmp' );
    ScrollArrows[ 3 ].DX := DDGetImage( lpDD, BMBack, InvisColor, False );
    ScrollArrows[ 3 ].X := 298;
    ScrollArrows[ 3 ].Y := 353 - 32;
    BMBack.LoadFromFile( InterfacePath + 'meruparrowLD.bmp' );
    ScrollArrows[ 4 ].DX := DDGetImage( lpDD, BMBack, InvisColor, False );
    ScrollArrows[ 4 ].X := ScrollArrows[ 0 ].X;
    ScrollArrows[ 4 ].Y := ScrollArrows[ 0 ].Y;
    BMBack.LoadFromFile( InterfacePath + 'meruparrowRD.bmp' );
    ScrollArrows[ 5 ].DX := DDGetImage( lpDD, BMBack, InvisColor, False );
    ScrollArrows[ 5 ].X := ScrollArrows[ 1 ].X;
    ScrollArrows[ 5 ].Y := ScrollArrows[ 1 ].Y;
    BMBack.LoadFromFile( InterfacePath + 'merdownarrowLD.bmp' );
    ScrollArrows[ 6 ].DX := DDGetImage( lpDD, BMBack, InvisColor, False );
    ScrollArrows[ 6 ].X := ScrollArrows[ 2 ].X;
    ScrollArrows[ 6 ].Y := ScrollArrows[ 2 ].Y;
    BMBack.LoadFromFile( InterfacePath + 'merdownarrowRD.bmp' );
    ScrollArrows[ 7 ].DX := DDGetImage( lpDD, BMBack, InvisColor, False );
    ScrollArrows[ 7 ].X := ScrollArrows[ 3 ].X;
    ScrollArrows[ 7 ].Y := ScrollArrows[ 3 ].Y;
 //end of arrows


    BMBack.LoadFromFile( InterfacePath + 'Merchant.bmp' );
    DXBack := DDGetImage( lpDD, BMBack, InvisColor, False );
  //DxDirty := DDGetImage(lpDD, BMBack, InvisColor, False); //for now this is how we will do it
    DXDirty := DDGetSurface( lpDD, GroundListWidth, GroundListHeight, InvisColor, true );
  //build the left side inventory space
    BuildGrid;
  //now we blit the screen to the backbuffer
    WrapperBltFast( lpDDSBack, 0, 0, DXBack, Rect( 0, 0, BMBack.width, BMBack.Height ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
  //Now for the Alpha'ed edges
    BMBack.LoadFromFile( InterfacePath + 'obInvRightShadow.bmp' );
    DXBorder := DDGetImage( lpDD, BMBack, InvisColor, False );
    DrawSub( lpDDSBack, Rect( 659, 0, 659 + BMBack.Width, BMBack.Height ), Rect( 0, 0, BMBack.Width, BMBack.Height ), DXBorder, True, Alpha );

    DXBorder := nil;

    BMBack.LoadFromFile( InterfacePath + 'obInvBottomShadow.bmp' );
    DXBorder := DDGetImage( lpDD, BMBack, InvisColor, False );
    DrawSub( lpDDSBack, Rect( 0, 456, BMBack.Width, 456 + BMBack.Height ), Rect( 0, 0, BMBack.Width, BMBack.Height ), DXBorder, True, Alpha );

    DXBorder := nil; //release DXBorder

  //Now put the names up
    pText.PlotText( Character.name, 43, 10, Alpha );
    pText.PlotText( Merchant.name, 355, 10, Alpha );
    pText.PlotText( IntToStr( Character.money ) + txtMessage[ 0 ], 297 - pText.TextLength( IntToStr( Character.money ) + txtMessage[ 0 ] ), 10, Alpha );

  //now the Buy and Sell buttons
    WrapperBltFast( DXSellItem, 0, 0, DXBack, Rect( 76, 362, 268, 386 ), DDBLTFAST_WAIT );
    WrapperBltFast( DXBuyItem, 0, 0, DXBack, Rect( 385, 362, 577, 386 ), DDBLTFAST_WAIT );
    pText.PlotTextCentered2( DXSellItem, txtMessage[ 1 ], 0, 192, 0, 255 );
    pText.PlotTextCentered2( DXBuyItem, txtMessage[ 2 ], 0, 192, 0, 255 );

    pText.PlotTextCentered( txtMessage[ 1 ], 46, 298, 362, BuySellAlpha );
    pText.PlotTextCentered( txtMessage[ 2 ], 355, 607, 362, BuySellAlpha );

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


{$IFDEF DirectX}
  //release the bitmap
    BMBack.Free;
  //Now plot all of the items on the grid
    for i := 0 to ItemList.Count - 1 do
    begin
      //WrapperBltFast( lpDDSBack, pTempItems(ItemList.Items[i]).InvX, pTempItems(ItemList.Items[i]).InvY, pTempItems(ItemList.Items[i]).DXSurface, Rect(0, 0, pTempItems(ItemList.Items[i]).W, pTempItems(ItemList.Items[i]).H), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT)
      if pTempItems( ItemList.Items[ i ] ).WhoHasThis = 3 then //if  in the ground slot
  //     WrapperBltFast( lpDDSBack, pTempItems(ItemList.Items[i]).InvX, pTempItems(ItemList.Items[i]).InvY, pTempItems(ItemList.Items[i]).DXSurface, Rect(0, 0, pTempItems(ItemList.Items[i]).W, pTempItems(ItemList.Items[i]).H), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT)
  //  else //In the ground slot so plot iconic image
        WrapperBltFast( lpDDSBack, pTempItems( ItemList.Items[ i ] ).InvX, pTempItems( ItemList.Items[ i ] ).InvY, pTempItems( ItemList.Items[ i ] ).DXSurfaceIcon, Rect( 0, 0, GroundListWidth, GroundListHeight ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
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
    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;
{$ENDIF}
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TMerchant.Init

procedure TMerchant.MouseDown( Sender : TAniview; Button : TMouseButton;
  Shift : TShiftState; X, Y, GridX, GridY : integer );
var
  i, j : integer;
  //B1, B2, B3, B4, B5: Boolean;
  pTemp : Pointer;
  rRect : TRect;
  daPrice : longint;
const
  FailName : string = 'TMerchant.MouseDown';
begin
  try
    if CurrentSelectedItem = -1 then
    begin //if no piece is being dragged pick one up
      if DlgScroll.ScrollIsShowing then
      begin
        if PtInRect( rect( 119, 30, 119 + 443, 30 + 90 ), point( X, Y ) ) or PtInRect( rect( 119, 373, 119 + 443, 373 + 70 ), point( X, Y ) ) then //or PtInRect(rect(171,50,171+338,380),point(X,Y)) then
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
      else if ( X > 595 ) and ( X < 668 ) and ( Y > 418 ) and ( Y < 463 ) then
      begin //they hit the back to button
      //WriteTheInventoryData;            //write the data back
        Close; //lose the screen
      end //Buy
      else if PtinRect( rect( 436, 362, 436 + 87, 362 + 24 ), point( X, Y ) ) and ( CurrentSelectedListItem > -1 ) then
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
              WrapperBltFast( lpDDSBack, 20, 412, DXBack, Rect( 20, 412, 20 + 550, 412 + 25 ), DDBLTFAST_WAIT ); //clean up before we plot text
              pText.PlotText( txtMessage[ 3 ], 20, 412, Alpha );
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
            WrapperBltFast( lpDDSBack, 20, 412, DXBack, Rect( 20, 412, 20 + 550, 412 + 25 ), DDBLTFAST_WAIT ); //clean up before we plot text
            pText.PlotText( ( txtMessage[ 4 ] ), 20, 412, Alpha );
          end;
        end; //endif
      end //Sell
      else if PtinRect( rect( 131, 361, 131 + 82, 361 + 24 ), point( X, Y ) ) and ( CurrentSelectedListItem > -1 ) then
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
      else if ( X > 271 ) and ( X < 287 ) and ( Y > 375 ) and ( Y < 407 ) then
      begin //left arrow for ground
        if GroundOrderList.Count > 1 then
        begin //get the prev item on the ground and show it
          j := TopGroundIndex;
          if j <> 0 then
          begin //if its not the first item in the list
          //replace the back from the DXBack buffer.
            WrapperBltFast( lpDDSBack, 287, 376, DXBack, Rect( 287, 376, 363, 406 ), DDBLTFAST_WAIT );
            pTempItems( GroundOrderList.Items[ j ] ).InvX := 999;
            pTempItems( GroundOrderList.Items[ j ] ).InvY := 999;
            j := j - 1;
          //Set the coordinates of the new item and Plot it
            pTempItems( GroundOrderList.Items[ j ] ).InvX := 288;
            pTempItems( GroundOrderList.Items[ j ] ).InvY := 377;
            WrapperBltFast( lpDDSBack, pTempItems( GroundOrderList.Items[ j ] ).InvX, pTempItems( GroundOrderList.Items[ j ] ).InvY, pTempItems( GroundOrderList.Items[ j ] ).DXSurfaceIcon, Rect( 0, 0, GroundListWidth, GroundListHeight ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
            TopGroundIndex := j;
          end
          else
          begin
          //making an obnoxious buzzing noise? - we cant go backwards from the first item
          end;
        end;
      end
      else if ( X > 364 ) and ( X < 376 ) and ( Y > 375 ) and ( Y < 407 ) then
      begin //right arrow for ground
        if GroundOrderList.Count > 1 then
        begin //get the Next item on the ground and show it
          j := TopGroundIndex;
          if j < ( GroundOrderList.Count - 1 ) then
          begin //if its not the last item in the list
          //replace the back from the DXBack buffer.
            WrapperBltFast( lpDDSBack, 287, 376, DXBack, Rect( 287, 376, 363, 406 ), DDBLTFAST_WAIT );
            pTempItems( GroundOrderList.Items[ j ] ).InvX := 999;
            pTempItems( GroundOrderList.Items[ j ] ).InvY := 999;
            j := j + 1;
          //Set the coordinates of the new item and Plot it
            pTempItems( GroundOrderList.Items[ j ] ).InvX := 288;
            pTempItems( GroundOrderList.Items[ j ] ).InvY := 377;
            WrapperBltFast( lpDDSBack, pTempItems( GroundOrderList.Items[ j ] ).InvX, pTempItems( GroundOrderList.Items[ j ] ).InvY, pTempItems( GroundOrderList.Items[ j ] ).DXSurfaceIcon, Rect( 0, 0, GroundListWidth, GroundListHeight ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
            TopGroundIndex := j;
          end
          else
          begin
          //making an obnoxious buzzing noise? - we cant go forwards from the last item
          end;
        end;
      end
      else if ( x > 287 ) and ( x < 376 ) and ( y > 363 ) and ( y < 406 ) and ( CurrentSelectedItem = -1 ) then
      begin //over the ground slot
        //If we are pulling this from the ground slot, pick a new top item
        if GroundOrderList.Count > 0 then
        begin
          CurrentSelectedItem := ItemList.IndexOf( GroundOrderList.items[ TopGroundIndex ] );
          if Button = mbRight then
          begin
            DlgScroll.OpenStatsScroll( pTempItems( ItemList.Items[ CurrentSelectedItem ] ).pItem );
            CurrentSelectedItem := -1;
          end
          else
          begin
            WrapperBltFast( lpDDSBack, 287, 376, DXBack, Rect( 287, 376, 363, 406 ), DDBLTFAST_WAIT ); //clean the box
            if GroundOrderList.Count > 1 then
            begin //get the next item on the ground and show it
              j := GroundOrderList.IndexOf( ItemList.Items[ CurrentSelectedItem ] );
              if ( j = ( GroundOrderList.Count - 1 ) ) then //if its the last item in the list
                j := 0 //set it to the first one
              else //set it to the item folowing this one
                j := j + 1;
              pTempItems( GroundOrderList.Items[ j ] ).InvX := 288; //325-pTempItems(GroundOrderList.Items[j]).IW div 2;
              pTempItems( GroundOrderList.Items[ j ] ).InvY := 377; //391-pTempItems(GroundOrderList.Items[j]).IH div 2;
              WrapperBltFast( lpDDSBack, pTempItems( GroundOrderList.Items[ j ] ).InvX, pTempItems( GroundOrderList.Items[ j ] ).InvY, pTempItems( GroundOrderList.Items[ j ] ).DXSurfaceIcon, Rect( 0, 0, GroundListWidth, GroundListHeight ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
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
            Tx := ( X ) - GroundListWidth div 2; //pTempItems(ItemList.Items[CurrentSelectedItem]).W div 2;
            Ty := ( Y ) - GroundListHeight div 2; //pTempItems(ItemList.Items[CurrentSelectedItem]).H div 2;
            //Plot relevant text
            WrapperBltFast( lpDDSBack, ClearLeft, ClearTop, DXBack, Rect( ClearLeft, ClearTop, ClearRight, ClearBottom ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot test
            if UseSmallFont then
              pText.PlotTinyTextBlock( GetSlotText, ClearLeft, ClearRight, SmlMsg, Alpha )
            else
              pText.PlotText( GetSlotText, ClearLeft, LrgMsg, Alpha );
            //save the background to the dirty DD surface based on the floating item
            WrapperBltFast( DXDirty, 0, 0, lpDDSBack, Rect( Tx, Ty, Tx + GroundListWidth, Ty + GroundListHeight ), DDBLTFAST_WAIT );
            //plot the item centered under the mouse pointer
            WrapperBltFast( lpDDSBack, Tx, Ty, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).DXSurfaceIcon, Rect( 0, 0, GroundListWidth, GroundListHeight ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
            ContainCursor( 1 );
          end; //if Button = mbRight
        end //if GroundOrderList > 0
      end
      else if PtInRect( rect( 46, 38, 298, 353 ), point( x, y ) ) or PtInRect( rect( 356, 38, 607, 353 ), point( x, y ) ) then
      begin ////Select an item to sell or buy
        i := 0;
        while ( i < ItemList.Count ) and ( CurrentSelectedItem = -1 ) do
        begin
          if ( pTempItems( ItemList.Items[ i ] ).WhoHasThis = 1 ) or ( pTempItems( ItemList.Items[ i ] ).WhoHasThis = 4 ) then
          begin
            if PtInRect( pTempItems( ItemList.Items[ i ] ).cRect, point( x, y ) ) then
            begin
              CurrentSelectedItem := i; //assign it for the sake of PlotText
//                if UseSmallFont then
              pText.PlotTinyTextBlock( ( GetSlotText + txtMessage[ 5 ] ), ClearLeft, ClearRight, SmlMsg, Alpha );
//                else
//                  pText.PlotText((GetSlotText + txtMessage[5]), ClearLeft,LrgMsg,Alpha);
              if Button = mbRight then
              begin
                DlgScroll.OpenStatsScroll( pTempItems( ItemList.Items[ CurrentSelectedItem ] ).pItem );
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
            if PtInRect( pTempItems( ItemList.Items[ i ] ).cRect, point( x, y ) ) then
            begin
              CurrentSelectedItem := i; //assign it for the sake of PlotText
//                if UseSmallFont then
              pText.PlotTinyTextBlock( ( GetSlotText + txtMessage[ 5 ] ), ClearLeft, ClearRight, SmlMsg, Alpha );
//                else
//                  pText.PlotText((GetSlotText + txtMessage[5]), ClearLeft,LrgMsg,Alpha);
              if Button = mbRight then
              begin
                DlgScroll.OpenStatsScroll( pTempItems( ItemList.Items[ CurrentSelectedItem ] ).pItem );
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
          if PtinRect( rect( ScrollArrows[ i ].X, ScrollArrows[ i ].Y, ScrollArrows[ i ].X + 23, ScrollArrows[ i ].Y + 32 ), point( X, Y ) ) then
          begin
            WrapperBltFast( lpDDSBack, ScrollArrows[ i ].X, ScrollArrows[ i ].Y, ScrollArrows[ i + 4 ].DX, Rect( 0, 0, 23, 32 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
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
            WrapperBltFast( lpDDSBack, ClearLeft, ClearTop, DXBack, Rect( ClearLeft, ClearTop, ClearRight, ClearBottom ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
            if UseSmallFont then
              pText.PlotTinyTextBlock( ( txtMessage[ 6 ] ), ClearLeft, ClearRight, SmlMsg, Alpha )
            else
              pText.PlotText( ( txtMessage[ 6 ] ), ClearLeft, LrgMsg, Alpha );
          end;
          i := i + 1;
        end; //wend
        if i < 99 then
        begin //we didnt find anything on the left side arrows- check for right side
          i := 0;
          while i < 4 do
          begin
            if PtinRect( rect( ScrollArrows[ i ].X + 310, ScrollArrows[ i ].Y, ScrollArrows[ i ].X + 23 + 310, ScrollArrows[ i ].Y + 32 ), point( X, Y ) ) then
            begin
              WrapperBltFast( lpDDSBack, ScrollArrows[ i ].X + 310, ScrollArrows[ i ].Y, ScrollArrows[ i + 4 ].DX, Rect( 0, 0, 23, 32 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

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
              WrapperBltFast( lpDDSBack, ClearLeft, ClearTop, DXBack, Rect( ClearLeft, ClearTop, ClearRight, ClearBottom ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
              if UseSmallFont then
                pText.PlotTinyTextBlock( ( txtMessage[ 11 ] ), ClearLeft, ClearRight, SmlMsg, Alpha )
              else
                pText.PlotText( ( txtMessage[ 11 ] ), ClearLeft, LrgMsg, Alpha )
            end;
            i := i + 1;
          end; //wend
        end; //endif i<99
      end; //endif DlgScroll
    end
    else
    begin //drop the piece if we can
    //cleanup
      WrapperBltFast( lpDDSBack, Tx, Ty, DXDirty, Rect( 0, 0, GroundListWidth, GroundListHeight ), DDBLTFAST_WAIT );
    //try to drop on ground
      if intersectRect( rRect, rect( 287, 376, 363, 406 ), rect( Tx, Ty, Tx + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, Ty + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ) ) then
      begin
        DropOnGround( CurrentSelectedItem );
        CurrentSelectedItem := -1;
        ContainCursor( 0 );
      end
      else if PtInRect( rect( 46, 38, 298, 353 ), point( x, y ) ) then
      begin //try to drop it in player inventory
        i := CurrentSelectedItem; //We have to do this- CurrentSelectedItem gets re-initialized in Collisiondetect
        if ItemFitsInInventory( i ) = false then
        begin //not enough room in inventory
          DropOnGround( i );
          CurrentSelectedItem := -1;
           //paint;
          WrapperBltFast( lpDDSBack, 20, 412, DXBack, Rect( 20, 412, 20 + 550, 412 + 25 ), DDBLTFAST_WAIT ); //clean up before we plot text
          pText.PlotText( txtMessage[ 3 ], 20, 412, Alpha );
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

    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TMerchant.MouseDown

procedure TMerchant.MouseMove( Sender : TAniview; Shift : TShiftState; X,
  Y, GridX, GridY : integer );
var
  Tw, Th : Integer;
  i : Integer;
const
  FailName : string = 'TMerchant.MouseDown';
begin
  try
                                  //This assigned(DXBack) is here to keep the program from crashing while Im developing it
    if ( CurrentSelectedItem > -1 ) and Assigned( DXBack ) then
    begin //are we dragging an item?
    //clean up
      WrapperBltFast( lpDDSBack, Tx, Ty, DXDirty, Rect( 0, 0, GroundListWidth, GroundListHeight ), DDBLTFAST_WAIT );
    //Compute the coords for the floating item
      Tx := ( X ) - GroundListWidth div 2;
      Ty := ( Y ) - GroundListHeight div 2;
      if Tx < 0 then
        Tx := 0;
      if Ty < 0 then
        Ty := 0;
      Tw := GroundListWidth;
      if ( Tx + Tw ) > 659 then
        Tx := 659 - Tw;

      Th := GroundListHeight;
      if ( Ty + Th ) > 463 then
        Ty := 463 - Th;

    //save the background to the dirty DD surface based on the floating item
      WrapperBltFast( DXDirty, 0, 0, lpDDSBack, Rect( Tx, Ty, Tx + Tw, Ty + Th ), DDBLTFAST_WAIT );
    //plot the item centered under the mouse pointer
      WrapperBltFast( lpDDSBack, Tx, Ty, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).DXSurfaceIcon, Rect( 0, 0, GroundListWidth, GroundListHeight ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      lpDDSFront.Flip( nil, DDFLIP_WAIT );
      WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
      MouseCursor.PlotDirty := false;
    end
    else if Assigned( DXBack ) and ( DlgScroll.ScrollIsShowing = False ) then
    begin //do the rollover
      i := 0; //find the item the mouse is down over
      WrapperBltFast( lpDDSBack, ClearLeft, ClearTop, DXBack, Rect( ClearLeft, ClearTop, ClearRight, ClearBottom ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
      while ( i < ItemList.Count ) and ( CurrentSelectedItem = -1 ) do
      begin
        if ( pTempItems( ItemList.Items[ i ] ).WhoHasThis = 1 ) or ( pTempItems( ItemList.Items[ i ] ).WhoHasThis = 4 ) then
        begin
          if PtInRect( pTempItems( ItemList.Items[ i ] ).cRect, point( x, y ) ) then
          begin
            CurrentSelectedItem := i; //assign it for the sake of PlotText
//            if UseSmallFont then
            pText.PlotTinyTextBlock( ( GetSlotText + txtMessage[ 5 ] ), ClearLeft, ClearRight, SmlMsg, Alpha );
//            else
//              pText.PlotText((GetSlotText + txtMessage[5]), ClearLeft,LrgMsg,Alpha);
          end;
        end
        else if pTempItems( ItemList.Items[ i ] ).WhoHasThis = 2 then
        begin //merchant list
          if PtInRect( pTempItems( ItemList.Items[ i ] ).cRect, point( x, y ) ) then
          begin
            CurrentSelectedItem := i; //assign it for the sake of PlotText
//            if UseSmallFont then
            pText.PlotTinyTextBlock( ( GetSlotText + txtMessage[ 5 ] ), ClearLeft, ClearRight, SmlMsg, Alpha );
//            else
//              pText.PlotText((GetSlotText + txtMessage[5]), ClearLeft,LrgMsg,Alpha);
          end;
        end; //endif
        i := i + 1;
      end; //wend
    //If we arent over an item see if we're over the ground slot
      if ( x > 287 ) and ( x < 363 ) and ( y > 363 ) and ( y < 406 ) and ( CurrentSelectedItem = -1 ) then
      begin //over the ground slot
        if GroundOrderList.Count > 0 then
        begin
          CurrentSelectedItem := ItemList.IndexOf( GroundOrderList.items[ TopGroundIndex ] );
//          if UseSmallFont then
          pText.PlotTinyTextBlock( ( GetSlotText + txtMessage[ 5 ] ), ClearLeft, ClearRight, SmlMsg, Alpha );
//          else
//            pText.PlotText((GetSlotText + txtMessage[5]), ClearLeft,LrgMsg,Alpha);
        end;
      end; //endif
    //Clean up arrows and back to game
    //WrapperBltFast( lpDDSBack, 300, 194, DXBack, Rect(300, 194, 348, 250), DDBLTFAST_WAIT);
    //Plot the arrows cleanup
      for i := 0 to 3 do
      begin
        WrapperBltFast( lpDDSBack, ScrollArrows[ i ].X, ScrollArrows[ i ].Y, DXBack, rect( ScrollArrows[ i ].X, ScrollArrows[ i ].Y, ScrollArrows[ i ].X + 23, ScrollArrows[ i ].Y + 32 ), DDBLTFAST_WAIT );
        WrapperBltFast( lpDDSBack, ScrollArrows[ i ].X + 310, ScrollArrows[ i ].Y, DXBack, rect( ScrollArrows[ i ].X + 310, ScrollArrows[ i ].Y, ScrollArrows[ i ].X + 23 + 310, ScrollArrows[ i ].Y + 32 ), DDBLTFAST_WAIT );
      end; //end for
    //ground arrows back to game cleanup
      WrapperBltFast( lpDDSBack, 271, 385, DXBack, Rect( 271, 385, 271 + 15, 385 + 20 ), DDBLTFAST_WAIT );
      WrapperBltFast( lpDDSBack, 364, 385, DXBack, Rect( 364, 385, 364 + 12, 385 + 20 ), DDBLTFAST_WAIT );
      WrapperBltFast( lpDDSBack, 588, 407, DXBack, Rect( 588, 407, 588 + 77, 407 + 54 ), DDBLTFAST_WAIT );
    //clean up buy and sell
      WrapperBltFast( lpDDSBack, 385, 362, DXBack, Rect( 385, 362, 385 + 192, 362 + 24 ), DDBLTFAST_WAIT );
      WrapperBltFast( lpDDSBack, 76, 362, DXBack, Rect( 76, 362, 76 + 192, 362 + 24 ), DDBLTFAST_WAIT );
    //now replot the text
      pText.PlotTextCentered( txtMessage[ 1 ], 46, 298, 362, BuySellAlpha );
      pText.PlotTextCentered( txtMessage[ 2 ], 355, 607, 362, BuySellAlpha );
    //Clean up secondary message line
      if not ( PtInRect( Rect( 436, 362, 436 + 87, 362 + 24 ), point( x, y ) ) or PtInRect( rect( 46, 38, 298, 353 ), point( x, y ) ) ) then
        WrapperBltFast( lpDDSBack, 20, 412, DXBack, Rect( 20, 412, 20 + 550, 412 + 23 ), DDBLTFAST_WAIT );

      if CurrentSelectedItem = -1 then
      begin //If we arent over an item then check arrows and back button
        if PtinRect( rect( 271, 375, 287, 407 ), point( X, Y ) ) then
        begin //over left arrow
          //plot highlighted arrow
          WrapperBltFast( lpDDSBack, 272, 385, DXLeftArrow, Rect( 0, 0, 14, 15 ), DDBLTFAST_WAIT );
          //plot a bit of informative text
          WrapperBltFast( lpDDSBack, ClearLeft, ClearTop, DXBack, Rect( ClearLeft, ClearTop, ClearRight, ClearBottom ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
          if UseSmallFont then
            pText.PlotTinyTextBlock( ( txtMessage[ 7 ] ), ClearLeft, ClearRight, SmlMsg, Alpha )
          else
            pText.PlotText( ( txtMessage[ 7 ] ), ClearLeft, LrgMsg, Alpha );
        end
        else if PtinRect( rect( 364, 375, 376, 407 ), point( X, Y ) ) then
        begin //over right arrow
          //plot highlighted arrow
          WrapperBltFast( lpDDSBack, 365, 387, DXRightArrow, Rect( 0, 0, 11, 11 ), DDBLTFAST_WAIT );
          //plot a bit of informative text
          WrapperBltFast( lpDDSBack, ClearLeft, ClearTop, DXBack, Rect( ClearLeft, ClearTop, ClearRight, ClearBottom ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
          if UseSmallFont then
            pText.PlotTinyTextBlock( ( txtMessage[ 8 ] ), ClearLeft, ClearRight, SmlMsg, Alpha )
          else
            pText.PlotText( ( txtMessage[ 8 ] ), ClearLeft, LrgMsg, Alpha );
        end
        else if PtinRect( rect( 588, 407, 588 + 77, 412 + 54 ), point( X, Y ) ) then
        begin //over back button
          //plot highlighted back to game
          WrapperBltFast( lpDDSBack, 588, 407, DXBackToGame, Rect( 0, 0, 77, 54 ), DDBLTFAST_WAIT );
          //don't plot a bit of informative text, just clean up
          WrapperBltFast( lpDDSBack, ClearLeft, ClearTop, DXBack, Rect( ClearLeft, ClearTop, ClearRight, ClearBottom ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
        end
        else if PtinRect( rect( 436, 362, 436 + 87, 362 + 24 ), point( X, Y ) ) then
        begin //over buy item
          //plot highlighted BuyItem
          WrapperBltFast( lpDDSBack, 385, 362, DXBuyItem, Rect( 0, 0, 192, 24 ), DDBLTFAST_WAIT );
          //plot a bit of informative text, then clean up
          WrapperBltFast( lpDDSBack, ClearLeft, ClearTop, DXBack, Rect( ClearLeft, ClearTop, ClearRight, ClearBottom ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
          if UseSmallFont then
            pText.PlotTinyTextBlock( ( txtMessage[ 9 ] ), ClearLeft, ClearRight, SmlMsg, Alpha )
          else
            pText.PlotText( txtMessage[ 9 ], ClearLeft, LrgMsg, Alpha );
        end
        else if PtinRect( rect( 131, 361, 131 + 82, 361 + 24 ), point( X, Y ) ) then
        begin //over sell item
          //plot highlighted Sell Item
          WrapperBltFast( lpDDSBack, 76, 362, DXSellItem, Rect( 0, 0, 192, 24 ), DDBLTFAST_WAIT );
          //plot a bit of informative text, then clean up
          WrapperBltFast( lpDDSBack, ClearLeft, ClearTop, DXBack, Rect( ClearLeft, ClearTop, ClearRight, ClearBottom ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
          if UseSmallFont then
            pText.PlotTinyTextBlock( ( txtMessage[ 10 ] ), ClearLeft, ClearRight, SmlMsg, Alpha )
          else
            pText.PlotText( txtMessage[ 10 ], ClearLeft, LrgMsg, Alpha );
        end
        else
        begin //if PtinRect(rect(300,225,347,247),point(X,Y)) then begin //over left ALL arrow
          //plot highlighted arrow
          i := 0;
          while i < 4 do
          begin
            if PtinRect( rect( ScrollArrows[ i ].X, ScrollArrows[ i ].Y, ScrollArrows[ i ].X + 23, ScrollArrows[ i ].Y + 32 ), point( X, Y ) ) then
            begin
              WrapperBltFast( lpDDSBack, ScrollArrows[ i ].X, ScrollArrows[ i ].Y, ScrollArrows[ i + 4 ].DX, Rect( 0, 0, 23, 32 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
              i := 99;
                 //plot a bit of informative text
              WrapperBltFast( lpDDSBack, ClearLeft, ClearTop, DXBack, Rect( ClearLeft, ClearTop, ClearRight, ClearBottom ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
              if UseSmallFont then
                pText.PlotTinyTextBlock( ( txtMessage[ 6 ] ), ClearLeft, ClearRight, SmlMsg, Alpha )
              else
                pText.PlotText( ( txtMessage[ 6 ] ), ClearLeft, LrgMsg, Alpha )
            end;
            i := i + 1;
          end; //wend
          if i < 99 then
          begin //we didnt find anything on the left side arrows- check for right side
            i := 0;
            while i < 4 do
            begin
              if PtinRect( rect( ScrollArrows[ i ].X + 310, ScrollArrows[ i ].Y, ScrollArrows[ i ].X + 23 + 310, ScrollArrows[ i ].Y + 32 ), point( X, Y ) ) then
              begin
                WrapperBltFast( lpDDSBack, ScrollArrows[ i ].X + 310, ScrollArrows[ i ].Y, ScrollArrows[ i + 4 ].DX, Rect( 0, 0, 23, 32 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
                i := 99;
                     //plot a bit of informative text
                WrapperBltFast( lpDDSBack, ClearLeft, ClearTop, DXBack, Rect( ClearLeft, ClearTop, ClearRight, ClearBottom ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
                if UseSmallFont then
                  pText.PlotTinyTextBlock( ( txtMessage[ 11 ] ), ClearLeft, ClearRight, SmlMsg, Alpha )
                else
                  pText.PlotText( ( txtMessage[ 11 ] ), ClearLeft, LrgMsg, Alpha )
              end;
              i := i + 1;
            end; //wend
          end; //endif i<99
        end
      end; //endif CurrentSelectedItem

      CurrentSelectedItem := -1; //deassign it
      lpDDSFront.Flip( nil, DDFLIP_WAIT );
      WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
      MouseCursor.PlotDirty := false;
    end;

  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TMerchant.MouseMove

procedure TMerchant.MouseUp( Sender : TAniview; Button : TMouseButton;
  Shift : TShiftState; X, Y, GridX, GridY : integer );
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
const
  FailName : string = 'TMerchant.Paint';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    WrapperBltFast( lpDDSBack, 0, 0, DXBack, Rect( 0, 0, 679, 476 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
  //Now plot all of the items on the grid(s), and ground slots
    for i := 0 to ItemList.Count - 1 do
    begin
      if pTempItems( ItemList.Items[ i ] ).WhoHasThis = 3 then //if in ground slot plot icon
        WrapperBltFast( lpDDSBack, pTempItems( ItemList.Items[ i ] ).InvX, pTempItems( ItemList.Items[ i ] ).InvY, pTempItems( ItemList.Items[ i ] ).DXSurfaceIcon, Rect( 0, 0, GroundListWidth, GroundListHeight ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT )
    // else
    //    WrapperBltFast( lpDDSBack, pTempItems(ItemList.Items[i]).InvX, pTempItems(ItemList.Items[i]).InvY, pTempItems(ItemList.Items[i]).DXSurface, Rect(0, 0, pTempItems(ItemList.Items[i]).W, pTempItems(ItemList.Items[i]).H), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT)
    end;
    ShowLeftList;
    ShowRightList;
  //Now put the names up
    pText.PlotText( Character.name, 43, 10, Alpha );
    pText.PlotText( Merchant.name, 355, 10, Alpha );
    pText.PlotText( IntToStr( Character.money ) + txtMessage[ 0 ], 297 - pText.TextLength( IntToStr( Character.money ) + txtMessage[ 0 ] ), 10, Alpha );
  //now the Buy and Sell buttons
    pText.PlotTextCentered( txtMessage[ 1 ], 46, 298, 362, BuySellAlpha );
    pText.PlotTextCentered( txtMessage[ 2 ], 355, 607, 362, BuySellAlpha );

    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;
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
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
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
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
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
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
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
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
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
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
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
  prRect : PRect;
const
  FailName : string = 'TMerchant.ContainCursor';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    new( prRect );
    prRect.top := 0;
    prRect.left := 0;
    if Action = 1 then
    begin //restore to fullscreen
      prRect.bottom := 478;
      prRect.Right := 640;
    end
    else
    begin //constrict to main inventory area
      prRect.bottom := 600;
      prRect.Right := 800;
    end;
    ClipCursor( prRect );
    Dispose( prRect );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TMerchant.ContainCursor


procedure TMerchant.BuildGrid;
var
  i, j : integer;
  //StartX,StartY: integer;
  DXGrid : IDirectDrawSurface; //DD surface holding our chunk O' grid to draw right grid with
  BM : TBitmap;
const
  FailName : string = 'TMerchant.BuildGrid';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
  //Load the grid graphic, and draw the left inventory area before we blit the screen to the backbuffer
    BM := TBitmap.create;
    BM.LoadFromFile( InterfacePath + 'merGrid.bmp' );
    DXGrid := DDGetImage( lpDD, BM, $00FFFF00, False );
    BM.free;

  //if OtherOb is TCharacter then begin
    for j := 0 to 8 do
    begin
      for i := 0 to 6 do
      begin
           //WrapperBltFast( DXBack, (607-36)-i*36,(352-35)-j*35,DXGrid,rect(0,0,38,37),DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
        DrawAlpha( DXBack, rect( ( 607 - 36 ) - i * 36, ( 352 - 35 ) - j * 35, ( 607 - 36 ) - i * 36 + 38, ( 352 - 35 ) - j * 35 + 37 ), rect( 0, 0, 38, 37 ), DXGrid, True, 160 );
           //WrapperBltFast( DXBack, (297-36)-i*36,(352-35)-j*35,DXGrid,rect(0,0,38,37),DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
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
    WrapperBltFast( DXBack, 269, 359, DXGroundBox, rect( 0, 0, 110, 54 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
  //Plot the arrows
    for i := 0 to 3 do
    begin
      WrapperBltFast( DXBack, ScrollArrows[ i ].X, ScrollArrows[ i ].Y, ScrollArrows[ i ].DX, rect( 0, 0, 23, 32 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      WrapperBltFast( DXBack, ScrollArrows[ i ].X + 310, ScrollArrows[ i ].Y, ScrollArrows[ i ].DX, rect( 0, 0, 23, 32 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
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
const
  FailName : string = 'TMerchant.ShowLeftList';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    j := 0;
    k := 1;
    WrapperBltFast( lpDDSBack, 46, 38, DXBack, Rect( 46, 38, 298, 352 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
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
            DrawAlpha( lpDDSBack, pTempItems( ItemList.Items[ CurrentSelectedListItem ] ).cRect, rect( 0, 0, 25, 25 ), DXBackHighlight, False, 75 );
          WrapperBltFast( lpDDSBack, 46, j * 35 + 38 + 3, pTempItems( ItemList.Items[ i ] ).DXSurfaceIcon, Rect( 0, 0, GroundListWidth, GroundListHeight ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
           //plot the name
           //pText.PlotTinyText(pTempItems(ItemList.Items[i]).pItem.name,126,j*35+38-3,250);
          pText.PlotTinyText( GetSHORTSlotText( pTempItems( ItemList.Items[ i ] ).pItem.name ), 126, j * 35 + 38 - 3, 250 );
           //If player is wearing this item, say so
          if pTempItems( ItemList.Items[ i ] ).BodySlot > -1 then
          begin //if hes wearing it, say so
            if pTempItems( ItemList.Items[ i ] ).BodySlot = 11 then
            begin //its a weapon
              pText.PlotTinyText( txtMessage[ 12 ], 126, j * 35 + 38 - 3 + 18, 250 );
            end
            else
            begin //an item, or armour
              pText.PlotTinyText( txtMessage[ 13 ], 126, j * 35 + 38 - 3 + 18, 250 )
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
          pText.PlotTinyText( IntToStr( Cost ) + txtMessage[ 0 ], 297 - pText.TinyTextLength( IntToStr( Cost ) + txtMessage[ 0 ] ), j * 35 + 38 - 3 + 18, 250 );
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
const
  FailName : string = 'TMerchant.ShowRightList';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    UniqueItemList := TStringList.create; //create this string list to make sure we only show one of each kind of item
    UniqueItemList.sorted := true;
    UniqueItemList.duplicates := dupError;
    j := 0;
    k := 1;
    WrapperBltFast( lpDDSBack, 356, 38, DXBack, Rect( 356, 38, 608, 352 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
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
            DrawAlpha( lpDDSBack, pTempItems( ItemList.Items[ CurrentSelectedListItem ] ).cRect, rect( 0, 0, 25, 25 ), DXBackHighlight, False, 75 );
          WrapperBltFast( lpDDSBack, 356, j * 35 + 38 + 3, pTempItems( ItemList.Items[ i ] ).DXSurfaceIcon, Rect( 0, 0, GroundListWidth, GroundListHeight ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
           //pText.PlotTinyText(pTempItems(ItemList.Items[i]).pItem.name,80+356,j*35+38-3,250);
          pText.PlotTinyText( GetSHORTSlotText( pTempItems( ItemList.Items[ i ] ).pItem.name ), 80 + 356, j * 35 + 38 - 3, 250 );
           //Plot the sale value
          if Character.charm < 1 then
            Cost := Round( pTempItems( ItemList.Items[ i ] ).pItem.Value * ( 1 + 10 * ( Merchant.SellingMarkup - 1 ) / 1 ) )
          else
            Cost := Round( pTempItems( ItemList.Items[ i ] ).pItem.Value * ( 1 + 10 * ( Merchant.SellingMarkup - 1 ) / Character.Charm ) );
//           Cost:= Round(pTempItems(ItemList.Items[i]).pItem.Value*Merchant.SellingMarkup);
          pText.PlotTinyText( IntToStr( Cost ) + txtMessage[ 0 ], 607 - pText.TinyTextLength( IntToStr( Cost ) + txtMessage[ 0 ] ), j * 35 + 38 - 3 + 18, 250 );
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
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
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
    WrapperBltFast( lpDDSBack, 287, 376, DXBack, Rect( 287, 376, 363, 406 ), DDBLTFAST_WAIT ); //clean out the Ground box
    pTempItems( ItemList.Items[ ItemIndex ] ).InvX := 288;
    pTempItems( ItemList.Items[ ItemIndex ] ).InvY := 377;
    pTempItems( ItemList.Items[ ItemIndex ] ).WhoHasThis := 3; //ground
    WrapperBltFast( lpDDSBack, pTempItems( ItemList.Items[ ItemIndex ] ).InvX, pTempItems( ItemList.Items[ ItemIndex ] ).InvY,
      pTempItems( ItemList.Items[ ItemIndex ] ).DXSurfaceIcon, Rect( 0, 0, GroundListWidth, GroundListHeight ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    WrapperBltFast( lpDDSBack, ClearLeft, ClearTop, DXBack, Rect( ClearLeft, ClearTop, ClearRight, ClearBottom ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clear text
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
begin
  if ScrollStateLeft <> 0 then
  begin
    GetCursorPos( P );
    i := 0;
    while i < 4 do
    begin
      if PtinRect( rect( ScrollArrows[ i ].X, ScrollArrows[ i ].Y, ScrollArrows[ i ].X + 23, ScrollArrows[ i ].Y + 32 ), P ) then
      begin
        WrapperBltFast( lpDDSBack, ScrollArrows[ i ].X, ScrollArrows[ i ].Y, ScrollArrows[ i + 4 ].DX, Rect( 0, 0, 23, 32 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
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
        WrapperBltFast( lpDDSBack, ClearLeft, ClearTop, DXBack, Rect( ClearLeft, ClearTop, ClearRight, ClearBottom ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
        if UseSmallFont then
          pText.PlotTinyTextBlock( ( txtMessage[ 6 ] ), ClearLeft, ClearRight, SmlMsg, Alpha )
        else
          pText.PlotText( ( txtMessage[ 6 ] ), ClearLeft, LrgMsg, Alpha );
        lpDDSFront.Flip( nil, DDFLIP_WAIT );
        WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
        MouseCursor.PlotDirty := false;
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
    GetCursorPos( P );
    i := 0;
    while i < 4 do
    begin
      if PtinRect( rect( ScrollArrows[ i ].X + 310, ScrollArrows[ i ].Y, ScrollArrows[ i ].X + 23 + 310, ScrollArrows[ i ].Y + 32 ), P ) then
      begin
        WrapperBltFast( lpDDSBack, ScrollArrows[ i ].X + 310, ScrollArrows[ i ].Y, ScrollArrows[ i + 4 ].DX, Rect( 0, 0, 23, 32 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

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
        WrapperBltFast( lpDDSBack, ClearLeft, ClearTop, DXBack, Rect( ClearLeft, ClearTop, ClearRight, ClearBottom ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
        if UseSmallFont then
          pText.PlotTinyTextBlock( ( txtMessage[ 11 ] ), ClearLeft, ClearRight, SmlMsg, Alpha )
        else
          pText.PlotText( ( txtMessage[ 11 ] ), ClearLeft, LrgMsg, Alpha );

        lpDDSFront.Flip( nil, DDFLIP_WAIT );
        WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
        MouseCursor.PlotDirty := false;
        break;
      end;
      i := i + 1;
    end; //wend
    if i = 4 then
      ScrollStateRight := 0;
  end;
end;

end.

