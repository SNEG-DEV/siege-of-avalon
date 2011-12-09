unit Inventory;
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
  Resource,
  GameText,
  Display,
  Statistics,
  Scroll,
  Anigrp30,
  Engine,
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
  end;

  TInventory = class( TDisplay )
  private
    BMBack : TBitmap; //The inventory screen bitmap used for loading
    ItemList : TList; //the list of items
    pInventoryItem : pTempItems; //The temporary inventory and equipment items combined
    CurrentSelectedItem : Integer; //Current Item being dragged about
    Tx, Ty : Integer; // x and y locs used with the offset of the dragged item
    DontPlotCurrentItem : boolean;
{$IFDEF DirectX}
    DXBack : IDirectDrawSurface; //DD surface that holds the inventory screen before blit
    DxDirty : IDirectDrawSurface; //DD for cleanup when dragging items
    DXCircle : IDirectDrawSurface; //circle used for outline
    DXLeftArrow : IDirectDrawSurface; //Inventory left arrow
    DXRightArrow : IDirectDrawSurface; //Inventory right arrow
    DXBackToGame : IDirectDrawSurface; //Back To Game highlight
    DXBrown : IDirectDrawSurface; //Show where we can drop item in inventory
{$ENDIF}
    SlotName : TStringList; //names of each slot we plot on screen
    SlotCoord : array[ 0..15 ] of BodySlotCoord; //coordinates used to generate rects
    //GroundList: TList;
    GroundOrderList : TList; //used to keep track of the order of items on the ground
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
    procedure ContainCursor( Action : integer ); //lock curson to screen during item drag
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
    procedure MouseDown( Sender : TAniview; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
    procedure MouseMove( Sender : TAniview;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
    procedure MouseUp( Sender : TAniview; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
  public
    Character : Tcharacter;
    GroundList : TList;
    Locked : bool;
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
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
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

    SlotName.Add( 'Misc1' );
    SlotCoord[ 13 ].cx := 343;
    SlotCoord[ 13 ].cy := 103;
    SlotCoord[ 13 ].cr := 21;

    SlotName.Add( 'Misc2' );
    SlotCoord[ 14 ].cx := 321;
    SlotCoord[ 14 ].cy := 138;
    SlotCoord[ 14 ].cr := 21;

    SlotName.Add( 'Misc3' );
    SlotCoord[ 15 ].cx := 363;
    SlotCoord[ 15 ].cy := 138;
    SlotCoord[ 15 ].cr := 21;

    for i := 0 to 15 do
    begin //set up the collision rects
      SlotCoord[ i ].Rect.Left := SlotCoord[ i ].cx - SlotCoord[ i ].cr;
      SlotCoord[ i ].Rect.Right := SlotCoord[ i ].cx + SlotCoord[ i ].cr;
      SlotCoord[ i ].Rect.Top := SlotCoord[ i ].cy - SlotCoord[ i ].cr;
      SlotCoord[ i ].Rect.Bottom := SlotCoord[ i ].cy + SlotCoord[ i ].cr;
      SlotCoord[ i ].UsedBy := -1;
    end;
  //Create the GroundList
    GroundList := TList.Create;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

destructor TInventory.Destroy;
const
  FailName : string = 'TInventory.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
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
  InvisColor : Integer; //Transparent color :RGB(0,255,255)
  i : Integer;
  t : TSlot; //Index for Equipment loop
  DXBorder : IDirectDrawSurface;
  WeHaveAWeaponAndThisIsTheIndex : integer; //for moving the weapon to a clear spot on barbie
  GreatestWidth, GreatestHeight : integer; //used to create the dirty rect surface
const
  FailName : string = 'TInventory.init';
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
    BMBack := TBitmap.Create;
  //transparent color
    InvisColor := $00FFFF00;
  //Create list
    ItemList := TList.Create; //create the ItemList
  //ItemList.Sorted := True;              //Indicate that it is always to be sorted
  //ItemList.Duplicates := dupAccept;     //Allow duplicates
    GroundOrderList := TList.Create;
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
      pInventoryItem.PItem := GroundList.Items[ i ];
      if i = 0 then
      begin
        pInventoryItem.InvX := 277; //315-pInventoryItem.pItem.width div 2;       //Only the first ground item is visible
        pInventoryItem.InvY := 386; //401-pInventoryItem.pItem.height div 2;
      end
      else
      begin
        pInventoryItem.InvX := 999; //set it offscreen so we dont see it
        pInventoryItem.InvY := 999;
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
      if pTempItems( ItemList.items[ i ] ).BodySlot > -1 then
        SlotCoord[ pTempItems( ItemList.Items[ i ] ).BodySlot ].UsedBy := i;
    end;
  //Get the barbie pics for all the items, as well as the GroundIcons and Shadows
    GreatestWidth := GroundListWidth; //we inot to ground list size - must be at least this big
    GreatestHeight := GroundListHeight;
    for i := 0 to ItemList.Count - 1 do
    begin
      pTempItems( ItemList.Items[ i ] ).DXSurface := pTempItems( ItemList.Items[ i ] ).pItem.GetInventoryImage;
      pTempItems( ItemList.Items[ i ] ).DXShadow := pTempItems( ItemList.Items[ i ] ).pItem.GetInventoryShadow;
      pTempItems( ItemList.Items[ i ] ).DXSurfaceIcon := pTempItems( ItemList.Items[ i ] ).pItem.GetIconicImage;
      //pTempItems(ItemList.Items[i]).IW := pTempItems(ItemList.Items[i]).pItem.width; //icon width
      //pTempItems(ItemList.Items[i]).IH := pTempItems(ItemList.Items[i]).pItem.height;//icon height
      pTempItems( ItemList.Items[ i ] ).W := pTempItems( ItemList.Items[ i ] ).pItem.InvW * 18;
      pTempItems( ItemList.Items[ i ] ).H := pTempItems( ItemList.Items[ i ] ).pItem.InvH * 26;
      //get the Greatest barbie pic Wodth and Height so we know what size to make the dirty surface
      if pTempItems( ItemList.Items[ i ] ).W > GreatestWidth then
        GreatestWidth := pTempItems( ItemList.Items[ i ] ).W;
      if pTempItems( ItemList.Items[ i ] ).H > GreatestHeight then
        GreatestHeight := pTempItems( ItemList.Items[ i ] ).H;
      //New 9-07
      if not assigned( pTempItems( ItemList.Items[ i ] ).DXSurface ) then
      begin
        pTempItems( ItemList.Items[ i ] ).InvX := -100;
        pTempItems( ItemList.Items[ i ] ).InvY := -100;
        pTempItems( ItemList.Items[ i ] ).W := 10;
        pTempItems( ItemList.Items[ i ] ).H := 10;
      end;
      //end new
    end;
  //Create the DirectRect fix surface
    DXDirty := DDGetSurface( lpDD, GreatestWidth, GreatestHeight, InvisColor, true );

    if WeHaveAWeaponAndThisIsTheIndex > -1 then
    begin //If wielding a weapon, put it in a spot where it doesnt collide
      Tx := pTempItems( ItemList.Items[ WeHaveAWeaponAndThisIsTheIndex ] ).InvX;
      Ty := pTempItems( ItemList.Items[ WeHaveAWeaponAndThisIsTheIndex ] ).InvY;
      WeaponPlacement( WeHaveAWeaponAndThisIsTheIndex );
      pTempItems( ItemList.Items[ WeHaveAWeaponAndThisIsTheIndex ] ).InvX := Tx;
      pTempItems( ItemList.Items[ WeHaveAWeaponAndThisIsTheIndex ] ).InvY := Ty;
    end; //endif

{ //all gone and no longer used - we just load the image over and over
  //We now have a list of character inventory items sorted by filename
  //Create the DirectDraw surfaces - don't load the same graphic more than once, just reuse the surface
   //The first item automatically gets a DXSurface - this is an init of sorts
  if ItemList.Count > 0 then begin      //if we have any items at all
    BMBack.LoadFromFile(InterfacePath +'items\' + ItemList.Strings[0]);
    pTempItems(ItemList.Items[0]).W := BMBack.width; //set the width and height here
    pTempItems(ItemList.Items[0]).H := BMBack.Height;

    pTempItems(ItemList.Items[0]).DXSurface := DDGetImage(lpDD, BMBack, InvisColor, False);

  end;
  for i := 1 to ItemList.Count - 1 do begin
    if ItemList.Strings[i] = ItemList.Strings[i - 1] then begin //if this item has the same barbie graphic as the last item, copy its DXSurface pointer
      pTempItems(ItemList.Items[i]).DXSurface := pTempItems(ItemList.Items[i - 1]).DXSurface;
      pTempItems(ItemList.Items[i]).W := pTempItems(ItemList.Items[i - 1]).W;
      pTempItems(ItemList.Items[i]).H := pTempItems(ItemList.Items[i - 1]).H;
    end
    else begin                          //if this item has a different filename, then create a new dxsurface
      BMBack.LoadFromFile(InterfacePath +'items\' + ItemList.Strings[i]);
      pTempItems(ItemList.Items[i]).W := BMBack.width; //set the width and height here
      pTempItems(ItemList.Items[i]).H := BMBack.Height;

      pTempItems(ItemList.Items[i]).DXSurface := DDGetImage(lpDD, BMBack, InvisColor, False);

    end;
  end;  }

  //Load the Background Bitmap and plot it
{$IFNDEF DirectX}
    BMBack.LoadFromFile( InterfacePath + 'inventory.bmp' );
    BitBlt( Game.Canvas.Handle, 0, 0, BMBack.width, BMBack.Height, BMBack.Canvas.Handle, 0, 0, SRCCOPY );
    Game.Refresh;
{$ENDIF}
{$IFDEF DirectX}
    BMBack.LoadFromFile( InterfacePath + 'invRightArrow.bmp' );
    DXRightArrow := DDGetImage( lpDD, BMBack, InvisColor, False );
    BMBack.LoadFromFile( InterfacePath + 'invLeftArrow.bmp' );
    DXLeftArrow := DDGetImage( lpDD, BMBack, InvisColor, False );
    BMBack.LoadFromFile( InterfacePath + 'invBackToGame.bmp' );
    DXBackToGame := DDGetImage( lpDD, BMBack, InvisColor, False );

    BMBack.LoadFromFile( InterfacePath + 'invRedCircle.bmp' );
    DXCircle := DDGetImage( lpDD, BMBack, InvisColor, False );
    BMBack.LoadFromFile( InterfacePath + 'merBackHighlight.bmp' );
    DXBrown := DDGetImage( lpDD, BMBack, InvisColor, False );

  //BMBack.LoadFromFile(InterfacePath +'creatures.bmp');
  //DXBack:=DDGetImage(lpDD,BMBack,InvisColor,false);
  //lpDDSBack.BltFast(0,0,DXBack,Rect(0,0,BMBack.width,BMBack.Height),DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
    BMBack.LoadFromFile( InterfacePath + 'inventory.bmp' );
    DXBack := DDGetImage( lpDD, BMBack, InvisColor, False );
  //DxDirty := DDGetImage(lpDD, BMBack, InvisColor, False); //for now this is how we will do it

    lpDDSBack.BltFast( 0, 0, DXBack, Rect( 0, 0, BMBack.width, BMBack.Height ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
  //Now for the Alpha'ed edges
    BMBack.LoadFromFile( InterfacePath + 'invRightInventoryAlpha.bmp' );
    DXBorder := DDGetImage( lpDD, BMBack, InvisColor, False );
    DrawSub( lpDDSBack, Rect( 659, 0, 659 + BMBack.Width, BMBack.Height ), Rect( 0, 0, BMBack.Width, BMBack.Height ), DXBorder, True, Alpha );

    DXBorder := nil;

    BMBack.LoadFromFile( InterfacePath + 'invBottomInventoryAlpha.bmp' );
    DXBorder := DDGetImage( lpDD, BMBack, InvisColor, False );
    DrawSub( lpDDSBack, Rect( 0, 460, BMBack.Width, 460 + BMBack.Height ), Rect( 0, 0, BMBack.Width, BMBack.Height ), DXBorder, True, Alpha );

    DXBorder := nil; //release DXBorder
{$ENDIF}

{$IFDEF DirectX}
  //release the bitmap
    BMBack.Free;
  //pText.Load13Graphic;
 // pText.PlotF13Text(lpDDSBack,'This is a test of Font 13!!',20,300,240);
 // pText.PlotF13Block(lpDDSBack,'This is a test of Font 13.  Hopefully, this sorry excuse for a font function can In Fact, Plot A Large Block O text Without Puking.',120,300,400,240);
 // pText.Unload13Graphic;
  //Now plot all of the items on the grid
{  for i := 0 to ItemList.Count - 1 do begin
    //lpDDSBack.BltFast(pTempItems(ItemList.Items[i]).InvX, pTempItems(ItemList.Items[i]).InvY, pTempItems(ItemList.Items[i]).DXSurface, Rect(0, 0, pTempItems(ItemList.Items[i]).W, pTempItems(ItemList.Items[i]).H), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
    if pTempItems(ItemList.Items[i]).ItemType <> 'Ground' then begin//if not in the ground slot
       DrawSub(lpDDSBack,rect(pTempItems(ItemList.Items[i]).InvX, pTempItems(ItemList.Items[i]).InvY,pTempItems(ItemList.Items[i]).InvX+pTempItems(ItemList.Items[i]).W, pTempItems(ItemList.Items[i]).InvY+pTempItems(ItemList.Items[i]).H),Rect(0, 0, pTempItems(ItemList.Items[i]).W, pTempItems(ItemList.Items[i]).H),pTempItems(ItemList.Items[i]).DXShadow ,True,ShadowAlpha);
       lpDDSBack.BltFast(pTempItems(ItemList.Items[i]).InvX, pTempItems(ItemList.Items[i]).InvY, pTempItems(ItemList.Items[i]).DXSurface, Rect(0, 0, pTempItems(ItemList.Items[i]).W, pTempItems(ItemList.Items[i]).H), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT)
    end
    else //In the ground slot so plot iconic image
       lpDDSBack.BltFast(pTempItems(ItemList.Items[i]).InvX, pTempItems(ItemList.Items[i]).InvY, pTempItems(ItemList.Items[i]).DXSurfaceIcon, Rect(0, 0, GroundListWidth, GroundListHeight), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
  end;  }

  //new code July 8 2000
    DontPlotCurrentItem := false;
    paint;
  //end new code

    pText.PlotText( IntToStr( Character.money ) + ' ' + txtCrowns, 240 - pText.TextLength( IntToStr( Character.money ) + ' ' + txtCrowns ), 10, Alpha );
  //Whew! Now we flip it all to the screen
    MouseCursor.Cleanup;
    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    lpDDSBack.BltFast( 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;
{$ENDIF}
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //Tinventory.init

procedure TInventory.MouseDown( Sender : TAniview; Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer; GridX, GridY : integer );
var
  i, j : Integer;
  pTemp : Pointer;
  //a,b: string;
  B1, B2, B3, B4 : Boolean;
  rRect : TRect;
  DontDropInGround : boolean;
const
  FailName : string = 'TInventory.mousedown';
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
      end
      else if ( X > 259 ) and ( X < 277 ) and ( Y > 385 ) and ( Y < 418 ) then
      begin //left arrow for ground
        if GroundOrderList.Count > 1 then
        begin //get the prev item on the ground and show it
          j := TopGroundIndex;
          if j <> 0 then
          begin //if its not the first item in the list
          //replace the back from the DXBack buffer.
          //lpDDSBack.BltFast(pTempItems(GroundOrderList.Items[j]).InvX, pTempItems(GroundOrderList.Items[j]).InvY, DXBack, Rect(pTempItems(GroundOrderList.Items[j]).InvX, pTempItems(GroundOrderList.Items[j]).InvY, pTempItems(GroundOrderList.Items[j]).InvX + pTempItems(GroundOrderList.Items[j]).W, pTempItems(GroundOrderList.Items[j]).InvY + pTempItems(GroundOrderList.Items[j]).H), DDBLTFAST_WAIT);
            lpDDSBack.BltFast( 275, 384, DXBack, Rect( 275, 384, 353, 416 ), DDBLTFAST_WAIT );
            pTempItems( GroundOrderList.Items[ j ] ).InvX := 999;
            pTempItems( GroundOrderList.Items[ j ] ).InvY := 999;
            j := j - 1;
          //Set the coordinates of the new item and Plot it
            pTempItems( GroundOrderList.Items[ j ] ).InvX := 277; //315-pTempItems(GroundOrderList.Items[j]).IW div 2;
            pTempItems( GroundOrderList.Items[ j ] ).InvY := 386; //401-pTempItems(GroundOrderList.Items[j]).IH div 2;
            lpDDSBack.BltFast( pTempItems( GroundOrderList.Items[ j ] ).InvX, pTempItems( GroundOrderList.Items[ j ] ).InvY, pTempItems( GroundOrderList.Items[ j ] ).DXSurfaceIcon, Rect( 0, 0, GroundListWidth, GroundListHeight ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
            TopGroundIndex := j;
          end
          else
          begin
          //making an obnoxious buzzing noise? - we cant go backwards from the first item
          end;
        end;
      end
      else if ( X > 353 ) and ( X < 365 ) and ( Y > 391 ) and ( Y < 418 ) then
      begin //right arrow for ground
        if GroundOrderList.Count > 1 then
        begin //get the Next item on the ground and show it
          j := TopGroundIndex;
          if j < ( GroundOrderList.Count - 1 ) then
          begin //if its not the last item in the list
          //replace the back from the DXBack buffer.
          //lpDDSBack.BltFast(pTempItems(GroundOrderList.Items[j]).InvX, pTempItems(GroundOrderList.Items[j]).InvY, DXBack, Rect(pTempItems(GroundOrderList.Items[j]).InvX, pTempItems(GroundOrderList.Items[j]).InvY, pTempItems(GroundOrderList.Items[j]).InvX + pTempItems(GroundOrderList.Items[j]).W, pTempItems(GroundOrderList.Items[j]).InvY + pTempItems(GroundOrderList.Items[j]).H), DDBLTFAST_WAIT);
            lpDDSBack.BltFast( 275, 384, DXBack, Rect( 275, 384, 353, 416 ), DDBLTFAST_WAIT );
            pTempItems( GroundOrderList.Items[ j ] ).InvX := 999;
            pTempItems( GroundOrderList.Items[ j ] ).InvY := 999;
            j := j + 1;
          //Set the coordinates of the new item and Plot it
            pTempItems( GroundOrderList.Items[ j ] ).InvX := 277; //315-pTempItems(GroundOrderList.Items[j]).IW div 2;
            pTempItems( GroundOrderList.Items[ j ] ).InvY := 386; //401-pTempItems(GroundOrderList.Items[j]).IH div 2;
            lpDDSBack.BltFast( pTempItems( GroundOrderList.Items[ j ] ).InvX, pTempItems( GroundOrderList.Items[ j ] ).InvY, pTempItems( GroundOrderList.Items[ j ] ).DXSurfaceIcon, Rect( 0, 0, GroundListWidth, GroundListHeight ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
            TopGroundIndex := j;
          end
          else
          begin
          //making an obnoxious buzzing noise? - we cant go forwards from the last item
          end;
        end;
      end
      else if ( x > 275 ) and ( x < 353 ) and ( y > 384 ) and ( y < 416 ) and ( CurrentSelectedItem = -1 ) then
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
            lpDDSBack.BltFast( 275, 384, DXBack, Rect( 275, 384, 353, 416 ), DDBLTFAST_WAIT ); //clean the box
            if GroundOrderList.Count > 1 then
            begin //get the next item on the ground and show it
              j := TopGroundIndex; //GroundOrderList.IndexOf(ItemList.Items[CurrentSelectedItem]);
              if ( j = ( GroundOrderList.Count - 1 ) ) then //if its the last item in the list
                j := 0 //set it to the first one
              else //set it to the item folowing this one
                j := j + 1;
              pTempItems( GroundOrderList.Items[ j ] ).InvX := 277; //315-pTempItems(GroundOrderList.Items[j]).IW div 2;
              pTempItems( GroundOrderList.Items[ j ] ).InvY := 386; //401-pTempItems(GroundOrderList.Items[j]).IH div 2;
              lpDDSBack.BltFast( pTempItems( GroundOrderList.Items[ j ] ).InvX, pTempItems( GroundOrderList.Items[ j ] ).InvY, pTempItems( GroundOrderList.Items[ j ] ).DXSurfaceIcon, Rect( 0, 0, GroundListWidth, GroundListHeight ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
              pTemp := Pointer( GroundOrderList.Items[ j ] ); //save the pointer to the new topmost item so we can do the delete and still track it
              GroundOrderList.Delete( TopGroundIndex ); //GroundOrderList.IndexOf(ItemList.Items[CurrentSelectedItem])); //remove this item from the GroundList pointer list
              TopGroundIndex := GroundOrderList.IndexOf( pTempItems( pTemp ) );
              //TopGroundIndex:=GroundOrderList.IndexOf(GroundOrderList.items[j]);
            end
            else
            begin
              GroundOrderList.Delete( TopGroundIndex ); //GroundOrderList.IndexOf(ItemList.Items[CurrentSelectedItem])); //remove this item from the GroundList pointer list
              TopGroundIndex := -1;
            end;
            ShowLegalSlots( CurrentSelectedItem ); //show the slots this item will fit in
            //Compute the coords for the floating item
            Tx := ( X ) - pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W div 2;
            Ty := ( Y ) - pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H div 2;
            //Plot relevant text
            lpDDSBack.BltFast( ClearLeft, ClearTop, DXBack, Rect( ClearLeft, ClearTop, ClearRight, ClearBottom ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot test
            if UseSmallFont then
              pText.PlotTinyTextBlock( GetSlotText, ClearLeft, ClearRight, SmlMsg, Alpha )
            else
              pText.PlotText( GetSlotText, ClearLeft, LrgMsg, Alpha );
            //save the background to the dirty DD surface based on the floating item
            DXDirty.BltFast( 0, 0, lpDDSBack, Rect( Tx, Ty, Tx + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, Ty + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ), DDBLTFAST_WAIT );
            //plot the item centered under the mouse pointer
            DrawSub( lpDDSBack, rect( Tx, Ty, Tx + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, Ty + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ), Rect( 0, 0, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ), pTempItems( ItemList.Items[ CurrentSelectedItem ] ).DXShadow, True, ShadowAlpha );
            lpDDSBack.BltFast( Tx, Ty, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).DXSurface, Rect( 0, 0, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
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
          if ( ( ( x ) >= pTempItems( ItemList.Items[ i ] ).InvX ) and ( ( x ) <= ( pTempItems( ItemList.Items[ i ] ).InvX + pTempItems( ItemList.Items[ i ] ).W ) ) and ( ( Y ) >= pTempItems( ItemList.Items[ i ] ).InvY ) and ( ( Y ) <= pTempItems( ItemList.Items[ i ] ).InvY + pTempItems( ItemList.Items[ i ] ).H ) and ( ( pTempItems( ItemList.Items[ i ] ).ItemType = 'Inventory' ) or ( pTempItems( ItemList.Items[ i ] ).ItemType = 'Equipment' ) ) ) then
          begin
            if Button = mbRight then
            begin
              DlgScroll.OpenStatsScroll( pTempItems( ItemList.Items[ i ] ).pItem );
            end
//          else if (X > 250) and Locked then begin
            else if ( X > 250 ) and Locked then
            begin
              //dont let players pick up item from the body of an NPC
            end
            else if ( X > 250 ) and not ( pTempItems( ItemList.Items[ i ] ).pItem.CanEquip( TSlot( pTempItems( ItemList.Items[ i ] ).BodySlot ), Character ) ) then
            begin
              //dont let players pick up an object that is in an invalid slot
            end
            else if ( X > 250 ) and Character.EquipmentLocked[ TSlot( pTempItems( ItemList.Items[ i ] ).BodySlot ) ] then
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
              if X > 250 then
              begin //If we're pulling this off of the body, then clear that slot
                SlotCoord[ pTempItems( ItemList.Items[ i ] ).BodySlot ].UsedBy := -1;
                pTempItems( ItemList.Items[ i ] ).BodySlot := -1; //JD 6/14/2000
              end;
              ShowLegalSlots( CurrentSelectedItem ); //show the slots this item will fit in
            //Compute the coords for the floating item
              Tx := ( X ) - pTempItems( ItemList.Items[ i ] ).W div 2;
              Ty := ( Y ) - pTempItems( ItemList.Items[ i ] ).H div 2;

              if Tx < 0 then
                Tx := 0;
              if Ty < 0 then
                Ty := 0;
              if ( Tx + pTempItems( ItemList.Items[ i ] ).W ) > 659 then
                Tx := 659 - pTempItems( ItemList.Items[ i ] ).W;
              if ( Ty + pTempItems( ItemList.Items[ i ] ).H ) > 463 then
                Ty := 463 - pTempItems( ItemList.Items[ i ] ).H;

            //save the background to the dirty DD surface based on the floating item
              DXDirty.BltFast( 0, 0, lpDDSBack, Rect( Tx, Ty, Tx + pTempItems( ItemList.Items[ i ] ).W, Ty + pTempItems( ItemList.Items[ i ] ).H ), DDBLTFAST_WAIT );
            //plot the item centered under the mouse pointer
              DrawSub( lpDDSBack, rect( Tx, Ty, Tx + pTempItems( ItemList.Items[ i ] ).W, Ty + pTempItems( ItemList.Items[ i ] ).H ), Rect( 0, 0, pTempItems( ItemList.Items[ i ] ).W, pTempItems( ItemList.Items[ i ] ).H ), pTempItems( ItemList.Items[ i ] ).DXShadow, True, ShadowAlpha );
              lpDDSBack.BltFast( Tx, Ty, pTempItems( ItemList.Items[ i ] ).DXSurface, Rect( 0, 0, pTempItems( ItemList.Items[ i ] ).W, pTempItems( ItemList.Items[ i ] ).H ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
              ContainCursor( 1 );
            //Plot relevant text
              lpDDSBack.BltFast( ClearLeft, ClearTop, DXBack, Rect( ClearLeft, ClearTop, ClearRight, ClearBottom ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot test
              if UseSmallFont then
                pText.PlotTinyTextBlock( GetSlotText, ClearLeft, ClearRight, SmlMsg, Alpha )
              else
                pText.PlotText( GetSlotText, ClearLeft, LrgMsg, Alpha );
            end;
          end;
          i := i + 1;
        end; //wend
      end; //the if relating to the arrows
    end
    else
    begin //drop the piece if we can
    //cleanup
      lpDDSBack.BltFast( Tx, Ty, DXDirty, Rect( 0, 0, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ), DDBLTFAST_WAIT );
    //try to drop on ground
    //first, if it's the leg1 slot, we add this in to make it easier to drop items in leg1 and avoid the ground slot
      DontDropInGround := false;
      if pTempItems( ItemList.Items[ CurrentSelectedItem ] ).PItem.CanEquip( slLeg1, Character ) then
      begin
        if PtinRect( SlotCoord[ 0 ].rect, point( X, Y ) ) then
          DontDropInGround := true;
      end;
      if pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H > 106 then
      begin
        if Tx + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W > 360 then
          DontDropInGround := true;
      end;
    //make sure it isn't a quest item- cant drop those
      if pTempItems( ItemList.Items[ CurrentSelectedItem ] ).DXSurfaceIcon = nil then
      begin
        DontDropInGround := true;
        ErrorCode := 256;
      end;
      if ( ( DontDropInGround = false ) and ( intersectRect( rRect, rect( 276, 386, 353, 416 ), rect( Tx, Ty, Tx + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, Ty + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ) ) ) ) then
      begin
      //DebugPrint(IntToStr(TopGroundIndex));
        if GroundOrderList.Count > 0 then
        begin //If we have any ground items
          pTempItems( GroundOrderList.Items[ TopGroundIndex ] ).InvX := 999; //put old item offscreen- no longer on top
          pTempItems( GroundOrderList.Items[ TopGroundIndex ] ).InvY := 999;
        //GroundOrderList.Insert(TopGroundIndex, pTempItems(ItemList.Items[CurrentSelectedItem]));
          j := GroundOrderList.Add( pTempItems( ItemList.Items[ CurrentSelectedItem ] ) );
          GroundOrderList.Move( j, TopGroundIndex );
        end
        else
        begin //there are no items in this list - this will automatically become zero (top spot)
          GroundOrderList.Add( pTempItems( ItemList.Items[ CurrentSelectedItem ] ) );
          TopGroundIndex := 0;
        end;

        lpDDSBack.BltFast( 276, 386, DXBack, Rect( 276, 386, 353, 416 ), DDBLTFAST_WAIT ); //clean out the Ground box
        pTempItems( ItemList.Items[ CurrentSelectedItem ] ).InvX := 277; //315-pTempItems(ItemList.Items[CurrentSelectedItem]).IW div 2;
        pTempItems( ItemList.Items[ CurrentSelectedItem ] ).InvY := 386; //401-pTempItems(ItemList.Items[CurrentSelectedItem]).IH div 2;
        pTempItems( ItemList.Items[ CurrentSelectedItem ] ).BodySlot := -1;
        pTempItems( ItemList.Items[ CurrentSelectedItem ] ).ItemType := 'Ground';
        lpDDSBack.BltFast( pTempItems( ItemList.Items[ CurrentSelectedItem ] ).InvX, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).InvY,
          pTempItems( ItemList.Items[ CurrentSelectedItem ] ).DXSurfaceIcon, Rect( 0, 0, GroundListWidth, GroundListHeight ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        lpDDSBack.BltFast( ClearLeft, ClearTop, DXBack, Rect( ClearLeft, ClearTop, ClearRight, ClearBottom ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clear text
        CleanUpLegalSlots;
        CurrentSelectedItem := -1;
        ContainCursor( 0 );
      end
      else if ( X > 250 ) and ( x < 652 ) and ( Y < 438 ) and ( Locked = false ) then
      begin //is the mouse over on the right (body side) of the screen
        i := TryToDropOnBody( X, Y );
        if i > -1 then
        begin //if we dropped in a slot successfully
          Tx := SlotCoord[ i ].cx - ( pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W div 2 );
          if ( i = 11 ) or ( i = 12 ) then
          begin //if Weapon or shield, they Hang from the center point
            Ty := SlotCoord[ i ].cy - pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H div 4;
            if ( i = 11 ) then
              WeaponPlacement( CurrentSelectedItem );
          end
          else //otherwise center the item in the slot
            Ty := SlotCoord[ i ].cy - ( pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H div 2 );
        //plot dropped item in slot
          DrawSub( lpDDSBack, rect( Tx, Ty, Tx + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, Ty + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ), Rect( 0, 0, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ), pTempItems( ItemList.Items[ CurrentSelectedItem ] ).DXShadow, True, ShadowAlpha );
          lpDDSBack.BltFast( Tx, Ty, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).DXSurface, Rect( 0, 0, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
          pTempItems( ItemList.Items[ CurrentSelectedItem ] ).InvX := Tx; //save new locations
          pTempItems( ItemList.Items[ CurrentSelectedItem ] ).InvY := Ty;
          pTempItems( ItemList.Items[ CurrentSelectedItem ] ).BodySlot := i;
          pTempItems( ItemList.Items[ CurrentSelectedItem ] ).ItemType := 'Equipment';
          pTempItems( ItemList.Items[ CurrentSelectedItem ] ).CharacterHadThisOnHim := true;
          if SlotCoord[ i ].UsedBy > -1 then
          begin //item already there, pass it to the mouse as the new floater
          //assign Slot to dropped item and CurrentSelectedItem to the item we got from the slot; swap them
            j := SlotCoord[ i ].UsedBy;
            if not ( pTempItems( ItemList.Items[ j ] ).pItem.CanEquip( TSlot( pTempItems( ItemList.Items[ j ] ).BodySlot ), Character ) ) then
            begin
              //dont let players pick up an object that is in an invalid slot
            end
            else if Character.EquipmentLocked[ TSlot( pTempItems( ItemList.Items[ j ] ).BodySlot ) ] then
            begin
              //dont let players pick up a locked object
            end
            else
            begin
              SlotCoord[ i ].UsedBy := CurrentSelectedItem;
              CurrentSelectedItem := j;
              pTempItems( ItemList.Items[ CurrentSelectedItem ] ).BodySlot := -1;
              DontPlotCurrentItem := true;
              CleanUpLegalSlots; //clean up after old circles
              DontPlotCurrentItem := false;
              ShowLegalSlots( CurrentSelectedItem ); //show the slots this item will fit in
            //Compute the coords for the floating item
              Tx := ( X ) - pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W div 2;
              Ty := ( Y ) - pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H div 2;
            //save the background to the dirty DD surface based on the floating item
              DXDirty.BltFast( 0, 0, lpDDSBack, Rect( Tx, Ty, Tx + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, Ty + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ), DDBLTFAST_WAIT );
            //plot the item centered under the mouse pointer
              DrawSub( lpDDSBack, rect( Tx, Ty, Tx + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, Ty + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ), Rect( 0, 0, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ), pTempItems( ItemList.Items[ CurrentSelectedItem ] ).DXShadow, True, ShadowAlpha );
              if assigned( pTempItems( ItemList.Items[ CurrentSelectedItem ] ).DXSurface ) then
                lpDDSBack.BltFast( Tx, Ty, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).DXSurface, Rect( 0, 0, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
            // //assign Slot to dropped item and CurrentSelectedItem to the item we got from the slot; swap them
            // j:=SlotCoord[i].UsedBy;
            // SlotCoord[i].UsedBy:=CurrentSelectedItem;
            // CurrentSelectedItem:=j;
            // pTempItems(ItemList.Items[CurrentSelectedItem]).BodySlot:=-1;
              //Plot relevant text
              lpDDSBack.BltFast( ClearLeft, ClearTop, DXBack, Rect( 20, ClearTop, ClearRight, ClearBottom ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot test
              if UseSmallFont then
                pText.PlotTinyTextBlock( GetSlotText, ClearLeft, ClearRight, SmlMsg, Alpha )
              else
                pText.PlotText( GetSlotText, ClearLeft, LrgMsg, Alpha );
            end;
          end
          else
          begin //The slot was empty, so - clear the CurrentSelectedItem item and clear the text
            lpDDSBack.BltFast( ClearLeft, ClearTop, DXBack, Rect( ClearLeft, ClearTop, ClearRight, ClearBottom ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
            SlotCoord[ i ].UsedBy := CurrentSelectedItem;
            CleanUpLegalSlots;
            CurrentSelectedItem := -1;
            ContainCursor( 0 );
          end;
        end
        else
        begin //plot error message- illegal drop tried - might not ne necessary
           //clean up - this plots the objects dirty, then the new text, then saves the dirty - prevents Dirty errors
          lpDDSBack.BltFast( Tx, Ty, DXDirty, Rect( 0, 0, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ), DDBLTFAST_WAIT );
          lpDDSBack.BltFast( ClearLeft, ClearTop, DXBack, Rect( ClearLeft, ClearTop, ClearRight, ClearBottom ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
          if UseSmallFont then
          begin
            if ErrorCode = 11 then //weapon error
              pText.PlotTinyTextBlock( txtMessage[ 0 ], ClearLeft, ClearRight, SmlMsg, Alpha )
            else if ErrorCode = 12 then //shield error
              pText.PlotTinyTextBlock( txtMessage[ 1 ], ClearLeft, ClearRight, SmlMsg, Alpha )
            else if ErrorCode = 255 then //Quest item drop on ground error
              pText.PlotTinyTextBlock( txtMessage[ 2 ], ClearLeft, ClearRight, SmlMsg, Alpha );
          end
          else
          begin
            if ErrorCode = 11 then //weapon error
              pText.PlotText( txtMessage[ 0 ], ClearLeft, LrgMsg, Alpha )
            else if ErrorCode = 12 then //shield error
              pText.PlotText( txtMessage[ 1 ], ClearLeft, LrgMsg, Alpha )
            else if ErrorCode = 255 then //Quest item drop on ground error
              pText.PlotText( txtMessage[ 2 ], ClearLeft, LrgMsg, Alpha );
          end;
          ErrorCode := -1;
          //save the background to the dirty DD surface based on the floating item
          DXDirty.BltFast( 0, 0, lpDDSBack, Rect( Tx, Ty, Tx + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, Ty + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ), DDBLTFAST_WAIT );
          DrawSub( lpDDSBack, rect( Tx, Ty, Tx + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, Ty + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ), Rect( 0, 0, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ), pTempItems( ItemList.Items[ CurrentSelectedItem ] ).DXShadow, True, ShadowAlpha );
          lpDDSBack.BltFast( Tx, Ty, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).DXSurface, Rect( 0, 0, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

        end;
      end
      else
      begin //check inventory side
        B1 := true; //((X - (pTempItems(ItemList.Items[CurrentSelectedItem]).W div 2) > 2) and (Y - (pTempItems(ItemList.Items[CurrentSelectedItem]).H div 2) > 35)); //is it on the grid?
        B2 := true; //(X < (237 - (pTempItems(ItemList.Items[CurrentSelectedItem]).W div 2 - 9))); //is it on the right side of the grid within 1 block?
        if not B2 then
        begin
          X := X - 18; //we redo this- added a forgivness factor for dropping on right edge, we need to move this back a slot
          B2 := ( X < ( 237 - ( pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W div 2 - 9 ) ) );
        end;
        B3 := true; //(Y < (421 - (pTempItems(ItemList.Items[CurrentSelectedItem]).H div 2 - 9))); //bottom side within 1 block
        if not B3 then
        begin
          Y := Y - 15; //we redo this- added a forgivness factor for dropping on right edge, we need to move this up a slot
          B3 := ( Y < ( 421 - ( pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H div 2 ) ) );
        end;
        B4 := DropAnItem( X, Y ); //CollisionCheck(X, Y);       //does it collide with any other items already in inventory?
        if ( B1 and B2 and B3 and B4 ) then
        begin //plot the item on the grid if it fits
        //Tx := Integer((X - 14 - (pTempItems(ItemList.Items[CurrentSelectedItem]).W div 2)) div 18) * 18 + 20;
        //Ty := Integer((Y - 47 - (pTempItems(ItemList.Items[CurrentSelectedItem]).H div 2)) div 26) * 26 + 57;
          DrawSub( lpDDSBack, rect( Tx, Ty, Tx + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, Ty + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ), Rect( 0, 0, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ), pTempItems( ItemList.Items[ CurrentSelectedItem ] ).DXShadow, True, ShadowAlpha );
          lpDDSBack.BltFast( Tx, Ty, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).DXSurface, Rect( 0, 0, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
          pTempItems( ItemList.Items[ CurrentSelectedItem ] ).InvX := Tx; //X-pTempItems(ItemList.Items[CurrentSelectedItem]).W div 2;
          pTempItems( ItemList.Items[ CurrentSelectedItem ] ).InvY := Ty; //Y-pTempItems(ItemList.Items[CurrentSelectedItem]).H div 2;
          pTempItems( ItemList.Items[ CurrentSelectedItem ] ).ItemType := 'Inventory';
          pTempItems( ItemList.Items[ CurrentSelectedItem ] ).CharacterHadThisOnHim := true;
          pTempItems( ItemList.Items[ CurrentSelectedItem ] ).BodySlot := -1;
          CleanUpLegalSlots;
          CurrentSelectedItem := -1;
          ContainCursor( 0 );
          lpDDSBack.BltFast( ClearLeft, ClearTop, DXBack, Rect( ClearLeft, ClearTop, ClearRight, ClearBottom ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //erase any message on the screen
        end
        else
        begin //plot failure message
           //clean up - this plots the objects dirty, then the new text, then saves the dirty - prevents Dirty errors
          lpDDSBack.BltFast( Tx, Ty, DXDirty, Rect( 0, 0, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ), DDBLTFAST_WAIT );
          lpDDSBack.BltFast( ClearLeft, ClearTop, DXBack, Rect( ClearLeft, ClearTop, ClearRight, ClearBottom ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
          if UseSmallFont then
            pText.PlotTinyTextBlock( txtMessage[ 3 ], ClearLeft, ClearRight, SmlMsg, Alpha )
          else
            pText.PlotText( txtMessage[ 3 ], ClearLeft, LrgMsg, Alpha );
          //save the background to the dirty DD surface based on the floating item
          DXDirty.BltFast( 0, 0, lpDDSBack, Rect( Tx, Ty, Tx + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, Ty + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ), DDBLTFAST_WAIT );
          DrawSub( lpDDSBack, rect( Tx, Ty, Tx + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, Ty + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ), Rect( 0, 0, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ), pTempItems( ItemList.Items[ CurrentSelectedItem ] ).DXShadow, True, ShadowAlpha );
          lpDDSBack.BltFast( Tx, Ty, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).DXSurface, Rect( 0, 0, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        end;
      end;
    end; //endif

    if Loaded then
    begin
      WriteTheInventoryData;
      lpDDSFront.Flip( nil, DDFLIP_WAIT );
      lpDDSBack.BltFast( 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
      MouseCursor.PlotDirty := false;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TInventory.MouseDown


procedure TInventory.MouseMove( Sender : TAniview; Shift : TShiftState; X, Y : Integer; GridX, GridY : integer );
var
  Tw, Th : Integer;
  i : Integer;
const
  FailName : string = 'TInventory.MouseMove';
begin
  try //This assigned(DXBack) is here to keep the program from crashing while Im developing it
    if ( CurrentSelectedItem > -1 ) and Assigned( DXBack ) then
    begin //are we dragging an item?
    //clean up
      lpDDSBack.BltFast( Tx, Ty, DXDirty, Rect( 0, 0, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ), DDBLTFAST_WAIT );
    //Compute the coords for the floating item
      Tx := ( X ) - pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W div 2;
      Ty := ( Y ) - pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H div 2;
      if Tx < 0 then
        Tx := 0;
      if Ty < 0 then
        Ty := 0;
      Tw := pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W;
      if ( Tx + Tw ) > 659 then
        Tx := 659 - Tw;

      Th := pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H;
      if ( Ty + Th ) > 463 then
        Ty := 463 - Th;

    //save the background to the dirty DD surface based on the floating item
      DXDirty.BltFast( 0, 0, lpDDSBack, Rect( Tx, Ty, Tx + Tw, Ty + Th ), DDBLTFAST_WAIT );
    //plot the item centered under the mouse pointer
      DrawSub( lpDDSBack, rect( Tx, Ty, Tx + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, Ty + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ), Rect( 0, 0, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ), pTempItems( ItemList.Items[ CurrentSelectedItem ] ).DXShadow, True, ShadowAlpha );
      lpDDSBack.BltFast( Tx, Ty, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).DXSurface, Rect( 0, 0, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W, pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      lpDDSFront.Flip( nil, DDFLIP_WAIT );
      lpDDSBack.BltFast( 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
      MouseCursor.PlotDirty := false;
    end
    else if Assigned( DXBack ) and ( DlgScroll.ScrollIsShowing = False ) then
    begin //do the rollover
      i := 0; //find the item the mouse is down over
      lpDDSBack.BltFast( ClearLeft, ClearTop, DXBack, Rect( ClearLeft, ClearTop, ClearRight, ClearBottom ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
      while ( i < ItemList.Count ) and ( CurrentSelectedItem = -1 ) do
      begin
        if ( ( x ) >= pTempItems( ItemList.Items[ i ] ).InvX ) and ( ( x ) <= ( pTempItems( ItemList.Items[ i ] ).InvX + pTempItems( ItemList.Items[ i ] ).W ) ) and ( ( Y ) >= pTempItems( ItemList.Items[ i ] ).InvY ) and ( ( Y ) <= pTempItems( ItemList.Items[ i ] ).InvY + pTempItems( ItemList.Items[ i ] ).H ) then
        begin
          CurrentSelectedItem := i; //assign it for the sake of PlotText
          if i = SlotCoord[ 11 ].UsedBy then
          begin //if Over weapon
            if UseSmallFont then
            begin
              if Character.Strength < TWeapon( pTempItems( ItemList.Items[ CurrentSelectedItem ] ).PItem ).MinStrength then //Weapon minstrength error
                pText.PlotTinyTextBlock( txtMessage[ 4 ], ClearLeft, ClearRight, SmlMsg, Alpha )
              else if Character.Coordination < TWeapon( pTempItems( ItemList.Items[ CurrentSelectedItem ] ).PItem ).MinCoordination then //weap mincoord
                pText.PlotTinyTextBlock( txtMessage[ 5 ], ClearLeft, ClearRight, SmlMsg, Alpha )
              else if Character.Restriction > TWeapon( pTempItems( ItemList.Items[ CurrentSelectedItem ] ).PItem ).MaxRestriction then //maxRestriction
                pText.PlotTinyTextBlock( txtMessage[ 6 ], ClearLeft, ClearRight, SmlMsg, Alpha )
              else
                pText.PlotTinyTextBlock( GetSlotText + txtMessage[ 7 ], ClearLeft, ClearRight, SmlMsg, Alpha );
            end
            else
            begin
              if Character.Strength < TWeapon( pTempItems( ItemList.Items[ CurrentSelectedItem ] ).PItem ).MinStrength then //Weapon minstrength error
                pText.PlotText( txtMessage[ 4 ], ClearLeft, LrgMsg, Alpha )
              else if Character.Coordination < TWeapon( pTempItems( ItemList.Items[ CurrentSelectedItem ] ).PItem ).MinCoordination then //weap mincoord
                pText.PlotText( txtMessage[ 5 ], ClearLeft, LrgMsg, Alpha )
              else if Character.Restriction > TWeapon( pTempItems( ItemList.Items[ CurrentSelectedItem ] ).PItem ).MaxRestriction then //maxRestriction
                pText.PlotText( txtMessage[ 6 ], ClearLeft, LrgMsg, Alpha )
              else
                pText.PlotTinyTextBlock( GetSlotText + txtMessage[ 7 ], ClearLeft, ClearRight, SmlMsg, Alpha );
//                pText.PlotText(GetSlotText + txtMessage[7], ClearLeft, LrgMsg,Alpha);
            end;
          end
          else
          begin
//          if UseSmallFont then
            pText.PlotTinyTextBlock( GetSlotText + txtMessage[ 7 ], ClearLeft, ClearRight, SmlMsg, Alpha );
//          else
//            pText.PlotText(GetSlotText + txtMessage[7], ClearLeft, LrgMsg,Alpha);
          end;
        end;
        i := i + 1;
      end; //wend
    //If we arent over an item see if we're over the ground slot
      if ( x > 275 ) and ( x < 353 ) and ( y > 384 ) and ( y < 416 ) and ( CurrentSelectedItem = -1 ) then
      begin //over the ground slot
        if GroundOrderList.Count > 0 then
        begin
          CurrentSelectedItem := ItemList.IndexOf( GroundOrderList.items[ TopGroundIndex ] );
//          if UseSmallFont then
          pText.PlotTinyTextBlock( ( GetSlotText + txtMessage[ 7 ] ), ClearLeft, ClearRight, SmlMsg, Alpha );
//          else
//            pText.PlotText((GetSlotText + txtMessage[7]), ClearLeft, LrgMsg,Alpha);
        end;
      end; //endif
    //Clean up arrows and back to game
      lpDDSBack.BltFast( 261, 394, DXBack, Rect( 261, 394, 261 + 14, 394 + 15 ), DDBLTFAST_WAIT );
      lpDDSBack.BltFast( 354, 396, DXBack, Rect( 354, 396, 354 + 11, 396 + 11 ), DDBLTFAST_WAIT );
      lpDDSBack.BltFast( 595, 416, DXBack, Rect( 595, 416, 595 + 74, 416 + 46 ), DDBLTFAST_WAIT );
      if CurrentSelectedItem = -1 then
      begin //If we arent over an item then check arrows and back button
        if PtinRect( rect( 259, 385, 277, 418 ), point( X, Y ) ) then
        begin //over left arrow
          //plot highlighted arrow
          lpDDSBack.BltFast( 261, 394, DXLeftArrow, Rect( 0, 0, 14, 15 ), DDBLTFAST_WAIT );
          //plot a bit of informative text
          lpDDSBack.BltFast( ClearLeft, ClearTop, DXBack, Rect( ClearLeft, ClearTop, ClearRight, ClearBottom ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
          if UseSmallFont then
            pText.PlotTinyTextBlock( txtMessage[ 8 ], ClearLeft, ClearRight, SmlMsg, Alpha )
          else
            pText.PlotText( txtMessage[ 8 ], ClearLeft, LrgMsg, Alpha );
        end
        else if PtinRect( rect( 353, 391, 365, 418 ), point( X, Y ) ) then
        begin //over right arrow
          //plot highlighted arrow
          lpDDSBack.BltFast( 354, 396, DXRightArrow, Rect( 0, 0, 11, 11 ), DDBLTFAST_WAIT );
          //plot a bit of informative text
          lpDDSBack.BltFast( ClearLeft, ClearTop, DXBack, Rect( ClearLeft, ClearTop, ClearRight, ClearBottom ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
          if UseSmallFont then
            pText.PlotTinyTextBlock( txtMessage[ 9 ], ClearLeft, ClearRight, SmlMsg, Alpha )
          else
            pText.PlotText( txtMessage[ 9 ], ClearLeft, LrgMsg, Alpha );
        end
        else if PtinRect( rect( 595, 416, 595 + 74, 416 + 46 ), point( X, Y ) ) then
        begin //over back button
          //plot highlighted back to game
          lpDDSBack.BltFast( 595, 416, DXBackToGame, Rect( 0, 0, 74, 46 ), DDBLTFAST_WAIT );
          //don't plot a bit of informative text, just clean up
          lpDDSBack.BltFast( ClearLeft, ClearTop, DXBack, Rect( ClearLeft, ClearTop, ClearRight, ClearBottom ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clean up before we plot text
        end;
      end; //endif CurrentSelectedItem
      CurrentSelectedItem := -1; //deassign it
      lpDDSFront.Flip( nil, DDFLIP_WAIT );
      lpDDSBack.BltFast( 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
      MouseCursor.PlotDirty := false;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //TInventory.MouseMove


procedure TInventory.MouseUp( Sender : TAniview; Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer; GridX, GridY : integer );
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

procedure TInventory.Paint;
var
  i : Integer;
const
  FailName : string = 'TInventory.paint';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    lpDDSBack.BltFast( 0, 0, DXBack, Rect( 0, 0, 679, 476 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    pText.PlotText( IntToStr( Character.money ) + ' ' + txtCrowns, 240 - pText.TextLength( IntToStr( Character.money ) + ' ' + txtCrowns ), 10, Alpha );
  //Now plot all of the items on the grid
    for i := 0 to ItemList.Count - 1 do
    begin
      if ( DontPlotCurrentItem = false ) or ( i <> CurrentSelectedItem ) then
      begin
        if pTempItems( ItemList.Items[ i ] ).ItemType <> 'Ground' then
        begin //if not in the ground slot
          if assigned( pTempItems( ItemList.Items[ i ] ).DXSurface ) then
          begin
            DrawSub( lpDDSBack, rect( pTempItems( ItemList.Items[ i ] ).InvX, pTempItems( ItemList.Items[ i ] ).InvY, pTempItems( ItemList.Items[ i ] ).InvX + pTempItems( ItemList.Items[ i ] ).W, pTempItems( ItemList.Items[ i ] ).InvY + pTempItems( ItemList.Items[ i ] ).H ), Rect( 0, 0, pTempItems( ItemList.Items[ i ] ).W, pTempItems( ItemList.Items[ i ] ).H ), pTempItems( ItemList.Items[ i ] ).DXShadow, True, ShadowAlpha );
            lpDDSBack.BltFast( pTempItems( ItemList.Items[ i ] ).InvX, pTempItems( ItemList.Items[ i ] ).InvY, pTempItems( ItemList.Items[ i ] ).DXSurface, Rect( 0, 0, pTempItems( ItemList.Items[ i ] ).W, pTempItems( ItemList.Items[ i ] ).H ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
              //If player stats are too low dim weapon-New June 8 00
            if ( pTempItems( ItemList.Items[ i ] ).BodySlot = 11 ) and ( pTempItems( ItemList.Items[ i ] ).PItem is TWeapon ) then
            begin
              if ( Character.Strength < TWeapon( pTempItems( ItemList.Items[ i ] ).PItem ).MinStrength ) //Weapon minstrength error
                or ( Character.Coordination < TWeapon( pTempItems( ItemList.Items[ i ] ).PItem ).MinCoordination ) //weap mincoord
                or ( Character.Restriction > TWeapon( pTempItems( ItemList.Items[ i ] ).PItem ).MaxRestriction ) then
              begin //maxRestriction
                DrawAlpha( lpDDSBack, rect( pTempItems( ItemList.Items[ i ] ).InvX, pTempItems( ItemList.Items[ i ] ).InvY, pTempItems( ItemList.Items[ i ] ).InvX + pTempItems( ItemList.Items[ i ] ).W, pTempItems( ItemList.Items[ i ] ).InvY + pTempItems( ItemList.Items[ i ] ).H ), rect( pTempItems( ItemList.Items[ i ] ).InvX, pTempItems( ItemList.Items[ i ] ).InvY, pTempItems( ItemList.Items[ i ] ).InvX + pTempItems( ItemList.Items[ i ] ).W, pTempItems( ItemList.Items[ i ] ).InvY + pTempItems( ItemList.Items[ i ] ).H ), DXBack, false, 120 );
              end;
            end;
          end;
           //end new code
        end
        else
        begin //In the ground slot so plot iconic image
          if assigned( pTempItems( ItemList.Items[ i ] ).DXSurfaceIcon ) then
            lpDDSBack.BltFast( pTempItems( ItemList.Items[ i ] ).InvX, pTempItems( ItemList.Items[ i ] ).InvY, pTempItems( ItemList.Items[ i ] ).DXSurfaceIcon, rect( 0, 0, GroundListWidth, GroundListHeight ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        end;
      end; //endif
    end;

    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    lpDDSBack.BltFast( 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;
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
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  Result := false;
  try
    CollisionHasNotOccured := True;
  //first get the rectangle desribing the area where this item will land on the grid
    R1.Left := X; //Integer((X - 14 - (pTempItems(ItemList.Items[CurrentSelectedItem]).W div 2)) div 18) * 18 + 20;
    R1.Right := R1.Left + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W;
    R1.Top := Y; //Integer((Y - 47 - (pTempItems(ItemList.Items[CurrentSelectedItem]).H div 2)) div 26) * 26 + 57;
    R1.Bottom := R1.Top + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H;
    for i := 0 to ItemList.Count - 1 do
    begin //check where we will land vs all other inv items for collision
      if i <> CurrentSelectedItem then
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
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  Result := -1;
  try
    SlotNumber := -1;
    j := 0;
  //first get the rectangle desribing the area where this item is
    R1.Left := Integer( X - ( pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W div 2 ) );
    R1.Right := R1.Left + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W;
    R1.Top := Integer( Y - ( pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H div 2 ) );
    R1.Bottom := R1.Top + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H;
    for i := 0 to 15 do
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
        if ( ( Abs( X - SlotCoord[ m ].cx ) + Abs( Y - SlotCoord[ m ].cy ) ) > ( Abs( X - SlotCoord[ SlotsHitIndex[ i ] ].cx ) + Abs( Y - SlotCoord[ SlotsHitIndex[ i ] ].cy ) ) ) then
          m := SlotsHitIndex[ i ]; //if the second slot is close assign its index to m
      end;
    //We watch for the shield and Weapons- if its a shield or weapon, we just chunk it on in there- special preference
      if WeaponFound and pTempItems( ItemList.Items[ CurrentSelectedItem ] ).PItem.CanEquip( TSlot( 11 ), Character ) then
      begin
        if TWeapon( pTempItems( ItemList.Items[ CurrentSelectedItem ] ).PItem ).TwoHanded and ( SlotCoord[ 12 ].Usedby <> -1 ) then
        begin
          SlotNumber := -1;
          ErrorCode := 11;
        end
        else
        begin
          SlotNumber := 11;
        end;
      end
      else if ShieldFound and pTempItems( ItemList.Items[ CurrentSelectedItem ] ).PItem.CanEquip( TSlot( 12 ), Character ) then
      begin
        SlotNumber := 12;
        if SlotCoord[ 11 ].UsedBy > -1 then
        begin //if weapon wielded
          if TWeapon( pTempItems( ItemList.Items[ SlotCoord[ 11 ].UsedBy ] ).PItem ).TwoHanded then
          begin //if its two handed
            SlotNumber := -1; //if he has 2 handed weapon, canw wear shield
            ErrorCode := 12;
          end;
        end;
      end
      else if pTempItems( ItemList.Items[ CurrentSelectedItem ] ).PItem.CanEquip( TSlot( m ), Character ) then
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
  Sentence : string;
  dl : integer;
  LoopCount : integer;
const
  FailName : string = 'TInventory.GetSlotText';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  Result := 'failure';
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
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    for t := slLeg1 to slMisc3 do
    begin
      if pTempItems( ItemList.Items[ Index ] ).PItem.CanEquip( t, Character ) and not Character.EquipmentLocked[ t ] then
      begin
        if ( t = slWeapon ) or ( t = slShield ) then
        begin //custom circle rect for weapon and shield
          DrawAlpha( lpDDSBack, Rect( SlotCoord[ Integer( t ) ].cx - 30, SlotCoord[ Integer( t ) ].cy - 20, SlotCoord[ Integer( t ) ].cx + 30, SlotCoord[ Integer( t ) ].cy + 40 ), Rect( 0, 0, 99, 99 ), DXCircle, True, 96 );
        end
        else
        begin //every other item
          DrawAlpha( lpDDSBack, SlotCoord[ Integer( t ) ].Rect, Rect( 0, 0, 99, 99 ), DXCircle, True, 96 );
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
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
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
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if ( CurrentSelectedItem <> -1 ) and CheckForGroundDrop then
    begin //player is dragging an item
      if DropAnItem( 20, 58 ) = true then
      begin
        pTempItems( ItemList.Items[ CurrentSelectedItem ] ).InvX := Tx;
        pTempItems( ItemList.Items[ CurrentSelectedItem ] ).InvY := Ty;
        pTempItems( ItemList.Items[ CurrentSelectedItem ] ).ItemType := 'Inventory';
        pTempItems( ItemList.Items[ CurrentSelectedItem ] ).CharacterHadThisOnHim := true;
        pTempItems( ItemList.Items[ CurrentSelectedItem ] ).BodySlot := -1;
      end
      else
      begin //failed- no room: drop it on ground
        pTempItems( ItemList.Items[ CurrentSelectedItem ] ).ItemType := 'Ground';
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
      pTempItems( ItemList.Items[ i ] ).PItem.InvX := ( pTempItems( ItemList.Items[ i ] ).InvX - 20 ) div 18;
      pTempItems( ItemList.Items[ i ] ).PItem.InvY := ( pTempItems( ItemList.Items[ i ] ).InvY - 57 ) div 26;
    //pTempItems(ItemList.Items[i]).pItem.BodySlot:=pTempItems(ItemList.Items[i]).BodySlot;
      pTempItems( ItemList.Items[ i ] ).PItem.Enabled := False; //this is only true if an item in on the ground
      if pTempItems( ItemList.Items[ i ] ).ItemType = 'Inventory' then
        Character.Inventory.Add( pTempItems( ItemList.Items[ i ] ).PItem )
      else if pTempItems( ItemList.Items[ i ] ).ItemType = 'Equipment' then
      begin
        if pTempItems( ItemList.Items[ i ] ).BodySlot > -1 then
        begin
          Character.Equipment[ TSlot( pTempItems( ItemList.Items[ i ] ).BodySlot ) ] := pTempItems( ItemList.Items[ i ] ).PItem;
        end;
      end
      else
      begin //its on the ground
      //put the item at the characters pos on the ground
        pTempItems( ItemList.Items[ i ] ).PItem.SetPos( Character.X, Character.Y, 0 );
        pTempItems( ItemList.Items[ i ] ).PItem.Enabled := True; //make it visible
        if pTempItems( ItemList.Items[ i ] ).CharacterHadThisOnHim and CheckForGroundDrop then
          pTempItems( ItemList.Items[ i ] ).PItem.Drop;
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
  prRect : PRect;
const
  FailName : string = 'TInventory.ContianCursor';
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
    begin //constrict to main inventory area
      prRect.bottom := 456;
      prRect.Right := 659;
    end
    else
    begin //resote to fullscreen
      prRect.bottom := 600;
      prRect.Right := 800;
    end;
    ClipCursor( prRect );
    Dispose( prRect );
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
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    //This routine checks for collisions with the weapon and Misc3 and the Feet-
    //It moves the weapon about accordingly to avoid unsightly collisions
    nx := Tx;
    ny := Ty;
    nw := pTempItems( ItemList.items[ ItemIndex ] ).W;
    nh := pTempItems( ItemList.items[ ItemIndex ] ).H;
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

procedure TInventory.Release;
var
  i : Integer;
const
  FailName : string = 'TInventory.release';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    CheckForGroundDrop := true;
    WriteTheInventoryData;
    ExText.Close;
{$IFNDEF DirectX}
    BMBack.Free;
{$ENDIF}
{$IFDEF DirectX}
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
      if Assigned( pTempItems( ItemList.Items[ i ] ).DXSurface ) then
        pTempItems( ItemList.Items[ i ] ).DXSurface := nil;
      if Assigned( pTempItems( ItemList.Items[ i ] ).DXSurfaceIcon ) then
        pTempItems( ItemList.Items[ i ] ).DXSurfaceIcon := nil;
      if Assigned( pTempItems( ItemList.Items[ i ] ).DXShadow ) then
        pTempItems( ItemList.Items[ i ] ).DXShadow := nil;

    end;
{$ENDIF}
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
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
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
    while j <= ( gWidth - pTempItems( ItemList.Items[ i ] ).pItem.InvW ) do
    begin //try to squeeze it in start upper left going to lower right
      k := 0;
      while k <= ( gHeight - pTempItems( ItemList.Items[ i ] ).pItem.InvH ) do
      begin
        XX := ( j * 18 ) + 20; //+pTempItems(ItemList.Items[i]).W div 2;
        YY := ( k * 26 ) + 57; //+pTempItems(ItemList.Items[i]).H div 2;
        if CollisionCheck( XX, YY ) then
        begin //if it mark the array
          for m := 0 to pTempItems( ItemList.Items[ i ] ).pItem.InvW - 1 do
          begin
            for n := 0 to pTempItems( ItemList.Items[ i ] ).pItem.InvH - 1 do
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
          DrawAlpha( lpDDSBack, rect( i * 18 + 21, j * 26 + 57, i * 18 + 21 + 18, j * 26 + 57 + 26 ), rect( 0, 0, 25, 25 ), DXBrown, False, 90 );
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
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  Result := false;
  try
    LastLowTotal := 9999; //initialize to insanely high number
    FoundASafePlaceToDrop := false;
   //upper left corner of floating bmp
    XX := X - pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W div 2;
    YY := Y - pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H div 2;

    for i := 0 to 11 do
    begin
      for j := 0 to 13 do
      begin
        if PlotArray[ i, j ] = 1 then
        begin
          if ( ( i * 18 + 20 + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).W ) < 238 ) and ( ( j * 26 + 57 + pTempItems( ItemList.Items[ CurrentSelectedItem ] ).H ) < 423 ) and ( CollisionCheck( i * 18 + 20, j * 26 + 57 ) ) then
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

end.
