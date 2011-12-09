unit NPCBehavior;
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
  Engine;
  
type
  CommandTargetRect = record
    rect : TRect; //collision rect for icon drops
    sr, dr : TRect; //source and destination rects for alpha blt - command targets only
    text : string;
    visible : boolean; //used with targets
    CommandType : integer; //Spell or Action or target
    CommandIndex : integer; //index to the appropriate list
    Target : TTargetType; //used with targets
  end;

  pSp = ^SpellRect;
  SpellRect = record
    SpellIndex : integer;
    dx, dy : integer;
    sr : Trect;
  end;

  pTA = ^TargetActionRect; //target and action icon rects, info
  TargetActionRect = record
    dx, dy : integer;
    sr : Trect;
    Text : string;
    Target : TTargetType; //used with targets
    AiPriority : TAIPriority; //used to assign to AI
    AiParameter : TAIParameter;
  end;

  TNPCBehavior = class( TDisplay )
  private
    //Timer
    FadeTimer : TTimer;
    FadeAlpha : integer;
    FadeIn : integer;
    CurrentSelectedItem : integer;
    CurrentSelectedType : integer;
    Tx, Ty : integer;
    //Bitmap stuff
    BMBack : TBitmap;
    DXBack : IDirectDrawSurface;
    DXBackToGame : IDirectDrawSurface;
    DXTarget : array[ 0..3 ] of IDirectDrawSurface;
    DXIcons : IDirectDrawSurface;
    DXDirty : IDirectDrawSurface;
    pTAIcon : pTA;
    ActList : TList; //locations and text for actions
    TargList : TList; //locationsand text for targets
    pSpellIcon : pSp;
    SpellList : TList; //list for location of spell icons
    CharSpellList : TStringList; //characters spelllist
    DestRect : array[ 0..7 ] of CommandTargetRect; //dest rects,info for icon drops
    procedure FadeTimerEvent( Sender : TObject );
    procedure LoadTargetActionIcons;
    procedure PlotMenu;
    procedure ContainCursor( action : integer );
    procedure LoadAIData;
    procedure WriteAIData;
  protected
    procedure MouseDown( Sender : TAniview; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
    procedure MouseMove( Sender : TAniview;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
  public
    CharAI : TPartyAI;
    DXSpellIcons : IDirectDrawSurface;
    constructor Create;
    destructor Destroy; override;
    procedure Paint; override;
    procedure Init; override;
    procedure Release; override;
  end;
implementation
uses
  AniDemo;
{ TNPCBehavior }

constructor TNPCBehavior.Create;
begin
  inherited;
end; //Create

destructor TNPCBehavior.Destroy;
begin
  inherited;
end; //Destroy

procedure TNPCBehavior.Init;
var
  InvisColor : integer;
  DXBorders : IDirectDrawSurface;
  BM : TBitmap;
begin
  if Loaded then
    Exit;
  inherited;
  MouseCursor.Cleanup;
  WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, ResWidth, ResHeight ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
  MouseCursor.PlotDirty := false;

  FadeTimer := TTimer.create( nil );
  FadeTimer.onTimer := FadeTimerEvent;
//  FadeTimer.TimerPriority:=tpNormal;
  FadeTimer.Interval := 100;
//  FadeTimer.resolution := 1;
  FadeTimer.enabled := false;

  CurrentSelectedItem := -1;
  CurrentSelectedType := -1;

  ActList := Tlist.create;
  TargList := Tlist.create;
  SpellList := TList.create;

  if CharAI.character <> nil then
    CharSpellList := CharAI.character.SpellList;

  pText.LoadFontGraphic( 'statistics' ); //load the statistics font graphic in
  BMBack := TBitmap.Create;
  BM := TBitmap.create;
  //transparent color
  InvisColor := $00FFFF00;

  //create the dirty surface
  DXDirty := DDGetSurface( lpDD, 35, 35, InvisColor, true );

  BMBack.LoadFromFile( InterfacePath + 'NPC.bmp' );
  DXBack := DDGetImage( lpDD, BMBack, InvisColor, False );

  BM.LoadFromFile( InterfacePath + 'NPCTarget1.bmp' );
  DXTarget[ 0 ] := DDGetImage( lpDD, BM, InvisColor, False );
  BM.LoadFromFile( InterfacePath + 'NPCTarget2.bmp' );
  DXTarget[ 1 ] := DDGetImage( lpDD, BM, InvisColor, False );
  BM.LoadFromFile( InterfacePath + 'NPCTarget3.bmp' );
  DXTarget[ 2 ] := DDGetImage( lpDD, BM, InvisColor, False );
  BM.LoadFromFile( InterfacePath + 'NPCTarget4.bmp' );
  DXTarget[ 3 ] := DDGetImage( lpDD, BM, InvisColor, False );

  BM.LoadFromFile( InterfacePath + 'NPCActionIcons.bmp' );
  DXIcons := DDGetImage( lpDD, BM, rgb( 255, 0, 255 ), False );

  BM.LoadFromFile( InterfacePath + 'NPCBackToGame.bmp' );
  DXBackToGame := DDGetImage( lpDD, BM, rgb( 255, 0, 255 ), False );

  BM.Free;

  LoadTargetActionIcons;
  WrapperBltFast( lpDDSBack, 0, 0, DXBack, Rect( 0, 0, BMBack.width, BMBack.Height ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
  LoadAIData;
  //Now for the Alpha'ed edges
  //BMBack.LoadFromFile(ExtractFilePath(Application.ExeName) + 'Dialog/DialogueBoxShadowMap.bmp');
  //DXBorders := DDGetImage(lpDD, BMBack, InvisColor, False);
  //DrawSub(lpDDSBack, Rect(190+XAdj, 130+YAdj, 190+XAdj + BMBack.Width, 130+YAdj+BMBack.Height), Rect(0, 0, BMBack.Width, BMBack.Height), DXBorders, True, 128);

  DXBorders := nil;
  BMBack.Free;

  pText.PlotText( CharAI.character.name, 9, 41, 240 );
  PlotMenu;
  lpDDSFront.Flip( nil, DDFLIP_WAIT );
  WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
  MouseCursor.PlotDirty := false;
end; //Init

procedure TNPCBehavior.LoadTargetActionIcons;
var
  i : integer;
  StartPos : integer;
  sPoint : TPoint;
  ProtectionCount : integer;
  CombatCount : integer;
  HealingCount : integer;
  OtherCount : integer;
begin
   //targets
  i := 0;
  StartPos := 0;
  case NPCList.count of
    1 : StartPos := 406 + 35 * 2;
    2 : StartPos := 406 + 35;
    3 : StartPos := 406 + 35;
    4 : StartPos := 406;
    5 : StartPos := 406;
  end;
  while i < NPCList.count do
  begin
    new( pTAIcon );
    pTAIcon.dx := StartPos + i * 35;
    pTAIcon.dy := 172;
    pTAIcon.sr := rect( 36, i * 36, 36 + 35, i * 36 + 35 );
    PTAIcon.text := 'Set target to: ' + TCharacter( NPCList.items[ i ] ).name;
    pTAIcon.Target := TTFriend;
    pTAIcon.AiParameter := paSpecificPartyMember;
    TargList.add( pTAIcon );
    i := i + 1;
  end;

  new( pTAIcon );
  pTAIcon.dx := StartPos + i * 35;
  pTAIcon.dy := 172;
  pTAIcon.sr := rect( 36, 5 * 36, 36 + 35, 5 * 36 + 35 );
  PTAIcon.text := 'Set target to: Any Party Member';
  pTAIcon.Target := TTFriend;
  pTAIcon.AiParameter := paAnyPartyMember;
  TargList.add( pTAIcon );
  i := i + 1;

  new( pTAIcon );
  pTAIcon.dx := StartPos + i * 35;
  pTAIcon.dy := 172;
  pTAIcon.sr := rect( 36, 6 * 36, 36 + 35, 6 * 36 + 35 );
  PTAIcon.text := 'Set target to: Any Ally';
  pTAIcon.Target := TTFriend;
  pTAIcon.AiParameter := paAnyAlly;
  TargList.add( pTAIcon );

  for i := 0 to 5 do
  begin
    new( pTAIcon );
    pTAIcon.dx := 390 + i * 35;
    pTAIcon.dy := 202;
    pTAIcon.sr := rect( 72, i * 36, 72 + 35, i * 36 + 35 );
    case i of
      0 : PTAIcon.text := 'Set target to: ' + TCharacter( CharAI.Character ).name + '''s target';
      1 : PTAIcon.text := 'Set target to: ' + TCharacter( CharAI.Character ).name + '''s attacker';
      2 : PTAIcon.text := 'Set target to: ' + TCharacter( CharAI.Character ).name + '''s nearest enemy';
      3 : PTAIcon.text := 'Set target to: The strongest enemy';
      4 : PTAIcon.text := 'Set target to: The weakest enemy';
      5 : PTAIcon.text := 'Set target to: The most magical enemy';
    end;
    case i of
      0 : pTAIcon.AiParameter := paPlayerTarget;
      1 : pTAIcon.AiParameter := paAttacker;
      2 : pTAIcon.AiParameter := paClosestEnemy;
      3 : pTAIcon.AiParameter := paStrongestEnemy;
      4 : pTAIcon.AiParameter := paWeakestEnemy;
      5 : pTAIcon.AiParameter := paMostMagicalEnemy;
    end;
    pTAIcon.Target := TTEnemy;
    TargList.add( pTAIcon );
  end;

   //Actions
  new( pTAIcon );
  pTAIcon.dx := 491;
  pTAIcon.dy := 51;
  pTAIcon.sr := rect( 0, 0, 0 + 35, 0 + 35 );
  PTAIcon.text := 'Attack';
  pTAIcon.Target := TTEnemy;
  pTAIcon.AiPriority := prAttack;
  ActList.add( pTAIcon );

  new( pTAIcon );
  pTAIcon.dx := 531;
  pTAIcon.dy := 51;
  pTAIcon.sr := rect( 0, 36, 0 + 35, 36 + 35 );
  PTAIcon.text := 'Guard';
  pTAIcon.Target := TTFriend;
  pTAIcon.AiPriority := prGuard;
  ActList.add( pTAIcon );

  new( pTAIcon );
  pTAIcon.dx := 571;
  pTAIcon.dy := 51;
  pTAIcon.sr := rect( 0, 72, 0 + 35, 72 + 35 );
  PTAIcon.text := 'Follow close behind';
  pTAIcon.Target := TTFriend;
  pTAIcon.AiPriority := prFollowClose;
  ActList.add( pTAIcon );

  new( pTAIcon );
  pTAIcon.dx := 491;
  pTAIcon.dy := 91;
  pTAIcon.sr := rect( 0, 144, 0 + 35, 144 + 35 );
  PTAIcon.text := 'Hide in Shadows';
  pTAIcon.AiPriority := prHide;
  pTAIcon.Target := TTNone;
  ActList.add( pTAIcon );

  new( pTAIcon );
  pTAIcon.dx := 531;
  pTAIcon.dy := 91;
  pTAIcon.sr := rect( 0, 180, 0 + 35, 180 + 35 );
  PTAIcon.text := 'Flee';
  pTAIcon.AiPriority := prFlee;
  pTAIcon.Target := TTNone;
  ActList.add( pTAIcon );

  new( pTAIcon );
  pTAIcon.dx := 571;
  pTAIcon.dy := 91;
  pTAIcon.sr := rect( 0, 108, 0 + 35, 108 + 35 );
  PTAIcon.text := 'Follow far behind';
  pTAIcon.Target := TTFriend;
  pTAIcon.AiPriority := prFollowFar;
  ActList.add( pTAIcon );

   //Spells
  ProtectionCount := 0;
  CombatCount := 0;
  HealingCount := 0;
  OtherCount := 0;
  for i := 0 to CharSpellList.count - 1 do
  begin
    new( pSpellIcon );
    sPoint := TSpell( CharSpellList.objects[ i ] ).GetIconXY( CharAI.character );
    pSpellIcon.sr := rect( sPoint.x, sPoint.y, sPoint.x + 32, sPoint.y + 32 );
    pSpellIcon.SpellIndex := i;
    if TSpell( CharSpellList.objects[ i ] ).CastingType = ctProtection then
    begin
      inc( ProtectionCount );
      case ProtectionCount of
        1 :
          begin
            pSpellIcon.dx := 58;
            pSpellIcon.dy := 267;
          end;
        2 :
          begin
            pSpellIcon.dx := 24;
            pSpellIcon.dy := 267;
          end;
        3 :
          begin
            pSpellIcon.dx := 92;
            pSpellIcon.dy := 267;
          end;
        4 :
          begin
            pSpellIcon.dx := 58;
            pSpellIcon.dy := 301;
          end;
        5 :
          begin
            pSpellIcon.dx := 24;
            pSpellIcon.dy := 301;
          end;
        6 :
          begin
            pSpellIcon.dx := 92;
            pSpellIcon.dy := 301;
          end;
        7 :
          begin
            pSpellIcon.dx := 58;
            pSpellIcon.dy := 335;
          end;
        8 :
          begin
            pSpellIcon.dx := 24;
            pSpellIcon.dy := 335;
          end;
        9 :
          begin
            pSpellIcon.dx := 92;
            pSpellIcon.dy := 335;
          end;
      end; //end case
    end
    else if TSpell( CharSpellList.objects[ i ] ).CastingType = ctCombat then
    begin
      inc( CombatCount );
      case CombatCount of
        1 :
          begin
            pSpellIcon.dx := 187;
            pSpellIcon.dy := 257;
          end;
        2 :
          begin
            pSpellIcon.dx := 187 + 34;
            pSpellIcon.dy := 257;
          end;
        3 :
          begin //row 2 center
            pSpellIcon.dx := 204;
            pSpellIcon.dy := 283;
          end;
        4 :
          begin
            pSpellIcon.dx := 170;
            pSpellIcon.dy := 283;
          end;
        5 :
          begin
            pSpellIcon.dx := 204 + 34;
            pSpellIcon.dy := 283;
          end;
        6 :
          begin
            pSpellIcon.dx := 187;
            pSpellIcon.dy := 309;
          end;
        7 :
          begin
            pSpellIcon.dx := 187 + 34;
            pSpellIcon.dy := 309;
          end;
        8 :
          begin
            pSpellIcon.dx := 187 - 34;
            pSpellIcon.dy := 257;
          end;
        9 :
          begin
            pSpellIcon.dx := 187 + 34 + 34;
            pSpellIcon.dy := 257;
          end;
        10 :
          begin
            pSpellIcon.dx := 187 - 34;
            pSpellIcon.dy := 309;
          end;
        11 :
          begin
            pSpellIcon.dx := 187 + 34 + 34;
            pSpellIcon.dy := 309;
          end;
        12 :
          begin
            pSpellIcon.dx := 170 - 34;
            pSpellIcon.dy := 283;
          end;
        13 :
          begin
            pSpellIcon.dx := 204 + 34 + 34;
            pSpellIcon.dy := 283;
          end;
        14 :
          begin //4th row- shouldnt be needed
            pSpellIcon.dx := 170 + 34;
            pSpellIcon.dy := 335;
          end;
        15 :
          begin
            pSpellIcon.dx := 170;
            pSpellIcon.dy := 335;
          end;
        16 :
          begin
            pSpellIcon.dx := 170 + 34 + 34;
            pSpellIcon.dy := 335;
          end;
        17 :
          begin
            pSpellIcon.dx := 170 - 34;
            pSpellIcon.dy := 335;
          end;
        18 :
          begin
            pSpellIcon.dx := 170 + 34 + 34 + 34;
            pSpellIcon.dy := 335;
          end;

      end; //end case

    end
    else if TSpell( CharSpellList.objects[ i ] ).CastingType = ctHealing then
    begin
      inc( HealingCount );
      case HealingCount of
        1 :
          begin
            pSpellIcon.dx := 351;
            pSpellIcon.dy := 267;
          end;
        2 :
          begin
            pSpellIcon.dx := 351 + 34;
            pSpellIcon.dy := 267;
          end;
        3 :
          begin
            pSpellIcon.dx := 351 - 34;
            pSpellIcon.dy := 267;
          end;
        4 :
          begin
            pSpellIcon.dx := 351 + 34 + 34;
            pSpellIcon.dy := 267;
          end;
        5 :
          begin
            pSpellIcon.dx := 351;
            pSpellIcon.dy := 301;
          end;
        6 :
          begin
            pSpellIcon.dx := 351 + 34;
            pSpellIcon.dy := 301;
          end;
        7 :
          begin
            pSpellIcon.dx := 351 - 34;
            pSpellIcon.dy := 301;
          end;
        8 :
          begin
            pSpellIcon.dx := 351 + 34 + 34;
            pSpellIcon.dy := 301;
          end;
        9 :
          begin //third row, probably not needed
            pSpellIcon.dx := 351;
            pSpellIcon.dy := 335;
          end;
        10 :
          begin
            pSpellIcon.dx := 351 + 34;
            pSpellIcon.dy := 335;
          end;
        11 :
          begin
            pSpellIcon.dx := 351 - 34;
            pSpellIcon.dy := 335;
          end;
        12 :
          begin
            pSpellIcon.dx := 351 + 34 + 34;
            pSpellIcon.dy := 335;
          end;
      end; //end case
    end
    else
    begin
      inc( OtherCount );
      case OtherCount of
        1 :
          begin
            pSpellIcon.dx := 531;
            pSpellIcon.dy := 267;
          end;
        2 :
          begin
            pSpellIcon.dx := 531 + 34;
            pSpellIcon.dy := 267;
          end;
        3 :
          begin
            pSpellIcon.dx := 531 - 34;
            pSpellIcon.dy := 267;
          end;
        4 :
          begin
            pSpellIcon.dx := 531 + 34 + 34;
            pSpellIcon.dy := 267;
          end;
        5 :
          begin
            pSpellIcon.dx := 531 - 34 - 34;
            pSpellIcon.dy := 267;
          end;
        6 :
          begin
            pSpellIcon.dx := 531;
            pSpellIcon.dy := 301;
          end;
        7 :
          begin
            pSpellIcon.dx := 531 + 34;
            pSpellIcon.dy := 301;
          end;
        8 :
          begin
            pSpellIcon.dx := 531 - 34;
            pSpellIcon.dy := 301;
          end;
        9 :
          begin
            pSpellIcon.dx := 531 + 34 + 34;
            pSpellIcon.dy := 301;
          end;
        10 :
          begin
            pSpellIcon.dx := 531 - 34 - 34;
            pSpellIcon.dy := 301;
          end;
        11 :
          begin //third row, probably not needed
            pSpellIcon.dx := 531;
            pSpellIcon.dy := 335;
          end;
        12 :
          begin
            pSpellIcon.dx := 531 + 34;
            pSpellIcon.dy := 335;
          end;
        13 :
          begin
            pSpellIcon.dx := 531 - 34;
            pSpellIcon.dy := 335;
          end;
        14 :
          begin
            pSpellIcon.dx := 531 + 34 + 34;
            pSpellIcon.dy := 335;
          end;
        15 :
          begin
            pSpellIcon.dx := 531 - 34 - 34;
            pSpellIcon.dy := 335;
          end;
      end; //endcase
    end; //endif

    SpellList.add( pSpellIcon );
  end; //end for i

   //Now plot them
  for i := 0 to TargList.count - 1 do
  begin
    WrapperBltFast( DXBack, pTA( TargList.items[ i ] ).dx, pTA( TargList.items[ i ] ).dy, DXIcons, pTA( TargList.items[ i ] ).sr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
  end;
  for i := 0 to ActList.count - 1 do
  begin
    WrapperBltFast( DXBack, pTA( ActList.items[ i ] ).dx, pTA( ActList.items[ i ] ).dy, DXIcons, pTA( ActList.items[ i ] ).sr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
  end;
  for i := 0 to SpellList.count - 1 do
  begin
    WrapperBltFast( DXBack, pSP( SpellList.items[ i ] ).dx, pSP( SpellList.items[ i ] ).dy, DXSpellIcons, pSP( SpellList.items[ i ] ).sr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
  end;

   //now we load the destination list
   //First the Commands
  DestRect[ 0 ].rect := rect( 178, 101, 219, 142 );
  DestRect[ 0 ].text := 'This is this character''s first Command Priority.  The character will attempt to perform this action before any other.  ' +
    'If the character cannot perform this action, he will attempt Command Priority 2.';
  DestRect[ 1 ].rect := rect( 230, 101, 271, 142 );
  DestRect[ 1 ].text := 'This is this character''s second Command Priority.  The character will attempt to perform this action if unable to execute Command 1.  ' +
    'If the character cannot perform this action, he will attempt Command Priority 3.';
  DestRect[ 2 ].rect := rect( 282, 101, 323, 142 );
  DestRect[ 2 ].text := 'This is this character''s third Command Priority.  The character will attempt to perform this action if unable to execute Commands 1 and 2.  ' +
    'If the character cannot perform this action, he will attempt Command Priority 4.';
  DestRect[ 3 ].rect := rect( 334, 101, 375, 142 );
  DestRect[ 3 ].text := 'This is this character''s fourth Command Priority.  The character will attempt to perform this action if unable to execute Commands 1, 2 and 3.';

   //Now the targets
  DestRect[ 4 ].rect := rect( 115, 155, 155, 196 );
  DestRect[ 4 ].sr := rect( 0, 0, 83, 98 );
  DestRect[ 4 ].dr := rect( 90, 133, 90 + 83, 133 + 98 );
  DestRect[ 4 ].text := 'This is the target for the action or spell loaded into Command Priority 1.';

  DestRect[ 5 ].rect := rect( 261, 39, 301, 81 );
  DestRect[ 5 ].sr := rect( 0, 0, 85, 75 );
  DestRect[ 5 ].dr := rect( 237, 18, 237 + 85, 18 + 75 );
  DestRect[ 5 ].text := 'This is the target for the action or spell loaded into Command Priority 2.';
  DestRect[ 6 ].rect := rect( 299, 170, 338, 211 );
  DestRect[ 6 ].sr := rect( 0, 0, 86, 99 );
  DestRect[ 6 ].dr := rect( 278, 148, 278 + 86, 148 + 99 );
  DestRect[ 6 ].text := 'This is the target for the action or spell loaded into Command Priority 3.';
  DestRect[ 7 ].rect := rect( 422, 74, 462, 115 );
  DestRect[ 7 ].sr := rect( 0, 0, 99, 95 );
  DestRect[ 7 ].dr := rect( 385, 48, 385 + 99, 48 + 95 );
  DestRect[ 7 ].text := 'This is the target for the action or spell loaded into Command Priority 4.';

   //Clear Index
  for i := 0 to 7 do
  begin
    DestRect[ i ].CommandIndex := -1;
    DestRect[ i ].visible := false;
   //   DrawAlpha(DXBack,DestRect[i].dr,DestRect[i].sr,DXTarget[i-4],True,100);
  end;

end; //LoadTargetActionIcons

procedure TNPCBehavior.PlotMenu;
begin


end; //PlotMenu



procedure TNPCBehavior.MouseDown( Sender : TAniview; Button : TMouseButton; Shift : TShiftState; X, Y, GridX, GridY : integer );
var
  i : integer;
  tempx, tempy, ti : integer;
  rRect : TRect;
begin
  if CurrentSelectedItem = -1 then
  begin
     //Check for Actions Icon click
    if PtInRect( rect( 490, 50, 607, 127 ), point( x, y ) ) then
    begin
      i := 0;
      while i < ActList.count do
      begin
        if PtInRect( rect( pTA( ActList.items[ i ] ).dx, pTA( ActList.items[ i ] ).dy, pTA( ActList.items[ i ] ).dx + 35, pTA( ActList.items[ i ] ).dy + 35 ), point( x, y ) ) then
        begin
          CurrentSelectedItem := i;
          CurrentSelectedType := 1;
          i := 999;
          Tx := X - 17;
          Ty := Y - 17;
              //clear text
          WrapperBltFast( lpDDSBack, 24, 365, DXBack, Rect( 24, 365, 550, 469 ), DDBLTFAST_WAIT );
          pText.PlotText( pTA( ActList.items[ CurrentSelectedItem ] ).text, 25, 365, 240 );
          pText.PlotTextBlock( 'You may drag this Action Icon to any of the four Command Priority slots.  After selecting an Action, you will be prompted for a Target, if necessary.', 25, 550, 395, 240 );
          WrapperBltFast( DXDirty, 0, 0, lpDDSBack, rect( Tx, Ty, Tx + 35, Ty + 35 ), DDBLTFAST_WAIT ); //save dirty
          WrapperBltFast( lpDDSBack, Tx, Ty, DXIcons, pTA( ActList.items[ CurrentSelectedItem ] ).sr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //PlotIcon
          ContainCursor( 1 );
        end;
        inc( i );
      end; //end while
    end //endif region check for Actions
    else if PtInRect( rect( 391, 172, 600, 237 ), point( x, y ) ) then
    begin //target Icons
      tempx := 0;
      tempy := 0;
      ti := -1;
      for i := 0 to TargList.count - 1 do
      begin
        if PtInRect( rect( pTA( TargList.items[ i ] ).dx, pTA( TargList.items[ i ] ).dy, pTA( TargList.items[ i ] ).dx + 35, pTA( TargList.items[ i ] ).dy + 35 ), point( x, y ) ) then
        begin
          if ti <> -1 then
          begin //we already found one collision - impossible to have more than two
            if ( abs( x - tempx ) + abs( y - tempy ) ) > ( abs( x - pTA( TargList.items[ i ] ).dx + 17 ) + abs( y - pTA( TargList.items[ i ] ).dy + 17 ) ) then
            begin
              ti := i; //This icon's center is closer to the pointer than the other collison
            end;
          end
          else
          begin //this is the first collision we have detected between pointer and an icon
            tempx := pTA( TargList.items[ i ] ).dx + 17;
            tempy := pTA( TargList.items[ i ] ).dy + 17;
            ti := i;
          end; //endif ti
        end; //endif ptInrect
      end; //end for
      if ti <> -1 then
      begin //if pointer is over an icon
        CurrentSelectedItem := ti;
        CurrentSelectedType := 2; //Target icon
        Tx := X - 17;
        Ty := Y - 17;
              //clear text
        WrapperBltFast( lpDDSBack, 24, 365, DXBack, Rect( 24, 365, 550, 469 ), DDBLTFAST_WAIT );
        pText.PlotText( pTA( TargList.items[ CurrentSelectedItem ] ).text, 25, 365, 240 );
        pText.PlotTextBlock( 'You may drag this Target Icon to any available Target slot.  You may not choose a target until you have dragged an Action Icon or a Spell Icon to a Command Priority slot.', 25, 550, 395, 240 );
        WrapperBltFast( DXDirty, 0, 0, lpDDSBack, rect( Tx, Ty, Tx + 35, Ty + 35 ), DDBLTFAST_WAIT ); //save dirty
        WrapperBltFast( lpDDSBack, Tx, Ty, DXIcons, pTA( TargList.items[ CurrentSelectedItem ] ).sr, DDBLTFAST_WAIT ); //PlotIcon
        ContainCursor( 1 );
      end;
    end //end target
    else if PtInRect( rect( 19, 262, 661, 382 ), point( x, y ) ) then
    begin //Spell Icons
      tempx := 0;
      tempy := 0;
      ti := -1;
      for i := 0 to SpellList.count - 1 do
      begin
        if PtInRect( rect( pSP( SpellList.items[ i ] ).dx, pSP( SpellList.items[ i ] ).dy, pSP( SpellList.items[ i ] ).dx + 32, pSP( SpellList.items[ i ] ).dy + 32 ), point( x, y ) ) then
        begin
          if ti <> -1 then
          begin //we already found one collision - impossible to have more than two
            if ( abs( x - tempx ) + abs( y - tempy ) ) > ( abs( x - pSP( SpellList.items[ i ] ).dx + 16 ) + abs( y - pSP( SpellList.items[ i ] ).dy + 16 ) ) then
            begin
              ti := i; //This icon's center is closer to the pointer than the other collison
            end;
          end
          else
          begin //this is the first collision we have detected between pointer and an icon
            tempx := pSP( SpellList.items[ i ] ).dx + 16;
            tempy := pSP( SpellList.items[ i ] ).dy + 16;
            ti := i;
          end; //endif ti
        end; //endif ptInrect
      end; //end for
      if ti <> -1 then
      begin //if pointer is over an icon
        CurrentSelectedItem := ti;
        CurrentSelectedType := 3; //spell icon
        Tx := X - 17;
        Ty := Y - 17;
              //clear text
        WrapperBltFast( lpDDSBack, 24, 365, DXBack, Rect( 24, 365, 550, 469 ), DDBLTFAST_WAIT );
        pText.PlotText( CharSpellList[ pSP( SpellList.items[ CurrentSelectedItem ] ).SpellIndex ], 25, 365, 240 );
        pText.PlotTextBlock( 'You may drag this Spell Icon to any of the four Command Priority slots.  After selecting a Spell, you will be prompted for a Target, if necessary.', 25, 550, 395, 240 );
        WrapperBltFast( DXDirty, 0, 0, lpDDSBack, rect( Tx, Ty, Tx + 35, Ty + 35 ), DDBLTFAST_WAIT ); //save dirty
        WrapperBltFast( lpDDSBack, Tx, Ty, DXSpellIcons, pSP( SpellList.items[ CurrentSelectedItem ] ).sr, DDBLTFAST_WAIT ); //PlotIcon
        ContainCursor( 1 );
      end;
    end //end spell region check
    else if PtInRect( rect( 576, 415, 576 + 74, 415 + 45 ), point( x, y ) ) then
    begin //back button hit
      close;
    end
    else
    begin //check for Command+target areas to clear boxes
      for i := 0 to 7 do
      begin
        if PtInRect( DestRect[ i ].rect, point( x, y ) ) then
        begin //if a click in DestRect clear it
          if i < 4 then
          begin //command box
            WrapperBltFast( lpDDSBack, DestRect[ i ].rect.left, DestRect[ i ].rect.top, DXBack, DestRect[ i ].rect, DDBLTFAST_WAIT ); //clear box
                    //WrapperBltFast( lpDDSBack, 24,365,DXBack,Rect(24,365,550,469),DDBLTFAST_WAIT); //clear text
            DestRect[ i ].CommandIndex := -1;
            DestRect[ i ].CommandType := -1;
                    //if DestRect[i+4].CommandIndex <> -1 then begin //if there is a target, then clear this
            WrapperBltFast( lpDDSBack, DestRect[ i + 4 ].dr.left, DestRect[ i + 4 ].dr.top, DXBack, DestRect[ i + 4 ].dr, DDBLTFAST_WAIT ); //clear box
                       //WrapperBltFast( lpDDSBack, 24,365,DXBack,Rect(24,365,550,469),DDBLTFAST_WAIT); //clear text
            DestRect[ i + 4 ].CommandIndex := -1;
            DestRect[ i + 4 ].CommandType := -1;
            DestRect[ i + 4 ].visible := false;
                    //end;
          end
          else
          begin //targets
            if DestRect[ i ].CommandIndex <> -1 then
            begin //if there is a target, then clear this
              WrapperBltFast( lpDDSBack, DestRect[ i ].dr.left, DestRect[ i ].dr.top, DXBack, DestRect[ i ].dr, DDBLTFAST_WAIT ); //clear box
              WrapperBltFast( lpDDSBack, DestRect[ i ].dr.left, DestRect[ i ].dr.top, DXTarget[ i - 4 ], DestRect[ i ].sr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clear box
                       //WrapperBltFast( lpDDSBack, 24,365,DXBack,Rect(24,365,550,469),DDBLTFAST_WAIT); //clear text
              DestRect[ i ].CommandIndex := -1;
              DestRect[ i ].CommandType := -1;
            end;
          end; //end i < 4
        end; //endif
      end; //end for
    end //end check Command and target boxes to clear
  end
  else
  begin //try to drop icon  Command Priority boxes and Target boxes
    if PtInRect( rect( 92, 40, 465, 211 ), point( x, y ) ) then
    begin
      tempx := 0;
      tempy := 0;
      ti := -1;
      for i := 0 to 7 do
      begin
        if IntersectRect( rRect, DestRect[ i ].rect, rect( x - 17, y - 17, x + 17, y + 17 ) ) then
        begin //PtInRect(DestRect[i].rect,point(x,y)) then begin
          if ti <> -1 then
          begin //we already found one collision - impossible to have more than two
            if ( abs( x - tempx ) + abs( y - tempy ) ) > ( abs( x - destRect[ i ].rect.left + 16 ) + abs( y - destRect[ i ].rect.top + 16 ) ) then
            begin
              ti := i; //This icon's center is closer to the pointer than the other collison
            end;
          end
          else
          begin //this is the first collision we have detected between pointer and an icon
            tempx := DestRect[ i ].rect.left + 16;
            tempy := DestRect[ i ].rect.top + 16;
            ti := i;
          end; //endif ti
        end; //endif ptInrect
      end; //end for
      if ti <> -1 then
      begin //if we're dropping over a command/target slot
        if ti < 4 then
        begin //if its a command slot
          if CurrentSelectedType <> 2 then
          begin
            WrapperBltFast( lpDDSBack, Tx, Ty, DXDirty, rect( 0, 0, 35, 35 ), DDBLTFAST_WAIT ); //clean up dirty
            WrapperBltFast( lpDDSBack, DestRect[ ti ].rect.left, DestRect[ ti ].rect.top, DXBack, DestRect[ ti ].rect, DDBLTFAST_WAIT ); //clear box
            WrapperBltFast( lpDDSBack, 24, 365, DXBack, Rect( 24, 365, 550, 469 ), DDBLTFAST_WAIT ); //clear text
            DestRect[ ti ].CommandIndex := CurrentSelectedItem;
            DestRect[ ti ].CommandType := CurrentSelectedType;
            if CurrentSelectedType = 1 then
              DestRect[ ti ].Target := pTA( ActList.items[ CurrentSelectedItem ] ).Target
            else
              DestRect[ ti ].Target := TSpell( CharSpellList.objects[ pSP( SpellList.items[ CurrentSelectedItem ] ).SpellIndex ] ).TargetType;
                     //DestRect[ti].Target:=TSpell(SpellList.items[CurrentSelectedItem]).TargetType;

                  //if this Action/Spell has a target Fade in the Target Box
            if DestRect[ ti ].Target <> TTNone then
            begin
              if DestRect[ ti + 4 ].CommandIndex <> -1 then
              begin
                if DestRect[ ti ].Target <> DestRect[ ti + 4 ].Target then
                begin //if old target not = erase
                  WrapperBltFast( lpDDSBack, DestRect[ ti + 4 ].dr.left, DestRect[ ti + 4 ].dr.top, DXBack, DestRect[ ti + 4 ].dr, DDBLTFAST_WAIT ); //clear box
                  WrapperBltFast( lpDDSBack, DestRect[ ti + 4 ].dr.left, DestRect[ ti + 4 ].dr.top, DXTarget[ ti ], DestRect[ ti + 4 ].sr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //clear box
                  DestRect[ ti + 4 ].CommandIndex := -1;
                  DestRect[ ti + 4 ].CommandType := -1;
                end;
              end;
              if DestRect[ ti + 4 ].visible = false then
              begin
                FadeIn := ti;
                FadeAlpha := 0;
                FadeTimer.enabled := true;
                DestRect[ ti + 4 ].visible := true;
              end;
            end
            else
            begin
              WrapperBltFast( lpDDSBack, DestRect[ ti + 4 ].dr.left, DestRect[ ti + 4 ].dr.top, DXBack, DestRect[ ti + 4 ].dr, DDBLTFAST_WAIT ); //clear box
              DestRect[ ti + 4 ].CommandIndex := -1;
              DestRect[ ti + 4 ].CommandType := -1;
              DestRect[ ti + 4 ].visible := false;
            end;

            if CurrentSelectedType = 1 then
            begin //if Action
              Tx := DestRect[ ti ].rect.left + ( ( DestRect[ ti ].rect.right - DestRect[ ti ].rect.left ) div 2 ) - 16;
              Ty := DestRect[ ti ].rect.top + ( ( DestRect[ ti ].rect.bottom - DestRect[ ti ].rect.top ) div 2 ) - 17;
              WrapperBltFast( lpDDSBack, Tx, Ty, DXIcons, pTA( ActList.items[ CurrentSelectedItem ] ).sr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //PlotIcon
            end
            else if CurrentSelectedType = 3 then
            begin //if spell
              Tx := DestRect[ ti ].rect.left + ( ( DestRect[ ti ].rect.right - DestRect[ ti ].rect.left ) div 2 ) - 15;
              Ty := DestRect[ ti ].rect.top + ( ( DestRect[ ti ].rect.bottom - DestRect[ ti ].rect.top ) div 2 ) - 15;
              WrapperBltFast( lpDDSBack, Tx, Ty, DXSpellIcons, pSP( SpellList.items[ CurrentSelectedItem ] ).sr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //PlotIcon
            end;
            CurrentSelectedItem := -1;
            ContainCursor( 0 );
          end
          else
          begin //tried to drop target icon in command slot
            WrapperBltFast( lpDDSBack, 24, 365, DXBack, Rect( 24, 365, 550, 469 ), DDBLTFAST_WAIT ); //clear text
            pText.PlotTextBlock( 'You can only place Action Icons or Spell Icons in this slot.', 25, 550, 395, 240 );
          end; //endif CurrentSelectedType <> 2
        end
        else
        begin //tried to drop over target box
          if DestRect[ ti ].visible then
          begin //(DestRect[ti-4].CommandIndex > -1) and (DestRect[ti-4].Target <> TTNone) then begin//if there is an icon in the command, then this Target box is visible so allow drop
            if CurrentSelectedType = 2 then
            begin //if its a target icon
                     //if this is the correct target type
              if DestRect[ ti - 4 ].Target = pTA( TargList.items[ CurrentSelectedItem ] ).Target then
              begin
                WrapperBltFast( lpDDSBack, Tx, Ty, DXDirty, rect( 0, 0, 35, 35 ), DDBLTFAST_WAIT ); //clean up dirty
                WrapperBltFast( lpDDSBack, 24, 365, DXBack, Rect( 24, 365, 550, 469 ), DDBLTFAST_WAIT ); //clear text
                DestRect[ ti ].CommandIndex := CurrentSelectedItem;
                DestRect[ ti ].CommandType := CurrentSelectedType;
                DestRect[ ti ].Target := pTA( TargList.items[ CurrentSelectedItem ] ).Target;
                Tx := DestRect[ ti ].rect.left + ( ( DestRect[ ti ].rect.right - DestRect[ ti ].rect.left ) div 2 ) - 16;
                Ty := DestRect[ ti ].rect.top + ( ( DestRect[ ti ].rect.bottom - DestRect[ ti ].rect.top ) div 2 ) - 17;
                WrapperBltFast( lpDDSBack, Tx, Ty, DXIcons, pTA( TargList.items[ CurrentSelectedItem ] ).sr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //PlotIcon
                CurrentSelectedItem := -1;
                ContainCursor( 0 );
              end
              else
              begin //diff target type
                WrapperBltFast( lpDDSBack, 24, 365, DXBack, Rect( 24, 365, 550, 469 ), DDBLTFAST_WAIT ); //clear text
                if DestRect[ ti - 4 ].Target = TTFriend then
                  pText.PlotTextBlock( 'You can only place party member Target Icons in this slot.  The party member icons are the round numbered icons on the right side of the screen.', 25, 550, 395, 240 )
                else
                  pText.PlotTextBlock( 'You can only place enemy Target Icons in this slot.  ', 25, 550, 395, 240 );
              end; //end if same targettype
            end
            else
            begin //otherwise tell them they cant drop it here
              WrapperBltFast( lpDDSBack, 24, 365, DXBack, Rect( 24, 365, 550, 469 ), DDBLTFAST_WAIT ); //clear text
              pText.PlotTextBlock( 'You can only place Target Icons in this slot.', 25, 550, 395, 240 );
            end;
          end
          else
          begin //no icon in command box, so treat this as dropped icon
            WrapperBltFast( lpDDSBack, 24, 365, DXBack, Rect( 24, 365, 550, 469 ), DDBLTFAST_WAIT ); //clear text
            WrapperBltFast( lpDDSBack, Tx, Ty, DXDirty, rect( 0, 0, 35, 35 ), DDBLTFAST_WAIT ); //clean up dirty
            CurrentSelectedItem := -1;
            ContainCursor( 0 );
          end; //enid commandindex check
        end; //endif ti < 4
      end
      else
      begin //just drop the icon
        WrapperBltFast( lpDDSBack, Tx, Ty, DXDirty, rect( 0, 0, 35, 35 ), DDBLTFAST_WAIT ); //clean up dirty
        CurrentSelectedItem := -1;
        ContainCursor( 0 );
      end; //endif ti
    end
    else
    begin //drop it
      WrapperBltFast( lpDDSBack, Tx, Ty, DXDirty, rect( 0, 0, 35, 35 ), DDBLTFAST_WAIT ); //clean up dirty
      CurrentSelectedItem := -1;
      ContainCursor( 0 );
    end; //endif CommandPriority box region check

  end; //endif CurrentSelectedItem

end; //MouseDown

procedure TNPCBehavior.MouseMove( Sender : TAniview; Shift : TShiftState; X, Y, GridX, GridY : integer );
var
  i : integer;
  tempx, tempy, ti : integer;
begin

  if CurrentSelectedItem = -1 then
  begin //no icon begin dragged
      //clear text
    WrapperBltFast( lpDDSBack, 24, 365, DXBack, Rect( 24, 365, 550, 499 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
      //Clear backbutton highlight
    WrapperBltFast( lpDDSBack, 576, 415, DXBack, rect( 576, 415, 576 + 74, 415 + 45 ), DDBLTFAST_WAIT );

      //plot rollover if any for Actions
    if PtInRect( rect( 490, 50, 607, 127 ), point( x, y ) ) then
    begin
      for i := 0 to ActList.count - 1 do
      begin
        if PtInRect( rect( pTA( ActList.items[ i ] ).dx, pTA( ActList.items[ i ] ).dy, pTA( ActList.items[ i ] ).dx + 35, pTA( ActList.items[ i ] ).dy + 35 ), point( x, y ) ) then
        begin
          pText.PlotText( pTA( ActList.items[ i ] ).text, 25, 365, 240 );
          pText.PlotTextBlock( 'You may drag this Action Icon to any of the four Command Priority slots.  After selecting an Action, you will be prompted for a Target, if necessary.', 25, 550, 395, 240 );
        end;
      end;
    end; //endif region check for Actions

      //plot rollover if any for targets
    if PtInRect( rect( 391, 172, 600, 237 ), point( x, y ) ) then
    begin
      tempx := 0;
      tempy := 0;
      ti := -1;
      for i := 0 to TargList.count - 1 do
      begin
        if PtInRect( rect( pTA( TargList.items[ i ] ).dx, pTA( TargList.items[ i ] ).dy, pTA( TargList.items[ i ] ).dx + 35, pTA( TargList.items[ i ] ).dy + 35 ), point( x, y ) ) then
        begin
          if ti <> -1 then
          begin //we already found one collision - impossible to have more than two
            if ( abs( x - tempx ) + abs( y - tempy ) ) > ( abs( x - pTA( TargList.items[ i ] ).dx + 17 ) + abs( y - pTA( TargList.items[ i ] ).dy + 17 ) ) then
            begin
              ti := i; //This icon's center is closer to the pointer than the other collison
            end;
          end
          else
          begin //this is the first collision we have detected between pointer and an icon
            tempx := pTA( TargList.items[ i ] ).dx + 17;
            tempy := pTA( TargList.items[ i ] ).dy + 17;
            ti := i;
          end; //endif tempx
        end; //endif ptInrect
      end; //end for
      if ti <> -1 then
      begin //if pointer is over an icon, plot the text
        pText.PlotText( pTA( TargList.items[ ti ] ).text, 25, 365, 240 );
        pText.PlotTextBlock( 'You may drag this Target Icon to any available Target slot.  You may not choose a target until you have dragged an Action Icon or a Spell Icon to a Command Priority slot.', 25, 550, 395, 240 );
      end;
    end; //endif ptinrect region - Target

      //plot rollover if any for Spells
    if y > 247 then
    begin
      tempx := 0;
      tempy := 0;
      ti := -1;
      for i := 0 to SpellList.count - 1 do
      begin
        if PtInRect( rect( pSP( SpellList.items[ i ] ).dx, pSP( SpellList.items[ i ] ).dy, pSP( SpellList.items[ i ] ).dx + 32, pSP( SpellList.items[ i ] ).dy + 32 ), point( x, y ) ) then
        begin
          if ti <> -1 then
          begin //we already found one collision - impossible to have more than two
            if ( abs( x - tempx ) + abs( y - tempy ) ) > ( abs( x - pSP( SpellList.items[ i ] ).dx + 16 ) + abs( y - pSP( SpellList.items[ i ] ).dy + 16 ) ) then
            begin
              ti := i; //This icon's center is closer to the pointer than the other collison
            end;
          end
          else
          begin //this is the first collision we have detected between pointer and an icon
            tempx := pSP( SpellList.items[ i ] ).dx + 16;
            tempy := pSP( SpellList.items[ i ] ).dy + 16;
            ti := i;
          end; //endif ti
        end; //endif ptInrect
      end; //end for
      if ti <> -1 then
      begin //if pointer is over an icon, plot the text
        pText.PlotText( CharSpellList[ pSP( SpellList.items[ ti ] ).SpellIndex ], 25, 365, 240 );
        pText.PlotTextBlock( 'You may drag this Spell Icon to any available Target slot.  You may not choose a target until you have dragged an Action Icon or a Spell Icon to a Command Priority slot.', 25, 550, 395, 240 );
      end;
    end; //endif Y > 247 spells

      //Plot Rollovers for Command Priority boxes and Target boxes
    if PtInRect( rect( 92, 40, 465, 211 ), point( x, y ) ) then
    begin
      tempx := 0;
      tempy := 0;
      ti := -1;
      for i := 0 to 7 do
      begin
        if PtInRect( DestRect[ i ].rect, point( x, y ) ) then
        begin
          if ti <> -1 then
          begin //we already found one collision - impossible to have more than two
            if ( abs( x - tempx ) + abs( y - tempy ) ) > ( abs( x - destRect[ i ].rect.left + 16 ) + abs( y - destRect[ i ].rect.top + 16 ) ) then
            begin
              ti := i; //This icon's center is closer to the pointer than the other collison
            end;
          end
          else
          begin //this is the first collision we have detected between pointer and an icon
            tempx := DestRect[ i ].rect.left + 16;
            tempy := DestRect[ i ].rect.top + 16;
            ti := i;
          end; //endif ti
        end; //endif ptInrect
      end; //end for
      if ti <> -1 then
      begin //if pointer is over a command slot, plot the text
            //pText.PlotText(CharSpellList[pSP(SpellList.items[ti]).SpellIndex],25,365,240);
            //pText.PlotTextBlock('You may drag this Spell Icon to any available Target slot.  You may not choose a target until you have dragged an Action Icon or a Spell Icon to a Command Priority slot.',25,550,395,240);
        if ti < 4 then
        begin
          if DestRect[ ti ].CommandIndex <> -1 then
          begin
            if DestRect[ ti ].CommandType = 1 then //action
              pText.PlotText( pTA( ActList.items[ DestRect[ ti ].CommandIndex ] ).text, 25, 365, 240 )
            else //spell
              pText.PlotText( CharSpellList[ pSP( SpellList.items[ DestRect[ ti ].CommandIndex ] ).SpellIndex ], 25, 365, 240 );
          end;
          pText.PlotTextBlock( DestRect[ ti ].text, 25, 550, 395, 240 )
        end
        else
        begin
          if DestRect[ ti ].visible then
          begin //if visible show text
            if DestRect[ ti ].CommandIndex <> -1 then
              pText.PlotText( pTA( TargList.items[ DestRect[ ti ].CommandIndex ] ).text, 25, 365, 240 );
            pText.PlotTextBlock( DestRect[ ti ].text, 25, 550, 395, 240 );
          end;
        end;
      end; //endif ti
    end; //endif CommandPriority box region check
    if PtInRect( rect( 576, 415, 576 + 74, 415 + 45 ), point( x, y ) ) then
    begin //plot back highlight
      WrapperBltFast( lpDDSBack, 576, 415, DXBackToGame, rect( 0, 0, 74, 45 ), DDBLTFAST_WAIT );
    end;
  end
  else
  begin //theres an icon being dragged
    WrapperBltFast( lpDDSBack, Tx, Ty, DXDirty, rect( 0, 0, 35, 35 ), DDBLTFAST_WAIT ); //clean up dirty
    Tx := X - 17;
    if Tx < 0 then
      Tx := 0;
    Ty := Y - 17;
    if Ty < 0 then
      Ty := 0;
    WrapperBltFast( DXDirty, 0, 0, lpDDSBack, rect( Tx, Ty, Tx + 35, Ty + 35 ), DDBLTFAST_WAIT ); //save dirty
    if CurrentSelectedType = 1 then
    begin //dragging an action icon
        //pText.PlotText(pTA(ActList.items[CurrentSelectedItem]).text,25,365,240);
        //pText.PlotTextBlock('You may drag this Action Icon to any of the four Command Priotity slots.  After selecting an Action, you will be prompted for a Target, if necessary.',25,550,395,240);
      WrapperBltFast( lpDDSBack, Tx, Ty, DXIcons, pTA( ActList.items[ CurrentSelectedItem ] ).sr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //PlotIcon
    end
    else if CurrentSelectedType = 2 then
    begin //dragging a target Icon
        //pText.PlotText(pTA(TargList.items[CurrentSelectedItem]).text,25,365,240);
        //pText.PlotTextBlock('You may drag this Target Icon to any available Target slot.  You may not choose a target until you have dragged an Action Icon or a Spell Icon to a Command Priority slot.',25,550,395,240);
      WrapperBltFast( lpDDSBack, Tx, Ty, DXIcons, pTA( TargList.items[ CurrentSelectedItem ] ).sr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //PlotIcon
    end
    else
    begin //dragging a spell icon
        //pText.PlotText(CharSpellList[pSP(SpellList.items[CurrentSelectedItem]).SpellIndex],25,365,240);
        //pText.PlotTextBlock('You may drag this Spell Icon to any available Target slot.  You may not choose a target until you have dragged an Action Icon or a Spell Icon to a Command Priority slot.',25,550,395,240);
      WrapperBltFast( lpDDSBack, Tx, Ty, DXSpellIcons, pSP( SpellList.items[ CurrentSelectedItem ] ).sr, DDBLTFAST_WAIT ); //PlotIcon
    end; //endif

  end; //endif CurrentSelectedItem

  lpDDSFront.Flip( nil, DDFLIP_WAIT );
  WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
  MouseCursor.PlotDirty := false;
end; //MouseMove


procedure TNPCBehavior.Paint;
begin

end;

procedure TNPCBehavior.Release;
var
  i : integer;
begin
  WriteAIData;

  FadeTimer.enabled := false;
  FadeTimer.free;
  FadeTimer := nil;

  DXBack := nil;
  DXBackToGame := nil;
  DXIcons := nil;
  DXDirty := nil;
  for i := 0 to 3 do
  begin
    DXTarget[ i ] := nil;
  end;

  for i := 0 to ActList.count - 1 do
  begin
    pTAIcon := pTA( ActList.Items[ i ] );
    dispose( pTAIcon );
  end;
  ActList.free;

  for i := 0 to TargList.count - 1 do
  begin
    pTAIcon := pTA( TargList.Items[ i ] );
    dispose( pTAIcon );
  end;
  TargList.free;

  for i := 0 to SpellList.count - 1 do
  begin
    pSpellIcon := SpellList.Items[ i ];
    dispose( pSpellIcon );
  end;
  SpellList.free;

  inherited;
end;

procedure TNPCBehavior.FadeTimerEvent( Sender : TObject );
begin
  if FadeAlpha < 115 then
  begin
    DrawAlpha( lpDDSBack, DestRect[ FadeIn + 4 ].dr, DestRect[ FadeIn + 4 ].sr, DXTarget[ FadeIn ], True, FadeAlpha );
    FadeAlpha := FadeAlpha + 25;
  end
  else
  begin
        //This is done so there isnt a color difference when clearing later
    WrapperBltFast( lpDDSBack, DestRect[ FadeIn + 4 ].dr.left, DestRect[ FadeIn + 4 ].dr.top, DXTarget[ FadeIn ], DestRect[ FadeIn + 4 ].sr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    FadeTimer.enabled := false;
  end;
  lpDDSFront.Flip( nil, DDFLIP_WAIT );
  WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
  MouseCursor.PlotDirty := false;
end; //FadeTimerEvent


procedure TNPCBehavior.ContainCursor( action : integer );
var
  prRect : PRect;
begin
  new( prRect );
  prRect.top := 0;
  prRect.left := 0;
  if Action = 1 then
  begin //restore to fullscreen
    prRect.bottom := 456;
    prRect.Right := 659;
  end
  else
  begin //constrict to main inventory area
    prRect.bottom := 600;
    prRect.Right := 800;
  end;
  ClipCursor( prRect );
  Dispose( prRect );
end; //TNPCBehavior.ContainCursor

procedure TNPCBehavior.WriteAIData;
var
  i : integer;
begin
   //first the priority boxes
  for i := 0 to 3 do
  begin
    if DestRect[ i ].CommandIndex <> -1 then
    begin
      if DestRect[ i ].CommandType = 1 then //action
        CharAI.Priority[ i + 1 ] := pTA( ActList[ DestRect[ i ].CommandIndex ] ).AiPriority
      else
      begin //spell
        CharAI.Priority[ i + 1 ] := prCast;
        CharAI.SpellToCast[ i + 1 ] := TSpell( CharSpellList.objects[ pSP( SpellList.items[ DestRect[ i ].CommandIndex ] ).SpellIndex ] );
      end;
    end
    else
    begin //no action in this priority slot
      CharAI.Priority[ i + 1 ] := prNone;
    end;
  end; //end for


   //Now the Target Boxes
  for i := 4 to 7 do
  begin
    if ( DestRect[ i ].CommandIndex <> -1 ) and ( DestRect[ i - 4 ].target <> TTNone ) then
    begin
      CharAI.Parameter[ i - 3 ] := pTA( TargList[ DestRect[ i ].CommandIndex ] ).AiParameter;
      if ( CharAI.Parameter[ i - 3 ] = paPlayerTarget ) or
        ( CharAI.Parameter[ i - 3 ] = paAttacker ) or
        ( CharAI.Parameter[ i - 3 ] = paClosestEnemy ) then
      begin
        CharAI.PartyMember[ i - 3 ] := CharAI.Character;
      end
      else if CharAI.Parameter[ i - 3 ] = paSpecificPartyMember then
      begin //characters are first in target list, so the index accurately represents which char
        CharAI.PartyMember[ i - 3 ] := NPCList[ DestRect[ i ].CommandIndex ];
      end; //end absurdly long multiline if statement
    end
    else
    begin //no action in this priority slot
      CharAI.Parameter[ i - 3 ] := paNone;
    end;
  end; //end for

end; //WriteAIData

procedure TNPCBehavior.LoadAIData;
var
  i, j, k : integer;
  ti : integer;
begin
  for i := 1 to 4 do
  begin
    if CharAI.Priority[ i ] <> prNone then
    begin
      j := 0;
      ti := -1;
      while j < ActList.count do
      begin
        if pTA( ActList.items[ j ] ).AiPriority = CharAI.Priority[ i ] then
        begin
          ti := j;
          j := 999;
        end;
        inc( j );
      end; //wend
      if j >= 999 then
      begin //found it- its an action
        DestRect[ i - 1 ].CommandIndex := ti;
        DestRect[ i - 1 ].CommandType := 1;
        DestRect[ i - 1 ].Target := pTA( ActList.items[ ti ] ).Target;
        Tx := DestRect[ i - 1 ].rect.left + ( ( DestRect[ i - 1 ].rect.right - DestRect[ i - 1 ].rect.left ) div 2 ) - 16;
        Ty := DestRect[ i - 1 ].rect.top + ( ( DestRect[ i - 1 ].rect.bottom - DestRect[ i - 1 ].rect.top ) div 2 ) - 17;
        WrapperBltFast( lpDDSBack, Tx, Ty, DXIcons, pTA( ActList.items[ ti ] ).sr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //PlotIcon
        if DestRect[ i - 1 ].Target <> ttNone then
        begin //if we have a target, plot it
          WrapperBltFast( lpDDSBack, DestRect[ i + 3 ].dr.left, DestRect[ i + 3 ].dr.top, DXTarget[ i - 1 ], DestRect[ i + 3 ].sr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
          if DestRect[ i - 1 ].Target = ttFriend then
          begin
            if CharAI.Parameter[ i ] = paSpecificPartyMember then
              j := NPCList.indexof( CharAI.PartyMember[ i ] ) //find the friend in the NPCList
            else if CharAI.Parameter[ i ] = paAnyPartyMember then
            begin //find the loc of the AnyPartyMember Icon ob
              k := 0;
              j := -1;
              while k < TargList.count do
              begin
                if pTA( TargList.items[ k ] ).AiParameter = paAnyPartyMember then
                begin
                  j := k;
                  k := 999;
                end;
                inc( k );
              end; //wend
            end
            else if CharAI.Parameter[ i ] = paAnyAlly then
            begin //find the loc of the AnyAlly Icon ob
              k := 0;
              j := -1;
              while k < TargList.count do
              begin
                if pTA( TargList.items[ k ] ).AiParameter = paAnyAlly then
                begin
                  j := k;
                  k := 999;
                end;
                inc( k );
              end; //wend
            end
            else //error- this should NEVER happen
              j := -1;
            if j <> -1 then
            begin
              Tx := DestRect[ i + 3 ].rect.left + ( ( DestRect[ i + 3 ].rect.right - DestRect[ i + 3 ].rect.left ) div 2 ) - 16;
              Ty := DestRect[ i + 3 ].rect.top + ( ( DestRect[ i + 3 ].rect.bottom - DestRect[ i + 3 ].rect.top ) div 2 ) - 17;
              WrapperBltFast( lpDDSBack, Tx, Ty, DXIcons, pTA( TargList.items[ j ] ).sr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //PlotIcon
              DestRect[ i + 3 ].CommandIndex := j;
              DestRect[ i + 3 ].Target := pTA( TargList.items[ j ] ).Target;
              DestRect[ i + 3 ].visible := true;
            end;
          end
          else
          begin //enemy
            j := 0;
            ti := -1;
                //get the index of the MyTarget Icon ob - its first in the list, these others follow
            while j < TargList.count do
            begin
              if pTA( TargList.items[ j ] ).AiParameter = paPlayerTarget then
              begin
                ti := j;
                j := 999;
              end;
              inc( j );
            end; //wend

            case CharAI.Parameter[ i ] of
              paPlayerTarget : ti := ti + 0;
              paAttacker : ti := ti + 1;
              paClosestEnemy : ti := ti + 2;
              paStrongestEnemy : ti := ti + 3;
              paWeakestEnemy : ti := ti + 4;
              paMostMagicalEnemy : ti := ti + 5;
            end; //end case
            Tx := DestRect[ i + 3 ].rect.left + ( ( DestRect[ i + 3 ].rect.right - DestRect[ i + 3 ].rect.left ) div 2 ) - 16;
            Ty := DestRect[ i + 3 ].rect.top + ( ( DestRect[ i + 3 ].rect.bottom - DestRect[ i + 3 ].rect.top ) div 2 ) - 17;
            WrapperBltFast( lpDDSBack, Tx, Ty, DXIcons, pTA( TargList.items[ ti ] ).sr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //PlotIcon
            DestRect[ i + 3 ].CommandIndex := ti;
            DestRect[ i + 3 ].Target := pTA( TargList.items[ ti ] ).Target;
            DestRect[ i + 3 ].visible := true;
          end; //endif target=ttFriend
        end; //if target <> None
      end
      else
      begin //it is a spell
        j := -1;
        k := 0;
        while k < CharSpellList.count do
        begin //get the index of this spell from charlist
          if CharSpellList.objects[ k ] = CharAI.SpellToCast[ i ] then
          begin
            j := k; //set j to the index
            k := 999;
          end;
          inc( k );
        end; //wend

        k := 0;
        ti := -1;
        while k < SpellList.count do
        begin //now find the spellicon in spellist that has this index
          if pSP( SpellList.items[ k ] ).SpellIndex = j then
          begin
            ti := k;
            k := 999;
          end;
          inc( k );
        end;
        if k >= 999 then
        begin //plot the spell icon and set the vals in DestRect
          DestRect[ i - 1 ].CommandIndex := ti;
          DestRect[ i - 1 ].CommandType := 3;
          DestRect[ i - 1 ].Target := TSpell( CharSpellList.objects[ pSP( SpellList.items[ ti ] ).SpellIndex ] ).TargetType;
          Tx := DestRect[ i - 1 ].rect.left + ( ( DestRect[ i - 1 ].rect.right - DestRect[ i - 1 ].rect.left ) div 2 ) - 15;
          Ty := DestRect[ i - 1 ].rect.top + ( ( DestRect[ i - 1 ].rect.bottom - DestRect[ i - 1 ].rect.top ) div 2 ) - 15;
          WrapperBltFast( lpDDSBack, Tx, Ty, DXSpellIcons, pSP( SpellList.items[ ti ] ).sr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //PlotIcon
             //now for Targets of spells
          if DestRect[ i - 1 ].Target <> ttNone then
          begin //if we have a target, plot it
            WrapperBltFast( lpDDSBack, DestRect[ i + 3 ].dr.left, DestRect[ i + 3 ].dr.top, DXTarget[ i - 1 ], DestRect[ i + 3 ].sr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
            if DestRect[ i - 1 ].Target = ttFriend then
            begin
              j := NPCList.indexof( CharAI.PartyMember[ i ] ); //find the friend in the NPCList
              if j <> -1 then
              begin
                Tx := DestRect[ i + 3 ].rect.left + ( ( DestRect[ i + 3 ].rect.right - DestRect[ i + 3 ].rect.left ) div 2 ) - 16;
                Ty := DestRect[ i + 3 ].rect.top + ( ( DestRect[ i + 3 ].rect.bottom - DestRect[ i + 3 ].rect.top ) div 2 ) - 17;
                WrapperBltFast( lpDDSBack, Tx, Ty, DXIcons, pTA( TargList.items[ j ] ).sr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //PlotIcon
                DestRect[ i + 3 ].CommandIndex := j;
                DestRect[ i + 3 ].Target := pTA( TargList.items[ j ] ).Target;
                DestRect[ i + 3 ].visible := true;
              end;
            end
            else
            begin //enemy
              j := 0;
              ti := -1;
                   //get the index of the MyTarget Icon ob - its first in the list, these others follow
              while j < TargList.count do
              begin
                if pTA( TargList.items[ j ] ).AiParameter = paPlayerTarget then
                begin
                  ti := j;
                  j := 999;
                end;
                inc( j );
              end; //wend

              case CharAI.Parameter[ i ] of
                paPlayerTarget : ti := ti + 0;
                paAttacker : ti := ti + 1;
                paClosestEnemy : ti := ti + 2;
                paStrongestEnemy : ti := ti + 3;
                paWeakestEnemy : ti := ti + 4;
                paMostMagicalEnemy : ti := ti + 5;
              end; //end case
              Tx := DestRect[ i + 3 ].rect.left + ( ( DestRect[ i + 3 ].rect.right - DestRect[ i + 3 ].rect.left ) div 2 ) - 16;
              Ty := DestRect[ i + 3 ].rect.top + ( ( DestRect[ i + 3 ].rect.bottom - DestRect[ i + 3 ].rect.top ) div 2 ) - 17;
              WrapperBltFast( lpDDSBack, Tx, Ty, DXIcons, pTA( TargList.items[ ti ] ).sr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT ); //PlotIcon
              DestRect[ i + 3 ].CommandIndex := ti;
              DestRect[ i + 3 ].Target := pTA( TargList.items[ ti ] ).Target;
              DestRect[ i + 3 ].visible := true;
            end; //endif target=ttFriend
          end; //if target <> None

        end; //endif j > -1
      end; //endif j >=999
    end; //endif CharAI.Priotity
  end; //end for

end; //loadAidata

end.

