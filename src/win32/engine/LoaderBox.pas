unit LoaderBox;
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
  Forms,
  Classes,
  Graphics,
  SysUtils,
  Engine,
  Anigrp30,
  logfile;

type
  TLoaderBox = class( TObject )
  private
    DxBox : IDirectDrawSurface;
    OldValue : integer;
    BltFx : DDBLTFX;
  public
    Loaded : Boolean;
    MaxValue : integer;
    FileName : string;
    procedure SetBar( CurrentValue : integer );
    constructor Create;
    destructor Destroy; override;
    procedure Init;
    procedure Release;
  end;

implementation
uses
  AniDemo;
{ TLoaderBox }

constructor TLoaderBox.Create;
const
  FailName : string = 'TLoaderBox.create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    BltFx.dwSize := SizeOf( BltFx ); //RGB(244,164,4)
    BltFx.dwFillColor := DDColorMatch( lpDDSFront, RGB( 32, 128, 16 ) );
    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

destructor TLoaderBox.Destroy;
const
  FailName : string = 'TLoaderBox.destroy';
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

procedure TLoaderBox.Init;
var
  BMBack : TBitmap;
  BltFx : DDBLTFX;
  i : integer;
const
  FailName : string = 'TLoaderBox.init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if Loaded then
      Exit;

    OldValue := 0;

    BMBack := TBitmap.create;

  //BMBack.LoadFromFile(InterfacePath+'LoaderBox.bmp');
    BMBack.LoadFromFile( FileName );
    DXBox := DDGetImage( lpDD, BMBack, $00FFFF00, False );
  //lpDDSBack.BltFast(250, 169, DXBox, Rect(0, 0, BMBack.width, BMBack.Height), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
  //Draw beveling
    BltFx.dwSize := SizeOf( BltFx );
    BltFx.dwFillColor := DDColorMatch( DXBox, RGB( 205, 205, 205 ) );
  //DXBox.Blt(rect(50+i-5,500+i-5,750,500+i+1-5),nil,rect(50+i-5,500+i-5,750,500+i+1-5),DDBLT_COLORFILL + DDBLT_WAIT, BltFx);

    lpDDSBack.BltFast( 0, 0, DXBox, Rect( 0, 0, BMBack.width, BMBack.Height ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
 { for i:=0 to 9 do begin
     DrawSub(lpDDSBack,rect(50+i-5,500+i-5,750,500+i+1-5),rect(50+i-5,500+i-5,750,500+i+1-5),DXBox,False,150-i*10);
     DrawSub(lpDDSBack,rect(50+i-5,500+i-5,50+i-5+1,550),rect(50+i-5,500+i-5,50+i-5+1,550),DXBox,False,150-i*10);
     lpDDSBack.Blt(rect(59-i-5,550+i-5,750,550+i+1-5),nil,rect(59-i-5,550+i-5,750,550+i+1-5),DDBLT_COLORFILL + DDBLT_WAIT, BltFx);
     DrawAlpha(lpDDSBack,rect(59-i-5,550+i-5,750,550+i+1-5),rect(59-i-5,550+i-5,750,550+i+1-5),DXBox,False,200-i*10);
  end; }
  //DrawSub(lpDDSBack,rect(50,500,750,501),rect(50,500,750,501),DXBox,False,100);
  //DrawSub(lpDDSBack,rect(50,500,750,550),rect(50,500,750,550),DXBox,False,100);
    for i := 0 to 25 do
    begin
      DrawSub( lpDDSBack, rect( 55, 505 + i, 745, 505 + i + 1 ), rect( 55, 505 + i, 745, 505 + i + 1 ), DXBox, False, 100 - i * 3 );
    end;
    BMBack.free;

    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    lpDDSBack.BltFast( 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //init

procedure TLoaderBox.Release;
const
  FailName : string = 'TLoaderBox.release';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    DXBox := nil;

    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

procedure TLoaderBox.SetBar( CurrentValue : integer );
var
  Value : integer;
const
  FailName : string = 'TLoaderBox.setbar';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
  //Value:=round(CurrentValue*(200.0/MaxValue));
//  Value:=round(CurrentValue*(680.0/MaxValue));
    Value := ( CurrentValue * 680 ) div MaxValue;
    if Value > OldValue then
    begin
     //lpDDSFront.Blt(rect(47+250,194+169,Value+47+250,212+169),nil,rect(47+250,194+169,Value+47+250,212+169),DDBLT_COLORFILL + DDBLT_WAIT, BltFx);
//     lpDDSFront.Blt(rect(60,510,Value+60,525),nil,rect(50,510,Value+60,525),DDBLT_COLORFILL + DDBLT_WAIT, BltFx);
      lpDDSFront.Blt( rect( OldValue + 60, 510, Value + 60, 525 ), nil, rect( 0, 0, 0, 0 ), DDBLT_COLORFILL + DDBLT_WAIT, BltFx );
      OldValue := Value;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //SetBar

end.
