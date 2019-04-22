unit DXUtil;
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

interface

uses
  Windows,
  Classes,
  DirectX,
  Graphics,
  Sysutils,
  LogFile,
  SoAOS.Graphics.Draw;

procedure GetSurfaceDims( var W, H : Integer; Surface : IDirectDrawSurface );
function DDGetSurface( lpDD : IDirectDraw; W, H : integer; Color : TColor; Video : Boolean; var ColorMatch : integer ) : IDirectDrawSurface; overload;
function DDGetSurface( lpDD : IDirectDraw; W, H : integer; Color : TColor; Video : Boolean ) : IDirectDrawSurface; overload;

implementation

procedure GetSurfaceDims( var W, H : Integer; Surface : IDirectDrawSurface );
var
  lpDDSurfaceDesc : TDDSurfaceDesc;
const
  FailName : string = 'DXUtil.GetSurfaceDims';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    lpDDSurfaceDesc.dwSize := SizeOf( lpDDSurfaceDesc );
    lpDDSurfaceDesc.dwFlags := DDSD_HEIGHT + DDSD_WIDTH;
    Surface.GetSurfaceDesc( lpDDSurfaceDesc );
    W := lpDDSurfaceDesc.dwWidth;
    H := lpDDSurfaceDesc.dwHeight;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

function DDGetSurface( lpDD : IDirectDraw; W, H : integer; Color : TColor; Video : Boolean; var ColorMatch : integer ) : IDirectDrawSurface;
var
  ddsd : TDDSurfaceDesc;
  pdds : IDirectDrawSurface;
  ddck : TDDCOLORKEY;
  BltFx : TDDBLTFX;
  pr: TRect;
const
  FailName : string = 'DXUtil.DDGetSurface2';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    ddsd.dwSize := SizeOf( ddsd );
    ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
    if Video then
      ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_VIDEOMEMORY
    else
      ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
    ddsd.dwWidth := W;
    ddsd.dwHeight := H;

    Result := nil;

    if ( lpdd.CreateSurface( ddsd, pdds, nil ) <> DD_OK ) then
    begin
      ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
      if ( lpdd.CreateSurface( ddsd, pdds, nil ) <> DD_OK ) then
        Exit;
    end;

    ColorMatch := SoAOS_DX_ColorMatch( pdds, ColorToRGB( Color ) );

    ddck.dwColorSpaceLowValue := ColorMatch;
    ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
    pdds.SetColorKey( DDCKEY_SRCBLT, @ddck );

    BltFx.dwSize := SizeOf( BltFx );
    BltFx.dwFillColor := ColorMatch;
    pr := Rect( 0, 0, W, H );
    pdds.Blt(@pr, nil, @pr, DDBLT_COLORFILL + DDBLT_WAIT, @BltFx );
    Result := pdds;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

function DDGetSurface( lpDD : IDirectDraw; W, H : integer; Color : TColor; Video : Boolean ) : IDirectDrawSurface;
var
  ddsd : TDDSurfaceDesc;
  pdds : IDirectDrawSurface;
  ddck : TDDCOLORKEY;
  BltFx : TDDBLTFX;
  ColorMatch : integer;
  pr: TRect;
const
  FailName : string = 'DXUtil.DDGetSurface1';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    ddsd.dwSize := SizeOf( ddsd );
    ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
    if Video then
      ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_VIDEOMEMORY
    else
      ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
    ddsd.dwWidth := W;
    ddsd.dwHeight := H;

    Result := nil;
    if ( lpdd.CreateSurface( ddsd, pdds, nil ) <> DD_OK ) then
    begin
      ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
      if ( lpdd.CreateSurface( ddsd, pdds, nil ) <> DD_OK ) then
        Exit;
    end;
    ColorMatch := SoAOS_DX_ColorMatch( pdds, ColorToRGB( Color ) );
    ddck.dwColorSpaceLowValue := ColorMatch;
    ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
    pdds.SetColorKey( DDCKEY_SRCBLT, @ddck );

    BltFx.dwSize := SizeOf( BltFx );
    BltFx.dwFillColor := ColorMatch;
    pr := Rect( 0, 0, W, H );
    pdds.Blt(@pr, nil, @pr, DDBLT_COLORFILL + DDBLT_WAIT, @BltFx );
    Result := pdds;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

end.

