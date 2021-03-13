unit DXUtil;
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
            DirectX

  Revision History:
  - 13 Jul 2003 - DL: Initial Upload to CVS
  - 10 Mar 2019 - SN: Forked on GitHub
  see git repo afterwards

*)

interface

uses
  Winapi.Windows,
  System.Classes,
//  Winapi.DirectDraw,
  DirectX,
  Vcl.Graphics, // replace by System.UITypes,
  System.Sysutils,
  LogFile,
  SoAOS.Graphics.Draw,
  SoAOS.Animation
  ;

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
  ZeroMemory(@lpDDSurfaceDesc, sizeof(lpDDSurfaceDesc));
  Log.DebugLog(FailName);
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
  ZeroMemory(@ddsd, sizeof(ddsd));
  Log.DebugLog(FailName);
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

    if ( lpdd_CreateSurface( ddsd, pdds, nil ) <> DD_OK ) then
    begin
      ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
      if ( lpdd_CreateSurface( ddsd, pdds, nil ) <> DD_OK ) then
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
  FillChar(ddsd, sizeof(ddsd), 0);
  Log.DebugLog(FailName);
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
    if ( lpdd_CreateSurface( ddsd, pdds, nil ) <> DD_OK ) then
    begin
      ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
      if ( lpdd_CreateSurface( ddsd, pdds, nil ) <> DD_OK ) then
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

