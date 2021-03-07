unit SoAOS.Graphics.Draw;
(*
  Siege Of Avalon : Open Source Edition

  Portions created by Steffen Nyeland are
  Copyright (C) 2019 - Steffen Nyeland.

  Contributor(s):
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

  Description: SoAOS DirectX drawing

  Requires: Delphi 10.3.3 or later
            DirectX

  Revision History:
  - 7 Apr 2019 - SN: Initial Commit to Git
  see git repo afterwards

*)

interface

uses
  Winapi.Windows,
  System.Types,
  System.UITypes,
  Vcl.Graphics,

//  Winapi.DirectDraw,
  DirectX;

/// <summary>
/// FastBlt whole backsurface to frontsurface - use when done with drawing on backsurface
/// </summary>
procedure SoAOS_DX_BltFront;
/// <summary>
/// Clean up surface by srcSur rectange at srcRect.Left, srcRect.Top
/// </summary>
procedure SoAOS_DX_BltFastWaitXY( srcSur: IDirectDrawSurface; srcRect: TRect );

function SoAOS_DX_SurfaceFromBMP ( bmp: TBitmap; color: TColor; video: Boolean = True ) : IDirectDrawSurface;
function SoAOS_DX_LoadBMP( filename: string; color: TColor; video: Boolean = True ) : IDirectDrawSurface; overload;
function SoAOS_DX_LoadBMP( filename: string; color: TColor; out width, height: integer; video: Boolean = True ) : IDirectDrawSurface; overload;
function SoAOS_DX_LoadBMPResource( resname: string; color: TColor; video: Boolean = True ) : IDirectDrawSurface; overload;
function SoAOS_DX_LoadBMPResource( resname: string; color: TColor; out width, height: integer; video: Boolean = True ) : IDirectDrawSurface; overload;
function SoAOS_DX_ColorMatch( pdds : IDirectDrawSurface; Color : TColor ) : Longint;

implementation

uses
  System.SysUtils,
  AniDemo,
  Engine,
  LogFile,
  SoAOS.Animation,
  DXUtil;

procedure SoAOS_DX_BltFront;
var
  pr: TRect;
begin
  lpDDSFront_Flip( nil, DDFLIP_WAIT );
  pr := Rect( 0, 0, ScreenMetrics.ScreenWidth, ScreenMetrics.ScreenHeight );
  lpDDSBack.BltFast( 0, 0, lpDDSFront, @pr, DDBLTFAST_WAIT );
  MouseCursor.PlotDirty := false;
end;

procedure SoAOS_DX_BltFastWaitXY( srcSur: IDirectDrawSurface; srcRect: TRect );
begin
  lpDDSBack.BltFast( srcRect.Left, srcRect.Top, srcSur, @srcRect, DDBLTFAST_WAIT );
end;

function SoAOS_DX_SurfaceFromBMP ( bmp: TBitmap; color: TColor; video: Boolean = True ) : IDirectDrawSurface;
var
  ddsd : TDDSurfaceDesc;
  pdds : IDirectDrawSurface;
  DC   : HDC;
  ddck : TDDCOLORKEY;
begin
  ZeroMemory(@ddsd, sizeof(ddsd));
    try
      ddsd.dwSize := SizeOf( ddsd );
      ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
      if Video then
        ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_VIDEOMEMORY
      else
        ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
      ddsd.dwWidth := bmp.width;
      ddsd.dwHeight := bmp.height;
      Result := nil;
      if ( lpdd_CreateSurface( ddsd, pdds, nil ) <> DD_OK ) then
      begin
        ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
        if ( lpdd_CreateSurface( ddsd, pdds, nil ) <> DD_OK ) then
          Exit;
      end;
      pdds.GetDC( DC );
      BitBlt( DC, 0, 0, bmp.width, bmp.height, bmp.Canvas.Handle, 0, 0, SRCCOPY );
      pdds.ReleaseDC( DC );
      ddck.dwColorSpaceLowValue := SoAOS_DX_ColorMatch( pdds, Color );
      ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
      pdds.SetColorKey( DDCKEY_SRCBLT, @ddck );
      Result := pdds;
    except
      on E : Exception do
        Log.log( 'DX_LoadBMP' + E.Message + #13+#10 + E.StackTrace );
    end;
end;

function SoAOS_DX_LoadBMP( filename: string; color: TColor; out width, height: integer; video: Boolean = True ) : IDirectDrawSurface;
var
  bmp  : TBitmap;
begin
  bmp := TBitmap.Create;
  try
    bmp.LoadFromFile(filename);
    width := bmp.Width;
    height := bmp.Height;
    Result := SoAOS_DX_SurfaceFromBMP ( bmp, color, video )
  finally
    bmp.Free;
  end;
end;

function SoAOS_DX_LoadBMP( filename: string; color: TColor; video: Boolean = True ) : IDirectDrawSurface;
var
  w, h : Integer;
begin
  Result := SoAOS_DX_LoadBMP( filename, color, w, h, video );
end;

function SoAOS_DX_LoadBMPResource( resname: string; color: TColor; out width, height: integer; video: Boolean = True ) : IDirectDrawSurface;
var
  bmp  : TBitmap;
begin
  bmp := TBitmap.Create;
  try
    bmp.LoadFromResourceName(HInstance, resname);
    width := bmp.Width;
    height := bmp.Height;
    Result := SoAOS_DX_SurfaceFromBMP ( bmp, color, video )
  finally
    bmp.Free;
  end;
end;

function SoAOS_DX_LoadBMPResource( resname: string; color: TColor; video: Boolean = True ) : IDirectDrawSurface;
var
  w, h : Integer;
begin
  Result := SoAOS_DX_LoadBMPResource( resname, color, w, h, video );
end;

function SoAOS_DX_ColorMatch( pdds : IDirectDrawSurface; Color : TColor ) : Longint;
var
  RGB, rgbT : COLORREF;
  DC : HDC;
  dw : Longint;
  ddsd : TDDSurfaceDesc;
  hres : HRESULT;
const
  FailName : string = 'DXUtil.DDColorMatch';
begin
  ZeroMemory(@ddsd, sizeof(ddsd));
  Log.DebugLog(FailName);
  Result := 0;
  try
    dw := 0;
    rgbT := 0;
    RGB := ColorToRGB( Color );
    if ( RGB <> CLR_INVALID ) then
    begin
      if ( pdds.GetDC( DC ) = DD_OK ) then
      begin
        rgbT := GetPixel( DC, 0, 0 ); // Save current pixel value
        SetPixel( DC, 0, 0, RGB ); // Set our value
        pdds.ReleaseDC( DC );
      end;
    end;
    ddsd.dwSize := SizeOf( ddsd );
    hres := pdds.Lock( nil, ddsd, 0, 0 );
    while ( hres = DDERR_WASSTILLDRAWING ) do
      hres := pdds.Lock( nil, ddsd, 0, 0 );
    if ( hres = DD_OK ) then
    begin
      dw := Longint( ddsd.lpSurface^ ); // Get DWORD
      if ( ddsd.ddpfPixelFormat.dwRGBBitCount < 32 ) then
        dw := dw and ( ( 1 shl ddsd.ddpfPixelFormat.dwRGBBitCount ) - 1 ); // Mask it to bpp
      pdds.Unlock( nil );
    end;
    if ( RGB <> CLR_INVALID ) then
    begin
      if ( pdds.GetDC( DC ) = DD_OK ) then
      begin
        SetPixel( DC, 0, 0, rgbT );
        pdds.ReleaseDC( DC );
      end;
    end;
    Result := dw;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end;

end.
