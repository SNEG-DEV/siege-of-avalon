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
  DirectDraw,
  Graphics,
  Sysutils,
  logger;

function DDColorMatch( pdds : IDirectDrawSurface; Color : TColor ) : Longint;
function DDGetImage( lpDD : IDirectDraw; BITMAP : TBitmap; Color : TColor; Video : Boolean ) : IDirectDrawSurface;
function DDGetOverlay( lpDD : IDirectDraw; BITMAP : TBitmap; Color : TColor ) : IDirectDrawSurface;
procedure GetSurfaceDims( var W, H : Integer; Surface : IDirectDrawSurface );
function DDGetSurface( lpDD : IDirectDraw; W, H : integer; Color : TColor; Video : Boolean; var ColorMatch : integer ) : IDirectDrawSurface; overload;
function DDGetSurface( lpDD : IDirectDraw; W, H : integer; Color : TColor; Video : Boolean ) : IDirectDrawSurface; overload;
function WrapperBltFast( DDDstSurface : IDirectDrawSurface; X : DWORD; Y : DWORD; DDSrcSurface : IDirectDrawSurface; SrcRect : TRect; Trans : DWORD ) : HResult;
function WrapperBlt( DDDstSurface : IDirectDrawSurface; DstRect : TRect; DDSrcSurface : IDirectDrawSurface; SrcRect : TRect; Flags : DWORD; DDBltFx : TDDBltFX ) : HResult;

implementation

function DDColorMatch( pdds : IDirectDrawSurface; Color : TColor ) : Longint;
var
  RGB, rgbT : COLORREF;
  DC : HDC;
  dw : Longint;
  ddsd : TDDSurfaceDesc;
  hres : HRESULT;
const
  FailName : string = 'DXUtil.DDColorMatch';
begin
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
      Log.LogError( E.Message, FailName );
  end;
end;

function DDGetImage( lpDD : IDirectDraw; BITMAP : TBitmap; Color : TColor; Video : Boolean ) : IDirectDrawSurface;
var
  ddsd : TDDSurfaceDesc;
  pdds : IDirectDrawSurface;
  DC : HDC;
  ddck : TDDCOLORKEY;
const
  FailName : string = 'DXUtil.DDGetImage';
begin
  try
    ddsd.dwSize := SizeOf( ddsd );
    ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH;
    if Video then
      ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_VIDEOMEMORY
    else
      ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
    ddsd.dwWidth := BITMAP.Width;
    ddsd.dwHeight := BITMAP.Height;

    Result := nil;

    if ( lpdd.CreateSurface( ddsd, pdds, nil ) <> DD_OK ) then
    begin
      ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
      if ( lpdd.CreateSurface( ddsd, pdds, nil ) <> DD_OK ) then
        Exit;
    end;

    pdds.GetDC( DC );
    BitBlt( DC, 0, 0, BITMAP.width, BITMAP.Height, BITMAP.Canvas.Handle, 0, 0, SRCCOPY );
    pdds.ReleaseDC( DC );
    ddck.dwColorSpaceLowValue := DDColorMatch( pdds, Color );
    ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
    pdds.SetColorKey( DDCKEY_SRCBLT, {$IFNDEF DX5}@{$ENDIF}ddck );
    Result := pdds;
  except
    on E : Exception do
      Log.LogError( E.Message, FailName );
  end;
end;

function DDGetOverlay( lpDD : IDirectDraw; BITMAP : TBitmap; Color : TColor ) : IDirectDrawSurface;
var
  ddsd : TDDSurfaceDesc;
  pdds : IDirectDrawSurface;
  DC : HDC;
  ddck : TDDCOLORKEY;
const
  FailName : string = 'DXUtil.DDGetOverlay';
begin
  try
    ZeroMemory( @ddsd, SizeOf( ddsd ) );
    ddsd.dwSize := SizeOf( ddsd );
    ddsd.dwFlags := DDSD_CAPS + DDSD_HEIGHT + DDSD_WIDTH + DDSD_PIXELFORMAT;
    ddsd.ddsCaps.dwCaps := DDSCAPS_VIDEOMEMORY or DDSCAPS_OVERLAY;
    ddsd.dwWidth := BITMAP.Width;
    ddsd.dwHeight := BITMAP.Height;
    ddsd.ddpfPixelFormat.dwSize := SizeOf( TDDPIXELFORMAT );
    ddsd.ddpfPixelFormat.dwFlags := DDPF_RGB;
    ddsd.ddpfPixelFormat.dwRGBBitCount := 16;
    ddsd.ddpfPixelFormat.dwRBitMask := $0000F800;
    ddsd.ddpfPixelFormat.dwGBitMask := $000007E0;
    ddsd.ddpfPixelFormat.dwBBitMask := $0000001F;

    Result := nil;

    if ( lpdd.CreateSurface( ddsd, pdds, nil ) <> DD_OK ) then
      Exit;

    pdds.GetDC( DC );
    BitBlt( DC, 0, 0, BITMAP.width, BITMAP.Height, BITMAP.Canvas.Handle, 0, 0, SRCCOPY );
    pdds.ReleaseDC( DC );
    ddck.dwColorSpaceLowValue := DDColorMatch( pdds, Color );
    ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
    pdds.SetColorKey( DDCKEY_SRCBLT, {$IFNDEF DX5}@{$ENDIF}ddck );
    Result := pdds;
  except
    on E : Exception do
      Log.LogError( E.Message, FailName );
  end;
end;

procedure GetSurfaceDims( var W, H : Integer; Surface : IDirectDrawSurface );
var
  lpDDSurfaceDesc : TDDSurfaceDesc;
const
  FailName : string = 'DXUtil.GetSurfaceDims';
begin
  try
    lpDDSurfaceDesc.dwSize := SizeOf( lpDDSurfaceDesc );
    lpDDSurfaceDesc.dwFlags := DDSD_HEIGHT + DDSD_WIDTH;
    Surface.GetSurfaceDesc( lpDDSurfaceDesc );
    W := lpDDSurfaceDesc.dwWidth;
    H := lpDDSurfaceDesc.dwHeight;
  except
    on E : Exception do
      Log.LogError( E.Message, FailName );
  end;
end;

function DDGetSurface( lpDD : IDirectDraw; W, H : integer; Color : TColor; Video : Boolean; var ColorMatch : integer ) : IDirectDrawSurface;
var
  ddsd : TDDSurfaceDesc;
  pdds : IDirectDrawSurface;
  ddck : TDDCOLORKEY;
  BltFx : TDDBLTFX;
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

    ColorMatch := DDColorMatch( pdds, ColorToRGB( Color ) );

    ddck.dwColorSpaceLowValue := ColorMatch;
    ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
    pdds.SetColorKey( DDCKEY_SRCBLT, {$IFNDEF DX5}@{$ENDIF}ddck );

    BltFx.dwSize := SizeOf( BltFx );
    BltFx.dwFillColor := ColorMatch;
    WrapperBlt( pdds, Rect( 0, 0, W, H ), nil, Rect( 0, 0, W, H ), DDBLT_COLORFILL + DDBLT_WAIT, BltFx );
    Result := pdds;
  except
    on E : Exception do
      Log.LogError( E.Message, FailName );
  end;
end;

function DDGetSurface( lpDD : IDirectDraw; W, H : integer; Color : TColor; Video : Boolean ) : IDirectDrawSurface;
var
  ddsd : TDDSurfaceDesc;
  pdds : IDirectDrawSurface;
  ddck : TDDCOLORKEY;
  BltFx : TDDBLTFX;
  ColorMatch : integer;
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

    ColorMatch := DDColorMatch( pdds, ColorToRGB( Color ) );

    ddck.dwColorSpaceLowValue := ColorMatch;
    ddck.dwColorSpaceHighValue := ddck.dwColorSpaceLowValue;
    pdds.SetColorKey( DDCKEY_SRCBLT, {$IFNDEF DX5}@{$ENDIF}ddck );

    BltFx.dwSize := SizeOf( BltFx );
    BltFx.dwFillColor := ColorMatch;
    WrapperBlt( pdds, Rect( 0, 0, W, H ), nil, Rect( 0, 0, W, H ), DDBLT_COLORFILL + DDBLT_WAIT, BltFx );
    Result := pdds;
  except
    on E : Exception do
      Log.LogError( E.Message, FailName );
  end;
end;

function WrapperBltFast( DDDstSurface : IDirectDrawSurface; X : DWORD; Y : DWORD; DDSrcSurface : IDirectDrawSurface; SrcRect : TRect;
  Trans : DWORD ) : HResult;
begin
  result := DDDstSurface.BltFast( X, Y, DDSrcSurface, {$IFNDEF DX5}@{$ENDIF}SrcRect, Trans );
end;

function WrapperBlt( DDDstSurface : IDirectDrawSurface; DstRect : TRect; DDSrcSurface : IDirectDrawSurface; SrcRect : TRect; Flags : DWORD; DDBltFx : TDDBltFX ) : HResult;
begin
  result := DDDstSurface.Blt( {$IFNDEF DX5}@{$ENDIF}DstRect, DDSrcSurface, {$IFNDEF DX5}@{$ENDIF}SrcRect, Flags, {$IFNDEF DX5}@{$ENDIF}DDBltFx );
end;

end.

