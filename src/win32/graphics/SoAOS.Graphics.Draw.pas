unit SoAOS.Graphics.Draw;

interface

uses
  Winapi.Windows,
  System.Types,
  System.UITypes,
  Vcl.Graphics,

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
function SoAOS_DX_ColorMatch( pdds : IDirectDrawSurface; Color : TColor ) : Longint;

implementation

uses
  System.SysUtils,
  AniDemo,
  Anigrp30,
  Engine,
  LogFile,

  DXUtil;

procedure SoAOS_DX_BltFront;
var
  pr: TRect;
begin
  lpDDSFront.Flip( nil, DDFLIP_WAIT );
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
      if ( lpdd.CreateSurface( ddsd, pdds, nil ) <> DD_OK ) then
      begin
        ddsd.ddsCaps.dwCaps := DDSCAPS_OFFSCREENPLAIN or DDSCAPS_SYSTEMMEMORY;
        if ( lpdd.CreateSurface( ddsd, pdds, nil ) <> DD_OK ) then
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
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
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
