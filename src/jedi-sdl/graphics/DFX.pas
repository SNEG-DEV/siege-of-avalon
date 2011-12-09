unit DFX;
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
  Messages,
  SysUtils,
  Classes,
  //Graphics,
  //Controls,
  //Forms,
  //Dialogs,
  digifx,
  DirectDraw,
  DXUtil,
  //Anigrp30,
  logger;

const
  pixelformats : array[ 0..2 ] of DWORD = ( PIXFMT_555, PIXFMT_565, PIXFMT_888 );

var
  dfx_pixelformat : DWORD = 0;
  dfx_hnd : DFXHND = 0;
  pf_index : Integer = 0;

type
  TBitPlane = class;

  TRLESprite = class( TObject )
  private
    PicCnt : DWORD;
    lpSpr : PRLEHDR;
    FMemSize : LongWord;
    function GetImage( Index : DWORD ) : PRLEHDR;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromRLE( FileName : string );
    procedure SaveToRLE( FileName : string );
    procedure LoadFromStream( Stream : TStream );
    procedure SaveToStream( Stream : TStream );
    procedure LoadFromBitPlane( BitPlane : TBitPlane );
    procedure LoadFromBitPlaneBits( Bits : PBITPLANE; Color : TColor );
    procedure LoadMoreFromBitPlane( BitPlane : TBitPlane );
    procedure LoadFromBitmap( BITMAP : TBitmap; FrameWidth, FrameHeight : Integer; Color : TColor );
    procedure Draw( Index : LongWord; X, Y : Integer; Bits : PBITPLANE );
    procedure DrawBlend( Index : LongWord; X, Y : Integer; Bits : PBITPLANE; SrcBlend, DstBlend : DWORD );
    procedure DrawMono( Index : LongWord; X, Y : Integer; Bits : PBITPLANE; Color : TColor );
    procedure DrawColorize( Index : longword; X, Y : integer; Bits : PBITPLANE; R, G, B, SrcBlend, DstBlend : integer );
    procedure DrawAntiAlias( Index : longword; X, Y : integer; Bits : PBITPLANE );
    procedure DrawSoften( Index : longword; X, Y : integer; Bits : PBITPLANE );
    property Frames : DWORD read PicCnt;
    property Image[ Index : DWORD ] : PRLEHDR read GetImage;
    property MemSize : LongWord read FMemSize;
  end;

  TBitPlane = class( TObject )
  private
    FKeyColor : TColor;
    FKeyIndex : LongWord;
    R, G, B : Byte;
    function GetBits : PBITPLANE;
    function GetHeight : LongWord;
    function GetWidth : LongWord;
    procedure SetKeyColor( const Value : TColor );
  public
    FBits : BITPLANE;
    constructor Create( Width, Height : Integer );
    destructor Destroy; override;
    procedure DrawToDC( DC : HDC; X, Y : Integer );
    procedure Clear;
    procedure Fill( Color : TColor );
    procedure Draw( X, Y : Integer; Bits : PBITPLANE );
    procedure DrawMono( X, Y : Integer; Bits : PBITPLANE; Color : TColor );
    procedure DrawBlend( X, Y : Integer; Bits : PBITPLANE; SrcBlend, DstBlend : DWORD );
    procedure DrawOutline( X, Y : integer; Bits : PBITPLANE; Color : TColor; Copy : boolean );
    procedure DrawColorize( X, Y : integer; Bits : PBITPLANE; R, G, B, SrcBlend, DstBlend : integer );
    procedure DrawShadow( X, Y : Integer; Bits : PBITPLANE; SrcBlend, DstBlend : DWORD; Angle, Height : integer );
    procedure DrawAntiAlias( X, Y : integer; Bits : PBITPLANE );
    property Bits : PBITPLANE read GetBits;
    property Width : LongWord read GetWidth;
    property Height : LongWord read GetHeight;
    property KeyColor : TColor read FKeyColor write SetKeyColor;
  end;

function DFXInit( Path : string ) : BOOL;
procedure DFXShutdown;
procedure DFXClearBitPlane( Plane : BITPLANE; Color : DWORD );

implementation

procedure DFXClearBitPlane( Plane : BITPLANE; Color : DWORD );
begin
  asm
    mov  ecx, Plane.bitsHgh
    mov  eax, Plane.bitsWdh
    mul  ecx
    mov  ecx, eax
    mov  eax, Color
    mov  edi, Plane.bitsPtr
    rep  stosw
  end;
end;

function DFXCheckSupport : BOOL;
var
  dfx_blitfx : BLITFX;
  rlehdr_temp : RLEHDR;
  bitplane_temp : BITPLANE;
  my_bitplane : BITPLANE;
begin
  my_bitplane.bitsFmt := dfx_pixelformat;
  dfx_blitfx.FXType := BLITFX_BLEND;
  dfx_blitfx.BlendSrcFactor := 0;
  dfx_blitfx.BlendDstFactor := 0;

  rlehdr_temp.PixFmt := dfx_pixelformat;
  rlehdr_temp.DataPtr := nil;
  bitplane_temp.bitsFmt := dfx_pixelformat;
  bitplane_temp.bitsPtr := nil;

  Result := False;

  if ( not digifxCheckSupport( dfx_hnd, DFX_DRAWRLE, @dfx_blitfx, @rlehdr_temp, @my_bitplane ) ) then
    Exit;
  if ( not digifxCheckSupport( dfx_hnd, DFX_DRAWBITPLANE, @dfx_blitfx, @bitplane_temp, @my_bitplane ) ) then
    Exit;

  dfx_blitfx.FXType := BLITFX_TEXTURED;
  rlehdr_temp.PixFmt := dfx_pixelformat;
  rlehdr_temp.DataPtr := nil;

  if ( not digifxCheckSupport( dfx_hnd, DFX_DRAWRLE, @dfx_blitfx, @rlehdr_temp, @my_bitplane ) ) then
    Exit;
  dfx_blitfx.FXType := BLITFX_MONO;
  if ( not digifxCheckSupport( dfx_hnd, DFX_DRAWRECT, @dfx_blitfx, @rlehdr_temp, @my_bitplane ) ) then
    Exit;
  if ( not digifxCheckSupport( dfx_hnd, DFX_DRAWLINE, @dfx_blitfx, @rlehdr_temp, @my_bitplane ) ) then
    Exit;

  Result := True;
end;

function DFXEnumProc( driverinfo : PChar ) : BOOL;
begin
  Result := False;

  dfx_hnd := digifxLoadDriver( driverinfo, dfx_pixelformat );
  if ( dfx_hnd <> 0 ) then
  begin
{$IFDEF DirectX}
    if dfx_pixelformat = PIXFMT_555 then
    begin
      if DDColorMatch( lpDDSBack, clWhite ) <= 32767 then
        exit;
    end
    else if dfx_pixelformat = PIXFMT_565 then
    begin
      if DDColorMatch( lpDDSBack, clWhite ) > 32767 then
        exit;
    end
{$ENDIF}
{$IFNDEF DirectX}
      if ( DFXCheckSupport( ) ) then
      begin
        Exit;
      end;
{$ENDIF}
  end;

  digifxFreeDriver( dfx_hnd );
  dfx_hnd := 0;

  Result := True;
end;

function DFXInit( Path : string ) : BOOL;
begin
  Result := False;

  if not digifxInit( PChar( Path ) ) then
    Exit;

  repeat
    dfx_pixelformat := pixelformats[ pf_index ];
    Inc( pf_index );
    digifxEnumDrivers( @DFXEnumProc );
  until ( dfx_hnd <> 0 ) or ( pf_index = High( pixelformats ) );

  if dfx_pixelformat = PIXFMT_555 then
    Log.LogStatus( 'Using 555 Driver', 'DFXInit' )
  else if dfx_pixelformat = PIXFMT_565 then
    Log.LogStatus( 'Using 565 Driver', 'DFXInit' )
  else if dfx_pixelformat = PIXFMT_888 then
    Log.LogStatus( 'Using 888 Driver', 'DFXInit' );

  Result := ( dfx_hnd <> 0 );
end;

procedure DFXShutdown;
begin
  digifxFreeDriver( dfx_hnd );
  digifxDone( );
end;

{ TRLESprite }

procedure TRLESprite.LoadFromRLE( FileName : string );
var
  TmpFile : THandle;
  Size, BuffSize, BytesCnt, i : DWORD;
  lpRLE, RelocOffset : PChar;
  p : PRLEHDR;
begin
  if Assigned( lpSpr ) then
  begin
    FreeMem( lpSpr.DataPtr );
    FreeMem( lpSpr );
    lpSpr := nil;
  end;

  TmpFile := CreateFile( PChar( FileName ), GENERIC_READ, 0, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0 );
  if ( TmpFile = INVALID_HANDLE_VALUE ) then
    raise Exception.CreateFmt( 'Could not load file %s.', [ FileName ] );

  ReadFile( TmpFile, PicCnt, SizeOf( PicCnt ), BytesCnt, nil );
  ReadFile( TmpFile, BuffSize, SizeOf( BuffSize ), BytesCnt, nil );
  Size := PicCnt * SizeOf( RLEHDR );
  GetMem( lpSpr, Size );
  ReadFile( TmpFile, lpSpr^, Size, BytesCnt, nil );
  GetMem( lpRLE, BuffSize );
  ReadFile( TmpFile, lpRLE^, BuffSize, BytesCnt, nil );
  CloseHandle( TmpFile );

  FMemSize := BuffSize;

  RelocOffset := PChar( lpRLE - lpSpr.DataPtr );
  p := lpSpr;
  for i := 1 to PicCnt do
  begin
    p.DataPtr := PChar( p.DataPtr + DWORD( RelocOffset ) );
    digifxConvertRLE( dfx_hnd, p );
    Inc( p );
  end;
end;

procedure TRLESprite.LoadFromBitmap( BITMAP : TBitmap; FrameWidth, FrameHeight : Integer; Color : TColor );
var
  i, j, k, W, H : Integer;
  BuffSize : DWORD;
  lpRLE : PRLEHDR;
  Bits : BITPLANE;
  TempBitmap : TBitmap;
  ImageSize : Cardinal;
  p : ^Byte;
  C : LongWord;
  R, G, B : Byte;
  Sizes, PSize : ^LongWord;
begin
  if Assigned( lpSpr ) then
  begin
    FreeMem( lpSpr.DataPtr );
    FreeMem( lpSpr );
    lpSpr := nil;
  end;

  W := BITMAP.width div FrameWidth;
  H := BITMAP.Height div FrameHeight;
  PicCnt := W * H;
  BuffSize := PicCnt * SizeOf( RLEHDR );
  GetMem( lpSpr, BuffSize );
  ZeroMemory( lpSpr, BuffSize );
  lpRLE := lpSpr;
  GetMem( Sizes, PicCnt * SizeOf( LongWord ) );

  C := ColorToRGB( Color );
  R := ( C and $FF );
  G := ( C and $FF00 ) shr 8;
  B := ( C and $FF0000 ) shr 16;
  C := digiFXConvertColor( dfx_hnd, R, G, B );

  PSize := Sizes;
  FMemSize := 0;
  for j := 0 to H - 1 do
  begin
    for i := 0 to W - 1 do
    begin
      TempBitmap := TBitmap.Create;
      TempBitmap.width := FrameWidth;
      TempBitmap.Height := FrameHeight;

      BitBlt( TempBitmap.Canvas.Handle, 0, 0, FrameWidth, FrameHeight,
        BITMAP.Canvas.Handle, i * FrameWidth, j * FrameHeight, SRCCOPY );
      TempBitmap.PixelFormat := pf16bit;

      Bits.bitsWdh := FrameWidth;
      Bits.bitsPitch := Bits.bitsWdh * 2;
      Bits.bitsHgh := FrameHeight;
      Bits.bitsFmt := PIXFMT_565;
      GetMem( Bits.bitsPtr, Bits.bitsPitch * Bits.bitsHgh );

      p := Pointer( Bits.bitsPtr );
      for k := 0 to FrameHeight - 1 do
      begin
        CopyMemory( p, TempBitmap.ScanLine[ k ], Bits.bitsPitch );
        Inc( p, Bits.bitsPitch );
      end;

      TempBitmap.Free;

      ImageSize := digifxCreateRLE( dfx_hnd, @Bits, C, lpRLE, nil, lpSpr );
      PSize^ := ImageSize;
      Inc( FMemSize, ImageSize );

      GetMem( p, ImageSize );
      digifxCreateRLE( dfx_hnd, @Bits, C, lpRLE, p, lpSpr );
      lpRLE.AdjX := lpRLE.SrcX;
      lpRLE.AdjY := lpRLE.SrcY;

      Inc( lpRLE );
      FreeMem( Bits.bitsPtr );
      Inc( PSize );
    end;
  end;

  lpRLE := lpSpr;
  PSize := Sizes;
  GetMem( p, FMemSize );
  for i := 1 to PicCnt do
  begin
    CopyMemory( p, lpRLE.DataPtr, PSize^ );
    FreeMem( lpRLE.DataPtr );
    lpRLE.DataPtr := Pointer( p );
    Inc( p, PSize^ );
    Inc( lpRLE );
    Inc( PSize );
  end;

  FreeMem( Sizes );
end;

procedure TRLESprite.SaveToRLE( FileName : string );
var
  TmpFile : THandle;
  BuffSize, BytesCnt : DWORD;
begin
  TmpFile := CreateFile( PChar( FileName ), GENERIC_WRITE, 0, nil, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, 0 );
  if ( TmpFile = INVALID_HANDLE_VALUE ) then
    Exit;

  BuffSize := FMemSize;

  WriteFile( TmpFile, PicCnt, SizeOf( PicCnt ), BytesCnt, nil );
  WriteFile( TmpFile, BuffSize, SizeOf( BuffSize ), BytesCnt, nil );
  WriteFile( TmpFile, lpSpr^, PicCnt * SizeOf( RLEHDR ), BytesCnt, nil );
  WriteFile( TmpFile, lpSpr.DataPtr^, BuffSize, BytesCnt, nil );
  CloseHandle( TmpFile );
end;

constructor TRLESprite.Create;
begin
  inherited;
end;

destructor TRLESprite.Destroy;
begin
  FreeMem( lpSpr.DataPtr );
  FreeMem( lpSpr );
  inherited;
end;

function TRLESprite.GetImage( Index : DWORD ) : PRLEHDR;
begin
  if ( Index < PicCnt ) then
  begin
    Result := lpSpr;
    Inc( Result, Index );
  end
  else
  begin
    Result := nil;
  end;
end;

procedure TRLESprite.Draw( Index : LongWord; X, Y : Integer; Bits : PBITPLANE );
var
  dfx_blitfx : BLITFX;
begin
  if Index >= PicCnt then
    exit;
  dfx_blitfx.FXType := BLITFX_NONE;
  try
    digifxDrawRLE( dfx_hnd, Image[ Index ], X, Y, @dfx_blitfx, Bits );
  except
    Log.LogError( '*** Error drawing resource', 'TRLESprite.Draw' );
  end;
end;

procedure TRLESprite.DrawBlend( Index : LongWord; X, Y : Integer;
  Bits : PBITPLANE; SrcBlend, DstBlend : DWORD );
var
  dfx_blitfx : BLITFX;
begin
  if Index >= PicCnt then
    exit;
  dfx_blitfx.FXType := BLITFX_BLEND;
  dfx_blitfx.BlendSrcFactor := SrcBlend;
  dfx_blitfx.BlendDstFactor := DstBlend;
  digifxDrawRLE( dfx_hnd, Image[ Index ], X, Y, @dfx_blitfx, Bits );
end;

procedure TRLESprite.DrawMono( Index : LongWord; X, Y : Integer;
  Bits : PBITPLANE; Color : TColor );
var
  dfx_blitfx : BLITFX;
  C : Longint;
begin
  if Index >= PicCnt then
    exit;
  C := ColorToRGB( Color );
  dfx_blitfx.FXType := BLITFX_MONO;
  dfx_blitfx.Color.R := ( C and $FF );
  dfx_blitfx.Color.G := ( C and $FF00 ) shr 8;
  dfx_blitfx.Color.B := ( C and $FF0000 ) shr 16;
  digifxDrawRLE( dfx_hnd, Image[ Index ], X, Y, @dfx_blitfx, Bits );
end;

procedure TRLESprite.DrawColorize( Index : longword; X, Y : integer; Bits : PBITPLANE; R, G, B, SrcBlend, DstBlend : integer );
var
  dfx_blitfx : BLITFX;
begin
  if Index >= PicCnt then
    exit;
  dfx_blitfx.SrcRFactor := round( SrcBlend * R / 100 );
  dfx_blitfx.SrcGFactor := round( SrcBlend * G / 100 );
  dfx_blitfx.SrcBFactor := round( SrcBlend * B / 100 );
  dfx_blitfx.DstRFactor := DstBlend;
  dfx_blitfx.DstGFactor := DstBlend;
  dfx_blitfx.DstBFactor := DstBlend;
  dfx_blitfx.FXType := BLITFX_COLORIZE;
  digifxDrawRLE( dfx_hnd, Image[ Index ], X, Y, @dfx_blitfx, Bits );
end;

procedure TRLESprite.DrawAntiAlias( Index : longword; X, Y : integer; Bits : PBITPLANE );
var
  dfx_blitfx : BLITFX;
begin
  if Index >= PicCnt then
    exit;
  dfx_blitfx.FXType := BLITFX_HENDS + BLITFX_SOFTEN;
  digifxDrawRLE( dfx_hnd, Image[ Index ], X, Y, @dfx_blitfx, Bits );
end;

procedure TRLESprite.DrawSoften( Index : longword; X, Y : integer;
  Bits : PBITPLANE );
var
  dfx_blitfx : BLITFX;
begin
  if Index >= PicCnt then
    exit;
  dfx_blitfx.FXType := BLITFX_SOFTEN;
  digifxDrawRLE( dfx_hnd, Image[ Index ], X, Y, @dfx_blitfx, Bits );
end;

procedure TRLESprite.LoadFromStream( Stream : TStream );
var
  Size, BuffSize, i : DWORD;
  lpRLE, RelocOffset : PChar;
  p : PRLEHDR;
begin
  if Assigned( lpSpr ) then
  begin
    FreeMem( lpSpr.DataPtr );
    FreeMem( lpSpr );
    lpSpr := nil;
  end;

  Stream.Read( PicCnt, SizeOf( PicCnt ) );
  Stream.Read( BuffSize, SizeOf( BuffSize ) );
  Size := PicCnt * SizeOf( RLEHDR );
  GetMem( lpSpr, Size );
  Stream.Read( lpSpr^, Size );
  GetMem( lpRLE, BuffSize );
  Stream.Read( lpRLE^, BuffSize );

  FMemSize := BuffSize;

  RelocOffset := PChar( lpRLE - lpSpr.DataPtr );
  p := lpSpr;
  for i := 1 to PicCnt do
  begin
    p.DataPtr := PChar( p.DataPtr + DWORD( RelocOffset ) );
    digifxConvertRLE( dfx_hnd, p );
    Inc( p );
  end;
end;

procedure TRLESprite.SaveToStream( Stream : TStream );
var
  BuffSize : DWORD;
begin
  BuffSize := FMemSize;

  Stream.Write( PicCnt, SizeOf( PicCnt ) );
  Stream.Write( BuffSize, SizeOf( BuffSize ) );
  Stream.Write( lpSpr^, PicCnt * SizeOf( RLEHDR ) );
  Stream.Write( lpSpr.DataPtr^, BuffSize );
end;

procedure TRLESprite.LoadMoreFromBitPlane( BitPlane : TBitPlane );
var
  i : Integer;
  BuffSize : DWORD;
  lpRLE : PRLEHDR;
  ImageSize : Cardinal;
  p : ^Byte;
  MoreCount, NewCount : longword;
  OldMemSize : integer;
  OldDataPtr : ^Byte;
  Offset : integer;
begin
  MoreCount := 1;
  NewCount := PicCnt + MoreCount;
  BuffSize := NewCount * SizeOf( RLEHDR );
  ReallocMem( lpSpr, BuffSize );
  lpRLE := lpSpr;
  inc( lpRLE, PicCnt );
  ZeroMemory( lpRLE, MoreCount * SizeOf( RLEHDR ) );

  ImageSize := digifxCreateRLE( dfx_hnd, BitPlane.Bits, BitPlane.FKeyIndex, lpRLE, nil, lpSpr );

  GetMem( p, ImageSize );
  digifxCreateRLE( dfx_hnd, BitPlane.Bits, BitPlane.FKeyIndex, lpRLE, p, lpSpr );
  lpRLE.AdjX := lpRLE.SrcX;
  lpRLE.AdjY := lpRLE.SrcY;

  lpRLE := lpSpr;
  OldMemSize := FMemSize;
  inc( FMemSize, ImageSize );
  OldDataPtr := pointer( lpSpr.DataPtr );
  ReallocMem( lpSpr.DataPtr, FMemSize );
  p := pointer( lpSpr.DataPtr );
  Offset := longword( p ) - longword( OldDataPtr );
  inc( lpRLE );
  for i := 2 to PicCnt do
  begin
    inc( lpRLE.DataPtr, Offset );
    inc( lpRLE );
  end;

  inc( p, OldMemSize );
  CopyMemory( p, lpRLE.DataPtr, ImageSize );
  FreeMem( lpRLE.DataPtr );
  lpRLE.DataPtr := Pointer( p );

  PicCnt := NewCount;
end;

procedure TRLESprite.LoadFromBitPlane( BitPlane : TBitPlane );
begin
  if Assigned( lpSpr ) then
  begin
    FreeMem( lpSpr.DataPtr );
    FreeMem( lpSpr );
    lpSpr := nil;
  end;

  PicCnt := 1;
  GetMem( lpSpr, SizeOf( RLEHDR ) );
  ZeroMemory( lpSpr, SizeOf( RLEHDR ) );

  FMemSize := digifxCreateRLE( dfx_hnd, BitPlane.Bits, BitPlane.FKeyIndex, lpSpr, nil, lpSpr );
  GetMem( lpSpr.DataPtr, FMemSize );
  digifxCreateRLE( dfx_hnd, BitPlane.Bits, BitPlane.FKeyIndex, lpSpr, lpSpr.DataPtr, lpSpr );
  lpSpr.AdjX := lpSpr.SrcX;
  lpSpr.AdjY := lpSpr.SrcY;
end;

procedure TRLESprite.LoadFromBitPlaneBits( Bits : PBITPLANE; Color : TColor );
var
  C, R, G, B, KeyIndex : integer;
begin
  if Assigned( lpSpr ) then
  begin
    FreeMem( lpSpr.DataPtr );
    FreeMem( lpSpr );
    lpSpr := nil;
  end;

  PicCnt := 1;
  GetMem( lpSpr, SizeOf( RLEHDR ) );
  ZeroMemory( lpSpr, SizeOf( RLEHDR ) );

  C := ColorToRGB( Color );
  R := ( C and $FF );
  G := ( C and $FF00 ) shr 8;
  B := ( C and $FF0000 ) shr 16;
  KeyIndex := digiFXConvertColor( dfx_hnd, R, G, B );

  FMemSize := digifxCreateRLE( dfx_hnd, Bits, KeyIndex, lpSpr, nil, lpSpr );
  GetMem( lpSpr.DataPtr, FMemSize );
  digifxCreateRLE( dfx_hnd, Bits, KeyIndex, lpSpr, lpSpr.DataPtr, lpSpr );
  lpSpr.AdjX := lpSpr.SrcX;
  lpSpr.AdjY := lpSpr.SrcY;
end;

{ TBitPlane }

procedure TBitPlane.Clear;
begin
  DFXClearBitPlane( FBits, FKeyIndex );
end;

procedure TBitPlane.Fill( Color : TColor );
var
  C : LongWord;
  R, G, B : Byte;
begin
  C := ColorToRGB( Color );
  R := ( C and $FF );
  G := ( C and $FF00 ) shr 8;
  B := ( C and $FF0000 ) shr 16;
  C := digiFXConvertColor( dfx_hnd, R, G, B );
  DFXClearBitPlane( FBits, C );
end;

constructor TBitPlane.Create( Width, Height : Integer );
begin
  inherited Create;
  ZeroMemory( @FBits, SizeOf( BITPLANE ) );
  FBits.bitsWdh := Width;
  FBits.bitsPitch := FBits.bitsWdh * 2;
  FBits.bitsHgh := Height;
  FBits.bitsFmt := dfx_pixelformat;
  GetMem( FBits.bitsPtr, FBits.bitsPitch * FBits.bitsHgh );
end;

destructor TBitPlane.Destroy;
begin
  FreeMem( FBits.bitsPtr );
  inherited;
end;

procedure TBitPlane.DrawToDC( DC : HDC; X, Y : Integer );
var
  BITMAPINFO : PBitmapInfo;
  biSize : Integer;
  p : ^longword;
begin
  biSize := SizeOf( TBitmapInfoHeader );
  GetMem( BITMAPINFO, biSize + 12 );
  ZeroMemory( @BITMAPINFO.bmiHeader, biSize );
  BITMAPINFO.bmiHeader.biSize := biSize;
  BITMAPINFO.bmiHeader.biWidth := FBits.bitsWdh;
  BITMAPINFO.bmiHeader.biHeight := FBits.bitsHgh;
  BITMAPINFO.bmiHeader.biHeight := -BITMAPINFO.bmiHeader.biHeight;
  BITMAPINFO.bmiHeader.biPlanes := 1;
  if dfx_pixelformat = PIXFMT_555 then
  begin
    BITMAPINFO.bmiHeader.biBitCount := 16;
    BITMAPINFO.bmiHeader.biCompression := BI_RGB;
  end
  else if dfx_pixelformat = PIXFMT_565 then
  begin
    BITMAPINFO.bmiHeader.biBitCount := 16;
    BITMAPINFO.bmiHeader.biCompression := BI_BITFIELDS;
    p := @BITMAPINFO.bmiColors[ 0 ];
    p^ := $F800; inc( p );
    p^ := $07E0; inc( p );
    p^ := $001F;
  end
  else if dfx_pixelformat = PIXFMT_888 then
  begin
    BITMAPINFO.bmiHeader.biBitCount := 24;
    BITMAPINFO.bmiHeader.biCompression := BI_RGB;
  end;
  SetDIBitsToDevice( DC, X, Y, FBits.bitsWdh, FBits.bitsHgh,
    0, 0, 0, FBits.bitsHgh, FBits.bitsPtr, BITMAPINFO^, DIB_RGB_COLORS );
  FreeMem( BITMAPINFO );
end;

procedure TBitPlane.Draw( X, Y : Integer; Bits : PBITPLANE );
var
  dfx_blitfx : BLITFX;
begin
  dfx_blitfx.FXType := BLITFX_NONE;
  digifxDrawBitPlane( dfx_hnd, @FBits, X, Y, FKeyIndex, @dfx_blitfx, Bits );
end;

function TBitPlane.GetBits : PBITPLANE;
begin
  Result := @FBits;
end;

function TBitPlane.GetHeight : LongWord;
begin
  Result := FBits.bitsWdh;
end;

function TBitPlane.GetWidth : LongWord;
begin
  Result := FBits.bitsHgh;
end;

procedure TBitPlane.SetKeyColor( const Value : TColor );
var
  C : LongWord;
begin
  FKeyColor := Value;
  C := ColorToRGB( Value );
  R := ( C and $FF );
  G := ( C and $FF00 ) shr 8;
  B := ( C and $FF0000 ) shr 16;
  FKeyIndex := digiFXConvertColor( dfx_hnd, R, G, B );
end;

procedure TBitPlane.DrawMono( X, Y : Integer; Bits : PBITPLANE; Color : TColor );
var
  dfx_blitfx : BLITFX;
  C : DWORD;
begin
  C := ColorToRGB( Color );
  dfx_blitfx.Color.R := ( C and $FF );
  dfx_blitfx.Color.G := ( C and $FF00 ) shr 8;
  dfx_blitfx.Color.B := ( C and $FF0000 ) shr 16;
  dfx_blitfx.FXType := BLITFX_MONO;
  digifxDrawBitPlane( dfx_hnd, @FBits, X, Y, FKeyIndex, @dfx_blitfx, Bits );
end;

procedure TBitPlane.DrawBlend( X, Y : Integer; Bits : PBITPLANE; SrcBlend,
  DstBlend : DWORD );
var
  dfx_blitfx : BLITFX;
begin
  dfx_blitfx.FXType := BLITFX_BLEND;
  dfx_blitfx.BlendSrcFactor := SrcBlend;
  dfx_blitfx.BlendDstFactor := DstBlend;
  digifxDrawBitPlane( dfx_hnd, @FBits, X, Y, FKeyIndex, @dfx_blitfx, Bits );
end;

procedure TBitPlane.DrawOutline( X, Y : integer; Bits : PBITPLANE;
  Color : TColor; Copy : boolean );
var
  dfx_blitfx : BLITFX;
  C : DWORD;
begin
  C := ColorToRGB( Color );
  dfx_blitfx.Color.R := ( C and $FF );
  dfx_blitfx.Color.G := ( C and $FF00 ) shr 8;
  dfx_blitfx.Color.B := ( C and $FF0000 ) shr 16;
  dfx_blitfx.FXType := BLITFX_OUTLINE;
  if not Copy then
    inc( dfx_blitfx.FXType, BLITFX_MONO );
  digifxDrawBitPlane( dfx_hnd, @FBits, X, Y, FKeyIndex, @dfx_blitfx, Bits );
end;

procedure TBitPlane.DrawColorize( X, Y : integer; Bits : PBITPLANE;
  R, G, B, SrcBlend, DstBlend : integer );
var
  dfx_blitfx : BLITFX;
begin
  dfx_blitfx.SrcRFactor := ( SrcBlend * R ) div 100;
  dfx_blitfx.SrcGFactor := ( SrcBlend * G ) div 100;
  dfx_blitfx.SrcBFactor := ( SrcBlend * B ) div 100;
  dfx_blitfx.DstRFactor := DstBlend;
  dfx_blitfx.DstGFactor := DstBlend;
  dfx_blitfx.DstBFactor := DstBlend;
  dfx_blitfx.FXType := BLITFX_COLORIZE;
  digifxDrawBitPlane( dfx_hnd, @FBits, X, Y, FKeyIndex, @dfx_blitfx, Bits );
end;

procedure TBitPlane.DrawShadow( X, Y : integer; Bits : PBITPLANE;
  SrcBlend, DstBlend : DWORD; Angle, Height : integer );
var
  dfx_blitfx : BLITFX;
begin
  dfx_blitfx.FXType := BLITFX_ROTATE + BLITFX_ZOOM + BLITFX_BLEND;
  dfx_blitfx.BlendSrcFactor := SrcBlend;
  dfx_blitfx.BlendDstFactor := DstBlend;
  dfx_blitfx.Angle := Angle;
  dfx_blitfx.ZoomX := 100;
  dfx_blitfx.ZoomY := Height;
  digifxDrawBitPlane( dfx_hnd, @FBits, X, Y, FKeyIndex, @dfx_blitfx, Bits );
end;

procedure TBitPlane.DrawAntiAlias( X, Y : integer; Bits : PBITPLANE );
var
  dfx_blitfx : BLITFX;
begin
  dfx_blitfx.FXType := BLITFX_HENDS + BLITFX_SOFTEN;
  digifxDrawBitPlane( dfx_hnd, @FBits, X, Y, FKeyIndex, @dfx_blitfx, Bits );
end;

end.

