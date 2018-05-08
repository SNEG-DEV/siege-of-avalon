unit AniDec30;
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
  Windows,
  Classes,
  Graphics,
  SysUtils,
  Controls,
  LogFile;

const
  MaxItems = 2047;
  MaxTiles = 2047;
  ItemListSize = 32767;
  MaxScripts = 128;
  MaxScriptFrames = 64;
  MaxZones = 255;
  MaxLightStates = 4;
  WorkWidth = 384;
  WorkHeight = 160;
  MaxSubMaps = 127;
  MaxZoneHeight = 2048;

type
  PGridInfo = ^GridInfo;
  GridInfo = packed record
    Figure : Pointer; //For collision detection
    FilterID : Smallint;
    TriggerID : Smallint;
    CollisionMask : Word;
    LineOfSightMask : Word;
    FilterMask : Word;
    TriggerMask : Word;
    Tile : array[ 0..4 ] of Word;
    Zone : array[ 0..4 ] of Byte;
    BitField : Byte; //Bit 7 denotes a diamond tile, Bit 6 is automap.
  end;

  PTileInfo = ^TileInfo;
  TileInfo = packed record
    ImageIndex : Word;
    Rows : Word;
    Columns : Word;
    Zone : Word;
    Element : Byte;
    Reserved : Byte;
  end;

  MapColumnHeaderInfo = packed record
    BaseLine : Longint;
    Top : Longint;
    Active : Boolean;
    Reserved : Byte;
  end;

  RowUpdateInfo = packed record
    Figure : Pointer; //The first figure on the row
    OverlapRow : Longint; //The last row that contains an item which could overlap this row
    DescendRow : Longint; //The first row which has an item that descends below its position to this row
    MaxHeight : Longint; //The tallest item on this row
    ItemIndex : Word; //The first item on the row
  end;

  PItemInfo = ^ItemInfo;
  ItemInfo = packed record
    Top : Longint;
    Left : Longint;
    Slope : Single;
    StripHeights : HGLOBAL;
    CollisionMasks : HGLOBAL;
    LineOfSightMasks : HGLOBAL;
    LightPoints : HGLOBAL;
    Width : Word;
    Height : Word;
    Strips : Word; //=roundup(Width/TileWidth)  Strips*Height=next Items top
    StripCount : Word;
    Used : Boolean;
    Visible : Boolean;
    AutoTransparent : Boolean;
    Vertical : Boolean;
  end;

  PItemInstanceInfo = ^ItemInstanceInfo;
  ItemInstanceInfo = packed record
    X : Longint;
    Y : Longint;
    ImageY : Word;
    Slope0 : Single;
    Slope1 : Single;
    Slope2 : Single;
    RefItem : word;
    FilterID : Smallint;
    XRayID : Smallint;
    ImageX : Word;
    Width : Word;
    Height : Word;
    VHeight : Word; //Height of region that may obscure objects behind it
    Next : Word;
    Zone : Word;
    AutoTransparent : Boolean;
    Visible : Boolean;
    Last : Boolean;
    Vertical : Boolean;
  end;

  ScriptInfo = packed record
    Frames : Word;
    FrameID : array[ 1..MaxScriptFrames ] of Word;
    Name : string[ 32 ];
    Multiplier : Word;
    Tag : Longint;
  end;

  BITMAP = record
    bmType : Longint;
    bmWidth : Longint;
    bmHeight : Longint;
    bmWidthBytes : Longint;
    bmPlanes : Integer;
    bmBitsPixel : Integer;
    bmBits : Pointer;
  end;

  TPixelFormat = ( pf555, pf565, pf888 );

procedure CreateMask( var Picture, Mask : HBITMAP; BITMAP : TBitmap; Color : TColor );
procedure CreateHighlightMask( Mask : HBITMAP; var HLMask : HBITMAP; W, H : Word );
procedure CreateHighlight( HLMask : HBITMAP; var HLPicture : HBITMAP; HLColor : TColor );
procedure GetStripHeights( var StripHeights : HGLOBAL; Mask : HBITMAP; W, H, StripWidth : Word );
function CreateShadowBrush : HBRUSH;
function Min( A, B : Single ) : Single;
function ATan( X, Y : Single ) : Single;
procedure Clip( ClipX1, ClipX2 : Integer; var DestX1, DestX2, SrcX1, SrcX2 : Integer );
procedure Clip1( ClipX1, ClipX2 : Integer; var DestX1, SrcX1, SrcX2 : Integer );
procedure Clip2( ClipX1, ClipX2 : Integer; var DestX1, SrcX1, W : Integer );

implementation

procedure CreateMask( var Picture, Mask : HBITMAP; BITMAP : TBitmap; Color : TColor );
var
  TempBitmap : TBitmap;
  DC, MaskDC : HDC;
  OldPicture, OldMask : HBITMAP;
  OldPalette : HPALETTE;
  ScreenDC : HDC;
begin
  if ( Picture <> 0 ) then
    DeleteObject( Picture );
  Picture := 0;
  if ( Mask <> 0 ) then
    DeleteObject( Mask );
  Mask := 0;

  TempBitmap := TBitmap.Create;
  TempBitmap.Assign( BITMAP );
  TempBitmap.TRANSPARENT := True;
  TempBitmap.TransparentMode := tmFixed;
  TempBitmap.TransparentColor := Color;
  TempBitmap.Canvas.Pen.Color := clBlack;
  ScreenDC := GetDC( 0 );

  DC := CreateCompatibleDC( ScreenDC );
  OldPalette := SelectPalette( DC, TempBitmap.Palette, False );
  MaskDC := CreateCompatibleDC( ScreenDC );
  Picture := CreateCompatibleBitmap( ScreenDC, BITMAP.width, BITMAP.Height );
  ReleaseDC( 0, ScreenDC );
  Mask := TempBitmap.ReleaseMaskHandle;

  OldMask := SelectObject( MaskDC, Mask );
  OldPicture := SelectObject( DC, Picture );
  BitBlt( DC, 0, 0, BITMAP.width, BITMAP.Height, BITMAP.Canvas.Handle, 0, 0, SRCCOPY );
  PatBlt( MaskDC, 0, 0, BITMAP.width, BITMAP.Height, DSTINVERT );
  BitBlt( DC, 0, 0, BITMAP.width, BITMAP.Height, MaskDC, 0, 0, SRCAND );
  PatBlt( MaskDC, 0, 0, BITMAP.width, BITMAP.Height, DSTINVERT );

  SelectPalette( DC, OldPalette, False );
  SelectObject( MaskDC, OldMask );
  SelectObject( DC, OldPicture );

  TempBitmap.Free;

  DeleteDC( MaskDC );
  DeleteDC( DC );
end;

procedure CreateHighlight( HLMask : HBITMAP; var HLPicture : HBITMAP; HLColor : TColor );
var
  BITMAP, MaskBMP : TBitmap;
begin
  if ( HLPicture <> 0 ) then
  begin
    DeleteObject( HLPicture );
    HLPicture := 0;
  end;
  MaskBMP := TBitmap.Create;
  MaskBMP.Handle := HLMask;
  BITMAP := TBitmap.Create;
  BITMAP.width := MaskBMP.width;
  BITMAP.Height := MaskBMP.Height;
  BITMAP.Canvas.Brush.Color := ( ColorToRGB( HLColor ) xor $FFFFFF );
  PatBlt( BITMAP.Canvas.Handle, 0, 0, BITMAP.width, BITMAP.Height, PATCOPY );
  BITMAP.Canvas.Brush.Color := clWhite;
  BitBlt( BITMAP.Canvas.Handle, 0, 0, BITMAP.width, BITMAP.Height, MaskBMP.Canvas.Handle, 0, 0, NOTSRCERASE );
  MaskBMP.ReleaseHandle;
  MaskBMP.Free;
  HLPicture := BITMAP.ReleaseHandle;
  BITMAP.Free;
end;

function Min( A, B : Single ) : Single;
begin
  if ( A < B ) then
    Result := A
  else
    Result := B;
end;

function CreateShadowBrush : HBRUSH;
var
  DC, TempDC : HDC;
  NewBitmap, OldBitmap : HBITMAP;
  i, j : Word;
  BitOn : Boolean;
begin
  TempDC := GetDC( 0 );
  DC := CreateCompatibleDC( TempDC );
  ReleaseDC( 0, TempDC );
  NewBitmap := CreateCompatibleBitmap( DC, 8, 8 );
  OldBitmap := SelectObject( DC, NewBitmap );

  PatBlt( DC, 0, 0, 8, 8, BLACKNESS );
  BitOn := False;
  for j := 0 to 7 do
  begin
    for i := 0 to 7 do
    begin
      if BitOn then
        SetPixelV( DC, i, j, clWhite );
      BitOn := not BitOn;
    end;
    BitOn := not BitOn;
  end;
  SelectObject( DC, OldBitmap );
  DeleteDC( DC );
  Result := CreatePatternBrush( NewBitmap );
  DeleteObject( NewBitmap );
end;

procedure GetStripHeights( var StripHeights : HGLOBAL; Mask : HBITMAP; W, H, StripWidth : Word );
var
  bmi : ^TBitmapInfo;
  ghBitmapInfo : HGLOBAL;
  DC : HDC;
  RowSize : Longint;
  hBits : HGLOBAL;
  BitsBase, Bits : ^Byte;
  BitOffset, ByteOffset : Integer;
  Strips : Integer;
  i, j, k : Integer;
  BytesCovered : Integer;
  BitMask, EndBits : Byte;
  MaxBit : Word;
  StripData : ^Word;
const
  FailName : string = 'AniDec30.GetStripHeights';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    DbgLog.LogEntry( FailName );
{$ENDIF}
  try

    RowSize := W div 8;
    if ( ( W mod 8 ) <> 0 ) then
      Inc( RowSize );
    if ( ( RowSize mod 4 ) <> 0 ) then
      Inc( RowSize, 4 - ( RowSize mod 4 ) );
    Strips := W div StripWidth;
    if ( ( W mod StripWidth ) <> 0 ) then
      Inc( Strips );
    StripHeights := GlobalAlloc( GHND, Strips * SizeOf( Word ) );
    StripData := GlobalLock( StripHeights );
    hBits := GlobalAlloc( GPTR, H * RowSize );
    BitsBase := GlobalLock( hBits );
    ghBitmapInfo := GlobalAlloc( GPTR, SizeOf( TBitmapInfoHeader ) + 1024 );
    bmi := GlobalLock( ghBitmapInfo );
    bmi^.bmiHeader.biSize := SizeOf( TBitmapInfoHeader );
    bmi^.bmiHeader.biPlanes := 1;
    bmi^.bmiHeader.biWidth := W;
    bmi^.bmiHeader.biHeight := H;
    bmi^.bmiHeader.biBitCount := 1;
    bmi^.bmiHeader.biCompression := BI_RGB;
    DC := GetDC( 0 );
    GetDIBits( DC, Mask, 0, H, BitsBase, bmi^, DIB_RGB_COLORS );
    ReleaseDC( 0, DC );

    for i := 1 to Strips do
    begin
      ByteOffset := ( ( i - 1 ) * StripWidth ) div 8;
      BitOffset := ( ( i - 1 ) * StripWidth ) mod 8;
      BytesCovered := ( ( StripWidth + BitOffset ) div 8 );
      if ( ( ( StripWidth + BitOffset ) mod 8 ) <> 0 ) then
        Inc( BytesCovered );
      MaxBit := 0;
      for j := 1 to BytesCovered do
      begin
        BitMask := $FF;
        if ( j = 1 ) then
        begin
          if ( StripWidth < 8 ) then
          begin
            BitMask := not ( ( 1 shl StripWidth ) - 1 );
          end;
          BitMask := BitMask shr BitOffset;
        end;
        if ( j = BytesCovered ) then
        begin
          if ( ( ( StripWidth + BitOffset ) mod 8 ) <> 0 ) then
          begin
            EndBits := not ( 1 shl ( 8 - ( ( StripWidth + BitOffset ) mod 8 ) ) - 1 );
            BitMask := BitMask and EndBits;
          end;
        end;
        if ( i = Strips ) then
        begin
          if ( ( ByteOffset + j ) * 8 > W ) then
          begin
            if ( ( ByteOffset + j ) * 8 > W + 8 ) then
              BitMask := 0
            else
            begin
              EndBits := not ( 1 shl ( 8 - ( W mod 8 ) ) - 1 );
              BitMask := BitMask and EndBits;
            end;
          end;
        end;
        Bits := BitsBase;
        Inc( Bits, ByteOffset + j - 1 );
        for k := H downto 1 do
        begin
          if ( ( Bits^ and BitMask ) <> BitMask ) then
          begin
            if ( k > MaxBit ) then
              MaxBit := k;
            Break;
          end;
          Inc( Bits, RowSize );
        end;
      end;
      StripData^ := MaxBit;
      Inc( StripData );
    end;

    GlobalUnlock( hBits );
    GlobalFree( hBits );
    GlobalUnlock( ghBitmapInfo );
    GlobalFree( ghBitmapInfo );
    GlobalUnlock( StripHeights );

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure CreateHighlightMask( Mask : HBITMAP; var HLMask : HBITMAP; W, H : Word );
var
  bmi : ^TBitmapInfo;
  ghBitmapInfo : HGLOBAL;
  hBitsIn, hBitsOut : HGLOBAL;
  BitsBaseIn, BitsIn, BitsBaseOut, BitsOut : ^Byte;
  DC, TempDC : HDC;
  TempBitmap : HBITMAP;
  RowSize : Longint;
  i, j : Integer;
  InByte, OutByte, BitMask : Byte;
  C, CPrev : Boolean;
begin
  RowSize := W div 8;
  if ( ( W mod 8 ) <> 0 ) then
    Inc( RowSize );
  if ( ( RowSize mod 4 ) <> 0 ) then
    Inc( RowSize, 4 - ( RowSize mod 4 ) );
  hBitsIn := GlobalAlloc( GPTR, H * RowSize );
  BitsBaseIn := GlobalLock( hBitsIn );
  ghBitmapInfo := GlobalAlloc( GPTR, SizeOf( TBitmapInfoHeader ) + 1024 );
  bmi := GlobalLock( ghBitmapInfo );
  bmi^.bmiHeader.biSize := SizeOf( TBitmapInfoHeader );
  bmi^.bmiHeader.biPlanes := 1;
  bmi^.bmiHeader.biWidth := W;
  bmi^.bmiHeader.biHeight := H;
  bmi^.bmiHeader.biBitCount := 1;
  bmi^.bmiHeader.biCompression := BI_RGB;
  DC := GetDC( 0 );
  SetTextColor( DC, clBlack );
  SetBkColor( DC, clWhite );
  GetDIBits( DC, Mask, 0, H, BitsBaseIn, bmi^, DIB_RGB_COLORS );
  ReleaseDC( 0, DC );

  hBitsOut := GlobalAlloc( GPTR, H * RowSize );
  BitsBaseOut := GlobalLock( hBitsOut );

  //Scan horizontally for edges
  for j := 1 to H do
  begin
    BitsOut := BitsBaseOut;
    Inc( BitsOut, ( j - 1 ) * RowSize );
    BitsIn := BitsBaseIn;
    Inc( BitsIn, ( j - 1 ) * RowSize );
    InByte := BitsIn^;
    OutByte := $FF;
    BitMask := $80;
    CPrev := True;
    for i := 1 to W do
    begin
      C := ( ( InByte and BitMask ) = BitMask );
      if ( ( not C ) and CPrev ) then
      begin
        if ( BitMask = $80 ) then
        begin
          if ( i > 1 ) then
          begin
            OutByte := ( OutByte and $FE );
            BitsOut^ := OutByte;
            Inc( BitsOut );
            OutByte := $FF;
          end;
        end
        else
          OutByte := ( OutByte and ( not ( BitMask shl 1 ) ) );
      end
      else if ( C and ( not CPrev ) ) then
      begin
        if ( BitMask = $80 ) then
        begin
          if ( i > 1 ) then
          begin
            BitsOut^ := OutByte;
            Inc( BitsOut );
            OutByte := $7F;
          end;
        end
        else
          OutByte := ( OutByte and ( not BitMask ) );
      end
      else
      begin
        if ( BitMask = $80 ) then
        begin
          if ( i > 1 ) then
          begin
            BitsOut^ := OutByte;
            Inc( BitsOut );
            OutByte := $FF;
          end;
        end
      end;
      CPrev := C;

      BitMask := ( BitMask shr 1 );
      if ( BitMask = 0 ) then
      begin
        Inc( BitsIn );
        InByte := BitsIn^;
        BitMask := $80;
      end;
    end;
    BitsOut^ := OutByte;
  end;

  //Scan Vertically
  for i := 1 to W do
  begin
    BitMask := ( $80 shr ( ( i - 1 ) mod 8 ) );
    BitsIn := BitsBaseIn;
    Inc( BitsIn, ( i - 1 ) div 8 );
    BitsOut := BitsBaseOut;
    Inc( BitsOut, ( i - 1 ) div 8 );
    CPrev := True;
    for j := 1 to H do
    begin
      C := ( ( BitsIn^ and BitMask ) = BitMask );
      if ( ( not C ) and CPrev ) then
      begin
        if ( j > 1 ) then
        begin
          Dec( BitsOut, RowSize );
          BitsOut^ := ( BitsOut^ and ( not BitMask ) );
          Inc( BitsOut, RowSize );
        end;
      end
      else if ( C and ( not CPrev ) ) then
      begin
        BitsOut^ := ( BitsOut^ and ( not BitMask ) );
      end;
      CPrev := C;

      Inc( BitsIn, RowSize );
      Inc( BitsOut, RowSize );
    end;
  end;

  DC := GetDC( 0 );
  TempDC := CreateCompatibleDC( DC );
  ReleaseDC( 0, DC );
  TempBitmap := SelectObject( TempDC, CreateCompatibleBitmap( TempDC, W, H ) );
  SetDIBitsToDevice( TempDC, 0, 0, W, H, 0, 0, 0, H, BitsBaseOut, bmi^, DIB_RGB_COLORS );
  HLMask := SelectObject( TempDC, TempBitmap );
  DeleteDC( TempDC );

  GlobalUnlock( hBitsIn );
  GlobalFree( hBitsIn );
  GlobalUnlock( hBitsOut );
  GlobalFree( hBitsOut );
  GlobalUnlock( ghBitmapInfo );
  GlobalFree( ghBitmapInfo );

end;

function ATan( X, Y : Single ) : Single;
begin
  if ( X = 0 ) then
  begin
    if ( Y >= 0 ) then
      Result := PI / 2
    else
      Result := 3 * PI / 2;
  end
  else if ( X > 0 ) then
  begin
    if ( Y >= 0 ) then
      Result := ArcTan( Y / X )
    else
      Result := ArcTan( Y / X ) + 2 * PI;
  end
  else
  begin
    Result := ArcTan( Y / X ) + PI;
  end;
  if Result < 0 then
    Result := Result + 2 * PI;
end;

procedure Clip( ClipX1, ClipX2 : Integer; var DestX1, DestX2, SrcX1, SrcX2 : Integer );
begin
  if ( DestX1 < ClipX1 ) then
  begin
    Inc( SrcX1, ClipX1 - DestX1 );
    DestX1 := ClipX1;
  end;
  if ( DestX2 > ClipX2 ) then
  begin
    Dec( SrcX2, DestX2 - ClipX2 );
    DestX2 := ClipX2;
  end;
end;

procedure Clip1( ClipX1, ClipX2 : Integer; var DestX1, SrcX1, SrcX2 : Integer );
begin
  if ( DestX1 < ClipX1 ) then
  begin
    Inc( SrcX1, ClipX1 - DestX1 );
    DestX1 := ClipX1;
  end;
  if ( DestX1 + ( SrcX2 - SrcX1 ) > ClipX2 ) then
  begin
    SrcX2 := SrcX1 + ClipX2 - DestX1;
  end;
end;

procedure Clip2( ClipX1, ClipX2 : Integer; var DestX1, SrcX1, W : Integer );
begin
  if ( DestX1 < ClipX1 ) then
  begin
    Dec( W, ClipX1 - DestX1 );
    Inc( SrcX1, ClipX1 - DestX1 );
    DestX1 := ClipX1;
  end;
  if ( DestX1 + W > ClipX2 ) then
  begin
    W := ClipX2 - DestX1;
  end;
end;

end.
