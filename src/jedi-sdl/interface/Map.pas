unit Map;
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
  SysUtils,
  Classes,
  Controls,
  Forms,
  Windows,
  Gametext,
  Graphics,
{$IFDEF DX5}
  DirectX,
{$ELSE}
  DirectDraw,
{$ENDIF}
  Anigrp30,
  AniDec30,
  Engine,
  DXEffects,
  DXUtil,
  Character,
  Display,
  LogFile;

type
  TMap = class( TDisplay )
  private
    FOnClose : TNotifyEvent;
    DXBack : IDirectDrawSurface;
    DXDirty : IDirectDrawSurface;
    MouseOverBack : boolean;
    ShowAll : boolean;
  protected
    procedure MouseDown( Sender : TAniview; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
    procedure MouseMove( Sender : TAniview;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
  public
    Map : TAniMap;
    Character : TCharacter;
    CharacterList : TList;
    MapName : string;
    procedure Init; override;
    property OnClose : TNotifyEvent read FOnClose write FOnClose;
  end;

implementation

uses
  AniDemo;

{ TMap }

procedure TMap.Init;
var
  Image : IDirectDrawSurface;
  BM : TBitmap;
  W, H : integer;
  OffsetX, OffsetY : integer;
  i, j, k : longint;
  Tile : PGridInfo;
  p, p1 : ^word;
  pAlpha, p1Alpha : ^word;
  AlphaR, AlphaG, AlphaB : word;
  CollisionMask, LineOfSightMask : word;
  ddsd, ddsdImage : TDDSurfaceDesc;
  x, y : integer;
  C : word;
  RShift, GBits, RMask, GMask : integer;
  R, G, B : byte;
  GDark, GLight : integer;
  Edge, Edge1 : boolean;
  Seen : boolean;
const
  FailName : string = 'TMap.Init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
    X1 := 0;
    Y1 := 0;
    X2 := 700;
    Y2 := 500;

    MouseOverBack := false;
    MouseCursor.Cleanup;
    WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, ResWidth, ResHeight ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;

    BM := TBitmap.create;
    BM.LoadFromFile( InterfacePath + 'MapBack.bmp' );
    DXBack := DDGetImage( lpDD, BM, clAqua, false );
    DXDirty := DDGetImage( lpDD, BM, clAqua, false );

    BM.LoadFromFile( InterfacePath + 'MapMaskedShadowLayer.bmp' );
    Image := DDGetImage( lpDD, BM, clBlack, false );
    try
      if assigned( Image ) then
      begin
        DrawSub( lpDDSBack, Rect( 0, 0, BM.Width, BM.Height ), Rect( 0, 0, BM.Width, BM.Height ), Image, True, 170 );
      end;
    finally
      Image := nil;
    end;

    BM.LoadFromFile( InterfacePath + 'MapColorLayer.bmp' );
    Image := DDGetImage( lpDD, BM, clAqua, true );
    try
      if assigned( Image ) then
      begin
        WrapperBltFast( lpDDSBack, 0, 0, Image, Rect( 0, 0, BM.width, BM.height ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        WrapperBltFast( DXDirty, 0, 0, lpDDSBack, rect( 560, 378, 560 + 61, 378 + 45 ), DDBLTFAST_WAIT );
      end;
    finally
      Image := nil;
    end;

    ShowAll := MapKnown or Character.TitleExists( 'MapAllKnown' );

    BM.LoadFromFile( InterfacePath + 'MapMaskLayer.bmp' );
    Image := DDGetImage( lpDD, BM, clBlack, false );
    try
      W := BM.Width;
      H := BM.height;
      BM.free;
      if assigned( Image ) then
      begin
        ddsd.dwSize := SizeOf( ddsd );
        if lpDDSBack.Lock( nil, ddsd, DDLOCK_WAIT, 0 ) = DD_OK then
        begin
          try
            ddsdImage.dwSize := SizeOf( ddsdImage );
            if Image.Lock( nil, ddsdImage, DDLOCK_WAIT, 0 ) = DD_OK then
            begin
              try
                if PixelFormat = pf555 then
                begin //pf888 not currently supported
                  RShift := 10;
                  GBits := $1F;
                  GDark := 4;
                  GLight := 12;
                end
                else
                begin
                  RShift := 11;
                  GBits := $3F;
                  GDark := 10;
                  GLight := 30;
                end;
                RMask := $1F shl RShift;
                GMask := GBits shl 5;

                OffsetX := W div 2 - ( Character.X div ( Map.TileWidth div 4 ) );
                OffsetY := H div 2 - ( Character.Y div ( Map.TileHeight div 4 ) );
                for j := 0 to Map.height - 1 do
                begin
                  Edge1 := ( j = 0 ) or ( j = Map.height - 1 );
                  y := j * 4 + OffsetY;
                  if y >= H then
                    break;
                  inc( y, 3 );
                  if y >= 0 then
                  begin
                    p := ddsd.lpSurface;
                    inc( PByte( p ), ( j * 4 + OffsetY ) * ddsd.lPitch );
                    inc( p, OffsetX );
                    pAlpha := ddsdImage.lpSurface;
                    inc( PByte( pAlpha ), ( j * 4 + OffsetY ) * ddsdImage.lPitch );
                    inc( pAlpha, OffsetX );
                    for i := 0 to Map.Width - 1 do
                    begin
                      Edge := Edge1 or ( i = 0 ) or ( i = Map.Width - 1 );
                      x := i * 4 + OffsetX;
                      if x >= w then
                        break;
                      inc( x, 3 );
                      if x >= 0 then
                      begin
                        Tile := Map.GetTile( i, j );
                        if ( Tile.CollisionMask > 0 ) or Edge then
                        begin
                          Seen := ShowAll or ( ( Tile.BitField and $40 ) > 0 );
                          if Seen or Edge then
                          begin
                            CollisionMask := Tile.CollisionMask;
                            LineOfSightMask := Tile.LineOfSightMask;
                            for k := 0 to 15 do
                            begin
                              if Edge or ( ( CollisionMask and 1 ) <> 0 ) then
                              begin
                                y := j * 4 + ( ( 15 - k ) div 4 ) + OffsetY;
                                if ( y >= 0 ) and ( y < H ) then
                                begin
                                  x := i * 4 + ( k mod 4 ) + OffsetX;
                                  if ( x >= 0 ) and ( x < W ) then
                                  begin
                                    p1Alpha := pAlpha;
                                    inc( PByte( p1Alpha ), ( ( 15 - k ) div 4 ) * ddsdImage.lPitch );
                                    inc( p1Alpha, k mod 4 );
                                    C := p1Alpha^;
                                    AlphaR := 3 * ( ( C and RMask ) shr RShift ) div 4;
                                    AlphaG := 3 * ( ( C and GMask ) shr 5 ) div 4;
                                    AlphaB := 3 * ( C and $1F ) div 4;


                                    p1 := p;
                                    inc( PByte( p1 ), ( ( 15 - k ) div 4 ) * ddsd.lPitch );
                                    inc( p1, k mod 4 );
                                    C := p1^;
                                    R := ( C and RMask ) shr RShift;
                                    G := ( C and GMask ) shr 5;
                                    B := ( C and $1F );

                                    if ( ( LineOfSightMask and 1 ) = 1 ) and Seen then
                                    begin
                                      R := ( ( $1F - AlphaR ) * R + AlphaR * 6 ) div $1F;
                                      G := ( ( GBits - AlphaG ) * G + AlphaG * GDark ) div GBits;
                                      B := ( ( $1F - AlphaB ) * B + AlphaB * 2 ) div $1F;
                                    end
                                    else
                                    begin
                                      R := ( ( $1F - AlphaR ) * R + AlphaR * 18 ) div $1F;
                                      G := ( ( GBits - AlphaG ) * G + AlphaG * GLight ) div GBits;
                                      B := ( ( $1F - AlphaB ) * B + AlphaB * 6 ) div $1F;
                                    end;

                                    C := ( R shl RShift ) + ( G shl 5 ) + B;
                                    p1^ := C;
                                  end;
                                end;
                              end;
                              CollisionMask := CollisionMask shr 1;
                              LineOfSightMask := LineOfSightMask shr 1;
                            end;
                          end;
                        end;
                      end;
                      inc( p, 4 );
                      inc( pAlpha, 4 );
                    end;
                  end;
                end;
              //Draw NPCs
                if assigned( CharacterList ) then
                begin
                  for i := 0 to CharacterList.count - 1 do
                  begin
                    x := TCharacter( CharacterList.items[ i ] ).X div ( Map.TileWidth div 4 ) + OffsetX;
                    y := TCharacter( CharacterList.items[ i ] ).Y div ( Map.TileHeight div 4 ) + OffsetY;
                    if ( x >= 1 ) and ( x < W - 1 ) and ( y >= 1 ) and ( y < H - 1 ) then
                    begin
                      pAlpha := ddsdImage.lpSurface;
                      inc( PByte( pAlpha ), y * ddsdImage.lPitch );
                      inc( pAlpha, x );
                      C := pAlpha^;
                      AlphaR := ( C and RMask ) shr RShift;
                      AlphaG := ( C and GMask ) shr 5;
                      AlphaB := ( C and $1F );

                      p := ddsd.lpSurface;
                      inc( PByte( p ), y * ddsd.lPitch );
                      inc( p, x );
                      C := p^;
                      R := ( C and RMask ) shr RShift;
                      G := ( C and GMask ) shr 5;
                      B := ( C and $1F );

                      if CharacterList.items[ i ] = Character then
                      begin
                      end
                      else
                      begin
                        R := ( ( 0 * AlphaR ) + R * ( $1F - AlphaR ) ) div $1F;
                        G := ( ( 0 * AlphaG ) + G * ( GBits - AlphaG ) ) div GBits;
                        B := ( ( $1F * AlphaB ) + B * ( $1F - AlphaB ) ) div $1F;
                        C := ( R shl RShift ) + ( G shl 5 ) + B;

                        p^ := C;
                        dec( p ); p^ := C;
                        dec( PByte( p ), ddsd.lPitch ); p^ := C;
                        inc( p ); p^ := C;
                        inc( p ); p^ := C;
                        inc( PByte( p ), ddsd.lPitch ); p^ := C;
                        inc( PByte( p ), ddsd.lPitch ); p^ := C;
                        dec( p ); p^ := C;
                        dec( p ); p^ := C;
                      end;
                    end;
                  end;
                end;

                x := Character.X div ( Map.TileWidth div 4 ) + OffsetX;
                y := Character.Y div ( Map.TileHeight div 4 ) + OffsetY;
                if ( x >= 1 ) and ( x < W - 1 ) and ( y >= 1 ) and ( y < H - 1 ) then
                begin
                  pAlpha := ddsdImage.lpSurface;
                  inc( PByte( pAlpha ), y * ddsdImage.lPitch );
                  inc( pAlpha, x );
                  C := pAlpha^;
                  AlphaR := ( C and RMask ) shr RShift;
                  AlphaG := ( C and GMask ) shr 5;
                  AlphaB := ( C and $1F );

                  p := ddsd.lpSurface;
                  inc( PByte( p ), y * ddsd.lPitch );
                  inc( p, x );
                  C := p^;
                  R := ( C and RMask ) shr RShift;
                  G := ( C and GMask ) shr 5;
                  B := ( C and $1F );

                  R := ( ( $1F * AlphaR ) + R * ( $1F - AlphaR ) ) div $1F;
                  G := ( ( 0 * AlphaG ) + G * ( GBits - AlphaG ) ) div GBits;
                  B := ( ( 0 * AlphaB ) + B * ( $1F - AlphaB ) ) div $1F;
                  C := ( R shl RShift ) + ( G shl 5 ) + B;

                  p^ := C;
                  dec( p ); p^ := C;
                  dec( PByte( p ), ddsd.lPitch ); p^ := C;
                  inc( p ); p^ := C;
                  inc( p ); p^ := C;
                  inc( PByte( p ), ddsd.lPitch ); p^ := C;
                  inc( PByte( p ), ddsd.lPitch ); p^ := C;
                  dec( p ); p^ := C;
                  dec( p ); p^ := C;
                end;
           {   p:=ddsd.lpSurface;
              inc(PByte(p),(H div 2)*ddsd.lPitch);
              inc(p,W div 2);
              p^:=RMask;
              dec(p); p^:=RMask;
              dec(PByte(p),ddsd.lPitch); p^:=RMask;
              inc(p); p^:=RMask;
              inc(p); p^:=RMask;
              inc(PByte(p),ddsd.lPitch); p^:=RMask;
              inc(PByte(p),ddsd.lPitch); p^:=RMask;
              dec(p); p^:=RMask;
              dec(p); p^:=RMask;   }
              finally
                Image.Unlock( nil );
              end;
            end;
          finally
            lpDDSBack.Unlock( nil );
          end;
        end;
      end;
    finally
      Image := nil;
    end;

    pText.LoadFontGraphic( 'statistics' );
    pText.PlotTextCentered( MapName, 60, 623, 5, 128 );

    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, ResWidth, ResHeight ), DDBLTFAST_NOCOLORKEY or DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TMap.MouseDown( Sender : TAniview; Button : TMouseButton;
  Shift : TShiftState; X, Y, GridX, GridY : integer );
const
  FailName : string = 'TMap.MouseDown';
begin
  try
    inherited;
  //if InBound then Close;
    if ptInRect( Rect( 560, 378, 560 + 61, 378 + 45 ), point( x, y ) ) then
    begin
      DXBack := nil;
      DXDirty := nil;
      Close;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //MouseDown

procedure TMap.MouseMove( Sender : TAniview; Shift : TShiftState; X, Y, GridX,
  GridY : integer );
const
  FailName : string = 'TMap.MouseMove';
begin
  try

    if ptInRect( Rect( 560, 378, 560 + 61, 378 + 45 ), point( x, y ) ) then
    begin
      if not MouseOverBack then
      begin
        //lpDDSFront.Flip(nil, DDFLIP_WAIT);
        MouseCursor.Cleanup;
        WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
        if assigned( DXBack ) then
          WrapperBltFast( lpDDSBack, 560, 378, DXBack, rect( 0, 0, 61, 45 ), DDBLTFAST_WAIT );
        lpDDSFront.Flip( nil, DDFLIP_WAIT );
        MouseCursor.PlotDirty := false;
        MouseOverBack := true;
      end;
    end
    else
    begin
      if MouseOverBack then
      begin
       //clean up
        MouseCursor.Cleanup;
        WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
        if assigned( DXDirty ) then
          WrapperBltFast( lpDDSBack, 560, 378, DXDirty, rect( 0, 0, 61, 45 ), DDBLTFAST_WAIT );
        lpDDSFront.Flip( nil, DDFLIP_WAIT );
        MouseCursor.PlotDirty := false;
        MouseOverBack := false;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //MouseMove

end.

