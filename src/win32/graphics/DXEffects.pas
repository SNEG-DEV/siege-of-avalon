unit DXEffects;
(*
  Siege Of Avalon : Open Source Edition

  Portions created by Digital Tome L.P. Texas USA are
  Copyright �1999-2000 Digital Tome L.P. Texas USA
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
  System.Types,
//  Winapi.DirectDraw,
  DirectX,
  Vcl.Graphics;

//procedure DrawAdd( Dest : IDirectDrawSurface; const DestRect, SrcRect : TRect; Source : IDirectDrawSurface;
//  TRANSPARENT : Boolean; Alpha : Integer );

procedure DrawAlpha( Dest : IDirectDrawSurface; const DestRect, SrcRect : TRect; Source : IDirectDrawSurface;
  TRANSPARENT : Boolean; Alpha : Integer );

//procedure DrawMult( Dest : IDirectDrawSurface; const DestRect, SrcRect : TRect; Source : IDirectDrawSurface;
//  TRANSPARENT : Boolean; Alpha : Integer );

procedure DrawSub( Dest : IDirectDrawSurface; const DestRect, SrcRect : TRect; Source : IDirectDrawSurface;
  TRANSPARENT : Boolean; Alpha : Integer );

//procedure DrawInvSub( Dest : IDirectDrawSurface; const DestRect, SrcRect : TRect; Source : IDirectDrawSurface;
//  TRANSPARENT : Boolean; Alpha : Integer );

//procedure FillRectAdd( Dest : IDirectDrawSurface; const DestRect : TRect; RGBCol : TColor );

procedure FillRectAlpha( Dest : IDirectDrawSurface; const DestRect : TRect; RGBCol : TColor;
  Alpha : Integer );

procedure FillRectSub( Dest : IDirectDrawSurface; const DestRect : TRect; RGBCol : TColor );

implementation

uses
  DXRender;

//procedure DrawAdd( Dest : IDirectDrawSurface; const DestRect, SrcRect : TRect; Source : IDirectDrawSurface;
//  TRANSPARENT : Boolean; Alpha : Integer );
//const
//  FailName : string = 'DXEffects.DrawAdd';
//var
//  Src_ddsd : TDDSURFACEDESC;
//  DestSurface, SrcSurface : TDXR_Surface;
//  Blend : TDXR_Blend;
//begin
//  if dxrDDSurfaceLock( Dest, DestSurface ) then
//  begin
//    try
//      if dxrDDSurfaceLock2( Source, Src_ddsd, SrcSurface ) then
//      begin
//        try
//          if DestSurface.ColorType = DXR_COLORTYPE_INDEXED then
//          begin
//            Blend := DXR_BLEND_ONE1;
//          end
//          else if Alpha >= 255 then
//          begin
//            Blend := DXR_BLEND_ONE1_ADD_ONE2;
//          end
//          else
//          begin
//            Blend := DXR_BLEND_SRCALPHA1_ADD_ONE2;
//          end;
//
//          dxrCopyRectBlend( DestSurface, SrcSurface,
//            DestRect, SrcRect, Blend, Alpha, TRANSPARENT, Src_ddsd.ddckCKSrcBlt.dwColorSpaceLowValue );
//        finally
//          dxrDDSurfaceUnLock( Source, SrcSurface )
//        end;
//      end;
//    finally
//      dxrDDSurfaceUnLock( Dest, DestSurface )
//    end;
//  end;
//end;

procedure DrawAlpha( Dest : IDirectDrawSurface; const DestRect, SrcRect : TRect; Source : IDirectDrawSurface;
  TRANSPARENT : Boolean; Alpha : Integer );
const
  FailName : string = 'DXEffects.DrawAlpha';
var
  Src_ddsd : TDDSURFACEDESC;
  DestSurface, SrcSurface : TDXR_Surface;
  Blend : TDXR_Blend;
begin
  FillChar(Src_ddsd, sizeof(Src_ddsd), 0);
  if dxrDDSurfaceLock( Dest, DestSurface ) then
  begin
    try
      if dxrDDSurfaceLock2( Source, Src_ddsd, SrcSurface ) then
      begin
        try
          if DestSurface.ColorType = DXR_COLORTYPE_INDEXED then
          begin
            Blend := DXR_BLEND_ONE1;
          end
          else if Alpha >= 255 then
          begin
            Blend := DXR_BLEND_ONE1;
          end
          else
          begin
            Blend := DXR_BLEND_SRCALPHA1_ADD_INVSRCALPHA2;
          end;

          dxrCopyRectBlend( DestSurface, SrcSurface,
            DestRect, SrcRect, Blend, Alpha, TRANSPARENT, Src_ddsd.ddckCKSrcBlt.dwColorSpaceLowValue );
        finally
          dxrDDSurfaceUnLock( Source, SrcSurface )
        end;
      end;
    finally
      dxrDDSurfaceUnLock( Dest, DestSurface )
    end;
  end;
end;

//procedure DrawMult( Dest : IDirectDrawSurface; const DestRect, SrcRect : TRect; Source : IDirectDrawSurface;
//  TRANSPARENT : Boolean; Alpha : Integer );
//const
//  FailName : string = 'DXEffects.DrawMult';
//var
//  Src_ddsd : TDDSURFACEDESC;
//  DestSurface, SrcSurface : TDXR_Surface;
//  Blend : TDXR_Blend;
//begin
//  if dxrDDSurfaceLock( Dest, DestSurface ) then
//  begin
//    try
//      if dxrDDSurfaceLock2( Source, Src_ddsd, SrcSurface ) then
//      begin
//        try
//          if DestSurface.ColorType = DXR_COLORTYPE_INDEXED then
//          begin
//            Blend := DXR_BLEND_ONE1;
//          end
//          else if Alpha >= 255 then
//          begin
//            Blend := DXR_BLEND_MODULATE;
//          end
//          else
//          begin
//            Blend := DXR_BLEND_MODULATEALPHA;
//          end;
//
//          dxrCopyRectBlend( DestSurface, SrcSurface,
//            DestRect, SrcRect, Blend, Alpha, TRANSPARENT, Src_ddsd.ddckCKSrcBlt.dwColorSpaceLowValue );
//        finally
//          dxrDDSurfaceUnLock( Source, SrcSurface )
//        end;
//      end;
//    finally
//      dxrDDSurfaceUnLock( Dest, DestSurface )
//    end;
//  end;
//end;

procedure DrawSub( Dest : IDirectDrawSurface; const DestRect, SrcRect : TRect; Source : IDirectDrawSurface;
  TRANSPARENT : Boolean; Alpha : Integer );
const
  FailName : string = 'DXEffects.DrawSub';
var
  Src_ddsd : TDDSURFACEDESC;
  DestSurface, SrcSurface : TDXR_Surface;
  Blend : TDXR_Blend;
begin
  FillChar(Src_ddsd, sizeof(Src_ddsd), 0);
  if dxrDDSurfaceLock( Dest, DestSurface ) then
  begin
    try
      if dxrDDSurfaceLock2( Source, Src_ddsd, SrcSurface ) then
      begin
        try
          if DestSurface.ColorType = DXR_COLORTYPE_INDEXED then
          begin
            Blend := DXR_BLEND_ONE1;
          end
          else if Alpha >= 255 then
          begin
            Blend := DXR_BLEND_ONE2_SUB_ONE1;
          end
          else
          begin
            Blend := DXR_BLEND_ONE2_SUB_SRCALPHA1;
          end;

          dxrCopyRectBlend( DestSurface, SrcSurface,
            DestRect, SrcRect, Blend, Alpha, TRANSPARENT, Src_ddsd.ddckCKSrcBlt.dwColorSpaceLowValue );
        finally
          dxrDDSurfaceUnLock( Source, SrcSurface )
        end;
      end;
    finally
      dxrDDSurfaceUnLock( Dest, DestSurface )
    end;
  end;
end;

//procedure DrawInvSub( Dest : IDirectDrawSurface; const DestRect, SrcRect : TRect; Source : IDirectDrawSurface;
//  TRANSPARENT : Boolean; Alpha : Integer );
//const
//  FailName : string = 'DXEffects.DrawInvSub';
//var
//  Src_ddsd : TDDSURFACEDESC;
//  DestSurface, SrcSurface : TDXR_Surface;
//  Blend : TDXR_Blend;
//begin
//  if dxrDDSurfaceLock( Dest, DestSurface ) then
//  begin
//    try
//      if dxrDDSurfaceLock2( Source, Src_ddsd, SrcSurface ) then
//      begin
//        try
//          if DestSurface.ColorType = DXR_COLORTYPE_INDEXED then
//          begin
//            Blend := DXR_BLEND_ONE1;
//          end
//          else if Alpha >= 255 then
//          begin
//            Blend := DXR_BLEND_ONE1_SUB_ONE2;
//          end
//          else
//          begin
//            Blend := DXR_BLEND_ONE2_SUB_SRCALPHA1; //Not currently supported
//          end;
//
//          dxrCopyRectBlend( DestSurface, SrcSurface,
//            DestRect, SrcRect, Blend, Alpha, TRANSPARENT, Src_ddsd.ddckCKSrcBlt.dwColorSpaceLowValue );
//        finally
//          dxrDDSurfaceUnLock( Source, SrcSurface )
//        end;
//      end;
//    finally
//      dxrDDSurfaceUnLock( Dest, DestSurface )
//    end;
//  end;
//end;

//procedure FillRectAdd( Dest : IDirectDrawSurface; const DestRect : TRect; RGBCol : TColor );
//const
//  FailName : string = 'DXEffects.FillRectAdd';
//var
//  DestSurface : TDXR_Surface;
//begin
//  if dxrDDSurfaceLock( Dest, DestSurface ) then
//  begin
//    try
//      dxrFillRectColorBlend( DestSurface, DestRect, DXR_BLEND_ONE1_ADD_ONE2, ColorToRGB( RGBCol ) );
//    finally
//      dxrDDSurfaceUnLock( Dest, DestSurface )
//    end;
//  end;
//end;

procedure FillRectAlpha( Dest : IDirectDrawSurface; const DestRect : TRect; RGBCol : TColor;
  Alpha : Integer );
const
  FailName : string = 'DXEffects.FillRectAlpha';
var
  DestSurface : TDXR_Surface;
begin
  if dxrDDSurfaceLock( Dest, DestSurface ) then
  begin
    try
      dxrFillRectColorBlend( DestSurface, DestRect, DXR_BLEND_SRCALPHA1_ADD_INVSRCALPHA2, ColorToRGB( RGBCol ) or ( Byte( Alpha ) shl 24 ) );
    finally
      dxrDDSurfaceUnLock( Dest, DestSurface )
    end;
  end;
end;

procedure FillRectSub( Dest : IDirectDrawSurface; const DestRect : TRect; RGBCol : TColor );
const
  FailName : string = 'DXEffects.FillRectSub';
var
  DestSurface : TDXR_Surface;
begin
  if dxrDDSurfaceLock( Dest, DestSurface ) then
  begin
    try
      dxrFillRectColorBlend( DestSurface, DestRect, DXR_BLEND_ONE2_SUB_ONE1, ColorToRGB( RGBCol ) );
    finally
      dxrDDSurfaceUnLock( Dest, DestSurface )
    end;
  end;
end;

end.

