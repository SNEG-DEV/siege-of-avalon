unit digifx;
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
//
// DigitalFX Delphi Interface Unit
// by Dariusz "De JET" Zolna
// Copyright (c) 1998 F.A.S.T Projects
//
// email: digifx@fastprojects.com
// http://www.fastprojects.com
//

interface

uses
  windows,
  sysutils;

const

  BLITFX_NONE = 0;
  BLITFX_LUT = 1;
  BLITFX_MONO = 2;
  BLITFX_BLEND = 4;
  BLITFX_SOFTEN = 8;
  BLITFX_TEXTURED = 16;
  BLITFX_ZOOM = 32;
  BLITFX_ROTATE = 64;
  BLITFX_SKIN = 128;
  BLITFX_MASK = 256;
  BLITFX_HENDS = 512;
  BLITFX_SUCKPIX = 1024;
  BLITFX_COLORIZE = 2048;
  BLITFX_COLORMASK = 4096;
  BLITFX_OUTLINE = 8192;
  BLITFX_CREATERLE = $80000000;

  PIXFMT_8 = 0;
  PIXFMT_555 = 1;
  PIXFMT_565 = 2;
  PIXFMT_888 = 3;
  PIXFMT_BGR = 128;

  DFX_DRAWRLE = 0;
  DFX_DRAWBITPLANE = 1;
  DFX_DRAWPIXELS = 2;
  DFX_DRAWRECT = 3;
  DFX_DRAWLINE = 4;

  NOKEYCOLOR = $FFFFFFFF;

type
  PRLEHDR = ^RLEHDR;
  RLEHDR = record
    SrcX : integer;
    SrcY : integer;
    Wdh : DWORD;
    Hgh : DWORD;
    AdjX : integer;
    AdjY : integer;
    PixFmt : DWORD;
    DataPtr : PChar;
  end;

  RGB = record
    B : BYTE;
    G : BYTE;
    R : BYTE;
    Unused : BYTE;
  end;

  PBITPLANE = ^BITPLANE;
  BITPLANE = record
    bitsPtr : PBYTE;
    bitsWdh : DWORD;
    bitsHgh : DWORD;
    bitsFmt : DWORD;
    bitsPitch : DWORD;
    BaseX : integer;
    BaseY : integer;
  end;

  PBLITFX = ^BLITFX;
  BLITFX = record
    FXType : DWORD;
    BlendSrcFactor : DWORD;
    BlendDstFactor : DWORD;
    LUTPtr : Pointer;
    Color : RGB;
    TexturePtr : PBITPLANE;
    SrcRFactor : DWORD;
    SrcGFactor : DWORD;
    SrcBFactor : DWORD;
    DstRFactor : DWORD;
    DstGFactor : DWORD;
    DstBFactor : DWORD;
    ColorMaskSet : RGB;
    ColorMaskClr : RGB;
    Angle : DWORD;
    ZoomX : DWORD;
    ZoomY : DWORD;
  end;

  PPIXEL = ^PIXEL;
  PIXEL = record
    StructSize : DWORD;
    X : integer;
    Y : integer;
    Color : DWORD;
  end;

type
  DFXHND = DWORD;
  DFXENUMCALLBACK = function( const s : PChar ) : boolean;


function digifxInit( const s : PChar ) : BOOL;
function digifxDone : BOOL;
function digifxEnumDrivers( lpEnumProc : DFXENUMCALLBACK ) : BOOL;
function digifxLoadDriver( lpDrvNamePtr : PChar; dwPixFmt : DWORD ) : DFXHND;
function digifxFreeDriver( hDFX : DFXHND ) : BOOL;
function digifxCreateRLE( hDFX : DFXHND; SrcBitsPtr : PBITPLANE; KeyColor : DWORD; RLEHDRPtr : PRLEHDR; BuffPtr : Pointer; FirstPtr : PRLEHDR ) : DWORD;
function digifxConvertRLE( hDFX : DFXHND; RLEHDRPtr : PRLEHDR ) : BOOL;
function digifxConvertBitplane( hDFX : DFXHND; BitplanePtr : PBITPLANE ) : BOOL;
function digifxDrawRLE( hDFX : DFXHND; RLEHDRPtr : PRLEHDR; XPos : integer; YPos : integer; FXPtr : PBLITFX; DstBitsPtr : PBITPLANE ) : BOOL;
function digifxRLEConvertToPixels( hDFX : DFXHND; RLEHDRPtr : PRLEHDR; PixTabPtr : PPIXEL; PixCnt : DWORD; PixSize : DWORD ) : DWORD;
function digifxDrawPixels( hDFX : DFXHND; PixTabPtr : PPIXEL; PixCnt : DWORD; XPos : integer; YPos : integer; FXPtr : PBLITFX; DstBitsPtr : PBITPLANE ) : BOOL;
function digifxDrawBitplane( hDFX : DFXHND; SrcBitsPtr : PBITPLANE; XPos : integer; YPos : integer; KeyColor : DWORD; FXPtr : PBLITFX; DstBitsPtr : PBITPLANE ) : BOOL;
function digifxDrawRect( hDFX : DFXHND; RectPtr : PRECT; XPos : integer; YPos : integer; FXPtr : PBLITFX; DstBitsPtr : PBITPLANE ) : BOOL;
function digifxDrawLine( hDFX : DFXHND; XBgn : integer; YBgn : integer; XEnd : integer; YEnd : integer; FXPtr : PBLITFX; DstBitsPtr : PBITPLANE ) : BOOL;
function digifxConvertColor( hDFX : DFXHND; R : BYTE; G : BYTE; B : BYTE ) : DWORD;
function digifxCheckSupport( hDFX : DFXHND; ProcNo : DWORD; FXPtr : PBLITFX; SrcPtr : Pointer; DstPtr : PBITPLANE ) : BOOL;
function digifxGetErrorText : PCHAR;

implementation

type
  DFXSTARTUPLIB = function : PDWORD; stdcall;

const
  // Due to the fact that Delphi ignores the case of identifiers I had
  // to change these consts from DFX_??? to DLL_???
  DLL_GetInfo = 0;
  DLL_Init = 1;
  DLL_Done = 2;
  DLL_CreateRLE = 3;
  DLL_ConvertRLE = 4;
  DLL_ConvertBitplane = 5;
  DLL_DrawRLE = 6;
  DLL_RLEConvertToPixels = 7;
  DLL_DrawPixels = 8;
  DLL_DrawBitplane = 9;
  DLL_DrawRect = 10;
  DLL_DrawLine = 11;
  DLL_ConvertColor = 12;
  DLL_CheckSupport = 13;

type
  DRVFILE = record
    Info : array[ 0..64 ] of AnsiChar;
    Fname : array[ 0..MAX_PATH - 1 ] of AnsiChar;
    Handle : HMODULE;
    RefCnt : DWORD;
    DFX : PDWORD;
    TabPtr : Pointer;
  end;

const
  DF_ID = 17478; // 'DF'

var
  DrvFilesTab : array[ 0..32 ] of DRVFILE;
  DriversCnt : DWORD;
  ErrPtr : PCHAR;
  RegEAX, RegEBX, RegECX, RegEDX, RegESI, RegEDI : DWORD;

function digifxInit( const s : PChar ) : BOOL;
var
  StrPtr : PChar;
  WorkDir : array[ 0..MAX_PATH - 1 ] of AnsiChar;
  TmpFname : array[ 0..MAX_PATH - 1 ] of AnsiChar;
  FindHnd : THandle;
  FindData : TWin32FindData;
  hMod : HMODULE;
  DFX : PDWORD;
  StartupLib : DFXSTARTUPLIB;
begin
  StrCopy( WorkDir, PChar( s ) );
  if ( WorkDir[ Length( s ) - 1 ] <> '\' ) then
    StrCat( WorkDir, '\' );
  DriversCnt := 0;
  StrCopy( TmpFname, WorkDir );
  StrCat( TmpFname, 'dfx_*.dll' );

  FindHnd := Windows.FindFirstFile( TmpFname, FindData );
  while ( FindHnd <> INVALID_HANDLE_VALUE ) do
  begin
    StrCopy( TmpFname, WorkDir );
    StrCat( TmpFname, FindData.cFileName );
    hMod := LoadLibrary( TmpFname );
    if ( hMod <> 0 ) then
    begin
      @StartupLib := GetProcAddress( hMod, 'StartupLibrary' );
      if ( @StartupLib <> nil ) then
      begin
        DFX := StartupLib( );
        asm
          mov eax, FindHnd;
          mov eax, DFX;
          call dword ptr [eax + DLL_GetInfo*4];
          mov StrPtr, esi;
        end;
        if ( StrLComp( StrPtr, 'DigitalFX', 9 ) = 0 ) then
        begin
          StrCopy( DrvFilesTab[ DriversCnt ].Info, StrPtr );
          StrCopy( DrvFilesTab[ DriversCnt ].Fname, TmpFname );
          DrvFilesTab[ DriversCnt ].RefCnt := 0;
          Inc( DriversCnt );
        end;
      end;
      FreeLibrary( hMod );
    end;
    if ( not Windows.FindNextFile( FindHnd, FindData ) ) then
      break;
  end;
  Windows.FindClose( FindHnd );

  result := true;
end;

function digifxDone : BOOL;
var
  i : DWORD;
begin
  for i := 0 to DriversCnt - 1 do
  begin
    if ( DrvFilesTab[ i ].RefCnt > 0 ) then
    begin
      FreeMem( DrvFilesTab[ i ].TabPtr );
      FreeLibrary( DrvFilesTab[ i ].Handle );
      DrvFilesTab[ i ].RefCnt := 0;
    end;
  end;
  result := false;
end;

function digifxEnumDrivers( lpEnumProc : DFXENUMCALLBACK ) : BOOL;
var
  i : DWORD;
begin
  for i := 0 to DriversCnt - 1 do
  begin
    if ( not lpEnumProc( DrvFilesTab[ i ].Info ) ) then
      break;
  end;
  result := TRUE;
end;

function CallDFX( hDFX : DFXHND; ProcNo : DWORD; REAX : DWORD; REBX : DWORD; RECX : DWORD; REDX : DWORD; RESI : DWORD; REDI : DWORD ) : BOOL;
var
  i : DWORD;
  TmpProcPtr : DWORD;
begin
  if ( ( hDFX and $FFFF0000 ) = ( DF_ID shl 16 ) ) then
  begin
    i := ( hDFX and $0000FFFF );
    if ( ( DrvFilesTab[ i ].RefCnt > 0 ) and ( i < DriversCnt ) ) then
    begin
      TmpProcPtr := DWORD( DrvFilesTab[ i ].DFX );
      asm
        pushad
      	mov     eax, ProcNo
				mov     ebx, TmpProcPtr
				mov     eax, [ebx+eax*4]
				mov     TmpProcPtr, eax
				mov     eax, REAX
				mov     ebx, REBX
				mov     ecx, RECX
				mov     edx, REDX
				mov     esi, RESI
				mov     edi, REDI
				push    ebp
        // Changed from "call [TmpProcPtr]"
				call    dword ptr [TmpProcPtr]
				pop     ebp
				mov     RegEAX, eax
				mov     RegEBX, ebx
				mov     RegECX, ecx
				mov     RegEDX, edx
				mov     RegESI, esi
				mov     RegEDI, edi
				jnc     @NoErr
				mov     ErrPtr, esi
				jmp     @Exit
			@NoErr:
				mov     ErrPtr, 0
			@Exit:
        popad
      end;
      if ( ErrPtr <> nil ) then
      begin
        result := false;
        exit;
      end
      else
      begin
        result := true;
        exit;
      end;
    end;
  end;

  ErrPtr := 'Bad DigitalFX driver handle.';
  result := false;
end;

function digifxLoadDriver( lpDrvNamePtr : PChar; dwPixFmt : DWORD ) : DFXHND;
var
  i : DWORD;
  hDFX : DFXHND;
  hMod : HMODULE;
  StartupLib : DFXSTARTUPLIB;
begin
  result := 0;
  for i := 0 to DriversCnt - 1 do
  begin
    if ( StrComp( lpDrvNamePtr, DrvFilesTab[ i ].Info ) = 0 ) then
    begin
      hDFX := i or ( DF_ID shl 16 );
      if ( DrvFilesTab[ i ].RefCnt = 0 ) then
      begin
        hMod := LoadLibrary( DrvFilesTab[ i ].Fname );
        if ( hMod = 0 ) then
          exit;

        @StartupLib := GetProcAddress( hMod, 'StartupLibrary' );
        if ( @StartupLib = nil ) then
          exit;

        DrvFilesTab[ i ].Handle := hMod;
        DrvFilesTab[ i ].DFX := StartupLib( );
        GetMem( DrvFilesTab[ i ].TabPtr, 96000 );
      end;
      inc( DrvFilesTab[ i ].RefCnt );
      if ( not CallDFX( hDFX, DLL_Init, dwPixFmt, DWORD( DrvFilesTab[ i ].TabPtr ), 1, 0, 0, 0 ) ) then
        exit
      else
        result := hDFX;
      exit;
    end;
  end;
end;

function digifxFreeDriver( hDFX : DFXHND ) : BOOL;
var
  i : DWORD;
begin
  result := false;

  if ( not CallDFX( hDFX, DLL_Done, 0, 0, 0, 0, 0, 0 ) ) then
    exit;

  i := hDFX and $0000FFFF;
  dec( DrvFilesTab[ i ].RefCnt );
  if ( DrvFilesTab[ i ].RefCnt = 0 ) then
  begin
    FreeMem( DrvFilesTab[ i ].TabPtr );
    result := FreeLibrary( DrvFilesTab[ i ].Handle );
  end
  else
    result := true;
end;

function digifxGetErrorText : PCHAR;
begin
  result := ErrPtr;
end;

function digifxCreateRLE( hDFX : DFXHND; SrcBitsPtr : PBITPLANE; KeyColor : DWORD; RLEHDRPtr : PRLEHDR; BuffPtr : Pointer; FirstPtr : PRLEHDR ) : DWORD;
begin
  result := 0;

  if ( CallDFX( hDFX, DLL_CreateRLE, KeyColor, DWORD( BuffPtr ), 0, DWORD( FirstPtr ), DWORD( SrcBitsPtr ), DWORD( RLEHDRPtr ) ) ) then
    result := RegEAX;
end;

// Return type mismatch: changed DWORD -> BOOL
// Moved NewBuffPtr inside the proc. Is this ptr a dummy ptr?

function digifxConvertRLE( hDFX : DFXHND; RLEHDRPtr : PRLEHDR ) : BOOL;
var
  NewBuffPtr : Pointer;
begin
  NewBuffPtr := nil;
  result := CallDFX( hDFX, DLL_ConvertRLE, 0, 0, 0, 0, DWORD( RLEHDRPtr ), DWORD( NewBuffPtr ) );
end;

// Return type mismatch: changed DWORD -> BOOL
// Moved NewBuffPtr inside the proc. Is this ptr a dummy ptr?

function digifxConvertBitplane( hDFX : DFXHND; BitplanePtr : PBITPLANE ) : BOOL;
var
  NewBuffPtr : Pointer;
begin
  NewBuffPtr := nil;
  result := CallDFX( hDFX, DLL_ConvertBitplane, 0, 0, 0, 0, DWORD( BitplanePtr ), DWORD( NewBuffPtr ) );
end;

function digifxDrawRLE( hDFX : DFXHND; RLEHDRPtr : PRLEHDR; XPos : integer; YPos : integer; FXPtr : PBLITFX; DstBitsPtr : PBITPLANE ) : BOOL;
begin
  result := CallDFX( hDFX, DLL_DrawRLE, DWORD( XPos ), DWORD( YPos ), 0, DWORD( FXPtr ), DWORD( RLEHDRPtr ), DWORD( DstBitsPtr ) );
end;

function digifxRLEConvertToPixels( hDFX : DFXHND; RLEHDRPtr : PRLEHDR; PixTabPtr : PPIXEL; PixCnt : DWORD; PixSize : DWORD ) : DWORD;
begin
  if ( CallDFX( hDFX, DLL_RLEConvertToPixels, DWORD( PixSize ), 0, DWORD( PixCnt ), 0, DWORD( RLEHDRPtr ), DWORD( PixTabPtr ) ) ) then
    result := RegEAX
  else
    result := 0;
end;

function digifxDrawPixels( hDFX : DFXHND; PixTabPtr : PPIXEL; PixCnt : DWORD; XPos : integer; YPos : integer; FXPtr : PBLITFX; DstBitsPtr : PBITPLANE ) : BOOL;
begin
  result := CallDFX( hDFX, DLL_DrawPixels, DWORD( PixTabPtr ), DWORD( XPos ), DWORD( YPos ), DWORD( PixCnt ), DWORD( DstBitsPtr ), DWORD( FXPtr ) );
end;

function digifxDrawBitplane( hDFX : DFXHND; SrcBitsPtr : PBITPLANE; XPos : integer; YPos : integer; KeyColor : DWORD; FXPtr : PBLITFX; DstBitsPtr : PBITPLANE ) : BOOL;
begin
  result := CallDFX( hDFX, DLL_DrawBitplane, DWORD( SrcBitsPtr ), DWORD( XPos ), DWORD( YPos ), KeyColor, DWORD( DstBitsPtr ), DWORD( FXPtr ) );
end;

function digifxDrawRect( hDFX : DFXHND; RectPtr : PRECT; XPos : integer; YPos : integer; FXPtr : PBLITFX; DstBitsPtr : PBITPLANE ) : BOOL;
begin
  result := CallDFX( hDFX, DLL_DrawRect, DWORD( RectPtr ), DWORD( XPos ), DWORD( YPos ), DWORD( FXPtr ), DWORD( DstBitsPtr ), 0 );
end;

function digifxDrawLine( hDFX : DFXHND; XBgn : integer; YBgn : integer; XEnd : integer; YEnd : integer; FXPtr : PBLITFX; DstBitsPtr : PBITPLANE ) : BOOL;
begin
  result := CallDFX( hDFX, DLL_DrawLine, DWORD( XBgn ), DWORD( YBgn ), DWORD( XEnd ), DWORD( YEnd ), DWORD( DstBitsPtr ), DWORD( FXPtr ) );
end;

function digifxConvertColor( hDFX : DFXHND; R : BYTE; G : BYTE; B : BYTE ) : DWORD;
var
  Color : DWORD;
begin
  asm
		mov al, R
		shl eax, 8
		mov al, G
		shl eax, 8
		mov al, B
		mov Color, eax
  end;
  if ( CallDFX( hDFX, DLL_ConvertColor, Color, 0, 0, 0, 0, 0 ) ) then
    result := RegEAX
  else
    result := NOKEYCOLOR;
end;

function digifxCheckSupport( hDFX : DFXHND; ProcNo : DWORD; FXPtr : PBLITFX; SrcPtr : Pointer; DstPtr : PBITPLANE ) : BOOL;
begin
  result := CallDFX( hDFX, DLL_CheckSupport, ProcNo, DWORD( FXPtr ), 0, 0, DWORD( SrcPtr ), DWORD( DstPtr ) );
end;

end.

