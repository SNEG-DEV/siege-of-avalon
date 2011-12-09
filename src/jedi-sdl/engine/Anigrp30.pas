unit Anigrp30;
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
  Classes,
  SysUtils,
  sdl,
  sdlinput,
  AniDec30,
  AStar,
  globals,
  SiegeTypes,
  CustomAniFigure;

type
  TScript = class( TObject )
  public
    Frames : Word;
    FrameID : array[ 1..MaxScriptFrames ] of Word;
    Multiplier : Word;
    Tag : Longint;
  end;

  TAniResource = class( TCustomAniResource )
  private
    function GetScript( const Name : string ) : TScript;
  protected

  public
    Scripts : TStringList;
    constructor Create;
    destructor Destroy; override;
    function AddScript( const Name : string; Script : TScript ) : Integer;
    property Script[ const Name : string ] : TScript read GetScript;
  end;



  TImageSheet = class( TAniResource )
  private
    Picture : PSDL_Surface;
    procedure SetImage( const Value : PSDL_Surface );
    function GetFrames : Longint;
  public
    FrameWidth : Longint;
    FrameHeight : Longint;
    FramesWide : Longint;
    FramesHigh : Longint;
    TransparentColor : TSDL_Color;
    constructor Create;
    procedure Draw( Canvas : PSDL_Surface; X, Y : Integer; Frame : Word ); override;
    procedure FreeResources; override;
    procedure Render( Figure : TCustomAniFigure ); override;
    property Frames : Longint read GetFrames;
    property Image : PSDL_Surface write SetImage;
  end;

  

procedure Register;
function MakeScript( const Frames : array of Word ) : ScriptInfo;
function FindColorMatch( Color : TSDL_Color ) : word;

var
  Debug : Longint;

implementation

uses
  Character;

function MakeScript( const Frames : array of Word ) : ScriptInfo;
var
  i, j : Integer;
begin
  j := 0;
  for i := Low( Frames ) to High( Frames ) do
  begin
    Inc( j );
    Result.FrameID[ j ] := Frames[ i ];
  end;
  Result.Frames := j;
end;

function FindColorMatch( Color : TSDL_Color ) : word;
begin
  result := DDColorMatch( lpDDSBack, Color );
end;


{ TAniResource }

        constructor TAniResource.Create;
        begin
          inherited;
          Scripts := TStringList.Create;
        end;

        destructor TAniResource.Destroy;
        var
          i : Integer;
        begin
          for i := 0 to Scripts.Count - 1 do
            TScript( Scripts.Objects[ i ] ).Free;
          Scripts.Free;
          FreeResources;
          inherited;
        end;

        procedure TAniResource.EnumLightSource( Figure : TAniFigure; Index, X, Y, Z : longint; Intensity : double; Radius : integer );
        begin

        end;

        function TAniResource.AddScript( const Name : string; Script : TScript ) : Integer;
        begin
          Result := Scripts.Add( Name );
          Scripts.Objects[ Result ] := Script;
        end;

        function TAniResource.GetScript( const Name : string ) : TScript;
        var
          i : Integer;
        begin
          i := Scripts.IndexOf( Name );
          if i >= 0 then
            Result := TScript( Scripts.Objects[ i ] )
          else
            Result := nil;
        end;

{ TImageSheet }

        constructor TImageSheet.Create;
        begin
          inherited;
        end;

        procedure TImageSheet.Draw( Canvas : TCanvas; X, Y : Integer; Frame : Word );
        begin

        end;

        procedure TImageSheet.FreeResources;
        begin
      {$IFDEF DirectX}
          Picture := nil;
      {$ENDIF}
      {$IFNDEF DirectX}
          DeleteObject( Picture );
          Picture := 0;
          DeleteObject( Mask );
          Mask := 0;
      {$ENDIF}
        end;

        function TImageSheet.GetFrames : Longint;
        begin
          Result := FramesWide * FramesHigh;
        end;

        procedure TImageSheet.Render( Figure : TAniFigure );
        var
          SrcX, SrcY : Longint;
        {$IFDEF DirectX}
          SrcX1, SrcY1, SrcX2, SrcY2 : Integer;
          DstX1, DstY1, DstX2, DstY2 : Integer;
        {$ENDIF}
        begin
          if ( Figure.Frame = 0 ) or not Figure.Visible then
            Exit;

          SrcX := FrameWidth * ( ( Figure.Frame - 1 ) mod FramesWide );
          SrcY := FrameHeight * ( ( Figure.Frame - 1 ) div FramesWide );

      {$IFDEF DirectX}
          SrcX1 := SrcX;
          SrcX2 := SrcX1 + Figure.Width;
          DstX1 := Figure.View.Left + Figure.PosX;
          DstX2 := DstX1 + Figure.Width;
          Clip( Figure.View.Left, Figure.View.Left + Figure.View.Width, DstX1, DstX2, SrcX1, SrcX2 );
          SrcY1 := SrcY;
          SrcY2 := SrcY1 + Figure.Height;
          DstY1 := Figure.View.Top + Figure.PosY;
          DstY2 := DstY1 + Figure.Height;
          Clip( Figure.View.Top, Figure.View.Top + Figure.View.Height, DstY1, DstY2, SrcY1, SrcY2 );
          WrapperBltFast( lpDDSBack, DstX1, DstY1, Picture,
            Rect( SrcX1, SrcY1, SrcX2, SrcY2 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
        end;

      {$IFDEF DirectX}

        procedure TImageSheet.SetImage( const Value : PSDL_Surface );
        {$ENDIF}
        {$IFNDEF DirectX}
          procedure TImageSheet.SetImage( const Value : TBitmap );
          {$ENDIF}
          begin
        {$IFDEF DirectX}
            Picture := nil;
            Picture := Value;
        {$ENDIF}
        {$IFNDEF DirectX}
            if not Assigned( Value ) then
            begin
              DeleteObject( Picture );
              Picture := 0;
              DeleteObject( Mask );
              Mask := 0;
            end
            else
            begin
              CreateMask( Picture, Mask, Value, ColorToRGB( TransparentColor ) );
            end;
        {$ENDIF}
          end;

end.

