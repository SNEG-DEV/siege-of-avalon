unit OpenAnim;
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
{$IFDEF DirectX}
  DirectX,
  DXUtil,
  DXEffects,
{$ENDIF}
  Windows,
  MMSystem,
  Messages,
  SysUtils,
  Classes,
  Graphics,
  Controls,
  Forms,
  Dialogs,
  ExtCtrls,
  Character,
  StdCtrls,
  Display,
  Anigrp30,
  math,
  Music,
  Resource,
  logfile;
  
type
  TOpenAnim = class( TDisplay )
  private
    LoopCounter : integer;
    KeepOnPlaying : boolean;
    //Bitmap stuff
    BMBack : TBitmap;
    DXBack : IDirectDrawSurface;
    DXSiege : IDirectDrawSurface;
    DXLogo : IDirectDrawSurface;
    XAdj, YAdj : integer;
    FCancel : boolean;
    procedure PlayAnim;
  protected
    procedure MouseDown( Sender : TAniview; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
    procedure KeyDown( Sender : TObject; var key : Word; Shift : TShiftState ); override;
  public
    pMusic : TMusic;
    constructor Create;
    destructor Destroy; override;
    procedure Init; override;
    procedure Release; override;
    property Cancel : boolean read FCancel;
  end;
implementation
uses
  AniDemo;
{ TOpenAnim }

constructor TOpenAnim.Create;
const
  FailName : string = 'TOpenAnim.create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //Create

destructor TOpenAnim.Destroy;
const
  FailName : string = 'TOpenAnim.destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //Destroy

procedure TOpenAnim.Init;
var
  InvisColor : integer;

const
  FailName : string = 'TOpenAnim.init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if Loaded then
      Exit;
    inherited;

    FCancel := false;
    LoopCounter := 0;
    if assigned( pMusic ) then
    begin
  //  sndPlaySound(PChar(SoundPath + 'Theme\IntroTitle.wav'),1);
      pMusic.OpenThisSong( SoundPath + 'Theme\IntroTitle.MP3' );
      pMusic.PlayThisSong;
      pMusic.SetSongVolume( 99 );
    end;
  //Sheet coordinate adjustments
    XAdj := 0;
    YAdj := -20;

    BMBack := TBitmap.Create;
  //transparent color
    InvisColor := $00FFFF00;

    BMBack.LoadFromFile( InterfacePath + 'aniSiege.bmp' );
    DXSiege := DDGetImage( lpDD, BMBack, InvisColor, false );

    BMBack.LoadFromFile( InterfacePath + 'aniDTIPresents.bmp' );
    DXLogo := DDGetImage( lpDD, BMBack, InvisColor, false );

    BMBack.LoadFromFile( InterfacePath + 'aniBack.bmp' );
    DXBack := DDGetImage( lpDD, BMBack, InvisColor, true );
    lpDDSBack.BltFast( 0, 0, DXBack, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );

    BMBack.Free;

  //DrawAlpha(lpDDSBack,rect(0,0,800,600),rect(0,0,25,25),DXSiege,false,255);
  //DrawSub(lpDDSBack,rect(0,0,800,600),rect(0,0,25,25),DXSiege,false,255);
    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    lpDDSBack.BltFast( 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );

    KeepOnPlaying := true;
    PlayAnim;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //Init


procedure TOpenAnim.KeyDown( Sender : TObject; var key : Word; Shift : TShiftState );
const
  FailName : string = 'TOpenAnim.Keydown';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if assigned( pMusic ) then
    begin
      pMusic.PauseThisSong;
    end;
     //sndPlaySound(nil,1);
    KeepOnPlaying := false;
    FCancel := true;
    Close;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //keyDown

procedure TOpenAnim.MouseDown( Sender : TAniview; Button : TMouseButton; Shift : TShiftState; X, Y, GridX, GridY : integer );
const
  FailName : string = 'TOpenAnim.Mousedown';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if assigned( pMusic ) then
    begin
      pMusic.PauseThisSong;
    end;
   //sndPlaySound(nil,1);
    KeepOnPlaying := false;
    FCancel := true;
    Close;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //MouseDown


procedure TOpenAnim.PlayAnim;
var
  OldTime : Longword;
  Adj, TimeDif : real;
  Phase : integer;
  Alpha : integer;
  MusicStillPlaying : boolean;
  MusicStartTime : Longword;
  StartFinalCount : Longword;
  i : integer;
const
  FailName : string = 'TOpenAnim.PlayAnim';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    StartFinalCount := 0;
    lpDDSBack.BltFast( 0, 0, DXBack, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
    MusicStillPlaying := true;
    MusicStartTime := GetTickCount;
    OldTime := GetTickCount;
    Adj := 0;
    Alpha := 0;
    Phase := 0;
    while KeepOnPlaying do
    begin
      if MusicStillPlaying then
      begin
        if GetTickCount - MusicStartTime > 19500 then
        begin
               //pMusic.PauseThisSong;
          MusicStillPlaying := false;
        end;
            //else
              //pMusic.SetSongVolume(95);
      end;
      if Phase = 0 then
      begin
        TimeDif := GetTickCount - OldTime;
        if TimeDif > 1980 then
        begin
          Phase := 1;
        end;
      end
      else if Phase = 1 then
      begin
        application.ProcessMessages;
        TimeDif := GetTickCount - OldTime;
        if Alpha < 100 then
          Adj := Adj + 24 * ( TimeDif / 1000 )
        else if Alpha < 200 then
          Adj := Adj + 92 * ( TimeDif / 1000 )
        else
          Adj := Adj + 172 * ( TimeDif / 1000 );
        OldTime := GetTickCount;
        if Adj >= 1 then
        begin
          Alpha := Alpha + round( Adj );

          if Alpha > 254 then
            Alpha := 254;
          application.ProcessMessages;
          for i := 0 to 11 do
          begin
            DrawAlpha( lpDDSBack, Rect( 0, 250 + i * 19, 800, 250 + i * 19 + 19 ), rect( 0, i * 19, 800, i * 19 + 19 ), DXLogo, true, Alpha );
            application.processmessages;
          end;
          application.ProcessMessages;
          Adj := Adj - Trunc( Adj );
          application.ProcessMessages;
          lpDDSFront.Flip( nil, DDFLIP_WAIT );
          application.ProcessMessages;
          lpDDSBack.BltFast( 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
          application.ProcessMessages;
        end;
        if Alpha >= 254 then
        begin
          Phase := 2;
          Adj := 0;
                 //Alpha:=205;
        end;
      end
      else if Phase = 2 then
      begin
        TimeDif := GetTickCount - OldTime;
        if Alpha > 100 then
          Adj := Adj + 92 * ( TimeDif / 1000 )
        else
          Adj := Adj + 62 * ( TimeDif / 1000 );
        OldTime := GetTickCount;
        if Adj >= 1 then
        begin
          application.ProcessMessages;
          Alpha := Alpha - round( Adj );
                 //if Alpha > 100 then Alpha:=Alpha-3;
          if Alpha < 0 then
            Alpha := 0;
          application.ProcessMessages;
          lpDDSBack.BltFast( 0, 250, DXBack, Rect( 0, 250, 800, 250 + 228 ), DDBLTFAST_WAIT );
          application.ProcessMessages;
          if Alpha > 0 then
          begin
//                    DrawAlpha(lpDDSBack,Rect(0, 250, 800, 250+228),rect(0,0,800,228),DXLogo,true,Alpha);
            for i := 0 to 11 do
            begin
              DrawAlpha( lpDDSBack, Rect( 0, 250 + i * 19, 800, 250 + i * 19 + 19 ), rect( 0, i * 19, 800, i * 19 + 19 ), DXLogo, true, Alpha );
              application.processmessages;
            end;
          end;
                 //DrawAlpha(lpDDSBack,Rect(0, 250, 800, 250+228),Rect(0, 250, 800, 250+228),DXBack,true,Alpha);
          application.ProcessMessages;
          Adj := Adj - Trunc( Adj );
          lpDDSFront.Flip( nil, DDFLIP_WAIT );
          application.ProcessMessages;
          lpDDSBack.BltFast( 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
          application.ProcessMessages;
        end;
        if Alpha <= 0 then
        begin
          Phase := 3;
          Adj := 0;
          Alpha := 0;
          OldTime := GetTickCount;
        end;
      end
      else if Phase = 3 then
      begin
        TimeDif := GetTickCount - OldTime;
        if TimeDif > 1500 then
        begin
          Phase := 4;
          StartFinalCount := GetTickCount;
        end;
      end
      else if Phase = 4 then
      begin
        TimeDif := GetTickCount - OldTime;
        if Alpha < 100 then
          Adj := Adj + 20 * ( TimeDif / 1000 )
        else if Alpha < 200 then
          Adj := Adj + 52 * ( TimeDif / 1000 )
        else
          Adj := Adj + 172 * ( TimeDif / 1000 );

        OldTime := GetTickCount;
        if Adj >= 1 then
        begin
          application.ProcessMessages;
          Alpha := Alpha + round( Adj );
          application.ProcessMessages;
          if Alpha > 255 then
            Alpha := 255;
                 //DrawAlpha(lpDDSBack,Rect(28, 11, 28+364, 11+190),rect(0,0,369,204),DXSiege,true,Alpha);
//                 DrawAlpha(lpDDSBack,Rect(0, 0, 500, 510),rect(0,0,500,510),DXSiege,true,Alpha);
          for i := 0 to 9 do
          begin
            DrawAlpha( lpDDSBack, Rect( 0, i * 51, 500, i * 51 + 51 ), rect( 0, i * 51, 500, i * 51 + 51 ), DXSiege, true, Alpha );
            application.processmessages;
          end;
          application.ProcessMessages;
          Adj := Adj - Trunc( Adj );
          application.ProcessMessages;
          lpDDSFront.Flip( nil, DDFLIP_WAIT );
          application.ProcessMessages;
          lpDDSBack.BltFast( 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
          application.ProcessMessages;
        end;
        if GetTickCount - StartFinalCount > 9000 then
        begin
          KeepOnPlaying := false;
          if assigned( pMusic ) then
          begin
            pMusic.PauseThisSong;
          end;
                 //sndPlaySound(nil,1);
        end;
      end;
      application.ProcessMessages;
    end; //wend

  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //PlayAnim

procedure TOpenAnim.Release;
const
  FailName : string = 'TOpenAnim.release';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    DXBack := nil;
    DXSiege := nil;
    DXLogo := nil;
    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

end.
