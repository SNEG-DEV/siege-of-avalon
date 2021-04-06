unit OpenAnim;
(*
  Siege Of Avalon : Open Source Edition

  Portions created by Digital Tome L.P. Texas USA are
  Copyright ©1999-2000 Digital Tome L.P. Texas USA
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

  Revision History:
  - 13 Jul 2003 - DL: Initial Upload to CVS
  - 10 Mar 2019 - SN: Forked on GitHub
  see git repo afterwards

*)

interface

uses
  //  Winapi.DirectDraw,
  DirectX,
  DXEffects,
  System.SysUtils,
  System.Types,
  System.Classes,
  Vcl.Controls,
  Vcl.Forms,
  Display,
  Music,
  Resource,
  SoAOS.Animation,
  LogFile;

type
  TOpenAnim = class( TDisplay )
  private
    LoopCounter : integer;
    KeepOnPlaying : boolean;
    //Bitmap stuff
    DXBack : IDirectDrawSurface;
    DXSiege : IDirectDrawSurface;
    DXLogo : IDirectDrawSurface;
    DXLetterbox: IDirectDrawSurface;
    XAdj, YAdj : integer;
    FCancel : boolean;
    FOffset: TPoint;

    procedure PlayAnim;
  protected
    procedure MouseDown( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer ); override;
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
  SoAOS.Types,
  SoAOS.Graphics.Draw,
  AniDemo;

{ TOpenAnim }

constructor TOpenAnim.Create;
const
  FailName : string = 'TOpenAnim.create';
begin
  Log.DebugLog( FailName );
  try
    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
  FOffset.X := (ScreenMetrics.ScreenWidth - 800) div 2;
  FOffset.Y := (ScreenMetrics.ScreenHeight - 600) div 2;
end; //Create

function OffsetRect(Rect: TRect; Offs: TPoint): TRect;
begin
  Result.Left := Rect.Left + Offs.X;
  Result.Top := Rect.Top + Offs.Y;
  Result.Width := Rect.Width;
  Result.Height := Rect.Height;
end;

destructor TOpenAnim.Destroy;
const
  FailName : string = 'TOpenAnim.destroy';
begin
  Log.DebugLog( FailName );
  try
    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //Destroy

procedure TOpenAnim.Init;
var
  pr : TRect;
const
  FailName : string = 'TOpenAnim.init';
begin
  Log.DebugLog( FailName );
  try
    if Loaded then
      Exit;
    inherited;

    FCancel := false;
    LoopCounter := 0;
    if assigned( pMusic ) then
    begin
  //  sndPlaySound(PChar(SoundPath + 'Theme\IntroTitle.wav'),1);
      pMusic.OpenThisSong( AnsiString ( SoundPath + 'Theme\IntroTitle.MP3' ) );
      pMusic.PlayThisSong;
      pMusic.SetSongVolume( MasterMusicVolume );
    end;
  //Sheet coordinate adjustments
    XAdj := 0;
    YAdj := -20;

    DXSiege := SoAOS_DX_LoadBMP( InterfacePath + 'aniSiege.bmp', cInvisColor );
    DXLogo := SoAOS_DX_LoadBMP( InterfacePath + 'aniDTIPresents.bmp', cInvisColor );
    DXBack := SoAOS_DX_LoadBMP( InterfacePath + 'aniBack.bmp', cInvisColor );
    if ScreenMetrics.ScreenHeight = 720 then
      DXLetterbox := SoAOS_DX_LoadBMP( InterfacePath + 'gMainMenuOverlay720.bmp', cInvisColor )
    else if ScreenMetrics.ScreenHeight = 1080 then
      DXLetterbox := SoAOS_DX_LoadBMP( InterfacePath + 'gMainMenuOverlay1080.bmp', cInvisColor )
    else
      DXLetterBox := nil;

    if DXLetterBox <> nil then
    begin
      pr := Rect( 0, 0, ScreenMetrics.ScreenWidth, ScreenMetrics.ScreenHeight);
      lpDDSBack.BltFast(0, 0, DXLetterbox, @pr, DDBLTFAST_WAIT);
    end;

    pr := Rect( 0, 0, 800, 600 ); //NOHD
    lpDDSBack.BltFast( FOffset.X, FOffset.Y, DXBack, @pr, DDBLTFAST_WAIT );

  //DrawAlpha(lpDDSBack,rect(0,0,800,600),rect(0,0,25,25),DXSiege,false,255);
  //DrawSub(lpDDSBack,rect(0,0,800,600),rect(0,0,25,25),DXSiege,false,255);
    SoAOS_DX_BltFront;
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
  Log.DebugLog( FailName );
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

procedure TOpenAnim.MouseDown( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer );
const
  FailName : string = 'TOpenAnim.Mousedown';
begin
  Log.DebugLog( FailName );
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
  pr : TRect;
const
  FailName : string = 'TOpenAnim.PlayAnim';
begin
  Log.DebugLog( FailName );
  try
    StartFinalCount := 0;
    pr := Rect( 0, 0, 800, 600 );  //NOHD
    lpDDSBack.BltFast( FOffset.X, FOffset.Y, DXBack, @pr, DDBLTFAST_WAIT );
    MusicStillPlaying := true;
    MusicStartTime := TThread.GetTickCount;
    OldTime := TThread.GetTickCount;
    Adj := 0;
    Alpha := 0;
    Phase := 0;
    while KeepOnPlaying do
    begin
      if MusicStillPlaying then
      begin
        if TThread.GetTickCount - MusicStartTime > 19500 then
        begin
               //pMusic.PauseThisSong;
          MusicStillPlaying := false;
        end;
            //else
              //pMusic.SetSongVolume(95);
      end;
      if Phase = 0 then
      begin
        TimeDif := TThread.GetTickCount - OldTime;
        if TimeDif > 1980 then
        begin
          Phase := 1;
        end;
      end
      else if Phase = 1 then
      begin
        application.ProcessMessages;
        TimeDif := TThread.GetTickCount - OldTime;
        if Alpha < 100 then
          Adj := Adj + 24 * ( TimeDif / 1000 )
        else if Alpha < 200 then
          Adj := Adj + 92 * ( TimeDif / 1000 )
        else
          Adj := Adj + 172 * ( TimeDif / 1000 );
        OldTime := TThread.GetTickCount;
        if Adj >= 1 then
        begin
          Alpha := Alpha + round( Adj );

          if Alpha > 254 then
            Alpha := 254;
          application.ProcessMessages;
          for i := 0 to 11 do
          begin
            DrawAlpha( lpDDSBack, OffsetRect(Rect( 0, 250 + i * 19, 800, 250 + i * 19 + 19 ), FOffset), rect( 0, i * 19, 800, i * 19 + 19 ), DXLogo, true, Alpha );  //NOHD
            application.processmessages;
          end;
          application.ProcessMessages;
          Adj := Adj - Trunc( Adj );
          application.ProcessMessages;
          lpDDSFront_Flip( nil, DDFLIP_WAIT );
          application.ProcessMessages;
          SoAOS_DX_BltFastWaitXY( lpDDSFront, Rect( 0, 0, ScreenMetrics.ScreenWidth, ScreenMetrics.ScreenHeight ) );
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
        TimeDif := TThread.GetTickCount - OldTime;
        if Alpha > 100 then
          Adj := Adj + 92 * ( TimeDif / 1000 )
        else
          Adj := Adj + 62 * ( TimeDif / 1000 );
        OldTime := TThread.GetTickCount;
        if Adj >= 1 then
        begin
          application.ProcessMessages;
          Alpha := Alpha - round( Adj );
                 //if Alpha > 100 then Alpha:=Alpha-3;
          if Alpha < 0 then
            Alpha := 0;
          application.ProcessMessages;
          pr := Rect( 0, 250, 800, 250 + 228 );  //NOHD
          lpDDSBack.BltFast( FOffset.X, FOffset.Y + 250, DXBack, @pr, DDBLTFAST_WAIT );
          application.ProcessMessages;
          if Alpha > 0 then
          begin
//                    DrawAlpha(lpDDSBack,Rect(0, 250, 800, 250+228),rect(0,0,800,228),DXLogo,true,Alpha);
            for i := 0 to 11 do
            begin
              DrawAlpha( lpDDSBack, OffsetRect(Rect( 0, 250 + i * 19, 800, 250 + i * 19 + 19 ), FOffset), rect( 0, i * 19, 800, i * 19 + 19 ), DXLogo, true, Alpha ); //NOHD
              application.processmessages;
            end;
          end;
                 //DrawAlpha(lpDDSBack,Rect(0, 250, 800, 250+228),Rect(0, 250, 800, 250+228),DXBack,true,Alpha);
          application.ProcessMessages;
          Adj := Adj - Trunc( Adj );
          lpDDSFront_Flip( nil, DDFLIP_WAIT );
          application.ProcessMessages;
          SoAOS_DX_BltFastWaitXY( lpDDSFront, Rect( 0, 0, ScreenMetrics.ScreenWidth, ScreenMetrics.ScreenHeight ) );
          application.ProcessMessages;
        end;
        if Alpha <= 0 then
        begin
          Phase := 3;
          Adj := 0;
          Alpha := 0;
          OldTime := TThread.GetTickCount;
        end;
      end
      else if Phase = 3 then
      begin
        TimeDif := TThread.GetTickCount - OldTime;
        if TimeDif > 1500 then
        begin
          Phase := 4;
          StartFinalCount := TThread.GetTickCount;
        end;
      end
      else if Phase = 4 then
      begin
        TimeDif := TThread.GetTickCount - OldTime;
        if Alpha < 100 then
          Adj := Adj + 20 * ( TimeDif / 1000 )
        else if Alpha < 200 then
          Adj := Adj + 52 * ( TimeDif / 1000 )
        else
          Adj := Adj + 172 * ( TimeDif / 1000 );

        OldTime := TThread.GetTickCount;
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
            DrawAlpha( lpDDSBack, OffsetRect(Rect( 0, i * 51, 500, i * 51 + 51 ), FOffset), rect( 0, i * 51, 500, i * 51 + 51 ), DXSiege, true, Alpha );
            application.processmessages;
          end;
          application.ProcessMessages;
          Adj := Adj - Trunc( Adj );
          application.ProcessMessages;
          lpDDSFront_Flip( nil, DDFLIP_WAIT );
          application.ProcessMessages;
          SoAOS_DX_BltFastWaitXY( lpDDSFront, Rect( 0, 0, ScreenMetrics.ScreenWidth, ScreenMetrics.ScreenHeight ) );
          application.ProcessMessages;
        end;
        if TThread.GetTickCount - StartFinalCount > 9000 then
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
  Log.DebugLog( FailName );
  try
    DXBack := nil;
    DXSiege := nil;
    DXLogo := nil;
    DXLetterbox := nil;
    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

end.
