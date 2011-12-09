unit sdlaudiomixer;
{******************************************************************************}
{
  $Id: sdlaudiomixer.pas,v 1.1 2005/05/25 23:15:42 savage Exp $
  
}
{                                                                              }
{       Borland Delphi SDL_Mixer - Simple DirectMedia Layer Mixer Library      }
{       Conversion of the Simple DirectMedia Layer Headers                     }
{                                                                              }
{ The initial developer of this Pascal code was :                              }
{ Ariel Jacob <ariel@global-rd.com>                                            }
{                                                                              }
{ Portions created by Ariel are                                                }
{ Copyright (C) 2005  Ariel Jacob.                                             }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{ Dominqiue Louis <Dominique@SavageSoftware.com.au>                            }
{                                                                              }
{ Obtained through:                                                            }
{ Joint Endeavour of Delphi Innovators ( Project JEDI )                        }
{                                                                              }
{ You may retrieve the latest version of this file at the Project              }
{ JEDI home page, located at http://delphi-jedi.org                            }
{                                                                              }
{ The contents of this file are used with permission, subject to               }
{ the Mozilla Public License Version 1.1 (the "License"); you may              }
{ not use this file except in compliance with the License. You may             }
{ obtain a copy of the License at                                              }
{ http://www.mozilla.org/MPL/MPL-1.1.html                                      }
{                                                                              }
{ Software distributed under the License is distributed on an                  }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{ implied. See the License for the specific language governing                 }
{ rights and limitations under the License.                                    }
{                                                                              }
{ Description                                                                  }
{ -----------                                                                  }
{   SDL_Mixer wrapper classed                                                  }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   SDL_Mixer.pas somewhere within your search path.                           }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{
  $Log: sdlaudiomixer.pas,v $
  Revision 1.1  2005/05/25 23:15:42  savage
  Latest Changes

  Revision 1.4  2005/05/13 12:29:35  savage
  Added PlayByName capabilities and completed more of the other music functions.

  Revision 1.3  2005/05/11 12:21:34  savage
  Slight change for music playback.

  Revision 1.2  2005/05/09 22:55:53  savage
  Added MP3 playing capabilities.

  Revision 1.1  2005/02/24 20:42:24  savage
  SDL Audio Class - Thanks to Ariel for his help in the initial implementation.


}
{******************************************************************************}

interface

uses
  Classes,
  Contnrs,
  SysUtils,
  sdl,
  sdl_mixer,
  smpeg;

type
  ESDLAudioException = class( Exception );

  TSDLAudio = class
  private
    FPaused : boolean;
    FFading : TMix_Fading;
    FPlaying : boolean;
    function GetVolume : integer; virtual; abstract;
    procedure SetVolume( const Value : integer ); virtual; abstract;
    function GetFading : TMix_Fading; virtual; abstract;
    function GetPaused : boolean; virtual; abstract;
    function GetPlaying : boolean; virtual; abstract;
  public
    constructor Create( const aFileName : string );

    procedure Play( aLoops : integer = 0 ); virtual;
    procedure Stop; virtual;
    procedure Pause; virtual;
    procedure Resume; virtual;

    procedure FadeIn( aTime : integer; aLoops : integer = 0 ); virtual;
    procedure FadeOut( aTime : integer ); virtual;

    procedure LoadFromFile( const aFileName : string ); virtual; 
    procedure LoadFromStream( aStream : TStream ); virtual; 

    procedure UnLoad; virtual;

    property IsPlaying : boolean read GetPlaying;
    property IsPaused : boolean read GetPaused;
    property Fading : TMix_Fading read GetFading;
    property Volume : integer read GetVolume write SetVolume;
  end;

  ESDLSoundEffect = class( ESDLAudioException );

  TSDLSoundEffect = class( TSDLAudio )
  private
    FPanning : Uint8;
    FDistance : Uint8;
    FAngle : Sint16;
    FMix_Chunk : PMix_Chunk;
    FChannel : integer;
    FGroup : integer;
    function GetFading : TMix_Fading; override;
    function GetPaused : boolean; override;
    function GetPlaying : boolean; override;
    function GetVolume : integer; override;
    procedure SetVolume( const Value : integer ); override;
  public
    destructor Destroy; override;

    procedure FadeIn( aTime : integer; aLoops : integer = 0 ); override;
    procedure FadeOut( aTime : integer ); override;

    procedure LoadFromFile( const aFileName : string ); override;
    procedure LoadFromStream( aStream : TStream ); override;

    procedure SetPanning( val : Uint8 );
    procedure SetDistance( Distance : Uint8 );
    procedure SetPosition( Angle : Sint16; Distance : Uint8 );
    function SetReverseStereo( aFlip : integer ) : integer;

    procedure Play( aLoops : integer = 0 ); override;
    procedure Stop; override;
    procedure Pause; override;
    procedure Resume; override;

    procedure UnLoad; override;
  published
    property Panning : Uint8 read FPanning;
    property Distance : Uint8 read FDistance;
    property Angle : SInt16 read FAngle;
    property Channel : integer read FChannel write FChannel;
    property Group : integer read FGroup;
  end;

  TSDLSoundEffectManager = class( TObjectList )
  private
    FSoundEffectNames : TStringList;
    function GetSoundEffectByName(const aSoundEffectName: string): TSDLSoundEffect;
    procedure SetSoundEffectByName(const aSoundEffectName: string; const aValue: TSDLSoundEffect);
  protected
    function GetItem( aIndex : Integer ) : TSDLSoundEffect;
    procedure SetItem( aIndex : Integer; aObject : TSDLSoundEffect );
  public
    destructor Destroy; override;
    function Add( aObject : TSDLSoundEffect; const aSoundEffectName: string = '' ) : Integer;
    function Extract( aItem : TSDLSoundEffect ) : TSDLSoundEffect; overload;
    function Extract( const aSoundEffectName: string ) : TSDLSoundEffect; overload;
    function Remove( aObject : TSDLSoundEffect ) : Integer; overload;
    function Remove( const aSoundEffectName: string ) : Integer; overload;
    function IndexOf( aObject : TSDLSoundEffect ) : Integer; overload;
    function IndexOf( const aSoundEffectName: string ) : Integer; overload;
    procedure Insert( aIndex : Integer; aObject : TSDLSoundEffect );
    function First : TSDLSoundEffect;
    function Last : TSDLSoundEffect;
    property Items[ Index : Integer ] : TSDLSoundEffect read GetItem write SetItem; default;
    property SoundEffectNames[const SoundEffectName: string]: TSDLSoundEffect read GetSoundEffectByName write SetSoundEffectByName;
  end;

  TMusicFinishedEvent = procedure{$IFNDEF NOT_OO} of object{$ENDIF};

  ESDLMusic = class( ESDLAudioException );

  TSDLMusic = class( TSDLAudio )
  private
    FMix_Music : PMix_Music;
    FPSMPEG : PSMPEG;
    FMusicFinishedEvent : TMusicFinishedEvent;
    FMusicPosition : double;
    FIsMP3 : boolean;
    function GetFading : TMix_Fading; override;
    function GetPaused : boolean; override;
    function GetPlaying : boolean; override;
    function GetVolume : integer; override;
    procedure SetVolume( const Value : integer ); override;
    function GetMusic : TMix_MusicType;
    procedure SetMusicPosition( const Value : double );
    procedure SetFinishedEvent( const Value : TMusicFinishedEvent );
  public
    destructor Destroy; override;

    procedure FadeIn( aTime : integer; aLoops : integer = 0 ); override;
    procedure FadeOut( aTime : integer ); override;

    procedure LoadFromFile( const aFileName : string ); override;
    procedure LoadFromStream( aStream : TStream ); override;

    procedure Play( aLoops : integer = 0 ); override;
    procedure Stop; override;
    procedure Pause; override;
    procedure Resume; override;
    procedure Rewind;

    procedure UnLoad; override;

    property OnMusicFinished : TMusicFinishedEvent read FMusicFinishedEvent write SetFinishedEvent;
    property MusicType : TMix_MusicType read GetMusic;
    property MusicPosition : double read FMusicPosition write SetMusicPosition;
  end;

  TSDLMusicManager = class( TObjectList )
  private
    FCurrentTrack: Integer;
    FTrackNames : TStringList;
    function GetTrackByName(const aTrackName: string): TSDLMusic;
    procedure SetTrackByName(const aTrackName: string; const aValue: TSDLMusic);
  protected
    function GetItem( aIndex : Integer ) : TSDLMusic;
    procedure SetItem( aIndex : Integer; aObject : TSDLMusic );
  public
    destructor Destroy; override;
    function Add( aObject : TSDLMusic; const aTrackName: string = '' ) : Integer;
    function Extract( aItem : TSDLMusic ) : TSDLMusic; overload;
    function Extract( const aTrackName: string ) : TSDLMusic; overload;
    function Remove( aObject : TSDLMusic ) : Integer; overload;
    function Remove( const aTrackName: string ) : Integer; overload;
    function IndexOf( aObject : TSDLMusic ) : Integer; overload;
    function IndexOf( const aTrackName: string ) : Integer; overload;
    procedure Insert( aIndex : Integer; aObject : TSDLMusic );
    function First : TSDLMusic;
    function Last : TSDLMusic;
    property Items[ Index : Integer ] : TSDLMusic read GetItem write SetItem; default;
    property TrackNames[const TrackName: string]: TSDLMusic read GetTrackByName write SetTrackByName;
  end;

  TSDLAudioManager = class( TObject )
  private
    FMusicManager : TSDLMusicManager;
    FSoundEffectManager : TSDLSoundEffectManager;
  public
    constructor Create( frequency : integer = MIX_DEFAULT_FREQUENCY; format : Uint16 = MIX_DEFAULT_FORMAT; channels : integer = MIX_DEFAULT_CHANNELS ; chunksize : integer = 1024 );
    destructor Destroy; override;

    property MusicManager : TSDLMusicManager read FMusicManager write FMusicManager;
    property SoundEffectManager : TSDLSoundEffectManager read FSoundEffectManager write FSoundEffectManager;
  end;

implementation

{ TSDLAudio }

constructor TSDLAudio.Create( const aFileName : string );
begin
  inherited Create;

  FPaused := false;
  FFading := MIX_NO_FADING;
  FPlaying := false;

  LoadFromFile( aFileName );
end;

procedure TSDLAudio.FadeIn( aTime, aLoops : integer );
begin
  FFading := MIX_FADING_IN;
end;

procedure TSDLAudio.FadeOut( aTime : integer );
begin
  FFading := MIX_FADING_OUT;
end;

procedure TSDLAudio.LoadFromFile(const aFileName: string);
begin
  UnLoad;
end;

procedure TSDLAudio.LoadFromStream(aStream: TStream);
begin
  UnLoad;
end;

procedure TSDLAudio.Pause;
begin
  FPlaying := false;
  FPaused := true;
end;

procedure TSDLAudio.Play( aLoops : integer );
begin
  FPlaying := true;
  FPaused := false;
end;

procedure TSDLAudio.Resume;
begin
  FPlaying := true;
  FPaused := false;
end;

procedure TSDLAudio.Stop;
begin
  FPlaying := false;
  FPaused := true;
end;

procedure TSDLAudio.UnLoad;
begin
  Stop;
end;

{ TSDLSoundEffect }

destructor TSDLSoundEffect.Destroy;
begin
  UnLoad;
  inherited;
end;

function TSDLSoundEffect.GetFading : TMix_Fading;
begin
  result := Mix_FadingChannel( FChannel );
end;

procedure TSDLSoundEffect.LoadFromFile( const aFileName : string );
begin
  inherited;
  FMix_Chunk := Mix_LoadWAV( PChar( aFileName ) );
end;

procedure TSDLSoundEffect.LoadFromStream( aStream : TStream );
begin
  inherited;
  
end;

procedure TSDLSoundEffect.SetPanning( val : Uint8 );
begin
  FPanning := val;
  Mix_SetPanning( FChannel, val, 255 - val );
end;

procedure TSDLSoundEffect.SetDistance( Distance : Uint8 );
begin
  FDistance := Distance;
  Mix_SetDistance( FChannel, FDistance );
end;

procedure TSDLSoundEffect.SetPosition( Angle : Sint16; Distance : Uint8 );
begin
  FAngle := Angle;
  FDistance := Distance;
  Mix_SetPosition( FChannel, FAngle, FDistance );
end;

procedure TSDLSoundEffect.UnLoad;
begin
  inherited;
  Mix_FreeChunk( FMix_Chunk );
end;

procedure TSDLSoundEffect.Pause;
begin
  inherited;
  Mix_Pause( FChannel );
end;

procedure TSDLSoundEffect.Play( aLoops : integer );
begin
  inherited;
  Mix_PlayChannel( FChannel, FMix_Chunk, aLoops );
end;

procedure TSDLSoundEffect.Resume;
begin
  inherited;
  Mix_Resume( FChannel );
end;

procedure TSDLSoundEffect.Stop;
begin
  inherited;
  Mix_HaltChannel( FChannel );
end;

function TSDLSoundEffect.GetPaused : boolean;
begin
  result := ( Mix_Paused( FChannel ) = 1 );
end;

procedure TSDLSoundEffect.FadeIn( aTime, aLoops : integer );
begin
  inherited;
  Mix_FadeInChannel( FChannel, FMix_Chunk, aLoops, aTime );
end;

procedure TSDLSoundEffect.FadeOut( aTime : integer );
begin
  inherited;
  Mix_FadeOutChannel( FChannel, aTime );
end;

procedure TSDLSoundEffect.SetVolume( const Value : integer );
begin
  Mix_VolumeChunk( FMix_Chunk, Value );
end;

function TSDLSoundEffect.GetVolume : integer;
begin
  result := Mix_VolumeChunk( FMix_Chunk, -1 );
end;

function TSDLSoundEffect.SetReverseStereo( aFlip : integer ) : integer;
begin
  result := Mix_SetReverseStereo( FChannel, aFlip );
end;

function TSDLSoundEffect.GetPlaying: boolean;
begin
  result := ( Mix_Playing( FChannel ) = 1 );
end;

{ TSDLMusic }

destructor TSDLMusic.Destroy;
begin
  UnLoad;
  inherited;
end;

procedure TSDLMusic.FadeIn( aTime, aLoops : integer );
begin
  inherited;
  Mix_FadeInMusic( FMix_Music, aLoops, aTime );
end;

procedure TSDLMusic.FadeOut( aTime : integer );
begin
  inherited;
  Mix_FadeOutMusic( aTime );
end;

function TSDLMusic.GetFading : TMix_Fading;
begin
  result := Mix_FadingMusic;
end;

function TSDLMusic.GetMusic : TMix_MusicType;
begin
  //result := Mix_GetMusicType( FMix_Music );
  if FIsMP3 then
    result := MUS_MP3
  else
    result := FMix_Music.type_;
end;

function TSDLMusic.GetPaused : boolean;
begin
  if FIsMP3 then
    result := SMPEG_status( FPSMPEG ) = STATUS_SMPEG_STOPPED
  else
    result := ( Mix_PausedMusic = 1 );
end;

function TSDLMusic.GetPlaying: boolean;
begin
  if FIsMP3 then
    result := SMPEG_status( FPSMPEG ) = STATUS_SMPEG_PLAYING
  else
    result := ( Mix_PlayingMusic = 1 );
end;

function TSDLMusic.GetVolume : integer;
begin
  if FIsMP3 then
    result := -1//SMPEG_setvolume( FPSMPEG, Value )
  else
    result := Mix_VolumeMusic( -1 );
end;

procedure TSDLMusic.LoadFromFile( const aFileName : string );
var
  // Audio Specs
  aspec : TSDL_AudioSpec;
  format : Uint16;
  freq, chan : integer;
begin
  inherited;
  FIsMP3 := LowerCase( ExtractFileExt( aFileName ) ) = '.mp3';
  if FIsMP3 then
  begin
    FPSMPEG := SMPEG_new( PChar( aFileName ), nil, 0 );

    // Disable Audio
    SMPEG_enableaudio( FPSMPEG, 0 );

    // Query Mixer
    Mix_QuerySpec( freq, format, chan );
    aspec.freq := freq;
    aspec.format := format;
    aspec.channels := chan;

    // Tell Smpeg what we want
    Smpeg_actualSpec( FPSMPEG, @aspec );

    // Hook the mixer audio playing function
    Mix_HookMusic( @SMPeg_PlayAudioSDL, FPSMPEG );

    // Reenable Audio
    SMPEG_enableaudio( FPSMPEG, 1 );
  end
  else
    FMix_Music := Mix_LoadMUS( PChar( aFileName ) );
end;

procedure TSDLMusic.LoadFromStream( aStream : TStream );
begin
  inherited;
  
end;

procedure TSDLMusic.Pause;
begin
  inherited;
  if FIsMP3 then
    SMPEG_pause( FPSMPEG )
  else
    Mix_PauseMusic;
end;

procedure TSDLMusic.Play( aLoops : integer );
begin
  inherited;
  if FIsMP3 then
  begin
    SMPEG_play( FPSMPEG );
    SMPEG_loop( FPSMPEG, aLoops );
  end
  else
  begin
    if ( Mix_PlayingMusic = 0 ) then
      Mix_PlayMusic( FMix_Music, aLoops );
  end;
end;

procedure TSDLMusic.Resume;
begin
  inherited;
  if FIsMP3 then
    SMPEG_pause( FPSMPEG )
  else
    Mix_ResumeMusic;
end;

procedure TSDLMusic.Rewind;
begin
  if FIsMP3 then
    SMPEG_rewind( FPSMPEG )
  else
    Mix_RewindMusic;
end;

procedure TSDLMusic.SetFinishedEvent( const Value : TMusicFinishedEvent );
begin
  FMusicFinishedEvent := Value;
  Mix_HookMusicFinished( @FMusicFinishedEvent );
end;

procedure TSDLMusic.SetMusicPosition( const Value : double );
begin
  if FMusicPosition <> Value then
  begin
    FMusicPosition := Value;
    if ( Mix_SetMusicPosition( Value ) = -1 ) then
      raise ESDLMusic.CreateFmt( 'Mix_SetMusicPosition : %s', [ Mix_GetError ] );
  end;
end;

procedure TSDLMusic.SetVolume( const Value : integer );
begin
  if FIsMP3 then
    SMPEG_setvolume( FPSMPEG, Value )
  else
    Mix_VolumeMusic( Value );
end;

procedure TSDLMusic.Stop;
begin
  inherited;
  if FIsMP3 then
    SMPEG_stop( FPSMPEG )
  else
    Mix_HaltMusic;
end;

procedure TSDLMusic.UnLoad;
begin
  inherited;
  if FIsMP3 then
  begin
    // Unhook mixer audio playback function
    Mix_HookMusic( nil, nil );
    
    SMPEG_delete( FPSMPEG )
  end
  else
    Mix_FreeMusic( FMix_Music );
end;

{ TSDLSoundEffectManager }

function TSDLSoundEffectManager.Add( aObject : TSDLSoundEffect; const aSoundEffectName: string ) : Integer;
begin
  if FSoundEffectNames = nil then
    FSoundEffectNames := TStringList.Create;

  if ( aSoundEffectName <> '' ) then
  begin
    if ( FSoundEffectNames.IndexOf( aSoundEffectName ) <> -1 ) then
    begin
      aObject.Free;
      result := -1;
      exit;
    end;
    FSoundEffectNames.Add( aSoundEffectName );
  end;

  result := inherited Add( aObject );
end;

function TSDLSoundEffectManager.Extract( aItem : TSDLSoundEffect ) : TSDLSoundEffect;
begin
  result := TSDLSoundEffect( inherited Extract( aItem ) );
  FSoundEffectNames.Delete( IndexOf( aItem ) );
end;

destructor TSDLSoundEffectManager.Destroy;
begin
  if FSoundEffectNames <> nil then
    FSoundEffectNames.Free;
  inherited;
end;

function TSDLSoundEffectManager.Extract( const aSoundEffectName: string): TSDLSoundEffect;
begin
  result := Extract( Items[ FSoundEffectNames.IndexOf( aSoundEffectName ) ] );
end;

function TSDLSoundEffectManager.First : TSDLSoundEffect;
begin
  result := TSDLSoundEffect( inherited First );
end;

function TSDLSoundEffectManager.GetItem( aIndex : Integer ) : TSDLSoundEffect;
begin
  result := TSDLSoundEffect( inherited GetItem( aIndex ) );
end;

function TSDLSoundEffectManager.GetSoundEffectByName(
  const aSoundEffectName: string): TSDLSoundEffect;
begin
  result := Items[ FSoundEffectNames.IndexOf( aSoundEffectName ) ];
end;

function TSDLSoundEffectManager.IndexOf( aObject : TSDLSoundEffect ) : Integer;
begin
  result := inherited IndexOf( aObject );
end;

function TSDLSoundEffectManager.IndexOf( const aSoundEffectName: string): Integer;
begin
  result := FSoundEffectNames.IndexOf( aSoundEffectName );
end;

procedure TSDLSoundEffectManager.Insert( aIndex : Integer; aObject : TSDLSoundEffect );
begin
  inherited Insert( aIndex, aObject );
end;

function TSDLSoundEffectManager.Last : TSDLSoundEffect;
begin
  result := TSDLSoundEffect( inherited Last );
end;

function TSDLSoundEffectManager.Remove( aObject : TSDLSoundEffect ) : Integer;
begin
  result := inherited Remove( aObject );
  FSoundEffectNames.Delete( result );
end;

function TSDLSoundEffectManager.Remove( const aSoundEffectName: string): Integer;
begin
  result := Remove( Items[ FSoundEffectNames.IndexOf( aSoundEffectName ) ] );
end;

procedure TSDLSoundEffectManager.SetItem( aIndex : Integer; aObject : TSDLSoundEffect );
begin
  inherited SetItem( aIndex, aObject );
end;

procedure TSDLSoundEffectManager.SetSoundEffectByName(const aSoundEffectName: string; const aValue: TSDLSoundEffect);
var
  I: Integer;
begin
  I := FSoundEffectNames.IndexOf(aSoundEffectName);
  if aValue <> nil then
  begin
    if I < 0 then
      Add( aValue, aSoundEffectName )
    else
      Items[ I ] := aValue;
  end
  else
  begin
    if I >= 0 then
      Remove( Items[ I ] );
  end;
end;

{ TSDLMusicManager }

function TSDLMusicManager.Add( aObject : TSDLMusic; const aTrackName: string ) : Integer;
begin
  if FTrackNames = nil then
    FTrackNames := TStringList.Create;

  if ( aTrackName <> '' ) then
  begin
    if ( FTrackNames.IndexOf( aTrackName ) <> -1 ) then
    begin
      aObject.Free;
      result := -1;
      Exit;
    end;
    FTrackNames.Add( aTrackName );
  end;

  result := inherited Add( aObject );
  FCurrentTrack := result;
end;

destructor TSDLMusicManager.Destroy;
begin
  if FTrackNames <> nil then
    FTrackNames.Free;
  inherited;
end;

function TSDLMusicManager.Extract( aItem : TSDLMusic ) : TSDLMusic;
begin
  result := TSDLMusic( inherited Extract( aItem ) );
  FTrackNames.Delete( IndexOf( aItem ) );
end;

function TSDLMusicManager.Extract(const aTrackName: string): TSDLMusic;
begin
  result := Extract( Items[ FTrackNames.IndexOf( aTrackName ) ] );
end;

function TSDLMusicManager.First : TSDLMusic;
begin
  result := TSDLMusic( inherited First );
end;

function TSDLMusicManager.GetItem( aIndex : Integer ) : TSDLMusic;
begin
  result := TSDLMusic( inherited GetItem( aIndex ) );
end;

function TSDLMusicManager.GetTrackByName(const aTrackName: string): TSDLMusic;
begin
  result := Items[ FTrackNames.IndexOf( aTrackName ) ];
end;

function TSDLMusicManager.IndexOf( aObject : TSDLMusic ) : Integer;
begin
  result := inherited IndexOf( aObject );
end;

function TSDLMusicManager.IndexOf(const aTrackName: string): Integer;
begin
  result := FTrackNames.IndexOf( aTrackName );
end;

procedure TSDLMusicManager.Insert( aIndex : Integer; aObject : TSDLMusic );
begin
  inherited Insert( aIndex, aObject );
end;

function TSDLMusicManager.Last : TSDLMusic;
begin
  result := TSDLMusic( inherited Last );
end;

function TSDLMusicManager.Remove( aObject : TSDLMusic ) : Integer;
begin
  result := inherited Remove( aObject );
  FTrackNames.Delete( result );
end;

function TSDLMusicManager.Remove(const aTrackName: string): Integer;
begin
  result := Remove( Items[ FTrackNames.IndexOf( aTrackName ) ] );
end;

procedure TSDLMusicManager.SetItem( aIndex : Integer; aObject : TSDLMusic );
begin
  inherited SetItem( aIndex, aObject );
end;

procedure TSDLMusicManager.SetTrackByName(const aTrackName: string; const aValue: TSDLMusic);
var
  I: Integer;
begin
  I := FTrackNames.IndexOf( aTrackName );
  if aValue <> nil then
  begin
    if I < 0 then
      Add( aValue, aTrackName )
    else
      Items[ I ] := aValue;
  end
  else
  begin
    if I >= 0 then
      Remove( Items[ I ] );
  end;
end;

{ TSDLAudioManager }

constructor TSDLAudioManager.Create( frequency : integer; format : Uint16;
  channels, chunksize : integer );
begin
  inherited Create;

  if ( Mix_OpenAudio( frequency, format, channels, chunksize ) < 0 ) then
    raise ESDLAudioException.CreateFmt( 'Mix_OpenAudio : %s', [ Mix_GetError ] );

  FMusicManager := TSDLMusicManager.Create;
  FSoundEffectManager := TSDLSoundEffectManager.Create;
end;

destructor TSDLAudioManager.Destroy;
begin
  if FMusicManager <> nil then
    FMusicManager.Free;
  if FSoundEffectManager <> nil then
    FSoundEffectManager.Free;

  inherited;
end;

initialization
  if ( SDL_WasInit( SDL_INIT_AUDIO ) = 0 ) then
    SDL_InitSubSystem( SDL_INIT_AUDIO );

finalization
  SDL_QuitSubSystem( SDL_INIT_AUDIO );

end.

