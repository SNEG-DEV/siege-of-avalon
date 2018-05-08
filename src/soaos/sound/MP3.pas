unit MP3;
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
  windows,
  classes,
  logfile,
  sysutils,
  engine;

type
  pFSOUND_STREAM = ^FSOUND_STREAM;
  FSOUND_STREAM = record
    Thing : byte;
  end;

  pFSOUND_SAMPLE = ^FSOUND_SAMPLE;
  FSOUND_SAMPLE = record
    Thing : byte;
  end;

  GAMESOUNDFILEDATA = record
    FileName : string;
    Count : integer;
    pWav : pFSOUND_SAMPLE;
  end;

  GAMESOUNDDATA = record
    Frequency : DWORD;
    Volume : DWORD;
    Pan : DWORD;
    Index : integer;
    Channel : integer;
  end;

  FSOUND_OUTPUTTYPES =
    (
    FSOUND_OUTPUT_NOSOUND, // NoSound driver, all calls to this succeed but do nothing.
    FSOUND_OUTPUT_WINMM, // Windows Multimedia driver.
    FSOUND_OUTPUT_DSOUND, // DirectSound driver.  You need this to get EAX or EAX2 support.
    FSOUND_OUTPUT_A3D // A3D driver.  You need this to get geometry and EAX reverb support.
    );

  FSOUND_MIXERTYPES =
    (
    FSOUND_MIXER_AUTODETECT, // This enables autodetection of the fastest mixer based on your cpu.
    FSOUND_MIXER_BLENDMODE, // This enables the standard non mmx, blendmode mixer.
    FSOUND_MIXER_MMXP5, // This enables the mmx, pentium optimized blendmode mixer.
    FSOUND_MIXER_MMXP6, // This enables the mmx, ppro/p2/p3 optimized mixer.

    FSOUND_MIXER_QUALITY_AUTODETECT, // This enables autodetection of the fastest quality mixer based on your cpu.
    FSOUND_MIXER_QUALITY_FPU, // This enables the interpolating FPU mixer.
    FSOUND_MIXER_QUALITY_MMXP5, // This enables the interpolating p5 MMX mixer.
    FSOUND_MIXER_QUALITY_MMXP6, // This enables the interpolating ppro/p2/p3 MMX mixer.
    FSOUND_MIXER_QUALITY_FPU_VOLUMERAMP // Enables the interpolating 'de-clicking' FPU mixer.
    );

const
  FSOUND_LOOP_OFF = $01; // For non looping samples.
  FSOUND_LOOP_NORMAL = $02; // For forward looping samples.
  FSOUND_NORMAL = $29; // (FSOUND_LOOP_OFF + FSOUND_8BITS + FSOUND_MONO)
  FSOUND_FREE = $FFFFFFFF; // value to play on any free channel, or to allocate a sample in a free sample slot.

function FSOUND_SetOutput( outputtype : FSOUND_OUTPUTTYPES ) : ByteBool; stdcall; external 'fmod.dll' name '_FSOUND_SetOutput@4';
function FSOUND_SetDriver( driver : DWORD ) : ByteBool; stdcall; external 'fmod.dll' name '_FSOUND_SetDriver@4';
function FSOUND_Init( mixrate, maxchannels, vcmmode : DWORD ) : ByteBool; stdcall; external 'fmod.dll' name '_FSOUND_Init@12';
procedure FSOUND_Close; stdcall; external 'fmod.dll' name '_FSOUND_Close@0';
function FSOUND_Stream_OpenMpeg( filename : pChar; mode : ULong ) : pFSOUND_STREAM; stdcall; external 'fmod.dll' name '_FSOUND_Stream_OpenMpeg@8';
function FSOUND_Stream_Play( channel : DWORD; stream : pFSOUND_STREAM ) : DWORD; stdcall; external 'fmod.dll' name '_FSOUND_Stream_Play@8';
function FSOUND_Stream_Stop( stream : pFSOUND_STREAM ) : ByteBool; stdcall; external 'fmod.dll' name '_FSOUND_Stream_Stop@4';
function FSOUND_Stream_Close( stream : pFSOUND_STREAM ) : ByteBool; stdcall; external 'fmod.dll' name '_FSOUND_Stream_Close@4';
function FSOUND_Stream_SetPaused( stream : pFSOUND_STREAM; paused : ByteBool ) : ByteBool; stdcall; external 'fmod.dll' name '_FSOUND_Stream_SetPaused@8';
function FSOUND_SetVolume( channel : DWORD; vol : DWORD ) : ByteBool; stdcall; external 'fmod.dll' name '_FSOUND_SetVolume@8';

function FSOUND_SetMixer( mixer : FSOUND_MIXERTYPES ) : ByteBool; stdcall; external 'fmod.dll' name '_FSOUND_SetMixer@4';
function FSOUND_Sample_LoadWav( index : DWORD; filename : pChar; mode : ULong ) : pFSOUND_SAMPLE; stdcall; external 'fmod.dll' name '_FSOUND_Sample_LoadWav@12';
procedure FSOUND_SetSFXMasterVolume( volume : DWORD ); stdcall; external 'fmod.dll' name '_FSOUND_SetSFXMasterVolume@4';
procedure FSOUND_Sample_Free( sptr : pFSOUND_SAMPLE ); stdcall; external 'fmod.dll' name '_FSOUND_Sample_Free@4';
function FSOUND_Sample_SetDefaults( sptr : pFSOUND_SAMPLE; deffreq, defvol, defpan, defpri : DWORD ) : ByteBool; stdcall; external 'fmod.dll' name '_FSOUND_Sample_SetDefaults@20';
function FSOUND_PlaySound( channel : DWORD; sptr : pFSOUND_SAMPLE ) : DWORD; stdcall; external 'fmod.dll' name '_FSOUND_PlaySound@8';
function FSOUND_PlaySoundAttrib( channel : DWORD; sptr : pFSOUND_SAMPLE; freq, vol, pan : DWORD ) : DWORD; stdcall; external 'fmod.dll' name '_FSOUND_PlaySoundAttrib@20';
function FSOUND_GetFrequency( channel : DWORD ) : DWORD; stdcall; external 'fmod.dll' name '_FSOUND_GetFrequency@4';
function FSOUND_SetFrequency( channel : DWORD; freq : DWORD ) : ByteBool; stdcall; external 'fmod.dll' name '_FSOUND_SetFrequency@8';
function FSOUND_SetPan( channel : DWORD; pan : DWORD ) : ByteBool; stdcall; external 'fmod.dll' name '_FSOUND_SetPan@8';
function FSOUND_Sample_GetDefaults( sptr : pFSOUND_SAMPLE; deffreq, defvol, defpan, defpri : pDWORD ) : ByteBool; stdcall; external 'fmod.dll' name '_FSOUND_Sample_GetDefaults@20';
function FSOUND_Sample_SetLoopMode( sptr : pFSOUND_SAMPLE; loopmode : ULong ) : ByteBool; stdcall; external 'fmod.dll' name '_FSOUND_Sample_SetLoopMode@8';
function FSOUND_StopSound( channel : DWORD ) : ByteBool; stdcall; external 'fmod.dll' name '_FSOUND_StopSound@4';
function FSOUND_SetHWND( HWND : pointer ) : ByteBool; stdcall; external 'fmod.dll' name '_FSOUND_SetHWND@4';
function FSOUND_SetBufferSize( Len_ns : DWORD ) : ByteBool; stdcall; external 'fmod.dll' name '_FSOUND_SetBufferSize@4';
function FSOUND_GetDriverName( id : DWORD ) : pChar; stdcall; external 'fmod.dll' name '_FSOUND_GetDriverName@4';

function CreateDirectSound( WindowHandle : integer ) : boolean;
procedure DestroyDirectSound;
//Music functions
function OpenMP3Song( TheSong : string; Looping : boolean ) : boolean;
function PlayMP3Song : boolean;
procedure PauseMP3Song( PauseIt : boolean );
procedure SetMP3Volume( Volume : integer );
//Sound FX functions
function LoadWavSound( WavFileName : string ) : integer;
function PlayWavSound( Index, Looping : integer ) : integer;
procedure StopWavSound( Index : integer );
function GetSampleRate( index : integer ) : integer;
procedure SetSoundValues( Index, Frequency, Volume, Pan : integer );
procedure DumpAllWavSounds;
procedure DumpWavSound( Index : integer );

procedure DebugPrint( S : string );

var
  MP3PlayerAvailable : boolean;
  MP3SongOpen : boolean;
  SongStream : PFSOUND_STREAM;

implementation
var
  OriginalSound : array[ 0..1000 ] of GAMESOUNDFILEDATA;
  SoundList : array[ 0..5000 ] of GAMESOUNDDATA;
  SoundAssignedToThisChannel : array[ 0..1000 ] of integer;

function CreateDirectSound( WindowHandle : integer ) : boolean;
var
  i : integer;
begin;
  //prepare the Sound arrays for use
  for i := 0 to 1000 do
  begin
    OriginalSound[ i ].FileName := '';
    OriginalSound[ i ].count := 0;
    SoundAssignedToThisChannel[ i ] := -2;
  end;
  for i := 0 to 5000 do
  begin
    SoundList[ i ].index := -1;
    SoundList[ i ].channel := -1;
  end;
  MP3SongOpen := false;
  MP3PlayerAvailable := false;
  Result := false;
//  FSOUND_SetHWND(@WindowHandle);
//  FSOUND_SetMixer(FSOUND_MIXER_QUALITY_FPU_VOLUMERAMP);
//  FSOUND_SetBufferSize(200);

  //DebugPrint(FSOUND_GetDriverName(0));
 // DebugPrint(FSOUND_GetDriverName(1));
 // DebugPrint(FSOUND_GetDriverName(2));


 // if (pos('live',lowercase(FSOUND_GetDriverName(0)))=0) and (pos('live',lowercase(FSOUND_GetDriverName(1)))=0) then begin
  if UseDirectSound then
  begin
    if FSOUND_SetOutput( FSOUND_OUTPUT_DSOUND ) = false then
    begin
      Log.log( 'MP3 SetOutput failed' );
      exit;
    end;
  end
  else
  begin
    if FSOUND_SetOutput( FSOUND_OUTPUT_WINMM ) = false then
    begin
      Log.log( 'MP3 SetOutput failed' );
      exit;
    end;
    if FSOUND_SetDriver( 0 ) = false then
    begin
      Log.log( 'MP3 SetDriver failed' );
      exit;
    end;
  end;
  if FSOUND_Init( 44100, 64, 0 ) = false then
  begin
    Log.log( 'MP3 Init failed' );
    exit;
  end;
  //succeeded
  Log.log( 'MP3 Init Succeeded' );
  MP3PlayerAvailable := true;
  Result := true;
end; //CreateDirectSound

procedure DestroyDirectSound;
begin
  if MP3PlayerAvailable then
  begin
    if MP3SongOpen then
    begin
      FSOUND_Stream_Close( SongStream );
    end;
    FSOUND_Close( );
  end;
end;

function OpenMP3Song( TheSong : string; Looping : boolean ) : boolean;
begin
  MP3SongOpen := false;
  Result := false;
  if MP3PlayerAvailable = true then
  begin
    if Looping then
      SongStream := FSOUND_Stream_OpenMpeg( pchar( TheSong ), FSOUND_LOOP_NORMAL ) //FSOUND_LOOP_NORMAL or FSOUND_NORMAL)
    else
      SongStream := FSOUND_Stream_OpenMpeg( pchar( TheSong ), 0 ); //FSOUND_NORMAL);
    if SongStream = nil then
    begin
      Log.log( 'MP3 SongOpen ' + theSong + ' failed' );
      exit;
    end
    else
    begin
      MP3SongOpen := true;
      Result := true;
      Log.log( 'MP3 SongOpen ' + theSong + ' succeeded' );
    end;
  end;
end; //OpenMP3Song

function PlayMP3Song : boolean;
begin
  result := false;
  if MP3PlayerAvailable and MP3SongOpen then
  begin
    if FSOUND_Stream_Play( 31, SongStream ) = $FFFFFFFF then
    begin
      Log.log( 'PlayMP3Song failed' );
    end
    else
    begin
      result := true;
      Log.log( 'PlayMP3SongSuceeded' )
    end;
  end; //endif

end; //PlayMP3Song

procedure PauseMP3Song( PauseIt : boolean );
begin
  if MP3PlayerAvailable and MP3SongOpen then
    FSOUND_Stream_SetPaused( SongStream, PauseIt );
end; //PauseMP3Song

procedure SetMP3Volume( Volume : integer ); //0 to 255
begin

  if MP3PlayerAvailable and MP3SongOpen then
    FSOUND_SetVolume( 31, Volume );

end; //SetMP3Volume

//DirectSound SupportFunctions

function LoadWavSound( WavFileName : string ) : integer;
var
  i, j, SoundIndex : integer;
  ThisIsANewSound : boolean;
  pSound : pFSOUND_SAMPLE;
  deffreq, defvol, defpan, defpri : DWORD;
begin
  Result := 0; //failure
  SoundIndex := -1;
  ThisIsANewSound := true;
  i := 0;
  while i < 1000 do
  begin //has this sound already been loaded?
    if WavFileName = OriginalSound[ i ].FileName then
    begin
      ThisIsANewSound := false;
      SoundIndex := i;
      i := 9999;
    end;
    inc( i );
  end;
  if ThisIsANewSound then
  begin //new sound
    i := 0;
       //find an open slot in array
    while ( OriginalSound[ i ].count > 0 ) and ( i < 999 ) do
    begin
      inc( i );
    end; //wend
    if i < 999 then
    begin //load the new wav, put it in slot i
      pSound := FSOUND_Sample_LoadWav( i, PChar( WavFileName ), FSOUND_LOOP_OFF );
      if pSound = nil then
      begin
        Log.log( 'Error in LoadWavSound: Unable to loadwavfile ' + WavFileName );
        exit;
      end;
      FSOUND_Sample_GetDefaults( pSound, @deffreq, @defvol, @defpan, @defpri );
      OriginalSound[ i ].FileName := WavFileName;
      OriginalSound[ i ].count := 1;
      OriginalSound[ i ].pWav := pSound;
          //find an open SoundList Slot
      j := 0;
      while ( SoundList[ j ].index > -1 ) and ( j < 999 ) do
      begin
        inc( j );
      end; //wend
      if j < 999 then
      begin //we found an unused slot
        SoundList[ j ].index := i; //OriginalSound Index
        SoundList[ j ].Frequency := deffreq;
        SoundList[ j ].Volume := 255; //defvol;
        SoundList[ j ].Pan := 127; //round((defpan-127)*78.431);
             //if SoundList[j].Pan > 10000 then
             //   SoundList[j].Pan:=10000;
        Result := j;
      end
      else
      begin //error!
        Log.log( 'Error in LoadWavSound: index greater than 999 for SoundList' );
        exit;
      end;
    end
    else
    begin //error
      Log.log( 'Error in LoadWavSound: index greater than 999 for OrignalSound' );
      exit;
    end;
  end
  else
  begin //sound already loaded
      //find an open SoundList Slot
    j := 0;
    while ( SoundList[ j ].index > -1 ) and ( j < 5000 ) do
    begin
      inc( j );
    end; //wend
    if j < 5000 then
    begin //we found an unused slot
      FSOUND_Sample_GetDefaults( OriginalSound[ SoundIndex ].pWav, @deffreq, @defvol, @defpan, @defpri );
      inc( OriginalSound[ SoundIndex ].count );
      SoundList[ j ].index := SoundIndex; //OriginalSound Index
      SoundList[ j ].Frequency := deffreq;
      SoundList[ j ].Volume := 255; //defvol;
         //Pan must be converted to -100 to 100 scale from 0 to 255 (FMOD) - ugly...but thats how the code is now
      SoundList[ j ].Pan := 127; //round((defpan-127)*78.431);
         //if SoundList[j].Pan > 10000 then
         //   SoundList[j].Pan:=10000;
      Result := j;
    end
    else
    begin //error!
      Log.log( 'Error in LoadWavSound: index greater than 999 for SoundList: sound already loaded section' );
      exit;
    end;
  end;

end; //LoadWavSound

function PlayWavSound( Index, Looping : integer ) : integer;
var
  i : integer;
begin
  if SoundList[ Index ].index > -1 then
  begin
    if Looping = 1 then
      FSOUND_Sample_SetLoopMode( OriginalSound[ SoundList[ index ].index ].pWav, FSOUND_LOOP_NORMAL )
    else
      FSOUND_Sample_SetLoopMode( OriginalSound[ SoundList[ index ].index ].pWav, FSOUND_LOOP_OFF );

    i := FSOUND_PlaySoundAttrib( FSOUND_FREE, OriginalSound[ SoundList[ index ].index ].pWav, SoundList[ index ].Frequency, SoundList[ index ].Volume, SoundList[ index ].Pan );
    if i = -1 then
    begin //failure
      Log.log( 'Error PlayWavsound: Unable to play sound' );
      Result := 0;
      exit;
    end
    else
    begin //store channel info so we know what sound is playing in this channel
      i := i and $FFF;
      SoundAssignedToThisChannel[ i ] := Index;
      SoundList[ Index ].channel := i;
      Result := 1;
    end;
  end
  else
  begin //invalid index
    Log.log( 'Error in getSamplerate: invalid index' );
    Result := -1;
    exit;
  end;
end; //PlayWavSound

procedure StopWavSound( Index : integer );
begin
  if ( SoundList[ Index ].index > -1 ) and ( SoundList[ Index ].channel > -1 ) then
  begin
    if SoundAssignedToThisChannel[ SoundList[ Index ].channel ] = Index then
    begin //if this sound is actually playing in this channel
      FSOUND_StopSound( SoundList[ Index ].channel );
      SoundList[ Index ].channel := -1;
      SoundAssignedToThisChannel[ SoundList[ Index ].channel ] := -2;
    end;
  end
  else
  begin //invalid index
    Log.log( 'Error in StopWavSound: invalid index' );
    exit;
  end;
end; //StopWavSound

function GetSampleRate( index : integer ) : integer;
begin
  if SoundList[ Index ].index > -1 then
  begin
    Result := SoundList[ Index ].Frequency;
  end
  else
  begin //invalid index
    Log.log( 'Error in getSamplerate: invalid index' );
    Result := -1;
    exit;
  end;
end; //GetSampleRate

procedure SetSoundValues( Index, Frequency, Volume, Pan : integer );
begin
  if SoundList[ Index ].index > -1 then
  begin
    SoundList[ Index ].Frequency := Frequency;
     //convert Volume from 0 to 100 to 0 to 255
    Volume := round( Volume * 2.5 );
    SoundList[ Index ].Volume := Volume;
     //Convert Pan from -10000 to +10000 to the FMOD scale of 0(left) to 255(right)
    Pan := round( ( Pan + 10000 ) * 0.01275 );
    SoundList[ Index ].Pan := Pan;
     //if this sound is playing in this channel now, change the frequency, volume, pan for that channel
    if SoundList[ Index ].channel <> -1 then
    begin
      if SoundAssignedToThisChannel[ SoundList[ Index ].channel ] = Index then
      begin
        FSOUND_SetVolume( SoundList[ Index ].channel, Volume );
        FSOUND_SetFrequency( SoundList[ Index ].channel, Frequency );
        FSOUND_SetPan( SoundList[ Index ].channel, Pan );
      end;
    end;
  end
  else
  begin //invalid index
    Log.log( 'Error in SetSoundValues: invalid index' );
    exit;
  end;

end; //SetSoundValues

procedure DumpWavSound( Index : integer );
begin
  if SoundList[ Index ].index > -1 then
  begin
       //decrease the sound instance count
    OriginalSound[ SoundList[ Index ].index ].count := OriginalSound[ SoundList[ Index ].index ].count - 1;
       //if this is the last instance of this sound, nuke the original and unload the WAV file
    if OriginalSound[ SoundList[ Index ].index ].count <= 0 then
    begin
      FSOUND_Sample_Free( OriginalSound[ SoundList[ Index ].index ].pWav );
      OriginalSound[ SoundList[ Index ].index ].count := 0;
      OriginalSound[ SoundList[ Index ].index ].FileName := '';
    end;
    SoundList[ Index ].index := -1;
    SoundList[ Index ].channel := -1;
  end
  else
  begin //error
    Log.log( 'Error in DumpWavSound: invalid index' );
    exit;
  end;

end; //DumpWavSound

procedure DumpAllWavSounds;
var
  i : integer;
begin
  for i := 0 to 1000 do
  begin
    if OriginalSound[ i ].count > 0 then
    begin
      FSOUND_Sample_Free( OriginalSound[ i ].pWav );
      OriginalSound[ i ].count := 0;
      OriginalSound[ i ].FileName := '';
    end;
    SoundAssignedToThisChannel[ i ] := -2;
  end; //end for

  for i := 0 to 5000 do
  begin
    SoundList[ i ].index := -1;
    SoundList[ i ].channel := -1;
  end;

end; //DumpAllWavSounds

procedure DebugPrint( S : string );
var
  F : TextFile;
begin

  AssignFile( F, 'd:' + '\mylog.txt' );
  Append( F );
  Write( F, S );
  WriteLn( F );
  CloseFile( F );

end;


end.
