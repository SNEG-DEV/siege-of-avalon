unit Music;
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
  Messages,
  SysUtils,
  Classes,
  Controls,
  StdCtrls,
  ExtCtrls,
  forms,
  Midi,
  MP3,
  LogFile;

type
  rSong = record
    Length : integer;
    Start : single;
  end;

  TMusic = class( TObject )
  private
    // CurrentPlayer: integer;               //Which player is currently playing? 0 or 1
   //  PlayIndex: integer;                   //index to the playlist
   //  PlayList: array [0..500] of integer;  //the song list to play
   //  LengthOfThisLoop: integer;            //Length in milliseconds of playing song segment
   //  OldTime: Longword;                    //Time start of this loop
   //  PauseTime: Longword;                  //used to resume songs at proper point
   //  Song: array [0..50] of rSong;         //data for each song
   //  NumberOfSegments: integer;  //Number of segments to play
   //  WholeSongLength: single;
     //SongTimer: TTimer;
    JustPlayTheSongOnce : boolean;
    SongType : integer; //MP3 or MIDI?
    MidiFileName : string; //if MIDI, save the filename
     //procedure SongTimerEvent(Sender:TObject);
    procedure LoadSongData( SongName : string );
  protected
  public
    procedure SetSongVolume( MusicVolume : integer );
    procedure SetGameVolume( GameVolume : integer ); //does seemingly nothing
    procedure ResumeThisSong;
    procedure PauseThisSong;
    procedure OpenThisSong( SongName : string );
    procedure PlayThisSong;
    constructor Create( WindowHandle : integer );
    destructor Destroy; override;
  end;

var
  MusicLib : TMusic;
  MasterMusicVolume : integer;

implementation

uses
  Sound;

{ TMusic }

constructor TMusic.Create( WindowHandle : integer );

begin
  inherited Create;
  Log.Log( 'Creating music object...' ); Log.flush;
  if not DaSoundCardAvailable then
    exit;
  //create the players
  //CreateDirectSound;
  CreateMidiPlayer;
  SongType := -1;

end; //TMusic.Create

destructor TMusic.Destroy;
const
  FailName : string = 'TMusic.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
  //DestroyDirectSound;
    if DaSoundCardAvailable then
    begin
      StopMidiSong;
      FreeMidi;
    end;
    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //TMusic.Destroy;

procedure TMusic.OpenThisSong( SongName : string );
begin
  if not DaSoundCardAvailable then
    exit;
   //Just in case, stop and close any songs playing
  if MP3SongOpen and ( SongType = 1 ) then
  begin
    FSOUND_Stream_Close( SongStream );
    MP3SongOpen := false;
  end;
  if SongType = 2 then
  begin
    StopMidiSong;
  end;
  SongType := -1;
  if FileExists( SongName ) then
  begin
    SongType := 1; //MP3
       //Open the Mp3 Song
    if MP3PlayerAvailable then
    begin
      SetSongVolume( 0 ); //set volume to zero
      LoadSongData( SongName );
      MP3SongOpen := true;
      if JustPlayTheSongOnce then
        OpenMP3Song( SongName, False ) //Dont loopit
      else
        OpenMP3Song( SongName, True ); //loopit
    end;
  end
  else if FileExists( ChangeFileExt( SongName, '.MID' ) ) then
  begin //if we cant find this file with .MP3 try it with .MID
    SongType := 2;
    MP3SongOpen := false;
    MidiFileName := ChangeFileExt( SongName, '.MID' );
  end
  else
  begin //BIG Problems
    SongType := -1;
      //Music File wasn't found
  end;
end; //TMusic.PlayThisSong

procedure TMusic.PlayThisSong;
begin
   //if not DaSoundCardAvailable or (MasterMusicVolume = 0) then exit;
  if not DaSoundCardAvailable then
    exit;
  if SongType = 1 then
  begin //MP3
    if MP3PlayerAvailable and MP3SongOpen then
    begin
      PlayMP3Song;
    end; //endif
  end
  else if SongType = 2 then
  begin //MIDI
    OpenMidiSong( MidiFileName );
  end
  else
  begin //Song type invalid
      //Music file type invalid
  end;

end; //TMusic.PlayThisSong


procedure TMusic.LoadSongData( SongName : string );
var
  //F: TextFile;
  //a,b: single;
  //NumberOfSongs: integer;
  DatFile : string;
  //SongString,s1: string;
  //i : integer;
begin
  if not DaSoundCardAvailable then
    exit;
  //create the name of the DAT file. The name will always be the song name with .DAT instead of .MP3
  DatFile := StringReplace( SongName, '.mp3', '.dat', [ rfIgnoreCase ] );

  JustPlayTheSongOnce := false;
  if FileExists( DatFile ) then
  begin
   {  AssignFile(F,DatFile);
     Reset(F);
     //Get the length of the entire song in seconds
     Read (F,WholeSongLength);
     //Get the number of song snippets
     Read (F,NumberOfSongs);
     //get the snipper info
     for i:=0 to NumberOfSongs-1 do begin
        Read(F,a,b);
        Song[i].Start:=a;
        Song[i].Length:=round((b-a)*1000);
     end;

     //Now read in the Play List for this song
     Readln(F);
     Readln(F,SongString);
     NumberOfSegments:=0;
     i:=0;
     while (i <= Length(SongString)) do begin
        s1:=SongString[i];
        if (s1 >='1') and (s1 <=IntToStr(NumberOfSongs)) then begin
           PlayList[NumberOfSegments]:=StrToInt(s1)-1;
           NumberOfSegments:=NumberOfSegments+1;
        end;
        i:=i+1;
     end; //wend

     CloseFile(F); }
  end
  else
  begin //didnt find dat file - So just play it once
    JustPlayTheSongOnce := true;
  end;

  //ReloadData;

end; //LoadSongData

{procedure TMusic.SongTimerEvent(Sender:TObject);

begin
   if not DaSoundCardAvailable or (MasterMusicVolume = 0) then exit;
   if GetTickCount - OldTime >= LengthOfThisLoop-20 then begin
      if CurrentPlayer=0 then begin  //if player zero hits the end, start player 1
         OldTime:=GetTickCount;
         LengthOfThisLoop:=Song[PlayList[PlayIndex]].Length;
         //Form1.label2.caption:='Song ' + IntToStr(NextSegment+1) + ', segment ' + IntToStr(SongPosition-1) + ' in this series.';
         PlaySong(1);
         StopSong(0);
         //Get the new Song Index- if we hit the end of the song list, pop back to the start
         PlayIndex:=PlayIndex + 1;
         if PlayIndex = NumberOfSegments then
            PlayIndex:=0;
         //set up the next song to play by seeking to its start to avoid pops and such
         Seek(Song[PlayList[PlayIndex]].Start,0);
         CurrentPlayer:=1;
      end
      else begin   //if player 1 has hit the end, start player 0
         OldTime:=GetTickCount;
         LengthOfThisLoop:=Song[PlayList[PlayIndex]].Length;
         //Form1.label2.caption:='Song ' + IntToStr(Segment+1) + ', segment ' + IntToStr(SongPosition-1) + ' in this series.';
         PlaySong(0);
         StopSong(1);
         PlayIndex:=PlayIndex +1;
         if PlayIndex = NumberOfSegments then
            PlayIndex:=0;
         Seek(Song[PlayList[PlayIndex]].Start,1);
         CurrentPlayer:=0;
      end
   end; //endif GetTickCount
   //TimePassed:=GetTickCount - OldTime;
end;//TMusic.SongTimerEvent
}

procedure TMusic.PauseThisSong;
begin
  if not DaSoundCardAvailable then
    exit;

  if SongType = 1 then
  begin
    PauseMP3Song( True );
  end
  else if SongType = 2 then
  begin //MIDI file- we cant pause, only stop
    StopMidiSong;
  end
  else
  begin //Song not valid
      //Songtype invalid
  end;
end;

procedure TMusic.ResumeThisSong;
begin
  if not DaSoundCardAvailable or ( MasterMusicVolume = 0 ) then
    exit;
  if SongType = 1 then
  begin
    PauseMP3Song( False );
  end
  else if SongType = 2 then
  begin //midisong - no resume
    OpenMidiSong( MidiFileName );
  end
  else
  begin //invalid song type
  end;

end; //resumethissong

procedure TMusic.SetGameVolume( GameVolume : integer );
begin
   //if not DaSoundCardAvailable then exit;
   //SetMasterVolume(GameVolume,0);
   //SetMasterVolume(GameVolume,1);
end;

procedure TMusic.SetSongVolume( MusicVolume : integer );
begin
  if not DaSoundCardAvailable then
    exit;
    //silent is 0, 100 is max volume
  if SongType = 1 then
  begin //Mp3
    MusicVolume := round( MusicVolume * 2.5 ); //set to to zero to 255
    SetMP3Volume( MusicVolume );
    SetMP3Volume( MusicVolume );
  end
  else if SongType = 2 then
  begin //for midi, volume normal is 0, and silent is -5500
    if MusicVolume > 0 then
    begin
      MusicVolume := MusicVolume - 50;
      MusicVolume := MusicVolume * 30; //110
          //MusicVolume:=round(100*ln(MusicVolume/100));
      if MusicVolume < -5500 then
        MusicVolume := -5500;
      SetMidiSongVolume( MusicVolume );
    end
    else
    begin
      SetMidiSongVolume( -5500 );
//          StopMidiSong;
    end;
  end
  else
  begin //invalid song type
  end;
end;

end.
