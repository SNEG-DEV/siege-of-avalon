unit Midi;
(*
  Siege Of Avalon : Open Source Edition

  Portions created by Digital Tome L.P. Texas USA are
  Copyright ©1999-2000 Digital Tome L.P. Texas USA
  All Rights Reserved.

  Portions created by Team SOAOS are
  Copyright (C) 2003 - Team SOAOS.

  Portions created by Stefen Nyeland are
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
            Soundlib.dll

  Revision History:
  - 13 Jul 2003 - DL: Initial Upload to CVS
  - 10 Mar 2020 - SN: Forked on GitHub
  see git repo afterwards

*)

interface

uses
  SysUtils,
  LogFile;

function CreateMidi : integer; stdcall; external 'Soundlib.dll';
procedure StopMidi; stdcall; external 'Soundlib.dll';
function OpenMidi( FileName : pAnsiChar ) : integer; stdcall; external 'Soundlib.dll';
function PlayMidi : integer; stdcall; external 'Soundlib.dll';
procedure FreeMidi; stdcall; external 'Soundlib.dll';
function SetMidiVolume( Volume : integer ) : integer; stdcall; external 'Soundlib.dll';
function GetMidiVolume : integer; stdcall; external 'Soundlib.dll';

function OpenMidiSong( FileName : AnsiString ) : integer;
function CreateMidiPlayer : integer;
procedure StopMidiSong;
function GetMidiSongVolume : integer;
function SetMidiSongVolume( Volume : integer ) : integer;
function PlayMidiSong : integer;

implementation

var
  MidiIsAvailable : integer;

function CreateMidiPlayer : integer;
const
  FailName : string = 'Midi.CreateMidiPlayer';
begin
  Log.DebugLog( FailName );
  result := 0;
  try
    MidiIsAvailable := CreateMidi;
    Result := MidiIsAvailable;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //CreateMidiPlayer

procedure StopMidiSong;
const
  FailName : string = 'Midi.StopMidiSong';
begin
  Log.DebugLog( FailName );
  try
    if MidiIsAvailable = 1 then
      StopMidi;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //StopMidiSong

function OpenMidiSong( FileName : AnsiString ) : integer;
const
  FailName : string = 'Midi.OpenMidiSong';
begin
  Log.DebugLog( FailName );
  result := 0;
  try
    if MidiIsAvailable = 1 then
      result := OpenMidi( addr( FileName ) )
    else
      result := 0;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //PlayMidiSong

function PlayMidiSong : integer;
const
  FailName : string = 'Midi.PlayMidiSong';
begin
  Log.DebugLog( FailName );
  result := 0;
  try
    result := PlayMidi;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //function PlayMidiSong: integer;

function GetMidiSongVolume : integer;
const
  FailName : string = 'Midi.GetMidiSongVolume';
begin
  Log.DebugLog( FailName );
  result := 0;
  try
    if MidiIsAvailable = 1 then
      result := GetMidiVolume
    else
      result := 0;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //GetMidiSongVolume


function SetMidiSongVolume( Volume : integer ) : integer;
const
  FailName : string = 'Midi.SetMidiSongVolume';
begin
  Log.DebugLog( FailName );
  result := 0;
  try
    if MidiIsAvailable = 1 then
      result := SetMidiVolume( Volume )
    else
      result := 0;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;
end; //SetMidiSongVolume

end.
