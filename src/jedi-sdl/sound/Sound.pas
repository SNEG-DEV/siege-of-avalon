unit Sound;
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
  Classes,
  SysUtils,
  Resource,
  MP3,
  LogFile;

type
  TSound = class( TObject )
  private
//    SoundList: TStringList;   +Pan Left not working+sound props for all objects
    SoundParser : TStringList;
    FFrequency : integer;
    InternalFrequency : integer;
    InternalVolume : integer;
    FVolume : integer;
    procedure SetVolume( const Value : integer );
  protected
  public
    function PlaySound( const Sounds : TDynamicSmallIntArray; Looping, Volume, Pan, Frequency : integer ) : integer;
    function PlaySoundByIndex( Index : integer; Looping, Volume, Pan, Frequency : integer ) : integer;
    function OpenSound( const NameList : string; InstanceCount : integer; var NameListCount : integer ) : TDynamicSmallIntArray;
    procedure StopSound( Index : integer );
    procedure FreeSound( var Sounds : TDynamicSmallIntArray );
    procedure FreeAllSounds;
    procedure SetSound( Index, Frequency, Volume, Pan : integer );
    constructor Create( WindowHandle : integer );
    destructor Destroy; override;
    property Volume : integer read FVolume write SetVolume;
  end;

//Function CreateDirectSound(WindowHandle: integer): integer; stdcall; external 'Soundlib.dll';
//Function DestroyDirectSound(): integer; stdcall; external 'Soundlib.dll';
{Function LoadWavSound(FileName: pchar): integer; stdcall; external 'Soundlib.dll';
Function PlayWavSound(Index, Looping: integer): integer; stdcall; external 'Soundlib.dll';
Procedure StopWavSound(Index: integer); stdcall; external 'Soundlib.dll';
Procedure SetSoundValues(Index, Frequency, Volume, Pan: integer); stdcall; external 'Soundlib.dll';
Procedure DumpWavSound(Index: integer); stdcall; external 'Soundlib.dll';
Procedure DumpAllWavSounds(); stdcall; external 'Soundlib.dll';
Function GetSampleRate(Index: integer):integer; stdcall; external 'Soundlib.dll';
}

var
  SoundLib : TSound;
  MasterSoundVolume : integer;
  DaSoundCardAvailable : boolean;

implementation

{ TSound }

constructor TSound.Create( WindowHandle : integer );
const
  FailName : string = 'TSound.create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited Create;
    Log.Log( 'Creating sound object...' ); Log.flush;
    FFrequency := 100;
    InternalFrequency := 22050;
    DaSoundCardAvailable := CreateDirectSound( WindowHandle ); //This load DirectSound //CreateDirectSound(WindowHandle);

   {if i = o then
      DaSoundCardAvailable:=true
   else
      DaSoundCardAvailable:=false;
   }
    SoundParser := TStringList.create;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //Create

destructor TSound.Destroy;
const
  FailName : string = 'TSound.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    SoundParser.free;
    if DaSoundCardAvailable then
    begin
      DumpAllWavSounds;
      DestroyDirectSound;
    end;
    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //Destroy

function TSound.OpenSound( const NameList : string; InstanceCount : integer; var NameListCount : integer ) : TDynamicSmallIntArray;
var
  i, j, k : longint;
  S : string;
const
  FailName : string = 'TSound.OpenSound';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  result := nil;
  try
    if NameList = '' then
      exit;
    SoundParser.CommaText := NameList;
    NameListCount := SoundParser.count;
    SetLength( result, NameListCount * InstanceCount );
    for k := 0 to InstanceCount - 1 do
    begin
      for i := 0 to NameListCount - 1 do
      begin
        S := SoundPath + 'sfx/' + SoundParser.strings[ i ] + '.wav';
        if FileExists( S ) then
        begin
          if DaSoundCardAvailable then
            j := LoadWavSound( S ) + 1
          else
            J := 0;
        end
        else
          j := 0;
        Result[ k * NameListCount + i ] := j;
      end;
    end;
    SoundParser.clear;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //OpenSound

function TSound.PlaySound( const Sounds : TDynamicSmallIntArray; Looping, Volume, Pan, Frequency : integer ) : integer;
var
  i, Index : longint;
const
  FailName : string = 'TSound.PlaySound';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  Result := 0;
  try
    if DaSoundCardAvailable and ( MasterSoundVolume > 0 ) then
    begin
      if Sounds = nil then
        exit;
      if length( Sounds ) > 0 then
      begin
        i := random( length( Sounds ) );
        Index := Sounds[ i ];
        if Index <= 0 then
          exit;
        InternalFrequency := ( GetSampleRate( Index - 1 ) * Frequency ) div 100;
        self.Volume := Volume;
        if DaSoundCardAvailable then
        begin
          SetSoundValues( Index - 1, InternalFrequency, InternalVolume, Pan );
          PlayWavSound( Index - 1, Looping );
        end;
        Result := Index;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //PlaySound

function TSound.PlaySoundByIndex( Index, Looping, Volume, Pan, Frequency : integer ) : integer;
const
  FailName : string = 'TSound.PlaySoundByIndex';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  Result := -1;
  try
    if DaSoundCardAvailable and ( MasterSoundVolume > 0 ) then
    begin
      if Index <= 0 then
        exit;
      InternalFrequency := ( GetSampleRate( Index - 1 ) * Frequency ) div 100;
      self.Volume := Volume;
      if DaSoundCardAvailable then
      begin
        SetSoundValues( Index - 1, InternalFrequency, InternalVolume, Pan );
        Result := PlayWavSound( Index - 1, Looping );
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //PlaySoundByIndex

procedure TSound.SetSound( Index, Frequency, Volume, Pan : integer );
const
  FailName : string = 'TSound.SetSound';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if DaSoundCardAvailable then
    begin
      if Index <= 0 then
        exit;
      InternalFrequency := ( GetSampleRate( Index - 1 ) * Frequency ) div 100;
      self.Volume := Volume;
      if DaSoundCardAvailable then
        SetSoundValues( Index - 1, InternalFrequency, InternalVolume, Pan );
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //SetSound

procedure TSound.StopSound( Index : integer );
const
  FailName : string = 'TSound.';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if DaSoundCardAvailable then
    begin
      if Index > 0 then
        StopWavSound( Index - 1 );
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //StopSound

procedure TSound.FreeAllSounds;
const
  FailName : string = 'TSound.FreeAllSounds';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if DaSoundCardAvailable then
      DumpAllWavSounds( );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //FreeAllSounds

procedure TSound.FreeSound( var Sounds : TDynamicSmallIntArray );
var
  i : integer;
const
  FailName : string = 'TSound.FreeSound';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if DaSoundCardAvailable then
    begin
      if Sounds = nil then
        exit;
      for i := 0 to high( Sounds ) do
      begin
        if DaSoundCardAvailable then
        begin
          if Sounds[ i ] > 0 then
            DumpWavSound( Sounds[ i ] - 1 );
        end;
      end;
    end;
    Sounds := nil;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end; //FreeSound

procedure TSound.SetVolume( const Value : integer );
const
  FailName : string = 'TSound.SetVolume';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if DaSoundCardAvailable then
    begin
      FVolume := Value;
      if FVolume <= 0 then
      begin
        InternalVolume := 0; //-10000;
      end
      else
      begin
        if FVolume > 100 then
          FVolume := 100;
        try
          InternalVolume := 0; //-10000;
          InternalVolume := 255 * FVolume div 100; //round(ln(FVolume/100)*1000);
        except
          on E : Exception do
            Log.log( '*** Error: SetVolume: ' + E.Message );
        end;
      //if InternalVolume<-10000 then InternalVolume:=-10000;
        if InternalVolume < 0 then
          InternalVolume := 0;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

end.

