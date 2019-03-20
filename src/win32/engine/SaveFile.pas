unit SaveFile;
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
  Windows,
  SysUtils,
  Classes,
  LogFile;

type
  TSavBlocks = ( sbMap, sbMapKnown, sbCharacter, sbItem, sbTravel, sbJournal, scAbstract, scSoundPlayer, scPathCorner, scTrigger, scSpriteObject,
    scItem, scWeapon, scQuiver, scBow, scContainer, scDoor, scCharacter, sbStartJournalIndex, sbQuest, siItem, sbAdventure,
    sbDeathScreen, sbStartQuestIndex, sbStartAdventureIndex, scEventTimer, sbIndex, sbMaxPartyMembers );

  PBlockPointer = ^TBlockPointer;
  TBlockPointer = record
    DataPos : longint;
    DataSize : longint;
    MapKnownPos : longint;
    MapKnownSize : longint;
  end;

  TSavFile = class( TObject )
  private
    CurrentBlockPointer : PBlockPointer;
    FCurrentMap : string;
    MapIndex : TStringList;
    MapStream : TmemoryStream;
    CreatedPartyMembers : boolean;
    CreatedTravelList : boolean;
    CreatedJournalList : boolean;
    CreatedAdventureList : boolean;
    CreatedQuestList : boolean;
    CreatedMapKnown : boolean;
    CreatedProperties : boolean;
    NewFormat : boolean;
    NewFile : boolean;
    NeedProperties : boolean;
    NeedMapKnown : boolean;

    FPartyMembers : TMemoryStream;
    FTravelList : TStringList;
    FJournalList : TStringList;
    FAdventureList : TStringList;
    FQuestList : TStringList;
    FMapKnown : TMemoryStream;
    FProperties : TMemoryStream;

    FFilename : string;
    NewFilename : string;

    procedure SetCurrentMap( const Value : string );

    function GetPartyMembers : TMemoryStream;
    function GetTravelList : TStringList;
    function GetJournalList : TStringList;
    function GetAdventureList : TStringList;
    function GetQuestList : TStringList;
    function GetMapKnown : TMemoryStream;
    function GetProperties : TMemoryStream;

    procedure SetPartyMembers( const Value : TMemoryStream );
    procedure SetTravelList( const Value : TStringList );
    procedure SetJournalList( const Value : TStringList );
    procedure SetAdventure( const Value : TStringList );
    procedure SetQuestList( const Value : TStringList );
    procedure SetMapKnown( const Value : TMemoryStream );
    procedure SetProperties( const Value : TMemoryStream );
  public
    CurrentScene : string;
    JournalIndex : integer;
    QuestIndex : integer;
    AdventureIndex : integer;
    MaxPartyMembers : integer;
    DeathScreen : string;
    MapName : string;
    SceneName : string;
    constructor Create;
    destructor Destroy; override;
    procedure Open( const Filename : string );
    procedure Load;
    procedure Save;
    procedure SaveAs( const Filename : string );
    procedure Close;
    property CurrentMap : string read FCurrentMap write SetCurrentMap;
    property MapKnown : TMemoryStream read GetMapKnown write SetMapKnown;
    property Properties : TMemoryStream read GetProperties write SetProperties;
    property PartyMembers : TMemoryStream read GetPartyMembers write SetPartyMembers;
    property TravelList : TStringList read GetTravelList write SetTravelList;
    property JournalList : TStringList read GetJournalList write SetJournalList;
    property AdventureList : TStringList read GetAdventureList write SetAdventure;
    property QuestList : TStringList read GetQuestList write SetQuestList;
    property Filename : string read FFilename;
  end;

const
  EOBMarker = $4242;

implementation

{ TSavFile }

procedure TSavFile.Close;
var
  i : integer;
  BlockPointer : PBlockPointer;
begin
  for i := 0 to MapIndex.count - 1 do
  begin
    BlockPointer := pointer( MapIndex.objects[ i ] );
    Dispose( BlockPointer );
  end;
  MapIndex.free;
  MapIndex := nil;
  if CreatedAdventureList then
  begin
    FAdventureList.free;
    FAdventureList := nil;
  end;
  if CreatedJournalList then
  begin
    FJournalList.free;
    FJournalList := nil;
  end;
  if CreatedPartyMembers then
  begin
    FPartyMembers.free;
    FPartyMembers := nil;
  end;
  if CreatedQuestList then
  begin
    FQuestList.free;
    FQuestList := nil;
  end;
  if CreatedTravelList then
  begin
    FTravelList.free;
    FTravelList := nil;
  end;
  if CreatedMapKnown then
  begin
    FMapKnown.free;
    FMapKnown := nil;
  end;
  if CreatedProperties then
  begin
    FProperties.free;
    FProperties := nil;
  end;
  MapStream.free;
  MapStream := nil;
end;

constructor TSavFile.Create;
begin
  inherited;
  MapIndex := TStringList.create;
  NeedProperties := true;
  NeedMapKnown := true;
end;

destructor TSavFile.Destroy;
begin
  Close;
  inherited;
end;

function TSavFile.GetAdventureList : TStringList;
begin
  if not assigned( FAdventureList ) then
  begin
    FAdventureList := TStringList.create;
    CreatedAdventureList := true;
  end;
  result := FAdventureList;
end;

function TSavFile.GetJournalList : TStringList;
begin
  if not assigned( FJournalList ) then
  begin
    FJournalList := TStringList.create;
    CreatedJournalList := true;
  end;
  result := FJournalList;
end;

function TSavFile.GetMapKnown : TMemoryStream;
var
  FileStream : TStream;
begin
  if not assigned( CurrentBlockPointer ) then
  begin
    result := nil;
    exit;
  end;

  if not assigned( FMapKnown ) then
  begin
    FMapKnown := TMemoryStream.create;
    CreatedMapKnown := true;
  end;
  result := FMapKnown;

  if NeedMapKnown then
  begin
    result.Clear;
    result.Size := CurrentBlockPointer.MapKnownSize;

    if assigned( MapStream ) then
    begin
      try
        MapStream.Position := CurrentBlockPointer.MapKnownPos;
        MapStream.Read( result.Memory^, CurrentBlockPointer.MapKnownSize );
      except
      end;
    end
    else
    begin
      FileStream := TFileStream.create( ChangeFileExt( FFilename, '.map' ), fmOpenRead or fmShareCompat );
      try
        FileStream.Position := CurrentBlockPointer.MapKnownPos;
        FileStream.Read( result.Memory^, CurrentBlockPointer.MapKnownSize );
      finally
        FileStream.free;
      end;
    end;
    NeedMapKnown := false;
  end;
end;

function TSavFile.GetPartyMembers : TMemoryStream;
begin
  if not assigned( FPartyMembers ) then
  begin
    FPartyMembers := TMemoryStream.create;
    CreatedPartyMembers := true;
  end;
  result := FPartyMembers;
end;

function TSavFile.GetProperties : TMemoryStream;
var
  FileStream : TFileStream;
begin
  if not assigned( CurrentBlockPointer ) then
  begin
    result := nil;
    exit;
  end;

  if not assigned( FProperties ) then
  begin
    FProperties := TMemoryStream.create;
    CreatedProperties := true;
  end;
  result := FProperties;

  if NeedProperties then
  begin
    result.Size := CurrentBlockPointer.DataSize;

    FileStream := TFileStream.create( Filename, fmOpenRead or fmShareCompat );
    try
      FileStream.Position := CurrentBlockPointer.DataPos;
      FileStream.Read( result.Memory^, CurrentBlockPointer.DataSize );
    finally
      FileStream.free;
    end;
    NeedProperties := false;
  end;
end;

function TSavFile.GetQuestList : TStringList;
begin
  if not assigned( FQuestList ) then
  begin
    FQuestList := TStringList.create;
    CreatedQuestList := true;
  end;
  result := FQuestList;
end;

function TSavFile.GetTravelList : TStringList;
begin
  if not assigned( FTravelList ) then
  begin
    FTravelList := TStringList.create;
    CreatedTravelList := true;
  end;
  result := FTravelList;
end;

procedure TSavFile.Load;
begin

end;

procedure TSavFile.Open( const Filename : string );
var
  Stream : TFileStream;
  IdxFile : string;
  BlockPos, BlockSize, L : longint;
  EOB, BB : word;
  Block : TSavBlocks;
  S : string;
  List : TStringList;
  BlockPointer : PBlockPointer;
  i : integer;
  PrevLevel : string;
  PlayerBlockOffset : longint;
  PlayerBlockSize : longint;
begin
  FFilename := Filename;

  if assigned( FPartyMembers ) then
    FPartyMembers.Clear
  else
  begin
    FPartyMembers := TMemoryStream.Create;
    CreatedPartyMembers := true;
  end;
  if assigned( FTravelList ) then
    FTravelList.Clear
  else
  begin
    FTravelList := TStringList.Create;
    CreatedTravelList := true;
  end;
  if assigned( FJournalList ) then
    FJournalList.Clear
  else
  begin
    FJournalList := TStringList.Create;
    CreatedJournalList := true;
  end;
  if assigned( FAdventureList ) then
    FAdventureList.Clear
  else
  begin
    FAdventureList := TStringList.Create;
    CreatedAdventureList := true;
  end;
  if assigned( FQuestList ) then
    FQuestList.Clear
  else
  begin
    FQuestList := TStringList.Create;
    CreatedQuestList := true;
  end;
  if assigned( FMapKnown ) then
  begin
    if CreatedMapKnown then
      FMapKnown.free;
    FMapKnown := nil;
    CreatedMapKnown := false;
  end;
  if assigned( FProperties ) then
  begin
    if CreatedProperties then
      FProperties.free;
    FProperties := nil;
    CreatedProperties := false;
  end;
  MaxPartyMembers := 2;

  for i := 0 to MapIndex.count - 1 do
  begin
    BlockPointer := pointer( MapIndex.objects[ i ] );
    Dispose( BlockPointer );
  end;
  MapIndex.Clear;

  if not FileExists( FFilename ) then
  begin
    NewFormat := true;
    exit;
  end;

  PlayerBlockOffset := 0;
  PlayerBlockSize := 0;
  MapStream := nil;
  BlockPointer := nil;
  PrevLevel := '';
  EOB := EOBMarker;
  List := TStringList.create;
  try

    IdxFile := ChangeFileExt( Filename, '.idx' );
    NewFormat := FileExists( IdxFile );
    if NewFormat then
    begin
      Stream := TFileStream.create( IdxFile, fmOpenRead or fmShareCompat );
      try
        while Stream.Position < Stream.Size do
        begin
          Stream.Read( Block, sizeof( Block ) );
          Stream.Read( BlockSize, sizeof( BlockSize ) );
          BlockPos := Stream.Position;
          case Block of
            sbIndex :
              begin
                Stream.Read( L, sizeof( L ) );
                if L > 0 then
                begin
                  SetLength( S, L );
                  Stream.read( S[ 1 ], L );
                  MapIndex.text := S;
                end;
                for i := 0 to MapIndex.count - 1 do
                begin
                  New( BlockPointer );
                  MapIndex.Objects[ i ] := pointer( BlockPointer );
                  Stream.read( BlockPointer^, sizeof( TBlockPointer ) );
                end;
              end;
            sbMap :
              begin
                if BlockSize = 0 then
                  S := ''
                else
                begin
                  SetLength( S, BlockSize );
                  Stream.Read( S[ 1 ], BlockSize );
                end;
                List.Text := S;
                MapName := List.Values[ 'Map' ];
                SceneName := List.Values[ 'Scene' ];
              end;
            sbTravel :
              begin
                if BlockSize > 0 then
                begin
                  SetLength( S, L );
                  Stream.Read( S[ 1 ], L );
                  TravelList.Text := S;
                end;
              end;
            sbJournal :
              begin
                if BlockSize > 0 then
                begin
                  SetLength( S, BlockSize );
                  Stream.Read( S[ 1 ], BlockSize );
                  FJournalList.Text := S;
                end;
              end;
            sbStartJournalIndex :
              begin
                Stream.Read( L, sizeof( L ) );
                JournalIndex := L;
              end;
            sbQuest :
              begin
                if BlockSize > 0 then
                begin
                  SetLength( S, BlockSize );
                  Stream.Read( S[ 1 ], BlockSize );
                  FQuestList.Text := S;
                end;
              end;
            sbAdventure :
              begin
                if BlockSize > 0 then
                begin
                  SetLength( S, BlockSize );
                  Stream.Read( S[ 1 ], BlockSize );
                  FAdventureList.Text := S;
                end;
              end;
            sbStartQuestIndex :
              begin
                Stream.Read( L, sizeof( L ) );
                QuestIndex := L;
              end;
            sbStartAdventureIndex :
              begin
                Stream.Read( L, sizeof( L ) );
                AdventureIndex := L;
              end;
            sbMaxPartyMembers :
              begin
                Stream.Read( L, sizeof( L ) );
                MaxPartyMembers := L;
              end;
            sbDeathScreen :
              begin
                DeathScreen := '';
                if BlockSize > 0 then
                begin
                  SetLength( S, BlockSize );
                  Stream.Read( S[ 1 ], BlockSize );
                  DeathScreen := S;
                end;
              end;
            sbCharacter, sbItem :
              begin
                PlayerBlockOffset := BlockPos - sizeof( BlockSize ) - sizeof( TSavBlocks );
                PlayerBlockSize := Stream.Size - ( BlockPos - sizeof( BlockSize ) - sizeof( TSavBlocks ) );
                break;
              end;
          end;
          Stream.Seek( BlockPos + BlockSize, soFromBeginning );
          Stream.Read( BB, sizeof( BB ) );
          if BB <> EOB then
          begin
            exit;
          end;
        end;
        Stream.Position := PlayerBlockOffset;
        FPartyMembers.position := 0;
        FPartyMembers.CopyFrom( Stream, PlayerBlockSize );
        FPartyMembers.position := 0;
      finally
        Stream.free;
      end;
    end
    else
    begin
      Stream := TFileStream.create( Filename, fmOpenRead or fmShareCompat );
      try
        while Stream.Position < Stream.Size do
        begin
          Stream.Read( Block, sizeof( Block ) );
          Stream.Read( BlockSize, sizeof( BlockSize ) );
          BlockPos := Stream.Position;
          case Block of
            sbMap :
              begin
                if BlockSize = 0 then
                  S := ''
                else
                begin
                  SetLength( S, BlockSize );
                  Stream.Read( S[ 1 ], BlockSize );
                  List.Text := S;
                end;
                MapName := List.Values[ 'Map' ];
                SceneName := List.Values[ 'Scene' ];
              end;
            sbMapKnown :
              begin
                //Extract and place in seperate file
                //This block will only be found in old files
                if not assigned( MapStream ) then
                  MapStream := TMemoryStream.create;
                Stream.read( L, sizeof( L ) );
                if L > 0 then
                begin
                  SetLength( S, L );
                  Stream.Read( S[ 1 ], L );
                  i := MapIndex.IndexOf( S );
                  if i >= 0 then
                  begin
                    BlockPointer := pointer( MapIndex.Objects[ i ] );
                  end
                  else
                  begin
                    New( BlockPointer );
                    i := MapIndex.add( S );
                    MapIndex.Objects[ i ] := pointer( BlockPointer );
                  end;
                  BlockPointer.MapKnownPos := MapStream.Position;
                  BlockPointer.MapKnownSize := BlockSize - sizeof( L ) - L;
                  MapStream.CopyFrom( Stream, BlockPointer.MapKnownSize );
                end;
              end;
            sbTravel :
              begin
                if BlockSize > 0 then
                begin
                  SetLength( S, L );
                  Stream.Read( S[ 1 ], L );
                  TravelList.Text := S;
                end;
              end;
            sbJournal :
              begin
                if BlockSize > 0 then
                begin
                  SetLength( S, BlockSize );
                  Stream.Read( S[ 1 ], BlockSize );
                  FJournalList.Text := S;
                end;
              end;
            sbStartJournalIndex :
              begin
                Stream.Read( L, sizeof( L ) );
                JournalIndex := L;
              end;
            sbQuest :
              begin
                if BlockSize > 0 then
                begin
                  SetLength( S, BlockSize );
                  Stream.Read( S[ 1 ], BlockSize );
                  FQuestList.Text := S;
                end;
              end;
            sbAdventure :
              begin
                if BlockSize > 0 then
                begin
                  SetLength( S, BlockSize );
                  Stream.Read( S[ 1 ], BlockSize );
                  FAdventureList.Text := S;
                end;
              end;
            sbStartQuestIndex :
              begin
                Stream.Read( L, sizeof( L ) );
                QuestIndex := L;
              end;
            sbStartAdventureIndex :
              begin
                Stream.Read( L, sizeof( L ) );
                AdventureIndex := L;
              end;
            sbMaxPartyMembers :
              begin
                Stream.Read( L, sizeof( L ) );
                MaxPartyMembers := L;
              end;
            sbDeathScreen :
              begin
                DeathScreen := '';
                if BlockSize > 0 then
                begin
                  SetLength( S, BlockSize );
                  Stream.Read( S[ 1 ], BlockSize );
                  DeathScreen := S;
                end;
              end;
            sbCharacter, sbItem :
              begin
                if PlayerBlockOffset = 0 then
                  PlayerBlockOffset := BlockPos - sizeof( BlockSize ) - sizeof( TSavBlocks );
                PlayerBlockSize := PlayerBlockSize + sizeof( EOB ) + sizeof( BlockSize ) + sizeof( TSavBlocks ) + BlockSize;
              end;
            scAbstract, scSoundPlayer, scPathCorner, scTrigger, scSpriteObject,
              scItem, scWeapon, scQuiver, scBow, scContainer, scDoor, scCharacter,
              scEventTimer :
              begin
                Stream.Read( L, sizeof( L ) );
                if L = 0 then
                  S := ''
                else
                begin
                  SetLength( S, L );
                  Stream.Read( S[ 1 ], L );
                end;
                if S = PrevLevel then
                begin
                  inc( BlockPointer.DataSize, BlockSize + sizeof( EOB ) + sizeof( BlockSize ) + sizeof( TSavBlocks ) );
                end
                else
                begin
                  i := MapIndex.IndexOf( S );
                  if i >= 0 then
                  begin
                    BlockPointer := pointer( MapIndex.Objects[ i ] );
                  end
                  else
                  begin
                    New( BlockPointer );
                    i := MapIndex.add( S );
                    MapIndex.Objects[ i ] := pointer( BlockPointer );
                  end;
                  BlockPointer.DataPos := BlockPos - sizeof( BlockSize ) - sizeof( TSavBlocks );
                  BlockPointer.DataSize := BlockSize + sizeof( EOB ) + sizeof( BlockSize ) + sizeof( TSavBlocks );
                  PrevLevel := S;
                end;
              end;
          end;
          Stream.Seek( BlockPos + BlockSize, soFromBeginning );
          Stream.Read( BB, sizeof( BB ) );
          if BB <> EOB then
          begin
            exit;
          end;
        end;
        Stream.Position := PlayerBlockOffset;
        FPartyMembers.position := 0;
        FPartyMembers.CopyFrom( Stream, PlayerBlockSize );
        FPartyMembers.position := 0;
      finally
        Stream.free;
      end;
    end;
  finally
    List.free;
  end;

  i := MapIndex.IndexOf( FCurrentMap );
  if i >= 0 then
  begin
    CurrentBlockPointer := pointer( MapIndex.Objects[ i ] );
  end
  else
  begin
    CurrentBlockPointer := nil;
  end;
end;

procedure TSavFile.Save;
var
  Stream : TFileStream;
  Mem : TmemoryStream;
  S : string;
  MapFilename : string;
  List : TStringList;
  EOB : word;
  Block : TSavBlocks;
  L : longint;
  i : integer;
  BlockPointer : PBlockPointer;
  NewIndex : boolean;
begin
  EOB := EOBMarker;
  List := TStringList.create;
  try
    if assigned( CurrentBlockPointer ) then
      NewIndex := false
    else
    begin
      NewIndex := true;
      new( CurrentBlockPointer );
      i := MapIndex.add( FCurrentMap );
      MapIndex.Objects[ i ] := TObject( CurrentBlockPointer );
      CurrentBlockPointer.DataPos := 0;
      CurrentBlockPointer.DataSize := 0;
      CurrentBlockPointer.MapKnownPos := 0;
      CurrentBlockPointer.MapKnownSize := 0;
    end;

    if not NewFormat or NewFile or not NeedProperties then
    begin
      if NewFormat and NewFile and NeedProperties then
      begin
        //if we're saveing as a new file, but haven't changed anything
        try
          if FileExists( FFilename ) then
            CopyFile( PChar( FFilename ), PChar( NewFilename ), false )
          else
            DeleteFile( NewFilename );
        except
        end;
      end
      else if NewFormat and not NewFile and not NeedProperties then
      begin
        if FileExists( FFilename ) then
        begin
          //Save in place - dont touch unchanged data before current block
          //Move current block to end
          Mem := TMemoryStream.create;
          try
            Stream := TFileStream.create( FFilename, fmOpenReadWrite or fmShareCompat );
            try
              if NewIndex then
              begin
                CurrentBlockPointer.DataPos := Stream.Size;
                CurrentBlockPointer.DataSize := FProperties.Size;
                if CurrentBlockPointer.DataSize > 0 then
                begin
                  Stream.Position := CurrentBlockPointer.DataPos;
                  FProperties.position := 0;
                  Stream.CopyFrom( FProperties, CurrentBlockPointer.DataSize );
                end;
              end
              else
              begin
                for i := 0 to MapIndex.count - 1 do
                begin
                  BlockPointer := pointer( MapIndex.objects[ i ] );

                  if BlockPointer <> CurrentBlockPointer then
                  begin
                    if ( BlockPointer.DataSize > 0 ) and ( BlockPointer.DataPos > CurrentBlockPointer.DataPos ) then
                    begin
                      Stream.Position := BlockPointer.DataPos;
                      BlockPointer.DataPos := CurrentBlockPointer.DataPos + Mem.Position;
                      Mem.CopyFrom( Stream, BlockPointer.DataSize );
                    end;
                  end;
                end;
                Stream.Position := CurrentBlockPointer.DataPos;
                Mem.Position := 0;
                Stream.CopyFrom( Mem, Mem.Size );
                CurrentBlockPointer.DataPos := Stream.Position;
                CurrentBlockPointer.DataSize := FProperties.Size;
                if CurrentBlockPointer.DataSize > 0 then
                begin
                  FProperties.position := 0;
                  Stream.CopyFrom( FProperties, CurrentBlockPointer.DataSize );
                end;
              end;
            finally
              Stream.free;
            end;
          finally
            Mem.free;
          end;
        end
        else
        begin
          Stream := TFileStream.create( FFilename, fmCreate or fmShareCompat );
          try
            if NewIndex then
            begin
              CurrentBlockPointer.DataPos := 0;
              CurrentBlockPointer.DataSize := FProperties.Size;
              if CurrentBlockPointer.DataSize > 0 then
              begin
                Stream.Position := CurrentBlockPointer.DataPos;
                FProperties.position := 0;
                Stream.CopyFrom( FProperties, CurrentBlockPointer.DataSize );
              end;
            end
          finally
            Stream.free;
          end;
        end;
      end
      else
      begin
        if FileExists( FFilename ) then
        begin
          Mem := TMemoryStream.create;
          try
            Stream := TFileStream.create( FFilename, fmOpenRead or fmShareCompat );
            try
              for i := 0 to MapIndex.count - 1 do
              begin
                BlockPointer := pointer( MapIndex.objects[ i ] );

                if BlockPointer <> CurrentBlockPointer then
                begin
                  if BlockPointer.DataSize > 0 then
                  begin
                    Stream.Position := BlockPointer.DataPos;
                    BlockPointer.DataPos := Mem.Position;
                    Mem.CopyFrom( Stream, BlockPointer.DataSize );
                  end;
                end;
              end;
              //Save Current Block at end
              if NeedProperties then
              begin
                if CurrentBlockPointer.DataSize > 0 then
                begin
                  Stream.Position := CurrentBlockPointer.DataPos;
                  CurrentBlockPointer.DataPos := Mem.Position;
                  Mem.CopyFrom( Stream, CurrentBlockPointer.DataSize );
                end;
              end
              else
              begin
                CurrentBlockPointer.DataSize := FProperties.Size;
                if CurrentBlockPointer.DataSize > 0 then
                begin
                  FProperties.Position := 0;
                  CurrentBlockPointer.DataPos := Mem.Position;
                  Mem.CopyFrom( FProperties, CurrentBlockPointer.DataSize );
                end;
              end;
            finally
              Stream.free;
            end;

            if NewFile then
              Mem.SaveToFile( NewFilename )
            else
              Mem.SaveToFile( FFilename );
          finally
            Mem.free;
          end;
        end
        else
        begin
          if NewFile then
            Stream := TFileStream.create( NewFilename, fmCreate or fmShareCompat )
          else
            Stream := TFileStream.create( FFilename, fmCreate or fmShareCompat );
          try
            if NewIndex then
            begin
              CurrentBlockPointer.DataPos := 0;
              CurrentBlockPointer.DataSize := FProperties.Size;
              if CurrentBlockPointer.DataSize > 0 then
              begin
                Stream.Position := CurrentBlockPointer.DataPos;
                FProperties.position := 0;
                Stream.CopyFrom( FProperties, CurrentBlockPointer.DataSize );
              end;
            end
          finally
            Stream.free;
          end;
        end;
      end;
    end;

    //Save map known info if necessary
    if NewFile then
    begin
      MapFilename := ChangeFileExt( NewFilename, '.map' );
      if not assigned( MapStream ) then
      begin
        try
          S := ChangeFileExt( FFilename, '.map' );
          if FileExists( S ) then
            CopyFile( PChar( S ), PChar( MapFilename ), false );
        except
        end;
      end;
    end
    else
      MapFilename := ChangeFileExt( FFilename, '.map' );
    if not NeedMapKnown then
    begin
      if assigned( MapStream ) then
      begin
        if NewIndex then
        begin
          CurrentBlockPointer.MapKnownPos := MapStream.Size;
          CurrentBlockPointer.MapKnownSize := FMapKnown.Size;
        end;
        MapStream.Position := CurrentBlockPointer.MapKnownPos;
        FMapKnown.Position := 0;
        MapStream.CopyFrom( FMapKnown, CurrentBlockPointer.MapKnownSize );
      end
      else
      begin
        if FileExists( MapFilename ) then
          Stream := TFileStream.create( MapFilename, fmOpenReadWrite or fmShareCompat )
        else
          Stream := TFileStream.create( MapFilename, fmCreate or fmShareCompat );
        try
          if NewIndex then
          begin
            CurrentBlockPointer.MapKnownPos := Stream.Size;
            CurrentBlockPointer.MapKnownSize := FMapKnown.Size;
          end;
          Stream.Position := CurrentBlockPointer.MapKnownPos;
          FMapKnown.Position := 0;
          Stream.CopyFrom( FMapKnown, CurrentBlockPointer.MapKnownSize );
        finally
          Stream.free;
        end;
      end;
    end;
    if assigned( MapStream ) then
      MapStream.SaveToFile( MapFilename );

    if NewFile then
      Stream := TFileStream.create( ChangeFileExt( NewFilename, '.idx' ), fmCreate or fmShareCompat )
    else
      Stream := TFileStream.create( ChangeFileExt( FFilename, '.idx' ), fmCreate or fmShareCompat );
    try

      Block := sbIndex;
      Stream.Write( Block, sizeof( Block ) );
      S := MapIndex.Text;
      L := Length( S ) + MapIndex.count * sizeof( TBlockPointer ) + sizeof( L );
      Stream.write( L, sizeof( L ) );
      L := Length( S );
      Stream.write( L, sizeof( L ) );
      if L > 0 then
        Stream.write( S[ 1 ], L );
      for i := 0 to MapIndex.count - 1 do
      begin
        Stream.write( PBlockPointer( MapIndex.objects[ i ] )^, sizeof( TBlockPointer ) );
      end;
      Stream.write( EOB, sizeof( EOB ) );

      Block := sbMap;
      Stream.Write( Block, sizeof( Block ) );
      List.Clear;
      S := 'Map=' + MapName;
      List.add( S );
      S := 'Scene=' + SceneName;
      List.add( S );
      S := List.Text;
      L := Length( S );
      Stream.write( L, sizeof( L ) );
      if L > 0 then
        Stream.write( S[ 1 ], L );
      Stream.write( EOB, sizeof( EOB ) );

      Block := sbTravel;
      Stream.Write( Block, sizeof( Block ) );
      i := TravelList.IndexOf( FCurrentMap );
      if i < 0 then
        TravelList.add( FCurrentMap );
      S := FCurrentMap + '|' + lowercase( CurrentScene );
      i := TravelList.IndexOf( S );
      if i < 0 then
        TravelList.add( S );
      S := TravelList.Text;
      L := Length( S );
      Stream.write( L, sizeof( L ) );
      if L > 0 then
        Stream.write( S[ 1 ], L );
      Stream.write( EOB, sizeof( EOB ) );

      Block := sbJournal;
      Stream.Write( Block, sizeof( Block ) );
      S := JournalList.Text;
      L := Length( S );
      Stream.write( L, sizeof( L ) );
      if L > 0 then
        Stream.write( S[ 1 ], L );
      Stream.write( EOB, sizeof( EOB ) );

      Block := sbStartJournalIndex;
      Stream.Write( Block, sizeof( Block ) );
      L := sizeof( JournalIndex );
      Stream.write( L, sizeof( L ) );
      Stream.write( JournalIndex, sizeof( JournalIndex ) );
      Stream.write( EOB, sizeof( EOB ) );

      Block := sbQuest;
      Stream.Write( Block, sizeof( Block ) );
      S := QuestList.Text;
      L := Length( S );
      Stream.write( L, sizeof( L ) );
      if L > 0 then
        Stream.write( S[ 1 ], L );
      Stream.write( EOB, sizeof( EOB ) );

      Block := sbAdventure;
      Stream.Write( Block, sizeof( Block ) );
      S := AdventureList.Text;
      L := Length( S );
      Stream.write( L, sizeof( L ) );
      if L > 0 then
        Stream.write( S[ 1 ], L );
      Stream.write( EOB, sizeof( EOB ) );

      Block := sbStartQuestIndex;
      Stream.Write( Block, sizeof( Block ) );
      L := sizeof( QuestIndex );
      Stream.write( L, sizeof( L ) );
      Stream.write( QuestIndex, sizeof( QuestIndex ) );
      Stream.write( EOB, sizeof( EOB ) );

      Block := sbStartAdventureIndex;
      Stream.Write( Block, sizeof( Block ) );
      L := sizeof( AdventureIndex );
      Stream.write( L, sizeof( L ) );
      Stream.write( AdventureIndex, sizeof( AdventureIndex ) );
      Stream.write( EOB, sizeof( EOB ) );

      Block := sbMaxPartyMembers;
      Stream.Write( Block, sizeof( Block ) );
      L := sizeof( MaxPartyMembers );
      Stream.write( L, sizeof( L ) );
      Stream.write( MaxPartyMembers, sizeof( MaxPartyMembers ) );
      Stream.write( EOB, sizeof( EOB ) );

      Block := sbDeathScreen;
      Stream.Write( Block, sizeof( Block ) );
      S := DeathScreen;
      L := Length( S );
      Stream.write( L, sizeof( L ) );
      if L > 0 then
        Stream.write( S[ 1 ], L );
      Stream.write( EOB, sizeof( EOB ) );

      FPartyMembers.position := 0;
      Stream.CopyFrom( FPartyMembers, FPartyMembers.Size );
    finally
      Stream.free;
    end;
  finally
    List.free;
  end;

  NewFormat := true;
end;

procedure TSavFile.SaveAs( const Filename : string );
begin
  if lowercase( Filename ) = lowercase( FFileName ) then
    Save
  else
  begin
    NewFilename := Filename;
    NewFile := true;
    Save;
    FFilename := NewFilename;
    NewFilename := '';
    NewFile := false;
  end;
end;

procedure TSavFile.SetAdventure( const Value : TStringList );
begin
  if CreatedAdventureList then
  begin
    FAdventureList.free;
    FAdventureList := nil;
    CreatedAdventureList := false;
  end;
  FAdventureList := Value;
end;

procedure TSavFile.SetJournalList( const Value : TStringList );
begin
  if CreatedJournalList then
  begin
    FJournalList.free;
    FJournalList := nil;
    CreatedJournalList := false;
  end;
  FJournalList := Value;
end;

procedure TSavFile.SetMapKnown( const Value : TMemoryStream );
begin
  if CreatedMapKnown then
  begin
    FMapKnown.free;
    FMapKnown := nil;
    CreatedMapKnown := false;
  end;
  FMapKnown := Value;
  NeedMapKnown := false;
end;

procedure TSavFile.SetCurrentMap( const Value : string );
var
  i : integer;
begin
  FCurrentMap := Value;
  i := MapIndex.IndexOf( FCurrentMap );
  if assigned( FMapKnown ) then
  begin
    if CreatedMapKnown then
      FMapKnown.free;
    FMapKnown := nil;
    CreatedMapKnown := false;
  end;
  if assigned( FProperties ) then
  begin
    if CreatedProperties then
      FProperties.free;
    FProperties := nil;
    CreatedProperties := false;
  end;
  NeedProperties := true;
  NeedMapKnown := true;
  if i >= 0 then
  begin
    CurrentBlockPointer := pointer( MapIndex.Objects[ i ] );
  end
  else
  begin
    CurrentBlockPointer := nil;
  end;
end;

procedure TSavFile.SetPartyMembers( const Value : TMemoryStream );
begin
  if CreatedPartyMembers then
  begin
    FPartyMembers.free;
    FPartyMembers := nil;
    CreatedPartyMembers := false;
  end;
  FPartyMembers := Value;
end;

procedure TSavFile.SetProperties( const Value : TMemoryStream );
begin
  if CreatedProperties then
  begin
    FProperties.free;
    FProperties := nil;
    CreatedProperties := false;
  end;
  FProperties := Value;
  NeedProperties := false;
end;

procedure TSavFile.SetQuestList( const Value : TStringList );
begin
  if CreatedQuestList then
  begin
    FQuestList.free;
    FQuestList := nil;
    CreatedQuestList := false;
  end;
  FQuestList := Value;
end;

procedure TSavFile.SetTravelList( const Value : TStringList );
begin
  if CreatedTravelList then
  begin
    FTravelList.free;
    FTravelList := nil;
    CreatedTravelList := false;
  end;
  FTravelList := Value;
end;

end.
