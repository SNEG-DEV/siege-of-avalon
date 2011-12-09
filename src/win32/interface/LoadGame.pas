unit LoadGame;
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
  GameText,
  Display,
  Anigrp30,
  Engine,
  Logfile;
type

  pItem = ^SelectableRect;
  SelectableRect = record
    rect : TRect;
    time : integer;
    text : string;
    date : string;
  end;

  TLoadGame = class( TDisplay )
  private
    //Info Box stuff
    CharacterName : array[ 1..5 ] of string;
    CharacterCount : integer;
    CharacterGif : string; //we delete this along with a file
    //Line editor stuff
    SavedFileName : string; //characters name
    CaratPosition : integer; //position in pixels
    CaratCharPosition : integer; //position in Characters
    CaratVisible : boolean;
    LoopCounter : integer;
    CaratTimer : TTimer;
    //Bitmap stuff
    BMBack : TBitmap; //The inventory screen bitmap used for loading
    DXBack : IDirectDrawSurface;
    DXBackHighlight : IDirectDrawSurface; //so we know which file is selected for loading
    DXLoad : IDirectDrawSurface;
    DXCancel : IDirectDrawSurface;
    DXok : IDirectDrawSurface;
    SelectRect : TList; //collision rects for selectable text
    pTextItem : pItem;
    XAdj, YAdj : integer; //adjust ments for placement of the sheet
    StartFile : integer; //the first file to display
    CurrentSelectedListItem : integer;
    DeleteBoxVisible : boolean;
    OverwriteBoxVisible : boolean;
    MustEnterNameBoxVisible : boolean;
    txtMessage : array[ 0..2 ] of string;
    ScrollState : integer;
    procedure AttemptToLoadGame;
    procedure AttemptToSaveGame;
    procedure CaratTimerEvent( Sender : TObject );
    procedure LoadText;
    procedure PlotMenu;
    procedure MoveList;
    procedure DeleteSavedFile;
    procedure OverwriteSavedFile;
    procedure MustEnterName;
    procedure ShowScreen;
    procedure ShowInfo;
    function LoadGame( GameName : string ) : boolean;
    //procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove( Sender : TObject; Shift : TShiftState; X, Y : Integer );
  protected
    procedure MouseDown( Sender : TAniview; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
    procedure MouseMove( Sender : TAniview;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
    procedure MouseUp( Sender : TAniview; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); override;
    procedure KeyDown( Sender : TObject; var key : Word; Shift : TShiftState ); override;
  public
    frmMain : TForm; //we need the  form passed into handle form mouse events
    LoadThisFile : string; //This is the file to Save To or to Load From right here!
    LastSavedFile : string; //The caller loads this string so we know what the last file saved to\Loaded from is.
    LoadFile : boolean;
    SceneName : string;
    MapName : string;
    TravelBlock : string;
    ldLoadLightRect : TRect;
    ldSaveLightRect : TRect;
    DXLoadRect : TRect;
    ldCancelRect : TRect;
    ldLoadDarkRect : TRect;
    ldSaveDarkRect : TRect;
    ldLoadUpperRect : TRect;
    ldSaveUpperRect : TRect;
    constructor Create;
    destructor Destroy; override;
    procedure Paint; override;
    procedure Init; override;
    procedure Release; override;
  end;
implementation
uses
  AniDemo,
  Resource,
  SaveFile;
{ TLoadGame }

constructor TLoadGame.Create;
const
  FailName : string = 'TLoadGame.create ';
begin

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
    ldCancelRect := Rect( 101, 450, 101 + 300, 450 + 45 );
    ldLoadDarkRect := Rect( 401, 450, 401 + 300, 450 + 45 );
    ldSaveDarkRect := Rect( 401, 450, 401 + 300, 450 + 45 );
    ldLoadUpperRect := Rect( 94, 19, 94 + 263, 19 + 45 );
    ldSaveUpperRect := Rect( 94, 19, 94 + 263, 19 + 45 );
    ldLoadLightRect := ldLoadDarkRect;
    ldSaveLightRect := ldSaveDarkRect;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end; //Create

destructor TLoadGame.Destroy;
const
  FailName : string = 'TLoadGame.Destory ';
begin

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end; //Destroy

procedure TLoadGame.Init;
var
  InvisColor : integer;
  DXTemp : IDirectDrawSurface;
  BM : TBitmap;
  i : integer;

const
  FailName : string = 'TLoadGame.init ';
begin

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if Loaded then
      Exit;
    inherited;

    ExText.Open( 'LoadGame' );
    for i := 0 to 2 do
      txtMessage[ i ] := ExText.GetText( 'Message' + inttostr( i ) );

    ScrollState := 0;
    CaratTimer := TTimer.create( nil );
    CaratTimer.onTimer := CaratTimerEvent;
//      CaratTimer.TimerPriority:=tpNormal;
    CaratTimer.Interval := 100;
//      CaratTimer.resolution := 1;
    LoopCounter := 0;
    CaratTimer.enabled := True;
    CaratPosition := 0;
    CaratCharPosition := 0;
    CaratVisible := true;
    SavedFileName := '';

    //Set mouse events for form
//  frmMain.OnMouseDown := FormMouseDown;
    frmMain.OnMouseMove := FormMouseMove;

    SelectRect := TList.create;

    StartFile := 0;


    DeleteBoxVisible := false;
    OverwriteBoxVisible := false;
    MustEnterNameBoxVisible := false;
    XAdj := 0;
    YAdj := 0;

    LoadText;

    pText.LoadFontGraphic( 'createchar' ); //load the statisctics font graphic in
    pText.LoadGoldFontGraphic;

    BMBack := TBitmap.Create;
    BM := TBitmap.Create;

  //transparent color
    InvisColor := $00FFFF00;

    if LoadFile then
    begin
      BMBack.LoadFromFile( InterfacePath + 'ldLoadLight.bmp' );
      DXLoadRect := ldLoadLightRect;
    end
    else
    begin
      BMBack.LoadFromFile( InterfacePath + 'ldSaveLight.bmp' );
      DXLoadRect := ldSaveLightRect;
    end;
    DXLoadRect.Right := BMBack.Width;
    DXLoadRect.Bottom := BMBack.Height;
    DXLoad := DDGetImage( lpDD, BMBack, InvisColor, False );

    BMBack.LoadFromFile( InterfacePath + 'ldCancel.bmp' );
    ldCancelRect.Right := BMBack.Width;
    ldCancelRect.Bottom := BMBack.Height;
    DXCancel := DDGetImage( lpDD, BMBack, InvisColor, False );

    BMBack.LoadFromFile( InterfacePath + 'ldOk.bmp' );
    DXok := DDGetImage( lpDD, BMBack, InvisColor, False );

    BMBack.LoadFromFile( InterfacePath + 'opYellow.bmp' );
    DXBackHighlight := DDGetImage( lpDD, BMBack, InvisColor, False );

    BMBack.LoadFromFile( InterfacePath + 'ldLoadSave.bmp' );
    DXBack := DDGetImage( lpDD, BMBack, InvisColor, False );

    if LoadFile then
    begin
      BM.LoadFromFile( InterfacePath + 'ldLoadDark.bmp' );
      DXTemp := DDGetImage( lpDD, BM, InvisColor, False );
      DXBack.BltFast( ldLoadDarkRect.Left, ldLoadDarkRect.Top, DXTemp, rect( 0, 0, BM.width, BM.height ), DDBLTFAST_WAIT );
    end
    else
    begin
      BM.LoadFromFile( InterfacePath + 'ldSaveDark.bmp' );
      DXTemp := DDGetImage( lpDD, BM, InvisColor, False );
      DXBack.BltFast( ldSaveDarkRect.Left, ldSaveDarkRect.Top, DXTemp, rect( 0, 0, BM.width, BM.height ), DDBLTFAST_WAIT );
    end;

    DXTemp := nil;

    if LoadFile then
    begin
      BM.LoadFromFile( InterfacePath + 'ldLoadUpper.bmp' );
      DXTemp := DDGetImage( lpDD, BM, InvisColor, False );
      DXBack.BltFast( ldLoadUpperRect.Left, ldLoadUpperRect.Top, DXTemp, rect( 0, 0, BM.width, BM.height ), DDBLTFAST_WAIT );
    end
    else
    begin
      BM.LoadFromFile( InterfacePath + 'ldSaveUpper.bmp' );
      DXTemp := DDGetImage( lpDD, BM, InvisColor, False );
      DXBack.BltFast( ldSaveUpperRect.Left, ldSaveUpperRect.Top, DXTemp, rect( 0, 0, BM.width, BM.height ), DDBLTFAST_WAIT );
    end;


    lpDDSBack.BltFast( 0, 0, DXBack, Rect( 0, 0, BMBack.width, BMBack.Height ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

    DXTemp := nil;
    BMBack.Free;
    BM.free;
    PlotMenu;

    if LoadFile then
      CurrentSelectedListItem := -1
    else
    begin
      CurrentSelectedListItem := 0;
      SavedFileName := pItem( SelectRect.items[ 0 ] ).Text;
      CaratPosition := pText.TextLength( SavedFileName );
      CaratCharPosition := Length( SavedFileName );
      ShowScreen;
      if LoadGame( pItem( SelectRect.items[ CurrentSelectedListItem ] ).Text ) then
      begin //get the char name and map name from the saved file
        ShowInfo;
      end;
    end;


    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    lpDDSBack.BltFast( 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end; //Init

procedure TLoadGame.LoadText;
var
  FileData : TSearchRec;
  FileNotFound : integer;
  TheFileName : array[ 0..30 ] of Char;
  i, j : integer;
  fDay, fYear, fMonth : word;
  fHour, fMin, fSec, fMsec : word;
  pTempItem : pointer;
const
  FailName : string = 'TLoadGame.LoadText ';
begin

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
   //Search for the first file meeting our criteria - create a Find file structure, and assign it a handle
    FileNotFound := FindFirst( ExtractFilePath( Application.ExeName ) + 'Games\*.sav', faAnyFile, FileData );
    i := 0;
    while FileNotFound = 0 do
    begin

       //get the filename without .sav on it
      StrLCopy( TheFileName, PChar( FileData.Name ), Length( FileData.Name ) - 4 );
      if ExtractFilename( TheFileName )[ 1 ] <> '~' then
      begin
        new( pTextItem );
        pTextItem.text := TheFileName;
          //Get the last time this file was accessed
        pTextitem.time := FileAge( ExtractFilePath( Application.ExeName ) + 'Games\' + FileData.Name );
        DecodeDate( FileDateToDateTime( pTextitem.time ), fYear, fMonth, fDay );
        DecodeTime( FileDateToDateTime( pTextitem.time ), fHour, fMin, fSec, fMsec );
        if fMin > 10 then
          pTextItem.Date := intToStr( fMonth ) + '/' + intToStr( fDay ) + ' ' + intToStr( fHour ) + ':' + intToStr( fMin ) //'/'+intToStr(fYear);
        else
          pTextItem.Date := intToStr( fMonth ) + '/' + intToStr( fDay ) + ' ' + intToStr( fHour ) + ':0' + intToStr( fMin ); //'/'+intToStr(fYear);
        pTextItem.rect := rect( 379, 66 + i * 35, 669, 66 + i * 35 + 35 );
        SelectRect.add( pTextItem );
        i := i + 1;
      end;
       //get the next file using the handle to the search structure - nonzero on failure to find file
      FileNotFound := FindNext( FileData );

    end; //wend

   //Close the search handle, free the memory
    FindClose( FileData );

   //Sort by the time
    for i := 0 to SelectRect.count - 2 do
    begin
      for j := ( i + 1 ) to SelectRect.count - 1 do
      begin
        if ( ( pItem( SelectRect.items[ j ] ).time > pItem( SelectRect.items[ i ] ).time ) and ( ( lowercase( trim( pItem( SelectRect.items[ i ] ).text ) ) <> lowercase( trim( LastSavedFile ) ) ) or ( trim( LastSavedFile ) = '' ) ) ) or ( ( lowercase( trim( LastSavedFile ) ) = lowercase( trim( pItem( SelectRect.items[ j ] ).text ) ) ) and ( trim( LastSavedFile ) <> '' ) ) then
        begin
          pTempItem := SelectRect.items[ j ];
          SelectRect.items[ j ] := SelectRect.items[ i ];
          SelectRect.items[ i ] := pTempItem;
        end; //endif
      end; //end for j
    end; //end for i
   //if this is a save game, leave a blank slot as well
    if LoadFile = false then
    begin
      new( pTextItem );
      pTextItem.text := '';
      pTextItem.Date := '';
      SelectRect.add( pTextItem );
      if trim( LastSavedFile ) = '' then
      begin //there is no last saved game, os slap a blank box at top
        SelectRect.Move( SelectRect.count - 1, 0 );
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end; //LoadText

procedure TLoadGame.PlotMenu;
var
  i, j : integer;
const
  FailName : string = 'TLoadGame.PlotMenu ';
begin

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try


    j := 0;
    for i := 0 to SelectRect.count - 1 do
    begin
      if ( i >= StartFile ) and ( i < StartFile + 9 ) then
      begin //only show 9 files
        pItem( SelectRect.items[ i ] ).rect := rect( 379, 66 + j * 35, 669, 66 + j * 35 + 35 );
        ptext.PlotText( pItem( SelectRect.items[ i ] ).text, pItem( SelectRect.items[ i ] ).rect.left, pItem( SelectRect.items[ i ] ).rect.top, 240 );
        ptext.PlotText( pItem( SelectRect.items[ i ] ).date, 590, pItem( SelectRect.items[ i ] ).rect.top, 240 );
        j := j + 1;
      end
      else
      begin //not on screen, set coll rect offscreen
        pItem( SelectRect.items[ i ] ).rect := rect( -100, -100, -110, -110 );
      end;
    end; //end for
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end; //PlotMenu


procedure TLoadGame.MoveList;
var
  i, j : integer;
const
  FailName : string = 'TLoadGame.MoveList ';
begin

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try


    j := 0;
    for i := 0 to SelectRect.count - 1 do
    begin
      if ( i >= StartFile ) and ( i < StartFile + 9 ) then
      begin //only show 10 files
        pItem( SelectRect.items[ i ] ).rect := rect( 379, 66 + j * 35, 669, 66 + j * 35 + 35 );
        j := j + 1;
      end
      else
      begin //not on screen, set coll rect offscreen
        pItem( SelectRect.items[ i ] ).rect := rect( -100, -100, -110, -110 );
        if CurrentSelectedListItem = i then
        begin //if current highlight scrolled offscreen, drop highlight
          CurrentSelectedListItem := -1;
//           lpDDSBack.BltFast(114,257,DXBack,rect(114,257,344,421),DDBLTFAST_WAIT);
//           lpDDSBack.BltFast(111,65,DXBack,rect(111,65,344,231),DDBLTFAST_WAIT);
        end;
      end;
    end; //end for
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end; //MoveList


procedure TLoadGame.KeyDown( Sender : TObject; var key : Word; Shift : TShiftState );
var
  i, X1, Y1 : integer;
  nRect : TRect;
  a : string;
const
  FailName : string = 'TLoadGame.Keydown ';
begin

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if ( CurrentSelectedListItem > -1 ) and ( LoadFile = false ) and ( DeleteBoxVisible = false ) and ( OverwriteBoxVisible = false ) and ( MustEnterNameBoxVisible = false ) then
    begin
      X1 := pItem( SelectRect.items[ CurrentSelectedListItem ] ).rect.left;
      Y1 := pItem( SelectRect.items[ CurrentSelectedListItem ] ).rect.top;
      nRect := pItem( SelectRect.Items[ CurrentSelectedListItem ] ).Rect;
         //lpDDSBack.BltFast(X1,Y1,DXBack,rect(X1,Y1,586,Y1+24),DDBLTFAST_WAIT);
      lpDDSBack.BltFast( nRect.left - 10, nRect.top - 5, DXBack, rect( nRect.left - 10, nRect.top - 5, nRect.right, nRect.bottom - 5 ), DDBLTFAST_WAIT );
      DrawAlpha( lpDDSBack, rect( nRect.left - 10, nRect.top - 5, nRect.right, nRect.bottom - 5 ), rect( 0, 0, 12, 12 ), DXBackHighlight, False, 40 );
      pText.PlotText( pItem( SelectRect.items[ CurrentSelectedListItem ] ).date, 590, pItem( SelectRect.items[ CurrentSelectedListItem ] ).rect.top, 240 );
      if ( ( Key > 64 ) and ( Key < 91 ) ) or ( ( Key > 47 ) and ( Key < 58 ) ) or ( Key = 32 ) or ( key = 189 ) then
      begin
        if ( ( Key > 64 ) and ( Key < 91 ) ) and ( Shift <> [ ssShift ] ) then //make the char lowercase
          Key := Key + 32;
            //if (Length(SavedFileName) < 13) and (pText.TextLength(SavedFileName)< 130) then begin
        if pText.TextLength( SavedFileName ) < 200 then
        begin
          if CaratCharPosition = Length( SavedFileName ) then
          begin //adding a char to end of string
            SavedFileName := SavedFileName + char( Key );
            CaratPosition := pText.TextLength( SavedFileName );
            CaratCharPosition := CaratCharPosition + 1;
          end
          else
          begin //inserting a char
            CaratCharPosition := CaratCharPosition + 1;
            SavedFileName := SavedFileName + 'z'; //increase the size of the string by a char
            for i := Length( SavedFileName ) downto CaratCharPosition do
            begin
              SavedFileName[ i ] := SavedFileName[ i - 1 ];
            end; //end for
            SavedFileName[ CaratCharPosition ] := char( Key );
            a := SavedFileName;
            SetLength( a, CaratCharPosition );
            CaratPosition := pText.TextLength( a );
          end;
        end; //endif if length< 21
      end
      else if ( Key = 8 ) then
      begin //backspace
        if SavedFileName <> '' then
        begin
          if CaratCharPosition = Length( SavedFileName ) then
          begin //if at the end of the name
            CaratCharPosition := CaratCharPosition - 1;
            if CaratCharPosition = 0 then
              SavedFileName := ''
            else
              SetLength( SavedFileName, CaratCharPosition );
            CaratPosition := pText.TextLength( SavedFileName );
          end
          else if CaratCharPosition > 0 then
          begin //in middle of name somewhere
            CaratCharPosition := CaratCharPosition - 1;
            for i := CaratCharPosition + 2 to Length( SavedFileName ) do
            begin //chop out the middle char
              SavedFileName[ i - 1 ] := SavedFileName[ i ];
            end; //end for
            SetLength( SavedFileName, Length( SavedFileName ) - 1 );
            a := SavedFileName;
            SetLength( a, CaratCharPosition );
            CaratPosition := pText.TextLength( a );
          end;

        end; //endif length
      end
      else if ( Key = 46 ) then
      begin //Delete
        if ( SavedFileName <> '' ) and ( CaratCharPosition <> Length( SavedFileName ) ) then
        begin
          if ( CaratCharPosition = 0 ) and ( Length( SavedFileName ) = 1 ) then
            SavedFileName := ''
          else
          begin
            for i := CaratCharPosition + 1 to Length( SavedFileName ) do
            begin
              SavedFileName[ i ] := SavedFileName[ i + 1 ];
            end;
            SetLength( SavedFileName, Length( SavedFileName ) - 1 );
          end;
        end;
      end
      else if Key = 37 then
      begin //left arrow
        if CaratCharPosition > 0 then
        begin
          CaratCharPosition := CaratCharPosition - 1;
          a := SavedFileName;
          SetLength( a, CaratCharPosition );
          CaratPosition := pText.TextLength( a );
        end; //endif
      end
      else if Key = 39 then
      begin //right arrow
        if CaratCharPosition < length( SavedFileName ) then
        begin
          CaratCharPosition := CaratCharPosition + 1;
          a := SavedFileName;
          SetLength( a, CaratCharPosition );
          CaratPosition := pText.TextLength( a );
        end; //endif
      end
      else if Key = 13 then
      begin
        AttemptToSaveGame;
      end;

         //Character.Name:=SavedFileName;
      pText.PlotText( SavedFileName, X1, Y1, 240 );
         //plot the Carat
      pText.PlotText( '|', CaratPosition + X1, Y1, 240 );
      lpDDSFront.Flip( nil, DDFLIP_WAIT );
      lpDDSBack.BltFast( 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
      MouseCursor.PlotDirty := false;
    end //endif
    else if key = 13 then
    begin
      AttemptToLoadGame;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end; //KeyDown

procedure TLoadGame.CaratTimerEvent( Sender : TObject );
var
  X1, Y1 : integer;
  P : TPoint;
  nRect : TRect;
const
  FailName : string = 'TLoadGame.Carattimereven';
begin

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) { and (CurrDbgGroup in Dbg????) } then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if ScrollState < 0 then
    begin
      GetCursorPos( P );
      if PtinRect( rect( 673, 203, 694, 218 ), P ) then
      begin //up arrow
        if ScrollState < -1 then
          inc( ScrollState )
        else
        begin
          if StartFile > 0 then
          begin
            StartFile := StartFile - 1;
            MoveList;
            ShowScreen;
          end;
        end;
      end
      else
        ScrollState := 0;
    end
    else if ScrollState > 0 then
    begin
      GetCursorPos( P );
      if PtinRect( rect( 673, 234, 694, 250 ), P ) then
      begin //down arrow
        if ScrollState > 1 then
          dec( ScrollState )
        else
        begin
          if StartFile + 8 < SelectRect.count - 1 then
          begin
            StartFile := StartFile + 1;
            MoveList;
            ShowScreen;
          end;
        end;
      end
      else
        ScrollState := 0;
    end;

    if LoadFile = false then
    begin
      if ( CurrentSelectedListItem > -1 ) and Loaded then
      begin
        nRect := pItem( SelectRect.Items[ CurrentSelectedListItem ] ).Rect;
        lpDDSBack.BltFast( nRect.left - 10, nRect.top - 5, DXBack, rect( nRect.left - 10, nRect.top - 5, nRect.right, nRect.bottom - 5 ), DDBLTFAST_WAIT );
        DrawAlpha( lpDDSBack, rect( nRect.left - 10, nRect.top - 5, nRect.right, nRect.bottom - 5 ), rect( 0, 0, 12, 12 ), DXBackHighlight, False, 40 );
        pText.PlotText( pItem( SelectRect.items[ CurrentSelectedListItem ] ).date, 590, pItem( SelectRect.items[ CurrentSelectedListItem ] ).rect.top, 240 );
        X1 := pItem( SelectRect.items[ CurrentSelectedListItem ] ).rect.left;
        Y1 := pItem( SelectRect.items[ CurrentSelectedListItem ] ).rect.top;

        inc( LoopCounter );
        if LoopCounter >= 5 then
        begin
          CaratVisible := ( CaratVisible = false );
          LoopCounter := 0;
        end;
        if CaratVisible then
        begin
          pText.PlotText( '|', CaratPosition + X1, Y1, 240 );
        end;
        pText.PlotText( SavedFileName, X1, Y1, 240 );

        lpDDSFront.Flip( nil, DDFLIP_WAIT );
        lpDDSBack.BltFast( 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
        MouseCursor.PlotDirty := false;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end; //TCreation.CaratTimerEvent


procedure TLoadGame.MouseDown( Sender : TAniview; Button : TMouseButton; Shift : TShiftState; X, Y, GridX, GridY : integer );
var
  i : integer;
  nRect : TRect;
  FileAlreadyExists : boolean;
  a : string;
const
  FailName : string = 'TLoadGame.MouseDown ';
begin

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try


    if ( DeleteBoxVisible = false ) and ( OverwriteBoxVisible = false ) and ( MustEnterNameBoxVisible = false ) then
    begin
      for i := 0 to SelectRect.count - 1 do
      begin
        if ( i >= StartFile ) and ( i < StartFile + 10 ) then
        begin
          if PtInRect( pItem( SelectRect.items[ i ] ).rect, point( x, y ) ) then
          begin
            if i <> CurrentSelectedListItem then
            begin
              CurrentSelectedListItem := i;
              SavedFileName := pItem( SelectRect.items[ i ] ).Text;
              CaratPosition := pText.TextLength( SavedFileName );
              CaratCharPosition := Length( SavedFileName );
              ShowScreen;
              if LoadGame( pItem( SelectRect.items[ i ] ).Text ) then
              begin //get the char name and map name from the saved file
                ShowInfo;
              end
              else
              begin
                     //lpDDSBack.BltFast(114,257,DXBack,rect(114,257,341,421),DDBLTFAST_WAIT);
                lpDDSBack.BltFast( 111, 65, DXBack, rect( 111, 65, 341, 231 ), DDBLTFAST_WAIT );
              end;
            end; //endif i <> CurrentSelectedListItem NEW July 8 2000
          end;
        end;
      end; //end for

      //check for scroll arrows
      if PtinRect( rect( 673, 203, 694, 218 ), point( X, Y ) ) then
      begin //up arrow
        if StartFile > 0 then
        begin
          StartFile := StartFile - 1;
          MoveList;
          ShowScreen;
          ScrollState := -3;
        end;
      end
      else if PtinRect( rect( 673, 234, 694, 250 ), point( X, Y ) ) then
      begin //down arrow
        if StartFile + 8 < SelectRect.count - 1 then
        begin
          StartFile := StartFile + 1;
          MoveList;
          ShowScreen;
          ScrollState := 3
        end;
      end //endif arrows
      else
        ScrollState := 0;

      if PtInRect( rect( 369, 400, 492, 428 ), point( x, y ) ) then
      begin //delete
        if ( CurrentSelectedListItem > -1 ) then
        begin
          if trim( pItem( SelectRect.items[ CurrentSelectedListItem ] ).Text ) <> '' then
          begin
            DeleteSavedFile;
          end;
        end;
      end
      else if PtInRect( rect( 581, 445, 581 + 121, 445 + 54 ), point( x, y ) ) then
      begin //load or save game
        if LoadFile then
        begin
          if CurrentSelectedListItem > -1 then
          begin
               //LoadThisFile:=ExtractFilePath(Application.ExeName) + 'Games\'+pItem(SelectRect.items[CurrentSelectedListItem]).text + '.sav';
            LoadThisFile := pItem( SelectRect.items[ CurrentSelectedListItem ] ).text;
            Close;
          end;
        end
        else
        begin //saving game
          if CurrentSelectedListItem > -1 then
          begin
            if length( trim( SavedFileName ) ) = 0 then
            begin
              MustEnterName;
            end
            else
            begin
              FileAlreadyExists := false; ////check to make sure this filename doesn't already exist
              for i := 0 to SelectRect.count - 1 do
              begin
                if lowercase( trim( pItem( SelectRect.items[ i ] ).text ) ) = lowercase( trim( SavedFileName ) ) then
                begin
                  FileAlreadyExists := true;
                end;
              end;
              if FileAlreadyExists then
              begin
                OverWriteSavedFile;
              end
              else
              begin
                LoadThisFile := SavedFileName; //pItem(SelectRect.items[CurrentSelectedListItem]).text;
                Close;
              end;
            end; //endif length
          end; //endif currentseleceditem
        end; //endif loadfile
      end
      else if PtInRect( rect( 95, 443, 95 + 165, 443 + 58 ), point( x, y ) ) then
      begin //cancel
        LoadThisFile := '';
        Close;
      end;
    end
    else
    begin //delete box is visible
      if DeleteBoxVisible then
      begin
        DeleteBoxVisible := false;
        nRect := pItem( SelectRect.Items[ CurrentSelectedListItem ] ).Rect;
           //if PtInRect(rect(422,509,474,541),point(x,y)) then begin //Yes pressed- del file
        if PtInRect( rect( nRect.left - 10 + 53, nRect.top + 32 + 78, nRect.left - 10 + 104, nRect.top + 32 + 109 ), point( x, y ) ) then
        begin //Yes pressed- del file
          DeleteFile( PChar( ExtractFilePath( Application.ExeName ) + 'Games\' + pItem( SelectRect.items[ CurrentSelectedListItem ] ).text + '.sav' ) );
          if FileExists( ExtractFilePath( Application.ExeName ) + 'Games\' + pItem( SelectRect.items[ CurrentSelectedListItem ] ).text + '.bmp' ) then
            DeleteFile( PChar( ExtractFilePath( Application.ExeName ) + 'Games\' + pItem( SelectRect.items[ CurrentSelectedListItem ] ).text + '.bmp' ) );
          try
            if FileExists( ExtractFilePath( Application.ExeName ) + 'Games\' + pItem( SelectRect.items[ CurrentSelectedListItem ] ).text + '.idx' ) then
              DeleteFile( PChar( ExtractFilePath( Application.ExeName ) + 'Games\' + pItem( SelectRect.items[ CurrentSelectedListItem ] ).text + '.idx' ) );
          except
          end;
          try
            if FileExists( ExtractFilePath( Application.ExeName ) + 'Games\' + pItem( SelectRect.items[ CurrentSelectedListItem ] ).text + '.map' ) then
              DeleteFile( PChar( ExtractFilePath( Application.ExeName ) + 'Games\' + pItem( SelectRect.items[ CurrentSelectedListItem ] ).text + '.map' ) );
          except
          end;
          a := ChangeFileExt( ArtPath + CharacterGif, '.pox' );
              //if FileExists(a) then
                 //DeleteFile(PChar(a)); -> This is OUT in a June 11 modification
          SelectRect.delete( CurrentSelectedListItem );
          CurrentSelectedListItem := -1;
          MoveList;
          Paint;
        end
        else if PtInRect( rect( nRect.left - 10 + 187, nRect.top + 32 + 78, nRect.left - 10 + 238, nRect.top + 32 + 109 ), point( x, y ) ) then
        begin //No pressed- dont delete
          MoveList;
          Paint;
        end;
      end
      else if OverwriteBoxVisible then
      begin
        nRect := pItem( SelectRect.Items[ CurrentSelectedListItem ] ).Rect;
        if PtInRect( rect( nRect.left - 10 + 53, nRect.top + 32 + 78, nRect.left - 10 + 104, nRect.top + 32 + 109 ), point( x, y ) ) then
        begin //Yes pressed- overwritefile
          OverwriteBoxVisible := false;
             //paint;
             //LoadThisFile:=ExtractFilePath(Application.ExeName) + 'Games\'+pItem(SelectRect.items[CurrentSelectedListItem]).text + '.sav';
          LoadThisFile := SavedFileName; //pItem(SelectRect.items[CurrentSelectedListItem]).text;
          Close;
        end
        else if PtInRect( rect( nRect.left - 10 + 187, nRect.top + 32 + 78, nRect.left - 10 + 238, nRect.top + 32 + 109 ), point( x, y ) ) then
        begin //No pressed- just clean screen
          OverwriteBoxVisible := false;
          paint;
        end;
      end
      else if MustEnterNameBoxVisible then
      begin
        nRect := pItem( SelectRect.Items[ CurrentSelectedListItem ] ).Rect;
        if PtInRect( rect( nRect.left - 10 + 122, nRect.top + 32 + 78, nRect.left - 10 + 175, nRect.top + 32 + 109 ), point( x, y ) ) then
        begin //Ok pressed
          MustEnterNameBoxVisible := false;
          paint;
        end;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end; //MouseDown

procedure TLoadGame.MouseMove( Sender : TAniview; Shift : TShiftState; X, Y, GridX, GridY : integer );
const
  FailName : string = 'TLoadGame.MouseMove ';
begin

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try


    if ( DeleteBoxVisible = false ) and ( OverwriteBoxVisible = false ) and ( MustEnterNameBoxVisible = false ) then
    begin
      //clear menu
  //    lpDDSBack.BltFast(368,52,DXBack,rect(368,52,671,386),DDBLTFAST_WAIT);
      //clear Delete
      //lpDDSBack.BltFast(8,417,DXBack,rect(8,417,64,436),DDBLTFAST_WAIT);
      //clear Cancel
      lpDDSBack.BltFast( ldCancelRect.Left, ldCancelRect.Top, DXBack, rect( ldCancelRect.Left, ldCancelRect.Top, ldCancelRect.Left + ldCancelRect.Right, ldCancelRect.Top + ldCancelRect.Bottom ), DDBLTFAST_WAIT );
      //Clear Load
      lpDDSBack.BltFast( DXLoadRect.Left, DXLoadRect.Top, DXBack, rect( DXLoadRect.Left, DXLoadRect.Top, DXLoadRect.Left + DXLoadRect.Right, DXLoadRect.Top + DXLoadRect.Bottom ), DDBLTFAST_WAIT );
      //highlight any options he's over
  {    for i:=0 to SelectRect.count -1 do begin
         //if its hightlighted plot highlight
         if (i=CurrentSelectedListItem) then begin
            nRect:=pItem(SelectRect.Items[CurrentSelectedListItem]).Rect;
            DrawAlpha(lpDDSBack,rect(nRect.left-10,nRect.top-5,nRect.right,nRect.bottom-5),rect(0,0,12,12),DXBackHighlight,False,40);
         end;
         if (LoadFile=false) and (CurrentSelectedListItem=i) then
            pText.PlotText(SavedFileName,pItem(SelectRect.items[i]).rect.left,pItem(SelectRect.items[i]).rect.top,240)
         else
            pText.PlotText(pItem(SelectRect.items[i]).text,pItem(SelectRect.items[i]).rect.left,pItem(SelectRect.items[i]).rect.top,240);

         pText.PlotText(pItem(SelectRect.items[i]).date,590,pItem(SelectRect.items[i]).rect.top,240);
      end;//end for
     }
      if PtInRect( rect( DXLoadRect.Left, DXLoadRect.Top, DXLoadRect.Left + DXLoadRect.Right, DXLoadRect.Top + DXLoadRect.Bottom ), point( x, y ) ) then
      begin //load game
        if CurrentSelectedListItem >= 0 then
          lpDDSBack.BltFast( DXLoadRect.Left, DXLoadRect.Top, DXLoad, rect( 0, 0, DXLoadRect.Right, DXLoadRect.Bottom ), DDBLTFAST_WAIT );
      end
      else if PtInRect( rect( ldCancelRect.Left, ldCancelRect.Top, ldCancelRect.Left + ldCancelRect.Right, ldCancelRect.Top + ldCancelRect.Bottom ), point( x, y ) ) then
      begin //cancel
        lpDDSBack.BltFast( ldCancelRect.Left, ldCancelRect.Top, DXCancel, rect( 0, 0, ldCancelRect.Right, ldCancelRect.Bottom ), DDBLTFAST_WAIT );
      end;

    end;

//  if LoadFile then begin
    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    lpDDSBack.BltFast( 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;
//  end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end; //MouseMove

procedure TLoadGame.FormMouseMove( Sender : TObject; Shift : TShiftState; X, Y : Integer );
const
  FailName : string = 'TLoadGame.FormMouseMove ';
begin

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    //clear Cancel
    lpDDSBack.BltFast( 95, 443, DXBack, rect( 95, 443, 95 + 165, 443 + 58 ), DDBLTFAST_WAIT );
    //Clear Load
    lpDDSBack.BltFast( 581, 445, DXBack, rect( 581, 445, 581 + 121, 445 + 54 ), DDBLTFAST_WAIT );
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end; //FormMouseMove


procedure TLoadGame.Paint;
const
  FailName : string = 'TLoadGame.paint ';
begin

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    lpDDSBack.BltFast( 0, 0, DXBack, Rect( 0, 0, 800, 600 ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    PlotMenu;
    ShowScreen;
    if CurrentSelectedListItem > -1 then
    begin
      if LoadGame( pItem( SelectRect.items[ CurrentSelectedListItem ] ).Text ) then
      begin //get the char name and map name from the saved file
        ShowInfo;
      end;
    end
    else
    begin
      lpDDSBack.BltFast( 114, 257, DXBack, rect( 114, 257, 344, 421 ), DDBLTFAST_WAIT );
      lpDDSBack.BltFast( 111, 65, DXBack, rect( 111, 65, 344, 231 ), DDBLTFAST_WAIT );
    end;

    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    lpDDSBack.BltFast( 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end; //paint


procedure TLoadGame.DeleteSavedFile;
var
  BM : TBitmap;
  DXBorders : IDirectDrawSurface;
  InvisColor : integer;
  nRect : TRect;
const
  FailName : string = 'TLoadGame.DeleteSavedFile ';
begin

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    BM := TBitmap.Create;
  //transparent color
    InvisColor := $00FFFF00;

    BM.LoadFromFile( InterfacePath + 'ldChooseBox.bmp' );
    DXBorders := DDGetImage( lpDD, BM, InvisColor, False );
  //lpDDSBack.BltFast(369, 431, DXBorders, Rect(0, 0, BM.width, BM.Height), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
    nRect := pItem( SelectRect.Items[ CurrentSelectedListItem ] ).Rect;
    lpDDSBack.BltFast( nRect.left - 10, nRect.top + 32, DXBorders, Rect( 0, 0, BM.width, BM.Height ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

    DXBorders := nil;

    if UseSmallFont then
      pText.PlotGoldTextBlock( txtMessage[ 0 ], nRect.left - 10 + 23, nRect.left - 10 + 281, nRect.top + 52, 240 ) //392,650,451,240)
    else
      pText.PlotTextBlock( txtMessage[ 0 ], nRect.left - 10 + 23, nRect.left - 10 + 281, nRect.top + 52, 240 ); //392,650,451,240);

  //DXDirty:= DDGetSurface(lpDD,300,117, InvisColor,true);
  //DXDirty.BltFast(0, 0, lpDDSBack, rect(369,431,669,548), DDBLTFAST_WAIT);
  //DXDirty.BltFast(0, 0, lpDDSBack, rect(nRect.left-10,nRect.top+32,nRect.left-10+300,nRect.top+32+117), DDBLTFAST_WAIT);

    DeleteBoxVisible := true;

    BM.Free;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end; //DeleteSavedFile

procedure TLoadGame.OverwriteSavedFile;
var
  BM : TBitmap;
  DXBorders : IDirectDrawSurface;
  InvisColor : integer;
  nRect : TRect;
const
  FailName : string = 'TCharacter.Attack ';
begin

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    BM := TBitmap.Create;
  //transparent color
    InvisColor := $00FFFF00;

    BM.LoadFromFile( InterfacePath + 'ldChooseBox.bmp' );
    DXBorders := DDGetImage( lpDD, BM, InvisColor, False );
  //lpDDSBack.BltFast(369, 431, DXBorders, Rect(0, 0, BM.width, BM.Height), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT);
    nRect := pItem( SelectRect.Items[ CurrentSelectedListItem ] ).Rect;
    lpDDSBack.BltFast( nRect.left - 10, nRect.top + 32, DXBorders, Rect( 0, 0, BM.width, BM.Height ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

    DXBorders := nil;

    if UseSmallFont then
      pText.PlotGoldTextBlock( txtMessage[ 1 ], nRect.left - 10 + 23, nRect.left - 10 + 281, nRect.top + 52, 240 ) //392,650,451,240)
    else
      pText.PlotTextBlock( txtMessage[ 1 ], nRect.left - 10 + 23, nRect.left - 10 + 281, nRect.top + 52, 240 ); //392,650,451,240);

  //DXDirty:= DDGetSurface(lpDD,300,117, InvisColor,true);
  //DXDirty.BltFast(0, 0, lpDDSBack, rect(369,431,669,548), DDBLTFAST_WAIT);
  //DXDirty.BltFast(0, 0, lpDDSBack, rect(nRect.left-10,nRect.top+32,nRect.left-10+300,nRect.top+32+117), DDBLTFAST_WAIT);

    OverwriteBoxVisible := true;

    BM.Free;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end; //OverwriteSavedFile

procedure TLoadGame.MustEnterName;
var
  BM : TBitmap;
  DXBorders : IDirectDrawSurface;
  InvisColor : integer;
  nRect : TRect;
begin
  BM := TBitmap.Create;
  //transparent color
  InvisColor := $00FFFF00;

  BM.LoadFromFile( InterfacePath + 'ldChooseBox.bmp' );
  DXBorders := DDGetImage( lpDD, BM, InvisColor, False );

  nRect := pItem( SelectRect.Items[ CurrentSelectedListItem ] ).Rect;
  lpDDSBack.BltFast( nRect.left - 10, nRect.top + 32, DXBorders, Rect( 0, 0, BM.width, BM.Height ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
  lpDDSBack.BltFast( nRect.left - 10, nRect.top + 32 + 75, DXok, Rect( 0, 0, 300, 42 ), DDBLTFAST_WAIT );
  DXBorders := nil;

  if UseSmallFont then
    pText.PlotGoldTextBlock( txtMessage[ 2 ], nRect.left - 10 + 23, nRect.left - 10 + 281, nRect.top + 52, 240 ) //392,650,451,240)
  else
    pText.PlotTextBlock( txtMessage[ 2 ], nRect.left - 10 + 23, nRect.left - 10 + 281, nRect.top + 52, 240 ); //392,650,451,240);

  //DXDirty:= DDGetSurface(lpDD,300,117, InvisColor,true);
  //DXDirty.BltFast(0, 0, lpDDSBack, rect(369,431,669,548), DDBLTFAST_WAIT);
  //DXDirty.BltFast(0, 0, lpDDSBack, rect(nRect.left-10,nRect.top+32,nRect.left-10+300,nRect.top+32+117), DDBLTFAST_WAIT);

  MustEnterNameBoxVisible := true;

  BM.Free;

end; //MustEnterName

procedure TLoadGame.ShowScreen;
var
  PicName : string;
  BM : TBitmap;
  DXTemp : IDirectDrawSurface;
  InvisColor : integer;
  i : integer;
  nRect : TRect;
const
  FailName : string = 'TLoadGame.ShowScreen ';
begin

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try


    if ( DeleteBoxVisible = false ) and ( OverwriteBoxVisible = false ) then
    begin
      //clear menu
      lpDDSBack.BltFast( 368, 52, DXBack, rect( 368, 52, 671, 386 ), DDBLTFAST_WAIT );
      //highlight any options he's over
      for i := 0 to SelectRect.count - 1 do
      begin
     // for i:= 0  to 9 do begin
             //if its hightlighted plot highlight
        if ( i >= StartFile ) and ( i < StartFile + 9 ) then
        begin
          if ( i = CurrentSelectedListItem ) then
          begin
            nRect := pItem( SelectRect.Items[ CurrentSelectedListItem ] ).Rect;
            DrawAlpha( lpDDSBack, rect( nRect.left - 10, nRect.top - 5, nRect.right, nRect.bottom - 5 ), rect( 0, 0, 12, 12 ), DXBackHighlight, False, 40 );
          end;
          pText.PlotText( pItem( SelectRect.items[ i ] ).text, pItem( SelectRect.items[ i ] ).rect.left, pItem( SelectRect.items[ i ] ).rect.top, 240 );
          pText.PlotText( pItem( SelectRect.items[ i ] ).date, 590, pItem( SelectRect.items[ i ] ).rect.top, 240 );
        end;
      end; //end for

    end; //endif

    if CurrentSelectedListItem > -1 then
      PicName := ExtractFilePath( Application.ExeName ) + 'Games\' + pItem( SelectRect.items[ CurrentSelectedListItem ] ).text + '.bmp'
    else
      PicName := '';

    if ( PicName <> '' ) and FileExists( PicName ) then
    begin
      BM := TBitmap.Create;
      InvisColor := $00FFFF00;
      BM.LoadFromFile( PicName );
      DXTemp := DDGetImage( lpDD, BM, InvisColor, False );

      lpDDSBack.BltFast( 114, 257, DXTemp, rect( 0, 0, 225, 162 ), DDBLTFAST_WAIT );
      BM.free;
      DXTemp := nil;
    end
    else
    begin
      lpDDSBack.BltFast( 114, 257, DXBack, rect( 114, 257, 340, 420 ), DDBLTFAST_WAIT );
    end; //endif


    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    lpDDSBack.BltFast( 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end; //TLoadGame.ShowScreen;


procedure TLoadGame.ShowInfo;
var
  i : integer;
const
  FailName : string = 'TLoadGame.Showinfo ';
begin

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try

    lpDDSBack.BltFast( 111, 65, DXBack, rect( 111, 65, 344, 231 ), DDBLTFAST_WAIT );

    pText.PlotTextBlock( MapName, 123, 340, 70, 240 );

    for i := 1 to CharacterCount do
    begin
      pText.PlotText( CharacterName[ i ], 133, 70 + i * 25, 240 );
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end; //ShowInfo

procedure TLoadGame.AttemptToLoadGame;
const
  FailName : string = 'TLoadGame.AttemptToLoadGame';
begin

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) { and (CurrDbgGroup in Dbg????) } then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if LoadFile then
    begin
      if CurrentSelectedListItem > -1 then
      begin
        LoadThisFile := pItem( SelectRect.items[ CurrentSelectedListItem ] ).text;
        Close;
      end;
    end
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end; //AttemptToLoadGame

procedure TLoadGame.AttemptToSaveGame;
var
  FileAlreadyExists : boolean;
  i : integer;
const
  FailName : string = 'TLoadGame.Attempt tosave';
begin

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) { and (CurrDbgGroup in Dbg????) } then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if CurrentSelectedListItem > -1 then
    begin
      if length( trim( SavedFileName ) ) = 0 then
      begin
        MustEnterName;
      end
      else
      begin
        FileAlreadyExists := false; ////check to make sure this filename doesn't already exist
        for i := 0 to SelectRect.count - 1 do
        begin
          if lowercase( trim( pItem( SelectRect.items[ i ] ).text ) ) = lowercase( trim( SavedFileName ) ) then
          begin
            FileAlreadyExists := true;
          end;
        end;
        if FileAlreadyExists then
        begin
          OverWriteSavedFile;
        end
        else
        begin
          LoadThisFile := SavedFileName; //pItem(SelectRect.items[CurrentSelectedListItem]).text;
          Close;
        end;
      end; //endif length
    end; //endif currentseleceditem
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end; //AttemptToSaveGame

procedure TLoadGame.Release;
var
  i : integer;
const
  FailName : string = 'TLoadGame.release ';
begin
  ExText.close;
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try


    if assigned( CaratTimer ) then
    begin
      CaratTimer.enabled := false;
      CaratTimer.free;
      CaratTimer := nil;
    end;

    pText.UnloadGoldFontGraphic;
    DXBack := nil;
    DXBackHighlight := nil;
    DXLoad := nil;
    DXCancel := nil;
    DXok := nil;

    if Assigned( SelectRect ) then
    begin
      for i := 0 to ( SelectRect.Count - 1 ) do
      begin
        pTextItem := pItem( SelectRect.Items[ i ] );
        Dispose( pTextItem );
      end;
      SelectRect.Free;
      SelectRect := nil;
    end;
    inherited;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TLoadGame.LoadGame( GameName : string ) : boolean;
var
  List : TStringList;
  S : string;
  Stream : TFileStream;
  EOB, BB : word;
  P, L : LongWord;
  Block : TSavBlocks;
  Filename : string;
  FoundCharacters : boolean;
const
  FailName : string = 'TCharacter.Attack ';
begin

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  result := false;
  try

    EOB := EOBMarker;
    CharacterCount := 0;
    TravelBlock := '';
    FoundCharacters := false;
    try
      Filename := ExtractFilePath( Application.ExeName ) + 'games\' + GameName + '.idx';
      if not FileExists( Filename ) then
        Filename := ExtractFilePath( Application.ExeName ) + 'games\' + GameName + '.sav';
    //Level:=lowercase(ChangeFileExt(ExtractFilename(LVLFile),''));
    //Scene:=CurrentScene;
      Stream := TFileStream.Create( Filename, fmOpenRead or fmShareCompat );
      try
        List := TStringList.create;
        try
          while Stream.Position < Stream.Size do
          begin
            Stream.Read( Block, sizeof( Block ) );
            Stream.Read( L, sizeof( L ) );
            P := Stream.Position;
            case Block of
              sbMap :
                begin
                  SetLength( S, L );
                  Stream.Read( S[ 1 ], L );
                  List.Text := S;
                  MapName := List.Values[ 'Map' ];
                  SceneName := List.Values[ 'Scene' ];
                end;
              sbTravel :
                begin
                  SetLength( TravelBlock, L );
                  Stream.Read( TravelBlock[ 1 ], L );
                end;
              sbCharacter :
                begin
                //Log.Log('  Loading character block');
                  SetLength( S, L );
                  Stream.Read( S[ 1 ], L );
                  List.Text := S;
                  CharacterCount := CharacterCount + 1;
                  CharacterName[ CharacterCount ] := List.Values[ 'CharacterName' ];
                  CharacterGif := List.Values[ 'Resource' ];
                  FoundCharacters := true;
                end;
              sbItem :
                begin
                end;
            else
              begin
                if FoundCharacters then
                  break;
              end;
            end;
            Stream.Seek( P + L, soFromBeginning );
            Stream.Read( BB, sizeof( BB ) );
            if BB <> EOB then
            begin
            //Log.Log('*** Error:  EOB not found');
              exit;
            end;
          end;
        finally
          List.free;
        end;
      finally
        Stream.free;
      end;
    except
      exit;
    end;

    result := true;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TLoadGame.MouseUp( Sender : TAniview; Button : TMouseButton;
  Shift : TShiftState; X, Y, GridX, GridY : integer );
begin
  inherited;

  ScrollState := 0;
end;

end.
