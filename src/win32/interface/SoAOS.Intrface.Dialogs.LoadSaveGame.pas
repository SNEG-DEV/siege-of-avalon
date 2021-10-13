unit SoAOS.Intrface.Dialogs.LoadSaveGame;
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

  Description: Load/Save Dialog - was LoadGame.pas - a lot more clean-up is coming

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
  System.IOUtils,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.ExtCtrls,
  SoAOS.Intrface.Dialogs,
  GameText,
  Engine,
  SoAOS.Animation,
  Logfile;

type

  pItem = ^SelectableRect;
  SelectableRect = record
    rect : TRect;
    time : integer;
    text : string;
    date : string;
  end;

  TLoadGame = class( TDialog )
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
    function GetCancelRect: TRect;
    function GetLoadSaveRect: TRect;
  protected
    procedure MouseDown( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer ); override;
    procedure MouseMove( Sender : TObject;
      Shift : TShiftState; X, Y, GridX, GridY : Integer ); override;
    procedure MouseUp( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer ); override;
    procedure MouseWheel( Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean );
    procedure KeyDown( Sender : TObject; var key : Word; Shift : TShiftState ); override;
  public
    frmMain : TForm; //we need the  form passed into handle form mouse events
    LoadThisFile : string; //This is the file to Save To or to Load From right here!
    LastSavedFile : string; //The caller loads this string so we know what the last file saved to\Loaded from is.
    LoadFile : boolean;
    SceneName : string;
    MapName : string;
    TravelBlock : AnsiString;
    procedure Paint; override;
    procedure Init; override;
    procedure Release; override;
    property CancelRect: TRect read GetCancelRect;
    property LoadSaveRect: TRect read GetLoadSaveRect;
  end;

implementation

uses
  SoAOS.Types,
  SoAOS.Intrface.Text,
  SoAOS.Graphics.Draw,
  AniDemo,
  Resource,
  SaveFile;

{ TLoadGame }

procedure TLoadGame.Init;
var
  DXTemp : IDirectDrawSurface;
  i : integer;
  pr : TRect;
const
  FailName : string = 'TLoadGame.init ';
begin

  Log.DebugLog( FailName );
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
    frmMain.OnMouseWheel := MouseWheel;

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

    //TODO: The rects are defined identical - clean
    if LoadFile then
      DXLoad := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'ldLoadLight.bmp', cInvisColor )
    else
      DXLoad := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'ldSaveLight.bmp', cInvisColor );

    DXCancel := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'ldCancel.bmp', cInvisColor );

    DXok := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'ldOk.bmp', cInvisColor );

    DXBackHighlight := SoAOS_DX_LoadBMP( InterfacePath + 'opYellow.bmp', cInvisColor );

    DXBack := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'ldLoadSave.bmp', cInvisColor, DlgWidth, DlgHeight );
    if ScreenMetrics.borderFile<>'' then //Neu hinpinseln, da z.B. DoA ein grünes Menü hat
        lpDDSBack.BltFast( 0, 0, TfrmMain(frmMain).FillBorder, nil, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    if LoadFile then
      DXTemp := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'ldLoadDark.bmp', cInvisColor )
    else
      DXTemp := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'ldSaveDark.bmp', cInvisColor );
    pr := Rect( 0, 0, DlgRect.dlgLoadSaveRect.Width, DlgRect.dlgLoadSaveRect.Height );
    DXBack.BltFast( DlgRect.dlgLoadSaveRect.Left, DlgRect.dlgLoadSaveRect.Top, DXTemp, @pr, DDBLTFAST_WAIT );

    DXTemp := nil;

    if LoadFile then
      DXTemp := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'ldLoadUpper.bmp', cInvisColor )
    else
      DXTemp := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'ldSaveUpper.bmp', cInvisColor );
    pr := Rect( 0, 0, DlgRect.dlgLoadSaveTitleRect.Width, DlgRect.dlgLoadSaveTitleRect.Height );
    DXBack.BltFast( DlgRect.dlgLoadSaveTitleRect.Left, DlgRect.dlgLoadSaveTitleRect.Top, DXTemp, @pr, DDBLTFAST_WAIT );
    pr := Rect( 0, 0, DlgWidth, DlgHeight );
    lpDDSBack.BltFast( Offset.X, Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

    DXTemp := nil;
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

    SoAOS_DX_BltFront;
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
  dt : TDateTime;
const
  FailName : string = 'TLoadGame.LoadText ';
begin

  Log.DebugLog( FailName );
  try
   //Search for the first file meeting our criteria - create a Find file structure, and assign it a handle
    FileNotFound := FindFirst( GamesPath + '*.sav', faAnyFile, FileData );
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
        FileAge( GamesPath + FileData.Name, dt );
        pTextitem.time := DateTimeToFileDate( dt ); //INVESTIGATE: Not sure if pTextitem.time makes any sense anymore
        DecodeDate( dt, fYear, fMonth, fDay );
        DecodeTime( dt, fHour, fMin, fSec, fMsec );
        if fMin > 10 then
          pTextItem.Date := intToStr( fMonth ) + '/' + intToStr( fDay ) + ' ' + intToStr( fHour ) + ':' + intToStr( fMin ) //'/'+intToStr(fYear);
        else
          pTextItem.Date := intToStr( fMonth ) + '/' + intToStr( fDay ) + ' ' + intToStr( fHour ) + ':0' + intToStr( fMin ); //'/'+intToStr(fYear);
        pTextItem.Rect := ApplyOffset( Rect( 379, 66 + i * 35, 669, 66 + i * 35 + 35 ) );
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
  r: TRect;
const
  FailName : string = 'TLoadGame.PlotMenu ';
begin
  Log.DebugLog( FailName );
  try

    j := 0;
    for i := 0 to SelectRect.count - 1 do
    begin
      if ( i >= StartFile ) and ( i < StartFile + 9 ) then
      begin //only show 9 files
        r := ApplyOffset( Rect( 379, 66 + j * 35, 669, 66 + j * 35 + 35 ) );
        pItem( SelectRect.items[ i ] ).rect := r;
        if Loadfile then
        begin
        //When saving, errorcode in Siege.log, so "if Loadfile" added
        pText.WriteText(pItem( SelectRect.items[ i ] ).text, pItem( SelectRect.items[ i ] ).rect.left, pItem( SelectRect.items[ i ] ).rect.top, 14);
        //ptext.PlotText( pItem( SelectRect.items[ i ] ).text, pItem( SelectRect.items[ i ] ).rect.left, pItem( SelectRect.items[ i ] ).rect.top, 240 );
        pText.WriteText(pItem( SelectRect.items[ i ] ).date, 590 + Offset.X, pItem( SelectRect.items[ i ] ).rect.top, 10);
        //ptext.PlotText( pItem( SelectRect.items[ i ] ).date, 590 + Offset.X, pItem( SelectRect.items[ i ] ).rect.top, 240 );
        end
        else
        begin
        ptext.PlotText( pItem( SelectRect.items[ i ] ).text, pItem( SelectRect.items[ i ] ).rect.left, pItem( SelectRect.items[ i ] ).rect.top, 240 );
        ptext.PlotText( pItem( SelectRect.items[ i ] ).date, 590 + Offset.X, pItem( SelectRect.items[ i ] ).rect.top, 240 );
        end;
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

  Log.DebugLog( FailName );
  try
    j := 0;
    for i := 0 to SelectRect.count - 1 do
    begin
      if ( i >= StartFile ) and ( i < StartFile + 9 ) then
      begin //only show 10 files
        pItem( SelectRect.items[ i ] ).Rect := ApplyOffset( Rect( 379, 66 + j * 35, 669, 66 + j * 35 + 35 ) );
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
  pr : TRect;
const
  FailName : string = 'TLoadGame.Keydown ';
begin

  Log.DebugLog( FailName );
  try
    if ( CurrentSelectedListItem > -1 ) and ( LoadFile = false ) and ( DeleteBoxVisible = false ) and ( OverwriteBoxVisible = false ) and ( MustEnterNameBoxVisible = false ) then
    begin
      X1 := pItem( SelectRect.items[ CurrentSelectedListItem ] ).rect.left;
      Y1 := pItem( SelectRect.items[ CurrentSelectedListItem ] ).rect.top;
      nRect := pItem( SelectRect.Items[ CurrentSelectedListItem ] ).Rect;
         //lpDDSBack.BltFast(X1,Y1,DXBack,rect(X1,Y1,586,Y1+24),DDBLTFAST_WAIT);
      pr := Rect( nRect.left - 10, nRect.top - 5, nRect.right, nRect.bottom - 5 );
      lpDDSBack.BltFast( nRect.left - 10, nRect.top - 5, DXBack, @pr, DDBLTFAST_WAIT );
      DrawAlpha( lpDDSBack, rect( nRect.left - 10, nRect.top - 5, nRect.right, nRect.bottom - 5 ), rect( 0, 0, 12, 12 ), DXBackHighlight, False, 40 );
      pText.PlotText( pItem( SelectRect.items[ CurrentSelectedListItem ] ).date, 590 + Offset.X, pItem( SelectRect.items[ CurrentSelectedListItem ] ).rect.top, 240 );
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
      SoAOS_DX_BltFront;
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
  pr : TRect;
const
  FailName : string = 'TLoadGame.Carattimereven';
begin

  Log.DebugLog( FailName );
  try
    if ScrollState < 0 then
    begin
      P := Mouse.CursorPos;
//      GetCursorPos( P );
      pr := ApplyOffset( Rect( 673, 203, 694, 218 ) );
      if pr.Contains( P ) then
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
      // GetCursorPos( P );
      P := Mouse.CursorPos;
      pr := ApplyOffset( Rect( 673, 234, 694, 250 ) );
      if pr.Contains( P ) then
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
        pr := Rect( nRect.left - 10 - Offset.X, nRect.top - 5 - Offset.Y, nRect.right - Offset.X, nRect.bottom - 5 - Offset.Y);
        lpDDSBack.BltFast( nRect.left - 10, nRect.top - 5, DXBack, @pr, DDBLTFAST_WAIT );
        DrawAlpha( lpDDSBack, rect( nRect.left - 10, nRect.top - 5, nRect.right, nRect.bottom - 5 ), rect( 0, 0, 12, 12 ), DXBackHighlight, False, 40 );
        pText.PlotText( pItem( SelectRect.items[ CurrentSelectedListItem ] ).date, 590 + Offset.X, pItem( SelectRect.items[ CurrentSelectedListItem ] ).rect.top, 240 );
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

        SoAOS_DX_BltFront;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end; //TCreation.CaratTimerEvent


procedure TLoadGame.MouseDown( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer );
var
  i : integer;
  nRect : TRect;
  FileAlreadyExists : boolean;
  a : string;
  pr, pr1, pr2 : TRect;
const
  FailName : string = 'TLoadGame.MouseDown ';
begin

  Log.DebugLog( FailName );
  try

    if ( DeleteBoxVisible = false ) and ( OverwriteBoxVisible = false ) and ( MustEnterNameBoxVisible = false ) then
    begin
      for i := 0 to SelectRect.count - 1 do
      begin
        if ( i >= StartFile ) and ( i < StartFile + 10 ) then
        begin
          if pItem( SelectRect.items[ i ] ).rect.Contains( Point( x, y ) ) then
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
                pr := rect( 111, 65, 341, 231 );
                lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
              end;
            end; //endif i <> CurrentSelectedListItem NEW July 8 2000
          end;
        end;
      end; //end for

      //check for scroll arrows
      pr1 := ApplyOffset( Rect( 673, 203, 694, 218 ) );
      pr2 := ApplyOffset( Rect( 673, 234, 694, 250 ) );
      if pr1.Contains( Point( X, Y ) ) then
      begin //up arrow
        if StartFile > 0 then
        begin
          StartFile := StartFile - 1;
          MoveList;
          ShowScreen;
          ScrollState := -3;
        end;
      end
      else if pr2.Contains( Point( X, Y ) ) then
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

      pr1 := ApplyOffset( Rect( 369, 400, 492, 428 ) );
      if pr1.Contains( Point( x, y ) ) then
      begin //delete
        if ( CurrentSelectedListItem > -1 ) then
        begin
          if trim( pItem( SelectRect.items[ CurrentSelectedListItem ] ).Text ) <> '' then
          begin
            DeleteSavedFile;
          end;
        end;
      end
      else if LoadSaveRect.Contains( Point (X, Y)) then // PtInRect( rect( 581, 445, 581 + 121, 445 + 54 ), point( x, y ) ) then
      begin //load or save game
        if LoadFile then
        begin
          if CurrentSelectedListItem > -1 then
          begin
               //LoadThisFile:=DefaultPath + 'Games\'+pItem(SelectRect.items[CurrentSelectedListItem]).text + '.sav';
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
      else if CancelRect.Contains( Point( X, Y)) then // PtInRect( rect( 95, 443, 95 + 165, 443 + 58 ), point( x, y ) ) then
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
          TFile.Delete( GamesPath + pItem( SelectRect.items[ CurrentSelectedListItem ] ).text + '.sav' );
          if TFile.Exists( GamesPath + pItem( SelectRect.items[ CurrentSelectedListItem ] ).text + '.bmp' ) then
            TFile.Delete( GamesPath + pItem( SelectRect.items[ CurrentSelectedListItem ] ).text + '.bmp' );
          try
            if TFile.Exists( GamesPath + pItem( SelectRect.items[ CurrentSelectedListItem ] ).text + '.idx' ) then
              TFile.Delete( GamesPath + pItem( SelectRect.items[ CurrentSelectedListItem ] ).text + '.idx' );
          except
          end;
          try
            if TFile.Exists( GamesPath + pItem( SelectRect.items[ CurrentSelectedListItem ] ).text + '.map' ) then
              TFile.Delete( GamesPath + pItem( SelectRect.items[ CurrentSelectedListItem ] ).text + '.map' );
          except
          end;
          a := ChangeFileExt( ResourcePath + CharacterGif, '.pox' );
              //if TFile.Exists(a) then
                 //TDelete.File(a); -> This is OUT in a June 11 modification
          SelectRect.Delete( CurrentSelectedListItem );
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
             //LoadThisFile:=DefaultPath + 'Games\'+pItem(SelectRect.items[CurrentSelectedListItem]).text + '.sav';
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

procedure TLoadGame.MouseMove( Sender : TObject;
      Shift : TShiftState; X, Y, GridX, GridY : Integer );
const
  FailName : string = 'TLoadGame.MouseMove ';
var
  pr : TRect;
begin

  Log.DebugLog( FailName );
  try
    if ( DeleteBoxVisible = false ) and ( OverwriteBoxVisible = false ) and ( MustEnterNameBoxVisible = false ) then
    begin
      //clear menu
  //    lpDDSBack.BltFast(368,52,DXBack,rect(368,52,671,386),DDBLTFAST_WAIT);
      //clear Delete
      //lpDDSBack.BltFast(8,417,DXBack,rect(8,417,64,436),DDBLTFAST_WAIT);
      //clear Cancel
      lpDDSBack.BltFast( CancelRect.Left, CancelRect.Top, DXBack, @DlgRect.dlgLoadSaveCancelRect, DDBLTFAST_WAIT );
      //Clear Load
      lpDDSBack.BltFast( LoadSaveRect.Left, LoadSaveRect.Top, DXBack, @DlgRect.dlgLoadSaveRect, DDBLTFAST_WAIT );
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
      if LoadSaveRect.Contains( Point( x, y ) ) then
      begin //load game
        if CurrentSelectedListItem >= 0 then
        begin
          pr := Rect( 0, 0, DlgRect.dlgLoadSaveRect.Width, DlgRect.dlgLoadSaveRect.Height );
          lpDDSBack.BltFast( LoadSaveRect.Left, LoadSaveRect.Top, DXLoad, @pr, DDBLTFAST_WAIT );
        end;
      end
      else if CancelRect.Contains( Point( x, y ) ) then
      begin //cancel
        pr := Rect( 0, 0, DlgRect.dlgLoadSaveCancelRect.Width, DlgRect.dlgLoadSaveCancelRect.Height );
        lpDDSBack.BltFast( CancelRect.Left, CancelRect.Top, DXCancel, @pr, DDBLTFAST_WAIT );
      end;

    end;

//  if LoadFile then begin
    SoAOS_DX_BltFront;
//  end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end; //MouseMove

procedure TLoadGame.FormMouseMove( Sender : TObject; Shift : TShiftState; X, Y : Integer );
const
  FailName : string = 'TLoadGame.FormMouseMove ';
var
  pr : TRect;
begin

  Log.DebugLog( FailName );
  try
    //clear Cancel
    pr := Rect( 95, 443, 95 + 165, 443 + 58 );
    lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
    //Clear Load
    pr := rect( 581, 445, 581 + 121, 445 + 54 );
    lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function TLoadGame.GetCancelRect: TRect;
begin
  Result := ApplyOffset( DlgRect.dlgLoadSaveCancelRect );
end;

function TLoadGame.GetLoadSaveRect: TRect;
begin
  Result := ApplyOffset( DlgRect.dlgLoadSaveRect );
end;

//FormMouseMove


procedure TLoadGame.Paint;
const
  FailName : string = 'TLoadGame.paint ';
var
  pr : TRect;
begin

  Log.DebugLog( FailName );
  try
    pr := Rect( 0, 0, DlgWidth, DlgHeight );
    lpDDSBack.BltFast( Offset.X, Offset.Y, DXBack, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
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
      pr := Rect( 114, 257, 344, 421 );
      lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
      pr := Rect( 111, 65, 344, 231 );
      lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
    end;

    SoAOS_DX_BltFront;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end; //paint

procedure TLoadGame.DeleteSavedFile;
var
  width, height : Integer;
  DXBorders : IDirectDrawSurface;
  nRect : TRect;
  pr : TRect;
const
  FailName : string = 'TLoadGame.DeleteSavedFile ';
begin

  Log.DebugLog( FailName );
  try

    DXBorders := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'ldChooseBox.bmp', cInvisColor, width, height );
    nRect := pItem( SelectRect.Items[ CurrentSelectedListItem ] ).Rect;
    pr := Rect( 0, 0, width, height );
    lpDDSBack.BltFast( nRect.left - 10, nRect.top + 32, DXBorders, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

    DXBorders := nil;

    if UseSmallFont then
      pText.PlotGoldTextBlock( txtMessage[ 0 ], nRect.left - 10 + 23, nRect.left - 10 + 281, nRect.top + 52, 240 ) //392,650,451,240)
    else
      pText.PlotTextBlock( txtMessage[ 0 ], nRect.left - 10 + 23, nRect.left - 10 + 281, nRect.top + 52, 240 ); //392,650,451,240);

    DeleteBoxVisible := true;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end; //DeleteSavedFile

procedure TLoadGame.OverwriteSavedFile;
var
  width, height : Integer;
  DXBorders : IDirectDrawSurface;
  nRect : TRect;
  pr : TRect;
const
  FailName : string = 'TLoadGame.OverwriteSavedFile ';
begin

  Log.DebugLog( FailName );
  try

    DXBorders := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'ldChooseBox.bmp', cInvisColor, width, height );
    nRect := pItem( SelectRect.Items[ CurrentSelectedListItem ] ).Rect;
    pr := Rect( 0, 0, width, height );
    lpDDSBack.BltFast( nRect.left - 10, nRect.top + 32, DXBorders, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );

    DXBorders := nil;

    if UseSmallFont then
      pText.PlotGoldTextBlock( txtMessage[ 1 ], nRect.left - 10 + 23, nRect.left - 10 + 281, nRect.top + 52, 240 ) //392,650,451,240)
    else
      pText.PlotTextBlock( txtMessage[ 1 ], nRect.left - 10 + 23, nRect.left - 10 + 281, nRect.top + 52, 240 ); //392,650,451,240);

    OverwriteBoxVisible := true;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end; //OverwriteSavedFile

procedure TLoadGame.MustEnterName;
var
  width, height : Integer;
  DXBorders : IDirectDrawSurface;
  nRect : TRect;
  pr : TRect;
begin
  DXBorders := SoAOS_DX_LoadBMP( InterfaceLanguagePath + 'ldChooseBox.bmp', cInvisColor, width, height );
  nRect := pItem( SelectRect.Items[ CurrentSelectedListItem ] ).Rect;
  pr := Rect( 0, 0, width, height );
  lpDDSBack.BltFast( nRect.left - 10, nRect.top + 32, DXBorders, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
  pr := Rect( 0, 0, 300, 42 );
  lpDDSBack.BltFast( nRect.left - 10, nRect.top + 32 + 75, DXok, @pr, DDBLTFAST_WAIT );
  DXBorders := nil;

  if UseSmallFont then
    pText.PlotGoldTextBlock( txtMessage[ 2 ], nRect.left - 10 + 23, nRect.left - 10 + 281, nRect.top + 52, 240 ) //392,650,451,240)
  else
    pText.PlotTextBlock( txtMessage[ 2 ], nRect.left - 10 + 23, nRect.left - 10 + 281, nRect.top + 52, 240 ); //392,650,451,240);

  MustEnterNameBoxVisible := true;

end; //MustEnterName

procedure TLoadGame.ShowScreen;
var
  PicName : string;
  DXTemp : IDirectDrawSurface;
  i : integer;
  nRect : TRect;
  pr : TRect;
const
  FailName : string = 'TLoadGame.ShowScreen ';
begin

  Log.DebugLog( FailName );
  try

    if ( DeleteBoxVisible = false ) and ( OverwriteBoxVisible = false ) then
    begin
      //clear menu
      pr := Rect( 368, 52, 671, 386 );
      lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
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
            pr := Rect( nRect.left - 10, nRect.top - 5, nRect.right, nRect.bottom - 5 );
            DrawAlpha( lpDDSBack, pr, rect( 0, 0, 12, 12 ), DXBackHighlight, False, 40 );
          end;
          if loadfile then
          begin
          //Bei Speichern fehler, sobald eine Datei angeklickt wird, siehe Siege.log
          //Mein Gedanke: Überschreiben des Bildes von Writetext mit dem alten Plottext verursacht Fehler
          pText.WriteText(pItem( SelectRect.items[ i ] ).text, pItem( SelectRect.items[ i ] ).rect.left, pItem( SelectRect.items[ i ] ).rect.top, 14);
          //pText.PlotText( pItem( SelectRect.items[ i ] ).text, pItem( SelectRect.items[ i ] ).rect.left, pItem( SelectRect.items[ i ] ).rect.top, 240 );
          pText.WriteText(pItem( SelectRect.items[ i ] ).date, 590 + Offset.X, pItem( SelectRect.items[ i ] ).rect.top, 10);
          //pText.PlotText( pItem( SelectRect.items[ i ] ).date, 590 + Offset.X, pItem( SelectRect.items[ i ] ).rect.top, 240 );
          end
          else
          begin
          pText.PlotText( pItem( SelectRect.items[ i ] ).text, pItem( SelectRect.items[ i ] ).rect.left, pItem( SelectRect.items[ i ] ).rect.top, 240 );
          pText.PlotText( pItem( SelectRect.items[ i ] ).date, 590 + Offset.X, pItem( SelectRect.items[ i ] ).rect.top, 240 );
          end;
        end;
      end; //end for
    end; //endif

    if CurrentSelectedListItem > -1 then
      PicName := GamesPath + pItem( SelectRect.items[ CurrentSelectedListItem ] ).text + '.bmp'
    else
      PicName := '';

    if ( PicName <> '' ) and TFile.Exists( PicName ) then
    begin
      DXTemp := SoAOS_DX_LoadBMP( PicName, cInvisColor );
      pr := Rect( 0, 0, 225, 162 );
      lpDDSBack.BltFast( 114 + Offset.X, 257 + Offset.Y, DXTemp, @pr, DDBLTFAST_WAIT );
      DXTemp := nil;
    end
    else
    begin
      pr := Rect( 114, 257, 340, 420 );
      lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );
    end; //endif

    SoAOS_DX_BltFront;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end; //TLoadGame.ShowScreen;


procedure TLoadGame.ShowInfo;
var
  i : integer;
  pr : TRect;
const
  FailName : string = 'TLoadGame.Showinfo ';
begin

  Log.DebugLog( FailName );
  try
    pr := Rect( 111, 65, 344, 231 );
    lpDDSBack.BltFast( pr.Left + Offset.X, pr.Top + Offset.Y, DXBack, @pr, DDBLTFAST_WAIT );

    PlotTextBlock( MapName, 123, 340, 70, 240, False, False );

    for i := 1 to CharacterCount do
    begin
      PlotText( CharacterName[ i ], 133, 70 + i * 25, 240 );
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

  Log.DebugLog( FailName );
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

  Log.DebugLog( FailName );
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
  Log.DebugLog( FailName );
  try

    if assigned( CaratTimer ) then
    begin
      CaratTimer.enabled := false;
      CaratTimer.free;
      CaratTimer := nil;
    end;

    frmMain.OnMouseWheel := nil;
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
  S : AnsiString;
  Stream : TFileStream;
  EOB, BB : word;
  P, L : LongWord;
  Block : TSavBlocks;
  Filename : string;
  FoundCharacters : boolean;
const
  FailName : string = 'TLoadGame.LoadGame ';
begin
  Log.DebugLog( FailName );
  result := false;
  if GameName<>'' then
  begin
    try
      EOB := EOBMarker;
      CharacterCount := 0;
      TravelBlock := '';
      FoundCharacters := false;
      try
        Filename := GamesPath + GameName + '.idx';
        if not TFile.Exists( Filename ) then
          Filename := GamesPath + GameName + '.sav';
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
                    inc( CharacterCount );
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
              Stream.Position := P + L; // Seek( P + L, soFromBeginning );
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
      Result := True;
    except
      on E : Exception do
        Log.log( FailName, E.Message, [ ] );
    end;
  end;
end;

procedure TLoadGame.MouseUp( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer );
begin
  inherited;

  ScrollState := 0;
end;

procedure TLoadGame.MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;

  if WheelDelta<0 then
  begin //up arrow
    if StartFile > 0 then
    begin
      StartFile := StartFile - 1;
      MoveList;
      ShowScreen;
      ScrollState := -3;
    end;
  end
  else if WheelDelta>0 then
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
end;

end.
