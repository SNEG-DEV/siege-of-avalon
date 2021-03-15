unit Display;
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
  System.SysUtils,
  System.Classes,
  System.Types,
  Vcl.Controls,
  Gametext,
  Engine,
  LogFile;

type
  TDisplay = class( TObject )
  private
    FOnClose : TNotifyEvent;
  protected
    WasActive : Boolean;
    InBound : Boolean;
    procedure MouseDown( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer ); virtual;
    procedure MouseMove( Sender : TObject;
      Shift : TShiftState; X, Y, GridX, GridY : Integer ); virtual;
    procedure MouseUp( Sender : TObject; Button : TMouseButton;
  Shift : TShiftState; X, Y, GridX, GridY : Integer ); virtual;
    procedure KeyDown( Sender : TObject; var key : Word;
      Shift : TShiftState ); virtual;
    procedure Close; virtual;
  public
    X1, Y1, X2, Y2 : Integer;
    pText : TGameText; //pointer to GameText object - caller must set this before init is called
    Loaded : Boolean;
    OldKeyDown : TKeyEvent;
    constructor Create;
    destructor Destroy; override;
    procedure Paint; virtual;
    procedure Init; virtual;
    procedure Release; virtual;
    property OnClose : TNotifyEvent read FOnClose write FOnClose;
    procedure DebugPlot( i : integer );
  end;

procedure DebugPrint( S : string );

implementation

uses
  AniDemo;

{ TDisplay }

procedure TDisplay.Close;
const
  FailName : string = 'TDisplay.Close';
begin
  Log.DebugLog(FailName);
  try

    if assigned( FOnClose ) then
      FOnClose( self );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

constructor TDisplay.Create;
const
  FailName : string = 'TDisplay.Create';
begin
  Log.DebugLog(FailName);
  try

    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

destructor TDisplay.Destroy;
const
  FailName : string = 'TDisplay.Destroy';
begin
  Log.DebugLog(FailName);
  try

    if Loaded then
      Release;
    inherited;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TDisplay.Init;
const
  FailName : string = 'TDisplay.init';
begin
  Log.DebugLog(FailName);
  try

    Loaded := True;
    Game.OnMouseDown := MouseDown;
    Game.OnMouseMove := MouseMove;
    Game.OnMouseUp := MouseUp;
    frmMain.OnKeyDown := KeyDown;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TDisplay.MouseDown( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer );
const
  FailName : string = 'TDisplay.Mousedown';
begin
  try
    InBound := ( X >= X1 ) and ( X < X2 ) and ( Y >= Y1 ) and ( Y < Y2 );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TDisplay.MouseMove( Sender : TObject;
      Shift : TShiftState; X, Y, GridX, GridY : Integer );
const
  FailName : string = 'TDisplay.MouseMove';
begin
  try
    InBound := ( X >= X1 ) and ( X < X2 ) and ( Y >= Y1 ) and ( Y < Y2 );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TDisplay.MouseUp( Sender : TObject; Button : TMouseButton;
  Shift : TShiftState; X, Y, GridX, GridY : Integer );
const
  FailName : string = 'TDisplay.MouseUp';
begin
  try
    InBound := ( X >= X1 ) and ( X < X2 ) and ( Y >= Y1 ) and ( Y < Y2 );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TDisplay.Paint;
begin

end;

procedure TDisplay.Release;
const
  FailName : string = 'TDisplay.Release';
begin
  Log.DebugLog(FailName);
  try

    OldKeyDown := nil;
    Loaded := False;
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure DebugPrint( S : string );
var
  F : TextFile;
begin

  AssignFile( F, ExtractFilePath( ParamStr(0) ) + '\mylog.txt' );
  //Reset(F);
  //Readln(F, S);
  Append( F ); //, extractfilepath(application.exename) + '\mylog.txt' );
  //Writeln(F);
  Write( F, S );
  WriteLn( F );
  CloseFile( F );

end;

procedure TDisplay.DebugPlot( i : integer );
var
  A : string;
const
  FailName : string = 'TDisplay.';
begin
  Log.DebugLog(FailName);
  try

    A := IntToStr( i );
    pText.PlotText( PChar( A ), 100, 300, 0 );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

procedure TDisplay.KeyDown( Sender : TObject; var key : Word;
  Shift : TShiftState );
const
  FailName : string = 'TDisplay.KeyDown';
begin
  Log.DebugLog(FailName);
  try
    if assigned( OldKeyDown ) then
      OldKeyDown( Sender, key, Shift );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

end.
