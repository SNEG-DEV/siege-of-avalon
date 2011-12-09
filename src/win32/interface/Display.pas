unit Display;
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
  SysUtils,
  Classes,
  Controls,
  Forms,
  Gametext,
  Anigrp30,
  Engine,
  INIFiles,
  LogFile;

type
  TExternalizer = class( TObject )
  private
    INI : TINIFile;
    FSection : string;
  public
    destructor Destroy; override;
    procedure Open( const Section : string );
    procedure Close;
    function GetText( const ID : string ) : string;
    procedure GetSection( Strings : TStrings );
  end;

  TDisplay = class( TObject )
  private
    FOnClose : TNotifyEvent;
  protected
    WasActive : Boolean;
    InBound : Boolean;
    procedure MouseDown( Sender : TAniview; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); virtual;
    procedure MouseMove( Sender : TAniview;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); virtual;
    procedure MouseUp( Sender : TAniview; Button : TMouseButton;
      Shift : TShiftState; X, Y : Integer; GridX, GridY : integer ); virtual;
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
const
  GroundListWidth : integer = 75;
  GroundListHeight : integer = 29;

var
  ExText : TExternalizer;

implementation

uses
  AniDemo;

{ TDisplay }

procedure TDisplay.Close;
const
  FailName : string = 'TDisplay.Close';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
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
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
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
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
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
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
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

procedure TDisplay.MouseDown( Sender : TAniview; Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer; GridX, GridY : integer );
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

procedure TDisplay.MouseMove( Sender : TAniview; Shift : TShiftState; X, Y : Integer; GridX, GridY : integer );
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

procedure TDisplay.MouseUp( Sender : TAniview; Button : TMouseButton;
  Shift : TShiftState; X, Y : Integer; GridX, GridY : integer );
const
  FailName : string = 'TDisplay.MosueUp';
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
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
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

  AssignFile( F, ExtractFilePath( Application.ExeName ) + '\mylog.txt' );
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
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
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
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if assigned( OldKeyDown ) then
      OldKeyDown( Sender, key, Shift );
  except
    on E : Exception do
      Log.log( FailName + E.Message );
  end;

end;

{ TExternalizer }

procedure TExternalizer.Close;
begin
  if assigned( INI ) then
  begin
    INI.free;
    INI := nil;
  end;
end;

destructor TExternalizer.Destroy;
begin
  INI.free;
  inherited;
end;

procedure TExternalizer.GetSection( Strings : TStrings );
begin
  INI.ReadSectionValues( FSection, Strings );
end;

function TExternalizer.GetText( const ID : string ) : string;
begin
  if assigned( INI ) then
    result := INI.ReadString( FSection, ID, '' )
  else
    result := '';
end;

procedure TExternalizer.Open( const Section : string );
begin
  FSection := Section;
  if not assigned( INI ) then
    INI := TIniFile.create( InterfacePath + 'text.ini' );
end;

initialization
  begin
    ExText := TExternalizer.create;
  end;

finalization
  begin
    ExText.free;
  end;


end.
