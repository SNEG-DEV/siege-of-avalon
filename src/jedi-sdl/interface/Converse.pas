unit Converse;
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
{$IFDEF DX5}
  DirectX,
{$ELSE}
  DirectDraw,
{$ENDIF}
  DXUtil,
  DXEffects,
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
  Display,
  Anigrp30,
  Engine,
  LogFile,
  INIFiles,
  strFunctions;

type
  TTextRect = class( TObject )
  public
    Rect : TRect;
    Text : string;
  end;

  TConverseBox = class( TDisplay )
  private
    Responses : TList;
    HLText : Integer;
    ReEntry : Boolean;
    bTraining : boolean;
    Caption : string;
    Section : string;

    INI : TMemINIFile;
    procedure LoadConversation;
  protected
    Image : IDirectDrawSurface;
    procedure MouseDown( Sender : TAniView; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer ); override;
    procedure MouseMove( Sender : TAniView;
      Shift : TShiftState; X, Y : Integer; MapX, mapY : Integer ); override;
    procedure MouseUp( Sender : TAniView; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer ); override;
  public
    ObjectRef : TGameObject;
    Conversation : string;
    slResponse : TStringList;

    constructor Create;
    destructor Destroy; override;
    procedure Paint; override;
    procedure Init; override;
    procedure Release; override;
  end;

function ParseStat( sTmp : string ) : string;
function ParseChapter( sTmp : string ) : string;
function ParseTitle( sTmp : string ) : string;
function ParseObject( sTmp : string ) : string;
function ParseWorn( sTmp : string ) : string;
function ParseParty( sTmp : string ) : string;
function CheckTitleAll( ObjRef : TGameObject; sTmp : string ) : boolean;
function CheckChapterAll( sTmp : string ) : boolean;
function CheckStatAll( sTmp : string ) : boolean;
function CheckObjectAll( sTmp : string ) : boolean;
function CheckWornAll( sTmp : string ) : boolean;
function CheckPartyAll( sTmp : string ) : boolean;
function CheckTitleOne( ObjRef : TGameObject; sTmp : string ) : boolean;
function CheckChapterOne( sTmp : string ) : boolean;
function CheckStatOne( sTmp : string ) : boolean;
function CheckObjectOne( sTmp : string ) : boolean;
function CheckWornOne( sTmp : string ) : boolean;
function CheckPartyOne( sTmp : string ) : boolean;

implementation

uses
  AniDemo,
  Resource;

{ TConverseBox }

constructor TConverseBox.Create;
const
  FailName : string = 'TConverseBox.Create';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
    Responses := TList.Create;
    slResponse := TStringList.Create;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

destructor TConverseBox.Destroy;
const
  FailName : string = 'TConverseBox.Destroy';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Responses.Free;
    Responses := nil;
    slResponse.Free;
    slResponse := nil;
    inherited;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TConverseBox.Init;
var
  Filename : string;
  BM : TBitmap;
  Shadow : IDirectDrawSurface;
const
  FailName : string = 'TConverseBox.Init';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;

    BM := TBitmap.Create;
    try
      BM.LoadFromFile( InterfacePath + 'DialogueBox.bmp' );
      Image := DDGetImage( lpDD, BM, clFuchsia, true );
      ReEntry := True;
      X1 := 65;
      Y1 := 40;
      X2 := X1 + BM.width;
      Y2 := Y1 + BM.Height;
    finally
      BM.free;
    end;
    HLText := -1;
    pText.LoadFontGraphic( 'Inventory' );

    Filename := Parse( Conversation, 0, '.' );
    Section := Parse( Conversation, 1, '.' );
    if ( Filename = '' ) then
    begin
      Close;
      exit;
    end;
    Filename := ArtPath + 'conversations/' + LanguagePath + Filename + '.cnv';
    INI := TMemINIFile.Create( Filename );

    LoadConversation;

  //DrawShadow
    BM := TBitmap.Create;
    try
      BM.LoadFromFile( InterfacePath + 'DialogueBoxshadowmap.bmp' );
      Shadow := DDGetImage( lpDD, BM, clFuchsia, false );
      try
        DrawSub( lpDDSBack, Rect( X1, Y1, X2, Y2 ), Rect( 0, 0, BM.width, BM.height ), Shadow, true, 170 );
      finally
        Shadow := nil;
      end;
    finally
      BM.free;
    end;
    MouseCursor.Cleanup;
    WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;
    pText.LoadTinyFontGraphic;

    Paint;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TConverseBox.MouseDown( Sender : TAniView; Button : TMouseButton;
  Shift : TShiftState; X, Y, GridX, GridY : Integer );
var
  strResponse : string;
const
  FailName : string = 'TConverseBox.MouseDown';
begin
  try
    inherited;
    if InBound then
    begin
      if Responses.count = 0 then
      begin
        Close;
        exit;
      end;
      if HLText >= 0 then
      begin
          //Response|NextLine|Script
        if HLText < slResponse.Count then
        begin
          strResponse := slResponse.Strings[ HLText ];

          if ( StrTokenAt( strResponse, '|', 2 ) = '' ) and ( StrTokenAt( strResponse, '|', 1 ) = '' ) then
          begin
            Close;
            exit;
          end;

          if StrTokenAt( strResponse, '|', 2 ) <> '' then
            RunScript( ObjectRef, StrTokenAt( strResponse, '|', 2 ) );

          if StrTokenAt( strResponse, '|', 1 ) <> '' then
          begin
            Section := StrTokenAt( strResponse, '|', 1 );
            LoadConversation;
            Paint;
          end
          else
            close;
        end
        else
          close;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TConverseBox.MouseMove( Sender : TAniView; Shift : TShiftState; X,
  Y, MapX, MapY : Integer );
var
  i : Integer;
const
  FailName : string = 'TConverseBox.MouseMove';
begin
  try
    inherited;
    if InBound then
    begin
      for i := 0 to Responses.Count - 1 do
      begin
        with TTextRect( Responses.Items[ i ] ) do
        begin
          if ( X >= Rect.Left ) and ( X < Rect.Right ) and ( Y >= Rect.Top ) and ( Y < Rect.Bottom ) then
          begin
            if i <> HLText then
            begin
              HLText := i;
              Paint;
            end;
            Exit;
          end;
        end;
      end;
    end;
    if HLText >= 0 then
    begin
      HLText := -1;
      Paint;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TConverseBox.MouseUp( Sender : TAniView; Button : TMouseButton;
  Shift : TShiftState; X, Y, GridX, GridY : Integer );
begin

end;

{$HINTS OFF -- Jim Shiflett}

procedure TConverseBox.LoadConversation;
var
  NewText : TTextRect;
  strCrntHeading : string;
  tmpStr, strTmp, S : string;
  strAll, StrOne : string;
  bAllStat, bAllParty, bAllTitle, bAllObj, bAllWrn, bAllChp : boolean;
  bOneStat, bOneParty, bOneTitle, bOneObj, bOneWrn, bOneChp : boolean;
  bOne : boolean;
  iLoop, jLoop : integer;
  return : string;
  sorry : string;
  training : string;
  hail : string;


const
  FailName : string = 'TConverseBox.LoadConversation';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    ExText.Open( 'converse' );
    Return := ExText.GetText( 'Return' );
    sorry := ExText.GetText( 'sorry' );
    training := ExText.GetText( 'training' );
    hail := ExText.GetText( 'hail' );
    ExtExt.Close;
    if Return = '' then
      return := 'Return';
    if sorry = '' then
      sorry := 'Sorry to have bothered you.';
    if training = '' then
      training := 'Current Training Point(s)';
    if hail = '' then
      hail := 'Hail and well met!';
    bTraining := false;

    if Section = '' then
    begin
      Section := Ini.ReadString( 'root', 'Else', '' );

      if LowerCase( ini.ReadString( 'root', 'random', 'false' ) ) = 'true' then
        Section := Ini.ReadString( 'root', 'goto' + IntToStr( Random( Ini.ReadInteger( 'root', 'count', 0 ) ) + 1 ), '' )
      else
          //get the section
        for iLoop := 1 to Ini.ReadInteger( 'root', 'count', 0 ) do
        begin
          bAllTitle := true;
          bAllStat := true;
          bAllObj := true;
          bAllWrn := true;
          bAllChp := true;
          bAllParty := true;
{**************these must be reset with each loop do not remove them*********}
          bOneTitle := false;
          bOneStat := false;
          bOneObj := false;
          bOneWrn := false;
          bOneChp := true;
          bOneParty := false;
{**************these must be reset with each loop do not remove them*********}

          bOne := true;
          tmpStr := '';
          StrAll := '';
          StrOne := '';

          tmpstr := Ini.ReadString( 'root', 'say' + IntToStr( iLoop ), '' );

          if LowerCase( strLeft( tmpstr, 3 ) ) = 'all' then
          begin
            strAll := StrTokenAt( tmpStr, ';', 0 );
            strOne := StrTokenAt( tmpStr, ';', 1 );
          end
          else if LowerCase( strLeft( tmpstr, 3 ) ) = 'one' then
            strOne := StrTokenAt( tmpStr, ';', 0 );


          if StrAll <> '' then
          begin
            bALlTitle := CheckTitleAll( ObjectRef, ParseTitle( strAll ) );
            bAllStat := CheckStatAll( ParseStat( strAll ) );
            bAllObj := CheckObjectAll( ParseObject( strAll ) );
            bAllWrn := CheckWornAll( ParseWorn( strAll ) );
            bAllChp := CheckChapterAll( ParseChapter( strAll ) );
            bAllParty := CheckPartyAll( ParseParty( strAll ) );
          end;

          if StrOne <> '' then
          begin
            bOneTitle := CheckTitleOne( ObjectRef, ParseTitle( strOne ) );
            bOneStat := CheckStatOne( ParseStat( strOne ) );
            bOneObj := CheckObjectOne( ParseObject( strOne ) );
            bOneWrn := CheckWornOne( ParseWorn( strOne ) );
            bOneChp := CheckChapterOne( ParseChapter( strOne ) );
            bOneParty := CheckPartyOne( ParseParty( strOne ) );

            if not ( bOneTitle ) and not ( bOneWrn ) and not ( bOneStat ) and not ( bOneObj ) and not ( bOneChp ) and not ( bOneParty ) then
              bOne := false;


          end;

          if bAllTitle and bAllParty and bAllStat and bAllObj and bAllWrn and bAllChp and bOne then
          begin
            Section := Ini.ReadString( 'root', 'goto' + IntToStr( iLoop ), '' );
            break;
          end;
        end;

    end;
    if Section <> '' then
    begin
      strCrntHeading := Section;
         //Say Text
      if ( Tcharacter( ObjectRef ) <> current ) and ( Tcharacter( ObjectRef ).properties[ 'charactername' ] <> '' ) then
        Caption := Tcharacter( ObjectRef ).properties[ 'charactername' ] + ':  ' + Ini.ReadString( strCrntHeading, 'Say', '' )
      else
        Caption := Ini.ReadString( strCrntHeading, 'Say', '' );


      if Ini.ReadString( strCrntHeading, 'train', '' ) = 'true' then
      begin
        Caption := Caption + #13#13 + training + ' = ' + IntToStr( Player.trainingpoints );
        bTraining := true;
      end;
    end
    else
      Caption := Tcharacter( ObjectRef ).properties[ 'charactername' ] + ':  ' + hail;


    caption := StringReplace( caption, '%playername%', player.name, [ rfReplaceAll, rfIgnoreCase ] );

    //Say Script
    if Ini.ReadString( strCrntHeading, 'script', '' ) <> '' then
      RunScript( ObjectRef, Ini.ReadString( strCrntHeading, 'script', '' ) );

    if Ini.ReadString( strCrntHeading, 'special', '' ) <> '' then
    begin
      strTmp := StrTokenAt( Ini.ReadString( strCrntHeading, 'special', '' ), '|', 0 );
      for jLoop := 0 to NPCList.count - 1 do
      begin
        if TCharacter( NPCList.Items[ jLoop ] ).Guid = strTmp then
        begin
          RunScript( TCharacter( NPCList.Items[ jLoop ] ), StrTokenAt( Ini.ReadString( strCrntHeading, 'special', '' ), '|', 1 ) );
        end;
      end;

    end;


    if Ini.ReadString( strCrntHeading, 'addquest', '' ) <> '' then
    begin
      for iLoop := 0 to StrTokenCount( Ini.ReadString( strCrntHeading, 'addquest', '' ), ';' ) - 1 do
        RunScript( ObjectRef, 'addquest(' + strTokenAt( Ini.ReadString( strCrntHeading, 'addquest', '' ), ';', iLoop ) + ')' );
        //RunScript(ObjectRef, '#showmessage.quest#');
    end;
    if Ini.ReadString( strCrntHeading, 'quest', '' ) <> '' then
    begin
      for iLoop := 0 to StrTokenCount( Ini.ReadString( strCrntHeading, 'quest', '' ), ';' ) - 1 do
        RunScript( ObjectRef, 'addquest(' + strTokenAt( Ini.ReadString( strCrntHeading, 'quest', '' ), ';', iLoop ) + ')' );
        //RunScript(ObjectRef, '#showmessage.quest#');
    end;

    if Ini.ReadString( strCrntHeading, 'adventure', '' ) <> '' then
    begin
      for iLoop := 0 to StrTokenCount( Ini.ReadString( strCrntHeading, 'adventure', '' ), ';' ) - 1 do
        RunScript( ObjectRef, 'Adventure(' + strTokenAt( Ini.ReadString( strCrntHeading, 'Adventure', '' ), ';', iLoop ) + ')' );
        //RunScript(ObjectRef, '#Showmessage.adventure#');
    end;


    if Ini.ReadString( strCrntHeading, 'removequest', '' ) <> '' then
      for iLoop := 0 to StrTokenCount( Ini.ReadString( strCrntHeading, 'removequest', '' ), ';' ) - 1 do
        RunScript( ObjectRef, 'removequest(' + strTokenAt( Ini.ReadString( strCrntHeading, 'removequest', '' ), ';', iLoop ) + ')' );


    for iLoop := 0 to Responses.count - 1 do
    begin
      TTextRect( Responses.items[ iLoop ] ).free;
    end;
    Responses.clear;

    slResponse.Clear;

(*****************************************************************)

    iLoop := 1;
    S := INI.ReadString( strCrntHeading, 'RspTxt' + IntToStr( iLoop ), '' );
    while S <> '' do
    begin
      bAllTitle := true;
      bAllStat := true;
      bAllObj := true;
      bAllWrn := true;
      bAllChp := true;
      bAllParty := true;
{**************these must be reset with each loop do not remove them*********}
      bOneTitle := false;
      bOneStat := false;
      bOneObj := false;
      bOneWrn := false;
      bOneChp := false;
      bOneParty := false;
      bOne := true;
{**************these must be reset with each loop do not remove them*********}

      tmpStr := '';
      StrAll := '';
      StrOne := '';
      tmpstr := Ini.ReadString( strCrntHeading, 'RspCnd' + IntToStr( iLoop ), '' );
      if LowerCase( strLeft( tmpstr, 3 ) ) = 'all' then
      begin
        strAll := StrTokenAt( tmpStr, ';', 0 );
        strOne := StrTokenAt( tmpStr, ';', 1 );
      end
      else if LowerCase( strLeft( tmpstr, 3 ) ) = 'one' then
        strOne := StrTokenAt( tmpStr, ';', 0 );

      if StrAll <> '' then
      begin
        bAllTitle := CheckTitleAll( ObjectRef, ParseTitle( strAll ) );
        bAllStat := CheckStatAll( ParseStat( strAll ) );
        bAllObj := CheckObjectAll( ParseObject( strAll ) );
        bAllWrn := CheckWornAll( ParseWorn( strAll ) );
        bAllChp := CheckChapterAll( ParseChapter( strAll ) );
        bAllParty := CheckPartyAll( ParseParty( strAll ) );
      end;
      if StrOne <> '' then
      begin
        bOneTitle := CheckTitleOne( ObjectRef, ParseTitle( strOne ) );
        bOneStat := CheckStatOne( ParseStat( strOne ) );
        bOneObj := CheckObjectOne( ParseObject( strOne ) );
        bOneWrn := checkWornOne( ParseWorn( StrOne ) );
        bOneChp := checkChapterOne( ParseChapter( strOne ) );
        bOneParty := checkPartyOne( ParseParty( strOne ) );

        if not ( bOneTitle ) and not ( bOneStat ) and not ( bOneObj ) and not ( bOneWrn ) and not ( bOneChp ) and not ( bOneParty ) then
          bOne := false;

      end;

      if bAllTitle and bAllParty and bAllStat and bAllObj and bAllWrn and bAllChp and bOne then
      begin
        NewText := TTextRect.Create;

        s := StringReplace( s, '%charactername%', Tcharacter( ObjectRef ).properties[ 'charactername' ], [ rfReplaceAll, rfIgnoreCase ] );
        s := StringReplace( s, '%playername%', player.name, [ rfReplaceAll, rfIgnoreCase ] );

        NewText.Text := S;
        Responses.Add( NewText );
        slResponse.Add( Ini.readString( strCrntHeading, 'RspTxt' + IntToStr( iLoop ), '' ) + '|' +
          Ini.readString( strCrntHeading, 'NxtLn' + IntToStr( iLoop ), '' ) + '|' +
          Ini.readString( strCrntHeading, 'Script' + IntToStr( iLoop ), '' ) );
      end;
      Inc( iLoop );
      S := INI.ReadString( Section, 'RspTxt' + IntToStr( iLoop ), '' );
    end;

    if LowerCase( Ini.ReadString( Section, 'RspDflt', '' ) ) <> 'none' then
    begin
      NewText := TTextRect.Create;
      if Ini.ReadString( Section, 'RspDflt', '' ) <> '' then
        NewText.Text := Ini.ReadString( Section, 'RspDflt', '' )
      else if Ini.ReadString( 'root', 'Default', '' ) <> '' then
        NewText.Text := Ini.ReadString( 'root', 'Default', '' )
      else
        NewText.Text := sorry;

      NewText.Text := StringReplace( NewText.Text, '%charactername%', Tcharacter( ObjectRef ).properties[ 'charactername' ], [ rfReplaceAll, rfIgnoreCase ] );
      NewText.Text := StringReplace( NewText.Text, '%playername%', player.name, [ rfReplaceAll, rfIgnoreCase ] );

      Responses.Add( NewText );
      slResponse.Add( sorry + '||' );

    end;

    if ( LowerCase( Ini.ReadString( Section, 'RspBack', '' ) ) <> 'none' ) and ( LowerCase( Ini.ReadString( Section, 'RspBack', '' ) ) <> '' ) then
    begin
      NewText := TTextRect.Create;
      NewText.Text := '[' + return + ']';
      Responses.Add( NewText );
      slResponse.Add( '[' + return + ']' + '|' + ini.readString( strCrntHeading, 'RspBack', '' ) );
    end;


  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;
{$HINTS ON -- Jim Shiflett}

procedure TConverseBox.Paint;
var
  S : string;
  R : TRect;
  i, j : Integer;
  W, H : integer;
const
  FailName : string = 'TConverseBox.Paint';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    j := Y1 + 30;
    if assigned( Image ) then
    begin
      GetSurfaceDims( W, H, Image );
      WrapperBltFast( lpDDSBack, X1, Y1, Image, Rect( 0, 0, W, H ), DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
    end
    else
    begin
      W := 420;
      H := 340;
    end;
    if Caption <> '' then
    begin
      R.Left := X1 + 30;
      R.Top := j;
      R.Right := X1 + W - 30;
      if UseSmallFont then
        j := j + 18 * pText.PlotTinyTextBlock( Caption, R.Left, R.Right, R.Top, 240 ) + 10
      else
        j := j + 22 * pText.PlotTextBlock( Caption, R.Left, R.Right, R.Top, 240 ) + 10;
      R.Bottom := j;

      if bTraining then
        inc( j, 50 )
      else
        inc( j, 20 );
    end;
    for i := 0 to Responses.count - 1 do
    begin
      S := TTextRect( Responses.items[ i ] ).Text;
      R.Left := X1 + 40;
      R.Top := j;
      R.Right := X1 + W - 30;
      if i = HLText then
      begin
        if UseSmallFont then
          j := j + 18 * pText.PlotTinyTextBlock( S, R.Left, R.Right, R.Top, 240 )
        else
          j := j + 22 * pText.PlotTextBlock( S, R.Left, R.Right, R.Top, 240 );
      end
      else
      begin
        if UseSmallFont then
          j := j + 18 * pText.PlotTinyTextBlock( S, R.Left, R.Right, R.Top, 120 )
        else
          j := j + 22 * pText.PlotTextBlock( S, R.Left, R.Right, R.Top, 120 );
      end;
      R.Bottom := j;
      TTextRect( Responses.items[ i ] ).Rect := R;
      Inc( j, 10 );
    end;
    lpDDSFront.Flip( nil, DDFLIP_WAIT );
    WrapperBltFast( lpDDSBack, 0, 0, lpDDSFront, Rect( 0, 0, 800, 600 ), DDBLTFAST_WAIT );
    MouseCursor.PlotDirty := false;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TConverseBox.Release;
var
  i : Integer;
const
  FailName : string = 'TConverseBox.Release';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    inherited;
    INI.Free;
    INI := nil;
    Image := nil;
    Image := nil;
    for i := 0 to Responses.Count - 1 do
    begin
      TTextRect( Responses.Items[ i ] ).Free;
    end;
    Responses.Clear;
    slResponse.Clear;
    pText.UnloadTinyFontGraphic;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;


function ParseStat( sTmp : string ) : string;
var
  iLoop : integer;
const
  FailName : string = 'Converse.ParseStat';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Result := '';
    if sTmp = '' then
      exit;

    Delete( sTmp, 1, 4 );
    strStriplast( sTmp );
    for iLoop := 0 to 2 do
    begin
      if LowerCase( strLeft( StrTokenAt( sTmp, ':', iLoop ), 3 ) ) = 'stt' then
      begin //List of Stats
        Result := StrTokenAt( sTmp, ':', iLoop );
        Delete( Result, 1, 4 );
        strStriplast( Result );
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function ParseTitle( sTmp : string ) : string;
var
  iLoop : integer;
const
  FailName : string = 'Converse.ParseTitle';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Result := '';
    if sTmp = '' then
      exit;

    Delete( sTmp, 1, 4 );
    strStriplast( sTmp );
    for iLoop := 0 to 2 do
    begin
      if LowerCase( strLeft( StrTokenAt( sTmp, ':', iLoop ), 3 ) ) = 'ttl' then
      begin //List of titles
        Result := StrTokenAt( sTmp, ':', iLoop );
        Delete( Result, 1, 4 );
        strStriplast( Result );
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function ParseChapter( sTmp : string ) : string;
var
  iLoop : integer;
const
  FailName : string = 'Converse.ParseChapter';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Result := '';
    if sTmp = '' then
      exit;

    Delete( sTmp, 1, 4 );
    strStriplast( sTmp );
    for iLoop := 0 to 2 do
    begin
      if LowerCase( strLeft( StrTokenAt( sTmp, ':', iLoop ), 3 ) ) = 'chp' then
      begin //List of titles
        Result := StrTokenAt( sTmp, ':', iLoop );
        Delete( Result, 1, 4 );
        strStriplast( Result );
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function ParseObject( sTmp : string ) : string;
var
  iLoop : integer;
const
  FailName : string = 'Converse.ParseObject';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Result := '';
    if sTmp = '' then
      exit;

    Delete( sTmp, 1, 4 );
    strStriplast( sTmp );

    for iLoop := 0 to 2 do
    begin
      if LowerCase( strLeft( StrTokenAt( sTmp, ':', iLoop ), 3 ) ) = 'obj' then
      begin //List of objects
        Result := StrTokenAt( sTmp, ':', iLoop );
        Delete( Result, 1, 4 );
        strStriplast( Result );
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function ParseWorn( sTmp : string ) : string;
var
  iLoop : integer;
const
  FailName : string = 'Converse.ParseWorn';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Result := '';
    if sTmp = '' then
      exit;

    Delete( sTmp, 1, 4 );
    strStriplast( sTmp );

    for iLoop := 0 to 2 do
    begin
      if LowerCase( strLeft( StrTokenAt( sTmp, ':', iLoop ), 3 ) ) = 'wrn' then
      begin //List of objects
        Result := StrTokenAt( sTmp, ':', iLoop );
        Delete( Result, 1, 4 );
        strStriplast( Result );
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function ParseParty( sTmp : string ) : string;
var
  iLoop : integer;
const
  FailName : string = 'Converse.ParseParty';
begin
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    Result := '';
    if sTmp = '' then
      exit;

    Delete( sTmp, 1, 4 );
    strStriplast( sTmp );

    for iLoop := 0 to 2 do
    begin
      if LowerCase( strLeft( StrTokenAt( sTmp, ':', iLoop ), 3 ) ) = 'pty' then
      begin //List of objects
        Result := StrTokenAt( sTmp, ':', iLoop );
        Delete( Result, 1, 4 );
        strStriplast( Result );
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function CheckPartyAll( sTmp : string ) : boolean;
var
  iLoop : integer;
  jLoop : integer;
  strTmp : string;
  b1 : boolean;
  b2 : boolean;
const
  FailName : string = 'Converse.CheckPartyAll';
begin
  Result := true;
  b1 := true;
  b2 := false;
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if sTmp = '' then
      exit;
    for iLoop := 0 to StrTokenCount( sTmp, ',' ) - 1 do
    begin
      strTmp := trim( StrTokenAt( sTmp, ',', iLoop ) );
      if strLeft( strTmp, 1 ) <> '!' then
      begin
        b1 := false;
        for jLoop := 0 to NPCList.count - 1 do
        begin
          if TCharacter( NPCList.Items[ jLoop ] ).Guid = strTmp then
            b1 := true;
        end;
        if not ( b1 ) then
          break;
      end
      else
      begin
        strStripFirst( strTmp ); //get rid of the '!'
        b2 := false;
        for jLoop := 0 to NPCList.count - 1 do
        begin
          if TCharacter( NPCList.Items[ jLoop ] ).Guid = strTmp then
            b2 := true;
        end;
        if b2 then
          break;
      end;
    end;
    if not ( b1 ) or b2 then
      Result := false;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function CheckTitleAll( ObjRef : TGameObject; sTmp : string ) : boolean;
var
  iLoop : integer;
  strTmp : string;
const
  FailName : string = 'Converse.CheckTitleAll';
begin
  Result := true;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if sTmp = '' then
      exit;

    for iLoop := 0 to StrTokenCount( sTmp, ',' ) - 1 do
    begin
      strTmp := trim( StrTokenAt( sTmp, ',', iLoop ) );
      if strLeft( strTmp, 1 ) <> '!' then
      begin
        if not ( Player.TitleExists( LowerCase( strTmp ) ) ) and not ( Tcharacter( ObjRef ).TitleExists( LowerCase( strTmp ) ) ) then //player does not have it but should
          Result := false;
      end
      else //Make sure player doesnt have title
      begin
        strStripFirst( strTmp ); //get rid of the '!'
        if ( Player.TitleExists( LowerCase( strTmp ) ) ) or ( Tcharacter( ObjRef ).TitleExists( LowerCase( strTmp ) ) ) then //player does have it but shouldnt
          Result := false;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function CheckChapterAll( sTmp : string ) : boolean;
var
  iLoop : integer;
  strTmp : string;
const
  FailName : string = 'Converse.CheckChapterAll';
begin
  Result := true;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if sTmp = '' then
      exit;

    for iLoop := 0 to StrTokenCount( sTmp, ',' ) - 1 do
    begin
      strTmp := trim( StrTokenAt( sTmp, ',', iLoop ) );
      if strLeft( strTmp, 1 ) <> '!' then
      begin
        if ( int64( 1 shl ( strtoint( strTmp ) - 1 ) ) and Chapters ) = 0 then
          Result := false;
      end
      else //Make sure player doesnt have title
      begin
        strStripFirst( strTmp ); //get rid of the '!'
        if ( int64( 1 shl ( strtoint( strTmp ) - 1 ) ) and Chapters ) <> 0 then
          Result := false;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function CheckStatAll( sTmp : string ) : boolean;
var
  iLoop : integer;
  strTmp : string;
const
  FailName : string = 'Converse.CheckStatAll';
begin
  Result := true;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if sTmp = '' then
      exit;
    for iLoop := 0 to 1 do
    begin
      strTmp := trim( strTokenAt( sTmp, ',', iLoop ) );
      if strTmp = '' then
        break;
      try
        if Pos( '>', strTmp ) <> 0 then
        begin
          if lowerCase( StrTokenAt( StrTmp, '>', 0 ) ) = 'charm' then
            if Current.Charm < StrToInt( StrTokenAt( strTmp, '>', 1 ) ) + 1 then
              Result := false;
        end
        else
        begin
          if lowerCase( StrTokenAt( StrTmp, '<', 0 ) ) = 'charm' then
            if Current.Charm > StrToInt( StrTokenAt( strTmp, '<', 1 ) ) - 1 then
              Result := false;
        end;

        if Pos( '>', strTmp ) <> 0 then
        begin
          if lowerCase( StrTokenAt( StrTmp, '>', 0 ) ) = 'combat' then
            if Current.Combat < StrToInt( StrTokenAt( strTmp, '>', 1 ) ) + 1 then
              Result := false;
        end
        else
        begin
          if lowerCase( StrTokenAt( StrTmp, '<', 0 ) ) = 'combat' then
            if Current.Combat > StrToInt( StrTokenAt( strTmp, '<', 1 ) ) - 1 then
              Result := false;
        end;

        if Pos( '>', strTmp ) <> 0 then
        begin
          if lowerCase( StrTokenAt( StrTmp, '>', 0 ) ) = 'constitution' then
            if Current.Constitution < StrToInt( StrTokenAt( strTmp, '>', 1 ) ) + 1 then
              Result := false;
        end
        else
        begin
          if lowerCase( StrTokenAt( StrTmp, '<', 0 ) ) = 'constitution' then
            if Current.Constitution > StrToInt( StrTokenAt( strTmp, '<', 1 ) ) - 1 then
              Result := false;
        end;

        if Pos( '>', strTmp ) <> 0 then
        begin
          if lowerCase( StrTokenAt( StrTmp, '>', 0 ) ) = 'coordination' then
            if Current.coordination < StrToInt( StrTokenAt( strTmp, '>', 1 ) ) + 1 then
              Result := false;
        end
        else
        begin
          if lowerCase( StrTokenAt( StrTmp, '<', 0 ) ) = 'coordination' then
            if Current.Coordination > StrToInt( StrTokenAt( strTmp, '<', 1 ) ) - 1 then
              Result := false;
        end;

        if Pos( '>', strTmp ) <> 0 then
        begin
          if lowerCase( StrTokenAt( StrTmp, '>', 0 ) ) = 'hitpoints' then
            if Current.Hitpoints < StrToInt( StrTokenAt( strTmp, '>', 1 ) ) + 1 then
              Result := false;
        end
        else
        begin
          if lowerCase( StrTokenAt( StrTmp, '<', 0 ) ) = 'hitpoints' then
            if Current.Hitpoints > StrToInt( StrTokenAt( strTmp, '<', 1 ) ) - 1 then
              Result := false;
        end;

        if Pos( '>', strTmp ) <> 0 then
        begin
          if lowerCase( StrTokenAt( StrTmp, '>', 0 ) ) = 'mana' then
            if Current.Mana < StrToInt( StrTokenAt( strTmp, '>', 1 ) ) + 1 then
              Result := false;
        end
        else
        begin
          if lowerCase( StrTokenAt( StrTmp, '<', 0 ) ) = 'mana' then
            if Current.Mana > StrToInt( StrTokenAt( strTmp, '<', 1 ) ) - 1 then
              Result := false;
        end;

        if Pos( '>', strTmp ) <> 0 then
        begin
          if lowerCase( StrTokenAt( StrTmp, '>', 0 ) ) = 'mysticism' then
            if Current.mysticism < StrToInt( StrTokenAt( strTmp, '>', 1 ) ) + 1 then
              Result := false;
        end
        else
        begin

          if lowerCase( StrTokenAt( StrTmp, '<', 0 ) ) = 'mysticism' then
            if Current.mysticism > StrToInt( StrTokenAt( strTmp, '<', 1 ) ) - 1 then
              Result := false;
        end;

        if Pos( '>', strTmp ) <> 0 then
        begin
          if lowerCase( StrTokenAt( StrTmp, '>', 0 ) ) = 'perception' then
            if Current.perception < StrToInt( StrTokenAt( strTmp, '>', 1 ) ) + 1 then
              Result := false;
        end
        else
        begin
          if lowerCase( StrTokenAt( StrTmp, '<', 0 ) ) = 'perception' then
            if Current.perception > StrToInt( StrTokenAt( strTmp, '<', 1 ) ) - 1 then
              Result := false;
        end;


        if Pos( '>', strTmp ) <> 0 then
        begin
          if lowerCase( StrTokenAt( StrTmp, '>', 0 ) ) = 'stealth' then
            if Current.stealth < StrToInt( StrTokenAt( strTmp, '>', 1 ) ) + 1 then
              Result := false;
        end
        else
        begin
          if lowerCase( StrTokenAt( StrTmp, '<', 0 ) ) = 'stealth' then
            if Current.stealth > StrToInt( StrTokenAt( strTmp, '<', 1 ) ) - 1 then
              Result := false;
        end;

        if Pos( '>', strTmp ) <> 0 then
        begin
          if lowerCase( StrTokenAt( StrTmp, '>', 0 ) ) = 'strenght' then
            if Current.strength < StrToInt( StrTokenAt( strTmp, '>', 1 ) ) + 1 then
              Result := false;
        end
        else
        begin
          if lowerCase( StrTokenAt( StrTmp, '<', 0 ) ) = 'strenght' then
            if Current.strength > StrToInt( StrTokenAt( strTmp, '<', 1 ) ) - 1 then
              Result := false;
        end;

        if Pos( '>', strTmp ) <> 0 then
        begin
          if lowerCase( StrTokenAt( StrTmp, '>', 0 ) ) = 'wounds' then
            if Current.wounds < StrToInt( StrTokenAt( strTmp, '>', 1 ) ) + 1 then
              Result := false;
        end
        else
        begin
          if lowerCase( StrTokenAt( StrTmp, '<', 0 ) ) = 'wounds' then
            if Current.wounds > StrToInt( StrTokenAt( strTmp, '<', 1 ) ) - 1 then
              Result := false;
        end;

        if Pos( '>', strTmp ) <> 0 then
        begin
          if lowerCase( StrTokenAt( StrTmp, '>', 0 ) ) = 'trainingpoints' then
            if Current.TrainingPoints < StrToInt( StrTokenAt( strTmp, '>', 1 ) ) + 1 then
              Result := false;
        end
        else
        begin
          if lowerCase( StrTokenAt( StrTmp, '<', 0 ) ) = 'trainingpoints' then
            if Current.TrainingPoints > StrToInt( StrTokenAt( strTmp, '<', 1 ) ) - 1 then
              Result := false;
        end;

      except
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function CheckObjectAll( sTmp : string ) : boolean;
var
  iLoop : integer;
  strTmp : string;
const
  FailName : string = 'Converse.CheckObjectAll';
begin
  Result := true;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if sTmp = '' then
      exit;

    for iLoop := 0 to strTokenCount( sTmp, ',' ) - 1 do
    begin
      strTmp := trim( StrTokenAt( sTmp, ',', iLoop ) );
      if strLeft( StrTmp, 1 ) <> '!' then
      begin
        if not ( Player.InInventory( LowerCase( StrTmp ) ) ) then //player does not have it but should
          Result := false;
      end
      else //Make sure player doesnt have title
      begin
        strStripFirst( StrTmp ); //get rid of the '!'
        if Player.InInventory( LowerCase( StrTmp ) ) then //player does have it but shouldnt
          Result := false;
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function CheckWornAll( sTmp : string ) : boolean;
var
  iLoop : integer;
  strTmp : string;
const
  FailName : string = 'Converse.CheckObjectAll';
begin
  Result := true;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if sTmp = '' then
      exit;

    for iLoop := 0 to strTokenCount( sTmp, ',' ) - 1 do
    begin
      strTmp := trim( StrTokenAt( sTmp, ',', iLoop ) );
      if strLeft( StrTmp, 1 ) <> '!' then
      begin
        if not ( Player.IsWorn( LowerCase( StrTmp ) ) ) then //player does not have it but should
          Result := false;
      end
      else //Make sure player doesnt have title
      begin
        strStripFirst( StrTmp ); //get rid of the '!'
        if Player.IsWorn( LowerCase( StrTmp ) ) then //player does have it but shouldnt
          Result := false;
      end;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function CheckPartyOne( sTmp : string ) : boolean;
var
  iLoop : integer;
  jLoop : integer;
  strTmp : string;
  b1 : boolean;
  b2 : boolean;

const
  FailName : string = 'Converse.CheckPartyOne';
begin
  Result := false;
  b1 := false;
  b2 := true;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if sTmp = '' then
      exit;
    for iLoop := 0 to StrTokenCount( sTmp, ',' ) - 1 do
    begin
      strTmp := trim( StrTokenAt( sTmp, ',', iLoop ) );
      if strLeft( strTmp, 1 ) <> '!' then
      begin
        b1 := false;
        for jLoop := 0 to NPCList.count - 1 do
        begin
          if TCharacter( NPCList.Items[ jLoop ] ).Guid = strTmp then
          begin
            b1 := true;
          end;
        end;
        if b1 then
          break

      end
      else
      begin
        strStripFirst( strTmp ); //get rid of the '!'
        b2 := false;
        for jLoop := 0 to NPCList.count - 1 do
        begin
          if TCharacter( NPCList.Items[ jLoop ] ).Guid = strTmp then
          begin
            b2 := true;
          end;
        end;
        if not ( b2 ) then
          break;
      end;
    end;
    if b1 or not ( b2 ) then
      Result := true;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;


function CheckTitleOne( ObjRef : TGameObject; sTmp : string ) : boolean;
var
  iLoop : integer;
  strTmp : string;
  // bTmp: boolean;
const
  FailName : string = 'Converse.CheckTitleOne';
begin
  Result := false;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if sTmp = '' then
      exit;

    for iLoop := 0 to StrTokenCount( sTmp, ',' ) - 1 do
    begin
      strTmp := trim( StrTokenAt( sTmp, ',', iLoop ) );
      if strLeft( strTmp, 1 ) <> '!' then
      begin
        if ( Player.TitleExists( strTmp ) ) or ( TCharacter( ObjRef ).TitleExists( strTmp ) ) then //player has it and should
          Result := true;
      end
      else //Make sure player doesnt have title
      begin
        strStripFirst( strTmp ); //get rid of the '!'
        if not ( Player.TitleExists( strTmp ) ) and not ( TCharacter( ObjRef ).TitleExists( strTmp ) ) then //player does have it and shouldnt
          Result := true;
      end;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function CheckChapterOne( sTmp : string ) : boolean;
var
  iLoop : integer;
  strTmp : string;
  bTmp : boolean;
const
  FailName : string = 'Converse.CheckChapterOne';
begin
  Result := false;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if sTmp = '' then
      exit;

    Result := false;

    bTmp := false;
    for iLoop := 0 to StrTokenCount( sTmp, ',' ) - 1 do
    begin
      strTmp := trim( StrTokenAt( sTmp, ',', iLoop ) );
      if strLeft( strTmp, 1 ) <> '!' then
      begin
        if ( int64( 1 shl ( strtoint( strTmp ) - 1 ) ) and Chapters ) <> 0 then
          bTmp := true;
      end
      else //Make sure the chapter doesnt exist
      begin
        strStripFirst( strTmp ); //get rid of the '!'
        if ( int64( 1 shl ( strtoint( strTmp ) - 1 ) ) and Chapters ) = 0 then
          bTmp := true;
      end;
      if bTmp then
        Result := true;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function CheckStatOne( sTmp : string ) : boolean;
var
  iLoop : integer;
  strTmp : string;
  bTmp : boolean;
const
  FailName : string = 'Converse.CheckStatOne';
begin
  Result := false;

{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if sTmp = '' then
      exit;

    Result := false;
    bTmp := false;
    for iLoop := 0 to 1 do
    begin
      strTmp := trim( strTokenAt( sTmp, ',', iLoop ) );
      if strTmp = '' then
        break;
      try
        if Pos( '>', strTmp ) <> 0 then
        begin
          if lowerCase( StrTokenAt( StrTmp, '>', 0 ) ) = 'charm' then
            if Current.Charm > StrToInt( StrTokenAt( strTmp, '>', 1 ) ) then
              bTmp := true;
        end
        else
        begin
          if lowerCase( StrTokenAt( StrTmp, '<', 0 ) ) = 'charm' then
            if Current.Charm < StrToInt( StrTokenAt( strTmp, '<', 1 ) ) then
              bTmp := true;
        end;

        if Pos( '>', strTmp ) <> 0 then
        begin
          if lowerCase( StrTokenAt( StrTmp, '>', 0 ) ) = 'combat' then
            if Current.Combat > StrToInt( StrTokenAt( strTmp, '>', 1 ) ) then
              bTmp := true;
        end
        else
        begin
          if lowerCase( StrTokenAt( StrTmp, '<', 0 ) ) = 'combat' then
            if Current.Combat < StrToInt( StrTokenAt( strTmp, '<', 1 ) ) then
              bTmp := true;
        end;

        if Pos( '>', strTmp ) <> 0 then
        begin
          if lowerCase( StrTokenAt( StrTmp, '>', 0 ) ) = 'constitution' then
            if Current.Constitution > StrToInt( StrTokenAt( strTmp, '>', 1 ) ) then
              bTmp := true;
        end
        else
        begin
          if lowerCase( StrTokenAt( StrTmp, '<', 0 ) ) = 'constitution' then
            if Current.Constitution < StrToInt( StrTokenAt( strTmp, '<', 1 ) ) then
              bTmp := true;
        end;

        if Pos( '>', strTmp ) <> 0 then
        begin
          if lowerCase( StrTokenAt( StrTmp, '>', 0 ) ) = 'coordination' then
            if Current.Coordination > StrToInt( StrTokenAt( strTmp, '>', 1 ) ) then
              bTmp := true;
        end
        else
        begin
          if lowerCase( StrTokenAt( StrTmp, '<', 0 ) ) = 'coordination' then
            if Current.Coordination < StrToInt( StrTokenAt( strTmp, '<', 1 ) ) then
              bTmp := true;
        end;

        if Pos( '>', strTmp ) <> 0 then
        begin
          if lowerCase( StrTokenAt( StrTmp, '>', 0 ) ) = 'hitpoints' then
            if Current.hitpoints > StrToInt( StrTokenAt( strTmp, '>', 1 ) ) then
              bTmp := true;
        end
        else
        begin
          if lowerCase( StrTokenAt( StrTmp, '<', 0 ) ) = 'hitpoints' then
            if Current.hitpoints < StrToInt( StrTokenAt( strTmp, '<', 1 ) ) then
              bTmp := true;
        end;

        if Pos( '>', strTmp ) <> 0 then
        begin
          if lowerCase( StrTokenAt( StrTmp, '>', 0 ) ) = 'mana' then
            if Current.mana > StrToInt( StrTokenAt( strTmp, '>', 1 ) ) then
              bTmp := true;
        end
        else
        begin
          if lowerCase( StrTokenAt( StrTmp, '<', 0 ) ) = 'mana' then
            if Current.mana < StrToInt( StrTokenAt( strTmp, '<', 1 ) ) then
              bTmp := true;
        end;

        if Pos( '>', strTmp ) <> 0 then
        begin
          if lowerCase( StrTokenAt( StrTmp, '>', 0 ) ) = 'mysticism' then
            if Current.mysticism > StrToInt( StrTokenAt( strTmp, '>', 1 ) ) then
              bTmp := true;
        end
        else
        begin
          if lowerCase( StrTokenAt( StrTmp, '<', 0 ) ) = 'mysticism' then
            if Current.mysticism < StrToInt( StrTokenAt( strTmp, '<', 1 ) ) then
              bTmp := true;
        end;

        if Pos( '>', strTmp ) <> 0 then
        begin
          if lowerCase( StrTokenAt( StrTmp, '>', 0 ) ) = 'perception' then
            if Current.perception > StrToInt( StrTokenAt( strTmp, '>', 1 ) ) then
              bTmp := true;
        end
        else
        begin
          if lowerCase( StrTokenAt( StrTmp, '<', 0 ) ) = 'perception' then
            if Current.perception < StrToInt( StrTokenAt( strTmp, '<', 1 ) ) then
              bTmp := true;
        end;


        if Pos( '>', strTmp ) <> 0 then
        begin
          if lowerCase( StrTokenAt( StrTmp, '>', 0 ) ) = 'stealth' then
            if Current.stealth > StrToInt( StrTokenAt( strTmp, '>', 1 ) ) then
              bTmp := true;
        end
        else
        begin
          if lowerCase( StrTokenAt( StrTmp, '<', 0 ) ) = 'stealth' then
            if Current.stealth < StrToInt( StrTokenAt( strTmp, '<', 1 ) ) then
              bTmp := true;
        end;

        if Pos( '>', strTmp ) <> 0 then
        begin
          if lowerCase( StrTokenAt( StrTmp, '>', 0 ) ) = 'strenght' then
            if Current.strength > StrToInt( StrTokenAt( strTmp, '>', 1 ) ) then
              bTmp := true;
        end
        else
        begin
          if lowerCase( StrTokenAt( StrTmp, '<', 0 ) ) = 'strenght' then
            if Current.strength < StrToInt( StrTokenAt( strTmp, '<', 1 ) ) then
              bTmp := true;
        end;
      except
      end;
      if bTmp then
        Result := true;
    end;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function CheckObjectOne( sTmp : string ) : boolean;
var
  iLoop : integer;
  strTmp : string;
  bTmp : boolean;
const
  FailName : string = 'Converse.CheckObjectOne';
begin
  Result := false;
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if sTmp = '' then
      exit;
    Result := false;
    bTmp := false;
    for iLoop := 0 to strTokenCount( sTmp, ',' ) - 1 do
    begin
      strTmp := trim( StrTokenAt( sTmp, ',', iLoop ) );
      if strLeft( StrTmp, 1 ) <> '!' then
      begin
        if Player.InInventory( StrTmp ) then //player does not have it but should
          bTmp := true;
      end
      else //Make sure player doesnt have title
      begin
        strStripFirst( StrTmp ); //get rid of the '!'
        if not ( Player.InInventory( StrTmp ) ) then //player does have it but shouldnt
          bTmp := true;
      end;
      if bTmp then
        Result := true;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

function CheckWornOne( sTmp : string ) : boolean;
var
  iLoop : integer;
  strTmp : string;
  bTmp : boolean;
const
  FailName : string = 'Converse.CheckObjectOne';
begin
  Result := false;
{$IFDEF DODEBUG}
  if ( CurrDbgLvl >= DbgLvlSevere ) then
    Log.LogEntry( FailName );
{$ENDIF}
  try
    if sTmp = '' then
      exit;
    Result := false;
    bTmp := false;
    for iLoop := 0 to strTokenCount( sTmp, ',' ) - 1 do
    begin
      strTmp := trim( StrTokenAt( sTmp, ',', iLoop ) );
      if strLeft( StrTmp, 1 ) <> '!' then
      begin
        if Player.IsWorn( StrTmp ) then //player does not have it but should
          bTmp := true;
      end
      else //Make sure player doesnt have title
      begin
        strStripFirst( StrTmp ); //get rid of the '!'
        if not ( Player.IsWorn( StrTmp ) ) then //player does have it but shouldnt
          bTmp := true;
      end;
      if bTmp then
        Result := true;
    end;

  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;


end.

