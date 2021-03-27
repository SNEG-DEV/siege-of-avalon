unit Converse;
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
//  Winapi.DirectDraw,
  DirectX,
  System.Types,
  System.Classes,
  System.Generics.Collections,
  Vcl.Controls,
  Character,
  SoAOS.Intrface.Dialogs,
  SoAOS.Animation,
  System.IniFiles,
  SoAOS.StrUtils;

type
  TTextRect = class( TObject )
  public
    Rect : TRect;
    Text : string;
  end;

  TConverseBox = class( TDialog )
  private
    Responses : TList<TTextRect>;
    HLText : Integer;
    ReEntry : Boolean;
    bTraining : boolean;
    Caption : string;
    Section : string;

    INI : TMemINIFile;
    procedure LoadConversation;
  protected
    Image : IDirectDrawSurface;
    procedure MouseDown( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer ); override;
    procedure MouseMove( Sender : TObject;
      Shift : TShiftState; X, Y, MapX, MapY : Integer ); override;
    procedure MouseUp( Sender : TObject; Button : TMouseButton;
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

function checkStat(s: TTokenString; statStr: string; curPropVal: Integer) : boolean; overload;
function checkStat(s: TTokenString; statStr: string; curPropVal: Single) : boolean; overload;
function checkStat(s: TTokenString; statStr: string; curPropVal: Double) : boolean; overload;

function checkStat1(s: TTokenString; statStr: string; curPropVal: Integer) : boolean; overload;
function checkStat1(s: TTokenString; statStr: string; curPropVal: Single) : boolean; overload;
function checkStat1(s: TTokenString; statStr: string; curPropVal: Double) : boolean; overload;

implementation

uses
  System.SysUtils,
  DXUtil,
  DXEffects,
  SoAOS.Types,
  SoAOS.Intrface.Text,
  SoAOS.Graphics.Draw,
  Engine,
  LogFile,
  AniDemo,
  Resource;

{ TConverseBox }

constructor TConverseBox.Create;
const
  FailName : string = 'TConverseBox.Create';
begin
  Log.DebugLog( FailName );
  try
    inherited;
    Responses := TList<TTextRect>.Create;
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
  Log.DebugLog( FailName );
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
  width, height : Integer;
  Filename : string;
  Shadow : IDirectDrawSurface;
const
  FailName : string = 'TConverseBox.Init';
begin
  Log.DebugLog( FailName );
  try
    inherited;

    Image := SoAOS_DX_LoadBMP( InterfacePath + 'DialogueBox.bmp', cTransparent, DlgWidth, DlgHeight );
    ReEntry := True;

    if ScreenMetrics.ScreenWidth>800 then
    begin
      X1 := 0;
      Y1 := 0;
      X2 := DlgWidth;
      Y2 := DlgHeight;
    end
    else
    begin
      X1 := 65;
      Y1 := 40;
      X2 := X1 + DlgWidth;
      Y2 := Y1 + DlgHeight;
    end;

    HLText := -1;
    pText.LoadFontGraphic( 'Inventory' );

    Filename := Parse( AnsiString ( Conversation ), 0, '.' );
    Section := Parse( AnsiString ( Conversation ), 1, '.' );
    if ( Filename = '' ) then
    begin
      Close;
      exit;
    end;
    Filename := ResourcePath + 'conversations\' + Language + '\' + Filename + '.cnv';
    INI := TMemINIFile.Create( Filename );

    LoadConversation;

  //DrawShadow
    Shadow := SoAOS_DX_LoadBMP( InterfacePath + 'DialogueBoxshadowmap.bmp', cTransparent, width, height );
    try
      DrawSub( lpDDSBack, ApplyOffset( Rect( X1, Y1, X2, Y2 ) ), Rect( 0, 0, width, height ), Shadow, true, 170 );
    finally
      Shadow := nil;
    end;

    SoAOS_DX_BltFront;
    pText.LoadTinyFontGraphic;

    Paint;
  except
    on E : Exception do
      Log.log( FailName, E.Message, [ ] );
  end;
end;

procedure TConverseBox.MouseDown( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer );
var
  strResponse : TTokenString;
const
  FailName : string = 'TConverseBox.MouseDown';
begin
  try
//    inherited;
//    if InBound then
    if ptInRect( ApplyOffset( Rect( X1, Y1, X2, X2 ) ), point( x, y ) ) then
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

          if ( strResponse.PipeToken( 2 ) = '' ) and ( strResponse.PipeToken( 1 ) = '' ) then
          begin
            Close;
            exit;
          end;

          if ( strResponse.PipeToken( 2 ) <> '' ) then
            RunScript( ObjectRef, strResponse.PipeToken( 2 ) );

          if ( strResponse.PipeToken( 1 ) <> '') then
          begin
            Section := strResponse.PipeToken( 1 );
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

procedure TConverseBox.MouseMove( Sender : TObject;
      Shift : TShiftState; X, Y, MapX, MapY : Integer );
var
  i : Integer;
const
  FailName : string = 'TConverseBox.MouseMove';
begin
  try
//    inherited;
//    if InBound then
    if ptInRect( ApplyOffset( Rect( X1, Y1, X2, X2 ) ), point( x, y ) ) then
    begin
      for i := 0 to Responses.Count - 1 do
      begin
//          if ( X >= Rect.Left ) and ( X < Rect.Right ) and ( Y >= Rect.Top ) and ( Y < Rect.Bottom ) then
        if PtInRect( ApplyOffset( Responses[i].Rect ), point( x, y ) ) then
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

procedure TConverseBox.MouseUp( Sender : TObject; Button : TMouseButton;
      Shift : TShiftState; X, Y, GridX, GridY : Integer );
begin

end;

{$HINTS OFF -- Jim Shiflett}

procedure TConverseBox.LoadConversation;
var
  NewText : TTextRect;
  strCrntHeading : string;
  tmpStr: TTokenString;
  strTmp, S : string;
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
  Log.DebugLog( FailName );
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

      if AnsiSameText(ini.ReadString( 'root', 'random', 'false' ), 'true') then
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

          tmpStr := Ini.ReadString( 'root', 'say' + IntToStr( iLoop ), '' );

          if string(tmpStr).StartsWith('all', True) then
          begin
            strAll := tmpStr.SemiToken( 0 );
            strOne := tmpStr.SemiToken( 1 );
          end
          else if string(tmpStr).StartsWith('one', True) then
            strOne := tmpStr.SemiToken( 0 );


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
        Caption := TCharacter( ObjectRef ).properties[ 'charactername' ] + ':  ' + Ini.ReadString( strCrntHeading, 'Say', '' )
      else
        Caption := Ini.ReadString( strCrntHeading, 'Say', '' );


      if Ini.ReadString( strCrntHeading, 'train', '' ) = 'true' then
      begin
        Caption := Caption + #13#13 + training + ' = ' + IntToStr( Player.trainingpoints );
        bTraining := true;
      end;
    end
    else
      Caption := TCharacter( ObjectRef ).properties[ 'charactername' ] + ':  ' + hail;


    caption := StringReplace( caption, '%playername%', player.name, [ rfReplaceAll, rfIgnoreCase ] );

    //Say Script
    if Ini.ReadString( strCrntHeading, 'script', '' ) <> '' then
      RunScript( ObjectRef, Ini.ReadString( strCrntHeading, 'script', '' ) );

    if Ini.ReadString( strCrntHeading, 'special', '' ) <> '' then
    begin
      strTmp := TTokenString(Ini.ReadString( strCrntHeading, 'special', '' )).PipeToken( 0 );
      for jLoop := 0 to NPCList.count - 1 do
      begin
        if TCharacter( NPCList.Items[ jLoop ] ).Guid = strTmp then
        begin
          RunScript( TCharacter( NPCList.Items[ jLoop ] ), TTokenString(Ini.ReadString( strCrntHeading, 'special', '' )).PipeToken( 1 ));
        end;
      end;

    end;


    if Ini.ReadString( strCrntHeading, 'addquest', '' ) <> '' then
    begin
      for iLoop := 0 to TTokenString(Ini.ReadString( strCrntHeading, 'addquest', '' )).TokenCount(';') - 1 do
        RunScript( ObjectRef, 'addquest(' + TTokenString(Ini.ReadString( strCrntHeading, 'addquest', '' )).SemiToken( iLoop ) + ')' );
        //RunScript(ObjectRef, '#showmessage.quest#');
    end;
    if Ini.ReadString( strCrntHeading, 'quest', '' ) <> '' then
    begin
      for iLoop := 0 to TTokenString(Ini.ReadString( strCrntHeading, 'quest', '' )).TokenCount(';') - 1 do
        RunScript( ObjectRef, 'addquest(' + TTokenString(Ini.ReadString( strCrntHeading, 'quest', '' )).SemiToken( iLoop ) + ')' );
        //RunScript(ObjectRef, '#showmessage.quest#');
    end;

    if Ini.ReadString( strCrntHeading, 'adventure', '' ) <> '' then
    begin
      for iLoop := 0 to TTokenString(Ini.ReadString( strCrntHeading, 'adventure', '' )).TokenCount(';') - 1 do
        RunScript( ObjectRef, 'Adventure(' + TTokenString(Ini.ReadString( strCrntHeading, 'Adventure', '' )).SemiToken( iLoop ) + ')' );
        //RunScript(ObjectRef, '#Showmessage.adventure#');
    end;


    if Ini.ReadString( strCrntHeading, 'removequest', '' ) <> '' then
      for iLoop := 0 to TTokenString(Ini.ReadString( strCrntHeading, 'removequest', '' )).TokenCount(';') - 1 do
        RunScript( ObjectRef, 'removequest(' + TTokenString(Ini.ReadString( strCrntHeading, 'removequest', '' )).SemiToken( iLoop ) + ')' );


    for iLoop := 0 to Responses.count - 1 do
      Responses[ iLoop ].Free;
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
      if string(tmpstr).StartsWith('all', True) then
      begin
        strAll := tmpStr.SemiToken( 0 );
        strOne := tmpStr.SemiToken( 1 );
      end
      else if string(tmpstr).StartsWith('one', True) then
        strOne := tmpStr.SemiToken( 0 );

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
  pr : TRect;
const
  FailName : string = 'TConverseBox.Paint';
begin
  Log.DebugLog( FailName );
  try
    j := Y1 + 30;
    if assigned( Image ) then
    begin
      GetSurfaceDims( W, H, Image );
      pr := Rect( 0, 0, W, H );
      lpDDSback.BltFast( X1 + Offset.X, Y1 + Offset.Y, Image, @pr, DDBLTFAST_SRCCOLORKEY or DDBLTFAST_WAIT );
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
        j := j + 18 * pText.PlotTinyTextBlock( Caption, R.Left + Offset.X, R.Right + Offset.X, R.Top + Offset.Y, 240 ) + 10
      else
        j := j + 22 * pText.PlotTextBlock( Caption, R.Left + Offset.X, R.Right + Offset.X, R.Top + Offset.Y, 240 ) + 10;
      R.Bottom := j;

      if bTraining then
        inc( j, 50 )
      else
        inc( j, 20 );
    end;
    for i := 0 to Responses.count - 1 do
    begin
      S := Responses[i].Text;
      R.Left := X1 + 40;
      R.Top := j;
      R.Right := X1 + W - 30;
      { TODO : Wrap pText plot into existing DIalogs PlotTexts }
      if i = HLText then
      begin
        if UseSmallFont then
          j := j + 18 * pText.PlotTinyTextBlock( S, R.Left + Offset.X, R.Right + Offset.X, R.Top + Offset.Y, 240 )
        else
          j := j + 22 * pText.PlotTextBlock( S, R.Left + Offset.X, R.Right + Offset.X, R.Top + Offset.Y, 240 );
      end
      else
      begin
        if UseSmallFont then
          j := j + 18 * pText.PlotTinyTextBlock( S, R.Left + Offset.X, R.Right + Offset.X, R.Top + Offset.Y, 120 )
        else
          j := j + 22 * pText.PlotTextBlock( S, R.Left + Offset.X, R.Right + Offset.X, R.Top + Offset.Y, 120 );
      end;
      R.Bottom := j;
      Responses[ i ].Rect := R;
      Inc( j, 10 );
    end;
    SoAOS_DX_BltFront;
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
  Log.DebugLog( FailName );
  try
    inherited;
    INI.Free;
    INI := nil;
    Image := nil;
    Image := nil;
    for i := 0 to Responses.Count - 1 do
      Responses[ i ].Free;
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
  Log.DebugLog( FailName );
  try
    Result := '';
    if sTmp = '' then
      exit;

    sTmp := sTmp.Substring(4, sTmp.Length-5);
    for iLoop := 0 to 2 do
    begin
      if TTokenString(sTmp).ColonToken( iLoop ).StartsWith('stt', True) then
      begin //List of Stats
        Result := TTokenString(sTmp).ColonToken( iLoop );
        Result := Result.Substring(4, Result.Length-5);
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
  Log.DebugLog( FailName );
  try
    Result := '';
    if sTmp = '' then
      exit;

    sTmp := sTmp.Substring(4, sTmp.Length-5);
    for iLoop := 0 to 2 do
    begin
      if TTokenString(sTmp).ColonToken( iLoop ).StartsWith('ttl', True) then
      begin //List of titles
        Result := TTokenString(sTmp).ColonToken( iLoop );
        Result := Result.Substring(4, Result.Length-5);
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
  Log.DebugLog( FailName );
  try
    Result := '';
    if sTmp = '' then
      exit;

    sTmp := sTmp.Substring(4, sTmp.Length-5);
    for iLoop := 0 to 2 do
    begin
      if TTokenString(sTmp).ColonToken( iLoop ).StartsWith('chp', True) then
      begin //List of titles
        Result := TTokenString(sTmp).ColonToken( iLoop );
        Result := Result.Substring(4, Result.Length-5);
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
  Log.DebugLog( FailName );
  try
    Result := '';
    if sTmp = '' then
      exit;

    sTmp := sTmp.Substring(4, sTmp.Length-5);
    for iLoop := 0 to 2 do
    begin
      if TTokenString(sTmp).ColonToken( iLoop ).StartsWith('obj', True) then
      begin //List of objects
        Result := TTokenString(sTmp).ColonToken( iLoop );
        Result := Result.Substring(4, Result.Length-5);
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
  Log.DebugLog( FailName );
  try
    Result := '';
    if sTmp = '' then
      exit;

    sTmp := sTmp.Substring(4, sTmp.Length-5);
    for iLoop := 0 to 2 do
    begin
      if TTokenString(sTmp).ColonToken( iLoop ).StartsWith('wrn', True) then
      begin //List of objects
        Result := TTokenString(sTmp).ColonToken( iLoop );
        Result := Result.Substring(4, Result.Length-5);
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
  Log.DebugLog( FailName );
  try
    Result := '';
    if sTmp = '' then
      exit;

    sTmp := sTmp.Substring(4, sTmp.Length-5);
    for iLoop := 0 to 2 do
    begin
      if TTokenString(sTmp).ColonToken( iLoop ).StartsWith('pty', True) then
      begin //List of objects
        Result := TTokenString(sTmp).ColonToken( iLoop );
        Result := Result.Substring(4, Result.Length-5);
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
  Log.DebugLog( FailName );
  try
    if sTmp = '' then
      exit;
    for iLoop := 0 to TTokenString(sTmp).TokenCount(',') - 1 do
    begin
      strTmp := TTokenString(sTmp).CommaToken( iLoop ).Trim;
      if strTmp[1] <> '!' then
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
        b2 := false;
        for jLoop := 0 to NPCList.count - 1 do
        begin
          if TCharacter( NPCList.Items[ jLoop ] ).Guid = strTmp.Remove(0, 1) then //get rid of the '!'
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

  Log.DebugLog( FailName );
  try
    if sTmp = '' then
      exit;

    for iLoop := 0 to TTokenString(sTmp).TokenCount(',') - 1 do
    begin
      strTmp := TTokenString(sTmp).CommaToken( iLoop ).Trim;
      if strTmp[1] <> '!' then
      begin
        if not ( Player.TitleExists( strTmp ) ) and not ( TCharacter( ObjRef ).TitleExists( strTmp ) ) then //player does not have it but should
          Result := false;
      end
      else //Make sure player doesnt have title
      begin
        strTmp := strTmp.Remove(0, 1); //get rid of the '!'
        if ( Player.TitleExists( strTmp ) ) or ( TCharacter( ObjRef ).TitleExists( strTmp ) ) then //player does have it but shouldnt
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

  Log.DebugLog( FailName );
  try
    if sTmp = '' then
      exit;

    for iLoop := 0 to TTokenString(sTmp).TokenCount(',') - 1 do
    begin
      strTmp := TTokenString(sTmp).CommaToken( iLoop ).Trim;
      if strTmp[1] <> '!' then
      begin
        if ( int64( 1 shl ( strTmp.ToInteger - 1 ) ) and Chapters ) = 0 then
          Result := false;
      end
      else //Make sure player doesnt have title
      begin
        if ( int64( 1 shl ( strTmp.Remove(0, 1).ToInteger - 1 ) ) and Chapters ) <> 0 then
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
  Result := True;

  Log.DebugLog( FailName );
  try
    if sTmp = '' then
      exit;
    for iLoop := 0 to 1 do
    begin
      strTmp := TTokenString(sTmp).CommaToken( iLoop ).Trim;
      if strTmp = '' then
        break;
      try
        if not checkStat(TTokenString(strTmp), 'charm', Current.Charm) then Exit(False);
        if not checkStat(TTokenString(strTmp), 'combat', Current.Combat) then Exit(False);
        if not checkStat(TTokenString(strTmp), 'constitution', Current.Constitution) then Exit(False);
        if not checkStat(TTokenString(strTmp), 'coordination', Current.coordination) then Exit(False);
        if not checkStat(TTokenString(strTmp), 'hitpoints', Current.Hitpoints) then Exit(False);
        if not checkStat(TTokenString(strTmp), 'mana', Current.Mana) then Exit(False);
        if not checkStat(TTokenString(strTmp), 'mysticism', Current.mysticism) then Exit(False);
        if not checkStat(TTokenString(strTmp), 'perception', Current.perception) then Exit(False);
        if not checkStat(TTokenString(strTmp), 'stealth', Current.stealth) then Exit(False);
        if not checkStat(TTokenString(strTmp), 'strenght', Current.strength) then Exit(False);
        if not checkStat(TTokenString(strTmp), 'wounds', Current.wounds) then Exit(False);
        if not checkStat(TTokenString(strTmp), 'trainingpoints', Current.TrainingPoints) then Exit(False);
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

  Log.DebugLog( FailName );
  try
    if sTmp = '' then
      exit;

    for iLoop := 0 to TTokenString(sTmp).TokenCount(',') - 1 do
    begin
      strTmp := TTokenString(sTmp).CommaToken( iLoop ).Trim;
      if StrTmp[1] <> '!' then
      begin
        if not ( Player.InInventory( StrTmp ) ) then //player does not have it but should
          Result := false;
      end
      else //Make sure player doesnt have title
      begin
        if Player.InInventory( strTmp.Remove(0, 1) ) then //player does have it but shouldnt //get rid of the '!'
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

  Log.DebugLog( FailName );
  try
    if sTmp = '' then
      exit;

    for iLoop := 0 to TTokenString(sTmp).TokenCount(',') - 1 do
    begin
      strTmp := TTokenString(sTmp).CommaToken( iLoop ).Trim;
      if StrTmp[1] <> '!' then
      begin
        if not ( Player.IsWorn( StrTmp ) ) then //player does not have it but should
          Result := false;
      end
      else //Make sure player doesnt have title
      begin
        if Player.IsWorn( strTmp.Remove(0, 1) ) then //player does have it but shouldnt //get rid of the '!'
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

  Log.DebugLog( FailName );
  try
    if sTmp = '' then
      exit;
    for iLoop := 0 to TTokenString(sTmp).TokenCount(',') - 1 do
    begin
      strTmp := TTokenString(sTmp).CommaToken( iLoop ).Trim;
      if strTmp[1] <> '!' then
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
        strTmp := strTmp.Remove(0, 1); //get rid of the '!'
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

  Log.DebugLog( FailName );
  try
    if sTmp = '' then
      exit;

    for iLoop := 0 to TTokenString(sTmp).TokenCount(',') - 1 do
    begin
      strTmp := TTokenString(sTmp).CommaToken( iLoop ).Trim;
      if strTmp[1] <> '!' then
      begin
        if ( Player.TitleExists( strTmp ) ) or ( TCharacter( ObjRef ).TitleExists( strTmp ) ) then //player has it and should
          Result := true;
      end
      else //Make sure player doesnt have title
      begin
        strTmp := strTmp.Remove(0, 1); //get rid of the '!'
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

  Log.DebugLog( FailName );
  try
    if sTmp = '' then
      exit;

    Result := false;

    bTmp := false;
    for iLoop := 0 to TTokenString(sTmp).TokenCount(',') - 1 do
    begin
      strTmp := TTokenString(sTmp).CommaToken( iLoop ).Trim;
      if strTmp[1] <> '!' then
      begin
        if ( int64( 1 shl ( strtoint( strTmp ) - 1 ) ) and Chapters ) <> 0 then
          bTmp := true;
      end
      else //Make sure the chapter doesnt exist
      begin
        if ( int64( 1 shl ( strTmp.Remove(0, 1).ToInteger - 1 ) ) and Chapters ) = 0 then //get rid of the '!'
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

  Log.DebugLog( FailName );
  try
    if sTmp = '' then
      exit;

    Result := false;
    bTmp := false;
    for iLoop := 0 to 1 do
    begin
      strTmp := TTokenString(sTmp).CommaToken( iLoop ).Trim;
      if strTmp = '' then
        break;
      try
        if checkStat1(TTokenString(strTmp), 'charm', Current.Charm) then bTmp := True;
        if checkStat1(TTokenString(strTmp), 'combat', Current.Combat) then bTmp := True;
        if checkStat1(TTokenString(strTmp), 'constitution', Current.Constitution) then bTmp := True;
        if checkStat1(TTokenString(strTmp), 'coordination', Current.Coordination) then bTmp := True;
        if checkStat1(TTokenString(strTmp), 'hitpoints', Current.hitpoints) then bTmp := True;
        if checkStat1(TTokenString(strTmp), 'mana', Current.mana) then bTmp := True;
        if checkStat1(TTokenString(strTmp), 'mysticism', Current.mysticism) then bTmp := True;
        if checkStat1(TTokenString(strTmp), 'perception', Current.perception) then bTmp := True;
        if checkStat1(TTokenString(strTmp), 'stealth', Current.stealth) then bTmp := True;
        if checkStat1(TTokenString(strTmp), 'strenght', Current.strength) then bTmp := True;
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
  Log.DebugLog( FailName );
  try
    if sTmp = '' then
      exit;
    Result := false;
    bTmp := false;
    for iLoop := 0 to TTokenString(sTmp).TokenCount(',') - 1 do
    begin
      strTmp := TTokenString(sTmp).CommaToken( iLoop ).Trim;
      if StrTmp[1] <> '!' then
      begin
        if Player.InInventory( StrTmp ) then //player does not have it but should
          bTmp := true;
      end
      else //Make sure player doesnt have title
      begin
        if not ( Player.InInventory( strTmp.Remove(0, 1) ) ) then //player does have it but shouldnt  //get rid of the '!'
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
  Log.DebugLog( FailName );
  try
    if sTmp = '' then
      exit;
    Result := false;
    bTmp := false;
    for iLoop := 0 to TTokenString(sTmp).TokenCount(',') - 1 do
    begin
      strTmp := TTokenString(sTmp).CommaToken( iLoop ).Trim;
      if StrTmp[1] <> '!' then
      begin
        if Player.IsWorn( StrTmp ) then //player does not have it but should
          bTmp := true;
      end
      else //Make sure player doesnt have title
      begin
        if not ( Player.IsWorn( strTmp.Remove(0, 1) ) ) then //player does have it but shouldnt //get rid of the '!'
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

function checkStat(s: TTokenString; statStr: string; curPropVal: integer) : boolean;
begin
  Result := True;
  if string(s).Contains( '>' ) then
  begin
    if AnsiSameText(s.gtToken( 0 ), statStr) then
      if curPropVal < s.gtToken( 1 ).ToInteger + 1 then
        Result := False;
  end
  else
  begin
    if AnsiSameText(s.ltToken( 0 ), statStr) then
      if curPropVal > s.ltToken( 1 ).ToInteger - 1 then
        Result := False;
  end;
end;

function checkStat(s: TTokenString; statStr: string; curPropVal: Single) : boolean;
begin
  Result := True;
  if string(s).Contains( '>' ) then
  begin
    if AnsiSameText(s.gtToken( 0 ), statStr) then
      if curPropVal < s.gtToken( 1 ).ToSingle + 1 then
        Result := False;
  end
  else
  begin
    if AnsiSameText(s.ltToken( 0 ), statStr) then
      if curPropVal > s.ltToken( 1 ).ToSingle - 1 then
        Result := False;
  end;
end;

function checkStat(s: TTokenString; statStr: string; curPropVal: Double) : boolean;
begin
  Result := True;
  if string(s).Contains( '>' ) then
  begin
    if AnsiSameText(s.gtToken( 0 ), statStr) then
      if curPropVal < s.gtToken( 1 ).ToSingle + 1 then
        Result := False;
  end
  else
  begin
    if AnsiSameText(s.ltToken( 0 ), statStr) then
      if curPropVal > s.ltToken( 1 ).ToSingle - 1 then
        Result := False;
  end;
end;

function checkStat1(s: TTokenString; statStr: string; curPropVal: Integer) : boolean;
begin
  Result := False;
  if string(s).Contains( '>' ) then
  begin
    if AnsiSameText(s.gtToken( 0 ), statStr) then
      if curPropVal > s.gtToken( 1 ).ToInteger then
        Result := True;
  end
  else
  begin
    if AnsiSameText(s.ltToken( 0 ), statStr) then
      if curPropVal > s.ltToken( 1 ).ToInteger then
        Result := True;
  end;
end;

function checkStat1(s: TTokenString; statStr: string; curPropVal: Single) : boolean;
begin
  Result := False;
  if string(s).Contains( '>' ) then
  begin
    if AnsiSameText(s.gtToken( 0 ), statStr) then
      if curPropVal > s.gtToken( 1 ).ToSingle then
        Result := True;
  end
  else
  begin
    if AnsiSameText(s.ltToken( 0 ), statStr) then
      if curPropVal > s.ltToken( 1 ).ToSingle then
        Result := True;
  end;
end;

function checkStat1(s: TTokenString; statStr: string; curPropVal: Double) : boolean;
begin
  Result := False;
  if string(s).Contains( '>' ) then
  begin
    if AnsiSameText(s.gtToken( 0 ), statStr) then
      if curPropVal > s.gtToken( 1 ).ToDouble then
        Result := True;
  end
  else
  begin
    if AnsiSameText(s.ltToken( 0 ), statStr) then
      if curPropVal > s.ltToken( 1 ).ToDouble then
        Result := True;
  end;
end;

end.
