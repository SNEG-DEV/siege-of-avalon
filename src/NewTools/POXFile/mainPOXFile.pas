unit mainPOXFile;
{******************************************************************************}
{                                                                              }
{               Siege Of Avalon : Open Source Edition <New Tooling>            }
{               -------------------------------------                          }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{ Steffen Nyeland <steffen@nyeland.dk>                                         }
{                                                                              }
{                                                                              }
{                                                                              }
{ You may retrieve the latest version of this file at the forked SOAOS project:}
{   https://github.com/SteveNew/Siege-of-Avalon-Open-Source                    }
{                                                                              }
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
{   Delphi 10.3 or later                                                       }
{   A free Community Edition is available from...                              }
{   https://www.embarcadero.com                                                }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{   Juni    12 2019 - SN : Initial Upload to GitHub                            }
{   Juli    16 2019 - discontinued - and continued as POXStudio at:            }
{                     https://github.com/SteveNew/POXStudio                    }
{                                                                              }
{******************************************************************************}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.ScrollBox, FMX.Memo, System.Generics.Collections,
  FMX.Ani, FMX.Controls.Presentation;

type
  PRLEHDR = ^RLEHDR;
  RLEHDR = record
    SrcX : integer;
    SrcY : integer;
    Wdh : DWORD;
    Hgh : DWORD;
    AdjX : integer;
    AdjY : integer;
    PixFmt : DWORD;
    DataPtr : PChar;
  end;

  TROMemoryStream = class(TMemoryStream)
  private
    OrigPtr : Pointer;
    OrigSize : LongInt;
  public
    procedure SetMemPointer(Ptr: Pointer; newSize: Longint);
    destructor Destroy; override;
  end;

  TDirectionEnum = ( Frames, NWFrames, NNFrames, NEFrames, EEFrames, SEFrames, SSFrames, SWFrames, WWFrames );

  TFrameList = class(TList<Integer>);

  TfrmMain = class(TForm)
    pnlTop: TPanel;
    btnLoad: TButton;
    lblINIData: TLabel;
    lblRLE: TLabel;
    memINI: TMemo;
    sbrStatus: TStatusBar;
    imgRLE: TImage;
    OpenDialog1: TOpenDialog;
    lblFilename: TLabel;
    lblResType: TLabel;
    tkbFrames: TTrackBar;
    sbPlay: TSpeedButton;
    sbPause: TSpeedButton;
    playFrames: TFloatAnimation;
    Panel1: TPanel;
    Action: TLabel;
    Label1: TLabel;
    sbNW: TSpeedButton;
    sbN: TSpeedButton;
    sbNE: TSpeedButton;
    sbW: TSpeedButton;
    sbStatic: TSpeedButton;
    sbE: TSpeedButton;
    sbSW: TSpeedButton;
    sbS: TSpeedButton;
    sbSE: TSpeedButton;
    rbX1: TRadioButton;
    rbX2: TRadioButton;
    rbX3: TRadioButton;
    btnExport: TButton;
    lblFrameCnt: TLabel;
    procedure btnLoadClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure tkbFramesChange(Sender: TObject);
    procedure sbPlayClick(Sender: TObject);
    procedure sbPauseClick(Sender: TObject);
    procedure rbX2Change(Sender: TObject);
    procedure rbX1Change(Sender: TObject);
    procedure rbX3Change(Sender: TObject);
    procedure directionClick(Sender: TObject);
    procedure actionChange2(Sender: TObject);
    procedure btnExportClick(Sender: TObject);
  private
    { Private declarations }
    actions: TStringList;
    actionButtons: TObjectList<TRadioButton>;
    currentActionIdx: Byte;
    currentDirection: Byte;
    currentObjectName: string;
    frameMultiplier: integer;
    movements: TObjectDictionary<Byte, TFrameList>;
    bmpList: TObjectList<TBitmap>;
    procedure zoom(factor: single);
    procedure parseIni(iniData: TStrings);
    procedure updActions;
    procedure updDirections;
    procedure updateTrackBar;
    procedure decodeRLE(rle: PRLEHDR; rleSize: integer; var bitmap: TBitmap);
  public
    { Public declarations }
    function LoadPOXFile(filename: string): Boolean;
  end;

var
  frmMain: TfrmMain;

implementation

uses
  System.Math, System.IniFiles, System.StrUtils, System.RTTI, System.IOUtils;

{$R *.fmx}

{ TForm8 }

procedure TfrmMain.btnExportClick(Sender: TObject);
var
  i: integer;
  exportpath: string;
begin
  exportpath := TPath.GetPicturesPath;
  if SelectDirectory('Select a directory', exportPath, exportPath) then
  begin
    exportpath := IncludeTrailingPathDelimiter(exportpath);
    for i := 0 to bmpList.Count-1 do
      bmplist[i].SaveToFile( exportpath+currentObjectName+'_frame'+(i+1).ToString+'.bmp' );
  end;
end;

procedure TfrmMain.btnLoadClick(Sender: TObject);
begin
  playFrames.Stop;
  if OpenDialog1.Execute() then
  begin
    memINI.Lines.Clear;
    rbX1.IsChecked := True;
    movements.Clear;
    tkbFrames.Value := 0;
    LoadPOXFile(OpenDialog1.FileName);
    imgRLE.Bitmap := bmpList[0];
    updActions;
    updDirections;
    btnExport.Enabled := True;
  end;
end;

procedure TfrmMain.decodeRLE(rle: PRLEHDR; rleSize: integer; var bitmap: TBitmap);
var
  i, x, y : integer;
  c : byte;
  colour: word;
  pxCol: TAlphaColorRec;
  rleData: TROMemoryStream;
  bmpData: TBitmapData;
begin
  pxCol.A := $FF;
  if bitmap.Map(TMapAccess.Write, bmpData) then
  begin
    x := 0;
    y := 0;
    rleData := TROMemoryStream.Create;
    rleData.SetMemPointer(rle.DataPtr, rleSize);
    rleData.Position := 0;
    rleData.Read(&c, 1);
    while (c > 0) and (c < 4) do
    begin
      case c of
        1 : begin // colour/pixel data
          rleData.Read(&i, 4);
          while i > 0 do
          begin
            rleData.Read(&colour, 2);
            pxCol.B := (Colour and $1F) shl 3;
            pxCol.G := ((Colour and $7E0) shr 5) shl 2;
            pxCol.R := ((Colour and $F800) shr 11) shl 3;
  // Alternatives - but above seems good enough and fastest
  //          r := (r * 527 + 23 ) shr 6; // floor(255/31 * R);
  //          g := (g * 259 + 33 ) shr 6; // floor(255/63 * G);
  //          b := (b * 527 + 23 ) shr 6; // floor(255/31 * B);
            bmpData.SetPixel(X+rle.AdjX, Y+rle.AdjY, pxCol.Color);
            inc(x);
            dec(i);
          end;
        end;
        2 : begin // add x offset
          rleData.Read(&i, 4);
          i := i div 2;
          inc(x, i);
        end;
        3 : inc(y); // new line, carriage return
      end;
      rleData.Read(&c, 1);
    end;
    FreeAndNil(rleData);
    bitmap.Unmap(bmpData);
  end;
end;

procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
var
  framelst: TFrameList;
begin
  bmpList.Free;
  actions.Free;
  actionButtons.Free;
  for framelst in movements.Values do
  begin
    framelst.Free;
  end;

  movements.Free;
  CanClose := True;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  actions := TStringList.Create;
  actions.Duplicates := dupIgnore;
  actions.Sorted := True;
  actionButtons := TObjectList<TRadioButton>.Create(false);
  bmpList := TObjectList<TBitmap>.Create();
  movements := TObjectDictionary<Byte, TFrameList>.Create();
end;

function TfrmMain.LoadPOXFile(filename: string): Boolean;
var
  f : TBufferedFileStream;
  str : AnsiString;
  M: array[ 1..2 ] of AnsiChar;
  BB : Word;
  PicCnt, Size, RLESize, i : DWORD;
  lpRLE, RelocOffset : PChar;
  p : PRLEHDR;
  lpSpr : PRLEHDR;
  L : LongWord;
  bitmap : TBitmap;
begin
  Result := False;
  currentObjectName := TPath.GetFileNameWithoutExtension(filename);
  lblFilename.Text := ExtractFileName(fileName);
  f := TBufferedFileStream.Create(fileName, fmOpenRead);
  try
    f.Read( L , SizeOf( L ) );
    if ( L<>$41584F50 ) then // 'POXA'
    begin
      ShowMessage('Not a valid "POX (Proprietary Object eXtension)" file.');
      Exit;
    end;
    f.Read( M, SizeOf( M ) );
    f.Read( BB, SizeOf( BB ) );
    if ( M = #83#84 ) then
    begin //ST
      f.Read( L, sizeof( L ) );
      lblResType.Text := 'Static Resource';
    end
    else if ( M = #67#67 ) then
    begin //CC
      f.Read( L, sizeof( L ) );
      lblResType.Text := 'Non-Layered Character Resource';
    end
    else if ( M = #76#67 ) then
    begin //LC
      L := f.Size - f.Position;
      lblResType.Text := 'Layered Character Resource';
    end
    else if ( M = #68#83 ) then
    begin //DS
      f.Read( L, sizeof( L ) );
      lblResType.Text := 'Door Resource';
    end
    else if ( M = #84#84 ) then
    begin //TT
      f.Read( L, sizeof( L ) );
      lblResType.Text := 'Tile Resource';
    end
    else if ( M = #80#82 ) then
    begin //PR
      f.Read( L, sizeof( L ) );
      lblResType.Text := 'Projectile Resource';
    end
    else if ( M = #83#67 ) then
    begin //SC
      f.Read( L, sizeof( L ) );
      lblResType.Text := 'Cast Resource';
    end
    else if ( M = #76#76 ) then
    begin //LL
      f.Read( L, sizeof( L ) );
      lblResType.Text := '(Linked) Layer Resource';
    end
    else if ( M = #73#73 ) then
    begin //II
      f.Read( L, sizeof( L ) );
      lblResType.Text := 'Inventory Resource';
    end
    else if ( M = #83#80 ) then
    begin //SP
      f.Read( L, sizeof( L ) );
      lblResType.Text := 'Resource';
    end
    else
    begin
      ShowMessage('Not a known resource type.');
      exit;
    end;
    SetLength( str, L );
    f.Read( str[ 1 ], L );

    memINI.Lines.Text := str;  // IniFile Data
    parseINI(memINI.Lines);

    f.Read( BB, SizeOf( BB ) );
    if BB = $4242 then
    begin

      f.Read( PicCnt, SizeOf( PicCnt ) );
      f.Read( RLESize, SizeOf( RLESize ) );
      Size := PicCnt * SizeOf( RLEHDR );
      GetMem( lpSpr, Size );
      f.Read( lpSpr^, Size );
      GetMem( lpRLE, RLESize );
      f.Read( lpRLE^, RLESize );

      bmpList.Clear;

      RelocOffset := PChar( lpRLE - lpSpr.DataPtr );
      p := lpSpr;
      for i := 1 to PicCnt do
      begin
        bitmap := TBitmap.Create;
        bitmap.Width := Trunc(imgRLE.Width);
        bitmap.Height := Trunc(imgRLE.Height);
//        bitmap.PixelFormat := TPixelFormat.BGR_565; // pf16bit;  // Should be bgr565
        p.DataPtr := PChar( p.DataPtr + DWORD( RelocOffset ) );

        decodeRLE(p, RLESize, bitmap);  // was digifxConvertRLE( dfx_hnd, p );

        bmpList.Add(bitmap);
        Inc( p );
      end;
      FreeMem(lpSpr);
      FreeMem(lpRLE);
    end;
  finally
    f.Free;
  end;
  Result := True;
end;

procedure TfrmMain.parseIni(iniData: TStrings);
var
  ini: TMemIniFile;
  actionStr: string;
  frameList: TFrameList;

  procedure addFrames;
  var
    a, i: integer;
    direction: TDirectionEnum;
    frames, str: string;
    frameList: TFrameList;
  begin
    for a := 0 to actions.Count-1 do
    begin
      for direction := Low(TDirectionEnum) to High(TDirectionEnum) do
      begin
        frames := ini.ReadString('Action '+actions[a], TRttiEnumerationType.GetName(direction), '');
        if Frames<>'' then
        begin
          frameList := TFrameList.Create;
          for str in Frames.Split([',']) do
          begin
            if TryStrToInt(str, i) then
              frameList.Add(i);
          end;
          movements.Add(a*10+ord(direction), frameList);
        end;
      end;
    end;
  end;

begin
  ini := TMemIniFile.Create('');
  try
    ini.SetStrings(iniData);
    actionStr := ini.ReadString('HEADER', 'Actions', '');
    actions.Clear;
    actions.AddStrings(actionStr.Split([',']));

    imgRLE.Width := ini.ReadInteger('HEADER', 'ImageWidth', 0);
    imgRLE.Height := ini.ReadInteger('HEADER', 'ImageHeight', 0);
    frameMultiplier := ini.ReadInteger('HEADER','FrameMultiplier', 1);

    for frameList in movements.Values do
    begin
      frameList.Free;
    end;
    movements.Clear;

    addFrames;

  finally
    ini.Free;
  end;
end;

procedure TfrmMain.actionChange2(Sender: TObject);
begin
  playFrames.Stop;
  currentActionIdx := actions.IndexOf(TRadioButton(Sender).Text);
  updDirections;
  updateTrackBar;
end;

procedure TfrmMain.rbX1Change(Sender: TObject);
begin
  zoom(1.0);
end;

procedure TfrmMain.rbX2Change(Sender: TObject);
begin
  zoom(2.0);
end;

procedure TfrmMain.rbX3Change(Sender: TObject);
begin
  zoom(3.0);
end;

procedure TfrmMain.sbPlayClick(Sender: TObject);
begin
  playFrames.StartValue := 0;
  playFrames.StopValue := tkbFrames.Max;
  playFrames.Duration := tkbFrames.Max * 0.1 * frameMultiplier; // 100 msec per frame
  playFrames.Start;
end;

procedure TfrmMain.directionClick(Sender: TObject);
begin
  playFrames.Stop;
  currentDirection := TSpeedButton(Sender).Tag;
  updateTrackBar;
end;

procedure TfrmMain.sbPauseClick(Sender: TObject);
begin
  playFrames.Stop;
end;

procedure TfrmMain.tkbFramesChange(Sender: TObject);
var
  frame: integer;
begin
  frame := movements[(currentActionIdx*10)+currentDirection][Trunc(tkbFrames.Value)];
  imgRLE.Bitmap := bmpList[frame-1];
  lblFrameCnt.Text := 'Frame: '+frame.ToString;
end;

procedure TfrmMain.updActions;
var
  rb: TRadioButton;
  a: integer;
begin
  for rb in actionButtons do
    rb.Free;

  panel1.Repaint;
  for a := 0 to actions.Count-1 do
  begin
    rb := TRadioButton.Create(nil);
    rb.Parent := Panel1;
    rb.Text := actions[a];
    rb.Position.X := 8;
    rb.Position.Y := 24 + ( 18 * a );
    rb.GroupName := 'action';
    rb.OnChange := actionChange2;
    actionButtons.add(rb);
  end;
end;

procedure TfrmMain.updDirections;
begin
  sbStatic.Enabled := movements.ContainsKey(currentActionIdx*10+0);
  sbNW.Enabled := movements.ContainsKey(currentActionIdx*10+1);
  sbN.Enabled := movements.ContainsKey(currentActionIdx*10+2);
  sbNE.Enabled := movements.ContainsKey(currentActionIdx*10+3);
  sbE.Enabled := movements.ContainsKey(currentActionIdx*10+4);
  sbSE.Enabled := movements.ContainsKey(currentActionIdx*10+5);
  sbS.Enabled := movements.ContainsKey(currentActionIdx*10+6);
  sbSW.Enabled := movements.ContainsKey(currentActionIdx*10+7);
  sbW.Enabled := movements.ContainsKey(currentActionIdx*10+8);
end;

procedure TfrmMain.updateTrackBar;
var
  frames: TFrameList;
begin
  tkbFrames.Value := 0;
  if movements.TryGetValue((currentActionIdx*10)+currentDirection, frames) then
  begin
    tkbFrames.Max := frames.Count-1;
    imgRLE.Bitmap := bmpList[frames[Trunc(tkbFrames.Value)]-1];
  end
  else
  begin
    tkbFrames.Max := tkbFrames.Min;
    imgRLE.Bitmap := bmpList[0];
  end;
  sbPlay.Enabled := tkbFrames.Max <> tkbFrames.Min;
end;

procedure TfrmMain.zoom(factor: single);
begin
  imgRLE.Scale.X := factor;
  imgRLE.Scale.Y := factor;
end;

{ TROMemoryStream }

destructor TROMemoryStream.Destroy;
begin
  SetPointer ( OrigPtr, OrigSize );
  inherited;
end;

procedure TROMemoryStream.SetMemPointer(Ptr: Pointer; newSize: Longint);
begin
  OrigPtr := Memory;
  OrigSize := Self.Size;
  SetPointer ( ptr, newSize );
end;

end.
