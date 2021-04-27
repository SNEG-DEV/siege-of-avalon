unit D3DMousePtr;

{$POINTERMATH ON}

interface

uses WinTypes, D3DRenderer, Vcl.ExtCtrls, Classes, AbstractMousePtr;

const
  CursorWidth = 32;
  CursorHeight = 32;

type
  TMouseUpdaterThread = class(TThread)
  private
    FLayer: TDXRenderLayer;
    FSize: TSize;
    FQuit: Boolean;
  public
    constructor Create(aLayer: TDXRenderLayer; aSize: TSize);
    destructor Destroy; override;
    procedure Execute; override;
    procedure Stop;
  end;

  TD3DMousePtr = class(TAbstractMousePtr)
  private
    FNumFrames: TSize;
    FMouseFrame: Integer;
    FPlotDirty: Boolean;
    FEnabled: Boolean;
    FLayer: TDXRenderLayer;
    FRenderer: TDXRenderer;
    FMouseThread: TMouseUpdaterThread;

    procedure SetPlotDirty(const Value: Boolean); override;
    function GetPlotDirty: Boolean; override;
    procedure SetEnabled(const Value: Boolean); override;
    function GetEnabled: Boolean; override;
  protected
  public
    constructor Create(Renderer: TDXRenderer);
    destructor Destroy; override;
    procedure Cleanup; override;
    procedure SetFrame(Frame: Integer ); override;
  end;

implementation

uses
  AniDemo,
  SysUtils,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.Imaging.pngimage,
  Vcl.Graphics
  ;

{ TMousePtr }

procedure TD3DMousePtr.Cleanup;
begin
end;

procedure UnpackBitmap(Source: TBitmap; Destination: PDWORD; TransparentColor: TColor);
var
  Bmp24: TBitmap;
  Row: PByte;
  I, J: Integer;
  Color: TColor;
begin
  Bmp24 := TBitmap.Create;
  try
    Bmp24.SetSize(Source.Width, Source.Height);
    Bmp24.PixelFormat := pf24bit;
    Bmp24.Canvas.Draw(0, 0, Source);

    for I := 0 to Source.Height - 1 do
    begin
      Row := Bmp24.ScanLine[I];
      for J := 0 to Source.Width do
      begin
        Color := Row[J * 3 + 2] + (Row[J * 3 + 1] shl 8) + (Row[J * 3] shl 16);
        if Color = TransparentColor then
          (Destination + Source.Width * I + J)^ := Color
        else
          (Destination + Source.Width * I + J)^ := Color or $FF000000;
      end;
    end;
  finally
    Bmp24.Free;
  end;
end;

constructor TD3DMousePtr.Create(Renderer: TDXRenderer);
var
  Sheet: TBitmap;
  Data: PDWORD;
begin
  FRenderer := Renderer;

  Sheet := TBitmap.Create;
  Sheet.LoadFromFile(InterfacePath + 'siegecursorsheet.bmp');
  FNumFrames := TSize.Create(Sheet.Width div CursorWidth, Sheet.Height div CursorHeight);
  GetMem(Data, Sheet.Width * Sheet.Height * 4);
  try
    UnpackBitmap(Sheet, Data, $FF00FF);
    FLayer := FRenderer.CreateLayer(sheet.Width, sheet.Height, dxfmt_8888, blend_transparent);
    FLayer.UpdateTexture(data, sheet.Width * 4);
  finally
    FreeMem(Data);
  end;
  FLayer.Enabled := False;
  FLayer.SetSourceRect(TRect.Create(0, 0, CursorWidth, CursorHeight));
  FLayer.SetDestRect(TRect.Create(0, 0, CursorWidth, CursorHeight));
  FMouseThread := TMouseUpdaterThread.Create(FLayer, TSize.Create(CursorWidth, CursorHeight));
  FMouseThread.Start;
end;

destructor TD3DMousePtr.Destroy;
begin
  FMouseThread.Destroy;
end;

function TD3DMousePtr.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TD3DMousePtr.GetPlotDirty: Boolean;
begin
  Result := FPlotDirty;
end;

procedure TD3DMousePtr.SetEnabled(const Value: Boolean);
begin
  if Assigned(FLayer) then
  begin
    FLayer.Enabled := Value;
  end;
  FEnabled := Value;
end;

procedure TD3DMousePtr.SetFrame(Frame: Integer);
var
  Rect: TRect;
begin
  Rect.Left := (Frame Mod FNumFrames.Width) * CursorWidth;
  Rect.Top := (FNumFrames.Height - Frame Div FNumFrames.Height + 1) * CursorHeight;
  Rect.Width := CursorWidth;
  Rect.Height := CursorHeight;
  FLayer.SetSourceRect(Rect);
end;

procedure TD3DMousePtr.SetPlotDirty(const Value: Boolean);
begin
end;

{ TMouseUpdaterThread }

constructor TMouseUpdaterThread.Create(aLayer: TDXRenderLayer; aSize: TSize);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FLayer := aLayer;
  FSize := aSize;
end;

destructor TMouseUpdaterThread.Destroy;
begin
  Stop;
  inherited;
end;

procedure TMouseUpdaterThread.Execute;
var
  rc: TRect;
  CursorPos: TPoint;
begin
  while not FQuit do
  begin
    GetCursorPos(CursorPos);
    ScreenToClient(frmMain.Handle, CursorPos);
    rc.TopLeft := CursorPos;
    rc.Width := FSize.Width;
    rc.Height := FSize.Height;
    FLayer.SetDestRect(rc);
    PostMessage(frmMain.Handle, 0, 0, 0);
    Sleep(10);
  end;
end;

procedure TMouseUpdaterThread.Stop;
begin
  FQuit := True;
  WaitFor;
end;

end.

