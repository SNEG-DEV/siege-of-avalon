unit D3DMousePtr;

interface

uses WinTypes, D3DRenderer, Vcl.ExtCtrls, Classes, AbstractMousePtr, PreciseTimer;

const
  CursorWidth = 32;
  CursorHeight = 32;

type
  TD3DMousePTr = class;

  TMouseUpdaterThread = class(TThread)
  private
    FD3DMousePtr: TD3DMousePtr;
    FQuit: Boolean;
    FTimer: TPreciseTimer;
  public
    constructor Create(D3DMousePtr: TD3DMousePtr);
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
    procedure SetPosition(Position: TPoint);
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

constructor TD3DMousePtr.Create(Renderer: TDXRenderer);
var
  Sheet: TBitmap;
begin
  FRenderer := Renderer;

  Sheet := TBitmap.Create;
  Sheet.LoadFromFile(InterfacePath + 'siegecursorsheet.bmp');
  FNumFrames := TSize.Create(Sheet.Width div CursorWidth, Sheet.Height div CursorHeight);
  FLayer := FRenderer.CreateLayerFromBitmap(Sheet, $FF00FF);
  FLayer.Enabled := False;
  FLayer.SetSourceRect(TRect.Create(0, 0, CursorWidth, CursorHeight));
  FLayer.SetDestRect(TRect.Create(0, 0, CursorWidth, CursorHeight));
  FMouseThread := TMouseUpdaterThread.Create(Self);
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
  if Frame < 0 then
    Frame := 0;
  if Frame > FNumFrames.Width * FNumFrames.Height - 1 then
    Frame := FNumFrames.Width * FNumFrames.Height - 1;

  Rect.Left := (Frame Mod FNumFrames.Width) * CursorWidth;
  Rect.Top := (FNumFrames.Height - Frame Div FNumFrames.Width - 1) * CursorHeight;
  Rect.Width := CursorWidth;
  Rect.Height := CursorHeight;
  FLayer.SetSourceRect(Rect);
  FRenderer.BringLayerToTheTop(FLayer);
end;

procedure TD3DMousePtr.SetPosition(Position: TPoint);
var
  rc: TRect;
begin
  ScreenToClient(frmMain.Handle, Position);
  FLayer.SetPosition(Position);
  rc.TopLeft := Position;
  rc.Width := CursorWidth;
  rc.Height := CursorHeight;
  FLayer.SetDestRect(rc);
end;

procedure TD3DMousePtr.SetPlotDirty(const Value: Boolean);
begin
end;

{ TMouseUpdaterThread }

constructor TMouseUpdaterThread.Create(D3DMousePtr: TD3DMousePtr);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FD3DMousePtr := D3DMousePtr;
  FTimer := TPreciseTimer.Create;
end;

destructor TMouseUpdaterThread.Destroy;
begin
  Stop;
  FTimer.Destroy;
  inherited;
end;

procedure TMouseUpdaterThread.Execute;
var
  CursorPos: TPoint;
begin
  while not FQuit do
  begin
    GetCursorPos(CursorPos);
    FD3DMousePtr.SetPosition(CursorPos);
    PostMessage(frmMain.Handle, 0, 0, 0);
    FTimer.Wait(10);
  end;
end;

procedure TMouseUpdaterThread.Stop;
begin
  FQuit := True;
  WaitFor;
end;

end.

