unit MfPlayer;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  WinApi.WinApiTypes,
  WinApi.ComBaseApi,
  WinApi.ActiveX.ObjBase,
  WinApi.MediaFoundationApi.MfUtils,
  WinApi.MediaFoundationApi.MfApi,
  WinApi.MediaFoundationApi.MfPlay,
  WinApi.MediaFoundationApi.MfError,

  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs;

type
  TfrmMfPlayer = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormShow(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
  private
    { Private declarations }
    procedure WMSize(var Msg: TMessage); message WM_SIZE;
  public
    MoviePath: String;
    { Public declarations }
    class function PlayMovie(Path: String): Integer; static;
  end;

type
  TMediaPlayerCallback = class(TInterfacedPersistent, IMFPMediaPlayerCallback)
  private
    procedure OnMediaPlayerEvent(var pEventHeader: MFP_EVENT_HEADER); stdcall;

  public
    constructor Create(); virtual;
    destructor Destroy(); override;

  end;

const
  WM_MFP_PLAYBACK_ENDED = WM_APP + 1;

var
  frmMfPlayer: TfrmMfPlayer;

  g_AppHandle: HWND;
  g_pPlayer: IMFPMediaPlayer;
  g_pPlayerCB: TMediaPlayerCallback;
  g_bHasVideo: BOOL;
  g_MoviePath: String;

procedure OnMediaItemCreated(pEvent: PMFP_MEDIAITEM_CREATED_EVENT);
procedure OnMediaItemSet(pEvent: PMFP_MEDIAITEM_SET_EVENT);
procedure ShowErrorMessage(fmt: string; hrErr: HResult);

procedure MfPlayer_AttachToWindow(Handle: HWND);
procedure MfPlayer_Paint;
procedure MfPlayer_Detach;
function MfPlayer_Play(Path: String): HRESULT;
procedure MfPlayer_Resize;

implementation

procedure MfPlayer_AttachToWindow(Handle: HWND);
begin
  if g_AppHandle = 0 then
  begin
    SetWindowLong(Handle, GWL_USERDATA, $BEEF);
    g_AppHandle := Handle;
    g_bHasVideo := False;
  end;
end;

procedure MfPlayer_Paint;
var
  ps: PAINTSTRUCT;
  whdc: HDC;
begin
  if g_AppHandle <> 0 then
  begin
    whdc := BeginPaint(g_AppHandle, ps);
    if (Assigned(g_pPlayer) and g_bHasVideo) then
      begin
        g_pPlayer.UpdateVideo();
      end
    else
      begin
        FillRect(whdc, ps.rcPaint, HBRUSH(COLOR_WINDOW + 1));
      end;
    EndPaint(whdc, ps);
  end;
end;

procedure MfPlayer_Detach;
begin
  if Assigned(g_pPlayer) then
  begin
    begin
      g_pPlayer.Stop();
      g_pPlayer.Shutdown();
      g_pPlayer := Nil;
    end;
   if Assigned(g_pPlayerCB) then
    begin
      FreeAndNil(g_pPlayerCB);
    end;
  end;
  g_AppHandle := 0;
end;

function MfPlayer_Play(Path: String): HRESULT;
var
  hr: HResult;
label
  done;
begin
  if g_AppHandle <> 0 then
  begin
    if not Assigned(g_pPlayer) then
      begin
        g_pPlayerCB := TMediaPlayerCallback.Create();
        if not Assigned(g_pPlayerCB) then
          begin
            hr := E_OUTOFMEMORY;
            goto done;
          end;
        hr := MFPCreateMediaPlayer(Nil,            // Mediafile path
                                   False,          // Start playback automatically?
                                   0,              // Flags
                                   g_pPlayerCB,    // Callback pointer
                                   g_AppHandle,    // Video window
                                   g_pPlayer       // The player
                                   );
        if Failed(hr) then
          goto done;
      end;
    hr := g_pPlayer.CreateMediaItemFromURL(PWideChar(Path),
                                          False,
                                          0,
                                          Nil);
  done:
    Result := hr;
  end;
end;

procedure MfPlayer_Resize;
var
  whdc: HDC;
  ps: PAINTSTRUCT;
begin
  if Assigned(g_pPlayer) then
  begin
    whdc := BeginPaint(g_AppHandle, ps);
    // Resize the video.
    g_pPlayer.UpdateVideo();
    {void} EndPaint(whdc, ps);
  end;
end;

procedure TfrmMfPlayer.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := False;
  MfPlayer_Detach;
  CanClose := True;
end;

procedure TfrmMfPlayer.FormCreate(Sender: TObject);
begin
  MfPlayer_AttachToWindow(Handle);
end;

procedure TfrmMfPlayer.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_ESCAPE then
  begin
    Close;
  end;
end;

procedure TfrmMfPlayer.FormPaint(Sender: TObject);
begin
  MfPlayer_Paint;
end;

procedure TfrmMfPlayer.FormShow(Sender: TObject);
begin
  if (MoviePath <> '') and not g_bHasVideo then
  begin
    g_bHasVideo := True;
    MfPlayer_Play(MoviePath);
  end;
end;

class function TfrmMfPlayer.PlayMovie(Path: String): Integer;
var
  f: TfrmMfPlayer;
begin
  f := TfrmMfPlayer.Create(nil);
  try
    f.MoviePath := Path;
    Result := f.ShowModal;
  finally
    f.Free;
  end;
end;

procedure TfrmMfPlayer.WMSize(var Msg: TMessage);
var
  whdc: HDC;
  ps: PAINTSTRUCT;

begin
  Inherited;  // OnResize method will be handled first
  if (Msg.wParam = SIZE_RESTORED) then
  begin
    MfPlayer_Resize;
  end;
end;

constructor TMediaPlayerCallback.Create();
begin
  inherited Create();
end;

destructor TMediaPlayerCallback.Destroy();
begin
  inherited Destroy();
end;

procedure TMediaPlayerCallback.OnMediaPlayerEvent(var pEventHeader: MFP_EVENT_HEADER);
begin
  if Failed(pEventHeader.hrEvent) then
    begin
      ShowErrorMessage('Playback error', pEventHeader.hrEvent);
      Exit;
    end;

  case (pEventHeader.eEventType) of
    MFP_EVENT_TYPE_MEDIAITEM_CREATED:
      begin
        OnMediaItemCreated(MFP_GET_MEDIAITEM_CREATED_EVENT(@pEventHeader));
      end;

    MFP_EVENT_TYPE_MEDIAITEM_SET:
      begin
        OnMediaItemSet(MFP_GET_MEDIAITEM_SET_EVENT(@pEventHeader));
      end;

    MFP_EVENT_TYPE_PLAYBACK_ENDED:
    begin
      if g_AppHandle <> 0 then
      begin
        PostMessage(GetParent(g_AppHandle), WM_MFP_PLAYBACK_ENDED, 0, 0);
      end;
    end;
  end;
end;

procedure OnMediaItemCreated(pEvent: PMFP_MEDIAITEM_CREATED_EVENT);
var
  hr: HResult;
  bHasVideo,
  bIsSelected: BOOL;

label
  done;

begin
  hr := S_OK;
  if Assigned(g_pPlayer) then
    begin
      bHasVideo := False;
      bIsSelected := False;
      hr := pEvent.pMediaItem.HasVideo(bHasVideo, bIsSelected);
      if Failed(hr) then goto done;
      g_bHasVideo := bHasVideo and bIsSelected;
      hr := g_pPlayer.SetMediaItem(pEvent.pMediaItem);
    end;

done:
  if Failed(hr) then
    ShowErrorMessage('Error playing this file.', hr);

end;


procedure OnMediaItemSet(pEvent: PMFP_MEDIAITEM_SET_EVENT);
var
  hr: HResult;

begin
  hr := g_pPlayer.Play();

  if Failed(hr) then
    ShowErrorMessage('IMFPMediaPlayer.Play failed.', hr);
end;


procedure ShowErrorMessage(fmt: string; hrErr: HResult);
var
  msg: string;

begin
  msg := Format('%s Resultcode: (%d)', [fmt, hrErr]);

  MessageBox(0,
             LPCWSTR(msg),
             LPCWSTR('Error'),
             MB_ICONERROR);
end;

{$R *.dfm}

initialization
begin
  // Initialize Media Foundation platform
  if Succeeded(MFStartup(MF_VERSION)) then
    CoInitializeEx(Nil,
                   COINIT_APARTMENTTHREADED or COINIT_DISABLE_OLE1DDE)
  else
    Abort();
end;


finalization
begin
  // Shutdown MF
  MFShutdown();
  CoUninitialize();
end;

end.
