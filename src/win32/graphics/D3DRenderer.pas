unit D3DRenderer;

interface

uses
  SysUtils, Classes, SyncObjs, System.Generics.Collections,
  Winapi.Windows, Winapi.Messages, Vcl.Graphics,
  Winapi.D3D11, Winapi.DXGI, Winapi.D3DCommon, Winapi.DXTypes, Winapi.DXGIFormat, Winapi.DXGIType,
  D3DShader, D3DMesh, PreciseTimer;

const
  dxfmt_565: Integer = 0;
  dxfmt_r16: Integer = 1;
  dxfmt_8888: Integer = 2;
  blend_none: Integer = 0;
  blend_transparent: Integer = 1;

type
  TDXRenderer = class;
  TDXPresenterThread = class;

  TDXRenderLayer = class
  private
    FRenderer: TDXRenderer;
    FDevice: ID3D11Device;
    FDeviceContext : ID3D11DeviceContext;
    FShader: TDXTextureShader;
    FBlendState: ID3D11BlendState;
    FTexture: ID3D11Texture2D;
    FTextureSRV: ID3D11ShaderResourceView;
    FEnabled: Boolean;
    FSize, FViewportSize: TSize;

    FSourceRect, FDestRect: TRect;
  public
    Constructor Create(Renderer: TDXRenderer; aWidth, aHeight: Integer; aFormat, aBlend: Integer);
    Destructor Destroy; override;

    Procedure UpdateTexture(Data: Pointer; Stride: Cardinal); overload;
    Procedure UpdateTexture(Data: Pointer; Stride: Cardinal; Rect: TRect); overload;
    procedure Activate;
    procedure SetSourceRect(aRect: TRect);
    procedure SetDestRect(aRect: TRect);
    procedure SetPosition(aPos: TPoint);

    property Enabled: Boolean read FEnabled write FEnabled;
    property Size: TSize read FSize;

    procedure Lock;
    procedure Unlock;
  end;

  TDXRenderer = class
    private
      FDevice: ID3D11Device;
      FDeviceContext: ID3D11DeviceContext;
      FCurrentFeatureLevel: TD3D_FEATURE_LEVEL;

      FSwapchain: IDXGISwapChain;
      FRenderTargetView: ID3D11RenderTargetView;

      FRasterizerState: ID3D11RasterizerState;
      FViewport: TD3D11_VIEWPORT;

      FReady, FEnableVSync: Boolean;
      FMaxFPS: Integer;

      FQuad: TDXModel;
      FLayers: TList<TDXRenderLayer>;
      FMainLayer: TDXRenderLayer;

      FThread: TDXPresenterThread;
      FQuitThread: Boolean;

      FDeviceLock: TCriticalSection;

      Function Initialize(aHWND: HWND; aWidth, aHeight, aRefreshRate: Integer; bWindowed: Boolean): HRESULT;
      Function Uninitialize: HRESULT;
      Function InitializeTexture(aWidth, aHeight: Integer): HRESULT;
    public
      Constructor Create(aHWND: HWND; aWidth, aHeight, aRefreshRate: Integer; bWindowed, bVSync: Boolean; aMaxFPS: Integer);
      Destructor Destroy; override;

      function CreateLayer(aWidth, aHeight: Integer; Format, Blend: Integer): TDXRenderLayer;
      function CreateLayerFromFile(FilePath: string; Transparent: TColor): TDXRenderLayer;
      function CreateLayerFromBitmap(Bitmap: TBitmap; Transparent: TColor): TDXRenderLayer;
      procedure BringLayerToTheTop(aLayer: TDXRenderLayer);

      Procedure UpdateTexture(Data: Pointer; Stride: Cardinal); overload;
      Procedure UpdateTexture(Data: Pointer; Stride: Cardinal; Rect: TRect); overload;
      Function Clear(aColor: TFourSingleArray): HRESULT;
      Function Render: HRESULT;
      Function Present: HRESULT;

      procedure EnableFullscreen(Enabled: Boolean);

      procedure StartPresenterThread;
      procedure StopPresenterThread;

      property Device: ID3D11Device read FDevice;
      property DeviceContext: ID3D11DeviceContext read FDeviceContext;

      procedure Lock;
      procedure Unlock;
  end;

  TDXPresenterThread = class(TThread)
    private
      FRenderer: TDXRenderer;
      FQuit: Boolean;

      FMaxFPS: Integer;
      FTimerFreq: Double;
      FCounterStart: Int64;
      FEndOfPrevFrame: Double;
      FTimer: TPreciseTimer;

      function GetQPC: Double;
    procedure ThrottleFPS;
    public
      constructor Create(aRenderer: TDXRenderer; aMaxFPS: Integer);
      destructor Destroy; override;
      procedure Execute; override;
      procedure Stop;
  end;

implementation

uses IOUtils, LogFile, Winapi.D3DX10;

var
  vertex_shader: string = 'cbuffer MatrixBuffer'#13#10 +
    '{'#13#10 +
    '    matrix OutputPosition;'#13#10 +
    '    matrix InputPosition;'#13#10 +
    '}'#13#10 +
    'struct VSInput'#13#10 +
    '{'#13#10 +
    '    float4 position : POSITION;'#13#10 +
    '    float2 texcoords : TEXCOORD0;'#13#10 +
    '};'#13#10 +
    ''#13#10 +
    'struct PSInput'#13#10 +
    '{'#13#10 +
    '    float4 position : SV_POSITION;'#13#10 +
    '    float2 texcoords : TEXCOORD0;'#13#10 +
    '};'#13#10 +
    ''#13#10 +
    'PSInput VSEntry(VSInput input)'#13#10 +
    '{'#13#10 +
    '    PSInput output;'#13#10 +
    '    input.position.w = 1.0f;'#13#10 +
    '    output.position = mul(input.position, OutputPosition);'#13#10 +
    '    float4 tex = float4(input.texcoords.x, input.texcoords.y, 0.0f, 1.0f);'#13#10 +
    '    tex = mul(tex, InputPosition);'#13#10 +
    '    output.texcoords = float2(tex.x, 1.0f - tex.y);'#13#10 +
    '    return output;'#13#10 +
    '}'#13#10;

  fragment_shader: string = 'Texture2D DiffuseMap;'#13#10 +
    'SamplerState SampleType;'#13#10 +
    ''#13#10 +
    'struct PSInput'#13#10 +
    '{'#13#10 +
    '    float4 position : SV_POSITION;'#13#10 +
    '    float2 texcoords : TEXCOORD0;'#13#10 +
    '};'#13#10 +
    ''#13#10 +
    'float4 PSEntry(PSInput vs_out) : SV_TARGET'#13#10 +
    '{'#13#10 +
    '	float4 color = DiffuseMap.Sample(SampleType, vs_out.texcoords); '#13#10 +
//    ' if (color.a < 0.1f) discard;'#13#10 +
    '	return color.rgba; '#13#10 +
    '}'#13#10;

  fragment_shader_R16_bitwise: string = 'Texture2D DiffuseMap;'#13#10 +
    'SamplerState SampleType;'#13#10 +
    ''#13#10 +
    'struct PSInput'#13#10 +
    '{'#13#10 +
    '    float4 position : SV_POSITION;'#13#10 +
    '    float2 texcoords : TEXCOORD0;'#13#10 +
    '};'#13#10 +
    ''#13#10 +
    'float4 PSEntry(PSInput vs_out) : SV_TARGET'#13#10 +
    '{'#13#10 +
    '    float4 c = DiffuseMap.Sample(SampleType, vs_out.texcoords);'#13#10 +
    '    int c565 = c.r * 65535;'#13#10 +
    '    int r = (c565 & 0xF800) >> 11;'#13#10 +
    '    int g = (c565 & 0x7E0) >> 5;'#13#10 +
    '    int b = c565 & 0x1F;'#13#10 +
    '	return float4(r / 32.0f, g / 64.0f, b / 32.0f, 1.0f);'#13#10 +
    '}'#13#10;

  fragment_shader_R16_int: string = 'Texture2D DiffuseMap;'#13#10 +
    'SamplerState SampleType;'#13#10 +
    ''#13#10 +
    'struct PSInput'#13#10 +
    '{'#13#10 +
    '    float4 position : SV_POSITION;'#13#10 +
    '    float2 texcoords : TEXCOORD0;'#13#10 +
    '};'#13#10 +
    ''#13#10 +
    'float4 PSEntry(PSInput vs_out) : SV_TARGET'#13#10 +
    '{'#13#10 +
    '    float4 c = DiffuseMap.Sample(SampleType, vs_out.texcoords);'#13#10 +
    '    int c565 = c.r * 65535;'#13#10 +
    '    int t = c565 / 32;'#13#10 +
    '    t = t * 32;'#13#10 +
    '    int b = c565 - t;'#13#10 +
    '    c565 = (c565 - b) / 32;'#13#10 +
    '    t = c565 / 64;'#13#10 +
    '    t = t * 64;'#13#10 +
    '    int g = c565 - t;'#13#10 +
    '    c565 = (c565 - g) / 64;'#13#10 +
    '    t = c565 / 32;'#13#10 +
    '    t = t * 32;'#13#10 +
    '    int r = c565 - t;'#13#10 +
    '	return float4(r / 32.0f, g / 64.0f, b / 32.0f, 1.0f);'#13#10 +
    '}'#13#10;

function TDXRenderer.Initialize(aHWND: HWND; aWidth, aHeight, aRefreshRate: Integer; bWindowed: Boolean): HRESULT;
var
  feature_level: Array[0..0] of TD3D_FEATURE_LEVEL;
  pBackbuffer: ID3D11Texture2D;

  swapchain_desc: DXGI_SWAP_CHAIN_DESC;
  rast_state_desc: TD3D11_RASTERIZER_DESC;

  dxgidev: IDXGIDevice;
  dxgiadapter: IDXGIAdapter;
  dxgifactory: IDXGIFactory;
begin
  If FReady then Begin
    Result := Uninitialize;
    If Failed(Result) then Exit;
  end;

  FDeviceLock := TCriticalSection.Create;
  FLayers := TList<TDXRenderLayer>.Create;

  if aRefreshRate = 0 then
    aRefreshRate := 60;

  ZeroMemory(@swapchain_desc, sizeof(swapchain_desc));
  With swapchain_desc do Begin
    BufferCount := 2;

    BufferDesc.Width := aWidth;
    BufferDesc.Height := aHeight;
    BufferDesc.Format := DXGI_FORMAT_R8G8B8A8_UNORM;
    BufferDesc.RefreshRate.Numerator := aRefreshRate;
    BufferDesc.RefreshRate.Denominator := 1;
    BufferDesc.ScanlineOrdering := DXGI_MODE_SCANLINE_ORDER_UNSPECIFIED;
    BufferDesc.Scaling := DXGI_MODE_SCALING_UNSPECIFIED;
    BufferUsage := DXGI_USAGE_RENDER_TARGET_OUTPUT;
    OutputWindow := aHWND;
    SampleDesc.Count := 1;
    SampleDesc.Quality := 0;
    Windowed := bWindowed;
    SwapEffect := DXGI_SWAP_EFFECT_SEQUENTIAL;
    Flags := 0;
  End;

  feature_level[0] := D3D_FEATURE_LEVEL_10_0;

  Result := D3D11CreateDeviceAndSwapChain(
      nil,
      D3D_DRIVER_TYPE_HARDWARE,
      0,
{$IFDEF DEBUG}
      D3D11_CREATE_DEVICE_DEBUG,
{$ELSE}
      0,
{$ENDIF}
      @feature_level[0],
      1,
      D3D11_SDK_VERSION,
      @swapchain_desc,
      FSwapchain,
      FDevice,
      FCurrentFeatureLevel,
      FDeviceContext
  );
  If Failed(Result) then
  begin
    Log.Log('D3D', 'Failed to create device and swapchain (%.8X)', [Result]);
  end;

  Result := FSwapchain.GetBuffer(0, ID3D11Texture2D, pBackbuffer);
  If Failed(Result) then
  begin
    Log.Log('D3D', 'Failed get back buffer (%.8X)', [Result]);
  end;


  Result := FDevice.CreateRenderTargetView(pBackbuffer, nil, FRenderTargetView);
  If Failed(Result) then
  begin
    Log.Log('D3D', 'Failed to create render target view (%.8X)', [Result]);
  end;

  pBackbuffer := nil;

  FDeviceContext.OMSetRenderTargets(1, FRenderTargetView, nil);

  ZeroMemory(@rast_state_desc, sizeof(rast_state_desc));
  With rast_state_desc do Begin
    AntialiasedLineEnable := True;
    CullMode := D3D11_CULL_NONE;
    DepthBias := 0;
    DepthBiasClamp := 0;
    DepthClipEnable := True;
    FillMode := D3D11_FILL_SOLID;
    FrontCounterClockwise := False;
    MultisampleEnable := False;
    ScissorEnable := False;
    SlopeScaledDepthBias := 0;
  End;

  Result := FDevice.CreateRasterizerState(rast_state_desc, FRasterizerState);
  If Failed(Result) then
  begin
    Log.Log('D3D', 'Failed to create rasterizer state (%.8X)', [Result]);
  end;

  FDeviceContext.RSSetState(FRasterizerState);

  ZeroMemory(@FViewport, sizeof(FViewport));
  With FViewport do Begin
    Width := aWidth;
    Height := aHeight;
    MinDepth := 0;
    MaxDepth := 1;
    TopLeftX := 0;
    TopLeftY := 0;
  End;

  FDeviceContext.RSSetViewports(1, @FViewport);
  FQuad := TDXModel.CreateQuad(FDeviceContext);

  (*
  if FileExists('shaders\default.vs') then
  begin
    vertex_shader := TFile.ReadAllText('shaders\default.vs');
  end;

  if FileExists('shaders\default.ps') then
  begin
    fragment_shader := TFile.ReadAllText('shaders\default.ps');
  end;
  *)

  InitializeTexture(aWidth, aHeight);

  FDevice.QueryInterface(IDXGIDevice, dxgidev);
  if Assigned(dxgidev) then
  begin
    dxgidev.GetParent(IDXGIAdapter, dxgiadapter);
    if Assigned(dxgiadapter) then
    begin
      dxgiadapter.GetParent(IDXGIFactory, dxgifactory);
      if Assigned(dxgifactory) then
      begin
        dxgifactory.MakeWindowAssociation(aHWND, DXGI_MWA_NO_ALT_ENTER or DXGI_MWA_NO_WINDOW_CHANGES);
        dxgifactory := nil;
      end;
      dxgiadapter := nil;
    end;
    dxgidev := nil;
  end;
  FReady := True;
end;

function TDXRenderer.Uninitialize: HRESULT;
var
  Layer: TDXRenderLayer;
begin
  If not FReady then
     Exit(E_FAIL);

  for Layer in FLayers do
  begin
    Layer.Free;
  end;

  FLayers.Clear;

  FDeviceLock.Free;

  FSwapchain.SetFullscreenState(FALSE, nil);

  FQuad.Free;

  FRasterizerState := nil;
  FRenderTargetView := nil;
  FDeviceContext := nil;
  FDevice := nil;

  FSwapchain := nil;

  FReady := False;

  Result := S_OK;
end;

function TDXRenderer.InitializeTexture(aWidth, aHeight: Integer): HRESULT;
begin
  FMainLayer := CreateLayer(aWidth, aHeight, dxfmt_r16, blend_none);
  Result := S_OK;
end;

procedure TDXRenderer.Lock;
begin
  FDeviceLock.Enter;
end;

procedure TDXRenderer.Unlock;
begin
  FDeviceLock.Leave;
end;

procedure TDXRenderer.UpdateTexture(Data: Pointer; Stride: Cardinal);
begin
  FMainLayer.UpdateTexture(Data, Stride);
end;

procedure TDXRenderer.UpdateTexture(Data: Pointer; Stride: Cardinal; Rect: TRect);
begin
  FMainLayer.UpdateTexture(Data, Stride, Rect);
end;

constructor TDXRenderer.Create(aHWND: HWND; aWidth, aHeight, aRefreshRate: Integer; bWindowed, bVSync: Boolean; aMaxFPS: Integer);
begin
  Inherited Create;

  FReady := False;
  FEnableVSync := bVSync;
  FMaxFPS := aMaxFPS;

  If Failed(Initialize(aHWND, aWidth, aHeight, aRefreshRate, bWindowed)) then
     Raise Exception.Create('Direct3D 11 initialization failed');
end;

function TDXRenderer.CreateLayer(aWidth, aHeight,
  Format, Blend: Integer): TDXRenderLayer;
begin
  Result := TDXRenderLayer.Create(Self, aWidth, aHeight, Format, Blend);
  FLayers.Add(Result);
end;

{$POINTERMATH ON}
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
{$POINTERMATH OFF}

function TDXRenderer.CreateLayerFromBitmap(Bitmap: TBitmap; Transparent: TColor): TDXRenderLayer;
var
  Data: PDWORD;
begin
  GetMem(Data, Bitmap.Width * Bitmap.Height * 4);
  try
    UnpackBitmap(Bitmap, Data, $FF00FF);
    Result := CreateLayer(Bitmap.Width, Bitmap.Height, dxfmt_8888, blend_transparent);
    Result.UpdateTexture(data, Bitmap.Width * 4);
  finally
    FreeMem(Data);
  end;
end;

function TDXRenderer.CreateLayerFromFile(FilePath: string; Transparent: TColor): TDXRenderLayer;
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.LoadFromFile(FilePath);
    Result := CreateLayerFromBitmap(Bitmap, Transparent);
  finally
    Bitmap.Free;
  end;
end;

destructor TDXRenderer.Destroy;
begin
  StopPresenterThread;
  Uninitialize;
  Inherited;
end;

procedure TDXRenderer.EnableFullscreen(Enabled: Boolean);
var
  dxgioutput: IDXGIOutput;
begin
  if Assigned(FSwapchain) then
  begin
    FSwapchain.GetContainingOutput(dxgioutput);
    if Assigned(dxgioutput) then
    begin
      FSwapchain.SetFullscreenState(Enabled, dxgioutput);
      dxgioutput := nil;
    end;
  end;
//  FSwapchain.SetFullscreenState(Enabled, )
end;

procedure TDXRenderer.BringLayerToTheTop(aLayer: TDXRenderLayer);
begin
  if FLayers.IndexOf(aLayer) <> -1 then
  begin
    FLayers.Remove(aLayer);
    FLayers.Add(aLayer);
  end;
end;

function TDXRenderer.Clear(aColor: TFourSingleArray): HRESULT;
begin
  If not FReady then Begin
    Result := E_FAIL;
    Exit;
  end;
  FDeviceContext.ClearRenderTargetView(FRenderTargetView, aColor);
  Result := S_OK;
end;

function TDXRenderer.Render: HRESULT;
var
  Layer: TDXRenderLayer;
begin
  Lock;
  try
    FDeviceContext.OMSetRenderTargets(1, FRenderTargetView, nil);
    If not FReady then Begin
      Result := E_FAIL;
      Exit;
    End;
    for Layer in FLayers do
    begin
      if Layer.Enabled then
      begin
        Layer.Activate;
        FQuad.Render(FDeviceContext);
      end;
    end;
  finally
    Unlock;
  end;
  Result := 0;//
end;

//TThreadProc

procedure TDXRenderer.StartPresenterThread;
begin
  if not Assigned(FThread) then
  begin
    FThread := TDXPresenterThread.Create(Self, FMaxFPS);
    FThread.Start;
  end;
end;

procedure TDXRenderer.StopPresenterThread;
begin
  if Assigned(FThread) then
  begin
    FThread.Stop;
    FThread.Destroy;
  end;
end;

function TDXRenderer.Present: HRESULT;
begin
  If not FReady then Begin
    Result := E_FAIL;
    Exit;
  End;

  Lock;
  try
    If FEnableVSync then
    Begin
      FSwapchain.Present(1, 0);
    end
    else
    Begin
      FSwapchain.Present(0, 0);
    end;
  finally
    Unlock;
  end;

  Result := S_OK;
end;

{ TDXRenderLayer }

constructor TDXRenderLayer.Create(Renderer: TDXRenderer; aWidth, aHeight, aFormat, aBlend: Integer);
var
  desc: TD3D11_TEXTURE2D_DESC;
  srv_desc: TD3D11_SHADER_RESOURCE_VIEW_DESC;
  blend_desc: TD3D11_BLEND_DESC;
  Res: HRESULT;
  NumVP: Cardinal;
  VP: TD3D11_VIEWPORT;

  FragShader: String;
  PixelFormat: DXGI_FORMAT;
begin
  FRenderer := Renderer;
  FDeviceContext := FRenderer.DeviceContext;
  FDevice := FRenderer.Device;

  FSize.Width := aWidth;
  FSize.Height := aHeight;

  FEnabled := True;

  FRenderer.Lock;
  try

    NumVP := 1;
    FDeviceContext.RSGetViewports(NumVP, @VP);
    FViewportSize.Width := Trunc(VP.Width);
    FViewportSize.Height := Trunc(VP.Height);

    FBlendState := nil;
    blend_desc := TD3D11_BLEND_DESC.Create(True);
    if aBlend = blend_transparent then
    begin
      blend_desc.RenderTarget[0].BlendEnable := True;
      blend_desc.RenderTarget[0].SrcBlend := D3D11_BLEND_SRC_ALPHA;
      blend_desc.RenderTarget[0].DestBlend := D3D11_BLEND_INV_SRC_ALPHA;
      blend_desc.RenderTarget[0].BlendOp := D3D11_BLEND_OP_ADD;
      blend_desc.RenderTarget[0].SrcBlendAlpha := D3D11_BLEND_ZERO;
      blend_desc.RenderTarget[0].DestBlendAlpha := D3D11_BLEND_ZERO;
      blend_desc.RenderTarget[0].BlendOpAlpha := D3D11_BLEND_OP_ADD;
      blend_desc.RenderTarget[0].RenderTargetWriteMask :=
        Byte(D3D11_COLOR_WRITE_ENABLE_ALL);
      Res := FDevice.CreateBlendState(blend_desc, FBlendState);
    end;

    if aFormat = dxfmt_565 then
    begin
      PixelFormat := DXGI_FORMAT_B5G6R5_UNORM;
      FragShader := fragment_shader;
    end
    else if aFormat = dxfmt_r16 then
    begin
      PixelFormat := DXGI_FORMAT_R16_UNORM;
      FragShader := fragment_shader_R16_int;
    end
    else if aFormat = dxfmt_8888 then
    begin
      PixelFormat := DXGI_FORMAT_R8G8B8A8_UNORM;
      FragShader := fragment_shader;
    end;

    With desc do
    Begin
      Width := aWidth;
      Height := aHeight;
      MipLevels := 0;
      ArraySize := 1;
      Format := PixelFormat;
      SampleDesc.Count := 1;
      SampleDesc.Quality := 0;
      Usage := D3D11_USAGE_DEFAULT;
      BindFlags := Ord(D3D11_BIND_SHADER_RESOURCE) or
        Ord(D3D11_BIND_RENDER_TARGET);
      CPUAccessFlags := 0;
      MiscFlags := Ord(D3D11_RESOURCE_MISC_GENERATE_MIPS);
    End;

    FDevice.CreateTexture2D(desc, nil, FTexture);

    With srv_desc do
    Begin
      Format := desc.Format;
      ViewDimension := D3D11_SRV_DIMENSION_TEXTURE2D;
      Texture2D.MostDetailedMip := 0;
      Texture2D.MipLevels := 1;
    End;

    FDevice.CreateShaderResourceView(FTexture, @srv_desc, FTextureSRV);

    FShader := TDXTextureShader.Create(FDevice, vertex_shader, FragShader);

    Res := FShader.SetTexture(FDeviceContext, FTextureSRV);

    If Failed(Res) then
    begin
      Log.Log('D3D', 'Failed to set texture to shader (%.8X)', [Res]);
    end;

  finally
    FRenderer.Unlock;
  end;

//  Res := FShader.Activate(FDeviceContext);
//  If Failed(Res) then
//  begin
//    Log.Log('D3D', 'Failed to activate shader (%.8X)', [Res]);
//  end;

end;

destructor TDXRenderLayer.Destroy;
begin
  FShader.Free;
  FTexture := nil;
  FTextureSRV := nil;
  FBlendState := nil;
  inherited;
end;

procedure TDXRenderLayer.Lock;
begin
  FRenderer.Lock;
end;

procedure TDXRenderLayer.SetDestRect(aRect: TRect);
var
  l, t, r, b: Single;
  T1, S, T2, Temp, RR: TD3DMatrix;
  H: Integer;
begin
  H := aRect.Top;
  aRect.Top := FViewportSize.Height - aRect.Bottom;
  aRect.Bottom := FViewportSize.Height - H;
  l := aRect.Left/FViewportSize.Width*2 - 1.0;
  t := aRect.Top/FViewportSize.Height*2 - 1.0;
  r := aRect.Right/FViewportSize.Width*2 - 1.0;
  b := aRect.Bottom/FViewportSize.Height*2 - 1.0;

  D3DXMatrixTranslation(T1, l, t, 0);
  D3DXMatrixScaling(S, (r - l)/2, (b - t)/2, 1.0);
  D3DXMatrixTranslation(T2, 1, 1, 0);
  D3DXMatrixMultiply(Temp, T2, S);
  D3DXMatrixMultiplyTranspose(RR, Temp, T1);
  Lock;
  FShader.OutputTransform := RR;
  Unlock;
end;

procedure TDXRenderLayer.SetPosition(aPos: TPoint);
var
  rc: TRect;
begin
  rc.TopLeft := aPos;
  rc.Size := FSize;
  SetDestRect(rc);
end;

procedure TDXRenderLayer.SetSourceRect(aRect: TRect);
var
  T1, S, T2, Temp, R: TD3DMatrix;
begin
  D3DXMatrixTranslation(T1, aRect.Left/FSize.Width, aRect.Top/FSize.Height, 0);
  D3DXMatrixScaling(S, aRect.Width/FSize.Width, aRect.Height/FSize.Height, 1.0);
  D3DXMatrixMultiplyTranspose(Temp, S, T1);
  Lock;
  FShader.InputTransform := Temp;
  Unlock;
end;

procedure TDXRenderLayer.Activate;
var
  f: TFourSingleArray;
begin
  f[0] := 0;
  f[1] := 0;
  f[2] := 0;
  f[3] := 0;
  FDeviceContext.OMSetBlendState(FBlendState, f, $FFFFFFFF);
  FShader.SetTexture(FDeviceContext, FTextureSRV);
  FShader.Activate(FDeviceContext);
end;

procedure TDXRenderLayer.UpdateTexture(Data: Pointer; Stride: Cardinal);
begin
  Lock;
  try
    FDeviceContext.UpdateSubresource(FTexture, 0, nil, data, stride, 0);
  finally
    Unlock;
  end;
end;

procedure TDXRenderLayer.Unlock;
begin
  FRenderer.Unlock;
end;

procedure TDXRenderLayer.UpdateTexture(Data: Pointer; Stride: Cardinal;
  Rect: TRect);
var
  Box: D3D11_BOX;
begin
  Box.left := Rect.Left;
  Box.top := Rect.Top;
  Box.right := Rect.Right;
  Box.bottom := Rect.Bottom;
  Box.front := 0;
  Box.back := 1;
  Lock;
  try
    FDeviceContext.UpdateSubresource(FTexture, 0, @Box, data, stride, 0);
  finally
    Unlock;
  end;
end;

{ TDXPresenterThread }

constructor TDXPresenterThread.Create(aRenderer: TDXRenderer; aMaxFPS: Integer);
var
  freq: Int64;
begin
  inherited Create(True);
  FTimer := TPreciseTimer.Create;
  FreeOnTerminate := False;
  FRenderer := aRenderer;
  FQuit := False;
  FMaxFPS := aMaxFPS;
  QueryPerformanceFrequency(freq);
  FTimerFreq := freq/1000.0;
  FEndOfPrevFrame := 0;
  QueryPerformanceCounter(FCounterStart);
end;

destructor TDXPresenterThread.Destroy;
begin
  FTimer.Destroy;
  Stop;
end;

function TDXPresenterThread.GetQPC: Double;
var
  a: Int64;
begin
  QueryPerformanceCounter(a);
  Result := a/FTimerFreq;
end;

procedure TDXPresenterThread.Execute;
begin
  while not FQuit do
  begin
    FRenderer.Render;
    FRenderer.Present;
    if FMaxFPS > 0 then
    begin
      ThrottleFPS;
    end
    else
    begin
      Sleep(1);
    end;
  end;
end;

procedure TDXPresenterThread.Stop;
begin
  FQuit := True;
  WaitFor;
end;

procedure TDXPresenterThread.ThrottleFPS;
var
  FrameTime: Double;
  CurrentTime: Double;
  TimeToWait: Double;
  I: Integer;
begin
  if FEndOfPrevFrame <> 0 then
  begin
    FrameTime := 1000.0 / FMaxFPS;
    CurrentTime := GetQPC;
    TimeToWait := FrameTime - (CurrentTime - FEndOfPrevFrame);
    FTimer.Wait(Trunc(TimeToWait) - 1);
    Sleep(1);
    FEndOfPrevFrame := CurrentTime;
  end
  else
  begin
    FEndOfPrevFrame := GetQPC;
  end;
end;

end.
