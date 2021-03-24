  unit D3DRenderer;

interface

uses
  SysUtils,
  Winapi.Windows, Winapi.Messages,
  Winapi.D3D11, Winapi.DXGI, Winapi.D3DCommon, Winapi.DXTypes, Winapi.DXGIFormat, Winapi.DXGIType,
  D3DShader, D3DMesh;

type
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

      FShader: TDXTextureShader;
      FQuad: TDXModel;

      FTexture: ID3D11Texture2D;
      FTextureSRV: ID3D11ShaderResourceView;

      Function Initialize(aHWND: HWND; aWidth, aHeight: Integer; bWindowed: Boolean): HRESULT;
      Function Uninitialize: HRESULT;
      Function InitializeTexture(aWidth, aHeight: Integer): HRESULT;
    public
      Constructor Create(aHWND: HWND; aWidth, aHeight: Integer; bWindowed: Boolean);
      Destructor Destroy; override;

      Procedure UpdateTexture(Data: Pointer; Stride: Cardinal); overload;
      Procedure UpdateTexture(Data: Pointer; Stride: Cardinal; Rect: TRect); overload;
      Function Clear(aColor: TFourSingleArray): HRESULT;
      Function Render: HRESULT;
      Function Present: HRESULT;
  end;

implementation

uses IOUtils, LogFile;

var
  vertex_shader: string = 'struct VSInput'#13#10 +
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
    '    PSInput output;    '#13#10 +
    '    output.position = input.position;'#13#10 +
    '    output.texcoords = float2(input.texcoords.x, 1.0f - input.texcoords.y);'#13#10 +
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
    '	return DiffuseMap.Sample(SampleType, vs_out.texcoords); '#13#10 +
    '}'#13#10;

  fragment_shader2: string = 'Texture2D DiffuseMap;'#13#10 +
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

function TDXRenderer.Initialize(aHWND: HWND; aWidth, aHeight: Integer; bWindowed: Boolean): HRESULT;
var
  feature_level: Array[0..0] of TD3D_FEATURE_LEVEL;
  pBackbuffer: ID3D11Texture2D;

  swapchain_desc: DXGI_SWAP_CHAIN_DESC;
  rast_state_desc: TD3D11_RASTERIZER_DESC;
begin
  If FReady then Begin
    Result := Uninitialize;
    If Failed(Result) then Exit;
  end;

  ZeroMemory(@swapchain_desc, sizeof(swapchain_desc));
  With swapchain_desc do Begin
    BufferCount := 2;

    BufferDesc.Width := aWidth;
    BufferDesc.Height := aHeight;
    BufferDesc.Format := DXGI_FORMAT_R8G8B8A8_UNORM;
    BufferDesc.RefreshRate.Numerator := 60;
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
      0,
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

  FShader := TDXTextureShader.Create(
      FDevice,
      vertex_shader,
      fragment_shader2
  );

  Result := InitializeTexture(aWidth, aHeight);
  If Failed(Result) then
  begin
    Log.Log('D3D', 'Failed to initialize texture (%.8X)', [Result]);
  end;

  Result := FShader.SetTexture(FDeviceContext, FTextureSRV);
  If Failed(Result) then
  begin
    Log.Log('D3D', 'Failed to set texture to shader (%.8X)', [Result]);
  end;

  Result := FShader.Activate(FDeviceContext);
  If Failed(Result) then
  begin
    Log.Log('D3D', 'Failed to activate shader (%.8X)', [Result]);
  end;

  FReady := True;
end;

function TDXRenderer.Uninitialize: HRESULT;
begin
  If not FReady then
     Exit(E_FAIL);

  FQuad.Free;
  FShader.Free;

  FRasterizerState := nil;
  FRenderTargetView := nil;
  FDeviceContext := nil;
  FDevice := nil;

  FSwapchain := nil;

  FTexture := nil;
  FTextureSRV := nil;

  FReady := False;

  Result := S_OK;
end;

function TDXRenderer.InitializeTexture(aWidth, aHeight: Integer): HRESULT;
var
  desc: TD3D11_TEXTURE2D_DESC;
  srv_desc: TD3D11_SHADER_RESOURCE_VIEW_DESC;
begin
  With desc do Begin
    Width := aWidth;
    Height := aHeight;
    MipLevels := 0;
    ArraySize := 1;
    Format :=  DXGI_FORMAT_R16_UNORM;// // DXGI_FORMAT_B5G6R5_UNORM;
    SampleDesc.Count := 1;
    SampleDesc.Quality := 0;
    Usage := D3D11_USAGE_DEFAULT;
    BindFlags := Ord(D3D11_BIND_SHADER_RESOURCE) or Ord(D3D11_BIND_RENDER_TARGET);
    CPUAccessFlags := 0;
    MiscFlags := Ord(D3D11_RESOURCE_MISC_GENERATE_MIPS);
  End;

  FDevice.CreateTexture2D(desc, nil, FTexture);

  With srv_desc do Begin
    Format := desc.Format;
    ViewDimension := D3D11_SRV_DIMENSION_TEXTURE2D;
    Texture2D.MostDetailedMip := 0;
    Texture2D.MipLevels := 1;
  End;

  Result := FDevice.CreateShaderResourceView(FTexture, @srv_desc, FTextureSRV);
end;

procedure TDXRenderer.UpdateTexture(Data: Pointer; Stride: Cardinal);
begin
  FDeviceContext.UpdateSubresource(FTexture, 0, nil, data, stride, 0);
end;

procedure TDXRenderer.UpdateTexture(Data: Pointer; Stride: Cardinal; Rect: TRect);
var
  Box: D3D11_BOX;
begin
  Box.left := Rect.Left;
  Box.top := Rect.Top;
  Box.right := Rect.Right;
  Box.bottom := Rect.Bottom;
  Box.front := 0;
  Box.back := 1;
  FDeviceContext.UpdateSubresource(FTexture, 0, nil, data, stride, 0);
end;

constructor TDXRenderer.Create(aHWND: HWND; aWidth, aHeight: Integer; bWindowed: Boolean);
begin
  Inherited Create;

  FReady := False;
  FEnableVSync := False;

  If Failed(Initialize(aHWND, aWidth, aHeight, bWindowed)) then
     Raise Exception.Create('Direct3D 11 initialization failed');
end;

destructor TDXRenderer.Destroy;
begin
  Uninitialize;
  Inherited;
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
begin
  If not FReady then Begin
    Result := E_FAIL;
    Exit;
  End;

  Result := FQuad.Render(FDeviceContext);
end;

function TDXRenderer.Present: HRESULT;
begin
  If not FReady then Begin
    Result := E_FAIL;
    Exit;
  End;

  If FEnableVSync then Begin
    FSwapchain.Present(1, 0);
  end else Begin
    FSwapchain.Present(0, 0);
  end;

  Result := S_OK;
end;

end.

