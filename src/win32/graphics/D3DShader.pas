unit D3DShader;

interface

uses
  Winapi.Windows, SysUtils,
  Winapi.D3D10, Winapi.D3D11, Winapi.DXGI, Winapi.D3DCommon, Winapi.DXTypes, Winapi.DXGIFormat, Winapi.DXGIType,
  Winapi.D3DX10, Winapi.D3DCompiler;

type
  { TDXAbstractShader }
  TDXAbstractShader = class
    private
      Function DumpErrorMessages(aFilename: string; pErrorBuffer: ID3D10Blob): HRESULT;
      Function Initialize(pDevice: ID3D11Device; aPSName, aVSName: string; aPSSource, aVSSource: string): HRESULT;
      Function Uninitialize: HRESULT;
    protected
      FDevice: ID3D11Device;
      FInputLayout: ID3D11InputLayout;

      FVS: ID3D11VertexShader;
      FPS: ID3D11PixelShader;

      FLayoutArray: Array[0..100] of TD3D11_INPUT_ELEMENT_DESC;
      FLayoutCount: Integer;

      //Following methods should be overriden by successor classes
      Function DecideInputLayout: HRESULT; virtual;
      Function OnInitialize: HRESULT; virtual;
      Function OnUninitialize: HRESULT; virtual;
      Function OnActivate(pDeviceContext: ID3D11DeviceContext): HRESULT; virtual;
    public
      Constructor Create(pDevice: ID3D11Device; aVSSource, aPSSource: string);
      Destructor Destroy; override;

      Function Activate(pDC: ID3D11DeviceContext): HRESULT;
  end;

  PDXTextureShaderCB = ^TDXTextureShaderCB;
  TDXTextureShaderCB = record
    proj, view, model: TD3DMATRIX;
  end;

  { TDXTextureShader }
  TDXTextureShader = class(TDXAbstractShader)
    protected
      FConstantBuffer: ID3D11Buffer;
      FSamplerState: ID3D11SamplerState;

      //Sets number/names/types of the generic attributes
      Function DecideInputLayout: HRESULT; override;
      Function OnInitialize: HRESULT; override;
      Function OnUninitialize: HRESULT; override;
      Function OnActivate(pDeviceContext: ID3D11DeviceContext): HRESULT; override;
    public
      OutputTransform, InputTransform: TD3DMatrix;
      Function SetTexture(pDC: ID3D11DeviceContext; pTexture: ID3D11ShaderResourceView): HRESULT;
  end;

implementation

uses LogFile;

{ TDXTextureShader }

function TDXTextureShader.DecideInputLayout: HRESULT;
begin
  FLayoutCount := 2;

  FLayoutArray[0].SemanticName := 'POSITION';
  FLayoutArray[0].SemanticIndex := 0;
  FLayoutArray[0].Format := DXGI_FORMAT_R32G32B32_FLOAT;
  FLayoutArray[0].InputSlot := 0;
  FLayoutArray[0].AlignedByteOffset := 0;
  FLayoutArray[0].InputSlotClass := D3D11_INPUT_PER_VERTEX_DATA;
  FLayoutArray[0].InstanceDataStepRate := 0;

  FLayoutArray[1].SemanticName := 'TEXCOORD';
  FLayoutArray[1].SemanticIndex := 0;
  FLayoutArray[1].Format := DXGI_FORMAT_R32G32_FLOAT;
  FLayoutArray[1].InputSlot := 0;
  FLayoutArray[1].AlignedByteOffset := D3D11_APPEND_ALIGNED_ELEMENT;
  FLayoutArray[1].InputSlotClass := D3D11_INPUT_PER_VERTEX_DATA;
  FLayoutArray[1].InstanceDataStepRate := 0;

  Result := S_OK;
end;

function TDXTextureShader.OnInitialize: HRESULT;
var
  buffer_desc: TD3D11_BUFFER_DESC;
  sampler_desc: TD3D11_SAMPLER_DESC;
begin
  ZeroMemory(@OutputTransform, SizeOf(OutputTransform));
  OutputTransform._11 := 1.0;
  OutputTransform._22 := 1.0;
  OutputTransform._33 := 1.0;
  OutputTransform._44 := 1.0;

  ZeroMemory(@InputTransform, SizeOf(InputTransform));
  InputTransform._11 := 1.0;
  InputTransform._22 := 1.0;
  InputTransform._33 := 1.0;
  InputTransform._44 := 1.0;

  With buffer_desc do Begin
    Usage := D3D11_USAGE_DYNAMIC;
    ByteWidth := 128;
    BindFlags := Ord(D3D11_BIND_CONSTANT_BUFFER);
    CPUAccessFlags := Ord(D3D11_CPU_ACCESS_WRITE);
    MiscFlags := 0;
    StructureByteStride := 0;
  End;

  Result := FDevice.CreateBuffer(buffer_desc, nil, FConstantBuffer);

  if Failed(Result) then
  begin
    Exit;
  end;


  //Set up sampler state desc
  With sampler_desc do Begin
    Filter := D3D11_FILTER_MIN_MAG_MIP_LINEAR;
    AddressU := D3D11_TEXTURE_ADDRESS_WRAP;
    AddressV := D3D11_TEXTURE_ADDRESS_WRAP;
    AddressW := D3D11_TEXTURE_ADDRESS_WRAP;
    MipLODBias := 0;
    MaxAnisotropy := 1;
    ComparisonFunc := D3D11_COMPARISON_ALWAYS;
    BorderColor[0] := 0;
    BorderColor[1] := 0;
    BorderColor[2] := 0;
    BorderColor[3] := 0;
    MinLOD := 0;
    MaxLOD := D3D11_FLOAT32_MAX;
  End;

  Result := FDevice.CreateSamplerState(sampler_desc, FSamplerState);
end;

function TDXTextureShader.OnUninitialize: HRESULT;
begin
  FConstantBuffer := nil;
  FSamplerState := nil;
  Result := S_OK;
end;

function TDXTextureShader.OnActivate(
  pDeviceContext: ID3D11DeviceContext): HRESULT;
type
  BufType = Array[0..1] of TD3DMatrix;
var
  mapped_res: TD3D11_MAPPED_SUBRESOURCE;
  buf: ^BufType;
begin
  Result := pDeviceContext.Map(FConstantBuffer, 0, D3D11_MAP_WRITE_DISCARD, 0, mapped_res);
  If Failed(Result) then
  begin
    Exit;
  end;

  buf := mapped_res.pData;
  buf^[0] := OutputTransform;
  buf^[1] := InputTransform;
  pDeviceContext.Unmap(FConstantBuffer, 0);

  pDeviceContext.VSSetConstantBuffers(0, 1, FConstantBuffer);
  pDeviceContext.PSSetSamplers(0, 1, FSamplerState);

  Result := S_OK;
end;

function TDXTextureShader.SetTexture(pDC: ID3D11DeviceContext;
  pTexture: ID3D11ShaderResourceView): HRESULT;
begin
  pDC.PSSetShaderResources(0, 1, pTexture);
  Result := S_OK;
end;

{ TDXAbstractShader }

function TDXAbstractShader.DumpErrorMessages(aFilename: string; pErrorBuffer: ID3D10Blob): HRESULT;
var
  error: string;
begin
  error := PAnsiChar(pErrorBuffer.GetBufferPointer());
  Log.Log('D3DShader', '%s', [error]);
end;

function TDXAbstractShader.Initialize(pDevice: ID3D11Device; aPSName,
  aVSName: string; aPSSource, aVSSource: string): HRESULT;
var
  pPSBlob, pVSBlob: ID3D10Blob;
  pErrorMsgs: ID3D10Blob;
begin
  Result := D3DCompile(
    PAnsiChar(AnsiString(aVSSource)),
    Length(aVSSource),
    PAnsiChar(AnsiString(aVSName)),
    nil, nil,
    'VSEntry',
    'vs_4_0',
    D3D10_SHADER_ENABLE_STRICTNESS,
    0,
    pVSBlob,
    pErrorMsgs);

  If Failed(Result) then Begin
    If pErrorMsgs = nil then Begin
      OutputDebugString(PChar(Format('Shader file "%s" not found.', [aVSName])));
      Exit;
  End;
    DumpErrorMessages('errors-vs.txt', pErrorMsgs);
    OutputDebugString(PChar(Format('Failed to compile vertex shader "%s". See file "errors-vs.txt" for more details.', [aVSName])));
    Exit;
  End;

  Result := D3DCompile(
      PAnsiChar(AnsiString(aPSSource)),
      Length(aPSSource),
      PAnsiChar(AnsiString(aPSName)),
      nil,
      nil,
      'PSEntry',
      'ps_4_0',
      D3D10_SHADER_ENABLE_STRICTNESS,
      0,
      pPSBlob,
      pErrorMsgs
  );

  If Failed(Result) then Begin
    If pErrorMsgs = nil then Begin
      OutputDebugString(PChar(Format('Shader file "%s" not found.', [aVSName])));
      Exit;
    End;
    DumpErrorMessages('errors-ps.txt', pErrorMsgs);
    OutputDebugString(PChar(Format('Failed to compile pixel shader "%s". See file "errors-ps.txt" for more details.', [aPSName])));
    Exit;
  End;

  Result := pDevice.CreateVertexShader(pVSBlob.GetBufferPointer(), pVSBlob.GetBufferSize(), nil, @FVS);
  If Failed(Result) then Exit;

  Result := pDevice.CreatePixelShader(pPSBlob.GetBufferPointer(), pPSBlob.GetBufferSize(), nil, FPS);
  If Failed(Result) then Exit;

  Result := DecideInputLayout;
  If Failed(Result) then Exit;

  Result := pDevice.CreateInputLayout(
      @FLayoutArray[0],
      FLayoutCount,
      pVSBlob.GetBufferPointer(),
      pVSBlob.GetBufferSize(),
      FInputLayout
  );
  If Failed(Result) then Exit;

  pVSBlob := nil;
  pPSBlob := nil;

  FDevice := pDevice;

  Result := OnInitialize;
end;

function TDXAbstractShader.Uninitialize: HRESULT;
begin
  FInputLayout := nil;
  FVS := nil;
  FPS := nil;

  Result := OnUninitialize;
  FDevice := nil;
end;

function TDXAbstractShader.DecideInputLayout: HRESULT;
begin
  Result := E_NOTIMPL;
end;

function TDXAbstractShader.OnInitialize: HRESULT;
begin
  Result := S_OK;
end;

function TDXAbstractShader.OnUninitialize: HRESULT;
begin
  Result := S_OK;
end;

function TDXAbstractShader.OnActivate(
  pDeviceContext: ID3D11DeviceContext): HRESULT;
begin
  If pDeviceContext = nil then Begin End; //Avoid hint
  Result := S_OK;
end;

constructor TDXAbstractShader.Create(pDevice: ID3D11Device; aVSSource, aPSSource: string);
begin
  Inherited Create();

  If Failed(Initialize(pDevice, 'default.ps', 'default.vs', aPSSource, aVSSource)) then
    Raise Exception.Create('Failed to initialize shader(aVSSource).');
end;

destructor TDXAbstractShader.Destroy;
begin
  Uninitialize;

  inherited Destroy;
end;

function TDXAbstractShader.Activate(pDC: ID3D11DeviceContext): HRESULT;
begin
  pDC.IASetInputLayout(FInputLayout);
  pDC.VSSetShader(FVS, nil, 0);
  pDC.PSSetShader(FPS, nil, 0);
  Result := OnActivate(pDC);
end;

end.

