unit D3DMesh;

interface

uses
  Winapi.Windows, SysUtils,
  Winapi.D3D11, Winapi.DXGI, Winapi.D3DCommon, Winapi.DXTypes, Winapi.DXGIFormat, Winapi.DXGIType,
  Winapi.D3DX10;

type
  TDXVertex = record
    position: TD3DXVECTOR3;
    texcoords: TD3DXVECTOR2;
  end;

  { TDXModel }

  TDXModel = class
    private
      { Number of entries in index buffer }
      FIndexCount: Integer;

      { Vertex and index buffers for model }
      FVertexBuffer,
      FIndexBuffer: ID3D11Buffer;

      Function Initialize(pDevice: ID3D11Device; aVertexBufferSize, aIndexBufferSize: Integer): HRESULT;
      Function Uninitialize: HRESULT;
    public
      Constructor CreateQuad(pDeviceContext: ID3D11DeviceContext);
      Destructor Destroy; override;

      Function Render(pDeviceContext: ID3D11DeviceContext): HRESULT;
  end;

  Function D3DColor4f(r, g, b, a: single): TD3DXVECTOR4;
  Function D3DColor3f(r, g, b: single): TD3DXVECTOR3;

  Function D3DVector2f(x, y: single): TD3DXVECTOR2;
  Function D3DVector3f(x, y, z: single): TD3DXVECTOR3;
  Function D3DVector4f(x, y, z, w: single): TD3DXVECTOR4;

  Function D3DXVector3f(x, y, z: Single): TD3DXVECTOR3;

implementation

function D3DColor4f(r, g, b, a: single): TD3DXVECTOR4;
begin
  Result.x := r;
  Result.y := g;
  Result.z := b;
  Result.w := a;
end;

function D3DColor3f(r, g, b: single): TD3DXVECTOR3;
begin
  Result.x := r;
  Result.y := g;
  Result.z := b;
end;

function D3DVector2f(x, y: single): TD3DXVECTOR2;
begin
  Result.x := x;
  Result.y := y;
end;

function D3DVector3f(x, y, z: single): TD3DXVECTOR3;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;

function D3DVector4f(x, y, z, w: single): TD3DXVECTOR4;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
  Result.w := w;
end;

function D3DXVector3f(x, y, z: Single): TD3DXVECTOR3;
begin
  Result.x := x;
  Result.y := y;
  Result.z := z;
end;

{ TDXModel }

function TDXModel.Initialize(pDevice: ID3D11Device; aVertexBufferSize,
  aIndexBufferSize: Integer): HRESULT;
var
  vert_buffer_desc,
  index_buffer_desc: TD3D11_BUFFER_DESC;
begin
  With vert_buffer_desc do Begin
    Usage := D3D11_USAGE_DYNAMIC;
    ByteWidth := aVertexBufferSize;
    BindFlags := Ord(D3D11_BIND_VERTEX_BUFFER);
    CPUAccessFlags := Ord(D3D11_CPU_ACCESS_WRITE);
    MiscFlags := 0;
    StructureByteStride := 0;
  End;

  Result := pDevice.CreateBuffer(vert_buffer_desc, nil, FVertexBuffer);
  If Failed(Result) then Exit;

  With index_buffer_desc do Begin
    Usage := D3D11_USAGE_DYNAMIC;
    ByteWidth := aIndexBufferSize;
    BindFlags := Ord(D3D11_BIND_INDEX_BUFFER);
    CPUAccessFlags := Ord(D3D11_CPU_ACCESS_WRITE);
    MiscFlags := 0;
    StructureByteStride := 0;
  End;

  Result := pDevice.CreateBuffer(index_buffer_desc, nil, FIndexBuffer);
end;

function TDXModel.Uninitialize: HRESULT;
begin
  FVertexBuffer := nil;
  FIndexBuffer := nil;

  Result := S_OK;
end;

constructor TDXModel.CreateQuad(pDeviceContext: ID3D11DeviceContext);
var
  pDevice: ID3D11Device;
  mapped_res: TD3D11_MAPPED_SUBRESOURCE;

  vertices: Array[0..3] of TDXVertex;
  indices: Array[0..6] of Word;
begin
  Inherited Create;

  pDeviceContext.GetDevice(pDevice);

  If Failed(Initialize(pDevice, SizeOf(vertices), SizeOf(indices))) then
     Raise Exception.Create('Failed to create mesh.');

  pDeviceContext.Map(FVertexBuffer, 0, D3D11_MAP_WRITE_DISCARD, 0, mapped_res);

  Try
    //Generate quad
    vertices[0].position := D3DVector3f(-1.0, -1.0, 0.0);
    vertices[1].position := D3DVector3f( 1.0,  1.0, 0.0);
    vertices[2].position := D3DVector3f(-1.0,  1.0, 0.0);
    vertices[3].position := D3DVector3f( 1.0, -1.0, 0.0);

    vertices[0].texcoords := D3DVector2f(0, 0);
    vertices[1].texcoords := D3DVector2f(1, 1);
    vertices[2].texcoords := D3DVector2f(0, 1);
    vertices[3].texcoords := D3DVector2f(1, 0);

    Move(vertices[0], mapped_res.pData^, SizeOf(vertices));
  Finally
    pDeviceContext.Unmap(FVertexBuffer, 0);
  End;

  pDeviceContext.Map(FIndexBuffer, 0, D3D11_MAP_WRITE_DISCARD, 0, mapped_res);

  Try
    indices[0] := 0;
    indices[1] := 2;
    indices[2] := 1;
    indices[3] := 0;
    indices[4] := 1;
    indices[5] := 3;
    Move(indices[0], mapped_res.pData^, SizeOf(indices));
  Finally
    pDeviceContext.Unmap(FIndexBuffer, 0);
  End;

  FIndexCount := 6;
end;

destructor TDXModel.Destroy;
begin
  Uninitialize;
  inherited Destroy;
end;

function TDXModel.Render(pDeviceContext: ID3D11DeviceContext): HRESULT;
var
  stride, offset: UINT;
begin
  stride := SizeOf(TDXVertex);
  offset := 0;

  pDeviceContext.IASetVertexBuffers(0, 1, FVertexBuffer, @stride, @offset);
  pDeviceContext.IASetIndexBuffer(FIndexBuffer, DXGI_FORMAT_R16_UINT, 0);
  pDeviceContext.IASetPrimitiveTopology(D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST);
  pDeviceContext.DrawIndexed(FIndexCount, 0, 0);

  Result := S_OK;
end;

end.

