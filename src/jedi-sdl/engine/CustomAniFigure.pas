unit CustomAniFigure;

interface

uses
  sdl;

type
  TCustomAniFigure = class;
  
  TCustomAniResource = class( TObject)
  public
    procedure Render( Figure : TCustomAniFigure ); virtual; abstract;
    procedure EnumLightSource( Figure : TCustomAniFigure; Index, X, Y, Z : longint; Intensity : double; Radius : integer ); virtual; abstract;
    procedure Draw( Canvas : PSDL_Surface; X, Y : Integer; Frame : Word ); virtual; abstract;
    procedure FreeResources; virtual; abstract;
  end;

  TCustomAniFigure = class( TObject )
  private
    FResource: TCustomAniResource;
  protected
    procedure SetResource( const Value : TCustomAniResource ); virtual;
  public
    procedure Render; virtual;
    procedure EnumLightSource( Index, X, Y, Z : longint; Intensity : double; Radius : integer ); virtual;
    procedure DoFrame; virtual;
    property Resource : TCustomAniResource read FResource write SetResource;
  end;

implementation

{ TCustomAniFigure }

procedure TCustomAniFigure.DoFrame;
begin

end;

procedure TCustomAniFigure.EnumLightSource(Index, X, Y, Z: Integer; Intensity: double; Radius: integer);
begin
  if Assigned( FResource ) then
    FResource.EnumLightSource( Self, Index, X, Y, Z, Intensity, Radius );
end;

procedure TCustomAniFigure.Render;
begin
  if Assigned( FResource ) then
    FResource.Render( Self );
end;

procedure TCustomAniFigure.SetResource(const Value: TCustomAniResource);
begin
  FResource := Value;
end;

end.

