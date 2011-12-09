unit CustomAniResource;

interface

uses
  CustomAniFigure;

type
  TCustomAniResource = class( TObject)
  public
    procedure Render( Figure : TCustomAniFigure ); virtual; abstract;
    procedure EnumLightSource( Figure : TCustomAniFigure; Index, X, Y, Z : longint; Intensity : double; Radius : integer ); virtual;
    procedure Draw( Canvas : PSDL_Surface; X, Y : Integer; Frame : Word ); virtual; abstract;
    procedure FreeResources; virtual; abstract;
  end;

implementation

end.
 