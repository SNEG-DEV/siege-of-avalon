unit AbstractMousePtr;

interface

type
  TAbstractMousePtr = class (TObject)
  protected
    procedure SetPlotDirty(const Value: Boolean); virtual; abstract;
    function GetPlotDirty: Boolean; virtual; abstract;
    procedure SetEnabled(const Value: Boolean); virtual; abstract;
    function GetEnabled: Boolean; virtual; abstract;
  public
    procedure SetFrame(Frame: Integer); virtual; abstract;
    procedure Cleanup; virtual; abstract;
    property PlotDirty: Boolean read GetPlotDirty write SetPlotDirty;
    property Enabled: Boolean read GetEnabled write SetEnabled;
  end;

implementation

end.
