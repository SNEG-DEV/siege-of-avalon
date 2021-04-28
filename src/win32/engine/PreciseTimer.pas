unit PreciseTimer;

interface

uses Winapi.Windows, Winapi.MMSystem, System.SyncObjs;

type
  TPreciseTimer = class
  private
    FEvent: THandle;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Wait(msec: Integer);
  end;

implementation

uses SysUtils;

{ TPreciseTimer }

constructor TPreciseTimer.Create;
begin
  FEvent := CreateEvent(nil, True, False, nil);
end;

destructor TPreciseTimer.Destroy;
begin
  CloseHandle(FEvent);
  inherited;
end;

procedure TPreciseTimer.Wait(msec: Integer);
begin
  if msec > 0 then
  begin
    WaitForSingleObject(FEvent, msec);
  end;
end;

end.

