{$DEFINE USE_WAITABLE_TIMER}

unit PreciseTimer;

interface

uses Winapi.Windows, Winapi.MMSystem, System.SyncObjs;

type
  TPreciseTimer = class
  private
    FHandle: THandle;
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
{$IFDEF USE_WAITABLE_TIMER}
  FHandle := CreateWaitableTimer(nil, True, nil);
{$ELSE}
  FHandle := CreateEvent(nil, True, False, nil);
{$ENDIF}
end;

destructor TPreciseTimer.Destroy;
begin
  CloseHandle(FHandle);
  inherited;
end;

procedure TPreciseTimer.Wait(msec: Integer);
{$IFDEF USE_WAITABLE_TIMER}
var
  ns: Int64;
{$ENDIF}
begin
  if msec > 0 then
  begin
{$IFDEF USE_WAITABLE_TIMER}
    ns := msec * 10;
    SetWaitableTimer(FHandle, ns, 0, nil, nil, False);
    WaitForSingleObject(FHandle, INFINITE);
{$ELSE}
    WaitForSingleObject(FHandle, msec);
{$ENDIF}
  end;
end;

end.

