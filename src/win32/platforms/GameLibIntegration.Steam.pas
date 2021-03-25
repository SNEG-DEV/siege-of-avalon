unit GameLibIntegration.Steam;

interface

uses
  Vcl.ExtCtrls, Steamworks, SteamworksClasses, SteamworksTypes;

type
  TSteamIntegration = class
    public
      constructor Create;
      destructor Destroy; override;
      procedure SetAchievement(ach_name : pAnsiChar);
      procedure ClearAchievement(ach_name : pAnsiChar);
    private
      timerRunCallback : TTimer;
      timerStoreStats : TTimer;
      StatsRecieved,
      NeedStoreStats : boolean;
      SteamCallbacks: TSteamCallbacks;
      procedure InitSteam;
      procedure TimerRunCallbackExec(Sender: TObject);
      procedure TimerStoreExec(Sender: TObject);
      procedure StoreStats;
    end;

var
  FSteamIntegration : TSteamIntegration;

procedure UserStatsRecieved(GameID: uint64; eResult: TResult; UserSteamID: TSteamID); cdecl;
procedure UserStatsStored(GameID: uint64; eResult: integer); cdecl;

implementation

procedure TSteamIntegration.TimerRunCallbackExec(Sender: TObject);
begin
  System_RunCallbacks;
end;

procedure TSteamIntegration.TimerStoreExec(Sender: TObject);
begin
  if NeedStoreStats then
    SteamUserStats_StoreStats;
end;

procedure TSteamIntegration.StoreStats;
begin
  NeedStoreStats := true;
  TimerStoreExec(nil);
  timerStoreStats.Enabled := false;
end;

destructor TSteamIntegration.Destroy;
begin
  timerRunCallback.Free;
  timerStoreStats.Free;
  inherited;
end;

constructor TSteamIntegration.Create;
begin
  StatsRecieved := false;
  NeedStoreStats := false;
  InitSteam;
end;

procedure TSteamIntegration.InitSteam;
begin
  System_InitWrapper;
  SteamCallbacks.OnUserStatsReceived := UserStatsRecieved;
  SteamCallbacks.OnUserStatsStored := UserStatsStored;
  System_RegisterCallbacks(SteamCallbacks);

  timerRunCallback := TTimer.Create(nil);
  timerRunCallback.Interval := 30;
  timerRunCallback.OnTimer := TimerRunCallbackExec;
  timerRunCallback.Enabled := true;

  timerStoreStats := TTimer.Create(nil);
  timerStoreStats.Interval := 120000;
  timerStoreStats.OnTimer := TimerStoreExec;
  timerStoreStats.Enabled := false;

  SteamUserStats_RequestCurrentStats;
end;

procedure TSteamIntegration.SetAchievement(ach_name : pAnsiChar);
begin
  if not StatsRecieved then
    exit;
  SteamUserStats_SetAchievement(ach_name);
  StoreStats;
end;

procedure TSteamIntegration.ClearAchievement(ach_name : pAnsiChar);
begin
  if not StatsRecieved then
    exit;
  SteamUserStats_ClearAchievement(ach_name);
  StoreStats;
end;

procedure UserStatsRecieved(GameID: uint64; eResult: TResult; UserSteamID: TSteamID);
begin
  FSteamIntegration.StatsRecieved := true;
end;

procedure UserStatsStored(GameID: uint64; eResult: integer); cdecl;
begin
  FSteamIntegration.NeedStoreStats := false;
end;

end.
