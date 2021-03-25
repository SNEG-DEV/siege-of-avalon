unit GameLibIntegration.Gog;

interface

uses
  Vcl.ExtCtrls, GalaxyWrapper;

const
  client_id = '54156286736179164';
  client_secret = '8d5522db101e5363b87361c27796ecf9b1fd7f317f2068aef70549112d5767b6';


type
  TGogIntegration = class
    public
      constructor Create;
      destructor Destroy; override;
      procedure SetAchievement(ach_name : pAnsiChar);
      procedure ClearAchievement(ach_name : pAnsiChar);
      procedure RequestStats;
    private
      gga : pGalaxyApi;
      timerRunProcessData : TTimer;
      timerStoreStats : TTimer;
      NeedStoreStats,
      StatsSuccess,
      IsAuthorised,
      StoreStatsSuccess : boolean;
      procedure InitGog;
      procedure TimerRunProcessDataExec(Sender: TObject);
      procedure TimerStoreExec(Sender: TObject);
      procedure StoreStats;
    end;

var
  FGogIntegration : TGogIntegration;

implementation

procedure TGogIntegration.TimerRunProcessDataExec(Sender: TObject);
begin
  gga.ProcessData;
end;

procedure TGogIntegration.TimerStoreExec(Sender: TObject);
begin
  if NeedStoreStats then
    gga.Stats.StoreStatsAndAchievements;
end;

procedure TGogIntegration.StoreStats;
begin
  NeedStoreStats := true;
  TimerStoreExec(nil);
  timerStoreStats.Enabled := false;
end;

procedure TGogIntegration.RequestStats;
var userId : int64;
begin
  if IsAuthorised then begin
    userId := gga.User.GetGalaxyID;
    gga.Stats.RequestUserStatsAndAchievements(userId);
  end;
end;

destructor TGogIntegration.Destroy;
begin
  timerRunProcessData.Free;
  timerStoreStats.Free;
  inherited;
end;

constructor TGogIntegration.Create;
begin
  StatsSuccess := false;
  StoreStatsSuccess := false;
  NeedStoreStats := false;
  IsAuthorised := false;
  InitGog;
end;

procedure AuthSuccess(context : pointer); cdecl;
begin
  FGogIntegration.IsAuthorised := true;
  FGogIntegration.RequestStats;
end;

procedure AuthLost(context : pointer); cdecl;
begin
  FGogIntegration.IsAuthorised := false;
end;


procedure AuthFailure(context : pointer; failureReason : integer); cdecl;
begin
  FGogIntegration.IsAuthorised := false;
end;

procedure UserStatsAndAchievementsRetrieveSuccess( context : pointer; userID : int64); cdecl;
begin
  FGogIntegration.StatsSuccess := true;
end;

procedure UserStatsAndAchievementsRetrieveFailure( context : pointer; userID : int64; failureReason : integer); cdecl;
begin
  FGogIntegration.StatsSuccess := false;
end;

procedure UserStatsAndAchievementsStoreSuccess( context : pointer); cdecl;
begin
  FGogIntegration.StoreStatsSuccess := true;
end;

procedure UserStatsAndAchievementsStoreFailure( context : pointer; failureReason : integer); cdecl;
begin
  FGogIntegration.StoreStatsSuccess := false;
end;

procedure TGogIntegration.InitGog;
var authListener : TGalaxyAuthListener;
  statsListener : TGalaxyUserStatsAndAchievementsRetrieveListener;
  storeStatsListener : TStatsAndAchievementsStoreListener;
begin
  gga := cgGetGalaxyAPI();
  gga.Init(client_id, client_secret);

  timerRunProcessData := TTimer.Create(nil);
  timerRunProcessData.Interval := 30;
  timerRunProcessData.OnTimer := TimerRunProcessDataExec;
  timerRunProcessData.Enabled := true;

  authListener.OnAuthSuccess := AuthSuccess;
  authListener.OnAuthFailure := AuthFailure;
  authListener.OnAuthLost    := AuthLost;

  statsListener.OnUserStatsAndAchievementsRetrieveSuccess := UserStatsAndAchievementsRetrieveSuccess;
  statsListener.OnUserStatsAndAchievementsRetrieveFailure := UserStatsAndAchievementsRetrieveFailure;

  storeStatsListener.OnUserStatsAndAchievementsStoreSuccess := UserStatsAndAchievementsStoreSuccess;
  storeStatsListener.OnUserStatsAndAchievementsStoreFailure := UserStatsAndAchievementsStoreFailure;

  gga.User.SignInGalaxy(false, @authListener, Self);
  gga.ListenerRegistrar.Register( 11 , @statsListener, nil);
  gga.ListenerRegistrar.Register( 12 , @storeStatsListener, nil);

  timerStoreStats := TTimer.Create(nil);
  timerStoreStats.Interval := 120000;
  timerStoreStats.OnTimer := TimerStoreExec;
  timerStoreStats.Enabled := false;
end;

procedure TGogIntegration.SetAchievement(ach_name : pAnsiChar);
begin
  if not StatsSuccess then
    exit;
  gga.Stats.SetAchievement(ach_name);
  StoreStats;
end;

procedure TGogIntegration.ClearAchievement(ach_name : pAnsiChar);
begin
  if not StatsSuccess then
    exit;
  gga.Stats.ClearAchievement(ach_name);
  StoreStats;
end;

end.
