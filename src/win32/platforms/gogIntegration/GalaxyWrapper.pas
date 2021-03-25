unit GalaxyWrapper;

interface

const
  WRAPPERLIB = 'CGalaxy.dll';


type TGalaxyApps = record
  IsDlcInstalled : function( productID : int64) : longBool; cdecl;
end;

type PGalaxyApps = ^TGalaxyApps;

type tGalaxyStats = record
  RequestUserStatsAndAchievements : procedure (userID: int64); cdecl;
  GetStatInt : function( name : pAnsiChar; userID : int64) : integer; cdecl;
  GetStatFloat : function( name : pAnsiChar; userID : int64) : Single; cdecl;
  SetStatInt : procedure ( name : pAnsiChar; value : integer); cdecl;
  SetStatFloat : procedure ( name : pAnsiChar; value : single); cdecl;
  UpdateAvgRateStat :  procedure ( name : pAnsiChar; countThisSession : single; sessionLength : double); cdecl;
  GetAchievement :  procedure ( name : pAnsiChar; unlocked : longbool; unlockTime : integer; userID : int64); cdecl;
  SetAchievement : procedure ( name : pAnsiChar); cdecl;
  ClearAchievement : procedure ( name : pAnsiChar); cdecl;
  StoreStatsAndAchievements : procedure; cdecl;
  ResetStatsAndAchievements : procedure; cdecl;
  GetAchievementDisplayName : function(name : pAnsiChar) : pAnsiChar; cdecl;
  GetAchievementDisplayNameCopy : procedure ( name : pAnsiChar; buffer :  pAnsiChar; bufferLength : integer); cdecl;
  GetAchievementDescription : function ( name : pAnsiChar) : pAnsiChar; cdecl;
  GetAchievementDescriptionCopy : procedure ( name : pAnsiChar; buffer : pAnsiChar; bufferLength : integer); cdecl;
  IsAchievementVisible : function ( name : pAnsiChar) : longbool; cdecl;
  RequestLeaderboards : procedure; cdecl;
  GetLeaderboardDisplayName : function ( name : pAnsiChar) : pAnsiChar; cdecl;
  GetLeaderboardDisplayNameCopy : procedure ( name : pAnsiChar; buffer: pAnsiChar; bufferLength : integer); cdecl;
  GetLeaderboardSortMethod : function ( name : pAnsiChar): integer; cdecl;
  GetLeaderboardDisplayType : function ( name : pAnsiChar): integer; cdecl;
  RequestLeaderboardEntriesGlobal : procedure ( name : pAnsiChar; rangeStart : integer; rangeEnd: integer); cdecl;
  RequestLeaderboardEntriesAroundUser : procedure ( name : pAnsiChar ; countBefore : integer; countAfter :  integer; userID : int64); cdecl;
  RequestLeaderboardEntriesForUsers : procedure (  name : pAnsiChar; userArray : int64; userArraySize: integer); cdecl;
  GetRequestedLeaderboardEntry : procedure ( index : integer; rank : integer; score : integer; userID : int64); cdecl;
  GetRequestedLeaderboardEntryWithDetails : procedure ( index : integer; rank : integer; score : integer; details : pointer; detailsSize : integer; outDetailsSize : integer; userID : int64); cdecl;
  SetLeaderboardScore : procedure ( name : pAnsiChar; score : integer; forceUpdate : longBool); cdecl;
  SetLeaderboardScoreWithDetails : procedure ( name : pAnsiChar; score : integer; details : pointer; detailSize : integer; forceUpdate : longBool); cdecl;
  GetLeaderboardEntryCount : function ( name : pAnsiChar) : integer; cdecl;
  FindLeaderboard : procedure ( name : pAnsiChar); cdecl;
  FindOrCreateLeaderboard : procedure ( name : pAnsiChar; displayName : pAnsiChar; sortMethod : integer; displayType : integer); cdecl;
  RequestUserTimePlayed : procedure ( userID : int64); cdecl;
  GetUserTimePlayed : function (userID : int64) : integer; cdecl;
end;

type PGalaxyStats = ^TGalaxyStats;


type TGalaxyListener = record
  Register : procedure( listenerType : integer; listener : pointer; context : pointer); cdecl;
  Unregister : procedure( listenerType : integer; listener : pointer); cdecl;
end;

type PGalaxyListener = ^TGalaxyListener;

type TGalaxyAuthListener = record
  OnAuthSuccess : procedure( context : pointer); cdecl;
  OnAuthFailure : procedure( context : pointer; failureReason : integer); cdecl;
  OnAuthLost    : procedure( context : pointer); cdecl;
end;

type PGalaxyAuthListener = ^TGalaxyAuthListener;

type TGalaxyUserStatsAndAchievementsRetrieveListener = record
  OnUserStatsAndAchievementsRetrieveSuccess : procedure( context : pointer; userID : int64); cdecl;
  OnUserStatsAndAchievementsRetrieveFailure : procedure( context : pointer; userID : int64; failureReason : integer); cdecl;
end;

type PGalaxyUserStatsAndAchievementsRetrieveListener = ^TGalaxyUserStatsAndAchievementsRetrieveListener;

type TStatsAndAchievementsStoreListener = record
  OnUserStatsAndAchievementsStoreSuccess : procedure( context : pointer); cdecl;
  OnUserStatsAndAchievementsStoreFailure : procedure( context : pointer; failureReason : integer); cdecl;
end;

type PStatsAndAchievementsStoreListener = ^TStatsAndAchievementsStoreListener;

type TGalaxyUser = record
  SignedIn : function : longbool; cdecl;
  GetGalaxyID : function : int64; cdecl;
  SignInWithSteamAppTicket : procedure (steamAppTicket : pAnsiChar; steamAppTicketLength : cardinal; personaName : pAnsiChar); cdecl;
  SignInGalaxy : procedure(requireOnline : longbool; listener : PGalaxyAuthListener; context : pointer); cdecl;
  RequestUserData : procedure(userID : int64); cdecl;
  GetUserData : function(key : pAnsiChar; userID : int64): pAnsiChar; cdecl;
  GetUserDataCopy : procedure( key : pAnsiChar; buffer : pAnsiChar; bufferLength : integer; userID : int64); cdecl;
  SetUserData : procedure(key : pAnsiChar; value : pAnsiChar); cdecl;
  GetUserDataCount : function(userID : int64) : integer; cdecl;
  GetUserDataByIndex : function(index : integer; key : pAnsiChar; keyLength : integer; value : pAnsiChar; valueLength : integer; userID : int64) : longbool; cdecl;
  DeleteUserData : procedure(key : pAnsiChar); cdecl;
  IsLoggedOn : function : longbool; cdecl;
  RequestEncryptedAppTicket : procedure(data : pointer; dataSize : integer); cdecl;
  GetEncryptedAppTicket : procedure ( encryptedAppTicket : pointer; maxEncryptedAppTicketLength : integer; currentEncryptedAppTicketLength : integer); cdecl;
  SignInWithServerKey : procedure (serverKey : pAnsiChar); cdecl;
  GetAccessToken : function : pAnsiChar; cdecl;
  GetAccessTokenCopy : procedure (buffer : pAnsiChar; bufferLength : integer); cdecl;
  ReportInvalidAccessToken : function (accessToken : pAnsiChar) : longBool; cdecl;
end;

type PGalaxyUser = ^TGalaxyUser;

type TGalaxyApi = record
  Init : procedure(clientId : pAnsiChar; clientSecret : pAnsiChar); cdecl;
  Shutdown : procedure; cdecl;
  ProcessData : procedure; cdecl;
  User : function: PGalaxyUser; cdecl;
  Friends : function: pointer; cdecl;
	Matchmaking : function: pointer; cdecl;
	Networking : function: pointer; cdecl;
	ServerNetworking : function: pointer; cdecl;
	Stats : function: PGalaxyStats; cdecl;
	Utils : function: pointer; cdecl;
	Apps : function: PGalaxyApps; cdecl;
	ListenerRegistrar : function: PGalaxyListener; cdecl;
	Logger : function: pointer; cdecl;
end;

type PGalaxyApi = ^TGalaxyApi;


function cgGetGalaxyAPI() : PGalaxyApi; stdcall; cdecl; external WRAPPERLIB;// index 0;

implementation

end.
