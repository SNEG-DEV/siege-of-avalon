//----------------------------------------------------
// © 2015 Andrey Volia
// 
// License: MIT
// Site: https://github.com/voliaandrey/steamwrapper
//----------------------------------------------------
unit SteamworksTypes;

{$IFDEF FPC}
 {$MODE DELPHI}
{$ENDIF}

interface

{$REGION 'Base types'}
Type
  Int8 = shortint;
  pInt8 = ^Int8;
  int16 = SmallInt;
  pInt16 = ^int16;
  int32 = integer;
  pInt32 = ^int32;
  uint8 = Byte;
  pUInt8 = ^uint8;
  uint16 = Word;
  pUInt16 = ^uint16;
  uint32 = Cardinal;
  pUInt32 = ^uint32;
  puint64 = ^uint64;
  TSalt = uint8;
  TGID = uint64;
  // this is baked into client messages and interfaces as an int,
  // make sure we never break this.
  TPackageId = uint32;
  // For convenience, we define a number of types that are just new names for GIDs
  TJobID = TGID;
  // Each Job has a unique ID
  TtxnID = TGID; // Each financial transaction has a unique ID
  // this is baked into client messages and interfaces as an int,
  // make sure we never break this.
  TAppId = uint32;
  TAssetClassId = uint64;
  TPhysicalItemId = uint32;
  // this is baked into client messages and interfaces as an int,
  // make sure we never break this.  AppIds and DepotIDs also presently
  // share the same namespace, but since we'd like to change that in the future
  // I've defined it seperately here.
  TDepotId = uint32;
  // RTime32
  // We use this 32 bit time representing real world time.
  // It offers 1 second resolution beginning on January 1, 1970 (Unix time)
  RTime32 = uint32;
  TCellID = uint32;
  // handle to a Steam API call
  TSteamAPICall = uint64;
  TAccountID = uint32;
  TPartnerId = uint32;
  // ID for a depot content manifest
  TManifestId = uint64;
{$ENDREGION}
{$REGION 'Base constants'}
const
  // maximum number of characters a lobby metadata key can be
  MaxLobbyKeyLength = 255;
  k_cubSaltSize: integer = 8;
  TGIDNil: TGID = $FFFFFFFF;
  k_TxnIDNil = $FFFFFFFF;
  k_TxnIDUnknown: TGID = 0;
  k_uPackageIdFreeSub: TPackageId = $0;
  k_uPackageIdInvalid: TPackageId = $FFFFFFFF;
  k_uAppIdInvalid: TAppId = $0;
  k_ulAssetClassIdInvalid: TAssetClassId = $0;
  k_uPhysicalItemIdInvalid: TPhysicalItemId = $0;
  k_uDepotIdInvalid: TDepotId = $0;
  k_uCellIDInvalid: TCellID = $FFFFFFFF;
  k_uAPICallInvalid: TSteamAPICall = $0;
  k_uPartnerIdInvalid: TPartnerId = 0;
  k_uManifestIdInvalid: TManifestId = 0;
  k_cchPublishedDocumentTitleMax = 128 + 1;
  k_cchPublishedDocumentDescriptionMax = 8000;
  k_cchPublishedDocumentChangeDescriptionMax = 8000;
  k_unEnumeratePublishedFilesMaxResults = 50;
  k_cchTagListMax = 1024 + 1;
  k_ccFilenameMax = 260;
  k_cchPublishedFileURLMax = 256;
  k_cbMaxGameServerGameDir = 32;
  k_cbMaxGameServerMapName = 32;
  k_cbMaxGameServerGameDescription = 64;
  k_cbMaxGameServerName = 64;
  k_cbMaxGameServerTags = 128;
  k_cbMaxGameServerGameData = 2048;
  k_cchStatNameMax = 128;
  k_cchLeaderboardNameMax = 128;
  k_cLeaderboardDetailsMax = 64;
  k_cubAppProofOfPurchaseKeyMax = 64;
  k_cchMaxRichPresenceKeys = 20;
  k_cchMaxRichPresenceKeyLength = 64;
  k_cchMaxRichPresenceValueLength = 256;
  // maximum length of friend group name (not including terminating nul)
  k_cchMaxFriendsGroupName = 64;
  // maximum number of groups a single user is allowed
  k_cFriendsGroupLimit = 100;
  k_cEnumerateFollowersMax = 50;
{$ENDREGION}

{$REGION 'Common types'}

Type
  // Steam universes.  Each universe is a self-contained Steam instance.
  TUniverse = (
   k_EUniverseInvalid = 0,
   k_EUniversePublic = 1,
   k_EUniverseBeta = 2,
   k_EUniverseInternal = 3,
   k_EUniverseDev = 4,
   // k_EUniverseRC = 5,				// no such universe anymore
   k_EUniverseMax
  );

  // Steam API call failure results
  TSteamAPICallFailure = (
   k_ESteamAPICallFailureNone = -1,                  // no failure
   k_ESteamAPICallFailureSteamGone = 0,              // the local Steam process has gone away
   k_ESteamAPICallFailureNetworkFailure = 1,         // the network connection to Steam has been broken, or was already broken
                                                     // SteamServersDisconnected callback will be sent around the same time
                                                     // SteamServersConnected will be sent when the client is able to talk to the Steam servers again
   k_ESteamAPICallFailureInvalidHandle = 2,          // the SteamAPICall_t handle passed in no longer exists
   k_ESteamAPICallFailureMismatchedCallback = 3      // GetAPICallResult() was called with the wrong callback type for this API call
  );

  // Input modes for the Big Picture gamepad text entry
  TGamepadTextInputMode = (
   k_EGamepadTextInputModeNormal = 0,
   k_EGamepadTextInputModePassword = 1
  );

  // Controls number of allowed lines for the Big Picture gamepad text entry
  TGamepadTextInputLineMode = (
   k_EGamepadTextInputLineModeSingleLine = 0,
   k_EGamepadTextInputLineModeMultipleLines = 1
  );

  // General result codes
  TResult = (
   k_EResultOK = 1, 												  // success
   k_EResultFail = 2, 												// generic failure
   k_EResultNoConnection = 3, 								// no/failed network connection
   // k_EResultNoConnectionRetry = 4,			  	// OBSOLETE - removed
   k_EResultInvalidPassword = 5, 							// password/ticket is invalid
   k_EResultLoggedInElsewhere = 6, 				    // same user logged in elsewhere
   k_EResultInvalidProtocolVer = 7, 				  // protocol version is incorrect
   k_EResultInvalidParam = 8, 								// a parameter is incorrect
   k_EResultFileNotFound = 9, 								// file was not found
   k_EResultBusy = 10, 												// called method busy - action not taken
   k_EResultInvalidState = 11, 								// called object was in an invalid state
   k_EResultInvalidName = 12, 								// name is invalid
   k_EResultInvalidEmail = 13, 								// email is invalid
   k_EResultDuplicateName = 14, 							// name is not unique
   k_EResultAccessDenied = 15, 								// access is denied
   k_EResultTimeout = 16,										  // operation timed out
   k_EResultBanned = 17, 											// VAC2 banned
   k_EResultAccountNotFound = 18,						  // account not found
   k_EResultInvalidSteamID = 19, 							// steamID is invalid
   k_EResultServiceUnavailable = 20,				  // The requested service is currently unavailable
   k_EResultNotLoggedOn = 21,								  // The user is not logged on
   k_EResultPending = 22, 										// Request is pending (may be in process, or waiting on third party)
   k_EResultEncryptionFailure = 23,				    // Encryption or Decryption failed
   k_EResultInsufficientPrivilege = 24, 			// Insufficient privilege
   k_EResultLimitExceeded = 25, 							// Too much of a good thing
   k_EResultRevoked = 26, 										// Access has been revoked (used for revoked guest passes)
   k_EResultExpired = 27, 										// License/Guest pass the user is trying to access is expired
   k_EResultAlreadyRedeemed = 28, 						// Guest pass has already been redeemed by account, cannot be acked again
   k_EResultDuplicateRequest = 29,						// The request is a duplicate and the action has already occurred in the past, ignored this time
   k_EResultAlreadyOwned = 30, 								// All the games in this guest pass redemption request are already owned by the user
   k_EResultIPNotFound = 31, 									// IP address not found
   k_EResultPersistFailed = 32, 							// failed to write change to the data store
   k_EResultLockingFailed = 33,               // failed to acquire access lock for this operation
   k_EResultLogonSessionReplaced = 34,
   k_EResultConnectFailed = 35,
   k_EResultHandshakeFailed = 36,
   k_EResultIOFailure = 37,
   k_EResultRemoteDisconnect = 38,
   k_EResultShoppingCartNotFound = 39,        // failed to find the shopping cart requested
   k_EResultBlocked = 40,                     // a user didn't allow it
   k_EResultIgnored = 41,                     // target is ignoring sender
   k_EResultNoMatch = 42,                     // nothing matching the request found
   k_EResultAccountDisabled = 43,
   k_EResultServiceReadOnly = 44,             // this service is not accepting content changes right now
   k_EResultAccountNotFeatured = 45,          // account doesn't have value, so this feature isn't available
   k_EResultAdministratorOK = 46,             // allowed to take this action, but only because requester is admin
   k_EResultContentVersion = 47,              // A Version mismatch in content transmitted within the Steam protocol.
   k_EResultTryAnotherCM = 48,                // The current CM can't service the user making a request, user should try another.
   k_EResultPasswordRequiredToKickSession= 49,// You are already logged in elsewhere, this cached credential login has failed.
   k_EResultAlreadyLoggedInElsewhere = 50,    // You are already logged in elsewhere, you must wait
   k_EResultSuspended = 51,                   // Long running operation (content download) suspended/paused
   k_EResultCancelled = 52,                   // Operation canceled (typically by user: content download)
   k_EResultDataCorruption = 53,              // Operation canceled because data is ill formed or unrecoverable
   k_EResultDiskFull = 54,                    // Operation canceled - not enough disk space.
   k_EResultRemoteCallFailed = 55,            // an remote call or IPC call failed
   k_EResultPasswordUnset = 56,               // Password could not be verified as it's unset server side
   k_EResultExternalAccountUnlinked = 57,     // External account (PSN, Facebook...) is not linked to a Steam account
   k_EResultPSNTicketInvalid = 58,            // PSN ticket was invalid
   k_EResultExternalAccountAlreadyLinked = 59,// External account (PSN, Facebook...) is already linked to some other account, must explicitly request to replace/delete the link first
   k_EResultRemoteFileConflict = 60,          // The sync cannot resume due to a conflict between the local and remote files
   k_EResultIllegalPassword = 61,             // The requested new password is not legal
   k_EResultSameAsPreviousValue = 62,         // new value is the same as the old one ( secret question and answer )
   k_EResultAccountLogonDenied = 63,          // account login denied due to 2nd factor authentication failure
   k_EResultCannotUseOldPassword = 64,        // The requested new password is not legal
   k_EResultInvalidLoginAuthCode = 65,        // account login denied due to auth code invalid
   k_EResultAccountLogonDeniedNoMail = 66,    // account login denied due to 2nd factor auth failure - and no mail has been sent
   k_EResultHardwareNotCapableOfIPT = 67,
   k_EResultIPTInitError = 68,
   k_EResultParentalControlRestricted = 69,   // operation failed due to parental control restrictions for current user
   k_EResultFacebookQueryError = 70,          // Facebook query returned an error
   k_EResultExpiredLoginAuthCode = 71,        // account login denied due to auth code expired
   k_EResultIPLoginRestrictionFailed = 72,
   k_EResultAccountLockedDown = 73,
   k_EResultAccountLogonDeniedVerifiedEmailRequired = 74,
   k_EResultNoMatchingURL = 75,
   k_EResultBadResponse = 76,                 // parse failure, missing field, etc.
   k_EResultRequirePasswordReEntry = 77,      // The user cannot complete the action until they re-enter their password
   k_EResultValueOutOfRange = 78,             // the value entered is outside the acceptable range
   k_EResultUnexpectedError = 79,             // something happened that we didn't expect to ever happen
   k_EResultDisabled = 80,                    // The requested service has been configured to be unavailable
   k_EResultInvalidCEGSubmission = 81,        // The set of files submitted to the CEG server are not valid !
   k_EResultRestrictedDevice = 82,            // The device being used is not allowed to perform this action
   k_EResultRegionLocked = 83,                // The action could not be complete because it is region restricted
   k_EResultRateLimitExceeded = 84,           // Temporary rate limit exceeded, try again later, different from k_EResultLimitExceeded which may be permanent
   k_EResultAccountLoginDeniedNeedTwoFactor = 85, // Need two-factor code to login
   k_EResultItemDeleted = 86,                     // The thing we're trying to access has been deleted
   k_EResultAccountLoginDeniedThrottle = 87,      // login attempt failed, try to throttle response to possible attacker
   k_EResultTwoFactorCodeMismatch = 88,           // two factor code mismatch
   k_EResultTwoFactorActivationCodeMismatch = 89  // activation code for two-factor didn't match
  );
  pResult = ^TResult;

  // Error codes for use with the voice functions
  TVoiceResult = (k_EVoiceResultOK = 0, k_EVoiceResultNotInitialized = 1, k_EVoiceResultNotRecording = 2, k_EVoiceResultNoData = 3, k_EVoiceResultBufferTooSmall = 4, k_EVoiceResultDataCorrupted = 5, k_EVoiceResultRestricted = 6, k_EVoiceResultUnsupportedCodec = 7);

  // Result codes to GSHandleClientDeny/Kick
  TDenyReason = (k_EDenyInvalid = 0, k_EDenyInvalidVersion = 1, k_EDenyGeneric = 2, k_EDenyNotLoggedOn = 3, k_EDenyNoLicense = 4, k_EDenyCheater = 5, k_EDenyLoggedInElseWhere = 6, k_EDenyUnknownText = 7, k_EDenyIncompatibleAnticheat = 8, k_EDenyMemoryCorruption = 9, k_EDenyIncompatibleSoftware = 10, k_EDenySteamConnectionLost = 11, k_EDenySteamConnectionError = 12, k_EDenySteamResponseTimedOut = 13, k_EDenySteamValidationStalled = 14, k_EDenySteamOwnerLeftGuestUser = 15);

  // return type of GetAuthSessionTicket
type
  TAuthTicket = uint32;

const
  k_HAuthTicketInvalid: TAuthTicket = 0;

Type
  // results from BeginAuthSession
  TBeginAuthSessionResult = (k_EBeginAuthSessionResultOK = 0, // Ticket is valid for this game and this steamID.
    k_EBeginAuthSessionResultInvalidTicket = 1,    // Ticket is not valid.
    k_EBeginAuthSessionResultDuplicateRequest = 2, // A ticket has already been submitted for this steamID
    k_EBeginAuthSessionResultInvalidVersion = 3,   // Ticket is from an incompatible interface version
    k_EBeginAuthSessionResultGameMismatch = 4,     // Ticket is not for this game
    k_EBeginAuthSessionResultExpiredTicket = 5     // Ticket has expired
    );

  // Callback values for callback ValidateAuthTicketResponse_t which is a response to BeginAuthSession
  TAuthSessionResponse = (k_EAuthSessionResponseOK = 0, // Steam has verified the user is online, the ticket is valid and ticket has not been reused.
    k_EAuthSessionResponseUserNotConnectedToSteam = 1,  // The user in question is not connected to steam
    k_EAuthSessionResponseNoLicenseOrExpired = 2,       // The license has expired.
    k_EAuthSessionResponseVACBanned = 3,                // The user is VAC banned for this game.
    k_EAuthSessionResponseLoggedInElseWhere = 4,        // The user account has logged in elsewhere and the session containing the game instance has been disconnected.
    k_EAuthSessionResponseVACCheckTimedOut = 5,         // VAC has been unable to perform anti-cheat checks on this user
    k_EAuthSessionResponseAuthTicketCanceled = 6,       // The ticket has been canceled by the issuer
    k_EAuthSessionResponseAuthTicketInvalidAlreadyUsed = 7, // This ticket has already been used, it is not valid.
    k_EAuthSessionResponseAuthTicketInvalid = 8,            // This ticket is not from a user instance currently connected to steam.
    k_EAuthSessionResponsePublisherIssuedBan = 9            // The user is banned for this game. The ban came via the web api and not VAC
    );

  // results from UserHasLicenseForApp
  TUserHasLicenseForAppResult = (
    k_EUserHasLicenseResultHasLicense = 0, // User has a license for specified app
    k_EUserHasLicenseResultDoesNotHaveLicense = 1, // User does not have a license for the specified app
    k_EUserHasLicenseResultNoAuth = 2 // User has not been authenticated
    );

  // Steam account types
  TAccountType = (k_EAccountTypeInvalid = 0, k_EAccountTypeIndividual = 1, // single user account
    k_EAccountTypeMultiseat = 2, // multiseat (e.g. cybercafe) account
    k_EAccountTypeGameServer = 3, // game server account
    k_EAccountTypeAnonGameServer = 4, // anonymous game server account
    k_EAccountTypePending = 5, // pending
    k_EAccountTypeContentServer = 6, // content server
    k_EAccountTypeClan = 7, k_EAccountTypeChat = 8, k_EAccountTypeConsoleUser = 9, // Fake SteamID for local PSN account on PS3 or Live account on 360, etc.
    k_EAccountTypeAnonUser = 10,
    // Max of 16 items in this field
    k_EAccountTypeMax);

  // -----------------------------------------------------------------------------
  // Purpose:
  // -----------------------------------------------------------------------------
  TAppReleaseState = (k_EAppReleaseState_Unknown = 0, // unknown, required appinfo or license info is missing
    k_EAppReleaseState_Unavailable = 1, // even if user 'just' owns it, can see game at all
    k_EAppReleaseState_Prerelease = 2, // can be purchased and is visible in games list, nothing else. Common appInfo section released
    k_EAppReleaseState_PreloadOnly = 3, // owners can preload app, not play it. AppInfo fully released.
    k_EAppReleaseState_Released = 4 // owners can download and play app.
    );

  // -----------------------------------------------------------------------------
  // Purpose:
  // -----------------------------------------------------------------------------
  TAppOwnershipFlags = (k_EAppOwnershipFlags_None = $0000, // unknown
    k_EAppOwnershipFlags_OwnsLicense = $0001, // owns license for this game
    k_EAppOwnershipFlags_FreeLicense = $0002, // not paid for game
    k_EAppOwnershipFlags_RegionRestricted = $0004, // owns app, but not allowed to play in current region
    k_EAppOwnershipFlags_LowViolence = $0008, // only low violence version
    k_EAppOwnershipFlags_InvalidPlatform = $0010, // app not supported on current platform
    k_EAppOwnershipFlags_SharedLicense = $0020, // license was granted by authorized local device
    k_EAppOwnershipFlags_FreeWeekend = $0040, // owned by a free weekend licenses
    k_EAppOwnershipFlags_RetailLicense = $0080, // has a retail license for game, (CD-Key etc)
    k_EAppOwnershipFlags_LicenseLocked = $0100, // shared license is locked (in use) by other user
    k_EAppOwnershipFlags_LicensePending = $0200, // owns app, but transaction is still pending. Can't install or play
    k_EAppOwnershipFlags_LicenseExpired = $0400, // doesn't own app anymore since license expired
    k_EAppOwnershipFlags_LicensePermanent = $0800, // permanent license, not borrowed, or guest or freeweekend etc
    k_EAppOwnershipFlags_LicenseRecurring = $000, // Recurring license, user is charged periodically
    k_EAppOwnershipFlags_LicenseCanceled = $2000 // Mark as canceled, but might be still active if recurring
    );

  // -----------------------------------------------------------------------------
  // Purpose: designed as flags to allow filters masks
  // -----------------------------------------------------------------------------
{$WARN BOUNDS_ERROR OFF}
  // Should be implemented as record consts
  TAppType = (k_EAppType_Invalid = $000, // unknown / invalid
    k_EAppType_Game = $001, // playable game, default type
    k_EAppType_Application = $002, // software application
    k_EAppType_Tool = $004, // SDKs, editors & dedicated servers
    k_EAppType_Demo = $008, // game demo
    k_EAppType_Media_DEPRECATED = $010, // legacy - was used for game trailers, which are now just videos on the web
    k_EAppType_DLC = $020, // down loadable content
    k_EAppType_Guide = $040, // game guide, PDF etc
    k_EAppType_Driver = $080, // hardware driver updater (ATI, Razor etc)
    k_EAppType_Config = $00, // hidden app used to config Steam features (backpack, sales, etc)
    k_EAppType_Film = $200, // A Movie (feature film)
    k_EAppType_TVSeries = $400, // A TV or other video series which will have episodes and perhaps seasons
    k_EAppType_Video = $800, // A video component of either a Film or TVSeries (may be the feature, an episode, preview, making-of, etc)
    k_EAppType_Plugin = $000, // Plug-in types for other Apps
    k_EAppType_Music = $2000, // Music files

    k_EAppType_Shortcut = $40000000, // just a shortcut, client side only
    k_EAppType_DepotOnly = $80000000 // placeholder since depots and apps share the same namespace
    );
{$WARN BOUNDS_ERROR ON}

  // -----------------------------------------------------------------------------
  // types of user game stats fields
  // WARNING: DO NOT RENUMBER EXISTING VALUES - STORED IN DATABASE
  // -----------------------------------------------------------------------------
  TSteamUserStatType = (k_ESteamUserStatTypeINVALID = 0, k_ESteamUserStatTypeINT = 1, k_ESteamUserStatTypeFLOAT = 2,
    // Read as FLOAT, set with count / session length
    k_ESteamUserStatTypeAVGRATE = 3, k_ESteamUserStatTypeACHIEVEMENTS = 4, k_ESteamUserStatTypeGROUPACHIEVEMENTS = 5,
    // max, for sanity checks
    k_ESteamUserStatTypeMAX);

  // -----------------------------------------------------------------------------
  // Purpose: Chat Entry Types (previously was only friend-to-friend message types)
  // -----------------------------------------------------------------------------
  TChatEntryType = (k_EChatEntryTypeInvalid = 0, k_EChatEntryTypeChatMsg = 1, // Normal text message from another user
    k_EChatEntryTypeTyping = 2, // Another user is typing (not used in multi-user chat)
    k_EChatEntryTypeInviteGame = 3, // Invite from other user into that users current game
    k_EChatEntryTypeEmote = 4, // text emote message (deprecated, should be treated as ChatMsg)
    // k_EChatEntryTypeLobbyGameStart = 5,	// lobby game is starting (dead - listen for LobbyGameCreated_t callback instead)
    k_EChatEntryTypeLeftConversation = 6, // user has left the conversation ( closed chat window )
    // Above are previous FriendMsgType entries, now merged into more generic chat entry types
    k_EChatEntryTypeEntered = 7, // user has entered the conversation (used in multi-user chat and group chat)
    k_EChatEntryTypeWasKicked = 8, // user was kicked (data: 64-bit steamid of actor performing the kick)
    k_EChatEntryTypeWasBanned = 9, // user was banned (data: 64-bit steamid of actor performing the ban)
    k_EChatEntryTypeDisconnected = 10, // user disconnected
    k_EChatEntryTypeHistoricalChat = 11, // a chat message from user's chat history or offilne message
    k_EChatEntryTypeReserved1 = 12, k_EChatEntryTypeReserved2 = 13);

  // -----------------------------------------------------------------------------
  // Purpose: Chat Room Enter Responses
  // -----------------------------------------------------------------------------
  TChatRoomEnterResponse = (k_EChatRoomEnterResponseSuccess = 1, // Success
    k_EChatRoomEnterResponseDoesntExist = 2, // Chat doesn't exist (probably closed)
    k_EChatRoomEnterResponseNotAllowed = 3, // General Denied - You don't have the permissions needed to join the chat
    k_EChatRoomEnterResponseFull = 4, // Chat room has reached its maximum size
    k_EChatRoomEnterResponseError = 5, // Unexpected Error
    k_EChatRoomEnterResponseBanned = 6, // You are banned from this chat room and may not join
    k_EChatRoomEnterResponseLimited = 7, // Joining this chat is not allowed because you are a limited user (no value on account)
    k_EChatRoomEnterResponseClanDisabled = 8, // Attempt to join a clan chat when the clan is locked or disabled
    k_EChatRoomEnterResponseCommunityBan = 9, // Attempt to join a chat when the user has a community lock on their account
    k_EChatRoomEnterResponseMemberBlockedYou = 10, // Join failed - some member in the chat has blocked you from joining
    k_EChatRoomEnterResponseYouBlockedMember = 11 // Join failed - you have blocked some member already in the chat
    );

  TLegacyKeyRegistration = procedure(CDKey, InstallPath: pAnsiChar);
  TLegacyKeyInstalled = procedure();

const
  k_unSteamAccountIDMask: NativeUInt = $FFFFFFFF;

const
  k_unSteamAccountInstanceMask: NativeUInt = $000FFFFF;

  // we allow 3 simultaneous user account instances right now, 1= desktop, 2 = console, 4 = web, 0 = all
const
  k_unSteamUserDesktopInstance: NativeUInt = 1;

const
  k_unSteamUserConsoleInstance: NativeUInt = 2;

const
  k_unSteamUserWebInstance: NativeUInt = 4;

Type
  // Special flags for Chat accounts - they go in the top 8 bits
  // of the steam ID's "instance", leaving 12 for the actual instances
  TChatSteamIDInstanceFlags = (k_EChatAccountInstanceMask = $00000FFF, // top 8 bits are flags
    k_EChatInstanceFlagClan = 80000, // top bit
    k_EChatInstanceFlagLobby = 40000, // next one down, etc
    k_EChatInstanceFlagMMSLobby = 20000 // next one down, etc
    );

  // -----------------------------------------------------------------------------
  // Purpose: Marketing message flags that change how a client should handle them
  // -----------------------------------------------------------------------------
  {$ifdef FPC}
  TMarketingMessageFlags = (
   k_EMarketingMessageFlagsNone = 0,
   k_EMarketingMessageFlagsHighPriority = 1 Shl 0,
   k_EMarketingMessageFlagsPlatformWindows = 1 Shl 1,
   k_EMarketingMessageFlagsPlatformMac = 1 Shl 2,
   k_EMarketingMessageFlagsPlatformLinux = 1 Shl 3
   );
  const k_EMarketingMessageFlagsPlatformRestrictions = byte(k_EMarketingMessageFlagsPlatformWindows) OR byte(k_EMarketingMessageFlagsPlatformMac) Or byte(k_EMarketingMessageFlagsPlatformLinux);
  Type
  {$else}
  TMarketingMessageFlags = (
   k_EMarketingMessageFlagsNone = 0,
   k_EMarketingMessageFlagsHighPriority = 1 Shl 0,
   k_EMarketingMessageFlagsPlatformWindows = 1 Shl 1,
   k_EMarketingMessageFlagsPlatformMac = 1 Shl 2,
   k_EMarketingMessageFlagsPlatformLinux = 1 Shl 3
   );
   const k_EMarketingMessageFlagsPlatformRestrictions = byte(k_EMarketingMessageFlagsPlatformWindows) OR byte(k_EMarketingMessageFlagsPlatformMac) Or byte(k_EMarketingMessageFlagsPlatformLinux);
   Type
   // aggregate flags
   //k_EMarketingMessageFlagsPlatformRestrictions = byte(k_EMarketingMessageFlagsPlatformWindows) OR byte(k_EMarketingMessageFlagsPlatformMac) Or byte(k_EMarketingMessageFlagsPlatformLinux));
  {$endif}


  // -----------------------------------------------------------------------------
  // Purpose: Possible positions to tell the overlay to show notifications in
  // -----------------------------------------------------------------------------
  TNotificationPosition = (k_EPositionTopLeft = 0, k_EPositionTopRight = 1, k_EPositionBottomLeft = 2, k_EPositionBottomRight = 3);

  TSteamAPIWarningMessageHook = procedure(a: integer; b: pAnsiChar); cdecl;
  TSteamAPI_PostAPIResultInProcess = procedure(callHandle: TSteamAPICall; b: pointer; CallbackSize: uint32; CallbackNum: integer); cdecl;
  TSteamAPI_CheckCallbackRegistered = procedure(CallbackNum: integer); cdecl;

  TValvePackingSentinel = record
    u32: uint32;
    u64: uint64;
    u16: uint16;
    d: double;
  end;

  // right-handed system
  // +y is up
  // +x is to the right
  // -z is going away from you
  // Distance unit is  meters
  THmdMatrix34 = array [0 .. 2, 0 .. 3] of single;
  pHmdMatrix34 = ^THmdMatrix34;

  THmdMatrix44 = array [0 .. 3, 0 .. 3] of single;
  pHmdMatrix44 = ^THmdMatrix44;

  // Used to return the post-distortion UVs for each color channel.
  // UVs range from 0 to 1 with 0,0 in the upper left corner of the
  // source render target. The 0,0 to 1,1 range covers a single eye.
  TDistortionCoordinates = record
    Red: array [0 .. 1] of single;
    Green: array [0 .. 1] of single;
    Blue: array [0 .. 1] of single;
  end;

  THmdEye = (Eye_Left = 0, Eye_Right = 1);

  TGraphicsAPIConvention = (
    API_DirectX = 0, // Normalized Z goes from 0 at the viewer to 1 at the far clip plane
    API_OpenGL = 1   // Normalized Z goes from 1 at the viewer to -1 at the far clip plane
    );

  THmdTrackingResult = (
    TrackingResult_Uninitialized = 1,
    TrackingResult_Calibrating_InProgress = 100,
    TrackingResult_Calibrating_OutOfRange = 101,
    TrackingResult_Running_OK = 200,
    TrackingResult_Running_OutOfRange = 201
  );
  pHmdTrackingResult = ^THmdTrackingResult;

  THmdError = (
    HmdError_None = 0,
    HmdError_Init_InstallationNotFound = 100,
    HmdError_Init_InstallationCorrupt = 101,
    HmdError_Init_VRClientDLLNotFound = 102,
    HmdError_Init_FileNotFound = 103,
    HmdError_Init_FactoryNotFound = 104,
    HmdError_Init_InterfaceNotFound = 105,
    HmdError_Init_InvalidInterface = 106,
    HmdError_Init_UserConfigDirectoryInvalid = 107,
    HmdError_Init_HmdNotFound = 108,
    HmdError_Init_NotInitialized = 109,
    HmdError_Driver_Failed = 200,
    HmdError_Driver_Unknown = 201,
    HmdError_Driver_HmdUnknown = 202,
    HmdError_Driver_NotLoaded = 203,
    HmdError_IPC_ServerInitFailed = 300,
    HmdError_IPC_ConnectFailed = 301,
    HmdError_IPC_SharedStateInitFailed = 302,
    HmdError_VendorSpecific_UnableToConnectToOculusRuntime = 1000
    );

  pHmdError = ^THmdError;

  TSteamUser = int32;

  TGameID = record
  private
    Data1: SmallInt; // 16 bit
    Data2: Byte; // 8 bit
    Data3: Byte; // 8 bit
    Data4: int32; // 32 bit
  public
    function GetAppID: integer;
    function GetType: integer;
    function GetModID: integer;
  end;

  TSteamID = record
  private
    Data1: uint32; // 32 bit
    Data2: SmallInt; // 16 bit
    Data3: Byte; // 8 bit
    Data4: Byte; // 8 bit
  public
    function GetAccountID: uint32; // unique account identifier
    function GetAccountInstance: uint32; // dynamic instance ID
    function GetAccountType: TAccountType; // type of account
    function GetUniverse: TUniverse; // universe this account belongs to
  end;

  PSteamID = ^TSteamID;

  TMouseCursor = (
   dc_user = 0,
   dc_none,
   dc_arrow,
   dc_ibeam,
   dc_hourglass,
   dc_waitarrow,
   dc_crosshair,
   dc_up,
   dc_sizenw,
   dc_sizese,
   dc_sizene,
   dc_sizesw,
   dc_sizew,
   dc_sizee,
   dc_sizen,
   dc_sizes,
   dc_sizewe,
   dc_sizens,
   dc_sizeall,
   dc_no,
   dc_hand,
   dc_blank, // don't show any custom cursor, just use your default
   dc_middle_pan,
   dc_north_pan,
   dc_north_east_pan,
   dc_east_pan,
   dc_south_east_pan,
   dc_south_pan,
   dc_south_west_pan,
   dc_west_pan,
   dc_north_west_pan,
   dc_alias,
   dc_cell,
   dc_colresize,
   dc_copycur,
   dc_verticaltext,
   dc_rowresize,
   dc_zoomin,
   dc_zoomout,
   dc_help,
   dc_custom,
   dc_last // custom cursors start from this value and up
  );

  THTMLKeyModifiers = (eHTMLKeyModifier_None = 0, eHTMLKeyModifier_AltDown = 1 shl 0, eHTMLKeyModifier_CrtlDown = 1 shl 1, eHTMLKeyModifier_ShiftDown = 1 shl 2);

  THTMLMouseButton = (eHTMLMouseButton_Left = 0, eHTMLMouseButton_Right = 1, eHTMLMouseButton_Middle = 2);

  TText64 = array [0 .. 63] of AnsiChar;
  TText128 = array [0 .. 127] of AnsiChar;
  TText256 = array [0 .. 255] of AnsiChar;
  TAchievementName = array [0 .. k_cchStatNameMax - 1] of AnsiChar;
  TProofOfPurchaseKey = array [0 .. k_cubAppProofOfPurchaseKeyMax - 1] of AnsiChar;
  TFileName = array [0 .. k_ccFilenameMax - 1] of AnsiChar;
  TPublishedFileId = array [0 .. k_unEnumeratePublishedFilesMaxResults - 1] of uint64;
  TRTimeData = array [0 .. k_unEnumeratePublishedFilesMaxResults - 1] of uint32;
  TTitle = array [0 .. k_cchPublishedDocumentTitleMax - 1] of AnsiChar;
  TDescription = array [0 .. k_cchPublishedDocumentDescriptionMax - 1] of AnsiChar;

  TTags = array [0 .. k_cchTagListMax - 1] of AnsiChar;
  TURL = array [0 .. k_cchPublishedFileURLMax - 1] of AnsiChar;
  TScore = array [0 .. k_unEnumeratePublishedFilesMaxResults - 1] of single;
  TConnectData = array [0 .. k_cchMaxRichPresenceValueLength - 1] of AnsiChar;

  TFollowers = array [0 .. k_cEnumerateFollowersMax - 1] of TSteamID;

  TGameDir = array [0 .. k_cbMaxGameServerGameDir - 1] of AnsiChar;
  TServerMapName = array [0 .. k_cbMaxGameServerMapName - 1] of AnsiChar;
  TGameTags = array [0 .. k_cbMaxGameServerTags - 1] of AnsiChar;
  TGameDescription = array [0 .. k_cbMaxGameServerGameDescription - 1] of AnsiChar;

  TSteamUGCDetails = record
    PublishedFileId: uint64;
    result: integer; // The result of the operation.
    FileType: integer; // Type of the file
    CreatorAppID: uint32; // ID of the app that created this file.
    ConsumerAppID: uint32; // ID of the app that will consume this file.
    Title: array [0 .. k_cchPublishedDocumentTitleMax - 1] of AnsiChar; // title of document
    Description: array [0 .. k_cchPublishedDocumentDescriptionMax - 1] of AnsiChar; // description of document
    SteamIDOwner: uint64; // Steam ID of the user who created this content.
    timeCreated: uint32; // time when the published file was created
    timeUpdated: uint32; // time when the published file was last updated
    timeAddedToUserList: uint32; // time when the user added the published file to their list (not always applicable)
    Visibility: integer; // visibility
    Banned: boolean; // whether the file was banned
    AcceptedForUse: boolean; // developer has specifically flagged this item as accepted in the Workshop
    TagsTruncated: boolean; // whether the list of tags was too long to be returned in the provided buffer
    Tags: array [0 .. k_cchTagListMax - 1] of AnsiChar; // comma separated list of all tags associated with this file
    // file/url information
    hFile: uint64; // The handle of the primary file
    hPreviewFile: uint64; // The handle of the preview file
    Filename: array [0 .. k_ccFilenameMax - 1] of AnsiChar; // The cloud filename of the primary file
    FileSize: int32; // Size of the primary file
    PreviewFileSize: int32; // Size of the preview file
    URL: array [0 .. k_cchPublishedFileURLMax - 1] of AnsiChar; // URL (for a video or a website)
    // voting information
    VotesUp: uint32; // number of votes up
    VotesDown: uint32; // number of votes down
    Score: single; // calculated score
    NumChildren: uint32; // if m_eFileType == k_EWorkshopFileTypeCollection, then this number will be the number of children contained within the collection
  end;

  pSteamUGCDetails = ^TSteamUGCDetails;

  // type of data request, when downloading leaderboard entries
  TLeaderboardDataRequest = (
   k_ELeaderboardDataRequestGlobal = 0,
   k_ELeaderboardDataRequestGlobalAroundUser = 1,
   k_ELeaderboardDataRequestFriends = 2,
   k_ELeaderboardDataRequestUsers = 3
  );

  // the sort order of a leaderboard
  TLeaderboardSortMethod = (
   k_ELeaderboardSortMethodNone = 0,
   k_ELeaderboardSortMethodAscending = 1, // top-score is lowest number
   k_ELeaderboardSortMethodDescending = 2 // top-score is highest number
  );

  // the display type (used by the Steam Community web site) for a leaderboard
  TLeaderboardDisplayType = (
   k_ELeaderboardDisplayTypeNone = 0,
   k_ELeaderboardDisplayTypeNumeric = 1, // simple numerical score
   k_ELeaderboardDisplayTypeTimeSeconds = 2, // the score represents a time, in seconds
   k_ELeaderboardDisplayTypeTimeMilliSeconds = 3 // the score represents a time, in milliseconds
  );

  TLeaderboardUploadScoreMethod = (
   k_ELeaderboardUploadScoreMethodNone = 0,
   k_ELeaderboardUploadScoreMethodKeepBest = 1, // Leaderboard will keep user's best score
   k_ELeaderboardUploadScoreMethodForceUpdate = 2 // Leaderboard will always replace score with specified
  );

  // handle to a single leaderboard
  TSteamLeaderboard = uint64;

  // handle to a set of downloaded entries in a leaderboard
  TSteamLeaderboardEntries = uint64;

  TUGCHandle = uint64;

  TLeaderboardEntry = record
    steamIDUser: TSteamID; // user with the entry - use SteamFriends-GetFriendPersonaName() & SteamFriends-GetFriendAvatar() to get more info
    GlobalRank, // [1..N], where N is the number of users with an entry in the leaderboard
    Score, // score as set in the leaderboard
    Details: int32; // number of int32 details available for this entry
    UGC: TUGCHandle; // handle for UGC attached to the entry
  end;

  pLeaderboardEntry = ^TLeaderboardEntry;

  TClientUnifiedMessageHandle = uint64;

  TP2PSessionState = record
    ConnectionActive: uint8; // true if we've got an active open connection
    Connecting: uint8; // true if we're currently trying to establish a connection
    P2PSessionError: uint8; // last error recorded (see enum above)
    UsingRelay: uint8; // true if it's going through a relay server (TURN)
    BytesQueuedForSend: int32;
    PacketsQueuedForSend: int32;
    RemoteIP: int32; // potential IP:Port of remote host. Could be TURN server.
    RemotePort: int16; // Only exists for compatibility with older authentication api's
  end;

  pP2PSessionState = ^TP2PSessionState;

  TSteamParamStringArray = record
    Strings: ppAnsiChar;
    NumStrings: int32;
  end;

  pSteamParamStringArray = ^TSteamParamStringArray;

  TFriendGameInfo = record
    GameID: TGameID;
    GameIP: uint32;
    GamePort, QueryPort: uint16;
    SteamIDLobby: TSteamID;
  end;

  pFriendGameInfo = ^TFriendGameInfo;

  TSteamControllerState = record
    // If packet num matches that on your prior call, then the controller state hasn't been changed since
    // your last call and there is no need to process it
    PacketNum: uint32;
    // bit flags for each of the buttons
    Buttons: uint64;
    // Left pad coordinates
    LeftPadX, LeftPadY,
    // Right pad coordinates
    RightPadX, RightPadY: shortint;
  end;

  pSteamControllerState = ^TSteamControllerState;

  TServerQuery = integer;

  TMatchMakingKeyValuePair = record
    Key: array [0 .. 255] of AnsiChar;
    Value: array [0 .. 255] of AnsiChar;
  end;

  pMatchMakingKeyValuePair = ^TMatchMakingKeyValuePair;
  ppMatchMakingKeyValuePair = ^pMatchMakingKeyValuePair;

  TServerListRequest = pointer;
  TMatchMakingServerResponse = (
   eServerResponded = 0,
   eServerFailedToRespond,
   eNoServersListedOnMasterServer // for the Internet query type, returned in response callback if no servers of this type match
  );

  TOnServerResponded = procedure(Req: TServerListRequest; Server: integer); cdecl;
  TOnServerFailedToRespond = procedure(Req: TServerListRequest; Server: integer); cdecl;
  TOnServerRefreshComplete = procedure(Req: TServerListRequest; Response: TMatchMakingServerResponse); cdecl;

  TServerDetails = record
    Name: pAnsiChar;
    QueryPort, ConnectionPort: uint16;
    IP: uint32;
    Ping: integer; // current ping time in milliseconds
    HadSuccessfulResponse, // server has responded successfully in the past
    DoNotRefresh: boolean; // server is marked as not responding and should no longer be refreshed
    GameDir: array [0 .. k_cbMaxGameServerGameDir - 1] of AnsiChar; // current game directory
    Map: array [0 .. k_cbMaxGameServerMapName - 1] of AnsiChar; // current map
    GameDescription: array [0 .. k_cbMaxGameServerGameDescription - 1] of AnsiChar; // game description
    AppID: uint32; // Steam App ID of this server
    Players, // total number of players currently on the server.  INCLUDES BOTS!
    MaxPlayers, // Maximum players that can join this server
    BotPlayers: integer; // Number of bots (i.e simulated players) on this server
    Password, // true if this server needs a password to join
    Secure: boolean; // Is this server protected by VAC
    TimeLastPlayed: uint32; // time (in unix time) when this server was last played on (for favorite/history servers)
    ServerVersion: integer; // server version as reported to Steam
    GameTags: array [0 .. k_cbMaxGameServerTags - 1] of AnsiChar; // the tags this server exposes
  end;

  TOnAddPlayerToList = procedure(Name: pAnsiChar; Score: integer; TimePlayed: single); cdecl;
  TSimpleEvent = procedure(); cdecl;
  TRulesResponded = procedure(Rule, Value: pAnsiChar); cdecl;

  TServerMode = (eServerModeInvalid = 0, // DO NOT USE
    eServerModeNoAuthentication = 1, // Don't authenticate user logins and don't list on the server list
    eServerModeAuthentication = 2, // Authenticate users, list on the server list, don't run VAC on clients that connect
    eServerModeAuthenticationAndSecure = 3 // Authenticate users, list on the server list and VAC protect clients
    );

  // lobby type description
  TLobbyType = (k_ELobbyTypePrivate = 0, // only way to join the lobby is to invite to someone else
    k_ELobbyTypeFriendsOnly = 1, // shows for friends or invitees, but not in lobby list
    k_ELobbyTypePublic = 2, // visible for friends and in lobby list
    k_ELobbyTypeInvisible = 3 // returned by search, but not visible to other friends
    // useful if you want a user in two lobbies, for example matching groups together
    // a user can be in only one regular lobby, and up to two invisible lobbies
    );

  // lobby search filter tools
  TLobbyComparison = (k_ELobbyComparisonEqualToOrLessThan = -2, k_ELobbyComparisonLessThan = -1, k_ELobbyComparisonEqual = 0, k_ELobbyComparisonGreaterThan = 1, k_ELobbyComparisonEqualToOrGreaterThan = 2, k_ELobbyComparisonNotEqual = 3);

  // lobby search distance. Lobby results are sorted from closest to farthest.
  TLobbyDistanceFilter = (k_ELobbyDistanceFilterClose, // only lobbies in the same immediate region will be returned
    k_ELobbyDistanceFilterDefault, // only lobbies in the same region or near by regions
    k_ELobbyDistanceFilterFar, // for games that don't have many latency requirements, will return lobbies about half-way around the globe
    k_ELobbyDistanceFilterWorldwide // no filtering, will match lobbies as far as India to NY (not recommended, expect multiple seconds of latency between the clients)
    );
{$ENDREGION}
{$REGION 'Callbacks definition'}

Type
  // server ----
  // Sent as a reply to ComputeNewPlayerCompatibility()
  TOnComputeNewPlayerCompatibilityResult = procedure(eResult, PlayersThatDontLikeCandidate, PlayersThatCandidateDoesntLike, ClanPlayersThatDontLikeCandidate: integer; CandidateSteamID: TSteamID); cdecl;
  // Sent as a reply to AssociateWithClan()
  TOnAssociateWithClanResult = procedure(eResult: integer); cdecl;
  // Sent as a reply to GetServerReputation()
  TOnGameServerReputation = procedure(eResult: TResult; ReputationScore: uint32; bBanned: boolean; BannedIP: uint32; usBannedPort: uint16; BannedGameID: uint64; BanExpires: uint32); cdecl;
  // send as a reply to RequestUserGroupStatus()
  TOnGameServerClientGroupStatus = procedure(UserSteamID: TSteamID; GroupSteamID: TSteamID; Member, Officer: boolean); cdecl;
  // GS gameplay stats info
  TOnGameServerGameplayStats = procedure(eResult: TResult; Rank: int32; TotalConnects, TotalMinutesPlayed: uint32); cdecl;
  // received when the game server requests to be displayed as secure (VAC protected)
  // Secure is true if the game server should display itself as secure to users, false otherwise
  TOnGameServerPolicyResponse = procedure(Secure: uint8); cdecl;
  // client achievement info
  TOnGameServerClientAchievementStatus = procedure(SteamID: uint64; Achievement: TText128; blocked: boolean); cdecl;
  // request the game server should kick the user
  TOnGameServerClientKick = procedure(SteamID: TSteamID; DenyReason: integer); cdecl;
  // client has been denied to connection to this game server
  TOnGameServerClientDeny = procedure(SteamID: TSteamID; DenyReason: integer; OptionalText: TText128); cdecl;
  // client has been approved to connect to this game server
  TOnGameServerClientApprove = procedure(SteamID, OwnerSteamID: TSteamID); cdecl;
  // server stats ----
  TOnGameServerStatsReceived = procedure(eResult: TResult; UserSteamID: TSteamID); cdecl;
  TOnGameServerStatsStored = procedure(eResult: TResult; UserSteamID: TSteamID); cdecl;
  TOnGameServerStatsUnloaded = procedure(UserSteamID: TSteamID); cdecl;
  // user ----
  // called when a connections to the Steam back-end has been established
  // this means the Steam client now has a working connection to the Steam servers
  // usually this will have occurred before the game has launched, and should
  // only be seen if the user has dropped connection due to a networking issue
  // or a Steam server update
  TOnSteamServersConnected = procedure(); cdecl;
  // called when a connection attempt has failed
  // this will occur periodically if the Steam client is not connected,
  // and has failed in it's retry to establish a connection
  TOnSteamServerConnectFailure = procedure(eResult: TResult); cdecl;
  // called if the client has lost connection to the Steam servers
  // real-time services will be disabled until a matching SteamServersConnected_t has been posted
  TOnSteamServersDisconnected = procedure(eResult: TResult); cdecl;
  // Sent by the Steam server to the client telling it to disconnect from the specified game server,
  // which it may be in the process of or already connected to.
  // The game client should immediately disconnect upon receiving this message.
  // This can usually occur if the user doesn't have rights to play on the game server.
  TOnClientGameServerDeny = procedure(uAppID, GameServerIP: uint32; GameServerPort: uint16; Secure: boolean; Reason: uint32); cdecl;
  // called when the callback system for this client is in an error state (and has flushed pending callbacks)
  // When getting this message the client should disconnect from Steam, reset any stored Steam state and reconnect.
  // This usually occurs in the rare event the Steam client has some kind of fatal error.
  TOnIPCFailure = procedure(FailureType: uint8); cdecl;
  // callback for BeginAuthSession
  TOnValidateAuthTicketResponse = procedure(SteamID: TSteamID; AuthSessionResponse: integer; OwnerSteamID: TSteamID); cdecl;
  // Purpose: called when a user has responded to a microtransaction authorization request
  TOnMicroTxnAuthorizationResponse = procedure(AppID: uint32; OrderID: uint64; Authorized: uint8); cdecl;
  // Purpose: Result from RequestEncryptedAppTicket
  TOnEncryptedAppTicketResponse = procedure(eResult: integer); cdecl;
  // callback for GetAuthSessionTicket
  TOnGetAuthSessionTicketResponse = procedure(AuthTicket: uint32; eResult: integer); cdecl;
  // Purpose: sent to your game in response to a steam://gamewebcallback/ command
  TOnGameWebCallback = procedure(URL: TText256); cdecl;
  // matchmaking ----
  TOnFavoritesListChanged = procedure(IP, QueryPort, ConnPort, AppID, Flags: uint32; bAdd: boolean; AccotId: uint32); cdecl;
  TOnLobbyInvite = procedure(UserSteamID: TSteamID; LobbySteamID: TSteamID; GameID: TSteamID); cdecl;
  TOnLobbyEnter = procedure(LobbySteamID: TSteamID; ChatPermissions: uint32; blocked: boolean; ChatRoomEnterResponse: uint32); cdecl;
  TOnLobbyDataUpdate = procedure(LobbySteamID: uint64; MemberSteamID: uint64; Success: uint8); cdecl;
  TOnLobbyChatUpdate = procedure(LobbySteamID: uint64; UserChangedSteamID: uint64; MakingChangeSteamID: uint64; ChatMemberStateChange: uint32); cdecl;
  TOnLobbyChatMsg = procedure(LobbySteamID: uint64; UserSteamID: uint64; ChatEntryType: uint8; iChatID: uint32); cdecl;
  TOnLobbyGameCreated = procedure(LobbySteamID: uint64; GameServerSteamID: uint64; IP: uint32; usPort: uint16); cdecl;
  TOnLobbyMatchList = procedure(LobbiesMatching: uint32); cdecl;
  TOnLobbyKicked = procedure(LobbySteamID: uint64; AdminSteamID: uint64; KickedDueToDisconnect: uint8); cdecl;
  TOnLobbyCreated = procedure(eResult: TResult; SteamIDLobby: TSteamID); cdecl;
  TOnPSNGameBootInviteResult = procedure(eResult: TResult; LobbySteamID: uint64); cdecl;
  TOnPSNGameBootInvitinteger = procedure(GameBootInviteExists: boolean; LobbySteamID: TSteamID); cdecl;
  TOnFavoritesListAccountsUpdated = procedure(eResult: integer); cdecl;
  // user stats ----
  TOnUserStatsReceived = procedure(GameID: uint64; eResult: TResult; UserSteamID: TSteamID); cdecl;
  TOnUserStatsStored = procedure(GameID: uint64; eResult: integer); cdecl;
  TOnUserAchievementStored = procedure(GameID: uint64; bGroupAchievement: boolean; AchievementName: TAchievementName; CurProgress, MaxProgress: uint32); cdecl;
  TOnLeaderboardFindResult = procedure(SteamLeaderboard: uint64; LeaderboardFod: uint8); cdecl;
  TOnLeaderboardScoresDownloaded = procedure(SteamLeaderboard: uint64; SteamLeaderboardEntries: uint64; EntryCount: integer); cdecl;
  TOnLeaderboardScoreUploaded = procedure(Success: uint8; SteamLeaderboard: uint64; Score: int32; ScoreChanged: uint8; GlobalRankNew: integer; GlobalRankPrevious: integer); cdecl;
  TOnNumberOfCurrentPlayers = procedure(Success: uint8; cPlayers: int32); cdecl;
  TOnUserStatsUnloaded = procedure(steamIDUser: TSteamID); cdecl;
  TOnUserAchievementIconFetched = procedure(GameID: TGameID; AchievementName: TAchievementName; Achieved: boolean; IconHandle: integer); cdecl;
  TOnGlobalAchievementPercentagesReady = procedure(GameID: uint64; eResult: integer); cdecl;
  TOnLeaderboardUGCSet = procedure(eResult: TResult; SteamLeaderboard: uint64); cdecl;
  TOnGlobalStatsReceived = procedure(GameID: uint64; eResult: integer); cdecl;
  // apps ----
  TOnDlcInstalled = procedure(AppID: uint32); cdecl;
  TOnRegisterActivationCodeResponse = procedure(RegisterActivationCodeResult: TResult; PackageRegistered: uint32); cdecl;
  TOnAppProofOfPurchaseKeyResponse = procedure(eResult: TResult; AppID: uint32; Key: TProofOfPurchaseKey); cdecl;
  TOnNewLaunchQueryParameters = procedure(); cdecl;
  // networking ----
  TOnP2PSessionRequest = procedure(RemoteSteamID: TSteamID); cdecl;
  TOnP2PSessionConnectFail = procedure(RemoteSteamID: TSteamID; P2PSessionError: uint8); cdecl;
  TOnSocketStatusCallback = procedure(Socket: uint32; ListenSocket: uint32; RemoteSteamID: TSteamID; SNetSocketState: integer); cdecl;
  // remote storage ----
  TOnRemoteStorageAppSyncedClient = procedure(AppID: uint32; eResult, NumDownloads: integer); cdecl;
  TOnRemoteStorageAppSyncedServer = procedure(AppID: uint32; eResult, NumUploads: integer); cdecl;
  TOnRemoteStorageAppSyncProgress = procedure(CurrentFile: TFileName; AppID: uint32; uBytesTransferredThisChk: uint32; AppPercentComplete: double; Uploading: boolean); cdecl;
  TOnRemoteStorageAppSyncStatusCheck = procedure(AppID: uint32; eResult: integer); cdecl;
  TOnRemoteStorageConflictResolution = procedure(AppID: uint32; eResult: integer); cdecl;
  TOnRemoteStorageFileShareResult = procedure(eResult: TResult; hFile: uint64; Filename: TFileName); cdecl;
  TOnRemoteStoragePublishFileResult = procedure(eResult: TResult; PublishedFileId: uint64; UserNeedsToAcceptWorkshopLegalAgreement: boolean); cdecl;
  TOnRemoteStorageDeletePublishedFileResult = procedure(eResult: TResult; PublishedFileId: uint64); cdecl;
  TOnRemoteStorageEnumerateUserPublishedFilesResult = procedure(eResult: TResult; ResultsReturned, TotalResultCnt: int32; PublishedFileId: TPublishedFileId); cdecl;
  TOnRemoteStorageSubscribePublishedFilinteger = procedure(eResult: TResult; PublishedFileId: uint64); cdecl;
  TOnRemoteStorageEnumerateUserSubscribedFilesResult = procedure(eResult: TResult; ResultsReturned: int32; TotalResultCnt: int32; PublishedFileId: TPublishedFileId; RTimeSubscribed: TRTimeData); cdecl;
  TOnRemoteStorageSubscribePublishedFileResult = procedure(eResult: TResult; PublishedFileId: uint64); cdecl;
  TOnRemoteStorageUnsubscribePublishedFileResult = procedure(eResult: TResult; PublishedFileId: uint64); cdecl;
  TOnRemoteStorageUpdatePublishedFileResult = procedure(eResult: TResult; PublishedFileId: uint64; UserNeedsToAcceptWorkshopLegalAgreement: boolean); cdecl;
  TOnRemoteStorageDownloadUGCResult = procedure(eResult: TResult; hFile: uint64; AppID: uint32; SizeInBytes: int32; Filename: TFileName; OwnerSteamID: uint64); cdecl;
  TOnRemoteStorageGetPublishedFileDetailsResult = procedure(eResult: TResult; PublishedFileId: uint64; CreatorAppID, ConsumerAppID: uint32; Title: TTitle; Description: TDescription; hFile, PreviewFile: uint64; OwnerSteamID: uint64; rtimeCreated, rtimeUpdated: uint32; eVisibility: integer; Banned: boolean; Tags: TTags; TagsTrcated: boolean; Filename: TFileName; FileSize, PreviewFileSize: int32; URL: TURL; FileType: integer; AcceptedForUse: boolean); cdecl;
  TOnRemoteStorageEnumerateWorkshopFilesResult = procedure(eResult: TResult; ResultsReturned, TotalResultCnt: int32; PublishedFileId: TPublishedFileId; Score: TScore; AppID, StartIndex: uint32); cdecl;
  TOnRemoteStorageGetPublishedItemVoteDetailsResult = procedure(eResult: TResult; PublishedFileId: uint64; VotesFor, VotesAgainst, Reports: int32; Score: single); cdecl;
  TOnRemoteStoragePublishedFileSubscribed = procedure(PublishedFileId: uint64; AppID: uint32); cdecl;
  TOnRemoteStoragePublishedFileUnsubscribed = procedure(PublishedFileId: uint64; AppID: uint32); cdecl;
  TOnRemoteStoragePublishedFileDeleted = procedure(PublishedFileId: uint64; AppID: uint32); cdecl;
  TOnRemoteStorageUpdateUserPublishedItemVoteResult = procedure(eResult: TResult; PublishedFileId: uint64); cdecl;
  TOnRemoteStorageUserVoteDetails = procedure(eResult: TResult; PublishedFileId: uint64; eVote: integer); cdecl;
  TOnRemoteStorageEnumerateUserSharedWorkshopFilesResult = procedure(eResult: TResult; ResultsReturned, TotalResultCount: int32; PublishedFileId: TPublishedFileId); cdecl;
  TOnRemoteStorageSetUserPublishedFileActionResult = procedure(eResult: TResult; PublishedFileId: uint64; eAction: integer); cdecl;
  TOnRemoteStorageEnumeratePublishedFilesByUserActionResult = procedure(eResult: TResult; eAction: integer; ResultsReturned, TotalResultCnt: int32; PublishedFileId: TPublishedFileId; rtimeUpdated: TRTimeData); cdecl;
  TOnRemoteStoragePublishFileProgress = procedure(PercentFile: double; Preview: boolean); cdecl;
  TOnRemoteStoragePublishedFileUpdated = procedure(PublishedFileId: uint64; AppID: uint32; hFile: uint64); cdecl;
  // screenshots ----
  // Screenshot successfully written or otherwise added to the library
  // and can now be tagged
  TOnScreenshotReady = procedure(LocalHandle: uint32; eResult: TResult); cdecl;
  // Screenshot has been requested by the user.  Only sent if
  // HookScreenshots() has been called, in which case Steam will not take
  // the screenshot itself.
  TOnScreenshotRequested = procedure(); cdecl;
  // HTTP ----
  TOnHTTPRequestCompleted = procedure(Request: uint32; ContextValue: uint64; RequestSuccessful: boolean; eStatusCode: integer); cdecl;
  TOnHTTPRequestHeadersReceived = procedure(Request: uint32; ContextValue: uint64); cdecl;
  TOnHTTPRequestDataReceived = procedure(Request: uint32; ContextValue: uint64; Offset, BytesReceived: uint32); cdecl;
  // unified messages ----
  TOnSteamUnifiedMessagesSendMethodResult = procedure(Handle, Context: uint64; eResult: TResult; ResponseSize: uint32); cdecl;
  // UGC ----
  TOnSteamUGCQueryCompleted = procedure(Handle: uint64; eResult: TResult; NumResultsReturned, TotalMatchingResults: uint32; CachedData: boolean); cdecl;
  TOnSteamUGCRequestUGCDetailsResult = procedure(Details: TSteamUGCDetails; CachedData: boolean); cdecl;
  TOnCreateItemResult = procedure(eResult: TResult; PublishedFileId: uint64; bUserNeedsToAcceptWorkshopLegalAgreement: boolean); cdecl;
  TOnSubmitItemUpdateResult = procedure(eResult: TResult; bUserNeedsToAcceptWorkshopLegalAgreement: boolean); cdecl;
  TOnItemInstalled = procedure(AppID: uint32; PublishedFileId: uint64); cdecl;
  // app list ----
  TOnSteamAppInstalled = procedure(AppID: uint32); cdecl;
  TOnSteamAppUninstalled = procedure(AppID: uint32); cdecl;
  // music player ----
  TOnPlaybackStatusHasChanged = procedure(); cdecl;
  TOnVolumeHasChanged = procedure(NewVolume: single); cdecl;
  // music player remote ----
  TOnMusicPlayerRemoteWillActivate = procedure(); cdecl;
  TOnMusicPlayerRemoteWillDeactivate = procedure(); cdecl;
  TOnMusicPlayerRemoteToFront = procedure(); cdecl;
  TOnMusicPlayerWillQuit = procedure(); cdecl;
  TOnMusicPlayerWantsPlay = procedure(); cdecl;
  TOnMusicPlayerWantsPause = procedure(); cdecl;
  TOnMusicPlayerWantsPlayPrevious = procedure(); cdecl;
  TOnMusicPlayerWantsPlayNext = procedure(); cdecl;
  TOnMusicPlayerWantsShuffled = procedure(Shuffled: boolean); cdecl;
  TOnMusicPlayerWantsLooped = procedure(Looped: boolean); cdecl;
  TOnMusicPlayerWantsVolume = procedure(NewVolume: single); cdecl;
  TOnMusicPlayerSelectsQueueEntry = procedure(ID: integer); cdecl;
  TOnMusicPlayerSelectsPlaylistEntry = procedure(ID: integer); cdecl;
  // HTML surface ----
  TOnCloseBrowser = procedure(); cdecl;
  TOnNeedsPaint = procedure(pBGRA: pAnsiChar; Wide, Tall, UpdateX, UpdateY, UpdateWide, UpdateTall, ScrollX, ScrollY: uint32; PageScale: single; PageSerial: uint32); cdecl;
  TOnStartRequest = procedure(URL, Target, PostData: pAnsiChar; IsRedirect: boolean); cdecl;
  TOnFinishedRequest = procedure(URL: pAnsiChar; PageTitle: pAnsiChar); cdecl;
  TOnURLChanged = procedure(URL, PostData: pAnsiChar; IsRedirect: boolean; PageTitle: pAnsiChar; NewNavigation: boolean); cdecl;
  TOnOpenLinkInNewTab = procedure(URL: pAnsiChar); cdecl;
  TOnChangedTitle = procedure(Title: pAnsiChar); cdecl;
  TOnSearchResults = procedure(Results, CurrentMatch: uint32); cdecl;
  TOnCanGoBackAndForward = procedure(CanGoBack, CanGoForward: boolean); cdecl;
  TOnHorizontalScroll = procedure(ScrollMax, ScrollCurrent: uint32; PageScale: single; Visible: boolean; PageSize: uint32); cdecl;
  TOnVerticalScroll = procedure(ScrollMax, ScrollCurrent: uint32; PageScale: single; Visible: boolean; PageSize: uint32); cdecl;
  TOnLinkAtPosition = procedure(X, Y: uint32; URL: pAnsiChar; Input, LiveLink: boolean); cdecl;
  TOnJSAlert = procedure(Msg: pAnsiChar); cdecl;
  TOnJSConfirm = procedure(Msg: pAnsiChar); cdecl;
  TOnFileOpenDialog = procedure(Title, InitialFile: pAnsiChar); cdecl;
  TOnComboNeedsPaint = procedure(pBGRA: pAnsiChar; Width: uint32; Height: uint32); cdecl;
  TOnShowPopup = procedure(); cdecl;
  TOnHidePopup = procedure(); cdecl;
  TOnSizePopup = procedure(X: uint32; Y: uint32; Width: uint32; Height: uint32); cdecl;
  TOnNewWindow = procedure(URL: pAnsiChar; X: uint32; Y: uint32; Wide: uint32; Tall: uint32); cdecl;
  TOnSetCursor = procedure(MouseCursor: TMouseCursor); cdecl;
  TOnStatusText = procedure(Msg: pAnsiChar); cdecl;
  TOnShowToolTip = procedure(Msg: pAnsiChar); cdecl;
  TOnUpdateToolTip = procedure(Msg: pAnsiChar); cdecl;
  TOnHideToolTip = procedure(); cdecl;
  TOnBrowserReady = procedure(IOFailure: boolean); cdecl;
  // utils ----
  TOnIPCountry = procedure(); cdecl;
  TOnLowBatteryPower = procedure(MinutesBatteryLeft: uint8); cdecl;
  TOnSteamAPICallCompleted = procedure(AsyncCall: uint64); cdecl;
  TOnSteamShutdown = procedure(); cdecl;
  TOnCheckFileSignature = procedure(CheckFileSignature: integer); cdecl;
  TOnGamepadTextInputDismissed = procedure(Submitted: boolean; SubmittedText: uint32); cdecl;
  // friends ----
  TOnPersonaStateChange = procedure(SteamID: uint64; ChangeFlags: integer); cdecl;
  TOnGameOverlayActivated = procedure(Active: uint8); cdecl;
  TOnGameServerChangeRequested = procedure(Server, Password: TText64); cdecl;
  TOnGameLobbyJoinRequested = procedure(LobbySteamID: TSteamID; FriendSteamID: TSteamID); cdecl;
  TOnAvatarImageLoaded = procedure(SteamID: TSteamID; iImage, iWide, iTall: integer); cdecl;
  TOnClanOfficerListResponse = procedure(ClanSteamID: TSteamID; OfficersCount: integer; Success: uint8); cdecl;
  TOnFriendRichPresenceUpdate = procedure(FriendSteamID: TSteamID; AppID: uint32); cdecl;
  TOnGameRichPresenceJoinRequested = procedure(FriendSteamID: TSteamID; Connect: TConnectData); cdecl;
  TOnGameConnectedClanChatMsg = procedure(ClanChatSteamID: TSteamID; UserSteamID: TSteamID; MessageID: integer); cdecl;
  TOnGameConnectedChatJoin = procedure(ClanChatSteamID: TSteamID; SUserteamID: TSteamID); cdecl;
  TOnGameConnectedChatLeave = procedure(ClanChatSteamID: TSteamID; UserSteamID: TSteamID; Kicked, Dropped: boolean); cdecl;
  TOnDownloadClanActivityCountsResult = procedure(Success: boolean); cdecl;
  TOnJoinClanChatRoomCompletionResult = procedure(ClanChatSteamID: TSteamID; ChatRoomEnterResponse: integer); cdecl;
  TOnGameConnectedFriendChatMsg = procedure(UserSteamID: TSteamID; iMessageID: integer); cdecl;
  TOnFriendsGetFollowerCount = procedure(eResult: TResult; SteamID: TSteamID; Count: integer); cdecl;
  TOnFriendsIsFollowing = procedure(eResult: TResult; SteamID: TSteamID; IsFollowing: boolean); cdecl;
  TOnFriendsEnumerateFollowingList = procedure(eResult: TResult; Followers: TFollowers; ResultsReturned, TotalResultCnt: int32); cdecl;
  TOnSetPersonaNameResponse = procedure(Success, LocalSuccess: boolean; result: integer); cdecl;
{$ENDREGION}
{$REGION 'Callbacks'}

  THTMLSurfaceCallbacks = record
    OnCloseBrowser: TOnCloseBrowser;
    OnNeedsPaint: TOnNeedsPaint;
    OnStartRequest: TOnStartRequest;
    OnFinishedRequest: TOnFinishedRequest;
    OnURLChanged: TOnURLChanged;
    OnOpenLinkInNewTab: TOnOpenLinkInNewTab;
    OnChangedTitle: TOnChangedTitle;
    OnSearchResults: TOnSearchResults;
    OnCanGoBackAndForward: TOnCanGoBackAndForward;
    OnHorizontalScroll: TOnHorizontalScroll;
    OnVerticalScroll: TOnVerticalScroll;
    OnLinkAtPosition: TOnLinkAtPosition;
    OnJSAlert: TOnJSAlert;
    OnJSConfirm: TOnJSConfirm;
    OnFileOpenDialog: TOnFileOpenDialog;
    OnComboNeedsPaint: TOnComboNeedsPaint;
    OnShowPopup: TOnShowPopup;
    OnHidePopup: TOnHidePopup;
    OnSizePopup: TOnSizePopup;
    OnNewWindow: TOnNewWindow;
    OnSetCursor: TOnSetCursor;
    OnStatusText: TOnStatusText;
    OnShowToolTip: TOnShowToolTip;
    OnUpdateToolTip: TOnUpdateToolTip;
    OnHideToolTip: TOnHideToolTip;
    OnBrowserReady: TOnBrowserReady;
  end;

  TSteamCallbacks = record
    OnComputeNewPlayerCompatibilityResult: TOnComputeNewPlayerCompatibilityResult;
    OnAssociateWithClanResult: TOnAssociateWithClanResult;
    OnGameServerReputation: TOnGameServerReputation;
    OnGameServerClientGroupStatus: TOnGameServerClientGroupStatus;
    OnGameServerGameplayStats: TOnGameServerGameplayStats;
    OnGameServerPolicyResponse: TOnGameServerPolicyResponse;
    OnGameServerClientAchievementStatus: TOnGameServerClientAchievementStatus;
    OnGameServerClientKick: TOnGameServerClientKick;
    OnGameServerClientDeny: TOnGameServerClientDeny;
    OnGameServerClientApprove: TOnGameServerClientApprove;
    OnGameServerStatsReceived: TOnGameServerStatsReceived;
    OnGameServerStatsStored: TOnGameServerStatsStored;
    OnGameServerStatsUnloaded: TOnGameServerStatsUnloaded;
    OnSteamServersConnected: TOnSteamServersConnected;
    OnSteamServerConnectFailure: TOnSteamServerConnectFailure;
    OnSteamServersDisconnected: TOnSteamServersDisconnected;
    OnClientGameServerDeny: TOnClientGameServerDeny;
    OnIPCFailure: TOnIPCFailure;
    OnValidateAuthTicketResponse: TOnValidateAuthTicketResponse;
    OnMicroTxnAuthorizationResponse: TOnMicroTxnAuthorizationResponse;
    OnEncryptedAppTicketResponse: TOnEncryptedAppTicketResponse;
    OnGetAuthSessionTicketResponse: TOnGetAuthSessionTicketResponse;
    OnGameWebCallback: TOnGameWebCallback;
    OnFavoritesListChanged: TOnFavoritesListChanged;
    OnLobbyInvite: TOnLobbyInvite;
    OnLobbyEnter: TOnLobbyEnter;
    OnLobbyDataUpdate: TOnLobbyDataUpdate;
    OnLobbyChatUpdate: TOnLobbyChatUpdate;
    OnLobbyChatMsg: TOnLobbyChatMsg;
    OnLobbyGameCreated: TOnLobbyGameCreated;
    OnLobbyMatchList: TOnLobbyMatchList;
    OnLobbyKicked: TOnLobbyKicked;
    OnLobbyCreated: TOnLobbyCreated;
    OnPSNGameBootInviteResult: TOnPSNGameBootInviteResult;
    OnFavoritesListAccountsUpdated: TOnFavoritesListAccountsUpdated;
    OnFavoritesListChanged_ms: TOnFavoritesListChanged;
    OnLobbyInvite_ms: TOnLobbyInvite;
    OnLobbyEnter_ms: TOnLobbyEnter;
    OnLobbyDataUpdate_ms: TOnLobbyDataUpdate;
    OnLobbyChatUpdate_ms: TOnLobbyChatUpdate;
    OnLobbyChatMsg_ms: TOnLobbyChatMsg;
    OnLobbyGameCreated_ms: TOnLobbyGameCreated;
    OnLobbyMatchList_ms: TOnLobbyMatchList;
    OnLobbyKicked_ms: TOnLobbyKicked;
    OnLobbyCreated_ms: TOnLobbyCreated;
    OnPSNGameBootInviteResult_ms: TOnPSNGameBootInviteResult;
    OnFavoritesListAccountsUpdated_ms: TOnFavoritesListAccountsUpdated;
    OnUserStatsReceived: TOnUserStatsReceived;
    OnUserStatsStored: TOnUserStatsStored;
    OnUserAchievementStored: TOnUserAchievementStored;
    OnLeaderboardFindResult: TOnLeaderboardFindResult;
    OnLeaderboardScoresDownloaded: TOnLeaderboardScoresDownloaded;
    OnLeaderboardScoreUploaded: TOnLeaderboardScoreUploaded;
    OnNumberOfCurrentPlayers: TOnNumberOfCurrentPlayers;
    OnUserStatsUnloaded: TOnUserStatsUnloaded;
    OnUserAchievementIconFetched: TOnUserAchievementIconFetched;
    OnGlobalAchievementPercentagesReady: TOnGlobalAchievementPercentagesReady;
    OnLeaderboardUGCSet: TOnLeaderboardUGCSet;
    OnGlobalStatsReceived: TOnGlobalStatsReceived;
    OnDlcInstalled: TOnDlcInstalled;
    OnRegisterActivationCodeResponse: TOnRegisterActivationCodeResponse;
    OnAppProofOfPurchaseKeyResponse: TOnAppProofOfPurchaseKeyResponse;
    OnNewLaunchQueryParameters: TOnNewLaunchQueryParameters;
    OnP2PSessionRequest: TOnP2PSessionRequest;
    OnP2PSessionConnectFail: TOnP2PSessionConnectFail;
    OnSocketStatusCallback: TOnSocketStatusCallback;
    OnRemoteStorageAppSyncedClient: TOnRemoteStorageAppSyncedClient;
    OnRemoteStorageAppSyncedServer: TOnRemoteStorageAppSyncedServer;
    OnRemoteStorageAppSyncProgress: TOnRemoteStorageAppSyncProgress;
    OnRemoteStorageAppSyncStatusCheck: TOnRemoteStorageAppSyncStatusCheck;
    OnRemoteStorageConflictResolution: TOnRemoteStorageConflictResolution;
    OnRemoteStorageFileShareResult: TOnRemoteStorageFileShareResult;
    OnRemoteStoragePublishFileResult: TOnRemoteStoragePublishFileResult;
    OnRemoteStorageDeletePublishedFileResult: TOnRemoteStorageDeletePublishedFileResult;
    OnRemoteStorageEnumerateUserPublishedFilesResult: TOnRemoteStorageEnumerateUserPublishedFilesResult;
    OnRemoteStorageSubscribePublishedFileResult: TOnRemoteStorageSubscribePublishedFileResult;
    OnRemoteStorageEnumerateUserSubscribedFilesResult: TOnRemoteStorageEnumerateUserSubscribedFilesResult;
    OnRemoteStorageUnsubscribePublishedFileResult: TOnRemoteStorageUnsubscribePublishedFileResult;
    OnRemoteStorageUpdatePublishedFileResult: TOnRemoteStorageUpdatePublishedFileResult;
    OnRemoteStorageDownloadUGCResult: TOnRemoteStorageDownloadUGCResult;
    OnRemoteStorageGetPublishedFileDetailsResult: TOnRemoteStorageGetPublishedFileDetailsResult;
    OnRemoteStorageEnumerateWorkshopFilesResult: TOnRemoteStorageEnumerateWorkshopFilesResult;
    OnRemoteStorageGetPublishedItemVoteDetailsResult: TOnRemoteStorageGetPublishedItemVoteDetailsResult;
    OnRemoteStoragePublishedFileSubscribed: TOnRemoteStoragePublishedFileSubscribed;
    OnRemoteStoragePublishedFileUnsubscribed: TOnRemoteStoragePublishedFileUnsubscribed;
    OnRemoteStoragePublishedFileDeleted: TOnRemoteStoragePublishedFileDeleted;
    OnRemoteStorageUpdateUserPublishedItemVoteResult: TOnRemoteStorageUpdateUserPublishedItemVoteResult;
    OnRemoteStorageUserVoteDetails: TOnRemoteStorageUserVoteDetails;
    OnRemoteStorageEnumerateUserSharedWorkshopFilesResult: TOnRemoteStorageEnumerateUserSharedWorkshopFilesResult;
    OnRemoteStorageSetUserPublishedFileActionResult: TOnRemoteStorageSetUserPublishedFileActionResult;
    OnRemoteStorageEnumeratePublishedFilesByUserActionResult: TOnRemoteStorageEnumeratePublishedFilesByUserActionResult;
    OnRemoteStoragePublishFileProgress: TOnRemoteStoragePublishFileProgress;
    OnRemoteStoragePublishedFileUpdated: TOnRemoteStoragePublishedFileUpdated;
    OnScreenshotReady: TOnScreenshotReady;
    OnScreenshotRequested: TOnScreenshotRequested;
    OnHTTPRequestCompleted: TOnHTTPRequestCompleted;
    OnHTTPRequestHeadersReceived: TOnHTTPRequestHeadersReceived;
    OnHTTPRequestDataReceived: TOnHTTPRequestDataReceived;
    OnSteamUnifiedMessagesSendMethodResult: TOnSteamUnifiedMessagesSendMethodResult;
    OnSteamUGCQueryCompleted: TOnSteamUGCQueryCompleted;
    OnSteamUGCRequestUGCDetailsResult: TOnSteamUGCRequestUGCDetailsResult;
    OnCreateItemResult: TOnCreateItemResult;
    OnSubmitItemUpdateResult: TOnSubmitItemUpdateResult;
    OnItemInstalled: TOnItemInstalled;
    OnSteamAppInstalled: TOnSteamAppInstalled;
    OnSteamAppUninstalled: TOnSteamAppUninstalled;
    OnPlaybackStatusHasChanged: TOnPlaybackStatusHasChanged;
    OnVolumeHasChanged: TOnVolumeHasChanged;
    OnMusicPlayerRemoteWillActivate: TOnMusicPlayerRemoteWillActivate;
    OnMusicPlayerRemoteWillDeactivate: TOnMusicPlayerRemoteWillDeactivate;
    OnMusicPlayerRemoteToFront: TOnMusicPlayerRemoteToFront;
    OnMusicPlayerWillQuit: TOnMusicPlayerWillQuit;
    OnMusicPlayerWantsPlay: TOnMusicPlayerWantsPlay;
    OnMusicPlayerWantsPause: TOnMusicPlayerWantsPause;
    OnMusicPlayerWantsPlayPrevious: TOnMusicPlayerWantsPlayPrevious;
    OnMusicPlayerWantsPlayNext: TOnMusicPlayerWantsPlayNext;
    OnMusicPlayerWantsShuffled: TOnMusicPlayerWantsShuffled;
    OnMusicPlayerWantsLooped: TOnMusicPlayerWantsLooped;
    OnMusicPlayerWantsVolume: TOnMusicPlayerWantsVolume;
    OnMusicPlayerSelectsQueueEntry: TOnMusicPlayerSelectsQueueEntry;
    OnMusicPlayerSelectsPlaylistEntry: TOnMusicPlayerSelectsPlaylistEntry;
    OnIPCountry: TOnIPCountry;
    OnLowBatteryPower: TOnLowBatteryPower;
    OnSteamAPICallCompleted: TOnSteamAPICallCompleted;
    OnSteamShutdown: TOnSteamShutdown;
    OnCheckFileSignature: TOnCheckFileSignature;
    OnGamepadTextInputDismissed: TOnGamepadTextInputDismissed;
    OnPersonaStateChange: TOnPersonaStateChange;
    OnGameOverlayActivated: TOnGameOverlayActivated;
    OnGameServerChangeRequested: TOnGameServerChangeRequested;
    OnGameLobbyJoinRequested: TOnGameLobbyJoinRequested;
    OnAvatarImageLoaded: TOnAvatarImageLoaded;
    OnClanOfficerListResponse: TOnClanOfficerListResponse;
    OnFriendRichPresenceUpdate: TOnFriendRichPresenceUpdate;
    OnGameRichPresenceJoinRequested: TOnGameRichPresenceJoinRequested;
    OnGameConnectedClanChatMsg: TOnGameConnectedClanChatMsg;
    OnGameConnectedChatJoin: TOnGameConnectedChatJoin;
    OnGameConnectedChatLeave: TOnGameConnectedChatLeave;
    OnDownloadClanActivityCountsResult: TOnDownloadClanActivityCountsResult;
    OnJoinClanChatRoomCompletionResult: TOnJoinClanChatRoomCompletionResult;
    OnGameConnectedFriendChatMsg: TOnGameConnectedFriendChatMsg;
    OnFriendsGetFollowerCount: TOnFriendsGetFollowerCount;
    OnFriendsIsFollowing: TOnFriendsIsFollowing;
    OnFriendsEnumerateFollowingList: TOnFriendsEnumerateFollowingList;
    OnSetPersonaNameResponse: TOnSetPersonaNameResponse;
  end;
{$ENDREGION}

implementation

function TSteamID.GetAccountID: uint32;
begin
  result := Data1;
end;

function TSteamID.GetAccountInstance: uint32;
begin
  result := Word(Data2) + (Data3 and $0F) shl 16;
end;

function TSteamID.GetAccountType: TAccountType;
begin
  result := TAccountType(Data3 shr 4);
end;

function TSteamID.GetUniverse: TUniverse;
begin
  result := TUniverse(Data4);
end;

{ TGameID }

function TGameID.GetAppID: integer;
begin
  result := Word(Data1) + (Data2 and $0F) shl 16;
end;

function TGameID.GetModID: integer;
begin
  result := Data3;
end;

function TGameID.GetType: integer;
begin
  result := Data4;
end;

end.
