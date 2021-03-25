//----------------------------------------------------
// © 2015 Andrey Volia
// © 2016-2018 Francesco Balzani - thecocce
// 
// License: MIT
// Site: https://github.com/voliaandrey/steamwrapper
//----------------------------------------------------
unit Steamworks;

{$IFDEF FPC}
 {$MODE DELPHI}
{$ENDIF}

interface

uses SteamworksTypes;
{$REGION 'Library name'}

const
  {$IFDEF MSWINDOWS}
    WRAPPERLIB = 'libSteamWrapper.dll';
  {$ENDIF}
  {$IFDEF DARWIN}
    WRAPPERLIB = 'libSteamWrapper.dylib';
  {$ENDIF}
  {$IFDEF LINUX}
    WRAPPERLIB = 'libSteamWrapper.so';
  {$ENDIF}

{$ENDREGION}
{$REGION 'System'}
  // init and start wrapper
function System_InitWrapper(): boolean; stdcall; cdecl; external WRAPPERLIB;
// shutdown wrapper
procedure System_ShutdownWrapper(); cdecl; external WRAPPERLIB;
// checks if a local Steam client is running
function System_IsSteamRunning(): boolean; cdecl; external WRAPPERLIB;
// Detects if your executable was launched through the Steam client, and restarts your game through
// the client if necessary. The Steam client will be started if it is not running.
//
// Returns: true if your executable was NOT launched through the Steam client. This function will
// then start your application through the client. Your current process should exit.
//
// false if your executable was started through the Steam client or a steam_appid.txt file
// is present in your game's directory (for development). Your current process should continue.
//
// NOTE: This function should be used only if you are using CEG or not using Steam's DRM. Once applied
// to your executable, Steam's DRM will handle restarting through Steam if necessary.
function System_RestartAppIfNecessary(OwnAppID: uint32): boolean; cdecl; external WRAPPERLIB;
// crash dump recording functions
procedure System_WriteMiniDump(StructuredExceptionCode: uint32; ExceptionInfo: pointer; BuildID: uint32); cdecl; external WRAPPERLIB;
procedure System_SetMiniDumpComment(Msg: pAnsiChar); cdecl; external WRAPPERLIB;
// To dispatch callbacks to registered listeners, call the function SteamAPI_RunCallbacks().
// It's best to call this at >10Hz, the more time between calls, the more potential latency
// between receiving events or results from the Steam API. Most games call this once per render-frame.
// All registered listener functions will be invoked during this call, in the callers thread context.
procedure System_RunCallbacks(); cdecl; external WRAPPERLIB;
// register callbacks
procedure System_RegisterCallbacks(cb: TSteamCallbacks); cdecl; external WRAPPERLIB;
{$ENDREGION}
{$REGION 'SteamMusic'}

Type
  TAudioPlaybackStatus = (AudioPlayback_Undefined = 0, AudioPlayback_Playing = 1, AudioPlayback_Paused = 2, AudioPlayback_Idle = 3);

function SteamMusic_IsEnabled(): boolean; cdecl; external WRAPPERLIB;
function SteamMusic_IsPlaying(): boolean; cdecl; external WRAPPERLIB;

function SteamMusic_GetPlaybackStatus(): TAudioPlaybackStatus; cdecl; external WRAPPERLIB;

procedure SteamMusic_Play(); cdecl; external WRAPPERLIB;
procedure SteamMusic_Pause(); cdecl; external WRAPPERLIB;
procedure SteamMusic_PlayPrevious(); cdecl; external WRAPPERLIB;
procedure SteamMusic_PlayNext(); cdecl; external WRAPPERLIB;

// volume is between 0.0 and 1.0
procedure SteamMusic_SetVolume(Volume: single); cdecl; external WRAPPERLIB;
function SteamMusic_GetVolume(): single; cdecl; external WRAPPERLIB;
{$ENDREGION}
{$REGION 'SteamMusicRemote'}
// Service Definition
function SteamMusicRemote_RegisterSteamMusicRemote(Name: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
function SteamMusicRemote_DeregisterSteamMusicRemote(): boolean; cdecl; external WRAPPERLIB;
function SteamMusicRemote_IsCurrentMusicRemote(): boolean; cdecl; external WRAPPERLIB;
function SteamMusicRemote_ActivationSuccess(Value: boolean): boolean; cdecl; external WRAPPERLIB;

function SteamMusicRemote_SetDisplayName(DisplayName: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
function SteamMusicRemote_SetPNGIcon_64x64(Buffer: pointer; BufferLength: uint32): boolean; cdecl; external WRAPPERLIB;

// Abilities for the user interface
function SteamMusicRemote_EnablePlayPrevious(Value: boolean): boolean; cdecl; external WRAPPERLIB;
function SteamMusicRemote_EnablePlayNext(Value: boolean): boolean; cdecl; external WRAPPERLIB;
function SteamMusicRemote_EnableShuffled(Value: boolean): boolean; cdecl; external WRAPPERLIB;
function SteamMusicRemote_EnableLooped(Value: boolean): boolean; cdecl; external WRAPPERLIB;
function SteamMusicRemote_EnableQueue(Value: boolean): boolean; cdecl; external WRAPPERLIB;
function SteamMusicRemote_EnablePlaylists(Value: boolean): boolean; cdecl; external WRAPPERLIB;

// Status
function SteamMusicRemote_UpdatePlaybackStatus(Status: TAudioPlaybackStatus): boolean; cdecl; external WRAPPERLIB;
function SteamMusicRemote_UpdateShuffled(Value: boolean): boolean; cdecl; external WRAPPERLIB;
function SteamMusicRemote_UpdateLooped(Value: boolean): boolean; cdecl; external WRAPPERLIB;
function SteamMusicRemote_UpdateVolume(Value: single): boolean; cdecl; external WRAPPERLIB;

// Current Entry
function SteamMusicRemote_CurrentEntryWillChange(): boolean; cdecl; external WRAPPERLIB;
function SteamMusicRemote_CurrentEntryIsAvailable(Available: boolean): boolean; cdecl; external WRAPPERLIB;
function SteamMusicRemote_UpdateCurrentEntryText(Text: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
function SteamMusicRemote_UpdateCurrentEntryElapsedSeconds(Value: integer): boolean; cdecl; external WRAPPERLIB;
function SteamMusicRemote_UpdateCurrentEntryCoverArt(Buffer: pointer; BufferLength: uint32): boolean; cdecl; external WRAPPERLIB;
function SteamMusicRemote_CurrentEntryDidChange(): boolean; cdecl; external WRAPPERLIB;

// Queue
function SteamMusicRemote_QueueWillChange(): boolean; cdecl; external WRAPPERLIB;
function SteamMusicRemote_ResetQueueEntries(): boolean; cdecl; external WRAPPERLIB;
function SteamMusicRemote_SetQueueEntry(ID: integer; Position: integer; EntryText: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
function SteamMusicRemote_SetCurrentQueueEntry(ID: integer): boolean; cdecl; external WRAPPERLIB;
function SteamMusicRemote_QueueDidChange(): boolean; cdecl; external WRAPPERLIB;

// Playlist
function SteamMusicRemote_PlaylistWillChange(): boolean; cdecl; external WRAPPERLIB;
function SteamMusicRemote_ResetPlaylistEntries(): boolean; cdecl; external WRAPPERLIB;
function SteamMusicRemote_SetPlaylistEntry(ID: integer; Position: integer; EntryText: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
function SteamMusicRemote_SetCurrentPlaylistEntry(ID: integer): boolean; cdecl; external WRAPPERLIB;
function SteamMusicRemote_PlaylistDidChange(): boolean; cdecl; external WRAPPERLIB;
{$ENDREGION}
{$REGION 'SteamUtils'}
// return the number of seconds since the user
function SteamUtils_GetSecondsSinceAppActive(): uint32; cdecl; external WRAPPERLIB;
function SteamUtils_GetSecondsSinceComputerActive(): uint32; cdecl; external WRAPPERLIB;

// the universe this client is connecting to
function SteamUtils_GetConnectedUniverse(): TUniverse; cdecl; external WRAPPERLIB;

// Steam server time - in PST, number of seconds since January 1, 1970 (i.e unix time)
function SteamUtils_GetServerRealTime(): uint32; cdecl; external WRAPPERLIB;

// returns the 2 digit ISO 3166-1-alpha-2 format country code this client is running in (as looked up via an IP-to-location database)
// e.g "US" or "UK".
function SteamUtils_GetIPCountry(): pAnsiChar; cdecl; external WRAPPERLIB;

// returns true if the image exists; and valid sizes were filled out
function SteamUtils_GetImageSize(Image: integer; Width: pUint32; Height: pUint32): boolean; cdecl; external WRAPPERLIB;

// returns true if the image exists; and the buffer was successfully filled out
// results are returned in RGBA format
// the destination buffer size should be 4 * height * width * sizeof(char)
function SteamUtils_GetImageRGBA(Image: integer; Dest: pUint8; DestBufferSize: integer): boolean; cdecl; external WRAPPERLIB;

// returns the IP of the reporting server for valve - currently only used in Source engine games
function SteamUtils_GetCSERIPPort(IP: pUint32; Port: pUint16): boolean; cdecl; external WRAPPERLIB;

// return the amount of battery power left in the current system in % [0..100], 255 for being on AC power
function SteamUtils_GetCurrentBatteryPower(): uint8; cdecl; external WRAPPERLIB;

// returns the appID of the current process
function SteamUtils_GetAppID(): uint32; cdecl; external WRAPPERLIB;

// Sets the position where the overlay instance for the currently calling game should show notifications.
// This position is per-game and if this function is called from outside of a game context it will do nothing.
procedure SteamUtils_SetOverlayNotificationPosition(NotificationPosition: TNotificationPosition); cdecl; external WRAPPERLIB;

// API asynchronous call results
// can be used directly; but more commonly used via the callback dispatch API
function SteamUtils_IsAPICallCompleted(SteamAPICall: TSteamAPICall; Failed: pBoolean): boolean; cdecl; external WRAPPERLIB;
function SteamUtils_GetAPICallFailureReason(SteamAPICall: TSteamAPICall): TSteamAPICallFailure; cdecl; external WRAPPERLIB;
function SteamUtils_GetAPICallResult(SteamAPICall: TSteamAPICall; Callback: pointer; cCallback: integer; CallbackExpected: integer; Failed: pBoolean): boolean; cdecl; external WRAPPERLIB;

// returns the number of IPC calls made since the last time this function was called
// Used for perf debugging so you can understand how many IPC calls your game makes per frame
// Every IPC call is at minimum a thread context switch if not a process one so you want to rate
// control how often you do them.
function SteamUtils_GetIPCCallCount(): uint32; cdecl; external WRAPPERLIB;

// API warning handling
// 'integer' is the severity; 0 for msg, 1 for warning
// 'pAnsiChar' is the text of the message
// callbacks will occur directly after the API function is called that generated the warning or message
procedure SteamUtils_SetWarningMessageHook(Fn: TSteamAPIWarningMessageHook); cdecl; external WRAPPERLIB;

// Returns true if the overlay is running & the user can access it. The overlay process could take a few seconds to
// start & hook the game process; so this function will initially return false while the overlay is loading.
function SteamUtils_IsOverlayEnabled(): boolean; cdecl; external WRAPPERLIB;

// Normally this call is unneeded if your game has a constantly running frame loop that calls the
// D3D Present API; or OGL SwapBuffers API every frame.
//
// However; if you have a game that only refreshes the screen on an event driven basis then that can break
// the overlay; as it uses your Present/SwapBuffers calls to drive it's internal frame loop and it may also
// need to Present() to the screen any time an even needing a notification happens or when the overlay is
// brought up over the game by a user.  You can use this API to ask the overlay if it currently need a present
// in that case; and then you can check for this periodically (roughly 33hz is desirable) and make sure you
// refresh the screen with Present or SwapBuffers to allow the overlay to do it's work.
function SteamUtils_OverlayNeedsPresent(): boolean; cdecl; external WRAPPERLIB;

// Activates the Big Picture text input dialog which only supports gamepad input
function SteamUtils_ShowGamepadTextInput(InputMode: TGamepadTextInputMode; LineInputMode: TGamepadTextInputLineMode; Description: pAnsiChar; CharMax: uint32; ExistingText: pAnsiChar): boolean; cdecl; external WRAPPERLIB;

// Returns previously entered text & length
function SteamUtils_GetEnteredGamepadTextLength(): uint32; cdecl; external WRAPPERLIB;
function SteamUtils_GetEnteredGamepadTextInput(Text: pAnsiChar; cText: uint32): boolean; cdecl; external WRAPPERLIB;

// returns the language the steam client is running in, you probably want SteamApps-GetCurrentGameLanguage instead; this is for very special usage cases
function SteamUtils_GetSteamUILanguage(): pAnsiChar; cdecl; external WRAPPERLIB;

// returns true if Steam itself is running in VR mod
function SteamUtils_IsSteamRunningInVR(): boolean; cdecl; external WRAPPERLIB;
{$ENDREGION}
{$REGION 'SteamVR'}
procedure SteamVR_Init(Error: pInteger); cdecl; external WRAPPERLIB;
procedure SteamVR_Shutdown(); cdecl; external WRAPPERLIB;
function SteamVR_IsHmdPresent(): boolean; cdecl; external WRAPPERLIB;

function SteamVR_GetStringForHmdError(Error: THmdError): pAnsiChar; cdecl; external WRAPPERLIB;
// ------------------------------------
// Display Methods
// ------------------------------------

// Size and position that the window needs to be on the VR display.
procedure SteamVR_GetWindowBounds(X, Y, Width, Height: pInt32); cdecl; external WRAPPERLIB;

// Suggested size for the intermediate render target that the distortion pulls from.
procedure SteamVR_GetRecommendedRenderTargetSize(Width, Height: pUint32); cdecl; external WRAPPERLIB;

// Gets the viewport in the frame buffer to draw the output of the distortion into
procedure SteamVR_GetEyeOutputViewport(Eye: THmdEye; X, Y, Width, Height: pUint32); cdecl; external WRAPPERLIB;

// The projection matrix for the specified eye
function SteamVR_GetProjectionMatrix(Eye: THmdEye; NearZ, FarZ: single; ProjType: TGraphicsAPIConvention): THmdMatrix44; cdecl; external WRAPPERLIB;

// The components necessary to build your own projection matrix in case your
// application is doing something fancy like infinite Z
procedure SteamVR_GetProjectionRaw(Eye: THmdEye; Left, Right, Top, Bottom: pSingle); cdecl; external WRAPPERLIB;

// Returns the result of the distortion function for the specified eye and input UVs. UVs go from 0;0 in
// the upper left of that eye's viewport and 1;1 in the lower right of that eye's viewport.
function SteamVR_ComputeDistortion(Eye: THmdEye; U, V: single): TDistortionCoordinates; cdecl; external WRAPPERLIB;

// Returns the transform from eye space to the head space. Eye space is the per-eye flavor of head
// space that provides stereo disparity. Instead of Model * View * Projection the sequence is Model * View * Eye^-1 * Projection.
// Normally View and Eye^-1 will be multiplied together and treated as View in your application.
function SteamVR_GetHeadFromEyePose(Eye: THmdEye): THmdMatrix34; cdecl; external WRAPPERLIB;

// For use in simple VR apps; this method returns the concatenation of the
// tracking pose and the eye matrix to get a full view matrix for each eye.
// This is ( GetEyeMatrix() ) * (GetWorldFromHeadPose() ^ -1 )
function SteamVR_GetViewMatrix(SecondsFromNow: single; MatLeftView, MatRightView: pHmdMatrix44; Result: pHmdTrackingResult): boolean; cdecl; external WRAPPERLIB;

// [D3D9 Only]
// Returns the adapter index that the user should pass into CreateDevice to set up D3D9 in such
// a way that it can go full screen exclusive on the HMD. Returns -1 if there was an error.
function SteamVR_GetD3D9AdapterIndex(): uint32; cdecl; external WRAPPERLIB;

// [D3D10/11 Only]
// Returns the adapter index and output index that the user should pass into EnumAdapters adn EnumOutputs
// to create the device and swap chain in DX10 and DX11. If an error occurs both indices will be set to -1.
procedure SteamVR_GetDXGIOutputInfo(AdapterIndex, AdapterOutputIndex: pInt32); cdecl; external WRAPPERLIB;

// [Windows Only]
// Notifies the system that the VR output will appear in a particular window.
procedure SteamVR_AttachToWindow(hWnd: pointer); cdecl; external WRAPPERLIB;

// ------------------------------------
// Tracking Methods
// ------------------------------------

// The pose that the tracker thinks that the HMD will be in at the specified
// number of seconds into the future. Pass 0 to get the current state.
//
// This is roughly analogous to the inverse of the view matrix in most applications; though
// many games will need to do some additional rotation or translation on top of the rotation
// and translation provided by the head pose.
//
// If this function returns true the pose has been populated with a pose that can be used by the application.
// Check Result for details about the pose; including messages that should be displayed to the user.

function SteamVR_GetTrackerFromHeadPose(PredictedSecondsFromNow: single; Pose: pHmdMatrix34; Result: pHmdTrackingResult): boolean; cdecl; external WRAPPERLIB;

// Passes back the pose matrix from the last successful call to GetWorldFromHeadPose(). Returns true if that matrix is
// valid (because there has been a previous successful pose.)
function SteamVR_GetLastTrackerFromHeadPose(Pose: pHmdMatrix34): boolean; cdecl; external WRAPPERLIB;

// Returns true if the tracker for this HMD will drift the Yaw component of its pose over time regardless of
// actual head motion. This is true for gyro-based trackers with no ground truth.
function SteamVR_WillDriftInYaw(): boolean; cdecl; external WRAPPERLIB;

// Sets the zero pose for the tracker coordinate system. After this call all WorldFromHead poses will be relative
// to the pose whenever this was called. The new zero coordinate system will not change the fact that the Y axis is
// up in the real world; so the next pose returned from GetWorldFromHeadPose after a call to ZeroTracker may not be
// exactly an identity matrix.
procedure SteamVR_ZeroTracker(); cdecl; external WRAPPERLIB;

// Returns the zero pose for the tracker coordinate system. If the tracker has never had a valid pose; this
// will be an identity matrix.
function SteamVR_GetTrackerZeroPose(): THmdMatrix34; cdecl; external WRAPPERLIB;

// ------------------------------------
// Administrative methods
// ------------------------------------

// The ID of the driver this HMD uses as a UTF-8 string. Returns the length of the ID in bytes. If
// the buffer is not large enough to fit the ID an empty string will be returned. In general, 128 bytes
// will be enough to fit any ID.
function SteamVR_GetDriverId(Buffer: pAnsiChar; BufferLen: uint32): uint32; cdecl; external WRAPPERLIB;

// The ID of this display within its driver this HMD uses as a UTF-8 string. Returns the length of the ID in bytes. If
// the buffer is not large enough to fit the ID an empty string will be returned. In general; 128 bytes
// will be enough to fit any ID.
function SteamVR_GetDisplayId(Buffer: pAnsiChar; BufferLen: uint32): uint32; cdecl; external WRAPPERLIB;
{$ENDREGION}
{$REGION 'SteamUser'}
// returns the TSteamUser this interface represents
// this is only used internally by the API, and by a few select interfaces that support multi-user
function SteamUser_GetSteamUser(): TSteamUser; cdecl; external WRAPPERLIB;

// returns true if the Steam client current has a live connection to the Steam servers.
// If false; it means there is no active connection due to either a networking issue on the local machine; or the Steam server is down/busy.
// The Steam client will automatically be trying to recreate the connection as often as possible.
function SteamUser_IsLoggedOn(): boolean; cdecl; external WRAPPERLIB;

// returns the TSteamID of the account currently logged into the Steam client
// a TSteamID is a unique identifier for an account; and used to differentiate users in all parts of the Steamworks API
procedure SteamUser_GetSteamID(var sid:TSteamID); cdecl; external WRAPPERLIB;

// Multiplayer Authentication functions

// InitiateGameConnection() starts the state machine for authenticating the game client with the game server
// It is the client portion of a three-way handshake between the client; the game server; and the steam servers
//
// Parameters:
// pointer AuthBlob - a pointer to empty memory that will be filled in with the authentication token.
// int MaxAuthBlob - the number of bytes of allocated memory in pBlob. Should be at least 2048 bytes.
// TSteamID steamIDGameServer - the steamID of the game server; received from the game server by the client
// TGameID gameID - the ID of the current game. For games without mods; this is just TGameID( <appID> )
// uint32 IPServer; uint16 PortServer - the IP address of the game server
// boolean Secure - whether or not the client thinks that the game server is reporting itself as secure (i.e. VAC is running)
//
// return value - returns the number of bytes written to Blob. If the return is 0; then the buffer passed in was too small; and the call has failed
// The contents of Blob should then be sent to the game server; for it to use to complete the authentication process.
function SteamUser_InitiateGameConnection(AuthBlob: pointer; MaxAuthBlob: integer; steamIDGameServer: TSteamID; IPServer: uint32; PortServer: uint16; Secure: boolean): integer; cdecl; external WRAPPERLIB;

// notify of disconnect
// needs to occur when the game client leaves the specified game server; needs to match with the InitiateGameConnection() call
procedure SteamUser_TerminateGameConnection(IPServer: uint32; PortServer: uint16); cdecl; external WRAPPERLIB;

// Legacy functions

// used by only a few games to track usage events
procedure SteamUser_TrackAppUsageEvent(gameID: TGameID; AppUsageEvent: integer; ExtraInfo: pAnsiChar); cdecl; external WRAPPERLIB;

// get the local storage folder for current Steam account to write application data; e.g. save games; configs etc.
// this will usually be something like "C:\Progam Files\Steam\userdata\<SteamID>\<AppID>\local"
function SteamUser_GetUserDataFolder(Buffer: pAnsiChar; cBuffer: integer): boolean; cdecl; external WRAPPERLIB;

// Starts voice recording. Once started; use GetVoice() to get the data
procedure SteamUser_StartVoiceRecording(); cdecl; external WRAPPERLIB;

// Stops voice recording. Because people often release push-to-talk keys early; the system will keep recording for
// a little bit after this function is called. GetVoice() should continue to be called until it returns
// k_eVoiceResultNotRecording
procedure SteamUser_StopVoiceRecording(); cdecl; external WRAPPERLIB;

// Determine the amount of captured audio data that is available in bytes.
// This provides both the compressed and uncompressed data. Please note that the uncompressed
// data is not the raw feed from the microphone: data may only be available if audible
// levels of speech are detected.
// UncompressedVoiceDesiredSampleRate is necessary to know the number of bytes to return in cUncompressed - can be set to 0 if you don't need uncompressed (the usual case)
// If you're upgrading from an older Steamworks API; you'll want to pass in 11025 to nUncompressedVoiceDesiredSampleRate
function SteamUser_GetAvailableVoice(Compressed, cUncompressed: pUint32; UncompressedVoiceDesiredSampleRate: uint32): TVoiceResult; cdecl; external WRAPPERLIB;

// Gets the latest voice data from the microphone. Compressed data is an arbitrary format, and is meant to be handed back to
// DecompressVoice() for playback later as a binary blob. Uncompressed data is 16-bit, signed integer, 11025Hz PCM format.
// Please note that the uncompressed data is not the raw feed from the microphone: data may only be available if audible
// levels of speech are detected, and may have passed through denoising filters; etc.
// This function should be called as often as possible once recording has started; once per frame at least.
// BytesWritten is set to the number of bytes written to pDestBuffer.
// UncompressedBytesWritten is set to the number of bytes written to UncompressedDestBuffer.
// You must grab both compressed and uncompressed here at the same time, if you want both.
// Matching data that is not read during this call will be thrown away.
// GetAvailableVoice() can be used to determine how much data is actually available.
// If you're upgrading from an older Steamworks API, you'll want to pass in 11025 to UncompressedVoiceDesiredSampleRate
function SteamUser_GetVoice(WantCompressed: boolean; DestBuffer: pointer; cDestBufferSize: uint32; BytesWritten: pUint32; WantUncompressed: boolean; UncompressedDestBuffer: pointer; cUncompressedDestBufferSize: uint32; nUncompressBytesWritten: pUint32; UncompressedVoiceDesiredSampleRate: uint32): TVoiceResult; cdecl; external WRAPPERLIB;

// Decompresses a chunk of compressed data produced by GetVoice().
// nBytesWritten is set to the number of bytes written to DestBuffer unless the return value is k_TVoiceResultBufferTooSmall.
// In that case; BytesWritten is set to the size of the buffer required to decompress the given
// data. The suggested buffer size for the destination buffer is 22 kilobytes.
// The output format of the data is 16-bit signed at the requested samples per second.
// If you're upgrading from an older Steamworks API; you'll want to pass in 11025 to DesiredSampleRate
function SteamUser_DecompressVoice(Compressed: pointer; cCompressed: uint32; DestBuffer: pointer; DestBufferSize: uint32; BytesWritten: pUint32; DesiredSampleRate: uint32): TVoiceResult; cdecl; external WRAPPERLIB;

// This returns the frequency of the voice data as it's stored internally; calling DecompressVoice() with this size will yield the best results
function SteamUser_GetVoiceOptimalSampleRate(): uint32; cdecl; external WRAPPERLIB;

// Retrieve ticket to be sent to the entity who wishes to authenticate you.
// cTicket retrieves the length of the actual ticket.
function SteamUser_GetAuthSessionTicket(Ticket: pointer; MaxTicket: integer; cTicket: pUint32): TAuthTicket; cdecl; external WRAPPERLIB;

// Authenticate ticket from entity steamID to be sure it is valid and isnt reused
// Registers for callbacks if the entity goes offline or cancels the ticket ( see ValidateAuthTicketResponse callback and EAuthSessionResponse )
function SteamUser_BeginAuthSession(AuthTicket: pointer; cAuthTicket: integer; steamID: TSteamID): TBeginAuthSessionResult; cdecl; external WRAPPERLIB;

// Stop tracking started by BeginAuthSession - called when no longer playing game with this entity
procedure SteamUser_EndAuthSession(steamID: TSteamID); cdecl; external WRAPPERLIB;

// Cancel auth ticket from GetAuthSessionTicket; called when no longer playing game with the entity you gave the ticket to
procedure SteamUser_CancelAuthTicket(AuthTicket: TAuthTicket); cdecl; external WRAPPERLIB;

// After receiving a user's authentication data; and passing it to BeginAuthSession, use this function
// to determine if the user owns downloadable content specified by the provided AppID.
function SteamUser_UserHasLicenseForApp(steamID: TSteamID; appID: TAppId): TUserHasLicenseForAppResult; cdecl; external WRAPPERLIB;

// returns true if this users looks like they are behind a NAT device. Only valid once the user has connected to steam
// (i.e a SteamServersConnected_t has been issued) and may not catch all forms of NAT.
function SteamUser_IsBehindNAT(): boolean; cdecl; external WRAPPERLIB;

// set data to be replicated to friends so that they can join your game
// TSteamID steamIDGameServer - the steamID of the game server; received from the game server by the client
// uint32 unIPServer; uint16 usPortServer - the IP address of the game server
procedure SteamUser_AdvertiseGame(steamIDGameServer: TSteamID; IPServer: uint32; PortServer: uint16); cdecl; external WRAPPERLIB;

// Requests a ticket encrypted with an app specific shared key
// DataToInclude; cDataToInclude will be encrypted into the ticket
// ( This is asynchronous, you must wait for the ticket to be completed by the server )
function SteamUser_RequestEncryptedAppTicket(DataToInclude: pointer; cDataToInclude: integer): TSteamAPICall; cdecl; external WRAPPERLIB;

// retrieve a finished ticket
function SteamUser_GetEncryptedAppTicket(Ticket: pointer; MaxTicket: integer; cTicket: pUint32): boolean; cdecl; external WRAPPERLIB;

// Trading Card badges data access
// if you only have one set of cards; the series will be 1
// the user has can have two different badges for a series; the regular (max level 5) and the foil (max level 1)
function SteamUser_GetGameBadgeLevel(Series: integer; Foil: boolean): integer; cdecl; external WRAPPERLIB;

// gets the Steam Level of the user; as shown on their profile
function SteamUser_GetPlayerSteamLevel(): integer; cdecl; external WRAPPERLIB;
{$ENDREGION}
{$REGION 'SteamHTMLSurface'}
function SteamHTMLSurface_Init: boolean; cdecl; external WRAPPERLIB;
function SteamHTMLSurface_Shutdown: boolean; cdecl; external WRAPPERLIB;

function SteamHTMLSurface_CreateBrowser(cb: THTMLSurfaceCallbacks; UserAgen, UserCSS: pAnsiChar): integer; cdecl; external WRAPPERLIB;
procedure SteamHTMLSurface_LoadURL(ID: uint32; URL: pAnsiChar); cdecl; external WRAPPERLIB;
procedure SteamHTMLSurface_Resize(ID: uint32; Width, Height: uint32); cdecl; external WRAPPERLIB;
procedure SteamHTMLSurface_RemoveBrowser(ID: uint32); cdecl; external WRAPPERLIB;

procedure SteamHTMLSurface_MouseUp(ID: uint32; MouseButton: THTMLMouseButton); cdecl; external WRAPPERLIB;
procedure SteamHTMLSurface_MouseDown(ID: uint32; MouseButton: THTMLMouseButton); cdecl; external WRAPPERLIB;
procedure SteamHTMLSurface_MouseDoubleClick(ID: uint32; MouseButton: THTMLMouseButton); cdecl; external WRAPPERLIB;
procedure SteamHTMLSurface_MouseMove(ID: uint32; X, Y: integer); cdecl; external WRAPPERLIB;
procedure SteamHTMLSurface_MouseWheel(ID: uint32; Delta: int32); cdecl; external WRAPPERLIB;

procedure SteamHTMLSurface_KeyDown(ID: uint32; NativeKeyCode: uint32; HTMLKeyModifiers: THTMLKeyModifiers); cdecl; external WRAPPERLIB;
procedure SteamHTMLSurface_KeyUp(ID: uint32; NativeKeyCode: uint32; HTMLKeyModifiers: THTMLKeyModifiers); cdecl; external WRAPPERLIB;
procedure SteamHTMLSurface_KeyChar(ID: uint32; UnicodeChar: uint32; HTMLKeyModifiers: THTMLKeyModifiers); cdecl; external WRAPPERLIB;

procedure SteamHTMLSurface_AllowStartRequest(ID: integer; Allowed: boolean); cdecl; external WRAPPERLIB;
procedure SteamHTMLSurface_JSDialogResponse(ID: integer; Result: boolean); cdecl; external WRAPPERLIB;
procedure SteamHTMLSurface_FileLoadDialogResponse(ID: integer; SelectedFiles: ppAnsiChar); cdecl; external WRAPPERLIB;

// Stop the load of the current html page
procedure SteamHTMLSurface_StopLoad(ID: integer); cdecl; external WRAPPERLIB;
// Reload (most likely from local cache) the current page
procedure SteamHTMLSurface_Reload(ID: integer); cdecl; external WRAPPERLIB;
// navigate back in the page history
procedure SteamHTMLSurface_GoBack(ID: integer); cdecl; external WRAPPERLIB;
// navigate forward in the page history
procedure SteamHTMLSurface_GoForward(ID: integer); cdecl; external WRAPPERLIB;

// add this header to any url requests from this browser
procedure SteamHTMLSurface_AddHeader(ID: integer; Key, Value: pAnsiChar); cdecl; external WRAPPERLIB;
// run this javascript script in the currently loaded page
procedure SteamHTMLSurface_ExecuteJavascript(ID: integer; Script: pAnsiChar); cdecl; external WRAPPERLIB;

// programmatically scroll this many pixels on the page
procedure SteamHTMLSurface_SetHorizontalScroll(ID: integer; AbsolutePixelScroll: uint32); cdecl; external WRAPPERLIB;
procedure SteamHTMLSurface_SetVerticalScroll(ID: integer; AbsolutePixelScroll: uint32); cdecl; external WRAPPERLIB;

// tell the html control if it has key focus currently, controls showing the I-beam cursor in text controls amongst other things
procedure SteamHTMLSurface_SetKeyFocus(ID: integer; HasKeyFocus: boolean); cdecl; external WRAPPERLIB;

// open the current pages html code in the local editor of choice, used for debugging
procedure SteamHTMLSurface_ViewSource(ID: integer); cdecl; external WRAPPERLIB;
// copy the currently selected text on the html page to the local clipboard
procedure SteamHTMLSurface_CopyToClipboard(ID: integer); cdecl; external WRAPPERLIB;
// paste from the local clipboard to the current html page
procedure SteamHTMLSurface_PasteFromClipboard(ID: integer); cdecl; external WRAPPERLIB;

// find this string in the browser, if bCurrentlyInFind is true then instead cycle to the next matching element
procedure SteamHTMLSurface_Find(ID: integer; SearchStr: pAnsiChar; CurrentlyInFind, Reverse: boolean); cdecl; external WRAPPERLIB;
// cancel a currently running find
procedure SteamHTMLSurface_StopFind(ID: integer); cdecl; external WRAPPERLIB;

// return details about the link at position x,y on the current page
procedure SteamHTMLSurface_GetLinkAtPosition(ID: integer; X, Y: integer); cdecl; external WRAPPERLIB;

// set a webcookie for the hostname in question
procedure SteamHTMLSurface_SetCookie(Hostname, Key, Value, Path: pAnsiChar; Expires: RTime32); cdecl; external WRAPPERLIB;

// Zoom the current page by flZoom ( from 0.0 to 4.0, so to zoom to 120% use 1.2 ), zooming around point X,Y in the page (use 0,0 if you don't care)
procedure SteamHTMLSurface_SetPageScaleFactor(ID: integer; Zoom: single; PointX, PointY: integer); cdecl; external WRAPPERLIB;
{$ENDREGION}
{$REGION 'SteamUserStats'}
// Ask the server to send down this user's data and achievements for this game
function SteamUserStats_RequestCurrentStats(): boolean; cdecl; external WRAPPERLIB;

// Data accessors
function SteamUserStats_GetStatInt32(Name: pAnsiChar; Data: pInt32): boolean; cdecl; external WRAPPERLIB;
function SteamUserStats_GetStatFloat(Name: pAnsiChar; Data: pSingle): boolean; cdecl; external WRAPPERLIB;

// Set / update data
function SteamUserStats_SetStatInt32(Name: pAnsiChar; Data: pInt32): boolean; cdecl; external WRAPPERLIB;
function SteamUserStats_SetStatFloat(Name: pAnsiChar; Data: pSingle): boolean; cdecl; external WRAPPERLIB;
function SteamUserStats_UpdateAvgRateStat(Name: pAnsiChar; CountThisSession: single; SessionLength: double): boolean; cdecl; external WRAPPERLIB;

// Achievement flag accessors
function SteamUserStats_GetAchievement(Name: pAnsiChar; Achieved: pBoolean): boolean; cdecl; external WRAPPERLIB;
function SteamUserStats_SetAchievement(Name: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
function SteamUserStats_ClearAchievement(Name: pAnsiChar): boolean; cdecl; external WRAPPERLIB;

// Get the achievement status, and the time it was unlocked if unlocked.
// If the return value is true, but the unlock time is zero;  that means it was unlocked before Steam
// began tracking achievement unlock times (December 2009). Time is seconds since January 1, 1970.
function SteamUserStats_GetAchievementAndUnlockTime(Name: pAnsiChar; Achieved: pBoolean; UnlockTime: pUint32): boolean; cdecl; external WRAPPERLIB;

// Store the current data on the server;  will get a callback when set
// And one callback for every new achievement
//
// If the callback has a result of k_EResultInvalidParam;  one or more stats
// uploaded has been rejected;  either because they broke constraints
// or were out of date. In this case the server sends back updated values.
// The stats should be re-iterated to keep in sync.
function SteamUserStats_StoreStats(): boolean; cdecl; external WRAPPERLIB;

// Achievement / GroupAchievement metadata

// Gets the icon of the achievement, which is a handle to be used in SteamUtils-GetImageRGBA(), or 0 if none set.
// A return value of 0 may indicate we are still fetching data, and you can wait for the UserAchievementIconFetched_t callback
// which will notify you when the bits are ready. If the callback still returns zero, then there is no image set for the
// specified achievement.
function SteamUserStats_GetAchievementIcon(Name: pAnsiChar): integer; cdecl; external WRAPPERLIB;

// Get general attributes for an achievement. Accepts the following keys:
// - "name" and "desc" for retrieving the localized achievement name and description (returned in UTF8)
// - "hidden" for retrieving if an achievement is hidden (returns "0" when not hidden, "1" when hidden)
function SteamUserStats_GetAchievementDisplayAttribute(Name, Key: pAnsiChar): pAnsiChar; cdecl; external WRAPPERLIB;

// Achievement progress - triggers an AchievementProgress callback, that is all.
// Calling this w/ N out of N progress will NOT set the achievement, the game must still do that.
function SteamUserStats_IndicateAchievementProgress(Name: pAnsiChar; CurProgress, MaxProgress: uint32): boolean; cdecl; external WRAPPERLIB;

// Used for iterating achievements. In general games should not need these functions because they should have a
// list of existing achievements compiled into them
function SteamUserStats_GetNumAchievements(): uint32; cdecl; external WRAPPERLIB;
// Get achievement name iAchievement in [0, GetNumAchievements)
function SteamUserStats_GetAchievementName(Achievement: uint32): pAnsiChar; cdecl; external WRAPPERLIB;

// Friends stats & achievements

// downloads stats for the user
// returns a UserStatsReceived_t received when completed
// if the other user has no stats, UserStatsReceived_t.m_eResult will be set to k_EResultFail
// these stats won't be auto-updated; you'll need to call RequestUserStats() again to refresh any data
function SteamUserStats_RequestUserStats(steamIDUser: TSteamID): TSteamAPICall; cdecl; external WRAPPERLIB;

// requests stat information for a user, usable after a successful call to RequestUserStats()
function SteamUserStats_GetUserStatInt32(steamIDUser: TSteamID; Name: pAnsiChar; Data: pInt32): boolean; cdecl; external WRAPPERLIB;
function SteamUserStats_GetUserStatFloat(steamIDUser: TSteamID; Name: pAnsiChar; Data: pSingle): boolean; cdecl; external WRAPPERLIB;
function SteamUserStats_GetUserAchievement(steamIDUser: TSteamID; Name: pAnsiChar; Achieved: pBoolean): boolean; cdecl; external WRAPPERLIB;
// See notes for GetAchievementAndUnlockTime above
function SteamUserStats_GetUserAchievementAndUnlockTime(steamIDUser: TSteamID; Name: pAnsiChar; Achieved: pBoolean; UnlockTime: pUint32): boolean; cdecl; external WRAPPERLIB;

// Reset stats
function SteamUserStats_ResetAllStats(AchievementsToo: boolean): boolean; cdecl; external WRAPPERLIB;

// Leaderboard functions

// asks the Steam back-end for a leaderboard by name;  and will create it if it's not yet
// This call is asynchronous;  with the result returned in LeaderboardFindResult_t
function SteamUserStats_FindOrCreateLeaderboard(LeaderboardName: pAnsiChar; LeaderboardSortMethod: TLeaderboardSortMethod; LeaderboardDisplayType: TLeaderboardDisplayType): TSteamAPICall; cdecl; external WRAPPERLIB;

// as above;  but won't create the leaderboard if it's not found
// This call is asynchronous;  with the result returned in LeaderboardFindResult_t
function SteamUserStats_FindLeaderboard(LeaderboardName: pAnsiChar): TSteamAPICall; cdecl; external WRAPPERLIB;

// returns the name of a leaderboard
function SteamUserStats_GetLeaderboardName(SteamLeaderboard: TSteamLeaderboard): pAnsiChar; cdecl; external WRAPPERLIB;

// returns the total number of entries in a leaderboard;  as of the last request
function SteamUserStats_GetLeaderboardEntryCount(SteamLeaderboard: TSteamLeaderboard): integer; cdecl; external WRAPPERLIB;

// returns the sort method of the leaderboard
function SteamUserStats_GetLeaderboardSortMethod(SteamLeaderboard: TSteamLeaderboard): TLeaderboardSortMethod; cdecl; external WRAPPERLIB;

// returns the display type of the leaderboard
function SteamUserStats_GetLeaderboardDisplayType(SteamLeaderboard: TSteamLeaderboard): TLeaderboardDisplayType; cdecl; external WRAPPERLIB;

// Asks the Steam back-end for a set of rows in the leaderboard.
// This call is asynchronous, with the result returned in LeaderboardScoresDownloaded_t
// LeaderboardScoresDownloaded_t will contain a handle to pull the results from GetDownloadedLeaderboardEntries() (below)
// You can ask for more entries than exist;  and it will return as many as do exist.
// k_ELeaderboardDataRequestGlobal requests rows in the leaderboard from the full table, with nRangeStart & nRangeEnd in the range [1, TotalEntries]
// k_ELeaderboardDataRequestGlobalAroundUser requests rows around the current user;  RangeStart being negate
// e.g. DownloadLeaderboardEntries( hLeaderboard,  k_ELeaderboardDataRequestGlobalAroundUser, -3,  3 ) will return 7 rows;  3 before the user, 3 after
// k_ELeaderboardDataRequestFriends requests all the rows for friends of the current user
function SteamUserStats_DownloadLeaderboardEntries(SteamLeaderboard: TSteamLeaderboard; LeaderboardDataRequest: TLeaderboardDataRequest; RangeStart, RangeEnd: integer): TSteamAPICall; cdecl; external WRAPPERLIB;
// as above;  but downloads leaderboard entries for an arbitrary set of users - ELeaderboardDataRequest is k_ELeaderboardDataRequestUsers
// if a user doesn't have a leaderboard entry, they won't be included in the result
// a max of 100 users can be downloaded at a time;  with only one outstanding call at a time
function SteamUserStats_DownloadLeaderboardEntriesForUsers(SteamLeaderboard: TSteamLeaderboard; Users: pSteamID; cUsers: integer): TSteamAPICall; cdecl; external WRAPPERLIB;

// Returns data about a single leaderboard entry
// use a for loop from 0 to LeaderboardScoresDownloaded_t-EntryCount to get all the downloaded entries
function SteamUserStats_GetDownloadedLeaderboardEntry(SteamLeaderboardEntries: TSteamLeaderboardEntries; index: integer; LeaderboardEntry: pLeaderboardEntry; Details: pInt32; DetailsMax: integer): boolean; cdecl; external WRAPPERLIB;

// Uploads a user score to the Steam back-end.
// This call is asynchronous;  with the result returned in LeaderboardScoreUploaded
// Details are extra game-defined information regarding how the user got that score
// pScoreDetails points to an array of int32's;  cScoreDetailsCount is the number of int32's in the list
Function SteamUserStats_UploadLeaderboardScore(SteamLeaderboard: TSteamLeaderboard; LeaderboardUploadScoreMethod: TLeaderboardUploadScoreMethod; Score: int32; ScoreDetails: pInt32; ScoreDetailsCount: integer): TSteamAPICall; cdecl; external WRAPPERLIB;

// Attaches a piece of user generated content the user's entry on a leaderboard.
// hContent is a handle to a piece of user generated content that was shared using ISteamUserRemoteStorage::FileShare().
// This call is asynchronous;  with the result returned in LeaderboardUGCSet_t.
function SteamUserStats_AttachLeaderboardUGC(SteamLeaderboard: TSteamLeaderboard; UGC: TUGCHandle): TSteamAPICall; cdecl; external WRAPPERLIB;

// Retrieves the number of players currently playing your game (online + offline)
// This call is asynchronous;  with the result returned in NumberOfCurrentPlayers
function SteamUserStats_GetNumberOfCurrentPlayers(): TSteamAPICall; cdecl; external WRAPPERLIB;

// Requests that Steam fetch data on the percentage of players who have received each achievement
// for the game globally.
// This call is asynchronous, with the result returned in GlobalAchievementPercentagesReady.
function SteamUserStats_RequestGlobalAchievementPercentages(): TSteamAPICall; cdecl; external WRAPPERLIB;

// Get the info on the most achieved achievement for the game;  returns an iterator index you can use to fetch
// the next most achieved afterwards.  Will return -1 if there is no data on achievement
// percentages (ie;  you haven't called RequestGlobalAchievementPercentages and waited on the callback).
function SteamUserStats_GetMostAchievedAchievementInfo(Name: pAnsiChar; NameBufLen: uint32; Percent: pSingle; Achieved: pBoolean): integer; cdecl; external WRAPPERLIB;

// Get the info on the next most achieved achievement for the game. Call this after GetMostAchievedAchievementInfo or another
// GetNextMostAchievedAchievementInfo call passing the iterator from the previous call. Returns -1 after the last
// achievement has been iterated.
function SteamUserStats_GetNextMostAchievedAchievementInfo(IteratorPrevious: integer; Name: pAnsiChar; NameBufLen: uint32; Percent: pSingle; Achieved: pBoolean): integer; cdecl; external WRAPPERLIB;

// Returns the percentage of users who have achieved the specified achievement.
function SteamUserStats_GetAchievementAchievedPercent(Name: pAnsiChar; Percent: pSingle): boolean; cdecl; external WRAPPERLIB;

// Requests global stats data;  which is available for stats marked as "aggregated".
// This call is asynchronous;  with the results returned in GlobalStatsReceived.
// nHistoryDays specifies how many days of day-by-day history to retrieve in addition
// to the overall totals. The limit is 60.
function SteamUserStats_RequestGlobalStats(HistoryDays: integer): TSteamAPICall; cdecl; external WRAPPERLIB;

// Gets the lifetime totals for an aggregated stat
function SteamUserStats_GetGlobalStatInt64(StatName: pAnsiChar; Data: pint64): boolean; cdecl; external WRAPPERLIB;
function SteamUserStats_GetGlobalStatDouble(StatName: pAnsiChar; Data: pDouble): boolean; cdecl; external WRAPPERLIB;

// Gets history for an aggregated stat. Data will be filled with daily values, starting with today.
// So when called, Data[0] will be today, Data[1] will be yesterday, and Data[2] will be two days ago;
// etc. cData is the size in bytes of the pubData buffer. Returns the number of
// elements actually set.
function SteamUserStats_GetGlobalStatHistoryInt64(StatName: pAnsiChar; Data: pint64; cData: uint32): int32; cdecl; external WRAPPERLIB;
function SteamUserStats_GetGlobalStatHistoryDouble(StatName: pAnsiChar; Data: pDouble; cData: uint32): int32; cdecl; external WRAPPERLIB;
{$ENDREGION}
{$REGION 'SteamUnifiedMessages'}
// Sends a service method (in binary serialized form) using the Steam Client.
// Returns a unified message handle (k_InvalidUnifiedMessageHandle if could not send the message).
function SteamUnifiedMessages_SendMethod(ServiceMethod: pAnsiChar; RequestBuffer: pointer; RequestBufferSize: uint32; Context: uint64): TClientUnifiedMessageHandle; cdecl; external WRAPPERLIB;

// Gets the size of the response and the EResult. Returns false if the response is not ready yet.
function SteamUnifiedMessages_GetMethodResponseInfo(Handle: TClientUnifiedMessageHandle; ResponseSize: pointer; Result: pResult): boolean; cdecl; external WRAPPERLIB;

// Gets a response in binary serialized form (and optionally release the corresponding allocated memory).
function SteamUnifiedMessages_GetMethodResponseData(Handle: TClientUnifiedMessageHandle; ResponseBuffer: pointer; ResponseBufferSize: uint32; AutoRelease: boolean): boolean; cdecl; external WRAPPERLIB;

// Releases the message and its corresponding allocated memory.
function SteamUnifiedMessages_ReleaseMethod(Handle: TClientUnifiedMessageHandle): boolean; cdecl; external WRAPPERLIB;

// Sends a service notification (in binary serialized form) using the Steam Client.
// Returns true if the notification was sent successfully.
function SteamUnifiedMessages_SendNotification(ServiceNotification: pAnsiChar; NotificationBuffer: pointer; NotificationBufferSize: uint32): boolean; cdecl; external WRAPPERLIB;
{$ENDREGION}
{$REGION 'SteamUGC'}
// Query UGC associated with a user. Creator app id or consumer app id must be valid and be set to the current running app. unPage should start at 1.
function SteamUGC_CreateQueryUserUGCRequest(AccountID: uint32; ListType, eMatchingUGCType, eSortOrder: integer; nCreatorAppID, nConsumerAppId, unPage: uint32): uint64; cdecl; external WRAPPERLIB;
// Query for all matching UGC. Creator app id or consumer app id must be valid and be set to the current running app. unPage should start at 1.
function SteamUGC_CreateQueryAllUGCRequest(QueryType, MatchingeMatchingUGCTypeFileType: integer; nCreatorAppID, nConsumerAppId, unPage: uint32): uint64; cdecl; external WRAPPERLIB;
// Send the query to Steam
function SteamUGC_SendQueryUGCRequest(Handle: uint64): uint64; cdecl; external WRAPPERLIB;
// Retrieve an individual result after receiving the callback for querying UGC
function SteamUGC_GetQueryUGCResult(Handle: uint64; index: uint32; Details: pSteamUGCDetails): boolean; cdecl; external WRAPPERLIB;
// Release the request to free up memory;after retrieving results
function SteamUGC_ReleaseQueryUGCRequest(Handle: uint64): boolean; cdecl; external WRAPPERLIB;
// Options to set for querying UGC
function SteamUGC_AddRequiredTag(Handle: uint64; TagName: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
function SteamUGC_AddExcludedTag(Handle: uint64; TagName: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
function SteamUGC_SetReturnLongDescription(Handle: uint64; ReturnLongDescription: boolean): boolean; cdecl; external WRAPPERLIB;
function SteamUGC_SetReturnTotalOnly(Handle: uint64; ReturnTotalOnly: boolean): boolean; cdecl; external WRAPPERLIB;
function SteamUGC_SetAllowCachedResponse(Handle: uint64; MaxAgeSeconds: uint32): boolean; cdecl; external WRAPPERLIB;
// Options only for querying user UGC
function SteamUGC_SetCloudFileNameFilter(Handle: uint64; MatchCloudFileName: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
// Options only for querying all UGC
function SteamUGC_SetMatchAnyTag(Handle: uint64; MatchAnyTag: boolean): boolean; cdecl; external WRAPPERLIB;
function SteamUGC_SetSearchText(Handle: uint64; SearchText: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
function SteamUGC_SetRankedByTrendDays(Handle: uint64; Days: uint32): boolean; cdecl; external WRAPPERLIB;
// Request full details for one piece of UGC
function SteamUGC_RequestUGCDetails(PublishedFileId: uint64; MaxAgeSeconds: uint32): uint64; cdecl; external WRAPPERLIB;
// Steam Workshop Creator API
function SteamUGC_CreateItem(nConsumerAppId: uint32; eFileType: integer): uint64; cdecl; external WRAPPERLIB;
function SteamUGC_StartItemUpdate(nConsumerAppId: uint32; PublishedFileId: uint64): uint64; cdecl; external WRAPPERLIB;
function SteamUGC_SetItemTitle(Handle: uint64; Title: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
function SteamUGC_SetItemDescription(Handle: uint64; Description: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
function SteamUGC_SetItemVisibility(Handle: uint64; eVisibility: integer): boolean; cdecl; external WRAPPERLIB;
function SteamUGC_SetItemTags(updateHandle: uint64; pTags: pSteamParamStringArray): boolean; cdecl; external WRAPPERLIB;
function SteamUGC_SetItemContent(Handle: uint64; ContentFolder: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
function SteamUGC_SetItemPreview(Handle: uint64; PreviewFile: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
function SteamUGC_SubmitItemUpdate(Handle: uint64; ChangeNote: pAnsiChar): uint64; cdecl; external WRAPPERLIB;
function SteamUGC_GetItemUpdateProgress(Handle: uint64; BytesProcessed, BytesTotal: puint64): integer; cdecl; external WRAPPERLIB;
// Steam Workshop Consumer API
function SteamUGC_SubscribeItem(PublishedFileId: uint64): uint64; cdecl; external WRAPPERLIB;
function SteamUGC_UnsubscribeItem(PublishedFileId: uint64): uint64; cdecl; external WRAPPERLIB;
function SteamUGC_GetNumSubscribedItems(): uint32; cdecl; external WRAPPERLIB;
function SteamUGC_GetSubscribedItems(PublishedFileId: puint64; MaxEntries: uint32): uint32; cdecl; external WRAPPERLIB;
// Get info about the item on disk. If you are supporting items published through the legacy RemoteStorage APIs then *LegacyItem will be set to true
// and pchFolder will contain the full path to the file rather than the containing folder.
function SteamUGC_GetItemInstallInfo(PublishedFileId: uint64; SizeOnDisk: puint64; Folder: pAnsiChar; FolderSize: uint32; LegacyItem: pBoolean): boolean; cdecl; external WRAPPERLIB;
function SteamUGC_GetItemUpdateInfo(PublishedFileId: uint64; NeedsUpdate: pBoolean; IsDownloading: pBoolean; BytesDownloadedCount: puint64; BytesTotalCount: puint64): boolean; cdecl; external WRAPPERLIB;
{$ENDREGION}
{$REGION 'SteamScreenshots'}
// Writes a screenshot to the user's screenshot library given the raw image data,which must be in RGB format.
// The return value is a handle that is valid for the duration of the game process and can be used to apply tags.
function SteamScreenshots_WriteScreenshot(pRGB: pointer; cubRGB: uint32; Width, Height: integer): uint32; cdecl; external WRAPPERLIB;
// Adds a screenshot to the user's screenshot library from disk. If a thumbnail is provided,it must be 200 pixels wide and the same aspect ratio
// as the screenshot,otherwise a thumbnail will be generated if the user uploads the screenshot. The screenshots must be in either JPEG or TGA format.
// The return value is a handle that is valid for the duration of the game process and can be used to apply tags.
// JPEG,TGA and PNG formats are supported.
function SteamScreenshots_AddScreenshotToLibrary(Filename: pAnsiChar; ThumbnailFilename: pAnsiChar; Width, Height: integer): uint32; cdecl; external WRAPPERLIB;
// Causes the Steam overlay to take a screenshot. If screenshots are being hooked by the game then a ScreenshotRequested_t callback is sent back to the game instead.
procedure SteamScreenshots_TriggerScreenshot(); cdecl; external WRAPPERLIB;
// Toggles whether the overlay handles screenshots when the user presses the screenshot hotkey, or the game handles them. If the game is hooking screenshots;
// then the ScreenshotRequested_t callback will be sent if the user presses the hotkey,and the game is expected to call WriteScreenshot or AddScreenshotToLibrary
// in response.
procedure SteamScreenshots_HookScreenshots(Hook: boolean); cdecl; external WRAPPERLIB;
// Sets metadata about a screenshot's location (for example: the name of the map)
function SteamScreenshots_SetLocation(Screenshot: uint32; Location: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
// Tags a user as being visible in the screenshot
function SteamScreenshots_TagUser(Screenshot: uint32; steamID: TSteamID): boolean; cdecl; external WRAPPERLIB;
// Tags a published file as being visible in the screenshot
function SteamScreenshots_TagPublishedFile(Screenshot: uint32; PublishedFileId: uint64): boolean; cdecl; external WRAPPERLIB;
{$ENDREGION}
{$REGION 'SteamRemoteStorage'}
// NOTE
//
// Filenames are case-insensitive;and will be converted to lowercase automatically.
// So "foo.bar" and "Foo.bar" are the same file;and if you write "Foo.bar" then
// iterate the files;the filename returned will be "foo.bar".
//

// file operations
function SteamRemoteStorage_FileWrite(Filename: pAnsiChar; pData: pointer; DataSize: uint32): boolean; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_FileRead(Filename: pAnsiChar; pData: pointer; DataSizeToRead: uint32): int32; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_FileForget(Filename: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_FileDelete(Filename: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_FileShare(Filename: pAnsiChar): uint64; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_SetSyncPlatforms(Filename: pAnsiChar; eRemoteStoragePlatform: integer): boolean; cdecl; external WRAPPERLIB;
// file operations that cause network IO
function SteamRemoteStorage_FileWriteStreamOpen(Filename: pAnsiChar): uint64; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_FileWriteStreamWriteChunk(writeHandle: uint64; pData: pointer; DataSize: uint32): boolean; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_FileWriteStreamClose(writeHandle: uint64): boolean; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_FileWriteStreamCancel(writeHandle: uint64): boolean; cdecl; external WRAPPERLIB;
// file information
function SteamRemoteStorage_FileExists(Filename: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_FilePersisted(Filename: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_GetFileSize(Filename: pAnsiChar): int32; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_GetFileTimestamp(Filename: pAnsiChar): int64; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_GetSyncPlatforms(Filename: pAnsiChar): integer; cdecl; external WRAPPERLIB;
// iteration
function SteamRemoteStorage_GetFileCount(): int32; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_GetFileNameAndSize(iFile: integer; FileSizeInBytes: pUint32): pAnsiChar; cdecl; external WRAPPERLIB;
// configuration management
function SteamRemoteStorage_GetQuota(TotalBytes: pUint32; AvailableBytes: pUint32): boolean; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_IsCloudEnabledForAccount(): boolean; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_IsCloudEnabledForApp(): boolean; cdecl; external WRAPPERLIB;
procedure SteamRemoteStorage_SetCloudEnabledForApp(IsEnabled: boolean); cdecl; external WRAPPERLIB;
// user generated content

// Downloads a UGC file. A priority value of 0 will download the file immediately;
// otherwise it will wait to download the file until all downloads with a lower priority
// value are completed. Downloads with equal priority will occur simultaneously.
function SteamRemoteStorage_UGCDownload(Content: uint64; Priority: uint32): uint64; cdecl; external WRAPPERLIB;
// Gets the amount of data downloaded so far for a piece of content. pnBytesExpected can be 0 if function returns false cdecl; external WRAPPERLIB;
// or if the transfer hasn't started yet;so be careful to check for that before dividing to get a percentage
function SteamRemoteStorage_GetUGCDownloadProgress(Content: uint64; BytesDownloaded: pUint32; BytesExpected: pUint32): boolean; cdecl; external WRAPPERLIB;
// Gets metadata for a file after it has been downloaded. This is the same metadata given in the RemoteStorageDownloadUGCResult_t call result
function SteamRemoteStorage_GetUGCDetails(Content: uint64; pnAppID: puint64; Name: pAnsiChar; FileSizeInBytes: pUint32; OwnerID: pSteamID): boolean; cdecl; external WRAPPERLIB;
// After download;gets the content of the file.
// Small files can be read all at once by calling this function with an offset of 0 and cubDataToRead equal to the size of the file. cdecl; external WRAPPERLIB;
// Larger files can be read in chunks to reduce memory usage (since both sides of the IPC client and the game itself must allocate
// enough memory for each chunk). Once the last byte is read;the file is implicitly closed and further calls to UGCRead will fail
// unless UGCDownload is called again.
// For especially large files (anything over 100MB) it is a requirement that the file is read in chunks.
function SteamRemoteStorage_UGCRead(Content: uint64; pData: pointer; DataSizeToRead: uint32; cOffset: uint32; eAction: integer): int32; cdecl; external WRAPPERLIB;
// Functions to iterate through UGC that has finished downloading but has not yet been read via UGCRead()
function SteamRemoteStorage_GetCachedUGCCount(): int32; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_GetCachedUGCHandle(iCachedContent: uint32): uint64; cdecl; external WRAPPERLIB;
// publishing UGC
function SteamRemoteStorage_PublishWorkshopFile(Filename, PreviewFile: pAnsiChar; nConsumerAppId: uint32; Title: pAnsiChar; Description: pAnsiChar; Visibility: integer; pTags: pSteamParamStringArray; EWorkshopFileType: integer): uint64; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_CreatePublishedFileUpdateRequest(PublishedFileId: uint64): uint64; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_UpdatePublishedFileFile(updateHandle: uint64; Filename: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_UpdatePublishedFilePreviewFile(updateHandle: uint64; PreviewFile: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_UpdatePublishedFileTitle(updateHandle: uint64; Title: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_UpdatePublishedFileDescription(updateHandle: uint64; Description: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_UpdatePublishedFileVisibility(updateHandle: uint64; eVisibility: integer): boolean; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_UpdatePublishedFileTags(updateHandle: uint64; pTags: pSteamParamStringArray): boolean; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_CommitPublishedFileUpdate(updateHandle: uint64): uint64; cdecl; external WRAPPERLIB;
// Gets published file details for the given publishedfileid. If unMaxSecondsOld is greater than 0;
// cached data may be returned;depending on how long ago it was cached. A value of 0 will force a refresh.
// A value of k_WorkshopForceLoadPublishedFileDetailsFromCache will use cached data if it exists;no matter how old it is.
function SteamRemoteStorage_GetPublishedFileDetails(unPublishedFileId: uint64; unMaxSecondsOld: uint32): uint64; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_DeletePublishedFile(unPublishedFileId: uint64): uint64; cdecl; external WRAPPERLIB;
// enumerate the files that the current user published with this app
function SteamRemoteStorage_EnumerateUserPublishedFiles(unStartIndex: uint32): uint64; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_SubscribePublishedFile(unPublishedFileId: uint64): uint64; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_EnumerateUserSubscribedFiles(unStartIndex: uint32): uint64; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_UnsubscribePublishedFile(unPublishedFileId: uint64): uint64; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_UpdatePublishedFileSetChangeDescription(updateHandle: uint64; ChangeDescription: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_GetPublishedItemVoteDetails(PublishedFileId: uint64): uint64; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_UpdateUserPublishedItemVote(PublishedFileId: uint64; IsVoteUp: boolean): uint64; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_GetUserPublishedItemVoteDetails(unPublishedFileId: uint64): uint64; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_EnumerateUserSharedWorkshopFiles(steamID: TSteamID; unStartIndex: uint32; RequiredTags, ExcludedTags: pSteamParamStringArray): uint64; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_PublishVideo(eVideoProvider: integer; VideoAccount: pAnsiChar; VideoIdentifier: pAnsiChar; PreviewFile: pAnsiChar; nConsumerAppId: uint32; Title, Description: pAnsiChar; Visibility: integer; Tags: pSteamParamStringArray): uint64; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_SetUserPublishedFileAction(unPublishedFileId: uint64; Action: integer): uint64; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_EnumeratePublishedFilesByUserAction(Action: integer; StartIndex: uint32): uint64; cdecl; external WRAPPERLIB;
// this method enumerates the public view of workshop files
function SteamRemoteStorage_EnumeratePublishedWorkshopFiles(eEnumerationType: integer; StartIndex: uint32; Count: uint32; Days: uint32; Tags, UserTags: pSteamParamStringArray): uint64; cdecl; external WRAPPERLIB;
function SteamRemoteStorage_UGCDownloadToLocation(Content: uint64; Location: pAnsiChar; Priority: uint32): uint64; cdecl; external WRAPPERLIB;
{$ENDREGION}
{$REGION 'SteamNetworking'}
/// /////////////////////////////////////////////////////////////////////////////////////////
// Session-less connection functions
// automatically establishes NAT-traversing or Relay server connections

// Sends a P2P packet to the specified user
// UDP-like;unreliable and a max packet size of 1200 bytes
// the first packet send may be delayed as the NAT-traversal code runs
// if we can't get through to the user;an error will be posted via the callback P2PSessionConnectFail_t
// see EP2PSend enum above for the descriptions of the different ways of sending packets
//
// nChannel is a routing number you can use to help route message to different systems 	- you'll have to call ReadP2PPacket()
// with the same channel number in order to retrieve the data on the other end
// using different channels to talk to the same user will still use the same underlying p2p connection;saving on resources
function SteamNetworking_SendP2PPacket(RemoteID: TSteamID; pData: pointer; DataSize: uint32; P2PSendType, nChannel: integer): boolean; cdecl; external WRAPPERLIB;
// returns true if any data is available for read;and the amount of data that will need to be read
function SteamNetworking_IsP2PPacketAvailable(MsgSize: pUint32; nChannel: integer): boolean; cdecl; external WRAPPERLIB;
// reads in a packet that has been sent from another user via SendP2PPacket()
// returns the size of the message and the steamID of the user who sent it in the last two parameters
// if the pBuffer passed in is too small;the message will be truncated
// this call is not blocking;and will return false if no data is available
function SteamNetworking_ReadP2PPacket(pDest: pointer; cubDest: uint32; MsgSize: pUint32; RemoteID: pSteamID; nChannel: integer): boolean; cdecl; external WRAPPERLIB;
// AcceptP2PSessionWithUser() should only be called in response to a P2PSessionRequest_t callback
// P2PSessionRequest_t will be posted if another user tries to send you a packet that you haven't talked to yet
// if you don't want to talk to the user;just ignore the request
// if the user continues to send you packets;another P2PSessionRequest_t will be posted periodically
// this may be called multiple times for a single user
// (if you've called SendP2PPacket() on the other user;this implicitly accepts the session request)
function SteamNetworking_AcceptP2PSessionWithUser(RemoteID: TSteamID): boolean; cdecl; external WRAPPERLIB;
// call CloseP2PSessionWithUser() when you're done talking to a user;will free up resources under-the-hood
// if the remote user tries to send data to you again;another P2PSessionRequest_t callback will be posted
function SteamNetworking_CloseP2PSessionWithUser(RemoteID: TSteamID): boolean; cdecl; external WRAPPERLIB;
// call CloseP2PChannelWithUser() when you're done talking to a user on a specific channel. Once all channels
// open channels to a user have been closed;the open session to the user will be closed and new data from this
// user will trigger a P2PSessionRequest_t callback
function SteamNetworking_CloseP2PChannelWithUser(RemoteID: TSteamID; Channel: integer): boolean; cdecl; external WRAPPERLIB;
// fills out P2PSessionState_t structure with details about the underlying connection to the user
// should only needed for debugging purposes
// returns false if no connection exists to the specified user
function SteamNetworking_GetP2PSessionState(RemoteID: TSteamID; ConnectionState: pP2PSessionState): boolean; cdecl; external WRAPPERLIB;
// Allow P2P connections to fall back to being relayed through the Steam servers if a direct connection
// or NAT-traversal cannot be established. Only applies to connections created after setting this value;
// or to existing connections that need to automatically reconnect after this value is set.
//
// P2P packet relay is allowed by default
function SteamNetworking_AllowP2PPacketRelay(IsAllow: boolean): boolean; cdecl; external WRAPPERLIB;
/// /////////////////////////////////////////////////////////////////////////////////////////
// LISTEN / CONNECT style interface functions
//
// This is an older set of functions designed around the Berkeley TCP sockets model
// it's preferential that you use the above P2P functions;they're more robust
// and these older functions will be removed eventually
//
/// /////////////////////////////////////////////////////////////////////////////////////////
// creates a socket and listens others to connect
// will trigger a SocketStatusCallback_t callback on another client connecting
// nVirtualP2PPort is the unique ID that the client will connect to;in case you have multiple ports
// this can usually just be 0 unless you want multiple sets of connections
// unIP is the local IP address to bind to
// pass in 0 if you just want the default local IP
// unPort is the port to use
// pass in 0 if you don't want users to be able to connect via IP/Port;but expect to be always peer-to-peer connections only
function SteamNetworking_CreateListenSocket(VirtualP2PPort: integer; IP: uint32; Port: uint16; AllowUseOfPacketRelay: boolean): uint32; cdecl; external WRAPPERLIB;
// creates a socket and begin connection to a remote destination
// can connect via a known steamID (client or game server);or directly to an IP
// on success will trigger a SocketStatusCallback_t callback
// on failure or timeout will trigger a SocketStatusCallback_t callback with a failure code in m_eSNetSocketState
function SteamNetworking_CreateP2PConnectionSocket(TargetID: TSteamID; VirtualPort, TimeoutSec: integer; AllowUseOfPacketRelay: boolean): uint32; cdecl; external WRAPPERLIB;
// disconnects the connection to the socket;if any;and invalidates the handle
// any unread data on the socket will be thrown away
// if bNotifyRemoteEnd is set;socket will not be completely destroyed until the remote end acknowledges the disconnect
function SteamNetworking_CreateConnectionSocket(IP: uint32; Port: uint16; TimeoutSec: integer): uint32; cdecl; external WRAPPERLIB;
// destroying a listen socket will automatically kill all the regular sockets generated from it
function SteamNetworking_DestroySocket(Socket: uint32; IsNotifyRemoteEnd: boolean): boolean; cdecl; external WRAPPERLIB;
function SteamNetworking_DestroyListenSocket(Socket: uint32; IsNotifyRemoteEnd: boolean): boolean; cdecl; external WRAPPERLIB;
// sending data
// must be a handle to a connected socket
// data is all sent via UDP;and thus send sizes are limited to 1200 bytes; after this;many routers will start dropping packets
// use the reliable flag with caution; although the resend rate is pretty aggressive;
// it can still cause stalls in receiving data (like TCP)
function SteamNetworking_SendDataOnSocket(Socket: uint32; pData: pointer; DataSize: uint32; IsReliable: boolean): boolean; cdecl; external WRAPPERLIB;
// receiving data
// returns false if there is no data remaining
// fills out *pcubMsgSize with the size of the next message;in bytes
function SteamNetworking_IsDataAvailableOnSocket(Socket: uint32; MsgSize: pUint32): boolean; cdecl; external WRAPPERLIB;
// fills in pubDest with the contents of the message
// messages are always complete;of the same size as was sent (i.e. packetized;not streaming)
// if *pcubMsgSize < cubDest;only partial data is written
// returns false if no data is available
function SteamNetworking_RetrieveDataFromSocket(Socket: uint32; Dest: pointer; DestSize: uint32; MsgSize: pUint32): boolean; cdecl; external WRAPPERLIB;
// checks for data from any socket that has been connected off this listen socket
// returns false if there is no data remaining
// fills out *pcubMsgSize with the size of the next message;in bytes
// fills out *phSocket with the socket that data is available on
function SteamNetworking_IsDataAvailable(ListenSocket: uint32; MsgSize: pUint32; Socket: pUint32): boolean; cdecl; external WRAPPERLIB;
// retrieves data from any socket that has been connected off this listen socket
// fills in pubDest with the contents of the message
// messages are always complete;of the same size as was sent (i.e. packetized;not streaming)
// if *pcubMsgSize < cubDest;only partial data is written
// returns false if no data is available
// fills out *phSocket with the socket that data is available on
function SteamNetworking_RetrieveData(ListenSocket: uint32; Dest: pointer; DestSize: uint32; MsgSize: pUint32; phSocket: pUint32): boolean; cdecl; external WRAPPERLIB;
// returns information about the specified socket;filling out the contents of the s
function SteamNetworking_GetSocketInfo(Socket: uint32; RemoteID: pSteamID; SocketStatus: pInteger; RemoteIP: pUint32; PortRemote: pUint16): boolean; cdecl; external WRAPPERLIB;
// returns which local port the listen socket is bound to
// *pnIP and *pnPort will be 0 if the socket is set to listen for P2P connections only
function SteamNetworking_GetListenSocketInfo(ListenSocket: uint32; IP: pUint32; Port: pUint16): boolean; cdecl; external WRAPPERLIB;
// returns true to describe how the socket ended up connecting
function SteamNetworking_GetSocketConnectionType(Socket: uint32): integer; cdecl; external WRAPPERLIB;
// max packet size;in bytes
function SteamNetworking_GetMaxPacketSize(Socket: uint32): integer; cdecl; external WRAPPERLIB;
{$ENDREGION}
{$REGION 'SteamMatchmaking'}
function SteamMatchmaking_GetFavoriteGameCount(): integer; cdecl; external WRAPPERLIB;
// returns the details of the game server
// Game is of range [0, GetFavoriteGameCount())
// IP,ConnPort are filled in the with IP:port of the game server
// Flags specify whether the game server was stored as an explicit favorite or in the history of connections
// RTime32LastPlayedOnServer is filled in the with the Unix time the favorite was added
function SteamMatchmaking_GetFavoriteGame(GameIndex: integer; AppNum: pUint32; IP: pUint32; ConnPort: pUint16; QueryPort: pUint16; Flags: pUint32; RTime32LastPlayedOnServer: pUint32): boolean; cdecl; external WRAPPERLIB;
// adds the game server to the local list, updates the time played of the server if it already exists in the list
function SteamMatchmaking_AddFavoriteGame(AppNum: uint32; nIP: uint32; ConnPort: uint16; QueryPort: uint16; Flags: uint32; RTime32LastPlayedOnServer: uint32): integer; cdecl; external WRAPPERLIB;
// removes the game server from the local storage, returns true if one was removed
function SteamMatchmaking_RemoveFavoriteGame(AppNum: uint32; IP: uint32; ConnPort: uint16; QueryPort: uint16; Flags: uint32): boolean; cdecl; external WRAPPERLIB;
// Get a list of relevant lobbies
// this is an asynchronous request
// results will be returned by LobbyMatchList callback & call result, with the number of lobbies found
// this will never return lobbies that are full
// to add more filter, the filter calls below need to be call before each and every SteamMatсhmaking_RequestLobbyList() call
function SteamMatchmaking_RequestLobbyList(): TSteamAPICall; cdecl; external WRAPPERLIB;
// filters for lobbies
// this needs to be called before RequestLobbyList() to take effect
// these are cleared on each call to RequestLobbyList()
procedure SteamMatchmaking_AddRequestLobbyListStringFilter(KeyToMatch: pAnsiChar; ValueToMatch: pAnsiChar; ComparisonType: integer); cdecl; external WRAPPERLIB;
// numerical comparison
procedure SteamMatchmaking_AddRequestLobbyListNumericalFilter(KeyToMatch: pAnsiChar; ValueToMatch: integer; ComparisonType: integer); cdecl; external WRAPPERLIB;
// returns results closest to the specified value. Multiple near filters can be added, with early filters taking precedence
procedure SteamMatchmaking_AddRequestLobbyListNearValueFilter(KeyToMatch: pAnsiChar; ValueToBeCloseTo: integer); cdecl; external WRAPPERLIB;
// returns only lobbies with the specified number of slots available
procedure SteamMatchmaking_AddRequestLobbyListFilterSlotsAvailable(SlotsAvailable: integer); cdecl; external WRAPPERLIB;
// sets the distance for which we should search for lobbies (based on users IP address to location map on the Steam backed)
procedure SteamMatchmaking_AddRequestLobbyListDistanceFilter(LobbyDistanceFilter: integer); cdecl; external WRAPPERLIB;
// sets how many results to return;the lower the count the faster it is to download the lobby results & details to the client
procedure SteamMatchmaking_AddRequestLobbyListResultCountFilter(MaxResults: integer); cdecl; external WRAPPERLIB;
procedure SteamMatchmaking_AddRequestLobbyListCompatibleMembersFilter(LobbyID: TSteamID); cdecl; external WRAPPERLIB;
// returns the TSteamID of a lobby,as retrieved by a RequestLobbyList call
// should only be called after a LobbyMatchList_t callback is received
// LobbyIndex is of the range [0,LobbyMatchList-LobbiesMatching)
function SteamMatchmaking_GetLobbyByIndex(LobbyIndex: integer): TSteamID; cdecl; external WRAPPERLIB;
// Create a lobby on the Steam servers.
// If private;then the lobby will not be returned by any RequestLobbyList() call; the TSteamID
// of the lobby will need to be communicated via game channels or via InviteUserToLobby()
// this is an asynchronous request
// results will be returned by LobbyCreated callback and call result; lobby is joined & ready to use at this point
// a LobbyEnter_t callback will also be received (since the local user is joining their own lobby)
function SteamMatchmaking_CreateLobby(LobbyType: TLobbyType; MaxMembers: integer): uint64; cdecl; external WRAPPERLIB;
// Joins an existing lobby
// this is an asynchronous request
// results will be returned by LobbyEnter callback & call result;check m_EChatRoomEnterResponse to see if was successful
// lobby metadata is available to use immediately on this call completing
function SteamMatchmaking_JoinLobby(LobbyID: TSteamID): uint64; cdecl; external WRAPPERLIB;
// Leave a lobby, this will take effect immediately on the client side
// other users in the lobby will be notified by a LobbyChatUpdate callback
procedure SteamMatchmaking_LeaveLobby(LobbyID: TSteamID); cdecl; external WRAPPERLIB;
// Invite another user to the lobby
// the target user will receive a LobbyInvite_t callback
// will return true if the invite is successfully sent;whether or not the target responds
// returns false if the local user is not connected to the Steam servers
// if the other user clicks the join link;a GameLobbyJoinRequested_t will be posted if the user is in-game;
// or if the game isn't running yet the game will be launched with the parameter +connect_lobby <64-bit lobby id>
function SteamMatchmaking_InviteUserToLobby(LobbyID, InviteID: TSteamID): boolean; cdecl; external WRAPPERLIB;
// Lobby iteration;for viewing details of users in a lobby
// only accessible if the lobby user is a member of the specified lobby
// persona information for other lobby members (name;avatar;etc.) will be asynchronously received
// and accessible via ISteamFriends interface
// returns the number of users in the specified lobby
function SteamMatchmaking_GetNumLobbyMembers(LobbyID: TSteamID): integer; cdecl; external WRAPPERLIB;
// returns the TSteamID of a user in the lobby
// iMember is of range [0, GetNumLobbyMembers())
// note that the current user must be in a lobby to retrieve TSteamIDs of other users in that lobby
function SteamMatchmaking_GetLobbyMemberByIndex(LobbyID: TSteamID; Member: integer): TSteamID; cdecl; external WRAPPERLIB;
// Get data associated with this lobby
// takes a simple key;and returns the string associated with it
// "" will be returned if no value is set,or if steamIDLobby is invalid
function SteamMatchmaking_GetLobbyData(LobbyID: TSteamID; Key: pAnsiChar): pAnsiChar; cdecl; external WRAPPERLIB;
// Sets a key/value pair in the lobby metadata
// each user in the lobby will be broadcast this new value;and any new users joining will receive any existing data
// this can be used to set lobby names,map,etc.
// to reset a key;just set it to ""
// other users in the lobby will receive notification of the lobby data change via a LobbyDataUpdate_t callback
function SteamMatchmaking_SetLobbyData(LobbyID: TSteamID; Key: pAnsiChar; Value: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
// returns the number of metadata keys set on the specified lobby
function SteamMatchmaking_GetLobbyDataCount(LobbyID: TSteamID): integer; cdecl; external WRAPPERLIB;
// returns a lobby metadata key/values pair by index;of range [0;GetLobbyDataCount())
function SteamMatchmaking_GetLobbyDataByIndex(LobbyID: TSteamID; LobbyDataIndex: integer; Key: pAnsiChar; KeyBufferSize: integer; Value: pAnsiChar; ValueBufferSize: integer): boolean; cdecl; external WRAPPERLIB;
// removes a metadata key from the lobby
function SteamMatchmaking_DeleteLobbyData(LobbyID: TSteamID; Key: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
// Gets per-user metadata for someone in this lobby
function SteamMatchmaking_GetLobbyMemberData(LobbyID: TSteamID; UserID: TSteamID; Key: pAnsiChar): pAnsiChar; cdecl; external WRAPPERLIB;
// Sets per-user metadata (for the local user implicitly)
procedure SteamMatchmaking_SetLobbyMemberData(LobbyID: TSteamID; Key: pAnsiChar; Value: pAnsiChar); cdecl; external WRAPPERLIB;
// Broadcasts a chat message to the all the users in the lobby
// users in the lobby (including the local user) will receive a LobbyChatMsg_t callback
// returns true if the message is successfully sent
// pvMsgBody can be binary or text data;up to 4k
// if pvMsgBody is text;cubMsgBody should be strlen(text) + 1;to include the null terminator
function SteamMatchmaking_SendLobbyChatMsg(LobbyID: TSteamID; pMsgBody: pointer; MsgBodySize: integer): boolean; cdecl; external WRAPPERLIB;
// Get a chat message as specified in a LobbyChatMsg_t callback
// iChatID is the LobbyChatMsg_t:m_iChatID value in the callback
// *pSteamIDUser is filled in with the TSteamID of the member
// *pvData is filled in with the message itself
// return value is the number of bytes written into the pBuffer
function SteamMatchmaking_GetLobbyChatEntry(LobbyID: TSteamID; ChatNum: integer; UserID: pSteamID; Data: pointer; DataSize: integer; ChatEntryType: integer): pInteger; cdecl; external WRAPPERLIB;
// Refreshes metadata for a lobby you're not necessarily in right now
// you never do this for lobbies you're a member of;only if your
// this will send down all the metadata associated with a lobby
// this is an asynchronous call
// returns false if the local user is not connected to the Steam servers
// results will be returned by a LobbyDataUpdate_t callback
// if the specified lobby doesn't exist;LobbyDataUpdate_t:m_bSuccess will be set to false
function SteamMatchmaking_RequestLobbyData(LobbyID: TSteamID): boolean; cdecl; external WRAPPERLIB;
// sets the game server associated with the lobby
// usually at this point;the users will join the specified game server
// either the IP/Port or the steamID of the game server has to be valid;depending on how you want the clients to be able to connect
procedure SteamMatchmaking_SetLobbyGameServer(LobbyID: TSteamID; GameServerIP: uint32; GameServerPort: uint16; GameServerID: TSteamID); cdecl; external WRAPPERLIB;
// returns the details of a game server set in a lobby - returns false if there is no game server set;or that lobby doesn't exist
function SteamMatchmaking_GetLobbyGameServer(LobbyID: TSteamID; GameServerIP: pUint32; GameServerPort: pUint16; GameServerID: pSteamID): boolean; cdecl; external WRAPPERLIB;
// set the limit on the # of users who can join the lobby
function SteamMatchmaking_SetLobbyMemberLimit(LobbyID: TSteamID; MaxMembers: integer): boolean; cdecl; external WRAPPERLIB;
// returns the current limit on the # of users who can join the lobby; returns 0 if no limit is defined
function SteamMatchmaking_GetLobbyMemberLimit(LobbyID: TSteamID): integer; cdecl; external WRAPPERLIB;
// updates which type of lobby it is
// only lobbies that are k_ELobbyTypePublic or k_ELobbyTypeInvisible;and are set to joinable;will be returned by RequestLobbyList() calls
function SteamMatchmaking_SetLobbyType(LobbyID: TSteamID; LobbyType: integer): boolean; cdecl; external WRAPPERLIB;
// sets whether or not a lobby is joinable - defaults to true for a new lobby
// if set to false;no user can join;even if they are a friend or have been invited
function SteamMatchmaking_SetLobbyJoinable(LobbyID: TSteamID; IsLobbyJoinable: boolean): boolean; cdecl; external WRAPPERLIB;
// returns the current lobby owner
// you must be a member of the lobby to access this
// there always one lobby owner - if the current owner leaves;another user will become the owner
// it is possible (bur rare) to join a lobby just as the owner is leaving;thus entering a lobby with self as the owner
function SteamMatchmaking_GetLobbyOwner(LobbyID: TSteamID): TSteamID; cdecl; external WRAPPERLIB;
// changes who the lobby owner is
// you must be the lobby owner for this to succeed;and steamIDNewOwner must be in the lobby
// after completion;the local user will no longer be the owner
function SteamMatchmaking_SetLobbyOwner(LobbyID: TSteamID; NewOwnerID: TSteamID): boolean; cdecl; external WRAPPERLIB;
// link two lobbies for the purposes of checking player compatibility
// you must be the lobby owner of both lobbies
function SteamMatchmaking_SetLinkedLobby(SteamIDLobby: TSteamID; SteamIDLobbyDependent: TSteamID): boolean; cdecl; external WRAPPERLIB;
{$ENDREGION}
{$REGION 'SteamMatchmakingServers'}
// [temporary: TEST] function SteamMathmaking_RequestInternetServerList(App: TAppId; Filters: ppMatchMakingKeyValuePair; cFilters: uint32; OnServerResponded: TOnServerResponded; OnServerFailedToRespond: TOnServerFailedToRespond; OnServerRefreshComplete: TOnServerRefreshComplete): TServerListRequest; cdecl; external WRAPPERLIB;

// Request a new list of servers of a particular type. These calls each correspond to one of the EMatchMakingType values.
// Each call allocates a new asynchronous request object.
// Request object must be released by calling SteamMatchmakingServers_ReleaseRequest(ServerListRequest)
function SteamMatchmakingServers_RequestInternetServerList(App: TAppId; Filters: ppMatchMakingKeyValuePair; nFilters: uint32; a: TOnServerResponded; b: TOnServerFailedToRespond; c: TOnServerRefreshComplete): pointer; cdecl; external WRAPPERLIB;
function SteamMatchmakingServers_RequestLANServerList(App: TAppId; a: TOnServerResponded; b: TOnServerFailedToRespond; c: TOnServerRefreshComplete): pointer; cdecl; external WRAPPERLIB;
function SteamMatchmakingServers_RequestFriendsServerList(App: TAppId; Filters: ppMatchMakingKeyValuePair; nFilters: uint32; a: TOnServerResponded; b: TOnServerFailedToRespond; c: TOnServerRefreshComplete): pointer; cdecl; external WRAPPERLIB;
function SteamMatchmakingServers_RequestFavoritesServerList(App: TAppId; Filters: ppMatchMakingKeyValuePair; nFilters: uint32; a: TOnServerResponded; b: TOnServerFailedToRespond; c: TOnServerRefreshComplete): pointer; cdecl; external WRAPPERLIB;
function SteamMatchmakingServers_RequestHistoryServerList(App: TAppId; Filters: ppMatchMakingKeyValuePair; nFilters: uint32; a: TOnServerResponded; b: TOnServerFailedToRespond; c: TOnServerRefreshComplete): pointer; cdecl; external WRAPPERLIB;
function SteamMatchmakingServers_RequestSpectatorServerList(App: TAppId; Filters: ppMatchMakingKeyValuePair; nFilters: uint32; a: TOnServerResponded; b: TOnServerFailedToRespond; c: TOnServerRefreshComplete): pointer; cdecl; external WRAPPERLIB;

// Releases the asynchronous request object and cancels any pending query on it if there's a pending query in progress.
// RefreshComplete callback is not posted when request is released.
procedure SteamMathmaking_ReleaseRequest(ServerListRequest: TServerListRequest); cdecl; external WRAPPERLIB;

// -----------------------------------------------------------------------------

procedure SteamMathmaking_GetServerDetails(ServerListRequest: TServerListRequest; Server: integer; var ServerDetails: TServerDetails); cdecl; external WRAPPERLIB;
procedure SteamMathmaking_SetServerDetails(ServerListRequest: TServerListRequest; Server: integer; QueryPort, ConnectionPort: uint16; IP: uint32; Name: pAnsiChar); cdecl; external WRAPPERLIB;
// Cancel an request which is operation on the given list type.  You should call this to cancel
// any in-progress requests before destructing a callback object that may have been passed
// to one of the above list request calls.  Not doing so may result in a crash when a callback
// occurs on the destructed object.
// Canceling a query does not release the allocated request handle.
// The request handle must be released using ReleaseRequest( Request )
procedure SteamMathmaking_CancelQuery(Request: TServerListRequest); cdecl; external WRAPPERLIB;
// Ping every server in your list again but don't update the list of servers
// Query callback installed when the server list was requested will be used
// again to post notifications and RefreshComplete, so the callback must remain
// valid until another RefreshComplete is called on it or the request
// is released with ReleaseRequest( Request )
procedure SteamMathmaking_RefreshQuery(Request: TServerListRequest); cdecl; external WRAPPERLIB;
// Returns true if the list is currently refreshing its server list
function SteamMathmaking_IsRefreshing(Request: TServerListRequest): boolean; cdecl; external WRAPPERLIB;
// How many servers in the given list, GetServerDetails above takes 0... GetServerCount() - 1
function SteamMathmaking_GetServerCount(Request: TServerListRequest): integer; cdecl; external WRAPPERLIB;
// Refresh a single server inside of a query (rather than all the servers )
procedure SteamMathmaking_RefreshServer(Request: TServerListRequest; Server: integer); cdecl; external WRAPPERLIB;

// -----------------------------------------------------------------------------
// Queries to individual servers directly via IP/Port
// -----------------------------------------------------------------------------
// Request updated ping time and other details from a single server
procedure SteamMathmaking_GetPingServer(IP: uint32; Port: uint16; var ServerDetails: TServerDetails; var IsFailure: boolean); cdecl; external WRAPPERLIB;
// Request the list of players currently playing on a server
function SteamMathmaking_GetPlayerDetails(IP: uint32; Port: uint16; OnAddPlayerToList: TOnAddPlayerToList; OnPlayersFailedToRespond, OnPlayersRefreshComplete: TSimpleEvent): TServerQuery; cdecl; external WRAPPERLIB;
// Request the list of rules that the server is running (See SteamGameServer-SetKeyValue() to set the rules server side)
function SteamMathmaking_GetServerRules(IP: uint32; Port: uint16; OnRulesResponded: TRulesResponded; OnRulesFailedToRespond, OnRulesRefreshComplete: TSimpleEvent): TServerQuery; cdecl; external WRAPPERLIB;
// Cancel server query
procedure SteamMathmaking_CancelServerQuery(query: TServerQuery); cdecl; external WRAPPERLIB;
{$ENDREGION}
{$REGION 'SteamHTTP'}
// Initializes a new HTTP request,returning a handle to use in further operations on it. Requires
// the method (GET or POST) and the absolute URL for the request. Only http requests (ie;not https) are
// currently supported,so this string must start with http:// or https:// and should look like http://store.steampowered.com/app/250/
// or such.
function SteamHTTP_CreateHTTPRequest(eHTTPRequestMethod: integer; AbsoluteURL: pAnsiChar): uint32; cdecl; external WRAPPERLIB;
// Set a context value for the request;which will be returned in the HTTPRequestCompleted_t callback after
// sending the request. This is just so the caller can easily keep track of which callbacks go with which request data.
function SteamHTTP_SetHTTPRequestContextValue(Request: uint32; ulContextValue: uint64): boolean; cdecl; external WRAPPERLIB;
// Set a timeout in seconds for the HTTP request;must be called prior to sending the request. Default
// timeout is 60 seconds if you don't call this. Returns false if the handle is invalid;or the request
// has already been sent.
function SteamHTTP_SetHTTPRequestNetworkActivityTimeout(Request: uint32; unTimeoutSeconds: uint32): boolean; cdecl; external WRAPPERLIB;
// Set a request header value for the request;must be called prior to sending the request. Will
// return false if the handle is invalid or the request is already sent.
function SteamHTTP_SetHTTPRequestHeaderValue(Request: uint32; HeaderName: pAnsiChar; HeaderValue: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
// Set a GET or POST parameter value on the request;which is set will depend on the EHTTPMethod specified
// when creating the request. Must be called prior to sending the request. Will return false if the
// handle is invalid or the request is already sent.
function SteamHTTP_SetHTTPRequestGetOrPostParameter(Request: uint32; ParamName: pAnsiChar; ParamValue: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
// Sends the HTTP request;will return false on a bad handle;otherwise use SteamCallHandle to wait on
// asynchronous response via callback.
//
// Note: If the user is in offline mode in Steam;then this will add a only-if-cached cache-control
// header and only do a local cache lookup rather than sending any actual remote request.
function SteamHTTP_SendHTTPRequest(Request: uint32; CallHandle: puint64): boolean; cdecl; external WRAPPERLIB;
// Sends the HTTP request;will return false on a bad handle;otherwise use SteamCallHandle to wait on
// asynchronous response via callback for completion;and listen for HTTPRequestHeadersReceived callback and
// HTTPRequestDataReceived_t callbacks while streaming.
function SteamHTTP_SendHTTPRequestAndStreamResponse(Request: uint32; CallHandle: puint64): boolean; cdecl; external WRAPPERLIB;
// Defers a request you have sent;the actual HTTP client code may have many requests queued;and this will move
// the specified request to the tail of the queue. Returns false on invalid handle;or if the request is not yet sent.
function SteamHTTP_DeferHTTPRequest(Request: uint32): boolean; cdecl; external WRAPPERLIB;
// Prioritizes a request you have sent;the actual HTTP client code may have many requests queued;and this will move
// the specified request to the head of the queue. Returns false on invalid handle;or if the request is not yet sent.
function SteamHTTP_PrioritizeHTTPRequest(Request: uint32): boolean; cdecl; external WRAPPERLIB;
// Checks if a response header is present in a HTTP response given a handle from HTTPRequestCompleted callback;also
// returns the size of the header value if present so the caller and allocate a correctly sized pBuffer for
// GetHTTPResponseHeaderValue.
function SteamHTTP_GetHTTPResponseHeaderSize(Request: uint32; HeaderName: pAnsiChar; unResponseHeaderSize: pUint32): boolean; cdecl; external WRAPPERLIB;
// Gets header values from a HTTP response given a handle from HTTPRequestCompleted callback;will return false if the
// header is not present or if your pBuffer is too small to contain it's value. You should first call
// BGetHTTPResponseHeaderSize to check for the presence of the header and to find out the size pBuffer needed.
function SteamHTTP_GetHTTPResponseHeaderValue(Request: uint32; HeaderName: pAnsiChar; HeaderValue: pUint8; pBufferSize: uint32): boolean; cdecl; external WRAPPERLIB;
// Gets the size of the body data from a HTTP response given a handle from HTTPRequestCompleted callback;will return false if the
// handle is invalid.
function SteamHTTP_GetHTTPResponseBodySize(Request: uint32; BodySize: pUint32): boolean; cdecl; external WRAPPERLIB;
// Gets the body data from a HTTP response given a handle from HTTPRequestCompleted callback;will return false if the
// handle is invalid or is to a streaming response;or if the provided pBuffer is not the correct size. Use BGetHTTPResponseBodySize first to find out
// the correct pBuffer size to use.
function SteamHTTP_GetHTTPResponseBodyData(Request: uint32; DatapBuffer: pUint8; pBufferSize: uint32): boolean; cdecl; external WRAPPERLIB;
// Gets the body data from a streaming HTTP response given a handle from HTTPRequestDataReceived callback. Will return false if the
// handle is invalid or is to a non-streaming response (meaning it wasn't sent with SendHTTPRequestAndStreamResponse);or if the pBuffer size and offset
// do not match the size and offset sent in HTTPRequestDataReceived_t.
function SteamHTTP_GetHTTPStreamingResponseBodyData(Request: uint32; cOffset: uint32; DataBuffer: pUint8; DataBufferSize: uint32): boolean; cdecl; external WRAPPERLIB;
// Releases an HTTP response handle;should always be called to free resources after receiving a HTTPRequestCompleted callback
// callback and finishing using the response.
function SteamHTTP_ReleaseHTTPRequest(Request: uint32): boolean; cdecl; external WRAPPERLIB;
// Gets progress on downloading the body for the request. This will be zero unless a response header has already been
// received which included a content-length field. For responses that contain no content-length it will report
// zero for the duration of the request as the size is unknown until the connection closes.
function SteamHTTP_GetHTTPDownloadProgressPct(Request: uint32; PercentOut: pSingle): boolean; cdecl; external WRAPPERLIB;
// Sets the body for an HTTP Post request. Will fail and return false on a GET request;and will fail if POST params
// have already been set for the request. Setting this raw body makes it the only contents for the post;the pchContentType
// parameter will set the content-type header for the request so the server may know how to interpret the body.
function SteamHTTP_SetHTTPRequestRawPostBody(Request: uint32; ContentType: pAnsiChar; Body: pUint8; BodySize: uint32): boolean; cdecl; external WRAPPERLIB;
{$ENDREGION}
{$REGION 'SteamServer'}
function SteamServer_Init(IP: uint32; GamePort, QueryPort: uint16; Flags: uint32; ServerMode: TServerMode; VersionString: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
/// Game product identifier. This is currently used by the master server for version checking purposes.
/// It's a required field;but will eventually will go away,and the AppID will be used for this purpose.
procedure SteamServer_SetProduct(Product: pAnsiChar); cdecl; external WRAPPERLIB;
/// Description of the game. This is a required field and is displayed in the steam server browser...for now.
/// This is a required field,but it will go away eventually,as the data should be determined from the AppID.
procedure SteamServer_SetGameDescription(GameDescription: pAnsiChar); cdecl; external WRAPPERLIB;
/// If your game is a "mod; " pass the string that identifies it. The default is an empty string;meaning
/// this application is the original game,not a mod.
procedure SteamServer_SetModDir(ModDir: pAnsiChar); cdecl; external WRAPPERLIB;
/// Is this is a dedicated server? The default value is false.
procedure SetDedicatedServer(IsDedicated: boolean); cdecl; external WRAPPERLIB;
/// Begin process to login to a persistent game server account
procedure SteamServer_LogOn(Token: pAnsiChar); cdecl; external WRAPPERLIB;
/// Login to a generic;anonymous account.
procedure SteamServer_LogOnAnonymous(); cdecl; external WRAPPERLIB;
// Begin process of logging game server out of steam
procedure SteamServer_LogOff(); cdecl; external WRAPPERLIB;
// status functions
function SteamServer_IsLoggedOn(): boolean; cdecl; external WRAPPERLIB;
function SteamServer_IsSecure(): boolean; cdecl; external WRAPPERLIB;
function SteamServer_GetSteamID(): TSteamID; cdecl; external WRAPPERLIB;
/// Returns true if the master server has requested a restart.
/// Only returns true once per request.
function SteamServer_WasRestartRequested(): boolean; cdecl; external WRAPPERLIB;
/// Max player count that will be reported to server browser and client queries
procedure SteamServer_SetMaxPlayerCount(PlayersMaxCount: integer); cdecl; external WRAPPERLIB;
/// Number of bots. Default value is zero
procedure SteamServer_SetBotPlayerCount(BotplayersCount: integer); cdecl; external WRAPPERLIB;
/// Set the name of server as it will appear in the server browser
procedure SteamServer_SetServerName(ServerName: pAnsiChar); cdecl; external WRAPPERLIB;
/// Set name of map to report in the server browser
procedure SteamServer_SetMapName(MapName: pAnsiChar); cdecl; external WRAPPERLIB;
/// Let people know if your server will require a password
procedure SteamServer_SetPasswordProtected(IsPasswordProtected: boolean); cdecl; external WRAPPERLIB;
/// Spectator server. The default value is zero;meaning the service
procedure SteamServer_SetSpectatorPort(SpectatorPort: uint16); cdecl; external WRAPPERLIB;
/// Name of the spectator server. (Only used if spectator port is nonzero.)
procedure SteamServer_SetSpectatorServerName(SpectatorServerName: pAnsiChar); cdecl; external WRAPPERLIB;
/// Call this to clear the whole list of key/values that are sent in rules queries.
procedure SteamServer_ClearAllKeyValues(); cdecl; external WRAPPERLIB;
/// Call this to add/update a key/value pair.
procedure SteamServer_SetKeyValue(Key, Value: pAnsiChar); cdecl; external WRAPPERLIB;
/// Sets a string defining the "gametags" for this server;this is optional;but if it is set
/// it allows users to filter in the matchmaking/server-browser interfaces based on the value
procedure SteamServer_SetGameTags(GameTags: pAnsiChar); cdecl; external WRAPPERLIB;
/// Sets a string defining the "gamedata" for this server;this is optional;but if it is set
/// it allows users to filter in the matchmaking/server-browser interfaces based on the value
/// don't set this unless it actually changes,its only uploaded to the master once (when
/// acknowledged)
procedure SteamServer_SetGameData(GameData: pAnsiChar); cdecl; external WRAPPERLIB;
/// Region identifier. This is an optional field;the default value is empty;meaning the "world" region
procedure SteamServer_SetRegion(Region: pAnsiChar); cdecl; external WRAPPERLIB;
// Handles receiving a new connection from a Steam user. This call will ask the Steam
// servers to validate the users identity,app ownership;and VAC status. If the Steam servers
// are off-line;then it will validate the cached ticket itself which will validate app ownership
// and identity. The AuthBlob here should be acquired on the game client using SteamUser()->InitiateGameConnection()
// and must then be sent up to the game server for authentication.
//
// Return Value: returns true if the users ticket passes basic checks. pSteamIDUser will contain the Steam ID of this user. pSteamIDUser must NOT be NULL
// If the call succeeds then you should expect a GSClientApprove_t or GSClientDeny callback which will tell you whether authentication
// for the user has succeeded or failed (the steamid in the callback will match the one returned by this call)
function SteamServer_SendUserConnectAndAuthenticate(ClientIP: uint32; AuthBlob: pointer; AuthBlobSize: uint32; UserSteamID: pSteamID): boolean; cdecl; external WRAPPERLIB;
// Creates a fake user (ie;a bot) which will be listed as playing on the server;but skips validation.
function SteamServer_CreateUnauthenticatedUserConnection(): TSteamID; cdecl; external WRAPPERLIB;
// Should be called whenever a user leaves our game server,this lets Steam internally
// track which users are currently on which servers for the purposes of preventing a single
// account being logged into multiple servers;showing who is currently on a server;etc.
procedure SteamServer_SendUserDisconnect(steamIDUser: TSteamID); cdecl; external WRAPPERLIB;
// Update the data to be displayed in the server browser and matchmaking interfaces for a user
// currently connected to the server. For regular users you must call this after you receive a
// GSUserValidationSuccess callback.
function SteamServer_UpdateUserData(UserID: TSteamID; PlayerName: pAnsiChar; Score: uint32): boolean; cdecl; external WRAPPERLIB;
// Retrieve ticket to be sent to the entity who wishes to authenticate you (using BeginAuthSession API).
// pcbTicket retrieves the length of the actual ticket.
function SteamServer_GetAuthSessionTicket(Ticket: pointer; MaxTicket: integer; ActualTicketLength: pUint32): uint32; cdecl; external WRAPPERLIB;
// Authenticate ticket (from SteamServer_GetAuthSessionTicket) from entity steamID to be sure it is valid and isnt reused
// Registers for callbacks if the entity goes offline or cancels the ticket (see ValidateAuthTicketResponse callback and EAuthSessionResponse)
function SteamServer_BeginAuthSession(AuthTicket: pointer; AuthTicketSize: integer; steamID: TSteamID): integer; cdecl; external WRAPPERLIB;
// Stop tracking started by BeginAuthSession - called when no longer playing game with this entity
procedure SteamServer_EndAuthSession(steamID: TSteamID); cdecl; external WRAPPERLIB;
// Cancel auth ticket from GetAuthSessionTicket,called when no longer playing game with the entity you gave the ticket to
procedure SteamServer_CancelAuthTicket(AuthTicket: uint32); cdecl; external WRAPPERLIB;
// After receiving a user's authentication data,and passing it to SendUserConnectAndAuthenticate;use this function
// to determine if the user owns downloadable content specified by the provided AppID.
function SteamServer_UserHasLicenseForApp(steamID: TSteamID; appID: uint32): integer; cdecl; external WRAPPERLIB;
// Ask if a user in in the specified group,results returns async by callback
// returns false if we're not connected to the steam servers and thus cannot ask
function SteamServer_RequestUserGroupStatus(UserID, GroupID: TSteamID): boolean; cdecl; external WRAPPERLIB;
// Returns the public IP of the server according to Steam,useful when the server is
// behind NAT and you want to advertise its IP in a lobby for other clients to directly connect to
function SteamServer_GetPublicIP(): uint32; cdecl; external WRAPPERLIB;
// These are in GameSocketShare mode, where instead of SteamGameServer creating its own
// socket to talk to the master server on;it lets the game use its socket to forward messages
// back and forth. This prevents us from requiring server ops to open up yet another port
// in their firewalls.
//
// the IP address and port should be in host order;i.e 127.0.0.1 == 0x7f000001

// These are used when you've elected to multiplex the game server's UDP socket
// rather than having the master server updater use its own sockets.
//
// Source games use this to simplify the job of the server admins;so they
// don't have to open up more ports on their firewalls.

// Call this when a packet that starts with 0xFFFFFFFF comes in. That means
// it's for us.
function SteamServer_HandleIncomingPacket(Data: pointer; DataSize: integer; SourceIP: uint32; SourcePort: uint16): boolean; cdecl; external WRAPPERLIB;
// AFTER calling HandleIncomingPacket for any packets that came in that frame;call this.
// This gets a packet that the master server updater needs to send out on UDP.
// It returns the length of the packet it wants to send;or 0 if there are no more packets to send.
// Call this each frame until it returns 0.
function SteamServer_GetNextOutgoingPacket(pOut: pointer; MaxOutCount: integer; NetAddress: pUint32; Port: pUint16): integer; cdecl; external WRAPPERLIB;
// Call this as often as you like to tell the master server updater whether or not
// you want it to be active (default: off).
procedure SteamServer_EnableHeartbeats(IsActive: boolean); cdecl; external WRAPPERLIB;
// You usually don't need to modify this.
// Pass -1 to use the default value for iHeartbeatInterval.
// Some mods change this.
procedure SteamServer_SetHeartbeatInterval(HeartbeatInterval: integer); cdecl; external WRAPPERLIB;
// Force a heartbeat to steam at the next opportunity
procedure SteamServer_ForceHeartbeat(); cdecl; external WRAPPERLIB;
// associate this game server with this clan for the purposes of computing player compat
function SteamServer_AssociateWithClan(ClanID: TSteamID): uint64; cdecl; external WRAPPERLIB;
// ask if any of the current players dont want to play with this new player - or vice versa
function SteamServer_ComputeNewPlayerCompatibility(NewPlayerID: TSteamID): uint64; cdecl; external WRAPPERLIB;
{$ENDREGION}
{$REGION 'SteamFriends'}
// returns the local players name - guaranteed to not be NULL.
// this is the same name as on the users community profile page
// this is stored in UTF-8 format
// like all the other interface functions that return a: pAnsiChar, it's important that this is not saved
// off:pointer, it will eventually be free'd or re-allocated
function SteamFriends_GetPersonaName(): pAnsiChar; cdecl; external WRAPPERLIB;
// Sets the player name;stores it on the server and publishes the changes to all friends who are online.
// Changes take place locally immediately, and a PersonaStateChange is posted, presuming success.
//
// The final results are available through the return value TSteamAPICall, using SetPersonaNameResponse.
//
// If the name change fails to happen on the server;then an additional global PersonaStateChange_t will be posted
// to change the name back;in addition to the SetPersonaNameResponse_t callback.
function SteamFriends_SetPersonaName(Name: pAnsiChar): uint64; cdecl; external WRAPPERLIB;
// gets the status of the current user
function SteamFriends_GetPersonaState(): integer; cdecl; external WRAPPERLIB;
// friend iteration
// takes a set of k_EFriendFlags, and returns the number of users the client knows about who meet that criteria
// then GetFriendByIndex() can then be used to return the id's of each of those users
function SteamFriends_GetFriendCount(FriendFlags: integer): integer; cdecl; external WRAPPERLIB;
// returns the steamID of a user
// iFriend is a index of range [0, GetFriendCount())
// iFriendsFlags must be the same value as used in GetFriendCount()
// the returned TSteamID can then be used by all the functions below to access details about the user
function SteamFriends_GetFriendByIndex(FriendIndex, FriendFlags: integer): TSteamID; cdecl; external WRAPPERLIB;
// returns a relationship to a user
function SteamFriends_GetFriendRelationship(FriendID: TSteamID): integer; cdecl; external WRAPPERLIB;
// returns the current status of the specified user
// this will only be known by the local user if steamIDFriend is in their friends list, on the same game server, in a chat room or lobby, or in a small group with the local user
function SteamFriends_GetFriendPersonaState(FriendID: TSteamID): integer; cdecl; external WRAPPERLIB;
// returns the name another user - guaranteed to not be NULL.
// same rules as GetFriendPersonaState() apply as to whether or not the user knowns the name of the other user
// note that on first joining a lobby, chat room or game server the local user will not known the name of the other users automatically; that information will arrive asyncronously
function SteamFriends_GetFriendPersonaName(FriendID: TSteamID): pAnsiChar; cdecl; external WRAPPERLIB;
// returns true if the friend is actually in a game;and fills in pFriendGameInfo with an extra details
function SteamFriends_GetFriendGamePlayed(FriendID: TSteamID; pFriendGameInfo: pFriendGameInfo): boolean; cdecl; external WRAPPERLIB;
// accesses old friends names - returns an empty string when their are no more items in the history
function SteamFriends_GetFriendPersonaNameHistory(FriendID: TSteamID; PersonaNameNum: integer): pAnsiChar; cdecl; external WRAPPERLIB;
// Returns nickname the current user has set for the specified player. Returns NULL if the no nickname has been set for that player.
function SteamFriends_GetPlayerNickname(PlayerID: TSteamID): pAnsiChar; cdecl; external WRAPPERLIB;
// returns true if the specified user meets any of the criteria specified in iFriendFlags
// iFriendFlags can be the union (binary or;|) of one or more k_EFriendFlags values
function SteamFriends_HasFriend(FriendID: TSteamID; FriendFlags: integer): boolean; cdecl; external WRAPPERLIB;
// clan (group) iteration and access functions
function SteamFriends_GetClanCount(): integer; cdecl; external WRAPPERLIB;
function SteamFriends_GetClanByIndex(Clan: integer): TSteamID; cdecl; external WRAPPERLIB;
function SteamFriends_GetClanName(ClanID: TSteamID): pAnsiChar; cdecl; external WRAPPERLIB;
function SteamFriends_GetClanTag(ClanID: TSteamID): pAnsiChar; cdecl; external WRAPPERLIB;
// returns the most recent information we have about what's happening in a clan
function SteamFriends_GetClanActivityCounts(ClanID: TSteamID; OnlineCount: pInteger; InGameCount: pInteger; ChattingCount: pInteger): boolean; cdecl; external WRAPPERLIB;
// for clans a user is a member of;they will have reasonably up-to-date information;but for others you'll have to download the info to have the latest
function SteamFriends_DownloadClanActivityCounts(ClansIDs: pSteamID; ClansToRequestCount: integer): uint64; cdecl; external WRAPPERLIB;
// iterators for getting users in a chat room;lobby;game server or clan
// note that large clans that cannot be iterated by the local user
// note that the current user must be in a lobby to retrieve TSteamIDs of other users in that lobby
// steamIDSource can be the steamID of a group;game server;lobby or chat room
function SteamFriends_GetFriendCountFromSource(SourceID: TSteamID): integer; cdecl; external WRAPPERLIB;
function SteamFriends_GetFriendFromSourceByIndex(SourceID: TSteamID; FriendIndex: integer): TSteamID; cdecl; external WRAPPERLIB;
// returns true if the local user can see that steamIDUser is a member or in steamIDSource
function SteamFriends_IsUserInSource(UserID: TSteamID; SourceID: TSteamID): boolean; cdecl; external WRAPPERLIB;
// User is in a game pressing the talk button (will suppress the microphone for all voice comms from the Steam friends UI)
procedure SteamFriends_SetInGameVoiceSpeaking(UserID: TSteamID; IsSpeaking: boolean); cdecl; external WRAPPERLIB;
// activates the game overlay;with an optional dialog to open
// valid options are "Friends","Community","Players","Settings","OfficialGameGroup","Stats","Achievements"
procedure SteamFriends_ActivateGameOverlay(Dialog: pAnsiChar); cdecl; external WRAPPERLIB;
// activates game overlay to a specific place
// valid options are
// "steamid" - opens the overlay web browser to the specified user or groups profile
// "chat" - opens a chat window to the specified user;or joins the group chat
// "jointrade" - opens a window to a Steam Trading session that was started with the ISteamEconomy/StartTrade Web API
// "stats" - opens the overlay web browser to the specified user's stats
// "achievements" - opens the overlay web browser to the specified user's achievements
// "friendadd" - opens the overlay in minimal mode prompting the user to add the target user as a friend
// "friendremove" - opens the overlay in minimal mode prompting the user to remove the target friend
// "friendrequestaccept" - opens the overlay in minimal mode prompting the user to accept an incoming friend invite
// "friendrequestignore" - opens the overlay in minimal mode prompting the user to ignore an incoming friend invite
procedure SteamFriends_ActivateGameOverlayToUser(Dialog: pAnsiChar; steamID: TSteamID); cdecl; external WRAPPERLIB;
// activates game overlay web browser directly to the specified URL
// full address with protocol type is required,e.g. http://www.steamgames.com/
procedure SteamFriends_ActivateGameOverlayToWebPage(URL: pAnsiChar); cdecl; external WRAPPERLIB;
// activates game overlay to store page for app
procedure SteamFriends_ActivateGameOverlayToStore(appID: uint32; Flag: integer); cdecl; external WRAPPERLIB;
// Mark a target user as 'played with'. This is a client-side only feature that requires that the calling user is
// in game
procedure SteamFriends_SetPlayedWith(UserPlayedWithID: TSteamID); cdecl; external WRAPPERLIB;
// activates game overlay to open the invite dialog. Invitations will be sent for the provided lobby.
procedure SteamFriends_ActivateGameOverlayInviteDialog(LobbyID: TSteamID); cdecl; external WRAPPERLIB;
// gets the small (32x32) avatar of the current user;which is a handle to be used in IClientUtils:GetImageRGBA();or 0 if none set
function SteamFriends_GetSmallFriendAvatar(FriendID: TSteamID): integer; cdecl; external WRAPPERLIB;
// gets the medium (64x64) avatar of the current user;which is a handle to be used in IClientUtils:GetImageRGBA();or 0 if none set
function SteamFriends_GetMediumFriendAvatar(FriendID: TSteamID): integer; cdecl; external WRAPPERLIB;
// gets the large (184x184) avatar of the current user;which is a handle to be used in IClientUtils:GetImageRGBA();or 0 if none set
// returns -1 if this image has yet to be loaded;in this case wait for a AvatarImageLoaded_t callback and then call this again
function SteamFriends_GetLargeFriendAvatar(FriendID: TSteamID): integer; cdecl; external WRAPPERLIB;
// requests information about a user - persona name & avatar
// if bRequireNameOnly is set;then the avatar of a user isn't downloaded
// - it's a lot slower to download avatars and churns the local cache;so if you don't need avatars;don't request them
// if returns true;it means that data is being requested;and a PersonaStateChanged_t callback will be posted when it's retrieved
// if returns false;it means that we already have all the details about that user;and functions can be called immediately
function SteamFriends_RequestUserInformation(UserID: TSteamID; IsRequireNameOnly: boolean): boolean; cdecl; external WRAPPERLIB;
// requests information about a clan officer list
// when complete;data is returned in ClanOfficerListResponse_t call result
// this makes available the calls below
// you can only ask about clans that a user is a member of
// note that this won't download avatars automatically; if you get an officer;
// and no avatar image is available;call RequestUserInformation(steamID;false) to download the avatar
function SteamFriends_RequestClanOfficerList(ClanID: TSteamID): uint64; cdecl; external WRAPPERLIB;
// iteration of clan officers - can only be done when a SteamFriends_RequestClanOfficerList() call has completed
// returns the steamID of the clan owner
function SteamFriends_GetClanOwner(ClanID: TSteamID): TSteamID; cdecl; external WRAPPERLIB;
// returns the number of officers in a clan (including the owner)
function SteamFriends_GetClanOfficerCount(ClanID: TSteamID): integer; cdecl; external WRAPPERLIB;
// returns the steamID of a clan officer;by index;of range [0; GetClanOfficerCount)
function SteamFriends_GetClanOfficerByIndex(ClanID: TSteamID; OfficerIndex: integer): TSteamID; cdecl; external WRAPPERLIB;
// if current user is chat restricted;he can't send or receive any text/voice chat messages.
// the user can't see custom avatars. But the user can be online and send/recv game invites.
// a chat restricted user can't add friends or join any groups.
function SteamFriends_GetUserRestrictions(): uint32; cdecl; external WRAPPERLIB;
// Rich Presence data is automatically shared between friends who are in the same game
// Each user has a set of Key/Value pairs
// Up to 20 different keys can be set
// There are two magic keys:
// "status" - a UTF-8 string that will show up in the 'view game info' dialog in the Steam friends list
// "connect" - a UTF-8 string that contains the command-line for how a friend can connect to a game
// GetFriendRichPresence() returns an empty string "" if no value is set
// SetRichPresence() to a NULL or an empty string deletes the key
// You can iterate the current set of keys for a friend with GetFriendRichPresenceKeyCount()
// and GetFriendRichPresenceKeyByIndex() (typically only used for debugging)
function SteamFriends_SetRichPresence(Key: pAnsiChar; Value: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
procedure SteamFriends_ClearRichPresence(); cdecl; external WRAPPERLIB;
function SteamFriends_GetFriendRichPresence(FriendID: TSteamID; Key: pAnsiChar): pAnsiChar; cdecl; external WRAPPERLIB;
function SteamFriends_GetFriendRichPresenceKeyCount(FriendID: TSteamID): integer; cdecl; external WRAPPERLIB;
function SteamFriends_GetFriendRichPresenceKeyByIndex(FriendID: TSteamID; KeyIndex: integer): pAnsiChar; cdecl; external WRAPPERLIB;
// Requests rich presence for a specific user.
procedure SteamFriends_RequestFriendRichPresence(FriendID: TSteamID); cdecl; external WRAPPERLIB;
// rich invite support
// if the target accepts the invite;the pchConnectString gets added to the command-line for launching the game
// if the game is already running;a GameRichPresenceJoinRequested_t callback is posted containing the connect string
// invites can only be sent to friends
function SteamFriends_InviteUserToGame(FriendID: TSteamID; ConnectString: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
// recently-played-with friends iteration
// this iterates the entire list of users recently played with;across games
// GetFriendCoplayTime() returns as a unix time
function SteamFriends_GetCoplayFriendCount(): integer; cdecl; external WRAPPERLIB;
function SteamFriends_GetCoplayFriend(CoplayFriend: integer): TSteamID; cdecl; external WRAPPERLIB;
function SteamFriends_GetFriendCoplayTime(FriendID: TSteamID): integer; cdecl; external WRAPPERLIB;
function SteamFriends_GetFriendCoplayGame(FriendID: TSteamID): uint32; cdecl; external WRAPPERLIB;
// chat interface for games
// this allows in-game access to group (clan) chats from in the game
// the behavior is somewhat sophisticated;because the user may or may not be already in the group chat from outside the game or in the overlay
// use ActivateGameOverlayToUser("chat";steamIDClan) to open the in-game overlay version of the chat
function SteamFriends_JoinClanChatRoom(ClanID: TSteamID): uint64; cdecl; external WRAPPERLIB;
function SteamFriends_LeaveClanChatRoom(ClanID: TSteamID): boolean; cdecl; external WRAPPERLIB;
function SteamFriends_GetClanChatMemberCount(ClanID: TSteamID): integer; cdecl; external WRAPPERLIB;
function SteamFriends_GetChatMemberByIndex(ClanID: TSteamID; UserIndex: integer): TSteamID; cdecl; external WRAPPERLIB;
function SteamFriends_SendClanChatMessage(ClanChatID: TSteamID; Text: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
function SteamFriends_GetClanChatMessage(ClanChatID: TSteamID; MessageIndex: integer; pText: pointer; TextMax: integer; ChatEntryType: pInteger; TeamIDChatter: pSteamID): integer; cdecl; external WRAPPERLIB;
function SteamFriends_IsClanChatAdmin(ClanChatID: TSteamID; UserID: TSteamID): boolean; cdecl; external WRAPPERLIB;
// interact with the Steam (game overlay / desktop)
function SteamFriends_IsClanChatWindowOpenInSteam(ClanChatID: TSteamID): boolean; cdecl; external WRAPPERLIB;
function SteamFriends_OpenClanChatWindowInSteam(ClanChatID: TSteamID): boolean; cdecl; external WRAPPERLIB;
function SteamFriends_CloseClanChatWindowInSteam(ClanChatID: TSteamID): boolean; cdecl; external WRAPPERLIB;
// peer-to-peer chat interception
// this is so you can show P2P chats inline in the game
function SteamFriends_SetListenForFriendsMessages(IsInterceptEnabled: boolean): boolean; cdecl; external WRAPPERLIB;
function SteamFriends_ReplyToFriendMessage(FriendID: TSteamID; MsgToSend: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
function SteamFriends_GetFriendMessage(FriendID: TSteamID; MessageIndex: integer; Data: pointer; DataSize: integer; ChatEntryType: pInteger): integer; cdecl; external WRAPPERLIB;
// following apis
function SteamFriends_GetFollowerCount(steamID: TSteamID): uint64; cdecl; external WRAPPERLIB;
function SteamFriends_IsFollowing(steamID: TSteamID): uint64; cdecl; external WRAPPERLIB;
function SteamFriends_EnumerateFollowingList(StartIndex: uint32): uint64; cdecl; external WRAPPERLIB;
{$ENDREGION}
{$REGION 'SteamController'}
// Must call init and shutdown when starting/ending use
function SteamController_Init(AbsolutePathToControllerConfigVDF: pAnsiChar): boolean; cdecl; external WRAPPERLIB;
function SteamController_Shutdown(): boolean; cdecl; external WRAPPERLIB;
// Get the state of the specified controller;returns false if that controller is not connected
function SteamController_GetControllerState(ControllerIndex: uint32; pState: pSteamControllerState): boolean; cdecl; external WRAPPERLIB;
// Trigger a haptic pulse on the controller
procedure SteamController_TriggerHapticPulse(ControllerIndex: uint32; TargetPad: integer; DurationMicroSec: shortint); cdecl; external WRAPPERLIB;
// Set the override mode which is used to choose to use different base/legacy bindings from your config file
procedure SteamController_SetOverrideMode(Mode: pAnsiChar); cdecl; external WRAPPERLIB;
{$ENDREGION}
{$REGION 'SteamApps'}
function SteamApps_IsSubscribed(): boolean; cdecl; external WRAPPERLIB;
function SteamApps_IsLowViolence(): boolean; cdecl; external WRAPPERLIB;
function SteamApps_IsCybercafe(): boolean; cdecl; external WRAPPERLIB;
function SteamApps_IsVACBanned(): boolean; cdecl; external WRAPPERLIB;
function SteamApps_GetCurrentGameLanguage(): pAnsiChar; cdecl; external WRAPPERLIB;
function SteamApps_GetAvailableGameLanguages(): pAnsiChar; cdecl; external WRAPPERLIB;
// only use this member if you need to check ownership of another game related to yours;a demo for example
function SteamApps_IsSubscribedApp(appID: uint32): boolean; cdecl; external WRAPPERLIB;
// Takes AppID of DLC and checks if the user owns the DLC & if the DLC is installed
function SteamApps_IsDlcInstalled(appID: uint32): boolean; cdecl; external WRAPPERLIB;
// returns the Unix time of the purchase of the app
function SteamApps_GetEarliestPurchaseUnixTime(appID: uint32): uint32; cdecl; external WRAPPERLIB;
// Checks if the user is subscribed to the current app through a free weekend
// This function will return false for users who have a retail or other type of license cdecl; external WRAPPERLIB;
// Before using;please ask your Valve technical contact how to package and secure your free weekened
function SteamApps_IsSubscribedFromFreeWeekend(): boolean; cdecl; external WRAPPERLIB;
// Returns the number of DLC pieces for the running app
function SteamApps_GetDLCCount(): integer; cdecl; external WRAPPERLIB;
// Returns metadata for DLC by index;of range [0;GetDLCCount()]
function SteamApps_GetDLCDataByIndex(iDLC: integer; appID: pUint32; IsAvailable: pBoolean; Name: pAnsiChar; NamepBufferSize: integer): boolean; cdecl; external WRAPPERLIB;
// Install/Uninstall control for optional DLC
procedure SteamApps_InstallDLC(appID: uint32); cdecl; external WRAPPERLIB;
procedure SteamApps_UninstallDLC(appID: uint32); cdecl; external WRAPPERLIB;
// Request cd-key for yourself or owned DLC. If you are interested in this
// data then make sure you provide us with a list of valid keys to be distributed
// to users when they purchase the game;before the game ships.
// You'll receive an AppProofOfPurchaseKeyResponse_t callback when
// the key is available (which may be immediately).
procedure SteamApps_RequestAppProofOfPurchaseKey(appID: uint32); cdecl; external WRAPPERLIB;
function SteamApps_GetCurrentBetaName(Name: pAnsiChar; NamepBufferSize: integer): boolean; cdecl; external WRAPPERLIB;
function SteamApps_MarkContentCorrupt(IsMissingFilesOnly: boolean): boolean; cdecl; external WRAPPERLIB;
function SteamApps_GetInstalledDepots(appID: uint32; Depots: pUint32; MaxDepots: uint32): uint32; cdecl; external WRAPPERLIB;
// returns current app install folder for AppID;returns folder name length
function SteamApps_GetAppInstallDir(appID: uint32; Folder: pAnsiChar; FolderpBufferSize: uint32): uint32; cdecl; external WRAPPERLIB;
// returns true if that app is installed (not necessarily owned)
function SteamApps_IsAppInstalled(appID: uint32): boolean; cdecl; external WRAPPERLIB;
// returns the SteamID of the original owner. If different from current user;it's borrowed
function SteamApps_GetAppOwner(): TSteamID; cdecl; external WRAPPERLIB;
// Returns the associated launch param if the game is run via steam://run/<appid>//?param1=value1;param2=value2;param3=value3 etc.
// Parameter names starting with the character '@' are reserved for internal use and will always return and empty string.
// Parameter names starting with an underscore '_' are reserved for steam features -- they can be queried by the game;
// but it is advised that you not param names beginning with an underscore for your own features.
function SteamApps_GetLaunchQueryParam(Key: pAnsiChar): pAnsiChar; cdecl; external WRAPPERLIB;
{$ENDREGION}
{$REGION 'SteamAppList'}
function SteamAppList_GetNumInstalledApps(): uint32; cdecl; external WRAPPERLIB;
function SteamAppList_GetInstalledApps(appID: pUint32; unMaxAppIDs: uint32): uint32; cdecl; external WRAPPERLIB;
// returns -1 if no name was found
function SteamAppList_GetAppName(nAppID: uint32; Name: pAnsiChar; NameMax: integer): integer; cdecl; external WRAPPERLIB;
// returns -1 if no dir was found
function SteamAppList_GetAppInstallDir(nAppID: uint32; Directory: pAnsiChar; NameMax: integer): integer; cdecl; external WRAPPERLIB;
// return the buildid of this app;may change at any time based on backend updates to the game
function SteamAppList_GetAppBuildId(nAppID: uint32): integer; cdecl; external WRAPPERLIB;
{$ENDREGION}

implementation

end.
