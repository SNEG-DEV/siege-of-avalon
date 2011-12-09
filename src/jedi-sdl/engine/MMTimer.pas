unit MMTimer;
{******************************************************************************}
{                                                                              }
{               Siege Of Avalon : Open Source Edition                          }
{               -------------------------------------                          }
{                                                                              }
{ Portions created by Digital Tome L.P. Texas USA are                          }
{ Copyright ©1999-2000 Digital Tome L.P. Texas USA                             }
{ All Rights Reserved.                                                         }
{                                                                              }
{ Portions created by Team SOAOS are                                           }
{ Copyright (C) 2003 - Team SOAOS.                                             }
{                                                                              }
{                                                                              }
{ Contributor(s)                                                               }
{ --------------                                                               }
{ Dominique Louis <Dominique@SavageSoftware.com.au>                            }
{                                                                              }
{                                                                              }
{                                                                              }
{ You may retrieve the latest version of this file at the SOAOS project page : }
{   http://www.sourceforge.com/projects/soaos                                  }
{                                                                              }
{ The contents of this file maybe used with permission, subject to             }
{ the GNU Lesser General Public License Version 2.1 (the "License"); you may   }
{ not use this file except in compliance with the License. You may             }
{ obtain a copy of the License at                                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Software distributed under the License is distributed on an                  }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or               }
{ implied. See the License for the specific language governing                 }
{ rights and limitations under the License.                                    }
{                                                                              }
{ Description                                                                  }
{ -----------                                                                  }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Requires                                                                     }
{ --------                                                                     }
{   DirectX Runtime libraris on Win32                                          }
{   They are available from...                                                 }
{   http://www.microsoft.com.                                                  }
{                                                                              }
{ Programming Notes                                                            }
{ -----------------                                                            }
{                                                                              }
{                                                                              }
{                                                                              }
{                                                                              }
{ Revision History                                                             }
{ ----------------                                                             }
{   July    13 2003 - DL : Initial Upload to CVS                               }
{                                                                              }
{******************************************************************************}

interface

uses
  Windows,
  Classes,
  SysUtils;

type
  TAniTimer = class;

  TAniTimerThread = class( TThread )
  private
    procedure TimerEvent;
  public
    AniTimer : TAniTimer;
    procedure Execute; override;
  end;

  TAniTimer = class( TComponent )
  private
    AniTimerThread : TAniTimerThread;
    TimerOn : Boolean;
    TimerThreadPriority : TThreadPriority;
    TimerPaused : Boolean;
    TimerInterval : Cardinal;
    TimerResolution : Cardinal;
    OnTimerEvent : TNotifyEvent;
    OnTimerEventHandle : Integer;
    TimerName : Integer;
  protected
    procedure InitTimer;
    procedure UpdateTimerStatus( NewOn : Boolean );
    procedure UpdateTimerPriority( NewPriority : TThreadPriority );
    procedure UpdateTimerInterval( NewInterval : Cardinal );
  public
    constructor Create( AOwner : TComponent ); override;
    destructor Destroy; override;
    procedure Resume;
    procedure Pause;
  published
    property Enabled : Boolean read TimerOn write UpdateTimerStatus default False;
    property TimerPriority : TThreadPriority read TimerThreadPriority write UpdateTimerPriority default tpNormal;
    property Interval : Cardinal read TimerInterval write UpdateTimerInterval default 100;
    property Resolution : Cardinal read TimerResolution write TimerResolution default 10;
    property OnTimer : TNotifyEvent read OnTimerEvent write OnTimerEvent;
    property Paused : boolean read TimerPaused;
  end;

  TAniTimerCallBack = procedure( NA1, NA2, AniTimerUser, NA3, NA4 : Integer ) stdcall;
  EAniTimer = class( Exception );

function KillTimer( AniTimerName : Integer ) : Integer; stdcall;
external 'WinMM.dll' name 'timeKillEvent';
function SetTimer( TimerInterval, TimerResolution : Integer;
  AniTimerCallBack : TAniTimerCallBack;
  AniTimerUser, AniTimerFlags : Integer ) : Integer; stdcall;
external 'WinMM.dll' name 'timeSetEvent';

implementation

procedure AniTimerCallBack( NA1, NA2, AniTimerUser, NA3, NA4 : Integer ); stdcall;
const
  FailName : string = 'MMTimer.AniTimerCallBack';
var
  AniTimer : TAniTimer;
begin
  AniTimer := TAniTimer( AniTimerUser );
  if Assigned( AniTimer ) then


    if not AniTimer.TimerPaused then
      SetEvent( AniTimer.OnTimerEventHandle );
end;

//---------------------------------------------------------------
//TAniTimerThread

procedure TAniTimerThread.TimerEvent;
const
  FailName : string = 'TAniTimerThread.TimerEvent';
begin
  if Assigned( AniTimer.OnTimerEvent ) then
    AniTimer.OnTimerEvent( AniTimer );
end;

procedure TAniTimerThread.Execute;
const
  FailName : string = 'TAniTimerThread.Execute';
begin
  while not Terminated do
  begin
    WaitForSingleObject( AniTimer.OnTimerEventHandle, INFINITE );
    if Terminated then
      break;
    synchronize( TimerEvent );
  end;
end;

//---------------------------------------------------------------------
//TAniTimer

constructor TAniTimer.Create( AOwner : TComponent );
begin
  inherited Create( AOwner );
  TimerOn := False;
  TimerInterval := 100;
  TimerResolution := 10;
  TimerPaused := False;
  TimerThreadPriority := tpNormal;
end;

destructor TAniTimer.Destroy;
begin
  Enabled := False;
  inherited Destroy;
end;

procedure TAniTimer.InitTimer;
const
  FailName : string = 'TAniTimer.InitTimer';
begin
  TimerName := SetTimer( TimerInterval, TimerResolution, @AniTimerCallBack, Integer( Self ), 1 );
  if TimerName = 0 then
  begin
    TimerOn := False;
    AniTimerThread.Terminate;
    SetEvent( OnTimerEventHandle );
    AniTimerThread := nil;
    CloseHandle( OnTimerEventHandle );
    raise EAniTimer.Create( 'AniTimer creation error.' );
  end;
end;

procedure TAniTimer.UpdateTimerStatus( NewOn : Boolean );
const
  FailName : string = 'TAniTimer.UpdateTimerStatus';
begin
  if NewOn = TimerOn then
    Exit;
  if ( csDesigning in ComponentState ) then
  begin
    TimerOn := NewOn;
    Exit;
  end;
  if NewOn then
  begin
    OnTimerEventHandle := CreateEvent( nil, False, False, nil );
    AniTimerThread := TAniTimerThread.Create( True );
    AniTimerThread.AniTimer := Self;
    AniTimerThread.FreeOnTerminate := True;
    AniTimerThread.Priority := TimerThreadPriority;
    AniTimerThread.Resume;
    InitTimer;
  end
  else
  begin
    KillTimer( TimerName );
    AniTimerThread.Terminate;
    SetEvent( OnTimerEventHandle );
    AniTimerThread := nil;
    CloseHandle( OnTimerEventHandle );
  end;
  TimerOn := NewOn;
end;

procedure TAniTimer.UpdateTimerInterval( NewInterval : Cardinal );
const
  FailName : string = 'TAniTimer.UpdateTimerInterval';
begin
  if NewInterval = TimerInterval then
    Exit;
  TimerInterval := NewInterval;
  if ( csDesigning in ComponentState ) then
    Exit;
  if Enabled then
  begin
    KillTimer( TimerName );
    InitTimer;
  end;
end;

procedure TAniTimer.UpdateTimerPriority( NewPriority : TThreadPriority );
const
  FailName : string = 'TAniTimer.UpdateTimerPriority';
begin
  if NewPriority = TimerThreadPriority then
    Exit;
  if Assigned( AniTimerThread ) then
    AniTimerThread.Priority := NewPriority;
  TimerThreadPriority := NewPriority;
end;

procedure TAniTimer.Pause;
const
  FailName : string = 'TAniTimer.Pause';
begin
  if TimerOn then
    AniTimerThread.Suspend;
  TimerPaused := True;
end;

procedure TAniTimer.Resume;
const
  FailName : string = 'TAniTimer.Resume';
begin
  if TimerOn then
    AniTimerThread.Resume;
  TimerPaused := False;
end;

end.

