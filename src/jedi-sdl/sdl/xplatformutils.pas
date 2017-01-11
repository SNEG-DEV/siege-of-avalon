unit xplatformutils;

{$I jedi-sdl.inc}

interface

uses
  sysutils,
  {$IFDEF WIN32}
  Windows;
  {$ELSE}
  BaseUnix,
  unixtype;
  {$ENDIF}

const
{$IFDEF MACOS}
  DIR_SEP = ':';
  DIR_CUR = ':';
{$ELSE}
{$IFDEF DARWIN}
  DIR_SEP = ':';
  DIR_CUR = ':';
{$ELSE}
{$IFDEF WIN32}
  DIR_SEP = '\';
  DIR_CUR = '';
{$ELSE}
  DIR_SEP = '/';
  DIR_CUR = '';
{$ENDIF}
{$ENDIF}
{$ENDIF}

procedure ExecAndWait( aProcess : string; aArguments : array of string );


implementation

procedure ExecAndWait( aProcess : string; aArguments : array of string );
begin
	// Just simply call the sysutils call.
	ExecuteProcess(aProcess, aArguments);
end;

end.
 
