unit GameLibIntegration;

interface

{$IFDEF GOG}
uses GameLibIntegration.Gog;
{$ENDIF}
{$IFDEF STEAM}
uses GameLibIntegration.Steam;
{$ENDIF}

type
  TGameLibIntegration = class
    public
      constructor Create;
      destructor Destroy; override;
      procedure SetAchievement(ach_name : pAnsiChar);
      procedure ClearAchievement(ach_name : pAnsiChar);
    end;

var
  FGameLibIntegration : TGameLibIntegration;

implementation

destructor TGameLibIntegration.Destroy;
begin
{$IFDEF GOG}
  FGogIntegration.Free;
{$ELSEIF STEAM}
  FSteamIntegration.Free;
{$ENDIF}
  inherited;
end;

constructor TGameLibIntegration.Create;
begin
{$IFDEF GOG}
  FGogIntegration := TGogIntegration.Create;
{$ENDIF}
{$IFDEF STEAM}
  FSteamIntegration := TSteamIntegration.Create;
{$ENDIF}
end;

procedure TGameLibIntegration.SetAchievement(ach_name : pAnsiChar);
begin
{$IFDEF GOG}
  FGogIntegration.SetAchievement(ach_name);
{$ENDIF}
{$IFDEF STEAM}
  FSteamIntegration.SetAchievement(ach_name);
{$ENDIF}

end;

procedure TGameLibIntegration.ClearAchievement(ach_name : pAnsiChar);
begin
{$IFDEF GOG}
  FGogIntegration.ClearAchievement(ach_name);
{$ENDIF}
{$IFDEF STEAM}
  FSteamIntegration.ClearAchievement(ach_name);
{$ENDIF}
end;

end.
