unit Achievements;

interface

uses
  System.Classes,
  System.Generics.Defaults,
  System.Generics.Collections,
  Character;

type
  TAchievements = class
  private
    FAchievements: TDictionary<string, string>;
  public
    constructor Create;
    destructor Destroy; override;
    function getIdByTitle( title: string; character: TCharacter; out ACH_ID: string ): Boolean;
  end;

implementation

uses
  System.SysUtils;

{ TAchievements }

constructor TAchievements.Create;
begin
  FAchievements := TDictionary<string, string>.Create;
  FAchievements.Add('01metavarous', 'ACH_REPORT_TO_AVAROUS');
  FAchievements.Add('01LetterReturned', 'ACH_SEALED_WITH_A_KISS');
  FAchievements.Add('Completed', 'ACH_FETCH_THE_HERBS');
  FAchievements.Add('01Bugs', 'ACH_WHATS_IN_THE_BASEMENT');
  FAchievements.Add('01FoundBrother', 'ACH_BROTHERS_IN_ARMS');
  FAchievements.Add('01ReturnedSteel', 'ACH_COLD_HARD_STEEL');
  FAchievements.Add('01PellHappy', 'ACH_PELLS_SORROW');
  FAchievements.Add('01Boots', 'ACH_CASE_THE_CHEST');
  FAchievements.Add('01happywizard', 'ACH_FETCH_THE_AMULET');
  FAchievements.Add('01recovered', 'ACH_FETCH_THE_CHALICE');
  FAchievements.Add('02estonequestcomp', 'ACH_RETRIEVE_THE_EARTHSTONE');
  FAchievements.Add('02traitorquestcomp', 'ACH_LETTER_FROM_A_TRAITOR');
  FAchievements.Add('02pqcompleted', 'ACH_WHO_POISONED_THE_KING');
  FAchievements.Add('02ringdeliv', 'ACH_RETURN_TRACYS_RING');
  FAchievements.Add('02weldonnopass', 'ACH_CHANGING_OF_THE_GUARD');
  FAchievements.Add('02signetsent', 'ACH_TRISTANS_FRIEND');
  FAchievements.Add('02killmycorpse', 'ACH_LYDIAS_FATE');
  FAchievements.Add('02courierquestcomp', 'ACH_COURIER_WHAT_COURIER');
  FAchievements.Add('05savedLurkers', 'ACH_FREE_THE_LURKERS');
  FAchievements.Add('02killedmycorpse', 'ACH_FREE_LYDIA');
  FAchievements.Add('03clearthepass', 'ACH_LETTER_TO_THE_KING');
  FAchievements.Add('03mabondone', 'ACH_PAPER_PUSHING');
  FAchievements.Add('03amuletquestcomp', 'ACH_KNIGHT_QUEST');
  FAchievements.Add('03signetreturned|03crownreturned', 'ACH_SALVAGE_RUN');
  FAchievements.Add('Weapon Mastery|Pain Control|Fortitude', 'ACH_OSKARIS_TRAINING_TASKS');
  FAchievements.Add('03know', 'ACH_GO_BACK_TO_THE_FOREST');
  FAchievements.Add('04sawram|04sawnaga|04sawwarriors', 'ACH_SCOUT_THE_ENEMY_CAMP');
  FAchievements.Add('04staffdone', 'ACH_A_LITTLE_FAVOR_FOR_OLON');
  FAchievements.Add('04satcheldone', 'ACH_STEAL_THE_SATCHEL');
  FAchievements.Add('04EdgardSafe', 'ACH_RESCUE_EDGARD');
  FAchievements.Add('04solocomplete', 'ACH_RANGER_QUEST');
  FAchievements.Add('04learnedcamo|04learnedfocus|04learnedsw', 'ACH_SCOUTMASTERS_TRAINING_TASKS');
  FAchievements.Add('04ramdestroyed', 'ACH_DESTROY_THE_RAM');
  FAchievements.Add('04odead', 'ACH_KILL_OVORON');
  //My idea: Mithras ONDie: player.iftitle(Hardmode);player.addtitle(06Hart);endif;
  //FAchievements.Add('06Hart', 'ACH_HARDMODE');
  // Extra SNEG
  FAchievements.Add('01EndChapter', 'ACH_ACT1');
  FAchievements.Add('02endofch2', 'ACH_ACT2');
  FAchievements.Add('03battlewon', 'ACH_ACT3');
  FAchievements.Add('04endofch4', 'ACH_ACT4');
  FAchievements.Add('05endofch5', 'ACH_ACT5');
  FAchievements.Add('stop', 'ACH_ACT6');
end;

destructor TAchievements.Destroy;
begin
  FAchievements.Free;
  inherited;
end;

function TAchievements.getIdByTitle( title: string; character: TCharacter; out ACH_ID: string ): Boolean;
var
  key: string;
  i: integer;
  complete, found: Boolean;
  arr : TArray<string>;
begin
  ACH_ID := '';
  for key in FAchievements.Keys do
  begin
    arr := key.Split(['|']);
    complete := True;
    found := False;
    for i := Low(arr) to High(arr) do
    begin
      if SameText(title, arr[i]) then
      begin
        found := true;
        Break
      end;
    end;
    // validate key
    if found then
    begin
      for i := Low(arr) to High(arr) do
      begin
        if not character.TitleExists(arr[i]) then // include wearables !?
        begin
          complete := False;
          Break;
        end;
      end;
      if complete then
      begin
        FAchievements.TryGetValue( key, ACH_ID);
        Break;
      end;
    end;
  end;
  Result := ACH_ID<>'';
end;

end.
