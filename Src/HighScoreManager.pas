unit HighScoreManager;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, Generics.Defaults,
  System.Math, System.IOUtils, System.JSON, System.DateUtils, UnMain, system.TypInfo;

type
  THighScore = record
    PlayerName: string;
    Time: Integer;
    Date: TDateTime;
  end;

  THighScoreManager = class
  private
    class var FHighScores: TDictionary<TGameLevel, TList<THighScore>>;
    class constructor Create;
    class destructor Destroy;
    class function LevelToStr(Level: TGameLevel): string;
    class function StrToLevel(const Value: string): TGameLevel;
  public
    class procedure LoadHighScores;
    class procedure SaveHighScores;
    class function IsNewHighScore(Level: TGameLevel; Time: Integer): Boolean;
    class procedure AddHighScore(Level: TGameLevel; const Name: string; Time: Integer);
    class function GetHighScores(Level: TGameLevel): TArray<THighScore>;
  end;

implementation

const
  HIGHSCORES_FILE = 'highscores.json';

class constructor THighScoreManager.Create;
var
  Level: TGameLevel;
begin
  FHighScores := TDictionary<TGameLevel, TList<THighScore>>.Create;
  for Level := Low(TGameLevel) to High(TGameLevel) do
    FHighScores.Add(Level, TList<THighScore>.Create);
end;

class destructor THighScoreManager.Destroy;
var
  Level: TGameLevel;
begin
  for Level := Low(TGameLevel) to High(TGameLevel) do
    FHighScores[Level].Free;
  FHighScores.Free;
end;

class function THighScoreManager.LevelToStr(Level: TGameLevel): string;
begin
  Result := GetEnumName(TypeInfo(TGameLevel), Integer(Level));
end;

class function THighScoreManager.StrToLevel(const Value: string): TGameLevel;
begin
  Result := TGameLevel(GetEnumValue(TypeInfo(TGameLevel), Value));
end;

class procedure THighScoreManager.LoadHighScores;
var
  JSONRoot: TJSONObject;
  Level: TGameLevel;
  ScoresArray: TJSONArray;
  ScoreItem: TJSONValue;
  ScoreData: TJSONObject;
  Score: THighScore;
  FilePath: string;
  i: Integer;
begin
  FilePath := TPath.Combine(TPath.GetDocumentsPath, HIGHSCORES_FILE);

  if not TFile.Exists(FilePath) then Exit;

  JSONRoot := TJSONObject.ParseJSONValue(TFile.ReadAllText(FilePath)) as TJSONObject;
  try
    for i := 0 to JSONRoot.Count - 1 do
    begin
      Level := StrToLevel(JSONRoot.Pairs[i].JsonString.Value);
      ScoresArray := JSONRoot.Pairs[i].JsonValue as TJSONArray;

      FHighScores[Level].Clear;
      for ScoreItem in ScoresArray do
      begin
        ScoreData := ScoreItem as TJSONObject;
        Score.PlayerName := ScoreData.GetValue<string>('PlayerName');
        Score.Time := ScoreData.GetValue<Integer>('Time');
        Score.Date := ISO8601ToDate(ScoreData.GetValue<string>('Date'));
        FHighScores[Level].Add(Score);
      end;
    end;
  finally
    JSONRoot.Free;
  end;
end;

class procedure THighScoreManager.SaveHighScores;
var
  JSONRoot: TJSONObject;
  Level: TGameLevel;
  LevelScores: TJSONArray;
  Score: THighScore;
  FilePath: string;
begin
  JSONRoot := TJSONObject.Create;
  try
    for Level in FHighScores.Keys do
    begin
      LevelScores := TJSONArray.Create;
      for Score in FHighScores[Level] do
      begin
        LevelScores.Add(
          TJSONObject.Create
            .AddPair('PlayerName', Score.PlayerName)
            .AddPair('Time', TJSONNumber.Create(Score.Time))
            .AddPair('Date', DateToISO8601(Score.Date))
        );
      end;
      JSONRoot.AddPair(LevelToStr(Level), LevelScores);
    end;

    FilePath := TPath.Combine(TPath.GetDocumentsPath, HIGHSCORES_FILE);
    TFile.WriteAllText(FilePath, JSONRoot.Format(2));
  finally
    JSONRoot.Free;
  end;
end;

class function THighScoreManager.IsNewHighScore(Level: TGameLevel; Time: Integer): Boolean;
begin
  Result := (FHighScores[Level].Count < 10) or
            (Time < FHighScores[Level].Last.Time);
end;

class procedure THighScoreManager.AddHighScore(Level: TGameLevel; const Name: string; Time: Integer);
var
  NewScore: THighScore;
begin
  NewScore.PlayerName := Name;
  NewScore.Time := Time;
  NewScore.Date := Now;

  FHighScores[Level].Add(NewScore);
  FHighScores[Level].Sort(
    TComparer<THighScore>.Construct(
      function(const Left, Right: THighScore): Integer
      begin
        Result := Left.Time - Right.Time;
      end
    )
  );

  while FHighScores[Level].Count > 10 do
    FHighScores[Level].Delete(10);
end;

class function THighScoreManager.GetHighScores(Level: TGameLevel): TArray<THighScore>;
begin
  Result := FHighScores[Level].ToArray;
end;

end.
