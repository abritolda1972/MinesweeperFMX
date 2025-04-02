unit HighScoreManager;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections,Generics.Defaults, System.Math,
  System.IOUtils, UnMain; // UnMain must contain the definition of TGameLevel

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
  public
    class procedure LoadHighScores;
    class procedure SaveHighScores;
    class function IsNewHighScore(Level: TGameLevel; Time: Integer): Boolean;
    class procedure AddHighScore(Level: TGameLevel; const Name: string; Time: Integer);
    class function GetHighScores(Level: TGameLevel): TArray<THighScore>;
  end;

implementation

const
  HIGHSCORES_FILE = 'highscores.dat';

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

class procedure THighScoreManager.LoadHighScores;
var
  Stream: TStream;
  Reader: TBinaryReader;
  Level: TGameLevel;
  Count, i: Integer;
  Score: THighScore;
  FilePath: string;
begin
  FilePath := TPath.Combine(TPath.GetDocumentsPath, HIGHSCORES_FILE);

  if TFile.Exists(FilePath) then
  begin
    Stream := TFileStream.Create(FilePath, fmOpenRead);
    try
      Reader := TBinaryReader.Create(Stream);
      try
        for Level := Low(TGameLevel) to High(TGameLevel) do
        begin
          Count := Reader.ReadInteger;
          for i := 0 to Count - 1 do
          begin
            Score.PlayerName := Reader.ReadString;
            Score.Time := Reader.ReadInteger;
            Score.Date := Reader.ReadDouble;
            FHighScores[Level].Add(Score);
          end;
        end;
      finally
        Reader.Free;
      end;
    finally
      Stream.Free;
    end;
  end;
end;

class procedure THighScoreManager.SaveHighScores;
var
  Stream: TStream;
  Writer: TBinaryWriter;
  Level: TGameLevel;
  Score: THighScore;
  FilePath: string;
begin
  FilePath := TPath.Combine(TPath.GetDocumentsPath, HIGHSCORES_FILE);

  Stream := TFileStream.Create(FilePath, fmCreate);
  try
    Writer := TBinaryWriter.Create(Stream);
    try
      for Level := Low(TGameLevel) to High(TGameLevel) do
      begin
        Writer.Write(FHighScores[Level].Count);
        for Score in FHighScores[Level] do
        begin
          Writer.Write(Score.PlayerName);
          Writer.Write(Score.Time);
          Writer.Write(Score.Date);
        end;
      end;
    finally
      Writer.Free;
    end;
  finally
    Stream.Free;
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

  // Keep only top 10
  while FHighScores[Level].Count > 10 do
    FHighScores[Level].Delete(10);
end;

class function THighScoreManager.GetHighScores(Level: TGameLevel): TArray<THighScore>;
begin
  Result := FHighScores[Level].ToArray;
end;

end.
