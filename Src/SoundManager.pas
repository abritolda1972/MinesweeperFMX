unit SoundManager;

interface

uses
  System.Generics.Collections, System.IOUtils, FMX.Media, System.SysUtils;

type
  TSoundManager = class
  private
    class var FMediaPlayers: TObjectDictionary<string, TMediaPlayer>;
  public
    // Initialization/Finalization
    class procedure Initialize;
    class procedure Finalize;

    // Load sounds
    class procedure LoadSound(const ASoundName, AFileName: string);

    // Play/Stop sounds
    class procedure PlaySound(const ASoundName: string);
    class procedure StopSound(const ASoundName: string);
  end;

implementation

{ TSoundManager }

class procedure TSoundManager.Initialize;
begin
  FMediaPlayers := TObjectDictionary<string, TMediaPlayer>.Create([doOwnsValues]);
end;

class procedure TSoundManager.Finalize;
begin
  FreeAndNil(FMediaPlayers);
end;

class procedure TSoundManager.LoadSound(const ASoundName, AFileName: string);
var
  FullPath: string;
  MediaPlayer: TMediaPlayer;
begin
  if not Assigned(FMediaPlayers) then
    raise Exception.Create('SoundManager não inicializado. Chame TSoundManager.Initialize antes.');

  FullPath := TPath.Combine(TPath.GetAppPath, AFileName);


  MediaPlayer := TMediaPlayer.Create(nil);
  try
    MediaPlayer.FileName := FullPath;
    // Preload audio (plays and stops quickly)
    MediaPlayer.Play;
    MediaPlayer.Stop;
    MediaPlayer.CurrentTime := 0;
    FMediaPlayers.Add(ASoundName, MediaPlayer);
  except
    MediaPlayer.Free;
    raise;
  end;
end;

class procedure TSoundManager.PlaySound(const ASoundName: string);
var
  MediaPlayer: TMediaPlayer;
begin
  if Assigned(FMediaPlayers) and FMediaPlayers.TryGetValue(ASoundName, MediaPlayer) then
  begin
     // Resets the audio position to the beginning
    MediaPlayer.CurrentTime := 0;
    if MediaPlayer.State = TMediaState.Playing then
      MediaPlayer.Stop;
    MediaPlayer.Play;
  end;
end;

class procedure TSoundManager.StopSound(const ASoundName: string);
var
  MediaPlayer: TMediaPlayer;
begin
  if Assigned(FMediaPlayers) and FMediaPlayers.TryGetValue(ASoundName, MediaPlayer) then
  begin
    MediaPlayer.Stop;
    MediaPlayer.CurrentTime := 0; // return to the beginning
  end;
end;

initialization
  TSoundManager.Initialize;

finalization
  TSoundManager.Finalize;

end.
