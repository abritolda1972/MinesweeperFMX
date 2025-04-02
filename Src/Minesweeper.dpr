program Minesweeper;

{$R *.dres}

uses
  System.StartUpCopy,
  FMX.Forms,
  FMX.Types,
  FMX.Skia,
  UnMain in 'UnMain.pas' {FormMain},
  SoundManager in 'SoundManager.pas',
  HighScoreManager in 'HighScoreManager.pas',
  HighScoreInputForm in 'HighScoreInputForm.pas' {FormHighScoreInput},
  HighScoreTableForm in 'HighScoreTableForm.pas' {FormHighScoreTable},
  LevelSelect in 'LevelSelect.pas' {FormLevelSelect},
  GameInstructions in 'GameInstructions.pas' {FormInstructions};

{$R *.res}

begin
  GlobalUseSkia := True;
  GlobalUseSkiaRasterWhenAvailable:=False;
  GlobalUseVulkan:=True;
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Portrait];
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
