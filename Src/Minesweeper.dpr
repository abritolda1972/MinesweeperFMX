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
  GameInstructions in 'GameInstructions.pas' {FormInstructions},
  About in 'About.pas' {FrmAbout};

{$R *.res}

begin
  {$IFDEF DEBUG}
 // ReportMemoryLeaksOnShutdown := True;
  {$ENDIF}
  GlobalUseSkia := True;
  GlobalSkiaBitmapsInParallel:= true;
  GlobalUseSkiaRasterWhenAvailable:=False;
  GlobalUseVulkan:=True;
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
