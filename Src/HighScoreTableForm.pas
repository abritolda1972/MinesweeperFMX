unit HighScoreTableForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Types,
  FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.ListView.Types, FMX.ListView.Appearances,
  FMX.ListView.Adapters.Base, FMX.ListView, HighScoreManager, UnMain, System.Skia,
  FMX.Skia;

type
  TFormHighScoreTable = class(TForm)
    ListView: TListView;
    BtnClose: TButton;
    SkLabel1: TSkLabel;
    procedure BtnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  public
    procedure ShowHighScores(Level: TGameLevel);
  end;

implementation

{$R *.fmx}

procedure TFormHighScoreTable.BtnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TFormHighScoreTable.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 Action := TCloseAction.caFree;
end;

procedure TFormHighScoreTable.ShowHighScores(Level: TGameLevel);
var
  Scores: TArray<THighScore>;
  Item: TListViewItem;
  i: Integer;
begin
 {$IFDEF ANDROID}
  Position := TFormPosition.ScreenCenter;
  StyleBook := Application.MainForm.StyleBook;
  {$ENDIF}

  ListView.Items.Clear;
  Scores := THighScoreManager.GetHighScores(Level);

  for i := 0 to High(Scores) do
  begin
    Item := ListView.Items.Add;
    Item.Text := Format('%d. %s', [i+1, Scores[i].PlayerName]);
    Item.Detail := Format('%d seconds - %s', [
      Scores[i].Time,
      FormatDateTime('dd/mm/yyyy', Scores[i].Date)
    ]);
  end;

  // Display correctly on both platforms
  {$IFDEF MSWINDOWS}
  ShowModal;
  {$ELSE}
  Show;
  {$ENDIF}
end;

end.
