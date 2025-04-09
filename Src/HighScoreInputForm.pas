unit HighScoreInputForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, FMX.Types,
  FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Controls.Presentation,
  FMX.StdCtrls, FMX.Edit, FMX.Layouts, System.Skia, FMX.Skia;

type
  TInputResultProc = reference to procedure(const PlayerName: string; Accepted: Boolean);

  TFormHighScoreInput = class(TForm)
    EditName: TEdit;
    BtnSave: TButton;
    LayoutMain: TLayout;
    BtnCancel: TButton;
    SkLabel1: TSkLabel;
    Laybtns: TLayout;
    Layout1: TLayout;
    SkSvg1: TSkSvg;
    procedure BtnSaveClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
  private
    { Private declarations }
    FCallback: TInputResultProc;
  public
    class procedure ShowInput(const CurrentName: string; Callback: TInputResultProc);
  end;

implementation

{$R *.fmx}

uses UnMain;

class procedure TFormHighScoreInput.ShowInput(const CurrentName: string; Callback: TInputResultProc);
var
  Form: TFormHighScoreInput;
begin
 Form := TFormHighScoreInput.Create(nil);
  try
    Form.EditName.Text := CurrentName;
    Form.FCallback := Callback;

   // Android-specific configuration
    {$IFDEF ANDROID}
    Form.Position := TFormPosition.ScreenCenter;
    Form.StyleBook := Application.MainForm.StyleBook;
    {$ENDIF}

    Form.Show;
  except
    Form.Free;
    raise;
  end;
end;



procedure TFormHighScoreInput.BtnCancelClick(Sender: TObject);
begin
if Assigned(FCallback) then
    FCallback('', False);
  Close;
end;

procedure TFormHighScoreInput.BtnSaveClick(Sender: TObject);
begin
if Assigned(FCallback) then
    FCallback(EditName.Text.Trim, True);
  Close;
end;

end.
