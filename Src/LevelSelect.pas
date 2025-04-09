unit LevelSelect;

interface

uses
  FMX.Forms, System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.StdCtrls, FMX.Controls, FMX.Controls.Presentation, FMX.Edit,
  FMX.Types, FMX.Objects, UnMain, FMX.Dialogs, System.Skia, FMX.Skia,
  FMX.Layouts, FMX.EditBox, FMX.SpinBox;

type
  TLevelSelectedEvent = procedure(Sender: TObject; ALevel: TGameLevel;
    Cols, Rows, Mines: Integer) of object;

  TFormLevelSelect = class(TForm)
    btnBeginner: TButton;
    btnIntermediate: TButton;
    btnExpert: TButton;
    btnAdvanced: TButton;
    btnCustom: TButton;
    rectCustom: TRectangle;
    lblCustom: TLabel;
    SkLabel1: TSkLabel;
    Label1: TLabel;
    edtColunas: TSpinBox;
    Layout1: TLayout;
    Layout2: TLayout;
    edtLinhas: TSpinBox;
    Label2: TLabel;
    Layout3: TLayout;
    edtMinas: TSpinBox;
    Label3: TLabel;
    Laybtn1: TLayout;
    Laybtn2: TLayout;
    SkSvg1: TSkSvg;
    Layicon: TLayout;
    procedure btnLevelClick(Sender: TObject);
    procedure btnCustomClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    MaxCols, MaxRows: Integer;
    FOnLevelSelected: TLevelSelectedEvent;
    function GetCustomCols: Integer;
    function GetCustomMines: Integer;
    function GetCustomRows: Integer;
    { Private declarations }
  public
    SelectedLevel: TGameLevel;
//    CustomCols: Integer;
//    CustomRows: Integer;
//    CustomMines: Integer;
  procedure ShowLevels(ParentForm: TForm);
  property CustomCols: Integer read GetCustomCols;
  property CustomRows: Integer read GetCustomRows;
  property CustomMines: Integer read GetCustomMines;
  property OnLevelSelected: TLevelSelectedEvent read FOnLevelSelected write FOnLevelSelected;
  end;

var
  FormLevelSelect: TFormLevelSelect;

implementation

{$R *.fmx}

procedure TFormLevelSelect.btnLevelClick(Sender: TObject);
begin
if Assigned(FOnLevelSelected) then
  begin
    FOnLevelSelected(Self,
      TGameLevel(TButton(Sender).Tag),
      trunc(edtColunas.Value),
      trunc(edtlinhas.Value),
      trunc(edtminas.Value)
    );
  end;
  {$IFDEF ANDROID}
  Self.Close;
  {$ENDIF}
end;

procedure TFormLevelSelect.FormClose(Sender: TObject; var Action: TCloseAction);
begin
{$IFDEF ANDROID}
  Action := TCloseAction.caFree;
  {$ENDIF}
end;

procedure TFormLevelSelect.FormCreate(Sender: TObject);
begin
// Valores máximos baseados na plataforma
 // {$IFDEF MSWINDOWS}
  MaxCols := 100;
  MaxRows := 100;
//  {$ENDIF}
//  {$IFDEF ANDROID}
//  MaxCols := Trunc(Screen.Width / 25);
//  MaxRows := Trunc((Screen.Height - 200) / 25);
//  {$ENDIF}

  edtColunas.Max:= MaxCols;
  edtlinhas.Max:= MaxRows;
  edtminas.Max:= Trunc(MaxCols * MaxRows * 0.8);

  edtColunas.Min:= 3;
  edtlinhas.Min:= 3;
  edtminas.Min:= 5;

end;

function TFormLevelSelect.GetCustomCols: Integer;
begin
 Result := trunc(edtColunas.Value) ;
end;

function TFormLevelSelect.GetCustomMines: Integer;
begin
 Result :=  trunc(edtminas.Value);;
end;

function TFormLevelSelect.GetCustomRows: Integer;
begin
 Result := trunc(edtlinhas.Value);
end;

procedure TFormLevelSelect.ShowLevels(ParentForm: TForm);
begin
 {$IFDEF MSWINDOWS}
  Self.ShowModal;
  {$ENDIF}
  {$IFDEF ANDROID}
  Self.Parent := ParentForm;
  Self.Show;
  {$ENDIF}
end;

procedure TFormLevelSelect.btnCustomClick(Sender: TObject);
begin

  try

    if (CustomCols < 3) or (CustomCols > MaxCols) then
      raise Exception.Create('Columns must be between 8-' + MaxCols.ToString);

    if (CustomRows < 3) or (CustomRows > MaxRows) then
      raise Exception.Create('Lines must be between 8-' + MaxRows.ToString);

    if (CustomMines < 5) or (CustomMines > ((MaxCols * MaxRows) * 0.9)) then
      raise Exception.Create('Mines should be in the top 5-' + Trunc((MaxCols * MaxRows) * 0.9).ToString);

    if (CustomMines < 5) or (CustomMines > ((CustomCols * CustomRows) * 0.9)) then
      raise Exception.Create('Number of mines must be less than number of cells 5-' + Trunc((CustomCols * CustomRows) * 0.9).ToString);

    SelectedLevel := glcustom;
    if Assigned(FOnLevelSelected) then
  begin
    FOnLevelSelected(Self,
      TGameLevel.glCustom,
      CustomCols,
      CustomRows,
      CustomMines
    );
  end;
  except
    on E: Exception do
      ShowMessage(E.Message);
  end;
end;

end.

