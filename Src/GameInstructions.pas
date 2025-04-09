unit GameInstructions;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, System.Skia,
  FMX.Skia, FMX.Layouts, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects;

type
  TFormInstructions = class(TForm)
    rectBackground: TRectangle;
    btnClose: TButton;
    ScrollBox1: TScrollBox;
    sklblContent: TSkLabel;
    procedure FormCreate(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormInstructions: TFormInstructions;

implementation

{$R *.fmx}

uses UnMain;

procedure TFormInstructions.btnCloseClick(Sender: TObject);
begin
 Close;
end;

procedure TFormInstructions.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
Action:= Tcloseaction.caFree;
end;

procedure TFormInstructions.FormCreate(Sender: TObject);
begin
 sklblContent.Words.Add(
    '# 🧨 Minesweeper - Ultimate Guide' + sLineBreak + sLineBreak +

    '## 🎯 Main Objective' + sLineBreak +
    'Reveal **all mine-free cells** using numeric clues and markers!' + sLineBreak + sLineBreak +

    '## 🕹️ Basic Controls' + sLineBreak +
    '+ First safe click: Mines are distributed after initial click' + sLineBreak +
    '+ Left-Click: Reveals cell' + sLineBreak +
    '+ Long Press (Mobile)/Right-Click (PC): Places/removes flag 🚩' + sLineBreak +
    '+ Double-Click: Reveals safe adjacent cells' + sLineBreak + sLineBreak +

    '## 🔢 Number System' + sLineBreak +
    'Each number indicates how many mines are in the **8 surrounding cells**:' + sLineBreak +
    '▸ Number 1: Light Blue' + sLineBreak +
    '▸ Number 2: Green' + sLineBreak +
    '▸ Number 3: Red' + sLineBreak + sLineBreak +

    '## 🚩 Advanced Strategies' + sLineBreak +
    '1. **Propositional Logic**:' + sLineBreak +
    '   ```' + sLineBreak +
    '   If a "1" has 1 flagged neighbor,' + sLineBreak +
    '   all other adjacent cells are safe!' + sLineBreak +
    '   ```' + sLineBreak +
    '2. **Cross Pattern**:' + sLineBreak +
    '   _Start by revealing central cells to maximize information_' + sLineBreak + sLineBreak +

    '## ⚠️ Danger Indicators' + sLineBreak +
    '||Bomb Cell|Detonated mine - Game Over|' + sLineBreak +
    '||Unmarked Cell|Potential unflagged mine|' + sLineBreak + sLineBreak +

    '## 🏆 Game Modes' + sLineBreak +
    '- Beginner (8x8 - 10 mines)' + sLineBreak +
    '- Intermediate (12x12 - 20 mines)' + sLineBreak +
    '- Expert (16x16 - 60 mines)' + sLineBreak +
    '- Advanced (30x16 - 100 mines)' + sLineBreak +
    '- Custom (Up to 100x100 cells)' + sLineBreak +
    '  With customizable number of mines' + sLineBreak + sLineBreak +

    '[📚 Learn more about](https://github.com/abritolda1972/MinesweeperFMX)');


end;

end.
