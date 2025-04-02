object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'Minesweeper com Skia4Delphi'
  ClientHeight = 500
  ClientWidth = 500
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 13
  object SkPaintBox: TSkPaintBox
    Left = 0
    Top = 41
    Width = 500
    Height = 459
    Align = alClient
    OnMouseUp = SkPaintBoxMouseUp
    OnDraw = SkPaintBoxDraw
  end
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 500
    Height = 41
    Align = alTop
    TabOrder = 0
    object lblMinas: TLabel
      Left = 112
      Top = 12
      Width = 31
      Height = 13
      Caption = 'Minas:'
    end
    object lblTempo: TLabel
      Left = 216
      Top = 12
      Width = 36
      Height = 13
      Caption = 'Tempo:'
    end
    object btnNovoJogo: TButton
      Left = 16
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Novo Jogo'
      TabOrder = 0
      OnClick = btnNovoJogoClick
    end
  end
  object Timer: TTimer
    OnTimer = TimerTimer
    Left = 240
    Top = 240
  end
end
