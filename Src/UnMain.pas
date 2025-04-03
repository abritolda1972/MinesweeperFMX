unit UnMain;

interface

uses System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Layouts, System.DateUtils,  FMX.Ani, System.IOUtils, System.Diagnostics,
  FMX.Objects, FMX.Effects, System.Math.Vectors, System.Math, System.Generics.Collections,
  SevenSegmentDisplay, SoundManager, FMX.Gestures, FMX.Controls, FMX.Types,
  FMX.Controls.Presentation, FMX.StdCtrls, System.Skia, FMX.Skia, FMX.Forms;

const CORES_NUMEROS: array [1 .. 8] of TAlphaColor = ($FF0000FF, // Blue
    $FF009600, // Green
    $FFFF0000, // Red
    $FF000080, // Navy blue
    $FF800000, // Bordeaux
    $FF00FFFF, // Cyan
    $FF000000, // Black
    $FF808080 // Grey
    ); COR_FUNDO_3D = $FFD0D0D0; COR_BORDA_CLARA = $FFFFFFFF;
  COR_BORDA_ESCURA = $FF808080; SOMBRA_OFFSET = 2; SOMBRA_SUAVIZACAO = 4;
  LONG_TIME = 4; // Contol Time for Long Tap

type
  TGameLevel = (glBeginner, glIntermediate, glExpert, glAdvanced, glCustom);

  TFormMain = class(TForm)
    SkPaintBox: TSkPaintBox;
    Timer: TTimer;
    Rectangle1: TRectangle;
    ShadowEffect1: TShadowEffect;
    nminas: TSevenSegmentDisplay;
    NSeconds: TSevenSegmentDisplay;
    BtnNewGame: TSkSvg;
    Rectangle2: TRectangle;
    SkLabel1: TSkLabel;
    TimerLongtap: TTimer;
    LayoutGameOver: TLayout;
    RectangleOverlay: TRectangle;
    SkLabelGameOver: TSkLabel;
    FloatAnimationPulse: TFloatAnimation;
    Layout1: TLayout;
    StyleBook1: TStyleBook;
    BtnHighScores: TSkSvg;
    btnhelp: TSkSvg;
    procedure FormCreate(Sender: TObject);
    procedure SkPaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure TimerTimer(Sender: TObject);
    procedure SkPaintBoxDraw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
    procedure BtnNewGameClick(Sender: TObject);
    procedure BtnNewGameMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure BtnNewGameMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure SkPaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure TimerLongtapTimer(Sender: TObject);
    procedure BtnNewGameMouseLeave(Sender: TObject);
    procedure SkPaintBoxDblClick(Sender: TObject);
    procedure BtnHighScoresClick(Sender: TObject);
    procedure btnhelpClick(Sender: TObject);
    procedure btnhelpTap(Sender: TObject; const Point: TPointF);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    NUM_MINAS: Integer;
    FCelulaSize: Single;
    FGameOver: Boolean;
    FPrimeiroClique: Boolean;
    FTempoInicio: TDateTime;
    MinasArray: array of array of Boolean;
    CelulasReveladas: array of array of Boolean;
    CelulasMarcadas: array of array of Boolean;
    MinasRestantes: Integer;
    FCelulaExplosao: TPoint;
    FBombAnimation: ISkottieAnimation;
    FExplosionAnimation: ISkottieAnimation;
    FBombStaticImage: ISkImage;

    // Flag Animation variables
    FFlagAnimation: ISkottieAnimation;
    FAnimations: TDictionary<TPoint, Single>;
    FAnimationTimer: TTimer;

    FExplosionProgress: Single; // Control explosion progress (0..1)
    TPX, TPY: Single;
    FRevealAnimations: TDictionary<TPoint, Single>;
    FRevealTimer: TTimer;

    FGameEndAnimationProgress: Single;
    FGameEndMessage: string;
    FGameEndColor: TAlphaColor;

    FGameLevel: TGameLevel;
    FColunas: Integer;
    FLinhas: Integer;

    TempoAtual: Integer; // save the time that ended when you win

    procedure ManualTap(ASender: TObject; X, Y: Single);
    procedure ManualLongTap(X, Y: Single);
    function TodasMinasMarcadas(X, Y: Integer): Boolean;
    procedure RevealAnimationTimer(Sender: TObject);
    procedure SetLevel(ALevel: TGameLevel; Acolumns, ALines, Amines: Integer);
    procedure NovoJogo;
    procedure RevelarCelula(X, Y: Integer);
    function ContarMinasVizinhas(X, Y: Integer): Integer;
    function VerificarVitoria: Boolean;
    procedure MostrarTodasMinas;
    procedure InicializarMinas(ExcluirX, ExcluirY: Integer);
    procedure ConfigurarAnimacoes;
    procedure DesenharTextoCentralizado(const ACanvas: ISkCanvas;
      const ATexto: string; const ARect: TRectF; ACor: TAlphaColor;
      TamanhoFonte: Single);
    procedure DesenharCelula3D(const ACanvas: ISkCanvas; const ARect: TRectF;
      Revelada, Marcada: Boolean);
    procedure AnimationTimerTimer(Sender: TObject);
    function ContarBandeirasVizinhas(X, Y: Integer): Integer;
    procedure HandleLevelSelected(Sender: TObject; ALevel: TGameLevel; Cols,
      Rows, Mines: Integer);
  public
    { Public declarations }
  end;

var FormMain: TFormMain;
    ClickInterval: Cardinal = 0;

implementation

{$R *.fmx}

uses HighScoreInputForm, HighScoreManager, HighScoreTableForm, LevelSelect,
  GameInstructions;

procedure CarregarSVGDoRecurso(SkSVG: TSkSvg; const NomeRecurso: string);
var ResourceStream: TResourceStream; SVGData: TBytes;
begin
  // Creates a stream for the resource of type RCDATA
  ResourceStream := TResourceStream.Create(HInstance, NomeRecurso, RT_RCDATA);
  try
    // Read the SVG data
    SetLength(SVGData, ResourceStream.Size);
    ResourceStream.ReadBuffer(SVGData[0], ResourceStream.Size);

    // Convert to UTF-8 string and load into TSkSVG
    SkSVG.Svg.Source := TEncoding.UTF8.GetString(SVGData);
  finally ResourceStream.Free;
  end;
end;

procedure TFormMain.DesenharCelula3D(const ACanvas: ISkCanvas;
  const ARect: TRectF; Revelada, Marcada: Boolean);
var
  LPaint: ISkPaint; LPath: ISkPath; LPathBuilder: ISkPathBuilder;
begin
  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;

  if not Revelada then begin
    // Efeito de relevo para células não reveladas
    LPathBuilder := TSkPathBuilder.Create;
    LPathBuilder.AddRect(ARect);
    LPath := LPathBuilder.Detach;

    // 3D shading
    LPaint.Color := COR_FUNDO_3D;
    LPaint.Style := TSkPaintStyle.Fill;
    LPaint.ImageFilter := TSkImageFilter.MakeDropShadowOnly(SOMBRA_OFFSET,
      SOMBRA_OFFSET, SOMBRA_SUAVIZACAO, SOMBRA_SUAVIZACAO, TAlphaColors.Black);
    ACanvas.DrawPath(LPath, LPaint);

    // Gradient fill
    LPaint.Reset;
    LPaint.Shader := TSkShader.MakeGradientLinear(PointF(ARect.Left, ARect.Top),
      PointF(ARect.Right, ARect.Bottom), [TAlphaColors.White,
      COR_FUNDO_3D], [0, 1]);
    ACanvas.DrawRect(ARect, LPaint);

    // 3D edges
    LPaint.Reset;
    LPaint.StrokeWidth := 1;
    LPaint.Style := TSkPaintStyle.Stroke;

    // Clear top/left edge
    LPaint.Color := COR_BORDA_CLARA;
    ACanvas.DrawLine(PointF(ARect.Left, ARect.Bottom - 1),
      PointF(ARect.Left, ARect.Top), LPaint);
    ACanvas.DrawLine(PointF(ARect.Left, ARect.Top),
      PointF(ARect.Right - 1, ARect.Top), LPaint);

    // Bottom edge/dark right
    LPaint.Color := COR_BORDA_ESCURA;
    ACanvas.DrawLine(PointF(ARect.Right - 1, ARect.Top + 1),
      PointF(ARect.Right - 1, ARect.Bottom - 1), LPaint);
    ACanvas.DrawLine(PointF(ARect.Left + 1, ARect.Bottom - 1),
      PointF(ARect.Right - 1, ARect.Bottom - 1), LPaint);
  end else begin
    // Lowered effect for revealed cells
    LPaint.Color := TAlphaColors.White;
    LPaint.Style := TSkPaintStyle.Fill;
    ACanvas.DrawRect(ARect, LPaint);

    // Internal shade
    LPaint.Color := TAlphaColors.LtGray;
    LPaint.Style := TSkPaintStyle.Stroke;
    LPaint.StrokeWidth := 1;
    ACanvas.DrawRect(RectF(ARect.Left + 0.5, ARect.Top + 0.5, ARect.Right - 0.5,
      ARect.Bottom - 0.5), LPaint);
  end;

end;

procedure TFormMain.RevealAnimationTimer(Sender: TObject);
var LKeys: TArray<TPoint>; LKey: TPoint;
begin
  LKeys := FRevealAnimations.Keys.ToArray;
  for LKey in LKeys do begin
    FRevealAnimations[LKey] := FRevealAnimations[LKey] + 0.1;
    if FRevealAnimations[LKey] >= 1 then FRevealAnimations.Remove(LKey);
  end;
  if FRevealAnimations.Count > 0 then SkPaintBox.Redraw;
end;

procedure TFormMain.SetLevel(ALevel: TGameLevel; Acolumns, ALines, Amines: Integer);
var MaxColumns, MaxLines: Integer;
begin

  MaxColumns := floor((FormMain.Width - 20) / 25);
  MaxLines := floor((FormMain.Height - floor(Layout1.Height) - 50) / 25);

  FGameLevel := ALevel;
  case ALevel of
{$IFDEF MSWINDOWS}
  glBeginner: begin
      FColunas := 8;
      FLinhas := 8;
      NUM_MINAS := 10;
    end; // 10 mines
  glIntermediate: begin
      FColunas := 12;
      FLinhas := 12;
      NUM_MINAS := 20;
    end; // 20 mines
  glExpert: begin
      FColunas := 16;
      FLinhas := 16;
      NUM_MINAS := 60;
    end; // 60 mines
  glAdvanced: begin
      FColunas := 30;
      FLinhas := 16;
      NUM_MINAS := 100;
    end; // 100 mines
  glCustom : begin
      FColunas := Acolumns;
      FLinhas := ALines;
      NUM_MINAS := Amines;
    end;
{$ENDIF}
{$IFDEF ANDROID}
  glBeginner: begin
      FColunas := 8;
      FLinhas := 8;
      NUM_MINAS := 10;
    end; // 10 mines
  glIntermediate: begin
      FColunas := 12;
      FLinhas := 12;
      NUM_MINAS := 20;
    end; // 20 mines
  glExpert: begin
      FColunas := MaxColumns;
      FLinhas := MaxLines div 2;
      NUM_MINAS := Ceil((FColunas * FLinhas) * 0.24);
    end; // ~62 mines
  glAdvanced: begin
      FColunas := MaxColumns;
      FLinhas := MaxLines;
      NUM_MINAS := Ceil((FColunas * FLinhas) * 0.26);
    end;
  glCustom : begin
       FColunas := Acolumns;
      FLinhas := ALines;
      NUM_MINAS := Amines;
    end;
{$ENDIF}
  end;

  // Adjust PaintBox size
  SkPaintBox.Width := FColunas * 25; // Width based on columns
  SkPaintBox.Height := FLinhas * 25; // Height based on lines

{$IFDEF MSWINDOWS}
  FormMain.Width := floor(SkPaintBox.Width + 40);
  FormMain.Height := floor(SkPaintBox.Height + Layout1.Height + 90);
{$ENDIF}

end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
FRevealAnimations.Free;
FAnimations.Free;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  SetLevel(tgamelevel.glBeginner,0,0,0); // Initial level

   THighScoreManager.LoadHighScores;  //Charges all scores by levels

  ConfigurarAnimacoes;  //Configure initial animations
  NovoJogo;  //load initial game board

end;

procedure TFormMain.NovoJogo;
var I, J: Integer;
begin
  FGameOver := False;
  FPrimeiroClique := True;
  MinasRestantes := NUM_MINAS;
  FCelulaExplosao := Point(-1, -1);

  SetLength(MinasArray, FColunas, FLinhas);
  SetLength(CelulasReveladas, FColunas, FLinhas);
  SetLength(CelulasMarcadas, FColunas, FLinhas);

  for I := 0 to FColunas - 1 do
    for J := 0 to FLinhas - 1 do begin
      MinasArray[I][J] := False;
      CelulasReveladas[I][J] := False;
      CelulasMarcadas[I][J] := False;
    end;

  nminas.value := NUM_MINAS;
  NSeconds.value := 0;
  Timer.Enabled := False;
  FAnimations.Clear;
  FRevealAnimations.Clear;

  LayoutGameOver.Visible := False;
  FloatAnimationPulse.Enabled := False;
  FGameEndAnimationProgress := 0;
  FGameOver := False;

  SkPaintBox.Redraw;
end;


//only for trick the game
function TFormMain.TodasMinasMarcadas(X, Y: Integer): Boolean;
var I, J, VizX, VizY: Integer;
begin
  Result := True;
  for I := -1 to 1 do
    for J := -1 to 1 do begin
      VizX := X + I;
      VizY := Y + J;
      if (VizX >= 0) and (VizX < FColunas) and (VizY >= 0) and (VizY < FLinhas)
      then begin
        // If it's a mine and it's not marked, it returns false
        if MinasArray[VizX][VizY] and not CelulasMarcadas[VizX][VizY] then begin
          Result := False;
          Exit;
        end;
      end;
    end;
end;

procedure TFormMain.SkPaintBoxDblClick(Sender: TObject);
var I, J, VizX, VizY, CelX, CelY, MinasVizinhas, BandeirasVizinhas: Integer;
begin
  if FGameOver then Exit;

  CelX := Trunc(TPX / FCelulaSize);
  CelY := Trunc(TPY / FCelulaSize);

  if (CelX < 0) or (CelX >= FColunas) or (CelY < 0) or (CelY >= FLinhas) then
      Exit;

  if CelulasReveladas[CelX][CelY] then begin
    MinasVizinhas := ContarMinasVizinhas(CelX, CelY);
    if MinasVizinhas > 0 then begin
      BandeirasVizinhas := ContarBandeirasVizinhas(CelX, CelY);

      // Check if all neighboring mines are really marked
      if (BandeirasVizinhas = MinasVizinhas)     //and TodasMinasMarcadas(CelX, CelY)
      then begin                                 // to trick the game uncomment the previous part of the code,
                                                 //it only reveals if the cell with the correct mine is marked
        // Reveal unmarked neighboring cells
        for I := -1 to 1 do
          for J := -1 to 1 do begin
            VizX := CelX + I;
            VizY := CelY + J;
            if (VizX >= 0) and (VizX < FColunas) and (VizY >= 0) and
              (VizY < FLinhas) then begin
              if not CelulasMarcadas[VizX][VizY] and not CelulasReveladas[VizX]
                [VizY] then begin
                RevelarCelula(VizX, VizY);

                // Game Over if it reveals an unmarked mine
                if MinasArray[VizX][VizY] then begin
                  Tsoundmanager.PlaySound('explosion');
                  FCelulaExplosao := Point(VizX, VizY);
                  MostrarTodasMinas;
                  Exit;
                end;
              end;
            end;
          end;

        if VerificarVitoria then begin
          FGameOver := True;
          Timer.Enabled := False;
        end;
        SkPaintBox.Redraw;
      end;
    end;
  end;
end;

procedure TFormMain.SkPaintBoxDraw(ASender: TObject; const ACanvas: ISkCanvas;
  const ADest: TRectF; const AOpacity: Single);
var
  I, J, MinasVizinhas: Integer;
  CelulaRect, LDestRect: TRectF;
  LProgress, LRevealProgress, LTextWidth,
    LTextHeight: Single;
  APaint, LPaint: ISkPaint;
  LFlagPath: ISkPath;
  LGradient: ISkShader;
  LKey: TPoint;
  LFont: ISkFont;
  LTextBlob: ISkTextBlob;
   LScale, LTranslate: TMatrix;
  LMetrics: TskfontMetrics;
  LBaseColor: TAlphaColorF;
  LDarkerColor: TAlphaColor;
begin
  FCelulaSize := Min(SkPaintBox.Width / FColunas, SkPaintBox.Height / FLinhas);

  // Drawing the bottom of the board
  APaint := TSkPaint.Create;
  APaint.Color := TAlphaColors.Darkgray;
  ACanvas.DrawRect(RectF(0, 0, SkPaintBox.Width, SkPaintBox.Height), APaint);

  for I := 0 to FColunas - 1 do
    for J := 0 to FLinhas - 1 do begin
      CelulaRect := RectF(I * FCelulaSize, J * FCelulaSize,
        (I + 1) * FCelulaSize, (J + 1) * FCelulaSize);

      // Draw cell with 3D effect
      DesenharCelula3D(ACanvas, CelulaRect, CelulasReveladas[I][J],
        CelulasMarcadas[I][J]);

      // Revealing effect
      if FRevealAnimations.TryGetValue(Point(I, J), LRevealProgress) then begin
        LPaint := TSkPaint.Create;
        LPaint.AntiAlias := True;
        // ******* Uncomment this code and comment out the following Lgradient for multicolor in the reveal

        // Determine base color based on number of neighboring mines
        // var LNumber := ContarMinasVizinhas(I, J);
        // var LBaseColor: TAlphaColor;
        //
        // case LNumber of
        // 1: LBaseColor := $FF00FF00; // Verde
        // 2: LBaseColor := $FFFFFF00; // Amarelo
        // 3: LBaseColor := $FFFFA500; // Laranja
        // 4: LBaseColor := $FFFF0000; // Vermelho
        // 5: LBaseColor := $FF8B0000; // Vermelho escuro
        // 6: LBaseColor := $FF800080; // Roxo
        // 7: LBaseColor := $FF4B0082; // Índigo
        // 8: LBaseColor := $FF000000; // Preto
        // else LBaseColor := $FF00FFFF; // Ciano para zero
        // end;
        //
        // LGradient := TSkShader.MakeGradientRadial(
        // PointF(CelulaRect.CenterPoint.X, CelulaRect.CenterPoint.Y),
        // FCelulaSize * 0.6 * LRevealProgress,
        // [TAlphaColorF.Create(LBaseColor).ToAlphaColor,          // Main color
        // TAlphaColorF.Create(LBaseColor).ToAlphaColor,          // Maintains intensity
        // TAlphaColorF.Create(LBaseColor).ToAlphaColor,     // Smooth transition
        // TAlphaColorF.Create(TAlphaColors.White).ToAlphaColor], // Final dissipation
        // [0, 0.3, 0.7, 1],
        // TSkTileMode.Clamp);

        // Circular wave gradient
        LGradient := TSkShader.MakeGradientRadial
          (PointF(CelulaRect.CenterPoint.X, CelulaRect.CenterPoint.Y),
          FCelulaSize * 0.6 * LRevealProgress,
          [TAlphaColorF.Create(TAlphaColors.White).ToAlphaColor,
          TAlphaColorF.Create(TAlphaColors.Black).ToAlphaColor,
          TAlphaColorF.Create(TAlphaColors.Lightblue).ToAlphaColor,
          TAlphaColorF.Create(TAlphaColors.red).ToAlphaColor], [0, 0.3, 0.7, 1],
          TSkTileMode.Clamp);

        LPaint.Shader := LGradient;
        LPaint.Style := TSkPaintStyle.Stroke;
        LPaint.StrokeWidth := FCelulaSize * 0.3 * (1 - LRevealProgress);
        ACanvas.DrawCircle(CelulaRect.CenterPoint,
          FCelulaSize * 0.4 * LRevealProgress, LPaint);
      end;
      if CelulasMarcadas[I][J] then begin
        LKey := Point(I, J);
        if FAnimations.TryGetValue(LKey, LProgress) then begin
          FFlagAnimation.SeekFrametime(LProgress);

          LDestRect := RectF(I * FCelulaSize + FCelulaSize * 0.2,
            J * FCelulaSize + FCelulaSize * 0.2, (I + 1) * FCelulaSize -
            FCelulaSize * 0.2, (J + 1) * FCelulaSize - FCelulaSize * 0.2);

          FFlagAnimation.Render(ACanvas, LDestRect);
        end;
      end;

      // Cell content
      if CelulasReveladas[I][J] then begin
        if MinasArray[I][J] then begin
          // Cell containing mine
          LDestRect := RectF(CelulaRect.Left + (CelulaRect.Width - FCelulaSize *
            0.8) / 2, CelulaRect.Top + (CelulaRect.Height - FCelulaSize * 0.8) /
            2, CelulaRect.Left + (CelulaRect.Width + FCelulaSize * 0.8) / 2,
            CelulaRect.Top + (CelulaRect.Height + FCelulaSize * 0.8) / 2);

          // Check if it is the cell that caused the explosion
          if (I = FCelulaExplosao.X) and (J = FCelulaExplosao.Y) and FGameOver
          then begin
            if FExplosionProgress < 1 then begin
              FExplosionAnimation.SeekFrametime(FExplosionProgress *
                FExplosionAnimation.Duration);
              FExplosionAnimation.Render(ACanvas, LDestRect);
            end
            else ACanvas.DrawImageRect(FBombStaticImage, LDestRect);
          end else begin
            // Other cells with mines only show the static image
            ACanvas.DrawImageRect(FBombStaticImage, LDestRect);
          end;
        end else begin
          // Cell without mine
          MinasVizinhas := ContarMinasVizinhas(I, J);
          if MinasVizinhas > 0 then
              DesenharTextoCentralizado(ACanvas, MinasVizinhas.ToString,
              CelulaRect, CORES_NUMEROS[MinasVizinhas], CelulaRect.Size.cx / 2);
        end;
      end;
    end;

  // Draw end-of-game message
  if LayoutGameOver.Visible then begin
    LPaint := TSkPaint.Create;
    LFont := TSkFont.Create(TSkTypeFace.MakeFromName('Impact',
      TSkFontStyle.Bold), 60);


    LBaseColor := TAlphaColorF.Create(FGameEndColor);
    LDarkerColor := TAlphaColorF.Create(EnsureRange(LBaseColor.R - 0.2, 0.0,
      1.0), EnsureRange(LBaseColor.G - 0.2, 0.0, 1.0),
      EnsureRange(LBaseColor.B - 0.2, 0.0, 1.0), LBaseColor.A).ToAlphaColor;

    // Animated gradient effect
    LPaint.Shader := TSkShader.MakeGradientLinear(PointF(0, 0),
      PointF(SkPaintBox.Width, SkPaintBox.Height), [FGameEndColor, LDarkerColor,
      FGameEndColor], [0, 0.5, 1], TSkTileMode.Mirror);

    // Shadow effect
    LPaint.ImageFilter := TSkImageFilter.MakeDropShadow(5, 5, 8, 8,
      TAlphaColors.Black);

    LTextWidth := LFont.MeasureText(FGameEndMessage);
    LFont.GetMetrics(LMetrics);
    LTextHeight := Abs(LMetrics.Ascent) + Abs(LMetrics.Descent);

    // Entry animation
    if FGameEndAnimationProgress < 1 then begin
      FGameEndAnimationProgress := FGameEndAnimationProgress + 0.05;
      LScale := TMatrix.CreateScaling(FGameEndAnimationProgress,
        FGameEndAnimationProgress);
      LTranslate := TMatrix.CreateTranslation
        ((SkPaintBox.Width - (LTextWidth * FGameEndAnimationProgress)) / 2,
        (SkPaintBox.Height - (LTextHeight * FGameEndAnimationProgress)) / 2);
      ACanvas.Concat(LScale * LTranslate);
    end;

    LTextBlob := TSkTextBlob.MakeFromText(FGameEndMessage, LFont);
    ACanvas.DrawTextBlob(LTextBlob, (SkPaintBox.Width - LTextWidth) / 2,
      (SkPaintBox.Height / 2) + (LTextHeight / 2) -
      Abs(LMetrics.Descent), LPaint);
  end;
end;

// Auxiliary procedure for safe initialization of mines
procedure TFormMain.InicializarMinas(ExcluirX, ExcluirY: Integer);
var I, J, Contador: Integer;
begin
  Randomize;
  Contador := 0;

  while Contador < NUM_MINAS do begin
    I := Random(FColunas);
    J := Random(FLinhas);

    if (I = ExcluirX) and (J = ExcluirY) then Continue;

    if not MinasArray[I][J] then begin
      MinasArray[I][J] := True;
      Inc(Contador);
    end;
  end;
end;

procedure TFormMain.SkPaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
  ClickInterval := 0;
  TimerLongtap.Enabled := True;
  TPX := X; // Stores current X coordinate
  TPY := Y; // Stores current Y coordinate
end;

procedure TFormMain.ManualLongTap(X, Y: Single);
var CelX, CelY: Integer; LCelula: TPoint;
begin
  if FGameOver then Exit;
  CelX := Trunc(X / FCelulaSize);
  CelY := Trunc(Y / FCelulaSize);

  if (CelX < 0) or (CelX >= FColunas) or (CelY < 0) or (CelY >= FLinhas) then
      Exit;

  Tsoundmanager.PlaySound('dropflag');
  LCelula := Point(CelX, CelY);
  if CelulasMarcadas[CelX][CelY] then begin
    // Remove animation
    FAnimations.Remove(LCelula);

  end else begin
    // Start animation
    FAnimations.AddOrSetValue(LCelula, 0);

  end;
  if not CelulasReveladas[CelX][CelY] then begin
    CelulasMarcadas[CelX][CelY] := not CelulasMarcadas[CelX][CelY];
    MinasRestantes := MinasRestantes +
      IfThen(CelulasMarcadas[CelX][CelY], -1, 1);
    nminas.value := MinasRestantes;

  end;

  SkPaintBox.Redraw;
End;

procedure TFormMain.ManualTap(ASender: TObject; X, Y: Single);
var CelX, CelY: Integer;
begin
{$IFDEF ANDROID}
  if (ASender = SkPaintBox) then Begin

    CelX := Trunc(X / FCelulaSize);
    CelY := Trunc(Y / FCelulaSize);

    if FGameOver then Exit;

    if (CelX < 0) or (CelX >= FColunas) or (CelY < 0) or (CelY >= FLinhas) then
        Exit;

    Tsoundmanager.PlaySound('click');
    if CelulasMarcadas[CelX][CelY] then Exit;

    if FPrimeiroClique then begin
      InicializarMinas(CelX, CelY);
      FTempoInicio := Now;
      Timer.Enabled := True;
      FPrimeiroClique := False;
    end;

    if MinasArray[CelX][CelY] then begin
      Tsoundmanager.PlaySound('explosion');
      FCelulaExplosao := TPoint.Create(CelX, CelY);
      MostrarTodasMinas;
    end else begin
      RevelarCelula(CelX, CelY);
      if VerificarVitoria then begin
        Tsoundmanager.PlaySound('venceu');
        FGameOver := True;
        Timer.Enabled := False;
      end;
    end;
    SkPaintBox.Redraw;

  End;
{$ENDIF}
End;

procedure TFormMain.SkPaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
var CelX, CelY: Integer; LCelula: TPoint;
begin
{$IFDEF ANDROID}
  if ClickInterval < LONG_TIME then begin
    TimerLongtap.Enabled := False;
    ClickInterval := 0;
    ManualTap(Sender, X, Y); // 'ListBox Short Touch..!!');
  end;
{$ENDIF}
{$IFDEF MSWINDOWS}
  if FGameOver then Exit;

  CelX := Trunc(X / FCelulaSize);
  CelY := Trunc(Y / FCelulaSize);

  if (CelX < 0) or (CelX >= FColunas) or (CelY < 0) or (CelY >= FLinhas) then
      Exit;

  if Button = TMouseButton.mbRight then begin
    Tsoundmanager.PlaySound('dropflag');
    LCelula := Point(CelX, CelY);
    if CelulasMarcadas[CelX][CelY] then begin
      // Remove animation
      FAnimations.Remove(LCelula);
    end else begin
      // Start animation
      FAnimations.AddOrSetValue(LCelula, 0);
    end;
    if not CelulasReveladas[CelX][CelY] then begin
      CelulasMarcadas[CelX][CelY] := not CelulasMarcadas[CelX][CelY];
      MinasRestantes := MinasRestantes +
        IfThen(CelulasMarcadas[CelX][CelY], -1, 1);
      nminas.value := MinasRestantes;
      SkPaintBox.Redraw;
    end;
  end else if Button = TMouseButton.mbLeft then begin

    if CelulasMarcadas[CelX][CelY] then Exit;

    if FPrimeiroClique then begin
      InicializarMinas(CelX, CelY);
      FTempoInicio := Now;
      Timer.Enabled := True;
      FPrimeiroClique := False;
    end;

    if MinasArray[CelX][CelY] then begin
      Tsoundmanager.PlaySound('explosion');
      FCelulaExplosao := Point(CelX, CelY);
      MostrarTodasMinas;
    end else begin
      Tsoundmanager.PlaySound('click');
      RevelarCelula(CelX, CelY);
      if VerificarVitoria then begin
        FGameOver := True;
        Timer.Enabled := False;
      end;
    end;
    SkPaintBox.Redraw;
  end;
{$ENDIF}
end;

procedure TFormMain.RevelarCelula(X, Y: Integer);
begin
  if (X < 0) or (X >= FColunas) or (Y < 0) or (Y >= FLinhas) then Exit;
  if CelulasReveladas[X][Y] or CelulasMarcadas[X][Y] then Exit;

  // Start reveal animation
  FRevealAnimations.AddOrSetValue(Point(X, Y), 0);

  CelulasReveladas[X][Y] := True;

  if ContarMinasVizinhas(X, Y) = 0 then begin
    RevelarCelula(X - 1, Y - 1);
    RevelarCelula(X - 1, Y);
    RevelarCelula(X - 1, Y + 1);
    RevelarCelula(X, Y - 1);
    RevelarCelula(X, Y + 1);
    RevelarCelula(X + 1, Y - 1);
    RevelarCelula(X + 1, Y);
    RevelarCelula(X + 1, Y + 1);
  end;
end;

function TFormMain.ContarBandeirasVizinhas(X, Y: Integer): Integer;
var I, J: Integer;
begin
  Result := 0;
  for I := -1 to 1 do
    for J := -1 to 1 do
      if (X + I >= 0) and (X + I < FColunas) and (Y + J >= 0) and
        (Y + J < FLinhas) then
        if CelulasMarcadas[X + I][Y + J] then Inc(Result);
end;

function TFormMain.ContarMinasVizinhas(X, Y: Integer): Integer;
var I, J: Integer;
begin
  Result := 0;
  for I := -1 to 1 do
    for J := -1 to 1 do
      if (X + I >= 0) and (X + I < FColunas) and (Y + J >= 0) and
        (Y + J < FLinhas) then
        if MinasArray[X + I][Y + J] then Inc(Result);
end;

procedure TFormMain.AnimationTimerTimer(Sender: TObject);
var
  LKey: Tpoint;
begin
  // Update animation progress
  for LKey in FAnimations.Keys.ToArray do begin
    FAnimations[LKey] := FAnimations[LKey] + 0.06; // Incrementa 16ms
    if FAnimations[LKey] > FFlagAnimation.Duration then FAnimations[LKey] := 0;
  end;

  // Update explosion progress
  if FGameOver and (FExplosionProgress < 1) then begin
    FExplosionProgress := FExplosionProgress +
      (0.06 / FExplosionAnimation.Duration);
    if FExplosionProgress > 1 then FExplosionProgress := 1;
  end;
  SkPaintBox.Redraw;
end;

procedure TFormMain.TimerLongtapTimer(Sender: TObject);
begin
{$IFDEF ANDROID}
  ClickInterval := ClickInterval + 1;

  if (ClickInterval = LONG_TIME) then begin
    TimerLongtap.Enabled := False;
    ManualLongTap(TPX, TPY);
  end;
{$ENDIF}
end;

procedure TFormMain.TimerTimer(Sender: TObject);
begin
  if not FGameOver then NSeconds.value := SecondsBetween(Now, FTempoInicio)
  else // if FExplosionProgress >= 1 then
  begin
    Timer.Enabled := False;
    Tsoundmanager.PlaySound('gameover');
  end;
end;

procedure TFormMain.ConfigurarAnimacoes;
var LSurface: ISkSurface; LCanvas: ISkCanvas; LDestRect: TRectF;
begin
 // Load sounds

{$IFDEF MSWINDOWS}
  Tsoundmanager.LoadSound('click', 'Assets\clicksound.mp3');
  Tsoundmanager.LoadSound('explosion', 'Assets\explosion.mp3');
  Tsoundmanager.LoadSound('gameover', 'Assets\gameover.mp3');
  Tsoundmanager.LoadSound('venceu', 'Assets\venceu.mp3');
  Tsoundmanager.LoadSound('dropflag', 'Assets\pin.mp3');
{$ENDIF}
{$IFDEF ANDROID}
  Tsoundmanager.LoadSound('click',
    System.IOUtils.TPath.Combine(System.IOUtils.TPath.GetDocumentsPath,
    'clicksound.mp3'));
  Tsoundmanager.LoadSound('explosion',
    System.IOUtils.TPath.Combine(System.IOUtils.TPath.GetDocumentsPath,
    'explosion.mp3'));
  Tsoundmanager.LoadSound('gameover',
    System.IOUtils.TPath.Combine(System.IOUtils.TPath.GetDocumentsPath,
    'gameover.mp3'));
  Tsoundmanager.LoadSound('venceu',
    System.IOUtils.TPath.Combine(System.IOUtils.TPath.GetDocumentsPath,
    'venceu.mp3'));
  Tsoundmanager.LoadSound('dropflag',
    System.IOUtils.TPath.Combine(System.IOUtils.TPath.GetDocumentsPath,
    'Pin.mp3'));
{$ENDIF}

  FRevealAnimations := TDictionary<TPoint, Single>.Create;
  FRevealTimer := TTimer.Create(Self);
  FRevealTimer.Interval := 16; // 60 FPS
  FRevealTimer.OnTimer := RevealAnimationTimer;
  FRevealTimer.Enabled := True;

  Timer.Interval := 16; // Set to 16ms (60 FPS)

  // Loading Lottie animations
{$IFDEF MSWINDOWS}
  FBombAnimation := TSkottieAnimation.MakeFromFile('Assets\bomb.json');
  FExplosionAnimation := TSkottieAnimation.MakeFromFile
    ('Assets\explosion.json');
{$ENDIF}
{$IFDEF ANDROID}
  FBombAnimation := TSkottieAnimation.MakeFromFile
    (System.IOUtils.TPath.Combine(System.IOUtils.TPath.GetDocumentsPath,
    'bomb.json'));
  FExplosionAnimation := TSkottieAnimation.MakeFromFile
    (System.IOUtils.TPath.Combine(System.IOUtils.TPath.GetDocumentsPath,
    'explosion.json'));
{$ENDIF}
  // Pre-render static pump frame
  LSurface := TSkSurface.MakeRaster(40, 40);
  LCanvas := LSurface.Canvas;

  // Clear the canvas
  LCanvas.Clear($00000000);

  // Render the first frame of the bomb animation
  LDestRect := RectF(0, 0, 40, 40);
  FBombAnimation.SeekFrame(0);
  FBombAnimation.Render(LCanvas, LDestRect);

  // Create the image snapshot
  FBombStaticImage := LSurface.MakeImageSnapshot;

{$IFDEF MSWINDOWS}
  FFlagAnimation := TSkottieAnimation.MakeFromFile
    ('Assets\flag_animation.json');
{$ENDIF}
{$IFDEF ANDROID}
  FFlagAnimation := TSkottieAnimation.MakeFromFile
    (System.IOUtils.TPath.Combine(System.IOUtils.TPath.GetDocumentsPath,
    'flag_animation.json'));
{$ENDIF}
  FAnimations := TDictionary<TPoint, Single>.Create;

  FAnimationTimer := TTimer.Create(Self);
  FAnimationTimer.Interval := 16; // ~60 FPS
  FAnimationTimer.OnTimer := AnimationTimerTimer;
  FAnimationTimer.Enabled := True;

end;

function TFormMain.VerificarVitoria: Boolean;
var I, J: Integer;
begin
  for I := 0 to FColunas - 1 do
    for J := 0 to FLinhas - 1 do
      if not MinasArray[I][J] and not CelulasReveladas[I][J] then Exit(False);
  Result := True;
  if Result then begin
    Tsoundmanager.PlaySound('venceu');
    FGameEndMessage := 'VITÓRIA!';
    FGameEndColor := $FF44FF44;
    FGameEndAnimationProgress := 0;
    LayoutGameOver.Visible := True;
    FloatAnimationPulse.Enabled := True;

    TempoAtual := SecondsBetween(Now, FTempoInicio);

    if THighScoreManager.IsNewHighScore(FGameLevel, TempoAtual) then begin
      TFormHighScoreInput.ShowInput('',
        procedure(const PlayerName: string; Accepted: Boolean)
        begin
          if Accepted and (PlayerName <> '') then begin
            THighScoreManager.AddHighScore(FGameLevel, PlayerName, TempoAtual);
            THighScoreManager.SaveHighScores;
          end;
        end);
    end;
  end;

end;

procedure TFormMain.MostrarTodasMinas;
var
  I, J: Integer;
begin
  FGameOver := True;
  FGameEndMessage := 'YOU LOOSE';
  FGameEndColor := $FFFF4444;
  FGameEndAnimationProgress := 0;
  LayoutGameOver.Visible := True;
  FloatAnimationPulse.Enabled := True;

  FExplosionProgress := 0; // Restart progress
  Timer.Enabled := True;

  // Reveal all the mines
  for I := 0 to FColunas - 1 do
    for J := 0 to FLinhas - 1 do
      if MinasArray[I][J] then CelulasReveladas[I][J] := True;

  // Ensure that the SkPaintBox is redesigned to show mines
  SkPaintBox.Redraw;
end;

procedure TFormMain.btnhelpClick(Sender: TObject);
begin
 {$IFDEF MSWINDOWS}
   btnhelpTap(sender,Tpointf.Create(0,0));
 {$ENDIF}

end;

procedure TFormMain.btnhelpTap(Sender: TObject; const Point: TPointF);
var
  InstructionsForm: TFormInstructions;
begin
  InstructionsForm := TFormInstructions.Create(nil); // Create the form
  try
    // Platform-specific settings
    {$IFDEF MSWINDOWS}
    InstructionsForm.ShowModal; // Displays as modal in Windows
    {$ENDIF}
    {$IFDEF ANDROID}
    InstructionsForm.Show; // Displays as normal screen on Android

    {$ENDIF}
  finally
//
  end;
end;

procedure TFormMain.BtnHighScoresClick(Sender: TObject);
var Form: TFormHighScoreTable;
begin
  Form := TFormHighScoreTable.Create(nil);
  try
    Form.ShowHighScores(FGameLevel);

    // Android-specific configuration
{$IFDEF ANDROID}
    Form.Show;
{$ENDIF}
  except
    Form.Free;
    raise;
  end;

end;

procedure TFormMain.HandleLevelSelected(Sender: TObject; ALevel: TGameLevel;
  Cols, Rows, Mines: Integer);
begin
  if ALevel = glCustom then begin
    SetLevel(ALevel,Cols,Rows,Mines);
  end else begin
    SetLevel(ALevel,0,0,0);
  end;
  NovoJogo;
  TFormLevelSelect(Sender).close;
end;

procedure TFormMain.BtnNewGameClick(Sender: TObject);
var
  FormLevel: TFormLevelSelect;
begin
  FormLevel := TFormLevelSelect.Create(nil);
  try
    FormLevel.OnLevelSelected := HandleLevelSelected; //callback
    FormLevel.edtColunas.Value:= FColunas;
    FormLevel.edtlinhas.Value:=  FLinhas;
    FormLevel.edtminas.Value:=  NUM_MINAS;
    {$IFDEF MSWINDOWS}
    FormLevel.ShowModal;
    {$ENDIF}
    {$IFDEF ANDROID}
    FormLevel.ShowLevels(Self);
    {$ENDIF}
  except
    FormLevel.Free;
  end;
end;

procedure TFormMain.BtnNewGameMouseDown(Sender: TObject; Button: TMouseButton;
Shift: TShiftState; X, Y: Single);
begin
  CarregarSVGDoRecurso(BtnNewGame, 'BTNGREEN');
end;

procedure TFormMain.BtnNewGameMouseLeave(Sender: TObject);
begin
  CarregarSVGDoRecurso(BtnNewGame, 'BTNBLACK');
end;

procedure TFormMain.BtnNewGameMouseUp(Sender: TObject; Button: TMouseButton;
Shift: TShiftState; X, Y: Single);
begin
  CarregarSVGDoRecurso(BtnNewGame, 'BTNBLACK');
end;

procedure TFormMain.DesenharTextoCentralizado(const ACanvas: ISkCanvas;
const ATexto: string; const ARect: TRectF; ACor: TAlphaColor;
TamanhoFonte: Single);
var LFont: ISkFont; LPaint: ISkPaint; LGlyphs: TArray<Word>;
  LPositions: TArray<TPointF>; LTextBlob: ISkTextBlob; LTextBounds: TRectF;
  LOrigin: TPointF;
begin
  LPaint := TSkPaint.Create;
  LPaint.Color := ACor;

  LFont := TSkFont.Create(TSkTypeFace.MakeFromName('Arial',
    TSkFontStyle.Normal), TamanhoFonte);
  LFont.Hinting := TSkFontHinting.None;

  // Measure text
  LFont.MeasureText(ATexto, LTextBounds);

  // Calculate origin
  LOrigin := TPointF.Create(ARect.Left + (ARect.Width - LTextBounds.Width) / 2,
    ARect.Top + (ARect.Height - LTextBounds.Height) / 2 + LTextBounds.Height);

  // Create glyphs
  LGlyphs := LFont.GetGlyphs(ATexto);
  LPositions := LFont.GetPositions(LGlyphs, LOrigin);

  // Create and draw blob
  LTextBlob := TSkTextBlob.MakeFromTextPositioned(ATexto, LPositions, LFont);
  ACanvas.DrawTextBlob(LTextBlob, 0, 0, LPaint);
end;

end.
