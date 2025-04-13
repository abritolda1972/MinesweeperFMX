unit UnMain;

interface

uses System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Layouts, System.DateUtils,  FMX.Ani, System.IOUtils, System.Diagnostics,
  FMX.Objects, FMX.Effects, System.Math.Vectors, System.Math, System.Generics.Collections,
  SoundManager, FMX.Gestures, FMX.Controls, FMX.Types,
  FMX.Controls.Presentation, FMX.StdCtrls, System.Skia, FMX.Skia, FMX.Forms;

const CORES_NUMEROS: array [1 .. 8] of TAlphaColor = ($FF0000FF, // Blue
    $FF009600, // Green
    $FFFF0000, // Red
    $FF000080, // Navy blue
    $FF800000, // Bordeaux
    $FF00FFFF, // Cyan
    $FF000000, // Black
    $FF808080 // Grey
    );
    COR_FUNDO_3D = $FF2D2F31;
    COR_BORDA_CLARA = $FFF6CA6B;
    COR_BORDA_ESCURA = $FFA8552A;
    COR_GRADIENTA = $FF37393C;


    SOMBRA_OFFSET = 2;
    SOMBRA_SUAVIZACAO = 4;
    LONG_TIME = 4; // Contol Time for Long Tap

type
  TGameLevel = (glBeginner, glIntermediate, glExpert, glAdvanced, glCustom);

  TFormMain = class(TForm)
    SkPaintBox: TSkPaintBox;
    Timer: TTimer;
    RectBackground: TRectangle;
    ShadowEffect1: TShadowEffect;
    imgLogoGame: TSkSvg;
    recttop: TRectangle;
    TimerLongtap: TTimer;
    LayoutGameOver: TLayout;
    RectangleOverlay: TRectangle;
    SkLabelGameOver: TSkLabel;
    FloatAnimationPulse: TFloatAnimation;
    Layout1: TLayout;
    StyleBook1: TStyleBook;
    RectFundoJogo: TRectangle;
    ScrollBoxGame: TScrollBox;
    GestureManager1: TGestureManager;
    rectsizescroll: TRectangle;
    MiniMapPaintBox: TSkPaintBox;
    SkSvg1: TSkSvg;
    Layout2: TLayout;
    btnrepeate: TButton;
    Laybtn: TLayout;
    btnnewLevel: TButton;
    ShadowEffect2: TShadowEffect;
    SkpNrMinas: TSkPaintBox;
    SkpNrSeconds: TSkPaintBox;
    LayMenu: TLayout;
    imgsetabtnmenu: TSkSvg;
    SkSvg3: TSkSvg;
    imgfundo: TSkSvg;
    FloatAnimationMenu: TFloatAnimation;
    SkLabel2: TSkLabel;
    Layabout: TLayout;
    LayLevel: TLayout;
    IMgLevel: TSkSvg;
    SkLabel3: TSkLabel;
    LayInstructions: TLayout;
    SkSvg4: TSkSvg;
    SkLabel4: TSkLabel;
    LayHighscores: TLayout;
    SkSvg5: TSkSvg;
    SkLabel5: TSkLabel;
    LayoutLogo: TLayout;
    SkLabel1: TSkLabel;
    Imgbombnumber: TSkSvg;
    SkSvg2: TSkSvg;
    LayLoadGame: TLayout;
    ImgLoadGame: TSkSvg;
    SkLabel6: TSkLabel;
    LaySaveGame: TLayout;
    ImgSaveGame: TSkSvg;
    SkLabel7: TSkLabel;
    LayBeginGame: TLayout;
    SkSvg6: TSkSvg;
    SkLabel8: TSkLabel;
    SkLabel9: TSkLabel;
    rectfundoiniciojogo: TRectangle;
    Rectangle1: TRectangle;
    InnerGlowEffect1: TInnerGlowEffect;
    imgbuttonstart: TSkSvg;
    SkLabel10: TSkLabel;
    GlowEffect1: TGlowEffect;
    FloatAnimationStart: TFloatAnimation;
    LayMenuZoom: TLayout;
    imgMenuZoom: TSkSvg;
    imgFundoMenuZoom: TSkSvg;
    SkLabel11: TSkLabel;
    Layout3: TLayout;
    imgcelllarger: TSkSvg;
    SkLabel12: TSkLabel;
    Layout4: TLayout;
    imgcellsmall: TSkSvg;
    SkLabel13: TSkLabel;
    FloatAnimationzoom: TFloatAnimation;
    procedure FormCreate(Sender: TObject);
    procedure SkPaintBoxMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure TimerTimer(Sender: TObject);
    procedure SkPaintBoxDraw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
    procedure SkPaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure TimerLongtapTimer(Sender: TObject);
    procedure SkPaintBoxDblClick(Sender: TObject);
    procedure BtnHighScoresClick(Sender: TObject);
    procedure btnhelpClick(Sender: TObject);
    procedure btnhelpTap(Sender: TObject; const Point: TPointF);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SkPaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure FormResize(Sender: TObject);
    procedure ScrollBoxGameViewportPositionChange(Sender: TObject;
      const OldViewportPosition, NewViewportPosition: TPointF;
      const ContentSizeChanged: Boolean);
    procedure MiniMapPaintBoxDraw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
    procedure btnrepeateClick(Sender: TObject);
    procedure btnrepeateTap(Sender: TObject; const Point: TPointF);
    procedure SkpNrMinasDraw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
    procedure SkpNrSecondsDraw(ASender: TObject; const ACanvas: ISkCanvas;
      const ADest: TRectF; const AOpacity: Single);
    procedure imgsetabtnmenuClick(Sender: TObject);
    procedure FloatAnimationMenuFinish(Sender: TObject);
    procedure IMgLevelClick(Sender: TObject);
    procedure IMgLevelTap(Sender: TObject; const Point: TPointF);
    procedure btnnewLevelTap(Sender: TObject; const Point: TPointF);
    procedure btnnewLevelClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SkSvg3Tap(Sender: TObject; const Point: TPointF);
    procedure SkSvg3Click(Sender: TObject);
    procedure ImgSaveGameTap(Sender: TObject; const Point: TPointF);
    procedure ImgLoadGameTap(Sender: TObject; const Point: TPointF);
    procedure ImgSaveGameClick(Sender: TObject);
    procedure ImgLoadGameClick(Sender: TObject);
    procedure imgbuttonstartTap(Sender: TObject; const Point: TPointF);
    procedure imgbuttonstartClick(Sender: TObject);
    procedure FloatAnimationStartFinish(Sender: TObject);
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo; var Handled: Boolean);
    procedure imgMenuZoomTap(Sender: TObject; const Point: TPointF);
    procedure FloatAnimationzoomFinish(Sender: TObject);
    procedure imgMenuZoomClick(Sender: TObject);
    procedure imgcellsmallTap(Sender: TObject; const Point: TPointF);
    procedure imgcelllargerTap(Sender: TObject; const Point: TPointF);
    procedure imgcelllargerClick(Sender: TObject);
    procedure imgcellsmallClick(Sender: TObject);
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
    FCachedCellTexture: ISkImage;

    FIsScrolling: Boolean;
    FLastMousePos: TPointF;

    //Number Mines shader
    FNrMinas: Integer;
    NSKeffect :Iskruntimeeffect;
    NSKshaderBuilder: Iskruntimeshaderbuilder;
    NSKShader: Iskshader;
    error: String;

    //Number Sconds Shader
    NTKeffect :Iskruntimeeffect;
    NTKshaderBuilder: Iskruntimeshaderbuilder;
    NTKShader: Iskshader;
    FNrSeconds: Integer;

    FLastZoomDistance: Integer;

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
    procedure PreRenderCellTexture;
    procedure RepositionSkPaintBox;
    procedure DrawClock(Acanvas: ISKCanvas; Adest: TRectF; Apaint: Iskpaint);
    procedure SetNrMinas(const Value: Integer);
    procedure DrawMines(Acanvas: ISKCanvas; Adest: TRectF; Apaint: Iskpaint);
    procedure SetNrSeconds(const Value: Integer);
    procedure SalvarJogo(const NomeArquivo: string);
    procedure CarregarJogo(const NomeArquivo: string);
    procedure CloseMenu;
    procedure CloseAnimationStart;
    procedure BeginAnimationStart;
    procedure UpdateZoom;
    procedure CloseMenuZoom;
  public
    { Public declarations }
    property NrMinas: Integer read FNrMinas write SetNrMinas default 0;
    property NrSeconds: Integer read FNrSeconds write SetNrSeconds default 0;

  end;

var FormMain: TFormMain;
    ClickInterval: Cardinal = 0;

implementation

{$R *.fmx}

uses HighScoreInputForm, HighScoreManager, HighScoreTableForm, LevelSelect,
  GameInstructions, About;


Procedure TFormMain.DrawMines(Acanvas: ISKCanvas; Adest: TRectF; Apaint: Iskpaint);
Begin

if assigned(NSKShaderBuilder) then
Begin
  if(NSKeffect.UniformExists('iResolution')) then
    NSKShaderBuilder.SetUniform('iResolution',[SkpNrMinas.Width, SkpNrMinas.Height,0]);
    if(NSKeffect.UniformExists('iTime')) then
    NSKShaderBuilder.SetUniform('iTime',single(random));
  if(NSKeffect.UniformExists('nnumber')) then
    NSKShaderBuilder.SetUniform('nnumber',NrMinas);
    NSKShader:=NSKshaderBuilder.MakeShader;

    Apaint.Shader:= NSKshader;
    Acanvas.Save;
      try

      Acanvas.ClipRect(adest);
      acanvas.DrawPaint(Apaint);

    finally
      Acanvas.Restore;
    end;
End;
End;



Procedure TFormMain.DrawClock(Acanvas: ISKCanvas; Adest: TRectF; Apaint: Iskpaint);
Begin

if assigned(NTKShaderBuilder) then
Begin
  if(NTKeffect.UniformExists('iResolution')) then
    NTKShaderBuilder.SetUniform('iResolution',[SkpNrSeconds.Width, SkpNrSeconds.Height,0]);
    if(NTKeffect.UniformExists('iTime')) then
    NTKShaderBuilder.SetUniform('iTime',single(random));
  if(NTKeffect.UniformExists('nnumber')) then
    NTKShaderBuilder.SetUniform('nnumber',NrSeconds);
    NTKShader:=NTKshaderBuilder.MakeShader;

    Apaint.Shader:= NTKshader;
    Acanvas.Save;
      try

      Acanvas.ClipRect(adest);
      acanvas.DrawPaint(Apaint);

    finally
      Acanvas.Restore;
    end;
End;
End;

//procedure TFormMain.RepositionSkPaintBox;
//var
//  ScrollBoxContent: TControl;
//  RequiredWidth, RequiredHeight: Single;
//begin
//  ScrollBoxContent := ScrollBoxGame.Content;
//  // Sets the minimum size of the Content (SkPaintBox + margins)
//  RequiredWidth := SkPaintBox.Width + 40; // 20px each side
//  RequiredHeight := SkPaintBox.Height + 40;
//
//  // Forces the size of the Content (even if SkPaintBox is small)
//  ScrollBoxContent.Width := RequiredWidth;
//  ScrollBoxContent.Height := RequiredHeight;
//
//  // Updates the background rectangle
//  rectsizescroll.Position.X := 0;
//  rectsizescroll.Position.Y := 0;
//  rectsizescroll.Width := RequiredWidth;
//  rectsizescroll.Height := RequiredHeight;
//
//  // Center or position with margins
//  if (ScrollBoxGame.Width >= RequiredWidth) and (ScrollBoxGame.Height >= RequiredHeight) then
//  begin
//    // Center the field
//    rectsizescroll.Position.X := (ScrollBoxGame.Width - SkPaintBox.Width) / 2-20;
//    rectsizescroll.Position.Y := (ScrollBoxGame.Height - SkPaintBox.Height) / 2-20;
//     if (ScrollBoxGame.Width >= RequiredWidth) then
//
//       SkPaintBox.Position.X := 20;
//      SkPaintBox.Position.Y := 20;
//      MiniMapPaintBox.Visible:=false;
//  end
//  else
//  begin
//
//    // Position top left corner with margins
//    SkPaintBox.Position.X := (rectsizescroll.Width - SkPaintBox.Width) / 2;
//    SkPaintBox.Position.Y := (rectsizescroll.Height - SkPaintBox.Height) / 2;
//
//  //  rectsizescroll.Position.Y := (ScrollBoxGame.Height - SkPaintBox.Height) / 2-20;
//    MiniMapPaintBox.Visible:=True;
//  end;
//
//  // Updateo ScrollBox
//  ScrollBoxGame.InvalidateContentSize;
//end;


procedure TFormMain.RepositionSkPaintBox;
var
  ScrollBoxContent: TControl;
  RequiredWidth, RequiredHeight: Single;
  LProportionalX, LProportionalY: Single;
begin
  // Salva a posição proporcional atual do ScrollBox
  if (SkPaintBox.Width > 0) and (SkPaintBox.Height > 0) then
  begin
    LProportionalX := ScrollBoxGame.ViewportPosition.X / SkPaintBox.Width;
    LProportionalY := ScrollBoxGame.ViewportPosition.Y / SkPaintBox.Height;
  end
  else
  begin
    LProportionalX := 0;
    LProportionalY := 0;
  end;

  ScrollBoxContent := ScrollBoxGame.Content;

  // Define o tamanho necessário incluindo margens
  RequiredWidth := SkPaintBox.Width + 40;  // 20px de margem em cada lado
  RequiredHeight := SkPaintBox.Height + 40;

  // Aplica o novo tamanho ao conteúdo
  ScrollBoxContent.Width := RequiredWidth;
  ScrollBoxContent.Height := RequiredHeight;

  // Atualiza o retângulo de fundo do ScrollBox
  rectsizescroll.Position.X := 0;
  rectsizescroll.Position.Y := 0;
  rectsizescroll.Width := RequiredWidth;
  rectsizescroll.Height := RequiredHeight;

  // Centraliza ou ajusta a posição
  if (ScrollBoxGame.Width >= RequiredWidth) and (ScrollBoxGame.Height >= RequiredHeight) then
  begin
    // Modo Centralizado
    SkPaintBox.Position.X := (RequiredWidth - SkPaintBox.Width) / 2;
    SkPaintBox.Position.Y := (RequiredHeight - SkPaintBox.Height) / 2;

    rectsizescroll.Position.X := (ScrollBoxGame.Width-RequiredWidth) / 2;
    rectsizescroll.Position.Y := ( ScrollBoxGame.Height-RequiredWidth) / 2;


    MiniMapPaintBox.Visible := False;
  end
  else
  begin
    // Modo com Scroll
    if (ScrollBoxGame.Width >= RequiredWidth) then
    Begin
      //SkPaintBox.Position.X := 20;
      rectsizescroll.Position.X := (ScrollBoxGame.Width-RequiredWidth) / 2;
      SkPaintBox.Position.Y := 20;
    End
    else
    if (ScrollBoxGame.Height >= RequiredHeight) then
    Begin
      rectsizescroll.Position.Y := ( ScrollBoxGame.Height-RequiredWidth) / 2;
      SkPaintBox.Position.X := 20;
    End
    else
    Begin
      SkPaintBox.Position.X := 20;
      SkPaintBox.Position.Y := 20;
    End;

    MiniMapPaintBox.Visible := True;
  end;

  // Restaura a posição proporcional
  ScrollBoxGame.ViewportPosition := PointF(
    LProportionalX * SkPaintBox.Width,
    LProportionalY * SkPaintBox.Height
  );

  // Força atualização do ScrollBox
  ScrollBoxGame.InvalidateContentSize;
  MiniMapPaintBox.Redraw;
end;

procedure TFormMain.PreRenderCellTexture;
var
  LSurface: ISkSurface;
  LCelulaSize: Single;
begin
  LCelulaSize := FCelulaSize;
  LSurface := TSkSurface.MakeRaster(Trunc(LCelulaSize), Trunc(LCelulaSize));
  LSurface.Canvas.Clear(TAlphaColors.Null);
  DesenharCelula3D(LSurface.Canvas, RectF(0, 0, LCelulaSize, LCelulaSize), False, False);
  FCachedCellTexture := LSurface.MakeImageSnapshot;
end;

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
  LPaint: ISkPaint;
  LPath: ISkPath;
  LPathBuilder: ISkPathBuilder;
  LInnerRect: TRectF; // Retângulo interno para espaçamento
begin
  LPaint := TSkPaint.Create;
  LPaint.AntiAlias := True;

  // Reduz 1 pixel em cada lado para criar 2px de espaço entre células
  LInnerRect := RectF(
    ARect.Left + 2,
    ARect.Top + 2,
    ARect.Right - 2,
    ARect.Bottom - 2
  );

  if not Revelada then begin
    // Ajusta o caminho para o retângulo interno
    LPathBuilder := TSkPathBuilder.Create;
    LPathBuilder.AddRect(LInnerRect);
    LPath := LPathBuilder.Detach;

    // 3D shading (usando o retângulo interno)
    LPaint.Color := COR_FUNDO_3D;
    LPaint.Style := TSkPaintStyle.Fill;
    LPaint.ImageFilter := TSkImageFilter.MakeDropShadowOnly(
      SOMBRA_OFFSET,
      SOMBRA_OFFSET,
      SOMBRA_SUAVIZACAO,
      SOMBRA_SUAVIZACAO,
      TAlphaColors.Black
    );
    ACanvas.DrawPath(LPath, LPaint);

    // Gradient fill (ajustado para o retângulo interno)
    LPaint.Reset;
    LPaint.Shader := TSkShader.MakeGradientLinear(
      PointF(LInnerRect.Left, LInnerRect.Top),
      PointF(LInnerRect.Right, LInnerRect.Bottom),
      [COR_GRADIENTA, COR_FUNDO_3D],
      [0, 1]
    );
    ACanvas.DrawRect(LInnerRect, LPaint);

    // Bordas 3D (desenhadas dentro do retângulo interno)
    LPaint.Reset;
    LPaint.StrokeWidth := 1;
    LPaint.Style := TSkPaintStyle.Stroke;

    // Borda clara (topo/esquerda)
    LPaint.Color := COR_BORDA_CLARA;
    ACanvas.DrawLine(
      PointF(LInnerRect.Left, LInnerRect.Bottom),
      PointF(LInnerRect.Left, LInnerRect.Top),
      LPaint
    );
    ACanvas.DrawLine(
      PointF(LInnerRect.Left, LInnerRect.Top),
      PointF(LInnerRect.Right, LInnerRect.Top),
      LPaint
    );

    // Borda escura (direita/baixo)
    LPaint.Color := COR_BORDA_ESCURA;
    ACanvas.DrawLine(
      PointF(LInnerRect.Right, LInnerRect.Top),
      PointF(LInnerRect.Right, LInnerRect.Bottom),
      LPaint
    );
    ACanvas.DrawLine(
      PointF(LInnerRect.Left, LInnerRect.Bottom),
      PointF(LInnerRect.Right, LInnerRect.Bottom),
      LPaint
    );
  end else begin
    // Célula revelada: Fundo branco apenas no retângulo interno
    LPaint.Color := TAlphaColors.White;
    LPaint.Style := TSkPaintStyle.Fill;
    ACanvas.DrawRect(LInnerRect, LPaint); // <-- Desenha apenas a área interna

//    // Borda interna (opcional)
//    LPaint.Color := TAlphaColors.Black;
//    LPaint.Style := TSkPaintStyle.Stroke;
//    LPaint.StrokeWidth := 1;
//    ACanvas.DrawRect(
//      RectF(
//        LInnerRect.Left + 1,
//        LInnerRect.Top + 1,
//        LInnerRect.Right - 1,
//        LInnerRect.Bottom - 1
//      ),
//      LPaint
//    );
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

procedure TFormMain.SalvarJogo(const NomeArquivo: string);
var
  Stream: TFileStream;
  I, J: Integer;
  TempByte: Byte;
  TempoDecorrido: Integer;
  LevelInt: Integer;
begin
  Stream := TFileStream.Create(NomeArquivo, fmCreate);
  try
    // Gravar versão do formato (para controle)
    Stream.WriteBuffer(AnsiString('MSV1'), 4);

    // Gravar nível atual
    LevelInt := Ord(FGameLevel);
    Stream.WriteBuffer(LevelInt, SizeOf(LevelInt));

    // Gravar dimensões do campo
    Stream.WriteBuffer(FColunas, SizeOf(FColunas));
    Stream.WriteBuffer(FLinhas, SizeOf(FLinhas));
    Stream.WriteBuffer(NUM_MINAS, SizeOf(NUM_MINAS));

    // Gravar matrizes
    for I := 0 to FColunas - 1 do
      for J := 0 to FLinhas - 1 do
      begin
        TempByte := Ord(MinasArray[I][J]);
        Stream.WriteBuffer(TempByte, SizeOf(TempByte));
      end;

    for I := 0 to FColunas - 1 do
      for J := 0 to FLinhas - 1 do
      begin
        TempByte := Ord(CelulasReveladas[I][J]);
        Stream.WriteBuffer(TempByte, SizeOf(TempByte));
      end;

    for I := 0 to FColunas - 1 do
      for J := 0 to FLinhas - 1 do
      begin
        TempByte := Ord(CelulasMarcadas[I][J]);
        Stream.WriteBuffer(TempByte, SizeOf(TempByte));
      end;

    // Gravar estado do jogo
    Stream.WriteBuffer(FPrimeiroClique, SizeOf(FPrimeiroClique));
    Stream.WriteBuffer(FGameOver, SizeOf(FGameOver));
    Stream.WriteBuffer(MinasRestantes, SizeOf(MinasRestantes));

    // Gravar tempo decorrido
    if not FPrimeiroClique then
      TempoDecorrido := SecondsBetween(Now, FTempoInicio)
    else
      TempoDecorrido := 0;

    Stream.WriteBuffer(TempoDecorrido, SizeOf(TempoDecorrido));

  finally
    Stream.Free;
  end;
end;

procedure TFormMain.ScrollBoxGameViewportPositionChange(Sender: TObject;
  const OldViewportPosition, NewViewportPosition: TPointF;
  const ContentSizeChanged: Boolean);
begin
   MiniMapPaintBox.Redraw;
end;

procedure TFormMain.SetLevel(ALevel: TGameLevel; Acolumns, ALines, Amines: Integer);
begin

    FGameLevel := ALevel;
    case ALevel of
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
      end; // ~62 mines
    glAdvanced: begin
        FColunas := 30;
        FLinhas := 30;
        NUM_MINAS := 250;
      end;
    glCustom : begin
         FColunas := Acolumns;
        FLinhas := ALines;
        NUM_MINAS := Amines;
      end;
    end;

    // Adjust PaintBox size
    SkPaintBox.Width := FColunas * FCelulaSize; // Width based on columns
    SkPaintBox.Height := FLinhas * FCelulaSize; // Height based on lines


  //{$IFDEF MSWINDOWS}
  //  FormMain.Width := floor(RectFundo.Width + 40);
  //  FormMain.Height := floor(RectFundo.Height + Layout1.Height + 90);
  //{$ENDIF}
   SkPaintBox.Position.X:=20;
   SkPaintBox.Position.y:=20;

   RepositionSkPaintBox; // ⬅️ Novo código

   PreRenderCellTexture;

end;

procedure TFormMain.SetNrSeconds(const Value: Integer);
begin
  FNrSeconds := Value;
  skpnrseconds.Redraw;
end;

procedure TFormMain.SetNrMinas(const Value: Integer);
begin
  FNrMinas := Value;
  skpnrminas.Redraw;
end;

procedure TFormMain.FloatAnimationMenuFinish(Sender: TObject);
begin
if laymenu.Tag=1 then
Begin
  imgsetabtnmenu.RotationAngle:=180;
end
else
Begin
 imgsetabtnmenu.RotationAngle:=0;
end;

end;

procedure TFormMain.FloatAnimationStartFinish(Sender: TObject);
begin
if LayBeginGame.Position.y <0 then
  LayBeginGame.Visible:=False;
end;

procedure TFormMain.FloatAnimationzoomFinish(Sender: TObject);
begin
  if laymenuzoom.Tag=1 then
  Begin
    imgMenuZoom.RotationAngle:=180;
  end
  else
  Begin
   imgMenuZoom.RotationAngle:=0;
  end;
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
FRevealAnimations.Free;
FAnimations.Free;
end;

procedure TFormMain.MiniMapPaintBoxDraw(ASender: TObject; const ACanvas: ISkCanvas;
  const ADest: TRectF; const AOpacity: Single);
var
  I, J: Integer;
  CellWidth, CellHeight: Single;
  MiniCellRect, ViewportRect: TRectF;
  MinPaint: ISKpaint;
begin
  // Calcular escala
  CellWidth := MiniMapPaintBox.Width / FColunas;
  CellHeight := MiniMapPaintBox.Height / FLinhas;
  MinPaint:=Tskpaint.Create;
  MinPaint.AntiAlias:=true;
  // Desenhar todas as células
  for I := 0 to FColunas - 1 do
    for J := 0 to FLinhas - 1 do
    begin
      MiniCellRect := RectF(
        I * CellWidth,
        J * CellHeight,
        (I + 1) * CellWidth,
        (J + 1) * CellHeight
      );

      // Cores simplificadas
      if CelulasReveladas[I][J] then
      Begin
        minpaint.Color:= $FFFFFFFF; //white tranparent
        ACanvas.DrawRect(MiniCellRect,minpaint);
      End
      else if CelulasMarcadas[I][J] then
      Begin
        minpaint.Color:= $FFFF0000; //Red transparent;
        ACanvas.DrawRect(MiniCellRect, minpaint)
      End
      else
      Begin
        minpaint.Color:= $96838383; //Gray transparent;
        ACanvas.DrawRect(MiniCellRect, minpaint);
      End;
    end;

  // Desenhar área visível atual
  ViewportRect := RectF(
    (ScrollBoxGame.ViewportPosition.X / SkPaintBox.Width) * MiniMapPaintBox.Width,
    (ScrollBoxGame.ViewportPosition.Y / SkPaintBox.Height) * MiniMapPaintBox.Height,
    ((ScrollBoxGame.ViewportPosition.X + ScrollBoxGame.Width) / SkPaintBox.Width) * MiniMapPaintBox.Width,
    ((ScrollBoxGame.ViewportPosition.Y + ScrollBoxGame.Height) / SkPaintBox.Height) * MiniMapPaintBox.Height
  );
   minpaint.Color:= TAlphaColors.Red;
   minpaint.Style:= Tskpaintstyle.Stroke;
   minpaint.StrokeWidth:=1;
  ACanvas.DrawRect(ViewportRect,minpaint);
end;


function CarregarShaderRecurso(const NomeRecurso: string):String;
var ResourceStream: TResourceStream; Adata: TBytes;
begin
  // Creates a stream for the resource of type RCDATA
  ResourceStream := TResourceStream.Create(HInstance, NomeRecurso, RT_RCDATA);
  try
    // Read the SVG data
    SetLength(Adata, ResourceStream.Size);
    ResourceStream.ReadBuffer(Adata[0], ResourceStream.Size);

    // Convert to UTF-8 string and load into TSkSVG
    result := TEncoding.UTF8.GetString(Adata);
  finally ResourceStream.Free;
  end;
end;

procedure TFormMain.UpdateZoom;
begin
  SkPaintBox.Width := FColunas * FCelulaSize;
  SkPaintBox.Height := FLinhas * FCelulaSize;
  PreRenderCellTexture; // Atualiza textura das células
  RepositionSkPaintBox; // Reposiciona no ScrollBox
  SkPaintBox.Redraw;    // Força redesenho
end;

procedure TFormMain.FormGesture(
  Sender: TObject;
  const EventInfo: TGestureEventInfo;
  var Handled: Boolean
);
var
  LCurrentDistance: Integer;
  LZoomFactor: Single;
begin
  if EventInfo.GestureID = igiZoom then
  begin
    // Verifica se o evento é o início do gesto
    if TInteractiveGestureFlag.gfBegin in EventInfo.Flags then
    begin
      FLastZoomDistance := EventInfo.Distance; // Salva distância inicial
    end

    // Verifica se o evento é o fim do gesto
    else if TInteractiveGestureFlag.gfEnd in EventInfo.Flags then
    begin
      LCurrentDistance := EventInfo.Distance;
      if FLastZoomDistance <> 0 then // Evita divisão por zero
      begin
        LZoomFactor := LCurrentDistance / FLastZoomDistance;
        FCelulaSize := Round(EnsureRange(FCelulaSize * LZoomFactor, 20, 80));
        UpdateZoom;
        Handled := True;
      end;
      FLastZoomDistance := 0; // Reseta para próxima interação
    end;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
Var
  MinesCounter: string;
begin
FCelulaSize:=40;
   MinesCounter:=CarregarShaderRecurso('SKNUMBER');
   NSKeffect:= Tskruntimeeffect.makeforshader(MinesCounter,error);
   NSKShaderBuilder:=  TSkRuntimeShaderBuilder.Create(NSKeffect);
   NSKShader:=NSKshaderBuilder.MakeShader;

   NTKeffect:= Tskruntimeeffect.makeforshader(MinesCounter,error);
   NTKShaderBuilder:=  TSkRuntimeShaderBuilder.Create(NTKeffect);
   NTKShader:=NTKshaderBuilder.MakeShader;


  ScrollBoxGame.Touch.InteractiveGestures := [TInteractiveGesture.Pan];
  SetLevel(tgamelevel.glBeginner,0,0,0); // Initial level
  THighScoreManager.LoadHighScores;  //Charges all scores by levels

  ConfigurarAnimacoes;  //Configure initial animations
  NovoJogo;  //load initial game board

Touch.InteractiveGestures := [TInteractiveGesture.Pan, TInteractiveGesture.Zoom];
  Touch.GestureManager := GestureManager1;
end;

procedure TFormMain.FormResize(Sender: TObject);
begin
 RepositionSkPaintBox;
 MiniMapPaintBox.Redraw; // Atualiza o minimapa

 laymenu.Position.y:=-310;
 laymenu.Position.x:= rectbackground.Width -laymenu.Width -10;

 LayMenuZoom.Position.y:= rectbackground.Height/2;
 LayMenuZoom.Position.x:= (LayMenuZoom.Width-20)*-1;

if LayBeginGame.Visible then
Begin
   laybegingame.Width:=rectbackground.Width;
   laybegingame.Height:=rectbackground.Height;
End;

end;

procedure TFormMain.FormShow(Sender: TObject);
begin
RepositionSkPaintBox;
  //inicia animação Begin Game
  Tsoundmanager.PlaySound('venceu');
  BeginAnimationStart;
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

  NrMinas := NUM_MINAS;
  NrSeconds := 0;
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
  //FCelulaSize := 30; //Min(SkPaintBox.Width / FColunas, SkPaintBox.Height / FLinhas);

  // Drawing the bottom of the board
  APaint := TSkPaint.Create;
  APaint.Color := TAlphaColors.black;
//  ACanvas.DrawRect(RectF(0, 0, SkPaintBox.Width, SkPaintBox.Height), APaint);
  ACanvas.DrawRect(ADest, APaint); // <-- Isso cria o espaço entre as células

  for I := 0 to FColunas - 1 do
    for J := 0 to FLinhas - 1 do begin
      CelulaRect := RectF(I * FCelulaSize, J * FCelulaSize,
        (I + 1) * FCelulaSize, (J + 1) * FCelulaSize);

       // Usar cache da textura para células NÃO reveladas e NÃO marcadas
      if not CelulasReveladas[I][J] and not CelulasMarcadas[I][J] then begin
        ACanvas.DrawImage(FCachedCellTexture, CelulaRect.Left, CelulaRect.Top);
      end
      else begin
        // Draw cell with 3D effect
        DesenharCelula3D(ACanvas, CelulaRect, CelulasReveladas[I][J], CelulasMarcadas[I][J]);
      end;

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
        if not FAnimations.ContainsKey(LKey) then
          FAnimations.Add(LKey, 0);
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
            else
            Begin
              FExplosionAnimation.SeekFrame(FExplosionAnimation.FPS-1);
              FExplosionAnimation.Render(ACanvas, LDestRect);
//             FExplosionAnimation.SeekFrametime(FExplosionProgress *
//                FExplosionAnimation.Duration);
//              FExplosionAnimation.Render(ACanvas, LDestRect);
//             ACanvas.DrawImageRect(FBombStaticImage, LDestRect);
            End;
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

//  // Draw end-of-game message
//  if LayoutGameOver.Visible then begin
//    LPaint := TSkPaint.Create;
//    LFont := TSkFont.Create(TSkTypeFace.MakeFromName('Impact',
//      TSkFontStyle.Bold), 60);
//
//
//    LBaseColor := TAlphaColorF.Create(FGameEndColor);
//    LDarkerColor := TAlphaColorF.Create(EnsureRange(LBaseColor.R - 0.2, 0.0,
//      1.0), EnsureRange(LBaseColor.G - 0.2, 0.0, 1.0),
//      EnsureRange(LBaseColor.B - 0.2, 0.0, 1.0), LBaseColor.A).ToAlphaColor;
//
//    // Animated gradient effect
//    LPaint.Shader := TSkShader.MakeGradientLinear(PointF(0, 0),
//      PointF(SkPaintBox.Width, SkPaintBox.Height), [FGameEndColor, LDarkerColor,
//      FGameEndColor], [0, 0.5, 1], TSkTileMode.Mirror);
//
//    // Shadow effect
//    LPaint.ImageFilter := TSkImageFilter.MakeDropShadow(5, 5, 8, 8,
//      TAlphaColors.Black);
//
//    LTextWidth := LFont.MeasureText(FGameEndMessage);
//    LFont.GetMetrics(LMetrics);
//    LTextHeight := Abs(LMetrics.Ascent) + Abs(LMetrics.Descent);
//
//    // Entry animation
//    if FGameEndAnimationProgress < 1 then begin
//      FGameEndAnimationProgress := FGameEndAnimationProgress + 0.05;
//      LScale := TMatrix.CreateScaling(FGameEndAnimationProgress,
//        FGameEndAnimationProgress);
//      LTranslate := TMatrix.CreateTranslation
//        ((SkPaintBox.Width - (LTextWidth * FGameEndAnimationProgress)) / 2,
//        (SkPaintBox.Height - (LTextHeight * FGameEndAnimationProgress)) / 2);
//      ACanvas.Concat(LScale * LTranslate);
//    end;
//
//    LTextBlob := TSkTextBlob.MakeFromText(FGameEndMessage, LFont);
//    ACanvas.DrawTextBlob(LTextBlob, (SkPaintBox.Width - LTextWidth) / 2,
//      (SkPaintBox.Height / 2) + (LTextHeight / 2) -
//      Abs(LMetrics.Descent), LPaint);
//  end;
  MiniMapPaintBox.Redraw;
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

procedure TFormMain.CloseMenu;
Begin
   if laymenu.Tag=1 then
 Begin
 FloatAnimationMenu.StartValue:=4;
 FloatAnimationMenu.StopValue:= -310;
 FloatAnimationMenu.Start;
    laymenu.Tag:=0;
 End;
End;

procedure TFormMain.SkPaintBoxMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
 // var
 //   LScrollPos: TPointF;
begin
closemenu;
closemenuzoom;

  FIsScrolling := False;
  FLastMousePos := PointF(X, Y); // Guardar posição inicial

//  LScrollPos := PointF(ScrollBoxGame.ViewportPosition.X, ScrollBoxGame.ViewportPosition.Y);
  ClickInterval := 0;
  TimerLongtap.Enabled := True;
  //TPX :=  X+LScrollPos.X-20; // Ajustar coordenadas com o scroll
  //TPY :=  Y+LScrollPos.Y-20;
  TPX := X; // Stores current X coordinate
  TPY := Y; // Stores current Y coordinate
end;

procedure TFormMain.SkPaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Single);
var
  Delta: TPointF;
begin

  if (ssLeft in Shift) and (not FGameOver) then
  begin
    Delta := tPointF.Create(X - FLastMousePos.X, Y - FLastMousePos.Y);

    // Se o movimento for significativo, ativar rolagem
    if (Abs(Delta.X) > 5) or (Abs(Delta.Y) > 5) then
    begin
      FIsScrolling := True;
      TimerLongtap.Enabled := False;
      ScrollBoxGame.ViewportPosition := PointF(
        ScrollBoxGame.ViewportPosition.X - Delta.X,
        ScrollBoxGame.ViewportPosition.Y - Delta.Y
      );
    end;
  end;

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
    NrMinas := MinasRestantes;

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
if not FIsScrolling then
  begin
  if ClickInterval < LONG_TIME then begin
    TimerLongtap.Enabled := False;
    ClickInterval := 0;
    ManualTap(Sender, X, Y); // 'ListBox Short Touch..!!');
  end;
 end;
{$ENDIF}
{$IFDEF MSWINDOWS}
if not FIsScrolling then
  begin
    TPX := X + ScrollBoxGame.ViewportPosition.X;
    TPY := Y + ScrollBoxGame.ViewportPosition.Y;

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
        NrMinas := MinasRestantes;
        SkPaintBox.Redraw;
      end;
  end
  else
  if Button = TMouseButton.mbLeft then
     begin

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
  end;

{$ENDIF}
FIsScrolling := False;
end;

procedure TFormMain.SkpNrMinasDraw(ASender: TObject; const ACanvas: ISkCanvas;
  const ADest: TRectF; const AOpacity: Single);
Var
 Lpaint: ISKpaint;
begin
Lpaint:= Tskpaint.Create;
Lpaint.AntiAlias:=True;
DrawMines(Acanvas, Adest,Lpaint);

end;

procedure TFormMain.SkpNrSecondsDraw(ASender: TObject; const ACanvas: ISkCanvas;
  const ADest: TRectF; const AOpacity: Single);
Var
 Lpaint: ISKpaint;
begin
Lpaint:= Tskpaint.Create;
Lpaint.AntiAlias:=True;
Drawclock(Acanvas, Adest,Lpaint);
end;

procedure TFormMain.SkSvg3Click(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
 SkSvg3Tap(Sender,Tpointf.create(0,0));
{$ENDIF}
end;

procedure TFormMain.SkSvg3Tap(Sender: TObject; const Point: TPointF);
begin

FrmAbout:=TFrmAbout.create(nil);
{$ifdef ANDROID}
FrmAbout.WindowState:= Twindowstate.wsMaximized;
{$ENDIF}
FrmAbout.Show;
closemenu;
end;

procedure TFormMain.IMgLevelClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  IMgLevelTap(sender,Tpointf.create(0,0));
{$ENDIF}
end;

procedure TFormMain.IMgLevelTap(Sender: TObject; const Point: TPointF);
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
  closemenu;
end;

procedure TFormMain.ImgLoadGameClick(Sender: TObject);
begin
 {$IFDEF MSWINDOWS}
ImgLoadGameTap(Sender,TPointF.Create(0,0));
{$ENDIF}
end;

procedure TFormMain.ImgLoadGameTap(Sender: TObject; const Point: TPointF);
begin
 {$IFDEF MSWINDOWS}
  CarregarJogo('savegame.msv');
  {$ELSE}
  CarregarJogo(system.ioutils.TPath.Combine(system.ioutils.TPath.GetDocumentsPath, 'savegame.msv'));
  {$ENDIF}
  closemenu;
end;

procedure TFormMain.CloseMenuZoom;
Begin
   if laymenuzoom.Tag=1 then
 Begin
 FloatAnimationzoom.StartValue:=3;
 FloatAnimationzoom.StopValue:= ((laymenuzoom.Width -20)*-1);
 FloatAnimationzoom.Start;
    laymenuzoom.Tag:=0;
 End;
End;

procedure TFormMain.imgMenuZoomClick(Sender: TObject);
begin
 {$IFDEF MSWINDOWS}
  imgMenuZoomTap(Sender,TPointF.Create(0,0));
 {$ENDIF}
end;

procedure TFormMain.imgMenuZoomTap(Sender: TObject; const Point: TPointF);
begin
 if laymenuzoom.Tag=1 then
 Begin
   CloseMenuZoom;
 End
 else
 Begin
  FloatAnimationzoom.StartValue:=((laymenuzoom.Width-20)*-1);
  FloatAnimationzoom.StopValue:= 3;
  FloatAnimationzoom.Start;
  laymenuzoom.Tag:=1;
 End;
end;

procedure TFormMain.ImgSaveGameClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
  ImgSaveGameTap(Sender,TPointF.Create(0,0));
  {$ENDIF}
end;

procedure TFormMain.ImgSaveGameTap(Sender: TObject; const Point: TPointF);
begin
  {$IFDEF MSWINDOWS}
  SalvarJogo('savegame.msv');
  {$ELSE}
  SalvarJogo(system.ioutils.TPath.Combine(system.ioutils.TPath.GetDocumentsPath, 'savegame.msv'));
  {$ENDIF}
  closemenu;
end;

procedure TFormMain.imgsetabtnmenuClick(Sender: TObject);
begin

 if laymenu.Tag=1 then
 Begin
   closemenu;
 End
 else
 Begin
  FloatAnimationMenu.StartValue:=-310;
  FloatAnimationMenu.StopValue:= 4;
  FloatAnimationMenu.Start;
  laymenu.Tag:=1;
 End;
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
  SkPaintBox.InvalidateRect(RectF(X * FCelulaSize, Y * FCelulaSize, (X+1)*FCelulaSize, (Y+1)*FCelulaSize));
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
if FAnimations.Count = 0 then Exit;

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
  if not FGameOver then
    NrSeconds := SecondsBetween(Now, FTempoInicio)
  else // if FExplosionProgress >= 1 then
  begin
    Timer.Enabled := False;
    Tsoundmanager.PlaySound('gameover');
  end;
end;

procedure TFormMain.CarregarJogo(const NomeArquivo: string);
var
  Stream: TFileStream;
  I, J: Integer;
  TempByte: Byte;
  FileVersion: array[0..3] of AnsiChar;
  TempoDecorrido: Integer;
  LevelInt: Integer;
begin
  Stream := TFileStream.Create(NomeArquivo, fmOpenRead);
  try
    // Verificar versão do arquivo
    Stream.ReadBuffer(FileVersion, 4);
    if FileVersion <> 'MSV1' then
      raise Exception.Create('Formato de arquivo inválido');

    Stream.ReadBuffer(LevelInt, SizeOf(LevelInt));
    FGameLevel := TGameLevel(LevelInt);

    // Ler dimensões do campo
    Stream.ReadBuffer(FColunas, SizeOf(FColunas));
    Stream.ReadBuffer(FLinhas, SizeOf(FLinhas));
    Stream.ReadBuffer(NUM_MINAS, SizeOf(NUM_MINAS));

    if FGameLevel = glCustom then
      SetLevel(FGameLevel, FColunas, FLinhas, NUM_MINAS)
    else
      SetLevel(FGameLevel, 0, 0, 0); // Níveis padrão usam configurações internas

    // Reinicializar arrays com novas dimensões
    SetLength(MinasArray, FColunas, FLinhas);
    SetLength(CelulasReveladas, FColunas, FLinhas);
    SetLength(CelulasMarcadas, FColunas, FLinhas);

    // Ler matrizes
    for I := 0 to FColunas - 1 do
      for J := 0 to FLinhas - 1 do
      begin
        Stream.ReadBuffer(TempByte, SizeOf(TempByte));
        MinasArray[I][J] := Boolean(TempByte);
      end;

    for I := 0 to FColunas - 1 do
      for J := 0 to FLinhas - 1 do
      begin
        Stream.ReadBuffer(TempByte, SizeOf(TempByte));
        CelulasReveladas[I][J] := Boolean(TempByte);
      end;

    for I := 0 to FColunas - 1 do
      for J := 0 to FLinhas - 1 do
      begin
        Stream.ReadBuffer(TempByte, SizeOf(TempByte));
        CelulasMarcadas[I][J] := Boolean(TempByte);
      end;

    // Ler estado do jogo
    Stream.ReadBuffer(FPrimeiroClique, SizeOf(FPrimeiroClique));
    Stream.ReadBuffer(FGameOver, SizeOf(FGameOver));
    Stream.ReadBuffer(MinasRestantes, SizeOf(MinasRestantes));

     FAnimations.Clear; // Limpar animações existentes

       // Restaurar animações das bandeiras
    for I := 0 to FColunas - 1 do
      for J := 0 to FLinhas - 1 do
      begin
        if CelulasMarcadas[I][J] then
        begin
          FAnimations.Add(Point(I, J), 0); // Iniciar progresso da animação
        end;
      end;

    // Garantir que o timer de animação está ativo
    FAnimationTimer.Enabled := True;

    // Configurar tempo
    Stream.ReadBuffer(TempoDecorrido, SizeOf(TempoDecorrido));
    if not FPrimeiroClique then
      FTempoInicio := IncSecond(Now, -TempoDecorrido)
    else
      FTempoInicio := Now;

    // Atualizar interface
    NrMinas := MinasRestantes;
    NrSeconds := TempoDecorrido;
    Timer.Enabled := not FGameOver and not FPrimeiroClique;

    // Redesenhar o jogo
    SkPaintBox.Width := FColunas * FCelulaSize;
    SkPaintBox.Height := FLinhas * FCelulaSize;
    RepositionSkPaintBox;
    SkPaintBox.Redraw;

  finally
    Stream.Free;
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
    FGameEndMessage := 'YOU WIN !';
    FGameEndColor := $FF44FF44;
    FGameEndAnimationProgress := 0;

    sklabelgameover.Words[0].Text:= FGameEndMessage;
    sklabelgameover.TextSettings.FontColor:= FGameEndColor;
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
  FGameEndMessage := 'YOU LOOSE !';
  FGameEndColor := $FFFF4444;
  FGameEndAnimationProgress := 0;
  sklabelgameover.Words[0].Text:= FGameEndMessage;
  sklabelgameover.TextSettings.FontColor:= FGameEndColor;
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
  closemenu;
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
  closemenu;
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
 ScrollBoxGame.InvalidateContentSize;
end;


procedure TFormMain.BeginAnimationStart;
begin

laybegingame.Width:=rectbackground.Width;
laybegingame.Height:=rectbackground.Height;
laybegingame.Position.X:=0;
laybegingame.Position.Y:=rectbackground.Height*-1;
laybegingame.Visible:=True;

Floatanimationstart.StartValue:=rectbackground.Height*-1;
Floatanimationstart.StopValue:=0;
Floatanimationstart.Start;
end;

procedure TFormMain.CloseAnimationStart;
begin
laybegingame.Height:=rectbackground.Height;
laybegingame.Position.X:=0;
laybegingame.Position.Y:=rectbackground.Height*-1;

 Floatanimationstart.StartValue:=0;
 Floatanimationstart.StopValue:=FormMain.Height*-1;
 Floatanimationstart.Start;
end;

procedure TFormMain.imgbuttonstartClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
imgbuttonstartTap(Sender,TPointF.create(0,0));
{$ENDIF}
end;

procedure TFormMain.imgbuttonstartTap(Sender: TObject; const Point: TPointF);
begin
Tsoundmanager.PlaySound('click');
 CloseAnimationStart;
end;

procedure TFormMain.imgcelllargerClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
imgcelllargerTap(sender,TPointf.create(0,0));
{$ENDIF}
end;

procedure TFormMain.imgcelllargerTap(Sender: TObject; const Point: TPointF);
begin

 if FCelulaSize < 80 then  // Limite máximo
  begin
    FCelulaSize := FCelulaSize + 5;
    UpdateZoom;
  end;

end;

procedure TFormMain.imgcellsmallClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
imgcellsmallTap(sender,TPointf.create(0,0));
{$ENDIF}
end;

procedure TFormMain.imgcellsmallTap(Sender: TObject; const Point: TPointF);
begin
 if FCelulaSize > 25 then  // Limite mínimo
  begin
    FCelulaSize := FCelulaSize - 5;
    UpdateZoom;
  end;
end;

procedure TFormMain.btnnewLevelClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
 IMgLevelTap(sender,TPointf.create(0,0));
{$ENDIF}
end;

procedure TFormMain.btnnewLevelTap(Sender: TObject; const Point: TPointF);
begin
  IMgLevelTap(sender,Point);
end;

procedure TFormMain.btnrepeateClick(Sender: TObject);
begin
{$IFDEF MSWINDOWS}
btnrepeateTap(self,Tpointf.Create(0,0));
{$ENDIF}
end;

procedure TFormMain.btnrepeateTap(Sender: TObject; const Point: TPointF);
begin
novojogo;
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
