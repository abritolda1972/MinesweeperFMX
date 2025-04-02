unit SevenSegmentDisplay;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Math,
  FMX.Types, FMX.Controls, FMX.Graphics, FMX.skia, System.Skia, System.DateUtils;

type
  TSevenSegmentDisplay = class(TSkPaintBox)
  private
    FDigitPaths: array[0..2, 0..6] of ISkPath;
    FValue: Integer;
    FTimer: TTimer;
    FSegmentColor: TAlphaColor;
    FInactiveColor: TAlphaColor;
    procedure SetValue(const AValue: Integer);
    procedure UpdateSegments;
    function CreateSegmentPath(const ADigitRect: TRectF; ASegment: Integer): ISkPath;
    procedure TimerTick(Sender: TObject);
    procedure SetAutoIncrement(const Value: Boolean);
    function GetAutoIncrement: Boolean;
    function GetInterval: Integer;
    procedure SetInterval(const Value: Integer);
    procedure SetSegmentColor(const Value: TAlphaColor);
    procedure SetInactiveColor(const Value: TAlphaColor);
  protected
    procedure Draw(const ACanvas: ISkCanvas; const ADest: TRectF; const AOpacity: Single); override;
    procedure Resize; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Value: Integer read FValue write SetValue default 0;
    property AutoIncrement: Boolean read GetAutoIncrement write SetAutoIncrement default False;
    property Interval: Integer read GetInterval write SetInterval default 1000;
    property SegmentColor: TAlphaColor read FSegmentColor write SetSegmentColor default TAlphaColorRec.Red;
    property InactiveColor: TAlphaColor read FInactiveColor write SetInactiveColor default TAlphaColorRec.Darkgray;
  end;

const
  SegmentsAtivos: array[0..9] of set of 0..6 = (
    [0, 5, 4, 3, 2, 1],   // 0
    [1, 2],                 // 1
    [0, 1, 3, 4, 6],       // 2
    [0, 1, 2, 3, 6],       // 3
    [5, 6, 1, 2],          // 4
    [0, 2, 3, 5, 6],       // 5
    [0, 2, 3, 4, 5, 6],    // 6
    [0, 1, 2],              // 7
    [0, 1, 2, 3, 4, 5, 6], // 8
    [0, 1, 2, 3, 5, 6]     // 9
  );

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ABSkia', [TSevenSegmentDisplay]);
end;

{ TSevenSegmentDisplay }

constructor TSevenSegmentDisplay.Create(AOwner: TComponent);
begin
  inherited;
  FSegmentColor := TAlphaColorRec.Red;
  FInactiveColor := $FF1C1C1D;
  FValue := 0;

  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 1000;
  FTimer.OnTimer := TimerTick;

  UpdateSegments;
end;

procedure TSevenSegmentDisplay.Draw(const ACanvas: ISkCanvas; const ADest: TRectF;
  const AOpacity: Single);
var
  i, j: Integer;
  LPaint: ISkPaint;
  LDigits: array[0..2] of Integer;
begin
  inherited;

  LDigits[0] := (FValue div 100) mod 10;
  LDigits[1] := (FValue div 10) mod 10;
  LDigits[2] := FValue mod 10;

  LPaint := TSkPaint.Create(TSkPaintStyle.Fill);
  LPaint.AntiAlias := True;
  LPaint.Color := FInactiveColor;
  for i := 0 to 2 do
    for j := 0 to 6 do
      ACanvas.DrawPath(FDigitPaths[i, j], LPaint);

  LPaint.Color := FSegmentColor;
  for i := 0 to 2 do
    for j := 0 to 6 do
      if j in SegmentsAtivos[LDigits[i]] then
        ACanvas.DrawPath(FDigitPaths[i, j], LPaint);
end;

function TSevenSegmentDisplay.GetAutoIncrement: Boolean;
begin
  Result := FTimer.Enabled;
end;

function TSevenSegmentDisplay.GetInterval: Integer;
begin
  Result := FTimer.Interval;
end;

procedure TSevenSegmentDisplay.Resize;
begin
  inherited;
  UpdateSegments;
end;

procedure TSevenSegmentDisplay.SetAutoIncrement(const Value: Boolean);
begin
  FTimer.Enabled := Value;
end;

procedure TSevenSegmentDisplay.SetInactiveColor(const Value: TAlphaColor);
begin
  if FInactiveColor <> Value then
  begin
    FInactiveColor := Value;
    Redraw;
  end;
end;

procedure TSevenSegmentDisplay.SetInterval(const Value: Integer);
begin
  FTimer.Interval := Value;
end;

procedure TSevenSegmentDisplay.SetSegmentColor(const Value: TAlphaColor);
begin
  if FSegmentColor <> Value then
  begin
    FSegmentColor := Value;
    Redraw;
  end;
end;

procedure TSevenSegmentDisplay.SetValue(const AValue: Integer);
begin
  if FValue <> AValue then
  begin
    FValue := EnsureRange(AValue, 0, 999);
    Redraw;
  end;
end;

procedure TSevenSegmentDisplay.TimerTick(Sender: TObject);
begin
  Value := (Value + 1) mod 1000;
end;

procedure TSevenSegmentDisplay.UpdateSegments;
var
  i, j: Integer;
  LDigitWidth, LDigitHeight: Single;
  LDigitRect: TRectF;
begin
  if (Width <= 0) or (Height <= 0) then
    Exit;

  LDigitWidth := Width / 3;
  LDigitHeight := Height;

  for i := 0 to 2 do
  begin
    LDigitRect := RectF(
      Width - (3 - i) * LDigitWidth,
      0,
      Width - (2 - i) * LDigitWidth,
      LDigitHeight
    );

    LDigitRect.Inflate(-LDigitWidth * 0.1, -LDigitHeight * 0.1);

    for j := 0 to 6 do
      FDigitPaths[i, j] := CreateSegmentPath(LDigitRect, j);
  end;
end;

function TSevenSegmentDisplay.CreateSegmentPath(const ADigitRect: TRectF;
  ASegment: Integer): ISkPath;
var
  LBuilder: ISkPathBuilder;
  LHeight, Thickness, Taper: Single;
  CenterY, VerticalPos: Single;
  VerticalOffset: Integer;
begin
  LHeight := ADigitRect.Height;
  Thickness := LHeight * 0.12;
  Taper := Thickness * 0.5;
  VerticalOffset := 2;
  CenterY := ADigitRect.CenterPoint.Y;

  LBuilder := TSkPathBuilder.Create;
  case ASegment of
    0:
      LBuilder.AddPolygon([
        PointF(ADigitRect.Left + Taper + Thickness/2, ADigitRect.Top - VerticalOffset),
        PointF(ADigitRect.Right - Taper - Thickness/2, ADigitRect.Top - VerticalOffset),
        PointF(ADigitRect.Right + Taper/4 - Thickness/2, (ADigitRect.Top + Thickness/2) - VerticalOffset),
        PointF(ADigitRect.Right - Taper - Thickness/2, (ADigitRect.Top + Thickness) - VerticalOffset),
        PointF(ADigitRect.Left + Taper + Thickness/2, (ADigitRect.Top + Thickness) - VerticalOffset),
        PointF(ADigitRect.Left - Taper/4 + Thickness/2, (ADigitRect.Top + Thickness/2) - VerticalOffset)
      ], True);
    1:
      LBuilder.AddPolygon([
        PointF(ADigitRect.Right, ADigitRect.Top + Taper + Thickness/2),
        PointF(ADigitRect.Right - Thickness/2, ADigitRect.Top - Taper/4 + Thickness/2),
        PointF(ADigitRect.Right - Thickness, ADigitRect.Top + Taper + Thickness/2),
        PointF(ADigitRect.Right - Thickness, ADigitRect.Top + LHeight/2 - Taper - Thickness/2),
        PointF(ADigitRect.Right - Thickness/2, ADigitRect.Top + LHeight/2 + Taper/4 - Thickness/2),
        PointF(ADigitRect.Right, ADigitRect.Top + LHeight/2 - Taper - Thickness/2)
      ], True);
    2:
      LBuilder.AddPolygon([
        PointF(ADigitRect.Right, ADigitRect.Bottom - Taper - Thickness/2),
        PointF(ADigitRect.Right - Thickness/2, ADigitRect.Bottom + Taper/4 - Thickness/2),
        PointF(ADigitRect.Right - Thickness, ADigitRect.Bottom - Taper - Thickness/2),
        PointF(ADigitRect.Right - Thickness, ADigitRect.Bottom - LHeight/2 + Taper + Thickness/2),
        PointF(ADigitRect.Right - Thickness/2, ADigitRect.Bottom - LHeight/2 - Taper/4 + Thickness/2),
        PointF(ADigitRect.Right, ADigitRect.Bottom - LHeight/2 + Taper + Thickness/2)
      ], True);
    3:
      LBuilder.AddPolygon([
        PointF(ADigitRect.Left + Taper + Thickness/2, ADigitRect.Bottom + VerticalOffset),
        PointF(ADigitRect.Right - Taper - Thickness/2, ADigitRect.Bottom + VerticalOffset),
        PointF(ADigitRect.Right + Taper/4 - Thickness/2, (ADigitRect.Bottom - Thickness/2) + VerticalOffset),
        PointF(ADigitRect.Right - Taper - Thickness/2, (ADigitRect.Bottom - Thickness) + VerticalOffset),
        PointF(ADigitRect.Left + Taper + Thickness/2, (ADigitRect.Bottom - Thickness) + VerticalOffset),
        PointF(ADigitRect.Left - Taper/4 + Thickness/2, (ADigitRect.Bottom - Thickness/2) + VerticalOffset)
      ], True);
    4:
      LBuilder.AddPolygon([
        PointF(ADigitRect.Left, ADigitRect.Bottom - Taper - Thickness/2),
        PointF(ADigitRect.Left + Thickness/2, ADigitRect.Bottom + Taper/4 - Thickness/2),
        PointF(ADigitRect.Left + Thickness, ADigitRect.Bottom - Taper - Thickness/2),
        PointF(ADigitRect.Left + Thickness, ADigitRect.Bottom - LHeight/2 + Taper + Thickness/2),
        PointF(ADigitRect.Left + Thickness/2, ADigitRect.Bottom - LHeight/2 - Taper/4 + Thickness/2),
        PointF(ADigitRect.Left, ADigitRect.Bottom - LHeight/2 + Taper + Thickness/2)
      ], True);
    5:
      LBuilder.AddPolygon([
        PointF(ADigitRect.Left, ADigitRect.Top + Taper + Thickness/2),
        PointF(ADigitRect.Left + Thickness/2, ADigitRect.Top - Taper/4 + Thickness/2),
        PointF(ADigitRect.Left + Thickness, ADigitRect.Top + Taper + Thickness/2),
        PointF(ADigitRect.Left + Thickness, ADigitRect.Top + LHeight/2 - Taper - Thickness/2),
        PointF(ADigitRect.Left + Thickness/2, ADigitRect.Top + LHeight/2 + Taper/4 - Thickness/2),
        PointF(ADigitRect.Left, ADigitRect.Top + LHeight/2 - Taper - Thickness/2)
      ], True);
    6:
      begin
        VerticalPos := CenterY - Thickness/2;
        LBuilder.AddPolygon([
          PointF(ADigitRect.Left + Taper + Thickness/2, VerticalPos),
          PointF(ADigitRect.Right - Taper - Thickness/2, VerticalPos),
          PointF(ADigitRect.Right + Taper/4 - Thickness/2, VerticalPos + Thickness/2),
          PointF(ADigitRect.Right - Taper - Thickness/2, VerticalPos + Thickness),
          PointF(ADigitRect.Left + Taper + Thickness/2, VerticalPos + Thickness),
          PointF(ADigitRect.Left - Taper/4 + Thickness/2, VerticalPos + Thickness/2)
        ], True);
      end;
  end;
  Result := LBuilder.Detach;
end;

end.
