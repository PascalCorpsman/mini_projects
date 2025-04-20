(******************************************************************************)
(* uLinedit.pas                                                    15.04.2025 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Visual component to edit a function with multiple base points*)
(*                                                                            *)
(* License     : See the file license.md, located under:                      *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(* Warranty    : There is no warranty, neither in correctness of the          *)
(*               implementation, nor anything other that could happen         *)
(*               or go wrong, use at your own risk.                           *)
(*                                                                            *)
(* Known Issues: none                                                         *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*                                                                            *)
(******************************************************************************)
Unit uLineEdit;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Controls, Graphics;

Type

  TPercentPoint = Record
    X, Y: Single; // in % [0..100]
  End;

  { TLineEdit }

  TLineEdit = Class(TGraphicControl)
  private
    FBrushColor: TColor;
    fGridColor: TColor;
    fGridPercent: Byte;
    FPointBoxColor: TColor;
    FPointBoxSelectedColor: TColor;
    FLineColor: TColor;
    fOnPointsChange: TNotifyEvent;
    FPenColor: TColor;
    fPoints: Array Of TPercentPoint;
    Function getPoint(index: integer): TPercentPoint;
    Function getPointCount: Integer;
    Procedure setBrushColor(AValue: TColor);
    Procedure setGridColor(AValue: TColor);
    Procedure setGridPercent(AValue: Byte);
    Procedure setLineBoxColor(AValue: TColor);
    Procedure setLineBoxSelectedColor(AValue: TColor);
    Procedure setLineColor(AValue: TColor);
    Procedure setPenColor(AValue: TColor);
  protected
    fSelectedPoint: integer;
    Procedure Paint; override;
    Procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    Procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    Procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    Property PointCount: Integer read getPointCount;
    Property Point[aindex: integer]: TPercentPoint read getPoint;

    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy(); override;

    Procedure Reset();
    Function AddPoint(aPoint: TPercentPoint): integer; // -1 = Fehler, sonst Index des neuen Punktes
    Function DelPoint(aIndex: integer): Boolean;
    Function SetPointXValue(aIndex: integer; XValue: Single): Boolean;
    Function SetPointYValue(aIndex: integer; YValue: Single): Boolean;

    (*
     * Berechnet f(x) für die durch die Liniensegmente dargestellte funktion
     *)
    Function F(x: Single): Single; // x in [0..100] -> [0..100]
  published
    Property BrushColor: TColor read FBrushColor write setBrushColor;
    Property PenColor: TColor read FPenColor write setPenColor;
    Property GridColor: TColor read fGridColor write setGridColor;
    Property GridPercent: Byte read fGridPercent write setGridPercent; // 0 = disabled
    Property LineColor: TColor read FLineColor write setLineColor;
    Property PointBoxColor: TColor read FPointBoxColor write setLineBoxColor;
    Property PointBoxSelectedColor: TColor read FPointBoxSelectedColor write setLineBoxSelectedColor;
    Property OnPointsChange: TNotifyEvent read fOnPointsChange write fOnPointsChange;
  End;

Function PercentPoint(x, y: Single): TPercentPoint;

Implementation

Uses math;

Procedure Nop(); // Debug only
Begin

End;

Const
  BoxWidth = 5;

Function PercentPoint(x, y: Single): TPercentPoint;
Begin
  result.x := x;
  result.y := y;
End;

{ TLineEdit }

Constructor TLineEdit.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  fOnPointsChange := Nil;
  FBrushColor := clWhite;
  FPenColor := clBlack;
  fGridColor := clBlack;
  FLineColor := clBlack;
  fPointBoxColor := clBlack;
  FPointBoxSelectedColor := clred;
  fGridPercent := 25;
  fPoints := Nil;
  Reset();
End;

Destructor TLineEdit.Destroy;
Begin
  Inherited Destroy();
End;

Procedure TLineEdit.Reset;
Begin
  setlength(fPoints, 2);
  fPoints[0] := PercentPoint(0, 0);
  fPoints[1] := PercentPoint(100, 100);
  fSelectedPoint := -1;
  Invalidate;
End;

Function TLineEdit.AddPoint(aPoint: TPercentPoint): Integer;
Var
  i, insertIndex: Integer;
Begin
  Result := -1;
  If (aPoint.X <= 0) Or (aPoint.X >= 100) Or
    (aPoint.Y < 0) Or (aPoint.Y > 100) Then Exit;

  // 1. Suchen des "Segmentes" zwischem dem der Potentielle neue Punkt erstellt werden soll
  insertIndex := Length(fPoints);
  For i := 0 To High(fPoints) Do Begin
    If aPoint.X < fPoints[i].X Then Begin
      insertIndex := i;
      Break;
    End;
  End;

  // 2. Erstellen des neuen Punktes
  SetLength(fPoints, high(fPoints) + 2);
  For i := High(fPoints) Downto insertIndex + 1 Do
    fPoints[i] := fPoints[i - 1];

  fPoints[insertIndex] := aPoint;

  Invalidate;
  // If assigned(fOnPointsChange) Then fOnPointsChange(self); -- Rein oder Raus ?, wenn rein muss es an anderen Stellen wieder gelöscht werden !
  Result := insertIndex;
End;

Function TLineEdit.DelPoint(aIndex: integer): Boolean;
Var
  i: Integer;
Begin
  result := false;
  If (aIndex <= 0) Or (aIndex >= High(fPoints)) Then exit; // der 1. und der letzte dürfen nicht gelöscht werden
  For i := fSelectedPoint To high(fPoints) - 1 Do Begin
    fPoints[i] := fPoints[i + 1];
  End;
  SetLength(fPoints, high(fPoints));
  Invalidate;
  // If assigned(fOnPointsChange) Then fOnPointsChange(self); -- Rein oder Raus ?, wenn rein muss es an anderen Stellen wieder gelöscht werden !
  result := true;
End;

Function TLineEdit.SetPointXValue(aIndex: integer; XValue: Single): Boolean;
Begin
  result := false;
  If (aIndex <= 0) Or (aIndex >= High(fPoints)) Then exit;
  If (XValue < 0) Or (XValue > 100) Then exit;
  If fPoints[aIndex - 1].X >= XValue Then exit;
  If fPoints[aIndex + 1].X <= XValue Then exit;
  fPoints[aIndex].x := XValue;
  Invalidate;
  // If assigned(fOnPointsChange) Then fOnPointsChange(self); -- Rein oder Raus ?, wenn rein muss es an anderen Stellen wieder gelöscht werden !
  result := true;
End;

Function TLineEdit.SetPointYValue(aIndex: integer; YValue: Single): Boolean;
Begin
  result := false;
  If (aIndex < 0) Or (aIndex > High(fPoints)) Then exit;
  fPoints[aIndex].Y := min(100, max(0, YValue));
  Invalidate;
  // If assigned(fOnPointsChange) Then fOnPointsChange(self); -- Rein oder Raus ?, wenn rein muss es an anderen Stellen wieder gelöscht werden !
  result := true;
End;

Function TLineEdit.F(x: Single): Single;
// This code was created using ChatGTP, and it worked on the first try (y)
Var
  i: Integer;
  x0, y0, x1, y1, t: Single;
Begin
  // Find the interval that contains x
  For i := 0 To high(fPoints) - 1 Do Begin
    If (x >= fPoints[i].x) And (x <= fPoints[i + 1].x) Then Begin
      x0 := fPoints[i].x;
      y0 := fPoints[i].y;
      x1 := fPoints[i + 1].x;
      y1 := fPoints[i + 1].y;
      // Linear interpolation
      t := (x - x0) / (x1 - x0);
      Result := y0 + t * (y1 - y0);
      Exit;
    End;
  End;
End;

Procedure TLineEdit.setBrushColor(AValue: TColor);
Begin
  If FBrushColor = AValue Then Exit;
  FBrushColor := AValue;
  Invalidate;
End;

Function TLineEdit.getPointCount: Integer;
Begin
  result := length(fPoints);
End;

Function TLineEdit.getPoint(index: integer): TPercentPoint;
Begin
  If (index < 0) Or (index > high(fPoints)) Then Raise exception.create('Error, out of bounds.');
  result := fPoints[index];
End;

Procedure TLineEdit.setGridColor(AValue: TColor);
Begin
  If fGridColor = AValue Then Exit;
  fGridColor := AValue;
  Invalidate;
End;

Procedure TLineEdit.setGridPercent(AValue: Byte);
Begin
  If fGridPercent = AValue Then Exit;
  fGridPercent := AValue;
  Invalidate;
End;

Procedure TLineEdit.setLineBoxColor(AValue: TColor);
Begin
  If FPointBoxColor = AValue Then Exit;
  FPointBoxColor := AValue;
  Invalidate;
End;

Procedure TLineEdit.setLineBoxSelectedColor(AValue: TColor);
Begin
  If FPointBoxSelectedColor = AValue Then Exit;
  FPointBoxSelectedColor := AValue;
  Invalidate;
End;

Procedure TLineEdit.setLineColor(AValue: TColor);
Begin
  If FLineColor = AValue Then Exit;
  FLineColor := AValue;
  Invalidate;
End;

Procedure TLineEdit.setPenColor(AValue: TColor);
Begin
  If FPenColor = AValue Then Exit;
  FPenColor := AValue;
  Invalidate;
End;

Procedure TLineEdit.Paint;

  Procedure PaintBox(Const P: TPercentPoint);
  Var
    w, x, y: integer;
  Begin
    w := Scale96ToForm(BoxWidth);
    x := round(Width * p.X / 100);
    y := round(Height * p.y / 100);
    canvas.Rectangle(x - w, height - y - w, x + w, height - y + w);
  End;

Var
  deltaX, deltaY: Single;
  x, y, i: Integer;
Begin
  Inherited Paint;
  // Alles Löschen
  canvas.Brush.Color := FBrushColor;
  canvas.Pen.Color := FPenColor;
  canvas.Rectangle(0, 0, Width, Height);
  // Grid
  If fGridPercent <> 0 Then Begin
    canvas.Pen.Color := fGridColor;
    deltaX := width / (100 / fGridPercent);
    deltaY := Height / (100 / fGridPercent);
    x := 1;
    While x * deltaX < Width Do Begin
      canvas.Line(
        round(x * deltaX), 1,
        round(x * deltaX), height - 1
        );
      inc(x);
    End;
    y := 1;
    While y * deltay < height Do Begin
      canvas.Line(
        1, round(y * deltay),
        width - 1, round(y * deltay)
        );
      inc(y);
    End;
  End;
  // Line's
  canvas.Pen.Color := FLineColor;
  For i := 0 To high(fPoints) - 1 Do Begin
    Canvas.Line(
      round(fPoints[i].X * Width / 100),
      height - round(fPoints[i].Y * Height / 100),
      round(fPoints[i + 1].X * Width / 100),
      height - round(fPoints[i + 1].Y * Height / 100)
      );
  End;
  // Die Punkte der Lines zum "ziehen"
  For i := 0 To high(fPoints) Do Begin
    If i = fSelectedPoint Then Begin
      canvas.Pen.Color := FPointBoxSelectedColor;
    End
    Else Begin
      canvas.Pen.Color := FPointBoxColor;
    End;
    PaintBox(fPoints[i]);
  End;
End;

Procedure TLineEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
Var
  w: integer;
  Function collide(Const P: TPercentPoint): Boolean;
  Var
    xx, yy: integer;
  Begin
    xx := round(Width * p.X / 100);
    yy := round(Height * p.y / 100);
    result :=
      (xx - w <= x) And
      (height - yy - w <= y) And
      (xx + w >= x) And
      (height - yy + w >= y);
  End;
Var
  i: Integer;
  p: TPercentPoint;
Begin
  Inherited MouseDown(Button, Shift, X, Y);
  // Suche nach einen aus zu wählenden Punkt
  fSelectedPoint := -1;
  w := Scale96ToForm(BoxWidth);
  For i := 0 To high(fPoints) Do Begin
    If collide(fPoints[i]) Then Begin
      fSelectedPoint := i;
      break;
    End;
  End;
  If ssleft In shift Then Begin
    // Hinzufügen eines neuen Punktes
    If fSelectedPoint = -1 Then Begin
      p.x := x * 100 / Width;
      p.Y := (height - y) * 100 / height;
      fSelectedPoint := AddPoint(p);
      If assigned(fOnPointsChange) Then fOnPointsChange(self);
    End;
    // Hinzufügen und "auswählen" eines neuen Punktes
    Invalidate;
  End;
  // Löschen eines Punktes
  If (ssright In shift) And (fSelectedPoint <> -1) Then Begin
    If DelPoint(fSelectedPoint) Then Begin
      fSelectedPoint := -1;
      If assigned(fOnPointsChange) Then fOnPointsChange(self);
    End;
  End;
End;

Procedure TLineEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
Var
  dy, dx: Single;
  op: TPercentPoint;
Begin
  Inherited MouseMove(Shift, X, Y);
  If (fSelectedPoint <> -1) And (ssLeft In shift) Then Begin
    op := fPoints[fSelectedPoint];
    fPoints[fSelectedPoint].Y := min(100, max(0, (height - y) * 100 / height));
    dx := 100 / Width;
    dy := 100 / Height;
    // Der erste und Letzte Punkt sind immer "FIX"
    If (fSelectedPoint <> 0) And (fSelectedPoint <> high(fPoints)) Then Begin
      // Der Punkt wird "X"-Wert Technisch immer zwischen seinem Vorgänger und Nachfolger eingesperrt, exclusive !
      fPoints[fSelectedPoint].x := min(fPoints[fSelectedPoint + 1].x - dx, max(fPoints[fSelectedPoint - 1].x + dx, (x * 100) / Width));
    End;
    // Nur "refreshen" wenn sich auch was geändert hat..
    If (abs(fPoints[fSelectedPoint].x - op.x) >= dx) Or (abs(fPoints[fSelectedPoint].Y - op.Y) >= dy) Then Begin
      If assigned(fOnPointsChange) Then fOnPointsChange(self);
      Invalidate;
    End;
  End;
End;

Procedure TLineEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
Begin
  Inherited MouseUp(Button, Shift, X, Y);
  fSelectedPoint := -1;
  Invalidate
End;

End.

