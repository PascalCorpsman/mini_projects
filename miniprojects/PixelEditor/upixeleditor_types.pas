Unit upixeleditor_types;

{$MODE ObjFPC}{$H+}
{$MODESWITCH nestedprocvars}

Interface

Uses
  Classes, SysUtils, ExtCtrls, upixeleditorlcl, ugraphics;

Const

  (*
   * Es folgt the "Tiefe" der verschiedenen Render Ebenen [-0.9 .. 0.9]
   *
   * Jede Ebene sollte sich zur nächst höheren / tieferen um mindestens 0.01 unterscheiden !
   *)
  LayerBackGroundColor = -0.91;
  LayerBackGroundGrid = -0.9; // Das Grid das hinter allem sein soll und nur bei Transparenten Pixeln zu sehen ist
  LayerImage = -0.8; // Die Eigentliche vom User erstellte Textur
  LayerForeGroundGrid = -0.05;
  LayerCursor = -0.02;
  LayerFormColor = -0.01;
  LayerLCL = 0.0;

  ZoomLevels: Array Of integer = (
    100, 500,
    1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000,
    5500, 6000, 6500, 7000, 7500, 8000, 8500, 9000, 9500,
    10000); // in %

  (*
   * Die Koordinaten des Image Edit bereichs in Absoluten unscallierten Fenster Koordinaten
   *)
  WindowLeft = 75;
  WindowTop = 38;
  WindowRight = 636;
  WindowBottom = 424;
  ScreenWidth = 640;
  ScreenHeight = 480;

Type

  TPixelCallback = Procedure(X, Y: integer) Of Object;

  TCursorShape = (// . = Pixel überdeckt, X = 0/0 - Koordinate
    // X
    csDot,
    //  ..
    // ....
    // .X..
    //  ..
    csSmallPoint,
    //   ....
    //  ......
    // ........
    // ........
    // ...X....
    // ........
    //  ......
    //   ....
    csBigPoint,
    // ..
    // X.
    csSmallQuad,
    // .....
    // .....
    // ..X..
    // .....
    // .....
    csQuad,
    // .........
    // .........
    // .........
    // .........
    // ....X....
    // .........
    // .........
    // .........
    // .........
    csBigQuad
    );

  TCursorSize = (
    // X
    cs1_1,
    // ...
    // .X.
    // ...
    cs3_3,
    // .....
    // .....
    // ..X..
    // .....
    // .....
    cs5_5,
    // .......
    // .......
    // .......
    // ...X...
    // .......
    // .......
    // .......
    cs7_7
    );

  TTool = (
    tSelect,
    tBrighten, tDarken,
    tEraser, tPen, tLine, tEllipse, tRectangle, tMirror, tBucket, tPincette);

  TCompactCursor = Record // Alles was an Faltungsroutinen geht
    PixelPos: Tpoint; // -1,-1 = Ungültig, sonst Bildposition in Pixeln, aktualisiert durch MouseMove und MouseDown
    Shape: TCursorShape;
    Size: TCursorSize;
  End;

  TCursor = Record
    Compact: TCompactCursor;
    // Alles was der Cursor noch mehr braucht
    LeftColor: TOpenGL_ColorBox;
    RightColor: TRGBA;
    LastTool: TTool;
    Tool: TTool;
    PixelDownPos: Tpoint; // -1,-1 = Ungültig, sonst Bildposition in Pixeln, aktualisiert durch MouseDown, gelöscht durch MouseUp
    Pos: Tpoint; // "Raw" Position auf dem Screen
    Shift: Boolean; // True wenn die Taste "Shift" gedrückt ist.
    Outline: Boolean; // True nut Outlines, sonst gefüllt
    LeftMouseButton: Boolean;
    RightMouseButton: Boolean;
  End;

  TScrollInfo = Record
    GlobalXOffset, GlobalYOffset: integer; // In ScreenKoordinaten
    ScrollPos: Tpoint; // In ScreenKoordinaten
  End;

  TSettings = Record
    GridAboveImage: Boolean;

    // TODO: Hier noch weitere Programmsettings einfügen ;)

  End;

Procedure Nop(); // Nur zum Debuggen ;)

// Faltet die CursorGröße und Form mit der Aktuellen Koordinate und Ruft Callback
// für jede sich ergebende Koordinate auf (alles in Bild Pixel Koordinaten)
Procedure FoldCursorOnPixel(Const Cursor: TCompactCursor; Callback: TPixelCallback);

// Zeichnet eine Linie von Cursor nach aTo und ruft für jeden Pixel FoldCursorOnPixel mit callback auf
Procedure Bresenham_Line(Cursor: TCompactCursor; aTo: TPoint; Callback: TPixelCallback);
Procedure Bresenham_Ellipse(Cursor: TCompactCursor; aTo: TPoint; Filled: Boolean; Callback: TPixelCallback);
Procedure RectangleOutline(Cursor: TCompactCursor; P2: TPoint; Callback: TPixelCallback);

Function MovePointToNextMainAxis(P: TPoint): TPoint; // Projiziert P auf die nächste Hauptachse oder Hauptdiagonale
Function AdjustToMaxAbsValue(P: Tpoint): TPoint;

// TODO: if in some future the "ImplicitFunctionSpecialization" switch is enabled, all this helper can be deleted !
Function IfThen(val: boolean; Const iftrue: TBevelStyle; Const iffalse: TBevelStyle): TBevelStyle Inline; overload;
Function IfThen(val: boolean; Const iftrue: TCursorSize; Const iffalse: TCursorSize): TCursorSize Inline; overload;
Function IfThen(val: boolean; Const iftrue: TCursorShape; Const iffalse: TCursorShape): TCursorShape Inline; overload;

Implementation

Uses math;

Var
  CursorPixelPos: Array[TCursorSize, TCursorShape] Of Array Of TPoint; // Wird im Initialization teil gesetzt, damit da nicht jedesmal bei DoCursorOnPixel berechnet werden muss

Procedure Nop;
Begin

End;

Procedure FoldCursorOnPixel(Const Cursor: TCompactCursor; Callback: TPixelCallback);
Var
  i: Integer;
Begin
  For i := 0 To high(CursorPixelPos[Cursor.Size, Cursor.Shape]) Do Begin
    Callback(
      CursorPixelPos[Cursor.Size, Cursor.Shape][i].X + Cursor.PixelPos.x,
      CursorPixelPos[Cursor.Size, Cursor.Shape][i].y + Cursor.PixelPos.y
      );
  End;
End;

Procedure Bresenham_Line(Cursor: TCompactCursor; aTo: TPoint; Callback: TPixelCallback);
Var
  x, y, t, dx, dy, incx, incy, pdx, pdy, ddx, ddy, es, el, err: integer;
Begin
  dx := aTo.x - Cursor.PixelPos.x;
  dy := aTo.y - Cursor.PixelPos.y;
  incx := sign(dx);
  incy := sign(dy);
  If (dx < 0) Then dx := -dx;
  If (dy < 0) Then dy := -dy;
  If (dx > dy) Then Begin
    pdx := incx;
    pdy := 0;
    ddx := incx;
    ddy := incy;
    es := dy;
    el := dx;
  End
  Else Begin
    pdx := 0;
    pdy := incy;
    ddx := incx;
    ddy := incy;
    es := dx;
    el := dy;
  End;
  x := Cursor.PixelPos.x;
  y := Cursor.PixelPos.y;
  err := el Div 2;
  FoldCursorOnPixel(Cursor, Callback);
  For t := 0 To el - 1 Do Begin
    err := err - es;
    If (err < 0) Then Begin
      err := err + el;
      x := x + ddx;
      y := y + ddy;
    End
    Else Begin
      x := x + pdx;
      y := y + pdy;
    End;
    Cursor.PixelPos := point(x, y);
    FoldCursorOnPixel(Cursor, Callback);
  End;
End;

Procedure Bresenham_Ellipse(Cursor: TCompactCursor; aTo: TPoint; Filled: Boolean; Callback: TPixelCallback);
Var
  i, x, y, width, height: integer;
  xm, ym, a, b: integer;
  dx, dy, a2, b2, err, e2: integer;
Begin
  x := min(Cursor.PixelPos.X, aTo.X);
  y := min(Cursor.PixelPos.Y, aTo.Y);
  width := abs(Cursor.PixelPos.X - aTo.X);
  height := abs(Cursor.PixelPos.Y - aTo.Y);
  // Init
  xm := x + width Div 2;
  ym := y + height Div 2;
  a := width Div 2;
  b := height Div 2;
  dx := 0;
  dy := b;
  a2 := a * a;
  b2 := b * b;
  err := b2 - (2 * b - 1) * a2; (* Fehler im 1. Schritt *)
  If (err = 0) Then Begin // Wenn wir eigentlich noch gar nichts zeichnen ;)
    If Filled Then Begin
      Callback(x, y);
    End
    Else Begin
      Cursor.PixelPos := point(x, y);
      FoldCursorOnPixel(Cursor, Callback);
    End;
    exit;
  End;
  // Paint
  Repeat
    If Filled Then Begin
      For i := -dx To dx Do Begin
        Callback(xm + i, ym + dy);
        Callback(xm + i, ym - dy);
      End;
    End
    Else Begin
      Cursor.PixelPos := point(xm + dx, ym + dy); // I. Quadrant
      FoldCursorOnPixel(Cursor, Callback);
      Cursor.PixelPos := point(xm - dx, ym + dy); // II. Quadrant
      FoldCursorOnPixel(Cursor, Callback);
      Cursor.PixelPos := point(xm - dx, ym - dy); // III. Quadrant
      FoldCursorOnPixel(Cursor, Callback);
      Cursor.PixelPos := point(xm + dx, ym - dy); // IV. Quadrant
      FoldCursorOnPixel(Cursor, Callback);
    End;
    e2 := 2 * err;
    If (e2 < (2 * dx + 1) * b2) Then Begin
      dx := dx + 1;
      err := err + (2 * dx + 1) * b2;
    End;
    If (e2 > -(2 * dy - 1) * a2) Then Begin
      dy := dy - 1;
      err := err - (2 * dy - 1) * a2;
    End;
  Until (dy < 0);

  dx := dx + 1;
  While (dx < a) Do Begin // fehlerhafter Abbruch bei flachen Ellipsen (b=1)
    If Filled Then Begin // -> Spitze der Ellipse vollenden
      For i := -dx To dx Do Begin
        Callback(xm + i, ym);
      End;
    End
    Else Begin
      Cursor.PixelPos := point(xm + dx, ym);
      FoldCursorOnPixel(Cursor, Callback);
      Cursor.PixelPos := point(xm - dx, ym);
      FoldCursorOnPixel(Cursor, Callback);
    End;
    dx := dx + 1;
  End;
End;

Procedure RectangleOutline(Cursor: TCompactCursor; P2: TPoint; Callback: TPixelCallback);
Var
  tl: TPoint;
  w, h, i, j: integer;
Begin
  tl := point(min(Cursor.PixelPos.X, p2.x), min(Cursor.PixelPos.Y, p2.y));
  w := abs(Cursor.PixelPos.x - p2.x);
  h := abs(Cursor.PixelPos.Y - p2.Y);
  For i := 0 To w - 1 Do Begin
    Cursor.PixelPos := point(tl.x + i, tl.Y);
    FoldCursorOnPixel(Cursor, Callback);
    Cursor.PixelPos := point(tl.x + i, tl.Y + h - 1);
    FoldCursorOnPixel(Cursor, Callback);
  End;
  For j := 1 To h - 2 Do Begin
    Cursor.PixelPos := point(tl.x, tl.y + j);
    FoldCursorOnPixel(Cursor, Callback);
    Cursor.PixelPos := point(tl.x + w - 1, tl.y + j);
    FoldCursorOnPixel(Cursor, Callback);
  End;
End;

Procedure InitCursorPixelPos;
Var
  PointList: Array[0..1023] Of TPoint;
  PointListCnt: Integer;

  Procedure AddCoord(i, j: integer);
  Var
    a: Integer;
    p: TPoint;
  Begin
    p := point(i, j);
    For a := 0 To PointListCnt - 1 Do Begin
      If PointList[a] = p Then Begin
        exit;
      End;
    End;
    PointList[PointListCnt] := p;
    inc(PointListCnt);
  End;

Var
  aSize: TCursorSize;

  Procedure AddCursorSize(i, j: integer);
  Var
    a, b: Integer;
  Begin
    Case aSize Of
      cs1_1: AddCoord(i, j);
      cs3_3: Begin
          For a := -1 To 1 Do Begin
            For b := -1 To 1 Do Begin
              AddCoord(i + a, j + b);
            End;
          End;
        End;
      cs5_5: Begin
          For a := -2 To 2 Do Begin
            For b := -2 To 2 Do Begin
              AddCoord(i + a, j + b);
            End;
          End;
        End;
      cs7_7: Begin
          For a := -3 To 3 Do Begin
            For b := -3 To 3 Do Begin
              AddCoord(i + a, j + b);
            End;
          End;
        End;
    End;
  End;

Var
  i, j: integer;
  aShape: TCursorShape;
Begin
  For aShape In TCursorShape Do Begin
    For aSize In TCursorSize Do Begin
      PointListCnt := 0;
      Case ashape Of
        csDot: AddCursorSize(0, 0);
        csSmallPoint: Begin
            For i := 0 To 1 Do Begin
              For j := 0 To 3 Do Begin
                AddCursorSize(i, j - 2);
                AddCursorSize(j - 1, -i);
              End;
            End;
          End;
        csBigPoint: Begin
            For i := 0 To 7 Do Begin
              For j := 0 To 3 Do Begin
                AddCursorSize(i - 3, j - 2);
                AddCursorSize(j - 1, i - 4);
              End;
            End;
            AddCursorSize(-2, -3);
            AddCursorSize(3, -3);
            AddCursorSize(-2, 2);
            AddCursorSize(3, 2);
          End;
        csSmallQuad: Begin
            For i := 0 To 1 Do Begin
              For j := 0 To 1 Do Begin
                AddCursorSize(i, j);
              End;
            End;
          End;
        csQuad: Begin
            For i := -2 To 2 Do Begin
              For j := -2 To 2 Do Begin
                AddCursorSize(i, j);
              End;
            End;
          End;
        csBigQuad: Begin
            For i := -3 To 3 Do Begin
              For j := -3 To 3 Do Begin
                AddCursorSize(i, j);
              End;
            End;
          End;
      End;
      setlength(CursorPixelPos[aSize, aShape], PointListCnt);
      For i := 0 To PointListCnt - 1 Do Begin
        CursorPixelPos[aSize, aShape][i] := PointList[i];
      End;
    End;
  End
End;

Function IfThen(val: boolean; Const iftrue: TBevelStyle;
  Const iffalse: TBevelStyle): TBevelStyle;
Begin
  result := specialize ifthen < TBevelStyle > (val, iftrue, iffalse);
End;

Function IfThen(val: boolean; Const iftrue: TCursorSize;
  Const iffalse: TCursorSize): TCursorSize;
Begin
  result := specialize ifthen < TCursorSize > (val, iftrue, iffalse);
End;

Function IfThen(val: boolean; Const iftrue: TCursorShape;
  Const iffalse: TCursorShape): TCursorShape;
Begin
  result := specialize ifthen < TCursorShape > (val, iftrue, iffalse);
End;

// unbelievable, but true, this code was created by using ChatGPT, and after some adjustmens it works like expected ;)

Function MovePointToNextMainAxis(P: TPoint): TPoint;
Var
  dist_x_axis, dist_y_axis, dist_diagonal1, dist_diagonal2: Integer;
  x, y, min_dist: Integer;
Begin
  x := p.x;
  y := p.y;

  // Calculate distances
  dist_x_axis := Abs(Y);
  dist_y_axis := Abs(X);
  dist_diagonal1 := Abs(X - Y);
  dist_diagonal2 := Abs(X + Y);

  // Find the minimum distance
  min_dist := dist_x_axis;

  If dist_y_axis < min_dist Then
    min_dist := dist_y_axis;
  If dist_diagonal1 < min_dist Then
    min_dist := dist_diagonal1;
  If dist_diagonal2 < min_dist Then
    min_dist := dist_diagonal2;

  // Move point to the corresponding line
  If min_dist = dist_x_axis Then
    result := Point(X, 0) // Move to the x-axis
  Else If min_dist = dist_y_axis Then
    result := Point(0, Y) // Move to the y-axis
  Else If min_dist = dist_diagonal1 Then
    result := Point(X, X) // Move to the main diagonal (X = Y)
  Else
    result := Point(X, -X); // Move to the other diagonal (X = -Y)
End;

Function AdjustToMaxAbsValue(P: Tpoint): TPoint;
Var
  max_abs: Integer;
Begin
  max_abs := max(abs(p.x), abs(p.y));
  result := point(sign(p.x) * max_abs, sign(p.y) * max_abs);
End;

Initialization
  InitCursorPixelPos;

End.

