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

  TCursorCallback = Procedure(x, y: integer) Is nested;

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

  TCursor = Record
    LeftColor: TOpenGL_ColorBox;
    RightColor: TRGBA;
    LastTool: TTool;
    Tool: TTool;
    PixelDownPos: Tpoint; // -1,-1 = Ungültig, sonst Bildposition in Pixeln, aktualisiert durch MouseDown, gelöscht durch MouseUp
    PixelPos: Tpoint; // -1,-1 = Ungültig, sonst Bildposition in Pixeln, aktualisiert durch MouseMove und MouseDown
    Pos: Tpoint; // "Raw" Position auf dem Screen
    Shape: TCursorShape;
    Size: TCursorSize;
  End;

  TScrollInfo = Record
    GlobalXOffset, GlobalYOffset: integer; // In ScreenKoordinaten
    ScrollPos: Tpoint; // In ScreenKoordinaten
  End;

  TSettings = Record
    GridAboveImage: Boolean;
  End;

Procedure Nop(); // Nur zum Debuggen ;)

// Faltet die CursorGröße und Form mit der Aktuellen Koordinate und Ruft Callback
// für jede sich ergebende Koordinate auf (alles in Bild Pixel Koordinaten)
Procedure DoCursorOnPixel(Const fCursor: TCursor; Callback: TCursorCallback);
Procedure Bresenham_Line(Const aFrom, aTo: TPoint; Callback: TCursorCallback);

// TODO: if in some future the "ImplicitFunctionSpecialization" switch is enabled, all this helper can be deleted !
Function IfThen(val: boolean; Const iftrue: TBevelStyle; Const iffalse: TBevelStyle): TBevelStyle Inline; overload;
Function IfThen(val: boolean; Const iftrue: TCursorSize; Const iffalse: TCursorSize): TCursorSize Inline; overload;
Function IfThen(val: boolean; Const iftrue: TCursorShape; Const iffalse: TCursorShape): TCursorShape Inline; overload;

Implementation

Uses math;

Var
  CursorPixelPos: Array[TCursorSize, TCursorShape] Of Array Of TPoint; // Wird im Initialization teil gesetzt, damit da nicht jedesmal bei DoCursorOnPixel berechnet werden muss

Procedure Nop();
Begin

End;

Procedure DoCursorOnPixel(Const fCursor: TCursor; Callback: TCursorCallback);
Var
  i: Integer;
Begin
  For i := 0 To high(CursorPixelPos[fCursor.Size, fCursor.Shape]) Do Begin
    Callback(
      CursorPixelPos[fCursor.Size, fCursor.Shape][i].X + fCursor.PixelPos.x,
      CursorPixelPos[fCursor.Size, fCursor.Shape][i].y + fCursor.PixelPos.y
      );
  End;
End;

Procedure Bresenham_Line(Const aFrom, aTo: TPoint; Callback: TCursorCallback);
Var
  x, y, t, dx, dy, incx, incy, pdx, pdy, ddx, ddy, es, el, err: integer;
Begin
  dx := aTo.x - aFrom.x;
  dy := aTo.y - aFrom.y;
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
  x := aFrom.x;
  y := aFrom.y;
  err := el Div 2;
  Callback(x, y);
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
    Callback(x, y);
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

Function IfThen(val: boolean; Const iftrue: TBevelStyle; Const iffalse: TBevelStyle): TBevelStyle Inline; overload;
Begin
  result := specialize ifthen < TBevelStyle > (val, iftrue, iffalse);
End;

Function IfThen(val: boolean; Const iftrue: TCursorSize; Const iffalse: TCursorSize): TCursorSize Inline; overload;
Begin
  result := specialize ifthen < TCursorSize > (val, iftrue, iffalse);
End;

Function IfThen(val: boolean; Const iftrue: TCursorShape; Const iffalse: TCursorShape): TCursorShape Inline; overload;
Begin
  result := specialize ifthen < TCursorShape > (val, iftrue, iffalse);
End;

Initialization
  InitCursorPixelPos;

End.

