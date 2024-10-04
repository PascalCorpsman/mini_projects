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

  TCursorShape = ( // . = Pixel überdeckt, X = 0/0 - Koordinate
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
    PixelPos: Tpoint; // -1,-1 = Ungültig, sonst Bildposition in Pixeln
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

Procedure Nop();

// TODO: if in some future the "ImplicitFunctionSpecialization" switch is enabled, all this helper can be deleted !
Function IfThen(val: boolean; Const iftrue: TBevelStyle; Const iffalse: TBevelStyle): TBevelStyle Inline; overload;
Function IfThen(val: boolean; Const iftrue: TCursorSize; Const iffalse: TCursorSize): TCursorSize Inline; overload;
Function IfThen(val: boolean; Const iftrue: TCursorShape; Const iffalse: TCursorShape): TCursorShape Inline; overload;

Implementation

Procedure Nop();
Begin

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


End.

