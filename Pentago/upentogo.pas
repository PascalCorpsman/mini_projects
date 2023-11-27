(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Pentago                                               *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit upentogo;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, // TShiftstate
  Controls, // TMousebutton
  LCLType, // VK-Codes
  Forms, // Application
  Graphics,
  SysUtils,
  LCLIntf, IntfGraphics, LResources,
  ugraphics,
  dglOpenGL, uOpenGL_ASCII_Font;

Const
  PentagoVersion = '1.0';

Type

  { tPentago }

    // Das Spielfeld
  TPentagoField = Array[0..5, 0..5] Of Shortint;

  TPentagostate = (
    psMainMenu, // Im Main Menü
    psPlayer_Weiss_Zug, // Weiser Spieler ist dran
    psPlayer_Schwarz_Zug,
    psPlayer_Weiss_Rotieren,
    psPlayer_Schwarz_Rotieren,
    psRotiereFeldUhrzeigersinn,
    psRotiereFeldGegenUhrzeigersinn,
    psWeissHatGewonnen,
    psSchwarzHatGewonnen,
    psShowFin // Zeigt nochmal das Spielfeld, aber ohne dass jemand was editieren kann
    );

  TPentago = Class
  private
    fAnimationOffx: integer; // Der Feldblock, welcher Animiert werden soll
    fAnimationOffy: integer; // Der Feldblock, welcher Animiert werden soll
    fAnimationStep: Integer;
    fAnimLastTimeStep: DWord;
    FNachAnimState: TPentagostate;
    FMouseInfo: gluint;
    FMouseInfo_cnt: integer;
    FPentagostate: TPentagostate;
    FField: TPentagoField;
    Procedure fGetWinner();
    Procedure fRotField(offx, offy: Integer; Drehsinn: Boolean);
    Function fNeedAnimation(Offx, Offy: Integer): Boolean;
    Procedure fGetFeld(Var offx, offy: Integer); // Setzt die Offx, Offy so das sie in die Mitte des jeweiligen Feldes Zeigen
    Procedure FStep(x, y: integer; Dir: Boolean);
    Procedure InitGame();
    Procedure fCenterText(Value: String; y: integer);
    Procedure fRenderRotField(Offx, Offy, Step: integer; DrehSinn: Boolean);
  public
    Constructor create;
    Destructor destroy; override;
    Procedure Render();
    Procedure OnKeyDown(Var Key: Word; Shift: TShiftState);
    Procedure OnMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  End;

Const
  ItemSize = 75; // Größe der Löcher
  FieldSpace = 10; // Abstand zwischen den 9er blöcken
  StepDelay = 10; // Die Rotationsgeschwindigkeit bei der Animation

Implementation

//Uses unit1; // Debugg

Procedure SetColor(Const Value: Shortint);
Begin
  Case Value Of
    -1: glcolor3f(0, 0, 0);
    0: glcolor3f(0.5, 0.5, 0.5);
    1: glcolor3f(1, 1, 1);
  End;
End;

Procedure RenderCircle(x, y, radius: Double; Filled: Boolean);
Const
  Count = 20;
Var
  i: Integer;
Begin
  If Filled Then
    glBegin(GL_POLYGON)
  Else
    glBegin(GL_Line_Loop);
  For i := 0 To Count - 1 Do
    glvertex3f(x + cos((2 * i * pi) / Count) * Radius, y + sin((2 * i * pi) / count) * Radius, 0);
  glend;
End;

Procedure RenderQuad(Const Value: TPentagoField; Offx, Offy: Integer);
Var
  Itemradius: Double;
  ItemSize15: Double;
  i, j: Integer;
Begin
  ItemSize15 := ItemSize * 3 / 2;
  Itemradius := 0.45 * ItemSize;
  glcolor3f(0.25, 0.25, 0.25);
  glbegin(gl_quads);
  glvertex3f(-ItemSize15, -ItemSize15, 0);
  glvertex3f(-ItemSize15, ItemSize15, 0);
  glvertex3f(ItemSize15, ItemSize15, 0);
  glvertex3f(ItemSize15, -ItemSize15, 0);
  glend;
  For i := -1 To 1 Do
    For j := -1 To 1 Do Begin
      SetColor(Value[offx + i, offy + j]);
      RenderCircle(itemsize * i, itemsize * j, Itemradius, Value[offx + i, offy + j] <> 0);
    End;
End;

{ tPentago }

Constructor TPentago.create;
Var
  b: Tbitmap;
  i, j: integer;
  TempIntfImg: TLazIntfImage;
  c: TRGB;
Begin
  Inherited Create;
  FPentagostate := psMainMenu;
  b := Tbitmap.Create;
  b.LoadFromLazarusResource('mouse_info');
  TempIntfImg := TLazIntfImage.Create(0, 0);
  TempIntfImg.LoadFromBitmap(B.Handle, B.MaskHandle);
  FMouseInfo := glGenLists(1);
  FMouseInfo_cnt := 0;
  glNewList(FMouseInfo, GL_COMPILE);
  glbegin(GL_points);
  For i := 0 To b.Width - 1 Do Begin
    For j := 0 To b.Height - 1 Do Begin
      If FPColorToColor(TempIntfImg.colors[i, j]) <> clfuchsia Then Begin
        c := ColorToRGB(FPColorToColor(TempIntfImg.colors[i, j]));
        glColor3ubv(@c);
        glVertex2i(i, j);
        inc(FMouseInfo_cnt);
      End;
    End;
  End;
  glend;
  glEndList();
  b.free;
  TempIntfImg.free;
End;

Destructor TPentago.destroy;
Begin
  glDeleteLists(FMouseInfo, FMouseInfo_cnt); // Delete All Display Lists
End;

Procedure TPentago.fGetWinner;
Var
  i, j: Integer;
  b: Boolean;
  res: Integer;
Begin
  res := 0;
  // Die Diagonalen sind so wenige das ist HardCoded
  // 2. Winkelhalbierende
  If (ffield[0, 0] <> 0) And
    (ffield[0, 0] = ffield[1, 1]) And
    (ffield[0, 0] = ffield[2, 2]) And
    (ffield[0, 0] = ffield[3, 3]) And
    (ffield[0, 0] = ffield[4, 4]) Then Begin
    res := res + ffield[0, 0];
  End;
  If (ffield[1, 1] <> 0) And
    (ffield[1, 1] = ffield[2, 2]) And
    (ffield[1, 1] = ffield[3, 3]) And
    (ffield[1, 1] = ffield[4, 4]) And
    (ffield[1, 1] = ffield[5, 5]) Then Begin
    res := res + ffield[1, 1];
  End;
  If (ffield[1, 0] <> 0) And
    (ffield[1, 0] = ffield[2, 1]) And
    (ffield[1, 0] = ffield[3, 2]) And
    (ffield[1, 0] = ffield[4, 3]) And
    (ffield[1, 0] = ffield[5, 4]) Then Begin
    res := res + ffield[1, 0];
  End;
  If (ffield[0, 1] <> 0) And
    (ffield[0, 1] = ffield[1, 2]) And
    (ffield[0, 1] = ffield[2, 3]) And
    (ffield[0, 1] = ffield[3, 4]) And
    (ffield[0, 1] = ffield[4, 5]) Then Begin
    res := res + ffield[0, 1];
  End;
  // 1. Winkelhalbierende
  If (ffield[0, 5] <> 0) And
    (ffield[0, 5] = ffield[1, 4]) And
    (ffield[0, 5] = ffield[2, 3]) And
    (ffield[0, 5] = ffield[3, 2]) And
    (ffield[0, 5] = ffield[4, 1]) Then Begin
    res := res + ffield[0, 5];
  End;
  If (ffield[1, 4] <> 0) And
    (ffield[1, 4] = ffield[2, 3]) And
    (ffield[1, 4] = ffield[3, 2]) And
    (ffield[1, 4] = ffield[4, 1]) And
    (ffield[1, 4] = ffield[5, 0]) Then Begin
    res := res + ffield[1, 4];
  End;
  If (ffield[0, 4] <> 0) And
    (ffield[0, 4] = ffield[1, 3]) And
    (ffield[0, 4] = ffield[2, 2]) And
    (ffield[0, 4] = ffield[3, 1]) And
    (ffield[0, 4] = ffield[4, 0]) Then Begin
    res := res + ffield[0, 4];
  End;
  If (ffield[1, 5] <> 0) And
    (ffield[1, 5] = ffield[2, 4]) And
    (ffield[1, 5] = ffield[3, 3]) And
    (ffield[1, 5] = ffield[4, 2]) And
    (ffield[1, 5] = ffield[5, 1]) Then Begin
    res := res + ffield[1, 5];
  End;
  // Die Waagrechten
  For j := 0 To 5 Do Begin
    b := True;
    If ffield[0, j] <> 0 Then Begin
      For i := 1 To 4 Do
        If ffield[0, j] <> ffield[i, j] Then b := False;
    End
    Else
      b := False;
    If b Then Begin
      Res := res + ffield[0, j];
    End;
    b := True;
    If ffield[1, j] <> 0 Then Begin
      For i := 1 To 4 Do
        If ffield[1, j] <> ffield[i + 1, j] Then b := False;
    End
    Else
      b := False;
    If b Then Begin
      Res := res + ffield[1, j];
    End;
  End;
  // Die Senkrechten
  For j := 0 To 5 Do Begin
    b := True;
    If ffield[j, 0] <> 0 Then Begin
      For i := 1 To 4 Do
        If ffield[j, 0] <> ffield[j, i] Then b := False;
    End
    Else
      b := False;
    If b Then Begin
      Res := res + ffield[j, 0];
    End;
    b := True;
    If ffield[j, 1] <> 0 Then Begin
      For i := 1 To 4 Do
        If ffield[j, 1] <> ffield[j, i + 1] Then b := False;
    End
    Else
      b := False;
    If b Then Begin
      Res := res + ffield[j, 1];
    End;
  End;
  If res > 0 Then Begin
    // Der Weiße Spieler hat gewonnen
    FPentagostate := psWeissHatGewonnen;
  End;
  If res < 0 Then Begin
    // Der Weiße Spieler hat gewonnen
    FPentagostate := psSchwarzHatGewonnen;
  End;
End;

Procedure TPentago.fRotField(offx, offy: Integer; Drehsinn: Boolean);
Var
  t: Shortint;
Begin
  If Drehsinn Then Begin
    // Die Ecken
    t := ffield[offx - 1, offy - 1];
    ffield[offx - 1, offy - 1] := ffield[offx - 1, offy + 1];
    ffield[offx - 1, offy + 1] := ffield[offx + 1, offy + 1];
    ffield[offx + 1, offy + 1] := ffield[offx + 1, offy - 1];
    ffield[offx + 1, offy - 1] := t;
    // Die Mittelstücke
    t := ffield[offx, offy - 1];
    ffield[offx, offy - 1] := ffield[offx - 1, offy];
    ffield[offx - 1, offy] := ffield[offx, offy + 1];
    ffield[offx, offy + 1] := ffield[offx + 1, offy];
    ffield[offx + 1, offy] := t;
  End
  Else Begin
    // Die Ecken
    t := ffield[offx - 1, offy - 1];
    ffield[offx - 1, offy - 1] := ffield[offx + 1, offy - 1];
    ffield[offx + 1, offy - 1] := ffield[offx + 1, offy + 1];
    ffield[offx + 1, offy + 1] := ffield[offx - 1, offy + 1];
    ffield[offx - 1, offy + 1] := t;
    // Die Mittelstücke
    t := ffield[offx, offy - 1];
    ffield[offx, offy - 1] := ffield[offx + 1, offy];
    ffield[offx + 1, offy] := ffield[offx, offy + 1];
    ffield[offx, offy + 1] := ffield[offx - 1, offy];
    ffield[offx - 1, offy] := t;
  End;
End;

Function TPentago.fNeedAnimation(Offx, Offy: Integer): Boolean;
Begin
  result := (ffield[offx - 1, offy] <> ffield[offx, offy - 1]) Or
    (ffield[offx - 1, offy] <> ffield[offx + 1, offy]) Or
    (ffield[offx - 1, offy] <> ffield[offx, offy + 1]) Or
    (ffield[offx - 1, offy - 1] <> ffield[offx + 1, offy - 1]) Or
    (ffield[offx - 1, offy - 1] <> ffield[offx + 1, offy + 1]) Or
    (ffield[offx - 1, offy - 1] <> ffield[offx - 1, offy + 1]);
End;

Procedure TPentago.fGetFeld(Var offx, offy: Integer);
Begin
  If offx < 3 Then
    offx := 1
  Else
    offx := 4;
  If offy < 3 Then
    offy := 1
  Else
    offy := 4;
End;

Procedure TPentago.FStep(x, y: integer; Dir: Boolean);
Begin
  Case FPentagostate Of
    psPlayer_Weiss_Zug: Begin
        If FField[x, y] = 0 Then Begin
          FPentagostate := psPlayer_Weiss_Rotieren;
          FField[x, y] := 1;
        End;
      End;
    psPlayer_Weiss_Rotieren: Begin
        fGetFeld(x, y);
        If fNeedAnimation(x, y) Then Begin // Drehen mit Animation
          fAnimationOffx := x;
          fAnimationOffy := y;
          fAnimationStep := 0;
          fAnimLastTimeStep := GetTickCount;
          FNachAnimState := psPlayer_Schwarz_Zug;
          If dir Then
            FPentagostate := psRotiereFeldUhrzeigersinn
          Else
            FPentagostate := psRotiereFeldGegenUhrzeigersinn;
        End
        Else Begin // Drehen ohne Animation !!
          If dir Then
            fRotField(x, y, true)
          Else
            fRotField(x, y, false);
          FPentagostate := psPlayer_Schwarz_Zug;
          fGetWinner();
        End;
      End;
    psPlayer_Schwarz_Zug: Begin
        If FField[x, y] = 0 Then Begin
          FPentagostate := psPlayer_Schwarz_Rotieren;
          FField[x, y] := -1;
        End;
      End;
    psPlayer_Schwarz_Rotieren: Begin
        fGetFeld(x, y);
        If fNeedAnimation(x, y) Then Begin // Drehen mit Animation
          fAnimationOffx := x;
          fAnimationOffy := y;
          fAnimationStep := 0;
          fAnimLastTimeStep := GetTickCount;
          FNachAnimState := psPlayer_Weiss_Zug;
          If dir Then
            FPentagostate := psRotiereFeldUhrzeigersinn
          Else
            FPentagostate := psRotiereFeldGegenUhrzeigersinn;
        End
        Else Begin // Drehen ohne Animation !!
          If dir Then
            fRotField(x, y, true)
          Else
            fRotField(x, y, false);
          FPentagostate := psPlayer_Weiss_Zug;
          fGetWinner();
        End;
      End;
  End;
End;

Procedure TPentago.InitGame;
Var
  i, j: Integer;
Begin
  For i := 0 To 5 Do
    For j := 0 To 5 Do
      FField[i, j] := 0;
  If random(100) >= 50 Then Begin
    FPentagostate := psPlayer_Weiss_Zug; // Weiser Spieler ist dran
  End
  Else Begin
    FPentagostate := psPlayer_Schwarz_Zug;
  End;
End;

Procedure TPentago.fCenterText(Value: String; y: integer);
Var
  w: integer;
Begin
  w := round(OpenGL_ASCII_Font.TextWidth(Value));
  OpenGL_ASCII_Font.Textout(320 - w Div 2, y, Value);
End;

Procedure TPentago.fRenderRotField(Offx, Offy, Step: integer; DrehSinn: Boolean
  );
Var
  TranslationVektorx,
    TranslationVektory: Double;
  Angle, Dist: Double;
Begin
  Dist := 3 * Itemsize / 2 + FieldSpace / 2;
  glpushmatrix;
  gltranslatef(320, 240, 0);
  TranslationVektorx := sqrt(2) * ItemSize / 2;
  TranslationVektory := TranslationVektorx;
  // Rendern Feld 1
  glpushmatrix;
  If (Offx = 1) And (offy = 1) Then Begin
    TranslationVektorx := -TranslationVektorx;
    TranslationVektory := -TranslationVektory;
    Case Step Of
      0..25: Begin
          gltranslatef(-Dist, -Dist, 0);
          gltranslatef(TranslationVektorx * Step / 25, TranslationVektory * Step / 25, 0);
        End;
      26..75: Begin
          angle := ((Step - 26) / 49) * 90;
          gltranslatef(-Dist, -Dist, 0);
          gltranslatef(TranslationVektorx, TranslationVektory, 0);
          If Drehsinn Then
            glrotatef(angle, 0, 0, 1)
          Else
            glrotatef(-angle, 0, 0, 1);
        End;
      76..100: Begin
          Step := 25 - (Step - 76);
          gltranslatef(-Dist, -Dist, 0);
          gltranslatef(TranslationVektorx * Step / 25, TranslationVektory * Step / 25, 0);
          If Drehsinn Then
            glrotatef(90, 0, 0, 1)
          Else
            glrotatef(-90, 0, 0, 1);
        End;
    End;
  End
  Else Begin
    gltranslatef(-Dist, -Dist, 0);
  End;
  RenderQuad(ffield, 1, 1);
  glpopmatrix;
  // Rendern Feld 2
  glpushmatrix;
  If (Offx = 4) And (offy = 1) Then Begin
    TranslationVektory := -TranslationVektory;
    Case Step Of
      0..25: Begin
          gltranslatef(Dist, -Dist, 0);
          gltranslatef(TranslationVektorx * Step / 25, TranslationVektory * Step / 25, 0);
        End;
      26..75: Begin
          angle := ((Step - 26) / 49) * 90;
          gltranslatef(Dist, -Dist, 0);
          gltranslatef(TranslationVektorx, TranslationVektory, 0);
          If Drehsinn Then
            glrotatef(angle, 0, 0, 1)
          Else
            glrotatef(-angle, 0, 0, 1);
        End;
      76..100: Begin
          Step := 25 - (Step - 76);
          gltranslatef(Dist, -Dist, 0);
          gltranslatef(TranslationVektorx * Step / 25, TranslationVektory * Step / 25, 0);
          If Drehsinn Then
            glrotatef(90, 0, 0, 1)
          Else
            glrotatef(-90, 0, 0, 1);
        End;
    End;
  End
  Else Begin
    gltranslatef(Dist, -Dist, 0);
  End;
  RenderQuad(ffield, 4, 1);
  glpopmatrix;
  // Rendern Feld 3
  glpushmatrix;
  If (Offx = 1) And (offy = 4) Then Begin
    TranslationVektorx := -TranslationVektorx;
    Case Step Of
      0..25: Begin
          gltranslatef(-Dist, Dist, 0);
          gltranslatef(TranslationVektorx * Step / 25, TranslationVektory * Step / 25, 0);
        End;
      26..75: Begin
          angle := ((Step - 26) / 49) * 90;
          gltranslatef(-Dist, Dist, 0);
          gltranslatef(TranslationVektorx, TranslationVektory, 0);
          If Drehsinn Then
            glrotatef(angle, 0, 0, 1)
          Else
            glrotatef(-angle, 0, 0, 1);
        End;
      76..100: Begin
          Step := 25 - (Step - 76);
          gltranslatef(-Dist, Dist, 0);
          gltranslatef(TranslationVektorx * Step / 25, TranslationVektory * Step / 25, 0);
          If Drehsinn Then
            glrotatef(90, 0, 0, 1)
          Else
            glrotatef(-90, 0, 0, 1);
        End;
    End;
  End
  Else Begin
    gltranslatef(-Dist, Dist, 0);
  End;
  RenderQuad(ffield, 1, 4);
  glpopmatrix;
  // Rendern Feld 4
  glpushmatrix;
  If (Offx = 4) And (offy = 4) Then Begin
    Case Step Of
      0..25: Begin
          gltranslatef(Dist, Dist, 0);
          gltranslatef(TranslationVektorx * Step / 25, TranslationVektory * Step / 25, 0);
        End;
      26..75: Begin
          angle := ((Step - 26) / 49) * 90;
          gltranslatef(Dist, Dist, 0);
          gltranslatef(TranslationVektorx, TranslationVektory, 0);
          If Drehsinn Then
            glrotatef(angle, 0, 0, 1)
          Else
            glrotatef(-angle, 0, 0, 1);
        End;
      76..100: Begin
          Step := 25 - (Step - 76);
          gltranslatef(Dist, Dist, 0);
          gltranslatef(TranslationVektorx * Step / 25, TranslationVektory * Step / 25, 0);
          If Drehsinn Then
            glrotatef(90, 0, 0, 1)
          Else
            glrotatef(-90, 0, 0, 1);
        End;
    End;
  End
  Else Begin
    gltranslatef(Dist, Dist, 0);
  End;
  RenderQuad(ffield, 4, 4);
  glpopmatrix;
  glpopmatrix;
End;

Procedure TPentago.Render;
Var
  Dist: Double;
  t: Dword;
  //diff: Integer;
Begin
  Case FPentagostate Of
    psMainMenu: Begin
        fCenterText('Pentago ver. ' + PentagoVersion + ' by Corpsman' + LineEnding +
          '        www.Corpsman.de' + LineEnding +
          '' + LineEnding + LineEnding + LineEnding + LineEnding +
          '     Please select:' + LineEnding + LineEnding +
          '       n = New game' + LineEnding + LineEnding +
          '     Esc = Close', 25);
      End;
    psRotiereFeldGegenUhrzeigersinn,
      psRotiereFeldUhrzeigersinn: Begin
        fRenderRotField(fAnimationOffx, fAnimationOffy, fAnimationStep, FPentagostate = psRotiereFeldUhrzeigersinn);
        // Nächster Schritt
        //i := SDL_GetTicks;
        t := GetTickCount;
        If t - StepDelay > fAnimLastTimeStep Then Begin
          //Testen
          //diff := t - fAnimLastTimeStep;
          //fAnimationStep := fAnimationStep + diff Div StepDelay; // So müsste die Zeit Normierte Variante aussehen
          fAnimationStep := fAnimationStep + 1; // So die Nicht Zeit Normierte Variante
          fAnimLastTimeStep := t;
          If fAnimationStep > 100 Then Begin
            // Drehen Des Feldes
            fRotField(fAnimationOffx, fAnimationOffy, FPentagostate = psRotiereFeldUhrzeigersinn);
            // Umschalten auf anderen Player
            FPentagostate := FNachAnimState;
            fGetWinner(); // Nach der Drehung wird auf Sieger Kontrolliert
          End;
        End;
      End;
    psWeissHatGewonnen: Begin
        fCenterText('White player wins.', 240);
      End;
    psSchwarzHatGewonnen: Begin
        fCenterText('Black player wins.', 240);
      End;
    psShowFin,
      psPlayer_Schwarz_Zug,
      psPlayer_Schwarz_Rotieren,
      psPlayer_Weiss_Rotieren,
      psPlayer_Weiss_Zug: Begin
        // Rendern Des Spielfeldes !!
        Dist := 3 * Itemsize / 2 + FieldSpace / 2;
        glpushmatrix;
        gltranslatef(320, 240, 0);
        glpushmatrix;
        gltranslatef(-Dist, -Dist, 0);
        RenderQuad(FField, 1, 1);
        glpopmatrix;
        glpushmatrix;
        gltranslatef(Dist, -Dist, 0);
        RenderQuad(FField, 4, 1);
        glpopmatrix;
        glpushmatrix;
        gltranslatef(-Dist, Dist, 0);
        RenderQuad(FField, 1, 4);
        glpopmatrix;
        glpushmatrix;
        gltranslatef(Dist, Dist, 0);
        RenderQuad(FField, 4, 4);
        glpopmatrix;
        glpopmatrix;
        If FPentagostate = psPlayer_Schwarz_Zug Then Begin
          OpenGL_ASCII_Font.Textout(10, 50, 'black turn');
        End;
        If FPentagostate = psPlayer_Weiss_Zug Then Begin
          OpenGL_ASCII_Font.Textout(10, 50, 'white turn');
        End;
        If FPentagostate = psPlayer_Weiss_Rotieren Then Begin
          OpenGL_ASCII_Font.Textout(10, 50, 'white rotate');
          glPushMatrix();
          glTranslatef(10, 60, 0);
          glCallList(FMouseInfo);
          glPopMatrix();
        End;
        If FPentagostate = psPlayer_Schwarz_Rotieren Then Begin
          OpenGL_ASCII_Font.Textout(10, 50, 'black rotate');
          glPushMatrix();
          glTranslatef(10, 60, 0);
          glCallList(FMouseInfo);
          glPopMatrix();
        End;
      End;
  End;
End;

Procedure TPentago.OnKeyDown(Var Key: Word; Shift: TShiftState);
Begin
  // ESC - Beendet immer.
  If (key = vk_escape) Then application.Terminate;
  Case FPentagostate Of
    psShowFin,
      psMainMenu: Begin
        Case key Of
          VK_N: InitGame();
        End;
      End;
    psWeissHatGewonnen,
      psSchwarzHatGewonnen: Begin
        FPentagostate := psShowFin;
      End;
  End;
End;

Procedure TPentago.OnMouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  dim: Array[0..3] Of Integer;
  w, h: integer;
  sx, sy: Single;
Begin
  Case FPentagostate Of
    psWeissHatGewonnen,
      psSchwarzHatGewonnen: Begin
        FPentagostate := psShowFin;
      End;
    psPlayer_Schwarz_Zug,
      psPlayer_Schwarz_Rotieren,
      psPlayer_Weiss_Zug,
      psPlayer_Weiss_Rotieren: Begin
        // Auslesen der Framebuffer Auflösung
        glGetIntegerv(GL_VIEWPORT, @dim[0]);
        w := dim[2];
        h := dim[3];
        // Umrechnen der evtl. Größeren Bildfläche auf den 640x480 Screen
        sx := 640 / w;
        sy := 480 / h;
        x := round(x * sx);
        y := round(y * sy);
        // Ermitteln des geklickten Feldes
        x := x - 320;
        y := y - 240;
        If x < 0 Then
          x := x + FieldSpace Div 2
        Else
          x := x - FieldSpace Div 2;
        If y < 0 Then
          y := y + FieldSpace Div 2
        Else
          y := y - FieldSpace Div 2;
        x := x + 6 * ItemSize;
        y := y + 6 * ItemSize;
        x := x Div ItemSize;
        y := y Div ItemSize;
        x := x - 3;
        y := y - 3;
        //form1.Caption := format('%.4d x %.4d', [x, y]);
        If (x >= 0) And (x <= 5) And (y >= 0) And (y <= 5) Then Begin // Lazarus mekert sonst rum
          FStep(x, y, ssleft In Shift);
        End;
      End;
  End;
End;

Initialization

{$I pentago.ressource}

End.

