(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Sudoku                                                *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit usudoku;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Graphics;

Const
  (* History : 0.01 - 1.11 = not tracked
               1.12 = Fix LCL Crash in unit1.lfm
               1.13 = publish code -> start with refactoring
                      FIX: editing numbers after Puzzle create was not possible anymore
   *)
  Ver = '1.13';

Type

  T3Pencil = Array[0..8] Of Boolean;

  T3SubField = Record
    Value: integer;
    Marked: Boolean;
    Maybeed: Boolean;
    Fixed: Boolean;
    Pencil: T3pencil;
  End;

  T3Field = Array[0..8] Of Array[0..8] Of T3subfield;

  // die Line Pencils welche extra erstellt werden
  TLinepencil = Array[0..17] Of T3Pencil;

  TRenderInfo = Record
    Cursor: TPoint;
    Show_Pencils_Numbers: Boolean;
    Show_Line_Pencil_numbers: Boolean;
    Edit_Line_Pencil_Numbers: Boolean;
    NumberHighLights: Array Of Boolean;
    NumberMarks: Array Of Boolean;
  End;

  TPencil = Array Of Boolean;

  Tfield = Array Of Array Of Record
    Value: integer;
    Marked: Boolean;
    Maybeed: Boolean;
    Fixed: Boolean;
    Pencil: TPencil;
  End;

  { TSudoku }

  TSudoku = Class
  private
    fField: Tfield;
    fDim, fsqrDim: integer;
  public
    Constructor Create(Dimension: integer); virtual;
    Destructor Destroy; override;

    Procedure Rotate(Clockwise: Boolean);
    Procedure Mirror(Horizontal: Boolean);

    Procedure ClearField;

    Procedure Mark(Number: integer);
    Procedure ResetAllMarker;

    Function IsMarked(x, y: Integer): Boolean;
    Function IsFullyFilled(): Boolean; // True, wenn alle Value's <> 0, ohne Checks ob tatsächlich gültig (nur für Algorithmen geeignet)
    Function IsSolved(): Boolean; // True, wenn das Sudoku tatsächlich korrekt gelöst ist

    Procedure SetValue(x, y, Value: integer; Fixed: Boolean);

    Procedure RenderTo(Const Canvas: TCanvas; Info: TRenderInfo);

    Procedure ClearAllNumberPencils;

    (* All following functions are needed during refactoring -> Shall be deleted in future *)
    Procedure LoadFrom(Const f: T3Field);
    Procedure StoreTo(Out f: T3Field);
  End;

Var
  Linepencil: TLinepencil; // Die LinienPencil's
  Druckbreite, Breite: Integer; // Globale Variable die die Breite eines Feldes auf dem Spielfeld in Pixeln angibt
  Bretthintergrundfarbe1: Tcolor; // Optische eigenschaften des Spieles
  Bretthintergrundfarbe2: Tcolor; // Optische eigenschaften des Spieles
  Maybeedcolor: Tcolor; // Optische eigenschaften des Spieles
  MarkedColor1: Tcolor; // Optische eigenschaften des Spieles
  MarkedColor2: Tcolor; // Optische eigenschaften des Spieles
  CursorMarker: Tcolor; // Optische eigenschaften des Spieles
  Fixedcolor: Tcolor; // Optische eigenschaften des Spieles
  Gitterfarbe: Tcolor; // Optische eigenschaften des Spieles
  FontColor: Tcolor; // Optische eigenschaften des Spieles
  Pencilcolor: Tcolor; // Optische eigenschaften des Spieles
  PencilcolorMarked: Tcolor; // Optische eigenschaften des Spieles
  LightenColor: Tcolor; // Optische eigenschaften des Spieles
  FormBackground: Tcolor; // Optische eigenschaften des Spieles
  unpencilallow: boolean;
  invalidnallow: boolean;
  substitution: Array[1..9] Of String[1];
  lc: integer; // Für das Line Edit brauchen wir ne Extra Variable


  (*
  Berechnet den Korrespondierenden Punkt zu (x,y) im Feld (0..N-1) x (0..N-1) gespiegelt an
  der Achse Direction ( 0..3 )
  *)
Function Mirrow(x, y, n, Direction: Integer): TPoint;

Function PencilEqual(Const a, b: TPencil): Boolean;

Implementation

Function Mirrow(x, y, n, Direction: Integer): TPoint;
Begin
  n := n - 1;
  Case Direction Of
    0: Begin // an der Waagrechten Spiegeln
        result.x := x;
        result.y := n - y;
      End;
    1: Begin // an der 1. Winkelhalbierenden
        result := point(y, x);
      End;
    2: Begin // and der Senkrechten
        result.x := n - x;
        result.y := y;
      End;
    3: Begin // an der 2. Winkelhalbierenden
        result := point(n - y, n - x);
      End
  Else // Fehlerfall
    result := point(x, y);
  End;
End;

// Gibt True zurück wenn die beiden Pencil's gleich sind

Function PencilEqual(Const a, b: TPencil): Boolean;
Var
  i: integer;
Begin
  result := false;
  If high(a) <> high(b) Then exit;
  For i := 0 To high(a) Do
    If a[i] <> b[i] Then Begin
      exit;
    End;
  result := true
End;

{ TSudoku }

Constructor TSudoku.Create(Dimension: integer);
Var
  i, j: Integer;
Begin
  Inherited Create;
  fDim := Dimension;
  fsqrDim := fDim * fDim;
  setlength(fField, fsqrDim, fsqrDim);
  For i := 0 To fsqrDim - 1 Do Begin
    For j := 0 To fsqrDim - 1 Do Begin
      setlength(fField[i, j].Pencil, fsqrDim);
    End;
  End;
  ClearField;
End;

Destructor TSudoku.Destroy;
Var
  i, j: Integer;
Begin
  For i := 0 To fsqrDim - 1 Do Begin
    For j := 0 To fsqrDim - 1 Do Begin
      setlength(fField[i, j].Pencil, 0);
    End;
  End;
  setlength(fField, 0, 0);
End;

Procedure TSudoku.Rotate(Clockwise: Boolean);
Var
  tmp: Tfield;
  x, y, z: integer;
Begin
  tmp := Nil;
  setlength(tmp, fsqrDim, fsqrDim);
  For x := 0 To fsqrDim - 1 Do Begin
    For y := 0 To fsqrDim - 1 Do Begin
      tmp[x, y].Value := ffield[x, y].value;
      tmp[x, y].Marked := ffield[x, y].Marked;
      tmp[x, y].Maybeed := ffield[x, y].Maybeed;
      tmp[x, y].Fixed := ffield[x, y].Fixed;
      setlength(tmp[x, y].Pencil, fsqrDim);
      For z := 0 To fsqrDim - 1 Do Begin
        tmp[x, y].Pencil[z] := ffield[x, y].pencil[z];
      End;
    End;
  End;
  If Clockwise Then Begin
    // Überschreiben des Orginales mit der Gedrehten Kopie
    For x := 0 To fsqrDim - 1 Do Begin
      For y := 0 To fsqrDim - 1 Do Begin
        ffield[x, y].Value := tmp[y, fsqrDim - 1 - x].value;
        ffield[x, y].Marked := tmp[y, fsqrDim - 1 - x].Marked;
        ffield[x, y].Maybeed := tmp[y, fsqrDim - 1 - x].Maybeed;
        ffield[x, y].Fixed := tmp[y, fsqrDim - 1 - x].Fixed;
        For z := 0 To fsqrDim - 1 Do Begin
          ffield[x, y].Pencil[z] := tmp[y, fsqrDim - 1 - x].pencil[z];
        End;
        setlength(tmp[y, fsqrDim - 1 - x].Pencil, 0);
      End;
    End;
  End
  Else Begin
    For x := 0 To fsqrDim - 1 Do Begin
      For y := 0 To fsqrDim - 1 Do Begin
        ffield[x, y].Value := tmp[fsqrDim - 1 - y, x].value;
        ffield[x, y].Marked := tmp[fsqrDim - 1 - y, x].Marked;
        ffield[x, y].Maybeed := tmp[fsqrDim - 1 - y, x].Maybeed;
        ffield[x, y].Fixed := tmp[fsqrDim - 1 - y, x].Fixed;
        For z := 0 To fsqrDim - 1 Do Begin
          ffield[x, y].Pencil[z] := tmp[fsqrDim - 1 - y, x].pencil[z];
        End;
        setlength(tmp[fsqrDim - 1 - y, x].Pencil, 0);
      End;
    End;
  End;
  setlength(tmp, 0, 0);
End;

Procedure TSudoku.Mirror(Horizontal: Boolean);
Var
  tmp: Tfield;
  x, y, z: integer;
Begin
  tmp := Nil;
  setlength(tmp, fsqrDim, fsqrDim);
  For x := 0 To fsqrDim - 1 Do Begin
    For y := 0 To fsqrDim - 1 Do Begin
      tmp[x, y].Value := ffield[x, y].value;
      tmp[x, y].Marked := ffield[x, y].Marked;
      tmp[x, y].Maybeed := ffield[x, y].Maybeed;
      tmp[x, y].Fixed := ffield[x, y].Fixed;
      setlength(tmp[x, y].Pencil, fsqrDim);
      For z := 0 To fsqrDim - 1 Do Begin
        tmp[x, y].Pencil[z] := ffield[x, y].pencil[z];
      End;
    End;
  End;
  If Horizontal Then Begin
    For x := 0 To fsqrDim - 1 Do Begin
      For y := 0 To fsqrDim - 1 Do Begin
        ffield[x, y].Value := tmp[fsqrDim - 1 - x, y].value;
        ffield[x, y].Marked := tmp[fsqrDim - 1 - x, y].Marked;
        ffield[x, y].Maybeed := tmp[fsqrDim - 1 - x, y].Maybeed;
        ffield[x, y].Fixed := tmp[fsqrDim - 1 - x, y].Fixed;
        For z := 0 To fsqrDim - 1 Do Begin
          ffield[x, y].Pencil[z] := tmp[fsqrDim - 1 - x, y].pencil[z];
        End;
        setlength(tmp[fsqrDim - 1 - x, y].Pencil, 0);
      End;
    End;
  End
  Else Begin
    For x := 0 To fsqrDim - 1 Do Begin
      For y := 0 To fsqrDim - 1 Do Begin
        ffield[x, y].Value := tmp[x, fsqrDim - 1 - y].value;
        ffield[x, y].Marked := tmp[x, fsqrDim - 1 - y].Marked;
        ffield[x, y].Maybeed := tmp[x, fsqrDim - 1 - y].Maybeed;
        ffield[x, y].Fixed := tmp[x, fsqrDim - 1 - y].Fixed;
        For z := 0 To fsqrDim - 1 Do Begin
          ffield[x, y].Pencil[z] := tmp[x, fsqrDim - 1 - y].pencil[z];
        End;
        setlength(tmp[x, fsqrDim - 1 - y].Pencil, 0);
      End;
    End;
  End;
  setlength(tmp, 0, 0);
End;

Procedure TSudoku.ClearField;
Var
  i, j, k: Integer;
Begin
  For i := 0 To fsqrDim - 1 Do Begin
    For j := 0 To fsqrDim - 1 Do Begin
      fField[i, j].Value := 0;
      fField[i, j].marked := False;
      fField[i, j].Maybeed := False;
      fField[i, j].Fixed := false;
      For k := 0 To fsqrDim - 1 Do
        fField[i, j].Pencil[k] := false;
    End;
  End;
End;

Procedure TSudoku.Mark(Number: integer);
  Procedure Submark(x, y: Integer);
  Var
    i, j: integer;
  Begin
    // Markieren des subBlock's
    For i := x - x Mod fDim To x - x Mod fDim + (fDim - 1) Do
      For j := y - y Mod fDim To y - y Mod fDim + (fDim - 1) Do
        fField[i, j].marked := True;
    // Markieren Waagrecht, Senkrecht
    For i := 0 To fsqrDim - 1 Do Begin
      fField[i, y].marked := true;
      fField[x, i].marked := true;
    End;
  End;
Var
  x, y: Integer;
Begin
  If Number In [1..fsqrDim] Then Begin
    // Markieren
    For x := 0 To fsqrDim - 1 Do Begin
      For y := 0 To fsqrDim - 1 Do Begin
        If fField[x, y].Value = Number Then
          Submark(x, y);
        // Alle Bereits eingetragenen Nummern müssen auch markiert werden !!
        If fField[x, y].Value <> 0 Then fField[x, y].Marked := True;
      End;
    End;
  End;
End;

Procedure TSudoku.ResetAllMarker;
Var
  i, j, k: Integer;
Begin
  For i := 0 To fsqrDim - 1 Do Begin
    For j := 0 To fsqrDim - 1 Do Begin
      fField[i, j].Marked := false;
      For k := 0 To fsqrDim - 1 Do Begin
        fField[i, j].Pencil[k] := true;
      End;
    End;
  End;
End;

Function TSudoku.IsMarked(x, y: Integer): Boolean;
Begin
  result := (fField[x, y].Value <> 0) Or (fField[x, y].Marked);
End;

Function TSudoku.IsFullyFilled(): Boolean;
Var
  i, j: Integer;
Begin
  result := false;
  For i := 0 To fsqrDim - 1 Do Begin
    For j := 0 To fsqrDim - 1 Do Begin
      If fField[i, j].Value = 0 Then exit;
    End;
  End;
  result := true;
End;

Function TSudoku.IsSolved(): Boolean;
Var
  i, j, x, y, z: Integer;
  penc: TPencil;
Begin
  result := IsFullyFilled();
  If result Then Begin
    penc := Nil;
    setlength(penc, fsqrDim);
    result := false;
    // Schaun Zeilenweise
    For j := 0 To fsqrDim - 1 Do Begin
      For i := 0 To fsqrDim - 1 Do
        penc[i] := false;
      For i := 0 To fsqrDim - 1 Do Begin
        If penc[fField[i, j].Value - 1] Then exit;
        penc[fField[i, j].Value - 1] := true;
      End;
    End;
    // Schaun Spaltenweise
    For i := 0 To fsqrDim - 1 Do Begin
      For j := 0 To fsqrDim - 1 Do
        penc[j] := false;
      For j := 0 To fsqrDim - 1 Do Begin
        If penc[fField[i, j].Value - 1] Then exit;
        penc[fField[i, j].Value - 1] := true;
      End;
    End;
    // Schaun Blockweise
    For i := 0 To fDim - 1 Do
      For j := 0 To fDim - 1 Do Begin
        For z := 0 To fsqrDim - 1 Do
          penc[z] := false;
        For x := 0 To fDim - 1 Do
          For y := 0 To fDim - 1 Do Begin
            If Penc[fField[i * fDim + x, j * fDim + y].value - 1] Then exit;
            Penc[fField[i * fDim + x, j * fDim + y].value - 1] := true;
          End;
      End;
    result := true;
  End;
End;

Procedure TSudoku.SetValue(x, y, Value: integer; Fixed: Boolean);
Begin
  fField[x, y].Value := Value;
  If value <> 0 Then Begin
    fField[x, y].Fixed := Fixed;
  End
  Else Begin
    fField[x, y].Fixed := false;
  End;
End;

Procedure TSudoku.RenderTo(Const Canvas: TCanvas; Info: TRenderInfo);
Var
  x, y, z, d: integer;
Begin
  // Die LinePencil's
  If info.Edit_Line_Pencil_Numbers Then Begin
    canvas.brush.style := bssolid;
    canvas.brush.color := CursorMarker;
    canvas.Pen.color := CursorMarker;
    If lc < 9 Then Begin
      canvas.rectangle(breite * (lc + 1), 1, breite * (lc + 2), Breite);
    End
    Else Begin
      canvas.rectangle(breite * 10, breite * (lc - 8), Breite * 11, breite * (lc - 7));
    End;
    canvas.brush.style := bsclear;
  End;
  // Malen der Line Pencil geschichten !!
  If info.Show_Line_Pencil_numbers Then Begin
    // Die Waagrechten beschriftungen.
    canvas.font.size := breite Div 5;
    For x := 0 To fsqrDim - 1 Do
      For z := 0 To fsqrDim - 1 Do
        If Linepencil[x][z] Then Begin
          If (Lc = x) And (info.Edit_Line_Pencil_Numbers) Then
            canvas.font.color := PencilcolorMarked
          Else
            canvas.font.color := Pencilcolor;
          Case z Of // TODO: das ist noch nicht abhängig von DIM !
            0..2: Begin
                If info.NumberHighLights[z] Then Begin
                  canvas.brush.color := LightenColor;
                  canvas.pen.color := LightenColor;
                  canvas.brush.style := bssolid;
                  d := (z + 1) Mod 3;
                  If D = 0 Then d := 3;
                  canvas.ellipse(breite * (x + 1) + 2 + (Breite Div 3) * (z Mod 3), {Breite * (y + 1) + } 2 + 0, breite * (x + 1) - 2 + (Breite Div 3) * d, {Breite * (y + 1) } -2 + Breite Div 3);
                  canvas.Brush.style := bsclear;
                End;
                canvas.textout(breite * (x + 1) + (Breite Div 3) * (z Mod 3) + ((Breite Div 3) - canvas.Textwidth(inttostr(z + 1))) Div 2, {Breite * (y + 1) } 1 + 0, substitution[(z + 1)]);
              End;
            3..5: Begin
                If info.NumberHighLights[z] Then Begin
                  canvas.brush.color := LightenColor;
                  canvas.pen.color := LightenColor;
                  canvas.brush.style := bssolid;
                  d := (z + 1) Mod 3;
                  If D = 0 Then d := 3;
                  canvas.ellipse(breite * (x + 1) + 2 + (Breite Div 3) * (z Mod 3), { Breite * (y + 1) + } 2 + breite Div 3, breite * (x + 1) - 2 + (Breite Div 3) * d, { Breite * (y + 1) } -2 + (Breite Div 3) * 2);
                  canvas.Brush.style := bsclear;
                End;
                canvas.textout(breite * (x + 1) + (Breite Div 3) * (z Mod 3) + ((Breite Div 3) - canvas.Textwidth(inttostr(z + 1))) Div 2, { Breite * (y + 1) +} 1 + Breite Div 3, substitution[(z + 1)]);
              End;
            6..8: Begin
                If info.NumberHighLights[z] Then Begin
                  canvas.brush.color := LightenColor;
                  canvas.pen.color := LightenColor;
                  canvas.brush.style := bssolid;
                  d := (z + 1) Mod 3;
                  If D = 0 Then d := 3;
                  canvas.ellipse(breite * (x + 1) + 2 + (Breite Div 3) * (z Mod 3), { Breite * (y + 1) + } 2 + (breite Div 3) * 2, breite * (x + 1) - 2 + (Breite Div 3) * d, { Breite * (y + 1) } -2 + Breite);
                  canvas.Brush.style := bsclear;
                End;
                canvas.textout(breite * (x + 1) + (Breite Div 3) * (z Mod 3) + ((Breite Div 3) - canvas.Textwidth(inttostr(z + 1))) Div 2, {Breite * (y + 1) +} 1 + (Breite Div 3) * 2, substitution[(z + 1)]);
              End;
          End;
        End;
    // Die senkrechten Beschriftungen.
    canvas.font.size := breite Div 5;
    canvas.font.color := Pencilcolor;
    For x := 0 To fsqrDim - 1 Do
      For z := 0 To fsqrDim - 1 Do
        If Linepencil[x + fsqrDim][z] Then Begin
          If (Lc = x + fsqrDim) And (info.Edit_Line_Pencil_Numbers) Then
            canvas.font.color := PencilcolorMarked
          Else
            canvas.font.color := Pencilcolor;
          Case z Of // TODO: das ist noch nicht abhängig von DIM !
            0..2: Begin
                If info.NumberHighLights[z] Then Begin
                  canvas.brush.color := LightenColor;
                  canvas.pen.color := LightenColor;
                  canvas.brush.style := bssolid;
                  d := (z + 1) Mod 3;
                  If D = 0 Then d := 3;
                  canvas.ellipse(breite * (10) + 2 + (Breite Div 3) * (z Mod 3), Breite * (x + 1) + 2 + 0, breite * (10) - 2 + (Breite Div 3) * d, Breite * (x + 1) - 2 + Breite Div 3);
                  canvas.Brush.style := bsclear;
                End;
                canvas.textout(breite * (10) + (Breite Div 3) * (z Mod 3) + ((Breite Div 3) - canvas.Textwidth(inttostr(z + 1))) Div 2, Breite * (x + 1) + 1 + 0, substitution[(z + 1)]);
              End;
            3..5: Begin
                If info.NumberHighLights[z] Then Begin
                  canvas.brush.color := LightenColor;
                  canvas.pen.color := LightenColor;
                  canvas.brush.style := bssolid;
                  d := (z + 1) Mod 3;
                  If D = 0 Then d := 3;
                  canvas.ellipse(breite * (10) + 2 + (Breite Div 3) * (z Mod 3), Breite * (x + 1) + 2 + breite Div 3, breite * (10) - 2 + (Breite Div 3) * d, Breite * (x + 1) - 2 + (Breite Div 3) * 2);
                  canvas.Brush.style := bsclear;
                End;
                canvas.textout(breite * (10) + (Breite Div 3) * (z Mod 3) + ((Breite Div 3) - canvas.Textwidth(inttostr(z + 1))) Div 2, Breite * (x + 1) + 1 + Breite Div 3, substitution[(z + 1)]);
              End;
            6..8: Begin
                If info.NumberHighLights[z] Then Begin
                  canvas.brush.color := LightenColor;
                  canvas.pen.color := LightenColor;
                  canvas.brush.style := bssolid;
                  d := (z + 1) Mod 3;
                  If D = 0 Then d := 3;
                  canvas.ellipse(breite * (10) + 2 + (Breite Div 3) * (z Mod 3), Breite * (x + 1) + 2 + (breite Div 3) * 2, breite * (10) - 2 + (Breite Div 3) * d, Breite * (x + 1) - 2 + Breite);
                  canvas.Brush.style := bsclear;
                End;
                canvas.textout(breite * (10) + (Breite Div 3) * (z Mod 3) + ((Breite Div 3) - canvas.Textwidth(inttostr(z + 1))) Div 2, Breite * (x + 1) + 1 + (Breite Div 3) * 2, substitution[(z + 1)]);
              End;
          End;
        End;
    // Die linien zwischen den Zahlen
    canvas.pen.color := Gitterfarbe;
    For z := 1 To fsqrDim + 1 Do Begin
      If Z <> fsqrDim + 1 Then Begin
        canvas.moveto(Breite * z, 1);
        canvas.lineto(Breite * z, Breite);
        canvas.moveto(Breite * 10, Breite * z);
        canvas.lineto(Breite * 11, Breite * z);
      End;
      If Z <> 1 Then Begin
        canvas.moveto(Breite * z - 1, 1);
        canvas.lineto(Breite * z - 1, Breite);
        canvas.moveto(Breite * 10, Breite * z - 1);
        canvas.lineto(Breite * 11, Breite * z - 1);
      End;
    End;
  End;
  // Malen des Gitters
  For y := 0 To fsqrDim - 1 Do
    For x := 0 To fsqrDim - 1 Do Begin
      Case x Of // TODO: das ist noch nicht abhängig von DIM !
        0..2, 6..8: Begin
            If y In [0..2, 6..8] Then Begin
              canvas.brush.color := Bretthintergrundfarbe1;
              If fField[x, y].marked Then canvas.brush.color := markedColor1;
            End
            Else Begin
              canvas.brush.color := Bretthintergrundfarbe2;
              If fField[x, y].marked Then canvas.brush.color := markedColor2;
            End;
          End;
        3..5: Begin
            If y In [3..5] Then Begin
              canvas.brush.color := Bretthintergrundfarbe1;
              If fField[x, y].marked Then canvas.brush.color := markedColor1;
            End
            Else Begin
              canvas.brush.color := Bretthintergrundfarbe2;
              If fField[x, y].marked Then canvas.brush.color := markedColor2;
            End;
          End;
      End;
      // Farbe zum Markieren des Aktuellen Feldes
      If (x = info.Cursor.x) And (y = info.Cursor.y) And Not info.Edit_Line_Pencil_Numbers Then canvas.brush.color := CursorMarker;
      // Malen des Feldes
      canvas.pen.color := Gitterfarbe;
      canvas.brush.Style := bssolid;
      canvas.rectangle(breite * (x + 1), Breite * (y + 1), breite * (x + 2), Breite * (y + 2));
      // Falls Zahlen Hervorgehoben werden sollen dann geschieht das hier !!
      If (ffield[x, y].Value <> 0) And info.NumberHighLights[ffield[x, y].Value - 1] Then Begin
        //If TToolbutton(form1.findcomponent('ToolButton' + inttostr(ffield[x, y].Value + 10))).down Then Begin
        canvas.brush.color := LightenColor;
        canvas.pen.color := LightenColor;
        canvas.Ellipse(breite * (x + 1) + 1, Breite * (y + 1) + 1, breite * (x + 2) - 1, Breite * (y + 2) - 1);
      End;
      // Malen der Beschriftung der Felder
      canvas.Brush.style := bsclear;
      // Malen der Pencil Zahlen
      If info.Show_Pencils_Numbers And (fField[x, y].value = 0) Then Begin
        canvas.font.size := breite Div 5;
        If ((info.Cursor.x = x) And (info.Cursor.y = y)) Or fField[x, y].marked Then
          canvas.font.color := PencilcolorMarked
        Else
          canvas.font.color := Pencilcolor;
        For z := 0 To fsqrDim - 1 Do
          If fField[x, y].Pencil[z] Then Begin
            Case z Of // TODO: das ist noch nicht abhängig von DIM !
              0..2: Begin
                  If info.NumberHighLights[z] Then Begin
                    canvas.brush.color := LightenColor;
                    canvas.pen.color := LightenColor;
                    canvas.brush.style := bssolid;
                    d := (z + 1) Mod 3;
                    If D = 0 Then d := 3;
                    canvas.ellipse(breite * (x + 1) + 2 + (Breite Div 3) * (z Mod 3), Breite * (y + 1) + 2 + 0, breite * (x + 1) - 2 + (Breite Div 3) * d, Breite * (y + 1) - 2 + Breite Div 3);
                    canvas.Brush.style := bsclear;
                  End;
                  canvas.textout(breite * (x + 1) + (Breite Div 3) * (z Mod 3) + ((Breite Div 3) - canvas.Textwidth(inttostr(z + 1))) Div 2, Breite * (y + 1) + 1 + 0, substitution[(z + 1)]);
                End;
              3..5: Begin
                  If info.NumberHighLights[z] Then Begin
                    canvas.brush.color := LightenColor;
                    canvas.pen.color := LightenColor;
                    canvas.brush.style := bssolid;
                    d := (z + 1) Mod 3;
                    If D = 0 Then d := 3;
                    canvas.ellipse(breite * (x + 1) + 2 + (Breite Div 3) * (z Mod 3), Breite * (y + 1) + 2 + breite Div 3, breite * (x + 1) - 2 + (Breite Div 3) * d, Breite * (y + 1) - 2 + (Breite Div 3) * 2);
                    canvas.Brush.style := bsclear;
                  End;
                  canvas.textout(breite * (x + 1) + (Breite Div 3) * (z Mod 3) + ((Breite Div 3) - canvas.Textwidth(inttostr(z + 1))) Div 2, Breite * (y + 1) + 1 + Breite Div 3, substitution[(z + 1)]);
                End;
              6..8: Begin
                  If info.NumberHighLights[z] Then Begin
                    canvas.brush.color := LightenColor;
                    canvas.pen.color := LightenColor;
                    canvas.brush.style := bssolid;
                    d := (z + 1) Mod 3;
                    If D = 0 Then d := 3;
                    canvas.ellipse(breite * (x + 1) + 2 + (Breite Div 3) * (z Mod 3), Breite * (y + 1) + 2 + (breite Div 3) * 2, breite * (x + 1) - 2 + (Breite Div 3) * d, Breite * (y + 1) - 2 + Breite);
                    canvas.Brush.style := bsclear;
                  End;
                  canvas.textout(breite * (x + 1) + (Breite Div 3) * (z Mod 3) + ((Breite Div 3) - canvas.Textwidth(inttostr(z + 1))) Div 2, Breite * (y + 1) + 1 + (Breite Div 3) * 2, substitution[(z + 1)]);
                End;
            End;
          End;
      End;
      // Malen des Textes des Feldes
      If fField[x, y].Value <> 0 Then Begin
        // Textgröße
        canvas.Font.size := Breite Div 2;
        // Textfarbe
        If fField[x, y].Fixed Then
          canvas.font.color := fixedcolor
        Else
          canvas.font.color := FontColor;
        // Farbe für geratene Felder
        If (fField[x, y].Maybeed) Then canvas.Font.color := Maybeedcolor;
        // Malen des Feldinhaltes
        canvas.textout(breite * (x + 1) + (Breite - canvas.textwidth(inttostr(fField[x, y].value))) Div 2, Breite * (y + 1) + 1 + (Breite - canvas.textheight(inttostr(fField[x, y].value))) Div 2, substitution[(fField[x, y].value)]);
      End;
    End;
End;

Procedure TSudoku.ClearAllNumberPencils;
  Procedure Submark(x, y, Number: Integer);
  Var
    i, j: integer;
  Begin
    // Markieren des 16 er Block's
    For i := x - x Mod fDim To x - x Mod fDim + fDim - 1 Do
      For j := y - y Mod fDim To y - y Mod fDim + fDim - 1 Do
        fField[i, j].Pencil[number - 1] := False;
    // Markeiren Waagrecht, Senkrecht
    For i := 0 To fsqrDim - 1 Do Begin
      fField[i, y].Pencil[number - 1] := False;
      fField[x, i].Pencil[number - 1] := False;
    End;
  End;
Var
  i, j: Integer;
Begin
  {  // Demarkieren der Pencil's
    For i := 0 To 8 Do
      For j := 0 To 8 Do Begin
        For z := 0 To 8 Do
          Value[i, j].Pencil[z] := true;
      End;}
    // Markieren Der Pencil's
  For i := 0 To fsqrDim - 1 Do
    For j := 0 To fsqrDim - 1 Do Begin
      If fField[i, j].value <> 0 Then
        Submark(i, j, fField[i, j].value);
    End;
End;

Procedure TSudoku.LoadFrom(Const f: T3Field);
Var
  i, j, k: Integer;
Begin
  If fdim <> 3 Then Raise Exception.Create('TSudoku.LoadFrom: error dim <> 3');
  For i := 0 To fsqrDim - 1 Do Begin
    For j := 0 To fsqrDim - 1 Do Begin
      fField[i, j].Value := f[i, j].Value;
      fField[i, j].marked := f[i, j].Marked;
      fField[i, j].Maybeed := f[i, j].Maybeed;
      fField[i, j].Fixed := f[i, j].Fixed;
      For k := 0 To fsqrDim - 1 Do
        fField[i, j].Pencil[k] := f[i, j].Pencil[k];
    End;
  End;
End;

Procedure TSudoku.StoreTo(Out f: T3Field);
Var
  i, j, k: Integer;
Begin
  If fdim <> 3 Then Raise Exception.Create('TSudoku.LoadFrom: error dim <> 3');
  For i := 0 To fsqrDim - 1 Do Begin
    For j := 0 To fsqrDim - 1 Do Begin
      f[i, j].Value := fField[i, j].Value;
      f[i, j].Marked := fField[i, j].marked;
      f[i, j].Maybeed := fField[i, j].Maybeed;
      f[i, j].Fixed := fField[i, j].Fixed;
      For k := 0 To fsqrDim - 1 Do
        f[i, j].Pencil[k] := fField[i, j].Pencil[k];
    End;
  End;
End;

End.

