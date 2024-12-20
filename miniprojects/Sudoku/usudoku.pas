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

  Tfield = Array Of Array Of Record
    Value: integer;
    Marked: Boolean;
    Maybeed: Boolean;
    Fixed: Boolean;
    Pencil: Array Of Boolean;
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

    Procedure RenderTo(Const Canvas: TCanvas; Info: TRenderInfo);

    (* All following functions are needed during refactoring -> Shall be deleted in future *)
    Procedure LoadFrom(Const f: T3Field);
    Procedure StoreTo(Var f: T3Field);
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
var
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

Procedure TSudoku.RenderTo(Const Canvas: TCanvas; Info: TRenderInfo);
Var
  x, y, z, d: integer;
Begin
  //rectangle(-1,-1,Breite*10,Breite *10);
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
    For x := 0 To 8 Do
      For z := 0 To 8 Do
        If Linepencil[x][z] Then Begin
          If (Lc = x) And (info.Edit_Line_Pencil_Numbers) Then
            canvas.font.color := PencilcolorMarked
          Else
            canvas.font.color := Pencilcolor;
          Case z Of
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
    For x := 0 To 8 Do
      For z := 0 To 8 Do
        If Linepencil[x + 9][z] Then Begin
          If (Lc = x + 9) And (info.Edit_Line_Pencil_Numbers) Then
            canvas.font.color := PencilcolorMarked
          Else
            canvas.font.color := Pencilcolor;
          Case z Of
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
    For z := 1 To 10 Do Begin
      If Z <> 10 Then Begin
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
  For y := 0 To 8 Do
    For x := 0 To 8 Do Begin
      Case x Of
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
        For z := 0 To 8 Do
          If fField[x, y].Pencil[z] Then Begin
            Case z Of
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

Procedure TSudoku.StoreTo(Var f: T3Field);
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

