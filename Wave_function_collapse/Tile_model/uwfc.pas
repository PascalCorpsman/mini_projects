(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Wave function collapse (tile model)                   *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit uwfc;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, ufilo, Graphics;

Const
  ConLeft = 0;
  ConRight = 1;
  ConUp = 2;
  ConDown = 3;

Type

  TWVCImage = Record
    Prop: integer;
    Connectors: Array[0..3] Of String;

    // Every property below this line is not used by Twfc, it is only declared here to not need to wrap to a nother datafield
    Filename: String;
    Bitmap: TBitmap;
  End;

  TWVCImageArray = Array Of TWVCImage;

  TGridElement = Record
    Index: Integer; // -1 = noch nicht vergeben
    Possibilities: Array Of Boolean;
    PSum: Integer; // Anzahl der Possibilities = true

    Forced: Boolean; // Wenn True, dann wurde dieser Wert über den User gesetzt.
  End;

  TGrid = Array Of Array Of TGridElement;

  TBoolMatrix = Array Of Array Of Boolean;

  TPointList = Array Of TPoint;

  TGridStack = specialize TFilo < TGrid > ;
  { Twfc }

  Twfc = Class
  private
    Images: TWVCImageArray;

    InvalidResult: Boolean;
    InvalidPos: TPoint;
    gs: TGridStack;

    Connections: Array[0..3] Of TBoolMatrix; // Die Verbindungsmatrix der Bilder
    Procedure UpdateGrid(i, j, n: Integer; Const M: TBoolMatrix);
    Procedure SetNum(i, j, n: Integer);
    Function GetLeastProbList(): TPointList;
    Procedure InitGrid();
    Procedure PushGrid();
    Procedure PopGrid();
    Procedure ClearGridstack();

  public
    Grid: TGrid;
    Cancel: Boolean;

    OnUpdate: TNotifyEvent;
    OnRenderTooLong: TNotifyEvent;

    Constructor Create;
    Destructor Destroy; override;

    Procedure SetDimension(ax, ay: Integer);

    Procedure LoadImages(Const aImages: TWVCImageArray);

    Procedure Run();

    Procedure ResetGrid();
    Procedure Clear;
  End;

Implementation

Uses math, forms;

{ Twfc }

Constructor Twfc.Create;
Var
  i: Integer;
Begin
  Images := Nil;
  For i := 0 To 3 Do Begin
    Connections[i] := Nil;
  End;
  Grid := Nil;
  OnUpdate := Nil;
  OnRenderTooLong := Nil;
End;

Destructor Twfc.Destroy;
Begin

End;

Procedure Twfc.SetDimension(ax, ay: Integer);
Begin

End;

Procedure Twfc.Clear;
Begin

End;

Procedure Twfc.UpdateGrid(i, j, n: Integer; Const M: TBoolMatrix);
Var
  x, c: Integer;
Begin
  // Außerhalb des Feldes
  If (i < 0) Or (i >= length(Grid)) Then exit;
  If (j < 0) Or (j >= length(Grid[i])) Then exit;
  c := 0;
  For x := 0 To length(Images) - 1 Do Begin
    If Not M[n, x] Then Grid[i, j].Possibilities[x] := false;
    If Grid[i, j].Possibilities[x] Then Begin
      inc(c);
    End;
  End;
  Grid[i, j].PSum := c;
End;

Procedure Twfc.SetNum(i, j, n: Integer);
Begin
  If Grid[i, j].Index <> -1 Then exit; // Wir versuchen ein bereits existierendes zu "ersetzen"
  Grid[i, j].Index := n;
  Grid[i, j].PSum := 0; // Das Feld ist ja gesetzt
  // Aktualisieren der "Möglichkeiten" der Angrenzenden
  UpdateGrid(i - 1, j, n, Connections[ConLeft]);
  UpdateGrid(i + 1, j, n, Connections[ConRight]);
  UpdateGrid(i, j - 1, n, Connections[ConUp]);
  UpdateGrid(i, j + 1, n, Connections[ConDown]);
End;

Function Twfc.GetLeastProbList: TPointList;
Var
  ls, i, j: integer;
Begin
  ls := length(Images);
  For i := 0 To high(Grid) Do Begin
    For j := 0 To high(Grid[i]) Do Begin
      If (Grid[i, j].Index = -1) Then Begin
        If (Grid[i, j].PSum = 0) Then Begin
          InvalidResult := true; // Das Backtracking anstoßen
          InvalidPos := point(i, j);
        End
        Else Begin
          ls := min(ls, Grid[i, j].PSum);
        End;
      End;
    End;
  End;
  result := Nil;
  For i := 0 To high(Grid) Do Begin
    For j := 0 To high(Grid[i]) Do Begin
      If (Grid[i, j].PSum = ls) And (Grid[i, j].Index = -1) Then Begin
        setlength(result, high(result) + 2);
        result[high(result)] := point(i, j);
      End;
    End;
  End;

End;

Procedure Twfc.InitGrid;
Var
  w, h, i, j, k: Integer;
  hasForced: Boolean;
Begin
  w := length(Grid);
  h := length(Grid[0]);
  hasForced := false;
  For i := 0 To w - 1 Do Begin
    For j := 0 To h - 1 Do Begin
      If Not Grid[i, j].Forced Then Begin // Die User Gesetzten werden natürlich nicht gelöscht !
        Grid[i, j].Index := -1;
      End
      Else Begin
        hasForced := true;
      End;
      setlength(Grid[i, j].Possibilities, length(Images));
      For k := 0 To high(Grid[i, j].Possibilities) Do Begin
        If Images[k].Prop > 0 Then Begin
          Grid[i, j].Possibilities[k] := true;
        End
        Else Begin
          Grid[i, j].Possibilities[k] := false;
        End;
        Grid[i, j].PSum := length(Images);
      End;
    End;
  End;
  // Setzen des / der ersten Feldes /Felder
  If hasForced Then Begin
    // Fall 1: Der User hat eigene Vorgaben gemacht, dann übernehmen wir diese
    For i := 0 To w - 1 Do Begin
      For j := 0 To h - 1 Do Begin
        If Grid[i, j].Forced Then Begin
          k := Grid[i, j].Index;
          Grid[i, j].Index := -1;
          setNum(i, j, k);
        End;
      End;
    End;
  End
  Else Begin
    // Fall 2: Das Feld ist Leer -> Wir setzen ein zufälliges 1. Feld
    i := random(w);
    j := random(h);
    k := -1;
    While k = -1 Do Begin // Sicherstellen, das wir dieses Eine Teil auch verwenden dürfen !
      k := random(length(Images));
      If Images[k].Prop = 0 Then k := -1;
    End;
    setNum(i, j, k);
  End;

End;

Procedure Twfc.PushGrid;
Var
  g: TGrid;
  i, j: Integer;
Begin
  g := Nil;
  setlength(g, length(Grid), length(Grid[0]));
  For i := 0 To high(Grid) Do Begin
    For j := 0 To high(Grid[0]) Do Begin
      g[i, j] := Grid[i, j];
    End;
  End;
  gs.Push(g);

End;

Procedure Twfc.PopGrid;
Var
  g: TGrid;
  i, j: Integer;
Begin
  If gs.IsEmpty Then Begin
    // Der Jump ist derart Riesig, dass wir nichts mehr zum "popen" haben
    InitGrid();
  End
  Else Begin
    g := gs.Pop;
    For i := 0 To high(Grid) Do Begin
      For j := 0 To high(Grid[0]) Do Begin
        Grid[i, j] := g[i, j];
      End;
    End;
    setlength(g, 0, 0);
  End;
End;

Procedure Twfc.ClearGridstack;
//Var
//  g: TGrid;
Begin
  While Not gs.IsEmpty Do Begin
    //    g :=
    gs.Pop;
    //    setlength(g, 0, 0);
  End;
End;

Procedure Twfc.LoadImages(Const aImages: TWVCImageArray);
Begin
  Images := aImages;
End;

Procedure Twfc.Run;
Var
  ac, r, i, j, k, PSum: Integer;
  pl: TPointList;
  a: Array Of Integer;
  BackJumpCounter: integer;
  WasInvalid: Boolean;
  StartTime: QWord;
  allowed_time: QWord;
  Triggered: Boolean;
Begin
  // 1. Connection Matrix Berechnen
  For i := 0 To 3 Do Begin
    setlength(Connections[i], length(Images), length(Images));
  End;
  (* Theoretisch kann jedes Teil mit Jedem Teil verbunden werden, das ganze in alle 4 Richtungen *)
  For i := 0 To high(Images) Do Begin
    For j := 0 To high(Images) Do Begin
      Connections[ConLeft][i, j] := Images[j].Connectors[ConRight] = Images[i].Connectors[ConLeft];
      Connections[ConRight][i, j] := Images[j].Connectors[ConLeft] = Images[i].Connectors[ConRight];
      Connections[ConUp][i, j] := Images[j].Connectors[ConDown] = Images[i].Connectors[ConUp];
      Connections[ConDown][i, j] := Images[j].Connectors[ConUp] = Images[i].Connectors[ConDown];
    End;
  End;

  Cancel := false;
  InitGrid();
  a := Nil;
  setlength(a, length(Images));

  (*
   * Die Theorie sagt folgendes
   * - Suchen aller Felder, welche die "Geringsten" Möglichkeiten haben
   * - Eines dieser Felder zufällig auf eine Möglichkeit setzen -> Repeat until alles besetzt.
   *
   * Eigentlich brüchte man einen Backtrack algorithmus, um immer alles zu füllen, aber ohne geht es meistens auch !
   *
   *)
  InvalidResult := false;
  WasInvalid := false;
  BackJumpCounter := 1;
  gs := TGridStack.create;
  pl := GetLeastProbList();
  StartTime := GetTickCount64;
  Triggered := false;
  allowed_time := min(500, length(Grid) * Length(Grid[0]) * 10);
  While assigned(pl) Do Begin
    // Wählen eines Zufälligen Feldes, aus der Liste derer die Noch Frei sind
    r := random(length(pl));
    i := pl[r].X;
    j := pl[r].Y;
    // Wenn wir ein Teil haben, was "Problematisch" ist dann setzen wir dieses nach dem Backtrack als 1.
    If WasInvalid Then Begin
      WasInvalid := false;
      i := InvalidPos.x;
      j := InvalidPos.Y;
    End;
    // Wählen eines Zufälligen Kandidaten aus der Liste der noch freien Kandidaten
    ac := 0;
    For k := 0 To length(Images) - 1 Do Begin
      If Grid[i, j].Possibilities[k] Then Begin
        a[ac] := k;
        inc(ac);
      End;
    End;
    // Der Versuch das Wahrscheinlichkeitsabhängig zu machen
    PSum := 0;
    For k := 0 To ac - 1 Do Begin
      psum := psum + Images[a[k]].Prop;
    End;
    r := random(psum + 1);
    PSum := 0;
    For k := 0 To ac - 1 Do Begin
      psum := psum + Images[a[k]].Prop;
      If r <= psum Then Begin
        setNum(i, j, a[k]);
        break;
      End;
    End;
    pl := GetLeastProbList();
    If InvalidResult Then Begin
      If assigned(OnUpdate) Then OnUpdate(self);
      For i := 0 To BackJumpCounter - 1 Do Begin
        PopGrid();
      End;
      InvalidResult := false;
      WasInvalid := true;
      pl := GetLeastProbList();
      BackJumpCounter := BackJumpCounter * 2 + 1;
    End
    Else Begin
      PushGrid();
      If BackJumpCounter > 1 Then
        BackJumpCounter := BackJumpCounter - 1;
    End;
    If (GetTickCount64 - StartTime > allowed_time) And Not Triggered Then Begin
      triggered := true;
      If assigned(OnRenderTooLong) Then OnRenderTooLong(self);
    End;
    If assigned(OnUpdate) Then OnUpdate(self);
    // Wenn der User die Animation wieder Abgeschaltet hat, dann muss er hier die Chance bekommen dennoch Cancel zu drücken
    If Triggered Then Begin
      Application.ProcessMessages;
    End;
    If cancel Then pl := Nil;
  End;
  ClearGridstack();
  gs.free;
End;

Procedure Twfc.ResetGrid();
Var
  i, j: Integer;
Begin
  For i := 0 To high(Grid) Do Begin
    For j := 0 To high(Grid[i]) Do Begin
      If Not Grid[i, j].Forced Then Begin
        Grid[i, j].Index := -1;
      End;
    End;
  End;
End;

End.

