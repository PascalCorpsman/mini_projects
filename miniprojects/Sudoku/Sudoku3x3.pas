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
Unit Sudoku3x3;

Interface

Uses
  unit6, Dialogs, Forms, usudoku;

Procedure GetKomplettSudoku(Var Data: T3Field); // Löst das Feld mittels BruteForce

Implementation

Type

  Pstack = ^Tstack;

  tStack = Record
    Data: T3field;
    Next: Pstack;
  End;

Var
  Stack: PStack; // Eine Breitensuche ist hier Tödlich !!!

Procedure Mark(Value: t3field; Number: integer);
Var
  s: TSudoku;
Begin
  s := TSudoku.Create(3);
  s.LoadFrom(Value);
  s.Mark(Number);
  s.StoreTo(value);
  s.free;
End;

// Alle Marker Resetten

Procedure ResetAllMarker(Var Data: T3field);
Var
  x, y, z: Integer;
Begin
  For x := 0 To 8 Do
    For y := 0 To 8 Do Begin
      Data[x, y].Marked := false;
      For z := 0 To 8 Do
        Data[x, y].Pencil[z] := true;
    End;
End;

// Hinten Hinzufügen

Procedure Add(Value: T3field);
Var
  bl: PStack;
Begin
  new(bl);
  bl^.next := Nil;
  bl^.Data := value;
  // Wenn man einen Stack nimmt
  If Stack = Nil Then Begin
    stack := bl;
  End
  Else Begin
    bl^.next := stack;
    stack := bl;
  End;
End;

// Ermitteln des Ersten Elementes

Function get: T3field;
Var
  erg: T3field;
  z: PStack;
Begin
  If stack <> Nil Then Begin
    // Wenn man einen Stack nimmt
    z := stack;
    erg := z^.data;
    stack := z^.next;
    dispose(z);
    result := erg;
  End;
End;

// Gibt alles Frei

Procedure Clear;
Var
  z, z1: PStack;
Begin
  z := stack;
  While z <> Nil Do Begin
    z1 := z;
    z := z^.next;
    dispose(z1);
  End;
  stack := Nil;
End;

// Versuch ein Komplettes Feld zu ermitteln

Procedure GetKomplettSudoku(Var Data: T3Field);
Label
  raus;
Var
  actual: T3field;
  x, y, z: Integer;
  sm2, sm, Formclose: boolean;
  starty: Integer;
  fdata: TSudoku;
Begin
  fdata := TSudoku.Create(3);
  fdata.LoadFrom(Data);
  If fdata.IsSolveable() Then Begin
    starty := random(9);
    Formclose := false;
    zwangsabbruch := false;
    If Not form6.visible Then Begin
      Formclose := true;
      form6.show;
    End;
    ResetAllMarker(data); // Reset Aller Marker
    stack := Nil; // Initialisieren des Stack's
    add(data); // Start für die Breitensuche
    // Die Tiefensuche
    While stack <> Nil Do Begin
      // Möglichkeiten für den Forzeitigen Abbruch
      application.ProcessMessages;
      If Zwangsabbruch Then Goto Raus;
      actual := get; // Hohlen des Obersten Elementes
      // Hohlen aller Pencil Daten
      fdata.LoadFrom(actual);
      fdata.ClearAllNumberPencils;
      fdata.StoreTo(actual);
      // Suchen des Feldes Das gerade Betrachtet wird
      // Garantie das wir mindestens 1 Feld Finden, und somit die While Schleife Terminiert !!
      sm := false;
      sm2 := false;
      x := 0; // Beruhigt den Compiler
      y := Starty; // Wir starten immer von der Selben Zufallszeile
      While (y < 9) And Not sm Do Begin
        x := 0;
        While (x < 9) And Not sm Do Begin
          If actual[x, y].value = 0 Then
            sm := true; // Freischalten des Suchens nach weiteren Einträgen, sm sperrt gleichzeiteg das x und y verändert werden und somit ist die position auch schon klar
          If Not sm Then inc(x);
        End;
        If Not sm Then Begin
          inc(y);
          // Da wir nicht unbedingt bei x = 0 = y  anfangen müssen wir in der Lage sein von x = 8 = y nach x = 0 = y zu springen
          If (y = 9) And Not sm2 Then Begin
            y := 0;
            sm2 := true; // Anzeigen das wir den Sprung von 8,8 nach 0,0 gemacht haben nd diesen kein 2. mal machen wollen.
          End;
        End;
      End; //   Es ist Fragwürdig ob das Prüfen was Bringt, aber irgendwie denke ich das es das schon tut
      fdata.LoadFrom(actual);
      If sm And fdata.IsSolveable() Then Begin
        For z := 8 Downto 0 Do
          If Actual[x, y].pencil[z] Then Begin
            Actual[x, y].value := z + 1; // Setzen des Wertes
            // Das Ende Der Breitensuche
            fdata.LoadFrom(actual);
            If fdata.IsFullyFilled Then Begin
              Clear;
              Data := Actual;
            End
            Else Begin
              add(Actual);
            End;
          End;
      End;
    End;
    Raus:
    If Zwangsabbruch Then
      clear;
    If Formclose And form6.visible Then Form6.close;
  End
  Else Begin
    Zwangsabbruch := true;
    showmessage('This Sudoku is impossible to solve');
  End;
  fdata.free;
End;

End.





