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

Procedure GetKomplettSudoku(Var Data: T3Field);
Procedure Solvebyxywing(Var Data: T3field);

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

// Braucht ein Field dessen pencil MArker bereits richtig Gesetzt sind, dann führt es die XY-Wing Methode durch und gibt das Ergebnins zurück.

Procedure Solvebyxywing(Var Data: T3field);
Var
  a, b, x1, y1, w, u, c,
    p1, p2, x, y, z, z1, z3: integer;
  penc1, penc2: T3pencil;
Begin
  For x := 0 To 8 Do
    For Y := 0 To 8 Do Begin
      // Methode 1 die xy, xz, yz Felder sind alle in unterschiedlichen 9er Blocks
      // das ganz geht nur wenn wir exakt 2 Penci's haben
      If (GetSetPencilscount(data[x, y].pencil) = 2) And (data[x, y].value = 0) Then Begin
        // ermitteln der beiden pencil werte
        p1 := -1;
        p2 := -1;
        For z := 0 To 8 Do
          If Data[x, y].pencil[z] Then Begin
            If P1 <> -1 Then p2 := z;
            If P2 = -1 Then p1 := z;
          End;
        // Nu wird Waagrecht geschaut ob es ein Tupel gibt das ebenfalls p1 , oder p2 enthällt
        // Z brauch hier net die Zahlen 0 bis x durchlaufen da die eh von einem anderen X gefunden werden.
        For z := 0 To 8 Do Begin
          // Wenn wir ein zweites Feld gefunden haben das ebenfalls 2 Penzil's hat und entweder p1, oder p2 ist da mit drin
          If (data[z, y].value = 0) And (z <> x) And (GetSetPencilscount(Data[z, y].pencil) = 2) And (Data[z, y].pencil[p1] Or Data[z, y].pencil[p2]) And (data[z, y].value = 0) Then
            // Wir müssen auch den Fall ausschliesen das es exakt die selben Pencil werte sind
            If (Data[z, y].pencil[p1] Xor Data[z, y].pencil[p2]) Then Begin
              // nun geht's von x, y ab nach unten und wir suchen ebenfalls ein Tupel
              For z1 := 0 To 8 Do
                If (data[x, z1].value = 0) And (GetSetPencilscount(Data[x, z1].pencil) = 2) And (Data[x, z1].pencil[p1] Or Data[x, z1].pencil[p2] And (z1 <> y)) Then
                  // Wir müssen auch den Fall ausschliesen das es exakt die selben Pencil werte sind
                  If (Data[x, z1].pencil[p1] Xor Data[x, z1].pencil[p2]) Then Begin
                    // nun müssen wir schaun ob der 2. PArameter auch der gleiche ist
                    penc1 := Data[x, z1].pencil;
                    penc2 := Data[z, y].pencil;
                    Penc1[p1] := false;
                    Penc1[p2] := false;
                    Penc2[p1] := false;
                    Penc2[p2] := false;
                    // es hat tatsächlich geklappt
                    If PencilEqual(penc1, penc2) And (GetSetPencilscount(penc1) = 1) Then Begin
                      // Das ist klar das das immer geht
                      For z3 := 0 To 8 Do
                        If Penc1[z3] Then Begin
                          Data[z, z1].pencil[z3] := false;
                        End;
                    End;
                  End;
              // aber nicht nur von x,y aus sondern auch von z aus
              For z1 := 0 To 8 Do
                If (data[z, z1].value = 0) And (GetSetPencilscount(Data[z, z1].pencil) = 2) And (Data[z, z1].pencil[p1] Or Data[z, z1].pencil[p2] And (z1 <> y)) Then
                  // Wir müssen auch den Fall ausschliesen das es exakt die selben Pencil werte sind
                  If (Data[z, z1].pencil[p1] Xor Data[z, z1].pencil[p2]) Then Begin
                    // nun müssen wir schaun ob der 2. PArameter auch der gleiche ist
                    penc1 := Data[z, z1].pencil;
                    penc2 := Data[x, y].pencil;
                    Penc1[p1] := false;
                    Penc1[p2] := false;
                    Penc2[p1] := false;
                    Penc2[p2] := false;
                    // es hat tatsächlich geklappt
                    If PencilEqual(penc1, penc2) And (GetSetPencilscount(penc1) = 1) Then Begin
                      // Das ist klar das das immer geht
                      For z3 := 0 To 8 Do
                        If Penc1[z3] Then Begin
                          Data[x, z1].pencil[z3] := false;
                        End;
                    End;
                  End;
              // Wir haben nun die Klassischen Fälle abgearbeitet jetzt gehtz an den Sonderfall
              // Zuerst mus aber sichergestellt werden das wir nicht im Selbe 9er Block sind.
              If (x Div 3) <> (z Div 3) Then Begin
                // wir suchen einen Passenden in dem 9er block von x,y
                a := x - x Mod 3;
                b := y - y Mod 3;
                For x1 := a To a + 2 Do
                  For y1 := b To b + 2 Do
                    If {Not ((x = x1)) And }(y <> y1) And (GetSetPencilscount(Data[x1, y1].pencil) = 2) And ((data[x1, y1].pencil[p1]) Or (data[x1, y1].pencil[p2])) Then
                      If (data[x1, y1].pencil[p1] Xor data[x1, y1].pencil[p2]) Then Begin
                        // nun müssen wir schaun ob der 2. PArameter auch der gleiche ist
                        penc1 := Data[z, y].pencil;
                        penc2 := Data[x1, y1].pencil;
                        Penc1[p1] := false;
                        Penc1[p2] := false;
                        Penc2[p1] := false;
                        Penc2[p2] := false;
                        // es hat tatsächlich geklappt
                        If PencilEqual(penc1, penc2) And (GetSetPencilscount(penc1) = 1) Then Begin
                          // Hohlen der Pencil Zahl Z
                          u := -1;
                          For w := 0 To 8 Do
                            If Penc1[w] Then u := w;
                          c := z - z Mod 3;
                          For w := 0 To 2 Do Begin
                            data[a + w, y].pencil[u] := false;
                            Data[c + w, y1].pencil[u] := false;
                          End;
                        End;
                      End;
                // wir suchen einen Passenden in dem 9er block von z,y, brauchen wir net machen das erledigen diverse schleifen schon
              End;
            End;
        End;
        // Wir schauen ob es senkrecht zu unserem gefundenen 2 er Feld
        For z := 0 To 8 Do
          // Wenn wir ein Feld gefunden haben das IN der Senkrechten ist und genau nur einen der PEncil's gleich hat
          If (z <> y) And (GetSetPencilscount(data[x, z].pencil) = 2) And (data[x, z].value = 0) And ((Data[x, z].pencil[p1] Xor Data[x, z].pencil[p2])) Then Begin
            // Dann schauen wir nach einem 2. Feld im 9er Block x , y nach dem 3.ten Feld
            a := x - x Mod 3;
            b := y - y Mod 3;
            For x1 := a To a + 2 Do
              For y1 := b To b + 2 Do
                // Wir haben ein drittes Feld gefunden das passen könte
                If Not (x1 = x) And (GetSetPencilscount(data[x1, y1].pencil) = 2) And (data[x1, y1].pencil[p1] Xor data[x1, y1].pencil[p2]) Then Begin
                  // nun müssen wir schaun ob der 2. PArameter auch der gleiche ist
                  penc1 := Data[x, z].pencil;
                  penc2 := Data[x1, y1].pencil;
                  Penc1[p1] := false;
                  Penc1[p2] := false;
                  Penc2[p1] := false;
                  Penc2[p2] := false;
                  // es hat tatsächlich geklappt
                  If PencilEqual(penc1, penc2) And (GetSetPencilscount(penc1) = 1) Then Begin
                    u := -1;
                    For w := 0 To 8 Do
                      If Penc1[w] Then u := w;
                    c := z - z Mod 3;
                    For w := 0 To 2 Do Begin
                      Data[x, b + w].pencil[u] := false;
                      Data[x1, c + w].pencil[u] := false;
                    End;
                  End;
                End;
            // Dann schauen wir nach einem 2. Feld im 9er Block x , z nach dem 3.ten Feld, brauchen wir net da z von 0 bis 8 läuft ;)
          End;
      End;
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


