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
Unit Sudoku5x5;

Interface

Uses Forms, dialogs;

Type
  T5Pencil = Array[0..24] Of Boolean;

  T5SubField = Record
    Value: integer;
    Fixed: Boolean;
    Marked: Boolean;
    Pencil: T5pencil;
  End;

  T5Field = Array[0..24] Of Array[0..24] Of T5subfield;

Procedure ClearField(Var Value: T5field);
Procedure New5Field(Var Value: T5field; Technik: Integer);
Procedure Mark(Var Value: T5field; Number: integer);
Procedure GetPencil(Var Value: T5field);
Procedure Solve5(Var Value: T5field; Step: boolean; Technik: Integer);
Function isready5(Value: T5field): boolean;
Function isreadyUser5(Value: t5field; Allowmessage: boolean): boolean;

Function Sudoku5Solvable(Value: T5field): Boolean;

Implementation

Uses Unit6; //, unit11, sysutils, windows, dialogs;

Type

  Pstack = ^Tstack;

  tStack = Record
    Data: T5field;
    Next: Pstack;
  End;

Var
  Stack: PStack; // Eine Breitensuche ist hier Tödlich !!!

  // Hinten Hinzufügen

Procedure Add(Value: T5field);
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

Function get: T5field;
Var
  erg: T5field;
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

Procedure ClearField(Var Value: T5field);
Var
  x, y, z: Integer;
Begin
  For x := 0 To 24 Do
    For y := 0 To 24 Do Begin
      Value[x, y].value := 0;
      Value[x, y].fixed := false;
      Value[x, y].Marked := false;
      For z := 0 To 24 Do
        Value[x, y].pencil[z] := true;
    End;
End;

// MArkeirt die Nummern 1.. 24 in Value , Nicht getestet !!

Procedure Mark(Var Value: T5field; Number: integer);
  Procedure Submark(x, y: Integer);
  Var
    xx, yy: integer;
  Begin
    // Markieren des 25 er Block's
    For xx := x - x Mod 5 To x - x Mod 5 + 4 Do
      For yy := y - y Mod 5 To y - y Mod 5 + 4 Do
        Value[xx, yy].marked := True;
    // Markeiren Waagrecht, Senkrecht
    For xx := 0 To 24 Do Begin
      Value[xx, y].marked := true;
      Value[x, xx].marked := true;
    End;
  End;
Var
  x, y, z: Integer;
Begin
  // Demarkieren
  For x := 0 To 24 Do
    For y := 0 To 24 Do Begin
      Value[x, y].Marked := false;
      If Number = 0 Then
        For z := 0 To 24 Do
          Value[x, y].Pencil[z] := true;
    End;
  If Number In [1..25] Then Begin
    // Markieren
    For x := 0 To 24 Do
      For y := 0 To 24 Do Begin
        If Value[x, y].value = Number Then
          Submark(x, y);
        // Alle Bereits eingetragenen Nummern müssen auch markiert werden !!
        If Value[x, y].value <> 0 Then Value[x, y].Marked := True;
      End;
  End;
End;

// Gibt True Zurück wenn das Feld Gelöst ist, Fertig , nicht getestet

Function isready5(Value: T5field): boolean;
Var
  erg: Boolean;
  x, y: Integer;
Begin
  erg := True;
  x := 0;
  y := 0;
  While erg And (y < 25) Do Begin
    If Value[x, y].Value = 0 Then erg := false;
    inc(x);
    If X = 25 Then Begin
      inc(y);
      x := 0;
    End;
  End;
  result := erg;
End;

Function isreadyUser5(Value: t5field; Allowmessage: boolean): boolean;
Var
  erg: Boolean;
  x, y, x1, y1, z: Integer;
  penc: Array[0..24] Of Boolean;
Begin
  erg := isready5(value);
  If erg Then Begin
    // Schaun Zeilenweise
    For y := 0 To 24 Do Begin
      For x := 0 To 24 Do
        penc[x] := false;
      For x := 0 To 24 Do Begin
        If penc[Value[x, y].Value - 1] Then erg := false;
        penc[Value[x, y].Value - 1] := true;
      End;
    End;
    // Schaun Spaltenweise
    For x := 0 To 24 Do Begin
      For y := 0 To 24 Do
        penc[y] := false;
      For y := 0 To 24 Do Begin
        If penc[Value[x, y].Value - 1] Then erg := false;
        penc[Value[x, y].Value - 1] := true;
      End;
    End;
    // Schaun Blockweise
    For x := 0 To 4 Do
      For y := 0 To 4 Do Begin
        For z := 0 To 24 Do
          penc[z] := false;
        For x1 := 0 To 4 Do
          For y1 := 0 To 4 Do Begin
            If Penc[Value[x * 5 + x1, y * 5 + y1].value - 1] Then erg := false;
            Penc[Value[x * 5 + x1, y * 5 + y1].value - 1] := true;
          End;
      End;
    If (Not erg) And Allowmessage Then
      showmessage('You filled out the Sudoku, but not correct');
  End;
  result := erg;
End;

Procedure GetPencil(Var Value: T5field);
  Procedure Submark(x, y, Number: Integer);
  Var
    xx, yy: integer;
  Begin
    // Markieren des 16 er Block's
    For xx := x - x Mod 5 To x - x Mod 5 + 4 Do
      For yy := y - y Mod 5 To y - y Mod 5 + 4 Do
        Value[xx, yy].Pencil[number - 1] := False;
    // Markeiren Waagrecht, Senkrecht
    For xx := 0 To 24 Do Begin
      Value[xx, y].Pencil[number - 1] := False;
      Value[x, xx].Pencil[number - 1] := False;
    End;
  End;
Var
  x, y, z: Integer;
Begin
  // Demarkieren der Pencil's
  For x := 0 To 24 Do
    For y := 0 To 24 Do Begin
      For z := 0 To 24 Do
        Value[x, y].Pencil[z] := true;
    End;
  // Markieren Der Pencil's
  For x := 0 To 24 Do
    For y := 0 To 24 Do Begin
      If Value[x, y].value <> 0 Then
        Submark(x, y, Value[x, y].value);
    End;
End;

Function Sudoku5Solvable(Value: T5field): Boolean;
Var
  sm, erg: Boolean;
  x, y, x1, y1, z: integer;
Begin
  erg := true;
  For z := 1 To 25 Do Begin
    Mark(Value, z);
    For y := 0 To 4 Do
      For x := 0 To 4 Do Begin
        sm := false;
        For y1 := 0 To 4 Do
          For x1 := 0 To 4 Do
            If (Not Value[x * 5 + x1, y * 5 + y1].marked) Or (Value[x * 5 + x1, y * 5 + y1].Value = z) Then sm := true;
        If Not sm Then erg := false;
      End;
  End;
  result := erg;
End;

Procedure SolveTry(Var Value: T5field);
Label
  raus;
Var
  actual: T5field;
  x, y, z: Integer;
  sm2, sm: boolean;
  starty: Integer;
  Asolver: Integer;

  Bloedsin: integer;
  bloedsinn: Boolean;
Begin
  bloedsinn := true;
  Asolver := random(25) + 1;
  Bloedsin := 0;
  starty := random(25);
  stack := Nil; // Initialisieren des Stack's
  add(Value); // Start für die Breitensuche
  // Die Tiefensuche
  While stack <> Nil Do Begin
    // Möglichkeiten für den Forzeitigen Abbruch
    application.ProcessMessages;
    If Zwangsabbruch Then Goto Raus;
    actual := get; // Hohlen des Obersten Elementes
    //  Versuch Anfang
    inc(Bloedsin);
    If Bloedsin Mod 100000 = 0 Then Begin
      bloedsinn := Not bloedsinn;
      Bloedsin := 0;
    End;
    // Versuch ende
    Value := actual; // Debuggen !!!!
    // Hohlen der Pencil Daten
    GetPencil(Actual);
    // Suchen des Feldes Das gerade Betrachtet wird
    // Garantie das wir mindestens 1 Feld Finden, und somit die While Schleife Terminiert !!
    sm := false;
    sm2 := false;
    x := 0; // Beruhigt den Compiler
    y := Starty; // Wir starten immer von der Selben Zufallszeile
    While (y < 25) And Not sm Do Begin
      x := 0;
      While (x < 25) And Not sm Do Begin
        If actual[x, y].value = 0 Then
          sm := true; // Freischalten des Suchens nach weiteren Einträgen, sm sperrt gleichzeiteg das x und y verändert werden und somit ist die position auch schon klar
        If Not sm Then inc(x);
      End;
      If Not sm Then Begin
        inc(y);
        // Da wir nicht unbedingt bei x = 0 = y  anfangen, müssen wir in der Lage sein von x = 25 = y nach x = 0 = y zu springen
        If (y = 25) And Not sm2 Then Begin
          y := 0;
          sm2 := true; // Anzeigen das wir den Sprung von 25,25 nach 0,0 gemacht haben nd diesen kein 2. mal machen wollen.
        End;
      End;
    End;
    // Es ist Fragwürdig ob das Prüfen was Bringt, aber irgendwie denke ich das es das schon tut
    If sm And Sudoku5Solvable(actual) Then Begin
      // (*
      If bloedsinn Then Begin
        z := Asolver - 1;
        While z <> Asolver Do Begin
          If Z <= -1 Then z := 24;
          If Actual[x, y].pencil[z] Then Begin
            Actual[x, y].value := z + 1; // Setzen des Wertes
            //            Solve5(actual,false,511); Erzeugt Fehler
                        // Das Ende Der Breitensuche
            If Isready5(actual) Then Begin
              Clear;
              Value := Actual;
              Application.ProcessMessages;
            End
            Else Begin
              add(Actual);
            End;
          End;
          dec(z);
          If Z <= -1 Then z := 24;
        End;
        dec(asolver);
        If Asolver <= -1 Then Asolver := 24; // *)
      End
      Else Begin
        //(*
        For z := 24 Downto 0 Do Begin
          If Actual[x, y].pencil[z] Then Begin
            Actual[x, y].value := z + 1; // Setzen des Wertes
            //            Solve5(actual,false,511); Erzeugt Fehler
                        // Das Ende Der Breitensuche
            If Isready5(actual) Then Begin
              Clear;
              Value := Actual;
              Application.ProcessMessages;
            End
            Else Begin
              add(Actual);
            End;
          End;
        End; // *)
      End;
    End;
  End;
  Raus:
  If Zwangsabbruch Then Begin
    // Anzeigen das wir nichts gefunden haben !!
    Value[0, 0].value := 0;
    clear;
  End;
End;

Function HiddenSingle(Var Value: T5field; Step: Boolean): Boolean;
Label
  Raus;
Var
  xn, yn, n, z, xx, x, yy, y: integer;
  Weiter: Boolean;
Begin
  xn := -1; // Beruhigt den Compiler
  yn := -1; // Beruhigt den Compiler
  Weiter := false;
  // Die Methode des Hidden Single !!
  z := 1;
  While z < 26 Do Begin
    //    For z := 1 To 16 Do Begin
    Mark(Value, z);
    For xx := 0 To 4 Do
      For yy := 0 To 4 Do Begin
        n := 0;
        For x := 0 To 4 Do
          For y := 0 To 4 Do
            If Not Value[xx * 5 + x, yy * 5 + y].marked Then Begin
              xn := xx * 5 + x;
              yn := yy * 5 + y;
              inc(n);
            End;
        // Wenn wir eine Zahl gefunden Haben
        If n = 1 Then Begin
          Value[xn, yn].value := z;
          weiter := true;
          If Step Then Goto raus;
        End;
      End;
    inc(z);
  End;
  Raus:
  result := Weiter;
End;

Function pencilcount(Value: T5pencil): integer;
Var
  z, erg: integer;
Begin
  erg := 0;
  For z := 0 To 24 Do
    If Value[z] Then inc(erg);
  result := erg;
End;

Function NakedSingle(Var Value: T5field; Step: Boolean): Boolean;
Label
  raus;
Var
  weiter: boolean;
  x, y, z: integer;
Begin
  Repeat
    weiter := false;
    // Zuerst die Pencil Daten rauskriegen
    GetPencil(Value);
    For x := 0 To 24 Do
      For y := 0 To 24 Do
        If (pencilcount(Value[x, y].pencil) = 1) And (Value[x, y].Value = 0) Then Begin
          // Weiter falls endlos
          weiter := true;
          // Zuweisen des Wertes
          For z := 0 To 24 Do
            If Value[x, y].pencil[z] Then Begin
              Value[x, y].value := z + 1;
            End;
          // Abbruch wenn nur step ist
          If Step Then Goto raus;
        End;
  Until Not weiter;
  raus:
  result := weiter;
End;

(*

Value = Zu lösendes Feld
Step = Wenn True dann wird nur ein einziger Wert eingefügt.
Technik :   1 : Hidden Single
            2 : Naked Single
            4 :
*)

Procedure Solve5(Var Value: T5field; Step: boolean; Technik: Integer);
Label
  Raus;
Var
  weiter: Boolean;
  x, y: integer;
Begin
  Repeat
    Weiter := false;
    If Technik And 1 = 1 Then Begin
      weiter := HiddenSingle(Value, step) Or weiter;
    End;
    If (Technik And 2 = 2) And ((Not weiter And Step) Or (Not Step)) Then Begin
      weiter := NakedSingle(Value, step) Or weiter;
    End;
  Until Not weiter Or step;
  raus:
  For x := 0 To 24 Do
    For y := 0 To 24 Do
      Value[x, y].marked := false;
End;

Procedure GetSolvable5Sudoku(Var Data: T5field; Technik: Integer);
Const
  MaxFehlercount = 25;
Var
  x, y, z: Integer;
  weiter: Boolean;
  Versuche: Integer;
  f: T5field;
Begin
  If isready5(Data) Then Begin // zuerst mus geschaut werden ob das Rätsel überhaupt Komplett ist, sonst haben wir eine Endlosschleife
    weiter := true; // Endlosschleife
    Versuche := 0; // Zähler für die Fehlversuche
    // Wegspeichern der Lösung
    For x := 0 To 24 Do
      For y := 0 To 24 Do
        f[x, y].value := Data[x, y].value;
    // Starten mit dem Löschend er Zahlen
    While weiter Do Begin
      Application.ProcessMessages;
      // Rücksetzen des Fieldes auf den zu letzt gefunden Stand
      For x := 0 To 24 Do
        For y := 0 To 24 Do
          Data[x, y].value := F[x, y].value;
      // Suchen des als nächstes zu löschenden Feldes
      x := random(25);
      y := random(25);
      While F[x, y].value = 0 Do Begin
        x := random(25);
        y := random(25);
      End;
      // Löschen des Feldes
      Data[x, y].value := 0;
      // Lösen des Rätsels
      solve5(data, false, Technik);
      // Schaun ob es lösbar war
      If Not isready5(Data) Then
        inc(Versuche) // Wenn nicht dann ist das ein Fehlversuch mehr
      Else Begin // Wenn es lösbar war wird der Wert auch in der Sicherung gelöscht.
        f[x, y].value := 0;
        Versuche := 0;
      End;
      // Abbruch der Endlosschleife
      If (Versuche > MaxFehlercount) Or zwangsabbruch Then Weiter := false;
    End;
    // Umschreiben der Sicherungskopie in das Ausgabe Field und dann setzen der entsprechenden Value's
    For x := 0 To 24 Do
      For y := 0 To 24 Do Begin
        Data[x, y].value := f[x, y].value;
        Data[x, y].Fixed := Not (Data[x, y].value = 0);
        Data[x, y].marked := false;
        For z := 0 To 24 Do
          Data[x, y].Pencil[z] := false;
      End;
  End;
End;

Procedure New5Field(Var Value: T5field; Technik: Integer);
Label
  raus, Rein;
Const
  Numbercount = 1; // Je Höher Numbercount desto Geringer ist die MAximale Rekursionstiefe, aber um so länger dauert es auch
Var
  x, y, n, z: integer;
Begin
  zwangsabbruch := false;
  Form6.show;
  application.ProcessMessages;
  rein:
  // Löschen des Feldes
  ClearField(Value);
  For n := 1 To Numbercount Do Begin
    x := random(25);
    y := random(25);
    z := random(25) + 1;
    Mark(Value, z);
    While Value[x, y].marked Or (Value[x, y].value <> 0) Do Begin
      application.ProcessMessages;
      x := random(25);
      y := random(25);
      If zwangsabbruch Or Not Sudoku5Solvable(Value) Then Goto raus;
    End;
    Value[x, y].value := z;
    Value[x, y].fixed := True;
    If Not Sudoku5Solvable(Value) Then Goto Rein;
  End;
  If Not Sudoku5Solvable(Value) Then Goto Rein;
  // Via Try and Error Lösen
  SolveTry(Value);
  // Wenn es Geklappt hat dann können wir nun hergehen und das Puzzle erstellen
  If Value[0, 0].value <> 0 Then Begin
    GetSolvable5Sudoku(Value, Technik);
  End;
  Raus:
  If Form6.visible Then form6.close;
  If Zwangsabbruch Then ClearField(Value);
End;

End.

