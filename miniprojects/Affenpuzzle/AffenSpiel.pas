(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Affenpuzzle                                           *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit AffenSpiel;

{$MODE ObjFPC}{$H+}

Interface

Uses uFilo, sysutils;

Type

  // Spezifikation der Teile [ 1.. 9 ], wobei 0 Gleich Kein Teil!!
  TTeilNummer = Byte;

  // Spezifikation der Bildchen auf den Teilen
  // Wobei die Pfeilspitzen Positiov sind
  // und die Pfeil enden negativ.
  // Immer ein Motiv hat den selben Plus und Minus Wert
  // Ohne Zusatz  = 1, -1
  // Dreieck = 2, -2
  // Viereck = 3, -3
  // Kreis = 4, -4
  // 0 wird nicht benutzt.
  TTeilItem = Shortint; // [-4..4]

  // Tdrehung * 90 ergibt die Drehung der Teile im Uhrzeigersinn.
  TDrehung = Byte; // [0..3]

  // Vier kanten pro Teil also 4 Teilitems wobei
  // obere Kante = 0 und dann im Uhrzeigersinn rum.
  TTeilItems = Array[0..3] Of Tteilitem;

  // Typendefinietion eines Teiles
  TTeil = Record
    // Teilnummer gibt die Nummer des Teis an
    Teil: Tteilnummer;
    // Sind die Bildchen an den Kanten der Teile
    // Erzeugt im Uhrzeigersinn beginnend Oben.
    Teilitems: Tteilitems;
    // Unser "Pointer" auf das Nächste Teil
    NaechstesTeil: Tteilnummer;
    // Die Angabe wie das Teil gedreht ist. wird nur für die
    // Ausgabe gebraucht.
    Drehung: Tdrehung;
    // Wenn True dann ist das Teil der Start der TeileKette
    IstStart: Boolean;
    // Wenn True dann wird das Teil in der Teilekette benutzt.
    WirdBenutzt: Boolean;
  End;

  //Typ für unser Feld
  TField = Array[1..9] Of Tteil;

  // Die Procedure die ein gültiges Feld findet.
Procedure Findefelder(Var Feld: Tfield); // Das Aktuel zu untersuchende Feld
// Erzeugt ein Feld das Leer ist.
Procedure Initialfield(Var Value: Tfield);
//
Procedure Push(Const Value: Tfield);
//
Function PopStack: Tfield;
Function TopStack: Tfield;
//
Function IsEmptyStack: Boolean;

Procedure ClearStack;

Procedure FreeStack;

Implementation

Var
  tmpField: TField;

Type
  TFieldStack = Specialize TFilo < Tfield > ;

Var
  VarStack: TFieldStack = Nil;

Procedure ClearStack;
Begin
  If Assigned(VarStack) Then Begin
    While Not Varstack.isempty Do
      PopStack;
  End;
End;

Procedure FreeStack;
Begin
  If Assigned(VarStack) Then Begin
    ClearStack;
    VarStack.free;
    VarStack := Nil;
  End;
End;

Procedure Push(Const Value: Tfield);
Begin
  If Not Assigned(VarStack) Then Begin
    VarStack := TFieldStack.create;
  End;
  Varstack.Push(Value);
End;

Function PopStack: Tfield;
Begin
  If Assigned(VarStack) Then Begin
    result := Varstack.Pop;
  End
  Else Begin
    Raise exception.create('Error, No Stack Present');
  End;
End;

Function TopStack: Tfield;
Begin
  If Assigned(VarStack) Then Begin
    result := Varstack.top;
  End
  Else Begin
    Raise exception.create('Error, No Stack Present');
  End;
End;

Function IsEmptyStack: Boolean;
Begin
  If Assigned(VarStack) Then Begin
    result := VarStack.isempty;
  End
  Else
    Result := True;
End;

// Dreht ein Teil um 90° im Uhrzeigersinn.

Function Rotateteil(Value: Tteil): Tteil;
Var
  Erg: Tteil; // Ergebniss
  C: Tteilitem; // Zwischenspeicher für das Tauschen.
  i: Integer;
Begin
  Erg := Value; // Initialisieren.
  If Erg.Drehung < 3 Then // anpassen der Graphischen Variable
    Erg.Drehung := Erg.Drehung + 1
  Else
    Erg.Drehung := 0;
  // Rotieren der Felder im Uhrzeigersinn.
  C := Erg.Teilitems[3];
  For I := 2 Downto 0 Do
    Erg.Teilitems[I + 1] := Erg.Teilitems[I];
  Erg.Teilitems[0] := C;
  // Rückgabe
  Result := Erg;
End;

// Konvertiert 4 Zahlenwerte in TTeilItems
// Damit sparen wir uns immer die 4 Code zeilen

Function TeilItems(V1, v2, v3, v4: Shortint): TTeilItems;
Begin
  result[0] := v1;
  result[1] := v2;
  result[2] := v3;
  result[3] := v4;
End;

// Liefert ein Teil im Urzustand zurück
// Im Prinzip ist das auch anders Machbar, Aber ich kann die
// Vorbelegung bei der Implementation der Variablen nicht leiden.
// Also gibts hier halt ein Paar Code Zeilen mehr.
// Auserdem Braucht die Rekursionsprocedur das auch.

Function Nullteil(Value: Integer): Tteil;
Var
  Erg: Tteil;
Begin
  // Anhand des Value wissen wir welches Teil gefragt wird
  // dies wird dann zurückgegeben.
  Erg.Teil := Value;
  Erg.Naechstesteil := 0;
  Erg.Drehung := 0;
  Erg.Iststart := False;
  Erg.Wirdbenutzt := False;
  Case Value Of
    // Teil 1
    1: Erg.Teilitems := TeilItems(-2, 4, 3, -1);
    //    1: Erg.Teilitems := TeilItems(3, 3, 3, 3);
    // Teil 2
    2: Erg.Teilitems := TeilItems(-3, 1, 2, -4);
    // Teil 3
    3: Erg.Teilitems := TeilItems(-3, 1, 2, -1);
    // Teil 4
    4: Erg.Teilitems := TeilItems(-3, 4, 1, -2);
    // Teil 5
    5: Erg.Teilitems := TeilItems(-2, 4, 3, -4);
    // Teil 6
    6: Erg.Teilitems := TeilItems(-2, 1, 3, -4);
    // Teil 7
    7: Erg.Teilitems := TeilItems(-2, 4, 2, -3);
    // Teil 8
    8: Erg.Teilitems := TeilItems(-3, -1, 1, 4)
  Else
    // Teil 9
    Erg.Teilitems := TeilItems(-3, -1, 4, 1);
  End;
  Result := Erg;
End;

(*
In Findefeld müssen die teile immer wieder zurückgesetzt werden.
Da die Felder sich aber durchaus von Nullteil Unterscheiden, muss
hier vorher Bakup erstellt werden, und das neue "Nullteil" aus dem Bakup
geladen werden.
*)

Function NullteilExt(Value: Integer): Tteil;
Begin
  result := tmpField[value];
End;

// Erzeugt ein Feld das Leer ist.
// Getestet, stimmt !!

Procedure Initialfield(Var Value: Tfield);
Var
  i: Integer;
Begin
  For I := 1 To 9 Do
    Value[I] := Nullteil(I);
End;

// ist es möglich Value1 links neben Value2 zu legen, dann
// gibt die Function True zurück.
// Getestet, stimmt !!

Function Waagrecht(Const Value1, Value2: Tteil): Boolean;
Begin
  Result := (Value1.Teilitems[1] + Value2.Teilitems[3]) = 0;
End;

// ist es möglich Value1 über Value2 zu legen, dann
// gibt die Function True zurück.
// Getestet, stimmt !!

Function Senkrecht(Const Value1, Value2: Tteil): Boolean;
Begin
  Result := (Value1.Teilitems[2] + Value2.Teilitems[0]) = 0;
End;

// Die Procedure die ein gültiges Feld findet.
// Es wird in der Procedur erklärt wie sie functionnier.

Procedure Findefeld(Var Feld: Tfield); // Das Aktuel zu untersuchende Feld
Var
  Rekursionstiefe: Integer; // Ermittelte Rekursionstiefe
  Start: Integer; // "Pointer" auf den Anfang
  Teil1, // "Schlepppointer"
  Teil2: Integer; // Schleppointer
  I: Integer; // Zählvariable
  J: Integer; // Zählvariable
  B: Boolean; // Abbruchbedingung ersetz Exit
  Value: Tfield; // Kopie der Eingabedaten.
Begin
  Value := Feld;
  // zuerst müssen wir das Startteil festlegen.
  I := 1; // Initialisieren
  // Initialisieren, bleibt start auf null haben wir ein Problem !!!
  Start := 0;
  While (I <= 9) And (Start = 0) Do Begin
    // Wir haben unser Startteil gefunden.
    If Value[I].Iststart Then
      Start := I;
    Inc(I);
  End;
  Rekursionstiefe := 1; // Initialisiern der Rekursiontiefe
  // Ermitteln der Echten Rekursionstiefe.
  B := True; // Schleife mit Abbruch
  I := Start; // Initialisieren mitr dem Startteil
  While B Do Begin
    B := Value[I].Naechstesteil <> 0;
    // wenn es ein nächstes Teil gibt.
    If B Then Begin
      // übernehmen des nächsten Teiles
      I := Value[I].Naechstesteil;
      // wir sind eine Rekursionstiefe weiter.
      Inc(Rekursionstiefe);
    End;
  End;
  // So werden die Teile nach einander gesucht.
  //
  //    8   3  4
  //    7   1  2
  //    9   5  6
  //
  Case Rekursionstiefe Of
    // Suchen des Teiles Rechts Mitte !!
    1: Begin
        // Wir laufen alle Teile ab und erzeugen deren Drehnungen
        // passt diese Wird eine Neue Rekursion erzeugt.
        For I := 1 To 9 Do Begin
          // wir müssen alle Teile Weglassen die schon benutzt
          // werden
          If Not (Value[I].Wirdbenutzt) Then Begin
            // das Teil i wurde noch nicht benutzt jetzt muss
            // geschaut werden ob wir es anlegen können.
            // Wenn Ja dann mus die Rekursion aufgerufen werden.
            // Wenn nicht nehmen wir eben das nächste Teil im
            // nächsten Schleifendurchlauf.
            J := 0;
            B := True;
            // Schauen ob das Teil irgendwie Gedreht ist.
            While (J <= 3) And B Do Begin
              // Wenn die Zwei Teile Neben einander passen.
              If Waagrecht(Value[Start], Value[I]) Then Begin
                // den Pointer Erzueugen der auf das nächste
                // Teil zeigt.
                Value[Start].Naechstesteil := I;
                Value[I].Wirdbenutzt := True;
                B := False;
                // Aufruf der nächsten Rekursion.
                //  If I=9 then Debuggen(value,90);end if;
                Findefeld(Value);
                // wenn wir das Teil einbauen konnten.
                // steht Value(i).wirdbenutzt auf True
                // dies darf aber in den neuen Fällen nicht
                // mehr sein.
                Value[I] := Nullteilext(I);
              End;
              // Drehen des Teiles.
              If B Then Begin
                // Nur Drehen wenn es sich noch lohnt.
                Value[I] := Rotateteil(Value[I]);
                Inc(J);
              End; // IF
            End; // While
          End; // IF
        End; // For
      End; // Case 1
    // Suchen des Teiles Oben Mitte
    2: Begin
        // Da wir ein Teil suchen das an das Start Teil passt
        // ist die das suchen fast gleich wi in Rekursionstiefe 1
        For I := 1 To 9 Do Begin
          // wir müssen alle Teile Weglassen die schon  benutzt
          // werden
          If Not (Value[I].Wirdbenutzt) Then Begin
            // das Teil i wurde noch nicht benutzt jetzt muss
            // geschaut werden ob wir es anlegen können.
            // Wenn Ja dann mus die Rekursion aufgerufen werden.
            // Wenn nicht nehmen wir eben das nächste Teil im
            // nächsten Schleifendurchlauf.
            J := 0;
            B := True;
            // Schauen ob das Teil irgendwie Gedreht ist.
            While (J <= 3) And B Do Begin
              // Wenn die Zwei Teile übereinander einander
              // passen.
              If Senkrecht(Value[I], Value[Start]) Then Begin
                // den Pointer Erzueugen der auf das nächste
                // Teil zeigt.
                // Das Letze Tei der Kette mus aber auf das
                // neue Teil zeigen.
                // Pointer auf das Zweite Teil hohlen
                Teil1 := Value[Start].Naechstesteil;
                // Zuweisen des nachfolgers von Teil 2
                // auf das neue Teil3
                Value[Teil1].Naechstesteil := I;
                Value[I].Wirdbenutzt := True;
                B := False;
                // Aufruf der nächsten Rekursion.
                //  Debuggen(Value,I);
                Findefeld(Value);
                // wenn wir das Teil einbauen konnten.
                // steht Value(i).wirdbenutzt auf True
                // dies darf aber in den neuen Fällen nicht
                // mehr sein.
                Value[I] := Nullteilext(I);
              End;
              If B Then Begin
                // Nur Drehen wenn es sich noch lohnt.
                // Drehen des Teiles.
                Value[I] := Rotateteil(Value[I]);
                Inc(J);
              End; // IF
            End; // While
          End; // IF
        End; // For
      End; // Case
    //Suchen des Teils Oben Rechts
    3: Begin
        // Auslesen des Teiles das Mitte Rechts liegt.
        Teil1 := Value[Start].Naechstesteil;
        // Auslesen des Teiles Oben Mitte
        Teil2 := Value[Teil1].Naechstesteil;
        // Da wir ein Teil suchen das an das Start Teil passt
        // ist die das suchen fast gleich wi in Rekursionstiefe 1
        For I := 1 To 9 Do Begin
          // wir müssen alle Teile Weglassen die schon  benutzt
          // werden
          If Not (Value[I].Wirdbenutzt) Then Begin
            // das Teil i wurde noch nicht benutzt jetzt muss
            // geschaut werden ob wir es anlegen können.
            // Wenn Ja dann mus die Rekursion aufgerufen werden.
            // Wenn nicht nehmen wir eben das nächste Teil im
            // nächsten Schleifendurchlauf.
            J := 0;
            B := True;
            // Schauen ob das Teil irgendwie Gedreht ist.
            While (J <= 3) And B Do Begin
              // Wenn das Teil über das Rechte Teil passt
              // und wenn es Rechts neben das andere Passt.
              If Senkrecht(Value[I], Value[Teil1]) And
                Waagrecht(Value[Teil2], Value[I]) Then Begin
                // wir haben ein teil gefunden
                // nun mus es in die Kette eingefügt werden.
                // Teil2 ist das Letze Gleid der Kette und
                // bekommt nun das Teil i als Nachfolger.
                Value[Teil2].Naechstesteil := I;
                Value[I].Wirdbenutzt := True;
                B := False;
                // Aufruf der nächsten Rekursion.
                Findefeld(Value);
                // wenn wir das Teil einbauen konnten.
                // steht Value(i).wirdbenutzt auf True
                // dies darf aber in den neuen Fällen nicht
                // mehr sein.
                Value[I] := Nullteilext(I);
              End;
              If B Then Begin
                // Nur Drehen wenn es sich noch lohnt.
                // Drehen des Teiles.
                Value[I] := Rotateteil(Value[I]);
                Inc(J);
              End; // IF
            End; // While
          End; // IF
        End; // For
      End; //Case
    // Suchen des Teils unten Mitte
    4: Begin
        // Da wir ein Teil suchen das an das Start Teil passt
        // ist die das suchen fast gleich wi in Rekursionstiefe 1
        For I := 1 To 9 Do Begin
          // wir müssen alle Teile Weglassen die schon  benutzt
          // werden
          If Not (Value[I].Wirdbenutzt) Then Begin
            // das Teil i wurde noch nicht benutzt jetzt muss
            // geschaut werden ob wir es anlegen können.
            // Wenn Ja dann mus die Rekursion aufgerufen werden.
            // Wenn nicht nehmen wir eben das nächste Teil im
            // nächsten Schleifendurchlauf.
            J := 0;
            B := True;
            // Schauen ob das Teil irgendwie Gedreht ist.
            While (J <= 3) And B Do Begin
              // Wenn die Zwei Teile übereinander einander
              // passen.
              If Senkrecht(Value[Start], Value[I]) Then Begin
                // den Pointer Erzueugen der auf das nächste
                // Teil zeigt.
                // Das Letze Teil der Kette mus aber auf das
                // neue Teil zeigen.
                // Pointer auf das Zweite Teil hohlen
                Teil1 := Start; // teil1
                Teil1 := Value[Teil1].Naechstesteil; //Teil2
                Teil1 := Value[Teil1].Naechstesteil; //Teil3
                Teil1 := Value[Teil1].Naechstesteil; //Teil4
                // Teil1 zeigt nun auf das Letze Teil der
                // Liste.
                // Zuweisen des nachfolgers von Teil 4
                // auf das neue Teil4
                Value[Teil1].Naechstesteil := I;
                Value[I].Wirdbenutzt := True;
                B := False;
                // Aufruf der nächsten Rekursion.
                Findefeld(Value);
                // wenn wir das Teil einbauen konnten.
                // steht Value(i).wirdbenutzt auf True
                // dies darf aber in den neuen Fällen nicht
                // mehr sein.
                Value[I] := Nullteilext(I);
              End;
              If B Then Begin
                // Nur Drehen wenn es sich noch lohnt.
                // Drehen des Teiles.
                Value[I] := Rotateteil(Value[I]);
                Inc(J);
              End; // IF
            End; // While
          End; // IF
        End; // For
      End; // Case
    // Nun suchen wir das Teil unten Rechts
    5: Begin
        // Dazu Brauchen wir das 2. Teil und das 5.te
        // Suchen des teils 5
        Teil1 := Start; // teil1
        Teil2 := Value[Teil1].Naechstesteil; //teil2
        Teil1 := Value[Teil2].Naechstesteil; //teil3
        Teil1 := Value[Teil1].Naechstesteil; //teil4
        Teil1 := Value[Teil1].Naechstesteil; //teil5
        For I := 1 To 9 Do Begin
          // wir müssen alle Teile Weglassen die schon  benutzt
          // werden
          If Not (Value[I].Wirdbenutzt) Then Begin
            // das Teil i wurde noch nicht benutzt jetzt muss
            // geschaut werden ob wir es anlegen können.
            // Wenn Ja dann mus die Rekursion aufgerufen werden.
            // Wenn nicht nehmen wir eben das nächste Teil im
            // nächsten Schleifendurchlauf.
            J := 0;
            B := True;
            // Schauen ob das Teil irgendwie Gedreht ist.
            While (J <= 3) And B Do Begin
              // Wenn das Teil über das Rechte Teil passt
              // und wenn es Rechts neben das andere Passt.
              If Senkrecht(Value[Teil2], Value[I]) And
                Waagrecht(Value[Teil1], Value[I]) Then Begin
                // wir haben ein teil gefunden
                // nun mus es in die Kette eingefügt werden.
                // Teil2 ist das Letze Gleid der Kette und
                // bekommt nun das Teil i als Nachfolger.
                Value[Teil1].Naechstesteil := I;
                Value[I].Wirdbenutzt := True;
                B := False;
                Findefeld(Value);
                // wenn wir das Teil einbauen konnten.
                // steht Value(i).wirdbenutzt auf True
                // dies darf aber in den neuen Fällen nicht
                // mehr sein.
                Value[I] := Nullteilext(I);
              End;
              If B Then Begin
                // Nur Drehen wenn es sich noch lohnt.
                // Drehen des Teiles.
                Value[I] := Rotateteil(Value[I]);
                Inc(J);
              End; // If
            End; // While
          End; // If
        End; // For
      End; // Case
    // Suchen Links Mitte
    6: Begin
        // Da wir ein Teil suchen das an das Start Teil passt
        // ist das suchen fast gleich wie in Rekursionstiefe 1
        For I := 1 To 9 Do Begin
          // wir müssen alle Teile Weglassen die schon  benutzt
          // werden
          If Not (Value[I].Wirdbenutzt) Then Begin
            // das Teil i wurde noch nicht benutzt jetzt muss
            // geschaut werden ob wir es anlegen können.
            // Wenn Ja dann mus die Rekursion aufgerufen werden.
            // Wenn nicht nehmen wir eben das nächste Teil im
            // nächsten Schleifendurchlauf.
            J := 0;
            B := True;
            // Schauen ob das Teil irgendwie Gedreht ist.
            While (J <= 3) And B Do Begin
              // Wenn die Zwei Teile übereinander einander
              // passen.
              If Waagrecht(Value[I], Value[Start]) Then Begin
                // den Pointer Erzueugen der auf das nächste
                // Teil zeigt.
                // Das Letze Teil der Kette mus aber auf das
                // neue Teil zeigen.
                // Pointer auf das Zweite Teil hohlen
                Teil1 := Start; //Teil1
                Teil1 := Value[Teil1].Naechstesteil; //Teil2
                Teil1 := Value[Teil1].Naechstesteil; //Teil3
                Teil1 := Value[Teil1].Naechstesteil; //Teil4
                Teil1 := Value[Teil1].Naechstesteil; //Teil5
                Teil1 := Value[Teil1].Naechstesteil; //Teil6
                // Zuweisen des nachfolgers von Teil 6
                // auf das neue Teil6
                Value[Teil1].Naechstesteil := I;
                Value[I].Wirdbenutzt := True;
                B := False;
                // Aufruf der nächsten Rekursion.
                Findefeld(Value);
                // wenn wir das Teil einbauen konnten.
                // steht Value(i).wirdbenutzt auf True
                // dies darf aber in den neuen Fällen nicht
                // mehr sein.
                Value[I] := Nullteilext(I);
              End;
              If B Then Begin
                // Nur Drehen wenn es sich noch lohnt.
                // Drehen des Teiles.
                Value[I] := Rotateteil(Value[I]);
                Inc(J);
              End; // IF
            End; //While
          End; // IF
        End; // For
      End; // Case
    // Schon fast Fertig wir suchen das Teil Links Oben.
    7: Begin
        // Wir müssen uns die Zeiger auf das Teil 3 und das Teil 7
        // hohlen
        // Suchen des teils 7
        Teil1 := Start; // Teil1
        Teil1 := Value[Teil1].Naechstesteil; //Teil2
        Teil1 := Value[Teil1].Naechstesteil; //Teil3
        Teil2 := Teil1; // Wegspeichern Teil3
        Teil1 := Value[Teil1].Naechstesteil; //Teil4
        Teil1 := Value[Teil1].Naechstesteil; //Teil5
        Teil1 := Value[Teil1].Naechstesteil; //Teil6
        Teil1 := Value[Teil1].Naechstesteil; //Teil7
        For I := 1 To 9 Do Begin
          // wir müssen alle Teile Weglassen die schon  benutzt
          // werden
          If Not (Value[I].Wirdbenutzt) Then Begin
            // das Teil i wurde noch nicht benutzt jetzt muss
            // geschaut werden ob wir es anlegen können.
            // Wenn Ja dann mus die Rekursion aufgerufen werden.
            // Wenn nicht nehmen wir eben das nächste Teil im
            // nächsten Schleifendurchlauf.
            J := 0;
            B := True;
            // Schauen ob das Teil irgendwie Gedreht ist.
            While (J <= 3) And B Do Begin
              // Wenn das Teil über das Linke Teil passt
              // und wenn es Links neben das andere Passt.
              If Senkrecht(Value[I], Value[Teil1]) And
                Waagrecht(Value[I], Value[Teil2]) Then Begin
                // wir haben ein teil gefunden
                // nun mus es in die Kette eingefügt werden.
                // Teil2 ist das Letze Gleid der Kette und
                // bekommt nun das Teil i als Nachfolger.
                //      Debuggen(Value, 22);
                Value[Teil1].Naechstesteil := I;
                Value[I].Wirdbenutzt := True;
                B := False;
                // Aufruf der nächsten Rekursion.
                Findefeld(Value);
                // wenn wir das Teil einbauen konnten.
                // steht Value(i).wirdbenutzt auf True
                // dies darf aber in den neuen Fällen nicht
                // mehr sein.
                Value[I] := Nullteilext(I);
              End;
              If B Then Begin
                // Nur Drehen wenn es sich noch lohnt.
                // Drehen des Teiles.
                Value[I] := Rotateteil(Value[I]);
                Inc(J);
              End; // IF
            End; // While
          End; // IF
        End; // For
      End; // Case
    // Endlich können wir entscheiden ob wir ein Gültiges Puzzle
    // Gefunden haben. denn wenn das Letze Teil passt dann Juhe.
    8: Begin
        // Wir müssen uns die Zeiger auf das Teil 5 und das Teil 7
        // hohlen
        // Suchen des teils 7
        Teil2 := Start; // Hohlen Teil1
        Teil2 := Value[Teil2].Naechstesteil; // Teil2
        Teil2 := Value[Teil2].Naechstesteil; // Teil3
        Teil2 := Value[Teil2].Naechstesteil; // Teil4
        Teil2 := Value[Teil2].Naechstesteil; // Teil5
        Teil1 := Teil2; // Wegspeichern Teil 5
        Teil2 := Value[Teil2].Naechstesteil; // Teil6
        Teil2 := Value[Teil2].Naechstesteil; // Teil7
        // Teil2 zeigt jetzt auf teil 7
        // Teil2 wird nun auf das Teil 5 zugewiesen
        For I := 1 To 9 Do Begin
          // wir müssen alle Teile Weglassen die schon  benutzt
          // werden
          If Not (Value[I].Wirdbenutzt) Then Begin
            // das Teil i wurde noch nicht benutzt jetzt muss
            // geschaut werden ob wir es anlegen können.
            // Wenn Ja dann mus die Rekursion aufgerufen werden.
            // Wenn nicht nehmen wir eben das nächste Teil im
            // nächsten Schleifendurchlauf.
            J := 0;
            B := True;
            // Schauen ob das Teil irgendwie Gedreht ist.
            While (J <= 3) And B Do Begin
              // Wenn das Teil über das Linke Teil passt
              // und wenn es Links neben das andere Passt.
              If Senkrecht(Value[Teil2], Value[I]) And
                Waagrecht(Value[I], Value[Teil1]) Then Begin
                // Suchen des letzen teiles
                Teil1 := Start; // Teil1
                Teil1 := Value[Teil1].Naechstesteil; // Teil2
                Teil1 := Value[Teil1].Naechstesteil; // Teil3
                Teil1 := Value[Teil1].Naechstesteil; // Teil4
                Teil1 := Value[Teil1].Naechstesteil; // Teil5
                Teil1 := Value[Teil1].Naechstesteil; // Teil6
                Teil1 := Value[Teil1].Naechstesteil; // Teil7
                Teil1 := Value[Teil1].Naechstesteil; // Teil8
                Value[Teil1].Naechstesteil := I;
                Value[I].Wirdbenutzt := True;
                B := False;
                // Aufruf der nächsten Rekursion.
                // Wir haben ein Gültiges Feld gefunden.
                // Nun kann die Rekursion aber endgültig
                // Beendet werden.
                // Speichern des Gefundenen Feldes
                Push(Value);

                // wenn wir das Teil einbauen konnten.
                // steht Value(i).wirdbenutzt auf True
                // dies darf aber in den neuen Fällen nicht
                // mehr sein.
                Value[I] := Nullteilext(I);
              End;
              If B Then Begin
                // Nur Drehen wenn es sich noch lohnt.
                // Drehen des Teiles.
                Value[I] := Rotateteil(Value[I]);
                Inc(J);
              End; // IF
            End; // While
          End; // IF
        End; // For
      End
      // Sollte Die Rekurion irgendeinen Mist bauen
      // kommen wir hier rein, das kommt zwar nie vor
      // aber da wir eh einen When Others Block schreiben
      // Müssen weil diese Dämliche Sprache das Verlangt machen
      // wir das eben gleich Richtig.
  Else Begin
      Raise exception.create('Recursion Error');
    End;
  End;
End;

Procedure ResetField(Var Value: Tfield);
Var
  i: Integer;
Begin
  For i := 1 To 9 Do Begin
    value[i].NaechstesTeil := 0;
    value[i].IstStart := false;
    value[i].WirdBenutzt := false;
    value[i].Drehung := 0;
  End;
End;

Procedure Findefelder(Var Feld: Tfield); // Das Aktuel zu untersuchende Feld
Var
  Afield: TField;
  i: Integer;
Begin
  ClearStack;
  // Starten der Rekursion 9 mal da ja jedes Teil in der Mitte sein
  // kann
  tmpField := feld;
  For I := 1 To 9 Do Begin
    Afield := Feld;
    ResetField(AField);
    // Anlegen des Urfeldes.
    Afield[I].Iststart := True;
    // Festlegen des Startfeldes
    Afield[I].Wirdbenutzt := True;
    // Teil wird benutzt.setzen
    // Starten der Rekursion dies mus 9 mal gemacht werden.
    // da wir unsere Rekursion aber von der Mitte aus starten
    // brauchen wir das erste Teil nicht drehen. da das nur
    // Doppelte Felder erzeugen würde.
    // Damit wird wieder zeit gespart.
    Findefeld(Afield);
  End;
End;

End.

