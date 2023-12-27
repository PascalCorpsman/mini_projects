(******************************************************************************)
(* utokenizer.pas                                                  ??.??.???? *)
(*                                                                            *)
(* Version     : 0.06                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : <Module_description>                                         *)
(*                                                                            *)
(* License     : See the file license.md, located under:                      *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(* Warranty    : There is no warranty, neither in correctness of the          *)
(*               implementation, nor anything other that could happen         *)
(*               or go wrong, use at your own risk.                           *)
(*                                                                            *)
(* Known Issues: none                                                         *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*               0.02 - Conversion to Unicode, Linux, Removed "UseUnderLinux" *)
(*                      inserted Compilerswitches, to do that.                *)
(*               0.03 - Bugfix, Operatoren die Präfixe von anderen Operatoren *)
(*                      sind müssen "sortiert" betrachtet werden.             *)
(*               0.04 - Rule String Erkennung,                                *)
(*                      Wenn AddRule(a,b) gemacht wurde und im String Steht   *)
(*                      ...a...ba..b..                                        *)
(*                      Wird wird das token                                   *)
(*                      a...b...b                                             *)
(*                      ausgegeben                                            *)
(*               0.05 - Anpassungen für 64-Bit Systeme                        *)
(*               0.06 - Fix Bug (mit workaround), letztes Token nicht sauber  *)
(*                      geparst.                                              *)
(*                                                                            *)
(******************************************************************************)
Unit utokenizer;

{$MODE ObjFPC}{$H+}

Interface

Uses Sysutils;

Type

  // Unsere Token Structur, hier könnten natürlich noch jede menge anderer INformationen stehn
  // Bei einem Compiler sind ja auch informationen über den Token , wie Typ usw nötig.
  TToken = Record
    Value: String; // Der Token ansich
    Line: PtrInt; // Die zeile in der der token steht (wird auch als Pointer Missbraucht, muss daher PtrInt sein !)
  End;

  // Unser Typ zum Speichern einer Regel
  TRule = Record
    BeginChar: String; // Die zeichenkette die einen Token einleitet
    EndChar: String; // Die ZeichenKette die einen Token beendet
  End;

  // Pointer auf Array of Token
  TTokenarray = Array Of TToken;

  // Der Eigentliche tokenizer
  TTokenizer = Class
  private
    FOperators: Array Of String; // Auflistung aller Operatoren ( Wenn sie gefunden werden trennen sie den alten Token und fügen diesen und sich selbst in die Tokenliste ein )
    Fseperators: Array Of String; // Auflistung aller Trennsymole die selbst aber nicht als Token aufgeführt werden.
    FRules: Array Of Trule; // Auflistung aller token die Übergeordnet sind, wie z.b. Strings
    Fscanlength: Integer; // Legt fest wie viele Zeichen der Parser im Vorraus Einliest.
    FCaseSensitive: Boolean; // Wenn True dann wird Groß Klein schreibung berücksichtigt.
  public
    Property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
    Constructor Create;
    Destructor Destroy; override;
    Procedure AddOperator(Value: String);
    Procedure AddSeperator(Value: String);
    Procedure AddRule(BeginChar, EndChar: String);
    Procedure ClearRules;
    Procedure ClearOperators;
    Procedure ClearSeperators;
    Function Scan(Value: String): TTokenArray; // Zerlegt den Text in Tokens
  End;

  (*
    ACHTUNG Tokens wird verändert !!

    Die Eingabe Tokenliste sollte aus TUnCommenter stammen und entsprechend mit Lineseperatoren versehen sein !!

    Damit diese Funktion Funktioniert, mus Lineseperator als Operator geadded sein !!!
  *)
Function NormalTokensToLineInfoTokens(Var Tokens: TTokenarray; Lineseperator: Char): TTokenarray; // Rechnet die Tokenliste von scan in eine mit entsprechenden Zeileninformationen um ( dazu mus aber numberline vom uncommenter an sein !!)

Function PtrToPtrInt(p: Pointer): Ptrint Inline;
Function PtrIntToPtr(p: PtrInt): Pointer Inline;

Implementation

Function PtrIntToPtr(p: PtrInt): Pointer Inline;
Begin
  result := {%H-} Pointer(p); // Kommt hier immer noch ein Hinweis, dann hat die Codeformatierung zwischen } und P ein Leerzeichen gemacht, das darf nicht sein.
End;

Function PtrToPtrInt(p: Pointer): Ptrint Inline;
Begin
  result := {%H-} Ptrint(p); // Kommt hier immer noch ein Hinweis, dann hat die Codeformatierung zwischen } und P ein Leerzeichen gemacht, das darf nicht sein.
End;

Function Max(v1, v2: integer): integer;
Begin
  If V1 > v2 Then
    result := v1
  Else
    result := v2;
End;

Constructor TTokenizer.Create;
Begin
  Inherited create;
  setlength(Foperators, 0);
  setlength(Fseperators, 0);
  setlength(FRules, 0);
  FCaseSensitive := false;
End;

Destructor TTokenizer.Destroy;
Begin
  //  Inherited destroy; // Braucht net da von Tobject abgeleitet
  setlength(FRules, 0);
  setlength(Foperators, 0);
  setlength(Fseperators, 0);
End;

Procedure TTokenizer.Addoperator(Value: String);
Var
  i, j: Integer;
  b: Boolean;
  s: String;
Begin
  (* Erst mal Checken ob der Operator bereits Existiert *)
  For i := 0 To High(Foperators) Do Begin
    If FOperators[i] = Value Then exit;
  End;
  setlength(Foperators, high(Foperators) + 2);
  Foperators[high(Foperators)] := Value;
  Fscanlength := max(Fscanlength, length(Value));
  (*
  Wenn ein bereits "geaddeter" Operator ein Präfix des zu addenden Operators ist
  dann wird der nun geaddete Operator nicht erkannt.
  => deswegen sortieren wir die Operatoren entsprechend um.
  *)
  b := True;
  i := -1;
  While b Do Begin
    inc(i);
    b := i < High(Foperators);
    For j := i + 1 To High(Foperators) Do Begin
      If (pos(Foperators[i], Foperators[j]) = 1) Then Begin
        s := Foperators[j];
        Foperators[j] := Foperators[i];
        Foperators[i] := s;
        i := -1;
        b := true;
        break;
      End;
    End;
  End;
End;

Procedure TTokenizer.Addseperator(Value: String);
Begin
  setlength(Fseperators, high(Fseperators) + 2);
  Fseperators[high(Fseperators)] := Value;
  Fscanlength := max(Fscanlength, length(Value));
End;

Procedure TTokenizer.AddRule(BeginChar, EndChar: String);
Begin
  // ohne Version 0.04
  //  Fscanlength := max(Fscanlength, length(BeginChar)); // Merken des Längsten Einführungszeichens unserer Tocken
  //  Fscanlength := max(Fscanlength, length(EndChar)); // Merken des Längsten Beendenzeichens unserer Tocken

  // mit Version 0.04
  Fscanlength := max(Fscanlength, length(BeginChar) + length(EndChar)); // Die Maximale Länge der Tokens wird größer, durch das Vorrauslesen.
  setlength(Frules, high(frules) + 2); // Übernehmen in die Rules Liste
  Frules[high(frules)].BeginChar := BeginChar;
  Frules[high(frules)].EndChar := EndChar;
End;

Procedure TTokenizer.ClearRules;
Begin
  setlength(Frules, 0); // Wieder Löschen der Regeln
End;

Procedure TTokenizer.ClearOperators;
Begin
  setlength(Foperators, 0);
End;

Procedure TTokenizer.ClearSeperators;
Begin
  setlength(Fseperators, 0);
End;

Function TTokenizer.scan(Value: String): TTokenArray;
Var
  erg: TTokenArray; // tmp Variable für die gefundenen Tokens
  token: String; // Speichern der zeichen Kette des Tokens der Gerade gelesen wird.
  akt: String; // Die Aktuell gelesenen Zeichen ( Formatiert )
  akttmp: String; // Die Aktuell gelesenen Zeichen ( Un Formatiert )
  ueber: integer; // Anzahl der im Nächsten Schritt zu überlesenden Zeichen.
  i2: integer; // Zählvariable
  i: integer; // Zählvariable
  b, bb: Boolean; // Schmiermerker
  fall: Boolean; // ist true wenn gerade ein Token geschrieben wurde.
  incase: Integer; // -1 wenn wir uns normal im Lesen befinden <>-1 wenn wir in einer Rule sind.
Begin
  erg := Nil;
  (*
   * Bugfix 0.05: Der Parser macht irgend einen Mist, Und erkennt das letzte Token nicht sauber
   *         Wenn man aber lauter Separatoren anfügt und als letztes dann einen Operator, dann gehts.
   *)
  If high(Fseperators) <> -1 Then Begin
    For i := 0 To (Fscanlength Div length(Fseperators[0])) + 1 Do Begin
      value := value + Fseperators[0];
    End;
  End;
  If high(FOperators) <> -1 Then Begin
    value := value + FOperators[0];
  End;
  (*
   * Bugfix 0.05: Ende
   *)
  setlength(erg, 0); // Initialisieren
  If (High(Frules) <> -1) Or (High(Foperators) <> -1) Or (High(Fseperators) <> -1) Then Begin // Wenn es überhaupt was zu tun gibt.
    incase := -1; // Initialisieren
    // Die ersten Paar Zeichen können auf einen Schlag eingelesen werden.
    If Length(Value) > Fscanlength Then Begin
      akt := copy(value, 1, Fscanlength - 1);
      delete(value, 1, Fscanlength - 1);
    End
    Else Begin
      akt := value;
      Value := '';
    End;
    token := ''; // Initialisieren
    ueber := 0; // Initialisieren
    // Zwischenspeichern des eingelesenen Textes Unformatiert
    akttmp := akt;
    If Not FCaseSensitive Then akt := lowercase(akt); // Formatierung für nicht Case Sensitiv
    While Length(Value) <> 0 Do Begin // Arbeiten so lange es noch einen Zu Lesenden text gibt.
      fall := false;
      While (Length(Value) <> 0) And (Length(akt) < Fscanlength) Do Begin
        // Weiterlesen im Text.
        If Length(Value) <> 0 Then Begin
          If FCaseSensitive Then // Wenn Casesensitive
            akt := akt + Value[1] // Dann wird Akttmp eigentlich sinnlos.
          Else
            akt := akt + lowercase(Value[1]);
          akttmp := akttmp + Value[1]; // Mitziehen von Akttmp
          delete(Value, 1, 1); // Löschen des bereits eingelesenen Textes
        End;
      End;
      //********************************************
      If Incase = -1 Then Begin
        For i := 0 To High(Foperators) Do Begin
          // schaun ob der Beginn unseres Tokens überhaupt geht.
          If length(akt) >= Length(Foperators[i]) Then Begin
            b := true; // Initialisieren
            i2 := 1; // Initialisieren
            While b And (i2 <= length(Foperators[i])) Do Begin // Solange der gelesene Text mit dem Tocken übereinstimmt, lesen
              If FCaseSensitive Then Begin // wieder die Case Sensitiv sache
                If akt[i2] <> Foperators[i][i2] Then b := false; // Wenn die Zeicehn nicht Gleich sind
              End
              Else Begin
                If akt[i2] <> lowercase(Foperators[i][i2]) Then b := false; // Wenn die Zeichen nicht Gleich sind
              End;
              inc(i2); // Weiterzählen auf den nächsten Char
            End;
            If b Then Begin // Wenn wir tatsächlich einen Token erkannt haben
              If Length(Token) <> 0 Then Begin
                setlength(Erg, high(erg) + 3);
                erg[high(erg) - 1].Value := token;
                erg[high(erg)].Value := Foperators[i];
              End
              Else Begin
                setlength(Erg, high(erg) + 2);
                erg[high(erg)].Value := Foperators[i];
              End;
              Token := '';
              ueber := Length(Foperators[i]) - 1;
              fall := true;
              break; // Raus.
            End;
          End;
        End;

        For i := 0 To High(fseperators) Do Begin
          // schaun ob der Beginn unseres Tokens überhaupt geht.
          If length(akt) >= Length(fseperators[i]) Then Begin
            b := true; // Initialisieren
            i2 := 1; // Initialisieren
            While b And (i2 <= length(fseperators[i])) Do Begin // Solange der gelesene Text mit dem Tocken übereinstimmt, lesen
              If FCaseSensitive Then Begin // wieder die Case Sensitiv sache
                If akt[i2] <> fseperators[i][i2] Then b := false; // Wenn die Zeicehn nicht Gleich sind
              End
              Else Begin
                If akt[i2] <> lowercase(fseperators[i][i2]) Then b := false; // Wenn die Zeicehn nicht Gleich sind
              End;
              inc(i2); // Weiterzählen auf den nächsten Char
            End;
            If b Then Begin // Wenn wir tatsächlich einen Token erkannt haben
              If Length(token) <> 0 Then Begin
                setlength(Erg, high(erg) + 2);
                erg[high(erg)].Value := token;
                Token := '';
              End;
              ueber := Length(Fseperators[i]) - 1;
              fall := true;
              break; // Raus.
            End;
          End;
        End;
        For i := 0 To High(Frules) Do
          // schaun ob der Beginn unseres Tokens überhaupt geht.
          If length(akt) >= Length(Frules[i].BeginChar) Then Begin
            b := true; // Initialisieren
            i2 := 1; // Initialisieren
            While b And (i2 <= length(Frules[i].BeginChar)) Do Begin // Solange der gelesene Text mit dem Tocken übereinstimmt, lesen
              If FCaseSensitive Then Begin // wieder die Case Sensitiv sache
                If akt[i2] <> Frules[i].BeginChar[i2] Then b := false; // Wenn die Zeicehn nicht Gleich sind
              End
              Else Begin
                If akt[i2] <> lowercase(Frules[i].BeginChar[i2]) Then b := false; // Wenn die Zeicehn nicht Gleich sind
              End;
              inc(i2); // Weiterzählen auf den nächsten Char
            End;
            If b Then Begin // Wenn wir tatsächlich einen Token erkannt haben
              incase := i; // Merken welcher Token es war
              ueber := Length(Frules[i].BeginChar) - 1;
              // Übernehmen des bis dahin entstandenen Tokens
              If Length(Token) <> 0 Then Begin
                setlength(erg, high(erg) + 2);
                erg[high(erg)].Value := Token;
              End;
              Token := Frules[i].BeginChar; // Der Begin unseres tokens mus nu übernommen werde.
              fall := True;
              break; // Wenn wir ein Token gefunden haben können wir natürlich keine weiteren mehr Finden.
            End;
          End;
      End
      Else Begin // Wenn wir in einer Rule sind
        i := incase;
        // schaun ob das Ende unseres Tokens überhaupt geht.
        If (length(akt) >= Length(Frules[i].EndChar)) Then Begin
          b := true; // Initialisieren
          i2 := 1; // Initialisieren
          While b And (i2 <= length(Frules[i].EndChar)) Do Begin // Solange der gelesene Text mit dem Tocken übereinstimmt, lesen
            If FCaseSensitive Then Begin // wieder die Case Sensitiv sache
              If akt[i2] <> Frules[i].EndChar[i2] Then b := false; // Wenn die Zeicehn nicht Gleich sind
            End
            Else Begin
              If akt[i2] <> lowercase(Frules[i].EndChar[i2]) Then b := false; // Wenn die Zeicehn nicht Gleich sind
            End;
            inc(i2); // Weiterzählen auf den nächsten Char
          End;
          If b Then Begin // Wenn wir tatsächlich einen Token erkannt haben
            // --------------------- Neuer Code Version 0.04 -------------- Anfang
            (*
            Wir dürfen nur dann aus der Regel Raus, wenn nach dem End Token nicht
            sofort ein Start Token Kommt !!
            *)
            i2 := 1;
            bb := true;
            While bb And (i2 <= length(Frules[i].BeginChar)) Do Begin
              If i2 + length(Frules[i].EndChar) > length(akt) Then
                Raise exception.create('Error in Tokenizer (Scan, end rule detection ) , please kontakt the Programmer !!!');
              If FCaseSensitive Then Begin // wieder die Case Sensitiv sache
                If akt[i2 + length(Frules[i].EndChar)] <> Frules[i].BeginChar[i2] Then bb := false; // Wenn die Zeicehn nicht Gleich sind
              End
              Else Begin
                If akt[i2 + length(Frules[i].EndChar)] <> lowercase(Frules[i].BeginChar[i2]) Then bb := false; // Wenn die Zeichen nicht Gleich sind
              End;
              inc(i2); // Weiterzählen auf den nächsten Char
            End;
            // Wenn sofort wieder ein Beginnchar gefunden wurde dann
            // Dürfen wir die Rule niht beenden lassen !!
            If bb Then b := false;
            // Die Rule Endet Tatsächlich Jetzt
            If b Then Begin
              Token := Token + Frules[i].EndChar; // Den Token Vervollständigen
              setlength(erg, high(erg) + 2); // Den token ins Ergebniss anhängen
              erg[high(erg)].Value := Token; // Den token ins Ergebniss anhängen
              Token := ''; // den neuen Token initialisieren.
              incase := -1; // zurücksetzen des Tokenmerkers
              fall := true; // Merken der Fallenden Flanke.
              Ueber := Length(Frules[i].EndChar) - 1; // Berechen der zu Überlesenden zeichen
            End
            Else Begin
              // Die Rule Endet noch nicht es geht noch weiter
              Token := Token + Frules[i].EndChar; // Den Token Vervollständeigen
              Ueber := Length(Frules[i].EndChar) + Length(Frules[i].BeginChar) - 1; // Berechen der zu Überlesenden zeichen
              fall := true; // Merken der Fallenden Flanke = Verhindern das Token erweitert wird, das haben wir ja schon gemacht.
            End;
            // --------------------- Neuer Code Version 0.04 -------------- Ende
            {
            // ALTER Code der ohne  Version 0.04 ist !!
            Token := Token + Frules[i].EndChar; // Den Token Vervollständeigen
            setlength(erg, high(erg) + 2); // Den token ins Ergebniss anhängen
            erg[high(erg)].Value := Token; // Den token ins Ergebniss anhängen
            Token := ''; // den neuen Token initialisieren.
            incase := -1; // zurücksetzen des Tokenmerkers
            fall := true; // Merken der Fallenden Flanke.
            Ueber := Length(Frules[i].EndChar) - 1; // Berechen der zu überlesenden zeichen
            //}
          End;
        End;
      End;
      // Wenn nicht gerade ein Token gefunden wurde dann lesen wir den Aktuellen text in unsere Tokenvariable ein.
      If Not fall Then Token := Token + akttmp[1];
      // Falls Zeichen Überlesen werden müssen machen wir das nun
      If Ueber <> 0 Then Begin
        Delete(akt, 1, ueber); // Der Witz ist das Akt immer Länger oder gleich lang wie Ueber + 1 ist !!
        Delete(akttmp, 1, ueber); // Der Witz ist das Akt immer Länger oder gleich lang wie Ueber + 1 ist !!
        Ueber := 0; // Zurücksetzen
      End;
      // Weiterlesen im Code , durch löschen des 1. elementes des Aktuell gelesenen Codes
      delete(akt, 1, 1);
      delete(akttmp, 1, 1);
    End;
  End;
  (*
  Ist das Letze Zeichen ein Seperator wird dies oben nicht erkannt ...
  *)
  For i := 0 To High(Fseperators) Do
    If Akt = Fseperators[i] Then
      akt := '';
  // Wenn am schlus was Übrig ist mus das noch geadet werden
  If (length(Token) <> 0) Or (length(akt) <> 0) Then Begin
    setlength(erg, high(erg) + 2); // Den token ins Ergebniss anhängen
    erg[high(erg)].Value := Token + akt; // Den token ins Ergebniss anhängen
  End;
  Result := erg; // Zurückgeben des Ergebnisses.
  (*
   * Bugfix 0.05: (siehe erste Zeilen von Scan)
   *)
  setlength(result, high(result));
End;

(*
Diese Funktion Konvertiert eine Tokenliste

Die Tokenliste mus derart sein das immer wieder

Lineseperator dann Linenummer kommt

Klar ist das somit die Allerletzte token eine Linenumber ist.

Auch klar ist, das Lineseperator ein Oparator des Tokenizers sein mus !
*)

Function NormalTokensToLineInfoTokens(Var Tokens: TTokenarray;
  Lineseperator: Char): TTokenarray;
Var
  n, rlc, i: integer;
Begin
  result := Nil;
  rlc := 0;
  // Gab es überhaupt irgendwas zum Parsen ?
  If High(tokens) <> -1 Then Begin
    // Einlesen der Zeilennummern aus den Tokens
    n := -1;
    For i := High(tokens) Downto 1 Do Begin
      (*
      Wird hier eine Access violation geworfen, so entspricht die Tokenliste nicht den Oben genannten definitionen.
      *)
      If tokens[i - 1].value = Lineseperator Then Begin
        n := strtoint(tokens[i].value);
        inc(rlc, 2);
      End;
      If n <> -1 Then
        tokens[i].Line := n;
    End;
    tokens[0].line := n; // Beim 1. Token mus das Manuel gemacht werden.
    // Übernehmen der Daten in die Globale Tokenliste
    Setlength(result, High(tokens) + 1 - rlc);
    n := 0;
    i := 0;
    While i <= High(tokens) Do Begin
      If tokens[i].value <> Lineseperator Then Begin
        Result[n] := tokens[i];
        inc(n);
      End
      Else Begin
        inc(i);
      End;
      inc(i);
    End;
  End
  Else
    setlength(result, 0);
End;

End.

