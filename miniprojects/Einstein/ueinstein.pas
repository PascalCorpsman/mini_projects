(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Einstein                                              *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit ueinstein;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, IniFiles;

Const
  // Alles >= 0 sind Einträge aus den Mengen
  Nichts = -1; // Merker für keinen Inhalt, Gültiger Inhalt alles >= 0
  SetName = -2; // Merker für den Namen einer Menge

Var
  EinsteinWidth: integer = 5; // Anzahl der Elemente in einer Menge
  EinsteinHeight: integer = 5; // Anzahl der Mengen

Type

  (*
   * Im Prinzip sind die Einstein Rätsel eine verkappte Version von Sudokus
   * Also kann man auch Sudoku Regeln anwenden.
   *
   * Jede Regel streicht also in ApplyRule zuerst Möglichkeiten weg.
   * und versucht anschließend Eindeutig gewordene Lösungen zu wählen.
   *
   * Das Wegstreichen geschieht jeweils dem Namen der Klasse entsprechend.
   *)

  TMemberSet = Array Of Integer; // Die Grundmenge
  TMemberSetArray = Array Of TMemberSet; // Menge von Grundmengen

  TRuleSelector = // Eine Liste aller Implementierten Regeln, damit diese in den Frames genutzt werden können
  (
    rsUnknown // Für Fehlermeldungen..
    , rsDependsOn // Senkrechte Mengendefinition "Hängt ab"
    , rsDistanceDependsOn // A Hängt von B ab, ist aber um Distance Spalten verschoben
    , rsLeftof // Eine Menge steht irgendwo Links von der anderen
    , rsDistanceLeftOf // A Hängt von B ab, ist aber um Distance Spalten Links verschoben
    , rsRightOf // Eine Menge steht irgendwo Rechts von der Anderen
    , rsDistanceRightOf // A Hängt von B ab, ist aber um Distance Spalten Rechts verschoben
    , rsEliminate // Senkrechte Mengendefinition "Schließt sich aus"
    , rsDistanceEliminate // Senkrechte Mengendefinition "Schließt sich aus" aber um Distance Spalten verschoben
    , rsMultiEliminate // Teilmengen schließen sich gegenseitig aus
    , rsIntDepends // in 2 unterschiedlichen Spalten bedingen zwei Regeln durch eine Zahl differenz (z.B. Olaf hat einen Hund mehr als der der ein Rotes Haus hat)
    // Todo : , rsIntGtDepends // in 2 unterschiedlichen Spalten bedingen zwei Regeln durch eine Zahl differenz (z.B. Olaf hat mindestens einen Hund mehr als der der ein Rotes Haus hat)
    // Todo : , rsDistanceIntDepends // Wie IntDepends, nur dass anstatt des 2. Elements Eine Distanz angegeben werden kann (z.B. Herr Nilson findet zwei Multis mehr als der Cacher direct neben ihm)
    );

  TResolveXYEvent = Function(x, y: integer): String Of Object; // Callback Routine zum Auflösen eines Namens y in der Spalte x
  TLogEvent = Procedure(Logentry: String) Of Object; // Callback für das Logger System

  TEinsteinRuleFrame = Class; // Vorwärtsdeklaration für Callback
  TRule = Class; // Vorwärtsdeklaration für TEinsteinRuleFrame

  TSelfDeleteCallback = Procedure(Sender: TEinsteinRuleFrame) Of Object; // Callback, für den Delete Button einer Rule

  TAttribute = Record // Eine Attributierte Regel hat Beschriftungen und inhalte
    Value: integer;
    LabelText: String;
  End;

  { TEinsteinRuleFrame }

  TEinsteinRuleFrame = Class(TFrame) // So ist es einfacher die Dinger zu verwalten *g*
  protected
    fDataChanged: Boolean; // Zum Merken ob sich was geändert hat.
    Procedure MarkAsChanged(Sender: TObject); // Event der fDataChanged auf True setzt
  public
    Rule: TRuleSelector; // Die Regel in die nachher abgeleitet werden soll
    ResolveXY: TResolveXYEvent; // Zum Auflösen der Beschriftungen
    SelfDeleteCallback: TSelfDeleteCallback; // Zum Anzeigen, dass wir selbst gelöscht werden sollen

    Constructor Create(TheOwner: TComponent); override;

    Procedure SetCaption(Value: String); virtual; abstract; // Überschrift der Regel Setzen
    Procedure RefreshTexts; virtual; abstract; // Anstoßen der Aktualisierung der Beschriftungen
    Function GetSet(Index: integer): TMemberSet; virtual; abstract; // Auslesen einer Menge (für Derive)
    Function ValidRule: Boolean; virtual; abstract; // True, wenn die Eingaben plausibel sind.

    (*
     * Zum initialisieren in OnShow
     *)
    Procedure SetSet(Index: integer; Set_: TMemberSet); virtual; abstract; // Zum Händischen Setzen einer Menge (nur für Form1.OnShow)
    Procedure SetUserHint(Data: String); virtual; abstract; // Zum Händischen setzen des Hints der Regel
    (*
     * Für den Solver
     *)
    Function DeriveRule: TRule; virtual; abstract;
    (*
     * Laden und Speichern der Regel
     *)
    Procedure SaveToFile(Const Ini: TInifile; Index: integer); virtual;
    Procedure LoadFromFile(Const Ini: TInifile; Index: integer); virtual;
    (*
     * Nur für die Attribut Frames
     *)
    Procedure SetAttribute(Index: integer; Value: integer); virtual; abstract;
    Procedure SetAttributeCountAndLabels(Const Labels: Array Of String); virtual; abstract; // Die Anzahl wird durch die Länge des Arrays bestimmt ;)
    (*
     * Für den Komfort
     *)
    Function DataChanged: Boolean; virtual; //True, wenn der Benutzer etwas geändert hat, wird bei Save bzw Load gelöscht
  End;

  TDataItem = Record
    Value: integer; // Der Ermittelte Wert
    Pencils: Array Of boolean; // Noch mögliche Elemente der Grundmenge, dabei entspriucht der Index dem Element und der Wert gibt an ob der wert noch möglich ist.
  End;

  { TRule }

  TRule = Class // Basisklasse, die alle Prototypen bereit hällt
  protected
    Function NakedSingle: boolean; // Wertet die Pencil Daten aus, und Schreibt eindeutige Datensätze in die Felder, sollte nach jedem ApplyRule aufgerufen werden
  public
    Number: integer; // Die Nummer der Regel nur fürs Printing
    Hint: String; // Der vom Benutzer eingegebene Text
    ResolveXY: TResolveXYEvent; // Zum Auflösen des Index in den String
    Constructor Create; virtual;
    Function Print: String; virtual; // Plottet die Regelnummer
    Function ApplyRule: Boolean; virtual; abstract; // Bearbeitet das Datenfeld entsprechend seiner Regel. True, wenn am Datenbestand etwas geändert wurde, sonst false
    Function Plausible: boolean; virtual; abstract; // True solange die jeweilige Regel prinzipiell noch erfüllbar wäre.
  End;

  { TIntDependence }

  TIntDependence = Class(Trule)
  protected
    fElement1Set: integer;
    fElement1Element: integer;
    fElement2Set: integer;
    fElement2Element: integer;
    fDifference: integer;
    fCathegory: integer;
    Function ResolveDifferingElement(SetIndex, Element, difference: integer): integer;
  public
    Constructor Create(Element1, Cathegory, Element2, difference: integer); virtual; reintroduce;
    Function Print: String; override; // Plottet die Regelnummer
    Function ApplyRule: Boolean; override;
    Function Plausible: boolean; override;
  End;

  { TMultiRule }

  TMultiRule = Class(Trule)
  protected
    fPrintSeperator: String; // Der Text der zwischen den beiden Mengen geschrieben wird.
    fSets1: TMemberSetArray; // !! ACHTUNG !! hier ist der 1. index die y-Koordinate die 2. Dimension ist dann die liste der Elemente je Koordinate
    fSets2: TMemberSetArray; // !! ACHTUNG !! hier ist der 1. index die y-Koordinate die 2. Dimension ist dann die liste der Elemente je Koordinate
  public
    Constructor Create; override;
    Function Print: String; override; // Plottet die Regelnummer
    Procedure SetSet(SetTwo: Boolean; Index: integer; Data: TMemberSet); virtual;
  End;

  { TMultiEliminate }

  TMultiEliminate = Class(TMultiRule)
  protected
  public
    Constructor Create; override;
    Function ApplyRule: Boolean; override;
    Function Plausible: boolean; override;
  End;

  { TSetRule }

  TSetRule = Class(TRule) // Regel die eine Grundmenge betrachtet (first, last, member ...)
  protected
    fRule: TMemberSet;
  public
    Constructor Create(Rule: TMemberSet); virtual; reintroduce;
  End;

  { TTwoSetRule }

  TTwoSetRule = Class(TRule) // Regel die 2 Grundmengen betrachten (left of, neighbour ...)
  private
    fRule1: TMemberSet;
    fRule2: TMemberSet;
  protected
    fPrintSeperator: String; // Der Text der zwischen den beiden Mengen geschrieben wird.
  public
    Constructor Create(Rule1, Rule2: TMemberSet); virtual; reintroduce;
    Function Print: String; override;
  End;

  { TTwoSetDistanceRule }

  TTwoSetDistanceRule = Class(TTwoSetRule)
  protected
    fDistance: integer;
    Procedure SetDistance(Const Value: Integer);
  public
    Property Distance: integer read fDistance write SetDistance;
  End;

  { TDependsOn }

  TDependsOn = Class(TSetRule) // Legt die Bedingung einer Spalte fest (etwa der Art : Der Däne trinkt Bier)
  public
    Function ApplyRule: Boolean; override;
    Function Print: String; override;
    Function Plausible: boolean; override;
  End;

  { TDistanceDependsOn }

  TDistanceDependsOn = Class(TTwoSetDistanceRule) // Legt fest, dass die Beiden Regeln genau einen Abstand von Distance haben
  public
    Constructor Create(Rule1, Rule2: TMemberSet); override;
    Function ApplyRule: Boolean; override;
    Function Plausible: boolean; override;
  End;

  { TLeftOf }

  TLeftOf = Class(TTwoSetRule) // Legt fest, dass Rule1 irgendwo Links von Rule2 steht
  public
    Constructor Create(Rule1, Rule2: TMemberSet); override;
    Function ApplyRule: Boolean; override;
    Function Plausible: boolean; override;
  End;

  { TLeftDistanceDependsOf }

  TLeftDistanceDependsOf = Class(TTwoSetDistanceRule) // Legt fest, dass Rule1 genau Distance spalten Links von rule2 steht
  public
    Constructor Create(Rule1, Rule2: TMemberSet); override;
    Function ApplyRule: Boolean; override;
    Function Plausible: boolean; override;
  End;

  { TRightOf }

  TRightOf = Class(TLeftOf) // Legt fest, dass Rule1 irgendwo Rechts von Rule2 steht
  public
    Constructor Create(Rule1, Rule2: TMemberSet); override;
    Function Print: String; override;
  End;

  { TRightDistanceDependsOf }

  TRightDistanceDependsOf = Class(TLeftDistanceDependsOf) // Legt fest, dass Rule1 genau Distance spalten Rechts von rule2 steht
  public
    Constructor Create(Rule1, Rule2: TMemberSet); override;
    Function Print: String; override;
  End;

  { TEliminate }

  TEliminate = Class(TTwoSetRule)
  public
    Constructor Create(Rule1, Rule2: TMemberSet); override;
    Function ApplyRule: Boolean; override;
    Function Plausible: boolean; override;
  End;

  { TDistanceEliminate }

  TDistanceEliminate = Class(TTwoSetDistanceRule) // Legt fest, dass die Beiden sich ausschließenden Regeln genau einen Abstand von Distance haben
  public
    Constructor Create(Rule1, Rule2: TMemberSet); override;
    Function ApplyRule: Boolean; override;
    Function Plausible: boolean; override;
  End;

  { TGuess }

  TGuess = Class(TDependsOn) // Sonderklasse für Annahmen, hier nimmt der Solver einfach immer "Plazierungstatsachen an"
  public
    Active: Boolean;
    Function ApplyRule: Boolean; override;
    Function Print: String; override;
    Procedure UpdateGuess(Guess: TMemberSet);
  End;

  TDataField = Array Of Array Of TDataItem; // Das Spielfeld als Globale variable => das könnte man auch besser realisieren..

Var
  Data: TDataField;

Function TMS(Const Data: Array Of Integer): TMemberSet; // Konvertierungsroutine, zum leichteren initialisiern des Basis Beispiels
Function RuleSelectorToString(value: TRuleSelector): String; // Konvertierungsroutine,
Function StringToRuleSelector(Value: String): TRuleSelector; // Konvertierungsroutine,

Procedure ResetData; // Löscht alles in Data
Function Solve(Const Rules: Array Of TRule; Logevent: TLogEvent; AddHints, IsGuessing: Boolean): Boolean; // Versucht durch Logisches Schließen Data zu lösen, true bei Erfolg.
Function IsSolved: Boolean; // True, wenn alles Fertig ist
Function SetValue(x, y: integer; Value: integer; RaiseException: Boolean = True): Boolean; // Setzt Value auf x,y und löscht entsprechend die Pencils

Procedure Nop; // Debug nur zum Setzen eines Haltepunktes

Implementation

Uses Dialogs;

Procedure Nop; // Debug nur zum Setzen eines Haltepunktes
Begin

End;

Function TMS(Const Data: Array Of Integer): TMemberSet;
Var
  i: Integer;
Begin
  setlength(result, length(data));
  For i := 0 To high(data) Do
    result[i] := data[i];
End;

Function RuleSelectorToString(value: TRuleSelector): String;
Begin
  result := '?';
  Case value Of
    rsDependsOn: result := 'depends on';
    rsDistanceDependsOn: result := 'depends distance on';
    rsDistanceLeftOf: result := 'left distance of';
    rsDistanceRightOf: result := 'right distance of';
    rsEliminate: Result := 'eliminate';
    rsDistanceEliminate: Result := 'eliminate distance on';
    rsLeftOf: result := 'left of';
    rsRightOf: result := 'right of';
    rsMultiEliminate: result := 'eliminates';
    rsIntDepends: result := 'int depends';
  End;
End;

Function StringToRuleSelector(Value: String): TRuleSelector;
Begin
  result := rsUnknown;
  Case lowercase(trim(value)) Of
    'depends on': Result := rsDependsOn;
    'depends distance on': result := rsDistanceDependsOn;
    'left distance of': Result := rsDistanceLeftOf;
    'right distance of': result := rsDistanceRightOf;
    'eliminate': Result := rsEliminate;
    'eliminate distance on': result := rsDistanceEliminate;
    'eliminates': result := rsMultiEliminate;
    'left of': Result := rsLeftOf;
    'right of': Result := rsRightOf;
    'int depends': result := rsIntDepends;
  End;
End;

Procedure ResetData;
Var
  i, j, k: Integer;
Begin
  For i := 0 To EinsteinWidth - 1 Do
    For j := 0 To EinsteinHeight - 1 Do Begin
      data[i, j].Value := Nichts;
      For k := 0 To EinsteinWidth - 1 Do Begin
        data[i, j].Pencils[k] := true;
      End;
    End;
End;

Function Solve(Const Rules: Array Of TRule; Logevent: TLogEvent; AddHints,
  IsGuessing: Boolean): Boolean;
Var
  bool: Boolean;
  i: Integer;
Begin
  // Festes eintragen der 1. Menge
  For i := 0 To EinsteinWidth - 1 Do Begin
    SetValue(i, 0, i);
  End;
  // So Lange durch die Regeln gehen, bis keine mehr etwas am Feld ändern kann..
  bool := true;
  While bool Do Begin
    bool := false;
    // So lange wie eine Regel noch irgendwas ändern könnte machen wir weiter
    For i := 0 To high(Rules) Do Begin
      If Rules[i].ApplyRule Then Begin
        If assigned(Logevent) Then Begin
          If AddHints Then Begin
            Logevent(rules[i].Print + ' ' + rules[i].Hint);
          End
          Else Begin
            Logevent(rules[i].Print);
          End;
        End;
        bool := true;
      End;
    End;
    If IsSolved() Then Begin // Sollte eigentlich unnötig sein, da beim Gelösten die Regeln nichts mehr weiter anfügen
      bool := false;
    End
    Else Begin
      // Prüfen ob alle Regeln überhaupt noch Erfüllbar sind, wenn nein Raus mit Fehler
      For i := 0 To high(rules) Do Begin
        If Not Rules[i].Plausible Then Begin
          If IsGuessing Then Begin // Wenn wir eh gerade am Raten sind, brauchen wir nichts ausgeben und steigen einfach nur aus.
            result := false;
            exit;
          End
          Else Begin
            ShowMessage('Rule :' + Rules[i].Print + LineEnding + LineEnding + 'is impossible to fit.' + LineEnding + LineEnding + 'Abort calculation now.');
            bool := false;
            break;
          End;
        End;
      End;
    End;
  End;
  result := IsSolved();
  If result Then Begin
    // Prüfen ob alle Regeln überhaupt noch Erfüllbar sind, wenn nein Raus mit Fehler
    For i := 0 To high(rules) Do Begin
      If Not Rules[i].Plausible Then Begin
        If IsGuessing Then Begin // Wenn wir eh gerade am Raten sind, brauchen wir nichts ausgeben und steigen einfach nur aus.
          result := false;
          exit;
        End
        Else Begin
          ShowMessage('Rule :' + Rules[i].Print + LineEnding + LineEnding + 'is impossible to fit.' + LineEnding + LineEnding + 'Abort calculation now.');
          bool := false;
          break;
        End;
      End;
    End;
  End;
End;

Function IsSolved: Boolean;
Var
  i, j: Integer;
Begin
  result := true;
  For i := 0 To EinsteinWidth - 1 Do
    For j := 0 To EinsteinHeight - 1 Do Begin
      If data[i, j].Value = Nichts Then Begin
        result := false;
        exit;
      End;
    End;
End;

Function ClearPencil(x, y, p: integer): boolean; // Löscht einen Pencilwert, wenn das "erlaubt" ist, sonst false
Begin
  result := false;
  If (data[x, y].value = nichts) // Ist das Feld noch nicht gesetzt
  And (data[x, y].pencils[p]) Then Begin // Ist der pencils noch nicht ausgestrichen
    data[x, y].pencils[p] := false;
    result := true;
  End;
End;

Function SetValue(x, y: integer; Value: integer; RaiseException: Boolean): Boolean;
Var
  j: integer;
Begin
  result := false;
  If (data[x, y].Value <> Nichts) And (data[x, y].Value <> value) Then Begin
    If RaiseException Then Begin
      Raise exception.create(format('Fehler, die Logik wollte eine bereits existierende Zahl[%d] mit einer anderen[%d] überschreiben.', [data[x, y].Value, value]));
    End;
    exit;
  End;
  If data[x, y].value <> value Then Begin // Nur wenn der Wert noch nicht gesetzt wurde, wird er gesetzt, sonst false
    result := true;
    Data[x, y].Value := value;
    // Löschen der Pencils für das entsprechende Feld
    For j := 0 To EinsteinWidth - 1 Do Begin
      // Löschen aller anderen Pencils in dem Feld, da es ja mit Value beschrieben ist
      If j <> Value Then Begin
        Data[x, y].Pencils[j] := false;
      End;
      // Austragen der Pencils für value in der Waagrechten Zeile, da es ja nun in x,y gesetzt ist
      If j = x Then Begin
        If Not Data[j, y].Pencils[value] Then Begin
          result := false;
          If RaiseException Then Begin
            Raise exception.Create('Fehler, die Logik wollte eine Zahl schreiben, obwohl die Pencils sagen dass diese Zahl hier nicht stehen darf.');
          End;
        End;
      End
      Else Begin
        Data[j, y].Pencils[value] := false;
      End;
    End;
  End;
End;

{ TIntDependence }

Constructor TIntDependence.Create(Element1, Cathegory, Element2,
  difference: integer);
Begin
  Inherited create;
  fElement1Set := Element1 Div EinsteinWidth;
  fElement1Element := Element1 Mod EinsteinWidth;
  fElement2Set := Element2 Div EinsteinWidth;
  fElement2Element := Element2 Mod EinsteinWidth;
  fCathegory := Cathegory;
  fDifference := Difference;
End;

Function TIntDependence.Print: String;
Var
  s: String;
Begin
  Result := Inherited Print + ' ';
  s := ResolveXY(fElement1Set, fElement1Element) + ': ';
  s := s + ResolveXY(fCathegory, -2) + ' ';
  If fDifference >= 0 Then s := s + '+';
  s := s + inttostr(fDifference) + ' = ';
  s := s + ResolveXY(fElement2Set, fElement2Element) + ': ';
  s := s + ResolveXY(fCathegory, -2);
  result := result + s;
End;

Function TIntDependence.ResolveDifferingElement(SetIndex, Element,
  difference: integer): integer;
  Function Eval(Value: String): integer;
  Begin
    value := trim(value);
    If pos(' ', Value) <> 0 Then Begin
      value := copy(value, 1, pos(' ', Value) - 1);
    End;
    result := strtointdef(value, Nichts);
  End;
Var
  el, i: integer;
Begin
  result := Nichts;
  el := Eval(ResolveXY(Setindex, Element));
  // In der Menge schauen, welchen Index das gesuchte Element hat
  For i := 0 To EinsteinHeight - 1 Do Begin
    If Eval(ResolveXY(Setindex, i)) = El + difference Then Begin
      result := i;
      exit;
    End;
  End;
End;

Function TIntDependence.ApplyRule: Boolean;
Var
  n, i, j: integer;
Begin
  result := false;
  // Berechnen der indizees für die Elemente
  For i := 0 To EinsteinWidth - 1 Do Begin
    // Element 1 ist gesetzt und kann gefunden werden
    If data[i][fElement1Set].Value = fElement1Element Then Begin
      // Ist die zu betrachtende Kathegorie auch gesetzt ?
      If data[i][fCathegory].Value <> nichts Then Begin
        // Nun da alle Vorbedingungen Gegeben sind, suchen wir
        n := ResolveDifferingElement(fCathegory, data[i][fCathegory].Value, fDifference);
        If n = -1 Then Begin
          // Todo : Werfen wir hier auch eine Exception ?
          exit;
        End;
        // A) die Differenzbedingung und setzen den fehlenden
        For j := 0 To EinsteinWidth - 1 Do Begin
          If data[j][fCathegory].Value = n Then Begin
            result := SetValue(j, fElement2Set, fElement2Element, false); // Eigentlich müsste man Prüfen ob die Regel setzen Darf, der Plausibilitätscheck fängt das nachher wieder ab ;)
            exit;
          End;
        End;
        // B) das Andere Element und setzen den fehlenden
        For j := 0 To EinsteinWidth - 1 Do Begin
          If data[j][fElement2Set].Value = fElement2Element Then Begin
            result := SetValue(j, fCathegory, n, false); // Eigentlich müsste man Prüfen ob die Regel setzen Darf, der Plausibilitätscheck fängt das nachher wieder ab ;)
            exit;
          End;
        End;
      End;
    End;
    // Element 2 ist gesetzt und kann gefunden werden
    If data[i][fElement2Set].Value = fElement2Element Then Begin
      // Ist die zu betrachtende Kathegorie auch gesetzt ?
      If data[i][fCathegory].Value <> nichts Then Begin
        // Nun da alle Vorbedingungen Gegeben sind, suchen wir
        n := ResolveDifferingElement(fCathegory, data[i][fCathegory].Value, fDifference);
        If n = -1 Then Begin
          // Todo : Werfen wir hier auch eine Exception ?
          exit;
        End;
        // A) die Differenzbedingung und setzen den fehlenden
        For j := 0 To EinsteinWidth - 1 Do Begin
          If data[j][fCathegory].Value = n Then Begin
            result := SetValue(j, fElement1Set, fElement1Element, false); // Eigentlich müsste man Prüfen ob die Regel setzen Darf, der Plausibilitätscheck fängt das nachher wieder ab ;)
            exit;
          End;
        End;
        // B) das Andere Element und setzen den fehlenden
        For j := 0 To EinsteinWidth - 1 Do Begin
          If data[j][fElement1Set].Value = fElement1Element Then Begin
            result := SetValue(j, fCathegory, n, false); // Eigentlich müsste man Prüfen ob die Regel setzen Darf, der Plausibilitätscheck fängt das nachher wieder ab ;)
            exit;
          End;
        End;
      End;
    End;
  End;
End;

Function TIntDependence.Plausible: boolean;
Var
  n, i, j: integer;
Begin
  result := true;
  // Wir werden erst Nicht mehr Plausibel, wenn wir alle 4 Betroffenen
  // Stellen finden und diese nicht mehr stimmen.
  For i := 0 To EinsteinWidth - 1 Do Begin
    // Element 1 ist gesetzt und kann gefunden werden
    If data[i][fElement1Set].Value = fElement1Element Then Begin
      // Ist die zu betrachtende Kathegorie auch gesetzt ?
      If data[i][fCathegory].Value <> nichts Then Begin
        // Nun da alle Vorbedingungen Gegeben sind, suchen wir
        n := ResolveDifferingElement(fCathegory, data[i][fCathegory].Value, fDifference);
        If n = -1 Then Begin
          // Todo : Werfen wir hier auch eine Exception ?
          result := false; // Auf jeden Fall kann die Regel nicht mehr erfüllt werden
          exit;
        End;
        // A) die Differenzbedingung und setzen den fehlenden
        For j := 0 To EinsteinWidth - 1 Do Begin
          If data[j][fCathegory].Value = n Then Begin
            // Stimmen der zu erwartende Elementname mit dem tatsächlichen überein ?
            If (data[j][fElement2Set].Value <> nichts) And // Eigentlich könnte man sich das hier Sparen, da der ApplyRule ja den wert setzt !
            (data[j][fElement2Set].Value <> fElement2Element) Then Begin
              result := false;
            End;
          End;
        End;
        // B) das Andere Element und setzen den fehlenden
        For j := 0 To EinsteinWidth - 1 Do Begin
          If data[j][fElement2Set].Value = fElement2Element Then Begin
            If (data[j][fCathegory].Value <> nichts) And // Eigentlich könnte man sich das hier Sparen, da der ApplyRule ja den wert setzt !
            (data[j][fCathegory].Value <> n) Then Begin
              result := false;
            End;
          End;
        End;
      End;
    End;
  End;
End;

{ TEinsteinRuleFrame }

Procedure TEinsteinRuleFrame.MarkAsChanged(Sender: TObject);
Begin
  fDataChanged := true;
End;

Constructor TEinsteinRuleFrame.Create(TheOwner: TComponent);
Begin
  Inherited Create(TheOwner);
  fDataChanged := false;
End;

Procedure TEinsteinRuleFrame.SaveToFile(Const Ini: TInifile; Index: integer);
Begin
  fDataChanged := false;
End;

Procedure TEinsteinRuleFrame.LoadFromFile(Const Ini: TInifile; Index: integer);
Begin
  fDataChanged := false;
End;

Function TEinsteinRuleFrame.DataChanged: Boolean;
Begin
  result := fDataChanged;
End;

{ TRule }

Constructor TRule.Create;
Begin
  Inherited create;
  Number := -1; // unbekannte Nummer
  Hint := '';
End;

Function TRule.NakedSingle: boolean;
Var
  x, y, i: integer;
  c: Array Of Integer;
  ci, cnt: integer;
Begin
  setlength(c, EinsteinWidth);
  result := false;
  // Prüfen ob es in einer Zeile einen Eintrag genau 1 mal gibt.
  For y := 0 To EinsteinHeight - 1 Do Begin
    For i := 0 To high(c) Do Begin
      c[i] := 0;
    End;
    // Aufsummieren alle Häufigkeiten aller Pencils in der Reihe y
    For x := 0 To EinsteinWidth - 1 Do Begin
      For i := 0 To EinsteinWidth - 1 Do Begin
        If Data[x, y].Pencils[i] Then inc(c[i]);
      End;
    End;
    // Prüfen ob es einen Eintrag genau 1 mal gibt
    For i := 0 To EinsteinWidth - 1 Do Begin
      If c[i] = 1 Then Begin
        // Suchen und Setzen dieser einen Zahl
        For x := 0 To EinsteinWidth - 1 Do Begin
          If (Data[x, y].Pencils[i]) And (Data[x, y].Value = Nichts) Then Begin
            result := SetValue(x, y, i) Or result;
            break;
          End;
        End;
      End;
      // Die Logik hat alle Möglichen Kandidaten gekillt, kein Lösen mehr möglich.
      // Todo : Das muss nach Setvalue, muss sich mit Guessing vertragen
      //If c[i] = 0 Then Begin
      //  Raise Exception.Create('Error all pencils canceled out, no solution possible.');
      //End;
    End;
  End;
  // Prüfen ob es nur noch einen Möglichen Eintrag in irgend einer Zelle gibt.
  For x := 0 To EinsteinWidth - 1 Do Begin
    For y := 0 To EinsteinHeight - 1 Do Begin
      cnt := 0;
      ci := -1;
      For i := 0 To EinsteinWidth - 1 Do Begin
        If Data[x, y].Pencils[i] Then Begin
          inc(cnt);
          ci := i;
        End;
      End;
      If cnt = 1 Then Begin // in dieser zelle ist nur noch eine Möglichkeit vorhanden, also setzen
        result := SetValue(x, y, ci) Or result;
      End;
    End;
  End;
  If result Then Begin
    //result :=
    NakedSingle();
  End;
  setlength(c, 0);
End;

Function TRule.Print: String;
Begin
  result := inttostr(Number) + ' :';
End;

{ TMultiRule }

Constructor TMultiRule.Create;
Begin
  Inherited;
  Setlength(fSets1, EinsteinHeight);
  Setlength(fSets2, EinsteinHeight);
End;

Function TMultiRule.Print: String;
Var
  i, j: integer;
  s, t: String;
Begin
  result := Inherited;
  s := '';
  For i := 0 To EinsteinHeight - 1 Do Begin
    t := '';
    For j := 0 To EinsteinWidth - 1 Do Begin
      If fSets1[i, j] <> nichts Then Begin
        If t <> '' Then t := t + ', ';
        t := t + ResolveXY(i, fSets1[i, j]);
      End;
    End;
    If t <> '' Then Begin
      If s <> '' Then s := s + ', ';
      If pos(',', t) <> 0 Then Begin
        s := s + '(' + t + ')';
      End
      Else Begin
        s := s + t;
      End;
    End;
  End;
  //  result := result + ' (' + s + ') ' + fPrintSeperator;
  result := result + ' ' + s + ' ' + fPrintSeperator + ' ';
  s := '';
  For i := 0 To EinsteinHeight - 1 Do Begin
    t := '';
    For j := 0 To EinsteinWidth - 1 Do Begin
      If fSets2[i, j] <> nichts Then Begin
        If t <> '' Then t := t + ', ';
        t := t + ResolveXY(i, fSets2[i, j]);
      End;
    End;
    If t <> '' Then Begin
      If s <> '' Then s := s + ', ';
      If pos(',', t) <> 0 Then Begin
        s := s + '(' + t + ')';
      End
      Else Begin
        s := s + t;
      End;
    End;
  End;
  //  result := result + ' (' + s + ')';
  result := result + ' ' + s;
End;

Procedure TMultiRule.SetSet(SetTwo: Boolean; Index: integer; Data: TMemberSet);
Begin
  If (index >= 0) And (index < EinsteinHeight) Then Begin
    If SetTwo Then Begin
      fSets2[index] := Data;
    End
    Else Begin
      fSets1[index] := Data;
    End;
  End;
End;

{ TMultiEliminate }

Constructor TMultiEliminate.Create;
Begin
  Inherited Create;
  fPrintSeperator := RuleSelectorToString(rsMultiEliminate);
End;

Function TMultiEliminate.ApplyRule: Boolean;
Var
  r1, r2: TMemberSetArray;
  i, x, y, xx: Integer;
  bool: Boolean;
Begin
  result := false;
  r1 := fSets1;
  r2 := fSets2;
  For i := 0 To 1 Do Begin
    For x := 0 To EinsteinWidth - 1 Do Begin
      bool := false;
      // Nach einer Spalte suchen in der R1 vorkommen kann
      For y := 0 To EinsteinHeight - 1 Do Begin
        For xx := 0 To EinsteinWidth - 1 Do Begin
          If (r1[y, xx] <> Nichts) And (data[x, y].value = r1[y, xx]) Then Begin
            bool := true;
            break;
          End;
        End;
        If bool Then break;
      End;
      If bool Then Begin
        // R1 kann hier Plaziert werden, also schmeisen wir alles Raus was in X mit R2 zu tun hat.
        For y := 0 To EinsteinHeight - 1 Do Begin
          For xx := 0 To EinsteinWidth - 1 Do Begin
            If (r2[y, xx] <> Nichts) Then Begin
              result := ClearPencil(x, y, r2[y, xx]) Or result;
            End;
          End;
        End;
      End;
    End;
    r1 := fSets2;
    r2 := fSets1;
  End;
  If result Then Begin // Es hat sich was geändert, also neu auswerten
    NakedSingle;
  End;
End;

Function TMultiEliminate.Plausible: boolean;
Var
  r1, r2: TMemberSetArray;
  i, x, y, xx: Integer;
  bool: Boolean;
Begin
  result := true;
  r1 := fSets1;
  r2 := fSets2;
  For i := 0 To 1 Do Begin
    For x := 0 To EinsteinWidth - 1 Do Begin
      bool := false;
      // Nach einer Spalte suchen in der R1 vorkommen kann
      For y := 0 To EinsteinHeight - 1 Do Begin
        For xx := 0 To EinsteinWidth - 1 Do Begin
          If (r1[y, xx] <> Nichts) And (data[x, y].value = r1[y, xx]) Then Begin
            bool := true;
            break;
          End;
        End;
        If bool Then break;
      End;
      If bool Then Begin
        // R1 kann hier Plaziert werden, also dürfen die anderen Regeln nicht Wahr werden
        For y := 0 To EinsteinHeight - 1 Do Begin
          For xx := 0 To EinsteinWidth - 1 Do Begin
            If (r2[y, xx] <> Nichts) And (data[x, y].value = r2[y, xx]) Then Begin // Die Regel ist definiert, aber Value steht genau auf einem Verbotenen
              result := false;
              exit;
            End;
          End;
        End;
      End;
    End;
    r1 := fSets2;
    r2 := fSets1;
  End;
End;

{ TSetRule }

Constructor TSetRule.Create(Rule: TMemberSet);
Begin
  Inherited create;
  fRule := Rule;
End;

{ TTwoSetRule }

Constructor TTwoSetRule.Create(Rule1, Rule2: TMemberSet);
Begin
  Inherited create;
  fRule1 := Rule1;
  fRule2 := Rule2;
End;

Function TTwoSetRule.Print: String;
Var
  i: integer;
  s: String;
Begin
  Result := Inherited Print + ' ';
  s := '';
  For i := 0 To EinsteinHeight - 1 Do Begin
    If fRule1[i] <> Nichts Then Begin
      If s <> '' Then s := s + ', ';
      s := s + ResolveXY(i, fRule1[i]);
    End;
  End;
  result := result + s + ' ' + fPrintSeperator + ' ';
  s := '';
  For i := 0 To EinsteinHeight - 1 Do Begin
    If fRule2[i] <> Nichts Then Begin
      If s <> '' Then s := s + ', ';
      s := s + ResolveXY(i, fRule2[i]);
    End;
  End;
  result := result + s;
End;

{ TTwoSetDistanceRule }

Procedure TTwoSetDistanceRule.SetDistance(Const Value: Integer);
Begin
  fDistance := value;
  fPrintSeperator := fPrintSeperator + ' ' + inttostr(value);
End;

{ TDependsOn }

Function TDependsOn.ApplyRule: Boolean;
Var
  x, y, yy: integer;
Begin
  Result := false;
  // Wenn ein y-Eintrag bereits mit was "Falschem" Belegt ist, dann schließt das automatisch alle y-Einträge auf allen anderen Zeilen aus
  For x := 0 To EinsteinWidth - 1 Do Begin
    For y := 0 To EinsteinHeight - 1 Do Begin
      If (fRule[y] <> Nichts) And (data[x, y].Value <> Nichts) And (data[x, y].Value <> fRule[y]) Then Begin // Ein Feststehender Wert passt nicht.
        For yy := 0 To EinsteinHeight - 1 Do Begin // Austragen aller Pencil informationen zu dieser Regel
          If (fRule[yy] <> Nichts) Then Begin
            result := ClearPencil(x, yy, frule[yy]) Or result;
          End;
        End;
      End;
    End;
  End;
  // Wenn ein y-Eintrag der für die Regel notwendig ist, schon gar nicht mehr möglich ist, dann schließt das die Regel ebenfalls aus
  For x := 0 To EinsteinWidth - 1 Do Begin
    For y := 0 To EinsteinHeight - 1 Do Begin
      If (fRule[y] <> Nichts) And (Not (data[x, y].Pencils[fRule[y]])) Then Begin
        For yy := 0 To EinsteinHeight - 1 Do Begin // Austragen aller Pencil informationen zu dieser Regel
          If (fRule[yy] <> Nichts) Then Begin
            result := ClearPencil(x, yy, frule[yy]) Or result;
          End;
        End;
      End;
    End;
  End;
  If result Then Begin
    NakedSingle;
  End;
End;

Function TDependsOn.Print: String;
Var
  i: integer;
  s: String;
Begin
  Result := Inherited Print + ' ';
  s := '';
  For i := 0 To EinsteinHeight - 1 Do Begin
    If fRule[i] <> Nichts Then Begin
      If s <> '' Then s := s + ' ' + RuleSelectorToString(rsDependsOn) + ' ';
      s := s + ResolveXY(i, fRule[i]);
    End;
  End;
  result := result + s;
End;

Function TDependsOn.Plausible: boolean;
Var
  x, y: integer;
  Bool: Boolean;
Begin
  result := false;
  For x := 0 To EinsteinWidth - 1 Do Begin
    (*
     * Es wird erst mal davon ausgegangen, dass die Regel hier passt und nach ausschließenden Elementen gesucht.
     * Da data[x,y].value aber immer über SetValue gesetzt wird und dieses Sicherstellt, dass die Pencils entsprechend stimmen, reicht ein
     * Vergleich auf die Pencils.
     *)
    bool := True;
    For y := 0 To EinsteinHeight - 1 Do Begin
      If (fRule[y] <> Nichts) And (Not Data[x, y].pencils[fRule[y]]) Then Begin // Die Regel kann hier definitiv nicht mehr stehen
        bool := false;
        break;
      End;
    End;
    // An Spalte X ist die Depends On Regel theoretisch noch machbar, also sind wir zufrieden.
    If bool Then Begin
      result := true;
      exit;
    End;
  End;
End;

{ TDistanceDependsOn }

Constructor TDistanceDependsOn.Create(Rule1, Rule2: TMemberSet);
Begin
  Inherited Create(Rule1, Rule2);
  fPrintSeperator := RuleSelectorToString(rsDistanceDependsOn);
End;

Function TDistanceDependsOn.ApplyRule: Boolean;
Var
  i: integer;
  Bool: Boolean;
  R1, R2: TMemberSet;
  x, xx, y: integer;
Begin
  If Number = 13 Then Begin
    nop;
  End;
  Result := false;
  r1 := fRule1;
  r2 := fRule2;
  For i := 0 To 1 Do Begin // Das ganze muss 2 mal gemacht werden einmal suchen wir nach der Position von r1 und matchen dann R2, und einmal andersrum
    For x := 0 To EinsteinWidth - 1 Do Begin
      bool := true;
      For y := 0 To EinsteinHeight - 1 Do Begin
        // Suchen einer Inkompatiblen Stelle
        If (r1[y] <> Nichts) And (Not (data[x, y].Pencils[r1[y]])) Then Begin
          bool := false;
          break;
        End;
      End;
      If Not bool Then Begin // Die Regel kann hier definitiv nicht umgesetzt werden => Hilft uns aber nichts

      End;
      If bool Then Begin // es gibt keine Konflickte, damit der untere Code aber arbeiten darf, muss mindestens ein Kriterium auch erfüllt sein
        bool := false;
        For y := 0 To EinsteinHeight - 1 Do Begin
          If (R1[y] <> Nichts) And (data[x, y].Value = R1[y]) Then Begin
            bool := true;
            break;
          End;
        End;
      End;
      If bool Then Begin // R1 passt in die Spalte x rein => Alles was R2 Definiert kann nun aus den Spalten x-Distance und x+Distance gestrichen werden
        For xx := 0 To EinsteinWidth - 1 Do Begin
          If abs(x - xx) <> Distance Then Begin
            For y := 0 To EinsteinHeight - 1 Do Begin
              If R2[y] <> Nichts Then Begin
                result := ClearPencil(xx, y, r2[y]) Or result;
              End;
            End;
          End;
        End;
      End;
    End;
    r1 := fRule2;
    r2 := fRule1;
  End;
  If result Then Begin // Es hat sich was geändert, also neu auswerten
    NakedSingle;
  End;
End;

Function TDistanceDependsOn.Plausible: boolean;
Var
  x, y, c, i: integer;
  Bool: Boolean;
  r1, r2: TMemberSet;
Begin
  result := true;
  r1 := fRule1;
  r2 := fRule2;
  For i := 0 To 1 Do Begin
    For x := 0 To EinsteinWidth - 1 Do Begin
      // 1. Schauen ob R1 irgendwo schon fixiert werden kann
      bool := false;
      For y := 0 To EinsteinHeight - 1 Do Begin
        If (r1[y] <> Nichts) And (data[x, y].value = r1[y]) Then Begin
          bool := true;
          break;
        End;
      End;
      // 2. Prüfen ob an mindestens einem der beiden möglichen Positionen die Regel noch gültig sein könnte
      If bool Then Begin
        c := 0;
        If x - Distance >= 0 Then Begin
          bool := true;
          For y := 0 To EinsteinHeight - 1 Do Begin
            // Die Regel ist Definiert, der Pencil sagt aber, dass ihr Wert hier nicht mehr erfüllt werden kann
            If (r2[y] <> Nichts) And (Not data[x - Distance, y].pencils[r2[y]]) Then Begin
              bool := false;
              break;
            End;
          End;
          If bool Then inc(c);
        End;
        If x + Distance < EinsteinWidth Then Begin
          bool := true;
          For y := 0 To EinsteinHeight - 1 Do Begin
            // Die Regel ist Definiert, der Pencil sagt aber, dass ihr Wert hier nicht mehr erfüllt werden kann
            If (r2[y] <> Nichts) And (Not data[x + Distance, y].pencils[r2[y]]) Then Begin
              bool := false;
              break;
            End;
          End;
          If bool Then inc(c);
        End;
        If c = 0 Then Begin // Es gibt keine Möglichkeit mehr R2 zu erfüllen, wenn R1 an x fest steht.
          result := false;
          exit;
        End;
      End;
    End;
    r1 := fRule2;
    r2 := fRule1;
  End;
End;

{ TLeftOf }

Constructor TLeftOf.Create(Rule1, Rule2: TMemberSet);
Begin
  Inherited Create(Rule1, Rule2);
  fPrintSeperator := RuleSelectorToString(rsLeftOf);
End;

Function TLeftOf.ApplyRule: Boolean;
Var
  y, x, xx: Integer;
  bool: Boolean;
Begin
  result := false;
  // 1. Wenn etwas Links von etwas anderem Stehen soll, kann das andere auf keinen Fall in der 1. Spalte stehen
  For y := 0 To EinsteinHeight - 1 Do Begin
    If (fRule2[y] <> Nichts) Then Begin
      result := ClearPencil(0, y, frule2[y]) Or result;
    End;
  End;
  // 2. Wenn etwas Links von etwas anderem Steht, kann dieses Etwas auf keinen Fall ganz Rechts stehen
  For y := 0 To EinsteinHeight - 1 Do Begin
    If (fRule1[y] <> Nichts) Then Begin
      result := ClearPencil(EinsteinWidth - 1, y, fRule1[y]) Or result;
    End;
  End;
  // 3. Wenn Das Linke fest steht, können "Links" davon alle Rechten weggestrichen werden.
  For x := 0 To EinsteinWidth - 1 Do Begin
    bool := true;
    For y := 0 To EinsteinHeight - 1 Do Begin
      // Suchen einer Inkompatiblen Stelle
      If (fRule1[y] <> Nichts) And (Not (data[x, y].Pencils[fRule1[y]])) Then Begin
        bool := false;
        break;
      End;
    End;
    //If Not bool Then Begin -- fRule1 passt nicht in x
    //End;
    If bool Then Begin // es gibt keine Konflickte, damit der untere Code aber arbeiten darf, muss mindestens ein Kriterium auch erfüllt sein
      bool := false;
      For y := 0 To EinsteinHeight - 1 Do Begin
        If (fRule1[y] <> Nichts) And (data[x, y].Value = fRule1[y]) Then Begin
          bool := true;
          break;
        End;
      End;
    End;
    If bool Then Begin
      // Wenn fRule1 in x passt, dann muss dies auch vollständig gehen, sonst wäre alles Falsch, also Sicherstellen, dass der Rest auch drin steht
      For y := 0 To EinsteinHeight - 1 Do Begin
        If (fRule1[y] <> Nichts) And (data[x, y].value <> fRule1[y]) Then Begin // Diese zeile Provoziert einen Fehler, wenn data[x, y].value <> fRule1[y] und data[x, y].value <> nichts
          result := SetValue(x, y, fRule1[y]) Or result; // wir ändern ja was, also auf jeden Raus geben, dass wir was gemacht haben
        End;
      End;
      // Links von x kann überall fRule2 gestrichen werden
      For xx := x - 1 Downto 0 Do Begin
        For y := 0 To EinsteinHeight - 1 Do Begin
          If (fRule2[y] <> Nichts) Then Begin
            result := ClearPencil(xx, y, fRule2[y]) Or result;
          End;
        End;
      End;
    End;
  End;
  // 4. Wenn Das Rechte fest steht, können "rechts" davon alle Linken weggestrichen werden.
  For x := 0 To EinsteinWidth - 1 Do Begin
    bool := true;
    For y := 0 To EinsteinHeight - 1 Do Begin
      // Suchen einer Inkompatiblen Stelle
      If (fRule2[y] <> Nichts) And (Not (data[x, y].Pencils[fRule2[y]])) Then Begin
        bool := false;
        break;
      End;
    End;
    //If Not bool Then Begin -- fRule2 passt nicht in x
    //End;
    If bool Then Begin // es gibt keine Konflickte, damit der untere Code aber arbeiten darf, muss mindestens ein Kriterium auch erfüllt sein
      bool := false;
      For y := 0 To EinsteinHeight - 1 Do Begin
        If (fRule2[y] <> Nichts) And (data[x, y].Value = fRule2[y]) Then Begin
          bool := true;
          break;
        End;
      End;
    End;
    If bool Then Begin
      // Wenn fRule2 in x passt, dann muss dies auch vollständig gehen, sonst wäre alles Falsch, also Sicherstellen, dass der Rest auch drin steht
      For y := 0 To EinsteinHeight - 1 Do Begin
        If (fRule2[y] <> Nichts) And (data[x, y].value <> fRule2[y]) Then Begin // Diese zeile Provoziert einen Fehler, wenn data[x, y].value <> fRule2[y] und data[x, y].value <> nichts
          result := SetValue(x, y, fRule2[y]) Or result; // wir ändern ja was, also auf jeden Raus geben, dass wir was gemacht haben
        End;
      End;
      // Links von x kann überall fRule1 gestrichen werden
      For xx := x + 1 To EinsteinWidth - 1 Do Begin
        For y := 0 To EinsteinHeight - 1 Do Begin
          If (fRule1[y] <> Nichts) Then Begin
            result := ClearPencil(xx, y, fRule1[y]) Or result;
          End;
        End;
      End;
    End;
  End;
  // 5. Von Links kommend, kann Rule2 so lange gestrichen werden, wie Rule1 auch nicht geht
  For x := 0 To EinsteinWidth - 1 Do Begin
    bool := true;
    For y := 0 To EinsteinHeight - 1 Do Begin
      If (fRule1[y] <> Nichts) And Not data[x, y].pencils[frule1[y]] Then Begin
        bool := false;
        break;
      End;
    End;
    If bool Then Begin // Die Regel1 geht zum 1. Mal
      For xx := 0 To x Do Begin
        For y := 0 To EinsteinHeight - 1 Do Begin
          If (fRule2[y] <> Nichts) Then Begin
            result := ClearPencil(xx, y, fRule2[y]) Or result;
          End;
        End;
      End;
      break;
    End;
  End;
  // 6. Von Rechts kommend, kann Rule1 so lange gestrichen werden, wie Rule2 auch nicht geht
  For x := EinsteinWidth - 1 Downto 0 Do Begin
    bool := true;
    For y := 0 To EinsteinHeight - 1 Do Begin
      If (fRule2[y] <> Nichts) And Not data[x, y].pencils[frule2[y]] Then Begin
        bool := false;
        break;
      End;
    End;
    If bool Then Begin // Die Regel2 geht zum 1. Mal
      For xx := EinsteinWidth - 1 Downto x Do Begin
        For y := 0 To EinsteinHeight - 1 Do Begin
          If (fRule1[y] <> Nichts) And data[xx, y].pencils[frule1[y]] Then Begin
            result := ClearPencil(xx, y, fRule1[y]) Or result;
          End;
        End;
      End;
      break;
    End;
  End;

  If Result Then Begin
    NakedSingle;
  End;
End;

Function TLeftOf.Plausible: boolean;
Var
  x, y, xx: Integer;
  bool: Boolean;
Begin
  result := true;
  // 1. Suchen nach dem Linken, wenn es dies gibt, dann darf Links davon das rechte natürlich nicht stehen
  For x := 0 To EinsteinWidth - 1 Do Begin
    bool := false;
    For y := 0 To EinsteinHeight - 1 Do Begin
      // Suchen einer Inkompatiblen Stelle
      If (fRule1[y] <> Nichts) And (data[x, y].value = fRule1[y]) Then Begin
        bool := true;
        break;
      End;
    End;
    If bool Then Begin // frule1 ist sicher in x Plazierbar, also darf frule2 nicht links davon stehen
      For xx := 0 To x Do Begin
        For y := 0 To EinsteinHeight - 1 Do Begin
          If (fRule2[y] <> Nichts) And (data[xx, y].value = fRule2[y]) Then Begin
            result := false;
            exit;
          End;
        End;
      End;
    End;
  End;
  // 1. Suchen nach dem Rechten, wenn es dies gibt, dann darf Rechts davon das Linke natürlich nicht stehen
  For x := 0 To EinsteinWidth - 1 Do Begin
    bool := false;
    For y := 0 To EinsteinHeight - 1 Do Begin
      // Suchen einer Inkompatiblen Stelle
      If (fRule2[y] <> Nichts) And (data[x, y].value = fRule2[y]) Then Begin
        bool := true;
        break;
      End;
    End;
    If bool Then Begin // frule2 ist sicher in x Plazierbar, also darf frule1 nicht rechts davon stehen
      For xx := x To EinsteinWidth - 1 Do Begin
        For y := 0 To EinsteinHeight - 1 Do Begin
          If (fRule1[y] <> Nichts) And (data[xx, y].value = fRule1[y]) Then Begin
            result := false;
            exit;
          End;
        End;
      End;
    End;
  End;
End;

{ TLeftDistanceDependsOf }

Constructor TLeftDistanceDependsOf.Create(Rule1, Rule2: TMemberSet);
Begin
  Inherited Create(Rule1, Rule2);
  fPrintSeperator := RuleSelectorToString(rsDistanceLeftOf);
End;

Function TLeftDistanceDependsOf.ApplyRule: Boolean;
Var
  x, y: integer;
  bool: Boolean;
Begin
  result := false;
  // Wenn etwas Links mit Abstand stehen soll, dann kann es Unmöglich in den Distance-1 Spalten Rechts stehen
  For x := EinsteinWidth - Distance To EinsteinWidth - 1 Do Begin
    For y := 0 To EinsteinHeight - 1 Do Begin
      If (fRule1[y] <> Nichts) Then Begin
        result := ClearPencil(x, y, fRule1[y]) Or result;
      End;
    End;
  End;
  // Wenn etwas Rechts mit Abstand stehen soll, dann kann es Unmöglich in den Distance-1 Spalten Links stehen
  For x := 0 To Distance - 1 Do Begin
    For y := 0 To EinsteinHeight - 1 Do Begin
      If (fRule2[y] <> Nichts) Then Begin
        result := ClearPencil(x, y, fRule2[y]) Or result;
      End;
    End;
  End;
  // Wenn das was Rechts neben dem Linken Steht nicht möglich ist, dann kann das Linke auch nicht möglich sein.
  For x := Distance To EinsteinWidth - 1 Do Begin
    bool := true;
    For y := 0 To EinsteinHeight - 1 Do Begin
      If (fRule2[y] <> Nichts) And (Not (data[x, y].Pencils[fRule2[y]])) Then Begin
        bool := false;
        break;
      End;
    End;
    If Not bool Then Begin // Regel 2 kann unmöglich in Spalte x liegen => in X-Distance kann alles aus Regel1 ausgeschlossen werden
      For y := 0 To EinsteinHeight - 1 Do Begin
        If (fRule1[y] <> Nichts) Then Begin
          result := ClearPencil(x - Distance, y, fRule1[y]) Or result;
        End;
      End;
    End;
  End;
  // Wenn das was Links neben dem Rechten steht nicht möglich ist, dann kann das Rechte auch nicht möglich sein.
  For x := 0 To EinsteinWidth - 1 - Distance Do Begin
    bool := true;
    For y := 0 To EinsteinHeight - 1 Do Begin
      If (fRule1[y] <> Nichts) And (Not (data[x, y].Pencils[fRule1[y]])) Then Begin
        bool := false;
        break;
      End;
    End;
    If Not bool Then Begin // Regel 1 kann unmöglich in Spalte x liegen => in X+Distance kann alles aus Regel2 ausgeschlossen werden
      For y := 0 To EinsteinHeight - 1 Do Begin
        If (fRule2[y] <> Nichts) Then Begin
          result := ClearPencil(x + Distance, y, fRule2[y]);
        End;
      End;
    End;
  End;
  // Prüfen ob eine der Beiden Regeln Existiert, dann muss entsprechend die andere Gesetzt werden. => So wie es aussieht braucht es das aber gar nicht ...
  If Result Then Begin
    NakedSingle;
  End;
End;

Function TLeftDistanceDependsOf.Plausible: boolean;
Var
  x, Y: Integer;
  bool: Boolean;
Begin
  result := true;
  For x := 0 To EinsteinWidth - 1 Do Begin
    // 1. Suchen ob das Linke feststeht
    bool := false;
    For Y := 0 To EinsteinHeight - 1 Do Begin
      If (fRule1[y] <> Nichts) And (data[x, y].value = fRule1[y]) Then Begin
        bool := true;
        break;
      End;
    End;
    // 1.1 Prüfen ob das Rechte dazu passt
    If bool Then Begin
      If x + Distance >= EinsteinWidth Then Begin // Die Linke Regel steht zu weit Rechts
        result := false;
        exit;
      End;
      For y := 0 To EinsteinHeight - 1 Do Begin
        // Der Pencil sollte gesetzt werden dürfen, kann aber nicht mehr ..
        If (frule2[y] <> Nichts) And (Not data[x + distance, y].pencils[frule2[y]]) Then Begin
          result := false;
          exit;
        End;
      End;
    End;
  End;
  For x := 0 To EinsteinWidth - 1 Do Begin
    // 2. Suchen ob das Rechte feststeht
    bool := false;
    For Y := 0 To EinsteinHeight - 1 Do Begin
      If (fRule2[y] <> Nichts) And (data[x, y].value = fRule2[y]) Then Begin
        bool := true;
        break;
      End;
    End;
    // 2.1 Prüfen ob das linke dazu passt
    If bool Then Begin
      If x - Distance < 0 Then Begin // Die Linke Regel steht zu weit Rechts
        result := false;
        exit;
      End;
      For y := 0 To EinsteinHeight - 1 Do Begin
        // Der Pencil sollte gesetzt werden dürfen, kann aber nicht mehr ..
        If (frule1[y] <> Nichts) And (Not data[x - distance, y].pencils[frule1[y]]) Then Begin
          result := false;
          exit;
        End;
      End;
    End;
  End;
End;

{ TRightOf }

Constructor TRightOf.Create(Rule1, Rule2: TMemberSet);
Begin
  Inherited Create(Rule2, Rule1);
  fPrintSeperator := RuleSelectorToString(rsrightOf);
End;

Function TRightOf.Print: String;
Var
  i: integer;
  s: String;
Begin
  result := inttostr(Number) + ' : '; // Inherited geht nicht, da es ja das Left of aufruft :(
  s := '';
  For i := 0 To EinsteinHeight - 1 Do Begin
    If fRule2[i] <> Nichts Then Begin
      If s <> '' Then s := s + ', ';
      s := s + ResolveXY(i, fRule2[i]);
    End;
  End;
  result := result + s + ' ' + fPrintSeperator + ' ';
  s := '';
  For i := 0 To EinsteinHeight - 1 Do Begin
    If fRule1[i] <> Nichts Then Begin
      If s <> '' Then s := s + ', ';
      s := s + ResolveXY(i, fRule1[i]);
    End;
  End;
  result := result + s;
End;

{ TRightDistanceDependsOf }

Constructor TRightDistanceDependsOf.Create(Rule1, Rule2: TMemberSet);
Begin
  Inherited Create(Rule2, Rule1);
  fPrintSeperator := RuleSelectorToString(rsDistanceRightOf);
End;

Function TRightDistanceDependsOf.Print: String;
Var
  i: integer;
  s: String;
Begin
  result := inttostr(Number) + ' : '; // Inherited geht nicht, da es ja das Left of aufruft :(
  s := '';
  For i := 0 To EinsteinHeight - 1 Do Begin
    If fRule2[i] <> Nichts Then Begin
      If s <> '' Then s := s + ', ';
      s := s + ResolveXY(i, fRule2[i]);
    End;
  End;
  result := result + s + ' ' + fPrintSeperator + ' ';
  s := '';
  For i := 0 To EinsteinHeight - 1 Do Begin
    If fRule1[i] <> Nichts Then Begin
      If s <> '' Then s := s + ', ';
      s := s + ResolveXY(i, fRule1[i]);
    End;
  End;
  result := result + s;
End;

{ TEliminate }

Constructor TEliminate.Create(Rule1, Rule2: TMemberSet);
Begin
  Inherited Create(Rule1, Rule2);
  fPrintSeperator := RuleSelectorToString(rsEliminate);
End;

Function TEliminate.ApplyRule: Boolean;
Var
  r1, r2: TMemberSet;
  i, x, y: Integer;
  bool: Boolean;
Begin
  result := false;
  r1 := fRule1;
  r2 := fRule2;
  For i := 0 To 1 Do Begin
    For x := 0 To EinsteinWidth - 1 Do Begin
      bool := true;
      For y := 0 To EinsteinHeight - 1 Do Begin
        // Suchen einer Inkompatiblen Stelle
        If (r1[y] <> Nichts) And (Not (data[x, y].Pencils[r1[y]])) Then Begin
          bool := false;
          break;
        End;
      End;
      //If Not bool Then Begin -- R1 passt nicht in x
      //End;
      If bool Then Begin // es gibt keine Konflickte, damit der untere Code aber arbeiten darf, muss mindestens ein Kriterium auch erfüllt sein
        bool := false;
        For y := 0 To EinsteinHeight - 1 Do Begin
          If (R1[y] <> Nichts) And (data[x, y].Value = R1[y]) Then Begin
            bool := true;
            break;
          End;
        End;
      End;
      If bool Then Begin // R1 passt in die Spalte x rein => Alles was R2 Definiert kann nun in der Spalte X gestrichen werden.
        For y := 0 To EinsteinHeight - 1 Do Begin
          If R2[y] <> Nichts Then Begin
            result := ClearPencil(x, y, r2[y]) Or result;
          End;
        End;
        // Wenn R1 in x passt, dann muss dies auch vollständig gehen, sonst wäre alles Falsch, also Sicherstellen, dass der Rest auch drin steht
        For y := 0 To EinsteinHeight - 1 Do Begin
          If (r1[y] <> Nichts) Then Begin // Diese zeile Provoziert einen Fehler, wenn data[x, y].value <> r1[y] und data[x, y].value <> nichts
            result := SetValue(x, y, r1[y]) Or result;
          End;
        End;
      End;
    End;
    r1 := fRule2;
    r2 := fRule1;
  End;
  If result Then Begin // Es hat sich was geändert, also neu auswerten
    NakedSingle;
  End;
End;

Function TEliminate.Plausible: boolean;
Var
  x, y, i: integer;
  r1, r2: TMemberSet;
  bool: Boolean;
Begin
  result := true;
  r1 := fRule1;
  r2 := fRule2;
  For i := 0 To 1 Do Begin
    // zuerst suchen nach einer Stelle an der R1 definitiv geht
    // Dann suchen ob R2 hier auch geht, wenn ja => Result := false
    For x := 0 To EinsteinWidth - 1 Do Begin
      bool := false;
      For y := 0 To EinsteinHeight - 1 Do Begin
        If (data[x, y].value <> Nichts) And (r1[y] = data[x, y].value) Then Begin // Eine der Regeln Passt hier
          bool := true;
          break;
        End;
      End;
      If bool Then Begin // R1 passt, also darf R2 nicht passen
        For y := 0 To EinsteinHeight - 1 Do Begin
          If (data[x, y].value <> Nichts) And (r2[y] = data[x, y].value) Then Begin // die andere Regel passt auch
            result := false;
            exit;
          End;
        End;
      End;
    End;
    r1 := fRule2;
    r2 := fRule1;
  End;
End;

{ TDistanceEliminate }

Constructor TDistanceEliminate.Create(Rule1, Rule2: TMemberSet);
Begin
  Inherited Create(Rule1, Rule2);
  fPrintSeperator := RuleSelectorToString(rsDistanceEliminate);
End;

Function TDistanceEliminate.ApplyRule: Boolean;
Var
  r1, r2: TMemberSet;
  i, x, y: Integer;
  bool: Boolean;
Begin
  result := false;
  r1 := fRule1;
  r2 := fRule2;
  For i := 0 To 1 Do Begin
    For x := 0 To EinsteinWidth - 1 Do Begin
      bool := true;
      For y := 0 To EinsteinHeight - 1 Do Begin
        // Suchen einer Inkompatiblen Stelle
        If (r1[y] <> Nichts) And (Not (data[x, y].Pencils[r1[y]])) Then Begin
          bool := false;
          break;
        End;
      End;
      //If Not bool Then Begin -- R1 passt nicht in x
      //End;
      If bool Then Begin // es gibt keine Konflickte, damit der untere Code aber arbeiten darf, muss mindestens ein Kriterium auch erfüllt sein
        bool := false;
        For y := 0 To EinsteinHeight - 1 Do Begin
          If (R1[y] <> Nichts) And (data[x, y].Value = R1[y]) Then Begin
            bool := true;
            break;
          End;
        End;
      End;
      If bool Then Begin // R1 passt in die Spalte x rein => Alles was R2 Definiert kann nun in der Spalte X +- Distance gestrichen werden.
        If x - Distance >= 0 Then Begin
          For y := 0 To EinsteinHeight - 1 Do Begin
            If R2[y] <> Nichts Then Begin
              result := ClearPencil(x - Distance, y, r2[y]) Or result;
            End;
          End;
        End;
        If x + Distance < EinsteinWidth Then Begin
          For y := 0 To EinsteinHeight - 1 Do Begin
            If R2[y] <> Nichts Then Begin
              result := ClearPencil(x + Distance, y, r2[y]) Or result;
            End;
          End;
        End;
        // Wenn R1 in x passt, dann muss dies auch vollständig gehen, sonst wäre alles Falsch, also Sicherstellen, dass der Rest auch drin steht
        For y := 0 To EinsteinHeight - 1 Do Begin
          If (r1[y] <> Nichts) Then Begin // Diese zeile Provoziert einen Fehler, wenn data[x, y].value <> r1[y] und data[x, y].value <> nichts
            result := SetValue(x, y, r1[y]) Or result; // wir ändern ja was, also auf jeden Raus geben, dass wir was gemacht haben
          End;
        End;
      End;
    End;
    r1 := fRule2;
    r2 := fRule1;
  End;
  If result Then Begin // Es hat sich was geändert, also neu auswerten
    NakedSingle;
  End;
End;

Function TDistanceEliminate.Plausible: boolean;
Var
  x, y, i: integer;
  r1, r2: TMemberSet;
  bool: Boolean;
Begin
  result := true;
  r1 := fRule1;
  r2 := fRule2;
  For i := 0 To 1 Do Begin
    // zuerst suchen nach einer Stelle an der R1 definitiv geht
    // Dann suchen ob R2 hier auch geht, wenn ja => Result := false
    For x := 0 To EinsteinWidth - 1 Do Begin
      bool := false;
      For y := 0 To EinsteinHeight - 1 Do Begin
        If (data[x, y].value <> Nichts) And (r1[y] = data[x, y].value) Then Begin // Eine der Regeln Passt hier
          bool := true;
          break;
        End;
      End;
      If bool Then Begin // R1 passt, also darf R2 nicht im Abstand passen
        For y := 0 To EinsteinHeight - 1 Do Begin
          If x - Distance >= 0 Then Begin
            If (data[x - Distance, y].value <> Nichts) And (r2[y] = data[x - Distance, y].value) Then Begin // die andere Regel passt auch
              result := false;
              exit;
            End;
          End;
          If x + Distance < EinsteinWidth Then Begin
            If (data[x + Distance, y].value <> Nichts) And (r2[y] = data[x + Distance, y].value) Then Begin // die andere Regel passt auch
              result := false;
              exit;
            End;
          End;
        End;
      End;
    End;
    r1 := fRule2;
    r2 := fRule1;
  End;
End;

{ TGuess }

Function TGuess.ApplyRule: Boolean;
Begin
  If Active Then Begin
    Result := Inherited ApplyRule;
  End
  Else Begin
    result := false;
  End;
End;

Function TGuess.Print: String;
Var
  i: integer;
  s: String;
Begin
  Result := inttostr(Number) + ' : '; // Geht nicht via Inherited, da wir ja von Depends On abgeleitet haben, dafür sparen wir den ApplyRule Code *g*
  s := '';
  For i := 0 To EinsteinHeight - 1 Do Begin
    If fRule[i] <> Nichts Then Begin
      If s <> '' Then s := s + ' ' + RuleSelectorToString(rsDependsOn) + ' ';
      s := s + ResolveXY(i, fRule[i]);
    End;
  End;
  result := result + 'Guess : ' + s;
End;

Procedure TGuess.UpdateGuess(Guess: TMemberSet);
Begin
  fRule := Guess;
End;

End.

