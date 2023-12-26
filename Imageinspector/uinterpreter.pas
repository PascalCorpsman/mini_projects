(******************************************************************************)
(* uInterpreter                                                    ??.??.???? *)
(*                                                                            *)
(* Version     : 0.04                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Implements a pascal interpreter                              *)
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
(* History     : 0.04 - Initial version                                       *)
(*               0.02 - Hinzufügen Single, TPixel                             *)
(*                      Abkapseln in tatsächlich eigentständige Klasse        *)
(*               0.03 - Unterstützen Proceduren / Externe Calls               *)
(*               0.04 - Strings                                               *)
(*                                                                            *)
(******************************************************************************)

(*
 * Usage:
 *
 *  Tinterpreter.create
 *  [Interpreter.RegisterExternalCall] -- Optional
 *  Tinterpreter.compile
 *   \-> Tinterpreter.errormessage
 *  Tinterpreter.CallFunction
 *
 * Known Bugs: - When parsing invalid code and throwing Errors, not all memory is freed correct.
 *             - Strings in Records will crash
 *             - Uncommenter does not Know anything about Strings a String like will crash '//'
 *
 * Übergabeparameter sind (leider) Strings, Structuren werden mittels ; getrennt
 * z.B. TPixel = 1.0;1.0;1.0;1.0
 *
 * Unterstützte Datentypen: Integer, Boolean, Single, TPixel
 *
 * Unterstützte Sprachkonstrukte (vergleich FreePascal):
 *                 | Konstrukt | Beschreibung                             |
 *                 --------------------------------------------------------
 *                   :=        | Zuweisung
 *                   record    | Felder (Dürfen aber nur Basistypen als Elemente haben)
 *                   Function  | Funktionsaufrufe (auch genestet)
 *                   Procedure | Procedure Aufrufe ! - Nocht nicht Vollständig !
 *                   if        | Bedingte Code Ausführung
 *                   for       | For-Schleife  (Absteigend und Aufsteigend)
 *                   while     | Kopfgesteuerte While-Schleife
 *
 * Unterstützte Operatoren:
 *                |  Operator     | Unterstützte Datentypen  | Ergebnistyp
 *                ------------------------------------------------------------
 * Bindung Stark->    - (unär)    | Integer, Single          | Integer, Single
 *                    + (unär)    | Integer, Single          | Integer, Single
 *                    trunc(unär) | Integer, Single          | Integer
 *                    round(unär) | Integer, Single          | Integer
 *                    /           | Integer, Single          | Single
 *                    div         | Integer                  | Integer
 *                    mod         | Integer                  | Integer
 *                    *           | Integer, Single          | Integer, Single
 *                    +           | Integer, Single          | Integer, Single
 *                    -           | Integer, Single          | Integer, Single
 *                    not         | Boolean                  | Boolean
 *                    and         | Boolean                  | Boolean
 *                    or          | Boolean                  | Boolean
 *                    xor         | Boolean                  | Boolean
 *                    =           | Integer, Single, Boolean | Boolean
 *                    <>          | Integer, Single          | Boolean
 *                    >           | Integer, Single          | Boolean
 *                    <           | Integer, Single          | Boolean
 *                    >=          | Integer, Single          | Boolean
 * Bindung Schwach->  <=          | Integer, Single          | Boolean
 *)
// Todo: Geschachtelte records zulassen [dazu müssten record strings in {} eingefasst werden, sonst ist der Parameter nicht mehr eindeutig] ?
// Todo: Array's zulassen ?
Unit uinterpreter;

{$MODE ObjFPC}{$H+}

{.$DEFINE debuggroutines}

{$IFDEF debuggroutines}
{$APPTYPE Console} // Sonst sieht man die Debugg ausgabe nich
{$ENDIF}

Interface

{$I ugenmathcalc.inc} // Wird nur eingebunden, damit die untenstehende Fehlermeldung generiert wird.

{$IFNDEF UseTokenizer}
Für GenMathCalc müssen die Defines: UseTokenizer und OOP_Callbacks aktiviert werden !
{$ENDIF}
{$IFNDEF OOP_Callbacks}
Für GenMathCalc müssen die Defines: UseTokenizer und OOP_Callbacks aktiviert werden !
{$ENDIF}

Uses sysutils, ugenmathcalc, utokenizer

{$IFDEF debuggroutines}
  , lclproc // Zum Debuggen ( DebugLn ), aber die Unit scheint noch deutlich mehr zu können !!
{$ENDIF}
  ;

Type

  Tinterpreter = Class; // Vorwärtsdeclaration

  (*
  Unser Basistoken von dem wir alles Ableiten
  *)

  { TCompiledToken }

  TCompiledToken = Class
  private
    fowner: Tinterpreter;
  public
    Constructor Create(Owner: Tinterpreter); virtual;
    Procedure Execute; virtual;
  End;

  (*
   * Intern: Zum Anfügen einer Variable an FVariables
   *)
  TName = Record
    value: String;
    line: integer;
  End;

  (*
   * ! Achtung!
   * Wer hier einen Typ hinzufügt muss das auch in TRecord.
   *)
  TVariablenType = (
    vtnone, // -- Ungültig
    vtbool, // = true / false [0 / 1] //Todo: Umbauen wie Float
    vtint, // int32 (mindestens)      //Todo: Umbauen wie Float
    vtfloat, // = Single
    vtString, // = String
    vtfield, // Der Datentyp ist ein Record als ganzes
    vtfieldelement // Der Datentyp ist ein Feld eines Records und
    );

  TSringArray = Array Of String;

  PVariable = ^TVariable;

  PVariableArray = Array Of PVariable;

  TVariable = Record
    Funktionsaufrufzeiger: Boolean; // Wenn das True ist, dann ist diese Variable ein Aufruf der entsprechenden Function und dieser steht in Line
    uebergabeparameter: Array Of PCalcTree; // Liste der Übergebenen Variablen ...
    Name: String; // Name der Variable
    Typ: TVariablenType; // Typ der Variablen
    Value: PtrInt; // 0 = false, >0 = true, int steht im Wert, bei Single, String oder TPixel = Pointer auf datentyp
    Line: Integer; // Die Code zeile in der die Variable declariert wurde (oder -1)
    RecordFieldIndex: integer; // Für Typ = field, fieldelement
    RecordName: String; // Für Typ = field, fieldelement
  End;

  TVariableArray = Array Of TVariable;

  TCompiledCode = Array Of TCompiledToken;

  TParam = Record
    Name: String;
    Typ: TVariablenType;
    RecordName: String; // Wenn Typ = field oder fieldelement ist, dann steht hier wie der RecordTypeName ist, damit er in fTypes gefunden werden kann
  End;

  TProcCallback = Function(Const Params: Array Of String): String Of Object;

  TExternalCall = Record
    Proc: TProcCallback;
    Params: Array Of TVariablenType;
    Name: String;
    restype: TVariablenType;
  End;

  PExternalCall = ^TExternalCall;

  { TProcedure }

  TProcedure = Class(TCompiledToken)
  private
    Nested: Boolean; // True, wenn es sich um eine Genestete Funktion handelt.
    Inners: Array Of TParam;
    LocalVarcount: Integer; // Gibt die Anzahl der Localen Variablen wieder
    InnerCode: TCompiledCode; // Der Code den die Funktion ausführt ..
  public
    // TODO: wider Private machen !
    Name: String; // Name der Function // -- Wieder Private machen
    ParamVarcount: Integer; // Gibt die Anzahl der Übergabeparameter mit an. // -- Wieder Private machen
    Params: Array Of TParam; // -- Wieder Private machen

    Deklarationline: Integer;
    Constructor create(Owner: Tinterpreter); override;
    Destructor Destroy; override;
    Procedure Execute; override;
  End;

  { TFunction }

  TFunction = Class(TProcedure)
  private
  public
    Result_: TParam; // Der Ergebistyp
  End;

  { TCall }

  TCall = Class(TProcedure)
  private
    fParamCalcTrees: Array Of PCalcTree;
  public
    Constructor create(Owner: Tinterpreter); override;
    Destructor Destroy; override;
    Procedure Execute; override;
  End;

  TProcedureclass = Class Of TProcedure;

  { TIfConstruct }

  TIfConstruct = Class(TCompiledToken)
  private
  public
    Bedingung: PCalcTree;
    TrueCode: TCompiledCode;
    FalseCode: TCompiledCode;
    Constructor create(Owner: Tinterpreter); override;
    Destructor Destroy; override;
    Procedure Execute; override;
  End;

  { TAssignment }

  TAssignment = Class(TCompiledToken)
  private
  public
    Ziel: String; // Name der Zielvariablen
    Ausdruck: PCalcTree; // Ausdruck der zugewiesen wird
    Constructor create(Owner: Tinterpreter); override;
    Destructor Destroy; override;
    Procedure Execute; override;
  End;

  { TForLoop }

  TForLoop = Class(TCompiledToken)
  private
  public
    Laufvariable: String;
    von, bis: PCalcTree;
    InnerCode: TCompiledCode;
    Constructor create(Owner: Tinterpreter); override;
    Destructor Destroy; override;
    Procedure Execute; override;
  End;

  { TForDownLoop }

  TForDownLoop = Class(TForLoop)
  private
  public
    Procedure Execute; override;
  End;

  { TWhile }

  TWhile = Class(TCompiledToken)
  private
  public
    Bedingung: PCalcTree;
    InnerCode: TCompiledCode;
    Constructor create(Owner: Tinterpreter); override;
    Destructor Destroy; override;
    Procedure Execute; override;
  End;

  TField = Record
    Name: String;
    Typ: TVariablenType;
    value: PtrInt;
  End;

  { TRecord }

  TRecord = Class
  private
    fFields: Array Of TField;
  public
    Name: String;
    Constructor Create();
    Destructor Destroy; override;
    Function AddField(ElementName: String; ElementType: TVariablenType): Boolean;
    Procedure CloneFrom(Const Source: TRecord);
    Procedure CopyValuesFrom(Const Source: TRecord);
    Procedure SetFieldValueByString(Index: integer; Value: String);
    Procedure SetFieldValueBySingle(index: integer; Value: Single);
    Function getFieldValueAsSingle(Index: integer): Single;

    Function ValuesToString: String;
  End;

  { Tinterpreter }

  Tinterpreter = Class
  private
    fExternalProcedures: Array Of TExternalCall;
    FRecords: Array Of TRecord;
    FVariables: PVariableArray;
    Ftokens: TTokenarray;
    fatoken, faLowertoken: String;
    FatokenLine, Fatokenindex: Integer;
    ferror: Boolean;
    FFunctionlist: Array Of TProcedure;

    SolverErrorString: String;
    Solver: TGenMathCalc;

    Function StringToType(Value: String; valueType: TVariablenType; RecordName: String): PtrInt;
    Function CheckVarname(Value: String): boolean;
    Function CheckFunctionName(value: String): Boolean;

    Function Eat(Value: String): Boolean;
    Procedure EatToken;
    Function EatVariablewithType: integer;
    Function EatFunProc(tp: TProcedureclass): TProcedure; // Helper Eat Function for Eat Procedure / Eat Function
    Function EatProcedure: TProcedure;
    Function Eatfunction: TFunction;
    Function EatInner: TCompiledCode;
    Function EatIFConstruct: TIfConstruct;
    Function EatFor: TForLoop;
    Function EatWhile: TWhile;
    Function EatAusdruck(Abbruchtoken: Array Of String): PCalcTree;
    Function EatZuweisung: TAssignment;
    Function EatCall: TCall;
    Procedure EatType;
    Procedure EatRecord;

    Procedure ClearAll;
    Procedure Abbruch(Value: String);
    Function Compileh(count: integer): TCompiledCode;
    Function PVariableToSingle(Value: Pointer): Single;
    Function ResolveValue(Value: PVariable): PtrInt;
    Function ValueIsFunctionName(Value: String): Boolean;
    Function ValueIsCall(Value: String): Boolean;
    Function IdentExists(Value: String; Line: Integer): Boolean; // Gibt True zurück wenn Value in der Variablenliste auftaucht, und sichtbar ist
    Procedure SetVar(Name: String; Value: PtrInt; ValueType: TVariablenType; index: integer);
    Function GetVarVal(Name: String): PtrInt;
    Function AppendVariable(VarName: TName; VarType: String): TVariablenType; // Fügt fVariables eine Variable Varname, vom Typ Vartype hinzu
    Function FunctionResultType(Functionname: String): TVariablenType; // Gibt den Result Datentyp der angefragten Funktion zurück (nur intern)

    (*
     * Debugg Routinen, diese dürften eigentlich nie Aufgerufen werden, und können in der Finalen Version verlöscht werden.
     *)
{$IFDEF debuggroutines}
    Procedure PlottAllVariables(Caption: String = ''); // Gibt auf die Konsole Alle Variablen aus
{$ENDIF}

    (*
     * Functionen für GenMathCalc
     *)
    // Float/int -> Float/Int
    Function uPlus_(Value1: Pointer): Pointer;
    Function PLus_(Value1, Value2: Pointer): Pointer;
    Function uMinus_(Value1: Pointer): Pointer;
    Function Minus_(Value1, Value2: Pointer): Pointer;
    Function Mul_(Value1, Value2: Pointer): Pointer;
    Function div_(Value1, Value2: Pointer): Pointer;
    Function floatdiv_(Value1, Value2: Pointer): Pointer;
    Function Mod_(Value1, Value2: Pointer): Pointer;
    Function Round_(Value1: Pointer): Pointer;
    Function Trunc_(Value1: Pointer): Pointer;

    // Float -> Bool
    Function Equal(Value1, Value2: Pointer): Pointer;
    Function NotEqual(Value1, Value2: Pointer): Pointer;
    Function Greater(Value1, Value2: Pointer): Pointer;
    Function Lesser(Value1, Value2: Pointer): Pointer;
    Function GreaterOrEqual(Value1, Value2: Pointer): Pointer;
    Function LesserOrEqual(Value1, Value2: Pointer): Pointer;

    // Bool -> Bool
    Function not_(Value1: Pointer): Pointer;
    Function SAND_(Value1, Value2: Pointer; Var ShortCircuit: Boolean): Pointer;
    Function SOr_(Value1, Value2: Pointer; Var ShortCircuit: Boolean): Pointer;
    Function XOR_(Value1, Value2: Pointer): Pointer;

    Procedure InitSolver;
    Procedure FreeSolver;
    Function CreateVariable(Value: String): Pointer;
    Procedure DisposeVariable(Value: Pointer);

    Function isCallableProcedure(value: String): Boolean;
    Function isKeyword(Value: String): Boolean;
  public
    Errormessage: String; // Todo : Als Property Machen

    Constructor create;
    Destructor Destroy; override;

    Function Compile(SoureCode: String): boolean; // Compiliert den Quellcode und initialisiert damit CallFunction / GetFunctionList / GiveFirstFunction

    Function CallFunction(Functionname: String; Parameter: TSringArray): String; // Ruft eine Funktion mit den parametern auf und gibt das Ergebnis zurück

    Function GetFunctionList: TStringArray; // Gibt eine Liste aller Verfügbaren Funktionen und deren erwarteter Parameter

    Function GiveFirstFunction: TProcedure; // Gibt die erste Definierte Funktion zurück (eigentlich ein Hack)

    Function RegisterExternalCall(PName: String; params: Array Of TVariablenType; ResType: TVariablenType; Callback: TProcCallback): Boolean;
  End;

Function VarTypeToStr(Value: TVariablenType): String;
Function StrToVarType(Value: String): TVariablenType;

Implementation

Uses uncommenter;

Var
  (*
   * Lexer, wird im Initialize und Finalize behandelt
   *)
  Tok: TTokenizer = Nil;
  unc: TUnCommenter;
  forma: TFormatSettings;

Procedure Nop; // Nur zum Debuggen
Begin

End;

Function VarTypeToStr(Value: TVariablenType): String;
Begin
  Case Value Of
    vtbool: result := 'boolean';
    vtint: result := 'integer';
    vtfloat: result := 'single';
    vtString: result := 'string';
  Else Begin
      Raise Exception.Create('Unknown Type.');
    End;
  End;
End;

Function StrToVarType(Value: String): TVariablenType;
Begin
  result := vtfield;
  If lowercase(Value) = 'boolean' Then result := vtbool;
  If lowercase(Value) = 'integer' Then result := vtint;
  If lowercase(Value) = 'single' Then result := vtfloat;
  If lowercase(Value) = 'string' Then result := vtString;
End;

{ TCall }

Constructor TCall.create(Owner: Tinterpreter);
Begin
  Inherited create(Owner);
  fParamCalcTrees := Nil;
End;

Destructor TCall.Destroy;
Var
  i: Integer;
Begin
  For i := 0 To high(fParamCalcTrees) Do Begin
    fowner.solver.FreeCalcTree(fParamCalcTrees[i]);
  End;
  setlength(fParamCalcTrees, 0);
  Inherited Destroy;
End;

Procedure TCall.Execute;
Var
  pst: ^String;
  ps: PSingle;
  dummy: Boolean;
  i: Integer;
  paras: TStringArray;
  tmp: PVariable;
  r: TRecord;
Begin
  // 1. Auswerten Aller Parameter
  setlength(paras, Length(fParamCalcTrees));
  For i := 0 To high(fParamCalcTrees) Do Begin
    tmp := fowner.solver.calc(fParamCalcTrees[i], dummy);
    Case tmp^.Typ Of
      vtbool, vtint: Begin
          paras[i] := inttostr(fowner.ResolveValue(tmp));
        End;
      vtString: Begin
          pst := PtrIntToPtr(fowner.ResolveValue(tmp));
          paras[i] := pst^;
        End;
      vtfloat: Begin
          ps := PtrIntToPtr(fowner.ResolveValue(tmp));
          paras[i] := floattostr(ps^, forma);
        End;
      vtfieldelement: Begin
          r := TRecord(PtrIntToPtr(fowner.ResolveValue(tmp)));
          paras[i] := format('%f', [r.getFieldValueAsSingle(tmp^.RecordFieldIndex)], forma);
        End;
      vtfield: Begin
          r := TRecord(PtrIntToPtr(fowner.ResolveValue(tmp)));
          paras[i] := r.ValuesToString;
        End;
    Else Begin
        Raise exception.create('ResolveValue, missing type implementation.');
      End;
    End;
    If Not dummy Then Begin
      fowner.DisposeVariable(tmp);
    End;
  End;
  // 2. Call durch den fowner;
  fowner.CallFunction(Name, paras);
  setlength(paras, 0);
End;

{ TProcedure }

Constructor TProcedure.create(Owner: Tinterpreter);
Begin
  Inherited;
  setlength(innerCode, 0);
  setlength(Params, 0);
  setlength(Inners, 0);
  ParamVarcount := 0;
  LocalVarcount := 0;
End;

Destructor TProcedure.Destroy;
Var
  i: Integer;
Begin
  setlength(Params, 0);
  setlength(Inners, 0);
  For i := 0 To high(InnerCode) Do
    innercode[i].free;
  setlength(innerCode, 0);
End;

Procedure TProcedure.Execute;
Var
  i: Integer;
Begin
  For i := 0 To high(InnerCode) Do
    innercode[i].execute;
End;

{ TRecord }

Constructor TRecord.Create();
Begin
  Inherited create();
  fFields := Nil;
End;

Destructor TRecord.Destroy;
Var
  i: integer;
  ps: PSingle;
  pstr: ^String;
Begin
  For i := 0 To high(fFields) Do Begin
    Case fFields[i].Typ Of
      vtbool, vtint: Begin
          // Nichts
        End;
      vtFloat: Begin
          ps := PtrIntToPtr(fFields[i].value);
          Dispose(ps);
        End;
      vtString: Begin
          pstr := PtrIntToPtr(fFields[i].value);
          pstr^ := '';
          Dispose(pstr);
        End
    Else Begin
        Raise Exception.Create('Record field type not supported.');
      End;
    End;
  End;
  setlength(fFields, 0);
End;

Function TRecord.AddField(ElementName: String; ElementType: TVariablenType
  ): Boolean;
Var
  ps: PSingle;
  pstr: ^String;
Begin
  result := false;
  // Todo : Check auf Gültigen Namen (Keine Doppelten und so ..)
  setlength(fFields, high(fFields) + 2);
  fFields[high(fFields)].Name := ElementName;
  fFields[high(fFields)].Typ := ElementType;
  fFields[high(fFields)].value := 0;
  Case ElementType Of
    vtbool, vtint: Begin
        // Nichts
      End;
    vtfloat: Begin
        new(ps);
        ps^ := 0.0;
        fFields[high(fFields)].value := PtrToPtrInt(ps);
      End;
    vtString: Begin
        new(pstr);
        pstr^ := '';
        fFields[high(fFields)].value := PtrToPtrInt(pstr);
      End
  Else Begin
      Raise Exception.Create('Record field type not supported.');
    End;
  End;
  result := true;
End;

Procedure TRecord.CloneFrom(Const Source: TRecord);
Var
  i: integer;
  pps, ps: PSingle;
  ppstr, pstr: ^String;
Begin
  Name := Source.Name;
  setlength(fFields, length(Source.fFields));
  For i := 0 To high(fFields) Do Begin
    fFields[i].Name := Source.fFields[i].Name;
    fFields[i].Typ := Source.fFields[i].Typ;
    Case fFields[i].Typ Of
      vtbool, vtint: Begin
          fFields[i].value := Source.fFields[i].value;
        End;
      vtfloat: Begin
          new(ps);
          pps := PtrIntToPtr(Source.fFields[i].value);
          ps^ := pps^;
          fFields[i].value := PtrToPtrInt(ps);
        End;
      vtString: Begin
          new(pstr);
          ppstr := PtrIntToPtr(Source.fFields[i].value);
          pstr^ := ppstr^;
          fFields[i].value := PtrToPtrInt(pstr);
        End
    Else Begin
        Raise Exception.Create('Record field type not supported.');
      End;
    End;
  End;
End;

Procedure TRecord.CopyValuesFrom(Const Source: TRecord);
Var
  i: Integer;
  pps, ps: PSingle;
  ppstr, pstr: ^String;
Begin
  // Todo: TRecord.CopyValuesFrom(
  If Name <> Source.Name Then Begin
    Raise Exception.Create('Trying to copy values from "' + Source.Name + '" to " ' + Name);
  End;
  // Todo: Hier könnten durchaus noch die ein oder Andere Prüfungen stattfinden ..
  For i := 0 To high(fFields) Do Begin
    Case fFields[i].Typ Of
      vtbool, vtint: Begin
          fFields[i].value := Source.fFields[i].value;
        End;
      vtfloat: Begin
          pps := PtrIntToPtr(Source.fFields[i].value);
          ps := PtrIntToPtr(fFields[i].value);
          ps^ := pps^;
        End;
      vtString: Begin
          ppstr := PtrIntToPtr(Source.fFields[i].value);
          pstr := PtrIntToPtr(fFields[i].value);
          pstr^ := ppstr^;
        End
    Else Begin
        Raise Exception.Create('Record field type not supported.');
      End;
    End;
  End;
End;

Procedure TRecord.SetFieldValueByString(Index: integer; Value: String);
Var
  ps: PSingle;
  pstr: ^String;
Begin
  // Achtung ist fast identisch zu Tinterpreter.StringToType
  value := lowercase(value);
  Case fFields[Index].Typ Of
    vtbool, vtint: Begin
        If pos(';', Value) <> 0 Then Begin
          Raise exception.Create('Invalid value for bool / int: ' + Value);
        End;
        If lowercase(Value) = 'true' Then Begin
          fFields[Index].value := 1;
        End
        Else Begin
          If lowercase(Value) = 'false' Then Begin
            fFields[Index].value := 0;
          End
          Else Begin
            fFields[Index].value := strtoint(value);
          End;
        End;
      End;
    vtfloat: Begin
        If pos(';', Value) <> 0 Then Begin
          Raise exception.Create('Invalid value for float: ' + Value);
        End;
        ps := PtrIntToPtr(fFields[Index].value);
        ps^ := StrToFloat(value, forma);
      End;
    vtString: Begin
        If pos(';', Value) <> 0 Then Begin
          Raise exception.Create('Invalid value for float: ' + Value);
        End;
        pstr := PtrIntToPtr(fFields[Index].value);
        // TODO: Ähh stimmt das schon so ?
        pstr^ := value;
      End
  Else Begin
      Raise Exception.Create('Record field type not supported.');
    End;
  End;
End;

Procedure TRecord.SetFieldValueBySingle(index: integer; Value: Single);
Var
  ps: PSingle;
Begin
  Case fFields[Index].Typ Of
    vtbool: Begin
        If (value <> 0) Then Begin
          fFields[Index].value := 1;
        End
        Else Begin
          fFields[Index].value := 0;
        End;
      End;
    vtint: Begin
        fFields[Index].value := round(value);
      End;
    vtfloat: Begin
        ps := PtrIntToPtr(fFields[Index].value);
        ps^ := value;
      End;
  Else Begin
      Raise Exception.Create('Record field type not supported.');
    End;
  End;
End;

Function TRecord.getFieldValueAsSingle(Index: integer): Single;
Var
  ps: PSingle;
Begin
  Case fFields[Index].Typ Of
    vtbool, vtint: Begin
        result := fFields[Index].value;
      End;
    vtfloat: Begin
        ps := PtrIntToPtr(fFields[Index].value);
        result := ps^;
      End;
  Else Begin
      Raise Exception.Create('Record field type not supported.');
    End;
  End;
End;

Function TRecord.ValuesToString: String;
Var
  i: Integer;
  ps: PSingle;
  pstr: ^String;
Begin
  result := '';
  For i := 0 To high(fFields) Do Begin
    If i <> 0 Then result := result + ';';
    Case fFields[i].Typ Of
      vtbool: Begin
          If fFields[i].value <> 0 Then
            result := result + 'true'
          Else
            result := result + 'false';
        End;
      vtint: Begin
          result := result + inttostr(fFields[i].value);
        End;
      vtfloat: Begin
          ps := PtrIntToPtr(fFields[i].value);
          result := result + Format('%f', [ps^], forma);
        End;
      vtString: Begin
          pstr := PtrIntToPtr(fFields[i].value);
          result := result + pstr^;
        End
    Else Begin
        Raise Exception.Create('Record field type not supported.');
      End;
    End;
  End;
End;

Function Tinterpreter.StringToType(Value: String; valueType: TVariablenType;
  RecordName: String): PtrInt;
Var
  ps: PSingle;
  pstr: ^String;
  r: TRecord;
  i, j: integer;
Begin
  // Achtung ist fast Identisch zu TRecord.SetFieldValueByString
  Case valueType Of
    vtbool, vtint: Begin
        If pos(';', Value) <> 0 Then Begin
          Raise exception.Create('Invalid value for bool / int: ' + Value);
        End;
        If lowercase(Value) = 'true' Then Begin
          result := 1;
        End
        Else Begin
          If lowercase(Value) = 'false' Then Begin
            result := 0;
          End
          Else Begin
            result := strtoint(Value);
          End;
        End;
      End;
    vtfloat: Begin
        If pos(';', Value) <> 0 Then Begin
          Raise exception.Create('Invalid value for float: ' + Value);
        End;
        new(ps);
        ps^ := strtofloat(value, forma);
        result := PtrToPtrInt(ps);
      End;
    vtString: Begin
        If pos(';', Value) <> 0 Then Begin // TODO: Das ist so natürlich Quatsch, da es ";" in einem String verbietet.
          Raise exception.Create('Invalid value for float: ' + Value);
        End;
        new(pstr);
        pstr^ := value;
        result := PtrToPtrInt(pstr);
      End;
    vtfield: Begin
        For i := 0 To high(FRecords) Do Begin
          If lowercase(FRecords[i].Name) = lowercase(RecordName) Then Begin
            r := TRecord.create();
            r.CloneFrom(FRecords[i]);
            value := value + ';';
            For j := 0 To high(r.fFields) Do Begin
              If value = '' Then Begin
                Raise exception.Create('Missing value for ' + RecordName);
              End;
              r.SetFieldValueByString(j, copy(value, 1, pos(';', value) - 1));
              delete(value, 1, pos(';', value));
            End;
            If value <> '' Then Begin
              Raise exception.Create('Defined to much fields for ' + RecordName);
            End;
            result := PtrToPtrInt(r);
            exit;
          End;
        End;
        Raise exception.create('Unknown datatype: ' + RecordName);
      End;
  Else Begin
      Raise Exception.Create('Unknown valuetype.');
    End;
  End;
End;

(*
Liefert die Position von Substring die als Letztes in S vorkommt
*)

Function posrev(Substr: String; S: String): Integer;
Var
  i, j: Integer;
  b: Boolean;
Begin
  result := 0;
  i := length(s) - length(Substr) + 1;
  While i > 0 Do Begin
    b := True;
    For j := 1 To Length(Substr) Do
      If s[i - 1 + j] <> Substr[j] Then Begin
        b := False;
        break;
      End;
    If b Then Begin
      result := i;
      exit;
    End;
    dec(i);
  End;
End;

(******************************************************************************)

{ Tinterpreter }

Constructor Tinterpreter.create;
Begin
  Inherited;
  setlength(fExternalProcedures, 0);
  setlength(FFunctionlist, 0);
  setlength(FVariables, 0);
  setlength(FRecords, 0);
  InitSolver;
End;

Destructor Tinterpreter.Destroy;
Begin
  clearall;
  FreeSolver;
End;

Function Tinterpreter.ResolveValue(Value: PVariable): PtrInt;
Var
  paras: TSringArray;
  i: Integer;
  b: Boolean;
  tmp: PVariable;
  ps: pSingle;
  pstr: ^String;
  r: TRecord;
Begin
  (*
  ist Value ein Funktionsaufruf wirds aufwendiger
  *)
  If Value^.Funktionsaufrufzeiger Then Begin
    setlength(paras, high(Value^.uebergabeparameter) + 1);
    For i := 0 To high(paras) Do Begin
      tmp := Solver.Calc(Value^.uebergabeparameter[i], b);
      Case tmp^.Typ Of
        vtbool, vtint: Begin
            paras[i] := inttostr(ResolveValue(tmp));
          End;
        vtfloat: Begin
            ps := PtrIntToPtr(ResolveValue(tmp));
            paras[i] := floattostr(ps^, forma);
          End;
        vtString: Begin
            pstr := PtrIntToPtr(ResolveValue(tmp));
            paras[i] := pstr^;
          End;
        vtfieldelement: Begin
            r := TRecord(PtrIntToPtr(ResolveValue(tmp)));
            paras[i] := format('%f', [r.getFieldValueAsSingle(tmp^.RecordFieldIndex)], forma);
          End;
        vtfield: Begin
            r := TRecord(PtrIntToPtr(ResolveValue(tmp)));
            paras[i] := r.ValuesToString;
          End;
      Else Begin
          Raise exception.create('ResolveValue, missing type implementation.');
        End;
      End;
      If Not b Then Begin
        DisposeVariable(tmp);
      End;
    End;
    result := StringToType(CallFunction(Value^.Name, paras), FunctionResultType(Value^.Name), value^.RecordName);
    setlength(paras, 0);
  End
  Else Begin
    // Kein Funktionsaufruf dann gehts einfach
    If Value^.name = 'const' Then
      Result := Value^.Value
    Else
      result := getVarVal(Value^.name);
  End;
End;

Function PtrIntToSingle(Value: PtrInt; valueType: TVariablenType; Index: integer): Single;
Var
  ps: psingle;
  r: TRecord;
Begin
  Case valueType Of
    vtint, vtbool: result := Value;
    vtfieldelement: Begin
        r := TRecord(PtrIntToPtr(Value));
        result := r.getFieldValueAsSingle(index);
      End;
    vtfloat: Begin
        ps := PtrIntToPtr(Value);
        result := ps^;
      End;
  Else Begin
      Raise Exception.Create('PtrIntToSingle missing type.');
    End;
  End;
End;

Function Tinterpreter.PVariableToSingle(Value: Pointer): Single;
Var
  res: PtrInt;
  pv: PVariable;
  ps: PSingle;
Begin
  res := ResolveValue(value);
  pv := Value;
  result := PtrIntToSingle(res, pv^.Typ, pv^.RecordFieldIndex);
  (*
   * Wenn ResolveValue ein Funktionsaufruf ist, dann muss hier das Ergebnis
   * Auch wieder frei gegeben werden.
   *)
  If pv^.Funktionsaufrufzeiger Then Begin
    Case pv^.Typ Of
      vtfieldelement, vtbool, vtint: Begin
          // Nichts
        End;
      vtfloat: Begin
          ps := PtrIntToPtr(res);
          dispose(ps);
        End;
    Else Begin
        Raise Exception.Create('Tinterpreter.PVariableToSingle missing type.');
      End;
    End;
  End;
End;

Function Tinterpreter.Greater(Value1, Value2: Pointer): Pointer;
Var
  pv1, pv2, p: PVariable;
  v1, v2: Single;
Begin
  pv1 := value1;
  pv2 := value2;
  // > geht nur mit Scalaren
  If (pv1^.Typ In [vtfield, vtString]) Or
    (pv2^.Typ In [vtfield, vtString]) Then Begin
    Raise exception.create('Error invalid Type for ">"');
  End;
  new(p);
  p^.Funktionsaufrufzeiger := false;
  setlength(p^.uebergabeparameter, 0);
  p^.Name := 'const';
  p^.Line := -1;
  v1 := PVariableToSingle(value1);
  v2 := PVariableToSingle(value2);
  p^.Typ := vtbool;
  p^.Value := ord(v1 > v2);
  result := p;
End;

Function Tinterpreter.Equal(Value1, Value2: Pointer): Pointer;
Var
  pv1, pv2, p: PVariable;
  v1, v2: Single;
  ps1, ps2: ^String;
Begin
  pv1 := value1;
  pv2 := value2;
  // = geht nur mit Scalaren
  If (pv1^.Typ In [vtfield]) Or
    (pv2^.Typ In [vtfield]) Then Begin
    Raise exception.create('Error invalid Type for "="');
  End;
  If ((pv1^.Typ = vtString) And (pv2^.Typ <> vtString)) Or
    ((pv1^.Typ <> vtString) And (pv2^.Typ = vtString)) Then Begin
    Raise exception.create('Error invalid Type for "="');
  End;
  new(p);
  p^.Funktionsaufrufzeiger := false;
  setlength(p^.uebergabeparameter, 0);
  p^.Name := 'const';
  p^.Line := -1;
  If (pv1^.Typ = vtString) Then Begin // String Vergleich
    ps1 := PtrIntToPtr(ResolveValue(pv1));
    ps2 := PtrIntToPtr(ResolveValue(pv2));
    p^.Typ := vtbool;
    p^.Value := ord(ps1^ = ps2^);
  End
  Else Begin
    v1 := PVariableToSingle(value1);
    v2 := PVariableToSingle(value2);
    p^.Typ := vtbool;
    p^.Value := ord(v1 = v2);
  End;
  result := p;
End;

Function Tinterpreter.NotEqual(Value1, Value2: Pointer): Pointer;
Var
  pv1, pv2, p: PVariable;
  v1, v2: Single;
  ps1, ps2: ^String;
Begin
  pv1 := value1;
  pv2 := value2;
  // * geht nur mit Scalaren
  // Todo : Eigentlich könnte man das auch für TPixel machen :-)
  If (pv1^.Typ In [vtfield]) Or
    (pv2^.Typ In [vtfield]) Then Begin
    Raise exception.create('Error invalid Type for "<>"');
  End;
  If ((pv1^.Typ = vtString) And (pv2^.Typ <> vtString)) Or
    ((pv1^.Typ <> vtString) And (pv2^.Typ = vtString)) Then Begin
    Raise exception.create('Error invalid Type for "<>"');
  End;
  new(p);
  p^.Funktionsaufrufzeiger := false;
  setlength(p^.uebergabeparameter, 0);
  p^.Name := 'const';
  p^.Line := -1;
  If (pv1^.Typ = vtString) Then Begin // String Vergleich
    ps1 := PtrIntToPtr(ResolveValue(pv1));
    ps2 := PtrIntToPtr(ResolveValue(pv2));
    p^.Typ := vtbool;
    p^.Value := ord(ps1^ <> ps2^);
  End
  Else Begin
    v1 := PVariableToSingle(value1);
    v2 := PVariableToSingle(value2);
    p^.Typ := vtbool;
    p^.Value := ord(v1 <> v2);
  End;
  result := p;
End;

Function Tinterpreter.Lesser(Value1, Value2: Pointer): Pointer;
Var
  pv1, pv2, p: PVariable;
  v1, v2: Single;
Begin
  pv1 := value1;
  pv2 := value2;
  // * geht nur mit Scalaren
  If (pv1^.Typ In [vtfield, vtString]) Or
    (pv2^.Typ In [vtfield, vtString]) Then Begin
    Raise exception.create('Error invalid Type for "<"');
  End;
  new(p);
  p^.Funktionsaufrufzeiger := false;
  setlength(p^.uebergabeparameter, 0);
  p^.Name := 'const';
  p^.Line := -1;
  v1 := PVariableToSingle(value1);
  v2 := PVariableToSingle(value2);
  p^.Typ := vtbool;
  p^.Value := ord(v1 < v2);
  result := p;
End;

Function Tinterpreter.GreaterOrEqual(Value1, Value2: Pointer): Pointer;
Var
  pv1, pv2, p: PVariable;
  v1, v2: Single;
Begin
  pv1 := value1;
  pv2 := value2;
  // * geht nur mit Scalaren
  If (pv1^.Typ In [vtfield, vtString]) Or
    (pv2^.Typ In [vtfield, vtString]) Then Begin
    Raise exception.create('Error invalid Type for ">="');
  End;
  new(p);
  p^.Funktionsaufrufzeiger := false;
  setlength(p^.uebergabeparameter, 0);
  p^.Name := 'const';
  p^.Line := -1;
  v1 := PVariableToSingle(value1);
  v2 := PVariableToSingle(value2);
  p^.Typ := vtbool;
  p^.Value := ord(v1 >= v2);
  result := p;
End;

Function Tinterpreter.LesserOrEqual(Value1, Value2: Pointer): Pointer;
Var
  pv1, pv2, p: PVariable;
  v1, v2: Single;
Begin
  pv1 := value1;
  pv2 := value2;
  // * geht nur mit Scalaren
  If (pv1^.Typ In [vtfield, vtString]) Or
    (pv2^.Typ In [vtfield, vtString]) Then Begin
    Raise exception.create('Error invalid Type for "<="');
  End;
  new(p);
  p^.Funktionsaufrufzeiger := false;
  setlength(p^.uebergabeparameter, 0);
  p^.Name := 'const';
  p^.Line := -1;
  v1 := PVariableToSingle(value1);
  v2 := PVariableToSingle(value2);
  p^.Typ := vtbool;
  p^.Value := ord(v1 <= v2);
  result := p;
End;

(*
Short Circuit AND
*)

Function Tinterpreter.SAND_(Value1, Value2: Pointer; Var ShortCircuit: Boolean
  ): Pointer;
Var
  v1, v2, p: PVariable;
  w1, w2: Integer;
Begin
  v1 := Value1;
  v2 := Value2;
  If Not (v1^.Typ = vtbool) Then Begin
    Raise Exception.Create('invalid type for and : ' + VarTypeToStr(v1^.Typ));
  End;
  If (Not ShortCircuit) And (Not (v2^.Typ = vtbool)) Then Begin
    Raise Exception.Create('invalid type for and : ' + VarTypeToStr(v2^.Typ));
  End;
  new(p);
  p^.Funktionsaufrufzeiger := false;
  setlength(p^.uebergabeparameter, 0);
  p^.Name := 'const';
  p^.Typ := vtBool;
  p^.Line := -1;
  // Wenns die Möglichkeit auf eine Abkürzung gibt, dann müssen wir das nun testen
  If ShortCircuit Then Begin
    // Short Circuit nur für Boolean
    If (v1^.Typ = vtbool) Then Begin
      w1 := ResolveValue(v1);
      p^.Value := w1;
      result := p;
      If (w1 = 0) Then Begin
        // Wir können den ShortCircuit anwenden wenn wir Boolean haben und
        // auf  false and X getestet haben.
        ShortCircuit := false;
      End;
    End
    Else Begin
      // Wenn keine ShortCicuit abkürzung gemacht werden konnte, dann müssen
      // wir den Speicher Freigeben und sonst nichts weiter tun,
      // Da Genmathcalc automatisch noch mal Sand_ aufruft.
      dispose(p);
      result := v1;
    End;
  End
  Else Begin
    If (v1^.Typ = v2^.Typ) Then Begin
      w1 := ResolveValue(v1);
      w2 := ResolveValue(v2);
      p^.Value := w1 And w2;
    End
    Else
      Raise exception.create('Error invalid Type for "and"');
    result := p;
  End;
End;

Function Tinterpreter.SOr_(Value1, Value2: Pointer; Var ShortCircuit: Boolean
  ): Pointer;
Var
  v1, v2, p: PVariable;
  w1, w2: Integer;
Begin
  v1 := Value1;
  v2 := Value2;
  If Not (v1^.Typ = vtbool) Then Begin
    Raise Exception.Create('invalid type for or : ' + VarTypeToStr(v1^.Typ));
  End;
  If (Not ShortCircuit) And (Not (v2^.Typ = vtbool)) Then Begin
    Raise Exception.Create('invalid type for or : ' + VarTypeToStr(v2^.Typ));
  End;
  new(p);
  p^.Funktionsaufrufzeiger := false;
  setlength(p^.uebergabeparameter, 0);
  p^.Name := 'const';
  p^.Typ := vtbool;
  p^.Line := -1;
  // Wenns die Möglichkeit auf eine Abkürzung gibt, dann müssen wir das nun testen
  If ShortCircuit Then Begin
    // Short Circuit nur für Boolean
    If (v1^.Typ = vtbool) Then Begin
      w1 := ResolveValue(v1);
      p^.Value := w1;
      result := p;
      If (w1 <> 0) Then Begin
        // Wir können den ShortCircuit anwenden wenn wir Boolena haben und
        // auf  false and X getestet haben.
        ShortCircuit := false;
      End;
    End
    Else Begin
      // Wenn keine ShortCicuit abkürzung gemacht werden konnte, dann müssen
      // wir den Speicher Freigeben und sonst nichts weiter tun,
      // Da Genmathcalc automatisch noch mal Sand_ aufruft.
      dispose(p);
      result := v1;
    End;
  End
  Else Begin
    If (v1^.Typ = v2^.Typ) Then Begin
      w1 := ResolveValue(v1);
      w2 := ResolveValue(v2);
      p^.Value := w1 Or w2;
    End
    Else
      Raise exception.create('Error invalid Type for "and"');
    result := p;
  End;
End;

Function Tinterpreter.XOR_(Value1, Value2: Pointer): Pointer;
Var
  v1, v2, p: PVariable;
  w1, w2: INteger;
Begin
  v1 := Value1;
  v2 := Value2;
  If Not (v1^.Typ = vtbool) Then Begin
    Raise Exception.Create('invalid type for xor : ' + VarTypeToStr(v1^.Typ));
  End;
  If Not (v2^.Typ = vtbool) Then Begin
    Raise Exception.Create('invalid type for xor : ' + VarTypeToStr(v2^.Typ));
  End;
  new(p);
  p^.Funktionsaufrufzeiger := false;
  setlength(p^.uebergabeparameter, 0);
  p^.Name := 'const';
  p^.Typ := vtBool;
  p^.Line := -1;
  If (v1^.Typ = v2^.Typ) Then Begin
    w1 := ResolveValue(v1);
    w2 := ResolveValue(v2);
    p^.Value := w1 Xor w2;
  End
  Else
    Raise exception.create('Error invalid Type for "Xor"');
  result := p;
End;

Function Tinterpreter.PLus_(Value1, Value2: Pointer): Pointer;
Var
  pv1, pv2, p: PVariable;
  v1, v2: Single;
  ps: PSingle;
  ps1, ps2, pps: ^String;
Begin
  pv1 := value1;
  pv2 := value2;
  // + geht nur mit Scalaren
  If (pv1^.Typ In [vtbool, vtfield]) Or
    (pv2^.Typ In [vtbool, vtfield]) Then Begin
    Raise exception.create('Error invalid Type for "+"');
  End;
  If ((pv1^.Typ = vtString) And (pv2^.Typ <> vtString)) Or
    ((pv1^.Typ <> vtString) And (pv2^.Typ = vtString)) Then Begin
    Raise exception.create('Error invalid Type for "+"');
  End;
  new(p);
  p^.Funktionsaufrufzeiger := false;
  setlength(p^.uebergabeparameter, 0);
  p^.Name := 'const';
  p^.Line := -1;
  If (pv1^.Typ = vtString) Then Begin
    ps1 := PtrIntToPtr(ResolveValue(pv1));
    ps2 := PtrIntToPtr(ResolveValue(pv2));
    p^.Typ := vtString;
    new(pps);
    pps^ := ps1^ + ps2^;
    p^.Value := PtrToPtrInt(pps);
  End
  Else Begin
    v1 := PVariableToSingle(value1);
    v2 := PVariableToSingle(value2);
    If (pv1^.Typ = vtint) And (pv2^.Typ = vtint) Then Begin
      p^.Typ := vtint;
      p^.Value := round(v1 + v2);
    End
    Else Begin
      p^.Typ := vtfloat;
      new(ps);
      ps^ := v1 + v2;
      p^.Value := PtrToPtrInt(ps);
    End;
  End;
  result := p;
End;

Function Tinterpreter.Minus_(Value1, Value2: Pointer): Pointer;
Var
  pv1, pv2, p: PVariable;
  v1, v2: Single;
  ps: PSingle;
Begin
  pv1 := value1;
  pv2 := value2;
  // - geht nur mit Scalaren
  If (pv1^.Typ In [vtbool, vtfield, vtString]) Or
    (pv2^.Typ In [vtbool, vtfield, vtString]) Then Begin
    Raise exception.create('Error invalid Type for "-"');
  End;
  new(p);
  p^.Funktionsaufrufzeiger := false;
  setlength(p^.uebergabeparameter, 0);
  p^.Name := 'const';
  p^.Line := -1;
  v1 := PVariableToSingle(value1);
  v2 := PVariableToSingle(value2);
  If (pv1^.Typ = vtint) And (pv2^.Typ = vtint) Then Begin
    p^.Typ := vtint;
    p^.Value := round(v1 - v2);
  End
  Else Begin
    p^.Typ := vtfloat;
    new(ps);
    ps^ := v1 - v2;
    p^.Value := PtrToPtrInt(ps);
  End;
  result := p;
End;

Function Tinterpreter.uPlus_(Value1: Pointer): Pointer;
Var
  pv1, p: PVariable;
  v1: Single;
  ps: PSingle;
Begin
  pv1 := value1;
  // +geht nur mit Scalaren
  If (pv1^.Typ In [vtbool, vtfield, vtString]) Then Begin
    Raise exception.create('Error invalid Type for "+"');
  End;
  new(p);
  p^.Funktionsaufrufzeiger := false;
  setlength(p^.uebergabeparameter, 0);
  p^.Name := 'const';
  p^.Line := -1;
  v1 := PVariableToSingle(value1);
  If (pv1^.Typ = vtint) Then Begin
    p^.Typ := vtint;
    p^.Value := round(v1);
  End
  Else Begin
    p^.Typ := vtfloat;
    new(ps);
    ps^ := v1;
    p^.Value := PtrToPtrInt(ps);
  End;
  result := p;
End;

Function Tinterpreter.uMinus_(Value1: Pointer): Pointer;
Var
  pv1, p: PVariable;
  v1: Single;
  ps: PSingle;
Begin
  pv1 := value1;
  // - geht nur mit Scalaren
  If (pv1^.Typ In [vtbool, vtfield, vtString]) Then Begin
    Raise exception.create('Error invalid Type for "-"');
  End;
  new(p);
  p^.Funktionsaufrufzeiger := false;
  setlength(p^.uebergabeparameter, 0);
  p^.Name := 'const';
  p^.Line := -1;
  v1 := PVariableToSingle(value1);
  If (pv1^.Typ = vtint) Then Begin
    p^.Typ := vtint;
    p^.Value := -round(v1);
  End
  Else Begin
    p^.Typ := vtfloat;
    new(ps);
    ps^ := -v1;
    p^.Value := PtrToPtrInt(ps);
  End;
  result := p;
End;

Function Tinterpreter.not_(Value1: Pointer): Pointer;
Var
  p, v1: PVariable;
  w: Integer;
Begin
  v1 := Value1;
  If Not (v1^.Typ = vtbool) Then Begin
    Raise Exception.Create('invalid type for not : ' + VarTypeToStr(v1^.Typ));
  End;
  new(p);
  p^.Funktionsaufrufzeiger := false;
  setlength(p^.uebergabeparameter, 0);
  p^.Name := 'const';
  p^.Typ := vtint;
  p^.Line := -1;
  w := ResolveValue(v1);
  If v1^.Typ = vtbool Then Begin
    If (w = 0) Then
      w := 1
    Else
      w := 0;
  End
  Else Begin
    p^.Value := Not w; //-- Einfach nur Not funktioniert nicht, da Bitweise gedreht wird !!
  End;
  p^.Value := w;
  result := p;
End;

Function Tinterpreter.Mul_(Value1, Value2: Pointer): Pointer;
Var
  pv1, pv2, p: PVariable;
  v1, v2: Single;
  ps: PSingle;
Begin
  pv1 := value1;
  pv2 := value2;
  // * geht nur mit Scalaren
  If (pv1^.Typ In [vtbool, vtfield, vtString]) Or
    (pv2^.Typ In [vtbool, vtfield, vtString]) Then Begin
    Raise exception.create('Error invalid Type for "*"');
  End;
  new(p);
  p^.Funktionsaufrufzeiger := false;
  setlength(p^.uebergabeparameter, 0);
  p^.Name := 'const';
  p^.Line := -1;
  v1 := PVariableToSingle(value1);
  v2 := PVariableToSingle(value2);
  If (pv1^.Typ = vtint) And (pv2^.Typ = vtint) Then Begin
    p^.Typ := vtint;
    p^.Value := round(v1 * v2);
  End
  Else Begin
    p^.Typ := vtfloat;
    new(ps);
    ps^ := v1 * v2;
    p^.Value := PtrToPtrInt(ps);
  End;
  result := p;
End;

Function Tinterpreter.div_(Value1, Value2: Pointer): Pointer;
Var
  pv1, pv2, p: PVariable;
  v1, v2: Single;
Begin
  pv1 := value1;
  pv2 := value2;
  // div geht nur mit Scalaren
  If (pv1^.Typ <> vtint) Or
    (pv2^.Typ <> vtint) Then Begin
    Raise exception.create('Error invalid Type for "div"');
  End;
  new(p);
  p^.Funktionsaufrufzeiger := false;
  setlength(p^.uebergabeparameter, 0);
  p^.Name := 'const';
  p^.Line := -1;
  v1 := PVariableToSingle(value1);
  v2 := PVariableToSingle(value2);
  If round(v2) = 0 Then Begin
    Raise Exception.Create('Error div by 0.');
  End;
  p^.Typ := vtint;
  p^.Value := round(v1) Div round(v2);
  result := p;
End;

Function Tinterpreter.floatdiv_(Value1, Value2: Pointer): Pointer;
Var
  pv1, pv2, p: PVariable;
  v1, v2: Single;
  ps: PSingle;
Begin
  pv1 := value1;
  pv2 := value2;
  // / geht nur mit Scalaren
  If (pv1^.Typ In [vtbool, vtfield, vtString]) Or
    (pv2^.Typ In [vtbool, vtfield, vtString]) Then Begin
    Raise exception.create('Error invalid Type for "/"');
  End;
  new(p);
  p^.Funktionsaufrufzeiger := false;
  setlength(p^.uebergabeparameter, 0);
  p^.Name := 'const';
  p^.Line := -1;
  v1 := PVariableToSingle(value1);
  v2 := PVariableToSingle(value2);
  If v2 = 0 Then Begin
    Raise Exception.Create('Division by 0');
  End;
  p^.Typ := vtfloat;
  new(ps);
  ps^ := v1 / v2;
  p^.Value := PtrToPtrInt(ps);
  result := p;
End;

Function Tinterpreter.Mod_(Value1, Value2: Pointer): Pointer;
Var
  pv1, pv2, p: PVariable;
  v1, v2: Single;
Begin
  pv1 := value1;
  pv2 := value2;
  // mod geht nur mit Scalaren
  If (pv1^.Typ <> vtint) Or
    (pv2^.Typ <> vtint) Then Begin
    Raise exception.create('Error invalid Type for "mod"');
  End;
  new(p);
  p^.Funktionsaufrufzeiger := false;
  setlength(p^.uebergabeparameter, 0);
  p^.Name := 'const';
  p^.Line := -1;
  v1 := PVariableToSingle(value1);
  v2 := PVariableToSingle(value2);
  If round(v2) = 0 Then Begin
    Raise Exception.Create('Error mod by 0.');
  End;
  p^.Typ := vtint;
  p^.Value := round(v1) Mod round(v2);
  result := p;
End;

Function Tinterpreter.Round_(Value1: Pointer): Pointer;
Var
  pv1, p: PVariable;
  v1: Single;
Begin
  pv1 := value1;
  If (pv1^.Typ <> vtint) And
    (pv1^.Typ <> vtfloat) Then Begin
    Raise exception.create('Error invalid Type for "Round"');
  End;
  new(p);
  p^.Funktionsaufrufzeiger := false;
  setlength(p^.uebergabeparameter, 0);
  p^.Name := 'const';
  p^.Line := -1;
  v1 := PVariableToSingle(value1);
  p^.Typ := vtint;
  p^.Value := round(v1);
  result := p;
End;

Function Tinterpreter.Trunc_(Value1: Pointer): Pointer;
Var
  pv1, p: PVariable;
  v1: Single;
Begin
  pv1 := value1;
  If (pv1^.Typ <> vtint) And
    (pv1^.Typ <> vtfloat) Then Begin
    Raise exception.create('Error invalid Type for "Trunc"');
  End;
  new(p);
  p^.Funktionsaufrufzeiger := false;
  setlength(p^.uebergabeparameter, 0);
  p^.Name := 'const';
  p^.Line := -1;
  v1 := PVariableToSingle(value1);
  p^.Typ := vtint;
  p^.Value := trunc(v1);
  result := p;
End;

Procedure Tinterpreter.DisposeVariable(Value: Pointer);
Var
  p: PVariable;
  j: integer;
  ps: PSingle;
  pstr: ^String;
  r: TRecord;
Begin
  If assigned(Value) Then Begin
    p := Value;
    For j := 0 To High(p^.uebergabeparameter) Do
      Solver.FreeCalcTree(p^.uebergabeparameter[j]);
    If Not p^.Funktionsaufrufzeiger Then Begin
      Case p^.Typ Of
        vtfieldelement, vtbool, vtint: Begin
            // Nichts
          End;
        vtfloat: Begin
            ps := PtrIntToPtr(p^.Value);
            dispose(ps);
          End;
        vtString: Begin
            pstr := PtrIntToPtr(p^.Value);
            pstr^ := '';
            Dispose(pstr);
          End;
        vtfield: Begin
            r := TRecord(PtrIntToPtr(p^.Value));
            r.free;
          End;
      Else Begin
          Raise Exception.Create('Error Tinterpreter.DisposeVariable, missing typ');
        End;
      End;
    End;
    dispose(p);
  End;
End;

Function Tinterpreter.isCallableProcedure(value: String): Boolean;
Var
  i: Integer;
Begin
  value := LowerCase(value);
  result := false;
  // Eine Extern Registrierte Procedur
  For i := 0 To high(fExternalProcedures) Do Begin
    If fExternalProcedures[i].Name = value Then Begin
      result := true;
      exit;
    End;
  End;
  // Eine Procedur die Im Code irgendwo deklariert ist.
  For i := 0 To high(FFunctionlist) Do
    If FFunctionlist[i].Name = value Then Begin
      result := true;
      exit;
    End;
End;

Function Tinterpreter.isKeyword(Value: String): Boolean;
Begin
  value := LowerCase(Value);
  // TODO: Das ist noch lange keine Vollständige Liste :(
  result :=
    (value = 'begin') Or
    (value = 'const') Or
    (value = 'end') Or
    (value = 'function') Or
    (value = 'for') Or
    (value = 'integer') Or
    (value = 'procedure') Or
    (value = 'single') Or
    (value = 'string') Or
    (value = 'then') Or
    (value = 'while') Or
    (value = 'var');
End;

Function Tinterpreter.CreateVariable(Value: String): Pointer;
Var
  p: PVariable;
  pp: Psingle;
  pstr: ^String;
  oValue: String;
Begin
  new(p);
  p^.Funktionsaufrufzeiger := False;
  setlength(p^.uebergabeparameter, 0);
  p^.Name := 'const';
  p^.Line := -1;
  oValue := Value;
  Value := Lowercase(Value);
  If (Value = 'true') Or (Value = 'false') Then Begin
    p^.Typ := vtbool;
    If Value = 'true' Then
      p^.Value := 1
    Else
      p^.Value := 0;
  End
  Else Begin
    If (length(Value) >= 2) And (value[1] = '''') And (value[length(value)] = '''') Then Begin // Ein String
      p^.Typ := vtString;
      new(pstr);
      pstr^ := copy(oValue, 2, length(oValue) - 2);
      p^.Value := PtrToPtrInt(pstr);
    End
    Else Begin
      If pos('.', value) <> 0 Then Begin // Floating Point Zahlen
        p^.Typ := vtfloat;
        Try
          If Not (value[1] In ['0'..'9']) Then // <-- Verhindern das Hex zahlen Akzeptiert werden.
            Raise exception.create(''); // Erzeugt einfach nur eine AV, die unten abgefangen wird, deswegen kann diese hier Leer sein.
          new(pp);
          pp^ := strtofloat(Value, forma);
          p^.Value := PtrToPtrInt(pp);
        Except
          SolverErrorString := SolverErrorString + LineEnding + 'Error "' + Value + '" is not bool or int or single ( maybe "' + value + '" is not a declared variable).';
          Raise;
        End;
      End
      Else Begin // Integer Zahlen
        p^.Typ := vtint;
        Try
          If Not (value[1] In ['0'..'9']) Then // <-- Verhindern das Hex zahlen Akzeptiert werden.
            Raise exception.create(''); // Erzeugt einfach nur eine AV, die unten abgefangen wird, deswegen kann diese hier Leer sein.
          p^.Value := strtoint(Value);
        Except
          SolverErrorString := SolverErrorString + LineEnding + 'Error "' + Value + '" is not bool or int or single ( maybe "' + value + ' is not a declared variable).';
          Raise;
        End;
      End;
    End;
  End;
  result := p;
End;

Procedure Tinterpreter.FreeSolver;
Begin
  solver.free;
  solver := Nil;
End;

Procedure Tinterpreter.InitSolver;
Begin
  If assigned(Solver) Then Freesolver;
  Solver := TGenMathCalc.create;
  solver.ClearOPs;
  (*
   * Wenn es hier scheppert, dann fehlt in genmathcalc die Option auf OOP_Callbacks
   *)
  solver.OnCreateValue := @CreateVariable;
  solver.OnFreeValue := @DisposeVariable;
  // Integer Operatoren
  solver.AddUnOP('-', @uMinus_);
  solver.AddUnOP('+', @uPlus_);
  solver.addUnOP('trunc', @Trunc_);
  solver.addUnOP('round', @Round_);
  solver.addBinOP('/', @floatdiv_);
  solver.AddBinOP('div', @div_);
  solver.AddBinOP('mod', @Mod_);
  solver.AddBinOP('*', @Mul_);
  solver.AddBinOP('+', @Plus_);
  solver.AddBinOP('-', @Minus_);
  // Boolean Operatoren
  solver.AddUnOP('not', @not_);
  solver.AddShortBinOP('and', @Sand_);
  solver.AddShortBinOP('or', @Sor_);
  solver.AddBinOP('xor', @xor_);
  // Int-> Boolean Operatoren
  solver.AddBinOP('=', @Equal);
  solver.AddBinOP('<>', @NotEqual);
  solver.AddBinOP('>', @Greater);
  solver.AddBinOP('<', @Lesser);
  solver.AddBinOP('>=', @GreaterOrEqual);
  solver.AddBinOP('<=', @LesserOrEqual);
End;

Procedure Tinterpreter.ClearAll;
Var
  i: Integer;
Begin
  For i := 0 To high(FRecords) Do Begin
    FRecords[i].Free;
  End;
  setlength(FRecords, 0);
  For i := 0 To High(FVariables) Do Begin
    DisposeVariable(FVariables[i]);
  End;
  setlength(FVariables, 0);
  For i := 0 To high(FFunctionlist) Do Begin
    FFunctionlist[i].free;
  End;
  setlength(FFunctionlist, 0);
End;

Function Tinterpreter.Compile(SoureCode: String): boolean;
Var
  s: String;
Begin
  result := false;
  // Lexer
  If pos('~', SoureCode) <> 0 Then Begin
    Errormessage := 'Error, due to internal reasons, the "~" symbol is not allowed in sourcecode.';
    exit;
  End;
  s := unc.Uncomment(SoureCode + LineEnding + 'EndOfFile' + LineEnding);
  //showmessage(s); // Debugg
  Ftokens := tok.scan(s);
  (*
  Kommt hier ein Compilerfehler, so wurde vergessen in GenMathCalc.pas das Compilerswitch use Tokenizer zu setzen !!
  *)
  Ftokens := NormalTokensToLineInfoTokens(Ftokens, '~');
  // Das Eigentliche Compile
  ClearAll;
  ferror := false;
  Errormessage := '';
  Fatokenindex := -1;
  result := True;
  SolverErrorString := '';
  // Wenn es überhaupt was gibt.
  If high(Ftokens) <> -1 Then Begin
    // den 1. Token einlesen.
    EatToken;
    // Aufruf des Generellen Einlesens von Code...
    Compileh(-1);
    // Erkennen des EOF
    If (faLowertoken <> 'endoffile') And (Not ferror) Then Abbruch('Error unknown identifer "' + Fatoken + '" at line : ' + inttostr(Fatokenline));
  End;
  If length(SolverErrorString) <> 0 Then Begin // Fehler die durch Genmathcalc rein gereicht werden
    Errormessage := Errormessage + SolverErrorString;
    Ferror := True;
  End;
  If Ferror Then Begin
    clearall;
    result := False;
  End;
End;

Function Tinterpreter.Compileh(count: integer): TCompiledCode;
Var
  t: TCompiledToken;
  i, localbearbeitet: Integer;
  b, c, d: Boolean;
  s: String;
Begin
  b := True;
  setlength(Result, 0);
  While b Do Begin
    b := False;
    If Fatokenindex < high(Ftokens) Then Begin
      // Auslesen einer Zuweisung
      If Ftokens[Fatokenindex + 1].value = ':=' Then Begin
        b := True;
        setlength(result, high(result) + 2);
        result[high(result)] := EatZuweisung;
      End;
      // Auslesen eines Procedur Calls
      If (Ftokens[Fatokenindex + 1].Value = '(') Or
        (Ftokens[Fatokenindex + 1].Value = ';') Then Begin
        If isCallableProcedure(faLowertoken) Then Begin
          b := true;
          setlength(result, high(result) + 2);
          result[high(result)] := EatCall;
        End;
      End;
    End;
    If faLowertoken = 'while' Then Begin
      b := True;
      t := EatWhile;
      setlength(result, high(result) + 2);
      result[high(result)] := t;
    End;
    If faLowertoken = 'type' Then Begin
      b := true;
      EatType;
    End;
    // For schleife
    If faLowertoken = 'for' Then Begin
      b := True;
      t := EatFor;
      setlength(result, high(result) + 2);
      result[high(result)] := t;
    End;

    // Parsen einer Funktion und allem was dazu gehört
    If (faLowertoken = 'function') Or (faLowertoken = 'procedure') Then Begin
      b := true;
      If (faLowertoken = 'function') Then Begin
        t := Eatfunction;
      End
      Else Begin
        t := EatProcedure;
      End;
      setlength(FFunctionlist, high(FFunctionlist) + 2);
      FFunctionlist[high(FFunctionlist)] := TProcedure(t);
      TProcedure(t).LocalVarcount := 0;
      TProcedure(t).nested := count <> -1;
      // Einlesen diverser Nestungen...
      localbearbeitet := 0;
      d := true;
      While (faLowertoken <> 'begin') And (Not ferror) Do Begin
        If Not d Then
          abbruch('Error unknown value : "' + fatoken + '" at line : ' + inttostr(FatokenLine));
        d := False;
        If (faLowertoken = 'function') Or (faLowertoken = 'procedure') Then Begin // Genestete Functionen
          d := true;
          Compileh(1);
        End;
        // einlesen der Lokalen variablen
        If faLowertoken = 'var' Then Begin
          d := true;
          c := True;
          While c Do Begin
            If (faLowertoken = 'var') Or Not (isKeyword(faLowertoken)) Then Begin
              TProcedure(t).LocalVarcount := TProcedure(t).LocalVarcount + EatVariablewithType;
              eat(';');
            End
            Else
              c := false;
            If ferror Then c := false;
          End;
          If (Not ferror) And (localbearbeitet - TProcedure(t).LocalVarcount <> 0) Then Begin
            // Einlesen der Lokalen Variablennamen, brauchen wir für Rekursion und
            // Zur Auflösung von Variablenzugehörigkeiten bei Function als Operator ..
            setlength(TProcedure(t).Inners, TProcedure(t).LocalVarcount);
            For i := 0 To TProcedure(t).LocalVarcount - 1 - localbearbeitet Do Begin
              s := FVariables[high(FVariables) - TProcedure(t).LocalVarcount + 1 + i + localbearbeitet]^.Name;
              TProcedure(t).Inners[i + localbearbeitet].Name := copy(s, length(TProcedure(t).name) + 2, length(s));
              TProcedure(t).Inners[i + localbearbeitet].Typ := FVariables[high(FVariables) - TProcedure(t).LocalVarcount + 1 + i + localbearbeitet]^.Typ;
            End;
            localbearbeitet := TProcedure(t).LocalVarcount;
          End;
        End;
      End;
      // Einlesen des Codes Innerhalb
      If Not ferror Then Begin
        TProcedure(t).InnerCode := Eatinner;
      End
      Else Begin
        TProcedure(t).InnerCode := Nil;
      End;
      eat(';');
    End;
    // Parsen von if
    If faLowertoken = 'if' Then Begin
      b := True;
      t := EatIFConstruct;
      setlength(result, high(result) + 2);
      result[high(result)] := t;
    End;
    If (count = 1) Or (ferror) Then b := False;
  End;
End;

Function Tinterpreter.EatProcedure: TProcedure;
Begin
  result := EatFunProc(TProcedure);
  If ferror Then exit;
  eat(';');
End;

Function Tinterpreter.Eatfunction: TFunction;
Var
  resName: TName;
Begin
  result := EatFunProc(TFunction) As TFunction;
  If ferror Then exit;
  // Auslesen des ResultTypes
  eat(':');
  resName.line := FatokenLine;
  resName.value := 'result';
  result.Result_.Typ := AppendVariable(resName, faLowertoken);
  result.Result_.Name := 'result';
  result.Result_.RecordName := faLowertoken;
  EatToken;
  //PlottAllVariables('Eatfunction');
  eat(';');
End;

Procedure Tinterpreter.EatToken;
Begin
  If Not ferror Then Begin
    inc(Fatokenindex);
    If Fatokenindex <= High(Ftokens) Then Begin
      fatoken := ftokens[Fatokenindex].Value;
      faLowertoken := lowercase(fatoken);
      FatokenLine := ftokens[Fatokenindex].Line + 1; // Alle Parser sind ja 0-Bassiert, Ausgaben sollen aber 1 Bassiert sein
    End
    Else Begin
      dec(Fatokenindex);
      fatoken := '';
      faLowertoken := '';
    End;
  End;
End;

Function Tinterpreter.EatInner: TCompiledCode;
Begin
  // Wenn wir Einen Begin - Ende Block Scannen
  If faLowertoken = 'begin' Then Begin
    eattoken;
    result := Compileh(-1);
    eat('end');
  End
  Else Begin
    // Nur einen Befehl einlesen
    result := Compileh(1);
  End;
End;

Function Tinterpreter.Eat(Value: String): Boolean;
Begin
  If Not Ferror Then Begin
    If faLowertoken = value Then Begin
      result := true;
      eattoken;
    End
    Else Begin
      result := false;
      Abbruch('Error expected "' + Value + '", but found "' + fatoken + '" at line : ' + inttostr(Fatokenline));
    End;
  End
  Else
    result := false;
End;

Procedure Tinterpreter.Abbruch(Value: String);
Begin
  errormessage := errormessage + LineEnding + Value;
  Ferror := True;
End;

(*
Liest Alle Variablen ein bis eine Gültige Typ Definition gefunden wurde.

Also terme der Form

x,y,z : integer

cav x : Integer

const y : Boolean

der Lokale Namespace wird ebenfalls mit angehängt
*)

Function Tinterpreter.EatVariablewithType: integer;
Var
  names: Array Of Tname;
  b: boolean;
  i: integer;
  rescount: integer;
Begin
  setlength(names, 0);
  rescount := 0;
  b := true;
  While b And (Not ferror) Do Begin
    // Var und Const wird einfach mal ignoriert *g*
    If (faLowertoken = 'var') Or (faLowertoken = 'const') Then Begin
      EatToken;
    End;
    setlength(names, high(names) + 2);
    names[high(names)].value := faLowertoken;
    names[high(names)].line := FatokenLine;
    If Not CheckVarname(fatoken) Then Abbruch('Error invalid identifier "' + fatoken + '" at line : ' + inttostr(fatokenline));
    eattoken;
    If Fatoken = ':' Then
      b := False
    Else Begin
      b := true;
      eat(',');
    End;
  End;
  // Lesen des Types
  If eat(':') Then Begin
    For i := 0 To high(names) Do Begin
      Case AppendVariable(names[i], faLowertoken) Of
        vtfield: rescount := rescount + FVariables[high(FVariables)]^.RecordFieldIndex + 1 + 1; // Die Anzahl der Felder müssen mit gezählt werden
      Else Begin
          rescount := rescount + 1;
        End;
      End;
    End;
    eattoken;
  End;
  result := rescount;
End;

Function Tinterpreter.EatFunProc(tp: TProcedureclass): TProcedure;
Var
  b: Boolean;
  i: integer;
Begin
  result := tp.create(self);
  Eattoken;
  result.name := faLowertoken;
  result.ParamVarcount := 0;
  result.deklarationline := FatokenLine;
  If Not CheckFunctionName(fatoken) Then abbruch('Error invalid identifier "' + fatoken + '" at line : ' + inttostr(fatokenline));
  If isCallableProcedure(faLowertoken) Then abbruch('Name already exists  "' + fatoken + '" at line : ' + inttostr(fatokenline));
  If ferror Then exit;
  Eattoken;
  // Wenn es Übergabeparameter gibt, dann parsen wir diese nun heraus
  If Fatoken = '(' Then Begin
    eattoken;
    // ist die Parameterliste Leer, dann springen wir gleich wieder raus
    If Fatoken = ')' Then Begin
      // Nichts, das Token wird unten gefressen
    End
    Else Begin
      // Auslesen der Parameter und deren Typ
      b := True;
      While b And (Not Ferror) Do Begin
        result.ParamVarcount := result.ParamVarcount + EatVariablewithType;
        If Fatoken <> ')' Then Begin
          If fatoken = ',' Then Begin
            Eat(',');
          End
          Else Begin
            Eat(';'); // Der Type wechselt
          End;
        End;
        If Ferror Or (fatoken = ')') Then b := False;
      End;
    End;
    eat(')');
  End;
  If ferror Then exit;
  setlength(Result.Params, result.ParamVarcount);
  For i := 0 To result.ParamVarcount - 1 Do Begin
    result.Params[i].Name := FVariables[high(FVariables) - result.ParamVarcount + 1 + i]^.Name;
    result.Params[i].Typ := FVariables[high(FVariables) - result.ParamVarcount + 1 + i]^.Typ;
    result.Params[i].RecordName := FVariables[high(FVariables) - result.ParamVarcount + 1 + i]^.RecordName;
  End;
End;

(*
gibt true Zurück wenn Value einem gültigen Variablennamen entspricht !!
*)

Function Tinterpreter.CheckVarname(Value: String): boolean;
//Var
//  i: Integer;
Begin
  // Todo: Implementierten
  result := true;
  value := LowerCase(value);
  If isKeyword(value) Then result := false;
  If isCallableProcedure(Value) Then result := false;
  // Die Variable Existiert schon -- Das Geht leider nicht, da FVariables keine Berücksichtigung der Sichrbarkeit hat
  // Eine Lokale Variable die gleich heist wie eine andere Lokale Variable würde hier also fälschlicherweise ein result := false triggern !
  //For i := 0 To high(FVariables) Do Begin
  //  If lowercase(FVariables[i]^.Name) = value Then Begin
  //    result := false;
  //    break;
  //  End;
  //End;
End;

Function Tinterpreter.CheckFunctionName(value: String): Boolean;
Begin
  result := CheckVarname(value);
End;

(*
Liest alles was Boolean oder Integer ist und gibt den Pointer auf den Rechenbaum zurück
*)

Function Tinterpreter.EatAusdruck(Abbruchtoken: Array Of String): PCalcTree;
Var
  varlist: TVarlist;
  data: TTokenarray;
  rpc, vi, i, j, k, klammerC: integer;
  r, b: Boolean;
  F: TProcedure;
  fun: TFunction;
  e: PExternalCall;
Begin
  result := Nil;
  If Not ferror Then Begin
    // Erstellen der Variablenliste für den Ausdruck
    setlength(Varlist, high(FVariables) + 1);
    (*
     * Da Genmathcalc von Vorne sucht, der Stack aber Rückweärts läuft,
     * müssen die Parameter Rückwärts übergeben werden, sonst klappt die Überdeckung der Variablen nicht Richtig.
     *)
    For i := 0 To High(Varlist) Do Begin
      varlist[i].Name := FVariables[High(Varlist) - i]^.Name;
      varlist[i].Value := FVariables[High(Varlist) - i];
    End;
    //    PlottAllVariables('EatAusdruck');
    (* For i := 0 To High(Varlist) Do Begin
        varlist[i].Name := FVariables[i]^.Name;
        varlist[i].Value := FVariables[i];
       End;//*)
    i := Fatokenindex;
    j := 0;
    setlength(data, high(Ftokens) - i); // Irgend eine Großen wert der garantiert immer reicht
    b := True;
    klammerC := 0; // Das zählen der Klammern verhindert , das wenn Abbruchtoken = ')' ist nicht innnerhalb von Klammerungen abgebrochen wird !!
    While (b Or (klammerC <> 0)) And (Not ferror) Do Begin
      r := True;
      For k := 0 To High(Abbruchtoken) Do
        If (lowercase(Ftokens[i].value) = Abbruchtoken[k]) Then Begin
          r := false;
          break;
        End;
      If r Or (klammerC <> 0) Then Begin
        If ValueIsFunctionName(lowercase(Ftokens[i].value)) Then Begin
          // Suchen der Betroffenen Funktion
          f := Nil;
          For k := 0 To High(FFunctionlist) Do
            If lowercase(Ftokens[i].value) = FFunctionlist[k].Name Then Begin
              f := FFunctionlist[k];
              break;
            End;
          If (f = Nil) Then Begin
            Abbruch('Can not find : "' + Ftokens[i].value + '" at line : ' + inttostr(FatokenLine));
          End;
          // Hinzufügen des Functionsnamens in die Varlist
          If f Is TFunction Then Begin
            fun := f As TFunction;
            setlength(FVariables, high(FVariables) + 2);
            new(FVariables[high(FVariables)]);
            vi := high(FVariables);
            FVariables[vi]^.Funktionsaufrufzeiger := True;
            FVariables[vi]^.Name := fun.name;
            FVariables[vi]^.Typ := fun.Result_.Typ;
            FVariables[vi]^.Value := 0;
            FVariables[vi]^.Line := fun.deklarationline;
            FVariables[vi]^.RecordFieldIndex := 0;
            FVariables[vi]^.RecordName := fun.Result_.RecordName;
            setlength(Varlist, high(Varlist) + 2);
            varlist[high(Varlist)].Value := FVariables[vi];
          End
          Else Begin
            Abbruch('Call a procedure whithin a expression "' + Ftokens[i].value + '" at line : ' + inttostr(FatokenLine));
            vi := 0;
          End;
          (*
           Die Funktionspointer können Mehrfach auftreten, und um das zu Verhindern geben wir ihnen einfach
           einen Eindeutigen Identifier mit, ihren Pointer, es kann auch jeder andere Wert sein, hauptsache ist er
           unterscheidet sich bei allen Gleichnamigen aufrufen !!
          *)
          varlist[high(Varlist)].Name := f.name + 'æ' + inttostr(vi);
          // Berechnen des Tatsächlichen Parameter Counts (ignorieren aller FeldElemente)
          rpc := 0;
          For k := 0 To high(F.Params) Do Begin
            If F.Params[k].Typ <> vtfieldelement Then Begin
              inc(rpc);
            End;
          End;
          setlength(FVariables[vi]^.uebergabeparameter, rpc);
          // Eintragen in die Parslist für den Ausdruck
          data[j].Value := f.name + 'æ' + inttostr(vi);
          data[j].Line := ftokens[i].Line;
          inc(j);
          // Parsen der Übergabeparameter
          inc(i, 2); // Wir überspringen den Funktionsnamentoken und die Klammer
          For k := 0 To rpc - 1 Do Begin
            Fatokenindex := i;
            //If k < f.ParamVarcount - 1 Then Begin
            If k < rpc - 1 Then Begin
              FVariables[vi]^.uebergabeparameter[k] := EatAusdruck([',']);
            End
            Else Begin
              FVariables[vi]^.uebergabeparameter[k] := EatAusdruck([')']);
            End;
            i := Fatokenindex + 1; // ist Praktisch Eattoken
          End;
          dec(i); // Die Schleife oben macht 1 plus zu viel ,)
          // Wenn die Funktion keine Parameter hat, kann denoch ein "()" folgen.
          If f.ParamVarcount = 0 Then Begin
            If Ftokens[i].value = '(' Then Begin
              inc(i);
              If Ftokens[i].value = ')' Then Begin
                //inc(i); // Darf nicht sein, da i auf dem Letzten geparsten Zeichen stehen bleiben muss
              End
              Else Begin
                // Abbruch
                b := false;
                KlammerC := 0;
                Abbruch('Error could not resolve the expression at line : ' + inttostr(FatokenLine) + ' missing ")".');
              End;
            End
            Else Begin
              dec(i); // Ohne Klammern muss i auf dem Funktionsnamen stehen bleiben.
            End;
          End;
        End
        Else Begin
          If ValueIsCall(lowercase(Ftokens[i].value)) Then Begin
            // Suchen der Betroffenen Funktion
            e := Nil;
            For k := 0 To High(fExternalProcedures) Do
              If lowercase(Ftokens[i].value) = fExternalProcedures[k].Name Then Begin
                e := @fExternalProcedures[k];
                break;
              End;
            If (e = Nil) Then Begin
              Abbruch('Can not find: "' + Ftokens[i].value + '" at line : ' + inttostr(FatokenLine));
            End;
            // Hinzufügen des Functionsnamens in die Varlist
            If e^.restype <> vtnone Then Begin
              setlength(FVariables, high(FVariables) + 2);
              new(FVariables[high(FVariables)]);
              vi := high(FVariables);
              FVariables[vi]^.Funktionsaufrufzeiger := True;
              FVariables[vi]^.Name := e^.Name;
              FVariables[vi]^.Typ := e^.restype;
              FVariables[vi]^.Value := 0;
              FVariables[vi]^.Line := -1; // Das kommt ja von Außen
              FVariables[vi]^.RecordFieldIndex := 0;
              // FVariables[vi]^.RecordName := fun.Result_.RecordName;
              FVariables[vi]^.RecordName := ''; // TODO: Klären was hier hin muss, ist aber wahrscheinlich nur, wenn die Externe Funktion eine Struktur zurückgibt
              setlength(Varlist, high(Varlist) + 2);
              varlist[high(Varlist)].Value := FVariables[vi];
            End
            Else Begin
              Abbruch('Call a function without a return value "' + Ftokens[i].value + '" at line : ' + inttostr(FatokenLine));
              vi := 0;
            End;
            (*
              Die Funktionspointer können Mehrfach auftreten, und um das zu Verhindern geben wir ihnen einfach
              einen Eindeutigen Identifier mit, ihren Pointer, es kann auch jeder andere Wert sein, hauptsache ist er
              unterscheidet sich bei allen Gleichnamigen aufrufen !!
             *)
            varlist[high(Varlist)].Name := e^.name + 'æ' + inttostr(vi);
            // Berechnen des Tatsächlichen Parameter Counts (ignorieren aller FeldElemente)
            rpc := 0;
            For k := 0 To high(e^.Params) Do Begin
              If e^.Params[k] <> vtfieldelement Then Begin
                inc(rpc);
              End;
            End;
            setlength(FVariables[vi]^.uebergabeparameter, rpc);
            // Eintragen in die Parslist für den Ausdruck
            data[j].Value := e^.name + 'æ' + inttostr(vi);
            data[j].Line := ftokens[i].Line;
            inc(j);
            // Parsen der Übergabeparameter
            inc(i, 2); // Wir überspringen den Funktionsnamentoken und die Klammer
            For k := 0 To rpc - 1 Do Begin
              Fatokenindex := i;
              //If k < f.ParamVarcount - 1 Then Begin
              If k < rpc - 1 Then Begin
                FVariables[vi]^.uebergabeparameter[k] := EatAusdruck([',']);
              End
              Else Begin
                FVariables[vi]^.uebergabeparameter[k] := EatAusdruck([')']);
              End;
              i := Fatokenindex + 1; // ist Praktisch Eattoken
            End;
            dec(i); // Die Schleife oben macht 1 plus zu viel ,)
            // Wenn die Funktion keine Parameter hat, kann denoch ein "()" folgen.
            If length(e^.Params) = 0 Then Begin
              If Ftokens[i].value = '(' Then Begin
                inc(i);
                If Ftokens[i].value = ')' Then Begin
                  //inc(i); // Darf nicht sein, da i auf dem Letzten geparsten Zeichen stehen bleiben muss
                End
                Else Begin
                  // Abbruch
                  b := false;
                  KlammerC := 0;
                  Abbruch('Error could not resolve the expression at line : ' + inttostr(FatokenLine) + ' missing ")".');
                End;
              End
              Else Begin
                dec(i); // Ohne Klammern muss i auf dem Funktionsnamen stehen bleiben.
              End;
            End;
          End
          Else Begin
            // ein Einfacher Ausdruck
            If ftokens[i].value = '(' Then inc(KlammerC);
            If ftokens[i].value = ')' Then dec(KlammerC);
            data[j].Value := ftokens[i].value;
            data[j].Line := ftokens[i].Line;
            inc(j);
          End;
        End;
        inc(i);
        // Erkennen über das Ende der Eingabe Gelesen, oder Ungültige Klammerung
        If (i > high(Ftokens)) Or (klammerC < 0) Then Begin
          b := False;
          KlammerC := 0;
          Abbruch('Error could not resolve the expression at line : ' + inttostr(FatokenLine));
        End;
      End
      Else
        b := False;
    End;
    // Endlich haben wir es geschafft nun kann der Ausdruck bestimmt werden..
    If (j <> 0) And (Not ferror) Then Begin
      Fatokenindex := i - 1;
      EatToken;
      r := false;
      For k := 0 To high(Abbruchtoken) Do
        If faLowertoken = Abbruchtoken[k] Then Begin
          r := True;
          break;
        End;
      If r Then Begin
        setlength(data, j);
        Try
          result := solver.Parse(data, varlist);
        Except
          abbruch('Error could not resolve the expression at line : ' + inttostr(FatokenLine));
          result := Nil;
        End;
      End
      Else Begin
        abbruch('Error could not resolve the expression at line : ' + inttostr(FatokenLine));
        result := Nil;
      End;
    End
    Else Begin
      If (Not ferror) Then Begin
        abbruch('Error could not resolve the expression at line : ' + inttostr(FatokenLine));
      End;
      result := Nil;
    End;
  End
  Else
    result := Nil;
End;

Function Tinterpreter.EatIFConstruct: TIfConstruct;
Begin
  result := TIfConstruct.create(self);
  eattoken;
  result.Bedingung := EatAusdruck(['then']);
  // Konnte die Bedingung correckt bestimmt werden gehts weiter
  If Not ferror Then Begin
    If eat('then') Then Begin
      result.TrueCode := EatInner;
      If faLowertoken = 'else' Then Begin
        eattoken;
        result.FalseCode := EatInner;
      End;
      If faLowertoken = ';' Then
        eat(';');
    End;
  End;
End;

Function Tinterpreter.ValueIsFunctionName(Value: String): Boolean;
Var
  i: Integer;
Begin
  result := false;
  For i := 0 To high(FFunctionlist) Do
    If FFunctionlist[i].Name = Value Then Begin
      result := true;
      exit;
    End;
End;

Function Tinterpreter.ValueIsCall(Value: String): Boolean;
Var
  i: Integer;
Begin
  value := LowerCase(Value);
  result := false;
  For i := 0 To high(fExternalProcedures) Do Begin
    If fExternalProcedures[i].Name = value Then Begin
      result := true;
      exit;
    End;
  End;
End;

Function Tinterpreter.EatZuweisung: TAssignment;
Begin
  If Not ferror Then Begin
    result := TAssignment.create(self);
    If IsValidIdent(faLowertoken, true) Then Begin
      If IdentExists(faLowertoken, FatokenLine) Then Begin
        result.Ziel := faLowertoken;
        eattoken;
        If eat(':=') Then Begin
          result.Ausdruck := EatAusdruck([';', 'else']);
          If faLowertoken = ';' Then
            eat(';');
        End;
      End
      Else
        Abbruch('Error unknown identifier "' + fatoken + '" at line : ' + inttostr(FatokenLine));
    End
    Else
      Abbruch('Error invalid identifier "' + fatoken + '" at line : ' + inttostr(FatokenLine));
  End
  Else
    result := Nil;
End;

Function Tinterpreter.EatCall: TCall;
Var
  e: PExternalCall;
  paramcnt: Integer;
  i: Integer;
Begin
  //  Dec(Fatokenindex);
  //  result := EatFunProc(TCall) As TCall;
  result := TCall.create(self);
  result.Name := faLowertoken;
  result.ParamVarcount := 0;
  result.Deklarationline := FatokenLine;
  result.Params := Nil;
  paramcnt := -1;
  // Bestimmen der ein Zu lesenden Parameter
  // 1. bereits Implementierte Funktionen / Proceduren ?
  For i := 0 To high(FFunctionlist) Do
    If FFunctionlist[i].Name = faLowertoken Then Begin
      paramcnt := length(FFunctionlist[i].Params);
      break;
    End;
  // Extern Definierte Funktionen / Proceduren ?
  If paramcnt = -1 Then Begin
    e := Nil;
    For i := 0 To high(fExternalProcedures) Do Begin
      If Result.Name = fExternalProcedures[i].Name Then Begin
        e := @fExternalProcedures[i];
        break;
      End;
    End;
    If e = Nil Then Begin
      Abbruch('Error unknown identifer "' + Fatoken + '" at line : ' + inttostr(Fatokenline));
      result.free;
      exit;
    End;
    paramcnt := length(e^.Params);
  End;
  EatToken;
  If (faLowertoken = '(') Or (paramcnt > 0) Then Begin // Der Aufruf hat Parameter
    // TODO: Wäre das nicht Cool, wenn wir eine Typprüfung für die Übergabeparameter hätten ?
    eat('(');
    For i := 0 To paramcnt - 1 Do Begin
      If i <> paramcnt - 1 Then Begin
        setlength(result.fParamCalcTrees, high(result.fParamCalcTrees) + 2);
        result.fParamCalcTrees[high(result.fParamCalcTrees)] := EatAusdruck([',']);
        Eat(',');
      End
      Else Begin
        setlength(result.fParamCalcTrees, high(result.fParamCalcTrees) + 2);
        result.fParamCalcTrees[high(result.fParamCalcTrees)] := EatAusdruck([')']);
        Eat(')');
      End;
      If ferror Then Begin
        result.free;
        exit;
      End;
    End;
    If paramcnt = 0 Then Begin // Eine Function / Procedur ohne Parameter aber der User hat dahinter ein () geschrieben
      eat(')');
    End;
  End;
  eat(';');
End;

Procedure Tinterpreter.EatType;
Var
  b: Boolean;
Begin
  eat('type');
  b := true;
  While b And (Not ferror) Do Begin
    b := false;
    // LR(1) auf "="
    If Fatokenindex < high(Ftokens) Then
      If Ftokens[Fatokenindex + 1].value = '=' Then Begin
        // Sollte dieser Parser je mehr wie Records können, müsste hier eine Differenzierung rein
        EatRecord;
        b := true;
      End;
  End;
End;

Procedure Tinterpreter.EatRecord;
Var
  RecordName: String;
  fn: Array Of String;
  ft: String;
  i: integer;
Begin
  RecordName := faLowertoken;
  EatToken;
  eat('=');
  eat('record');
  // Das Die Variablendeklarationen parsen
  If Not ferror Then Begin
    setlength(FRecords, high(FRecords) + 2);
    FRecords[high(FRecords)] := TRecord.create();
    FRecords[high(FRecords)].Name := RecordName;
    While (faLowertoken <> 'end') And (Not ferror) Do Begin
      setlength(fn, 0);
      While (faLowertoken <> ':') And (Not ferror) Do Begin
        setlength(fn, high(fn) + 2);
        fn[high(fn)] := faLowertoken;
        EatToken();
        If faLowertoken = ',' Then eat(',');
      End;
      eat(':');
      ft := faLowertoken;
      EatToken();
      eat(';');
      If Not ferror Then Begin
        If high(fn) = -1 Then Abbruch('No variable names found.');
        For i := 0 To high(fn) Do Begin
          If Not FRecords[high(FRecords)].AddField(fn[i], StrToVarType(ft)) Then Begin
            Abbruch('Could not add field "' + fn[i] + '" of type "' + ft + '" to record "' + RecordName + '"');
          End;
        End;
      End;
    End;
  End;
  eat('end');
  eat(';');
End;

Function Tinterpreter.IdentExists(Value: String; Line: Integer): Boolean;
Var
  i: Integer;
Begin
  result := false;
  For i := high(FVariables) Downto 0 Do
    If (Not FVariables[i]^.Funktionsaufrufzeiger) And
      (lowercase(Value) = FVariables[i]^.name) Then
      If line >= FVariables[i]^.Line Then Begin
        result := True;
        exit;
      End;
End;

Function Tinterpreter.GiveFirstFunction: TProcedure;
Begin
  result := Nil;
  If High(FFunctionlist) <> -1 Then
    result := FFunctionlist[0];
End;

Function Tinterpreter.RegisterExternalCall(PName: String;
  params: Array Of TVariablenType; ResType: TVariablenType;
  Callback: TProcCallback): Boolean;
Var
  i: Integer;
Begin
  result := false;
  If iskeyWord(PName) Then exit;
  PName := LowerCase(PName);
  result := Not isCallableProcedure(PName);
  If result Then Begin
    setlength(fExternalProcedures, high(fExternalProcedures) + 2);
    fExternalProcedures[high(fExternalProcedures)].Name := PName;
    setlength(fExternalProcedures[high(fExternalProcedures)].Params, length(params));
    For i := 0 To high(params) Do Begin
      fExternalProcedures[high(fExternalProcedures)].Params[i] := Params[i];
    End;
    fExternalProcedures[high(fExternalProcedures)].Proc := Callback;
    fExternalProcedures[high(fExternalProcedures)].restype := ResType;
  End;
End;

Function Tinterpreter.CallFunction(Functionname: String; Parameter: TSringArray
  ): String;
Var
  fun: TFunction;
  f: TProcedure;
  vi, os, i, j: Integer;
  ps, ps2: PSingle;
  pStr, pStr2: ^String;
  r, r2: TRecord;
  n: TName;
Begin
  result := '';
  // Aufruf einer Externen Funktion / Procedur
  For i := 0 To high(fExternalProcedures) Do Begin
    If fExternalProcedures[i].Name = Functionname Then Begin
      result := fExternalProcedures[i].Proc(Parameter);
      exit;
    End;
  End;
  // Suchen der Funktion
  f := Nil;
  Functionname := lowercase(Functionname);
  For i := 0 To high(FFunctionlist) Do
    If FFunctionlist[i].Name = Functionname Then Begin
      f := FFunctionlist[i];
      break;
    End;
  If Not assigned(f) Then Begin
    Raise Exception.Create('No implementation for: ' + Functionname);
  End;
  // Alle Lokalen Variablen auf den Stack legen
  os := high(FVariables) + 1;
  setlength(FVariables, high(FVariables) + 1 + f.LocalVarcount);
  For i := 0 To f.LocalVarcount - 1 Do Begin
    new(FVariables[i + os]);
    FVariables[i + os]^.Funktionsaufrufzeiger := False;
    setlength(FVariables[i + os]^.uebergabeparameter, 0);
    FVariables[i + os]^.Name := f.Inners[i].Name;
    FVariables[i + os]^.Typ := f.Inners[i].Typ;
    FVariables[i + os]^.Value := 0;
    Case FVariables[i + os]^.Typ Of // Vor Initialisieren der Lokalen Variablen, sollten es Pointerdaten sein ;)
      vtfloat: Begin
          new(pS);
          ps^ := 0.0;
          FVariables[i + os]^.Value := PtrToPtrInt(ps);
        End;
      vtString: Begin
          new(pStr);
          pstr^ := '';
          FVariables[i + os]^.Value := PtrToPtrInt(pstr);
        End;
      vtfield: Begin
          FVariables[i + os]^.Value := PtrToPtrInt(TRecord.Create);
        End;
    End;
    FVariables[i + os]^.Line := -1;
  End;
  // Alle parameter auf den Stack legen
  j := 0;
  For i := 0 To f.ParamVarcount - 1 Do Begin
    Case f.Params[i].Typ Of
      vtint, vtbool: Begin
          If j >= length(Parameter) Then Begin
            Raise Exception.Create('Error invalid number of parameters for function: ' + f.Name);
          End;
          n.line := -1;
          n.value := f.Params[i].Name;
          AppendVariable(n, VarTypeToStr(f.Params[i].Typ));
          FVariables[high(FVariables)]^.value := StringToType(Parameter[j], f.Params[i].Typ, f.Params[i].RecordName);
          inc(j);
        End;
      vtfloat: Begin
          If j >= length(Parameter) Then Begin
            Raise Exception.Create('Error invalid number of parameters for function: ' + f.Name);
          End;
          n.line := -1;
          n.value := f.Params[i].Name;
          AppendVariable(n, VarTypeToStr(f.Params[i].Typ));
          ps := PtrIntToPtr(StringToType(Parameter[j], f.Params[i].Typ, f.Params[i].RecordName));
          ps2 := PtrIntToPtr(FVariables[high(FVariables)]^.value);
          ps2^ := ps^;
          Dispose(ps);
          inc(j);
        End;
      vtString: Begin
          If j >= length(Parameter) Then Begin
            Raise Exception.Create('Error invalid number of parameters for function: ' + f.Name);
          End;
          n.line := -1;
          n.value := f.Params[i].Name;
          AppendVariable(n, VarTypeToStr(f.Params[i].Typ));
          pStr := PtrIntToPtr(StringToType(Parameter[j], f.Params[i].Typ, f.Params[i].RecordName));
          pStr2 := PtrIntToPtr(FVariables[high(FVariables)]^.value);
          pStr2^ := pStr^;
          Dispose(pStr);
          inc(j);
        End;
      vtfield: Begin
          If j >= length(Parameter) Then Begin
            Raise Exception.Create('Error invalid number of parameters for function: ' + f.Name);
          End;
          n.line := -1;
          n.value := f.Params[i].Name;
          AppendVariable(n, f.Params[i].RecordName);
          r := TRecord(PtrIntToPtr(StringToType(Parameter[j], f.Params[i].Typ, f.Params[i].RecordName)));
          r2 := TRecord(PtrIntToPtr(FVariables[high(FVariables)]^.value));
          r2.CopyValuesFrom(r);
          r.free;
          inc(j);
        End;
      vtfieldelement: Begin
          // Nichts das macht pixel schon ...
        End;
    End;
  End;
  If j <> length(Parameter) Then Begin
    Raise Exception.Create('Error invalid number of parameters for function: ' + f.Name);
  End;
  // Die Result Variable auf den Stack legen
  If f Is TFunction Then Begin
    fun := f As TFunction;
    n.line := -1;
    n.value := 'result';
    vi := length(FVariables);
    If fun.Result_.Typ = vtfield Then Begin
      AppendVariable(n, fun.Result_.RecordName);
    End
    Else Begin
      AppendVariable(n, VarTypeToStr(fun.Result_.Typ));
    End;
  End;
  // Aufruf der Eigentlichen Function
  //PlottAllVariables('functioncall');
  f.execute;
  // Ausgabe des Ergebnisses
  If f Is TFunction Then Begin
    Case fun.Result_.Typ Of
      vtbool: Begin
          If FVariables[vi]^.Value = 0 Then
            result := 'false'
          Else
            result := 'true';
        End;
      vtint: Begin
          result := inttostr(FVariables[vi]^.Value);
        End;
      vtfloat: Begin
          ps := PtrIntToPtr(FVariables[vi]^.Value);
          result := floattostr(ps^, forma);
        End;
      vtString: Begin
          pStr := PtrIntToPtr(FVariables[vi]^.Value);
          result := pStr^;
        End;
      vtfield: Begin
          r := TRecord(PtrIntToPtr(FVariables[vi]^.Value));
          result := r.ValuesToString;
        End
    Else Begin
        Raise exception.create('Missing implementation for result type "Tinterpreter.CallFunction"');
      End;
    End;
  End
  Else Begin
    // Es wurde eine Procedure Aufgerufen, die hat natürlich keinen Result
    result := '';
  End;
  // Freigabe des "Stack" speichers
  For i := os To high(FVariables) Do Begin
    DisposeVariable(FVariables[i]);
  End;
  setlength(FVariables, os);
End;

Function Tinterpreter.GetFunctionList: TStringArray;
Var
  i, j: integer;
  s: String;
Begin
  setlength(result, 0);
  For i := 0 To high(FFunctionlist) Do Begin
    If Not FFunctionlist[i].Nested Then Begin
      setlength(result, high(Result) + 2);
      s := FFunctionlist[i].Name + '(';
      For j := 0 To FFunctionlist[i].ParamVarcount - 1 Do Begin
        If Not (FFunctionlist[i].Params[j].Typ In [vtfieldelement]) Then Begin
          If j <> 0 Then Begin
            s := s + ', ';
          End;
          If FFunctionlist[i].Params[j].Typ = vtfield Then Begin
            s := s + '<' + FFunctionlist[i].Params[j].RecordName + '>';
          End
          Else Begin
            s := s + '<' + VarTypeToStr(FFunctionlist[i].Params[j].Typ) + '>';
          End;
        End;
      End;
      If FFunctionlist[i] Is TFunction Then Begin
        If (FFunctionlist[i] As TFunction).Result_.Typ = vtfield Then Begin
          s := s + '):<' + (FFunctionlist[i] As TFunction).Result_.RecordName + '>;';
        End
        Else Begin
          s := s + '):<' + VarTypeToStr((FFunctionlist[i] As TFunction).Result_.Typ) + '>;';
        End;
      End
      Else Begin
        s := s + ')'; // Eine Procedur
      End;
      result[high(result)] := s;
    End;
  End;
End;

Procedure Tinterpreter.SetVar(Name: String; Value: PtrInt;
  ValueType: TVariablenType; index: integer);
Var
  i: Integer;
  s: Single;
  ps: Psingle;
  pstr, pstr2: ^String;
  r, r2: TRecord;
Begin
  If (ValueType <> vtfield) And (ValueType <> vtString) Then Begin
    s := PtrIntToSingle(value, ValueType, index);
  End;
  For i := high(FVariables) Downto 0 Do
    If FVariables[i]^.Name = Name Then Begin
      Case FVariables[i]^.Typ Of
        vtbool, vtint: Begin
            // Todo: hier brüchte es evtl eine Typprüfung..
            FVariables[i]^.Value := round(s);
          End;
        vtfloat: Begin
            ps := PtrIntToPtr(fVariables[i]^.Value);
            ps^ := s;
          End;
        vtString: Begin
            pstr2 := PtrIntToPtr(value);
            pstr := PtrIntToPtr(fVariables[i]^.Value);
            pstr^ := pstr2^;
          End;
        vtfield: Begin
            r := TRecord(PtrIntToPtr(fVariables[i]^.Value));
            r2 := TRecord(PtrIntToPtr(Value));
            r.CopyValuesFrom(r2);
          End;
        vtfieldelement: Begin
            r := TRecord(PtrIntToPtr(fVariables[i]^.Value));
            r.SetFieldValueBySingle(fVariables[i]^.RecordFieldIndex, s);
          End;
      End;
      break;
    End;
  //PlottAllVariables('SetVar');
End;

Function Tinterpreter.GetVarVal(Name: String): PtrInt;
Var
  i: Integer;
Begin
  name := lowercase(name);
  For i := high(FVariables) Downto 0 Do
    If FVariables[i]^.Name = Name Then Begin
      result := FVariables[i]^.Value;
      exit;
    End;
  Raise exception.create('Error "' + name + '" not known.');
End;

Function Tinterpreter.AppendVariable(VarName: TName; VarType: String
  ): TVariablenType;
Var
  i, index, j: Integer;
  p: PSingle;
  pstr: ^String;
  r: TRecord;
Begin
  result := vtnone; // Egal, wenn das unten nicht definiert wird, gehts eh in Error
  setlength(FVariables, high(FVariables) + 2);
  index := high(FVariables);
  Case VarType Of
    'boolean': Begin
        new(FVariables[index]);
        FVariables[index]^.Name := VarName.value;
        FVariables[index]^.Funktionsaufrufzeiger := false;
        setlength(FVariables[index]^.uebergabeparameter, 0);
        FVariables[index]^.Line := VarName.line;
        FVariables[index]^.Typ := vtbool;
        FVariables[index]^.Value := 0;
        FVariables[index]^.RecordName := '';
        FVariables[index]^.RecordFieldIndex := 0;
        result := vtbool;
      End;
    'integer': Begin
        new(FVariables[index]);
        FVariables[index]^.Name := VarName.value;
        FVariables[index]^.Funktionsaufrufzeiger := false;
        setlength(FVariables[index]^.uebergabeparameter, 0);
        FVariables[index]^.Line := VarName.line;
        FVariables[index]^.Typ := vtint;
        FVariables[index]^.Value := 0;
        FVariables[index]^.RecordName := '';
        FVariables[index]^.RecordFieldIndex := 0;
        result := vtint;
      End;
    'single': Begin
        new(FVariables[index]);
        FVariables[index]^.Name := VarName.value;
        FVariables[index]^.Funktionsaufrufzeiger := false;
        setlength(FVariables[index]^.uebergabeparameter, 0);
        FVariables[index]^.Line := VarName.line;
        FVariables[index]^.Typ := vtfloat;
        new(P);
        p^ := 0.0;
        FVariables[index]^.Value := PtrToPtrInt(p);
        FVariables[index]^.RecordName := '';
        FVariables[index]^.RecordFieldIndex := 0;
        result := vtfloat;
      End;
    'string': Begin
        new(FVariables[index]);
        FVariables[index]^.Name := VarName.value;
        FVariables[index]^.Funktionsaufrufzeiger := false;
        setlength(FVariables[index]^.uebergabeparameter, 0);
        FVariables[index]^.Line := VarName.line;
        FVariables[index]^.Typ := vtString;
        new(pstr);
        pstr^ := '';
        FVariables[index]^.Value := PtrToPtrInt(pstr);
        FVariables[index]^.RecordName := '';
        FVariables[index]^.RecordFieldIndex := 0;
        result := vtfloat;
      End;
  Else Begin
      For i := 0 To high(FRecords) Do Begin
        If FRecords[i].Name = VarType Then Begin
          new(FVariables[index]);
          FVariables[index]^.Name := VarName.value;
          FVariables[index]^.Funktionsaufrufzeiger := false;
          setlength(FVariables[index]^.uebergabeparameter, 0);
          FVariables[index]^.Line := VarName.line;
          FVariables[index]^.Typ := vtfield;
          r := TRecord.Create();
          r.CloneFrom(FRecords[i]);
          FVariables[index]^.Value := PtrToPtrInt(r);
          FVariables[index]^.RecordName := VarType;
          FVariables[index]^.RecordFieldIndex := 0;
          index := length(FVariables);
          setlength(FVariables, length(FVariables) + length(r.fFields));
          For j := 0 To high(r.fFields) Do Begin
            new(FVariables[index + j]);
            FVariables[index + j]^.Name := VarName.value + '.' + lowercase(r.fFields[j].Name);
            FVariables[index + j]^.Funktionsaufrufzeiger := false;
            setlength(FVariables[index + j]^.uebergabeparameter, 0);
            FVariables[index + j]^.Line := VarName.line;
            FVariables[index + j]^.Typ := vtfieldelement;
            FVariables[index + j]^.Value := PtrToPtrInt(r);
            FVariables[index + j]^.RecordFieldIndex := j;
            FVariables[index + j]^.RecordName := r.Name;
          End;
        End;
        result := vtfield;
        exit;
      End;
    End;
    setlength(FVariables, high(FVariables)); // Das fälschlicherweise angefügte Element wieder löschen
    Abbruch('Error unknown type "' + VarType + '" at line : ' + inttostr(VarName.line));
  End;
End;

Function Tinterpreter.FunctionResultType(Functionname: String): TVariablenType;
Var
  i: Integer;
Begin
  // 1. Interne Funktionen
  For i := 0 To high(FFunctionlist) Do
    If FFunctionlist[i].Name = Functionname Then Begin
      If (FFunctionlist[i] Is TFunction) Then Begin
        result := (FFunctionlist[i] As TFunction).Result_.Typ;
      End
      Else Begin
        result := vtnone;
      End;
      exit;
    End;
  // 2. Externe Functionen
  For i := 0 To high(fExternalProcedures) Do Begin
    If fExternalProcedures[i].Name = Functionname Then Begin
      result := fExternalProcedures[i].restype;
      exit;
    End;
  End;
  Raise Exception.Create('Could not resolve ' + Functionname);
End;

{$IFDEF debuggroutines}

Procedure Tinterpreter.PlottAllVariables(caption: String = '');
Var
  i: Integer;
Begin
  (*
   * In der IDE Ansicht -> Debugg Fenster -> Terminal Ausgabe, und schon sieht man alles *g*
   *)
  If caption <> '' Then
    debugln(caption)
  Else
    debugln('Variablen');
  For i := 0 To high(FVariables) Do Begin
    debugln(format(
      '%2d F:%d Line:%d Name:%s Typ:%s Value:%d',
      [i
      , ord(FVariables[i]^.Funktionsaufrufzeiger),
        FVariables[i]^.Line,
        FVariables[i]^.Name,
        VarTypeToStr(FVariables[i]^.Typ),
        FVariables[i]^.Value
        ]
        ), forma);
  End;
End;
{$ENDIF}

Function Tinterpreter.EatFor: TForLoop;
Var
  p: PCalcTree;
  i: integer;
  l: String;
  b: Boolean;
Begin
  eat('for');
  l := faLowertoken;
  If Not CheckVarname(l) Then Abbruch('Error "' + fatoken + '" is not a valid variable, at Line ' + inttostr(FatokenLine));
  (*
  Prüfen obs die Variable überhaupt gibt, ist aber noch lang nicht ausgereift ...
  *)
  b := false;
  For i := 0 To high(fvariables) Do Begin
    If fvariables[i]^.Name = l Then b := true;
  End;
  If Not b Then Abbruch('Error, unknown "' + fatoken + '", at Line ' + inttostr(FatokenLine));
  EatToken;
  eat(':=');
  p := EatAusdruck(['to', 'downto']);
  If faLowertoken = 'to' Then Begin
    result := TForLoop.create(self);
    eat('to');
  End
  Else Begin
    result := TForDownLoop.create(self);
    eat('downto');
  End;
  If Not ferror Then Begin
    result.Laufvariable := l;
    result.von := p;
    result.bis := EatAusdruck(['do']);
    eat('do');
    result.InnerCode := EatInner;
    If falowertoken = ';' Then
      eat(';');
  End;
End;

Function Tinterpreter.EatWhile: TWhile;
Begin
  eat('while');
  result := TWhile.create(self);
  result.Bedingung := EatAusdruck(['do']);
  eat('do');
  result.InnerCode := EatInner;
  If faLowertoken = ';' Then
    eat(';');
End;

{ TCompiledToken }

Constructor TCompiledToken.Create(Owner: Tinterpreter);
Begin
  fowner := Owner;
End;

Procedure TCompiledToken.Execute;
Begin
  (*
  Wenn diese Exception geworfen wird, dann bedeutet das das eine Abgeleitete Klasse
  die Execute Methode nicht überschrieben, bzw gesetzt hat.
  *)
  Raise Exception.create('internal Error please contact : www.Corpsman.de.vu');
End;

{ TIfConstruct }

Constructor TIfConstruct.create(Owner: Tinterpreter);
Begin
  Inherited;
  Bedingung := Nil;
  setlength(TrueCode, 0);
  setlength(FalseCode, 0);
End;

Destructor TIfConstruct.Destroy;
Var
  i: integer;
Begin
  If assigned(Bedingung) Then
    fowner.solver.FreeCalcTree(Bedingung);
  For i := 0 To high(truecode) Do
    truecode[i].free;
  For i := 0 To high(falsecode) Do
    falsecode[i].free;
  setlength(TrueCode, 0);
  setlength(FalseCode, 0);
End;

Procedure TIfConstruct.Execute;
Var
  dummy: Boolean;
  p: PVariable;
  i: Integer;
  v: Integer;
Begin
  p := fowner.solver.Calc(Bedingung, dummy);
  // Eigentlich müsste V genau wie bei TAssignment freigegeben werden
  // Das V aber immer vom Typ Bool ist, bleibt uns das hier erspart :-)
  v := fowner.ResolveValue(p);
  If v = 0 Then Begin
    For i := 0 To high(falseCode) Do
      falseCode[i].execute;
  End
  Else Begin
    For i := 0 To high(TrueCode) Do
      TrueCode[i].execute;
  End;
  If Not dummy Then Begin
    For i := 0 To High(p^.uebergabeparameter) Do
      fowner.Solver.FreeCalcTree(p^.uebergabeparameter[i]);
    fowner.DisposeVariable(p);
  End;
End;

{ TAssignment }

Constructor TAssignment.create(Owner: Tinterpreter);
Begin
  Inherited;
  Ausdruck := Nil;
End;

Destructor TAssignment.Destroy;
Begin
  If assigned(Ausdruck) Then
    fowner.solver.FreeCalcTree(Ausdruck);
End;

Procedure TAssignment.Execute;
Var
  p, pv: PVariable;
  dummy: Boolean;
  w: PtrInt;
Begin
  p := fowner.solver.calc(Ausdruck, dummy);
  w := fowner.ResolveValue(p);
  fowner.SetVar(Ziel, w, p^.Typ, p^.RecordFieldIndex);
  If p^.Funktionsaufrufzeiger Then Begin
    // Bei Funktionszeigern, muss w korrekt wieder Freigegeben werden
    // Da w aber nicht in der PVariable Datenstruktur vorliegt muss diese
    // Erst Künstlich geschaffen werden, auf diese Art und weise kann
    // dann wieder DisposeVariable verwendet werden, ohne das hier ein
    // sonderfall entsteht.
    new(pv);
    pv^.Typ := p^.Typ;
    pv^.Value := w;
    fowner.DisposeVariable(pv);
  End;
  If Not dummy Then Begin
    fowner.DisposeVariable(p);
  End;
End;

{ TForLoop }

Constructor TForLoop.create(Owner: Tinterpreter);
Begin
  Inherited;
  setlength(InnerCode, 0);
  von := Nil;
  bis := Nil;
End;

Destructor TForLoop.Destroy;
Var
  i: Integer;
Begin
  fowner.solver.FreeCalcTree(von);
  von := Nil;
  fowner.solver.FreeCalcTree(bis);
  bis := Nil;
  For i := 0 To high(innerCode) Do Begin
    InnerCode[i].free;
  End;
  setlength(InnerCode, 0);
End;

Procedure TForLoop.Execute;
Var
  p1, p2: PVariable;
  d1, d2: Boolean;
  w1, w2, i, j: Integer;
Begin
  d1 := true;
  d2 := true;
  p1 := fowner.solver.calc(von, d1);
  w1 := fowner.ResolveValue(p1);
  p2 := fowner.solver.calc(bis, d2);
  w2 := fowner.ResolveValue(p2);
  // Eigentlich müssten w1 und w2 genau wie bei TAssignment freigegeben werden
  // Das w1 und w2 aber immer vom Typ Integer sind, bleibt uns das hier erspart :-)
  For i := w1 To w2 Do Begin
    fowner.SetVar(Laufvariable, i, vtint, 0);
    For j := 0 To high(innercode) Do
      innercode[j].execute;
  End;
  If Not d1 Then Begin
    fowner.DisposeVariable(p1);
  End;
  If Not d2 Then Begin
    fowner.DisposeVariable(p2);
  End;
End;

{ TForDownLoop }

Procedure TForDownLoop.Execute;
Var
  p1, p2: PVariable;
  d1, d2: Boolean;
  w1, w2, i, j: Integer;
Begin
  d1 := True;
  d2 := True;
  p1 := fowner.solver.calc(von, d1);
  w1 := fowner.ResolveValue(p1);
  p2 := fowner.solver.calc(bis, d2);
  w2 := fowner.ResolveValue(p2);
  // Eigentlich müssten w1 und w2 genau wie bei TAssignment freigegeben werden
  // Das w1 und w2 aber immer vom Typ Integer sind, bleibt uns das hier erspart :-)
  For i := w1 Downto w2 Do Begin
    fowner.SetVar(Laufvariable, i, vtint, 0);
    For j := 0 To high(innercode) Do
      innercode[j].execute;
  End;
  If Not d1 Then Begin
    fowner.DisposeVariable(p1);
  End;
  If Not d2 Then Begin
    fowner.DisposeVariable(p2);
  End;
End;

{ TWhile }

Constructor TWhile.create(Owner: Tinterpreter);
Begin
  Inherited;
  Bedingung := Nil;
  SetLength(innerCode, 0);
End;

Destructor TWhile.Destroy;
Var
  i: Integer;
Begin
  If assigned(Bedingung) Then
    fowner.solver.FreeCalcTree(Bedingung);
  For i := 0 To high(innerCode) Do Begin
    InnerCode[i].free;
  End;
  setlength(InnerCode, 0);
End;

Procedure TWhile.Execute;
Var
  bed: PVariable;
  w, i: Integer;
  d: Boolean;
Begin
  // Abprüfen der Bedingung
  bed := fowner.solver.calc(Bedingung, d);
  w := fowner.ResolveValue(bed);
  // Eigentlich müsste V genau wie bei TAssignment freigegeben werden
  // Das V aber immer vom Typ Bool ist, bleibt uns das hier erspart :-)
  If Not d Then Begin
    fowner.DisposeVariable(bed);
  End;
  // Das eigentliche Ausführen der While Schleife
  While w <> 0 Do Begin
    For i := 0 To high(innercode) Do
      innercode[i].execute;
    // Abprüfen der Bedingung
    bed := fowner.solver.calc(Bedingung, d);
    w := fowner.ResolveValue(bed);
    // Eigentlich müsste V genau wie bei TAssignment freigegeben werden
    // Das V aber immer vom Typ Bool ist, bleibt uns das hier erspart :-)
    If Not d Then Begin
      fowner.DisposeVariable(bed);
    End;
  End;
End;

Initialization

  Tok := TTokenizer.create;
  tok.CaseSensitive := false;
  tok.ClearRules;
  tok.ClearOperators;
  tok.ClearSeperators;
  tok.AddRule('''', ''''); // Strings
  tok.Addoperator('>=');
  tok.Addoperator('<=');
  tok.Addoperator('<>');
  tok.Addoperator('=');
  tok.Addoperator('<');
  tok.Addoperator('/');
  tok.Addoperator('>');
  tok.Addoperator(':=');
  tok.Addoperator(':');
  tok.Addoperator(';');
  tok.Addoperator(',');
  tok.Addoperator('(');
  tok.Addoperator(')');
  tok.Addoperator('+');
  tok.Addoperator('-');
  tok.Addoperator('*');
  tok.Addoperator('~'); // Unser Zeilen Nummerierungszeichen
  tok.Addseperator(' ');
  tok.Addseperator(#13);
  tok.Addseperator(#10);
  unc := TUnCommenter.create;
  unc.DellEmptyLines := true;
  unc.ExtraCharLine := '~';
  unc.NumberLines := True;
  unc.AddRule('//', '', true);
  unc.AddRule('(*', '*)', false);
  unc.AddRule('{', '}', false);
  forma := DefaultFormatSettings;
  forma.DecimalSeparator := '.';

Finalization
  tok.free;
  unc.free;

End.

