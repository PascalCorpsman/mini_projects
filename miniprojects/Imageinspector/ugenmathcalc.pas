(******************************************************************************)
(* ugenmathcalc.pas                                                 1.11.2007 *)
(*                                                                            *)
(* Version     : 0.11                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Diese Unit zeigt die Implementierung eines Generischen Mathe *)
(*               solvers. Die Klasse TGenMathCalc kann benutzt werden um      *)
(*               Verschiedenste Mathematische Ausdrücke zu berechnen.         *)
(*               Die genau Nutzung ist im Sample zu ersehen.                  *)
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
(* Known Issues:                                                              *)
(*              - bei Unären Operanden, kann es vorkommen, das ein noch nicht *)
(*                evaluierter Baum geschluckt wird                            *)
(*                => Dann wird das Ergebniss ungültig.                        *)
(*                Workaround : Durch anpassen der Bindungsstärke der Unären   *)
(*                             Operanden kann dieses Problem meistens         *)
(*                             umschifft werden. Wenn nicht fängt der Code    *)
(*                             dies nun mittels AV ab.                        *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*               0.02 - Conversion to Unicode                                 *)
(*               0.03 - Added Short Circuit BinOP                             *)
(*               0.04 - Added Support for Random Values, Raise Exception if   *)
(*                      Internal Symbol is in workdata                        *)
(*               0.05 - Added Bugfix with "Raise Exception if Internal Symbol *)
(*                      is in workdata"                                       *)
(*               0.06 - Added Support for handling Unary Operands with same   *)
(*                      symbol than Binary Operands                           *)
(*               0.07 - Added visualize routine for calculated trees          *)
(*               0.08 - Added raise exceptions on invalid calculated formulas *)
(*                      (see known bugs)                                      *)
(*               0.09 - Fixed parsing direction for binary Operands           *)
(*               0.10 - Anpassungen für 64-Bit Systeme                        *)
(*               0.11 - Fix Memleak bei Short-Circuit Evaluation              *)
(*                                                                            *)
(******************************************************************************)
Unit ugenmathcalc;

{$MODE objfpc}{$H+}

(*
 * If you get a compiler error with missing file
 * just create a file namend "ugenmathcalc.inc" in your project folder and
 * insert the following content:
 *
 * ---------- Content of file ----------

// Use if you want to use the Tokenizer from www.Corpsman.de
{$DEFINE UseTokenizer}

// Use OOP Callbacks for operands
{$DEFINE OOP_Callbacks}

// Give the ability to render the calculation tree on a canvas
{.$define ALLOW_RENDERTREE}

// Use Binary operands left and right sided.
{.$define Beidseitig}

   ---------- End content of file ----------
 *)

{$I ugenmathcalc.inc} // Einbinden der diversen Steuerdefines

Interface

Uses
{$IFDEF UseTokenizer}
  utokenizer,
{$ENDIF}
{$IFDEF ALLOW_RENDERTREE}
  math,
  Classes,
  Graphics,
{$ENDIF}
  Sysutils;

Type

{$IFNDEF UseTokenizer}
  TToken = Record
    Value: String; // Der Token ansich
    Line: PtrInt; // Die zeile in der der token steht (wird auch als Pointer Missbraucht, muss daher PtrInt sein !)
  End;
  // Pointer auf Array of Token
  TTokenarray = Array Of TToken;
{$ELSE}
  TToken = utokenizer.TToken;
  TTokenarray = utokenizer.TTokenarray;
{$ENDIF}

{$IFDEF ALLOW_RENDERTREE}
  TElementToString = Function(Value: Pointer): String;
{$ENDIF}

  // Unser Generischer Parser sollte auch mit Variablen umgehen können.
  // Dies wird hier gemacht ;)
  TVarItem = Record
    Name: String;
    Value: Pointer;
    Callback: Boolean; // Wenn True, dann ist Pointer kein Zeiger auf eine Variable, sondern ein Zeiger auf eine Callback der Art TDynCreator siehe Deklaration TDynCreator
  End;

  TModifiier = Record // Zur Konvertierung von Gleichnamigen Unären Operatoden und Binären Operatoren
    FromOP, ToOP: String;
  End;

  TOperatorList = Array Of String; // Eine Liste aller verwendeten Operatoren
  TModifiierList = Array Of TModifiier;

  TVarlist = Array Of TVarItem;

  (*
  TShortBinProc ermöglicht eine Short Circuit Evaluation diese geschieht folgendermasen.

  Will Calc eine mittels TShortBinProc übergebene Funktion berechnen so wird zuerst der
  Parameter 1 ausgewertet und mittels

  TShortBinProc(Value1, nil, true)

  Aufgerufen.
  Wird der Var Parameter von TShortBinProc auf False gesetzt, so
  Rechnet calc mit dem Zurückgegebenen wert weiter, ohne Value 2 zu berechnen.

  Wird der Var Parameter nicht verändert ( = false gesetzt ), so wird auch Value 2 Berechnet und

  TShortBinProc(TShortBinProc(Value1, nil, true), Value2, false)
  Aufgerufen. Wichtig : TShortBinProc(Value1, nil, true) wird hierbei garantiert nur 1 mal ausgewertet !!

  *)

  TShortBinProc = Function(Value1, Value2: Pointer; Var ShortCircuit: Boolean): Pointer{$IFDEF OOP_Callbacks} Of Object{$ENDIF}; // Eine Binäre Operation
  TBinProc = Function(Value1, Value2: Pointer): Pointer{$IFDEF OOP_Callbacks} Of Object{$ENDIF}; // Eine Binäre Operation
  TUnProc = Function(Value1: Pointer): Pointer{$IFDEF OOP_Callbacks} Of Object{$ENDIF}; // Eine Unäre Operation
  TFreeProc = Procedure(Value: Pointer){$IFDEF OOP_Callbacks} Of Object{$ENDIF}; // Wird Aufgerufen um ein Value Frei zu geben ( Variablen werden nicht Freigegeben )
  TCreateProc = Function(Value: String): Pointer{$IFDEF OOP_Callbacks} Of Object{$ENDIF}; // Wandelt einen String der als Zahl Erkannt wird in einen Pointer um
  (*
    Will man in einer Formel pro Auswertung, mehr wie nur eine Zufallsvariable nutzen ( z.B. Random * Random )
    so kann man dies nicht mittels Variablen realisieren, da eine Variable immer nur 1 mal pro TGenMathCalc.Calc gesetzt werden kann.
    Um dieses Feature dennoch zu realisieren kann der TVarlist anstatt einen Pointer auf die Variable einen Pointer auf eine TDynCreator
    Routine geben, diese wird dann bei jeder "Auswertung" explizit aufgerufen.
   *)
  TDynCreator = Function(): Pointer; // Die Callback Routine, wenn man eine Variable braucht die bei jeder Auswertung neu erzeugt werden muss ( z.B. Zufall )

  // Der RechenBaum
  PCalcTree = ^TCalctree;

  TCalcTree = Record
    Left: PCalcTree; // Linker UnterBaum
    Right: PCalcTree; // Rechter UnterBaum
    Operation: PtrInt; // Operator , >= 0 = Pointer in OP Liste, < 0 ( = -1 )= Blatt
    Value: Pointer; // Pointer auf die Variable
    isVar: Boolean; // Wenn True dann wird der Pointer nicht Freigegeben, wenn der Tree Freigegeben wird.
    isValue: Boolean; // Wenn True dann ist Value eine "Zahl" / Oder eine Callback auf eine Konstante
    isCallback: Boolean; // Wenn True, dann ist die Value eine Callback und wird extra aufgerufen
    alreadyParsed: Boolean; // Ist damit der Parser nicht noch nen Extra Typ Braucht, für den eigentlichen Rechner aber unnötig.
  End;

  // Ein Operator
  TOperator = Record
{$IFDEF Beidseitig}
    UnProc_Left: TUnProc;
    UnProc_Right: TUnProc;
{$ELSE}
    UnProc: TUnProc;
{$ENDIF}
    BinProc: TBinProc;
    SBinProc: TShortBinProc;
    Identifer: String;
  End;

  // Die Klasse die alles macht
  TGenMathCalc = Class
  private
    fRecDepth: integer;
    // die Operatoren
    FOperators: Array Of TOperator;
    FOperatorList: TOperatorList; // Eine Liste aller verwendeten Operatoren
    FUnaryModifiierList: TModifiierList; // Eine Liste Aller Unären Operatoren, welche vor dem Parsen konvertiert werden müssen
    Function CheckKlammern(Tokenlist: TTokenarray): Boolean; // Gibt True zurück wenn die Klammerung korreckt ist.
    Function CalcHelper(Value: PCalcTree): Pointer; // Rechnet einen TCalctree aus
  public
    OnCreateValue: TCreateProc; // Wird aufgerufen wenn der Parser einen Token in eine Value umwandeln mus
    OnFreeValue: TFreeProc; // Wird aufgerufen um die Pointer entsprechend wieder Frei zu geben
    Constructor Create;
    Destructor Destroy; override;
    Procedure AddBinOP(Identifer: String; Proc: TBinProc);
    Procedure AddShortBinOP(Identifer: String; Proc: TShortBinProc);
{$IFDEF Beidseitig}
    Procedure AddUnOP_Left(Identifer: String; Proc: TUnProc);
    Procedure AddUnOP_Right(Identifer: String; Proc: TUnProc);
{$ELSE}
    Procedure AddUnOP(Identifer: String; Proc: TUnProc);
{$ENDIF}
    Procedure ClearOPs; // Löscht die Liste der Operanden
    Function Parse(TokenList: TTokenarray; Varlist: TVarlist): PCalcTree; // Parst den Baum aus der Tokenliste
    Function Calc(Value: PCalcTree; Out Result_is_Value: Boolean): Pointer; // Rechnet einen TCalctree aus
    Procedure FreeCalcTree(Value: PCalcTree);
  End;

{$IFDEF ALLOW_RENDERTREE}
Procedure RenderTree(Const Calculator: TGenMathCalc; Const Tree: PCalcTree;
  Const Canvas: tcanvas; Const Rect: TRect; Conversion: TElementToString;
  VarList: TVarlist);
{$ENDIF}

{$IFNDEF UseTokenizer}
Function PtrToPtrInt(p: Pointer): Ptrint Inline;
Function PtrIntToPtr(p: PtrInt): Pointer Inline;
{$ENDIF}

Implementation
(*
Dieses Zeichen wird für die interne Pointer umwandlungen benutzt, und sollte nicht Teil
irgendeines Ausdruckes sein !!
*)
Const
  InternalSymbol = '~';

{$IFDEF ALLOW_RENDERTREE}

Procedure RenderTree(Const Calculator: TGenMathCalc; Const Tree: PCalcTree;
  Const Canvas: tcanvas; Const Rect: TRect; Conversion: TElementToString;
  VarList: TVarlist);
Type
  PTMPSubTree = ^TTMPSubTree;
  TTMPSubTree = Record
    Left, Right: PTMPSubTree;
    x, y: Integer;
    Text: String;
  End;
Var
  x, y, xmax, yMax: Integer;
  dx, dy: Single;

  (*
   * Convertiert einen PsubTree in einen PTMPSubTree, und schreibt gleichzeitig die entsprechenden Labels
   *)

  Procedure ConvertTree(In_: PCalcTree; Out_: PTMPSubTree);
  Var
    t: PTMPSubTree;
    i: integer;
    b: Boolean;
  Begin
    //    If assigned(in_) Then Begin -- Braucht man nicht, da alles Vorher gechecked wird.
    y := y + 1;
    ymax := max(ymax, y);
    // Traverse Left
    If Assigned(in_^.Left) Then Begin
      new(t);
      t^.Left := Nil;
      t^.Right := Nil;
      out_^.Left := t;
      ConvertTree(in_^.Left, t);
    End;
    // Visit
    x := x + 1;
    xmax := x;
    If in_^.Operation >= 0 Then Begin
      // Der Knoten ist ein Innerer Knoten = Operator
      out_^.Text := Calculator.FOperators[in_^.Operation].Identifer;
      If pos(InternalSymbol, out_^.Text) <> 0 Then Begin // Die Unären Operatoren kennzeichnen sich so
        delete(out_^.Text, pos(InternalSymbol, out_^.Text), 1);
      End;
    End
    Else Begin
      // Der Knoten ist ein Blatt
      b := false;
      // Variablen und Callbacks im Namen Auflösen
      For i := 0 To high(VarList) Do Begin
        If varlist[i].Value = in_^.Value Then Begin
          out_^.Text := VarList[i].Name;
          b := true;
          break;
        End;
      End;
      // alles andere Ist ein normales Blatt und muss von der Anwendung "Konvertiert" werden.
      If Not b Then Begin
        out_^.Text := Conversion(in_^.Value);
      End;
    End;
    out_^.x := x;
    out_^.y := y;
    // Traverse Right
    If Assigned(in_^.Right) Then Begin
      new(t);
      t^.Left := Nil;
      t^.Right := Nil;
      out_^.Right := t;
      ConvertTree(in_^.Right, t);
    End;
    y := y - 1;
    //    End; -- Braucht man nicht, da alles Vorher gechecked wird.
  End;

  Procedure FreeSubTree(tree: PTMPSubTree);
  Begin
    If Assigned(tree) Then Begin
      FreeSubTree(tree^.Left);
      FreeSubTree(tree^.Right);
      dispose(tree);
    End;
  End;

  Procedure PrintEdgeTree(Tree: PTMPSubTree);
  Begin
    If assigned(tree) Then Begin
      If assigned(tree^.Left) Then Begin
        canvas.MoveTo(min(rect.Left, Rect.Right) + round(dx * tree^.x), min(rect.Top, Rect.Bottom) + round(dy * tree^.y));
        canvas.LineTo(min(rect.Left, Rect.Right) + round(dx * tree^.Left^.x), min(rect.Top, Rect.Bottom) + round(dy * tree^.Left^.y));
      End;
      If assigned(tree^.Right) Then Begin
        canvas.MoveTo(min(rect.Left, Rect.Right) + round(dx * tree^.x), min(rect.Top, Rect.Bottom) + round(dy * tree^.y));
        canvas.LineTo(min(rect.Left, Rect.Right) + round(dx * tree^.Right^.x), min(rect.Top, Rect.Bottom) + round(dy * tree^.Right^.y));
      End;
      PrintEdgeTree(tree^.Left);
      PrintEdgeTree(tree^.Right);
    End;
  End;

  Procedure PrintNodeTree(Tree: PTMPSubTree);
  Const
    distance = 5;
  Var
    st: String;
    ddx, ddy: integer;
  Begin
    If assigned(tree) Then Begin
      st := tree^.Text; //Conversion(tree^.Element);
      ddx := Canvas.TextWidth(st) Div 2;
      ddy := canvas.TextHeight(st) Div 2;
      // Das "Kringelchen"
      canvas.Ellipse(min(rect.Left, Rect.Right) + round(dx * tree^.x) - distance - ddx,
        min(rect.Top, Rect.Bottom) + round(dy * tree^.y) - distance - ddy,
        min(rect.Left, Rect.Right) + round(dx * tree^.x) + distance + Canvas.TextWidth(st) - ddx,
        min(rect.Top, Rect.Bottom) + round(dy * tree^.y) + distance + Canvas.Textheight(st) - ddy);
      // Der Text
      canvas.TextOut(min(rect.Left, Rect.Right) + round(dx * tree^.x) - ddx,
        min(rect.Top, Rect.Bottom) + round(dy * tree^.y) - ddy, st);
      PrintNodeTree(tree^.Left);
      PrintNodeTree(tree^.Right);
    End;
  End;

Var
  TMPTreeRoot: PTMPSubTree;
  w, h: Integer;
Begin
  // Den Screen Löschen
  canvas.Brush.Color := clwhite;
  canvas.Brush.Style := bssolid;
  canvas.Pen.Color := clwhite;
  canvas.Rectangle(rect);
  If Not assigned(tree) Then exit;
  new(TMPTreeRoot);
  TMPTreeRoot^.Left := Nil;
  TMPTreeRoot^.Right := Nil;
  x := 0;
  y := 0;
  xmax := 0;
  ymax := 0;
  // Umrechnen des Baumes in einen mit Coordinaten
  ConvertTree(Tree, TMPTreeRoot);
  w := max(rect.Left, Rect.Right) - min(rect.Left, Rect.Right);
  h := max(rect.Top, Rect.Bottom) - min(rect.Top, Rect.Bottom);
  xmax := xmax + 1;
  ymax := ymax + 1;
  dx := w / xmax;
  dy := h / ymax;
  // Malen des Baumes
  canvas.Pen.Color := clblack;
  canvas.Font.Color := clblack;
  canvas.Brush.Color := clwhite;
  canvas.Brush.Style := bssolid;
  PrintEdgeTree(TMPTreeRoot);
  PrintNodeTree(TMPTreeRoot);
  // Freigeben des Temp Mal Baumes
  FreeSubTree(TMPTreeRoot);
End;
{$ENDIF}

{$IFNDEF UseTokenizer}

Function PtrIntToPtr(p: PtrInt): Pointer Inline;
Begin
  result := {%H-}Pointer(p); // Kommt hier immer noch ein Hinweis, dann hat die Codeformatierung zwischen } und P ein Leerzeichen gemacht, das darf nicht sein.
End;

Function PtrToPtrInt(p: Pointer): Ptrint Inline;
Begin
  result := {%H-}Ptrint(p); // Kommt hier immer noch ein Hinweis, dann hat die Codeformatierung zwischen } und P ein Leerzeichen gemacht, das darf nicht sein.
End;
{$ENDIF}

Procedure FixUnaryOperands(Var TokenList: TTokenArray;
  UnaryModifierList: Array Of TModifiier;
  Operators: TOperatorList);
Var
  inUnary, inOP,
    i, j: integer;
Begin
  For i := 0 To high(TokenList) - 1 Do Begin
    inUnary := -1;
    // Schaun ob der Token in der Unary Liste ist
    For j := 0 To high(UnaryModifierList) Do Begin
      If TokenList[i].Value = UnaryModifierList[j].FromOP Then Begin
        inUnary := j;
        break;
      End;
    End;
    inOP := -1;
    If (i > 0) Then Begin
      For j := 0 To high(Operators) Do Begin
        If (TokenList[i - 1].Value = Operators[j]) Then Begin
          inOP := j;
          break;
        End;
      End;
    End;
    (*
     * Ein Unärer Operator liegt vor, wenn Links
     * ein Operator ist und i ist ein Unärer Operator
     *)
    If (inUnary <> -1) And ((I = 0) Or (inOP <> -1)) Then Begin
      TokenList[i].Value := UnaryModifierList[inUnary].ToOP;
    End;
  End;
End;

{ TGenMathCalc }

Constructor TGenMathCalc.Create;
Begin
  Inherited;
  Setlength(Foperators, 0);
  fRecDepth := 0;
  setlength(FOperatorList, 0);
End;

Destructor TGenMathCalc.Destroy;
Begin
  ClearOPs;
End;

Procedure TGenMathCalc.AddBinOP(Identifer: String; Proc: TBinProc);
Var
  i: Integer;
Begin
  If pos(InternalSymbol, Identifer) <> 0 Then Begin
    Raise exception.create('Error "' + InternalSymbol + '" not allowed in Identifer.');
  End;
  Identifer := lowercase(Identifer);
  If Length(Identifer) = 0 Then
    Raise Exception.create('Error Identiferlength has to be grater 0.');
  For i := 0 To High(Foperators) Do
    //If Foperators[i].Identifer = Identifer Then
    If (Foperators[i].Identifer = Identifer) And assigned(Foperators[i].BinProc) Then
      Raise Exception.create('Error double used Identifer.');
  setlength(Foperators, high(Foperators) + 2);
  //  Foperators[high(Foperators)].Unaer := False;
  Foperators[high(Foperators)].BinProc := Proc;
  Foperators[high(Foperators)].SBinProc := Nil;
{$IFDEF Beidseitig}
  Foperators[high(Foperators)].UnProc_left := Nil;
  Foperators[high(Foperators)].UnProc_Right := Nil;
{$ELSE}
  Foperators[high(Foperators)].UnProc := Nil;
{$ENDIF}
  Foperators[high(Foperators)].Identifer := Identifer;
  setlength(FOperatorList, high(FOperatorList) + 2);
  FOperatorList[high(FOperatorList)] := Identifer;
  // Suchen ob Operatoren "Gleich" sind
  For i := 0 To high(FOperators) - 1 Do Begin
    If FOperators[i].Identifer = Identifer Then Begin
      FOperators[i].Identifer := Identifer + InternalSymbol; // Der Andere ist der Unäre Operator, und muss geändert werden
      FOperatorList[i] := Identifer + InternalSymbol; // Der Andere ist der Unäre Operator, und muss geändert werden
      SetLength(FUnaryModifiierList, high(FUnaryModifiierList) + 2);
      FUnaryModifiierList[high(FUnaryModifiierList)].FromOP := Identifer;
      FUnaryModifiierList[high(FUnaryModifiierList)].ToOP := Identifer + InternalSymbol;
    End;
  End;
End;

Procedure TGenMathCalc.AddShortBinOP(Identifer: String; Proc: TShortBinProc);
Var
  i: Integer;
Begin
  If pos(InternalSymbol, Identifer) <> 0 Then Begin
    Raise exception.create('Error "' + InternalSymbol + '" not allowed in Identifer.');
  End;
  Identifer := lowercase(Identifer);
  If Length(Identifer) = 0 Then
    Raise Exception.create('Error Identiferlength has to be grater 0.');
  For i := 0 To High(Foperators) Do
    //If Foperators[i].Identifer = Identifer Then
    If (Foperators[i].Identifer = Identifer) And assigned(Foperators[i].BinProc) Then
      Raise Exception.create('Error double used Identifer.');
  setlength(Foperators, high(Foperators) + 2);
  //  Foperators[high(Foperators)].Unaer := False;
  Foperators[high(Foperators)].BinProc := Nil;
  Foperators[high(Foperators)].SBinProc := Proc;
{$IFDEF Beidseitig}
  Foperators[high(Foperators)].UnProc_left := Nil;
  Foperators[high(Foperators)].UnProc_Right := Nil;
{$ELSE}
  Foperators[high(Foperators)].UnProc := Nil;
{$ENDIF}
  Foperators[high(Foperators)].Identifer := Identifer;
  setlength(FOperatorList, high(FOperatorList) + 2);
  FOperatorList[high(FOperatorList)] := Identifer;
  // Suchen ob Operatoren "Gleich" sind
  For i := 0 To high(FOperators) - 1 Do Begin
    If FOperators[i].Identifer = Identifer Then Begin
      FOperators[i].Identifer := Identifer + InternalSymbol; // Der Andere ist der Unäre Operator, und muss geändert werden
      FOperatorList[i] := Identifer + InternalSymbol; // Der Andere ist der Unäre Operator, und muss geändert werden
      SetLength(FUnaryModifiierList, high(FUnaryModifiierList) + 2);
      FUnaryModifiierList[high(FUnaryModifiierList)].FromOP := Identifer;
      FUnaryModifiierList[high(FUnaryModifiierList)].ToOP := Identifer + InternalSymbol;
    End;
  End;
End;

{$IFDEF Beidseitig}

Procedure TGenMathCalc.AddUnOP_Right(Identifer: String; Proc: TUnProc);
Var
  i: Integer;
Begin
  If pos(InternalSymbol, Identifer) <> 0 Then Begin
    Raise exception.create('Error "' + InternalSymbol + '" not allowed in Identifer.');
  End;
  Identifer := lowercase(Identifer);
  If Length(Identifer) = 0 Then
    Raise Exception.create('Error Identiferlength has to be grater 0.');
  For i := 0 To High(Foperators) Do
    //If Foperators[i].Identifer = Identifer Then
    If (Foperators[i].Identifer = Identifer) And assigned(Foperators[i].UnProc) Then
      Raise Exception.create('Error double used Identifer.');
  setlength(Foperators, high(Foperators) + 2);
  //  Foperators[high(Foperators)].Unaer := True;
  Foperators[high(Foperators)].BinProc := Nil;
  Foperators[high(Foperators)].SBinProc := Nil;
  Foperators[high(Foperators)].UnProc_left := Nil;
  Foperators[high(Foperators)].UnProc_Right := Proc;
  Foperators[high(Foperators)].Identifer := Identifer;
  setlength(FOperatorList, high(FOperatorList) + 2);
  FOperatorList[high(FOperatorList)] := Identifer;
  // Suchen ob Operatoren "Gleich" sind
  For i := 0 To high(FOperators) - 1 Do Begin
    If FOperators[i].Identifer = Identifer Then Begin
      SetLength(FUnaryModifiierList, high(FUnaryModifiierList) + 2);
      FUnaryModifiierList[high(FUnaryModifiierList)].FromOP := Identifer;
      FUnaryModifiierList[high(FUnaryModifiierList)].ToOP := Identifer + InternalSymbol;
      FOperators[high(FOperators)].Identifer := Identifer + InternalSymbol; // Wir sind der doppelte Eintrag und müssen uns anpassen
      FOperatorList[high(FOperators)] := Identifer + InternalSymbol; // Der Andere ist der Unäre Operator, und muss geändert werden
    End;
  End;
End;

Procedure TGenMathCalc.AddUnOP_Left(Identifer: String; Proc: TUnProc);
{$ELSE}

Procedure TGenMathCalc.AddUnOP(Identifer: String; Proc: TUnProc);
{$ENDIF}
Var
  i: Integer;
Begin
  If pos(InternalSymbol, Identifer) <> 0 Then Begin
    Raise exception.create('Error "' + InternalSymbol + '" not allowed in Identifer.');
  End;
  Identifer := lowercase(Identifer);
  If Length(Identifer) = 0 Then
    Raise Exception.create('Error Identiferlength has to be grater 0.');
  For i := 0 To High(Foperators) Do
    //If Foperators[i].Identifer = Identifer Then
    If (Foperators[i].Identifer = Identifer) And assigned(Foperators[i].UnProc) Then
      Raise Exception.create('Error double used Identifer.');
  setlength(Foperators, high(Foperators) + 2);
  //  Foperators[high(Foperators)].Unaer := True;
  Foperators[high(Foperators)].BinProc := Nil;
  Foperators[high(Foperators)].SBinProc := Nil;
{$IFDEF Beidseitig}
  Foperators[high(Foperators)].UnProc_left := Proc;
  Foperators[high(Foperators)].UnProc_Right := Nil;
{$ELSE}
  Foperators[high(Foperators)].UnProc := Proc;
{$ENDIF}
  Foperators[high(Foperators)].Identifer := Identifer;
  setlength(FOperatorList, high(FOperatorList) + 2);
  FOperatorList[high(FOperatorList)] := Identifer;
  // Suchen ob Operatoren "Gleich" sind
  For i := 0 To high(FOperators) - 1 Do Begin
    If FOperators[i].Identifer = Identifer Then Begin
      SetLength(FUnaryModifiierList, high(FUnaryModifiierList) + 2);
      FUnaryModifiierList[high(FUnaryModifiierList)].FromOP := Identifer;
      FUnaryModifiierList[high(FUnaryModifiierList)].ToOP := Identifer + InternalSymbol;
      FOperators[high(FOperators)].Identifer := Identifer + InternalSymbol; // Wir sind der doppelte Eintrag und müssen uns anpassen
      FOperatorList[high(FOperators)] := Identifer + InternalSymbol; // Der Andere ist der Unäre Operator, und muss geändert werden
    End;
  End;
End;

Procedure TGenMathCalc.FreeCalcTree(Value: PCalcTree);
Begin
  If assigned(Value) Then Begin
    If Value^.isValue Then Begin
      // Wenn Value eine evtl Globale Variable ist dann wird sie nicht mit OnFreeValue freigegeben
      If Not Value^.isVar Then Begin
        If assigned(OnFreeValue) Then
          OnFreeValue(Value^.Value)
        Else
          Raise Exception.create('Error no OnFreeValue Procedure set.');
      End;
    End
    Else Begin
      FreeCalcTree(value^.Left);
      FreeCalcTree(value^.Right);
    End;
    dispose(Value);
  End;
End;

Function TGenMathCalc.CheckKlammern(Tokenlist: TTokenarray): Boolean;
Var
  kl, i: Integer;
Begin
  kl := 0;
  For i := 0 To High(Tokenlist) Do Begin
    If Tokenlist[i].Value = '(' Then inc(kl);
    If Tokenlist[i].Value = ')' Then Begin
      dec(kl);
      If kl < 0 Then Begin
        result := False;
        exit;
      End;
    End;
  End;
  result := kl = 0;
  If high(Tokenlist) = -1 Then result := False;
End;

Function TGenMathCalc.Calc(Value: PCalcTree; Out Result_is_Value: Boolean): Pointer; // Rechnet einen PCalctree aus
Begin
  (*
  Dieser "Hack" ist leider Notwendig,
  da sonst im Fall , wenn der Baum aus nur einem einzigen Value besteht das ergebniss 2 Mal freigegeben werden würde.

  =>

  Wenn Result_is_Value = True, dann darf das Ergebniss nicht Frei gegeben werden !!!!

  *)
  If assigned(Value) Then Begin
    result := calcHelper(Value);
    Result_is_Value := Value^.isValue;
  End
  Else Begin
    result := Nil;
    Result_is_Value := true;
  End;
End;

Function TGenMathCalc.CalcHelper(Value: PCalcTree): Pointer;
Var
  l, r: Pointer;
  dummy: Boolean;
Begin
  If Value = Nil Then Begin
    //    Raise Exception.create('Error no CalcTree to solve.');
    Result := Nil;
    exit;
  End;
  If value^.isValue Then Begin // Wenn ein Ergebniss
    If value^.isCallback Then Begin
      result := TDynCreator(Value^.Value)(); // Hier Rufen wir die Callback zum erzeugen einer Neuen Variable mittels Callback auf
    End
    Else Begin
      Result := Value^.Value;
    End;
  End
  Else Begin
    // Binäre Operationen
    If assigned(fOperators[Value^.Operation].BinProc) Or
      assigned(fOperators[Value^.Operation].SBinProc) Then Begin
      // Die Short Circuit Variante.
      If assigned(fOperators[Value^.Operation].SBinProc) Then Begin
        l := CalcHelper(Value^.Left);
        dummy := true;
        result := fOperators[Value^.Operation].SBinProc(l, Nil, dummy);
        If dummy Then Begin
          dummy := false;
          r := CalcHelper(Value^.Right);
          If assigned(OnFreeValue) Then Begin // Das ergebnis schmeißen wir weg, es wird unten nochmal erzeugt
            OnFreeValue(result);
          End;
          result := fOperators[Value^.Operation].SBinProc(l, r, dummy);
          If assigned(OnFreeValue) Then Begin
            If Value^.Left^.Operation <> -1 Then
              OnFreeValue(l);
            If Value^.Right^.Operation <> -1 Then
              OnFreeValue(r);
          End
          Else
            Raise Exception.create('Error no OnFreeValue Procedure set.');
        End
        Else Begin
          If assigned(OnFreeValue) Then Begin
            If Value^.Left^.Operation <> -1 Then
              OnFreeValue(l);
            // Der Rechte Ast mus nicht Frei gegeben werden, da er ja nicht evaluiert wurde.
            // If Value^.Right^.Operation <> -1 Then
            //   OnFreeValue(r);
          End
          Else
            Raise Exception.create('Error no OnFreeValue Procedure set.');

        End;
      End
      Else Begin
        // Berechnen der Zwischenergebnisse
        l := CalcHelper(Value^.Left);
        r := CalcHelper(Value^.Right);
        result := fOperators[Value^.Operation].BinProc(l, r);
        // Freigeben der Zwischenergebnisse
        If assigned(OnFreeValue) Then Begin
          If Value^.Left^.Operation <> -1 Then
            OnFreeValue(l);
          If Value^.Right^.Operation <> -1 Then
            OnFreeValue(r);
        End
        Else
          Raise Exception.create('Error no OnFreeValue Procedure set.');

      End;
    End
    Else Begin
{$IFDEF Beidseitig}
      If assigned(fOperators[Value^.Operation].UnProc_left) Then Begin
        r := CalcHelper(Value^.Right);
        result := fOperators[Value^.Operation].UnProc_left(r);
        // Freigeben der Zwischenergebnisse
        If assigned(OnFreeValue) Then Begin
          If Value^.Right^.Operation <> -1 Then
            OnFreeValue(r);
        End
        Else
          Raise Exception.create('Error no OnFreeValue Procedure set.');
      End
      Else Begin
        l := CalcHelper(Value^.Left);
        result := fOperators[Value^.Operation].UnProc_Right(l);
        // Freigeben der Zwischenergebnisse
        If assigned(OnFreeValue) Then Begin
          If Value^.Left^.Operation <> -1 Then
            OnFreeValue(l);
        End
        Else
          Raise Exception.create('Error no OnFreeValue Procedure set.');
      End;
{$ELSE}
      r := CalcHelper(Value^.Right);
      result := fOperators[Value^.Operation].UnProc(r);
      // Freigeben der Zwischenergebnisse
      If assigned(OnFreeValue) Then Begin
        If Value^.Right^.Operation <> -1 Then
          OnFreeValue(r);
      End
      Else
        Raise Exception.create('Error no OnFreeValue Procedure set.');
{$ENDIF}
    End;
  End;
End;

Function TGenMathCalc.Parse(TokenList: TTokenarray;
  Varlist: TVarlist): PCalcTree;

  Function isvar(Value: String): Integer;
  Var
    i: integer;
  Begin
    result := -1;
    For i := 0 To high(Varlist) Do
      If (Varlist[i].Name = Value) And (Not Varlist[i].Callback) Then Begin
        result := i;
        exit;
      End;
  End;

  Function isOP(Value: String): Integer;
  Var
    i: integer;
  Begin
    result := -1;
    For i := 0 To high(FOperators) Do
      If FOperators[i].Identifer = Value Then Begin
        result := i;
        exit;
      End;
  End;

  Function isCallback(value: String): integer;
  Var
    i: integer;
  Begin
    result := -1;
    For i := 0 To high(Varlist) Do
      If (Varlist[i].Name = Value) And (Varlist[i].Callback) Then Begin
        result := i;
        exit;
      End;
  End;

  Function PosP(Substr: Integer; Const S: Array Of PCalcTree): Integer;
  Var
    i: Integer;
  Begin
{$IFDEF Beidseitig}
{$WARNING ACHTUNG bei Beidseitig Unären Operatoren muss die Suchrichtung der Bindungsrichtung der Operatoren angepasst werden !!}
{$ENDIF}
    result := -1;
    If assigned(FOperators[Substr].UnProc) Then Begin
      // Unäre Operanden müssen von Rechts nach Links geparst werden
      For i := High(s) Downto 0 Do Begin
        If (s[i]^.Operation = Substr) And Not (s[i]^.alreadyParsed) Then Begin
          result := i;
          break;
        End;
      End;
    End
    Else Begin
      // Bei Binären Operanden muss von Links nach Rechts geparst werden, sonst wäre 6 - 3 - 3 = 6
      For i := 0 To High(s) Do Begin
        If (s[i]^.Operation = Substr) And Not (s[i]^.alreadyParsed) Then Begin
          result := i;
          break;
        End;
      End;
    End;
  End;

  Function PosT(Substr: String): Integer;
  Var
    i: Integer;
  Begin
    For i := 0 To High(Tokenlist) Do
      If Tokenlist[i].Value = Substr Then Begin
        result := i;
        exit;
      End;
    result := -1;
  End;

Var
  i, j, k, m: Integer;
  l: TTokenarray;
  p: PCalctree;
  zwischenliste: Array Of PCalcTree;
  v: String;
Begin
  result := Nil;
  Zwischenliste := Nil;
  If High(Foperators) = -1 Then
    Raise Exception.create('Error no Operators are defined.');
  If Not CheckKlammern(Tokenlist) Then Begin // Schnell Check auf Konsistente Klammerung
    exit;
  End;
  inc(fRecDepth);
  // Initialisierung
  For i := 0 To High(Varlist) Do Begin
    Varlist[i].Name := Lowercase(Varlist[i].Name);
    If pos(InternalSymbol, Varlist[i].Name) <> 0 Then Begin
      Raise exception.create('Error "' + InternalSymbol + '" not allowed in Varlist.');
    End;
  End;
  // Die gültigen Token dürfen nur geprüft werden wenn die Routine aufgerufen werden sonst käme die AV
  If (fRecDepth = 1) Then Begin
    For i := 0 To high(TokenList) Do Begin
      If pos(InternalSymbol, TokenList[i].Value) <> 0 Then Begin
        Raise exception.create('Error "' + InternalSymbol + '" not allowed in tokenlist.');
      End;
    End;
  End;
  // Behandlung der Klammerung ..
  j := PosT(')');
  While j <> -1 Do Begin
    // Dadurch das wir oben ein Checkklammern haben wissen wir das hier die "(" gefunden wird !!!
    For k := j Downto 0 Do
      If Tokenlist[k].value = '(' Then Begin
        // Umbasteln so das die Tokenliste nun nur den innersten Klammerausdruck hat.
        l := Nil;
        setlength(l, j - k - 1);
        For m := 0 To High(l) Do
          l[m] := Tokenlist[k + m + 1];
        // Parsen des Klammerausdruckes
        p := Parse(l, varlist);
        // Und ersetzen der Klammer durch ihr Ergebniss
        TokenList[k].Value := InternalSymbol;
        TokenList[k].Line := PtrToPtrInt(p);
        // Löschen aller Tokens die die Klammer beinhaltete
        For m := k + 1 To High(Tokenlist) - High(l) - 2 Do
          Tokenlist[m] := Tokenlist[m + high(l) + 2];
        setlength(tokenlist, high(tokenlist) - high(l) - 1);
        setlength(l, 0);
        break;
      End;
    j := PosT(')');
  End;
  FixUnaryOperands(TokenList, FUnaryModifiierList, FOperatorList);
  // Umwandeln aller Variablen in Pointer, bzw umrechnen in unsere Kettenstrucktur
  setlength(Zwischenliste, high(Tokenlist) + 1);
  For i := 0 To High(Tokenlist) Do Begin
    v := Lowercase(Tokenlist[i].Value);
    // Da stand mal ein Klammerausdruck den Basteln wir nun einfach wieder in den Baum.
    If v = InternalSymbol Then Begin
      Zwischenliste[i] := PtrIntToPtr(Tokenlist[i].Line);
      continue;
    End;
    new(Zwischenliste[i]);
    Zwischenliste[i]^.left := Nil;
    Zwischenliste[i]^.Right := Nil;
    Zwischenliste[i]^.alreadyParsed := False;
    j := isOP(v);
    If j <> -1 Then Begin // Wenn wir einen Operanden gefunden haben
      Zwischenliste[i]^.Operation := j;
      Zwischenliste[i]^.Value := Nil;
      Zwischenliste[i]^.isvalue := false;
      Zwischenliste[i]^.isvar := false;
      Zwischenliste[i]^.isCallback := false;
      continue;
    End;
    Zwischenliste[i]^.alreadyParsed := true; // Alles Ab jetzt sind Konstanten oder Variablen (also Blätter im Baum) und sind damit bereits verarbeitet.
    j := isvar(v);
    If j <> -1 Then Begin // Wenn wir eine Variable gefunden haben
      Zwischenliste[i]^.Operation := -1;
      Zwischenliste[i]^.Value := Varlist[j].Value;
      Zwischenliste[i]^.isvalue := true;
      Zwischenliste[i]^.isvar := true;
      Zwischenliste[i]^.isCallback := false;
      Continue;
    End;
    j := isCallback(v);
    If j <> -1 Then Begin
      Zwischenliste[i]^.Operation := -1;
      //If Not (Varlist[j].Value Is TDynCreator) Then Begin // Das geht leider nicht :(
      //  raise exception.create('Error Var is not a Callback');
      //End;
      Zwischenliste[i]^.Value := Varlist[j].Value;
      Zwischenliste[i]^.isvalue := true;
      Zwischenliste[i]^.isvar := true;
      Zwischenliste[i]^.isCallback := true;
      continue;
    End;
    // Wenn unser Token eine ganz normale "Zahl" ist
    Zwischenliste[i]^.Operation := -1;
    Zwischenliste[i]^.isvalue := true;
    Zwischenliste[i]^.isvar := false;
    Zwischenliste[i]^.isCallback := false;
    If assigned(OnCreateValue) Then
      Zwischenliste[i]^.Value := OnCreateValue(Tokenlist[i].Value)
    Else
      Raise Exception.create('Error no OnCreateValue function set.');
  End;
  // Berechnen entsprechend der Bindungen
  For i := 0 To high(Foperators) Do Begin
    j := PosP(i, Zwischenliste);
    While j <> -1 Do Begin // So lange es den Operator gibt müssen wir ihn Parsen
      If assigned(Foperators[i].BinProc) Or assigned(Foperators[i].SBinProc) Then Begin // Für Binäre Operatoren
        If (j = high(Zwischenliste)) Or (j = 0) Then Begin // Der Binäre Operator kann nicht als letztes oder erstes Element stehen, da er nicht Links und Rechts schlucken kann..
          Raise Exception.Create('Error invalid formula.');
        End;
        Zwischenliste[j]^.Left := Zwischenliste[j - 1];
        Zwischenliste[j]^.Right := Zwischenliste[j + 1];
        Zwischenliste[j]^.alreadyParsed := True;
        If Not zwischenliste[j + 1]^.alreadyParsed Then Begin
          // Todo : Das hier ist ein selbstgemachtes Problem uns sollte bereinigt werden !!
          (*
           * siehe Fehlerbeschreibung unten.
           *)
          Raise Exception.create('TGenMathCalc : Internal error, could not evaluate all operands.');

        End;
        If Not zwischenliste[j - 1]^.alreadyParsed Then Begin
          // Todo : Das hier ist ein selbstgemachtes Problem uns sollte bereinigt werden !!
          (*
           * siehe Fehlerbeschreibung unten.
           *)
          Raise Exception.create('TGenMathCalc : Internal error, could not evaluate all operands.');
        End;
        Zwischenliste[j - 1] := Zwischenliste[j];
        For k := j To high(Zwischenliste) - 2 Do
          Zwischenliste[k] := Zwischenliste[k + 2];
        setlength(Zwischenliste, high(Zwischenliste) - 1);
      End
      Else Begin // Für Unäre Operatoren
{$IFDEF Beidseitig}
        If assigned(Foperators[i].unProc_left) Then Begin
          If j = high(Zwischenliste) Then
            Raise Exception.Create('Error invalid formula.');
          Zwischenliste[j]^.Right := Zwischenliste[j + 1];
          Zwischenliste[j]^.alreadyParsed := True;
          For k := j + 1 To high(Zwischenliste) - 1 Do
            Zwischenliste[k] := Zwischenliste[k + 1];
        End
        Else Begin
          If j = 0 Then
            Raise Exception.Create('Error invalid formula.');
          Zwischenliste[j]^.Left := Zwischenliste[j - 1];
          Zwischenliste[j]^.alreadyParsed := True;
          Zwischenliste[j - 1] := Zwischenliste[j];
          For k := j To high(Zwischenliste) - 1 Do
            Zwischenliste[k] := Zwischenliste[k + 1];
        End;
{$ELSE}
        If j = high(Zwischenliste) Then
          Raise Exception.Create('Error invalid formula.');
        Zwischenliste[j]^.Right := Zwischenliste[j + 1];

        If Not zwischenliste[j + 1]^.alreadyParsed Then Begin
          // Todo : Das hier ist ein selbstgemachtes Problem und sollte bereinigt werden !!
          (*
           * siehe Fehlerbeschreibung unten.
           *)
          Raise Exception.create('TGenMathCalc : Internal error, could not evaluate all operands.');
        End;
        Zwischenliste[j]^.alreadyParsed := True;
        For k := j + 1 To high(Zwischenliste) - 1 Do
          Zwischenliste[k] := Zwischenliste[k + 1];
{$ENDIF}
        setlength(Zwischenliste, high(Zwischenliste));
      End;
      j := PosP(i, Zwischenliste);
    End;
  End;
  dec(fRecDepth);
  (*
   * Wenn diese AV kommt, dann kann das z.B. Daran liegen, dass
   * ein UNärer Operator einen anderen Unären Operator gefressen hat, dieser
   * aber selbst noch nichts gefressen hat.
   * z.B. "- sin 1"
   *  hier muss die Bindungsstärke von - kleiner, der von sin sein !!
   * Allerdings kommt dann immer noch der Fehler wenn man schreibt
   *    "- sin - 1", hingegen "- sin (-1)" wird richtig aufgelöst
   *)
  If high(zwischenliste) <> 0 Then Begin
    Raise exception.create('TGenMathCalc : Internal error, could not evaluate all operands.');
  End;
  result := zwischenliste[0];
End;

Procedure TGenMathCalc.ClearOPs;
Begin
  Setlength(Foperators, 0);
  setlength(FOperatorList, 0);
  setlength(FUnaryModifiierList, 0);
End;

End.

