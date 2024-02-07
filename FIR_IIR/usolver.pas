(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of FIR IIR                                               *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit usolver;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, ugenmathcalc, utokenizer, uncommenter, math;

Const
  // Minimal für Single ca. 6.0E-8
  EpsilonKugel = 1.0E-6; // Zwei Zahlen die sich um Weniger als EpsilonKugel unterscheiden werden als Gleich betrachtet

Type
  //  TDatatype = Single;
  TDatatype = double;

  PDatatype = ^TDatatype;

  { TSolver }

  TSolver = Class
  private
    ftokenizer: TTokenizer;
    fCalculator: TGenMathCalc;
    funcommenter: TUnCommenter;
    fFormula: PCalcTree;
    f_X: TDatatype;
    fVarlist: TVarlist;
  public
    Constructor create;
    Destructor destroy; override;
    Function Create_Formula(Value: String): Boolean;
    Function EvalFormula(X: TDatatype): TDatatype;
  End;

Var
  defFormat: TFormatSettings;

Implementation

{ TSolver }

Function Creater(Value: String): Pointer;
Var
  res: PDatatype;
Begin
  new(res);
  If lowercase(value) = 'pi' Then Begin
    res^ := Pi;
  End
  Else If lowercase(value) = 'e' Then Begin
    res^ := 2.7182818284590452353602874713526624977572470936999595749669;
  End
  Else
    res^ := StrToFloatDef(value, 0, defFormat);
  result := res;
End;

Procedure Freeer(Value: Pointer);
Var
  tmp: PDatatype;
Begin
  If assigned(value) Then Begin
    tmp := value;
    dispose(tmp);
  End;
End;

Function Mul_(Value1, Value2: Pointer): Pointer;
Var
  res: PDatatype;
Begin
  new(res);
  res^ := PDatatype(value1)^ * PDatatype(value2)^;
  result := res;
End;

Function Add_(Value1, Value2: Pointer): Pointer;
Var
  res: PDatatype;
Begin
  new(res);
  res^ := PDatatype(value1)^ + PDatatype(value2)^;
  result := res;
End;

Function Sub_(Value1, Value2: Pointer): Pointer;
Var
  res: PDatatype;
Begin
  new(res);
  res^ := PDatatype(value1)^ - PDatatype(value2)^;
  result := res;
End;

Function Sub_u(Value1: Pointer): Pointer;
Var
  res: PDatatype;
Begin
  new(res);
  res^ := -PDatatype(value1)^;
  result := res;
End;

Function Add_u(Value1: Pointer): Pointer;
Var
  res: PDatatype;
Begin
  new(res);
  res^ := PDatatype(value1)^;
  result := res;
End;

Function div_(Value1, Value2: Pointer): Pointer;
Var
  res: PDatatype;
Begin
  new(res);
  If PDatatype(value2)^ <> 0 Then
    res^ := PDatatype(value1)^ / PDatatype(value2)^
  Else
    res^ := 0;
  result := res;
End;

Function Power_(Value1, Value2: Pointer): Pointer;
Var
  res: PDatatype;
Begin
  new(res);
  res^ := Power(PDatatype(value1)^, PDatatype(value2)^);
  result := res;
End;

Function Min_(Value1, Value2: Pointer): Pointer;
Var
  res: PDatatype;
Begin
  new(res);
  res^ := min(PDatatype(value1)^, PDatatype(value2)^);
  result := res;
End;

Function Max_(Value1, Value2: Pointer): Pointer;
Var
  res: PDatatype;
Begin
  new(res);
  res^ := Max(PDatatype(value1)^, PDatatype(value2)^);
  result := res;
End;

Function isssame(v1, v2: TDatatype): boolean;
Begin
  result := abs(v1 - v2) <= EpsilonKugel;
End;

Function Eq_(Value1, Value2: Pointer): Pointer;
Var
  res: PDatatype;
Begin
  new(res);
  If isssame(PDatatype(value1)^, PDatatype(value2)^) Then
    res^ := 1
  Else
    res^ := 0;
  result := res;
End;

Function nEq_(Value1, Value2: Pointer): Pointer;
Var
  res: PDatatype;
Begin
  new(res);
  If isssame(PDatatype(value1)^, PDatatype(value2)^) Then
    res^ := 0
  Else
    res^ := 1;
  result := res;
End;

Function lt_(Value1, Value2: Pointer): Pointer;
Var
  res: PDatatype;
Begin
  new(res);
  If PDatatype(value1)^ < PDatatype(value2)^ Then
    res^ := 1
  Else
    res^ := 0;
  result := res;
End;

Function lteq_(Value1, Value2: Pointer): Pointer;
Var
  res: PDatatype;
Begin
  new(res);
  If PDatatype(value1)^ <= PDatatype(value2)^ Then
    res^ := 1
  Else
    res^ := 0;
  result := res;
End;

Function gt_(Value1, Value2: Pointer): Pointer;
Var
  res: PDatatype;
Begin
  new(res);
  If PDatatype(value1)^ > PDatatype(value2)^ Then
    res^ := 1
  Else
    res^ := 0;
  result := res;
End;

Function gteq_(Value1, Value2: Pointer): Pointer;
Var
  res: PDatatype;
Begin
  new(res);
  If PDatatype(value1)^ >= PDatatype(value2)^ Then
    res^ := 1
  Else
    res^ := 0;
  result := res;
End;

Function abs_(Value1: Pointer): Pointer;
Var
  res: PDatatype;
Begin
  new(res);
  res^ := abs(PDatatype(value1)^);
  result := res;
End;

Function sin_(Value1: Pointer): Pointer;
Var
  res: PDatatype;
Begin
  new(res);
  res^ := sin(PDatatype(value1)^);
  result := res;
End;

Function cos_(Value1: Pointer): Pointer;
Var
  res: PDatatype;
Begin
  new(res);
  res^ := cos(PDatatype(value1)^);
  result := res;
End;

Function sqrt_(Value1: Pointer): Pointer;
Var
  res: PDatatype;
Begin
  new(res);
  If (PDatatype(value1)^ < 0) Then Begin
    res^ := 0;
  End
  Else Begin
    res^ := sqrt(PDatatype(value1)^);
  End;
  result := res;
End;

Function sqr_(Value1: Pointer): Pointer;
Var
  res: PDatatype;
Begin
  new(res);
  res^ := sqr(PDatatype(value1)^);
  result := res;
End;

Constructor TSolver.create;
Begin
  Inherited create;
  fFormula := Nil;
  setlength(fVarlist, 1);
  fVarlist[0].Name := 'x';
  fVarlist[0].Value := @f_x;
  // Der Unkommenter
  funcommenter := TUnCommenter.Create;
  funcommenter.CaseSensitive := false;
  funcommenter.AddRule('//', '', true);
  funcommenter.AddRule('{', '}', false);
  funcommenter.DellEmptyLines := true;
  funcommenter.NumberLines := false;
  // Tokenizer
  ftokenizer := TTokenizer.Create;
  ftokenizer.CaseSensitive := false;
  ftokenizer.AddSeperator(' '); // Leerzeichen
  ftokenizer.AddSeperator(#13); // Cr
  ftokenizer.AddSeperator(#10); // Rt
  ftokenizer.AddSeperator(#9); // Tab
  ftokenizer.AddOperator('(');
  ftokenizer.AddOperator(')');
  ftokenizer.AddOperator('abs');
  ftokenizer.AddOperator('sqrt');
  ftokenizer.AddOperator('sqr');
  ftokenizer.AddOperator('sin');
  ftokenizer.AddOperator('cos');
  ftokenizer.AddOperator('min');
  ftokenizer.AddOperator('=');
  ftokenizer.AddOperator('<>');
  ftokenizer.AddOperator('<');
  ftokenizer.AddOperator('<=');
  ftokenizer.AddOperator('>');
  ftokenizer.AddOperator('>=');
  //  ftokenizer.AddOperator('tan');
  ftokenizer.AddOperator('^');
  ftokenizer.AddOperator('/');
  ftokenizer.AddOperator('*');
  ftokenizer.AddOperator('-');
  ftokenizer.AddOperator('+');
  //  ftokenizer.AddOperator('Pi');
  //  ftokenizer.AddOperator('x');
  //  ftokenizer.AddOperator('e');
  // Solver
  fCalculator := TGenMathCalc.Create;
  fCalculator.OnCreateValue := @Creater;
  fCalculator.OnFreeValue := @Freeer;

  fCalculator.AddUnOP('-', @Sub_u);
  fCalculator.AddUnOP('+', @Add_u);
  fCalculator.AddUnOP('abs', @Abs_);
  fCalculator.AddUnOP('sqrt', @Sqrt_);
  fCalculator.AddUnOP('sqr', @Sqr_);
  fCalculator.AddUnOP('sin', @Sin_);
  fCalculator.AddUnOP('cos', @Cos_);
  fCalculator.AddBinOP('^', @Power_);
  fCalculator.AddBinOP('/', @Div_);
  fCalculator.AddBinOP('*', @Mul_);
  fCalculator.AddBinOP('-', @Sub_);
  fCalculator.AddBinOP('+', @Add_);
  fCalculator.AddBinOP('max', @max_);
  fCalculator.AddBinOP('min', @min_);
  fCalculator.AddBinOP('=', @eq_);
  fCalculator.AddBinOP('<>', @neq_);
  fCalculator.AddBinOP('<', @lt_);
  fCalculator.AddBinOP('<=', @lteq_);
  fCalculator.AddBinOP('>', @gt_);
  fCalculator.AddBinOP('>=', @gteq_);
End;

Destructor TSolver.destroy;
Begin
  If Assigned(fFormula) Then Begin
    fCalculator.FreeCalcTree(FFormula);
    FFormula := Nil;
  End;
  setlength(fVarlist, 0);
  ftokenizer.free;
  fCalculator.free;
  funcommenter.free;
End;

Function TSolver.Create_Formula(Value: String): Boolean;
Var
  tokens: TTokenarray;
Begin
  Try
    // Freigeben alter Daten
    If Assigned(fFormula) Then Begin
      fCalculator.FreeCalcTree(FFormula);
      FFormula := Nil;
    End;
    // Entfernen aller Kommentare
    value := funcommenter.Uncomment(value);
    // Tokenliste Erzeugen
    tokens := ftokenizer.Scan(value + '    '); // Workaround um Bug in TTokenzier
    // Abschneiden des letzten evtl. Falschen Tokens
    If high(tokens) <> 0 Then Begin
      If trim(tokens[high(tokens)].Value) = '' Then Begin
        setlength(tokens, high(tokens));
      End;
    End;
    // Rechenbaum Erstellen
    fFormula := fCalculator.Parse(tokens, fVarlist);
    // Freigeben temporärer daten
    setlength(tokens, 0);
    result := true;
  Except
    fFormula := Nil; // Wenn was nicht stimmt
    result := false;
  End;
End;

Function TSolver.EvalFormula(X: TDatatype): TDatatype;
Var
  Freeerg: Boolean;
  erg: PDatatype;
Begin
  If assigned(fFormula) Then Begin
    // Übernehmen von X
    f_X := x;
    // Berechnen des Ergebnisses
    erg := fCalculator.Calc(fFormula, freeerg);
    // Übernehmen des Ergebnisses
    If assigned(erg) Then Begin
      result := erg^;
    End
    Else
      result := 0;
    // Freigeben falls notwendig
    freeerg := Not freeerg;
    If Freeerg Then dispose(erg);
  End
  Else
    result := 0;
End;

End.

