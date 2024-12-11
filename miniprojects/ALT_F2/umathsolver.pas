(******************************************************************************)
(* umathsolver.pas                                                 ??.??.???? *)
(*                                                                            *)
(* Version     : 0.09                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Can parse simple mathematical formulars and calculate the    *)
(*               corresponding result.                                        *)
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
(*               0.02 - Einfügen Operanden "<<" und ">>"                      *)
(*               0.03 - Einfügen Operand "mod"                                *)
(*               0.04 - Decimalseparator raus geführt                         *)
(*               0.05 - Trunc, Ceil, Round,  <, <=, >, >=, Max, Min           *)
(*               0.06 - =, <>                                                 *)
(*               0.07 - Formatieren von Binärzahlen Nibble weise              *)
(*               0.08 - trunc, floor repariert                                *)
(*               0.09 - a^b für a < 0 und b ganzzahlig                        *)
(*                                                                            *)
(******************************************************************************)

Unit umathsolver;

{$MODE objfpc}{$H+}

(*
 * Liste der Zahldefinitionen:
 *  123   = Dezimalzahl
 *  0xAB  = Hexzahl
 *  %101  = Binärzahl
 *
 * Liste der Verfügbaren Operationen, Bindungsstärke Stark nach schwach:
 *
 * Unäre Operanden
 *      ln         = Logarithmus Naturalis
 *      abs        = Absolutwert
 *      sqrt       = Quadratwurzel
 *      sqr        = Quadrat
 *      sin        = Sinus
 *      cos        = Cosinus
 *      tan        = Tangens
 *      +          = Positive Zahl
 *      -          = Negative Zahl
 *      trunc      = Die nächst kleinere Integer Zahl
 *      floor      = Die nächst kleinere Integer Zahl
 *      ceil       = Die nächst größere Integer Zahl
 *      round      = Rundet zur nächsten Integer Zahl
 *
 * Binäre Operanden
 *      ^          = Exponenzieren
 *      shl,<<     = Shift left
 *      shr,>>     = Shift right
 *      /          = geteilt
 *      mod        = Modulo            (geht auch mit Float zahlen 5.5 mod 3 = 2.5)
 *      *          = Multiplikation
 *      -          = Subtraktion
 *      +          = Addition
 *      min        = kleinere der beiden Zahlen (Achtung schreibweise: 4 min 5)
 *      max        = größere der beiden Zahlen (Achtung schreibweise: 4 max 5)
 *      <          = Kleiner        => [0,1]
 *      <=         = Kleiner Gleich => [0,1]
 *      >          = Größer         => [0,1]
 *      >=         = Größer Gleich  => [0,1]
 *      =          = Gleich         => [0,1]
 *      <>         = Ungleich       => [0,1]
 *)

Interface

Uses
  Classes,
  SysUtils,
  utokenizer,
  (*
   * Damit der GenMathCalc funktioniert muss die .inc die folgenden Inhalt haben:
   *

   // Use if you want to use the Tokenizer from www.Corpsman.de
   {$DEFINE UseTokenizer}

   // Use OOP Callbacks for operands
   {.$DEFINE OOP_Callbacks}

   // Give the ability to render the calculation tree on a canvas
   {.$define ALLOW_RENDERTREE}

   // Use Binary operands left and right sided.
   {.$define Beidseitig}

   *)
  ugenmathcalc,
  mp_types, mp_real, mp_base;

Function EvalString(Value: String; DetailedIntFormats: Boolean = false): String;

(*
 * Der Kommatrenner zwischen Vor und Nachkomma, default "."
 *)
Function GetDecimalSeparator(): Char;
Procedure SetDecimalSeparator(value: Char);

Implementation

Uses strutils;

Var
  tok: TTokenizer;
  calc: TGenMathCalc;
  Result_is_Float: Boolean;

Function OnCreate(Value: String): Pointer;
Var
  res: pmp_float;
Begin
  If (pos(':', value) <> 0) Or (Trim(value) = '') Then Begin
    Raise exception.create('Fehler in Wert : ' + Value);
  End;
  new(res);
  mpf_init(res^);
  If lowercase(value) = 'e' Then Begin
    mpf_set_exp1(res^);
    Result_is_Float := true;
  End
  Else Begin
    If lowercase(value) = 'pi' Then Begin
      mpf_set_pi(res^);
      Result_is_Float := true;
    End
    Else Begin
      If pos('0x', lowercase(value)) = 1 Then Begin
        mpf_read_hex(res^, pchar(copy(value, 3, length(value))));
      End
      Else Begin
        If pos('%', lowercase(value)) = 1 Then Begin
          mpf_read_radix(res^, pchar(copy(value, 2, length(value))), 2);
        End
        Else Begin
          mpf_read_decimal(res^, pchar(value));
          If pos(mp_fract_sep, value) <> 0 Then Begin
            Result_is_Float := true;
          End;
        End;
      End;
    End;
  End;
  result := res;
End;

Procedure OnFree(Value: Pointer);
Var
  res: pmp_float;
Begin
  res := Value;
  mpf_clear(res^);
  dispose(res);
End;

Function Add_Float(v1, v2: Pointer): Pointer;
Var
  res: pmp_float;
Begin
  new(res);
  mpf_init(res^);
  mpf_add(pmp_float(v1)^, pmp_float(v2)^, res^);
  result := res;
End;

Function Min_Float(v1, v2: Pointer): Pointer;
Var
  res: pmp_float;
Begin
  new(res);
  mpf_init(res^);
  If mpf_is_lt(pmp_float(v1)^, pmp_float(v2)^) Then Begin
    mpf_copy(pmp_float(v1)^, res^);
  End
  Else Begin
    mpf_copy(pmp_float(v2)^, res^);
  End;
  result := res;
End;

Function Max_Float(v1, v2: Pointer): Pointer;
Var
  res: pmp_float;
Begin
  new(res);
  mpf_init(res^);
  If mpf_is_gt(pmp_float(v1)^, pmp_float(v2)^) Then Begin
    mpf_copy(pmp_float(v1)^, res^);
  End
  Else Begin
    mpf_copy(pmp_float(v2)^, res^);
  End;
  result := res;
End;

Function lt_Float(v1, v2: Pointer): Pointer;
Var
  res: pmp_float;
Begin
  new(res);
  mpf_init(res^);
  If mpf_is_lt(pmp_float(v1)^, pmp_float(v2)^) Then Begin
    mpf_read_decimal(res^, '1');
  End
  Else Begin
    mpf_read_decimal(res^, '0');
  End;
  Result_is_Float := false;
  result := res;
End;

Function gt_Float(v1, v2: Pointer): Pointer;
Var
  res: pmp_float;
Begin
  new(res);
  mpf_init(res^);
  If mpf_is_gt(pmp_float(v1)^, pmp_float(v2)^) Then Begin
    mpf_read_decimal(res^, '1');
  End
  Else Begin
    mpf_read_decimal(res^, '0');
  End;
  Result_is_Float := false;
  result := res;
End;

Function le_Float(v1, v2: Pointer): Pointer;
Var
  res: pmp_float;
Begin
  new(res);
  mpf_init(res^);
  If mpf_is_le(pmp_float(v1)^, pmp_float(v2)^) Then Begin
    mpf_read_decimal(res^, '1');
  End
  Else Begin
    mpf_read_decimal(res^, '0');
  End;
  Result_is_Float := false;
  result := res;
End;

Function ge_Float(v1, v2: Pointer): Pointer;
Var
  res: pmp_float;
Begin
  new(res);
  mpf_init(res^);
  If mpf_is_ge(pmp_float(v1)^, pmp_float(v2)^) Then Begin
    mpf_read_decimal(res^, '1');
  End
  Else Begin
    mpf_read_decimal(res^, '0');
  End;
  Result_is_Float := false;
  result := res;
End;

Function Equal_Float(v1, v2: Pointer): Pointer;
Var
  res: pmp_float;
Begin
  new(res);
  mpf_init(res^);
  If mpf_is_eq(pmp_float(v1)^, pmp_float(v2)^) Then Begin
    mpf_read_decimal(res^, '1');
  End
  Else Begin
    mpf_read_decimal(res^, '0');
  End;
  Result_is_Float := false;
  result := res;
End;

Function NEqual_Float(v1, v2: Pointer): Pointer;
Var
  res: pmp_float;
Begin
  new(res);
  mpf_init(res^);
  If mpf_is_eq(pmp_float(v1)^, pmp_float(v2)^) Then Begin
    mpf_read_decimal(res^, '0');
  End
  Else Begin
    mpf_read_decimal(res^, '1');
  End;
  Result_is_Float := false;
  result := res;
End;

Function Add_Floatu(v1: Pointer): Pointer;
Var
  res: pmp_float;
Begin
  new(res);
  mpf_init(res^);
  mpf_copy(pmp_float(v1)^, res^);
  result := res;
End;

Function Abs_Float(v1: Pointer): Pointer;
Var
  res: pmp_float;
Begin
  new(res);
  mpf_init(res^);
  mpf_abs(pmp_float(v1)^, res^);
  result := res;
End;

Function sqr_Float(v1: Pointer): Pointer;
Var
  res: pmp_float;
Begin
  new(res);
  mpf_init(res^);
  mpf_sqr(pmp_float(v1)^, res^);
  result := res;
End;

Function sqrt_Float(v1: Pointer): Pointer;
Var
  res: pmp_float;
Begin
  new(res);
  mpf_init(res^);
  mpf_sqrt(pmp_float(v1)^, res^);
  result := res;
  Result_is_Float := true;
End;

Function sub_Floatu(v1: Pointer): Pointer;
Var
  res: pmp_float;
  tmp: mp_float;
Begin
  new(res);
  mpf_init2(res^, tmp);
  mpf_read_decimal(tmp, '-1');
  mpf_mul(tmp, pmp_float(v1)^, res^);
  mpf_clear(tmp);
  result := res;
End;

Function trunc_Float(v1: Pointer): Pointer;
Var
  res: pmp_float;
  tmp: mp_int;
Begin
  new(res);
  mp_init(tmp);
  mpf_init(res^);
  mpf_trunc(pmp_float(v1)^, tmp);
  mpf_set_mpi(res^, tmp);
  mp_clear(tmp);
  result := res;
  Result_is_Float := false;
End;

Function Ceil_Float(v1: Pointer): Pointer;
Var
  res: pmp_float;
  tmpf: mp_float;
  tmp: mp_int;
Begin
  new(res);
  mp_init(tmp);
  mpf_init2(res^, tmpf);
  mpf_trunc(pmp_float(v1)^, tmp);
  (* Wenn die Quellzahl bereits eine Integer Zahl ist, dann darf nichts passieren *)
  mpf_frac(pmp_float(v1)^, tmpf);
  If Not mpf_is0(tmpf) Then Begin
    mp_add_int(tmp, 1, tmp);
  End;
  mpf_set_mpi(res^, tmp);
  mpf_clear(tmpf);
  mp_clear(tmp);
  result := res;
  Result_is_Float := false;
End;

Function Round_Float(v1: Pointer): Pointer;
Var
  res: pmp_float;
  tmp: mp_int;
Begin
  new(res);
  mp_init(tmp);
  mpf_init(res^);
  mpf_round(pmp_float(v1)^, tmp);
  mpf_set_mpi(res^, tmp);
  mp_clear(tmp);
  result := res;
  Result_is_Float := false;
End;

Function shift_left_Float(v1, v2: Pointer): Pointer;
Var
  mull: mp_float;
  res: pmp_float;
Begin
  new(res);
  mpf_init(res^);
  mpf_init(mull);
  mpf_set_int(mull, 2);
  mpf_expt(mull, pmp_float(v2)^, mull);
  mpf_mul(pmp_float(v1)^, mull, res^);
  mpf_clear(mull);
  result := res;
End;

Function shift_right_Float(v1, v2: Pointer): Pointer;
Var
  mull: mp_float;
  res: pmp_float;
Begin
  new(res);
  mpf_init(res^);
  mpf_init(mull);
  mpf_set_int(mull, 2);
  mpf_expt(mull, pmp_float(v2)^, mull);
  mpf_div(pmp_float(v1)^, mull, res^);
  mpf_clear(mull);
  result := res;
End;

Function sub_Float(v1, v2: Pointer): Pointer;
Var
  res: pmp_float;
Begin
  new(res);
  mpf_init(res^);
  mpf_sub(pmp_float(v1)^, pmp_float(v2)^, res^);
  result := res;
End;

Function sin_Float(v1: Pointer): Pointer;
Var
  res: pmp_float;
Begin
  new(res);
  mpf_init(res^);
  mpf_sin(pmp_float(v1)^, res^);
  result := res;
  Result_is_Float := true;
End;

Function cos_Float(v1: Pointer): Pointer;
Var
  res: pmp_float;
Begin
  new(res);
  mpf_init(res^);
  mpf_cos(pmp_float(v1)^, res^);
  result := res;
  Result_is_Float := true;
End;

Function tan_Float(v1: Pointer): Pointer;
Var
  res: pmp_float;
Begin
  new(res);
  mpf_init(res^);
  mpf_tan(pmp_float(v1)^, res^);
  result := res;
  Result_is_Float := true;
End;

Function tanh_Float(v1: Pointer): Pointer;
Var
  res: pmp_float;
Begin
  new(res);
  mpf_init(res^);
  mpf_tanh(pmp_float(v1)^, res^);
  result := res;
  Result_is_Float := true;
End;

Function div_Float(v1, v2: Pointer): Pointer;
Var
  res: pmp_float;
Begin
  new(res);
  mpf_init(res^);
  If Not mpf_is0(pmp_float(v2)^) Then Begin
    mpf_div(pmp_float(v1)^, pmp_float(v2)^, res^);
    Result_is_Float := true;
  End
  Else Begin
    mpf_set0(res^);
  End;
  result := res;
End;

Function mod_Float(v1, v2: Pointer): Pointer;
Var
  ai: mp_int;
  a, b: mp_float;
  res: pmp_float;
Begin
  new(res);
  mpf_init(res^);
  mpf_init2(a, b);
  mp_init(ai);
  If Not mpf_is0(pmp_float(v2)^) Then Begin
    //res := v1 - trunc(v1 / v2) * v2
    mpf_div(pmp_float(v1)^, pmp_float(v2)^, a);
    mpf_trunc(a, ai);
    mpf_mul_mpi(pmp_float(v2)^, ai, b);
    mpf_sub(pmp_float(v1)^, b, res^);
    mpf_clear2(a, b);
    mp_clear(ai);
  End
  Else Begin
    mpf_set0(res^);
  End;
  // Result_is_Float := false; 5.5 mod 3 = 2.5 => Float auch möglich !
  result := res;
End;

Function Pow_Float(v1, v2: Pointer): Pointer;
Var
  tmp, tmp2: mp_float;
  tmpi: mp_int;
  res: pmp_float;
Begin
  (*
   * Für a^b gibt es verschiendene Fälle
   *
   * a > 0: res := a^b;
   *
   * a = 0: res := 0;
   *
   * a < 0:
   *     if ganzzahlig(b) then begin
   *       if b mod 2 = 0 then begin
   *         res := abs(a)^b;
   *       end else begin
   *         res := -(abs(a)^b);
   *       end;
   *     end else begin
   *       res := 0;
   *     end;
   *
   *)
  new(res);
  mpf_init(res^);
  If s_mpf_is0(pmp_float(v1)^) Then Begin
    mpf_set0(res^);
  End
  Else Begin
    If s_mpf_is_ge0(pmp_float(v1)^) Then Begin
      mpf_expt(pmp_float(v1)^, pmp_float(v2)^, res^);
    End
    Else Begin
      mpf_init(tmp2);
      mpf_int(pmp_float(v2)^, tmp2);
      If mpf_is_eq(pmp_float(v2)^, tmp2) Then Begin
        mpf_init(tmp);
        mpf_abs(pmp_float(v1)^, tmp);
        mpf_expt(tmp, pmp_float(v2)^, res^);
        mp_init(tmpi);
        mpf_trunc(tmp2, tmpi);
        If mp_isodd(tmpi) Then Begin
          mpf_chs(res^, res^);
        End;
        mp_clear(tmpi);
        mpf_clear(tmp);
      End
      Else Begin
        mpf_set0(res^);
      End;
      mpf_clear(tmp2);
    End;
  End;
  result := res;
End;

Function Mul_Float(v1, v2: Pointer): Pointer;
Var
  res: pmp_float;
Begin
  new(res);
  mpf_init(res^);
  mpf_mul(pmp_float(v1)^, pmp_float(v2)^, res^);
  result := res;
End;

Function Ln_Float(v1: Pointer): Pointer;
Var
  tmp: mp_float;
  res: pmp_float;
Begin
  new(res);
  mpf_init2(res^, tmp);
  mpf_set0(tmp);
  If mpf_is_gt(pmp_float(v1)^, tmp) Then Begin
    mpf_ln(pmp_float(v1)^, pmp_float(res)^);
  End
  Else Begin
    mpf_set0(res^);
  End;
  mpf_clear(tmp);
  result := res;
  Result_is_Float := true;
End;

Function EvalString(Value: String; DetailedIntFormats: Boolean): String;
  Function PrettyHex(V: int64): String;
  Var
    tmp: String;
    dist: String;
  Begin
    result := format('%0.16X', [v]);
    dist := ' '; // " " wenn die Nibble separiert werden sollen, sonst ""
    If v >= 0 Then Begin
      // Führende 0en löschen
      While (length(result) > 1) And (result[1] = '0') Do Begin
        delete(result, 1, 1);
      End;
      // Ausrichten an Byte, Word, Int, Int64 Grenzen
      Case length(result) Of
        1: result := '0' + Result;
        3: result := AddChar('0', result, 4);
        5..7: result := AddChar('0', result, 8);
        9..15: result := AddChar('0', result, 16);
      End;
    End
    Else Begin
      While (length(result) > 1) And (result[1] = 'F') Do Begin
        delete(result, 1, 1);
      End;
      // Ausrichten an Byte, Word, Int, Int64 Grenzen
      Case length(result) Of
        1: result := 'F' + Result;
        3: result := AddChar('F', result, 4);
        5..7: result := AddChar('F', result, 8);
        9..15: result := AddChar('F', result, 16);
      End;
    End;
    // Einfügen der "Nibble" Gruppierungen
    If length(result) > 4 Then Begin
      tmp := '';
      While result <> '' Do Begin
        tmp := tmp + dist + copy(result, 1, 4);
        delete(result, 1, 4);
      End;
      result := trim(tmp);
    End;
    result := '0x' + result;
  End;

  Function PrettyBool(V: int64): String;
  Var
    dist: String;
    tmp: String;
    i: Integer;
  Begin
    dist := ' '; // " " wenn die Nibble separiert werden sollen, sonst ""
    tmp := PrettyHex(v);
    result := '';
    For i := 3 To length(tmp) Do Begin
      Case tmp[i] Of
        '0': result := result + dist + '0000';
        '1': result := result + dist + '0001';
        '2': result := result + dist + '0010';
        '3': result := result + dist + '0011';
        '4': result := result + dist + '0100';
        '5': result := result + dist + '0101';
        '6': result := result + dist + '0110';
        '7': result := result + dist + '0111';
        '8': result := result + dist + '1000';
        '9': result := result + dist + '1001';
        'A': result := result + dist + '1010';
        'B': result := result + dist + '1011';
        'C': result := result + dist + '1100';
        'D': result := result + dist + '1101';
        'E': result := result + dist + '1110';
        'F': result := result + dist + '1111';
      End;
    End;
    result := '%' + trim(result);
  End;

Var
  tokens: TTokenarray;
  resstring: String;
  tree: PCalcTree;
  res: pmp_float;
  bool: Boolean;
  i64: Int64;
Begin
  result := 'could not evaluate.';
  Result_is_Float := false;
  set_mp_error(0); // Zurücksetzen evtl alter Fehler von früheren Auswertungen
  Try
    resstring := '';
    tokens := tok.Scan(Value);
    tree := calc.Parse(tokens, Nil);
    res := calc.Calc(tree, bool);
    If assigned(res) Then Begin
      If Result_is_Float Then Begin
        resstring := mpf_adecimal_alt(res^, 40);
      End
      Else Begin
        mpf_int(res^, res^);
        resstring := mpf_adecimal_alt(res^, 40);
        If DetailedIntFormats Then Begin
          Try
            i64 := StrToInt64(resstring);
            resstring := resstring + ' (' + PrettyHex(i64) + ' ' + PrettyBool(i64) + ')';
          Except

          End;
        End;
      End;
      If Not bool Then Begin
        mpf_clear(res^);
        dispose(res);
      End;
    End;
    calc.FreeCalcTree(tree);
  Except
    exit;
  End;
  result := resstring;
  If mp_error <> 0 Then Begin
    result := 'could not evaluate.';
  End;
End;

Function GetDecimalSeparator(): Char;
Begin
  result := mp_fract_sep;
End;

Procedure SetDecimalSeparator(value: Char);
Begin
  mp_fract_sep := value;
End;

Initialization

  tok := TTokenizer.Create;
  tok.AddSeperator(' ');
  tok.AddOperator('(');
  tok.AddOperator(')');
  tok.AddOperator('+');
  tok.AddOperator('*');
  tok.AddOperator('-');
  tok.AddOperator('/');
  tok.AddOperator('^');
  tok.AddOperator('<');
  tok.AddOperator('<=');
  tok.AddOperator('<<');
  tok.AddOperator('shl');
  tok.AddOperator('=');
  tok.AddOperator('>');
  tok.AddOperator('>=');
  tok.AddOperator('>>');
  tok.AddOperator('<>');
  tok.AddOperator('shr');
  tok.AddOperator('min');
  tok.AddOperator('max');
  tok.AddOperator('floor');
  tok.AddOperator('trunc');
  tok.AddOperator('ceil');
  tok.AddOperator('round');

  calc := TGenMathCalc.Create;
  calc.OnCreateValue := @OnCreate;
  calc.OnFreeValue := @OnFree;

  calc.AddUnOP('ln', @Ln_Float);
  calc.AddUnOP('abs', @abs_float);
  calc.AddUnOP('sqrt', @sqrt_float);
  calc.AddUnOP('sqr', @sqr_float);
  calc.AddUnOP('sin', @Sin_float);
  calc.AddUnOP('cos', @Cos_float);
  calc.AddUnOP('tan', @Tan_float);
  calc.AddUnOP('tanh', @Tanh_float);
  calc.AddUnOP('+', @Add_Floatu);
  calc.AddUnOP('-', @Sub_Floatu);
  calc.AddUnOP('floor', @trunc_Float);
  calc.AddUnOP('trunc', @trunc_Float);
  calc.AddUnOP('ceil', @Ceil_Float);
  calc.AddUnOP('round', @Round_Float);

  calc.AddBinOP('^', @Pow_Float);
  calc.AddBinOP('shl', @shift_left_Float);
  calc.AddBinOP('<<', @shift_left_Float);
  calc.AddBinOP('shr', @shift_right_Float);
  calc.AddBinOP('>>', @shift_right_Float);
  calc.AddBinOP('/', @Div_Float);
  calc.AddBinOP('mod', @Mod_Float);
  calc.AddBinOP('*', @Mul_Float);
  calc.AddBinOP('-', @Sub_Float);
  calc.AddBinOP('+', @Add_Float);
  calc.AddBinOP('min', @Min_Float);
  calc.AddBinOP('max', @Max_Float);
  calc.AddBinOP('<', @LT_Float);
  calc.AddBinOP('<=', @LE_Float);
  calc.AddBinOP('>', @GT_Float);
  calc.AddBinOP('>=', @GE_Float);
  calc.AddBinOP('=', @Equal_Float);
  calc.AddBinOP('<>', @NEqual_Float);

Finalization;
  tok.Free;
  calc.free;

End.

