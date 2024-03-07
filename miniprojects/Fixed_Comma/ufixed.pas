(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Fixed comma                                           *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit ufixed;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils;


(*
 * If you get a compiler error with missing file
 * just create a file namend "ufixed.inc" in your project folder and
 * insert the following content:
 *
 * ---------- Content of file ----------

 {.$DEFINE AllowAccesToInternalData} // Zugriff auf die interne Datenstructur

   ---------- End content of file ----------
 *)




{$I ufixed.inc}

Type

  { TFixedComma }

  TFixedComma = Class
  private
    fone: uint64;
    fvalue: uint64;
    fn: integer;
    fp: integer;
    fsigned: Boolean;
    Function getValue: Double;
    Procedure setValue(AValue: Double);
  public
{$IFDEF AllowAccesToInternalData}
    Property RaWValue: uint64 read fvalue write fvalue;
{$ENDIF}
    Property Value: Double read getValue write setValue;
    Constructor Create(N, P: integer; Signed: Boolean); virtual;
    Destructor Destroy; override;
  End;

Operator + (a, b: TFixedComma): TFixedComma;
Operator - (a, b: TFixedComma): TFixedComma;
Operator * (a, b: TFixedComma): TFixedComma;
Operator / (a, b: TFixedComma): TFixedComma;

Implementation

Procedure Check(a, b: TFixedComma);
Begin
  If (a.fp <> b.fp) Then Raise exception.create('Oparand with different comma position');
  If (a.fn <> b.fn) Then Raise exception.create('Oparand with different bit width');
  If (a.fsigned <> b.fsigned) Then Raise exception.create('Oparand with different signing');
End;

Operator + (a, b: TFixedComma): TFixedComma;
Begin
  Check(a, b);
  result := TFixedComma.Create(a.fn, a.fp, a.fsigned);
  result.fvalue := a.fvalue + b.fvalue;
  // Todo: Overflow detection => Clamt to Upper Border
  If result.fvalue >= (1 Shl result.fp) Then Begin
    result.fvalue := (1 Shl result.fp) - 1;
  End;
End;


Operator - (a, b: TFixedComma): TFixedComma;
Begin
  Check(a, b);
  result := TFixedComma.Create(a.fn, a.fp, a.fsigned);
  result.fvalue := a.fvalue - b.fvalue;
  // Todo: Underflow detection => Clamt to Upper Border
End;

Operator * (a, b: TFixedComma): TFixedComma;
Begin
  Check(a, b);
  result := TFixedComma.Create(a.fn, a.fp, a.fsigned);
  result.fvalue := a.fvalue * b.fvalue;
  result.fvalue := result.fvalue Shr (a.fp);
End;


Operator / (a, b: TFixedComma): TFixedComma;
Begin
  Check(a, b);
  result := TFixedComma.Create(a.fn, a.fp, a.fsigned);
  result.fvalue := a.fvalue Div b.fvalue;
  // Todo: Overflow detection => Clamt to Upper Border
End;


{ TFixedComma }

Function TFixedComma.getValue: Double;
Begin
  If fsigned Then Begin
    result := int64(fvalue) / fone;
  End
  Else Begin
    result := fvalue / fone;
  End;
End;

Procedure TFixedComma.setValue(AValue: Double);
Begin
  fvalue := trunc(AValue * fone);
End;

Constructor TFixedComma.Create(N, P: integer; Signed: Boolean);
Begin
  Inherited create;
  If (n Mod 8 <> 0) Or (n <= 0) Or (n > 32) Then Begin
    Raise Exception.Create('Invalid Bitnumber.');
  End;
  If (p + 1 > n) Then Begin
    Raise Exception.Create('Invalid comma position.');
  End;
  fn := n;
  If Signed Then Begin
    fp := p;
  End
  Else Begin
    fp := p + 1;
  End;
  fone := 1 Shl fp;
  fsigned := Signed;
  fvalue := 0;
End;

Destructor TFixedComma.Destroy;
Begin

End;

End.

