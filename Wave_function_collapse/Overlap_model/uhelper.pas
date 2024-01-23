(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Wave function collapse (Ovelap mode)                  *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit uhelper;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils;

Const
  Infinity: Integer = high(Integer);

Type
  TIntArray = Array Of Integer;
  TIntArrayArray = Array Of TIntArray;
  TPattern = TIntArrayArray;
  TPatternArray = Array Of TPattern;
  TPointArray = Array Of TPoint;

Function includes(Const Data: TIntArray; value: Integer): Boolean; overload;
Function includes(Const Data: TPointArray; value: TPoint): Boolean; overload;
Function includes(Const Data: TPatternArray; Const value: TPattern): Boolean; overload;

Procedure Push(Var data: TIntArray; value: Integer); overload;
Procedure Push(Var data: TPointArray; value: TPoint); overload;
Procedure Push(Var Data: TPatternArray; Const value: TPattern); overload;

Procedure Pop(Var Data: TIntArrayArray); // TODO: Eigentlich wäre das eine Function, die das "Pop" Element zurück gibt
Procedure Shift(Var Data: TIntArrayArray); // TODO: Eigentlich wäre das eine Function, die das "Shift" Element zurück gibt
Function IndexOf(Const data: TIntArray; value: Integer): integer;
Function Has(Const data: TIntArray; value: Integer): boolean;

Function flip1DArray(Const Data: TIntArrayArray): TIntArrayArray;
Function transpose2DArray(Const Data: TIntArrayArray): TIntArrayArray;
Function arrayIsEqual(a, b: TIntArrayArray): boolean;

Procedure Delete(Var data: TIntArray; value: Integer);
Function Splice(Var a: TPointArray): TPoint; // TODO: Normal this routine takes 3 Arguments arg2 = Startindex, arg3 = len

Implementation

Function includes(Const Data: TIntArray; value: Integer): Boolean; overload;
Var
  i: Integer;
Begin
  result := false;
  For i := 0 To high(data) Do Begin
    If data[i] = value Then Begin
      result := true;
      exit;
    End;
  End;
End;

Function includes(Const Data: TPointArray; value: TPoint): Boolean; overload;
Var
  i: Integer;
Begin
  result := false;
  For i := 0 To high(data) Do Begin
    If data[i] = value Then Begin
      result := true;
      exit;
    End;
  End;
End;

Function includes(Const Data: TPatternArray; Const value: TPattern): Boolean; overload;
Var
  i, j, x: Integer;
  valid: Boolean;
Begin
  result := false;
  For x := 0 To high(data) Do Begin
    valid := true;
    For i := 0 To high(value) Do Begin
      For j := 0 To high(value[i]) Do Begin
        If data[x][i, j] <> value[i, j] Then Begin
          valid := false;
          break;
        End;
      End;
      If Not valid Then break;
    End;
    If valid Then Begin
      result := true;
      exit;
    End;
  End;
End;

Procedure Push(Var data: TIntArray; value: Integer); overload;
Begin
  setlength(data, high(data) + 2);
  data[high(data)] := value;
End;

Procedure Push(Var data: TPointArray; value: TPoint); overload;
Begin
  setlength(data, high(data) + 2);
  data[high(data)] := value;
End;

Procedure Push(Var Data: TPatternArray; Const value: TPattern); overload;
//Var
//  i, j: Integer;
Begin
  setlength(data, high(data) + 2);
  setlength(data[high(data)], length(value), length(value[0]));
  data[high(data)] := value;
  //  For i := 0 To high(value) Do Begin
  //    For j := 0 To high(value[i]) Do Begin
  //      data[high(data)][i, j] := value[i, j];
  //    End;
  //  End;
End;

Procedure Pop(Var Data: TIntArrayArray); // TODO: Eigentlich wäre das eine Function, die das "Pop" Element zurück gibt
Begin
  setlength(data, high(data));
End;

Procedure Shift(Var Data: TIntArrayArray); // TODO: Eigentlich wäre das eine Function, die das "Shift" Element zurück gibt
Var
  i: Integer;
Begin
  For i := 1 To high(data) Do Begin
    data[i - 1] := data[i];
  End;
  setlength(Data, high(data));
End;

Function IndexOf(Const data: TIntArray; value: Integer): integer;
Var
  i: Integer;
Begin
  result := -1;
  For i := 0 To high(data) Do Begin
    If data[i] = value Then Begin
      result := i;
      exit;
    End;
  End;
End;

Function Has(Const data: TIntArray; value: Integer): boolean;
Var
  i: Integer;
Begin
  result := false;
  For i := 0 To high(data) Do Begin
    If data[i] = value Then Begin
      result := true;
      exit;
    End;
  End;
End;

Procedure Delete(Var data: TIntArray; value: Integer);
Var
  i, j: Integer;
Begin
  For i := 0 To high(data) Do Begin
    If data[i] = value Then Begin
      For j := i To high(data) - 1 Do Begin
        data[j] := data[j + 1];
      End;
      setlength(data, high(data));
      exit;
    End;
  End;
End;

Function flip1DArray(Const Data: TIntArrayArray): TIntArrayArray;
Var
  i, j: Integer;
Begin
  result := Nil;
  setlength(result, length(data));
  For i := 0 To high(data) Do Begin
    setlength(result[i], length(data[high(data) - i]));
    For j := 0 To high(data[high(data) - i]) Do Begin
      result[i, j] := data[high(data) - i, j];
    End;
  End;
End;

Function transpose2DArray(Const Data: TIntArrayArray): TIntArrayArray;
Var
  i, j: Integer;
Begin
  result := Nil;
  setlength(result, length(data[0]), length(data));
  For i := 0 To high(data[0]) Do Begin
    For j := 0 To high(data) Do Begin
      result[i, j] := data[j, i];
    End;
  End;
End;

Function arrayIsEqual(a, b: TIntArrayArray): boolean;
Var
  i, j: integer;
Begin
  // Checks if array a is equal to array b.
  // JS sucks with being sensible.
  result := true;
  For i := 0 To high(a) Do Begin
    For j := 0 To high(a[i]) Do Begin
      If a[i, j] <> b[i, j] Then Begin
        result := false;
        exit;
      End;
    End;
  End;
End;

Function Splice(Var a: TPointArray): TPoint;
Var
  i: Integer;
Begin
  result := a[0];
  For i := 1 To high(a) Do Begin
    a[i - 1] := a[i];
  End;
  setlength(a, high(a));
End;

End.

