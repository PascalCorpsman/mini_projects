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
Unit umatcher;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, uhelper;

Type
  { TMatcher }

  TMatcher = Class
  private
    patterns: Array Of Array[0..3] Of Array Of integer;
    pattLen: Integer;
  public
    Constructor Create(PatternCount: integer);
    Destructor Destroy(); override;

    Function tileCompatible(Const a_, b_: TPattern; direction: Integer): Boolean;
    Procedure AddPattern(Pattern, neighbor, direction: integer);
    Function match(pStates: TIntArray; neighbor_states: TIntArrayArray): TIntArray;
  End;

Implementation

{ TMatcher }

Constructor TMatcher.Create(PatternCount: integer);
Var
  i, j: Integer;
Begin
  setlength(patterns, PatternCount);
  For i := 0 To high(patterns) Do Begin
    For j := 0 To 3 Do Begin
      patterns[i][j] := Nil;
    End;
  End;
  pattLen := PatternCount;
End;

Destructor TMatcher.Destroy;
Begin
  Inherited Destroy;
End;

Function TMatcher.tileCompatible(Const a_, b_: TPattern; direction: Integer
  ): Boolean;
Var
  A, B: TPattern;
  i, j: Integer;
Begin
  a := Nil;
  b := Nil;
  setlength(a, length(a_), length(a_[0]));
  For i := 0 To high(a) Do Begin
    For j := 0 To high(a[i]) Do Begin
      a[i, j] := a_[i, j];
    End;
  End;
  setlength(b, length(b_), length(b_[0]));
  For i := 0 To high(b) Do Begin
    For j := 0 To high(b[i]) Do Begin
      b[i, j] := b_[i, j];
    End;
  End;

  // Check if the tile a overlaps b in a specified direction

  Case direction Of
    0: Begin // Checks the up direction
        pop(a);
        shift(b);
      End;
    1: Begin // Checks the left direction
        A := transpose2DArray(A);
        pop(a);
        B := transpose2DArray(B);
        shift(B);
      End;

    2: Begin // Checks the down direction
        shift(A);
        pop(B);
      End;

    3: Begin // Checks the right direction
        A := transpose2DArray(A);
        shift(A);
        B := transpose2DArray(B);
        pop(B);
      End;
  End;

  result := arrayIsEqual(A, B);
End;

Procedure TMatcher.AddPattern(Pattern, neighbor, direction: integer);
Begin
  If assigned(patterns[Pattern][direction]) Then Begin
    setlength(patterns[Pattern][direction], high(patterns[Pattern][direction]) + 2);
    patterns[Pattern][direction][high(patterns[Pattern][direction])] := neighbor;
  End
  Else Begin
    setlength(patterns[Pattern][direction], 1);
    patterns[Pattern][direction][0] := neighbor; // This made it work!
  End;
End;

Function TMatcher.match(pStates: TIntArray; neighbor_states: TIntArrayArray): TIntArray;
Var
  current_possibilities, possibilities: TIntArray;
  direction, oppositeDirection, i, state, j, k, elt: Integer;
Begin
  possibilities := Nil;
  setlength(possibilities, length(pStates));
  For i := 0 To high(pStates) Do Begin
    possibilities[i] := pStates[i];
  End;

  For direction := 0 To 4 - 1 Do Begin
    oppositeDirection := (direction + 2) Mod 4;
    current_possibilities := Nil;

    For j := 0 To high(neighbor_states[direction]) Do Begin
      state := neighbor_states[direction][j];
      For k := 0 To high(patterns[state][oppositeDirection]) Do Begin
        elt := patterns[state][oppositeDirection][k];
        setlength(current_possibilities, high(current_possibilities) + 2);
        current_possibilities[high(current_possibilities)] := elt;
      End;
    End;

    For i := 0 To length(patterns) - 1 Do Begin
      If Not has(current_possibilities, i) Then delete(possibilities, i);
    End;
  End;
  result := possibilities;
End;
End.

