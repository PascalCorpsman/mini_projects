(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Imageshop                                             *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit uoperator_helper;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, unodes;

Const
  White: TPixel = (r: 1; g: 1; b: 1; a: 1);
  Black: TPixel = (r: 0; g: 0; b: 0; a: 1);

  (*
   * Sellt sicher, dass ein Wert im Bereich [0..1] ist
   * Alles drüber bzw drunter wird auf den entsprechend letzten Gültigen Wert abgebildet.
   *)
Function Clamp(Value: Single): Single;

(*
 * Berechnet eine A*(1-Percent) + B*Percent => Interpolation zwischen 2 Pixeld
 *)
Function Mix(A, B: TPixel; Percent: Single): TPixel;

(*
 * Value in [0..1]
 * Result = Passende Farbe auf der Regenbogenscala
 *)
Function Hue(Value: Single): TPixel;

Implementation

Function Clamp(Value: Single): Single;
Begin
  If value > 1 Then Begin
    result := 1;
  End
  Else Begin
    If value < 0 Then Begin
      result := 0;
    End
    Else Begin
      result := value;
    End;
  End;
End;

Function Mix(A, B: TPixel; Percent: Single): TPixel;
Var
  Invert: Single;
Begin
  If Percent < 0.001 Then
    Result := A
  Else If Percent > 0.999 Then
    Result := B
  Else Begin
    Invert := 1 - Percent;
    Result.B := Clamp(B.B * Percent + A.B * Invert);
    Result.G := Clamp(B.G * Percent + A.G * Invert);
    Result.R := Clamp(B.R * Percent + A.R * Invert);
    Result.A := Clamp(B.A * Percent + A.A * Invert);
  End;
End;

Function Hue(Value: Single): TPixel;
Const
  Step = 1 / 6;
Var
  R, G, B: Single;
Begin
  R := 0;
  G := 0;
  B := 0;
  If Value < 0 Then
    R := 1
  Else If Value < 1 * Step Then Begin
    R := 1;
    G := Value / Step;
  End
  Else If Value < 2 * Step Then Begin
    R := 1 - (Value - 1 * Step) / Step;
    G := 1;
  End
  Else If Value < 3 * Step Then Begin
    G := 1;
    B := (Value - 2 * Step) / Step;
  End
  Else If Value < 4 * Step Then Begin
    G := 1 - (Value - 3 * Step) / Step;
    B := 1;
  End
  Else If Value < 5 * Step Then Begin
    B := 1;
    R := (Value - 4 * Step) / Step;
  End
  Else If Value < 6 * Step Then Begin
    B := 1 - (Value - 5 * Step) / Step;
    R := 1;
  End
  Else
    R := 1;
  Result.R := Clamp(R);
  Result.G := Clamp(G);
  Result.B := Clamp(B);
  Result.A := 1;
End;

End.

