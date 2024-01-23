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
Unit utile;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, uhelper, Graphics;

Type

  { TTile }

  TTile = Class
  private

    total_states, x, y: integer;
    _hasCollapsed: Boolean;
    Function getColorTableIndex: TColor;
  public
    Color: TColor;
    States: TIntArray; // Liste der noch "möglichen" Wellen auf diesem Feld

    Property ColorTableIndex: TColor read getColorTableIndex;
    Constructor Create(aStates: TIntArray; atotal_states, ax, ay: integer);

    Procedure Collapse();

    Function hasCollapsed: Boolean;
    Procedure SetHasCollapsed();
    Function getEntropy(): integer;
  End;

Implementation

{ TTile }

Constructor TTile.Create(aStates: TIntArray; atotal_states, ax, ay: integer);
Begin
  States := aStates;
  total_states := atotal_states;
  x := ax;
  y := ay;
  _hasCollapsed := false;
End;

Function TTile.getColorTableIndex: TColor;
Begin
  result := states[0];
End;

Procedure TTile.Collapse();
Var
  i, j: Integer;
Begin
  _hasCollapsed := true;
  // Picks a random state and makes it the only one in the list
  i := Random(length(States));
  j := States[i];
  setlength(States, 1);
  States[0] := j;
End;

Function TTile.hasCollapsed: Boolean;
Begin
  result := _hasCollapsed;
End;

Procedure TTile.SetHasCollapsed();
Begin
  _hasCollapsed := true;
End;

Function TTile.getEntropy: integer;
Begin
  // Returns infinity if the tile has collapsed and returns the
  // length of the states if the tile hasn't collapsed
  If (length(states) > 1) Then Begin
    result := length(states);
  End
  Else Begin
    _hasCollapsed := true;
    result := uhelper.Infinity;
  End;
End;

End.

