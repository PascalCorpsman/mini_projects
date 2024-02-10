(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of heapsim                                               *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
(*                                                                            *)
(* Description : Base implementation of a heap for benchmarking in heapsim.   *)
(*                                                                            *)
(******************************************************************************)
Unit ufirstfitheap;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, ufreelistheap;

Type

  { TFirstFitHeap }

  TFirstFitHeap = Class(TFreeListHeap)
  public
    Procedure ReCreate(); override; // Nur die Steuerung ob Freigegebene Blöcke hinten oder Vorne in die Freispeicherliste eingefügt werden
    (*
     * Strategie der Allokation :
     * Der erste Block in der Freispeicherliste der die Angeforderte Größe
     * bereitstellen kann wird genommen.
     *
     * Freie Blöcke werden Vorne angefügt.
     *)
    Function GetMem(size: ptruint): Pointer; override;
  End;

Implementation

Procedure TFirstFitHeap.ReCreate;
Begin
  Inherited ReCreate;
  FreeMode := fmInsertFirst;
  // FreeMode := fmInsertLast; // InsertFirst ist empirisch bestimmt einen Ticken schneller ca. 0.1-0.2 Sec
End;

Function TFirstFitHeap.GetMem(size: ptruint): Pointer;
Var
  p1: integer;
  i: ptruint;
Begin
  If size < 8 Then size := 8; // Die Freispeicherliste erzwingt eine Mindestgröße von 8 Byte
  Result := Inherited GetMem(size);
  // 1. Regulär suchen nach einem Groß genugen Block
  p1 := FListFirst;
  While p1 <> ptr_Nil Do Begin
    If Get_Len(p1) >= size Then Begin
      result := AllocateBlock(p1, size);
      exit;
    End;
    p1 := Get_Next(p1);
  End;
  // 2. Kein Groß genuger Block mehr vorhanden, durch ResortFreeList schaun ob nicht doch noch was geht.
  Repeat
    i := ResortFreeList();
    If i >= size Then Begin
      // Das ist zwar ein Rekursiver Aufruf, aber einer der Gewiss Terminiert, weil wir nun wissen, dass es einen Block gibt, der Size aufnehmen kann.
      result := GetMem(size);
      exit;
    End;
  Until i = 0;
  // 3. Nichts zu machen. Result bleibt Nil.
End;

End.

