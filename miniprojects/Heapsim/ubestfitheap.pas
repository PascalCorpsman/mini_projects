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
(* Description : Implementation of a heap using the best fit method to find   *)
(*               next block to allocate.                                      *)
(*                                                                            *)
(******************************************************************************)
Unit ubestfitheap;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, ufreelistheap;

Type

  { TBestFitHeap }

  TBestFitHeap = Class(TFreeListHeap)
  public
    Procedure ReCreate(); override; // Nur die Steuerung ob Freigegebene Blöcke hinten oder Vorne in die Freispeicherliste eingefügt werden
    (*
     * Strategie der Allokation :
     * Durchsuche die Freispeicherliste, nehme immer den Block, welcher den kleinsten Verschnitt übrig läst.
     *
     * Freie Blöcke werden Vorne angefügt.
     *)
    Function GetMem(size: ptruint): Pointer; override;
  End;

Implementation

{ TBestFitHeap }

Procedure TBestFitHeap.ReCreate;
Begin
  Inherited ReCreate;
  FreeMode := fmInsertFirst;
  // FreeMode := fmInsertLast; // Verlangsamt Bestfit um 0.1 - 0.4 Sec
End;

Function TBestFitHeap.GetMem(size: ptruint): Pointer;
Var
  pg, d, dd, p: Integer;
  i: ptruint;
Begin
  If size < 8 Then size := 8; // Die Freispeicherliste erzwingt eine Mindestgröße von 8 Byte
  Result := Inherited GetMem(size);
  // 1. Suchen nach dem besten Block ohne ein ResortFreeList zu machen.
  d := HeapSize + 100; // Egal hauptsache unrealistisch Groß
  p := FListFirst;
  pg := ptr_Nil;
  While p <> ptr_Nil Do Begin
    dd := Get_Len(p) - size;
    If ((dd) >= 0) And (dd < d) Then Begin
      If dd = 0 Then Begin // Wir haben den Optimalen Block gefunden.
        result := AllocateBlock(p, size);
        exit;
      End
      Else Begin
        d := dd;
        pg := p;
      End;
    End;
    p := Get_Next(p);
  End;
  If pg <> ptr_Nil Then Begin // Den Block mit dem Kleinsten Verschnitt Wählen
    result := AllocateBlock(pg, size);
    exit;
  End;
  // 2. wir haben keinen groß Genugen Block gefunden
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

