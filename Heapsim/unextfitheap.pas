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
Unit unextfitheap;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, ufreelistheap;

Type

  { TNextFitHeap }

  TNextFitHeap = Class(TFreeListHeap)
  private
    FLastAnker: integer; // -- Wird nicht in Heapsize berücksichtigt, da eine geeignete Implementierung diesen Pointer in FListFirst einbetten könnte.
  public
    Procedure ReCreate(); override; // Nur die Steuerung ob Freigegebene Blöcke hinten oder Vorne in die Freispeicherliste eingefügt werden
    (*
     * Strategie der Allokation :
     * Der erste Block in der Freispeicherliste der die Angeforderte Größe
     * bereitstellen kann wird genommen. Aber es wird immer ab dem Alokierten Block
     * weiter gesucht.
     *
     * Freie Blöcke werden Hinten angefügt.
     *)
    Function GetMem(size: ptruint): Pointer; override;
  End;

Implementation

{ TNextFitHeap }

Procedure TNextFitHeap.ReCreate;
Begin
  Inherited ReCreate;
  FLastAnker := FListFirst;
  // FreeMode := fmInsertFirst; // Das ist empirisch bestimmt ein klein wenig Langsammer ca. 0.5 Sec. Bei Default Einstellungen
  FreeMode := fmInsertLast;
End;

Function TNextFitHeap.GetMem(size: ptruint): Pointer;
Var
  p1: integer;
  i: ptruint;
Begin
  If size < 8 Then size := 8; // Die Freispeicherliste erzwingt eine Mindestgröße von 8 Byte
  Result := Inherited GetMem(size);
  // 1. Regulär suchen nach einem Groß genugen Block
  p1 := FLastAnker;
  Repeat
    If Get_Len(p1) >= size Then Begin
      FLastAnker := Get_next(p1);
      result := AllocateBlock(p1, size);
      If FLastAnker = ptr_Nil Then FLastAnker := FListFirst; // Sollte der zuvor Allokierte Block zufällig der Letzte gewesen sein, muss der Ringschluss entsprechend berücksichtigt werden.
      exit;
    End;
    p1 := Get_Next(p1);
    If p1 = ptr_Nil Then p1 := FListFirst; // Damit wirds zur ner Ringliste ;)
  Until p1 = FLastAnker;
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

