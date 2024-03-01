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
(* Description : Prototype implementation to realise different freelist heaps *)
(*                                                                            *)
(******************************************************************************)
Unit ufreelistheap;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, ubaseHeap;

Const
  ptr_Nil: integer = -1; // Quasi ein "Nil" Pointer, darf nicht 0 sein, da das ein Gültiger Array index ist !

Type

  TFreeMode = (
    fmInsertFirst, // Wird ein Block Freigegeben so wird er Vorne an die Freispeicherliste eingefügt
    fmInsertLast // Wird ein Block Freigegeben so wird er Hinten an die Freispeicherliste eingefügt
    );
  (*
   *
   * Aufbau eines Elementes der Freelist
   *  <prev><next><len>< .. >
   *  < > stehen für 4 Byte
   *  < .. > stehen für len viele Bytes
   *
   * Aufbau eines Allokierten Blocks
   *  <len>< .. >
   *  < .. > stehen für len viele Bytes
   *
   * Kleinster Allokierter Block = 5 Byte ( 4 byte für länge, + mindestens 1 Byte Daten)
   *
   *)

  { TFreeListHeap }

  (*
   * TFreeListHeap stellt alle notwendigen Routinen zum Bereitstellen eines
   * Freispeicherlist basierten Heaps zur Verfügung
   *
   * Abgeleitete Kindclassen müssen lediglich die Routine Getmem implementieren.
   *
   * Intern wird eine doppelverpointerte Lineare Liste verwendet (gemäß obiger
   * definition). Die Liste wird auf dem Ungenutzten Speicher abgelegt.
   *
   * Können keine weiteren Datenblöcke mehr allokiert werden, so kann mittels
   * ResortFreeList nach noch verfügbaren Speicherblocken gesucht werden.
   *
   * Bugs, Offene Punkte : Was Passiert, wenn das Letzte Element der Liste Exakt aufgebraucht wird ?
   *)

  TFreeListHeap = Class(TBaseHeap)
  private
    (*
     * Werden zur Internen Verwaltung der Freispeicherliste benötigt.
     * Auf die Freispeicherliste darf nur Lesend zugegriffen werden.
     * Mittels Allocate Block, kann ein Element aus der Liste entnommen werden.
     *)
    Procedure Set_Next(P, Value: integer); // Setzen des Next Pointers
    Procedure Set_Prev(P, Value: integer); // Setzen des Prev Pointers
    Procedure Set_Len(P, Value: integer); // Setzen der Anzahl Verfügbarer Bytes bei Allokation des Blockes
  protected
    FListFirst: integer; // Pointer auf das 1. Element
    FListLast: integer; // Pointer auf das Letzte Element
    FreeMode: TFreeMode; // Steuer Variable, ob FreeBlock, die Blöcke am Ende oder Anfang der Freispeicherliste Einfügen soll, entsprechend wird auch bei Resortliste andersrum gesucht
    (*
     * Die Ware Heapsize - Größe der internen Speicherstruktur
     *)
    Function GetHeapSize(): integer; override;
    (*
     * Hängt einen Block aus der Liste Aus und gibt einen gültigen
     * Speicherbereich zurück.
     * Dabei wird der Aktuelle Block geteilt, der Vordere Teil wird zurückgegeben
     * der neue Hintere Teil wird wieder Korrekt in die Liste Eingehängt.
     * Mindestgröße für size ist 8 Byte !
     *)
    Function AllocateBlock(DataStartPos, Size: integer): Pointer;
    (*
     * Fügt den Block der Freispeicherliste hinzu
     *)
    Procedure FreeBlock(DataStartPos, size: integer);
    (*
     * Sortiert die Freispeicherliste neu, in der Hoffnung, das sich damit
     * Fragmentierungen auflösen. Sobald 2 Aufeinander Folgende Blöcke
     * zusammengefasst werden konnten, wird abgebrochen und die Größe des neu
     * gewonnenen Speicherbereiches zurück gegeben.
     *
     * Soll also ein Speicherblock der Größe K- Allokiert werden und konnte
     * vorher nicht in der Freispeicherliste gefunden werden, so geht dies wie
     * folgt :
     *
     * var integer k := "Gesuchter Freier Speicher in Byte"
     * var integer s;
     * repeat
     *   s := ResortFreeList();
     *   if s >= k then begin
     *     "Suche den zusammengefassten Block, allokiere ihn"
     *     exit;
     *   end;
     * until s = 0;
     * Fehler, konnte keinen Freien Speicher mehr finden.
     *
     * Will man den Speicher Maximal Möglich Defragmentieren so kann dies
     * mittels :
     *
     * var integer s;
     * repeat
     *   s := ResortFreeList();
     * until s = 0;
     *
     * Erreicht werden.
     *)
    Function ResortFreeList(): integer;
    Function Get_Next(P: integer): Integer; // "Zeiger" auf nächstes Element der Freispeicherliste
    Function Get_prev(P: integer): integer; // "Zeiger" auf Vorheriges Element der Freispeicherliste
    Function Get_Len(P: integer): integer; // Anzahl der maximal durch diesen Block allokierbaren Bytes
  public
    Procedure ReCreate(); override;
    Function FreeMem(P: Pointer): ptruint; override;
  End;

Implementation

{ TFreeListHeap }

Function TFreeListHeap.AllocateBlock(DataStartPos, Size: integer): Pointer;
Var
  p, Prev_, Next_, len_: integer;
Begin
  If size < 8 Then Begin
    Raise exception.create('Error, invalid size.');
  End;
  // Die Daten des zu teilenden Blockes
  Prev_ := Get_Prev(DataStartPos);
  Next_ := Get_Next(DataStartPos);
  len_ := Get_Len(DataStartPos);
  If size > len_ Then Begin
    Raise exception.create('Error, invalid size, freelist block not big enough.');
  End;
  If len_ - size < 12 Then Begin // Wenn nach der Allokation weniger als 12 Byte = sizeof(header Freispeicherliste) übrig bleiben, dann wird der ganze Bereich genommen.
    size := len_;
    // Der Block wird komplett Allokiert
    If Prev_ = ptr_Nil Then Begin // Wenn das 1. Element der Liste vernichtet wird
      FListFirst := Next_;
      If FListFirst <> ptr_Nil Then Begin
        Set_Prev(FListFirst, ptr_Nil);
      End
      Else Begin
        // Der letzte Block der Freispeicherliste wurde allokiert
        FListLast := ptr_Nil;
      End;
    End
    Else Begin // Ein Element irgendwo in der Mitte
      Set_Next(Prev_, Next_);
      If next_ = ptr_Nil Then Begin
        FListLast := Prev_;
      End
      Else Begin
        Set_Prev(Next_, Prev_);
      End;
    End;
  End
  Else Begin
    // von dem Block bleibt etwas übrig
    p := DataStartPos + Size + 4; // Die Size + 4 Byte für die Länge
    If Prev_ = ptr_Nil Then Begin // Wenn das 1. Element der Liste vernichtet wird
      FListFirst := p;
      Set_Prev(p, ptr_Nil);
    End
    Else Begin // Ein Element irgendwo in der Mitte
      set_Next(prev_, p);
      Set_Prev(p, Prev_);
    End;
    Set_Next(p, Next_);
    If next_ = ptr_Nil Then Begin
      FListLast := P;
    End
    Else Begin
      Set_Prev(Next_, p);
    End;
    Set_Len(p, len_ - size - 4);
  End;
  WriteInt(DataStartPos, size);
  result := @FData[DataStartPos + 4];
End;

Procedure TFreeListHeap.FreeBlock(DataStartPos, size: integer);
Begin
  // Fügt den Freien Block einfach Vorn in die Freispeicherliste ein
  If FreeMode = fmInsertFirst Then Begin
    If FListFirst = ptr_Nil Then Begin
      // Es gibt keine Freispeicherliste mehr, diese muss nun mittels diesem Block wieder Initial angelegt werden
      FListFirst := DataStartPos;
      FListLast := DataStartPos;
      Set_Prev(FListFirst, ptr_Nil);
      Set_Next(FListFirst, ptr_Nil);
    End
    Else Begin
      Set_Prev(FListFirst, DataStartPos);
      Set_Next(DataStartPos, FListFirst);
      Set_Prev(DataStartPos, ptr_Nil);
      FListFirst := DataStartPos;
    End;
  End
  Else Begin
    If FListLast = ptr_Nil Then Begin
      // Es gibt keine Freispeicherliste mehr, diese muss nun mittels diesem Block wieder Initial angelegt werden
      FListFirst := DataStartPos;
      FListLast := DataStartPos;
      Set_Prev(FListLast, ptr_Nil);
      Set_Next(FListLast, ptr_Nil);
    End
    Else Begin
      Set_Next(FListLast, DataStartPos);
      Set_Prev(DataStartPos, FListLast);
      Set_Next(DataStartPos, ptr_Nil);
      FListLast := DataStartPos;
    End;
  End;
  Set_Len(DataStartPos, size);
End;

Procedure TFreeListHeap.ReCreate;
Begin
  Inherited ReCreate;
  // Den Heap So initialisieren, dass er eine "Leere" Liste Beinhaltet
  FListFirst := 0;
  FListLast := 0;
  Set_Prev(FListFirst, ptr_Nil); // Prev Pointer = Nil
  Set_Next(FListFirst, ptr_Nil); // Next Pointer = Nil
  Set_Len(FListFirst, length(FData) - 4); // Frei Verfügbarer Speicher
  FreeMode := fmInsertFirst; // Egal Was, hauptsache Initialisiert, die Kindklasse entscheidet dies selbst.
End;

Function TFreeListHeap.FreeMem(P: Pointer): ptruint;
Var
  l: Integer;
Begin
  Result := Inherited FreeMem(P);
  // Zeigt der Zeiger auf unseren Speicherbereich ?
  If (p >= @FData[0]) And (p <= @fdata[high(fdata)]) Then Begin
    l := AllocatedByteCount(p);
    FreeBlock(p - @fdata[0] - 4, l);
    result := 0;
  End
  Else Begin
    result := 1;
    Raise exception.Create('Error not owned by ' + ClassName);
  End;
End;

Function TFreeListHeap.ResortFreeList: integer;
Var
  p1, p1_nachfolger_Addr, p2: integer;
Begin
  fFreeListReorganisations := fFreeListReorganisations + 1;
  result := 0;
  If FreeMode = fmInsertFirst Then Begin
    If FListFirst = ptr_Nil Then exit; // Es gibt keine Freispeicherliste mehr, die man Optimieren könnte
    p1 := FListFirst;
    Repeat
      // Suchen ob irgend ein Nachfolger von P1 direkt nach P1 Steht, also Remerged werden kann
      p1_nachfolger_Addr := p1 + Get_Len(p1) + 4;
      p2 := FListFirst; // Die Suche nach einem Passenden Nachfolger muss immer die Ganze Liste durchsuchen, da der im Speicher Liegende Nachfolger zu P1 in der Liste vor P1 liegen kann.
      // Durch die Freispeicherliste durch gehen und bei allen Nachsehen ob der Speicher zusammenhängend ist.
      While (p2 <> ptr_Nil) And (p1_nachfolger_Addr <> p2) Do Begin
        p2 := Get_Next(p2);
      End;
      If p1_nachfolger_Addr = p2 Then Begin // P1 und P2 können nun Verschmolzen werden
        // Das Element P2 aus der Liste Werfen.
        If Get_Next(p2) = ptr_Nil Then Begin
          // Wir verschmelzen mit dem Letzten Element der Liste, also muss dessen Vorletzter der Letzte werden.
          Set_Next(Get_prev(p2), ptr_Nil);
          FListLast := Get_prev(p2);
        End
        Else Begin
          // Die Zeiger einfach um uns Rum Biegen
          If Get_prev(p2) = ptr_Nil Then Begin
            Set_Prev(Get_Next(p2), ptr_Nil);
            FListFirst := Get_Next(p2);
          End
          Else Begin
            Set_Next(Get_prev(p2), Get_Next(p2));
            Set_Prev(Get_Next(p2), Get_prev(p2));
          End;
        End;
        // So nun kann P1 um die Größe von P2 + 4 (der Len Pointer verschwindet ja auch) vergrößert werden
        Set_Len(p1, Get_Len(p1) + Get_Len(p2) + 4);
        result := Get_Len(p1); // Melden des neu gewonnenen größeren Blockes
        exit;
      End;
      p1 := Get_Next(p1);
    Until p1 = ptr_Nil;
  End
  Else Begin
    If FListLast = ptr_Nil Then exit; // Es gibt keine Freispeicherliste mehr, die man Optimieren könnte
    p1 := FListLast;
    Repeat
      // Suchen ob irgend ein Nachfolger von P1 direkt nach P1 Steht, also Remerged werden kann
      p1_nachfolger_Addr := p1 + Get_Len(p1) + 4;
      p2 := FListLast; // Die Suche nach einem Passenden Nachfolger muss immer die Ganze Liste durchsuchen, da der im Speicher Liegende Nachfolger zu P1 in der Liste vor P1 liegen kann.
      // Durch die Freispeicherliste durch gehen und bei allen Nachsehen ob der Speicher zusammenhängend ist.
      While (p2 <> ptr_Nil) And (p1_nachfolger_Addr <> p2) Do Begin
        p2 := Get_prev(p2);
      End;
      If p1_nachfolger_Addr = p2 Then Begin // P1 und P2 können nun Verschmolzen werden
        // Das Element P2 aus der Liste Werfen.
        If Get_Next(p2) = ptr_Nil Then Begin
          // Wir verschmelzen mit dem Letzten Element der Liste, also muss dessen Vorletzter der Letzte werden.
          Set_Next(Get_prev(p2), ptr_Nil);
          FListLast := Get_prev(p2);
        End
        Else Begin
          // Die Zeiger einfach um uns Rum Biegen
          If Get_prev(p2) = ptr_Nil Then Begin
            Set_Prev(Get_Next(p2), ptr_Nil);
            FListFirst := Get_Next(p2);
          End
          Else Begin
            Set_Next(Get_prev(p2), Get_Next(p2));
            Set_Prev(Get_Next(p2), Get_prev(p2));
          End;
        End;
        // So nun kann P1 um die Größe von P2 + 4 (der Len Pointer verschwindet ja auch) vergrößert werden
        Set_Len(p1, Get_Len(p1) + Get_Len(p2) + 4);
        result := Get_Len(p1); // Melden des neu gewonnenen größeren Blockes
        exit;
      End;
      p1 := Get_prev(p1);
    Until p1 = ptr_Nil;

  End;
End;

Function TFreeListHeap.Get_prev(P: integer): integer;
Begin
  result := ReadInt(p);
End;

Function TFreeListHeap.Get_Next(P: integer): Integer;
Begin
  result := ReadInt(p + 4);
End;

Function TFreeListHeap.Get_Len(P: integer): integer;
Begin
  result := ReadInt(p + 8);
End;

Procedure TFreeListHeap.Set_Prev(P, Value: integer);
Begin
  WriteInt(p, value);
End;

Procedure TFreeListHeap.Set_Next(P, Value: integer);
Begin
  WriteInt(p + 4, value);
End;

Procedure TFreeListHeap.Set_Len(P, Value: integer);
Begin
  WriteInt(p + 8, value);
End;

Function TFreeListHeap.GetHeapSize: integer;
Begin
  Result := Inherited GetHeapSize - 8; // Die beiden Listenpointer, FreeMode wird nicht berücksichtigt, da das ja normalerweise Hardcodiert wäre.
End;

End.

