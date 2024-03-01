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
(* Description : This unit implements a heap by using a bitvector for marking *)
(*               used and unused heap blocks.                                 *)
(*                                                                            *)
(******************************************************************************)
Unit ubitvectorheap;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, ubaseHeap;

Type

  { TBitVectorHeap }

  TBitVectorHeap = Class(TBaseHeap)
  private
    FirstFree: integer; // Eine Reine Optimierung, wird nicht in der Speicherbetrachtung berücksichtigt, da die Information auch in StartOfHeap eingebettet werden könnte, beschleunigt aber die Default Einstellungen um ca. 60 % in der Zeit
    StartOfHeap: integer; // Eigentlich müsste man diese 4 Byte vom gesamt verfügbaren Heap abziehen und im Heap Allokieren, getHeapSize berücksichtigt dies.
    (*
     * Reserviert "Size+4" Bytes, beginnend mit DataStartpos und gibt dann den
     * Pointer auf einen Gültigen Heap Zeiger zurück.
     * => Da immer 8 Byte weise Allokiert wird, und die Kontrollstruktur 4 Byte
     *    groß ist ergibt sich Pointer mod 8 zu 4.
     *)
    Function AllocateBlock(DataStartPos, Size: integer): Pointer; // fertig
    (*
     * Gibt den Speicher eines gültigen Heap Zeigers inclusive seiner
     * Kontrollstruktur (4 Byte) ohne weitere Prüfung frei.
     *)
    Procedure FreeBlock(DataStartPos, size: integer); // fertig
  protected
    Function GetHeapSize(): integer; override; // fertig
  public
    (*
     * Strategie der Allokation :
     * Im Prinzip first Fit, aber mit der Besonderheit, dass sich der Heap durch
     * die Bitmaske selbst defragmentiert.
     *)
    Function GetMem(size: ptruint): Pointer; override; // fertig
    Function FreeMem(P: Pointer): ptruint; override; // fertig
    Procedure ReCreate(); override; // Quasi der Constructor
  End;

Implementation

{ TBitVectorHeap }

Procedure TBitVectorHeap.ReCreate;
Var
  i: Integer;
Begin
  Inherited ReCreate;
  (*
   * Der Heap wird wie Folgt aufgeteilt :
   *
   * [BitVektorFeld][Allokierbarer Speicher]
   *
   * Der Allokierbare Speicher beginnt an : StartOfHeap und ist (((high(FData) + 1) - StartOfHeap) Bytes Groß
   *
   *)
  StartOfHeap := ((high(FData) + 1) Div 8) + (8 - ((high(FData) + 1) Mod 8)) Mod 8; // An 8-Byte Grenze Ausrichten, nach oben gerundet
  FirstFree := StartOfHeap;
  // Die Bitvektor Tabelle mit "Frei" initialisieren
  For i := 0 To StartOfHeap - 1 Do Begin
    fdata[i] := 0;
  End;
  (*
   * Bei der hier gezeigten Methode ist 1/8 des 1/8 Großen Bitfeldes die
   * Referenz auf sich selbst und kann Folglich nie Allokiert werden
   *
   * => Die ersten 1/64 Bit des Speichers werden somit nie Geschrieben /
   * verwendet und ist damit Verschwendet. Hier besteht entsprechend
   * Optimierungspotential.
   *
   *)
End;

Function TBitVectorHeap.AllocateBlock(DataStartPos, Size: integer): Pointer; // Sieht erst mal Gut Aus
Var
  i: Integer;
  addr, ByteIndex, BitIndex: integer;
Begin
  // Nur an 8-Byte Ausgerichteteten Stellen wird Speicher Alokiert
  If DataStartPos Mod 8 <> 0 Then Begin
    Raise exception.create('Error, AllocateBlock, implementation invalid.');
  End;
  // Speichern der Size Informationen
  WriteInt(DataStartPos, size);
  // Rückgabe des Nutzdaten Pointers
  result := @FData[DataStartPos + 4];
  // Umrechnen in die Bitmaskenkoordinaten
  size := size + 4; // Berücksichtigen der 4 Bytes für die Größe des Speichers, diese müssen Natürlich auch gesperrt werden
  // Markieren des Speichers Als Belegt
  For i := 0 To trunc((size - 1) / 8) Do Begin
    // Markieren der Benutzt Bits
    addr := i * 8 + DataStartPos;
    ByteIndex := addr Div 64;
    BitIndex := (addr Mod 64) Div 8;
    FData[ByteIndex] := FData[ByteIndex] Or (1 Shl BitIndex);
  End;
End;

Procedure TBitVectorHeap.FreeBlock(DataStartPos, size: integer);
Var
  i: Integer;
  addr: Integer;
  ByteIndex: Integer;
  BitIndex: Integer;
Begin
  // Der Interne Header interessiert nicht, wir geben ihn einfach mit Frei.
  DataStartPos := DataStartPos - 4;
  If DataStartPos < FirstFree Then Begin
    FirstFree := DataStartPos;
  End;
  size := size + 4;
  If DataStartPos Mod 8 <> 0 Then Begin
    Raise exception.create('Error, FreeBlock, implementation invalid.');
  End;
  // Löschen der entsprechenden Benutzt Flags
  For i := 0 To trunc((size - 1) / 8) Do Begin
    addr := i * 8 + DataStartPos;
    ByteIndex := addr Div 64;
    BitIndex := (addr Mod 64) Div 8;
    FData[ByteIndex] := FData[ByteIndex] And (Not (1 Shl BitIndex));
  End;
End;

Function TBitVectorHeap.GetHeapSize: integer;
Begin
  Result := Inherited GetHeapSize;
  (*
   * Das Bitfeld, kann ja nicht allokiert werden.
   * Ebenfalls nicht nicht die Variable StartOfHeap
   *)
  result := result - StartOfHeap - sizeof(StartOfHeap);
End;

Function TBitVectorHeap.GetMem(size: ptruint): Pointer;
Var
  i, addr: integer;
  ByteIndex: Integer;
  BitIndex: Integer;
  found: Boolean;
Begin
  Result := Inherited GetMem(size); // Result ist "Nil", dient nur der Statistik
  If size = 0 Then exit; // Wenn gar nichts gewünscht ist, Fehler abfangen
  // Suchen der ersten Freien Position die size+4 Bytes Groß ist
  addr := FirstFree;
  While addr <= length(FData) - size - 4 Do Begin
    found := true;
    For i := (size + 4) Div 8 Downto 0 Do Begin
      ByteIndex := (addr + i * 8) Div 64;
      BitIndex := ((addr + i * 8) Mod 64) Div 8;
      If ((FData[ByteIndex] And (1 Shl BitIndex)) <> 0) Then Begin // Speicher bereits belegt
        found := false;
        // Da wir Wissen, dass bis Addr + i * 8 kein freier Block mehr kommt der Groß Genug ist
        // Können wir diesen auch gleich überspringen => das beschleunigt den Algorithmus locker um Faktor 40
        If i <> 0 Then Begin
          addr := addr + (i - 1) * 8;
        End;
        break;
      End;
    End;
    // Es konnte an Addr genug Speicher gefunden werden
    If found Then Begin
      result := AllocateBlock(addr, size);
      If addr = FirstFree Then Begin
        FirstFree := addr + Size;
        FirstFree := FirstFree + (8 - ((FirstFree) Mod 8)) Mod 8; // Aufrunden auf den nächsten 8er Block
      End;
      exit;
    End;
    inc(addr, 8);
  End;
  // Wenn wir hier gelandet sind, dann konnten wir keinen Groß genugen Block finden.
End;

Function TBitVectorHeap.FreeMem(P: Pointer): ptruint;
Var
  l: integer;
Begin
  Inherited Freemem(p);
  // Zeigt der Zeiger auf unseren Speicherbereich ?
  If (p >= @FData[0]) And (p <= @fdata[high(fdata)]) Then Begin
    l := AllocatedByteCount(p);
    FreeBlock(p - @fdata[0], l);
    result := 0;
  End
  Else Begin
    result := 1;
    Raise exception.Create('Error not owned by ' + ClassName);
  End;
End;

End.



