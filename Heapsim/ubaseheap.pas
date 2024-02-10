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
Unit ubaseHeap;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils;

Type
  (*
   * Basisimplementierung eines SpeicherManagers, als Prototyp für andere Klassen
   *)

  (*
   * Aufbau eines Allokierten Blocks
   *
   * < > steht für 1 Byte
   *
   *   <HiWoHiByS><HiWoLoByS><LoWoHiByS><LoWoLoByS><Data0> .. <DataS-1>
   *                                                 |-> Der Pointer nach Außen Zeigt auf dieses Element
   *  <HiWoHiByS> = (Size and $FF000000) shr 24
   *  <HiWoLoByS> = (Size and $00FF0000) shr 16
   *  <LoWoHiByS> = (Size and $0000FF00) shr 8
   *  <LoWoLoByS> = (Size and $000000FF) shr 0
   *  <Data0> = 1. Byte der Nutzdaten
   *  <DataS-1> = letztes Byte der Nutzdaten
   *
   * Kleinster Allokierter Block = 8 Byte (auch wenn nur 1 Byte angefordert wird)
   *
   * Wenn eine andere Interne datenstruktur verwendet wird. Muss die "AllocatedByteCount" angepasst werden.
   *)

  { TBaseHeap }

  TBaseHeap = Class
  private
    fTotalBytesAllocated: int64; // Gesamt Anzahl an Allokierten Bytes
    fTotalBytesFreed: int64; // Gesamt Anzahl an Freigegebenen Bytes
  protected
    FData: Array Of Byte; // Der Allokierte Heap
    fFreeListReorganisations: int64; // Anzahl der Speicher Neuorganisationen Brauchen nur Nachfahren von TFreeListHeap, so kann die Simulationssoftware sich einige Fallunterscheidungen sparen.
    (*
     * Gibt die Größe des Allokierten Speichers durch p zurück
     *)
    Function AllocatedByteCount(P: Pointer): integer; virtual;
    (*
     * Die Ware Heapsize - (Größe der internen Speicherstruktur) <- Richtig zu setzen durch die jeweilige Kindklasse, so wird nur Length(Fdata) zurückgegeben
     *)
    Function GetHeapSize(): integer; virtual;
    (*
     * Hilfsroutine, Schreibt eine Int Zahl beginnend mit Addr in den Speicher
     *)
    Procedure WriteInt(Addr, value: integer);
    (*
     * Hilfsroutine, Liest passend zu WriteInt ab Addr ein integer Variable vom Speicher
     *)
    Function ReadInt(Addr: integer): integer;
  public
    (*
     * Todo : Allokierten Speicher mit §C0FFEE füllen und beim "Free" anzeigen wieviel % Speicher verwendet / umgeschrieben wurden
     *)
    (*
     * Todo : Memorydump als .bmp
     *)
    Property TotalBytesAllocated: int64 read fTotalBytesAllocated; // Summe über alle FreeMem
    Property TotalBytesFreed: int64 read fTotalBytesFreed; // Summe über alle GetMem
    Property HeapSize: integer read GetHeapSize; // Size aus dem Constructor
    Property FreeListReorganisations: int64 read fFreeListReorganisations; // Anzahl der Neuorganisierungen der FreeListe
    (*
     * !! ACHTUNG !!
     * Nicht den Constructor Überschreiben, sondern die Recreate Routine, sonst
     * kann die Simulation die Heaps nicht zurücksetzen.
     *)
    Constructor Create(Size: uint32); virtual; // Größe in Byte
    Destructor destroy; override;
    (*
     * Erzeugt einen internen Datenzustand wie nach Create
     *)
    Procedure ReCreate(); virtual;
    (*
     * Returns Pointer, if Space is aviable, otherwise Nil
     *)
    Function GetMem(size: ptruint): Pointer; virtual;
    (*
     * Frees Memory allocated by p, error if p is not pointing to heap
     *)
    Function FreeMem(P: Pointer): ptruint; virtual;
    (*
     * Gibt die tatsächlich für einen Block allokierte Größe in Byte zurück
     *)
    Function MemSize(p: Pointer): ptruint;
  End;

Implementation

{ TBaseHeap }

function TBaseHeap.GetHeapSize: integer;
Begin
  result := length(FData);
End;

procedure TBaseHeap.WriteInt(Addr, value: integer);
Begin
  fdata[Addr + 0] := (value And $FF000000) Shr 24;
  fdata[Addr + 1] := (value And $00FF0000) Shr 16;
  fdata[Addr + 2] := (value And $0000FF00) Shr 8;
  fdata[Addr + 3] := (value And $000000FF) Shr 0;
End;

function TBaseHeap.ReadInt(Addr: integer): integer;
Var
  i: Integer;
Begin
  result := fdata[addr];
  For i := 1 To 3 Do Begin
    result := result Shl 8;
    result := result Or fdata[addr + i];
  End;
End;

function TBaseHeap.AllocatedByteCount(P: Pointer): integer;
Begin
  // Umrechnen des Pointes in die Addresse an der die Längeninformation steht.
  result := ReadInt(p - @fdata[0] - 4);
End;

constructor TBaseHeap.Create(Size: uint32);
Begin
  Inherited create;
  If size = 0 Then Begin
    Raise Exception.Create(ClassName + '.create : Error invalid Heap size : ' + inttostr(Size));
  End;
  setlength(FData, size);
  ReCreate();
End;

destructor TBaseHeap.destroy;
Begin
  setlength(fdata, 0);
End;

procedure TBaseHeap.ReCreate;
Begin
  fTotalBytesAllocated := 0;
  fTotalBytesFreed := 0;
  fFreeListReorganisations := 0;
End;

function TBaseHeap.GetMem(size: ptruint): Pointer;
Begin
  result := Nil;
  inc(fTotalBytesAllocated, size);
End;

function TBaseHeap.FreeMem(P: Pointer): ptruint;
Begin
  inc(fTotalBytesFreed, AllocatedByteCount(p));
  result := 1; // 0 = Fehlerfrei
End;

function TBaseHeap.MemSize(p: Pointer): ptruint;
begin
  result := AllocatedByteCount(p);
end;

End.

