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
Unit ubuddyheap;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, ubaseHeap;

Type

  (*
   * Eigentlich müsste man die Baumstruktur irgendwie im Speicher des Heaps
   * Abbilden und dort Verwalten. Da dies aber nicht gerade einfach zu
   * implementieren ist, wurde nur der für die Baumimplementierung notwendige
   * Speicherplatz "Gesperrt" und die Verwaltungsstruktur Klassisch Realisiert.
   * Dem Benchmarking sollte dies keinen Abbruch tun, da ein Knoten immer gleich
   * viel Speicher verbraucht. Die Allokation über den FPC-Internen Mechanismus
   * also extrem effizient unterstützt wird => Siehe Best- / Next- Fit
   * Simulation mit fester Blockgröße aber unterschiedlichen Lebenszyklen.
   *)
  PNode = ^TNode;

  TNode = Record
    // -- Variablen die Wirklich notwendig sind
    Space: integer; // Speicher, der unterhalb dieses Knotens noch verfügbar ist
    Left, Right: pNode; // Die Kind Knoten
    // -- Variablen die die Implementierung vereinfachen, eine "Richtige"
    //    Implementierung benötigt diese nicht, daher werden sie in der
    //    Speicherverbrauchsrechnung nicht Berücksichtigt.
    Parent: PNode; // Pointer auf den Übergeordneten Knoten, zur einfacheren Navigation im Baum
    Size: Integer; // Der Knoten ist für Size viele Bytes Zuständig
    ArrayIndex: integer; // Der Knoten Verwaltet den Speicherbereich ab Array Index
  End;

  { TBuddyHeap }

  TBuddyHeap = Class(TBaseHeap)
  private
    fBlockSize: integer; // Als Interne Konstante Gehalten, wäre normalerweise eine Const
    FRoot: PNode; // Die Wurzel des Freispeicherbaums
    FInternaStructurSize: integer; // Die Anzahl der Bytes, welche durch den Baum Verbraten werden
    Function SearchBlockSize(Size: integer; Node: PNode): PNode;
  protected
    Function GetHeapSize(): integer; override;
    Procedure FreeNode(Node: PNode); // Zum Aufräumen der internen Datenstruktur bei Recreate und Destroy
    Function AllocatedByteCount(P: Pointer): integer; override;
  public
    Constructor Create(Size: uint32); override;
    Destructor destroy; override;
    (*
     * Strategie der Allokation :
     * Mittels Binärbaum, wird der Heap Maskiert, und Blockweise Zurückgegeben
     * die Kleinste Blockgröße ist dabei fBlockSize Byte. Durch die Baumstruktur soll
     * das Suchen Freier Blöcke Drastisch Reduziert werden. Die Interne Struktur
     * ist dafür Teurer.
     * Der Algorithmus ist ebenfalls Selbst Defragmentierend.
     * Besonderheit : Die Defragmentierung findet beim Free statt.
     *)
    Function GetMem(size: ptruint): Pointer; override;
    Function FreeMem(P: Pointer): ptruint; override;
    Procedure ReCreate(); override; // Quasi der Constructor
  End;

Implementation

(*
 * Gibt die nächst höhere 2erpotenz zu value zurück, entnommen aus "Hackers Delight" ;)
 *)

Function CeilP2(value: integer): integer;
Begin
  value := value - 1;
  value := value Or (value Shr 1);
  value := value Or (value Shr 2);
  value := value Or (value Shr 4);
  value := value Or (value Shr 8);
  value := value Or (value Shr 16);
  result := value + 1;
End;

{ TBuddyHeap }

Procedure TBuddyHeap.ReCreate;
Begin
  Inherited ReCreate;
  (*
   * Je Größer fBlockSize desto weniger Knoten im Binärbaum, aber bei
   * Allokieren von besonders Kleinen Blöcken auch um so mehr Verschnitt
   *)
  // fBlockSize := 16; // Bei dieser Blöckgröße konvergiert der Strukturaufwand gegen ca. 150 % -> Mehr Speicher verbrauch als verfügbar ;-)
  // fBlockSize := 32; // Bei dieser Blöckgröße konvergiert der Strukturaufwand gegen ca. 75 %
  // fBlockSize := 64; // Bei dieser Blöckgröße konvergiert der Strukturaufwand gegen ca. 36 %
  // fBlockSize := 128; // Bei dieser Blöckgröße konvergiert der Strukturaufwand gegen ca. 18.75 %
  fBlockSize := 256; // Bei dieser Blöckgröße konvergiert der Strukturaufwand gegen ca.  9.37 %
  // fBlockSize := 512; // Bei dieser Blöckgröße konvergiert der Strukturaufwand gegen ca.  4.7 %
  FreeNode(FRoot);
  new(FRoot);
  FRoot^.Right := Nil;
  FRoot^.Left := Nil;
  FRoot^.Parent := Nil;
  FRoot^.Space := Length(FData);
  FRoot^.Size := Length(FData);
  FRoot^.ArrayIndex := 0;
  (*
   * Ein Knotenelement hat 3 * 4Byte
   * Kleinster Allokierbarer Block sind fBlockSize Byte
   * Damit hat die Unterste Ebene des Baumes
   * Length(FData) div fBlockSize = n Blätter.
   * Diese Blätter haben n-1 Knoten über sich.
   * => Der Baum wird also insgesammt 2*n -1 Elemente beinhalten.
   * Entsprechend müssen vom Heap
   * 3*4*(2*n-1) Bytes für die Datenstruktur Reserviert werden.
   *                                              24 * Length(FData)
   * Umgestellt gibt das  FInternaStructurSize =  ------------------  - 12
   *                                                 fBlockSize
   *)
  FInternaStructurSize := (24 * Length(FData)) Div fBlockSize - 12;
  FInternaStructurSize := CeilP2(FInternaStructurSize);// Das kommt ja auch noch, dass wir nur Blockweise Allokieren dürfen => Erhöhen auf Speicherverbrauch 12.5 % (Bei Blockgröße 256)
  Getmem(FInternaStructurSize); // Das hier wird Allokiert und "Weggeschmissen", da die Interne Struktur so viel Speicher benötigen würde.
End;

(*
 * Schaut ob im Baum ein Block der Größe Frei und Verfügbar ist, wenn nicht Nil
 *)

Function TBuddyHeap.SearchBlockSize(Size: integer; Node: PNode): PNode;
Begin
  result := Nil;
  If Not assigned(Node) Then exit;
  // Sind wir an einem Blatt und es ist Großgenug, wird es zurück gegeben.
  If (node^.Space = Size) And (node^.Left = Nil) And (node^.Right = Nil) Then Begin
    result := Node;
    exit;
  End;
  If node^.Space >= Size Then Begin
    result := SearchBlockSize(size, node^.Left);
    If Not assigned(Result) Then Begin
      result := SearchBlockSize(size, node^.Right);
    End;
  End;
End;

Function TBuddyHeap.GetHeapSize: integer;
Begin
  Result := Inherited GetHeapSize - FInternaStructurSize;
End;

Procedure TBuddyHeap.FreeNode(Node: PNode);
Begin
  If assigned(node) Then Begin
    FreeNode(node^.Left);
    FreeNode(node^.Right);
    Dispose(node);
  End;
End;

Function TBuddyHeap.AllocatedByteCount(P: Pointer): integer;
Var
  n: PNode;
  ArrayIndex: integer;
Begin
  Result := 0;
  If (p >= @FData[0]) And (p <= @fdata[high(fdata)]) Then Begin
    ArrayIndex := p - @fdata[0];
    // Suchen des Passenden Blattes
    n := FRoot;
    While (n^.ArrayIndex <> ArrayIndex) Or assigned(n^.Left) Or assigned(n^.Right) Do Begin
      If ArrayIndex >= n^.ArrayIndex + (n^.Size Div 2) Then Begin
        n := n^.Right;
      End
      Else Begin
        n := n^.Left;
      End;
    End;
    // Das Blatt ist gefunden
    result := n^.Size;
  End;
End;

Constructor TBuddyHeap.Create(Size: uint32);
Begin
  // Inherited Create ruft Recreate auf, aber Root wird in Recreate Freigegeben,
  // also muss FRoot bereits Vorher initialisiert sein.
  FRoot := Nil;
  Inherited Create(Size);
End;

Destructor TBuddyHeap.destroy;
Begin
  FreeNode(FRoot);
  Inherited destroy;
End;

Function TBuddyHeap.GetMem(size: ptruint): Pointer;
Var
  SearchSize: integer;
  nn, n: Pnode;
Begin
  If size < fBlockSize Then Begin
    size := fBlockSize;
  End
  Else Begin
    size := CeilP2(size);
  End;
  Result := Inherited GetMem(size);
  SearchSize := size;
  Repeat
    n := SearchBlockSize(SearchSize, FRoot);
    // Wir werden den Knoten nie Allokieren können
    If Not Assigned(n) Then Begin
      SearchSize := SearchSize * 2;
      If SearchSize > FRoot^.Space Then Begin
        result := Nil;
        exit;
      End;
    End;
  Until assigned(n);
  // Es Gibt einen Knoten der Prinzipiell in der Lage ist unser Objekt Auf zu nehmen
  // Wir müssen aber ggf zuerst die Knoten Splitten um auf unsere Gesuchte Größe zu kommen
  While n^.Space > size Do Begin
    // Wir müssen N in 2 Knoten Splitten und gehen Links nach Unten um weiter zu splitten
    // Das Rechte Kind zuerst
    new(nn);
    n^.Right := nn;
    nn^.Parent := n;
    nn^.Space := n^.Space Div 2;
    nn^.Size := n^.Size Div 2;
    nn^.Left := Nil;
    nn^.Right := Nil;
    nn^.ArrayIndex := n^.ArrayIndex + nn^.Size;
    // Dann das Linke
    new(nn);
    n^.Left := nn;
    nn^.Parent := n;
    nn^.Space := n^.Space Div 2;
    nn^.Size := n^.Size Div 2;
    nn^.Left := Nil;
    nn^.Right := Nil;
    nn^.ArrayIndex := n^.ArrayIndex;
    n := nn;
  End;
  result := @FData[n^.ArrayIndex];
  // Auf dem Pfad nach Oben Allen Unseren Speicher Abziehen
  While assigned(n) Do Begin
    n^.Space := n^.Space - size;
    n := n^.Parent;
  End;
  // So steht in der Wurzel imme wieviel Speicher noch Allokierbar ist ;)
End;

Function TBuddyHeap.FreeMem(P: Pointer): ptruint;
Var
  ArrayIndex: integer;
  np, n: PNode;
  b: Boolean;
Begin
  Result := Inherited FreeMem(P);
  If (p >= @FData[0]) And (p <= @fdata[high(fdata)]) Then Begin
    ArrayIndex := p - @fdata[0];
    // Suchen des Passenden Blattes
    n := FRoot;
    While (n^.ArrayIndex <> ArrayIndex) Or assigned(n^.Left) Or assigned(n^.Right) Do Begin
      If ArrayIndex >= n^.ArrayIndex + (n^.Size Div 2) Then Begin
        n := n^.Right;
      End
      Else Begin
        n := n^.Left;
      End;
    End;
    // Das Blatt ist gefunden, nun wird es "Wieder Hergestellt"
    If assigned(n^.Parent) Then Begin
      n^.Space := n^.Parent^.Size Div 2;
      np := n^.Parent;
      // Allen Parents Mitteilen, dass wir mehr Speicher haben
      While assigned(np) Do Begin
        np^.Space := np^.Space + n^.Space;
        np := np^.Parent;
      End;
      // Nun die Frage der Fragen, kann Defragmentiert werden ?
      b := true;
      While (b) Do Begin
        b := n^.Parent^.Space = n^.Parent^.Size; // Wenn der Parent wieder Allen Speicher hat
        If b Then Begin
          n := n^.Parent;
          n^.Space := n^.Size;
          Dispose(n^.Left);
          Dispose(n^.Right);
          n^.Left := Nil;
          n^.Right := Nil;
          b := assigned(n^.Parent); // Wenn wir bei der Wurzel sind ist Schluss
        End;
      End;
    End
    Else Begin
      // Jemand hatte die Wurzel Allokiet = Alles
      n^.Space := length(FData);
    End;
    result := 0;
  End
  Else Begin
    result := 1;
    Raise exception.Create('Error not owned by ' + ClassName);
  End;
End;

End.

