(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of GIT gui                                               *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit ugitgraph;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Graphics;

Const
  (*
   * Maximum number of Branches that can be Displayed at the same time
   * This is a more or less "random" number. So if you have projects with more
   * active branches, feel free to increase.
   *)
  MaxBranches = 32;
  //MaxBranches = 4; // Debugg
  BranchColors: Array[0..7] Of TColor = (clBlack, clRed, clLime, clBlue, $00808080, $00008080, $00808000, $00808000);

Type
  (*
   * Der RevisionGraph besteht aus vielen "TeilSegmenten"
   * Knifflig sind nur dir Bögen
   *
   *        |
   *    LU /|\ RU
   *    ---------
   *       \|/
   *    UL  |  UR
   *)
  TFieldElement = (
    feHalfVLineUp, feHalfVLineDown,
    feHalfLeftHLine, feHalfRightHLine,
    feCircle, feRectangle,
    feArcLU, feArcUL,
    feArcRU, feArcUR
    );

  TFieldElements = Set Of TFieldElement;

  TField = Record
    Elements: TFieldElements;
    PrimColor: TColor; // Farbe aller Linien, Kreise, Rechtecke und des Bogenstücks welcher die Innere Senkrechte berührt
    SecColor: TColor; // Die 2. Farbe des Bogenstücks
    oldHash, Hash: String;
    Active: Boolean; // True = diese "Swimlane" wird gerade für Hasches genutzt, false = Frei ( es kann aber durchaus vorkommen, dass "Waagrechte linien" durch nicht aktive felder laufen
  End;

  TFieldRow = Array[0..MaxBranches - 1] Of TField;

  TGraph = Array Of TFieldRow;


Type
  TGraphInfo = Record
    hash: String;
    ParentsHash: Array Of String;
  End;

  TGraphInfoArray = Array Of TGraphInfo;

  (*
   *
   *)
Function CalcGraph(Const GraphInfo: TGraphInfoArray): TGraph;


Procedure DrawGraphRow(Const Canvas: TCanvas; Const aRow: TFieldRow; Const aRect: Trect);

Implementation

Uses math;

Procedure Nop(); // Debug Only
Begin

End;

Function InterpolateColor(scale: Single; ca, cb: TCOlor): Tcolor;
Var
  r, g, b: integer;
  ra, ga, ba, rb, gb, bb: uint8;
Begin
  ra := ca And $FF;
  ga := (ca And $FF00) Shr 8;
  ba := (ca And $FF0000) Shr 16;
  rb := cb And $FF;
  gb := (cb And $FF00) Shr 8;
  bb := (cb And $FF0000) Shr 16;
  r := min(255, max(0, round((1 - scale) * ra + scale * rb)));
  g := min(255, max(0, round((1 - scale) * ga + scale * gb)));
  b := min(255, max(0, round((1 - scale) * ba + scale * bb)));
  result := RGBToColor(r, g, b);
End;

Procedure DrawGraphRow(Const Canvas: TCanvas; Const aRow: TFieldRow; Const aRect: Trect);
Var
  w, wh, wf: Integer;

  Procedure DrawElement(Const aElement: TField; Const r: Trect);
  Const
    Steps = 10;
  Var
    s, c: Double;
    ox, oy, x, y, i: Integer;
  Begin
    Canvas.Brush.Color := aElement.SecColor;
    Canvas.Pen.Color := aElement.SecColor;
    If feHalfLeftHLine In aElement.Elements Then Begin
      Canvas.Line(r.Left + wh, r.Top + wh, r.Left, r.Top + wh);
    End;
    If feHalfRightHLine In aElement.Elements Then Begin
      Canvas.Line(r.Left + wh, r.Top + wh, r.Right + 1, r.Top + wh);
    End;
    Canvas.Brush.Color := aElement.PrimColor;
    Canvas.Pen.Color := aElement.PrimColor;
    If feHalfVLineUp In aElement.Elements Then Begin
      Canvas.Line(r.Left + wh, r.Top + wh, r.Left + wh, r.Top - 1);
    End;
    If feHalfVLineDown In aElement.Elements Then Begin
      Canvas.Line(r.Left + wh, r.Top + wh, r.Left + wh, r.Bottom);
    End;
    (*
     * Die Bögen sind doof, eigentlich sollte man das Pixelbasiert machen
     * aber Pixelweiser zugriff ist Langsamer als Linien malen
     *)
    If feArcUL In aElement.Elements Then Begin
      i := 0;
      sincos(pi * i / (2 * Steps), s, c);
      ox := r.Left + round(c * wh);
      oy := r.Bottom - round(s * wh);
      For i := 1 To steps Do Begin
        Canvas.Pen.Color := InterpolateColor(i / steps, aElement.PrimColor, aElement.SecColor);
        sincos(pi * i / (2 * Steps), s, c);
        x := r.Left + round(c * wh);
        y := r.Bottom - round(s * wh);
        Canvas.Line(ox, oy, x, y);
        ox := x;
        oy := y;
      End;
      Canvas.Line(ox, oy, x - 1, y);
    End;
    If feArcLU In aElement.Elements Then Begin
      i := 0;
      sincos(pi * i / (2 * Steps), s, c);
      ox := r.Left + round(c * wh);
      oy := r.Top + round(s * wh);
      For i := 1 To steps Do Begin
        Canvas.Pen.Color := InterpolateColor(i / steps, aElement.PrimColor, aElement.SecColor);
        sincos(pi * i / (2 * Steps), s, c);
        x := r.Left + round(c * wh);
        y := r.Top + round(s * wh);
        Canvas.Line(ox, oy, x, y);
        ox := x;
        oy := y;
      End;
      Canvas.Line(ox, oy, x - 1, y);
    End;
    If feArcRU In aElement.Elements Then Begin
      i := 0;
      sincos(pi * i / (2 * Steps), s, c);
      ox := r.Right - round(c * wh);
      oy := r.Top + round(s * wh);
      For i := 1 To steps Do Begin
        Canvas.Pen.Color := InterpolateColor(i / steps, aElement.PrimColor, aElement.SecColor);
        sincos(pi * i / (2 * Steps), s, c);
        x := r.Right - round(c * wh);
        y := r.Top + round(s * wh);
        Canvas.Line(ox, oy, x, y);
        ox := x;
        oy := y;
      End;
      Canvas.Line(ox, oy, x - 1, y);
    End;
    If feArcUR In aElement.Elements Then Begin
      i := 0;
      sincos(pi * i / (2 * Steps), s, c);
      ox := r.Right - round(c * wh);
      oy := r.Bottom - round(s * wh);
      For i := 1 To steps Do Begin
        Canvas.Pen.Color := InterpolateColor(i / steps, aElement.PrimColor, aElement.SecColor);
        sincos(pi * i / (2 * Steps), s, c);
        x := r.Right - round(c * wh);
        y := r.Bottom - round(s * wh);
        Canvas.Line(ox, oy, x, y);
        ox := x;
        oy := y;
      End;
      Canvas.Line(ox, oy, x - 1, y);
    End;
    If feCircle In aElement.Elements Then Begin
      Canvas.Ellipse(r.Left + wf, r.Top + wf, r.Right - wf, r.Bottom - wf);
    End;
    If feRectangle In aElement.Elements Then Begin
      Canvas.Rectangle(r.Left + wf, r.Top + wf, r.Right - wf, r.Bottom - wf);
    End;
  End;

Var
  r: TRect;
  i: Integer;
Begin
  w := aRect.Bottom - aRect.Top;
  wh := w Div 2;
  wf := w Div 4;
  r.Left := aRect.Left;
  r.Top := aRect.Top;
  r.Right := aRect.Left + w;
  r.Bottom := aRect.Bottom;
  For i := 0 To high(aRow) Do Begin
    If aRow[i].Elements <> [] Then Begin
      drawElement(aRow[i], r);
    End;
    r.Left := r.Left + w;
    r.Right := r.Right + w;
  End;
End;

Function CalcGraph(Const GraphInfo: TGraphInfoArray): TGraph;
Var
  Graph: TGraph;

  Function GetEmptySwimLane(Index: Integer): Integer;
  Var
    i: Integer;
  Begin
    result := -1;
    For i := 0 To MaxBranches - 1 Do Begin
      If Not Graph[index, i].Active Then Begin
        result := i;
        exit;
      End;
    End;
    Raise exception.create('Error, to much simultanous branches, recompile with a bigger MaxBranches constant.');
  End;

  Function GetSwimLane(Index: integer; Hash: String): integer;
  Var
    i: Integer;
  Begin
    result := -1; // Nicht gefunden
    For i := 0 To MaxBranches - 1 Do Begin
      If Graph[index, i].Hash = Hash Then Begin
        result := i;
        exit;
      End;
    End;
  End;

Var
  j, i, index, k, newindex, l: Integer;
Begin
  // "Leeren" Graphen initialisieren
  Graph := Nil;
  setlength(Graph, length(GraphInfo) + 1);
  For j := 0 To high(Graph) Do Begin
    For i := 0 To MaxBranches - 1 Do Begin
      Graph[j, i].Elements := [];
      Graph[j, i].PrimColor := BranchColors[i Mod length(BranchColors)];
      Graph[j, i].SecColor := clBlack;
      Graph[j, i].oldHash := '';
      Graph[j, i].Hash := '';
      Graph[j, i].Active := false;
    End;
  End;
  // Befüllen mit Look Ahead 1
  For j := 0 To high(GraphInfo) Do Begin
    index := GetSwimLane(j + 1, GraphInfo[j].hash);
    If index = -1 Then Begin // Hier Startet was neues -> Neuen Index Suchen
      index := GetEmptySwimLane(j + 1);
      Graph[j + 1, index].Active := true;
      Graph[j + 1, index].Hash := GraphInfo[0].hash;
      Graph[j + 1, index].Elements := [feCircle, feHalfVLineDown];
    End;
    Graph[j + 1, index].Elements := Graph[j + 1, index].Elements + [feCircle];
    // Die SwimLanes weiter Führen
    If j < high(GraphInfo) Then Begin
      For k := 0 To MaxBranches - 1 Do Begin
        If Graph[j + 1, k].Active Then Begin
          If (Graph[j, k].Hash <> '') Then Begin // Gibt es oberhalb einen existierenden Hash -> Senkrechte Linie rein
            Graph[j + 1, k].Elements := Graph[j + 1, k].Elements + [feHalfVLineDown, feHalfVLineUp];
          End;
          If k = index Then Begin
            Case length(GraphInfo[j].ParentsHash) Of
              0: Begin
                  // Hier startet ein Branch aus dem Nichts
                End;
              1: Begin
                  Graph[j + 2, index].Active := true;
                  Graph[j + 2, index].oldHash := Graph[j + 2, index].Hash;
                  Graph[j + 2, index].Hash := GraphInfo[j].ParentsHash[0];
                End;
              2: Begin
                  // Hier ein Merge von zwei neuen Branches -> aus einer Swimlane werden 2
                  newindex := GetEmptySwimLane(j + 1);
                  Graph[j + 1, index].Elements := Graph[j + 1, index].Elements - [feCircle];
                  Graph[j + 1, index].Elements := Graph[j + 1, index].Elements + [feRectangle, feHalfVLineUp];
                  If index < newindex Then Begin
                    // Die Neue Swimlane liegt "rechts"
                    Graph[j + 1, index].Elements := Graph[j + 1, index].Elements + [feHalfRightHLine];
                    // Das Waagrechte Teilstück
                    For l := index + 1 To newindex - 1 Do Begin
                      // TODO: Farbe 2 einfügen
                      Graph[j + 1, l].Elements := Graph[j + 1, l].Elements + [feHalfLeftHLine, feHalfRightHLine];
                    End;
                    // Der Bogen nach Unten
                    Graph[j + 1, newindex].Elements := Graph[j + 1, newindex].Elements + [feArcUL];
                    //Graph[j + 1, newindex].Elements := Graph[j + 1, newindex].Elements - [feHalfVLineDown, feHalfVLineUp];
                  End
                  Else Begin
                    // Die Neue Swimlane liegt "Links"
                    Graph[j + 1, index].Elements := Graph[j + 1, index].Elements + [feHalfLeftHLine];
                    // Das Waagrechte Teilstück
                    For l := newindex + 1 To index - 1 Do Begin
                      // TODO: Farbe 2 einfügen
                      Graph[j + 1, l].Elements := Graph[j + 1, l].Elements + [feHalfLeftHLine, feHalfRightHLine];
                    End;
                    // Der Bogen nach unten
                    Graph[j + 1, newindex].Elements := Graph[j + 1, index].Elements + [feArcUR];
                    //Graph[j + 1, index].Elements := Graph[j + 1, index].Elements - [feHalfVLineDown, feHalfVLineUp];
                  End;
                  Graph[j + 1, newindex].Hash := GraphInfo[j].ParentsHash[1];

                  Graph[j + 1, index].oldHash := Graph[j + 1, index].Hash;
                  Graph[j + 1, index].Hash := GraphInfo[j].ParentsHash[0];

                  Graph[j + 1, newindex].Active := true;
                  Graph[j + 2, index].Active := true;

                  Graph[j + 2, index].oldHash := Graph[j + 2, index].Hash;
                  Graph[j + 2, index].Hash := GraphInfo[j].ParentsHash[0];

                  Graph[j + 2, newindex].Active := true;
                  Graph[j + 2, newindex].Hash := GraphInfo[j].ParentsHash[1];
                End;
            End;
          End
          Else Begin
            // Den Aktuellen Hash die Swimlane weiter laufen lassen ...
            Graph[j + 2, k].Active := true;
            If Graph[j + 2, k].Hash <> '' Then Begin // Debug Remove
              If Graph[j + 2, k].Hash <> Graph[j + 1, k].Hash Then Begin // Debug Remove
                Raise exception.create('Logic error, branch will be "killed"'); // Debug Remove
              End; // Debug Remove
            End // Debug Remove
            Else Begin // Debug Remove
              Graph[j + 2, k].oldHash := Graph[j + 2, k].Hash; // TODO: Braucht man das oder ist das Overkill ?
              Graph[j + 2, k].Hash := Graph[j + 1, k].Hash;
            End; // Debug Remove
          End;
        End;
      End;
    End
    Else Begin
      // Der aller letzte Knoten, oder auch der 1. Kommit überhaupt
      Graph[j + 1, index].Elements := Graph[j + 1, index].Elements + [feHalfVLineUp];
    End;
    // Mergen der Swimlanes
    For i := 0 To MaxBranches - 1 Do Begin
      For k := i + 1 To MaxBranches - 1 Do Begin
        If (Graph[j + 1, i].Active) And (Graph[j + 1, k].Active) And (
          ((Graph[j + 1, i].Hash = Graph[j + 1, k].Hash) And (Graph[j + 1, i].Hash <> '')) // Der Reguläre Branch
          Or ((Graph[j + 1, i].oldHash = Graph[j + 1, k].Hash) And (Graph[j + 1, i].oldHash <> '')) // Wenn ein Merge und Branch Gleichzeitig sind
          Or ((Graph[j + 1, i].Hash = Graph[j + 1, k].oldHash) And (Graph[j + 1, i].Hash <> '')) // Wenn ein Merge und Branch Gleichzeitig sind
          Or ((Graph[j + 1, i].oldHash = Graph[j + 1, k].oldHash) And (Graph[j + 1, i].oldHash <> '')) // Wenn ein Merge und Branch Gleichzeitig sind
          )
          Then Begin
          // Ist der Merge nach Links oder nach Rechts ?
          If (feCircle In Graph[j + 1, i].Elements) Or (feRectangle In Graph[j + 1, i].Elements) Then Begin
            // Der Merge geht von Rechts nach Links
            // Den "Rechten" Platt machen
            Graph[j + 1, k].Elements := Graph[j + 1, k].Elements - [feHalfVLineUp, feHalfVLineDown];
            Graph[j + 1, k].Elements := Graph[j + 1, k].Elements + [feArcLU];
            Graph[j + 1, k].oldHash := '';
            Graph[j + 1, k].hash := '';
            Graph[j + 1, i].Elements := Graph[j + 1, i].Elements - [feCircle];
            Graph[j + 1, i].Elements := Graph[j + 1, i].Elements + [feRectangle, feHalfRightHLine];

            Graph[j + 2, k].oldHash := '';
            Graph[j + 2, k].hash := '';
            Graph[j + 2, k].Active := false;
          End
          Else Begin
            // Der Merge geht von Links nach Rechts
            // Den "Linken" Platt machen
            Graph[j + 1, i].Elements := Graph[j + 1, i].Elements - [feHalfVLineUp, feHalfVLineDown];
            Graph[j + 1, i].Elements := Graph[j + 1, i].Elements + [feArcRU];
            Graph[j + 1, i].oldHash := '';
            Graph[j + 1, i].hash := '';
            Graph[j + 1, k].Elements := Graph[j + 1, k].Elements - [feCircle];
            Graph[j + 1, k].Elements := Graph[j + 1, k].Elements + [feRectangle, feHalfLeftHLine];

            Graph[j + 2, i].oldHash := '';
            Graph[j + 2, i].hash := '';
            Graph[j + 2, i].Active := false;
            //
          End;
          // Der Waagrechte Strich zwischen den beiden "Mergenden"
          For l := i + 1 To k - 1 Do Begin
            // TODO: Farbe 2 einfügen
            Graph[j + 1, l].Elements := Graph[j + 1, l].Elements + [feHalfLeftHLine, feHalfRightHLine];
          End;
        End;
      End;
    End;
  End;
  result := Graph;
End;

End.

