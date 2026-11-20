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

{.$DEFINE Debug}

Interface

Uses
  Classes, SysUtils, Graphics;

Const
  (*
   * Maximum number of Branches that can be Displayed at the same time
   * This is a more or less "random" number. So if you have projects with more
   * active branches, feel free to increase.
   *)
{$IFDEF Debug}
  MaxBranches = 4; // Debugg, set the value as small as possible to not disturbe ;)
{$ELSE}
  MaxBranches = 32;
{$ENDIF}
  BranchColors: Array[0..7] Of TColor = (clBlack, clRed, clLime, clBlue, $00808080, $00008080, $00808000, $00800080);

Type
  (*
   * Der RevisionGraph besteht aus vielen "TeilSegmenten"
   * Knifflig sind nur die Bögen
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
   * Calculates the TGraph content corresponding to the Graphinfo which was extracted by the command:
   *  git --no-pager log --date-order --pretty=format:"%H;%P;%s" --all > log.txt
   * Children must precede their parents. Row zero is reserved for working tree changes.
   * if aSingleFileFolder = true -> only 1 single chain is created ignoring everything else
   *)
Function CalcGraph(Const GraphInfo: TGraphInfoArray; aSingleFileFolder: Boolean): TGraph;

Procedure DrawGraphRow(Const Canvas: TCanvas; Const aRow: TFieldRow; Const aRect: Trect);

Implementation

Uses math
{$IFDEF Debug}
  , unit1
  , forms
{$ENDIF}
  ;

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
    Canvas.Brush.Color := aElement.PrimColor;
    Canvas.Pen.Color := aElement.PrimColor;
    If feHalfVLineUp In aElement.Elements Then Begin
      Canvas.Line(r.Left + wh, r.Top + wh, r.Left + wh, r.Top - 1);
    End;
    If feHalfVLineDown In aElement.Elements Then Begin
      Canvas.Line(r.Left + wh, r.Top + wh, r.Left + wh, r.Bottom);
    End;
    // Always draw the horizontal lines in Front of the vertical ones
    Canvas.Brush.Color := aElement.SecColor;
    Canvas.Pen.Color := aElement.SecColor;
    If feHalfLeftHLine In aElement.Elements Then Begin
      Canvas.Line(r.Left + wh, r.Top + wh, r.Left, r.Top + wh);
    End;
    If feHalfRightHLine In aElement.Elements Then Begin
      Canvas.Line(r.Left + wh, r.Top + wh, r.Right + 1, r.Top + wh);
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
    Canvas.Brush.Color := aElement.PrimColor;
    Canvas.Pen.Color := aElement.PrimColor;
    If feCircle In aElement.Elements Then Begin
      Canvas.Ellipse(r.Left + wf, r.Top + wf, r.Right - wf, r.Bottom - wf);
    End;
    If feRectangle In aElement.Elements Then Begin
      Canvas.Rectangle(r.Left + wf, r.Top + wf, r.Right - wf, r.Bottom - wf);
    End;
  End;

Var
  r: TRect;
  i, PenWidth: Integer;
Begin
  w := aRect.Bottom - aRect.Top;
  wh := w Div 2;
  wf := w Div 4;
  r.Left := aRect.Left;
  r.Top := aRect.Top;
  r.Right := aRect.Left + w;
  r.Bottom := aRect.Bottom;
  PenWidth := Canvas.Pen.Width;
  Canvas.Pen.Width := Max(2, w Div 12);
  Try
    For i := 0 To high(aRow) Do Begin
      If r.Right > aRect.Right Then break;
      If aRow[i].Elements <> [] Then Begin
        drawElement(aRow[i], r);
      End;
      r.Left := r.Left + w;
      r.Right := r.Right + w;
    End;
  Finally
    Canvas.Pen.Width := PenWidth;
  End;
End;

{$IFDEF Debug}

Function toLen(aValue: String; aLen: integer): String;
Begin
  result := aValue;
  While length(result) < aLen Do Begin
    result := ' ' + Result;
  End;
End;
{$ENDIF}

Function CalcGraph(Const GraphInfo: TGraphInfoArray; aSingleFileFolder: Boolean): TGraph;
Var
  Lanes: Array[0..MaxBranches - 1] Of String;
  Row, Lane, Parent, Target, i: Integer;

  Function FindLane(Const Hash: String): Integer;
  Var
    k: Integer;
  Begin
    For k := 0 To High(Lanes) Do
      If Lanes[k] = Hash Then exit(k);
    Result := -1;
  End;

  Function EmptyLane: Integer;
  Begin
    Result := FindLane('');
    If Result = -1 Then
      raise Exception.Create('Too many simultaneous branches. Increase MaxBranches.');
  End;

  Procedure Connect(aSource, aTarget: Integer);
  Var
    k: Integer;
  Begin
    If aSource = aTarget Then Begin
      Include(Result[Row, aSource].Elements, feHalfVLineDown);
      exit;
    End;
    If aSource < aTarget Then Begin
      Include(Result[Row, aSource].Elements, feHalfRightHLine);
      Include(Result[Row, aTarget].Elements, feArcUL);
    End
    Else Begin
      Include(Result[Row, aSource].Elements, feHalfLeftHLine);
      Include(Result[Row, aTarget].Elements, feArcUR);
    End;
    For k := Min(aSource, aTarget) + 1 To Max(aSource, aTarget) - 1 Do
      Result[Row, k].Elements := Result[Row, k].Elements +
        [feHalfLeftHLine, feHalfRightHLine];
    For k := Min(aSource, aTarget) To Max(aSource, aTarget) Do
      Result[Row, k].SecColor := Result[Row, aSource].PrimColor;
  End;

Begin
  Result := Nil;
  SetLength(Result, Length(GraphInfo) + 1);
  For i := 0 To High(Lanes) Do Lanes[i] := '';
  For Row := 1 To High(Result) Do Begin
    For i := 0 To High(Lanes) Do Begin
      Result[Row, i].PrimColor := BranchColors[i Mod Length(BranchColors)];
      Result[Row, i].SecColor := Result[Row, i].PrimColor;
      Result[Row, i].oldHash := Lanes[i];
      If Lanes[i] <> '' Then
        Result[Row, i].Elements := [feHalfVLineUp, feHalfVLineDown];
    End;
    If aSingleFileFolder Then Begin
      Lane := 0;
      Lanes[0] := '';
      Result[Row, 0].Elements := [feCircle];
      If Row > 1 Then Include(Result[Row, 0].Elements, feHalfVLineUp);
      If Row < High(Result) Then Include(Result[Row, 0].Elements, feHalfVLineDown);
    End
    Else Begin
      Lane := FindLane(GraphInfo[Row - 1].hash);
      If Lane = -1 Then Lane := EmptyLane;
      Exclude(Result[Row, Lane].Elements, feHalfVLineDown);
      If Length(GraphInfo[Row - 1].ParentsHash) > 1 Then
        Include(Result[Row, Lane].Elements, feRectangle)
      Else
        Include(Result[Row, Lane].Elements, feCircle);
      Lanes[Lane] := '';
      For Parent := 0 To High(GraphInfo[Row - 1].ParentsHash) Do Begin
        Target := FindLane(GraphInfo[Row - 1].ParentsHash[Parent]);
        If Target = -1 Then Begin
          If (Parent = 0) And (Lanes[Lane] = '') Then Target := Lane
          Else Target := EmptyLane;
          Lanes[Target] := GraphInfo[Row - 1].ParentsHash[Parent];
        End;
        Connect(Lane, Target);
      End;
    End;
    For i := 0 To High(Lanes) Do Begin
      Result[Row, i].Hash := Lanes[i];
      Result[Row, i].Active := Lanes[i] <> '';
    End;
  End;
End;

End.

