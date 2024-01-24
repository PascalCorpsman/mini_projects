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
(*
 * This started as FPC port of https://github.com/D-T-666/wave-function-collapse-p5
 *)
Unit uwfc;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Graphics, uhelper, umatcher, utile;

Type

  TInfo = Record
    TotalCellsToCollapse: integer;
    CollapsedCells: integer;
    WavesCount: integer; // Anzahl der Möglichen Wellen
    WavesCombinationCount: integer; // Anzahl aller möglichen "Kombinationen" von Wellen
    Backlog: Integer;
  End;

  TOnUpdatedStep = Procedure(Sender: TObject; Const Info: TInfo) Of Object;

  { TWFC }

  TWFC = Class
  private
    fWavesCount: integer; // Für Info
    fWavesCombinationCount: integer; // Für Info
    fCollapsedCells: Integer; // Already collapsed cells during this run
    fabort: Boolean; // Wenn True, dann wird die Run routine so schnell wie möglich beendet.
    fDisableVertWrap, fDisableHorWrap: Boolean; // Wenn True, dann wird das "Umschlagen" über die entsprechende Kante unterbunden.
    fBottomwaves: TIntArray; // Alle Hier drin sind "Unten" Waves
    fRightWaves: TIntArray; // Alle Hier drin sind "Right" Waves

    finished: Boolean;
    grid: Array Of Array Of TTile;

    color_table: TIntArray;
    Patterns: TIntArray; // ACHTUNG: Das sind FarbPatterns, keine Wellen der Patterns !

    Matcher: TMatcher;

    background_color: TColor;

    w, h, n: Integer;

    affected: TPointArray;

    Procedure ClearGrid();
    Procedure Seed();
    Function getNeighborIndicies(i, j: integer): TPointArray;
    Procedure UpdateStep();

    Function IsDone(): Boolean;
    Function getLowestEntropyLocation(): TPoint;
  public
    OnUpdatedStep: TOnUpdatedStep;

    Constructor Create();
    Destructor Destroy(); override;
    Procedure InitFromImage(Image: TBitmap; aN: integer; symmetry, DisableVertWrap, DisableHorWrap: Boolean);

    Procedure Run(aw, ah: Integer);

    Function GetImage(): TBitmap;
    Procedure Abort;
  End;

Implementation

Uses
  LCLIntf, math;

{ TField }

Procedure TWFC.ClearGrid;
Var
  statesCnt, i, j, k: Integer;
  states: TIntArray;
  allow: Boolean;
Begin
  // Initializes the grid. Also clears the grid if already populated
  If Assigned(grid) Then Begin
    For i := 0 To high(grid) Do Begin
      For j := 0 To high(grid[i]) Do Begin
        grid[i][j].free;
      End;
    End;
  End;
  grid := Nil;
  setlength(grid, h, w);
  For i := 0 To h - 1 Do Begin
    For j := 0 To w - 1 Do Begin
      States := Nil;
      setlength(States, length(patterns));
      statesCnt := 0;
      For k := 0 To high(states) Do Begin
        allow := true;
        If fDisableVertWrap Then Begin
          If i = 0 Then Begin
            If Not Has(fBottomwaves, k) Then Begin
              allow := false;
            End;
          End
          Else Begin
            If Has(fBottomwaves, k) Then Begin
              allow := false;
            End;
          End;
        End;
        If fDisableHorWrap Then Begin
          If j = w - 1 Then Begin
            If Not Has(fRightWaves, k) Then Begin
              allow := false;
            End;
          End
          Else Begin
            If Has(fRightWaves, k) Then Begin
              allow := false;
            End;
          End;
        End;
        If allow Then Begin
          states[statesCnt] := k;
          inc(statesCnt);
        End;
      End;
      If fDisableHorWrap Or fDisableVertWrap Then Begin
        setlength(states, statesCnt);
      End;
      If Not assigned(states) Then Begin
        Raise exception.create('Error, no valid states for cell found.');
      End;
      grid[i][j] := TTile.Create(states, length(patterns), j, i);
    End;
  End;
End;

Procedure TWFC.Seed;
Var
  i, j: Integer;
Begin
  If fDisableVertWrap Then Begin
    i := 0;
  End
  Else Begin
    i := trunc((random(11) + 45) * h / 100);
  End;
  If fDisableHorWrap Then Begin
    j := w - 1;
  End
  Else Begin
    j := trunc((random(11) + 45) * w / 100);
  End;
  grid[i][j].collapse();
  grid[i][j].color := color_table[
    patterns[grid[i][j].ColorTableIndex]
    ];
  affected := getNeighborIndicies(i, j);
  fCollapsedCells := 1;
End;

Function TWFC.getNeighborIndicies(i, j: integer): TPointArray;
Begin
  result := Nil;
  // Gets the warped inidicies of the
  // neighbors of a specified index.
  setlength(result, 4);
  // Right
  result[0] := point(
    (i + 0 + H) Mod H,
    (j + 1 + W) Mod W
    );
  // Down
  result[1] := point(
    (i + 1 + H) Mod H,
    (j + 0 + W) Mod W
    );
  // Left
  result[2] := point(
    (i + 0 + H) Mod H,
    (j - 1 + W) Mod W
    );
  // Up
  result[3] := point(
    (i - 1 + H) Mod H,
    (j + 0 + W) Mod W
    );
End;

Procedure TWFC.UpdateStep;
Var
  current, min: TPoint;
  _In, _jn, i, j, dir: integer;
  neighborIndicies: TPointArray;
  neighbors: TIntArrayArray;
  pStates, nStates: TIntArray;
  nStatesLen: SizeInt;
Begin
  // TODO: Speedup converting "affected" into a array that's size can only increase
  //       and storeing real size in a separate variable ..
  If (Not isDone()) Then Begin
    // Collapse one with the smallest entropy
    If Not assigned(affected) Then Begin
      // Get the indicies of the tile with the minimum entropy.
      min := getLowestEntropyLocation();

      // Colapse the tile.
      grid[Min.x][Min.y].collapse();
      inc(fCollapsedCells);

      // Add the neighbors of the tile to the affected list to be updated later.
      affected := getNeighborIndicies(Min.x, Min.y);

      // Set the color of the tile to the corresponding patterns (0,0) tile
      grid[Min.x][Min.y].color := color_table[
        patterns[grid[Min.x][Min.y].ColorTableIndex]
        ];
    End;

    // For every affected tile
    If Not assigned(affected) Then exit;
    current := Splice(affected);

    While (grid[current.x][current.y].hasCollapsed()) Do Begin
      If Not assigned(affected) Then exit;
      current := Splice(affected);
    End;

    // Get the location
    i := current.x;
    j := current.y;

    // Get the neighbor indicies
    neighborIndicies := getNeighborIndicies(i, j);

    // Get the neighbor states
    neighbors := Nil;
    setlength(neighbors, 4);
    // Loop over every neighbor location
    For dir := 0 To 4 - 1 Do Begin
      // Get the location
      _in := neighborIndicies[dir].X;
      _jn := neighborIndicies[dir].Y;
      // Add the coresponding tiles states to the neighbor states array
      neighbors[dir] := grid[_iN][_jN].states;
    End;

    // Get previous states
    pStates := grid[i][j].states;

    // Get new states and the direction of the possible collapse
    nStates := matcher.match(pStates, neighbors);
    nStatesLen := length(nStates);

    // If the size of the previous and new states are different,
    // and the length of new states is greater than 0
    If ((length(pStates) <> nStatesLen) And (nStatesLen > 0)) Then Begin
      // Update tiles states to be the new states
      grid[i][j].states := nStates;

      // If the length of new states is 1, it means that
      // the tile has collapsed
      If (nStatesLen = 1) Then Begin
        // Set the color of the tile to the coresponding paterns (0,0) tile
        grid[i][j].color := color_table[patterns[nStates[0]]];
        grid[i][j].SetHasCollapsed;
        inc(fCollapsedCells);
      End;

      // For every neighbor indicies
      For dir := 0 To 4 - 1 Do Begin
        // If those indicies are not already in the
        // affected array or in the new affected array,
        // add it to the new affected array
        If Not includes(affected, neighborIndicies[dir]) Then Begin
          push(affected, neighborIndicies[dir]);
        End;
      End;
    End;
  End
  Else Begin
    finished := true;
  End;
End;

Function TWFC.IsDone: Boolean;
Var
  i, j: Integer;
Begin
  result := false;
  For i := 0 To h - 1 Do Begin
    For j := 0 To w - 1 Do Begin
      If Not grid[i, j].hasCollapsed Then Begin
        exit;
      End;
    End;
  End;
  result := true;
End;

Function TWFC.getLowestEntropyLocation: TPoint;
Var
  aEntropy, iInd, jInd, i, j: Integer;
Begin
  iInd := -1;
  jInd := -1;
  aEntropy := uhelper.Infinity;
  For i := 0 To h - 1 Do Begin
    For j := 0 To w - 1 Do Begin
      If aEntropy > grid[i, j].getEntropy() Then Begin
        aEntropy := grid[i, j].getEntropy();
        iInd := i;
        jInd := j;
      End;
    End;
  End;
  result := point(iInd, jInd);
End;

Constructor TWFC.Create;
Begin
  Matcher := Nil;
  grid := Nil;
  OnUpdatedStep := Nil;
End;

Destructor TWFC.Destroy;
Var
  i, j: Integer;
Begin
  matcher.free;
  If Assigned(grid) Then Begin
    For i := 0 To high(grid) Do
      For j := 0 To high(grid[i]) Do
        grid[i][j].free;
  End;
  setlength(grid, 0, 0);
End;

Procedure TWFC.InitFromImage(Image: TBitmap; aN: integer; symmetry,
  DisableVertWrap, DisableHorWrap: Boolean);
Var
  col, direction, ind, i, j, u, v: Integer;
  pattern, rgba_map: TIntArrayArray;
  color_frequencies: TIntArray;
  r, g, b: Integer;
  iW, iH, rotation, BottomWavesCnt, RightWavesCnt: Integer;
  _patterns: TPatternArray;
  _Colors: TIntArray;
  parsed_patterns: TPatternArray;
Begin
  (*
   * Symmetry makes no sense when disabling wrapping
   *)
  If DisableVertWrap Then symmetry := false;
  If DisableHorWrap Then symmetry := false;
  fDisableVertWrap := DisableVertWrap;
  fDisableHorWrap := DisableHorWrap;
  fBottomwaves := Nil;
  fRightWaves := Nil;
  n := aN;
  iW := Image.width;
  iH := Image.height;

  rgba_map := Nil;
  setlength(rgba_map, ih, iw);
  color_table := Nil;
  color_frequencies := Nil;
  // Loop over the height of the image.
  For j := 0 To IH - 1 Do Begin
    For i := 0 To IW - 1 Do Begin
      col := Image.Canvas.Pixels[i, j];
      If (Not includes(color_table, col)) Then Begin
        push(color_frequencies, 0);
        push(color_table, col);
      End;
      ind := indexOf(color_table, col);
      rgba_map[j][i] := ind;
      color_frequencies[ind] := color_frequencies[ind] + 1;
    End;
  End;

  rgba_map := transpose2DArray(flip1DArray(rgba_map));

  // initialize the list that will hold the _patterns.
  _patterns := Nil;
  If fDisableVertWrap Then Begin
    setlength(fBottomwaves, iw * 8); // Wegen Symmetry kann das bis zu 8 mal mehr werden !
    BottomWavesCnt := 0;
  End;
  If fDisableHorWrap Then Begin
    setlength(fRightWaves, iw * 8); // Wegen Symmetry kann das bis zu 8 mal mehr werden !
    RightWavesCnt := 0;
  End;

  // Loop over the width and height of the image to extract _patterns.
  For j := 0 To ih - 1 Do Begin
    For i := 0 To iw - 1 Do Begin

      // initialize a 2d pattern
      pattern := Nil;
      setlength(pattern, n, n);

      // loop over a NxN box with the offset i,j in the image
      // to extract a single pattern
      For u := 0 To n - 1 Do Begin
        For v := 0 To n - 1 Do Begin
          pattern[u][v] := rgba_map[(i + u) Mod iW][(j + v) Mod iH];
        End;
      End;
      If (Not includes(_patterns, pattern)) Then Begin
        // Now that we have our pattern extracted wecheck if the symmetry is enabled.
        If (symmetry) Then Begin
          // If symmetry is enabled, we need to do all the rotations and reflections.
          // Loop over all directions.
          For rotation := 0 To 4 - 1 Do Begin
            // Tanspose the pattern
            pattern := transpose2DArray(pattern);
            // Check if this instance of the pattern is in the
            // patterns list. If not, add it to the list
            If Not includes(_patterns, pattern) Then Begin
              push(_patterns, pattern);
            End;
            // Flip the pattern
            pattern := flip1DArray(pattern);
            // Check if this instance of the pattern is in the
            // _patterns list. If not, add it to the list
            If Not includes(_patterns, pattern) Then Begin
              push(_patterns, pattern);
            End;
            // If you think about it Transpose+Flip = Rot90°
          End;
        End
        Else Begin
          // If we're not doing any symmetry, We can just
          // check if this instance of the pattern is in the
          // _patterns list. If not, add it to the list
          push(_patterns, pattern);
          If fDisableVertWrap And (j = iH - 1) Then Begin
            fBottomwaves[BottomWavesCnt] := high(_patterns);
            inc(BottomWavesCnt);
          End;
          If fDisableHorWrap And (i = iw - 1) Then Begin
            fRightWaves[RightWavesCnt] := high(_patterns);
            inc(RightWavesCnt);
          End;
        End;
      End
      Else Begin
        // TODO: Wenn FLoored und j = iH -1 dann muss diese Wave auch in das FloorWaves Array !
      End;
    End;
  End;
  If fDisableVertWrap Then Begin
    setlength(fBottomwaves, BottomWavesCnt);
  End;
  If fDisableHorWrap Then Begin
    setlength(fRightWaves, RightWavesCnt);
  End;
  // Initialize the matcher object
  parsed_patterns := Nil;
  Matcher.free;
  Matcher := TMatcher.Create(length(_Patterns));

  fWavesCount := length(_patterns);
  fWavesCombinationCount := 0;
  // Check every pattern for every other pattern
  For i := 0 To high(_patterns) Do Begin
    For j := 0 To high(_patterns) Do Begin
      If (i = 0) Then Begin
        push(parsed_patterns, _patterns[j]);
      End;
      // Check for compatibility in every direction
      For direction := 0 To 4 - 1 Do Begin
        // If compatible, add it to the matcher as a posibility
        If (Matcher.tileCompatible(parsed_patterns[i], parsed_patterns[j], direction)) Then Begin
          // // if (Matcher.tileCompatible(JSON.parse(_patterns[i]), JSON.parse(_patterns[j]), direction)) {
          matcher.addPattern(i, j, direction);
          inc(fWavesCombinationCount);
        End;
      End;
    End;
  End;

  _Colors := Nil;
  setlength(_Colors, length(_patterns));
  For i := 0 To high(_patterns) Do Begin
    _Colors[i] := _patterns[i][0][0];
  End;

  // Calculate an opaque background color by darkening and
  // hueshifting the most used color in the picture
  ind := -1;
  j := -1;
  For i := 0 To high(color_frequencies) Do Begin
    If color_frequencies[i] > j Then Begin
      j := color_frequencies[i];
      ind := i;
    End;
  End;
  col := color_table[ind];
  r := getRValue(col);
  g := getGValue(col);
  b := getBValue(col);
  r := max(0, min(255, round(r * 0.7)));
  g := max(0, min(255, round(g * 0.75)));
  b := max(0, min(255, round(b * 0.85)));
  background_color := RGB(r, g, b);
  Patterns := _Colors;
End;

Procedure TWFC.Run(aw, ah: Integer);
Var
  info: TInfo;
Begin
  w := aw;
  h := ah;
  affected := Nil;
  clearGrid();
  Seed();
  finished := false;
  fabort := false;
  info.TotalCellsToCollapse := aw * ah;
  info.CollapsedCells := 0;
  info.WavesCombinationCount := fWavesCombinationCount;
  info.WavesCount := fWavesCount;
  While (Not Finished) And (Not fabort) Do Begin
    updateStep();
    If assigned(OnUpdatedStep) Then Begin
      info.CollapsedCells := fCollapsedCells;
      info.Backlog := length(affected);
      OnUpdatedStep(self, info);
    End;
  End;
End;

Function TWFC.GetImage: TBitmap;
Var
  i, j: Integer;
Begin
  result := TBitmap.Create;
  result.Width := w;
  result.Height := h;
  result.Canvas.Brush.Color := background_color;
  result.Canvas.Rectangle(-1, -1, w + 1, h + 1);
  For j := 0 To h - 1 Do Begin
    For i := 0 To w - 1 Do Begin
      If grid[j, i].hasCollapsed() Then Begin
        result.Canvas.Pixels[i, j] := grid[j, i].color;
      End;
    End;
  End;
End;

Procedure TWFC.Abort;
Begin
  fabort := true;
End;

End.

