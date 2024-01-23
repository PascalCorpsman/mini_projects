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
  Classes, SysUtils, Graphics;

Type
  TIntArray = Array Of Integer;
  TIntArrayArray = Array Of TIntArray;
  TPattern = TIntArrayArray;
  TPatternArray = Array Of TPattern;
  TPointArray = Array Of TPoint;

  { TMatcher }

  TMatcher = Class
  private
    patterns: Array Of Array[0..3] Of Array Of integer;
    pattLen: Integer;
  public
    Constructor Create(PatternCount: integer);
    Destructor Destroy(); override;

    Function tileCompatible(Const a_, b_: TPattern; direction: Integer): Boolean;
    Procedure AddPattern(Pattern, neighbor, direction: integer);
    Function match(pStates: TIntArray; neighbor_states: TIntArrayArray): TIntArray;
  End;

  { TTile }

  TTile = Class
  private
    States: TIntArray;
    pLen, total_states, x, y: integer;
    _hasCollapsed: Boolean;
  public
    color: TColor;
    Constructor Create(aStates: TIntArray; atotal_states, ax, ay: integer);

    Procedure Collapse();
    Function hasCollapsed: Boolean;
    Function getEntropy(): integer;
  End;

  { TWFC }

  TWFC = Class
  private
    fabort: Boolean;
    finished: Boolean;
    grid: Array Of Array Of TTile;

    color_table: TIntArray;
    Patterns: TIntArray; // ACHTUNG: Das sind FarbPatterns !

    Matcher: TMatcher;

    patternsLength: integer; // TODO: Kann wieder Raus

    background_color: TColor;

    w, h, n: Integer;

    affected: TPointArray;

    parsed_patterns: TPatternArray; // TODO: ggf, kann der auch wieder Local in InitFromImage sein.
    Procedure ClearGrid();
    Procedure Seed();
    Function getNeighborIndicies(i, j: integer): TPointArray;
    Procedure UpdateStep();

    Function IsDone(): Boolean;
    Function getLowestEntropyLocation(): TPoint;
  public
    OnUpdatedStep: TNotifyEvent;

    Constructor Create();
    Destructor Destroy(); override;
    Procedure InitFromImage(Image: TBitmap; aN: integer; symmetry: Boolean);

    Procedure Run(aw, ah: Integer);

    Function GetImage(): TBitmap;
    Procedure Abort;
  End;

Implementation

Uses
  LCLIntf, math
  , Dialogs // Debug
  ;

Const
  Infinity: INteger = high(Integer);

Procedure Nop();
Begin

End;

Function includes(Const Data: TIntArray; value: Integer): Boolean; overload;
Var
  i: Integer;
Begin
  result := false;
  For i := 0 To high(data) Do Begin
    If data[i] = value Then Begin
      result := true;
      exit;
    End;
  End;
End;

Function includes(Const Data: TPointArray; value: TPoint): Boolean; overload;
Var
  i: Integer;
Begin
  result := false;
  For i := 0 To high(data) Do Begin
    If data[i] = value Then Begin
      result := true;
      exit;
    End;
  End;
End;

Function includes(Const Data: TPatternArray; Const value: TPattern): Boolean; overload;
Var
  i, j, x: Integer;
  valid: Boolean;
Begin
  result := false;
  For x := 0 To high(data) Do Begin
    valid := true;
    For i := 0 To high(value) Do Begin
      For j := 0 To high(value[i]) Do Begin
        If data[x][i, j] <> value[i, j] Then Begin
          valid := false;
          break;
        End;
      End;
      If Not valid Then break;
    End;
    If valid Then Begin
      result := true;
      exit;
    End;
  End;
End;

Procedure Push(Var data: TIntArray; value: Integer); overload;
Begin
  setlength(data, high(data) + 2);
  data[high(data)] := value;
End;

Procedure Push(Var data: TPointArray; value: TPoint); overload;
Begin
  setlength(data, high(data) + 2);
  data[high(data)] := value;
End;

Procedure Push(Var Data: TPatternArray; Const value: TPattern); overload;
Var
  i, j: Integer;
Begin
  setlength(data, high(data) + 2);
  setlength(data[high(data)], length(value), length(value[0]));
  // TODO: Klären ob man das so "Krass machen muss, oder ob Referenzen reichen"
  //  data[high(data)] := value;
  For i := 0 To high(value) Do Begin
    For j := 0 To high(value[i]) Do Begin
      data[high(data)][i, j] := value[i, j];
    End;
  End;
End;

Procedure Pop(Var Data: TIntArrayArray); // TODO: Eigentlich wäre das eine Function, die das "Pop" Element zurück gibt
Begin
  setlength(data, high(data));
End;

Procedure Shift(Var Data: TIntArrayArray); // TODO: Eigentlich wäre das eine Function, die das "Shift" Element zurück gibt
Var
  i: Integer;
Begin
  For i := 1 To high(data) Do Begin
    data[i - 1] := data[i];
  End;
  setlength(Data, high(data));
End;

Function IndexOf(Const data: TIntArray; value: Integer): integer;
Var
  i: Integer;
Begin
  result := -1;
  For i := 0 To high(data) Do Begin
    If data[i] = value Then Begin
      result := i;
      exit;
    End;
  End;
End;

Function Has(Const data: TIntArray; value: Integer): boolean;
Var
  i: Integer;
Begin
  result := false;
  For i := 0 To high(data) Do Begin
    If data[i] = value Then Begin
      result := true;
      exit;
    End;
  End;
End;

Procedure Delete(Var data: TIntArray; value: Integer);
Var
  i, j: Integer;
Begin
  For i := 0 To high(data) Do Begin
    If data[i] = value Then Begin
      For j := i To high(data) - 1 Do Begin
        data[j] := data[j + 1];
      End;
      setlength(data, high(data));
      exit;
    End;
  End;
End;

Function flip1DArray(Const Data: TIntArrayArray): TIntArrayArray;
Var
  i, j: Integer;
Begin
  result := Nil;
  setlength(result, length(data));
  For i := 0 To high(data) Do Begin
    setlength(result[i], length(data[high(data) - i]));
    For j := 0 To high(data[high(data) - i]) Do Begin
      result[i, j] := data[high(data) - i, j];
    End;
  End;
End;

Function transpose2DArray(Const Data: TIntArrayArray): TIntArrayArray;
Var
  i, j: Integer;
Begin
  result := Nil;
  setlength(result, length(data[0]), length(data));
  For i := 0 To high(data[0]) Do Begin
    For j := 0 To high(data) Do Begin
      result[i, j] := data[j, i];
    End;
  End;
End;

Function arrayIsEqual(a, b: TIntArrayArray): boolean;
Var
  i, j: integer;
Begin
  // Checks if array a is equal to array b.
  // JS sucks with being sensible.
  result := true;
  For i := 0 To high(a) Do Begin
    For j := 0 To high(a[i]) Do Begin
      If a[i, j] <> b[i, j] Then Begin
        result := false;
        exit;
      End;
    End;
  End;
End;

Function Splice(Var a: TPointArray): TPoint;
Var
  i: Integer;
Begin
  result := a[0];
  For i := 1 To high(a) Do Begin
    a[i - 1] := a[i];
  End;
  setlength(a, high(a));
End;

{ TMatcher }

Constructor TMatcher.Create(PatternCount: integer);
Var
  i, j: Integer;
Begin
  setlength(patterns, PatternCount);
  For i := 0 To high(patterns) Do Begin
    For j := 0 To 3 Do Begin
      patterns[i][j] := Nil;
    End;
  End;
  pattLen := PatternCount;
End;

Destructor TMatcher.Destroy;
Begin
  Inherited Destroy;
End;

Function TMatcher.tileCompatible(Const a_, b_: TPattern; direction: Integer
  ): Boolean;
Var
  A, B: TPattern;
  i, j: Integer;
Begin
  a := Nil;
  b := Nil;
  setlength(a, length(a_), length(a_[0]));
  For i := 0 To high(a) Do Begin
    For j := 0 To high(a[i]) Do Begin
      a[i, j] := a_[i, j];
    End;
  End;
  setlength(b, length(b_), length(b_[0]));
  For i := 0 To high(b) Do Begin
    For j := 0 To high(b[i]) Do Begin
      b[i, j] := b_[i, j];
    End;
  End;

  // Check if the tile a overlaps b in a specified direction

  Case direction Of
    0: Begin // Checks the up direction
        pop(a);
        shift(b);
      End;
    1: Begin // Checks the left direction
        A := transpose2DArray(A);
        pop(a);
        B := transpose2DArray(B);
        shift(B);
      End;

    2: Begin // Checks the down direction
        shift(A);
        pop(B);
      End;

    3: Begin // Checks the right direction
        A := transpose2DArray(A);
        shift(A);
        B := transpose2DArray(B);
        pop(B);
      End;
  End;

  result := arrayIsEqual(A, B);
End;

Procedure TMatcher.AddPattern(Pattern, neighbor, direction: integer);
Begin
  //    if (this.patterns[pattern] != undefined) {
  If assigned(patterns[Pattern][direction]) Then Begin
    //      this.patterns[pattern][direction].push(neighbor);
    setlength(patterns[Pattern][direction], high(patterns[Pattern][direction]) + 2);
    patterns[Pattern][direction][high(patterns[Pattern][direction])] := neighbor;
  End
  Else Begin
    setlength(patterns[Pattern][direction], 1);
    patterns[Pattern][direction][0] := neighbor; // This made it work!
  End;
  // this.pattLen = this.patterns.length; -- Das ist quatsch, wir haben das ja schon initialisiert !
End;

Function TMatcher.match(pStates: TIntArray; neighbor_states: TIntArrayArray): TIntArray;
Var
  current_possibilities, possibilities: TIntArray;
  direction, oppositeDirection, i, state, j, k, elt: Integer;
Begin
  // let possibilities = new Set(pStates); -- der Set operator macht hier eigentlich keinen Sinn, weil in pStates keine doppelten sind !
  possibilities := Nil;
  setlength(possibilities, length(pStates));
  For i := 0 To high(pStates) Do Begin
    possibilities[i] := pStates[i];
  End;

  For direction := 0 To 4 - 1 Do Begin
    oppositeDirection := (direction + 2) Mod 4;
    current_possibilities := Nil;

    //      neighbor_states[direction].forEach((state) =>
    //        this.patterns[state][oppositeDirection].forEach((elt) =>
    //          current_possibilities.add(elt)
    //        )
    //      );

    For j := 0 To high(neighbor_states[direction]) Do Begin
      state := neighbor_states[direction][j];
      For k := 0 To high(patterns[state][oppositeDirection]) Do Begin
        elt := patterns[state][oppositeDirection][k];
        setlength(current_possibilities, high(current_possibilities) + 2);
        current_possibilities[high(current_possibilities)] := elt;
      End;
    End;

    //      for (let i = 0; i < this.pattLen; i++)
    For i := 0 To length(patterns) - 1 Do Begin
      If Not has(current_possibilities, i) Then delete(possibilities, i);
      //        if (!current_possibilities.has(i)) possibilities.delete(i);
    End;
  End;
  result := possibilities;
End;

{ TTile }

Constructor TTile.Create(aStates: TIntArray; atotal_states, ax, ay: integer);
Begin
  States := aStates;
  total_states := atotal_states;
  x := ax;
  y := ay;
  pLen := length(States);
  _hasCollapsed := false;
End;

Procedure TTile.Collapse;
Var
  i, j: Integer;
Begin
  _hasCollapsed := true;
  // Picks a random state and makes it the only one in the list
  i := Random(length(States));
  j := States[i];
  setlength(States, 1);
  States[0] := j;
End;

Function TTile.hasCollapsed: Boolean;
Begin
  result := _hasCollapsed;
End;

Function TTile.getEntropy(): integer;
Begin
  // Returns infinity if the tile has collapsed and returns the
  // length of the states if the tile hasn't collapsed
  If (length(states) > 1) Then Begin
    result := length(states);
  End
  Else Begin
    _hasCollapsed := true;
    result := Infinity;
  End;
End;

{ TField }

Procedure TWFC.ClearGrid;
Var
  i, j, k: Integer;
  states: TIntArray;
Begin
  // Initializes the grid. Also clears the grid if already populated
  If Assigned(grid) Then Begin
    For i := 0 To high(grid) Do
      For j := 0 To high(grid[i]) Do
        grid[i][j].free;
  End;
  grid := Nil;
  setlength(grid, h, w);

  For i := 0 To h - 1 Do Begin
    For j := 0 To w - 1 Do Begin
      States := Nil;
      setlength(States, length(patterns));
      For k := 0 To high(states) Do Begin
        states[k] := k;
      End;
      grid[i][j] := TTile.Create(states, length(patterns), j, i);
    End;
  End;
End;

Procedure TWFC.Seed;
Var
  i, j: Integer;
Begin
  i := trunc((random(11) + 45) * h / 100);
  j := trunc((random(11) + 45) * w / 100);

  grid[i][j].collapse();
  grid[i][j].color := color_table[
    patterns[grid[i][j].states[0]]
    ];

  affected := getNeighborIndicies(i, j);
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
  If (Not isDone()) Then Begin
    // Collapse one with the smallest entropy
    If Not assigned(affected) Then Begin
      // Get the indicies of the tile with the minimum entropy.
      min := getLowestEntropyLocation();

      // Colapse the tile.
      grid[Min.x][Min.y].collapse();

      // Add the neighbors of the tile to the affected list to be updated later.
      affected := getNeighborIndicies(Min.x, Min.y);

      // Set the color of the tile to the corresponding patterns (0,0) tile
      grid[Min.x][Min.y].color := color_table[
        patterns[grid[Min.x][Min.y].states[0]]
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

    // console.time("mathcher.match");
    // Get new states and the direction of the possible collapse
    // console.time("match.match");
    nStates := matcher.match(pStates, neighbors);
    // console.timeEnd("match.match");
    nStatesLen := length(nStates);
    // console.timeEnd("mathcher.match");

    // If the size of the previous and new states are different,
    // and the length of new states is greater than 0
    If ((length(pStates) <> nStatesLen) And (nStatesLen > 0)) Then Begin
      // fill(0, 0, 255);
      // noStroke();
      // rect(
      //   this.grid[i][j].x * tileW,
      //   this.grid[i][j].y * tileH,
      //   tileW,
      //   tileH
      // );
      // Update tiles states to be the new states
      grid[i][j].states := nStates;

      // If the length of new states is 1, it means that
      // the tile has collapsed
      If (nStatesLen = 1) Then Begin
        // Set the color of the tile to the coresponding paterns (0,0) tile
        grid[i][j].color := color_table[patterns[nStates[0]]];
        grid[i][j]._hasCollapsed := true;
      End
      Else Begin
        //          let r = 0;
        //          let g = 0;
        //          let b = 0;
        //          for (let k = 0; k < nStatesLen; k++) {
        //            r += this.color_table[this.patterns[nStates[k]]][0];
        //            g += this.color_table[this.patterns[nStates[k]]][1];
        //            b += this.color_table[this.patterns[nStates[k]]][2];
        //          }
        //          this.grid[i][j].color = color(
        //            r / nStatesLen,
        //            g / nStatesLen,
        //            b / nStatesLen
        //          );
      End;

      // For every neighbor indicies
      For dir := 0 To 4 - 1 Do Begin
        // If those indicies are not already in the
        // affected array or in the new affected array,
        // add it to the new affected array
//          if (!this.affected.includes(neighborIndicies[dir])) {
        If Not includes(affected, neighborIndicies[dir]) Then Begin
          push(affected, neighborIndicies[dir]);
        End;
      End;
    End;
  End
  Else Begin
    //      if (!finished) {
    //        main_timer += performance.now();
    //        total_collapse_count++;
    //        console.log(
    //          "%c Average collapse time: " + main_timer / total_collapse_count,
    //          "color: #2a7a4a"
    //        );
    //      }
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
  // Irgend was perverse Großes, was es so nie geben kann
  iInd := -1;
  jInd := -1;
  aEntropy := Infinity;
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
  //      let grid = []; // The grid of entropies
  //    let minCol = []; // Collumn of minimum numbers
  //
  //    for (let i = 0; i < this.H; i++) {
  //      // Initialize the row of entropies
  //      let entropy_row = [];
  //
  //      // Populate the row of entropies with values
  //      for (let j = 0; j < this.W; j++) {
  //        entropy_row[j] = this.grid[i][j].getEntropy();
  //      }
  //
  //      grid[i] = entropy_row;
  //
  //      // Store the minimum of the row in the minCol
  //      minCol[i] = Min(entropy_row);
  //    }
  //
  //    // Get the y index of the minimum value in the collumn
  //    let iInd = minCol.indexOf(Min(minCol));
  //    // Get the x index of the minimum value in the collumn
  //    let jInd = grid[iInd].indexOf(Min(minCol));
  //
//      return [iInd, jInd];
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

Procedure TWFC.InitFromImage(Image: TBitmap; aN: integer; symmetry: Boolean);
Var
  col, direction, ind, i, j, u, v: Integer;
  pattern, rgba_map: TIntArrayArray;
  color_frequencies: TIntArray;
  r, g, b: Integer;
  iW, iH, rotation: Integer;
  _patterns: TPatternArray;
  _Colors: TIntArray;
Begin

  n := aN;
  iW := Image.width;
  iH := Image.height;
  // console.time("Built the rgb map");
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
  // console.timeEnd("Built the rgb map");

  // console.time("Collected the _patterns");

  // initialize the list that will hold the _patterns.
  _patterns := Nil;

  // Loop over the width and height of the image to extract _patterns.
  For i := 0 To IW - 1 Do Begin
    For j := 0 To IH - 1 Do Begin
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
        End;
      End;
    End;
  End;

  // console.timeEnd("Collected the _patterns");

  // console.time("Compiled the matcher");
  // Initialize the matcher object
  parsed_patterns := Nil;
  Matcher.free;
  Matcher := TMatcher.Create(length(_Patterns));

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
        End;
      End;
    End;
  End;
  // console.timeEnd("Compiled the matcher");
  // console.log(`${matcher._patterns.length} _patterns`);

  // console.time("Set up color table");
  _Colors := Nil;
  setlength(_Colors, length(_patterns));
  For i := 0 To high(_patterns) Do Begin
    _Colors[i] := _patterns[i][0][0];
  End;
  // console.timeEnd("Set up color table");

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
Begin
  w := aw;
  h := ah;
  patternsLength := length(patterns);
  affected := Nil;
  clearGrid();
  Seed();
  finished := false;
  fabort := false;
  While (Not Finished) And (Not fabort) Do Begin
    updateStep();
    If assigned(OnUpdatedStep) Then
      OnUpdatedStep(self);
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
  For i := 0 To h - 1 Do Begin
    For j := 0 To w - 1 Do Begin
      If grid[i, j].hasCollapsed() Then Begin
        result.Canvas.Pixels[w - j - 1, i] := grid[i, j].color;
      End;
    End;
  End;
End;

Procedure TWFC.Abort;
Begin
  fabort := true;
End;

End.

