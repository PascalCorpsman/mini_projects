(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Clickomania                                           *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit uclickomania;

{$MODE objfpc}{$H+}

Interface

Uses
  SysUtils, classes, dglOpenGL, uopengl_graphikengine, math,
  uopengl_spriteengine, lclintf, graphics, uvectormath;

Const
  // Game Version
  Version: Single = 0.04;

Type
  TRGB = Record
    r: single;
    g: single;
    b: single;
  End;

  (*
  -2     = Feld wurde gerade gelöscht
  -1     = Unbesetzt
  0 .. X = Feldfarbe
  *)
  Tfield = Array Of Array Of Integer;

  TBackGroundstyle = (bsDefault, bsFromPicture, bsFromDirectory, bsgray);
  TGameState = (gsWaitForPlayerInput, gsAnimExplosion, gsAnimDropStones, gsAnimMoveStonesLeft);
  TScoreMode = (smLow, smHigh);

  { TUndoStack }

  TUndoStack = Class
  private
    FStack: Array Of TField;
    Fstack2: Array Of Integer;
  public
    Constructor create;
    Destructor destroy; override;
    Procedure Clear;
    Procedure Push;
    Function Pop: Boolean;
  End;

  { THighscoreEngine }

  THighScoreSubResult = Record
    Name: String;
    Points: Integer;
  End;

  THighScoreResult = Record
    Results: Array[0..9] Of THighScoreSubResult;
    TotalGames: Integer;
    averagePoints: Integer;
  End;

  THighscoreSubitem = Record
    Width: Integer;
    Height: Integer;
    Colors: Integer;
    Spezials: Boolean;
    Name: String;
    Stones: Integer;
    Scores: Integer;
  End;

  THighScoreList = Array Of THighscoreSubitem;

  THighscoreEngine = Class
  private
    FHighscoreList: THighScoreList;
    FChanged: Boolean;
    pw: Array[0..99] Of Integer;
  public
    Constructor Create;
    Destructor Destroy; override;
    Procedure ClearAll;
    Procedure Clear(Width, Height, Colors: Integer; Spezials: Boolean);
    Procedure AddScore(Width, Height, Colors: Integer; Spezials: Boolean; Name: String; Stones, Scores: Integer);
    Procedure SaveToFile(Filename: String);
    Procedure LoadFromFile(Filename: String);
    Function ShowHighscores(Width, Height, Colors: Integer; Spezials: Boolean; Mode: TScoreMode): THighScoreResult;
  End;

Var
  (****************************************************************************)
  (* Optionen                                                                 *)
  (****************************************************************************)
  // Karteikarte Field
  FieldColumns: Integer = 10;
  FieldRows: Integer = 16;
  StonesHeight: integer = 21;
  StonesWidth: Integer = 20;

  // Karteikarte Stones
  ColorNumbers: integer = 5;
  F_XsTextured: Boolean = false;
  AlphaF_XsTextured: Single = 0.5; // der Grad der Durchsichtigkeit für Textured
  F_XsVoyer: Boolean = false;
  f_XsTracelines: Boolean = true;
  SoundsPlaysounds: Boolean = false;

  // Karteikarte Background
  Backstyle: TBackgroundstyle = bsdefault;
  DirectPicture: String = '';
  DirectDir: String = '';

  // Karteikarte Actions
  EnableSpecialStones: Boolean = false;
  EnableAnimations: boolean = true;
  EnableAutoDropCountDown: Boolean = false;
  Duration: integer = 90;

  // Karteikarte Special
  Trackthegame: Boolean = true;
  ScoreMode: TScoreMode = smlow;

  (****************************************************************************)
  (* In Game Variablen                                                        *)
  (****************************************************************************)
  GameState: TGameState = gsWaitForPlayerInput; // Der Spielstatus
  Field: Tfield = Nil; // Das Aktuelle Feld
  cntField: Array Of Array Of Boolean; // TMP Feld, zum Zählen der Löschbaren Steine
  Backtex: Integer = 0; // Der OpenGL Pointer für das Hintergrundbild
  AppPath: String;
  rockettex: integer;
  BombTex: Integer;
  StoneTex: Integer;
  UsedSpezials: Boolean; // Benötigt für die Highscore
  UsedColorNumbers: Integer; // Benötigt für die Highscore
  ExpSprite: Integer; // Sprite Pointer
  DropStartTime: DWord;
  GameScores: Integer; // Die Punkte für die High Score Wertung
  (****************************************************************************)

Const

  DropStoneDelay = 250; // Zeit in MS die es dauert bis alle Steine Runter gefallen sind
  //DropStoneDelay = 5000; // Nur zum Debuggen

  // Konstanten für Spezial Steine
  // Der Wert der Konstanten ist fast egal, er mus nur > High(StoneColors) und disjunkt sein.
  SpezialStoneRocketUp = 1000;
  SpezialStoneRocketDown = 1001;
  SpezialStoneRocketLeft = 1002;
  SpezialStoneRocketRight = 1003;
  SpezialStoneBomb = 1004;
  SpezialStoneStone = 1005;

  // Wahrscheinlichkeit, für einen "Spezial" Stein in %
  WkeitSpecial = 4;
Var
  // Alle Stein Farben die so Möglich sind ...
  StoneColorsDef: Array[0..14] Of TRGB = // Die Default Farben
  ((r: 255 / 255; g: 0 / 255; b: 0 / 255),
    (r: 0 / 255; g: 0 / 255; b: 255 / 255),
    (r: 0 / 255; g: 128 / 255; b: 0 / 255),
    (r: 255 / 255; g: 128 / 255; b: 0 / 255),
    (r: 192 / 255; g: 192 / 255; b: 192 / 255),
    (r: 128 / 255; g: 128 / 255; b: 0 / 255),
    (r: 0 / 255; g: 255 / 255; b: 0 / 255),
    (r: 0 / 255; g: 128 / 255; b: 128 / 255),
    (r: 0 / 255; g: 0 / 255; b: 128 / 255),
    (r: 128 / 255; g: 0 / 255; b: 0 / 255),
    (r: 255 / 255; g: 255 / 255; b: 0 / 255),
    (r: 255 / 255; g: 0 / 255; b: 255 / 255),
    (r: 128 / 255; g: 0 / 255; b: 128 / 255),
    (r: 0 / 255; g: 255 / 255; b: 255 / 255),
    (r: 128 / 255; g: 128 / 255; b: 128 / 255));

  StoneColors: Array[0..14] Of TRGB =
  ((r: 255 / 255; g: 0 / 255; b: 0 / 255),
    (r: 0 / 255; g: 0 / 255; b: 255 / 255),
    (r: 0 / 255; g: 128 / 255; b: 0 / 255),
    (r: 255 / 255; g: 128 / 255; b: 0 / 255),
    (r: 192 / 255; g: 192 / 255; b: 192 / 255),
    (r: 128 / 255; g: 128 / 255; b: 0 / 255),
    (r: 0 / 255; g: 255 / 255; b: 0 / 255),
    (r: 0 / 255; g: 128 / 255; b: 128 / 255),
    (r: 0 / 255; g: 0 / 255; b: 128 / 255),
    (r: 128 / 255; g: 0 / 255; b: 0 / 255),
    (r: 255 / 255; g: 255 / 255; b: 0 / 255),
    (r: 255 / 255; g: 0 / 255; b: 255 / 255),
    (r: 128 / 255; g: 0 / 255; b: 128 / 255),
    (r: 0 / 255; g: 255 / 255; b: 255 / 255),
    (r: 128 / 255; g: 128 / 255; b: 128 / 255));

  Highscore: THighscoreEngine;

Function StringFromStream(Const Stream: TStream): String;
Procedure StringToStream(Const Stream: TStream; Value: String);
// Erzeugt ein neues Feld
Procedure initGame;
// Rendert das Field im WaitonPlayerInput Status
Procedure RenderField;
// Rendert das Feld mit kleinen Explusionen an den Steinen die gerade gelöscht wurden
Procedure RenderExplosionField;
// Rendert das Feld in welchem die Steine Runter Fallen
Procedure RenderDropStoneField;
// Rendert das Feld in welchem die Steine nach Links Rutschen
Procedure RenderMoveStonesLeft;
// Prüft ob sich im Field an Koordinate x,y ein Löschbarer Stein befindet
Function CoordCanBeDeleted(x, y: Integer): Boolean;
// Löscht alle Zusammenhängende Felder ausgehend von x,y
Procedure DeleteCoord(x, y: Integer);
// Läst alle Steine auf einen Schlag fallen
Procedure DropStonesInField;
// Läst alle Steine nach Links Wandern
Procedure MoveStonesLeftInField;
// Lädt alle -2 in -1 um
Procedure ClearAktTrigger;
// Speichert ein Feld
Procedure Push;
// Stellt ein Feld wieder her
Function Pop: Boolean;
// Löscht den UndoStack
Procedure ClearUndo;
// Berechnet die High Score der Jeweiligen Koordinate
Function ScoreCoord(x, y: Integer): Integer;
// Berechnet die Punktzahl der entsprechenden gruppe
Function score(g: Integer): Integer;
// Konvertierungsfunktionen
Function ColorToTRGB(Value: TColor): TRGB;
Function TRGBToColor(Value: TRGB): TColor;

Implementation

{$I clickomania.inc}

Var
  undostack: TUndoStack;

Function TRGBToColor(Value: TRGB): TColor;
Var
  r, g, b: integer;
Begin
  r := min(255, max(0, round((value.r * 255))));
  g := min(255, max(0, round((value.g * 255))));
  b := min(255, max(0, round((value.b * 255))));
  result := ($000000FF And r) Or (($000000FF And g) Shl 8) Or (($000000FF And b) Shl 16);
End;

Function ColorToTRGB(Value: TColor): TRGB;
Var
  r, g, b: integer;
Begin
  r := (value And $000000FF);
  g := (value And $0000FF00) Shr 8;
  b := (value And $00FF0000) Shr 16;
  result.r := r / 255;
  result.g := g / 255;
  result.b := b / 255;
End;

Procedure ClearUndo;
Begin
  undostack.Clear;
End;

Procedure ClearAktTrigger;
Var
  i, j: Integer;
Begin
  For i := 0 To FieldColumns - 1 Do
    For j := 0 To FieldRows - 1 Do
      If field[i, j] = -2 Then
        field[i, j] := -1;
End;

Procedure Push;
Begin
  undostack.push;
End;

Function Pop: Boolean;
Begin
  result := undostack.pop;
End;

Procedure MoveStonesLeftInField;
Var
  i, j, k: Integer;
  b: Boolean;
Begin
  i := 0;
  While i < FieldColumns - 1 Do Begin
    If Field[i, FieldRows - 1] = -1 Then Begin
      b := false;
      For j := 0 To FieldRows Do
        For k := i To FieldColumns - 1 Do Begin
          If k < FieldColumns - 1 Then Begin
            field[k, j] := field[k + 1, j];
            // Nur wenn auch Tatsächlich eine Reihe Verschoben wurde, darf
            // Hier das Verschieben nochmals versucht werden.
            If field[k, j] >= 0 Then b := True;
          End
          Else
            field[k, j] := -1;
        End;
      If b Then
        dec(i);
    End;
    inc(i);
  End;
End;

Procedure DropStonesInField;
Var
  t, i, j: Integer;
  b: Boolean;
Begin
  For i := 0 To FieldColumns - 1 Do Begin
    j := FieldRows - 1;
    While j > 0 Do Begin
      If Field[i, j] = -2 Then Begin
        t := j;
        b := False;
        While field[i, t] = -2 Do Begin
          j := t;
          While j > 0 Do Begin
            field[i, j] := field[i, j - 1];
            // Wenn auch nur ein einziger Stein viel, dann wird an dieser Stelle
            // Vorsichtshalber noch mal fallen gelassen.
            If field[i, j] >= 0 Then b := True;
            dec(j);
          End;
          field[i, 0] := -1;
        End;
        If b Then
          j := t;
      End;
      dec(j);
    End;
  End;
End;

Function score(g: Integer): Integer;
Begin
  If (g = 0) Or (g = 1) Then
    result := 0
  Else Begin
    g := g - 2;
    result := g * g + g + 2;
  End;
End;

Function ScoreCoord(x, y: Integer): Integer;
Var
  r, oldnum: integer;

  Procedure ScoreCoordHelper(i, j: Integer);
  Begin
    If (i >= 0) And (j >= 0) And (i < FieldColumns) And (j < FieldRows) Then
      If (Field[i, j] = oldnum) And (Not cntfield[i, j]) Then Begin
        cntfield[i, j] := true;
        inc(r);
        ScoreCoordHelper(i + 1, j);
        ScoreCoordHelper(i - 1, j);
        ScoreCoordHelper(i, j + 1);
        ScoreCoordHelper(i, j - 1);
      End;
  End;
Var
  i, j: Integer;
Begin
  r := 0;
  // Spezial Steine
  If field[x, y] > High(StoneColors) Then Begin
    Case field[x, y] Of
      SpezialStoneRocketUp: Begin
          For j := y Downto 0 Do
            If Field[x, j] >= 0 Then
              inc(r);
        End;
      SpezialStoneRocketDown: Begin
          For j := y To FieldRows - 1 Do
            If Field[x, j] >= 0 Then
              inc(r);
        End;
      SpezialStoneRocketLeft: Begin
          For i := x Downto 0 Do
            If Field[i, y] >= 0 Then
              inc(r);
        End;
      SpezialStoneRocketRight: Begin
          For i := x To FieldColumns - 1 Do
            If Field[i, y] >= 0 Then
              inc(r);
        End;
      SpezialStoneBomb: Begin
          For i := max(0, x - 1) To min(x + 1, FieldColumns - 1) Do
            For j := max(0, y - 1) To min(y + 1, FieldRows - 1) Do
              If Field[i, j] >= 0 Then
                inc(r);
        End;
    End;
  End
  Else Begin
    // Init
    For i := 0 To FieldColumns - 1 Do
      For j := 0 To FieldRows - 1 Do
        cntfield[i, j] := false;
    oldnum := field[x, y];
    ScoreCoordHelper(x, y);
  End;
  result := r;
End;

Procedure DeleteCoord(x, y: Integer);
Var
  oldnum: Integer;
  // Rekursives Floodfill, eigentlich Böse, aber
  // da die Felder hier so klein sind gehts ;)
  Procedure DeleteCoordHelper(i, j: Integer);
  Begin
    If (i >= 0) And (j >= 0) And (i < FieldColumns) And (j < FieldRows) Then
      If Field[i, j] = oldnum Then Begin
        Field[i, j] := -2;
        DeleteCoordHelper(i + 1, j);
        DeleteCoordHelper(i - 1, j);
        DeleteCoordHelper(i, j + 1);
        DeleteCoordHelper(i, j - 1);
      End;
  End;
Var
  g, i, j: Integer;
Begin
  // Berechnen der High Scores
  g := ScoreCoord(x, y);
  GameScores := GameScores + score(g);
  // Spezial Steine
  If field[x, y] > High(StoneColors) Then Begin
    Case field[x, y] Of
      SpezialStoneRocketUp: Begin
          For j := y Downto 0 Do
            If Field[x, j] >= 0 Then
              Field[x, j] := -2;
        End;
      SpezialStoneRocketDown: Begin
          For j := y To FieldRows - 1 Do
            If Field[x, j] >= 0 Then
              Field[x, j] := -2;
        End;
      SpezialStoneRocketLeft: Begin
          For i := x Downto 0 Do
            If Field[i, y] >= 0 Then
              Field[i, y] := -2;
        End;
      SpezialStoneRocketRight: Begin
          For i := x To FieldColumns - 1 Do
            If Field[i, y] >= 0 Then
              Field[i, y] := -2;
        End;
      SpezialStoneBomb: Begin
          For i := max(0, x - 1) To min(x + 1, FieldColumns - 1) Do
            For j := max(0, y - 1) To min(y + 1, FieldRows - 1) Do
              If Field[i, j] >= 0 Then
                Field[i, j] := -2;
        End;
    End;
  End
  Else Begin
    oldnum := field[x, y];
    DeleteCoordHelper(x, y);
  End;
End;

Procedure RenderMoveStonesLeft;
Var
  b: Boolean;
  c, i, j: integer;
  dt: Single;
Begin
  dt := min(1, (GetTickCount - DropStartTime) / DropStoneDelay);
  glpushmatrix;
  If F_XsTextured Then Begin
    glenable(gl_Blend);
    glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);
  End;
  b := True;
  c := 0;
  For i := 0 To FieldColumns - 1 Do Begin
    If field[i, FieldRows - 1] < 0 Then inc(c);
    glpushmatrix;
    For j := 0 To FieldRows - 1 Do Begin
      // Rendern des Feldes
      If Field[i, j] >= 0 Then Begin
        glBindTexture(GL_TEXTURE_2d, 0);
        // Rendern des Eigentlichen Quads
        If Field[i, j] > High(StoneColors) Then Begin
          If c <> 0 Then
            b := false;
          If F_XsTextured Then
            glcolor4f(1, 1, 1, 1)
          Else
            glcolor3f(1, 1, 1);
          glPushMatrix;
          glTranslatef(-dt * StonesWidth * c, 0, 0);
          If field[i, j] = SpezialStoneRocketUp Then
            RenderAlphaRQuad(point(0, 0), point(StonesWidth, StonesHeight), 270, false, rockettex)
          Else If field[i, j] = SpezialStoneRocketdown Then
            RenderAlphaRQuad(point(0, 0), point(StonesWidth, StonesHeight), 90, false, rockettex)
          Else If field[i, j] = SpezialStoneRocketleft Then
            RenderAlphaRQuad(point(0, 0), point(StonesWidth, StonesHeight), 180, false, rockettex)
          Else If field[i, j] = SpezialStoneRocketright Then
            RenderAlphaRQuad(point(0, 0), point(StonesWidth, StonesHeight), 0, false, rockettex)
          Else If field[i, j] = SpezialStoneBomb Then
            RenderAlphaRQuad(point(0, 0), point(StonesWidth, StonesHeight), 180, false, bombtex)
          Else If field[i, j] = SpezialStoneStone Then
            RenderAlphaRQuad(point(0, 0), point(StonesWidth, StonesHeight), 180, false, Stonetex);
          glpopmatrix;
          glBindTexture(GL_TEXTURE_2d, 0);
        End
        Else If field[i, j] >= 0 Then Begin
          If c <> 0 Then
            b := false;
          If F_XsTextured Then
            glcolor4f(StoneColors[field[i, j]].r, StoneColors[field[i, j]].g, StoneColors[field[i, j]].b, AlphaF_XsTextured)
          Else
            glColor3fv(@StoneColors[field[i, j]]);
          glbegin(gl_quads);
          glVertex2f(-dt * StonesWidth * c, 0);
          glVertex2f(StonesWidth - dt * StonesWidth * c, 0);
          glVertex2f(StonesWidth - dt * StonesWidth * c, StonesHeight);
          glVertex2f(0 - dt * StonesWidth * c, StonesHeight);
          glend;
        End;
        (*
        Das Problem ist, dass die senkrechte Linie beim Verschieben, ab und an "Komisch" aussieht.
        Die einzige Behebung ist das Ein bzw. Einkommentieren der 4 mit

        //-------------------------------------

        gekenzeichneten Linien
        *)
      End; //-------------------------------------
      If c = 0 Then Begin // C = 0
        If F_XsTextured Then
          glcolor4f(0, 0, 0, 0)
        Else
          glcolor3f(0, 0, 0);
        If f_XsTracelines Then Begin
          glbegin(GL_LINES);
          // Rahmen nur an den Rändern
          // Waagrechte Linien
          If j > 0 Then Begin
            If (Field[i, j] <> field[i, j - 1]) And ((Field[i, j] >= 0) Or (Field[i, j - 1] >= 0)) Then Begin
              glVertex2f(0, 0);
              glVertex2f(StonesWidth, 0);
            End;
            If j = FieldRows - 1 Then
              If field[i, j] >= 0 Then Begin
                glVertex2f(StonesWidth, StonesHeight + dt * StonesHeight * c - 1);
                glVertex2f(0, StonesHeight + dt * StonesHeight * c - 1);
              End;
          End
          Else If field[i, j] >= 0 Then Begin
            glVertex2f(0, 0);
            glVertex2f(StonesWidth, 0);
          End;
          // Senkrechte Liniean
          If i > 0 Then Begin
            If (field[i, j] <> field[i - 1, j]) And ((Field[i - 1, j] >= 0) Or (Field[i, j] >= 0)) Then Begin
              glVertex2f(0, 0);
              glVertex2f(0, StonesHeight + dt * StonesHeight * c);
            End;
            If i = FieldColumns - 1 Then
              If Field[i, j] >= 0 Then Begin
                glVertex2f(StonesWidth, 0);
                glVertex2f(StonesWidth, StonesHeight + dt * StonesHeight * c);
              End;
            If i < FieldColumns - 1 Then
              If (Field[i, j] >= 0) And (Field[i + 1, j] < 0) Then Begin
                glVertex2f(StonesWidth, 0);
                glVertex2f(StonesWidth, StonesHeight + dt * StonesHeight * c);
              End;
          End
          Else If Field[i, j] >= 0 Then Begin
            glVertex2f(1, 0);
            glVertex2f(1, StonesHeight + dt * StonesHeight * c);
          End;
          glend;
        End
        Else Begin
          // Ein rahmen um das Komplette Feld
          If field[i, j] >= 0 Then Begin
            glbegin(GL_LINE_LOOP);
            glVertex2f(1, dt * StonesHeight * c);
            glVertex2f(StonesWidth, dt * StonesHeight * c);
            glVertex2f(StonesWidth, StonesHeight + dt * StonesHeight * c - 1);
            glVertex2f(0, StonesHeight + dt * StonesHeight * c - 1);
            glend;
          End;
        End;
      End; // C = 0
      gltranslatef(0, StonesHeight, 0);
    End;
    glpopmatrix;
    gltranslatef(StonesWidth, 0, 0);
  End;
  If b Then Begin
    DropStartTime := GetTickCount - 2 * DropStoneDelay;
  End;
  If F_XsTextured Then
    gldisable(gl_Blend);
  glpopmatrix;
End;

Function CoordCanBeDeleted(x, y: Integer): Boolean;
Begin
  result := False;
  // Spezial Steine
  If field[x, y] > High(StoneColors) Then Begin
    If Field[x, y] <> SpezialStoneStone Then result := true;
  End
    // Normale Steine
  Else If Field[x, y] >= 0 Then Begin
    If x > 0 Then
      If Field[x, y] = field[x - 1, y] Then
        result := true;
    If x < FieldColumns - 1 Then
      If Field[x, y] = field[x + 1, y] Then
        result := true;
    If y > 0 Then
      If Field[x, y] = field[x, y - 1] Then
        result := true;
    If y < FieldRows - 1 Then
      If Field[x, y] = field[x, y + 1] Then
        result := true;
  End;
End;

Procedure initGame;
Var
  i, j: Integer;
  bs: TBackgroundstyle;
  l: Tstringlist;
  sr: TSearchRec;
  ext, d: String;
Begin
  // Setzen für Korrektes Speichern Highscore Relevanter Kriterien
  GameScores := 0;
  UsedSpezials := EnableSpecialStones;
  UsedColorNumbers := ColorNumbers;
  // Löschen der History
  UndoStack.Clear;
  // Setzen der Spielfeldgröße
  setlength(field, FieldColumns, FieldRows);
  setlength(cntField, FieldColumns, FieldRows);
  // Setzen der Farben
  For j := 0 To High(field[0]) Do
    For i := 0 To High(field) Do Begin
      If EnableSpecialStones Then Begin
        // Schaun ob der Stein ein Spezial Stein ist.
        If random(100) < WkeitSpecial Then Begin
          // Wahl des Spezial Steines
          Case Random(100) Of
            // rackete
            0..32: Begin
                Case Random(4) Of
                  0: field[i, j] := SpezialStoneRocketDown;
                  1: field[i, j] := SpezialStoneRocketUp;
                  2: field[i, j] := SpezialStoneRocketLeft;
                  3: field[i, j] := SpezialStoneRocketRight;
                End;
              End;
            // Bombe
            33..65: Begin
                field[i, j] := SpezialStoneBomb;
              End;
            // Stein
            66..99: Begin
                field[i, j] := SpezialStoneStone;
              End;
          End;
        End
        Else
          // Mit Spezial Steine, aber halt grad kein Spezial Stein
          field[i, j] := random(ColorNumbers);
      End
      Else
        // Ohne Spezial Steine
        field[i, j] := random(ColorNumbers);
    End;
  // Laden der Hintergrund Graphik
  bs := Backstyle;
  Case Backstyle Of
    bsFromDirectory: Begin
        d := IncludeTrailingPathDelimiter(DirectDir);
        If DirectoryExists(d) Then Begin
          l := Tstringlist.create;
          If (FindFirst(d + '*', faAnyFile, SR) = 0) Then Begin
            Repeat
              ext := lowercase(ExtractFileExt(d + SR.Name));
              If (ext = '.bmp') Or (ext = '.jpg') Or (ext = '.png') Then Begin
                l.add(d + sr.name);
              End;
            Until FindNext(SR) <> 0;
            FindClose(SR);
          End;
          If l.count = 0 Then
            bs := bsDefault
          Else
            d := l[random(l.count)];
          l.free;
        End
        Else
          bs := bsDefault;
      End;
    bsFromPicture: Begin
        If Not FileExists(DirectPicture) Then
          bs := bsDefault;
      End;
  End;
  Case bs Of
    bsDefault: Begin
        Backtex :=  OpenGL_GraphikEngine.LoadGraphik(AppPath + 'GFX' + PathDelim + 'default.png', smStretch);
      End;
    bsFromDirectory: Begin
        Backtex := OpenGL_GraphikEngine.LoadGraphik(d, smStretch);
      End;
    bsFromPicture: Begin
        Backtex := OpenGL_GraphikEngine.LoadGraphik(DirectPicture, smStretch);
      End;
  End;
End;

Procedure RenderField;
Var
  i, j: Integer;
Begin
  glpushmatrix;
  If F_XsTextured Then Begin
    glenable(gl_Blend);
    glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);
  End;
  For j := 0 To FieldRows - 1 Do Begin
    glpushmatrix;
    For i := 0 To FieldColumns - 1 Do Begin
      // Rendern des Eigentlichen Quads
      If Field[i, j] > High(StoneColors) Then Begin
        If F_XsTextured Then
          glcolor4f(1, 1, 1, 1)
        Else
          glcolor3f(1, 1, 1);
        If field[i, j] = SpezialStoneRocketUp Then
          RenderAlphaRQuad(point(0, 0), point(StonesWidth, StonesHeight), 270, false, rockettex)
        Else If field[i, j] = SpezialStoneRocketdown Then
          RenderAlphaRQuad(point(0, 0), point(StonesWidth, StonesHeight), 90, false, rockettex)
        Else If field[i, j] = SpezialStoneRocketleft Then
          RenderAlphaRQuad(point(0, 0), point(StonesWidth, StonesHeight), 180, false, rockettex)
        Else If field[i, j] = SpezialStoneRocketright Then
          RenderAlphaRQuad(point(0, 0), point(StonesWidth, StonesHeight), 0, false, rockettex)
        Else If field[i, j] = SpezialStoneBomb Then
          RenderAlphaRQuad(point(0, 0), point(StonesWidth, StonesHeight), 180, false, bombtex)
        Else If field[i, j] = SpezialStoneStone Then
          RenderAlphaRQuad(point(0, 0), point(StonesWidth, StonesHeight), 180, false, Stonetex);
        glBindTexture(GL_TEXTURE_2d, 0);
      End
      Else If field[i, j] >= 0 Then Begin
        If F_XsTextured Then
          glcolor4f(StoneColors[field[i, j]].r, StoneColors[field[i, j]].g, StoneColors[field[i, j]].b, AlphaF_XsTextured)
        Else
          glColor3fv(@StoneColors[field[i, j]]);
        glBegin(gl_quads);
        glVertex2i(0, 0);
        glVertex2i(StonesWidth, 0);
        glVertex2i(StonesWidth, StonesHeight);
        glVertex2i(0, StonesHeight);
        glend;
      End;
      If F_XsTextured Then
        glcolor4f(0, 0, 0, 0)
      Else
        glcolor3f(0, 0, 0);
      If f_XsTracelines Then Begin
        glbegin(GL_LINES);
        // Rahmen nur an den Rändern
        // Waagrechte Linien
        If j > 0 Then Begin
          If (Field[i, j] <> field[i, j - 1]) Then Begin
            glVertex2i(0, 0);
            glVertex2i(StonesWidth, 0);
          End;
          If j = FieldRows - 1 Then
            If field[i, j] >= 0 Then Begin
              glVertex2i(StonesWidth, StonesHeight - 1);
              glVertex2i(0, StonesHeight - 1);
            End;
        End
        Else If field[i, j] >= 0 Then Begin
          glVertex2i(0, 0);
          glVertex2i(StonesWidth, 0);
        End;
        // Senkrechte Liniean
        If i > 0 Then Begin
          If (field[i, j] <> field[i - 1, j]) And ((Field[i - 1, j] >= 0) Or (Field[i, j] >= 0)) Then Begin
            glVertex2i(0, 0);
            glVertex2i(0, StonesHeight);
          End;
          If i = FieldColumns - 1 Then
            If Field[i, j] >= 0 Then Begin
              glVertex2i(StonesWidth, 0);
              glVertex2i(StonesWidth, StonesHeight);
            End;
        End
        Else If Field[i, j] >= 0 Then Begin
          glVertex2i(1, 0);
          glVertex2i(1, StonesHeight);
        End;
        glend;
      End
      Else Begin
        // Ein rahmen um das Komplette Feld
        If field[i, j] >= 0 Then Begin
          glbegin(GL_LINE_LOOP);
          glVertex2i(1, 0);
          glVertex2i(StonesWidth, 0);
          glVertex2i(StonesWidth, StonesHeight - 1);
          glVertex2i(0, StonesHeight - 1);
          glend;
        End;
      End;
      glTranslatef(StonesWidth, 0, 0);
    End;
    glpopmatrix;
    glTranslatef(0, StonesHeight, 0);
  End;
  If F_XsTextured Then
    gldisable(gl_Blend);
  glpopmatrix;
End;

Procedure RenderExplosionField;
Var
  i, j: Integer;
Begin
  glpushmatrix;
  If F_XsTextured Then Begin
    glenable(gl_Blend);
    glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);
  End;
  glScalef(StonesWidth, StonesHeight, 1);
  For j := 0 To FieldRows - 1 Do Begin
    glpushmatrix;
    For i := 0 To FieldColumns - 1 Do Begin
      // Rendern des Eigentlichen Quads
      If Field[i, j] > High(StoneColors) Then Begin
        If F_XsTextured Then
          glcolor4f(1, 1, 1, 1)
        Else
          glcolor3f(1, 1, 1);
        If field[i, j] = SpezialStoneRocketUp Then
          RenderAlphaRQuad(v2(0, 0), v2(1, 0.99), 270, false, rockettex)
        Else If field[i, j] = SpezialStoneRocketdown Then
          RenderAlphaRQuad(v2(0, 0), v2(1, 0.99), 90, false, rockettex)
        Else If field[i, j] = SpezialStoneRocketleft Then
          RenderAlphaRQuad(v2(0, 0), v2(1, 0.99), 180, false, rockettex)
        Else If field[i, j] = SpezialStoneRocketright Then
          RenderAlphaRQuad(v2(0, 0), v2(1, 0.99), 0, false, rockettex)
        Else If field[i, j] = SpezialStoneBomb Then
          RenderAlphaRQuad(v2(0, 0), v2(1, 0.99), 180, false, bombtex)
        Else If field[i, j] = SpezialStoneStone Then
          RenderAlphaRQuad(v2(0, 0), v2(1, 0.99), 180, false, Stonetex);
        glBindTexture(GL_TEXTURE_2d, 0);
      End
      Else If Field[i, j] = -2 Then Begin
        If F_XsTextured Then
          glcolor4f(1, 1, 1, 1)
        Else
          glcolor3f(1, 1, 1);
        OpenGL_SpriteEngine.RenderSprite(ExpSprite);
        glBindTexture(GL_TEXTURE_2d, 0);
      End
      Else If field[i, j] >= 0 Then Begin
        If F_XsTextured Then
          glcolor4f(StoneColors[field[i, j]].r, StoneColors[field[i, j]].g, StoneColors[field[i, j]].b, AlphaF_XsTextured)
        Else
          glColor3fv(@StoneColors[field[i, j]]);
        glBegin(gl_quads);
        glVertex2i(0, 0);
        glVertex2i(1, 0);
        glVertex2i(1, 1);
        glVertex2i(0, 1);
        glend;
      End;
      If F_XsTextured Then
        glcolor4f(0, 0, 0, 0)
      Else
        glcolor3f(0, 0, 0);
      If f_XsTracelines Then Begin
        glbegin(GL_LINES);
        // Rahmen nur an den Rändern
        // Waagrechte Linien
        If j > 0 Then Begin
          If (Field[i, j] <> field[i, j - 1]) Then Begin
            glVertex2i(0, 0);
            glVertex2i(1, 0);
          End;
          If j = FieldRows - 1 Then
            If field[i, j] >= 0 Then Begin
              glVertex2f(1, 0.99);
              glVertex2f(0, 0.99);
            End;
        End
        Else If field[i, j] >= 0 Then Begin
          glVertex2f(0, 0);
          glVertex2f(1, 0);
        End;
        // Senkrechte Liniean
        If i > 0 Then Begin
          If (field[i, j] <> field[i - 1, j]) And ((Field[i - 1, j] >= 0) Or (Field[i, j] >= 0)) Then Begin
            glVertex2f(0, 0);
            glVertex2f(0, 1);
          End;
          If i = FieldColumns - 1 Then
            If Field[i, j] >= 0 Then Begin
              glVertex2f(1, 0);
              glVertex2f(1, 1);
            End;
        End
        Else If Field[i, j] >= 0 Then Begin
          glVertex2f(0.01, 0);
          glVertex2f(0.01, 1);
        End;
        glend;
      End
      Else Begin
        // Ein rahmen um das Komplette Feld
        If field[i, j] >= 0 Then Begin
          glbegin(GL_LINE_LOOP);
          glVertex2f(0.01, 0);
          glVertex2f(1, 0);
          glVertex2f(1, 0.99);
          glVertex2f(0, 0.99);
          glend;
        End;
      End;
      glTranslatef(1, 0, 0);
    End;
    glpopmatrix;
    glTranslatef(0, 1, 0);
  End;
  glScalef(1, 1, 1);
  If F_XsTextured Then
    gldisable(gl_Blend);
  glpopmatrix;
End;

Procedure RenderDropStoneField;
Var
  jj, oj, oc, // Korrecktur Traceline
  c, i, j: Integer;
  dt: Single;
  b: Boolean;
Begin
  dt := min(1, (GetTickCount - DropStartTime) / DropStoneDelay);
  glpushmatrix;
  If F_XsTextured Then Begin
    glenable(gl_Blend);
    glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);
  End;
  b := True;
  c := 0; // Korrecktur Traceline
  jj := FieldRows - 1; // Korrecktur Traceline
  For i := 0 To FieldColumns - 1 Do Begin
    glpushmatrix;
    oc := c; // Korrecktur Traceline
    oj := jj; // Korrecktur Traceline
    jj := FieldRows - 1; // Korrecktur Traceline
    c := 0;
    For j := FieldRows - 1 Downto 0 Do Begin
      // Zählen der "Lücken"
      If Field[i, j] = -2 Then Begin
        inc(c);
        jj := j; // Korrecktur Traceline
      End;
      // Rendern des Feldes
      If Field[i, j] >= 0 Then Begin
        glBindTexture(GL_TEXTURE_2d, 0);
        // Rendern des Eigentlichen Quads
        If Field[i, j] > High(StoneColors) Then Begin
          If c <> 0 Then
            b := false;
          If F_XsTextured Then
            glcolor4f(1, 1, 1, 1)
          Else
            glcolor3f(1, 1, 1);
          glpushmatrix;
          glTranslatef(0, j * StonesHeight + dt * StonesHeight * c, 0);
          If field[i, j] = SpezialStoneRocketUp Then
            RenderAlphaRQuad(point(0, 0), point(StonesWidth, StonesHeight), 270, false, rockettex)
          Else If field[i, j] = SpezialStoneRocketdown Then
            RenderAlphaRQuad(point(0, 0), point(StonesWidth, StonesHeight), 90, false, rockettex)
          Else If field[i, j] = SpezialStoneRocketleft Then
            RenderAlphaRQuad(point(0, 0), point(StonesWidth, StonesHeight), 180, false, rockettex)
          Else If field[i, j] = SpezialStoneRocketright Then
            RenderAlphaRQuad(point(0, 0), point(StonesWidth, StonesHeight), 0, false, rockettex)
          Else If field[i, j] = SpezialStoneBomb Then
            RenderAlphaRQuad(point(0, 0), point(StonesWidth, StonesHeight), 180, false, bombtex)
          Else If field[i, j] = SpezialStoneStone Then
            RenderAlphaRQuad(point(0, 0), point(StonesWidth, StonesHeight), 180, false, Stonetex);
          glpopmatrix;
          glBindTexture(GL_TEXTURE_2d, 0);
        End
        Else If field[i, j] >= 0 Then Begin
          If c <> 0 Then
            b := false;
          If F_XsTextured Then
            glcolor4f(StoneColors[field[i, j]].r, StoneColors[field[i, j]].g, StoneColors[field[i, j]].b, AlphaF_XsTextured)
          Else
            glColor3fv(@StoneColors[field[i, j]]);
          glbegin(gl_quads);
          glVertex2f(0, j * StonesHeight + dt * StonesHeight * c);
          glVertex2f(StonesWidth, j * StonesHeight + dt * StonesHeight * c);
          glVertex2f(StonesWidth, j * StonesHeight + StonesHeight + dt * StonesHeight * c);
          glVertex2f(0, j * StonesHeight + StonesHeight + dt * StonesHeight * c);
          glend;
        End;
      End;
      // rendern der Umrandung
      If c = 0 Then Begin // C = 0
        If F_XsTextured Then
          glcolor4f(0, 0, 0, 0)
        Else
          glcolor3f(0, 0, 0);
        If f_XsTracelines Then Begin
          glbegin(GL_LINES);
          // Rahmen nur an den Rändern
          // Waagrechte Linien
          If j > 0 Then Begin
            If (Field[i, j] <> field[i, j - 1]) And ((Field[i, j] >= 0) Or (Field[i, j - 1] >= 0)) Then Begin
              glVertex2f(0, j * StonesHeight + dt * StonesHeight * c);
              glVertex2f(StonesWidth, j * StonesHeight + dt * StonesHeight * c);
            End;
            If j = FieldRows - 1 Then
              If field[i, j] >= 0 Then Begin
                glVertex2f(StonesWidth, j * StonesHeight + StonesHeight + dt * StonesHeight * c - 1);
                glVertex2f(0, j * StonesHeight + StonesHeight + dt * StonesHeight * c - 1);
              End;
          End
          Else If field[i, j] >= 0 Then Begin
            glVertex2f(0, j * StonesHeight + dt * StonesHeight * c);
            glVertex2f(StonesWidth, j * StonesHeight + dt * StonesHeight * c);
          End;
          // Senkrechte Liniean
          If i > 0 Then Begin
            If (field[i, j] <> field[i - 1, j]) And ((Field[i - 1, j] >= 0) Or (Field[i, j] >= 0)) Then Begin
              (*
                Die Senkrechten Linien dürfen nur an Steine die nicht fallen
                oj, und oc geben dabei an ob in der Vorherigen Spalte ein Stein viel, und wenn ja ab welcher Höhe
              *)
              If (oc = 0) Or (j > oj) Then Begin // Korrecktur Traceline
                glVertex2f(0, j * StonesHeight + dt * StonesHeight * c);
                glVertex2f(0, j * StonesHeight + StonesHeight + dt * StonesHeight * c);
              End;
            End;
            If i = FieldColumns - 1 Then
              If Field[i, j] >= 0 Then Begin
                glVertex2f(StonesWidth, j * StonesHeight + dt * StonesHeight * c);
                glVertex2f(StonesWidth, j * StonesHeight + StonesHeight + dt * StonesHeight * c);
              End;
          End
          Else If Field[i, j] >= 0 Then Begin
            glVertex2f(1, j * StonesHeight + dt * StonesHeight * c);
            glVertex2f(1, j * StonesHeight + StonesHeight + dt * StonesHeight * c);
          End;
          glend;
        End
        Else Begin
          // Ein rahmen um das Komplette Feld
          If field[i, j] >= 0 Then Begin
            glbegin(GL_LINE_LOOP);
            glVertex2f(1, j * StonesHeight + dt * StonesHeight * c);
            glVertex2f(StonesWidth, j * StonesHeight + dt * StonesHeight * c);
            glVertex2f(StonesWidth, j * StonesHeight + StonesHeight + dt * StonesHeight * c - 1);
            glVertex2f(0, j * StonesHeight + StonesHeight + dt * StonesHeight * c - 1);
            glend;
          End;
        End;
      End; // C = 0
    End;
    glpopmatrix;
    gltranslatef(StonesWidth, 0, 0);
  End;
  If F_XsTextured Then
    gldisable(gl_Blend);
  glpopmatrix;
  // Wenn es nichts zum runterfallen gibt.
  If b Then Begin
    DropStartTime := GetTickCount - 2 * DropStoneDelay;
  End;
End;

Procedure StringToStream(Const Stream: TStream; Value: String);
Var
  i: Integer;
  b: byte;
Begin
  i := length(value);
  stream.write(i, sizeof(i));
  For i := 1 To length(value) Do Begin
    b := ord(value[i]);
    stream.write(b, sizeof(b));
  End;
End;

Function StringFromStream(Const Stream: TStream): String;
Var
  i: Integer;
  b: Byte;
Begin
  i := 0; // beruhigt den Compiler
  b := 0; // beruhigt den Compiler
  result := '';
  stream.read(i, sizeof(i));
  setlength(result, i);
  For i := 1 To Length(result) Do Begin
    stream.read(b, sizeof(b));
    result[i] := chr(b);
  End;
End;

{ TUndoStack }

Constructor TUndoStack.create;
Begin
  Inherited;
  Fstack := Nil;
  Fstack2 := Nil;
End;

Destructor TUndoStack.destroy;
Begin
  Clear;
End;

Procedure TUndoStack.Clear;
Var
  i, j: Integer;
Begin
  For i := 0 To High(fstack) Do Begin
    For j := 0 To High(fstack[i]) Do
      setlength(fstack[i, j], 0);
    setlength(fstack[i], 0);
  End;
  setlength(fstack, 0);
  setlength(fstack2, 0);
End;

Procedure TUndoStack.Push;
Var
  i, j: Integer;
Begin
  setlength(fstack2, high(fstack2) + 2);
  fstack2[high(fstack2)] := GameScores;
  setlength(fstack, high(fstack) + 2);
  setlength(fstack[high(fstack)], high(field) + 1, high(field[0]) + 1);
  For i := 0 To high(field) Do
    For j := 0 To High(field[0]) Do
      fstack[high(fstack)][i, j] := field[i, j];
End;

Function TUndoStack.Pop: Boolean;
Var
  i, j: Integer;
Begin
  If assigned(fstack) Then Begin
    result := true;
    GameScores := fstack2[high(fstack2)];
    setlength(fstack2, high(fstack2));
    For i := 0 To high(field) Do
      For j := 0 To High(field[0]) Do
        field[i, j] := fstack[high(fstack)][i, j];
    setlength(fstack[high(fstack)], 0, 0);
    setlength(fstack, high(fstack));
  End
  Else
    result := false;
End;

{ THighscoreEngine }

Constructor THighscoreEngine.Create;
Var
  i: Integer;
Begin
  Inherited;
  FHighscoreList := Nil;
  FChanged := false;
  RandSeed := 83485;
  For i := 0 To High(pw) Do
    pw[i] := random(high(integer));
End;

Destructor THighscoreEngine.Destroy;
Begin
  clearall;
End;

Procedure THighscoreEngine.Clearall;
Begin
  If high(FHighscoreList) <> -1 Then FChanged := True;
  setlength(FHighscoreList, 0);
End;

Procedure THighscoreEngine.Clear(Width, Height, Colors: Integer;
  Spezials: Boolean);
Var
  i, j: Integer;
Begin
  i := high(FHighscoreList);
  While i >= 0 Do Begin
    If (FHighscoreList[i].Width = Width) And
      (FHighscoreList[i].Height = Height) And
      (FHighscoreList[i].Colors = Colors) And
      (FHighscoreList[i].Spezials = Spezials) Then Begin
      For j := i To High(FHighscoreList) - 1 Do
        FHighscoreList[j] := FHighscoreList[j + 1];
      setlength(FHighscoreList, high(FHighscoreList));
      Fchanged := True;
    End;
    dec(i);
  End;
End;

Procedure THighscoreEngine.AddScore(Width, Height, Colors: Integer;
  Spezials: Boolean; Name: String; Stones, Scores: Integer);
Begin
  FChanged := True;
  setlength(FHighscoreList, high(FHighscoreList) + 2);
  FHighscoreList[high(FHighscoreList)].Width := width;
  FHighscoreList[high(FHighscoreList)].Height := Height;
  FHighscoreList[high(FHighscoreList)].Colors := Colors;
  FHighscoreList[high(FHighscoreList)].Spezials := Spezials;
  FHighscoreList[high(FHighscoreList)].Name := Name;
  FHighscoreList[high(FHighscoreList)].Stones := Stones;
  FHighscoreList[high(FHighscoreList)].Scores := Scores;
End;

Procedure THighscoreEngine.SaveToFile(Filename: String);
Var
  f: TFileStream;
  i, j, k: Integer;
  pwc: Integer;
  s: String;
  b: byte;
Begin
  If (High(FHighscoreList) <> -1) And FChanged Then Begin
    pwc := 0;
    f := TFileStream.create(Filename, fmcreate Or fmopenwrite);
    f.write(version, sizeof(Version));
    i := High(FHighscoreList) + 1;
    f.Write(i, sizeof(i));
    For i := 0 To High(FHighscoreList) Do Begin
      k := FHighscoreList[i].Width Xor pw[pwc];
      f.Write(k, sizeof(k));
      pwc := (pwc + 1) Mod (high(pw) + 1);

      k := FHighscoreList[i].Height Xor pw[pwc];
      f.Write(k, sizeof(k));
      pwc := (pwc + 1) Mod (high(pw) + 1);

      k := FHighscoreList[i].Colors Xor pw[pwc];
      f.Write(k, sizeof(k));
      pwc := (pwc + 1) Mod (high(pw) + 1);
      f.Write(FHighscoreList[i].Spezials, sizeof(FHighscoreList[i].Spezials));
      k := length(FHighscoreList[i].Name);
      f.Write(k, sizeof(k));
      s := FHighscoreList[i].Name;
      For j := 1 To Length(s) Do Begin
        b := ord(s[j]);
        k := b Xor pw[pwc];
        f.write(k, sizeof(k));
        pwc := (pwc + 1) Mod (high(pw) + 1);
      End;
      k := FHighscoreList[i].Stones Xor pw[pwc];
      f.Write(k, sizeof(k));
      pwc := (pwc + 1) Mod (high(pw) + 1);
      k := FHighscoreList[i].Scores Xor pw[pwc];
      f.Write(k, sizeof(k));
      pwc := (pwc + 1) Mod (high(pw) + 1);
    End;
    f.free;
    FChanged := false;
  End
    // Wenn die Highscores Leer sind
  Else If FChanged Then Begin
    DeleteFile(Filename);
  End;
End;

Procedure THighscoreEngine.LoadFromFile(Filename: String);
Var
  pwc, i, j, k: Integer;
  f: TFilestream;
  s: String;
  b: Byte;
  ss: Single;
Begin
  clearall;
  FChanged := false;
  If FileExists(Filename) Then Begin
    f := TFileStream.create(filename, fmopenread);
    ss := 0;
    f.read(ss, sizeof(ss));
    If ss <> version Then Begin
      f.free;
      exit;
    End;
    i := 0;
    f.read(i, sizeof(i));
    setlength(FHighscoreList, i);
    pwc := 0;
    k := 0; // Beruhigt den Compiler
    For i := 0 To High(FHighscoreList) Do Begin
      f.read(k, sizeof(k));
      FHighscoreList[i].Width := k Xor pw[pwc];
      pwc := (pwc + 1) Mod (high(pw) + 1);

      f.read(k, sizeof(k));
      FHighscoreList[i].Height := k Xor pw[pwc];
      pwc := (pwc + 1) Mod (high(pw) + 1);

      f.read(k, sizeof(k));
      FHighscoreList[i].Colors := k Xor pw[pwc];
      pwc := (pwc + 1) Mod (high(pw) + 1);

      f.read(FHighscoreList[i].Spezials, sizeof(FHighscoreList[i].Spezials));
      f.read(k, sizeof(k));
      s := '';
      setlength(s, k);

      For j := 1 To Length(s) Do Begin
        f.read(k, sizeof(k));
        b := byte(k Xor pw[pwc]);
        s[j] := chr(b);
        pwc := (pwc + 1) Mod (high(pw) + 1);
      End;
      FHighscoreList[i].Name := s;

      f.read(k, sizeof(k));
      FHighscoreList[i].Stones := k Xor pw[pwc];
      pwc := (pwc + 1) Mod (high(pw) + 1);

      f.read(k, sizeof(k));
      FHighscoreList[i].Scores := k Xor pw[pwc];
      pwc := (pwc + 1) Mod (high(pw) + 1);
    End;
    f.free;
  End;
End;

Function THighscoreEngine.ShowHighscores(Width, Height, Colors: Integer;
  Spezials: Boolean; Mode: TScoreMode): THighScoreResult;

Type
  TSSubArr = Record
    Name: String;
    points: Integer;
  End;

  TSubArr = Array Of TSSubArr;

Var
  a: TSubArr;

  Procedure Quick(li, re: integer);
  Var
    h: TSSubArr;
    p, l, r: Integer;
  Begin
    If Li < Re Then Begin
      p := a[Trunc((li + re) / 2)].points; // Auslesen des Pivo Elementes
      l := Li;
      r := re;
      While l < r Do Begin
        While a[l].points < p Do
          inc(l);
        While a[r].points > p Do
          dec(r);
        If L <= R Then Begin
          h := a[l];
          a[l] := a[r];
          a[r] := h;
          inc(l);
          dec(r);
        End;
      End;
      quick(li, r);
      quick(l, re);
    End;
  End;

Var
  av, i: Integer;
Begin
  // Init
  For i := 0 To High(result.Results) Do Begin
    result.Results[i].Name := 'No Entry';
    If Mode = smLow Then
      result.Results[i].Points := width * Height
    Else
      result.Results[i].Points := 0;
  End;
  // Suchen der Beteiligten Spiele
  a := Nil;
  For i := 0 To high(FHighscoreList) Do
    If (FHighscoreList[i].Width = Width) And
      (FHighscoreList[i].Height = Height) And
      (FHighscoreList[i].Colors = Colors) And
      (FHighscoreList[i].Spezials = Spezials) Then Begin
      setlength(a, high(a) + 2);
      a[high(a)].Name := FHighscoreList[i].Name;
      If Mode = smLow Then
        a[high(a)].points := FHighscoreList[i].Stones
      Else
        a[high(a)].points := FHighscoreList[i].Scores;
    End;
  // Sortieren nach Punkten
  Quick(0, High(a));
  // Auslesen der Besten 10
  If Mode = smLow Then Begin
    For i := 0 To Min(9, High(a)) Do Begin
      result.Results[i].name := a[i].Name;
      result.Results[i].Points := a[i].points;
    End;
  End
  Else Begin
    For i := 0 To Min(9, High(a)) Do Begin
      result.Results[i].name := a[High(a) - i].Name;
      result.Results[i].Points := a[High(a) - i].points;
    End;
  End;
  // Berechnen Average, GesamtCount
  av := 0;
  For i := 0 To High(a) Do
    av := av + a[i].points;
  av := trunc(av / max((high(a) + 1), 1));
  If Mode = smLow Then Begin
    If av <= 0 Then av := width * Height;
  End
  Else Begin
    If av <= 0 Then av := 0;
  End;
  result.averagePoints := av;
  result.TotalGames := high(a) + 1;
  // Freigeben
  setlength(a, 0);
End;

Initialization

  AppPath := IncludeTrailingPathDelimiter(extractfilepath(Paramstr(0)));
  undostack := TUndoStack.create;
  Highscore := THighscoreEngine.create;

Finalization

  Highscore.free;
  Highscore := Nil;
  undostack.free;
  undostack := Nil;

End.

