(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of 2048                                                  *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit u2048;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Graphics, uopengl_truetype_font
{$IFDEF Linux}
  , lclintf
{$ENDIF}
  ;

Type

  TPieceColor = Record
    FontColor: TColor;
    BackColor: TColor;
  End;

  TDir = (dUp, dDown, dLeft, dRight);
  TBoardState = (bsIdle, bsfalling);

Const

  BoardSize: integer = 4 - 1; // Entspricht einem 4x4 Feld

  FallTime: integer = 250; // Zeit in ms wie Lange es dauern soll, bis die "Fall" Animation beendet ist.

  BoardBackColor: TColor = 160 * 65536 + 173 * 256 + 187;

  PieceColors: Array[0..12] Of TPieceColor = (
    (FontColor: 180 * 65536 + 192 * 256 + 205; BackColor: 180 * 65536 + 192 * 256 + 205), // 2^0 = 1 // -- Entspricht aber "0"
    (FontColor: 101 * 65536 + 110 * 256 + 119; BackColor: 218 * 65536 + 228 * 256 + 238), // 2^1 = 2
    (FontColor: 101 * 65536 + 110 * 256 + 119; BackColor: 200 * 65536 + 224 * 256 + 237), // 2^2 = 4
    (FontColor: 242 * 65536 + 246 * 256 + 249; BackColor: 121 * 65536 + 177 * 256 + 242), // 2^3 = 8
    (FontColor: 242 * 65536 + 246 * 256 + 249; BackColor: 099 * 65536 + 149 * 256 + 245), // 2^4 = 16
    (FontColor: 242 * 65536 + 246 * 256 + 249; BackColor: 095 * 65536 + 124 * 256 + 246), // 2^5 = 32
    (FontColor: 242 * 65536 + 246 * 256 + 249; BackColor: 059 * 65536 + 094 * 256 + 246), // 2^6 = 64
    (FontColor: 242 * 65536 + 246 * 256 + 249; BackColor: 114 * 65536 + 207 * 256 + 237), // 2^7 = 128
    (FontColor: 242 * 65536 + 246 * 256 + 249; BackColor: 097 * 65536 + 204 * 256 + 237), // 2^8 = 256
    (FontColor: 242 * 65536 + 246 * 256 + 249; BackColor: 080 * 65536 + 200 * 256 + 237), // 2^9 = 512
    (FontColor: 242 * 65536 + 246 * 256 + 249; BackColor: 063 * 65536 + 197 * 256 + 237), // 2^10 = 1024
    (FontColor: 242 * 65536 + 246 * 256 + 249; BackColor: 046 * 65536 + 194 * 256 + 238), // 2^11 = 2048
    (FontColor: 242 * 65536 + 246 * 256 + 249; BackColor: 051 * 65536 + 058 * 256 + 061) // 2^12 = 4096 ++
    );

Type
  { TPiece }

  TPiece = Class
  private
  public
    Width: integer;
    Height: integer;
    Value: int64;
    FallingDistanze: TPoint; // Richtung in Piece Breiten
    Constructor Create();
    Procedure Render;
    Procedure RenderFalling(Progress: Single); // in % wie viel der "FallStrecke" rum ist
  End;

  { TBoard }

  TBoard = Class
  private
    fFallDir: TDir;
    fFallTick: int64;
    fInternalState: TBoardState;
    fWidth: integer;
    fHeight: integer;
    BoardAddPoints: int64; // Aufsammeln Zukünftiger Punkte (ist normalerweise = 0)
    sboard: Array Of Array Of Record
      value: int64;
      alreadyused: boolean;
    End;
    Field: Array Of Array Of TPiece;
    Procedure EvalFalling();
    Procedure ReInit(Value: integer);
  public
    Points: int64;
    Constructor Create();
    Destructor Destroy; override;
    Procedure restart;
    Procedure Resize(w, h: integer);
    Procedure Render;
    Procedure StartFalling(Dir: TDir);
    Function IsFalling(): Boolean;
    Procedure SaveToStream(Const Stream: TSTream);
    Procedure LoadFromStream(Const Stream: TSTream);
    Function IsFull: Boolean;
    Function Index(i, j: integer): int64;
  End;

Var
  OpenGlFont: TOpenGL_TrueType_Font = Nil;

Procedure CreateFont(Filename: String); // initialisiert font

Implementation

Uses dglopengl, math;

Procedure glColor(value: TColor);
Begin
  glColor3ub(value And $FF, (value And $FF00) Shr 8, (value And $FF0000) Shr 16);
End;

(* Gibt fast den 2er Logarithmus aus *)

Function Log2(value: integer): integer;
Begin
  result := -1;
  Case value Of
    0: result := 0;
    2: result := 1;
    4: result := 2;
    8: result := 3;
    16: result := 4;
    32: result := 5;
    64: result := 6;
    128: result := 7;
    256: result := 8;
    512: result := 9;
    1024: result := 10;
    2048: result := 11;
  End;
  If value >= 4096 Then result := 12;
End;

Procedure CreateFont(Filename: String);
Begin
  If Not assigned(OpenGlFont) Then Begin
    OpenGlFont := TOpenGL_TrueType_Font.Create();
    OpenGlFont.LoadfromFile(Filename);
  End;
End;

{ TPiece }

Constructor TPiece.Create;
Begin
  Inherited create;
  Width := 10;
  Height := 10;
  value := 0;
End;

Procedure TPiece.Render;
Var
  ColorIndex: integer;
  Border: Single;
  s: String;
Begin
  glPushMatrix;
  If value <> 0 Then
    glTranslatef(0, 0, 0.25);
  ColorIndex := Log2(value);
  glColor(PieceColors[ColorIndex].BackColor);
  // Der Hintergrund
  Border := Width / 20;
  glbegin(GL_QUADS);
  glVertex3f(Border, Border, 0);
  glVertex3f(Width - Border, Border, 0);
  glVertex3f(Width - Border, Height - Border, 0);
  glVertex3f(Border, Height - Border, 0);
  glend;
  // Value
  If value <> 0 Then Begin
    If width > 10 Then Begin
      glTranslatef(0, 0, 0.125);
      OpenGlFont.Color := PieceColors[ColorIndex].FontColor;
      s := IntToStr(value);
      OpenGlFont.Size := (Width - 10) / max(length(s), 3);
      OpenGlFont.Textout(
        round((Width - OpenGlFont.TextWidth(s)) / 2),
        round((height - OpenGlFont.TextHeight(s)) / 2),
        s
        );
    End;
  End;
  glPopMatrix;
End;

Procedure TPiece.RenderFalling(Progress: Single);
Var
  i: integer;
Begin
  // Den Hintergrund des Tiles Festhalten
  If value <> 0 Then Begin
    i := value;
    value := 0;
    render;
    value := i;
  End;
  glPushMatrix;
  glTranslatef(FallingDistanze.x * Width * Progress, FallingDistanze.y * Height * Progress, 0);
  Render;
  glPopMatrix;
End;

{ TBoard }

Procedure TBoard.EvalFalling;
Var
  i: Integer;
  j: Integer;
  b: Boolean;
Begin
  fInternalState := bsIdle;
  // 1. Zusammenrechnen aller Teile die Rummgefallen sind
  Points := points + BoardAddPoints;
  BoardAddPoints := 0;
  // 2. Setzen der neuen Werte
  For i := 0 To BoardSize Do Begin
    For j := 0 To BoardSize Do Begin
      Field[i, j].Value := sboard[i, j].value;
    End;
  End;
  // 3. Erzeugen eines neuen Teils "Rein Zufällig"
  b := true;
  While b Do Begin
    i := random(BoardSize + 1);
    j := random(BoardSize + 1);
    If (field[i, j].value = 0) And b Then Begin
      field[i, j].value := 2;
      b := false;
    End;
    // Fragwürdig ob das Rein soll ??
    If (field[i, j].value = 2) And b Then Begin
      BoardAddPoints := 4;
      field[i, j].value := 4;
      b := false;
    End;
  End;
End;

Procedure TBoard.ReInit(Value: integer);
Var
  i: Integer;
  j: Integer;
Begin
  // Altes Freigeben
  For i := 0 To BoardSize Do Begin
    For j := 0 To BoardSize Do Begin
      field[i, j].free;
    End;
  End;
  // Neues Erzeugen
  BoardSize := value;
  setlength(field, BoardSize + 1, BoardSize + 1);
  setlength(sboard, Value + 1, Value + 1);
  For i := 0 To BoardSize Do Begin
    For j := 0 To BoardSize Do Begin
      field[i, j] := TPiece.Create;
    End;
  End;
  Resize(fWidth, fHeight);
End;

Procedure TBoard.restart;
Var
  i: Integer;
  j: Integer;
Begin
  points := 0;
  BoardAddPoints := 0;
  fInternalState := bsIdle;
  For i := 0 To BoardSize Do Begin
    For j := 0 To BoardSize Do Begin
      field[i, j].value := 0;
    End;
  End;
  field[random(BoardSize + 1), random(BoardSize + 1)].Value := 2;
End;

Constructor TBoard.Create;
Var
  i: Integer;
  j: Integer;
Begin
  Inherited create;
  Points := 0;
  BoardAddPoints := 0;
  fInternalState := bsIdle;
  setlength(field, BoardSize + 1, BoardSize + 1);
  setlength(sboard, BoardSize + 1, BoardSize + 1);
  For i := 0 To BoardSize Do Begin
    For j := 0 To BoardSize Do Begin
      field[i, j] := TPiece.Create;
    End;
  End;
  field[random(BoardSize + 1), random(BoardSize + 1)].Value := 2;

  // Diverse Problemfälle

  //field[0, 2].value := 4;
  //field[1, 2].value := 4;
  //field[2, 2].value := 4;
  //field[3, 2].value := 4;

  //field[0, 2].value := 4;
  //field[1, 2].value := 4;
  //field[2, 2].value := 8;
  //field[3, 2].value := 16;
End;

Destructor TBoard.Destroy;
Var
  i: Integer;
  j: Integer;
Begin
  For i := 0 To BoardSize Do
    For j := 0 To BoardSize Do
      field[i, j].free;
  setlength(field, 0, 0);
  setlength(sboard, 0, 0);
End;

Procedure TBoard.Resize(w, h: integer);
Var
  i: Integer;
  j: Integer;
  d: integer;
Begin
  fWidth := w;
  fHeight := h;
  d := trunc((min(w, h) - 30) / (BoardSize + 1));
  For i := 0 To BoardSize Do
    For j := 0 To BoardSize Do Begin
      field[i, j].Width := d;
      field[i, j].Height := d;
    End;
End;

Procedure TBoard.Render;
Var
  i, j: integer;
  b: boolean;
  t: int64;
  s: Single;
Begin
  b := IsFalling();
  If b Then Begin
    t := GetTickCount64;
    t := (t - fFallTick);
    s := t / FallTime;
    // Das Fallen ist beedent
    If s >= 1 Then Begin
      b := false;
      EvalFalling;
    End;
  End;
  glPushMatrix;
  glTranslatef(fWidth / 2 - ((BoardSize + 1) / 2) * Field[0, 0].Width, fHeight / 2 - ((BoardSize + 1) / 2) * Field[0, 0].Height, -0.25);
  glColor(BoardBackColor);
  glbegin(GL_QUADS);
  glVertex3f(0, 0, 0);
  glVertex3f(Field[0, 0].Width * (BoardSize + 1), 0, 0);
  glVertex3f(Field[0, 0].Width * (BoardSize + 1), Field[0, 0].Height * (BoardSize + 1), 0);
  glVertex3f(0, Field[0, 0].Height * (BoardSize + 1), 0);
  glend;
  glTranslatef(0, 0, 0.25);
  For j := 0 To BoardSize Do Begin
    glPushMatrix;
    For i := 0 To BoardSize Do Begin
      If b Then Begin
        Field[i, j].RenderFalling(s);
      End
      Else Begin
        Field[i, j].Render;
      End;
      glTranslatef(Field[0, 0].Width, 0, 0);
    End;
    glPopMatrix;
    glTranslatef(0, Field[0, 0].Height, 0);
  End;
  glPopMatrix;
End;

Procedure TBoard.StartFalling(Dir: TDir);
Var
  k, i, j, z: integer;
  b: Boolean;
Begin
  If Not IsFalling() Then Begin
    b := false;
    // Arbeitskopie erstellen
    For i := 0 To BoardSize Do
      For j := 0 To BoardSize Do Begin
        sboard[i, j].value := field[i, j].Value;
        sboard[i, j].alreadyused := false;
        field[i, j].FallingDistanze := point(0, 0);
      End;
    // Berechnen der Aus zu führenden Bewegungen und der Resultierenden Punkte
    Case dir Of
      dup: Begin
          For i := 0 To BoardSize Do Begin
            For j := 0 To BoardSize Do Begin
              If sboard[i, j].value <> 0 Then Begin
                k := 0;
                For z := 1 To BoardSize Do Begin
                  If (j - z >= 0) Then Begin
                    If ((sboard[i, j - z].value = 0) Or (sboard[i, j - z].value = sboard[i, j].value)) And (Not sboard[i, j - z].alreadyused) Then Begin
                      k := z;
                    End
                    Else Begin
                      break;
                    End;
                  End;
                End;
                If k <> 0 Then Begin
                  b := true;
                  Field[i, j].FallingDistanze := point(0, -k);
                  If sboard[i, j - k].value <> 0 Then Begin
                    BoardAddPoints := BoardAddPoints + sboard[i, j - k].value + sboard[i, j].value;
                    sboard[i, j - k].alreadyused := true;
                  End;
                  sboard[i, j - k].value := sboard[i, j - k].value + sboard[i, j].value;
                  sboard[i, j].value := 0;
                End;
              End;
            End;
          End;
        End;
      ddown: Begin
          For i := 0 To BoardSize Do Begin
            For j := BoardSize Downto 0 Do Begin
              If sboard[i, j].value <> 0 Then Begin
                k := 0;
                For z := 1 To BoardSize Do Begin
                  If (j + z <= BoardSize) Then Begin
                    If ((sboard[i, j + z].value = 0) Or (sboard[i, j + z].value = sboard[i, j].value)) And (Not sboard[i, j + z].alreadyused) Then Begin
                      k := z;
                    End
                    Else Begin
                      break;
                    End;
                  End;
                End;
                If k <> 0 Then Begin
                  b := true;
                  Field[i, j].FallingDistanze := point(0, k);
                  If sboard[i, j + k].value <> 0 Then Begin
                    sboard[i, j + k].alreadyused := true;
                    BoardAddPoints := BoardAddPoints + sboard[i, j + k].value + sboard[i, j].value;
                  End;
                  sboard[i, j + k].value := sboard[i, j + k].value + sboard[i, j].value;
                  sboard[i, j].value := 0;
                End;
              End;
            End;
          End;
        End;
      dright: Begin
          For j := 0 To BoardSize Do Begin
            For i := BoardSize Downto 0 Do Begin
              If sboard[i, j].value <> 0 Then Begin
                k := 0;
                For z := 1 To BoardSize Do Begin
                  If (i + z <= BoardSize) Then Begin
                    If ((sboard[i + z, j].value = 0) Or (sboard[i + z, j].value = sboard[i, j].value)) And (Not sboard[i + z, j].alreadyused) Then Begin
                      k := z;
                    End
                    Else Begin
                      break;
                    End;
                  End;
                End;
                If k <> 0 Then Begin
                  b := true;
                  Field[i, j].FallingDistanze := point(k, 0);
                  If sboard[i + k, j].value <> 0 Then Begin
                    BoardAddPoints := BoardAddPoints + sboard[i + k, j].value + sboard[i, j].value;
                    sboard[i + k, j].alreadyused := true;
                  End;
                  sboard[i + k, j].value := sboard[i + k, j].value + sboard[i, j].value;
                  sboard[i, j].value := 0;
                End;
              End;
            End;
          End;
        End;
      dleft: Begin
          For j := 0 To BoardSize Do Begin
            For i := 0 To BoardSize Do Begin
              If sboard[i, j].value <> 0 Then Begin
                k := 0;
                For z := 1 To BoardSize Do Begin
                  If (i - z >= 0) Then Begin
                    If ((sboard[i - z, j].value = 0) Or (sboard[i - z, j].value = sboard[i, j].value)) And (Not sboard[i - z, j].alreadyused) Then Begin
                      k := z;
                    End
                    Else Begin
                      break;
                    End;
                  End;
                End;
                If k <> 0 Then Begin
                  b := true;
                  Field[i, j].FallingDistanze := point(-k, 0);
                  If sboard[i - k, j].value <> 0 Then Begin
                    sboard[i - k, j].alreadyused := true;
                    BoardAddPoints := BoardAddPoints + sboard[i - k, j].value + sboard[i, j].value;
                  End;
                  sboard[i - k, j].value := sboard[i - k, j].value + sboard[i, j].value;
                  sboard[i, j].value := 0;
                  sboard[i, j].alreadyused := false;
                End;
              End;
            End;
          End;
        End;
    End;

    // Starten der Sequenz, aber nur, wenn sich auch was bewegt hat.
    If b Then Begin
      fFallDir := Dir;
      fFallTick := GetTickCount64;
      fInternalState := bsFalling;
    End;
  End;
End;

Function TBoard.IsFalling: Boolean;
Begin
  result := fInternalState = bsFalling;
End;

Procedure TBoard.SaveToStream(Const Stream: TSTream);
Var
  i, j: integer;
Begin
  stream.Write(BoardSize, sizeof(BoardSize));
  stream.Write(Points, sizeof(points));
  stream.Write(BoardAddPoints, sizeof(BoardAddPoints));
  For i := 0 To BoardSize Do Begin
    For j := 0 To BoardSize Do Begin
      stream.Write(Field[i, j].value, sizeof(Field[i, j].value));
    End;
  End;
End;

Procedure TBoard.LoadFromStream(Const Stream: TSTream);
Var
  v, i, j: integer;
Begin
  v := 0;
  stream.read(v, sizeof(BoardSize));
  ReInit(v);
  stream.read(Points, sizeof(points));
  stream.read(BoardAddPoints, sizeof(BoardAddPoints));
  For i := 0 To BoardSize Do Begin
    For j := 0 To BoardSize Do Begin
      stream.read(Field[i, j].value, sizeof(Field[i, j].value));
    End;
  End;
End;

Function TBoard.IsFull: Boolean;
Var
  i, j: integer;
Begin
  result := true;
  // Gibt es noch freie Plätze
  For i := 0 To BoardSize Do Begin
    For j := 0 To BoardSize Do Begin
      If Field[i, j].value = 0 Then Begin
        result := false;
        exit;
      End;
    End;
  End;
  // Keine Freien Plätze, dann gibt es vielleicht noch Gleiche Benachbarte Zahlen ?
  For j := 0 To BoardSize Do Begin // Prüfung Waagrechte
    For i := 1 To BoardSize Do Begin
      If field[i, j].value = field[i - 1, j].value Then Begin
        result := false;
        exit;
      End;
    End;
  End;
  For i := 0 To BoardSize Do Begin // Prüfung Senkrechte
    For j := 1 To BoardSize Do Begin
      If field[i, j].value = field[i, j - 1].value Then Begin
        result := false;
        exit;
      End;
    End;
  End;
End;

Function TBoard.Index(i, j: integer): int64;
Begin
  result := Field[i, j].value;
End;

End.

