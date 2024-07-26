(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Bridge builder                                        *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit umap;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, uphysik, uopengl_graphikengine, uvectormath;

Type

  TSimState = (ssIdle, ssCountdown);

  TMapCollider = Record
    p1, p2, p3: TPoint;
  End;

  { TMap }

  TMap = Class
  private
    fBackTexFilename: String; // Wird Relativ gespeichert, ist zur Laufzeit aber Absolut !
    fBackTex: TGraphikItem;
    fFixedBoltTex: TGraphikItem;
    fUserBolts: Array Of TVector2; // Durch den User angefügte Punkte -> werden nicht gespeichert
    fColliders: Array Of TMapCollider;
    fDeadZone: Array Of TMapCollider;
    fFixedEdges: Array Of TPoint;
    fUserEdges: Array Of TPoint;

    fLastTick: QWord;
    fEngine: TSpringEngine;
    fSimState: TSimState;
    fsimStartTime: QWord;

    Procedure AdjustFinalZone;
    Function getStartPoint: TVector2;
    Procedure SetStartPoint(AValue: TVector2);

  public
    // TODO: Interface aufräumen !
    Offset, Dim: TPoint;
    Filename: String;

    ShowGrid: Boolean;
    ShowCollider: Boolean;
    ShowDeadZones: Boolean;

    FinalZone: Array[0..3] Of TVector2;
    EditMode: Boolean;

    fFixedBolts: Array Of TVector2;
    Zoom: Single;

    Property StartPoint: TVector2 read getStartPoint write SetStartPoint;

    Constructor Create(); virtual;
    Destructor Destroy(); override;

    Procedure Render();

    // Game Sachen
    Procedure Reset();
    Function FindPointIndex(x, y: integer): integer;
    Function AddPoint(x, y: integer): Integer;
    Function GetPointByIndex(Index: integer): TVector2;
    Procedure ToggleUserEdge(StartNodeIndex, EndNodeIndex: Integer);
    Procedure CheckAndMaybeRemovePoint(NodeIndex: integer);

    Procedure StartSim;
    Procedure StopSim;

    // Editor Sachen
    Function AddFixedBolt(x, y: integer): integer;
    Procedure DelFixedBolt(x, y: integer);
    Function FindFixedBolt(x, y: integer): integer;

    Procedure ToggleFixedEdge(StartNodeIndex, EndNodeIndex: Integer);

    Procedure ToggleCollider(p1, p2, p3: TPoint);
    Function FindColliderPoint(x, y: integer): Tpoint;
    Procedure SetColliderpoint(Col, P: TPoint);

    Procedure ToggleDeadzone(p1, p2, p3: TPoint);
    Function FindDeadzonePoint(x, y: integer): Tpoint;
    Procedure SetDeadzonepoint(Col, P: TPoint);

    Function findFinalZoneIndex(x, y: integer): integer;

    Procedure Clear; // Löscht einfach alles auf der Karte
    Procedure SaveToFile(Const aFilename: String);
    Procedure LoadFromFile(Const aFilename: String);

    Procedure LoadBackTexture(Const aFilename: String);
    Procedure InitOpenGL;
  End;

Implementation

Uses dglOpenGL, IniFiles, ubridge_builder, uOpenGL_ASCII_Font;

{ TMap }

Procedure TMap.AdjustFinalZone;
  Function Fix(v: TVector2): TVector2;
  Var
    x, y: integer;
  Begin
    x := round(v.x);
    y := round(v.y);
    result := v2(
      x - x Mod GridSize + GridSize Div 2,
      y - y Mod GridSize + GridSize Div 2
      );
  End;

Begin
  FinalZone[0] := fix(v2(dim.x - 100, dim.Y - 100));
  FinalZone[1] := fix(v2(dim.x - 50, dim.Y - 100));
  FinalZone[2] := fix(v2(dim.x - 50, dim.Y - 50));
  FinalZone[3] := fix(v2(dim.x - 100, dim.Y - 50));
End;

Function TMap.getStartPoint: TVector2;
Begin
  result := fFixedBolts[0];
End;

Procedure TMap.SetStartPoint(AValue: TVector2);
Begin
  fFixedBolts[0] := AValue;
End;

Constructor TMap.Create;
Begin
  Inherited Create;
  EditMode := false;
  fBackTex.Image := -1;
  fFixedBoltTex.Image := -1;
  ShowGrid := false;
  ShowCollider := false;
  fEngine := Nil;
  Clear;
End;

Destructor TMap.Destroy;
Begin
  Clear;
End;

Procedure TMap.Render;
Var
  i: Integer;
  v: TVector2;
  n: QWord;
  s: String;
Begin
  If assigned(fEngine) Then Begin
    n := GetTickCount64;
    fEngine.Simulate((n - fLastTick) / 1000);
    fLastTick := n;
  End;
  glColor4f(1, 1, 1, 1);
  glPushMatrix;
  glTranslatef(-Offset.x, -Offset.y, 0);
  glScalef(zoom, zoom, zoom);
  If fBackTex.Image <> -1 Then Begin
    RenderQuad(0, 0, fBackTex);
  End;
  If fSimState = ssCountdown Then Begin
    n := GetTickCount64 - fsimStartTime;
    s := inttostr(n Div 1000);
    glBindTexture(GL_TEXTURE_2D, 0);
    OpenGL_ASCII_Font.ColorV3 := v3(1, 1, 1);
    // Da die Karte ja eigentlich gezoomt und mit Offset ist, muss für die "gemittelte" Zeitanzeige, dass hier alles "zurück" gerechnet werden
    glPushMatrix();
    glTranslatef(Offset.x / zoom, Offset.y / zoom, 0);
    OpenGL_ASCII_Font.Size := OpenGL_ASCII_Font.Size * 2 / zoom;
    glTranslatef((ScreenWidth - OpenGL_ASCII_Font.TextWidth(s)) / (2 * zoom), 15 / Zoom, 0);
    OpenGL_ASCII_Font.Textout(0, 0, s);
    OpenGL_ASCII_Font.Size := OpenGL_ASCII_Font.Size / (2 / zoom);
    glPopMatrix();
    // TODO: Zeit Dynamisch einstellen
    If n >= 5000 Then Begin
      // TODO: Nun geht der Belastungstest los ..

      hier gehts weiter ..

      fSimState := ssIdle;
    End;
  End;
  If ShowGrid Then Begin
    glPushMatrix;
    glBindTexture(GL_TEXTURE_2D, 0);
    glColor3f(0.5, 0.5, 0.5);
    glBegin(GL_LINES);
    For i := 0 To Dim.x Div GridSize Do Begin
      glVertex2f(GridSize * i, 0);
      glVertex2f(GridSize * i, dim.Y);
    End;
    For i := 0 To Dim.Y Div GridSize Do Begin
      glVertex2f(0, GridSize * i);
      glVertex2f(dim.x, GridSize * i);
    End;
    glend;
    glPopMatrix;
  End;
  glBindTexture(GL_TEXTURE_2D, 0);
  If EditMode Then Begin
    glColor3f(0.5, 0.75, 0.5);
    glBegin(GL_POLYGON);
    glVertex2f(FinalZone[0].x, FinalZone[0].y);
    glVertex2f(FinalZone[1].x, FinalZone[1].y);
    glVertex2f(FinalZone[2].x, FinalZone[2].y);
    glVertex2f(FinalZone[3].x, FinalZone[3].y);
    glend;
    glColor3f(0.25, 0.75, 0.25);
    glBegin(GL_LINE_LOOP);
    glVertex2f(FinalZone[0].x, FinalZone[0].y);
    glVertex2f(FinalZone[1].x, FinalZone[1].y);
    glVertex2f(FinalZone[2].x, FinalZone[2].y);
    glVertex2f(FinalZone[3].x, FinalZone[3].y);
    glend;
    RenderAlphaQuad(FinalZone[0].y - fFixedBoltTex.OrigWidth Div 2, FinalZone[0].x - fFixedBoltTex.OrigHeight Div 2, fFixedBoltTex);
    RenderAlphaQuad(FinalZone[1].y - fFixedBoltTex.OrigWidth Div 2, FinalZone[1].x - fFixedBoltTex.OrigHeight Div 2, fFixedBoltTex);
    RenderAlphaQuad(FinalZone[2].y - fFixedBoltTex.OrigWidth Div 2, FinalZone[2].x - fFixedBoltTex.OrigHeight Div 2, fFixedBoltTex);
    RenderAlphaQuad(FinalZone[3].y - fFixedBoltTex.OrigWidth Div 2, FinalZone[3].x - fFixedBoltTex.OrigHeight Div 2, fFixedBoltTex);
    If ShowCollider Then Begin
      glBindTexture(GL_TEXTURE_2D, 0);
      glColor3f(0.25, 0.25, 0.25);
      For i := 0 To high(fColliders) Do Begin
        glBegin(GL_TRIANGLES);
        glVertex2f(fColliders[i].p1.x, fColliders[i].p1.y);
        glVertex2f(fColliders[i].p2.x, fColliders[i].p2.y);
        glVertex2f(fColliders[i].p3.x, fColliders[i].p3.y);
        glend();
      End;
      glColor3f(0.5, 0.5, 0.5);
      For i := 0 To high(fColliders) Do Begin
        glBegin(GL_LINE_LOOP);
        glVertex2f(fColliders[i].p1.x, fColliders[i].p1.y);
        glVertex2f(fColliders[i].p2.x, fColliders[i].p2.y);
        glVertex2f(fColliders[i].p3.x, fColliders[i].p3.y);
        glend();
      End;
    End;
    If ShowDeadZones Then Begin
      glBindTexture(GL_TEXTURE_2D, 0);
      glColor3f(0.75, 0.5, 0.5);
      For i := 0 To high(fDeadZone) Do Begin
        glBegin(GL_TRIANGLES);
        glVertex2f(fDeadZone[i].p1.x, fDeadZone[i].p1.y);
        glVertex2f(fDeadZone[i].p2.x, fDeadZone[i].p2.y);
        glVertex2f(fDeadZone[i].p3.x, fDeadZone[i].p3.y);
        glend();
      End;
      glColor3f(0.75, 0.25, 0.25);
      For i := 0 To high(fDeadZone) Do Begin
        glBegin(GL_LINE_LOOP);
        glVertex2f(fDeadZone[i].p1.x, fDeadZone[i].p1.y);
        glVertex2f(fDeadZone[i].p2.x, fDeadZone[i].p2.y);
        glVertex2f(fDeadZone[i].p3.x, fDeadZone[i].p3.y);
        glend();
      End;
    End;
  End;
  glBindTexture(GL_TEXTURE_2D, 0);
  glLineWidth(3);
  glColor3f(0.25, 0.25, 0.25);
  glBegin(GL_LINES);
  For i := 0 To high(fFixedEdges) Do Begin
    glVertex2f(fFixedBolts[fFixedEdges[i].X].x, fFixedBolts[fFixedEdges[i].X].y);
    glVertex2f(fFixedBolts[fFixedEdges[i].y].x, fFixedBolts[fFixedEdges[i].y].y);
  End;
  glEnd;
  glColor3f(0.25, 0.25, 0.75);
  glBegin(GL_LINES);
  If assigned(fEngine) Then Begin
    For i := 0 To fEngine.SpringCount - 1 Do Begin
      //If (fEngine.GridPoint[fEngine.Spring[i].P1].UserData >= Length(fFixedBolts)) Or
        //(fEngine.GridPoint[fEngine.Spring[i].P2].UserData >= Length(fFixedBolts)) Then Begin
      v := fEngine.GridPoint[fEngine.Spring[i].P1].Pos;
      glVertex2f(v.x, v.y);
      v := fEngine.GridPoint[fEngine.Spring[i].P2].Pos;
      glVertex2f(v.x, v.y);
      //End;
    End;
  End
  Else Begin
    For i := 0 To high(fUserEdges) Do Begin
      v := GetPointByIndex(fUserEdges[i].x);
      glVertex2f(v.x, v.y);
      v := GetPointByIndex(fUserEdges[i].Y);
      glVertex2f(v.x, v.y);
    End;
  End;
  glEnd;
  glLineWidth(1);
  glColor3f(1, 0, 0);
  RenderAlphaQuad(fFixedBolts[0].y - fFixedBoltTex.OrigWidth Div 2, fFixedBolts[0].x - fFixedBoltTex.OrigHeight Div 2, fFixedBoltTex);
  glColor4f(1, 1, 1, 1);
  For i := 1 To high(fFixedBolts) Do Begin // Index 0 ist der StartPunkt !
    RenderAlphaQuad(fFixedBolts[i].y - fFixedBoltTex.OrigWidth Div 2, fFixedBolts[i].x - fFixedBoltTex.OrigHeight Div 2, fFixedBoltTex);
  End;
  glColor4f(0.25, 0.25, 0.75, 1);
  If assigned(fEngine) Then Begin
    For i := 0 To fEngine.GridPointCount - 1 Do Begin
      If fEngine.GridPoint[i].UserData >= Length(fFixedBolts) Then Begin
        RenderAlphaQuad(fEngine.GridPoint[i].Pos.y - fFixedBoltTex.OrigWidth Div 2, fEngine.GridPoint[i].Pos.x - fFixedBoltTex.OrigHeight Div 2, fFixedBoltTex);
      End;
    End;
  End
  Else Begin
    For i := 0 To high(fUserBolts) Do Begin
      RenderAlphaQuad(fUserBolts[i].y - fFixedBoltTex.OrigWidth Div 2, fUserBolts[i].x - fFixedBoltTex.OrigHeight Div 2, fFixedBoltTex);
    End;
  End;
  glPopMatrix;
End;

Procedure TMap.Reset;
Begin
  StopSim;
  ShowCollider := false;
  ShowDeadZones := false;
  setlength(fUserBolts, 0);
  setlength(fUserEdges, 0);
End;

Function TMap.FindPointIndex(x, y: integer): integer;
Var
  i: Integer;
Begin
  result := -1;
  // Ist es ein Fixed Bolt ?
  For i := 0 To high(fFixedBolts) Do Begin
    If lenv2(fFixedBolts[i] - v2(x, y)) <= Epsilon Then Begin
      result := i;
      exit;
    End;
  End;
  // Nein, dann evtl ein "Dynamischer"
  For i := 0 To high(fUserBolts) Do Begin
    If lenv2(fUserBolts[i] - v2(x, y)) <= Epsilon Then Begin
      result := i + length(fFixedBolts);
      exit;
    End;
  End;
End;

Function TMap.AddPoint(x, y: integer): Integer;
Begin
  result := FindPointIndex(x, y);
  If result = -1 Then Begin
    setlength(fUserBolts, high(fUserBolts) + 2);
    fUserBolts[high(fUserBolts)] := v2(x, y);
    result := high(fUserBolts) + Length(fFixedBolts);
  End;
End;

Function TMap.GetPointByIndex(Index: integer): TVector2;
Begin
  result := ZeroV2();
  If (index >= 0) And (Index <= high(fFixedBolts)) Then Begin
    result := fFixedBolts[index];
    exit;
  End;
  index := index - length(fFixedBolts);
  If (index >= 0) And (Index <= high(fUserBolts)) Then Begin
    result := fUserBolts[index];
  End;
End;

Procedure TMap.CheckAndMaybeRemovePoint(NodeIndex: integer);
Var
  i: Integer;
Begin
  If NodeIndex <= high(fFixedBolts) Then exit; // Fixed Bolts werden nicht gelöscht !
  // gibt es noch mindestens eine Kante die auf NodeIndex zeigt ?
  For i := 0 To high(fUserEdges) Do Begin
    If (fUserEdges[i].X = NodeIndex) Or (fUserEdges[i].Y = NodeIndex) Then Begin
      exit;
    End;
  End;
  // Der Nodeindex ist "Freigestellt" -> Nu wird er gelöscht.
  // Erst alle Kanten Indexe anpassen
  For i := 0 To high(fUserEdges) Do Begin
    If fUserEdges[i].X > NodeIndex Then fUserEdges[i].X := fUserEdges[i].X - 1;
    If fUserEdges[i].Y > NodeIndex Then fUserEdges[i].Y := fUserEdges[i].Y - 1;
  End;
  // Dann aus den Userbolts raus werfen ..
  NodeIndex := NodeIndex - length(fFixedBolts);
  For i := NodeIndex To high(fUserBolts) - 1 Do Begin
    fUserBolts[i] := fUserBolts[i + 1];
  End;
  setlength(fUserBolts, high(fUserBolts));
End;

Procedure TMap.StartSim;
Var
  i, index: Integer;
  a, b: TVector2;
Begin
  fSimStartTime := GetTickCount64;
  fSimState := ssCountdown;
  fLastTick := GetTickCount64;
  If assigned(fEngine) Then fEngine.Free;
  uphysik.Gravity := v2(0, 9.8);
  fEngine := TSpringEngine.Create;
  // Set all Points in order to not take care of them anymore ;)
  For i := 0 To high(fFixedBolts) Do Begin
    index := fEngine.AddPoint(fFixedBolts[i], 0, i);
    fEngine.SetFixed(index, true);
  End;
  For i := 0 To high(fUserBolts) Do Begin
    fEngine.AddPoint(fUserBolts[i], 0, length(fFixedBolts) + i);
  End;
  // Set all Springs
  For i := 0 To high(fUserEdges) Do Begin
    a := GetPointByIndex(fUserEdges[i].x);
    b := GetPointByIndex(fUserEdges[i].Y);
    // TODO: Rauskriegen was hier gute Konstanten sind !
    fEngine.AddSpring(a, b, 50, 5, 100, i);
  End;
  // Set All Collider
  For i := 0 To high(fColliders) Do Begin
    // TODO: Rauskriegen was hier ne gute Bounciness ist
    fEngine.AddCollider(
      fColliders[i].p1,
      fColliders[i].p2,
      fColliders[i].p3,
      0.5,
      0
      );
  End;
End;

Procedure TMap.StopSim;
Begin
  If assigned(fEngine) Then fEngine.Free;
  fEngine := Nil;
  fSimState := ssIdle;
End;

Procedure TMap.ToggleUserEdge(StartNodeIndex, EndNodeIndex: Integer);
Var
  i, j: Integer;
Begin
  // Gibts das Ding schon ? -> Dann löschen
  For i := 0 To high(fUserEdges) Do Begin
    If ((fUserEdges[i].x = StartNodeIndex) And (fUserEdges[i].Y = EndNodeIndex)) Or
      ((fUserEdges[i].Y = StartNodeIndex) And (fUserEdges[i].x = EndNodeIndex)) Then Begin
      For j := i To high(fUserEdges) - 1 Do Begin
        fUserEdges[j] := fUserEdges[j + 1];
      End;
      SetLength(fUserEdges, high(fUserEdges));
      CheckAndMaybeRemovePoint(StartNodeIndex);
      CheckAndMaybeRemovePoint(EndNodeIndex);
      exit;
    End;
  End;
  // Sonst Hinzufügen
  setlength(fUserEdges, high(fUserEdges) + 2);
  fUserEdges[high(fUserEdges)] := point(StartNodeIndex, EndNodeIndex);
End;

Function TMap.FindFixedBolt(x, y: integer): integer;
Var
  i: Integer;
Begin
  result := -1;
  For i := 0 To high(fFixedBolts) Do Begin
    If lenv2(fFixedBolts[i] - v2(x, y)) <= Epsilon Then Begin
      result := i;
      exit;
    End;
  End;
End;

Procedure TMap.ToggleFixedEdge(StartNodeIndex, EndNodeIndex: Integer);
Var
  i, j: Integer;
Begin
  // Gibts das Ding schon ? -> Dann löschen
  For i := 0 To high(fFixedEdges) Do Begin
    If ((fFixedEdges[i].x = StartNodeIndex) And (fFixedEdges[i].Y = EndNodeIndex)) Or
      ((fFixedEdges[i].Y = StartNodeIndex) And (fFixedEdges[i].x = EndNodeIndex)) Then Begin
      For j := i To high(fFixedEdges) - 1 Do Begin
        fFixedEdges[j] := fFixedEdges[j + 1];
      End;
      SetLength(fFixedEdges, high(fFixedEdges));
      exit;
    End;
  End;
  // Sonst Hinzufügen
  setlength(fFixedEdges, high(fFixedEdges) + 2);
  fFixedEdges[high(fFixedEdges)] := point(StartNodeIndex, EndNodeIndex);
End;

Procedure TMap.ToggleCollider(p1, p2, p3: TPoint);
Var
  i, j: Integer;
Begin
  // Prüfen ob es den Colider schon gibt, wenn ja wird dieser Gelöscht
  For i := 0 To high(fColliders) Do Begin
    If ((fColliders[i].p1 = p1) And (fColliders[i].p2 = p2) And (fColliders[i].p3 = p3)) Or
      ((fColliders[i].p1 = p1) And (fColliders[i].p2 = p3) And (fColliders[i].p3 = p2)) Or
      ((fColliders[i].p1 = p3) And (fColliders[i].p2 = p1) And (fColliders[i].p3 = p2)) Or
      ((fColliders[i].p1 = p2) And (fColliders[i].p2 = p1) And (fColliders[i].p3 = p3)) Or
      ((fColliders[i].p1 = p3) And (fColliders[i].p2 = p2) And (fColliders[i].p3 = p1)) Or
      ((fColliders[i].p1 = p2) And (fColliders[i].p2 = p3) And (fColliders[i].p3 = p1)) Then Begin
      For j := i To high(fColliders) - 1 Do Begin
        fColliders[j] := fColliders[j + 1];
      End;
      setlength(fColliders, high(fColliders));
      exit;
    End;
  End;
  setlength(fColliders, high(fColliders) + 2);
  fColliders[high(fColliders)].p1 := p1;
  fColliders[high(fColliders)].p2 := p2;
  fColliders[high(fColliders)].p3 := p3;
End;

Function TMap.FindColliderPoint(x, y: integer): Tpoint;
Var
  i: Integer;
Begin
  result := point(-1, -1);
  For i := 0 To high(fColliders) Do Begin
    If fColliders[i].p1 = point(x, y) Then Begin
      result.x := i;
      result.y := 1;
      exit;
    End;
    If fColliders[i].p2 = point(x, y) Then Begin
      result.x := i;
      result.y := 2;
      exit;
    End;
    If fColliders[i].p3 = point(x, y) Then Begin
      result.x := i;
      result.y := 3;
      exit;
    End;
  End;
End;

Procedure TMap.SetColliderpoint(Col, P: TPoint);
Begin
  Case col.y Of
    1: fColliders[col.x].p1 := p;
    2: fColliders[col.x].p2 := p;
    3: fColliders[col.x].p3 := p;
  End;
End;

Procedure TMap.ToggleDeadzone(p1, p2, p3: TPoint);
Var
  i, j: Integer;
Begin
  // Prüfen ob es den Colider schon gibt, wenn ja wird dieser Gelöscht
  For i := 0 To high(fDeadZone) Do Begin
    If ((fDeadZone[i].p1 = p1) And (fDeadZone[i].p2 = p2) And (fDeadZone[i].p3 = p3)) Or
      ((fDeadZone[i].p1 = p1) And (fDeadZone[i].p2 = p3) And (fDeadZone[i].p3 = p2)) Or
      ((fDeadZone[i].p1 = p3) And (fDeadZone[i].p2 = p1) And (fDeadZone[i].p3 = p2)) Or
      ((fDeadZone[i].p1 = p2) And (fDeadZone[i].p2 = p1) And (fDeadZone[i].p3 = p3)) Or
      ((fDeadZone[i].p1 = p3) And (fDeadZone[i].p2 = p2) And (fDeadZone[i].p3 = p1)) Or
      ((fDeadZone[i].p1 = p2) And (fDeadZone[i].p2 = p3) And (fDeadZone[i].p3 = p1)) Then Begin
      For j := i To high(fDeadZone) - 1 Do Begin
        fDeadZone[j] := fDeadZone[j + 1];
      End;
      setlength(fDeadZone, high(fDeadZone));
      exit;
    End;
  End;
  setlength(fDeadZone, high(fDeadZone) + 2);
  fDeadZone[high(fDeadZone)].p1 := p1;
  fDeadZone[high(fDeadZone)].p2 := p2;
  fDeadZone[high(fDeadZone)].p3 := p3;
End;

Function TMap.FindDeadzonePoint(x, y: integer): Tpoint;
Var
  i: Integer;
Begin
  result := point(-1, -1);
  For i := 0 To high(fDeadZone) Do Begin
    If fDeadZone[i].p1 = point(x, y) Then Begin
      result.x := i;
      result.y := 1;
      exit;
    End;
    If fDeadZone[i].p2 = point(x, y) Then Begin
      result.x := i;
      result.y := 2;
      exit;
    End;
    If fDeadZone[i].p3 = point(x, y) Then Begin
      result.x := i;
      result.y := 3;
      exit;
    End;
  End;
End;

Procedure TMap.SetDeadzonepoint(Col, P: TPoint);
Begin
  Case col.y Of
    1: fDeadZone[col.x].p1 := p;
    2: fDeadZone[col.x].p2 := p;
    3: fDeadZone[col.x].p3 := p;
  End;
End;

Function TMap.AddFixedBolt(x, y: integer): integer;
Begin
  Result := FindFixedBolt(x, y);
  If result <> -1 Then exit;
  SetLength(fFixedBolts, high(fFixedBolts) + 2);
  fFixedBolts[high(fFixedBolts)] := v2(x, y);
  result := high(fFixedBolts);
End;

Procedure TMap.DelFixedBolt(x, y: integer);
Var
  i, j, k: Integer;
Begin
  For i := 0 To high(fFixedBolts) Do Begin
    If lenv2(fFixedBolts[i] - v2(x, y)) <= Epsilon Then Begin
      If i = 0 Then exit; // Der StartNode kann nicht gelöscht werden
      // Alle Kanten Berücksichtigen
      For j := high(fFixedEdges) Downto 0 Do Begin
        If (fFixedEdges[j].X = i) Or (fFixedEdges[j].Y = i) Then Begin
          // Die Kante muss gelöscht werden
          For k := j To high(fFixedEdges) - 1 Do Begin
            fFixedEdges[k] := fFixedEdges[k + 1];
          End;
          setlength(fFixedEdges, high(fFixedEdges));
        End
        Else Begin
          // Die Kante Überlebt, aber ggf muss ihr Index Verringert werden
          If fFixedEdges[j].X > i Then fFixedEdges[j].X := fFixedEdges[j].X - 1;
          If fFixedEdges[j].y > i Then fFixedEdges[j].y := fFixedEdges[j].y - 1;
        End;
      End;
      // Das Eigentliche Löschen des Punktes
      For j := i To high(fFixedBolts) - 1 Do Begin
        fFixedBolts[j] := fFixedBolts[j + 1];
      End;
      setlength(fFixedBolts, high(fFixedBolts));
      exit;
    End;
  End;
End;

Function TMap.findFinalZoneIndex(x, y: integer): integer;
Var
  i: Integer;
Begin
  result := -1;
  For i := 0 To high(FinalZone) Do Begin
    If LenV2(v2(x, y) - FinalZone[i]) < Epsilon Then Begin
      result := i;
      exit;
    End;
  End;
End;

Procedure TMap.Clear;
Begin
  If assigned(fEngine) Then fEngine.Free;
  fEngine := Nil;
  zoom := 1.0;
  Offset := Point(0, 0);
  dim := Point(ScreenWidth, ScreenHeight);
  OpenGL_GraphikEngine.RemoveGraphik(fBackTex);
  fBackTex.Image := -1;
  Filename := '';
  setlength(fFixedBolts, 1);
  fFixedBolts[0] := v2(10, 10);
  setlength(fFixedEdges, 0);
  AdjustFinalZone;
  setlength(fColliders, 0);
  setlength(fDeadZone, 0);
  Reset();
End;

Procedure TMap.SaveToFile(Const aFilename: String);
Var
  ini: TIniFile;
  i: Integer;
Begin
  Filename := aFilename;
  ini := TIniFile.Create(aFilename);
  ini.WriteString('General', 'Texture', ExtractRelativePath(afilename, fBackTexFilename));
  ini.WriteInteger('Fixed Bolts', 'Count', length(fFixedBolts));
  For i := 0 To high(fFixedBolts) Do Begin
    ini.WriteString('Fixed Bolts', 'Bolt' + inttostr(i), format('%d/%d', [round(fFixedBolts[i].x), round(fFixedBolts[i].y)]));
  End;
  For i := 0 To 3 Do Begin
    ini.WriteString('FinalZone', 'P' + inttostr(i + 1), format('%d/%d', [round(FinalZone[i].x), round(FinalZone[i].y)]));
  End;
  ini.WriteInteger('Fixed Edges', 'Count', length(fFixedEdges));
  For i := 0 To high(fFixedEdges) Do Begin
    ini.WriteString('Fixed Edges', 'E' + inttostr(i), format('%d/%d', [fFixedEdges[i].x, fFixedEdges[i].y]));
  End;
  ini.WriteInteger('Collider', 'Count', length(fColliders));
  For i := 0 To high(fColliders) Do Begin
    ini.WriteString('Collider', 'C' + inttostr(i) + '_1', format('%d/%d', [fColliders[i].p1.x, fColliders[i].p1.y]));
    ini.WriteString('Collider', 'C' + inttostr(i) + '_2', format('%d/%d', [fColliders[i].p2.x, fColliders[i].p2.y]));
    ini.WriteString('Collider', 'C' + inttostr(i) + '_3', format('%d/%d', [fColliders[i].p3.x, fColliders[i].p3.y]));
  End;
  ini.WriteInteger('Deadzone', 'Count', length(fDeadZone));
  For i := 0 To high(fDeadZone) Do Begin
    ini.WriteString('Deadzone', 'D' + inttostr(i) + '_1', format('%d/%d', [fDeadZone[i].p1.x, fDeadZone[i].p1.y]));
    ini.WriteString('Deadzone', 'D' + inttostr(i) + '_2', format('%d/%d', [fDeadZone[i].p2.x, fDeadZone[i].p2.y]));
    ini.WriteString('Deadzone', 'D' + inttostr(i) + '_3', format('%d/%d', [fDeadZone[i].p3.x, fDeadZone[i].p3.y]));
  End;

  // Hier weiter mit Speichern

  ini.free;
End;

Procedure TMap.LoadFromFile(Const aFilename: String);
Var
  ini: TIniFile;
  dir, s: String;
  i: Integer;
  sa: TStringArray;
Begin
  Clear;
  ini := TIniFile.Create(aFilename);
  fBackTexFilename := ini.ReadString('General', 'Texture', '');
  If fBackTexFilename <> '' Then Begin
    dir := ExtractFilePath(aFilename);
    LoadBackTexture(IncludeTrailingPathDelimiter(dir) + fBackTexFilename);
  End;
  setlength(fFixedBolts, ini.ReadInteger('Fixed Bolts', 'Count', 0));
  For i := 0 To high(fFixedBolts) Do Begin
    s := ini.ReadString('Fixed Bolts', 'Bolt' + inttostr(i), '');
    sa := s.Split('/');
    fFixedBolts[i] := v2(strtoint(sa[0]), strtoint(sa[1]));
  End;
  For i := 0 To 3 Do Begin
    s := ini.ReadString('FinalZone', 'p' + inttostr(i + 1), '');
    sa := s.Split('/');
    FinalZone[i] := v2(strtoint(sa[0]), strtoint(sa[1]));
  End;
  setlength(fFixedEdges, ini.ReadInteger('Fixed Edges', 'Count', 0));
  For i := 0 To high(fFixedEdges) Do Begin
    s := ini.ReadString('Fixed Edges', 'E' + inttostr(i), '');
    sa := s.Split('/');
    fFixedEdges[i] := point(strtoint(sa[0]), strtoint(sa[1]));
  End;
  setlength(fColliders, ini.ReadInteger('Collider', 'Count', 0));
  For i := 0 To high(fColliders) Do Begin
    s := ini.ReadString('Collider', 'C' + inttostr(i) + '_1', '');
    sa := s.Split('/');
    fColliders[i].p1 := point(strtoint(sa[0]), strtoint(sa[1]));
    s := ini.ReadString('Collider', 'C' + inttostr(i) + '_2', '');
    sa := s.Split('/');
    fColliders[i].p2 := point(strtoint(sa[0]), strtoint(sa[1]));
    s := ini.ReadString('Collider', 'C' + inttostr(i) + '_3', '');
    sa := s.Split('/');
    fColliders[i].p3 := point(strtoint(sa[0]), strtoint(sa[1]));
  End;
  setlength(fDeadZone, ini.ReadInteger('Deadzone', 'Count', 0));
  For i := 0 To high(fDeadZone) Do Begin
    s := ini.ReadString('Deadzone', 'D' + inttostr(i) + '_1', '');
    sa := s.Split('/');
    fDeadZone[i].p1 := point(strtoint(sa[0]), strtoint(sa[1]));
    s := ini.ReadString('Deadzone', 'D' + inttostr(i) + '_2', '');
    sa := s.Split('/');
    fDeadZone[i].p2 := point(strtoint(sa[0]), strtoint(sa[1]));
    s := ini.ReadString('Deadzone', 'D' + inttostr(i) + '_3', '');
    sa := s.Split('/');
    fDeadZone[i].p3 := point(strtoint(sa[0]), strtoint(sa[1]));
  End;

  // Hier weiter mit Laden

  ini.free;
  Filename := aFilename;
End;

Procedure TMap.LoadBackTexture(Const aFilename: String);
Begin
  fBackTexFilename := aFilename;
  OpenGL_GraphikEngine.RemoveGraphik(fBackTex);
  fBackTex := OpenGL_GraphikEngine.LoadGraphikItem(aFilename, smClamp);
  dim := point(fBackTex.OrigWidth, fBackTex.OrigHeight);
  AdjustFinalZone;
End;

Procedure TMap.InitOpenGL;
Begin
  fFixedBoltTex := OpenGL_GraphikEngine.LoadAlphaGraphikItem('GFX' + PathDelim + 'fixed_bolt.png');
  // Die Original Textur ist 16x16, aber das ist zu Groß, wenn man das so "hinhackt"
  // dann bleibt die Textur Hochauflösend ;), das geht hier aber nur weil die
  // Textur eine 2^X größe hat ordentlich !
  fFixedBoltTex.OrigWidth := 8;
  fFixedBoltTex.OrigHeight := 8;
  fFixedBoltTex.StretchedWidth := 8;
  fFixedBoltTex.StretchedHeight := 8;
End;

End.

