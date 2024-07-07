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

  { TMap }

  TMap = Class
  private
    fBackTexFilename: String; // Wird Relativ gespeichert, ist zur Laufzeit aber Absolut !
    fBackTex: TGraphikItem;
    fFixedBoltTex: TGraphikItem;

    fFixedEdges: Array Of TPoint;

    Procedure AdjustFinalZone;
    Function getStartPoint: TVector2;
    Procedure SetStartPoint(AValue: TVector2);
  public
    // TODO: Interface aufräumen !
    Offset, Dim: TPoint;
    Filename: String;
    ShowGrid: Boolean;
    FinalZone: Array[0..3] Of TVector2;
    EditMode: Boolean;

    fFixedBolts: Array Of TVector2;

    Property StartPoint: TVector2 read getStartPoint write SetStartPoint;


    Constructor Create(); virtual;
    Destructor Destroy(); override;

    Procedure Render();

    Function AddFixedBolt(x, y: integer): integer;
    Procedure DelFixedBolt(x, y: integer);
    Function FindFixedBolt(x, y: integer): integer;

    Procedure ToggleFixedEdge(StartNodeIndex, EndNodeIndex: Integer);

    Function findFinalZoneIndex(x, y: integer): integer;

    Procedure Clear; // Löscht einfach alles auf der Karte
    Procedure SaveToFile(Const aFilename: String);
    Procedure LoadFromFile(Const aFilename: String);

    Procedure LoadBackTexture(Const aFilename: String);
    Procedure InitOpenGL;
  End;

Implementation

Uses dglOpenGL, IniFiles, ubridge_builder;

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
  Clear;
End;

Destructor TMap.Destroy;
Begin
  Clear;
End;

Procedure TMap.Render;
Var
  i: Integer;
Begin
  glColor4f(1, 1, 1, 1);
  glPushMatrix;
  glTranslatef(-Offset.x, -Offset.y, 0);
  RenderQuad(0, 0, fBackTex);
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
  glLineWidth(1);
  glColor3f(1, 0, 0);
  RenderAlphaQuad(fFixedBolts[0].y - fFixedBoltTex.OrigWidth Div 2, fFixedBolts[0].x - fFixedBoltTex.OrigHeight Div 2, fFixedBoltTex);
  glColor4f(1, 1, 1, 1);
  For i := 1 To high(fFixedBolts) Do Begin
    RenderAlphaQuad(fFixedBolts[i].y - fFixedBoltTex.OrigWidth Div 2, fFixedBolts[i].x - fFixedBoltTex.OrigHeight Div 2, fFixedBoltTex);
  End;
  glPopMatrix;
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
  Offset := Point(0, 0);
  dim := Point(ScreenWidth, ScreenHeight);
  OpenGL_GraphikEngine.RemoveGraphik(fBackTex);
  fBackTex.Image := -1;
  Filename := '';
  setlength(fFixedBolts, 1);
  fFixedBolts[0] := v2(10, 10);
  setlength(fFixedEdges, 0);
  AdjustFinalZone;
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
End;

End.

