(******************************************************************************)
(* uopengl_primitives                                              ??.??.???? *)
(*                                                                            *)
(* Version     : 0.02                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Gives higher level abstract OpenGL Objects for easy modeling.*)
(*                                                                            *)
(* License     : See the file license.md, located under:                      *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(* Warranty    : There is no warranty, neither in correctness of the          *)
(*               implementation, nor anything other that could happen         *)
(*               or go wrong, use at your own risk.                           *)
(*                                                                            *)
(* Known Issues: none                                                         *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*               0.02 - TOpenGL_Box                                           *)
(*                                                                            *)
(******************************************************************************)
Unit uopengl_primitives;

{$MODE ObjFPC}{$H+}

Interface

Uses uvectormath,
  dglopengl;

Type

  (*
  Implementierung einer Box von Hand.

  Noch nicht Getestet :

  Noch nicht Proggrammiert :

     Texturemapping
  *)
  TOpenGL_Box = Class
  private
    fdimx, fdimy, fdimz: GLfloat;
  public
    Constructor Create; virtual;
    Destructor Destroy; override;
    Procedure Render; virtual;
    Procedure SetDimension(dimX, dimY, DimZ: GLfloat); virtual;
  End;

  (*
  Implementierung einer GluSphere von Hand.

  Noch nicht Getestet :

     Texturemapping, ( müste aber gehn )

  Noch nicht Proggrammiert :

     Punktübergabe an OpenGL mittels Addressen.
  *)
  TOpenGL_Sphere = Class
  private
    Fslices: Glint;
    Fstacks: Glint;
    Fpoints: TVector3Array;
    Fnormals: TVector3Array;
  public
    Constructor Create; virtual;
    Destructor Destroy; override;
    Procedure Render; virtual;
    Procedure SetDimension(Radius: GLfloat; Slices, Stacks: GLint); virtual;
  End;

  (*
  Die Eigentlichen GluDisks haben noch Loops
  Aber die Implementierung war mir hier zu stressig, da nie benötigt.

  Noch nicht Getestet :

     Texturemapping, ( müste aber gehn )

  Noch nicht Proggrammiert :

    Cullfacing Richtung ??

  *)
  TOpenGL_Disk = Class
  private
    Fslices: Glint;
    Fpoints: TVector3Array;
    FTexCoords: TVector2Array;
  public
    Constructor Create; virtual;
    Destructor Destroy; override;
    Procedure Render; virtual;
    Procedure SetDimension(Radius: GlFloat; Slices: Glint); virtual;
  End;

  (*
  Die Eigentliche GluCylinder haben noch Stacks auf diese haben wir hier aber Verzichtet

  Noch nicht Getestet :

     Texturemapping, ( müste aber gehn )

  Noch Nicht Programmiert

    Bei einer Negativen Höhe kann man reinschaun, da müssen dann alle Richtungen Vertauscht werden , oder was weis ich ...
    Vielleicht Reicht auch eine Translierung ...
  *)
  TOpenGL_Cylinder = Class
  protected
    Fslices: Glint;
    Fpoints: TVector3Array;
    FNormals: TVector3Array;
    FTexCoords: TVector2Array;
  public
    Constructor Create; virtual;
    Destructor Destroy; override;
    Procedure Render; virtual;
    Procedure SetDimension(BaseRadius, TopRadius, Height: glfloat;
      Slices: GLint); virtual;
  End;

  (*
  Genau Gleich wie TOpenGL_Cylinder nur eben mit Deckel und Boden

  Noch nicht Getestet :

     Texturemapping, ( müste aber gehn )

  Noch Nicht Programmiert

    Bei einer Negativen Höhe kann man reinschaun, da müssen dann alle Richtungen Vertauscht werden , oder was weis ich ...
    Vielleicht Reicht auch eine Translierung ...
  *)

  { TOpenGL_Tin }

  TOpenGL_Tin = Class(TOpenGL_Cylinder)
  protected
    FTopMiddle: TVector3;
    FTopRadius: glfloat;
    FBaseRadius: glfloat;
  public
    Procedure Render; override;
    Procedure Render(TopTex, BottomTex: integer); virtual;
    Procedure SetDimension(BaseRadius, TopRadius, Height: glfloat;
      Slices: GLint); override;
  End;

Implementation

Uses math;

{ TOpenGL_Box }

Constructor TOpenGL_Box.Create;
Begin
  fdimx := 0;
  fdimy := 0;
  fdimz := 0;
End;

Destructor TOpenGL_Box.Destroy;
Begin
  Inherited Destroy;
End;

Procedure TOpenGL_Box.Render;
Begin
  glbegin(GL_QUADS);
  // Up Face
  glnormal3f(0, 1, 0);
  glvertex3f(fdimx / 2, fdimy / 2, fdimz / 2);
  glnormal3f(0, 1, 0);
  glvertex3f(fdimx / 2, fdimy / 2, -fdimz / 2);
  glnormal3f(0, 1, 0);
  glvertex3f(-fdimx / 2, fdimy / 2, -fdimz / 2);
  glnormal3f(0, 1, 0);
  glvertex3f(-fdimx / 2, fdimy / 2, fdimz / 2);
  // Right Face
  glnormal3f(1, 0, 0);
  glvertex3f(fdimx / 2, fdimy / 2, fdimz / 2);
  glnormal3f(1, 0, 0);
  glvertex3f(fdimx / 2, -fdimy / 2, fdimz / 2);
  glnormal3f(1, 0, 0);
  glvertex3f(fdimx / 2, -fdimy / 2, -fdimz / 2);
  glnormal3f(1, 0, 0);
  glvertex3f(fdimx / 2, fdimy / 2, -fdimz / 2);
  // Front Face
  glnormal3f(0, 0, -1);
  glvertex3f(fdimx / 2, fdimy / 2, -fdimz / 2);
  glnormal3f(0, 0, -1);
  glvertex3f(fdimx / 2, -fdimy / 2, -fdimz / 2);
  glnormal3f(0, 0, -1);
  glvertex3f(-fdimx / 2, -fdimy / 2, -fdimz / 2);
  glnormal3f(0, 0, -1);
  glvertex3f(-fdimx / 2, fdimy / 2, -fdimz / 2);
  // Down Face
  glnormal3f(0, -1, 0);
  glvertex3f(-fdimx / 2, -fdimy / 2, fdimz / 2);
  glnormal3f(0, -1, 0);
  glvertex3f(-fdimx / 2, -fdimy / 2, -fdimz / 2);
  glnormal3f(0, -1, 0);
  glvertex3f(fdimx / 2, -fdimy / 2, -fdimz / 2);
  glnormal3f(0, -1, 0);
  glvertex3f(fdimx / 2, -fdimy / 2, fdimz / 2);
  // Left Face
  glnormal3f(-1, 0, 0);
  glvertex3f(-fdimx / 2, fdimy / 2, -fdimz / 2);
  glnormal3f(-1, 0, 0);
  glvertex3f(-fdimx / 2, -fdimy / 2, -fdimz / 2);
  glnormal3f(-1, 0, 0);
  glvertex3f(-fdimx / 2, -fdimy / 2, fdimz / 2);
  glnormal3f(-1, 0, 0);
  glvertex3f(-fdimx / 2, fdimy / 2, fdimz / 2);
  // Back Face
  glnormal3f(0, 0, -1);
  glvertex3f(-fdimx / 2, fdimy / 2, fdimz / 2);
  glnormal3f(0, 0, -1);
  glvertex3f(-fdimx / 2, -fdimy / 2, fdimz / 2);
  glnormal3f(0, 0, -1);
  glvertex3f(fdimx / 2, -fdimy / 2, fdimz / 2);
  glnormal3f(0, 0, -1);
  glvertex3f(fdimx / 2, fdimy / 2, fdimz / 2);
  glend();
End;

Procedure TOpenGL_Box.SetDimension(dimX, dimY, DimZ: GLfloat);
Begin
  fdimx := dimX;
  fdimy := dimY;
  fdimz := DimZ;
End;

{ Tsphere }

Constructor TOpenGL_Sphere.Create;
Begin
  Inherited;
  Setlength(Fnormals, 0);
  Setlength(Fpoints, 0);
  fslices := 0;
  fstacks := 0;
End;

Destructor TOpenGL_Sphere.Destroy;
Begin
  Setlength(Fnormals, 0);
  Setlength(Fpoints, 0);
  fslices := 0;
  fstacks := 0;
End;

Procedure TOpenGL_Sphere.Render;
Var
  e, e1, e2, i, j: integer;
  h, ah, ah2, dw, dwh: single;
Begin
  If High(Fpoints) = -1 Then
    exit;
  // Zeichnen des obersten Ringes
  h := 0.5 - cos((pi / Fstacks * 1)) * 0.5;
  dw := 1 / Fslices;
  dwh := dw / 2;
  glbegin(GL_TRIANGLES);
  For i := 1 To Fslices - 1 Do Begin
    // Einhängen des Top Punktes
    glTexCoord2f((i - 1) * dw + dwh, 1);
    glnormal3fv(@fnormals[0]);
    glvertex3fv(@Fpoints[0]);
    // Suchen der unteren Beiden
    glTexCoord2f((i - 1) * dw, 1 - h);
    glNormal3f(fnormals[i].x, fnormals[i].y, fnormals[i].z);
    glvertex3f(Fpoints[i].x, Fpoints[i].y, Fpoints[i].z);
    // Suchen der unteren Beiden
    glTexCoord2f((i) * dw, 1 - h);
    glNormal3f(fnormals[i + 1].x, fnormals[i + 1].y, fnormals[i + 1].z);
    glvertex3f(Fpoints[i + 1].x, Fpoints[i + 1].y, Fpoints[i + 1].z);
  End;
  // Einhängen des Top Punktes
  i := Fslices;
  glTexCoord2f((i - 1) * dw + dwh, 1);
  glNormal3f(fnormals[0].x, fnormals[0].y, fnormals[0].z);
  glvertex3f(Fpoints[0].x, Fpoints[0].y, Fpoints[0].z);
  ah := 1 - h;
  // Suchen der unteren Beiden
  glTexCoord2f((i - 1) * dw, ah);
  glNormal3f(fnormals[i].x, fnormals[i].y, fnormals[i].z);
  glvertex3f(Fpoints[i].x, Fpoints[i].y, Fpoints[i].z);
  // Suchen der unteren Beiden
  glTexCoord2f((i) * dw, ah);
  glNormal3f(fnormals[1].x, fnormals[1].y, fnormals[1].z);
  glvertex3f(Fpoints[1].x, Fpoints[1].y, Fpoints[1].z);
  glend;
  // Zeichnen der Mittleren Ringe
  glbegin(GL_TRIANGLE_STRIP);
  For j := 1 To Fstacks - 2 Do Begin // Die einzelnen Bändchen
    // Den Präfix Laden
    e := Fslices * (j - 1) + 1;
    ah := -(0.5 - cos({degtorad}(pi / Fstacks * j)) * 0.5);
    ah2 := -(0.5 - cos({degtorad}(pi / Fstacks * (j + 1))) * 0.5);
    glTexCoord2f(0, ah);
    glNormal3f(fnormals[e].x, fnormals[e].y, fnormals[e].z);
    glvertex3f(Fpoints[e].x, Fpoints[e].y, Fpoints[e].z);
    e := Fslices * (j) + 1;
    glTexCoord2f(0, ah2);
    glNormal3f(fnormals[e].x, fnormals[e].y, fnormals[e].z);
    glvertex3f(Fpoints[e].x, Fpoints[e].y, Fpoints[e].z);
    For i := 1 To Fslices - 1 Do Begin
      e := Fslices * (j - 1) + i + 1;
      glTexCoord2f(dw * i, ah);
      glNormal3f(fnormals[e].x, fnormals[e].y, fnormals[e].z);
      glvertex3f(Fpoints[e].x, Fpoints[e].y, Fpoints[e].z);
      e := Fslices * (j) + i + 1;
      glTexCoord2f(dw * i, ah2);
      glNormal3f(fnormals[e].x, fnormals[e].y, fnormals[e].z);
      glvertex3f(Fpoints[e].x, Fpoints[e].y, Fpoints[e].z);
    End;
    // Der Suffix
    e := Fslices * (j - 1) + 1;
    glTexCoord2f(1, ah);
    glNormal3f(fnormals[e].x, fnormals[e].y, fnormals[e].z);
    glvertex3f(Fpoints[e].x, Fpoints[e].y, Fpoints[e].z);
    e := Fslices * (j) + 1;
    glTexCoord2f(1, ah2);
    glNormal3f(fnormals[e].x, fnormals[e].y, fnormals[e].z);
    glvertex3f(Fpoints[e].x, Fpoints[e].y, Fpoints[e].z);
  End;
  glend;
  // Zeichnen des Bottom Ginges
  glbegin(GL_TRIANGLES);
  e := High(Fpoints);
  j := Fslices * (Fstacks - 2);
  h := 0.5 - cos({degtorad}(pi / Fstacks * 1)) * 0.5;
  For i := 1 To Fslices - 1 Do Begin
    e1 := j + i + 1;
    e2 := j + i;
    // Einhängen des Bottom Punktes
    glTexCoord2f((i - 1) * dw + dwh, 0);
    glNormal3f(fnormals[e].x, fnormals[e].y, fnormals[e].z);
    glvertex3f(Fpoints[e].x, Fpoints[e].y, Fpoints[e].z);
    // Suchen der Oberen Beiden
    glTexCoord2f((i) * dw, h);
    glNormal3f(fnormals[e1].x, fnormals[e1].y, fnormals[e1].z);
    glvertex3f(Fpoints[e1].x, Fpoints[e1].y, Fpoints[e1].z);
    // Suchen der Oberen Beiden
    glTexCoord2f((i - 1) * dw, h);
    glNormal3f(fnormals[e2].x, fnormals[e2].y, fnormals[e2].z);
    glvertex3f(Fpoints[e2].x, Fpoints[e2].y, Fpoints[e2].z);
  End;
  // Einhängen des Bottom Punktes
  glTexCoord2f(1 - dwh, 0);
  glNormal3f(fnormals[e].x, fnormals[e].y, fnormals[e].z);
  glvertex3f(Fpoints[e].x, Fpoints[e].y, Fpoints[e].z);
  e2 := j + Fslices;
  e1 := j + 1;
  // Suchen der Oberen Beiden
  glTexCoord2f(1, h);
  glNormal3f(fnormals[e1].x, fnormals[e1].y, fnormals[e1].z);
  glvertex3f(Fpoints[e1].x, Fpoints[e1].y, Fpoints[e1].z);
  // Suchen der Oberen Beiden
  glTexCoord2f(1 - dw, h);
  glNormal3f(fnormals[e2].x, fnormals[e2].y, fnormals[e2].z);
  glvertex3f(Fpoints[e2].x, Fpoints[e2].y, Fpoints[e2].z);
  glend;
End;

Procedure TOpenGL_Sphere.SetDimension(Radius: GLfloat; Slices, Stacks: TGLint);
Var
  element, i, j: integer;
  h, s, c, w: extended;
  sqrr, tmpr, dSlices: single;
Begin
  If (slices > 3) And (stacks > 1) And (Radius > 0) Then Begin
    setlength(Fpoints, (Stacks - 1) * Slices + 2);
    setlength(fnormals, high(Fpoints) + 1);
    fslices := slices;
    fstacks := stacks;
    dSlices := 360 / Slices;
    element := 1;
    // Der Top Punkt
    Fpoints[0] := v3(0, radius, 0);
    // Der BodenPunkt
    Fpoints[high(Fpoints)] := v3(0, -radius, 0);
    // Berechnen der Hüllenpunkte
    sqrr := radius * radius;
    For i := 1 To (Stacks) - 1 Do Begin // Anzahl der Ringe
      h := radius - cos((pi / Stacks * i)) * radius;
      tmpr := sqrt(sqrr - sqr(Radius - h));
      // Berechnen des Radiuses des aktuellen Ringes in dieser Ebene
      For j := 1 To (slices) Do Begin // Anzahl der Punkte Pro Ring
        // Berechnung des Bogenmaswinkels
        w := (dSlices * j) * pi / 180;
        SinCos(w, s, c);
        Fpoints[Element] := v3(s * tmpr, radius - h, c * tmpr);
        Inc(element);
      End;
    End;
    // Da alle Vektroren die Länge Radius haben müssen wir diese nicht extra berechnen sondern
    // können gleich mit Radius Normieren.
    For i := 0 To High(Fpoints) Do Begin
      fnormals[i].x := Fpoints[i].x / Radius;
      fnormals[i].y := Fpoints[i].y / Radius;
      fnormals[i].z := Fpoints[i].z / Radius;
    End;
  End
  Else Begin
    // Ungültige Eingaben Löschen die Sphere Daten
    Setlength(Fnormals, 0);
    Setlength(Fpoints, 0);
    fslices := 0;
    fstacks := 0;
  End;
End;

{ TOpenGL_Disk }

Constructor TOpenGL_Disk.Create;
Begin
  Inherited;
  setlength(Fpoints, 0);
  setlength(FTexCoords, 0);
End;

Destructor TOpenGL_Disk.Destroy;
Begin
  setlength(Fpoints, 0);
  setlength(FTexCoords, 0);

End;

Procedure TOpenGL_Disk.Render;
Const
  Normal: TVector3 = (x: 0; y: 1; z: 0);
  Origin: TVector3 = (x: 0; y: 0; z: 0);
Var
  i: integer;
Begin
  If High(Fpoints) = -1 Then
    exit;
  // Wir wollen Phong Shading deswegen hat Jeder Vertex eine Normale.
  glbegin(GL_TRIANGLE_FAN);
  // Mittelpunkt
  glnormal3fv(@Normal);
  glTexCoord2f(0.5, 0.5);
  glVertex3fv(@Origin);
  // Einmal Rum
  For i := 0 To Fslices - 1 Do Begin
    glnormal3fv(@Normal);
    glTexCoord2fv(@FTexCoords[i]);
    glVertex3fv(@Fpoints[i]);
  End;
  // Kreis Schliesen
  glnormal3fv(@Normal);
  glTexCoord2fv(@FTexCoords[0]);
  glVertex3fv(@Fpoints[0]);
  glend;
End;

Procedure TOpenGL_Disk.SetDimension(Radius: GlFloat; Slices: Glint);
Var
  i: integer;
  c, s, w: extended;
Begin
  If (Slices < 3) Or (Radius <= 0) Then Begin
    setlength(Fpoints, 0);
  End
  Else Begin
    fslices := Slices;
    Setlength(Fpoints, Fslices);
    setlength(FTexCoords, Fslices);
    w := 360 / Fslices;
    For i := 0 To Fslices - 1 Do Begin
      SinCos(-w * i * Pi / 180, s, c);
      // Berechnen der ObjectKoordinaten
      Fpoints[i] := v3(c * Radius, 0, s * Radius);
      // Berechnen der Dazugehörigen TextureKoordinaten
      FTexCoords[i] := v2((c + 1) / 2, (s + 1) / 2);
    End;
  End;
End;

{ TOpenGL_Cylinder }

Constructor TOpenGL_Cylinder.Create;
Begin
  Inherited;
  Setlength(Fpoints, 0);
  Setlength(Fnormals, 0);
  Setlength(fTexCoords, 0);
End;

Destructor TOpenGL_Cylinder.Destroy;
Begin
  Setlength(Fpoints, 0);
  Setlength(Fnormals, 0);
  Setlength(FTexCoords, 0);
End;

Procedure TOpenGL_Cylinder.Render;
Var
  i: integer;
Begin
  If High(Fpoints) = -1 Then
    exit;
  glbegin(GL_QUAD_STRIP);
  // Einmal Rum
  For i := 0 To Fslices - 1 Do Begin
    glnormal3fv(@Fnormals[i]);
    glTexCoord2fv(@FTexCoords[i]);
    glVertex3fv(@Fpoints[i]);
    glnormal3fv(@Fnormals[i]);
    glTexCoord2fv(@FTexCoords[Fslices + i]);
    glVertex3fv(@Fpoints[Fslices + i]);
  End;
  // Den Wickel Schliesen.
  glnormal3fv(@Fnormals[0]);
  glTexCoord2f(1, 1);
  glVertex3fv(@Fpoints[0]);
  glnormal3fv(@Fnormals[0]);
  glTexCoord2f(1, 0);
  glVertex3fv(@Fpoints[Fslices]);
  glend;
End;

Procedure TOpenGL_Cylinder.SetDimension(baseRadius, topRadius, Height: glfloat;
  slices: GLint);
Var
  i: integer;
  s, c, w: extended;
Begin
  If (Slices < 3) Or (BaseRadius < 0) Or (TopRadius < 0) Or (Height <= 0) Or
    ((BaseRadius = 0) And (TopRadius = 0)) Then Begin
    Setlength(Fpoints, 0);
    Setlength(Fnormals, 0);
    Setlength(FTexCoords, 0);
  End
  Else Begin
    Fslices := Slices;
    setlength(Fpoints, 2 * slices);
    Setlength(Fnormals, Slices);
    Setlength(FtexCoords, 2 * slices);
    w := 360 / Fslices;
    (*
    0 .. (Fslices - 1) : Oberer Kreis
    Fslices .. High(Fpoints) : Unterer Kreis
    *)
    For i := 0 To Fslices - 1 Do Begin
      SinCos(-w * i * Pi / 180, s, c);
      Fpoints[i] := v3(c * TopRadius, Height, s * TopRadius);
      Fpoints[Fslices + i] := v3(c * baseRadius, 0, s * baseRadius);
      // Nie Normalen brauchts ja Pro Vertex, aber unten und Oben sind ja immer Gleich, also nur 1 mal
      Fnormals[i] := NormV3(v3(c, (BaseRadius - TopRadius) / Height, s));
      // TexturKoordinaten
      FTexCoords[i] := v2(i / Fslices, 1);
      FTexCoords[Fslices + i] := v2(i / Fslices, 0);
    End;
  End;
End;

{ TOpenGL_Tin }

Procedure TOpenGL_Tin.Render;
Const
  TopNormal: TVector3 = (x: 0; y: 1; z: 0);
  BaseNormal: TVector3 = (x: 0; y: - 1; z: 0);
  Origin: TVector3 = (x: 0; y: 0; z: 0);
Var
  i: integer;
Begin
  Inherited;
  // Was den Tin vom Cylinder unterscheided ist das er Abgeschlossen ist
  // Also mus nun noch Deckel und Boden Gemalt werden.
  If High(Fpoints) = -1 Then
    exit;
  // Der Deckel
  If FTopradius <> 0 Then Begin
    // Wir wollen Phong Shading deswegen hat Jeder Vertex eine Normale.
    glbegin(GL_TRIANGLE_FAN);
    // Mittelpunkt
    glnormal3fv(@TopNormal);
    glTexCoord2f(0.5, 0.5);
    glVertex3fv(@FTopMiddle);
    // Einmal Rum
    For i := 0 To Fslices - 1 Do Begin
      glnormal3fv(@TopNormal);
      glTexCoord2fv(@FTexCoords[i]);
      glVertex3fv(@Fpoints[i]);
    End;
    // Kreis Schliesen
    glnormal3fv(@TopNormal);
    glTexCoord2fv(@FTexCoords[0]);
    glVertex3fv(@Fpoints[0]);
    glend;
  End;
  // Der Boden wird im Prinzig Rückwärts Gerendert
  If FBaseRadius <> 0 Then Begin
    // Wir wollen Phong Shading deswegen hat Jeder Vertex eine Normale.
    glbegin(GL_TRIANGLE_FAN);
    // Mittelpunkt
    glnormal3fv(@BaseNormal);
    glTexCoord2f(0.5, 0.5);
    glVertex3fv(@Origin);
    // Kreis Schliesen
    glnormal3fv(@BaseNormal);
    glTexCoord2fv(@FTexCoords[0]);
    glVertex3fv(@Fpoints[Fslices]);
    // Einmal Rum
    For i := 2 * Fslices - 1 Downto Fslices Do Begin
      glnormal3fv(@BaseNormal);
      glTexCoord2fv(@FTexCoords[i]);
      glVertex3fv(@Fpoints[i]);
    End;
    glend;
  End;
End;

Procedure TOpenGL_Tin.Render(TopTex, BottomTex: integer);
Const
  TopNormal: TVector3 = (x: 0; y: 1; z: 0);
  BaseNormal: TVector3 = (x: 0; y: - 1; z: 0);
  Origin: TVector3 = (x: 0; y: 0; z: 0);
Var
  i: integer;
Begin
  Inherited render;
  // Was den Tin vom Cylinder unterscheided ist das er Abgeschlossen ist
  // Also mus nun noch Deckel und Boden Gemalt werden.
  If High(Fpoints) = -1 Then
    exit;
  // Der Deckel
  If FTopradius <> 0 Then Begin
    glBindTexture(GL_TEXTURE_2D, topTex);
    // Wir wollen Phong Shading deswegen hat Jeder Vertex eine Normale.
    glbegin(GL_TRIANGLE_FAN);
    // Mittelpunkt
    glnormal3fv(@TopNormal);
    glTexCoord2f(0.5, 0.5);
    glVertex3fv(@FTopMiddle);
    // Einmal Rum
    For i := 0 To Fslices - 1 Do Begin
      glnormal3fv(@TopNormal);
      glTexCoord2fv(@FTexCoords[i]);
      glVertex3fv(@Fpoints[i]);
    End;
    // Kreis Schliesen
    glnormal3fv(@TopNormal);
    glTexCoord2fv(@FTexCoords[0]);
    glVertex3fv(@Fpoints[0]);
    glend;
  End;
  // Der Boden wird im Prinzig Rückwärts Gerendert
  If FBaseRadius <> 0 Then Begin
    glBindTexture(GL_TEXTURE_2D, BottomTex);
    // Wir wollen Phong Shading deswegen hat Jeder Vertex eine Normale.
    glbegin(GL_TRIANGLE_FAN);
    // Mittelpunkt
    glnormal3fv(@BaseNormal);
    glTexCoord2f(0.5, 0.5);
    glVertex3fv(@Origin);
    // Kreis Schliesen
    glnormal3fv(@BaseNormal);
    glTexCoord2fv(@FTexCoords[0]);
    glVertex3fv(@Fpoints[Fslices]);
    // Einmal Rum
    For i := 2 * Fslices - 1 Downto Fslices Do Begin
      glnormal3fv(@BaseNormal);
      glTexCoord2fv(@FTexCoords[i]);
      glVertex3fv(@Fpoints[i]);
    End;
    glend;
  End;
End;

Procedure TOpenGL_Tin.SetDimension(BaseRadius, TopRadius, Height: glfloat;
  Slices: GLint);
Begin
  // Hier Brauchen wir die Radius sachen damit wir ein klein wenig Ressourcensparender Rendern können.
  FTopRadius := TopRadius;
  FBaseRadius := BaseRadius;
  FTopMiddle := v3(0, Height, 0);
  Inherited;
End;

End.

