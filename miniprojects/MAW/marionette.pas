(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of MAW                                                   *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit marionette;

{$MODE ObjFPC}{$H+}

Interface

Uses classes, sysutils, uOpenGL_Primitives, dglopengl, uvectormath;

Type

  TMSphere = Class(TOpenGL_Sphere)
  private
    Fid: Byte;
  public
    RotateMatrix: TMatrix4x4;
    Property ID: Byte read FID;
    Constructor Create(ID_: Byte); reintroduce;
    Procedure Reset;
  End;

  TMTin = Class(TOpenGL_Tin)
  private
    Fid: Byte;
  public
    RotateMatrix: TMatrix4x4;
    Property ID: Byte read FID;
    Constructor Create(ID_: Byte); reintroduce;
    Procedure Reset;
  End;

  (*

  Nu bleibts daran das Ding Schöner zu machen ..

  *)

  TMarionette = Class
  private
    FSelectedBodyPart: Integer;
    Hals: TMSphere;
    Kopf: TMSphere;
    Bauch: TMSphere;
    LHuefte: TMSphere;
    RHuefte: TMSphere;
    RKnie: TMSphere;
    LKnie: TMSphere;
    RFerse: TMSphere;
    LFerse: TMSphere;
    RSchulter: TMSphere;
    LSchulter: TMSphere;
    REllenBogen: TMSphere;
    LEllenBogen: TMSphere;
    RHandWurzel: TMSphere;
    LHandWurzel: TMSphere;

    Becken: TMTin;
    OberKoerper: TMTin;
    ROberSchenkel: TMTin;
    LOberSchenkel: TMTin;
    RUnterSchenkel: TMTin;
    LUnterSchenkel: TMTin;
    RFus: TMTin;
    LFus: TMTin;
    ROberArm: TMTin;
    LOberArm: TMTin;
    RUnterArm: TMTin;
    LUnterArm: TMTin;
    RHand: TMTin;
    LHand: TMTin;

    Procedure Prepare(Value: TMTin; toSelect: Boolean); overload;
    Procedure Prepare(Value: TMSphere; toSelect: Boolean); overload;
    Procedure FRender(toSelect: Boolean);
  public
    SelectedColor: TVector3;
    Constructor Create;
    Destructor Destroy; override;
    Function Select(x, y: glint): Integer; // Gibt die ID des ausgewählten Körperteils wieder. !! ist aber Modifiziert , gibt nur entsprechnde Gliedmasen keine Gelenke !!
    Procedure SelectBodyPart(ID: Glint);
    Procedure Render;
    Procedure Rotate(ID: Glint; dx, dy, dz: GLfloat);
    Procedure ResetAllRotations;
    Procedure SaveToFile(Filename: String);
    Procedure LoadFromFile(Filename: String);
  End;

Implementation

{ TMarionette }

Constructor TMarionette.Create;
Begin
  Inherited;
  // ID 0 gibt es nicht !!!
  FSelectedBodyPart := 0;
  Bauch := TMSphere.create(1);
  Bauch.SetDimension(0.5, 16, 12);

  Becken := TMTin.create(2);
  Becken.SetDimension(0.5, 0.5, 0.75, 16);

  RHuefte := TMSphere.create(3);
  Rhuefte.SetDimension(0.25, 16, 12);

  ROberschenkel := TMTin.create(4);
  ROberschenkel.SetDimension(0.2, 0.25, 1, 16);

  RKnie := TMSphere.create(5);
  RKnie.SetDimension(0.2, 16, 12);

  RUnterSchenkel := TMTin.create(6);
  RUnterSchenkel.SetDimension(0.15, 0.2, 1.15, 16);

  RFerse := TMSphere.create(7);
  RFerse.SetDimension(0.15, 16, 12);

  RFus := TMTin.create(8);
  RFus.SetDimension(0.10, 0.10, 0.5, 16);

  LHuefte := TMSphere.create(9);
  Lhuefte.SetDimension(0.25, 16, 12);

  LOberschenkel := TMTin.create(10);
  LOberschenkel.SetDimension(0.2, 0.25, 1, 16);

  LKnie := TMSphere.create(11);
  LKnie.SetDimension(0.2, 16, 12);

  LUnterSchenkel := TMTin.create(12);
  LUnterSchenkel.SetDimension(0.15, 0.2, 1.15, 16);

  LFerse := TMSphere.create(13);
  LFerse.SetDimension(0.15, 16, 12);

  LFus := TMTin.create(14);
  LFus.SetDimension(0.10, 0.10, 0.5, 16);

  OberKoerper := TMTin.create(15);
  OberKoerper.SetDimension(0.5, 0.7, 1.5, 16);

  RSchulter := TMSphere.create(16);
  RSchulter.SetDimension(0.2, 16, 12);

  ROberarm := TMTin.create(17);
  ROberarm.SetDimension(0.2, 0.15, 0.9, 16);

  REllenBogen := TMSphere.create(18);
  REllenBogen.SetDimension(0.15, 16, 12);

  RUnterArm := TMTin.create(19);
  RUnterArm.SetDimension(0.15, 0.1, 0.9, 16);

  RHandWurzel := TMSphere.create(20);
  RHandWurzel.SetDimension(0.1, 16, 12);

  RHand := TMTin.create(21);
  RHand.SetDimension(0.15, 0.15, 0.3, 16);

  LSchulter := TMSphere.create(22);
  LSchulter.SetDimension(0.2, 16, 12);

  LOberarm := TMTin.create(23);
  LOberarm.SetDimension(0.2, 0.15, 0.9, 16);

  LEllenBogen := TMSphere.create(24);
  LEllenBogen.SetDimension(0.15, 16, 12);

  LUnterArm := TMTin.create(25);
  LUnterArm.SetDimension(0.15, 0.1, 0.9, 16);

  LHandWurzel := TMSphere.create(26);
  LHandWurzel.SetDimension(0.1, 16, 12);

  LHand := TMTin.create(27);
  LHand.SetDimension(0.15, 0.15, 0.3, 16);

  Hals := TMSphere.create(28);
  hals.SetDimension(0.25, 16, 12);

  Kopf := TMSphere.Create(29);
  Kopf.SetDimension(0.55, 16, 12);

  SelectedColor := v3(1, 0, 0);
  ResetAllRotations;
End;

Destructor TMarionette.Destroy;
Begin
  OberKoerper.free;
  Bauch.free;
  Becken.free;
  Hals.free;
  Kopf.free;

  RHand.free;
  LHand.free;

  RHandWurzel.free;
  LHandWurzel.free;

  RUnterArm.free;
  LUnterArm.free;

  REllenBogen.free;
  LEllenBogen.free;

  RSchulter.free;
  LSchulter.free;

  ROberarm.free;
  LOberarm.free;

  Rfus.free;
  Lfus.free;

  RFerse.free;
  LFerse.free;

  RUnterSchenkel.free;
  LUnterSchenkel.free;

  RKnie.free;
  LKnie.free;

  ROberschenkel.free;
  LOberschenkel.free;

  Rhuefte.free;
  Lhuefte.free;
End;

Procedure TMarionette.ResetAllRotations;
Begin
  OberKoerper.reset;
  Bauch.reset;
  Becken.reset;
  Hals.reset;
  Kopf.reset;

  RHand.reset;
  LHand.reset;
  RHandWurzel.reset;
  LHandWurzel.reset;
  RUnterArm.reset;
  LUnterArm.reset;
  REllenBogen.reset;
  LEllenBogen.reset;
  ROberarm.reset;
  LOberarm.reset;
  RSchulter.reset;
  LSchulter.reset;
  Rfus.reset;
  Lfus.reset;
  RFerse.reset;
  LFerse.reset;
  RUnterSchenkel.reset;
  LUnterSchenkel.reset;
  RKnie.reset;
  LKnie.reset;
  ROberschenkel.reset;
  LOberschenkel.reset;
  Rhuefte.reset;
  Lhuefte.reset;
End;

Procedure TMarionette.Render;
Begin
  Frender(False);
End;

(*
Man hätte Prepare auch nur einmal schreiben müssen wenn man da Ettliche andere OOP Tricks benutzt
So gehts schneller einfacher und ist genau so übersichtlich ;).
Wichtig nur in beiden Prepare Procedure mus das selbe stehen !!
*)

Procedure TMarionette.Prepare(Value: TMTin; toSelect: Boolean);
Begin
  glMultMatrixf(@Value.RotateMatrix[0, 0]);
  If ToSelect Then
    glColor3ub(Value.id, 0, 0)
  Else If FSelectedBodyPart = Value.id Then
    glcolor3fv(@SelectedColor)
  Else
    glcolor3f(1, 1, 1);
End;

Procedure TMarionette.Prepare(Value: TMSphere; toSelect: Boolean);
Begin
  glMultMatrixf(@Value.RotateMatrix[0, 0]);
  If ToSelect Then
    glColor3ub(Value.id, 0, 0)
  Else If FSelectedBodyPart = Value.id Then
    glcolor3fv(@SelectedColor)
  Else
    glcolor3f(1, 1, 1);
End;

(*
Ist toSelect = True wird der Rotkanal der Farbe jedes Objectes zur ID.
*)

Procedure TMarionette.FRender(toSelect: Boolean);
Var
  lig: GLboolean;
Begin
  glpushmatrix;
  lig := glIsEnabled(GL_LIGHTING);
  If lig And Toselect Then
    glDisable(gl_lighting);
  glcolor4f(1, 1, 1, 1);
  // Zum Rücksetzen mus Color wieder mit 1,1,1,1 Zurückgesetzt werden !!!
  If Lig Then Begin
    glColorMaterial(GL_FRONT, GL_DIFFUSE);
    glenable(GL_COLOR_MATERIAL);
  End;

  // Die Drehung des Gesamten Mänchens
  Prepare(Bauch, Toselect);
  Bauch.render;
  // Der Oberkörper
  glpushmatrix;
  gltranslatef(0, 0.25, 0);
  Prepare(OberKoerper, toselect);
  OberKoerper.render;
  // Hals + Kopf
  glpushmatrix;
  gltranslatef(0, 1.65, 0);
  Prepare(Hals, toselect);
  Hals.render;
  gltranslatef(0, 0.55, 0);
  Prepare(Kopf, toselect);
  Kopf.render;

  (*
  Hier stehen der Hals und der Kopf
  *)
  glpopmatrix;
  // Rechter Arm
  glpushmatrix;
  gltranslatef(-0.75, 1.00, 0);
  Prepare(RSchulter, toselect);
  RSchulter.render;
  glrotatef(90, 0, 0, 1);
  gltranslatef(0.0, 0.1, 0);
  Prepare(ROberarm, toselect);
  ROberarm.render;
  gltranslatef(0.0, 1.0, 0);
  Prepare(REllenBogen, toselect);
  REllenBogen.render;
  gltranslatef(0.0, 0.1, 0);
  Prepare(RUnterArm, toselect);
  RUnterArm.render;
  gltranslatef(0.0, 0.95, 0);
  Prepare(RHandWurzel, toselect);
  RHandWurzel.render;
  gltranslatef(0, 0.05, 0);
  Prepare(RHand, toselect);
  RHand.render;
  glpopmatrix;
  // Linker Arm
  glpushmatrix;
  gltranslatef(0.75, 1.00, 0);
  Prepare(lSchulter, toselect);
  lSchulter.render;
  glrotatef(-90, 0, 0, 1);
  gltranslatef(0.0, 0.1, 0);
  Prepare(lOberarm, toselect);
  lOberarm.render;
  gltranslatef(0.0, 1.0, 0);
  Prepare(LEllenBogen, toselect);
  LEllenBogen.render;
  gltranslatef(0.0, 0.1, 0);
  Prepare(LUnterArm, toselect);
  LUnterArm.render;
  gltranslatef(0.0, 0.95, 0);
  Prepare(LHandWurzel, toselect);
  LHandWurzel.render;
  gltranslatef(0, 0.05, 0);
  Prepare(LHand, toselect);
  LHand.render;
  glpopmatrix;
  glpopmatrix;
  // Rendern der Unteren Extremitäten
  glpushmatrix;
  gltranslatef(0, -1, 0);
  Prepare(Becken, toselect);
  Becken.render;
  // Rechtes Bein
  glpushmatrix;
  gltranslatef(-0.25, -0.10, 0);
  Prepare(Rhuefte, toselect);
  Rhuefte.render;
  gltranslatef(0, -1.15, 0);
  Prepare(ROberschenkel, toselect);
  ROberschenkel.render;
  gltranslatef(0, -0.1, 0);
  Prepare(RKnie, toselect);
  RKnie.render;
  gltranslatef(0, -1.25, 0);
  Prepare(RUnterSchenkel, toselect);
  RUnterSchenkel.render;
  gltranslatef(0, -0.1, 0);
  Prepare(RFerse, toselect);
  RFerse.render;
  glrotatef(-90, 1, 0, 0);
  gltranslatef(0, -0.1, -0.1);
  Prepare(RFus, toselect);
  RFus.render;
  glpopmatrix;
  // Linkes Bein
  glpushmatrix;
  gltranslatef(0.25, -0.10, 0);
  Prepare(Lhuefte, toselect);
  Lhuefte.render;
  gltranslatef(0, -1.15, 0);
  Prepare(LOberschenkel, toselect);
  LOberschenkel.render;
  gltranslatef(0, -0.1, 0);
  Prepare(LKnie, toselect);
  LKnie.render;
  gltranslatef(0, -1.25, 0);
  Prepare(LUnterSchenkel, toselect);
  LUnterSchenkel.render;
  gltranslatef(0, -0.1, 0);
  Prepare(LFerse, toselect);
  LFerse.render;
  glrotatef(-90, 1, 0, 0);
  gltranslatef(0, -0.1, -0.1);
  Prepare(LFus, toselect);
  LFus.render;
  glpopmatrix;

  glpopmatrix;
  // Rendern des Oberkörpers
  glpushmatrix;
  glpopmatrix;

  glcolor4f(1, 1, 1, 1);
  If Lig Then Begin
    gldisable(GL_COLOR_MATERIAL);
    If Toselect Then
      glenable(gl_lighting);
  End;
  glpopmatrix;
End;

Procedure TMarionette.Rotate(ID: Glint; dx, dy, dz: GLfloat);
Begin
  glPushMatrix();
  glLoadIdentity();
  Case ID Of
    1: Begin // Drehung der Gesamtfigur , am Bauch
        //        glTranslatef(Bauch.rotateMatrix[12], Bauch.rotateMatrix[13], Bauch.rotateMatrix[14]);
        glTranslatef(Bauch.rotateMatrix[3, 0], Bauch.rotateMatrix[3, 1], Bauch.rotateMatrix[3, 2]);
        glRotatef(dx, 1.0, 0.0, 0.0);
        glRotatef(dy, 0.0, 1.0, 0.0);
        glRotatef(dz, 0.0, 0.0, 1.0);
        //        glTranslatef(-Bauch.rotateMatrix[12], -Bauch.rotateMatrix[13], -Bauch.rotateMatrix[14]);
        glTranslatef(-Bauch.rotateMatrix[3, 0], -Bauch.rotateMatrix[3, 1], -Bauch.rotateMatrix[3, 2]);
        glMultMatrixf(@Bauch.rotateMatrix[0, 0]);
        glGetFloatv(GL_MODELVIEW_MATRIX, @Bauch.rotateMatrix[0, 0]);
      End;
    2: Begin // Becken
        gltranslatef(0, 1, 0);
        glRotatef(dx, 1.0, 0.0, 0.0);
        glRotatef(dy, 0.0, 1.0, 0.0);
        glRotatef(dz, 0.0, 0.0, 1.0);
        gltranslatef(0, -1, 0);
        glMultMatrixf(@Becken.rotateMatrix[0, 0]);
        glGetFloatv(GL_MODELVIEW_MATRIX, @Becken.rotateMatrix[0, 0]);
      End;
    3, 4: Begin
        gltranslatef(0, 1.15, 0);
        glRotatef(dx, 1.0, 0.0, 0.0);
        glRotatef(dy, 0.0, 1.0, 0.0);
        glRotatef(dz, 0.0, 0.0, 1.0);
        gltranslatef(0, -1.15, 0);
        glMultMatrixf(@ROberschenkel.rotateMatrix[0, 0]);
        glGetFloatv(GL_MODELVIEW_MATRIX, @ROberschenkel.rotateMatrix[0, 0]);
      End;
    5, 6: Begin
        gltranslatef(0, 1.25, 0);
        glRotatef(dx, 1.0, 0.0, 0.0);
        glRotatef(dy, 0.0, 1.0, 0.0);
        glRotatef(dz, 0.0, 0.0, 1.0);
        gltranslatef(0, -1.25, 0);
        glMultMatrixf(@RUnterSchenkel.rotateMatrix[0, 0]);
        glGetFloatv(GL_MODELVIEW_MATRIX, @RUnterSchenkel.rotateMatrix[0, 0]);
      End;
    7, 8: Begin
        gltranslatef(0, 0.1, 0.1);
        glRotatef(dx, 1.0, 0.0, 0.0);
        glRotatef(dy, 0.0, 1.0, 0.0);
        glRotatef(dz, 0.0, 0.0, 1.0);
        gltranslatef(0, -0.1, -0.1);
        glMultMatrixf(@RFus.rotateMatrix[0, 0]);
        glGetFloatv(GL_MODELVIEW_MATRIX, @RFus.rotateMatrix[0, 0]);
      End;
    9, 10: Begin
        gltranslatef(0, 1.15, 0);
        glRotatef(dx, 1.0, 0.0, 0.0);
        glRotatef(dy, 0.0, 1.0, 0.0);
        glRotatef(dz, 0.0, 0.0, 1.0);
        gltranslatef(0, -1.15, 0);
        glMultMatrixf(@LOberschenkel.rotateMatrix[0, 0]);
        glGetFloatv(GL_MODELVIEW_MATRIX, @LOberschenkel.rotateMatrix[0, 0]);
      End;
    11, 12: Begin
        gltranslatef(0, 1.25, 0);
        glRotatef(dx, 1.0, 0.0, 0.0);
        glRotatef(dy, 0.0, 1.0, 0.0);
        glRotatef(dz, 0.0, 0.0, 1.0);
        gltranslatef(0, -1.25, 0);
        glMultMatrixf(@LUnterSchenkel.rotateMatrix[0, 0]);
        glGetFloatv(GL_MODELVIEW_MATRIX, @LUnterSchenkel.rotateMatrix[0, 0]);
      End;
    13, 14: Begin
        gltranslatef(0, 0.1, 0.1);
        glRotatef(dx, 1.0, 0.0, 0.0);
        glRotatef(dy, 0.0, 1.0, 0.0);
        glRotatef(dz, 0.0, 0.0, 1.0);
        gltranslatef(0, -0.1, -0.1);
        glMultMatrixf(@LFus.rotateMatrix[0, 0]);
        glGetFloatv(GL_MODELVIEW_MATRIX, @LFus.rotateMatrix[0, 0]);
      End;
    15: Begin
        gltranslatef(0, -0.25, 0);
        glRotatef(dx, 1.0, 0.0, 0.0);
        glRotatef(dy, 0.0, 1.0, 0.0);
        glRotatef(dz, 0.0, 0.0, 1.0);
        gltranslatef(0, 0.25, 0);
        glMultMatrixf(@Oberkoerper.rotateMatrix[0, 0]);
        glGetFloatv(GL_MODELVIEW_MATRIX, @Oberkoerper.rotateMatrix[0, 0]);
      End;
    16, 17: Begin
        gltranslatef(0, -0.1, 0);
        glRotatef(dx, 1.0, 0.0, 0.0);
        glRotatef(dy, 0.0, 1.0, 0.0);
        glRotatef(dz, 0.0, 0.0, 1.0);
        gltranslatef(0, 0.1, 0);
        glMultMatrixf(@ROberarm.rotateMatrix[0, 0]);
        glGetFloatv(GL_MODELVIEW_MATRIX, @ROberarm.rotateMatrix[0, 0]);
      End;
    18, 19: Begin
        gltranslatef(0, -0.1, 0);
        glRotatef(dx, 1.0, 0.0, 0.0);
        glRotatef(dy, 0.0, 1.0, 0.0);
        glRotatef(dz, 0.0, 0.0, 1.0);
        gltranslatef(0, 0.1, 0);
        glMultMatrixf(@RUnterarm.rotateMatrix[0, 0]);
        glGetFloatv(GL_MODELVIEW_MATRIX, @RUnterarm.rotateMatrix[0, 0]);
      End;
    20, 21: Begin
        gltranslatef(0, -0.05, 0);
        glRotatef(dx, 1.0, 0.0, 0.0);
        glRotatef(dy, 0.0, 1.0, 0.0);
        glRotatef(dz, 0.0, 0.0, 1.0);
        gltranslatef(0, 0.05, 0);
        glMultMatrixf(@Rhand.rotateMatrix[0, 0]);
        glGetFloatv(GL_MODELVIEW_MATRIX, @Rhand.rotateMatrix[0, 0]);
      End;
    22, 23: Begin
        gltranslatef(0, -0.1, 0);
        glRotatef(dx, 1.0, 0.0, 0.0);
        glRotatef(dy, 0.0, 1.0, 0.0);
        glRotatef(dz, 0.0, 0.0, 1.0);
        gltranslatef(0, 0.1, 0);
        glMultMatrixf(@LOberarm.rotateMatrix[0, 0]);
        glGetFloatv(GL_MODELVIEW_MATRIX, @LOberarm.rotateMatrix[0, 0]);
      End;
    24, 25: Begin
        gltranslatef(0, -0.1, 0);
        glRotatef(dx, 1.0, 0.0, 0.0);
        glRotatef(dy, 0.0, 1.0, 0.0);
        glRotatef(dz, 0.0, 0.0, 1.0);
        gltranslatef(0, 0.1, 0);
        glMultMatrixf(@LUnterarm.rotateMatrix[0, 0]);
        glGetFloatv(GL_MODELVIEW_MATRIX, @LUnterarm.rotateMatrix[0, 0]);
      End;
    26, 27: Begin
        gltranslatef(0, -0.05, 0);
        glRotatef(dx, 1.0, 0.0, 0.0);
        glRotatef(dy, 0.0, 1.0, 0.0);
        glRotatef(dz, 0.0, 0.0, 1.0);
        gltranslatef(0, 0.05, 0);
        glMultMatrixf(@Lhand.rotateMatrix[0, 0]);
        glGetFloatv(GL_MODELVIEW_MATRIX, @Lhand.rotateMatrix[0, 0]);
      End;
    28, 29: Begin
        gltranslatef(0, -0.55, 0);
        glRotatef(dx, 1.0, 0.0, 0.0);
        glRotatef(dy, 0.0, 1.0, 0.0);
        glRotatef(dz, 0.0, 0.0, 1.0);
        gltranslatef(0, 0.55, 0);
        glMultMatrixf(@Kopf.rotateMatrix[0, 0]);
        glGetFloatv(GL_MODELVIEW_MATRIX, @Kopf.rotateMatrix[0, 0]);
      End;
  End;
  glPopMatrix();
End;

Function TMarionette.Select(x, y: glint): Integer;
Var
  C: Integer;
Begin
  // Löschen des Framebuffers
  glClear(GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT);
  // Rendern im Select Mode
  FRender(true);
  // Read the Element Color = ID
  glReadPixels(X, Y, 1, 1, GL_RGBA, GL_UNSIGNED_BYTE, @C);
  // Calculate to RGB
  C := C And $FFFFFF;
  // je Nach Körperteil müssen wir das Ganze umPipen
  // Da ja nur bestimmte Körperteile Erlaubt sind.
  If c = 3 Then c := 4;
  If c = 5 Then c := 6;
  If c = 7 Then c := 8;
  If c = 9 Then c := 10;
  If c = 11 Then c := 12;
  If c = 13 Then c := 14;
  If c = 16 Then c := 17;
  If c = 18 Then c := 19;
  If c = 20 Then c := 21;
  If c = 22 Then c := 23;
  If c = 24 Then c := 25;
  If c = 26 Then c := 27;
  If c = 28 Then c := 29;

  Result := C;
End;

Procedure TMarionette.SelectBodyPart(ID: Glint);
Begin
  (*
  Die Obere Grenze entspricht den Körperteilen !!
  *)
  If ID > 0 Then FSelectedBodyPart := ID;
End;

Procedure TMarionette.LoadFromFile(Filename: String);
Var
  f: Tfilestream;
Begin
  If Fileexists(Filename) Then Begin
    ResetAllRotations;
    f := TFileStream.create(Filename, fmopenread);
    f.read(Bauch.rotateMatrix[0, 0], sizeof(Bauch.rotateMatrix));
    f.read(Becken.rotateMatrix[0, 0], sizeof(Becken.rotateMatrix));
    f.read(ROberschenkel.rotateMatrix[0, 0], sizeof(ROberschenkel.rotateMatrix));
    f.read(RUnterSchenkel.rotateMatrix[0, 0], sizeof(RUnterSchenkel.rotateMatrix));
    f.read(RFus.rotateMatrix[0, 0], sizeof(RFus.rotateMatrix));
    f.read(LOberschenkel.rotateMatrix[0, 0], sizeof(LOberschenkel.rotateMatrix));
    f.read(LUnterSchenkel.rotateMatrix[0, 0], sizeof(LUnterSchenkel.rotateMatrix));
    f.read(LFus.rotateMatrix[0, 0], sizeof(LFus.rotateMatrix));
    f.read(Oberkoerper.rotateMatrix[0, 0], sizeof(Oberkoerper.rotateMatrix));
    f.read(ROberarm.rotateMatrix[0, 0], sizeof(ROberarm.rotateMatrix));
    f.read(RUnterarm.rotateMatrix[0, 0], sizeof(RUnterarm.rotateMatrix));
    f.read(Rhand.rotateMatrix[0, 0], sizeof(Rhand.rotateMatrix));
    f.Read(LOberarm.rotateMatrix[0, 0], sizeof(LOberarm.rotateMatrix));
    f.Read(LUnterarm.rotateMatrix[0, 0], sizeof(LUnterarm.rotateMatrix));
    f.read(Lhand.rotateMatrix[0, 0], sizeof(Lhand.rotateMatrix));
    f.read(Kopf.rotateMatrix[0, 0], sizeof(Kopf.rotateMatrix));

    f.free;
  End;
End;

Procedure TMarionette.SaveToFile(Filename: String);
Var
  f: Tfilestream;
Begin
  f := TFileStream.create(Filename, fmopenwrite Or fmcreate);
  f.write(Bauch.rotateMatrix[0, 0], sizeof(Bauch.rotateMatrix));
  f.write(Becken.rotateMatrix[0, 0], sizeof(Becken.rotateMatrix));
  f.write(ROberschenkel.rotateMatrix[0, 0], sizeof(ROberschenkel.rotateMatrix));
  f.write(RUnterSchenkel.rotateMatrix[0, 0], sizeof(RUnterSchenkel.rotateMatrix));
  f.write(RFus.rotateMatrix[0, 0], sizeof(RFus.rotateMatrix));
  f.write(LOberschenkel.rotateMatrix[0, 0], sizeof(LOberschenkel.rotateMatrix));
  f.write(LUnterSchenkel.rotateMatrix[0, 0], sizeof(LUnterSchenkel.rotateMatrix));
  f.write(LFus.rotateMatrix[0, 0], sizeof(LFus.rotateMatrix));
  f.write(Oberkoerper.rotateMatrix[0, 0], sizeof(Oberkoerper.rotateMatrix));
  f.write(ROberarm.rotateMatrix[0, 0], sizeof(ROberarm.rotateMatrix));
  f.write(RUnterarm.rotateMatrix[0, 0], sizeof(RUnterarm.rotateMatrix));
  f.write(Rhand.rotateMatrix[0, 0], sizeof(Rhand.rotateMatrix));
  f.write(LOberarm.rotateMatrix[0, 0], sizeof(LOberarm.rotateMatrix));
  f.write(LUnterarm.rotateMatrix[0, 0], sizeof(LUnterarm.rotateMatrix));
  f.write(Lhand.rotateMatrix[0, 0], sizeof(Lhand.rotateMatrix));
  f.write(Kopf.rotateMatrix[0, 0], sizeof(Kopf.rotateMatrix));

  f.free;
End;

{ TMSphere }

Constructor TMSphere.Create(ID_: Byte);
Begin
  Inherited create;
  Fid := id_;
  Reset;
End;

Procedure TMSphere.Reset;
Begin
  RotateMatrix := IdentityMatrix4x4;
End;

{ TMTin }

Constructor TMTin.Create(ID_: Byte);
Begin
  Inherited create;
  Fid := id_;
  Reset;
End;

Procedure TMTin.Reset;
Begin
  RotateMatrix := IdentityMatrix4x4;
End;

End.

