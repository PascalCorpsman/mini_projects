(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Parken                                                *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit uparken;

{$MODE objfpc}{$H+}

Interface

Uses
  dglopengl, Classes, SysUtils, graphics, math, ugraphics, uvectormath, fileutil;

Type

  TGameState = (gsWait, gsError, gsInGame, gsEditCar, gsEditTrailer, gsEditMap);

  TOOBB = Array[0..3] Of TVector2;

  TBaseAnhaenger = Class;

  TLine = Record
    Start, Goal: TVector2;
    Color: TRGB;
  End;


  { TBaseVehicle }

  TBaseVehicle = Class
  private
    FAnkerPosition: TVector2; // Punkt zwischen den Vorderrädern
    FPosition: TVector2; // Punkt zwischen den Hinterrädern
    Function GetAnhaengerAnker: TVector2; virtual; // Ermitteln der Anhängerpos
  public
    Anhaenger: TBaseAnhaenger;
    ForceRenderAnhaengerKupplung: Boolean;
    Property AnhaengerAnker: TVector2 read GetAnhaengerAnker;
    Constructor create;
    Destructor destroy; override;
    Procedure LoadFromFile(Filename: String); virtual;
    Procedure LoadFromStream(Const Stream: TStream); virtual;
    Procedure SaveToFile(Filename: String);
    Procedure SaveToStream(Const Stream: TStream); virtual;
  End;

  { TCar }

  TCar = Class(TBaseVehicle)
  private
    FCarDir: Single; // Gesamt Winkel des Fahrzeuges
    Fangle: Single; // Winkel der Vorderräder
    Procedure RenderWheel(Angle_: Single); // Zeichnen eines Rades
    Procedure SetAngle(Value: Single);
    Procedure SetCarDir(Value: Single);
    Function GetOOBB: TOOBB; // Ermitteln der OOBB
    Function GetAnhaengerAnker: TVector2; override;
  public
    Alpha: Single;
    W1: Single;
    W2: Single;
    H1: Single;
    H2: Single;
    H3: Single;
    H4: Single;
    Speed: Single;
    Property OOBB: TOOBB read GetOOBB;
    Property Angle: Single read Fangle write SetAngle;
    Property CarAngle: Single read FCarDir write SetCarDir;
    Constructor create;
    Destructor destroy; override;
    Procedure Render;
    Procedure Move(dt: Single);
    Procedure LoadFromStream(Const Stream: TStream); override;
    Procedure SaveToStream(Const Stream: TStream); override;
  End;

  { TBaseAnhaenger }

  TBaseAnhaenger = Class(TBaseVehicle)
  private
    FOwner: TBaseVehicle;
    Function GetOOBB: TOOBB; virtual;
    Procedure RenderWheel(Width, Height, Angle_: Single);
  public
    Property OOBB: TOOBB read GetOOBB;
    Constructor Create(Owner: TBaseVehicle); virtual;
    Destructor destroy; override;
    Procedure Render; virtual;
    Procedure SetAnhaenger; virtual;
    Procedure Move; virtual;
    Procedure LoadFromFile(FilenameString: String); override;
  End;

  { TEinachsenAnhaenger }

  TEinAchsenAnhaenger = Class(TBaseAnhaenger)
  private
    Fdir: Single;
    Function GetOOBB: TOOBB; override;
    Function GetAnhaengerAnker: TVector2; override;
  public
    W1: Single;
    W2: Single;
    H1: Single;
    H2: Single;
    H3: Single;
    H4: Single;
    Constructor Create(Owner: TBaseVehicle); override;
    Destructor destroy; override;
    Procedure Render; override;
    Procedure SetAnhaenger; override;
    Procedure Move; override;
    Procedure LoadFromStream(Const Stream: TStream); override;
    Procedure SaveToStream(Const Stream: TStream); override;
  End;

  { TZweiAchsenAnhaenger }

  TZweiAchsenAnhaenger = Class(TBaseAnhaenger)
  private
    Fdir: Single;
    Fdir2: Single;
    FPosition2: TVector2;
    Function GetOOBB: TOOBB; override;
    Function GetAnhaengerAnker: TVector2; override;
  public
    W1: Single;
    W2: Single;
    H1: Single;
    H2: Single;
    H3: Single;
    H4: Single;
    H5: Single;
    Constructor Create(Owner: TBaseVehicle); override;
    Destructor destroy; override;
    Procedure Render; override;
    Procedure SetAnhaenger; override;
    Procedure Move; override;
    Procedure LoadFromStream(Const Stream: TStream); override;
    Procedure SaveToStream(Const Stream: TStream); override;
  End;

  { TArea }

  TArea = Class
  private
    FCarPosition: TVector2;
    FCarAngle: Single;
    FLines: Array Of TLine;
    FLabelLines: Array Of TLine;
    fAreaWidth: Integer;
    fAreaHeight: Integer;
    FParkarea: TOOBB;
    Function ColwithOOBB(oobb: TOOBB): Boolean;
    Function BoundinBound(oobb1, oobb2: TOOBB): Boolean;
  public
    RenderStartPos: Boolean;
    Property AreaWidth: Integer read fAreaWidth;
    Property AreaHeight: Integer read fAreaHeight;
    Constructor create;
    Destructor destroy; override;
    Procedure Clear;
    Procedure Render;
    Procedure SetCar(Const Car: TCar); // setzt das Auto auf die "Start" Position
    Procedure SetCarPosition(Pos: TVector2; Angle: Single);
    Procedure SafeToFile(Filename: String);
    Procedure LoadFromFile(Filename: String);
    Procedure AddLine(Start, Goal: TVector2; Color: TRGB);
    Procedure AddLabelLine(Start, Goal: TVector2; Color: TRGB);
    Procedure DelLine(Start, Goal: TVector2);
    Function Collision(Const Car: TCar): Boolean;
    Procedure SetParkArea(Position: TVector2; Width, Length, Angle: Single);
    Function Finished(Const Car: TCar): Boolean;
    Function FindToleanzPoint(P: TPoint; Toleranz: Single): TPoint;
  End;

Const
  // Die Nummern Entsprechen dem Itemindex der Combobox von Unit3
  EinachsenAnhaenger: integer = 0;
  ZweiAchsenAnhaenger: integer = 1;

Var
  AppWidth, AppHeight: Integer;
  GameState: TGameState = gsWait;

Procedure Go2d();
Procedure Exit2d();

Implementation

Uses lazutf8;

Const
  FLoatToleranze: Single = 1.0;

Function CompareV2(a, b: TVector2; Tolerance: Single): Boolean;
Begin
  result := LenV2(b - a) <= Tolerance;
End;

Procedure Go2d();
Begin
  glMatrixMode(GL_PROJECTION);
  glPushMatrix(); // Store The Projection Matrix
  glLoadIdentity(); // Reset The Projection Matrix
  //  glOrtho(0, 640, 0, 480, -1, 1); // Set Up An Ortho Screen
  If (GameState = gsEditCar) Or
    (GameState = gsEditTrailer)
    Then Begin
    glOrtho(0, 200, 200, 0, -1, 1); // Set Up An Ortho Screen
  End
  Else Begin
    glOrtho(0, AppWidth, Appheight, 0, -1, 1); // Set Up An Ortho Screen
  End;
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix(); // Store old Modelview Matrix
  glLoadIdentity(); // Reset The Modelview Matrix
End;

Procedure Exit2d();
Begin
  glMatrixMode(GL_PROJECTION);
  glPopMatrix(); // Restore old Projection Matrix
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix(); // Restore old Projection Matrix
End;

{ TCar }

Constructor TCar.create;
Begin
  Inherited create;
  Alpha := 45; // Maximaler "Lenk" winkel für das Fahrzeug
  H1 := 5; // siehe TCar.png
  H2 := 55; // siehe TCar.png
  H3 := 50; // siehe TCar.png
  H4 := 58; // siehe TCar.png
  W1 := 30; // siehe TCar.png
  W2 := 30; // siehe TCar.png
  FAnkerPosition := v2(100, 20);
  Fposition := v2(100, 20 + H3);
  FCarDir := -90;
  Fangle := 0;
  Speed := 0;
  Anhaenger := Nil;
End;

Destructor TCar.destroy;
Begin
  Inherited destroy;
End;

Function TCar.GetOOBB: TOOBB;
Var
  dir_, diro_: TVector2;
Begin
  // Richtung
  dir_ := normv2(SubV2(FAnkerPosition, FPosition));
  // Orthogonale Richtung
  diro_ := ScaleV2(w1 / 2, normv2(v2(dir_.y, -dir_.x)));
  // Setzen der OOBB-Koordinaten
  result[0] := addv2(FAnkerPosition, addv2(scalev2(h1, dir_), diro_));
  result[1] := addv2(FAnkerPosition, addv2(scalev2(-h2, dir_), diro_));
  result[2] := addv2(FAnkerPosition, subv2(scalev2(-h2, dir_), diro_));
  result[3] := addv2(FAnkerPosition, subv2(scalev2(h1, dir_), diro_));
End;

Function TCar.GetAnhaengerAnker: TVector2;
Var
  d: TVector2;
Begin
  d := normv2(subv2(FPosition, FAnkerPosition));
  result := addv2(FAnkerPosition, scalev2(h4, d));
End;

Procedure TCar.RenderWheel(Angle_: Single);
Const
  steps = 10;
Var
  wb, wh: Single;
  i: Integer;
Begin
  If Angle_ <> 0 Then Begin
    glpushmatrix;
    glRotatef(angle_, 0, 0, 1);
  End;
  wb := W2 / 8;
  wh := H3 / 5;
  glcolor3f(0.5, 0.5, 0.5);
  glbegin(GL_TRIANGLE_FAN);
  glVertex2f(0, 0);
  For i := 0 To steps Do Begin
    glvertex2f(wb * cos(i * 2 * pi / steps), wh * sin(i * 2 * pi / steps));
  End;
  glend;
  glcolor3f(0.0, 0.0, 0.0);
  glbegin(GL_Line_loop);
  For i := 0 To steps Do Begin
    glvertex2f(wb * cos(i * 2 * pi / steps), wh * sin(i * 2 * pi / steps));
  End;
  glend;
  If Angle_ <> 0 Then Begin
    glpopmatrix;
  End;
End;

Procedure TCar.SetAngle(Value: Single);
Begin
  If (value >= -Alpha) And (value <= Alpha) Then
    Fangle := value;
End;

Procedure Tcar.SetCarDir(Value: Single);
Var
  dir: TVector2;
Begin
  dir := normv2(v2(cos(degtorad(value)), sin(degtorad(value))));
  // Nachziehen der Hinterräder
  Fposition := subv2(FAnkerPosition, ScaleV2(h3, dir));
  FCarDir := value;
End;

Procedure TCar.Render;
//Var
//  f: TVector2;
//  ob: TOOBB;
Begin
  glpushmatrix;
  glLineWidth(1);
  glcolor3f(0, 0, 0);
  // Die Karosse
  gltranslatef(FAnkerPosition.x, FAnkerPosition.y, 0);
  glRotatef(FCarDir + 90, 0, 0, 1);
  glbegin(gl_lines);
  glVertex2f(0, 0);
  glVertex2f(0, h3);
  glVertex2f(-w2 / 2, 0);
  glVertex2f(w2 / 2, 0);
  glVertex2f(-w2 / 2, h3);
  glVertex2f(w2 / 2, h3);
  glend;
  // Die Hinterräder
  glpushmatrix;
  gltranslatef(w2 / 2, h3, 0);
  RenderWheel(0);
  gltranslatef(-w2, 0, 0);
  RenderWheel(0);
  glpopmatrix;
  // Die Vorderräder
  glpushmatrix;
  gltranslatef(w2 / 2, 0, 0);
  RenderWheel(Fangle);
  gltranslatef(-w2, 0, 0);
  RenderWheel(Fangle);
  glpopmatrix;
  // Object Orientierte Bounding Box
  //  glcolor3f(0.25, 0.25, 0.25);
  glcolor3f(0.0, 0.5, 0.5);
  glbegin(gl_Line_Loop);
  glVertex2f(-w1 / 2, -h1);
  glVertex2f(w1 / 2, -h1);
  glVertex2f(w1 / 2, h2);
  glVertex2f(-w1 / 2, h2);
  glend;
  // Anhängerkupplung
  If Assigned(Anhaenger) Or ForceRenderAnhaengerKupplung Then Begin
    glpushmatrix;
    gltranslatef(0, h4, 0);
    glcolor3f(0, 0, 0);
    glbegin(gl_Line_Loop);
    glVertex2f(0, -5);
    glVertex2f(-5, 0);
    glVertex2f(0, 5);
    glVertex2f(5, 0);
    glend;
    glpopmatrix;
  End;
  glpopmatrix;
  If Assigned(Anhaenger) Then
    Anhaenger.Render;
  //  f := AnhaengerAnker;
  //    ob := GetOOBB;
  //    glcolor3f(1, 0, 0);
  //    glbegin(GL_POINTS);
  //  glvertex2fv(@f);
  //    glVertex2fv(@ob[0]);
  //    glVertex2fv(@ob[1]);
  //    glVertex2fv(@ob[2]);
  //    glVertex2fv(@ob[3]);
  //    glend;
End;

Procedure TCar.Move(dt: Single);
Var
  dir, geardir, op, ofp: TVector2;
Begin
  If (speed <> 0) And (dt <> 0) Then Begin
    op := FAnkerPosition;
    ofp := FPosition;
    dir := normv2(subv2(op, ofp));
    FCardir := radtodeg(arctan2(dir.y, dir.x));
    // Bewegung der Vorderräder
    geardir := v2(cos(degtorad(Fangle + FCardir)), sin(degtorad(Fangle + FCardir)));
    geardir := Normv2(geardir);
    //    FAnkerPosition := addv2(op, ScaleV2(dt * Speed, geardir));
    FAnkerPosition := op + dt * Speed * geardir;
    // Nachziehen der Hinterräder
    Fposition := subv2(op, ScaleV2(h3, dir));
    If Assigned(Anhaenger) Then
      Anhaenger.move;
  End;
End;

Procedure TCar.LoadFromStream(Const Stream: TStream);
Begin
  stream.read(Alpha, sizeof(Alpha));
  stream.read(h1, sizeof(h1));
  stream.read(h2, sizeof(h2));
  stream.read(h3, sizeof(h3));
  stream.read(h4, sizeof(h4));
  stream.read(w1, sizeof(w1));
  stream.read(w2, sizeof(w2));
End;

Procedure TCar.SaveToStream(Const Stream: TStream);
Begin
  stream.Write(Alpha, sizeof(Alpha));
  stream.Write(h1, sizeof(h1));
  stream.Write(h2, sizeof(h2));
  stream.Write(h3, sizeof(h3));
  stream.Write(h4, sizeof(h4));
  stream.Write(w1, sizeof(w1));
  stream.Write(w2, sizeof(w2));
End;

{ TBaseAnhaenger }

Constructor TBaseAnhaenger.Create(Owner: TBaseVehicle);
Begin
  Inherited create;
  FOwner := Owner;
  If assigned(FOwner) Then
    FOwner.Anhaenger := self;
  Anhaenger := Nil;
End;

Destructor TBaseAnhaenger.destroy;
Begin
  Inherited Destroy;
End;

Procedure TBaseAnhaenger.Render;
Begin
  Raise Exception.create('Error forgot to implement : ' + ClassName + '.Render.');
End;

Procedure TBaseAnhaenger.SetAnhaenger;
Begin
  Raise Exception.create('Error forgot to implement : ' + ClassName + '.SetAnhaenger.');
End;

Procedure TBaseAnhaenger.Move;
Begin
  Raise Exception.create('Error forgot to implement : ' + ClassName + '.Move.');
End;

Procedure TBaseAnhaenger.LoadFromFile(FilenameString: String);
Begin
  Raise Exception.create('LoadFromFile not allowed in Class "' + ClassName + '"');
End;

{$WARNINGS OFF}

Function TBaseAnhaenger.GetOOBB: TOOBB;
Begin
  Raise Exception.create('Error forgot to implement : ' + ClassName + '.GetOOBB.');
End;
{$WARNINGS ON}

Procedure TBaseAnhaenger.RenderWheel(Width, Height, Angle_: Single);
Const
  steps = 10;
Var
  wb, wh: Single;
  i: Integer;
Begin
  If Angle_ <> 0 Then Begin
    glpushmatrix;
    glRotatef(angle_, 0, 0, 1);
  End;
  wb := Width / 8;
  wh := Height / 5;
  glcolor3f(0.5, 0.5, 0.5);
  glbegin(GL_TRIANGLE_FAN);
  glVertex2f(0, 0);
  For i := 0 To steps Do Begin
    glvertex2f(wb * cos(i * 2 * pi / steps), wh * sin(i * 2 * pi / steps));
  End;
  glend;
  glcolor3f(0.0, 0.0, 0.0);
  glbegin(GL_Line_loop);
  For i := 0 To steps Do Begin
    glvertex2f(wb * cos(i * 2 * pi / steps), wh * sin(i * 2 * pi / steps));
  End;
  glend;
  If Angle_ <> 0 Then Begin
    glpopmatrix;
  End;
End;

{ TArea }

Function TArea.ColwithOOBB(oobb: TOOBB): Boolean;
Var
  i: Integer;
  d1, d2, d3, d4: TVector2;
Begin
  result := false;
  // Check mit den Level Grenzen
  For i := 0 To 3 Do Begin
    If oobb[i].x < 0 Then result := true;
    If oobb[i].y < 0 Then result := true;
    If oobb[i].x > AreaWidth Then result := true;
    If oobb[i].y > AreaHeight Then result := true;
  End;
  If result Then exit;
  // die ersten 4 Linien sind die "Randbegrenzung" die berechnen wir anders
  For i := 4 To high(Flines) Do Begin
    result := result
      Or IntersectLine_segments(flines[i].Start, flines[i].Goal, oobb[0], oobb[1], d1)
      Or IntersectLine_segments(flines[i].Start, flines[i].Goal, oobb[1], oobb[2], d2)
      Or IntersectLine_segments(flines[i].Start, flines[i].Goal, oobb[2], oobb[3], d3)
      Or IntersectLine_segments(flines[i].Start, flines[i].Goal, oobb[3], oobb[0], d4);
    If result Then exit;
  End;
End;

Function TArea.BoundinBound(oobb1, oobb2: TOOBB): Boolean;
Var
  i: Integer;
Begin
  result := True;
  For i := 0 To 3 Do Begin
    result := result And PointInOOBB(oobb1[i], oobb2[0], oobb2[1], oobb2[2], oobb2[3]);
  End;
End;

Constructor TArea.create;
Const
  w = 639;
  h = 452;
Begin
  Inherited Create;
  fAreaWidth := w;
  fAreaHeight := h;
  RenderStartPos := false;
  setlength(FLabelLines, 0);
  setlength(FLines, 0);
  AddLine(v2(0, 0), v2(w, 0), ColorToRGB(clblack));
  AddLine(v2(w, 0), v2(w, h), ColorToRGB(clblack));
  AddLine(v2(w, h), v2(0, h), ColorToRGB(clblack));
  AddLine(v2(0, h), v2(0, 0), ColorToRGB(clblack));
End;

Destructor TArea.destroy;
Begin

End;

Procedure TArea.Clear;
Begin
  setlength(Flines, 4);
  setlength(FLabelLines, 0);
  SetParkArea(v2(100, 100), 50, 100, 0);
  SetCarPosition(v2(100, 200), 0);
End;

Procedure TArea.Render;
Var
  i: Integer;
Begin
  // Die Parkplatzfläche
  glcolor3f(0.75, 0.75, 0.75);
  glbegin(GL_TRIANGLE_STRIP);
  glVertex2fv(@FParkarea[0]);
  glVertex2fv(@FParkarea[3]);
  glVertex2fv(@FParkarea[1]);
  glVertex2fv(@FParkarea[2]);
  glend;
  glcolor3f(0.5, 0.5, 0.5);
  glbegin(GL_Line_Loop);
  glVertex2fv(@FParkarea[0]);
  glVertex2fv(@FParkarea[1]);
  glVertex2fv(@FParkarea[2]);
  glVertex2fv(@FParkarea[3]);
  glend;
  // Die Linien im Level
  glbegin(GL_LINES);
  For i := 0 To High(Flines) Do Begin
    glColor3ubv(@Flines[i].Color);
    glVertex2fv(@Flines[i].Start);
    glVertex2fv(@Flines[i].Goal);
  End;
  For i := 0 To High(FLabelLines) Do Begin
    glColor3ubv(@FLabelLines[i].Color);
    glVertex2fv(@FLabelLines[i].Start);
    glVertex2fv(@FLabelLines[i].Goal);
  End;
  glend;
  // Wenn der "Start" Pfeil gerendert werden soll
  If RenderStartPos Then Begin
    glcolor3f(1, 0, 0);
    glpushmatrix;
    glTranslatef(FCarPosition.x, FCarPosition.y, 0);
    glRotatef(FCarAngle, 0, 0, 1);
    glbegin(gl_lines);
    glVertex2f(0, 0);
    glVertex2f(50, 0);
    glVertex2f(50, 0);
    glVertex2f(40, 10);
    glVertex2f(50, 0);
    glVertex2f(40, -10);
    glend;
    glpopmatrix;
  End;
End;

Procedure TArea.SetCar(Const Car: TCar);
Begin
  car.Speed := 0;
  car.Angle := 0;
  car.FAnkerPosition := FCarPosition;
  car.CarAngle := FCarAngle;
  // Drehen, / Setzen des Anhängers
  If assigned(Car.Anhaenger) Then Begin
    Car.Anhaenger.setAnhaenger;
  End;
End;

Procedure TArea.SetCarPosition(Pos: TVector2; Angle: Single);
Begin
  FCarPosition := pos;
  FCarAngle := Angle;
End;

Procedure TArea.SafeToFile(Filename: String);
Var
  f: TFilestream;
  i: Integer;
Begin
  f := TFileStream.create(utf8tosys(Filename), fmcreate Or fmopenWrite);
  // Fahrzeug Start Position / Winkel
  f.Write(FCarPosition, sizeof(FCarPosition));
  f.Write(FCarAngle, sizeof(FCarAngle));
  // Die Col Lines
  i := high(FLines) + 1;
  f.Write(i, sizeof(i));
  For i := 0 To High(Flines) Do Begin
    f.Write(flines[i], sizeof(flines[i]));
  End;
  // Die Label Lines
  i := high(FLabelLines) + 1;
  f.Write(i, sizeof(i));
  For i := 0 To High(FLabelLines) Do Begin
    f.Write(FLabelLines[i], sizeof(FLabelLines[i]));
  End;
  // Die "Parkplatzdaten"
  f.Write(FParkarea, sizeof(FParkarea));


  f.free;
End;

Procedure TArea.LoadFromFile(Filename: String);
Var
  f: TFilestream;
  i: Integer;
Begin
  f := TFileStream.create(utf8tosys(Filename), fmopenRead);
  f.read(FCarPosition, sizeof(FCarPosition));
  f.read(FCarAngle, sizeof(FCarAngle));
  i := 0;
  f.read(i, sizeof(i));
  setlength(Flines, i);
  For i := 0 To High(Flines) Do Begin
    f.read(flines[i], sizeof(flines[i]));
  End;
  f.read(i, sizeof(i));
  setlength(FLabelLines, i);
  For i := 0 To High(FLabelLines) Do Begin
    f.read(FLabelLines[i], sizeof(FLabelLines[i]));
  End;
  f.Read(FParkarea, sizeof(FParkarea));

  f.free;
End;

Procedure TArea.AddLine(Start, Goal: TVector2; Color: TRGB);
Var
  i: Integer;
  b: Boolean;
Begin
  b := True;
  // Jede Linie wird nur 1 mal hinzugefügt
  For i := 0 To High(FLines) Do Begin
    If (Comparev2(start, flines[i].Start, FLoatToleranze) And Comparev2(Goal, flines[i].Goal, FLoatToleranze)) Or
      (Comparev2(start, flines[i].Goal, FLoatToleranze) And Comparev2(goal, flines[i].Start, FLoatToleranze)) Then Begin
      b := false;
      break;
    End;
  End;
  If b Then Begin
    setlength(flines, high(flines) + 2);
    flines[high(Flines)].Color := Color;
    flines[high(Flines)].Start := Start;
    flines[high(Flines)].Goal := Goal;
  End;
End;

Procedure TArea.AddLabelLine(Start, Goal: TVector2; Color: TRGB);
Var
  i: Integer;
  b: Boolean;
Begin
  b := True;
  // Jede Linie wird nur 1 mal hinzugefügt
  For i := 0 To High(FLabelLines) Do Begin
    If (Comparev2(start, FLabelLines[i].Start, FLoatToleranze) And Comparev2(Goal, FLabelLines[i].Goal, FLoatToleranze)) Or
      (Comparev2(start, FLabelLines[i].Goal, FLoatToleranze) And Comparev2(goal, FLabelLines[i].Start, FLoatToleranze)) Then Begin
      b := false;
      break;
    End;
  End;
  If b Then Begin
    setlength(FLabelLines, high(FLabelLines) + 2);
    FLabelLines[high(FLabelLines)].Color := Color;
    FLabelLines[high(FLabelLines)].Start := Start;
    FLabelLines[high(FLabelLines)].Goal := Goal;
  End;
End;

Procedure TArea.DelLine(Start, Goal: TVector2);
Var
  i, j: Integer;
Begin
  (*
  0 - 3 = Äußere Begrenzung
  *)
  For i := 4 To High(FLines) Do Begin
    If (Comparev2(start, flines[i].Start, 5) And Comparev2(Goal, flines[i].Goal, 5)) Or
      (Comparev2(start, flines[i].Goal, 5) And Comparev2(goal, flines[i].Start, 5)) Then Begin
      For j := i To high(Flines) - 1 Do Begin
        Flines[j] := Flines[j + 1];
      End;
      setlength(flines, high(flines));
      Exit;
    End;
  End;
  // Löschen LabelLine
  For i := 0 To High(FLabelLines) Do Begin
    If (Comparev2(start, FLabelLines[i].Start, 5) And Comparev2(Goal, FLabelLines[i].Goal, 5)) Or
      (Comparev2(start, FLabelLines[i].Goal, 5) And Comparev2(goal, FLabelLines[i].Start, 5)) Then Begin
      For j := i To high(FLabelLines) - 1 Do Begin
        FLabelLines[j] := FLabelLines[j + 1];
      End;
      setlength(FLabelLines, high(FLabelLines));
      Exit;
    End;
  End;
End;

Function TArea.Collision(Const Car: TCar): Boolean;
Var
  vehicle2, vehicle: TBaseAnhaenger;
  o1, o2: TOOBB;
  vehicles: Array Of TBaseAnhaenger;
  i, j: Integer;
Begin
  o1 := Car.OOBB;
  result := ColwithOOBB(o1);
  // Testen Level mit allen Anhängern
  vehicle := car.Anhaenger;
  While Assigned(vehicle) Do Begin
    o2 := vehicle.OOBB;
    result := result Or ColwithOOBB(o2);
    // Testen des Cars mit allen Anhängern
    result := result Or
      IntersectOOBB(o1[0], o1[1], o1[2], o1[3],
      o2[0], o2[1], o2[2], o2[3]);
    vehicle := vehicle.Anhaenger;
  End;
  // Testen aller Anhänger mit einander
  vehicle := car.Anhaenger;
  If assigned(vehicle) Then Begin
    setlength(vehicles, 1);
    vehicles[0] := vehicle;
    vehicle2 := vehicle.Anhaenger;
    While assigned(Vehicle2) Do Begin
      setlength(vehicles, high(vehicles) + 2);
      vehicles[high(vehicles)] := vehicle2;
      vehicle2 := vehicle2.Anhaenger;
    End;
    For i := 0 To high(vehicles) - 1 Do Begin
      o1 := vehicles[i].OOBB;
      For j := i + 1 To high(vehicles) Do Begin
        o2 := vehicles[j].OOBB;
        result := result Or
          IntersectOOBB(o1[0], o1[1], o1[2], o1[3],
          o2[0], o2[1], o2[2], o2[3]);
      End;
    End;
    setlength(vehicles, 0);
  End;
End;

Procedure TArea.SetParkArea(Position: TVector2; Width, Length, Angle: Single);
Var
  dir, diro: TVector2;
Begin
  dir := ScaleV2(length, normv2(v2(cos(degtorad(angle)), sin(degtorad(angle)))));
  diro := ScaleV2(Width / 2, normv2(v2(dir.y, -dir.x)));
  FParkarea[0] := addv2(Position, diro);
  FParkarea[1] := addv2(Position, addv2(dir, diro));
  FParkarea[2] := addv2(Position, subv2(dir, diro));
  FParkarea[3] := subv2(Position, diro);
End;

Function TArea.Finished(Const Car: TCar): Boolean;
Var
  vehicle: TBaseAnhaenger;
Begin
  result := false;
  // Nur bei stehendem/ fast stehendem Auto ist "Finish"
  If abs(car.Speed) < 1 Then Begin
    result := BoundinBound(car.oobb, FParkarea);
    // Testen mit allen Anhängern
    vehicle := car.Anhaenger;
    While assigned(vehicle) And result Do Begin
      result := BoundinBound(vehicle.OOBB, FParkarea);
      vehicle := vehicle.Anhaenger;
    End;
  End;
End;

Function TArea.FindToleanzPoint(P: TPoint; Toleranz: Single): TPoint;
Var
  i: Integer;
  p_: TVector2;
Begin
  result := point(-1, -1);
  p_ := v2(p.x, p.y);
  For i := 0 To high(Flines) Do Begin
    If CompareV2(p_, flines[i].Start, Toleranz) Then Begin
      result := point(round(flines[i].Start.x), round(flines[i].Start.y));
      exit;
    End;
    If CompareV2(p_, flines[i].Goal, Toleranz) Then Begin
      result := point(round(flines[i].Goal.x), round(flines[i].Goal.y));
      exit;
    End;
  End;
  For i := 0 To high(FLabellines) Do Begin
    If CompareV2(p_, FLabellines[i].Start, Toleranz) Then Begin
      result := point(round(FLabellines[i].Start.x), round(FLabellines[i].Start.y));
      exit;
    End;
    If CompareV2(p_, FLabellines[i].Goal, Toleranz) Then Begin
      result := point(round(FLabellines[i].Goal.x), round(FLabellines[i].Goal.y));
      exit;
    End;
  End;
End;

{ TEinachsenAnhaenger }

Constructor TEinachsenAnhaenger.Create(Owner: TBaseVehicle);
Begin
  Inherited Create(Owner);
  H1 := 15; // siehe TEinAchsenAnhaenger.png
  H2 := 5; // siehe TEinAchsenAnhaenger.png
  H3 := 25; // siehe TEinAchsenAnhaenger.png
  H4 := 5; // siehe TEinAchsenAnhaenger.png
  w1 := 25; // siehe TEinAchsenAnhaenger.png
  w2 := 30; // siehe TEinAchsenAnhaenger.png
  Fdir := -90;
  FAnkerPosition := v2(100, 100);
  Fposition := v2(100, 100 + H3);
End;

Destructor TEinachsenAnhaenger.destroy;
Begin
  Inherited destroy;
End;

Function TEinachsenAnhaenger.GetOOBB: TOOBB;
Var
  dir_, diro_: TVector2;
Begin
  // Richtung
  dir_ := normv2(SubV2(FAnkerPosition, FPosition));
  // Orthogonale Richtung
  diro_ := ScaleV2(w1 / 2, normv2(v2(dir_.y, -dir_.x)));
  // Setzen der OOBB-Koordinaten
  result[0] := addv2(FPosition, addv2(scalev2(h1, dir_), diro_));
  result[1] := addv2(FPosition, addv2(scalev2(-h2, dir_), diro_));
  result[2] := addv2(FPosition, subv2(scalev2(-h2, dir_), diro_));
  result[3] := addv2(FPosition, subv2(scalev2(h1, dir_), diro_));
End;

Function TEinachsenAnhaenger.GetAnhaengerAnker: TVector2;
Var
  d: TVector2;
Begin
  d := normv2(subv2(FPosition, FAnkerPosition));
  result := addv2(FPosition, scalev2(h4, d));
End;

Procedure TEinachsenAnhaenger.Render;
//Var
//  ob: TOOBB;
Begin
  glPushMatrix;
  // Die Karosse
  gltranslatef(FPosition.x, FPosition.y, 0);
  glRotatef(Fdir + 90, 0, 0, 1);
  glcolor3f(0, 0, 0);
  glbegin(gl_lines);
  glVertex2f(0, 0);
  glVertex2f(0, -h3);
  glVertex2f(-w2 / 2, 0);
  glVertex2f(w2 / 2, 0);
  glend;
  // Die Räder
  glpushmatrix;
  gltranslatef(w2 / 2, 0, 0);
  RenderWheel(w2, h3, 0);
  gltranslatef(-w2, 0, 0);
  RenderWheel(w2, h3, 0);
  glpopmatrix;
  // Object Orientierte Bounding Box
  //  glcolor3f(0.25, 0.25, 0.25);
  glcolor3f(0.0, 0.5, 0.5);
  glbegin(gl_Line_Loop);
  glVertex2f(-w1 / 2, -h1);
  glVertex2f(w1 / 2, -h1);
  glVertex2f(w1 / 2, h2);
  glVertex2f(-w1 / 2, h2);
  glend;
  // Anhängerkupplung
  If Assigned(Anhaenger) Or ForceRenderAnhaengerKupplung Then Begin
    glpushmatrix;
    gltranslatef(0, h4, 0);
    glcolor3f(0, 0, 0);
    glbegin(gl_Line_Loop);
    glVertex2f(0, -5);
    glVertex2f(-5, 0);
    glVertex2f(0, 5);
    glVertex2f(5, 0);
    glend;
    glpopmatrix;
  End;
  glpopmatrix;
  If Assigned(Anhaenger) Then
    Anhaenger.Render;
  //  f := AnhaengerAnker;
  //  ob := GetOOBB;
  //  glcolor3f(1, 0, 0);
  //  glbegin(GL_POINTS);
  //  glvertex2fv(@f);
  //  glVertex2fv(@ob[0]);
  //  glVertex2fv(@ob[1]);
  //  glVertex2fv(@ob[2]);
  //  glVertex2fv(@ob[3]);
  //  glend;
End;

Procedure TEinachsenAnhaenger.SetAnhaenger;
Var
  dir: TVector2;
Begin
  // Übernehmen der Position
  FAnkerPosition := Fowner.AnhaengerAnker;
  dir := normv2(subv2(FOwner.FAnkerPosition, FAnkerPosition));
  FPosition := addv2(FAnkerPosition, scalev2(-h3, dir));
  Fdir := radtodeg(arctan2(dir.y, dir.x));
  // Weiterreichen der Daten an die angehängten Objekte
  If Assigned(Anhaenger) Then
    Anhaenger.SetAnhaenger;
End;

Procedure TEinachsenAnhaenger.Move;
Var
  dir, op, ofp: TVector2;
Begin
  op := FAnkerPosition;
  ofp := FPosition;
  dir := normv2(subv2(op, ofp));
  Fdir := radtodeg(arctan2(dir.y, dir.x));
  FAnkerPosition := FOwner.AnhaengerAnker;
  // Nachziehen der Räder
  Fposition := subv2(op, ScaleV2(h3, dir));
  If Assigned(Anhaenger) Then
    Anhaenger.move;
End;

Procedure TEinachsenAnhaenger.LoadFromStream(Const Stream: TStream);
Begin
  stream.read(h1, sizeof(h1));
  stream.read(h2, sizeof(h2));
  stream.read(h3, sizeof(h3));
  stream.read(h4, sizeof(h4));
  stream.read(w1, sizeof(w1));
  stream.read(w2, sizeof(w2));
End;

Procedure TEinachsenAnhaenger.SaveToStream(Const Stream: TStream);
Begin
  stream.Write(EinachsenAnhaenger, sizeof(EinachsenAnhaenger));
  stream.Write(h1, sizeof(h1));
  stream.Write(h2, sizeof(h2));
  stream.Write(h3, sizeof(h3));
  stream.Write(h4, sizeof(h4));
  stream.Write(w1, sizeof(w1));
  stream.Write(w2, sizeof(w2));
End;

{ TBaseVehicle }

{$WARNINGS OFF}

Function TBaseVehicle.GetAnhaengerAnker: TVector2;
Begin
  Raise Exception.create('Error forgot to implement : ' + ClassName + '.GetAnhaengerAnker.');
End;
{$WARNINGS ON}

Constructor TBaseVehicle.create;
Begin
  Inherited create;
  ForceRenderAnhaengerKupplung := false;
End;

Destructor TBaseVehicle.destroy;
Begin
  If assigned(Anhaenger) Then
    Anhaenger.Free;
End;

Procedure TBaseVehicle.LoadFromFile(Filename: String);
Var
  f: TFilestream;
Begin
  f := TFileStream.create(utf8tosys(Filename), fmopenread);
  LoadFromStream(f);
  f.free;
End;

Procedure TBaseVehicle.LoadFromStream(Const Stream: TStream);
Begin
  Raise Exception.create('Error forgot to implement : ' + ClassName + '.LoadFromStream.');
End;

Procedure TBaseVehicle.SaveToFile(Filename: String);
Var
  f: TFilestream;
Begin
  f := TFileStream.create(utf8tosys(Filename), fmcreate Or fmopenwrite);
  SaveToStream(f);
  f.free;
End;

Procedure TBaseVehicle.SaveToStream(Const Stream: TStream);
Begin
  Raise Exception.create('Error forgot to implement : ' + ClassName + '.SaveToStream.');
End;

{ TZweiAchsenAnhaenger }

Constructor TZweiAchsenAnhaenger.Create(Owner: TBaseVehicle);
Begin
  Inherited Create(Owner);
  H1 := 15; // siehe TZweiAchsenAnhaenger.png
  H2 := 55; // siehe TZweiAchsenAnhaenger.png
  H3 := 50; // siehe TZweiAchsenAnhaenger.png
  H4 := 55; // siehe TZweiAchsenAnhaenger.png
  H5 := 25; // siehe TZweiAchsenAnhaenger.png
  w1 := 25; // siehe TZweiAchsenAnhaenger.png
  w2 := 30; // siehe TZweiAchsenAnhaenger.png
  Fdir := -90;
  Fdir2 := -90;
  FAnkerPosition := v2(100, 50);
  Fposition := v2(100, 50 + H5);
  Fposition2 := v2(100, 50 + H4);
End;

Destructor TZweiAchsenAnhaenger.destroy;
Begin
  Inherited destroy;
End;

Function TZweiAchsenAnhaenger.GetOOBB: TOOBB;
Var
  dir_, diro_: TVector2;
Begin
  // Richtung
  dir_ := normv2(subv2(fposition, fposition2));
  // Orthogonale Richtung
  diro_ := ScaleV2(w1 / 2, normv2(v2(dir_.y, -dir_.x)));
  // Setzen der OOBB-Koordinaten
  result[0] := addv2(FPosition, addv2(scalev2(h1, dir_), diro_));
  result[1] := addv2(FPosition, addv2(scalev2(-h2, dir_), diro_));
  result[2] := addv2(FPosition, subv2(scalev2(-h2, dir_), diro_));
  result[3] := addv2(FPosition, subv2(scalev2(h1, dir_), diro_));
End;

Function TZweiAchsenAnhaenger.GetAnhaengerAnker: TVector2;
Var
  dir: TVector2;
Begin
  dir := normv2(subv2(fposition2, fposition));
  Result := addv2(Fposition, scalev2(h4, dir));
End;

Procedure TZweiAchsenAnhaenger.Render;
//Var
//  ob: TOOBB;
//  f: TVector2;
Begin
  glpushmatrix;
  gltranslatef(Fposition.x, fposition.y, 0);
  // Die Deixel
  glpushmatrix;
  glRotatef(Fdir + 90, 0, 0, 1);
  glColor3f(0, 0, 0);
  glbegin(gl_lines);
  glVertex2f(0, 0);
  glVertex2f(0, -h5);
  glVertex2f(-w2 / 2, 0);
  glVertex2f(w2 / 2, 0);
  glend;
  glpushmatrix;
  glTranslatef(w2 / 2, 0, 0);
  RenderWheel(w2, h3, 0);
  glpopmatrix;
  glpushmatrix;
  glTranslatef(-w2 / 2, 0, 0);
  RenderWheel(w2, h3, 0);
  glpopmatrix;
  glpopmatrix;
  // Der Wagen
  glpushmatrix;
  glRotatef(Fdir2 + 90, 0, 0, 1);
  glColor3f(0, 0, 0);
  glbegin(gl_lines);
  glvertex2f(0, 0);
  glvertex2f(0, h3);
  glvertex2f(-w2 / 2, h3);
  glvertex2f(w2 / 2, h3);
  glend;
  glpushmatrix;
  glTranslatef(w2 / 2, h3, 0);
  RenderWheel(w2, h3, 0);
  glpopmatrix;
  glpushmatrix;
  glTranslatef(-w2 / 2, h3, 0);
  RenderWheel(w2, h3, 0);
  glpopmatrix;
  // Die OOBB
  glcolor3f(0.0, 0.5, 0.5);
  glbegin(gl_Line_Loop);
  glVertex2f(-w1 / 2, -h1);
  glVertex2f(w1 / 2, -h1);
  glVertex2f(w1 / 2, h2);
  glVertex2f(-w1 / 2, h2);
  glend;
  // Anhängerkupplung
  If Assigned(Anhaenger) Or ForceRenderAnhaengerKupplung Then Begin
    glpushmatrix;
    gltranslatef(0, h4, 0);
    glcolor3f(0, 0, 0);
    glbegin(gl_Line_Loop);
    glVertex2f(0, -5);
    glVertex2f(-5, 0);
    glVertex2f(0, 5);
    glVertex2f(5, 0);
    glend;
    glpopmatrix;
  End;
  glpopmatrix;
  glpopmatrix;
  // Debugg
  //  f := AnhaengerAnker;
  //  ob := GetOOBB;
  //  glcolor3f(1, 0, 0);
  //  glbegin(GL_POINTS);
  //  glvertex2fv(@f);
  //  glVertex2fv(@ob[0]);
  //  glVertex2fv(@ob[1]);
  //  glVertex2fv(@ob[2]);
  //  glVertex2fv(@ob[3]);
  //  glend;
  If Assigned(Anhaenger) Then
    Anhaenger.Render;
End;

Procedure TZweiAchsenAnhaenger.SetAnhaenger;
Var
  dir: TVector2;
Begin
  // Übernehmen der Position
  FAnkerPosition := Fowner.AnhaengerAnker;
  dir := normv2(subv2(FOwner.FAnkerPosition, FAnkerPosition));
  FPosition := addv2(FAnkerPosition, scalev2(-h5, dir));
  Fdir := radtodeg(arctan2(dir.y, dir.x));
  fdir2 := fdir;
  FPosition2 := addv2(FAnkerPosition, scalev2(-h3, dir));
  // Weiterreichen der Daten an die angehängten Objekte
  If Assigned(Anhaenger) Then
    Anhaenger.SetAnhaenger;
End;

Procedure TZweiAchsenAnhaenger.Move;
Var
  dir, op, ofp, ofp2: TVector2;
Begin
  op := FAnkerPosition;
  ofp := FPosition;
  ofp2 := FPosition2;
  dir := normv2(subv2(op, ofp));
  Fdir := radtodeg(arctan2(dir.y, dir.x));
  FAnkerPosition := FOwner.AnhaengerAnker;
  // Nachziehen der Vorder
  Fposition := subv2(op, ScaleV2(h5, dir));
  dir := normv2(subv2(ofp, ofp2));
  Fdir2 := radtodeg(arctan2(dir.y, dir.x));
  // Nachziehen der Hinter
  Fposition2 := subv2(ofp, ScaleV2(h3, dir));
  // Weiterleiten an den Anhänger
  If Assigned(Anhaenger) Then
    Anhaenger.move;
End;

Procedure TZweiAchsenAnhaenger.LoadFromStream(Const Stream: TStream);
Begin
  stream.read(h1, sizeof(h1));
  stream.read(h2, sizeof(h2));
  stream.read(h3, sizeof(h3));
  stream.read(h4, sizeof(h4));
  stream.read(h5, sizeof(h5));
  stream.read(w1, sizeof(w1));
  stream.read(w2, sizeof(w2));
End;

Procedure TZweiAchsenAnhaenger.SaveToStream(Const Stream: TStream);
Begin
  stream.Write(ZweiAchsenAnhaenger, sizeof(ZweiAchsenAnhaenger));
  stream.Write(h1, sizeof(h1));
  stream.Write(h2, sizeof(h2));
  stream.Write(h3, sizeof(h3));
  stream.Write(h4, sizeof(h4));
  stream.Write(h5, sizeof(h5));
  stream.Write(w1, sizeof(w1));
  stream.Write(w2, sizeof(w2));
End;

End.

