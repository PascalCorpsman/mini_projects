(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of example PingPong                                      *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit elements;

{$MODE ObjFPC}{$H+}

Interface

Uses
  classes, math,
  LCLIntf, LCLType, // LMessages, // Trect
  graphics; // TCanvas, TColor

Type
  TBall = Class;

  TFpoint = Record
    x, y: Single;
  End;

  { TBall }

  TBall = Class
  private
    fvx, fvy, fx, fy, fr, fm: Single;
    fc: Tcolor;
    Function getpos: Tfpoint;
    Procedure Setpos(Value: TFpoint);
    Function getSpeed: Tfpoint;
    Procedure SetSpeed(Value: TFpoint);
  public
    Property Color: Tcolor read FC write fC;
    Property Radius: Single read fr;
    Property Position: Tfpoint read getpos write Setpos;
    Property SpeedVektor: TFpoint read getSpeed write SetSpeed;
    Property Mass: Single read Fm write fm;

    Constructor Create(Position_, SpeedVektor_: TFpoint; Radius_, Mass_: single);
    Destructor Destroy; override;

    Procedure Render(Const Canvas: TCanvas);
    Procedure CalculateMass;
    Procedure BorderCollision(CollisionRect: Trect; InsideCollision: Boolean = True);
    Procedure CollideWithOther(Const Ball2: TBall);
    Procedure Move;
    Procedure ApplyFriction(Friction: Single);
  End;

Function Point(x, y: Single): Tfpoint;
Function Rect(ALeft, ATop, ARight, ABottom: Integer): Trect;

Implementation

Function Point(x, y: Single): Tfpoint;
Begin
  result.x := x;
  result.y := y;
End;

Function Rect(ALeft, ATop, ARight, ABottom: Integer): Trect;
Begin
  With result Do Begin
    Left := aleft;
    Right := aright;
    Top := atop;
    bottom := abottom;
  End;
End;

Constructor TBall.Create(Position_, SpeedVektor_: TFpoint; Radius_,
  Mass_: single);
Begin
  Inherited Create;
  fc := Random(256 * 256 * 256); // zufällige Farbe für unsere Kugel.
  fx := position_.x;
  fy := position_.y;
  fr := radius_;
  fm := mass_;
  fvx := SpeedVektor_.x;
  fvy := SpeedVektor_.y;
End;

Destructor TBall.Destroy;
Begin
  //  Inherited Destroy; // Brauchen wir net da von Tobject abgeleitet
End;

Function TBall.getpos: Tfpoint;
Begin
  result.x := fx;
  result.y := fy;
End;

Procedure TBall.Setpos(Value: TFpoint);
Begin
  fx := value.x;
  fy := value.y;
End;

Function TBall.getSpeed: Tfpoint;
Begin
  result.x := fVx;
  result.y := fVy;
End;

Procedure TBall.SetSpeed(Value: TFpoint);
Begin
  fvx := value.x;
  fvy := value.y;
End;

Procedure TBall.Move;
Begin
  fx := fx + fvx;
  fy := fy + fvy;
End;

Procedure TBall.ApplyFriction(Friction: Single);
Begin
  SpeedVektor := Point(SpeedVektor.x * (1 - Friction), SpeedVektor.y * (1 - Friction));
End;

Function Tangens(Value: Extended): Extended;
Begin
  result := tan(degtorad(value));
End;

Function Sinus(E: Extended): Extended;
Begin
  Result := sin(degtorad(e));
End;

Function CoSinus(E: Extended): Extended;
Begin
  Result := cos(degtorad(e));
End;

Procedure Tausche(Var i1, i2: integer);
Var
  i3: integer;
Begin
  i3 := i1;
  i1 := i2;
  i2 := i3;
End;

Function arcTangens(x, y: Extended): Extended;
Begin
  Result := 0;
  If (x = 0) Then Begin
    If (Y >= 0) Then result := 90;
    If (Y < 0) Then result := 270;
  End
  Else Begin
    result := radtodeg(arctan(Y / X));
    If ((X < 0) And (y > 0)) Or ((x < 0) And (Y <= 0)) Then result := 180 + result;
    If (X > 0) And (Y < 0) Then result := 360 + result;
  End;
End;

Function EllipseRechteckcollision(E1, R1: Trect): boolean;
Type
  Tpunkt = Record
    X, Y: Extended;
  End;
Var
  SN1, SN2, X, Alpha: extended;
  p1, p2: Tpoint;
  N1, N2: Tpunkt;
  radius1, radius2: integer;
Begin
  result := false;
  If E1.left > E1.right Then tausche(E1.left, E1.right);
  If E1.Top > E1.bottom Then tausche(E1.top, E1.bottom);
  If r1.left > r1.right Then tausche(r1.left, r1.right);
  If r1.Top > r1.bottom Then tausche(r1.top, r1.bottom);
  p1.x := E1.left + ((E1.right - E1.left) Div 2);
  p1.y := E1.top + ((E1.Bottom - E1.top) Div 2);
  p2.x := r1.left + ((r1.right - r1.left) Div 2);
  p2.y := r1.top + ((r1.Bottom - r1.top) Div 2);
  Alpha := arcTangens(p1.x - p2.x, p1.y - p2.y);
  X := Hypot(p1.x - p2.x, p1.y - p2.y);
  Radius1 := p1.x - E1.left;
  Radius2 := p1.y - E1.top; // Radius 1 Horizontal, Radius2 Vertikal
  N1.X := cosinus(alpha) * radius1 + P1.X;
  N1.Y := sinus(Alpha) * radius2 + P1.Y;
  SN1 := Hypot(p1.x - n1.x, p1.y - n1.y);
  Case round(Alpha) Of
    0..45: Begin
        n2.x := R1.right;
        n2.y := round(tangens(alpha) * ((r1.Bottom - p2.y) / 2)) + p2.y;
      End;
    46..90: Begin
        n2.y := R1.top;
        n2.x := round(tangens(alpha - 45) * ((r1.right - p2.x) / 2)) + p2.x;
      End;
    91..135: Begin
        n2.y := R1.top;
        n2.x := round(tangens(alpha - 90) * ((r1.right - p2.x) / 2)) + p2.x;
      End;
    136..225: Begin
        n2.x := r1.left;
        n2.y := round(tangens(alpha) * ((r1.Bottom - p2.y) / 2)) + p2.y;
      End;
    226..270: Begin
        n2.y := r1.bottom;
        n2.x := round(tangens(alpha - 225) * ((r1.right - p2.x) / 2)) + p2.x;
      End;
    271..315: Begin
        n2.y := r1.bottom;
        n2.x := round(tangens(alpha - 270) * ((r1.right - p2.x) / 2)) + p2.x;
      End;
    316..360: Begin
        n2.x := R1.right;
        n2.y := round(tangens(alpha) * ((r1.Bottom - p2.y) / 2)) + p2.y;
      End;
  End;
  SN2 := Hypot(p2.x - n2.x, p2.y - n2.y);
  If (x <= (sn1 + Sn2)) Then result := true;
End;

Procedure TBall.BorderCollision(CollisionRect: Trect; InsideCollision: Boolean);
Begin
  If Insidecollision Then Begin // Bedeutet die Kugel befindet sich inherhalt des Rechtecks
    If ((fx - fr < CollisionRect.Left) And (fvx < 0)) Or ((fx + fr > CollisionRect.Right) And (fvx > 0)) Then fvx := -fvx;
    If ((fy - fr < CollisionRect.top) And (fvy < 0)) Or ((fy + fr > CollisionRect.Bottom) And (fvy > 0)) Then fvy := -fvy;
  End
  Else Begin // bedeutet die Kugel befindet sich auserhalb des Rechtecks
    If EllipseRechteckcollision(rect(round(fx - fr), round(fy - fr), round(fx + fr), round(fy + fr)), CollisionRect) Then Begin
      If ((fy < CollisionRect.top) And (fvy > 0)) Or ((fy > CollisionRect.Bottom) And (fvy < 0)) Then fvy := -fvy;
      If ((fx < CollisionRect.left) And (fvx > 0)) Or ((fx > CollisionRect.Right) And (fvx < 0)) Then fvx := -fvx;
    End;
  End;
End;

Procedure TBall.CollideWithOther(Const Ball2: TBall);
Var
  dx, dy, dxs, dys, L, // Die Variablen für den Abstand der beiden Kugeln
  M11, M21, M12, M22, // Die Variablen der Transformationsmatrix
  Vp1, Vp2, Vs1, Vs2, MTot, Vp1_, Vp2_: Single;
  tmpv: TFpoint;
Begin
  tmpv := ball2.Position; // Hohlen der position der anderen Kugel
  DX := tmpv.x - fx; // Delta x
  DY := tmpv.y - fy; // Delta y
  dxs := dx * dx; // Da wir ein wenig Zeitoptimiert arbeiten wollen speichern wir und die Quadrate zwischen
  dys := dy * dy; // Da wir ein wenig Zeitoptimiert arbeiten wollen speichern wir und die Quadrate zwischen
  l := fr + ball2.Radius; // die Strecke der beiden Radien Addiert
  // da l * l bestimmt schneller ist wie Wurzel ziehen machen wir das so, unter der Annahme das es möglichst selten eine Kollision gibt.
  // Im anderen Fall wäre die Berechnung von L vor dem If und dem Vergleich auf tr schneller.
  If dxs + dys <= l * l Then Begin
    L := sqrt(dxs + dys); // Abstand

    // Berechnen der Transformationsmatrix
    M11 := DX / L;
    M12 := -DY / L;
    M21 := DY / L;
    M22 := DX / L;

    // Koordinatentransformation teil 1
    tmpv := ball2.SpeedVektor;
    Vp1 := fVx * M11 + fVy * -M12;
    Vp2 := tmpv.x * M11 + tmpv.y * -M12;

    If Vp1 - Vp2 < 0 Then exit; // Bälle gehen bereits auseinander, dann Exit

    // Koordinatentransformation teil 2 , aus Optimierungsgründen hinter dem Exit.
    Vs1 := fVx * -M21 + fVy * M22;
    Vs2 := tmpv.x * -M21 + tmpv.y * M22;

    // das Verwurschteln der Massen
    MTot := fM + ball2.Mass;
    Vp1_ := (fM - ball2.Mass) / MTot * Vp1 + 2 * ball2.Mass / MTot * Vp2;
    Vp2_ := (ball2.Mass - fM) / MTot * Vp2 + 2 * fM / MTot * Vp1;

    // Rücktransformation
    fVx := Vp1_ * M11 + Vs1 * M12;
    fVy := Vp1_ * M21 + Vs1 * M22;
    tmpv := point(Vp2_ * M11 + Vs2 * M12, Vp2_ * M21 + Vs2 * M22);
    ball2.SpeedVektor := tmpv;
  End;
End;

Procedure TBall.CalculateMass;
Begin
  fm := 4 / 3 * fR * fR * fR * pi;
End;

Procedure TBall.Render(Const Canvas: TCanvas);
Begin
  With canvas Do Begin
    pen.color := fc;
    brush.color := fc;
    brush.style := bssolid;
    Ellipse(round(fx - fr), round(fy - fr), round(fx + fr), round(fy + fr));
  End;
End;

End.

