(******************************************************************************)
(* uPhysik                                                         01.06.2024 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Physik engine that emulates lots of springs, that can collide*)
(*               against "collider"                                           *)
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
(*                                                                            *)
(******************************************************************************)
Unit uphysik;

{$MODE ObjFPC}{$H+}
{$MODESWITCH ADVANCEDRECORDS}

Interface

Uses
  Classes, SysUtils, uvectormath;

Const
  SimulationDelta: Single = 0.001; // Egal wie Groß dt in Simulate ist -> die Simulation findet immer in 1ms schritten statt !

  (*
   * Dämpfung des Systems, zur Simulation eines Luftwiderstandes / an die Umwelt "abgegebene" Energie
   * 0 = Keine Dämpfung = keine Verluste (das System schwingt ewig)
   * Je Größer der Wert, desto Krasser die Dämpfung alles über 1 ist aber irgendwie unsinnig
   *)
  Attenuation: Single = 0.25;

  Gravity: TVector2 = (x: 0; y: - 9.8);

Type

  TGridPoint = Record
    Pos: TVector2;
    Fixed: Boolean;
    Inertia: TVector2;
    Mass: Single;
  End;

  TSpring = Record
    P1, P2: Integer; // index to TGridpoint
    RestLen: Single; // If there is no force attached to the spring, then this is its len
    BreakForce: Single; // If the Inertia is more than this -> Break
    SpringConst: Single; // The "Force" the Spring is applying to if streched or compressed
  End;

  TCollider = Record
    p1, p2, p3: TVector2;
    Bouncines: Single; // 0 =  Bei Kollision, bleibt der Kollidierende quasi "Kleben", 1 = Voll Elastischer Stoß
  End;

  TOnRenderSpringEvent = Procedure(Sender: TObject; Const Spring: TSpring) Of Object; // TODO: Umstellen auf Index
  TOnRenderColliderEvent = Procedure(Sender: TObject; Const Collider: TCollider) Of Object; // TODO: Umstellen auf Index
  TOnCollideEvent = Procedure(Sender: TObject; GridPointIndex, ColliderIndex: integer; Normal: TVector2) Of Object;

  { TSpringEngine }

  TSpringEngine = Class
  private
    fGridPoints: Array Of TGridPoint; // Gridpunkte
    fSprings: Array Of TSpring; // Federn
    FCollider: Array Of TCollider;

    Function FindPoint(Const P: TVector2): Integer;
    Function AddPoint(Const P: TVector2; PointMass: Single): Integer;

    Function DoCollissionWith(PointIndex, ColliderIndex: Integer): TVector2;
    Procedure ProjectPointTo(Var P: TVector2; Const P1, P2: TVector2);

    Function GetGridPoint(index: integer): TGridPoint;
    Function GetCollider(index: integer): TCollider;

    Procedure SetCollider(index: integer; AValue: TCollider);
    Procedure SetGridPoint(index: integer; AValue: TGridPoint);
  public
    OnRenderSpringEvent: TOnRenderSpringEvent;
    OnRenderColliderEvent: TOnRenderColliderEvent;
    OnCollideEvent: TOnCollideEvent;
    OnSpringBreak: TNotifyEvent;

    Property GridPoint[index: integer]: TGridPoint read GetGridPoint write SetGridPoint;
    Property Collider[index: integer]: TCollider read GetCollider write SetCollider;

    Procedure SetFixed(index: Integer; aValue: Boolean);

    Constructor Create;
    Destructor Destroy; override;

    Procedure Clear;

    Procedure AddSpring(p1, p2: TVector2; Mass, SpringConst, SpringBreakForce: Single);
    Procedure AddCollider(p1, p2, p3: TVector2; Bouncines: Single); overload; // Always fixed
    Procedure AddCollider(c: TCollider); overload;

    Procedure ResetInertias;

    Procedure Simulate(dt: Single);
    Procedure Render;
  End;

Type

  { TColliderhelper }

  TColliderhelper = Record Helper For TCollider
    Procedure Translate(v: TVector2);
    Function Clone: TCollider;
    Procedure Rotate(Angle: Single);
  End;

Implementation

{ TSpringEngine }

Constructor TSpringEngine.Create;
Begin
  Inherited Create;
  fSprings := Nil;
  fGridPoints := Nil;
  FCollider := Nil;
  OnRenderSpringEvent := Nil;
  OnRenderColliderEvent := Nil;
  OnCollideEvent := Nil;
  OnSpringBreak := Nil;
End;

Destructor TSpringEngine.Destroy;
Begin
  Clear;
End;

Function TSpringEngine.GetGridPoint(index: integer): TGridPoint;
Begin
  If (index >= 0) And (index < length(fGridPoints)) Then Begin
    result := fGridPoints[index];
  End
  Else Begin
    Raise Exception.Create('TSpringEngine.GetGridPoint: Error, out of bounds');
  End;
End;

Procedure TSpringEngine.SetGridPoint(index: integer; AValue: TGridPoint);
Begin
  If (index >= 0) And (index < length(fGridPoints)) Then Begin
    fGridPoints[index] := AValue;
  End
  Else Begin
    Raise Exception.Create('TSpringEngine.SetGridPoint: Error, out of bounds');
  End;
End;

Function TSpringEngine.GetCollider(index: integer): TCollider;
Begin
  If (index >= 0) And (index < length(FCollider)) Then Begin
    result := FCollider[index];
  End
  Else Begin
    Raise Exception.Create('TSpringEngine.GetCollider: Error, out of bounds');
  End;
End;

Procedure TSpringEngine.SetCollider(index: integer; AValue: TCollider);
Begin
  If (index >= 0) And (index < length(FCollider)) Then Begin
    FCollider[index] := AValue;
  End
  Else Begin
    Raise Exception.Create('TSpringEngine.SetCollider: Error, out of bounds');
  End;
End;

Procedure TSpringEngine.SetFixed(index: Integer; aValue: Boolean);
Begin
  If (index >= 0) And (index < length(fGridPoints)) Then Begin
    fGridPoints[index].Fixed := AValue;
  End
  Else Begin
    Raise Exception.Create('TSpringEngine.SetFixed: Error, out of bounds');
  End;
End;

Function TSpringEngine.FindPoint(Const P: TVector2): Integer;
Var
  i: Integer;
Begin
  result := -1;
  For i := 0 To high(fGridPoints) Do Begin
    If (abs(P.x - fGridPoints[i].Pos.x) <= Epsilon) And
      (abs(P.y - fGridPoints[i].Pos.y) <= Epsilon) Then Begin
      result := i;
      exit;
    End;
  End;
End;

Function TSpringEngine.AddPoint(Const P: TVector2; PointMass: Single): Integer;
Begin
  result := findpoint(p);
  If result = -1 Then Begin
    setlength(fGridPoints, high(fGridPoints) + 2);
    result := high(fGridPoints);
    fGridPoints[result].Pos := p;
    fGridPoints[result].Fixed := false;
    fGridPoints[result].Inertia := ZeroV2();
    fGridPoints[result].Mass := PointMass;
  End
  Else Begin
    fGridPoints[result].Mass := fGridPoints[Result].Mass + PointMass;
  End;
End;

Function TSpringEngine.DoCollissionWith(PointIndex, ColliderIndex: Integer
  ): TVector2;
Var
  Center: TVector2;
Begin
  Center := (FCollider[ColliderIndex].p1 + FCollider[ColliderIndex].p2 + FCollider[ColliderIndex].p3) / 3;
  result := ZeroV2();
  If PointInTriangle(fGridPoints[PointIndex].Pos, FCollider[ColliderIndex].p1, FCollider[ColliderIndex].p2, Center) Then Begin
    // Collide with 1,2,Center
    ProjectPointTo(fGridPoints[PointIndex].Pos, FCollider[ColliderIndex].p1, FCollider[ColliderIndex].p2);
    result := v2(-(FCollider[ColliderIndex].p2.y - FCollider[ColliderIndex].p1.y), FCollider[ColliderIndex].p2.x - FCollider[ColliderIndex].p1.x);
  End
  Else Begin
    If PointInTriangle(fGridPoints[PointIndex].Pos, FCollider[ColliderIndex].p2, FCollider[ColliderIndex].p3, Center) Then Begin
      // Collide with 2,3,Center
      ProjectPointTo(fGridPoints[PointIndex].Pos, FCollider[ColliderIndex].p2, FCollider[ColliderIndex].p3);
      result := v2(-(FCollider[ColliderIndex].p3.y - FCollider[ColliderIndex].p2.y), FCollider[ColliderIndex].p3.x - FCollider[ColliderIndex].p2.x);
    End
    Else Begin
      // Collide with 3,1,Center
      ProjectPointTo(fGridPoints[PointIndex].Pos, FCollider[ColliderIndex].p3, FCollider[ColliderIndex].p1);
      result := v2(-(FCollider[ColliderIndex].p1.y - FCollider[ColliderIndex].p3.y), FCollider[ColliderIndex].p1.x - FCollider[ColliderIndex].p3.x);
    End;
  End;
  result := NormV2(result);
End;


(*
 * Projiziert den Punkt P auf den Lotfuspunkt der Geraden durch P1 und P2
 *
 * Das Hört sich kompliziert an, ist in 2D aber tatsächlich recht einfach ;)
 *)

Procedure TSpringEngine.ProjectPointTo(Var P: TVector2; Const P1, P2: TVector2);
Begin
  p := CalculatePlumbFootPoint(p, P1, P2);
End;

Procedure TSpringEngine.Clear;
Begin
  setlength(fSprings, 0);
  setlength(fGridPoints, 0);
  setlength(FCollider, 0);
End;

Procedure TSpringEngine.AddSpring(p1, p2: TVector2; Mass, SpringConst,
  SpringBreakForce: Single);
Var
  index1, index2, i: Integer;
Begin
  index1 := AddPoint(p1, Mass / 2);
  index2 := AddPoint(p2, Mass / 2);
  // Check, if Spring already exists.
  For i := 0 To high(fSprings) Do Begin
    If ((fSprings[i].P1 = index1) And (fSprings[i].P2 = index2)) Or
      ((fSprings[i].P2 = index1) And (fSprings[i].P1 = index2)) Then exit;
  End;
  setlength(fSprings, high(fSprings) + 2);
  fSprings[high(fSprings)].P1 := index1;
  fSprings[high(fSprings)].P2 := index2;
  fSprings[high(fSprings)].RestLen := LenV2(fGridPoints[index1].Pos - fGridPoints[index2].Pos);
  fSprings[high(fSprings)].BreakForce := SpringBreakForce;
  fSprings[high(fSprings)].SpringConst := SpringConst;
End;

Procedure TSpringEngine.AddCollider(p1, p2, p3: TVector2; Bouncines: Single);
Begin
  setlength(FCollider, high(FCollider) + 2);
  FCollider[high(FCollider)].p1 := p1;
  FCollider[high(FCollider)].p2 := p2;
  FCollider[high(FCollider)].p3 := p3;
  FCollider[high(FCollider)].Bouncines := Bouncines;
End;

Procedure TSpringEngine.AddCollider(c: TCollider);
Begin
  setlength(FCollider, high(FCollider) + 2);
  FCollider[high(FCollider)] := c;
End;

Procedure TSpringEngine.ResetInertias;
Var
  i: Integer;
Begin
  For i := 0 To high(fGridPoints) Do Begin
    fGridPoints[i].Inertia := ZeroV2();
  End;
End;

Procedure TSpringEngine.Simulate(dt: Single);
Var
  Oversample, i, j, k: Integer;
  Normal, MoveDir, p1, p2, Distance: TVector2;
  DistanceLen, Force: Single;
Begin
  For Oversample := 0 To round(dt / SimulationDelta) - 1 Do Begin
    // Reset "inertia's"
    For i := 0 To High(fGridPoints) Do Begin
      fGridPoints[i].inertia := fGridPoints[i].inertia + (SimulationDelta * fGridPoints[i].mass) * Gravity;
    End;
    // Summ all forces to each point
    For i := high(fSprings) Downto 0 Do Begin
      p1 := fGridPoints[fSprings[i].P1].Pos;
      p2 := fGridPoints[fSprings[i].P2].Pos;
      Distance := (p2 - p1);
      DistanceLen := LenV2(Distance);
      // Check if the Spring breaks, due too much stretching..
      Force := (DistanceLen - fSprings[i].RestLen) * fSprings[i].SpringConst;
      If Force >= fSprings[i].BreakForce Then Begin
        If assigned(OnSpringBreak) Then Begin
          OnSpringBreak(self);
        End;
        For k := i To high(fSprings) - 1 Do Begin
          fSprings[k] := fSprings[k + 1];
        End;
        setlength(fSprings, high(fSprings));
        Continue;
      End;
      MoveDir := NormV2(Distance);
      // Now Apply the Forces to the attached points, taking into account if a point is fixed
      If fGridPoints[fSprings[i].P1].Fixed Then Begin
        If Not fGridPoints[fSprings[i].P2].Fixed Then Begin
          // P1 = Fixed, P2 = Moveable
          fGridPoints[fSprings[i].P2].Inertia := fGridPoints[fSprings[i].P2].Inertia - 1 * Force * MoveDir;
        End;
      End
      Else Begin
        If fGridPoints[fSprings[i].P2].Fixed Then Begin
          // P1 = Moveable, P2 = Fixed
          fGridPoints[fSprings[i].P1].Inertia := fGridPoints[fSprings[i].P1].Inertia + 1 * Force * MoveDir;
        End
        Else Begin
          // P1 = Moveable, P2 = Moveable
          fGridPoints[fSprings[i].P1].Inertia := fGridPoints[fSprings[i].P1].Inertia + 0.5 * Force * MoveDir;
          fGridPoints[fSprings[i].P2].Inertia := fGridPoints[fSprings[i].P2].Inertia - 0.5 * Force * MoveDir;
        End;
      End;
    End;
    // Now Move the Points according to their iniertia's
    For i := 0 To high(fGridPoints) Do Begin
      If Not fGridPoints[i].Fixed Then Begin
        fGridPoints[i].Pos := fGridPoints[i].Pos + SimulationDelta * fGridPoints[i].Inertia;
        // Einrechnen des "Luftwiderstandes"
        fGridPoints[i].Inertia := fGridPoints[i].Inertia * (1 - Attenuation * SimulationDelta);
      End;
    End;
    // All Move is done -> Check the collides
    For i := 0 To high(fGridPoints) Do Begin
      For j := 0 To high(FCollider) Do Begin
        If PointInTriangle(fGridPoints[i].Pos, FCollider[j].p1, FCollider[j].p2, FCollider[j].p3) Then Begin
          Normal := DoCollissionWith(i, j);
          If assigned(OnCollideEvent) Then Begin
            // What ever: der User will seine eigene Umrechnung der Inertias machen
            OnCollideEvent(self, i, j, Normal);
          End
          Else Begin
            // Default
            // 1. Normale um 90 Grad drehen
            normal := v2(normal.y, -Normal.x);
            // 2. Projizieren
            Distance := HadamardV2(Normal, fGridPoints[i].Inertia) - fGridPoints[i].Inertia;
            fGridPoints[i].Inertia := fGridPoints[i].Inertia + (1 + FCollider[j].Bouncines) * Distance;
          End;
        End;
      End;
    End;
  End;
End;

Procedure TSpringEngine.Render;
Var
  i: Integer;
Begin
  If Assigned(OnRenderSpringEvent) Then Begin
    For i := 0 To high(fSprings) Do Begin
      OnRenderSpringEvent(self, fSprings[i]);
    End;
  End;
  If assigned(OnRenderColliderEvent) Then Begin
    For i := 0 To high(FCollider) Do Begin
      OnRenderColliderEvent(self, FCollider[i]);
    End;
  End;
End;

{ TColliderhelper }

Procedure TColliderhelper.Translate(v: TVector2);
Begin
  self.p1 := self.p1 + v;
  self.p2 := self.p2 + v;
  self.p3 := self.p3 + v;
End;

Function TColliderhelper.Clone: TCollider;
Begin
  result := self;
End;

Procedure TColliderhelper.Rotate(Angle: Single);
Var
  c: TVector2;
  m: TMatrix2x2;
Begin
  c := (Self.p1 + Self.p2 + Self.p3) / 3;
  m := CalculateRotationMatrix(Angle);
  self.p1 := (m * (Self.p1 - c)) + c;
  self.p2 := (m * (Self.p2 - c)) + c;
  self.p3 := (m * (Self.p3 - c)) + c;
End;

End.

