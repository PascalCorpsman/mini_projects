(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of SimpleRay                                             *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit raytracer_math;

{$MODE ObjFPC}{$H+}

Interface

Uses math;

Type

  DWord = Longword;

  TRayFloat = Single;
  //  TRayFloat = Double;
  //  TRayFloat = Extended;

  TVector2f = Record
    x, y: TRayfloat;
  End;

  TVector3f = Record
    x, y, z: TRayfloat;
  End;

  TVector4f = Record
    x, y, z, w: TRayfloat;
  End;

  Tvector2fDynArr = Array Of Tvector2f;

  (*
  ACHTUNG diese Werte wurden aufgrund Ettlicher Tests ermittelt.
  Die Echten Werte wie sie noch bei Double und Extended stehen
  Sind nicht Nutzbar. Da es sonst Access Violations hagelt ohne Ende !!
  ( für die Max werte, bei den Min werten werden die Collisionen nicht mehr erkannt.)
  *)
Var
  MinFloat: TRayFloat = 0.00001; // Gibt an ab welcher Minimalen Toleranz zwei Float werte als unterschiedlich erkannt werden
  MaxFloat: TRayFloat = 10000000000.0; // Nimmt man einen Größeren Wert gibts wieder AV's

Function V2(x, y: TRayfloat): Tvector2f; // Konvertierung
Function V3(x, y, z: TRayfloat): Tvector3f; // Konvertierung
Function V4(x, y, z, w: TRayfloat): Tvector4f; // Konvertierung
Function ScaleV2(Scalar: TRayFloat; Value: TVector2f): TVector2f; // Multiplivation mit einem Scalar
Function AddV2(Value1, Value2: TVector2f): TVector2f; // Addiert 2 Vectoren
Function SubV2(Value1, Value2: TVector2f): TVector2f; // Subtrahiert Value2 von Value1
Function LengthV3(Value: TVector3f): TRayFloat; // Gibt die Länge des Vectors zurück
Function LengthV3Sqr(Value: TVector3f): TRayFloat; // Gibt die Länge^2 des Vectors zurück
Function NormalizeV3(Value: TVector3f): TVector3f; // Scalliert den Vector so das die Länge = 1
Function ScaleV3(Scalar: TRayFloat; Value: TVector3f): TVector3f; // Multiplivation mit einem Scalar
Function AddV3(Value1, Value2: TVector3f): TVector3f; // Addiert 2 Vectoren
Function SubV3(Value1, Value2: TVector3f): TVector3f; // Subtrahiert  Value2 von Value1
Function Detv2(v1, v2: TVector2f): single;
Function DotProdv3(Value1, Value2: TVector3f): TRayFloat; // Berechnet das Skalarprodukt von Value1 und Value2
Function CrossProdV3(Value1, Value2: TVector3f): TVector3f; // Berechnet das Kreuzprodukt von Value1 und Value2
Function CompProdv3(Value1, Value2: TVector3f): TVector3f; // Berechnet die Komponentenweise Multiplikation zweier Vectoren
Function Determinante3x3(Value1, Value2, Value3: TVector3f): TRayFloat; // Berechnet die Determinante der 2x2 Matrix die die Vektoren Value 1 - 3 bilden
Function RotateV3(Value, Rotvector: TVector3f): TVector3f; // Dreht einen Vektor um den Ursprung
Function SameV3(Value1, Value2: TVector3f): boolean; // Vergleicht 2 Vektoren True bei Gleichheit
Function Same(Value1, Value2: TRayFloat): boolean; // Vergleicht 2 Scalare True bei Gleichheit

Function Max(V1, V2: TRayFloat): TRayFloat; overload; // gibt den Größeren der 2 Werte zurück
Function Min(V1, V2: TRayFloat): TRayFloat; overload; // gibt den kleineren der 2 Werte zurück
Function Max(V1, V2: Integer): Integer; overload; // gibt den Größeren der 2 Werte zurück
Function Min(V1, V2: Integer): Integer; overload; // gibt den kleineren der 2 Werte zurück

Function AOverB(Value1, Value2: TVector3f; Alpha: TRayFloat): TVector3f; // Blendet 2 Varbwerte in einander über.
Function AOverBSin(Value1, Value2: TVector3f; Alpha: TRayFloat): TVector3f; // Blendet 2 Varbwerte in einander über, vie Sinus Cosinus
Function InterpoliereCos(Wert1, Wert2, Faktorwert1: extended): extended; // siehe unten
Function InterpoliereLinear(Wert1, Wert2, Faktorwert1: extended): extended; // siehe unten
Function InterpoliereCosV3(Wert1, Wert2: TVector3f; Faktorwert1: extended): TVector3f; // siehe unten
Function InterpoliereLinearV3(Wert1, Wert2: TVector3f; Faktorwert1: extended): TVector3f; // siehe unten

Function Power(Base, Exponent: TRayFloat): TRayFloat; // berechnet Base ^ Exponent
Function GetRValue(rgb: DWORD): Byte; // Extrahiert den Roten Farbkanal aus einer Farbe
Function GetGValue(rgb: DWORD): Byte; // Extrahiert den Grünen Farbkanal aus einer Farbe
Function GetBValue(rgb: DWORD): Byte; // Extrahiert den Blauen Farbkanal aus einer Farbe
Function GetLuminance(Value: TVector3f): TRayFloat;

Implementation

Function InterpoliereCos(Wert1, Wert2, Faktorwert1: extended): extended;
// Cosinus/Sinus artige Interpolation von zwei werten
// FaktorWert1 muss zwischen 0 und 1 liegen
Var
  a: extended;
Begin
  a := (cos(Faktorwert1 * Pi) + 1) / 2;
  result := Wert1 * a + Wert2 * (1 - a);
End;

// Versucht genau Gleich wie InterpoliereCos zu sein, nur eben Linear

Function InterpoliereLinear(Wert1, Wert2, Faktorwert1: extended): extended;
Begin
  Result := Wert1 * (1 - Faktorwert1) + Wert2 * Faktorwert1;
End;

Function InterpoliereCosV3(Wert1, Wert2: TVector3f; Faktorwert1: extended): TVector3f; // siehe unten
Var
  ma, a: extended;
Begin
  a := (cos(Faktorwert1 * Pi) + 1) / 2;
  ma := 1 - a;
  result.x := wert1.x * a + Wert2.x * ma;
  result.y := wert1.y * a + Wert2.y * ma;
  result.z := wert1.z * a + Wert2.z * ma;
End;

Function InterpoliereLinearV3(Wert1, Wert2: TVector3f; Faktorwert1: extended): TVector3f; // siehe unten
Var
  ma: Extended;
Begin
  ma := 1 - Faktorwert1;
  result.x := wert1.x * Faktorwert1 + Wert2.x * ma;
  result.y := wert1.y * Faktorwert1 + Wert2.y * ma;
  result.z := wert1.z * Faktorwert1 + Wert2.z * ma;
End;

Function IntPower(Base: Extended; Exponent: Integer): Extended;
Var
  i, j: Integer;
Begin
  result := base;
  For i := 1 To Exponent - 1 Do
    result := result * base;
End;

Function Power(Base, Exponent: TRayFloat): TRayFloat;
Begin
  If Exponent = 0.0 Then
    Result := 1.0 { n**0 = 1 }
  Else If (Base = 0.0) And (Exponent > 0.0) Then
    Result := 0.0 { 0**n = 0, n > 0 }
  Else If (Frac(Exponent) = 0.0) And (Abs(Exponent) <= MaxInt) Then
    Result := IntPower(Base, Integer(Trunc(Exponent)))
  Else
    Result := Exp(Exponent * Ln(Base))
End;

Function GetRValue(rgb: DWORD): Byte;
Begin
  Result := Byte(rgb);
End;

Function GetGValue(rgb: DWORD): Byte;
Begin
  Result := Byte(rgb Shr 8);
End;

Function GetBValue(rgb: DWORD): Byte;
Begin
  Result := Byte(rgb Shr 16);
End;

Function GetLuminance(Value: TVector3f): TRayFloat;
Begin
  // Y = 0.3R + 0.59G + 0.11B
  result := Value.x * 0.3 + Value.y * 0.59 + Value.z * 0.11;
End;

Function Max(V1, V2: Integer): Integer;
Begin
  If V1 > v2 Then
    result := v1
  Else
    result := v2;
End;

Function Min(V1, V2: Integer): Integer;
Begin
  If V1 < v2 Then
    result := v1
  Else
    result := v2;
End;

Function Max(V1, V2: TRayFloat): TRayFloat;
Begin
  If V1 > v2 Then
    result := v1
  Else
    result := v2;
End;

Function Min(V1, V2: TRayFloat): TRayFloat;
Begin
  If V1 < v2 Then
    result := v1
  Else
    result := v2;
End;

Function V2(x, y: TRayfloat): Tvector2f;
Begin
  result.x := x;
  result.y := y;
End;

Function V3(x, y, z: TRayfloat): Tvector3f;
Begin
  result.x := x;
  result.y := y;
  result.z := z;
End;

Function V4(x, y, z, w: TRayfloat): Tvector4f;
Begin
  result.x := x;
  result.y := y;
  result.z := z;
  result.w := w;
End;

Function ScaleV2(Scalar: TRayFloat; Value: TVector2f): TVector2f;
Begin
  result.x := Scalar * Value.x;
  result.y := Scalar * Value.y;
End;

Function AddV2(Value1, Value2: TVector2f): TVector2f;
Begin
  result.x := value1.x + value2.x;
  result.y := value1.y + value2.y;
End;

Function SubV2(Value1, Value2: TVector2f): TVector2f;
Begin
  result.x := value1.x - value2.x;
  result.y := value1.y - value2.y;
End;

Function LengthV3(Value: TVector3f): TRayFloat;
Begin
  result := sqrt(sqr(value.x) + sqr(value.y) + sqr(value.z));
End;

Function LengthV3Sqr(Value: TVector3f): TRayFloat;
Begin
  result := sqr(value.x) + sqr(value.y) + sqr(value.z);
End;

Function ScaleV3(Scalar: TRayFloat; Value: TVector3f): TVector3f;
Begin
  result.x := Scalar * Value.x;
  result.y := Scalar * Value.y;
  result.z := Scalar * Value.z;
End;

Function NormalizeV3(Value: TVector3f): TVector3f;
Var
  l: TRayFloat;
Begin
  l := lengthV3(Value);
  If l <> 0 Then
    result := scaleV3(1 / l, value)
  Else
    result := v3(0, 0, 0);
End;

Function AddV3(Value1, Value2: TVector3f): TVector3f;
Begin
  result.x := value1.x + value2.x;
  result.y := value1.y + value2.y;
  result.z := value1.z + value2.z;
End;

Function SubV3(Value1, Value2: TVector3f): TVector3f;
Begin
  result.x := value1.x - value2.x;
  result.y := value1.y - value2.y;
  result.z := value1.z - value2.z;
End;

Function DotProdv3(Value1, Value2: TVector3f): TRayFloat;
Begin
  result := value1.x * value2.x + value1.y * value2.y + value1.z * value2.z;
End;

Function CompProdv3(Value1, Value2: TVector3f): TVector3f;
Begin
  result.x := Value1.x * value2.x;
  result.y := Value1.y * value2.y;
  result.z := Value1.z * value2.z;
End;

Function CrossProdV3(Value1, Value2: TVector3f): TVector3f;
Begin
  result.x := value1.y * Value2.z - value1.z * value2.y;
  result.y := value1.z * Value2.x - value1.x * value2.z;
  result.z := value1.x * Value2.y - value1.y * value2.x;
End;

Function Determinante3x3(Value1, Value2, Value3: TVector3f): TRayFloat;
Begin
  result :=
    Value1.x * value2.y * value3.z
    + Value2.x * value3.y * value1.z
    + Value3.x * value1.y * value2.z
    - Value1.z * value2.y * value3.x
    - Value2.z * value3.y * value1.x
    - Value3.z * value1.y * value2.x;
End;

Function RotateV3(Value, Rotvector: TVector3f): TVector3f;
Var
  tmp: TVector3f;
  sin, cos: Extended;
Begin
  // Drehung um die X - Achse
  If Rotvector.x <> 0 Then Begin
    SinCos(Rotvector.x * pi / 180, sin, cos);
    tmp.x := Value.x; // Hier Passiert Ja nichts
    tmp.y := cos * Value.y - sin * Value.z;
    tmp.z := sin * Value.y + cos * Value.z;
    Value := tmp;
  End;
  // Drehung um die Y - Achse
  If Rotvector.y <> 0 Then Begin
    SinCos(Rotvector.y * pi / 180, sin, cos);
    tmp.x := cos * Value.x - sin * Value.z;
    tmp.y := Value.y; // Hier Passiert Ja nichts
    tmp.z := sin * Value.x + Cos * Value.z;
    Value := tmp;
  End;
  // Drehung um die Z - Achse
  If Rotvector.z <> 0 Then Begin
    SinCos(Rotvector.z * pi / 180, sin, cos);
    tmp.x := Cos * Value.x - sin * Value.y;
    tmp.y := sin * Value.x + Cos * Value.y;
    tmp.z := Value.z; // Hier Passiert Ja nichts
    Value := tmp;
  End;
  result := value;
End;

Function Same(Value1, Value2: TRayFloat): boolean; // Vergleicht 2 Scalare True bei Gleichheit
Begin
  result := (abs(Value1 - Value2) <= Minfloat);
End;

Function SameV3(Value1, Value2: TVector3f): boolean;
Var
  x, y, z: TRayFloat;
Begin
  x := abs(Value1.x - value2.x);
  y := abs(Value1.y - value2.y);
  z := abs(Value1.z - value2.z);
  result := (x <= MinFloat) And (y <= MinFloat) And (z <= MinFloat);
End;

Function AOverB(Value1, Value2: TVector3f; Alpha: TRayFloat): TVector3f;
Begin
  result := addv3(scalev3(alpha, value1), scalev3(1 - alpha, value2));
End;

Function AOverBSin(Value1, Value2: TVector3f; Alpha: TRayFloat): TVector3f;
Begin
  Alpha := (cos((1 - Alpha) * Pi) + 1) / 2;
  result := addv3(scalev3(alpha, value1), scalev3(1 - alpha, value2));
End;

Function Detv2(v1, v2: TVector2f): single;
Begin
  result := v1.x * v2.y - v1.y * v2.x;
End;

End.

