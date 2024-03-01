(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Imageshop                                             *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
(*
 * List of all Operators that will could be added to the sidemenu
 *
 * To insert a new one derive from one of the four classes:
 *
 * Operator classes
 *   TOneInputNode
 *   TOneInputSliderNode <-- prefered are these (if slider is full right they are the same as the version without slider)
 *
 * Blend classes
 *   TTwoInputNode
 *   TTwoInpudSliderNode <-- prefered are these (if slider is full right they are the same as the version without slider)
 *
 * depending on your needs.
 * Override the Corresponding
 *   DoThePixelOperation
 * or
 *   DoTheSliderPixelOperation
 *
 * and add your class to InitializeOperations or InitializeBlends.
 *
 * Refer Examples below
 *)
Unit uoperations;

{$MODE objfpc}{$H+}

(*
 * Betrifft den Operator Posterize:
 *
 * Wenn Aktiv, dann kann man nicht zwischen dem Original und dem
 * PosterStep 5 faden, sondern die Postersteps zwischen 2 und 10
 *
 *)
{.$DEFINE POSTER_SLIDERSTEPS}

Interface

Uses
  extctrls, unodes;

Type

  TNodeClass = Class Of TNode;

  TAddOperator = Procedure(Name: String; Op: TNodeClass) Of Object;

  (*
   * Das Sind 2 damit wir die In der Listbox nach Operator und Blend separiert haben
   * Ist ein Rein Optisches Gimick
   *)
Procedure InitializeOperations(Add: TAddOperator);
Procedure InitializeBlends(Add: TAddOperator);

Implementation

Uses uoperator_helper, Types, math;

(*
 * The most of the following operands are inspired by:
 *  https://www.getlazarus.org/learn/tutorials/examples/imageshop/
 *
 * Wie Wärs mit ein paar mehr: https://en.wikipedia.org/wiki/Blend_modes
 *
 *)

Type

  (*********** Pixel Operations ***********)

    { TInvert }

  TInvert = Class(TOneInputSliderNode)
  protected
    Function DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y, SliderValue: Single): TPixel; override;
    Function GetHint(): String; override;
  End;

  { TSaturation }

  TSaturation = Class(TOneInputSliderNode)
  protected
    Function DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y, SliderValue: Single): TPixel; override;
    Function GetHint(): String; override;
  End;

  { THue }

  THue = Class(TOneInputSliderNode)
  protected
    Function DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y, SliderValue: Single): TPixel; override;
    Function GetHint(): String; override;
  End;

  { TBlackOrWhite }

  TBlackOrWhite = Class(TOneInputSliderNode)
  protected
    Function DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y, SliderValue: Single): TPixel; override;
    Function GetHint(): String; override;
  End;

  { TBrighten }

  TBrighten = Class(TOneInputSliderNode)
  protected
    Function DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y, SliderValue: Single): TPixel; override;
    Function GetHint(): String; override;
  End;

  { TContrast }

  TContrast = Class(TOneInputSliderNode)
  protected
    Function DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y, SliderValue: Single): TPixel; override;
    Function GetHint(): String; override;
  End;

  { TDarken }

  TDarken = Class(TOneInputSliderNode)
  protected
    Function DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y, SliderValue: Single): TPixel; override;
    Function GetHint(): String; override;
  End;

  { TRed }

  TRed = Class(TOneInputSliderNode)
  protected
    Function DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y, SliderValue: Single): TPixel; override;
    Function GetHint(): String; override;
  End;

  { TGreen }

  TGreen = Class(TOneInputSliderNode)
  protected
    Function DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y, SliderValue: Single): TPixel; override;
    Function GetHint(): String; override;
  End;

  { TBlue }

  TBlue = Class(TOneInputSliderNode)
  protected
    Function DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y, SliderValue: Single): TPixel; override;
    Function GetHint(): String; override;
  End;

  { TAlpha }

  TAlpha = Class(TOneInputSliderNode)
  protected
    Function DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y, SliderValue: Single): TPixel; override;
    Function GetHint(): String; override;
  End;

  { TRollHorizontal }

  TRollHorizontal = Class(TOneInputSliderNode)
  protected
    Function DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y, SliderValue: Single): TPixel; override;
    Function GetHint(): String; override;
  public
    Function GetPixelValue(x, y: Single): TPixel; override;
  End;

  { TRollVertical }

  TRollVertical = Class(TOneInputSliderNode)
  protected
    Function DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y, SliderValue: Single): TPixel; override;
    Function GetHint(): String; override;
  public
    Function GetPixelValue(x, y: Single): TPixel; override;
  End;

  { TRotate }

  TRotate = Class(TOneInputSliderNode)
  protected
    Function DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y, SliderValue: Single): TPixel; override;
    Function GetHint(): String; override;
  public
    Function GetPixelValue(x, y: Single): TPixel; override;
  End;

  { TPosterize }

  TPosterize = Class(TOneInputSliderNode)
  protected
    Function DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y, SliderValue: Single): TPixel; override;
    Function GetHint(): String; override;
  End;

  { TSepia }

  TSepia = Class(TOneInputSliderNode)
  protected
    Function DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y, SliderValue: Single): TPixel; override;
    Function GetHint(): String; override;
  End;

  (*********** Blend Operations ***********)

  { TOpacity }

  TOpacity = Class(TTwoInputSliderNode)
  protected
    Function DoTheSliderPixelOperation(Const Pixel1, Pixel2: TPixel; x, y, SliderValue: Single): TPixel; override;
    Function GetHint(): String; override;
  End;

  { TDisolve }

  TDisolve = Class(TTwoInputSliderNode)
  protected
    Function DoTheSliderPixelOperation(Const Pixel1, Pixel2: TPixel; x, y, SliderValue: Single): TPixel; override;
    Function GetHint(): String; override;
  End;

  { TBlock }

  TBlock = Class(TTwoInputSliderNode)
  protected
    Function DoTheSliderPixelOperation(Const Pixel1, Pixel2: TPixel; x, y, SliderValue: Single): TPixel; override;
    Function GetHint(): String; override;
  End;

  { TMultiply }

  TMultiply = Class(TTwoInputSliderNode)
  protected
    Function DoTheSliderPixelOperation(Const Pixel1, Pixel2: TPixel; x, y, SliderValue: Single): TPixel; override;
    Function GetHint(): String; override;
  End;

  { TScreen }

  TScreen = Class(TTwoInputSliderNode)
  protected
    Function DoTheSliderPixelOperation(Const Pixel1, Pixel2: TPixel; x, y, SliderValue: Single): TPixel; override;
    Function GetHint(): String; override;
  End;

  { TAddition }

  TAddition = Class(TTwoInputSliderNode)
  protected
    Function DoTheSliderPixelOperation(Const Pixel1, Pixel2: TPixel; x, y, SliderValue: Single): TPixel; override;
    Function GetHint(): String; override;
  End;

  { TSubtraction }
  TSubtraction = Class(TTwoInputSliderNode)
  protected
    Function DoTheSliderPixelOperation(Const Pixel1, Pixel2: TPixel; x, y, SliderValue: Single): TPixel; override;
    Function GetHint(): String; override;
  End;

  { TWipe }

  TWipe = Class(TTwoInputSliderNode)
  protected
    Function DoTheSliderPixelOperation(Const Pixel1, Pixel2: TPixel; x, y, SliderValue: Single): TPixel; override;
    Function GetHint(): String; override;
  End;

  { TCircle }

  TCircle = Class(TTwoInputSliderNode)
  protected
    Function DoTheSliderPixelOperation(Const Pixel1, Pixel2: TPixel; x, y, SliderValue: Single): TPixel; override;
    Function GetHint(): String; override;
  End;

  { TMask }

  TMask = Class(TTwoInputSliderNode)
  protected
    Function DoTheSliderPixelOperation(Const Pixel1, Pixel2: TPixel; x, y, SliderValue: Single): TPixel; override;
    Function GetHint(): String; override;
  End;

Procedure InitializeOperations(Add: TAddOperator);
Begin
  Add('Red Channel', tRed);
  Add('Green Channel', tGreen);
  Add('Blue Channel', tBlue);
  Add('Saturation', tSaturation);
  Add('Alpha Channel', tAlpha);
  Add('Black or White', tBlackOrWhite);
  Add('Brighten', tBrighten);
  Add('Contrast', tContrast);
  Add('Darken', tDarken);
  add('Invert', TInvert);
  Add('Hue', THue);
  Add('Posterize', TPosterize);
  Add('Sepia', TSepia);
  Add('Roll Vertical', TRollVertical);
  Add('Roll Horizontal', TRollHorizontal);
  Add('Rotate', TRotate);
End;

Procedure InitializeBlends(Add: TAddOperator);
Begin
  Add('Opacity', TOpacity);
  Add('Disolve', TDisolve);
  Add('Multiply', TMultiply);
  Add('Screen', TScreen);
  Add('Addition', TAddition);
  Add('Subtraction', TSubtraction);
  Add('Mask', TMask);
  add('Wipe', TWipe);
  Add('Circle', TCircle);
  Add('Blocks', TBlock);
End;

{ TMask }

Function TMask.DoTheSliderPixelOperation(Const Pixel1, Pixel2: TPixel; x, y,
  SliderValue: Single): TPixel;
Var
  p: TPixel;
Begin
  (*
   * A wird entsprechend seines A in B übergeblendet
   *)
  p.r := Pixel1.r * (1 - Pixel1.a) + Pixel2.r * Pixel1.a;
  p.g := Pixel1.g * (1 - Pixel1.a) + Pixel2.g * Pixel1.a;
  p.b := Pixel1.b * (1 - Pixel1.a) + Pixel2.b * Pixel1.a;
  p.a := Pixel2.a;
  result := Mix(Pixel2, P, SliderValue);
End;

Function TMask.GetHint(): String;
Begin
  Result := Inherited GetHint();
End;

{ TScreen }

Function TScreen.DoTheSliderPixelOperation(Const Pixel1, Pixel2: TPixel; x, y,
  SliderValue: Single): TPixel;
Var
  P: TPixel;
Begin
  //f(a,b) =   1 - (1 -    a    )   (1 -    b    )
  P.R := clamp(1 - (1 - Pixel1.R) * (1 - Pixel2.R));
  P.G := clamp(1 - (1 - Pixel1.G) * (1 - Pixel2.G));
  P.B := clamp(1 - (1 - Pixel1.B) * (1 - Pixel2.B));
  P.A := clamp(1 - (1 - Pixel1.A) * (1 - Pixel2.A));
  result := Mix(Pixel2, P, SliderValue);
End;

Function TScreen.GetHint(): String;
Begin
  Result := 'Screen blend: ' + Inherited GetHint();
End;

{ TSepia }

Function TSepia.DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y,
  SliderValue: Single): TPixel;
Var
  r, g, b: Single;
  p: TPixel;
Begin
  r := 0.393 * Pixel1.R + 0.769 * Pixel1.G + 0.189 * Pixel1.B;
  g := 0.349 * Pixel1.R + 0.686 * Pixel1.G + 0.168 * Pixel1.B;
  b := 0.272 * Pixel1.R + 0.534 * Pixel1.G + 0.131 * Pixel1.B;
  p.r := Clamp(r);
  p.g := Clamp(g);
  p.b := Clamp(b);
  p.a := Pixel1.a;
  Result := mix(pixel1, p, SliderValue);
End;

Function TSepia.GetHint(): String;
Begin
  Result := 'Converts into sepia colors: ' + Inherited GetHint();
End;

{ TPosterize }

Function TPosterize.DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y,
  SliderValue: Single): TPixel;
{$IFNDEF POSTER_SLIDERSTEPS}
Const
  (*
   * Erlaubter Wertebereich [1 .. 128[
   * Sinnvoll irgendwas im Bereich [2 .. 10], alles drüber ist quatsch.
   *)
  Postersteps = 5;
{$ENDIF}
Var
{$IFDEF POSTER_SLIDERSTEPS}
  Postersteps,
{$ENDIF}
  delta, r, g, b: integer;
  p: TPixel;
Begin
  r := round(pixel1.r * 255);
  g := round(pixel1.g * 255);
  b := round(pixel1.b * 255);
{$IFDEF POSTER_SLIDERSTEPS}
  Postersteps := 2 + round(SliderValue * 8);
{$ENDIF}
  delta := 256 Div Postersteps;
  r := r - r Mod delta;
  g := g - g Mod delta;
  b := b - b Mod delta;
  p.r := r / 255;
  p.g := g / 255;
  p.b := b / 255;
  p.a := Pixel1.a;
{$IFDEF POSTER_SLIDERSTEPS}
  Result := p;
{$ELSE}
  Result := mix(pixel1, p, SliderValue);
{$ENDIF}
End;

Function TPosterize.GetHint(): String;
Begin
  Result := 'Posterizes a image by value: ' + Inherited GetHint();
End;

{ TRotate }

Function TRotate.DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y,
  SliderValue: Single): TPixel;
Begin
  Result := Pixel1;
End;

Function TRotate.GetHint(): String;
Begin
  Result := 'Rotate a image: ' + Inherited GetHint();
End;

Function TRotate.GetPixelValue(x, y: Single): TPixel;
Var
  xx, yy, s, c: Extended;
  phi: SIngle;
Begin
  phi := GetSliderPosition * 2 * pi;
  sincos(phi, s, c);
  x := x - 0.5;
  y := y - 0.5;
  xx := c * x - s * y;
  yy := s * x + c * y;
  x := clamp(xx + 0.5);
  y := clamp(yy + 0.5);
  Result := Inherited GetPixelValue(x, y);
End;

{ TRollV }

Function TRollVertical.DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y,
  SliderValue: Single): TPixel;
Begin
  Result := Pixel1;
End;

Function TRollVertical.GetHint(): String;
Begin
  Result := 'Rolls the image vertically: ' + Inherited GetHint();
End;

Function TRollVertical.GetPixelValue(x, y: Single): TPixel;
Begin
  y := y + GetSliderPosition;
  If y < 0 Then y := y + 1;
  If y > 1 Then y := y - 1;
  Result := Inherited GetPixelValue(x, y);
End;

{ TRollH }

Function TRollHorizontal.DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y,
  SliderValue: Single): TPixel;
Begin
  Result := Pixel1;
End;

Function TRollHorizontal.GetHint(): String;
Begin
  Result := 'Rolls the image horizontally: ' + Inherited GetHint();
End;

Function TRollHorizontal.GetPixelValue(x, y: Single): TPixel;
Begin
  x := x + GetSliderPosition;
  If x < 0 Then x := x + 1;
  If x > 1 Then x := x - 1;
  Result := Inherited GetPixelValue(x, y);
End;

{ TCircle }

Function TCircle.DoTheSliderPixelOperation(Const Pixel1, Pixel2: TPixel; x, y,
  SliderValue: Single): TPixel;
Var
  D, W, H: Single;
Begin
  D := 1.42 * SliderValue / 2;
  W := (X - 0.5) / 2;
  H := (Y - 0.5) / 2;
  If Sqrt(W * W + H * H) < D Then
    result := Pixel1
  Else
    result := Pixel2;
End;

Function TCircle.GetHint(): String;
Begin
  Result := 'Blends by using a increasing circle 1 = full blend: ' + Inherited GetHint();
End;

{ TSubstraction }

Function TSubtraction.DoTheSliderPixelOperation(Const Pixel1, Pixel2: TPixel;
  x, y, SliderValue: Single): TPixel;
Var
  P: TPixel;
Begin
  P.B := clamp(Pixel2.B - Pixel1.B);
  P.G := clamp(Pixel2.G - Pixel1.G);
  P.R := clamp(Pixel2.R - Pixel1.R);
  P.A := clamp(Pixel2.A - Pixel1.A);
  result := Mix(Pixel2, P, SliderValue);
End;

Function TSubtraction.GetHint(): String;
Begin
  Result := 'Subtracts both images 1 = full subtract: ' + Inherited GetHint();
End;

{ TAddition }

Function TAddition.DoTheSliderPixelOperation(Const Pixel1, Pixel2: TPixel; x,
  y, SliderValue: Single): TPixel;
Var
  P: TPixel;
Begin
  P.B := clamp(Pixel1.B + Pixel2.B);
  P.G := clamp(Pixel1.G + Pixel2.G);
  P.R := clamp(Pixel1.R + Pixel2.R);
  P.A := clamp(Pixel1.A + Pixel2.A);
  result := Mix(Pixel2, P, SliderValue);
End;

Function TAddition.GetHint(): String;
Begin
  Result := 'Adds both images 1 = full add: ' + Inherited GetHint();
End;

{ TMultiply }

Function TMultiply.DoTheSliderPixelOperation(Const Pixel1, Pixel2: TPixel; x,
  y, SliderValue: Single): TPixel;
Var
  P: TPixel;
Begin
  P.B := clamp(Pixel1.B * Pixel2.B);
  P.G := clamp(Pixel1.G * Pixel2.G);
  P.R := clamp(Pixel1.R * Pixel2.R);
  P.A := clamp(Pixel1.A * Pixel2.A);
  result := Mix(Pixel2, P, SliderValue);
End;

Function TMultiply.GetHint(): String;
Begin
  Result := 'Multiply image 1 = full multiply: ' + Inherited GetHint();
End;

{ TBlock }

Function TBlock.DoTheSliderPixelOperation(Const Pixel1, Pixel2: TPixel; x, y,
  SliderValue: Single): TPixel;
Const
  BlockSize = 50;
Var
  Fade: Single;
  xx, yy: integer;
  p: TPoint;
Begin
  If SliderValue < 0.001 Then
    Result := Pixel2
  Else If SliderValue > 0.999 Then
    Result := Pixel1
  Else Begin
    p := GetImageDimension();
    xx := round(x * p.x);
    yy := round(y * p.y);
    Inc(xX, BlockSize);
    Inc(yY, BlockSize);
    RandSeed := ((Xx Div BlockSize) + (Yy Div BlockSize) * (Xx Div BlockSize) * 73 + p.x * 31 + p.y * 57 * p.x * 31);
    Fade := SliderValue + 0.2 - Random();
    If SliderValue < 0.5 Then
      Fade := Fade * (SliderValue / 0.5);
    If Fade < 0.001 Then
      Result := Pixel2
    Else If Fade < 0.2 Then
      Result := Mix(Pixel2, Pixel1, Fade / 0.2)
    Else
      Result := Pixel1;
  End;
End;

Function TBlock.GetHint(): String;
Begin
  Result := 'Blend by using a random block pattern 1 = full blend: ' + Inherited GetHint();
End;

{ TDisolve }

Function TDisolve.DoTheSliderPixelOperation(Const Pixel1, Pixel2: TPixel; x, y,
  SliderValue: Single): TPixel;
Begin
  If (x = 0) And (y = 0) Then Begin
    RandSeed := GetImageDimension().x * GetImageDimension().y;
  End;
  If SliderValue < 0.001 Then
    result := Pixel2
  Else If SliderValue > 0.999 Then
    result := Pixel1
  Else Begin
    If Random() < SliderValue Then
      result := Pixel1
    Else
      result := Pixel2;
  End;
End;

Function TDisolve.GetHint(): String;
Begin
  Result := 'Disolves both images, 1 = full disolve: ' + Inherited GetHint();
End;

{ TOpacity }

Function TOpacity.DoTheSliderPixelOperation(Const Pixel1, Pixel2: TPixel; x, y,
  SliderValue: Single): TPixel;
Begin
  result := Mix(Pixel2, Pixel1, SliderValue);
End;

Function TOpacity.GetHint(): String;
Begin
  Result := 'Blends both images, 1 = full blend: ' + Inherited GetHint();
End;

{ TAlpha }

Function TAlpha.DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y,
  SliderValue: Single): TPixel;
Begin
  result.r := Pixel1.r;
  result.g := Pixel1.g;
  result.b := Pixel1.b;
  result.a := Clamp(Pixel1.a * SliderValue);
End;

Function TAlpha.GetHint(): String;
Begin
  Result := 'Affects only alpha: ' + Inherited GetHint();
End;

{ TBlue }

Function TBlue.DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y,
  SliderValue: Single): TPixel;
Begin
  result.r := Pixel1.r;
  result.g := Pixel1.g;
  result.b := Clamp(Pixel1.b + (SliderValue - 0.5) * 2);
  result.a := Pixel1.a;
End;

Function TBlue.GetHint(): String;
Begin
  Result := 'Affects only blue: ' + Inherited GetHint();
End;

{ TGreen }

Function TGreen.DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y,
  SliderValue: Single): TPixel;
Begin
  result.r := Pixel1.r;
  result.g := Clamp(Pixel1.g + (SliderValue - 0.5) * 2);
  result.b := Pixel1.b;
  result.a := Pixel1.a;
End;

Function TGreen.GetHint(): String;
Begin
  Result := 'Affects only green: ' + Inherited GetHint();
End;

{ TRed }

Function TRed.DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y,
  SliderValue: Single): TPixel;
Begin
  result.r := Clamp(Pixel1.R + (SliderValue - 0.5) * 2);
  result.g := Pixel1.g;
  result.b := Pixel1.b;
  result.a := Pixel1.a;
End;

Function TRed.GetHint(): String;
Begin
  Result := 'Affects only red: ' + Inherited GetHint();
End;

{ TDarken }

Function TDarken.DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y,
  SliderValue: Single): TPixel;
Begin
  result.B := Clamp(Pixel1.B - SliderValue);
  result.G := Clamp(Pixel1.G - SliderValue);
  result.R := Clamp(Pixel1.R - SliderValue);
  result.a := pixel1.a;
End;

Function TDarken.GetHint(): String;
Begin
  Result := 'Darkens the image 1 = black: ' + Inherited GetHint();
End;

{ TContrast }

Function TContrast.DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y,
  SliderValue: Single): TPixel;
Var
  B, G, R: Single;
Begin
  B := (Pixel1.B - 0.5) * 4 * SliderValue;
  G := (Pixel1.G - 0.5) * 4 * SliderValue;
  R := (Pixel1.R - 0.5) * 4 * SliderValue;
  Result.B := Clamp(Pixel1.B + B);
  Result.G := Clamp(Pixel1.G + G);
  Result.R := Clamp(Pixel1.R + R);
  Result.a := pixel1.a;
End;

Function TContrast.GetHint(): String;
Begin
  Result := 'Contrast 0.5 = off: ' + Inherited GetHint();
End;

{ TBrighten }

Function TBrighten.DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y,
  SliderValue: Single): TPixel;
Begin
  result.B := Clamp(Pixel1.B + SliderValue);
  result.G := Clamp(Pixel1.G + SliderValue);
  result.R := Clamp(Pixel1.R + SliderValue);
  result.a := Pixel1.a;
End;

Function TBrighten.GetHint(): String;
Begin
  Result := 'Brighten the image 1 = white: ' + Inherited GetHint();
End;

{ TBlackOrWhite }

Function TBlackOrWhite.DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y,
  SliderValue: Single): TPixel;
Var
  A: Single;
Begin
  A := Pixel1.A;
  If Pixel1.R + Pixel1.G + Pixel1.B > SliderValue * 3 Then
    Result := White
  Else
    Result := Black;
  Result.A := A;
End;

Function TBlackOrWhite.GetHint(): String;
Begin
  Result := 'White if pixel is > value: ' + Inherited GetHint();
End;

{ THue }

Function THue.DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y,
  SliderValue: Single): TPixel;
Var
  P: TPixel;
  A: Single;
Begin
  P := Hue(SliderValue);
  A := Pixel1.A;
  result := Mix(P, White, Pixel1.B * 0.863 + Pixel1.G * 0.275 + Pixel1.R * 0.510);
  //  result := Mix(P, White, Pixel1.B * 0.0722 + Pixel1.G * 0.7152 + Pixel1.R * 0.2126); // Wikipedia Formula
  result.A := A;
End;

Function THue.GetHint(): String;
Begin
  Result := 'Hue color by source saturation: ' + Inherited GetHint();
End;

{ TSaturation }

Function TSaturation.DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y,
  SliderValue: Single): TPixel;
Var
  P: TPixel;
  D: Single;
Begin
  D := clamp(Pixel1.B * 0.863 + Pixel1.G * 0.275 + Pixel1.R * 0.510); // Orig autors Formula
  //  D := clamp(Pixel1.B * 0.0722 + Pixel1.G * 0.7152 + Pixel1.R * 0.2126); // Wikipedia Formula
  P.B := D;
  P.G := D;
  P.R := D;
  P.A := Pixel1.A;
  result := Mix(P, Pixel1, SliderValue);
End;

Function TSaturation.GetHint(): String;
Begin
  result := 'Converts the image into grayscale 0 = gray: ' + Inherited GetHint();
End;

{ TInvert }

Function TInvert.DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y,
  SliderValue: Single): TPixel;
Var
  P: TPixel;
Begin
  P.B := 1 - Pixel1.B;
  P.G := 1 - Pixel1.G;
  P.R := 1 - Pixel1.R;
  P.A := Pixel1.A;
  result := Mix(Pixel1, P, SliderValue);
End;

Function TInvert.GetHint(): String;
Begin
  Result := 'Inverts the colors of the image 1 = inverted:' + Inherited GetHint();
End;

{ TWipe }

Function TWipe.DoTheSliderPixelOperation(Const Pixel1, Pixel2: TPixel; x, y,
  SliderValue: Single): TPixel;
Begin
  If SliderValue < 0.001 Then
    Result := Pixel2
  Else If SliderValue > 0.999 Then
    Result := Pixel1
  Else Begin
    If x < SliderValue Then Begin
      Result := Pixel1;
    End
    Else Begin
      Result := Pixel2;
    End;
  End;
End;

Function TWipe.GetHint(): String;
Begin
  Result := 'Wipe by slider value: ' + Inherited GetHint();
End;

End.

