(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Gorilla                                               *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit ugorilla_helper;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Graphics, uvectormath, uopengl_graphikengine;

Type

  { T2DPatch }

  T2DPatch = Class
  private
    fchanged: Boolean;
    fGraphikItem: TGraphikItem;
    fBuffer: Array Of UInt32;
    Procedure InitOpenGL;
    Function TColorToUint32(aColor: TColor; aAlpha: Byte): UInt32;
  public
    Constructor Create(aWidth, aHeight: Integer; Name: String); virtual;
    Destructor Destroy; override;

    (*
     * Der Versuch ein uopengl_graphikengine ähnliches Interface zu bieten ;)
     * Die Unterscheidung Alpha / nicht Alpha braucht es aber nicht, da wir immer Alpha haben !
     *)
    Procedure Render(Left, Top: Single; Depth: Single = 0);
    Procedure Render(Center: TVector2; Depth, Angle: Single);

    Procedure Fill(aColor: TColor; aAlpha: Byte = 0);
    Procedure SetPixelByColor(x, y: integer; aColor: TColor; aAlpha: Byte = 0);
  End;

Implementation

Uses dglOpenGL;

Function IsPowerOfTwo(Value: Integer): Boolean;
Var
  i: Integer;
Begin
  i := 1;
  While i < Value Do
    i := i Shl 1;
  result := i = Value;
End;

Function GetNextPowerOfTwo(Value: integer): Integer;
Var
  i: Integer;
Begin
  i := 1;
  While i < Value Do
    i := i Shl 1;
  result := i;
End;

{ T2DPatch }

Constructor T2DPatch.Create(aWidth, aHeight: Integer; Name: String);
Begin
  Inherited Create;
  fGraphikItem.Image := 0;
  fGraphikItem.IsAlphaImage := true;
  fGraphikItem.Name := Name;
  fGraphikItem.Stretched := smClamp; // Das gibt die besten Visuellen Ergebnisse!
  fGraphikItem.OrigWidth := aWidth;
  fGraphikItem.OrigHeight := aHeight;
  If IsPowerOfTwo(fGraphikItem.OrigWidth) Then Begin
    fGraphikItem.StretchedWidth := aWidth;
  End
  Else Begin
    fGraphikItem.StretchedWidth := GetNextPowerOfTwo(aWidth);
  End;
  If IsPowerOfTwo(fGraphikItem.OrigHeight) Then Begin
    fGraphikItem.StretchedHeight := aHeight;
  End
  Else Begin
    fGraphikItem.StretchedHeight := GetNextPowerOfTwo(aHeight);
  End;
  fBuffer := Nil;
  InitOpenGL;
End;

Destructor T2DPatch.Destroy;
Begin
  setlength(fBuffer, 0);
  If fGraphikItem.Image <> 0 Then Begin
    glDeleteTextures(1, @fGraphikItem.Image);
  End;
  fGraphikItem.Image := 0;
End;

Procedure T2DPatch.Render(Left, Top: Single; Depth: Single);
Begin
  If fchanged Then Begin
    glBindTexture(GL_TEXTURE_2D, fGraphikItem.Image);
    glTexImage2D(GL_TEXTURE_2D, 0, gl_RGBA, fGraphikItem.StretchedWidth, fGraphikItem.StretchedHeight, 0, GL_RGBA, GL_UNSIGNED_BYTE, @fBuffer[0]);
    fchanged := false;
  End;
  RenderAlphaQuad(Left, Top, Depth, fGraphikItem);
End;

Procedure T2DPatch.Render(Center: TVector2; Depth, Angle: Single);
Begin
  If fchanged Then Begin
    glBindTexture(GL_TEXTURE_2D, fGraphikItem.Image);
    glTexImage2D(GL_TEXTURE_2D, 0, gl_RGBA, fGraphikItem.StretchedWidth, fGraphikItem.StretchedHeight, 0, GL_RGBA, GL_UNSIGNED_BYTE, @fBuffer[0]);
    fchanged := false;
  End;
  RenderAlphaQuad(Center, Depth, Angle, fGraphikItem);
End;

Procedure T2DPatch.Fill(aColor: TColor; aAlpha: Byte);
Var
  avalue: UInt32;
  i: Integer;
Begin
  fchanged := true;
  avalue := TColorToUint32(aColor, aAlpha);
  For i := 0 To high(fBuffer) Do Begin
    fBuffer[i] := avalue;
  End;
End;

Procedure T2DPatch.SetPixelByColor(x, y: integer; aColor: TColor; aAlpha: Byte);
Var
  avalue: UInt32;
  index: Integer;
Begin
  index := x + y * fGraphikItem.StretchedWidth;
  If (index >= 0) And (index <= high(fBuffer)) Then Begin
    avalue := TColorToUint32(aColor, aAlpha);
    If fBuffer[index] <> avalue Then Begin
      fBuffer[index] := avalue;
      fchanged := true;
    End;
  End;
End;

Procedure T2DPatch.InitOpenGL;
Begin
  setlength(fBuffer, fGraphikItem.StretchedWidth * fGraphikItem.StretchedHeight);
  glGenTextures(1, @fGraphikItem.Image);
  glBindTexture(GL_TEXTURE_2D, fGraphikItem.Image);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  Fill(0, 255); // Leert den Puffer und sorgt beim 1. Render für eine Initialisierung des Puffers
End;

Function T2DPatch.TColorToUint32(aColor: TColor; aAlpha: Byte): UInt32;
Var
  r, g, b: Byte;
Begin
  r := (aColor Shr 0) And $FF;
  g := (aColor Shr 8) And $FF;
  b := (aColor Shr 16) And $FF;
  result :=
    (aAlpha Shl 24) Or
    (b Shl 16) Or
    (g Shl 8) Or
    (r Shl 0);
End;

End.

