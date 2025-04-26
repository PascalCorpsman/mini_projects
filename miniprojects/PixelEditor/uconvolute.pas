(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of PixelEditor                                           *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit uconvolute;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, FPImage, Graphics, IntfGraphics, upixeleditor_types;

Const
  Precision = 256;

Type



  TConvolutions = Array Of TConvolution;

  (*
   * Liest aus einem String das 1. TConvolution aus
   *)
Function StringToConvolution(ConvolutionString: String): TConvolution;

(*
 * Liest aus einem String alle TConvolutions aus
 * Fileformat und eine Quelle: https://github.com/aseprite/aseprite/blob/main/data/convmatr.def
 *)
Function FileToConvolutions(Filename: String): TConvolutions;

Procedure Convolute(Const bm: TBitmap; Const Convolution: TConvolution; Chan: TChannels);

Implementation

Uses math;

(*
# Format:
#
#   matrix-name w h cx cy { data } div bias target
#
# References:
#
#   w      = 1 ... N
#   h      = 1 ... N
#            width and height of the matrix
#
#   cx     = 0 ... w-1
#   cy     = 0 ... h-1
#            center of the matrix
#
#   data   = N0 N1 ... N(w*h)
#            elements of the matrix
#
#   div    = N != 0 | auto
#            final division factor
#            auto = sum(data)
#
#   bias   = -N ... +N | auto
#            final shift
#
#   target = rgba
#            [r]ed, [g]reen, [b]lue, [a]lpha
#*)

(*
 * Quelle: https://github.com/aseprite/aseprite/blob/main/src/app/commands/filters/convolution_matrix_stock.cpp -> ConvolutionMatrixStock::reloadStock()
 *)

Function StringToConvolution(ConvolutionString: String): TConvolution;
Var
  index, i, v, _div, bias: integer;
  biass, divs, tmp: String;
  a: TStringArray;
Begin
  // Cleanup String to be easy parsable
  For i := 1 To length(ConvolutionString) Do Begin
    If ConvolutionString[i] In [#0..#31] Then Begin
      ConvolutionString[i] := ' ';
    End;
  End;
  index := pos('  ', ConvolutionString);
  While index <> 0 Do Begin
    delete(ConvolutionString, index, 1);
    index := pos('  ', ConvolutionString);
  End;
  ConvolutionString := trim(ConvolutionString);
  // matrix-name w h cx cy { data } div bias target
  // 1. Name
  index := pos(' ', ConvolutionString);
  result.Name := copy(ConvolutionString, 1, index - 1);
  delete(ConvolutionString, 1, index);
  // w h cx cy { data } div bias target
  index := pos(' ', ConvolutionString);
  result.w := strtointdef(copy(ConvolutionString, 1, index - 1), 0);
  delete(ConvolutionString, 1, index);
  // h cx cy { data } div bias target
  index := pos(' ', ConvolutionString);
  result.h := strtointdef(copy(ConvolutionString, 1, index - 1), 0);
  delete(ConvolutionString, 1, index);
  // Zumindest die Obere Schranke der Dimenion scheint eher Willkürlich zu sein ..
  If (result.w <= 0) Or (result.w >= 32) Or
    (result.h <= 0) Or (result.h >= 32) Then Begin
    Raise exception.Create('Invalid dimension');
  End;
  // cx cy { data } div bias target
  index := pos(' ', ConvolutionString);
  result.cx := strtointdef(copy(ConvolutionString, 1, index - 1), 0);
  delete(ConvolutionString, 1, index);
  // cy { data } div bias target
  index := pos('{', ConvolutionString);
  result.cy := strtointdef(trim(copy(ConvolutionString, 1, index - 1)), 0);
  delete(ConvolutionString, 1, index);
  If (result.cx < 0) Or (result.cx >= result.w) Or
    (result.cy < 0) Or (result.cy >= result.h) Then Begin
    Raise exception.Create('Invalid centre coordinates');
  End;
  // data } div bias target
  index := pos('}', ConvolutionString);
  tmp := trim(copy(ConvolutionString, 1, index - 1));
  delete(ConvolutionString, 1, index);
  ConvolutionString := trim(ConvolutionString);
  a := tmp.Split(' ');
  If length(a) <> result.w * result.h Then Begin
    Raise exception.Create('Invalid matrix data');
  End;
  setlength(result.data, result.w, result.h);
  _div := 0;
  For i := 0 To high(a) Do Begin
    v := strtointdef(a[i], 0) * Precision;
    result.data[i Mod result.w, i Div result.w] := v;
    _div := _div + v;
  End;
  // div bias target
  ConvolutionString := lowercase(ConvolutionString);
  index := pos(' ', ConvolutionString);
  divs := copy(ConvolutionString, 1, index - 1);
  delete(ConvolutionString, 1, index);
  If _div > 0 Then Begin
    bias := 0;
  End
  Else Begin
    If _div = 0 Then Begin
      _div := Precision;
      bias := 128;
    End
    Else Begin
      _div := abs(_div);
      bias := 255;
    End;
  End;
  If divs = 'auto' Then Begin
    result._div := _div;
  End
  Else Begin
    result._div := strtointdef(divs, 0) * Precision;
  End;

  // bias target
  index := pos(' ', ConvolutionString);
  biass := copy(ConvolutionString, 1, index - 1);
  If biass = 'auto' Then Begin
    result.bias := bias;
  End
  Else Begin
    result.bias := strtointdef(biass, 0);
  End;
  delete(ConvolutionString, 1, index);
  // target
  // ??
End;

Function clamp(v: integer): integer;
Begin
  result := min(255, max(0, v));
End;

Function FileToConvolutions(Filename: String): TConvolutions;
Var
  sl: TStringList;
  s: String;
  c: TConvolution;
  i: Integer;
Begin
  result := Nil;
  If Not FileExists(Filename) Then exit;
  sl := TStringList.Create;
  sl.LoadFromFile(Filename);
  s := '';
  For i := 0 To sl.Count - 1 Do Begin
    If pos('#', trim(sl[i])) = 1 Then Continue; // Skip comments
    s := s + ' ' + sl[i];
    // TODO: das geht so lange gut, bis nach nden nur Alpha filtern doch noch ein "normaler" kommt, dann knallts ..
    If pos(' rgb', sl[i]) <> 0 Then Begin
      Try
        c := StringToConvolution(s);
        setlength(result, high(result) + 2);
        result[high(result)] := c;
      Except
        // Nichts ??
      End;
      s := '';
    End;
  End;
  sl.free;
End;

Procedure Convolute(Const bm: TBitmap; Const Convolution: TConvolution;
  Chan: TChannels);
Var
  source, dest: TLazIntfImage;

  Function fold(x, y: integer): TFPColor;
  Var
    i, j, px, py: Integer;
    r, g, b: integer;
    c: TFPColor;
  Begin
    result := source.Colors[x, y];
    If Convolution._div <> 0 Then Begin // Bei Div = 0 wird der Filter deactiviert!
      r := 0;
      g := 0;
      b := 0;
      For i := 0 To Convolution.w - 1 Do Begin
        For j := 0 To Convolution.h - 1 Do Begin
          px := x + i - Convolution.cx;
          py := y + j - Convolution.cy;
          If (px >= 0) And (px < source.Width) And
            (py >= 0) And (py < source.Height) Then Begin
            c := source.Colors[px, py];
            r := r + (c.Red Shr 8) * Convolution.data[i, j];
            g := g + (c.Green Shr 8) * Convolution.data[i, j];
            b := b + (c.Blue Shr 8) * Convolution.data[i, j];
          End;
        End;
      End;
      If TChannel.cR In Chan Then Begin
        result.Red := clamp(r Div Convolution._div + Convolution.bias) Shl 8;
      End;
      If TChannel.cg In Chan Then Begin
        result.Green := clamp(g Div Convolution._div + Convolution.bias) Shl 8;
      End;
      If TChannel.cb In Chan Then Begin
        result.Blue := clamp(b Div Convolution._div + Convolution.bias) Shl 8;
      End;
    End;
  End;
Var
  i, j: Integer;
Begin
  source := bm.CreateIntfImage;
  dest := bm.CreateIntfImage;
  For i := 0 To source.Width - 1 Do Begin
    For j := 0 To source.Height - 1 Do Begin
      dest.Colors[i, j] := Fold(i, j);
    End;
  End;
  bm.LoadFromIntfImage(dest);
  source.free;
  dest.free;
End;

End.

