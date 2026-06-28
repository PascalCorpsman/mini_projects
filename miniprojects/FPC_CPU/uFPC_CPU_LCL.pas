(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of FPC_CPU                                               *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit uFPC_CPU_LCL;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Graphics, classes;

Const
  PipeLineFetchBGColor = clred;
  PipeLineFetchFGColor = clWhite;

  PipeLineDecodeBGColor = clGreen;
  PipeLineDecodeFGColor = clWhite;

  PipeLineExecuteBGColor = clBlue;
  PipeLineExecuteFGColor = clWhite;

  PipeLineWritebackBGColor = clolive;
  PipeLineWritebackFGColor = clWhite;

Type
  TDir = (dUp, dDown, dLeft, dRight);

Procedure DrawArrowHead(Const Canvas: TCanvas; aPoint: TPoint; Dir: TDir; aColor: TColor);
Procedure DrawLine(Const Canvas: TCanvas; a, b: TPoint; aColor: TColor);

Implementation

Procedure DrawArrowHead(Const Canvas: TCanvas; aPoint: TPoint; Dir: TDir; aColor: TColor);
Const
  Dim = 8;
Var
  a, b, c: TPoint;
Begin
  Case Dir Of
    dRight: Begin
        a := Point(aPoint.x - Dim, aPoint.y - Dim);
        b := Point(aPoint.x, aPoint.y);
        c := Point(aPoint.x - Dim, aPoint.y + Dim);
      End;
    dDown: Begin
        a := Point(aPoint.x - Dim, aPoint.y - Dim);
        b := Point(aPoint.x, aPoint.y);
        c := Point(aPoint.x + Dim, aPoint.y - Dim);
      End;
    dUp: Begin
        a := Point(aPoint.x - Dim, aPoint.y + Dim);
        b := Point(aPoint.x, aPoint.y);
        c := Point(aPoint.x + Dim, aPoint.y + Dim);
      End;
    dLeft: Begin
        a := Point(aPoint.x + Dim, aPoint.y - Dim);
        b := Point(aPoint.x, aPoint.y);
        c := Point(aPoint.x + Dim, aPoint.y + Dim);
      End;
  End;
  DrawLine(Canvas, a, b, aColor);
  DrawLine(Canvas, b, c, aColor);
End;

Procedure DrawLine(Const Canvas: TCanvas; a, b: TPoint; aColor: TColor);
Begin
  Canvas.Pen.Color := aColor;
  Canvas.Pen.Width := 3;
  Canvas.Line(a, b);
  Canvas.Pen.Width := 1;
End;

End.

