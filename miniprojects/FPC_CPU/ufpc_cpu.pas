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
Unit uFPC_CPU;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Graphics;

Type

  TCmd = (
    cADD,
    cAND,
    cCMP,
    cHLT,
    cJMP,
    cJNZ,
    cJZ,
    cLabel, // Target für JMP, JNZ, JZ
    cLOAD,
    cMOV,
    cNOT,
    cNOP,
    cOR,
    cSHL,
    cSHR,
    cSTORE,
    cSUB,
    cXOR
    );

  TPipelineStep = (
    psFetch,
    psDecode,
    psExecute,
    psWriteBack
    );

  TAssemblerCMD = Record
    Line: Integer;
    Cmd: TCmd;
    PipelineStep: TPipelineStep;
    // Weitere Attribute, nicht unbedingt für alle TPipelineStep relevant
    JumpTarget: Integer; // cJMP, cJNZ, cJZ, Angegeben ist die "Line"
    aLabel: String; // cLabel
    LeftOperand, RightOperand: String; // cADD, cAND, cCMP, cLOAD, cMOV, cNOT, cOR, cSHL, cSHR, cSTORE, cSUB, cXOR
  End;

  TAssemblerCMDs = Array Of TAssemblerCMD;

  TDir = (dUp, dDown, dLeft, dRight);

Function PipelineStepToStr(aPipelineStep: TPipelineStep): String;
Function CMDToStr(aCmd: TCmd; LeftOperand, RightOperand: String): String;

Procedure DrawArrowHead(Const Canvas: TCanvas; aPoint: Tpoint; Dir: TDir; aColor: TColor);
Procedure DrawLine(Const Canvas: TCanvas; a, b: TPoint; aColor: TColor);

Implementation

Function PipelineStepToStr(aPipelineStep: TPipelineStep): String;
Begin
  Case aPipelineStep Of
    psFetch: result := 'Fetch';
    psDecode: result := 'Decode';
    psExecute: result := 'Execute';
    psWriteBack: result := 'WriteBack';
  Else
    Raise exception.create('PipelineStepToStr: undefined case');
  End;
End;

Function CMDToStr(aCmd: TCmd; LeftOperand, RightOperand: String): String;
Begin
  result := '';
  Case aCmd Of
    cADD: result := 'ADD';
    cAND: result := 'AND';
    cCMP: result := 'CMP';
    cHLT: result := 'HLT';
    cJMP: result := 'JMP';
    cJNZ: result := 'JNZ';
    cJZ: result := 'JZ';
    cLabel: Begin
        Result := LeftOperand + ':';
        exit;
      End;
    cLOAD: result := 'LOAD';
    cMOV: result := 'MOV';
    cNOT: result := 'NOT';
    cOR: result := 'OR';
    cSHL: result := 'SHL';
    cSHR: result := 'SHR';
    cSTORE: result := 'STORE';
    cSUB: result := 'SUB';
    cXOR: result := 'XOR';
  Else Begin
      Raise exception.create('CMDToStr: undefined case');
    End;
  End;
  result := result + ' ' + LeftOperand;
  If RightOperand <> '' Then Begin
    result := result + ', ' + RightOperand;
  End;
End;

Procedure DrawArrowHead(Const Canvas: TCanvas; aPoint: Tpoint; Dir: TDir;
  aColor: TColor);
Const
  Dim = 8;
Var
  a, b, c: TPoint;
Begin
  Case Dir Of
    dRight: Begin
        a := point(aPoint.x - dim, aPoint.y - dim);
        b := point(aPoint.x, aPoint.y);
        c := point(aPoint.x - dim, aPoint.y + dim);
      End;
    dDown: Begin
        a := point(aPoint.x - dim, aPoint.y - dim);
        b := point(aPoint.x, aPoint.y);
        c := point(aPoint.x + dim, aPoint.y - dim);
      End;
    dUp: Begin
        a := point(aPoint.x - dim, aPoint.y + dim);
        b := point(aPoint.x, aPoint.y);
        c := point(aPoint.x + dim, aPoint.y + dim);
      End
  End;
  DrawLine(canvas, a, b, aColor);
  DrawLine(canvas, b, c, aColor);
End;

Procedure DrawLine(Const Canvas: TCanvas; a, b: TPoint; aColor: TColor);
Begin
  Canvas.Pen.Color := aColor;
  Canvas.Pen.Width := 3;
  Canvas.Line(a, b);
  Canvas.Pen.Width := 1;
End;

End.

