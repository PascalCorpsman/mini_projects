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
  Classes, SysUtils;

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
    JumpTarget: Integer; // cJMP, cJNZ, cJZ
    aLabel: String; // cLabel
    LeftOperand, RightOperand: String; // cADD, cAND, cCMP, cLOAD, cMOV, cNOT, cOR, cSHL, cSHR, cSTORE, cSUB, cXOR
  End;

  TAssemblerCMDs = Array Of TAssemblerCMD;

Function PipelineStepToStr(aPipelineStep: TPipelineStep): String;

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

End.

