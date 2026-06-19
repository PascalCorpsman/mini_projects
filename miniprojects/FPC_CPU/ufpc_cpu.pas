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

  TCmd = (
    cADD,
    cAND,
    cCALL, // Calls a subroutine = Push PC to Stack and Jump
    cCMP,
    cDIV,
    cHLT,
    cJMP, // Jump, without any condition
    cJNZ, // Jump if zero flag is not set
    cJZ, // Jump if zero flag is set
    cLabel, // Target für JMP, JNZ, JZ
    cLOAD,
    cMOV,
    cMUL,
    cNOT,
    cNOP,
    cOR,
    cPOP,
    cPUSH,
    cRET, // Returns from subroutine = pop stack to PC
    cSHL,
    cSHR,
    cSTORE,
    cSUB,
    cXOR
    );

  TPipelineStep = (
    psNone, // Den gibts nicht in der CPU, hilft aber beim Rendern..
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
    LeftOperand, RightOperand: String; // cADD, cAND, cDIV, cCMP, cLOAD, cMUL, cMOV, cNOT, cOR, cSHL, cSHR, cSTORE, cSUB, cXOR
  End;

  TAssemblerCMDs = Array Of TAssemblerCMD;

  TDir = (dUp, dDown, dLeft, dRight);

Var
  fCMDs: TAssemblerCMDs;
  PipeLine: Array[0..3] Of Integer; // Index in fCMD
  PipeLineDepth: Integer;

Function PipelineStepToStr(aPipelineStep: TPipelineStep): String;
Function CMDToStr(aCmd: TCmd; LeftOperand, RightOperand: String): String;

Procedure DrawArrowHead(Const Canvas: TCanvas; aPoint: Tpoint; Dir: TDir; aColor: TColor);
Procedure DrawLine(Const Canvas: TCanvas; a, b: TPoint; aColor: TColor);

Var
  LastError: String;
  LastErrorLine: integer = -1;

Function Compile(Const aCode: TStrings): TAssemblerCMDs;

Procedure Nop();

Function isnumber(aValue: String): Boolean;

Function FindNextValidProgramLine(aLine: integer; IgnoreCalls: Boolean = false): integer;
Function convertCodeLineToCMDIndex(aLine: integer): integer;

Implementation

Procedure Nop();
Begin

End;

Function isnumber(aValue: String): Boolean;
Var
  i: Integer;
Begin
  result := false;
  If avalue = '' Then exit;
  If avalue[1] = '-' Then delete(aValue, 1, 1);
  If avalue = '' Then exit;
  For i := 1 To length(avalue) Do Begin
    If (Not (avalue[i] In ['0'..'9'])) Then exit;
  End;
  result := true;
End;

Function FindNextValidProgramLine(aLine: integer; IgnoreCalls: Boolean
  ): integer;
Var
  i: Integer;
Begin
  If (aLine < 0) Or (aLine > high(fCMDs)) Then Begin
    result := high(fCMDs) + 1;
    exit;
  End;

  result := aLine + 1;
  If result > high(fCMDs) Then exit;

  // This is a kind of simple Branch Prediction :)
  If (fCMDs[aLine].Cmd = cJMP)
    Or ((fCMDs[aLine].Cmd = cCALL) And (IgnoreCalls = false)) Then Begin
    For i := 0 To high(fCMDs) Do Begin
      If fCMDs[i].Line = fCMDs[aLine].JumpTarget Then Begin
        result := i;
        break;
      End;
    End;
  End;
  While (result <= high(fCMDs)) And (fcmds[result].Cmd = cLabel) Do Begin
    result := result + 1;
  End;
End;

Function convertCodeLineToCMDIndex(aLine: integer): integer;
Var
  i: Integer;
Begin
  result := -1;
  For i := 0 To high(fCMDs) Do Begin
    If fCMDs[i].Line = aLine Then Begin
      result := i;
      exit;
    End;
  End;
End;

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
    cCALL: result := 'CALL';
    cCMP: result := 'CMP';
    cDIV: result := 'DIV';
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
    cMUL: result := 'MUL';
    cNOT: result := 'NOT';
    cNOP: result := 'NOP';
    cOR: result := 'OR';
    cPOP: result := 'POP';
    cPUSH: result := 'PUSH';
    cRET: result := 'RET';
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
      End;
    dLeft: Begin
        a := point(aPoint.x + dim, aPoint.y - dim);
        b := point(aPoint.x, aPoint.y);
        c := point(aPoint.x + dim, aPoint.y + dim);
      End;
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

(*
 * Diese Funktion ist mehr Heuristik, denn Compiler, aber für Assembler reichts ;)
 *)

Function LineToCode(aLine: String; Out cmd: TAssemblerCMD): Boolean;
Var
  pre, suf, op1, op2: String;
Begin
  result := false;
  aLine := trim(UpperCase(aLine));
  cmd.Line := -1; // Wird vom Aufrufer definiert
  // cmd.Cmd := ?;
  cmd.PipelineStep := psFetch;
  cmd.JumpTarget := -1;
  cmd.aLabel := '';
  cmd.LeftOperand := '';
  cmd.RightOperand := '';
  // 1. Kommentare raus
  If pos(';', aLine) <> 0 Then Begin
    delete(aline, pos(';', aLine), length(aLine));
  End;
  aline := trim(aLine);
  If aLine = '' Then exit; // Eine Leere Zeile ist nix, also raus
  // Labels
  If pos(':', aLine) <> 0 Then Begin
    pre := trim(copy(aLine, 1, pos(':', aLine) - 1));
    suf := trim(copy(aline, pos(':', aLine) + 1, length(aline)));
    If (pre = '') Then Begin
      LastError := 'invalid label';
      exit;
    End;
    If (suf <> '') Then Begin
      LastError := 'after ":" no chars allowed';
      exit;
    End;
    cmd.aLabel := pre;
    cmd.Cmd := cLabel;
    result := true;
    exit;
  End;
  // 2. Alle "Jumps"
  If (aline[1] = 'J') Or (pos('CALL ', aLine) = 1) Then Begin
    pre := trim(copy(aLine, 1, pos(' ', aLine) - 1));
    suf := trim(copy(aline, pos(' ', aLine) + 1, length(aline)));
    If pos(' ', suf) <> 0 Then Begin
      LastError := 'invalid jump target';
      exit;
    End;
    Case pre Of
      'JMP': cmd.Cmd := cJMP;
      'JNZ': cmd.Cmd := cJNZ;
      'JZ': cmd.Cmd := cJZ;
      'CALL': cmd.cmd := cCALL;
    Else Begin
        LastError := 'unknown jump command';
        exit;
      End;
    End;
    // Labels dürfen nicht mit Zahlen beginnen
    If suf[1] In ['0'..'9'] Then Begin
      LastError := 'invalid jump target';
      exit;
    End;
    cmd.LeftOperand := suf;
    result := true;
    exit;
  End;
  // Befehle ohne Parameter
  If aline = 'NOP' Then Begin
    cmd.Cmd := cNOP;
    result := true;
    exit;
  End;
  If aline = 'HLT' Then Begin
    cmd.Cmd := cHLT;
    result := true;
    exit;
  End;
  If aline = 'RET' Then Begin
    cmd.Cmd := cRET;
    result := true;
    exit;
  End;
  pre := trim(copy(aLine, 1, pos(' ', aLine) - 1));
  OP1 := trim(copy(aline, pos(' ', aLine) + 1, length(aline)));
  // Befehle mit nur einem Parameter
  Case pre Of
    'NOT': Begin
        cmd.Cmd := cNOT;
        cmd.LeftOperand := op1;
        result := true;
        exit;
      End;
    'POP': Begin
        cmd.Cmd := cPOP;
        cmd.LeftOperand := op1;
        result := true;
        exit;
      End;
    'PUSH': Begin
        cmd.Cmd := cPUSH;
        cmd.LeftOperand := op1;
        result := true;
        exit;
      End;
  End;
  // Alle Anderen Befehle sind der Form <CMD>" "<Register1>","<Register2>"
  OP2 := trim(copy(OP1, pos(',', OP1) + 1, length(OP1)));
  OP1 := trim(copy(OP1, 1, pos(',', OP1) - 1));
  Case Pre Of
    'ADD': cmd.Cmd := cADD;
    'AND': cmd.Cmd := cAND;
    'CMP': cmd.Cmd := cCMP;
    'DIV': cmd.Cmd := cDIV;
    'LOAD': cmd.Cmd := cLOAD;
    'MOV': cmd.Cmd := cMOV;
    'MUL': cmd.Cmd := cMUL;
    'OR': cmd.Cmd := cOR;
    'SHL': cmd.Cmd := cSHL;
    'SHR': cmd.Cmd := cSHR;
    'STORE': cmd.Cmd := cSTORE;
    'SUB': cmd.Cmd := cSUB;
    'XOR': cmd.Cmd := cXOR;
  Else Begin
      LastError := 'unknown command';
      exit;
    End;
  End;
  If trim(op2) = '' Then Begin
    LastError := 'missing second operand';
    exit;
  End;
  cmd.LeftOperand := op1;
  cmd.RightOperand := op2;
  result := true;
End;

Function Compile(Const aCode: TStrings): TAssemblerCMDs;
Var
  i, j: Integer;
  cmd: TAssemblerCMD;
  found: Boolean;
Begin
  result := Nil;
  LastError := '';
  LastErrorLine := -1;
  // 1. Pass Zeilenweise Code "portieren"
  For i := 0 To aCode.Count - 1 Do Begin
    If LineToCode(aCode[i], cmd) Then Begin
      setlength(result, high(Result) + 2);
      cmd.Line := i;
      result[high(result)] := cmd;
    End
    Else Begin
      If LastError <> '' Then Begin
        lasterror := 'Line[' + inttostr(i + 1) + '] : ' + LastError;
        LastErrorLine := i;
        setlength(result, 0);
        exit;
      End;
    End;
  End;
  // 2. Pass die Jump's auflösen
  For i := 0 To high(result) Do Begin
    If result[i].Cmd In [cJMP, cJZ, cJNZ, cCALL] Then Begin
      found := false;
      For j := 0 To high(result) Do Begin
        If (result[j].Cmd = cLabel) And (result[j].aLabel = Result[i].LeftOperand) Then Begin
          Result[i].JumpTarget := result[j].Line;
          found := true;
          break;
        End;
      End;
      If Not found Then Begin
        lasterror := 'Line[' + inttostr(result[i].Line + 1) + '] : unable to resolve label';
        LastErrorLine := result[i].Line;
        setlength(result, 0);
        exit;
      End;
    End;
  End;
  If Not assigned(Result) Then Begin
    LastError := 'No code.';
    LastErrorLine := 0;
  End;
End;

End.

