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
  Classes, SysUtils, Math;

Type

  TCmd = (
    cADD,
    cAND,
    cCALL, // Calls a subroutine = Push PC to Stack and Jump
    cCMP,
    cDIV,
    cHLT,
    cJC, // Jump if carry flag is set
    cJMP, // Jump, without any condition
    cJN, // Jump if negative flag is set
    cJNC, // Jump if carry flag is not set
    cJNN, // Jump if negative flag is not set
    cJNZ, // Jump if zero flag is not set
    cJZ, // Jump if zero flag is set
    cLabel, // Target für JMP, JNZ, JZ
    cLOAD,
    cMOV,
    cMUL,
    cNOP,
    cNOT,
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

Var
  fCMDs: TAssemblerCMDs;
  PipeLine: Array[0..3] Of Integer; // Index in fCMD
  PipeLineDepth: Integer;

Function PipelineStepToStr(aPipelineStep: TPipelineStep): String;
Function CMDToStr(aCmd: TCmd; LeftOperand, RightOperand: String): String;

Var
  LastError: String;
  LastErrorLine: integer = -1;

Function Compile(Const aCode: TStrings): TAssemblerCMDs;

Procedure Nop();

Function isnumber(aValue: String): Boolean;

Function FindNextValidProgramLine(aLine: integer; IgnoreCalls: Boolean = false): integer;
Function convertCodeLineToCMDIndex(aLine: integer): integer;

// ---------------------------------------------------------------------------
// TCPUEngine – LCL-free execution engine for headless / unit-test use
// ---------------------------------------------------------------------------
// The engine models CPU state (registers, flags, stack) as plain Pascal data.
// It replicates the pipeline logic from TForm1.Button2Click / WriteBack,
// including the RAW-hazard stall for CMP and PUSH after ALU write-back.
//
// Usage:
//   eng := TCPUEngine.Create;
//   eng.LoadProgram(Compile(lines), pipelineMode);
//   while not eng.Step() do ;  // run until HLT
//   ShowMessage(inttostr(eng.RegA));
//   eng.Free;
// ---------------------------------------------------------------------------

Type
  TCPUEngine = Class
  private
    fCmds: TAssemblerCMDs;
    fBranchTargets: Array Of Integer; // resolved jump target (cmd index)

    fRegA, fRegB, fRegC, fRegD: Integer;
    fFlagZero, fFlagCarry, fFlagNegative: Boolean;
    fStack: Array Of Integer; // stores 1-based CALL line numbers
    fMemory: Array[100..119] Of Integer;

    fPipelineMode: Boolean;
    fPipeline: Array[0..3] Of Integer;
    fPipelineDepth: Integer;
    fPendingTarget: Integer;
    fHalted: Boolean;
    fEndedWithoutHalt: Boolean;

    Function GetReg(Const aName: String): Integer;
    Procedure SetReg(Const aName: String; aValue: Integer);
    Function OperandValue(Const aOp: String): Integer;
    Function CmdIndexFromLine(aLine: Integer): Integer;

    Function FindNextCmd(aCmdIdx: Integer; IgnoreCalls: Boolean = false): Integer;
    Function ResolveRunnable(aCmdIdx: Integer): Integer;
    Function IsBranchTaken(aCmdIdx: Integer): Boolean;
    Procedure ChangePipeline(aSlot, aNewIdx: Integer);
    Procedure FlushFrom(aFromSlot: Integer);
    Procedure DoFetch(aSlot: Integer);
    Procedure EnsureFetch;
    Procedure DoExecute(aSlot: Integer);
    Function DoWriteBack(aSlot: Integer): Boolean;
    Procedure PushStack(aValue: Integer);
    Function PopStack: Integer;
    Function GetMemory(aAddress: Integer): Integer;
    Procedure SetMemory(aAddress, aValue: Integer);
  public
    Constructor Create;
    Destructor Destroy; override;

    // Load a compiled program and reset all CPU state.
    Procedure LoadProgram(Const aCmds: TAssemblerCMDs; aPipelineMode: Boolean = false);

    // Advance the CPU by one clock tick.
    // Returns True when the program has halted (HLT or end of code).
    Function Step(): Boolean;

    // Run until halted or aMaxSteps ticks have elapsed (safety valve).
    Procedure RunToHalt(aMaxSteps: Integer = 10000);

    Property RegA: Integer read fRegA write fRegA;
    Property RegB: Integer read fRegB write fRegB;
    Property RegC: Integer read fRegC write fRegC;
    Property RegD: Integer read fRegD write fRegD;
    Property FlagZero: Boolean read fFlagZero;
    Property FlagCarry: Boolean read fFlagCarry;
    Property FlagNegative: Boolean read fFlagNegative;
    Property Halted: Boolean read fHalted;
    Property EndedWithoutHalt: Boolean read fEndedWithoutHalt;
    Function StackCount: Integer;
    Function StackTop: Integer; // top = most recently pushed
    Function GetStackValue(aIndex: Integer): Integer;
    Function GetMemoryValue(aAddress: Integer): Integer;
    Procedure SetMemoryValue(aAddress, aValue: Integer);
    Function GetPipelineDepth: Integer;
    Function GetPipelineSlot(aSlot: Integer): Integer;
    Function GetCommands: TAssemblerCMDs;
  End;

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
    cJC: result := 'JC';
    cJN: result := 'JN';
    cJNC: result := 'JNC';
    cJNN: result := 'JNN';
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
      'JNC': cmd.Cmd := cJNC;
      'JC': cmd.Cmd := cJC;
      'JN': cmd.Cmd := cJN;
      'JNN': cmd.Cmd := cJNN;
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
    If result[i].Cmd In [cJMP, cJC, cJN, cJNC, cJNN, cJZ, cJNZ, cCALL] Then Begin
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

// ---------------------------------------------------------------------------
// TCPUEngine Implementation
// ---------------------------------------------------------------------------

Constructor TCPUEngine.Create;
Begin
  Inherited Create;
  fPendingTarget := -1;
End;

Destructor TCPUEngine.Destroy;
Begin
  Inherited Destroy;
End;

// --- Stack helpers ----------------------------------------------------------

Procedure TCPUEngine.PushStack(aValue: Integer);
Begin
  SetLength(fStack, Length(fStack) + 1);
  // shift right
  If Length(fStack) > 1 Then
    Move(fStack[0], fStack[1], (Length(fStack) - 1) * SizeOf(Integer));
  fStack[0] := aValue;
End;

Function TCPUEngine.PopStack: Integer;
Begin
  result := 0;
  If Length(fStack) = 0 Then exit;
  result := fStack[0];
  If Length(fStack) > 1 Then
    Move(fStack[1], fStack[0], (Length(fStack) - 1) * SizeOf(Integer));
  SetLength(fStack, Length(fStack) - 1);
End;

Function TCPUEngine.StackCount: Integer;
Begin
  result := Length(fStack);
End;

Function TCPUEngine.StackTop: Integer;
Begin
  result := 0;
  If Length(fStack) > 0 Then
    result := fStack[0];
End;

Function TCPUEngine.GetStackValue(aIndex: Integer): Integer;
Begin
  result := 0;
  If (aIndex >= 0) And (aIndex < Length(fStack)) Then
    result := fStack[aIndex];
End;

Function TCPUEngine.GetMemory(aAddress: Integer): Integer;
Begin
  result := 0;
  If (aAddress >= Low(fMemory)) And (aAddress <= High(fMemory)) Then
    result := fMemory[aAddress];
End;

Procedure TCPUEngine.SetMemory(aAddress, aValue: Integer);
Begin
  If (aAddress >= Low(fMemory)) And (aAddress <= High(fMemory)) Then
    fMemory[aAddress] := aValue;
End;

Function TCPUEngine.GetMemoryValue(aAddress: Integer): Integer;
Begin
  result := GetMemory(aAddress);
End;

Procedure TCPUEngine.SetMemoryValue(aAddress, aValue: Integer);
Begin
  SetMemory(aAddress, aValue);
End;

Function TCPUEngine.GetPipelineDepth: Integer;
Begin
  result := fPipelineDepth;
End;

Function TCPUEngine.GetPipelineSlot(aSlot: Integer): Integer;
Begin
  result := -1;
  If (aSlot >= 0) And (aSlot <= High(fPipeline)) Then
    result := fPipeline[aSlot];
End;

Function TCPUEngine.GetCommands: TAssemblerCMDs;
Begin
  result := Copy(fCmds, 0, Length(fCmds));
End;

// --- Register helpers -------------------------------------------------------

Function TCPUEngine.GetReg(Const aName: String): Integer;
Begin
  result := 0;
  Case aName Of
    'A': result := fRegA;
    'B': result := fRegB;
    'C': result := fRegC;
    'D': result := fRegD;
  End;
End;

Procedure TCPUEngine.SetReg(Const aName: String; aValue: Integer);
Begin
  Case aName Of
    'A': fRegA := aValue;
    'B': fRegB := aValue;
    'C': fRegC := aValue;
    'D': fRegD := aValue;
  End;
End;

Function TCPUEngine.OperandValue(Const aOp: String): Integer;
Begin
  Case aOp Of
    'A': result := fRegA;
    'B': result := fRegB;
    'C': result := fRegC;
    'D': result := fRegD;
  Else
    result := StrToIntDef(aOp, 0);
  End;
End;

Function TCPUEngine.CmdIndexFromLine(aLine: Integer): Integer;
Var
  i: Integer;
Begin
  result := -1;
  For i := 0 To High(fCmds) Do Begin
    If fCmds[i].Line = aLine Then Begin
      result := i;
      exit;
    End;
  End;
End;

// --- Pipeline helpers -------------------------------------------------------

Function TCPUEngine.FindNextCmd(aCmdIdx: Integer; IgnoreCalls: Boolean): Integer;
Var
  i: Integer;
Begin
  If (aCmdIdx < 0) Or (aCmdIdx > High(fCmds)) Then Begin
    result := High(fCmds) + 1;
    exit;
  End;
  result := aCmdIdx + 1;
  If result > High(fCmds) Then exit;
  If (fCmds[aCmdIdx].Cmd = cJMP)
    Or ((fCmds[aCmdIdx].Cmd = cCALL) And (Not IgnoreCalls)) Then Begin
    For i := 0 To High(fCmds) Do Begin
      If fCmds[i].Line = fCmds[aCmdIdx].JumpTarget Then Begin
        result := i;
        break;
      End;
    End;
  End;
  While (result <= High(fCmds)) And (fCmds[result].Cmd = cLabel) Do
    result := result + 1;
End;

Function TCPUEngine.ResolveRunnable(aCmdIdx: Integer): Integer;
Begin
  result := aCmdIdx;
  If (result < 0) Or (result > High(fCmds)) Then Begin
    result := -1;
    exit;
  End;
  While (result <= High(fCmds)) And (fCmds[result].Cmd = cLabel) Do
    Inc(result);
  If result > High(fCmds) Then result := -1;
End;

Function TCPUEngine.IsBranchTaken(aCmdIdx: Integer): Boolean;
Begin
  result := false;
  If (aCmdIdx < 0) Or (aCmdIdx > High(fCmds)) Then exit;
  Case fCmds[aCmdIdx].Cmd Of
    cJMP: result := true;
    cJZ: result := fFlagZero;
    cJNZ: result := Not fFlagZero;
    cJC: result := fFlagCarry;
    cJN: result := fFlagNegative;
    cJNC: result := Not fFlagCarry;
    cJNN: result := Not fFlagNegative;
  End;
End;

Procedure TCPUEngine.ChangePipeline(aSlot, aNewIdx: Integer);
Begin
  fPipeline[aSlot] := aNewIdx;
  If (aNewIdx >= 0) And (aNewIdx <= High(fCmds)) Then
    fCmds[aNewIdx].PipelineStep := psFetch;
End;

Procedure TCPUEngine.FlushFrom(aFromSlot: Integer);
Var
  i: Integer;
Begin
  For i := aFromSlot To fPipelineDepth - 1 Do Begin
    If (fPipeline[i] >= 0) And (fPipeline[i] <= High(fCmds)) Then
      fCmds[fPipeline[i]].PipelineStep := psNone;
    fPipeline[i] := -1;
  End;
End;

Procedure TCPUEngine.DoFetch(aSlot: Integer);
Var
  nextIdx: Integer;
Begin
  fCmds[fPipeline[aSlot]].PipelineStep := psDecode;
  If fPipelineMode And (aSlot < 3) Then Begin
    nextIdx := FindNextCmd(fPipeline[aSlot]);
    fPipeline[aSlot + 1] := nextIdx;
    If (nextIdx >= 0) And (nextIdx <= High(fCmds)) Then Begin
      fCmds[nextIdx].PipelineStep := psFetch;
    End
    Else
      fPipeline[aSlot + 1] := -1;
  End;
End;

Procedure TCPUEngine.EnsureFetch;
Var
  i, highestUsed, nextIdx: Integer;
  hasFetch: Boolean;
Begin
  If Not fPipelineMode Then exit;
  highestUsed := -1;
  hasFetch := false;
  For i := 0 To fPipelineDepth - 1 Do Begin
    If (fPipeline[i] >= 0) And (fPipeline[i] <= High(fCmds)) Then Begin
      highestUsed := i;
      If fCmds[fPipeline[i]].PipelineStep = psFetch Then
        hasFetch := true;
    End;
  End;
  If hasFetch Then exit;
  If (highestUsed = -1) Or (highestUsed >= fPipelineDepth - 1) Then exit;
  nextIdx := FindNextCmd(fPipeline[highestUsed]);
  If nextIdx > High(fCmds) Then exit;
  fPipeline[highestUsed + 1] := nextIdx;
  fCmds[nextIdx].PipelineStep := psFetch;
End;

Procedure TCPUEngine.DoExecute(aSlot: Integer);
Var
  cmd: TAssemblerCMD;
  branchTarget: Integer;
Begin
  cmd := fCmds[fPipeline[aSlot]];

  // CMP always evaluated at Execute phase (matching hardware semantics)
  If cmd.Cmd = cCMP Then Begin
    fFlagZero := OperandValue(cmd.LeftOperand) = OperandValue(cmd.RightOperand);
    fFlagCarry := OperandValue(cmd.LeftOperand) < OperandValue(cmd.RightOperand);
    fFlagNegative := (OperandValue(cmd.LeftOperand) - OperandValue(cmd.RightOperand)) < 0;
  End;

  // Pipeline-mode only: resolve branches/returns/halt at Execute
  If fPipelineMode Then Begin
    If cmd.Cmd In [cJMP, cJC, cJN, cJNC, cJNN, cJZ, cJNZ] Then Begin
      If IsBranchTaken(fPipeline[aSlot]) Then Begin
        FlushFrom(aSlot + 1);
        branchTarget := ResolveRunnable(fBranchTargets[fPipeline[aSlot]]);
        If (branchTarget >= 0) And (branchTarget <= High(fCmds)) Then Begin
          If aSlot < fPipelineDepth - 1 Then Begin
            fPipeline[aSlot + 1] := branchTarget;
            fCmds[branchTarget].PipelineStep := psFetch;
            fPendingTarget := -1;
          End
          Else
            fPendingTarget := branchTarget;
        End;
      End;
    End;

    If cmd.Cmd = cRET Then Begin
      FlushFrom(aSlot + 1);
      If Length(fStack) > 0 Then Begin
        // Stack holds 1-based line of the CALL instruction; find next after it
        branchTarget := CmdIndexFromLine(PopStack - 1);
        branchTarget := FindNextCmd(branchTarget, true);
        If (branchTarget >= 0) And (branchTarget <= High(fCmds)) Then Begin
          If aSlot < fPipelineDepth - 1 Then Begin
            fPipeline[aSlot + 1] := branchTarget;
            fCmds[branchTarget].PipelineStep := psFetch;
            fPendingTarget := -1;
          End
          Else
            fPendingTarget := branchTarget;
        End;
      End;
    End;

    If cmd.Cmd = cHLT Then
      FlushFrom(aSlot + 1);
  End;

  fCmds[fPipeline[aSlot]].PipelineStep := psWriteBack;
End;

Function TCPUEngine.DoWriteBack(aSlot: Integer): Boolean;
Var
  cmd: TAssemblerCMD;
  vRight, retTarget: Integer;
  fromRet: Boolean;
Begin
  result := false;
  fromRet := false;
  cmd := fCmds[fPipeline[aSlot]];

  Case cmd.Cmd Of
    cHLT: Begin
        fHalted := true;
        result := true;
        exit;
      End;
    cNOP, cCMP, cLabel: ; // nothing to commit (CMP done at Execute)

    cRET: Begin
        If Not fPipelineMode Then Begin
          If Length(fStack) > 0 Then Begin
            retTarget := CmdIndexFromLine(PopStack - 1);
            retTarget := FindNextCmd(retTarget, true);
            ChangePipeline(aSlot, retTarget);
            fromRet := true;
          End;
        End;
        // Pipeline mode: already handled in DoExecute
      End;

    cCALL: Begin
        // Push return address (1-based line of CALL = aCMD.Line+1)
        PushStack(cmd.Line + 1);
        // Redirect pipeline slot to call target
        If fPipelineMode Then
          ChangePipeline(aSlot, fBranchTargets[fPipeline[aSlot]])
        Else
          ChangePipeline(aSlot, ResolveRunnable(fBranchTargets[fPipeline[aSlot]]));
        fromRet := true; // suppress standard PC advance below
      End;

    cJMP: Begin
        If Not fPipelineMode Then
          ChangePipeline(aSlot, ResolveRunnable(fBranchTargets[fPipeline[aSlot]]));
        fromRet := true; // JMP never "falls through"
      End;
    cJZ: Begin
        If fPipelineMode Then Begin
          fromRet := true;
        End
        Else Begin
          If fFlagZero Then Begin
            ChangePipeline(aSlot, ResolveRunnable(fBranchTargets[fPipeline[aSlot]]));
            fromRet := true;
          End;
        End;
      End;
    cJNZ: Begin
        If fPipelineMode Then Begin
          fromRet := true;
        End
        Else Begin
          If Not fFlagZero Then Begin
            ChangePipeline(aSlot, ResolveRunnable(fBranchTargets[fPipeline[aSlot]]));
            fromRet := true;
          End;
        End;
      End;
    cJC: Begin
        If fPipelineMode Then Begin
          fromRet := true;
        End
        Else Begin
          If fFlagCarry Then Begin
            ChangePipeline(aSlot, ResolveRunnable(fBranchTargets[fPipeline[aSlot]]));
            fromRet := true;
          End;
        End;
      End;
    cJNC: Begin
        If fPipelineMode Then Begin
          fromRet := true;
        End
        Else Begin
          If Not fFlagCarry Then Begin
            ChangePipeline(aSlot, ResolveRunnable(fBranchTargets[fPipeline[aSlot]]));
            fromRet := true;
          End;
        End;
      End;
    cJN: Begin
        If fPipelineMode Then Begin
          fromRet := true;
        End
        Else Begin
          If fFlagNegative Then Begin
            ChangePipeline(aSlot, ResolveRunnable(fBranchTargets[fPipeline[aSlot]]));
            fromRet := true;
          End;
        End;
      End;
    cJNN: Begin
        If fPipelineMode Then Begin
          fromRet := true;
        End
        Else Begin
          If Not fFlagNegative Then Begin
            ChangePipeline(aSlot, ResolveRunnable(fBranchTargets[fPipeline[aSlot]]));
            fromRet := true;
          End;
        End;
      End;

    cMOV: SetReg(cmd.LeftOperand, OperandValue(cmd.RightOperand));
    cLOAD: SetReg(cmd.LeftOperand, GetMemory(OperandValue(cmd.RightOperand)));
    cADD: SetReg(cmd.LeftOperand, GetReg(cmd.LeftOperand) + OperandValue(cmd.RightOperand));
    cSUB: SetReg(cmd.LeftOperand, GetReg(cmd.LeftOperand) - OperandValue(cmd.RightOperand));
    cAND: SetReg(cmd.LeftOperand, GetReg(cmd.LeftOperand) And OperandValue(cmd.RightOperand));
    cOR: SetReg(cmd.LeftOperand, GetReg(cmd.LeftOperand) Or OperandValue(cmd.RightOperand));
    cXOR: SetReg(cmd.LeftOperand, GetReg(cmd.LeftOperand) Xor OperandValue(cmd.RightOperand));
    cMUL: SetReg(cmd.LeftOperand, GetReg(cmd.LeftOperand) * OperandValue(cmd.RightOperand));
    cDIV: Begin
        vRight := OperandValue(cmd.RightOperand);
        If vRight <> 0 Then
          SetReg(cmd.LeftOperand, GetReg(cmd.LeftOperand) Div vRight);
      End;
    cSHL: SetReg(cmd.LeftOperand, GetReg(cmd.LeftOperand) Shl OperandValue(cmd.RightOperand));
    cSHR: SetReg(cmd.LeftOperand, GetReg(cmd.LeftOperand) Shr OperandValue(cmd.RightOperand));
    cNOT: SetReg(cmd.LeftOperand, Not GetReg(cmd.LeftOperand));
    cSTORE: SetMemory(OperandValue(cmd.RightOperand), OperandValue(cmd.LeftOperand));

    cPUSH: PushStack(OperandValue(cmd.LeftOperand));
    cPOP: Begin
        If Length(fStack) > 0 Then
          SetReg(cmd.LeftOperand, PopStack);
      End;
  End;

  // Advance PC in non-pipeline mode
  If Not fPipelineMode Then Begin
    If Not fromRet Then
      ChangePipeline(aSlot, FindNextCmd(fPipeline[aSlot]));

    // End of program without explicit HLT: stop cleanly.
    If (fPipeline[aSlot] >= 0) And (fPipeline[aSlot] <= High(fCmds)) Then
      fCmds[fPipeline[aSlot]].PipelineStep := psFetch
    Else Begin
      fEndedWithoutHalt := true;
      fHalted := true;
      result := true;
      exit;
    End;
  End;
End;

// --- Public -----------------------------------------------------------------

Procedure TCPUEngine.LoadProgram(Const aCmds: TAssemblerCMDs; aPipelineMode: Boolean);
Var
  i, j, memAddr: Integer;
Begin
  fCmds := Copy(aCmds, 0, Length(aCmds));
  fPipelineMode := aPipelineMode;
  fPipelineDepth := IfThen(aPipelineMode, 4, 1);

  // Reset CPU state
  fRegA := 0;
  fRegB := 0;
  fRegC := 0;
  fRegD := 0;
  fFlagZero := false;
  fFlagCarry := false;
  fFlagNegative := false;
  SetLength(fStack, 0);
  For memAddr := Low(fMemory) To High(fMemory) Do
    fMemory[memAddr] := 0;
  fHalted := false;
  fEndedWithoutHalt := false;
  fPendingTarget := -1;

  // Reset pipeline
  For i := 0 To 3 Do
    fPipeline[i] := -1;
  For i := 0 To High(fCmds) Do
    fCmds[i].PipelineStep := psNone;

  // Build branch-target table (cmd index for each jump target)
  SetLength(fBranchTargets, Length(fCmds));
  For i := 0 To High(fCmds) Do Begin
    fBranchTargets[i] := -1;
    If fCmds[i].Cmd In [cJMP, cJC, cJN, cJNC, cJNN, cJZ, cJNZ, cCALL] Then Begin
      For j := 0 To High(fCmds) Do Begin
        If fCmds[j].Line = fCmds[i].JumpTarget Then Begin
          fBranchTargets[i] := j;
          break;
        End;
      End;
    End;
  End;

  // Place first instruction into pipeline slot 0
  If Length(fCmds) = 0 Then Begin
    fHalted := true;
    exit;
  End;
  fPipeline[0] := 0;
  fCmds[0].PipelineStep := psFetch;
End;

Function TCPUEngine.Step(): Boolean;
Var
  p, i: Integer;
  rawHazardStallFrom: Integer;
  stackHazard: Boolean;
Begin
  result := false;
  If fHalted Then Begin
    result := true;
    exit;
  End;
  If (fPipeline[0] < 0) Or (fPipeline[0] > High(fCmds)) Then Begin
    fEndedWithoutHalt := true;
    fHalted := true;
    result := true;
    exit;
  End;

  // RAW hazard: stall slots >= 1 when register WriteBack and CMP/PUSH are in Execute
  rawHazardStallFrom := fPipelineDepth;
  If fPipelineMode And
    (fPipeline[0] >= 0) And (fPipeline[0] <= High(fCmds)) And
    (fPipeline[1] >= 0) And (fPipeline[1] <= High(fCmds)) And
    (fCmds[fPipeline[0]].PipelineStep = psWriteBack) And
    (fCmds[fPipeline[1]].PipelineStep = psExecute) And
    (fCmds[fPipeline[0]].Cmd In [cADD, cAND, cDIV, cNOT, cMUL, cOR, cSHL, cSHR, cSUB, cXOR, cMOV, cLOAD, cPOP]) Then Begin
    If fCmds[fPipeline[1]].Cmd = cCMP Then Begin
      If (fCmds[fPipeline[1]].LeftOperand = fCmds[fPipeline[0]].LeftOperand) Or
        (fCmds[fPipeline[1]].RightOperand = fCmds[fPipeline[0]].LeftOperand) Then
        rawHazardStallFrom := 1;
    End;
    If fCmds[fPipeline[1]].Cmd = cPUSH Then Begin
      If fCmds[fPipeline[1]].LeftOperand = fCmds[fPipeline[0]].LeftOperand Then
        rawHazardStallFrom := 1;
    End;
  End;

  stackHazard := false;
  If fPipelineMode And
    (fPipeline[0] >= 0) And (fPipeline[0] <= High(fCmds)) And
    (fPipeline[1] >= 0) And (fPipeline[1] <= High(fCmds)) And
    (fCmds[fPipeline[0]].PipelineStep = psWriteBack) And
    (fCmds[fPipeline[1]].PipelineStep = psExecute) And
    (fCmds[fPipeline[1]].Cmd = cRET) And
    (fCmds[fPipeline[0]].Cmd In [cCALL, cPUSH, cPOP, cRET]) Then Begin
    stackHazard := true;
  End;

  For p := fPipelineDepth - 1 Downto 0 Do Begin
    If fPipeline[p] = -1 Then Continue;
    If p >= rawHazardStallFrom Then Continue;
    If stackHazard And (p = 1) Then Continue;
    Case fCmds[fPipeline[p]].PipelineStep Of
      psFetch: DoFetch(p);
      psDecode: fCmds[fPipeline[p]].PipelineStep := psExecute;
      psExecute: DoExecute(p);
      psWriteBack: Begin
          If DoWriteBack(p) Then Begin
            result := true;
            exit;
          End;
          If fPipelineMode And (p = 0) Then Begin
            // Shift pipeline left
            For i := 0 To 2 Do
              fPipeline[i] := fPipeline[i + 1];
            fPipeline[3] := -1;
            If (fPendingTarget >= 0) And (fPendingTarget <= High(fCmds)) Then Begin
              fPipeline[fPipelineDepth - 1] := fPendingTarget;
              fCmds[fPendingTarget].PipelineStep := psFetch;
              fPendingTarget := -1;
            End;
            EnsureFetch;
          End;
        End;
    End;
  End;
  result := fHalted;
End;

Procedure TCPUEngine.RunToHalt(aMaxSteps: Integer);
Var
  i: Integer;
Begin
  For i := 1 To aMaxSteps Do
    If Step() Then exit;
End;

End.

