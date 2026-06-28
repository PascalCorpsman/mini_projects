Unit TestCase1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, fpcunit, testregistry, uFPC_CPU;

Type

  { TFPC_CPU_tests }

  TFPC_CPU_tests = Class(TTestCase)
  private
    // Helper: compile one or more lines separated by line-feed
    Function Compile(Const aCode: String): TAssemblerCMDs;
    // Helper: run the code through the engine and return it
    Function Run(Const aCode: String; Pipeline: Boolean = false): TCPUEngine;
  protected
    Procedure SetUp; override;
    Procedure TearDown; override;
  published
    // ---- Compiler tests ------------------------------------------------
    Procedure TestCompile_Empty;
    Procedure TestCompile_SingleMOV;
    Procedure TestCompile_Comment;
    Procedure TestCompile_LabelAndJump;
    Procedure TestCompile_InvalidCommand;
    Procedure TestCompile_UnresolvedLabel;

    // ---- Execution: basic -----------------------------------------------
    Procedure TestExec_MOV_Immediate;
    Procedure TestExec_MOV_Register;
    Procedure TestExec_ADD;
    Procedure TestExec_SUB;
    Procedure TestExec_MUL;
    Procedure TestExec_DIV;
    Procedure TestExec_AND;
    Procedure TestExec_OR;
    Procedure TestExec_XOR;
    Procedure TestExec_SHL;
    Procedure TestExec_SHR;
    Procedure TestExec_NOT;

    // ---- Execution: flags / CMP -----------------------------------------
    Procedure TestExec_CMP_Equal;
    Procedure TestExec_CMP_Less;
    Procedure TestExec_CMP_Negative;
    Procedure TestExec_JZ_Taken;
    Procedure TestExec_JZ_NotTaken;
    Procedure TestExec_JNZ_Taken;
    Procedure TestExec_JNZ_NotTaken;
    Procedure TestExec_JN_Taken;
    Procedure TestExec_JN_NotTaken;
    Procedure TestExec_JNN_Taken;
    Procedure TestExec_JNN_NotTaken;
    Procedure TestExec_JMP;

    // ---- Execution: stack -----------------------------------------------
    Procedure TestExec_LOAD_STORE;
    Procedure TestExec_PUSH_POP;
    Procedure TestExec_CALL_RET;
    Procedure TestExec_SumArrayProgram;
    Procedure TestExec_SumArrayProgram_Pipeline;

    // ---- Pipeline regression tests --------------------------------------
    // The test that originally exposed the hazard:
    //   MOV A,1 / MOV B,2 / ADD A,B / PUSH A  → stack must hold 3, not 1
    Procedure TestPipeline_PushAfterADD_NoPipeline;
    Procedure TestPipeline_PushAfterADD_Pipeline;
    Procedure TestPipeline_CMPAfterADD;
  End;

Implementation

{ ---- Helpers -------------------------------------------------------------- }

Function TFPC_CPU_tests.Compile(Const aCode: String): TAssemblerCMDs;
Var
  sl: TStringList;
Begin
  sl := TStringList.Create;
  Try
    sl.Text := aCode;
    result := uFPC_CPU.Compile(sl);
  Finally
    sl.Free;
  End;
End;

Function TFPC_CPU_tests.Run(Const aCode: String; Pipeline: Boolean): TCPUEngine;
Var
  cmds: TAssemblerCMDs;
Begin
  cmds := Compile(aCode);
  AssertTrue('Compile failed: ' + LastError, Assigned(cmds));
  result := TCPUEngine.Create;
  result.LoadProgram(cmds, Pipeline);
  result.RunToHalt;
End;

{ ---- Setup / Teardown ----------------------------------------------------- }

Procedure TFPC_CPU_tests.SetUp;
Begin
End;

Procedure TFPC_CPU_tests.TearDown;
Begin
End;

{ ---- Compiler tests ------------------------------------------------------- }

Procedure TFPC_CPU_tests.TestCompile_Empty;
Var
  cmds: TAssemblerCMDs;
Begin
  cmds := Compile('');
  AssertFalse('Empty source should yield nil/empty result', Assigned(cmds) And (Length(cmds) > 0));
End;

Procedure TFPC_CPU_tests.TestCompile_SingleMOV;
Var
  cmds: TAssemblerCMDs;
Begin
  cmds := Compile('MOV A, 42');
  AssertTrue('Compile returned nil', Assigned(cmds));
  AssertEquals('Expected 1 command', 1, Length(cmds));
  AssertTrue('Command should be cMOV', cmds[0].Cmd = cMOV);
  AssertEquals('LeftOperand', 'A', cmds[0].LeftOperand);
  AssertEquals('RightOperand', '42', cmds[0].RightOperand);
End;

Procedure TFPC_CPU_tests.TestCompile_Comment;
Var
  cmds: TAssemblerCMDs;
Begin
  // Inline comment should be stripped; only one instruction compiled
  cmds := Compile('MOV A, 1 ; this is a comment');
  AssertTrue('Compile returned nil', Assigned(cmds));
  AssertEquals('Expected 1 command', 1, Length(cmds));
  AssertEquals('RightOperand should be 1', '1', cmds[0].RightOperand);
End;

Procedure TFPC_CPU_tests.TestCompile_LabelAndJump;
Var
  cmds: TAssemblerCMDs;
  i, labelLine, jmpIdx: Integer;
Begin
  cmds := Compile('JMP END_LABEL' + LineEnding + 'NOP' + LineEnding + 'END_LABEL:');
  AssertTrue('Compile returned nil', Assigned(cmds));
  // Find JMP and label
  jmpIdx := -1;
  labelLine := -1;
  For i := 0 To High(cmds) Do Begin
    If cmds[i].Cmd = cJMP Then jmpIdx := i;
    If cmds[i].Cmd = cLabel Then labelLine := cmds[i].Line;
  End;
  AssertTrue('JMP not found', jmpIdx >= 0);
  AssertTrue('Label not found', labelLine >= 0);
  AssertEquals('JMP target line should point to label line', labelLine, cmds[jmpIdx].JumpTarget);
End;

Procedure TFPC_CPU_tests.TestCompile_InvalidCommand;
Var
  cmds: TAssemblerCMDs;
Begin
  cmds := Compile('FOOBAR A, B');
  AssertFalse('Invalid command should return nil/empty', Assigned(cmds) And (Length(cmds) > 0));
  AssertTrue('LastError should be set', LastError <> '');
End;

Procedure TFPC_CPU_tests.TestCompile_UnresolvedLabel;
Var
  cmds: TAssemblerCMDs;
Begin
  cmds := Compile('JMP NONEXISTENT');
  AssertFalse('Unresolved label should return nil/empty', Assigned(cmds) And (Length(cmds) > 0));
  AssertTrue('LastError should mention the label', LastError <> '');
End;

{ ---- Basic execution tests ------------------------------------------------ }

Procedure TFPC_CPU_tests.TestExec_MOV_Immediate;
Var
  eng: TCPUEngine;
Begin
  eng := Run('MOV A, 99' + LineEnding + 'HLT');
  Try
    AssertEquals('A = 99', 99, eng.RegA);
  Finally eng.Free;
  End;
End;

Procedure TFPC_CPU_tests.TestExec_MOV_Register;
Var
  eng: TCPUEngine;
Begin
  eng := Run('MOV A, 7' + LineEnding + 'MOV B, A' + LineEnding + 'HLT');
  Try
    AssertEquals('A = 7', 7, eng.RegA);
    AssertEquals('B = 7', 7, eng.RegB);
  Finally eng.Free;
  End;
End;

Procedure TFPC_CPU_tests.TestExec_ADD;
Var
  eng: TCPUEngine;
Begin
  eng := Run('MOV A, 3' + LineEnding + 'MOV B, 4' + LineEnding + 'ADD A, B' + LineEnding + 'HLT');
  Try
    AssertEquals('A = 7', 7, eng.RegA);
  Finally eng.Free;
  End;
End;

Procedure TFPC_CPU_tests.TestExec_SUB;
Var
  eng: TCPUEngine;
Begin
  eng := Run('MOV A, 10' + LineEnding + 'MOV B, 3' + LineEnding + 'SUB A, B' + LineEnding + 'HLT');
  Try
    AssertEquals('A = 7', 7, eng.RegA);
  Finally eng.Free;
  End;
End;

Procedure TFPC_CPU_tests.TestExec_MUL;
Var
  eng: TCPUEngine;
Begin
  eng := Run('MOV A, 6' + LineEnding + 'MOV B, 7' + LineEnding + 'MUL A, B' + LineEnding + 'HLT');
  Try
    AssertEquals('A = 42', 42, eng.RegA);
  Finally eng.Free;
  End;
End;

Procedure TFPC_CPU_tests.TestExec_DIV;
Var
  eng: TCPUEngine;
Begin
  eng := Run('MOV A, 20' + LineEnding + 'MOV B, 4' + LineEnding + 'DIV A, B' + LineEnding + 'HLT');
  Try
    AssertEquals('A = 5', 5, eng.RegA);
  Finally eng.Free;
  End;
End;

Procedure TFPC_CPU_tests.TestExec_AND;
Var
  eng: TCPUEngine;
Begin
  eng := Run('MOV A, 12' + LineEnding + 'MOV B, 10' + LineEnding + 'AND A, B' + LineEnding + 'HLT');
  Try
    AssertEquals('A = 8', 8, eng.RegA); // 1100 AND 1010 = 1000
  Finally eng.Free;
  End;
End;

Procedure TFPC_CPU_tests.TestExec_OR;
Var
  eng: TCPUEngine;
Begin
  eng := Run('MOV A, 12' + LineEnding + 'MOV B, 10' + LineEnding + 'OR A, B' + LineEnding + 'HLT');
  Try
    AssertEquals('A = 14', 14, eng.RegA); // 1100 OR 1010 = 1110
  Finally eng.Free;
  End;
End;

Procedure TFPC_CPU_tests.TestExec_XOR;
Var
  eng: TCPUEngine;
Begin
  eng := Run('MOV A, 12' + LineEnding + 'MOV B, 10' + LineEnding + 'XOR A, B' + LineEnding + 'HLT');
  Try
    AssertEquals('A = 6', 6, eng.RegA); // 1100 XOR 1010 = 0110
  Finally eng.Free;
  End;
End;

Procedure TFPC_CPU_tests.TestExec_SHL;
Var
  eng: TCPUEngine;
Begin
  eng := Run('MOV A, 3' + LineEnding + 'MOV B, 2' + LineEnding + 'SHL A, B' + LineEnding + 'HLT');
  Try
    AssertEquals('A = 12', 12, eng.RegA);
  Finally eng.Free;
  End;
End;

Procedure TFPC_CPU_tests.TestExec_SHR;
Var
  eng: TCPUEngine;
Begin
  eng := Run('MOV A, 12' + LineEnding + 'MOV B, 2' + LineEnding + 'SHR A, B' + LineEnding + 'HLT');
  Try
    AssertEquals('A = 3', 3, eng.RegA);
  Finally eng.Free;
  End;
End;

Procedure TFPC_CPU_tests.TestExec_NOT;
Var
  eng: TCPUEngine;
Begin
  eng := Run('MOV A, 0' + LineEnding + 'NOT A' + LineEnding + 'HLT');
  Try
    AssertEquals('A = NOT 0 = -1', -1, eng.RegA);
  Finally eng.Free;
  End;
End;

{ ---- Flags / CMP ---------------------------------------------------------- }

Procedure TFPC_CPU_tests.TestExec_CMP_Equal;
Var
  eng: TCPUEngine;
Begin
  eng := Run('MOV A, 5' + LineEnding + 'MOV B, 5' + LineEnding + 'CMP A, B' + LineEnding + 'HLT');
  Try
    AssertTrue('Zero flag should be set', eng.FlagZero);
    AssertFalse('Carry flag should not be set', eng.FlagCarry);
  Finally eng.Free;
  End;
End;

Procedure TFPC_CPU_tests.TestExec_CMP_Less;
Var
  eng: TCPUEngine;
Begin
  eng := Run('MOV A, 3' + LineEnding + 'MOV B, 7' + LineEnding + 'CMP A, B' + LineEnding + 'HLT');
  Try
    AssertFalse('Zero flag should not be set', eng.FlagZero);
    AssertTrue('Carry flag should be set (A < B)', eng.FlagCarry);
  Finally eng.Free;
  End;
End;

Procedure TFPC_CPU_tests.TestExec_CMP_Negative;
Var
  eng: TCPUEngine;
Begin
  eng := Run('MOV A, 3' + LineEnding + 'MOV B, 7' + LineEnding + 'CMP A, B' + LineEnding + 'HLT');
  Try
    AssertTrue('Negative flag should be set (A-B < 0)', eng.FlagNegative);
  Finally eng.Free;
  End;

  eng := Run('MOV A, 7' + LineEnding + 'MOV B, 3' + LineEnding + 'CMP A, B' + LineEnding + 'HLT');
  Try
    AssertFalse('Negative flag should not be set (A-B >= 0)', eng.FlagNegative);
  Finally eng.Free;
  End;
End;

Procedure TFPC_CPU_tests.TestExec_JZ_Taken;
Var
  eng: TCPUEngine;
Begin
  // JZ should jump over MOV A,99 when zero flag is set
  eng := Run(
    'MOV A, 5' + LineEnding +
    'MOV B, 5' + LineEnding +
    'CMP A, B' + LineEnding +
    'JZ DONE' + LineEnding +
    'MOV A, 99' + LineEnding +
    'DONE:' + LineEnding +
    'HLT');
  Try
    AssertEquals('A should stay 5 (jump taken)', 5, eng.RegA);
  Finally eng.Free;
  End;
End;

Procedure TFPC_CPU_tests.TestExec_JZ_NotTaken;
Var
  eng: TCPUEngine;
Begin
  eng := Run(
    'MOV A, 1' + LineEnding +
    'MOV B, 2' + LineEnding +
    'CMP A, B' + LineEnding +
    'JZ DONE' + LineEnding +
    'MOV A, 42' + LineEnding +
    'DONE:' + LineEnding +
    'HLT');
  Try
    AssertEquals('A should be 42 (jump not taken)', 42, eng.RegA);
  Finally eng.Free;
  End;
End;

Procedure TFPC_CPU_tests.TestExec_JNZ_Taken;
Var
  eng: TCPUEngine;
Begin
  eng := Run(
    'MOV A, 1' + LineEnding +
    'MOV B, 2' + LineEnding +
    'CMP A, B' + LineEnding +
    'JNZ DONE' + LineEnding +
    'MOV A, 99' + LineEnding +
    'DONE:' + LineEnding +
    'HLT');
  Try
    AssertEquals('A should stay 1 (JNZ taken)', 1, eng.RegA);
  Finally eng.Free;
  End;
End;

Procedure TFPC_CPU_tests.TestExec_JNZ_NotTaken;
Var
  eng: TCPUEngine;
Begin
  eng := Run(
    'MOV A, 5' + LineEnding +
    'MOV B, 5' + LineEnding +
    'CMP A, B' + LineEnding +
    'JNZ DONE' + LineEnding +
    'MOV A, 77' + LineEnding +
    'DONE:' + LineEnding +
    'HLT');
  Try
    AssertEquals('A should be 77 (JNZ not taken, equal)', 77, eng.RegA);
  Finally
    eng.Free;
  End;
End;

Procedure TFPC_CPU_tests.TestExec_JN_Taken;
Var
  eng: TCPUEngine;
Begin
  eng := Run(
    'MOV A, 1' + LineEnding +
    'MOV B, 2' + LineEnding +
    'CMP A, B' + LineEnding +
    'JN DONE' + LineEnding +
    'MOV A, 99' + LineEnding +
    'DONE:' + LineEnding +
    'HLT');
  Try
    AssertEquals('A should stay 1 (JN taken)', 1, eng.RegA);
  Finally
    eng.Free;
  End;
End;

Procedure TFPC_CPU_tests.TestExec_JN_NotTaken;
Var
  eng: TCPUEngine;
Begin
  eng := Run(
    'MOV A, 5' + LineEnding +
    'MOV B, 2' + LineEnding +
    'CMP A, B' + LineEnding +
    'JN DONE' + LineEnding +
    'MOV A, 77' + LineEnding +
    'DONE:' + LineEnding +
    'HLT');
  Try
    AssertEquals('A should be 77 (JN not taken)', 77, eng.RegA);
  Finally
    eng.Free;
  End;
End;

Procedure TFPC_CPU_tests.TestExec_JNN_Taken;
Var
  eng: TCPUEngine;
Begin
  eng := Run(
    'MOV A, 5' + LineEnding +
    'MOV B, 2' + LineEnding +
    'CMP A, B' + LineEnding +
    'JNN DONE' + LineEnding +
    'MOV A, 99' + LineEnding +
    'DONE:' + LineEnding +
    'HLT');
  Try
    AssertEquals('A should stay 5 (JNN taken)', 5, eng.RegA);
  Finally
    eng.Free;
  End;
End;

Procedure TFPC_CPU_tests.TestExec_JNN_NotTaken;
Var
  eng: TCPUEngine;
Begin
  eng := Run(
    'MOV A, 1' + LineEnding +
    'MOV B, 2' + LineEnding +
    'CMP A, B' + LineEnding +
    'JNN DONE' + LineEnding +
    'MOV A, 66' + LineEnding +
    'DONE:' + LineEnding +
    'HLT');
  Try
    AssertEquals('A should be 66 (JNN not taken)', 66, eng.RegA);
  Finally
    eng.Free;
  End;
End;

Procedure TFPC_CPU_tests.TestExec_JMP;
Var
  eng: TCPUEngine;
Begin
  eng := Run(
    'JMP DONE' + LineEnding +
    'MOV A, 99' + LineEnding +
    'DONE:' + LineEnding +
    'HLT');
  Try
    AssertEquals('A should be 0 (skipped by JMP)', 0, eng.RegA);
  Finally
    eng.Free;
  End;
End;

{ ---- Stack tests ---------------------------------------------------------- }

Procedure TFPC_CPU_tests.TestExec_LOAD_STORE;
Var
  eng: TCPUEngine;
Begin
  eng := Run(
    'MOV A, 42' + LineEnding +
    'STORE A, 100' + LineEnding +
    'LOAD B, 100' + LineEnding +
    'HLT');
  Try
    AssertEquals('Memory[100] should be 42', 42, eng.GetMemoryValue(100));
    AssertEquals('B should be loaded with Memory[100]', 42, eng.RegB);
  Finally
    eng.Free;
  End;
End;

Procedure TFPC_CPU_tests.TestExec_PUSH_POP;
Var
  eng: TCPUEngine;
Begin
  eng := Run(
    'MOV A, 42' + LineEnding +
    'PUSH A' + LineEnding +
    'MOV A, 0' + LineEnding +
    'POP A' + LineEnding +
    'HLT');
  Try
    AssertEquals('Stack should be empty after POP', 0, eng.StackCount);
    AssertEquals('A should be restored to 42', 42, eng.RegA);
  Finally
    eng.Free;
  End;
End;

Procedure TFPC_CPU_tests.TestExec_CALL_RET;
Var
  eng: TCPUEngine;
Begin
  // CALL sets A=99 in subroutine, returns, then A stays 99
  eng := Run(
    'CALL MYSUB' + LineEnding +
    'HLT' + LineEnding +
    'MYSUB:' + LineEnding +
    'MOV A, 99' + LineEnding +
    'RET');
  Try
    AssertEquals('A should be 99 after CALL/RET', 99, eng.RegA);
    AssertEquals('Stack should be empty after RET', 0, eng.StackCount);
  Finally
    eng.Free;
  End;
End;

Procedure TFPC_CPU_tests.TestExec_SumArrayProgram;
Var
  eng: TCPUEngine;
  cmds: TAssemblerCMDs;
Begin
  cmds := Compile(
    'LOAD A,100' + LineEnding +
    'MOV B,101' + LineEnding +
    'PUSH B' + LineEnding +
    'PUSH A' + LineEnding +
    'CALL sumArray' + LineEnding +
    'POP A' + LineEnding +
    'STORE A,119' + LineEnding +
    'HLT' + LineEnding +
    'sumArray:' + LineEnding +
    'POP D' + LineEnding +
    'POP C' + LineEnding +
    'POP B' + LineEnding +
    'PUSH D' + LineEnding +
    'MOV D,0' + LineEnding +
    'loop:' + LineEnding +
    'CMP C,0' + LineEnding +
    'JZ end' + LineEnding +
    'LOAD A,B' + LineEnding +
    'ADD D,A' + LineEnding +
    'ADD B,1' + LineEnding +
    'SUB C,1' + LineEnding +
    'JMP loop' + LineEnding +
    'end:' + LineEnding +
    'POP A' + LineEnding +
    'PUSH D' + LineEnding +
    'PUSH A' + LineEnding +
    'RET');
  AssertTrue('Compile failed: ' + LastError, Assigned(cmds));

  eng := TCPUEngine.Create;
  Try
    eng.LoadProgram(cmds, false);
    eng.SetMemoryValue(100, 3);
    eng.SetMemoryValue(101, 1);
    eng.SetMemoryValue(102, 2);
    eng.SetMemoryValue(103, 3);
    eng.RunToHalt;

    AssertEquals('Result at Memory[119] should be 6', 6, eng.GetMemoryValue(119));
    AssertEquals('Stack should be empty at end', 0, eng.StackCount);
  Finally
    eng.Free;
  End;
End;

Procedure TFPC_CPU_tests.TestExec_SumArrayProgram_Pipeline;
Var
  eng: TCPUEngine;
  cmds: TAssemblerCMDs;
Begin
  cmds := Compile(
    'LOAD A,100' + LineEnding +
    'MOV B,101' + LineEnding +
    'PUSH B' + LineEnding +
    'PUSH A' + LineEnding +
    'CALL sumArray' + LineEnding +
    'POP A' + LineEnding +
    'STORE A,119' + LineEnding +
    'HLT' + LineEnding +
    'sumArray:' + LineEnding +
    'POP D' + LineEnding +
    'POP C' + LineEnding +
    'POP B' + LineEnding +
    'PUSH D' + LineEnding +
    'MOV D,0' + LineEnding +
    'loop:' + LineEnding +
    'CMP C,0' + LineEnding +
    'JZ end' + LineEnding +
    'LOAD A,B' + LineEnding +
    'ADD D,A' + LineEnding +
    'ADD B,1' + LineEnding +
    'SUB C,1' + LineEnding +
    'JMP loop' + LineEnding +
    'end:' + LineEnding +
    'POP A' + LineEnding +
    'PUSH D' + LineEnding +
    'PUSH A' + LineEnding +
    'RET');
  AssertTrue('Compile failed: ' + LastError, Assigned(cmds));

  eng := TCPUEngine.Create;
  Try
    eng.LoadProgram(cmds, true);
    eng.SetMemoryValue(100, 3);
    eng.SetMemoryValue(101, 1);
    eng.SetMemoryValue(102, 2);
    eng.SetMemoryValue(103, 3);
    eng.RunToHalt;

    AssertEquals('Result at Memory[119] should be 6 (pipeline)', 6, eng.GetMemoryValue(119));
    AssertEquals('Stack should be empty at end (pipeline)', 0, eng.StackCount);
  Finally
    eng.Free;
  End;
End;

{ ---- Pipeline regression tests -------------------------------------------- }

Procedure TFPC_CPU_tests.TestPipeline_PushAfterADD_NoPipeline;
// The original bug scenario executed in sequential (non-pipeline) mode.
// Regression: must still work correctly.
Var
  eng: TCPUEngine;
Begin
  eng := Run(
    'MOV A, 1' + LineEnding +
    'MOV B, 2' + LineEnding +
    'ADD A, B' + LineEnding +
    'PUSH A' + LineEnding +
    'HLT',
    {Pipeline=} false);
  Try
    AssertEquals('Stack should have 1 entry', 1, eng.StackCount);
    AssertEquals('Stack top must be 3 (non-pipeline)', 3, eng.StackTop);
  Finally eng.Free;
  End;
End;

Procedure TFPC_CPU_tests.TestPipeline_PushAfterADD_Pipeline;
// The actual regression: without the RAW-hazard stall, PUSH reads the
// register *before* ADD has written back, yielding 1 instead of 3.
Var
  eng: TCPUEngine;
Begin
  eng := Run(
    'MOV A, 1' + LineEnding +
    'MOV B, 2' + LineEnding +
    'ADD A, B' + LineEnding +
    'PUSH A' + LineEnding +
    'HLT',
    {Pipeline=} true);
  Try
    AssertEquals('Stack should have 1 entry', 1, eng.StackCount);
    AssertEquals('Stack top must be 3 (pipeline hazard fixed)', 3, eng.StackTop);
  Finally eng.Free;
  End;
End;

Procedure TFPC_CPU_tests.TestPipeline_CMPAfterADD;
// CMP must see the updated value of A after ADD, both in pipeline mode.
Var
  eng: TCPUEngine;
Begin
  eng := Run(
    'MOV A, 5' + LineEnding +
    'MOV B, 5' + LineEnding +
    'ADD A, B' + LineEnding + // A = 10
    'CMP A, B' + LineEnding + // 10 vs 5 → ZF=0, CF=0
    'HLT',
    {Pipeline=} true);
  Try
    AssertFalse('ZF should be 0 (10 <> 5)', eng.FlagZero);
    AssertFalse('CF should be 0 (10 >= 5)', eng.FlagCarry);
  Finally eng.Free;
  End;
End;

Initialization
  RegisterTest(TFPC_CPU_tests);
End.

