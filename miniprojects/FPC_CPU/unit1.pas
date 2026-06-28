(******************************************************************************)
(* FPC_CPU                                                         08.06.2026 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : This is a software emulated CPU, that shows how assembly     *)
(*               code is beeing processed in a CPU                            *)
(*                                                                            *)
(* License     : See the file license.md, located under:                      *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(* Warranty    : There is no warranty, neither in correctness of the          *)
(*               implementation, nor anything other that could happen         *)
(*               or go wrong, use at your own risk.                           *)
(*                                                                            *)
(* Known Issues: none                                                         *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  ExtCtrls, Menus, SynEdit, SynHighlighterAny, uFPC_CPU, SynEditMarks;

Type

  TLineInfo = Record
    isRunnable: Boolean;
    hasBreakPoint: Boolean;
    PipelineStep: TPipelineStep;
  End;

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    CheckBox5: TCheckBox;
    DebugMarks: TImageList;
    Edit7: TEdit;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SynAnySyn1: TSynAnySyn;
    SynEdit1: TSynEdit;
    Timer1: TTimer;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure SynEdit1GutterClick(Sender: TObject; X, Y, Line: integer;
      mark: TSynEditMark);
    Procedure SynEdit1KeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState
      );
    Procedure SynEdit1Paint(Sender: TObject; ACanvas: TCanvas);
    Procedure SynEdit1SpecialLineColors(Sender: TObject; Line: integer;
      Var Special: boolean; Var FG, BG: TColor);
  private
    aTick: integer;
    fLineInfo: Array Of TLineInfo;
    AktualLine: Integer;
    fEngine: TCPUEngine;
    defcaption: String;
    Function IsRunableLine(aLine: integer): Boolean;
    Function HasBreakpoint(aLine: integer): Boolean;
    Procedure ScrollToFetchCMD(Const i: Integer);
    Procedure SetLinePipeLineState(aLine: integer; aStep: TPipelineStep);
    Procedure SyncMemoryToEngine;
    Procedure SyncMemoryFromEngine;
    Procedure SyncStackViewFromEngine;
    Procedure SyncStateFromEngine;

  public

    Procedure ResetLCLToCompile;
    Procedure SetLCLToExecute;
    Procedure ResetCMDVisualizations;

    Procedure RefreshVisualization;
  End;

Var
  Form1: TForm1;
  FormShowOnce: Boolean = true;

Implementation

{$R *.lfm}

Uses LCLType, Unit2, math, uFPC_CPU_LCL;

Const
  IndexBreakPoint = 0;
  IndexIsRunnable = 1;
  IndexAktualLine = 2;
  IndexNotDebugableLine = 3;

  DefaultAutoStepTimeInMS = 100;

  { TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  (*
   * TODO:
   *  - F8
   *  - Die Flags Sinnvoll auswerten / Benutzen (da fehlen noch entsprechende Jump Befehle)
   *  - Branch Prediction unit!
   *)
  //Der Demo Code macht nun Murks
   (* Start with Debug
  // BUG: Was auch immer grad aus der ALU Raus kommt wird nur mit NOP korrekt gepusht
  SynEdit1.Clear;
  synedit1.lines.add('MOV A, 1');
  synedit1.lines.add('MOV B, 2');
  synedit1.lines.add('ADD A, B; Not matter which operand, inserting a NOP before push, fixes it');
  synedit1.lines.add('PUSH A ; BUG must be 3');
  // End of Debug *)
  defcaption := 'FPC_CPU ver 0.01 by Corpsman, www.Corpsman.de';
  caption := defcaption;
  Edit7.text := inttostr(DefaultAutoStepTimeInMS);
  fEngine := Nil;
End;

Procedure TForm1.FormShow(Sender: TObject);
Var
  w: Integer;
Begin
  If FormShowOnce Then Begin
    FormShowOnce := false;
    w := width + form2.Width + Scale96ToScreen(8);
    left := (Screen.Width - w) Div 2;
    form2.Left := left + Width + Scale96ToScreen(8);
    ResetLCLToCompile;
    form2.Show;
  End;
End;

Procedure TForm1.SynEdit1GutterClick(Sender: TObject; X, Y, Line: integer;
  mark: TSynEditMark);
Var
  j, i: Integer;
Begin
  // Convert 1 based Lines to 0 based lines as "usual"
  line := line - 1;

  If Line > high(fLineInfo) Then Begin
    j := length(fLineInfo);
    setlength(fLineInfo, Line + 1);
    For i := j To high(fLineInfo) Do Begin
      fLineInfo[i].isRunnable := false;
      fLineInfo[i].hasBreakPoint := false;
    End;
  End;
  fLineInfo[Line].hasBreakPoint := Not fLineInfo[Line].hasBreakPoint;
  SynEdit1.Invalidate;
End;

Procedure TForm1.SynEdit1KeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Const
{$IFDEF Windows}
  HashKey = 191;
{$ELSE}
  HashKey = 222; //ord('#');
{$ENDIF}
Var
  s: String;
  y2, y, i: Integer;
  Add: Boolean;
Begin
  // Toggle Breakpoint
  If (key = VK_F5) Then Begin
    SynEdit1GutterClick(Nil, 0, 0, SynEdit1.CaretY, Nil);
  End;
  // Step
  If (key = VK_F7) Then Begin
    If (button2.Enabled) Then Begin
      // Step in compiled code
      Button2.Click;
    End
    Else Begin
      // Step without a compiled code -> compile
      Button1.Click;
    End;
  End;
  // Compile
  If (key = VK_F9) Then Begin
    If Button1.Enabled Then Button1.Click; // Compile if needed
    If Button3.Enabled Then Button3.Click; // Run compiled code if possible
  End;
  // CTRL + F2
  If (ssCtrl In Shift) And (key = VK_F2) And (button4.enabled) Then Begin
    button4.Click;
  End;
  // CTRL + S
  If (ssCtrl In Shift) And (key = VK_S) Then Begin
    button6.Click;
  End;
  // CTRL + # toggles comment ;)
  If (ssCtrl In Shift) And (key = HashKey) Then Begin
    If SynEdit1.SelAvail Then Begin
      // There is a selection
      y := SynEdit1.BlockBegin.y - 1;
      y2 := SynEdit1.BlockEnd.y - 1;
    End
    Else Begin
      // The cursor is in one single line, no selection
      y := SynEdit1.CaretY;
      dec(y);
      y2 := Y;
    End;
    s := trim(SynEdit1.Lines[y]);
    Add := false;
    If (s <> '') Then Begin
      If s[1] <> ';' Then Begin
        Add := true;
      End;
    End;
    For i := y To y2 Do Begin
      If add Then Begin
        SynEdit1.Lines[i] := ';' + SynEdit1.Lines[i];
      End
      Else Begin
        If pos(';', trim(SynEdit1.Lines[i])) = 1 Then Begin
          s := SynEdit1.Lines[i];
          delete(s, pos(';', s), 1);
          SynEdit1.Lines[i] := s;
        End;
      End;
    End;
  End;
End;

Procedure TForm1.SynEdit1Paint(Sender: TObject; ACanvas: TCanvas);
Var
  index, aline, y, x: Integer;
Begin
  For aline := SynEdit1.TopLine - 1 To SynEdit1.TopLine + SynEdit1.Height Div SynEdit1.LineHeight Do Begin
    y := SynEdit1.LineHeight * (aline - SynEdit1.TopLine + 1);
    x := 0;
    index := -1;
    // Anzeigen aller Zeilen die der Rechner als Kompilierbar ansieht -> schwächste Prio
    If IsRunableLine(aline) Then index := IndexIsRunnable;
    If HasBreakpoint(aline) Then Begin
      If index = -1 Then Begin
        index := IndexNotDebugableLine;
      End
      Else Begin
        index := IndexBreakPoint;
      End;
    End;
    If (ALine = AktualLine) And Not CheckBox5.Checked Then Begin
      index := IndexAktualLine;
    End;
    If index <> -1 Then Begin
      DebugMarks.draw(SynEdit1.Canvas, x + 4, y, index);
    End;
  End;
End;

Procedure TForm1.SynEdit1SpecialLineColors(Sender: TObject; Line: integer;
  Var Special: boolean; Var FG, BG: TColor);
Begin
  // Convert 1 based Lines to 0 based lines as "usual"
  line := line - 1;
  If LastErrorLine = Line Then Begin
    Special := true;
    BG := clRed;
    FG := clWhite;
  End;
  If (line < 0) Or (Line > high(fLineInfo)) Then exit;
  Case fLineInfo[line].PipelineStep Of
    psFetch: Begin
        Special := true;
        BG := PipeLineFetchBGColor;
        fg := PipeLineFetchFGColor;
      End;
    psDecode: Begin
        Special := true;
        BG := PipeLineDecodeBGColor;
        fg := PipeLineDecodeFGColor;
      End;
    psExecute: Begin
        Special := true;
        BG := PipeLineExecuteBGColor;
        FG := PipeLineExecuteFGColor;
      End;
    psWriteBack: Begin
        Special := true;
        BG := PipeLineWritebackBGColor;
        FG := PipeLineWritebackFGColor;
      End;
  End;
End;

Function TForm1.IsRunableLine(aLine: integer): Boolean;
Begin
  result := false;
  If (aLine >= 0) And (aLine <= High(fLineInfo)) Then
    result := fLineInfo[aLine].isRunnable;
End;

Function TForm1.HasBreakpoint(aLine: integer): Boolean;
Begin
  result := false;
  If (aLine >= 0) And (aLine <= High(fLineInfo)) Then
    result := fLineInfo[aLine].hasBreakPoint;
End;

Procedure TForm1.ScrollToFetchCMD(Const i: Integer);
Var
  aLine: Integer;
Begin
  If fCMDs[PipeLine[i]].PipelineStep = psFetch Then Begin
    aLine := fCMDs[PipeLine[i]].Line - 1;
    If (aline < SynEdit1.TopLine) Or
      (aline > SynEdit1.BottomLine - 3) Then Begin
      SynEdit1.TopLine := max(0, aLine - SynEdit1.LinesInWindow + 5);
    End;
  End;
End;

Procedure TForm1.SetLinePipeLineState(aLine: integer; aStep: TPipelineStep);
Begin
  If (aLine >= 0) And (aLine <= High(fLineInfo)) Then
    fLineInfo[aLine].PipelineStep := aStep;
End;

Procedure TForm1.SyncMemoryToEngine;
Var
  addr, x, y: Integer;
Begin
  If Not Assigned(fEngine) Then exit;
  For addr := 100 To 119 Do Begin
    x := (addr - 100) Mod 5 + 1;
    y := (addr - 100) Div 5 + 1;
    fEngine.SetMemoryValue(addr, StrToIntDef(form2.Memory.Cells[x, y], 0));
  End;
End;

Procedure TForm1.SyncMemoryFromEngine;
Var
  addr, x, y: Integer;
Begin
  If Not Assigned(fEngine) Then exit;
  For addr := 100 To 119 Do Begin
    x := (addr - 100) Mod 5 + 1;
    y := (addr - 100) Div 5 + 1;
    form2.Memory.Cells[x, y] := IntToStr(fEngine.GetMemoryValue(addr));
  End;
End;

Procedure TForm1.SyncStackViewFromEngine;
Var
  i: Integer;
Begin
  form2.Stack.Items.BeginUpdate;
  Try
    form2.Stack.Items.Clear;
    If Assigned(fEngine) Then Begin
      For i := 0 To fEngine.StackCount - 1 Do Begin
        form2.Stack.Items.Add(inttostr(fEngine.GetStackValue(i)));
      End;
    End;
  Finally
    form2.Stack.Items.EndUpdate;
  End;
End;

Procedure TForm1.SyncStateFromEngine;
Var
  i, p, cmdIdx: Integer;
Begin
  If Not Assigned(fEngine) Then exit;

  fCMDs := fEngine.GetCommands;
  PipeLineDepth := fEngine.GetPipelineDepth;
  For i := 0 To 3 Do Begin
    PipeLine[i] := fEngine.GetPipelineSlot(i);
  End;

  For i := 0 To high(fLineInfo) Do Begin
    fLineInfo[i].PipelineStep := psNone;
  End;

  For p := 0 To PipeLineDepth - 1 Do Begin
    cmdIdx := PipeLine[p];
    If (cmdIdx >= 0) And (cmdIdx <= high(fCMDs)) Then Begin
      SetLinePipeLineState(fCMDs[cmdIdx].Line, fCMDs[cmdIdx].PipelineStep);
    End;
  End;

  AktualLine := -1;
  For p := 0 To PipeLineDepth - 1 Do Begin
    cmdIdx := PipeLine[p];
    If (cmdIdx >= 0) And (cmdIdx <= high(fCMDs)) And
      (fCMDs[cmdIdx].PipelineStep = psFetch) Then Begin
      AktualLine := fCMDs[cmdIdx].Line;
      break;
    End;
  End;
  If (AktualLine = -1) And (PipeLine[0] >= 0) And (PipeLine[0] <= high(fCMDs)) Then Begin
    AktualLine := fCMDs[PipeLine[0]].Line;
  End;

  form2.RegisterA.Text := inttostr(fEngine.RegA);
  form2.RegisterB.Text := inttostr(fEngine.RegB);
  form2.RegisterC.Text := inttostr(fEngine.RegC);
  form2.RegisterD.Text := inttostr(fEngine.RegD);
  form2.ALU_out_flag_zero.Checked := fEngine.FlagZero;
  form2.ALU_out_flag_carry.Checked := fEngine.FlagCarry;
  form2.ALU_out_flag_negative.Checked := fEngine.FlagNegative;
  form2.ALU_out_flag_overflow.Checked := false;
  SyncMemoryFromEngine;
  SyncStackViewFromEngine;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  i: Integer;
  cmds: TAssemblerCMDs;
Begin
  // Compile
  cmds := Compile(SynEdit1.Lines);
  If Not assigned(cmds) Then Begin
    ShowMessage('Error: ' + LastError);
    SynEdit1.Invalidate;
    exit;
  End;
  fCMDs := cmds;

  If Assigned(fEngine) Then FreeAndNil(fEngine);
  fEngine := TCPUEngine.Create;
  fEngine.LoadProgram(cmds, CheckBox5.Checked);
  SyncMemoryToEngine;

  If Not form2.visible Then form2.show;

  setlength(fLineInfo, SynEdit1.Lines.Count);
  For i := 0 To SynEdit1.Lines.Count - 1 Do Begin
    fLineInfo[i].isRunnable := false;
    fLineInfo[i].PipelineStep := psNone;
  End;
  For i := 0 To high(fCMDs) Do Begin
    If fCMDs[i].Cmd <> cLabel Then
      fLineInfo[fCMDs[i].Line].isRunnable := true;
  End;

  SyncStateFromEngine;
  SetLCLToExecute;
  RefreshVisualization;
  SynEdit1.SetFocus;
  SynEdit1.Invalidate;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Var
  halted: Boolean;
Begin
  If Not Assigned(fEngine) Then Begin
    ResetLCLToCompile;
    exit;
  End;

  halted := fEngine.Step;
  inc(aTick);
  SyncStateFromEngine;
  RefreshVisualization;

  If halted Then Begin
    If Timer1.Enabled Then Button3.Click;
    ShowMessage('Finished.');
    ResetLCLToCompile;
    exit;
  End;

  If (AktualLine >= 0) And HasBreakpoint(AktualLine) Then Begin
    If Timer1.Enabled Then Button3.Click;
  End;

  SynEdit1.SetFocus;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  If Timer1.Enabled Then Begin
    Timer1.Enabled := false;
    button3.caption := 'Auto step [ms]';
  End
  Else Begin
    timer1.interval := strtointdef(edit7.text, DefaultAutoStepTimeInMS);
    Timer1.Enabled := true;
    button3.caption := 'Stop';
  End;
  SynEdit1.SetFocus;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Begin
  // Halt
  ResetLCLToCompile;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Var
  m: TMemoryStream;
  i, j: Integer;
Begin
  If Not OpenDialog1.Execute Then exit;
  OpenDialog1.InitialDir := ExtractFilePath(OpenDialog1.FileName);
  SaveDialog1.InitialDir := ExtractFilePath(OpenDialog1.FileName);
  SaveDialog1.FileName := OpenDialog1.FileName;
  m := TMemoryStream.Create;
  m.LoadFromFile(OpenDialog1.FileName);
  // 1. Code
  SynEdit1.Text := m.ReadAnsiString;
  // 2. Memory
  For i := 1 To 5 Do
    For j := 1 To 4 Do
      form2.Memory.Cells[i, j] := m.ReadAnsiString;
  // 3. Stack
  form2.Stack.Items.Text := m.ReadAnsiString;
  m.free;
  SynEdit1.SetFocus;
  caption := defcaption + ' - ' + ExtractFileName(OpenDialog1.FileName);
End;

Procedure TForm1.Button6Click(Sender: TObject);
Var
  m: TMemoryStream;
  i, j: Integer;
Begin
  If Not SaveDialog1.Execute Then exit;
  OpenDialog1.InitialDir := ExtractFilePath(SaveDialog1.FileName);
  SaveDialog1.InitialDir := ExtractFilePath(SaveDialog1.FileName);
  OpenDialog1.FileName := SaveDialog1.FileName;
  m := TMemoryStream.Create;
  // 1. Code
  m.WriteAnsiString(SynEdit1.Text);
  // 2. Memory
  For i := 1 To 5 Do
    For j := 1 To 4 Do
      m.WriteAnsiString(form2.Memory.Cells[i, j]);
  // 3. Stack
  m.WriteAnsiString(form2.Stack.Items.Text);
  m.SaveToFile(SaveDialog1.FileName);
  m.free;
  SynEdit1.SetFocus;
  caption := defcaption + ' - ' + ExtractFileName(SaveDialog1.FileName);
End;

Procedure TForm1.ResetLCLToCompile;
Var
  i: Integer;
Begin
  LastErrorLine := -1;
  Button1.Enabled := true;
  Button2.Enabled := false;
  Button3.Enabled := false;
  Button4.Enabled := false;
  Button5.Enabled := true;
  edit7.enabled := false;
  form2.CU_Fetched_CMD.caption := '';
  form2.CU_Pipeline_State.caption := '';
  form2.RegisterA.text := '';
  form2.RegisterB.text := '';
  form2.RegisterC.text := '';
  form2.RegisterD.text := '';
  form2.Programcounter.text := '';
  SynEdit1.ReadOnly := false;
  ResetCMDVisualizations;
  For i := 0 To 3 Do Begin
    PipeLine[i] := -1;
  End;
  If Assigned(fEngine) Then FreeAndNil(fEngine);
  form2.ALU_out_flag_zero.Checked := false;
  form2.ALU_out_flag_carry.Checked := false;
  form2.ALU_out_flag_negative.Checked := false;
  form2.ALU_out_flag_overflow.Checked := false;
  If Timer1.Enabled Then button3.Click;
  form2.Actual_tick.caption := '';
  setlength(fCMDs, 0);
  // So überleben die Breakpoints ;)
  For i := 0 To high(fLineInfo) Do Begin
    fLineInfo[i].isRunnable := false;
    fLineInfo[i].PipelineStep := psNone;
  End;
  AktualLine := -1;
  aTick := 0;
  CheckBox5.Enabled := true;
  form2.Label16.Visible := false;
  form2.Label17.Visible := false;
  form2.Label18.Visible := false;
  form2.Label19.Visible := false;
  form2.Invalidate;
  Synedit1.Invalidate;
  If visible Then
    SynEdit1.SetFocus;
End;

Procedure TForm1.SetLCLToExecute;
Begin
  Button1.Enabled := false;
  Button2.Enabled := true;
  Button3.Enabled := true;
  Button4.Enabled := true;
  Button5.Enabled := false;
  edit7.enabled := true;
  SynEdit1.ReadOnly := true;
  CheckBox5.Enabled := false;
  aTick := 1;
  If CheckBox5.Checked Then Begin
    form2.Label16.Visible := true;
    form2.Label17.Visible := true;
    form2.Label18.Visible := true;
    form2.Label19.Visible := true;
  End;
End;

Procedure TForm1.ResetCMDVisualizations;
Begin
  form2.StackPointer.Font.Color := clblack;
  form2.StackPointer.Font.Style := [];
  form2.CU_Fetched_CMD.Font.Color := clblack;
  form2.CU_Fetched_CMD.Font.Style := [];
  form2.ALU_Operation.caption := '';
  form2.ALU_Operation.Font.Color := clblack;
  form2.ALU_Operation.Font.Style := [];
  form2.ALU_in_Left_OP.caption := '';
  form2.ALU_in_Left_OP.Font.Color := clblack;
  form2.ALU_in_Left_OP.Font.Style := [];
  form2.ALU_in_Right_OP.caption := '';
  form2.ALU_in_Right_OP.Font.Color := clblack;
  form2.ALU_in_Right_OP.Font.Style := [];
  form2.ALU_out_result.caption := '';
  form2.ALU_out_result.Font.Color := clblack;
  form2.ALU_out_result.Font.Style := [];
  form2.Const_in_For_ALU.caption := '';
  form2.Const_in_For_ALU.Font.Color := clblack;
  form2.Const_in_For_ALU.Font.Style := [];
  form2.CU_Target_ProgramCounter.caption := '';
  form2.CU_Target_ProgramCounter.Font.Color := clblack;
  form2.CU_Target_ProgramCounter.Font.Style := [];
  form2.RegisterA.Font.Color := clBlack;
  form2.RegisterA.Font.Style := [];
  form2.RegisterB.Font.Color := clBlack;
  form2.RegisterB.Font.Style := [];
  form2.RegisterC.Font.Color := clBlack;
  form2.RegisterC.Font.Style := [];
  form2.RegisterD.Font.Color := clBlack;
  form2.RegisterD.Font.Style := [];
  form2.Programcounter.Font.Color := clBlack;
  form2.Programcounter.Font.Style := [];
  form2.ALU_out_flag_zero.Font.Color := clBlack;
  form2.ALU_out_flag_zero.Font.Style := [];
  form2.ALU_out_flag_carry.Font.Color := clBlack;
  form2.ALU_out_flag_carry.Font.Style := [];
  form2.ALU_out_flag_negative.Font.Color := clBlack;
  form2.ALU_out_flag_negative.Font.Style := [];
  form2.ALU_out_flag_overflow.Font.Color := clBlack;
  form2.ALU_out_flag_overflow.Font.Style := [];
End;

Procedure TForm1.RefreshVisualization;
Var
  i: Integer;
  controlCmd: TAssemblerCMD;
  hasControlCmd: Boolean;
  stageRendered: Boolean;
Begin
  ResetCMDVisualizations;

  hasControlCmd := false;
  For i := 0 To PipeLineDepth - 1 Do Begin
    If (PipeLine[i] >= 0) And (PipeLine[i] <= high(fCMDs)) Then Begin
      If fCMDs[PipeLine[i]].PipelineStep = psFetch Then Begin
        controlCmd := fCMDs[PipeLine[i]];
        hasControlCmd := true;
      End;
      ScrollToFetchCMD(i);
    End;
  End;

  If CheckBox5.Checked Then Begin
    // Render all active pipeline stages each tick so WB visualization is never skipped.
    stageRendered := false;
    For i := 0 To PipeLineDepth - 1 Do
      If (PipeLine[i] >= 0) And (PipeLine[i] <= high(fCMDs)) And
         (fCMDs[PipeLine[i]].PipelineStep = psFetch) And (Not stageRendered) Then Begin
        form2.VisualizeCmdLCL(fCMDs[PipeLine[i]]);
        stageRendered := true;
      End;

    stageRendered := false;
    For i := 0 To PipeLineDepth - 1 Do
      If (PipeLine[i] >= 0) And (PipeLine[i] <= high(fCMDs)) And
         (fCMDs[PipeLine[i]].PipelineStep = psDecode) And (Not stageRendered) Then Begin
        form2.VisualizeCmdLCL(fCMDs[PipeLine[i]]);
        stageRendered := true;
      End;

    stageRendered := false;
    For i := 0 To PipeLineDepth - 1 Do
      If (PipeLine[i] >= 0) And (PipeLine[i] <= high(fCMDs)) And
         (fCMDs[PipeLine[i]].PipelineStep = psExecute) And (Not stageRendered) Then Begin
        form2.VisualizeCmdLCL(fCMDs[PipeLine[i]]);
        stageRendered := true;
      End;

    stageRendered := false;
    For i := 0 To PipeLineDepth - 1 Do
      If (PipeLine[i] >= 0) And (PipeLine[i] <= high(fCMDs)) And
         (fCMDs[PipeLine[i]].PipelineStep = psWriteBack) And (Not stageRendered) Then Begin
        form2.VisualizeCmdLCL(fCMDs[PipeLine[i]]);
        stageRendered := true;
      End;
  End
  Else Begin
    If (PipeLine[0] >= 0) And (PipeLine[0] <= high(fCMDs)) Then Begin
      controlCmd := fCMDs[PipeLine[0]];
      hasControlCmd := true;
      form2.VisualizeCmdLCL(controlCmd);
    End;
  End;

  If hasControlCmd Then Begin
    form2.CU_Fetched_CMD.Caption := CMDToStr(controlCmd.Cmd, controlCmd.LeftOperand, controlCmd.RightOperand);
    form2.CU_Pipeline_State.Caption := PipelineStepToStr(controlCmd.PipelineStep);
    form2.Programcounter.Text := inttostr(controlCmd.Line + 1);

    If CheckBox5.Checked Then Begin
      form2.CU_Fetched_CMD.Font.Color := PipeLineFetchBGColor;
      form2.CU_Pipeline_State.Font.Color := PipeLineFetchBGColor;
      form2.CU_Fetched_CMD.Font.Style := [fsBold];
    End;
  End
  Else Begin
    // In pipeline mode the control unit only shows Fetch. During pipeline drain it stays empty.
    form2.CU_Fetched_CMD.Caption := '';
    form2.CU_Pipeline_State.Caption := '';
    form2.Programcounter.Text := '';
  End;

  If aTick <> 0 Then Begin
    form2.Actual_tick.caption := format('Clock tick: %d', [aTick]);
  End;
  form2.Invalidate;
  SynEdit1.Invalidate;
End;

End.

