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

  TBranchPrediction = Record
    TrueTarget: Integer; // Index in FCMD's wenn der Jump gemacht wird
    FalseTarget: Integer; // Index in FCMD's wenn der Jump nicht gemacht werden soll
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
    Procedure MenuItem1Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure MenuItem3Click(Sender: TObject);
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
    fBranchPrediction: Array Of TBranchPrediction;
    fPendingBranchTarget: Integer;
    Function IsRunableLine(aLine: integer): Boolean;
    Function HasBreakpoint(aLine: integer): Boolean;
    Procedure ScrollToFetchCMD(Const i: Integer);
    Procedure SetLinePipeLineState(aLine: integer; aStep: TPipelineStep);
    Procedure ChangeCMDIndexTo(PipeLineIndex, aNewProgramCounter: Integer);
    Function OperandToInt(Const aOperand: String): Integer;
    Function IsBranchTaken(aCmdIndex: Integer): Boolean;
    Function ResolveRunnableCmdIndex(aCmdIndex: Integer): Integer;
    Procedure FlushPipelineFrom(aFromIndex: Integer);
    Procedure InjectBranchTarget(aBranchPipeLineIndex: Integer);
    Procedure EnsurePipelineHasFetch;
    Procedure CheckPipelineBreakpoint;
    Procedure Fetch(aPipelineIndex: Integer);
    Function WriteBack(aPipelineIndex: Integer): boolean;

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

Uses LCLType, Unit2, math;

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
  caption := 'FPC_CPU ver 0.01 by Corpsman, www.Corpsman.de';
  Edit7.text := inttostr(DefaultAutoStepTimeInMS);
End;

Procedure TForm1.FormShow(Sender: TObject);
Begin
  If FormShowOnce Then Begin
    FormShowOnce := false;
    ResetLCLToCompile;
    form2.Show;
  End;
End;

Procedure TForm1.MenuItem1Click(Sender: TObject);
Begin
  form2.ListBox1.Clear;
End;

Procedure TForm1.MenuItem2Click(Sender: TObject);
Var
  s: String;
Begin
  s := InputBox('Enter a value', 'Enter a integer value', '0');
  If (s <> '') And isnumber(s) Then Begin
    form2.ListBox1.Items.Insert(0, s);
  End;
End;

Procedure TForm1.MenuItem3Click(Sender: TObject);
Begin
  If form2.ListBox1.Items.Count <> 0 Then Begin
    form2.ListBox1.Items.Delete(0);
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

Procedure TForm1.ChangeCMDIndexTo(PipeLineIndex, aNewProgramCounter: Integer);
Begin
  If (PipeLine[PipeLineIndex] >= 0) And (PipeLine[PipeLineIndex] <= high(fCMDs)) Then
    SetLinePipeLineState(fCMDs[PipeLine[PipeLineIndex]].Line, psNone);
  PipeLine[PipeLineIndex] := aNewProgramCounter;
End;

Function TForm1.OperandToInt(Const aOperand: String): Integer;
Begin
  result := 0;
  Case aOperand Of
    'A': result := strtointdef(form2.Edit1.Text, 0);
    'B': result := strtointdef(form2.Edit2.Text, 0);
    'C': result := strtointdef(form2.Edit3.Text, 0);
    'D': result := strtointdef(form2.Edit4.Text, 0);
  Else
    result := strtointdef(aOperand, 0);
  End;
End;

Function TForm1.IsBranchTaken(aCmdIndex: Integer): Boolean;
Begin
  result := false;
  If (aCmdIndex < 0) Or (aCmdIndex > high(fCMDs)) Then exit;
  Case fCMDs[aCmdIndex].Cmd Of
    cJMP: result := true;
    cJZ: result := form2.CheckBox1.Checked;
    cJNZ: result := Not form2.CheckBox1.Checked;
  End;
End;

Function TForm1.ResolveRunnableCmdIndex(aCmdIndex: Integer): Integer;
Begin
  result := aCmdIndex;
  If (result < 0) Or (result > high(fCMDs)) Then Begin
    result := -1;
    exit;
  End;
  While (result <= high(fCMDs)) And (fCMDs[result].Cmd = cLabel) Do Begin
    inc(result);
  End;
  If result > high(fCMDs) Then
    result := -1;
End;

Procedure TForm1.FlushPipelineFrom(aFromIndex: Integer);
Var
  i: Integer;
Begin
  For i := aFromIndex To PipeLineDepth - 1 Do Begin
    If (PipeLine[i] >= 0) And (PipeLine[i] <= high(fCMDs)) Then Begin
      fCMDs[PipeLine[i]].PipelineStep := psNone;
      SetLinePipeLineState(fCMDs[PipeLine[i]].Line, psNone);
    End;
    PipeLine[i] := -1;
  End;
End;

Procedure TForm1.InjectBranchTarget(aBranchPipeLineIndex: Integer);
Var
  target, insertIndex: Integer;
Begin
  insertIndex := aBranchPipeLineIndex + 1;
  If insertIndex >= PipeLineDepth Then exit;

  target := ResolveRunnableCmdIndex(fBranchPrediction[PipeLine[aBranchPipeLineIndex]].TrueTarget);
  If (target < 0) Or (target > high(fCMDs)) Then exit;

  PipeLine[insertIndex] := target;
  fCMDs[target].PipelineStep := psFetch;
  SetLinePipeLineState(fCMDs[target].Line, psFetch);
End;

Procedure TForm1.EnsurePipelineHasFetch;
Var
  i: Integer;
  highestUsed: Integer;
  hasFetch: Boolean;
  nextIndex: Integer;
Begin
  If Not CheckBox5.Checked Then exit;

  highestUsed := -1;
  hasFetch := false;
  For i := 0 To PipeLineDepth - 1 Do Begin
    If (PipeLine[i] >= 0) And (PipeLine[i] <= high(fCMDs)) Then Begin
      highestUsed := i;
      If fCMDs[PipeLine[i]].PipelineStep = psFetch Then
        hasFetch := true;
    End;
  End;

  If hasFetch Then exit;
  If (highestUsed = -1) Or (highestUsed >= PipeLineDepth - 1) Then exit;

  nextIndex := FindNextValidProgramLine(PipeLine[highestUsed]);
  If nextIndex > high(fCMDs) Then exit;

  PipeLine[highestUsed + 1] := nextIndex;
  fCMDs[nextIndex].PipelineStep := psFetch;
  SetLinePipeLineState(fCMDs[nextIndex].Line, psFetch);
End;

Procedure TForm1.CheckPipelineBreakpoint;
Var
  i: Integer;
Begin
  If Not CheckBox5.Checked Then exit;

  For i := 0 To PipeLineDepth - 1 Do Begin
    If (PipeLine[i] >= 0) And (PipeLine[i] <= high(fCMDs)) And
      (fCMDs[PipeLine[i]].PipelineStep = psFetch) Then Begin
      AktualLine := fCMDs[PipeLine[i]].Line;
      If HasBreakpoint(AktualLine) Then Begin
        If Timer1.Enabled Then Button3.Click;
      End;
      exit;
    End;
  End;
End;

Procedure TForm1.Fetch(aPipelineIndex: Integer);
Begin
  fcmds[PipeLine[aPipelineIndex]].PipelineStep := psDecode;
  If CheckBox5.Checked Then Begin
    // Fill the pipeline ;)
    If aPipelineIndex <> 3 Then Begin
      ChangeCMDIndexTo(aPipelineIndex + 1, FindNextValidProgramLine(PipeLine[aPipelineIndex]));
      If PipeLine[aPipelineIndex + 1] <= high(fCMDs) Then Begin
        fcmds[PipeLine[aPipelineIndex + 1]].PipelineStep := psFetch;
        SetLinePipeLineState(fcmds[PipeLine[aPipelineIndex + 1]].Line, fcmds[PipeLine[aPipelineIndex + 1]].PipelineStep);
      End
      Else Begin
        // There are no more commands to be executed
        PipeLine[aPipelineIndex + 1] := -1;
      End;
    End;
  End;
End;

Function TForm1.WriteBack(aPipelineIndex: Integer): boolean;
Var
  x, y: Integer;
  el: TEdit;
  vLeft, vRight: Integer;
  FromRet: Boolean;
Begin
  result := false;
  FromRet := false;
  If PipeLine[aPipelineIndex] = -1 Then exit;
  Case fcmds[PipeLine[aPipelineIndex]].Cmd Of
    cHLT: Begin
        If Timer1.Enabled Then Button3.Click; // Stop Autoclicker before Message box!
        showmessage('Finished.');
        ResetLCLToCompile;
        result := true;
        exit;
      End;
    cRET: Begin
        // In Pipeline mode, cRET is already handled in Execute phase
        If Not CheckBox5.Checked Then Begin
          If form2.ListBox1.Items.Count <> 0 Then Begin
            x := convertCodeLineToCMDIndex(strtoint(form2.ListBox1.Items[0]) - 1);
            form2.ListBox1.Items.Delete(0);
          End
          Else Begin
            x := -1; // Stack ist Empty -> Ungültig
          End;
          ChangeCMDIndexTo(aPipelineIndex, FindNextValidProgramLine(x, true));
          FromRet := true;
        End;
      End;
    cCALL: Begin
        ChangeCMDIndexTo(aPipelineIndex, fBranchPrediction[PipeLine[aPipelineIndex]].TrueTarget);
      End;
    cJMP: Begin
        If Not CheckBox5.Checked Then Begin
          ChangeCMDIndexTo(aPipelineIndex, fBranchPrediction[PipeLine[aPipelineIndex]].TrueTarget);
        End;
      End;
    cJZ: Begin
        If form2.CheckBox1.Checked And (Not CheckBox5.Checked) Then Begin
          ChangeCMDIndexTo(aPipelineIndex, fBranchPrediction[PipeLine[aPipelineIndex]].TrueTarget);
        End;
      End;
    cJNZ: Begin
        If (Not form2.CheckBox1.Checked) And (Not CheckBox5.Checked) Then Begin
          ChangeCMDIndexTo(aPipelineIndex, fBranchPrediction[PipeLine[aPipelineIndex]].TrueTarget);
        End;
      End;
    cADD, cAnd, cDIV, cNot, cMUL, cOr, cSHL, cSHR, cSub, cXOR: Begin
        el := form2.OperandToEdit(fcmds[PipeLine[aPipelineIndex]].LeftOperand);
        vLeft := OperandToInt(fcmds[PipeLine[aPipelineIndex]].LeftOperand);
        vRight := OperandToInt(fcmds[PipeLine[aPipelineIndex]].RightOperand);
        Case fcmds[PipeLine[aPipelineIndex]].Cmd Of
          cADD: el.Text := inttostr(vLeft + vRight);
          cAnd: el.Text := inttostr(vLeft And vRight);
          cDIV: el.Text := inttostr(vLeft Div vRight);
          cNot: el.Text := inttostr(Not vLeft);
          cMul: el.Text := inttostr(vLeft * vRight);
          cOr: el.Text := inttostr(vLeft Or vRight);
          cSHL: el.Text := inttostr(vLeft Shl vRight);
          cSHR: el.Text := inttostr(vLeft Shr vRight);
          cSub: el.Text := inttostr(vLeft - vRight);
          cXOR: el.Text := inttostr(vLeft Xor vRight);
        End;
      End;
    cSTORE: Begin
        // Decode right Operand to x,y in Stringgrid
        x := (strtoint(fcmds[PipeLine[aPipelineIndex]].RightOperand) - 100) Mod 5 + 1;
        y := (strtoint(fcmds[PipeLine[aPipelineIndex]].RightOperand) - 100) Div 5 + 1;
        el := Nil;
        Case fcmds[PipeLine[aPipelineIndex]].LeftOperand Of
          'A': el := form2.Edit1;
          'B': el := form2.Edit2;
          'C': el := form2.Edit3;
          'D': el := form2.Edit4;
        End;
        form2.StringGrid1.Cells[x, y] := inttostr(strtointdef(el.text, 0));
      End;
  End;
  (*
   * Here comes the logic to "switch" to the next command, this is usually
   * aCMDIndex + 1, except on J* commands.
   *)
  SetLinePipeLineState(fcmds[PipeLine[aPipelineIndex]].Line, psNone);
  If Not CheckBox5.Checked Then Begin
    If Not FromRet Then
      ChangeCMDIndexTo(aPipelineIndex, FindNextValidProgramLine(PipeLine[aPipelineIndex]));
    fcmds[PipeLine[aPipelineIndex]].PipelineStep := psFetch;
    // If the user sets a break point, this is the moment, where we stop the automation ;)
    AktualLine := fcmds[PipeLine[aPipelineIndex]].Line;
    If HasBreakpoint(AktualLine) Then Begin
      If Timer1.Enabled Then button3.click;
    End;
  End;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  i: Integer;
Begin
  // Compile
  fCMDs := Compile(SynEdit1.Lines);
  If Not assigned(fCMDs) Then Begin
    ShowMessage('Error: ' + LastError);
    SynEdit1.Invalidate;
    exit;
  End;
  If Not form2.visible Then form2.show;
  // Calculate all Jump Targets
  setlength(fBranchPrediction, length(fCMDs));
  For i := 0 To high(fCMDs) Do Begin
    fBranchPrediction[i].TrueTarget := -1;
    fBranchPrediction[i].FalseTarget := -1;
    If fCMDs[i].Cmd In [cJMP, cJNZ, cJZ, cCALL] Then Begin
      fBranchPrediction[i].TrueTarget := convertCodeLineToCMDIndex(fCMDs[i].JumpTarget);
      fBranchPrediction[i].FalseTarget := i;
    End;
  End;
  fPendingBranchTarget := -1;
  setlength(fLineInfo, SynEdit1.Lines.Count);
  For i := 0 To SynEdit1.Lines.Count - 1 Do Begin
    fLineInfo[i].isRunnable := false;
    fLineInfo[i].PipelineStep := psNone;
  End;
  For i := 0 To high(fCMDs) Do Begin
    If fCMDs[i].Cmd <> cLabel Then
      fLineInfo[fCMDs[i].Line].isRunnable := true;
  End;
  For i := 0 To 3 Do
    ChangeCMDIndexTo(i, -1);
  If CheckBox5.Checked Then Begin
    PipeLineDepth := 4;
  End
  Else Begin
    PipeLineDepth := 1;
  End;
  ChangeCMDIndexTo(0, 0);
  AktualLine := fCMDs[PipeLine[0]].Line;
  SetLinePipeLineState(AktualLine, psFetch);
  SetLCLToExecute;
  RefreshVisualization;
  ScrollToFetchCMD(0);
  SynEdit1.SetFocus;
  SynEdit1.Invalidate;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Var
  i, p: Integer;
  vLeft, vRight: Integer;
  branchTarget: Integer;
Begin
  // Step
  If (PipeLine[0] < 0) Or (PipeLine[0] > high(fCMDs)) Then Begin
    ResetLCLToCompile;
    exit;
  End;
  For p := PipeLineDepth - 1 Downto 0 Do Begin
    If PipeLine[p] = -1 Then Continue;
    Case fcmds[PipeLine[p]].PipelineStep Of
      psFetch: Fetch(p);
      psDecode: fcmds[PipeLine[p]].PipelineStep := psExecute;
      psExecute: Begin
          If fcmds[PipeLine[p]].Cmd = cCMP Then Begin
            vLeft := OperandToInt(fcmds[PipeLine[p]].LeftOperand);
            vRight := OperandToInt(fcmds[PipeLine[p]].RightOperand);
            form2.CheckBox1.Checked := vLeft = vRight;
            form2.CheckBox2.Checked := vLeft < vRight;
            form2.CheckBox3.Checked := false;
            form2.CheckBox4.Checked := false;
          End;
          If CheckBox5.Checked And (fcmds[PipeLine[p]].Cmd In [cJMP, cJZ, cJNZ]) Then Begin
            If IsBranchTaken(PipeLine[p]) Then Begin
              FlushPipelineFrom(p + 1);
              branchTarget := ResolveRunnableCmdIndex(fBranchPrediction[PipeLine[p]].TrueTarget);
              If (branchTarget >= 0) And (branchTarget <= high(fCMDs)) Then Begin
                // Fast path: if there is a younger slot, refill it immediately.
                If p < PipeLineDepth - 1 Then Begin
                  PipeLine[p + 1] := branchTarget;
                  fcmds[branchTarget].PipelineStep := psFetch;
                  SetLinePipeLineState(fcmds[branchTarget].Line, psFetch);
                  fPendingBranchTarget := -1;
                End
                Else Begin
                  // Fallback: branch is in the last slot, inject after next shift.
                  fPendingBranchTarget := branchTarget;
                End;
              End;
            End;
          End;
          If CheckBox5.Checked And (fcmds[PipeLine[p]].Cmd = cRET) Then Begin
            FlushPipelineFrom(p + 1);
            // Get RET target from stack (ListBox1)
            If form2.ListBox1.Items.Count <> 0 Then Begin
              branchTarget := convertCodeLineToCMDIndex(strtoint(form2.ListBox1.Items[0]) - 1);
              form2.ListBox1.Items.Delete(0);
              // Use FindNextValidProgramLine with IgnoreCalls=true, same as in non-pipeline mode
              branchTarget := FindNextValidProgramLine(branchTarget, true);
              If (branchTarget >= 0) And (branchTarget <= high(fCMDs)) Then Begin
                If p < PipeLineDepth - 1 Then Begin
                  PipeLine[p + 1] := branchTarget;
                  fcmds[branchTarget].PipelineStep := psFetch;
                  SetLinePipeLineState(fcmds[branchTarget].Line, psFetch);
                  fPendingBranchTarget := -1;
                End
                Else Begin
                  fPendingBranchTarget := branchTarget;
                End;
              End;
            End;
          End;
          If CheckBox5.Checked And (fcmds[PipeLine[p]].Cmd = cHLT) Then Begin
            // Stop fetching new instructions after HLT
            FlushPipelineFrom(p + 1);
          End;
          fcmds[PipeLine[p]].PipelineStep := psWriteBack;
        End;
      psWriteBack: Begin
          If WriteBack(p) Then exit;
          If CheckBox5.Checked And (p = 0) Then Begin
            For i := 0 To 2 Do Begin
              pipeline[i] := pipeline[i + 1];
            End;
            pipeline[3] := -1;
            If (fPendingBranchTarget >= 0) And (fPendingBranchTarget <= high(fCMDs)) Then Begin
              PipeLine[PipeLineDepth - 1] := fPendingBranchTarget;
              fcmds[fPendingBranchTarget].PipelineStep := psFetch;
              SetLinePipeLineState(fcmds[fPendingBranchTarget].Line, psFetch);
              fPendingBranchTarget := -1;
            End;
            EnsurePipelineHasFetch;
          End;
        End;
    End;
    SetLinePipeLineState(fcmds[PipeLine[p]].Line, fcmds[PipeLine[p]].PipelineStep);
  End;
  CheckPipelineBreakpoint;
  ResetCMDVisualizations;
  inc(aTick);
  RefreshVisualization;
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
  m := TMemoryStream.Create;
  m.LoadFromFile(OpenDialog1.FileName);
  // 1. Code
  SynEdit1.Text := m.ReadAnsiString;
  // 2. Memory
  For i := 1 To 5 Do
    For j := 1 To 4 Do
      form2.StringGrid1.Cells[i, j] := m.ReadAnsiString;
  // 3. Stack
  form2.ListBox1.Items.Text := m.ReadAnsiString;
  m.free;
  SynEdit1.SetFocus;
End;

Procedure TForm1.Button6Click(Sender: TObject);
Var
  m: TMemoryStream;
  i, j: Integer;
Begin
  If Not SaveDialog1.Execute Then exit;
  m := TMemoryStream.Create;
  // 1. Code
  m.WriteAnsiString(SynEdit1.Text);
  // 2. Memory
  For i := 1 To 5 Do
    For j := 1 To 4 Do
      m.WriteAnsiString(form2.StringGrid1.Cells[i, j]);
  // 3. Stack
  m.WriteAnsiString(form2.ListBox1.Items.Text);
  m.SaveToFile(SaveDialog1.FileName);
  m.free;
  SynEdit1.SetFocus;
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
  Button6.Enabled := true;
  edit7.enabled := false;
  form2.Label7.caption := '';
  form2.Label8.caption := '';
  form2.Edit1.text := '';
  form2.Edit2.text := '';
  form2.Edit3.text := '';
  form2.Edit4.text := '';
  form2.Edit6.text := '';
  SynEdit1.ReadOnly := false;
  ResetCMDVisualizations;
  For i := 0 To 3 Do Begin
    PipeLine[i] := -1;
  End;
  form2.CheckBox1.Checked := false;
  form2.CheckBox2.Checked := false;
  form2.CheckBox3.Checked := false;
  form2.CheckBox4.Checked := false;
  If Timer1.Enabled Then button3.Click;
  form2.label14.caption := '';
  setlength(fCMDs, 0);
  // So überleben die Breakpoints ;)
  For i := 0 To high(fLineInfo) Do Begin
    fLineInfo[i].isRunnable := false;
    fLineInfo[i].PipelineStep := psNone;
  End;
  AktualLine := -1;
  aTick := 0;
  fPendingBranchTarget := -1;
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
  Button6.Enabled := false;
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
  form2.label5.Font.Color := clblack;
  form2.label5.Font.Style := [];
  form2.label7.Font.Color := clblack;
  form2.label7.Font.Style := [];
  form2.label9.caption := '';
  form2.label9.Font.Color := clblack;
  form2.label9.Font.Style := [];
  form2.label10.caption := '';
  form2.label10.Font.Color := clblack;
  form2.label10.Font.Style := [];
  form2.label11.caption := '';
  form2.label11.Font.Color := clblack;
  form2.label11.Font.Style := [];
  form2.label12.caption := '';
  form2.label12.Font.Color := clblack;
  form2.label12.Font.Style := [];
  form2.label13.caption := '';
  form2.label13.Font.Color := clblack;
  form2.label13.Font.Style := [];
  form2.label15.caption := '';
  form2.label15.Font.Color := clblack;
  form2.label15.Font.Style := [];
  form2.edit1.Font.Color := clBlack;
  form2.edit1.Font.Style := [];
  form2.edit2.Font.Color := clBlack;
  form2.edit2.Font.Style := [];
  form2.edit3.Font.Color := clBlack;
  form2.edit3.Font.Style := [];
  form2.edit4.Font.Color := clBlack;
  form2.edit4.Font.Style := [];
  form2.edit6.Font.Color := clBlack;
  form2.edit6.Font.Style := [];
  form2.CheckBox1.Font.Color := clBlack;
  form2.CheckBox1.Font.Style := [];
  form2.CheckBox2.Font.Color := clBlack;
  form2.CheckBox2.Font.Style := [];
  form2.CheckBox3.Font.Color := clBlack;
  form2.CheckBox3.Font.Style := [];
  form2.CheckBox4.Font.Color := clBlack;
  form2.CheckBox4.Font.Style := [];
End;

Procedure TForm1.RefreshVisualization;
Var
  i: Integer;
Begin
  For i := 0 To PipeLineDepth - 1 Do Begin
    If (PipeLine[i] >= 0) And (PipeLine[i] <= high(fCMDs)) Then Begin
      form2.VisualizeCmdLCL(fCMDs[PipeLine[i]]);
      // Sicherstellen, dass der "Fetch" Sichtbar ist
      ScrollToFetchCMD(i);
    End;
  End;
  If aTick <> 0 Then Begin
    form2.label14.caption := format('Clock tick: %d', [aTick]);
  End;
  form2.Invalidate;
  SynEdit1.Invalidate;
End;

End.

