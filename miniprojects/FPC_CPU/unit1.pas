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
  ExtCtrls, SynEdit, SynHighlighterAny, uFPC_CPU, SynEditMarks;

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
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    DebugMarks: TImageList;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    GroupBox1: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ListBox1: TListBox;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    StringGrid1: TStringGrid;
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
    Procedure FormPaint(Sender: TObject);
    Procedure SynEdit1GutterClick(Sender: TObject; X, Y, Line: integer;
      mark: TSynEditMark);
    Procedure SynEdit1Paint(Sender: TObject; ACanvas: TCanvas);
    Procedure SynEdit1SpecialLineColors(Sender: TObject; Line: integer;
      Var Special: boolean; Var FG, BG: TColor);
  private
    fCMDs: TAssemblerCMDs;
    aTick: integer;
    PipeLine: Array[0..3] Of Integer;
    PipeLineDepth: Integer;
    fLineInfo: Array Of TLineInfo;
    AktualLine: Integer;
    fBranchPrediction: Array Of TBranchPrediction;
    fPendingBranchTarget: Integer;
    Function FindNextValidProgramLine(aLine: integer): integer;
    Function IsRunableLine(aLine: integer): Boolean;
    Function HasBreakpoint(aLine: integer): Boolean;
    Procedure SetLinePipeLineState(aLine: integer; aStep: TPipelineStep);
    Procedure ChangeCMDIndexTo(PipeLineIndex, aNewProgramCounter: Integer);
    Function OperandToInt(Const aOperand: String): Integer;
    Function IsBranchTaken(aCmdIndex: Integer): Boolean;
    Function ResolveRunnableCmdIndex(aCmdIndex: Integer): Integer;
    Procedure FlushPipelineFrom(aFromIndex: Integer);
    Procedure InjectBranchTarget(aBranchPipeLineIndex: Integer);
    Procedure EnsurePipelineHasFetch;
    Procedure Fetch(aPipelineIndex: Integer);
    Function WriteBack(aPipelineIndex: Integer): boolean;
  public
    Procedure ResetLCLToCompile;
    Procedure SetLCLToExecute;
    Procedure ResetCMDVisualizations;
    Procedure VisualizeCmdLCL(Const aCMD: TAssemblerCMD);
    Procedure VisualizeCmdDraw(Const aCMD: TAssemblerCMD);
    Procedure RefreshVisualization;
  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Const
  IndexBreakPoint = 0;
  IndexIsRunnable = 1;
  IndexAktualLine = 2;
  IndexNotDebugableLine = 3;

  PipeLineFetchBGColor = clred;
  PipeLineDecodeBGColor = clGreen;
  PipeLineExecuteBGColor = clBlue;
  PipeLineExecuteFGColor = clWhite;
  PipeLineWritebackBGColor = clolive;

  DefaultAutoStepTimeInMS = 100;

  { TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Var
  i: Integer;
Begin
  (*
   * TODO:
   *  - STACK, via Push Pop
   *  - Subfunctions via CALL ( und seinem Gegenstück ?)
   *  - Die Flags Sinnvoll auswerten / Benutzen (da fehlen noch entsprechende Jump Befehle)
   *  - Branch Prediction unit!
   *)

  caption := 'FPC_CPU ver 0.01 by Corpsman, www.Corpsman.de';
  StringGrid1.Cells[0, 0] := 'Memory';
  For i := 1 To 5 Do Begin
    StringGrid1.Cells[i, 0] := inttostr(i - 1);
  End;
  For i := 1 To 4 Do Begin
    StringGrid1.Cells[0, i] := inttostr((i - 1) * 5 + 100);
  End;
  Edit7.text := inttostr(DefaultAutoStepTimeInMS);
  ResetLCLToCompile;
  //StringGrid1.Cells[1, 1] := '5'; // Debug remove
  (*
   * Default "Simple" demo
   * Mem[102] := mem[100] + mem[101];
   * )
  StringGrid1.Cells[1, 1] := '20';
  StringGrid1.Cells[2, 1] := '22';
  SynEdit1.Clear;
  SynEdit1.Lines.Add('; Adds the 2 values stored in');
  SynEdit1.Lines.Add('; memory on adress 100 and 101');
  SynEdit1.Lines.Add('; and stores them into memory');
  SynEdit1.Lines.Add('; adress 102');
  SynEdit1.Lines.Add('');
  SynEdit1.Lines.Add('LOAD A, 100');
  SynEdit1.Lines.Add('LOAD B, 101');
  SynEdit1.Lines.Add('ADD A, B');
  SynEdit1.Lines.Add('STORE A, 102');
  SynEdit1.Lines.Add('HLT');
  // *)
  label16.font.Color := PipeLineFetchBGColor;
  label17.font.Color := PipeLineDecodeBGColor;
  label18.font.Color := PipeLineExecuteBGColor;
  label19.font.Color := PipeLineWritebackBGColor;
End;

Procedure TForm1.FormPaint(Sender: TObject);
Var
  i: Integer;
Begin
  For i := 0 To PipeLineDepth - 1 Do Begin
    If (PipeLine[i] >= 0) And (PipeLine[i] <= high(fCMDs)) Then Begin
      VisualizeCmdDraw(fCMDs[PipeLine[i]]);
    End;
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
    j := high(fLineInfo);
    setlength(fLineInfo, Line);
    For i := j To high(fLineInfo) Do Begin
      fLineInfo[i].isRunnable := false;
      fLineInfo[i].hasBreakPoint := false;
    End;
  End;
  fLineInfo[Line].hasBreakPoint := Not fLineInfo[Line].hasBreakPoint;
  SynEdit1.Invalidate;
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
      End;
    psDecode: Begin
        Special := true;
        BG := PipeLineDecodeBGColor;
      End;
    psExecute: Begin
        Special := true;
        BG := PipeLineExecuteBGColor;
        FG := PipeLineExecuteFGColor;
      End;
    psWriteBack: Begin
        Special := true;
        BG := PipeLineWritebackBGColor;
      End;
  End;
End;

Function TForm1.FindNextValidProgramLine(aLine: integer): integer;
Var
  i: Integer;
Begin
  If (aLine < 0) Or (aLine > high(fCMDs)) Then Begin
    result := high(fCMDs) + 1;
    exit;
  End;

  result := aLine + 1;
  If result > high(fCMDs) Then exit;

  // This is a kind of Branch Prediction :)
  If fCMDs[aLine].Cmd = cJMP Then Begin
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
    'A': result := strtointdef(Edit1.Text, 0);
    'B': result := strtointdef(Edit2.Text, 0);
    'C': result := strtointdef(Edit3.Text, 0);
    'D': result := strtointdef(Edit4.Text, 0);
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
    cJZ: result := CheckBox1.Checked;
    cJNZ: result := Not CheckBox1.Checked;
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
Begin
  result := false;
  If PipeLine[aPipelineIndex] = -1 Then exit;
  Case fcmds[PipeLine[aPipelineIndex]].Cmd Of
    cHLT: Begin
        If Timer1.Enabled Then Button3.Click; // Stop Autoclicker before Message box!
        showmessage('Finished.');
        ResetLCLToCompile;
        result := true;
        exit;
      End;
    cJMP: Begin
        If Not CheckBox5.Checked Then Begin
          ChangeCMDIndexTo(aPipelineIndex, fBranchPrediction[PipeLine[aPipelineIndex]].TrueTarget);
        End;
      End;
    cJZ: Begin
        If CheckBox1.Checked And (Not CheckBox5.Checked) Then Begin
          ChangeCMDIndexTo(aPipelineIndex, fBranchPrediction[PipeLine[aPipelineIndex]].TrueTarget);
        End;
      End;
    cJNZ: Begin
        If (Not CheckBox1.Checked) And (Not CheckBox5.Checked) Then Begin
          ChangeCMDIndexTo(aPipelineIndex, fBranchPrediction[PipeLine[aPipelineIndex]].TrueTarget);
        End;
      End;
    cADD, cAnd, cNot, cOr, cSHL, cSHR, cSub, cXOR: Begin
        el := Nil;
        Case fcmds[PipeLine[aPipelineIndex]].LeftOperand Of
          'A': el := Edit1;
          'B': el := Edit2;
          'C': el := Edit3;
          'D': el := Edit4;
        End;
        vLeft := OperandToInt(fcmds[PipeLine[aPipelineIndex]].LeftOperand);
        vRight := OperandToInt(fcmds[PipeLine[aPipelineIndex]].RightOperand);
        Case fcmds[PipeLine[aPipelineIndex]].Cmd Of
          cADD: el.Text := inttostr(vLeft + vRight);
          cAnd: el.Text := inttostr(vLeft And vRight);
          cNot: el.Text := inttostr(Not vLeft);
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
          'A': el := Edit1;
          'B': el := Edit2;
          'C': el := Edit3;
          'D': el := Edit4;
        End;
        StringGrid1.Cells[x, y] := inttostr(strtointdef(el.text, 0));
      End;
  End;
  (*
   * Here comes the logic to "switch" to the next command, this is usually
   * aCMDIndex + 1, except on J* commands.
   *)
  SetLinePipeLineState(fcmds[PipeLine[aPipelineIndex]].Line, psNone);
  If Not CheckBox5.Checked Then Begin
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
  // Calculate all Jump Targets
  setlength(fBranchPrediction, length(fCMDs));
  For i := 0 To high(fCMDs) Do Begin
    fBranchPrediction[i].TrueTarget := -1;
    fBranchPrediction[i].FalseTarget := -1;
    If fCMDs[i].Cmd In [cJMP, cJNZ, cJZ] Then Begin
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
            CheckBox1.Checked := vLeft = vRight;
            CheckBox2.Checked := vLeft < vRight;
            CheckBox3.Checked := false;
            CheckBox4.Checked := false;
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
  ResetCMDVisualizations;
  inc(aTick);
  RefreshVisualization;
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
      StringGrid1.Cells[i, j] := m.ReadAnsiString;
  // 3. Stack
  i := 0;
  m.Read(i, sizeof(i));
  If i <> 0 Then Begin
    // TODO: -- Implement Stack load Routine
  End
  Else Begin
    edit5.text := '';
    ListBox1.Clear;
  End;
  m.free;
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
      m.WriteAnsiString(StringGrid1.Cells[i, j]);
  // 3. Stack
  // TODO: -- Implement Stack Save Routine
  i := 0;
  m.write(i, sizeof(i));
  m.SaveToFile(SaveDialog1.FileName);
  m.free;
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
  Label7.caption := '';
  Label8.caption := '';
  Edit1.text := '';
  Edit2.text := '';
  Edit3.text := '';
  Edit4.text := '';
  Edit5.text := '';
  Edit6.text := '';
  SynEdit1.ReadOnly := false;
  ResetCMDVisualizations;
  For i := 0 To 3 Do Begin
    PipeLine[i] := -1;
  End;
  CheckBox1.Checked := false;
  CheckBox2.Checked := false;
  CheckBox3.Checked := false;
  CheckBox4.Checked := false;
  If Timer1.Enabled Then button3.Click;
  label14.caption := '';
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
  Label16.Visible := false;
  Label17.Visible := false;
  Label18.Visible := false;
  Label19.Visible := false;
  Refresh;
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
    Label16.Visible := true;
    Label17.Visible := true;
    Label18.Visible := true;
    Label19.Visible := true;
  End;
End;

Procedure TForm1.ResetCMDVisualizations;
Begin
  label7.Font.Color := clblack;
  label7.Font.Style := [];
  label9.caption := '';
  label9.Font.Color := clblack;
  label9.Font.Style := [];
  label10.caption := '';
  label10.Font.Color := clblack;
  label10.Font.Style := [];
  label11.caption := '';
  label11.Font.Color := clblack;
  label11.Font.Style := [];
  label12.caption := '';
  label12.Font.Color := clblack;
  label12.Font.Style := [];
  label13.caption := '';
  label13.Font.Color := clblack;
  label13.Font.Style := [];
  label15.caption := '';
  label15.Font.Color := clblack;
  label15.Font.Style := [];
  edit1.Font.Color := clBlack;
  edit1.Font.Style := [];
  edit2.Font.Color := clBlack;
  edit2.Font.Style := [];
  edit3.Font.Color := clBlack;
  edit3.Font.Style := [];
  edit4.Font.Color := clBlack;
  edit4.Font.Style := [];
  edit6.Font.Color := clBlack;
  edit6.Font.Style := [];
  CheckBox1.Font.Color := clBlack;
  CheckBox1.Font.Style := [];
  CheckBox2.Font.Color := clBlack;
  CheckBox2.Font.Style := [];
  CheckBox3.Font.Color := clBlack;
  CheckBox3.Font.Style := [];
  CheckBox4.Font.Color := clBlack;
  CheckBox4.Font.Style := [];
End;

Procedure TForm1.RefreshVisualization;
Var
  i: Integer;
Begin
  For i := 0 To PipeLineDepth - 1 Do Begin
    If (PipeLine[i] >= 0) And (PipeLine[i] <= high(fCMDs)) Then Begin
      VisualizeCmdLCL(fCMDs[PipeLine[i]]);
    End;
  End;
  If aTick <> 0 Then Begin
    label14.caption := format('Clock tick: %d', [aTick]);
  End;
  Invalidate;
End;

Procedure TForm1.VisualizeCmdLCL(Const aCMD: TAssemblerCMD);
Var
  x, y: integer;
  el, er: TEdit;
  aColor: TColor;
Begin
  Edit6.text := inttostr(aCMD.Line + 1);
  If Not CheckBox5.Checked Then Begin
    label7.caption := CMDToStr(aCMD.Cmd, aCMD.LeftOperand, aCMD.RightOperand);
    label8.caption := PipelineStepToStr(aCMD.PipelineStep);
  End
  Else Begin
    label7.caption := '';
    label8.caption := '';
  End;
  Case aCMD.PipelineStep Of
    psFetch: Begin
        If CheckBox5.Checked Then Begin
          label7.caption := CMDToStr(aCMD.Cmd, aCMD.LeftOperand, aCMD.RightOperand);
          label8.caption := PipelineStepToStr(aCMD.PipelineStep);
        End;
        aColor := PipeLineFetchBGColor;
        label8.Font.Color := aColor;
        label7.Font.Color := aColor;
        label7.Font.Style := [fsBold];
      End;
    psDecode: Begin
        aColor := PipeLineDecodeBGColor;
        label8.Font.Color := aColor;
        Case aCMD.Cmd Of
          cJMP, cJZ, cJNZ: Begin
              If (aCMD.Cmd = cJMP)
                Or ((aCMD.Cmd = cJZ) And CheckBox1.Checked)
                Or ((aCMD.Cmd = cJNZ) And (Not CheckBox1.Checked))
                Then Begin
                label15.Caption := inttostr(FindNextValidProgramLine(aCMD.JumpTarget) + 1);
                label15.Font.Color := aColor;
                label15.Font.Style := [fsBold];
              End;
              If (aCMD.Cmd = cJZ) Or (aCMD.Cmd = cJNZ) Then Begin
                CheckBox1.Font.Color := aColor;
                CheckBox1.Font.Style := [fsBold];
              End;
            End;
          cCMP, cADD: Begin
              er := Nil;
              Case aCMD.RightOperand Of
                'A': er := Edit1;
                'B': er := Edit2;
                'C': er := Edit3;
                'D': er := Edit4;
              End;
              If Not assigned(er) Then Begin
                label13.caption := aCMD.RightOperand;
                label13.Font.Color := aColor;
                label13.Font.Style := [fsBold];
              End;
            End;
        End;
      End;
    psExecute: Begin
        aColor := PipeLineExecuteBGColor;
        label8.Font.Color := aColor;
        Case aCMD.Cmd Of
          cJMP, cJZ, cJNZ: Begin
              If (aCMD.Cmd = cJMP)
                Or ((aCMD.Cmd = cJZ) And CheckBox1.Checked)
                Or ((aCMD.Cmd = cJNZ) And (Not CheckBox1.Checked))
                Then Begin
                edit6.Text := inttostr(FindNextValidProgramLine(aCMD.JumpTarget) + 1);
                edit6.Font.Color := aColor;
                edit6.Font.Style := [fsBold];
              End;
            End;
          cLOAD: Begin
              // Decode right Operand to x,y in Stringgrid
              x := (strtoint(aCMD.RightOperand) - 100) Mod 5 + 1;
              y := (strtoint(aCMD.RightOperand) - 100) Div 5 + 1;
              el := Nil;
              Case aCMD.LeftOperand Of
                'A': el := Edit1;
                'B': el := Edit2;
                'C': el := Edit3;
                'D': el := Edit4;
              End;
              el.text := inttostr(strtointdef(StringGrid1.Cells[x, y], 0));
              el.Font.Color := aColor;
              el.Font.Style := [fsBold];
            End;
          cMOV: Begin
              el := Nil;
              Case aCMD.LeftOperand Of
                'A': el := Edit1;
                'B': el := Edit2;
                'C': el := Edit3;
                'D': el := Edit4;
              End;
              er := Nil;
              Case aCMD.RightOperand Of
                'A': er := Edit1;
                'B': er := Edit2;
                'C': er := Edit3;
                'D': er := Edit4;
              End;
              If assigned(er) Then Begin
                el.text := er.Text;
              End
              Else Begin
                el.text := aCMD.RightOperand;
              End;
              el.Font.Color := aColor;
              el.Font.Style := [fsBold];
            End;
          cCMP: Begin
              el := Nil;
              Case aCMD.LeftOperand Of
                'A': el := Edit1;
                'B': el := Edit2;
                'C': el := Edit3;
                'D': el := Edit4;
              End;
              er := Nil;
              Case aCMD.RightOperand Of
                'A': er := Edit1;
                'B': er := Edit2;
                'C': er := Edit3;
                'D': er := Edit4;
              End;
              label9.Caption := CMDToStr(aCMD.Cmd, '', '');
              label9.Font.Color := aColor;
              label9.Font.Style := [fsBold];
              label10.Caption := el.Text;
              label10.Font.Color := aColor;
              label10.Font.Style := [fsBold];
              If assigned(er) Then Begin
                label11.Caption := er.Text;
              End
              Else Begin
                label11.Caption := aCMD.RightOperand;
              End;
              label11.Font.Color := aColor;
              label11.Font.Style := [fsBold];
              CheckBox1.Font.Color := aColor;
              CheckBox1.Font.Style := [fsBold];
              CheckBox2.Font.Color := aColor;
              CheckBox2.Font.Style := [fsBold];
              CheckBox3.Font.Color := aColor;
              CheckBox3.Font.Style := [fsBold];
              CheckBox4.Font.Color := aColor;
              CheckBox4.Font.Style := [fsBold];

              // Sign Flag (SF): Entspricht dem höchstwertigen Bit des Subtraktionsergebnisses. Ist das Ergebnis negativ (das zweite Register ist größer), wird das Flag auf 1 gesetzt, andernfalls auf 0.
              // Parity Flag (PF): Gibt an, ob die Anzahl der gesetzten Bits (1en) im niedrigsten Byte des Ergebnisses gerade oder ungerade ist.Auxiliary
              // Carry Flag (AF): Wird bei BCD-Arithmetik (Binary Coded Decimal) verwendet und zeigt einen Übertrag oder Untertrag vom niederwertigen zum höherwertigen Halb-Byte (Nibble) an.
            End;
          cADD, cAND, cNot, cOr, cSHL, cSHR, cSub, cXOR: Begin
              el := Nil;
              Case aCMD.LeftOperand Of
                'A': el := Edit1;
                'B': el := Edit2;
                'C': el := Edit3;
                'D': el := Edit4;
              End;
              er := Nil;
              Case aCMD.RightOperand Of
                'A': er := Edit1;
                'B': er := Edit2;
                'C': er := Edit3;
                'D': er := Edit4;
              End;
              label9.Caption := CMDToStr(aCMD.Cmd, '', '');
              label9.Font.Color := aColor;
              label9.Font.Style := [fsBold];
              label10.Caption := el.Text;
              label10.Font.Color := aColor;
              label10.Font.Style := [fsBold];
              If assigned(er) Then Begin
                label11.Caption := er.Text;
              End
              Else Begin
                label11.Caption := aCMD.RightOperand;
              End;
              label11.Font.Color := aColor;
              label11.Font.Style := [fsBold];
            End;
        End;
      End;
    psWriteBack: Begin
        aColor := PipeLineWritebackBGColor;
        label8.Font.Color := aColor;
        Case aCMD.Cmd Of
          cJMP, cJZ, cJNZ: Begin
              If (aCMD.Cmd = cJMP)
                Or ((aCMD.Cmd = cJZ) And CheckBox1.Checked)
                Or ((aCMD.Cmd = cJNZ) And (Not CheckBox1.Checked))
                Then Begin
                edit6.Text := inttostr(FindNextValidProgramLine(aCMD.JumpTarget) + 1);
              End;
            End;
          cADD, cAND, cNot, cOr, cSHL, cSHR, cSub, cXOR: Begin
              el := Nil;
              Case aCMD.LeftOperand Of
                'A': el := Edit1;
                'B': el := Edit2;
                'C': el := Edit3;
                'D': el := Edit4;
              End;
              er := Nil;
              Case aCMD.RightOperand Of
                'A': er := Edit1;
                'B': er := Edit2;
                'C': er := Edit3;
                'D': er := Edit4;
              End;
              label9.Caption := CMDToStr(aCMD.Cmd, '', '');
              label10.Caption := el.Text;
              If assigned(er) Then Begin
                label11.Caption := er.Text;
              End
              Else Begin
                label11.Caption := aCMD.RightOperand;
              End;
              Case aCMD.Cmd Of
                cADD: label12.Caption := inttostr(strtointdef(label10.Caption, 0) + strtointdef(label11.Caption, 0));
                cAND: label12.Caption := inttostr(strtointdef(label10.Caption, 0) And strtointdef(label11.Caption, 0));
                cNot: label12.Caption := inttostr(Not strtointdef(label10.Caption, 0));
                cOr: label12.Caption := inttostr(strtointdef(label10.Caption, 0) Or strtointdef(label11.Caption, 0));
                cSHL: label12.Caption := inttostr(strtointdef(label10.Caption, 0) Shl strtointdef(label11.Caption, 0));
                cSHR: label12.Caption := inttostr(strtointdef(label10.Caption, 0) Shr strtointdef(label11.Caption, 0));
                cSub: label12.Caption := inttostr(strtointdef(label10.Caption, 0) - strtointdef(label11.Caption, 0));
                cXOR: label12.Caption := inttostr(strtointdef(label10.Caption, 0) Xor strtointdef(label11.Caption, 0));
              End;
              label12.Font.Color := aColor;
              label12.Font.Style := [fsBold];
            End;
        End;
      End;
  End;
End;

Procedure TForm1.VisualizeCmdDraw(Const aCMD: TAssemblerCMD);
Var
  a, b, c, d, e, f: TPoint;
  el: TEdit;
  er: TEdit;
  aColor: TColor;
Begin
  Case aCMD.PipelineStep Of
    psFetch: Begin
        aColor := PipeLineFetchBGColor;
        // Load CMD from Program Memory into Decoder
        a.x := (SynEdit1.Left + SynEdit1.Width) + Scale96ToForm(8);
        a.Y := GroupBox1.Top + GroupBox1.Height Div 2;
        b.x := GroupBox1.Left - Scale96ToForm(8);
        b.Y := GroupBox1.Top + GroupBox1.Height Div 2;
        DrawLine(canvas, a, b, aColor);
        DrawArrowHead(Canvas, b, dRight, aColor);
      End;
    psDecode: Begin
        aColor := PipeLineDecodeBGColor;
        Case aCMD.Cmd Of
          cJMP, cJZ, cJNZ: Begin
              If (aCMD.Cmd = cJMP)
                Or ((aCMD.Cmd = cJZ) And CheckBox1.Checked)
                Or ((aCMD.Cmd = cJNZ) And (Not CheckBox1.Checked))
                Then Begin
                a.x := GroupBox1.Left - Scale96ToForm(8);
                a.y := GroupBox1.Top + Label15.top + GroupBox1.Height - GroupBox1.ClientHeight;
                b.x := edit6.left + edit6.Width Div 2;
                b.y := a.y;
                c.X := b.x;
                c.y := edit6.top + edit6.Height + Scale96ToForm(8);
                DrawLine(canvas, a, b, aColor);
                DrawLine(canvas, b, c, aColor);
                DrawArrowHead(Canvas, c, dUp, aColor);
              End;
            End;
          cLOAD: Begin
              c.x := StringGrid1.Left - Scale96ToForm(8);
              Case aCMD.LeftOperand Of
                'A': Begin
                    a.x := Edit1.Left + edit1.Width Div 2;
                    a.y := edit1.top - Scale96ToForm(8);
                    c.y := StringGrid1.Top + Scale96ToForm(2);
                  End;
                'B': Begin
                    a.x := Edit2.Left + edit2.Width Div 2;
                    a.y := edit2.top - Scale96ToForm(8);
                    c.y := StringGrid1.Top + Scale96ToForm(2 + StringGrid1.DefaultRowHeight Div 3);
                  End;
                'C': Begin
                    a.x := Edit3.Left + edit3.Width Div 2;
                    a.y := edit3.top - Scale96ToForm(8);
                    c.y := StringGrid1.Top + Scale96ToForm(2 + (StringGrid1.DefaultRowHeight * 2) Div 3);
                  End;
                'D': Begin
                    a.x := Edit4.Left + edit4.Width Div 2;
                    a.y := edit4.top - Scale96ToForm(8);
                    c.y := StringGrid1.Top + Scale96ToForm(2 + StringGrid1.DefaultRowHeight);
                  End;
              End;
              b.x := a.x;
              b.y := c.y;
              DrawLine(canvas, a, b, aColor);
              DrawLine(canvas, b, c, aColor);
              DrawArrowHead(Canvas, a, dDown, aColor);
            End;
          cMOV: Begin
              Case aCMD.LeftOperand Of
                'A': Begin
                    a.x := Edit1.Left + edit1.Width Div 2;
                    a.y := edit1.top - Scale96ToForm(8);
                  End;
                'B': Begin
                    a.x := Edit2.Left + edit2.Width Div 2;
                    a.y := edit2.top - Scale96ToForm(8);
                  End;
                'C': Begin
                    a.x := Edit3.Left + edit3.Width Div 2;
                    a.y := edit3.top - Scale96ToForm(8);
                  End;
                'D': Begin
                    a.x := Edit4.Left + edit4.Width Div 2;
                    a.y := edit4.top - Scale96ToForm(8);
                  End;
              End;

              Hier muss noch der Register Register Fall rein


              b := a;
              b.y := a.y - Canvas.TextHeight('B');
              DrawLine(canvas, a, b, aColor);
              DrawArrowHead(Canvas, a, dDown, aColor);
              canvas.Font.Color := aColor;
              canvas.Font.Style := [fsBold];
              canvas.Brush.Style := bsClear;
              canvas.TextOut(b.x - canvas.TextWidth(aCMD.RightOperand) Div 2, b.y - Canvas.TextHeight('B') * 2 + Scale96ToForm(8), aCMD.RightOperand);
              canvas.Font.Style := [];
              canvas.Font.Color := clBlack;
            End;
          cCMP, cADD: Begin
              el := Nil;
              Case aCMD.LeftOperand Of
                'A': el := Edit1;
                'B': el := Edit2;
                'C': el := Edit3;
                'D': el := Edit4;
              End;
              er := Nil;
              Case aCMD.RightOperand Of
                'A': er := Edit1;
                'B': er := Edit2;
                'C': er := Edit3;
                'D': er := Edit4;
              End;
              // Left Operand Arrow
              a.x := el.Left + el.Width Div 2;
              a.y := el.Top + el.Height + Scale96ToForm(8);
              d.x := Image1.Left + Image1.Width Div 6;
              d.y := Image1.Top - Scale96ToForm(8);
              b.x := a.x;
              b.y := (a.y + d.y) Div 2 - Scale96ToForm(4);
              c.x := d.x;
              c.y := (a.y + d.y) Div 2 - Scale96ToForm(4);
              DrawLine(canvas, a, b, aColor);
              DrawLine(canvas, b, c, aColor);
              DrawLine(canvas, c, d, aColor);
              DrawArrowHead(Canvas, d, dDown, aColor);

              // Right Operand Arrow
              If assigned(er) Then Begin
                a.x := er.Left + er.Width Div 2;
                a.y := er.Top + er.Height + Scale96ToForm(8);
              End
              Else Begin
                a.x := label13.Left + label13.Width Div 2;
                a.y := label13.Top + label13.Height + Scale96ToForm(8);
              End;
              d.x := Image1.Left + (Image1.Width * 5) Div 6;
              d.y := Image1.Top - Scale96ToForm(8);
              b.x := a.x;
              b.y := (a.y + d.y) Div 2 + Scale96ToForm(4);
              c.x := d.x;
              c.y := (a.y + d.y) Div 2 + Scale96ToForm(4);
              DrawLine(canvas, a, b, aColor);
              DrawLine(canvas, b, c, aColor);
              DrawLine(canvas, c, d, aColor);
              DrawArrowHead(Canvas, d, dDown, aColor);
            End;
        End;
      End;
    psExecute: Begin
        aColor := PipeLineExecuteBGColor;
        Case aCMD.Cmd Of
          cCMP: Begin
              a.x := Image1.Left + Image1.Width + Scale96ToForm(8);
              a.Y := Image1.Top + Image1.Height Div 2;
              b.x := CheckBox1.Left - Scale96ToForm(8);
              b.Y := a.y;
              DrawLine(canvas, a, b, aColor);
              DrawArrowHead(Canvas, b, dRight, aColor);
            End;
        End;
      End;
    psWriteBack: Begin
        aColor := PipeLineWritebackBGColor;
        Case aCMD.Cmd Of
          cSTORE: Begin
              c.x := StringGrid1.Left - Scale96ToForm(8);
              Case aCMD.LeftOperand Of
                'A': Begin
                    a.x := Edit1.Left + edit1.Width Div 2;
                    a.y := edit1.top - Scale96ToForm(8);
                    c.y := StringGrid1.Top + Scale96ToForm(2);
                  End;
                'B': Begin
                    a.x := Edit2.Left + edit2.Width Div 2;
                    a.y := edit2.top - Scale96ToForm(8);
                    c.y := StringGrid1.Top + Scale96ToForm(2 + StringGrid1.DefaultRowHeight Div 3);
                  End;
                'C': Begin
                    a.x := Edit3.Left + edit3.Width Div 2;
                    a.y := edit3.top - Scale96ToForm(8);
                    c.y := StringGrid1.Top + Scale96ToForm(2 + (StringGrid1.DefaultRowHeight * 2) Div 3);
                  End;
                'D': Begin
                    a.x := Edit4.Left + edit4.Width Div 2;
                    a.y := edit4.top - Scale96ToForm(8);
                    c.y := StringGrid1.Top + Scale96ToForm(2 + StringGrid1.DefaultRowHeight);
                  End;
              End;
              b.x := a.x;
              b.y := c.y;
              DrawLine(canvas, a, b, aColor);
              DrawLine(canvas, b, c, aColor);
              DrawArrowHead(Canvas, c, dRight, aColor);
            End;
          cADD, cAND, cNot, cOr, cSHL, cSHR, cSub, cXOR: Begin
              el := Nil;
              Case aCMD.LeftOperand Of
                'A': el := Edit1;
                'B': el := Edit2;
                'C': el := Edit3;
                'D': el := Edit4;
              End;
              a.x := Image1.Left + Image1.Width Div 2;
              a.y := label12.top + label12.height + Scale96ToForm(8);
              b.x := a.x;
              b.y := a.y + Scale96ToForm(16);
              c.x := Image1.Left - Scale96ToForm(8);
              c.y := b.y;
              d.x := c.x;
              d.y := Image1.Top - Scale96ToForm(8);
              e.X := el.left + el.Width Div 2;
              e.y := d.y;
              f.x := e.x;
              f.y := el.Top + el.Height + Scale96ToForm(8);
              DrawLine(canvas, a, b, aColor);
              DrawLine(canvas, b, c, aColor);
              DrawLine(canvas, c, d, aColor);
              DrawLine(canvas, d, e, aColor);
              DrawLine(canvas, e, f, aColor);
              DrawArrowHead(Canvas, f, dUp, aColor);
            End;
        End;
      End;
  End;
End;

End.

