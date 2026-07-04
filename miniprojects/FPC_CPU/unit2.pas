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
Unit Unit2;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  ExtCtrls, Menus, uFPC_CPU, uFPC_CPU_LCL;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    ALU_out_flag_zero: TCheckBox;
    ALU_out_flag_carry: TCheckBox;
    ALU_out_flag_negative: TCheckBox;
    ALU_out_flag_overflow: TCheckBox;
    RegisterA: TEdit;
    RegisterB: TEdit;
    RegisterC: TEdit;
    RegisterD: TEdit;
    Programcounter: TEdit;
    GroupBox1: TGroupBox;
    Image1: TImage;
    Label1: TLabel;
    ALU_in_Left_OP: TLabel;
    ALU_in_Right_OP: TLabel;
    ALU_out_result: TLabel;
    Const_in_For_ALU: TLabel;
    Actual_tick: TLabel;
    CU_Target_ProgramCounter: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    StackPointer: TLabel;
    Label6: TLabel;
    CU_Fetched_CMD: TLabel;
    CU_Pipeline_State: TLabel;
    ALU_Operation: TLabel;
    Stack: TListBox;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    PopupMenu1: TPopupMenu;
    Separator1: TMenuItem;
    Memory: TStringGrid;
    Procedure FormCreate(Sender: TObject);
    Procedure FormPaint(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure MenuItem1Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure MenuItem3Click(Sender: TObject);
  private

  public
    Function OperandToEdit(aOperand: String): TEdit;

    Procedure VisualizeCmdLCL(Const aCMD: TAssemblerCMD);
    Procedure VisualizeCmdDraw(Const aCMD: TAssemblerCMD);
  End;

Var
  Form2: TForm2;

Implementation

{$R *.lfm}

Uses Unit1;

{ TForm2 }

Procedure TForm2.FormCreate(Sender: TObject);
Var
  i: Integer;
Begin
  caption := 'FPC-CPU';

  Memory.Cells[0, 0] := 'Memory';
  For i := 1 To 5 Do Begin
    Memory.Cells[i, 0] := inttostr(i - 1);
  End;
  For i := 1 To 4 Do Begin
    Memory.Cells[0, i] := inttostr((i - 1) * 5 + 100);
  End;
  Memory.Cells[1, 1] := '3';
  Memory.Cells[2, 1] := '1';
  Memory.Cells[3, 1] := '2';
  Memory.Cells[4, 1] := '3';
  label16.font.Color := PipeLineFetchBGColor;
  label17.font.Color := PipeLineDecodeBGColor;
  label18.font.Color := PipeLineExecuteBGColor;
  label19.font.Color := PipeLineWritebackBGColor;

  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
End;

Procedure TForm2.FormPaint(Sender: TObject);
Var
  i: Integer;
Begin
  For i := 0 To PipeLineDepth - 1 Do Begin
    If (PipeLine[i] >= 0) And (PipeLine[i] <= high(fCMDs)) Then Begin
      VisualizeCmdDraw(fCMDs[PipeLine[i]]);
    End;
  End;
End;

Procedure TForm2.FormShow(Sender: TObject);
Begin
  left := form1.Left + form1.Width + Scale96ToScreen(8);
  top := form1.top;
  image1.width := ALU_in_Right_OP.left + ALU_in_Right_OP.Width - ALU_in_Left_OP.left;
  image1.height := ALU_Operation.top + ALU_Operation.Height - ALU_in_Left_OP.top;
End;

Procedure TForm2.MenuItem1Click(Sender: TObject);
Begin
  // Display-only mode: stack is owned by engine state sync.
End;

Procedure TForm2.MenuItem2Click(Sender: TObject);
Begin
  // Display-only mode: stack is owned by engine state sync.
End;

Procedure TForm2.MenuItem3Click(Sender: TObject);
Begin
  // Display-only mode: stack is owned by engine state sync.
End;

Function TForm2.OperandToEdit(aOperand: String): TEdit;
Begin
  result := Nil;
  Case aOperand Of
    'A': result := RegisterA;
    'B': result := RegisterB;
    'C': result := RegisterC;
    'D': result := RegisterD;
  End;
End;

Procedure TForm2.VisualizeCmdLCL(Const aCMD: TAssemblerCMD);
  Function OperandValueFromUI(Const aOperand: String): Integer;
  Var
    aEdit: TEdit;
  Begin
    aEdit := OperandToEdit(aOperand);
    If Assigned(aEdit) Then Begin
      result := StrToIntDef(aEdit.Text, 0);
    End
    Else Begin
      result := StrToIntDef(aOperand, 0);
    End;
  End;

  Function ALUResultFromCMD(Const aALUCMD: TAssemblerCMD): String;
  Var
    aLeft, aRight: Integer;
  Begin
    aLeft := OperandValueFromUI(aALUCMD.LeftOperand);
    aRight := OperandValueFromUI(aALUCMD.RightOperand);
    Case aALUCMD.Cmd Of
      cADD: result := IntToStr(aLeft + aRight);
      cAND: result := IntToStr(aLeft And aRight);
      cDIV: If aRight <> 0 Then result := IntToStr(aLeft Div aRight) Else result := IntToStr(aLeft);
      cNot: result := IntToStr(Not aLeft);
      cOr: result := IntToStr(aLeft Or aRight);
      cMul: result := IntToStr(aLeft * aRight);
      cSHL: result := IntToStr(aLeft Shl aRight);
      cSHR: result := IntToStr(aLeft Shr aRight);
      cSub: result := IntToStr(aLeft - aRight);
      cXOR: result := IntToStr(aLeft Xor aRight);
    Else
      result := '0';
    End;
  End;

Var
  x, y: integer;
  el, er: TEdit;
  aColor: TColor;
  s: String;
Begin
  If Not ((Not Form1.CheckBox5.Checked) And (aCMD.PipelineStep = psWriteBack) And (aCMD.Cmd = cRET)) Then Begin
    Programcounter.text := inttostr(aCMD.Line + 1);
  End;
  If Not Form1.CheckBox5.Checked Then Begin
    CU_Fetched_CMD.caption := CMDToStr(aCMD.Cmd, aCMD.LeftOperand, aCMD.RightOperand);
    CU_Pipeline_State.caption := PipelineStepToStr(aCMD.PipelineStep);
  End
  Else Begin
    CU_Fetched_CMD.caption := '';
    CU_Pipeline_State.caption := '';
  End;
  Case aCMD.PipelineStep Of
    psFetch: Begin
        If Form1.CheckBox5.Checked Then Begin
          CU_Fetched_CMD.caption := CMDToStr(aCMD.Cmd, aCMD.LeftOperand, aCMD.RightOperand);
          CU_Pipeline_State.caption := PipelineStepToStr(aCMD.PipelineStep);
        End;
        aColor := PipeLineFetchBGColor;
        CU_Pipeline_State.Font.Color := aColor;
        CU_Fetched_CMD.Font.Color := aColor;
        CU_Fetched_CMD.Font.Style := [fsBold];
      End;
    psDecode: Begin
        aColor := PipeLineDecodeBGColor;
        CU_Pipeline_State.Font.Color := aColor;
        Case aCMD.Cmd Of
          cPUSH: Begin
              el := OperandToEdit(aCMD.LeftOperand);
              If Assigned(el) Then Begin
                el.Font.Color := aColor;
                el.Font.Style := [fsBold];
              End;
              StackPointer.Font.Color := aColor;
              StackPointer.Font.Style := [fsBold];
            End;
          cJMP, cJZ, cJNZ, cJN, cJNN, cCALL: Begin
              If (aCMD.Cmd = cJMP)
                Or (aCMD.Cmd = cCALL)
                Or ((aCMD.Cmd = cJZ) And ALU_out_flag_zero.Checked)
                Or ((aCMD.Cmd = cJNZ) And (Not ALU_out_flag_zero.Checked))
                Or ((aCMD.Cmd = cJN) And ALU_out_flag_negative.Checked)
                Or ((aCMD.Cmd = cJNN) And (Not ALU_out_flag_negative.Checked))
                Then Begin
                CU_Target_ProgramCounter.Caption := inttostr(FindNextValidProgramLine(aCMD.JumpTarget) + 1);
                CU_Target_ProgramCounter.Font.Color := aColor;
                CU_Target_ProgramCounter.Font.Style := [fsBold];
              End;
              If (aCMD.Cmd = cJZ) Or (aCMD.Cmd = cJNZ) Then Begin
                ALU_out_flag_zero.Font.Color := aColor;
                ALU_out_flag_zero.Font.Style := [fsBold];
              End;
              If (aCMD.Cmd = cJN) Or (aCMD.Cmd = cJNN) Then Begin
                ALU_out_flag_negative.Font.Color := aColor;
                ALU_out_flag_negative.Font.Style := [fsBold];
              End;
            End;
          // All CMD's that go through the ALU
          cADD, cAnd, cCMP, cDIV, cMUL, cNot, cOr, cSHL, cSHR, cSUB, cXOR: Begin
              er := OperandToEdit(aCMD.RightOperand);
              If Not assigned(er) Then Begin
                Const_in_For_ALU.caption := aCMD.RightOperand;
                Const_in_For_ALU.Font.Color := aColor;
                Const_in_For_ALU.Font.Style := [fsBold];
              End;
            End;
        End;
      End;
    psExecute: Begin
        aColor := PipeLineExecuteBGColor;
        CU_Pipeline_State.Font.Color := aColor;
        Case aCMD.Cmd Of
          cRET: Begin
              If Stack.Items.Count <> 0 Then Begin
                Programcounter.text := inttostr(
                  fCMDs[FindNextValidProgramLine(convertCodeLineToCMDIndex(strtoint(Stack.Items[0]) - 1), true)].Line + 1
                  );
                // Execute preview in non-pipeline mode: RET consumes stack top immediately.
                If Not Form1.CheckBox5.Checked Then Begin
                  Stack.Items.Delete(0);
                End;
              End
              Else Begin
                Programcounter.text := '0'; // Stack ist Empty -> Ungültig
              End;
              Programcounter.Font.Color := aColor;
              Programcounter.Font.Style := [fsBold];
              StackPointer.Font.Color := aColor;
            End;
          cCALL: Begin
              Programcounter.text := inttostr(FindNextValidProgramLine(aCMD.JumpTarget) + 1);
              Programcounter.Font.Color := aColor;
              Programcounter.Font.Style := [fsBold];
              // Execute preview in non-pipeline mode.
              If Not Form1.CheckBox5.Checked Then Begin
                s := IntToStr(aCMD.Line + 1);
                If (Stack.Items.Count = 0) Or (Stack.Items[0] <> s) Then Begin
                  Stack.Items.Insert(0, s);
                End;
              End;
              StackPointer.Font.Color := aColor;
            End;
          cPUSH: Begin
              el := OperandToEdit(aCMD.LeftOperand);
              If Assigned(el) Then Begin
                el.Font.Color := aColor;
                el.Font.Style := [fsBold];
              End;
              StackPointer.Font.Color := aColor;
            End;
          cPOP: Begin
              el := OperandToEdit(aCMD.LeftOperand);
              el.Font.Color := aColor;
              el.Font.Style := [fsBold];
              StackPointer.Font.Color := aColor;
            End;
          cJMP, cJZ, cJNZ, cJN, cJNN: Begin
              If (aCMD.Cmd = cJMP)
                Or ((aCMD.Cmd = cJZ) And ALU_out_flag_zero.Checked)
                Or ((aCMD.Cmd = cJNZ) And (Not ALU_out_flag_zero.Checked))
                Or ((aCMD.Cmd = cJN) And ALU_out_flag_negative.Checked)
                Or ((aCMD.Cmd = cJNN) And (Not ALU_out_flag_negative.Checked))
                Then Begin
                Programcounter.Text := inttostr(FindNextValidProgramLine(aCMD.JumpTarget) + 1);
                Programcounter.Font.Color := aColor;
                Programcounter.Font.Style := [fsBold];
              End;
            End;
          cLOAD: Begin
              // LOAD bypasses ALU visualization: show direct memory->register effect.
              el := OperandToEdit(aCMD.LeftOperand);
              If Assigned(el) Then Begin
                el.Font.Color := aColor;
                el.Font.Style := [fsBold];
              End;
            End;
          cMOV: Begin
              el := OperandToEdit(aCMD.LeftOperand);
              er := OperandToEdit(aCMD.RightOperand);
              // MOV bypasses ALU: only highlight source/destination path.
              If Assigned(er) Then Begin
                er.Font.Color := aColor;
                er.Font.Style := [fsBold];
                If Assigned(el) Then Begin
                  el.Text := er.Text;
                End;
              End;
              If (Not Assigned(er)) And Assigned(el) Then Begin
                el.Text := aCMD.RightOperand;
              End;
              If Assigned(el) Then Begin
                el.Font.Color := aColor;
                el.Font.Style := [fsBold];
              End;
            End;
          cCMP: Begin
              el := OperandToEdit(aCMD.LeftOperand);
              er := OperandToEdit(aCMD.RightOperand);
              ALU_Operation.Caption := CMDToStr(aCMD.Cmd, '', '');
              ALU_Operation.Font.Color := aColor;
              ALU_Operation.Font.Style := [fsBold];
              ALU_in_Left_OP.Caption := el.Text;
              ALU_in_Left_OP.Font.Color := aColor;
              ALU_in_Left_OP.Font.Style := [fsBold];
              If assigned(er) Then Begin
                ALU_in_Right_OP.Caption := er.Text;
              End
              Else Begin
                ALU_in_Right_OP.Caption := aCMD.RightOperand;
              End;
              ALU_in_Right_OP.Font.Color := aColor;
              ALU_in_Right_OP.Font.Style := [fsBold];
              ALU_out_flag_zero.Font.Color := aColor;
              ALU_out_flag_zero.Font.Style := [fsBold];
              ALU_out_flag_carry.Font.Color := aColor;
              ALU_out_flag_carry.Font.Style := [fsBold];
              ALU_out_flag_negative.Font.Color := aColor;
              ALU_out_flag_negative.Font.Style := [fsBold];
              ALU_out_flag_overflow.Font.Color := aColor;
              ALU_out_flag_overflow.Font.Style := [fsBold];

              // Sign Flag (SF): Entspricht dem höchstwertigen Bit des Subtraktionsergebnisses. Ist das Ergebnis negativ (das zweite Register ist größer), wird das Flag auf 1 gesetzt, andernfalls auf 0.
              // Parity Flag (PF): Gibt an, ob die Anzahl der gesetzten Bits (1en) im niedrigsten Byte des Ergebnisses gerade oder ungerade ist.Auxiliary
              // Carry Flag (AF): Wird bei BCD-Arithmetik (Binary Coded Decimal) verwendet und zeigt einen Übertrag oder Untertrag vom niederwertigen zum höherwertigen Halb-Byte (Nibble) an.
            End;
          // All CMD's that go through the ALU
          cADD, cAND, cDIV, cMUL, cNot, cOr, cSHL, cSHR, cSub, cXOR: Begin
              er := OperandToEdit(aCMD.RightOperand);
              ALU_Operation.Caption := CMDToStr(aCMD.Cmd, '', '');
              ALU_Operation.Font.Color := aColor;
              ALU_Operation.Font.Style := [fsBold];
              ALU_in_Left_OP.Caption := aCMD.LeftOperand;
              ALU_in_Left_OP.Font.Color := aColor;
              ALU_in_Left_OP.Font.Style := [fsBold];
              ALU_in_Right_OP.Caption := aCMD.RightOperand;
              ALU_in_Right_OP.Font.Color := aColor;
              ALU_in_Right_OP.Font.Style := [fsBold];
              ALU_out_result.Caption := ALUResultFromCMD(aCMD);
              ALU_out_result.Font.Color := aColor;
              ALU_out_result.Font.Style := [fsBold];
            End;
        End;
      End;
    psWriteBack: Begin
        aColor := PipeLineWritebackBGColor;
        CU_Pipeline_State.Font.Color := aColor;
        Case aCMD.Cmd Of
          cCALL: Begin
              Programcounter.Text := inttostr(FindNextValidProgramLine(aCMD.JumpTarget) + 1);
              // WriteBack preview in non-pipeline mode to keep stack state stable across steps.
              If Not Form1.CheckBox5.Checked Then Begin
                s := IntToStr(aCMD.Line + 1);
                If (Stack.Items.Count = 0) Or (Stack.Items[0] <> s) Then Begin
                  Stack.Items.Insert(0, s);
                End;
              End;
              StackPointer.Font.Color := aColor;
            End;
          cPOP: Begin
              el := OperandToEdit(aCMD.LeftOperand);
              If Stack.Items.Count <> 0 Then Begin
                If Assigned(el) Then Begin
                  el.Text := Stack.Items[0];
                End;
                Stack.Items.Delete(0);
              End;
              If Assigned(el) Then Begin
                el.Font.Color := aColor;
                el.Font.Style := [fsBold];
              End;
              StackPointer.Font.Color := aColor;
            End;
          cPUSH: Begin
              el := OperandToEdit(aCMD.LeftOperand);
              If Assigned(el) Then Begin
                Stack.Items.Insert(0, el.Text);
              End
              Else Begin
                Stack.Items.Insert(0, aCMD.LeftOperand);
              End;
              StackPointer.Font.Color := aColor;
            End;
          cRET: Begin
              // Keep RET WB visualization stable: stack was already consumed in Execute preview.
              If (Not Form1.CheckBox5.Checked) And (Stack.Items.Count > 0) Then Begin
                Stack.Items.Delete(0);
              End;
              Programcounter.Font.Color := aColor;
              Programcounter.Font.Style := [fsBold];
              StackPointer.Font.Color := aColor;
            End;
          cJMP, cJZ, cJNZ, cJN, cJNN: Begin
              If (aCMD.Cmd = cJMP)
                Or ((aCMD.Cmd = cJZ) And ALU_out_flag_zero.Checked)
                Or ((aCMD.Cmd = cJNZ) And (Not ALU_out_flag_zero.Checked))
                Or ((aCMD.Cmd = cJN) And ALU_out_flag_negative.Checked)
                Or ((aCMD.Cmd = cJNN) And (Not ALU_out_flag_negative.Checked))
                Then Begin
                Programcounter.Text := inttostr(FindNextValidProgramLine(aCMD.JumpTarget) + 1);
              End;
            End;
          cADD, cAND, cDIV, cNot, cOr, cMUL, cSHL, cSHR, cSub, cXOR: Begin
              el := OperandToEdit(aCMD.LeftOperand);
              ALU_Operation.Caption := CMDToStr(aCMD.Cmd, '', '');
              ALU_Operation.Font.Color := aColor;
              ALU_Operation.Font.Style := [fsBold];
              ALU_in_Left_OP.Caption := aCMD.LeftOperand;
              ALU_in_Left_OP.Font.Color := aColor;
              ALU_in_Left_OP.Font.Style := [fsBold];
              ALU_in_Right_OP.Caption := aCMD.RightOperand;
              ALU_in_Right_OP.Font.Color := aColor;
              ALU_in_Right_OP.Font.Style := [fsBold];
              ALU_out_result.Caption := ALUResultFromCMD(aCMD);
              ALU_out_result.Font.Color := aColor;
              ALU_out_result.Font.Style := [fsBold];
              If Assigned(el) Then Begin
                el.Font.Color := aColor;
                el.Font.Style := [fsBold];
              End;
            End;
          cMOV: Begin
              el := OperandToEdit(aCMD.LeftOperand);
              er := OperandToEdit(aCMD.RightOperand);
              // MOV bypasses ALU: keep ALU panel untouched.
              If Assigned(er) Then Begin
                er.Font.Color := aColor;
                er.Font.Style := [fsBold];
                If Assigned(el) Then Begin
                  el.Text := er.Text;
                End;
              End
              Else Begin
                If Assigned(el) Then Begin
                  el.Text := aCMD.RightOperand;
                End;
              End;
              If Assigned(el) Then Begin
                el.Font.Color := aColor;
                el.Font.Style := [fsBold];
              End;
            End;
          cLOAD: Begin
              er := OperandToEdit(aCMD.RightOperand);
              If Assigned(er) Then Begin
                s := er.Text;
              End
              Else Begin
                s := aCMD.RightOperand;
              End;
              x := (StrToIntDef(s, 100) - 100) Mod 5 + 1;
              y := (StrToIntDef(s, 100) - 100) Div 5 + 1;
              el := OperandToEdit(aCMD.LeftOperand);
              If Assigned(el) Then Begin
                If (x >= 1) And (x <= 5) And (y >= 1) And (y <= 4) Then Begin
                  el.Text := Memory.Cells[x, y];
                End
                Else Begin
                  el.Text := '0';
                End;
              End;
              // In WriteBack only the destination register should be emphasized.
              If Assigned(el) Then Begin
                el.Font.Color := aColor;
                el.Font.Style := [fsBold];
              End;
            End;
        End;
      End;
  End;
End;

Procedure TForm2.VisualizeCmdDraw(Const aCMD: TAssemblerCMD);
  Function GetEditPoint(Const e: TEdit; Above: Boolean = true): TPoint;
  Begin
    result.x := e.Left + e.Width Div 2;
    If above Then Begin
      result.y := e.top - Scale96ToForm(8);
    End
    Else Begin
      result.y := e.top + e.height + Scale96ToForm(8);
    End;
  End;

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
        a.x := {(SynEdit1.Left + SynEdit1.Width) +} Scale96ToForm(8);
        a.Y := GroupBox1.Top + GroupBox1.Height Div 2;
        b.x := GroupBox1.Left - Scale96ToForm(8);
        b.Y := GroupBox1.Top + GroupBox1.Height Div 2;
        DrawLine(canvas, a, b, aColor);
        DrawArrowHead(Canvas, b, dRight, aColor);
      End;
    psDecode: Begin
        aColor := PipeLineDecodeBGColor;
        Case aCMD.Cmd Of
          cPUSH: Begin
              // Decode preview only: show immediate source text without transfer arrow.
              el := OperandToEdit(aCMD.LeftOperand);
              If Not assigned(el) Then Begin
                a.x := Memory.Left;
                a.y := Stack.Top;
                canvas.Font.Color := aColor;
                canvas.Font.Style := [fsBold];
                canvas.Brush.Style := bsClear;
                canvas.TextOut(a.x - canvas.TextWidth(aCMD.LeftOperand) - Scale96ToForm(8), a.y, aCMD.LeftOperand);
                canvas.Font.Style := [];
                canvas.Font.Color := clBlack;
              End;
            End;
          cRET: Begin
              // Der Pfeil Stack -> PC
              a.x := Programcounter.left + Programcounter.Width Div 4;
              a.y := Programcounter.top + Programcounter.Height + Scale96ToForm(8);
              c.x := Stack.Left - Scale96ToForm(8);
              c.y := Stack.Top;
              b.x := a.x;
              b.y := c.y;
              DrawLine(canvas, a, b, aColor);
              DrawLine(canvas, b, c, aColor);
              DrawArrowHead(Canvas, a, dUp, aColor);
            End;
          cCALL: Begin
              // Der Pfeil "aufgelöster" Jump to PC
              a.x := GroupBox1.Left - Scale96ToForm(8);
              a.y := GroupBox1.Top + CU_Target_ProgramCounter.top + GroupBox1.Height - GroupBox1.ClientHeight;
              b.x := Programcounter.left + Programcounter.Width * 3 Div 4;
              b.y := a.y;
              c.X := b.x;
              c.y := Programcounter.top + Programcounter.Height + Scale96ToForm(8);
              DrawLine(canvas, a, b, aColor);
              DrawLine(canvas, b, c, aColor);
              DrawArrowHead(Canvas, c, dUp, aColor);
              // Der Pfeil PC -> Stack
              a.x := Programcounter.left + Programcounter.Width Div 4;
              a.y := Programcounter.top + Programcounter.Height + Scale96ToForm(8);
              c.x := Stack.Left - Scale96ToForm(8);
              c.y := Stack.Top;
              b.x := a.x;
              b.y := c.y;
              DrawLine(canvas, a, b, aColor);
              DrawLine(canvas, b, c, aColor);
              DrawArrowHead(Canvas, c, dRight, aColor);
            End;
          cPop: Begin
              Case aCMD.LeftOperand Of
                'A': a := GetEditPoint(RegisterA);
                'B': a := GetEditPoint(RegisterB);
                'C': a := GetEditPoint(RegisterC);
                'D': a := GetEditPoint(RegisterD);
              End;
              c.X := Stack.Left - Scale96ToForm(8);
              c.Y := Stack.Top;
              b.x := a.x;
              b.y := c.y;
              DrawLine(canvas, a, b, aColor);
              DrawLine(canvas, b, c, aColor);
              DrawArrowHead(Canvas, a, dDown, aColor);
            End;
              cJMP, cJZ, cJNZ, cJN, cJNN: Begin
              If (aCMD.Cmd = cJMP)
                Or ((aCMD.Cmd = cJZ) And ALU_out_flag_zero.Checked)
                Or ((aCMD.Cmd = cJNZ) And (Not ALU_out_flag_zero.Checked))
                Or ((aCMD.Cmd = cJN) And ALU_out_flag_negative.Checked)
                Or ((aCMD.Cmd = cJNN) And (Not ALU_out_flag_negative.Checked))
                Then Begin
                a.x := GroupBox1.Left - Scale96ToForm(8);
                a.y := GroupBox1.Top + CU_Target_ProgramCounter.top + GroupBox1.Height - GroupBox1.ClientHeight;
                b.x := Programcounter.left + Programcounter.Width Div 2;
                b.y := a.y;
                c.X := b.x;
                c.y := Programcounter.top + Programcounter.Height + Scale96ToForm(8);
                DrawLine(canvas, a, b, aColor);
                DrawLine(canvas, b, c, aColor);
                DrawArrowHead(Canvas, c, dUp, aColor);
              End;
            End;
          cMOV: Begin
              Case aCMD.LeftOperand Of
                'A': a := GetEditPoint(RegisterA);
                'B': a := GetEditPoint(RegisterB);
                'C': a := GetEditPoint(RegisterC);
                'D': a := GetEditPoint(RegisterD);
              End;
              b := a;
              b.y := a.y - Canvas.TextHeight('B');
              If (length(aCMD.RightOperand) = 1) And (aCMD.RightOperand[1] In ['A'..'D']) Then Begin
                Case aCMD.RightOperand Of
                  'A': d := GetEditPoint(RegisterA);
                  'B': d := GetEditPoint(RegisterB);
                  'C': d := GetEditPoint(RegisterC);
                  'D': d := GetEditPoint(RegisterD);
                End;
                c.x := d.x;
                c.y := b.y;
                DrawLine(canvas, b, c, aColor);
                DrawLine(canvas, c, d, aColor);
              End
              Else Begin
                canvas.Font.Color := aColor;
                canvas.Font.Style := [fsBold];
                canvas.Brush.Style := bsClear;
                canvas.TextOut(b.x - canvas.TextWidth(aCMD.RightOperand) Div 2, b.y - Canvas.TextHeight('B') * 2 + Scale96ToForm(8), aCMD.RightOperand);
                canvas.Font.Style := [];
                canvas.Font.Color := clBlack;
              End;
              DrawLine(canvas, a, b, aColor);
              DrawArrowHead(Canvas, a, dDown, aColor);
            End;
          cADD, cAND, cCMP, cDiv, cMUL, cNOT, cOR, cSHL, cSHR, cSUB, cXOR: Begin
              el := Nil;
              Case aCMD.LeftOperand Of
                'A': el := RegisterA;
                'B': el := RegisterB;
                'C': el := RegisterC;
                'D': el := RegisterD;
              End;
              er := Nil;
              Case aCMD.RightOperand Of
                'A': er := RegisterA;
                'B': er := RegisterB;
                'C': er := RegisterC;
                'D': er := RegisterD;
              End;
              // Left Operand Arrow
              a := GetEditPoint(el, false);
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
                a := GetEditPoint(er, false);
              End
              Else Begin
                a.x := Const_in_For_ALU.Left + Const_in_For_ALU.Width Div 2;
                a.y := Const_in_For_ALU.Top + Const_in_For_ALU.Height + Scale96ToForm(8);
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
          cPUSH: Begin
              el := OperandToEdit(aCMD.LeftOperand);
              If assigned(el) Then Begin
                a := GetEditPoint(el);
              End
              Else Begin
                a.x := Memory.Left;
                a.y := Stack.Top;
                canvas.Font.Color := aColor;
                canvas.Font.Style := [fsBold];
                canvas.Brush.Style := bsClear;
                canvas.TextOut(a.x - canvas.TextWidth(aCMD.LeftOperand) - Scale96ToForm(8), a.y, aCMD.LeftOperand);
                canvas.Font.Style := [];
                canvas.Font.Color := clBlack;
              End;
              c.X := Stack.Left - Scale96ToForm(8);
              c.Y := Stack.Top;
              b.x := a.x;
              b.y := c.y;
              DrawLine(canvas, a, b, aColor);
              DrawLine(canvas, b, c, aColor);
              DrawArrowHead(Canvas, c, dRight, aColor);
            End;
          cLOAD: Begin
              c.x := Memory.Left - Scale96ToForm(8);
              Case aCMD.LeftOperand Of
                'A': Begin
                    a := GetEditPoint(RegisterA);
                    c.y := Memory.Top + Scale96ToForm(2);
                  End;
                'B': Begin
                    a := GetEditPoint(RegisterB);
                    c.y := Memory.Top + Scale96ToForm(2 + Memory.DefaultRowHeight Div 3);
                  End;
                'C': Begin
                    a := GetEditPoint(RegisterC);
                    c.y := Memory.Top + Scale96ToForm(2 + (Memory.DefaultRowHeight * 2) Div 3);
                  End;
                'D': Begin
                    a := GetEditPoint(RegisterD);
                    c.y := Memory.Top + Scale96ToForm(2 + Memory.DefaultRowHeight);
                  End;
              End;
              b.x := a.x;
              b.y := c.y;
              DrawLine(canvas, a, b, aColor);
              DrawLine(canvas, b, c, aColor);
              DrawArrowHead(Canvas, a, dDown, aColor);
              er := OperandToEdit(aCMD.RightOperand);
              If assigned(er) Then Begin
                a := GetEditPoint(er);
                b.x := a.x;
                DrawLine(canvas, a, b, aColor);
                DrawLine(canvas, b, c, aColor);
              End;
            End;
          cCMP: Begin
              a.x := Image1.Left + Image1.Width + Scale96ToForm(8);
              a.Y := Image1.Top + Image1.Height Div 2;
              b.x := ALU_out_flag_zero.Left - Scale96ToForm(8);
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
              c.x := Memory.Left - Scale96ToForm(8);
              Case aCMD.LeftOperand Of
                'A': Begin
                    a := GetEditPoint(RegisterA);
                    c.y := Memory.Top + Scale96ToForm(2);
                  End;
                'B': Begin
                    a := GetEditPoint(RegisterB);
                    c.y := Memory.Top + Scale96ToForm(2 + Memory.DefaultRowHeight Div 3);
                  End;
                'C': Begin
                    a := GetEditPoint(RegisterC);
                    c.y := Memory.Top + Scale96ToForm(2 + (Memory.DefaultRowHeight * 2) Div 3);
                  End;
                'D': Begin
                    a := GetEditPoint(RegisterD);
                    c.y := Memory.Top + Scale96ToForm(2 + Memory.DefaultRowHeight);
                  End;
              End;
              b.x := a.x;
              b.y := c.y;
              DrawLine(canvas, a, b, aColor);
              DrawLine(canvas, b, c, aColor);
              DrawArrowHead(Canvas, c, dRight, aColor);
            End;
          cADD, cAND, cDIV, cNot, cMUL, cOr, cSHL, cSHR, cSub, cXOR: Begin
              el := Nil;
              Case aCMD.LeftOperand Of
                'A': el := RegisterA;
                'B': el := RegisterB;
                'C': el := RegisterC;
                'D': el := RegisterD;
              End;
              a.x := Image1.Left + Image1.Width Div 2;
              a.y := ALU_out_result.top + ALU_out_result.height + Scale96ToForm(8);
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

