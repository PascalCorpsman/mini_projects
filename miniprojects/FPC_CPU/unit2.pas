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
  ExtCtrls, Menus, uFPC_CPU;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit6: TEdit;
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
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    PopupMenu1: TPopupMenu;
    Separator1: TMenuItem;
    StringGrid1: TStringGrid;
    Procedure FormCreate(Sender: TObject);
    Procedure FormPaint(Sender: TObject);
    Procedure FormShow(Sender: TObject);
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

  StringGrid1.Cells[0, 0] := 'Memory';
  For i := 1 To 5 Do Begin
    StringGrid1.Cells[i, 0] := inttostr(i - 1);
  End;
  For i := 1 To 4 Do Begin
    StringGrid1.Cells[0, i] := inttostr((i - 1) * 5 + 100);
  End;
  StringGrid1.Cells[1, 1] := '3';
  StringGrid1.Cells[2, 1] := '1';
  StringGrid1.Cells[3, 1] := '2';
  StringGrid1.Cells[4, 1] := '3';
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
End;

Function TForm2.OperandToEdit(aOperand: String): TEdit;
Begin
  result := Nil;
  Case aOperand Of
    'A': result := Edit1;
    'B': result := Edit2;
    'C': result := Edit3;
    'D': result := Edit4;
  End;
End;

Procedure TForm2.VisualizeCmdLCL(Const aCMD: TAssemblerCMD);
Var
  x, y: integer;
  el, er: TEdit;
  aColor: TColor;
  s: String;
Begin
  Edit6.text := inttostr(aCMD.Line + 1);
  If Not Form1.CheckBox5.Checked Then Begin
    label7.caption := CMDToStr(aCMD.Cmd, aCMD.LeftOperand, aCMD.RightOperand);
    label8.caption := PipelineStepToStr(aCMD.PipelineStep);
  End
  Else Begin
    label7.caption := '';
    label8.caption := '';
  End;
  Case aCMD.PipelineStep Of
    psFetch: Begin
        If Form1.CheckBox5.Checked Then Begin
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
          cJMP, cJZ, cJNZ, cCALL: Begin
              If (aCMD.Cmd = cJMP)
                Or (aCMD.Cmd = cCALL)
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
          // All CMD's that go through the ALU
          cADD, cAnd, cCMP, cDIV, cMUL, cNot, cOr, cSHL, cSHR, cSUB, cXOR: Begin
              er := OperandToEdit(aCMD.RightOperand);
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
          cRET: Begin
              If ListBox1.Items.Count <> 0 Then Begin
                edit6.text := inttostr(
                  fCMDs[FindNextValidProgramLine(convertCodeLineToCMDIndex(strtoint(ListBox1.Items[0]) - 1), true)].Line + 1
                  );
              End
              Else Begin
                edit6.text := '0'; // Stack ist Empty -> Ungültig
              End;
              edit6.Font.Color := aColor;
              edit6.Font.Style := [fsBold];
            End;
          cCALL: Begin
              ListBox1.Items.Insert(0, Edit6.Text);
              edit6.text := inttostr(FindNextValidProgramLine(aCMD.JumpTarget) + 1);
              edit6.Font.Color := aColor;
              edit6.Font.Style := [fsBold];
              label5.Font.Color := aColor;
            End;
          cPUSH: Begin
              el := OperandToEdit(aCMD.LeftOperand);
              If Assigned(el) Then Begin
                ListBox1.Items.Insert(0, el.Text);
                el.Font.Color := aColor;
                el.Font.Style := [fsBold];
              End
              Else Begin
                ListBox1.Items.Insert(0, aCMD.LeftOperand);
              End;
              label5.Font.Color := aColor;
            End;
          cPOP: Begin
              el := OperandToEdit(aCMD.LeftOperand);
              If ListBox1.Items.Count <> 0 Then Begin
                el.text := ListBox1.Items[0];
                ListBox1.Items.Delete(0);
              End
              Else Begin
                el.text := '0';
              End;
              el.Font.Color := aColor;
              el.Font.Style := [fsBold];
              label5.Font.Color := aColor;
            End;
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
              er := OperandToEdit(aCMD.RightOperand);
              If assigned(er) Then Begin
                s := er.Text;
              End
              Else Begin
                s := aCMD.RightOperand;
              End;
              x := (strtoint(s) - 100) Mod 5 + 1;
              y := (strtoint(s) - 100) Div 5 + 1;
              el := OperandToEdit(aCMD.LeftOperand);
              el.text := inttostr(strtointdef(StringGrid1.Cells[x, y], 0));
              el.Font.Color := aColor;
              el.Font.Style := [fsBold];
            End;
          cMOV: Begin
              el := OperandToEdit(aCMD.LeftOperand);
              er := OperandToEdit(aCMD.RightOperand);
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
              el := OperandToEdit(aCMD.LeftOperand);
              er := OperandToEdit(aCMD.RightOperand);
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
          // All CMD's that go through the ALU
          cADD, cAND, cDIV, cMUL, cNot, cOr, cSHL, cSHR, cSub, cXOR: Begin
              el := OperandToEdit(aCMD.LeftOperand);
              er := OperandToEdit(aCMD.RightOperand);
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
          cRET: Begin
              If ListBox1.Items.Count <> 0 Then Begin
                edit6.text := inttostr(
                  fCMDs[FindNextValidProgramLine(convertCodeLineToCMDIndex(strtoint(ListBox1.Items[0]) - 1), true)].Line + 1
                  );
              End
              Else Begin
                edit6.text := '0'; // Stack ist Empty -> Ungültig
              End;
            End;
          cJMP, cJZ, cJNZ, cCALL: Begin
              If (aCMD.Cmd = cJMP)
                Or (aCMD.Cmd = cCALL)
                Or ((aCMD.Cmd = cJZ) And CheckBox1.Checked)
                Or ((aCMD.Cmd = cJNZ) And (Not CheckBox1.Checked))
                Then Begin
                edit6.Text := inttostr(FindNextValidProgramLine(aCMD.JumpTarget) + 1);
              End;
            End;
          cADD, cAND, cDIV, cNot, cOr, cMUL, cSHL, cSHR, cSub, cXOR: Begin
              el := OperandToEdit(aCMD.LeftOperand);
              er := OperandToEdit(aCMD.RightOperand);
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
                cDIV: label12.Caption := inttostr(strtointdef(label10.Caption, 0) Div strtointdef(label11.Caption, 0));
                cNot: label12.Caption := inttostr(Not strtointdef(label10.Caption, 0));
                cOr: label12.Caption := inttostr(strtointdef(label10.Caption, 0) Or strtointdef(label11.Caption, 0));
                cMul: label12.Caption := inttostr(strtointdef(label10.Caption, 0) * strtointdef(label11.Caption, 0));
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
          cRET: Begin
              // Der Pfeil Stack -> PC
              a.x := edit6.left + edit6.Width Div 4;
              a.y := edit6.top + edit6.Height + Scale96ToForm(8);
              c.x := ListBox1.Left - Scale96ToForm(8);
              c.y := ListBox1.Top;
              b.x := a.x;
              b.y := c.y;
              DrawLine(canvas, a, b, aColor);
              DrawLine(canvas, b, c, aColor);
              DrawArrowHead(Canvas, a, dUp, aColor);
            End;
          cCALL: Begin
              // Der Pfeil "aufgelöster" Jump to PC
              a.x := GroupBox1.Left - Scale96ToForm(8);
              a.y := GroupBox1.Top + Label15.top + GroupBox1.Height - GroupBox1.ClientHeight;
              b.x := edit6.left + edit6.Width * 3 Div 4;
              b.y := a.y;
              c.X := b.x;
              c.y := edit6.top + edit6.Height + Scale96ToForm(8);
              DrawLine(canvas, a, b, aColor);
              DrawLine(canvas, b, c, aColor);
              DrawArrowHead(Canvas, c, dUp, aColor);
              // Der Pfeil PC -> Stack
              a.x := edit6.left + edit6.Width Div 4;
              a.y := edit6.top + edit6.Height + Scale96ToForm(8);
              c.x := ListBox1.Left - Scale96ToForm(8);
              c.y := ListBox1.Top;
              b.x := a.x;
              b.y := c.y;
              DrawLine(canvas, a, b, aColor);
              DrawLine(canvas, b, c, aColor);
              DrawArrowHead(Canvas, c, dRight, aColor);
            End;
          cPUSH: Begin
              el := OperandToEdit(aCMD.LeftOperand);
              If assigned(el) Then Begin
                a := GetEditPoint(el);
              End
              Else Begin
                a.x := StringGrid1.Left;
                a.y := ListBox1.Top;
                canvas.Font.Color := aColor;
                canvas.Font.Style := [fsBold];
                canvas.Brush.Style := bsClear;
                canvas.TextOut(a.x - canvas.TextWidth(aCMD.LeftOperand) - Scale96ToForm(8), a.y, aCMD.LeftOperand);
                canvas.Font.Style := [];
                canvas.Font.Color := clBlack;
              End;
              c.X := ListBox1.Left - Scale96ToForm(8);
              c.Y := ListBox1.Top;
              b.x := a.x;
              b.y := c.y;
              DrawLine(canvas, a, b, aColor);
              DrawLine(canvas, b, c, aColor);
              DrawArrowHead(Canvas, c, dRight, aColor);
            End;
          cPop: Begin
              Case aCMD.LeftOperand Of
                'A': a := GetEditPoint(edit1);
                'B': a := GetEditPoint(edit2);
                'C': a := GetEditPoint(edit3);
                'D': a := GetEditPoint(edit4);
              End;
              c.X := ListBox1.Left - Scale96ToForm(8);
              c.Y := ListBox1.Top;
              b.x := a.x;
              b.y := c.y;
              DrawLine(canvas, a, b, aColor);
              DrawLine(canvas, b, c, aColor);
              DrawArrowHead(Canvas, a, dDown, aColor);
            End;
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
                    a := GetEditPoint(edit1);
                    c.y := StringGrid1.Top + Scale96ToForm(2);
                  End;
                'B': Begin
                    a := GetEditPoint(edit2);
                    c.y := StringGrid1.Top + Scale96ToForm(2 + StringGrid1.DefaultRowHeight Div 3);
                  End;
                'C': Begin
                    a := GetEditPoint(edit3);
                    c.y := StringGrid1.Top + Scale96ToForm(2 + (StringGrid1.DefaultRowHeight * 2) Div 3);
                  End;
                'D': Begin
                    a := GetEditPoint(edit4);
                    c.y := StringGrid1.Top + Scale96ToForm(2 + StringGrid1.DefaultRowHeight);
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
          cMOV: Begin
              Case aCMD.LeftOperand Of
                'A': a := GetEditPoint(edit1);
                'B': a := GetEditPoint(edit2);
                'C': a := GetEditPoint(edit3);
                'D': a := GetEditPoint(edit4);
              End;
              b := a;
              b.y := a.y - Canvas.TextHeight('B');
              If (length(aCMD.RightOperand) = 1) And (aCMD.RightOperand[1] In ['A'..'D']) Then Begin
                Case aCMD.RightOperand Of
                  'A': d := GetEditPoint(edit1);
                  'B': d := GetEditPoint(edit2);
                  'C': d := GetEditPoint(edit3);
                  'D': d := GetEditPoint(edit4);
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
                    a := GetEditPoint(edit1);
                    c.y := StringGrid1.Top + Scale96ToForm(2);
                  End;
                'B': Begin
                    a := GetEditPoint(edit2);
                    c.y := StringGrid1.Top + Scale96ToForm(2 + StringGrid1.DefaultRowHeight Div 3);
                  End;
                'C': Begin
                    a := GetEditPoint(edit3);
                    c.y := StringGrid1.Top + Scale96ToForm(2 + (StringGrid1.DefaultRowHeight * 2) Div 3);
                  End;
                'D': Begin
                    a := GetEditPoint(edit4);
                    c.y := StringGrid1.Top + Scale96ToForm(2 + StringGrid1.DefaultRowHeight);
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

