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
  ExtCtrls, SynEdit, SynHighlighterAny, uFPC_CPU;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
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
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ListBox1: TListBox;
    StringGrid1: TStringGrid;
    SynAnySyn1: TSynAnySyn;
    SynEdit1: TSynEdit;
    Timer1: TTimer;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormPaint(Sender: TObject);
  private
    fCMDs: TAssemblerCMDs;
    aCMDIndex: integer;
  public
    Procedure ResetLCLToCompile;
    Procedure SetLCLToExecute;
    Procedure ResetCMDVisualizations;
    Procedure VisualizeCmd(Const aCMD: TAssemblerCMD; aColor: TColor);

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Var
  i: Integer;
Begin
  (*
   * TODO:
   *  - Haltepunkte
   *  - Visualisieren des PC im Synedit während der Simulation
   *  - "Compile"
   *  - Visualisieren der J* Befehle
   *  - Implementieren der fehlenden Befehle
   *  - STACK, via Push Pop
   *  - Subfunctions via CALL ( und seinem Gegenstück ?)
   *  - Laden / Speichern eines Programms (Achtung inclusive Memory!)
   *  - Die Flags Sinnvol auswerten / Benutzen (da fehlen noch entsprechende Jump Befehle)
   *  - Pipelining ;)
   *)
  caption := 'FPC_CPU ver 0.01 by Corpsman, www.Corpsman.de';
  StringGrid1.Cells[0, 0] := 'Memory';
  For i := 1 To 5 Do Begin
    StringGrid1.Cells[i, 0] := inttostr(i - 1);
  End;
  For i := 1 To 4 Do Begin
    StringGrid1.Cells[0, i] := inttostr((i - 1) * 5 + 100);
  End;
  Edit7.text := '1000';
  ResetLCLToCompile;
  StringGrid1.Cells[1, 1] := '1';
End;

Procedure TForm1.FormPaint(Sender: TObject);
Begin
  If (aCMDIndex >= 0) And (aCMDIndex <= high(fCMDs)) Then Begin
    VisualizeCmd(fCMDs[aCMDIndex], clBlack);
  End;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  // Compile
  // TODO: "Compiler" schreiben ;)
  aCMDIndex := 0;
  setlength(fCMDs, 13);

  fcmds[0].Cmd := cLOAD;
  fcmds[0].LeftOperand := 'A';
  fcmds[0].RightOperand := '100';
  fcmds[0].Line := 4;
  fcmds[0].PipelineStep := psFetch;

  fcmds[1].Cmd := cADD;
  fcmds[1].LeftOperand := 'A';
  fcmds[1].RightOperand := '1';
  fcmds[1].Line := 5;
  fcmds[1].PipelineStep := psFetch;

  fcmds[2].Cmd := cMOV;
  fcmds[2].LeftOperand := 'B';
  fcmds[2].RightOperand := '0';
  fcmds[2].Line := 7;
  fcmds[2].PipelineStep := psFetch;

  fcmds[3].Cmd := cMOV;
  fcmds[3].LeftOperand := 'C';
  fcmds[3].RightOperand := '1';
  fcmds[3].Line := 8;
  fcmds[3].PipelineStep := psFetch;

  fcmds[4].Cmd := cLabel;
  fcmds[4].LeftOperand := 'LOOP';
  fcmds[4].RightOperand := '';
  fcmds[4].Line := 10;
  fcmds[4].PipelineStep := psFetch;

  fcmds[5].Cmd := cCMP;
  fcmds[5].LeftOperand := 'C';
  fcmds[5].RightOperand := 'A';
  fcmds[5].Line := 11;
  fcmds[5].PipelineStep := psFetch;

  fcmds[6].Cmd := cJZ;
  fcmds[6].LeftOperand := 'END';
  fcmds[6].RightOperand := '';
  fcmds[6].Line := 12;
  fcmds[6].JumpTarget := 18;
  fcmds[6].PipelineStep := psFetch;

  fcmds[7].Cmd := cADD;
  fcmds[7].LeftOperand := 'B';
  fcmds[7].RightOperand := 'C';
  fcmds[7].Line := 14;
  fcmds[7].PipelineStep := psFetch;

  fcmds[8].Cmd := cADD;
  fcmds[8].LeftOperand := 'C';
  fcmds[8].RightOperand := '1';
  fcmds[8].Line := 15;
  fcmds[8].PipelineStep := psFetch;

  fcmds[9].Cmd := cJMP;
  fcmds[9].LeftOperand := 'LOOP';
  fcmds[9].RightOperand := '';
  fcmds[9].Line := 16;
  fcmds[9].JumpTarget := 10;
  fcmds[9].PipelineStep := psFetch;

  fcmds[10].Cmd := cLabel;
  fcmds[10].LeftOperand := 'END';
  fcmds[10].RightOperand := '';
  fcmds[10].Line := 18;
  fcmds[10].PipelineStep := psFetch;

  fcmds[11].Cmd := cSTORE;
  fcmds[11].LeftOperand := 'B';
  fcmds[11].RightOperand := '101';
  fcmds[11].Line := 19;
  fcmds[11].PipelineStep := psFetch;

  fcmds[12].Cmd := cHLT;
  fcmds[12].LeftOperand := '';
  fcmds[12].RightOperand := '';
  fcmds[12].Line := 20;
  fcmds[12].PipelineStep := psFetch;

  SetLCLToExecute;
  Refresh;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Var
  x, y, i: Integer;
  el: TEdit;
Begin
  // Step
  If (aCMDIndex < 0) Or (aCMDIndex > high(fCMDs)) Then Begin
    ResetLCLToCompile;
    exit;
  End;
  Case fcmds[aCMDIndex].PipelineStep Of
    psFetch: fcmds[aCMDIndex].PipelineStep := psDecode;
    psDecode: fcmds[aCMDIndex].PipelineStep := psExecute;
    psExecute: fcmds[aCMDIndex].PipelineStep := psWriteBack;
    psWriteBack: Begin
        Case fcmds[aCMDIndex].Cmd Of
          cHLT: Begin
              showmessage('Finished.');
              ResetLCLToCompile;
              exit;
            End;
          cJMP: Begin
              For i := 0 To high(fCMDs) Do Begin
                If fCMDs[i].Line = fCMDs[aCMDIndex].JumpTarget Then Begin
                  aCMDIndex := i;
                  break;
                End;
              End;
            End;
          cJZ: Begin
              If CheckBox1.Checked Then Begin
                For i := 0 To high(fCMDs) Do Begin
                  If fCMDs[i].Line = fCMDs[aCMDIndex].JumpTarget Then Begin
                    aCMDIndex := i;
                    break;
                  End;
                End;
              End;
            End;
          cADD: Begin
              el := Nil;
              Case fcmds[aCMDIndex].LeftOperand Of
                'A': el := Edit1;
                'B': el := Edit2;
                'C': el := Edit3;
                'D': el := Edit4;
              End;
              el.text := label12.caption;
            End;
          cSTORE: Begin
              // Decode right Operand to x,y in Stringgrid
              x := (strtoint(fcmds[aCMDIndex].RightOperand) - 100) Mod 5 + 1;
              y := (strtoint(fcmds[aCMDIndex].RightOperand) - 100) Div 5 + 1;
              el := Nil;
              Case fcmds[aCMDIndex].LeftOperand Of
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
        aCMDIndex := aCMDIndex + 1;
        // We overread Labels, as they do not exist in real machine code.
        While fcmds[aCMDIndex].Cmd = cLabel Do Begin
          aCMDIndex := aCMDIndex + 1;
        End;
        fcmds[aCMDIndex].PipelineStep := psFetch;
      End;
  End;
  ResetCMDVisualizations;
  //  Invalidate; // Which one is better ?
  Refresh; // Which one is better ?
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  If Timer1.Enabled Then Begin
    Timer1.Enabled := false;
    button3.caption := 'Auto step [ms]';
  End
  Else Begin
    timer1.interval := strtointdef(edit7.text, 1000);
    Timer1.Enabled := true;
    button3.caption := 'Stop';
  End;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Begin
  // Halt
  ResetLCLToCompile;
End;

Procedure TForm1.ResetLCLToCompile;
Begin
  Button1.Enabled := true;
  Button2.Enabled := false;
  Button3.Enabled := false;
  Button4.Enabled := false;
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
  aCMDIndex := -1;
  CheckBox1.Checked := false;
  CheckBox2.Checked := false;
  CheckBox3.Checked := false;
  CheckBox4.Checked := false;
  Timer1.Enabled := false;
  button3.caption := 'Auto step [ms]';
  Refresh;
End;

Procedure TForm1.SetLCLToExecute;
Begin
  Button1.Enabled := false;
  Button2.Enabled := true;
  Button3.Enabled := true;
  Button4.Enabled := true;
  edit7.enabled := true;
  SynEdit1.ReadOnly := true;
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
  edit1.Font.Color := clBlack;
  edit1.Font.Style := [];
  edit2.Font.Color := clBlack;
  edit2.Font.Style := [];
  edit3.Font.Color := clBlack;
  edit3.Font.Style := [];
  edit4.Font.Color := clBlack;
  edit4.Font.Style := [];
  CheckBox1.Font.Color := clBlack;
  CheckBox1.Font.Style := [];
  CheckBox2.Font.Color := clBlack;
  CheckBox2.Font.Style := [];
  CheckBox3.Font.Color := clBlack;
  CheckBox3.Font.Style := [];
  CheckBox4.Font.Color := clBlack;
  CheckBox4.Font.Style := [];
End;

Procedure TForm1.VisualizeCmd(Const aCMD: TAssemblerCMD; aColor: TColor);
Var
  a, b, c, d, e, f: TPoint;
  x, y: integer;
  el, er: TEdit;
  cl, cr: Integer;
Begin
  (*
   * TODO: Wie die Jump's visualisieren ?
   *)
  Edit6.text := inttostr(aCMD.Line + 1);
  label8.caption := PipelineStepToStr(aCMD.PipelineStep);
  label7.caption := CMDToStr(aCMD.Cmd, aCMD.LeftOperand, aCMD.RightOperand);
  Case aCMD.PipelineStep Of
    psFetch: Begin
        // Load CMD from Program Memory into Decoder
        a.x := (SynEdit1.Left + SynEdit1.Width) + Scale96ToForm(8);
        a.Y := GroupBox1.Top + GroupBox1.Height Div 2;
        b.x := GroupBox1.Left - Scale96ToForm(8);
        b.Y := GroupBox1.Top + GroupBox1.Height Div 2;
        DrawLine(canvas, a, b, aColor);
        DrawArrowHead(Canvas, b, dRight, aColor);
        label7.Font.Color := aColor;
        label7.Font.Style := [fsBold];
      End;
    psDecode: Begin
        Case aCMD.Cmd Of
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
                label13.caption := aCMD.RightOperand;
                label13.Font.Color := aColor;
                label13.Font.Style := [fsBold];
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
        Case aCMD.Cmd Of
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
              el.text := aCMD.RightOperand;
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
              label11.Caption := er.Text;
              label11.Font.Color := aColor;
              label11.Font.Style := [fsBold];
              a.x := Image1.Left + Image1.Width + Scale96ToForm(8);
              a.Y := Image1.Top + Image1.Height Div 2;
              b.x := CheckBox1.Left - Scale96ToForm(8);
              b.Y := a.y;
              DrawLine(canvas, a, b, aColor);
              DrawArrowHead(Canvas, b, dRight, aColor);

              cl := strtointdef(Label10.Caption, 0);
              cr := strtointdef(Label11.Caption, 0);
              // Zero Flag (ZF): Wird auf 1 gesetzt, wenn beide Register den exakt
              //                 gleichen Wert haben (da das Ergebnis der Subtraktion 0 ist).
              //                 Andernfalls ist es 0.
              CheckBox1.Checked := cl = cr;
              CheckBox1.Font.Color := aColor;
              CheckBox1.Font.Style := [fsBold];

              // Carry Flag (CF): Wird als "Borrow" (Übertrag) bei der Subtraktion verwendet.
              //                  Es wird auf 1 gesetzt, wenn der Wert des zweiten Registers
              //                  (Subtrahend) größer ist als der Wert des ersten Registers
              //                  (Minuend).
              CheckBox2.Checked := cl < cr;
              CheckBox2.Font.Color := aColor;
              CheckBox2.Font.Style := [fsBold];

              // Negative Flag: Todo definieren
              // TODO: Implement Negative Flag
              CheckBox3.Checked := false;
              CheckBox3.Font.Color := aColor;
              CheckBox3.Font.Style := [fsBold];

              // Overflow Flag (OF): Zeigt einen arithmetischen Überlauf an. Es wird auf 1
              //                     gesetzt, wenn das Vorzeichen des Ergebnisses falsch ist
              //                     (z.B. wenn man von einer sehr großen negativen Zahl eine
              //                           positive Zahl abzieht und das Register überläuft).
              // TODO: Implement Overflow Flag
              CheckBox4.Checked := false;
              CheckBox4.Font.Color := aColor;
              CheckBox4.Font.Style := [fsBold];

              // Sign Flag (SF): Entspricht dem höchstwertigen Bit des Subtraktionsergebnisses. Ist das Ergebnis negativ (das zweite Register ist größer), wird das Flag auf 1 gesetzt, andernfalls auf 0.
              // Parity Flag (PF): Gibt an, ob die Anzahl der gesetzten Bits (1en) im niedrigsten Byte des Ergebnisses gerade oder ungerade ist.Auxiliary
              // Carry Flag (AF): Wird bei BCD-Arithmetik (Binary Coded Decimal) verwendet und zeigt einen Übertrag oder Untertrag vom niederwertigen zum höherwertigen Halb-Byte (Nibble) an.
            End;
          cADD: Begin
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
        // Needs to be done in "Step", as refresh can be fired to often..
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
          cADD: Begin
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
              Case aCMD.Cmd Of
                cADD: label12.Caption := inttostr(strtointdef(label10.Caption, 0) + strtointdef(label11.Caption, 0));
              End;
              label12.Font.Color := aColor;
              label12.Font.Style := [fsBold];
            End;
        End;
      End;
  End;
End;

End.

