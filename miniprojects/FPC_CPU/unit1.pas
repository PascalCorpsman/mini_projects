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
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    StringGrid1: TStringGrid;
    SynAnySyn1: TSynAnySyn;
    SynEdit1: TSynEdit;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
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

  (*
  1. Fetch
    PC -> Memory
    Instruction geladen

  2. Decode
    Control Unit entscheidet:
    welche Register?
    ALU-Operation?

  3. Execute
    Daten bewegen sich
    ALU rechnet

  4. Writeback
    Ergebnis zurück ins Register
  *)
End;

Procedure TForm1.FormPaint(Sender: TObject);
Begin
  ResetCMDVisualizations;
  If (aCMDIndex >= 0) And (aCMDIndex <= high(fCMDs)) Then Begin
    VisualizeCmd(fCMDs[aCMDIndex], clBlack);
  End;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  // Compile
  // TODO: "Compiler" schreiben ;)
  aCMDIndex := 0;
  setlength(fCMDs, 12);

  fcmds[0].Cmd := cLOAD;
  fcmds[0].LeftOperand := 'A';
  fcmds[0].RightOperand := '100';
  fcmds[0].Line := 4;
  fcmds[0].PipelineStep := psFetch;

  fcmds[1].Cmd := cMOV;
  fcmds[1].LeftOperand := 'B';
  fcmds[1].RightOperand := '0';
  fcmds[1].Line := 6;
  fcmds[1].PipelineStep := psFetch;

  fcmds[2].Cmd := cMOV;
  fcmds[2].LeftOperand := 'C';
  fcmds[2].RightOperand := '1';
  fcmds[2].Line := 7;
  fcmds[2].PipelineStep := psFetch;

  SetLCLToExecute;
  Refresh;

End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  // Step
  If
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
  label9.caption := '';

End;

Procedure TForm1.VisualizeCmd(Const aCMD: TAssemblerCMD; aColor: TColor);
Begin
  Edit6.text := inttostr(aCMD.Line + 1);
  label8.caption := PipelineStepToStr(aCMD.PipelineStep);
  Case aCMD.PipelineStep Of
    psFetch: Begin

      End;
    psDecode: Begin

      End;
    psExecute: Begin

      End;
    psWriteBack: Begin

      End;
  End;
End;

End.

