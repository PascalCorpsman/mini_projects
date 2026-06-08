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
(*
 *
 *)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids,
  SynEdit, SynHighlighterAny;

Type

  { TForm1 }

  TForm1 = Class(TForm)
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
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    StringGrid1: TStringGrid;
    SynAnySyn1: TSynAnySyn;
    SynEdit1: TSynEdit;
    Procedure FormCreate(Sender: TObject);
  private

  public

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

End.

