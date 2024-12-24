(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Sudoku                                                *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit10;

{$MODE ObjFPC}{$H+}

Interface

Uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Printers, LResources;

Type
  TForm10 = Class(TForm)
    ComboBox1: TComboBox;
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Edit1: TEdit;
    Label3: TLabel;
    ScrollBar1: TScrollBar;
    Label4: TLabel;
    Procedure Button1Click(Sender: TObject);
    Procedure Edit1KeyPress(Sender: TObject; Var Key: Char);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

Var
  Form10: TForm10;

Implementation

{$R *.lfm}

Uses usudoku;

Procedure TForm10.Button1Click(Sender: TObject);
Begin
  Druckbreite := scrollbar1.Position;
  ModalResult := mrOK;
End;

Procedure TForm10.Edit1KeyPress(Sender: TObject; Var Key: Char);
Begin
  If Not (key In ['0'..'9', #8]) Then key := #0;
End;

End.

