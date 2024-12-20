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
Unit Unit3;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Forms, Classes, Controls, StdCtrls, LResources;

Type
  TForm3 = Class(TForm)
    CheckBox1: TCheckBox;
    Button1: TButton;
    CheckBox2: TCheckBox;
    Procedure FormCreate(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

Var
  Form3: TForm3;

Implementation

Uses Unit1;

{$R *.lfm}

Procedure TForm3.FormCreate(Sender: TObject);
Begin
  Caption := 'Sudoku ver. : ' + ver + ' Options';
End;

Procedure TForm3.Button1Click(Sender: TObject);
Begin
  invalidnallow := checkbox2.checked;
  unpencilallow := checkbox1.checked;
  close;
End;

End.

