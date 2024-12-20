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
Unit Unit15;

{$MODE ObjFPC}{$H+}

Interface

Uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, LResources;

Type
  TForm15 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    Button3: TButton;
    Button4: TButton;
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

Var
  Form15: TForm15;
  CreaterOptions: integer;

Implementation

{$R *.lfm}

Procedure TForm15.Button2Click(Sender: TObject);
Begin
  CreaterOptions := -1;
  Close;
End;

Procedure TForm15.Button3Click(Sender: TObject);
Var
  x: integer;
Begin
  For x := 1 To 9 Do
    Tcheckbox(findcomponent('Checkbox' + inttostr(x))).checked := true;
End;

Procedure TForm15.Button4Click(Sender: TObject);
Var
  x: integer;
Begin
  For x := 1 To 9 Do
    Tcheckbox(findcomponent('Checkbox' + inttostr(x))).checked := false;
End;

Function exp(Value: integer): integer;
Var
  x, erg: INteger;
Begin
  erg := 1;
  For x := 1 To Value - 1 Do
    erg := erg * 2;
  result := erg;
End;

Procedure TForm15.Button1Click(Sender: TObject);
Var
  x: integer;
Begin
  CreaterOptions := 0;
  For x := 1 To 9 Do
    If Tcheckbox(findcomponent('Checkbox' + inttostr(x))).checked Then inc(CreaterOptions, exp(x));
  close;
End;

End.

