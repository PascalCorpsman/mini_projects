(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Jazz                                                  *)
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

{$MODE objfpc}{$H+}

Interface

Uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ujazz, math;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    Button1: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Label4: TLabel;
    Edit3: TEdit;
    Label5: TLabel;
    Edit4: TEdit;
    Label6: TLabel;
    Edit5: TEdit;
    CheckBox1: TCheckBox;
    Label7: TLabel;
    Label8: TLabel;
    CheckBox2: TCheckBox;
    Label9: TLabel;
    CheckBox3: TCheckBox;
    Edit6: TEdit;
    Label10: TLabel;
    Edit7: TEdit;
    Label11: TLabel;
    Edit8: TEdit;
    Label12: TLabel;
    Edit9: TEdit;
    Label13: TLabel;
    Edit10: TEdit;
    Label14: TLabel;
    Label15: TLabel;
    Edit11: TEdit;
    Edit12: TEdit;
    Label16: TLabel;
    Procedure Button1Click(Sender: TObject);
    Procedure Edit1KeyPress(Sender: TObject; Var Key: Char);
    Procedure Edit4KeyPress(Sender: TObject; Var Key: Char);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

Var
  Form2: TForm2;

Implementation

{$R *.lfm}

Procedure TForm2.Button1Click(Sender: TObject);
Begin
  screenwidth := strtoint(Edit1.text);
  screenheight := strtoint(Edit2.text);
  BallRotatespeed := strtoint(Edit3.text);
  Ballspeed := strtofloat(edit4.text);
  Minball := max(1, strtoint(Edit5.text));
  windowscursor := checkbox1.checked;
  sinnlos := checkbox2.checked;
  ballcollision := checkbox3.checked;
  maxtime := strtoint(edit6.text);
  sprung := strtoint(edit7.text);
  barspeed := strtoint(edit8.text);
  autosprung := strtoint(edit9.text);
  Bezeichnung := edit10.text;
  Ballspeedinc2 := strtofloat(edit11.text);
  Teiler := strtoint(edit12.text);
  close;
End;

Procedure TForm2.Edit1KeyPress(Sender: TObject; Var Key: Char);
Begin
  If Not (key In ['0'..'9', #8]) Then key := #0;
End;

Procedure TForm2.Edit4KeyPress(Sender: TObject; Var Key: Char);
Begin
  If Key = '.' Then Key := ',';
  If Not (key In ['0'..'9', ',', #8]) Then key := #0;
End;

End.

