(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Superdom                                              *)
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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, ExtCtrls;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    RadioGroup1: TRadioGroup;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    SpinEdit5: TSpinEdit;
    SpinEdit6: TSpinEdit;
    SpinEdit7: TSpinEdit;
    Procedure Button3Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  End;

Var
  Form2: TForm2;

Implementation

{$R *.lfm}

{ TForm2 }

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  caption := 'Game settings';
  Tform(self).Constraints.MaxHeight := Tform(self).Height;
  Tform(self).Constraints.MinHeight := Tform(self).Height;
  Tform(self).Constraints.Maxwidth := Tform(self).width;
  Tform(self).Constraints.Minwidth := Tform(self).width;
End;

Procedure TForm2.Button3Click(Sender: TObject);
Begin
  SpinEdit1.Value := 1;
  SpinEdit2.Value := 1;
  SpinEdit3.Value := 2;
  SpinEdit4.Value := 2;
  SpinEdit5.Value := 0;
  SpinEdit6.Value := 0;
  SpinEdit7.Value := 2;
End;

End.

