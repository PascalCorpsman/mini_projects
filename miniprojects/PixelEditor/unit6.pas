(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of PixelEditor                                           *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit6;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  Spin;

Type

  { TForm6 }

  TForm6 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    SpinEdit5: TFloatSpinEdit;
    SpinEdit6: TFloatSpinEdit;
    Procedure FormCreate(Sender: TObject);
    Procedure SpinEdit3Change(Sender: TObject);
    Procedure SpinEdit4Change(Sender: TObject);
    Procedure SpinEdit5Change(Sender: TObject);
    Procedure SpinEdit6Change(Sender: TObject);
  private
    Block: Boolean;
  public
    Procedure InitWith(aWidth, aHeight: Integer; Global: Boolean);

  End;

Var
  Form6: TForm6;

Implementation

{$R *.lfm}

Uses math;

{ TForm6 }

Procedure TForm6.FormCreate(Sender: TObject);
Begin
  caption := 'Resize / Scale';
End;

Procedure TForm6.SpinEdit3Change(Sender: TObject);
Begin
  // Change on New Width
  If block Then exit;
  block := true;
  SpinEdit5.Value := SpinEdit3.Value * 100 / SpinEdit1.Value;
  If CheckBox1.Checked Then Begin
    SpinEdit4.Value := round(SpinEdit3.Value * SpinEdit2.Value / max(1, SpinEdit1.Value));
    SpinEdit6.Value := SpinEdit4.Value * 100 / max(1, SpinEdit2.Value);
  End;
  block := false;
End;

Procedure TForm6.SpinEdit4Change(Sender: TObject);
Begin
  // Change on New Height
  If block Then exit;
  block := true;
  SpinEdit6.Value := SpinEdit4.Value * 100 / SpinEdit2.Value;
  If CheckBox1.Checked Then Begin
    SpinEdit3.Value := round(SpinEdit4.Value * SpinEdit1.Value / max(1, SpinEdit2.Value));
    SpinEdit5.Value := SpinEdit3.Value * 100 / max(1, SpinEdit1.Value);
  End;
  block := false;
End;

Procedure TForm6.SpinEdit5Change(Sender: TObject);
Begin
  // Change on New Width Percent
  If block Then exit;
  block := true;
  SpinEdit3.Value := (SpinEdit1.Value * SpinEdit5.Value) / 100;
  If CheckBox1.Checked Then Begin
    SpinEdit4.Value := round(SpinEdit3.Value * SpinEdit2.Value / max(1, SpinEdit1.Value));
    SpinEdit6.Value := SpinEdit4.Value * 100 / max(1, SpinEdit2.Value);
  End;
  block := false;
End;

Procedure TForm6.SpinEdit6Change(Sender: TObject);
Begin
  // Change on New Height Percent
  If block Then exit;
  block := true;
  SpinEdit4.Value := (SpinEdit2.Value * SpinEdit6.Value) / 100;
  If CheckBox1.Checked Then Begin
    SpinEdit3.Value := round(SpinEdit4.Value * SpinEdit1.Value / max(1, SpinEdit2.Value));
    SpinEdit5.Value := SpinEdit3.Value * 100 / max(1, SpinEdit1.Value);
  End;
  block := false;
End;

Procedure TForm6.InitWith(aWidth, aHeight: Integer; Global: Boolean);
Begin
  CheckBox1.Checked := false; // Damit man die Werte in Ruhe setzen kann
  SpinEdit1.Value := aWidth;
  SpinEdit3.Value := aWidth;
  SpinEdit5.Value := 100;
  SpinEdit2.Value := aHeight;
  SpinEdit4.Value := aHeight;
  SpinEdit6.Value := 100;
  ComboBox1.ItemIndex := 1 - ord(Global);
  CheckBox1.Checked := true;
End;

End.

