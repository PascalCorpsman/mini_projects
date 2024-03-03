(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of KLab                                                  *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit4;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

Type

  { TForm4 }

  TForm4 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SpeedButton1: TSpeedButton;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure ComboBox1Change(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure SpeedButton1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  End;

  TForm4Mode = (f4mupload, f4mdownload, f4merase, f4mverify);

Var
  Form4: TForm4;
  Form4Mode: TForm4Mode;

Implementation

{$R *.lfm}

Uses uklab, lazutf8, LazFileUtils;

Var
  cp: tcpu;

  { TForm4 }

Procedure TForm4.Button1Click(Sender: TObject);
Begin
  Case Form4Mode Of
    f4mupload: Begin
        // Upload
        ExecuteCommand(CreateProgrammCommand(Edit1.text, cp));
        If CheckBox1.Checked Then
          close;
      End;
    f4mdownload: Begin
        // Download
        If SaveDialog1.Execute Then Begin
          ExecuteCommand(CreateLoadProgrammCommand(systoutf8(SaveDialog1.FileName), cp));
          close;
        End;
      End;
    f4merase: Begin
        // Erase
        ExecuteCommand(CreateEraseCommand(cp));
        If CheckBox1.Checked Then
          close;
      End;
    f4mverify: Begin
        // Verify
        ExecuteCommand(CreateVerifyProgrammCommand(Edit1.text, cp));
        If CheckBox1.Checked Then
          close;
      End;
  End;
End;

Procedure TForm4.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm4.ComboBox1Change(Sender: TObject);
Begin
  cp := uklab.loadcpu(ComboBox1.Text);
End;

Procedure TForm4.FormCreate(Sender: TObject);
Begin
  OpenDialog1.InitialDir := AppPath;
  SaveDialog1.InitialDir := AppPath;
  edit1.text := '';
  Tform(self).Constraints.MaxHeight := Tform(self).Height;
  Tform(self).Constraints.MinHeight := Tform(self).Height;
  Tform(self).Constraints.Maxwidth := Tform(self).width;
  Tform(self).Constraints.Minwidth := Tform(self).width;
End;

Procedure TForm4.SpeedButton1Click(Sender: TObject);
Begin
  If OpenDialog1.Execute Then
    Edit1.Text := systoutf8(OpenDialog1.FileName);

End;

End.

