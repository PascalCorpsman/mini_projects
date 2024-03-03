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
Unit Unit7;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

Type

  { TForm7 }

  TForm7 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    ListBox1: TListBox;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  End;

Var
  Form7: TForm7;
  form7result: Boolean;

Implementation

{$R *.lfm}

{ TForm7 }

Procedure TForm7.Button4Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm7.FormCreate(Sender: TObject);
Begin
End;

Procedure TForm7.Button3Click(Sender: TObject);
Begin
  form7result := true;
  close;
End;

Procedure TForm7.Button1Click(Sender: TObject);
Begin
  If SelectDirectoryDialog1.Execute Then Begin
    ListBox1.Items.Add(SelectDirectoryDialog1.FileName);
  End;
End;

Procedure TForm7.Button2Click(Sender: TObject);
Begin
  If ListBox1.ItemIndex <> -1 Then Begin
    listbox1.Items.Delete(ListBox1.ItemIndex);
  End;
End;

End.

