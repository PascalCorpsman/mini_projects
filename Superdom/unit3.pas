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
Unit Unit3;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

Type

  { TForm3 }

  TForm3 = Class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button11: TButton;
    Button12: TButton;
    Button13: TButton;
    Button14: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Edit1: TEdit;
    Procedure Button10Click(Sender: TObject);
    Procedure Button11Click(Sender: TObject);
    Procedure Button12Click(Sender: TObject);
    Procedure Button13Click(Sender: TObject);
    Procedure Button14Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure Button8Click(Sender: TObject);
    Procedure Button9Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  End;

Var
  Form3: TForm3;

Implementation

{$R *.lfm}

{ TForm3 }

Procedure TForm3.FormCreate(Sender: TObject);
Begin
  Tform(self).Constraints.MaxHeight := Tform(self).Height;
  Tform(self).Constraints.MinHeight := Tform(self).Height;
  Tform(self).Constraints.Maxwidth := Tform(self).width;
  Tform(self).Constraints.Minwidth := Tform(self).width;
  caption := 'Edit Number';
End;

Procedure TForm3.Button12Click(Sender: TObject);
Begin
  edit1.text := '';
End;

Procedure TForm3.Button13Click(Sender: TObject);
Begin
  Edit1.text := edit1.text + '0';
End;

Procedure TForm3.Button14Click(Sender: TObject);
Begin
  If length(edit1.text) > 0 Then Begin
    edit1.text := copy(edit1.text, 1, length(edit1.text) - 1);
  End;
End;

Procedure TForm3.Button10Click(Sender: TObject);
Begin
  Edit1.text := edit1.text + '8';
End;

Procedure TForm3.Button11Click(Sender: TObject);
Begin
  Edit1.text := edit1.text + '9';
End;

Procedure TForm3.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm3.Button3Click(Sender: TObject);
Begin
  Edit1.text := edit1.text + '1';
End;

Procedure TForm3.Button4Click(Sender: TObject);
Begin
  Edit1.text := edit1.text + '2';
End;

Procedure TForm3.Button5Click(Sender: TObject);
Begin
  Edit1.text := edit1.text + '3';
End;

Procedure TForm3.Button6Click(Sender: TObject);
Begin
  Edit1.text := edit1.text + '4';
End;

Procedure TForm3.Button7Click(Sender: TObject);
Begin
  Edit1.text := edit1.text + '5';
End;

Procedure TForm3.Button8Click(Sender: TObject);
Begin
  Edit1.text := edit1.text + '6';
End;

Procedure TForm3.Button9Click(Sender: TObject);
Begin
  Edit1.text := edit1.text + '7';
End;

End.

