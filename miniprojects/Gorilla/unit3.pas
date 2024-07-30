(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Gorilla                                               *)
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
    Edit1: TEdit;
    ListBox1: TListBox;
    Procedure Button1Click(Sender: TObject);
    Procedure Edit1KeyPress(Sender: TObject; Var Key: char);
    Procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  End;

Var
  Form3: TForm3;

Procedure ReceiveChatMessage(Message: String);

Implementation

{$R *.lfm}

Uses
  unit1,
  ugorilla;

{ TForm3 }

Procedure ReceiveChatMessage(Message: String);
Begin
  form3.listbox1.Items.add(message);
  form3.listbox1.ItemIndex := form3.listbox1.Items.count - 1;
  If Not form3.visible Then
    form3.show;
End;

Procedure TForm3.FormCreate(Sender: TObject);
Begin
  caption := 'Chatroom';
  edit1.text := '';
End;

Procedure TForm3.Button1Click(Sender: TObject);
Begin
  If length(edit1.text) = 0 Then exit;
  Gorilla.SendMessage(opChatMessage, gorilla.chatname + ': ' + edit1.text);
  listbox1.Items.add(gorilla.chatname + ': ' + edit1.text);
  listbox1.ItemIndex := listbox1.Items.count - 1;
  edit1.text := '';
End;

Procedure TForm3.Edit1KeyPress(Sender: TObject; Var Key: char);
Begin
  If key = #13 Then Button1.OnClick(Nil);
End;

End.

