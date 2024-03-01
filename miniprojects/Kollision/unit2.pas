(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Kollision                                             *)
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
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, uHighscoreEngine;
Type

  { TForm2 }

  TForm2 = Class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Procedure Button1Click(Sender: TObject);
    Procedure Edit1KeyPress(Sender: TObject; Var Key: char);
  private
    { private declarations }
  public
    { public declarations }
  End;

Var
  Form2: TForm2;

Implementation

{$R *.lfm}

Uses unit1;

{ TForm2 }

Function TrimmLen(Len: integer; Fillchar, value: String): String;
Begin
  While Length(value) < len Do
    Value := Fillchar + Value;
  result := value;
End;

Procedure TForm2.Button1Click(Sender: TObject);
Var
  items: TItemList;
  s: String;
  i: Integer;
Begin
  Highscores.add(Edit1.text, strtoint(label3.caption));
  Highscores.Save;
  items := Highscores.Show(false);
  s := '';
  For i := 0 To high(items) Do Begin
    s := s + items[i].name + ' : ' + inttostr(items[i].points Div 1000) + ',' + TrimmLen(3, '0', inttostr(items[i].points Mod 1000)) + #13;
  End;
  setlength(items, 0);
  showmessage(s);
  close;
End;

Procedure TForm2.Edit1KeyPress(Sender: TObject; Var Key: char);
Begin
  If key = #13 Then button1click(Nil);
End;

End.

