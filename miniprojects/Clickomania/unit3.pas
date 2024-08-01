(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Clickomania                                           *)
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

{$MODE objfpc}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  uclickomania;

Type

  { TForm3 }

  TForm3 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Edit1KeyPress(Sender: TObject; Var Key: char);
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

{$I clickomania.inc}

Uses unit1;

{ TForm3 }

Procedure TForm3.FormCreate(Sender: TObject);
Begin
  caption := 'This is a new highscore!';
  Edit1.text := '';
End;

Procedure TForm3.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm3.Edit1KeyPress(Sender: TObject; Var Key: char);
Begin
  If key = #13 Then Button1Click(Nil);
End;

Procedure TForm3.Button1Click(Sender: TObject);
Begin
  Highscore.AddScore(high(field) + 1, high(field[0]) + 1, UsedColorNumbers, UsedSpezials, Edit1.text, strtoint(form1.StatusBar1.Panels[1].Text), GameScores);
  close;
End;

End.

