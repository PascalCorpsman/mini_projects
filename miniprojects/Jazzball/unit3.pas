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
Unit Unit3;

{$MODE objfpc}{$H+}

Interface

Uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ujazz, highscoreengine;

Type
  TForm3 = Class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    Procedure Button1Click(Sender: TObject);
    Procedure FormPaint(Sender: TObject);
    Procedure Memo1Enter(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

Var
  Form3: TForm3;

Implementation

{$R *.lfm}

Procedure TForm3.Button1Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm3.FormPaint(Sender: TObject);
Begin
  Button1.setfocus;
End;

Procedure TForm3.Memo1Enter(Sender: TObject);
Begin
  Button1.setfocus;
End;

Procedure TForm3.Button2Click(Sender: TObject);
Begin
  Highscore.free;
  DeleteFile(IncludeTrailingPathDelimiter(extractfilepath(paramstr(0))) + 'highscore.dat');
  highscore := thighscore.create(IncludeTrailingPathDelimiter(extractfilepath(paramstr(0))) + 'highscore.dat', 17);
  Memo1.clear;
  Memo1.lines := highscore.show;
  memo1.lines.Insert(0, '');
End;

End.

