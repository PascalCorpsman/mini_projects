(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of tippsy3                                               *)
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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Procedure Button1Click(Sender: TObject);
    Procedure Edit1KeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  End;

Var
  Form2: TForm2;

Implementation

Uses Unit1;

{$R *.lfm}

{ TForm2 }

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  edit1.text := '';
  caption := 'Hidden results..';
End;

Procedure TForm2.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  canclose := edit1.text = UnlockPW;
  edit1.text := '';
End;

Procedure TForm2.Button1Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm2.Edit1KeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState
  );
Begin
  If key = 13 Then button1.OnClick(Nil);
End;

End.

