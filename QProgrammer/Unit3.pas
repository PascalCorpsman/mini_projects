(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Q-Programmer                                          *)
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

{$MODE ObjFPC}{$H+}

Interface

Uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, LResources;

Type
  TForm3 = Class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Procedure FormCreate(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

Var
  Form3: TForm3;

Implementation

{$R *.lfm}

Procedure TForm3.FormCreate(Sender: TObject);
Begin
  caption := 'Help';
End;

Procedure TForm3.Button1Click(Sender: TObject);
Begin
  close;
End;

End.

 
