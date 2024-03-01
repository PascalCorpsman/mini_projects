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
Unit Unit5;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

Type

  { TForm5 }

  TForm5 = Class(TForm)
    Procedure FormCreate(Sender: TObject);
  private

  public
    buttons: Array Of TButton;
    panels: Array Of TPanel;
  End;

Var
  Form5: TForm5;

Implementation

{$R *.lfm}

{ TForm5 }

Procedure TForm5.FormCreate(Sender: TObject);
Begin
  buttons := Nil;
  panels := Nil;
  caption := 'Menu';
End;

End.

