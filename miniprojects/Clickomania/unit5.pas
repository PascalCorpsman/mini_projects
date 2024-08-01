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
Unit Unit5;

{$MODE objfpc}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

Type

  { TForm5 }

  TForm5 = Class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Procedure Button1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  End;

Var
  Form5: TForm5;

Implementation

{$R *.lfm}

{$I clickomania.inc}

{ TForm5 }

Procedure TForm5.FormCreate(Sender: TObject);
Begin
  caption := 'Info';
  Constraints.MaxHeight := height;
  Constraints.MinHeight := height;
  Constraints.MaxWidth := width;
  Constraints.MinWidth := width;
End;

Procedure TForm5.Button1Click(Sender: TObject);
Begin
  close;
End;

End.

