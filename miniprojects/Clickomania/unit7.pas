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
Unit Unit7;

{$MODE objfpc}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

Type

  { TForm7 }

  TForm7 = Class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Procedure Button1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  End;

Var
  Form7: TForm7;

Implementation

{$R *.lfm}

{ TForm7 }

Procedure TForm7.FormCreate(Sender: TObject);
Begin
  caption := 'About';
  label1.caption :=
    'Clickomania is a adaption to the windows-version' + LineEnding +
    'of the game from Matthias Schüssler, available' + LineEnding +
    'under www.clickomania.ch under the name' + LineEnding +
    '"Classic Clickomania".' + LineEnding + LineEnding +
    'The differences to the origin are listed in diff.txt.' + LineEnding + LineEnding +
    'Autor : Corpsman' + LineEnding +
    'Support and bug report : www.Corpsman.de';
  Constraints.MinHeight := height;
  Constraints.MaxHeight := height;
  Constraints.MinWidth := width;
  Constraints.MaxWidth := width;
End;

Procedure TForm7.Button1Click(Sender: TObject);
Begin
  close;
End;

End.

