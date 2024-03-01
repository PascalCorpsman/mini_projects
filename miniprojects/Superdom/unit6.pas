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
Unit Unit6;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

Type

  { TForm6 }

  TForm6 = Class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Procedure Button1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private

  public

  End;

Var
  Form6: TForm6;

Implementation

{$R *.lfm}

{ TForm6 }

Procedure TForm6.FormCreate(Sender: TObject);
Begin
  caption := 'Gameplay';
  memo1.text :=
    'Colors:' + LineEnding +
    '  buildings: purple = human, green = ai' + LineEnding +
    '  land: bright = human, dark = ai' + LineEnding + LineEnding +
    'Costs:' + LineEnding +
    '  (extra) move unit: 100' + LineEnding +
    '  men: 1' + LineEnding +
    '  tank : 300' + LineEnding +
    '  plane: 600' + LineEnding +
    '  farm : 1150' + LineEnding +
    '  industry: 1300' + LineEnding +
    '  nuke: 2000' + LineEnding + LineEnding +
    'Ressources:' + LineEnding +
    '- each men eats one food per round, if not' + LineEnding +
    '  enough food available, the men die.' + LineEnding +
    '- money on the bank get each round a invest.' + LineEnding + LineEnding +
    'Rounds:' + LineEnding +
    '  (farm and industry production) -> buy -> move units -> (feed units) -> war'
    ;
End;

Procedure TForm6.Button1Click(Sender: TObject);
Begin
  close;
End;

End.

