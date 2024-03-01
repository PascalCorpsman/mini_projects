(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Einstein                                              *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit4;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

Type

  { TForm4 }

  TForm4 = Class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Procedure Button1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  End;

Var
  Form4: TForm4;

Implementation

{$R *.lfm}

{ TForm4 }

Procedure TForm4.FormCreate(Sender: TObject);
Begin
  caption := 'Onlinehelp, examples.';
  memo1.text :=
    'Depends on:' + LineEnding +
    '  The Briton lifes in the red house' + LineEnding +
    '  -> (red,Briton,-,-,-)' + LineEnding +
    'Distance depends on:' + LineEnding +
    '  The man living next to the man that smokes Rothmanns is Dane' + LineEnding +
    '  -> (-,-,-,Rothmanns,-) neighbour of (-,Dane,-,-,-), Distance = 1' + LineEnding +
    'Left of:' + LineEnding +
    '  The briton lifes anywhere left from the man with the cat' + LineEnding +
    '  -> (-,Briton,-,-,-) left of (-,-,-,-,cat), Distance = 1' + LineEnding +
    'Distance left of:' + LineEnding +
    '  The direct left neighbour of the red house drinks milk' + LineEnding +
    '  -> (red,-,-,-,-) direct left of (-,-,milk,-,-), Distance = 1' + LineEnding +
    'Right of:' + LineEnding +
    '  The briton lifes anywhere right from the man with the cat' + LineEnding +
    '  -> (-,Briton,-,-,-) right of (-,-,-,-,cat), Distance = 1' + LineEnding +
    'Distance right of:' + LineEnding +
    '  The direct right neighbour of the green house smokes Pallmall' + LineEnding +
    '  -> (green,-,-,-,-) direct right of (-,-,-,Pallmall,-), Distance = 1' + LineEnding +
    'Eliminate:' + LineEnding +
    '  The man living in the red house does not have a cat.' + LineEnding +
    '  -> (red,-,-,-,-) eliminate (-,-,-,-,cat)' + LineEnding +
    'Distance eliminate:' + LineEnding +
    '  The Briton does not live next to the man that smokes Rothmanns' + LineEnding +
    '  -> (-,Briton,-,-,-) neighbour of (-,-,-,Rothmanns,-), Distance = 1' + LineEnding +
    'IntegerDepends: (attention this is only allowed if the cathegory elements are numbers or like "number text"' + LineEnding +
    '  The Man in the Red house drinks 2 more beer than the man with the cat' + LineEnding +
    '  -> (Red,beer,-2,Cat)'
    ;
End;

Procedure TForm4.Button1Click(Sender: TObject);
Begin
  close;
End;

End.

