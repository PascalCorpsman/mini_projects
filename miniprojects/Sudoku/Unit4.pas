(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Sudoku                                                *)
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

{$MODE ObjFPC}{$H+}

Interface

Uses
  Forms, Classes, Controls, StdCtrls, ComCtrls, LResources, usudoku;

Type
  TForm4 = Class(TForm)
    TreeView1: TTreeView;
    Button1: TButton;
    Memo1: TMemo;
    Procedure FormCreate(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
    Procedure TreeView1Click(Sender: TObject);
    Procedure TreeView1GetSelectedIndex(Sender: TObject; Node: TTreeNode);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;
Const
  Rules = 'Sudoku ver. : ' + ver + '  by Corpsman | Targetsoft |' + LineEnding + LineEnding +
    'Rules for Sudoku :' + LineEnding + LineEnding +
    'You have to insert numbers ( or substituted characters ) from 1 - 9 in each row and each column.' + LineEnding + LineEnding +
    'But not enough in each row and column every number can be placed only one times, same thing with the 3x3 field''s which marked extra.';

  HowtoSolve = 'The puzzle''s created from this programm can always be solved trough logical or mathematik thinking.' + LineEnding + LineEnding +
    'If you are new in this game then use the effect options to see singled field''s. But only one effect at same time! For more read the effect help.';

  Kontrol = 'You can move the cursor, with the "asdw" key or by click on the field you want.' + LineEnding + LineEnding +
    'By press a number the number will be inserted in the field.' + LineEnding +
    'By holding the "shift" key while pressing the number you enter a Maybe number.' + LineEnding +
    'Use this if you are not shure if the number is right.' + LineEnding + LineEnding +
    'If you want to insert a pencil number then check the box for editing pencil''s and press the number key''s.' + LineEnding + LineEnding +
    'You can delete a number by pressing "0" or the number in field / pencil twice.';

  Effect = 'The effect''s are only tools to get better to your solution.' + LineEnding + LineEnding +
    'You can use them or not, in both cases you could reach your aim.';

  Lineffect = 'This option marks each field which is blocked by the number from which you selected it.' + LineEnding + LineEnding +
    'Enable this effect by clicking left on it''s picture. Disable it by clicking twice on the picture.' + LineEnding +
    'If you have enabled more than one effect and want to use only one, than click on this one with the right mouse button to select this and deselect all other''s.';

  highlighter = 'The Highliter show''s each value in the field or pencil''s which is same.' + LineEnding + LineEnding +
    'Enable a higlighter by clicking left on it''s picture. Disable it by clicking twice on the picture.' + LineEnding +
    'If you have enabled more than one highlighter and want to use only one, than click on this one with the right mouse button to select this and deselect all other''s.';

  Pencils = 'The pencil''s are numbers you cen write in each field to solve the Sudoku.' + LineEnding +
    'If you check the edit Pencil Text you can edit the pencils.' + LineEnding +
    'by pressing an number it will be added to the pencil''s by pressing it twice it wil be deleted from the pencil''s' + LineEnding + LineEnding +
    'If you hit the autopencil button the computer write''s then pencil''s for you. By clicking the clear pencil button all pencil data wil be erased.';

  Linepencils = 'The line pencil''s can be used to pencil all numbers missing in the row or column.';

  Fieldpencils = 'The fieldpencil can be used to write down all possible numbers for a field.';

  numbers = 'There are 3 colors for the numbers in the field.' + LineEnding + LineEnding +
    'normaly Black is used for the color''s of the numbers which were fixed.' + LineEnding + LineEnding +
    'yellow is used for the numbers which you think they could be wrong' + LineEnding + LineEnding +
    'lightblue is used for normal inserted numbers.' + LineEnding + LineEnding +
    'If you want to change the color''s go to the color options';

  actionf = 'You are hanging, or just don''t find the next number, no problem just let the computer get it.' + LineEnding + LineEnding +
    'There are two mode''s "Solve it" solve''s the complete Sudoko, if possible.' + LineEnding + LineEnding +
    '"Solve step" give''s you just only one number, so you could go on with your puzzle.' + LineEnding + LineEnding +
    'The solving options can be used if you want the computer to use onle some solving method''s. But without all solving method''s it could be possible that the computer cannot solve the sudoko.' + LineEnding + LineEnding +
    '! Attention !' + LineEnding + LineEnding +
    'The method "try and error" is no real solving method. It''s better you don''t use this methof for normal Sudoko''s.';

  byhiddensingle = 'A hidden single number is a number which can only be at one position and on no one other.' + LineEnding + LineEnding +
    'This is because in the 9 block are other numbers and all free field''s exept one were blocked trough the same number.';

  bynakedsingle = 'a naked single number is a number which is the last one in a row , or column , or 9 block.';

  byblockandc = 'not implemented yet';

  byblockandb = 'not implemented yet';

  bynakedsubset = 'If you have n field''s in your row, block or column and these field''s have n candidates , which are the same.' + LineEnding +
    ' Then you could delete these candidates from all other field''s of these row, block or column.';

  byhiddensubset = 'implemented..';

  byxwing = 'not implemented yet';

  byxywing = 'implemented..';

  byforcinchains = 'not implemented yet';

  bytryerror = 'This solving method is not logikal, or mathematik.' + LineEnding + LineEnding +
    'If you have a Sudoko and you are not shure if there is a solution at all.' + LineEnding +
    'Then try to get the solution with this technologie.' + LineEnding + LineEnding +
    'But think that it can take a lot of time to get a solution.';

Var
  Form4: TForm4;

Implementation

{$R *.lfm}

Procedure TForm4.FormCreate(Sender: TObject);
Begin
  Caption := 'Sudoku ver. : ' + ver + ' Help';
End;

Procedure TForm4.Button1Click(Sender: TObject);
Begin
  Close;
End;

Procedure TForm4.TreeView1Click(Sender: TObject);
Begin
  If TreeView1.Selected.Text = 'Rules' Then
    memo1.text := rules;
  If TreeView1.Selected.Text = 'How to solve' Then
    memo1.text := HowtoSolve;
  If TreeView1.Selected.Text = 'Controls' Then
    memo1.text := Kontrol;
  If TreeView1.Selected.Text = 'Effects' Then
    memo1.text := Effect;
  If TreeView1.Selected.Text = 'Line effects' Then
    memo1.text := Lineffect;
  If TreeView1.Selected.Text = 'Highlighter' Then
    memo1.text := highlighter;
  If TreeView1.Selected.Text = 'Pencils' Then
    memo1.text := Pencils;
  If TreeView1.Selected.Text = 'Line pencils' Then
    memo1.text := Linepencils;
  If TreeView1.Selected.Text = 'Field pencils' Then
    memo1.text := Fieldpencils;
  If TreeView1.Selected.Text = 'Numbers' Then
    memo1.text := numbers;
  If TreeView1.Selected.Text = 'Action' Then
    memo1.text := actionf;
  If TreeView1.Selected.Text = 'by hidden single' Then
    memo1.text := byhiddensingle;
  If TreeView1.Selected.Text = 'by naked single' Then
    memo1.text := bynakedsingle;
  If TreeView1.Selected.Text = 'by block and column integration' Then
    memo1.text := byblockandc;
  If TreeView1.Selected.Text = 'by block and block integration' Then
    memo1.text := byblockandb;
  If TreeView1.Selected.Text = 'by naked subset' Then
    memo1.text := bynakedsubset;
  If TreeView1.Selected.Text = 'by hidden subset' Then
    memo1.text := byhiddensubset;
  If TreeView1.Selected.Text = 'by X - Wing / Swordfish' Then
    memo1.text := byxwing;
  If TreeView1.Selected.Text = 'by XY - Wing' Then
    memo1.text := byxywing;
  If TreeView1.Selected.Text = 'by Forcing Chains' Then
    memo1.text := byforcinchains;
  If TreeView1.Selected.Text = 'by try and error' Then
    memo1.text := bytryerror;
End;

Procedure TForm4.TreeView1GetSelectedIndex(Sender: TObject;
  Node: TTreeNode);
Begin
  TreeView1Click(Nil);
End;

End.

