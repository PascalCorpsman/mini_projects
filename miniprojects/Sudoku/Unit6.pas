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
Unit Unit6;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Graphics, Forms, Classes, Controls, StdCtrls, ComCtrls, ExtCtrls, LResources;

Type
  TForm6 = Class(TForm)
    Button1: TButton;
    Label1: TLabel;
    ProgressBar1: TProgressBar;
    Timer1: TTimer;
    Procedure Button1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormPaint(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure Timer1Timer(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

Var
  Form6: TForm6;
  zwangsabbruch: Boolean;

Implementation

Uses
  Unit1
  , Unit11
  , Unit12
  , Unit13
  ;

{$R *.lfm}

Procedure TForm6.Button1Click(Sender: TObject);
Begin
  // Graphischer Schnickschnack hat nichts zu bedeuten sied aber net aus  ;)
  Timer1.enabled := false;
  zwangsabbruch := true;
  close;
End;

Procedure TForm6.FormCreate(Sender: TObject);
Begin
  Label1.caption := 'Sometimes it could be possible that the Computer' + LineEnding + 'hang''s.' + LineEnding + 'Maybe the Computer is trying to solve a not' + LineEnding + 'solvable Sudoku.' + LineEnding + 'In this Case push the Cancel Button';
End;

Procedure TForm6.FormPaint(Sender: TObject);
Begin
  If Not Timer1.enabled Then Begin
    Timer1.enabled := true;
    ProgressBar1.Position := 0;
    canvas.pen.color := clblack;
    canvas.Brush.color := clbtnface;
    canvas.brush.style := bssolid;
    canvas.rectangle(0, 0, form6.width, form6.height);
    label1.Repaint;
    Button1.Repaint;
  End;
End;

Procedure TForm6.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Begin
  If Not Form11.visible Then
    form1.SetFocus;
End;

Procedure TForm6.Timer1Timer(Sender: TObject);
Begin
  // Graphischer Schnickschnack hat nichts zu bedeuten sied aber net aus  ;)
  ProgressBar1.Position := ProgressBar1.Position + 1;
  If ProgressBar1.Position = 100 Then ProgressBar1.Position := 0;
  If (Form11.visible Or form12.visible Or form13.visible) And Form6.visible Then
    Form6.BringToFront;
End;

End.

