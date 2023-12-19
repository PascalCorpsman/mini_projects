(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of ImageInspector                                        *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit10;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls;

Type

  { TForm10 }

  TForm10 = Class(TForm)
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure TrackBar1Change(Sender: TObject);
  private

  public
    FImageBackup: TBitmap;
    Procedure CreateBackup;
    Procedure DoTheMagic(Final: Boolean);
    Procedure ResetSettings();
  End;

Var
  Form10: TForm10;

Implementation

{$R *.lfm}

{ TForm10 }

Uses unit1, ugraphics;


Procedure TForm10.FormCreate(Sender: TObject);
Begin
  caption := 'Ilumination';
  FImageBackup := Nil;
End;

Procedure TForm10.FormDestroy(Sender: TObject);
Begin
  If assigned(FImageBackup) Then
    FImageBackup.free;
  FImageBackup := Nil
End;

Procedure TForm10.TrackBar1Change(Sender: TObject);
Begin
  DoTheMagic(false);
End;

Procedure TForm10.DoTheMagic(Final: Boolean);
Begin
  If CheckBox1.Checked Then Begin
    form1.fImage.Canvas.Draw(0, 0, FImageBackup);
    Brightning(form1.fImage, TrackBar1.Position);
    Contrast(form1.fImage, TrackBar2.Position);
  End
  Else Begin
    form1.fImage.Canvas.Draw(0, 0, FImageBackup);
  End;
  form1.ReCreateStretchBMP();
  form1.PaintBox1.Invalidate;
End;

Procedure TForm10.ResetSettings();
Begin
  CheckBox1.Checked := true;
  TrackBar1.Position := 0;
  TrackBar2.Position := 0;
End;

Procedure TForm10.CreateBackup;
Begin
  If assigned(FImageBackup) Then
    FImageBackup.free;
  FImageBackup := TBitmap.Create;
  FImageBackup.Assign(form1.fimage);
  DoTheMagic(False);
End;

End.

