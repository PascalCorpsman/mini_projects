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
Unit Unit4;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls;

Type

  { TForm4 }

  TForm4 = Class(TForm)
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    Procedure CheckBox1Change(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
  private
  public
    FImageBackup: TBitmap;
    Procedure CreateBackup;
    Procedure DoTheMagic(Final: Boolean);
    Procedure ResetSettings();
  End;

Var
  Form4: TForm4;

Implementation

{$R *.lfm}

Uses unit1, ugraphics;

{ TForm4 }

Procedure TForm4.FormCreate(Sender: TObject);
Begin
  caption := 'Vignetting';
  FImageBackup := Nil;
End;

Procedure TForm4.CheckBox1Change(Sender: TObject);
Begin
  DoTheMagic(false);
End;

Procedure TForm4.FormDestroy(Sender: TObject);
Begin
  If assigned(FImageBackup) Then
    FImageBackup.free;
  FImageBackup := Nil
End;

Procedure TForm4.DoTheMagic(Final: Boolean);
Begin
  If CheckBox1.Checked Then Begin
    form1.fImage.Canvas.Draw(0, 0, FImageBackup);
    Vignetting(Form1.fImage, TrackBar1.Position / 100, 1, TrackBar2.Position / 100);
  End
  Else Begin
    form1.fImage.Canvas.Draw(0, 0, FImageBackup);
  End;
  form1.ReCreateStretchBMP();
  form1.PaintBox1.Invalidate;
End;

Procedure TForm4.ResetSettings();
Begin
  CheckBox1.Checked := true;
  // Default, die Ränder aufhellen
  TrackBar1.Position := 100; // Strength
  TrackBar2.Position := 200; // Max Level
End;

Procedure TForm4.CreateBackup;
Begin
  If assigned(FImageBackup) Then
    FImageBackup.free;
  FImageBackup := TBitmap.Create;
  FImageBackup.Assign(form1.fimage);
  DoTheMagic(False);
End;

End.

