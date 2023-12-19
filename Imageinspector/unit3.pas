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
Unit Unit3;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls;

Type

  { TForm3 }

  TForm3 = Class(TForm)
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    TrackBar4: TTrackBar;
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
  Form3: TForm3;

Implementation

{$R *.lfm}

Uses unit1, ugraphics, uvectormath;

{ TForm3 }

Procedure TForm3.FormCreate(Sender: TObject);
Begin
  caption := 'Perspective correction';
  FImageBackup := Nil;
End;

Procedure TForm3.CheckBox1Change(Sender: TObject);
Begin
  DoTheMagic(false);
End;

Procedure TForm3.FormDestroy(Sender: TObject);
Begin
  If assigned(FImageBackup) Then
    FImageBackup.free;
  FImageBackup := Nil
End;

Procedure TForm3.DoTheMagic(Final: Boolean);
Var
  LensCenter: TVector2;
  Strength: Single;
Begin
  If CheckBox1.Checked Then Begin
    form1.fImage.Canvas.Draw(0, 0, FImageBackup);
    LensCenter := v2(Form1.fImage.Width / 2, Form1.fImage.Height / 2) - v2((TrackBar2.Position * Form1.fImage.Width) / 200, (TrackBar3.Position * Form1.fImage.Height) / 200);
    Strength := TrackBar1.Position / 100;
    If final Then Begin
      Aberration(form1.fImage, LensCenter, Strength, Strength, 1, TrackBar4.Position / 100, imBilinear);
    End
    Else Begin
      Aberration(form1.fImage, LensCenter, Strength, Strength, 0, TrackBar4.Position / 100, imNearestNeighbour);
    End;
  End
  Else Begin
    form1.fImage.Canvas.Draw(0, 0, FImageBackup);
  End;
  form1.ReCreateStretchBMP();
  form1.PaintBox1.Invalidate;
End;

Procedure TForm3.ResetSettings();
Begin
  CheckBox1.Checked := true;
  TrackBar1.Position := 0;
  TrackBar2.Position := 0;
  TrackBar3.Position := 0;
  TrackBar4.Position := 100;
End;

Procedure TForm3.CreateBackup;
Begin
  If assigned(FImageBackup) Then
    FImageBackup.free;
  FImageBackup := TBitmap.Create;
  FImageBackup.Assign(form1.fimage);
  DoTheMagic(False);
End;

End.

