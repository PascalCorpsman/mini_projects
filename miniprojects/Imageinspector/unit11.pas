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
Unit Unit11;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls;

Type

  { TForm11 }

  TForm11 = Class(TForm)
    CheckBox1: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Label5: TLabel;
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
  Form11: TForm11;

Implementation

{$R *.lfm}

Uses unit1, ugraphics, uvectormath;

{ TForm11 }

Procedure TForm11.FormCreate(Sender: TObject);
Begin
  caption := 'Image';
  FImageBackup := Nil;
End;

Procedure TForm11.CheckBox1Change(Sender: TObject);
Begin
  DoTheMagic(false);
End;

Procedure TForm11.FormDestroy(Sender: TObject);
Begin
  If assigned(FImageBackup) Then
    FImageBackup.free;
  FImageBackup := Nil
End;

Procedure TForm11.CreateBackup;
Begin
  If assigned(FImageBackup) Then
    FImageBackup.free;
  FImageBackup := TBitmap.Create;
  FImageBackup.Assign(form1.fimage);
  DoTheMagic(False);
End;

Procedure TForm11.DoTheMagic(Final: Boolean);
Var
  v: TVectorN;
  m: TMatrixNxM;
Begin
  If CheckBox1.Checked Then Begin
    form1.fImage.Canvas.Draw(0, 0, FImageBackup);
    // Schärfen
    If TrackBar1.Position <> 0 Then Begin
      (*
       * Die Idee ist, man wendet den Laplace Operator an und sorgt aber dafür das das Gewicht der Matxix
       * 1 ist (anstatt normalerweise 0) in dem man im Zentrum 1 addiert
       *)
      Case TrackBar1.Position Of // Versuch die Nach Schwere zu sortieren (empirisch)
        1: m := MNxM(3, [
            0, -1, 0,
              -1, 5, -1,
              0, -1, 0]); // ISBN: 3-8273-7155-4 , Seite 182
        2: m := MNxM(3, [
            -1, -4, -1,
              -4, 21, 4,
              -1, -4, -1]); //  ISBN: 978-3-540-21888-3, Seite 63
        3: m := MNxM(3, [
            -1, -1, -1,
              -1, 9, -1,
              -1, -1, -1]); // ISBN: 3-8273-7155-4 , Seite 182
        4: m := MNxM(5,
            [
            0, 0, -1, 0, 0,
              0, -1, -2, -1, 0,
              -1, -2, 17, -2, -1,
              0, -1, -2, -1, 0,
              0, 0, -1, 0, 0
              ]); //  ISBN: 978-3-540-21888-3, Seite 65
      End;
      FoldImage(Form1.fImage, m, true);
    End;
    // Glätten
    If TrackBar2.Position <> 0 Then Begin
      // Filterung via Gausfilter
      Case TrackBar2.Position Of
        1: v := vn([1, 1]);
        2: v := vn([1, 2, 1]);
        3: v := vn([1, 3, 3, 1]);
        4: v := vn([1, 4, 6, 4, 1]);
        5: v := vn([1, 5, 10, 10, 5, 1]);
        6: v := vn([1, 6, 15, 20, 15, 6, 1]);
        7: v := vn([1, 7, 21, 35, 35, 21, 7, 1]);
        8: v := vn([1, 8, 28, 56, 70, 56, 28, 8, 1]);
        9: v := vn([1, 9, 36, 84, 126, 126, 84, 36, 9, 1]);
        10: v := vn([1, 10, 45, 120, 210, 252, 210, 120, 45, 10, 1]);
      End;
      m := MNxM(1, v) * v.Transpose();
      FoldImage(Form1.fImage, m, false);
    End;
  End
  Else Begin
    form1.fImage.Canvas.Draw(0, 0, FImageBackup);
  End;
  form1.ReCreateStretchBMP();
  form1.PaintBox1.Invalidate;
End;

Procedure TForm11.ResetSettings();
Begin
  CheckBox1.Checked := true;
  TrackBar1.Position := 0;
  TrackBar2.Position := 0;
End;

End.

