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
Unit Unit13;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls;

Type

  { TForm13 }

  TForm13 = Class(TForm)
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
  Form13: TForm13;

Implementation

{$R *.lfm}

Uses unit1, ugraphics, IntfGraphics, fpImage, uvectormath;

{ TForm13 }

Procedure TForm13.FormCreate(Sender: TObject);
Begin
  caption := 'Color';
  FImageBackup := Nil;
End;

Procedure TForm13.CheckBox1Change(Sender: TObject);
Begin
  DoTheMagic(false);
End;

Procedure TForm13.FormDestroy(Sender: TObject);
Begin
  If assigned(FImageBackup) Then
    FImageBackup.free;
  FImageBackup := Nil
End;

Procedure TForm13.CreateBackup;
Begin
  If assigned(FImageBackup) Then
    FImageBackup.free;
  FImageBackup := TBitmap.Create;
  FImageBackup.Assign(form1.fimage);
  DoTheMagic(False);
End;

Procedure TForm13.DoTheMagic(Final: Boolean);
Var
  intf: TLazIntfImage;
  var_interval, gray_factor, fract: Single;
  c: TFPColor;
  hsl: THSL;
  i, j: Integer;
Begin
  If CheckBox1.Checked Then Begin
    form1.fImage.Canvas.Draw(0, 0, FImageBackup);
    // Sättigung
    If (TrackBar1.Position <> 0) Or (TrackBar2.Position <> 0) Then Begin
      fract := TrackBar1.Position / 50;
      //  Quelle: https://www.pocketmagic.net/enhance-saturation-in-images-programatically/
      intf := Form1.fImage.CreateIntfImage;
      For i := 0 To intf.Width - 1 Do Begin
        For j := 0 To intf.Height - 1 Do Begin

          c := intf.Colors[i, j];
          hsl := FPColorToHSL(c);
          If fract >= 0 Then Begin
            // we don't want to saturate unsaturated colors -> we get only defects
            // for unsaturared colors this tends to 0

            gray_factor := hsl.s / 255.0;
            // how far can we go?
            // if we increase saturation, we have "255-s" space left
            var_interval := 255 - hsl.s;
            // compute the new saturation
            hsl.s := clamp(hsl.s + round(fract * var_interval * gray_factor), 0, 255);
          End
          Else Begin
            // how far can we go?
            // for decrease we have "s" left
            var_interval := hsl.s;
            hsl.s := clamp(hsl.s + round(fract * var_interval), 0, 255);
          End;
          hsl.h := (hsl.h + TrackBar2.Position) Mod 360; // Der H Anteil lässt sich einfacher Manipulieren ;)
          c := HSLToFPColor(hsl);
          intf.Colors[i, j] := c;
        End;
      End;
      Form1.fImage.LoadFromIntfImage(intf);
      intf.free;
    End;
  End
  Else Begin
    form1.fImage.Canvas.Draw(0, 0, FImageBackup);
  End;
  form1.ReCreateStretchBMP();
  form1.PaintBox1.Invalidate;
End;

Procedure TForm13.ResetSettings();
Begin
  CheckBox1.Checked := true;
  TrackBar1.Position := 0;
  TrackBar2.Position := 0;
End;

End.

