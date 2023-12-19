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
Unit Unit9;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls;

Type

  { TForm9 }

  TForm9 = Class(TForm)
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    ComboBox1: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    Procedure CheckBox1Click(Sender: TObject);
    Procedure CheckBox2Click(Sender: TObject);
    Procedure ComboBox1Change(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
  private
    Procedure Histo();
  public
    FImageBackup: TBitmap;
    Procedure CreateBackup;
    Procedure DoTheMagic(Final: Boolean);
    Procedure ResetSettings();
  End;

Var
  Form9: TForm9;

Implementation

{$R *.lfm}

Uses unit1, ugraphics, uvectormath, IntfGraphics, fpImage, math;

{ TForm9 }

Procedure TForm9.FormCreate(Sender: TObject);
Begin
  caption := 'Histogram correction';
  FImageBackup := Nil;
End;

Procedure TForm9.CheckBox1Click(Sender: TObject);
Begin
  If CheckBox1.Checked Then CheckBox2.Checked := false;
  DoTheMagic(false);
End;

Procedure TForm9.CheckBox2Click(Sender: TObject);
Begin
  If CheckBox2.Checked Then CheckBox1.Checked := false;
  DoTheMagic(false);
End;

Procedure TForm9.ComboBox1Change(Sender: TObject);
Begin
  DoTheMagic(false);
End;

Procedure TForm9.FormDestroy(Sender: TObject);
Begin
  If assigned(FImageBackup) Then
    FImageBackup.free;
  FImageBackup := Nil
End;

Procedure TForm9.Histo();
Type
  TColorInfo = Record
    minV, maxV: integer; // Index der 1. bzw letzten Zahl <> 0
    lums: Array[0..255] Of Integer;
    CumMap: Array[0..255] Of Integer;
  End;

Var
  C: Array[0..3] Of TColorInfo; // 0 = Red, 1 = Green, 2 = Blue, 3 = Gray
  UpperBorder, LowerBorder: Integer;
  UseUpper, UseLower: Boolean;

  Procedure Update(Index, ColValue: Integer);
  Begin
    c[index].lums[ColValue] := c[index].lums[ColValue] + 1;
    c[index].minV := min(c[index].minV, ColValue);
    c[index].maxV := max(c[index].maxV, ColValue);
  End;

  Function StretchColor(c: TFPColor; mi, ma: integer): TFPColor;
  Var
    f: Integer;
  Begin
    If UseLower Then mi := LowerBorder;
    If UseUpper Then ma := UpperBorder;
    f := round(ConvertDimension(mi, ma, c.Red Shr 8, 0, 255));
    c.Red := clamp(f, 0, 255) Shl 8;
    f := round(ConvertDimension(mi, ma, c.Green Shr 8, 0, 255));
    c.Green := clamp(f, 0, 255) Shl 8;
    f := round(ConvertDimension(mi, ma, c.Blue Shr 8, 0, 255));
    c.Blue := clamp(f, 0, 255) Shl 8;
    c.Alpha := 255 Shr 8;
    result := c;
  End;

Var
  dim, i, j: Integer;
  intf: TLazIntfImage;
  tcol, col, rcol: TFPColor;
  lum, mi, ma: Integer;
  Cummulativ, Stretched: Boolean;
  ColorVariant: integer;
Begin
  Cummulativ := CheckBox1.Checked;
  Stretched := CheckBox2.Checked;
  ColorVariant := ComboBox1.ItemIndex;
  UpperBorder := TrackBar1.Position;
  LowerBorder := TrackBar2.Position;
  UseUpper := CheckBox3.Checked;
  useLower := CheckBox4.Checked;
  // Alle Notwendigen Werte Erfassen
  For i := 0 To 3 Do Begin
    c[i].minV := 256;
    c[i].maxV := -1;
    For j := 0 To 255 Do Begin
      c[i].lums[j] := 0;
    End;
  End;
  intf := Form1.fImage.CreateIntfImage;
  For i := 0 To Form1.fImage.Width - 1 Do Begin
    For j := 0 To Form1.fImage.Height - 1 Do Begin
      col := intf.Colors[i, j];
      Update(0, col.Red Shr 8);
      Update(1, col.Green Shr 8);
      Update(2, col.Blue Shr 8);
      lum := FPColortoLuminanz(col);
      Update(3, lum);
    End;
  End;
  If Cummulativ Then Begin
    (*
     * Die Formel der Cummulativen HistogrammKorrektur stammt von:
     * https://www.programmersought.com/article/67436787775/  (Dort Zeilen 27 - 32
     *)
    dim := Form1.fImage.Width * Form1.fImage.Height;
    For j := 0 To 3 Do Begin
      lum := 0;
      // Kopieren der Luminant
      For i := 0 To 255 Do Begin
        c[j].CumMap[i] := c[j].lums[i];
      End;
      For i := 1 To 255 Do Begin
        c[j].CumMap[i] := c[j].CumMap[i] + c[j].CumMap[i - 1];
        c[j].CumMap[i - 1] := (c[j].CumMap[i - 1] * 255) Div dim;
      End;
      c[j].CumMap[255] := (c[j].CumMap[255] * 255) Div dim;
    End;
  End;
  Case ColorVariant Of
    0, 5: Begin // Grayscale, convert and Grayscale
        mi := c[3].minV;
        ma := c[3].maxV;
      End; //InitByArray(l);
    1, 2, 3: Begin
        mi := c[ColorVariant - 1].minV;
        ma := c[ColorVariant - 1].maxV;
      End;
    4: Begin // Jeder Farbkanal Separat
      End;
  End;
  For i := 0 To Form1.fImage.Width - 1 Do Begin
    For j := 0 To Form1.fImage.Height - 1 Do Begin
      col := intf.Colors[i, j];
      If ColorVariant = 5 Then Begin // Convertierung nach Graustufen
        col.Red := FPColortoLuminanz(col) Shl 8;
        col.Green := col.Red;
        col.Blue := col.Red;
        col.Alpha := 255 * 256;
      End;
      If Cummulativ Then Begin
        Case ColorVariant Of
          0, 5: Begin
              rcol.Red := c[3].CumMap[col.Red Shr 8] Shl 8;
              rcol.Green := c[3].CumMap[col.Green Shr 8] Shl 8;
              rcol.Blue := c[3].CumMap[col.Blue Shr 8] Shl 8;
              rcol.Alpha := 255 * 256;
            End;
          1: rcol.Red := c[0].CumMap[col.Red Shr 8] Shl 8;
          2: rcol.Green := c[1].CumMap[col.Green Shr 8] Shl 8;
          3: rcol.Blue := c[2].CumMap[col.Blue Shr 8] Shl 8;
          4: Begin
              rcol.Red := c[0].CumMap[col.Red Shr 8] Shl 8;
              rcol.Green := c[1].CumMap[col.Green Shr 8] Shl 8;
              rcol.Blue := c[2].CumMap[col.Blue Shr 8] Shl 8;
              rcol.Alpha := 255 * 256;
            End;
        End;
      End;
      If Stretched Then Begin
        rcol := StretchColor(col, mi, ma);
      End;
      Case ColorVariant Of
        0, 5: col := rcol;
        1: col.Red := rcol.red;
        2: col.Green := rcol.Green;
        3: col.Blue := rcol.Blue;
        4: Begin
            If Cummulativ Then Begin
              col := rcol;
            End;
            If Stretched Then Begin
              rcol := StretchColor(col, c[0].minV, c[0].maxV);
              tcol.Red := rcol.Red;
              rcol := StretchColor(col, c[1].minV, c[1].maxV);
              tcol.Green := rcol.Green;
              rcol := StretchColor(col, c[2].minV, c[2].maxV);
              tcol.Blue := rcol.Blue;
              tcol.Alpha := rcol.Alpha;
              col := tcol;
            End;
          End;
      End;
      intf.Colors[i, j] := col;
    End;
  End;
  Form1.fImage.LoadFromIntfImage(intf);
  intf.Free;
End;

Procedure TForm9.CreateBackup;
Begin
  If assigned(FImageBackup) Then
    FImageBackup.free;
  FImageBackup := TBitmap.Create;
  FImageBackup.Assign(form1.fimage);
  DoTheMagic(False);
End;

Procedure TForm9.DoTheMagic(Final: Boolean);
Begin
  If CheckBox1.Checked Or CheckBox2.Checked Then Begin
    form1.fImage.Canvas.Draw(0, 0, FImageBackup);
    Histo();
  End
  Else Begin
    form1.fImage.Canvas.Draw(0, 0, FImageBackup);
  End;
  form1.ReCreateStretchBMP();
  form1.PaintBox1.Invalidate;
End;

Procedure TForm9.ResetSettings();
Begin
  CheckBox1.Checked := true;
  CheckBox2.Checked := false;
  ComboBox1.ItemIndex := 4;
  CheckBox3.Checked := false;
  TrackBar1.Position := 255;
  CheckBox3.Checked := false;
  TrackBar2.Position := 0;
  DoTheMagic(false);
End;

End.

