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
Unit Unit8;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  TAGraph, TASeries, TATransformations, TARadialSeries;

Type

  { TForm8 }

  TForm8 = Class(TForm)
    Chart1: TChart;
    Grau: TLineSeries;
    Label2: TLabel;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    Rot: TLineSeries;
    Gruen: TLineSeries;
    Blau: TLineSeries;
    KomGrau: TLineSeries;
    KomRot: TLineSeries;
    KomGruen: TLineSeries;
    KomBlau: TLineSeries;
    ChartAxisTransformations1: TChartAxisTransformations;
    ChartAxisTransformations1LogarithmAxisTransform1: TLogarithmAxisTransform;
    ComboBox1: TComboBox;
    Label1: TLabel;
    Procedure ComboBox1Change(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure RadioGroup1Click(Sender: TObject);
  private
  public
    Procedure RefreshData();

  End;

Var
  Form8: TForm8;

Implementation

{$R *.lfm}

Uses unit1, IntfGraphics, ugraphics, fpImage, Math;

Type
  TColorInfo = Record
    minV, maxV: integer; // Index der 1. bzw letzten Zahl <> 0
    lums: Array[0..255] Of Integer; // Anzahl der Werte je Farbwert
    vSum: integer; // Summe über alle Werte des Kanals
    cSum: Integer; // Anzahl der von 0 Verschiedenen Werte
    Mean: Single; // vSum / cSum
    Deviation: Single; // Die Mittlere Abweichung
    Max_y: integer; // Höchster Wert in lums
  End;

Var
  C: Array[0..3] Of TColorInfo; // 0 = Red, 1 = Green, 2 = Blue, 3 = Gray
  Kom_C_max_Y: Array[0..4] Of Integer;
  Labels: Array[0..4] Of String; // 0 = Red, 1 = Green, 2 = Blue, 3 = Gray, 4 = All

  { TForm8 }

Procedure TForm8.FormCreate(Sender: TObject);
Begin
  caption := 'Histogram';
End;

Procedure TForm8.RadioGroup1Click(Sender: TObject);
Begin
  ChartAxisTransformations1LogarithmAxisTransform1.Enabled := RadioGroup1.ItemIndex = 1;
End;

Procedure TForm8.ComboBox1Change(Sender: TObject);
Var
  index, yMax: integer;
Begin
  Case ComboBox1.ItemIndex Of
    0: Begin
        index := 3;
        ymax := c[3].Max_y;
      End;
    1: Begin
        index := 0;
        ymax := c[0].Max_y;
      End;
    2: Begin
        index := 1;
        ymax := c[1].Max_y;
      End;
    3: Begin
        index := 2;
        ymax := c[2].Max_y;
      End;
    4: Begin
        index := 4;
        ymax := max(c[0].Max_y, max(c[1].Max_y, c[2].Max_y));
      End;
  End;
  If RadioGroup2.ItemIndex = 0 Then Begin
    Rot.ShowLines := (ComboBox1.ItemIndex = 1) Or (ComboBox1.ItemIndex = 4);
    Gruen.ShowLines := (ComboBox1.ItemIndex = 2) Or (ComboBox1.ItemIndex = 4);
    Blau.ShowLines := (ComboBox1.ItemIndex = 3) Or (ComboBox1.ItemIndex = 4);
    Grau.ShowLines := ComboBox1.ItemIndex = 0;
    KomRot.ShowLines := false;
    KomGruen.ShowLines := false;
    KomBlau.ShowLines := false;
    KomGrau.ShowLines := false;
  End
  Else Begin
    Rot.ShowLines := false;
    Gruen.ShowLines := false;
    Blau.ShowLines := false;
    Grau.ShowLines := false;
    KomRot.ShowLines := (ComboBox1.ItemIndex = 1) Or (ComboBox1.ItemIndex = 4);
    KomGruen.ShowLines := (ComboBox1.ItemIndex = 2) Or (ComboBox1.ItemIndex = 4);
    KomBlau.ShowLines := (ComboBox1.ItemIndex = 3) Or (ComboBox1.ItemIndex = 4);
    KomGrau.ShowLines := ComboBox1.ItemIndex = 0;
    If ComboBox1.ItemIndex = 4 Then Begin
      ymax := max(Kom_C_max_Y[0], max(Kom_C_max_Y[1], Kom_C_max_Y[2]));
    End
    Else Begin
      ymax := Kom_C_max_Y[index];
    End;
  End;
  label1.Caption := 'Info:' + LineEnding + Labels[index];

  Chart1.AxisList[0].Range.Max := yMax;
  Chart1.AxisList[0].Range.UseMax := true;
End;

Procedure TForm8.RefreshData();

  Procedure Update(Index, ColValue: Integer);
  Begin
    c[index].lums[ColValue] := c[index].lums[ColValue] + 1;
    c[index].Max_y := max(c[index].Max_y, c[index].lums[ColValue]);
    c[index].minV := min(c[index].minV, ColValue);
    c[index].maxV := max(c[index].maxV, ColValue);
    If ColValue <> 0 Then Begin
      c[index].vSum := c[index].vSum + ColValue;
      c[index].cSum := c[index].cSum + 1;
    End;
  End;

Var
  i, j, lum: Integer;
  intf: TLazIntfImage;
  col: TFPColor;
  f: Single;
Begin
  Grau.Clear; // Luminanz
  Rot.Clear; // R
  Gruen.Clear; // G
  Blau.Clear; // B
  KomGrau.Clear; // Luminanz
  KomRot.Clear; // R
  KomGruen.Clear; // G
  KomBlau.Clear; // B
  For i := 0 To 3 Do Begin
    Kom_C_max_Y[i] := 0;
    c[i].minV := 256;
    c[i].maxV := -1;
    c[i].cSum := 0;
    c[i].vSum := 0;
    c[i].Max_y := 0;
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
  intf.Free;
  For i := 0 To 255 Do Begin
    Grau.AddXY(i, c[3].lums[i]);
    Rot.AddXY(i, c[0].lums[i]);
    Gruen.AddXY(i, c[1].lums[i]);
    Blau.AddXY(i, c[2].lums[i]);

    Kom_C_max_Y[0] := Kom_C_max_Y[0] + c[0].lums[i];
    Kom_C_max_Y[1] := Kom_C_max_Y[1] + c[1].lums[i];
    Kom_C_max_Y[2] := Kom_C_max_Y[2] + c[2].lums[i];
    Kom_C_max_Y[3] := Kom_C_max_Y[3] + c[3].lums[i];

    KomGrau.AddXY(i, Kom_C_max_Y[3]);
    KomRot.AddXY(i, Kom_C_max_Y[0]);
    KomGruen.AddXY(i, Kom_C_max_Y[1]);
    KomBlau.AddXY(i, Kom_C_max_Y[2]);
  End;
  For i := 0 To 3 Do Begin
    // Berechnen
    c[i].Mean := c[i].vSum / max(1, c[i].cSum);
    f := 0;
    For j := 0 To 255 Do Begin
      f := f + abs(c[i].lums[j] - c[i].Mean);
    End;
    c[i].Deviation := f / max(1, c[i].cSum);
    // Label erstellen
    Labels[i] :=
      'Mean: ' + format('%0.1f', [c[i].mean]) + LineEnding
      + 'Deviation: ' + format('%0.2f', [c[i].Deviation]) + LineEnding
      + 'Minimum: ' + inttostr(c[i].minV) + LineEnding
      + 'Maximum: ' + inttostr(c[i].maxV) + LineEnding
      + ' '; // Fix GTK RenderBug
    //  ;
  End;
  Labels[4] :=
    'Red:' + LineEnding + Labels[0] + LineEnding +
    'Green:' + LineEnding + Labels[1] + LineEnding +
    'Blue:' + LineEnding + Labels[2] + LineEnding +
    ' '; // Fix GTK RenderBug

  ComboBox1Change(Nil);
  RadioGroup1Click(Nil);
End;

End.

