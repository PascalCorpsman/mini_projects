(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of FIR IIR                                               *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit2;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Menus, usolver, math;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    Button1: TButton;
    MenuItem1: TMenuItem;
    PaintBox1: TPaintBox;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    Procedure Button1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure MenuItem1Click(Sender: TObject);
    Procedure PaintBox1Paint(Sender: TObject);
    Procedure PaintBox1Resize(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  End;

Var
  Form2: TForm2;
  bitmap: Tbitmap = Nil;
  renderproperties: Record
    xmin: TDatatype;
    xmax: TDatatype;
    ymin: TDatatype;
    ymax: TDatatype;
    show_orig: Boolean;
    orig_color: TColor;
    dest_color: TColor;
    show_x_achis: Boolean;
    show_y_achis: Boolean;
    mark_x_achis: Boolean;
    mark_y_achis: Boolean;
    mark_color: TColor;
    mark_x_delta: TDatatype;
    mark_y_delta: TDatatype;
    enhance_mark_x_achis: Boolean;
    enhance_mark_y_achis: Boolean;
    enhance_mark_color: TColor;
    enhance_mark_x_delta: TDatatype;
    enhance_mark_y_delta: TDatatype;
  End;
  Need_2d_Output: Boolean;

Implementation

{$R *.lfm}

Uses unit1;

{ TForm2 }

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  caption := 'Graphic output';
  bitmap := TBitmap.Create;
  bitmap.Width := PaintBox1.Width;
  bitmap.Height := PaintBox1.Height;
  SaveDialog1.InitialDir := ExtractFilePath(paramstr(0));
End;

Procedure TForm2.FormDestroy(Sender: TObject);
Begin
  bitmap.free;
  bitmap := Nil;
End;

Procedure TForm2.MenuItem1Click(Sender: TObject);
Begin
  If SaveDialog1.Execute Then Begin
    bitmap.SaveToFile(SaveDialog1.FileName);
  End;
End;

(*
 *       vmin / rmin                      vmax / rmax
 *       |                                |
 *    ---------------------------------------
 *                  |
 *                  v / result
 *
 * Berechnet den Wert von result so, dass er an der entsprechend
 * gleichen Stelle in rmin / rmax liegt, wie  v in vmin / vmax ist.
 *)

Function convert_dimension(vmin, vmax, v: TDatatype; rmin, rmax: integer): integer;
Begin
  If (vmax - vmin = 0) Then Begin
    result := rmin;
    exit;
  End
  Else Begin
    result := round((((v - vmin) * (rmax - rmin)) / (vmax - vmin)) + rmin);
  End;
End;

Procedure TForm2.PaintBox1Paint(Sender: TObject);
Var
  x, y, i: integer;
  fx, fy: TDatatype;
  h, w: integer;
  render_outside: Boolean;
Begin
  If assigned(bitmap) Then Begin
    If Need_2d_Output Then Begin
      PaintBox1.Canvas.StretchDraw(rect(0, 0, PaintBox1.Width, PaintBox1.Height), form1.Daten_D2);
    End
    Else Begin
      // Alles Löschen
      bitmap.Canvas.Brush.Color := clwhite;
      bitmap.Canvas.Brush.Style := bsSolid;
      bitmap.Canvas.Rectangle(-1, -1, bitmap.Width + 1, bitmap.height + 1);
      h := bitmap.Height;
      w := bitmap.Width;
      // -----------------Anzeigen des Grids und aller seiner Nuoncen ------------
      // Anzeigen der enhanced Markierungen y - Achse
      If renderproperties.enhance_mark_y_achis And (renderproperties.enhance_mark_y_delta > 0) Then Begin
        bitmap.Canvas.Pen.Color := renderproperties.enhance_mark_color;
        fy := 0;
        // Anfahren des 1. delta Punktes >= xmin
        While fy > min(renderproperties.ymin, renderproperties.ymax) Do
          fy := fy - renderproperties.enhance_mark_y_delta;
        While fy < min(renderproperties.ymin, renderproperties.ymax) Do
          fy := fy + renderproperties.enhance_mark_y_delta;
        While fy < max(renderproperties.ymin, renderproperties.ymax) Do Begin
          y := convert_dimension(renderproperties.ymin, renderproperties.ymax, fy, h, 0);
          bitmap.Canvas.MoveTo(0, y);
          bitmap.Canvas.lineTo(w, y);
          fy := fy + renderproperties.enhance_mark_y_delta;
        End;
      End;
      // Wenn die x-Achse Echt Positioniert werden kann
      If (min(renderproperties.ymin, renderproperties.ymax) <= 0) And
        (max(renderproperties.ymin, renderproperties.ymax) >= 0) Then Begin
        y := convert_dimension(renderproperties.ymin, renderproperties.ymax, 0, h, 0);
        render_outside := false;
      End
      Else Begin
        // Wenn die x- Achse eigentlich nicht sicht bar wäre
        y := h - 10;
        render_outside := true;
      End;
      // Anzeigen der enhanced Markierungen
      If renderproperties.enhance_mark_x_achis And (renderproperties.enhance_mark_x_delta > 0) Then Begin
        bitmap.Canvas.Pen.Color := renderproperties.enhance_mark_color;
        fx := 0;
        // Anfahren des 1. delta Punktes >= xmin
        While fx > min(renderproperties.xmin, renderproperties.xmax) Do
          fx := fx - renderproperties.enhance_mark_x_delta;
        While fx < min(renderproperties.xmin, renderproperties.xmax) Do
          fx := fx + renderproperties.enhance_mark_x_delta;
        While fx < max(renderproperties.xmin, renderproperties.xmax) Do Begin
          x := convert_dimension(renderproperties.xmin, renderproperties.xmax, fx, 0, w);
          bitmap.Canvas.MoveTo(x, 0);
          bitmap.Canvas.lineTo(x, h);
          fx := fx + renderproperties.enhance_mark_x_delta;
        End;
      End;
      bitmap.Canvas.Pen.Color := clblack;
      // Anzeigen der x-Achse
      If renderproperties.show_x_achis Then Begin
        // Malen der x- Achse
        If render_outside Then Begin
          bitmap.Canvas.Pen.Style := psDashDot;
        End
        Else Begin
          bitmap.Canvas.Pen.Style := psSolid;
        End;
        bitmap.Canvas.MoveTo(0, y);
        bitmap.Canvas.lineTo(w, y);
      End;
      bitmap.Canvas.Pen.Style := psSolid;
      // Anzeigen der Markierungen
      If renderproperties.mark_x_achis And (renderproperties.mark_x_delta > 0) Then Begin
        bitmap.Canvas.Pen.Color := renderproperties.mark_color;
        fx := 0;
        // Anfahren des 1. delta Punktes >= xmin
        While fx > min(renderproperties.xmin, renderproperties.xmax) Do
          fx := fx - renderproperties.mark_x_delta;
        While fx < min(renderproperties.xmin, renderproperties.xmax) Do
          fx := fx + renderproperties.mark_x_delta;
        While fx < max(renderproperties.xmin, renderproperties.xmax) Do Begin
          x := convert_dimension(renderproperties.xmin, renderproperties.xmax, fx, 0, w);
          bitmap.Canvas.MoveTo(x, y - 4);
          bitmap.Canvas.lineTo(x, y + 4);
          fx := fx + renderproperties.mark_x_delta;
        End;
      End;
      // Wenn die y-Achse Echt Positioniert werden kann
      If (min(renderproperties.xmin, renderproperties.xmax) <= 0) And
        (max(renderproperties.xmin, renderproperties.xmax) >= 0) Then Begin
        x := convert_dimension(renderproperties.xmin, renderproperties.xmax, 0, 0, w);
        render_outside := false;
      End
      Else Begin
        // Wenn die y- Achse eigentlich nicht sicht bar wäre
        x := 10;
        render_outside := true;
      End;
      bitmap.Canvas.Pen.Color := clblack;
      If renderproperties.mark_y_achis And (renderproperties.mark_y_delta > 0) Then Begin
        bitmap.Canvas.Pen.Color := renderproperties.mark_color;
        fy := 0;
        // Anfahren des 1. delta Punktes >= xmin
        While fy > min(renderproperties.ymin, renderproperties.ymax) Do
          fy := fy - renderproperties.mark_y_delta;
        While fy < min(renderproperties.ymin, renderproperties.ymax) Do
          fy := fy + renderproperties.mark_y_delta;
        While fy < max(renderproperties.ymin, renderproperties.ymax) Do Begin
          y := convert_dimension(renderproperties.ymin, renderproperties.ymax, fy, h, 0);
          bitmap.Canvas.MoveTo(x - 4, y);
          bitmap.Canvas.lineTo(x + 4, y);
          fy := fy + renderproperties.mark_y_delta;
        End;
      End;
      // Anzeigen der y-Achse
      If renderproperties.show_y_achis Then Begin
        // Malen der y- Achse
        If render_outside Then Begin
          bitmap.Canvas.Pen.Style := psDashDot;
        End
        Else Begin
          bitmap.Canvas.Pen.Style := psSolid;
        End;
        bitmap.Canvas.MoveTo(x, 0);
        bitmap.Canvas.lineTo(x, h);
      End;
      bitmap.Canvas.Pen.Style := psSolid;
      // Rendern der Original Funktion
      If renderproperties.show_orig And assigned(form1.Samples) And (form1.sample_cnt > 0) Then Begin
        bitmap.Canvas.Pen.Color := renderproperties.orig_color;
        x := convert_dimension(renderproperties.xmin, renderproperties.xmax, form1.Samples[0].x, 0, w);
        y := convert_dimension(renderproperties.ymin, renderproperties.ymax, form1.Samples[0].y, h, 0);
        bitmap.Canvas.moveto(x, y);
        For i := 1 To form1.sample_cnt - 1 Do Begin
          x := convert_dimension(renderproperties.xmin, renderproperties.xmax, form1.Samples[i].x, 0, w);
          y := convert_dimension(renderproperties.ymin, renderproperties.ymax, form1.Samples[i].y, h, 0);
          bitmap.Canvas.lineto(x, y);
        End;
      End;
      // Als Allerletztes die Gefilterte Funktion
      If assigned(form1.results) And (form1.sample_cnt > 0) Then Begin
        bitmap.Canvas.Pen.Color := renderproperties.dest_color;
        x := convert_dimension(renderproperties.xmin, renderproperties.xmax, form1.results[0].x, 0, w);
        y := convert_dimension(renderproperties.ymin, renderproperties.ymax, form1.results[0].y, h, 0);
        bitmap.Canvas.moveto(x, y);
        For i := 1 To form1.sample_cnt - 1 Do Begin
          x := convert_dimension(renderproperties.xmin, renderproperties.xmax, form1.results[i].x, 0, w);
          y := convert_dimension(renderproperties.ymin, renderproperties.ymax, form1.results[i].y, h, 0);
          bitmap.Canvas.lineto(x, y);
        End;
      End;
      // Ausgabe auf das Display
      PaintBox1.Canvas.Draw(0, 0, bitmap);
    End;
  End
  Else Begin
    caption := 'Bitmap already freed';
  End;
End;

Procedure TForm2.PaintBox1Resize(Sender: TObject);
Begin
  If assigned(bitmap) Then Begin
    bitmap.Width := PaintBox1.Width;
    bitmap.Height := PaintBox1.Height;
    PaintBox1.Invalidate;
  End;
End;

Procedure TForm2.Button1Click(Sender: TObject);
Begin
  visible := false;
End;

End.

