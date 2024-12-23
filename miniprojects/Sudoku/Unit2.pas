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
Unit Unit2;

{$MODE ObjFPC}{$H+}

Interface

Uses
  SysUtils, Graphics, Forms, Classes, Controls, Dialogs, ExtCtrls,
  StdCtrls, LResources, usudoku;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    PaintBox1: TPaintBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Button1: TButton;
    Button2: TButton;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Shape5: TShape;
    Shape6: TShape;
    Shape7: TShape;
    Shape8: TShape;
    Shape9: TShape;
    Shape10: TShape;
    Shape11: TShape;
    Shape12: TShape;
    Shape13: TShape;
    Shape14: TShape;
    Label13: TLabel;
    ColorDialog1: TColorDialog;
    Button3: TButton;
    Procedure PaintBox1Paint(Sender: TObject);
    Procedure Shape2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
  private
    { Private-Deklarationen }
    fRepaintEvent: TNotifyEvent;
  public
    { Public-Deklarationen }
    Procedure Init(RepaintEvent: TNotifyEvent);
  End;

Var
  Form2: TForm2;

Implementation

{$R *.lfm}

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  Caption := 'Sudoku ver. : ' + ver + ' Color Options';
End;

Procedure TForm2.Shape2Click(Sender: TObject);
Begin
  // Auswahl einer Farbe
  Colordialog1.color := TShape(Sender).Canvas.brush.color;
  If Colordialog1.Execute Then Begin
    TShape(Sender).brush.color := Colordialog1.color;
    PaintBox1.Invalidate;
  End;
End;

Procedure TForm2.PaintBox1Paint(Sender: TObject);
Var
  x, y: integer;
Begin
  // Zeichnen des Vorschau bildes
  With PaintBox1.canvas Do Begin
    x := 6;
    y := 6;
    // Der Background
    pen.color := clblack;
    brush.style := bssolid;
    brush.color := Shape14.brush.color;
    Rectangle(0, 0, 193, 193);
    // das Gitter, bzw die Hintergünde des Gitters
    pen.color := Shape9.brush.color;
    brush.color := Shape2.brush.color;
    rectangle(x, y, x + 60, y + 60);
    brush.color := Shape6.brush.color;
    rectangle(x + 60, y, x + 120, y + 60);
    rectangle(x, y + 60, x + 60, y + 120);
    brush.color := Shape3.brush.color;
    rectangle(x + 120, y, x + 180, y + 60);
    brush.color := Shape5.brush.color;
    rectangle(x + 60, y + 60, x + 120, y + 120);
    rectangle(x + 120, y + 60, x + 180, y + 120);
    rectangle(x + 60, y + 120, x + 120, y + 180);
    rectangle(x + 120, y + 120, x + 180, y + 180);
    brush.color := Shape7.brush.color;
    rectangle(x, y + 120, x + 60, y + 180);
    brush.color := Shape13.brush.color;
    Pen.color := Shape13.brush.color;
    Ellipse(x + 61, y + 61, x + 119, y + 119);
    Ellipse(x + 2, y + 2, x + 18, y + 18);
    Ellipse(x + 122, y + 2, x + 138, y + 18);
    Ellipse(x + 2, y + 122, x + 18, y + 138);
    Brush.Style := bsclear;
    // Die Pencil Einträge
    font.Size := 12;
    font.color := Shape11.brush.color;
    x := 11;
    textout(x + 1, y + 1, '1');
    textout(x + 1, y + 41, '7');
    textout(x + 21, y + 1, '2');
    textout(x + 21, y + 21, '5');
    textout(x + 21, y + 41, '8');
    textout(x + 41, y + 1, '3');
    textout(x + 41, y + 21, '6');
    textout(x + 41, y + 41, '9');
    textout(x + 121, y + 1, '1');
    textout(x + 121, y + 41, '7');
    textout(x + 141, y + 1, '2');
    textout(x + 141, y + 21, '5');
    textout(x + 141, y + 41, '8');
    textout(x + 161, y + 21, '6');
    textout(x + 161, y + 41, '9');
    Font.color := Shape12.brush.color;
    textout(x + 1, y + 81, '4');
    textout(x + 1, y + 101, '7');
    textout(x + 1, y + 121, '1');
    textout(x + 1, y + 141, '4');
    textout(x + 1, y + 161, '7');
    textout(x + 21, y + 61, '2');
    textout(x + 21, y + 81, '5');
    textout(x + 21, y + 101, '8');
    textout(x + 21, y + 141, '5');
    textout(x + 21, y + 161, '8');
    textout(x + 41, y + 81, '6');
    textout(x + 41, y + 101, '9');
    textout(x + 41, y + 121, '3');
    textout(x + 41, y + 141, '6');
    textout(x + 41, y + 161, '9');
    textout(x + 121, y + 141, '4');
    textout(x + 121, y + 161, '7');
    textout(x + 141, y + 141, '5');
    textout(x + 141, y + 161, '8');
    textout(x + 161, y + 141, '6');
    textout(x + 161, y + 161, '9');
    // Die Feld nummern
    x := 6;
    font.size := 30;
    font.Color := Shape10.brush.color;
    textout(x + 80, y + 66, '1');
    textout(x + 80, y + 6, '4');
    font.Color := Shape4.brush.color;
    textout(x + 140, y + 66, '3');
    font.Color := Shape8.brush.color;
    textout(x + 80, y + 126, '2');
  End;
End;

Procedure TForm2.Button3Click(Sender: TObject);
Begin
  // Die Standert Farben
  Shape2.Brush.color := clbtnface;
  Shape3.Brush.color := clgray;
  Shape4.Brush.color := clyellow;
  Shape5.Brush.color := clBlue;
  Shape6.Brush.color := clnavy;
  Shape7.Brush.color := clgreen;
  Shape8.Brush.color := clblack;
  Shape9.Brush.color := Clblack;
  Shape10.Brush.color := $C08000;
  Shape11.Brush.color := clmaroon;
  Shape12.Brush.color := $4080FF;
  Shape13.Brush.color := CLaqua;
  Shape14.Brush.color := clbtnface;
  PaintBox1.Invalidate;
End;

Procedure TForm2.FormShow(Sender: TObject);
Begin
  PaintBox1.Invalidate;
End;

Procedure TForm2.Init(RepaintEvent: TNotifyEvent);
Begin
  fRepaintEvent := RepaintEvent;
End;

End.

