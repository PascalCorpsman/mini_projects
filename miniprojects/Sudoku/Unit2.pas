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
  {Windows,} SysUtils, Graphics, Forms, Classes, Controls, Dialogs, ExtCtrls,
  StdCtrls, LResources;

Type
  TForm2 = Class(TForm)
    Image1: TImage;
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
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    Image8: TImage;
    Image9: TImage;
    Image10: TImage;
    Image11: TImage;
    Image12: TImage;
    Image13: TImage;
    Label13: TLabel;
    Image14: TImage;
    ColorDialog1: TColorDialog;
    Button3: TButton;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Image2Click(Sender: TObject);
    Procedure FormPaint(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

Var
  Form2: TForm2;

Implementation

Uses
  usudoku
  , unit1;

{$R *.lfm}

// Anzeigen des Vorschau Fenster's hier sind alle Effecte Hardcoded

Procedure Redraw;
Var
  x, y: integer;
Begin
  With form2 Do Begin
    // Zeichnen der Farbauswahl
    For x := 2 To 14 Do
      Timage(findcomponent('Image' + inttostr(x))).canvas.Rectangle(0, 0, 30, 13);
    // Zeichnen des Vorschau bildes
    With image1.canvas Do Begin
      x := 6;
      y := 6;
      // Der Background
      pen.color := clblack;
      brush.style := bssolid;
      brush.color := image14.canvas.brush.color;
      Rectangle(0, 0, 193, 193);
      // das Gitter, bzw die Hintergünde des Gitters
      pen.color := image9.canvas.brush.color;
      brush.color := image2.canvas.brush.color;
      rectangle(x, y, x + 60, y + 60);
      brush.color := image6.canvas.brush.color;
      rectangle(x + 60, y, x + 120, y + 60);
      rectangle(x, y + 60, x + 60, y + 120);
      brush.color := image3.canvas.brush.color;
      rectangle(x + 120, y, x + 180, y + 60);
      brush.color := image5.canvas.brush.color;
      rectangle(x + 60, y + 60, x + 120, y + 120);
      rectangle(x + 120, y + 60, x + 180, y + 120);
      rectangle(x + 60, y + 120, x + 120, y + 180);
      rectangle(x + 120, y + 120, x + 180, y + 180);
      brush.color := image7.canvas.brush.color;
      rectangle(x, y + 120, x + 60, y + 180);
      brush.color := image13.canvas.brush.color;
      Pen.color := image13.canvas.brush.color;
      Ellipse(x + 61, y + 61, x + 119, y + 119);
      Ellipse(x + 2, y + 2, x + 18, y + 18);
      Ellipse(x + 122, y + 2, x + 138, y + 18);
      Ellipse(x + 2, y + 122, x + 18, y + 138);
      Brush.Style := bsclear;
      // Die Pencil einträge
      font.Size := 12;
      font.color := image11.canvas.brush.color;
      x := 11;
      textout(x + 1, y + 1, '1');
      textout(x + 1, y + 21, '4');
      textout(x + 1, y + 41, '7');
      textout(x + 21, y + 1, '2');
      textout(x + 21, y + 21, '5');
      textout(x + 21, y + 41, '8');
      textout(x + 41, y + 1, '3');
      textout(x + 41, y + 21, '6');
      textout(x + 41, y + 41, '9');
      textout(x + 121, y + 1, '1');
      textout(x + 121, y + 21, '4');
      textout(x + 121, y + 41, '7');
      textout(x + 141, y + 1, '2');
      textout(x + 141, y + 21, '5');
      textout(x + 141, y + 41, '8');
      textout(x + 161, y + 21, '6');
      textout(x + 161, y + 41, '9');
      Font.color := image12.canvas.brush.color;
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
      textout(x + 61, y + 21, '4');
      textout(x + 61, y + 41, '7');
      textout(x + 81, y + 21, '5');
      textout(x + 81, y + 41, '8');
      textout(x + 101, y + 1, '3');
      textout(x + 101, y + 21, '6');
      textout(x + 101, y + 41, '9');
      textout(x + 121, y + 141, '4');
      textout(x + 121, y + 161, '7');
      textout(x + 141, y + 141, '5');
      textout(x + 141, y + 161, '8');
      textout(x + 161, y + 141, '6');
      textout(x + 161, y + 161, '9');
      // Die Feld nummern
      x := 6;
      font.size := 30;
      font.Color := image10.canvas.brush.color;
      textout(x + 80, y + 66, '1');
      font.Color := image4.canvas.brush.color;
      textout(x + 140, y + 66, '3');
      font.Color := image8.canvas.brush.color;
      textout(x + 80, y + 126, '2');
    End;
    Button1.Repaint;
    Button2.Repaint;
    Button3.Repaint;

  End;
End;

Procedure TForm2.Button1Click(Sender: TObject);
Var
  x: integer;
Begin
  // Übernehmen der Farben in die Variablen der Form1
  Bretthintergrundfarbe1 := Form2.image2.canvas.brush.color;
  Bretthintergrundfarbe2 := Form2.image3.canvas.brush.color;
  Maybeedcolor := Form2.image4.canvas.brush.color;
  MarkedColor1 := Form2.image5.canvas.brush.color;
  MarkedColor2 := Form2.image6.canvas.brush.color;
  CursorMarker := Form2.image7.canvas.brush.color;
  Fixedcolor := Form2.image8.canvas.brush.color;
  Gitterfarbe := Form2.image9.canvas.brush.color;
  FontColor := Form2.image10.canvas.brush.color;
  Pencilcolor := Form2.image11.canvas.brush.color;
  PencilcolorMarked := Form2.image12.canvas.brush.color;
  LightenColor := Form2.image13.canvas.brush.color;
  FormBackground := Form2.image14.canvas.brush.color;
  // Sonderfall Hintergrund = Schwarz
  For x := 1 To 6 Do Begin
    TCheckbox(Form1.findcomponent('Checkbox' + inttostr(x))).color := FormBackground;
    If FormBackground = clblack Then
      TCheckbox(Form1.findcomponent('Checkbox' + inttostr(x))).font.color := clwhite
    Else
      TCheckbox(Form1.findcomponent('Checkbox' + inttostr(x))).font.color := clblack;
  End;
  // Neuzeichnen
  form1.Drawfield(Nil);
  // Raus
  close;
End;

Procedure TForm2.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm2.Image2Click(Sender: TObject);
Begin
  // Auswahl einer Farbe
  Colordialog1.color := Timage(Sender).Canvas.brush.color;
  If Colordialog1.Execute Then Begin
    Timage(Sender).Canvas.brush.color := Colordialog1.color;
    Redraw; // Neu Zeichnen des Vorschau fensters
  End;
End;

Procedure TForm2.FormPaint(Sender: TObject);
Begin
  // neuzeichnen
//  Redraw;
End;

Procedure TForm2.Button3Click(Sender: TObject);
Begin
  // Die Standert Farben
  image2.canvas.Brush.color := clbtnface;
  image3.canvas.Brush.color := clgray;
  image4.canvas.Brush.color := clyellow;
  image5.canvas.Brush.color := clBlue;
  image6.canvas.Brush.color := clnavy;
  image7.canvas.Brush.color := clgreen;
  image8.canvas.Brush.color := clblack;
  image9.canvas.Brush.color := Clblack;
  image10.canvas.Brush.color := $00C08000;
  image11.canvas.Brush.color := clmaroon;
  image12.canvas.Brush.color := $004080FF;
  image13.canvas.Brush.color := CLaqua;
  image14.canvas.Brush.color := clbtnface;
  redraw;
End;

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  Caption := 'Sudoku ver. : ' + ver + ' Color Options';
End;

Procedure TForm2.FormShow(Sender: TObject);
Begin
  redraw;
End;

End.

