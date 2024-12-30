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
Unit Unit12;

{$MODE ObjFPC}{$H+}

Interface

Uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, printers, LResources, Sudoku4x4;

Type
  TForm12 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Procedure Button3Click(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

Var
  Form12: TForm12;
  P: TPrinter;

Implementation

Uses
  math
  , usudoku
  , Unit6
  , Unit10
  , Unit11
  , Unit15
  ;

{$R *.lfm}

Procedure Drucke(rect: Trect; sudo: T4field);
Var
  breite: integer;
  yo, Textsize, xo, xx, yy, x, y, z: integer;
Begin
  // Dafür sorgen das unser Rect imemr Optimal ist
  If Rect.top > Rect.bottom Then Begin
    z := Rect.top;
    Rect.top := Rect.bottom;
    rect.bottom := z;
  End;
  If Rect.left > Rect.right Then Begin
    z := Rect.left;
    Rect.left := Rect.right;
    rect.right := z;
  End;
  // Ermitteln der FeldBreite
  breite := min(abs(rect.Left - rect.Right) Div 18, abs(rect.top - rect.Bottom) Div 18);
  // Ermitteln des Offset für die Höhe und Breite
  xo := -breite Div 4 + rect.left;
  yo := round((abs(rect.top - rect.Bottom) - (Breite * 16.5)) / 2) + rect.top;
  Textsize := 1;
  p.canvas.Font.Size := Textsize;
  While p.canvas.TextHeight('8') < Breite - (Breite Div 4) Do Begin
    inc(Textsize);
    p.canvas.Font.Size := Textsize;
  End;
  // malen des Gitters und der ganzen sachen
  p.canvas.pen.color := clblack;
  p.canvas.pen.width := Form10.ScrollBar1.position;
  p.canvas.Brush.Style := bsclear;
  For xx := 0 To 3 Do
    For yy := 0 To 3 Do
      For x := 0 To 3 Do
        For y := 0 To 3 Do Begin
          // Malen des Rahmens für das Feld [xx*4+x,yy*4+y]
          p.canvas.rectangle(xo + breite + x * breite + round(XX * breite * 4.25), yo + y * breite + round(yy * (breite * 4.25)), xo + Breite + (x + 1) * breite + round(XX * breite * 4.25), yo + (y + 1) * breite + round(yy * breite * 4.25));
          // Malen einer Zahl
          If Sudo[xx * 4 + x, yy * 4 + y].Value <> 0 Then Begin
            p.canvas.Font.Size := Textsize;
            p.canvas.textout((breite - P.canvas.TextWidth(inttostr(Sudo[xx * 4 + x, yy * 4 + y].Value))) Div 2 + xo + breite + x * breite + round(XX * breite * 4.25), yo + (breite - P.canvas.Textheight(inttostr(Sudo[xx * 4 + x, yy * 4 + y].Value))) Div 2 + y * breite + round(yy * (breite * 4.25)), inttostr(Sudo[xx * 4 + x, yy * 4 + y].value));
          End;
        End;
End;

Procedure TForm12.Button3Click(Sender: TObject);
Begin
  Close;
End;

Procedure TForm12.Button1Click(Sender: TObject);
Var
  Stop, i: Integer;
Begin
  // Fals der Druckauftrag abgebrochen werden soll
  p := Printer; //TPrinter.create;
  p.PrinterIndex := -1;
  form10.ScrollBar1.position := 1;
  Form10.ComboBox1.Items := p.Printers;
  Form10.Edit1.text := '1';
  If Form10.ComboBox1.Items.count = 0 Then Begin
    showmessage('No printer found');
    exit;
  End;
  Form10.ComboBox1.Text := Form10.ComboBox1.items[p.PrinterIndex];
  // Aufruf des Druckdialoges
  If Form10.showmodal = mrOK Then Begin
    stop := strtointdef(form10.Edit1.text, 1);
    p.PrinterIndex := form10.combobox1.ItemIndex;
    // Einstellen Hochformat
    p.Orientation := poPortrait;
    // Name des Druckauftrages
    p.Title := 'Sudoku ver. : ' + ver + ' by Corpsman | Targetsoft |';
    P.BeginDoc;
    // Ermöglichen von Mehrfach ausdrucken
    For I := 1 To Stop Do Begin
      // Start Druckauftrag
      // Drucken des Feldes
      Drucke(rect(0, 0, p.Pagewidth, p.Pageheight), fourfield);
      // Ausdrucken der Werbung
      PrintAdvertising;
      // Neue seite bei mehrfach ausdrucken
      If I <> Stop Then
        p.NewPage;
    End;
    // Druckauftrag beenden
    p.EndDoc;
    // Freigeben des Druckers
    // Schliesen des Fenster's
    Close;
  End;
End;

Procedure TForm12.Button2Click(Sender: TObject);
Var
  f: T4field;
  Stop, i: Integer;
Begin
  Form15.showmodal;
  If CreaterOptions <= 0 Then exit;
  // Fals der Druckauftrag abgebrochen werden soll
  p := Printer; //TPrinter.create;
  p.PrinterIndex := -1;
  form10.ScrollBar1.position := 1;
  Form10.ComboBox1.Items := p.Printers;
  Form10.Edit1.text := '1';
  If Form10.ComboBox1.Items.count = 0 Then Begin
    showmessage('No printer found');
    exit;
  End;
  Form10.ComboBox1.Text := Form10.ComboBox1.items[p.PrinterIndex];
  // Aufruf des Druckdialoges
  If Form10.showmodal = mrOK Then Begin
    stop := strtointdef(form10.Edit1.text, 1);
    New4Field(f, CreaterOptions);
    // Wenn nicht abgebrochen wurde kann gedruckt werden.
    If Not zwangsabbruch Then Begin
      p := TPrinter.create;
      // Name des Druckauftrages
      p.Title := 'Sudoku ver. : ' + ver + ' by Corpsman | Targetsoft |';
      // Einstellen Hochformat
      p.Orientation := poPortrait;
      // Start Druckauftrag
      P.BeginDoc;
      // Ermöglichen von Mehrfach ausdrucken
      For I := 1 To Stop Do Begin
        Drucke(rect(0, 0, p.Pagewidth, p.Pageheight), f);
        // Ausdrucken der Werbung
        PrintAdvertising();
        // Neue seite bei mehrfach ausdrucken
        If I <> Stop Then
          p.NewPage;
      End;
      // Druckauftrag beenden
      p.EndDoc;
      // Freigabe des Feldes
      // Schliesen des Fenster's
      Close;
    End
    Else Begin
      Showmessage('You Canceled this will Cancel the Print job too.');
    End;
  End;
End;

End.

