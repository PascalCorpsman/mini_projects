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
Unit Unit9;

{$MODE ObjFPC}{$H+}

Interface

Uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, LResources, usudoku;

Type

  { TForm9 }

  TForm9 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    CheckBox1: TCheckBox;
    Procedure Button4Click(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
    fField: TSudoku;
  public
    { Public-Deklarationen }
    Procedure Init(Const aField: TSudoku);
  End;

Var
  Form9: TForm9;

Implementation

Uses
  math
  , Printers
  , Unit1
  , Unit6
  , Unit7
  , Unit10
  ;

{$R *.lfm}

// Druckt ein Sudoku in das Rect das hier angegeben wird, allerdings ohne Begin und Enddock !!!!

Procedure Drucke(Rect: Trect; Sudo: T3field);
Var
  PencilBreite, breite: integer;
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
  breite := min(abs(rect.Left - rect.Right) Div 11, abs(rect.top - rect.Bottom) Div 11);
  PencilBreite := Breite Div 4;
  // Ermitteln des Offset für die Höhe und Breite
  xo := -breite Div 4 + rect.left;
  yo := round((abs(rect.top - rect.Bottom) - (Breite * 9.5)) / 2) + rect.top;
  Textsize := 1;
  Printer.canvas.Font.Size := Textsize;
  While Printer.canvas.TextHeight('8') < Breite - (Breite Div 4) Do Begin
    inc(Textsize);
    Printer.canvas.Font.Size := Textsize;
  End;
  // malen des Gitters und der ganzen sachen
  Printer.canvas.pen.color := clblack;
  Printer.canvas.pen.width := Form10.ScrollBar1.position;
  Printer.canvas.Brush.Style := bsclear;
  For xx := 0 To 2 Do
    For yy := 0 To 2 Do
      For x := 0 To 2 Do
        For y := 0 To 2 Do Begin
          // Malen des Rahmens für das Feld [xx*3+x,yy*3+y]
          Printer.canvas.rectangle(xo + breite + x * breite + round(XX * breite * 3.25), yo + y * breite + round(yy * (breite * 3.25)), xo + Breite + (x + 1) * breite + round(XX * breite * 3.25), yo + (y + 1) * breite + round(yy * breite * 3.25));
          // Malen einer Zahl
          If Sudo[xx * 3 + x, yy * 3 + y].Value <> 0 Then Begin
            Printer.canvas.Font.Size := Textsize;
            Printer.canvas.textout((breite - Printer.canvas.TextWidth(inttostr(Sudo[xx * 3 + x, yy * 3 + y].Value))) Div 2 + xo + breite + x * breite + round(XX * breite * 3.25), yo + (breite - Printer.canvas.Textheight(inttostr(Sudo[xx * 3 + x, yy * 3 + y].Value))) Div 2 + y * breite + round(yy * (breite * 3.25)), substitution[Sudo[xx * 3 + x, yy * 3 + y].value]);
          End
          Else Begin
            // Wenn die Einzelpencil's auch egdruckt werden sollen
            If form9.checkbox1.checked Then Begin
              Printer.canvas.Font.Size := Textsize Div 3;
              For z := 0 To 8 Do Begin
                If Sudo[xx * 3 + x, yy * 3 + y].Pencil[z] Then
                  Printer.canvas.textout(-Printer.canvas.textwidth(substitution[z + 1]) Div 2 + Pencilbreite * ((z Mod 3) + 1) + xo + breite + x * breite + round(XX * breite * 3.25), yo - Printer.canvas.textheight(substitution[z + 1]) Div 2 + Pencilbreite * ((z Div 3) + 1) + y * breite + round(yy * (breite * 3.25)), substitution[z + 1])
              End;
            End;
          End;
        End;
End;

Procedure TForm9.FormCreate(Sender: TObject);
Begin
  fField := TSudoku.Create(3);
End;

Procedure TForm9.FormDestroy(Sender: TObject);
Begin
  fField.Free;
  fField := Nil;
End;

Procedure TForm9.Button4Click(Sender: TObject);
Begin
  Close;
End;

Procedure TForm9.Button1Click(Sender: TObject);
Var
  Stop, i: Integer;
  f: T3Field;
Begin
  // Print Actual Field
  form10.ScrollBar1.position := Druckbreite;
  Form10.ComboBox1.Items := Printer.Printers;
  Form10.Edit1.text := '1';
  If Form10.ComboBox1.Items.count = 0 Then Begin
    showmessage('No printer found');
    exit;
  End;
  Form10.ComboBox1.Text := Form10.ComboBox1.items[Printer.PrinterIndex];
  // Aufruf des Druckdialoges
  If Form10.showmodal = mrOK Then Begin
    stop := strtointdef(form10.Edit1.text, 1);
    Printer.PrinterIndex := form10.combobox1.ItemIndex;
    // Einstellen Hochformat
    Printer.Orientation := poPortrait;
    // Name des Druckauftrages
    Printer.Title := 'Sudoku ver. : ' + ver + ' by Corpsman | www.Corpsman.de';
    Printer.BeginDoc;
    // Ermöglichen von Mehrfach ausdrucken
    For I := 1 To Stop Do Begin
      // Start Druckauftrag
      // Drucken des Feldes
      If checkbox1.checked Then Begin
        fField.ResetAllNumberPencils;
        fField.ClearAllNumberPencils;
      End;
      fField.StoreTo(f);
      Drucke(rect(0, 0, Printer.Pagewidth, Printer.Pageheight), f);
      // Ausdrucken der Werbung
      PrintAdvertising();
      // Neue seite bei mehrfach ausdrucken
      If I <> Stop Then
        Printer.NewPage;
    End;
    // Druckauftrag beenden
    Printer.EndDoc;
    // Schliesen des Fenster's
    Close;
  End;
End;

Procedure TForm9.Button2Click(Sender: TObject);
Var
  Stop, i: Integer;
  field: T3Field;
  s: TSudoku;
Begin
  // Print New Field
  // Falls der Druckauftrag abgebrochen werden soll
  form10.ScrollBar1.position := Druckbreite;
  Form10.ComboBox1.Items := Printer.Printers;
  Form10.Edit1.text := '1';
  If Form10.ComboBox1.Items.count = 0 Then Begin
    showmessage('No printer found');
    exit;
  End;
  Form10.ComboBox1.Text := Form10.ComboBox1.items[Printer.PrinterIndex];
  // Aufruf des Druckdialoges
  If Form10.showmodal = mrOK Then Begin
    stop := strtointdef(form10.Edit1.text, 1);
    // Wegspeichern des Aktuell angezeigten Feldes
    // Einstellen der Erstelloptionen
    s := TSudoku.Create(fField.Dimension);
    form1.Puzzle1Click(Nil);
    form7.Init(s, Nil); // Das muss nach form1.Puzzle1Click(Nil); kommen !
    // Erstellen eines neuen Feldes
    form7.Button1Click(Nil);
    s.CloneFieldFrom(form7.Sudoku);
    // Falls die Pencil's auch gewünscht sind
    If checkbox1.checked Then Begin
      s.ResetAllNumberPencils;
      s.ClearAllNumberPencils;
    End;
    // Wenn nicht abgebrochen wurde kann gedruckt werden.
    If Not zwangsabbruch Then Begin
      Printer.PrinterIndex := form10.combobox1.ItemIndex;
      // Einstellen Hochformat
      Printer.Orientation := poPortrait;
      // Name des Druckauftrages
      Printer.Title := 'Sudoku ver. : ' + ver + ' by Corpsman | www.Corpsman.de';
      // Start Druckauftrag
      Printer.BeginDoc;
      // Ermöglichen von Mehrfach ausdrucken
      For I := 1 To Stop Do Begin
        s.StoreTo(field);
        Drucke(rect(0, 0, Printer.Pagewidth, Printer.Pageheight), field);
        // Ausdrucken der Werbung
        PrintAdvertising();
        // Neue seite bei mehrfach ausdrucken
        If I <> Stop Then
          Printer.NewPage;
      End;
      // Druckauftrag beenden
      Printer.EndDoc;
      // Schliesen des Fenster's
      s.free;
      Close;
    End
    Else Begin
      s.free;
      Showmessage('You Canceled this will Cancel the Print job too.');
    End;
  End;
End;

Procedure TForm9.Button3Click(Sender: TObject);
Var
  z: integer;
  daten: Array[0..3] Of T3field;
  Stop, i: Integer;
  s: TSudoku;
Begin
  // Print 4 new fields
  // Fals der Druckauftrag abgebrochen werden soll
  form10.ScrollBar1.position := Druckbreite;
  Form10.ComboBox1.Items := Printer.Printers;
  Form10.Edit1.text := '1';
  If Form10.ComboBox1.Items.count = 0 Then Begin
    showmessage('No printer found');
    exit;
  End;
  Form10.ComboBox1.Text := Form10.ComboBox1.items[Printer.PrinterIndex];
  // Aufruf des Druckdialoges
  If Form10.showmodal = mrOK Then Begin
    stop := strtointdef(form10.Edit1.text, 1);
    s := TSudoku.Create(fField.Dimension);
    For z := 0 To 3 Do Begin
      // Einstellen der Erstelloptionen
      form1.Puzzle1Click(Nil);
      form7.Init(s, Nil); // Das muss nach form1.Puzzle1Click(Nil); kommen !
      // Erstellen eines neuen Feldes
      form7.Button1Click(Nil);
      // Speichern der Sudoku's
      s.CloneFieldFrom(form7.Sudoku);
      // Falls die Pencil's auch gewünscht sind
       If checkbox1.checked Then Begin
         s.ResetAllNumberPencils;
         s.ClearAllNumberPencils;
       End;
      s.StoreTo(daten[z]);
      // Falls Abgebrochen wird
      If Zwangsabbruch Then break;
    End;
    s.free;
    // Wenn nicht abgebrochen wurde kann gedruckt werden.
    If Not zwangsabbruch Then Begin
      Printer.PrinterIndex := form10.combobox1.ItemIndex;
      // Einstellen Hochformat
      Printer.Orientation := poPortrait;
      // Name des Druckauftrages
      Printer.Title := 'Sudoku ver. : ' + ver + ' by Corpsman | www.Corpsman.de';
      // Start Druckauftrag
      Printer.BeginDoc;
      // Ermöglichen von Mehrfach ausdrucken
      For I := 1 To Stop Do Begin
        // Links oben für 4
        Drucke(rect(0, 0, Printer.Pagewidth Div 2, Printer.Pageheight Div 2), Daten[0]);
        // Rechts oben für 4
        Drucke(rect(Printer.Pagewidth Div 2, 0, Printer.Pagewidth, Printer.Pageheight Div 2), Daten[1]);
        // Links unten für 4
        Drucke(rect(0, Printer.Pageheight Div 2, Printer.Pagewidth Div 2, Printer.Pageheight), Daten[2]);
        // Rechts unten für 4
        Drucke(rect(Printer.Pagewidth Div 2, Printer.Pageheight Div 2, Printer.Pagewidth, Printer.Pageheight), Daten[3]);
        // Ausdrucken der Werbung
        PrintAdvertising();
        // Neue seite bei mehrfach ausdrucken
        If I <> Stop Then
          Printer.NewPage;
      End;
      // Druckauftrag beenden
      Printer.EndDoc;
      // Schliesen des Fenster's
      Close;
    End
    Else Begin
      Showmessage('You Canceled this will Cancel the Print job too.');
    End;
  End;
End;

Procedure TForm9.Button5Click(Sender: TObject);
Var
  daten: Array[0..5] Of T3field;
  z: integer;
  Stop, i: Integer;
  s: TSudoku;
Begin
  // Print 6 new fields
  // Fals der Druckauftrag abgebrochen werden soll
  form10.ScrollBar1.position := Druckbreite;
  Form10.ComboBox1.Items := Printer.Printers;
  Form10.Edit1.text := '1';
  If Form10.ComboBox1.Items.count = 0 Then Begin
    showmessage('No printer found');
    exit;
  End;
  Form10.ComboBox1.Text := Form10.ComboBox1.items[Printer.PrinterIndex];
  // Aufruf des Druckdialoges
  If Form10.showmodal = mrOK Then Begin
    stop := strtointdef(form10.Edit1.text, 1);
    s := TSudoku.Create(fField.Dimension);
    For z := 0 To 5 Do Begin
      // Einstellen der Erstelloptionen
      form1.Puzzle1Click(Nil);
      form7.Init(s, Nil); // Das muss nach form1.Puzzle1Click(Nil); kommen !
      // Erstellen eines neuen Feldes
      form7.Button1Click(Nil);
      // Speichern der Sudoku's
      s.CloneFieldFrom(form7.Sudoku);
      // Falls die Pencil's auch gewünscht sind
      If checkbox1.checked Then Begin
        s.ResetAllNumberPencils;
        s.ClearAllNumberPencils;
      End;
      s.StoreTo(daten[z]);
      // Falls Abgebrochen wird
      If Zwangsabbruch Then break;
    End;
    s.free;
    // Wenn nicht abgebrochen wurde kann gedruckt werden.
    If Not zwangsabbruch Then Begin
      Printer.PrinterIndex := form10.combobox1.ItemIndex;
      // Einstellen Hochformat
      Printer.Orientation := poPortrait;
      // Name des Druckauftrages
      Printer.Title := 'Sudoku ver. : ' + ver + ' by Corpsman | www.Corpsman.de';
      // Start Druckauftrag
      Printer.BeginDoc;
      // Ermöglichen von Mehrfach ausdrucken
      For I := 1 To Stop Do Begin
        // Links oben für 6
        Drucke(rect(0, 0, Printer.Pagewidth Div 2, Printer.Pageheight Div 3), Daten[0]);
        // Rechts oben für 6
        Drucke(rect(Printer.Pagewidth Div 2, 0, Printer.Pagewidth, Printer.Pageheight Div 3), Daten[1]);
        // Mitte Links für 6
        Drucke(rect(0, Printer.pageheight Div 3, Printer.Pagewidth Div 2, (Printer.Pageheight Div 3) * 2), Daten[2]);
        // Mitte Rechts für 6
        Drucke(rect(Printer.pagewidth Div 2, Printer.pageheight Div 3, Printer.Pagewidth, (Printer.Pageheight Div 3) * 2), Daten[3]);
        // Links unten für 6
        Drucke(rect(0, (Printer.Pageheight Div 3) * 2, Printer.Pagewidth Div 2, Printer.Pageheight), Daten[4]);
        // Rechts unten für 6
        Drucke(rect(Printer.Pagewidth Div 2, (Printer.Pageheight Div 3) * 2, Printer.Pagewidth, Printer.Pageheight), Daten[5]);
        // Ausdrucken der Werbung
        PrintAdvertising();
        // Neue seite bei mehrfach ausdrucken
        If I <> Stop Then
          Printer.NewPage;
      End;
      // Druckauftrag beenden
      Printer.EndDoc;
      // Schliesen des Fenster's
      Close;
    End
    Else Begin
      Showmessage('You Canceled this will Cancel the Print job too.');
    End;
  End;
End;

Procedure TForm9.Init(Const aField: TSudoku);
Begin
  fField.Free;
  fField := TSudoku.Create(aField.Dimension);
  fField.CloneFieldFrom(aField);
End;

End.



