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
  StdCtrls, Printers, LResources;

Type
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
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

Var
  Form9: TForm9;
  P: TPrinter;

Implementation

Uses
  math
  , usudoku
  , Sudoku3x3
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
  p.canvas.Font.Size := Textsize;
  While p.canvas.TextHeight('8') < Breite - (Breite Div 4) Do Begin
    inc(Textsize);
    p.canvas.Font.Size := Textsize;
  End;
  // malen des Gitters und der ganzen sachen
  p.canvas.pen.color := clblack;
  p.canvas.pen.width := Form10.ScrollBar1.position;
  p.canvas.Brush.Style := bsclear;
  For xx := 0 To 2 Do
    For yy := 0 To 2 Do
      For x := 0 To 2 Do
        For y := 0 To 2 Do Begin
          // Malen des Rahmens für das Feld [xx*3+x,yy*3+y]
          p.canvas.rectangle(xo + breite + x * breite + round(XX * breite * 3.25), yo + y * breite + round(yy * (breite * 3.25)), xo + Breite + (x + 1) * breite + round(XX * breite * 3.25), yo + (y + 1) * breite + round(yy * breite * 3.25));
          // Malen einer Zahl
          If Sudo[xx * 3 + x, yy * 3 + y].Value <> 0 Then Begin
            p.canvas.Font.Size := Textsize;
            p.canvas.textout((breite - P.canvas.TextWidth(inttostr(Sudo[xx * 3 + x, yy * 3 + y].Value))) Div 2 + xo + breite + x * breite + round(XX * breite * 3.25), yo + (breite - P.canvas.Textheight(inttostr(Sudo[xx * 3 + x, yy * 3 + y].Value))) Div 2 + y * breite + round(yy * (breite * 3.25)), substitution[Sudo[xx * 3 + x, yy * 3 + y].value]);
          End
          Else Begin
            // Wenn die Einzelpencil's auch egdruckt werden sollen
            If form9.checkbox1.checked Then Begin
              p.canvas.Font.Size := Textsize Div 3;
              For z := 0 To 8 Do Begin
                If Sudo[xx * 3 + x, yy * 3 + y].Pencil[z] Then
                  p.canvas.textout(-p.canvas.textwidth(substitution[z + 1]) Div 2 + Pencilbreite * ((z Mod 3) + 1) + xo + breite + x * breite + round(XX * breite * 3.25), yo - p.canvas.textheight(substitution[z + 1]) Div 2 + Pencilbreite * ((z Div 3) + 1) + y * breite + round(yy * (breite * 3.25)), substitution[z + 1])
              End;
            End;
          End;
        End;
End;

Procedure TForm9.Button4Click(Sender: TObject);
Begin
  Close;
End;

Procedure TForm9.Button1Click(Sender: TObject);
Var
  Stop, i: Integer;
Begin
  // Fals der Druckauftrag abgebrochen werden soll
  Drucken := false;
  p := Printer;
  form10.ScrollBar1.position := Druckbreite;
  p.PrinterIndex := -1;
  Form10.ComboBox1.Items := p.Printers;
  Form10.Edit1.text := '1';
  If Form10.ComboBox1.Items.count = 0 Then Begin
    showmessage('No printer found');
    exit;
  End;
  Form10.ComboBox1.Text := Form10.ComboBox1.items[p.PrinterIndex];
  // Aufruf des Druckdialoges
  Form10.showmodal;
  stop := strtointdef(form10.Edit1.text, 1);
  // Wenn gedruckt werden darf
  If Drucken Then Begin
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
      If Not Form1.CheckBox4.checked And checkbox1.checked Then
        Form1.Button1.onclick(Nil);
      Drucke(rect(0, 0, p.Pagewidth, p.Pageheight), field);
      // Ausdrucken der Werbung
      DruckeWerbung(p);
      // Neue seite bei mehrfach ausdrucken
      If I <> Stop Then
        p.NewPage;
    End;
    // Druckauftrag beenden
    p.EndDoc;
    // Schliesen des Fenster's
    Close;
  End;
End;

Procedure TForm9.Button2Click(Sender: TObject);
Var
  f: T3field;
  x, y, z: integer;
  Stop, i: Integer;
Begin
  // Fals der Druckauftrag abgebrochen werden soll
  Drucken := false;
  p := Printer; //TPrinter.create;
  p.PrinterIndex := -1;
  form10.ScrollBar1.position := Druckbreite;
  Form10.ComboBox1.Items := p.Printers;
  Form10.Edit1.text := '1';
  If Form10.ComboBox1.Items.count = 0 Then Begin
    showmessage('No printer found');
    exit;
  End;
  Form10.ComboBox1.Text := Form10.ComboBox1.items[p.PrinterIndex];
  // Aufruf des Druckdialoges
  Form10.showmodal;
  stop := strtointdef(form10.Edit1.text, 1);
  // Wenn gedruckt werden darf
  If Drucken Then Begin
    // Wegspeichern des Aktuell angezeigten Feldes
    For x := 0 To 8 Do
      For y := 0 To 8 Do Begin
        f[x, y].Value := field[x, y].Value;
        f[x, y].Marked := field[x, y].marked;
        f[x, y].Maybeed := field[x, y].Maybeed;
        f[x, y].Fixed := field[x, y].Fixed;
        For z := 0 To 8 Do
          f[x, y].pencil[z] := field[x, y].Pencil[z];
      End;
    // Einstellen der Erstelloptionen
    form1.Puzzle1Click(Nil);
    // Erstellen eines neuen Feldes
    form7.Button1Click(Nil);
    // Falls die Pencil's auch gewünscht sind
    If checkbox1.checked Then
      Form1.Button1.onclick(Nil);
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
        Drucke(rect(0, 0, p.Pagewidth, p.Pageheight), field);
        // Ausdrucken der Werbung
        DruckeWerbung(p);
        // Neue seite bei mehrfach ausdrucken
        If I <> Stop Then
          p.NewPage;
      End;
      // Druckauftrag beenden
      p.EndDoc;
      // Freigabe des Feldes
      // Zurückschreiben des Feldes
      For x := 0 To 8 Do
        For y := 0 To 8 Do Begin
          Field[x, y].Value := f[x, y].Value;
          Field[x, y].Marked := f[x, y].marked;
          Field[x, y].Maybeed := f[x, y].Maybeed;
          Field[x, y].Fixed := f[x, y].Fixed;
          For z := 0 To 8 Do
            Field[x, y].pencil[z] := f[x, y].Pencil[z];
        End;
      // Schliesen des Fenster's
      Close;
    End
    Else Begin
      // Zurückschreiben des Feldes
      For x := 0 To 8 Do
        For y := 0 To 8 Do Begin
          Field[x, y].Value := f[x, y].Value;
          Field[x, y].Marked := f[x, y].marked;
          Field[x, y].Maybeed := f[x, y].Maybeed;
          Field[x, y].Fixed := f[x, y].Fixed;
          For z := 0 To 8 Do
            Field[x, y].pencil[z] := f[x, y].Pencil[z];
        End;
      Showmessage('You Canceled this will Cancel the Print job too.');
    End;
  End;
End;

Procedure TForm9.Button3Click(Sender: TObject);
Var
  f: T3field;
  daten: Array[0..3] Of T3field;
  x, y, z: integer;
  Stop, i: Integer;
Begin
  // Fals der Druckauftrag abgebrochen werden soll
  Drucken := false;
  p := Printer; //TPrinter.create;
  p.PrinterIndex := -1;
  form10.ScrollBar1.position := Druckbreite;
  Form10.ComboBox1.Items := p.Printers;
  Form10.Edit1.text := '1';
  If Form10.ComboBox1.Items.count = 0 Then Begin
    showmessage('No printer found');
    exit;
  End;
  Form10.ComboBox1.Text := Form10.ComboBox1.items[p.PrinterIndex];
  // Aufruf des Druckdialoges
  Form10.showmodal;
  stop := strtointdef(form10.Edit1.text, 1);
  // Wenn gedruckt werden darf
  If Drucken Then Begin
    // Wegspeichern des Aktuell angezeigten Feldes
    For x := 0 To 8 Do
      For y := 0 To 8 Do Begin
        f[x, y].Value := field[x, y].Value;
        f[x, y].Marked := field[x, y].marked;
        f[x, y].Maybeed := field[x, y].Maybeed;
        f[x, y].Fixed := field[x, y].Fixed;
        For z := 0 To 8 Do
          f[x, y].pencil[z] := field[x, y].Pencil[z];
      End;
    For z := 0 To 3 Do Begin
      // Einstellen der Erstelloptionen
      form1.Puzzle1Click(Nil);
      // Erstellen eines neuen Feldes
      form7.Button1Click(Nil);
      // Falls die Pencil's auch gewünscht sind
      If checkbox1.checked Then
        Form1.Button1.onclick(Nil);
      // Speichern der Sudoku's
      For x := 0 To 8 Do
        For y := 0 To 8 Do Begin
          daten[z][x, y].Value := field[x, y].Value;
        End;
      // Falls Abgebrochen wird
      If Zwangsabbruch Then break;
    End;
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
        // Links oben für 4
        Drucke(rect(0, 0, p.Pagewidth Div 2, p.Pageheight Div 2), Daten[0]);
        // Rechts oben für 4
        Drucke(rect(p.Pagewidth Div 2, 0, p.Pagewidth, p.Pageheight Div 2), Daten[1]);
        // Links unten für 4
        Drucke(rect(0, p.Pageheight Div 2, p.Pagewidth Div 2, p.Pageheight), Daten[2]);
        // Rechts unten für 4
        Drucke(rect(p.Pagewidth Div 2, p.Pageheight Div 2, p.Pagewidth, p.Pageheight), Daten[3]);
        // Ausdrucken der Werbung
        DruckeWerbung(p);
        // Neue seite bei mehrfach ausdrucken
        If I <> Stop Then
          p.NewPage;
      End;
      // Druckauftrag beenden
      p.EndDoc;
      // Freigabe des Feldes
      // Zurückschreiben des Feldes
      For x := 0 To 8 Do
        For y := 0 To 8 Do Begin
          Field[x, y].Value := f[x, y].Value;
          Field[x, y].Marked := f[x, y].marked;
          Field[x, y].Maybeed := f[x, y].Maybeed;
          Field[x, y].Fixed := f[x, y].Fixed;
          For z := 0 To 8 Do
            Field[x, y].pencil[z] := f[x, y].Pencil[z];
        End;
      // Schliesen des Fenster's
      Close;
    End
    Else Begin
      // Zurückschreiben des Feldes
      For x := 0 To 8 Do
        For y := 0 To 8 Do Begin
          Field[x, y].Value := f[x, y].Value;
          Field[x, y].Marked := f[x, y].marked;
          Field[x, y].Maybeed := f[x, y].Maybeed;
          Field[x, y].Fixed := f[x, y].Fixed;
          For z := 0 To 8 Do
            Field[x, y].pencil[z] := f[x, y].Pencil[z];
        End;
      Showmessage('You Canceled this will Cancel the Print job too.');
    End;
  End;
End;

Procedure TForm9.Button5Click(Sender: TObject);
Var
  f: T3field;
  daten: Array[0..5] Of T3field;
  x, y, z: integer;
  Stop, i: Integer;
Begin
  // Fals der Druckauftrag abgebrochen werden soll
  Drucken := false;
  p := Printer; //TPrinter.create;
  p.PrinterIndex := -1;
  form10.ScrollBar1.position := Druckbreite;
  Form10.ComboBox1.Items := p.Printers;
  Form10.Edit1.text := '1';
  If Form10.ComboBox1.Items.count = 0 Then Begin
    showmessage('No printer found');
    exit;
  End;
  Form10.ComboBox1.Text := Form10.ComboBox1.items[p.PrinterIndex];
  // Aufruf des Druckdialoges
  Form10.showmodal;
  stop := strtointdef(form10.Edit1.text, 1);
  // Wenn gedruckt werden darf
  If Drucken Then Begin
    // Wegspeichern des Aktuell angezeigten Feldes
    For x := 0 To 8 Do
      For y := 0 To 8 Do Begin
        f[x, y].Value := field[x, y].Value;
        f[x, y].Marked := field[x, y].marked;
        f[x, y].Maybeed := field[x, y].Maybeed;
        f[x, y].Fixed := field[x, y].Fixed;
        For z := 0 To 8 Do
          f[x, y].pencil[z] := field[x, y].Pencil[z];
      End;
    For z := 0 To 5 Do Begin
      // Einstellen der Erstelloptionen
      form1.Puzzle1Click(Nil);
      // Erstellen eines neuen Feldes
      form7.Button1Click(Nil);
      // Falls die Pencil's auch gewünscht sind
      If checkbox1.checked Then
        Form1.Button1.onclick(Nil);
      // Speichern der Sudoku's
      For x := 0 To 8 Do
        For y := 0 To 8 Do Begin
          daten[z][x, y].Value := field[x, y].Value;
        End;
      // Falls Abgebrochen wird
      If Zwangsabbruch Then break;
    End;
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
        // Links oben für 6
        Drucke(rect(0, 0, p.Pagewidth Div 2, p.Pageheight Div 3), Daten[0]);
        // Rechts oben für 6
        Drucke(rect(p.Pagewidth Div 2, 0, p.Pagewidth, p.Pageheight Div 3), Daten[1]);
        // Mitte Links für 6
        Drucke(rect(0, p.pageheight Div 3, p.Pagewidth Div 2, (p.Pageheight Div 3) * 2), Daten[2]);
        // Mitte Rechts für 6
        Drucke(rect(p.pagewidth Div 2, p.pageheight Div 3, p.Pagewidth, (p.Pageheight Div 3) * 2), Daten[3]);
        // Links unten für 6
        Drucke(rect(0, (p.Pageheight Div 3) * 2, p.Pagewidth Div 2, p.Pageheight), Daten[4]);
        // Rechts unten für 6
        Drucke(rect(p.Pagewidth Div 2, (p.Pageheight Div 3) * 2, p.Pagewidth, p.Pageheight), Daten[5]);
        // Ausdrucken der Werbung
        DruckeWerbung(P);
        // Neue seite bei mehrfach ausdrucken
        If I <> Stop Then
          p.NewPage;
      End;
      // Druckauftrag beenden
      p.EndDoc;
      // Freigabe des Feldes
      // Zurückschreiben des Feldes
      For x := 0 To 8 Do
        For y := 0 To 8 Do Begin
          Field[x, y].Value := f[x, y].Value;
          Field[x, y].Marked := f[x, y].marked;
          Field[x, y].Maybeed := f[x, y].Maybeed;
          Field[x, y].Fixed := f[x, y].Fixed;
          For z := 0 To 8 Do
            Field[x, y].pencil[z] := f[x, y].Pencil[z];
        End;
      // Schliesen des Fenster's
      Close;
    End
    Else Begin
      // Zurückschreiben des Feldes
      For x := 0 To 8 Do
        For y := 0 To 8 Do Begin
          Field[x, y].Value := f[x, y].Value;
          Field[x, y].Marked := f[x, y].marked;
          Field[x, y].Maybeed := f[x, y].Maybeed;
          Field[x, y].Fixed := f[x, y].Fixed;
          For z := 0 To 8 Do
            Field[x, y].pencil[z] := f[x, y].Pencil[z];
        End;
      Showmessage('You Canceled this will Cancel the Print job too.');
    End;
  End;
End;

End.

