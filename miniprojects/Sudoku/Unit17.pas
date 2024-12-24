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
Unit Unit17;

{$MODE ObjFPC}{$H+}

Interface

Uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Printers, LResources, Sudoku2x2;

Type
  TForm17 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Procedure Button3Click(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

Var
  Form17: TForm17;
  P: TPrinter;

Implementation

Uses
  math
  , usudoku
  , Unit6
  , Unit10
  , Unit15
  , Unit16;

{$R *.lfm}

Procedure Drucke(Rect: Trect; Value: T2field);
Var
  z, Breite, yo, xo, x, y: integer;
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
  breite := min(abs(rect.Left - rect.Right) Div 6, abs(rect.top - rect.Bottom) Div 6);
  With p.canvas Do Begin
    p.canvas.pen.width := Form10.ScrollBar1.position;
    xo := (abs(rect.left - rect.right) Div 2) - round(2.25 * breite) + rect.left;
    yo := (abs(rect.top - rect.Bottom) Div 2) - round(2.25 * breite) + rect.top;
    For y := 0 To 3 Do
      For x := 0 To 3 Do Begin
        Rectangle(xo + Breite * x + (x Div 2) * Breite Div 2,
          yo + Breite * y + (y Div 2) * Breite Div 2,
          xo + Breite * (x + 1) + (x Div 2) * Breite Div 2,
          yo + Breite * (y + 1) + (y Div 2) * Breite Div 2);
      End;
    Font.size := 1;
    While Textheight('8') < (Breite - Breite Div 4) Do
      Font.size := Font.size + 1;
    For y := 0 To 3 Do
      For x := 0 To 3 Do Begin
        font.color := clblack;
        If Value[x, y].value <> 0 Then
          textout(xo + Breite * x + (x Div 2) * Breite Div 2 + (Breite - Textwidth(inttostr(Value[x, y].value))) Div 2, (Breite - Textheight(inttostr(Value[x, y].value))) Div 2 + yo + Breite * y + (y Div 2) * Breite Div 2, inttostr(Value[x, y].value));
      End;
  End;
End;

Procedure TForm17.Button3Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm17.Button1Click(Sender: TObject);
Var
  Stop, i: Integer;
Begin
  // Fals der Druckauftrag abgebrochen werden soll
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
  If Form10.showmodal = mrOK Then Begin
    stop := strtointdef(form10.Edit1.text, 1);
    p := TPrinter.create;
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
      Drucke(rect(0, 0, p.Pagewidth, p.Pageheight), twofield);
      //      Drucke(rect(0, 0, p.Pagewidth Div 2, p.Pageheight Div 2), twofield);
      //      Drucke(rect(p.Pagewidth Div 2, p.Pageheight Div 2, p.Pagewidth, p.Pageheight), twofield);
            // Ausdrucken der Werbung
      PrintAdvertising();
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

Procedure TForm17.Button2Click(Sender: TObject);
Var
  f: T2field;
  Stop, i: Integer;
Begin
  Form15.showmodal;
  If CreaterOptions <= 0 Then exit;
  // Fals der Druckauftrag abgebrochen werden soll
  p := printer; //TPrinter.create;
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
  If Form10.showmodal = mrOK Then Begin
    stop := strtointdef(form10.Edit1.text, 1);
    New2Field(f, CreaterOptions);
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

Procedure TForm17.Button7Click(Sender: TObject);
Var
  f: Array[0..1] Of T2field;
  Stop, i: Integer;
  //  x, y: integer;
Begin
  Form15.showmodal;
  If CreaterOptions <= 0 Then exit;
  // Fals der Druckauftrag abgebrochen werden soll
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
  If Form10.showmodal = mrOK Then Begin
    stop := strtointdef(form10.Edit1.text, 1);
    New2Field(f[0], CreaterOptions);
    New2Field(f[1], CreaterOptions);
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
        Drucke(rect(0, 0, p.Pagewidth, p.Pageheight Div 2), f[0]);
        Drucke(rect(0, p.Pageheight Div 2, p.Pagewidth, p.Pageheight), f[1]);
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

Procedure TForm17.Button4Click(Sender: TObject);
Var
  f: Array[0..3] Of T2field;
  Stop, i: Integer;
Begin
  Form15.showmodal;
  If CreaterOptions <= 0 Then exit;
  // Fals der Druckauftrag abgebrochen werden soll
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
  If Form10.showmodal = mrOK Then Begin
    stop := strtointdef(form10.Edit1.text, 1);
    New2Field(f[0], CreaterOptions);
    New2Field(f[1], CreaterOptions);
    New2Field(f[2], CreaterOptions);
    New2Field(f[3], CreaterOptions);
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
        Drucke(rect(0, 0, p.Pagewidth Div 2, p.Pageheight Div 2), f[0]);
        Drucke(rect(p.Pagewidth Div 2, 0, p.Pagewidth, p.Pageheight Div 2), f[1]);
        Drucke(rect(0, p.Pageheight Div 2, p.Pagewidth Div 2, p.Pageheight), f[2]);
        Drucke(rect(p.Pagewidth Div 2, p.Pageheight Div 2, p.Pagewidth, p.Pageheight), f[3]);
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

Procedure TForm17.Button5Click(Sender: TObject);
Var
  f: Array[0..5] Of T2field;
  Stop, i: Integer;
Begin
  Form15.showmodal;
  If CreaterOptions <= 0 Then exit;
  // Fals der Druckauftrag abgebrochen werden soll
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
  If Form10.showmodal = mrOK Then Begin
    stop := strtointdef(form10.Edit1.text, 1);
    New2Field(f[0], CreaterOptions);
    New2Field(f[1], CreaterOptions);
    New2Field(f[2], CreaterOptions);
    New2Field(f[3], CreaterOptions);
    New2Field(f[4], CreaterOptions);
    New2Field(f[5], CreaterOptions);
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
        Drucke(rect(0, 0, p.Pagewidth Div 2, p.Pageheight Div 3), f[0]);
        Drucke(rect(p.Pagewidth Div 2, 0, p.Pagewidth, p.Pageheight Div 3), f[1]);
        Drucke(rect(0, p.Pageheight Div 3, p.Pagewidth Div 2, (p.Pageheight Div 3) * 2), f[2]);
        Drucke(rect(p.Pagewidth Div 2, p.Pageheight Div 3, p.Pagewidth, (p.Pageheight Div 3) * 2), f[3]);
        Drucke(rect(0, (p.Pageheight Div 3) * 2, p.Pagewidth Div 2, p.Pageheight), f[4]);
        Drucke(rect(p.Pagewidth Div 2, (p.Pageheight Div 3) * 2, p.Pagewidth, p.Pageheight), f[5]);
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

Procedure TForm17.Button6Click(Sender: TObject);
Var
  f: Array[0..7] Of T2field;
  Stop, i: Integer;
Begin
  Form15.showmodal;
  If CreaterOptions <= 0 Then exit;
  // Fals der Druckauftrag abgebrochen werden soll
  p := Printer;
  p.PrinterIndex := -1;
  TPrinter.create;
  form10.ScrollBar1.position := Druckbreite;
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
    New2Field(f[0], CreaterOptions);
    New2Field(f[1], CreaterOptions);
    New2Field(f[2], CreaterOptions);
    New2Field(f[3], CreaterOptions);
    New2Field(f[4], CreaterOptions);
    New2Field(f[5], CreaterOptions);
    New2Field(f[6], CreaterOptions);
    New2Field(f[7], CreaterOptions);
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
        Drucke(rect(0, 0, p.Pagewidth Div 2, p.Pageheight Div 4), f[0]);
        Drucke(rect(p.Pagewidth Div 2, 0, p.Pagewidth, p.Pageheight Div 4), f[1]);
        Drucke(rect(0, p.Pageheight Div 4, p.Pagewidth Div 2, p.Pageheight Div 2), f[2]);
        Drucke(rect(p.Pagewidth Div 2, p.Pageheight Div 4, p.Pagewidth, p.Pageheight Div 2), f[3]);
        Drucke(rect(0, p.Pageheight Div 2, p.Pagewidth Div 2, (p.Pageheight Div 4) * 3), f[4]);
        Drucke(rect(p.Pagewidth Div 2, p.Pageheight Div 2, p.Pagewidth, (p.Pageheight Div 4) * 3), f[5]);
        Drucke(rect(0, (p.Pageheight Div 4) * 3, p.Pagewidth Div 2, p.Pageheight), f[6]);
        Drucke(rect(p.Pagewidth Div 2, (p.Pageheight Div 4) * 3, p.Pagewidth, p.Pageheight), f[7]);
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

