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
    fOptions: TSolveOptions;
  public
    { Public-Deklarationen }
    Procedure Init(Const aField: TSudoku; aOptions: TSolveOptions);
  End;

Var
  Form9: TForm9;

Implementation

Uses
  Printers
  , Unit6
  , Unit7
  , Unit10
  ;

{$R *.lfm}

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
//      If checkbox1.checked Then Begin // -- Wenn der User mit Pencils haben will, dann werden die Gedruckt die er selbst eingegeben hat.
//        fField.ResetAllNumberPencils;
//        fField.ClearAllNumberPencils;
//      End;
      fField.PrintToRectangle(rect(0, 0, Printer.Pagewidth, Printer.Pageheight), Form10.ScrollBar1.Position, CheckBox1.Checked);
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
    form7.Init(s, fOptions); // Das muss nach form1.Puzzle1Click(Nil); kommen !
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
        s.PrintToRectangle(rect(0, 0, Printer.Pagewidth, Printer.Pageheight), Form10.ScrollBar1.Position, CheckBox1.Checked);
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
  daten: Array[0..3] Of TSudoku;
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
    For z := 0 To high(daten) Do Begin
      daten[z] := Nil;
    End;
    For z := 0 To high(daten) Do Begin
      s := TSudoku.Create(fField.Dimension);
      // Einstellen der Erstelloptionen
      form7.Init(s, fOptions); // Das muss nach form1.Puzzle1Click(Nil); kommen !
      // Erstellen eines neuen Feldes
      form7.Button1Click(Nil);
      // Speichern der Sudoku's
      s.CloneFieldFrom(form7.Sudoku);
      // Falls die Pencil's auch gewünscht sind
      If checkbox1.checked Then Begin
        s.ResetAllNumberPencils;
        s.ClearAllNumberPencils;
      End;
      daten[z] := s;
      // Falls Abgebrochen wird
      If Zwangsabbruch Then break;
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
        // Links oben für 4
        daten[0].PrintToRectangle(rect(0, 0, Printer.Pagewidth Div 2, Printer.Pageheight Div 2), Form10.ScrollBar1.Position, CheckBox1.Checked);
        // Rechts oben für 4
        daten[1].PrintToRectangle(rect(Printer.Pagewidth Div 2, 0, Printer.Pagewidth, Printer.Pageheight Div 2), Form10.ScrollBar1.Position, CheckBox1.Checked);
        // Links unten für 4
        daten[2].PrintToRectangle(rect(0, Printer.Pageheight Div 2, Printer.Pagewidth Div 2, Printer.Pageheight), Form10.ScrollBar1.Position, CheckBox1.Checked);
        // Rechts unten für 4
        daten[3].PrintToRectangle(rect(Printer.Pagewidth Div 2, Printer.Pageheight Div 2, Printer.Pagewidth, Printer.Pageheight), Form10.ScrollBar1.Position, CheckBox1.Checked);
        // Ausdrucken der Werbung
        PrintAdvertising();
        // Neue seite bei mehrfach ausdrucken
        If I <> Stop Then
          Printer.NewPage;
      End;
      // Druckauftrag beenden
      Printer.EndDoc;
      // Schliesen des Fenster's
      For i := 0 To high(daten) Do Begin
        If assigned(daten[i]) Then daten[i].free;
        daten[i] := Nil;
      End;
      Close;
    End
    Else Begin
      Showmessage('You Canceled this will Cancel the Print job too.');
    End;
    For i := 0 To high(daten) Do Begin
      If assigned(daten[i]) Then daten[i].free;
      daten[i] := Nil;
    End;
  End;
End;

Procedure TForm9.Button5Click(Sender: TObject);
Var
  daten: Array[0..5] Of TSudoku;
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
    For z := 0 To high(daten) Do Begin
      daten[z] := Nil;
    End;
    For z := 0 To high(daten) Do Begin
      s := TSudoku.Create(fField.Dimension);
      // Einstellen der Erstelloptionen
      form7.Init(s, fOptions); // Das muss nach form1.Puzzle1Click(Nil); kommen !
      // Erstellen eines neuen Feldes
      form7.Button1Click(Nil);
      // Speichern der Sudoku's
      s.CloneFieldFrom(form7.Sudoku);
      // Falls die Pencil's auch gewünscht sind
      If checkbox1.checked Then Begin
        s.ResetAllNumberPencils;
        s.ClearAllNumberPencils;
      End;
      daten[z] := s;
      // Falls Abgebrochen wird
      If Zwangsabbruch Then break;
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
        // Links oben für 6
        daten[0].PrintToRectangle(rect(0, 0, Printer.Pagewidth Div 2, Printer.Pageheight Div 3), Form10.ScrollBar1.Position, CheckBox1.Checked);
        // Rechts oben für 6
        daten[1].PrintToRectangle(rect(Printer.Pagewidth Div 2, 0, Printer.Pagewidth, Printer.Pageheight Div 3), Form10.ScrollBar1.Position, CheckBox1.Checked);
        // Mitte Links für 6
        daten[2].PrintToRectangle(rect(0, Printer.pageheight Div 3, Printer.Pagewidth Div 2, (Printer.Pageheight Div 3) * 2), Form10.ScrollBar1.Position, CheckBox1.Checked);
        // Mitte Rechts für 6
        daten[3].PrintToRectangle(rect(Printer.pagewidth Div 2, Printer.pageheight Div 3, Printer.Pagewidth, (Printer.Pageheight Div 3) * 2), Form10.ScrollBar1.Position, CheckBox1.Checked);
        // Links unten für 6
        daten[4].PrintToRectangle(rect(0, (Printer.Pageheight Div 3) * 2, Printer.Pagewidth Div 2, Printer.Pageheight), Form10.ScrollBar1.Position, CheckBox1.Checked);
        // Rechts unten für 6
        daten[5].PrintToRectangle(rect(Printer.Pagewidth Div 2, (Printer.Pageheight Div 3) * 2, Printer.Pagewidth, Printer.Pageheight), Form10.ScrollBar1.Position, CheckBox1.Checked);
        // Ausdrucken der Werbung
        PrintAdvertising();
        // Neue seite bei mehrfach ausdrucken
        If I <> Stop Then
          Printer.NewPage;
      End;
      // Druckauftrag beenden
      Printer.EndDoc;
      // Schliesen des Fenster's
      For i := 0 To high(daten) Do Begin
        If assigned(daten[i]) Then daten[i].free;
        daten[i] := Nil;
      End;
      Close;
    End
    Else Begin
      Showmessage('You Canceled this will Cancel the Print job too.');
    End;
    For i := 0 To high(daten) Do Begin
      If assigned(daten[i]) Then daten[i].free;
      daten[i] := Nil;
    End;
  End;
End;

Procedure TForm9.Init(Const aField: TSudoku; aOptions: TSolveOptions);
Begin
  fField.Free;
  fField := TSudoku.Create(aField.Dimension);
  fField.CloneFieldFrom(aField);
  fOptions := aOptions;
End;

End.

