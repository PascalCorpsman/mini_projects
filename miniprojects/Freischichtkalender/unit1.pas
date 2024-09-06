(******************************************************************************)
(* Freischichtkalender                                             ??.??.???? *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Prints a year calender on a DinA5 page                       *)
(*                                                                            *)
(* License     : See the file license.md, located under:                      *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(* Warranty    : There is no warranty, neither in correctness of the          *)
(*               implementation, nor anything other that could happen         *)
(*               or go wrong, use at your own risk.                           *)
(*                                                                            *)
(* Known Issues: none                                                         *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, printers, StdCtrls,
  ExtCtrls, PrintersDlgs, ufcal;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    ListBox1: TListBox;
    OpenDialog1: TOpenDialog;
    PaintBox1: TPaintBox;
    PrintDialog1: TPrintDialog;
    SaveDialog1: TSaveDialog;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure Button8Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
  private
    FCal: TFCal;
    fchanged: Boolean;
    Procedure LCLToCal();
    Procedure DaysToLCL();
  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses unit2, LCLType;

{ TForm1 }

Procedure TForm1.Button2Click(Sender: TObject);
Var
  p: TPrinter;
Begin
  Printer.Orientation := poPortrait;
  If PrintDialog1.Execute Then Begin
    LCLToCal();
    p := Printer;
    p.Title := fcal.CalenderTitle;
    p.BeginDoc;
    FCal.RenderToCanvas(p.Canvas, rect(0, 0, p.PageWidth, p.PageHeight), crJanToJun);
    p.NewPage;
    FCal.RenderToCanvas(p.Canvas, rect(0, 0, p.PageWidth, p.PageHeight), crJulToDec);
    p.EndDoc;
  End;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Var
  bm: TBitmap;
Begin
  bm := tbitmap.create;
  bm.Width := 2479;
  bm.Height := 3508;
  bm.canvas.brush.Color := clWhite;
  bm.Canvas.Rectangle(-1, -1, bm.Width + 1, bm.Height + 1);
  LCLToCal();
  FCal.RenderToCanvas(bm.Canvas, rect(0, 0, bm.Width, bm.Height), crJanToJun);
  FCal.RenderToCanvas(bm.Canvas, rect(0, bm.Height Div 2, bm.Width, bm.Height + bm.Height Div 2), crJulToDec);
  //  bm.SaveToFile('/home/corpsman/Desktop/out.bmp');
  bm.SaveToFile('C:\Data\MEA.rep\Temp\out.bmp');
  bm.free;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Var
  sl: TStringList;
  i: Integer;
Begin
  If SaveDialog1.Execute Then Begin
    sl := TStringList.Create;
    sl.Add('1'); // File Version
    sl.Add(edit1.text); // Year
    sl.Add(edit2.text); // Title
    sl.add(edit3.text); // Footnote
    sl.add('Holidays');
    For i := 0 To high(FCal.Holidays) Do Begin
      sl.add(format('%d %d %s', [FCal.Holidays[i].Day, FCal.Holidays[i].Month, FCal.Holidays[i].Title]));
    End;
    sl.add('Vacationdays');
    For i := 0 To high(FCal.VacationDays) Do Begin
      sl.add(format('%d %d %s', [FCal.VacationDays[i].Day, FCal.VacationDays[i].Month, FCal.VacationDays[i].Title]));
    End;
    sl.add('Labeleddays');
    For i := 0 To high(FCal.LabeledDays) Do Begin
      sl.add(format('%d %d %s', [FCal.LabeledDays[i].Day, FCal.LabeledDays[i].Month, FCal.LabeledDays[i].Title]));
    End;
    sl.SaveToFile(SaveDialog1.FileName);
    sl.free;
    fchanged := false;
  End;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Var
  sl: TStringList;
  d, m, t, s, fw: String;
  i: integer;
Begin
  If OpenDialog1.Execute Then Begin
    fchanged := false;
    sl := TStringList.Create;
    sl.LoadFromFile(OpenDialog1.FileName);
    setlength(FCal.Holidays, 0);
    setlength(FCal.VacationDays, 0);
    setlength(FCal.LabeledDays, 0);
    fw := sl[0]; // Die Dateiversion -- Für Später
    edit1.text := sl[1];
    edit2.text := sl[2];
    edit3.text := sl[3];
    // Auslesen der Holidays
    i := 5;
    While (sl[i] <> 'Vacationdays') Do Begin
      s := sl[i];
      If trim(s) <> '' Then Begin
        d := copy(s, 1, pos(' ', s) - 1);
        delete(s, 1, pos(' ', s));
        m := copy(s, 1, pos(' ', s) - 1);
        delete(s, 1, pos(' ', s));
        t := trim(s);
        setlength(FCal.Holidays, length(FCal.Holidays) + 1);
        FCal.Holidays[high(FCal.Holidays)].Day := strtoint(d);
        FCal.Holidays[high(FCal.Holidays)].Month := strtoint(m);
        FCal.Holidays[high(FCal.Holidays)].Title := t;
      End;
      inc(i);
    End;
    inc(i); // Das Vacationdays überspringen
    // Auslesen Vacationdays
    While (sl[i] <> 'Labeleddays') Do Begin
      s := sl[i];
      If trim(s) <> '' Then Begin
        d := copy(s, 1, pos(' ', s) - 1);
        delete(s, 1, pos(' ', s));
        m := copy(s, 1, pos(' ', s) - 1);
        delete(s, 1, pos(' ', s));
        t := trim(s);
        setlength(FCal.VacationDays, length(FCal.VacationDays) + 1);
        FCal.VacationDays[high(FCal.VacationDays)].Day := strtoint(d);
        FCal.VacationDays[high(FCal.VacationDays)].Month := strtoint(m);
        FCal.VacationDays[high(FCal.VacationDays)].Title := t;
      End;
      inc(i);
    End;
    inc(i); // das "Labeldays" überspringen
    // Auslesen Labeleddays
    While (i < sl.count) Do Begin
      s := sl[i];
      If trim(s) <> '' Then Begin
        d := copy(s, 1, pos(' ', s) - 1);
        delete(s, 1, pos(' ', s));
        m := copy(s, 1, pos(' ', s) - 1);
        delete(s, 1, pos(' ', s));
        t := trim(s);
        setlength(FCal.LabeledDays, length(FCal.LabeledDays) + 1);
        FCal.LabeledDays[high(FCal.LabeledDays)].Day := strtoint(d);
        FCal.LabeledDays[high(FCal.LabeledDays)].Month := strtoint(m);
        FCal.LabeledDays[high(FCal.LabeledDays)].Title := t;
      End;
      inc(i);
    End;
    sl.free;
    DaysToLCL();
  End;
End;

Procedure TForm1.Button6Click(Sender: TObject);
Begin
  Close;
End;

Procedure TForm1.Button7Click(Sender: TObject);
Begin
  // Add
  form2.cal := FCal;
  If form2.Showmodal = mrOK Then Begin
    DaysToLCL();
  End;
End;

Procedure TForm1.Button8Click(Sender: TObject);
Var
  d, m, s: String;
  di, mi, i, j: integer;
  c: Char;
Begin
  // Del
  If ListBox1.ItemIndex <> -1 Then Begin
    s := ListBox1.Items[ListBox1.ItemIndex];
    c := s[1];
    delete(s, 1, 2);
    d := copy(s, 1, pos(' ', s) - 1);
    delete(s, 1, pos(' ', s));
    m := copy(s, 1, pos(' ', s) - 1);
    di := strtoint(d);
    mi := strtoint(m);
    Case c Of
      'H': Begin
          For i := 0 To high(FCal.Holidays) Do Begin
            If (FCal.Holidays[i].Day = di) And (FCal.Holidays[i].Month = mi) Then Begin
              For j := i To high(FCal.Holidays) - 1 Do Begin
                FCal.Holidays[j] := FCal.Holidays[j + 1];
              End;
              SetLength(FCal.Holidays, high(FCal.Holidays));
              fchanged := true;
              DaysToLCL();
              break;
            End;
          End;
        End;
      'V': Begin
          For i := 0 To high(FCal.VacationDays) Do Begin
            If (FCal.VacationDays[i].Day = di) And (FCal.VacationDays[i].Month = mi) Then Begin
              For j := i To high(FCal.VacationDays) - 1 Do Begin
                FCal.VacationDays[j] := FCal.VacationDays[j + 1];
              End;
              SetLength(FCal.VacationDays, high(FCal.VacationDays));
              fchanged := true;
              DaysToLCL();
              break;
            End;
          End;
        End;
      'L': Begin
          For i := 0 To high(FCal.LabeledDays) Do Begin
            If (FCal.LabeledDays[i].Day = di) And (FCal.LabeledDays[i].Month = mi) Then Begin
              For j := i To high(FCal.LabeledDays) - 1 Do Begin
                FCal.LabeledDays[j] := FCal.LabeledDays[j + 1];
              End;
              SetLength(FCal.LabeledDays, high(FCal.LabeledDays));
              fchanged := true;
              DaysToLCL();
              break;
            End;
          End;
        End;
    End;
  End;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  If fchanged Then Begin
    If id_no = application.MessageBox('Not saved yet, are you shure you want to close without saving ?', 'Warning', MB_YESNO Or MB_ICONWARNING) Then Begin
      CanClose := false;
    End;
  End;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  LCLToCal();
  PaintBox1.Canvas.Pen.Color := clblack;
  PaintBox1.Canvas.Brush.Color := clWhite;
  PaintBox1.Canvas.Rectangle(-1, -1, PaintBox1.Width + 1, PaintBox1.Height + 1);
  If CheckBox1.Checked Then Begin
    FCal.RenderToCanvas(PaintBox1.Canvas, rect(0, 0, PaintBox1.Width, PaintBox1.Height * 2), crJulToDec);
  End
  Else Begin
    FCal.RenderToCanvas(PaintBox1.Canvas, rect(0, 0, PaintBox1.Width, PaintBox1.Height * 2), crJanToJun);
  End;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  (*
   * Historie: 0.01 = Initialversion
   *           0.02 = Spalten gedreht, Farbe angepasst, Höhe neu berechnet, Ferien haben eigene Farbe ...
   *           0.03 = Robusteres laden
   *           0.04 = Add Preview für zweite Jahreshälfte
   *)
  caption := 'Calender creater ver.: 0.04 by www.Corpsman.de';
  FCal := TFCal.create;
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
  edit1.text := FormatDateTime('YYYY', now);
  edit1.text := inttostr(strtoint(edit1.text) + 1);
  edit2.text := 'STIHL Betriebskalender ' + edit1.text;
  edit3.text := 'Ferien Baden Württemberg ' + edit1.text;
  DaysToLCL();
  fchanged := false;
End;

Procedure TForm1.FormDestroy(Sender: TObject);
Begin
  FCal.Free;
End;

Procedure TForm1.LCLToCal();
Begin
  FCal.Year := strtoint(edit1.text);
  FCal.CalenderTitle := Edit2.Text;
  FCal.FootNote := Edit3.Text;
End;

Procedure TForm1.DaysToLCL();

  Function GetIndex(item: integer): integer;
  Var
    d, m, s: String;
  Begin
    s := ListBox1.Items[item];
    delete(s, 1, 2);
    d := copy(s, 1, pos(' ', s) - 1);
    delete(s, 1, pos(' ', s));
    m := copy(s, 1, pos(' ', s) - 1);
    result := strtoint(m) * 100 + strtoint(d);
  End;

  Procedure Quick(li, re: integer);
  Var
    l, r, p: Integer;
    h: String;
  Begin
    If Li < Re Then Begin
      p := GetIndex(Trunc((li + re) / 2)); // Auslesen des Pivo Elementes
      l := Li;
      r := re;
      While l < r Do Begin
        While GetIndex(l) < p Do
          inc(l);
        While GetIndex(r) > p Do
          dec(r);
        If L <= R Then Begin
          If l <> r Then Begin
            h := listbox1.items[l];
            listbox1.items[l] := listbox1.items[r];
            listbox1.items[r] := h;
          End;
          inc(l);
          dec(r);
        End;
      End;
      quick(li, r);
      quick(l, re);
    End;

  End;

Var
  i: Integer;
Begin
  ListBox1.Clear;
  For i := 0 To high(FCal.Holidays) Do Begin
    ListBox1.items.add(format('H %d %d %s', [FCal.Holidays[i].Day, FCal.Holidays[i].Month, FCal.Holidays[i].Title]));
  End;
  For i := 0 To high(FCal.VacationDays) Do Begin
    ListBox1.items.add(format('V %d %d %s', [FCal.VacationDays[i].Day, FCal.VacationDays[i].Month, FCal.VacationDays[i].Title]));
  End;
  For i := 0 To high(FCal.LabeledDays) Do Begin
    ListBox1.items.add(format('L %d %d %s', [FCal.LabeledDays[i].Day, FCal.LabeledDays[i].Month, FCal.LabeledDays[i].Title]));
  End;
  Quick(0, ListBox1.Items.Count - 1);
End;

End.

