(******************************************************************************)
(* Turing_Maschine                                                ??.??.???? *)
(*                                                                            *)
(* Version     : 0.07                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Simulates a turing machine                                   *)
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
(*
  TODO :

  UTF8 Support !!!
  - Das Problem ist das der UTF8 Support das Stringhändling höllisch verkompliziert, da ja nicht mehr length(string) = anzahl der Zeichen
  das Komplette Programm ist an ettlichen Stellen aber auf diese Eigenart angewiesen ..
  Init Prüft auch entsprechend
  *)
(*                                                                            *)
(* History     : 0.07 - Initial version                                       *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Dialogs, StdCtrls,
  Grids, Spin, Menus, lclintf, LCLType, lclproc;

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
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    OpenDialog1: TOpenDialog;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    StringGrid1: TStringGrid;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure CheckBox2Click(Sender: TObject);
    Procedure Edit1Change(Sender: TObject);
    Procedure Edit2KeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure GroupBox1Resize(Sender: TObject);
    Procedure GroupBox3Resize(Sender: TObject);
    Procedure MenuItem1Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure MenuItem3Click(Sender: TObject);
    Procedure MenuItem4Click(Sender: TObject);
    Procedure MenuItem7Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Procedure ShowTuringState; // zeigt alle Variablen der Maschine in der LCL an
  End;

Const
  Version: Single = 0.07; // Programm Version

Var
  Form1: TForm1;
  TuringState: integer = -1; // Zum Speichern des Aktuellen Zustands der Maschine
  TuringStep: Integer = -1; // Schrittzähler der Maschine
  TuringCommand: Integer = 0; // Zeigt den zuletzt ausgeführten Befeh an
  NTuringCommand: Integer = -1; // Zeigt den nächsten ausgeführten Befehl an
  BandLeft: String = ''; // Das Band Links vom Kopf ( inclusive Kopf )
  BandRight: String = ''; // Das Band Rechts vom Kopf
  EmptySymbol: char = '#'; // Das "Leer" symbol
  CanRun: Boolean = false; // Wenn True dann wurde die Maschine Korreckt initialisiert
  TuringRunnning: Boolean = false; // zeigt die Simulation an
  caption0: String; // Die Caption ohne Dateinamen oder so was
  changed_: Boolean = False; // Wenn True dann wurde die Maschine geändert

Implementation

Uses Unit3, unit2, LazFileUtils, LazUTF8;

{$R *.lfm}

Procedure StringToStream(Const Stream: TStream; Value: String);
Var
  i: Integer;
  b: byte;
Begin
  i := length(value);
  stream.write(i, sizeof(i));
  For i := 1 To length(value) Do Begin
    b := ord(value[i]);
    stream.write(b, sizeof(b));
  End;
End;

Function StringFromStream(Const Stream: TStream): String;
Var
  i: Integer;
  b: Byte;
Begin
  i := 0; // beruhigt den Compiler
  b := 0; // beruhigt den Compiler
  stream.read(i, sizeof(i));
  setlength(result, i);
  For i := 1 To Length(result) Do Begin
    stream.read(b, sizeof(b));
    result[i] := chr(b);
  End;
End;

Function TuringKopfread: char;
Begin
  If length(BandLeft) <> 0 Then
    result := BandLeft[length(BandLeft)]
  Else
    result := EmptySymbol;
End;

Procedure TuringKopfWrite(Value: Char);
Begin
  If length(BandLeft) <> 0 Then Begin
    BandLeft[length(BandLeft)] := value;
  End
  Else
    BandLeft := value;
End;

Procedure TuringkopfMove(Value: String);
Var
  b: Boolean;
Begin
  value := lowercase(value);
  If value = 'left' Then Begin
    If length(BandLeft) <> 0 Then Begin
      BandRight := BandLeft[length(BandLeft)] + BandRight;
      delete(bandleft, length(bandleft), 1);
    End
    Else Begin
      BandRight := EmptySymbol + BandRight;
    End;
  End
  Else If value = 'right' Then Begin
    If length(BandRight) <> 0 Then Begin
      bandleft := bandleft + BandRight[1];
      delete(bandright, 1, 1);
    End
    Else Begin
      bandleft := bandleft + EmptySymbol;
    End;
  End;
  (* Wir Löschen unnötige Bandinhalte *)
  b := True;
  While b Do Begin
    b := false;
    If length(BandLeft) <> 0 Then Begin
      If BandLeft[1] = EmptySymbol Then Begin
        delete(BandLeft, 1, 1);
        b := true;
      End;
    End;
  End;
  b := True;
  While b Do Begin
    b := false;
    If length(BandRight) <> 0 Then Begin
      If BandRight[length(BandRight)] = EmptySymbol Then Begin
        delete(BandRight, length(BandRight), 1);
        b := true;
      End;
    End;
  End;
End;

{ TForm1 }

Procedure TForm1.ShowTuringState;
Begin
  label3.Caption := BandRight + EmptySymbol + EmptySymbol + '..';
  If length(bandLeft) > 0 Then
    label2.caption := bandLeft[length(bandLeft)]
  Else
    label2.caption := EmptySymbol;
  label1.caption := '..' + EmptySymbol + EmptySymbol + copy(bandLeft, 1, length(bandLeft) - 1);
  label1.left := label2.left - label1.width - 10;
  label5.caption := inttostr(TuringState);
  label15.caption := inttostr(TuringStep);
End;

Procedure TForm1.GroupBox1Resize(Sender: TObject);
Begin
  label2.left := (GroupBox1.Width - Label2.width) Div 2;
  Label3.left := Label2.left + label2.width + 10;
  label1.left := label2.left - label1.width - 10;
End;

Procedure TForm1.GroupBox3Resize(Sender: TObject);
Var
  w: Integer;
Begin
  CheckBox1.Checked := true;
  w := (GroupBox3.ClientWidth - 60) Div 5;
  button6.left := 10;
  button6.width := w;
  button1.left := button6.left + button6.Width + 10;
  button1.Width := w;
  CheckBox2.left := button1.left;
  button2.left := button1.left + button1.Width + 10;
  button2.Width := w;
  label16.left := button2.left;
  SpinEdit4.Left := label16.left + label16.width + 5;
  button3.left := button2.left + button2.Width + 10;
  button3.Width := w;
  button4.left := button3.left + button3.Width + 10;
  button4.Width := w;
End;

Procedure TForm1.MenuItem1Click(Sender: TObject);
Var
  i: Integer;
Begin
  If (StringGrid1.RowCount > 1) And (StringGrid1.Selection.Top > 0) Then Begin
    StringGrid1.DeleteColRow(false, StringGrid1.Selection.Top);
    For i := 1 To StringGrid1.RowCount - 1 Do
      StringGrid1.cells[0, i] := inttostr(i);
    CanRun := false;
    changed_ := True;
  End;
End;

Procedure TForm1.MenuItem2Click(Sender: TObject);
Var
  b: Boolean;
  i, j: integer;
  r, s: String;
Begin
  i := StringGrid1.Selection.Top;
  If i > 0 Then Begin
    form2.caption := 'Edit rule';
    form2.Edit1.text := StringGrid1.Cells[1, i];
    form2.Edit2.text := StringGrid1.Cells[2, i];
    form2.Edit3.text := StringGrid1.Cells[3, i];
    If lowercase(StringGrid1.Cells[4, i]) = 'left' Then
      form2.ComboBox1.ItemIndex := 0
    Else If lowercase(StringGrid1.Cells[4, i]) = 'right' Then
      form2.ComboBox1.ItemIndex := 2
    Else
      form2.ComboBox1.ItemIndex := 1;
    form2.Edit5.text := StringGrid1.Cells[5, i];
    form2.Edit4.text := StringGrid1.Cells[6, i];
    Form2OK := false;
    form2.showmodal;
    If form2OK Then Begin
      b := true;
      // Es mus immer ein Symbol eingegeben werden
      Try
        strtoint(form2.edit1.Text);
        strtoint(form2.edit5.Text);
      Except
        b := false;
      End;
      If Length(form2.Edit2.text) <> 1 Then b := false;
      If Length(form2.Edit3.text) <> 1 Then b := false;
      // Keine regel darf doppelt sein
      r := lowercase(form2.Edit1.text);
      s := lowercase(form2.Edit2.text);
      For j := 1 To StringGrid1.RowCount - 1 Do Begin
        If (lowercase(StringGrid1.Cells[1, j]) = r) And
          (lowercase(StringGrid1.Cells[2, j]) = s) And (i <> j) Then Begin
          b := false;
          break;
        End;
      End;
      If b Then Begin
        StringGrid1.Cells[1, i] := form2.Edit1.text;
        StringGrid1.Cells[2, i] := form2.Edit2.text;
        StringGrid1.Cells[3, i] := form2.Edit3.text;
        StringGrid1.Cells[4, i] := form2.ComboBox1.text;
        StringGrid1.Cells[5, i] := form2.Edit5.text;
        StringGrid1.Cells[6, i] := form2.Edit4.text;
        // Nur wenn auch wirklich was geändert wurde CanRun := False
        CanRun := false;
        changed_ := True;
      End
      Else Begin
        Showmessage('Error, the changed rule is not valid, and will not inserted.');
      End;
    End;
  End;
End;

Procedure TForm1.MenuItem3Click(Sender: TObject);
Begin
  StringGrid1.RowCount := 1;
  CanRun := false;
  changed_ := True;
End;

Procedure TForm1.MenuItem4Click(Sender: TObject);
Var
  i, j, k: integer;
  t: Array Of String;
Begin
  // Sortieren nach Zuständen und Read Symbolen
  // zuerst irgendwie nach Symbolen Sortieren
  StringGrid1.SortColRow(true, 2);
  // Dann Ordnungserhaltend nach Zuständen
  t := Nil;
  setlength(t, StringGrid1.ColCount);
  For i := StringGrid1.RowCount - 1 Downto 2 Do Begin
    For j := 2 To i Do Begin
      If strtoint(StringGrid1.Cells[1, j]) < strtoint(StringGrid1.Cells[1, j - 1]) Then Begin
        For k := 0 To StringGrid1.ColCount - 1 Do
          t[k] := StringGrid1.Cells[k, j - 1];
        For k := 0 To StringGrid1.ColCount - 1 Do
          StringGrid1.Cells[k, j - 1] := StringGrid1.Cells[k, j];
        For k := 0 To StringGrid1.ColCount - 1 Do
          StringGrid1.Cells[k, j] := t[k];
      End;
    End;
  End;
  For i := 1 To StringGrid1.RowCount - 1 Do
    StringGrid1.cells[0, i] := inttostr(i);
  setlength(t, 0);
End;

Procedure TForm1.MenuItem7Click(Sender: TObject);
Begin
  form3.UpdateDFA;
  form3.RenderDFA;
  form3.ShowModal;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Var
  i: Integer;
Begin
  caption0 := 'Turing machine ver. ' + floattostrf(version, fffixed, 7, 2) + ' by Corpsman, support : www.Corpsman.de';
  caption := caption0;
  OpenDialog1.InitialDir := ExtractFilePath(paramstr(0));
  saveDialog1.InitialDir := ExtractFilePath(paramstr(0));
  // Init der Felder
  SpinEdit1.value := 1;
  SpinEdit2.value := 0;
  SpinEdit3.value := 1;
  edit1.text := '';
  edit4.text := '#';
  edit2.text := '';
  edit3.text := '#';
  StringGrid1.ColCount := 7;
  StringGrid1.FixedCols := 0;
  StringGrid1.RowCount := 1;
  StringGrid1.ColWidths[0] := 50;
  For i := 1 To 6 Do
    StringGrid1.ColWidths[i] := 100;
  StringGrid1.Cells[0, 0] := 'Nr.';
  StringGrid1.Cells[1, 0] := 'Aktual state';
  StringGrid1.Cells[2, 0] := 'Read symbol';
  StringGrid1.Cells[3, 0] := 'Write symbol';
  StringGrid1.Cells[4, 0] := 'Move dir';
  StringGrid1.Cells[5, 0] := 'New state';
  StringGrid1.Cells[6, 0] := 'Comment';
  // Setzen der Positionen des Bandes
  GroupBox1Resize(Nil);
  GroupBox3Resize(Nil);
  // Pressen Init Button
  Button6Click(Nil);
  Changed_ := false;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  aktsym: Char;
  i: Integer;
  sta: String;
  b: Boolean;
Begin
  // Automatisches Init
  If (Not canrun) And CheckBox1.Checked Then Begin
    // Wenn die Maschine sich im Simulationsmodus abschaltet, darf sie nicht wieder neu gestartet werden.
    If button2.caption <> 'S&top' Then
      Button6.OnClick(Nil);
  End;
  If canrun Then Begin
    b := false;
    aktsym := TuringKopfread;
    sta := inttostr(TuringState);
    TuringCommand := 0;
    For i := 1 To StringGrid1.RowCount - 1 Do
      // Der Aktuelle Zustand
      If (lowercase(aktsym) = lowercase(StringGrid1.Cells[2, i])) And
        (sta = StringGrid1.Cells[1, i]) Then Begin
        TuringKopfWrite(StringGrid1.Cells[3, i][1]);
        TuringkopfMove(StringGrid1.Cells[4, i]);
        TuringState := strtoint(StringGrid1.Cells[5, i]);
        b := true;
        TuringCommand := i;
        // Anzeigen des Gerade angewendeten Zustandes
        If Not checkbox2.checked Then
          StringGrid1.Row := i;
        break;
      End;
    // Ausrechnen des "Nächsten" Turing Schrittes
    NTuringCommand := 0;
    aktsym := TuringKopfread;
    sta := inttostr(TuringState);
    For i := 1 To StringGrid1.RowCount - 1 Do
      // Der Aktuelle Zustand
      If (lowercase(aktsym) = lowercase(StringGrid1.Cells[2, i])) And
        (sta = StringGrid1.Cells[1, i]) Then Begin
        NTuringCommand := i;
        If CheckBox2.checked Then Begin
          StringGrid1.Row := NTuringCommand;
        End;
        break;
      End;
    inc(TuringStep);
    ShowTuringState;
    If Not b Then Begin
      TuringRunnning := false;
      canrun := false;
      showmessage('Error, can not find a rule for "Aktual state = ' + inttostr(TuringState) + ' , read symbol = ' + aktsym + '"');
    End;
    If TuringState = SpinEdit1.value Then Begin
      canrun := false;
      TuringRunnning := false;
      showmessage('Reached finish state.');
    End;
  End
  Else Begin
    TuringRunnning := false;
    showmessage('Error, please Init the maschine first.');
  End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Var
  d: DWord;
Begin
  If button2.caption = '&Run' Then Begin
    // Ausführen des 1. Schrittes
    Button1.OnClick(Nil);
    TuringRunnning := true;
    d := gettickcount;
    // Wenn die Caption erst jetzt umgestellt wird, dann geht auto init noch
    button2.caption := 'S&top';
    While TuringRunnning And canrun Do Begin
      If GetTickCount - d > SpinEdit4.Value Then Begin
        d := gettickcount;
        // Ein Schritt ausführen
        Button1.OnClick(Nil);
      End;
      // 100 % CPU load verhindern
      Application.ProcessMessages;
      sleep(10);
    End;
    // Sollte die Maschine, von allein anhalten, dann muss auch zurückgesetzt werden
    TuringRunnning := false;
    button2.caption := '&Run';
  End
  Else Begin
    TuringRunnning := false;
    button2.caption := '&Run';
  End;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Var
  f: Tfilestream;
  i: Integer;
  s: Single;
Begin
  If OpenDialog1.execute Then Begin
    form3.ClearGraph;
    // Wenn die Maschine gerade simuliert wird, halten wir sie an
    If button2.caption = 'S&top' Then Begin
      button2.OnClick(Nil);
    End;
    caption := caption0 + ', filename = ' + ExtractFileNameOnly(OpenDialog1.FileName);
    SaveDialog1.InitialDir := ExtractFilePath(OpenDialog1.FileName);
    i := -1; // Beruhigt den Compiler
    f := TFileStream.create(OpenDialog1.FileName, fmopenread);
    // Version laden
    s := -1;
    f.read(s, sizeof(s));
    If s > version Then Begin
      showmessage('The file version is newer than the programm version, loading will be canceled.');
      f.free;
      exit;
    End;
    // Speichern ob das Ding laufen kann
    f.read(canrun, sizeof(canrun));
    // der Altuelle Zustand
    f.read(TuringState, sizeof(TuringState));
    // Der Schrittzähler
    f.read(TuringStep, sizeof(TuringStep));
    // Das Linke Band
    BandLeft := StringFromStream(f);
    // Das Rechte Band
    Bandright := StringFromStream(f);
    // Die Transitionstabelle
    f.read(i, sizeof(i));
    StringGrid1.RowCount := i + 1;
    For i := 1 To StringGrid1.RowCount - 1 Do Begin
      StringGrid1.Cells[0, i] := inttostr(i);
      StringGrid1.Cells[1, i] := StringFromStream(f);
      StringGrid1.Cells[2, i] := StringFromStream(f);
      StringGrid1.Cells[3, i] := StringFromStream(f);
      StringGrid1.Cells[4, i] := StringFromStream(f);
      StringGrid1.Cells[5, i] := StringFromStream(f);
      StringGrid1.Cells[6, i] := StringFromStream(f);
    End;
    // Die Initial sachen der Turing Maschine
    // Start Band Inhalt
    Edit1.text := StringFromStream(f);
    // Startzustand
    f.read(i, sizeof(i));
    SpinEdit2.value := i;
    // Endzustand
    f.read(i, sizeof(i));
    SpinEdit1.value := i;
    // Init Position
    f.read(i, sizeof(i));
    SpinEdit3.value := i;
    // Eingabe Alphabet
    Edit2.text := StringFromStream(f);
    // Band Alphabet
    Edit3.text := StringFromStream(f);
    // Das Empty Symbol
    f.read(EmptySymbol, sizeof(EmptySymbol));
    f.free;
    ShowTuringState;
    changed_ := false;
  End;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Var
  f: Tfilestream;
  i: Integer;
Begin
  If SaveDialog1.execute Then Begin
    caption := caption0 + ', filename = ' + ExtractFileNameOnly(SaveDialog1.FileName);
    f := TFileStream.create(SaveDialog1.FileName, fmcreate Or fmopenwrite);
    // Speichern der Programm Version ..
    f.Write(version, sizeof(version));
    // Speichern ob das Ding laufen kann
    f.write(canrun, sizeof(canrun));
    // der Altuelle Zustand
    f.Write(TuringState, sizeof(TuringState));
    // Der Schrittzähler
    f.Write(TuringStep, sizeof(TuringStep));
    // Das Linke Band
    StringToStream(f, BandLeft);
    // Das Rechte Band
    StringToStream(f, Bandright);
    // Die Transitionstabelle
    i := StringGrid1.RowCount - 1;
    f.Write(i, sizeof(i));
    For i := 1 To StringGrid1.RowCount - 1 Do Begin
      StringToStream(f, StringGrid1.Cells[1, i]);
      StringToStream(f, StringGrid1.Cells[2, i]);
      StringToStream(f, StringGrid1.Cells[3, i]);
      StringToStream(f, StringGrid1.Cells[4, i]);
      StringToStream(f, StringGrid1.Cells[5, i]);
      StringToStream(f, StringGrid1.Cells[6, i]);
    End;
    // Die Initial sachen der Turing Maschine
    // Start Band Inhalt
    StringToStream(f, Edit1.text);
    // Startzustand
    i := SpinEdit2.Value;
    f.Write(i, sizeof(i));
    // Endzustand
    i := SpinEdit1.Value;
    f.Write(i, sizeof(i));
    // Init Position
    i := SpinEdit3.Value;
    f.Write(i, sizeof(i));
    // Eingabe Alphabet
    StringToStream(f, Edit2.text);
    // Band Alphabet
    StringToStream(f, Edit3.text);
    // Das Empty Symbol
    f.Write(EmptySymbol, sizeof(EmptySymbol));
    f.free;
    changed_ := false;
  End;
End;

Procedure TForm1.Button6Click(Sender: TObject);
Var
  i: integer;
  a, b: String;
  aktsym: Char;
  sta: String;

Begin
  // Wenn die Maschine gerade simuliert wird, halten wir sie an
  If button2.caption = 'S&top' Then Begin
    button2.OnClick(Nil);
  End;
  CanRun := true;
  TuringState := SpinEdit2.Value;
  BandLeft := copy(edit1.text, 1, SpinEdit3.value);
  BandRight := copy(edit1.text, SpinEdit3.value + 1, length(edit1.text));
  // Check auf utf8 Zeichen
  If length(edit1.text) <> utf8length(edit1.text) Then Begin
    CanRun := false;
    showmessage('Error, there are unallowed symbols (like ä,ö,ü) in the band content.');
  End;
  If length(edit2.text) <> utf8length(edit2.text) Then Begin
    CanRun := false;
    showmessage('Error, there are unallowed symbols (like ä,ö,ü) in the input alphabet.');
  End;
  If length(edit3.text) <> utf8length(edit3.text) Then Begin
    CanRun := false;
    showmessage('Error, there are unallowed symbols (like ä,ö,ü) in the band alphabet.');
  End;
  If length(edit4.text) <> utf8length(edit4.text) Then Begin
    CanRun := false;
    showmessage('Error, there empty symbol is a unallowed symbols, like ä,ö,ü.');
  End;
  // Check ob in den Regeln nur gültige Symbole verwendet werden
  For i := 1 To StringGrid1.RowCount - 1 Do Begin
    If pos(StringGrid1.Cells[2, i], edit3.text) = 0 Then Begin
      CanRun := false;
      Showmessage('Error, rule number ' + inttostr(i) + ' uses a invalid read symbol is not part of the band alphabet.');
    End;
    If pos(StringGrid1.Cells[3, i], edit3.text) = 0 Then Begin
      CanRun := false;
      Showmessage('Error, rule number ' + inttostr(i) + ' uses a invalid write symbol is not part of the band alphabet.');
    End;
  End;
  // Check ob das Eingabealphabet Teilmenge des Bandalphabetes ist
  a := edit2.text;
  b := edit3.text;
  While Length(a) <> 0 Do Begin
    If pos(a[1], b) <> 0 Then
      delete(a, 1, 1)
    Else Begin
      a := '';
      canrun := false;
      Showmessage('Error, input alphabet has to be a part set of band alphabet.');
    End;
  End;
  // Check Eingabe mus aus Eingabealphabet Zeichen bestehen
  a := edit1.text;
  b := edit2.text;
  While Length(a) <> 0 Do Begin
    If pos(a[1], b) <> 0 Then
      delete(a, 1, 1)
    Else Begin
      a := '';
      canrun := false;
      Showmessage('Error, band content involves symbols not containing to the input alphabet.');
    End;
  End;
  // Check Empty Symbol mus teil des Bandalphabetes sein
  If pos(edit4.text, edit3.text) = 0 Then Begin
    canRun := false;
    Showmessage('Error, empty symbol has to be part of the band alphabet.');
  End;
  // Check Empty Symbol darf nicht Teil des Eingabealphabetes sein
  If pos(edit4.text, edit2.text) <> 0 Then Begin
    canRun := false;
    Showmessage('Error, empty symbol is not allowed as part of the input alphabet.');
  End;
  TuringStep := 0;
  ShowTuringState;
  // Ausrechnen des Nächsten Turingschrittes
  TuringCommand := 0;
  NTuringCommand := 0;
  aktsym := TuringKopfread;
  sta := inttostr(TuringState);
  For i := 1 To StringGrid1.RowCount - 1 Do
    // Der Aktuelle Zustand
    If (lowercase(aktsym) = lowercase(StringGrid1.Cells[2, i])) And
      (sta = StringGrid1.Cells[1, i]) Then Begin
      NTuringCommand := i;
      break;
    End;
  If CheckBox2.checked Then Begin
    StringGrid1.Row := NTuringCommand;
  End;

End;

Procedure TForm1.Button7Click(Sender: TObject);
Var
  b: Boolean;
  r, s: String;
  i: Integer;
Begin
  form2.caption := 'New rule';
  Form2OK := false;
  form2.showmodal;
  If form2OK Then Begin
    b := true;
    // Es mus immer ein Symbol eingegeben werden
    Try
      strtoint(form2.edit1.Text);
      strtoint(form2.edit5.Text);
    Except
      b := false;
    End;
    If Length(form2.Edit2.text) <> 1 Then b := false;
    If Length(form2.Edit3.text) <> 1 Then b := false;
    // Keine regel darf doppelt sein
    r := lowercase(form2.Edit1.text);
    s := lowercase(form2.Edit2.text);
    For i := 1 To StringGrid1.RowCount - 1 Do Begin
      If (lowercase(StringGrid1.Cells[1, i]) = r) And
        (lowercase(StringGrid1.Cells[2, i]) = s) Then Begin
        b := false;
        break;
      End;
    End;
    //Check ob eingefügt werden kann
    If b Then Begin
      StringGrid1.RowCount := StringGrid1.RowCount + 1;
      StringGrid1.Cells[0, StringGrid1.RowCount - 1] := inttostr(StringGrid1.RowCount - 1);
      StringGrid1.Cells[1, StringGrid1.RowCount - 1] := form2.Edit1.text;
      StringGrid1.Cells[2, StringGrid1.RowCount - 1] := form2.Edit2.text;
      StringGrid1.Cells[3, StringGrid1.RowCount - 1] := form2.Edit3.text;
      StringGrid1.Cells[4, StringGrid1.RowCount - 1] := form2.ComboBox1.text;
      StringGrid1.Cells[5, StringGrid1.RowCount - 1] := form2.Edit5.text;
      StringGrid1.Cells[6, StringGrid1.RowCount - 1] := form2.Edit4.text;
      // Speichern, dass erst noch initialisiert werden mus
      canRun := false;
      changed_ := True;
    End
    Else Begin
      Showmessage('Error, the new rule is not valid, and will not inserted.');
    End;
  End;
End;

Procedure TForm1.CheckBox2Click(Sender: TObject);
Begin
  If CheckBox2.checked Then
    StringGrid1.Row := NTuringCommand
  Else
    StringGrid1.Row := TuringCommand;
End;

Procedure TForm1.Edit1Change(Sender: TObject);
Begin
  CanRun := false;
  changed_ := True;
End;

Procedure TForm1.Edit2KeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState
  );
Begin
  If pos(lowercase(chr(key)), lowercase(Tedit(sender).text)) <> 0 Then Begin
    key := 0;
    showmessage('Value is already used.');
  End;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  If changed_ Then Begin
    If id_yes = Application.MessageBox('Trying to close without saving the machine.'#13#10 +
      'If you continue then all changes will be lost.'#13#10#13#10 +
      'Do you want to exit without saving ?', 'Warning', MB_YESNO Or MB_ICONWARNING) Then
      canclose := true
    Else
      canclose := false;
  End;
End;

End.

