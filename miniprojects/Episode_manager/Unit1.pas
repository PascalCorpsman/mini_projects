(******************************************************************************)
(* Episode manager                                                 ??.??.???? *)
(*                                                                            *)
(* Version     : 0.03                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Database to store and organize series                        *)
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
(*               0.02 - ??                                                    *)
(*               0.03 - translation into english                              *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE ObjFPC}{$H+}

Interface

Uses
  SysUtils, Classes, Controls, Forms, Dialogs,
  StdCtrls, CheckLst, lcltype;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button11: TButton;
    Label1: TLabel;
    Label2: TLabel;
    GroupBox1: TGroupBox;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    GroupBox2: TGroupBox;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Button5: TButton;
    Button6: TButton;
    GroupBox3: TGroupBox;
    Button7: TButton;
    Label6: TLabel;
    Label7: TLabel;
    Button8: TButton;
    Button9: TButton;
    Button12: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    CheckBox1: TCheckBox;
    CheckListBox1: TCheckListBox;
    CheckListBox2: TCheckListBox;
    Button13: TButton;
    Button14: TButton;
    Procedure Button1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure Button2Click(Sender: TObject);
    Procedure Edit1KeyPress(Sender: TObject; Var Key: Char);
    Procedure Button3Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure Button6Click(Sender: TObject);
    Procedure Button12Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure Button8Click(Sender: TObject);
    Procedure Button9Click(Sender: TObject);
    Procedure Button10Click(Sender: TObject);
    Procedure Button11Click(Sender: TObject);
    Procedure Button13Click(Sender: TObject);
    Procedure Button14Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

  TDatensatz = Record
    Gesehen: Boolean;
    Serie: String;
    Staffel: String;
    Episodenname: String;
  End;

  TDatabase = Class
  private
    Fdata: Array Of TDatensatz;
    Fchanged: Boolean;
    Function Encode(Value: TDatensatz): String;
    Function Decode(Value: String): TDatensatz;
    Function pretty(Value: TDatensatz): String;
    Function PrettyToDatensatz(Value: String): TDatensatz;
  public
    Constructor create;
    Destructor Destroy; override;
    Function SavetoFile(filename: String): Boolean;
    Function LoadFromFile(filename: String): Boolean;
    Function Changed: Boolean;
    Function GebeAlleSerien: Tstringlist;
    Function GebeAlleStaffeln: Tstringlist;
    Function GebeAlleDatensaetzeMit(Serie: String; Staffel: String; IgnoriereGesehen: Boolean): Tstringlist;
    Procedure Sort;
    Procedure Clear;
    Procedure AddItem(Serie: String; Staffel: String; Episodenname: String);
    Procedure MarkWithValue(Const List: TStringlist; Value: Boolean);
  End;

Const
  Version = '0.03';
  // Trenner = chr(195) + chr(166); das "æ" zeichen
  Trenner = '~';

Var
  Form1: TForm1;
  Database: TDatabase;
  LoadedDatabase: String;

Implementation

{$R *.lfm}

Uses Unit2;

Procedure Readini;
Var
  f: Textfile;
  fs, s: String;
Begin
  fs := IncludeTrailingBackslash(ExtractFilePath(paramstr(0))) + 'user.cfg';
  If Fileexists(fs) Then Begin
    assignfile(f, fs);
    reset(f);
    Readln(f, s);
    If Version = s Then Begin
      Readln(f, s);
      LoadedDatabase := s;
    End
    Else Begin
      closefile(f);
      Showmessage('Error, invalid config file version (' + s + ') skip loading.');
      exit;
    End;
    readln(f, s);
    form1.button14.Visible := odd(Strtoint(s));
    closefile(f);
  End;
End;

Procedure WriteIni;
Var
  f: Textfile;
  fs: String;
Begin
  fs := IncludeTrailingBackslash(ExtractFilePath(paramstr(0))) + 'user.cfg';
  assignfile(f, fs);
  rewrite(f);
  writeln(f, version);
  writeln(f, LoadedDatabase);
  writeln(f, inttostr(ord(form1.button14.Visible)));
  closefile(f);
End;

Procedure AktualisiereCheckListboxen;
Var
  l: TStringlist;
  i: Integer;
  s: String;
Begin
  l := Database.GebeAlleSerien;
  form1.CheckListBox1.Clear;
  For i := 0 To l.count - 1 Do Begin
    form1.CheckListBox1.items.add(l[i]);
    form1.CheckListBox1.Checked[i] := true;
  End;
  l.free;
  l := Database.GebeAlleStaffeln;
  form1.CheckListBox2.Clear;
  For i := 0 To l.count - 1 Do Begin
    form1.CheckListBox2.items.add(l[i]);
    form1.CheckListBox2.Checked[i] := true;
  End;
  l.free;
  If Length(LoadedDatabase) <> 0 Then Begin
    s := ExtractFileName(LoadedDatabase);
    s := Copy(s, 1, length(s) - 4);
    form1.Label2.caption := S;
  End
  Else
    form1.Label2.caption := 'None';
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  Close;
End;

Function PointInRect(P: TPoint; R: TRect): boolean;
Var
  t: Integer;
Begin
  If r.left > r.right Then Begin
    t := r.left;
    r.left := r.right;
    r.right := t;
  End;
  If r.top > r.bottom Then Begin
    t := r.Bottom;
    r.bottom := r.top;
    r.top := t;
  End;
  result := (r.left <= p.x) And (r.right >= p.x) And
    (r.top <= p.y) And (r.bottom >= p.y);
End;

Procedure TForm1.FormCreate(Sender: TObject);
Var
  i: Integer;
  r: Trect;
  s: String;
Begin
  (*
  Wenn die Opendialoge abstürzen, dann leigt das an gtk2 und nicht an Lazarus !!

  sudo rm .gtkrc-2.0-kde4

  behebt das Problem.
  *)
  (*
  Bei einem Multimonitorsystem wollen wir die Anwendung immer da starten wo der Mauscursor ist.
  *)
  If screen.MonitorCount <> 1 Then Begin
    For i := 0 To screen.MonitorCount - 1 Do Begin
      r := screen.Monitors[i].BoundsRect;
      If PointInRect(Mouse.CursorPos, r) Then Begin
        left := (screen.Monitors[i].width - form1.width) Div 2 + screen.Monitors[i].BoundsRect.left;
        top := (screen.Monitors[i].height - form1.height) Div 2 + screen.Monitors[i].BoundsRect.top;
        break;
      End;
    End;
  End
  Else Begin
    left := (screen.width - form1.width) Div 2;
    top := (screen.height - form1.height) Div 2;
  End;
  Randomize;
  s := IncludeTrailingBackslash(ExtractFilePath(paramstr(0)));
  Opendialog1.InitialDir := s;
  savedialog1.InitialDir := s;
  Database := TDatabase.create;
  Button2.onclick(Nil);
  Caption := 'Episode Manager ver.: ' + version + ' by Corpsman | support www.Corpsman.de';
  Label2.caption := 'None';
  Readini;
  If FileExists(LoadedDatabase) Then Begin
    Database.LoadFromFile(LoadedDatabase);
    AktualisiereCheckListboxen;
  End
  Else
    LoadedDatabase := '';
End;

Procedure TForm1.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Begin
  writeini;
  Database.free;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  LoadedDatabase := '';
  Database.Clear;
  Edit1.text := '';
  Edit2.text := '';
  Edit3.text := '';
  AktualisiereCheckListboxen;
End;

Procedure TForm1.Edit1KeyPress(Sender: TObject; Var Key: Char);
Begin
  If Key = Trenner Then key := #0;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Var
  s: String;
Begin
  If Database.Changed Then Begin
    If ID_NO = application.Messagebox('Actual database is not stored, load new anyway ?', 'Question', mb_YESNO Or MB_ICONQUESTION) Then
      exit;
  End;
  If Opendialog1.execute Then Begin
    s := IncludeTrailingBackslash(ExtractFilePath(opendialog1.filename));
    Opendialog1.InitialDir := s;
    savedialog1.InitialDir := s;
    Database.LoadFromFile(opendialog1.filename);
    LoadedDatabase := opendialog1.filename;
    AktualisiereCheckListboxen;
  End;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Begin
  Database.AddItem(edit1.text, edit2.text, edit3.text);
  AktualisiereCheckListboxen;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Begin
  If Length(LoadedDatabase) <> 0 Then Begin
    Database.SavetoFile(LoadedDatabase);
  End
  Else
    Button13Click(Nil);
End;

Procedure TForm1.Button13Click(Sender: TObject);
Var
  s: String;
Begin
  If savedialog1.execute Then Begin
    s := IncludeTrailingBackslash(ExtractFilePath(savedialog1.filename));
    Opendialog1.InitialDir := s;
    savedialog1.InitialDir := s;
    Database.SavetoFile(savedialog1.filename);
    LoadedDatabase := savedialog1.filename;
    AktualisiereCheckListboxen;
  End;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  canclose := true;
  If Database.Changed Then Begin
    If ID_NO = application.messagebox('Database not yet stored, close anyway?', 'Question', MB_YESNO Or MB_ICONQUESTION) Then
      canclose := false;
  End;
End;

Procedure TForm1.Button6Click(Sender: TObject);
Begin
  Edit1.text := '';
  Edit2.text := '';
  Edit3.text := '';
End;

Procedure TForm1.Button12Click(Sender: TObject);
Var
  l: Tstringlist;
  g, i, j, k: integer;
  s: String;
Begin
  form2.CheckListBox1.clear;
  g := 0;
  For i := 0 To Checklistbox1.Items.count - 1 Do
    If Checklistbox1.Checked[i] Then
      For j := 0 To Checklistbox2.Items.count - 1 Do
        If Checklistbox2.Checked[j] Then Begin
          l := Database.gebeAlleDatensaetzemit(Checklistbox1.Items[i], Checklistbox2.Items[j], true);
          For k := 0 To l.count - 1 Do Begin
            // Extrahieren des Zählers für Gesehen
            If pos('yes', copy(l[k], pos('|', l[k]) - 6, 6)) <> 0 Then inc(g);
            s := l[k];
            form2.CheckListBox1.items.
              add(
              s
              );
            form2.CheckListBox1.checked[form2.CheckListBox1.items.count - 1] := false;
          End;
          l.free;
        End;
  form2.label1.caption := inttostr(Form2.checklistbox1.items.count) + ' found entries. ' + inttostr(g) + ' already seen.';
  form2.showmodal;
End;

Procedure TForm1.Button7Click(Sender: TObject);
Var
  l2, l: Tstringlist;
  i, j, k: integer;
  s: String;
Begin
  l2 := Tstringlist.create;
  l2.clear;
  // Extrahieren aller Möglichen Werte
  For i := 0 To Checklistbox1.Items.count - 1 Do
    If Checklistbox1.Checked[i] Then
      For j := 0 To Checklistbox2.Items.count - 1 Do
        If Checklistbox2.Checked[j] Then Begin
          l := Database.gebeAlleDatensaetzemit(Checklistbox1.Items[i], Checklistbox2.Items[j], Checkbox1.checked);
          For k := 0 To l.count - 1 Do Begin
            l2.add(l[k]);
          End;
          l.free;
        End;
  If l2.count = 0 Then Begin
    showmessage('Error, there is no dataset found with the actual settings.');
  End
  Else Begin
    // Auswählen des Eintrages
    k := Random(l2.count);
    s := l2[k];
    s := copy(s, pos('|', s) + 2, length(s));
    If ID_YES = application.Messagebox(pchar(
      'The following dataset has been choosen' + LineEnding + LineEnding +
      s + LineEnding + LineEnding +
      'Do you want to mark this dataset as seen?'
      ), 'Info', MB_YESNO Or MB_ICONINFORMATION) Then Begin
      s := l2[k];
      l2.clear;
      l2.add(s);
      Database.MarkWithValue(l2, true);
      If Length(LoadedDatabase) <> 0 Then Begin
        Database.SavetoFile(LoadedDatabase);
      End;
    End;
  End;
  l2.free;
End;

Procedure TForm1.Button8Click(Sender: TObject);
Var
  i: integer;
Begin
  For i := 0 To Checklistbox1.items.count - 1 Do
    Checklistbox1.checked[i] := true;
End;

Procedure TForm1.Button9Click(Sender: TObject);
Var
  i: integer;
Begin
  For i := 0 To Checklistbox1.items.count - 1 Do
    Checklistbox1.checked[i] := false;
End;

Procedure TForm1.Button10Click(Sender: TObject);
Var
  i: integer;
Begin
  For i := 0 To Checklistbox2.items.count - 1 Do
    Checklistbox2.checked[i] := true;
End;

Procedure TForm1.Button11Click(Sender: TObject);
Var
  i: integer;
Begin
  For i := 0 To Checklistbox2.items.count - 1 Do
    Checklistbox2.checked[i] := false;
End;

{ TDatabase }

Procedure TDatabase.AddItem(Serie, Staffel, Episodenname: String);
Begin
  Fchanged := true;
  setlength(Fdata, high(Fdata) + 2);
  Fdata[high(Fdata)].Gesehen := false;
  Fdata[high(Fdata)].Serie := serie;
  Fdata[high(Fdata)].Staffel := Staffel;
  Fdata[high(Fdata)].Episodenname := Episodenname;
  sort;
End;

Constructor TDatabase.create;
Begin
  Inherited;
  Fchanged := false;
  setlength(Fdata, 0);
End;

Destructor TDatabase.Destroy;
Begin
  setlength(Fdata, 0);
End;

Function TDatabase.Changed: Boolean;
Begin
  result := Fchanged;
End;

Procedure TDatabase.Clear;
Begin
  setlength(Fdata, 0);
End;

Function TDatabase.Decode(Value: String): TDatensatz;
Begin
  // Ein Typischer Datensatz
  // Serie Trenner Staffel Trenner Episodenname Trenner 1
  result.Serie := copy(Value, 1, pos(Trenner, value) - 1);
  Delete(Value, 1, pos(Trenner, value) + Length(Trenner) - 1);
  result.Staffel := copy(Value, 1, pos(Trenner, value) - 1);
  Delete(Value, 1, pos(Trenner, value) + Length(Trenner) - 1);
  result.Episodenname := copy(Value, 1, pos(Trenner, value) - 1);
  Delete(Value, 1, pos(Trenner, value) + Length(Trenner) - 1);
  If Length(value) = 1 Then
    result.Gesehen := odd(strtoint(Value))
  Else
    result.Gesehen := false;
End;

Function TDatabase.Encode(Value: TDatensatz): String;
Begin
  result := Value.serie + Trenner + Value.Staffel + Trenner + value.Episodenname + Trenner + inttostr(ord(value.Gesehen));
  //  result := Value.serie + '~' + Value.Staffel + '~' + value.Episodenname + '~' + inttostr(ord(value.Gesehen));
End;

Function TDatabase.LoadFromFile(filename: String): Boolean;
Var
  l: Tstringlist;
  i: Integer;
Begin
  If FileExists(Filename) Then Begin
    Fchanged := false;
    l := TStringlist.create;
    result := True;
    Try
      l.LoadFromFile(Filename);
      setlength(Fdata, l.count);
      For i := 0 To l.count - 1 Do
        If Length(l[i]) <> 0 Then
          Fdata[i] := Decode(l[i]);
    Except
      result := false;
      Fchanged := true;
    End;
    l.free;
  End
  Else
    Result := false;
End;

Function TDatabase.SavetoFile(filename: String): Boolean;
Var
  f: Textfile;
  i: Integer;
Begin
  Fchanged := false;
  Try
    assignfile(f, filename);
    rewrite(f);
    For i := 0 To High(Fdata) Do
      writeln(f, encode(Fdata[i]));
    closefile(f);
    result := true;
  Except
    result := false;
    Fchanged := true;
  End;
End;

Procedure TDatabase.Sort;
  Procedure Quick(li, re: integer);
  Var
    l, r, p: Integer;
    h: TDatensatz;
  Begin
    If Li < Re Then Begin
      p := {Data[} Trunc((li + re) / 2) {]}; // Auslesen des Pivo Elementes
      l := Li;
      r := re;
      While l < r Do Begin
        While CompareStr(encode(Fdata[l]), encode(Fdata[p])) < 0 Do
          //        While Fdata[l] < p Do
          inc(l);
        While CompareStr(encode(Fdata[r]), encode(Fdata[p])) > 0 Do
          //        While Fdata[r] > p Do
          dec(r);
        If L <= R Then Begin
          h := Fdata[l];
          fdata[l] := FData[r];
          fdata[r] := h;
          inc(l);
          dec(r);
        End;
      End;
      quick(li, r);
      quick(l, re);
    End;
  End;
Begin
  Quick(0, high(Fdata));
End;

Function TDatabase.GebeAlleSerien: Tstringlist;
Var
  i: Integer;
Begin
  result := TStringList.create;
  result.Duplicates := dupIgnore;
  result.Sorted := true;
  For i := 0 To High(fdata) Do
    result.add(fdata[i].Serie);
End;

Function TDatabase.GebeAlleStaffeln: Tstringlist;
Var
  i: Integer;
Begin
  result := TStringList.create;
  result.Duplicates := dupIgnore;
  result.Sorted := true;
  For i := 0 To High(fdata) Do
    result.add(fdata[i].Staffel);
End;

Function TDatabase.GebeAlleDatensaetzeMit(Serie,
  Staffel: String; IgnoriereGesehen: Boolean): Tstringlist;
Var
  i: Integer;
Begin
  result := TStringList.create;
  For i := 0 To High(Fdata) Do
    If (Fdata[i].Serie = Serie) And (Fdata[i].Staffel = Staffel) Then Begin
      If IgnoriereGesehen Then
        result.add(pretty(Fdata[i]))
      Else If Not Fdata[i].Gesehen Then
        result.add(pretty(Fdata[i]));
    End;
End;

Function TDatabase.pretty(Value: TDatensatz): String;
Begin
  If Value.Gesehen Then
    result := 'Seen = yes | '
  Else
    result := 'Seen =  no | ';
  result := result + 'Series : ' + Value.Serie +
    ' | Season : ' + Value.Staffel +
    ' | Episode : ' + Value.Episodenname;
End;

Function TDatabase.PrettyToDatensatz(Value: String): TDatensatz;
Var
  s: String;
Begin
  s := Copy(value, 1, pos('|', value));
  delete(value, 1, pos('|', value) + 1);
  result.Gesehen := pos('yes', s) <> 0;
  s := Copy(value, 1, pos('|', value) - 2);
  delete(value, 1, pos('|', value) + 1);
  result.Serie := copy(s, pos(':', s) + 2, length(s));
  s := Copy(value, 1, pos('|', value) - 2);
  delete(value, 1, pos('|', value) + 1);
  result.Staffel := copy(s, pos(':', s) + 2, length(s));
  s := value;
  result.Episodenname := copy(s, pos(':', s) + 2, length(s));
End;

Procedure TDatabase.MarkWithValue(Const List: TStringlist; Value: Boolean);
Var
  a, i: Integer;
  dt: TDatensatz;
Begin
  If list.count <> 0 Then Begin
    a := 0;
    dt := PrettyToDatensatz(list[a]);
    For i := 0 To High(fdata) Do
      If (Fdata[i].Serie = dt.Serie) And
        (Fdata[i].Staffel = dt.Staffel) And
        (Fdata[i].Episodenname = dt.Episodenname) Then Begin
        Fchanged := True;
        Fdata[i].Gesehen := Value;
        inc(a);
        If a = list.count Then
          exit
        Else Begin
          dt := PrettyToDatensatz(list[a]);
        End;
      End;
    If length(LoadedDatabase) <> 0 Then Begin
      Database.SavetoFile(LoadedDatabase);
    End;
  End;
End;

Procedure TForm1.Button14Click(Sender: TObject);
Begin
  Database.Sort;
End;

End.

