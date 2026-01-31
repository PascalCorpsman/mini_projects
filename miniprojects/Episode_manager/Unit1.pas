(******************************************************************************)
(* Episode manager                                                 ??.??.???? *)
(*                                                                            *)
(* Version     : 0.04                                                         *)
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
(*               0.04 - connect to fileserver server API                      *)
(*                      switch to real .ini fileformat                        *)
(*                      add ability to edit datasets                          *)
(*                      add ability to delete a dataset                       *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE ObjFPC}{$H+}

Interface

Uses
  SysUtils, Classes, Controls, Forms, Dialogs,
  StdCtrls, CheckLst, lcltype, Buttons, IniFiles, uepisodenmanager;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button11: TButton;
    ImageList1: TImageList;
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
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
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
    Procedure SpeedButton1Click(Sender: TObject);
    Procedure SpeedButton2Click(Sender: TObject);
    Procedure SpeedButton3Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;


Const
  Version = '0.04';

Var
  Form1: TForm1;
  Database: TDatabase;
  LoadedDatabase: String;
  Ini: TIniFile = Nil;

Implementation

{$R *.lfm}

Uses
  Unit2 // Show selected
  , unit3 // Settings dialog
  //, unit4 -- Diff view
  //, unit5 --TDatensatz Detailed view
  , usslconnector
  ;

Procedure Readini;
Var
  fs: String;
Begin
  fs := IncludeTrailingBackslash(ExtractFilePath(paramstr(0))) + 'user.ini';
  ini := TIniFile.Create(fs);
  LoadedDatabase := ini.ReadString('General', 'LastLoadedDB', '');
End;

Procedure WriteIni;
Begin
  If Not assigned(ini) Then exit;
  ini.WriteString('General', 'LastLoadedDB', LoadedDatabase);
  ini.free;
  ini := Nil;
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
  If Key = PrettyTrenner Then key := #0;
  If key = #13 Then Button5.Click;
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
Begin
  Form2.DBToLCL;
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


Procedure TForm1.Button14Click(Sender: TObject);
Begin
  Database.Sort;
End;

Procedure TForm1.SpeedButton1Click(Sender: TObject);
Var
  m: TMemoryStream;
Begin
  If Database.Changed Then Begin
    showmessage('Error, save Database first.');
    exit;
  End;
  // Upload Database
  If Not Login(
    Ini.ReadString('Filechecker', 'URL', 'https://127.0.0.1')
    , Ini.ReadString('Filechecker', 'Port', '8443')
    , ClientID
    , Ini.ReadString('Filechecker', 'User', 'Username')
    , Ini.ReadString('Filechecker', 'Password', '')) Then Begin
    showmessage('Failed to log in.');
    exit;
  End;
  m := TMemoryStream.Create;
  m.LoadFromFile(LoadedDatabase);
  If Not SendDB(m) Then Begin
    showmessage('Error, unable to send database.');
    m.free;
    logout;
    exit;
  End;
  m.free;
  logout;
  showmessage('Done.');
End;

Procedure TForm1.SpeedButton2Click(Sender: TObject);
Begin
  // Settings
  form3.init(ini);
  If form3.ShowModal = mrOK Then Begin
    form3.StoreSettings(Ini);
  End;
End;

Procedure TForm1.SpeedButton3Click(Sender: TObject);
Var
  m: TMemoryStream;
  other: TDatabase;
  dummy: String;
Begin
  // Upload Database
  If Not Login(
    Ini.ReadString('Filechecker', 'URL', 'https://127.0.0.1')
    , Ini.ReadString('Filechecker', 'Port', '8443')
    , ClientID
    , Ini.ReadString('Filechecker', 'User', 'Username')
    , Ini.ReadString('Filechecker', 'Password', '')) Then Begin
    showmessage('Failed to log in.');
    exit;
  End;
  m := RequestaDBAndDownloadIt(dummy);
  Logout;
  If Not assigned(m) Then Begin
    showmessage('Error, failed to download.');
    exit;
  End;
  // Als nächsted DB-Mergen ;)
  other := TDatabase.create;
  other.LoadFromStream(m);
  m.free;
  Database.MergeOtherIn(other);
  other.free;
  AktualisiereCheckListboxen;
End;

End.

