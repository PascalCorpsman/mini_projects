(******************************************************************************)
(* Simple Search                                                   ??.??.???? *)
(*                                                                            *)
(* Version     : 0.13                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : A Windows 98 Style search dialog                             *)
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
(* Known Issues: Das Drag & Drop nach draußen ist noch nicht optimal, aber    *)
(*                  geht                                                      *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*                                                                            *)
(*               0.02 - Versuch Regex zu integrieren, die regex Klasse scheint*)
(*                        aber nicht korrekt zu arbeiten, deswegen inaktiv ...*)
(*                      Umstellen InFile Search auf TMemorystream             *)
(*                      Einbaun Zeitmessung der Suche                         *)
(*               0.03 - StayOnTop Option, OnKeypress auch im Scan in          *)
(*               0.04 - Drag Drop von Dateinamen aus der Anwendung in andere  *)
(*                        Rein.                                               *)
(*               0.05 - UTF8 Zeichen in Dateinamen wurden falsch behandelt.   *)
(*               0.06 - Suche nach xy.exe hatte die Datei nicht gefunden      *)
(*                        obwohl es sie gab.                                  *)
(*                      Bei jedem neuen Unterverzeichnisscan ein              *)
(*                        Application.processmessages (dann friert die        *)
(*                        Anwendung nicht ein, wenn lange nichts gefunden     *)
(*                        wird)                                               *)
(*                      Bessere Ausgabe in der Statusbar.                     *)
(*                      Exportiere Dateiliste                                 *)
(*               0.07 - Anpassung Auswertung Parameter, damit das ganze mit   *)
(*                        Caja geht                                           *)
(*               0.08 - Freischalten multiselect, delete                      *)
(*               0.09 - Suchen Ersetzen auf Dateinamen Ebene                  *)
(*               0.10 - Copy Filefolder                                       *)
(*               0.11 - Anzeigen Löschen                                      *)
(*               0.12 - Copy selected to                                      *)
(*               0.13 - do not follow symlinks                                *)
(*                                                                            *)
(* Missing Features: Exclude beim Suchen                                      *)
(*                                                                            *)
(******************************************************************************)

Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, lazFileUtils, lazutf8, Forms, Controls, Graphics, Dialogs, StdCtrls,
  LCLType, Menus, ComCtrls, Regex, NativeDnD;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ListBox1: TListBox;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    SaveDialog1: TSaveDialog;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    StatusBar1: TStatusBar;
    Procedure Button1Click(Sender: TObject);
    Procedure Edit2KeyPress(Sender: TObject; Var Key: char);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDropFiles(Sender: TObject; Const FileNames: Array Of String);
    Procedure FormShow(Sender: TObject);
    Procedure MenuItem10Click(Sender: TObject);
    Procedure MenuItem11Click(Sender: TObject);
    Procedure MenuItem12Click(Sender: TObject);
    Procedure MenuItem1Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure MenuItem3Click(Sender: TObject);
    Procedure MenuItem4Click(Sender: TObject);
    Procedure MenuItem5Click(Sender: TObject);
    Procedure MenuItem6Click(Sender: TObject);
    Procedure MenuItem8Click(Sender: TObject);
    Procedure MenuItem9Click(Sender: TObject);
  private
    { private declarations }
    Procedure DragFileListEvent(Sender: TObject; FileList: TStringList);
  public
    { public declarations }
    Procedure AddFile(Filename: String);
    Procedure ProceedFind(Filename: String);
    Procedure GetFilesInDirectory(ADirectory: String; Const AMask: Tstringlist; ARekursiv: Boolean);
  End;

Var
  Form1: TForm1;
  panik: Boolean;
  BasePath: String;
  Rex: TRegexEngine;
  fDragDrop: TNativeDragSource;

Implementation

{$R *.lfm}

Uses Clipbrd, LCLIntf, unit2, FileUtil;

{ TForm1 }

Procedure TForm1.ProceedFind(Filename: String);
  Function lower(b: Byte): Byte;
  Begin
    result := b;
    If (result >= 65) And (result <= 90) Then Begin
      result := result + 32;
    End;
  End;

Var
  f: TFilestream;
  m: TMemoryStream;
  mc, ms: int64;
  s: String;
  sb: Array Of byte;
  b: Byte;
  CaseSensitive: Boolean;
  j, i: Integer;
  searchptrs: Array Of Integer;
Begin
  If edit3.text = '' Then Begin
    AddFile(Filename);
  End
  Else Begin
    If Not (FileExistsUTF8(Filename)) Then exit; // Wenn Filename ein Verzeichnis ist.
    s := Edit3.text;
    CaseSensitive := CheckBox2.Checked;
    If Not CaseSensitive Then Begin
      s := LowerCase(s);
    End;
    sb := Nil;
    setlength(sb, Length(s));
    searchptrs := Nil;
    For i := 0 To high(sb) Do Begin
      sb[i] := ord(s[i + 1]);
    End;
    Try
      f := TFileStream.Create(UTF8ToSys(Filename), fmOpenRead);
    Except
      showmessage('Error opening file : ' + Filename);
      // f.free; -- Darf nicht freigegeben werden, weil es anscheinend auch nicht richtig erzeugt wurde.
      exit;
    End;
    m := TMemoryStream.Create;
    m.CopyFrom(f, f.Size);
    m.Position := 0;
    f.free;
    ms := m.Size;
    mc := 0;
    b := 0;
    While (mc <= ms) And (Not panik) Do Begin
      m.read(b, sizeof(b));
      inc(mc);
      If Not CaseSensitive Then Begin
        b := lower(b);
      End;
      // Das Suchwort wird evtl neu gefunden, jede ÜBereinstimmung mit dem ersten zeichen des Suchworts startet eine neue "Suche"
      If sb[0] = b Then Begin
        setlength(searchptrs, high(searchptrs) + 2);
        searchptrs[high(searchptrs)] := 0;
      End;
      For i := high(searchptrs) Downto 0 Do Begin
        If sb[searchptrs[i]] = b Then Begin
          inc(searchptrs[i]);
          If searchptrs[i] > high(sb) Then Begin
            AddFile(Filename);
            //closefile(f);
            m.free;
            setlength(sb, 0);
            setlength(searchptrs, 0);
            exit;
          End;
        End
        Else Begin
          // Löschen des Aktuellen Such versuchs, hat nicht geklappt
          For j := i To high(searchptrs) - 1 Do
            searchptrs[j] := searchptrs[j + 1];
          setlength(searchptrs, high(searchptrs));
        End;
      End;
    End;
    m.free;
  End;
End;

Procedure TForm1.GetFilesInDirectory(ADirectory: String;
  Const AMask: Tstringlist; ARekursiv: Boolean);
Var
  sr: TSearchRec;
  t: String;
  s: RawByteString;
  dummy1, dummy2, i: integer;
  b: Boolean;
Begin
  If panik Then exit;
  Application.ProcessMessages;
  // Include Trailing Backslash
  ADirectory := IncludeTrailingPathDelimiter(ADirectory);
  // Suchen der Dateien im Ordner
  If (FindFirstUTF8(ADirectory + '*', faAnyFile And faDirectory, SR) = 0) Then Begin
    Repeat
      // Dank dieser Variante sind wir case insensitiv, obwohl es das Betriebsystem eventuell ist !
      For i := 0 To amask.count - 1 Do Begin
        If CheckBox3.Checked Then Begin
          t := ADirectory + SR.Name;
          dummy2 := 0;
          b := Rex.MatchString(t, dummy1, dummy2);
        End
        Else Begin
          If (pos('.', amask[i]) <> 0) Then Begin
            t := lowercase(ExtractFileExt(ADirectory + SR.Name));
          End
          Else Begin
            t := lowercase(ADirectory + SR.Name);
          End;
          b := (pos(amask[i], t) <> 0) Or (amask[i] = '*') Or (lowercase(SR.Name) = lowercase(amask[i]));
        End;
        If b Then Begin
          If (SR.Name <> '.') And (SR.Name <> '..') Then Begin
            StatusBar1.Panels[0].Text := copy(ADirectory + SR.Name, length(BasePath), length(ADirectory + SR.Name));
            ProceedFind(ADirectory + SR.Name);
            Application.ProcessMessages;
          End;
        End;
      End;
      (*
       * Rekursiver Abstieg
       *)
      If ARekursiv Then Begin
        If (SR.Name <> '.') And (SR.Name <> '..') And (SR.Attr And FaDirectory = FaDirectory) Then Begin
{$IFDEF LiNux}
          s := '';
          FileGetSymLinkTarget(ADirectory + SR.Name, s);
          If s = '' Then Begin // Kein Symlink gefunden
{$ENDIF}
            GetFilesInDirectory(ADirectory + SR.Name, AMask, True);
{$IFDEF LINux}
          End;
{$ENDIF}
        End;
      End;
    Until (FindNextUTF8(SR) <> 0) Or panik;
  End;
  FindCloseUTF8(SR);
End;

Procedure TForm1.FormDropFiles(Sender: TObject; Const FileNames: Array Of String
  );
Begin
  If length(FileNames) <> 0 Then Begin
    If pos('.', FileNames[0]) <> 0 Then Begin
      edit1.text := ExtractFilePath(FileNames[0]);
      edit2.text := ExtractFileExt(FileNames[0]);
    End
    Else Begin
      edit1.text := IncludeTrailingPathDelimiter(FileNames[0]);
      edit2.text := '*';
    End;
  End;
End;

Procedure TForm1.FormShow(Sender: TObject);
Begin
  edit2.SetFocus;
End;

Procedure TForm1.MenuItem10Click(Sender: TObject);
Var
  sl: TStringList;
  i: Integer;
Begin
  // Copy Foldername
  sl := TStringList.Create;
  For i := 0 To ListBox1.Items.Count - 1 Do Begin
    If ListBox1.Selected[i] Then Begin
      sl.add(ExtractFilePath(ListBox1.Items[i]));
    End;
  End;
  Clipboard.AsText := sl.CommaText;
  sl.free;
End;

Procedure TForm1.MenuItem11Click(Sender: TObject);
Var
  p, sou, Des: String;
  c, i: Integer;
Begin
  If SelectDirectoryDialog1.Execute Then Begin
    p := IncludeTrailingPathDelimiter(SelectDirectoryDialog1.FileName);
    c := 0;
    For i := 0 To ListBox1.Items.Count - 1 Do Begin
      If ListBox1.Selected[i] Then Begin
        sou := ListBox1.Items[i];
        Des := p + ExtractFileName(ListBox1.Items[i]);
        If Not CopyFile(utf8tosys(sou), utf8tosys(Des)) Then Begin
          ShowMessage('Error, could not copy: ' + LineEnding +
            '  ' + Sou +
            'to ' + LineEnding +
            '  ' + des);
        End
        Else Begin
          inc(c);
        End;
      End;
    End;
    showmessage(format('Copied %d files.', [c]));
  End;
End;

Procedure TForm1.MenuItem12Click(Sender: TObject);
Begin
  showmessage('Todo.');
End;

Procedure TForm1.MenuItem1Click(Sender: TObject);
Var
  sl: TStringList;
  i: Integer;
Begin
  // Copy Filename
  sl := TStringList.Create;
  For i := 0 To ListBox1.Items.Count - 1 Do Begin
    If ListBox1.Selected[i] Then Begin
      sl.add(ListBox1.Items[i]);
    End;
  End;
  Clipboard.AsText := sl.CommaText;
  sl.free;
End;

Procedure TForm1.MenuItem2Click(Sender: TObject);
Var
  i: integer;
Begin
  // Open Containing Folder
  For i := 0 To ListBox1.Items.Count - 1 Do Begin
    If ListBox1.Selected[i] Then Begin
      OpenURL(ExtractFilePath(ListBox1.Items[i]));
    End;
  End;
End;

Procedure TForm1.MenuItem3Click(Sender: TObject);
Var
  i: integer;
Begin
  // Öffne Datei
  For i := 0 To ListBox1.Items.Count - 1 Do Begin
    If ListBox1.Selected[i] Then Begin
      OpenURL(ListBox1.Items[i]);
    End;
  End;
End;

Procedure TForm1.MenuItem4Click(Sender: TObject);
Begin
  // Stay on Top
  MenuItem4.Checked := Not MenuItem4.Checked;
  If MenuItem4.Checked Then Begin
    form1.FormStyle := fsSystemStayOnTop;
  End
  Else Begin
    form1.FormStyle := fsNormal;
  End;
End;

Procedure TForm1.MenuItem5Click(Sender: TObject);
Begin
  // Export found list
  If SaveDialog1.Execute Then Begin
    ListBox1.Items.SaveToFile(SaveDialog1.FileName);
  End;
End;

Procedure TForm1.MenuItem6Click(Sender: TObject);
Var
  i: Integer;
  error: String;
Begin
  // Delete
  error := '';
  For i := ListBox1.Items.Count - 1 Downto 0 Do Begin
    If ListBox1.Selected[i] Then Begin
      If DeleteFileUTF8(ListBox1.Items[i]) Then Begin
        ListBox1.Items.Delete(i);
      End
      Else Begin
        error := error + LineEnding + ListBox1.Items[i];
      End;
    End;
  End;
  If error <> '' Then Begin
    showmessage('Error unable to delete: ' + error);
  End;
End;

Procedure TForm1.MenuItem8Click(Sender: TObject);
Var
  i: integer;
Begin
  // Select all
  For i := 0 To ListBox1.Items.Count - 1 Do Begin
    ListBox1.Selected[i] := true;
  End;
End;

Procedure TForm1.MenuItem9Click(Sender: TObject);
Var
  i, c: integer;
Begin
  c := 0;
  For i := 0 To ListBox1.Items.Count - 1 Do Begin
    If ListBox1.Selected[i] Then Begin
      inc(c);
      break;
    End;
  End;
  If c = 0 Then Begin
    showmessage('Error nothing selected.');
    exit;
  End;
  form2.Edit1Change(Nil);
  form2.ShowModal;
End;

Procedure TForm1.DragFileListEvent(Sender: TObject; FileList: TStringList);
Var
  i: Integer;
Begin
  For i := 0 To ListBox1.Items.Count - 1 Do Begin
    If ListBox1.Selected[i] Then Begin
      FileList.Add(ListBox1.Items[i]);
    End;
  End;
End;

Procedure TForm1.AddFile(Filename: String);
Begin
  ListBox1.Items.Add(Filename);
  If (ListBox1.ItemIndex = -1) Or (ListBox1.ItemIndex = ListBox1.Items.Count - 2) Then
    ListBox1.TopIndex := ListBox1.Items.Count - 1;
  Application.ProcessMessages;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Var
  param: String;
Begin
  caption := 'Simple scan ver 0.13, by Corpsman, www.Corpsman.de';
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
  Application.Title := caption;
{$IFDEF Linux}
  edit1.text := '\';
{$ELSE}
  edit1.text := 'c:\';
{$ENDIF}
  edit2.text := '';
  edit3.text := '';
  param := trim(ParamStrUTF8(1));
  param := StringReplace(param, '"', '', [rfReplaceAll]);
  param := trim(param);
  If FileExistsUTF8(param) Or DirectoryExistsUTF8(param) Then Begin
    FormDropFiles(self, param);
  End;
  rex := TRegexEngine.Create('');
  fDragDrop := TNativeDragSource.Create(self);
  fDragDrop.Control := ListBox1;
  fDragDrop.OnDragGetFileList := @DragFileListEvent;
End;

Procedure TForm1.Button1Click(Sender: TObject);

  Function prettyTime(val: int64): String;
  Var
    s, e: String;
    v: int64;
  Begin
    If val <= 0 Then Begin
      result := '0.0s';
    End
    Else Begin
      e := 'ms';
      s := '.';
      v := 0;
      If val > 1000 Then Begin // Sekunden
        e := 's';
        v := val Mod 1000;
        val := val Div 1000;
        If val > 60 Then Begin // Minuten
          e := 'm';
          s := ':';
          v := val Mod 60;
          val := val Div 60;
        End;
        If val > 60 Then Begin // Stunden
          e := 'h';
          s := ':';
          v := val Mod 60;
          val := val Div 60;
        End;
      End;
      If v = 0 Then Begin
        result := format('%d%s', [val, e]);
      End
      Else Begin
        result := format('%d%s%0.3d%s', [val, s, v, e]);
      End;
    End;
  End;

Var
  path: String;
  sl: TStringList;
  i: Integer;
  Startime: int64;
  EndTime: int64;
Begin
  If button1.Caption = 'Scan' Then Begin
    If Not DirectoryExistsUTF8(Edit1.Text) Then Begin
      showmessage('Unable to open: ' + Edit1.Text);
      exit;
    End;
    If trim(edit2.text) = '' Then edit2.text := '*';
    button1.Caption := 'Stop';
    StatusBar1.Panels[0].Text := 'Searching..';
    panik := false;
    ListBox1.Clear;
    path := edit1.text;
    sl := Tstringlist.create;
    sl.CommaText := lowercase(Edit2.Text);
    // Abschneiden eines "*" wenn z.B. *.pas steht
    For i := 0 To sl.count - 1 Do Begin
      If (sl[i][1] = '*') And (length(sl[i]) > 1) Then Begin
        sl[i] := copy(sl[i], 2, length(sl[i]));
      End;
    End;
    BasePath := path;
    If CheckBox3.Checked Then Begin
      rex.RegexString := edit2.text;
    End;
    startime := GetTickCount64;
    GetFilesInDirectory(path, sl, CheckBox1.Checked);
    EndTime := GetTickCount64;
    sl.free;
    button1.Caption := 'Scan';
    ListBox1.Invalidate;
    If panik Then Begin
      StatusBar1.Panels[0].Text := format('Search canceled, %d elements found. [Time: %s]', [ListBox1.Items.Count, prettyTime(EndTime - Startime)]);
    End
    Else Begin
      StatusBar1.Panels[0].Text := format('Search done, %d elements found. [Time: %s]', [ListBox1.Items.Count, prettyTime(EndTime - Startime)]);
    End;
  End
  Else Begin
    // Stop
    panik := true;
  End;
End;

Procedure TForm1.Edit2KeyPress(Sender: TObject; Var Key: char);
Begin
  If key = #13 Then Button1.Click;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  rex.free;
End;

End.

