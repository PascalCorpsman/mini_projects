(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of GIT gui                                               *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, PairSplitter,
  ExtCtrls, Grids, Menus;

Const

  (*
   * History: 0.01 = Initialversion (26.11.2023)
   *          0.02 = Added Psoudo Popup Menu to Button1
   *          0.03 = Fix: Lines added / removed was calculated wrong
   *          0.04 = Fix popup menu entry on add to gitignore
   *                 Add: "Clear staging area" button
   *
   * Icons geladen von: https://peacocksoftware.com/silk
   *)
  DefCaption = ' - Commit - CorpsmanGit ver. 0.04';
  CommitText = 'Commit               | ▼';
  ReCommitText = 'ReCommit           | ▼';
  CommitAndPushText = 'Commit && Push | ▼';

Type
  TBufferListItem = Record
    IndexPath: String;
    IndexStatus: String;
  End;

  TBufferList = Array Of TBufferListItem;

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    PopupMenu3: TPopupMenu;
    Separator1: TMenuItem;
    StringGrid1: TStringGrid;
    Procedure Button1Click(Sender: TObject);
    Procedure Button1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure Button2Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure CheckBox1Click(Sender: TObject);
    Procedure CheckBox4Click(Sender: TObject);
    Procedure CheckBox8Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure Label10Click(Sender: TObject);
    Procedure Label4Click(Sender: TObject);
    Procedure Label5Click(Sender: TObject);
    Procedure Label6Click(Sender: TObject);
    Procedure Label7Click(Sender: TObject);
    Procedure Memo1Change(Sender: TObject);
    Procedure MenuItem11Click(Sender: TObject);
    Procedure MenuItem12Click(Sender: TObject);
    Procedure MenuItem13Click(Sender: TObject);
    Procedure MenuItem14Click(Sender: TObject);
    Procedure MenuItem1Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure MenuItem3Click(Sender: TObject);
    Procedure MenuItem4Click(Sender: TObject);
    Procedure MenuItem5Click(Sender: TObject);
    Procedure MenuItem7Click(Sender: TObject);
    Procedure MenuItem8Click(Sender: TObject);
    Procedure MenuItem9Click(Sender: TObject);
    Procedure PopupMenu1Popup(Sender: TObject);
    Procedure StringGrid1CheckboxToggled(Sender: TObject; aCol, aRow: Integer;
      aState: TCheckboxState);
    Procedure StringGrid1CompareCells(Sender: TObject; ACol, ARow, BCol,
      BRow: Integer; Var Result: integer);
    Procedure StringGrid1DblClick(Sender: TObject);
    Procedure StringGrid1KeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure StringGrid1KeyPress(Sender: TObject; Var Key: char);
    Procedure StringGrid1PrepareCanvas(Sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
  private
    BeforeAmentTextBakup: String;
    ProjectRoot: String;
    Procedure ReloadStringgridContent;
    Procedure UpdateInfo();
    Procedure CheckEnableCommitButton();

    Function GetSelection(): TBufferList;
    Function GetIndexOfBufferItem(Const Item: TBufferListItem): Integer;
    Procedure Commit();
  public
    Procedure LoadCommitInformations();
  End;

Var
  Form1: TForm1;
  Form1ShowOnce: Boolean = true;

Implementation

{$R *.lfm}

Uses ugit_common, LazFileUtils, LazUTF8, uGITOptions, ugitprogress, LCLType, lclintf;

Const
  IndexChecked = 0;
  IndexPath = 1;
  IndexFileExt = 2;
  IndexStatus = 3;

  { TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Var
  aDir: String;
  i: Integer;
Begin
  (*
   * Known Bugs:
   *)
  CheckBox1Click(Nil);
  memo1.clear;
  edit1.text := '';
  aDir := '';
  For i := 1 To Paramcount Do Begin
    If DirectoryExistsUTF8(trim(ParamStrUTF8(i))) Then Begin
      aDir := IncludeTrailingPathDelimiter(trim(ParamStrUTF8(i)));
      break;
    End;
    If FileExistsUTF8(trim(ParamStrUTF8(i))) Then Begin
      aDir := trim(ParamStrUTF8(i));
      break;
    End;
  End;
  ProjectRoot := GetRepoRoot(aDir);
  If ProjectRoot = '' Then Begin
    showmessage('"' + aDir + '" is not a valid git repository.');
    halt;
  End;
  LoadCommitInformations();
  button1.caption := CommitText;
End;

Procedure TForm1.FormShow(Sender: TObject);
Begin
  If Form1ShowOnce Then Begin
    Form1ShowOnce := false;
    Memo1.SetFocus;
  End;
End;

Procedure TForm1.Label10Click(Sender: TObject);
Var
  i: Integer;
Begin
  // Modified
  For i := 1 To StringGrid1.RowCount - 1 Do Begin
    StringGrid1.Cells[0, i] := BoolToStr(StringGrid1.Cells[3, i] = TextModified, '1', '0');
  End;
End;

Procedure TForm1.Label4Click(Sender: TObject);
Var
  i: Integer;
Begin
  // All
  For i := 1 To StringGrid1.RowCount - 1 Do Begin
    StringGrid1.cells[0, i] := '1';
  End;
End;

Procedure TForm1.Label5Click(Sender: TObject);
Var
  i: Integer;
Begin
  // None
  For i := 1 To StringGrid1.RowCount - 1 Do Begin
    StringGrid1.cells[0, i] := '0';
  End;
End;

Procedure TForm1.Label6Click(Sender: TObject);
Var
  i: Integer;
Begin
  // Unversioned
  For i := 1 To StringGrid1.RowCount - 1 Do Begin
    StringGrid1.Cells[0, i] := BoolToStr(StringGrid1.Cells[3, i] = TextNotVersioned, '1', '0');
  End;
End;

Procedure TForm1.Label7Click(Sender: TObject);
Var
  i: Integer;
Begin
  // Versioned
  For i := 1 To StringGrid1.RowCount - 1 Do Begin
    StringGrid1.Cells[0, i] := BoolToStr(StringGrid1.Cells[3, i] <> TextNotVersioned, '1', '0');
  End;
End;

Procedure TForm1.Memo1Change(Sender: TObject);
Begin
  CheckEnableCommitButton();
End;

Procedure TForm1.MenuItem11Click(Sender: TObject);
Begin
  button1.caption := CommitText;
End;

Procedure TForm1.MenuItem12Click(Sender: TObject);
Begin
  button1.caption := ReCommitText;
End;

Procedure TForm1.MenuItem13Click(Sender: TObject);
Begin
  button1.caption := CommitAndPushText;
End;

Procedure TForm1.MenuItem14Click(Sender: TObject);
Var
  res: TStringList;
Begin
  // Clear Staging area
  res := RunCommand(ProjectRoot, 'git', ['reset', 'HEAD', '--', '.']);
  res.free;
  ReloadStringgridContent();
End;

Procedure TForm1.MenuItem1Click(Sender: TObject);
Var
  li, i, j: integer;
  res: TStringList;
  pathbakup: String;
  BufferList: TBufferList;
  showWarning: Boolean;
Begin
  // ADD
  showWarning := false;
  BufferList := GetSelection();
  For j := 0 To high(BufferList) Do Begin
    li := GetIndexOfBufferItem(BufferList[j]);
    If li = -1 Then Continue;
    If StringGrid1.Cells[IndexStatus, li] = TextNotVersioned Then Begin
      pathbakup := StringGrid1.Cells[IndexPath, li];
      res := RunCommand(ProjectRoot, 'git', ['add', StringGrid1.Cells[IndexPath, li]]);
      res.free;
      ReloadStringgridContent();
      // Da wir die Datei ja explizit geadded haben wollten tragen wir nun den "Hacken" ein, der duch das ReloadStringgridContent "erhalten" wurde..
      For i := 1 To StringGrid1.RowCount - 1 Do Begin
        If StringGrid1.Cells[IndexPath, i] = pathbakup Then Begin
          StringGrid1.Cells[IndexChecked, i] := '1';
          StringGrid1.Row := i; // Select the row that changed :=)
          break;
        End;
      End;
      UpdateInfo();
    End
    Else Begin
      showWarning := true;
    End;
  End;
  If showWarning Then Begin
    showmessage('Error, only not versioned files are allowed to be added.');
  End;
End;

Procedure TForm1.MenuItem2Click(Sender: TObject);
Var
  li, i, j: integer;
  res: TStringList;
  pathbakup: String;
  BufferList: TBufferList;
Begin
  // Revert
  BufferList := GetSelection();
  For j := 0 To high(BufferList) Do Begin
    li := GetIndexOfBufferItem(BufferList[j]);
    If li = -1 Then Continue;
    Case StringGrid1.Cells[IndexStatus, li] Of
      TextDeleted: Begin // Revert Deleted -> wieder her stellen
          res := RunCommand(ProjectRoot, 'git', ['checkout', '--', StringGrid1.Cells[IndexPath, li]]);
          If res.count = 0 Then Begin
            StringGrid1.DeleteColRow(false, li);
            UpdateInfo();
          End
          Else Begin
            ShowMessage(res.Text);
          End;
          res.free;
        End;
      TextAdded: Begin // Revert Add -> nicht mehr versionieren
          pathbakup := StringGrid1.Cells[IndexPath, li];
          res := RunCommand(ProjectRoot, 'git', ['reset', 'HEAD', StringGrid1.Cells[IndexPath, li]]);
          res.free;
          ReloadStringgridContent;
          // Da wir die Datei ja explizit nicht geadded haben wollten tragen wir nun den "Hacken" weg, der duch das ReloadStringgridContent "erhalten" wurde..
          For i := 1 To StringGrid1.RowCount - 1 Do Begin
            If StringGrid1.Cells[IndexPath, i] = pathbakup Then Begin
              StringGrid1.Cells[IndexChecked, i] := '0';
              StringGrid1.Row := i; // Select the row that changed :=)
              break;
            End;
          End;
          UpdateInfo();
        End;
    End;
  End;
End;

Procedure TForm1.MenuItem3Click(Sender: TObject);
Begin
  // Compare with base -> weiter leiten an dbl click
  StringGrid1DblClick(Nil);
End;

Procedure TForm1.MenuItem4Click(Sender: TObject);
Var
  res: TStringList;
Begin
  // Paste last recent Message
  res := RunCommand(ProjectRoot, 'git', ['log', '-1', '--pretty=%B']);
  Memo1.text := res.Text;
  res.free;
End;

Procedure TForm1.MenuItem5Click(Sender: TObject);
Begin
  // Paste Recent Messages
  showmessage('Todo: show a list of all last messages and let the user select which one to select ..');
End;

Procedure TForm1.MenuItem7Click(Sender: TObject);
Var
  li, j: integer;
  fn: String;
  BufferList: TBufferList;
Begin
  // Delete
  BufferList := GetSelection();
  For j := 0 To high(BufferList) Do Begin
    li := GetIndexOfBufferItem(BufferList[j]);
    If li = -1 Then Continue;
    fn := IncludeTrailingPathDelimiter(ProjectRoot) + StringGrid1.Cells[IndexPath, li];
    If FileExistsUTF8(fn) Then Begin
      If DeleteFileUTF8(fn) Then Begin
        ReloadStringgridContent;
        UpdateInfo();
      End
      Else Begin
        showmessage('Error, could not delete: ' + fn);
      End;
    End
    Else Begin
      showmessage('Error, could not find: ' + fn);
    End;
  End;
End;

Procedure TForm1.MenuItem8Click(Sender: TObject);
Var
  li, j: integer;
  fn, fo: String;
  BufferList: TBufferList;
Begin
  // Open Folder
  BufferList := GetSelection();
  For j := 0 To high(BufferList) Do Begin
    li := GetIndexOfBufferItem(BufferList[j]);
    If li = -1 Then Continue;
    fn := IncludeTrailingPathDelimiter(ProjectRoot) + StringGrid1.Cells[IndexPath, li];
    fo := ExtractFileDir(fn);
    OpenDocument(fo);
  End;
End;

Procedure TForm1.MenuItem9Click(Sender: TObject);
Var
  i, li, j: Integer;
  entry, fn: String;
  sl: TStringList;
  added, showWarning: Boolean;
  BufferList: TBufferList;
Begin
  // Add to Git Ignore
  BufferList := GetSelection();
  showWarning := false;
  For j := 0 To high(BufferList) Do Begin
    li := GetIndexOfBufferItem(BufferList[j]);
    If li = -1 Then Continue;
    If StringGrid1.Cells[IndexStatus, li] = TextNotVersioned Then Begin
      fn := IncludeTrailingPathDelimiter(ProjectRoot) + '.gitignore';
      sl := TStringList.Create;
      added := false;
      // 1. Gibt es die Git Ignore schon ?
      If FileExistsUTF8(fn) Then Begin
        sl.LoadFromFile(fn);
      End
      Else Begin
        added := true;
      End;
      entry := TMenuItem(sender).Caption;
      sl.Add(entry);
      sl.SaveToFile(fn);
      sl.free;
      ReloadStringgridContent;
      // Wir wollten ja den Ignore -> Adden
      If added Then Begin
        For i := 1 To StringGrid1.RowCount - 1 Do Begin
          If StringGrid1.Cells[IndexPath, i] = '.gitignore' Then Begin
            StringGrid1.Cells[IndexChecked, i] := '1';
            break;
          End;
        End;
      End;
      UpdateInfo();
    End
    Else Begin
      showWarning := true;
    End;
  End;
  If showWarning Then Begin
    showmessage('Error, only un versioned files can be ignored.');
  End;
End;

Procedure TForm1.PopupMenu1Popup(Sender: TObject);
Var
  li: integer;
Begin
  // Prüfen auf was das Popup erstellt wurde ..
  li := StringGrid1.Selection.Top;
  MenuItem1.Visible := false; // ADD
  MenuItem2.Visible := false; // Revert
  MenuItem6.Visible := false; // Add to Git Ignore
  If li <> -1 Then Begin
    If StringGrid1.Selection.Top = StringGrid1.Selection.Bottom Then Begin
      MenuItem9.Caption := StringGrid1.Cells[IndexPath, li];
    End
    Else Begin
      MenuItem9.Caption := 'By name';
    End;
    MenuItem10.Caption := '*' + StringGrid1.Cells[IndexFileExt, li];
    Case StringGrid1.Cells[IndexStatus, li] Of
      TextDeleted: MenuItem2.Visible := true;
      TextNotVersioned: Begin
          MenuItem1.Visible := true;
          MenuItem6.Visible := true;
        End;
      TextAdded: MenuItem2.Visible := true;
    End;
  End
  Else Begin
    // TODO: Verhindern dass das Popup kommt ?
  End;
End;

Procedure TForm1.StringGrid1CheckboxToggled(Sender: TObject; aCol,
  aRow: Integer; aState: TCheckboxState);
Begin
  UpdateInfo();
End;

Procedure TForm1.StringGrid1CompareCells(Sender: TObject; ACol, ARow, BCol,
  BRow: Integer; Var Result: integer);
Begin
  Case ACol Of
    IndexChecked: Begin
        result := strtointdef(StringGrid1.Cells[ACol, arow], 0) - strtointdef(StringGrid1.Cells[BCol, brow], 0);
      End
  Else Begin
      result := CompareStr(StringGrid1.Cells[acol, arow], StringGrid1.Cells[bcol, brow]);
    End;
  End;
  If StringGrid1.SortOrder = soDescending Then Begin
    result := -Result;
  End;
End;

Procedure TForm1.StringGrid1DblClick(Sender: TObject);
Var
  li: integer;
  res: TStringList;
  fn: String;
Begin
  // Diff Anzeigen
  // TODO: Support Multiple Lines !
  li := StringGrid1.Selection.Top;
  Case StringGrid1.Cells[IndexStatus, li] Of
    TextModified: Begin
        If GetEffectiveRepoSettings(ProjectRoot).DiffTool <> '' Then Begin
          res := RunCommand(ProjectRoot, 'git', ['difftool', StringGrid1.Cells[IndexPath, li], '--no-prompt']);
          res.free;
        End
        Else Begin
          showmessage('Error, no difftool set.');
        End;
      End;
    TextDeleted: Begin
        // Die Datei ist gelöscht -> wir müssen sie geschwind laden damit wir sie anzeigen können
        fn := GetTempFileName;
        fn := ChangeFileExt(fn, StringGrid1.Cells[IndexFileExt, li]);
        res := RunCommand(ProjectRoot, 'git', ['show', 'HEAD:' + StringGrid1.Cells[IndexPath, li]]);
        res.SaveToFile(fn);
        res.free;
        OpenDocument(fn);
        // TODO: Theoretisch sollte man das TMP-File nachdem OpenDocument fertig ist auch wieder löschen ...
      End
  Else Begin
      OpenDocument(IncludeTrailingPathDelimiter(ProjectRoot) + StringGrid1.Cells[IndexPath, li]);
    End;
  End;
End;

Procedure TForm1.StringGrid1KeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  // Neu Einlesen des Quellverzeichnisses
  If key = VK_F5 Then Begin
    ReloadStringgridContent;
  End;
End;

Procedure TForm1.StringGrid1KeyPress(Sender: TObject; Var Key: char);
Begin
  // Unter Windows gehts, unter Linux muss das von Hand gemacht werden *g*
  If (key = #32) And (StringGrid1.Selection.Top <> -1) Then Begin
    If StringGrid1.Cells[0, StringGrid1.Selection.Top] = '1' Then Begin
      StringGrid1.Cells[0, StringGrid1.Selection.Top] := '0';
    End
    Else Begin
      StringGrid1.Cells[0, StringGrid1.Selection.Top] := '1';
    End;
    StringGrid1.Invalidate;
    // Evtl, hat sich ein Checked geändert
    UpdateInfo;
  End;
End;

Procedure TForm1.StringGrid1PrepareCanvas(Sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
Begin
  If arow = 0 Then Begin
    StringGrid1.canvas.Font.Color := clblack;
  End
  Else Begin
    Case StringGrid1.Cells[IndexStatus, arow] Of
      TextModified: StringGrid1.canvas.Font.Color := clblue;
      TextAdded: StringGrid1.canvas.Font.Color := clPurple;
      TextDeleted: StringGrid1.canvas.Font.Color := clMaroon;
    Else
      StringGrid1.canvas.Font.Color := clblack;
    End;
    If arow = StringGrid1.Selection.Top Then Begin
      StringGrid1.canvas.Font.Color := clWhite;
    End;
  End;
End;

Procedure TForm1.LoadCommitInformations;
Var
  r: TCommitInformations;
  i: Integer;
  bUnversioned, bVersioned, bAdded, bDeleted, bModified: Boolean;
Begin
  r := GetCommitInformations(ProjectRoot, '0000000000000000000000000000000000000000');
  caption := r.RepoRoot + DefCaption;
  Label15.caption := r.BranchSelector.Local;
  StringGrid1.RowCount := length(r.CommitFileInfo) + 1;
  bUnversioned := false;
  bVersioned := false;
  bAdded := false;
  bDeleted := false;
  bModified := false;
  For i := 0 To high(r.CommitFileInfo) Do Begin
    StringGrid1.Cells[0, i + 1] := BoolToStr(r.CommitFileInfo[i].Status <> csNotVersioned, '1', '0');
    StringGrid1.Cells[1, i + 1] := r.CommitFileInfo[i].FileName;
    StringGrid1.Cells[2, i + 1] := ExtractFileExt(r.CommitFileInfo[i].FileName);
    StringGrid1.Cells[3, i + 1] := StatusToString(r.CommitFileInfo[i].Status);
    StringGrid1.Cells[4, i + 1] := inttostr(r.CommitFileInfo[i].LinesAdded);
    StringGrid1.Cells[5, i + 1] := inttostr(r.CommitFileInfo[i].LinesRemoved);
    bUnversioned := bUnversioned Or (r.CommitFileInfo[i].Status = csNotVersioned);
    bVersioned := bVersioned Or (r.CommitFileInfo[i].Status = csModified)
      Or (r.CommitFileInfo[i].Status = csAddedWithModifications)
      Or (r.CommitFileInfo[i].Status = csModifiedWithFurtherModifications)
      Or (r.CommitFileInfo[i].Status = csAdded)
      Or (r.CommitFileInfo[i].Status = csDeleted)
      Or (r.CommitFileInfo[i].Status = csRenamed)
      Or (r.CommitFileInfo[i].Status = csCopied);
    badded := bAdded Or (r.CommitFileInfo[i].Status = csAdded);
    bDeleted := bDeleted Or (r.CommitFileInfo[i].Status = csDeleted);
    bModified := bModified Or (r.CommitFileInfo[i].Status = csModified)
      Or (r.CommitFileInfo[i].Status = csAddedWithModifications)
      Or (r.CommitFileInfo[i].Status = csModifiedWithFurtherModifications);
  End;
  label6.Enabled := bUnversioned;
  label7.Enabled := bVersioned;
  label8.Enabled := bAdded;
  label9.Enabled := bDeleted;
  label10.Enabled := bModified;
  StringGrid1.AutoSizeColumns;
  StringGrid1.ColWidths[0] := Scale96ToForm(50); // Das Autosize scheitert bei der "Checkbox" Spalte
  CheckEnableCommitButton();
  UpdateInfo();
End;

Procedure TForm1.UpdateInfo;
Var
  i, cnt: integer;
Begin
  cnt := 0;
  For i := 1 To StringGrid1.RowCount - 1 Do Begin
    If StringGrid1.cells[0, i] = '1' Then Begin
      inc(cnt);
    End;
  End;
  label13.caption := format('%d files selected, %d files total', [cnt, StringGrid1.RowCount - 1]);
End;

Procedure TForm1.ReloadStringgridContent;
Var
  i: Integer;
  j: Integer;
  bakup: Array Of Record
    Checked: String;
    Path: String;
  End;
  s: String;
Begin
  s := Memo1.Text;
  // Speichern des "Checked" und Unchecked Status
  bakup := Nil;
  setlength(bakup, StringGrid1.RowCount - 1);
  For i := 0 To high(bakup) Do Begin
    bakup[i].Checked := StringGrid1.Cells[IndexChecked, i + 1];
    bakup[i].Path := StringGrid1.Cells[IndexPath, i + 1];
  End;
  // Neu Laden
  LoadCommitInformations;
  // Wieder Herstellen von "allem" was geht
  For i := 1 To StringGrid1.RowCount - 1 Do Begin
    For j := 0 To high(bakup) Do Begin
      If bakup[j].Path = StringGrid1.Cells[IndexPath, i] Then Begin
        StringGrid1.Cells[IndexChecked, i] := bakup[j].Checked;
        break;
      End;
    End;
  End;
  Memo1.Text := s;
  UpdateInfo();
End;

Procedure TForm1.CheckBox1Click(Sender: TObject);
Begin
  edit1.Visible := CheckBox1.Checked;
End;

Procedure TForm1.CheckBox4Click(Sender: TObject);
Var
  allowed: Boolean;
  i: Integer;
Begin
  If CheckBox4.Checked Then Begin
    // Das Ammend, geht nur, wenn keine Modifizierten / Geaddedet, Gelöschten .. Dateien da sind
    allowed := true;
    For i := 1 To StringGrid1.RowCount - 1 Do Begin
      If StringGrid1.Cells[IndexStatus, i] = TextAdded Then allowed := false;
      If StringGrid1.Cells[IndexStatus, i] = TextModified Then allowed := false;
      If StringGrid1.Cells[IndexStatus, i] = TextDeleted Then allowed := false;
      If StringGrid1.Cells[IndexStatus, i] = TextRenamed Then allowed := false;
      If StringGrid1.Cells[IndexStatus, i] = TextCopied Then allowed := false;
    End;
    If Not allowed Then Begin
      CheckBox4.Checked := false;
      ShowMessage('Amend the last commit message is only allowed if there is nothing staged.');
      exit;
    End;
    BeforeAmentTextBakup := Memo1.Text;
    MenuItem4Click(Nil); // Laden des letzten Log textes
    // TODO: Theoretisch müsste der komplette letzte Commit in das Stringgrid geladen werden, aber wie ?
    //       \-> So ist das Ammend nur zum ändern der Texte geeignet...
  End
  Else Begin
    Memo1.Text := BeforeAmentTextBakup;
  End;
End;

Procedure TForm1.CheckBox8Click(Sender: TObject);
Var
  i: Integer;
Begin
  // Show hide versioned files
  If CheckBox8.Checked Then Begin
    ReloadStringgridContent;
  End
  Else Begin
    StringGrid1.BeginUpdate;
    For i := StringGrid1.RowCount - 1 Downto 1 Do Begin
      If StringGrid1.cells[IndexStatus, i] = TextNotVersioned Then Begin
        StringGrid1.DeleteColRow(false, i);
      End;
    End;
    StringGrid1.EndUpdate(true);
    UpdateInfo();
  End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  // Nichts der Code Steht im Mouse Down !
End;

Procedure TForm1.Button1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  p: TPoint;
Begin
  If Not Button1.Enabled Then exit;
  // Entweder der "Button" oder das Popup
  If x > button1.Width - Button1.Height Then Begin
    p := Button1.ClientToScreen(point(0, button1.Height));
    PopupMenu3.PopUp(p.x, p.y);
  End
  Else Begin
    Case Button1.Caption Of
      CommitText: Commit();
      ReCommitText: showmessage('Todo.');
      CommitAndPushText: showmessage('Todo.');
    End;
  End;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Begin
  // Options
  GitOptions.LoadOptions(ProjectRoot);
  GitOptions.Showmodal;
End;

Procedure TForm1.Commit();
Var
  cnt, i: integer;
  sl, res: TStringList;
  fn: String;
  Params: Array Of String;
Begin
  // Commit
  // -- Darf überhaupt Eingecheckt werden ?
  cnt := 0;
  For i := 1 To StringGrid1.RowCount - 1 Do Begin
    If StringGrid1.Cells[IndexChecked, i] = '1' Then Begin
      inc(cnt);
    End;
  End;
  If (cnt = 0) And (Not CheckBox4.Checked) Then Begin // Beim Amend lassen wir es zu, dass nichts geändert wird ..
    showmessage('Error, nothing selected.');
    exit;
  End;
  GitProgress.ProgressBar1.Position := 0;
  GitProgress.ProgressBar1.Max := cnt;
  GitProgress.Memo1.Clear;
  GitProgress.ModalResult := mrNone;
  GitProgress.SetContinueButtonTo('Push');
  GitProgress.Show;
  Application.ProcessMessages;
  // Einchecken der Einzelnen Dateien
  For i := 1 To StringGrid1.RowCount - 1 Do Begin
    If StringGrid1.Cells[IndexChecked, i] = '1' Then Begin
      GitProgress.ProgressBar1.Position := GitProgress.ProgressBar1.Position + 1;
      Application.ProcessMessages;
      If StringGrid1.Cells[IndexStatus, i] = TextDeleted Then Begin
        res := RunCommand(ProjectRoot, 'git', ['rm', StringGrid1.Cells[IndexPath, i]]);
      End
      Else Begin
        res := RunCommand(ProjectRoot, 'git', ['add', StringGrid1.Cells[IndexPath, i]]);
      End;
      res.free;
    End;
  End;
  // Es gab was ein zu checken, dann adden wir das nun
  fn := GetTempFileName();
  sl := TStringList.Create;
  sl.Text := Memo1.Text + LineEnding;
  sl.SaveToFile(fn);
  sl.free;
  Params := Nil;
  setlength(Params, 1);
  Params[0] := 'commit';
  If CheckBox4.Checked Then Begin
    setlength(Params, high(Params) + 2);
    Params[high(Params)] := '--amend';
  End;
  setlength(Params, high(Params) + 2);
  Params[high(Params)] := '--file=' + fn;
  If GitProgress.RunVisualCommand(ProjectRoot, 'git', Params) Then Begin
    DeleteFile(fn);
    close;
  End
  Else Begin
    DeleteFile(fn);
  End;
End;

Procedure TForm1.CheckEnableCommitButton;
Var
  b: Boolean;
Begin
  b := true;
  If StringGrid1.RowCount = 1 Then b := false;
  If trim(memo1.Text) = '' Then b := false;
  button1.enabled := b;
End;

Function TForm1.GetSelection: TBufferList;
Var
  i: LongInt;
Begin
  Result := Nil;
  setlength(Result, StringGrid1.Selection.Bottom - StringGrid1.Selection.Top + 1);
  For i := StringGrid1.Selection.Top To StringGrid1.Selection.Bottom Do Begin
    Result[i - StringGrid1.Selection.Top].IndexPath := StringGrid1.Cells[IndexPath, i];
    Result[i - StringGrid1.Selection.Top].IndexStatus := StringGrid1.Cells[IndexStatus, i];
  End;
End;

Function TForm1.GetIndexOfBufferItem(Const Item: TBufferListItem): Integer;
Var
  i: Integer;
Begin
  Result := -1;
  For i := 1 To StringGrid1.RowCount - 1 Do Begin
    If Item.IndexPath = StringGrid1.Cells[IndexPath, i] Then Begin
      Result := i;
      break;
    End;
  End;
End;

End.

