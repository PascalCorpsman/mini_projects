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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  FileCtrl, Buttons, Grids, ExtCtrls, Menus, Types, ugitgraph;

(*
 * Docu: https://tortoisegit.org/docs/tortoisegit/tgit-dug-showlog.html
 *)

 (*
  * History: 0.01 = Initialversion (26.11.2023)
  *          0.02 = Activate Git graph
  *          0.03 = FIX: Git graph could not display Merge and Branch at the same line
  *          0.04 = FIX: Git graph could not display Merge of a branch without "removing" branch
  *          0.05 = FIX: Git graph sometimes merged the wrong branches
  *          0.06 = ADD: show Branch locations (Red / Green / Brown text in front of commit message)
  *                 ADD: Ability to create branches / tags
  *
  * Icons geladen von: https://peacocksoftware.com/silk
  *)

Const
  DefCaption = ' - Log Messages - CorpsmanGit ver. 0.06';

  IndexActionFileModified = 0; // If a revision modified a file or directory, the modified icon is shown in the first column.
  IndexActionFileAdded = 1; // If a revision added a File Or directory, the added icon Is shown In the second column.
  IndexActionFileDeleted = 2; // If a revision deleted a file or directory, the deleted icon is shown in the third column.
  IndexActionFileRenamed = 3; // If a revision replaced(rename) a file, the replaced icon is shown in the fourth column.

  BitFileModified = 1;
  BitFileAdded = 2;
  BitFileDeleted = 4;
  BitFileRenamed = 8;

Type

  TBranch = Record
    Name: String;
    Color: TColor;
  End;

  TAdditional = Record
    Branchs: Array Of TBranch;
    Tags: Array Of String;
  End;

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
    ComboBox1: TComboBox;
    DateEdit1: TDateEdit;
    DateEdit2: TDateEdit;
    Edit1: TEdit;
    FilterComboBox1: TFilterComboBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem39: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem40: TMenuItem;
    MenuItem41: TMenuItem;
    MenuItem42: TMenuItem;
    MenuItem43: TMenuItem;
    MenuItem44: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    PopupMenu3: TPopupMenu;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    Separator4: TMenuItem;
    Separator5: TMenuItem;
    Separator6: TMenuItem;
    Separator7: TMenuItem;
    Separator8: TMenuItem;
    Separator9: TMenuItem;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    StringGrid1: TStringGrid;
    StringGrid2: TStringGrid;
    Procedure Button2Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure CheckBox2Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure MenuItem24Click(Sender: TObject);
    Procedure MenuItem25Click(Sender: TObject);
    Procedure MenuItem44Click(Sender: TObject);
    Procedure PopupMenu3Popup(Sender: TObject);
    Procedure StringGrid1Click(Sender: TObject);
    Procedure StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    Procedure StringGrid1KeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure StringGrid2DblClick(Sender: TObject);
  private
    ProjectRoot: String;
    Graph: TGraph;
    Additionals: Array Of TAdditional;
    FirstLoadLCL: Boolean;
  public
    Procedure LoadLCL;
  End;

Var
  FormShowOnce: Boolean = true;
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses
  unit2 // Create Branch Dialog
  , LCLType
  , uGITOptions, ugit_common, LazFileUtils, LazUTF8, math, DateUtils, LCLIntf;

{ TForm1 }

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  Close
End;

Procedure TForm1.Button5Click(Sender: TObject);
Begin
  // Options
  GitOptions.LoadOptions(ProjectRoot);
  GitOptions.Showmodal;
End;

Procedure TForm1.Button6Click(Sender: TObject);
Var
  p: Tpoint;
Begin
  p := Button6.ClientToScreen(point(0, button6.Height));
  PopupMenu1.PopUp(p.x, p.y);
End;

Procedure TForm1.Button7Click(Sender: TObject);
Var
  p: Tpoint;
Begin
  p := Button7.ClientToScreen(point(0, button7.Height));
  PopupMenu2.PopUp(p.x, p.y);
End;

Procedure TForm1.CheckBox2Click(Sender: TObject);
Begin
  LoadLCL; // Geändert Alle Branches oder nicht
End;

Procedure TForm1.FormCreate(Sender: TObject);
Var
  i: Integer;
  aDir: String;
Begin
  // TODO: Context Menu completely missing !
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
  (*
   * Known Bugs:
   *)
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
  FirstLoadLCL := true;
End;

Procedure TForm1.FormShow(Sender: TObject);
Begin
  StringGrid1.SetFocus;
  If FormShowOnce Then Begin
    FormShowOnce := false;
    LoadLCL;
  End;
End;

Procedure TForm1.MenuItem24Click(Sender: TObject);
Var
  li: Integer;
Begin
  // Create Branch at this version
  li := StringGrid1.Selection.Top;
  If li <= 0 Then exit; // Keine Ahnung nichts ausgewählt
  form2.Init(ProjectRoot, StringGrid1.Cells[5, li], 'Branch');
  If form2.showmodal = mrOK Then Begin
    LoadLCL;
  End;
End;

Procedure TForm1.MenuItem25Click(Sender: TObject);
Var
  li: LongInt;
Begin
  // Create Tag at this version
  li := StringGrid1.Selection.Top;
  If li <= 0 Then exit; // Keine Ahnung nichts ausgewählt
  form2.Init(ProjectRoot, StringGrid1.Cells[5, li], 'Tag');
  If form2.showmodal = mrOK Then Begin
    LoadLCL;
  End;
End;

Procedure TForm1.MenuItem44Click(Sender: TObject);
Var
  li: LongInt;
  res: TStringList;
Begin
  // Delete Tag
  // Create Tag at this version
  li := StringGrid1.Selection.Top;
  If li <= 0 Then exit; // Keine Ahnung nichts ausgewählt
  res := RunCommand(ProjectRoot, 'git', ['tag', '-d', copy(MenuItem44.caption, length('Delete refs/tags/') + 1, length(MenuItem44.caption))]);
  showmessage(res.text);
  res.free;
  //form2.Init(ProjectRoot, StringGrid1.Cells[5, li], 'Tag');
  //If form2.showmodal = mrOK Then Begin
  LoadLCL;
  //End;
End;

Procedure TForm1.PopupMenu3Popup(Sender: TObject);
Var
  li: Integer;
Begin
  // Create Branch at this version
  li := StringGrid1.Selection.Top;
  If li <= 0 Then exit; // Keine Ahnung nichts ausgewählt
  // TODO: Anpassen aller möglichen Popup Einträge ...
  MenuItem24.Visible := li > 1; // Create Branch at this version
  MenuItem25.Visible := li > 1; // Create Tag at this version
  MenuItem44.Visible := assigned(Additionals[li].Tags);
  Separator9.Visible := MenuItem44.Visible;
  If assigned(Additionals[li].Tags) Then Begin
    // TODO: Theoretisch sollte hier ermittelt werden auf welchem Tag die Maus steht und dann der Richtige angewählt werden !
    MenuItem44.Caption := 'Delete refs/tags/' + Additionals[li].Tags[0];
  End;
End;

Procedure TForm1.StringGrid1Click(Sender: TObject);
Var
  li, i: Integer;
  CHash: String;
  info: TCommitInformations;
Begin
  li := StringGrid1.Selection.Top;
  If li <= 0 Then exit; // Keine Ahnung nichts ausgewählt
  cHash := StringGrid1.Cells[5, li];
  info := GetCommitInformations(ProjectRoot, CHash);
  Memo1.Text := 'SHA-1: ' + cHash + LineEnding + LineEnding + '* ' + info.LogMessage;
  StringGrid2.RowCount := length(info.CommitFileInfo) + 1;
  For i := 0 To high(info.CommitFileInfo) Do Begin
    StringGrid2.Cells[0, i + 1] := info.CommitFileInfo[i].FileName;
    StringGrid2.Cells[1, i + 1] := ExtractFileExt(info.CommitFileInfo[i].FileName);
    StringGrid2.Cells[2, i + 1] := StatusToString(info.CommitFileInfo[i].Status);
    StringGrid2.Cells[3, i + 1] := inttostr(info.CommitFileInfo[i].LinesAdded);
    StringGrid2.Cells[4, i + 1] := inttostr(info.CommitFileInfo[i].LinesRemoved);
  End;
  StringGrid2.AutoSizeColumns;
End;

Procedure TForm1.StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
Var
  w: integer;
  aValue: Integer;
  r: TRect;
  offset, h, j: integer;
  bc, fc: TColor;
Begin
  If (aCol = 1) And (aRow > 0) Then Begin // Spalte "Actions"
    // Hintergrund Löschen
    If gdSelected In aState Then Begin
      StringGrid1.Canvas.Brush.Color := clHighlight;
      StringGrid1.Canvas.Pen.Color := clHighlight;
    End
    Else Begin
      StringGrid1.Canvas.Brush.Color := StringGrid1.Color;
      StringGrid1.Canvas.Pen.Color := StringGrid1.Color;
    End;
    StringGrid1.Canvas.Rectangle(aRect);
    // Auslesen der Actions und passend Malen
    aValue := strtointdef(StringGrid1.Cells[aCol, aRow], 0);
    w := Scale96ToForm(ImageList1.Width);
    h := Scale96ToForm(ImageList1.Height);
    r.Left := aRect.Left + w Div 2;
    r.Right := aRect.Left + (w * 3) Div 2;
    r.Top := aRect.Top + (StringGrid1.RowHeights[aRow] - h) Div 2;
    r.Bottom := aRect.Top + (StringGrid1.RowHeights[aRow] - h) Div 2 + h;
    If aValue And BitFileModified > 0 Then Begin
      ImageList1.StretchDraw(StringGrid1.Canvas, IndexActionFileModified, r);
    End;
    r.Left := r.Left + w;
    r.Right := r.Right + w;
    If aValue And BitFileAdded > 0 Then Begin
      ImageList1.StretchDraw(StringGrid1.Canvas, IndexActionFileAdded, r);
    End;
    r.Left := r.Left + w;
    r.Right := r.Right + w;
    If aValue And BitFileDeleted > 0 Then Begin
      ImageList1.StretchDraw(StringGrid1.Canvas, IndexActionFileDeleted, r);
    End;
    r.Left := r.Left + w;
    r.Right := r.Right + w;
    If aValue And BitFileRenamed > 0 Then Begin
      ImageList1.StretchDraw(StringGrid1.Canvas, IndexActionFileRenamed, r);
    End;
  End;
  If (aCol = 0) And (aRow > 0) Then Begin
    If high(Graph) < aRow - 1 Then exit; // Graph noch nicht initialisiert
    DrawGraphRow(StringGrid1.Canvas, Graph[aRow - 1], aRect);
  End;
  If (aCol = 2) Then Begin
    offset := 0;
    //  // Die Zelle "Löschen"
    If assigned(Additionals[aRow].tags) Or Assigned(Additionals[aRow].Branchs) Then Begin
      bc := StringGrid1.canvas.Brush.Color;
      fc := StringGrid1.canvas.Font.Color;
      StringGrid1.canvas.Pen.Color := StringGrid1.canvas.Brush.Color;
      StringGrid1.canvas.Rectangle(aRect.Left + 1, aRect.Top + 1, aRect.Right - 1, aRect.Bottom - 1);
      offset := 0;
      If assigned(Additionals[aRow].Branchs) Then Begin
        For j := 0 To high(Additionals[aRow].Branchs) Do Begin
          StringGrid1.canvas.Brush.Color := Additionals[aRow].Branchs[j].Color;
          StringGrid1.canvas.Font.Color := clBlack;
          StringGrid1.canvas.TextOut(aRect.Left + offset, (aRect.Bottom + aRect.Top - StringGrid1.Canvas.TextHeight('8')) Div 2, Additionals[aRow].Branchs[j].Name);
          offset := offset + StringGrid1.canvas.TextWidth(Additionals[aRow].Branchs[j].Name) + Scale96ToForm(2);
        End;
      End;
      If assigned(Additionals[aRow].tags) Then Begin
        StringGrid1.canvas.Brush.Color := clYellow;
        StringGrid1.canvas.Font.Color := clBlack;
        For j := 0 To high(Additionals[aRow].tags) Do Begin
          StringGrid1.canvas.TextOut(aRect.Left + offset, (aRect.Bottom + aRect.Top - StringGrid1.Canvas.TextHeight('8')) Div 2, Additionals[aRow].tags[j]);
          offset := offset + StringGrid1.canvas.TextWidth(Additionals[aRow].tags[j]) + Scale96ToForm(2);
        End;
      End;
      // Den eigentlichen Text der Zelle anfügen
      StringGrid1.canvas.Brush.Color := bc;
      StringGrid1.canvas.Font.Color := fc;
      StringGrid1.canvas.TextOut(aRect.Left + offset, (aRect.Bottom + aRect.Top - StringGrid1.Canvas.TextHeight('8')) Div 2, StringGrid1.Cells[aCol, aRow]);
    End;
  End;
End;

Procedure TForm1.StringGrid1KeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If key = vk_F5 Then LoadLCL;
End;

Procedure TForm1.StringGrid2DblClick(Sender: TObject);
Var
  li: integer;
  hash: String;
  res: TStringList;
Begin
  If GetEffectiveRepoSettings(ProjectRoot).DiffTool <> '' Then Begin
    li := StringGrid2.Selection.Top;
    If li <= 0 Then exit;
    hash := StringGrid1.Cells[5, StringGrid1.Selection.Top];
    If hash = '0000000000000000000000000000000000000000' Then Begin
      // TODO: Wie wird "Deleted" behandelt ?
      If StringGrid2.Cells[2, li] <> TextModified Then Begin
        res := TStringList.Create;
        OpenDocument(IncludeTrailingPathDelimiter(ProjectRoot) + StringGrid2.Cells[0, li]);
      End
      Else Begin
        res := RunCommand(ProjectRoot, 'git', ['difftool', StringGrid2.Cells[0, li], '--no-prompt']);
      End;
    End
    Else Begin
      res := RunCommand(ProjectRoot, 'git', ['difftool', '-y', hash, '--', StringGrid2.Cells[0, li]]);
    End;
    res.free;
  End
  Else Begin
    showmessage('Error, no difftool set.');
  End;
End;

Procedure TForm1.LoadLCL;

  Procedure CalcAdditionals();
  Var
    res: TStringList;
    i, j: Integer;
    sa: TStringArray;
    aBranch, p: String;

  Begin
    // First get name of Actual Branch by using "git symbolic-ref --short HEAD" or "git branch --show-current"
    res := RunCommand(ProjectRoot, 'git', ['symbolic-ref', '--short', 'HEAD']);
    aBranch := trim(res.Text);
    res.free;
    Additionals := Nil;
    setlength(Additionals, StringGrid1.RowCount);
    For i := 0 To high(Additionals) Do Begin
      Additionals[i].tags := Nil;
      Additionals[i].Branchs := Nil;
    End;
    form2.ComboBox2.Clear;
    form2.ComboBox3.Clear;
    form2.ComboBox3.Items.Add('FETCH_HEAD');
    res := RunCommand(ProjectRoot, 'git', ['show-ref', '--heads', '--tags' {, '--dereference'}]);
    For i := 0 To res.Count - 1 Do Begin
      If trim(res[i]) <> '' Then Begin
        sa := trim(res[i]).Split(' ');
        p := sa[1];
        For j := 2 To StringGrid1.RowCount - 1 Do Begin
          If StringGrid1.Cells[5, j] = sa[0] Then Begin
            If pos('refs/heads/', p) = 1 Then Begin
              setlength(Additionals[j].Branchs, high(Additionals[j].Branchs) + 2);
              Additionals[j].Branchs[high(Additionals[j].Branchs)].Name := copy(p, length('refs/heads/') + 1, length(p));
              form2.ComboBox3.Items.Add(Additionals[j].Branchs[high(Additionals[j].Branchs)].Name);
              If aBranch = Additionals[j].Branchs[high(Additionals[j].Branchs)].Name Then Begin
                Additionals[j].Branchs[high(Additionals[j].Branchs)].Color := $000000C8; // Rot
                form2.ComboBox3.ItemIndex := form2.ComboBox3.Items.Count - 1;
              End
              Else Begin
                Additionals[j].Branchs[high(Additionals[j].Branchs)].Color := $0000C300; // Grün
              End;
            End;
            If pos('refs/tags/', p) = 1 Then Begin
              setlength(Additionals[j].tags, high(Additionals[j].tags) + 2);
              Additionals[j].tags[high(Additionals[j].tags)] := copy(p, length('refs/tags/') + 1, length(p));
              form2.ComboBox2.Items.Add(Additionals[j].tags[high(Additionals[j].tags)]);
              form2.ComboBox2.ItemIndex := 0;
            End;
            break;
          End;
        End;
      End;
    End;
    res.free;
    res := RunCommand(ProjectRoot, 'git', ['ls-remote', 'origin']);
    For i := 0 To res.Count - 1 Do Begin
      If trim(res[i]) <> '' Then Begin
        sa := trim(res[i]).Split(#9);
        p := sa[1];
        For j := 2 To StringGrid1.RowCount - 1 Do Begin
          If StringGrid1.Cells[5, j] = sa[0] Then Begin
            If pos('refs/heads/', p) = 1 Then Begin
              setlength(Additionals[j].Branchs, high(Additionals[j].Branchs) + 2);
              Additionals[j].Branchs[high(Additionals[j].Branchs)].Name := 'origin/' + copy(p, length('refs/heads/') + 1, length(p));
              Additionals[j].Branchs[high(Additionals[j].Branchs)].Color := $00AADDFF; // Hellbraun
              form2.ComboBox3.Items.Add('remotes/' + Additionals[j].Branchs[high(Additionals[j].Branchs)].Name);
            End;
            break;
          End;
        End;
      End;
    End;
    res.free;
  End;

Var
  sl: TStringList;
  aParams: Array Of String;
  sa, sa2: TStringArray;
  aActions, i, j: Integer;
  s: String;
  d, FromDate, ToDate: TDateTime;
  first: Boolean;
  GraphInfo: TGraphInfoArray;
Begin
  caption := ProjectRoot + DefCaption;
  edit1.text := '';
  StringGrid1.RowCount := 2;
  StringGrid1.Cells[2, 1] := 'Working tree changes';
  StringGrid1.Cells[5, 1] := '0000000000000000000000000000000000000000';
  aParams := Nil;
  (*
   * Command to create debug log streams for Git_Graph test environment:
   * git --no-pager log --pretty=format:"%H;%P;%s" --all > log.txt
   *)
  setlength(aParams, 5);
  aParams[0] := '--no-pager';
  aParams[1] := 'log';
  aParams[2] := '--date=format:"%d.%m.%Y %H:%M:%S"';
  aParams[3] := '--pretty=format:"%H;%P;%an;%ad;%s"';
  aParams[4] := '--name-status';
  If CheckBox2.Checked Then Begin
    setlength(aParams, high(aParams) + 2);
    aParams[high(aParams)] := '--all';
  End;
  sl := RunCommand(ProjectRoot, 'git', aParams);
  i := 0;
  GraphInfo := Nil;
  first := true;
  While i < sl.count Do Begin
    If pos(';', sl[i]) <> 0 Then Begin
      s := sl[i];
      If pos('"', s) = 1 Then Begin
        delete(s, 1, 1);
        delete(s, length(s), 1);
      End;
      sa := s.Split(';');
      s := sa[3];
      If pos('"', s) = 1 Then Begin
        delete(s, 1, 1);
        delete(s, length(s), 1);
      End;
      sa[3] := s;
      If first Then Begin
        first := false;
        d := ScanDateTime('DD.MM.YYYY HH:NN:SS', sa[3]);
        FromDate := d;
        ToDate := d;
      End
      Else Begin
        d := ScanDateTime('DD.MM.YYYY HH:NN:SS', sa[3]);
        FromDate := min(FromDate, d);
        ToDate := max(ToDate, d);
      End;
      StringGrid1.RowCount := StringGrid1.RowCount + 1;
      setlength(GraphInfo, high(GraphInfo) + 2);
      GraphInfo[high(GraphInfo)].hash := sa[0];
      If trim(sa[1]) <> '' Then Begin
        sa2 := trim(sa[1]).Split(' ');
        setlength(GraphInfo[high(GraphInfo)].ParentsHash, length(sa2));
        For j := 0 To high(sa2) Do Begin
          GraphInfo[high(GraphInfo)].ParentsHash[j] := sa2[j];
        End;
      End
      Else Begin
        GraphInfo[high(GraphInfo)].ParentsHash := Nil;
      End;
      StringGrid1.Cells[5, StringGrid1.RowCount - 1] := sa[0]; // Der hash des Commits, so dass wir den nachher auch laden können
      StringGrid1.Cells[3, StringGrid1.RowCount - 1] := sa[2];
      StringGrid1.Cells[4, StringGrid1.RowCount - 1] := sa[3];
      s := sa[4];
      For j := 5 To high(sa) Do Begin
        s := s + ';' + sa[j];
      End;
      StringGrid1.Cells[2, StringGrid1.RowCount - 1] := s;
      inc(i);
      aActions := 0;
      While (i < sl.count) And (pos(';', sl[i]) = 0) Do Begin // Auslesen der Actions
        s := sl[i];
        s := copy(sl[i], 1, pos(#9, sl[i]));
        If pos('M', s) <> 0 Then aActions := aActions Or BitFileModified;
        If pos('A', s) <> 0 Then aActions := aActions Or BitFileAdded;
        If pos('D', s) <> 0 Then aActions := aActions Or BitFileDeleted;
        If pos('R', s) <> 0 Then aActions := aActions Or BitFileRenamed;
        inc(i);
      End;
      StringGrid1.Cells[1, StringGrid1.RowCount - 1] := inttostr(aActions);
    End
    Else Begin // Sollte eigentlich nicht auftreten
      inc(i);
    End;
  End;
  Graph := CalcGraph(GraphInfo);
  CalcAdditionals();
  // TODO: Sammeln der Actions eines Branches damit die auch alle Richtig angezeigt werden können
  DateEdit1.Date := FromDate;
  DateEdit2.Date := ToDate;
  sl.free;
  If FirstLoadLCL Then Begin
    FirstLoadLCL := false;
    StringGrid1.AutoSizeColumns;
  End;
  StringGrid1.ColWidths[2] := min(StringGrid1.ColWidths[2], Form1.Width Div 2);
  StringGrid1.ColWidths[0] := Scale96ToForm(120);
  StringGrid1.ColWidths[1] := max(StringGrid1.ColWidths[1], Scale96ToForm(ImageList1.Width * 5));
  StringGrid1.Selection := rect(0, 1, StringGrid1.ColCount - 1, 1); // Die 1. Zeile Anwählen
  StringGrid1Click(Nil);
End;

End.

