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
  FileCtrl, Buttons, Grids, ExtCtrls, Menus, Types;

(*
 * Docu: https://tortoisegit.org/docs/tortoisegit/tgit-dug-showlog.html
 *)

 (*
  * History: 0.01 = Initialversion (26.11.2023)
  *
  * Icons geladen von: https://peacocksoftware.com/silk
  *)

Const
  DefCaption = ' - Log Messages - CorpsmanGit ver. 0.01';

  IndexActionFileModified = 0; // If a revision modified a file or directory, the modified icon is shown in the first column.
  IndexActionFileAdded = 1; // If a revision added a File Or directory, the added icon Is shown In the second column.
  IndexActionFileDeleted = 2; // If a revision deleted a file or directory, the deleted icon is shown in the third column.
  IndexActionFileRenamed = 3; // If a revision replaced(rename) a file, the replaced icon is shown in the fourth column.

  BitFileModified = 1;
  BitFileAdded = 2;
  BitFileDeleted = 4;
  BitFileRenamed = 8;

  GrapColors: Array[0..7] Of TColor = (clBlack, clRed, clLime, clBlue, $00808080, $00008080, $00808000, $00808000);

Type
  (*
   * Der RevisionGraph besteht aus vielen "TeilSegmenten"
   * Knifflig sind nur dir Bögen
   *
   *        |
   *    LU /|\ RU
   *    ---------
   *       \|/
   *    UL  |  UR
   *)
  TFieldElement = (
    feHalfVLineUp, feHalfVLineDown,
    feHalfLeftHLine, feHalfRightHLine,
    feCircle, feRectangle,
    feArcLU, feArcUL,
    feArcRU, feArcUR
    );

  TFieldElements = Set Of TFieldElement;

  TField = Record
    Elements: TFieldElements;
    PrimColor: TColor; // Farbe aller Linien, Kreise, Rechtecke und des Bogenstücks welcher die Innere Senkrechte berührt
    SecColor: TColor; // Die 2. Farbe des Bogenstücks
  End;

  TFieldRow = Array Of TField;

  TGraph = Array Of TFieldRow;

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
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    Panel1: TPanel;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    Separator4: TMenuItem;
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
    Procedure StringGrid1Click(Sender: TObject);
    Procedure StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    Procedure StringGrid2DblClick(Sender: TObject);
  private
    ProjectRoot: String;
    Graph: TGraph;
  public
    Procedure LoadLCL;

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses uGITOptions, ugit_common, LazFileUtils, LazUTF8, math, DateUtils, LCLIntf;

Function InterpolateColor(scale: Single; ca, cb: TCOlor): Tcolor;
Var
  r, g, b: integer;
  ra, ga, ba, rb, gb, bb: uint8;
Begin
  ra := ca And $FF;
  ga := (ca And $FF00) Shr 8;
  ba := (ca And $FF0000) Shr 16;
  rb := cb And $FF;
  gb := (cb And $FF00) Shr 8;
  bb := (cb And $FF0000) Shr 16;
  r := min(255, max(0, round((1 - scale) * ra + scale * rb)));
  g := min(255, max(0, round((1 - scale) * ga + scale * gb)));
  b := min(255, max(0, round((1 - scale) * ba + scale * bb)));
  result := RGBToColor(r, g, b);
End;

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
  LoadLCL;
End;

Procedure TForm1.FormShow(Sender: TObject);
Begin
  StringGrid1.SetFocus;
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
  w, wh, wf: Integer;

  Procedure DrawElement(Const aElement: TField; r: TRect);
  Const
    Steps = 10;
  Var
    s, c: Double;
    ox, oy, x, y, i: Integer;
  Begin
    StringGrid1.Canvas.Brush.Color := aElement.PrimColor;
    StringGrid1.Canvas.Pen.Color := aElement.PrimColor;

    If feHalfVLineUp In aElement.Elements Then Begin
      StringGrid1.Canvas.Line(r.Left + wh, r.Top + wh, r.Left + wh, r.Top - 1);
    End;

    If feHalfVLineDown In aElement.Elements Then Begin
      StringGrid1.Canvas.Line(r.Left + wh, r.Top + wh, r.Left + wh, r.Bottom);
    End;

    If feHalfLeftHLine In aElement.Elements Then Begin
      StringGrid1.Canvas.Line(r.Left + wh, r.Top + wh, r.Left, r.Top + wh);
    End;
    If feHalfRightHLine In aElement.Elements Then Begin
      StringGrid1.Canvas.Line(r.Left + wh, r.Top + wh, r.Right, r.Top + wh);
    End;

    (*
     * Die Bögen sind doof, eigentlich sollte man das Pixelbasiert machen
     * aber Pixelweiser zugriff ist Langsamer als Linien malen
     *)
    If feArcUL In aElement.Elements Then Begin
      i := 0;
      sincos(pi * i / (2 * Steps), s, c);
      ox := r.Left + round(c * wh);
      oy := r.Bottom - round(s * wh);
      For i := 1 To steps Do Begin
        StringGrid1.Canvas.Pen.Color := InterpolateColor(i / steps, aElement.PrimColor, aElement.SecColor);
        sincos(pi * i / (2 * Steps), s, c);
        x := r.Left + round(c * wh);
        y := r.Bottom - round(s * wh);
        StringGrid1.Canvas.Line(ox, oy, x, y);
        ox := x;
        oy := y;
      End;
      StringGrid1.Canvas.Line(ox, oy, x - 1, y);
    End;

    If feArcLU In aElement.Elements Then Begin
      i := 0;
      sincos(pi * i / (2 * Steps), s, c);
      ox := r.Left + round(c * wh);
      oy := r.Top + round(s * wh);
      For i := 1 To steps Do Begin
        StringGrid1.Canvas.Pen.Color := InterpolateColor(i / steps, aElement.PrimColor, aElement.SecColor);
        sincos(pi * i / (2 * Steps), s, c);
        x := r.Left + round(c * wh);
        y := r.Top + round(s * wh);
        StringGrid1.Canvas.Line(ox, oy, x, y);
        ox := x;
        oy := y;
      End;
      StringGrid1.Canvas.Line(ox, oy, x - 1, y);
    End;


    If feArcRU In aElement.Elements Then Begin
      i := 0;
      sincos(pi * i / (2 * Steps), s, c);
      ox := r.Right - round(c * wh);
      oy := r.Top + round(s * wh);
      For i := 1 To steps Do Begin
        StringGrid1.Canvas.Pen.Color := InterpolateColor(i / steps, aElement.PrimColor, aElement.SecColor);
        sincos(pi * i / (2 * Steps), s, c);
        x := r.Right - round(c * wh);
        y := r.Top + round(s * wh);
        StringGrid1.Canvas.Line(ox, oy, x, y);
        ox := x;
        oy := y;
      End;
      StringGrid1.Canvas.Line(ox, oy, x - 1, y);
    End;

    If feArcUR In aElement.Elements Then Begin
      i := 0;
      sincos(pi * i / (2 * Steps), s, c);
      ox := r.Right - round(c * wh);
      oy := r.Bottom - round(s * wh);
      For i := 1 To steps Do Begin
        StringGrid1.Canvas.Pen.Color := InterpolateColor(i / steps, aElement.PrimColor, aElement.SecColor);
        sincos(pi * i / (2 * Steps), s, c);
        x := r.Right - round(c * wh);
        y := r.Bottom - round(s * wh);
        StringGrid1.Canvas.Line(ox, oy, x, y);
        ox := x;
        oy := y;
      End;
      StringGrid1.Canvas.Line(ox, oy, x - 1, y);
    End;

    If feCircle In aElement.Elements Then Begin
      StringGrid1.Canvas.Ellipse(r.Left + wf, r.Top + wf, r.Right - wf, r.Bottom - wf);
    End;
    If feRectangle In aElement.Elements Then Begin
      StringGrid1.Canvas.Rectangle(r.Left + wf, r.Top + wf, r.Right - wf, r.Bottom - wf);
    End;
  End;

Var
  i: integer;
  aValue: Integer;
  r: TRect;
  h: integer;
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
    w := aRect.Bottom - aRect.Top;
    wh := w Div 2;
    wf := w Div 4;
    r.Left := aRect.Left;
    r.Top := aRect.Top;
    r.Right := aRect.Left + w;
    r.Bottom := aRect.Bottom;
    For i := 0 To high(Graph[aRow - 1]) Do Begin
      drawElement(Graph[aRow - 1, i], r);
      r.Left := r.Left + w;
      r.Right := r.Right + w;
    End;
  End;
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

Type
  TGraphInfo = Record
    hash, ParentHash: String;
  End;

Var
  sl: TStringList;
  aParams: Array Of String;
  GraphInfo: Array Of TGraphInfo;
  sa: TStringArray;
  aActions, i, j: Integer;
  s: String;
  d, FromDate, ToDate: TDateTime;
  first: Boolean;
Begin
  (*
   * Das hier geht eigentlich 1a, es fehlt nur noch der Git Graph :/
   *)

  //  Hier die Parents raus werfen und dann
  //  in einem 2. Pass machen wie hier beschrieben: https://stackoverflow.com/questions/4739683/how-does-git-log-graph-or-hg-graphlog-work

  //  git rev-list head
  //  git log --graph --pretty=format:"%H"

  caption := ProjectRoot + DefCaption;
  Graph := Nil;
  StringGrid1.RowCount := 2;
  StringGrid1.Cells[2, 1] := 'Working tree changes';
  StringGrid1.Cells[5, 1] := '0000000000000000000000000000000000000000';
  aParams := Nil;
  setlength(aParams, 5);
  aParams[0] := '--no-pager';
  aParams[1] := 'log';
  aParams[2] := '--date=format:"%d.%m.%Y %H:%M:%S"';
  aParams[3] := '--pretty=format:"%H;%P;%an;%ad;%s"';
  aParams[4] := '--name-status';
  (*
  aParams[0] := '--no-pager';
  aParams[1] := 'log';
  aParams[2] := '--graph';
  aParams[3] := '--date=format:"%d.%m.%Y %H:%M:%S"';
  aParams[4] := '--pretty=format:"%H;%P;%an;%ad;%s"';
  aParams[5] := '--name-status';
  *)
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
      GraphInfo[high(GraphInfo)].ParentHash := sa[1];
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
  DateEdit1.Date := FromDate;
  DateEdit2.Date := ToDate;
  sl.free;
  StringGrid1.AutoSizeColumns;
  StringGrid1.ColWidths[0] := Scale96ToForm(120);
  StringGrid1.ColWidths[1] := max(StringGrid1.ColWidths[1], Scale96ToForm(ImageList1.Width * 5));
  StringGrid1.Selection := rect(0, 1, StringGrid1.ColCount - 1, 1); // Die 1. Zeile Anwählen
  StringGrid1Click(Nil);
End;

End.

