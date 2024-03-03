(******************************************************************************)
(* KLab                                                            ??.??.???? *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : AtMega developer IDE.                                        *)
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
(* Known Issues: Bekannte Bugs: Verändert man die Quellcode Dateien, während  *)
(*               Klab geöffnet ist und compiliert dann werden die Änderungen  *)
(*               nicht übernommen (sondern von der gepufferten Version        *)
(*               überschrieben)                                               *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*               0.02 = Kleinere Schönheitsfehler Korrigiert                  *)
(*               0.03 = Den Fuse Bit Editor erweitert um die Handeingabe.     *)
(*               0.04 = Bugfix, wenn Projektname = Name einer Unit,           *)
(*                       funktionierte der Projekt Manager nicht mehr.        *)
(*               0.05 = Einführen FileMonitoring (erkennen wenn Datei von     *)
(*                       Externem Programm verändert wurde)                   *)
(*               0.06 = Umschreiben TProcess, so dass es unter Windoof auch   *)
(*                       geht.                                                *)
(*                      Verbesserungen bei der Auswertung eines Erstellten    *)
(*                       Projekts (wenn .hex File nicht erzeugt wurde, kamen  *)
(*                       komische Fehler bezüglich der Speicherauswertung)    *)
(*               0.07 = Anpassen an neue LCL units                            *)
(*                      Automatischs Scrollen in Terminal Empfangsmemo        *)
(*               0.08 = entfernen ChangeMonitor Classe AV's ohne ende unter   *)
(*                       Linux64                                              *)
(*                      fix Nicht initialisiert Bug bei "CreateCompileCommand"*)
(*               0.09 = der Besen hatte die klablog.txt nicht gelöscht        *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, SynEdit, SynHighlighterCpp, Forms, Controls,
  Graphics, Dialogs, Menus, ComCtrls, StdCtrls, ExtCtrls, lcltype, uklab, math,
  clipbrd, Buttons, uuart_deprecated, ulazcomment, SynEditHighlighter
{$IFDEF windows}
  , synaser
{$ENDIF}
  ;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    ComboBox5: TComboBox;
    ComboBox6: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    ImageList1: TImageList;
    ImageList2: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    ListBox1: TListBox;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    Memo2: TMemo;
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
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    PopupMenu3: TPopupMenu;
    PopupMenu4: TPopupMenu;
    PopupMenu5: TPopupMenu;
    PopupMenu6: TPopupMenu;
    RadioGroup1: TRadioGroup;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    SaveDialog3: TSaveDialog;
    SpeedButton2: TSpeedButton;
    SpeedButton1: TSpeedButton;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    Splitter3: TSplitter;
    Splitter4: TSplitter;
    StatusBar1: TStatusBar;
    StatusBar2: TStatusBar;
    SynCppSyn1: TSynCppSyn;
    SynEdit1: TSynEdit;
    SynEdit2: TSynEdit;
    TabControl1: TTabControl;
    TabControl2: TTabControl;
    Timer1: TTimer;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ToolButton2: TToolButton;
    ToolButton20: TToolButton;
    ToolButton21: TToolButton;
    ToolButton22: TToolButton;
    ToolButton23: TToolButton;
    ToolButton24: TToolButton;
    ToolButton25: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    TreeView1: TTreeView;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure CheckBox1Change(Sender: TObject);
    Procedure CheckBox2Change(Sender: TObject);
    Procedure ComboBox2Change(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure GroupBox2Resize(Sender: TObject);
    Procedure GroupBox3Resize(Sender: TObject);
    Procedure ListBox1DblClick(Sender: TObject);
    Procedure MenuItem10Click(Sender: TObject);
    Procedure MenuItem11Click(Sender: TObject);
    Procedure MenuItem13Click(Sender: TObject);
    Procedure MenuItem16Click(Sender: TObject);
    Procedure MenuItem17Click(Sender: TObject);
    Procedure MenuItem18Click(Sender: TObject);
    Procedure MenuItem19Click(Sender: TObject);
    Procedure MenuItem21Click(Sender: TObject);
    Procedure MenuItem25Click(Sender: TObject);
    Procedure MenuItem26Click(Sender: TObject);
    Procedure MenuItem27Click(Sender: TObject);
    Procedure MenuItem29Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure MenuItem31Click(Sender: TObject);
    Procedure MenuItem32Click(Sender: TObject);
    Procedure MenuItem33Click(Sender: TObject);
    Procedure MenuItem34Click(Sender: TObject);
    Procedure MenuItem35Click(Sender: TObject);
    Procedure MenuItem37Click(Sender: TObject);
    Procedure MenuItem38Click(Sender: TObject);
    Procedure MenuItem39Click(Sender: TObject);
    Procedure MenuItem3Click(Sender: TObject);
    Procedure MenuItem40Click(Sender: TObject);
    Procedure MenuItem41Click(Sender: TObject);
    Procedure MenuItem44Click(Sender: TObject);
    Procedure MenuItem5Click(Sender: TObject);
    Procedure MenuItem6Click(Sender: TObject);
    Procedure MenuItem8Click(Sender: TObject);
    Procedure SpeedButton1Click(Sender: TObject);
    Procedure SpeedButton2Click(Sender: TObject);
    Procedure SynEdit1Change(Sender: TObject);
    Procedure SynEdit1KeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState
      );
    Procedure SynEdit1KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure SynEdit1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure SynEdit1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure SynEdit2KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure SynEdit2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure SynEdit2MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure TabControl1Change(Sender: TObject);
    Procedure TabControl2Change(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
    Procedure ToolButton10Click(Sender: TObject);
    Procedure ToolButton11Click(Sender: TObject);
    Procedure ToolButton13Click(Sender: TObject);
    Procedure ToolButton14Click(Sender: TObject);
    Procedure ToolButton16Click(Sender: TObject);
    Procedure ToolButton17Click(Sender: TObject);
    Procedure ToolButton18Click(Sender: TObject);
    Procedure ToolButton19Click(Sender: TObject);
    Procedure ToolButton20Click(Sender: TObject);
    Procedure ToolButton21Click(Sender: TObject);
    Procedure ToolButton22Click(Sender: TObject);
    Procedure ToolButton24Click(Sender: TObject);
    Procedure ToolButton25Click(Sender: TObject);
    Procedure ToolButton2Click(Sender: TObject);
    Procedure ToolButton8Click(Sender: TObject);
    Procedure TreeView1DblClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Procedure FixLCLWidgetProperties;
    Procedure Set_Highlighter;
  End;

Var
  Form1: TForm1;
  CompileError: Boolean;
  uart: TUart;

Implementation

{$R *.lfm}

Uses unit2, unit3, unit4, unit5, unit6, unit8, lazutf8, LazFileUtils;

{ TForm1 }

Procedure TForm1.Set_Highlighter;

  Procedure cp_(Attri_Out: Pointer; Const Attri_in: TSynHighlighterAttributes);
  Begin
    TSynHighlighterAttributes(Attri_out).ForeGround := Attri_in.Foreground;
    TSynHighlighterAttributes(Attri_out).BackGround := Attri_in.BackGround;
    TSynHighlighterAttributes(Attri_out).Style := Attri_in.Style;
  End;

Begin
  cp_(SynCppSyn1.AsmAttri, form8.SynCppSyn1.AsmAttri);
  cp_(SynCppSyn1.CommentAttri, form8.SynCppSyn1.CommentAttri);
  cp_(SynCppSyn1.DirecAttri, form8.SynCppSyn1.DirecAttri);
  cp_(SynCppSyn1.IdentifierAttri, form8.SynCppSyn1.IdentifierAttri);
  cp_(SynCppSyn1.InvalidAttri, form8.SynCppSyn1.InvalidAttri);
  cp_(SynCppSyn1.KeyAttri, form8.SynCppSyn1.KeyAttri);
  cp_(SynCppSyn1.NumberAttri, form8.SynCppSyn1.NumberAttri);
  cp_(SynCppSyn1.SpaceAttri, form8.SynCppSyn1.SpaceAttri);
  cp_(SynCppSyn1.StringAttri, form8.SynCppSyn1.StringAttri);
  cp_(SynCppSyn1.SymbolAttri, form8.SynCppSyn1.SymbolAttri);
  SynEdit1.Color := SynCppSyn1.SpaceAttri.Background;
  SynEdit1.SelectedColor.Foreground := form8.synedit1.SelectedColor.ForeGround;
  SynEdit1.SelectedColor.Background := form8.synedit1.SelectedColor.Background;
  SynEdit1.SelectedColor.Style := form8.synedit1.SelectedColor.Style;
  SynEdit1.RightEdgeColor := form8.SynEdit1.RightEdgeColor;
  SynEdit2.RightEdgeColor := form8.SynEdit1.RightEdgeColor;
  SynEdit2.Color := SynCppSyn1.SpaceAttri.Background;
  SynEdit2.SelectedColor.Foreground := form8.synedit1.SelectedColor.ForeGround;
  SynEdit2.SelectedColor.Background := form8.synedit1.SelectedColor.Background;
  SynEdit2.SelectedColor.Style := form8.synedit1.SelectedColor.Style;
End;

Procedure TForm1.MenuItem2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.MenuItem31Click(Sender: TObject);
Begin
  // war mal sichtbar ist es nun nicht mehr, da CTRL+C nicht als Globaler Shortcut sein darf ..
  If assigned(AktualSynedit) Then Begin
    Clipboard.AsText := copy(AktualSynedit.Text, AktualSynedit.SelStart, AktualSynedit.SelEnd -
      AktualSynedit.SelStart + 1);
  End;
End;

Procedure TForm1.MenuItem32Click(Sender: TObject);
Begin
  If assigned(AktualSynedit) Then Begin
    If length(AktualSynedit.SelText) <> 0 Then Begin
      form5.CheckBox7.Checked := False; // Ganzer Text
      form5.CheckBox8.Checked := True; // Nur Selektierter Text
      form5.CheckBox9.Checked := True; // Von Cursor Pos
      form5.CheckBox10.Checked := false; // Vom Beginn an
      form5.CheckBox5.Checked := true; // Richtung Vorwärts
      form5.CheckBox6.Checked := false; // Richtung Rückwärts
    End
    Else Begin
      form5.CheckBox8.Checked := False; // Nur Selektierter Text
      form5.CheckBox7.Checked := True; // Ganzer Text
    End;
    form5.Show;
  End;
End;

Procedure TForm1.MenuItem33Click(Sender: TObject);
Begin
  If Assigned(AktualSynedit) Then Begin
    form6.Show;
    form6.edit1.setfocus;
  End;
End;

Procedure TForm1.MenuItem34Click(Sender: TObject);
Var
  CursorPos: TPoint;
  TopLine: integer;
  st, se: integer;
  sy: TSynEdit;
Begin
  sy := AktualSynedit;
  If assigned(sy) Then Begin
    CursorPos := sy.CaretXY;
    topline := sy.TopLine;
    st := sy.SelStart;
    se := sy.SelEnd;
    (*
    In Der Hoffnung, das diese if Bedingung nur kommt, wenn der Cursor hinter einer
    Zeile Steht, und nichts selektiert wurde ..
    *)
    If (cursorpos.x > length(sy.LineText)) And (length(sy.SelText) = 0) Then Begin
      st := CursorPosToIndex(sy.Lines, Cursorpos);
      se := st;
    End;
    // Do Comment.
    Comment(sy.Lines, st, se, true);
    sy.CaretXY := CursorPos;
    sy.TopLine := topline;
    sy.OnChange(Nil);
  End;
End;

Procedure TForm1.MenuItem35Click(Sender: TObject);
Var
  i: integer;
Begin
  If AktualSynedit = SynEdit1 Then Begin
    If TabControl1.TabIndex <> -1 Then Begin
      i := FindIndexbyname(TabControl1.Tabs[TabControl1.TabIndex]);
      If i <> -1 Then Begin
        Clipboard.AsText := OpenedFiles[i].Filename;
      End;
    End;
  End;
  If AktualSynedit = SynEdit2 Then Begin
    If TabControl2.TabIndex <> -1 Then Begin
      i := FindIndexbyname(TabControl2.Tabs[TabControl2.TabIndex]);
      If i <> -1 Then Begin
        Clipboard.AsText := OpenedFiles[i].Filename;
      End;
    End;
  End;
End;

Procedure TForm1.MenuItem37Click(Sender: TObject);
Var
  CursorPos: TPoint;
  TopLine: integer;
  st, se: integer;
  sy: TSynEdit;
Begin
  sy := AktualSynedit;
  If assigned(sy) Then Begin
    If (sy.SelStart <> sy.SelEnd) Then Begin
      CursorPos := sy.CaretXY;
      topline := sy.TopLine;
      st := min(sy.SelStart, sy.SelEnd);
      se := max(sy.SelStart, sy.SelEnd);
      sy.Text := copy(sy.text, 1, st - 1) + lowercase(copy(sy.Text, st, se - st)) + copy(sy.Text, se, length(sy.Text));
    End;
    sy.CaretXY := CursorPos;
    sy.TopLine := topline;
    sy.OnChange(Nil);
  End;
End;

Procedure TForm1.MenuItem38Click(Sender: TObject);
Var
  CursorPos: TPoint;
  TopLine: integer;
  st, se: integer;
  sy: TSynEdit;
Begin
  sy := AktualSynedit;
  If assigned(sy) Then Begin
    CursorPos := sy.CaretXY;
    topline := sy.TopLine;
    st := sy.SelStart;
    se := sy.SelEnd;
    (*
    In Der Hoffnung, das diese if Bedingung nur kommt, wenn der Cursor hinter einer
    Zeile Steht, und nichts selektiert wurde ..
    *)
    If (cursorpos.x > length(sy.LineText)) And (length(sy.SelText) = 0) Then Begin
      st := CursorPosToIndex(sy.Lines, Cursorpos);
      se := st;
    End;
    // Do Comment.
    TextBlockMove(sy.Lines, st, se, false);
    sy.CaretXY := CursorPos;
    sy.TopLine := topline;
    sy.OnChange(Nil);
  End;
End;

Procedure TForm1.MenuItem39Click(Sender: TObject);
Var
  CursorPos: TPoint;
  TopLine: integer;
  st, se: integer;
  sy: TSynEdit;
Begin
  sy := AktualSynedit;
  If assigned(sy) Then Begin
    CursorPos := sy.CaretXY;
    topline := sy.TopLine;
    st := sy.SelStart;
    se := sy.SelEnd;
    (*
    In Der Hoffnung, das diese if Bedingung nur kommt, wenn der Cursor hinter einer
    Zeile Steht, und nichts selektiert wurde ..
    *)
    If (cursorpos.x > length(sy.LineText)) And (length(sy.SelText) = 0) Then Begin
      st := CursorPosToIndex(sy.Lines, Cursorpos);
      se := st;
    End;
    // Do Comment.
    TextBlockMove(sy.Lines, st, se, true);
    sy.CaretXY := CursorPos;
    sy.TopLine := topline;
    sy.OnChange(Nil);
  End;
End;

Procedure TForm1.MenuItem3Click(Sender: TObject);
Var
  i: integer;
Begin
  If TabControl1.tabs.Count <= 1 Then Begin
    showmessage('Error, you have to open at least 2 Files to use this functionality.');
  End
  Else Begin
    TabControl2.Visible := true;
    splitter3.Visible := true;
    If TabControl1.tabIndex <> -1 Then Begin
      i := FindIndexbyname(TabControl1.Tabs[TabControl1.tabIndex]);
      OpenSourceFile(OpenedFiles[i].Filename, 1);
    End;
  End;
End;

Procedure TForm1.MenuItem40Click(Sender: TObject);
Var
  CursorPos: TPoint;
  TopLine: integer;
  st, se: integer;
  sy: TSynEdit;
Begin
  sy := AktualSynedit;
  If assigned(sy) Then Begin
    If (sy.SelStart <> sy.SelEnd) Then Begin
      CursorPos := sy.CaretXY;
      topline := sy.TopLine;
      st := min(sy.SelStart, sy.SelEnd);
      se := max(sy.SelStart, sy.SelEnd);
      sy.Text := copy(sy.text, 1, st - 1) + uppercase(copy(sy.Text, st, se - st)) + copy(sy.Text, se, length(sy.Text));
    End;
    sy.CaretXY := CursorPos;
    sy.TopLine := topline;
    sy.OnChange(Nil);
  End;
End;

Procedure TForm1.MenuItem41Click(Sender: TObject);
Begin
  If SaveDialog3.Execute Then Begin
    memo2.Lines.SaveToFile(SaveDialog3.FileName);
  End;
End;

Procedure TForm1.MenuItem44Click(Sender: TObject);
Begin
  Clipboard.AsText := memo2.Text;
End;

Procedure TForm1.MenuItem21Click(Sender: TObject);
Var
  j: integer;
Begin
  If TabControl2.tabIndex <> -1 Then Begin
    j := FindIndexbyname(TabControl2.Tabs[TabControl2.tabIndex]);
    OpenSourceFile(OpenedFiles[j].Filename, 0);
  End;
End;

Procedure TForm1.MenuItem25Click(Sender: TObject);
Begin
  // Kopiere Aktuelle Zeile in Zwischenspeicher
  Clipboard.AsText := ListBox1.Items[ListBox1.ItemIndex];
End;

Procedure TForm1.MenuItem26Click(Sender: TObject);
Begin
  // Kopiere alles in Zwischenspeicher
  Clipboard.AsText := ListBox1.Items.Text;
End;

Procedure TForm1.MenuItem27Click(Sender: TObject);
Begin
  listbox1.Clear;
End;

Procedure TForm1.MenuItem29Click(Sender: TObject);
Var
  s: String;
  i: Integer;
Begin
  If Not assigned(TreeView1.Selected) Then exit;
  If 'Headers' = TreeView1.Selected.Text Then exit;
  If 'Source' = TreeView1.Selected.Text Then exit;
  If ExtractFileNameonly(AktualProjectFilename) = TreeView1.Selected.Text Then exit;
  s := TreeView1.Selected.Text;
  If TreeView1.Selected.Parent.Text = 'Headers' Then s := s + '.h';
  If TreeView1.Selected.Parent.Text = 'Source' Then s := s + '.c';
  i := FindIndexbyname(s);
  If i = -1 Then Begin
    s := copy(s, 1, length(s) - 1) + uppercase(s[length(s)]);
    i := FindIndexbyname(s);
  End;
  If i = -1 Then Raise Exception.create('Error Could not find "' + s + '" in Project files.');
  If OpenedFiles[i].Changed Then Begin
    If id_no = application.MessageBox(pchar('The File "' + ExtractFileName(OpenedFiles[i].Filename) + '" has been changed, but not saved.'#13#10'If you remove it, the changes will get lost. Remove it anyway ?'), 'Question', MB_YESNO) Then exit;
  End;
  RemoveFileFromProject(OpenedFiles[i].Filename);
End;

Procedure TForm1.MenuItem5Click(Sender: TObject);
Var
  i: Integer;
Begin
  If (AktualProjectFilename <> '') Then Begin
    OpenDialog2.InitialDir := AktualProjectWorkPath;
    If OpenDialog2.Execute Then Begin
      For i := 0 To OpenDialog2.Files.Count - 1 Do
        OpenSourceFile(systoutf8(OpenDialog2.Files[i]), 0);
    End;
  End
  Else Begin
    Showmessage('Error you first need to create a project.');
  End;
End;

Procedure TForm1.MenuItem6Click(Sender: TObject);
Begin
  If SaveDialog1.Execute Then Begin
    NewProject(SysToUTF8(SaveDialog1.FileName));
  End;
End;

Procedure TForm1.MenuItem8Click(Sender: TObject);
Var
  i: Integer;
Begin
  If AktualProjectWorkPath <> '' Then
    OpenDialog1.InitialDir := AktualProjectWorkPath;
  If OpenDialog1.Execute Then Begin
    For i := 0 To OpenDialog1.Files.Count - 1 Do Begin
      //    Splitter3.Visible := true;
      //    TabControl2.Visible := true;
      LoadFile(systoutf8(OpenDialog1.Files[i]));
    End;
  End;
End;

Procedure TForm1.SpeedButton1Click(Sender: TObject);
Var
  bits: integer;
  par: Char;
  sf, hf: Boolean;
Begin
  bits := ComboBox6.ItemIndex + 5;
  par := ComboBox5.Text[1];
  sf := false;
  hf := false;
  Case ComboBox3.ItemIndex Of
    1: hf := true;
    2: sf := true;
  End;
  // Verbinden
  If uart.connect(ComboBox1.Text, strtoint(ComboBox2.Text), bits, par, ComboBox4.ItemIndex, sf, hf) Then Begin
    SpeedButton1.Visible := false;
    SpeedButton2.Visible := true;
    Timer1.Enabled := true;
  End;
End;

Procedure TForm1.SpeedButton2Click(Sender: TObject);
Begin
  // Trennen
  Timer1.Enabled := false;
  uart.close;
  SpeedButton1.Visible := true;
  SpeedButton2.Visible := false;
End;

Procedure TForm1.SynEdit1Change(Sender: TObject);
Begin
  Editorchange();
End;

Procedure TForm1.SynEdit1KeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
{$IFDEF Windows}
  If (ssCtrl In shift) And (key = 191) Then
    MenuItem34click(Nil);
{$ELSE}
  If (ssCtrl In shift) And (key = 222) Then
    MenuItem34click(Nil);
{$ENDIF}
  // Kopieren
  If (ssCtrl In shift) And ((key = ord('c')) Or (key = ord('C'))) Then
    MenuItem31click(Nil);
End;

Procedure TForm1.SynEdit1KeyUp(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  StatusBar1.Panels[0].Text := inttostr(SynEdit1.CaretY) + ': ' + inttostr(SynEdit1.Caretx);
End;

Procedure TForm1.SynEdit2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  AktualEditor := 1;
  AktualSynedit := SynEdit2;
End;

Procedure TForm1.SynEdit2MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  StatusBar2.Panels[0].Text := inttostr(SynEdit2.CaretY) + ': ' + inttostr(SynEdit2.Caretx);
End;

Procedure TForm1.SynEdit1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  AktualEditor := 0;
  AktualSynedit := SynEdit1;
End;

Procedure TForm1.SynEdit1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  StatusBar1.Panels[0].Text := inttostr(SynEdit1.CaretY) + ': ' + inttostr(SynEdit1.Caretx);
End;

Procedure TForm1.SynEdit2KeyUp(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  StatusBar2.Panels[0].Text := inttostr(SynEdit2.CaretY) + ': ' + inttostr(SynEdit2.Caretx);
End;

Procedure TForm1.TabControl1Change(Sender: TObject);
Var
  i: Integer;
Begin
  If TabControl1.TabIndex <> -1 Then Begin
    i := FindIndexbyname(TabControl1.Tabs[TabControl1.tabIndex]);
    If i <> -1 Then Begin
      OpenSourceFile(OpenedFiles[i].Filename, 0);
    End;
    StatusBar1.Panels[0].Text := '';
  End;
End;

Procedure TForm1.TabControl2Change(Sender: TObject);
Var
  i: Integer;
Begin
  If TabControl2.tabIndex <> -1 Then Begin
    i := FindIndexbyname(TabControl2.Tabs[TabControl2.tabIndex]);
    If (i <> -1) Then Begin
      OpenSourceFile(OpenedFiles[i].Filename, 1);
    End;
    StatusBar2.Panels[0].Text := '';
  End;
End;

Procedure TForm1.Timer1Timer(Sender: TObject);
Var
  i: integer;
  s: String;
  c: byte;
Begin
  If RadioGroup1.ItemIndex = 0 Then Begin
    s := uart.Read;
    If s <> '' Then Begin
      memo2.Text := memo2.Text + s;
      memo2.SelStart := length(memo2.Text); // Scroll Down
    End;
  End;
  If RadioGroup1.ItemIndex = 1 Then Begin
    s := uart.Read;
    If s <> '' Then Begin
      For i := 1 To length(s) Do Begin
        c := ord(s[i]);
        memo2.Text := memo2.Text + format(' %.2x', [c]);
      End;
      memo2.SelStart := length(memo2.Text); // Scroll Down
    End;
  End;
End;

Procedure TForm1.ToolButton10Click(Sender: TObject);
Var
  in_f, out_f: String;
  i: integer;
Begin
  i := -1;
  MenuItem27Click(Nil);
  // Suchen der Datei
  If AktualEditor = 0 Then Begin
    If TabControl1.TabIndex <> -1 Then
      i := FindIndexbyname(TabControl1.Tabs[TabControl1.TabIndex]);
  End
  Else Begin
    If TabControl2.TabIndex <> -1 Then
      i := FindIndexbyname(TabControl2.Tabs[TabControl2.TabIndex]);
  End;
  If i = -1 Then Begin
    exit; // nicht gefunden Raus
  End;
  If lowercase(ExtractFileExt(OpenedFiles[i].Filename)) = '.c' Then Begin
    // Autosave
    If GlobalChanged Then Begin
      MenuItem11Click(Nil);
    End;
    in_f := OpenedFiles[i].Filename;
    out_f := in_f;
    out_f[length(out_f)] := 'o';
    ExecuteCommand(CreateCompileCommand(in_f, out_f));
  End;
End;

Procedure TForm1.ToolButton11Click(Sender: TObject);
Var
  sl: TStringList;
  s, in_f, out_f: String;
  i: integer;
Begin
  //  Das hier geht wohl nicht mehr ..
  ToolButton11.Enabled := false;
  CompileError := false;
  sl := ListOfAllCFiles(); // Alle C-Files auslesen
  If sl.Count <> 0 Then Begin
    ListBox1.Clear;
    Application.ProcessMessages;
    ToolButton24.Click; // Alles Löschen
    Application.ProcessMessages;
    // Autosave
    If GlobalChanged Then Begin
      MenuItem11Click(Nil);
    End;
    s := '';
    For i := 0 To sl.Count - 1 Do Begin // Compilieren
      in_f := sl[i];
      out_f := in_f;
      out_f[length(out_f)] := 'o';
{$IFDEF Windows}
      If pos(' ', in_f) <> 0 Then Begin
        CompileError := True;
        addmessage(Error_no_Spaces_In_Paths);
        break;
      End;
{$ENDIF}
      s := s + ' ' + out_f; // Sammeln der o-Files für den Linker
      ExecuteCommand(CreateCompileCommand(in_f, out_f));
      If Not FileExistsUTF8(out_f) Then
        CompileError := True;
    End;
    If Not CompileError Then Begin
      ExecuteCommand(CreateLinkerCommand(s)); // Linken
      // Wenn erfolgreich erstellt werden konnte
      If FileExistsUTF8(AktualProjectWorkPath + KlabConfig.OutFile) Then Begin
        If Not Check_File_Sizes(cpu, AktualProjectWorkPath + KlabConfig.OutFile) Then Begin
          CompileError := true;
          addmessage('Error, Project is too big for CPU.');
        End
        Else Begin
          ExecuteCommand(CreateObjectCopyCommand());
          addmessage('Project built successfully.');
        End;
      End
      Else Begin
        CompileError := true;
      End;
    End;
  End
  Else
    CompileError := true;
  sl.free;
  ToolButton11.Enabled := true;
End;

Procedure TForm1.ToolButton13Click(Sender: TObject);
Begin
  // Enlarge Font
  SynEdit1.Font.Size := SynEdit1.Font.Size + 1;
  SynEdit2.Font.Size := SynEdit1.Font.Size;
  KlabConfig.FontSize := SynEdit1.Font.Size;
End;

Procedure TForm1.ToolButton25Click(Sender: TObject);
Begin
  SynEdit1.Font.Size := DefaultEditorFontSize;
  SynEdit2.Font.Size := SynEdit1.Font.Size;
  KlabConfig.FontSize := SynEdit1.Font.Size;
End;

Procedure TForm1.ToolButton14Click(Sender: TObject);
Begin
  // Shrink Font
  SynEdit1.Font.Size := max(SynEdit1.Font.Size - 1, 0);
  SynEdit2.Font.Size := SynEdit1.Font.Size;
  KlabConfig.FontSize := SynEdit1.Font.Size;
End;

Procedure TForm1.ToolButton16Click(Sender: TObject);
Var
  s: String;
Begin
  Form4Mode := f4merase;
  form4.Edit1.Visible := false;
  form4.Label1.Visible := false;
  form4.SpeedButton1.Visible := false;
  s := Form4.Caption;
  Form4.Caption := 'Erase Controller';
  form4.ComboBox1.Items.Text := KlabConfig.Aviable_CPUS.Text;
  form4.ComboBox1.Text := KlabConfig.CPU;
  form4.ComboBox1Change(Nil);
  form4.ShowModal;
  Form4.Caption := s;
  form4.Edit1.Visible := True;
  form4.Label1.Visible := True;
  form4.SpeedButton1.Visible := True;
End;

Procedure TForm1.ToolButton17Click(Sender: TObject);
Var
  s: String;
Begin
  //Upload Project hex
  s := AktualProjectWorkPath + KlabConfig.HexFile;
  ExecuteCommand(CreateProgrammCommand(s, cpu));
End;

Procedure TForm1.ToolButton18Click(Sender: TObject);
Begin
  Form4Mode := f4mupload;
  form4.ComboBox1.Items.Text := KlabConfig.Aviable_CPUS.Text;
  form4.ComboBox1.Text := KlabConfig.CPU;
  form4.ComboBox1Change(Nil);
  form4.CheckBox1.Checked := true;
  form4.ShowModal;
End;

Procedure TForm1.ToolButton19Click(Sender: TObject);
Var
  s: String;
Begin
  s := Form4.Caption;
  Form4.Caption := 'Verify Hexfile';
  Form4Mode := f4mverify;
  form4.ComboBox1.Items.Text := KlabConfig.Aviable_CPUS.Text;
  form4.ComboBox1.Text := KlabConfig.CPU;
  form4.ComboBox1Change(Nil);
  form4.CheckBox1.Checked := true;
  form4.ShowModal;
  Form4.Caption := s;
End;

Procedure TForm1.ToolButton20Click(Sender: TObject);
Var
  s: String;
Begin
  Form4Mode := f4mdownload;
  form4.Edit1.Visible := false;
  form4.Label1.Visible := false;
  form4.SpeedButton1.Visible := false;
  form4.CheckBox1.Visible := false;
  s := Form4.Caption;
  Form4.Caption := 'Download Hexfile';
  form4.ComboBox1.Items.Text := KlabConfig.Aviable_CPUS.Text;
  form4.ComboBox1.Text := KlabConfig.CPU;
  form4.ComboBox1Change(Nil);
  form4.ShowModal;
  Form4.Caption := s;
  form4.Edit1.Visible := True;
  form4.Label1.Visible := True;
  form4.SpeedButton1.Visible := True;
  form4.CheckBox1.Visible := True;
End;

Procedure TForm1.ToolButton21Click(Sender: TObject);
Var
  reconnect: boolean;
Begin
  ToolButton21.Enabled := false;
  reconnect := false;
  If CheckBox2.Checked Then Begin
    memo2.Clear;
    Application.ProcessMessages; // Anzeigen, dass sich was geändert hat.
  End;
  If CheckBox1.Checked And SpeedButton2.Visible Then Begin
    SpeedButton2.OnClick(Nil);
    reconnect := true;
    Application.ProcessMessages; // Anzeigen, dass sich was geändert hat.
  End;
  // Ignite
  ToolButton11Click(Nil); // Build Project
  If Not CompileError Then // Wird von Build Project Initialisiert
    ToolButton17Click(Nil); // Upload File
  If reconnect Then Begin
    SpeedButton1.OnClick(Nil);
  End;
  ToolButton21.Enabled := True;
End;

Procedure TForm1.ToolButton22Click(Sender: TObject);
Begin
  // Fuses
  form3.ComboBox1.Items.text := KlabConfig.Aviable_CPUS.Text;
  form3.loadcpu(KlabConfig.CPU);
  form3.ShowModal;
End;

Procedure TForm1.ToolButton24Click(Sender: TObject);
Var
  i: integer;
  aDirectory: String;
Begin
  form1.ListBox1.Clear;
  aDirectory := IncludeTrailingPathDelimiter(ExtractFilePath(AktualProjectFilename));
  DeleteAllFilesInDir(aDirectory);
  If KlabConfig.usesearchpath Then Begin
    For i := 0 To KlabConfig.searchpaths.Count - 1 Do Begin
      DeleteAllFilesInDir(KlabConfig.searchpaths[i]);
    End;
  End;
  If KlabConfig.uselibsearchpath Then Begin
    For i := 0 To KlabConfig.libsearchpaths.Count - 1 Do Begin
      DeleteAllFilesInDir(KlabConfig.libsearchpaths[i]);
    End;
  End;
  If FileExistsUTF8(ADirectory + KlabConfig.Logfile) Then Begin
    If DeleteFileUTF8(ADirectory + KlabConfig.Logfile) Then Begin
      AddMessage('Deleted : ' + ADirectory + KlabConfig.Logfile);
    End
    Else Begin
      AddMessage('Could not delete : ' + ADirectory + KlabConfig.Logfile);
    End;
  End;
End;

Procedure TForm1.ToolButton2Click(Sender: TObject);
Begin
  CloseSourceFile();
End;

Procedure TForm1.ToolButton8Click(Sender: TObject);
Begin
  form2.Load_all_Konfigs;
  form2.showmodal;
End;

Procedure TForm1.TreeView1DblClick(Sender: TObject);
Var
  s: String;
  i: Integer;
Begin
  If 'Headers' = TreeView1.Selected.Text Then exit;
  If 'Source' = TreeView1.Selected.Text Then exit;
  //  If ExtractFileNameonly(AktualProjectFilename) = TreeView1.Selected.Text Then exit;
  If TreeView1.Selected = TreeView1.Items[0] Then exit;
  s := TreeView1.Selected.Text;
  If TreeView1.Selected.Parent.Text = 'Headers' Then s := s + '.h';
  If TreeView1.Selected.Parent.Text = 'Source' Then s := s + '.c';
  i := FindIndexbyname(s);
  If i = -1 Then Begin
    s := copy(s, 1, length(s) - 1) + uppercase(s[length(s)]);
    i := FindIndexbyname(s);
  End;
  If i = -1 Then Raise Exception.create('Error Could not find "' + s + '" in Project files.');
  OpenSourceFile(OpenedFiles[i].Filename, 0);
End;

Procedure TForm1.FixLCLWidgetProperties;
Var
  i: integer;
Begin
  // Der Versuch evtl Falsch Zusammengesetzte Splitter Kombinationen wieder zu Korrigieren
  If splitter3.Visible Then Begin
    i := tabControl1.Left + tabControl1.Width;
    Splitter3.Left := i;
  End;
  If Splitter2.Visible And Splitter3.Visible Then Begin
    i := tabControl1.Left + tabControl1.Width;
    splitter2.visible := false;
    splitter3.visible := false;
    tabControl2.Visible := false;
    GroupBox2.Visible := false;
    Splitter3.Left := i;
    tabControl2.left := i + Splitter3.Width;
    Splitter2.Left := i + Splitter3.Width + tabControl2.Width;
    GroupBox2.left := i + Splitter3.Width + tabControl2.Width + Splitter2.Width;
    splitter3.visible := true;
    tabControl2.Visible := true;
    splitter2.visible := true;
    GroupBox2.Visible := true;
  End;
End;

Procedure TForm1.FormCreate(Sender: TObject);
{$IFDEF windows}
Var
  s: String;
{$ENDIF}
Begin
  memo1.clear;
  memo2.clear;
  ComboBox1.Items.Clear;
  SpeedButton2.Top := SpeedButton1.top;
  SpeedButton2.Visible := false;
{$IFDEF windows}
  s := GetSerialPortNames();
  s := StringReplace(s, ',', #13#10, [rfReplaceAll, rfIgnoreCase]);
  ComboBox1.Items.Text := s;
{$ELSE}
  ComboBox1.Items.Add('/dev/ttyS0');
  ComboBox1.Items.Add('/dev/ttyS1');
  ComboBox1.Items.Add('/dev/ttyS2');
  ComboBox1.Items.Add('/dev/ttyS3');
  ComboBox1.Items.Add('/dev/ttyUSB0');
  ComboBox1.Items.Add('/dev/ttyUSB1');
{$ENDIF}
  (*
  Bugs, offene Punkte, Features :

  - Lazcomment, funktioniert nicht Richtig, wenn der Cursor hinter dem Zeilenende steht.
  *)
  // Todo :  Auto Code Completion
  // Todo : Code Formating
  caption := defcaption;
  LoadDefaultEditorSettings();
  // Konfiguration Uart
  ComboBox1.text := KlabConfig.Uart_Device;
  ComboBox2.text := inttostr(KlabConfig.Uart_BaudRate);
  ComboBox3.ItemIndex := KlabConfig.Uart_Flow_Control;
  ComboBox4.ItemIndex := KlabConfig.Uart_Stop;
  ComboBox5.ItemIndex := KlabConfig.Uart_Parity;
  ComboBox6.ItemIndex := KlabConfig.Uart_Bits;
  CheckBox1.Checked := KlabConfig.Uart_autoDisconnect;
  // Alles was halt so onCreate Initialisiert werden sollte
  AppPath := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStrUTF8(0)));
  SaveDialog1.InitialDir := AppPath;
  SaveDialog2.InitialDir := AppPath;
  OpenDialog1.InitialDir := AppPath;
  OpenDialog2.InitialDir := AppPath;
  // Einstellen der ganzen Aligns
  Splitter4.Align := alBottom;
  listbox1.Align := alBottom;
  GroupBox1.Align := alLeft;
  Splitter1.Align := alLeft;
  GroupBox2.Align := alRight;
  Splitter2.Align := alRight;
  tabControl2.Align := alright;
  Splitter3.Align := alRight;
  tabControl1.Align := alclient;
  SynEdit1.Align := alclient;
  SynEdit2.Align := alclient;
  // Abschalten Editor 2
  tabControl2.Visible := false;
  splitter3.Visible := false;
  SynEdit1.Visible := false;
  LoadEditorProperties(); // Laden der Editor Einstellungen
  // Gibt es eine Projekt Datei, welche mit übergeben wurde ?
  If FileExistsUTF8(ParamStrUTF8(1)) Then Begin
    LoadFile(ParamStrUTF8(1));
  End
  Else Begin
    // Wenn es ein zuletzt geöffnetes Projekt gab.
    If FileExistsUTF8(AktualProjectFilename) Then Begin
      LoadFile(AktualProjectFilename);
    End;
  End;
  uart := TUart.create;
End;

Procedure TForm1.GroupBox2Resize(Sender: TObject);
Var
  w: integer;
  s: String;
Begin
  w := GroupBox2.Width Div 5;
  ComboBox1.Width := 2 * w - label1.width - 20;
  label2.left := 2 * w + 5;
  ComboBox2.Width := 2 * w - label2.width - 20;
  ComboBox2.left := label2.left + label2.width + 5;
  SpeedButton1.Left := ComboBox2.left + ComboBox2.Width + 5;
  SpeedButton1.Width := w;
  SpeedButton2.Left := ComboBox2.left + ComboBox2.Width + 5;
  SpeedButton2.Width := w;
  s := '-';
  For w := 1 To ((GroupBox2.Width - 20) Div 5) Do
    s := s + '-';
  label3.caption := s;
  label5.caption := s;
End;

Procedure TForm1.GroupBox3Resize(Sender: TObject);
Begin
  ComboBox3.Width := (GroupBox3.Width Div 2) - 20;
  ComboBox4.Width := (GroupBox3.Width Div 2) - 20;
  ComboBox5.Left := (GroupBox3.Width Div 2) + 10;
  ComboBox5.Width := (GroupBox3.Width Div 2) - 20;
  ComboBox6.Left := (GroupBox3.Width Div 2) + 10;
  ComboBox6.Width := (GroupBox3.Width Div 2) - 20;
End;

Procedure TForm1.ListBox1DblClick(Sender: TObject);
Var
  s, a, b, c: String;
  j, i: Integer;
  loaded_: Boolean;
Begin
  If ListBox1.ItemIndex <> -1 Then Begin
    s := ListBox1.items[ListBox1.ItemIndex];
{$IFDEF Windows}
    a := copy(s, 1, 3);
    delete(s, 1, 3);
{$ELSE}
    a := '';
{$ENDIF}
    a := a + copy(s, 1, pos(':', s) - 1);
    delete(s, 1, pos(':', s));
    b := copy(s, 1, pos(':', s) - 1);
    delete(s, 1, length(b) + 1);
    c := copy(s, 1, pos(':', s) - 1);
    delete(s, 1, length(c) + 1);
    // Die Datei gibt es, also versuchen wir an die entsprechende Code Stelle zu springen
    j := FindIndexbyname(a);
    If (j <> -1) Then Begin
      loaded_ := false;
      (* das Suchen in Synedit1 können wir uns sparen, denn irgendwo muss das File ja geöffnet werden..
      // Herausfinden ob die Datei in einem der Beiden TTabControls geladen ist ..
      For i := 0 To TabControl1.Tabs.Count - 1 Do Begin
        // Wir haben die Source File in TabControl 1 gefunden
        If (FindIndexbyname(TabControl1.Tabs[i]) = j) Then Begin
          // Sicherstellen, dass die File auch "OnTop" ist.
          loaded := True;
          OpenSourceFile(OpenedFiles[j].Filename, 0);
          // Anspringen der Zeile
          If strtointdef(b, -1) <> -1 Then Begin
            SynEdit1.TopLine := max(0, strtoint(b) - 10);
            SynEdit1.CaretY := strtoint(b) - 1;
          End;
          If strtointdef(c, -1) <> -1 Then Begin
            SynEdit1.CaretX := strtoint(c) - 1;
          End
          Else Begin
            SynEdit1.CaretX := 0;
          End;
          SynEdit1.SetFocus;
        End;
      End; *)
      For i := 0 To TabControl2.Tabs.Count - 1 Do Begin
        // Wir haben die Source File in TabControl 1 gefunden
        If (FindIndexbyname(TabControl2.Tabs[i]) = j) Then Begin
          // Sicherstellen, dass die File auch "OnTop" ist.
          loaded_ := True;
          OpenSourceFile(OpenedFiles[j].Filename, 1);
          // Anspringen der Zeile
          If strtointdef(b, -1) <> -1 Then Begin
            SynEdit2.TopLine := max(0, strtoint(b) - 10);
            SynEdit2.CaretY := strtoint(b) - 1;
          End;
          If strtointdef(c, -1) <> -1 Then Begin
            SynEdit2.CaretX := strtoint(c) - 1;
          End
          Else Begin
            SynEdit2.CaretX := 0;
          End;
          SynEdit2.SetFocus;
        End;
      End;
      // Wenn die Datei in keinen der beiden Fenster Geladen ist, dann öffnen wir sie im 1.
      If Not (loaded_) Then Begin
        OpenSourceFile(OpenedFiles[j].Filename, 0);
        // Anspringen der Zeile
        If strtointdef(b, -1) <> -1 Then Begin
          SynEdit1.TopLine := max(0, strtoint(b) - 10);
          SynEdit1.CaretY := strtoint(b) - 1;
        End;
        If strtointdef(c, -1) <> -1 Then Begin
          SynEdit1.CaretX := strtoint(c) - 1;
        End
        Else Begin
          SynEdit1.CaretX := 0;
        End;
        SynEdit1.SetFocus;
      End;
    End;
  End;
End;

Procedure TForm1.MenuItem10Click(Sender: TObject);
Var
  b: Boolean;
Begin
  If SaveDialog1.Execute Then Begin
    //    b := Timer1.Enabled;
    //    If b Then Begin
    //      timer1.Enabled := false;
    //    End;
    b := SpeedButton2.Visible;
    If b Then Begin
      SpeedButton2.Click;
    End;
    SaveProjectAs(systoutf8(SaveDialog1.FileName));
    //    If b Then Begin
    //      timer1.Enabled := true;
    //    End;
    If b Then Begin
      SpeedButton1.Click;
    End;
  End;
End;

Procedure TForm1.MenuItem11Click(Sender: TObject);
Var
  b: Boolean;
Begin
  //  b := Timer1.Enabled;
  //  If b Then Begin
  //    timer1.Enabled := false;
  //  End;
  b := SpeedButton2.Visible;
  If b Then Begin
    SpeedButton2.Click; // Disconnect from uart
  End;
  sleep(150);
  Try
    SaveProject();
  Except
    showmessage('Error while save project.');
  End;
  //  If b Then Begin
  //    timer1.Enabled := true;
  //  End;
  If b Then Begin
    SpeedButton1.Click;
  End;
End;

Procedure TForm1.MenuItem13Click(Sender: TObject);
Begin
  EditorReload();
End;

Procedure TForm1.MenuItem16Click(Sender: TObject);
Begin
  If assigned(sender) Then
    MenuItem16.Checked := Not MenuItem16.Checked;
  ListBox1.Visible := MenuItem16.Checked;
  Splitter4.Visible := MenuItem16.Checked;
  form1.FixLCLWidgetProperties;
End;

Procedure TForm1.MenuItem17Click(Sender: TObject);
Begin
  If assigned(sender) Then
    MenuItem17.Checked := Not MenuItem17.Checked;
  GroupBox1.Visible := MenuItem17.Checked;
  Splitter1.Visible := MenuItem17.Checked;
  form1.FixLCLWidgetProperties;
End;

Procedure TForm1.MenuItem18Click(Sender: TObject);
Begin
  If assigned(sender) Then
    MenuItem18.Checked := Not MenuItem18.Checked;
  GroupBox2.Visible := MenuItem18.Checked;
  Splitter2.Visible := MenuItem18.Checked;
  form1.FixLCLWidgetProperties;
End;

Procedure TForm1.MenuItem19Click(Sender: TObject);
  Procedure cp(Von: TSynHighlighterAttributes; nach: Pointer);
  Begin
    TSynHighlighterAttributes(nach).Background := von.Background;
    TSynHighlighterAttributes(nach).Foreground := von.Foreground;
    TSynHighlighterAttributes(nach).Style := Von.style;
  End;
Begin
  // Laden der Aktuellen Settings nach Form8
  cp(SynCppSyn1.asmattri, Form8.SynCppSyn1.asmattri);
  cp(SynCppSyn1.CommentAttri, Form8.SynCppSyn1.CommentAttri);
  cp(SynCppSyn1.DirecAttri, Form8.SynCppSyn1.DirecAttri);
  cp(SynCppSyn1.IdentifierAttri, Form8.SynCppSyn1.IdentifierAttri);
  cp(SynCppSyn1.InvalidAttri, Form8.SynCppSyn1.InvalidAttri);
  cp(SynCppSyn1.KeyAttri, Form8.SynCppSyn1.KeyAttri);
  cp(SynCppSyn1.NumberAttri, Form8.SynCppSyn1.NumberAttri);
  form8.SynEdit1.RightEdgeColor := SynEdit1.RightEdgeColor;
  cp(SynCppSyn1.SpaceAttri, Form8.SynCppSyn1.SpaceAttri);
  cp(SynCppSyn1.StringAttri, Form8.SynCppSyn1.StringAttri);
  cp(SynCppSyn1.SymbolAttri, Form8.SynCppSyn1.SymbolAttri);
  form8.synedit1.Color := synedit1.color;
  form8.SynEdit1.SelectedColor.Foreground := synedit1.SelectedColor.Foreground;
  form8.SynEdit1.SelectedColor.Background := synedit1.SelectedColor.Background;
  form8.SynEdit1.SelectedColor.Style := synedit1.SelectedColor.Style;
  form8.ListBox1Click(Nil);
  form8.showmodal;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  canclose := true;
  If GlobalChanged Then Begin
    If id_no = Application.MessageBox(
      'There are unsaved files, do you want to close anyway ?', 'Info', MB_OK Or MB_YESNO) Then
      canclose := False;
  End;
End;

Procedure TForm1.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Begin
  uart.Free;
  KlabConfig.Aviable_CPUS.free;
  KlabConfig.searchpaths.free;
  KlabConfig.libsearchpaths.free;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  memo2.Clear;
End;

Procedure TForm1.CheckBox1Change(Sender: TObject);
Begin
  KlabConfig.Uart_autoDisconnect := CheckBox1.Checked;
End;

Procedure TForm1.CheckBox2Change(Sender: TObject);
Begin
  KlabConfig.Uart_autoClean := CheckBox2.Checked;
End;

Procedure TForm1.ComboBox2Change(Sender: TObject);
Begin
  KlabConfig.Uart_BaudRate := strtoint(ComboBox2.text);
End;

Function isHexNum(c: Char): Boolean;
Begin
  result := (c In ['0'..'9', 'a'..'f', 'A'..'F']);
End;

Function HextoInt(Value: String): Cardinal;
Var
  m: cardinal;
  c: byte;
Begin
  value := uppercase(value);
  result := 0;
  m := 1;
  While length(value) <> 0 Do Begin
    c := ord(value[length(value)]);
    delete(value, length(value), 1);
    If c In [48..57] Then
      result := result + (c - 48) * m;
    If c In [65..70] Then
      result := result + (c - 55) * m;
    m := m * 16;
  End;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  s: String;
  data: TBytes;
  i, j: Integer;
Begin
  If SpeedButton2.Visible Then Begin
    s := Memo1.Lines.Text;
    setlength(data, 0);
    i := 1;
    While i <= length(s) Do Begin
      setlength(data, high(data) + 2);
      If (s[i] = '#') And (i + 2 <= length(s)) Then Begin
        If (isHexNum(s[i + 1]) And (isHexNum(s[i + 2]))) Then Begin
          j := HextoInt(s[i + 1] + s[i + 2]);
          data[high(data)] := j;
          inc(i);
          inc(i);
        End
        Else Begin
          data[high(data)] := ord(s[i]);
        End;
      End
      Else Begin
        data[high(data)] := ord(s[i]);
      End;
      inc(i);
    End;
    uart.WriteByteArr(data);
    setlength(data, 0);
  End
  Else
    showmessage('You need to connect first.');
End;

End.

