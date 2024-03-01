(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of ImageInspector                                        *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit6;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  Menus, SynEdit, SynHighlighterPas, SynCompletion, uinterpreter;

Type

  { TForm6 }

  TForm6 = Class(TForm)
    ImageList1: TImageList;
    ListBox1: TListBox;
    Memo1: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    PopupMenu1: TPopupMenu;
    SynCompletion1: TSynCompletion;
    SynEdit1: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure ListBox1Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure SynEdit1Change(Sender: TObject);
    Procedure ToolButton1Click(Sender: TObject);
    Procedure ToolButton2Click(Sender: TObject);
    Procedure ToolButton3Click(Sender: TObject);
    Procedure ToolButton5Click(Sender: TObject);
    Procedure ToolButton7Click(Sender: TObject);
    Procedure ToolButton8Click(Sender: TObject);
  private
    fPath: String;
    fChanged: Boolean;
    OldIndex: integer;

    Procedure LoadFirstOrEmpty();
    Function AskChange(): Boolean; // True, wenn Wechsel erlaubt, sonst false
  public
    interpreter: Tinterpreter;
    InterpreterValid: Boolean;
    Procedure Init(Path: String);
  End;

Var
  Form6: TForm6;

Implementation

{$R *.lfm}

Uses FileUtil, LazFileUtils, unit1, LCLIntf, LCLType, unit7;

{ TForm6 }

Procedure TForm6.FormCreate(Sender: TObject);
Begin
  Caption := 'Script editor';
  memo1.Align := alBottom;
  ListBox1.Align := alRight;
  SynEdit1.Align := alClient;
  interpreter := form1.NewInterpreter();
  InterpreterValid := false;
  memo1.clear;
End;

Procedure TForm6.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  If form7.Visible Then Form7.Hide;
End;

Procedure TForm6.FormDestroy(Sender: TObject);
Begin
  interpreter.free;
  interpreter := Nil;
End;

Procedure TForm6.ListBox1Click(Sender: TObject);
Var
  f: String;
  sl: TStringList;
Begin
  If ListBox1.ItemIndex = -1 Then exit;
  If Not AskChange() Then Begin
    ListBox1.ItemIndex := OldIndex;
    exit;
  End;

  f := fPath + ListBox1.Items[ListBox1.ItemIndex] + '.script';
  sl := TStringList.Create;
  sl.LoadFromFile(f);
  SynEdit1.Text := sl.Text;
  sl.free;
  fChanged := false;
  OldIndex := ListBox1.ItemIndex;
End;

Procedure TForm6.MenuItem2Click(Sender: TObject);
Begin
  // Code Format
  showmessage('Todo.');
End;

Procedure TForm6.SynEdit1Change(Sender: TObject);
Begin
  fChanged := true;
End;

Procedure TForm6.ToolButton1Click(Sender: TObject);
Var
  s: String;
  i: Integer;
Begin
  // New
  If Not AskChange() Then exit;
  s := trim(InputBox('Question', 'Enter name:', ''));
  If s = '' Then Begin
    showmessage('Error you did not enter a name.');
    exit;
  End;
  For i := 0 To ListBox1.Items.Count - 1 Do Begin
    If lowercase(s) = lowercase(ListBox1.Items[i]) Then Begin
      showmessage('Error, a script named "' + s + '" already exists.');
      exit;
    End;
  End;
  ListBox1.Items.Add(s);
  ListBox1.ItemIndex := ListBox1.Items.Count - 1;
  SynEdit1.Text :=
    '(*' + LineEnding +
    ' * Put a description comment here.' + LineEnding +
    ' *)' + LineEnding +
    'Procedure OnRender();' + LineEnding +
    'Begin' + LineEnding +
    '  (*' + LineEnding +
    '   * put your code here.' + LineEnding +
    '   *)' + LineEnding +
    'End;';
  SynEdit1.ReadOnly := false;
  Memo1.Clear;
  fChanged := true;
  ToolButton2Click(Nil); // Save
End;

Procedure TForm6.ToolButton2Click(Sender: TObject);
Var
  sl: TStringList;
  f: String;
Begin
  // Save
  If fChanged Then Begin
    sl := TStringList.Create;
    sl.Text := SynEdit1.Text;
    f := fPath + ListBox1.Items[ListBox1.ItemIndex] + '.script';
    sl.SaveToFile(f);
    sl.free;
    fChanged := false;
  End;
End;

Procedure TForm6.ToolButton3Click(Sender: TObject);
Var
  f: String;
Begin
  // Delete
  If ListBox1.ItemIndex = -1 Then exit;
  If id_yes = Application.MessageBox(pchar('Are you shure you want to delete the script: ' + ListBox1.Items[ListBox1.ItemIndex]), 'Question', MB_YESNO Or MB_ICONQUESTION) Then Begin
    f := fPath + ListBox1.Items[ListBox1.ItemIndex] + '.script';
    If Not DeleteFileUTF8(f) Then Begin
      showmessage('Error, could not delete: ' + f);
    End
    Else Begin
      ListBox1.Items.Delete(ListBox1.ItemIndex);
      LoadFirstOrEmpty();
    End;
  End;
End;

Procedure TForm6.ToolButton5Click(Sender: TObject);
Begin
  // Compile
  ToolButton2Click(Nil); // Save
  InterpreterValid := false;
  If interpreter.Compile(SynEdit1.Text) Then Begin
    InterpreterValid := true;
    memo1.text := 'Well done.';
    // Eigentlich sollte das keine Fehler mehr werfen, aber der Compiler erkennt eine Sitation wie "10.0 div 2" nicht, da er kein echtes Typsystem hat.
    Try
      interpreter.CallFunction('OnRender', []);
    Except
      On av: Exception Do Begin
        memo1.Text := av.Message;
        InterpreterValid := false;
      End;
    End;
    Form1.PaintBox1.Invalidate;
  End
  Else Begin
    memo1.Text := interpreter.Errormessage;
  End;
End;

Procedure TForm6.ToolButton7Click(Sender: TObject);
Begin
  // Help
  form7.Show;
End;

Procedure TForm6.ToolButton8Click(Sender: TObject);
Begin
  If AskChange() Then close;
End;

Procedure TForm6.LoadFirstOrEmpty();
Begin
  If ListBox1.Items.Count > 0 Then Begin
    ListBox1.ItemIndex := 0;
    ListBox1Click(Nil);
  End
  Else Begin
    SynEdit1.text := 'Create a new script';
    SynEdit1.ReadOnly := true;
  End;
End;

Function TForm6.AskChange(): Boolean;
Begin
  If fChanged Then Begin
    If ID_YES = application.MessageBox('You did some changes, they get lost if you switch to a other script, change now?', 'Warning', MB_YESNO Or MB_ICONQUESTION) Then Begin
      result := true;
    End
    Else Begin
      result := false;
    End;
  End
  Else Begin
    result := true;
  End;
End;

Procedure TForm6.Init(Path: String);
Var
  sl: TStringList;
  i: Integer;
Begin
  ListBox1.Clear;
  fPath := IncludeTrailingPathDelimiter(Path);
  sl := FindAllFiles(fpath, '*.script', false);
  For i := 0 To sl.count - 1 Do Begin
    ListBox1.Items.add(ExtractFileNameOnly(sl[i]));
  End;
  sl.free;
  LoadFirstOrEmpty;
End;

End.

