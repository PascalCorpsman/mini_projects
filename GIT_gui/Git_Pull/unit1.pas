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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

Const
  (*
   * History: 0.01 = Initialversion (28.11.2023)
   *)
  DefCaption = ' - Pull - CorpsmanGit ver. 0.01';

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure RadioButton1Click(Sender: TObject);
  private
    ProjectRoot: String;
  public

    Procedure LoadLCL;

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses uGITOptions, ugit_common, LazFileUtils, LazUTF8, ugitprogress;

{ TForm1 }

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Var
  params: Array Of String;
  s: String;
  i: Integer;
Begin
  // OK
  GitProgress.ProgressBar1.Position := 0;
  GitProgress.ProgressBar1.Max := 100;
  GitProgress.Memo1.Clear;
  GitProgress.ModalResult := mrNone;
  GitProgress.SetContinueButtonTo('Pulled Diff');
  GitProgress.Show;
  Application.ProcessMessages;
  params := Nil;
  setlength(params, 5);
  params[0] := 'pull';
  params[1] := '--progress';
  params[2] := '-v';
  params[3] := '--no-rebase';
  params[4] := ComboBox1.Text;
  // Preview des Commands
  s := 'git';
  For i := 0 To high(params) Do Begin
    s := s + ' ' + params[i];
  End;
  GitProgress.Memo1.Append(s);
  If GitProgress.RunVisualCommand(ProjectRoot, 'git', params) Then Begin
    Close;
  End
  Else Begin
  End;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Begin
  // Options
  GitOptions.LoadOptions(ProjectRoot);
  GitOptions.Showmodal;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Var
  aDir: String;
  i: Integer;
Begin
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
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

Procedure TForm1.RadioButton1Click(Sender: TObject);
Begin
  ComboBox1.Enabled := RadioButton1.Checked;
  ComboBox2.Enabled := RadioButton2.Checked;
  CheckBox6.Enabled := RadioButton1.Checked;
End;

Procedure TForm1.LoadLCL;
Var
  bi: TBranchInfo;
  i: Integer;
Begin
  caption := ProjectRoot + DefCaption;
  bi := GetBrancheInfo(ProjectRoot);

  ComboBox1.Clear;
  For i := 0 To high(bi.Destinations) Do Begin
    ComboBox1.items.add(bi.Destinations[i]);
  End;
  ComboBox1.Text := bi.Destinations[0];

  ComboBox2.Clear;
  ComboBox2.Text := '';

  ComboBox3.Clear;
  ComboBox3.Text := '';
  For i := 0 To high(bi.Remotes) Do Begin
    ComboBox3.Items.Add(bi.Remotes[i]);
  End;
  If pos(bi.ActualLocal, ComboBox3.Items.Text) <> 0 Then Begin
    ComboBox3.Text := bi.ActualLocal;
  End;

  RadioButton1Click(Nil);
End;

End.

