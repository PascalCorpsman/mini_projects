Unit Unit2;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    Memo1: TMemo;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    Procedure Button3Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure RadioButton4Click(Sender: TObject);
  private
    ProjectRoot: String;

  public
    Procedure Init(aProjectRoot, CommitHash, aMode: String);

  End;

Var
  Form2: TForm2;

Implementation

{$R *.lfm}

Uses ugit_common;

Const
  DefCaption = ' - Create %s - CorpsmanGit';

  { TForm2 }

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
End;

Procedure TForm2.Button3Click(Sender: TObject);
Var
  res: TStringList;
Begin
  If Label1.caption = 'Branch' Then Begin
    If Not IsValidBranchName(edit1.text) Then Begin
      showmessage('Error branch name must not be empty or is invalid.');
      exit;
    End;
    // Create a Branch

    If RadioButton4.Checked Then Begin
      //git branch <branch-name> <commit-hash>
      res := RunCommand(ProjectRoot, 'git', ['branch', edit1.text, ComboBox1.Text]);
      If trim(res.text) <> '' Then Begin
        ShowMessage(res.text);
      End
      Else Begin
        res.free;
        ModalResult := mrOK;
      End;
    End;

    If ModalResult = mrOK Then Begin // Bei Erfolg wechseln wir gleich in den neuen Branch
      If CheckBox3.Checked Then Begin
        res := RunCommand(ProjectRoot, 'git', ['checkout', edit1.text]);
        showmessage(res.text);
        res.free;
      End;
    End;
  End
  Else Begin
    // Create a Tag
    If (trim(edit1.text) = '') Or (Pos('"', Edit1.Text) <> 0) Or (Pos(' ', Edit1.Text) <> 0) Then Begin
      showmessage('Error tag name mus not be empty or is invalid.');
      exit;
    End;
    If RadioButton4.Checked Then Begin
      //git branch <branch-name> <commit-hash>
      // TODO: Wenn die memo einen Inhalt hat, dann: git tag -a <tag-name> -m "Tag message" <commit-hash>
      res := RunCommand(ProjectRoot, 'git', ['tag', '"' + edit1.text + '"', ComboBox1.Text]);
      If trim(res.text) <> '' Then Begin
        ShowMessage(res.text);
      End
      Else Begin
        res.free;
        ModalResult := mrOK;
      End;
    End;
  End;
End;

Procedure TForm2.RadioButton4Click(Sender: TObject);
Begin
  ComboBox3.Enabled := RadioButton2.Checked;
  button2.Enabled := RadioButton2.Checked;

  ComboBox2.Enabled := RadioButton3.Checked;

  ComboBox1.Enabled := RadioButton4.Checked;
  button1.Enabled := RadioButton4.Checked;
End;

Procedure TForm2.Init(aProjectRoot, CommitHash, aMode: String);
Begin
  ProjectRoot := aProjectRoot;
  caption := ProjectRoot + format(DefCaption, [amode]);
  Edit1.text := '';
  Label1.caption := aMode;
  RadioButton4.Checked := true;
  RadioButton4Click(Nil);
  ComboBox1.Text := CommitHash;
  ModalResult := mrNone;
  Memo1.Clear;
End;

End.

