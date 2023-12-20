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
    Procedure Init(aProjectRoot, CommitHash: String);

  End;

Var
  Form2: TForm2;

Implementation

{$R *.lfm}

Const
  DefCaption = ' - Create Branch - CorpsmanGit';

  { TForm2 }

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
End;

Procedure TForm2.Button3Click(Sender: TObject);
Begin

  //git branch <branch-name> <commit-hash>
  //
  //Replace <branch-name> with the desired name for your new branch and <commit-hash> with the hash of the commit where you want the branch to start.
  //
  //For example, if you want to create a branch named "new-feature" at the commit with hash "abc123":
  //
  //bash
  //
  //git branch new-feature abc123
  //
  //After creating the branch, you can switch to it using:
  //
  //bash
  //
  //git checkout new-feature

  ModalResult := mrOK;
End;

Procedure TForm2.RadioButton4Click(Sender: TObject);
Begin
  ComboBox3.Enabled := RadioButton2.Checked;
  button2.Enabled := RadioButton2.Checked;

  ComboBox2.Enabled := RadioButton3.Checked;

  ComboBox1.Enabled := RadioButton4.Checked;
  button1.Enabled := RadioButton4.Checked;
End;

Procedure TForm2.Init(aProjectRoot, CommitHash: String);
Begin
  ProjectRoot := aProjectRoot;
  caption := ProjectRoot + DefCaption;
  Edit1.text := '';
  RadioButton4.Checked := true;
  RadioButton4Click(Nil);
  ComboBox1.Text := CommitHash;
  ModalResult := mrNone;
  Memo1.Clear;
End;

End.

