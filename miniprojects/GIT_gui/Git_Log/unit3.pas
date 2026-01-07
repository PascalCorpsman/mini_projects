Unit Unit3;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

Type

  { TForm3 }

  TForm3 = Class(TForm)
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
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    Procedure Button3Click(Sender: TObject);
    Procedure CheckBox1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure RadioButton2Change(Sender: TObject);
  private
    ProjectRoot: String;

  public
    Procedure Init(aProjectRoot, CommitHash, aMode: String);
  End;

Var
  Form3: TForm3;

Implementation

{$R *.lfm}

Uses
  ugitprogress;

{ TForm3 }

Procedure TForm3.FormCreate(Sender: TObject);
Begin
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
End;

Procedure TForm3.RadioButton2Change(Sender: TObject);
Begin
  ComboBox3.Enabled := RadioButton2.Checked;
  ComboBox1.Enabled := RadioButton4.Checked;
End;

Procedure TForm3.CheckBox1Click(Sender: TObject);
Begin
  edit1.visible := CheckBox1.Checked;
End;

Procedure TForm3.Button3Click(Sender: TObject);
Var
  params: Array Of String;
  s: String;
  i: Integer;
Begin
  // OK
  GitProgress.InitProgressbar(100);
  GitProgress.Memo1.Clear;
  GitProgress.ModalResult := mrNone;
  GitProgress.SetContinueButtonTo('Update Submodules');
  GitProgress.Show;
  Application.ProcessMessages;
  params := Nil;
  setlength(params, 2);
  params[0] := 'checkout';
  If CheckBox1.Checked Then Begin
    setlength(params, 4);
    params[1] := '-b';
    params[2] := Edit1.text;
  End;
  If RadioButton2.Checked Then
    params[high(params)] := ComboBox3.Text;
  If RadioButton3.Checked Then
    params[high(params)] := ComboBox2.Text;
  If RadioButton4.Checked Then
    params[high(params)] := ComboBox1.Text;
  // Preview des Commands
  s := 'git';
  For i := 0 To high(params) Do Begin
    s := s + ' ' + params[i];
  End;
  GitProgress.Memo1.Append(s);
  If GitProgress.RunVisualCommand(ProjectRoot, 'git', params) Then Begin
    ModalResult := mrOK;
  End
  Else Begin
  End;
End;

Procedure TForm3.Init(aProjectRoot, CommitHash, aMode: String);
Begin
  caption := aProjectRoot + ' - Switch/Checkout';
  ProjectRoot := aProjectRoot;

  ComboBox1.Text := '';
  ComboBox2.Text := '';
  ComboBox3.Text := '';
  label1.Caption := '';
  Edit1.Visible := false;

  Case aMode Of
    'Branch': Begin
        RadioButton2.Checked := true;
        ComboBox3.Text := CommitHash;
      End;
    'Tag': Begin
        RadioButton3.Checked := true;
        ComboBox2.Text := CommitHash;
      End;
    'Commit': Begin
        RadioButton4.Checked := true;
        label1.Caption := 'Branch_' + copy(CommitHash, 1, 8);
        Edit1.text := 'Branch_' + copy(CommitHash, 1, 8);
        Edit1.Visible := false;
        ComboBox1.Text := CommitHash;
        ComboBox1.Enabled := true;
        CheckBox1.Checked := false;
      End;
  End;
End;

End.

