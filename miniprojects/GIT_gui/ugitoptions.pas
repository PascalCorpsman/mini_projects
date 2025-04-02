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
Unit uGITOptions;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ugit_common;

Type

  { TGitOptions }

  TGitOptions = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    RadioGroup1: TRadioGroup;
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure RadioGroup1Click(Sender: TObject);
  private
    ProjectRoot: String;
    Procedure SettingsToLCL(Const value: TRepoSettings);
    Function LCLToSettings(): TRepoSettings;
  public

    Procedure LoadOptions(aProjectRoot: String);

  End;

Var
  GitOptions: TGitOptions;

Implementation

{$R *.lfm}

Procedure TGitOptions.SettingsToLCL(Const value: TRepoSettings);
Begin
  edit1.text := value.UserName;
  edit2.text := value.EMail;
  edit3.text := value.DiffTool;
  ComboBox1.Text := value.eof;
End;

Function TGitOptions.LCLToSettings(): TRepoSettings;
Begin
  result.UserName := edit1.Text;
  result.EMail := edit2.Text;
  result.DiffTool := edit3.Text;
  result.eof := ComboBox1.Text;
End;

Procedure TGitOptions.Button3Click(Sender: TObject);
Var
  s: TRepoSettings;
Begin
  // Save
  s := LCLToSettings();
  SetRepoSettings(ProjectRoot, lowercase(ComboBox2.Items[ComboBox2.ItemIndex]), s);
End;

Procedure TGitOptions.Button2Click(Sender: TObject);
Begin
  // Save SSH Command
  SetRepoSSHCommand(ProjectRoot, Edit4.text);
End;

Procedure TGitOptions.FormCreate(Sender: TObject);
Begin
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
End;

Procedure TGitOptions.RadioGroup1Click(Sender: TObject);
Begin
  Case RadioGroup1.ItemIndex Of
    0: Begin // Effektive, dass ist komplizierter
        SettingsToLCL(GetEffectiveRepoSettings(ProjectRoot));
      End;
    1..3: Begin
        SettingsToLCL(GetRepoSettings(ProjectRoot, lowercase(RadioGroup1.Items[RadioGroup1.ItemIndex])));
      End;
  End;
End;

Procedure TGitOptions.LoadOptions(aProjectRoot: String);
Begin
  ProjectRoot := aProjectRoot;
  RadioGroup1.ItemIndex := 1;
  RadioGroup1.OnClick(Nil);
  Edit4.text := GetRepoSSHCommand(aProjectRoot);
End;

End.

