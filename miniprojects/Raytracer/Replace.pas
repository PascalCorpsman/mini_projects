(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Sch‰chterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of SimpleRay                                             *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Replace;

{$MODE ObjFPC}{$H+}

Interface

Uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

Type
  TForm3 = Class(TForm)
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBox10: TCheckBox;
    Procedure Button3Click(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure ComboBox2KeyPress(Sender: TObject; Var Key: Char);
    Procedure FormPaint(Sender: TObject);
    Procedure ComboBox1KeyPress(Sender: TObject; Var Key: Char);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

Var
  Form3: TForm3;

Implementation

Uses Unit1, SynEditTypes;

{$R *.lfm}

Procedure TForm3.Button3Click(Sender: TObject);
Begin
  Close;
End;

Procedure TForm3.Button1Click(Sender: TObject);
Var
  options: TSynSearchOptions;
Begin
  If (Length(ComboBox1.text) = 0) Or (Length(ComboBox2.text) = 0) Then Close;
  options := [];
  //  if frMatchCase in SourceReplaceDialog.Options then
  //    Include(Options,ssoMatchCase);
  //  if frWholeWord in SourceReplaceDialog.Options then
  //    Include(Options,ssoWholeWord);
  //  if not (frDown in SourceReplaceDialog.Options) then
  //    Include(Options,ssoBackwards);
  //  if (frReplace in SourceReplaceDialog.Options) then
  Include(Options, ssoReplace);
  //  if (frReplaceAll in SourceReplaceDialog.Options) then

   {(ssoMatchCase, ssoWholeWord, ssoBackwards,
      ssoEntireScope, ssoSelectedOnly, ssoReplace, ssoReplaceAll, ssoPrompt);
    }

  If checkbox2.checked Then Include(Options, ssoSelectedOnly); // Nur das Selectierte !!
  If checkbox3.checked Then Include(Options, ssoMatchCase); // Groﬂ Kleinschreibung

  //mainform.SynEditSearch1.
  form1.SynEdit1.SearchReplace(combobox1.text, combobox2.text, options);

  {  if mainform.SynEdit1.SearchReplace(SourceReplaceDialog.FindText,SourceReplaceDialog.ReplaceText,Options) = 0 then
      if MessageDlg('String not found. Restart from beginning?',mtInformation,[mbyes,mbno],0) = mryes then
      begin
        SynEdit1.CaretX := 1;
        SynEdit1.CaretY := 1;
        if SynEdit1.SearchReplace(SourceReplaceDialog.FindText,SourceReplaceDialog.ReplaceText,Options) = 0 then
          ShowMessage('String not found');
      end;

    }
  Combobox1.items.add(Combobox1.Text);
  Combobox2.items.add(Combobox2.Text);

  Close;
End;

Procedure TForm3.Button2Click(Sender: TObject);
Var
  options: TSynSearchOptions;
Begin
  If (Length(ComboBox1.text) = 0) Or (Length(ComboBox2.text) = 0) Then close;
  options := [];
  Include(Options, ssoReplaceAll);
  If checkbox2.checked Then Include(Options, ssoSelectedOnly);
  form1.SynEdit1.SearchReplace(combobox1.text, combobox2.text, options);
  Combobox1.items.add(Combobox1.Text);
  Combobox2.items.add(Combobox2.Text);
  close;
End;

Procedure TForm3.ComboBox2KeyPress(Sender: TObject; Var Key: Char);
Begin
  If Key = #27 Then close;
  If Key = #13 Then Button1.onclick(Nil);
End;

Procedure TForm3.FormPaint(Sender: TObject);
Begin
  Combobox1.SetFocus;
End;

Procedure TForm3.ComboBox1KeyPress(Sender: TObject; Var Key: Char);
Begin
  If Key = #13 Then Button1.onclick(Nil);
  If Key = #27 Then close;
End;

End.

