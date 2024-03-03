(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of KLab                                                  *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit2;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, Spin, Buttons, types;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    CheckBox1: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    CheckBox13: TCheckBox;
    CheckBox14: TCheckBox;
    CheckBox15: TCheckBox;
    CheckBox16: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    FloatSpinEdit1: TFloatSpinEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label28: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    PageControl1: TPageControl;
    SpeedButton1: TSpeedButton;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Tabsheet4: TTabSheet;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure CheckBox10Click(Sender: TObject);
    Procedure CheckBox14Click(Sender: TObject);
    Procedure CheckBox6Click(Sender: TObject);
    Procedure CheckBox7Click(Sender: TObject);
    Procedure CheckBox8Click(Sender: TObject);
    Procedure CheckBox9Click(Sender: TObject);
    Procedure ComboBox1Change(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Procedure Load_all_Konfigs();
    Procedure Read_all_Konfigs();
  End;

Var
  Form2: TForm2;

Implementation

{$R *.lfm}

Uses
  uklab,
  lazutf8,
  unit1, // Für Global changed
  unit7;

Var
  searchpath: String;
  libsearchpath: String;

  { TForm2 }

Procedure TForm2.Load_all_Konfigs;
Begin
  // Common
  ComboBox1.Items.Text := KlabConfig.Aviable_CPUS.Text;
  ComboBox1.Text := KlabConfig.CPU;
  SpinEdit1.Value := KlabConfig.Clock;
  edit4.text := KlabConfig.HexFile;
  edit5.text := KlabConfig.MapFile;
  edit6.text := KlabConfig.Logfile;
  edit7.text := KlabConfig.OutFile;
  ComboBox1Change(Nil);
  // Compiler
  Edit1.text := KlabConfig.CompilerCommand;
  CheckBox1.Checked := KlabConfig.MCall;
  CheckBox2.Checked := KlabConfig.Wstrict;
  CheckBox3.Checked := KlabConfig.Wall;
  CheckBox4.Checked := KlabConfig.g;
  CheckBox5.Checked := KlabConfig.definecpu;
  CheckBox15.Checked := KlabConfig.usesearchpath;
  searchpath := KlabConfig.searchpaths.Text;
  CheckBox16.Checked := KlabConfig.uselibsearchpath;
  libsearchpath := KlabConfig.libsearchpaths.Text;
  Case KlabConfig.optimization Of
    '0': ComboBox2.ItemIndex := 0;
    '1': ComboBox2.ItemIndex := 1;
    '2': ComboBox2.ItemIndex := 2;
    '3': ComboBox2.ItemIndex := 3;
    's': ComboBox2.ItemIndex := 4;
  End;
  // Linker
  edit2.text := KlabConfig.LinkerCommand;
  edit3.text := KlabConfig.ObjectCopyCommand;
  // Programmer
  CheckBox6.Checked := KlabConfig.OverrideBaud;
  CheckBox7.Checked := KlabConfig.SpecifyJTAGPeriod;
  CheckBox8.Checked := KlabConfig.SpecifyProgrammerType;
  CheckBox9.Checked := KlabConfig.SpecifyConnectionPort;
  CheckBox10.Checked := KlabConfig.SpecifyExternalConfigFile;
  CheckBox11.Checked := KlabConfig.DisableAutoErase;
  CheckBox12.Checked := KlabConfig.DoNotWrite;
  CheckBox13.Checked := KlabConfig.OverrideInvalidSignature;
  CheckBox14.Checked := KlabConfig.CountErases;
  ComboBox3.Text := inttostr(KlabConfig.RS232BaudRate);
  FloatSpinEdit1.Value := KlabConfig.JTAGPeriod / 10;
  edit8.text := KlabConfig.ProgrammerType;
  edit9.text := KlabConfig.ConnectionPort;
  Edit10.text := KlabConfig.ExternalFile;
  SpinEdit2.Value := KlabConfig.EraseCount;
  CheckBox6Click(Nil);
  CheckBox7Click(Nil);
  CheckBox8Click(Nil);
  CheckBox9Click(Nil);
  CheckBox10Click(Nil);
  CheckBox14Click(Nil);
End;

Procedure TForm2.Read_all_Konfigs;
Begin
  // Liest Unit2 aus und Speichert es in KlabConfig
  // Common
  KlabConfig.CPU := ComboBox1.Text;
  KlabConfig.Clock := SpinEdit1.Value;
  KlabConfig.HexFile := edit4.text;
  KlabConfig.MapFile := edit5.text;
  KlabConfig.Logfile := edit6.text;
  KlabConfig.OutFile := edit7.text;
  cpu := LoadCPU(KlabConfig.CPU);
  // Compiler
  KlabConfig.CompilerCommand := Edit1.text;
  KlabConfig.MCall := CheckBox1.Checked;
  KlabConfig.Wstrict := CheckBox2.Checked;
  KlabConfig.Wall := CheckBox3.Checked;
  KlabConfig.g := CheckBox4.Checked;
  KlabConfig.definecpu := CheckBox5.Checked;
  KlabConfig.usesearchpath := CheckBox15.Checked;
  KlabConfig.searchpaths.Text := searchpath;
  KlabConfig.uselibsearchpath := CheckBox16.Checked;
  KlabConfig.libsearchpaths.Text := libsearchpath;
  Case ComboBox2.ItemIndex Of
    0: KlabConfig.optimization := '0';
    1: KlabConfig.optimization := '1';
    2: KlabConfig.optimization := '2';
    3: KlabConfig.optimization := '3';
    4: KlabConfig.optimization := 's';
  End;
  // Linker
  KlabConfig.LinkerCommand := edit2.text;
  KlabConfig.ObjectCopyCommand := edit3.text;
  // Programmer
  KlabConfig.OverrideBaud := CheckBox6.Checked;
  KlabConfig.SpecifyJTAGPeriod := CheckBox7.Checked;
  KlabConfig.SpecifyProgrammerType := CheckBox8.Checked;
  KlabConfig.SpecifyConnectionPort := CheckBox9.Checked;
  KlabConfig.SpecifyExternalConfigFile := CheckBox10.Checked;
  KlabConfig.DisableAutoErase := CheckBox11.Checked;
  KlabConfig.DoNotWrite := CheckBox12.Checked;
  KlabConfig.OverrideInvalidSignature := CheckBox13.Checked;
  KlabConfig.CountErases := CheckBox14.Checked;
  KlabConfig.RS232BaudRate := strtointdef(ComboBox3.Text, 19200);
  KlabConfig.JTAGPeriod := round(FloatSpinEdit1.Value * 10);
  KlabConfig.ProgrammerType := edit8.text;
  KlabConfig.ConnectionPort := edit9.text;
  KlabConfig.ExternalFile := Edit10.text;
  KlabConfig.EraseCount := SpinEdit2.Value;
End;

Procedure TForm2.Button3Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm2.Button5Click(Sender: TObject);
Begin
  form7.SelectDirectoryDialog1.InitialDir := ExtractFilePath(ParamStrUTF8(0));
  form7.Caption := 'Search Path Properties';
  form7.ListBox1.Items.Text := searchpath;
  form7result := false;
  form7.ShowModal;
  If form7result Then Begin
    searchpath := form7.ListBox1.Items.Text;
  End;
End;

Procedure TForm2.Button6Click(Sender: TObject);
Begin
  form7.SelectDirectoryDialog1.InitialDir := ExtractFilePath(ParamStrUTF8(0));
  form7.Caption := 'LibSearch Path Properties';
  form7.ListBox1.Items.Text := libsearchpath;
  form7result := false;
  form7.ShowModal;
  If form7result Then Begin
    libsearchpath := form7.ListBox1.Items.Text;
  End;
End;

Procedure TForm2.CheckBox6Click(Sender: TObject);
Begin
  ComboBox3.Enabled := CheckBox6.Checked;
  label26.Enabled := CheckBox6.Checked;
End;

Procedure TForm2.CheckBox7Click(Sender: TObject);
Begin
  FloatSpinEdit1.Enabled := CheckBox7.Checked;
  label27.Enabled := CheckBox7.Checked;
End;

Procedure TForm2.CheckBox8Click(Sender: TObject);
Begin
  edit8.Enabled := CheckBox8.Checked;
End;

Procedure TForm2.CheckBox9Click(Sender: TObject);
Begin
  edit9.Enabled := CheckBox9.Checked;

End;

Procedure TForm2.CheckBox10Click(Sender: TObject);
Begin
  edit10.Enabled := CheckBox10.Checked;
End;

Procedure TForm2.CheckBox14Click(Sender: TObject);
Begin
  label28.Enabled := CheckBox14.Checked;
  button4.Enabled := CheckBox14.Checked;
  SpinEdit2.Enabled := CheckBox14.Checked;
End;

Procedure TForm2.ComboBox1Change(Sender: TObject);
Var
  cp: TCPU;
Begin
  cp := LoadCPU(ComboBox1.Text);
  SpinEdit1.MaxValue := cp.maxcpu;
  label15.Caption := inttostr(cp.flash);
  label16.Caption := inttostr(cp.ram);
  label17.Caption := inttostr(cp.maxcpu);
  label18.Caption := inttostr(cp.eeprom);
  label15.Left := label19.Left - label15.Width - 10;
  label16.Left := label19.Left - label16.Width - 10;
  label17.Left := label19.Left - label17.Width - 10;
  label18.Left := label19.Left - label18.Width - 10;
  TabSheet1.Repaint;
End;

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  Tform(self).Constraints.MaxHeight := Tform(self).Height;
  Tform(self).Constraints.MinHeight := Tform(self).Height;
  Tform(self).Constraints.Maxwidth := Tform(self).width;
  Tform(self).Constraints.Minwidth := Tform(self).width;
  PageControl1.PageIndex := 0;
{$IFDEF WINDOWS}
  label10.Visible := false;
  edit6.Visible := false;
{$ENDIF}
End;

Procedure TForm2.FormShow(Sender: TObject);
Begin
  label15.Left := label19.Left - label15.Width - 10;
  label16.Left := label19.Left - label16.Width - 10;
  label17.Left := label19.Left - label17.Width - 10;
  label18.Left := label19.Left - label18.Width - 10;
End;

Procedure TForm2.Button1Click(Sender: TObject);
Begin
  If Not GlobalChanged Then Begin
    form1.caption := form1.caption + '*';
  End;
  GlobalChanged := true;
  Read_all_Konfigs;
  close;
End;

Procedure TForm2.Button2Click(Sender: TObject);
Begin
  Read_all_Konfigs;
  SaveEditorProperties();
End;

End.

