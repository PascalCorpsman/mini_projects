(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Clickomania                                           *)
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
  Classes, SysUtils, Forms, Graphics, Dialogs, ExtCtrls, StdCtrls, Spin,
  ComCtrls, uclickomania;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    ComboBox1: TComboBox;
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
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    RadioButton4: TRadioButton;
    RadioButton5: TRadioButton;
    RadioButton6: TRadioButton;
    ScrollBar1: TScrollBar;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    SpinEdit4: TSpinEdit;
    SpinEdit5: TSpinEdit;
    SpinEdit6: TSpinEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure CheckBox1Change(Sender: TObject);
    Procedure CheckBox8Change(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure RadioButton1Change(Sender: TObject);
    Procedure RadioButton5Change(Sender: TObject);
    Procedure SpinEdit3Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  End;

Var
  Form2: TForm2;
  backdirpic: String = '';
  backdirdir: String = '';
  veraendert: Boolean = false;

Implementation

Uses unit1, unit6;

{$R *.lfm}

{$I clickomania.inc}

{ TForm2 }

Procedure TForm2.Button3Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm2.Button4Click(Sender: TObject);
Begin
  If OpenDialog1.execute Then Begin
    backdirpic := OpenDialog1.FileName;
    form2.label15.caption := ExtractFileName(backdirpic);
  End;
End;

Procedure TForm2.Button5Click(Sender: TObject);
Begin
  If SelectDirectoryDialog1.execute Then Begin
    backdirdir := SelectDirectoryDialog1.FileName;
    form2.label16.caption := backdirdir;
  End;
  //  weiter mit Choose File und Choose Dir
//
//  Danach Undo
//  Danach animation

End;

Procedure TForm2.Button6Click(Sender: TObject);
Var
  i: Integer;
  l: Tlabel;
Begin
  For i := 0 To high(StoneColors) Do Begin
    l := Tlabel(form6.FindComponent('Label' + inttostr(i + 1)));
    If i < SpinEdit5.value Then
      l.font.color := clred
    Else
      l.font.color := clblack;
    clss[i] := cls[i];
  End;
  form6.showmodal;
End;

Procedure TForm2.Button7Click(Sender: TObject);
Begin
  ShowMessage('In high score mode you need to get as much points as possible.' + LineEnding +
    'To see how many points a stone will give, right click on the stone.');
End;

Procedure TForm2.CheckBox1Change(Sender: TObject);
Begin
  ScrollBar1.Enabled := CheckBox1.checked;
End;

Procedure TForm2.CheckBox8Change(Sender: TObject);
Begin
  label18.Enabled := CheckBox8.checked;
  label19.Enabled := CheckBox8.checked;
  SpinEdit6.Enabled := CheckBox8.checked;
End;

Procedure TForm2.Button1Click(Sender: TObject);
Begin

  // Karteikarte Field
  form2.SpinEdit1.value := 16;
  form2.SpinEdit2.value := 10;
  form2.SpinEdit3.value := 21;
  form2.SpinEdit4.value := 20;
  form2.SpinEdit3Change(Nil);

  // Karteikarte Stones
  form2.SpinEdit5.value := 5;
  form2.CheckBox1.Checked := false;
  form2.ScrollBar1.Enabled := false;
  form2.ScrollBar1.Position := 50;
  form2.CheckBox2.Checked := false;
  form2.CheckBox3.Checked := true;
  form2.CheckBox4.Checked := false;
  form6.Button1.OnClick(Nil);
  form6.Button2.OnClick(Nil);

  // Karteikarte Background
  backdirpic := '';
  backdirdir := '';
  label15.caption := '[none selected]';
  label16.caption := '[none selected]';

  // Karteikarte Actions
  form2.CheckBox6.checked := false;
  form2.CheckBox7.checked := true;
  form2.CheckBox8.checked := false;
  form2.SpinEdit6.Value := 90;

  // Karteikarte Special
  form2.CheckBox5.checked := true;
  form2.RadioButton4.checked := true;


End;

Procedure TForm2.Button2Click(Sender: TObject);
Var
  f: TFileStream;
  i: Integer;
Begin

  // Karteikarte Field
  FieldRows := SpinEdit1.value;
  FieldColumns := SpinEdit2.value;
  StonesHeight := SpinEdit3.value;
  StonesWidth := SpinEdit4.value;

  // Karteikarte Stones
  ColorNumbers := SpinEdit5.value;
  F_XsTextured := CheckBox1.Checked;
  AlphaF_XsTextured := ScrollBar1.Position / 100;
  F_XsVoyer := CheckBox2.Checked;
  f_XsTracelines := CheckBox3.Checked;
  SoundsPlaysounds := CheckBox4.Checked;
  For i := 0 To High(StoneColors) Do
    stonecolors[i] := ColorToTRGB(cls[i]);

  // Karteikarte Background
  If RadioButton1.checked Then
    Backstyle := bsFromPicture;
  If RadioButton2.checked Then
    Backstyle := bsFromDirectory;
  If RadioButton3.checked Then
    Backstyle := bsDefault;
  If RadioButton6.checked Then
    Backstyle := bsgray;
  DirectPicture := backdirpic;
  DirectDir := backdirdir;

  // Karteikarte Actions
  EnableSpecialStones := CheckBox6.checked;
  EnableAnimations := CheckBox7.checked;
  EnableAutoDropCountDown := CheckBox8.checked;
  Duration := SpinEdit6.Value;

  // Karteikarte Special
  Trackthegame := CheckBox5.checked;
  form1.SpeedButton2.Enabled := Trackthegame;
  If RadioButton4.checked Then ScoreMode := smLow;
  If RadioButton5.checked Then ScoreMode := smHigh;

  // Speichern des Ganzen
  f := TFileStream.create(ConfigFile, fmOpenWrite Or fmcreate);
  f.write(version, sizeof(version));

  // Karteikarte Field
  f.write(FieldColumns, sizeof(FieldColumns));
  f.write(FieldRows, sizeof(FieldRows));
  f.write(StonesHeight, sizeof(StonesHeight));
  f.write(StonesWidth, sizeof(StonesWidth));

  // Karteikarte Stones
  f.write(ColorNumbers, sizeof(ColorNumbers));
  f.write(F_XsTextured, sizeof(F_XsTextured));
  f.write(AlphaF_XsTextured, sizeof(AlphaF_XsTextured));
  f.write(F_XsVoyer, sizeof(F_XsVoyer));
  f.write(f_XsTracelines, sizeof(f_XsTracelines));
  f.write(SoundsPlaysounds, sizeof(SoundsPlaysounds));
  f.write(StoneColors, sizeof(StoneColors));

  // Karteikarte Background
  f.write(Backstyle, sizeof(TBackgroundstyle));
  StringToStream(f, DirectPicture);
  StringToStream(f, DirectDir);

  // Karteikarte Actions
  f.write(EnableSpecialStones, sizeof(EnableSpecialStones));
  f.write(EnableAnimations, sizeof(EnableAnimations));
  f.write(EnableAutoDropCountDown, sizeof(EnableAutoDropCountDown));
  f.write(Duration, sizeof(Duration));

  // Karteikarte Special
  f.write(Trackthegame, sizeof(Trackthegame));
  f.write(ScoreMode, sizeof(ScoreMode));

  f.free;

  veraendert := true;

  close;
End;

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  OpenDialog1.InitialDir := apppath;
  SelectDirectoryDialog1.InitialDir := apppath;
  caption := 'Options';
  PageControl1.TabIndex := 0;
  panel2.caption := '';
End;

Procedure TForm2.RadioButton1Change(Sender: TObject);
Begin
  Button4.Enabled := RadioButton1.Checked;
  Button5.Enabled := RadioButton2.Checked;
End;

Procedure TForm2.RadioButton5Change(Sender: TObject);
Begin
  button7.enabled := RadioButton5.checked;
End;

Procedure TForm2.SpinEdit3Change(Sender: TObject);
Var
  fw, fh: Integer;
Begin
  panel2.Width := SpinEdit4.Value;
  panel2.height := SpinEdit3.Value;
  fw := SpinEdit2.value * SpinEdit4.value + 10;
  fh := SpinEdit1.value * SpinEdit3.value + 67;
  label4.Caption := 'Window height ' + inttostr(fh) + ' Pixel';
  label5.Caption := 'Window width ' + inttostr(fw) + ' Pixel';
  If fw < screen.Width Then
    label5.font.color := clblack
  Else
    label5.font.color := clred;
  If fh < screen.Height Then
    label4.font.color := clblack
  Else
    label4.font.color := clred;
End;

End.

