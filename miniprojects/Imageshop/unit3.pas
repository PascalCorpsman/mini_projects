(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Imageshop                                             *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit3;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  SynEdit, SynHighlighterPas, SynCompletion, uinterpreter;

Type

  { TForm3 }

  TForm3 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    SynEdit1: TSynEdit;
    SynPasSyn1: TSynPasSyn;
    Procedure Button1Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
  private
    comp: Tinterpreter;
  public
    Procedure PrepareForBlend;
    Procedure PrepareForOp;
    Function Compilable(): Boolean;
  End;

Var
  Form3: TForm3;

Implementation

{$R *.lfm}

Uses unit4;

{ TForm3 }

Procedure TForm3.Button5Click(Sender: TObject);
Begin
  form4.left := Form3.Left + Form3.Width;
  form4.Top := Form3.Top;
  If Not form4.Visible Then form4.Show;
End;

Procedure TForm3.Button1Click(Sender: TObject);
Begin
  If comp.Compile(SynEdit1.Text) Then Begin
    Memo1.Text := 'well done.';
    If Panel2.Height <= 1 Then
      Panel2.Height := 30;
  End
  Else Begin
    Memo1.Text := comp.Errormessage;
    If Panel2.Height <= 1 Then
      Panel2.Height := 100;
  End;
End;

Procedure TForm3.Button3Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm3.Button4Click(Sender: TObject);
Begin
  button1.Click;
  If memo1.Text <> 'well done.' Then Begin
    showmessage('Please fix your code first.');
    exit;
  End;
  ModalResult := mrOK;
End;

Procedure TForm3.FormCreate(Sender: TObject);
Begin
  caption := 'Enter Sourcecode';
  Panel1.Caption := '';
  Panel2.Caption := '';
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
  comp := Tinterpreter.create;
  Memo1.Text := '';
End;

Procedure TForm3.FormDestroy(Sender: TObject);
Begin
  comp.free;
End;

Procedure TForm3.PrepareForBlend;
Begin
  SynEdit1.Text :=
    '(*' + LineEnding +
    ' * Enter your implementation below, do not change the function interface !' + LineEnding +
    ' *)' + LineEnding +
    'Type' + LineEnding +
    '  TPixel = Record' + LineEnding +
    '    r, g, b, a: Single;' + LineEnding +
    '  End;' + LineEnding +
    '' + LineEnding +
    'Function DoTheSliderPixelOperation(Const Pixel1, Pixel2: TPixel; x, y, SliderValue: Single): TPixel;' + LineEnding +
    'Begin' + LineEnding +
    '  // This function does indeed nothing.' + LineEnding +
    '  result := Pixel1;' + LineEnding +
    'End;';
  GroupBox3.Visible := true;
  edit1.text := '1.0';
  edit2.text := '1.0';
  edit3.text := '1.0';
  edit4.text := '1.0';

  edit5.text := '1.0';
  edit6.text := '1.0';
  edit7.text := '1.0';
  edit8.text := '1.0';

  edit9.text := '0.0';
  edit10.text := '0.0';
  edit11.text := '0.0';
  Panel2.Height := 1;
End;

Procedure TForm3.PrepareForOp;
Begin
  SynEdit1.Text :=
    '(*' + LineEnding +
    ' * Enter your implementation below, do not change the function interface !' + LineEnding +
    ' *)' + LineEnding +
    'Type' + LineEnding +
    '  TPixel = Record' + LineEnding +
    '    r, g, b, a: Single;' + LineEnding +
    '  End;' + LineEnding +
    '' + LineEnding +
    'Function DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y, SliderValue: Single): TPixel;' + LineEnding +
    'Begin' + LineEnding +
    '  // This function does indeed nothing.' + LineEnding +
    '  result := Pixel1;' + LineEnding +
    'End;';
  GroupBox3.Visible := false;
  edit1.text := '1.0';
  edit2.text := '1.0';
  edit3.text := '1.0';
  edit4.text := '1.0';

  edit9.text := '0.0';
  edit10.text := '0.0';
  edit11.text := '0.0';
  Panel2.Height := 1;
End;

Function TForm3.Compilable(): Boolean;
Begin
  result := false;
End;

End.

