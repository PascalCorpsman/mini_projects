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
Unit Unit4;

{$MODE objfpc}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

Type

  { TForm4 }

  TForm4 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
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
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    SpeedButton1: TSpeedButton;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure GroupBox2Resize(Sender: TObject);
    Procedure SpeedButton1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Procedure OrientLabels(Sender: TObject);
  End;

Var
  Form4: TForm4;

Implementation

{$R *.lfm}

{$I clickomania.inc}

Uses unit1, uclickomania;

{ TForm4 }

Procedure TForm4.FormCreate(Sender: TObject);
Var
  l: Tlabel;
  i: Integer;
Begin
  caption := 'Statistics';
  For i := 5 To 14 Do Begin
    l := Tlabel(FindComponent('Label' + inttostr(i)));
    l.caption := inttostr(i - 4) + '.';
    l := Tlabel(FindComponent('Label' + inttostr(i + 10)));
    l.left := 45;
    l.font.color := clblue;
  End;
  Constraints.MinHeight := height;
  Constraints.MaxHeight := height;
End;

Procedure TForm4.GroupBox2Resize(Sender: TObject);
Begin
  OrientLabels(Nil);
End;

Procedure TForm4.SpeedButton1Click(Sender: TObject);
Begin
  Form1.SpeedButton5Click(Nil);
End;

Procedure TForm4.OrientLabels(Sender: TObject);
Var
  i: Integer;
  l: Tlabel;
Begin
  For i := 25 To 34 Do Begin
    l := Tlabel(FindComponent('Label' + inttostr(i)));
    l.left := GroupBox2.Width - 20 - l.width;
  End;
End;

Procedure TForm4.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm4.Button1Click(Sender: TObject);
Begin
  Highscore.Clear(high(field) + 1, high(field[0]) + 1, UsedColorNumbers, UsedSpezials);
  form1.SpeedButton3Click(Nil);
End;

End.

