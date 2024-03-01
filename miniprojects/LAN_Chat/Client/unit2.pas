(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Lan chat                                              *)
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

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    ColorDialog1: TColorDialog;
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
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    ScrollBar1: TScrollBar;
    Shape1: TShape;
    Shape2: TShape;
    Shape3: TShape;
    Shape4: TShape;
    Timer1: TTimer;
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private
    Procedure OnShapeClick(Sender: TObject);
  public

  End;

Var
  Form2: TForm2;

Implementation

{$R *.lfm}

Uses
  unit1,
  ulanchatcommon,
  uip;

{ TForm2 }

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  caption := 'Settings';
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
  Shape1.OnClick := @OnShapeClick;
  Shape2.OnClick := @OnShapeClick;
  Shape3.OnClick := @OnShapeClick;
  Shape4.OnClick := @OnShapeClick;
End;

Procedure TForm2.FormShow(Sender: TObject);
Begin
  // Enable UDP-Polling for Server
  If trim(edit1.text) = '' Then Begin
    Timer1.Enabled := true;
    If Not form1.LUDPComponent1.Connect('', UDPPingPort) Then Begin
      timer1.enabled := false;
    End;
  End;
End;

Procedure TForm2.Timer1Timer(Sender: TObject);
Var
  N: TNetworkAdapterList;
  i: Integer;
  s: String;
Begin
  Try
    n := GetLocalIPs();
    For i := 0 To high(n) Do Begin
      s := CalculateBroadCastAddressFromAddress(n[i].IpAddress, n[i].SubnetMask);
      form1.LUDPComponent1.SendMessage('Ping', s);
    End;
  Except
    timer1.enabled := false;
    Showmessage('Could not ping, maybe there is no valid network card present.');
  End;
End;

Procedure TForm2.OnShapeClick(Sender: TObject);
Begin
  ColorDialog1.Color := TShape(Sender).Brush.Color;
  If ColorDialog1.Execute Then Begin
    TShape(Sender).Brush.Color := ColorDialog1.Color;
  End;
End;

End.

