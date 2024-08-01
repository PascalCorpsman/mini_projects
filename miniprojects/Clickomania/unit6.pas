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
Unit Unit6;

{$MODE objfpc}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, uclickomania;

Type

  { TForm6 }

  TForm6 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ColorDialog1: TColorDialog;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Panel1: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel16: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormPaint(Sender: TObject);
    Procedure Panel2Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  End;

Var
  Form6: TForm6;
  cls: Array[0..high(StoneColors)] Of TColor;
  clss: Array[0..high(StoneColors)] Of TColor;

Implementation

{$R *.lfm}

{$I clickomania.inc}

{ TForm6 }

Procedure TForm6.FormCreate(Sender: TObject);
Var
  l: Tlabel;
  p: TPanel;
  i: integer;
Begin
  panel1.Caption := '';
  For i := 1 To 15 Do Begin
    l := Tlabel(FindComponent('Label' + inttostr(i)));
    l.Caption := 'Stone ' + inttostr(i);
    p := Tpanel(findcomponent('Panel' + inttostr(i + 1)));
    p.caption := '';
  End;
  label9.caption := 'Stone   9';
  Caption := 'Select color';
  Constraints.MinHeight := height;
  Constraints.MaxHeight := height;
  Constraints.MinWidth := width;
  Constraints.MaxWidth := width;
End;

Procedure TForm6.Button3Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm6.Button1Click(Sender: TObject);
Var
  i: Integer;
Begin
  For i := 0 To high(StoneColors) Do Begin
    clss[i] := TRGBToColor(StoneColorsDef[i])
  End;
  If assigned(sender) Then
    FormPaint(Nil);
End;

Procedure TForm6.Button2Click(Sender: TObject);
Var
  i: Integer;
Begin
  For i := 0 To High(StoneColors) Do
    cls[i] := clss[i];
  If Assigned(sender) Then
    close;
End;

Procedure TForm6.FormPaint(Sender: TObject);
Var
  i: Integer;
  p: Tpanel;
Begin
  For i := 2 To 16 Do Begin
    p := Tpanel(FindComponent('Panel' + inttostr(i)));
    p.Canvas.brush.color := clss[i - 2];
    p.Canvas.Pen.color := clblack;
    p.canvas.Rectangle(3, 3, 22, 17);
  End;
End;

Procedure TForm6.Panel2Click(Sender: TObject);
Var
  p: TPanel;
  s: String;
  i: Integer;
Begin
  p := Tpanel(sender);
  s := p.name;
  i := strtoint(copy(s, length('PanelX'), length(s)));
  ColorDialog1.Color := clss[i - 2];
  If ColorDialog1.execute Then Begin
    clss[i - 2] := ColorDialog1.Color;
  End;
  FormPaint(Nil);
End;

End.

