(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Q-Programmer                                          *)
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
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, LResources, uinterpreter;

Type
  TForm2 = Class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Edit2: TEdit;
    Label3: TLabel;
    Edit3: TEdit;
    Label4: TLabel;
    Edit4: TEdit;
    Label5: TLabel;
    Edit5: TEdit;
    Label6: TLabel;
    Edit6: TEdit;
    ScrollBar1: TScrollBar;
    Button1: TButton;
    Label7: TLabel;
    Button2: TButton;
    Procedure FormCreate(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
    Procedure ScrollBar1Change(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

Var
  Form2: TForm2;
  paras: TSringArray;
  names: TSringArray;
  fname: String;
  oi: integer;

Implementation

Uses unit1;

{$R *.lfm}

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  caption := 'Testrun';
End;

Procedure TForm2.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm2.Button1Click(Sender: TObject);
Var
  i: Integer;
  e: Tedit;
Begin
  // Speichern
  For i := 0 To 5 Do Begin
    e := Tedit(form2.findcomponent('Edit' + inttostr(i + 1)));
    If e.visible Then Begin
      paras[oi + i] := e.text;
    End
    Else
      break;
  End;
  (*
  Ein Kleiner Syntaxcheck ??
  *)
  label7.caption := 'Result : ' + form1.Interpreter.CallFunction(fname, Paras);
End;

Procedure TForm2.ScrollBar1Change(Sender: TObject);
Var
  i: Integer;
  e: Tedit;
  l: TLabel;
Begin
  // Speichern
  For i := 0 To 5 Do Begin
    e := Tedit(form2.findcomponent('Edit' + inttostr(i + 1)));
    paras[oi + i] := e.text;
  End;
  // Laden
  oi := ScrollBar1.position;
  For i := 0 To 5 Do Begin
    l := TLabel(form2.findcomponent('Label' + inttostr(i + 1)));
    e := Tedit(form2.findcomponent('Edit' + inttostr(i + 1)));
    e.text := paras[oi + i];
    l.caption := names[oi + i];
  End;
End;

End.

