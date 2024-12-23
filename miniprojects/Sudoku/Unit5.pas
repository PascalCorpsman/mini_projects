(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Sudoku                                                *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit5;

{$MODE ObjFPC}{$H+}

Interface

Uses
  SysUtils, Forms, Dialogs, Classes, Controls, StdCtrls, Buttons, LResources,
  usudoku;

Type

  { TForm5 }

  TForm5 = Class(TForm)
    GroupBox1: TGroupBox;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    ComboBox1: TComboBox;
    Label1: TLabel;
    BitBtn6: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn7: TBitBtn;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    ComboBox4: TComboBox;
    Label2: TLabel;
    ComboBox5: TComboBox;
    ComboBox6: TComboBox;
    ComboBox7: TComboBox;
    Label3: TLabel;
    ComboBox8: TComboBox;
    ComboBox9: TComboBox;
    BitBtn8: TBitBtn;
    Procedure FormCreate(Sender: TObject);
    Procedure BitBtn5Click(Sender: TObject);
    Procedure BitBtn6Click(Sender: TObject);
    Procedure BitBtn3Click(Sender: TObject);
    Procedure BitBtn2Click(Sender: TObject);
    Procedure BitBtn1Click(Sender: TObject);
    Procedure BitBtn4Click(Sender: TObject);
    Procedure BitBtn7Click(Sender: TObject);
    Procedure ComboBox7KeyPress(Sender: TObject; Var Key: Char);
    Procedure BitBtn8Click(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
  private
    { Private-Deklarationen }
    fField: TSudoku;
    fRepaintEvent: TNotifyEvent;
  public
    { Public-Deklarationen }
    Property Sudoku: TSudoku read fField;
    Procedure Init(Const aField: TSudoku; RepaintEvent: TNotifyEvent);
  End;

Var
  Form5: TForm5;

Implementation

{$R *.lfm}

Var
  beenden: boolean;

Procedure TForm5.FormCreate(Sender: TObject);
Begin
  Caption := 'Sudoku ver. : ' + ver + ' Modify';
  fField := TSudoku.Create(3);
End;

Procedure TForm5.FormDestroy(Sender: TObject);
Begin
  fField.free;
  fField := Nil;
End;

Procedure TForm5.BitBtn5Click(Sender: TObject);
Var
  x: Integer;
Begin
  beenden := true;
  BitBtn7.OnClick(Nil); // Prüfen auf Gültige Substitution's
  If beenden Then Begin
    For x := 1 To 9 Do
      substitution[x] := TCombobox(Form5.findcomponent('Combobox' + inttostr(x))).text;
    fRepaintEvent(Nil);
    close;
  End;
End;

Procedure TForm5.BitBtn6Click(Sender: TObject);
Begin
  Close;
End;

Procedure TForm5.BitBtn3Click(Sender: TObject);
Begin
  fField.Rotate(false);
  fRepaintEvent(Nil);
End;

Procedure TForm5.BitBtn2Click(Sender: TObject);
Begin
  fField.Mirror(true);
  fRepaintEvent(Nil);
End;

Procedure TForm5.BitBtn1Click(Sender: TObject);
Begin
  fField.Mirror(false);
  fRepaintEvent(Nil);
End;

Procedure TForm5.BitBtn4Click(Sender: TObject);
Begin
  fField.Rotate(true);
  fRepaintEvent(Nil);
End;

Procedure TForm5.BitBtn7Click(Sender: TObject);
Var
  x, y: Integer;
Begin
  beenden := true;
  // erst Prüfen ob die eingaben Disjunct sind, dann
  For x := 1 To 9 Do
    For y := 1 To 9 Do
      If X <> y Then
        If TCombobox(Form5.findcomponent('Combobox' + inttostr(x))).text =
          TCombobox(Form5.findcomponent('Combobox' + inttostr(y))).text Then
          beenden := false;
  If Not Beenden Then
    showmessage('Error two Characters are the same !')
  Else If Sender <> Nil Then
    showmessage('Everything seem''s to be OK.');
End;

Procedure TForm5.ComboBox7KeyPress(Sender: TObject; Var Key: Char);
Var
  s: String;
Begin
  // Nur Großbuchstaben, die Klein buchstaben sind Maybeed
  s := Key;
  key := uppercase(s)[1];
  // ohne A S D W
  If Not (Key In ['1'..'9' {, 'B', 'C', 'E'..'R', 'T'..'V', 'X'..'Z' (*, 'b', 'c', 'e'..'r', 't'..'v', 'x'..'z'*)}]) Then key := #0;
  If Key <> #0 Then TCombobox(sender).text := '';
End;

Procedure TForm5.BitBtn8Click(Sender: TObject);
Var
  i: Integer;
Begin
  // Reset der Substitution Felder
  For i := 1 To 9 Do
    TCombobox(Form5.findcomponent('Combobox' + inttostr(i))).text := inttostr(i);
End;

Procedure TForm5.Init(Const aField: TSudoku; RepaintEvent: TNotifyEvent);
Begin
  fField.CloneFieldFrom(aField);
  fRepaintEvent := RepaintEvent;
End;

End.

