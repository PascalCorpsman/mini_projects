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
  Sudoku3x3;

Type
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
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

Var
  Form5: TForm5;

Implementation

{$R *.lfm}

Uses Unit1;
Var
  beenden: boolean;

Procedure TForm5.FormCreate(Sender: TObject);
Begin
  Caption := 'Sudoku ver. : ' + ver + ' Modify';
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
    drawfield;
    close;
  End;
End;

Procedure TForm5.BitBtn6Click(Sender: TObject);
Begin
  Close;
End;

Procedure TForm5.BitBtn3Click(Sender: TObject);
Var
  f: T3field;
  x, y, z: integer;
Begin
  // Kopieren des Feldes
  For x := 0 To 8 Do
    For y := 0 To 8 Do Begin
      f[x, y].Value := field[x, y].value;
      f[x, y].Marked := field[x, y].Marked;
      f[x, y].Maybeed := field[x, y].Maybeed;
      f[x, y].Fixed := field[x, y].Fixed;
      For z := 0 To 8 Do
        f[x, y].Pencil[z] := field[x, y].pencil[z];
    End;
  // Überschreiben des Orginales mit der Gedrehten Kopie
  For x := 0 To 8 Do
    For y := 0 To 8 Do Begin
      field[x, y].Value := f[8 - y, x].value;
      field[x, y].Marked := f[8 - y, x].Marked;
      field[x, y].Maybeed := f[8 - y, x].Maybeed;
      field[x, y].Fixed := f[8 - y, x].Fixed;
      For z := 0 To 8 Do
        field[x, y].Pencil[z] := f[8 - y, x].pencil[z];
    End;
  Drawfield;
End;

Procedure TForm5.BitBtn2Click(Sender: TObject);
Var
  f: T3field;
  x, y, z: integer;
Begin
  // Kopieren des Feldes
  For x := 0 To 8 Do
    For y := 0 To 8 Do Begin
      f[x, y].Value := field[x, y].value;
      f[x, y].Marked := field[x, y].Marked;
      f[x, y].Maybeed := field[x, y].Maybeed;
      f[x, y].Fixed := field[x, y].Fixed;
      For z := 0 To 8 Do
        f[x, y].Pencil[z] := field[x, y].pencil[z];
    End;
  // Überschreiben des Orginales mit der Gedrehten Kopie
  For x := 0 To 8 Do
    For y := 0 To 8 Do Begin
      field[x, y].Value := f[8 - x, y].value;
      field[x, y].Marked := f[8 - x, y].Marked;
      field[x, y].Maybeed := f[8 - x, y].Maybeed;
      field[x, y].Fixed := f[8 - x, y].Fixed;
      For z := 0 To 8 Do
        field[x, y].Pencil[z] := f[8 - x, y].pencil[z];
    End;
  Drawfield;
End;

Procedure TForm5.BitBtn1Click(Sender: TObject);
Var
  f: T3field;
  x, y, z: integer;
Begin
  // Kopieren des Feldes
  For x := 0 To 8 Do
    For y := 0 To 8 Do Begin
      f[x, y].Value := field[x, y].value;
      f[x, y].Marked := field[x, y].Marked;
      f[x, y].Maybeed := field[x, y].Maybeed;
      f[x, y].Fixed := field[x, y].Fixed;
      For z := 0 To 8 Do
        f[x, y].Pencil[z] := field[x, y].pencil[z];
    End;
  // Überschreiben des Orginales mit der Gedrehten Kopie
  For x := 0 To 8 Do
    For y := 0 To 8 Do Begin
      field[x, y].Value := f[x, 8 - y].value;
      field[x, y].Marked := f[x, 8 - y].Marked;
      field[x, y].Maybeed := f[x, 8 - y].Maybeed;
      field[x, y].Fixed := f[x, 8 - y].Fixed;
      For z := 0 To 8 Do
        field[x, y].Pencil[z] := f[x, 8 - y].pencil[z];
    End;
  Drawfield;
End;

Procedure TForm5.BitBtn4Click(Sender: TObject);
Var
  f: T3field;
  x, y, z: integer;
Begin
  // Kopieren des Feldes
  For x := 0 To 8 Do
    For y := 0 To 8 Do Begin
      f[x, y].Value := field[x, y].value;
      f[x, y].Marked := field[x, y].Marked;
      f[x, y].Maybeed := field[x, y].Maybeed;
      f[x, y].Fixed := field[x, y].Fixed;
      For z := 0 To 8 Do
        f[x, y].Pencil[z] := field[x, y].pencil[z];
    End;
  // Überschreiben des Orginales mit der Gedrehten Kopie
  For x := 0 To 8 Do
    For y := 0 To 8 Do Begin
      field[x, y].Value := f[y, 8 - x].value;
      field[x, y].Marked := f[y, 8 - x].Marked;
      field[x, y].Maybeed := f[y, 8 - x].Maybeed;
      field[x, y].Fixed := f[y, 8 - x].Fixed;
      For z := 0 To 8 Do
        field[x, y].Pencil[z] := f[y, 8 - x].pencil[z];
    End;
  Drawfield;
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
  x: Integer;
Begin
  // Reset der Substitution Felder
  For x := 1 To 9 Do
    TCombobox(Form5.findcomponent('Combobox' + inttostr(x))).text := inttostr(x);
End;

End.

