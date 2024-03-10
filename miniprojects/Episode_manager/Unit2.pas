(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Episode manager                                       *)
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
  SysUtils, Classes, Forms, StdCtrls, CheckLst;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    CheckListBox1: TCheckListBox;
    Label1: TLabel;
    Procedure FormCreate(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure FormResize(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

Var
  Form2: TForm2;

Implementation

{$R *.lfm}

Uses Unit1;

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  caption := 'Found entries';
End;

Procedure TForm2.Button5Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm2.FormResize(Sender: TObject);
Begin
  If Form2.width < 500 Then Form2.width := 500;
  If Form2.height < 220 Then Form2.height := 220;
  {  Button5.top := form2.height - 63;
    button5.width := form2.width - 28;
    Button1.top := form2.height - 101;
    Button2.top := form2.height - 101;
    Button3.top := form2.height - 101;
    Button4.top := form2.height - 101;
    button4.left := Form2.width - 181;
    button3.left := Form2.width - 93;
    Checklistbox1.width := Form2.width - 28;
    checklistbox1.height := Form2.height - 124;
    label1.top := form2.Height - 100;
}End;

Procedure TForm2.Button4Click(Sender: TObject);
Var
  i: integer;
Begin
  For i := 0 To Checklistbox1.Items.count - 1 Do
    Checklistbox1.Checked[i] := true;
End;

Procedure TForm2.Button3Click(Sender: TObject);
Var
  i: integer;
Begin
  For i := 0 To Checklistbox1.Items.count - 1 Do
    Checklistbox1.Checked[i] := false;
End;

Procedure TForm2.Button1Click(Sender: TObject);
Var
  l: Tstringlist;
  r, g, t, i, j, k: integer;
Begin
  g := 0;
  t := CheckListBox1.ItemIndex;
  r := CheckListBox1.TopIndex;
  CheckListBox1.Items.BeginUpdate;
  l := TStringList.create;
  l.clear;
  For i := 0 To Checklistbox1.items.count - 1 Do
    If Checklistbox1.Checked[i] Then
      l.add(Checklistbox1.Items[i]);
  Database.MarkWithValue(l, true);
  l.free;
  // Neu Laden
  form2.CheckListBox1.clear;
  For i := 0 To form1.Checklistbox1.Items.count - 1 Do
    If form1.Checklistbox1.Checked[i] Then
      For j := 0 To form1.Checklistbox2.Items.count - 1 Do
        If form1.Checklistbox2.Checked[j] Then Begin
          l := Database.gebeAlleDatensaetzemit(form1.Checklistbox1.Items[i], form1.Checklistbox2.Items[j], true);
          For k := 0 To l.count - 1 Do Begin
            // Extrahieren des Zählers für Gesehen
            If pos('yes', copy(l[k], pos('|', l[k]) - 6, 6)) <> 0 Then inc(g);
            form2.CheckListBox1.items.add(l[k]);
            form2.CheckListBox1.checked[form2.CheckListBox1.items.count - 1] := false;
          End;
          l.free;
        End;
  CheckListBox1.Items.EndUpdate;
  CheckListBox1.ItemIndex := t;
  CheckListBox1.TopIndex := r;
  form2.label1.caption := inttostr(Form2.checklistbox1.items.count) + ' found entries. ' + inttostr(g) + ' seen.';
End;

Procedure TForm2.Button2Click(Sender: TObject);
Var
  l: Tstringlist;
  r, g, t, i, j, k: integer;
Begin
  g := 0;
  t := CheckListBox1.ItemIndex;
  r := CheckListBox1.TopIndex;
  CheckListBox1.Items.BeginUpdate;
  l := TStringList.create;
  l.clear;
  For i := 0 To Checklistbox1.items.count - 1 Do
    If Checklistbox1.Checked[i] Then
      l.add(Checklistbox1.Items[i]);
  Database.MarkWithValue(l, false);
  l.free;
  // Neu Laden
  form2.CheckListBox1.clear;
  For i := 0 To form1.Checklistbox1.Items.count - 1 Do
    If form1.Checklistbox1.Checked[i] Then
      For j := 0 To form1.Checklistbox2.Items.count - 1 Do
        If form1.Checklistbox2.Checked[j] Then Begin
          l := Database.gebeAlleDatensaetzemit(form1.Checklistbox1.Items[i], form1.Checklistbox2.Items[j], true);
          For k := 0 To l.count - 1 Do Begin
            // Extrahieren des Zählers für Gesehen
            If pos('yes', copy(l[k], pos('|', l[k]) - 6, 6)) <> 0 Then inc(g);
            form2.CheckListBox1.items.add(l[k]);
            form2.CheckListBox1.checked[form2.CheckListBox1.items.count - 1] := false;
          End;
          l.free;
        End;
  CheckListBox1.Items.EndUpdate;
  CheckListBox1.ItemIndex := t;
  CheckListBox1.TopIndex := r;
  form2.label1.caption := inttostr(Form2.checklistbox1.items.count) + ' found entries. ' + inttostr(g) + ' seen.';

End;

End.

