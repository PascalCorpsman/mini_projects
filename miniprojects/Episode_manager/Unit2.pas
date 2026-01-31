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
  SysUtils, Classes, Forms, StdCtrls, CheckLst, Dialogs, Controls, Menus;

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
    Procedure CheckListBox1DblClick(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    Procedure DBToLCL;
  End;

Var
  Form2: TForm2;

Implementation

{$R *.lfm}

Uses
  Unit1
  , unit5
  , uepisodenmanager;

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  caption := 'Found entries';
  Constraints.MinWidth := 500;
  Constraints.MinHeight := 220;
End;

Procedure TForm2.CheckListBox1DblClick(Sender: TObject);
Var
  s: String;
  d, nd: TDatensatz;
  r, t: Integer;
Begin
  s := CheckListBox1.Items[CheckListBox1.ItemIndex];
  d := TDatabase.PrettyToDatensatz(s);
  form5.Init(d);
  Case form5.ShowModal Of
    mrOK: Begin
        nd := form5.lclToDatensatz;
        Database.ReplaceWith(d, nd);
        t := CheckListBox1.ItemIndex;
        r := CheckListBox1.TopIndex;
        DBToLCL;
        CheckListBox1.ItemIndex := t;
        CheckListBox1.TopIndex := r;
      End;
    mrNo: Begin // Wird zweckentfremdet für "delete"
        nd := form5.lclToDatensatz;
        If Not Database.DeleteDataset(nd) Then Begin
          showmessage('Error, unable to delete dataset.');
          exit;
        End;
        t := CheckListBox1.ItemIndex;
        r := CheckListBox1.TopIndex;
        DBToLCL;
        CheckListBox1.ItemIndex := t;
        CheckListBox1.TopIndex := r;
      End;
  End;
End;

Procedure TForm2.Button5Click(Sender: TObject);
Begin
  close;
End;

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
  r, t, i: integer;
Begin
  t := CheckListBox1.ItemIndex;
  r := CheckListBox1.TopIndex;
  l := TStringList.create;
  l.clear;
  For i := 0 To Checklistbox1.items.count - 1 Do
    If Checklistbox1.Checked[i] Then
      l.add(Checklistbox1.Items[i]);
  Database.MarkWithValue(l, true);
  l.free;
  DBToLCL;
  CheckListBox1.ItemIndex := t;
  CheckListBox1.TopIndex := r;
End;

Procedure TForm2.Button2Click(Sender: TObject);
Var
  l: Tstringlist;
  r, t, i: integer;
Begin
  t := CheckListBox1.ItemIndex;
  r := CheckListBox1.TopIndex;
  l := TStringList.create;
  l.clear;
  For i := 0 To Checklistbox1.items.count - 1 Do
    If Checklistbox1.Checked[i] Then
      l.add(Checklistbox1.Items[i]);
  Database.MarkWithValue(l, false);
  l.free;
  DBToLCL;
  CheckListBox1.ItemIndex := t;
  CheckListBox1.TopIndex := r;
End;

Procedure TForm2.DBToLCL;
Var
  g, i, j, k: Integer;
  l: TStringList;
Begin
  // Neu Laden
  g := 0;
  CheckListBox1.Items.BeginUpdate;
  CheckListBox1.clear;
  For i := 0 To form1.Checklistbox1.Items.count - 1 Do
    If form1.Checklistbox1.Checked[i] Then
      For j := 0 To form1.Checklistbox2.Items.count - 1 Do
        If form1.Checklistbox2.Checked[j] Then Begin
          l := Database.gebeAlleDatensaetzemit(form1.Checklistbox1.Items[i], form1.Checklistbox2.Items[j], true);
          For k := 0 To l.count - 1 Do Begin
            // Extrahieren des Zählers für Gesehen
            If pos('yes', copy(l[k], pos('|', l[k]) - 6, 6)) <> 0 Then inc(g);
            CheckListBox1.items.add(l[k]);
            CheckListBox1.checked[CheckListBox1.items.count - 1] := false;
          End;
          l.free;
        End;
  CheckListBox1.Items.EndUpdate;
  label1.caption := inttostr(checklistbox1.items.count) + ' found entries. ' + inttostr(g) + ' seen.';
End;

End.

