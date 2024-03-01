(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Parken                                                *)
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

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  math;

Type

  { TForm5 }

  TForm5 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ListBox1: TListBox;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    OpenDialog3: TOpenDialog;
    OpenDialog4: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure Button8Click(Sender: TObject);
    Procedure Button9Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  End;

Var
  Form5: TForm5;

Implementation

Uses lazutf8;

{$R *.lfm}

{ TForm5 }

Procedure TForm5.FormCreate(Sender: TObject);
Begin
  caption := 'Quest properties';
  label4.caption := '';
  label5.caption := '';
  ListBox1.clear;
  OpenDialog1.InitialDir := ExtractFilePath(paramstr(0));
  OpenDialog2.InitialDir := ExtractFilePath(paramstr(0));
  OpenDialog3.InitialDir := ExtractFilePath(paramstr(0));
  OpenDialog4.InitialDir := ExtractFilePath(paramstr(0));
  saveDialog1.InitialDir := ExtractFilePath(paramstr(0));
  // Größe des Fensters Festlegen
  Constraints.MinHeight := height;
  Constraints.MaxHeight := height;
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
End;

Procedure TForm5.Button2Click(Sender: TObject);
Begin
  close;
End;


Procedure TForm5.Button3Click(Sender: TObject);
Var
  f: Textfile;
  i: Integer;
  s: String;
Begin
  If OpenDialog1.Execute Then Begin
    assignfile(f, OpenDialog1.FileName);
    reset(f);
    readln(f, s);
    label4.caption := s;
    readln(f, s);
    listbox1.clear;
    For i := 0 To strtoint(s) - 1 Do Begin
      readln(f, s);
      listbox1.items.add(s);
    End;
    readln(f, s);
    label5.caption := s;
    closefile(f);
  End;
End;

Procedure TForm5.Button4Click(Sender: TObject);
Var
  f: Textfile;
  i: Integer;
Begin
  If SaveDialog1.Execute Then Begin
    assignfile(f, SaveDialog1.FileName);
    rewrite(f);
    writeln(f, Label4.caption);
    writeln(f, inttostr(ListBox1.Items.Count));
    For i := 0 To ListBox1.Items.Count - 1 Do Begin
      writeln(f, ListBox1.Items[i]);
    End;
    writeln(f, Label5.caption);
    closefile(f);
  End;
End;

Procedure TForm5.Button5Click(Sender: TObject);
Var
  i: Integer;
Begin
  If listbox1.itemindex > 0 Then Begin
    i := listbox1.ItemIndex;
    listbox1.Items.Exchange(i, i - 1);
    listbox1.ItemIndex := i - 1;
  End;
End;

Procedure TForm5.Button6Click(Sender: TObject);
Begin
  If OpenDialog3.Execute Then Begin
    Listbox1.items.add(ExtractFileName(systoutf8(OpenDialog3.FileName)));
  End;
End;

Procedure TForm5.Button7Click(Sender: TObject);
Var
  i: Integer;
Begin
  If Listbox1.itemindex <> -1 Then Begin
    i := Listbox1.itemindex;
    Listbox1.items.Delete(i);
    i := min(i, listbox1.items.count - 1);
    If i >= 0 Then
      Listbox1.itemindex := i;
  End;
End;

Procedure TForm5.Button8Click(Sender: TObject);
Var
  i: Integer;
Begin
  If listbox1.itemindex < listbox1.items.count - 1 Then Begin
    i := listbox1.ItemIndex;
    listbox1.Items.Exchange(i, i + 1);
    listbox1.ItemIndex := i + 1;
  End;
End;

Procedure TForm5.Button9Click(Sender: TObject);
Begin
  If OpenDialog4.Execute Then Begin
    label5.caption := ExtractFileName(systoutf8(OpenDialog4.FileName));
  End;
End;

Procedure TForm5.Button1Click(Sender: TObject);
Begin
  If OpenDialog2.Execute Then Begin
    Label4.caption := ExtractFileName(systoutf8(OpenDialog2.FileName));
  End;
End;

End.

