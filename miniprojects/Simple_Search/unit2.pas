(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Simple Search                                         *)
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

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    StringGrid1: TStringGrid;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Edit1Change(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure StringGrid1KeyPress(Sender: TObject; Var Key: char);
    Procedure StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
      Var CanSelect: Boolean);
  private

  public

  End;

Var
  Form2: TForm2;

Implementation

{$R *.lfm}

Uses unit1;

Const
  Index_Checked = 0;
  Index_New_Display = 1;
  Index_Old_Display = 2;
  Index_New_Full = 3;
  Index_Old_Full = 4;
Var
  asrow: Integer = -1;

  { TForm2 }

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  caption := 'Search replace..';
  edit1.text := '';
  edit2.text := '';
End;

Procedure TForm2.StringGrid1KeyPress(Sender: TObject; Var Key: char);
Begin
  If (key = #32) And (asrow > 0) And (asrow < StringGrid1.RowCount) Then Begin
    If StringGrid1.Cells[Index_Checked, asrow] = '1' Then Begin
      StringGrid1.Cells[Index_Checked, asrow] := '0';
    End
    Else Begin
      StringGrid1.Cells[Index_Checked, asrow] := '1';
    End;
  End;
End;

Procedure TForm2.StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
  Var CanSelect: Boolean);
Begin
  asrow := aRow;
End;

Procedure TForm2.Edit1Change(Sender: TObject);
Var
  i: integer;
  SearchPattern, ReplaceWith, fn, fnn, fp: String;
Begin
  StringGrid1.RowCount := 1;
  asrow := -1;
  If edit1.text <> '' Then Begin
    SearchPattern := Edit1.text;
    ReplaceWith := Edit2.Text;
    StringGrid1.BeginUpdate;
    For i := 0 To form1.ListBox1.Items.Count - 1 Do Begin
      If form1.ListBox1.Selected[i] Then Begin
        fn := ExtractFileName(form1.ListBox1.Items[i]);
        If CheckBox1.Checked Then Begin
          fnn := StringReplace(fn, SearchPattern, ReplaceWith, [rfReplaceAll]);
        End
        Else Begin
          fnn := StringReplace(fn, SearchPattern, ReplaceWith, [rfReplaceAll, rfIgnoreCase]);
        End;
        // Es hat sich was geändert
        If fn <> fnn Then Begin
          fp := ExtractFilePath(form1.ListBox1.Items[i]);
          StringGrid1.RowCount := StringGrid1.RowCount + 1;
          StringGrid1.Cells[Index_Checked, StringGrid1.RowCount - 1] := '1';
          StringGrid1.Cells[Index_New_Full, StringGrid1.RowCount - 1] := fp + fnn;
          StringGrid1.Cells[Index_Old_Full, StringGrid1.RowCount - 1] := form1.ListBox1.Items[i];
          If CheckBox2.Checked Then Begin
            // Nur Dateinamen, Pfade Ignorieren
            StringGrid1.Cells[Index_New_Display, StringGrid1.RowCount - 1] := fnn;
            StringGrid1.Cells[Index_Old_Display, StringGrid1.RowCount - 1] := fn;
          End
          Else Begin
            StringGrid1.Cells[Index_New_Display, StringGrid1.RowCount - 1] := fp + fnn;
            StringGrid1.Cells[Index_Old_Display, StringGrid1.RowCount - 1] := form1.ListBox1.Items[i];
          End;
        End;
      End;
    End;
    StringGrid1.EndUpdate;
    StringGrid1.AutoSizeColumns;
  End;
End;

Procedure TForm2.Button1Click(Sender: TObject);
Var
  i: integer;
Begin
  // Apply
  For i := 1 To StringGrid1.RowCount - 1 Do Begin
    If StringGrid1.Cells[Index_Checked, i] = '1' Then Begin
      If Not RenameFile(StringGrid1.Cells[Index_Old_Full, i], StringGrid1.Cells[Index_New_Full, i]) Then Begin
        showmessage('Error could not rename:' + LineEnding +
          StringGrid1.Cells[Index_Old_Full, i] + LineEnding + LineEnding +
          'to' + LineEnding +
          StringGrid1.Cells[Index_New_Full, i]);
      End;
    End;
  End;
  close;
End;

Procedure TForm2.Button2Click(Sender: TObject);
Begin
  // Cancel
  close;
End;

Procedure TForm2.Button3Click(Sender: TObject);
Var
  i: Integer;
Begin
  // Select All
  For i := 1 To StringGrid1.RowCount - 1 Do Begin
    StringGrid1.Cells[Index_Checked, i] := '1';
  End;
End;

Procedure TForm2.Button4Click(Sender: TObject);
Var
  i: Integer;
Begin
  // Select None
  For i := 1 To StringGrid1.RowCount - 1 Do Begin
    StringGrid1.Cells[Index_Checked, i] := '0';
  End;
End;

End.

