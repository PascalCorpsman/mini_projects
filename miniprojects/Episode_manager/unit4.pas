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
Unit unit4;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, StdCtrls, Menus,
  uepisodenmanager, Types;

Const
  IndexRightToLeft = 0;
  IndexNothing = 1;

Type

  { TForm4 }

  TForm4 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    ImageList1: TImageList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PopupMenu1: TPopupMenu;
    StringGrid1: TStringGrid;
    Procedure FormCreate(Sender: TObject);
    Procedure MenuItem1Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure StringGrid1DblClick(Sender: TObject);
    Procedure StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    Procedure StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
      Var CanSelect: Boolean);
  private
  public
    aDirs: Array Of Integer;
    Procedure Clear;
    Procedure Insert(l, r: String);
  End;

Var
  Form4: TForm4;

Implementation

{$R *.lfm}

Var
  aSelectedRow: Integer;

  { TForm4 }

Procedure TForm4.FormCreate(Sender: TObject);
Begin
  caption := 'Diff viewe';
End;

Procedure TForm4.MenuItem1Click(Sender: TObject);
Var
  i: integer;
Begin
  // To Left
  For i := StringGrid1.Selection.Top To StringGrid1.Selection.Bottom Do Begin
    aDirs[i] := IndexRightToLeft;
  End;
  StringGrid1.Invalidate;
End;

Procedure TForm4.MenuItem2Click(Sender: TObject);
Var
  i: integer;
Begin
  // Nothing
  For i := StringGrid1.Selection.Top To StringGrid1.Selection.Bottom Do Begin
    aDirs[i] := IndexNothing;
  End;
  StringGrid1.Invalidate;
End;

Procedure TForm4.StringGrid1DblClick(Sender: TObject);
Begin
  If aSelectedRow = -1 Then exit;
  aDirs[aSelectedRow] := (aDirs[aSelectedRow] + 1) Mod 2;
  StringGrid1.Invalidate;
End;

Procedure TForm4.StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
Begin
  If (acol <> 1) Or (arow < 1) Then exit;
  If aRow > high(aDirs) Then exit;
  ImageList1.Draw(StringGrid1.Canvas,
    (aRect.Left + aRect.Right - 16) Div 2
    , (aRect.Top + aRect.Bottom - 16) Div 2
    , adirs[aRow]);
End;

Procedure TForm4.StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
  Var CanSelect: Boolean);
Begin
  aselectedRow := aRow;
End;

Procedure TForm4.Clear;
Begin
  StringGrid1.Cells[0, 0] := 'Local';
  StringGrid1.Cells[2, 0] := 'Remote';
  StringGrid1.RowCount := 1;
  aDirs := Nil;
  aSelectedRow := -1;
End;

Procedure TForm4.Insert(l, r: String);
Begin
  If l <> '' Then l := TDatabase.Pretty(TDatabase.Decode(l));
  If r <> '' Then r := TDatabase.Pretty(TDatabase.Decode(r));
  StringGrid1.RowCount := StringGrid1.RowCount + 1;
  StringGrid1.Cells[0, StringGrid1.RowCount - 1] := l;
  StringGrid1.Cells[2, StringGrid1.RowCount - 1] := r;
  setlength(aDirs, StringGrid1.RowCount);
  adirs[high(aDirs)] := IndexRightToLeft;
End;

End.

