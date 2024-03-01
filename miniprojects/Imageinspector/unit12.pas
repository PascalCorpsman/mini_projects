(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of ImageInspector                                        *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit12;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Grids, uwidgets;

Type

  { TForm12 }

  TForm12 = Class(TForm)
    Button1: TButton;
    StringGrid1: TStringGrid;
    Procedure Button1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private

  public
    Procedure RefreshData;
    Procedure RefreshDataObj(Sender: TMeasureElement);
  End;

Var
  Form12: TForm12;

Implementation

{$R *.lfm}

Uses unit1;

{ TForm12 }

Procedure TForm12.Button1Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm12.FormCreate(Sender: TObject);
Begin
  caption := 'Table of measurements';
End;

Procedure TForm12.RefreshData;
Var
  i: Integer;
Begin
  StringGrid1.BeginUpdate;
  StringGrid1.ColCount := 5;
  StringGrid1.Cells[GridIndexID, 0] := 'ID';
  StringGrid1.Cells[GridIndexObjType, 0] := 'Object-Type';
  StringGrid1.Cells[GridIndexMUnit, 0] := 'Unit of measurement';
  StringGrid1.Cells[GridIndexValue, 0] := 'Value';
  StringGrid1.Cells[GridIndexUnit, 0] := 'Unit';
  StringGrid1.RowCount := 1;
  For i := 0 To high(form1.fMeasureElements) Do Begin
    form1.fMeasureElements[i].AddInfoToStringgrid(StringGrid1, i + 1);
  End;
  StringGrid1.EndUpdate();
  StringGrid1.AutoAdjustColumns;
End;

Procedure TForm12.RefreshDataObj(Sender: TMeasureElement);
Var
  i, j: Integer;
Begin
  For i := 0 To high(form1.fMeasureElements) Do Begin
    If Form1.fMeasureElements[i] = Sender Then Begin
      For j := 1 To StringGrid1.RowCount - 1 Do Begin
        If StringGrid1.Cells[GridIndexID, j] = inttostr(i + 1) Then Begin
          form1.fMeasureElements[i].AddInfoToStringgrid(StringGrid1, i + 1, j);
          exit;
        End;
      End;
    End;
  End;
End;

End.

