(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Bitverknuepfungen                                     *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit4;

{$MODE ObjFPC}{$H+}

Interface

Uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, Menus;

Type
  TForm4 = Class(TForm)
    GroupBox1: TGroupBox;
    StringGrid1: TStringGrid;
    Button1: TButton;
    PpForm4: TPopupMenu;
    Colourize1: TMenuItem;
    Procedure Button1Click(Sender: TObject);
    Procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    Procedure Colourize1Click(Sender: TObject);
    Procedure FormResize(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

Var
  Form4: TForm4;

Procedure Drawform4;

Implementation

Uses unit1;

{$R *.lfm}

Procedure DrawForm4;
  Procedure Clear;
  Var
    x, y: Integer;
  Begin
    For x := 0 To form4.stringgrid1.colcount - 1 Do
      For y := 0 To form4.stringgrid1.rowcount - 1 Do
        form4.stringgrid1.cells[x, y] := '';
  End;
Begin
  //  With form4.stringgrid1 Do Begin
  form4.stringgrid1.defaultcolwidth := 64;
  Case High(Variablen) Of
    -1: Begin // Leer
        form4.stringgrid1.Rowcount := 0;
        form4.stringgrid1.colcount := 0;
        form4.stringgrid1.cells[0, 0] := 'Error';
      End;
    0: Begin // Eine Variable !!
        form4.stringgrid1.Rowcount := 5;
        form4.stringgrid1.colcount := 4;
        clear;
        form4.stringgrid1.Cells[1, 2] := form1.StringGrid1.cells[0, 0];
        form4.stringgrid1.Cells[2, 2] := 'not ' + form1.StringGrid1.cells[0, 0];
        form4.stringgrid1.Cells[1, 3] := form1.stringgrid1.cells[1, 2];
        form4.stringgrid1.Cells[2, 3] := form1.stringgrid1.cells[1, 1];
      End;
    1: Begin // Zwei Variable !!
        form4.stringgrid1.Rowcount := 6;
        form4.stringgrid1.colcount := 5;
        clear;
        form4.stringgrid1.Cells[2, 2] := form1.StringGrid1.cells[0, 0];
        form4.stringgrid1.Cells[3, 2] := 'not ' + form1.StringGrid1.cells[0, 0];
        form4.stringgrid1.Cells[1, 3] := form1.StringGrid1.cells[1, 0];
        form4.stringgrid1.Cells[1, 4] := 'not ' + form1.StringGrid1.cells[1, 0];
        form4.stringgrid1.Cells[2, 3] := form1.StringGrid1.cells[2, 4];
        form4.stringgrid1.Cells[3, 3] := form1.StringGrid1.cells[2, 3];
        form4.stringgrid1.Cells[2, 4] := form1.StringGrid1.cells[2, 2];
        form4.stringgrid1.Cells[3, 4] := form1.StringGrid1.cells[2, 1];
      End;
    2: Begin // Drei Variable !!
        form4.stringgrid1.Rowcount := 7;
        form4.stringgrid1.colcount := 7;
        clear;
        form4.stringgrid1.Cells[2, 2] := form1.StringGrid1.cells[0, 0];
        form4.stringgrid1.Cells[3, 2] := form1.StringGrid1.cells[0, 0];
        form4.stringgrid1.Cells[4, 2] := 'not ' + form1.StringGrid1.cells[0, 0];
        form4.stringgrid1.Cells[5, 2] := 'not ' + form1.StringGrid1.cells[0, 0];
        form4.stringgrid1.Cells[1, 3] := form1.StringGrid1.cells[1, 0];
        form4.stringgrid1.Cells[1, 4] := 'not ' + form1.StringGrid1.cells[1, 0];
        form4.stringgrid1.Cells[2, 5] := 'not ' + form1.StringGrid1.cells[2, 0];
        form4.stringgrid1.Cells[3, 5] := form1.StringGrid1.cells[2, 0];
        form4.stringgrid1.Cells[4, 5] := form1.StringGrid1.cells[2, 0];
        form4.stringgrid1.Cells[5, 5] := 'not ' + form1.StringGrid1.cells[2, 0];
        form4.stringgrid1.Cells[5, 4] := form1.StringGrid1.cells[3, 1];
        form4.stringgrid1.Cells[4, 4] := form1.StringGrid1.cells[3, 2];
        form4.stringgrid1.Cells[5, 3] := form1.StringGrid1.cells[3, 3];
        form4.stringgrid1.Cells[4, 3] := form1.StringGrid1.cells[3, 4];
        form4.stringgrid1.Cells[2, 4] := form1.StringGrid1.cells[3, 5];
        form4.stringgrid1.Cells[3, 4] := form1.StringGrid1.cells[3, 6];
        form4.stringgrid1.Cells[2, 3] := form1.StringGrid1.cells[3, 7];
        form4.stringgrid1.Cells[3, 3] := form1.StringGrid1.cells[3, 8];
      End;
    3: Begin // Vier Variable !!
        form4.stringgrid1.Rowcount := 9;
        form4.stringgrid1.colcount := 8;
        clear;
        form4.stringgrid1.Cells[2, 2] := form1.StringGrid1.cells[0, 0];
        form4.stringgrid1.Cells[3, 2] := form1.StringGrid1.cells[0, 0];
        form4.stringgrid1.Cells[4, 2] := 'not ' + form1.StringGrid1.cells[0, 0];
        form4.stringgrid1.Cells[5, 2] := 'not ' + form1.StringGrid1.cells[0, 0];
        form4.stringgrid1.Cells[1, 3] := form1.StringGrid1.cells[1, 0];
        form4.stringgrid1.Cells[1, 4] := form1.StringGrid1.cells[1, 0];
        form4.stringgrid1.Cells[1, 5] := 'not ' + form1.StringGrid1.cells[1, 0];
        form4.stringgrid1.Cells[1, 6] := 'not ' + form1.StringGrid1.cells[1, 0];
        form4.stringgrid1.Cells[3, 7] := form1.StringGrid1.cells[2, 0];
        form4.stringgrid1.Cells[4, 7] := form1.StringGrid1.cells[2, 0];
        form4.stringgrid1.Cells[2, 7] := 'not ' + form1.StringGrid1.cells[2, 0];
        form4.stringgrid1.Cells[5, 7] := 'not ' + form1.StringGrid1.cells[2, 0];
        form4.stringgrid1.cells[6, 3] := 'not ' + form1.StringGrid1.cells[3, 0];
        form4.stringgrid1.cells[6, 6] := 'not ' + form1.StringGrid1.cells[3, 0];
        form4.stringgrid1.cells[6, 4] := form1.StringGrid1.cells[3, 0];
        form4.stringgrid1.cells[6, 5] := form1.StringGrid1.cells[3, 0];
        form4.stringgrid1.Cells[2, 3] := form1.StringGrid1.cells[4, 13];
        form4.stringgrid1.Cells[3, 3] := form1.StringGrid1.cells[4, 15];
        form4.stringgrid1.Cells[4, 3] := form1.StringGrid1.cells[4, 7];
        form4.stringgrid1.Cells[5, 3] := form1.StringGrid1.cells[4, 5];
        form4.stringgrid1.Cells[2, 4] := form1.StringGrid1.cells[4, 14];
        form4.stringgrid1.Cells[3, 4] := form1.StringGrid1.cells[4, 16];
        form4.stringgrid1.Cells[4, 4] := form1.StringGrid1.cells[4, 8];
        form4.stringgrid1.Cells[5, 4] := form1.StringGrid1.cells[4, 6];
        form4.stringgrid1.Cells[2, 5] := form1.StringGrid1.cells[4, 10];
        form4.stringgrid1.Cells[3, 5] := form1.StringGrid1.cells[4, 12];
        form4.stringgrid1.Cells[4, 5] := form1.StringGrid1.cells[4, 4];
        form4.stringgrid1.Cells[5, 5] := form1.StringGrid1.cells[4, 2];
        form4.stringgrid1.Cells[2, 6] := form1.StringGrid1.cells[4, 9];
        form4.stringgrid1.Cells[3, 6] := form1.StringGrid1.cells[4, 11];
        form4.stringgrid1.Cells[4, 6] := form1.StringGrid1.cells[4, 3];
        form4.stringgrid1.Cells[5, 6] := form1.StringGrid1.cells[4, 1];
      End;
    4: Begin // Fünf Variable !!
        form4.stringgrid1.Rowcount := 19;
        form4.stringgrid1.colcount := 9;
        clear;
        form4.stringgrid1.Cells[1, 2] := Form1.stringgrid1.cells[4, 0];
        form4.stringgrid1.Cells[3, 3] := Form1.stringgrid1.cells[0, 0];
        form4.stringgrid1.Cells[4, 3] := Form1.stringgrid1.cells[0, 0];
        form4.stringgrid1.Cells[5, 3] := 'not ' + Form1.stringgrid1.cells[0, 0];
        form4.stringgrid1.Cells[6, 3] := 'not ' + Form1.stringgrid1.cells[0, 0];
        form4.stringgrid1.Cells[2, 4] := Form1.stringgrid1.cells[1, 0];
        form4.stringgrid1.Cells[2, 5] := Form1.stringgrid1.cells[1, 0];
        form4.stringgrid1.Cells[2, 6] := 'not ' + Form1.stringgrid1.cells[1, 0];
        form4.stringgrid1.Cells[2, 7] := 'not ' + Form1.stringgrid1.cells[1, 0];
        form4.stringgrid1.Cells[3, 8] := 'not ' + Form1.stringgrid1.cells[2, 0];
        form4.stringgrid1.Cells[4, 8] := Form1.stringgrid1.cells[2, 0];
        form4.stringgrid1.Cells[5, 8] := Form1.stringgrid1.cells[2, 0];
        form4.stringgrid1.Cells[6, 8] := 'not ' + Form1.stringgrid1.cells[2, 0];
        form4.stringgrid1.Cells[7, 7] := 'not ' + Form1.stringgrid1.cells[3, 0];
        form4.stringgrid1.Cells[7, 4] := 'not ' + Form1.stringgrid1.cells[3, 0];
        form4.stringgrid1.Cells[7, 6] := Form1.stringgrid1.cells[3, 0];
        form4.stringgrid1.Cells[7, 5] := Form1.stringgrid1.cells[3, 0];
        form4.stringgrid1.Cells[1, 10] := 'not ' + Form1.stringgrid1.cells[4, 0];
        form4.stringgrid1.Cells[3, 11] := Form1.stringgrid1.cells[0, 0];
        form4.stringgrid1.Cells[4, 11] := Form1.stringgrid1.cells[0, 0];
        form4.stringgrid1.Cells[5, 11] := 'not ' + Form1.stringgrid1.cells[0, 0];
        form4.stringgrid1.Cells[6, 11] := 'not ' + Form1.stringgrid1.cells[0, 0];
        form4.stringgrid1.Cells[2, 12] := Form1.stringgrid1.cells[1, 0];
        form4.stringgrid1.Cells[2, 13] := Form1.stringgrid1.cells[1, 0];
        form4.stringgrid1.Cells[2, 14] := 'not ' + Form1.stringgrid1.cells[1, 0];
        form4.stringgrid1.Cells[2, 15] := 'not ' + Form1.stringgrid1.cells[1, 0];
        form4.stringgrid1.Cells[3, 16] := 'not ' + Form1.stringgrid1.cells[2, 0];
        form4.stringgrid1.Cells[4, 16] := Form1.stringgrid1.cells[2, 0];
        form4.stringgrid1.Cells[5, 16] := Form1.stringgrid1.cells[2, 0];
        form4.stringgrid1.Cells[6, 16] := 'not ' + Form1.stringgrid1.cells[2, 0];
        form4.stringgrid1.Cells[7, 15] := 'not ' + Form1.stringgrid1.cells[3, 0];
        form4.stringgrid1.Cells[7, 12] := 'not ' + Form1.stringgrid1.cells[3, 0];
        form4.stringgrid1.Cells[7, 14] := Form1.stringgrid1.cells[3, 0];
        form4.stringgrid1.Cells[7, 13] := Form1.stringgrid1.cells[3, 0];
        form4.stringgrid1.Cells[3, 4] := Form1.stringgrid1.cells[5, 26];
        form4.stringgrid1.Cells[4, 4] := Form1.stringgrid1.cells[5, 30];
        form4.stringgrid1.Cells[5, 4] := Form1.stringgrid1.cells[5, 14];
        form4.stringgrid1.Cells[6, 4] := Form1.stringgrid1.cells[5, 10];
        form4.stringgrid1.Cells[3, 5] := Form1.stringgrid1.cells[5, 28];
        form4.stringgrid1.Cells[4, 5] := Form1.stringgrid1.cells[5, 32];
        form4.stringgrid1.Cells[5, 5] := Form1.stringgrid1.cells[5, 16];
        form4.stringgrid1.Cells[6, 5] := Form1.stringgrid1.cells[5, 12];
        form4.stringgrid1.Cells[3, 6] := Form1.stringgrid1.cells[5, 20];
        form4.stringgrid1.Cells[4, 6] := Form1.stringgrid1.cells[5, 24];
        form4.stringgrid1.Cells[5, 6] := Form1.stringgrid1.cells[5, 8];
        form4.stringgrid1.Cells[6, 6] := Form1.stringgrid1.cells[5, 4];
        form4.stringgrid1.Cells[3, 7] := Form1.stringgrid1.cells[5, 18];
        form4.stringgrid1.Cells[4, 7] := Form1.stringgrid1.cells[5, 22];
        form4.stringgrid1.Cells[5, 7] := Form1.stringgrid1.cells[5, 6];
        form4.stringgrid1.Cells[6, 7] := Form1.stringgrid1.cells[5, 2];
        form4.stringgrid1.Cells[3, 12] := Form1.stringgrid1.cells[5, 25];
        form4.stringgrid1.Cells[4, 12] := Form1.stringgrid1.cells[5, 29];
        form4.stringgrid1.Cells[5, 12] := Form1.stringgrid1.cells[5, 13];
        form4.stringgrid1.Cells[6, 12] := Form1.stringgrid1.cells[5, 9];
        form4.stringgrid1.Cells[3, 13] := Form1.stringgrid1.cells[5, 27];
        form4.stringgrid1.Cells[4, 13] := Form1.stringgrid1.cells[5, 31];
        form4.stringgrid1.Cells[5, 13] := Form1.stringgrid1.cells[5, 15];
        form4.stringgrid1.Cells[6, 13] := Form1.stringgrid1.cells[5, 11];
        form4.stringgrid1.Cells[3, 14] := Form1.stringgrid1.cells[5, 19];
        form4.stringgrid1.Cells[4, 14] := Form1.stringgrid1.cells[5, 23];
        form4.stringgrid1.Cells[5, 14] := Form1.stringgrid1.cells[5, 7];
        form4.stringgrid1.Cells[6, 14] := Form1.stringgrid1.cells[5, 3];
        form4.stringgrid1.Cells[3, 15] := Form1.stringgrid1.cells[5, 17];
        form4.stringgrid1.Cells[4, 15] := Form1.stringgrid1.cells[5, 21];
        form4.stringgrid1.Cells[5, 15] := Form1.stringgrid1.cells[5, 5];
        form4.stringgrid1.Cells[6, 15] := Form1.stringgrid1.cells[5, 1];
      End;
    5: Begin // Sechs Variablen
        form4.stringgrid1.Rowcount := 19;
        form4.stringgrid1.colcount := 17;
        clear;
        form4.stringgrid1.Cells[1, 2] := Form1.stringgrid1.cells[4, 0] + ' and ' + Form1.stringgrid1.cells[5, 0];
        form4.stringgrid1.Cells[3, 3] := Form1.stringgrid1.cells[0, 0];
        form4.stringgrid1.Cells[4, 3] := Form1.stringgrid1.cells[0, 0];
        form4.stringgrid1.Cells[5, 3] := 'not ' + Form1.stringgrid1.cells[0, 0];
        form4.stringgrid1.Cells[6, 3] := 'not ' + Form1.stringgrid1.cells[0, 0];
        form4.stringgrid1.Cells[2, 4] := Form1.stringgrid1.cells[1, 0];
        form4.stringgrid1.Cells[2, 5] := Form1.stringgrid1.cells[1, 0];
        form4.stringgrid1.Cells[2, 6] := 'not ' + Form1.stringgrid1.cells[1, 0];
        form4.stringgrid1.Cells[2, 7] := 'not ' + Form1.stringgrid1.cells[1, 0];
        form4.stringgrid1.Cells[3, 8] := 'not ' + Form1.stringgrid1.cells[2, 0];
        form4.stringgrid1.Cells[4, 8] := Form1.stringgrid1.cells[2, 0];
        form4.stringgrid1.Cells[5, 8] := Form1.stringgrid1.cells[2, 0];
        form4.stringgrid1.Cells[6, 8] := 'not ' + Form1.stringgrid1.cells[2, 0];
        form4.stringgrid1.Cells[7, 7] := 'not ' + Form1.stringgrid1.cells[3, 0];
        form4.stringgrid1.Cells[7, 4] := 'not ' + Form1.stringgrid1.cells[3, 0];
        form4.stringgrid1.Cells[7, 6] := Form1.stringgrid1.cells[3, 0];
        form4.stringgrid1.Cells[7, 5] := Form1.stringgrid1.cells[3, 0];
        form4.stringgrid1.Cells[1, 10] := Form1.stringgrid1.cells[4, 0] + ' and not ' + Form1.stringgrid1.cells[5, 0];
        form4.stringgrid1.Cells[3, 11] := Form1.stringgrid1.cells[0, 0];
        form4.stringgrid1.Cells[4, 11] := Form1.stringgrid1.cells[0, 0];
        form4.stringgrid1.Cells[5, 11] := 'not ' + Form1.stringgrid1.cells[0, 0];
        form4.stringgrid1.Cells[6, 11] := 'not ' + Form1.stringgrid1.cells[0, 0];
        form4.stringgrid1.Cells[2, 12] := Form1.stringgrid1.cells[1, 0];
        form4.stringgrid1.Cells[2, 13] := Form1.stringgrid1.cells[1, 0];
        form4.stringgrid1.Cells[2, 14] := 'not ' + Form1.stringgrid1.cells[1, 0];
        form4.stringgrid1.Cells[2, 15] := 'not ' + Form1.stringgrid1.cells[1, 0];
        form4.stringgrid1.Cells[3, 16] := 'not ' + Form1.stringgrid1.cells[2, 0];
        form4.stringgrid1.Cells[4, 16] := Form1.stringgrid1.cells[2, 0];
        form4.stringgrid1.Cells[5, 16] := Form1.stringgrid1.cells[2, 0];
        form4.stringgrid1.Cells[6, 16] := 'not ' + Form1.stringgrid1.cells[2, 0];
        form4.stringgrid1.Cells[7, 15] := 'not ' + Form1.stringgrid1.cells[3, 0];
        form4.stringgrid1.Cells[7, 12] := 'not ' + Form1.stringgrid1.cells[3, 0];
        form4.stringgrid1.Cells[9, 3] := 'not ' + Form1.stringgrid1.cells[4, 0] + ' and ' + Form1.stringgrid1.cells[5, 0];
        form4.stringgrid1.Cells[11, 3] := Form1.stringgrid1.cells[0, 0];
        form4.stringgrid1.Cells[12, 3] := Form1.stringgrid1.cells[0, 0];
        form4.stringgrid1.Cells[13, 3] := 'not ' + Form1.stringgrid1.cells[0, 0];
        form4.stringgrid1.Cells[14, 3] := 'not ' + Form1.stringgrid1.cells[0, 0];
        form4.stringgrid1.Cells[10, 4] := Form1.stringgrid1.cells[1, 0];
        form4.stringgrid1.Cells[10, 5] := Form1.stringgrid1.cells[1, 0];
        form4.stringgrid1.Cells[10, 6] := 'not ' + Form1.stringgrid1.cells[1, 0];
        form4.stringgrid1.Cells[10, 7] := 'not ' + Form1.stringgrid1.cells[1, 0];
        form4.stringgrid1.Cells[11, 8] := 'not ' + Form1.stringgrid1.cells[2, 0];
        form4.stringgrid1.Cells[12, 8] := Form1.stringgrid1.cells[2, 0];
        form4.stringgrid1.Cells[13, 8] := Form1.stringgrid1.cells[2, 0];
        form4.stringgrid1.Cells[14, 8] := 'not ' + Form1.stringgrid1.cells[2, 0];
        form4.stringgrid1.Cells[15, 7] := 'not ' + Form1.stringgrid1.cells[3, 0];
        form4.stringgrid1.Cells[15, 4] := 'not ' + Form1.stringgrid1.cells[3, 0];
        form4.stringgrid1.Cells[15, 6] := Form1.stringgrid1.cells[3, 0];
        form4.stringgrid1.Cells[15, 5] := Form1.stringgrid1.cells[3, 0];
        form4.stringgrid1.Cells[9, 10] := 'not ' + Form1.stringgrid1.cells[4, 0] + ' and not ' + Form1.stringgrid1.cells[5, 0];
        form4.stringgrid1.Cells[11, 11] := Form1.stringgrid1.cells[0, 0];
        form4.stringgrid1.Cells[12, 11] := Form1.stringgrid1.cells[0, 0];
        form4.stringgrid1.Cells[13, 11] := 'not ' + Form1.stringgrid1.cells[0, 0];
        form4.stringgrid1.Cells[14, 11] := 'not ' + Form1.stringgrid1.cells[0, 0];
        form4.stringgrid1.Cells[10, 12] := Form1.stringgrid1.cells[1, 0];
        form4.stringgrid1.Cells[10, 13] := Form1.stringgrid1.cells[1, 0];
        form4.stringgrid1.Cells[10, 14] := 'not ' + Form1.stringgrid1.cells[1, 0];
        form4.stringgrid1.Cells[10, 15] := 'not ' + Form1.stringgrid1.cells[1, 0];
        form4.stringgrid1.Cells[11, 16] := 'not ' + Form1.stringgrid1.cells[2, 0];
        form4.stringgrid1.Cells[12, 16] := Form1.stringgrid1.cells[2, 0];
        form4.stringgrid1.Cells[13, 16] := Form1.stringgrid1.cells[2, 0];
        form4.stringgrid1.Cells[14, 16] := 'not ' + Form1.stringgrid1.cells[2, 0];
        form4.stringgrid1.Cells[15, 15] := 'not ' + Form1.stringgrid1.cells[3, 0];
        form4.stringgrid1.Cells[15, 12] := 'not ' + Form1.stringgrid1.cells[3, 0];
        form4.stringgrid1.Cells[15, 13] := Form1.stringgrid1.cells[3, 0];
        form4.stringgrid1.Cells[15, 14] := Form1.stringgrid1.cells[3, 0];
        // ausfüllen der Einzelzellen
        form4.stringgrid1.Cells[3, 4] := form1.StringGrid1.cells[6, 52];
        form4.stringgrid1.Cells[4, 4] := form1.StringGrid1.cells[6, 60];
        form4.stringgrid1.Cells[5, 4] := form1.StringGrid1.cells[6, 28];
        form4.stringgrid1.Cells[6, 4] := form1.StringGrid1.cells[6, 20];
        form4.stringgrid1.Cells[3, 5] := form1.StringGrid1.cells[6, 56];
        form4.stringgrid1.Cells[4, 5] := form1.StringGrid1.cells[6, 64];
        form4.stringgrid1.Cells[5, 5] := form1.StringGrid1.cells[6, 32];
        form4.stringgrid1.Cells[6, 5] := form1.StringGrid1.cells[6, 24];
        form4.stringgrid1.Cells[3, 6] := form1.StringGrid1.cells[6, 40];
        form4.stringgrid1.Cells[4, 6] := form1.StringGrid1.cells[6, 48];
        form4.stringgrid1.Cells[5, 6] := form1.StringGrid1.cells[6, 16];
        form4.stringgrid1.Cells[6, 6] := form1.StringGrid1.cells[6, 8];
        form4.stringgrid1.Cells[3, 7] := form1.StringGrid1.cells[6, 36];
        form4.stringgrid1.Cells[4, 7] := form1.StringGrid1.cells[6, 44];
        form4.stringgrid1.Cells[5, 7] := form1.StringGrid1.cells[6, 12];
        form4.stringgrid1.Cells[6, 7] := form1.StringGrid1.cells[6, 4];
        form4.stringgrid1.Cells[3, 12] := form1.StringGrid1.cells[6, 51];
        form4.stringgrid1.Cells[4, 12] := form1.StringGrid1.cells[6, 59];
        form4.stringgrid1.Cells[5, 12] := form1.StringGrid1.cells[6, 27];
        form4.stringgrid1.Cells[6, 12] := form1.StringGrid1.cells[6, 19];
        form4.stringgrid1.Cells[3, 13] := form1.StringGrid1.cells[6, 55];
        form4.stringgrid1.Cells[4, 13] := form1.StringGrid1.cells[6, 63];
        form4.stringgrid1.Cells[5, 13] := form1.StringGrid1.cells[6, 31];
        form4.stringgrid1.Cells[6, 13] := form1.StringGrid1.cells[6, 23];
        form4.stringgrid1.Cells[3, 14] := form1.StringGrid1.cells[6, 39];
        form4.stringgrid1.Cells[4, 14] := form1.StringGrid1.cells[6, 47];
        form4.stringgrid1.Cells[5, 14] := form1.StringGrid1.cells[6, 15];
        form4.stringgrid1.Cells[6, 14] := form1.StringGrid1.cells[6, 7];
        form4.stringgrid1.Cells[3, 15] := form1.StringGrid1.cells[6, 35];
        form4.stringgrid1.Cells[4, 15] := form1.StringGrid1.cells[6, 43];
        form4.stringgrid1.Cells[5, 15] := form1.StringGrid1.cells[6, 11];
        form4.stringgrid1.Cells[6, 15] := form1.StringGrid1.cells[6, 3];
        form4.stringgrid1.Cells[11, 4] := form1.StringGrid1.cells[6, 50];
        form4.stringgrid1.Cells[12, 4] := form1.StringGrid1.cells[6, 58];
        form4.stringgrid1.Cells[13, 4] := form1.StringGrid1.cells[6, 26];
        form4.stringgrid1.Cells[14, 4] := form1.StringGrid1.cells[6, 18];
        form4.stringgrid1.Cells[11, 5] := form1.StringGrid1.cells[6, 54];
        form4.stringgrid1.Cells[12, 5] := form1.StringGrid1.cells[6, 62];
        form4.stringgrid1.Cells[13, 5] := form1.StringGrid1.cells[6, 30];
        form4.stringgrid1.Cells[14, 5] := form1.StringGrid1.cells[6, 22];
        form4.stringgrid1.Cells[11, 6] := form1.StringGrid1.cells[6, 38];
        form4.stringgrid1.Cells[12, 6] := form1.StringGrid1.cells[6, 46];
        form4.stringgrid1.Cells[13, 6] := form1.StringGrid1.cells[6, 14];
        form4.stringgrid1.Cells[14, 6] := form1.StringGrid1.cells[6, 6];
        form4.stringgrid1.Cells[11, 7] := form1.StringGrid1.cells[6, 34];
        form4.stringgrid1.Cells[12, 7] := form1.StringGrid1.cells[6, 42];
        form4.stringgrid1.Cells[13, 7] := form1.StringGrid1.cells[6, 10];
        form4.stringgrid1.Cells[14, 7] := form1.StringGrid1.cells[6, 2];
        form4.stringgrid1.Cells[11, 12] := form1.StringGrid1.cells[6, 49];
        form4.stringgrid1.Cells[12, 12] := form1.StringGrid1.cells[6, 57];
        form4.stringgrid1.Cells[13, 12] := form1.StringGrid1.cells[6, 25];
        form4.stringgrid1.Cells[14, 12] := form1.StringGrid1.cells[6, 17];
        form4.stringgrid1.Cells[11, 13] := form1.StringGrid1.cells[6, 53];
        form4.stringgrid1.Cells[12, 13] := form1.StringGrid1.cells[6, 61];
        form4.stringgrid1.Cells[13, 13] := form1.StringGrid1.cells[6, 29];
        form4.stringgrid1.Cells[14, 13] := form1.StringGrid1.cells[6, 21];
        form4.stringgrid1.Cells[11, 14] := form1.StringGrid1.cells[6, 37];
        form4.stringgrid1.Cells[12, 14] := form1.StringGrid1.cells[6, 45];
        form4.stringgrid1.Cells[13, 14] := form1.StringGrid1.cells[6, 13];
        form4.stringgrid1.Cells[14, 14] := form1.StringGrid1.cells[6, 5];
        form4.stringgrid1.Cells[11, 15] := form1.StringGrid1.cells[6, 33];
        form4.stringgrid1.Cells[12, 15] := form1.StringGrid1.cells[6, 41];
        form4.stringgrid1.Cells[13, 15] := form1.StringGrid1.cells[6, 9];
        form4.stringgrid1.Cells[14, 15] := form1.StringGrid1.cells[6, 1];
      End;
  End;
  Case High(Variablen) Of
    0..5: Begin
        form4.stringgrid1.FixedRows := 1;
      End;
  End;
End;

Procedure TForm4.Button1Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm4.StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
Var
  sg: TStringgrid;
Begin
  sg := TStringGrid(Sender);
  sg.canvas.Brush.Color := clbtnface;
  sg.canvas.font.Color := clblack;
  If Colourize1.checked Then Begin
    If sg.Cells[acol, arow] = '0' Then sg.canvas.brush.color := clred;
    If sg.Cells[acol, arow] = '1' Then sg.canvas.brush.color := clgreen;
  End;
  sg.canvas.FillRect(Rect);
  sg.canvas.TextRect(Rect, Rect.Left + 2, Rect.Top + 2, sg.Cells[aCol, aRow]);
End;

Procedure TForm4.Colourize1Click(Sender: TObject);
Begin
  Colourize1.checked := Not Colourize1.checked;
  stringgrid1.Repaint;
End;

Procedure TForm4.FormResize(Sender: TObject);
Begin
  Form4.button1.left := (Form4.width - form4.button1.width) Div 2;
End;

End.

