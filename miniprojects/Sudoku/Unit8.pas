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
Unit Unit8;

{$MODE ObjFPC}{$H+}

Interface

Uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Grids, StdCtrls, LResources, Sudoku3x3;

Type
  TForm8 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    StringGrid1: TStringGrid;
    Procedure StringGrid1KeyPress(Sender: TObject; Var Key: Char);
    Procedure StringGrid1KeyUp(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure Button2Click(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

Var
  Form8: TForm8;

Implementation

Uses Unit1, Unit6;

{$R *.lfm}

Procedure TForm8.StringGrid1KeyPress(Sender: TObject; Var Key: Char);
Begin
  If Not (Key In [#32, #8, 'X', 'x']) Then key := #0;
  If Key = 'x' Then key := 'X';
End;

Procedure TForm8.StringGrid1KeyUp(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Var
  x, y: integer;
Begin
  For x := 0 To 8 Do
    For y := 0 To 8 Do
      If length(Stringgrid1.cells[x, y]) = 2 Then Begin
        Stringgrid1.cells[x, y] := Stringgrid1.cells[x, y][2];
      End;
End;

Procedure TForm8.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm8.Button1Click(Sender: TObject);
Label
  Raus, rein;
Const
  Numbercount = 9;
Var
  f: T3field;
  x, y, z, n: integer;
  geschafft: Boolean;
  //  save: boolean;
Begin
  For x := 0 To 8 Do
    For y := 0 To 8 Do Begin
      f[x, y].value := 0;
    End;
  zwangsabbruch := false; // Es gibt while schleifen die sind nicht Sicher, dann mus Abgebrochen werden können.
  Form6.show; // Anzeige Abbrechen Dialog
  geschafft := false;
  // Wegspeichern der by Try Error Methode
//  Save := form1.bytryanderror1.checked;
  // Zum erstellen eines Functionierenden Sudoku brauchen wir die Try and Error methode
  form1.bytryanderror1.checked := true;
  // Lösen des Sudoku, mit allem was geht
  With form1 Do Begin
    byhiddensingle1.checked := true;
    bynakedsingle1.checked := true;
    byBlockandColumninteractions1.checked := true;
    byblockandblockinteractions1.checked := true;
    bynakedsubset1.checked := true;
    byhiddensubset1.checked := true;
    byXWingSwordfish1.checked := true;
    byXYWing1.checked := true;
    ForcingChains1.checked := true;
  End;
  While (Not geschafft) And (Not zwangsabbruch) Do Begin
    rein:
    For x := 0 To 8 Do
      For y := 0 To 8 Do Begin
        f[x, y].value := 0;
      End;

    // Dafür sorgen das wir auf alle Fälle jedesmal eine andere Startposition haben
    For z := 1 To Numbercount Do Begin
      For x := 0 To 8 Do
        For y := 0 To 8 Do
          F[x, y].Marked := false;
      x := random(9); // Die neuen Koordinaten
      y := random(9); // Die neuen Koordinaten
      n := Random(9) + 1; // Die eingefügte Zahl
      Mark( f,n); // Markieren der Zahlen
      // Suchen eines Freien Feldes
      While (F[x, y].value <> 0) Or (f[x, y].Marked) Do Begin
        Application.ProcessMessages;
        x := random(9);
        y := random(9);
        If Zwangsabbruch Then Begin
          Goto Raus; // Es ist Versuch jede While Schleife mit dieser Abbruch möglichkeit zu versehen, man weis ja nie !
        End;
      End;
      F[x, y].value := n; // Zuweisen des Feldes
    End;

    // ist das sudoku jetzt schon nicht mehr lösbar dann neustarten
    If Not (Sudoku3solvable(f)) Then Goto rein;

    form1.bytryanderror1.checked := true;
    // Lösen
    Solve(false, True, f);
    For x := 0 To 8 Do
      For y := 0 To 8 Do
        If Not (StringGrid1.cells[x, y] = 'X') Then f[x, y].value := 0;
    Goto raus;
    form1.bytryanderror1.checked := false;
    // Lösen
    Solve(false, True, f);
    If isready3(f) Then geschafft := true;
  End;

    If Geschafft Then Begin
  raus:
  //Begin
    form6.Close;
    For x := 0 To 8 Do
      For y := 0 To 8 Do Begin
        field[x, y].value := f[x, y].value;
        field[x, y].Marked := false;
        field[x, y].Fixed := Not (field[x, y].value = 0);
        field[x, y].Maybeed := false;
        For z := 0 To 8 Do
          field[x, y].Pencil[z] := false;
      End;
    drawfield;
    close;

  End;

End;

Procedure TForm8.FormCreate(Sender: TObject);
Begin
  stringgrid1.cells[2, 0] := 'X';
  stringgrid1.cells[1, 1] := 'X';
  stringgrid1.cells[0, 2] := 'X';
  stringgrid1.cells[6, 0] := 'X';
  stringgrid1.cells[7, 1] := 'X';
  stringgrid1.cells[8, 2] := 'X';
  stringgrid1.cells[4, 1] := 'X';
  stringgrid1.cells[3, 2] := 'X';
  stringgrid1.cells[5, 2] := 'X';
  stringgrid1.cells[2, 3] := 'X';
  stringgrid1.cells[4, 3] := 'X';
  stringgrid1.cells[6, 3] := 'X';
  stringgrid1.cells[1, 4] := 'X';
  stringgrid1.cells[3, 4] := 'X';
  stringgrid1.cells[5, 4] := 'X';
  stringgrid1.cells[7, 4] := 'X';
  stringgrid1.cells[2, 5] := 'X';
  stringgrid1.cells[4, 5] := 'X';
  stringgrid1.cells[6, 5] := 'X';
  stringgrid1.cells[0, 6] := 'X';
  stringgrid1.cells[3, 6] := 'X';
  stringgrid1.cells[5, 6] := 'X';
  stringgrid1.cells[8, 6] := 'X';
  stringgrid1.cells[1, 7] := 'X';
  stringgrid1.cells[4, 7] := 'X';
  stringgrid1.cells[7, 7] := 'X';
  stringgrid1.cells[2, 8] := 'X';
  stringgrid1.cells[6, 8] := 'X';
End;

End.

