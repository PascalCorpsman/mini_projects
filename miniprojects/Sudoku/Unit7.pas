(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Sch�chterle (Corpsman)                                   *)
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
Unit Unit7;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Forms, Classes, Controls, StdCtrls, Dialogs, LResources, SysUtils, Sudoku3x3,
  LCLType;

Type

  { TForm7 }

  TForm7 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox10: TCheckBox;
    GroupBox1: TGroupBox;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    Button3: TButton;
    Button4: TButton;
    Procedure Button2Click(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure FormPaint(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

Var
  Form7: TForm7;

Implementation

Uses
  usudoku
  , unit1
  , Unit6
  ;

{$R *.lfm}

Procedure TForm7.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm7.Button1Click(Sender: TObject);
Label
  Raus, Rein; // Diverse Sprungmarken, es w�rde auch ohne gehen aber das ist hier zu aufwendig
Const
  Numbercount = 9; // Die Anzahl der Nummern die zu anfang in das Feld eingef�gt werden, es m�ssen genug sein damit es jedesmal ein anderes Feld gibt und wenig genug damit der Rechner es auch ja hinbekommt
Var
  n, x, y, z: Integer; // diverse Z�hlvariablen
  Save: Boolean; // wie a nur ebn f�r Try and Error
  f: TSudoku; // Das Field das erzeugt wird, wird gebraucht damit die Unit1 keine Schritte anzeigen kann
  tmpf: T3Field;
  a: Array[1..9] Of Boolean; // Zum Speichern der Solving methoden
  s: String;
  ch: Tcheckbox;
Begin
  // Create New Field
  // es mus mindestens hidden single , oder naked singele activiert sein die anderen methoden f�gen keine Zahlen ein.
  If Checkbox2.checked Or Checkbox1.checked Then Begin
    f := TSudoku.Create(3);
    If Not checkbox2.checked And (checkbox3.checked Or checkbox4.checked Or checkbox5.checked Or checkbox6.checked Or checkbox7.checked Or checkbox8.checked Or checkbox9.checked) Then Begin
      s := '';
      For x := 3 To 9 Do Begin
        ch := Tcheckbox(findcomponent('Checkbox' + inttostr(x)));
        If ch.Checked And ch.Visible Then
          s := s + LineEnding + ch.Caption;
      End;
      If Length(s) <> 0 Then
        If ID_NO = Application.messagebox(pchar('You did not select "by naked single", this will automaticaly disable ' + LineEnding + s + LineEnding + LineEnding + ' would you like to go on anyway ?'), 'question', MB_ICONQUESTION + MB_YESNO) Then Begin
          f.free;
          exit;
        End;
    End;
    If Visible Then
      Visible := false; // Keiner soll die Unit7 von jetzt ab mehr sehn.
    Resetopt; // R�cksetzen der Graphischen Objecte
    zwangsabbruch := false; // Es gibt while schleifen die sind nicht Sicher, dann mus Abgebrochen werden k�nnen.
    Form6.show; // Anzeige Abbrechen Dialog
    Application.ProcessMessages;
    // Erst Wegspeichern aller angekreuzten Solve Technicken aus der Form1
    With form1 Do Begin
      a[1] := byhiddensingle1.checked;
      a[2] := bynakedsingle1.checked;
      a[3] := byBlockandColumninteractions1.checked;
      a[4] := byblockandblockinteractions1.checked;
      a[5] := bynakedsubset1.checked;
      a[6] := byhiddensubset1.checked;
      a[7] := byXWingSwordfish1.checked;
      a[8] := byXYWing1.checked;
      a[9] := ForcingChains1.checked;
    End;
    Rein: // Wenn wir festgestellt haben das die Zuf�llig kreierten Zahlen das L�sen unm�glich machen w�rden
    For x := 0 To 8 Do
      For y := 0 To 8 Do Begin
        Field[x, y].value := 0;
        Field[x, y].Marked := false;
        Field[x, y].Maybeed := false;
        Field[x, y].Fixed := false;
        For z := 0 To 8 Do Begin
          field[x, y].Pencil[z] := false;
        End;
        f.LoadFrom(Field);
      End;
    // --- Neu
   // Drawfield; // L�schen der Anzeige auf der Form 1
   // Daf�r sorgen das wir auf alle F�lle jedesmal eine andere Startposition haben
    For z := 1 To Numbercount Do Begin
      F.ResetAllMarker;
      x := random(9); // Die neuen Koordinaten
      y := random(9); // Die neuen Koordinaten
      n := Random(9) + 1; // Die eingef�gte Zahl
      f.Mark(n); // Markieren der Zahlen
      // Suchen eines Freien Feldes
      While (F.IsMarked(x, y)) Do Begin
        Application.ProcessMessages;
        x := random(9);
        y := random(9);
        If Zwangsabbruch Then Begin
          Goto Raus; // Es ist Versuch jede While Schleife mit dieser Abbruch m�glichkeit zu versehen, man weis ja nie !
        End;
      End;
      F.SetValue(x, y, n, false); // Zuweisen des Feldes
    End;
    // ist das sudoku jetzt schon nicht mehr l�sbar dann neustarten
    f.StoreTo(tmpf);
    If Not (Sudoku3solvable(tmpf)) Then Goto rein;
    // Wegspeichern der by Try Error Methode
    Save := form1.bytryanderror1.checked;
    // Zum erstellen eines Functionierenden Sudoku brauchen wir die Try and Error methode
    form1.bytryanderror1.checked := true;
    // L�sen des Sudoku, mit allem was geht
    With form1 Do Begin
      byhiddensingle1.checked := False;
      bynakedsingle1.checked := False;
      byBlockandColumninteractions1.checked := False;
      byblockandblockinteractions1.checked := False;
      bynakedsubset1.checked := False;
      byhiddensubset1.checked := False;
      byXWingSwordfish1.checked := False;
      byXYWing1.checked := False;
      ForcingChains1.checked := False;
    End;
    f.StoreTo(tmpf);
    Solve(false, True, tmpf);
    If Not isready3(tmpf) And Not Zwangsabbruch Then Begin // Falls unsere try and error methode nichts gefunden hat versuchen wir es nochmal
      Goto rein;
    End;
    f.LoadFrom(tmpf);
    // Zum basteln eines Puzzles mus sie allerdings wieder Raus
    form1.bytryanderror1.checked := False;
    // Basteln des R�tsel's
    With form1 Do Begin
      // Dann Einstellen der Solve Options die benutzt werden d�rfen
      byhiddensingle1.checked := form7.Checkbox1.checked;
      bynakedsingle1.checked := form7.Checkbox2.checked;
      byBlockandColumninteractions1.checked := form7.Checkbox3.checked;
      byblockandblockinteractions1.checked := form7.Checkbox4.checked;
      bynakedsubset1.checked := form7.Checkbox5.checked;
      byhiddensubset1.checked := form7.Checkbox6.checked;
      byXWingSwordfish1.checked := form7.Checkbox7.checked;
      byXYWing1.checked := form7.Checkbox8.checked;
      ForcingChains1.checked := form7.Checkbox9.checked;
    End;
    f.StoreTo(tmpf);
    If checkbox10.checked Then Begin
      HackSudoku(tmpf, random(4))
    End
    Else Begin
      HackSudoku(tmpf);
    End;
    f.LoadFrom(tmpf);
    // Zur�cksetzen der Try Error Methode
    form1.bytryanderror1.checked := Save;
    //  Wieder Setzen der Alten Solve Values
    With form1 Do Begin
      byhiddensingle1.checked := a[1];
      bynakedsingle1.checked := a[2];
      byBlockandColumninteractions1.checked := a[3];
      byblockandblockinteractions1.checked := a[4];
      bynakedsubset1.checked := a[5];
      byhiddensubset1.checked := a[6];
      byXWingSwordfish1.checked := a[7];
      byXYWing1.checked := a[8];
      ForcingChains1.checked := a[9];
    End;
    // Umladen Der Variablen und schaun ob abgebrochen wurde
    Raus:
    If Form6.visible Then Form6.close;
    // Wenn ein Zwangsabbruch war
    If zwangsabbruch Then Begin
      For x := 0 To 8 Do
        For y := 0 To 8 Do Begin
          Field[x, y].value := 0;
          Field[x, y].Marked := false;
          Field[x, y].Maybeed := false;
          Field[x, y].Fixed := false;
          For z := 0 To 8 Do
            Field[x, y].pencil[z] := false;
        End;
    End
    Else Begin // Bei Erfolgreicher suche
      f.StoreTo(tmpf);
      For x := 0 To 8 Do
        For y := 0 To 8 Do Begin
          tmpF[x, y].Marked := false;
          tmpF[x, y].Maybeed := false;
          // Field[x, y].Fixed := F[x, y].value <> 0; -- WTF: Warum geht diese Zuweisung nicht und es muss das If implementiert werden ?
          If tmpF[x, y].value = 0 Then Begin
            tmpF[x, y].Fixed := false;
          End
          Else Begin
            tmpF[x, y].Fixed := true;
          End;
          For z := 0 To 8 Do
            tmpF[x, y].pencil[z] := false;
        End;
      f.LoadFrom(tmpf);
      f.StoreTo(Field);
    End;
    // Ausgabe auf den Monitor
    form1.PaintBox1.Invalidate;
    // Beenden
    f.free;
    Close;
  End
  Else Begin
    showmessage('You have at min to select "by hidden single" or "by naked single".');
    zwangsabbruch := true;
  End;
End;

Procedure TForm7.FormCreate(Sender: TObject);
Begin
  Caption := 'Sudoku ver. : ' + ver + ' New Game';
End;

Procedure TForm7.Button3Click(Sender: TObject);
Var
  x: integer;
Begin
  For x := 1 To 9 Do
    Tcheckbox(findcomponent('Checkbox' + inttostr(x))).checked := true;
End;

Procedure TForm7.Button4Click(Sender: TObject);
Var
  x: integer;
Begin
  For x := 1 To 9 Do
    Tcheckbox(findcomponent('Checkbox' + inttostr(x))).checked := false;
End;

Procedure TForm7.FormPaint(Sender: TObject);
Begin
  button1.setfocus;
End;

End.

