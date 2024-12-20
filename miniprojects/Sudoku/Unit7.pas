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

Uses Unit1, Unit6;

{$R *.lfm}

Procedure TForm7.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm7.Button1Click(Sender: TObject);
Label
  Raus, Rein; // Diverse Sprungmarken, es würde auch ohne gehen aber das ist hier zu aufwendig
Const
  Numbercount = 9; // Die Anzahl der Nummern die zu anfang in das Feld eingefügt werden, es müssen genug sein damit es jedesmal ein anderes Feld gibt und wenig genug damit der Rechner es auch ja hinbekommt
Var
  n, x, y, z: Integer; // diverse Zählvariablen
  Save: Boolean; // wie a nur ebn für Try and Error
  f: T3field; // Das Field das erzeugt wird, wird gebraucht damit die Unit1 keine Schritte anzeigen kann
  a: Array[1..9] Of Boolean; // Zum Speichern der Solving methoden
  s: String;
  ch: Tcheckbox;
Begin
  // es mus mindestens hidden single , oder naked singele activiert sein die anderen methoden fügen keine Zahlen ein.
  If Checkbox2.checked Or Checkbox1.checked Then Begin
    If Not checkbox2.checked And (checkbox3.checked Or checkbox4.checked Or checkbox5.checked Or checkbox6.checked Or checkbox7.checked Or checkbox8.checked Or checkbox9.checked) Then Begin
      s := '';
      For x := 3 To 9 Do Begin
        ch := Tcheckbox(findcomponent('Checkbox' + inttostr(x)));
        If ch.Checked And ch.Visible Then
          s := s + #13 + ch.Caption;
      End;
      If Length(s) <> 0 Then
        If ID_NO = Application.messagebox(pchar('You did not select "by naked single", this will automaticaly disable ' + #13 + s + #13#13 + ' would you like to go on anyway ?'), 'question', MB_ICONQUESTION + MB_YESNO) Then
          exit;
    End;
    If Visible Then
      Visible := false; // Keiner soll die Unit7 von jetzt ab mehr sehn.
    Resetopt; // Rücksetzen der Graphischen Objecte
    zwangsabbruch := false; // Es gibt while schleifen die sind nicht Sicher, dann mus Abgebrochen werden können.
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
    Rein: // Wenn wir festgestellt haben das die Zufällig kreierten Zahlen das Lösen unmöglich machen würden
    For x := 0 To 8 Do
      For y := 0 To 8 Do Begin
        Field[x, y].value := 0;
        Field[x, y].Marked := false;
        Field[x, y].Maybeed := false;
        Field[x, y].Fixed := false;
        F[x, y].value := 0;
        F[x, y].Marked := false;
        F[x, y].Maybeed := false;
        F[x, y].Fixed := false;
        For z := 0 To 8 Do Begin
          f[x, y].Pencil[z] := false;
          field[x, y].Pencil[z] := false;
        End;
      End;
    // --- Neu
   // Drawfield; // Löschen der Anzeige auf der Form 1
   // Dafür sorgen das wir auf alle Fälle jedesmal eine andere Startposition haben
    For z := 1 To Numbercount Do Begin
      For x := 0 To 8 Do
        For y := 0 To 8 Do
          F[x, y].Marked := false;
      x := random(9); // Die neuen Koordinaten
      y := random(9); // Die neuen Koordinaten
      n := Random(9) + 1; // Die eingefügte Zahl
      Mark(f, n); // Markieren der Zahlen
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
    // Wegspeichern der by Try Error Methode
    Save := form1.bytryanderror1.checked;
    // Zum erstellen eines Functionierenden Sudoku brauchen wir die Try and Error methode
    form1.bytryanderror1.checked := true;
    // Lösen des Sudoku, mit allem was geht
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
    Solve(false, True, f);
    If Not isready3(f) And Not Zwangsabbruch Then // Falls unsere try and error methode nichts gefunden hat versuchen wir es nochmal
      Goto rein;
    // Zum basteln eines Puzzles mus sie allerdings wieder Raus
    form1.bytryanderror1.checked := False;
    // Basteln des Rätsel's
    With form1 Do Begin
      // Dann Einstellen der Solve Options die benutzt werden dürfen
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
    If checkbox10.checked Then
      HackSudoku(f, random(4))
    Else
      HackSudoku(f);
    // Zurücksetzen der Try Error Methode
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
      For x := 0 To 8 Do
        For y := 0 To 8 Do Begin
          Field[x, y].value := F[x, y].value;
          Field[x, y].Marked := false;
          Field[x, y].Maybeed := false;
          // Field[x, y].Fixed := F[x, y].value <> 0; -- WTF: Warum geht diese Zuweisung nicht und es muss das If implementiert werden ?
          If F[x, y].value = 0 Then Begin
            Field[x, y].Fixed := false;
          End
          Else Begin
            Field[x, y].Fixed := true;
          End;
          For z := 0 To 8 Do
            Field[x, y].pencil[z] := false;
        End;
    End;
    // Ausgabe auf den Monitor
    Drawfield;
    // Beenden
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

