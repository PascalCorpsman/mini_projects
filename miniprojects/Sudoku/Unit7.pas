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
  Forms, Classes, Controls, StdCtrls, Dialogs, LResources, SysUtils,
  LCLType, usudoku;

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
    Procedure FormDestroy(Sender: TObject);
    Procedure FormPaint(Sender: TObject);
  private
    { Private-Deklarationen }
    fField: TSudoku;
    fRepaintEvent: TNotifyEvent;
    fOptions: TSolveOptions;
    Function OnLCLUpdateEvent: Boolean;
  public
    { Public-Deklarationen }
    Property Sudoku: TSudoku read fField;
    Procedure Init(Const aField: TSudoku; RepaintEvent: TNotifyEvent;
      aOptions: TSolveOptions);
  End;

Var
  Form7: TForm7;

Implementation

Uses
  unit1, // Es muss nur noch das HackSudoku raus, dann kann die Abhängigkeit zu unit1 auch gelöscht werden ;)
  Unit6
  ;

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
  f: TSudoku; // Das Field das erzeugt wird, wird gebraucht damit die Unit1 keine Schritte anzeigen kann
  Field, tmpf: T3Field;
  s: String;
  ch: Tcheckbox;
Begin
  // Create New Field
  // es mus mindestens hidden single , oder naked singele activiert sein die anderen methoden fügen keine Zahlen ein.
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
    zwangsabbruch := false; // Es gibt while schleifen die sind nicht Sicher, dann mus Abgebrochen werden können.
    Form6.show; // Anzeige Abbrechen Dialog
    Application.ProcessMessages;
    Rein: // Wenn wir festgestellt haben das die Zufällig kreierten Zahlen das Lösen unmöglich machen würden
    For x := 0 To 8 Do Begin
      For y := 0 To 8 Do Begin
        Field[x, y].value := 0;
        Field[x, y].Marked := false;
        Field[x, y].Maybeed := false;
        Field[x, y].Fixed := false;
        For z := 0 To 8 Do Begin
          field[x, y].Pencil[z] := false;
        End;
      End;
    End;
    f.LoadFrom(Field);
    // Dafür sorgen das wir auf alle Fälle jedesmal eine andere Startposition haben
    For z := 1 To Numbercount Do Begin
      F.ResetAllMarkerAndPencils;
      x := random(9); // Die neuen Koordinaten
      y := random(9); // Die neuen Koordinaten
      n := Random(9) + 1; // Die eingefügte Zahl
      f.Mark(n); // Markieren der Zahlen
      // Suchen eines Freien Feldes
      While (F.IsMarked(x, y)) Do Begin
        Application.ProcessMessages;
        x := random(9);
        y := random(9);
        If Zwangsabbruch Then Begin
          Goto Raus; // Es ist Versuch jede While Schleife mit dieser Abbruch möglichkeit zu versehen, man weis ja nie !
        End;
      End;
      F.SetValue(x, y, n, false); // Zuweisen des Feldes
    End;
    // ist das sudoku jetzt schon nicht mehr lösbar dann neustarten
    If Not (f.IsSolveable) Then Goto rein;
    f.solve(false, AllSolveOptions, @OnLCLUpdateEvent);
    If Not f.IsFullyFilled() And Not Zwangsabbruch Then Begin // Falls unsere try and error methode nichts gefunden hat versuchen wir es nochmal
      Goto rein;
    End;
    f.StoreTo(tmpf);
    If checkbox10.checked Then Begin
      form1.HackSudoku(tmpf, random(4))
    End
    Else Begin
      form1.HackSudoku(tmpf);
    End;
    f.LoadFrom(tmpf);
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
      fField.CloneFieldFrom(f);
    End;
    // Ausgabe auf den Monitor
    If assigned(fRepaintEvent) Then fRepaintEvent(Nil); // TODO: das hier sollte eigentlich dauerhaft raus können ..
    // Beenden
    f.free;
    Close;
  End
  Else Begin
    showmessage('You have to at min select "by hidden single" or "by naked single".');
    zwangsabbruch := true;
  End;
End;

Procedure TForm7.FormCreate(Sender: TObject);
Begin
  Caption := 'Sudoku ver. : ' + ver + ' New Game';
  fField := TSudoku.Create(3);
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

Procedure TForm7.FormDestroy(Sender: TObject);
Begin
  fField.free;
  fField := Nil;
End;

Procedure TForm7.FormPaint(Sender: TObject);
Begin
  button1.setfocus;
End;

Function TForm7.OnLCLUpdateEvent: Boolean;
Begin
  Application.ProcessMessages;
  result := zwangsabbruch;
End;

Procedure TForm7.Init(Const aField: TSudoku; RepaintEvent: TNotifyEvent;
  aOptions: TSolveOptions);
Begin
  fField.Free;
  fField := TSudoku.Create(aField.Dimension);
  fRepaintEvent := RepaintEvent;
  fOptions := aOptions;
  CheckBox1.Checked := soHiddenSingle In aOptions;
  CheckBox2.Checked := soNakedSingle In aOptions;
  CheckBox3.Checked := soBlockAndColumnInteraction In aOptions;
  CheckBox4.Checked := soBlockAndBlockInteraction In aOptions;
  CheckBox5.Checked := soNakedSubset In aOptions;
  CheckBox6.Checked := soHiddenSubset In aOptions;
  CheckBox7.Checked := soXWing In aOptions;
  CheckBox8.Checked := soXYWing In aOptions;
  CheckBox9.Checked := soForcingChains In aOptions;
End;

End.

