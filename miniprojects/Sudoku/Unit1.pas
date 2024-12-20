(******************************************************************************)
(* Sudoku                                                          ??.??.???? *)
(*                                                                            *)
(* Version     : 1.13                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Implementation of a Sudoku solver and puzzle creator         *)
(*                                                                            *)
(* License     : See the file license.md, located under:                      *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(* Warranty    : There is no warranty, neither in correctness of the          *)
(*               implementation, nor anything other that could happen         *)
(*               or go wrong, use at your own risk.                           *)
(*                                                                            *)
(* Known Issues: none                                                         *)
(*                                                                            *)
(* History     : 0.01 - 1.11 = not tracked                                    *)
(*               1.12 = Fix LCL Crash in unit1.lfm                            *)
(*               1.13 = publish code -> start with refactoring                *)
(*                                                                            *)
(******************************************************************************)

Unit Unit1;

{$MODE ObjFPC}{$H+}

Interface

Uses
  SysUtils, Graphics, Forms, Classes, Controls, Dialogs, Menus,
  StdCtrls, ComCtrls, Sudoku3x3, ExtCtrls, lcltype;

Const
  Ver = '1.13';

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Panel1: TPanel;
    MainMenu1: TMainMenu;
    Datei1: TMenuItem;
    Beenden1: TMenuItem;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    Load1: TMenuItem;
    Save1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    New1: TMenuItem;
    Puzzle1: TMenuItem;
    Clearfield1: TMenuItem;
    Options1: TMenuItem;
    Help1: TMenuItem;
    General1: TMenuItem;
    Support1: TMenuItem;
    Warranty1: TMenuItem;
    Colors1: TMenuItem;
    Resetfield1: TMenuItem;
    Solveit1: TMenuItem;
    SolveOptions1: TMenuItem;
    Allowall1: TMenuItem;
    Allownone1: TMenuItem;
    Solvestep1: TMenuItem;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    ToolButton8: TToolButton;
    ToolButton9: TToolButton;
    ToolButton10: TToolButton;
    ToolButton11: TToolButton;
    ToolButton12: TToolButton;
    ToolButton13: TToolButton;
    ToolButton14: TToolButton;
    ToolButton15: TToolButton;
    ToolButton16: TToolButton;
    ToolButton17: TToolButton;
    ToolButton18: TToolButton;
    ToolButton19: TToolButton;
    ImageList1: TImageList;
    CheckBox5: TCheckBox;
    Button1: TButton;
    Button2: TButton;
    CheckBox6: TCheckBox;
    Action1: TMenuItem;
    General2: TMenuItem;
    bytryanderror1: TMenuItem;
    bynakedsubset1: TMenuItem;
    EditMenue1: TMenuItem;
    Modify1: TMenuItem;
    byhiddensubset1: TMenuItem;
    byBlockandColumninteractions1: TMenuItem;
    byblockandblockinteractions1: TMenuItem;
    byXYWing1: TMenuItem;
    ForcingChains1: TMenuItem;
    byhiddensingle1: TMenuItem;
    bynakedsingle1: TMenuItem;
    byXWingSwordfish1: TMenuItem;
    Maybenumbersgoodnumbers1: TMenuItem;
    MaybanumberclearField1: TMenuItem;
    SpecialPuzzle1: TMenuItem;
    Print1: TMenuItem;
    UNbeli1: TMenuItem;
    N4x41: TMenuItem;
    N5x51: TMenuItem;
    Info1: TMenuItem;
    N2x21: TMenuItem;
    Procedure Beenden1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormPaint(Sender: TObject);
    Procedure FormResize(Sender: TObject);
    Procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure FormKeyPress(Sender: TObject; Var Key: Char);
    Procedure CheckBox1Click(Sender: TObject);
    Procedure CheckBox3Click(Sender: TObject);
    Procedure CheckBox2Click(Sender: TObject);
    Procedure CheckBox4Click(Sender: TObject);
    Procedure Clearfield1Click(Sender: TObject);
    Procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure Panel1Paint(Sender: TObject);
    Procedure Support1Click(Sender: TObject);
    Procedure Warranty1Click(Sender: TObject);
    Procedure Save1Click(Sender: TObject);
    Procedure Load1Click(Sender: TObject);
    Procedure Resetfield1Click(Sender: TObject);
    Procedure ToolButton1Click(Sender: TObject);
    Procedure ToolButton11Click(Sender: TObject);
    Procedure Colors1Click(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure CheckBox5Click(Sender: TObject);
    Procedure CheckBox6Click(Sender: TObject);
    Procedure ToolButton1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure General2Click(Sender: TObject);
    Procedure ToolButton11MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure General1Click(Sender: TObject);
    Procedure Solveit1Click(Sender: TObject);
    Procedure Solvestep1Click(Sender: TObject);
    Procedure Allowall1Click(Sender: TObject);
    Procedure Allownone1Click(Sender: TObject);
    Procedure bymarkingnumbersClick(Sender: TObject);
    Procedure Modify1Click(Sender: TObject);
    Procedure Puzzle1Click(Sender: TObject);
    Procedure Maybenumbersgoodnumbers1Click(Sender: TObject);
    Procedure MaybanumberclearField1Click(Sender: TObject);
    Procedure SpecialPuzzle1Click(Sender: TObject);
    Procedure Print1Click(Sender: TObject);
    Procedure N4x41Click(Sender: TObject);
    Procedure N5x51Click(Sender: TObject);
    Procedure Info1Click(Sender: TObject);
    Procedure N2x21Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

  // die Line Pencils welche extra erstellt werden
  TLinepencil = Array[0..17] Of T3Pencil;

Var
  Form1: TForm1;
  bm: Tbitmap; // gegen das Flimmern
  Linepencil: TLinepencil; // Die LinienPencil's
  Field: T3field; // Das Speilfeld
  Druckbreite, Breite: Integer; // Globale Variable die die Breite eines Feldes auf dem Spielfeld in Pixeln angibt
  mx, my: integer; // globalen x,y Koordinaten der Maus im Feld
  lc: integer; // Für das Line Edit brauchen wir ne Extra Variable
  Bretthintergrundfarbe1: Tcolor; // Optische eigenschaften des Spieles
  Bretthintergrundfarbe2: Tcolor; // Optische eigenschaften des Spieles
  Maybeedcolor: Tcolor; // Optische eigenschaften des Spieles
  MarkedColor1: Tcolor; // Optische eigenschaften des Spieles
  MarkedColor2: Tcolor; // Optische eigenschaften des Spieles
  CursorMarker: Tcolor; // Optische eigenschaften des Spieles
  Fixedcolor: Tcolor; // Optische eigenschaften des Spieles
  Gitterfarbe: Tcolor; // Optische eigenschaften des Spieles
  FontColor: Tcolor; // Optische eigenschaften des Spieles
  Pencilcolor: Tcolor; // Optische eigenschaften des Spieles
  PencilcolorMarked: Tcolor; // Optische eigenschaften des Spieles
  LightenColor: Tcolor; // Optische eigenschaften des Spieles
  FormBackground: Tcolor; // Optische eigenschaften des Spieles
  unpencilallow: boolean;
  invalidnallow: boolean;
  substitution: Array[1..9] Of String[1];

  // Zeichnet das Komplette Spielfeld neu
Procedure Drawfield;
Procedure Solve(Step, invisible: boolean; Var Data: T3field);
Procedure HackSudoku(Var Data: T3Field; Direction: Integer = -1);
// die nachfolgenden Proceduren sind nur damit Ki nicht nach unten Kopiert werden mus hier oben
Procedure Resetopt;
//Procedure Marknumbers(Value: integer; Var Data: T3field);
//Procedure GetrealPencilnumbers(Var data: T3field);
Procedure getLinePencil(Var Data: T3Field);
Function min(v1, v2: Integer): integer;

Implementation

Uses Unit2, Unit3, Unit4, Unit5, Unit6, Unit7, Unit8, Unit9,
  Unit11, Unit13, Unit16;

{$R *.lfm}


// Liefert den Kleineren von V1 und V2 zurück

Function min(v1, v2: Integer): integer;
Var
  erg: integer;
Begin
  erg := v1;
  If erg > V2 Then erg := v2;
  result := erg;
End;

// Ist Step = True dann wird nur ein Step gemacht bei step = False, wird das Rätsel Komplett gelöst.

Procedure Solve(Step, invisible: boolean; Var Data: T3field);

  Procedure setfocus(x, y: integer);
  Begin
    If Step Then Begin
      mx := x;
      my := y;
    End;
  End;

  // Fügt eine zahl in ein Feld ein und löscht alle Pencil werte
  Procedure WriteNumber(x, y, number: integer);
  Var
    z: integer;
  Begin
    data[x, y].value := number;
    For z := 0 To 8 Do
      Data[x, y].pencil[z] := false;
  End;

Label
  Schlus;
Var
  wp: Tpoint;
  y3, x2, y2, w, x1, y1, x, y, z: Integer;
  wieder, ssolve, nochmal, weiter, b: Boolean;
  zah: Array[1..9] Of 0..9; // Kann zu Boolean gemacht werden!
Begin
  // Erst mal das Feld von altlasten säubern
  For x := 0 To 8 Do
    For y := 0 To 8 Do
      If Data[x, y].value <> 0 Then
        Writenumber(x, y, Data[x, y].value);
  // aussschalten der edit functionen
  form1.checkbox6.checked := false;
  form1.checkbox3.checked := false;
  If (Not Step) And (Not invisible) Then resetopt;
  b := true; // start der Endlosschleife
  While b Do Begin
    weiter := false; // Wird von jedem Allgorithmus der was verändert auf True gesetzt damit es weiter gehen kann, und gleichzeitig immer nur einer am Werk ist
    // Ermitteln der weiteren Nummern via Markierend er Nummern
    If form1.byhiddensingle1.checked { And Not weiter } Then Begin
      For z := 1 To 9 Do Begin // Probieren von allen Zahlen
        // zuerst löschen aller markierungen
        For x := 0 To 8 Do
          For y := 0 To 8 Do
            Data[x, y].marked := false;
        // Markieren der Zahl Z
        Mark(Data, z);
        // absuchen der 9er blocks ob da irgendwo ne Freie Zahl ist
        For x := 0 To 2 Do
          For y := 0 To 2 Do Begin
            w := 0;
            For x1 := 0 To 2 Do
              For y1 := 0 To 2 Do
                // Wenn wir ein Leeres Feld gefunden haben, zählen wir in w das wievielte es ist, und speichern dessen koordinaten
                If Not (Data[x * 3 + x1, y * 3 + y1].marked) Then Begin
                  inc(w);
                  wp.x := x * 3 + x1;
                  wp.y := y * 3 + y1;
                End;
            // Es wurde nur ein Leeres Feld gefunden , damit ist dieser Step erledigt.
            If w = 1 Then Begin
              weiter := true; // Da wir noch was machen konnten , ist noch nicht sicher ob wir Fertig sind.
              WriteNumber(wp.x, wp.y, z);
              setfocus(wp.x, wp.y); // mitziehen des Cursors damit der User weis was wir gemacht haben
              // Neustart des Lösen's
              Goto schlus;
            End;
          End;
      End;
    End;
    If form1.bynakedsingle1.checked Or form1.bynakedsubset1.checked Or form1.byhiddensubset1.checked Or Form1.byXYWing1.checked Or false Then Begin
      // Ab jetzt geht's an's eingemachte, zur Vorbereitung braucehn wir aber Korreckte Pencil's
      For x := 0 To 8 Do
        For y := 0 To 8 Do Begin
          data[x, y].marked := false; // Demarkieren aller Felder
          For z := 0 To 8 Do
            data[x, y].Pencil[z] := True; // Demarkieren aller Pencils
        End;
      For x := 0 To 17 Do
        For y := 0 To 8 Do
          Linepencil[x][y] := true; // Demarkieren der Linepencil's
      ssolve := true; // Da unser System recht Kompliziert ist müssen wir es auch oft woederholen
      While ssolve Do Begin
        getlinepencil(data); // Ermitteln der Korreckten Line pencil's
        GetPencil(data); // Ermitteln der Pencil's in den Feldern
        ssolve := false; // Aber nicht zu oft wiederhohlen
        If form1.byhiddensubset1.checked Then Hiddensubset(Data);
        // Nachdem wir nun gute Vorraussetzungen Geschaffen haben, können wir mit unseren Tricks loslegen
        If form1.bynakedsubset1.checked Then Begin
          // Wir suchen alle Pencil's raus die Gleich sind, finden wir welche dann können diese dann bei den anderen gelöscht werden
          nochmal := true;
          While nochmal Do Begin
            nochmal := false; // Abbruch
            // Betrachten der 9 Spalten
            For z := 0 To 8 Do Begin
              wp.x := -1;
              For x := 0 To 8 Do Begin
                // Keines der Felder ist bis jetzt betrachtet worden
                For y := 1 To 9 Do
                  zah[y] := 0;
                // Prüfen des Actuellen Feldes mit allen anderen
                For y := 0 To 8 Do
                  If X <> y Then // nicht mit sich selbst vergleichen
                    If Comppencil(data[z, x].Pencil, data[z, y].pencil) And (data[z, x].value = 0) And (data[z, y].value = 0) Then Begin // Sind die Pencil's gleich
                      wp.x := pencilcount(data[z, x].Pencil); // speichern der Anzahl der Pencil's
                      zah[x + 1] := 1;
                      zah[y + 1] := 1;
                    End;
                // Ermitteln der Anzahl der gefunden Felder
                w := 0;
                For y3 := 1 To 9 Do
                  If Zah[y3] = 1 Then inc(w);
                // Wir haben tatsächlich mehrere Felder mit gleichen Pencil's gefudnen , d.h. wir können deren Werte nun löschen
                If (W = Wp.x) And (w > 1) Then Begin
                  For y3 := 1 To 9 Do // Hohlen des Ersten Feldes mit den ZU löschenden Pencils
                    If Zah[y3] = 1 Then Begin
                      w := y3 - 1;
                      break;
                    End;
                  // Löschen der Pencil werte der anderen Felder
                  For y3 := 0 To 8 Do
                    If Zah[y3 + 1] <> 1 Then
                      For y := 0 To 8 Do
                        If data[z, w].pencil[y] And data[z, y3].pencil[y] Then Begin
                          data[z, y3].pencil[y] := false; // Es gab tatsächlich was zu löschen
                          nochmal := true; // wenn es einmal geklappt hat , dann Vielleicht auch ein zweites mal
                          ssolve := true; // Wir haben die Pencil's verändert mal schauen ob nachher ein anderes System das gebrauchen kann
                        End;
                End;
              End;
            End;
            // betrachten der 9er Bklock's
            For x1 := 0 To 2 Do
              For y1 := 0 To 2 Do Begin
                wp.x := -1;
                For x := 0 To 2 Do
                  For y := 0 To 2 Do Begin
                    // Keines der Felder ist bis jetzt betrachtet worden
                    For z := 1 To 9 Do
                      zah[z] := 0;
                    // Prüfen des Actuellen Feldes mit allen anderen
                    For x2 := 0 To 2 Do
                      For y2 := 0 To 2 Do
                        //nicht mit sich selbst vergleichen
                        If Not ((x2 = x) And (y = y2)) Then
                          If comppencil(data[x1 * 3 + x, y1 * 3 + y].pencil, data[x1 * 3 + x2, y1 * 3 + y2].pencil) And (data[x1 * 3 + x, y1 * 3 + y].value = 0) And (data[x1 * 3 + x2, y1 * 3 + y2].value = 0) Then Begin
                            //                            inc(w);
                            wp.x := pencilcount(data[x1 * 3 + x, y1 * 3 + y].pencil);
                            zah[(x2 + y2 * 3) + 1] := 1; // MArkieren der Felder deren Werte nachher nicht gelöscht werden dürfen
                            zah[(x + y * 3) + 1] := 1; // MArkieren der Felder deren Werte nachher nicht gelöscht werden dürfen
                          End;
                    w := 0;
                    For y2 := 1 To 9 Do
                      If Zah[y2] = 1 Then inc(w);
                    // Wir haben tatsächlich mehrere Felder mit gleichen Pencil's gefudnen , d.h. wir können deren Werte nun löschen
                    If (W = Wp.x) And (w > 1) Then Begin
                      For x2 := 1 To 9 Do // Hohlen des Ersten Feldes mit den ZU löschenden Pencils
                        If Zah[x2] = 1 Then Begin
                          w := x2 - 1;
                          wp.x := w Mod 3;
                          wp.y := w Div 3;
                          break;
                        End;
                      For x2 := 0 To 2 Do
                        For y3 := 0 To 2 Do
                          If ZAh[(x2 + y3 * 3) + 1] <> 1 Then
                            // Löschen der Pencil einträge
                            For y2 := 0 To 8 Do Begin
                              If Data[x1 * 3 + wp.x, y1 * 3 + wp.y].pencil[y2] And Data[x1 * 3 + x2, y1 * 3 + y3].pencil[y2] Then Begin
                                Data[x1 * 3 + x2, y1 * 3 + y3].pencil[y2] := false; // Es gab tatsächlich was zu löschen
                                nochmal := true; // wenn es einmal geklappt hat , dann Vielleicht auch ein zweites mal
                                ssolve := true; // Wir haben die Pencil's verändert mal schauen ob nachher ein anderes System das gebrauchen kann
                              End;
                            End;
                    End;
                  End;
              End;
            // Betrachten der 9 Reihen
            For z := 0 To 8 Do Begin
              // Keines der Felder ist bis jetzt betrachtet worden
              For x := 1 To 9 Do
                zah[x] := 0;
              wp.x := -1;
              For x := 0 To 8 Do Begin
                // Keines der Felder ist bis jetzt betrachtet worden
                For y := 1 To 9 Do
                  zah[y] := 0;
                // Prüfen des Actuellen Feldes mit allen anderen
                For y := 0 To 8 Do
                  If X <> y Then // nicht mit sich selbst vergleichen
                    If Comppencil(data[x, z].Pencil, data[y, z].pencil) And (Data[x, z].value = 0) And (Data[y, z].value = 0) Then Begin // Sind die Pencil's gleich
                      wp.x := pencilcount(Data[x, z].Pencil); // speichern der Anzahl der Pencil's
                      zah[x + 1] := 1;
                      zah[y + 1] := 1;
                    End;
                // Ermitteln der Anzahl der Gefundenen Felder
                w := 0;
                For y3 := 1 To 9 Do
                  If Zah[y3] = 1 Then inc(w);
                // Wir haben tatsächlich mehrere Felder mit gleichen Pencil's gefudnen , d.h. wir können deren Werte nun löschen
                If (W = Wp.x) And (w > 1) Then Begin
                  For y3 := 1 To 9 Do // Hohlen des Ersten Feldes mit den ZU löschenden Pencils
                    If Zah[y3] = 1 Then Begin
                      w := y3 - 1;
                      break;
                    End;
                  // Löschen der Pencil werte der anderen Felder
                  For y3 := 0 To 8 Do
                    If Zah[y3 + 1] <> 1 Then
                      For y := 0 To 8 Do
                        If Data[w, z].pencil[y] And Data[y3, z].pencil[y] Then Begin
                          Data[y3, z].pencil[y] := false; // Es gab tatsächlich was zu löschen
                          nochmal := true; // wenn es einmal geklappt hat , dann Vielleicht auch ein zweites mal
                          ssolve := true; // Wir haben die Pencil's verändert mal schauen ob nachher ein anderes System das gebrauchen kann
                        End;
                End;
              End;
            End;
          End;
        End;
        // das Lösen via Hidden Subset
        If form1.byhiddensubset1.checked Then Begin

        End;
        If Form1.byXYWing1.checked Then Begin
          Solvebyxywing(Field);
        End;
        {

          Block and Column / Row Interactions
          Block / Block Interactions
          Hidden Subset
          XY-Wing

        }
        // Alle Tricks haben eingewirkt nun  schauen wir ob wir nicht doch ne Zahl gefunden haben die gesetzt werden kann ;)
        // Haben wir eine allein stehende Zahl gefunden dann können wir sie in allen entsprechenden anderen Feldern austragen
        If Form1.bynakedsingle1.checked Then Begin
          wieder := True;
          While wieder Do Begin // Wir löschen so lange zahlen aus den Pencils's bis es nicht mehr geht
            wieder := false;
            For x := 0 To 8 Do
              For y := 0 To 8 Do Begin
                If (Data[x, y].value = 0) Then Begin
                  w := 0;
                  wp.x := -1;
                  For z := 0 To 8 Do
                    If Data[x, y].pencil[z] Then Begin
                      inc(w);
                      wp.x := z;
                    End;
                  // Wir haben eine einzelne Zahl gefunden nun gilt es sie aus allen anderen ZAhlen aus zu tragen
                  If w = 1 Then Begin
                    weiter := true; // Da wir noch was machen konnten , ist noch nicht sicher ob wir Fertig sind.
                    // Waagrecht Senkrecht
                    For z := 0 To 8 Do Begin
                      If z <> x Then Begin
                        If Data[z, y].Pencil[wp.x] Then wieder := true;
                        Data[z, y].Pencil[wp.x] := false;
                      End;
                      If z <> y Then Begin
                        If Data[x, z].Pencil[wp.x] Then wieder := true;
                        Data[x, z].Pencil[wp.x] := false;
                      End;
                    End;
                    // Die 9 er Block's
                    z := wp.x;
                    wp.x := x - x Mod 3;
                    wp.y := y - y Mod 3;
                    For x1 := 0 To 2 Do
                      For y1 := 0 To 2 Do
                        If ((wp.x + x1) <> x) And ((wp.y + y1) <> y) Then Begin
                          If data[wp.x + x1, wp.y + y1].pencil[z] Then wieder := true;
                          data[wp.x + x1, wp.y + y1].pencil[z] := false;
                        End;
                  End;
                End;
              End;
          End;
          //If Weiter Then Goto schlus;}
          // Alle Tricks haben eingewirkt nun  schauen wir ob wir nicht doch ne Zahl gefunden haben die gesetzt werden kann ;)
          For x := 0 To 8 Do // Wir suchen einfach ein Feld das nur noch einen einzigen Pencil wert hat.
            For y := 0 To 8 Do Begin
              If Data[x, y].value = 0 Then Begin
                w := 0; // Speichern der Zahl die alleine ist
                wp.x := 0; // Speichern ob die Zahl wirklich alleine ist ;)
                For z := 0 To 8 Do // anschauen der Pencil's
                  // Das Feld selbst darf aber auch nicht schon belegt sein, die Pencil erstell Procedur berechnet das nicht
                  If Data[x, y].pencil[z] Then Begin
                    w := z;
                    inc(wp.x);
                  End;
                If Wp.x = 1 Then Begin
                  weiter := true; // Da wir noch was machen konnten , ist noch nicht sicher ob wir Fertig sind.
                  WriteNumber(x, y, w + 1);
                  // Löschen dieser Zahl aus den Pencil daten der anderen
                  // Waagrecht Senkrecht
                  wp.x := w;
                  For z := 0 To 8 Do Begin
                    Data[z, y].Pencil[wp.x] := false;
                    Data[x, z].Pencil[wp.x] := false;
                  End;
                  // Im  9er Block
                  z := wp.x;
                  wp.x := x - x Mod 3;
                  wp.y := y - y Mod 3;
                  For x1 := 0 To 2 Do
                    For y1 := 0 To 2 Do
                      data[wp.x + x1, wp.y + y1].pencil[z] := false;
                  setfocus(x, y); // mitziehen des Cursors damit der User weis was wir gemacht haben
                  // Neustart der Ki
                  Goto schlus;
                End;
              End;
            End;
        End;
      End;
    End;
    // Hilft alles nichts so mus der Zufall Helfen
    If form1.bytryanderror1.checked Then Begin
      // Ist ausgelagert , da es eine ganz eigene Art von lösen ist
      GetKomplettSudoku(Data);
      weiter := false; // Danach braucht nichts mehr Probiert werden
    End;
    Schlus:
    If Step Or isready3(Data) Or (Not weiter) Then b := false;
  End;
  If Not invisible Then Begin
    // Demarkieren aller Felder
    For x := 0 To 8 Do
      For y := 0 To 8 Do Begin
        Data[x, y].marked := false;
        //        For z := 0 To 8 Do
        //          Data[x, y].Pencil[z] := True;
      End;
    // Schauen ob irgendwelche Zahlen schon komplett sind
    For x1 := 1 To 9 Do
      zah[x1] := 0;
    For x1 := 0 To 8 Do
      For y1 := 0 To 8 Do
        If Data[x1, y1].value <> 0 Then
          inc(zah[Data[x1, y1].value]);
    For x1 := 1 To 9 Do
      If zah[x1] = 9 Then Begin
        TToolbutton(form1.Findcomponent('ToolButton' + inttostr(x1))).enabled := false;
        TToolbutton(form1.Findcomponent('ToolButton' + inttostr(x1 + 10))).enabled := false;
        TToolbutton(form1.Findcomponent('ToolButton' + inttostr(x1 + 10))).Down := false;
        TToolbutton(form1.Findcomponent('ToolButton' + inttostr(x1))).Down := False;
        For x := 0 To 8 Do
          For y := 0 To 8 Do
            Data[x, y].marked := false;
      End
      Else Begin
        TToolbutton(form1.Findcomponent('ToolButton' + inttostr(x1))).enabled := true;
        TToolbutton(form1.Findcomponent('ToolButton' + inttostr(x1 + 10))).enabled := true;
      End;
    // Wenn nur ein Step gewünscht war dann schauen wir uns nun mal an was die Pencil's gemacht haben
  //    If Step { And (form1.Checkbox4.checked Or form1.Checkbox5.checked) } Then
  //      form1.button1.onclick(Nil);
    Drawfield;
  End;
End;

// Auslesen der User.ini

Procedure Readini;
Var
  f: Textfile;
  s: String;
  x: integer;
Begin
  If Fileexists(IncludeTrailingPathDelimiter(extractfilepath(application.exename)) + 'user.ini') Then Begin
    assignfile(f, IncludeTrailingPathDelimiter(extractfilepath(application.exename)) + 'user.ini');
    reset(f);
    Readln(f, s);
    Bretthintergrundfarbe1 := stringtocolor(s);
    readln(f, s);
    Bretthintergrundfarbe2 := stringtocolor(s);
    readln(f, s);
    Maybeedcolor := stringtocolor(s);
    readln(f, s);
    MarkedColor1 := stringtocolor(s);
    readln(f, s);
    MarkedColor2 := stringtocolor(s);
    readln(f, s);
    CursorMarker := stringtocolor(s);
    readln(f, s);
    Fixedcolor := stringtocolor(s);
    readln(f, s);
    Gitterfarbe := stringtocolor(s);
    readln(f, s);
    FontColor := stringtocolor(s);
    readln(f, s);
    Pencilcolor := stringtocolor(s);
    readln(f, s);
    PencilcolorMarked := stringtocolor(s);
    readln(f, s);
    LightenColor := stringtocolor(s);
    readln(f, s);
    FormBackground := stringtocolor(s);
    For x := 1 To 6 Do Begin
      TCheckbox(Form1.findcomponent('Checkbox' + inttostr(x))).color := FormBackground;
      If FormBackground = clblack Then
        TCheckbox(Form1.findcomponent('Checkbox' + inttostr(x))).font.color := clwhite
      Else
        TCheckbox(Form1.findcomponent('Checkbox' + inttostr(x))).font.color := clblack;
    End;
    readln(f, s);
    unpencilallow := odd(strtoint(s));
    For x := 1 To 9 Do Begin
      readln(f, s);
      substitution[x] := s;
    End;
    With form1 Do Begin
      readln(f, s);
      byhiddensingle1.checked := odd(strtoint(s));
      readln(f, s);
      bynakedsingle1.checked := odd(strtoint(s));
      readln(f, s);
      bytryanderror1.checked := odd(strtoint(s));
      readln(f, s);
      bynakedsubset1.checked := odd(strtoint(s));
      readln(f, s);
      byhiddensubset1.checked := odd(strtoint(s));
      readln(f, s);
      byBlockandColumninteractions1.checked := odd(strtoint(s));
      readln(f, s);
      byblockandblockinteractions1.checked := odd(strtoint(s));
      readln(f, s);
      byXWingSwordfish1.checked := odd(strtoint(s));
      readln(f, s);
      byXYWing1.checked := odd(strtoint(s));
      readln(f, s);
      ForcingChains1.checked := odd(strtoint(s));
      readln(f, s);
      Druckbreite := strtoint(s);
    End;
    readln(f, s);
    invalidnallow := odd(strtoint(s));
    closefile(f);
  End
  Else Begin
    For x := 1 To 9 Do
      substitution[x] := inttostr(x);
    form1.bytryanderror1.checked := false;
    // Vorsicht eine änderung hier mus auch in Form2 geändert werden !!!
    Bretthintergrundfarbe1 := clbtnface;
    Bretthintergrundfarbe2 := clgray;
    Maybeedcolor := clyellow;
    MarkedColor1 := clBlue;
    MarkedColor2 := clnavy;
    CursorMarker := clgreen;
    Fixedcolor := clblack;
    Gitterfarbe := Clblack;
    FontColor := $00C08000;
    Pencilcolor := clmaroon;
    PencilcolorMarked := $004080FF;
    LightenColor := CLaqua;
    FormBackground := clbtnface;
    unpencilallow := true;
    invalidnallow := true;
    Druckbreite := 1;
  End;
End;

// Schreiben der User.ini

Procedure Writeini;
Var
  f: Textfile;
  x: integer;
Begin
  assignfile(f, IncludeTrailingPathDelimiter(extractfilepath(application.exename)) + 'user.ini');
  rewrite(f);
  writeln(f, colortostring(Bretthintergrundfarbe1));
  writeln(f, colortostring(Bretthintergrundfarbe2));
  writeln(f, colortostring(Maybeedcolor));
  writeln(f, colortostring(MarkedColor1));
  writeln(f, colortostring(MarkedColor2));
  writeln(f, colortostring(CursorMarker));
  writeln(f, colortostring(Fixedcolor));
  writeln(f, colortostring(Gitterfarbe));
  writeln(f, colortostring(FontColor));
  writeln(f, colortostring(Pencilcolor));
  writeln(f, colortostring(PencilcolorMarked));
  writeln(f, colortostring(LightenColor));
  writeln(f, colortostring(FormBackground));
  writeln(f, inttostr(ord(unpencilallow)));
  For x := 1 To 9 Do
    writeln(f, substitution[x]);
  With form1 Do Begin
    writeln(f, inttostr(ord(byhiddensingle1.checked)));
    writeln(f, inttostr(ord(bynakedsingle1.checked)));
    writeln(f, inttostr(ord(bytryanderror1.checked)));
    writeln(f, inttostr(ord(bynakedsubset1.checked)));
    writeln(f, inttostr(ord(byhiddensubset1.checked)));
    writeln(f, inttostr(ord(byBlockandColumninteractions1.checked)));
    writeln(f, inttostr(ord(byblockandblockinteractions1.checked)));
    writeln(f, inttostr(ord(byXWingSwordfish1.checked)));
    writeln(f, inttostr(ord(byXYWing1.checked)));
    writeln(f, inttostr(ord(ForcingChains1.checked)));
  End;
  writeln(f, inttostr(Druckbreite));
  writeln(f, inttostr(ord(invalidnallow)));
  closefile(f);
End;

{// Diese Procedur kann nur Pencil einträge Löschen !!

Procedure GetrealPencilnumbers(Var data: T3field);
Var
  x, y, z: integer;
  zahlen: Array Of Integer;

  // Fügt dem Array Zahlen den Wert Value ein wenn dieser noch nicht vorhanden ist.
  Procedure add(Value: integer);
  Var
    b: Boolean;
    w: integer;
  Begin
    b := true;
    For w := 0 To high(Zahlen) Do
      If Value = Zahlen[w] Then Begin
        b := false;
        break;
      End;
    If b Then Begin
      setlength(zahlen, high(zahlen) + 2);
      Zahlen[high(zahlen)] := value;
    End;
  End;

  // Ermitteln der Zahlen des Feldes x1,y1 die nicht mehr in Frage kommen können !!
  Procedure Getz(x1, y1: integer);
  Var
    a, b, c, d, w: integer;
  Begin
    setlength(Zahlen, 0);
    // Suchen der Zahlen Waagrecht und Senkrecht
    For w := 0 To 8 Do Begin
      If data[x1, w].value <> 0 Then add(data[x1, w].value);
      If data[w, y1].value <> 0 Then add(data[w, y1].value);
    End;
    // Suchen der Zahlen im 9er Feld
    a := x1 - (x1 Mod 3);
    b := y1 - (y1 Mod 3);
    For c := 0 To 2 Do
      For d := 0 To 2 Do
        // Prüfen der auser dem gewählten Feld
        If ((a + c) <> x1) Or ((b + d) <> y1) Then Begin
          If data[a + c, b + d].value <> 0 Then add(data[a + c, b + d].value);
        End;
  End;

Begin
  setlength(Zahlen, 0);
  For x := 0 To 8 Do Begin
    For y := 0 To 8 Do Begin
      // Ermitteln der bereits gesetzten Nummern
      getz(x, y);
      // löschen der Pencil einträge
      For z := 0 To High(Zahlen) Do
        If data[x, y].Pencil[Zahlen[z] - 1] Then data[x, y].Pencil[Zahlen[z] - 1] := false;
    End;
  End;
  setlength(Zahlen, 0);
End;
}

// Fügt wieder einen Penzil wert ein

Procedure UnPencil(x, y, value: integer; Var Data: T3field);
Var
  a, b, c, d, z: integer;
Begin
  If Not unpencilallow Then exit;
  If Value = 0 Then exit;
  // Unpenzil für die Felder !!
  For z := 0 To 8 Do Begin
    Data[x, z].Pencil[value - 1] := true;
    Data[z, y].Pencil[value - 1] := true;
  End;
  // MArkieren des 9 er Blockes der Zahl
  a := x - (x Mod 3);
  b := y - (y Mod 3);
  For c := 0 To 2 Do
    For d := 0 To 2 Do
      Data[a + c, b + d].Pencil[value - 1] := true;
  // UNpencil für die Lines
  Linepencil[x][Value - 1] := true;
  Linepencil[9 + y][Value - 1] := true;

End;
{
// Markiert alle Zahlen = Value und die Felder die durch diese Zahl verdeckt werden

Procedure Marknumbers(Value: integer; Var Data: T3field);
// Markiert alles Waagrecht und Senkrecht
Procedure Submark(x1, y1: integer);
Var
a, b, c, d, z: integer;
Begin
For z := 0 To 8 Do Begin
Data[x1, z].marked := true;
Data[z, y1].marked := true;
End;
// MArkieren des 9 er Blockes der Zahl
a := x1 - (x1 Mod 3);
b := y1 - (y1 Mod 3);
For c := 0 To 2 Do
For d := 0 To 2 Do
Data[a + c, b + d].marked := true;
End;
Var
x2, y2: integer;
Begin
If value = 0 Then exit;
// Markeiren aller Zahlen die Gleich dem Field [x,y] sind
For x2 := 0 To 8 Do
For y2 := 0 To 8 Do Begin
If Data[x2, y2].value = value Then Submark(x2, y2);
// markeiren aller Zahlen die schon eingetragen sind
If Data[x2, y2].value <> 0 Then Data[x2, y2].MArked := true;
End;
End;      }

// Ermittelt die Line Pencils

Procedure getLinePencil(Var Data: T3Field);
Var
  x, y: integer;
  zahlen: Array Of Integer;

  // Fügt dem Array Zahlen den Wert Value ein wenn dieser noch nicht vorhanden ist.
  Procedure add(Value: integer);
  Var
    b: Boolean;
    w: integer;
  Begin
    b := true;
    For w := 0 To high(Zahlen) Do
      If Value = Zahlen[w] Then Begin
        b := false;
        break;
      End;
    If b Then Begin
      setlength(zahlen, high(zahlen) + 2);
      Zahlen[high(zahlen)] := value;
    End;
  End;

Begin
  setlength(zahlen, 0);
  // erst mal alle Zahlen reinbauen
{  For x := 0 To 17 Do
    For y := 0 To 8 Do
      Linepencil[x][y] := true;  }
  // Zuerst die Senkrechten Linien
  For x := 0 To 8 Do Begin
    setlength(zahlen, 0);
    For y := 0 To 8 Do
      If Data[x, y].value <> 0 Then add(Data[x, y].value);
    For y := 0 To high(Zahlen) Do
      Linepencil[x][Zahlen[y] - 1] := false;
  End;
  // Dann die Waagrechten Linien
  For y := 0 To 8 Do Begin
    setlength(zahlen, 0);
    For x := 0 To 8 Do
      If Data[x, y].value <> 0 Then add(Data[x, y].value);
    For x := 0 To high(Zahlen) Do
      Linepencil[y + 9][Zahlen[x] - 1] := false;
  End;
  // Freigeben der Variablen
  setlength(zahlen, 0);
End;

// Zeichnet das Komplette Spielfeld

Procedure Drawfield;
Var
  x, y, z, d: integer;
Begin
  If bm = Nil Then exit;
  With bm.canvas Do Begin
    If Form1.checkbox1.checked And (mx > -1) Then MArk(field, field[mx, my].value);
    // Löschen des Bildschirms
    brush.style := bssolid;
    brush.color := FormBackground;
    rectangle(-1, -1, form1.width + 1, form1.height + 1);
    //rectangle(-1,-1,Breite*10,Breite *10);
   // Die LinePencil's
    If form1.Checkbox6.checked Then Begin
      brush.style := bssolid;
      brush.color := CursorMarker;
      Pen.color := CursorMarker;
      If lc < 9 Then Begin
        rectangle(breite * (lc + 1), 1, breite * (lc + 2), Breite);
      End
      Else Begin
        rectangle(breite * 10, breite * (lc - 8), Breite * 11, breite * (lc - 7));
      End;
      brush.style := bsclear;
    End;
    // Malen der Line Pencil geschichten !!
    If form1.Checkbox5.checked Then Begin
      // Die Waagrechten beschriftungen.
      font.size := breite Div 5;
      For x := 0 To 8 Do
        For z := 0 To 8 Do
          If Linepencil[x][z] Then Begin
            If (Lc = x) And (form1.checkbox6.checked) Then
              font.color := PencilcolorMarked
            Else
              font.color := Pencilcolor;
            Case z Of
              0..2: Begin
                  If TToolButton(form1.findcomponent('Toolbutton' + inttostr(11 + z))).Down Then Begin
                    brush.color := LightenColor;
                    pen.color := LightenColor;
                    brush.style := bssolid;
                    d := (z + 1) Mod 3;
                    If D = 0 Then d := 3;
                    ellipse(breite * (x + 1) + 2 + (Breite Div 3) * (z Mod 3), {Breite * (y + 1) + } 2 + 0, breite * (x + 1) - 2 + (Breite Div 3) * d, {Breite * (y + 1) } -2 + Breite Div 3);
                    Brush.style := bsclear;
                  End;
                  textout(breite * (x + 1) + (Breite Div 3) * (z Mod 3) + ((Breite Div 3) - Textwidth(inttostr(z + 1))) Div 2, {Breite * (y + 1) } 1 + 0, substitution[(z + 1)]);
                End;
              3..5: Begin
                  If TToolButton(form1.findcomponent('Toolbutton' + inttostr(11 + z))).Down Then Begin
                    brush.color := LightenColor;
                    pen.color := LightenColor;
                    brush.style := bssolid;
                    d := (z + 1) Mod 3;
                    If D = 0 Then d := 3;
                    ellipse(breite * (x + 1) + 2 + (Breite Div 3) * (z Mod 3), { Breite * (y + 1) + } 2 + breite Div 3, breite * (x + 1) - 2 + (Breite Div 3) * d, { Breite * (y + 1) } -2 + (Breite Div 3) * 2);
                    Brush.style := bsclear;
                  End;
                  textout(breite * (x + 1) + (Breite Div 3) * (z Mod 3) + ((Breite Div 3) - Textwidth(inttostr(z + 1))) Div 2, { Breite * (y + 1) +} 1 + Breite Div 3, substitution[(z + 1)]);
                End;
              6..8: Begin
                  If TToolButton(form1.findcomponent('Toolbutton' + inttostr(11 + z))).Down Then Begin
                    brush.color := LightenColor;
                    pen.color := LightenColor;
                    brush.style := bssolid;
                    d := (z + 1) Mod 3;
                    If D = 0 Then d := 3;
                    ellipse(breite * (x + 1) + 2 + (Breite Div 3) * (z Mod 3), { Breite * (y + 1) + } 2 + (breite Div 3) * 2, breite * (x + 1) - 2 + (Breite Div 3) * d, { Breite * (y + 1) } -2 + Breite);
                    Brush.style := bsclear;
                  End;
                  textout(breite * (x + 1) + (Breite Div 3) * (z Mod 3) + ((Breite Div 3) - Textwidth(inttostr(z + 1))) Div 2, {Breite * (y + 1) +} 1 + (Breite Div 3) * 2, substitution[(z + 1)]);
                End;
            End;
          End;
      // Die senkrechten beschriftungen.
      font.size := breite Div 5;
      font.color := Pencilcolor;
      For x := 0 To 8 Do
        For z := 0 To 8 Do
          If Linepencil[x + 9][z] Then Begin
            If (Lc = x + 9) And (form1.checkbox6.checked) Then
              font.color := PencilcolorMarked
            Else
              font.color := Pencilcolor;
            Case z Of
              0..2: Begin
                  If TToolButton(form1.findcomponent('Toolbutton' + inttostr(11 + z))).Down Then Begin
                    brush.color := LightenColor;
                    pen.color := LightenColor;
                    brush.style := bssolid;
                    d := (z + 1) Mod 3;
                    If D = 0 Then d := 3;
                    ellipse(breite * (10) + 2 + (Breite Div 3) * (z Mod 3), Breite * (x + 1) + 2 + 0, breite * (10) - 2 + (Breite Div 3) * d, Breite * (x + 1) - 2 + Breite Div 3);
                    Brush.style := bsclear;
                  End;
                  textout(breite * (10) + (Breite Div 3) * (z Mod 3) + ((Breite Div 3) - Textwidth(inttostr(z + 1))) Div 2, Breite * (x + 1) + 1 + 0, substitution[(z + 1)]);
                End;
              3..5: Begin
                  If TToolButton(form1.findcomponent('Toolbutton' + inttostr(11 + z))).Down Then Begin
                    brush.color := LightenColor;
                    pen.color := LightenColor;
                    brush.style := bssolid;
                    d := (z + 1) Mod 3;
                    If D = 0 Then d := 3;
                    ellipse(breite * (10) + 2 + (Breite Div 3) * (z Mod 3), Breite * (x + 1) + 2 + breite Div 3, breite * (10) - 2 + (Breite Div 3) * d, Breite * (x + 1) - 2 + (Breite Div 3) * 2);
                    Brush.style := bsclear;
                  End;
                  textout(breite * (10) + (Breite Div 3) * (z Mod 3) + ((Breite Div 3) - Textwidth(inttostr(z + 1))) Div 2, Breite * (x + 1) + 1 + Breite Div 3, substitution[(z + 1)]);
                End;
              6..8: Begin
                  If TToolButton(form1.findcomponent('Toolbutton' + inttostr(11 + z))).Down Then Begin
                    brush.color := LightenColor;
                    pen.color := LightenColor;
                    brush.style := bssolid;
                    d := (z + 1) Mod 3;
                    If D = 0 Then d := 3;
                    ellipse(breite * (10) + 2 + (Breite Div 3) * (z Mod 3), Breite * (x + 1) + 2 + (breite Div 3) * 2, breite * (10) - 2 + (Breite Div 3) * d, Breite * (x + 1) - 2 + Breite);
                    Brush.style := bsclear;
                  End;
                  textout(breite * (10) + (Breite Div 3) * (z Mod 3) + ((Breite Div 3) - Textwidth(inttostr(z + 1))) Div 2, Breite * (x + 1) + 1 + (Breite Div 3) * 2, substitution[(z + 1)]);
                End;
            End;
          End;
      // Die linien zwischen den Zahlen
      pen.color := Gitterfarbe;
      For z := 1 To 10 Do Begin
        If Z <> 10 Then Begin
          moveto(Breite * z, 1);
          lineto(Breite * z, Breite);
          moveto(Breite * 10, Breite * z);
          lineto(Breite * 11, Breite * z);
        End;
        If Z <> 1 Then Begin
          moveto(Breite * z - 1, 1);
          lineto(Breite * z - 1, Breite);
          moveto(Breite * 10, Breite * z - 1);
          lineto(Breite * 11, Breite * z - 1);
        End;
      End;
    End;
    // Markieren der Felder die Permanent Markiert werden müssen
    For x := 1 To 9 Do
      If TToolbutton(form1.findcomponent('ToolButton' + inttostr(x))).down Then
        Mark(field, x);
    // Malen des Gitters
    For y := 0 To 8 Do
      For x := 0 To 8 Do Begin
        Case x Of
          0..2, 6..8: Begin
              If y In [0..2, 6..8] Then Begin
                brush.color := Bretthintergrundfarbe1;
                If Field[x, y].marked Then brush.color := markedColor1;
              End
              Else Begin
                brush.color := Bretthintergrundfarbe2;
                If Field[x, y].marked Then brush.color := markedColor2;
              End;
            End;
          3..5: Begin
              If y In [3..5] Then Begin
                brush.color := Bretthintergrundfarbe1;
                If Field[x, y].marked Then brush.color := markedColor1;
              End
              Else Begin
                brush.color := Bretthintergrundfarbe2;
                If Field[x, y].marked Then brush.color := markedColor2;
              End;
            End;
        End;
        // Farbe zum Markieren des Aktuellen Feldes
        If (x = mx) And (y = my) And Not Form1.checkbox6.checked Then brush.color := CursorMarker;
        // Malen des Feldes
        pen.color := Gitterfarbe;
        brush.Style := bssolid;
        rectangle(breite * (x + 1), Breite * (y + 1), breite * (x + 2), Breite * (y + 2));
        // Falls Zahlen Hervorgehoben werden sollen dann geschieht das hier !!
        If TToolbutton(form1.findcomponent('ToolButton' + inttostr(field[x, y].Value + 10))).down Then Begin
          brush.color := LightenColor;
          pen.color := LightenColor;
          Ellipse(breite * (x + 1) + 1, Breite * (y + 1) + 1, breite * (x + 2) - 1, Breite * (y + 2) - 1);
        End;
        // Malen der Beschriftung der Felder
        Brush.style := bsclear;
        // Malen der Pencil Zahlen
        If form1.Checkbox4.checked And (Field[x, y].value = 0) Then Begin
          font.size := breite Div 5;
          If ((mx = x) And (my = y)) Or Field[x, y].marked Then
            font.color := PencilcolorMarked
          Else
            font.color := Pencilcolor;
          For z := 0 To 8 Do
            If Field[x, y].Pencil[z] Then Begin
              Case z Of
                0..2: Begin
                    If TToolButton(form1.findcomponent('Toolbutton' + inttostr(11 + z))).Down Then Begin
                      brush.color := LightenColor;
                      pen.color := LightenColor;
                      brush.style := bssolid;
                      d := (z + 1) Mod 3;
                      If D = 0 Then d := 3;
                      ellipse(breite * (x + 1) + 2 + (Breite Div 3) * (z Mod 3), Breite * (y + 1) + 2 + 0, breite * (x + 1) - 2 + (Breite Div 3) * d, Breite * (y + 1) - 2 + Breite Div 3);
                      Brush.style := bsclear;
                    End;
                    textout(breite * (x + 1) + (Breite Div 3) * (z Mod 3) + ((Breite Div 3) - Textwidth(inttostr(z + 1))) Div 2, Breite * (y + 1) + 1 + 0, substitution[(z + 1)]);
                  End;
                3..5: Begin
                    If TToolButton(form1.findcomponent('Toolbutton' + inttostr(11 + z))).Down Then Begin
                      brush.color := LightenColor;
                      pen.color := LightenColor;
                      brush.style := bssolid;
                      d := (z + 1) Mod 3;
                      If D = 0 Then d := 3;
                      ellipse(breite * (x + 1) + 2 + (Breite Div 3) * (z Mod 3), Breite * (y + 1) + 2 + breite Div 3, breite * (x + 1) - 2 + (Breite Div 3) * d, Breite * (y + 1) - 2 + (Breite Div 3) * 2);
                      Brush.style := bsclear;
                    End;
                    textout(breite * (x + 1) + (Breite Div 3) * (z Mod 3) + ((Breite Div 3) - Textwidth(inttostr(z + 1))) Div 2, Breite * (y + 1) + 1 + Breite Div 3, substitution[(z + 1)]);
                  End;
                6..8: Begin
                    If TToolButton(form1.findcomponent('Toolbutton' + inttostr(11 + z))).Down Then Begin
                      brush.color := LightenColor;
                      pen.color := LightenColor;
                      brush.style := bssolid;
                      d := (z + 1) Mod 3;
                      If D = 0 Then d := 3;
                      ellipse(breite * (x + 1) + 2 + (Breite Div 3) * (z Mod 3), Breite * (y + 1) + 2 + (breite Div 3) * 2, breite * (x + 1) - 2 + (Breite Div 3) * d, Breite * (y + 1) - 2 + Breite);
                      Brush.style := bsclear;
                    End;
                    textout(breite * (x + 1) + (Breite Div 3) * (z Mod 3) + ((Breite Div 3) - Textwidth(inttostr(z + 1))) Div 2, Breite * (y + 1) + 1 + (Breite Div 3) * 2, substitution[(z + 1)]);
                  End;
              End;
            End;
        End;
        // Malen des Textes des Feldes
        If Field[x, y].Value <> 0 Then Begin
          // Textgröße
          Font.size := Breite Div 2;
          // Textfarbe
          If Field[x, y].Fixed Then
            font.color := fixedcolor
          Else
            font.color := FontColor;
          // Farbe für geratene Felder
          If (Field[x, y].Maybeed) Then Font.color := Maybeedcolor;
          // Malen des Feldinhaltes
          textout(breite * (x + 1) + (Breite - textwidth(inttostr(Field[x, y].value))) Div 2, Breite * (y + 1) + 1 + (Breite - textheight(inttostr(Field[x, y].value))) Div 2, substitution[(Field[x, y].value)]);
        End;
      End;
  End;
  //  Form1.canvas.Draw(0, 0, bm);
  Form1.Panel1.Canvas.Draw(0, 0, bm);
End;

Procedure Resetopt;
Var
  x: integer;
Begin
  With form1 Do Begin
    Button2.onclick(Nil);
    // Rücksetzen der ganzen Graphischen Zusatzsachen
    checkbox1.checked := false;
    checkbox2.checked := false;
    checkbox3.checked := false;
    checkbox4.checked := false;
    checkbox5.checked := false;
    checkbox6.checked := false;
    lc := 0;
    mx := 0;
    my := 0;
    For x := 1 To 19 Do Begin
      ttoolbutton(findcomponent('Toolbutton' + inttostr(x))).enabled := true;
      ttoolbutton(findcomponent('Toolbutton' + inttostr(x))).Down := false;
    End;
  End;
End;

(*
Berechnet den Korrespondierenden Punkt zu (x,y) im Feld (0..N-1) x (0..N-1) gespiegelt an
der Achse Direction ( 0..3 )
*)

Function Mirrow(x, y, n, Direction: Integer): TPoint;
Begin
  n := n - 1;
  Case Direction Of
    0: Begin // an der Waagrechten Spiegeln
        result.x := x;
        result.y := n - y;
      End;
    1: Begin // an der 1. Winkelhalbierenden
        result := point(y, x);
      End;
    2: Begin // and der Senkrechten
        result.x := n - x;
        result.y := y;
      End;
    3: Begin // an der 2. Winkelhalbierenden
        result := point(n - y, n - x);
      End
  Else // Fehlerfall
    result := point(x, y);
  End;
End;

// Diese Procedur benötigt ein Vollständiges Sudoku in der variable Field
// Dieses wir dann mit Hilfe von Solve umgewandelt in ein noch zu lösendes Sudoku

Procedure HackSudoku(Var Data: T3Field; Direction: Integer = -1);
Const
  MaxFehlercount = 25;
Var
  x, y, z: Integer;
  weiter: Boolean;
  Versuche: Integer;
  f: T3field;
  p: TPoint;
Begin
  If isready3(Data) Then Begin // zuerst mus geschaut werden ob das Rätsel überhaupt Komplett ist, sonst haben wir eine Endlosschleife
    form1.bytryanderror1.checked := false; // Ausschalten des Try and error teile's der Ki, da es sonst sinnlos wird
    zwangsabbruch := false;
    weiter := true; // Endlosschleife
    Versuche := 0; // Zähler für die Fehlversuche
    // Wegspeichern der Lösung
    For x := 0 To 8 Do
      For y := 0 To 8 Do
        f[x, y].value := Data[x, y].value;
    // Starten mit dem Löschen der Zahlen
    While weiter Do Begin
      Application.ProcessMessages;
      // Rücksetzen des Fieldes auf den zu letzt gefunden Stand
      For x := 0 To 8 Do
        For y := 0 To 8 Do
          Data[x, y].value := F[x, y].value;
      // Suchen des als nächstes zu löschenden Feldes
      x := random(9);
      y := random(9);
      While F[x, y].value = 0 Do Begin
        x := random(9);
        y := random(9);
      End;
      // Löschen des Feldes
      Data[x, y].value := 0;

      // Den Gespiegelten Punkt berechnen und Löschen
      p := Mirrow(x, y, 9, Direction);
      If (p.x < 0) Or (p.y < 0) Or (p.x > 8) Or (p.y > 8) Then Begin
        Raise exception.Create('Fehler in Mirrow: X=' + inttostr(x) + ' Y=' + Inttostr(y) + ' P.X=' + inttostr(p.x) + ' P.Y=' + inttostr(p.y) + ' Direction=' + inttostr(direction));
      End;
      Data[p.x, p.y].value := 0;

      // Lösen des Rätsels
      Solve(false, true, data);
      // Schaun ob es lösbar war
      If Not isready3(Data) Then
        inc(Versuche) // Wenn nicht dann ist das ein Fehlversuch mehr
      Else Begin // Wenn es lösbar war wird der Wert auch in der Sicherung gelöscht.
        f[x, y].value := 0;
        f[p.x, p.y].value := 0;
        Versuche := 0;
      End;
      // Abbruch der Endlosschleife
      If (Versuche > MaxFehlercount) Or zwangsabbruch Then Begin
        Weiter := false;
      End;
    End;
    // Umschreiben der Sicherungskopie in das Ausgabe Field und dann setzen der entsprechenden Value's
    For x := 0 To 8 Do Begin
      For y := 0 To 8 Do Begin
        Data[x, y].value := f[x, y].value;
        Data[x, y].Fixed := Not (Data[x, y].value = 0);
        Data[x, y].marked := false;
        For z := 0 To 8 Do Begin
          Data[x, y].Pencil[z] := false;
        End;
      End;
    End;
  End;
End;

Procedure TForm1.Beenden1Click(Sender: TObject);
Begin
  Close;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  {
  To Do Liste:

  Evtl bringt es was wenn man die Sudoku's alle 200000 schritte um 90 Grad dreht für 5x5, bzw 4x4

  Ki fertig Schreiben die die Dinger Löst;

  Insbesondere Fehlen Noch: (Wenn sie eingebaut sind müssen sie im Objektinspektor auf Visible = True gesetzt werden, ebenso bei Form7)

  byBlockandColumninteractions1 + Hilfe für diese KI;
  byblockandblockinteractions1 + Hilfe für diese KI;
  byXWingSwordfish1 + Hilfe für diese KI;
  ForcingChains1 + Hilfe für diese KI;
  //}
  Randomize;
  bm := tbitmap.create;
  bm.width := form1.width;
  bm.height := form1.height;
  Resetopt;
  Readini;
  SaveDialog1.initialdir := ExtractFilePath(application.exename);
  openDialog1.initialdir := ExtractFilePath(application.exename);
  ClearField(Field);
  Caption := 'Sudoku ver. : ' + ver + ' by Corpsman | www.Corpsman.de |';
  Panel1.OnKeyPress := OnKeyPress;
End;

Procedure TForm1.FormPaint(Sender: TObject);
Begin
  Drawfield;
End;

Procedure TForm1.FormResize(Sender: TObject);
Var
  x: Integer;
Begin
  If Form1.height < 480 Then Form1.height := 480;
  If form1.Width < form1.height + 130 Then form1.Width := form1.height + 130;
  //  Breite := min(Form1.height - 32, Form1.width - 120) Div 11;
  Breite := min(Panel1.ClientHeight, Panel1.ClientWidth) Div 11;
  For x := 1 To 6 Do
    TCheckbox(findcomponent('Checkbox' + inttostr(x))).left := Form1.width - 195;
  button1.left := Form1.width - 160;
  button2.left := Form1.width - 160;
  If Assigned(bm) Then Begin
    bm.width := Panel1.ClientWidth;
    bm.height := Panel1.ClientHeight;
    Drawfield;
  End;
End;

Procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
End;

Procedure TForm1.FormKeyPress(Sender: TObject; Var Key: Char);
// Fügt unter beachtung aller Bedingugnen eine Zahl in das Feld ein
  Procedure AddZahl(Value, X, y: integer);
  Var
    c, d, a, b, z: integer;
    e: Boolean;
  Begin
    // Fixed Zahlen können nicht überschrieben werden !!
    If Field[x, y].Fixed And Not checkbox2.checked Then Begin
      showmessage('Field is fixed, you cannot override it!');
      exit;
    End;
    // Die eingabe eines Pencil Wertes
    If checkbox3.checked Then Begin
      If Value = 0 Then exit;
      Field[x, y].Pencil[value - 1] := Not Field[x, y].Pencil[value - 1];
      // überprüfen ob der Pencil überhaupt sinn macht
      If Field[x, y].Pencil[value - 1] Then Begin
        // Prüfen ob die Zahl Waagrecht / Senkrecht rein darf
        e := true;
        For z := 0 To 8 Do Begin
          If (Field[z, y].value = Value) And Not (z = x) Then e := false;
          If (Field[x, z].value = Value) And Not (z = y) Then e := false;
        End;
        // Prüfen ob die Zahl in das entsprechende 9er Feld Darf
        a := x - (x Mod 3);
        b := y - (y Mod 3);
        For c := 0 To 2 Do
          For d := 0 To 2 Do
            // Prüfen der Zahl im 9er Feld auser dem gewählten Feld
            If ((a + c) <> x) Or ((b + d) <> y) Then Begin
              If Field[a + c, b + d].value = Value Then e := false;
            End;
        If invalidnallow Then e := true; // Wenn auch ungültige Zahlen eingegeben werden können.
        If Not E Then Begin
          Field[x, y].Pencil[value - 1] := false;
          showmessage('Character for this field impossible.');
        End;
      End;
    End
      // Die eingabe eines normalen wertes
    Else Begin
      // Löschen eines Wertes
      If Value = 0 Then Begin
        For c := 0 To 8 Do
          For d := 0 To 8 Do
            Field[c, d].marked := false;
        // nur in speziell des Falles das eine Zahl gelöscht wird darf sie bei den Pencils hinzugefügt werden.
        UnPencil(x, y, Field[x, Y].value, field);
      End;
      // Prüfen ob die Zahl Waagrecht / Senkrecht rein darf
      e := true;
      For z := 0 To 8 Do Begin
        If (Field[z, y].value = Value) And Not (z = x) Then e := false;
        If (Field[x, z].value = Value) And Not (z = y) Then e := false;
      End;
      // Prüfen ob die Zahl in das entsprechende 9er Feld Darf
      a := x - (x Mod 3);
      b := y - (y Mod 3);
      For c := 0 To 2 Do
        For d := 0 To 2 Do
          // Prüfen der Zahl im 9er Feld auser dem gewählten Feld
          If ((a + c) <> x) Or ((b + d) <> y) Then Begin
            If Field[a + c, b + d].value = Value Then
              e := false;
          End;
      // Wenn die Zahl gelöscht wird
      If Value = 0 Then e := true;
      If invalidnallow Then e := true; // Wenn auch ungültige Zahlen eingegeben werden können.
      // das Feld Aktualisieren
      If e Then Begin
        // Zuweisen des neuen Feldwertes
        If Field[x, y].value = value Then Begin
          If Not (Not Field[x, y].Fixed And checkbox2.checked) Then Begin
            // nur in speziell des Falles das eine Zahl gelöscht wird darf sie bei den Pencils hinzugefügt werden.
            For c := 0 To 8 Do
              For d := 0 To 8 Do
                Field[c, d].marked := false;
            UnPencil(x, y, Field[x, Y].value, field);
            Field[x, y].value := 0; // Rücksetzen des Feldwertes
          End;
        End
        Else Begin
          If Field[x, y].value <> 0 Then Begin
            // nur in speziell des Falles das eine Zahl gelöscht wird darf sie bei den Pencils hinzugefügt werden.
            For c := 0 To 8 Do
              For d := 0 To 8 Do
                Field[c, d].marked := false;
            UnPencil(x, y, Field[x, Y].value, field);
          End;
          Field[x, y].value := value; // Setzen des Feldes mit dem Wert
        End;
        // zuweisen ob Fixed wert, oder nur normale Zahl
        If Field[x, y].value <> 0 Then
          Field[x, y].Fixed := form1.checkbox2.checked
        Else
          Field[x, y].fixed := false;
        // Ermitteln der Pencil Werte
        If checkbox4.checked Then GetPencil(field);
      End
      Else
        showmessage('Your Number is not allowed in this position');
    End;
  End;
  {
    Function keygueltig(taste: Char): boolean;
    Var
      erg: Boolean;
      z: integer;
    Begin
      erg := false;
      For z := 1 To 9 Do
        If substitution[1] = Taste Then Begin
          erg := true;
          break;
        End;
      result := erg;
    End;

    Function getkeyindex(Taste: Char): integer;
    Var
      erg: integer;
      z: integer;
    Begin
      erg := 0;
      For z := 1 To 9 Do
        If substitution[1] = Taste Then Begin
          erg := z;
          break;
        End;
      result := erg;
    End;
       }
Var
  x1, x2, y1, y2: integer;
  zah: Array[1..9] Of 0..9;
  a: Boolean;
Begin
  If isreadyUser3(field, false) And Not (key In ['a', 'A', '0', 's', 'S', 'd', 'D', 'w', 'W']) Then exit;
  // Wenn wir uns im edit Line Pencil Modus befinden
  If Checkbox6.checked Then Begin
    // Bewegen des Cursors
    If (key In ['a', 'A', 's', 'S', 'd', 'D', 'w', 'W']) Then Begin
      If ((Key = 'a') Or (Key = 'A') Or (Key = 'w') Or (Key = 'W')) And (lc > 0) Then dec(lc);
      If ((Key = 's') Or (Key = 'S') Or (Key = 'd') Or (Key = 'D')) And (lc < 17) Then inc(lc);
      Drawfield;
      exit;
    End
    Else Begin
      // Eingabe der Pencil werte
      If Key In ['1'..'9'] Then Begin
        a := true;
        x2 := strtoint(key);
        If lc < 9 Then Begin
          For x1 := 0 To 8 Do
            If x2 = Field[lc, x1].value Then a := false;
        End
        Else Begin
          For x1 := 0 To 8 Do
            If x2 = Field[x1, lc - 9].value Then a := false;
        End;
        If invalidnallow Then a := true; // Wenn auch ungültige Zahlen eingegeben werden können.
        If A Then
          Linepencil[lc][strtoint(key) - 1] := Not Linepencil[lc][strtoint(key) - 1]
        Else
          showmessage('Character for this field impossible.');
      End;
      Drawfield;
    End;
    exit;
  End;
  // Eingaben im Feld
  // Steuerung des Cursors
  If (Key = 's') Or (Key = 'S') Then
    If my < 8 Then inc(my);
  If (Key = 'w') Or (Key = 'W') Then
    If my > 0 Then dec(my);
  If (Key = 'a') Or (Key = 'A') {Or (key = #37)} Then
    If mx > 0 Then dec(mx);
  If (Key = 'd') Or (Key = 'D') Then
    If mx < 8 Then inc(mx);
  // Einfügen und Löschen von Zahlen
  If (Key In ['0'..'9']) And (mx <> -1) Then Begin
    AddZahl(StrToInt(key), mx, my);
    Field[mx, my].Maybeed := false;
  End;
  // Einfügen der Geschätzten Zahlen
  If mx <> -1 Then Begin
    If (Key In ['!', '"', '§', '$', '%', '&', '/', '(', ')']) Then Begin
      Field[mx, my].Maybeed := True;
      If Key = '!' Then
        AddZahl(1, mx, my);
      If Key = '"' Then
        AddZahl(2, mx, my);
      If Key = '§' Then
        AddZahl(3, mx, my);
      If Key = '$' Then
        AddZahl(4, mx, my);
      If Key = '%' Then
        AddZahl(5, mx, my);
      If Key = '&' Then
        AddZahl(6, mx, my);
      If Key = '/' Then
        AddZahl(7, mx, my);
      If Key = '(' Then
        AddZahl(8, mx, my);
      If Key = ')' Then
        AddZahl(9, mx, my);
      If Checkbox5.checked Then
        getLinePencil(Field);
    End;
  End;
  // Hohlen der ganzen Linepencil sachen
  If Checkbox5.checked Then
    getLinePencil(Field);
  // überprüfen ob vielleicht schon von einer Zahl alle gefunden wurden
  For x1 := 1 To 9 Do
    zah[x1] := 0;
  For x1 := 0 To 8 Do
    For y1 := 0 To 8 Do
      If Field[x1, y1].value <> 0 Then
        inc(zah[Field[x1, y1].value]);
  {  If Key = '0' Then Begin

    End;}
  For x1 := 1 To 9 Do
    If zah[x1] = 9 Then Begin
      TToolbutton(Findcomponent('ToolButton' + inttostr(x1))).enabled := false;
      TToolbutton(form1.Findcomponent('ToolButton' + inttostr(x1 + 10))).enabled := false;
      TToolbutton(form1.Findcomponent('ToolButton' + inttostr(x1 + 10))).Down := false;
      TToolbutton(Findcomponent('ToolButton' + inttostr(x1))).Down := False;
      For x2 := 0 To 8 Do
        For y2 := 0 To 8 Do
          Field[x2, y2].marked := false;
    End
    Else Begin
      TToolbutton(Findcomponent('ToolButton' + inttostr(x1))).enabled := true;
      TToolbutton(form1.Findcomponent('ToolButton' + inttostr(x1 + 10))).enabled := true;
    End;
  // Schauen ob Fertig.
//  a := true;
//  For x1 := 1 To 9 Do
//    If TToolbutton(Findcomponent('ToolButton' + inttostr(x1))).enabled Then a := false;
  a := isreadyUser3(field, true);
  // Zeichnen des Feldes
  drawfield;
  // Anzeigen das Fertig
  If A Then Begin
    Drawfield;
    If Not (key In ['a', 'A', 's', 'S', 'd', 'D', 'w', 'W', '0']) Then
      showmessage('You solved the Sudoku.');
  End;
End;

Procedure TForm1.CheckBox1Click(Sender: TObject);
Var
  x, y: Integer;
Begin
  If Not Checkbox1.checked Then Begin
    For x := 0 To 8 Do
      For y := 0 To 8 Do
        Field[x, y].marked := false;
    If (mx In [0..8]) And (my In [0..8]) Then
      Field[mx, my].Marked := true;
  End;
  drawfield;
End;

Procedure TForm1.CheckBox3Click(Sender: TObject);
Begin
  If checkbox3.checked Then Begin
    checkbox2.checked := false;
    checkbox6.checked := false;
    checkbox4.checked := True;
  End;
End;

Procedure TForm1.CheckBox2Click(Sender: TObject);
Begin
  If Checkbox2.checked Then Begin
    checkbox3.checked := false;
    checkbox6.checked := false;
  End;
End;

Procedure TForm1.CheckBox4Click(Sender: TObject);
Begin
  If Checkbox4.checked Then GetPencil(field);
  If checkbox3.checked And Not Checkbox4.checked Then checkbox3.checked := false;
  drawfield;
End;

Procedure TForm1.Clearfield1Click(Sender: TObject);
Begin
  Resetopt;
  ClearField(Field);
  Drawfield;
End;

Procedure TForm1.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  x1, y1: integer;
Begin
  // die Auswahl für die Linepencil's
  If Checkbox6.checked Then Begin
    If (X >= Breite) And (x <= Breite * 10) And (y <= breite) Then Begin
      lc := x Div Breite - 1;
    End;
    If (X > Breite * 10) And (x <= Breite * 11) And (y >= Breite) And (y <= Breite * 10) Then Begin
      lc := y Div Breite + 8;
    End;
  End
  Else Begin
    // Löschen aller Markierungen
    mx := -1;
    my := -1;
    For x1 := 0 To 8 Do
      For y1 := 0 To 8 Do
        Field[x1, y1].marked := false;
    If (X >= Breite) And (x <= Breite * 10) And
      (y >= Breite) And (y <= Breite * 10) Then Begin
      // Ausrechnen der Koordinaten des neu Markierten Feldes
      x1 := x Div Breite - 1;
      y1 := y Div Breite - 1;
      mx := x1;
      my := y1;
    End;
  End;
  Drawfield;
End;

Procedure TForm1.Panel1Paint(Sender: TObject);
Begin
  Drawfield;
End;

Procedure TForm1.Support1Click(Sender: TObject);
Begin
  Showmessage('Sudoku ' + ver + #13 + 'Support : http://www.corpsman.de.vu/');

  // Uses shellapi;
  // ShellExecute(0,Nil,PChar('http://www.corpsman.de.vu/'),Nil,Nil,SW_NORMAL);

End;

Procedure TForm1.Warranty1Click(Sender: TObject);
Begin
  Showmessage('This programm is freeware' + #13 +
    'in case of this there is no warranty.' + #13 + #13 +
    'The programmer takes no warrenty for damages in soft- or hardware.');
End;

Procedure TForm1.Save1Click(Sender: TObject);
Var
  F: TFilestream;
Begin
  If Savedialog1.execute Then Begin
    SaveDialog1.initialdir := ExtractFilePath(Savedialog1.Filename);
    openDialog1.initialdir := ExtractFilePath(Savedialog1.Filename);
    f := Tfilestream.create(Savedialog1.Filename, fmCreate Or fmOpenWrite);
    f.write(Field, sizeof(Field));
    f.write(linepencil, sizeof(linepencil));
    f.Free;
  End;
End;

Procedure TForm1.Load1Click(Sender: TObject);
Var
  x, y: integer;
  F: TFilestream;
Begin
  If opendialog1.execute Then Begin
    SaveDialog1.initialdir := ExtractFilePath(opendialog1.Filename);
    openDialog1.initialdir := ExtractFilePath(opendialog1.Filename);
    // Zurücksetzen der Graphischen Hilfsmittel
    ResetOpt;
    f := Tfilestream.create(opendialog1.Filename, fmOpenRead);
    f.Read(Field, sizeof(Field));
    f.Read(linepencil, sizeof(linepencil));
    f.Free;
    For x := 0 To 8 Do
      For y := 0 To 8 Do
        Field[x, y].marked := false;
    drawfield;
  End;
End;

Procedure TForm1.Resetfield1Click(Sender: TObject);
Var
  x, y: integer;
Begin
  Resetopt;
  // Löschen aller Einträge des Users
  For x := 0 To 8 Do
    For y := 0 To 8 Do Begin
      If Not (Field[x, y].Fixed) Then Field[x, y].value := 0;
      Field[x, y].MArked := false;
    End;
  Drawfield;
End;

Procedure TForm1.ToolButton1Click(Sender: TObject);
Var
  x, y: integer;
Begin
  If TToolbutton(Sender).Down = false Then Begin
    For x := 0 To 8 Do
      For y := 0 To 8 Do
        Field[x, y].marked := false;
  End;
  Drawfield;
End;

Procedure TForm1.ToolButton11Click(Sender: TObject);
Begin
  Drawfield;
End;

Procedure TForm1.Colors1Click(Sender: TObject);
Begin
  Form2.image2.canvas.brush.color := Bretthintergrundfarbe1;
  Form2.image3.canvas.brush.color := Bretthintergrundfarbe2;
  Form2.image4.canvas.brush.color := Maybeedcolor;
  Form2.image5.canvas.brush.color := MarkedColor1;
  Form2.image6.canvas.brush.color := MarkedColor2;
  Form2.image7.canvas.brush.color := CursorMarker;
  Form2.image8.canvas.brush.color := Fixedcolor;
  Form2.image9.canvas.brush.color := Gitterfarbe;
  Form2.image10.canvas.brush.color := FontColor;
  Form2.image11.canvas.brush.color := Pencilcolor;
  Form2.image12.canvas.brush.color := PencilcolorMarked;
  Form2.image13.canvas.brush.color := LightenColor;
  Form2.image14.canvas.brush.color := FormBackground;
  Form2.showmodal;
End;

Procedure TForm1.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Begin
  Writeini;
  bm.free;
  bm := Nil;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  x, y, z: integer;
Begin
  For x := 0 To 17 Do
    For y := 0 To 8 Do
      Linepencil[x][y] := true;
  getlinepencil(Field);
  For x := 0 To 8 Do
    For y := 0 To 8 Do
      For z := 0 To 8 Do
        field[x, y].Pencil[z] := true;
  GetPencil(field);
  //  checkbox4.checked := true;
  Drawfield;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Var
  x, y, z: integer;
Begin
  // Löschen der Linepencil's
  For x := 0 To 17 Do
    For y := 0 To 8 Do
      Linepencil[x][y] := false;
  // Löschen der Field pencil's
  For x := 0 To 8 Do
    For y := 0 To 8 Do
      For z := 0 To 8 Do
        field[x, y].Pencil[z] := false;
  //  checkbox3.checked := true;
  Drawfield;
End;

Procedure TForm1.CheckBox5Click(Sender: TObject);
Begin
  If Checkbox5.checked Then getLinePencil(Field);
  If Not Checkbox5.checked Then Begin
    checkbox6.checked := false;
  End;
  drawfield;
End;

Procedure TForm1.CheckBox6Click(Sender: TObject);
Begin

  Checkbox2.checked := false;
  If Checkbox6.checked Then Begin
    checkbox5.checked := True;
    Checkbox3.checked := false;
  End;
  Drawfield;
End;

Procedure TForm1.ToolButton1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  x1, y1: integer;
Begin
  If SSright In shift Then Begin
    For x1 := 0 To 8 Do
      For y1 := 0 To 8 Do
        Field[x1, y1].marked := false;
    For x1 := 1 To 9 Do
      TTOolbutton(form1.findcomponent('Toolbutton' + inttostr(x1))).down := false;
    TTOolbutton(sender).Down := true;
    Drawfield;
  End;
End;

Procedure TForm1.General2Click(Sender: TObject);
Begin
  form3.checkbox2.checked := invalidnallow;
  Form3.checkbox1.checked := unpencilallow;
  form3.showmodal;
End;

Procedure TForm1.ToolButton11MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  x1: integer;
Begin
  If SSright In shift Then Begin
    For x1 := 11 To 19 Do
      TTOolbutton(form1.findcomponent('Toolbutton' + inttostr(x1))).down := false;
    TTOolbutton(sender).Down := true;
    Drawfield;
  End;
End;

Procedure TForm1.General1Click(Sender: TObject);
Begin
  form4.memo1.text := rules;
  form4.showmodal;
End;

Procedure TForm1.Solveit1Click(Sender: TObject);
Begin
  If Not (Sudoku3solvable(field)) Then Begin
    showmessage('Impossible to solve Sudoku');
    Drawfield;
  End
  Else Begin
    Solve(False, false, field);
    Showmessage('Ready');
    Drawfield;
  End;
End;

Procedure TForm1.Solvestep1Click(Sender: TObject);
Var
  a: Boolean;
  x1: integer;
Begin
  If Not (Sudoku3solvable(field)) Then Begin
    showmessage('Impossible to solve Sudoku');
  End
  Else Begin
    If form1.bytryanderror1.checked Then Begin
      If ID_NO = application.messagebox(pchar('You slected the solving method by try and error.' + #13 + 'If this step is necessary your Sudoku will be completed at all.' + #13 + #13 + 'do you want this ?'), 'Question', MB_YESNO + MB_ICONQUESTION) Then
        exit;
    End;
    Solve(true, false, field);
    // Schauen ob Fertig.
    a := true;
    For x1 := 1 To 9 Do
      If TToolbutton(Findcomponent('ToolButton' + inttostr(x1))).enabled Then a := false;
    // Zeichnen des Feldes
    // Anzeigen das Fertig
    If A Then Begin
      mx := -1;
      my := -1;
      Drawfield;
      showmessage('You solved the Sudoku.');
    End;
  End;
End;

Procedure TForm1.Allowall1Click(Sender: TObject);
Begin
  byhiddensingle1.checked := true;
  bynakedsingle1.checked := true;
  bytryanderror1.checked := true;
  bynakedsubset1.checked := true;
  byhiddensubset1.checked := true;
  byBlockandColumninteractions1.checked := true;
  byblockandblockinteractions1.checked := true;
  byXWingSwordfish1.checked := true;
  byXYWing1.checked := true;
  ForcingChains1.checked := true;
End;

Procedure TForm1.Allownone1Click(Sender: TObject);
Begin
  byhiddensingle1.checked := false;
  bynakedsingle1.checked := false;
  bytryanderror1.checked := false;
  bynakedsubset1.checked := false;
  byhiddensubset1.checked := false;
  byBlockandColumninteractions1.checked := false;
  byblockandblockinteractions1.checked := false;
  byXWingSwordfish1.checked := False;
  byXYWing1.checked := false;
  ForcingChains1.checked := false;
End;

Procedure TForm1.bymarkingnumbersClick(Sender: TObject);
Begin
  TMenuItem(sender).Checked := Not TMenuItem(sender).Checked;
End;

Procedure TForm1.Modify1Click(Sender: TObject);
Var
  x: Integer;
Begin
  For x := 1 To 9 Do Begin
    TCombobox(Form5.findcomponent('Combobox' + inttostr(x))).text := substitution[x];
  End;
  Form5.showmodal;
End;

Procedure TForm1.Puzzle1Click(Sender: TObject);
Begin
  // Übernehmen der Solvin Methoden
  form7.checkbox1.checked := byhiddensingle1.checked;
  form7.checkbox2.checked := bynakedsingle1.checked;
  form7.checkbox3.checked := byBlockandColumninteractions1.checked;
  form7.checkbox4.checked := byblockandblockinteractions1.checked;
  form7.checkbox5.checked := bynakedsubset1.checked;
  form7.checkbox6.checked := byhiddensubset1.checked;
  form7.checkbox7.checked := byXWingSwordfish1.checked;
  form7.checkbox8.checked := byXYWing1.checked;
  form7.checkbox9.checked := ForcingChains1.checked;
  // Deaktivieren für das Drucken
  If Sender <> Nil Then Begin
    Form7.showmodal;
    // Nach dem Schliesen sollte das Hauptfenster wieder aktiviert werden
    Form1.SetFocus;
    Drawfield;
  End;
End;

Procedure TForm1.Maybenumbersgoodnumbers1Click(Sender: TObject);
Var
  x, y: Integer;
Begin
  For x := 0 To 8 Do
    For y := 0 To 8 Do
      If Field[x, y].Maybeed Then Field[x, y].Maybeed := false;
  Drawfield;
End;

Procedure TForm1.MaybanumberclearField1Click(Sender: TObject);
Var
  x, y: Integer;
Begin
  For x := 0 To 8 Do
    For y := 0 To 8 Do
      If Field[x, y].Maybeed Then Begin
        Field[x, y].Maybeed := false;
        UnPencil(x, y, Field[x, y].Value, Field);
        Field[x, y].Value := 0;
      End;
  Drawfield;
End;

Procedure TForm1.SpecialPuzzle1Click(Sender: TObject);
Begin
  showmessage('Not complete Implemented');
  form8.showmodal;
End;

Procedure TForm1.Print1Click(Sender: TObject);
Begin
  Form9.showmodal;
End;

Procedure TForm1.N4x41Click(Sender: TObject);
Begin
  showmessage('These Sudoku''s were something Special, partly there Debuggininfo''s aviable.' + #13#13 +
    'If you want to so the progress of the creater then' + #13 + 'click on the field while the creating message is shown.' + #13#13 +
    'Normal time for creating a Sudoko with this size 10 - 20 sek.');
  Form11.showmodal;
End;

Procedure TForm1.N5x51Click(Sender: TObject);
Begin
  showmessage('These Sudoku''s were something Special, partly there Debuggininfo''s aviable.' + #13#13 +
    'If you want to so the progress of the creater then' + #13 + 'click on the field while the creating message is shown.' + #13#13 +
    'Normal time for creating a Sudoko with this size 30 - 90 sek.');
  Form13.showmodal;
End;

Procedure TForm1.Info1Click(Sender: TObject);
Var
  x, y, z: integer;
Begin
  z := 0;
  For x := 0 To 8 Do
    For y := 0 To 8 Do
      If Field[x, y].value <> 0 Then inc(z);
  Showmessage('This Sudoku needs ' + inttostr(9 * 9) + ' numbers to be complete.' + #13#13 +
    'At the moment there were ' + inttostr(z) + ' numbers inserted.');
End;

Procedure TForm1.N2x21Click(Sender: TObject);
Begin
  Form16.showmodal;
End;

End.



