(******************************************************************************)
(* Sudoku                                                          ??.??.2005 *)
(*                                                                            *)
(* Version     : see usudoku.pas                                              *)
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
(* History     : see usudoku.pas                                              *)
(*                                                                            *)
(******************************************************************************)

Unit Unit1;

{$MODE ObjFPC}{$H+}

Interface

Uses
  SysUtils, Graphics, Forms, Classes, Controls, Dialogs, Menus,
  StdCtrls, ComCtrls, usudoku, Sudoku3x3, ExtCtrls, lcltype;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    MainMenu1: TMainMenu;
    Datei1: TMenuItem;
    Beenden1: TMenuItem;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    Load1: TMenuItem;
    PaintBox1: TPaintBox;
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
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormResize(Sender: TObject);
    Procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure FormKeyPress(Sender: TObject; Var Key: Char);
    Procedure CheckBox1Click(Sender: TObject);
    Procedure CheckBox3Click(Sender: TObject);
    Procedure CheckBox2Click(Sender: TObject);
    Procedure CheckBox4Click(Sender: TObject);
    Procedure Clearfield1Click(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
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
    ffield: TSudoku;
  public
    { Public-Deklarationen }
    mx, my: integer; // globalen x,y Koordinaten der Maus im Feld
    Procedure Drawfield(Sender: TObject); // TODO: Muss Private werden
  End;

Var
  Form1: TForm1;
  bm: Tbitmap; // gegen das Flimmern
  Field: T3field; // Das Spielfeld

Procedure Solve(Step, invisible: boolean; Var Data: T3field);
Procedure HackSudoku(Var Data: T3Field; Direction: Integer = -1);
// die nachfolgenden Proceduren sind nur damit Ki nicht nach unten Kopiert werden mus hier oben
Procedure Resetopt;
//Procedure Marknumbers(Value: integer; Var Data: T3field);
//Procedure GetrealPencilnumbers(Var data: T3field);
Procedure getLinePencil(Var Data: T3Field);

Implementation

Uses
  math
  , Unit2 // Edit Color Dialog
  , Unit3 // Settings
  , Unit4 // Online Help
  , Unit5 // Modify dialog
  , Unit6 // Progressbar during creation
  , Unit7 // New Dialog
  , Unit8 // Mask Editor
  , Unit9 // Print Dialog
  // Unit10 // Print Setup Dialog
  , Unit11 // 4x4 Fields
  // Unit12 // Print Detail dialog 4x4 ?
  , Unit13 // 5x5 Fields
  // Unit14 // Print Detail dialog 5x5 ?
  // Unit15 // New Dialog for 2x2, 4x4, 5x5
  , Unit16 // 2x2 Fields
  ;

{$R *.lfm}

// Ist Step = True dann wird nur ein Step gemacht bei step = False, wird das Rätsel Komplett gelöst.

Procedure Solve(Step, invisible: boolean; Var Data: T3field);

  Procedure setfocus(x, y: integer);
  Begin
    If Step Then Begin
      form1.mx := x;
      form1.my := y;
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
    form1.Drawfield(Nil);
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
  // Unpencil für die Lines
  Linepencil[x][Value - 1] := true;
  Linepencil[9 + y][Value - 1] := true;
End;

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

Procedure TForm1.Drawfield(Sender: TObject);
Var
  Info: TRenderInfo;
  i: Integer;
Begin
  If bm = Nil Then exit;
  If Form1.checkbox1.checked And (mx > -1) Then Mark(field, field[mx, my].value);
  // Markieren der Felder die Permanent Markiert werden müssen
  For i := 1 To 9 Do
    If TToolbutton(form1.findcomponent('ToolButton' + inttostr(i))).down Then
      Mark(field, i);
  ffield.LoadFrom(Field);
  info.Cursor := point(mx, my);
  setlength(info.NumberHighLights, 9);
  setlength(info.NumberMarks, 9);
  For i := 0 To 8 Do Begin
    info.NumberHighLights[i] := TToolButton(form1.findcomponent('Toolbutton' + inttostr(11 + i))).Down
  End;
  info.Show_Pencils_Numbers := Checkbox4.checked;
  info.Show_Line_Pencil_numbers := Checkbox5.checked;
  info.Edit_Line_Pencil_Numbers := Checkbox6.checked;
  ffield.RenderTo(bm.Canvas, info);
  //  Form1.canvas.Draw(0, 0, bm);
  Form1.PaintBox1.Canvas.Draw(0, 0, bm);
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

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  ffield.Free;
  ffield := Nil;
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
  ffield := TSudoku.Create(3);
  bm := tbitmap.create;
  bm.width := form1.width;
  bm.height := form1.height;
  Resetopt;
  Readini;
  SaveDialog1.initialdir := ExtractFilePath(application.exename);
  openDialog1.initialdir := ExtractFilePath(application.exename);
  mx := 0;
  my := 0;
  Caption := 'Sudoku ver. : ' + ver + ' by Corpsman | www.Corpsman.de |';
  //PaintBox1.OnKeyPress := OnKeyPress;
End;

Procedure TForm1.FormResize(Sender: TObject);
Var
  x: Integer;
Begin
  If Form1.height < 480 Then Form1.height := 480;
  If form1.Width < form1.height + 130 Then form1.Width := form1.height + 130;
  //  Breite := min(Form1.height - 32, Form1.width - 120) Div 11;
  Breite := min(PaintBox1.ClientHeight, PaintBox1.ClientWidth) Div 11;
  For x := 1 To 6 Do
    TCheckbox(findcomponent('Checkbox' + inttostr(x))).left := Form1.width - 195;
  button1.left := Form1.width - 160;
  button2.left := Form1.width - 160;
  If Assigned(bm) Then Begin
    bm.width := PaintBox1.ClientWidth;
    bm.height := PaintBox1.ClientHeight;
    Drawfield(Nil);
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
      Drawfield(Nil);
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
      Drawfield(Nil);
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
  Drawfield(Nil);
  // Anzeigen das Fertig
  If A Then Begin
    Drawfield(Nil);
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
  Drawfield(Nil);
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
  Drawfield(Nil);
End;

Procedure TForm1.Clearfield1Click(Sender: TObject);
Begin
  Resetopt;
  mx := 0;
  my := 0;
  ffield.LoadFrom(Field);
  ffield.ClearField;
  ffield.StoreTo(Field);
  Drawfield(Nil);
End;

Procedure TForm1.FormShow(Sender: TObject);
Begin
  PaintBox1.Invalidate;
End;

Procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
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
  Drawfield(Nil);
End;

Procedure TForm1.Panel1Paint(Sender: TObject);
Begin
  Drawfield(Nil);
End;

Procedure TForm1.Support1Click(Sender: TObject);
Begin
  Showmessage('Sudoku ' + ver + LineEnding + 'Support : http://www.corpsman.de/');
End;

Procedure TForm1.Warranty1Click(Sender: TObject);
Begin
  Showmessage('This programm is freeware' + LineEnding +
    'in case of this there is no warranty.' + LineEnding + LineEnding +
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
    Drawfield(Nil);
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
  Drawfield(Nil);
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
  Drawfield(Nil);
End;

Procedure TForm1.ToolButton11Click(Sender: TObject);
Begin
  Drawfield(Nil);
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
  Drawfield(Nil);
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
  Drawfield(Nil);
End;

Procedure TForm1.CheckBox5Click(Sender: TObject);
Begin
  If Checkbox5.checked Then getLinePencil(Field);
  If Not Checkbox5.checked Then Begin
    checkbox6.checked := false;
  End;
  Drawfield(Nil);
End;

Procedure TForm1.CheckBox6Click(Sender: TObject);
Begin

  Checkbox2.checked := false;
  If Checkbox6.checked Then Begin
    checkbox5.checked := True;
    Checkbox3.checked := false;
  End;
  Drawfield(Nil);
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
    Drawfield(Nil);
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
    Drawfield(Nil);
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
    Drawfield(Nil);
  End
  Else Begin
    Solve(False, false, field);
    Showmessage('Ready');
    Drawfield(Nil);
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
      If ID_NO = application.messagebox(pchar('You slected the solving method by try and error.' + LineEnding +
        'If this step is necessary your Sudoku will be completed at all.' + LineEnding + LineEnding +
        'do you want this ?'), 'Question', MB_YESNO + MB_ICONQUESTION) Then
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
      Drawfield(Nil);
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
  form5.Init(ffield, @Drawfield);
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
    Drawfield(Nil);
  End;
End;

Procedure TForm1.Maybenumbersgoodnumbers1Click(Sender: TObject);
Var
  x, y: Integer;
Begin
  For x := 0 To 8 Do
    For y := 0 To 8 Do
      If Field[x, y].Maybeed Then Field[x, y].Maybeed := false;
  Drawfield(Nil);
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
  Drawfield(Nil);
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
  showmessage('These Sudoku''s were something Special, partly there Debuggininfo''s aviable.' + LineEnding + LineEnding +
    'If you want to so the progress of the creater then' + LineEnding +
    'click on the field while the creating message is shown.' + LineEnding + LineEnding +
    'Normal time for creating a Sudoko with this size 10 - 20 sek.');
  Form11.showmodal;
End;

Procedure TForm1.N5x51Click(Sender: TObject);
Begin
  showmessage('These Sudoku''s were something Special, partly there Debuggininfo''s aviable.' + LineEnding + LineEnding +
    'If you want to so the progress of the creater then' + LineEnding +
    'click on the field while the creating message is shown.' + LineEnding + LineEnding +
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
  Showmessage('This Sudoku needs ' + inttostr(9 * 9) + ' numbers to be complete.' + LineEnding + LineEnding +
    'At the moment there were ' + inttostr(z) + ' numbers inserted.');
End;

Procedure TForm1.N2x21Click(Sender: TObject);
Begin
  Form16.showmodal;
End;

End.

