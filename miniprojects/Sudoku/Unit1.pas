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
  StdCtrls, ComCtrls, usudoku, ExtCtrls, lcltype;

Type

  TLinepencil = Array Of TPencil;

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
    fLinepencil: TLinepencil; // Die LinienPencil's -> 3x3 Only :/
    mx, my: integer; // globalen x,y Koordinaten der Maus im Feld
    bm: Tbitmap; // Quasi noch ein Double buffered

    lc: integer; // Für das Line Edit brauchen wir ne Extra Variable

    Procedure ApplyFromModifyAndRepaintField(Sender: TObject);

    Procedure RefreshField(Sender: TObject);

    Function OnLCLUpdateEvent(): Boolean;
    Function OnStepLCLUpdateEvent(): Boolean;
    Procedure getLinePencil();
    Procedure UnPencil(x, y, value: integer);
    Function GetSudokuOptions: TSolveOptions;
    Procedure Resetopt;
    Procedure InitFieldDim(NewDim: Integer);
    Procedure Readini;
    Procedure Writeini;
  public
    { Public-Deklarationen }
    Procedure Drawfield(Sender: TObject); // TODO: Muss Private werden -> und dann Raus fliegen !
  End;

Var
  Form1: TForm1;

Implementation

Uses
  math
  , Unit2 // Edit Color Dialog
  , Unit3 // Settings
  , Unit4 // Online Help
  , Unit5 // Modify dialog
  , Unit6 // Progressbar during creation
  , Unit7 // New Dialog
  , Unit9 // Print Dialog
  // Unit10 // Print Setup Dialog
  , Unit11 // 4x4 Fields
  // Unit12 // Print Detail dialog 4x4 ?
  , Unit13 // 5x5 Fields
  // Unit14 // Print Detail dialog 5x5 ?
  // Unit15 // New Dialog for 2x2, 4x4, 5x5
  , Unit16 // 2x2 Fields
  // Unit17 // Print Detail dialog 2x2 ?
  ;

{$R *.lfm}

// Ermittelt die Line Pencils

Procedure TForm1.getLinePencil;
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
  // Zuerst die Senkrechten Linien
  For x := 0 To sqr(ffield.Dimension) - 1 Do Begin
    setlength(zahlen, 0);
    For y := 0 To sqr(ffield.Dimension) - 1 Do
      If ffield.value[x, y] <> 0 Then add(ffield.value[x, y]);
    For y := 0 To high(Zahlen) Do
      fLinepencil[x][Zahlen[y] - 1] := false;
  End;
  // Dann die Waagrechten Linien
  For y := 0 To sqr(ffield.Dimension) - 1 Do Begin
    setlength(zahlen, 0);
    For x := 0 To sqr(ffield.Dimension) - 1 Do
      If ffield.value[x, y] <> 0 Then add(ffield.value[x, y]);
    For x := 0 To high(Zahlen) Do
      fLinepencil[y + sqr(ffield.Dimension)][Zahlen[x] - 1] := false;
  End;
  setlength(zahlen, 0);
End;

// Auslesen der User.ini

Procedure TForm1.Readini;
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
    DefaultDruckbreite := strtoint(s);
    readln(f, s);
    invalidnallow := odd(strtoint(s));
    closefile(f);
  End
  Else Begin
    For x := 1 To 9 Do Begin // Das Gibt es nur für 3x3 Felder, also quasi OK
      substitution[x] := inttostr(x);
    End;
    form1.bytryanderror1.checked := false;
    // Vorsicht eine Änderung hier mus auch in Form2 geändert werden !!!
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
    DefaultDruckbreite := 1;
  End;
End;

// Schreiben der User.ini

Procedure TForm1.Writeini;
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
  For x := 1 To 9 Do Begin // Das Gibt es nur für 3x3 Felder, also quasi OK
    writeln(f, substitution[x]);
  End;
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
  writeln(f, inttostr(DefaultDruckbreite));
  writeln(f, inttostr(ord(invalidnallow)));
  closefile(f);
End;

// Fügt wieder einen Penzil wert ein

Procedure TForm1.UnPencil(x, y, value: integer);
Begin
  If Not unpencilallow Then exit;
  ffield.Unpencil(x, y, value);
  // Unpencil für die Lines
  fLinepencil[x][Value - 1] := true;
  fLinepencil[sqr(ffield.Dimension) + y][Value - 1] := true;
End;

Function TForm1.GetSudokuOptions: TSolveOptions;
Begin
  // Übernehmen der Solve Methoden
  Result := [];
  If byhiddensingle1.checked Then Result := Result + [soHiddenSingle];
  If bynakedsingle1.checked Then Result := Result + [soNakedSingle];
  If byBlockandColumninteractions1.checked Then Result := Result + [soBlockAndColumnInteraction];
  If byblockandblockinteractions1.checked Then Result := Result + [soBlockAndBlockInteraction];
  If bynakedsubset1.checked Then Result := Result + [soNakedSubset];
  If byhiddensubset1.checked Then Result := Result + [soHiddenSubset];
  If byXWingSwordfish1.checked Then Result := Result + [soXWing];
  If byXYWing1.checked Then Result := Result + [soXYWing];
  If ForcingChains1.checked Then Result := Result + [soForcingChains];
  If bytryanderror1.Checked Then Result := Result + [soTryAndError];
End;

// Zeichnet das Komplette Spielfeld

Procedure TForm1.Drawfield(Sender: TObject);
Var
  Info: TRenderInfo;
  i: Integer;
Begin
  If bm = Nil Then exit;
  If checkbox1.checked And (mx > -1) Then Begin
    ffield.ResetAllMarker;
    ffield.Mark(ffield.value[mx, my]);
  End;
  // Markieren der Felder die Permanent Markiert werden müssen
  // TODO: Das hier geht nicht bei Dimension > 3 !
  For i := 1 To 9 Do Begin
    If TToolbutton(form1.findcomponent('ToolButton' + inttostr(i))).down Then Begin
      ffield.Mark(i);
    End;
  End;
  info.Cursor := point(mx, my);
  info.LinePencilIndex := lc;
  info.Rect := PaintBox1.ClientRect;
  // TODO: Das hier geht nicht bei Dimension > 3 !
  setlength(info.NumberHighLights, 9);
  For i := 0 To 8 Do Begin
    info.NumberHighLights[i] := TToolButton(form1.findcomponent('Toolbutton' + inttostr(11 + i))).Down
  End;
  info.Show_Pencils_Numbers := Checkbox4.checked;
  info.Show_Line_Pencil_numbers := Checkbox5.checked;
  info.Edit_Line_Pencil_Numbers := Checkbox6.checked;
  setlength(info.LinePencil, ffield.Dimension * ffield.Dimension * 2);
  For i := 0 To high(info.LinePencil) Do Begin
    info.LinePencil[i] := fLinepencil[i];
  End;
  // Löschen des Bildschirms
  bm.canvas.brush.style := bssolid;
  bm.canvas.brush.color := FormBackground;
  bm.canvas.rectangle(-1, -1, bm.width + 1, bm.height + 1);
  ffield.RenderTo(bm.Canvas, info);
  Form1.PaintBox1.Canvas.Draw(0, 0, bm);
End;

Procedure TForm1.Resetopt;
Var
  x: integer;
Begin
  Button2.onclick(Nil); // Clear Pencil Numbers
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

Procedure TForm1.InitFieldDim(NewDim: Integer);
Var
  i: Integer;
Begin
  If NewDim <> ffield.Dimension Then Begin
    ffield.free;
    ffield := TSudoku.Create(NewDim);
    setlength(fLinepencil, 2 * sqr(NewDim));
    For i := 0 To high(fLinepencil) Do Begin
      setlength(fLinepencil[i], sqr(NewDim));
    End;
    // TODO: Hier muss dann auch noch das Steuern der LCL-Komponenten mit rein ..
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
  Constraints.MinHeight := 480;
  Constraints.MinWidth := 640;
  Randomize;
  // RandSeed := 42; // -- Enable for testing to get everytime first the same Sudoku
  // RandSeed := 128; // -- Enable for testing to get everytime first the same Sudoku
  ffield := TSudoku.Create(2); //-- Absichtlich Falsch, damit InitField alles Sauber initialisieren kann
  InitFieldDim(3);

  //  ffield := TSudoku.Create(3);
  //  InitFieldDim(2);

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
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Var
  i: Integer;
Begin
  For i := 0 To high(fLinepencil) Do Begin
    setlength(fLinepencil[i], 0);
  End;
  setlength(fLinepencil, 0);
  ffield.Free;
  ffield := Nil;
End;

Procedure TForm1.FormResize(Sender: TObject);
Var
  x: Integer;
Begin
  For x := 1 To 6 Do Begin // Warum ist das nicht mit Anchors gemacht ?
    TCheckbox(findcomponent('Checkbox' + inttostr(x))).left := Form1.width - Scale96ToForm(195);
  End;
  button1.left := Form1.width - Scale96ToForm(160);
  button2.left := Form1.width - Scale96ToForm(160);
  If Assigned(bm) Then Begin
    bm.width := PaintBox1.Width;
    bm.height := PaintBox1.Height;
    PaintBox1.Invalidate;
  End;
End;

Procedure TForm1.FormKeyPress(Sender: TObject; Var Key: Char);
// Fügt unter Beachtung aller Bedingungen eine Zahl in das Feld ein
  Procedure AddZahl(Value, X, y: integer);
  Var
    c, d, a, b, z: integer;
    e: Boolean;
  Begin
    If value > sqr(ffield.Dimension) Then exit; // TODO: Das muss noch komplett anders gelöst werden, wenn Felder Größer 3x3 kommen ..
    // Fixed Zahlen können nicht überschrieben werden !!
    If fField.IsFixed(x, y) And Not checkbox2.checked Then Begin
      showmessage('Field is fixed, you cannot override it!');
      exit;
    End;
    // Die eingabe eines Pencil Wertes
    If checkbox3.checked Then Begin
      If Value = 0 Then exit;
      fField.TogglePencil(x, y, value); // TODO: auf Lange sicht sollte das Toggle wieder raus, es gibt Set und get !
      // überprüfen ob der Pencil überhaupt sinn macht
      If ffield.IsPencilSet(x, y, value) Then Begin
        // Prüfen ob die Zahl Waagrecht / Senkrecht rein darf
        e := true;
        For z := 0 To sqr(ffield.Dimension) - 1 Do Begin
          If (fField.value[z, y] = Value) And Not (z = x) Then e := false;
          If (fField.value[x, z] = Value) And Not (z = y) Then e := false;
        End;
        // Prüfen ob die Zahl in das entsprechende 9er Feld Darf
        a := x - (x Mod ffield.Dimension);
        b := y - (y Mod ffield.Dimension);
        For c := 0 To ffield.Dimension - 1 Do
          For d := 0 To ffield.Dimension - 1 Do
            // Prüfen der Zahl im 9er Feld auser dem gewählten Feld
            If ((a + c) <> x) Or ((b + d) <> y) Then Begin
              If fField.value[a + c, b + d] = Value Then e := false;
            End;
        If invalidnallow Then e := true; // Wenn auch ungültige Zahlen eingegeben werden können.
        If Not E Then Begin
          fField.ClearPencil(x, y, value);
          showmessage('Character for this field impossible.');
        End;
      End;
    End
      // Die eingabe eines normalen wertes
    Else Begin
      // Löschen eines Wertes
      If Value = 0 Then Begin
        ffield.ResetAllMarker;
        // nur in speziell des Falles das eine Zahl gelöscht wird darf sie bei den Pencils hinzugefügt werden.
        UnPencil(x, y, fField.value[x, Y]);
      End;
      // Prüfen ob die Zahl Waagrecht / Senkrecht rein darf
      e := true;
      For z := 0 To sqr(ffield.Dimension) - 1 Do Begin
        If (fField.value[z, y] = Value) And Not (z = x) Then e := false;
        If (fField.value[x, z] = Value) And Not (z = y) Then e := false;
      End;
      // Prüfen ob die Zahl in das entsprechende 9er Feld Darf
      a := x - (x Mod ffield.Dimension);
      b := y - (y Mod ffield.Dimension);
      For c := 0 To ffield.Dimension - 1 Do
        For d := 0 To ffield.Dimension - 1 Do
          // Prüfen der Zahl im 9er Feld auser dem gewählten Feld
          If ((a + c) <> x) Or ((b + d) <> y) Then Begin
            If fField.value[a + c, b + d] = Value Then
              e := false;
          End;
      // Wenn die Zahl gelöscht wird
      If Value = 0 Then e := true;
      If invalidnallow Then e := true; // Wenn auch ungültige Zahlen eingegeben werden können.
      // das Feld Aktualisieren
      If e Then Begin
        // Zuweisen des neuen Feldwertes
        If fField.value[x, y] = value Then Begin
          If Not (Not fField.IsFixed(x, y) And checkbox2.checked) Then Begin
            // nur in speziell des Falles das eine Zahl gelöscht wird darf sie bei den Pencils hinzugefügt werden.
            ffield.ResetAllMarker;
            UnPencil(x, y, fField.value[x, Y]);
            ffield.SetValue(x, y, 0, false); // Rücksetzen des Feldwertes
          End;
        End
        Else Begin
          If fField.value[x, y] <> 0 Then Begin
            // nur in speziell des Falles das eine Zahl gelöscht wird darf sie bei den Pencils hinzugefügt werden.
            ffield.ResetAllMarker;
            UnPencil(x, y, fField.value[x, Y]);
          End;
          ffield.SetValue(x, y, Value, false); // Setzen des Feldes mit dem Wert
        End;
        // zuweisen ob Fixed wert, oder nur normale Zahl
        If fField.value[x, y] <> 0 Then Begin
          ffield.SetValue(x, y, fField.value[x, Y], form1.checkbox2.checked);
        End;
        // Ermitteln der Pencil Werte
        If checkbox4.checked Then Begin
          ffield.ClearAllNumberPencils;
        End;
      End
      Else
        showmessage('Your Number is not allowed in this position');
    End;
  End;
Var
  x1, x2, y1: integer;
  zah: Array Of integer;
  a: Boolean;
Begin
  zah := Nil;
  setlength(zah, sqr(ffield.Dimension) + 1);
  If ffield.IsSolved() And Not (key In ['a', 'A', '0', 's', 'S', 'd', 'D', 'w', 'W']) Then Begin
    exit;
  End;
  // Wenn wir uns im edit Line Pencil Modus befinden
  If Checkbox6.checked Then Begin
    // Bewegen des Cursors
    If (key In ['a', 'A', 's', 'S', 'd', 'D', 'w', 'W']) Then Begin
      If ((Key = 'a') Or (Key = 'A') Or (Key = 'w') Or (Key = 'W')) And (lc > 0) Then dec(lc);
      If ((Key = 's') Or (Key = 'S') Or (Key = 'd') Or (Key = 'D')) And (lc < sqr(ffield.Dimension) * 2 - 1) Then inc(lc);
      PaintBox1.Invalidate;
      exit;
    End
    Else Begin
      // Eingabe der Pencil werte
      If Key In ['1'..'9'] Then Begin // TODO: Das wird bei > 3x3 problematisch
        a := true;
        x2 := strtoint(key);
        If lc < sqr(ffield.Dimension) Then Begin
          For x1 := 0 To sqr(ffield.Dimension) - 1 Do
            If x2 = fField.value[lc, x1] Then a := false;
        End
        Else Begin
          For x1 := 0 To sqr(ffield.Dimension) - 1 Do
            If x2 = fField.value[x1, lc - sqr(ffield.Dimension)] Then a := false;
        End;
        If invalidnallow Then a := true; // Wenn auch ungültige Zahlen eingegeben werden können.
        If A Then
          fLinepencil[lc][strtoint(key) - 1] := Not fLinepencil[lc][strtoint(key) - 1]
        Else
          showmessage('Character for this field impossible.');
      End;
      PaintBox1.Invalidate;
    End;
    exit;
  End;
  // Eingaben im Feld
  // Steuerung des Cursors
  If (Key = 's') Or (Key = 'S') Then
    If my < sqr(ffield.Dimension) - 1 Then inc(my);
  If (Key = 'w') Or (Key = 'W') Then
    If my > 0 Then dec(my);
  If (Key = 'a') Or (Key = 'A') {Or (key = #37)} Then
    If mx > 0 Then dec(mx);
  If (Key = 'd') Or (Key = 'D') Then
    If mx < sqr(ffield.Dimension) - 1 Then inc(mx);
  // Einfügen und Löschen von Zahlen
  If (Key In ['0'..'9']) And (mx <> -1) Then Begin
    AddZahl(StrToInt(key), mx, my);
    fField.SetMayBeed(mx, my, false);
  End;
  // Einfügen der Geschätzten Zahlen
  If mx <> -1 Then Begin
    If (Key In ['!', '"', '?' {='§'}, '$', '%', '&', '/', '(', ')']) Then Begin
      fField.SetMaybeed(mx, my, True);
      If Key = '!' Then
        AddZahl(1, mx, my);
      If Key = '"' Then
        AddZahl(2, mx, my);
      If Key = '?' {='§'} Then
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
        getLinePencil();
    End;
  End;
  // Hohlen der ganzen Linepencil sachen
  If Checkbox5.checked Then
    getLinePencil();
  // überprüfen ob vielleicht schon von einer Zahl alle gefunden wurden
  For x1 := 1 To sqr(ffield.Dimension) Do
    zah[x1] := 0;
  For x1 := 0 To sqr(ffield.Dimension) - 1 Do
    For y1 := 0 To sqr(ffield.Dimension) - 1 Do
      If fField.value[x1, y1] <> 0 Then
        inc(zah[fField.value[x1, y1]]);
  {  If Key = '0' Then Begin

    End;}
  For x1 := 1 To 9 Do
    If (x1 > high(zah)) Or (zah[x1] = sqr(ffield.Dimension)) Then Begin
      TToolbutton(Findcomponent('ToolButton' + inttostr(x1))).enabled := false;
      TToolbutton(form1.Findcomponent('ToolButton' + inttostr(x1 + 10))).enabled := false;
      TToolbutton(form1.Findcomponent('ToolButton' + inttostr(x1 + 10))).Down := false;
      TToolbutton(Findcomponent('ToolButton' + inttostr(x1))).Down := False;
      ffield.ResetAllMarker;
    End
    Else Begin
      TToolbutton(Findcomponent('ToolButton' + inttostr(x1))).enabled := true;
      TToolbutton(form1.Findcomponent('ToolButton' + inttostr(x1 + 10))).enabled := true;
    End;
  // Schauen ob Fertig.
  If ffield.IsFullyFilled() And (Not (key In ['a', 'A', 's', 'S', 'd', 'D', 'w', 'W', '0'])) Then Begin
    If ffield.IsSolved() Then Begin
      showmessage('You solved the Sudoku.');
    End
    Else Begin
      showmessage('You filled out the Sudoku, but not correct.');
    End;
  End;
  PaintBox1.Invalidate;
End;

Procedure TForm1.CheckBox1Click(Sender: TObject);
Begin
  If Not Checkbox1.checked Then Begin
    ffield.ResetAllMarker;
    If (mx In [0..sqr(ffield.Dimension) - 1]) And (my In [0..sqr(ffield.Dimension) - 1]) Then
      ffield.SetMarked(mx, my, true);
  End;
  PaintBox1.Invalidate;
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
  If Checkbox4.checked Then Begin
    ffield.ClearAllNumberPencils;
  End;
  If checkbox3.checked And Not Checkbox4.checked Then checkbox3.checked := false;
  PaintBox1.Invalidate;
End;

Procedure TForm1.Clearfield1Click(Sender: TObject);
Begin
  Resetopt;
  mx := 0;
  my := 0;
  ffield.ClearField;
  PaintBox1.Invalidate;
End;

Procedure TForm1.FormShow(Sender: TObject);
Begin
  PaintBox1.Invalidate;
End;

Procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  x1, y1: integer;
  Breite: integer;
Begin
  Breite := min(PaintBox1.Height, PaintBox1.Width) Div (sqr(ffield.Dimension) + 2); // Eigentlich müsste da doch auch ein 10 Reichen und dann wäre es sqr(dim)+1 ?
  // die Auswahl für die Linepencil's
  If Checkbox6.checked Then Begin
    If (X >= Breite) And (x <= Breite * (sqr(ffield.Dimension) + 1)) And (y <= breite) Then Begin
      lc := x Div Breite - 1;
    End;
    If (X > Breite * (sqr(ffield.Dimension) + 1)) And (x <= Breite * (sqr(ffield.Dimension) + 2)) And (y >= Breite) And (y <= Breite * (sqr(ffield.Dimension) + 1)) Then Begin
      lc := y Div Breite + (sqr(ffield.Dimension) - 1);
    End;
  End
  Else Begin
    // Löschen aller Markierungen
    mx := -1;
    my := -1;
    ffield.ResetAllMarker;
    If (X >= Breite) And (x <= Breite * (sqr(ffield.Dimension) + 1)) And
      (y >= Breite) And (y <= Breite * (sqr(ffield.Dimension) + 1)) Then Begin
      // Ausrechnen der Koordinaten des neu Markierten Feldes
      x1 := x Div Breite - 1;
      y1 := y Div Breite - 1;
      mx := x1;
      my := y1;
    End;
  End;
  PaintBox1.Invalidate;
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
  Showmessage('See: https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md for detailed informations.');
End;

Procedure TForm1.Save1Click(Sender: TObject);
Var
  F: TFilestream;
Begin
  If Savedialog1.execute Then Begin
    SaveDialog1.initialdir := ExtractFilePath(Savedialog1.Filename);
    openDialog1.initialdir := ExtractFilePath(Savedialog1.Filename);
    f := Tfilestream.create(Savedialog1.Filename, fmCreate Or fmOpenWrite);
    ffield.SaveToStream(f);
    f.write(flinepencil, sizeof(flinepencil));
    f.Free;
  End;
End;

Procedure TForm1.Load1Click(Sender: TObject);
Var
  F: TFilestream;
Begin
  If opendialog1.execute Then Begin
    SaveDialog1.initialdir := ExtractFilePath(opendialog1.Filename);
    openDialog1.initialdir := ExtractFilePath(opendialog1.Filename);
    // Zurücksetzen der Graphischen Hilfsmittel
    ResetOpt;
    f := Tfilestream.create(opendialog1.Filename, fmOpenRead);
    ffield.LoadFromStream(f);
    f.Read(flinepencil, sizeof(flinepencil));
    f.Free;
    ffield.ResetAllMarker;
    mx := 0;
    my := 0;
    PaintBox1.Invalidate;
  End;
End;

Procedure TForm1.Resetfield1Click(Sender: TObject);
Var
  x, y: integer;
Begin
  Resetopt;
  // Löschen aller Einträge des Users
  For x := 0 To sqr(ffield.Dimension) - 1 Do
    For y := 0 To sqr(ffield.Dimension) - 1 Do Begin
      If Not (fField.IsFixed(x, y)) Then ffield.SetValue(x, y, 0, false);
      fField.SetMarked(x, y, false);
    End;
  PaintBox1.Invalidate;
End;

Procedure TForm1.ToolButton1Click(Sender: TObject);
Begin
  If TToolbutton(Sender).Down = false Then Begin
    ffield.ResetAllMarker;
  End;
  PaintBox1.Invalidate;
End;

Procedure TForm1.ToolButton11Click(Sender: TObject);
Begin
  PaintBox1.Invalidate;
End;

Procedure TForm1.Colors1Click(Sender: TObject);
Var
  x: integer;
Begin
  Form2.Shape2.brush.color := Bretthintergrundfarbe1;
  Form2.Shape3.brush.color := Bretthintergrundfarbe2;
  Form2.Shape4.brush.color := Maybeedcolor;
  Form2.Shape5.brush.color := MarkedColor1;
  Form2.Shape6.brush.color := MarkedColor2;
  Form2.Shape7.brush.color := CursorMarker;
  Form2.Shape8.brush.color := Fixedcolor;
  Form2.Shape9.brush.color := Gitterfarbe;
  Form2.Shape10.brush.color := FontColor;
  Form2.Shape11.brush.color := Pencilcolor;
  Form2.Shape12.brush.color := PencilcolorMarked;
  Form2.Shape13.brush.color := LightenColor;
  Form2.Shape14.brush.color := FormBackground;

  If Form2.showmodal = mrOK Then Begin
    // Übernehmen der Farben in die Variablen der Form1
    Bretthintergrundfarbe1 := Form2.Shape2.brush.color;
    Bretthintergrundfarbe2 := Form2.Shape3.brush.color;
    Maybeedcolor := Form2.Shape4.brush.color;
    MarkedColor1 := Form2.Shape5.brush.color;
    MarkedColor2 := Form2.Shape6.brush.color;
    CursorMarker := Form2.Shape7.brush.color;
    Fixedcolor := Form2.Shape8.brush.color;
    Gitterfarbe := Form2.Shape9.brush.color;
    FontColor := Form2.Shape10.brush.color;
    Pencilcolor := Form2.Shape11.brush.color;
    PencilcolorMarked := Form2.Shape12.brush.color;
    LightenColor := Form2.Shape13.brush.color;
    FormBackground := Form2.Shape14.brush.color;
    // Sonderfall Hintergrund = Schwarz
    For x := 1 To 6 Do Begin
      TCheckbox(findcomponent('Checkbox' + inttostr(x))).color := FormBackground;
      If FormBackground = clblack Then
        TCheckbox(findcomponent('Checkbox' + inttostr(x))).font.color := clwhite
      Else
        TCheckbox(findcomponent('Checkbox' + inttostr(x))).font.color := clblack;
    End;
    // Neuzeichnen
    PaintBox1.Invalidate;
  End;
End;

Procedure TForm1.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Begin
  Writeini;
  bm.free;
  bm := Nil;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  x, y: integer;
Begin
  // Auto Pencil Numbers
  For x := 0 To sqr(ffield.Dimension) * 2 - 1 Do
    For y := 0 To sqr(ffield.Dimension) - 1 Do
      fLinepencil[x][y] := true;
  getlinepencil();
  ffield.ResetAllNumberPencils;
  ffield.ClearAllNumberPencils;
  PaintBox1.Invalidate;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Var
  x, y: integer;
Begin
  // Löschen der Linepencil's
  For x := 0 To 2 * ffield.Dimension * ffield.Dimension - 1 Do
    For y := 0 To ffield.Dimension * ffield.Dimension - 1 Do
      fLinepencil[x][y] := false;
  // Löschen der Field pencil's
  ffield.EraseAllNumberPencils;
  PaintBox1.Invalidate;
End;

Procedure TForm1.CheckBox5Click(Sender: TObject);
Begin
  If Checkbox5.checked Then getLinePencil();
  If Not Checkbox5.checked Then Begin
    checkbox6.checked := false;
  End;
  PaintBox1.Invalidate;
End;

Procedure TForm1.CheckBox6Click(Sender: TObject);
Begin
  Checkbox2.checked := false;
  If Checkbox6.checked Then Begin
    checkbox5.checked := True;
    Checkbox3.checked := false;
  End;
  PaintBox1.Invalidate;
End;

Procedure TForm1.ToolButton1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  x1: integer;
Begin
  If SSright In shift Then Begin
    ffield.ResetAllMarker;
    For x1 := 1 To 9 Do
      TTOolbutton(form1.findcomponent('Toolbutton' + inttostr(x1))).down := false;
    TTOolbutton(sender).Down := true;
    PaintBox1.Invalidate;
  End;
End;

Procedure TForm1.General2Click(Sender: TObject);
Begin
  form3.checkbox2.checked := invalidnallow;
  Form3.checkbox1.checked := unpencilallow;
  form3.showmodal;
End;

Procedure TForm1.ToolButton11MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  x1: integer;
Begin
  If SSright In shift Then Begin
    For x1 := 11 To 19 Do
      TTOolbutton(form1.findcomponent('Toolbutton' + inttostr(x1))).down := false;
    TTOolbutton(sender).Down := true;
    PaintBox1.Invalidate;
  End;
End;

Procedure TForm1.General1Click(Sender: TObject);
Begin
  form4.memo1.text := rules;
  form4.showmodal;
End;

Procedure TForm1.Solveit1Click(Sender: TObject);
Var
  aFormclose: Boolean;
  zah: Array Of integer;
  x1, y1: Integer;
Begin
  If Not (ffield.IsSolveable) Then Begin
    showmessage('Impossible to solve Sudoku');
    PaintBox1.Invalidate;
  End
  Else Begin
    aFormclose := false;
    zwangsabbruch := false;
    If Not form6.visible Then Begin
      aFormclose := true;
    End;
    ffield.Solve(false, GetSudokuOptions(), @OnStepLCLUpdateEvent);
    ffield.ResetAllMarker;
    getlinepencil(); // Ermitteln der Korreckten Line pencil's
    // Schauen ob irgendwelche Zahlen schon komplett sind und entsprechend setzen der Toolbuttons
    setlength(zah, sqr(ffield.Dimension) + 1);
    For x1 := 1 To sqr(ffield.Dimension) Do
      zah[x1] := 0;
    For x1 := 0 To sqr(ffield.Dimension) - 1 Do
      For y1 := 0 To sqr(ffield.Dimension) - 1 Do
        If ffield.value[x1, y1] <> 0 Then
          inc(zah[ffield.value[x1, y1]]);
    For x1 := 1 To sqr(ffield.Dimension) Do
      If zah[x1] = sqr(ffield.Dimension) Then Begin
        TToolbutton(form1.Findcomponent('ToolButton' + inttostr(x1))).enabled := false;
        TToolbutton(form1.Findcomponent('ToolButton' + inttostr(x1 + 10))).enabled := false;
        TToolbutton(form1.Findcomponent('ToolButton' + inttostr(x1 + 10))).Down := false;
        TToolbutton(form1.Findcomponent('ToolButton' + inttostr(x1))).Down := False;
        ffield.ResetAllMarker;
      End
      Else Begin
        TToolbutton(form1.Findcomponent('ToolButton' + inttostr(x1))).enabled := true;
        TToolbutton(form1.Findcomponent('ToolButton' + inttostr(x1 + 10))).enabled := true;
      End;

    If aFormclose And form6.visible Then Form6.close;

    PaintBox1.Invalidate;
    If ffield.isSolved() Then Begin
      Showmessage('Ready');
    End
    Else Begin
      showmessage('Unable to solve');
    End;
  End;
End;

Procedure TForm1.Solvestep1Click(Sender: TObject);
Var
  UsedTryError, a, aFormclose: Boolean;
  x1, y1: integer;
  p: TPoint;
  zah: Array Of integer;
  options: TSolveOptions;
Begin
  If Not (ffield.IsSolveable) Then Begin
    showmessage('Impossible to solve Sudoku');
  End
  Else Begin
    Options := GetSudokuOptions();
    If (soTryAndError In options) Then Begin
      If ID_NO = application.messagebox(pchar('You slected the solving method by try and error.' + LineEnding +
        'If this step is necessary your Sudoku will be completed at all.' + LineEnding + LineEnding +
        'do you want this ?'), 'Question', MB_YESNO + MB_ICONQUESTION) Then Begin
        exit;
      End;
      aFormclose := false;
      zwangsabbruch := false;
      If Not form6.visible Then Begin
        aFormclose := true;
      End;
    End;
    UsedTryError := ffield.Solve(true, Options, @OnStepLCLUpdateEvent);
    ffield.ResetAllMarker;
    If (soTryAndError In options) And UsedTryError Then Begin
      If aFormclose And form6.visible Then Form6.close;
      If Not ffield.IsSolved() Then Begin
        showmessage('This Sudoku is impossible to solve');
      End;
    End;
    getlinepencil(); // Ermitteln der Korreckten Line pencil's
    p := ffield.StepPos;
    mx := p.x;
    my := p.y;
    // TODO: Das hier ist doppelt mit SolveIt -> raus ziehen
    // Schauen ob irgendwelche Zahlen schon komplett sind und entsprechend setzen der Toolbuttons
    setlength(zah, Sqr(ffield.Dimension) + 1);
    For x1 := 1 To Sqr(ffield.Dimension) Do
      zah[x1] := 0;
    For x1 := 0 To Sqr(ffield.Dimension) - 1 Do
      For y1 := 0 To Sqr(ffield.Dimension) - 1 Do
        If ffield.value[x1, y1] <> 0 Then
          inc(zah[ffield.value[x1, y1]]);
    For x1 := 1 To Sqr(ffield.Dimension) Do
      If zah[x1] = Sqr(ffield.Dimension) Then Begin
        TToolbutton(form1.Findcomponent('ToolButton' + inttostr(x1))).enabled := false;
        TToolbutton(form1.Findcomponent('ToolButton' + inttostr(x1 + 10))).enabled := false;
        TToolbutton(form1.Findcomponent('ToolButton' + inttostr(x1 + 10))).Down := false;
        TToolbutton(form1.Findcomponent('ToolButton' + inttostr(x1))).Down := False;
        ffield.ResetAllMarker;
      End
      Else Begin
        TToolbutton(form1.Findcomponent('ToolButton' + inttostr(x1))).enabled := true;
        TToolbutton(form1.Findcomponent('ToolButton' + inttostr(x1 + 10))).enabled := true;
      End;
    // Anzeigen das Fertig
    a := true;
    For x1 := 1 To 9 Do
      If TToolbutton(Findcomponent('ToolButton' + inttostr(x1))).enabled Then a := false;
    If A Then Begin
      mx := -1;
      my := -1;
      showmessage('You solved the Sudoku.');
    End;
    PaintBox1.Invalidate;
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
  If ffield.Dimension <> 3 Then Begin
    Showmessage('This feature is only available for 3x3 Fields');
    exit;
  End;
  For x := 1 To 9 Do Begin
    TCombobox(Form5.findcomponent('Combobox' + inttostr(x))).text := substitution[x];
  End;
  form5.Init(ffield, @ApplyFromModifyAndRepaintField);
  Form5.showmodal;
End;

Procedure TForm1.Puzzle1Click(Sender: TObject);
Begin
  // New Puzzle 3x3
  InitFieldDim(3);
  Form7.init(ffield, GetSudokuOptions());
  Form7.showmodal;
  ffield.CloneFieldFrom(form7.Sudoku);
  Button2Click(Nil); // Erst mal alle Pencils löschen
  PaintBox1.Invalidate;
End;

Procedure TForm1.N2x21Click(Sender: TObject);
Begin
  // New Puzzle 2x2
  form16.ShowModal; // TODO: Alt -> Raus
  exit; // TODO: Alt -> Raus
  // Ab hier das "Neue", wenn es denn mal tut
  InitFieldDim(2);
  Form7.init(ffield, GetSudokuOptions());
  Form7.showmodal;
  ffield.CloneFieldFrom(form7.Sudoku);
  Button2Click(Nil); // Erst mal alle Pencils löschen
  PaintBox1.Invalidate;
End;

Procedure TForm1.N4x41Click(Sender: TObject);
Begin
  // New Puzzle 4x4
  showmessage('These Sudoku''s were something special, partly there debuggininfo''s aviable.' + LineEnding + LineEnding +
    'If you want to so the progress of the creater then' + LineEnding +
    'click on the field while the creating message is shown.' + LineEnding + LineEnding +
    'Normal time for creating a Sudoko with this size 10 - 20 sek.');
  Form11.showmodal; // TODO: Alt -> Raus
  exit; // TODO: Alt -> Raus
  // Ab hier das "Neue", wenn es denn mal tut
  InitFieldDim(4);
  Form7.init(ffield, GetSudokuOptions());
  Form7.showmodal;
  ffield.CloneFieldFrom(form7.Sudoku);
  Button2Click(Nil); // Erst mal alle Pencils löschen
  PaintBox1.Invalidate;
End;

Procedure TForm1.N5x51Click(Sender: TObject);
Begin
  // New Puzzle 5x5
  showmessage('These Sudoku''s were something special, partly there debuggininfo''s aviable.' + LineEnding + LineEnding +
    'If you want to so the progress of the creater then' + LineEnding +
    'click on the field while the creating message is shown.' + LineEnding + LineEnding +
    'Normal time for creating a Sudoko with this size 30 - 90 sek.');
  Form13.showmodal; // TODO: Alt -> Raus
  exit; // TODO: Alt -> Raus
  // Ab hier das "Neue", wenn es denn mal tut
  InitFieldDim(5);
  Form7.init(ffield, GetSudokuOptions());
  Form7.showmodal;
  ffield.CloneFieldFrom(form7.Sudoku);
  Button2Click(Nil); // Erst mal alle Pencils löschen
  PaintBox1.Invalidate;
End;

Procedure TForm1.Maybenumbersgoodnumbers1Click(Sender: TObject);
Var
  x, y: Integer;
Begin
  For x := 0 To ffield.Dimension - 1 Do
    For y := 0 To ffield.Dimension - 1 Do
      ffield.SetMaybeed(x, y, false);
  PaintBox1.Invalidate;
End;

Procedure TForm1.MaybanumberclearField1Click(Sender: TObject);
Var
  x, y: Integer;
Begin
  For x := 0 To ffield.Dimension - 1 Do
    For y := 0 To ffield.Dimension - 1 Do
      If fField.GetMaybeed(x, y) Then Begin
        ffield.SetMaybeed(x, y, false);
        UnPencil(x, y, fField.value[x, y]);
        ffield.SetValue(x, y, 0, false);
      End;
  PaintBox1.Invalidate;
End;

Procedure TForm1.SpecialPuzzle1Click(Sender: TObject);
Begin
  showmessage('Not complete Implemented');
  // Form8.showmodal;
End;

Procedure TForm1.Print1Click(Sender: TObject);
Begin
  form9.init(ffield, GetSudokuOptions());
  Form9.showmodal;
End;

Procedure TForm1.Info1Click(Sender: TObject);
Var
  x, y, z: integer;
Begin
  z := 0;
  For x := 0 To ffield.Dimension - 1 Do
    For y := 0 To ffield.Dimension - 1 Do
      If fField.value[x, y] <> 0 Then inc(z);
  Showmessage('This Sudoku needs ' + inttostr(ffield.Dimension * ffield.Dimension) + ' numbers to be complete.' + LineEnding + LineEnding +
    'At the moment there were ' + inttostr(z) + ' numbers inserted.');
End;

Procedure TForm1.ApplyFromModifyAndRepaintField(Sender: TObject);
Begin
  ffield.CloneFieldFrom(form5.Sudoku);
  PaintBox1.Invalidate;
End;

Procedure TForm1.RefreshField(Sender: TObject);
Begin
  PaintBox1.Invalidate;
End;

Function TForm1.OnLCLUpdateEvent: Boolean;
Begin
  Application.ProcessMessages;
  result := zwangsabbruch;
End;

Function TForm1.OnStepLCLUpdateEvent: Boolean;
Begin
  If Not Form6.Visible Then Begin
    form6.Show;
  End;
  Application.ProcessMessages;
  result := zwangsabbruch;
End;

End.

