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
Unit Unit16;

{$MODE ObjFPC}{$H+}

Interface

Uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Sudoku2x2,
  Menus, LResources;

Type
  TForm16 = Class(TForm)
    MainMenu1: TMainMenu;
    Datei1: TMenuItem;
    New1: TMenuItem;
    Print1: TMenuItem;
    Load1: TMenuItem;
    Save1: TMenuItem;
    Close1: TMenuItem;
    Cheat1: TMenuItem;
    editfixednumbers1: TMenuItem;
    Showpossiblepencilincaption1: TMenuItem;
    Action1: TMenuItem;
    Info1: TMenuItem;
    Reset1: TMenuItem;
    Step1: TMenuItem;
    Solveit1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Procedure Close1Click(Sender: TObject);
    Procedure editfixednumbers1Click(Sender: TObject);
    Procedure Showpossiblepencilincaption1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormPaint(Sender: TObject);
    Procedure FormResize(Sender: TObject);
    Procedure New1Click(Sender: TObject);
    Procedure Load1Click(Sender: TObject);
    Procedure Save1Click(Sender: TObject);
    Procedure Info1Click(Sender: TObject);
    Procedure Reset1Click(Sender: TObject);
    Procedure Step1Click(Sender: TObject);
    Procedure Solveit1Click(Sender: TObject);
    Procedure FormKeyPress(Sender: TObject; Var Key: Char);
    Procedure Print1Click(Sender: TObject);
    Procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

Var
  Form16: TForm16;
  twoField: T2field;
  bm2: Tbitmap;
  m2x, m2y, Breite2: integer;

Implementation

Uses
  math
  , usudoku
  , Unit6
  , Unit15
  , Unit17
  ;

{$R *.lfm}

Procedure Draw2Field;
Var
  yo, xo, x, y: integer;
Begin
  With bm2.canvas Do Begin
    xo := ((Form16.width) Div 2) - 3 * Breite2;
    yo := -Breite2 Div 2;
    // Löschen des Bildschirms
    brush.style := bssolid;
    brush.color := FormBackground;
    rectangle(-1, -1, form16.width + 1, form16.height + 1);
    // Malen des Gitters
    For y := 0 To 3 Do
      For x := 0 To 3 Do Begin
        Case x Of
          0, 1: Begin
              If y In [0, 1] Then Begin
                brush.color := Bretthintergrundfarbe1;
                If twoField[x, y].marked Then brush.color := markedColor1;

              End
              Else Begin
                brush.color := Bretthintergrundfarbe2;
                If twoField[x, y].marked Then brush.color := markedColor2;
              End;
            End;
          2, 3: Begin
              If y In [2, 3] Then Begin
                brush.color := Bretthintergrundfarbe1;
                If twoField[x, y].marked Then brush.color := markedColor1;
              End
              Else Begin
                brush.color := Bretthintergrundfarbe2;
                If twoField[x, y].marked Then brush.color := markedColor2;
              End;
            End;
        End;
        // Farbe zum Markieren des Aktuellen Feldes
        If (x = m2x) And (y = m2y) Then brush.color := CursorMarker;
        // Malen des Feldes
        pen.color := Gitterfarbe;
        rectangle(xo + Breite2 * (x + 1), yo + Breite2 * (y + 1), xo + Breite2 * (x + 2), yo + Breite2 * (y + 2));
      End;
    Brush.Style := bsclear;
    // Textgröße
    Font.size := Breite2 Div 2;
    For y := 0 To 3 Do
      For x := 0 To 3 Do Begin
        // Textfarbe
        If twoField[x, y].Fixed Then
          font.color := fixedcolor
        Else
          font.color := FontColor;
        If twofield[x, y].value <> 0 Then
          Textout(xo + (Breite2 - Textwidth(inttostr(twofield[x, y].value))) Div 2 + Breite2 * (x + 1), yo + (Breite2 - Textheight(inttostr(twofield[x, y].value))) Div 2 + Breite2 * (y + 1), inttostr(twofield[x, y].value))
      End;
  End;
  form16.canvas.draw(0, 0, bm2);
End;

Procedure TForm16.Close1Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm16.editfixednumbers1Click(Sender: TObject);
Begin
  editfixednumbers1.checked := Not editfixednumbers1.checked;
End;

Procedure TForm16.Showpossiblepencilincaption1Click(Sender: TObject);
Begin
  ShowpossiblePencilinCaption1.checked := Not ShowpossiblePencilinCaption1.checked;
End;

Procedure TForm16.FormCreate(Sender: TObject);
Begin
  ClearField(twofield);
  SaveDialog1.initialdir := ExtractFilePath(application.exename);
  openDialog1.initialdir := ExtractFilePath(application.exename);
  Caption := 'Sudoku ver. : ' + ver + ' by Corpsman | www.Corpsman.de.vu |';
  bm2 := Tbitmap.create;
  bm2.PixelFormat := pf24bit;
  m2x := 0;
  m2y := 0;
End;

Procedure TForm16.FormDestroy(Sender: TObject);
Begin
  bm2.free;
  bm2 := Nil;
End;

Procedure TForm16.FormPaint(Sender: TObject);
Begin
  If assigned(bm2) And Not form6.visible Then
    Draw2Field;
End;

Procedure TForm16.FormResize(Sender: TObject);
Begin
  If Form16.height < 480 Then Form16.height := 480;
  If Form16.Width < Form16.height + 160 Then Form16.Width := Form16.height + 160;
  Breite2 := min(Form16.height - 32, Form16.width) Div 5;
  If assigned(bm2) And Not form6.visible Then Begin
    bm2.width := form16.width;
    bm2.height := form16.height;
    Draw2field;
  End;
End;

Procedure TForm16.New1Click(Sender: TObject);
Begin
  form15.showmodal;
  If CreaterOptions > 0 Then Begin
    New2Field(twoField, CreaterOptions);
  End;
  Draw2Field;
End;

Procedure TForm16.Load1Click(Sender: TObject);
Var
  x, y: integer;
  F: TFilestream;
Begin
  If opendialog1.execute Then Begin
    SaveDialog1.initialdir := ExtractFilePath(opendialog1.Filename);
    openDialog1.initialdir := ExtractFilePath(opendialog1.Filename);
    f := Tfilestream.create(opendialog1.Filename, fmOpenRead);
    f.Read(twofield, sizeof(twofield));
    f.Free;
    For x := 0 To 3 Do
      For y := 0 To 3 Do
        twofield[x, y].marked := false;
    draw2field;
  End;
End;

Procedure TForm16.Save1Click(Sender: TObject);
Var
  F: TFilestream;
Begin
  If Savedialog1.execute Then Begin
    SaveDialog1.initialdir := ExtractFilePath(Savedialog1.Filename);
    openDialog1.initialdir := ExtractFilePath(Savedialog1.Filename);
    f := Tfilestream.create(Savedialog1.Filename, fmCreate Or fmOpenWrite);
    f.write(twofield, sizeof(twofield));
    f.Free;
  End;
End;

Procedure TForm16.Info1Click(Sender: TObject);
Var
  x, y, z: integer;
Begin
  z := 0;
  For x := 0 To 3 Do
    For y := 0 To 3 Do
      If twoField[x, y].value <> 0 Then inc(z);
  Showmessage('This Sudoku needs ' + inttostr(4 * 4) + ' numbers to be complete.' + LineEnding + LineEnding +
    'At the moment there were ' + inttostr(z) + ' numbers inserted.');
End;

Procedure TForm16.Reset1Click(Sender: TObject);
Var
  x, y: integer;
Begin
  For x := 0 To 3 Do
    For y := 0 To 3 Do
      If Not twofield[x, y].fixed Then
        twofield[x, y].value := 0;
  Draw2field;
End;

Procedure TForm16.Step1Click(Sender: TObject);
Begin
  If Not (Sudoku2Solvable(twofield)) Then Begin
    showmessage('Impossible to solve Sudoku');
  End
  Else Begin
    Solve2(twofield, true, 511);
    Draw2field;
    If isready2(twofield) Then
      showmessage('You solved the Sudoku.');
  End;
End;

Procedure TForm16.Solveit1Click(Sender: TObject);
Begin
  If Not (Sudoku2Solvable(twofield)) Then Begin
    showmessage('Impossible to solve Sudoku');
  End
  Else Begin
    Solve2(twofield, false, 511);
    Draw2field;
    If isready2(twofield) Then
      showmessage('You solved the Sudoku.');
  End;
End;

Procedure TForm16.FormKeyPress(Sender: TObject; Var Key: Char);
Var
  xo, z: Integer;
  s: String;
Begin
  If m2x = -1 Then exit;
  If Key In ['a', 'A', 's', 'S', 'd', 'D', 'w', 'W'] Then Begin
    s := key;
    s := lowercase(s);
    If (s = 'a') And (m2x > 0) Then dec(m2x);
    If (s = 's') And (m2y < 3) Then inc(m2y);
    If (s = 'd') And (m2x < 3) Then inc(m2x);
    If (s = 'w') And (m2y > 0) Then dec(m2y);
    If ShowpossiblePencilinCaption1.checked And (twofield[m2x, m2y].value = 0) Then Begin
      GetPencil(twofield);
      s := '';
      For xo := 0 To 3 Do
        If twofield[m2x, m2y].pencil[xo] Then
          s := s + ' ' + inttostr(xo + 1);
      Form16.caption := s;
    End
    Else
      Caption := 'Sudoku ver. : ' + ver + ' by Corpsman | Targetsoft |';
  End;
  If Key In ['0'..'4'] Then Begin
    If twofield[m2x, m2y].Fixed And Not (editfixednumbers1.checked) Then exit;
    z := strtoint(Key);
    mark(twofield, z);
    If Z <> 0 Then Begin
      If Not twofield[m2x, m2y].marked Then Begin
        mark(twofield, 0);
        twofield[m2x, m2y].Value := z;
        twofield[m2x, m2y].Fixed := editfixednumbers1.checked;
      End
      Else Begin
        mark(twofield, 0);
        If invalidnallow Then Begin
          twofield[m2x, m2y].Value := z;
          twofield[m2x, m2y].Fixed := editfixednumbers1.checked;
        End
        Else
          Showmessage('You cannot place ' + inttostr(z) + ' in this Field');
      End;
    End
    Else Begin
      // Löschen der Zahl
      twofield[m2x, m2y].Value := z;
      twofield[m2x, m2y].Fixed := false;
    End;
  End;
  Draw2field;
  If Not (Key In ['a', 'A', 's', 'S', 'd', 'D', 'w', 'W', '0']) Then
    If isreadyUser2(twofield, true) Then
      //    If isready2(twofield) Then
      showmessage('You solved the Sudoku.');
End;

Procedure TForm16.Print1Click(Sender: TObject);
Begin
  form17.showmodal;
End;

Procedure TForm16.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  xo, x1, y1: integer;
  s: String;
Begin
  m2x := -1;
  m2x := -1;
  //xo := Form16.width Div 2 - 3 * Breite2; //round((Form16.width - Breite2 * 4) / 2.5);
  xo := Form16.width Div 2 - 2 * Breite2;
  If (X >= xo) And (x <= Breite2 * 4 + xo) And (y >= Breite2 Div 2) And (y <= Breite2 * 5 - breite2 Div 2) Then Begin
    // Ausrechnen der Koordinaten des neu Markierten Feldes
    x1 := (x - xo) Div Breite2;
    y1 := (y - Breite2 Div 2) Div Breite2;
    m2x := x1;
    m2y := y1;
  End;
  Draw2field;
  If ShowpossiblePencilinCaption1.checked And (twofield[m2x, m2y].value = 0) Then Begin
    GetPencil(twofield);
    s := '';
    For xo := 0 To 3 Do
      If twofield[m2x, m2y].pencil[xo] Then
        s := s + ' ' + inttostr(xo + 1);
    Form16.caption := s;
  End
  Else
    Caption := 'Sudoku ver. : ' + ver + ' by Corpsman | Targetsoft |';
End;

End.

