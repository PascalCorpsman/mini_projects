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
Unit Unit11;

{$MODE ObjFPC}{$H+}

Interface

Uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Sudoku4x4,
  Menus, ExtCtrls, LResources;

Type
  TForm11 = Class(TForm)
    MainMenu1: TMainMenu;
    Datei1: TMenuItem;
    New1: TMenuItem;
    Close1: TMenuItem;
    Action1: TMenuItem;
    Step1: TMenuItem;
    Solve1: TMenuItem;
    Reset1: TMenuItem;
    Timer1: TTimer;
    Cheat1: TMenuItem;
    ShowpossiblePencilinCaption1: TMenuItem;
    Load1: TMenuItem;
    Save1: TMenuItem;
    Print1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ratethesudoku1: TMenuItem;
    editfixednumbers1: TMenuItem;
    Procedure Close1Click(Sender: TObject);
    Procedure New1Click(Sender: TObject);
    Procedure FormPaint(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormResize(Sender: TObject);
    Procedure FormKeyPress(Sender: TObject; Var Key: Char);
    Procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure FormDestroy(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
    Procedure Step1Click(Sender: TObject);
    Procedure Solve1Click(Sender: TObject);
    Procedure Reset1Click(Sender: TObject);
    Procedure ShowpossiblePencilinCaption1Click(Sender: TObject);
    Procedure Load1Click(Sender: TObject);
    Procedure Save1Click(Sender: TObject);
    Procedure Print1Click(Sender: TObject);
    Procedure ratethesudoku1Click(Sender: TObject);
    Procedure editfixednumbers1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

Var
  Form11: TForm11;
  FourField: T4field;
  bm4: Tbitmap;
  m4x, m4y, Breite4: integer;
  ae4: String;

Implementation

Uses
  math
  , usudoku
  , Unit6
  , Unit12
  , Unit15;

{$R *.lfm}

Procedure Draw4field;
Var
  xo, x, y: integer;
Begin
  With bm4.canvas Do Begin
    xo := (Form11.width - Breite4 * 16) Div 3;
    // Löschen des Bildschirms
    brush.style := bssolid;
    brush.color := FormBackground;
    rectangle(-1, -1, form11.width + 1, form11.height + 1);
    // Malen des Gitters
    For y := 0 To 15 Do
      For x := 0 To 15 Do Begin
        Case x Of
          0..3, 8..11: Begin
              If y In [0..3, 8..11] Then Begin
                brush.color := Bretthintergrundfarbe1;
                If FourField[x, y].marked Then brush.color := markedColor1;
              End
              Else Begin
                brush.color := Bretthintergrundfarbe2;
                If FourField[x, y].marked Then brush.color := markedColor2;
              End;
            End;
          4..7, 12..15: Begin
              If y In [4..7, 12..15] Then Begin
                brush.color := Bretthintergrundfarbe1;
                If FourField[x, y].marked Then brush.color := markedColor1;
              End
              Else Begin
                brush.color := Bretthintergrundfarbe2;
                If FourField[x, y].marked Then brush.color := markedColor2;
              End;
            End;
        End;
        // Farbe zum Markieren des Aktuellen Feldes
        If (x = m4x) And (y = m4y) Then brush.color := CursorMarker;
        // Malen des Feldes
        pen.color := Gitterfarbe;
        rectangle(xo + Breite4 * (x + 1), Breite4 * (y + 1), xo + Breite4 * (x + 2), Breite4 * (y + 2));
      End;
    Brush.Style := bsclear;
    // Textgröße
    Font.size := Breite4 Div 2;
    For y := 0 To 15 Do
      For x := 0 To 15 Do Begin
        // Textfarbe
        If FourField[x, y].Fixed Then
          font.color := fixedcolor
        Else
          font.color := FontColor;
        If fourfield[x, y].value <> 0 Then
          Textout(xo + (Breite4 - Textwidth(inttostr(Fourfield[x, y].value))) Div 2 + Breite4 * (x + 1), (Breite4 - Textheight(inttostr(Fourfield[x, y].value))) Div 2 + Breite4 * (y + 1), inttostr(Fourfield[x, y].value))
      End;
  End;
  form11.canvas.draw(0, 0, bm4);
End;

Procedure TForm11.Close1Click(Sender: TObject);
Begin
  Close;
End;

Procedure TForm11.New1Click(Sender: TObject);
Begin
  form15.showmodal;
  If CreaterOptions > 0 Then Begin
    New4Field(fourField, CreaterOptions);
  End;
  Draw4Field;
End;

Procedure TForm11.FormPaint(Sender: TObject);
Begin
  If assigned(bm4) And Not form6.visible Then
    Draw4Field;
End;

Procedure TForm11.FormCreate(Sender: TObject);
Begin
  ClearField(FOurfield);
  SaveDialog1.initialdir := ExtractFilePath(application.exename);
  openDialog1.initialdir := ExtractFilePath(application.exename);
  ae4 := '';
  Caption := 'Sudoku ver. : ' + ver + ' by Corpsman | www.Corpsman.de.vu |';
  bm4 := Tbitmap.create;
  bm4.PixelFormat := pf24bit;
  m4x := 0;
  m4y := 0;
End;

Procedure TForm11.FormResize(Sender: TObject);
Begin
  If Form11.height < 480 Then Form11.height := 480;
  If Form11.Width < Form11.height + 130 Then Form11.Width := Form11.height + 130;
  Breite4 := min(Form11.height - 32, Form11.width) Div 18;
  If assigned(bm4) And Not form6.visible Then Begin
    bm4.width := form11.width;
    bm4.height := form11.height;
    Draw4field;
  End;
End;

Procedure TForm11.FormKeyPress(Sender: TObject; Var Key: Char);
Var
  xo, z: Integer;
  s: String;
Begin
  If m4x = -1 Then exit;
  If Key In ['a', 'A', 's', 'S', 'd', 'D', 'w', 'W'] Then Begin
    s := key;
    s := lowercase(s);
    If (s = 'a') And (m4x > 0) Then dec(m4x);
    If (s = 's') And (m4y < 15) Then inc(m4y);
    If (s = 'd') And (m4x < 15) Then inc(m4x);
    If (s = 'w') And (m4y > 0) Then dec(m4y);
    If ShowpossiblePencilinCaption1.checked And (Fourfield[m4x, m4y].value = 0) Then Begin
      GetPencil(Fourfield);
      s := '';
      For xo := 0 To 15 Do
        If Fourfield[m4x, m4y].pencil[xo] Then
          s := s + ' ' + inttostr(xo + 1);
      Form11.caption := s;
    End
    Else
      Caption := 'Sudoku ver. : ' + ver + ' by Corpsman | Targetsoft |';
  End;
  If Key In ['0'..'9'] Then Begin
    If Fourfield[m4x, m4y].Fixed And Not (editfixednumbers1.checked) Then exit;
    ae4 := ae4 + key;
    If Length(ae4) = 2 Then Begin
      z := strtoint(ae4);
      If z < 17 Then Begin
        mark(Fourfield, z);
        If Z <> 0 Then Begin
          If Not Fourfield[m4x, m4y].marked Then Begin
            mark(Fourfield, 0);
            Fourfield[m4x, m4y].Value := z;
            Fourfield[m4x, m4y].Fixed := editfixednumbers1.checked;
          End
          Else Begin
            mark(Fourfield, 0);
            If invalidnallow Then Begin
              Fourfield[m4x, m4y].Value := z;
              Fourfield[m4x, m4y].Fixed := editfixednumbers1.checked;
            End
            Else
              Showmessage('You cannot place ' + inttostr(z) + ' in this Field');
          End;
        End
        Else Begin
          // Löschen der Zahl
          Fourfield[m4x, m4y].Value := z;
          Fourfield[m4x, m4y].Fixed := false;
        End;
        ae4 := '';
      End;
    End;
  End;
  Draw4field;
  If Not (Key In ['a', 'A', 's', 'S', 'd', 'D', 'w', 'W', '0']) Then
    If isreadyUser4(Fourfield, true) Then
      //    If isready4(Fourfield) Then
      showmessage('You solved the Sudoku.');
End;

Procedure TForm11.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  xo, x1, y1: integer;
  s: String;
Begin
  m4x := -1;
  m4x := -1;
  xo := (Form11.width - Breite4 * 16) Div 3;
  If (X >= Breite4 + xo) And (x <= Breite4 * 17 + xo) And
    (y >= Breite4) And (y <= Breite4 * 17) Then Begin
    // Ausrechnen der Koordinaten des neu Markierten Feldes
    x1 := (x - xo) Div Breite4 - 1;
    y1 := y Div Breite4 - 1;
    m4x := x1;
    m4y := y1;
  End;
  Draw4field;
  If ShowpossiblePencilinCaption1.checked And (Fourfield[m4x, m4y].value = 0) Then Begin
    GetPencil(Fourfield);
    s := '';
    For xo := 0 To 15 Do
      If Fourfield[m4x, m4y].pencil[xo] Then
        s := s + ' ' + inttostr(xo + 1);
    Form11.caption := s;
  End
  Else
    Caption := 'Sudoku ver. : ' + ver + ' by Corpsman | Targetsoft |';
End;

Procedure TForm11.FormDestroy(Sender: TObject);
Begin
  bm4.free;
  bm4 := Nil;
End;

Procedure TForm11.Timer1Timer(Sender: TObject);
Begin
  ae4 := '';
End;

Procedure TForm11.Step1Click(Sender: TObject);
Begin
  If Not (Sudoku4Solvable(Fourfield)) Then Begin
    showmessage('Impossible to solve Sudoku');
  End
  Else Begin
    Solve4(Fourfield, true, 511);
    Draw4field;
    If isready4(Fourfield) Then
      showmessage('You solved the Sudoku.');
  End;
End;

Procedure TForm11.Solve1Click(Sender: TObject);
Begin
  If Not (Sudoku4Solvable(Fourfield)) Then Begin
    showmessage('Impossible to solve Sudoku');
  End
  Else Begin
    Solve4(Fourfield, false, 511);
    Draw4field;
    If isready4(Fourfield) Then
      showmessage('You solved the Sudoku.');
  End;
End;

Procedure TForm11.Reset1Click(Sender: TObject);
Var
  x, y: integer;
Begin
  For x := 0 To 15 Do
    For y := 0 To 15 Do
      If Not Fourfield[x, y].fixed Then
        Fourfield[x, y].value := 0;
  Draw4field;
End;

Procedure TForm11.ShowpossiblePencilinCaption1Click(Sender: TObject);
Begin
  ShowpossiblePencilinCaption1.checked := Not ShowpossiblePencilinCaption1.checked;
End;

Procedure TForm11.Load1Click(Sender: TObject);
Var
  x, y: integer;
  F: TFilestream;
Begin
  If opendialog1.execute Then Begin
    SaveDialog1.initialdir := ExtractFilePath(opendialog1.Filename);
    openDialog1.initialdir := ExtractFilePath(opendialog1.Filename);
    f := Tfilestream.create(opendialog1.Filename, fmOpenRead);
    f.Read(Fourfield, sizeof(Fourfield));
    f.Free;
    For x := 0 To 15 Do
      For y := 0 To 15 Do
        Fourfield[x, y].marked := false;
    draw4field;
  End;
End;

Procedure TForm11.Save1Click(Sender: TObject);
Var
  F: TFilestream;
Begin
  If Savedialog1.execute Then Begin
    SaveDialog1.initialdir := ExtractFilePath(Savedialog1.Filename);
    openDialog1.initialdir := ExtractFilePath(Savedialog1.Filename);
    f := Tfilestream.create(Savedialog1.Filename, fmCreate Or fmOpenWrite);
    f.write(Fourfield, sizeof(Fourfield));
    f.Free;
  End;
End;

Procedure TForm11.Print1Click(Sender: TObject);
Begin
  form12.showmodal;
End;

Procedure TForm11.ratethesudoku1Click(Sender: TObject);
Var
  x, y, z: integer;
Begin
  z := 0;
  For x := 0 To 15 Do
    For y := 0 To 15 Do
      If FourField[x, y].value <> 0 Then inc(z);
  Showmessage('This Sudoku needs ' + inttostr(16 * 16) + ' numbers to be complete.' + LineEnding + LineEnding +
    'At the moment there were ' + inttostr(z) + ' numbers inserted.');
End;

Procedure TForm11.editfixednumbers1Click(Sender: TObject);
Begin
  editfixednumbers1.checked := Not editfixednumbers1.checked;
End;

End.

