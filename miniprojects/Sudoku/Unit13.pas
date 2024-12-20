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
Unit Unit13;

{$MODE ObjFPC}{$H+}

Interface

Uses
  SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, Sudoku5x5, ExtCtrls, LResources;

Type
  TForm13 = Class(TForm)
    MainMenu1: TMainMenu;
    Datei1: TMenuItem;
    New1: TMenuItem;
    Print1: TMenuItem;
    Load1: TMenuItem;
    Save1: TMenuItem;
    Close1: TMenuItem;
    Cheat1: TMenuItem;
    Action1: TMenuItem;
    Showpossiblepencilincaption1: TMenuItem;
    Reset1: TMenuItem;
    Step1: TMenuItem;
    Solveit1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Timer1: TTimer;
    Info1: TMenuItem;
    editfixednumbers1: TMenuItem;
    Procedure New1Click(Sender: TObject);
    Procedure Save1Click(Sender: TObject);
    Procedure Load1Click(Sender: TObject);
    Procedure Close1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormKeyPress(Sender: TObject; Var Key: Char);
    Procedure FormPaint(Sender: TObject);
    Procedure FormResize(Sender: TObject);
    Procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure Showpossiblepencilincaption1Click(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
    Procedure Reset1Click(Sender: TObject);
    Procedure Print1Click(Sender: TObject);
    Procedure Step1Click(Sender: TObject);
    Procedure Solveit1Click(Sender: TObject);
    Procedure Info1Click(Sender: TObject);
    Procedure editfixednumbers1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

Var
  Form13: TForm13;
  FiveField: T5field;
  bm5: Tbitmap;
  m5x, m5y, Breite5: integer;
  ae5: String;

Implementation

Uses
  math
  , usudoku
  , Unit6
  , Unit14
  , Unit15;

{$R *.lfm}

Procedure Draw5field;
Var
  xo, x, y: integer;
Begin
  With bm5.canvas Do Begin
    xo := round((Form13.width - Breite5 * 25) / 2.5);
    // Löschen des Bildschirms
    brush.style := bssolid;
    brush.color := FormBackground;
    rectangle(-1, -1, form13.width + 1, form13.height + 1);
    // Malen des Gitters
    For y := 0 To 24 Do
      For x := 0 To 24 Do Begin
        Case x Of
          0..4, 10..14, 20..24: Begin
              If y In [5..9, 15..19] Then Begin
                brush.color := Bretthintergrundfarbe1;
                If fiveField[x, y].marked Then brush.color := markedColor1;
              End
              Else Begin
                brush.color := Bretthintergrundfarbe2;
                If fiveField[x, y].marked Then brush.color := markedColor2;
              End;
            End;
          5..9, 15..19: Begin
              If y In [0..4, 10..14, 20..24] Then Begin
                brush.color := Bretthintergrundfarbe1;
                If fiveField[x, y].marked Then brush.color := markedColor1;
              End
              Else Begin
                brush.color := Bretthintergrundfarbe2;
                If fiveField[x, y].marked Then brush.color := markedColor2;
              End;
            End;
        End;
        // Farbe zum Markieren des Aktuellen Feldes
        If (x = m5x) And (y = m5y) Then brush.color := CursorMarker;
        // Malen des Feldes
        pen.color := Gitterfarbe;
        rectangle(xo + Breite5 * (x + 1), Breite5 * (y + 1), xo + Breite5 * (x + 2), Breite5 * (y + 2));
      End;
    Brush.Style := bsclear;
    // Textgröße
    Font.size := Breite5 Div 2;
    For y := 0 To 24 Do
      For x := 0 To 24 Do Begin
        // Textfarbe
        If fiveField[x, y].Fixed Then
          font.color := fixedcolor
        Else
          font.color := FontColor;
        If fiveField[x, y].value <> 0 Then
          Textout(xo + (Breite5 - Textwidth(inttostr(fiveField[x, y].value))) Div 2 + Breite5 * (x + 1), (Breite5 - Textheight(inttostr(fiveField[x, y].value))) Div 2 + Breite5 * (y + 1), inttostr(fiveField[x, y].value))
      End;
  End;
  form13.canvas.draw(0, 0, bm5);
  //  }
End;

Procedure TForm13.New1Click(Sender: TObject);
Begin
  form15.showmodal;
  If CreaterOptions > 0 Then Begin
    New5Field(fiveField, CreaterOptions);
  End;
  Draw5Field;
End;

Procedure TForm13.Save1Click(Sender: TObject);
Var
  F: TFilestream;
Begin
  If Savedialog1.execute Then Begin
    SaveDialog1.initialdir := ExtractFilePath(Savedialog1.Filename);
    openDialog1.initialdir := ExtractFilePath(Savedialog1.Filename);
    f := Tfilestream.create(Savedialog1.Filename, fmCreate Or fmOpenWrite);
    f.write(fivefield, sizeof(fivefield));
    f.Free;
  End;
End;

Procedure TForm13.Load1Click(Sender: TObject);
Var
  x, y: integer;
  F: TFilestream;
Begin
  If opendialog1.execute Then Begin
    SaveDialog1.initialdir := ExtractFilePath(opendialog1.Filename);
    openDialog1.initialdir := ExtractFilePath(opendialog1.Filename);
    f := Tfilestream.create(opendialog1.Filename, fmOpenRead);
    f.Read(fivefield, sizeof(fivefield));
    f.Free;
    For x := 0 To 24 Do
      For y := 0 To 24 Do
        fivefield[x, y].marked := false;
    draw5field;
  End;
End;

Procedure TForm13.Close1Click(Sender: TObject);
Begin
  Close;
End;

Procedure TForm13.FormCreate(Sender: TObject);
Begin
  ClearField(Fivefield);
  SaveDialog1.initialdir := ExtractFilePath(application.exename);
  openDialog1.initialdir := ExtractFilePath(application.exename);
  ae5 := '';
  Caption := 'Sudoku ver. : ' + ver + ' by Corpsman | www.Corpsman.de.vu |';
  bm5 := Tbitmap.create;
  bm5.PixelFormat := pf24bit;
  m5x := 0;
  m5y := 0;
End;

Procedure TForm13.FormDestroy(Sender: TObject);
Begin
  bm5.free;
  bm5 := Nil;
End;

Procedure TForm13.FormKeyPress(Sender: TObject; Var Key: Char);
Var
  xo, z: Integer;
  s: String;
Begin
  If m5x = -1 Then exit;
  If Key In ['a', 'A', 's', 'S', 'd', 'D', 'w', 'W'] Then Begin
    s := key;
    s := lowercase(s);
    If (s = 'a') And (m5x > 0) Then dec(m5x);
    If (s = 's') And (m5y < 24) Then inc(m5y);
    If (s = 'd') And (m5x < 24) Then inc(m5x);
    If (s = 'w') And (m5y > 0) Then dec(m5y);
    If ShowpossiblePencilinCaption1.checked And (fivefield[m5x, m5y].value = 0) Then Begin
      GetPencil(fivefield);
      s := '';
      For xo := 0 To 24 Do
        If fivefield[m5x, m5y].pencil[xo] Then
          s := s + ' ' + inttostr(xo + 1);
      Form13.caption := s;
    End
    Else
      Caption := 'Sudoku ver. : ' + ver + ' by Corpsman | Targetsoft |';
  End;
  If Key In ['0'..'9'] Then Begin
    If fivefield[m5x, m5y].Fixed And Not (editfixednumbers1.checked) Then exit;
    ae5 := ae5 + key;
    If Length(ae5) = 2 Then Begin
      z := strtoint(ae5);
      If z < 26 Then Begin
        mark(fivefield, z);
        If Z <> 0 Then Begin
          If Not fivefield[m5x, m5y].marked Then Begin
            mark(fivefield, 0);
            fivefield[m5x, m5y].Value := z;
            fivefield[m5x, m5y].Fixed := editfixednumbers1.checked;
          End
          Else Begin
            mark(fivefield, 0);
            If invalidnallow Then Begin
              fivefield[m5x, m5y].Value := z;
              fivefield[m5x, m5y].Fixed := editfixednumbers1.checked;
            End
            Else
              Showmessage('You cannot place ' + inttostr(z) + ' in this Field');
          End;
        End
        Else Begin
          // Löschen der Zahl
          fivefield[m5x, m5y].Value := z;
          fivefield[m5x, m5y].Fixed := false;
        End;
        ae5 := '';
      End;
    End;
  End;
  Draw5field;
  If Not (Key In ['a', 'A', 's', 'S', 'd', 'D', 'w', 'W', '0']) Then
    If isreadyUser5(fivefield, true) Then
      //    If isready5(fivefield) Then
      showmessage('You solved the Sudoku.');
End;

Procedure TForm13.FormPaint(Sender: TObject);
Begin
  If assigned(bm5) And Not form6.visible Then
    Draw5Field;
End;

Procedure TForm13.FormResize(Sender: TObject);
Begin
  If Form13.height < 480 Then Form13.height := 480;
  If Form13.Width < Form13.height + 130 Then Form13.Width := Form13.height + 130;
  Breite5 := min(Form13.height - 32, Form13.width) Div 27;
  If assigned(bm5) And Not form6.visible Then Begin
    bm5.width := form13.width;
    bm5.height := form13.height;
    Draw5field;
  End;
End;

Procedure TForm13.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  xo, x1, y1: integer;
  s: String;
Begin
  m5x := -1;
  m5x := -1;
  xo := round((Form13.width - Breite5 * 25) / 2.5);
  If (X >= Breite5 + xo) And (x <= Breite5 * 26 + xo) And
    (y >= Breite5) And (y <= Breite5 * 26) Then Begin
    // Ausrechnen der Koordinaten des neu Markierten Feldes
    x1 := (x - xo) Div Breite5 - 1;
    y1 := y Div Breite5 - 1;
    m5x := x1;
    m5y := y1;
  End;
  Draw5field;
  If ShowpossiblePencilinCaption1.checked And (fivefield[m5x, m5y].value = 0) Then Begin
    GetPencil(fivefield);
    s := '';
    For xo := 0 To 24 Do
      If fivefield[m5x, m5y].pencil[xo] Then
        s := s + ' ' + inttostr(xo + 1);
    Form13.caption := s;
  End
  Else
    Caption := 'Sudoku ver. : ' + ver + ' by Corpsman | Targetsoft |';
End;

Procedure TForm13.Showpossiblepencilincaption1Click(Sender: TObject);
Begin
  ShowpossiblePencilinCaption1.checked := Not ShowpossiblePencilinCaption1.checked;
End;

Procedure TForm13.Timer1Timer(Sender: TObject);
Begin
  ae5 := '';
End;

Procedure TForm13.Reset1Click(Sender: TObject);
Var
  x, y: integer;
Begin
  For x := 0 To 24 Do
    For y := 0 To 24 Do
      If Not fivefield[x, y].fixed Then
        fivefield[x, y].value := 0;
  Draw5field;
End;

Procedure TForm13.Print1Click(Sender: TObject);
Begin
  form14.showmodal;
End;

Procedure TForm13.Step1Click(Sender: TObject);
Begin
  If Not (Sudoku5Solvable(fivefield)) Then Begin
    showmessage('Impossible to solve Sudoku');
  End
  Else Begin
    Solve5(fivefield, true, 511);
    Draw5field;
    If isready5(fivefield) Then
      showmessage('You solved the Sudoku.');
  End;
End;

Procedure TForm13.Solveit1Click(Sender: TObject);
Begin
  If Not (Sudoku5Solvable(fivefield)) Then Begin
    showmessage('Impossible to solve Sudoku');
  End
  Else Begin
    Solve5(fivefield, false, 511);
    Draw5field;
    If isready5(fivefield) Then
      showmessage('You solved the Sudoku.');
  End;
End;

Procedure TForm13.Info1Click(Sender: TObject);
Var
  x, y, z: integer;
Begin
  z := 0;
  For x := 0 To 24 Do
    For y := 0 To 24 Do
      If fiveField[x, y].value <> 0 Then inc(z);
  Showmessage('This Sudoku needs ' + inttostr(25 * 25) + ' numbers to be complete.' + LineEnding + LineEnding +
    'At the moment there were ' + inttostr(z) + ' numbers inserted.');
End;

Procedure TForm13.editfixednumbers1Click(Sender: TObject);
Begin
  editfixednumbers1.checked := Not editfixednumbers1.checked;
End;

End.

