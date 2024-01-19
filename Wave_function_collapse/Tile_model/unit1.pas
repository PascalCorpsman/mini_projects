(******************************************************************************)
(* Wave function collapse                                          17.01.2024 *)
(*                                                                            *)
(* Version     : 0.02                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Implementation of wave function collapse algorithm           *)
(*               (Tile-Mode)                                                  *)
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
(* Known Issues:                                                              *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*               0.02 - Backjumping (like backtracking but with jumps)        *)
(*                                                                            *)
(******************************************************************************)
// Inspired by https://www.youtube.com/watch?v=rI_y2GAlQFM
(*
 Ideen: Man könnte eine "Beschränkung" einbauen, dass ein Teil Maximal X-Mal generiert werden darf
        Auf diese weise könnte man z.B. Weggabelungen reglementieren

 *)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ufilo;

Const

  ConLeft = 0;
  ConRight = 1;
  ConUp = 2;
  ConDown = 3;

Type

  TWVCImage = Record
    Filename: String;
    Bitmap: TBitmap;
    Prop: integer;
    Connectors: Array[0..3] Of String;
  End;

  TGridElement = Record
    Index: Integer; // -1 = noch nicht vergeben
    Forced: Boolean; // Wenn True, dann wurde dieser Wert über den User gesetzt.
    Possibilities: Array Of Boolean;
    PSum: Integer; // Anzahl der Possibilities = true
  End;

  TGrid = Array Of Array Of TGridElement;

  TBoolMatrix = Array Of Array Of Boolean;

  TPointList = Array Of TPoint;

  TGridStack = specialize TFilo < TGrid > ;

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    PaintBox1: TPaintBox;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    ScrollBox1: TScrollBox;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure Button8Click(Sender: TObject);
    Procedure Button9Click(Sender: TObject);
    Procedure Edit1KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure Edit2KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure Edit3KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure Edit4KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure Edit5KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure Image1Click(Sender: TObject);
    Procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure PaintBox1Paint(Sender: TObject);
  private
    Grid: TGrid;
    Connections: Array[0..3] Of TBoolMatrix; // Die Verbindungsmatrix der Bilder
    SelectedIndex: integer;
    Images: Array Of TWVCImage;
    Procedure AppendImageToLCL(index: integer);
    Procedure Clear;
    Procedure AddImage(Const Filename: String);
    Procedure SetSelectedImage(Index: Integer);
    Function LoadSystem(Const Filename: String): Boolean;
  public


  End;

Var
  Form1: TForm1;
  cancel: Boolean;

Implementation

{$R *.lfm}

Uses IniFiles, math;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Var
  i: Integer;
Begin
  caption := 'Wave Function Collapse Demo ver. 0.01';
  // Aufräumen, der Entwickler Hilfen
  edit1.free;
  edit2.free;
  edit3.free;
  edit4.free;
  edit5.free;
  Image1.Free;
  // Init
  Images := Nil;
  For i := 0 To 3 Do Begin
    Connections[i] := Nil;
  End;
  edit6.text := '10';
  edit7.text := '10';
  edit8.text := '10';
  Grid := Nil;
  Randomize;
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
  // Load a default tileset if one exists ;)
  If Loadsystem('paths.sys') Then Begin
    Button6.Click;
  End;
End;

Procedure TForm1.FormDestroy(Sender: TObject);
Begin
  Clear;
End;

Procedure TForm1.Image1Click(Sender: TObject);
Var
  index: Integer;
Begin
  index := TImage(sender).Tag;
  SetSelectedImage(index);
End;

Procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  w, h: Integer;
Begin
  If SelectedIndex = -1 Then exit;
  If SelectedIndex > high(Images) Then exit;
  Button7.Click;
  w := Images[SelectedIndex].Bitmap.Width;
  h := Images[SelectedIndex].Bitmap.Height;
  x := x Div w;
  y := y Div h;
  If ssleft In shift Then Begin
    Grid[x, y].Forced := true;
    Grid[x, y].Index := SelectedIndex;
  End
  Else Begin
    Grid[x, y].Forced := false;
    Grid[x, y].Index := -1;
  End;
  PaintBox1.Invalidate;
End;

Procedure TForm1.PaintBox1Paint(Sender: TObject);
Var
  i, j: Integer;
Begin
  // Clear Back
  PaintBox1.Canvas.Brush.Color := clRed;
  PaintBox1.Canvas.Rectangle(-1, -1, PaintBox1.Width + 1, PaintBox1.Height + 1);
  If Not assigned(Grid) Then exit;
  For i := 0 To high(Grid) Do Begin
    For j := 0 To high(Grid[i]) Do Begin
      If Grid[i, j].Index <> -1 Then Begin
        PaintBox1.Canvas.Draw(i * Images[0].Bitmap.Width, j * Images[0].Bitmap.Height, Images[Grid[i, j].Index].Bitmap);
      End;
    End;
  End;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  i: Integer;
  fn, ap, frn: String;
  sl: TStringList;
Begin
  // Import Images
  If OpenDialog1.Execute Then Begin
    Clear;
    sl := TStringList.Create;
    sl.Sorted := true;
    For i := 0 To OpenDialog1.Files.Count - 1 Do Begin
      sl.Add(OpenDialog1.Files[i]);
    End;
    sl.Sort;
    For i := 0 To OpenDialog1.Files.Count - 1 Do Begin
      fn := sl[i];
      ap := ExtractFilePath(ParamStr(0));
      frn := ExtractRelativePath(ap, fn);
      AddImage(frn);
    End;
    sl.free;
    If assigned(Images) Then Begin
      SetSelectedImage(0);
    End
    Else Begin
      SetSelectedImage(-1);
    End;
    button3.click;
  End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  // Load System
  If OpenDialog2.Execute Then Begin
    Loadsystem(OpenDialog2.FileName);
  End;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Var
  w, h, i, j: integer;
Begin
  // Set Dimension
  If Not assigned(Images) Then exit;
  w := StrToIntdef(Edit6.Text, 0);
  h := StrToIntdef(Edit7.Text, 0);
  PaintBox1.Width := w * Images[0].Bitmap.Width;
  PaintBox1.Height := h * Images[0].Bitmap.Height;
  setlength(Grid, w, h);
  For i := 0 To w - 1 Do Begin
    For j := 0 To h - 1 Do Begin
      Grid[i, j].Index := -1;
      Grid[i, j].Forced := false;
    End;
  End;
  PaintBox1.Invalidate;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Var
  ini: tinifile;
  i: Integer;
Begin
  If SaveDialog1.Execute Then Begin
    ini := TIniFile.Create(SaveDialog1.FileName);
    ini.CacheUpdates := true;
    ini.WriteInteger('Images', 'Count', length(Images));
    For i := 0 To high(Images) Do Begin
      ini.WriteString('Images', 'Image' + IntToStr(i), images[i].Filename);
      ini.WriteInteger('Images', 'ImageP' + IntToStr(i), images[i].Prop);
      ini.WriteString('Images', 'ImageL' + IntToStr(i), images[i].Connectors[ConLeft]);
      ini.WriteString('Images', 'ImageR' + IntToStr(i), images[i].Connectors[ConRight]);
      ini.WriteString('Images', 'ImageU' + IntToStr(i), images[i].Connectors[ConUp]);
      ini.WriteString('Images', 'ImageD' + IntToStr(i), images[i].Connectors[ConDown]);
    End;
    ini.Free;
  End;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button6Click(Sender: TObject);

Var
  InvalidResult: Boolean;
  InvalidPos: TPoint;

  Procedure UpdateGrid(i, j, n: Integer; Const M: TBoolMatrix);
  Var
    x, c: Integer;
  Begin
    // Außerhalb des Feldes
    If (i < 0) Or (i >= length(Grid)) Then exit;
    If (j < 0) Or (j >= length(Grid[i])) Then exit;
    c := 0;
    For x := 0 To length(Images) - 1 Do Begin
      If Not M[n, x] Then Grid[i, j].Possibilities[x] := false;
      If Grid[i, j].Possibilities[x] Then Begin
        inc(c);
      End;
    End;
    Grid[i, j].PSum := c;
  End;

  Procedure SetNum(i, j, n: Integer);
  Begin
    If Grid[i, j].Index <> -1 Then exit; // Wir versuchen ein bereits existierendes zu "ersetzen"
    Grid[i, j].Index := n;
    Grid[i, j].PSum := 0; // Das Feld ist ja gesetzt
    // Aktualisieren der "Möglichkeiten" der Angrenzenden
    UpdateGrid(i - 1, j, n, Connections[ConLeft]);
    UpdateGrid(i + 1, j, n, Connections[ConRight]);
    UpdateGrid(i, j - 1, n, Connections[ConUp]);
    UpdateGrid(i, j + 1, n, Connections[ConDown]);
  End;

  Function GetLeastProbList(): TPointList;
  Var
    ls, i, j: integer;
  Begin
    ls := length(Images);
    For i := 0 To high(Grid) Do Begin
      For j := 0 To high(Grid[i]) Do Begin
        If (Grid[i, j].Index = -1) Then Begin
          If (Grid[i, j].PSum = 0) Then Begin
            InvalidResult := true; // Das Backtracking anstoßen
            InvalidPos := point(i, j);
          End
          Else Begin
            ls := min(ls, Grid[i, j].PSum);
          End;
        End;
      End;
    End;
    result := Nil;
    For i := 0 To high(Grid) Do Begin
      For j := 0 To high(Grid[i]) Do Begin
        If (Grid[i, j].PSum = ls) And (Grid[i, j].Index = -1) Then Begin
          setlength(result, high(result) + 2);
          result[high(result)] := point(i, j);
        End;
      End;
    End;
  End;

  Procedure InitGrid();
  Var
    w, h, i, j, k: Integer;
    hasForced: Boolean;
  Begin
    w := length(Grid);
    h := length(Grid[0]);
    hasForced := false;
    For i := 0 To w - 1 Do Begin
      For j := 0 To h - 1 Do Begin
        If Not Grid[i, j].Forced Then Begin // Die User Gesetzten werden natürlich nicht gelöscht !
          Grid[i, j].Index := -1;
        End
        Else Begin
          hasForced := true;
        End;
        setlength(Grid[i, j].Possibilities, length(Images));
        For k := 0 To high(Grid[i, j].Possibilities) Do Begin
          If Images[k].Prop > 0 Then Begin
            Grid[i, j].Possibilities[k] := true;
          End
          Else Begin
            Grid[i, j].Possibilities[k] := false;
          End;
          Grid[i, j].PSum := length(Images);
        End;
      End;
    End;
    // Setzen des / der ersten Feldes /Felder
    If hasForced Then Begin
      // Fall 1: Der User hat eigene Vorgaben gemacht, dann übernehmen wir diese
      For i := 0 To w - 1 Do Begin
        For j := 0 To h - 1 Do Begin
          If Grid[i, j].Forced Then Begin
            k := Grid[i, j].Index;
            Grid[i, j].Index := -1;
            setNum(i, j, k);
          End;
        End;
      End;
    End
    Else Begin
      // Fall 2: Das Feld ist Leer -> Wir setzen ein zufälliges 1. Feld
      i := random(w);
      j := random(h);
      k := -1;
      While k = -1 Do Begin // Sicherstellen, das wir dieses Eine Teil auch verwenden dürfen !
        k := random(length(Images));
        If Images[k].Prop = 0 Then k := -1;
      End;
      setNum(i, j, k);
    End;
  End;

Var
  gs: TGridStack;

  Procedure PushGrid();
  Var
    g: TGrid;
    i, j: Integer;
  Begin
    g := Nil;
    setlength(g, length(Grid), length(Grid[0]));
    For i := 0 To high(Grid) Do Begin
      For j := 0 To high(Grid[0]) Do Begin
        g[i, j] := Grid[i, j];
      End;
    End;
    gs.Push(g);
  End;

  Procedure PopGrid();
  Var
    g: TGrid;
    i, j: Integer;
  Begin
    If gs.IsEmpty Then Begin
      // Der Jump ist derart Riesig, dass wir nichts mehr zum "popen" haben
      InitGrid();
    End
    Else Begin
      g := gs.Pop;
      For i := 0 To high(Grid) Do Begin
        For j := 0 To high(Grid[0]) Do Begin
          Grid[i, j] := g[i, j];
        End;
      End;
      setlength(g, 0, 0);
    End;
  End;

  Procedure ClearGridstack();
  Var
    g: TGrid;
  Begin
    While Not gs.IsEmpty Do Begin
      g := gs.Pop;
      setlength(g, 0, 0);
    End;
  End;

Var
  ac, r, i, j, k, PSum: Integer;
  pl: TPointList;
  a: Array Of Integer;
  BackJumpCounter: integer;
  WasInvalid: Boolean;
  StartTime: QWord;
  allowed_time: QWord;
  Triggered: Boolean;
Begin
  If button6.caption = 'Cancel' Then Begin
    Cancel := true;
    button6.caption := 'Create';
    Button6.Enabled := false;
    exit;
  End;
  Button6.Enabled := false;
  // Create
  // 1. Connection Matrix Berechnen
  For i := 0 To 3 Do Begin
    setlength(Connections[i], length(Images), length(Images));
  End;
  (* Theoretisch kann jedes Teil mit Jedem Teil verbunden werden, das ganze in alle 4 Richtungen *)
  For i := 0 To high(Images) Do Begin
    For j := 0 To high(Images) Do Begin
      Connections[ConLeft][i, j] := Images[j].Connectors[ConRight] = Images[i].Connectors[ConLeft];
      Connections[ConRight][i, j] := Images[j].Connectors[ConLeft] = Images[i].Connectors[ConRight];
      Connections[ConUp][i, j] := Images[j].Connectors[ConDown] = Images[i].Connectors[ConUp];
      Connections[ConDown][i, j] := Images[j].Connectors[ConUp] = Images[i].Connectors[ConDown];
    End;
  End;
  // 2. Grid Initialisieren
  If Not assigned(Grid) Then button3.Click;
  If Not assigned(Grid) Then exit;
  InitGrid();
  a := Nil;
  setlength(a, length(Images));

  (*
   * Die Theorie sagt folgendes
   * - Suchen aller Felder, welche die "Geringsten" Möglichkeiten haben
   * - Eines dieser Felder zufällig auf eine Möglichkeit setzen -> Repeat until alles besetzt.
   *
   * Eigentlich brüchte man einen Backtrack algorithmus, um immer alles zu füllen, aber ohne geht es meistens auch !
   *
   *)
  InvalidResult := false;
  WasInvalid := false;
  BackJumpCounter := 1;
  gs := TGridStack.create;
  pl := GetLeastProbList();
  StartTime := GetTickCount64;
  Triggered := false;
  allowed_time := min(500, length(Grid) * Length(Grid[0]) * 10);
  While assigned(pl) Do Begin
    // Wählen eines Zufälligen Feldes, aus der Liste derer die Noch Frei sind
    r := random(length(pl));
    i := pl[r].X;
    j := pl[r].Y;
    // Wenn wir ein Teil haben, was "Problematisch" ist dann setzen wir dieses nach dem Backtrack als 1.
    If WasInvalid Then Begin
      WasInvalid := false;
      i := InvalidPos.x;
      j := InvalidPos.Y;
    End;
    // Wählen eines Zufälligen Kandidaten aus der Liste der noch freien Kandidaten
    ac := 0;
    For k := 0 To length(Images) - 1 Do Begin
      If Grid[i, j].Possibilities[k] Then Begin
        a[ac] := k;
        inc(ac);
      End;
    End;
    // Der Versuch das Wahrscheinlichkeitsabhängig zu machen
    PSum := 0;
    For k := 0 To ac - 1 Do Begin
      psum := psum + Images[a[k]].Prop;
    End;
    r := random(psum + 1);
    PSum := 0;
    For k := 0 To ac - 1 Do Begin
      psum := psum + Images[a[k]].Prop;
      If r <= psum Then Begin
        setNum(i, j, a[k]);
        break;
      End;
    End;
    pl := GetLeastProbList();
    If InvalidResult Then Begin
      If CheckBox1.Checked Then Begin
        PaintBox1.Invalidate;
        Application.ProcessMessages;
        sleep(strtointdef(edit8.text, 10));
      End;
      For i := 0 To BackJumpCounter - 1 Do Begin
        PopGrid();
      End;
      InvalidResult := false;
      WasInvalid := true;
      pl := GetLeastProbList();
      BackJumpCounter := BackJumpCounter * 2 + 1;
    End
    Else Begin
      PushGrid();
      If BackJumpCounter > 1 Then
        BackJumpCounter := BackJumpCounter - 1;
    End;
    If (GetTickCount64 - StartTime > allowed_time) And Not Triggered Then Begin
      triggered := true;
      Showmessage(
        'Activateing preview now, as it seem to be to difficult to create the map.' + LineEnding +
        'Click on "Cancel" button to skip actual creation'
        );
      button6.caption := 'Cancel';
      CheckBox1.Checked := true;
      edit8.text := '1';
      Button6.Enabled := true;
      cancel := false;
    End;
    If CheckBox1.Checked Then Begin
      PaintBox1.Invalidate;
      sleep(strtointdef(edit8.text, 10));
      Application.ProcessMessages;
    End;
    // Wenn der User die Animation wieder Abgeschaltet hat, dann muss er hier die Chance bekommen dennoch Cancel zu drücken
    If Triggered Then Begin
      Application.ProcessMessages;
    End;
    If cancel Then pl := Nil;
  End;
  ClearGridstack();
  gs.free;
  PaintBox1.Invalidate;
  button6.caption := 'Create';
  Button6.Enabled := true;
End;

Procedure TForm1.Button7Click(Sender: TObject);
Var
  i, j: Integer;
Begin
  For i := 0 To high(Grid) Do Begin
    For j := 0 To high(Grid[i]) Do Begin
      If Not Grid[i, j].Forced Then Begin
        Grid[i, j].Index := -1;
      End;
    End;
  End;
  PaintBox1.Invalidate;
End;

Procedure TForm1.Button8Click(Sender: TObject);
Begin
  Showmessage(
    '1. Load images and set "connectors" or load a system' + LineEnding +
    '2. Set dimension and create' + LineEnding +
    '[Optional]' + LineEnding +
    '3. Click on a image on the left side to "select"' + LineEnding +
    '4. Click on the preview area to set element (right click removes)' + LineEnding +
    LineEnding +
    'If you get red areas after create, this means that your model is not "connected" enough.'
    );
End;

Procedure TForm1.Button9Click(Sender: TObject);
Var
  bm: TBitmap;
Begin
  // Export Image
  If SaveDialog2.Execute Then Begin
    bm := TBitmap.Create;
    bm.Width := PaintBox1.Width;
    bm.Height := PaintBox1.Height;
    bm.Canvas.CopyRect(rect(0, 0, bm.Width, bm.Height), PaintBox1.Canvas, rect(0, 0, bm.Width, bm.Height));
    bm.SaveToFile(SaveDialog2.FileName);
    bm.free;
  End;
End;

Procedure TForm1.Edit1KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
Var
  index: integer;
Begin
  index := Tedit(sender).Tag;
  Images[index].Connectors[ConLeft] := Tedit(sender).Text;
End;

Procedure TForm1.Edit2KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
Var
  index: integer;
Begin
  index := Tedit(sender).Tag;
  Images[index].Connectors[ConUp] := Tedit(sender).Text;
End;

Procedure TForm1.Edit3KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
Var
  index: integer;
Begin
  index := Tedit(sender).Tag;
  Images[index].Connectors[ConRight] := Tedit(sender).Text;
End;

Procedure TForm1.Edit4KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
Var
  index: integer;
Begin
  index := Tedit(sender).Tag;
  Images[index].Connectors[ConDown] := Tedit(sender).Text;
End;

Procedure TForm1.Edit5KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
Var
  index: integer;
Begin
  index := Tedit(sender).Tag;
  Images[index].Prop := strtointdef(Tedit(sender).Text, 0);
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  cancel := true;
End;

Procedure TForm1.Clear;
Var
  i: Integer;
Begin
  For i := ScrollBox1.ComponentCount - 1 Downto 0 Do Begin
    ScrollBox1.Components[i].free;
  End;
  For i := 0 To high(Images) Do Begin
    Images[i].Bitmap.Free;
  End;
  setlength(Images, 0);
End;

Procedure TForm1.AppendImageToLCL(index: integer);
Var
  img: TImage;
  e: TEdit;
  t: Integer;
Begin
  // Übernehmen in die Gui
  t := 8 + index * 128;
  // Das Wahrscheinlichkeits Edit
  e := TEdit.Create(ScrollBox1);
  e.Name := 'Prop' + inttostr(index + 1);
  e.Parent := ScrollBox1;
  e.Top := t;
  e.Left := 8;
  e.Width := 50;
  e.Tag := index;
  e.Text := inttostr(Images[index].Prop);
  e.OnKeyUp := @Edit5KeyUp;
  e.Hint := 'Propabillity for the image' + LineEnding +
    '0 = disabled' + LineEnding +
    'The higher the value, the more higher the propabillity for it to come up.';
  e.ShowHint := true;

  e := TEdit.Create(ScrollBox1);
  e.Name := 'Left' + inttostr(index + 1);
  e.Parent := ScrollBox1;
  e.Top := t + 48;
  e.Left := 8;
  e.Width := 50;
  e.Tag := index;
  e.Text := Images[index].Connectors[ConLeft];
  e.OnKeyUp := @Edit1KeyUp;
  e.Hint := 'Definition of the "left" connector, this has to match the "right" connector of the other images';
  e.ShowHint := true;

  e := TEdit.Create(ScrollBox1);
  e.Name := 'Top' + inttostr(index + 1);
  e.Parent := ScrollBox1;
  e.Top := t;
  e.Left := 64;
  e.Width := 50;
  e.Tag := index;
  e.Text := Images[index].Connectors[ConUp];
  e.OnKeyUp := @Edit2KeyUp;
  e.Hint := 'Definition of the "top" connector, this has to match the "bottom" connector of the other images';
  e.ShowHint := true;

  e := TEdit.Create(ScrollBox1);
  e.Name := 'Right' + inttostr(index + 1);
  e.Parent := ScrollBox1;
  e.Top := t + 48;
  e.Left := 120;
  e.Width := 50;
  e.Tag := index;
  e.Text := Images[index].Connectors[ConRight];
  e.OnKeyUp := @Edit3KeyUp;
  e.Hint := 'Definition of the "right" connector, this has to match the "left" connector of the other images';
  e.ShowHint := true;

  e := TEdit.Create(ScrollBox1);
  e.Name := 'Down' + inttostr(index + 1);
  e.Parent := ScrollBox1;
  e.Top := t + 88;
  e.Left := 64;
  e.Width := 50;
  e.Tag := index;
  e.Text := Images[index].Connectors[ConDown];
  e.OnKeyUp := @Edit4KeyUp;
  e.Hint := 'Definition of the "bottom" connector, this has to match the "top" connector of the other images';
  e.ShowHint := true;

  img := TImage.Create(ScrollBox1);
  img.Name := 'Img' + inttostr(index + 1);
  img.Parent := ScrollBox1;
  img.Top := t + 32;
  img.Left := 64;
  img.AutoSize := false;
  img.Width := 50;
  img.Height := 50;
  img.Center := true;
  img.Stretch := true;
  img.Picture.Assign(Images[index].Bitmap);
  img.Tag := index;
  img.OnClick := @Image1Click;
End;

Procedure TForm1.AddImage(Const Filename: String);
Var
  i: Integer;
Begin
  // Eintragen in die Images
  setlength(Images, high(Images) + 2);
  Images[high(Images)].Filename := Filename;
  Images[high(Images)].Bitmap := TBitmap.Create;
  Images[high(Images)].Bitmap.LoadFromFile(Filename);
  Images[high(Images)].Prop := 100;
  For i := 0 To 3 Do Begin
    Images[high(Images)].Connectors[i] := '';
  End;
  // PreCheck und ggf raus
  If high(Images) <> 0 Then Begin
    If (Images[0].Bitmap.Width <> Images[High(Images)].Bitmap.Width) Or
    (Images[0].Bitmap.Height <> Images[High(Images)].Bitmap.Height) Then Begin
      showmessage('Error, all images need to have same dimension, skipping: ' + Filename);
      Images[high(Images)].Bitmap.free;
      setlength(Images, high(Images));
      exit;
    End;
  End;
  AppendImageToLCL(high(Images));
End;

Procedure TForm1.SetSelectedImage(Index: Integer);
Begin
  If index >= 0 Then Begin
    Image2.Picture.Assign(Images[index].Bitmap);
  End
  Else Begin
    Image2.Picture.Clear;
  End;
  SelectedIndex := index;
End;

Function TForm1.LoadSystem(Const Filename: String): Boolean;
Var
  ini: TIniFile;
  i: Integer;
Begin
  result := false;
  Clear;
  If Not FileExists(Filename) Then exit;
  result := true;
  ini := TIniFile.Create(Filename);
  setlength(Images, ini.ReadInteger('Images', 'Count', 0));
  For i := 0 To high(Images) Do Begin
    images[i].Filename := ini.ReadString('Images', 'Image' + IntToStr(i), '');
    images[i].Bitmap := TBitmap.Create;
    images[i].Bitmap.LoadFromFile(images[i].Filename);
    images[i].Prop := ini.ReadInteger('Images', 'ImageP' + IntToStr(i), images[i].Prop);
    images[i].Connectors[ConLeft] := ini.ReadString('Images', 'ImageL' + IntToStr(i), '');
    images[i].Connectors[ConRight] := ini.ReadString('Images', 'ImageR' + IntToStr(i), '');
    images[i].Connectors[ConUp] := ini.ReadString('Images', 'ImageU' + IntToStr(i), '');
    images[i].Connectors[ConDown] := ini.ReadString('Images', 'ImageD' + IntToStr(i), '');
    AppendImageToLCL(i);
  End;
  ini.free;
  SetSelectedImage(-1);
  button3.Click;
End;

End.

