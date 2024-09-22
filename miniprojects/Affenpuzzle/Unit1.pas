(******************************************************************************)
(* Affenpuzzle                                                     27.04.2007 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Try to show how to solve the Ape Puzzle by tree prunging     *)
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
(* History     : 0.01 - Initial version                                       *)
(*                                                                            *)
(******************************************************************************)

Unit Unit1;

{$MODE ObjFPC}{$H+}

Interface

Uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, AffenSpiel,
  StdCtrls, IntfGraphics, fpImage;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    ListBox1: TListBox;
    Button5: TButton;
    Button6: TButton;
    Label1: TLabel;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure FormPaint(Sender: TObject);
    Procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure ListBox1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

Const
  Xoff = 100;
  Yoff = 220;

Var
  Form1: TForm1;
  img: Array[0..7] Of Tbitmap;
  AField: TField;
  Gefunden: Array Of TField;

Implementation

{$R *.lfm}

Procedure Drehen90Grad(Const Bitmap: TBitmap);
Var
  i, j: Integer;
  s, d: TLazIntfImage;
  dbm: Tbitmap;
  ImgHandle, ImgMaskHandle: HBitmap;
Begin
  dbm := Tbitmap.create;
  bitmap.PixelFormat := pf24bit;
  dbm.PixelFormat := pf24bit;
  dbm.Width := bitmap.height;
  dbm.Height := bitmap.Width;
  s := TLazIntfImage.Create(0, 0);
  s.LoadFromBitmap(bitmap.Handle, bitmap.MaskHandle);
  d := TLazIntfImage.Create(0, 0);
  d.LoadFromBitmap(dbm.Handle, dbm.MaskHandle);
  For i := 0 To Bitmap.Width - 1 Do
    For j := 0 To Bitmap.Height - 1 Do Begin
      d.Colors[Bitmap.Height - j - 1, i] := s.Colors[i, j];
    End;
  d.CreateBitmaps(ImgHandle, ImgMaskHandle, false);
  Bitmap.Handle := ImgHandle;
  Bitmap.MaskHandle := ImgMaskHandle;
  s.free;
  d.free;
  dbm.free;
End;

Procedure Drehen180Grad(Const Bitmap: TBitmap);
Var
  i, j: Integer;
  s, d: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
Begin
  bitmap.PixelFormat := pf24bit;
  s := TLazIntfImage.Create(0, 0);
  s.LoadFromBitmap(bitmap.Handle, bitmap.MaskHandle);
  d := TLazIntfImage.Create(0, 0);
  d.LoadFromBitmap(bitmap.Handle, bitmap.MaskHandle);
  //  d.LoadFromBitmap(dbm.Handle, dbm.MaskHandle);
  For i := 0 To Bitmap.Width - 1 Do
    For j := 0 To Bitmap.Height - 1 Do Begin
      d.Colors[bitmap.width - i - 1, bitmap.height - j - 1] := s.Colors[i, j];
    End;
  d.CreateBitmaps(ImgHandle, ImgMaskHandle, false);
  Bitmap.Handle := ImgHandle;
  Bitmap.MaskHandle := ImgMaskHandle;
  s.free;
  d.free;
End;

// Fertig

Procedure DrawPiece(Const Canvas: TCanvas; x, y: integer; Teil: TTeil);
Const
  w = 22;
  h = 22;
Var
  j, i, r: Integer;
  bm: Tbitmap;
Begin
  With canvas Do Begin
    bm := Tbitmap.create;
    bm.width := w;
    bm.height := h;
    bm.PixelFormat := pf24bit;
    Brush.Color := clwhite;
    pen.color := clblack;
    // Hintergrund
    rectangle(x, y, x + 100, y + 100);
    For j := 0 To 3 Do Begin
      // Umrechnen TeilItem nach IMG index
      i := abs(Teil.Teilitems[j]) - 1;
      If Teil.Teilitems[j] < 0 Then inc(i, 4);
      bm.canvas.Draw(0, 0, img[i]);
      bm.pixelformat := pf24bit;
      r := j Mod 4;
      Case r Of
        0: Drehen180Grad(bm);
        1: Begin
            Drehen90Grad(bm);
            bm.pixelformat := pf24bit;
            Drehen180Grad(bm);
          End;
        3: Drehen90Grad(bm);
      End;
      //      Canvas.textout(x + j * 20, y - 20, inttostr(i));
      Case r Of
        0: draw(x + (100 - w) Div 2, y, bm);
        1: draw(x + 100 - w, y + (100 - h) Div 2, bm);
        2: draw(x + (100 - w) Div 2, y + 100 - h, bm);
        3: draw(x, y + (100 - h) Div 2, bm);
      End;
      canvas.textout(x + 45, y + 45, inttostr(teil.teil));
    End;
    bm.free;
  End;
End;

Procedure DrawfieldListWise(Const Canvas: Tcanvas; x, y: integer; Field: Tfield);
Var
  j, I: Integer;
  xd, yd: Integer;
Begin
  j := -1;
  For i := 1 To 9 Do Begin
    If Field[i].IstStart Then j := i;
  End;
  // Anzeigen der Kette die Reihenfolge ist hierbei
  //
  //    8   3  4
  //    7   1  2
  //    9   5  6
  //
  If j <> -1 Then Begin
    // 1
    xd := x + 100;
    yd := y + 100;
    DrawPiece(canvas, xd, yd, field[j]);
    // 2
    j := Field[j].NaechstesTeil;
    xd := x + 200;
    yd := y + 100;
    DrawPiece(canvas, xd, yd, field[j]);
    // 3
    j := Field[j].NaechstesTeil;
    xd := x + 100;
    yd := y;
    DrawPiece(canvas, xd, yd, field[j]);
    // 4
    j := Field[j].NaechstesTeil;
    xd := x + 200;
    yd := y;
    DrawPiece(canvas, xd, yd, field[j]);
    // 5
    j := Field[j].NaechstesTeil;
    xd := x + 100;
    yd := y + 200;
    DrawPiece(canvas, xd, yd, field[j]);
    // 6
    j := Field[j].NaechstesTeil;
    xd := x + 200;
    yd := y + 200;
    DrawPiece(canvas, xd, yd, field[j]);
    // 7
    j := Field[j].NaechstesTeil;
    xd := x;
    yd := y + 100;
    DrawPiece(canvas, xd, yd, field[j]);
    // 8
    j := Field[j].NaechstesTeil;
    xd := x;
    yd := y;
    DrawPiece(canvas, xd, yd, field[j]);
    // 9
    j := Field[j].NaechstesTeil;
    xd := x;
    yd := y + 200;
    DrawPiece(canvas, xd, yd, field[j]);
  End;
End;

Procedure Drawfield(Const Canvas: Tcanvas; x, y: integer; Field: Tfield);
Var
  i, j: Integer;
Begin
  For i := 0 To 2 Do
    For j := 0 To 2 Do
      DrawPiece(canvas, x + 100 * i, y + 100 * j, field[(i + 1) + (j * 3)]);
End;

Procedure TForm1.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Var
  i: integer;
Begin
  setlength(Gefunden, 0);
  FreeStack;
  For i := 0 To 7 Do
    IMG[i].free;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  Listbox1.items.clear;
  setlength(Gefunden, 0);
  ClearStack;
  Findefelder(Afield);
  Drawfield(canvas, xoff, yoff, afield);
  While Not isemptystack Do Begin
    setlength(Gefunden, high(Gefunden) + 2);
    Gefunden[High(gefunden)] := PopStack;
    Listbox1.items.add('LSG' + inttostr(Listbox1.items.count + 1));
  End;
  If Listbox1.items.Count = 0 Then
    showmessage('No solutions found for these parts !!');
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  If Listbox1.itemindex <> -1 Then
    DrawfieldListwise(canvas, xoff + 3 * 100 + 50, yoff, Gefunden[Listbox1.itemindex]);
End;

Procedure TForm1.FormCreate(Sender: TObject);
Var
  s: String;
Begin
  Caption := 'Affenpuzzle by Corpsman | Support : www.Corpsman.de';
  Label1.caption := 'Affenpuzzle by Corpsman' + LineEnding +
    'www.Corpsman.de' + LineEnding +
    'How to:' + LineEnding +
    'Each part consists of 4 "edges", click on the small images to change these.' + LineEnding + LineEnding +
    'By clicking on search, the pruned deapth search algorithm tries to find all solutions for the' + LineEnding +
    'given set of parts and lists them on the left.' + LineEnding + LineEnding +
    'All solutions that can be created by rotating the complete field are ignored.';

  setlength(Gefunden, 0);
  //  Weiter mit Vergleichen Und Testen !!
  Initialfield(Afield);
  s := IncludeTrailingBackslash(ExtractFilePath(paramstr(0)));
  Opendialog1.initialdir := s;
  savedialog1.initialdir := s;
  s := s + 'Affen' + PathDelim;
  img[0] := Tbitmap.create;
  img[0].LoadFromFile(s + 'Top1.bmp');
  img[1] := Tbitmap.create;
  img[1].LoadFromFile(s + 'Top2.bmp');
  img[2] := Tbitmap.create;
  img[2].LoadFromFile(s + 'Top3.bmp');
  img[3] := Tbitmap.create;
  img[3].LoadFromFile(s + 'Top4.bmp');
  img[4] := Tbitmap.create;
  img[4].LoadFromFile(s + 'Boden1.bmp');
  img[5] := Tbitmap.create;
  img[5].LoadFromFile(s + 'Boden2.bmp');
  img[6] := Tbitmap.create;
  img[6].LoadFromFile(s + 'Boden3.bmp');
  img[7] := Tbitmap.create;
  img[7].LoadFromFile(s + 'Boden4.bmp');
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Begin
  Initialfield(Afield);
  Drawfield(canvas, xoff, yoff, afield);
End;

Procedure TForm1.FormPaint(Sender: TObject);
Begin
  Drawfield(canvas, xoff, yoff, afield);
  Button2Click(Nil);
End;

Procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  px, py: integer;
  Part: Integer;
  Item: Integer;
Begin
  If (x >= Xoff) And (X <= xoff + 300) And
    (y >= Yoff) And (Y <= Yoff + 300) Then Begin
    x := x - xoff;
    y := y - yoff;
    px := x Div 100;
    py := y Div 100;
    Part := px + py * 3 + 1; // Errechnen der Teil Nummer
    x := x Mod 100;
    y := y Mod 100;
    item := -1;
    If (y < 25) And (x In [25..75]) Then Item := 0;
    If (y > 75) And (x In [25..75]) Then Item := 2;
    If (x > 75) And (y In [25..75]) Then Item := 1;
    If (x < 25) And (y In [25..75]) Then Item := 3;
    If Ssleft In shift Then Begin
      px := Afield[Part].Teilitems[item] + 1;
      If Px = 5 Then px := -4;
      If PX = 0 Then Px := 1;
      Afield[Part].Teilitems[item] := PX;
    End;
    If ssright In shift Then Begin
      px := Afield[Part].Teilitems[item] - 1;
      If Px = -5 Then px := 4;
      If PX = 0 Then Px := -1;
      Afield[Part].Teilitems[item] := PX;
    End;
    Drawfield(canvas, xoff, yoff, afield);
    (* Löschen der Suchergebnisse *)
    If item <> -1 Then Begin
      Listbox1.items.clear;
      setlength(Gefunden, 0);
      ClearStack;
    End;
  End;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Var
  f: Tfilestream;
  s: String;
Begin
  If Opendialog1.execute Then Begin
    f := TFileStream.create(opendialog1.FileName, fmopenread);
    s := IncludeTrailingBackslash(ExtractFilePath(opendialog1.FileName));
    opendialog1.initialdir := s;
    savedialog1.initialdir := s;
    f.read(Afield, sizeof(afield));
    f.free;
    Drawfield(canvas, xoff, yoff, afield);
  End;
End;

Procedure TForm1.Button6Click(Sender: TObject);
Var
  f: Tfilestream;
  s: String;
Begin
  If Savedialog1.execute Then Begin
    f := TFileStream.create(Savedialog1.FileName, fmopenwrite Or fmCreate);
    s := IncludeTrailingBackslash(ExtractFilePath(Savedialog1.FileName));
    opendialog1.initialdir := s;
    savedialog1.initialdir := s;
    f.write(Afield, sizeof(afield));
    f.free;
  End;
End;

Procedure TForm1.ListBox1Click(Sender: TObject);
Begin
  Button2.Click;
End;

End.

