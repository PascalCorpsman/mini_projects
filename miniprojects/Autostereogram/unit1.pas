(******************************************************************************)
(* Autostereogram                                                  30.11.2025 *)
(*                                                                            *)
(* Version     : 0.02                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Pogram to calculate the magic eye images                     *)
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
(*               0.02 - Load "finished" images for testing via contextmenu    *)
(*                      ADD: ability to tweak depth image before converting   *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Menus, IntfGraphics;

Type
  TRawData = Array Of Array Of Byte;

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    OpenDialog1: TOpenDialog;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    ScrollBar3: TScrollBar;
    ScrollBar4: TScrollBar;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure MenuItem1Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure ScrollBar1Change(Sender: TObject);
    Procedure ScrollBar2Change(Sender: TObject);
    Procedure ScrollBar3Change(Sender: TObject);
    Procedure ScrollBar4Change(Sender: TObject);
  private
    Raw: TRawData;
    initialized: Boolean;

    Function BitmapToRawData(Const BM: TBitmap): TRawData;
  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses math, FPImage;

Function Map(vmin, vmax, v: Single; rmin, rmax: Single): Single;
Begin
  If (vmax - vmin = 0) Then Begin
    result := rmin;
    exit;
  End
  Else Begin
    result := ((((v - vmin) * (rmax - rmin)) / (vmax - vmin)) + rmin);
  End;
End;

{ TForm1 }

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  If OpenDialog1.Execute Then Begin
    initialized := true;
    Image1.Picture.LoadFromFile(OpenDialog1.FileName);
    label2.caption := inttostr(Image1.Picture.Width);
    label3.caption := inttostr(Image1.Picture.Height);
    Button2.Click;
  End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Var
  tmp_bm: TBitmap; // Temporäres Bild zur Umwandlung der Daten..
  tmp_img: TLazIntfImage;
  c: TFPColor;

  // Die Arbeitspuffer
  pattern: TRawData; // Das Random Pattern, welches sich pattern_div mal auf dem Zielbild widerhohlt
  depth_data: TRawData; // Die Tiefeninformationen, die es zu Kodieren gillt 0 = Weit Weg, 255 = sehr nah
  out_data: TRawData; // Das Ergebnis Graustufen Bild

  Image_Width, Image_Height: Integer;

  i, j, shift: Integer;
  pattern_width, pattern_div, idx: integer;

  minDepth, maxDepth: byte;
Begin
  If Not initialized Then exit;

  minDepth := ScrollBar3.Position;
  maxDepth := 255 - (100 - ScrollBar4.Position);
  // 1. Tiefenbild extrahieren
  tmp_bm := TBitmap.Create;
  tmp_bm.Assign(Image1.Picture);

  depth_data := BitmapToRawData(tmp_bm);

  // 2. Zielbild daten bestimmen
  Image_Width := tmp_bm.Width;
  Image_Height := tmp_bm.Height;

  // Optionales Nachträgliches "verbiegen" des eingeladenen Tiefenbildes ;)
  If (minDepth <> 1) Or (maxDepth <> 255) Then Begin
    For i := 0 To Image_Width - 1 Do
      For j := 0 To Image_Height - 1 Do Begin
        If depth_data[i, j] <> 0 Then Begin
          depth_data[i, j] := round(Map(1, 255, depth_data[i, j], minDepth, maxDepth));
        End;
      End;
  End;

  // 3. Pattern erstellen
  tmp_bm.Free;
  pattern := Nil;
  pattern_div := ScrollBar2.Position; // Sollte im Bereich 8 bis 12 Liegen, ggf durch den Nutzer bestimmen lassen..
  pattern_width := Image_Width Div pattern_div;

  SetLength(pattern, pattern_width, Image_Height);
  For i := 0 To pattern_width - 1 Do
    For j := 0 To Image_Height - 1 Do
      pattern[i, j] := Random(256);

  // 4. out_data vorbereiten
  out_data := Nil;
  SetLength(out_data, Image_Width, Image_Height);

  // 5. Die eigentliche "Projektion"
  For i := 0 To Image_Width - 1 Do Begin
    For j := 0 To Image_Height - 1 Do Begin
      // Links wird erst mal das Random Pattern widerhohlt
      If i < pattern_width Then Begin
        out_data[i, j] := pattern[i, j];
      End
      Else Begin
        // Verschieben der Pixel entsprechend der Tiefeninformationen ;)
//        shift := trunc(map(0, 255, depth_data[i, j], minDepth, maxDepth) / pattern_div);
        shift := trunc(depth_data[i, j] / pattern_div);
        idx := i - pattern_width + shift;
        idx := max(0, min(idx, Image_Width - 1));
        out_data[i, j] := out_data[idx, j];
      End;
    End;
  End;

  // 6. out_data in Bitmap zurückschreiben
  tmp_bm := TBitmap.Create;
  tmp_bm.Width := Image_Width;
  tmp_bm.Height := Image_Height;
  tmp_img := tmp_bm.CreateIntfImage;
  For i := 0 To Image_Width - 1 Do
    For j := 0 To Image_Height - 1 Do Begin
      c.Alpha := 0;
      c.Red := out_data[i, j] Shl 8;
      c.Green := c.Red;
      c.Blue := c.Red;
      tmp_img.Colors[i, j] := c;
    End;
  tmp_bm.LoadFromIntfImage(tmp_img);
  tmp_img.Free;

  // 7. Ergebnis anzeigen
  Image2.Picture.Assign(tmp_bm);

  // 8. Die Inverse Ansicht vorbereiten und anzeigen
  image3.Left := Image2.Left + image2.Width + 15;
  image3.Picture.Assign(tmp_bm);
  ScrollBar1.Left := image3.Left;
  ScrollBar1.Width := image2.Width;
  ScrollBar1.Max := image2.Width;
  label4.left := ScrollBar1.Left;
  label5.left := label4.left + label4.Width + 5;
  raw := out_data;
  If ScrollBar1.Position = pattern_width Then Begin
    ScrollBar1Change(Nil);
  End
  Else Begin
    ScrollBar1.Position := pattern_width;
  End;
  tmp_bm.Free;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Var
  bm: TBitmap;
  d, i, j: Integer;
  dc: single;
  db: Byte;
Begin
  (*
   * Erzeugt eine Tiefenkarte für eine Kugel
   *)
  bm := TBitmap.Create;
  bm.Width := 640;
  bm.Height := 480;
  bm.Canvas.Brush.Color := clBlack;
  bm.Canvas.Rectangle(-1, -1, 641, 481);
  For i := -200 To 200 Do Begin
    For j := -200 To 200 Do Begin
      d := sqr(i) + sqr(j);
      If d <= 200 * 200 Then Begin
        dc := (1 - sqr((sqrt(d) / 225))) * 255;
        db := min(max(round(dc), 0), 255);
        bm.canvas.pixels[320 + i, 240 + j] := db * $010101;
      End;
    End;
  End;
  If SaveDialog1.Execute Then Begin
    bm.SaveToFile(SaveDialog1.FileName);
  End;
  bm.Free;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  // https://www.youtube.com/watch?v=LEewrwTjOhQ
  (*
   * Magic eye texture generation inspired by: https://github.com/synesthesiam/magicpy
   *)
  caption := 'Magic eye image creator ver. 0.02, by Corpsman, www.Corpsman.de';
  Randomize;
  Raw := Nil;
  initialized := false;
  ScrollBar2Change(Nil);
  ScrollBar3Change(Nil);
  ScrollBar4Change(Nil);
End;

Procedure TForm1.MenuItem1Click(Sender: TObject);
Var
  b: TBitmap;
Begin
  If SaveDialog1.Execute Then Begin
    b := TBitmap.Create;
    b.assign(Image2.Picture);
    b.SaveToFile(SaveDialog1.FileName);
    b.free;
  End;
End;

Procedure TForm1.MenuItem2Click(Sender: TObject);
Var
  bm: TBitmap;
  pattern_width: integer;
Begin
  If OpenDialog1.Execute Then Begin
    bm := TBitmap.Create;
    bm.LoadFromFile(OpenDialog1.FileName);
    Image2.Picture.Assign(bm);

    image3.Left := Image2.Left + image2.Width + 15;
    image3.Picture.Assign(bm);
    ScrollBar1.Left := image3.Left;
    ScrollBar1.Width := image2.Width;
    ScrollBar1.Max := image2.Width;
    label4.left := ScrollBar1.Left;
    label5.left := label4.left + label4.Width + 5;
    raw := BitmapToRawData(bm);
    // TODO: mittels Koinzidenztest die Patternwith direkt bestimmen!
    pattern_width := bm.Width Div 10;
    If ScrollBar1.Position = pattern_width Then Begin
      ScrollBar1Change(Nil);
    End
    Else Begin
      ScrollBar1.Position := pattern_width;
    End;
    bm.Free;
  End;
End;

Procedure TForm1.ScrollBar1Change(Sender: TObject);
Var
  off, i, j: Integer;
  c: TFPColor;
  Target: TBitmap;
  RawImg: TLazIntfImage;
Begin
  off := ScrollBar1.Position;
  label5.caption := format('%dpx', [off]);
  If Not assigned(raw) Then exit;
  Target := TBitmap.create;
  target.Width := length(raw);
  target.Height := length(raw[0]);
  RawImg := Target.CreateIntfImage;
  For j := 0 To high(raw[0]) Do Begin
    For i := 0 To high(raw) Do Begin
      c.Alpha := 0;
      If i < off Then Begin
        c.Red := raw[i, j] Shl 8;
      End
      Else Begin
        c.Red := abs(raw[i, j] - raw[i - off, j]) Shl 8;
      End;
      c.Green := c.Red;
      c.Blue := c.Red;
      RawImg.Colors[i, j] := c;
    End;
  End;
  Target.LoadFromIntfImage(RawImg);
  Image3.Picture.Assign(target);
  RawImg.free;
  target.free;
End;

Procedure TForm1.ScrollBar2Change(Sender: TObject);
Begin
  Label6.Caption := inttostr(ScrollBar2.Position);
  Button2.Click;
End;

Procedure TForm1.ScrollBar3Change(Sender: TObject);
Begin
  Label11.Caption := inttostr(ScrollBar3.Position);
  Button2.Click;
End;

Procedure TForm1.ScrollBar4Change(Sender: TObject);
Begin
  Label12.Caption := inttostr(255 - (100 - ScrollBar4.Position));
  Button2.Click;
End;

Function TForm1.BitmapToRawData(Const BM: TBitmap): TRawData;
Var
  i, j: Integer;
  tmp_img: TLazIntfImage;
Begin
  Result := Nil;
  SetLength(Result, bm.Width, bm.Height);
  tmp_img := bm.CreateIntfImage;
  For i := 0 To bm.Width - 1 Do
    For j := 0 To bm.Height - 1 Do
      Result[i, j] := (tmp_img.Colors[i, j].Red Shr 8) And $FF;
  tmp_img.Free;

End;

End.

