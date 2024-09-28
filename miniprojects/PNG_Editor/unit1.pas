(******************************************************************************)
(* PNG Editor                                                      ??.??.???? *)
(*                                                                            *)
(* Version     : 0.06                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Split and merge alpha channel of .png Images                 *)
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
(*               0.02 - Anzeige der Bild Dimensionen, wenn vorhanden          *)
(*               0.03 - Checkbox beim Laden der Transparenz zum Binarisieren  *)
(*                      via clFuchsia                                         *)
(*               0.04 - Umstellung auf eigene Zoom Algorithmen beim Rendern   *)
(*               0.05 - start mit drag / drop                                 *)
(*               0.06 - improove drag / drop                                  *)
(*                                                                            *)
(******************************************************************************)

Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    CheckBox1: TCheckBox;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    Label2: TLabel;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDropFiles(Sender: TObject; Const FileNames: Array Of String);
  private
    fNormalImage: TBitmap; // Original unscalliert
    fAlphaImage: TBitmap; // Original unscalliert
    Procedure LoadAlpha(Const FileName: String);
    Procedure LoadNormal(Const FileName: String);
    Procedure RefreshDimensions();
    Procedure Clear(Normal, Alpha: Boolean);
    Function CreateScaledImage(Const Source: TBitmap): TBitmap;
    Function LoadImage(Const Filename: String): TBitmap;
    Procedure LoadPNG(Const Filename: String);
  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses
  IntfGraphics, fpImage, LCLType, ugraphics, GraphType, Math, types;

{ TForm1 }

Procedure TForm1.Button1Click(Sender: TObject);

Begin
  // Load PNG
  If OpenDialog1.Execute Then Begin
    LoadPNG(OpenDialog1.FileName);
  End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  // Load Normal
  If OpenDialog2.Execute Then Begin
    LoadNormal(OpenDialog2.FileName);
  End;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  // Load Alpha
  If OpenDialog2.Execute Then Begin
    LoadAlpha(OpenDialog2.FileName);
  End;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Begin
  // Save Normal
  If Not assigned(fNormalImage) Then Begin
    showmessage('Error, no image to save.');
    exit;
  End;
  If SaveDialog1.Execute Then Begin
    fNormalImage.SaveToFile(SaveDialog1.FileName);
  End;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Begin
  // Save Alpha
  If Not assigned(fAlphaImage) Then Begin
    showmessage('Error, no image to save.');
    exit;
  End;
  If SaveDialog1.Execute Then Begin
    fAlphaImage.SaveToFile(SaveDialog1.FileName);
  End;
End;

Procedure TForm1.Button6Click(Sender: TObject);
Var
  DestBitmap: TBitmap;
  NormalSource, AlphaSource, Dest: TLazIntfImage;
  NormalColor, AlphaColor: TFPColor;
  ImgHandle, ImgMaskHandle: HBitmap;
  png: TPortableNetworkGraphic;
  i, j: Integer;
Begin
  If Not assigned(fNormalImage) Or Not assigned(fAlphaImage) Then Begin
    showmessage('Error, need both images to store as .png');
    exit;
  End;
  If (fAlphaImage.Width <> fNormalImage.Width) Or (fAlphaImage.Height <> fNormalImage.Height) Then Begin
    ShowMessage('Error, alphamask and normal image differ in size.');
    exit;
  End;
  // Save PNG
  If SaveDialog2.Execute Then Begin
    DestBitmap := TBitmap.Create;
    DestBitmap.Width := fAlphaImage.Width;
    DestBitmap.Height := fAlphaImage.Height;

    NormalSource := TLazIntfImage.Create(0, 0);
    NormalSource.LoadFromBitmap(fNormalImage.Handle, fNormalImage.MaskHandle);
    AlphaSource := TLazIntfImage.Create(0, 0);
    AlphaSource.LoadFromBitmap(fAlphaImage.Handle, fAlphaImage.MaskHandle);

    Dest := TLazIntfImage.Create(0, 0, [riqfRGB, riqfAlpha]);
    Dest.SetSize(fAlphaImage.Width, fAlphaImage.Height);

    For i := 0 To fAlphaImage.Width - 1 Do Begin
      For j := 0 To fAlphaImage.Height - 1 Do Begin
        NormalColor := NormalSource.Colors[i, j];
        AlphaColor := AlphaSource.Colors[i, j];
        NormalColor.Alpha := AlphaColor.red; // Egal ist ja eh Graustufen
        Dest.Colors[i, j] := NormalColor;
      End;
    End;
    Dest.CreateBitmaps(ImgHandle, ImgMaskHandle, false);
    DestBitmap.Handle := ImgHandle;
    DestBitmap.MaskHandle := ImgMaskHandle;
    png := TPortableNetworkGraphic.Create;
    png.Assign(DestBitmap);
    png.SaveToFile(SaveDialog2.FileName);
    png.free;
    DestBitmap.free;
    Dest.free;
  End;
End;

Procedure TForm1.Button7Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  Clear(true, true);
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  Label2.Caption := '';
  caption := 'PNG-Editor ver. 0.06 by Corpsman';
  Application.Title := caption;
  fNormalImage := Nil;
  fAlphaImage := Nil;
End;

Procedure TForm1.FormDropFiles(Sender: TObject; Const FileNames: Array Of String
  );
Begin
  // Differenzieren nach "ziel" und "quelle" ;)
  If FileExists(FileNames[0]) Then Begin
    If lowercase(ExtractFileExt(FileNames[0])) = '.png' Then Begin
      LoadPNG(FileNames[0]);
    End
    Else Begin
      If PtInRect(Image1.BoundsRect, ScreenToClient(Mouse.CursorPos)) Then Begin
        LoadNormal(FileNames[0]);
      End
      Else Begin
        LoadAlpha(FileNames[0]);
      End;
    End;
  End;
End;

Procedure TForm1.RefreshDimensions;
Var
  s: String;
Begin
  s := '';
  If assigned(fNormalImage) Then Begin
    s := format('Image: %d x %d', [fNormalImage.Width, fNormalImage.Height]);
  End;
  If assigned(fAlphaImage) Then Begin
    If s <> '' Then s := s + LineEnding;
    s := s + format('Alpha: %d x %d', [fAlphaImage.Width, fAlphaImage.Height]);
  End;
  label2.caption := s;
End;

Procedure TForm1.LoadNormal(Const FileName: String);
Var
  ScaledImage: TBitmap;
Begin
  Clear(true, false);
  fNormalImage := LoadImage(FileName);
  fNormalImage.Transparent := false;
  ScaledImage := CreateScaledImage(fNormalImage);
  Image1.Picture.Assign(ScaledImage);
  ScaledImage.free;
  RefreshDimensions();
End;

Procedure TForm1.LoadAlpha(Const FileName: String);
  Procedure BinariseByCLFuchsia(Const Bitmap: TBitmap);
  Var
    TempIntfImg: TLazIntfImage;
    ImgHandle, ImgMaskHandle: HBitmap;
    fuchsia, CurColor: TFPColor;
    i, j: Integer;
  Begin
    TempIntfImg := TLazIntfImage.Create(0, 0);
    TempIntfImg.LoadFromBitmap(Bitmap.Handle, Bitmap.MaskHandle);
    For j := 0 To bitmap.height - 1 Do
      For i := 0 To bitmap.width - 1 Do Begin
        fuchsia := ColorToFPColor(clFuchsia);
        If TempIntfImg.Colors[i, j] = fuchsia Then Begin
          curcolor.red := 0;
        End
        Else Begin
          curcolor.red := 255 * 256;
        End;
        curcolor.green := curcolor.red;
        curcolor.blue := curcolor.red;
        curcolor.alpha := 255 * 256;
        TempIntfImg.Colors[i, j] := curcolor;
      End;
    TempIntfImg.CreateBitmaps(ImgHandle, ImgMaskHandle, false);
    Bitmap.Handle := ImgHandle;
    Bitmap.MaskHandle := ImgMaskHandle;
    TempIntfImg.free;
  End;
Var
  ScaledImage: TBitmap;
Begin
  Clear(false, true);
  fAlphaImage := LoadImage(FileName);
  fAlphaImage.Transparent := false;
  If CheckBox1.Checked Then Begin
    BinariseByCLFuchsia(fAlphaImage);
  End
  Else Begin
    ConvertToGrayscale(fAlphaImage);
  End;
  ScaledImage := CreateScaledImage(fAlphaImage);
  Image2.Picture.Assign(ScaledImage);
  ScaledImage.free;
  RefreshDimensions();
End;

Procedure TForm1.Clear(Normal, Alpha: Boolean);
Begin
  If Normal Then Begin
    If assigned(fNormalImage) Then Begin
      fNormalImage.free;
    End;
    fNormalImage := Nil;
    image1.Picture.Clear;
  End;
  If Alpha Then Begin
    If assigned(fAlphaImage) Then Begin
      fAlphaImage.free;
    End;
    fAlphaImage := Nil;
    image2.Picture.Clear;
  End;
End;

Function TForm1.CreateScaledImage(Const Source: TBitmap): TBitmap;
Var
  dimMax, dimx, dimy: integer;
  scale: Single;
  Mode: TInterpolationMode;
Begin
  // Rechnet Source so um, dass es Maximal in ein Bild der Größe 256x256 passt
  // Ist Source zu Groß wird es Bilinear klein gerechnet
  // sonst Nearest Neighbor gezoomt
  result := TBitmap.Create;
  result.Width := 256;
  result.Height := 256;
  // Den nachher nicht übermalten Teil "transparent" machen
  result.Transparent := false;
  result.Canvas.Pen.Color := clbtnface;
  result.Canvas.Brush.Color := clbtnface;
  //  result.Transparent := true;
  //  result.TransparentColor := form1.Color;
  //  result.Canvas.Pen.Color := form1.Color;
  //  result.Canvas.Brush.Color := form1.Color;
  result.Canvas.Rectangle(0, 0, 257, 257);
  dimx := source.Width;
  dimy := source.Height;
  dimMax := max(dimx, dimy);
  scale := 256 / dimMax;
  If dimMax > 256 Then Begin
    // Shrink
    mode := imBilinear;
  End
  Else Begin
    // Zoom
    mode := imNone;
  End;
  // Source Transparents kaputt machen
  source.TransparentColor := clNone;
  source.Transparent := false;
  Stretchdraw(result,
    rect(
    round((256 - scale * dimx) / 2),
    round((256 - scale * dimy) / 2),
    round((256 + scale * dimx) / 2),
    round((256 + scale * dimy) / 2)
    ), source, Mode);
End;

Function TForm1.LoadImage(Const Filename: String): TBitmap;
Var
  gc: TGraphicClass;
  g: TGraphic;
Begin
  result := TBitmap.Create;
  // So generisch wie es eben nur geht Das Bild laden und als TBitmap wieder raus ;)
  gc := TPicture.FindGraphicClassWithFileExt(ExtractFileExt(Filename));
  g := gc.Create;
  g.LoadFromFile(FileName);
  result.Assign(g);
  g.free;
End;

Procedure TForm1.LoadPNG(Const Filename: String);
Var
  p: TPortableNetworkGraphic;
  i, j: Integer;
  Normal, Alpha: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
  NormalColor, AlphaColor: TFPColor;
  ScaledImage: TBitmap;
Begin
  OpenDialog1.InitialDir := ExtractFileDir(Filename);
  OpenDialog2.InitialDir := ExtractFileDir(Filename);
  SaveDialog1.InitialDir := ExtractFileDir(Filename);
  SaveDialog2.InitialDir := ExtractFileDir(Filename);
  Clear(true, true);
  p := TPortableNetworkGraphic.Create;
  p.LoadFromFile(FileName);
  fNormalImage := TBitmap.Create;
  fNormalImage.Assign(p);
  fAlphaImage := TBitmap.Create;
  fAlphaImage.Width := fNormalImage.Width;
  fAlphaImage.Height := fNormalImage.Height;
  Normal := TLazIntfImage.Create(0, 0);
  Normal.LoadFromBitmap(fNormalImage.Handle, fNormalImage.MaskHandle);
  Alpha := TLazIntfImage.Create(0, 0);
  Alpha.LoadFromBitmap(fAlphaImage.Handle, fAlphaImage.MaskHandle);
  For i := 0 To fNormalImage.Width - 1 Do Begin
    For j := 0 To fNormalImage.Height - 1 Do Begin
      NormalColor := Normal.Colors[i, j];
      AlphaColor.red := NormalColor.alpha;
      AlphaColor.green := NormalColor.alpha;
      AlphaColor.blue := NormalColor.alpha;
      AlphaColor.alpha := 255 * 256;
      Alpha.Colors[i, j] := AlphaColor;
      NormalColor.alpha := 255 * 256;
      Normal.Colors[i, j] := NormalColor;
    End;
  End;
  Normal.CreateBitmaps(ImgHandle, ImgMaskHandle, false);
  fNormalImage.Handle := ImgHandle;
  fNormalImage.MaskHandle := ImgMaskHandle;
  Alpha.CreateBitmaps(ImgHandle, ImgMaskHandle, false);
  fAlphaImage.Handle := ImgHandle;
  fAlphaImage.MaskHandle := ImgMaskHandle;
  ScaledImage := CreateScaledImage(fNormalImage);
  Image1.Picture.Assign(ScaledImage);
  ScaledImage.free;
  ScaledImage := CreateScaledImage(fAlphaImage);
  Image2.Picture.Assign(ScaledImage);
  ScaledImage.free;
  RefreshDimensions();
End;

End.

