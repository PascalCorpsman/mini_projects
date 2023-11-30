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
    Procedure FormCreate(Sender: TObject);
  private
    Procedure RefreshDimensions();
  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses
  IntfGraphics, fpImage, LCLType, ugraphics, GraphType;

{ TForm1 }

Procedure TForm1.Button1Click(Sender: TObject);
Var
  p: TPortableNetworkGraphic;
  b, b2: TBitmap;
  i, j: Integer;
  Source, Dest: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
  CurColor, DestColor: TFPColor;
Begin
  // Load PNG
  If OpenDialog1.Execute Then Begin
    p := TPortableNetworkGraphic.Create;
    p.LoadFromFile(OpenDialog1.FileName);
    b := TBitmap.Create;
    b.Assign(p);
    b2 := TBitmap.Create;
    b2.Width := b.Width;
    b2.Height := b.Height;
    Source := TLazIntfImage.Create(0, 0);
    Source.LoadFromBitmap(b.Handle, b.MaskHandle);
    Dest := TLazIntfImage.Create(0, 0);
    Dest.LoadFromBitmap(b2.Handle, b2.MaskHandle);
    For i := 0 To b.Width - 1 Do Begin
      For j := 0 To b.Height - 1 Do Begin
        CurColor := Source.Colors[i, j];
        DestColor.red := CurColor.alpha;
        DestColor.green := CurColor.alpha;
        DestColor.blue := CurColor.alpha;
        DestColor.alpha := 255 * 256;
        Dest.Colors[i, j] := DestColor;
        CurColor.alpha := 255 * 256;
        Source.Colors[i, j] := CurColor;
      End;
    End;
    Source.CreateBitmaps(ImgHandle, ImgMaskHandle, false);
    b.Handle := ImgHandle;
    b.MaskHandle := ImgMaskHandle;
    Dest.CreateBitmaps(ImgHandle, ImgMaskHandle, false);
    b2.Handle := ImgHandle;
    b2.MaskHandle := ImgMaskHandle;
    Image1.Picture.Assign(b);
    image2.Picture.Assign(b2);
    Source.Free;
    Dest.free;
    b.free;
    b2.free;
    RefreshDimensions();
  End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  // Load Normal
  If OpenDialog2.Execute Then Begin
    Image1.Picture.LoadFromFile(OpenDialog2.FileName);
    RefreshDimensions();
  End;
End;

Procedure TForm1.Button3Click(Sender: TObject);
  Procedure BinariseByCLFuchsia(Const Bitmap: TBitmap);
  Var
    TempIntfImg: TLazIntfImage;
    ImgHandle, ImgMaskHandle: HBitmap;
    fuchsia, CurColor: TFPColor;
    i, j: Integer;
  Begin
    //  Bitmap.pixelformat := pf24bit; -- Das Darf nicht drin sein, sonst sind evtl alle Werte 0 !!!
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
  b: TBitmap;
Begin
  // Load Alpha
  If OpenDialog2.Execute Then Begin
    Image2.Picture.LoadFromFile(OpenDialog2.FileName);
    b := TBitmap.Create;
    b.Assign(Image2.Picture.Bitmap);
    If CheckBox1.Checked Then Begin
      BinariseByCLFuchsia(b);
    End
    Else Begin
      ConvertToGrayscale(b);
    End;
    Image2.Picture.Assign(b);
    b.free;
    RefreshDimensions();
  End;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Var
  b: TBitmap;
Begin
  // Save Normal
  If SaveDialog1.Execute Then Begin
    b := TBitmap.Create;
    b.Assign(Image1.Picture.Bitmap);
    b.SaveToFile(SaveDialog1.FileName);
    b.free;
  End;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Var
  b: TBitmap;
Begin
  // Save Alpha
  If SaveDialog1.Execute Then Begin
    b := TBitmap.Create;
    b.Assign(Image2.Picture.Bitmap);
    b.SaveToFile(SaveDialog1.FileName);
    b.free;
  End;
End;

Procedure TForm1.Button6Click(Sender: TObject);
Var
  b, a, d: TBitmap;
  Source, Alpha, Dest: TLazIntfImage;
  SColor, AColor: TFPColor;
  ImgHandle, ImgMaskHandle: HBitmap;
  p: TPortableNetworkGraphic;
  i, j: Integer;
Begin
  // Save PNG
  If SaveDialog2.Execute Then Begin
    b := TBitmap.Create;
    b.Assign(Image1.Picture.Bitmap); // Das Quellbild ohne Alpha
    a := TBitmap.Create;
    a.Assign(Image2.Picture.Bitmap); // Der Alphakanal des Quellbildes
    If (a.Width <> b.Width) Or (a.Height <> b.Height) Then Begin
      ShowMessage('Error alphamask and normal image differ in size.');
      a.free;
      b.Free;
      exit;
    End;
    d := TBitmap.Create;
    d.Width := a.Width;
    d.Height := a.Height;

    source := TLazIntfImage.Create(0, 0);
    Source.LoadFromBitmap(b.Handle, b.MaskHandle);

    Alpha := TLazIntfImage.Create(0, 0);
    Alpha.LoadFromBitmap(a.Handle, a.MaskHandle);

    Dest := TLazIntfImage.Create(0, 0, [riqfRGB, riqfAlpha]);
    Dest.SetSize(a.Width, a.Height);

    For i := 0 To a.Width - 1 Do Begin
      For j := 0 To a.Height - 1 Do Begin
        SColor := Source.Colors[i, j];
        AColor := Alpha.Colors[i, j];
        SColor.alpha := AColor.red; // Egal ist ja eh Graustufen
        Dest.Colors[i, j] := SColor;
      End;
    End;

    Dest.CreateBitmaps(ImgHandle, ImgMaskHandle, false);
    d.Handle := ImgHandle;
    d.MaskHandle := ImgMaskHandle;

    p := TPortableNetworkGraphic.Create;
    p.Assign(d);
    p.SaveToFile(SaveDialog2.FileName);
    p.free;
    d.free;
    a.Free;
    b.Free;
    source.free;
    Alpha.free;
    Dest.free;
  End;
End;

Procedure TForm1.Button7Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  (*
   * Historie: 0.01 = Initialversion
   *           0.02 = Anzeige der Bild Dimensionen, wenn vorhanden
   *           0.03 = Checkbox beim Laden der Transparenz zum Binarisieren via clFuchsia
   *)
  Label2.Caption := '';
  caption := 'PNG-Editor ver. 0.03 by Corpsman';
  Application.Title := caption;
End;

Procedure TForm1.RefreshDimensions();
Begin
  label2.caption := format('Image: %d x %d' + LineEnding + 'Alpha: %d x %d', [Image1.Picture.Width, Image1.Picture.Height, Image2.Picture.Width, Image2.Picture.Height]);
End;

End.

