Unit uimage;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, ugraphics, upixeleditor_types;

Type

  { TImage }

  TImage = Class
  private
    fChanged: Boolean;
    fOpenGLImage: integer;
    fPixels: TPixelArea;
    Function getHeight: integer;
    Function getWidth: integer;
  public
    Filename: String;

    Property Changed: Boolean read fChanged;

    Property Height: integer read getHeight;
    Property Width: integer read getWidth;

    Constructor Create(); virtual;
    Destructor Destroy(); override;

    Function GetColorAt(x, y: integer): TRGBA;

    Procedure SetColorAt(x, y: integer; c: TRGBA);

    Procedure SetSize(aWidth, aHeight: Integer);
    Procedure Clear();

    Procedure Render();

    Procedure AppendToPEStream(Const Stream: TStream; Const aFilename: String);
    Procedure LoadFromPEStream(Const Stream: TStream; Const aFilename: String);

    Procedure ExportAsBMP(aFilename: String; TransparentColor: TRGBA);
    Procedure ImportFromBMP(aFilename: String; TransparentColor: TRGBA);

    Procedure ExportAsPNG(aFilename: String);
    Procedure ImportFromPNG(aFilename: String);

    Procedure Rescale(NewWidth, NewHeight: integer; Mode: TScaleMode);
  End;

Procedure FloodFill(SourceColor: TRGBA; aPos: TPoint; Toleranz: integer; Const Image: TImage; Callback: TPixelCallback);

Procedure RescalePixelArea(Var Data: TPixelArea; NewWidth, NewHeight: Integer; ScaleMode: TScaleMode);
Procedure RotatePixelAreaCounterClockwise90(Var Data: TPixelArea);
Procedure UpsideDownPixelArea(Var Data: TPixelArea);
Procedure LeftRightPixelArea(Var Data: TPixelArea);

Implementation

Uses
  IntfGraphics, fpImage, Graphics, LCLType, math
  , dglOpenGL, uopengl_graphikengine
  , uvectormath
  ;

Procedure FloodFill(SourceColor: TRGBA; aPos: TPoint;
  Toleranz: integer; Const Image: TImage;
  Callback: TPixelCallback);
Var
  Visited: Array Of Array Of Boolean;

  Procedure Visit(x, y: integer);
  Begin
    If (x < 0) Or (x >= Image.Width) Or
      (y < 0) Or (y >= image.Height) Or
      (Visited[x, y]) Then exit;
    Visited[x, y] := true;
    If ColorMatch(SourceColor, image.GetColorAt(x, y), Toleranz) Then Begin
      Callback(x, y);
      Visit(x + 1, y);
      Visit(x - 1, y);
      visit(x, y - 1);
      visit(x, y + 1);
    End;
  End;
Var
  i, j: Integer;
Begin
  Visited := Nil;
  setlength(Visited, Image.Width, Image.Height);
  For i := 0 To Image.Width - 1 Do Begin
    For j := 0 To Image.Height - 1 Do Begin
      Visited[i, j] := false;
    End;
  End;
  Visit(aPos.X, aPos.y);
End;

Function InterpolateLinear(c1, c2: TRGBA; f: Single): TRGBA;
Var
  r, g, b, r1, r2, g1, g2, b1, b2: integer;
Begin
  // Ist eine der beiden Farben Unsichtbar dominiert die andere
  If c1.a = 255 Then Begin
    result.r := c2.r Div 2;
    result.g := c2.g Div 2;
    result.b := c2.b Div 2;
    result.a := c2.a;
    exit;
  End;
  If c2.a = 255 Then Begin
    result.r := c1.r Div 2;
    result.g := c1.g Div 2;
    result.b := c1.b Div 2;
    result.a := c1.a;
    exit;
  End;
  r1 := c1.R;
  r2 := c2.R;
  g1 := c1.G;
  g2 := c2.G;
  b1 := c1.B;
  b2 := c2.B;

  r := round(r2 * f + r1 * (1 - f));
  g := round(g2 * f + g1 * (1 - f));
  b := round(b2 * f + b1 * (1 - f));

  r := Clamp(r, 0, 255);
  g := Clamp(g, 0, 255);
  b := Clamp(b, 0, 255);

  result.R := r;
  result.G := g;
  result.B := b;
  result.A := 0;
End;

Procedure RescalePixelArea(Var Data: TPixelArea; NewWidth, NewHeight: Integer;
  ScaleMode: TScaleMode);
Var
  tmp: TPixelArea;
  w, h: integer;

  Function GetPixel(x, y: Single): TRGBA;
  Var
    xi, yi, i: Integer;
    fx, fy: Single;
    p: Array[0..15] Of TPoint;
    a, c: Array[0..3] Of TRGBA;
  Begin
    xi := trunc(x);
    yi := trunc(y);
    fx := x - xi;
    fy := y - yi;
    p[0] := point(xi, yi);
    p[1] := point(min(w, xi + 1), yi);
    p[2] := point(xi, min(h, yi + 1));
    p[3] := point(min(w, xi + 1), min(h, yi + 1));
    For i := 0 To 3 Do Begin
      c[i] := tmp[p[i].x, p[i].y];
    End;
    If ScaleMode = smScale Then Begin
      If fx <= 0.5 Then Begin
        If fy <= 0.5 Then Begin
          result := c[0];
        End
        Else Begin
          result := c[2];
        End;
      End
      Else Begin
        If fy <= 0.5 Then Begin
          result := c[1];
        End
        Else Begin
          result := c[3];
        End;
      End;
    End
    Else Begin
      a[0] := InterpolateLinear(c[0], c[2], fy);
      a[1] := InterpolateLinear(c[1], c[3], fy);
      result := InterpolateLinear(a[0], a[1], fx);
    End;
  End;

Var
  i, j: Integer;
Begin
  tmp := data;
  w := high(data) - 0;
  h := high(data[0]) - 0;
  data := Nil;
  setlength(data, NewWidth, NewHeight);
  For i := 0 To NewWidth - 1 Do Begin
    For j := 0 To NewHeight - 1 Do Begin
      Case ScaleMode Of
        smResize: Begin
            If (i <= high(tmp)) And (j <= high(tmp[0])) Then Begin
              data[i, j] := tmp[i, j];
            End
            Else Begin
              data[i, j] := ColorTransparent;
            End;
          End;
        smScale, // Entpsricht einer Neares Neighbour Interpolation
        smSmoothScale: Begin // Entspricht einer Billinearen Interpolation
            data[i, j] := getpixel(i * w / (NewWidth - 1), j * h / (NewHeight - 1));
          End;
      End;
    End;
  End;
End;

Procedure RotatePixelAreaCounterClockwise90(Var Data: TPixelArea);
Var
  tmp: TPixelArea;
  i, j: Integer;
Begin
  tmp := data;
  data := Nil;
  setlength(data, length(tmp[0]), length(tmp));
  For i := 0 To high(tmp) Do Begin
    For j := 0 To high(tmp[0]) Do Begin
      data[j, i] := tmp[high(tmp) - i, j];
    End;
  End;
End;

Procedure UpsideDownPixelArea(Var Data: TPixelArea);
Var
  i, j: Integer;
  c: TRGBA;
Begin
  For i := 0 To high(Data) Do Begin
    For j := 0 To high(Data[0]) Div 2 Do Begin
      c := data[i, j];
      data[i, j] := data[i, high(data[0]) - j];
      data[i, high(data[0]) - j] := c;
    End;
  End;
End;

Procedure LeftRightPixelArea(Var Data: TPixelArea);
Var
  i, j: Integer;
  c: TRGBA;
Begin
  For i := 0 To high(Data) Div 2 Do Begin
    For j := 0 To high(Data[0]) Do Begin
      c := data[i, j];
      data[i, j] := data[high(data) - i, j];
      data[high(data) - i, j] := c;
    End;
  End;
End;

{ TImage }

Function TImage.getHeight: integer;
Begin
  If assigned(fPixels) Then Begin
    result := length(fPixels[0]);
  End
  Else Begin
    result := 0;
  End;
End;

Function TImage.getWidth: integer;
Begin
  result := length(fPixels);
End;

Constructor TImage.Create;
Begin
  Inherited Create;
  setlength(fPixels, 0, 0);
  fOpenGLImage := 0;
  fChanged := false;
  Clear();
End;

Destructor TImage.Destroy;
Begin
  setlength(fPixels, 0, 0);
  If fOpenGLImage <> 0 Then Begin
    glDeleteTextures(1, @fOpenGLImage);
    fOpenGLImage := 0;
  End;
End;

Function TImage.GetColorAt(x, y: integer): TRGBA;
Begin
  result := RGBA(0, 0, 0, 255);
  If (x < 0) Or (x >= Width) Or (y < 0) Or (y >= Height) Then exit;
  result := fPixels[x, y];
End;

Procedure TImage.SetColorAt(x, y: integer; c: TRGBA);
Var
  oc: Array[0..3] Of Byte;
Begin
  // Übernehmen in die Interne Struktur
  If (x < 0) Or (x >= Width) Or (y < 0) Or (y >= Height) Then exit;
  If fPixels[x, y] <> c Then Begin
    fChanged := true;
    fPixels[x, y] := c;
    // Übernehmen nach OpenGL
    oc[0] := c.r;
    oc[1] := c.g;
    oc[2] := c.b;
    oc[3] := c.a;
    glEnable(GL_TEXTURE_2D);
    glBindTexture(gl_texture_2d, fOpenGLImage);
    glTexSubImage2D(GL_TEXTURE_2D, 0, x, y, 1, 1, GL_RGBA, GL_UNSIGNED_BYTE, @oc[0]);
  End;
End;

Procedure TImage.SetSize(aWidth, aHeight: Integer);
Var
  data: Array Of Byte;
  i, j: Integer;
Begin
  Clear();
  // 2. Neu erstellen
  setlength(fPixels, aWidth, aHeight);
  glGenTextures(1, @fOpenGLImage);
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, fOpenGLImage);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  data := Nil;
  setlength(Data, aWidth * aHeight * 4);
  For i := 0 To high(data) Do Begin
    If (i + 1) Mod 4 = 0 Then Begin
      data[i] := 255; // Alpha Kanal auf Durchsichtig setzten
    End
    Else Begin
      data[i] := 0; // Die Farbe ist erst mal egal -> Schwarz
    End;
  End;
  glTexImage2D(GL_TEXTURE_2D, 0, gl_RGBA, aWidth, Aheight, 0, GL_RGBA, GL_UNSIGNED_BYTE, @Data[0]);
  For i := 0 To high(fPixels) Do Begin
    For j := 0 To high(fPixels[i]) Do Begin
      fPixels[i, j] := ColorTransparent;
    End;
  End;
End;

Procedure TImage.Clear;
Var
  i, j: Integer;
Begin
  For i := 0 To high(fPixels) Do Begin
    For j := 0 To high(fPixels[i]) Do Begin
      fPixels[i, j] := ColorTransparent;
    End;
    SetLength(fPixels[i], 0);
  End;
  SetLength(fPixels, 0);
  If fOpenGLImage <> 0 Then Begin
    glDeleteTextures(1, @fOpenGLImage);
    fOpenGLImage := 0;
  End;
  fChanged := false;
  Filename := '';
End;

Procedure TImage.Render;
Var
  b: {$IFDEF USE_GL}Byte{$ELSE}Boolean{$ENDIF};
  w, h: integer;
Begin
  If fOpenGLImage = 0 Then exit;
  w := length(fPixels);
  h := length(fPixels[0]);
  glColor4f(1, 1, 1, 1);
  B := glIsEnabled(gl_Blend);
  If Not (b{$IFDEF USE_GL} = 1{$ENDIF}) Then
    glenable(gl_Blend);
  glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);
  glBindTexture(gl_texture_2d, fOpenGLImage);
  glbegin(gl_quads);
  glTexCoord2f(0, 0);
  glvertex3f(0, 0, 0);
  glTexCoord2f(1, 0);
  glvertex3f(w, 0, 0);
  glTexCoord2f(1, 1);
  glvertex3f(w, h, 0);
  glTexCoord2f(0, 1);
  glvertex3f(0, h, 0);
  glend;
  If Not (b{$IFDEF USE_GL} = 1{$ENDIF}) Then
    gldisable(gl_blend);
End;

Procedure TImage.AppendToPEStream(Const Stream: TStream; Const aFilename: String
  );
Var
  i, j: integer;
Begin
  i := Width;
  stream.Write(i, SizeOf(i));
  j := Height;
  stream.Write(j, SizeOf(j));
  For j := 0 To Height - 1 Do Begin
    For i := 0 To Width - 1 Do Begin
      stream.Write(fPixels[i, j], sizeof(fPixels[i, j]));
    End;
  End;
  fChanged := false;
  Filename := aFilename;
End;

Procedure TImage.LoadFromPEStream(Const Stream: TStream;
  Const aFilename: String);
Var
  i, j: integer;
  c: TRGBA;
Begin
  i := -1;
  j := -1;
  stream.Read(i, SizeOf(i));
  stream.Read(j, SizeOf(j));
  SetSize(i, j);
  For j := 0 To Height - 1 Do Begin
    For i := 0 To Width - 1 Do Begin
      stream.Read(fPixels[i, j], sizeof(fPixels[i, j]));
      // Sieht unsinnig aus, aber initialisiert das OpenGL Bild ;)
      c := GetColorAt(i, j);
      If c.r = 255 Then Begin
        fPixels[i, j].r := 0;
      End
      Else Begin
        fPixels[i, j].r := 255;
      End;
      SetColorAt(i, j, c);
    End;
  End;
  fChanged := false;
  Filename := aFilename;
End;

Procedure TImage.ExportAsBMP(aFilename: String; TransparentColor: TRGBA);
Var
  b: Tbitmap;
  TempIntfImg: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
  j, i: Integer;
  c: TRGBA;
Begin
  b := TBitmap.Create;
  b.Width := Width;
  b.Height := Height;
  TempIntfImg := TLazIntfImage.Create(0, 0);
  TempIntfImg.LoadFromBitmap(b.Handle, b.MaskHandle);
  For j := 0 To height - 1 Do Begin
    For i := 0 To Width - 1 Do Begin
      c := GetColorAt(i, j);
      If c = ColorTransparent Then Begin
        TempIntfImg.Colors[i, j] := RGBAToFPColor(TransparentColor);
      End
      Else Begin
        TempIntfImg.Colors[i, j] := RGBAToFPColor(c);
      End;
    End;
  End;
  TempIntfImg.CreateBitmaps(ImgHandle, ImgMaskHandle, false);
  b.Handle := ImgHandle;
  b.MaskHandle := ImgMaskHandle;
  TempIntfImg.free;
  b.SaveToFile(aFilename);
  b.free;
  fChanged := false;
  Filename := aFilename;
End;

Procedure TImage.ImportFromBMP(aFilename: String; TransparentColor: TRGBA);
Var
  b: TBitmap;
  i, j: Integer;
  TempIntfImg: TLazIntfImage;
  c: TRGBA;
Begin
  b := TBitmap.Create;
  b.LoadFromFile(aFilename);
  SetSize(b.Width, b.Height);
  TempIntfImg := TLazIntfImage.Create(0, 0);
  TempIntfImg.LoadFromBitmap(b.Handle, b.MaskHandle);
  For j := 0 To height - 1 Do Begin
    For i := 0 To Width - 1 Do Begin
      c := FPColorToRGBA(TempIntfImg.Colors[i, j]);
      c.a := 0;
      If c = TransparentColor Then Begin
        SetColorAt(i, j, ColorTransparent);
      End
      Else Begin
        SetColorAt(i, j, c);
      End;
    End;
  End;
  TempIntfImg.free;
  b.free;
  fChanged := false;
  Filename := aFilename;
End;

Procedure TImage.ExportAsPNG(aFilename: String);
Var
  png: TPortableNetworkGraphic;
  b: Tbitmap;
  TempIntfImg: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
  j, i: Integer;
  c: TRGBA;
Begin
  b := TBitmap.Create;
  b.Width := Width;
  b.Height := Height;
  b.PixelFormat := pf32bit;
  TempIntfImg := TLazIntfImage.Create(0, 0);
  TempIntfImg.LoadFromBitmap(b.Handle, b.MaskHandle);
  For j := 0 To height - 1 Do Begin
    For i := 0 To Width - 1 Do Begin
      c := GetColorAt(i, j);
      c.a := 255 - c.a;
      TempIntfImg.Colors[i, j] := RGBAToFPColor(c);
    End;
  End;
  TempIntfImg.CreateBitmaps(ImgHandle, ImgMaskHandle, false);
  b.Handle := ImgHandle;
  b.MaskHandle := ImgMaskHandle;
  TempIntfImg.free;
  png := TPortableNetworkGraphic.Create;
  png.Assign(b);
  png.SaveToFile(aFilename);
  png.free;
  b.free;
  fChanged := false;
  Filename := aFilename;
End;

Procedure TImage.ImportFromPNG(aFilename: String);
Var
  png: TPortableNetworkGraphic;
  b: TBitmap;
  i, j: Integer;
  TempIntfImg: TLazIntfImage;
  c: TRGBA;
Begin
  b := TBitmap.Create;
  png := TPortableNetworkGraphic.Create;
  png.LoadFromFile(aFilename);
  b.Assign(png);
  png.free;
  SetSize(b.Width, b.Height);
  TempIntfImg := TLazIntfImage.Create(0, 0);
  TempIntfImg.LoadFromBitmap(b.Handle, b.MaskHandle);
  For j := 0 To height - 1 Do Begin
    For i := 0 To Width - 1 Do Begin
      c := FPColorToRGBA(TempIntfImg.Colors[i, j]);
      c.a := 255 - c.a;
      SetColorAt(i, j, c);
    End;
  End;
  TempIntfImg.free;
  b.free;
  fChanged := false;
  Filename := aFilename;
End;

Procedure TImage.Rescale(NewWidth, NewHeight: integer; Mode: TScaleMode);
Var
  a: TPixelArea;
  i, j: Integer;
Begin
  If (NewWidth = Width) And (NewHeight = Height) Then exit; // Alles bereits bestens
  // Am Einfachsten ist es sich alles zu Bakupen und dann das Bild neu zu erstellen
  a := Nil;
  setlength(a, Width, Height);
  For i := 0 To Width - 1 Do Begin
    For j := 0 To Height - 1 Do Begin
      a[i, j] := fPixels[i, j];
    End;
  End;
  RescalePixelArea(a, NewWidth, NewHeight, Mode);
  SetSize(NewWidth, NewHeight);
  For i := 0 To Width - 1 Do Begin
    For j := 0 To Height - 1 Do Begin
      SetColorAt(i, j, a[i, j]);
    End;
  End;
End;

End.

