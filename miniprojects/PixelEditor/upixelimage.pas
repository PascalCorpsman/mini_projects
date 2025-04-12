(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of PixelEditor                                           *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit upixelimage;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Graphics, ugraphics, upixeleditor_types;

Type

  TUpdate = Record
    Counter: Integer; // 0 = Deaktiviert, > 0 = Schachtelungstiefe der BeginUpdate Aufrufe
    tl, br: TPoint;
  End;

  { TPixelImage }

  TPixelImage = Class
  private
    fChanged: Boolean;
    fOpenGLImage: integer;
    fPixels: TPixelArea;
    fMonochronPixels: TPixelArea;
    fMonochronOpenGLImage: integer;
    fUpdate: TUpdate;
    Function getHeight: integer;
    Function getWidth: integer;
  public
    Filename: String;

    Property PixelData: TPixelArea read fPixels;

    Property Changed: Boolean read fChanged;

    Property Height: integer read getHeight;
    Property Width: integer read getWidth;

    Constructor Create(); virtual;
    Destructor Destroy(); override;

    Function GetColorAt(x, y: integer): TRGBA;

    Procedure BeginUpdate;
    Procedure SetColorAt(x, y: integer; c: TRGBA);
    Procedure EndUpdate;

    Procedure SetSize(aWidth, aHeight: Integer);
    Procedure Clear();

    Procedure Render(Monochrone: Boolean);

    Procedure AppendToPEStream(Const Stream: TStream; Const aFilename: String);
    Procedure LoadFromPEStream(Const Stream: TStream; Const aFilename: String);

    Function AsBMP(TransparentColor: TRGBA): TBitmap;
    Procedure ExportAsBMP(aFilename: String; TransparentColor: TRGBA);
    Procedure ImportFromBMP(Const Bitmap: TBitmap; aFilename: String; TransparentColor: TRGBA);

    Procedure ExportAsPNG(aFilename: String);
    Procedure ImportFromPNG(aFilename: String);

    Procedure Rescale(NewWidth, NewHeight: integer; Mode: TScaleMode);
    Procedure UpsideDown;
    Procedure RotateCounterClockwise90;
    Procedure LeftRight;
    Procedure Rotate(Angle: Single; ScaleMode: TScaleMode);
    Procedure FloodFill(SourceColor: TRGBA; aPos: TPoint; Toleranz: integer; Callback: TPixelCallback);
  End;

Implementation

Uses
  FPWritePNG, IntfGraphics, LCLType, math
  , dglOpenGL, uopengl_graphikengine
  , uvectormath
  , ufifo
  ;

Procedure TPixelImage.FloodFill(SourceColor: TRGBA; aPos: TPoint; Toleranz: integer;
  Callback: TPixelCallback);

Type
  TPointFifo = specialize TBufferedFifo < TPoint > ;
Var
  Visited: Array Of Array Of Boolean;
  Fifo: TPointFifo;
Var
  i, j: Integer;
  p: TPoint;
Begin
  BeginUpdate;
  Visited := Nil;
  Fifo := TPointFifo.create(Width * Height);
  setlength(Visited, Width, Height);
  For i := 0 To Width - 1 Do Begin
    For j := 0 To Height - 1 Do Begin
      Visited[i, j] := false;
    End;
  End;
  fifo.Push(point(aPos.X, aPos.y));
  While Not fifo.isempty Do Begin
    p := Fifo.Pop;
    If (p.x >= 0) And (p.x < Width) And
      (p.y >= 0) And (p.y < Height) And
      (Not Visited[p.x, p.y]) Then Begin
      Visited[p.x, p.y] := true;
      If ColorMatch(SourceColor, GetColorAt(p.x, p.y), Toleranz) Then Begin
        Callback(p.x, p.y);
        fifo.Push(point(p.x + 1, p.y));
        fifo.Push(point(p.x - 1, p.y));
        fifo.Push(point(p.x, p.y - 1));
        fifo.Push(point(p.x, p.y + 1));
      End;
    End;
  End;
  fifo.free;
  EndUpdate;
End;

Function InterpolateLinear(c1, c2: TRGBA; f: Single): TRGBA; // TODO: in TPixelImage integrieren
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

Function GetPixel(Const Source: TPixelArea; x, y: Single; ScaleMode: TScaleMode): TRGBA; // TODO: in TPixelImage integrieren
Var
  xi, yi, i: Integer;
  fx, fy: Single;
  p: Array[0..3] Of TPoint;
  a, c: Array[0..3] Of TRGBA;
Begin
  xi := trunc(x);
  yi := trunc(y);
  fx := x - xi;
  fy := y - yi;
  p[0] := point(xi, yi);
  p[1] := point(min(high(source), xi + 1), yi);
  p[2] := point(xi, min(high(source[0]), yi + 1));
  p[3] := point(min(high(source), xi + 1), min(high(source[0]), yi + 1));
  For i := 0 To 3 Do Begin
    c[i] := Source[p[i].x, p[i].y];
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

Procedure RescalePixelArea(Var Data: TPixelArea; NewWidth, NewHeight: Integer;
  ScaleMode: TScaleMode); // TODO: in TPixelImage integrieren
Var
  tmp: TPixelArea;
  w, h: integer;
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
            data[i, j] := getpixel(tmp, i * w / (NewWidth - 1), j * h / (NewHeight - 1), ScaleMode);
          End;
      End;
    End;
  End;
End;

Procedure RotatePixelAreaCounterClockwise90(Var Data: TPixelArea); // TODO: in TPixelImage integrieren
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

Procedure LeftRightPixelArea(Var Data: TPixelArea); // TODO: in TPixelImage integrieren
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

Procedure RotatePixelArea(Var Data: TPixelArea; Angle: Single;
  ScaleMode: TScaleMode); // TODO: in TPixelImage integrieren
Var
  im3, m3: TMatrix3x3;

  Function Transform(x, y: Single): TVector2;
  Var
    tmp: TVector3;
  Begin
    tmp := v3(x, y, 1);
    tmp := m3 * tmp;
    result := v2(tmp.x, tmp.y);
  End;

  Function iTransform(x, y: Single): TVector2;
  Var
    tmp: TVector3;
  Begin
    tmp := v3(x, y, 1);
    tmp := im3 * tmp;
    result := v2(tmp.x, tmp.y);
  End;
Var
  source: TPixelArea;
  m2: TMatrix2x2;
  v, mi, ma: TVector2;
  ti, tj, w, h, i, j: Integer;
  c: TRGBA;
Begin
  // Bestimmen der Dimension der ZielDaten
  If ScaleMode = smResize Then ScaleMode := smScale;
  m2 := CalculateRotationMatrix(Angle);
  m3 := IdentityMatrix3x3;
  For i := 0 To 1 Do Begin
    For j := 0 To 1 Do Begin
      m3[i, j] := m2[i, j];
    End;
  End;
  source := Data;
  w := length(source);
  h := length(source[0]);
  mi := Transform(0, 0);
  ma := mi;
  mi := minv2(mi, Transform(W, 0));
  ma := maxv2(ma, Transform(W, 0));
  mi := minv2(mi, Transform(W, H));
  ma := maxv2(ma, Transform(W, H));
  mi := minv2(mi, Transform(0, H));
  ma := maxv2(ma, Transform(0, H));
  v := (ma - mi);
  data := Nil;
  setlength(data, round(v.x), round(v.y));
  w := length(data);
  h := length(data[0]);
  iM3 := InvertMatrix2(M3);
  If iM3.Equal(Zero3x3()) Then Begin
    (*
     * Kommt diese AV, dann muss das halt auch noch implementiert werden !
     *)
    Raise exception.create('RotatePixelArea, matrix not invertable.');
  End
  Else Begin
    For i := trunc(mi.x) To ceil(ma.x) Do Begin
      For j := trunc(mi.y) To ceil(ma.y) Do Begin
        v := iTransform(i, j);
        v := v - v2(0.5, 0.5); // Die Pixelmitte von Links Oben nach Mitte verschieben
        If (v.x >= 0) And (v.x < length(source)) And
          (v.y >= 0) And (v.y < length(source[0])) Then Begin
          c := GetPixel(Source, v.x, v.y, ScaleMode);
        End
        Else Begin
          c := ColorTransparent;
        End;
        ti := trunc(i - mi.x);
        tj := trunc(j - mi.y);
        If (ti >= 0) And (ti < w) And
          (tj >= 0) And (tj < h) Then Begin
          data[ti, tj] := c;
        End;
      End;
    End;
  End;
End;

{ TPixelImage }

Function TPixelImage.getHeight: integer;
Begin
  If assigned(fPixels) Then Begin
    result := length(fPixels[0]);
  End
  Else Begin
    result := 0;
  End;
End;

Function TPixelImage.getWidth: integer;
Begin
  result := length(fPixels);
End;

Constructor TPixelImage.Create;
Begin
  Inherited Create;
  setlength(fPixels, 0, 0);
  setlength(fMonochronPixels, 0, 0);
  fOpenGLImage := 0;
  fMonochronOpenGLImage := 0;
  fChanged := false;
  Clear();
End;

Destructor TPixelImage.Destroy;
Begin
  setlength(fPixels, 0, 0);
  setlength(fMonochronPixels, 0, 0);
  If fOpenGLImage <> 0 Then Begin
    glDeleteTextures(1, @fOpenGLImage);
    fOpenGLImage := 0;
  End;
  If fMonochronOpenGLImage <> 0 Then Begin
    glDeleteTextures(1, @fMonochronOpenGLImage);
    fMonochronOpenGLImage := 0;
  End;
End;

Function TPixelImage.GetColorAt(x, y: integer): TRGBA;
Begin
  result := ColorTransparent;
  If (x < 0) Or (x >= Width) Or (y < 0) Or (y >= Height) Then exit;
  result := fPixels[x, y];
End;

Procedure TPixelImage.BeginUpdate;
Begin
  inc(fUpdate.Counter);
  If fUpdate.Counter = 1 Then Begin
    fUpdate.tl := Point(Width + 1, Height + 1);
    fUpdate.br := point(-1, -1);
  End;
End;

Procedure TPixelImage.EndUpdate;
Var
  c, x, y, w, h, i, j: integer;
  l: byte;
  Data, Data2: Array Of Array[0..3] Of Byte;
Begin
  fUpdate.Counter := max(fUpdate.Counter - 1, 0);
  If (fUpdate.Counter = 0) And (fUpdate.br.X <> -1) Then Begin // Es gab tatsächlich was zum Updaten
    x := fUpdate.tl.x;
    y := fUpdate.tl.Y;
    w := fUpdate.br.x - fUpdate.tl.x + 1;
    h := fUpdate.br.Y - fUpdate.tl.y + 1;
    c := 0;
    data := Nil;
    setlength(data, w * h);
    data2 := Nil;
    setlength(data2, w * h);
    For j := 0 To h - 1 Do Begin
      For i := 0 To w - 1 Do Begin
        data[c][0] := fPixels[x + i, y + j].r;
        data[c][1] := fPixels[x + i, y + j].g;
        data[c][2] := fPixels[x + i, y + j].b;
        data[c][3] := fPixels[x + i, y + j].a;
        l := RGBAtoLuminanz(fPixels[x + i, y + j]);
        data2[c][0] := l;
        data2[c][1] := l;
        data2[c][2] := l;
        data2[c][3] := fPixels[x + i, y + j].a;
        inc(c);
      End;
    End;
    glEnable(GL_TEXTURE_2D);
    glBindTexture(gl_texture_2d, fOpenGLImage);
    glTexSubImage2D(GL_TEXTURE_2D, 0, x, y, w, h, GL_RGBA, GL_UNSIGNED_BYTE, @Data[0]);
    glBindTexture(gl_texture_2d, fMonochronOpenGLImage);
    glTexSubImage2D(GL_TEXTURE_2D, 0, x, y, w, h, GL_RGBA, GL_UNSIGNED_BYTE, @Data2[0]);
    fUpdate.br.X := -1;
  End;
End;

Procedure TPixelImage.SetColorAt(x, y: integer; c: TRGBA);
Var
  oc, oc2: Array[0..3] Of Byte;
Begin
  // Übernehmen in die Interne Struktur
  If (x < 0) Or (x >= Width) Or (y < 0) Or (y >= Height) Then exit;
  If fPixels[x, y] <> c Then Begin
    fChanged := true;
    fPixels[x, y] := c;
    fMonochronPixels[x, y].r := RGBAtoLuminanz(c);
    fMonochronPixels[x, y].g := fMonochronPixels[x, y].r;
    fMonochronPixels[x, y].b := fMonochronPixels[x, y].r;
    fMonochronPixels[x, y].a := c.a;
    If fUpdate.Counter = 0 Then Begin
      // Übernehmen nach OpenGL
      oc[0] := c.r;
      oc[1] := c.g;
      oc[2] := c.b;
      oc[3] := c.a;
      oc2[0] := fMonochronPixels[x, y].r;
      oc2[1] := fMonochronPixels[x, y].r;
      oc2[2] := fMonochronPixels[x, y].r;
      oc2[3] := c.a;
      glEnable(GL_TEXTURE_2D);
      glBindTexture(gl_texture_2d, fOpenGLImage);
      glTexSubImage2D(GL_TEXTURE_2D, 0, x, y, 1, 1, GL_RGBA, GL_UNSIGNED_BYTE, @oc[0]);
      glBindTexture(gl_texture_2d, fMonochronOpenGLImage);
      glTexSubImage2D(GL_TEXTURE_2D, 0, x, y, 1, 1, GL_RGBA, GL_UNSIGNED_BYTE, @oc2[0]);
    End
    Else Begin
      fUpdate.tl.x := min(fUpdate.tl.x, x);
      fUpdate.tl.Y := min(fUpdate.tl.Y, y);
      fUpdate.br.x := max(fUpdate.br.x, x);
      fUpdate.br.Y := max(fUpdate.br.Y, y);
    End;
  End;
End;

Procedure TPixelImage.SetSize(aWidth, aHeight: Integer);
Var
  data: Array Of Byte;
  i, j: Integer;
Begin
  Clear();
  // 2. Neu erstellen
  setlength(fPixels, aWidth, aHeight);
  setlength(fMonochronPixels, aWidth, aHeight);
  glGenTextures(1, @fOpenGLImage);
  glGenTextures(1, @fMonochronOpenGLImage);
  glEnable(GL_TEXTURE_2D);
  glBindTexture(GL_TEXTURE_2D, fOpenGLImage);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glBindTexture(GL_TEXTURE_2D, fMonochronOpenGLImage);
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
  glBindTexture(GL_TEXTURE_2D, fOpenGLImage);
  glTexImage2D(GL_TEXTURE_2D, 0, gl_RGBA, aWidth, Aheight, 0, GL_RGBA, GL_UNSIGNED_BYTE, @Data[0]);
  glBindTexture(GL_TEXTURE_2D, fMonochronOpenGLImage);
  glTexImage2D(GL_TEXTURE_2D, 0, gl_RGBA, aWidth, Aheight, 0, GL_RGBA, GL_UNSIGNED_BYTE, @Data[0]);
  For i := 0 To high(fPixels) Do Begin
    For j := 0 To high(fPixels[i]) Do Begin
      fPixels[i, j] := ColorTransparent;
      fMonochronPixels[i, j] := ColorTransparent;
    End;
  End;
End;

Procedure TPixelImage.Clear;
Var
  i, j: Integer;
Begin
  fUpdate.Counter := 0;
  For i := 0 To high(fPixels) Do Begin
    SetLength(fPixels[i], 0);
    SetLength(fMonochronPixels[i], 0);
  End;
  SetLength(fPixels, 0);
  SetLength(fMonochronPixels, 0);
  If fOpenGLImage <> 0 Then Begin
    glDeleteTextures(1, @fOpenGLImage);
    fOpenGLImage := 0;
  End;
  If fMonochronOpenGLImage <> 0 Then Begin
    glDeleteTextures(1, @fMonochronOpenGLImage);
    fMonochronOpenGLImage := 0;
  End;
  fChanged := false;
  Filename := '';
End;

Procedure TPixelImage.Render(Monochrone: Boolean);
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
  If Monochrone Then Begin
    glBindTexture(gl_texture_2d, fMonochronOpenGLImage);
  End
  Else Begin
    glBindTexture(gl_texture_2d, fOpenGLImage);
  End;
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

Procedure TPixelImage.AppendToPEStream(Const Stream: TStream; Const aFilename: String
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

Procedure TPixelImage.LoadFromPEStream(Const Stream: TStream; Const aFilename: String
  );
Var
  i, j: integer;
  c: TRGBA;
Begin
  i := -1;
  j := -1;
  stream.Read(i, SizeOf(i));
  stream.Read(j, SizeOf(j));
  SetSize(i, j);
  BeginUpdate;
  For j := 0 To Height - 1 Do Begin
    For i := 0 To Width - 1 Do Begin
      stream.Read(fPixels[i, j], sizeof(fPixels[i, j]));
      // Sieht unsinnig aus, aber initialisiert das OpenGL Bild und den Monochron Buffer ;)
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
  EndUpdate;
  fChanged := false;
  Filename := aFilename;
End;

Function TPixelImage.AsBMP(TransparentColor: TRGBA): TBitmap;
Var
  TempIntfImg: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
  j, i: Integer;
  c: TRGBA;
Begin
  result := TBitmap.Create;
  result.Width := Width;
  result.Height := Height;
  TempIntfImg := TLazIntfImage.Create(0, 0);
  TempIntfImg.LoadFromBitmap(result.Handle, result.MaskHandle);
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
  result.Handle := ImgHandle;
  result.MaskHandle := ImgMaskHandle;
  TempIntfImg.free;
End;

Procedure TPixelImage.ExportAsBMP(aFilename: String; TransparentColor: TRGBA);
Var
  b: Tbitmap;
Begin
  b := AsBMP(TransparentColor);
  b.SaveToFile(aFilename);
  b.free;
  fChanged := false;
  Filename := aFilename;
End;

Procedure TPixelImage.ImportFromBMP(Const Bitmap: TBitmap; aFilename: String;
  TransparentColor: TRGBA);
Var
  i, j: Integer;
  TempIntfImg: TLazIntfImage;
  c: TRGBA;
Begin
  SetSize(Bitmap.Width, Bitmap.Height);
  TempIntfImg := TLazIntfImage.Create(0, 0);
  TempIntfImg.LoadFromBitmap(Bitmap.Handle, Bitmap.MaskHandle);
  BeginUpdate;
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
  EndUpdate;
  TempIntfImg.free;
  fChanged := false;
  Filename := aFilename;
End;

Procedure TPixelImage.ExportAsPNG(aFilename: String);
Var
  b: Tbitmap;
  TempIntfImg: TLazIntfImage;
  j, i: Integer;
  c: TRGBA;
  writer: TFPWriterPNG;
Begin
  // 1. Convert to BMP
  b := TBitmap.Create;
  b.PixelFormat := pf32bit; // Wichtig für Alpha
  TempIntfImg := b.CreateIntfImage;
  TempIntfImg.SetSize(Width, Height);
  For j := 0 To height - 1 Do Begin
    For i := 0 To Width - 1 Do Begin
      c := GetColorAt(i, j);
      c.a := 255 - c.a;
      TempIntfImg.Colors[i, j] := RGBAToFPColor(c);
    End;
  End;
  writer := TFPWriterPNG.Create;
  writer.UseAlpha := true;
  writer.WordSized := false; // Reduce to 32-Bit (default 64-Bit)
  TempIntfImg.SaveToFile(aFilename, writer);
  writer.Free;
  TempIntfImg.Free;
  b.free;
  fChanged := false;
  Filename := aFilename;
End;

Procedure TPixelImage.ImportFromPNG(aFilename: String);
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
  BeginUpdate;
  For j := 0 To height - 1 Do Begin
    For i := 0 To Width - 1 Do Begin
      c := FPColorToRGBA(TempIntfImg.Colors[i, j]);
      c.a := 255 - c.a;
      SetColorAt(i, j, c);
    End;
  End;
  EndUpdate;
  TempIntfImg.free;
  b.free;
  fChanged := false;
  Filename := aFilename;
End;

Procedure TPixelImage.Rescale(NewWidth, NewHeight: integer; Mode: TScaleMode);
Var
  a: TPixelArea;
  i, j: Integer;
  fn: String;
Begin
  If (NewWidth = Width) And (NewHeight = Height) Then exit; // Alles bereits bestens
  fn := Filename;
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
  BeginUpdate;
  For i := 0 To Width - 1 Do Begin
    For j := 0 To Height - 1 Do Begin
      SetColorAt(i, j, a[i, j]);
    End;
  End;
  EndUpdate;
  Filename := fn;
End;

Procedure TPixelImage.UpsideDown;
Var
  a: TPixelArea;
  i, j: integer;
  c: TRGBA;
Begin
  a := Nil;
  setlength(a, width, height);
  For i := 0 To Width - 1 Do Begin
    For j := 0 To Height - 1 Do Begin
      a[i, j] := fPixels[i, j];
    End;
  End;
  For i := 0 To high(a) Do Begin
    For j := 0 To high(a[0]) Div 2 Do Begin
      c := a[i, j];
      a[i, j] := a[i, high(a[0]) - j];
      a[i, high(a[0]) - j] := c;
    End;
  End;
  BeginUpdate;
  For i := 0 To Width - 1 Do Begin
    For j := 0 To Height - 1 Do Begin
      SetColorAt(i, j, a[i, j]);
    End;
  End;
  EndUpdate;
  setlength(a, 0, 0);
End;

Procedure TPixelImage.RotateCounterClockwise90;
Var
  a: TPixelArea;
  i, j: integer;
  fn: String;
Begin
  a := Nil;
  fn := Filename;
  setlength(a, width, height);
  For i := 0 To Width - 1 Do Begin
    For j := 0 To Height - 1 Do Begin
      a[i, j] := fPixels[i, j];
    End;
  End;
  RotatePixelAreaCounterClockwise90(a);
  If Height <> Width Then Begin
    SetSize(Height, Width);
  End;
  BeginUpdate;
  For i := 0 To Width - 1 Do Begin
    For j := 0 To Height - 1 Do Begin
      SetColorAt(i, j, a[i, j]);
    End;
  End;
  EndUpdate;
  setlength(a, 0, 0);
  Filename := fn;
End;

Procedure TPixelImage.LeftRight;
Var
  a: TPixelArea;
  i, j: integer;
Begin
  a := Nil;
  setlength(a, width, height);
  For i := 0 To Width - 1 Do Begin
    For j := 0 To Height - 1 Do Begin
      a[i, j] := fPixels[i, j];
    End;
  End;
  LeftRightPixelArea(a);
  BeginUpdate;
  For i := 0 To Width - 1 Do Begin
    For j := 0 To Height - 1 Do Begin
      SetColorAt(i, j, a[i, j]);
    End;
  End;
  EndUpdate;
  setlength(a, 0, 0);
End;

Procedure TPixelImage.Rotate(Angle: Single; ScaleMode: TScaleMode);
Var
  a: TPixelArea;
  i, j: integer;
  fn: String;
Begin
  a := Nil;
  fn := Filename;
  setlength(a, width, height);
  For i := 0 To Width - 1 Do Begin
    For j := 0 To Height - 1 Do Begin
      a[i, j] := fPixels[i, j];
    End;
  End;
  RotatePixelArea(a, Angle, ScaleMode);
  If (length(a) <> Width) Or (Length(a[0]) <> Height) Then Begin
    SetSize(length(a), length(a[0]));
  End;
  BeginUpdate;
  For i := 0 To Width - 1 Do Begin
    For j := 0 To Height - 1 Do Begin
      SetColorAt(i, j, a[i, j]);
    End;
  End;
  EndUpdate;
  setlength(a, 0, 0);
  Filename := fn;
End;

End.

