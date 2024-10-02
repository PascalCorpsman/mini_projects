Unit uimage;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, ugraphics;

Type

  TLayer = (lBackground, lMiddle, lForeground, lAll); // !! Achtung !!, die Reihenfolge darf nicht verändert werden

  { TImage }

  TImage = Class
  private
    fOpenGLImage: integer;
    fPixels: Array Of Array Of Array[lBackground..lForeground] Of TRGBA;
    Function getHeight: integer;
    Function getWidth: integer;
  public

    Property Height: integer read getHeight;
    Property Width: integer read getWidth;

    Constructor Create(); virtual;
    Destructor Destroy(); override;

    Function GetColorAt(x, y: integer; aLayer: TLayer): TRGBA;

    Procedure SetColorAt(x, y: integer; aLayer: TLayer; c: TRGBA);

    Procedure SetSize(aWidth, aHeight: Integer);
    Procedure Clear(aLayer: TLayer);

    Procedure Render();
  End;

Const
  Transparent: TRGBA = (r: 0; g: 0; b: 0; a: 255);

Implementation

Uses dglOpenGL, uopengl_graphikengine;

{ TImage }

Function TImage.getHeight: integer;
Begin
  result := length(fPixels[0]);
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
End;

Destructor TImage.Destroy;
Begin
  setlength(fPixels, 0, 0);
  If fOpenGLImage <> 0 Then Begin
    glDeleteTextures(1, @fOpenGLImage);
    fOpenGLImage := 0;
  End;
End;

Function TImage.GetColorAt(x, y: integer; aLayer: TLayer): TRGBA;
Var
  i: TLayer;
Begin
  result := RGBA(0, 0, 0, 255);
  If (x < 0) Or (x >= Width) Or (y < 0) Or (y >= Height) Then exit;
  If Not (aLayer In [lForeground, lMiddle, lBackground]) Then exit;
  result := fPixels[x, y][lBackground];
  For i := aLayer Downto lBackground Do Begin
    If fPixels[x, y][i].a = 0 Then Begin // TODO: Will man je irgendwann mal dass die Layer geblendet werden können, muss man hier ran !
      result := fPixels[x, y][i];
      break;
    End;
  End;
End;

Procedure TImage.SetColorAt(x, y: integer; aLayer: TLayer; c: TRGBA);
Var
  oc: Array[0..3] Of Byte;
Begin
  // Übernehmen in die Interne Struktur
  If (x < 0) Or (x >= Width) Or (y < 0) Or (y >= Height) Then exit;
  If Not (aLayer In [lForeground, lMiddle, lBackground]) Then exit;
  If fPixels[x, y][aLayer] <> c Then Begin
    fPixels[x, y][aLayer] := c;
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
  i: Integer;
Begin
  setlength(fPixels, aWidth, aHeight);
  // Der OpenGL part
  // 1. ggf aufräumen
  If fOpenGLImage <> 0 Then Begin
    glDeleteTextures(1, @fOpenGLImage);
    fOpenGLImage := 0;
  End;
  // 2. Neu erstellen
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
  Clear(lAll);
End;

Procedure TImage.Clear(aLayer: TLayer);
Var
  i, j: Integer;
Begin
  For i := 0 To high(fPixels) Do Begin
    For j := 0 To high(fPixels[i]) Do Begin
      If aLayer = lAll Then Begin
        fPixels[i, j][lBackground] := Transparent;
        fPixels[i, j][lMiddle] := Transparent;
        fPixels[i, j][lForeground] := Transparent;
      End
      Else Begin
        fPixels[i, j][aLayer] := Transparent;
      End;
    End;
  End;
End;

Procedure TImage.Render;
Var
  b: {$IFDEF USE_GL}Byte{$ELSE}Boolean{$ENDIF};
  w, h: integer;
Begin
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

End.

