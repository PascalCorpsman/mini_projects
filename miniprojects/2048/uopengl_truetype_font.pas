(******************************************************************************)
(* uopengl_truetype_font.pas                                       ??.??.???? *)
(*                                                                            *)
(* Version     : 0.02                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Unit OpenGL TrueType Font stellt die Klasse                  *)
(*               TOpenGL_TrueType_Font zur Verfügung                          *)
(*               Die Kommentare sind pasdoc Kompatibel                        *)
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
(*               0.02 - Bugfix Setcolor                                       *)
(*                                                                            *)
(******************************************************************************)
Unit uopengl_truetype_font;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, graphics, fileutil, math, dglopengl, IntfGraphics,
  fpImage, uvectormath, LConvEncoding;

Type
  //Basis Datentyp zum Abspeichern eines Pixels mit Transparenzwert
  TPixel = Record
    Alpha: byte;
    x: Byte;
    y: Byte;
  End;

  // Datentyp für einen Buchstaben
  TLetter = Array Of TPixel;

  { TOpenGL_TrueType_Font }

  TOpenGL_TrueType_Font = Class
  private
    FLetters: Array[0..255] Of TLetter; //< Die datencontainer für die Einzelnen Buchstaben
    fCharWidths: Array[0..255] Of integer; //< Die Breiten der Einzelnen Buchstaben ( das sie nicht jedes mal neu ermittelt werden müssen )
    fCharheight: integer; //< Alle Buchstaben sind Gleich hoch ( so hoch wie das Leerzeichen
    fColor: TVector3; //< Farbe der Schrift
    fsize: Single; //< Größe der Schrift in Pixeln ( Höhe )
    Fscreenwidth: integer; //< Aus Kompatibilitätsgründen werden nicht oder nur Teilweise unterstützt
    Fscreenheight: integer; //< Aus Kompatibilitätsgründen werden nicht oder nur Teilweise unterstützt
    FFontGo2D: boolean; //< Aus Kompatibilitätsgründen werden nicht oder nur Teilweise unterstützt
    Function getcolor: TColor;
    Function getcolor3v: TVector3;
    Function getcolorfp: TFPColor;
    Function getsize: Single;
    Procedure setColor(AValue: TColor);
    Procedure setColor3v(AValue: TVector3);
    Procedure setColorfp(AValue: TFPColor);
    Procedure setsize(AValue: Single);
    Procedure SaveLetter(Const Stream: TFileStream; Const Letter: TLetter);
    Function LoadLetter(Const Stream: TFileStream): TLetter;
    Procedure DrawLetter(Index: byte);
    Procedure Go2d(); //< Aus Kompatibilitätsgründen werden nicht oder nur Teilweise unterstützt
    Procedure Exit2d(); //< Aus Kompatibilitätsgründen werden nicht oder nur Teilweise unterstützt
  public
    Property Color: TColor read getcolor write setColor; //< Zugriff auf die Schriftfarbe
    Property Color3v: TVector3 read getcolor3v write setColor3v; //< Zugriff auf die Schriftfarbe
    Property Colorfp: TFPColor read getcolorfp write setColorfp; //< Zugriff auf die Schriftfarbe
    Property Size: Single read getsize write setsize; //< Höhe der Schrift in Pixeln

    Constructor Create();
    Destructor Destroy; override;

    Procedure Init_Create_Font_by_Bitmaps();
    Procedure Add_Letter(Index: byte; Const Image: tbitmap);
    Procedure SaveToFile(Filename: String);

    Procedure LoadfromFile(Filename: String);
    Procedure Textout(x, y: integer; Text: String);
    Function TextWidth(Text: String): single;
    Function TextHeight(text: String): single;
    Procedure SetColor(r, g, b: Float); //< Aus Kompatibilitätsgründen werden nicht oder nur Teilweise unterstützt
    Property FontGo2D: Boolean read FFontGo2D write FFontGo2D; //< Aus Kompatibilitätsgründen werden nicht oder nur Teilweise unterstützt, Wenn True, dann ruft die Routine beim Textout ein Go2D und Exit2D auf
    Property ScreenWidth: integer read Fscreenwidth write Fscreenwidth; //< Aus Kompatibilitätsgründen werden nicht oder nur Teilweise unterstützt
    Property ScreenHeight: integer read Fscreenheight write Fscreenheight; //< Aus Kompatibilitätsgründen werden nicht oder nur Teilweise unterstützt
  End;

Implementation

(*
  Konvertiert eine TFPColor nach TColor
  @param Color Quellfarbe
  @return Konvertierte Farbe
 *)

Function FPColorToColor(Const Color: TFPColor): TColor;
Begin
  result := byte(color.red Shr 8) Or (color.green And $FF00) Or ((color.blue And $FF00) Shl 8);
End;

(*
  Berechnet anhand der Luminanzgleichung den Helligkeitswert einer Farbe
  @param C Farbe in RGB Format
  @return Helligkeitswert der Farbe
 *)

Function Luminance(C: TFPColor): Byte;
Begin
  //   Y = 0.3R + 0.59G + 0.11B
  result := min(255, max(0, round(0.3 * (c.red Shr 8) + 0.59 * (c.green Shr 8) + 0.11 * (c.blue Shr 8))));
End;

{ TOpenGL_TrueType_Font }

(*
  Initialisiert die Klassenvariablen
 *)

Constructor TOpenGL_TrueType_Font.Create;
Var
  i: integer;
Begin
  Inherited create;
  Init_Create_Font_by_Bitmaps();
  For i := 0 To 255 Do Begin
    fCharWidths[i] := 0;
  End;
  fCharheight := 0;
  fColor := v3(1, 1, 1);
  fsize := 0;
  FFontGo2D := false;
End;

(*
  Freigeben aller Klassenvariablen
 *)

Destructor TOpenGL_TrueType_Font.Destroy;
Begin
  Init_Create_Font_by_Bitmaps; // Löschen der Evtl initialisierten Chars
End;

Procedure TOpenGL_TrueType_Font.Go2d();
Begin
  glMatrixMode(GL_PROJECTION);
  glPushMatrix(); // Store The Projection Matrix
  glLoadIdentity(); // Reset The Projection Matrix
  glOrtho(0, Fscreenwidth, Fscreenheight, 0, -1, 1); // Set Up An Ortho Screen
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix(); // Store old Modelview Matrix
  glLoadIdentity(); // Reset The Modelview Matrix
End;

Procedure TOpenGL_TrueType_Font.Exit2d();
Begin
  glMatrixMode(GL_PROJECTION);
  glPopMatrix(); // Restore old Projection Matrix
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix(); // Restore old Projection Matrix
End;

(*
  Gibt die Aktuell Verwendete Farbe der Schrift zurücl
  @return Aktuell verwendete Renderfarbe
 *)

Function TOpenGL_TrueType_Font.getcolor: TColor;
Var
  r, g, b: integer;
Begin
  r := max(0, min(255, round(fColor.x * 255)));
  g := max(0, min(255, round(fColor.y * 255)));
  b := max(0, min(255, round(fColor.z * 255)));
  result := r Or (g Shl 8) Or (b Shl 16);
End;

(*
  Setzt die Schriftfarbe
  @param AValue neue Schriftfarbe
 *)

Procedure TOpenGL_TrueType_Font.setColor(AValue: TColor);
Begin
  fcolor.x := byte(Avalue) / 255;
  fcolor.y := byte(Avalue Shr 8) / 255;
  fcolor.z := byte(Avalue Shr 16) / 255;
End;

(*
  Speichert einen Buchstaben in den Stream
  @param Stream Stream in den Gespeichert wird
  @param Letter Zu Speichernder Buchstabe
 *)

Procedure TOpenGL_TrueType_Font.SaveLetter(Const Stream: TFileStream; Const Letter: TLetter);
Var
  i: integer;
Begin
  i := high(letter) + 1;
  Stream.Write(i, SizeOf(i));
  For i := 0 To high(letter) Do Begin
    Stream.Write(letter[i], SizeOf(TPixel));
  End;
End;

(*
  Lädt einen Buchstaben aus einem Stream
  @param Stream Datenquelle
  @return geladener Buchstabe
 *)

Function TOpenGL_TrueType_Font.LoadLetter(Const Stream: TFileStream): TLetter;
Var
  i: integer;
Begin
  i := 0;
  stream.Read(i, SizeOf(i));
  result := Nil;
  setlength(result, i);
  For i := 0 To high(result) Do Begin
    stream.Read(result[i], sizeof(TPixel));
  End;
End;

(*
  @param Filename Dateiname der zu Speicherung der Font
 *)

Procedure TOpenGL_TrueType_Font.SaveToFile(Filename: String);
Var
  f: TFileStream;
  i: integer;
Begin
  f := TFileStream.Create(Filename, fmcreate Or fmOpenWrite);
  For i := 0 To 255 Do Begin
    SaveLetter(F, FLetters[i]);
  End;
  f.free;
End;

(*
  @param Filename Dateiname der zu ladenden Font
 *)

Procedure TOpenGL_TrueType_Font.LoadfromFile(Filename: String);
Var
  f: TFileStream;
  j, i: integer;
Begin
  Init_Create_Font_by_Bitmaps;
  f := TFileStream.Create(filename, fmOpenRead);
  fCharHeight := 0;
  For i := 0 To 255 Do Begin
    FLetters[i] := LoadLetter(f);
    fCharWidths[i] := 0;
    For j := 0 To high(FLetters[i]) Do Begin
      fCharHeight := max(fCharHeight, FLetters[i][j].y);
      fCharWidths[i] := max(fCharWidths[i], FLetters[i][j].x);
    End;
  End;
  fsize := fCharHeight;
  f.Free;
End;

(*
  Initialisiert alle Notwendigen Variablen zum Erzeugen der True Type Font
 *)

Procedure TOpenGL_TrueType_Font.Init_Create_Font_by_Bitmaps();
Var
  i: integer;
Begin
  // Alle Letters "löschen"
  For i := 0 To 255 Do Begin
    setlength(FLetters[i], 0);
  End;
End;

(*
  @param index ASCII-Code des zu übernehmenden Buchstabens
  @param Image Bilddaten des Zu übernehmenden Buchstabens, dieser sollte in Schwarz weis sein, auf Schwarzem Hintergrund. Weis = Opak, Schwarz = Transparent Alle Bilder sind unten angeschlagen
 *)

Procedure TOpenGL_TrueType_Font.Add_Letter(Index: byte; Const Image: tbitmap);
Var
  i, j: integer;
  TempIntfImg: TLazIntfImage;
  cnt: integer;
  lum: byte;
Begin
  setlength(FLetters[index], 50);
  cnt := 0;
  TempIntfImg := TLazIntfImage.Create(0, 0);
  TempIntfImg.LoadFromBitmap(Image.Handle, Image.MaskHandle);
  For i := 0 To Image.Width - 1 Do
    For j := 0 To Image.Height - 1 Do Begin
      lum := Luminance(TempIntfImg.Colors[i, j]);
      If lum > 0 Then Begin
        FLetters[index][cnt].x := i;
        FLetters[index][cnt].y := j;
        FLetters[index][cnt].Alpha := lum;
        inc(cnt);
        If cnt > high(FLetters[index]) Then Begin
          setlength(FLetters[index], high(FLetters[index]) + 51);
        End;
      End;
    End;
  If cnt = 0 Then Begin
    // So machen wir aus dem Leerzeichen ein Zeichen der Breite Width
    setlength(Fletters[index], 1);
    fletters[index][0].y := image.Height - 1;
    fletters[index][0].Alpha := 0;
    fletters[index][0].x := image.Width - 1;
    cnt := 1;
  End;
  setlength(FLetters[index], cnt);
  TempIntfImg.Free;
End;

(*
  Zeichned den Buchstaben mit ASCII Wert Index
  @param Index ASCII-Wert des Buchstabens
 *)

Procedure TOpenGL_TrueType_Font.DrawLetter(Index: byte);
Var
  i: integer;
  alpha: float;
Begin
  If high(FLetters[index]) <> -1 Then Begin
    glbegin(GL_POINTS);
    For i := 0 To High(FLetters[index]) Do Begin
      If fletters[index][i].Alpha <> 0 Then Begin // Wenn der Pixel Voll transparent ist, brauchen wir ihn auch nicht malen..
        alpha := fletters[index][i].Alpha / 255;
        glColor4f(fColor.x, fColor.y, fColor.z, 1 - alpha);
        glVertex2i(fletters[index][i].x, fletters[index][i].y);
      End;
    End;
    glend();
    glTranslatef(fCharWidths[index] + 2, 0, 0);
  End;
End;

(*
  Gibt einen Text an den Koordinaten x,y aus
  ( Es wird von einem Windows üblichen Koordinatensystem ausgegangen )
  @param x X-Position des Textes
  @param y Y-Position des Textes
  @param Text der Aus zu gebende Text
 *)

Procedure TOpenGL_TrueType_Font.Textout(x, y: integer; Text: String);
Var
  k: integer;
  c: integer;
  sc: Single;
  ps: Single;
  b: {$IFDEF USE_GL}Byte{$ELSE}Boolean{$ENDIF};
Begin
  glBindTexture(GL_TEXTURE_2D, 0); // Entladen des Texturspeichers, da dieser uns beeinflusst
  B := glIsEnabled(gl_Blend);
  If Not (b{$IFDEF USE_GL} = 1{$ENDIF}) Then
    glenable(gl_Blend);
  glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);
  glPushMatrix();
  If FFontGo2D Then Go2d();
  glTranslatef(x, y, 0);
  glGetFloatv(GL_POINT_SIZE, @ps);
  sc := (fsize / fCharheight);
  glPointSize(sc + 0.45 + ps - 1); // Ohne diesen Faktor, sind in diversen Skallierungen keine ganzen Buchstaben zu sehen.
  glScalef(sc, sc, sc);
  glPushMatrix();
  text := ConvertEncoding(text, EncodingUTF8, 'iso88591');
  For k := 1 To length(text) Do Begin
    c := ord(text[k]);
    If (c = 10) Then Begin
      glPopMatrix();
      glTranslatef(0, fCharheight, 0);
      glPushMatrix();
    End
    Else If c <> 13 Then Begin
      If (high(FLetters[c]) = -1) Then c := 32; // Zeichen die nicht Spezifiziert sind werden als " " gemalt.
      DrawLetter(c);
    End;
  End;
  glPopMatrix();
  glPopMatrix();
  glPointSize(ps);
  If Not (b{$IFDEF USE_GL} = 1{$ENDIF}) Then
    gldisable(gl_blend);
  If FFontGo2D Then Exit2d();
  glcolor3f(1, 1, 1); // Reset def OpenGL Farbe, nach außen
End;

(*
  Liefert die Breite in Pixeln des Textes Zurück
  @param Text Text von welchem die Breite bestimmt werden soll.
  @return Textbreite in Pixeln
 *)

Function TOpenGL_TrueType_Font.TextWidth(Text: String): single;
  Function SubTextWidth(data: String): Single;
  Var
    i: integer;
    c: byte;
  Begin
    result := 0;
    For i := 1 To length(data) Do Begin
      c := ord(data[i]);
      result := result + fCharWidths[c] + 2;
    End;
  End;
Var
  j: single;
  s: String;
Begin
  j := 0;
  While pos(#13, text) <> 0 Do
    delete(text, pos(#13, text), 1);
  While pos(#10, text) <> 0 Do Begin
    s := copy(text, 1, pos(#10, text) - 1);
    j := max(j, SubTextWidth(s));
    delete(Text, 1, pos(#10, text));
  End;
  j := max(j, SubTextWidth(text));
  result := j * (fsize / fCharheight);
End;

(*
  Liefert die Höhe in Pixeln des Textes Zurück
  @param Text Text von welchem die Breite bestimmt werden soll.
  @return Texthöhe in Pixeln
 *)

Function TOpenGL_TrueType_Font.TextHeight(text: String): single;
Var
  i, j: integer;
Begin
  j := 1;
  For i := 1 To length(text) Do Begin
    If ord(text[i]) = 10 Then inc(j);
  End;
  result := j * fsize;
End;

(*
  Setzt die Schriftfarbe
  @param r Rot-Wert der Farbe [0..1]
  @param g Grün-Wert der Farbe [0..1]
  @param b Blau-Wert der Farbe [0..1]
 *)

Procedure TOpenGL_TrueType_Font.SetColor(r, g, b: Float);
Begin
  fColor := v3(r, g, b);
End;

(*
  Gibt die Aktuelle Zeichhöhe in Pixeln zurück
  @return Höhe eines Zeichens in Pixel
 *)

Function TOpenGL_TrueType_Font.getsize: Single;
Begin
  result := fsize;
End;

(*
  Setzt die Pixelhöhe aller Zeichen
  @param AValue Neue Höhe in Pixel
 *)

Procedure TOpenGL_TrueType_Font.setsize(AValue: Single);
Begin
  fsize := AValue;
End;

(*
  Gibt die Aktuell Verwendete Farbe der Schrift zurücl
  @return Aktuell verwendete Renderfarbe
 *)

Function TOpenGL_TrueType_Font.getcolor3v: TVector3;
Begin
  result := fColor;
End;

(*
  Gibt die Aktuell Verwendete Farbe der Schrift zurücl
  @return Aktuell verwendete Renderfarbe
 *)

Function TOpenGL_TrueType_Font.getcolorfp: TFPColor;
Var
  r, g, b: integer;
Begin
  r := max(0, min(255, round(fColor.x * 255))) Shl 8 Or $FF;
  g := max(0, min(255, round(fColor.y * 255))) Shl 8 Or $FF;
  b := max(0, min(255, round(fColor.z * 255))) Shl 8 Or $FF;
  result.red := r;
  result.green := g;
  result.blue := b;
  result.alpha := 0;
End;

(*
  Setzt die Schriftfarbe
  @param AValue neue Schriftfarbe
 *)

Procedure TOpenGL_TrueType_Font.setColor3v(AValue: TVector3);
Begin
  fColor := AValue;
End;

(*
  Setzt die Schriftfarbe
  @param AValue neue Schriftfarbe
 *)

Procedure TOpenGL_TrueType_Font.setColorfp(AValue: TFPColor);
Begin
  fcolor.x := ((AValue.red And $FF00) Shr 8) / 255;
  fcolor.y := ((AValue.green And $FF00) Shr 8) / 255;
  fcolor.z := ((AValue.blue And $FF00) Shr 8) / 255;
End;

End.

