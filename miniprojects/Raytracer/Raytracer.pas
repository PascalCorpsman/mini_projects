(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of SimpleRay                                             *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
(*                                                                            *)
(* WICHTIG                                                                    *)
(*                                                                            *)
(* Bitte immer den TexturManager für Texturen benutzen.                       *)
(* hierbei ist zu beachten das dann das Property                              *)
(*                                                                            *)
(* Shared_Texture                                                             *)
(*                                                                            *)
(* des Jeweiligen Materials des Primitives auf True gesetzt werden mus !!     *)
(*                                                                            *)
(* Am einfachsten ist es man benutzt den Rayparser, dieser setzt und Checkt   *)
(* alles was notwendig ist.                                                   *)
(*                                                                            *)
(******************************************************************************)

(*

ACHTUNG !!!

Diese Unit ist rein auf Funktion hin erstellt worden.

Zum Größten Teil wurde auf Optimierung Komplett verzichtet.

Teilweise steht im Code aber wo Optimiert werden könnte.

*)

Unit Raytracer;

{$MODE ObjFPC}{$H+}

Interface

Uses
  IntfGraphics, fpImage,
  classes, LCLIntf, LCLType, // Trect, benötigt für Canvas.Copyrect
  Graphics, // Tbitmap
  sysutils, // Exceptions
  math,
  Raytracer_Math; // 3D-Math

Type
  (*
  Diverse Ordinaltypen zum Konfigurieren des Raytracers
  *)
  TLightOverflowMode = (Clamp, Scale);
  TCullMode = (FrontFace, BackFace);
  TInterpolationMode = (Nearest_Neighbour, Biliniar, Cosinus, Triliniar);
  TTexParameter = (ClampMode, RepeatMode);
  TShadowMode = (Linear, Cos);

  (*
  Ach wäre das schön wenn man dieses Record von ausen net sehen könnte
  *)
  TTexturemanagerData = Record
    Filename: String;
    Texture: TLazIntfImage;
  End;

  (*
  Basisklasse für alle Materialeigenschaften die ein Primitiv haben kann
  *)
  TMaterial = Record
    Shared_Texture: Boolean; // True = Bitmap wird nicht Kopiert, nur der Pointer, False = neue Instanz wird erstellt.
    Texture: TLazIntfImage; // Pointer auf die Textur.
    TransparentTexture: TLazIntfImage; // Überschreibt den Transparent wert , dafür aber als Texture ( Im Prinzip Alpha Channeltransparents )
    TexParameter: TTexParameter; // Gibt an wie u,v parameter auserhalb [ 0 .. 1 ] behandelt werden sollen
    TexInterpolationMode: TInterpolationMode; // gibt den InterpolationsModus der Textur an
    Reflectance: TRayFloat; // [ 0 .. 1 ], 0 = diffuse, 1 = Crome
    Transparence: TRayFloat; // [ 0 .. 1 ], 0 = undurchsichtig, 1 = Durchsichtig
    SmoothShadow: Trayfloat; //  x >= 0, 0 = aus, x > 0 = breite des "Smooth" schattens den das Object wirft.
    SmoothShadowMode: TShadowMode; //  Liniar, Cos
    SoftshadowBorder: TRayFloat; //  x >= 0, 0 = aus, x > 0 = breite des "Smooth" schattens Auf dem Object.
    SoftShadowMode: TShadowMode; // Liniar, Cos
    HighlightExponent: TRayFloat; // gibt im Prinzip die Breite der Highlights an
    SpecularColor: TVector3f; // Die Specular wirkenden Farbanteile
    DiffuseColor: TVector3f; // Die Diffus wirkenden Farbanteile
    AmbientColor: TVector3f; // Die Ambient wirkenden Farbanteile
    EmissionColor: TVector3f; // Die Farbanteile die Selbstleuchtend sind
  End;

  (*
  Der Texturmanager sorgt dafür das der Raytracer nicht ein und die Selbe Textur
  zig fach in den Speicher Läd
  *)

  TTextureManager = Class
  private
    FData: Array Of TTexturemanagerData;
  public
    Constructor create;
    Destructor destroy; override;
    // Fügt ein neues Bild in die Datenbank ein , falls es vorher nicht schon Existierte
    // in Beiden Fällen wird der Pointer auf das Bild zurückgegeben.
    Function AddTexture(Filename: String): TLazIntfImage;
    // Löschen aller Texturen.
    Procedure Clear;
  End;

  (*
  Basisklasse für alle primitiven
  *)
  TPrimitive = Class
  private
    FObjectname: Integer;
    Function GetUV(Value: TVector3f): TVector2f; virtual;
  public
    Name: String;
    Material: TMaterial;
    SmoothShadowFactor: TRayFloat;
    Constructor create; virtual;
    Destructor destroy; override;
    Procedure Rotate(Angle, dx, dy, dz: TRayFloat); virtual;
    Procedure Translate(Value: TVector3f); virtual;
    Function Intersect(Position, Direction: TVector3f; Schattenstrahl: Boolean): TRayFloat; virtual;
    Function NormalAtPoint(Value: TVector3f): TVector3f; virtual;
    Function ColorAtPoint(value: TVector3f): Tvector3f; virtual;
    Function TransparentsAtPoint(value: TVector3f): TRayFloat; virtual;
  End;

  TPrimitiveArray = Array Of TPrimitive;

  (*
  Ein Dreieck
  *)
  TTriangle = Class(Tprimitive)
  private
    SmoothshadowPoints,
      Fpoints: Array[0..2] Of Tvector3f;
    FNormal: TVector3f;
    FN: TVector3f; // Die nicht normierte Normale
    FLennSqr: TRayFloat; // Das Quadrat der Länge der nicht normierten Normalen
    Fv0_1: TVector3f;
    Fv0_2: TVector3f;
    Function Baryzent(Value: TVector3f): TVector3f;
    Function GetUV(Value: TVector3f): TVector2f; override;
  public
    TextureCoord: Array[0..2] Of TVector2f;
    Constructor create; override;
    Destructor destroy; override;
    Procedure Rotate(Angle, dx, dy, dz: TRayFloat); override;
    Procedure Translate(Value: TVector3f); override;
    Procedure Define(Point1, Point2, Point3: TVector3f);
    Procedure CalculateSmoothShadows;
    Function Intersect(Position, Direction: TVector3f; Schattenstrahl: Boolean): TRayFloat; override;
    Function NormalAtPoint(Value: TVector3f): TVector3f; override;
    Function ColorAtPoint(value: TVector3f): Tvector3f; override;
  End;

  (*
  Ein Quad

  Quads sind Speziell, da sie eigentlich nicht existieren, aber doch sehr häufig gebraucht werden.
  um uns das Lieben hier einfacher zu machen werden Quads beim Adden im Raytracer automatisch in
  2 Dreiecke Zerlegt. Das Spart deutlich Logik, ob's schneller ist... Egal.
  *)
  TQuad = Class(TPrimitive)
  private
  public
    Fpoints: Array[0..3] Of TVector3f; // Zwischenspeichern unserer Punkte
    TextureCoord: Array[0..3] Of TVector2f;
    Constructor create; override;
    Destructor destroy; override;
    Procedure Rotate(Angle, dx, dy, dz: TRayFloat); override;
    Procedure Translate(Value: TVector3f); override;
    Procedure Define(Point1, Point2, Point3, Point4: TVector3f);
    Function Intersect(Position, Direction: TVector3f; Schattenstrahl: Boolean): TRayFloat; override;
    Function NormalAtPoint(Value: TVector3f): TVector3f; override;
    Function ColorAtPoint(value: TVector3f): Tvector3f; override;
  End;

  (*
  Eine Kugel
  *)
  TSphere = Class(TPrimitive)
  private
    Fposition: TVector3f;
    Fradius: TRayFloat;
    FSP: TVector3f; // Fürs texturmapping brauchen wir den Pfeil zum Pol
    FSE: TVector3f; // Fürs texturmapping brauchen wir den Pfeil nach Osten
    Function GetUV(Value: TVector3f): TVector2f; override;
  public
    Constructor create; override;
    Destructor destroy; override;
    Procedure Rotate(Angle, dx, dy, dz: TRayFloat); override;
    Procedure Translate(Value: TVector3f); override;
    Function Define(Position: TVector3f; Radius: TRayFloat): Boolean;
    Function Intersect(Position, Direction: TVector3f; Schattenstrahl: Boolean): TRayFloat; override;
    Function NormalAtPoint(Value: TVector3f): TVector3f; override;
    Function ColorAtPoint(value: TVector3f): Tvector3f; override;
  End;

  (*
  Eine Lichtquelle
  *)
  TLight = Class(TPrimitive)
  private
  public
    IsActive: boolean;
    Position: Tvector3f;
    Constructor create; override;
    Destructor Destroy; override;
  End;

  (*
  Die eigentliche Raytracerklasse
  *)
  TRaytracer = Class
  private
    FLights: Array Of TLight;
    FElements: Array Of TPrimitive;
    FEyepos: Tvector3f;
    FEyeUp: Tvector3f;
    FEyeDir: Tvector3f;
    FRekursionDepth: Integer;
    Fobjectname: integer;
    Fz: Integer;
    FButtomLeft: TVector2f;
    FTopRight: TVector2f;
    Procedure SetRekursionDepth(value: integer);
    Function Raytrace(Depth: Integer; Position, Direction: TVector3f; Distance: TRayFloat): Tvector3f;
    Function Intersect(Position, Direction: TVector3f; Schattenstrahl: Boolean; Var Primitive: TPrimitive): TRayFloat;
    Function FGetPrimitiveCount: integer;
  public
    LightOverflowMode: TLightOverflowMode;
    UseFog: Boolean;
    FogMin: TRayFloat;
    FogMax: TRayFloat;
    FogColor: TVector3f;
    FogMode: TShadowMode;
    UseOrthoProjection: Boolean;
    UseCullfaceing: Boolean;
    CullFaceMode: TCullMode;
    BackgroundColor: TVector3f;
    MaxusedRecursionDeth: integer;
    Property RekursionDepth: Integer read FRekursionDepth write SetRekursionDepth;
    Property PrimitiveCount: Integer read FGetPrimitiveCount;
    Constructor Create;
    Destructor Destroy; override;
    (*
    Die Schnittstelle des Raytracers nach ausen.

    Werden Lichtquellen geadded so wird geschaut ob 2 an der selben Position sind. Wenn ja dann wird die alte überschrieben.
    *)
    Procedure AddPrimitive(Value: TPrimitive);
    Procedure ClearScene;
    Procedure Render(Const Canvas: TCanvas; Width, Height: Integer);
    Procedure SetEye(Position, Up, Direction: Tvector3f);
    Procedure LookAt(Position, Center, Up: Tvector3f);
    Procedure SetOrtho(ButtomLeft, TopRight: TVector2f; Z: TRayFloat);
  End;

Function InitialisedMaterial: TMaterial;

Implementation

//Uses Unit1; // Nur zum Debuggen

Type
  PixArray = Array[1..3] Of Byte;

Function Rect(ALeft, ATop, ARight, ABottom: Integer): TRect;
Begin
  result.left := aleft;
  result.Top := atop;
  result.Right := aright;
  result.Bottom := abottom;
End;
{
Function ArcTan2(Y, X: Extended): Extended;
Asm
        FLD     Y
        FLD     X
        FPATAN
        FWAIT
End;
}

Function ArcCos(X: Extended): Extended;
Begin
  Result := ArcTan2(Sqrt(1 - X * X), X);
End;

Function ReadPixel(Const Bitmap: TLazIntfImage; x, y: Integer): TVector3f;
Var
  c: TFPColor;
Begin
  If assigned(bitmap) Then Begin
    c := Bitmap.Colors[x, y];
    result.x := (c.red Shr 8) / 255;
    result.y := (c.green Shr 8) / 255;
    result.z := (c.blue Shr 8) / 255;
  End
  Else
    result := v3(0, 0, 0);
  {
  If assigned(bitmap) Then Begin
    c := bitmap.Canvas.Pixels[x, y];

    result := V3(
      (c And $FF) / 255,
      ((c And $FF00) Shr 8) / 255,
      ((c And $FF0000) Shr 16) / 255
      );
    //    Row := bitmap.ScanLine[y];
    //    inc(Row, x);
    //    result := v3(row^[3] / 255, row^[2] / 255, row^[1] / 255);
  End
  Else
    result := v3(0, 0, 0);}
End;

(*
Diese Function interpoloert anhand der u und v Parameter einen Pixel aus der Übergebenen Graphik
Dies geschieht in Abhängigkeit der Übergebenen Modi
*)

Function Interpolate(Const Bitmap: TLazIntfImage; u, v: TRayFloat; Mode: TInterpolationMode; TexParameter: TTexParameter): Tvector3f;
Var
  i, nx, ny: Integer;
  rx, ry: TRayFloat;
  P: Array[0..3] Of Tpoint;
  c: Array[0..3] Of TVector3f;
  tp1, tp2: TVector3f;
Begin
  Case Mode Of
    Nearest_Neighbour: Begin
        //        nx := round(u * bitmap.width);
        //        ny := round(v * bitmap.Height);
        nx := Trunc(u * bitmap.width); // Nach neuesten Ergebnissen müste Trunc Besser sein als Round !
        ny := Trunc(v * bitmap.Height); // Nach neuesten Ergebnissen müste Trunc Besser sein als Round !
        Case TexParameter Of
          ClampMode: Begin
              nx := min(bitmap.width - 1, nx);
              nx := max(0, nx);
              ny := min(bitmap.height - 1, ny);
              ny := max(0, ny);
            End;
          RepeatMode: Begin
              While nx < 0 Do
                inc(nx, bitmap.width);
              nx := nx Mod Bitmap.width;
              While ny < 0 Do
                inc(ny, bitmap.height);
              ny := ny Mod Bitmap.height;
            End;
        End;
        // Das könnte deutlich schneller mittels Scanline gemacht werden !!!!
        result := ReadPixel(bitmap, nx, ny);
      End;
    Biliniar, Cosinus: Begin
        rx := u * Bitmap.Width;
        ry := v * bitmap.height;
        p[0].x := Trunc(rx);
        p[0].y := Trunc(ry);
        P[1].x := p[0].x + 1;
        P[1].y := p[0].y;
        P[2].x := p[0].x + 1;
        P[2].y := p[0].y + 1;
        P[3].x := p[0].x;
        P[3].y := p[0].y + 1;
        Case TexParameter Of
          ClampMode: Begin
              For i := 0 To 3 Do Begin
                P[i].x := min(bitmap.width - 1, P[i].x);
                P[i].x := max(0, P[i].x);
                P[i].y := min(bitmap.height - 1, P[i].y);
                P[i].y := max(0, P[i].y);
              End;
            End;
          RepeatMode: Begin
              For i := 0 To 3 Do Begin
                While P[i].x < 0 Do
                  inc(P[i].x, bitmap.width);
                P[i].x := P[i].x Mod Bitmap.width;
                While P[i].y < 0 Do
                  inc(P[i].y, bitmap.height);
                P[i].y := P[i].y Mod Bitmap.height;
              End;
            End;
        End;
        // Hohlen der RGB Werte an den Entsprechenden Pixelpositionen
        For i := 0 To 3 Do Begin
          c[i] := ReadPixel(bitmap, P[i].x, p[i].y);
        End;
        // Das Interpolieren
        rx := rx - Trunc(rx);
        ry := ry - Trunc(ry);
        If Mode = cosinus Then Begin
          tp1 := InterpoliereCosV3(c[0], c[1], rx);
          tp2 := InterpoliereCosV3(c[3], c[2], rx);
          Result := InterpoliereCosV3(tp1, tp2, ry);
        End
        Else Begin
          tp1 := InterpoliereLinearV3(c[1], c[0], rx);
          tp2 := InterpoliereLinearV3(c[2], c[3], rx);
          Result := InterpoliereLinearV3(tp2, tp1, ry);
        End;
      End;
    Triliniar: Begin
        Raise Exception.create('Fehler noch keine Triliniare Interpolation Implementiert');
      End;
  End;
End;

(*
Eigentlich sollte es Egal sein was hier wie Definiert wird.
Ein Ordentlicher user macht das eh noch mal nachträglich, aber man weis ja nie.
*)

Function InitialisedMaterial: TMaterial;
Begin
  result.Reflectance := 0.0;
  result.SmoothShadow := 0.0;
  result.SmoothShadowMode := linear;
  result.SoftshadowBorder := 0;
  result.SoftShadowMode := linear;
  result.Transparence := 0.0;
  Result.EmissionColor := v3(0.0, 0.0, 0.0);
  result.AmbientColor := v3(0.0, 0.0, 0.0);
  result.DiffuseColor := v3(1.0, 1.0, 1.0);
  result.SpecularColor := v3(1.0, 1.0, 1.0);
  result.HighlightExponent := 64.0;
  result.Texture := Nil;
  result.TransparentTexture := Nil;
  result.Shared_Texture := false;
  result.TexParameter := ClampMode;
  result.TexInterpolationMode := Nearest_Neighbour;
End;

{ TPrimitive }

Constructor TPrimitive.create;
Begin
  Inherited;
  SmoothShadowFactor := 0;
  FObjectname := -1;
  name := 'TPrimitive';
  Material := InitialisedMaterial;
End;

Destructor TPrimitive.destroy;
Begin
  //  inherited; // Brauch mer net da von Tobject abgeleitet
  // Nur die die wir wirklich selbst erzeugt haben geben wir auch Frei
  If Not material.Shared_Texture Then
    If assigned(material.texture) Then
      material.texture.free;
  material.texture := Nil;
End;

Function TPrimitive.GetUV(Value: TVector3f): TVector2f;
Begin
  Raise exception.Create('GetUV was not implemented in ' + name);
End;

Function TPrimitive.NormalAtPoint(Value: TVector3f): TVector3f;
Begin
  Raise exception.Create('NormalAtPoint was not implemented in ' + name);
End;

Function TPrimitive.Intersect(Position, Direction: TVector3f; Schattenstrahl: Boolean): TRayFloat;
Begin
  Raise exception.Create('Intersect was not implemented in ' + name);
End;

Function TPrimitive.ColorAtPoint(value: TVector3f): Tvector3f;
Begin
  Raise exception.Create('ColorAtPoint was not implemented in ' + name);
End;

Procedure TPrimitive.Rotate(Angle, dx, dy, dz: TRayFloat);
Begin
  Raise exception.Create('Rotate was not implemented in ' + name);
End;

Function TPrimitive.TransparentsAtPoint(value: TVector3f): TRayFloat;
Var
  uv: TVector2f;
Begin
  If assigned(material.transparentTexture) Then Begin
    uv := GetUV(Value);
    Result := GetLuminance(Interpolate(material.transparentTexture, 1 - uv.x, 1 - uv.y, material.TexInterpolationMode, material.TexParameter));
  End
  Else
    result := Material.Transparence;
End;

Procedure TPrimitive.Translate(Value: TVector3f);
Begin
  Raise exception.Create('Translate was not implemented in ' + name);
End;

{ TRaytracer }

Constructor TRaytracer.Create;
Begin
  Inherited;
  setlength(fElements, 0);
  setlength(FLights, 0);

  ClearScene;
End;

Destructor TRaytracer.Destroy;
Begin
  //  inherited; // Brauch mer net da von Tobject abgeleitet
  ClearScene;
End;

Procedure TRaytracer.AddPrimitive(Value: TPrimitive);
Var
  i: Integer;
  added: Boolean;
Begin
  If assigned(value) Then Begin
    value.FObjectname := Fobjectname;
    inc(Fobjectname);
  End;
  // Dreiecke
  If Value Is TTriangle Then Begin
    setlength(Felements, high(Felements) + 2);
    Felements[high(felements)] := TTriangle.create;
    TTriangle(Felements[high(felements)]).FObjectname := Value.FObjectname;
    TTriangle(Felements[high(felements)]).Define(TTriangle(value).Fpoints[0], TTriangle(value).Fpoints[1], TTriangle(value).Fpoints[2]);
    TTriangle(Felements[high(felements)]).TextureCoord := TTriangle(value).TextureCoord;
    TTriangle(Felements[high(felements)]).Material := TTriangle(value).Material;
    TTriangle(Felements[high(felements)]).CalculateSmoothShadows;
    If assigned(TTriangle(value).Material.texture) And (Not TTriangle(value).Material.Shared_Texture) Then Begin
      TTriangle(Felements[high(felements)]).Material.Texture := TLazIntfImage.Create(0, 0);
      TTriangle(Felements[high(felements)]).Material.Texture.Assign(TTriangle(value).Material.texture);
    End;
    exit;
  End;
  // Rechtecke
  If Value Is TQuad Then Begin
    (*
    Da es ja eigentlich keine Rechtecke Gibt werden die nun Trianguliert und dann als Triangles geadded.
    *)
    Setlength(Felements, high(Felements) + 3);
    Felements[high(felements) - 1] := TTriangle.create;
    Felements[high(felements)] := TTriangle.create;
    TTriangle(Felements[high(felements) - 1]).FObjectname := Value.FObjectname;
    TTriangle(Felements[high(felements)]).FObjectname := Value.FObjectname;
    // Übernehmen der Punkte
    TTriangle(Felements[high(felements) - 1]).Define(Tquad(Value).fpoints[0], Tquad(Value).fpoints[1], Tquad(Value).fpoints[3]);
    //Übernehmen der Texturkoordinaten
    TTriangle(Felements[high(felements) - 1]).TextureCoord[0] := Tquad(Value).TextureCoord[0];
    TTriangle(Felements[high(felements) - 1]).TextureCoord[1] := Tquad(Value).TextureCoord[1];
    TTriangle(Felements[high(felements) - 1]).TextureCoord[2] := Tquad(Value).TextureCoord[3];
    // Übernehmen der Punkte
    TTriangle(Felements[high(felements)]).Define(Tquad(Value).fpoints[1], Tquad(Value).fpoints[2], Tquad(Value).fpoints[3]);
    //Übernehmen der Texturkoordinaten
    TTriangle(Felements[high(felements)]).TextureCoord[0] := Tquad(Value).TextureCoord[1];
    TTriangle(Felements[high(felements)]).TextureCoord[1] := Tquad(Value).TextureCoord[2];
    TTriangle(Felements[high(felements)]).TextureCoord[2] := Tquad(Value).TextureCoord[3];
    // Übernehmen der Materialeigenschaften
    TTriangle(Felements[high(felements) - 1]).Material := TQuad(Value).Material;
    If assigned(TQuad(value).Material.texture) And (Not TQuad(value).Material.Shared_Texture) Then Begin
      TTriangle(Felements[high(felements) - 1]).Material.Texture := TLazIntfImage.create(0, 0);
      TTriangle(Felements[high(felements) - 1]).Material.Texture.Assign(TQuad(value).Material.texture);
    End;
    // Das 2.Triangle übernimmt auf alle Fälle mittels Shared_texture, das Spart Ressourcen
    TTriangle(Felements[high(felements)]).Material := TTriangle(Felements[high(felements) - 1]).Material;
    TTriangle(Felements[high(felements)]).Material.Shared_Texture := True;
    exit;
  End;
  // Kugeln
  If Value Is TSphere Then Begin
    Setlength(Felements, high(Felements) + 2);
    Felements[high(Felements)] := TSphere.create;
    TSphere(Felements[high(Felements)]).Fposition := TSphere(value).fposition;
    TSphere(Felements[high(Felements)]).Fradius := TSphere(value).Fradius;
    TSphere(Felements[high(Felements)]).FSP := TSphere(value).FSP;
    TSphere(Felements[high(Felements)]).FSE := TSphere(value).FSE;
    TSphere(Felements[high(felements)]).FObjectname := Value.FObjectname;
    TSphere(Felements[high(Felements)]).Material := TSphere(value).Material;
    If assigned(TSphere(value).Material.Texture) And (Not TTriangle(value).Material.Shared_Texture) Then Begin
      TSphere(Felements[high(Felements)]).Material.texture := TLazIntfImage.create(0, 0);
      TSphere(Felements[high(Felements)]).Material.texture.Assign(TSphere(value).Material.Texture);
    End;
    exit;
  End; // *)
  // Lichter
  If Value Is TLight Then Begin
    added := False;
    For i := 0 To high(Flights) Do Begin
      If SameV3(Flights[i].Position, TLight(Value).Position) Then Begin
        flights[i].Material := Tlight(Value).Material;
        flights[i].Position := Tlight(Value).Position;
        flights[i].IsActive := Tlight(Value).IsActive;
        added := True;
        Break;
      End;
    End;
    If Not added Then Begin
      setlength(Flights, high(flights) + 2);
      flights[high(flights)] := TLight.create;
      flights[high(flights)].FObjectname := Value.FObjectname;
      flights[high(flights)].Material := Tlight(Value).Material;
      flights[high(flights)].Position := Tlight(Value).Position;
      flights[high(flights)].IsActive := Tlight(Value).IsActive;
    End;
    exit;
  End;

  // Falls wir mal vergessen sollte eine Primitive Klasse zu adden.
  Raise Exception.create('Error Raytracer cannot add the class : ' + Value.name);
End;

Procedure TRaytracer.ClearScene;
Var
  i: Integer;
Begin
  // Initialisiern mit den Globalen Werten
  UseFog := false;
  FogMin := 1;
  FogMax := 2;
  FogColor := v3(0.3, 0.3, 0.3);
  FogMode := Linear;
  LightOverflowMode := Clamp;
  SetEye(v3(0, 0, 0), v3(0, 1, 0), v3(0, 0, 1));
  UseOrthoProjection := false;
  UseCullfaceing := false;
  CullFaceMode := FrontFace;
  FRekursionDepth := 10;
  BackgroundColor := v3(0, 0, 0);

  MinFloat := 0.00001; // Gibt an ab welcher Minimalen Toleranz zwei Float werte als unterschiedlich erkannt werden
  MaxFloat := 10000000000.0; // Nimmt man einen Größeren Wert gibts wieder AV's
  Fobjectname := 0;
  For i := 0 To High(Felements) Do
    Felements[i].free;
  For i := 0 To High(Flights) Do
    FLights[i].free;
  setlength(fElements, 0);
  setlength(FLights, 0);
  FButtomleft := v2(0, 0);
  Ftopright := v2(1, 1);
  Fz := 1;
End;

(*

Diese Function Leistet wohl die Hauptarbeit.

Von Position aus wird ein Strahl quer durch die Scene in Richtung Direction geschossen.
Zurückgegeben wird das kleinste Lambda das vom Aufpunkt der Schnittgerade ein Object in der Scene trifft.

Im Falle einer Collision wird dann das entsprechende Primitive ebenfalls zurückgegeben.

Bei keiner Collision wird -1 zurückgeben.
*)

Function TRaytracer.Intersect(Position, Direction: TVector3f; Schattenstrahl: Boolean; Var Primitive: TPrimitive): TRayFloat;
Var
  i: Integer;
  lambda: TRayFloat;
Begin
  result := MaxFloat;
  Primitive := Nil;
  // Wir müssen wohl oder übel alle Elemente durchgehen und Fragen ob sie ne Coll haben
  // Hier wären jede Menge Ansatzpunkte zm Optimieren ( OctTrees ... )
  For i := 0 To high(Felements) Do Begin
    lambda := Felements[i].Intersect(position, Direction, Schattenstrahl);
    If lambda >= 0 Then
      If Result > lambda Then Begin
        // Die neue Kürzeste Strecke Speichern
        Result := Lambda;
        // Das Collicidierte Element speichern wir für spätere berechnungen gleich mal mit.
        Primitive := Felements[i];
      End;
  End;
  // Wir halten uns an die Spezification
  If Result = MaxFloat Then result := -1;
End;

Function TRaytracer.Raytrace(Depth: Integer; Position, Direction: TVector3f; Distance: TRayFloat): Tvector3f;
Label
  Nochmal;
Var
  Reflectiondir: TVector3f;
  DirectionToLight: TVector3f;
  ReflectionLightdir: TVector3f;
  EmessiverAnteil: TVector3f;
  AmbienterAnteil: TVector3f;
  SpecularerAnteil: TVector3f;
  DiffuserAnteil: TVector3f;
  NormalAtPoint: TVector3f;
  ColorAtPoint: TVector3f;
  TransparentsAtPoint: TRayFloat;
  newpos: TVector3f;
  LightLambda: TRayFloat;
  Lambda: TRayFloat;
  dummy: TPrimitive;
  dummy2: TPrimitive;
  Primitive: TPrimitive;
  i: integer;
  b: Boolean;
  c: Boolean;
  (* Variablen die Ausschlieslich dazu dienen eine im Schatten liegnde Position durch Transparente Objecte hindurch  zu beleuchten *)
  LightedTransparents: TRayFloat;
  LightedColor: TVector3f;
  LightedPosition: TVector3f;
  LightedDiffuseColor: TVector3f;
  LightedSpecularColor: TVector3f;
  (* Diese Variablen werden für den Sanften Schatten benutzt *)
  SoftShadowFactor: TRayFloat;
  SoftShadowMode: TShadowMode;
  SoftShadowLambda1: TRayFloat;
  SoftShadowLambda2: TRayFloat;
  SoftShadowPosition: TVector3f;
  (* Diese Variablen werden für den Sanften Schatten benutzt, Ende *)
  (* Diese Variablen ermöglichen den Sanften Schattenwurf *)
  SmoothShadowFactor: trayfloat;
  SmoothShadowMode: TShadowMode;
  SmoothshadowPosition: TVector3f;
  (* Diese Variablen ermöglichen den Sanften Schattenwurf, Ende *)

  Procedure PruefeHalbschatten(Von: TVector3f);
  Begin
    If assigned(dummy) Then
      If (Dummy.SmoothShadowFactor <> 0) Then Begin
        SmoothShadowFactor := Dummy.SmoothShadowFactor;
        SmoothShadowMode := dummy.material.SmoothShadowMode;
        SmoothshadowPosition := von;
        // Prüfen ob wir zufällig im Kernschatten eines anderen Objects liegen
        LightLambda := Intersect(SmoothshadowPosition, DirectionToLight, false, dummy2);
        If Lengthv3(scalev3(LightLambda, DirectionToLight)) > lengthv3(subv3(newpos, Flights[i].Position)) Then Begin
          Dummy2 := Nil;
          LightLambda := -1;
        End;
        If Lightlambda <> -1 Then SmoothShadowFactor := 0;
        // Wenn die Chance besteht das sich mehrere Halbschatten überschneiden dann müssen wir das nun Prüfen
        If LightLambda = -1 Then Begin
          b := True;
          While b Do Begin
            LightLambda := Intersect(SmoothshadowPosition, DirectionToLight, True, dummy2);
            If Lightlambda = -1 Then
              b := False
            Else Begin
              // Wenn wir was hinter der Lichquelle Erwischt haben
              If Lengthv3(scalev3(LightLambda, DirectionToLight)) > lengthv3(subv3(SmoothshadowPosition, Flights[i].Position)) Then Begin
                Dummy2 := Nil;
                LightLambda := -1;
                b := False;
              End;
              // wir sind Tatsächlich im Halbschatten von irgendwas anderem Gelandet.
              If LightLambda <> -1 Then Begin
                // Wir Hohlen uns den Maximalen Abschwächungsfaktor raus
                SmoothShadowFactor := max(SmoothShadowFactor, dummy2.SmoothShadowFactor);
                // Dieser Versuch Fruchtete nicht.
                // SmoothShadowFactor := min(1, SmoothShadowFactor + Dummy2.SmoothShadowFactor);
                // Und Rücken mit unserer Position näher zur Lichtquelle, falls es noch mehr Halbschatten gibt.
                SmoothshadowPosition := addv3(SmoothshadowPosition, scalev3(Lightlambda + 2 * minfloat, DirectionToLight));
              End;
            End;
          End;
          // Da wir nur Halbschatten Addiert haben bleiben wir im Sichtbaren bereich !!
          LightLambda := -1;
        End;
      End;
  End;

Begin
  SoftShadowMode := linear; // Beruhigt den Compiler
  // Rekursionsabbruch
  If Depth > FRekursionDepth Then Begin
    Result := BackgroundColor;
  End
  Else Begin // Das eigentliche Raytracing
    MaxusedRecursionDeth := max(MaxusedRecursionDeth, Depth);
    nochmal:
    // Suchen des Schnittpunktes mit dem erstgelegenen Primitiv in Strahlrichtung
    lambda := Intersect(Position, Direction, false, Primitive);
    If (Lambda >= 0) Then Begin
      // Berechnen der Position an der der Strahl mit dem Primitive Kollidiert ist.
      newpos := addv3(position, ScaleV3(lambda, direction));
      // Extrahieren der Normalen an diesem Punkt
      NormalAtPoint := Primitive.NormalAtPoint(Newpos);
      // Wenn wir Culling haben, dann gillt es hier zu schaun ob wir den Punkt überhaupt Sehen ;)
      If UseCullfaceing Then Begin
        Case CullFaceMode Of
          FrontFace: Begin
              // Der Pixel ist eigentlich unsichtbar für uns.
              // d.h. wir springen einfach an die Collpos und suchen weiter.
              If DotProdv3(direction, NormalAtPoint) < 0 Then Begin
                position := addv3(newpos, scalev3(2 * MinFloat, direction));
                Goto Nochmal;
              End;
            End;
          BackFace: Begin
              // Der Pixel ist eigentlich unsichtbar für uns.
              // d.h. wir springen einfach an die Collpos und suchen weiter.
              If DotProdv3(direction, NormalAtPoint) > 0 Then Begin
                position := addv3(newpos, scalev3(2 * MinFloat, direction));
                Goto Nochmal;
              End;
            End;
        End;
      End;
      // Erst mal Initialisieren wir unseren Pixel mit Nichts = Schwarz
      Result := v3(0, 0, 0);
      // Wir Hohlen uns den Farbwert den unser Primitive an diesem Punkt hat.
      // Dieser Farbwert ist natürlich nur Sinnvoll wenn wir ne Textur haben.
      // Da wir aber immer Brav prüfen ob wir ne Tex haben können wir das so machen
      // und sparen Rechenzeit.
      ColorAtPoint := Primitive.ColorAtPoint(NewPos);
      // Wenn unser Primitiv selbstleuchtend ist dann mus dieser Farbanteil nun auch mit Reingerechnet werden
      EmessiverAnteil := primitive.material.EmissionColor;
      If assigned(Primitive.Material.Texture) Then
        AmbienterAnteil := CompProdv3(EmessiverAnteil, ColorAtPoint);
      Result := Addv3(EmessiverAnteil, Result);

      // Dann Itterieren wir durch alle Lichtquellen durch und hohlen uns die
      // Einzelnen Farbanteile
      For i := 0 To high(Flights) Do
        // Wir wollen die Option haben einzelne Lichter Ab zu schalten.
        If Flights[i].IsActive Then Begin
          (************************************************************************)
          // Als Erstes Berechnen wir mal die Ambienten Farbanteile.
          AmbienterAnteil := CompProdv3(Primitive.Material.AmbientColor, flights[i].Material.AmbientColor);
          // Hat unser Primitive eine Textur mus natürlich die Textur mit dem Ambienten Anteil genommen werden.
          If assigned(Primitive.Material.Texture) Then
            AmbienterAnteil := CompProdv3(AmbienterAnteil, ColorAtPoint);
          // und Aufhellen des GesamtLichtes um den Ambienten Anteil
          Result := Addv3(AmbienterAnteil, Result);
          // Der Spekulare Anteil ergibt sich aus dem Skalarprodukt von
          // Reflektiertem Lichtstrahl und Blickrichtung, scalliert mit dem
          // Spekularen Anteil der Lichtquelle.
          // Allerdings gibt es ein Highlight nur wenn wir überhaupt die
          // Lichtquelle sehen können.
          DirectionToLight := normalizev3(subv3(Flights[i].Position, newpos));
          // Wir gehen ein winziges Stücken von unserer Neuen Position zurück und senden den Schattenstrahl
          // die Kleine Differenz mus gemacht werden sonst collidieren wir nicht mit unserem Primitive ( falls wir uns selbst verdecken )
          LightLambda := Intersect(addv3(newpos, scalev3(-2 * minfloat, Direction)), DirectionToLight, true, dummy);
          // Wenn wir zwar ein Primitiv Treffen, dieses allerdings hinter der Lichtquelle Liegt so kann es unseren Pixel nicht Abschatten.
          If Lengthv3(scalev3(LightLambda, DirectionToLight)) > lengthv3(subv3(newpos, Flights[i].Position)) Then Begin
            Dummy := Nil;
            LightLambda := -1;
          End;
          (************************************************************************)
          (*
          Wenn wir einen Weichen Schatten werfen sollen dann hat unser Dummy nun den Smoothshadowfaktor für uns Berechnet ;)
          *)
          SmoothShadowFactor := 0; // Beruhigt den Compiler, braucht man aber net da es nur kommt wenn SmoothShadowFactor <> 0
          SmoothShadowMode := linear; // Beruhigt den Compiler, braucht man aber net da es nur kommt wenn SmoothShadowFactor <> 0
          // Wir müssen die Position übergeben, da bei der SoftBorder ebenfalls getestet werden mus, aber halt von anderer Position
          Pruefehalbschatten(addv3(newpos, scalev3(-2 * minfloat, Direction)));
          (************************************************************************)
          (*
          Dieses Stücken Code versucht eine Weiche Schattenkante hin zu bekommen

          Dies wird momentan nur auf Gekrümmten Oberflächen erreicht, genauergesagt auf Kugeln.

          Für alle Anderen Primitive mus dann entsprechend hier und da ein klein wenig Code ingefügt und angepasst werden.
          *)
          SoftShadowFactor := 0; // Initialisieren
          //(*
          If (Primitive Is TSphere) And (LightLambda <> -1) And (Dummy = Primitive) Then Begin
            If Primitive.material.SoftshadowBorder <> 0 Then Begin
              // Wir senden einen Schattenstrahl um den Austrittpunkt aus unserem Object zu finden.
              SoftShadowLambda1 := Intersect(addv3(newpos, scalev3(2 * minfloat, Direction)), DirectionToLight, false, dummy); // es ist zwar ein Schattenstrahl aber die Berechnung des Smoothshadow ist schon vorbei
              // Wenn wir innerhalt des Bereiches sind in dem wir Soft Shadows haben wollen dann gehen wir her und versuchen diesen zu berechnen.
              If (SoftShadowLambda1 <= Tsphere(Primitive).material.SoftshadowBorder * Tsphere(primitive).Fradius) And (Dummy = Primitive) Then Begin
                // Wir Berechnen den Austrittspunkt des 2. Schattenstrahles aus unserem Object
                (*
                  Eigentlich müste da 2 * stehen und nicht 8 * aber bei 2* reicht es noch nicht nd es treten hier und da Pixelfehler auf ...
                *)
                SoftShadowPosition := addv3(addv3(newpos, scalev3(2 * minfloat, Direction)), scalev3(SoftShadowLambda1 + 8 * minfloat, DirectionToLight));
                (*
                Blöderweise müssen wir hier nochmals auf Halbschatten Prüfen
                *)
                (******************************************************************************)
                (* Einschub Prüfen ob in der Borderline zufällig ein Smoothshadow liegt.      *)
                (******************************************************************************)
                SoftShadowLambda2 := Intersect(SoftShadowPosition, DirectionToLight, True, dummy);
                If Lengthv3(scalev3(SoftShadowLambda2, DirectionToLight)) > lengthv3(subv3(SoftShadowPosition, Flights[i].Position)) Then Begin
                  Dummy2 := Nil;
                  SoftShadowLambda2 := -1;
                End;
                If SoftShadowLambda2 <> -1 Then Begin
                  Pruefehalbschatten(SoftShadowPosition);
                End;
                (******************************************************************************)
                (* Ende Einschub Prüfen auf Smoothshadow                                      *)
                (******************************************************************************)
                // von diesem Austittspunkt aus schauen wir ob dieser beleuchtet ist, nur wennn dies der Fall ist, dann brauchen wir zu schattieren
                SoftShadowLambda2 := Intersect(SoftShadowPosition, DirectionToLight, false, dummy); // es ist zwar ein Schattenstrahl aber die Berechnung des Smoothshadow ist schon vorbei
                // Erst mal schaun ob der Schnittpunkt mit dem Object vor oder hinter der Lampe ist
                // Wenn wir mit einem Object hinter Der Lampe schneiden sind wir Trotzdem noch beleuchtet.
                If Lengthv3(scalev3(SoftShadowLambda2, DirectionToLight)) > lengthv3(subv3(SoftShadowPosition, Flights[i].Position)) Then Begin
                  SoftShadowLambda2 := -1;
                  Dummy := Nil;
                End;
                // Alles Hat geklappt nun suchen wir uns den Abschwächungsfaktor
                If SoftShadowLambda2 = -1 Then Begin
                  SoftShadowFactor := SoftShadowLambda1 / (Primitive.material.SoftshadowBorder * Tsphere(primitive).Fradius);
                  SoftShadowMode := Tsphere(Primitive).material.SoftShadowMode;
                  // Wir müssen die Beleuchtung nun extra Freischalten.
                  LightLambda := -1;
                End;
              End;
            End;
          End; //*)
          // Wenn unser Pixel nicht verdeckt ist dann kann er beleuchtet werden.
          If LightLambda = -1 Then Begin
            // Dann Addieren wir den Difussen Anteil
            // Dazu Brauchen wir erstmal die Normale an dem Punkt, diese wird dann
            // mit der Strahlrichtung verglichen und berechnet den Diffusen Lichtanteil.
            // Das Ganze wird dann Letztendes mit dem Diffusen Anteil der Lichtquelle Scalliert.
            If UseCullfaceing Then Begin
              (*
              Hier fehlt evtl noch der Cullface Mode
              *)
              DiffuserAnteil := CompProdv3(scalev3(max(0, dotprodv3(NormalAtPoint, Direction)), Primitive.Material.DiffuseColor), Flights[i].Material.DiffuseColor);
            End
            Else
              DiffuserAnteil := CompProdv3(scalev3(abs(dotprodv3(NormalAtPoint, Direction)), Primitive.Material.DiffuseColor), Flights[i].Material.DiffuseColor);
            // Bei einer Texturierung des Primitiven wird die Texturfarbe noch mit Reingerechnet.
            If assigned(Primitive.Material.Texture) Then
              DiffuserAnteil := CompProdv3(DiffuserAnteil, ColorAtPoint);
            // Überblenden in den Bereich des Schattens
            If (SoftShadowFactor <> 0) Then Begin
              Case SoftShadowMode Of
                linear: DiffuserAnteil := AOverB(v3(0, 0, 0), DiffuserAnteil, SoftShadowFactor);
                Cos: DiffuserAnteil := AOverBSin(v3(0, 0, 0), DiffuserAnteil, SoftShadowFactor);
              End;
            End;
            If SmoothShadowFactor <> 0 Then Begin
              Case SmoothShadowMode Of
                linear: DiffuserAnteil := AOverB(v3(0, 0, 0), DiffuserAnteil, SmoothShadowFactor);
                Cos: DiffuserAnteil := AOverBSin(v3(0, 0, 0), DiffuserAnteil, SmoothShadowFactor);
              End;
            End;
            // Und Addieren des Diffusen anteils
            result := AddV3(result, DiffuserAnteil);
            (************************************************************************)
            // Wenn man will das das Difusse Licht nicht mit Schattenberechnung gerendert will mus diese Zeile rein und die oben raus ...
            // If LightLambda = -1 Then Begin
            ReflectionLightdir := ScaleV3(-1, SubV3(ScaleV3(2 * DotProdv3(NormalAtPoint, DirectionToLight), NormalAtPoint), DirectionToLight));
            // Erst mal die Speculare Farbe Berechnen
            SpecularerAnteil := CompProdv3(Flights[i].Material.SpecularColor, primitive.Material.SpecularColor);
            If assigned(Primitive.Material.Texture) Then
              SpecularerAnteil := CompProdv3(SpecularerAnteil, ColorAtPoint);
            // Überblenden in den Bereich des Schattens
            If SoftShadowFactor <> 0 Then Begin
              Case SoftShadowMode Of
                linear: SpecularerAnteil := AOverB(v3(0, 0, 0), SpecularerAnteil, SoftShadowFactor);
                Cos: SpecularerAnteil := AOverBSin(v3(0, 0, 0), SpecularerAnteil, SoftShadowFactor);
              End;
            End;
            If SmoothShadowFactor <> 0 Then Begin
              Case SmoothShadowMode Of
                linear: SpecularerAnteil := AOverB(v3(0, 0, 0), SpecularerAnteil, SmoothShadowFactor);
                Cos: SpecularerAnteil := AOverBSin(v3(0, 0, 0), SpecularerAnteil, SmoothShadowFactor);
              End;
            End;
            // Dann wird sie in Abhängigkeit des Exponenten Aufaddiert
            If UseCullfaceing Then Begin
              Result := Addv3(Result, scalev3(power(max(0, dotprodv3(ReflectionLightdir, Direction)), primitive.Material.HighlightExponent), SpecularerAnteil));
            End
            Else
              // So Sähe wieder Beidseitiges beleuchten aus
              Result := Addv3(Result, scalev3(power(abs(dotprodv3(ReflectionLightdir, Direction)), primitive.Material.HighlightExponent), SpecularerAnteil));
          End;
          // Wenn wir Benumpra gerechnet haben dann mus trotzdem auf Transparente Objecte Geprüft werden
          If (SoftShadowFactor <> 0) Or (SmoothShadowFactor <> 0) Then
            Lightlambda := 0;
          // Alles was beschattet wird mus auf Schatten durch Transparente Objecte geprüft werden !!
          If Lightlambda <> -1 Then Begin
            (******************************************************************************)
            (* Transparente Abschattungen !!!!                                            *)
            (******************************************************************************)
            b := True;
            c := True;
            LightedDiffuseColor := Flights[i].Material.DiffuseColor;
            LightedSpecularColor := Flights[i].Material.SpecularColor;
            // Den Starpunkt unseres Schattenstrahles Initialisieren
            LightedPosition := Newpos;
            While b Do Begin
              // Da wir nicht genau sagen können ob der Dummy Pointer sich geändert hat berechnen wir die Collision nochmals.
              LightLambda := Intersect(addv3(LightedPosition, scalev3(-2 * minfloat, Direction)), DirectionToLight, true, dummy);
              // Wenn wir zwar ein Primitiv Treffen, dieses allerdings hinter der Lichtquelle Liegt so kann es unseren Pixel nicht Abschatten.
              If Lengthv3(scalev3(LightLambda, DirectionToLight)) > lengthv3(subv3(LightedPosition, Flights[i].Position)) Then Begin
                Dummy := Nil;
                LightLambda := -1;
              End;
              If LightLambda = -1 Then
                b := False;
              // Nun da wir uns im Klaren darüber sind das wir ein anderes Primitiv schneiden berechnen wir die Durchscheinende Farbe
              If LightLambda <> -1 Then Begin
                // Berechnen der Position an der wir das schattenwerfende Primitiv treffen
                LightedPosition := addv3(LightedPosition, scalev3(Lightlambda + 2 * minfloat, DirectionToLight));
                //addv3(addv3(LightedPosition, scalev3(2 * minfloat, Direction)), scalev3(LightLambda {+ 8 * minfloat}, DirectionToLight));
                // Die Transparents des Objectes Betrachten
                LightedTransparents := dummy.TransparentsAtPoint(LightedPosition);
                // Ist das Object Tatsächlich transparent so kann geblendet werden
                If LightedTransparents <> 0 Then Begin
                  // Durch die Transparents verändern wir im Prinzip ja die Lichtquellenfarbe, diese
                  // ist nun in den Entsprechenden 2 Variablen herausgeführt und kann nun mitels der Transparents verändert und
                  // Gefiltert werden.
                  LightedColor := dummy.Material.DiffuseColor;
                  If assigned(Dummy.material.texture) Then
                    LightedColor := CompProdv3(LightedColor, dummy.ColorAtPoint(LightedPosition));
                  // Abschwächen der Lichtfarbe
                  LightedColor := ScaleV3(LightedTransparents, LightedColor);
                  // Blenden der Farbe anteilig nach Transparenz
                  LightedDiffuseColor := AOverB(LightedDiffuseColor, LightedColor, LightedTransparents);
                  LightedSpecularColor := AOverB(LightedSpecularColor, LightedColor, LightedTransparents);
                End
                Else Begin // Wenn wir auf ein Opakes Object Treffen dann kann nichts durscheinen !!
                  b := False;
                  c := False;
                End;
              End;
            End;
            If c Then Begin

              (* Da bei einer Totalen Transparents das Volle Licht durchkoommen mus initialisieren wir wie wenn kein Schatten dagewesen wäre !!*)
              LightedColor := v3(0, 0, 0);
              // Dann Addieren wir den Difussen Anteil
              // Dazu Brauchen wir erstmal die Normale an dem Punkt, diese wird dann
              // mit der Strahlrichtung verglichen und berechnet den Diffusen Lichtanteil.
              // Das Ganze wird dann Letztendes mit dem Diffusen Anteil der Lichtquelle Scalliert.
              If UseCullfaceing Then Begin
                (*
                Hier fehlt evtl noch der Cullface Mode
                *)
                DiffuserAnteil := CompProdv3(scalev3(max(0, dotprodv3(NormalAtPoint, Direction)), Primitive.Material.DiffuseColor), LightedDiffuseColor);
              End
              Else
                DiffuserAnteil := CompProdv3(scalev3(abs(dotprodv3(NormalAtPoint, Direction)), Primitive.Material.DiffuseColor), LightedDiffuseColor);
              // Bei einer Texturierung des Primitiven wird die Texturfarbe noch mit Reingerechnet.
              If assigned(Primitive.Material.Texture) Then
                DiffuserAnteil := CompProdv3(DiffuserAnteil, ColorAtPoint);
              // Überblenden in den Bereich des Schattens
              If (SoftShadowFactor <> 0) Then Begin
                Case SoftShadowMode Of
                  linear: DiffuserAnteil := AOverB(v3(0, 0, 0), DiffuserAnteil, SoftShadowFactor);
                  Cos: DiffuserAnteil := AOverBSin(v3(0, 0, 0), DiffuserAnteil, SoftShadowFactor);
                End;
              End;
              If SmoothShadowFactor <> 0 Then Begin
                Case SmoothShadowMode Of
                  linear: DiffuserAnteil := AOverB(v3(0, 0, 0), DiffuserAnteil, SmoothShadowFactor);
                  Cos: DiffuserAnteil := AOverBSin(v3(0, 0, 0), DiffuserAnteil, SmoothShadowFactor);
                End;
              End;
              // Und Addieren des Diffusen anteils
              LightedColor := AddV3(LightedColor, DiffuserAnteil);
              (************************************************************************)
              // Wenn man will das das Difusse Licht nicht mit Schattenberechnung gerendert will mus diese Zeile rein und die oben raus ...
              // If LightLambda = -1 Then Begin
              ReflectionLightdir := ScaleV3(-1, SubV3(ScaleV3(2 * DotProdv3(NormalAtPoint, DirectionToLight), NormalAtPoint), DirectionToLight));
              // Erst mal die Speculare Farbe Berechnen
              SpecularerAnteil := CompProdv3(LightedSpecularColor, primitive.Material.SpecularColor);
              If assigned(Primitive.Material.Texture) Then
                SpecularerAnteil := CompProdv3(SpecularerAnteil, ColorAtPoint);
              // Überblenden in den Bereich des Schattens
              If SoftShadowFactor <> 0 Then Begin
                Case SoftShadowMode Of
                  linear: SpecularerAnteil := AOverB(v3(0, 0, 0), SpecularerAnteil, SoftShadowFactor);
                  Cos: SpecularerAnteil := AOverBSin(v3(0, 0, 0), SpecularerAnteil, SoftShadowFactor);
                End;
              End;
              If SmoothShadowFactor <> 0 Then Begin
                Case SmoothShadowMode Of
                  linear: SpecularerAnteil := AOverB(v3(0, 0, 0), SpecularerAnteil, SmoothShadowFactor);
                  Cos: SpecularerAnteil := AOverBSin(v3(0, 0, 0), SpecularerAnteil, SmoothShadowFactor);
                End;
              End;
              // Dann wird sie in Abhängigkeit des Exponenten Aufaddiert
              If UseCullfaceing Then Begin
                LightedColor := Addv3(LightedColor, scalev3(power(max(0, dotprodv3(ReflectionLightdir, Direction)), primitive.Material.HighlightExponent), SpecularerAnteil));
              End
              Else
                // So Sähe wieder Beidseitiges beleuchten aus
                LightedColor := Addv3(LightedColor, scalev3(power(abs(dotprodv3(ReflectionLightdir, Direction)), primitive.Material.HighlightExponent), SpecularerAnteil));

              Result := Addv3(Result, LightedColor);
            End;
            //            (*             // Wenn Ein Oject im Schatten Liegt dann kann es trotzdem Beleuchtet werden, dazu bedarf es lediglich der Transparents des anderen Primitives das den Schatten wirft.da
            //                         b := True;
            //                         // Da wir nicht genau sagen können ob der Dummy Pointer sich geändert hat berechnen wir die Collision nochmals.
            //                         LightLambda := Intersect(addv3(newpos, scalev3(-2 * minfloat, Direction)), DirectionToLight, true, dummy);
            //
            //                         // Wenn wir zwar ein Primitiv Treffen, dieses allerdings hinter der Lichtquelle Liegt so kann es unseren Pixel nicht Abschatten.
            //                         If Lengthv3(scalev3(LightLambda, DirectionToLight)) > lengthv3(subv3(newpos, Flights[i].Position)) Then Begin
            //                           Dummy := Nil;
            //                           LightLambda := -1;
            //                         End;
            //                         If LightLambda <> -1 Then Begin // Eigentlich wissen wir beim aller ersten durchlauf das Lightlambda <> -1 ist, aber der übersichtlickeit halber bleibts mal drin
            //                           // Berechnen der Position an der wir das schattenwerfende Primitiv treffen
            //                           LightedPosition := addv3(addv3(newpos, scalev3(2 * minfloat, Direction)), scalev3(LightLambda {+ 8 * minfloat}, DirectionToLight));
            //// Die Transparents des Objectes Betrachten
            //LightedTransparents := dummy.TransparentsAtPoint(LightedPosition);
            //// Nur wenn dieses Object überhaupt Transparent ist haben wir eine Chance
            //If LightedTransparents <> 0 Then Begin
            //  // Wir Starten mit der Diffusen Farbe unseres Primitives und Blenden dann die Diffusen Farbanteile der anderen drüber.
            //  LightedColor := CompProdv3(Primitive.Material.DiffuseColor, Flights[i].Material.DiffuseColor);
            //  // Bei einer Texturierung des Primitiven wird die Texturfarbe noch mit Reingerechnet.
            //  If assigned(Primitive.Material.Texture) Then
            //    LightedColor := CompProdv3(LightedColor, ColorAtPoint);
            //  // Wir Berechnen den Farbwert des Dummys an dieser Position
            //  dummycolor := dummy.Material.DiffuseColor;
            //  If assigned(dummy.Material.texture) Then
            //    dummycolor := CompProdv3(dummycolor, dummy.ColorAtPoint(LightedPosition));
            //  LightedColor := AOverB(LightedColor, dummycolor, 1 - LightedTransparents);
            //
            //  //  LightedShadow:TRayFloat;
            //  //  LightedDistance:TRayFloat;
            //  //  LightedTransparents:TRayFloat;
            ////            LightedColor := Primitive.Material.DiffuseColor;
            ////            if assigned(
            //
            //  //            if dummy.TransparentsAtPoint
            //
            //  Der Plan ist nun so lange In Richtung der Lichquelle zu gehen und die Farbwerte der Tranparenten Objecte zu Blenden bis wir an der
            //    Lichtquelle angelangt sind.Treffen wir dazwischen irgendwann mal auf ein Nicht Transparentes Primitiv können wir frühzeitig abbrechen.
            //
            //  While b Do Begin
            //
            //    //              If Lengthv3(scalev3(LightLambda, DirectionToLight)) > lengthv3(subv3(SmoothshadowPosition, Flights[i].Position)) Then Begin
            //
            //  End;
            //End;
            //End; // *)
          End;
        End;

      // End;
       (*
       Ab Hier wissen wir nun Definitiv welche Farbe unser Punkt hat.
       Nun können wir Sachen wie Transparentz, Reflectanz ...
       Reinrechnen.
       *)
      If primitive.Material.Reflectance > 0 Then Begin
        // Bei Reflektierenden oberflächen mus die Rekursion weitergehn.
        // Dies wird gemacht in dem wir uns Rekursiv wieder aufrufen und dabei
        // die Neue Richtung einschlagen.
        Reflectiondir := normalizev3(subv3(direction, scalev3(2 * dotprodv3(direction, normalatPoint), Normalatpoint)));
        // Damit wir nicht in unserem element hängen bleiben verschieben wir die neue pos um ein winzug kleines Stückchen.
        result := AOverB(Raytrace(depth + 1, addv3(newpos, scalev3(2 * minfloat, Reflectiondir)), Reflectiondir, Distance + Lambda), result, primitive.Material.Reflectance);
      End;
      // Wir Berechnen die Transparents des Punktes so können
      TransparentsAtPoint := Primitive.TransparentsAtPoint(newpos);
      // Haben wir ein transparentes Material mus weiter Raytraced werden.
      If TransparentsAtPoint > 0 Then Begin
        // Blending der Transparenten Farbe
        result := AoverB(Raytrace(depth + 1, addv3(newpos, scalev3(2 * MinFloat, direction)), direction, Distance + Lambda), result, TransparentsAtPoint);
      End;
      (* Einrechnen des Nebels *)
      If usefog Then Begin
        If Distance + Lambda >= fogmin Then Begin
          If Distance + Lambda <= fogmax Then Begin
            Case FogMode Of
              Linear: Result := AOverB(FogColor, result, (Distance + Lambda - fogmin) / (Fogmax - Fogmin));
              Cos: Result := AOverBsin(FogColor, result, (Distance + Lambda - fogmin) / (Fogmax - Fogmin));
            End;
          End
          Else
            Result := FogColor;
        End;
      End;
      (* Ende Farbberechnung *)
    End
    Else Begin // Wenn kein Primitive mehr geschnitten wird
      // Bei Nebel bedeutet das Sehn ins Unendliche = Schaun in den Nebel
      If UseFog Then
        result := FogColor
      Else
        Result := BackgroundColor;
    End;
  End;
End;

Procedure TRaytracer.Render(Const Canvas: TCanvas; Width, Height: Integer);
Var
  Viewrect: Array[0..5] Of Tvector3f;
  AspectRatio: TRayFloat;
  Position: TVector3f;
  Direction: TVector3f;
  T1: TVector3f; // Temp Variable zum Berechnen des Sichtfensters
  Color: TVector3f;
  x, y: integer;
  c, b: Boolean; // Schmiermerker für die Exception sachen
  tmpvalue: TRayFloat;
  diff: TVector2f;
  // Wir drücken uns vor Canvas.Pixels[,] damit wird das Rendern deutlich schneller
  temp: tbitmap;
  R: Trect;
  TempIntfImg: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
  CurColor: TFPColor;
Begin
  Diff := subv2(FTopRight, fbuttomleft);
  MaxusedRecursionDeth := 0;
  b := True;
  c := False;
  For x := 0 To High(Flights) Do
    If Flights[x].IsActive Then Begin
      b := False;
      break;
    End;
  For x := 0 To high(Felements) Do
    If Not SameV3(FElements[x].Material.EmissionColor, v3(0, 0, 0)) Then Begin
      c := True;
      break;
    End;
  If ((High(Flights) = -1) Or b) And Not c Then
    Raise Exception.create('Error there are no active lights definied.');
  If (High(Felements) = -1) Then
    Raise Exception.create('Error there are no elements definied.');
  If (width <= 0) Or (height <= 0) Then
    Raise Exception.create('Error invalid render dimensions.');
  If UseFog Then
    If FogMin >= FogMax Then
      Raise Exception.create('Error invalid Fog settings.');
  temp := TBitmap.create;
  temp.PixelFormat := pf24bit;
  temp.width := width;
  temp.height := height;
  TempIntfImg := TLazIntfImage.Create(0, 0);
  TempIntfImg.LoadFromBitmap(temp.Handle, temp.MaskHandle);

  // Erst mal berechnen wir das Quad das nachher unserem Sichtfeld entspricht
  AspectRatio := Width / Height;
  // Unser Sichtfeld wird immer auf das Rect( -AspectRatio, +1, +AspectRatio, -1) gezogen
  T1 := ScaleV3(Aspectratio, CrossProdV3(FEyeDir, FEyeUp));
  // Links Oben
  Viewrect[4] := AddV3(FEyeUp, SubV3(Feyepos, T1));
  // Rechts Unten
  Viewrect[5] := AddV3(scalev3(-1, FEyeUp), AddV3(Feyepos, T1));
  // Der Aufpunkt für unsere Screen Quad
  Viewrect[0] := SubV3(Viewrect[4], scaleV3(2, FeyeUp));
  // Der U - Vector ( Y - Scallierung )
  Viewrect[1] := Subv3(Viewrect[4], Viewrect[0]);
  // Der V - Vector ( X - Scallierung
  Viewrect[2] := Subv3(Viewrect[5], Viewrect[0]);
  // Berechnen der Echten Eyepos diese ist je nach Öffnungswinkel des Objectives weiter weg oder net
  // Hier haben wir mal einen Starren Öffnungswinkel von 90° reinprogrammiert
  // Will man den Öffnungswinkel korreckt einbaun mus der -1 Term entsprechend geändert werden.
  // 0 => entspricht Öffnungswinkel = 180° ( unmöglich !!!!! , da dann sonst das Auge in der Bildebene wäre und nur Blödsinnige directions berechnet würden.)
  // -1 => entspricht Öffnungswinkel = 90°
  // -2 => entspricht Öffnungswinkel = 60°
  Viewrect[3] := Addv3(FEyepos, scalev3(-1, FEyeDir));
  // Rendern des Images , wir senden durch jeden Pixel einen Strahl
  For y := 0 To height - 1 Do Begin
    //    Row := Temp.ScanLine[height - y - 1];
    //    inc(row, width - 1);
    For x := 0 To width - 1 Do Begin
      // Berechnen der Strahlrichtung
      If UseOrthoProjection Then Begin
        direction := v3(0, 0, fz);
        position := addv3(v3(FButtomLeft.x, Fbuttomleft.y, 0), v3(diff.x * (width - 1 - x) / width, diff.y * y / height, 0));
      End
      Else Begin
        // wir benutzen OpenGL Farbeigenschaften, d.h. Werte in [0 .. 1]
        Position := addv3(Viewrect[0], addv3(scalev3(x / width, Viewrect[2]), scalev3(y / Height, Viewrect[1])));
        direction := normalizev3(subv3(position, Viewrect[3]));
      End;
      // Berechnen der Farben des Pixels
      Color := Raytrace(0, Position, Direction, 0);
      // Ganz am Schluss Wollen wir noch Alles zu Helle auf Weis Abbilden
      // So liegt unsere Farbe definitiv wieder in [ 0 .. 1 ]
      Case LightOverflowMode Of
        clamp: Begin
            Color.x := Min(Color.x, 1);
            Color.y := Min(Color.y, 1);
            Color.z := Min(Color.z, 1);
          End;
        Scale: Begin
            tmpvalue := max(Color.x, max(Color.y, Color.z));
            If tmpvalue > 1 Then
              Color := ScaleV3(1 / tmpvalue, Color);
          End;
      End;
      // Schreiben der Ergebnissfarbe in das Bitmap
      // Sieht hier Kryptisch aus, ist so aber am schnellsten
      CurColor.red := round(min(255, max(0, color.x * 255))) Shl 8;
      CurColor.green := round(min(255, max(0, color.y * 255))) Shl 8;
      CurColor.Blue := round(min(255, max(0, color.z * 255))) Shl 8;
      TempIntfImg.Colors[x, height - y - 1] := CurColor;
    End;
  End;
  TempIntfImg.CreateBitmaps(ImgHandle, ImgMaskHandle, false);
  temp.Handle := ImgHandle;
  temp.MaskHandle := ImgMaskHandle;
  TempIntfImg.free;
  // Normalerweise wird ja -1 gemacht, aber da Copyrect genau wie bei OpenGL Arbeitet
  // müssen wir einen Pixel mehr kopieren
  r := rect(0, 0, width {- 1}, height {- 1});
  canvas.CopyRect(R, temp.canvas, R);
  temp.free;
End;

Procedure TRaytracer.SetEye(Position, Up, Direction: Tvector3f);
Begin
  Feyepos := Position;
  FeyeUp := normalizev3(UP);
  FEyeDir := normalizev3(Direction);
End;

Procedure TRaytracer.SetOrtho(ButtomLeft, TopRight: TVector2f; Z: TRayFloat);
Begin
  FButtomleft := Buttomleft;
  Ftopright := Topright;
  If z >= 0 Then
    Fz := 1
  Else
    Fz := -1;
End;

Procedure TRaytracer.LookAt(Position, Center, Up: Tvector3f);
Begin
  Feyepos := Position;
  FeyeUp := normalizev3(UP);
  FEyeDir := normalizev3(subv3(Center, position));
End;

Function TRaytracer.FGetPrimitiveCount: integer;
Begin
  result := high(FElements) + 1;
End;

Procedure TRaytracer.SetRekursionDepth(value: integer);
Begin
  If Value >= 0 Then
    FRekursionDepth := value
  Else
    FRekursionDepth := 1;
End;

{ TLight }

Constructor TLight.create;
Begin
  Inherited;
  name := 'TLight';
  isactive := True;
  Material.SpecularColor := v3(1, 1, 1);
  Material.DiffuseColor := v3(1, 1, 1);
  Material.AmbientColor := v3(1, 1, 1);
End;

Destructor TLight.Destroy;
Begin
  Inherited;
End;

{ TSphere }

Constructor TSphere.create;
Begin
  Inherited;
  name := 'TSphere';
  Fposition := v3(0, 0, 0);
  Fradius := 0;
  FSP := v3(0, 1, 0);
  FSE := v3(0, 0, 1);
End;

Destructor TSphere.destroy;
Begin
  Inherited;
End;

Function TSphere.Define(Position: TVector3f; Radius: TRayFloat): Boolean;
Begin
  If Radius > 0 Then Begin
    Fposition := Position;
    Fradius := Radius;
    FSP := AddV3(Position, v3(0, 1, 0));
    FSE := AddV3(Position, v3(0, 0, 1));
    result := True;
  End
  Else
    result := False;
End;

Function TSphere.GetUV(Value: TVector3f): TVector2f;
Var
  tmp, u, v, phi, theta: TRayFloat;
  sn, se, sp: TVector3f;
Begin
  // Umrechnen auf Lokales Koordinatensystem
  sn := NormalizeV3(subv3(value, fposition));
  sp := SubV3(fsp, fposition);
  se := SubV3(fse, fposition);
  // Numerische Instabilitäten abfangen
  tmp := -DotProdv3(sn, sp);
  tmp := min(1, max(-1, tmp));
  phi := arccos(tmp);
  v := phi / pi;
  // Numerische Instabilitäten abfangen
  tmp := DotProdv3(se, sn) / sin(phi);
  tmp := min(1, max(-1, tmp));
  theta := (arccos(tmp) / (2 * pi));
  If DotProdv3(CrossProdV3(sp, se), sn) > 0 Then
    u := theta
  Else
    u := 1 - theta;
  result.x := u;
  result.y := v;
End;

Function TSphere.ColorAtPoint(value: TVector3f): Tvector3f;
Var
  uv: TVector2f;
Begin
  //Irgendwas stimmt noch net ...
  If assigned(material.texture) Then Begin
    uv := GetUV(Value);
    Result := Interpolate(material.Texture, 1 - uv.x, 1 - uv.y, material.TexInterpolationMode, material.TexParameter);
  End
  Else
    result := Material.AmbientColor;
End;

(*
Die hier genutze version von Kugel Geradenschnitt ist von

http://www.devmaster.net/wiki/Ray-sphere_intersection

sie functioniert nur weil Direction normiert ist !!

bzw leider ist diese Optimierte Version nur Tauglich wenn der Strahl von ausen kommt

*)
(*
Function TSphere.Intersect(Position, Direction: TVector3f): TRayFloat;
Var
  dst: TVector3f;
  b, c, d: TRayFloat;
Begin
  dst := subv3(position, Fposition);
  B := dotprodv3(dst, direction);
  C := dotprodv3(dst, dst) - sqr(Fradius);
  D := sqr(B) - C;
  If d > 0 Then Begin
    result := -B - sqrt(D);
  End
  Else
    Result := -1;
End; //*)

(*
So wars bisher, geht auch, ist aber halt viel mehr Rumgerechne ...
*)
(*
Function TSphere.Intersect(Position, Direction: TVector3f): TRayFloat;
Var
  l1, l2, diskriminante, {a,} b, c: TRayFloat;
Begin
  Direction := scalev3(-1, Direction);
  result := -1;
  // Da Direction Normiert ist können wir uns die Berechnung sparen a wird immer 1 sein !!!
  //  a := sqr(Direction.x) + sqr(Direction.y) + sqr(Direction.z);
  b := 2 * (direction.x * (Fposition.x - Position.x) + direction.y * (Fposition.y - Position.y) + direction.z * (Fposition.z - Position.z));
  c := sqr(Position.x) + sqr(Position.y) + sqr(Position.z) - 2 * (Position.x * Fposition.x + Position.y * Fposition.y + Position.z * Fposition.z) + sqr(Fposition.x) + sqr(Fposition.y) + sqr(Fposition.z) - sqr(Fradius);
  diskriminante := sqr(b) - 4 {* a} * c;
  If diskriminante >= 0 Then Begin
    diskriminante := sqrt(diskriminante);
    l1 := (-b + diskriminante) / 2 {* a};
    l2 := (-b - diskriminante) / 2 {* a};
    If l1 < 0 Then l1 := MaxFloat;
    If l2 < 0 Then l2 := MaxFloat;
    result := min(l1, l2);
    If Result = MaxFloat Then result := -1;
  End;
End;
// *)

Function TSphere.Intersect(Position, Direction: TVector3f; Schattenstrahl: Boolean): TRayFloat;
Var
  p1, p2: TVector3f;
  len, l1t, l2t, l1, l2, diskriminante, b, c: TRayFloat;
Begin
  Direction := scalev3(-1, Direction);
  result := -1;
  //  Da Direction Normiert ist, können wir uns die Berechnung sparen a wird immer 1 sein !!!
  //  a := sqr(Direction.x) + sqr(Direction.y) + sqr(Direction.z);
  b := 2 * (direction.x * (Fposition.x - Position.x) + direction.y * (Fposition.y - Position.y) + direction.z * (Fposition.z - Position.z));
  If (Material.SmoothShadow = 0) Or Not (Schattenstrahl) Then Begin
    c := sqr(Position.x) + sqr(Position.y) + sqr(Position.z) - 2 * (Position.x * Fposition.x + Position.y * Fposition.y + Position.z * Fposition.z) + sqr(Fposition.x) + sqr(Fposition.y) + sqr(Fposition.z) - sqr(Fradius);
    diskriminante := sqr(b) - 4 {* a} * c;
    If diskriminante >= 0 Then Begin
      diskriminante := sqrt(diskriminante);
      l1 := (-b + diskriminante) / 2 {* a};
      l2 := (-b - diskriminante) / 2 {* a};
      If l1 < 0 Then l1 := MaxFloat;
      If l2 < 0 Then l2 := MaxFloat;
      result := min(l1, l2);
      If Result = MaxFloat Then result := -1;
    End;
  End
  Else Begin
    // Wenn eine Smooth Schattenkante Berechnet werden mus dann wirds aufwendiger
    c := sqr(Position.x) + sqr(Position.y) + sqr(Position.z) - 2 * (Position.x * Fposition.x + Position.y * Fposition.y + Position.z * Fposition.z) + sqr(Fposition.x) + sqr(Fposition.y) + sqr(Fposition.z) - sqr(Fradius + material.SmoothShadow * Fradius);
    diskriminante := sqr(b) - 4 {* a} * c;
    // Zurücksetzen alter Werte
    SmoothShadowFactor := 0;
    If diskriminante >= 0 Then Begin
      diskriminante := sqrt(diskriminante);
      l1 := (-b + diskriminante) / 2 {* a};
      l2 := (-b - diskriminante) / 2 {* a};
      l1t := l1;
      l2t := l2;
      If l1 < 0 Then l1 := MaxFloat;
      If l2 < 0 Then l2 := MaxFloat;
      result := min(l1, l2);
      If Result = MaxFloat Then result := -1;
      // Wenn Wir einen Schnitt hatten dann müssen wir ausrechnen ob Kernschatten oder nur Halbschatten
      If Result <> -1 Then Begin
        // Wir berechnen die 2 Schnittpunkte
        p1 := addv3(position, scalev3(-l1t, direction));
        p2 := addv3(position, scalev3(-l2t, direction));
        len := lengthv3(subv3(Fposition, scalev3(0.5, addv3(p1, p2))));
        If len > (fradius + 2 * minfloat) Then Begin // Wir sind im Halbschatten
          len := len - Fradius;
          SmoothShadowFactor := 1 - (len / (material.SmoothShadow * Fradius));
          // Wir sind zwar im Halbschatten, aber dadurch das unser Dummy nicht zurückgegeben wird wenn wir -1 zurückgeben müssen wir
          // leider die entsprechenden Result werte Beibehalten.
          // Result := -1;
        End
        Else Begin // Wir sind im Kernschatten
          // Im Kernschatten müssen wir ja das Korreckte Lambda berechnen , deswegen geht die Rechnerei noch mal von Vorne Los
          c := sqr(Position.x) + sqr(Position.y) + sqr(Position.z) - 2 * (Position.x * Fposition.x + Position.y * Fposition.y + Position.z * Fposition.z) + sqr(Fposition.x) + sqr(Fposition.y) + sqr(Fposition.z) - sqr(Fradius);
          diskriminante := sqr(b) - 4 {* a} * c;
          If diskriminante >= 0 Then Begin
            diskriminante := sqrt(diskriminante);
            l1 := (-b + diskriminante) / 2 {* a};
            l2 := (-b - diskriminante) / 2 {* a};
            If l1 < 0 Then l1 := MaxFloat;
            If l2 < 0 Then l2 := MaxFloat;
            result := min(l1, l2);
            If Result = MaxFloat Then result := -1;
          End;
        End;
      End;
    End;
  End;
End;

(*
Wie schön das die Normale Mit Kugeln so leicht zu berechnen ist ;)
Wenn man weis das der Punkt element der Kugel ist !!
*)

Function TSphere.NormalAtPoint(Value: TVector3f): TVector3f;
Begin
  result := normalizev3(subv3(fposition, Value));
End;

Procedure TSphere.Translate(Value: TVector3f);
Begin
  Fposition := addv3(fposition, value);
  fse := addV3(fse, value);
  fsp := addV3(fsp, value);
End;

Procedure TSphere.Rotate(Angle, dx, dy, dz: TRayFloat);
Var
  RotV: TVector3f;
Begin
  RotV := v3(dx * Angle, dy * Angle, dz * Angle);
  Fposition := RotateV3(Fposition, Rotv);
  Fsp := RotateV3(Fsp, Rotv);
  Fse := RotateV3(Fse, Rotv);
End;

{ TTriangle }

Constructor TTriangle.create;
Begin
  Inherited;
  name := 'TTriangle';
  Fpoints[0] := v3(0, 0, 0);
  Fpoints[1] := v3(1, 0, 0);
  Fpoints[2] := v3(1, 0, 1);
  TextureCoord[0] := v2(0, 1);
  TextureCoord[1] := v2(0, 0);
  TextureCoord[2] := v2(1, 0);
End;

Destructor TTriangle.destroy;
Begin
  Inherited;
End;
{
(*
Hier ist einfach nur

http://de.wikipedia.org/wiki/Heronsche_Formel

implementiert.
*)

Function TTriangle.Heron(Point1, Point2, Point3: TVector3f): TRayFloat;
Var
s, a, b, c: TRayFloat;
Begin
// Die Längen der Dreieckskanten berechnen
a := lengthv3(subv3(Point2, point1));
b := lengthv3(subv3(Point3, point2));
c := lengthv3(subv3(Point1, point3));
s := (a + b + c) / 2;
// Wir verhindern Negative Wurzeln, in Manch seltenen Fällen addiert sich die Rechnerungenauigkeit leider dazu auf.
s := max(0, s * (s - a) * (s - b) * (s - c));
result := sqrt(s);
End;     }

(*
Rechnet die Baryzentrischen Coordinaten zu einem Punkt aus der in der selben Ebene liegt-

Another way to think of barycentric coordinates is by the relative areas of the subtriangles
defined by the intersection point and the triangle vertices.

        1         If the area of triangle 123 is A, then the area of
       /|\        P23 is rA.  Area 12P is sA and area 1P3 is tA.
      / | \       With this image, it is obvious that r+s+t must equal
     /  |  \      one.  If r, s, or t go outside the range zero to one,
    / t | s \     P will be outside the triangle.
   /  _-P-_  \
  / _-     -_ \
 /_-    r    -_\
2---------------3

By using the above are relationships, the following equations define r, s and t.

        N = triangle normal = (vec(1 2) cross vec(1 3))
            (vec(1 P) cross vec(1 3)) dot N
        s = -------------------------------
                     (length N)^2
            (vec(1 2) cross vec(1 P)) dot N
        t = -------------------------------
                     (length N)^2
        r = 1 - (s + t)
*)

Function TTriangle.Baryzent(Value: TVector3f): TVector3f;
Var
  v0_V: TVector3f;
Begin
  v0_V := subv3(Value, Fpoints[0]);
  result.y := DotProdv3(crossprodv3(v0_V, Fv0_2), Fn) * Flennsqr;
  result.z := DotProdv3(crossprodv3(Fv0_1, v0_V), Fn) * Flennsqr;
  result.x := 1 - (result.y + Result.z);
End;

Function TTriangle.GetUV(Value: TVector3f): TVector2f;
Var
  bar: TVector3f;
Begin
  bar := Baryzent(Value);
  result := addv2(addv2(ScaleV2(bar.x, texturecoord[0]), ScaleV2(bar.y, texturecoord[1])), ScaleV2(bar.z, texturecoord[2]));
End;

Function TTriangle.ColorAtPoint(value: TVector3f): Tvector3f;
Var
  p: TVector2f;
Begin
  If material.texture <> Nil Then Begin
    If Flennsqr <> 0 Then Begin
      //      bar := Baryzent(Value);
      p := GetUV(value); //addv2(addv2(ScaleV2(bar.x, texturecoord[0]), ScaleV2(bar.y, texturecoord[1])), ScaleV2(bar.z, texturecoord[2]));
      Result := Interpolate(material.Texture, p.x, (1 - p.y), material.TexInterpolationMode, material.TexParameter);
    End
    Else
      result := Material.AmbientColor;
  End
  Else Begin
    result := Material.AmbientColor;
  End;
End;

Procedure TTriangle.CalculateSmoothShadows;
Var
  i: Integer;
  tmp, m: TVector3f;
Begin
  If Material.SmoothShadow <> 0 Then Begin
    // Berechnen des Schwerpunktes
    m := scalev3(1 / 3, addv3(Fpoints[0], addv3(Fpoints[1], Fpoints[2])));
    // Wir Berechnen die Nach ausen Verschobeben Punkte
    For i := 0 To 2 Do Begin
      tmp := subv3(Fpoints[i], m);
      tmp := ScaleV3(1 + Material.SmoothShadow, tmp);
      SmoothshadowPoints[i] := addv3(m, tmp);
    End;
  End;
End;

Procedure TTriangle.Define(Point1, Point2, Point3: TVector3f);
Begin
  Fpoints[0] := Point1;
  Fpoints[1] := Point2;
  Fpoints[2] := Point3;
  Fnormal := normalizev3(CrossProdv3(subv3(FPoints[1], FPoints[0]), subv3(FPoints[2], FPoints[0])));
  Fn := CrossProdV3(subv3(fpoints[1], fpoints[0]), subv3(Fpoints[2], Fpoints[0]));
  Flennsqr := lengthv3sqr(Fn);
  If Flennsqr <> 0 Then Flennsqr := 1 / Flennsqr;
  Fv0_1 := subv3(Fpoints[1], Fpoints[0]);
  Fv0_2 := subv3(Fpoints[2], Fpoints[0]);
End;

{
Function Heron(p1, p2, p3: TVector3f): TRayFloat;
Var
  s, a, b, c: TRayFloat;
Begin
  a := lengthv3(subv3(p1, p2));
  b := lengthv3(subv3(p1, p3));
  c := lengthv3(subv3(p3, p2));
  s := (a + b + c) / 2;
  result := sqrt(max(s * (s - a) * (s - b) * (s - c), 0));
End;
}

Function getLambda(o1, d1, o2, d2: TVector3f): TRayFloat;
Var
  o1_v2, d1_v2, o2_v2, d2_v2: TVector2f;
  ldet, gesdet: TRayFloat;
Begin
  // Umrechenn in R2
  o1_v2.x := o1.x;
  o1_v2.y := o1.z;
  o2_v2.x := o2.x;
  o2_v2.y := o2.z;
  d1_v2.x := d1.x;
  d1_v2.y := d1.z;
  d2_v2.x := d2.x;
  d2_v2.y := d2.z;

  gesdet := DetV2(d1_v2, ScaleV2(-1, d2_v2));
  If gesdet = 0 Then Begin
    result := -1;
    exit;
  End;
  ldet := detv2(subv2(o2_v2, o1_v2), ScaleV2(-1, d2_v2));
  Result := ldet / gesdet;
End;

Function GetAbstand(Value, O, D: TVector3f): TRayFloat;
Var
  l, t1, t2, t3: TRayFloat;
  p: TVector3f;
Begin
  result := 0;
  t1 := DotProdv3(d, o);
  t2 := DotProdv3(d, d);
  t3 := DotProdv3(Value, d);
  If t2 <> 0 Then Begin
    l := (-t1 + t3) / t2;
    p := addv3(o, scalev3(l, d));
    result := LengthV3(subv3(p, Value));
  End;
End;

Function Baryzent2(Value: TVector3f; Points: Array Of TVector3f): TVector3f;
Var
  v0_V: TVector3f;
  Fn: TVector3f;
  Flennsqr: TRayFloat;
  Fv0_1, Fv0_2: TVector3f;
Begin
  fn := CrossProdV3(subv3(points[1], points[0]), subv3(points[2], points[0]));
  Flennsqr := lengthv3sqr(Fn);
  If Flennsqr <> 0 Then Flennsqr := 1 / Flennsqr;
  Fv0_1 := subv3(points[1], points[0]);
  Fv0_2 := subv3(points[2], points[0]);
  v0_V := subv3(Value, points[0]);
  result.y := DotProdv3(crossprodv3(v0_V, Fv0_2), Fn) * Flennsqr;
  result.z := DotProdv3(crossprodv3(Fv0_1, v0_V), Fn) * Flennsqr;
  result.x := 1 - (result.y + Result.z);
End;

Function TTriangle.Intersect(Position, Direction: TVector3f; Schattenstrahl: Boolean): TRayFloat;
Var
  d0, d1, d2, m, Collpos, erg, v1_0, v1_2, v0_1, v0_2: TVector3f;
  mn, u, v, Det3x3: TRayFloat;
Begin
  result := -1;
  If (Material.SmoothShadow = 0) Or Not Schattenstrahl Then Begin
    (*
    Wir Berechnen 2 Paralellogramme, nur wenn unsere Gerade mit beiden Schneidet dann liegen wir im Dreieck.
    *)
    // 1. Parallelogramm Vektor 0-1 und 0-2
    // Wir müssen das LGS Lösen das geht am Einfachsten mit Cramersche Regel
    direction := Scalev3(-1, direction);
    v0_1 := SubV3(fpoints[1], fpoints[0]);
    v0_2 := SubV3(fpoints[2], fpoints[0]);
    // Erst mal die Gesammt determinante
    det3x3 := Determinante3x3(v0_1, v0_2, Direction);
    If det3x3 <> 0 Then Begin
      // den Ergebnissvektor berechnen
      Erg := subv3(Position, Fpoints[0]);
      // Dann die Determinante für den X - Anteil
      v := Determinante3x3(erg, v0_2, Direction) / Det3x3;
      // Nur wenn der Schnittpunkt im Quad liegt müssen wir weiterrechnen
      If (V >= 0) And (v <= 1.0) Then Begin
        // Selbes Spiel nun für den Y - Anteil
        u := Determinante3x3(v0_1, erg, Direction) / det3x3;
        // Wenn das Geklappt hat dann mus nun noch das 2. Parallelogramm  1-0 und 1-2 geprüft werden.
        If (u >= 0) And (u <= 1.0) Then Begin
          v1_0 := SubV3(fpoints[0], fpoints[1]);
          v1_2 := SubV3(fpoints[2], fpoints[1]);
          // Erst mal die Gesammt determinante
          det3x3 := Determinante3x3(v1_0, v1_2, Direction);
          // den Ergebnissvektor berechnen
          Erg := subv3(Position, Fpoints[1]);
          // Dann die Determinante für den X - Anteil
          v := Determinante3x3(erg, v1_2, Direction) / Det3x3;
          If (V >= 0) And (v <= 1.0) Then Begin
            // Selbes Spiel nun für den Y - Anteil
            u := Determinante3x3(v1_0, erg, Direction) / det3x3;
            If (u >= 0) And (u <= 1.0) Then Begin
              result := Determinante3x3(v1_0, v1_2, erg) / det3x3;
            End;
          End;
        End;
      End;
    End;
  End
  Else Begin // Bei einer Weichen Schattenkannte gehts hier Rund
    SmoothShadowFactor := 0;
    (*
    Wir Berechnen 2 Paralellogramme, nur wenn unsere Gerade mit beiden Schneidet dann liegen wir im Dreieck.
    *)
    // 1. Parallelogramm Vektor 0-1 und 0-2
    // Wir müssen das LGS Lösen das geht am Einfachsten mit Cramersche Regel
    direction := Scalev3(-1, direction);
    v0_1 := SubV3(SmoothshadowPoints[1], SmoothshadowPoints[0]);
    v0_2 := SubV3(SmoothshadowPoints[2], SmoothshadowPoints[0]);
    // Erst mal die Gesammt determinante
    det3x3 := Determinante3x3(v0_1, v0_2, Direction);
    If det3x3 <> 0 Then Begin
      // den Ergebnissvektor berechnen
      Erg := subv3(Position, SmoothshadowPoints[0]);
      // Dann die Determinante für den X - Anteil
      v := Determinante3x3(erg, v0_2, Direction) / Det3x3;
      // Nur wenn der Schnittpunkt im Quad liegt müssen wir weiterrechnen
      If (V >= 0) And (v <= 1.0) Then Begin
        // Selbes Spiel nun für den Y - Anteil
        u := Determinante3x3(v0_1, erg, Direction) / det3x3;
        // Wenn das Geklappt hat dann mus nun noch das 2. Parallelogramm  1-0 und 1-2 geprüft werden.
        If (u >= 0) And (u <= 1.0) Then Begin
          v1_0 := SubV3(SmoothshadowPoints[0], SmoothshadowPoints[1]);
          v1_2 := SubV3(SmoothshadowPoints[2], SmoothshadowPoints[1]);
          // Erst mal die Gesammt determinante
          det3x3 := Determinante3x3(v1_0, v1_2, Direction);
          // den Ergebnissvektor berechnen
          Erg := subv3(Position, SmoothshadowPoints[1]);
          // Dann die Determinante für den X - Anteil
          v := Determinante3x3(erg, v1_2, Direction) / Det3x3;
          If (V >= 0) And (v <= 1.0) Then Begin
            // Selbes Spiel nun für den Y - Anteil
            u := Determinante3x3(v1_0, erg, Direction) / det3x3;
            If (u >= 0) And (u <= 1.0) Then Begin
              // Nun da wir sicher sind das wir unser Dreieck schneiden gilt es raus zu bekommen ob wir es im Kern oder Halbschatten schneiden
              // Am Besten geht das in dem wir die Baryzentischen Koordinaten des Schnittpunktes ausrechnen.
              result := Determinante3x3(v1_0, v1_2, erg) / det3x3;
              //  Collpos := AddV3(position, scalev3(-result, direction));
              // Berechnen des Schnittpunktes mit dem Dreick
              mn := Intersect(Position, Scalev3(-1, direction), false);
              // Wenn wir das Große Dreieck Schneiden, das Kleine Aber nicht.
              If mn = -1 Then Begin
                Collpos := AddV3(position, scalev3(-result, direction));
                m := Baryzent2(collpos, Fpoints);
                d0 := SubV3(Fpoints[1], Fpoints[0]);
                d1 := SubV3(Fpoints[2], Fpoints[1]);
                d2 := SubV3(Fpoints[0], Fpoints[2]);
                If (m.x < 0) And (m.y < 0) Then Begin
                  SmoothShadowFactor := 1 - 3 * GetAbstand(collpos, Fpoints[1], d1) / material.SmoothShadow;
                  SmoothShadowFactor := min(SmoothShadowFactor, 1 - 3 * GetAbstand(collpos, Fpoints[2], d2) / material.SmoothShadow);
                End;
                If (m.x < 0) And (m.z < 0) Then Begin
                  SmoothShadowFactor := 1 - 3 * GetAbstand(collpos, Fpoints[0], d0) / material.SmoothShadow;
                  SmoothShadowFactor := min(SmoothShadowFactor, 1 - 3 * GetAbstand(collpos, Fpoints[1], d1) / material.SmoothShadow);
                End;
                If (m.y < 0) And (m.z < 0) Then Begin
                  SmoothShadowFactor := 1 - 3 * GetAbstand(collpos, Fpoints[0], d0) / material.SmoothShadow;
                  SmoothShadowFactor := min(SmoothShadowFactor, 1 - 3 * GetAbstand(collpos, Fpoints[2], d2) / material.SmoothShadow);

                End;
                If (m.x < 0) And (m.y >= 0) And (m.z >= 0) Then Begin
                  SmoothShadowFactor := 1 - 3 * GetAbstand(collpos, Fpoints[1], d1) / material.SmoothShadow;
                End;
                If (m.x >= 0) And (m.y < 0) And (m.z >= 0) Then Begin
                  SmoothShadowFactor := 1 - 3 * GetAbstand(collpos, Fpoints[2], d2) / material.SmoothShadow;
                End;
                If (m.x >= 0) And (m.y >= 0) And (m.z < 0) Then Begin
                  SmoothShadowFactor := 1 - 3 * GetAbstand(collpos, Fpoints[0], d0) / material.SmoothShadow;
                End;
              End;
            End;
          End;
        End;
      End;
    End;
  End;
End;

Function TTriangle.NormalAtPoint(Value: TVector3f): TVector3f;
Begin
  result := Fnormal;
End;

Procedure TTriangle.Rotate(Angle, dx, dy, dz: TRayFloat);
Var
  i: Integer;
  RotV: TVector3f;
Begin
  RotV := v3(dx * Angle, dy * Angle, dz * Angle);
  For i := 0 To 2 Do
    fpoints[i] := RotateV3(fpoints[i], RotV);
  Fnormal := normalizev3(CrossProdv3(subv3(FPoints[1], FPoints[0]), subv3(FPoints[2], FPoints[0])));
  Fn := CrossProdV3(subv3(fpoints[1], fpoints[0]), subv3(Fpoints[2], Fpoints[0]));
  Flennsqr := lengthv3sqr(Fn);
  If Flennsqr <> 0 Then Flennsqr := 1 / Flennsqr;
  Fv0_1 := subv3(Fpoints[1], Fpoints[0]);
  Fv0_2 := subv3(Fpoints[2], Fpoints[0]);
End;

Procedure TTriangle.Translate(Value: TVector3f);
Var
  i: Integer;
Begin
  For i := 0 To 2 Do
    Fpoints[i] := AddV3(Fpoints[i], value);
End;

{ TTexturmanager }

Constructor TTextureManager.create;
Begin
  Inherited;
  setlength(Fdata, 0);
End;

Destructor TTextureManager.destroy;
Begin
  clear;
End;

Function TTextureManager.AddTexture(Filename: String): TLazIntfImage;
Var
  Name: String;
  i: Integer;
  jp: TJPEGImage;
  b: Tbitmap;
Begin
  name := LowerCase(Filename);
  // Schaun obs die Textur schon gibt.
  For i := 0 To High(Fdata) Do
    If Name = Fdata[i].Filename Then Begin
      result := Fdata[i].texture;
      exit;
    End;
  // Laden der Textur wenn sie noch nicht geladen wurde
  setlength(fdata, high(fdata) + 2);
  Fdata[high(fdata)].Filename := name;
  b := Tbitmap.create;
  If lowercase(ExtractFileExt(Filename)) = '.bmp' Then
    b.LoadFromFile(Filename)
  Else Begin
    If lowercase(ExtractFileExt(Filename)) = '.jpg' Then Begin
      jp := TJPEGImage.create;
      jp.LoadFromFile(Filename);
      b.assign(jp);
      jp.free;
    End
    Else Begin
      b.free;
      Result := Nil;
      setlength(fdata, high(fdata));
      exit;
    End;
  End;
  b.PixelFormat := pf24bit;
  Fdata[high(fdata)].Texture := TLazIntfImage.Create(0, 0);
  Fdata[high(fdata)].Texture.LoadFromBitmap(b.Handle, b.MaskHandle);
  Result := Fdata[high(fdata)].Texture;
  b.free;
End;

Procedure TTextureManager.Clear;
Var
  i: Integer;
Begin
  For i := 0 To High(fdata) Do
    Fdata[i].Texture.free;
  setlength(Fdata, 0);
End;

{ TQuad }

Constructor TQuad.create;
Begin
  Inherited;
  Fpoints[0] := v3(0, 0, 0);
  Fpoints[1] := v3(1, 0, 0);
  Fpoints[2] := v3(1, 1, 0);
  Fpoints[3] := v3(0, 1, 0);
  TextureCoord[0] := v2(0, 0);
  TextureCoord[1] := v2(1, 0);
  TextureCoord[2] := v2(1, 1);
  TextureCoord[3] := v2(0, 1);
End;

Destructor TQuad.destroy;
Begin
  Inherited;
End;

Procedure TQuad.Define(Point1, Point2, Point3, Point4: TVector3f);
Begin
  Fpoints[0] := Point1;
  Fpoints[1] := Point2;
  Fpoints[2] := Point3;
  Fpoints[3] := Point4;
End;

Procedure TQuad.Rotate(Angle, dx, dy, dz: TRayFloat);
Var
  i: Integer;
  RotV: TVector3f;
Begin
  RotV := v3(dx * Angle, dy * Angle, dz * Angle);
  For i := 0 To 3 Do
    fpoints[i] := RotateV3(fpoints[i], RotV);
End;

Procedure TQuad.Translate(Value: TVector3f);
Var
  i: Integer;
Begin
  For i := 0 To 3 Do
    Fpoints[i] := AddV3(Fpoints[i], value);
End;

Function TQuad.ColorAtPoint(value: TVector3f): Tvector3f;
Begin
  Raise Exception.create('Error never call the ColorAtPoint function of TQuad, the raytracer handles this by other ways.');
End;

Function TQuad.Intersect(Position, Direction: TVector3f; Schattenstrahl: Boolean): TRayFloat;
Begin
  Raise Exception.create('Error never call the Intersect function of TQuad, the raytracer handles this by other ways.');
End;

Function TQuad.NormalAtPoint(Value: TVector3f): TVector3f;
Begin
  Raise Exception.create('Error never call the NormalAtPoint function of TQuad, the raytracer handles this by other ways.');
End;

End.

