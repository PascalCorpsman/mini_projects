(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Digiman                                               *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit udigiman;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Graphics;

Const
  Grid = 4;
  FileVersion: Integer = 1;

Type

  TState = (sOff, sOn, {sOffToOn, sOnToOff,} sUndefined);

  TDigimanElement = Class;
  Tline = Class;

  TPointArray = Array Of TPoint;

  (*
   * Der Eigentliche Emulator
   *)

  { TDigiman }

  TDigiman = Class
  private
    fElements: Array Of TDigimanElement;
    fLines: Array Of Tline; //Macht das handling leichter, müsste aber nicht Extra sein ..
    fBridges: TPointArray;
    fBridgeImage: TBitmap;
    Procedure initElementsIndexWithElement(Const aElement: TDigimanElement; aIndex: integer);
    Procedure initLinesIndexWithElement(Const aElement: Tline; aIndex: integer);
  protected
    Procedure RemoveAllConnectionsTo(aElement: TDigimanElement);
    Function ElementToElementIndex(Const aElement: TDigimanElement): integer;
    Procedure CalculateAndAddIntersectionPoints(Const aLine1, aLine2: Tline);
    Procedure CalculateLineBridges;
  public
    ShowPegel: Boolean;
    ShowConnectionPoints: Boolean;

    Constructor Create(); virtual;
    Destructor Destroy(); override;

    Procedure RenderTo(Const aCanvas: TCanvas; aOffset: TPoint);

    Procedure AddElement(Const aElement: TDigimanElement);
    Procedure DelElement(Const aElement: TDigimanElement);
    Procedure Clear();

    Function GetElementAtPos(x, y: integer): TDigimanElement;

    Procedure SaveToFile(Const aFilename: String);
    Procedure LoadFromFile(Const aFilename: String);
  End;

  (*
   * Ein Element welches in TDigiman verwaltet wird
   *)

  TIndexElement = Record
    Element: TDigimanElement;
    Index: integer;
  End;

  { TDigimanElement }

  TDigimanElement = Class
  private
    fOwner: TDigiman;
  protected
    fEvaluated: Boolean;
    fEvaluatedState: TState;
    InElements: Array Of TIndexElement; //Jeder In Point hat ein InElement

    Function getHeight: integer; virtual; // Abstract
    Function getWidth: integer; virtual; // Abstract
    Function LoadImage(aFilename: String): TBitmap; // Helper function for constructors
    Procedure RemoveAllConnectionsTo(aElement: TDigimanElement); virtual;
    Function _In(index: Integer): TState;
  public
    Top: integer;
    Left: integer;

    InPoints: Array Of Tpoint; // Relativ zu Top / Left
    OutPoints: Array Of Tpoint; // Relativ zu Top / Left

    Property Width: integer read getWidth;
    Property Height: integer read getHeight;

    Constructor Create(); virtual;
    Destructor Destroy(); override;

    Procedure SaveToStream(Const aStream: TStream); virtual;
    Procedure LoadFromStream(Const aStream: TStream); virtual;

    Procedure setPosition(aX, aY: Integer);
    Procedure RenderTo(Const aCanvas: TCanvas; aOffset: TPoint); virtual; // Abstract

    Procedure Click; virtual;
    Function Clone: TDigimanElement; virtual abstract;

    Function GetState(aOutindex: integer): Tstate; virtual; // Abstract
    Procedure ResetEvalState;

    Procedure SetInElement(
      aInIndex: integer; // Der Eingang an dem aOutElement angeschlossen ist
      aOutElement: TDigimanElement; // das Element, welches wir lesen
      aOutIndex: integer // Der Ausgang des Elementes welches wir lesen
      );

    (*
     * True, wenn die Koordinate aX, aY einen der InPoints / OutPoints getroffen hat.
     * *Index = -1 => Kein Hit, sonst index im jeweiligen array
     *)
    Function InOutPointHit(aX, aY: integer; Out InIndex, OutIndex: integer): Boolean;
  End;

  { TTool }

  TTool = Class(TDigimanElement) // Kein Eigentliches Element, aber es hilft in der Gui Steuerung
  private
    fImage: TBitmap;
  protected
    Function getHeight: integer; override;
    Function getWidth: integer; override;
  public
    Constructor Create(); override;
    Destructor Destroy(); override;

    Procedure RenderTo(Const aCanvas: TCanvas; aOffset: TPoint); override;

    Function Clone: TDigimanElement; override;
  End;

  { TEraser }

  TEraser = Class(TTool)
  public
    Constructor Create(); override;
  End;

  { TLineTool }

  TLineTool = Class(TTool)
  public
    Constructor Create(); override;
  End;

  TLine = Class(TDigimanElement)
  private
    fPoints: Array Of TPoint;
    fInElement: TDigimanElement;
    fInIndex: integer;
    fOutElement: TDigimanElement;
    fOutIndex: integer;
  public
    Constructor Create(); override;

    Procedure SaveToStream(Const aStream: TStream); override;
    Procedure LoadFromStream(Const aStream: TStream); override;

    Procedure RenderTo(Const aCanvas: TCanvas; aOffset: TPoint); override;

    Function Clone: TDigimanElement; override;

    Function GetState(aindex: integer): Tstate; override;
    Function PointCollideWithLine(Const aPoint: TPoint): Boolean;
  End;

  { TUserText }

  TUserText = Class(TDigimanElement)
  private
    fWidth, fHeight: integer;
  protected
    Function getHeight: integer; override;
    Function getWidth: integer; override;

  public
    Text: String;
    Constructor Create(); override;

    Procedure SaveToStream(Const aStream: TStream); override;
    Procedure LoadFromStream(Const aStream: TStream); override;

    Procedure RenderTo(Const aCanvas: TCanvas; aOffset: TPoint); override;

    Function Clone: TDigimanElement; override;
  End;

  { TUserInput }

  TUserInput = Class(TDigimanElement)
  private
    fState: TState;
    fUpImage, fDownImage: TBitmap;
  protected
    Function getHeight: integer; override;
    Function getWidth: integer; override;

  public
    Constructor Create(); override;
    Destructor Destroy(); override;

    Procedure SaveToStream(Const aStream: TStream); override;
    Procedure LoadFromStream(Const aStream: TStream); override;

    Procedure RenderTo(Const aCanvas: TCanvas; aOffset: TPoint); override;

    Procedure Click; override;

    Function GetState(aindex: Integer): Tstate; override;

    Function Clone: TDigimanElement; override;
  End;

  { TProbe }

  TProbe = Class(TDigimanElement)
  private
    fOnImage, fOffImage, fUnknownImage: TBitmap;
  protected
    Function getHeight: integer; override;
    Function getWidth: integer; override;
  public
    Constructor Create(); override;
    Destructor Destroy(); override;

    Procedure RenderTo(Const aCanvas: TCanvas; aOffset: TPoint); override;

    Function Clone: TDigimanElement; override;
  End;

  { TOneInOneOut }

  TOneInOneOut = Class(TDigimanElement)
  private
    fImage: TBitmap;
  protected
    Function getHeight: integer; override;
    Function getWidth: integer; override;
  public
    Constructor Create(); override;
    Destructor Destroy(); override;

    Procedure RenderTo(Const aCanvas: TCanvas; aOffset: TPoint); override;
  End;

  { TTwoInOneOut }

  TTwoInOneOut = Class(TDigimanElement)
  private
    fImage: TBitmap;
  protected
    Function getHeight: integer; override;
    Function getWidth: integer; override;
  public
    Constructor Create(); override;
    Destructor Destroy(); override;

    Procedure RenderTo(Const aCanvas: TCanvas; aOffset: TPoint); override;
  End;

  { TNot }

  TNot = Class(TOneInOneOut)
  public
    Constructor Create(); override;

    Function Clone: TDigimanElement; override;

    Function GetState(aindex: Integer): Tstate; override;
  End;

  { TAnd }

  TAnd = Class(TTwoInOneOut)
  protected
  public
    Constructor Create(); override;

    Function GetState(aOutindex: integer): Tstate; override;

    Function Clone: TDigimanElement; override;
  End;

  { TOr }

  TOr = Class(TTwoInOneOut)
  public
    Constructor Create(); override;

    Function GetState(aOutindex: integer): Tstate; override;

    Function Clone: TDigimanElement; override;
  End;

  { TNor }

  TNor = Class(TTwoInOneOut)
  public
    Constructor Create(); override;

    Function GetState(aOutindex: integer): Tstate; override;

    Function Clone: TDigimanElement; override;
  End;

  { TNand }

  TNand = Class(TTwoInOneOut)
  public
    Constructor Create(); override;

    Function GetState(aOutindex: integer): Tstate; override;

    Function Clone: TDigimanElement; override;
  End;

  TLineCreateMode = (lcmIdle, lcmAddCorners);

  { TLineCreateHelper }

  TLineCreateHelper = Class
  private
    fPoints: Array Of TPoint;
    fInIndex: integer;
    fOutIndex: integer;
    fInElement: TDigimanElement;
    fOutElement: TDigimanElement;
    fAktualMousePos: TPoint;
  public
    Mode: TLineCreateMode;
    Constructor Create;

    Procedure RenderTo(Const aCanvas: TCanvas; aOffset: TPoint);

    Procedure SetActualMousePosition(aX, aY: integer);

    Procedure StartNewLine(StartElement: TDigimanElement; InIndex, OutIndex: integer);
    Procedure AddCorner(aX, aY: integer);
    Function EndLine(EndElement: TDigimanElement; InIndex, OutIndex: integer): boolean;
    Procedure DelLastCorner;
  End;

Implementation

Uses Dialogs;

Function _Not(aState: TState): TState;
Begin
  result := sUndefined;
  Case aState Of
    sOff: result := sOn;
    sOn: Result := sOff;
    //    sOffToOn: Result := sOnToOff;
    //    sOnToOff: Result := sOffToOn;
  End;
End;

Function _or(aS1, aS2: TState): TState;
Begin
  Result := sUndefined;
  If (as1 = sOn) Or (as2 = sOn) Then Begin
    Result := sOn;
  End
  Else Begin
    If (as1 = sOff) Or (as2 = sOff) Then Begin
      Result := sOff
    End;
  End;
End;

Function _and(aS1, aS2: TState): TState;
Begin
  Result := sUndefined;
  If (aS1 = sOn) And (aS2 = sOn) Then
    Result := sOn
  Else Begin
    If (aS1 = sOff) Or (aS2 = sOff) Then
      Result := sOff
    Else If (aS1 = sOn) Or (aS2 = sOn) Then
      Result := sOn
  End;
End;

Function CreateDigimanElemenFromString(Const ClassName: String): TDigimanElement;
Begin
  result := Nil;
  (*
   * Hier sind nicht alle klassen gelistet, nur die die geladen und gespeichert werden können..
   *)
  Case lowercase(ClassName) Of
    'tnand': result := TNand.Create();
    'tnor': result := TNor.Create();
    'tor': result := Tor.Create();
    'tand': result := Tand.Create();
    'tnot': result := TNot.Create();
    'tprobe': result := TProbe.Create();
    'tuserinput': result := TUserInput.Create();
    'tusertext': result := TUserText.Create();
  Else Begin
      Raise exception.create('Error: ' + ClassName + ' not implemented in CreateDigimanElemenFromString');
    End;
  End;
End;

Procedure LineStateToCanvas(Const aCanvas: TCanvas; aPos: Tpoint; aState: TState);
Begin
  If aState In [{sOffToOn,} sUndefined, sOn] Then Begin
    acanvas.Pixels[aPos.x, apos.y] := clRed;
    acanvas.Pixels[aPos.x + 1, apos.y] := clRed;
    acanvas.Pixels[aPos.x, apos.y - 1] := clRed;
    acanvas.Pixels[aPos.x + 1, apos.y - 1] := clRed;
    acanvas.Pixels[aPos.x, apos.y - 4] := clRed;
    acanvas.Pixels[aPos.x + 1, apos.y - 4] := clRed;
    acanvas.Pixels[aPos.x, apos.y - 5] := clRed;
    acanvas.Pixels[aPos.x + 1, apos.y - 5] := clRed;
  End;
  If aState In [{sOffToOn,} sOn] Then Begin
    acanvas.Pixels[aPos.x, apos.y - 2] := clRed;
    acanvas.Pixels[aPos.x + 1, apos.y - 2] := clRed;
    acanvas.Pixels[aPos.x, apos.y - 3] := clRed;
    acanvas.Pixels[aPos.x + 1, apos.y - 3] := clRed;
  End;
End;

Function PointInPointCollider(aX, aY: integer; Const aPoint: Tpoint): Boolean;
Begin
  result :=
    (aPoint.x - 2 <= ax) And (aPoint.x + 3 >= aX) And
    (aPoint.Y - 2 <= ay) And (aPoint.Y + 3 >= ay);
End;

Procedure ConnectionPointToCanvas(Const aCanvas: TCanvas; aPos: Tpoint);
Begin
  aCanvas.Brush.Color := clGreen;
  aCanvas.Pen.Color := clGreen;
  aCanvas.Rectangle(aPos.x - 2, aPos.Y - 2, aPos.x + 3, aPos.Y + 3);
End;

Function PointCollideWithSegment(Const aCollider, aFrom, aTo: Tpoint): boolean;
Var
  dx: Integer;
Begin
  result := false;
  dx := ato.X - aFrom.X;
  // First Horizontal
  If (aCollider.Y >= aFrom.Y - 2) And
    (aCollider.Y <= aFrom.Y + 3) And
    (aCollider.x >= aFrom.X) And
    (aCollider.x <= aFrom.X + dx) Then result := true;
  // Second Vertical
  If (aCollider.x >= aTo.X - 2) And
    (aCollider.x <= aTo.x + 3) And
    (aCollider.Y >= aFrom.Y) And
    (aCollider.Y <= aTo.Y) Then result := true;
End;

Procedure DrawSegment(Const aCanvas: TCanvas; aOffset: TPoint; aFrom, aTo: Tpoint);
Var
  dx: integer;
Begin
  aFrom := aFrom - aOffset;
  aTo := aTo - aOffset;
  dx := ato.X - aFrom.X;
  // First Horizontal
  aCanvas.Line(aFrom.X, aFrom.Y, aFrom.X + dx, aFrom.Y);
  // Second Vertical
  aCanvas.Line(aFrom.X + dx, aFrom.Y, aTo.X, aTo.Y);
End;

// -- Start content created using ChatGPT

Function Between(Value, A, B: Integer): Boolean;
Begin
  If A > B Then
    Result := (Value >= B) And (Value <= A)
  Else
    Result := (Value >= A) And (Value <= B);
End;

Function IntersectHorzVert(hFrom, hTo, vFrom, vTo: TPoint; Out p: TPoint): Boolean;
Begin
  // hFrom/hTo = horizontales Segment
  // vFrom/vTo = vertikales Segment

  Result :=
    Between(vFrom.x, hFrom.x, hTo.x) And
    Between(hFrom.y, vFrom.y, vTo.y);

  If Result Then Begin
    p.x := vFrom.x;
    p.y := hFrom.y;
  End;
End;

Function IntersectHorzHorz(a1, a2, b1, b2: TPoint; Out p: TPoint): Boolean;
Begin
  Result := False;

  // gleiche Y-Linie?
  If a1.y <> b1.y Then Exit;

  // Überlappung prüfen
  If Between(a1.x, b1.x, b2.x) Then Begin
    p := a1;
    Exit(True);
  End;

  If Between(a2.x, b1.x, b2.x) Then Begin
    p := a2;
    Exit(True);
  End;

  If Between(b1.x, a1.x, a2.x) Then Begin
    p := b1;
    Exit(True);
  End;
End;

Function IntersectVertVert(a1, a2, b1, b2: TPoint; Out p: TPoint): Boolean;
Begin
  Result := False;

  // gleiche X-Linie?
  If a1.x <> b1.x Then Exit;

  // Überlappung prüfen
  If Between(a1.y, b1.y, b2.y) Then Begin
    p := a1;
    Exit(True);
  End;

  If Between(a2.y, b1.y, b2.y) Then Begin
    p := a2;
    Exit(True);
  End;

  If Between(b1.y, a1.y, a2.y) Then Begin
    p := b1;
    Exit(True);
  End;
End;

Function SegmentCollide(Const aFrom1, aTo1, aFrom2, aTo2: TPoint; Out p: TPoint): Boolean;
Var
  h1From, h1To, v1From, v1To: TPoint;
  h2From, h2To, v2From, v2To: TPoint;
  k1, k2: TPoint; // Knickpunkte
Begin
  // Segment 1 zerlegen
  h1From := aFrom1;
  h1To.x := aTo1.x;
  h1To.y := aFrom1.y;

  v1From := h1To;
  v1To := aTo1;
  k1 := h1To; // Knickpunkt 1

  // Segment 2 zerlegen
  h2From := aFrom2;
  h2To.x := aTo2.x;
  h2To.y := aFrom2.y;

  v2From := h2To;
  v2To := aTo2;
  k2 := h2To; // Knickpunkt 2

  // Kombinationen prüfen
  Result :=
    IntersectHorzVert(h1From, h1To, v2From, v2To, p) Or
    IntersectHorzVert(h2From, h2To, v1From, v1To, p) Or
    IntersectHorzHorz(h1From, h1To, h2From, h2To, p) Or
    IntersectVertVert(v1From, v1To, v2From, v2To, p);

  // Endpunkte UND Knickpunkte ignorieren
  If Result Then Begin
    If (p = aFrom1) Or
      (p = aTo1) Or
      (p = aFrom2) Or
      (p = aTo2) Or
      (p = k1) Or
      (p = k2) Then
      Result := False;
  End;
End;
// -- End content created using ChatGPT

{ TDigimanElement }

Constructor TDigimanElement.Create;
Begin
  Inherited create;
  fEvaluated := false;
  fOwner := Nil;
  InPoints := Nil;
  InElements := Nil;
  OutPoints := Nil;
End;

Destructor TDigimanElement.Destroy;
Begin

End;

Procedure TDigimanElement.SaveToStream(Const aStream: TStream);
Begin
  aStream.Write(top, SizeOf(Top));
  aStream.Write(Left, SizeOf(Left));
End;

Procedure TDigimanElement.LoadFromStream(Const aStream: TStream);
Begin
  aStream.Read(top, SizeOf(Top));
  aStream.Read(Left, SizeOf(Left));
End;

Function TDigimanElement.getHeight: integer;
Begin
  result := 0;
  Raise exception.Create(ClassName + '.getHeight not implemented.');
End;

Function TDigimanElement.getWidth: integer;
Begin
  result := 0;
  Raise exception.Create(ClassName + '.getWidth not implemented.');
End;

Function TDigimanElement.LoadImage(aFilename: String): TBitmap;
Begin
  result := TBitmap.Create;
  result.LoadFromFile('GFX' + PathDelim + aFilename);
  result.TransparentColor := clFuchsia;
  result.Transparent := true;
End;

Procedure TDigimanElement.RemoveAllConnectionsTo(aElement: TDigimanElement);
Var
  i: Integer;
Begin
  For i := 0 To high(InElements) Do Begin
    If InElements[i].Element = aElement Then Begin
      InElements[i].Element := Nil;
      InElements[i].Index := -1;
    End;
  End;
End;

Function TDigimanElement._In(index: Integer): TState;
Begin
  result := sUndefined;
  If (index >= 0) And (index <= high(InElements)) And assigned(InElements[index].Element) Then Begin
    result := InElements[index].Element.GetState(InElements[index].Index);
  End;
End;

Procedure TDigimanElement.setPosition(aX, aY: Integer);
Begin
  Left := (aX - Width Div 2);
  Top := (aY - Height Div 2);
  Left := Left - Left Mod Grid;
  Top := Top - Top Mod Grid;
  If assigned(fOwner) Then
    fOwner.RemoveAllConnectionsTo(Self);
End;

Procedure TDigimanElement.RenderTo(Const aCanvas: TCanvas; aOffset: TPoint);
Begin
  Raise exception.Create(ClassName + '.Render not implemented.');
End;

Procedure TDigimanElement.Click;
Begin
  // Nichts, ..
End;

Function TDigimanElement.GetState(aOutindex: integer): Tstate;
Begin
  result := sUndefined;
End;

Procedure TDigimanElement.ResetEvalState;
Begin
  fEvaluated := False;
End;

Procedure TDigimanElement.SetInElement(aInIndex: integer;
  aOutElement: TDigimanElement; aOutIndex: integer);
Begin
  InElements[aInIndex].Element := aOutElement;
  InElements[aInIndex].Index := aOutIndex;
End;

Function TDigimanElement.InOutPointHit(aX, aY: integer; Out InIndex,
  OutIndex: integer): Boolean;
Var
  i: Integer;
Begin
  result := false;
  ax := ax - Left;
  ay := ay - Top;
  InIndex := -1;
  OutIndex := -1;
  For i := 0 To high(InPoints) Do Begin
    If PointInPointCollider(ax, ay, InPoints[i]) Then Begin
      InIndex := i;
      result := true;
      exit;
    End;
  End;
  For i := 0 To high(OutPoints) Do Begin
    If PointInPointCollider(ax, ay, OutPoints[i]) Then Begin
      OutIndex := i;
      result := true;
      exit;
    End;
  End;
End;

{ TTool }

Function TTool.getHeight: integer;
Begin
  Result := fImage.Height;
End;

Function TTool.getWidth: integer;
Begin
  Result := fImage.Width;
End;

Constructor TTool.Create;
Begin
  Inherited Create();
  fImage := Nil;
End;

Destructor TTool.Destroy;
Begin
  fImage.Free;
  fImage := Nil;
  Inherited Destroy();
End;

Procedure TTool.RenderTo(Const aCanvas: TCanvas; aOffset: TPoint);
Begin
  acanvas.Draw(Left - aOffset.x, Top - aOffset.y, fImage);
End;

Function TTool.Clone: TDigimanElement;
Begin
  result := Nil;
  Raise exception.create(ClassName + ' can not be cloned.');
End;

{ TEraser }

Constructor TEraser.Create();
Begin
  Inherited Create();
  fImage := LoadImage('Eraser.bmp');
End;

{ TLineTool }

Constructor TLineTool.Create();
Begin
  Inherited Create();
  fImage := LoadImage('Linetool.bmp');
End;

Constructor TLine.Create;
Begin
  Inherited Create();
  fPoints := Nil;
  fInElement := Nil;
  fOutIndex := -1;
End;

Procedure TLine.SaveToStream(Const aStream: TStream);
Var
  i: integer;
Begin
  i := Length(fPoints);
  aStream.Write(i, SizeOf(i));
  For i := 0 To high(fPoints) Do Begin
    aStream.Write(fPoints[i], sizeof(fPoints[i]));
  End;
End;

Procedure TLine.LoadFromStream(Const aStream: TStream);
Var
  i: integer;
Begin
  i := 0;
  aStream.Read(i, SizeOf(i));
  setlength(fPoints, i);
  For i := 0 To high(fPoints) Do Begin
    aStream.Read(fPoints[i], SizeOf(fPoints[i]));
  End;
End;

Procedure TLine.RenderTo(Const aCanvas: TCanvas; aOffset: TPoint);
Var
  i: Integer;
Begin
  aCanvas.Pen.Color := clBlack;
  For i := 0 To high(fPoints) - 1 Do Begin
    DrawSegment(aCanvas, aOffset, fPoints[i], fPoints[i + 1]);
  End;
End;

Function TLine.Clone: TDigimanElement;
Begin
  result := Nil;
  Raise exception.create('TLine can not be cloned !');
End;

Function TLine.GetState(aindex: integer): Tstate;
Begin
  Result := fInElement.GetState(fOutIndex);
End;

Function TLine.PointCollideWithLine(Const aPoint: TPoint): Boolean;
Var
  i: Integer;
Begin
  result := false;
  For i := 0 To high(fPoints) - 1 Do Begin
    If PointCollideWithSegment(aPoint, fPoints[i], fPoints[i + 1]) Then Begin
      result := true;
      exit;
    End;
  End;
End;

{ TUserText }

Function TUserText.getHeight: integer;
Begin
  Result := fHeight;
End;

Function TUserText.getWidth: integer;
Begin
  Result := fWidth;
End;

Constructor TUserText.Create;
Begin
  Inherited Create();
  Text := '';
  fWidth := 0;
  fHeight := 0;
End;

Procedure TUserText.SaveToStream(Const aStream: TStream);
Begin
  Inherited SaveToStream(aStream);
  aStream.WriteAnsiString(Text);
End;

Procedure TUserText.LoadFromStream(Const aStream: TStream);
Begin
  Inherited LoadFromStream(aStream);
  Text := aStream.ReadAnsiString;
End;

Procedure TUserText.RenderTo(Const aCanvas: TCanvas; aOffset: TPoint);
Begin
  aCanvas.Brush.Color := clWhite;
  aCanvas.TextOut(Left - aOffset.x, Top - aOffset.Y, Text);
  If fWidth = 0 Then Begin
    fWidth := aCanvas.TextWidth(text);
    fHeight := aCanvas.TextHeight(text);
  End;
End;

Function TUserText.Clone: TDigimanElement;
Var
  s: String;
Begin
  result := TUserText.Create();
  s := InputBox('Please enter a text:', '', 'Text');
  If trim(s) = '' Then s := 'Text';
  TUserText(result).Text := s;
End;

Procedure TDigiman.initElementsIndexWithElement(
  Const aElement: TDigimanElement; aIndex: integer);
Begin
  aElement.fOwner := self;
  fElements[aIndex] := aElement;
End;

Procedure TDigiman.initLinesIndexWithElement(Const aElement: Tline;
  aIndex: integer);
Begin
  aElement.fOwner := self;
  fLines[aIndex] := aElement;

  // Die "Elemente" müssen nun auch verbunden werden
  fLines[aIndex].fOutElement.SetInElement(
    fLines[aIndex].fOutIndex,
    fLines[aIndex].fInElement,
    fLines[aIndex].finIndex
    );
End;

Procedure TDigiman.RemoveAllConnectionsTo(aElement: TDigimanElement);
Var
  i, j: Integer;
Begin
  For i := 0 To high(fElements) Do Begin
    fElements[i].RemoveAllConnectionsTo(aElement);
  End;
  // ggf. Löschen von Linien..
  For i := high(fLines) Downto 0 Do Begin
    If (fLines[i].fOutElement = aElement) Or
      (fLines[i].fInElement = aElement) Then Begin
      fLines[i].Free;
      For j := i To high(fLines) - 1 Do Begin
        fLines[j] := fLines[j + 1];
      End;
      setlength(fLines, high(fLines));
    End;
  End;
End;

Function TDigiman.ElementToElementIndex(Const aElement: TDigimanElement
  ): integer;
Var
  i: Integer;
Begin
  result := -1;
  For i := 0 To high(fElements) Do Begin
    If aElement = fElements[i] Then Begin
      result := i;
      exit;
    End;
  End;
End;

Procedure TDigiman.CalculateAndAddIntersectionPoints(Const aLine1, aLine2: Tline
  );
Var
  i, j: Integer;
  p: Tpoint;
Begin
  // Jedes Segment von Linie 1 muss mit jedem Segment von Linie 2 verglichen werden
  // Schneiden sich 2 Segmente -> Schnittpunkt Berechnen und Hinzufügen
  For i := 0 To high(aLine1.fPoints) - 1 Do Begin
    For j := 0 To high(aLine2.fPoints) - 1 Do Begin
      If SegmentCollide(aLine1.fPoints[i], aLine1.fPoints[i + 1],
        aLine2.fPoints[j], aLine2.fPoints[j + 1], p) Then Begin
        setlength(fBridges, high(fBridges) + 2);
        fBridges[high(fBridges)] := p;
      End;
    End;
  End;
End;

Procedure TDigiman.CalculateLineBridges;
Var
  i, j: Integer;
Begin
  setlength(fBridges, 0);
  (*
   * Berechnet Schnittpunkte von Linien, an jeden Schnittpunkt werden "Brücken"
   * Gezeichnet
   *)
  For i := 0 To high(fLines) - 1 Do Begin
    For j := i + 1 To high(fLines) Do Begin
      CalculateAndAddIntersectionPoints(fLines[i], fLines[j]);
    End;
  End;
End;

Constructor TDigiman.Create;
Begin
  Inherited Create();
  fElements := Nil;
  fLines := Nil;
  fBridges := Nil;
  ShowPegel := true;
  ShowConnectionPoints := false;
  fBridgeImage := TBitmap.Create;
  fBridgeImage.LoadFromFile('GFX' + PathDelim + 'Bridge.bmp');
End;

Destructor TDigiman.Destroy;
Begin
  Clear;
  fBridgeImage.free;
  fBridgeImage := Nil;
End;

Procedure TDigiman.RenderTo(Const aCanvas: TCanvas; aOffset: TPoint);
Var
  i: Integer;
Begin
  For i := 0 To high(fElements) Do Begin
    fElements[i].ResetEvalState;
  End;
  For i := 0 To high(fLines) Do Begin
    fLines[i].RenderTo(aCanvas, aOffset);
  End;
  For i := 0 To high(fBridges) Do Begin
    aCanvas.Draw(fBridges[i].X - fBridgeImage.Width Div 2,
      fBridges[i].Y - fBridgeImage.Height + 1, fBridgeImage);
  End;
  For i := 0 To high(fElements) Do Begin
    fElements[i].RenderTo(aCanvas, aOffset);
  End;
End;

Procedure TDigiman.AddElement(Const aElement: TDigimanElement);
Var
  aLine: TLine;
Begin
  If aElement Is TLine Then Begin
    aLine := aElement As TLine;
    setlength(fLines, high(fLines) + 2);
    initLinesIndexWithElement(aLine, high(fLines));
    CalculateLineBridges;
  End
  Else Begin
    setlength(fElements, high(fElements) + 2);
    initElementsIndexWithElement(aElement, high(fElements));
  End;
End;

Procedure TDigiman.DelElement(Const aElement: TDigimanElement);
Var
  i, j: Integer;
Begin
  If aElement Is TLine Then Begin
    For i := 0 To high(fLines) Do Begin
      If fLines[i] = aElement Then Begin
        // Aushängen
        fLines[i].fOutElement.SetInElement(
          fLines[i].fOutIndex,
          Nil, -1);
        // Löschen der Linie
        fLines[i].Free;
        For j := i To high(fLines) - 1 Do Begin
          fLines[j] := fLines[j + 1];
        End;
        SetLength(fLines, high(fLines));
        CalculateLineBridges;
        exit;
      End;
    End;
  End
  Else Begin
    For i := 0 To high(fElements) Do Begin
      If fElements[i] = aElement Then Begin
        RemoveAllConnectionsTo(aElement);
        fElements[i].Free;
        For j := i To high(fElements) - 1 Do Begin
          fElements[j] := fElements[j + 1];
        End;
        SetLength(fElements, high(fElements));
        exit;
      End;
    End;
  End;
End;

Procedure TDigiman.Clear;
Var
  i: Integer;
Begin
  For i := 0 To high(fElements) Do Begin
    fElements[i].Free;
  End;
  setlength(fElements, 0);
  For i := 0 To high(fLines) Do Begin
    fLines[i].Free;
  End;
  setlength(fLines, 0);
  setlength(fBridges, 0);
End;

Function TDigiman.GetElementAtPos(x, y: integer): TDigimanElement;
Var
  i: Integer;
Begin
  result := Nil;
  For i := 0 To high(fElements) Do Begin
    If (fElements[i].Left <= x) And (fElements[i].Left + fElements[i].Width >= x) And
      (fElements[i].Top <= y) And (fElements[i].Top + fElements[i].Height >= y) Then Begin
      result := fElements[i];
      exit;
    End;
  End;
  For i := 0 To high(fLines) Do Begin
    If fLines[i].PointCollideWithLine(point(x, y)) Then Begin
      result := fLines[i];
      exit;
    End;
  End;
End;

Procedure TDigiman.SaveToFile(Const aFilename: String);
Var
  m: TMemoryStream;
  i, j: Integer;
Begin
  m := TMemoryStream.Create;
  m.Write(FileVersion, sizeof(FileVersion));
  i := length(fElements);
  m.Write(i, SizeOf(i));
  // Die Elemente
  For i := 0 To high(fElements) Do Begin
    m.WriteAnsiString(fElements[i].ClassName);
    fElements[i].saveToStream(m);
  End;
  // Die Linien sind ein wenig "Kniffliger"
  i := length(fLines);
  m.Write(i, SizeOf(i));
  For i := 0 To high(fLines) Do Begin
    // Die "internen" Linien Daten
    fLines[i].SaveToStream(m);
    // Die Connections
    j := ElementToElementIndex(fLines[i].fInElement);
    m.Write(j, SizeOf(j));
    j := fLines[i].fInIndex;
    m.Write(j, SizeOf(j));
    j := ElementToElementIndex(fLines[i].fOutElement);
    m.Write(j, SizeOf(j));
    j := fLines[i].fOutIndex;
    m.Write(j, SizeOf(j));
  End;

  // TODO: Implementieren

  m.SaveToFile(aFilename);
  m.free;
End;

Procedure TDigiman.LoadFromFile(Const aFilename: String);
Var
  m: TMemoryStream;
  j, i, FV: integer;
  e: TDigimanElement;
  l: TLine;
Begin
  Clear();
  m := TMemoryStream.Create;
  m.LoadFromFile(aFilename);
  FV := 0;
  m.Read(FV, sizeof(FileVersion));
  // TODO: differ by Fileversion

  i := 0;
  m.Read(i, SizeOf(i));
  setlength(fElements, i);
  For i := 0 To high(fElements) Do Begin
    e := CreateDigimanElemenFromString(m.ReadAnsiString);
    e.LoadFromStream(m);
    initElementsIndexWithElement(e, i);
  End;

  i := 0;
  j := 0;
  m.Read(i, SizeOf(i));
  setlength(fLines, i);
  For i := 0 To high(fLines) Do Begin
    l := TLine.Create();
    l.LoadFromStream(m);
    m.Read(j, SizeOf(j));
    l.fInElement := fElements[j];
    m.Read(j, SizeOf(j));
    l.fInIndex := j;
    m.Read(j, SizeOf(j));
    l.fOutElement := fElements[j];
    m.Read(j, SizeOf(j));
    l.fOutIndex := j;
    initLinesIndexWithElement(l, i);
  End;

  // Todo: Implementieren

  m.free;
  CalculateLineBridges;
End;

{ TUserInput }

Function TUserInput.getHeight: integer;
Begin
  Result := fUpImage.Height;
End;

Function TUserInput.getWidth: integer;
Begin
  Result := fUpImage.Width;
End;

Constructor TUserInput.Create;
Begin
  fUpImage := LoadImage('Userinput_up.bmp');
  fDownImage := LoadImage('Userinput_down.bmp');
  fState := sOff;
  setlength(OutPoints, 1);
  OutPoints[0] := point(26, 8);
End;

Destructor TUserInput.Destroy;
Begin
  fUpImage.Free;
  fDownImage.Free;
  fUpImage := Nil;
  fDownImage := Nil;
  Inherited Destroy();
End;

Procedure TUserInput.SaveToStream(Const aStream: TStream);
Begin
  Inherited SaveToStream(aStream);
  aStream.Write(fState, SizeOf(fState));
End;

Procedure TUserInput.LoadFromStream(Const aStream: TStream);
Begin
  Inherited LoadFromStream(aStream);
  aStream.Read(fState, SizeOf(fState));
End;

Procedure TUserInput.RenderTo(Const aCanvas: TCanvas; aOffset: TPoint);
Begin
  Case fState Of
    sOff: acanvas.Draw(Left - aOffset.X, Top - aOffset.Y, fUpImage);
    sOn: acanvas.Draw(Left - aOffset.X, Top - aOffset.Y, fDownImage);
  End;
  If fOwner.ShowPegel Then Begin
    LineStateToCanvas(aCanvas, point(left + 26, top + 7) - aOffset, fState);
  End;
  If fOwner.ShowConnectionPoints Then Begin
    ConnectionPointToCanvas(aCanvas, point(left, top) + OutPoints[0] - aOffset);
  End;
End;

Procedure TUserInput.Click;
Begin
  Case fState Of
    sOff: fState := sOn;
    sOn: fState := sOff;
  End;
End;

Function TUserInput.GetState(aindex: Integer): Tstate;
Begin
  Result := fState;
End;

Function TUserInput.Clone: TDigimanElement;
Begin
  result := TUserInput.Create();
End;

{ TProbe }

Function TProbe.getHeight: integer;
Begin
  Result := fOnImage.Height;
End;

Function TProbe.getWidth: integer;
Begin
  Result := fOnImage.Width;
End;

Constructor TProbe.Create;
Begin
  Inherited Create();
  fOnImage := LoadImage('Probe_on.bmp');
  fOffImage := LoadImage('Probe_off.bmp');
  fUnknownImage := LoadImage('Probe_undef.bmp');
  setlength(InPoints, 1);
  InPoints[0] := point(1, 8);
  setlength(InElements, 1);
  InElements[0].Element := Nil;
  InElements[0].Index := -1;
End;

Destructor TProbe.Destroy;
Begin
  fOffImage.free;
  fOnImage.free;
  fUnknownImage.Free;

  fOffImage := Nil;
  fOnImage := Nil;
  fUnknownImage := Nil;
  Inherited Destroy();
End;

Procedure TProbe.RenderTo(Const aCanvas: TCanvas; aOffset: TPoint);
Var
  state: TState;
Begin
  If assigned(InElements[0].Element) Then Begin
    state := InElements[0].Element.GetState(InElements[0].Index);
  End
  Else Begin
    state := sUndefined;
  End;
  Case State Of
    sOff {, sOnToOff}: acanvas.Draw(Left - aOffset.X, Top - aOffset.Y, fOffImage);
    sOn {, sOffToOn}: acanvas.Draw(Left - aOffset.X, Top - aOffset.Y, fOnImage);
    sUndefined: acanvas.Draw(Left - aOffset.X, Top - aOffset.Y, fUnknownImage);
  End;
  //  If fOwner.ShowPegel Then Begin
  //    LineStateToCanvas(aCanvas, point(left + 1, top + 7) - aOffset, State);
  //  End;
  If fOwner.ShowConnectionPoints Then Begin
    ConnectionPointToCanvas(aCanvas, point(left, top) + InPoints[0] - aOffset);
  End;
End;

Function TProbe.Clone: TDigimanElement;
Begin
  result := TProbe.Create();
End;

{ TOneInOneOut }

Function TOneInOneOut.getHeight: integer;
Begin
  Result := fImage.Height;
End;

Function TOneInOneOut.getWidth: integer;
Begin
  Result := fImage.Width;
End;

Constructor TOneInOneOut.Create;
Begin
  Inherited Create();
  fImage := Nil;
  setlength(InPoints, 1);
  InPoints[0] := point(1, 8);
  setlength(InElements, 1);
  InElements[0].Element := Nil;
  InElements[0].Index := -1;
  setlength(OutPoints, 1);
  OutPoints[0] := point(30, 8);
End;

Destructor TOneInOneOut.Destroy;
Begin
  fImage.Free;
  fImage := Nil;
  Inherited Destroy();
End;

Procedure TOneInOneOut.RenderTo(Const aCanvas: TCanvas; aOffset: TPoint);
Begin
  acanvas.Draw(Left - aOffset.x, Top - aOffset.y, fImage);

  If fOwner.ShowPegel Then Begin
    LineStateToCanvas(aCanvas, point(left - 1, top - 1) + OutPoints[0] - aOffset, GetState(0));
  End;

  If fOwner.ShowConnectionPoints Then Begin
    ConnectionPointToCanvas(aCanvas, point(left, top) + InPoints[0] - aOffset);
    ConnectionPointToCanvas(aCanvas, point(left, top) + OutPoints[0] - aOffset);
  End;
End;

{ TTwoInOneOut }

Function TTwoInOneOut.getHeight: integer;
Begin
  Result := fImage.Height;
End;

Function TTwoInOneOut.getWidth: integer;
Begin
  Result := fImage.Width;
End;

Constructor TTwoInOneOut.Create();
Begin
  Inherited Create();
  fImage := Nil;
  setlength(InPoints, 2);
  InPoints[0] := point(1, 4);
  InPoints[1] := point(1, 20);
  setlength(InElements, 2);
  InElements[0].Element := Nil;
  InElements[0].Index := -1;
  InElements[1].Element := Nil;
  InElements[1].Index := -1;
  setlength(OutPoints, 1);
  OutPoints[0] := point(26, 12);
End;

Destructor TTwoInOneOut.Destroy();
Begin
  fImage.free;
  fImage := Nil;

  Inherited Destroy();
End;

Procedure TTwoInOneOut.RenderTo(Const aCanvas: TCanvas; aOffset: TPoint);
Begin
  acanvas.Draw(Left - aOffset.x, Top - aOffset.y, fImage);

  If fOwner.ShowPegel Then Begin
    LineStateToCanvas(aCanvas, point(left - 1, top - 1) + OutPoints[0] - aOffset, GetState(0));
  End;

  If fOwner.ShowConnectionPoints Then Begin
    ConnectionPointToCanvas(aCanvas, point(left, top) + InPoints[0] - aOffset);
    ConnectionPointToCanvas(aCanvas, point(left, top) + InPoints[1] - aOffset);
    ConnectionPointToCanvas(aCanvas, point(left, top) + OutPoints[0] - aOffset);
  End;
End;

{ TNot }

Constructor TNot.Create;
Begin
  Inherited Create();
  fImage := LoadImage('Not.bmp');
End;

Function TNot.Clone: TDigimanElement;
Begin
  result := TNot.Create();
End;

Function TNot.GetState(aindex: Integer): Tstate;
Begin
  If fEvaluated Then Begin
    result := fEvaluatedState;
  End
  Else Begin
    fEvaluated := true;
    Result := _Not(_in(0));
    fEvaluatedState := result;
  End;
End;

{ TAnd }

Constructor TAnd.Create;
Begin
  Inherited Create();
  fImage := LoadImage('And.bmp');
End;

Function TAnd.GetState(aOutindex: integer): Tstate;
Begin
  If fEvaluated Then Begin
    result := fEvaluatedState;
  End
  Else Begin
    fEvaluated := true;
    Result := sUndefined;
    result := _and(_In(0), _in(1));
    fEvaluatedState := result;
  End;
End;

Function TAnd.Clone: TDigimanElement;
Begin
  result := TAnd.Create();
End;

{ TOr }

Constructor TOr.Create;
Begin
  Inherited Create();
  fImage.free;
  fImage := LoadImage('Or.bmp');
End;

Function TOr.GetState(aOutindex: integer): Tstate;
Begin
  If fEvaluated Then Begin
    result := fEvaluatedState;
  End
  Else Begin
    fEvaluated := true;
    Result := sUndefined;
    result := _or(_In(0), _in(1));
    fEvaluatedState := result;
  End;
End;

Function TOr.Clone: TDigimanElement;
Begin
  Result := TOr.Create();
End;

{ TNOr }

Constructor TNor.Create;
Begin
  Inherited Create();
  fImage.free;
  fImage := LoadImage('Nor.bmp');
  OutPoints[0] := point(30, 12);
End;

Function TNor.GetState(aOutindex: integer): Tstate;
Begin
  If fEvaluated Then Begin
    result := fEvaluatedState;
  End
  Else Begin
    fEvaluated := true;
    Result := sUndefined;
    result := _not(_or(_In(0), _in(1)));
    fEvaluatedState := result;
  End;
End;

Function TNor.Clone: TDigimanElement;
Begin
  Result := TNor.Create();
End;

{ TNand }

Constructor TNand.Create;
Begin
  Inherited Create();
  fImage.free;
  fImage := LoadImage('Nand.bmp');
  OutPoints[0] := point(30, 12);
End;

Function TNand.GetState(aOutindex: integer): Tstate;
Begin
  If fEvaluated Then Begin
    result := fEvaluatedState;
  End
  Else Begin
    fEvaluated := true;
    Result := sUndefined;
    result := _not(_and(_In(0), _in(1)));
    fEvaluatedState := result;
  End;
End;

Function TNand.Clone: TDigimanElement;
Begin
  Result := TNand.Create();
End;

{ TLineCreateHelper }

Constructor TLineCreateHelper.Create;
Begin
  Mode := lcmIdle;
  fPoints := Nil;
End;

Procedure TLineCreateHelper.RenderTo(Const aCanvas: TCanvas; aOffset: TPoint);
Var
  i: Integer;
  tmp: Tpoint;
Begin
  If (mode = lcmIdle) Or (Not Assigned(fPoints)) Then exit;
  aCanvas.Pen.Color := clBlack;
  For i := 0 To high(fPoints) - 1 Do Begin
    DrawSegment(aCanvas, aOffset, fPoints[i], fPoints[i + 1]);
  End;
  tmp.x := fAktualMousePos.x - fAktualMousePos.x Mod Grid;
  tmp.Y := fAktualMousePos.Y - fAktualMousePos.Y Mod Grid;
  DrawSegment(aCanvas, aOffset, fPoints[high(fPoints)], tmp);
End;

Procedure TLineCreateHelper.SetActualMousePosition(aX, aY: integer);
Begin
  fAktualMousePos := Point(ax, ay);
End;

Procedure TLineCreateHelper.StartNewLine(StartElement: TDigimanElement;
  InIndex, OutIndex: integer);
Begin
  setlength(fPoints, 1);
  fPoints[0] := point(StartElement.Left, StartElement.Top);
  fInElement := Nil;
  fInIndex := -1;
  fOutElement := Nil;
  fOutIndex := -1;
  If InIndex <> -1 Then Begin
    fPoints[0] := fPoints[0] + StartElement.InPoints[InIndex];
    fOutElement := StartElement;
    fOutIndex := InIndex;
  End;
  If OutIndex <> -1 Then Begin
    fPoints[0] := fPoints[0] + StartElement.OutPoints[OutIndex];
    fInElement := StartElement;
    fInIndex := OutIndex;
  End;
  Mode := lcmAddCorners;
  SetActualMousePosition(fPoints[0].x, fPoints[0].y);
End;

Procedure TLineCreateHelper.AddCorner(aX, aY: integer);
Begin
  // Allign to grid
  aX := aX - aX Mod Grid;
  aY := aY - aY Mod Grid;
  setlength(fPoints, high(fPoints) + 2);
  fPoints[high(fPoints)] := point(aX, aY);
End;

Function TLineCreateHelper.EndLine(EndElement: TDigimanElement; InIndex,
  OutIndex: integer): boolean;
Var
  aLine: TLine;
  p: TPoint;
  i: Integer;
Begin
  result := false;
  If InIndex <> -1 Then Begin
    p := EndElement.InPoints[InIndex] + point(EndElement.Left, EndElement.Top);
    If assigned(fOutElement) Then exit; // Line with 2 Out Elements
    fOutElement := EndElement;
    fOutIndex := InIndex;
  End;
  If OutIndex <> -1 Then Begin
    p := EndElement.OutPoints[OutIndex] + point(EndElement.Left, EndElement.Top);
    If assigned(fInElement) Then exit; // Line with 2 In Elements
    fInElement := EndElement;
    fInIndex := OutIndex;
  End;
  aLine := TLine.Create();
  setlength(aLine.fPoints, length(fPoints) + 1);
  For i := 0 To high(fPoints) Do Begin
    aLine.fPoints[i] := fPoints[i];
  End;
  aLine.fPoints[High(aLine.fPoints)] := P;
  aLine.fInElement := fInElement;
  aLine.fInIndex := fInIndex;
  aLine.fOutElement := fOutElement;
  aLine.fOutIndex := fOutIndex;
  fInElement.fOwner.AddElement(aLine);
  result := true;
  Mode := lcmIdle;
End;

Procedure TLineCreateHelper.DelLastCorner;
Begin
  If assigned(fPoints) Then Begin
    setlength(fPoints, high(fPoints));
  End;
  If Not Assigned(fPoints) Then Mode := lcmIdle;
End;

End.

