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

  TState = (sOff, sOn, sOffToOn, sOnToOff, sUndefined);

  TDigimanElement = Class;
  Tline = Class;

  (*
   * Der Eigentliche Emulator
   *)

  { TDigiman }

  TDigiman = Class
  private
    fElements: Array Of TDigimanElement;
    fLines: Array Of Tline; //Macht das handling leichter, müsste aber nicht Extra sein ..
    Procedure initElementsIndexWithElement(Const aElement: TDigimanElement; aIndex: integer);
    Procedure initLinesIndexWithElement(Const aElement: Tline; aIndex: integer);
  protected
    Procedure RemoveAllConnectionsTo(aElement: TDigimanElement);
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

  { TDigimanElement }

  TDigimanElement = Class
  private
    fOwner: TDigiman;
  protected
    Function getHeight: integer; virtual; // Abstract
    Function getWidth: integer; virtual; // Abstract
    Function LoadImage(aFilename: String): TBitmap; // Helper function for constructors
    Procedure RemoveAllConnectionsTo(aElement: TDigimanElement); virtual;
  public
    Top: integer;
    Left: integer;

    InElements: Array Of TDigimanElement; //Jeder In Point hat ein InElement
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

    Function GetState(aindex: integer): Tstate; virtual; // Abstract

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
    fOutIndex: integer; // The Out Index of the inElement
    fInElement: TDigimanElement;
    fOutElement: TDigimanElement;
  public
    Constructor Create(); override;

    Procedure RenderTo(Const aCanvas: TCanvas; aOffset: TPoint); override;

    Function GetState(aindex: integer): Tstate; override;
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
    fState: TState;
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
  End;

  { TAnd }

  TAnd = Class(TTwoInOneOut)
  protected
  public
    Constructor Create(); override;

    Function Clone: TDigimanElement; override;
  End;

  { TOr }

  TOr = Class(TTwoInOneOut)
  public
    Constructor Create(); override;

    Function Clone: TDigimanElement; override;
  End;

  { TNor }

  TNor = Class(TTwoInOneOut)
  public
    Constructor Create(); override;

    Function Clone: TDigimanElement; override;
  End;

  { TNand }

  TNand = Class(TTwoInOneOut)
  public
    Constructor Create(); override;

    Function Clone: TDigimanElement; override;
  End;

  TLineCreateMode = (lcmIdle, lcmAddCorners);

  { TLineCreateHelper }

  TLineCreateHelper = Class
  private
    fPoints: Array Of TPoint;
    fOutIndex: integer; // The Out Index of the inElement
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
  Else Begin
      Raise exception.create('Error: ' + ClassName + ' not implemented in CreateDigimanElemenFromString');
    End;
  End;
End;

Procedure LineStateToCanvas(Const aCanvas: TCanvas; aPos: Tpoint; aState: TState);
Begin
  If aState In [sOffToOn, sUndefined, sOn] Then Begin
    acanvas.Pixels[aPos.x, apos.y] := clRed;
    acanvas.Pixels[aPos.x + 1, apos.y] := clRed;
    acanvas.Pixels[aPos.x, apos.y - 1] := clRed;
    acanvas.Pixels[aPos.x + 1, apos.y - 1] := clRed;
    acanvas.Pixels[aPos.x, apos.y - 4] := clRed;
    acanvas.Pixels[aPos.x + 1, apos.y - 4] := clRed;
    acanvas.Pixels[aPos.x, apos.y - 5] := clRed;
    acanvas.Pixels[aPos.x + 1, apos.y - 5] := clRed;
  End;
  If aState In [sOffToOn, sOn] Then Begin
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

Procedure DrawSegment(Const aCanvas: TCanvas; aOffset: TPoint; aFrom, aTo: Tpoint);
Var
  dx: integer;
Begin
  aFrom := aFrom + aOffset;
  aTo := aTo + aOffset;
  dx := ato.X - aFrom.X;
  // First Horizontal
  aCanvas.Line(aFrom.X, aFrom.Y, aFrom.X + dx, aFrom.Y);
  // Second Vertical
  aCanvas.Line(aFrom.X + dx, aFrom.Y, aTo.X, aTo.Y);
End;

{ TDigimanElement }

Constructor TDigimanElement.Create;
Begin
  Inherited create;
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
    If InElements[i] = aElement Then InElements[i] := Nil;
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

Function TDigimanElement.GetState(aindex: integer): Tstate;
Begin
  result := sUndefined;
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

Constructor TTool.Create();
Begin
  Inherited Create();
  fImage := Nil;
End;

Destructor TTool.Destroy();
Begin
  fImage.Free;
  fImage := Nil;
  Inherited Destroy();
End;

Procedure TTool.RenderTo(Const aCanvas: TCanvas; aOffset: TPoint);
Begin
  acanvas.Draw(Left - aOffset.x, Top - aOffset.y, fImage);
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

Procedure TLine.RenderTo(Const aCanvas: TCanvas; aOffset: TPoint);
Var
  i: Integer;
Begin
  aCanvas.Pen.Color := clBlack;
  For i := 0 To high(fPoints) - 1 Do Begin
    DrawSegment(aCanvas, aOffset, fPoints[i], fPoints[i + 1]);
  End;
End;

Function TLine.GetState(aindex: integer): Tstate;
Begin
  Result := fInElement.GetState(fOutIndex);
End;

Procedure TDigiman.initElementsIndexWithElement(
  Const aElement: TDigimanElement; aIndex: integer);
Begin
  aElement.fOwner := self;
  fElements[aIndex] := aElement;
End;

Procedure TDigiman.initLinesIndexWithElement(Const aElement: TLine;
  aIndex: integer);
Begin
  aElement.fOwner := self;
  fLines[aIndex] := aElement;
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

Constructor TDigiman.Create;
Begin
  Inherited Create();
  fElements := Nil;
  fLines := Nil;
  ShowPegel := true;
  ShowConnectionPoints := false;
End;

Destructor TDigiman.Destroy;
Begin
  Clear;
End;

Procedure TDigiman.RenderTo(Const aCanvas: TCanvas; aOffset: TPoint);
Var
  i: Integer;
Begin
  For i := 0 To high(fLines) Do Begin
    fLines[i].RenderTo(aCanvas, aOffset);
  End;
  For i := 0 To high(fElements) Do Begin
    fElements[i].RenderTo(aCanvas, aOffset);
  End;
End;

Procedure TDigiman.AddElement(Const aElement: TDigimanElement);
Begin
  If aElement Is TLine Then Begin
    setlength(fLines, high(fLines) + 2);
    initLinesIndexWithElement(aElement As TLine, high(fLines));
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
        // TODO: löschen aller Linien die mit aElement zu tun haben

        fLines[i].Free;
        For j := i To high(fLines) - 1 Do Begin
          fLines[j] := fLines[j + 1];
        End;
        SetLength(fLines, high(fLines));
        exit;
      End;
    End;
  End
  Else Begin
    For i := 0 To high(fElements) Do Begin
      If fElements[i] = aElement Then Begin
        // TODO: löschen aller Linien die mit aElement zu tun haben

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
End;

Procedure TDigiman.SaveToFile(Const aFilename: String);
Var
  m: TMemoryStream;
  i: Integer;
Begin
  m := TMemoryStream.Create;
  m.Write(FileVersion, sizeof(FileVersion));
  i := length(fElements);
  m.Write(i, SizeOf(i));
  For i := 0 To high(fElements) Do Begin
    m.WriteAnsiString(fElements[i].ClassName);
    fElements[i].saveToStream(m);
  End;

  // TODO: Implementieren

  m.SaveToFile(aFilename);
  m.free;
End;

Procedure TDigiman.LoadFromFile(Const aFilename: String);
Var
  m: TMemoryStream;
  i, FV: integer;
  e: TDigimanElement;
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

  // Todo: Implementieren

  m.free;
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
  fState := sUndefined;
  setlength(InPoints, 1);
  InPoints[0] := point(1, 8);
  setlength(InElements, 1);
  InElements[0] := Nil;
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
Begin
  Case fState Of
    sOff, sOnToOff: acanvas.Draw(Left - aOffset.X, Top - aOffset.Y, fOffImage);
    sOn, sOffToOn: acanvas.Draw(Left - aOffset.X, Top - aOffset.Y, fOnImage);
    sUndefined: acanvas.Draw(Left - aOffset.X, Top - aOffset.Y, fUnknownImage);
  End;
  If fOwner.ShowPegel Then Begin
    LineStateToCanvas(aCanvas, point(left + 1, top + 7) - aOffset, fState);
  End;
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
  InElements[0] := Nil;
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
  InElements[0] := Nil;
  InElements[1] := Nil;
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

{ TAnd }

Constructor TAnd.Create;
Begin
  Inherited Create();
  fImage := LoadImage('And.bmp');
End;

Function TAnd.Clone: TDigimanElement;
Begin
  result := TAnd.Create();
End;

{ TOr }

Constructor TOr.Create();
Begin
  Inherited Create();
  fImage.free;
  fImage := LoadImage('Or.bmp');
End;

Function TOr.Clone: TDigimanElement;
Begin
  Result := TOr.Create();
End;

{ TNOr }

Constructor TNOr.Create();
Begin
  Inherited Create();
  fImage.free;
  fImage := LoadImage('Nor.bmp');
  OutPoints[0] := point(30, 12);
End;

Function TNOr.Clone: TDigimanElement;
Begin
  Result := TNor.Create();
End;

{ TNand }

Constructor TNand.Create();
Begin
  Inherited Create();
  fImage.free;
  fImage := LoadImage('Nand.bmp');
  OutPoints[0] := point(30, 12);
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
  fOutElement := Nil;
  fOutIndex := -1;
  If InIndex <> -1 Then Begin
    fPoints[0] := fPoints[0] + StartElement.InPoints[InIndex];
    fOutElement := StartElement;
  End;
  If OutIndex <> -1 Then Begin
    fPoints[0] := fPoints[0] + StartElement.OutPoints[OutIndex];
    fInElement := StartElement;
    fOutIndex := OutIndex;
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
  End;
  If OutIndex <> -1 Then Begin
    p := EndElement.OutPoints[OutIndex] + point(EndElement.Left, EndElement.Top);
    If assigned(fInElement) Then exit; // Line with 2 In Elements
    fInElement := EndElement;
    fOutIndex := InIndex;
  End;
  aLine := TLine.Create();
  setlength(aLine.fPoints, length(fPoints) + 1);
  For i := 0 To high(fPoints) Do Begin
    aLine.fPoints[i] := fPoints[i];
  End;
  aLine.fPoints[High(aLine.fPoints)] := P;
  aLine.fInElement := fInElement;
  aLine.fOutIndex := fOutIndex;
  aLine.fOutElement := fOutElement;
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

