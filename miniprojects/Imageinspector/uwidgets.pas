(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of ImageInspector                                        *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit uwidgets;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, controls, SysUtils, Graphics, ExtCtrls, grids
  , uvectormath
  , ueventer;

Const
  GridIndexID = 0;
  GridIndexObjType = 1;
  GridIndexMUnit = 2;
  GridIndexValue = 3;
  GridIndexUnit = 4;

Type

  TMeasureElement = Class;
  TMeasureElementClass = Class Of TMeasureElement;

  TPreviewKind = (pkNone, pkLine, pkMultiLine, pkPolygon, pkRectangle, pkLinePoint, pk2PointCircle, pk3PointCircle, pkAngle, pkArc, pkArrow, pkText);

  TPointArray = Array Of TPoint;

  { TKnob }

  TKnob = Class(TEventerClass)
  private
    fParent: TMeasureElement;
    dx, dy: integer;
  protected
    Function GetClientRect: Trect; override;
    Procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    Procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
  public
    Constructor Create(Owner: TPaintBox); override;
    Procedure Render; virtual;
  End;

  { TTextField }

  TTextField = Class(TKnob)
  private
    fText: String;
    Procedure setText(AValue: String);
    Function TextHeight(Text: String): integer;
    Function TextWidth(Text: String): Integer;
  protected
    Procedure DblClick; override;
  public
    ReadOnly: Boolean;
    Property Text: String read fText write setText;
    Constructor Create(Owner: TPaintBox); override;
    Procedure Render; override;
  End;

  { TMeasureElement }

  TMeasureElement = Class // Basisklasse für Alle Measure Elemente
  private
    fOwner: TPaintBox;
    fText: TTextField;
    FTextInitialized: Boolean;
    fPoints: Array Of TKnob;
    fOnchange: TNotifyEvent;
    Function getEnabled: Boolean; virtual;
    Function getTextvisible: Boolean; virtual;
    Function getVisible: Boolean; virtual;
    Procedure setEnabled(AValue: Boolean); virtual;
    Procedure setTextvisible(AValue: Boolean); virtual;
    Procedure setVisible(AValue: Boolean); virtual;
    Function ClassToIdentifier(): int32;
  protected
    Procedure RefreshByKnob(Sender: TKnob); virtual;
    Procedure LoadFromStream(Const Stream: TStream); virtual; overload; // Darf nur aus "Class Function LoadFromStream" heraus aufgerufen werden !!
  public
    (*
      ID: Integer; // Zur Identifizierung
      Name:String; // Zur Identifizierung
    *)
    Property OnChange: TNotifyEvent read fOnchange write fOnchange;
    Property Enabled: Boolean read getEnabled write setEnabled;
    Property TextVisible: Boolean read getTextvisible write setTextvisible;
    Property Visible: Boolean read getVisible write setVisible;

    Constructor Create(Owner: TPaintBox); virtual;
    Destructor Destroy(); override;

    Procedure InitText(); virtual;
    Procedure RefreshText(); virtual; // Abstract
    Procedure Render; virtual; // Abstract

    Procedure AddInfoToStringgrid(Const Stringgrid: TStringGrid; Index: integer; Line: integer = -1); virtual; //abstract;

    Function Hit(x, y: Integer): Boolean; virtual; // True, wenn das Element in Pixelkoordinaten angeklickt werden "könnte" (geht ja net, weils
    Procedure SetPoints(Points: TPointArray); virtual;
    Function GetPoints(): TPointArray; virtual;

    Procedure SaveToStream(Const Stream: TStream); virtual;
    Class Function LoadFromStream(Const Stream: TStream; Owner: TPaintBox): TMeasureElement; overload;
  End;

  { TMultiLine }

  TMultiLine = Class(TMeasureElement)
  private
    Function GetLength(): Single;
  public
    Procedure AddInfoToStringgrid(Const Stringgrid: TStringGrid; Index: integer; Line: integer = -1); override;
    Procedure RefreshText(); override;
    Procedure Render; override;
  End;

  { TPolygon }

  TPolygon = Class(TMultiLine)
  private
    Function GetArea(): Single;
  public
    Procedure AddInfoToStringgrid(Const Stringgrid: TStringGrid; Index: integer; Line: integer = -1); override;
    Procedure RefreshText(); override;
    Procedure Render; override;
  End;

  { TRectangle }

  TRectangle = Class(TMeasureElement)
  private
    p1, p2: TKnob;
    Procedure setEnabled(AValue: Boolean); override;
    Function GetWidth(): Single;
    Function GetHeight(): Single;
    Function GetArea(): Single;
    Procedure setVisible(AValue: Boolean); override;

  protected
    Procedure RefreshByKnob(Sender: TKnob); override;
  public
    Procedure AddInfoToStringgrid(Const Stringgrid: TStringGrid; Index: integer; Line: integer = -1); override;

    Procedure RefreshText(); override;
    Procedure Render; override;

    Constructor Create(Owner: TPaintBox); override;
    Destructor Destroy(); override;

    Function Hit(x, y: Integer): Boolean; override;
    Procedure SetPoints(Points: TPointArray); override;
  End;

  { TLinePoint }

  TLinePoint = Class(TMeasureElement)
  private
    fLP: TKnob;
    Procedure setEnabled(AValue: Boolean); override;
    Function GetLength(): Single;
  protected
    Procedure RefreshByKnob(Sender: TKnob); override;
  public

    Constructor Create(Owner: TPaintBox); override;
    Destructor Destroy(); override;

    Procedure AddInfoToStringgrid(Const Stringgrid: TStringGrid; Index: integer; Line: integer = -1); override;

    Procedure RefreshText(); override;
    Procedure Render; override;
    Function Hit(x, y: Integer): Boolean; override;
    Procedure SetPoints(Points: TPointArray); override;
  End;

  { T2PointCircle }

  T2PointCircle = Class(TMeasureElement)
  private
    fMiddle: TKnob;
    Procedure setEnabled(AValue: Boolean); override;
    Function GetCircumfence(): Single;
    Function GetArea(): Single;
    Function GetDiameter(): Single;
  protected
    Procedure RefreshByKnob(Sender: TKnob); override;
  public

    Constructor Create(Owner: TPaintBox); override;
    Destructor Destroy(); override;

    Procedure AddInfoToStringgrid(Const Stringgrid: TStringGrid; Index: integer; Line: integer = -1); override;

    Procedure RefreshText(); override;
    Procedure Render; override;
    Function Hit(x, y: Integer): Boolean; override;
    Procedure SetPoints(Points: TPointArray); override;
  End;

  { T3PointCircle }

  T3PointCircle = Class(TMeasureElement)
  private
    fMiddle: TKnob;
    Procedure setEnabled(AValue: Boolean); override;
    Procedure CalcMiddle();
    Function GetCircumfence(): Single;
    Function GetArea(): Single;
    Function GetDiameter(): Single;

  protected
    Procedure RefreshByKnob(Sender: TKnob); override;
  public

    Constructor Create(Owner: TPaintBox); override;
    Destructor Destroy(); override;

    Procedure AddInfoToStringgrid(Const Stringgrid: TStringGrid; Index: integer; Line: integer = -1); override;

    Procedure RefreshText(); override;
    Procedure Render; override;
    Function Hit(x, y: Integer): Boolean; override;
    Procedure SetPoints(Points: TPointArray); override;
  End;

  { TAngle }

  TAngle = Class(TMeasureElement)
  private
    Function GetAngle(): Single;
  public
    Procedure AddInfoToStringgrid(Const Stringgrid: TStringGrid; Index: integer; Line: integer = -1); override;
    Procedure RefreshText(); override;
    Procedure Render; override;
  End;

  { TArc }

  TArc = Class(T3PointCircle)
  private
    Function GetAngle(): Single;
    Function GetLength(): Single;
    Function GetRadius(): Single;
  public
    Procedure AddInfoToStringgrid(Const Stringgrid: TStringGrid; Index: integer; Line: integer = -1); override;
    Procedure RefreshText(); override;
    Procedure Render; override;
  End;

  { TText }

  TText = Class(TMeasureElement)
  private
  protected
    Function getEnabled: Boolean; override;

    Procedure LoadFromStream(Const Stream: TStream); override;
  public
    Procedure AddInfoToStringgrid(Const Stringgrid: TStringGrid; Index: integer; Line: integer = -1); override;
    Procedure SetText(Text: String);
    Function GetText(): String;
    Procedure RefreshText(); override;
    Procedure Render; override;
    Procedure SaveToStream(Const Stream: TStream); override;
  End;

  { TArrow }

  TArrow = Class(TMeasureElement)
  private
  protected
  public
    Procedure AddInfoToStringgrid(Const Stringgrid: TStringGrid; Index: integer; Line: integer = -1); override;

    Procedure RefreshText(); override;
    Procedure Render; override;
  End;

  { TMetrik }

  TMetrik = Class(TMultiLine)
  private
    fDistance: Single;
    Procedure setDistance(aValue: Single); // Definiert die Länge der Strecke p1, p2
  protected
    Procedure LoadFromStream(Const Stream: TStream); override;

  public
    Property Distance: Single read fDistance write setDistance;

    Constructor Create(Owner: TPaintBox); override;

    Procedure RefreshText(); override; // !! ACHTUNG !! das wird misbraucht zum Auslosen des OnChanged Events, die Metrik selbst hat keinen Text !!

    Function PixelToDistance(Len: Single): Single;
    Function PixelToArea(Area: Single): Single;

    Procedure SaveToStream(Const Stream: TStream); override;
  End;

  { TPerspectiveCorrection }

  TPerspectiveCorrection = Class(TMeasureElement)
  private
  public
    Constructor Create(Owner: TPaintBox); override;

    Procedure RefreshText(); override;
    Procedure Render; override;
  End;

  { TRotateCorrection }

  TRotateCorrection = Class(TMeasureElement)
  public
    Constructor Create(Owner: TPaintBox); override;

    Procedure RefreshText(); override;
    Procedure Render; override;
  End;

  { THeightCorrection }

  THeightCorrection = Class(TMeasureElement)
  private
    fText2: TTextField;
    Procedure setVisible(AValue: Boolean); override;
    Procedure UpdateTextPositions;
    Function GetTexts(Index: integer): String;
    Procedure SetTexts(Index: integer; aValue: String);
  protected
    Procedure RefreshByKnob(Sender: TKnob); override;
  public
    Property Texts[Index: integer]: String read GetTexts write SetTexts;
    Constructor Create(Owner: TPaintBox); override;
    Destructor Destroy(); override;

    Procedure RefreshText(); override;
    Procedure Render; override;

    Procedure SetPoints(Points: TPointArray); override;

  End;

  { TPreview }

  TPreview = Class
  private
  public
    aMousePos: TPoint; // Fimage Pixelpunkte
    Points: TPointArray; // Fimage Pixelpunkte
    Kind: TPreviewKind;
    Constructor Create();
    Destructor Destroy(); override;

    Procedure Render(Const Canvas: TCanvas);
    Procedure Clear;
  End;

  (*
   *      vmin / rmin               vmax / rmax
   *       |                                |
   *    ---------------------------------------
   *                  |
   *                 v / result
   *
   * Berechnet den Wert von result so, dass er an der entsprechEnd
   * gleichen Stelle in rmin / rmax liegt, wie  v in vmin / vmax ist.
   *)

Function convert_dimension(vmin, vmax, v: Single; rmin, rmax: Single): Single;
Procedure Nop; // Zum Debuggen

Function TextWidth(Const Canvas: TCanvas; Text: String): Integer;
Function TextHeight(Const Canvas: TCanvas; Text: String): integer;

Implementation

// TODO: das ist "Eklig" das sollte wieder raus ..
Uses
  unit1 // Wegen InverseTransformRoutine, PixelToDistance, GetPixelUnit, GetPixelAreaUnit
  , math,
  ugraphics
  ;

Type
  TWrapper = Record
    Index: Int32;
    Cls: TMeasureElementClass;
  End;

Const

  (*
   * Fürs laden und Speichern, alle Index müssen disjunkt und >= 0 sein !!
   *)
  MeasureElementIdentifier: Array[0..10] Of TWrapper =
  (
    (index: 0; Cls: TMultiLine),
    (index: 1; Cls: TMetrik),
    (index: 2; Cls: TPolygon),
    (index: 3; Cls: TRectangle),
    (index: 4; Cls: TLinePoint),
    (index: 5; Cls: T2PointCircle),
    (index: 6; Cls: T3PointCircle),
    (index: 7; Cls: TAngle),
    (index: 8; Cls: TArc),
    (index: 9; Cls: TText),
    (index: 10; Cls: TArrow)
    );

Procedure Nop;
Begin

End;

Function TextHeight(Const Canvas: TCanvas; Text: String): integer;
Var
  Lines: integer;
Begin
  lines := 1;
  While pos(LineEnding, text) <> 0 Do Begin
    inc(lines);
    delete(Text, pos(LineEnding, text), length(LineEnding));
  End;
  result := Canvas.TextHeight('Aq') * lines;
End;

Function TextWidth(Const Canvas: TCanvas; Text: String): Integer;
Var
  s, t: String;
Begin
  result := 0;
  s := Text + LineEnding;
  While pos(LineEnding, s) <> 0 Do Begin
    t := copy(s, 1, pos(LineEnding, s) - 1);
    delete(s, 1, pos(LineEnding, s) + Length(LineEnding) - 1);
    result := max(result, Canvas.TextWidth(t));
  End;
End;

Function IdentifierToClass(Identifier: int32): TMeasureElementClass;
Var
  i: Integer;
Begin
  result := Nil;
  For i := low(MeasureElementIdentifier) To high(MeasureElementIdentifier) Do Begin
    If Identifier = MeasureElementIdentifier[i].Index Then Begin
      result := MeasureElementIdentifier[i].Cls;
      exit;
    End;
  End;
  Raise Exception.Create('Error could not evaluate: ' + inttostr(Identifier) + ' to a existing class.');
End;

Function convert_dimension(vmin, vmax, v: Single; rmin, rmax: Single): Single;
Begin
  If (vmax - vmin = 0) Then Begin // Div by 0 abfangen
    result := rmin;
    exit;
  End
  Else Begin
    result := ((((v - vmin) * (rmax - rmin)) / (vmax - vmin)) + rmin);
  End;
End;

Procedure RenderCircle(Const Canvas: TCanvas; M: Tpoint; r: integer);
Var
  d, rr, mm: TPoint;
Begin
  mm := InverseTransformRoutine(m.x, m.y);
  rr := InverseTransformRoutine(r, r, true);
  d := Point(5, 5);
  Canvas.brush.Style := bsClear;
  Canvas.Ellipse(mm.x - rr.x + d.x, mm.y - rr.y + d.y, mm.x + rr.x + d.x, mm.y + rr.y + d.y);
  Canvas.brush.Style := bsSolid;
End;

(*
 * Zeichnet einen Kreisbogen mit Radius r um Center, die Gerade Center /Start und Center Stop geben die start und Entpunkte des Kreisbogens an.
 *)

Procedure RenderArc(Const Canvas: TCanvas; Center: TPoint; r: integer; Start, Stop: TPoint);
Var
  st, sp, d, rr, mm: TPoint;
Begin
  // TODO: Das Problem ist beim nicht 1:1 Scallieren verzieht es den Arc leider :(
  mm := InverseTransformRoutine(Center.x, Center.y);
  rr := InverseTransformRoutine(r, r, true);
  d := point(5, 5);
  sp := InverseTransformRoutine(Start.x, Start.y);
  st := InverseTransformRoutine(Stop.x, Stop.y);
  // TODO: Eigentlich sollt der Brush unsichtbar sein, das klappt aber aus irgend einem Grund nicht :(
  Canvas.brush.Style := bsClear;
  Canvas.Arc(mm.x - rr.x + d.x, mm.y - rr.y + d.y, mm.x + rr.x + d.x, mm.y + rr.y + d.y, st.X, st.y, sp.x, sp.y);
  Canvas.brush.Style := bsSolid;
End;

Procedure RenderLine(Const Canvas: TCanvas; p1, p2: TPoint);
Var
  d, pp1, pp2: Tpoint;
Begin
  pp1 := InverseTransformRoutine(p1.x, p1.y);
  pp2 := InverseTransformRoutine(p2.x, p2.y);
  d := Point(5, 5);
  Canvas.Line(pp1.x + d.x, pp1.y + d.y, pp2.x + d.x, pp2.y + d.y);
End;

Function toPoint(Const Knob: TKnob): Tpoint;
Begin
  result.x := Knob.Left;
  result.Y := Knob.Top;
End;

{ TRotateCorrection }

Constructor TRotateCorrection.Create(Owner: TPaintBox);
Var
  pa: TPointArray;
  i: Integer;
Begin
  Inherited Create(Owner);
  pa := Nil;
  setlength(pa, 2);
  For i := 0 To 1 Do Begin
    pa[i] := point(0, 0);
  End;
  SetPoints(pa); // Sicherstellen, dass immer 4 Knopf Elemente da sind, wenn auch ungenutzt
  Visible := false;
  ;
End;

Procedure TRotateCorrection.RefreshText();
Begin

End;

Procedure TRotateCorrection.Render;
Var
  i: Integer;
Begin
  If Visible Then Begin
    If Enabled Then Begin
      fOwner.Canvas.pen.Color := clGreen;
    End
    Else Begin
      fOwner.Canvas.pen.Color := clred;
    End;
    RenderLine(fOwner.Canvas, toPoint(fPoints[0]), toPoint(fPoints[1]));
  End;
  For i := 0 To 1 Do Begin
    fPoints[i].Render;
  End;
End;

{ THeightCorrection }

Procedure THeightCorrection.setVisible(AValue: Boolean);
Begin
  Inherited setVisible(avalue);
  fText2.Visible := AValue;
End;

Procedure THeightCorrection.SetPoints(Points: TPointArray);
Begin
  Inherited SetPoints(Points);
  UpdateTextPositions();
End;

Procedure THeightCorrection.RefreshByKnob(Sender: TKnob);
Begin
  Inherited RefreshByKnob(Sender);
  UpdateTextPositions();
End;

Procedure THeightCorrection.UpdateTextPositions;
Begin
  fText.Top := (fPoints[0].Top + fPoints[1].Top) Div 2;
  fText.Left := (fPoints[0].Left + fPoints[1].Left) Div 2;
  fText2.Top := (fPoints[2].Top + fPoints[3].Top) Div 2;
  fText2.Left := (fPoints[2].Left + fPoints[3].Left) Div 2;
End;

Function THeightCorrection.GetTexts(Index: integer): String;
Begin
  result := '';
  If index = 0 Then result := fText.Text;
  If index = 1 Then result := fText2.Text;
End;

Procedure THeightCorrection.SetTexts(Index: integer; aValue: String);
Begin
  If index = 0 Then fText.Text := aValue;
  If index = 1 Then fText2.Text := aValue;
End;

Constructor THeightCorrection.Create(Owner: TPaintBox);
Var
  pa: TPointArray;
  i: Integer;
Begin
  Inherited Create(Owner);
  fText2 := TTextField.Create(Owner);
  fText.ReadOnly := false;
  fText2.ReadOnly := false;
  pa := Nil;
  setlength(pa, 4);
  For i := 0 To 3 Do Begin
    pa[i] := point(0, 0);
  End;
  SetPoints(pa); // Sicherstellen, dass immer 3 Knopf Elemente da sind, wenn auch ungenutzt
  InitText();
  Visible := false;
End;

Destructor THeightCorrection.Destroy();
Begin
  fText2.Free;
  Inherited Destroy();
End;

Procedure THeightCorrection.RefreshText();
Begin
End;

Procedure THeightCorrection.Render;
Var
  i: Integer;
Begin
  If Visible Then Begin
    If Enabled Then Begin
      fOwner.Canvas.pen.Color := clGreen;
    End
    Else Begin
      fOwner.Canvas.pen.Color := clred;
    End;
    RenderLine(fOwner.Canvas, toPoint(fPoints[0]), toPoint(fPoints[1]));
    RenderLine(fOwner.Canvas, toPoint(fPoints[2]), toPoint(fPoints[3]));
  End;
  For i := 0 To 3 Do Begin
    fPoints[i].Render;
  End;
  fText.Render;
  fText2.Render;
End;

{ TPerspectiveCorrection }

Constructor TPerspectiveCorrection.Create(Owner: TPaintBox);
Var
  pa: TPointArray;
  i: Integer;
Begin
  Inherited Create(Owner);
  pa := Nil;
  setlength(pa, 4);
  For i := 0 To 3 Do Begin
    pa[i] := point(0, 0);
  End;
  SetPoints(pa); // Sicherstellen, dass immer 4 Knopf Elemente da sind, wenn auch ungenutzt
  Visible := false;
End;

Procedure TPerspectiveCorrection.RefreshText();
Begin
  // Haben wir keinen
End;

Procedure TPerspectiveCorrection.Render;
Var
  i: Integer;
Begin
  If Visible Then Begin
    If Enabled Then Begin
      fOwner.Canvas.pen.Color := clGreen;
    End
    Else Begin
      fOwner.Canvas.pen.Color := clred;
    End;
    RenderLine(fOwner.Canvas, toPoint(fPoints[0]), toPoint(fPoints[1]));
    RenderLine(fOwner.Canvas, toPoint(fPoints[1]), toPoint(fPoints[3]));
    RenderLine(fOwner.Canvas, toPoint(fPoints[2]), toPoint(fPoints[3]));
    RenderLine(fOwner.Canvas, toPoint(fPoints[2]), toPoint(fPoints[0]));
  End;
  For i := 0 To 3 Do Begin
    fPoints[i].Render;
  End;
End;

{ TArrow }

Procedure TArrow.AddInfoToStringgrid(Const Stringgrid: TStringGrid;
  Index: integer; Line: integer);
Begin
  // Nichts, der hat keinen Eintrag im CSV
End;

Procedure TArrow.RefreshText();
Begin
  fText.Visible := false; // Der Pfeil hat keinen Text
End;

Procedure TArrow.Render;
Var
  p1, p2, d: TPoint;
Begin
  If Enabled Then Begin
    fOwner.Canvas.pen.Color := clGreen;
  End
  Else Begin
    fOwner.Canvas.pen.Color := clred;
  End;
  fOwner.Canvas.pen.Width := 2;
  p1 := InverseTransformRoutine(fPoints[0].Left, fPoints[0].Top);
  p2 := InverseTransformRoutine(fPoints[1].Left, fPoints[1].Top);
  d := point(5, 5);
  RenderArrow(fOwner.Canvas, p1 + d, p2 + d);
  fOwner.Canvas.pen.Width := 1;
  // Der Pfeil Rendert seine Knöpfe nicht, obwohl sie da sind ;)
  //  fPoints[0].Render;
  //  fPoints[1].Render;
End;

{ TText }

Function TText.getEnabled: Boolean;
Begin
  Result := Inherited getEnabled Or fText.Enabled;
End;

Procedure TText.LoadFromStream(Const Stream: TStream);
Var
  s: String;
Begin
  Inherited LoadFromStream(Stream);
  s := Stream.ReadAnsiString;
  If LineEnding <> #10 Then Begin
    s := StringReplace(s, #10, LineEnding, [rfReplaceAll]);
  End;
  fText.Text := s;
End;

Procedure TText.AddInfoToStringgrid(Const Stringgrid: TStringGrid;
  Index: integer; Line: integer);
Begin
  // Nichts, der hat keinen Eintrag im CSV
End;

Procedure TText.SetText(Text: String);
Var
  i: Integer;
Begin
  fText.Text := Text;
  // Sobald der Text gesetzt wurde benötigen wir den Punkt nicht mehr
  For i := 0 To high(fPoints) Do Begin
    fPoints[i].Free;
  End;
  setlength(fPoints, 0);
End;

Function TText.GetText(): String;
Begin
  result := fText.Text;
End;

Procedure TText.RefreshText();
Begin
  // Nichts zu tun, Text wurde durch User eingegeben
End;

Procedure TText.Render;
Begin
  fText.Render;
End;

Procedure TText.SaveToStream(Const Stream: TStream);
Var
  s: String;
Begin
  Inherited SaveToStream(Stream);
  s := fText.Text;
  If LineEnding <> #10 Then Begin
    s := StringReplace(s, LineEnding, #10, [rfReplaceAll]);
  End;
  stream.WriteAnsiString(s);
End;

{ TArc }

Function TArc.GetAngle(): Single;
Var
  t, p1, p2, p3, c: TVector2;
Begin
  // Die Drehung der Punkte muss leider immer On the fly gemacht werden
  p1 := toPoint(fPoints[0]);
  p2 := toPoint(fPoints[1]);
  p3 := toPoint(fPoints[2]);
  c := toPoint(fMiddle);
  If AngleV2_2(p1 - c, p3 - c) < AngleV2_2(p1 - c, p2 - c) Then Begin
    t := p3;
    p3 := p2;
    p2 := t;
  End;
  Result := AngleV2_2(p1 - c, p3 - c);
End;

Function TArc.GetLength(): Single;
Var
  u, a: Single;
Begin
  a := GetAngle();
  u := 2 * pi * GetRadius();
  result := (u * a) / 360;
End;

Function TArc.GetRadius(): Single;
Var
  p1, c: TVector2;
Begin
  p1 := toPoint(fPoints[0]);
  c := toPoint(fMiddle);
  result := PixelToDistance(abs(p1 - c));
End;

Procedure TArc.AddInfoToStringgrid(Const Stringgrid: TStringGrid;
  Index: integer; Line: integer);
Var
  i: integer;
Begin
  If line = -1 Then Begin
    i := Stringgrid.RowCount;
    Stringgrid.RowCount := i + 3;
  End
  Else Begin
    i := Line;
  End;
  Stringgrid.Cells[GridIndexID, i] := inttostr(Index);
  Stringgrid.Cells[GridIndexObjType, i] := '3-Point-Arc';
  Stringgrid.Cells[GridIndexMUnit, i] := 'Length';
  Stringgrid.Cells[GridIndexValue, i] := format('%0.2f', [GetLength()]);
  Stringgrid.Cells[GridIndexUnit, i] := GetPixelUnit();

  Stringgrid.Cells[GridIndexID, i + 1] := inttostr(Index);
  Stringgrid.Cells[GridIndexObjType, i + 1] := '3-Point-Arc';
  Stringgrid.Cells[GridIndexMUnit, i + 1] := 'Angle';
  Stringgrid.Cells[GridIndexValue, i + 1] := format('%0.2f', [GetAngle()]);
  Stringgrid.Cells[GridIndexUnit, i + 1] := '°';

  Stringgrid.Cells[GridIndexID, i + 2] := inttostr(Index);
  Stringgrid.Cells[GridIndexObjType, i + 2] := '3-Point-Arc';
  Stringgrid.Cells[GridIndexMUnit, i + 2] := 'Radius';
  Stringgrid.Cells[GridIndexValue, i + 2] := format('%0.2f', [GetRadius()]);
  Stringgrid.Cells[GridIndexUnit, i + 2] := GetPixelUnit();
End;

Procedure TArc.RefreshText();
Begin
  fText.Text :=
    format('Length: %0.2f %s', [GetLength(), GetPixelUnit()]) + LineEnding +
  format('Angle: %0.2f °', [GetAngle()]) + LineEnding +
  format('Radius: %0.2f %s', [GetRadius(), GetPixelUnit()]);
End;

Procedure TArc.Render;
Var
  t, p1, p2, p3, c: TVector2;
Begin
  If Not Visible Then exit;
  If Enabled Then Begin
    fOwner.Canvas.pen.Color := clGreen;
  End
  Else Begin
    fOwner.Canvas.pen.Color := clred;
  End;
  // Die Drehung der Punkte muss leider immer On the fly gemacht werden
  p1 := toPoint(fPoints[0]);
  p2 := toPoint(fPoints[1]);
  p3 := toPoint(fPoints[2]);
  c := toPoint(fMiddle);
  If AngleV2_2(p1 - c, p3 - c) < AngleV2_2(p1 - c, p2 - c) Then Begin
    t := p3;
    p3 := p2;
    p2 := t;
  End;
  fOwner.Canvas.pen.Width := 2;
  fOwner.Canvas.Brush.Color := clWhite;
  RenderArc(fOwner.Canvas, c, round(abs(p1 - c)), p1, p3);
  fOwner.Canvas.pen.Width := 1;
  fOwner.Canvas.Pen.Style := psDot;
  RenderLine(fOwner.Canvas, p1, c);
  RenderLine(fOwner.Canvas, c, p3);
  fOwner.Canvas.Pen.Style := psSolid;
  fPoints[0].Render;
  fPoints[1].Render;
  fPoints[2].Render;
  fMiddle.Render;
  fText.Render;
End;

{ TAngle }

Function TAngle.GetAngle(): Single;
Var
  vv1, vv2: TVector2;
Begin
  vv1 := toPoint(fPoints[0]) - toPoint(fPoints[1]);
  vv2 := toPoint(fPoints[2]) - toPoint(fPoints[1]);
  result := AngleV2_2(vv1, vv2);
End;

Procedure TAngle.AddInfoToStringgrid(Const Stringgrid: TStringGrid;
  Index: integer; Line: integer);
Var
  i: integer;
Begin
  If line = -1 Then Begin
    i := Stringgrid.RowCount;
    Stringgrid.RowCount := i + 1;
  End
  Else Begin
    i := Line;
  End;
  Stringgrid.Cells[GridIndexID, i] := inttostr(Index);
  Stringgrid.Cells[GridIndexObjType, i] := 'Angle';
  Stringgrid.Cells[GridIndexMUnit, i] := 'Angle';
  Stringgrid.Cells[GridIndexValue, i] := format('%0.2f', [GetAngle()]);
  Stringgrid.Cells[GridIndexUnit, i] := '°';
End;

Procedure TAngle.RefreshText();
Begin
  fText.Text := format('Angle: %0.2f °', [GetAngle()]);
End;

Procedure TAngle.Render;
Var
  r: Integer;
Begin
  If Enabled Then Begin
    fOwner.Canvas.pen.Color := clGreen;
  End
  Else Begin
    fOwner.Canvas.pen.Color := clred;
  End;
  fOwner.Canvas.pen.Width := 2;
  RenderLine(fOwner.Canvas, toPoint(fPoints[0]), toPoint(fPoints[1]));
  RenderLine(fOwner.Canvas, toPoint(fPoints[1]), toPoint(fPoints[2]));
  r := round(min(Abs(toPoint(fpoints[0]) - toPoint(fpoints[1])), abs(toPoint(fpoints[1]) - toPoint(fpoints[2]))));
  r := max(15, round(0.15 * r));
  RenderArc(fOwner.canvas, toPoint(fpoints[1]), r, toPoint(fpoints[0]), toPoint(fpoints[2]));
  fOwner.Canvas.pen.Width := 1;
  fPoints[0].Render;
  fPoints[1].Render;
  fPoints[2].Render;
  fText.Render;
End;

{ T3PointCircle }

Constructor T3PointCircle.Create(Owner: TPaintBox);
Begin
  Inherited Create(Owner);
  fMiddle := TKnob.Create(Owner);
  fMiddle.Visible := false;
  fMiddle.fParent := self;
End;

Destructor T3PointCircle.Destroy();
Begin
  fMiddle.free;
  Inherited Destroy();
End;

Procedure T3PointCircle.setEnabled(AValue: Boolean);
Begin
  Inherited setEnabled(AValue);
  fMiddle.Enabled := AValue;
End;

Procedure T3PointCircle.CalcMiddle();
Var
  c: TCircle;
  m: Tpoint;
Begin
  c := PointsToCircumCircle(toPoint(fPoints[0]), toPoint(fPoints[1]), toPoint(fPoints[2]));
  If c.radius > 0 Then Begin
    fMiddle.Visible := true;
    m := c.Center;
    fMiddle.Left := m.X;
    fMiddle.Top := m.y;
  End
  Else Begin
    fMiddle.Visible := false;
  End;
End;

Procedure T3PointCircle.RefreshByKnob(Sender: TKnob);
Var
  d, nm, om: Tpoint;
Begin
  Inherited RefreshByKnob(Sender);
  If sender = fMiddle Then Begin
    nm := toPoint(fMiddle);
    CalcMiddle();
    om := toPoint(fMiddle);
    d := nm - om;
    fMiddle.Left := nm.x;
    fMiddle.Top := nm.y;
    fPoints[0].Left := fPoints[0].Left + d.x;
    fPoints[0].Top := fPoints[0].Top + d.y;
    fPoints[1].Left := fPoints[1].Left + d.x;
    fPoints[1].Top := fPoints[1].Top + d.y;
    fPoints[2].Left := fPoints[2].Left + d.x;
    fPoints[2].Top := fPoints[2].Top + d.y;
  End
  Else Begin
    CalcMiddle();
  End;
End;

Function T3PointCircle.GetCircumfence(): Single;
Begin
  result := pi * GetDiameter();
End;

Function T3PointCircle.GetArea(): Single;
Begin
  result := pi * sqr(GetDiameter() / 2);
End;

Function T3PointCircle.GetDiameter(): Single;
Var
  d: Single;
Begin
  d := abs(point(fPoints[0].Left, fPoints[0].Top) - point(fMiddle.Left, fMiddle.Top)) * 2;
  result := PixelToDistance(d);
End;

Procedure T3PointCircle.AddInfoToStringgrid(Const Stringgrid: TStringGrid;
  Index: integer; Line: integer);
Var
  i: integer;
Begin
  If line = -1 Then Begin
    i := Stringgrid.RowCount;
    Stringgrid.RowCount := i + 3;
  End
  Else Begin
    i := Line;
  End;
  Stringgrid.Cells[GridIndexID, i] := inttostr(Index);
  Stringgrid.Cells[GridIndexObjType, i] := '3-Point-Circle';
  Stringgrid.Cells[GridIndexMUnit, i] := 'Circumfence';
  Stringgrid.Cells[GridIndexValue, i] := format('%0.2f', [GetCircumfence()]);
  Stringgrid.Cells[GridIndexUnit, i] := GetPixelUnit();

  Stringgrid.Cells[GridIndexID, i + 1] := inttostr(Index);
  Stringgrid.Cells[GridIndexObjType, i + 1] := '3-Point-Circle';
  Stringgrid.Cells[GridIndexMUnit, i + 1] := 'Area';
  Stringgrid.Cells[GridIndexValue, i + 1] := format('%0.2f', [GetArea()]);
  Stringgrid.Cells[GridIndexUnit, i + 1] := GetPixelAreaUnit();

  Stringgrid.Cells[GridIndexID, i + 2] := inttostr(Index);
  Stringgrid.Cells[GridIndexObjType, i + 2] := '3-Point-Circle';
  Stringgrid.Cells[GridIndexMUnit, i + 2] := 'Diameter';
  Stringgrid.Cells[GridIndexValue, i + 2] := format('%0.2f', [GetDiameter()]);
  Stringgrid.Cells[GridIndexUnit, i + 2] := GetPixelUnit();
End;

Procedure T3PointCircle.RefreshText();
Begin
  fText.Text :=
    format('Circumfence: %0.2f %s', [GetCircumfence(), GetPixelUnit()]) + LineEnding +
  format('Area: %0.2f %s', [GetArea(), GetPixelAreaUnit()]) + LineEnding +
  format('Diameter: %0.2f %s', [GetDiameter(), GetPixelUnit()]);
End;

Procedure T3PointCircle.Render;
Var
  r: integer;
Begin
  If Enabled Then Begin
    fOwner.Canvas.pen.Color := clGreen;
  End
  Else Begin
    fOwner.Canvas.pen.Color := clred;
  End;
  fOwner.Canvas.pen.Width := 2;
  r := round(abs(point(fPoints[0].Left, fPoints[0].top) - point(fMiddle.Left, fMiddle.top)));
  RenderCircle(fOwner.Canvas, point(fMiddle.Left, fMiddle.Top), r);
  fOwner.Canvas.pen.Width := 1;
  fPoints[0].Render;
  fPoints[1].Render;
  fPoints[2].Render;
  fMiddle.Render;
  fText.Render;
End;

Function T3PointCircle.Hit(x, y: Integer): Boolean;
Begin
  Result := Inherited Hit(x, y);
  If Not result Then Begin
    result := PointInRect(point(x, y), fMiddle.ClientRect);
  End;
End;

Procedure T3PointCircle.SetPoints(Points: TPointArray);
Begin
  Inherited SetPoints(Points);
  fMiddle.Visible := true;
  CalcMiddle();
End;

{ T2PointCircle }

Constructor T2PointCircle.Create(Owner: TPaintBox);
Begin
  Inherited Create(Owner);
  fMiddle := TKnob.Create(Owner);
  fMiddle.Visible := false;
  fMiddle.fParent := self;
End;

Destructor T2PointCircle.Destroy();
Begin
  fMiddle.free;
  Inherited Destroy();
End;

Procedure T2PointCircle.setEnabled(AValue: Boolean);
Begin
  Inherited setEnabled(AValue);
  fMiddle.Enabled := AValue;
End;

Procedure T2PointCircle.RefreshByKnob(Sender: TKnob);
Var
  d, m: Tpoint;
Begin
  Inherited RefreshByKnob(Sender);
  m := (point(fPoints[0].Left, fPoints[0].top) + point(fPoints[1].Left, fPoints[1].top)) / 2;
  If sender = fMiddle Then Begin
    d := point(fMiddle.Left, fMiddle.Top) - m;
    fPoints[0].Left := fPoints[0].Left + d.x;
    fPoints[0].Top := fPoints[0].Top + d.y;
    fPoints[1].Left := fPoints[1].Left + d.x;
    fPoints[1].Top := fPoints[1].Top + d.y;
  End
  Else Begin
    fMiddle.Left := m.x;
    fMiddle.Top := m.y;
  End;
End;

Function T2PointCircle.GetCircumfence(): Single;
Begin
  result := pi * GetDiameter();
End;

Function T2PointCircle.GetArea(): Single;
Begin
  result := pi * sqr(GetDiameter() / 2);
End;

Function T2PointCircle.GetDiameter(): Single;
Var
  d: Single;
Begin
  d := abs(point(fPoints[0].Left, fPoints[0].Top) - point(fPoints[1].Left, fPoints[1].Top));
  result := PixelToDistance(d);
End;

Procedure T2PointCircle.AddInfoToStringgrid(Const Stringgrid: TStringGrid;
  Index: integer; Line: integer);
Var
  i: integer;
Begin
  If line = -1 Then Begin
    i := Stringgrid.RowCount;
    Stringgrid.RowCount := i + 3;
  End
  Else Begin
    i := Line;
  End;
  Stringgrid.Cells[GridIndexID, i] := inttostr(Index);
  Stringgrid.Cells[GridIndexObjType, i] := 'Circle';
  Stringgrid.Cells[GridIndexMUnit, i] := 'Circumfence';
  Stringgrid.Cells[GridIndexValue, i] := format('%0.2f', [GetCircumfence()]);
  Stringgrid.Cells[GridIndexUnit, i] := GetPixelUnit();

  Stringgrid.Cells[GridIndexID, i + 1] := inttostr(Index);
  Stringgrid.Cells[GridIndexObjType, i + 1] := 'Circle';
  Stringgrid.Cells[GridIndexMUnit, i + 1] := 'Area';
  Stringgrid.Cells[GridIndexValue, i + 1] := format('%0.2f', [GetArea()]);
  Stringgrid.Cells[GridIndexUnit, i + 1] := GetPixelAreaUnit();

  Stringgrid.Cells[GridIndexID, i + 2] := inttostr(Index);
  Stringgrid.Cells[GridIndexObjType, i + 2] := 'Circle';
  Stringgrid.Cells[GridIndexMUnit, i + 2] := 'Diameter';
  Stringgrid.Cells[GridIndexValue, i + 2] := format('%0.2f', [GetDiameter()]);
  Stringgrid.Cells[GridIndexUnit, i + 2] := GetPixelUnit();
End;

Procedure T2PointCircle.RefreshText();
Begin
  fText.Text :=
    format('Circumfence: %0.2f %s', [GetCircumfence(), GetPixelUnit()]) + LineEnding +
  format('Area: %0.2f %s', [GetArea(), GetPixelAreaUnit()]) + LineEnding +
  format('Diameter: %0.2f %s', [GetDiameter(), GetPixelUnit()]);
End;

Procedure T2PointCircle.Render;
Var
  r: integer;
Begin
  If Enabled Then Begin
    fOwner.Canvas.pen.Color := clGreen;
  End
  Else Begin
    fOwner.Canvas.pen.Color := clred;
  End;
  fOwner.Canvas.pen.Width := 2;
  r := round(abs(point(fPoints[0].Left, fPoints[0].top) - point(fPoints[1].Left, fPoints[1].top)) / 2);
  RenderCircle(fOwner.Canvas, point(fMiddle.Left, fMiddle.Top), r);
  fOwner.Canvas.pen.Width := 1;
  fPoints[0].Render;
  fPoints[1].Render;
  fMiddle.Render;
  fText.Render;
End;

Function T2PointCircle.Hit(x, y: Integer): Boolean;
Begin
  Result := Inherited Hit(x, y);
  If Not result Then Begin
    result := PointInRect(point(x, y), fMiddle.ClientRect);
  End;
End;

Procedure T2PointCircle.SetPoints(Points: TPointArray);
Var
  m: Tpoint;
Begin
  Inherited SetPoints(Points);
  m := (point(fPoints[0].Left, fPoints[0].top) + point(fPoints[1].Left, fPoints[1].top)) / 2;
  fMiddle.Left := m.x;
  fMiddle.Top := m.y;
  fMiddle.Visible := true;
End;

{ TLinePoint }

Constructor TLinePoint.Create(Owner: TPaintBox);
Begin
  Inherited Create(Owner);
  fLP := TKnob.Create(Owner);
  fLP.Visible := false;
  fLP.fParent := self;
End;

Destructor TLinePoint.Destroy();
Begin
  fLP.free;
  Inherited Destroy();
End;

Procedure TLinePoint.setEnabled(AValue: Boolean);
Begin
  Inherited setEnabled(AValue);
  fLP.Enabled := AValue;
End;

Procedure TLinePoint.RefreshByKnob(Sender: TKnob);
Var
  lpo, dp, d, d2: TVector2;
  p: TPoint;
  l: Single;
Begin
  Inherited RefreshByKnob(Sender);
  If sender = flp Then Begin
    // Sicherstellen das der flp sich nur auf der Geraden fpoints[0], fppoints[1] bewgen kann
    lpo := CalculateOrthoganlProjection(toPoint(fPoints[0]), toPoint(fPoints[1]), toPoint(fPoints[2]));
    p := CalculateOrthoganlProjection(toPoint(fPoints[0]), toPoint(fPoints[1]), toPoint(flp));
    l := abs(lpo - point(fPoints[2].Left, fPoints[2].Top));
    flp.Left := p.x;
    flp.Top := p.y;
    // Entsrechend muss nun fpoints[2] verschoben werden
    d := toPoint(flp) - toPoint(fPoints[0]);
    d2 := toPoint(flp) - toPoint(fPoints[1]);
    If LenV2(d) > lenv2(d2) Then Begin
      dp := NormV2(v2(-d.y, d.x)) * l + toPoint(flp);
      fPoints[2].Left := round(dp.x);
      fPoints[2].Top := round(dp.y);
    End
    Else Begin
      dp := NormV2(v2(d2.y, -d2.x)) * l + toPoint(flp);
      fPoints[2].Left := round(dp.x);
      fPoints[2].Top := round(dp.y);
    End;
    RefreshText();
  End
  Else Begin
    p := CalculateOrthoganlProjection(toPoint(fPoints[0]), toPoint(fPoints[1]), toPoint(fPoints[2]));
    flp.Left := p.x;
    flp.Top := p.y;
  End;
End;

Function TLinePoint.GetLength(): Single;
Var
  l: Single;
Begin
  l := abs(point(flp.Left, flp.Top) - point(fPoints[2].Left, fPoints[2].Top));
  result := PixelToDistance(l);
End;

Procedure TLinePoint.AddInfoToStringgrid(Const Stringgrid: TStringGrid;
  Index: integer; Line: integer);
Var
  i: integer;
Begin
  If line = -1 Then Begin
    i := Stringgrid.RowCount;
    Stringgrid.RowCount := i + 1;
  End
  Else Begin
    i := Line;
  End;
  Stringgrid.Cells[GridIndexID, i] := inttostr(Index);
  Stringgrid.Cells[GridIndexObjType, i] := 'Line-Point-Distance';
  Stringgrid.Cells[GridIndexMUnit, i] := 'Length';
  Stringgrid.Cells[GridIndexValue, i] := format('%0.2f', [getLength()]);
  Stringgrid.Cells[GridIndexUnit, i] := GetPixelUnit();
End;

Procedure TLinePoint.RefreshText();
Begin
  fText.Text := format('Length: %0.2f %s', [GetLength(), GetPixelUnit()]);
End;

Procedure TLinePoint.Render;
Var
  tp1, tp2: TPoint;
Begin
  If Enabled Then Begin
    fOwner.Canvas.pen.Color := clGreen;
  End
  Else Begin
    fOwner.Canvas.pen.Color := clred;
  End;
  tp1 := toPoint(fPoints[0]) - toPoint(fLP);
  tp2 := toPoint(fPoints[1]) - toPoint(fLP);
  fOwner.canvas.Pen.Style := psDot;
  fOwner.canvas.Brush.Color := clwhite;
  fOwner.canvas.Brush.Style := bsClear;
  RenderLine(fOwner.canvas, toPoint(fPoints[2]) + tp1, toPoint(fPoints[2]) + tp2);
  fOwner.canvas.Brush.Style := bsSolid;
  fOwner.Canvas.Pen.Style := psSolid;
  fOwner.Canvas.pen.Width := 2;
  RenderLine(fOwner.canvas, toPoint(fLP), toPoint(fPoints[2]));
  RenderLine(fOwner.canvas, toPoint(fPoints[0]), toPoint(fPoints[1]));
  fOwner.Canvas.pen.Width := 1;
  fPoints[0].Render;
  fPoints[1].Render;
  fPoints[2].Render;
  fLP.Render;
  fText.Render;
End;

Function TLinePoint.Hit(x, y: Integer): Boolean;
Begin
  Result := Inherited Hit(x, y);
  If Not result Then Begin
    result := PointInRect(point(x, y), flp.ClientRect);
  End;
End;

Procedure TLinePoint.SetPoints(Points: TPointArray);
Var
  p: TPoint;
Begin
  Inherited SetPoints(Points);
  p := CalculateOrthoganlProjection(Points[0], Points[1], Points[2]);
  flp.Left := p.x;
  flp.Top := p.y;
  flp.Visible := true;
End;

{ TRectangle }

Constructor TRectangle.Create(Owner: TPaintBox);
Begin
  Inherited Create(Owner);
  p1 := TKnob.Create(Owner);
  p1.Visible := false;
  p1.fParent := self;
  p2 := TKnob.Create(Owner);
  p2.Visible := false;
  p2.fParent := self;
End;

Destructor TRectangle.Destroy();
Begin
  p1.free;
  p2.free;
  Inherited Destroy();
End;

Function TRectangle.Hit(x, y: Integer): Boolean;
Begin
  Result := Inherited Hit(x, y);
  If Not result Then Begin
    result := PointInRect(point(x, y), p1.ClientRect) Or
      PointInRect(point(x, y), p2.ClientRect);
  End;
End;

Procedure TRectangle.setEnabled(AValue: Boolean);
Begin
  Inherited setEnabled(AValue);
  p1.Enabled := AValue;
  p2.Enabled := AValue;
End;

Procedure TRectangle.setVisible(AValue: Boolean);
Begin
  Inherited setVisible(AValue);
  p1.Visible := AValue;
  p2.Visible := AValue;
End;

Function TRectangle.GetWidth(): Single;
Begin
  result := PixelToDistance(abs(fPoints[0].Left - fPoints[1].Left));
End;

Function TRectangle.GetHeight(): Single;
Begin
  result := PixelToDistance(abs(fPoints[0].Top - fPoints[1].Top));
End;

Function TRectangle.GetArea(): Single;
Begin
  result := GetWidth() * GetHeight();
End;

Procedure TRectangle.RefreshByKnob(Sender: TKnob);
Begin
  Inherited RefreshByKnob(Sender);
  If Sender = fPoints[0] Then Begin
    p1.Left := fPoints[0].Left;
    p2.Top := fPoints[0].Top;
  End;
  If Sender = fPoints[1] Then Begin
    p2.Left := fPoints[1].Left;
    p1.Top := fPoints[1].Top;
  End;
  If Sender = p1 Then Begin
    fPoints[0].Left := p1.Left;
    fPoints[1].Top := p1.Top;
  End;
  If Sender = p2 Then Begin
    fPoints[1].Left := p2.Left;
    fPoints[0].Top := p2.Top;
  End;
End;

Procedure TRectangle.AddInfoToStringgrid(Const Stringgrid: TStringGrid;
  Index: integer; Line: integer);
Var
  i: integer;
Begin
  If line = -1 Then Begin
    i := Stringgrid.RowCount;
    Stringgrid.RowCount := i + 3;
  End
  Else Begin
    i := Line;
  End;

  Stringgrid.Cells[GridIndexID, i] := inttostr(Index);
  Stringgrid.Cells[GridIndexObjType, i] := 'Rectangle';
  Stringgrid.Cells[GridIndexMUnit, i] := 'Area';
  Stringgrid.Cells[GridIndexValue, i] := format('%0.2f', [GetArea()]);
  Stringgrid.Cells[GridIndexUnit, i] := GetPixelAreaUnit();

  Stringgrid.Cells[GridIndexID, i + 1] := inttostr(Index);
  Stringgrid.Cells[GridIndexObjType, i + 1] := 'Rectangle';
  Stringgrid.Cells[GridIndexMUnit, i + 1] := 'Width';
  Stringgrid.Cells[GridIndexValue, i + 1] := format('%0.2f', [GetWidth()]);
  Stringgrid.Cells[GridIndexUnit, i + 1] := GetPixelUnit();

  Stringgrid.Cells[GridIndexID, i + 2] := inttostr(Index);
  Stringgrid.Cells[GridIndexObjType, i + 2] := 'Rectangle';
  Stringgrid.Cells[GridIndexMUnit, i + 2] := 'Height';
  Stringgrid.Cells[GridIndexValue, i + 2] := format('%0.2f', [GetHeight()]);
  Stringgrid.Cells[GridIndexUnit, i + 2] := GetPixelUnit();
End;

Procedure TRectangle.RefreshText();
Begin
  fText.Text :=
    format('Area: %0.2f %s', [GetArea, GetPixelAreaUnit()]) + LineEnding +
  format('Width: %0.2f %s', [GetWidth(), GetPixelUnit()]) + LineEnding +
  format('Height: %0.2f %s', [GetHeight(), GetPixelUnit()]);
End;

Procedure TRectangle.Render;
Var
  i: Integer;
  pp1, pp2, pp3, pp4, d: TPoint;
Begin
  If Visible Then Begin
    fOwner.canvas.Pen.Color := clblack;
    fOwner.canvas.Pen.Width := 2;
    If enabled Then Begin
      fOwner.canvas.Pen.Color := clGreen;
    End
    Else Begin
      fOwner.canvas.Pen.Color := clRed;
    End;
    d := Point(5, 5);
    pp1 := InverseTransformRoutine(fPoints[0].left, fPoints[0].top);
    pp2 := InverseTransformRoutine(p1.left, p1.top);
    pp3 := InverseTransformRoutine(fPoints[1].left, fPoints[1].top);
    pp4 := InverseTransformRoutine(p2.left, p2.top);
    fOwner.Canvas.Line(pp1 + d, pp2 + d);
    fOwner.Canvas.Line(pp2 + d, pp3 + d);
    fOwner.Canvas.Line(pp3 + d, pp4 + d);
    fOwner.Canvas.Line(pp4 + d, pp1 + d);
    fOwner.canvas.Pen.Width := 1;
    p1.Render;
    p2.Render;
    For i := 0 To high(fPoints) Do Begin
      fPoints[i].Render;
    End;
    fText.Render;
  End;
End;

Procedure TRectangle.SetPoints(Points: TPointArray);
Begin
  Inherited SetPoints(Points);
  p1.Left := fpoints[0].Left;
  p1.Top := fpoints[1].Top;
  p2.Left := fpoints[1].Left;
  p2.Top := fpoints[0].Top;
  p1.Visible := true;
  p2.Visible := true;
End;

{ TPolygon }

Procedure TPolygon.Render;
Var
  i: integer;
  pp1, pp2, d: TPoint;
Begin
  If Visible Then Begin
    fOwner.canvas.Pen.Color := clblack;
    fOwner.canvas.Pen.Width := 2;
    If enabled Then Begin
      fOwner.canvas.Pen.Color := clGreen;
    End
    Else Begin
      fOwner.canvas.Pen.Color := clRed;
    End;
    d := Point(5, 5);
    For i := 0 To high(fPoints) - 1 Do Begin
      pp1 := InverseTransformRoutine(fPoints[i].left, fPoints[i].top);
      pp2 := InverseTransformRoutine(fPoints[i + 1].left, fPoints[i + 1].top);
      fOwner.canvas.Line(pp1.X + d.x, pp1.Y + d.y, pp2.X + d.x, pp2.Y + d.y);
    End;
    pp1 := InverseTransformRoutine(fPoints[high(fPoints)].left, fPoints[high(fPoints)].top);
    pp2 := InverseTransformRoutine(fPoints[0].left, fPoints[0].top);
    fOwner.canvas.Line(pp1.X + d.x, pp1.Y + d.y, pp2.X + d.x, pp2.Y + d.y);
    fOwner.canvas.Pen.Width := 1;
  End;
  For i := 0 To high(fPoints) Do Begin
    fPoints[i].Render;
  End;
  fText.Render;
End;

Function TPolygon.GetArea(): Single;
Var
  a: Single;
  i: Integer;
  pts: TVector2Array;
Begin
  pts := Nil;
  setlength(pts, length(fPoints));
  For i := 0 To high(fPoints) - 1 Do Begin
    pts[i] := v2(fPoints[i].Left, fPoints[i].Top);
  End;
  pts[high(fPoints)] := v2(fPoints[high(fPoints)].Left, fPoints[high(fPoints)].Top); // Der wird ja nicht über die Schleife abgedeckt.
  a := CalculatePolygonArea(pts);
  result := PixelToArea(a);
End;

Procedure TPolygon.AddInfoToStringgrid(Const Stringgrid: TStringGrid;
  Index: integer; Line: integer);
Var
  i: integer;
Begin
  If line = -1 Then Begin
    i := Stringgrid.RowCount;
    Stringgrid.RowCount := i + 2;
  End
  Else Begin
    i := Line;
  End;

  Stringgrid.Cells[GridIndexID, i] := inttostr(Index);
  Stringgrid.Cells[GridIndexObjType, i] := 'Polygon';
  Stringgrid.Cells[GridIndexMUnit, i] := 'Area';
  Stringgrid.Cells[GridIndexValue, i] := format('%0.2f', [GetArea()]);
  Stringgrid.Cells[GridIndexUnit, i] := GetPixelAreaUnit();

  Stringgrid.Cells[GridIndexID, i + 1] := inttostr(Index);
  Stringgrid.Cells[GridIndexObjType, i + 1] := 'Polygon';
  Stringgrid.Cells[GridIndexMUnit, i + 1] := 'Perimeter';
  Stringgrid.Cells[GridIndexValue, i + 1] := format('%0.2f', [getLength()]);
  Stringgrid.Cells[GridIndexUnit, i + 1] := GetPixelUnit();
End;

Procedure TPolygon.RefreshText();
Begin
  fText.Text := format('Area: %0.2f %s', [GetArea(), GetPixelAreaUnit()]) + LineEnding +
  format('Perimeter: %0.2f %s', [GetLength(), GetPixelUnit()]);
End;

{ TMultiLine }

Function TMultiLine.GetLength(): Single;
Var
  i: integer;
Begin
  result := 0;
  For i := 0 To high(fPoints) - 1 Do Begin
    result := result + sqrt(sqr(fPoints[i].Top - fPoints[i + 1].Top) + sqr(fPoints[i].Left - fPoints[i + 1].Left));
  End;
  result := PixelToDistance(result);
End;

Procedure TMultiLine.AddInfoToStringgrid(Const Stringgrid: TStringGrid;
  Index: integer; Line: integer);
Var
  i: integer;
Begin
  If line = -1 Then Begin
    i := Stringgrid.RowCount;
    Stringgrid.RowCount := i + 1;
  End
  Else Begin
    i := Line;
  End;
  Stringgrid.Cells[GridIndexID, i] := inttostr(Index);
  Stringgrid.Cells[GridIndexObjType, i] := 'Line';
  Stringgrid.Cells[GridIndexMUnit, i] := 'Length';
  Stringgrid.Cells[GridIndexValue, i] := format('%0.2f', [getLength()]);
  Stringgrid.Cells[GridIndexUnit, i] := GetPixelUnit();
End;

Procedure TMultiLine.RefreshText();
Begin
  fText.Text := format('Length: %0.2f %s', [GetLength(), GetPixelUnit()]);
End;

Procedure TMultiLine.Render;
Var
  i: integer;
  pp1, pp2, d: TPoint;
Begin
  If Visible Then Begin
    fOwner.canvas.Pen.Color := clblack;
    fOwner.canvas.Pen.Width := 2;
    If enabled Then Begin
      fOwner.canvas.Pen.Color := clGreen;
    End
    Else Begin
      fOwner.canvas.Pen.Color := clRed;
    End;
    d := point(5, 5);
    For i := 0 To high(fPoints) - 1 Do Begin
      pp1 := InverseTransformRoutine(fPoints[i].left, fPoints[i].top);
      pp2 := InverseTransformRoutine(fPoints[i + 1].left, fPoints[i + 1].top);
      fOwner.canvas.Line(pp1.X + d.x, pp1.Y + d.y, pp2.X + d.x, pp2.Y + d.y);
    End;
    fOwner.canvas.Pen.Width := 1;
  End;
  For i := 0 To high(fPoints) Do Begin
    fPoints[i].Render;
  End;
  fText.Render;
End;

{ TTextField }

Constructor TTextField.Create(Owner: TPaintBox);
Begin
  Inherited Create(Owner);
  Text := '';
  ReadOnly := false;
End;

Procedure TTextField.setText(AValue: String);
Begin
  // If fText = AValue Then Exit; -- Absichtlich Deaktiviert, damit ein Font Wechsel im Canvas hier durchschlagen kann !
  fText := AValue;
  width := TextWidth(fText) + 10;
  Height := TextHeight(fText) + 10;
End;

Function TTextField.TextHeight(Text: String): integer;
Begin
  result := uwidgets.TextHeight(FOwner.Canvas, Text);
End;

Function TTextField.TextWidth(Text: String): Integer;
Begin
  result := uwidgets.TextWidth(FOwner.Canvas, Text);
End;

Procedure TTextField.DblClick;
Begin
  If assigned(FOwner.OnDblClick) Then Begin
    FOwner.OnDblClick(self);
  End;
End;

Procedure TTextField.Render;
Var
  p, d: TPoint;
  l, lh: integer;
  s, t: String;
Begin
  If Not visible Then exit;
  If Enabled Then Begin
    FOwner.canvas.Pen.Color := clGreen;
  End
  Else Begin
    FOwner.canvas.Pen.Color := clred;
  End;
  FOwner.canvas.Brush.Color := clBlack;
  p := InverseTransformRoutine(left, top);
  d := point(Width, Height);
  FOwner.canvas.Rectangle(p.x, p.y, p.x + d.x, p.y + d.y);
  FOwner.canvas.Font.Color := clWhite;
  lh := TextHeight('Aq');
  s := fText + LineEnding;
  l := 0;
  While pos(LineEnding, s) <> 0 Do Begin
    t := copy(s, 1, pos(LineEnding, s) - 1);
    delete(s, 1, pos(LineEnding, s) + Length(LineEnding) - 1);
    FOwner.canvas.TextOut(p.x + 5, p.y + 5 + l * lh, t);
    inc(l);
  End;
End;

{ TMeasureElement }

Constructor TMeasureElement.Create(Owner: TPaintBox);
Begin
  Inherited create;
  fOnchange := Nil;
  fOwner := Owner;
  fPoints := Nil;
  FTextInitialized := false;
  fText := TTextField.Create(Owner);
  fText.Visible := false;
  fText.ReadOnly := true;
End;

Destructor TMeasureElement.Destroy();
Var
  i: Integer;
Begin
  For i := 0 To high(fPoints) Do Begin
    fPoints[i].Free;
  End;
  fText.free;
End;

Function TMeasureElement.getVisible: Boolean;
Begin
  If assigned(fPoints) Then Begin
    result := fPoints[0].Visible;
  End
  Else Begin
    result := false;
  End;
End;

Procedure TMeasureElement.setVisible(AValue: Boolean);
Var
  i: Integer;
Begin
  For i := 0 To high(fPoints) Do Begin
    fPoints[i].Visible := AValue;
  End;
  If FTextInitialized Then Begin
    fText.Visible := AValue;
  End;
End;

Function TMeasureElement.ClassToIdentifier(): int32;
Var
  i: Integer;
Begin
  result := -1;
  For i := low(MeasureElementIdentifier) To high(MeasureElementIdentifier) Do Begin
    If MeasureElementIdentifier[i].Cls = ClassType Then Begin
      result := MeasureElementIdentifier[i].Index;
      break;
    End;
  End;
  If result = -1 Then Begin
    Raise exception.create('Missing "MeasureElementIdentifier" implementation for: ' + ClassName);
  End;
End;

Procedure TMeasureElement.RefreshByKnob(Sender: TKnob);
Begin
  RefreshText();
  If assigned(fOnchange) Then
    Onchange(self);
End;

Function TMeasureElement.getEnabled: Boolean;
Begin
  If assigned(fPoints) Then Begin
    result := fPoints[0].Enabled;
  End
  Else Begin
    result := false;
  End;
End;

Function TMeasureElement.getTextvisible: Boolean;
Begin
  result := fText.Visible;
End;

Procedure TMeasureElement.setEnabled(AValue: Boolean);
Var
  i: Integer;
Begin
  For i := 0 To high(fPoints) Do Begin
    fPoints[i].Enabled := AValue;
  End;
  fText.Enabled := AValue;
End;

Procedure TMeasureElement.setTextvisible(AValue: Boolean);
Begin
  fText.Visible := AValue;
End;

Procedure TMeasureElement.InitText();
Var
  x, y, i: integer;
Begin
  fText.Visible := true;
  FTextInitialized := true;
  x := 0;
  y := 0;
  For i := 0 To high(fPoints) Do Begin
    x := x + fPoints[i].Left;
    y := y + fPoints[i].Top;
  End;
  fText.Left := x Div max(length(fPoints), 1);
  fText.Top := y Div max(length(fPoints), 1);
  RefreshText;
End;

Procedure TMeasureElement.RefreshText();
Begin
  Raise exception.create('Missing "RefreshText" implementation for: ' + ClassName);
End;

Procedure TMeasureElement.Render;
Begin
  Raise exception.create('Missing "Render" implementation for: ' + ClassName);
End;

Procedure TMeasureElement.AddInfoToStringgrid(Const Stringgrid: TStringGrid;
  Index: integer; Line: integer);
Begin
  Raise exception.create('Missing "AddInfoToStringgrid" implementation for: ' + ClassName);
End;

Function TMeasureElement.Hit(x, y: Integer): Boolean;
Var
  i: Integer;
  p: TPoint;
Begin
  p := TransformRoutine2(fText.width, fText.Height, true);
  result := PointInRect(point(x, y), Rect(fText.left, fText.top, fText.left + p.X, fText.top + p.Y));
  If Not result Then Begin
    For i := 0 To high(fPoints) Do Begin
      p := TransformRoutine2(fPoints[i].width, fPoints[i].Height, true);
      If PointInRect(point(x, y), Rect(fPoints[i].left, fPoints[i].top, fPoints[i].left + p.X, fPoints[i].top + p.Y)) Then Begin
        result := true;
        break;
      End;
    End;
  End;
End;

Procedure TMeasureElement.SetPoints(Points: TPointArray);
Var
  i: Integer;
Begin
  For i := 0 To high(fPoints) Do Begin
    fPoints[i].Free;
  End;
  setlength(fPoints, length(Points));
  For i := 0 To high(Points) Do Begin
    fPoints[i] := TKnob.Create(fOwner);
    fPoints[i].fParent := self;
    fPoints[i].Left := Points[i].x;
    fPoints[i].Top := Points[i].Y;
  End;
End;

Function TMeasureElement.GetPoints(): TPointArray;
Var
  i: Integer;
Begin
  result := Nil;
  setlength(result, length(fPoints));
  For i := 0 To high(Result) Do Begin
    result[i] := toPoint(fPoints[i]);
  End;
End;

Procedure TMeasureElement.SaveToStream(Const Stream: TStream);
Var
  j: int32;
  i: Integer;
  p: TPoint;
Begin
  // Speichern der Klassen Identifikation
  j := ClassToIdentifier();
  stream.Write(j, SizeOf(j));
  // Die Einlezlen Punkte Speichern
  j := length(fPoints);
  stream.Write(j, SizeOf(j));
  For i := 0 To high(fPoints) Do Begin
    p := point(fPoints[i].Left, fPoints[i].Top);
    stream.Write(p, SizeOf(p));
  End;
  // Die Position der Beschriftung Speichern
  j := fText.Left;
  stream.Write(j, SizeOf(j));
  j := fText.Top;
  stream.Write(j, SizeOf(j));
End;

Procedure TMeasureElement.LoadFromStream(Const Stream: TStream);
Var
  j: int32;
  i: Integer;
  pts: TPointArray;
Begin
  pts := Nil;
  j := 0;
  Stream.Read(j, SizeOf(j));
  setlength(pts, j);
  For i := 0 To high(pts) Do Begin
    Stream.Read(pts[i], sizeof(pts[i]));
  End;
  SetPoints(pts);
  // Die Beschriftung muss zuerst Erzeugt und Initialisiert werden
  InitText();
  // Dann kann sie gesetzt werden
  j := 0;
  Stream.Read(j, SizeOf(j));
  fText.Left := j;
  j := 0;
  Stream.Read(j, SizeOf(j));
  fText.Top := j;
End;

Class Function TMeasureElement.LoadFromStream(Const Stream: TStream;
  Owner: TPaintBox): TMeasureElement;
Var
  cls: TMeasureElementClass;
  j: int32;
Begin
  j := -1;
  stream.Read(j, SizeOf(j));
  cls := IdentifierToClass(j);
  result := cls.Create(Owner);
  result.LoadFromStream(Stream);
End;

{ TMetrik }

Constructor TMetrik.Create(Owner: TPaintBox);
Begin
  Inherited Create(Owner);
End;

Procedure TMetrik.setDistance(aValue: Single);
Begin
  If aValue = fDistance Then exit;
  fDistance := aValue;
  If assigned(OnChange) Then Begin
    OnChange(self);
  End;
End;

Procedure TMetrik.LoadFromStream(Const Stream: TStream);
Begin
  Inherited LoadFromStream(Stream);
  fDistance := 0;
  stream.Read(fDistance, sizeof(fDistance));
  (*
   * LoadfromStream Initialisiert den Text, die Metrik hat aber keinen,
   * also machen wir das hier wieder rückgängig.
   *)
  fText.Visible := false;
  FTextInitialized := false;
End;

Procedure TMetrik.RefreshText();
Begin
  If assigned(OnChange) Then Begin
    OnChange(self);
  End;
End;

Function TMetrik.PixelToDistance(Len: Single): Single;
Var
  distp1p2: Single;
Begin
  (*
   * Urmechnung Mittels Strahlensatz
   * Strecke P1, P2 = Distanz, dann entspricht Len = ?
   *)
  distp1p2 := sqrt(sqr(fPoints[0].Left - fPoints[1].Left) + sqr(fPoints[0].Top - fPoints[1].Top));
  If distp1p2 = 0 Then distp1p2 := 1;
  result := len * fDistance / distp1p2;
End;

Function TMetrik.PixelToArea(Area: Single): Single;
Var
  len: Single;
Begin
  len := PixelToDistance(1);
  result := Area * sqr(len);
End;

Procedure TMetrik.SaveToStream(Const Stream: TStream);
Begin
  Inherited SaveToStream(Stream);
  stream.Write(fDistance, sizeof(fDistance));
End;

{ TKnob }

Constructor TKnob.Create(Owner: TPaintBox);
Begin
  Inherited Create(Owner);
  fParent := Nil;
  FOwner := Owner;
  Width := 10;
  height := 10;
End;

Function TKnob.GetClientRect: Trect;
Var
  p: TPoint;
Begin
  p := TransformRoutine2(width, Height, true);
  result := Rect(left, top, left + p.X, top + p.Y);
End;

Procedure TKnob.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
Begin
  Inherited MouseDown(Button, Shift, X, Y);
  dx := x;
  dy := y;
End;

Procedure TKnob.MouseMove(Shift: TShiftState; X, Y: Integer);
Begin
  Inherited MouseMove(Shift, X, Y);
  If (ssleft In shift) And Visible And Enabled And (FMouseDown) Then Begin
    left := left + x - dx;
    top := top + y - dy;
    If Assigned(fParent) Then fParent.RefreshByKnob(Self);
    FOwner.Invalidate;
  End;
End;

Procedure TKnob.Render;
Var
  p, d: TPoint;
Begin
  If Not visible Then exit;
  fOwner.canvas.Pen.Color := clblack;
  If enabled Then Begin
    fOwner.canvas.Brush.Color := clgreen;
  End
  Else Begin
    fOwner.canvas.Brush.Color := clred;
  End;
  p := InverseTransformRoutine(left, top);
  d := point(Width, height);
  fOwner.canvas.Rectangle(p.x, p.y, p.x + d.x, p.y + d.y);
End;

{ TPreview }

Constructor TPreview.Create();
Begin
  Inherited Create;
  Kind := pkNone;
  Points := Nil;
End;

Destructor TPreview.Destroy();
Begin
  setlength(Points, 0);
End;

Procedure TPreview.Render(Const Canvas: TCanvas);
  Procedure RenderKnob(p: TPoint);
  Var
    d: TPoint;
  Begin
    canvas.Brush.Color := clgreen;
    Canvas.pen.Color := clblack;
    p := InverseTransformRoutine(p.x, p.y);
    d := Point(10, 10);
    canvas.Rectangle(p.x, p.y, p.x + d.x, p.y + d.y);
  End;
Var
  r, i: Integer;
  p1, p2, p3, tp1, tp2, m, lp: TPoint;
  c: TCircle;
Begin
  Case Kind Of
    pkArrow,
      pkAngle,
      pkLine,
      pkMultiLine,
      pkPolygon: Begin
        // Viele Punkte verbunden mit Linien
        Canvas.pen.Color := clred;
        If kind = pkAngle Then Begin
          If length(Points) = 2 Then Begin
            r := round(min(Abs(points[0] - points[1]), abs(points[1] - aMousePos)));
            r := max(15, round(0.15 * r));
            RenderArc(canvas, Points[1], r, Points[0], aMousePos);
          End;
        End;
        For i := 0 To high(Points) - 1 Do Begin
          RenderLine(canvas, Points[i], points[i + 1]);
        End;
        RenderLine(canvas, Points[high(Points)], aMousePos);
        For i := 0 To high(Points) Do Begin
          RenderKnob(Points[i]);
        End;
        RenderKnob(aMousePos);
      End;
    pkRectangle: Begin
        Canvas.pen.Color := clred;
        RenderLine(canvas, Points[0], Point(aMousePos.x, points[0].y));
        RenderLine(canvas, Point(aMousePos.x, points[0].y), aMousePos);
        RenderLine(canvas, aMousePos, Point(points[0].x, aMousePos.y));
        RenderLine(canvas, Point(points[0].x, aMousePos.y), Points[0]);
        RenderKnob(Points[0]);
        RenderKnob(Point(aMousePos.x, points[0].y));
        RenderKnob(Point(points[0].x, aMousePos.y));
        RenderKnob(aMousePos);
      End;
    pkLinePoint: Begin
        Canvas.pen.Color := clred;
        If Length(Points) = 1 Then Begin
          RenderLine(canvas, Points[0], aMousePos); // Während dem Erstellen der Reverenzlinie
        End
        Else Begin
          RenderLine(canvas, Points[0], Points[1]); // Die Referenzlinie
          // Der Lotpunkt von der Maus zur Geraden [0] -> [1]
          lp := CalculateOrthoganlProjection(Points[0], Points[1], aMousePos);
          tp1 := Points[0] - lp;
          tp2 := Points[1] - lp;

          Canvas.Pen.Style := psDot;
          RenderLine(canvas, aMousePos + tp1, aMousePos + tp2);
          Canvas.Pen.Style := psSolid;

          RenderLine(canvas, lp, aMousePos);
          RenderKnob(lp);
          RenderKnob(Points[1]);
        End;
        RenderKnob(Points[0]);
        RenderKnob(aMousePos);
      End;
    pk2PointCircle: Begin
        m := (Points[0] + aMousePos) / 2;
        r := round(abs(Points[0] - aMousePos) / 2);
        Canvas.pen.Color := clred;
        RenderCircle(canvas, m, r);
        RenderKnob(Points[0]);
        RenderKnob(aMousePos);
      End;
    pk3PointCircle: Begin
        If Length(points) > 1 Then Begin
          RenderKnob(Points[1]);
          c := PointsToCircumCircle(Points[0], Points[1], aMousePos);
          If c.radius > 0 Then Begin
            Canvas.pen.Color := clred;
            RenderCircle(canvas, c.Center, round(c.radius));
          End
          Else Begin
            // Die Maus liegt auf der Geraden Points[0] ,Points[1] => Kein Kreis möglich !
          End;
        End;
        RenderKnob(Points[0]);
        RenderKnob(aMousePos);
      End;
    pkArc: Begin
        If Length(points) > 1 Then Begin
          RenderKnob(Points[1]);
          c := PointsToCircumCircle(Points[0], Points[1], aMousePos);
          If c.radius > 0 Then Begin
            p1 := Points[0];
            p2 := Points[1];
            p3 := aMousePos;
            If AngleV2_2(p1 - c.Center, p3 - c.Center) < AngleV2_2(p1 - c.Center, p2 - c.Center) Then Begin
              lp := p3;
              p3 := p2;
              p2 := lp;
            End;
            Canvas.pen.Color := clred;
            Canvas.Brush.Color := clWhite;
            RenderArc(Canvas, c.Center, round(c.radius), p1, p3);
            Canvas.Pen.Style := psDot;
            RenderLine(canvas, p1, c.Center);
            RenderLine(canvas, c.Center, p3);
            Canvas.Pen.Style := psSolid;
          End
          Else Begin
            // Die Maus liegt auf der Geraden Points[0] ,Points[1] => Kein Kreis möglich !
          End;
        End;
        RenderKnob(Points[0]);
        RenderKnob(aMousePos);
      End;
  End;
End;

Procedure TPreview.Clear;
Begin
  setlength(Points, 0);
  Kind := pkNone;
End;

End.

