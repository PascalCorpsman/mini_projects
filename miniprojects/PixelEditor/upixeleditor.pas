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
Unit upixeleditor;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Controls, OpenGLContext, uopengl_widgetset, upixeleditorlcl,
  ExtCtrls, uimage, ugraphics;

Const
  (*
   * History: 0.01 - Initialversion
   *)
  Version = '0.01';


  (*
   * Es folgt the "Tiefe" der verschiedenen Render Ebenen [-0.9 .. 0.9]
   *
   * Jede Ebene sollte sich zur nächst höheren / tieferen um mindestens 0.01 unterscheiden !
   *)

  LayerBackGroundGrid = -0.9; // Das Grid das hinter allem sein soll und nur bei Transparenten Pixeln zu sehen ist
  LayerImage = -0.8; // Die Eigentliche vom User erstellte Textur
  LayerForeGroundGrid = -0.05;
  LayerFormColor = -0.01;
  LayerLCL = 0.0;

  ZoomLevels: Array Of integer = (100, 500, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, 5000); // in %

Type

  TCursor = Record
    LeftColor: TRGBA;
    RightColor: TRGBA;
  End;

  { TPixelEditor }

  TPixelEditor = Class
  private
    fCursor: TCursor;
    fPixelPos: Tpoint; // -1,-1 = Ungültig, sonst Bildposition in Pixeln
    FOwner: TOpenGLControl;
    fZoom: integer; // Akruelle Zoomstufe in %
    fAktualLayer: TLayer;
    fImage: TImage; // Das Object um das es hier eigentlich geht ;)

    FElements: Array Of TOpenGL_BaseClass;

    // Titelleiste oben
    NewButton: TOpenGL_Bevel;
    OpenButton: TOpenGL_Bevel;
    SaveButton: TOpenGL_Bevel;
    SaveAsButton: TOpenGL_Bevel;
    ExitButton: TOpenGL_Bevel;
    GridButton: TOpenGL_Bevel;
    ZoomInButton: TOpenGL_Bevel;
    ZoomInfoTextbox: TOpenGL_Textbox;
    ZoomOutButton: TOpenGL_Bevel;
    UndoButton: TOpenGL_Bevel;

    // Menüleiste Links
    SelectButton: TOpenGL_Bevel;
    SelectModeButton: TOpenGL_ToggleButton;
    RotateCounterClockwise90: TOpenGL_Bevel;
    RotateClockwise90: TOpenGL_Bevel;
    Rotate180: TOpenGL_Bevel;
    RotateAngle: TOpenGL_Bevel;
    BrightenButton: TOpenGL_Bevel;
    DarkenButton: TOpenGL_Bevel;
    CurserSize1: TOpenGL_Bevel;
    CurserSize2: TOpenGL_Bevel;
    CurserSize3: TOpenGL_Bevel;
    CurserSize4: TOpenGL_Bevel;
    EraserButton: TOpenGL_Bevel;
    PencilButton: TOpenGL_Bevel;
    CursorRoundSize1: TOpenGL_Bevel;
    CursorRoundSize2: TOpenGL_Bevel;
    CursorRoundSize3: TOpenGL_Bevel;
    CursorSquareSize1: TOpenGL_Bevel;
    CursorSquareSize2: TOpenGL_Bevel;
    CursorSquareSize3: TOpenGL_Bevel;
    LineButton: TOpenGL_Bevel;
    CircleButton: TOpenGL_Bevel;
    SquareButton: TOpenGL_Bevel;
    OutlineButton: TOpenGL_Bevel;
    FilledButton: TOpenGL_Bevel;
    MirrorButton: TOpenGL_Bevel;
    MirrorHorButton: TOpenGL_Bevel;
    MirrorVertButton: TOpenGL_Bevel;
    Mirror4Button: TOpenGL_Bevel;
    FloodFillButton: TOpenGL_Bevel;
    FloodFillModeButton: TOpenGL_Bevel;
    ColorPickButton: TOpenGL_Bevel;

    // Menüleiste unten

    SelectTransparentColor: TOpenGL_Bevel;
    Color1: TOpenGL_ColorBox;
    Color2: TOpenGL_ColorBox;
    Color3: TOpenGL_ColorBox;
    Color4: TOpenGL_ColorBox;
    Color5: TOpenGL_ColorBox;
    Color6: TOpenGL_ColorBox;
    Color7: TOpenGL_ColorBox;
    Color8: TOpenGL_ColorBox;

    InfoLabel: TOpenGl_Label; // Anzeige Aktuelle Position und Pixelfarbe unter Position
    InfoDetailLabel: TOpenGl_Label; // Zeigt beim Linien/ Ellipse/ Rechteck tool die "Delta's" an

    SelectLayerButton: TOpenGL_Bevel;

    Procedure OnNewButtonClick(Sender: TObject);
    Procedure OnOpenButtonClick(Sender: TObject);
    Procedure OnSaveButtonClick(Sender: TObject);
    Procedure OnSaveAsButtonClick(Sender: TObject);
    Procedure OnExitButtonClick(Sender: TObject);
    Procedure OnGridButtonClick(Sender: TObject);
    Procedure OnZoomOutButtonClick(Sender: TObject);
    Procedure OnZoomInButtonClick(Sender: TObject);
    Procedure OnUndoButtonClick(Sender: TObject);

    Procedure OnSelectButtonClick(Sender: TObject);
    Procedure OnRotateCounterClockwise90ButtonClick(Sender: TObject);
    Procedure OnRotateClockwise90ButtonClick(Sender: TObject);
    Procedure OnRotate180ButtonClick(Sender: TObject);
    Procedure OnRotateAngleButtonClick(Sender: TObject);
    Procedure OnBrightenButtonClick(Sender: TObject);
    Procedure OnDarkenButtonClick(Sender: TObject);
    Procedure OnCurserSize1ButtonClick(Sender: TObject);
    Procedure OnCurserSize2ButtonClick(Sender: TObject);
    Procedure OnCurserSize3ButtonClick(Sender: TObject);
    Procedure OnCurserSize4ButtonClick(Sender: TObject);
    Procedure OnEraserButtonClick(Sender: TObject);
    Procedure OnPencilButtonClick(Sender: TObject);
    Procedure OnCursorRoundSize1Click(Sender: TObject);
    Procedure OnCursorRoundSize2Click(Sender: TObject);
    Procedure OnCursorRoundSize3Click(Sender: TObject);
    Procedure OnCursorSquareSize1Click(Sender: TObject);
    Procedure OnCursorSquareSize2Click(Sender: TObject);
    Procedure OnCursorSquareSize3Click(Sender: TObject);
    Procedure OnLineButtonClick(Sender: TObject);
    Procedure OnCircleButtonClick(Sender: TObject);
    Procedure OnSquareButtonClick(Sender: TObject);
    Procedure OnOutlineButtonClick(Sender: TObject);
    Procedure OnFilledButtonClick(Sender: TObject);
    Procedure OnMirrorButtonClick(Sender: TObject);
    Procedure OnMirror4ButtonClick(Sender: TObject);
    Procedure OnMirrorVertButtonClick(Sender: TObject);
    Procedure OnMirrorHorButtonClick(Sender: TObject);
    Procedure OnFloodFillButtonClick(Sender: TObject);
    Procedure OnFloodFillModeButtonClick(Sender: TObject);
    Procedure OnColorPickButtonClick(Sender: TObject);

    Procedure OnSelectTransparentColorClick(Sender: TObject);
    Procedure OnColorClick(Sender: TObject);

    Procedure OnSelectLayerButtonClick(Sender: TObject);

    Procedure OpenGLControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    Procedure OpenGLControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);

    Procedure OpenGLControlMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; Var Handled: Boolean);
    Procedure OpenGLControlMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; Var Handled: Boolean);

    Procedure RenderGrid;
    Procedure RenderImage;
    Procedure RenderLCL;

    Procedure AddElement(Const value: TOpenGL_BaseClass);

    Procedure NewImage(aWidth, aHeight: Integer);
    Procedure SetZoom(ZoomValue: integer);
    Procedure CenterAt(aX, aY: integer);
    Procedure ZoomIn();
    Procedure ZoomOut();

    Function CursorToPixel(x, y: integer): TPoint;
    Procedure SetLeftColor(Const C: TRGBA);
    Procedure UpdateInfoLabel;
  public
    FormCloseEvent: TNotifyEvent; // Um der Besitzerklasse mit zu teilen, dass die Anwendung beendet werden will
    Constructor Create; virtual;
    Destructor Destroy; override;

    Procedure MakeCurrent(Owner: TOpenGLControl);

    Procedure Render();
  End;

Implementation

Uses math, dglOpenGL, Graphics, uOpenGL_ASCII_Font
  , uopengl_graphikengine
  , uvectormath;

// for debuging ;)

Procedure Nop();
Begin

End;

{ TPixelEditor }

Procedure TPixelEditor.OnNewButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnOpenButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnSaveButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnSaveAsButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnExitButtonClick(Sender: TObject);
Begin
  If assigned(FormCloseEvent) Then FormCloseEvent(self);
End;

Procedure TPixelEditor.OnGridButtonClick(Sender: TObject);
Begin
  // Der Gridbutton togglet eigentlich nur, den Rest macht ja Render :)
  If GridButton.Style = bsLowered Then Begin
    GridButton.Style := bsRaised;
  End
  Else Begin
    GridButton.Style := bsLowered;
  End;
End;

Procedure TPixelEditor.OnZoomOutButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnZoomInButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnUndoButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnSelectButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnRotateCounterClockwise90ButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnRotateClockwise90ButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnRotate180ButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnRotateAngleButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnBrightenButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnDarkenButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnCurserSize1ButtonClick(Sender: TObject);
Begin
  CurserSize1.Style := bsRaised;
  CurserSize2.Style := bsLowered;
  CurserSize3.Style := bsLowered;
  CurserSize4.Style := bsLowered;
End;

Procedure TPixelEditor.OnCurserSize2ButtonClick(Sender: TObject);
Begin
  CurserSize1.Style := bsLowered;
  CurserSize2.Style := bsRaised;
  CurserSize3.Style := bsLowered;
  CurserSize4.Style := bsLowered;
End;

Procedure TPixelEditor.OnCurserSize3ButtonClick(Sender: TObject);
Begin
  CurserSize1.Style := bsLowered;
  CurserSize2.Style := bsLowered;
  CurserSize3.Style := bsRaised;
  CurserSize4.Style := bsLowered;
End;

Procedure TPixelEditor.OnCurserSize4ButtonClick(Sender: TObject);
Begin
  CurserSize1.Style := bsLowered;
  CurserSize2.Style := bsLowered;
  CurserSize3.Style := bsLowered;
  CurserSize4.Style := bsRaised;
End;

Procedure TPixelEditor.OnSelectLayerButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OpenGLControlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
  fPixelPos := CursorToPixel(x, y);
  If ssLeft In shift Then Begin
    If fPixelPos.X <> -1 Then Begin
      fImage.SetColorAt(fPixelPos.X, fPixelPos.y, fAktualLayer, fCursor.LeftColor);
    End;
  End;
  If ssRight In shift Then Begin
  End;
  UpdateInfoLabel();
End;

Procedure TPixelEditor.OpenGLControlMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
Begin
  fPixelPos := CursorToPixel(x, y);
  If ssLeft In shift Then Begin
    If fPixelPos.X <> -1 Then Begin
      fImage.SetColorAt(fPixelPos.X, fPixelPos.y, fAktualLayer, fCursor.LeftColor);
    End;
  End;
  If ssRight In shift Then Begin
  End;
  UpdateInfoLabel();
End;

Procedure TPixelEditor.OpenGLControlMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; Var Handled: Boolean);
Begin
  ZoomIn();
End;

Procedure TPixelEditor.OpenGLControlMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; Var Handled: Boolean);
Begin
  ZoomOut();
End;

Procedure TPixelEditor.RenderGrid;
Var
  zf: Single;
  i: Integer;
Begin
  // Der Generelle Hintergrund
  glPushMatrix;
  glTranslatef(0, 0, LayerBackGroundGrid);
  glBindTexture(GL_TEXTURE_2D, 0);
  glColor3ub(51, 51, 51);
  glbegin(GL_QUADS);
  glVertex2f(0, 0);
  glVertex2f(640, 0);
  glVertex2f(640, 480);
  glVertex2f(0, 480);
  glEnd;
  // Kein Grid gewünscht / Sinnvoll
  If (GridButton.Style = bsLowered) Or (fZoom <= 5 - 00) Then Begin
    glPopMatrix;
    exit;
  End;
  glTranslatef(75, 38, 0.01); // Anfahren obere Linke Ecke
  glColor3ub(102, 102, 102);
  zf := (fZoom / 100);
  For i := 0 To ceil(640 / zf) Do Begin
    If i Mod 5 = 0 Then glLineWidth(2);
    glBegin(GL_LINES);
    If i <= fImage.Width Then Begin
      glVertex2f(i * zf, 0);
      glVertex2f(i * zf, fImage.Height * zf);
    End;
    If i <= fImage.Height Then Begin
      glVertex2f(0, i * zf);
      glVertex2f(fImage.Width * zf, i * zf);
    End;
    glend;
    glLineWidth(1);
  End;
  glPopMatrix;
End;

Procedure TPixelEditor.RenderImage;
Begin
  glPushMatrix;
  glTranslatef(75, 38, LayerImage); // Anfahren der Linken Oberen Ecke
  glColor4f(1, 1, 1, 1);
  glScalef(fZoom / 100, fZoom / 100, 1);
  fImage.Render();
  glPopMatrix;
End;

Procedure TPixelEditor.OnEraserButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnPencilButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnCursorRoundSize1Click(Sender: TObject);
Begin
  CursorRoundSize1.Style := bsRaised;
  CursorRoundSize2.Style := bsLowered;
  CursorRoundSize3.Style := bsLowered;
  CursorSquareSize1.Style := bsLowered;
  CursorSquareSize2.Style := bsLowered;
  CursorSquareSize3.Style := bsLowered;
End;

Procedure TPixelEditor.OnCursorRoundSize2Click(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnCursorRoundSize3Click(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnCursorSquareSize1Click(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnCursorSquareSize2Click(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnCursorSquareSize3Click(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnLineButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnCircleButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnSquareButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnOutlineButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnFilledButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnMirrorButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnMirror4ButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnMirrorVertButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnMirrorHorButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnFloodFillButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnFloodFillModeButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnColorPickButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnSelectTransparentColorClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnColorClick(Sender: TObject);
Var
  c: TRGBA;
Begin
  c := (sender As TOpenGL_ColorBox).Color;
  If fCursor.LeftColor = c Then Begin
    // TODO: Den ColorPick Dialog auf machen ;)
  End
  Else Begin
    SetLeftColor(C);
  End;
End;

Procedure TPixelEditor.RenderLCL;
Var
  i: integer;
Begin
  // 1. Rendern des Grauen Hintergrunds
  glBindTexture(GL_TEXTURE_2D, 0);
  glPushMatrix;
  glTranslatef(0, 0, LayerFormColor);
  glColor3ub($80, $80, $80);
  glBegin(GL_QUADS);
  // Oben Toolbar
  glVertex2f(-1, -1);
  glVertex2f(-1, 38);
  glVertex2f(641, 38);
  glVertex2f(641, -1);
  // Links Toolbar
  glVertex2f(-1, -1);
  glVertex2f(-1, 481);
  glVertex2f(75, 481);
  glVertex2f(75, -1);
  // Rechts
  glVertex2f(640 - 3, -1);
  glVertex2f(640 - 3, 480);
  glVertex2f(641, 480);
  glVertex2f(641, -1);
  // Unten Toolbar
  glVertex2f(-1, 425);
  glVertex2f(-1, 481);
  glVertex2f(641, 481);
  glVertex2f(641, 425);
  glend();
  glPopMatrix;

  // 2. Rendern der Eigentlichen LCL Elemente
  glPushMatrix;
  glTranslatef(0, 0, LayerLCL);
  For i := 0 To high(FElements) Do Begin
    FElements[i].Render();
  End;
  glPopMatrix;
End;

Procedure TPixelEditor.AddElement(Const value: TOpenGL_BaseClass);
Begin
  setlength(FElements, high(FElements) + 2);
  FElements[high(FElements)] := value;
End;

Procedure TPixelEditor.NewImage(aWidth, aHeight: Integer);
Begin
  // Reset aller Curser
  OnCurserSize1ButtonClick(Nil);
  OnCursorRoundSize1Click(Nil);
  OnOutlineButtonClick(Nil);
  OnMirrorVertButtonClick(Nil);
  SetZoom(1000);
  fImage.SetSize(aWidth, aHeight);
  fImage.Clear(lAll);
  CenterAt(aWidth Div 2, aHeight Div 2);
  GridButton.Style := bsRaised;
  fAktualLayer := lMiddle;
  UpdateInfoLabel();
End;

Procedure TPixelEditor.SetZoom(ZoomValue: integer);
Begin
  fZoom := ZoomValue;
  ZoomInfoTextbox.Caption := inttostr(ZoomValue) + '%';
End;

Procedure TPixelEditor.CenterAt(aX, aY: integer);
Begin

End;

Procedure TPixelEditor.ZoomIn;
Var
  i: integer;
Begin
  For i := 0 To high(ZoomLevels) Do Begin
    If fZoom = ZoomLevels[i] Then Begin
      SetZoom(ZoomLevels[max(0, i - 1)]);
      break;
    End;
  End;
End;

Procedure TPixelEditor.ZoomOut;
Var
  i: integer;
Begin
  For i := 0 To high(ZoomLevels) Do Begin
    If fZoom = ZoomLevels[i] Then Begin
      SetZoom(ZoomLevels[min(high(ZoomLevels), i + 1)]);
      break;
    End;
  End;
End;

Function TPixelEditor.CursorToPixel(x, y: integer): TPoint;
Var
  rx, ry: Single;
  riy, rix: Integer;
Begin
  result := point(-1, -1);
  // 1. Translation auf 0 / 0
  rx := x - (75 * FOwner.Width / 640);
  ry := y - (37 * FOwner.Height / 480);
  // 2. Raus Rechnen der Form Verzerrung
  rx := rx / FOwner.Width * 640;
  ry := ry * 100 / fZoom;
  // 3. Berücksichtigen des Zooms
  rx := rx * 100 / fZoom;
  ry := ry / FOwner.Height * 480;
  // 4. Anpassen Pixel Mittelpunkt
  rx := rx - 0.5;
  ry := ry - 0.5;
  // Limitieren auf die Image Größe
  rix := round(rx);
  riy := round(ry);
  If (rix >= 0) And (rix < fImage.Width) And
    (riy >= 0) And (riy < fImage.Height) Then Begin
    result := point(rix, riy);
  End;
End;

Procedure TPixelEditor.SetLeftColor(Const C: TRGBA);
Begin
  fCursor.LeftColor := c;
End;

Procedure TPixelEditor.UpdateInfoLabel;
Var
  c: TRGBA;
Begin
  If fPixelPos.x = -1 Then Begin
    InfoLabel.caption := '';
    InfoDetailLabel.Caption := '';
  End
  Else Begin
    c := fImage.GetColorAt(fPixelPos.x, fPixelPos.y, fAktualLayer);
    InfoLabel.caption := format('%d,%d', [fPixelPos.x, fPixelPos.y]);
    If c.a = 0 Then Begin
      InfoLabel.caption := InfoLabel.caption + LineEnding + format('%d/%d/%d', [c.r, c.g, c.b]);
    End;
    // TODO: InfoDetailLabel.Caption befüllen
  End;
End;

Constructor TPixelEditor.Create;
Begin
  Inherited Create;
  fImage := TImage.Create();
  FormCloseEvent := Nil;
End;

Destructor TPixelEditor.Destroy;
Var
  i: Integer;
Begin
  fImage.Free;
  For i := 0 To high(FElements) Do Begin
    FElements[i].Free;
  End;
  setlength(FElements, 0);
End;

Procedure TPixelEditor.MakeCurrent(Owner: TOpenGLControl);
Var
  image, i: Integer;
Begin

  FOwner := Owner;

  FElements := Nil;

  owner.OnMouseWheelDown := @OpenGLControlMouseWheelDown;
  owner.OnMouseWheelup := @OpenGLControlMouseWheelUp;
  owner.OnMouseDown := @OpenGLControlMouseDown;
  owner.OnMouseMove := @OpenGLControlMouseMove;

{$I upixeleditor_constructor.inc}

  NewImage(128, 128);

  fCursor.LeftColor := Color1.Color;
  fCursor.RightColor := Transparent;

  // (* TODO: Debug to be removed
  NewImage(32, 32);
  For i := 0 To 9 Do Begin
    fImage.SetColorAt(i, i, fAktualLayer, rgba(255, 0, 0, 0));
  End;
  // *)

End;

Procedure TPixelEditor.Render;
Begin
  RenderLCL;
  RenderGrid;
  RenderImage;
End;

End.

