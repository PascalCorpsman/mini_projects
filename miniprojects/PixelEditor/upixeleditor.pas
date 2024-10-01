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
  Classes, SysUtils, OpenGLContext, uopengl_widgetset, upixeleditorlcl,
  ExtCtrls;

Const
  (*
   * History: 0.01 - Initialversion
   *)
  Version = '0.01';


  (*
   * Es folgt the "Tiefe" der verschiedenen Render Ebenen [-1 .. 1]
   *
   * Jede Ebene sollte sich zur nächst höheren / tieferen um mindestens 0.01 unterscheiden !
   *)

  LayerBackGroundGrid = -1.0; // Das Grid das hinter allem sein soll und nur bei Transparenten Pixeln zu sehen ist
  LayerImage = -0.9; // Die Eigentliche vom User erstellte Textur
  LayerForeGroundGrid = -0.05;
  LayerFormColor = -0.01;
  LayerLCL = 0.0;

Type


  { TPixelEditor }

  TPixelEditor = Class
  private

    FElements: Array Of TOpenGL_BaseClass;

    // Titelleiste oben
    NewButton: TOpenGL_Bevel;
    OpenButton: TOpenGL_Bevel;
    SaveButton: TOpenGL_Bevel;
    SaveAsButton: TOpenGL_Bevel;
    ExitButton: TOpenGL_Bevel;
    GridButton: TOpenGL_Bevel;

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
    SelectLayerButton: TOpenGL_Bevel;

    Procedure OnNewButtonClick(Sender: TObject);
    Procedure OnOpenButtonClick(Sender: TObject);
    Procedure OnSaveButtonClick(Sender: TObject);
    Procedure OnSaveAsButtonClick(Sender: TObject);
    Procedure OnExitButtonClick(Sender: TObject);
    Procedure OnGridButtonClick(Sender: TObject);

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
    Procedure OnSelectLayerButtonClick(Sender: TObject);
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

    Procedure RenderLCL;

    Procedure AddElement(Const value: TOpenGL_BaseClass);

    Procedure NewImage(aWidth, aHeight: Integer);
  public
    FormCloseEvent: TNotifyEvent; // Um der Besitzerklasse mit zu teilen, dass die Anwendung beendet werden will
    Constructor Create; virtual;
    Destructor Destroy; override;

    Procedure MakeCurrent(Owner: TOpenGLControl);

    Procedure Render();
  End;

Implementation

Uses dglOpenGL, Graphics, uOpenGL_ASCII_Font, uopengl_graphikengine;

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

Procedure TPixelEditor.RenderLCL;
Var
  i: integer;
Begin
  // 1. Rendern des Grauen Hintergrunds
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
End;

Constructor TPixelEditor.Create;
Begin
  Inherited Create;
  FormCloseEvent := Nil;
End;

Destructor TPixelEditor.Destroy;
Var
  i: Integer;
Begin
  For i := 0 To high(FElements) Do Begin
    FElements[i].Free;
  End;
  setlength(FElements, 0);
End;

Procedure TPixelEditor.MakeCurrent(Owner: TOpenGLControl);
Var
  image: Integer;
Begin

{$I upixeleditor_constructor.inc}

  NewImage(128, 128);
End;

Procedure TPixelEditor.Render;
Begin
  RenderLCL;
End;

End.

