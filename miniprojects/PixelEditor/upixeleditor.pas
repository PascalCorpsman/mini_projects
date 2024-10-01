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

    // Menüleiste Links
    SelectButton: TOpenGL_Bevel;
    RotateCounterClockwise90: TOpenGL_Bevel;
    RotateClockwise90: TOpenGL_Bevel;
    Rotate180: TOpenGL_Bevel;
    RotateAngle: TOpenGL_Bevel;

    // Menüleiste unten

    SelectLayerButton: TOpenGL_Bevel;

    Procedure OnNewButtonClick(Sender: TObject);
    Procedure OnOpenButtonClick(Sender: TObject);
    Procedure OnSaveButtonClick(Sender: TObject);
    Procedure OnSaveAsButtonClick(Sender: TObject);
    Procedure OnExitButtonClick(Sender: TObject);

    Procedure OnSelectButtonClick(Sender: TObject);
    Procedure OnRotateCounterClockwise90ButtonClick(Sender: TObject);
    Procedure OnRotateClockwise90ButtonClick(Sender: TObject);

    Procedure OnSelectLayerButtonClick(Sender: TObject);

    Procedure RenderLCL;

    Procedure AddElement(Const value: TOpenGL_BaseClass);
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

procedure TPixelEditor.OnNewButtonClick(Sender: TObject);
Begin

End;

procedure TPixelEditor.OnOpenButtonClick(Sender: TObject);
Begin

End;

procedure TPixelEditor.OnSaveButtonClick(Sender: TObject);
Begin

End;

procedure TPixelEditor.OnSaveAsButtonClick(Sender: TObject);
Begin

End;

procedure TPixelEditor.OnExitButtonClick(Sender: TObject);
Begin
  If assigned(FormCloseEvent) Then FormCloseEvent(self);
End;

procedure TPixelEditor.OnSelectButtonClick(Sender: TObject);
Begin

End;

procedure TPixelEditor.OnRotateCounterClockwise90ButtonClick(Sender: TObject);
Begin

End;

procedure TPixelEditor.OnRotateClockwise90ButtonClick(Sender: TObject);
begin

end;

procedure TPixelEditor.OnSelectLayerButtonClick(Sender: TObject);
Begin

End;

procedure TPixelEditor.RenderLCL;
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

procedure TPixelEditor.AddElement(const value: TOpenGL_BaseClass);
Begin
  setlength(FElements, high(FElements) + 2);
  FElements[high(FElements)] := value;
End;

constructor TPixelEditor.Create;
Begin
  Inherited Create;
  FormCloseEvent := Nil;
End;

destructor TPixelEditor.Destroy;
Var
  i: Integer;
Begin
  For i := 0 To high(FElements) Do Begin
    FElements[i].Free;
  End;
  setlength(FElements, 0);
End;

procedure TPixelEditor.MakeCurrent(Owner: TOpenGLControl);
Var
  image: Integer;
Begin
{$I upixeleditor_constructor.inc}
End;

procedure TPixelEditor.Render;
Begin
  RenderLCL;
End;

End.

