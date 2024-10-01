Unit upixeleditorlcl;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, controls, SysUtils, OpenGLContext, uopengl_widgetset,
  uopengl_graphikengine, ExtCtrls;

Type

  TBevelStyle = ExtCtrls.TBevelStyle;

  { TOpenGL_Bevel }

  TOpenGL_Bevel = Class(TOpenGl_Image)
  protected
    FOwner: TOpenGLControl;
    fmDown: Boolean;
    fStyle: TBevelStyle;
    Procedure OnRender(); override;

    Procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    Procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

  public
    Property Style: TBevelStyle read fStyle write fStyle;
    Constructor Create(Owner: TOpenGLControl); override;
  End;

  { TOpenGL_ToggleButton }

  TOpenGL_ToggleButton = Class(TOpenGL_Bevel)
  protected
    FDownimage: TGraphikItem;
    Procedure Click; override;
    Procedure OnRender(); override;
    Procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

  public
    Procedure SetDownImage(OpenGLIndex: integer); overload;
  End;

Implementation

Uses dglOpenGL, math;

{ TOpenGL_Bevel }

Procedure TOpenGL_Bevel.OnRender;
Begin
  If Not Visible Then exit;
  glPushMatrix;
  glTranslatef(1, 1, 0);
  Width := Width - 2;
  Height := Height - 2;
  Inherited OnRender();
  Width := Width + 2;
  Height := Height + 2;
  glPopMatrix;
  glBindTexture(GL_TEXTURE_2D, 0);
  glPushMatrix;
  glTranslatef(Left, Top, 0);
  If (fStyle = bsRaised) Or (fmDown) Then Begin
    glColor3ub($FF, $FF, $00);
  End
  Else Begin
    glColor3ub($0, $0, $0);
  End;
  //glLineWidth(max(FOwner.Width / 640, FOwner.Height / 480) * 2);
  glLineWidth(max(FOwner.Width / 640, FOwner.Height / 480) * 1); // Debug, zum Ausmessen der Positionen !
  glBegin(GL_LINE_LOOP);
  glVertex2f(0, 1);
  glVertex2f(Width - 1, 1);
  glVertex2f(Width - 1, Height);
  glVertex2f(0, Height);
  glend;
  glPointSize(1);
  glPopMatrix;
End;

Procedure TOpenGL_Bevel.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
Begin
  Inherited MouseDown(Button, Shift, X, Y);
  fmDown := true;
End;

Procedure TOpenGL_Bevel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
Begin
  Inherited MouseUp(Button, Shift, X, Y);
  fmDown := false;
End;

Constructor TOpenGL_Bevel.Create(Owner: TOpenGLControl);
Begin
  FOwner := Owner;
  Inherited Create(Owner);
  fStyle := bsLowered;
  fmDown := false;
End;

{ TOpenGL_ToggleButton }

Procedure TOpenGL_ToggleButton.Click;
Begin
  If fStyle = bsLowered Then Begin
    fStyle := bsRaised;
  End
  Else Begin
    fStyle := bsLowered;
  End;
  Inherited Click();
End;

Procedure TOpenGL_ToggleButton.OnRender;
Var
  tmp: TGraphikItem;
  bool: Boolean;
Begin
  bool := fStyle = bsRaised;
  fStyle := bsLowered;
  If bool Then Begin
    tmp := Fimage;
    Fimage := FDownimage;
  End;
  Inherited OnRender();
  If bool Then Begin
    fStyle := bsRaised;
    Fimage := tmp;
  End;
End;

Procedure TOpenGL_ToggleButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  Inherited MouseDown(Button, Shift, X, Y);
  fmDown := false;
End;

Procedure TOpenGL_ToggleButton.SetDownImage(OpenGLIndex: integer);
Begin
  fDownImage := OpenGL_GraphikEngine.GetInfo(OpenGLIndex);
End;

End.

