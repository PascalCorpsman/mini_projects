Unit upixeleditorlcl;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, controls, SysUtils, OpenGLContext, uopengl_widgetset,
  ExtCtrls;

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
    Constructor Create(Owner: TOpenGLControl); override;
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
  // glLineWidth(max(FOwner.Width / 640, FOwner.Height / 480) * 2);
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

End.

