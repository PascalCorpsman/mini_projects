(******************************************************************************)
(* Pentago                                                         ??.??.???? *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Implementation of the Board game Pentago playable by 2       *)
(*               players                                                      *)
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
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

{$DEFINE DebuggMode}

Interface

Uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls,
  OpenGlcontext,
  uOpenGL_ASCII_Font,
  math,
  upentogo,
  dglOpenGL // http://wiki.delphigl.com/index.php/dglOpenGL.pas
  // , opengl_graphikengine // Die OpenGLGraphikengine ist eine Eigenproduktionvon www.Corpsman.de, und kann getrennt geladen werden.
  ;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    OpenGLControl1: TOpenGLControl;
    Timer1: TTimer;
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure OpenGLControl1KeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
    Procedure OpenGLControl1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure OpenGLControl1Paint(Sender: TObject);
    Procedure OpenGLControl1Resize(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Procedure Go2d();
    Procedure Exit2d();
  End;

Var
  Form1: TForm1;
  Initialized: Boolean = false; // Wenn True dann ist OpenGL initialisiert
  Pentago: tPentago;

Implementation

{$R *.lfm}

{ TForm1 }

Procedure Tform1.Go2d();
Begin
  glMatrixMode(GL_PROJECTION);
  glPushMatrix(); // Store The Projection Matrix
  glLoadIdentity(); // Reset The Projection Matrix
  //  glOrtho(0, 640, 480,0, -1, 1); // Set Up An Ortho Screen
  glOrtho(0, OpenGLControl1.Width, OpenGLControl1.height, 0, -1, 1); // Set Up An Ortho Screen
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix(); // Store old Modelview Matrix
  glLoadIdentity(); // Reset The Modelview Matrix
End;

Procedure Tform1.Exit2d();
Begin
  glMatrixMode(GL_PROJECTION);
  glPopMatrix(); // Restore old Projection Matrix
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix(); // Restore old Projection Matrix
End;

Var
  allowcnt: Integer = 0;

Procedure TForm1.OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
Begin
  If allowcnt > 2 Then Begin
    exit;
  End;
  inc(allowcnt);
  // Sollen Dialoge beim Starten ausgeführt werden ist hier der Richtige Zeitpunkt
  If allowcnt = 1 Then Begin
    // Init dglOpenGL.pas , Teil 2
    ReadExtensions; // Anstatt der Extentions kann auch nur der Core geladen werden. ReadOpenGLCore;
    ReadImplementationProperties;
  End;
  If allowcnt = 2 Then Begin // Dieses If Sorgt mit dem obigen dafür, dass der Code nur 1 mal ausgeführt wird.
    Create_ASCII_Font();
    Pentago := TPentago.create;
    // Der Anwendung erlauben zu Rendern.
    Initialized := True;
    OpenGLControl1Resize(Nil);
  End;
  Form1.Invalidate;
End;

Procedure TForm1.OpenGLControl1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  Pentago.OnMouseDown(Button, Shift, x, y);
End;

Procedure TForm1.OpenGLControl1Paint(Sender: TObject);
Begin
  If Not Initialized Then Exit;
  // Render Szene
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClear(GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity();
  uOpenGL_ASCII_Font.go2d(640, 480);
  Pentago.render();
  uOpenGL_ASCII_Font.exit2d;
  OpenGLControl1.SwapBuffers;
End;

Procedure TForm1.OpenGLControl1Resize(Sender: TObject);
Begin
  If Initialized Then Begin
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glViewport(0, 0, OpenGLControl1.Width, OpenGLControl1.Height);
    gluPerspective(45.0, OpenGLControl1.Width / OpenGLControl1.Height, 0.1, 100.0);
    glMatrixMode(GL_MODELVIEW);
    glPointSize(max(OpenGLControl1.Width / 640, OpenGLControl1.Height / 480));
  End;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  // Init dglOpenGL.pas , Teil 1
  caption := 'Pentago ver. ' + PentagoVersion + ' by Corpsman www.Corpsman.de';
  If Not InitOpenGl Then Begin
    showmessage('Error, could not init dglOpenGL.pas');
    Halt;
  End;
  (*
  60 - FPS entsprechen
  0.01666666 ms
  Ist Interval auf 16 hängt das gesamte system, bei 17 nicht.
  Generell sollte die Interval Zahl also dynamisch zum Rechenaufwand, mindestens aber immer 17 sein.
  *)
  OpenGLControl1.Align := alClient;
  ClientWidth := 640;
  ClientHeight := 480;
  Randomize;
  Timer1.Interval := 17;
End;

Procedure TForm1.OpenGLControl1KeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  pentago.OnKeyDown(key, Shift);
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  Timer1.Enabled := false;
  Initialized := false;
  Pentago.Free;
End;

Procedure TForm1.Timer1Timer(Sender: TObject);
{$IFDEF DebuggMode}
Var
  i: Cardinal;
  p: Pchar;
{$ENDIF}
Begin
  If Initialized Then Begin
    OpenGLControl1.Invalidate;
{$IFDEF DebuggMode}
    i := glGetError();
    If i <> 0 Then Begin
      Timer1.Enabled := false;
      p := gluErrorString(i);
      showmessage('OpenGL Error (' + inttostr(i) + ') occured.' + #13#13 +
        'OpenGL Message : "' + p + '"'#13#13 +
        'Applikation will be terminated.');
      close;
    End;
{$ENDIF}
  End;
End;

End.

