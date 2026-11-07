(******************************************************************************)
(* Pentago                                                         ??.??.???? *)
(*                                                                            *)
(* Version     : 0.02                                                         *)
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
(*               0.02 - Migrate rendering to shaders and GTK3                 *)
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
    Procedure FormDestroy(Sender: TObject);
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
    Procedure SetupFrame;
  public
    { public declarations }
  End;

Var
  Form1: TForm1;
  Initialized: Boolean = false; // Wenn True dann ist OpenGL initialisiert
  Pentago: tPentago;

Implementation

{$R *.lfm}

Uses uopengl_graphikengine, uopengl_shaderprimitives, uopengl_legacychecker;

{ TForm1 }

Var
  allowcnt: Integer = 0;

Procedure OnOpenGLLegacyCall(Severity: GLuint; aMessage: String);
Begin
  Initialized := False;
  ShowMessage('OpenGL error (' + IntToStr(Severity) + '): ' + aMessage +
    LineEnding + 'Application will be terminated.');
  Halt(1);
End;

Procedure TForm1.SetupFrame;
Begin
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClear(GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT);
End;

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
    RegisterLegacyCheckerCallback(@OnOpenGLLegacyCall);
  End;
  If allowcnt = 2 Then Begin // Dieses If Sorgt mit dem obigen dafür, dass der Code nur 1 mal ausgeführt wird.
    OpenGL_GraphikEngine.Clear;
    SetupFrame;
    If Not Assigned(glCreateShader) Then Begin
      ReadExtensions;
      ReadImplementationProperties;
      RegisterLegacyCheckerCallback(@OnOpenGLLegacyCall);
    End;
    If Not Assigned(glCreateShader) Then Begin
      OnOpenGLLegacyCall(0, 'Shader support is required.');
    End;
    OpenGL_GraphikEngine_InitializeShaderSystem;
    OpenGL_ShaderPrimitives_InitializeShaderSystem;
    Create_ASCII_Font();
    Pentago := TPentago.Create;
    // Der Anwendung erlauben zu Rendern.
    Initialized := True;
    ReActivateKHRDebug;
    OpenGLControl1Resize(Nil);
  End;
  Form1.Invalidate;
End;

Procedure TForm1.OpenGLControl1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  If Initialized Then Pentago.OnMouseDown(Button, Shift, X, Y);
End;

Procedure TForm1.OpenGLControl1Paint(Sender: TObject);
Begin
  If Not Initialized Then Exit;
  // Render Szene
  SetupFrame;
  Go2d(640, 480);
  Pentago.Render();
  Exit2d;
  OpenGLControl1.SwapBuffers;
End;

Procedure TForm1.OpenGLControl1Resize(Sender: TObject);
Begin
  If Initialized And OpenGLControl1.MakeCurrent Then Begin
    glViewport(0, 0, OpenGLControl1.Width, OpenGLControl1.Height);
    glShaderPointSize(Max(OpenGLControl1.Width / 640, OpenGLControl1.Height / 480));
    OpenGLControl1.Invalidate;
  End;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  // Init dglOpenGL.pas , Teil 1
  caption := 'Pentago ver. ' + PentagoVersion + ' by Corpsman www.Corpsman.de';
  If Not InitOpenGl Then Begin
    showmessage('Error, could not init dglOpenGL.pas');
    Halt(1);
  End;
  OpenGLControl1.AutoResizeViewport := True;
  OpenGLControl1.DebugContext := True;
  OpenGLControl1.OpenGLMajorVersion := 3;
  OpenGLControl1.OpenGLMinorVersion := 3;
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
  OpenGLControl1.Invalidate;
End;

Procedure TForm1.FormDestroy(Sender: TObject);
Begin
  Timer1.Enabled := False;
  Initialized := False;
  If OpenGLControl1.MakeCurrent Then Begin
    FreeAndNil(Pentago);
    FreeAndNil(OpenGL_ASCII_Font);
    OpenGL_GraphikEngine.Clear;
    OpenGL_ShaderPrimitives_FinalizeShaderSystem;
    OpenGL_GraphikEngine_FinalizeShaderSystem;
  End;
End;

Procedure TForm1.OpenGLControl1KeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If Initialized Then Pentago.OnKeyDown(Key, Shift);
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  Timer1.Enabled := false;
  Initialized := false;
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
      OnOpenGLLegacyCall(i, 'OpenGL Message: "' + p + '"');
    End;
{$ENDIF}
  End;
End;

End.

