(******************************************************************************)
(* RGB_Jumper                                                      12.07.2025 *)
(*                                                                            *)
(* Version     : 0.02                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Tiny Jump and Run game                                       *)
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
(*               0.02 - Port to shader                                        *)
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
  (*
   * Kommt ein Linkerfehler wegen OpenGL dann: sudo apt-get install freeglut3-dev
   *)
  dglOpenGL // http://wiki.delphigl.com/index.php/dglOpenGL.pas
  , uopengl_graphikengine // Die OpenGLGraphikengine ist eine Eigenproduktion von www.Corpsman.de, und kann getrennt auf https://github.com/PascalCorpsman/Examples/tree/master/OpenGL geladen werden.
  , uOpenGL_ASCII_Font
  , uopengl_legacychecker
  , urgb_jumper
  ;

Const
  DefCaption = 'RGB_Jumper by Corpsman ver. 0.02';

Type

  { TForm1 }

  TForm1 = Class(TForm)
    OpenGLControl1: TOpenGLControl;
    Timer1: TTimer;
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
    Procedure OpenGLControl1Paint(Sender: TObject);
    Procedure OpenGLControl1Resize(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Game: TGame;
  End;

Var
  Form1: TForm1;
  Initialized: Boolean = false; // Wenn True dann ist OpenGL initialisiert

Implementation

{$R *.lfm}

Uses uopengl_widgetset;

{ TForm1 }

Var
  allowcnt: Integer = 0;

{$IFNDEF LEGACYMODE}

Procedure OnOpenGLLegacyCall(Severity: GLuint; aMessage: String);
Begin
  Initialized := false;
  showmessage(
    format('Error, unallowed OpenGL legacy call: %d = %s', [Severity, aMessage])
    );
  halt;
End;
{$ENDIF}

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
{$IFNDEF LEGACYMODE}
    RegisterLegacyCheckerCallback(@OnOpenGLLegacyCall);
{$ENDIF}
  End;
  If allowcnt = 2 Then Begin // Dieses If Sorgt mit dem obigen dafür, dass der Code nur 1 mal ausgeführt wird.
    OpenGL_GraphikEngine.clear;
    Create_ASCII_Font();
{$IFDEF LEGACYMODE}
    glenable(GL_TEXTURE_2D); // Texturen
{$ENDIF}
    //    glEnable(GL_DEPTH_TEST); // Tiefentest
    //    glDepthFunc(gl_less);
{$IFNDEF LEGACYMODE}
    If Not Assigned(glCreateShader) Then Begin
      // On Windows it seems that you need to "reload" the core functions for proper function
      ReadExtensions;
      ReadImplementationProperties;
      RegisterLegacyCheckerCallback(@OnOpenGLLegacyCall);
      // if still not available, then halt
      If Not Assigned(glCreateShader) Then Begin
        showmessage('glCreateShader not available, use legacy mode..');
        halt;
      End;
    End;
    OpenGL_GraphikEngine_InitializeShaderSystem;
{$ENDIF}
{$IFNDEF LEGACYMODE}
    ReActivateKHRDebug; // Reenable KHRDebug
{$ENDIF}
    game.Initialize(OpenGLControl1);
    // Der Anwendung erlauben zu Rendern.
    Initialized := True;
    OpenGLControl1Resize(Nil);
  End;
  Form1.Invalidate;
End;

Procedure TForm1.OpenGLControl1Paint(Sender: TObject);
Begin
  If Not Initialized Then Exit;
  // Render Szene
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClear(GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT);
{$IFDEF LEGACYMODE}
  glLoadIdentity();
{$ENDIF}
  WidgetSetGo2d(128, 128);
  game.Render();
  WidgetSetExit2d();
  OpenGLControl1.SwapBuffers;
End;

Procedure TForm1.OpenGLControl1Resize(Sender: TObject);
Begin
  If Initialized Then Begin
{$IFDEF LEGACYMODE}
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glViewport(0, 0, OpenGLControl1.Width, OpenGLControl1.Height);
    gluPerspective(45.0, OpenGLControl1.Width / OpenGLControl1.Height, 0.1, 100.0);
    glMatrixMode(GL_MODELVIEW);
{$ELSE}
    If OpenGLControl1.MakeCurrent Then
      glViewport(0, 0, OpenGLControl1.Width, OpenGLControl1.Height);
    OpenGLControl1.Invalidate;
{$ENDIF}
  End;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  caption := DefCaption;
  // Init dglOpenGL.pas , Teil 1
  If Not InitOpenGl Then Begin
    showmessage('Error, could not init dglOpenGL.pas');
    Halt;
  End;
{$IFNDEF LEGACYMODE}
  OpenGLControl1.AutoResizeViewport := True; // This is crucial for GTK3, don't know why, but without it the demo does not work
  OpenGLControl1.DebugContext := True; // Required so the GL driver actually generates KHR_debug messages
{$ENDIF}
  (*
  60 - FPS entsprechen
  0.01666666 ms
  Ist Interval auf 16 hängt das gesamte system, bei 17 nicht.
  Generell sollte die Interval Zahl also dynamisch zum Rechenaufwand, mindestens aber immer 17 sein.
  *)
  Timer1.Interval := 17;
  game := TGame.Create();
  OpenGLControl1.Align := alClient;
End;

Procedure TForm1.FormDestroy(Sender: TObject);
Begin
{$IFNDEF LEGACYMODE}
  If OpenGLControl1.MakeCurrent Then Begin
    OpenGL_GraphikEngine_FinalizeShaderSystem;
  End;
{$ENDIF}
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  Initialized := false;
  game.free;
  game := Nil;
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
      showmessage('OpenGL Error (' + inttostr(i) + ') occured.' + LineEnding + LineEnding +
        'OpenGL Message : "' + p + '"' + LineEnding + LineEnding +
        'Applikation will be terminated.');
      close;
    End;
{$ENDIF}
  End;
End;

End.

