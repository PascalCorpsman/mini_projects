(******************************************************************************)
(* MAW                                                             20.11.2006 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Demo for picking and matrix multiplications                  *)
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
  SysUtils, Classes, Controls, Forms, Dialogs,
  ExtCtrls, dglopengl, Marionette, StdCtrls, OpenGLContext;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Button2: TButton;
    Button3: TButton;
    OpenDialog1: TOpenDialog;
    OpenGLControl1: TOpenGLControl;
    SaveDialog1: TSaveDialog;
    Button4: TButton;
    ScrollBar1: TScrollBar;
    Button5: TButton;
    Button6: TButton;
    Label2: TLabel;
    Timer1: TTimer;
    Button7: TButton;
    Button8: TButton;
    Label3: TLabel;
    Timer2: TTimer;
    Button9: TButton;
    Button10: TButton;
    Label4: TLabel;
    Timer3: TTimer;
    Button11: TButton;
    Timer4: TTimer;
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormResize(Sender: TObject);
    Procedure FormKeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure Button1Click(Sender: TObject);
    Procedure OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
    Procedure OpenGLControl1Paint(Sender: TObject);
    Procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    Procedure Button2Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure ScrollBar1Change(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
    Procedure Button5MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure Button5MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure Button6MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure Button7MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure Timer2Timer(Sender: TObject);
    Procedure Button7MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure Button8MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure Button9MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure Button9MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure Timer3Timer(Sender: TObject);
    Procedure Button10MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure Button11Click(Sender: TObject);
    Procedure Timer4Timer(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    // The Render Function
    Procedure Render;
  End;

Const
  ROT_SENSITIVITY: Single = 0.5;

Var
  Form1: TForm1;
  Initialized: Boolean = False;
  Man: TMarionette;
  Mouse: TPoint;
  Selected: Integer = 0;
  xrot, yrot, zrot: Integer;

Implementation

{$R *.lfm}

Procedure Tform1.Render;
Begin
  // Clearscreen
  glClear(GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT);
  glViewport(0, 0, OpenGLControl1.width, OpenGLControl1.height);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  gluLookAt(0, 1.5 + scrollbar1.Position / 10, -3 - Scrollbar1.position / 11, 0, 0, 0, 0, 1, 0);
  man.render;

  // Redraw screen
  OpenGLControl1.SwapBuffers;
End;

Var
  allowcnt: integer = 0;

Procedure TForm1.OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
// Lichtersachen
Const
  LAmbient: Array[0..3] Of Single = (1, 1, 1, 1);
  Lpos: Array[0..3] Of Single = (3, 6, -3, 1);
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
  // Licht
  glLightfv(GL_LIGHT0, GL_AMBIENT, @LAmbient);
  //  glLightfv(GL_Light1, GL_DIFFUSE, @Diffuse);
  //  glLightfv(GL_LIGHT1, GL_SPECULAR, @Spekular);
  glLightfv(GL_LIGHT0, GL_POSITION, @Lpos);
  glEnable(GL_LIGHT0);
  glEnable(gl_lighting);

  // Enable Depth Test
  glEnable(GL_DEPTH_TEST);
  // Set Detph Function
  glDepthFunc(GL_LEQUAL);

  glenable(GL_CULL_FACE);

  // Set Clear Color = black
  glClearColor(0, 0, 0, 0);

  button11.onclick(Nil);
  // Set OpenGL initialized
  Initialized := true;
  OnResize(Nil);
  Timer4.Enabled := true;
End;

Procedure TForm1.OpenGLControl1Paint(Sender: TObject);
Begin
  Render;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  // Init dglOpenGL.pas , Teil 1
  If Not InitOpenGl Then Begin
    showmessage('Error, could not init dglOpenGL.pas');
    Halt;
  End;
  Caption := 'MAW Editor, ver 0.01 by Corpsman | support : www.Corpsman.de';
  Savedialog1.InitialDir := ExtractFilePath(paramstr(0));
  opendialog1.InitialDir := ExtractFilePath(paramstr(0));
  //  label1.left := 656;
  label1.Anchors := [akright, aktop];

  // Aktivate the Idle Procedure
  man := TMarionette.create;
End;

Procedure TForm1.FormDestroy(Sender: TObject);
Begin
  // Free OpenGL
  Initialized := false;
  man.free;
End;

Procedure TForm1.FormResize(Sender: TObject);
Begin
  // resize OpenGL
  If Not Initialized Then exit;
  glViewport(0, 0, OpenGLControl1.Width, OpenGLControl1.height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(90, OpenGLControl1.clientwidth / OpenGLControl1.Clientheight, 0.1, 128);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
End;

Procedure TForm1.FormKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If Key = 27 Then close;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  man.ResetAllRotations;
End;

Procedure TForm1.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  mouse.x := x;
  mouse.y := y;
  // Wenn wir nur Teile des Mänches Auswählen wollen.
  If ssleft In shift Then Begin
    Selected := man.Select(x, form1.OpenGLControl1.height - 1 - y);
    man.SelectBodyPart(Selected);
    //    Caption := inttostr(Selected);
  End;
End;

Procedure TForm1.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
Var
  rotx, roty: Single;
Begin
  If (ssright In shift) Then Begin
    rotY := (x - mouse.X) * ROT_SENSITIVITY;
    rotX := (y - mouse.Y) * ROT_SENSITIVITY;
    mouse.X := x;
    mouse.Y := y;
    man.Rotate(Selected, rotx, roty, 0);
  End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  If Savedialog1.execute Then Begin
    man.SavetoFile(Savedialog1.FileName);
  End;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Begin
  Close;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  If Opendialog1.execute Then Begin
    man.LoadFromFile(Opendialog1.FileName);
  End;
End;

Procedure TForm1.ScrollBar1Change(Sender: TObject);
Begin
  render;
End;

Procedure TForm1.Timer1Timer(Sender: TObject);
Begin
  man.Rotate(Selected, Xrot, 0, 0);
End;

Procedure TForm1.Button5MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  If ssleft In shift Then Begin
    xrot := 1;
    Timer1.enabled := True;
    man.Rotate(Selected, Xrot, 0, 0);
  End;
End;

Procedure TForm1.Button5MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  Timer1.enabled := false;
End;

Procedure TForm1.Button6MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  If ssleft In shift Then Begin
    xrot := -1;
    Timer1.enabled := True;
    man.Rotate(Selected, Xrot, 0, 0);
  End;
End;

Procedure TForm1.Button7MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  If ssleft In shift Then Begin
    yrot := 1;
    Timer2.enabled := True;
    man.Rotate(Selected, 0, yRot, 0);
  End;
End;

Procedure TForm1.Timer2Timer(Sender: TObject);
Begin
  man.Rotate(Selected, 0, Yrot, 0);
End;

Procedure TForm1.Button7MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  Timer2.enabled := False;
End;

Procedure TForm1.Button8MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  If ssleft In shift Then Begin
    yrot := -1;
    Timer2.enabled := True;
    man.Rotate(Selected, 0, yRot, 0);
  End;
End;

Procedure TForm1.Button9MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  If ssleft In shift Then Begin
    zrot := 1;
    Timer3.enabled := True;
    man.Rotate(Selected, 0, 0, ZRot);
  End;
End;

Procedure TForm1.Button9MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  Timer3.enabled := False;
End;

Procedure TForm1.Timer3Timer(Sender: TObject);
Begin
  man.Rotate(Selected, 0, 0, ZRot);
End;

Procedure TForm1.Button10MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  If ssleft In shift Then Begin
    zrot := -1;
    Timer3.enabled := True;
    man.Rotate(Selected, 0, 0, ZRot);
  End;
End;

Procedure TForm1.Button11Click(Sender: TObject);
Begin
  Selected := 1;
  Man.SelectBodyPart(Selected);
End;

Procedure TForm1.Timer4Timer(Sender: TObject);
{$IFDEF DebuggMode}
Var
  i: Cardinal;
  p: Pchar;
{$ENDIF}
Begin
  // Render Scene
  If Initialized Then Begin
    OpenGLControl1.Invalidate;
{$IFDEF DebuggMode}
    i := glGetError();
    If i <> 0 Then Begin
      p := gluErrorString(i);
      showmessage('OpenGL Error (' + inttostr(i) + ') occured.' + #13#13 +
        'OpenGL Message : "' + p + '"'#13#13 +
        'Applikation will be terminated.');
      Timer1.Enabled := false;
      close;
    End;
{$ENDIF}
  End;
End;

End.

