(******************************************************************************)
(* PixelEditor                                                     29.09.2024 *)
(*                                                                            *)
(* Version     : see upixeleditor.pas                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Image manipulation program for pixel art images              *)
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
(* History     : see upixeleditor.pas                                         *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

{$DEFINE DebuggMode}

Interface

Uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, IniPropStorage, Menus,
  OpenGlcontext,
  (*
   * Kommt ein Linkerfehler wegen OpenGL dann: sudo apt-get install freeglut3-dev
   *)
  dglOpenGL // http://wiki.delphigl.com/index.php/dglOpenGL.pas
  , upixeleditor
  ;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    IniPropStorage1: TIniPropStorage;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    OpenGLControl1: TOpenGLControl;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    Separator1: TMenuItem;
    Timer1: TTimer;
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormDropFiles(Sender: TObject; Const FileNames: Array Of String);
    Procedure MenuItem1Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
    Procedure OpenGLControl1Paint(Sender: TObject);
    Procedure OpenGLControl1Resize(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    Editor: TPixelEditor;
    Procedure FormCloseEvent(Sender: TObject);
  public
    { public declarations }
  End;

Var
  Form1: TForm1;
  Initialized: Boolean = false; // Wenn True dann ist OpenGL initialisiert


Function GetValue(Identifier, Default: String): String;
Procedure SetValue(Identifier, Value: String);

Implementation

{$R *.lfm}

Uses
  LCLType
  , uopengl_graphikengine
  , uOpenGL_ASCII_Font
  , uopengl_widgetset;

{ TForm1 }

Var
  allowcnt: Integer = 0;

Function GetValue(Identifier, Default: String): String;
Begin
  result := form1.IniPropStorage1.ReadString(Identifier, Default);
End;

Procedure SetValue(Identifier, Value: String);
Begin
  Form1.IniPropStorage1.WriteString(Identifier, Value);
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
  End;
  If allowcnt = 2 Then Begin // Dieses If Sorgt mit dem obigen dafür, dass der Code nur 1 mal ausgeführt wird.
    (*
    Man bedenke, jedesmal wenn der Renderingcontext neu erstellt wird, müssen sämtliche Graphiken neu Geladen werden.
    Bei Nutzung der TOpenGLGraphikengine, bedeutet dies, das hier ein clear durchgeführt werden mus !!
    *)
    OpenGL_GraphikEngine.clear;
    Create_ASCII_Font;
    glenable(GL_TEXTURE_2D); // Texturen
    glEnable(GL_DEPTH_TEST); // Tiefentest
    glDepthFunc(gl_less);
    Editor.MakeCurrent(OpenGLControl1);
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
  glLoadIdentity();
  WidgetSetGo2d(640, 480);
  Editor.Render();
  WidgetSetExit2d();
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
    Editor.CheckScrollBorders;
  End;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  Constraints.MinWidth := 640;
  Constraints.MinHeight := 480;
  defcaption := 'PixelEditor ver. ' + Version + ' by Uwe Schächterle, www.Corpsman.de';
  caption := defcaption;
  Application.Title := 'PixelEditor';
  // Init dglOpenGL.pas , Teil 1
  If Not InitOpenGl Then Begin
    showmessage('Error, could not init dglOpenGL.pas');
    Halt;
  End;
  OpenGLControl1.Align := alClient;
  Editor := TPixelEditor.Create();
  // 25 Bilder die Sekunde reichen, ist ja nur ein Editor ;)
  Timer1.Interval := 40;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  If Editor.Changed Then Begin
    If id_no = Application.MessageBox('There are unsaved changes which will get lost. Do you really want to close without saving?', 'Question', MB_YESNO Or MB_iconQuestion) Then Begin
      canclose := false;
      exit;
    End;
  End;
  Initialized := false;
End;

Procedure TForm1.FormDestroy(Sender: TObject);
Begin
  editor.Free;
End;

Procedure TForm1.FormDropFiles(Sender: TObject; Const FileNames: Array Of String
  );
Begin
  Editor.LoadImage(FileNames[0]);
End;

Procedure TForm1.MenuItem1Click(Sender: TObject);
Begin
  Editor.Spritify;
End;

Procedure TForm1.MenuItem2Click(Sender: TObject);
Begin
  // Nichts das Popup geht auch so zu ;)
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

Procedure TForm1.FormCloseEvent(Sender: TObject);
Begin
  close;
End;

End.

