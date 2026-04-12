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
    MenuItem10: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    OpenGLControl1: TOpenGLControl;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    Separator4: TMenuItem;
    Timer1: TTimer;
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormDropFiles(Sender: TObject; Const FileNames: Array Of String);
    Procedure MenuItem10Click(Sender: TObject);
    Procedure MenuItem1Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure MenuItem3Click(Sender: TObject);
    Procedure MenuItem4Click(Sender: TObject);
    Procedure MenuItem5Click(Sender: TObject);
    Procedure MenuItem6Click(Sender: TObject);
    Procedure MenuItem7Click(Sender: TObject);
    Procedure MenuItem8Click(Sender: TObject);
    Procedure MenuItem9Click(Sender: TObject);
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
  // , unit2 // Options
  // , unit3 // New Dialog
  // , unit4 // Select Transparent Color Dialog
  // , unit5 // Floodfill tolerance Dialog
  // , unit6 // Resize Dialog
  // , unit7 // Rotate preview Dialog
  // , unit8 // Color Curve Dialog
  // , unit9 // Convolution Dialog
  , uopengl_graphikengine
  , uopengl_shaderprimitives
  , uOpenGL_ASCII_Font
  , uopengl_widgetset
  , upixeleditor_types
{$IFNDEF LEGACYMODE}
  , uopengl_legacychecker
{$ENDIF}
  ;

{ TForm1 }

Var
  allowcnt: Integer = 0;

{$IFNDEF LEGACYMODE}

Procedure OnOpenGLLegacyCall(Severity: GLuint; aMessage: String);
Begin
  showmessage(
    format('Error, unallowed OpenGL legacy call: %d = %s', [Severity, aMessage])
    );
  halt;
End;
{$ENDIF}

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
{$IFNDEF LEGACYMODE}
    RegisterLegacyCheckerCallback(@OnOpenGLLegacyCall);
{$ENDIF}
  End;
  If allowcnt = 2 Then Begin // Dieses If Sorgt mit dem obigen dafür, dass der Code nur 1 mal ausgeführt wird.
    (*
    Man bedenke, jedesmal wenn der Renderingcontext neu erstellt wird, müssen sämtliche Graphiken neu Geladen werden.
    Bei Nutzung der TOpenGLGraphikengine, bedeutet dies, das hier ein clear durchgeführt werden mus !!
    *)
    OpenGL_GraphikEngine.clear;
{$IFDEF LEGACYMODE}
    glenable(GL_TEXTURE_2D); // Texturen
{$ENDIF}
    glEnable(GL_DEPTH_TEST); // Tiefentest
    glDepthFunc(gl_less); // Shader negieren uDepth, damit höhere Werte näher sind
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
    OpenGL_ShaderPrimitives_InitializeShaderSystem;
{$ENDIF}
    Create_ASCII_Font;
    Editor.MakeCurrent(OpenGLControl1);
    // Der Anwendung erlauben zu Rendern.
    Initialized := True;
{$IFNDEF LEGACYMODE}
    ReActivateKHRDebug; // Reenable KHRDebug
{$ENDIF}
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
  WidgetSetGo2d(640, 480);
  Editor.Render();
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
    Editor.CheckScrollBorders;
  End;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  (*
   * Recherche:
   * Aseprite: https://github.com/aseprite/aseprite/tree/main
   *           Convolution Matrix: https://github.com/aseprite/aseprite/blob/main/data/convmatr.def
   *
   * Pixelperfect Algo: https://deepnight.net/blog/tools/pixel-perfect-drawing/
   *
   * Abbildungsfunktion f(rgba) -> RGBA
   * Farbersetzungdialog -> mehrere Eingangsfarben werden automatisch convertiert zu mehreren Ausgangsfarben, im Trivialfall ein "suchen/ersetzen"
   *)
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
{$IFNDEF LEGACYMODE}
  OpenGLControl1.AutoResizeViewport := True; // This is crucial for GTK3, don't know why, but without it the demo does not work
  OpenGLControl1.DebugContext := True; // Required so the GL driver actually generates KHR_debug messages
{$ENDIF}
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
{$IFNDEF LEGACYMODE}
  If OpenGLControl1.MakeCurrent Then Begin
    OpenGL_GraphikEngine_FinalizeShaderSystem;
    OpenGL_ShaderPrimitives_FinalizeShaderSystem;
  End;
{$ENDIF}
  editor.Free;
  editor := Nil;
End;

Procedure TForm1.FormDropFiles(Sender: TObject; Const FileNames: Array Of String
  );
Begin
  If lowercase(ExtractFileExt(FileNames[0])) = '.pcp' Then Begin
    Editor.LoadColorPaletteFromFile(FileNames[0]);
  End
  Else Begin
    Editor.LoadImage(FileNames[0]);
  End;
End;

Procedure TForm1.MenuItem10Click(Sender: TObject);
Begin
  Editor.OnClearBackGroundImage;
End;

Procedure TForm1.MenuItem1Click(Sender: TObject);
Begin
  Editor.Spritify;
End;

Procedure TForm1.MenuItem2Click(Sender: TObject);
Begin
  // Nichts das Popup geht auch so zu ;)
End;

Procedure TForm1.MenuItem3Click(Sender: TObject);
Begin
  Editor.InvertColors;
End;

Procedure TForm1.MenuItem4Click(Sender: TObject);
Begin
  Editor.ConvertToGrayscale;
End;

Procedure TForm1.MenuItem5Click(Sender: TObject);
Begin
  Editor.SelectByColor;
End;

Procedure TForm1.MenuItem6Click(Sender: TObject);
Begin
  Editor.InvertSelection;
End;

Procedure TForm1.MenuItem7Click(Sender: TObject);
Begin
  Editor.ExportSelection;
End;

Procedure TForm1.MenuItem8Click(Sender: TObject);
Var
  key: word;
Begin
  key := VK_C;
  OpenGLControl1.OnKeyDown(OpenGLControl1, key, [ssCtrl]);
End;

Procedure TForm1.MenuItem9Click(Sender: TObject);
Begin
  Editor.OnLoadBackgroundImage;
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

