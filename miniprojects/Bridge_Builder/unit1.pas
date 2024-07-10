(******************************************************************************)
(* Bridge builder                                                  06.07.2024 *)
(*                                                                            *)
(* Version     : see ubridge_builder.pas                                      *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Bridge building game                                         *)
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
(* History     : ubridge_builder.pas                                          *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}
{$DEFINE DebuggMode}

Interface

Uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Menus, StdCtrls, ExtDlgs,
  OpenGlcontext,
  (*
   * Kommt ein Linkerfehler wegen OpenGL dann: sudo apt-get install freeglut3-dev
   *)
  dglOpenGL // http://wiki.delphigl.com/index.php/dglOpenGL.pas
  , uopengl_graphikengine // Die OpenGLGraphikengine ist eine Eigenproduktion von www.Corpsman.de, und kann getrennt auf https://github.com/PascalCorpsman/Examples/tree/master/OpenGL geladen werden.
  , ubridge_builder
  , uopengl_widgetset
  , uOpenGL_ASCII_Font
  ;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    OpenDialog1: TOpenDialog;
    OpenGLControl1: TOpenGLControl;
    OpenPictureDialog1: TOpenPictureDialog;
    SaveDialog1: TSaveDialog;
    Timer1: TTimer;
    Procedure FormCreate(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure MenuItem3Click(Sender: TObject);
    Procedure MenuItem4Click(Sender: TObject);
    Procedure MenuItem5Click(Sender: TObject);
    Procedure MenuItem6Click(Sender: TObject);
    Procedure MenuItem8Click(Sender: TObject);
    Procedure OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
    Procedure OpenGLControl1Paint(Sender: TObject);
    Procedure OpenGLControl1Resize(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    Procedure SwitchToEditor(Sender: TObject);
    Procedure LeaveEditor(Sender: TObject);
    Procedure LoadLevel(Sender: TObject);
  public
    { public declarations }
    game: TBridgeBuilder;
  End;

Var
  Form1: TForm1;
  Initialized: Boolean = false; // Wenn True dann ist OpenGL initialisiert

Implementation

{$R *.lfm}

{ TForm1 }

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
    (*
    Man bedenke, jedesmal wenn der Renderingcontext neu erstellt wird, müssen sämtliche Graphiken neu Geladen werden.
    Bei Nutzung der TOpenGLGraphikengine, bedeutet dies, das hier ein clear durchgeführt werden mus !!
    *)
//    {
    OpenGL_GraphikEngine.clear;
    glenable(GL_TEXTURE_2D); // Texturen
    //    glEnable(GL_DEPTH_TEST); // Tiefentest
    //    glDepthFunc(gl_less);
    Create_ASCII_Font;
    game.InitOpenGL(OpenGLControl1);
    //    }
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
  WidgetSetGo2d(ScreenWidth, ScreenHeight);
  game.render();
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
  End;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  caption := 'Bridge builder ver. ' + Version + ' by Corpsman';
  // Init dglOpenGL.pas , Teil 1
  If Not InitOpenGl Then Begin
    showmessage('Error, could not init dglOpenGL.pas');
    Halt;
  End;
  game := TBridgeBuilder.create;
  game.OnSwitchToEditor := @SwitchToEditor;
  game.OnLeaveEditor := @LeaveEditor;
  game.OnLoadButton := @LoadLevel;
  LeaveEditor(Nil);
  Form1.ClientWidth := ScreenWidth;
  form1.ClientHeight := ScreenHeight;
  OpenGLControl1.Align := alClient;
  (*
  60 - FPS entsprechen
  0.01666666 ms
  Ist Interval auf 16 hängt das gesamte system, bei 17 nicht.
  Generell sollte die Interval Zahl also dynamisch zum Rechenaufwand, mindestens aber immer 17 sein.
  *)
  Timer1.Interval := 17;
End;

Procedure TForm1.MenuItem2Click(Sender: TObject);
Begin
  game.SwitchToMainMenu;
End;

Procedure TForm1.MenuItem3Click(Sender: TObject);
Begin
  // New Level
  game.Map.Clear;
End;

Procedure TForm1.MenuItem4Click(Sender: TObject);
Begin
  // Save Level as
  If SaveDialog1.Execute Then Begin
    game.Map.SaveToFile(SaveDialog1.Filename);
  End;
End;

Procedure TForm1.MenuItem5Click(Sender: TObject);
Begin
  // Load Level
  If OpenDialog1.Execute Then Begin
    game.LoadFromFile(OpenDialog1.FileName);
  End;
End;

Procedure TForm1.MenuItem6Click(Sender: TObject);
Begin
  // Save Level
  If game.Map.Filename = '' Then Begin
    MenuItem4Click(Nil);
  End
  Else Begin
    game.Map.SaveToFile(game.Map.Filename);
  End;
End;

Procedure TForm1.MenuItem8Click(Sender: TObject);
Begin
  MenuItem8.Checked := Not MenuItem8.Checked;
  game.map.ShowGrid := MenuItem8.Checked;
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

Procedure TForm1.SwitchToEditor(Sender: TObject);
Begin
  menu := MainMenu1;
End;

Procedure TForm1.LeaveEditor(Sender: TObject);
Begin
  menu := Nil;
End;

Procedure TForm1.LoadLevel(Sender: TObject);
Begin
  //  If OpenDialog1.Execute Then
  Begin
    //    game.LoadFromFile(OpenDialog1.FileName);
            //LoadFromFile(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'Levels' + PathDelim + 'Level_01.lvl'); // TODO: Debug remove !
    game.LoadFromFile(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'Levels' + PathDelim + 'Level_02.lvl'); // TODO: Debug remove !
    game.SwitchToGame;
  End;
End;

End.

