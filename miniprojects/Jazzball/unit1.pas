(******************************************************************************)
(* Jazz                                                            13.06.2010 *)
(*                                                                            *)
(* Version     : 1.21                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : FPC, OpenGL Reimplemantation of the classic game "jezzball"  *)
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
(*
xxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxx
x   n        xxxxxxxx
x   n        xxxxxxxx
x i n  Ball  xxxxxxxx
x   n        xxxxxxxx
x   n        xxxxxxxx
xxxxxxxxxxxxxxxxxxxxx
xxxxxxxxxxxxxxxxxxxxx

wenn n der neue Balken ist, wird der Bereich i nicht ausgefüllt !!            *)
(*                                                                            *)
(* History     : 0.01 - 1.20 not documented                                   *)
(*               1.21 - switch to shader rendering                            *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

{$DEFINE DebuggMode}

Interface

Uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Menus,
  OpenGlcontext,
  dglOpenGL,
  uopengl_graphikengine // Zum Laden von Graphiken
  , lcltype, StdCtrls
  , ujazz
  , highscoreengine
  , lclintf
  ;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    OpenGLControl1: TOpenGLControl;
    Timer1: TTimer;
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormResize(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure MenuItem3Click(Sender: TObject);
    Procedure MenuItem4Click(Sender: TObject);
    Procedure MenuItem6Click(Sender: TObject);
    Procedure MenuItem7Click(Sender: TObject);
    Procedure MenuItem8Click(Sender: TObject);
    Procedure OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
    Procedure OpenGLControl1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure OpenGLControl1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
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

Implementation

Uses unit2, unit3
  , uopengl_shaderprimitives
  , uopengl_legacychecker
  ;

{$R *.lfm}

{ TForm1 }

Var
  allowcnt: Integer = 0;

Procedure OnOpenGLLegacyCall(Severity: GLuint; aMessage: String);
Begin
  Initialized := false;
  showmessage(
    format('Error, unallowed OpenGL legacy call: %d = %s', [Severity, aMessage])
    );
  halt;
End;

Procedure TForm1.OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
Begin
  If allowcnt > 2 Then Begin
    //allow := false;
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
  If allowcnt = 2 Then Begin
    (*
    Man bedenke, jedesmal wenn der Renderingcontext neu erstellt wird, müssen sämtliche Graphiken neu Geladen werden.
    Bei Nutzung der TOpenGLGraphikengine, bedeutet dies, das hier ein clear durchgeführt werden mus !!
    *)
    OpenGL_GraphikEngine.clear;
    SetupFrame;
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
    LoadGraphiks;
    {
      glEnable(GL_DEPTH_TEST); // Tiefentest
      glDepthFunc(gl_less);
    }
    // Der Anwendung erlauben zu Rendern.
    Initialized := True;
{$IFNDEF LEGACYMODE}
    ReActivateKHRDebug; // Reenable KHRDebug
{$ENDIF}
    OpenGLControl1Resize(Nil);
  End;
  Form1.Invalidate;
End;

Procedure TForm1.OpenGLControl1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  Amousepos := point(x, y);
  If ((x Div 20) > high(level.data) - 1) Or ((x Div 20) = 0) Then exit;
  If ((y Div 20) > high(level.data[1]) - 1) Or ((y Div 20) = 0) Then exit;
  If Sinnlos Then Begin
    If Button = mbLeft Then level.data[x Div 20, y Div 20] := 1;
    If Button = mbright Then level.data[x Div 20, y Div 20] := 0;
  End
  Else Begin
    If (Button = mbLeft) And Not (BarOnProgress) Then Begin
      If (x Div 20) > high(level.data) Then exit;
      If (y Div 20) > high(level.data[1]) Then exit;
      level.data[x Div 20, y Div 20] := 2;
      BarOnProgress := true;
    End;
    If Button = mbright Then Begin
      Mousedir := Not mousedir; // mousedir = true = waagrecht
      If windowscursor Then Begin
        form1.image1.visible := Mousedir;
        form1.image2.visible := Not Mousedir;
      End;
    End;
  End;
End;

Procedure TForm1.OpenGLControl1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
Begin
  If sinnlos Then Begin
    If ((x Div 20) > high(level.data) - 1) Or ((x Div 20) = 0) Then exit;
    If ((y Div 20) > high(level.data[1]) - 1) Or ((y Div 20) = 0) Then exit;
    If ssleft In shift Then level.data[x Div 20, y Div 20] := 1;
    If ssright In shift Then level.data[x Div 20, y Div 20] := 0;
  End;
  Amousepos := point(x, y);
End;

Procedure TForm1.OpenGLControl1Paint(Sender: TObject);
Begin
  If Not Initialized Then Exit;
  // Render Szene
  SetupFrame;
  go2d(OpenGLControl1.Width, OpenGLControl1.Height);
  If GameState = gmIngame Then Begin
    If Not (sinnlos) Then Begin // zählt in 1 Sekundenschritte Nach unten
      If (gettickcount - 1000) >= Uhrtime Then Begin
        Uhrtime := gettickcount;
        dec(level.time);
        label2.caption := 'Time : ' + inttostr(level.time);
      End;
      If (gettickcount - barspeed) >= bartime Then Begin
        bartime := gettickcount;
        barprogress;
      End;
      If level.time = 0 Then spielende;
    End;
    MoveBalls;
    RenderGame;
  End;
  exit2d;
  OpenGLControl1.SwapBuffers;
End;

Procedure TForm1.OpenGLControl1Resize(Sender: TObject);
Begin
  If Initialized Then Begin
    If OpenGLControl1.MakeCurrent Then
      glViewport(0, 0, OpenGLControl1.Width, OpenGLControl1.Height);
    OpenGLControl1.Invalidate;
  End;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  caption := 'Jazzball ver. ' + floattostrf(version, fffixed, 7, 2);
  color := clblack;
  label1.font.color := cllime;
  label2.font.color := cllime;
  label3.font.color := cllime;
  label4.font.color := cllime;
  label5.font.color := cllime;
  Randomize;
  // Init dglOpenGL.pas , Teil 1
  If Not InitOpenGl Then Begin
    showmessage('Error, could not init dglOpenGL.pas');
    Halt;
  End;
  OpenGLControl1.AutoResizeViewport := True; // This is crucial for GTK3, don't know why, but without it the demo does not work
  OpenGLControl1.DebugContext := True; // Required so the GL driver actually generates KHR_debug messages
  (*
  60 - FPS entsprechen
  0.01666666 ms
  Ist Interval auf 16 hängt das gesamte system, bei 17 nicht.
  Generell sollte die Interval Zahl also dynamisch zum Rechenaufwand, mindestens aber immer 17 sein.
  *)
  Timer1.Interval := 20; // = 50 Bilder die Sekunde.
  highscore := thighscore.create(IncludeTrailingPathDelimiter(extractfilepath(paramstr(0))) + 'highscore.dat', 17);
  Mousedir := true;
  Readini;
  image1.visible := false;
  image2.visible := false;
  label1.Visible := false;
  label2.Visible := false;
  label3.Visible := false;
  label4.Visible := false;
  label5.Visible := false;
  width := screenwidth;
  height := screenheight;
  top := round((screen.height / 2) - (screenheight / 2));
  left := round((screen.width / 2) - (screenwidth / 2));
  Invalidate; // Trigger a new OpenGLContext creation ..
End;

Procedure TForm1.FormDestroy(Sender: TObject);
Begin
  If OpenGLControl1.MakeCurrent Then Begin
    OpenGL_GraphikEngine_FinalizeShaderSystem;
    OpenGL_ShaderPrimitives_FinalizeShaderSystem;
  End;
End;

Procedure TForm1.FormResize(Sender: TObject);
Const
  leftoff = 80;
Var
  h, dh: Integer;
Begin
  image1.top := 10;
  image2.top := 10;
  image1.left := form1.width - leftoff + 25;
  image2.left := form1.width - leftoff + 25;
  h := OpenGLControl1.Height - 50;
  dh := h Div 5;
  label1.left := form1.width - leftoff + 5;
  label1.top := 70;
  label2.left := form1.width - leftoff + 5;
  label2.top := 70 + dh;
  label3.left := form1.width - leftoff + 5;
  label3.top := 70 + 2 * dh;
  label4.left := form1.width - leftoff + 5;
  label4.top := 70 + 3 * dh;
  label5.left := form1.width - leftoff + 5;
  label5.top := 70 + 4 * dh;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Var
  i: Integer;
Begin
  Initialized := false;
  writeini;
  For i := 0 To High(balls) Do
    balls[i].free;
  setlength(balls, 0);
End;

Procedure TForm1.MenuItem2Click(Sender: TObject);
Begin
  Uhrtime := gettickcount;
  bartime := gettickcount;
  level.Count := 0;
  form1.Label1.caption := 'Level : ' + inttostr(level.Count + 1);
  level.points := 0;
  form1.label5.caption := 'Points : ' + inttostr(level.points);
  mousedir := true;
  naechsteslevel;
  ballspeedinc := 0;
  // Damit die Labels richtig gesetzt werden.
  FormResize(Nil);
End;

Procedure TForm1.MenuItem3Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.MenuItem4Click(Sender: TObject);
Begin
  form2.Edit1.text := inttostr(screenwidth);
  form2.Edit2.text := inttostr(screenheight);
  form2.Edit3.text := inttostr(BallRotatespeed);
  form2.edit4.text := Floattostr(Ballspeed);
  form2.edit5.text := inttostr(minBall);
  form2.edit6.text := inttostr(maxtime);
  form2.edit7.text := inttostr(sprung);
  form2.edit8.text := inttostr(barspeed);
  form2.edit9.text := inttostr(autosprung);
  form2.edit10.text := Bezeichnung;
  form2.edit11.text := floattostr(bAllspeedinc2);
  form2.edit12.text := inttostr(teiler);
  form2.checkbox1.checked := windowscursor;
  form2.checkbox2.checked := sinnlos;
  form2.checkbox3.checked := ballcollision;
  form2.showmodal;
  If GameState = gmIngame Then Begin
    If sinnlos Then Begin
      OpenGLControl1.Cursor := crdefault;
      Image1.Visible := false;
      Image1.Visible := false;
      Label1.Visible := false;
      Label2.Visible := false;
      Label3.Visible := false;
      Label4.Visible := false;
      Label5.Visible := false;
    End
    Else Begin
      Label1.Visible := true;
      Label2.Visible := true;
      Label3.Visible := true;
      Label4.Visible := true;
      Label5.Visible := true;
      If Windowscursor Then Begin
        OpenGLControl1.Cursor := crdefault;
        Image1.Visible := mousedir;
        Image1.Visible := Not mousedir;
      End
      Else Begin
        OpenGLControl1.Cursor := crnone;
        OpenGLControl1.SetFocus;
        Image1.Visible := false;
        Image1.Visible := false;
      End;
    End;
  End;
End;

Procedure TForm1.MenuItem6Click(Sender: TObject);
Begin
  form3.Memo1.clear;
  form3.Memo1.lines := highscore.show;
  If form3.memo1.lines.count <> 0 Then
    form3.memo1.lines.Insert(0, '');
  form3.ShowModal;
End;

Procedure TForm1.MenuItem7Click(Sender: TObject);
Begin
  Application.MessageBox(
    'The Game is the reimplementation from the Jazzbal Game from Suse Linux.' + LineEnding +
    'The goal of the game is to fill as much blocks as possible.' + LineEnding + LineEnding +
    'To switch between horizontal and vertical use the right mouse button.'
    , ' Info', MB_ICONINFORMATION Or MB_OK);
End;

Procedure TForm1.MenuItem8Click(Sender: TObject);
Begin
  //  showmessage('Dieses Spiel wurde erstellt von Corpsman@web.de');
  Application.MessageBox('This game was created from corpsman' + LineEnding + LineEnding +
    'support : www.Corpsman.de' + LineEnding +
    'license : https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md' + LineEnding +
    'warranty : there is no warranty', 'Info', MB_ICONINFORMATION Or MB_OK);
End;

Procedure TForm1.Timer1Timer(Sender: TObject);
{$IFDEF DebuggMode}
Var
  i: Cardinal;
  p: Pchar;
{$ENDIF}
Begin
  If Initialized Then Begin
    //OpenGLControl1.DoOnPaint;
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

Procedure TForm1.SetupFrame;
Begin
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClear(GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT);
End;

End.

