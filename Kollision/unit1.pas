(******************************************************************************)
(* Kollision                                                       ??.??.???? *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Simple reaction game, try to let your ball not collide with  *)
(*               others.                                                      *)
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

Interface

Uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, math,
  (*
  Kann die Unit nicht gefunden werden und ist das Package LazOpenGLContext
  installiert, so kann dies folgendermasen gelöst werden :
  Den Dialog
  Project-> Project Inspector
  öffnen, auf das
  "+"
  klicken
  bei "neue Anforderung" in der Rubrik
  "package Name" das Packet LazOpenGLContext anwählen.
  *)
  OpenGlcontext
  , dglOpenGL
  , elements
  , uOpenGL_Primitives
  , lclintf
  , uHighscoreEngine
  ;

Type

  { TForm1 }

  TGameState = (gsWait, gsInGame);

  TBallState = (bsarmed, bsunarmed);

  TForm1 = Class(TForm)
    OpenGLControl1: TOpenGLControl;
    Timer1: TTimer;
    Procedure FormCreate(Sender: TObject);
    Procedure FormKeyPress(Sender: TObject; Var Key: char);
    Procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    Procedure FormResize(Sender: TObject);
    Procedure OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
    Procedure OpenGLControl1Paint(Sender: TObject);
    Procedure OpenGLControl1Resize(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Procedure Go2D;
    Procedure Exit2D;
  End;

  TextBall = Class(TBall)
  private
    createtime: Dword;
  public
    State: TBallState;
    Constructor Create(Position_, SpeedVektor_: TFpoint; Radius_, Mass_: single);
    Function CollideWithOther(Const Ball2: TBall): Boolean; override;
    Procedure Move(dt: Single); override;
  End;

Const
  Ballradius = 15;
  Mass = 1;
  SpeedValue = 500;
  Schonzeit = 2000;
  StartBallCount = 4;
  NextBallDelay = 20000;

Var
  Highscores: THighscoreEngine;
  Form1: TForm1;
  Initialized: Boolean = false;
  Applicationwidth: Integer;
  Applicationheight: Integer;
  GameState: TGameState = gsWait;
  balls: Array Of TextBall;
  OpenGLBall: TOpenGL_Sphere;
  UserBall: TextBall = Nil;
  NextBalltime, starttime: Dword;
  lft, fpsc, FPst: Dword;

Implementation

{$R *.lfm}

Uses Unit2;

{ TForm1 }

Procedure InitBalls;
Var
  i: Integer;
Begin
  If Not assigned(OpenGLBall) Then Begin
    OpenGLBall := TOpenGL_Sphere.create;
    OpenGLBall.SetDimension(Ballradius, 4, 6);
  End;
  For i := 0 To high(balls) Do Begin
    balls[i].free;
  End;
  setlength(balls, StartBallCount);
  For i := 0 To high(balls) Do Begin
    balls[i] := TextBall.create(
      Fpoint(Ballradius + Random(form1.clientwidth - 2 * Ballradius), Ballradius + Random(form1.clientheight - 2 * Ballradius))
      , Fpoint(Random(2 * SpeedValue) - SpeedValue, random(2 * SpeedValue) - SpeedValue)
      , Ballradius
      , Mass
      );
  End;
  UserBall := TextBall.create(fpoint(0, 0), fpoint(0, 0), ballradius, mass);
  UserBall.State := bsarmed;
End;

Procedure DoTimeStep;
Var
  i, j: Integer;
  time: Dword;
Begin
  // Neu Erzeugen von Bällen
  If NextBalltime < Gettickcount Then Begin
    NextBalltime := NextBalltime + NextBallDelay;
    setlength(balls, high(balls) + 2);
    balls[high(balls)] := TextBall.create(
      Fpoint(Ballradius + Random(form1.clientwidth - 2 * Ballradius), Ballradius + Random(form1.clientheight - 2 * Ballradius))
      , Fpoint(Random(2 * SpeedValue) - SpeedValue, random(2 * SpeedValue) - SpeedValue)
      , Ballradius
      , Mass
      );
  End;
  // Movement
  time := GetTickCount;
  For i := 0 To High(balls) Do Begin
    balls[i].Move((Time - lft) / 1000);
    If UserBall.CollideWithOther(balls[i]) Then Begin
      time := GetTickCount;
      GameState := gswait;
      form2.label3.caption := inttostr(time - Starttime);
      form2.showmodal;
    End;
  End;
  // Ball Ball Collision
  For i := 0 To High(balls) Do
    For j := i + 1 To High(balls) Do
      balls[i].CollideWithOther(Balls[j]);
  // Die Borders des Bildschirms
  For i := 0 To High(balls) Do
    balls[i].BorderCollision(rect(0, 0, form1.clientwidth, form1.clientheight), true);
  lft := Gettickcount;
End;

Procedure TForm1.Go2D;
Begin
  glMatrixMode(GL_PROJECTION);
  glPushMatrix(); // Store The Projection Matrix
  glLoadIdentity(); // Reset The Projection Matrix
  (*
  2D-Mode wie Windows, oben Links = (0,0)
  *)
  glOrtho(0, Applicationwidth, Applicationheight, 0, -20, 20); // Set Up An Ortho Screen
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix(); // Store old Modelview Matrix
  glLoadIdentity(); // Reset The Modelview Matrix

End;

Procedure TForm1.Exit2d;
Begin
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix(); // Restore old Projection Matrix
  glMatrixMode(GL_PROJECTION);
  glPopMatrix(); // Restore old Projection Matrix
End;

Procedure TForm1.FormCreate(Sender: TObject);
Var
  i: integer;
  s: String;
Begin
  For i := 1 To Paramcount Do Begin
    s := lowercase(paramstr(i));
    If pos('-rendertimer=', Lowercase(s)) <> 0 Then Begin
      s := copy(s, pos('=', s) + 1, length(s));
      s := trim(s);
      Timer1.Interval := max(1, StrToIntDef(s, 1));
    End;
  End;
  // Highscore Engine
  s := IncludeTrailingBackslash(ExtractFilePath(paramstr(0))) + 'highscores.dat';
  Highscores := THighscoreEngine.create(s, 'Blubberblub', 10);
  Highscores.InsertAlways := true;
  FPst := Gettickcount;
  // Init dglOpenGL.pas , Teil 1
  If Not InitOpenGl Then Begin
    showmessage('Error, could not init dglOpenGL.pas');
    halt;
  End;
  // Das OpenGL Widget initialisieren.
  ClientWidth := 500;
  clientheight := 500;

  // Volle Größe vom Panel Annehmen
  OpenGLControl1.Align := alClient;

  randomize;
  InitBalls;
  Tform(self).Constraints.MaxHeight := Tform(self).Height;
  Tform(self).Constraints.MinHeight := Tform(self).Height;
  Tform(self).Constraints.Maxwidth := Tform(self).width;
  Tform(self).Constraints.Minwidth := Tform(self).width;
End;

Procedure TForm1.FormKeyPress(Sender: TObject; Var Key: char);
Begin
  If key = #27 Then close;
End;

Procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  Case GameState Of
    gsWait: Begin
        caption := 'In game';
        GameState := gsingame;
        starttime := GetTickCount;
        InitBalls;
        NextBalltime := Starttime + NextBallDelay;
        lft := starttime;
      End;
  End;
End;

Procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
Var
  oldpos: Tfpoint;
Begin
  oldpos := UserBall.Position;
  UserBall.Position := fpoint(x, y);
  If UserBall.BorderCollision(rect(0, 0, form1.clientwidth, form1.clientheight), true) Then Begin
    UserBall.Position := oldpos;
  End;
End;

Procedure TForm1.FormResize(Sender: TObject);
Begin
  ClientWidth := 500;
  clientheight := 500;
End;

Var
  allowcnt: Integer = 0;

Procedure TForm1.OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
Const
  LAmbient: Array[0..3] Of Single = (1, 1, 1, 1);
  Lpos: Array[0..3] Of Single = (100, 206, -300, 1);
Begin
  If allowcnt > 2 Then Begin
    exit;
  End;
  inc(allowcnt);
  If allowcnt = 1 Then Begin
    // Init dglOpenGL.pas , Teil 2
    ReadExtensions; // Anstatt der Extentions kann auch nur der Core geladen werden. ReadOpenGLCore;
    ReadImplementationProperties;
  End;
  If allowcnt = 2 Then Begin
    // Hier Kommt der GLInit Teil.
    (*
    Man bedenke, jedesmal wenn der Renderingcontext neu erstellt wird, müssen sämtliche Graphiken neu Geladen werden.
    Bei Nutzung der TOpenGLGraphikengine, bedeutet dies, das hier ein clear durchgeführt werden mus !!
    *)
    glenable(gl_Cull_face);
    glLightfv(GL_LIGHT0, GL_AMBIENT, @LAmbient);
    glLightfv(GL_LIGHT0, GL_POSITION, @Lpos);
    glEnable(GL_LIGHT0);
    glEnable(gl_lighting);

    glColorMaterial(GL_FRONT, GL_DIFFUSE);
    glenable(GL_COLOR_MATERIAL);

    // Der Anwendung erlauben zu Rendern.
    Initialized := True;

  End;
  form1.Invalidate;
End;

Procedure TForm1.OpenGLControl1Paint(Sender: TObject);
Var
  dt, i: Integer;
Begin
  If Not Initialized Then Exit;
  //  FPS und Zeit Zählen
  If Gettickcount - 1000 > fpst Then Begin
    fpst := Gettickcount;
    dt := (fpst - starttime) Div 1000;
    caption := 'FPS : ' + inttostr(fpsc) + ', time :' + inttostr(dt) + ', balls : ' + inttostr(high(balls) + 1) + ' Made by : www.Corpsman.de';
    fpsc := 0;
  End;
  inc(fpsc);
  glClearColor(0.5, 0.5, 0.5, 0.0);
  glClear(GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT);
  Case GameState Of
    gsWait: Begin
        caption := 'Klick on Playground, to start.';
      End;
    gsInGame: Begin
        DoTimeStep;
        Go2d;
        For i := 0 To High(Balls) Do Begin
          glpushmatrix;
          gltranslatef(balls[i].Position.x, balls[i].Position.y, 0);
          glcolor3f(1, 0, 0);
          OpenGLBall.render;
          glpopmatrix;
        End;
        glpushmatrix;
        gltranslatef(UserBall.Position.x, UserBall.Position.y, 0);
        glcolor3f(0, 0, 1);
        OpenGLBall.render;
        glpopmatrix;
        Exit2d;
      End;
  End;
  OpenGLControl1.SwapBuffers;
End;

Procedure TForm1.OpenGLControl1Resize(Sender: TObject);
Begin
  ApplicationWidth := OpenGLControl1.Width;
  ApplicationHeight := OpenGLControl1.Height;
End;

Procedure TForm1.Timer1Timer(Sender: TObject);
Begin
  If Initialized Then Begin
    //  OpenGLControl1.Invalidate;
    OpenGLControl1.DoOnPaint;

  End;
End;

Constructor TExtball.Create(Position_, SpeedVektor_: TFpoint; Radius_, Mass_: single);
Begin
  Inherited Create(Position_, SpeedVektor_, Radius_, Mass_);
  State := bsunarmed;
  createtime := Gettickcount;
End;

Function TExtball.CollideWithOther(Const Ball2: TBall): Boolean;
Begin
  If (State = bsunarmed) Or (TExtball(ball2).State = bsunarmed) Then Begin
    result := false;
  End
  Else
    result := Inherited CollideWithOther(ball2);
End;

Procedure TExtball.Move(dt: Single);
Begin
  If state = bsunarmed Then Begin
    If createtime + Schonzeit < Gettickcount Then
      state := bsarmed;
  End
  Else
    Inherited Move(dt);
End;

End.

