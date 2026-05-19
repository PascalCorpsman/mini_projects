(******************************************************************************)
(* Pongwars                                                        21.09.2025 *)
(*                                                                            *)
(* Version     : 0.03                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Implements the pongwars simulation                           *)
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
(*               0.02 - show points on right sides                            *)
(*               0.03 - port to shader rendering                              *)
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
  , uopengl_legacychecker
  , uopengl_shaderprimitives
  , uOpenGL_ASCII_Font
  , uvectormath;

Const
  BrightCol = $FFCC99;
  DarkCol = $996600;
  BlockDim = 25;

Type

  TPong = Record
    Position: TVector2;
    Velocity: TVector2;
    Color: TVector3;
  End;

  TPongWars = Record
    Field: Array Of Array Of Boolean; // True = Bright, False = Dark
    Bright, Dark: TPong;
    BallTexture: TGraphikItem;
    Paused: Boolean;
    JitterDirectionAndSpeed: Boolean;
    MaxSpeed, MinSpeed: Single;
  End;

  { TForm1 }

  TForm1 = Class(TForm)
    OpenGLControl1: TOpenGLControl;
    Timer1: TTimer;
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure OpenGLControl1KeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
    Procedure OpenGLControl1Paint(Sender: TObject);
    Procedure OpenGLControl1Resize(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    PongWars: TPongWars;

    Procedure CenterFormOnScreen;
    Procedure ReadParameters;
    Procedure CreateTextures;
    Procedure Reset;
    Procedure Simulate;
  public
    { public declarations }
  End;

Var
  Form1: TForm1;
  Initialized: Boolean = false; // Wenn True dann ist OpenGL initialisiert

Implementation

{$R *.lfm}

Uses LCLType, math;

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
    (*
    Man bedenke, jedesmal wenn der Renderingcontext neu erstellt wird, müssen sämtliche Graphiken neu Geladen werden.
    Bei Nutzung der TOpenGLGraphikengine, bedeutet dies, das hier ein clear durchgeführt werden mus !!
    *)
    OpenGL_GraphikEngine.clear;
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
    Create_ASCII_BigFont;
    // Der Anwendung erlauben zu Rendern.
    Initialized := True;
    ReActivateKHRDebug; // Reenable KHRDebug
    OpenGLControl1Resize(Nil);
    ReadParameters;
    CreateTextures;
  End;
  Form1.Invalidate;
End;

Procedure TForm1.OpenGLControl1Paint(Sender: TObject);
  Procedure Pixel(i, j: Single);
  Begin
    glShaderBegin(GL_TRIANGLE_FAN);
    glShaderVertex(i * BlockDim, j * BlockDim, 0);
    glShaderVertex(i * BlockDim, j * BlockDim + BlockDim, 0);
    glShaderVertex(i * BlockDim + BlockDim, j * BlockDim + BlockDim, 0);
    glShaderVertex(i * BlockDim + BlockDim, j * BlockDim, 0);
    glShaderEnd();
  End;

Var
  bright, dark, i, j: Integer;
  s: String;
Begin
  If Not Initialized Then Exit;
  If PongWars.Paused Then exit;
  Simulate;
  // Render Szene
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClear(GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT);
  go2d(OpenGLControl1.Width, OpenGLControl1.Height);
  // 1. Das Feld
  UseColorShader;
  bright := 0;
  dark := 0;
  For i := 0 To high(PongWars.Field) Do Begin
    For j := 0 To high(PongWars.Field[0]) Do Begin
      If PongWars.Field[i, j] Then Begin
        inc(bright);
        SetShaderColor(PongWars.Bright.Color.x, PongWars.Bright.Color.y, PongWars.Bright.Color.z);
      End
      Else Begin
        inc(dark);
        SetShaderColor(PongWars.Dark.Color.x, PongWars.Dark.Color.y, PongWars.Dark.Color.z);
      End;
      Pixel(i, j);
    End;
  End;
  // 2. Die Spieler (ohne tiefentest geht das ;))
  UseTextureShader(v4(PongWars.Bright.Color, 1));
  glenable(gl_Blend);
  glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);
  RenderQuad(PongWars.Bright.Position * BlockDim + v2(BlockDim, BlockDim) / 2, 0, BlockDim, BlockDim, 0, PongWars.BallTexture);
  UseTextureShader(v4(PongWars.Dark.Color, 1));
  RenderQuad(PongWars.Dark.Position * BlockDim + v2(BlockDim, BlockDim) / 2, 0, BlockDim, BlockDim, 0, PongWars.BallTexture);
  gldisable(gl_blend);
  // Die Lebenspunkte
  glBindTexture(GL_TEXTURE_2D, 0);
  // Show the darks points (which are the bright fields) on the left
  OpenGL_ASCII_Font.ColorV3 := PongWars.Dark.Color;
  OpenGL_ASCII_Font.Textout(10, 10, inttostr(bright));
  // Show the brights points (which are the dark fields) on the right
  OpenGL_ASCII_Font.ColorV3 := PongWars.Bright.Color;
  s := inttostr(dark);
  OpenGL_ASCII_Font.Textout(Width - 10 - length(s) * 10, 10, s);
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
  // Init dglOpenGL.pas , Teil 1
  If Not InitOpenGl Then Begin
    showmessage('Error, could not init dglOpenGL.pas');
    Halt;
  End;
  OpenGLControl1.AutoResizeViewport := True; // This is crucial for GTK3, don't know why, but without it the demo does not work
  OpenGLControl1.DebugContext := True; // Required so the GL driver actually generates KHR_debug messages
  caption := 'Pongwars 0.03, by Corpsman';
  Randomize;
  OpenGLControl1.Align := alClient;
  (*
  60 - FPS entsprechen
  0.01666666 ms
  Ist Interval auf 16 hängt das gesamte system, bei 17 nicht.
  Generell sollte die Interval Zahl also dynamisch zum Rechenaufwand, mindestens aber immer 17 sein.
  *)
  Timer1.Interval := 17;
End;

Procedure TForm1.FormDestroy(Sender: TObject);
Begin
  If OpenGLControl1.MakeCurrent Then Begin
    OpenGL_GraphikEngine_FinalizeShaderSystem;
    OpenGL_ShaderPrimitives_FinalizeShaderSystem;
  End;
End;

Procedure TForm1.OpenGLControl1KeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If key = vk_Space Then Begin
    PongWars.Paused := Not PongWars.Paused;
  End;
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

Procedure TForm1.CenterFormOnScreen;
Begin
  left := (Monitor.Width - width) Div 2;
  Top := (Monitor.Height - Height) Div 2;
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
End;

Procedure TForm1.ReadParameters;
Var
  i, j: Integer;
  b: Boolean;
  newWidth, newheight: LongInt;
Begin
  Reset;
  (*
   * ggf. überschreiben der Defaults durch Parameter
   *)
  i := 1;
  newWidth := 24;
  newheight := 24;
  While i <= ParamCount Do Begin
    Case lowercase(ParamStr(i)) Of
      'jitter': Begin
          inc(i);
          PongWars.JitterDirectionAndSpeed := lowercase(ParamStr(i)) = 'true';
        End;
      'paused': Begin
          inc(i);
          PongWars.Paused := lowercase(ParamStr(i)) = 'true';
        End;
      'width': Begin
          inc(i);
          newWidth := strtointdef(ParamStr(i), newWidth);
        End;
      'height': Begin
          inc(i);
          newheight := strtointdef(ParamStr(i), newheight);
        End;
      // TODO: mehr parametrisierbar machen ;)
    End;
    inc(i);
  End;
  setlength(PongWars.Field, newWidth, newheight);
  Width := BlockDim * newWidth;
  Height := BlockDim * newheight;
  PongWars.Dark.Position := v2(newWidth Div 4, newheight Div 2);
  PongWars.Bright.Position := v2((newWidth * 3) Div 4, newheight Div 2);
  CenterFormOnScreen;
  (*
   * Erzeugen des "Spiels"
   *)
  For i := 0 To newWidth - 1 Do Begin
    b := i <= (newWidth Div 2) - 1;
    For j := 0 To newheight - 1 Do Begin
      PongWars.Field[i, j] := b;
    End;
  End;
End;

Procedure TForm1.CreateTextures;
Begin
  PongWars.BallTexture := OpenGL_GraphikEngine.LoadAlphaGraphikItem('gfx' + PathDelim + 'ball.bmp');
End;

Procedure TForm1.Reset;

(*
 * Erzeugt eine Richtung die entlang der beiden Hauptdiagonalen mit einer Streuung von 22.5 Grad verläuft
 *)
  Function getRandomDirection(): TVector2;
  Var
    angle: Single;
    radAngle: Float;
  Begin
    angle := random(46) - 22.5; // Zufallszahl im Bereich [-22.5..22.5]
    angle := angle + random(4) * 90; // Zufälliger Quadrant
    angle := angle + 45; // Drehen um 45°, dass es die Diagonalen trift
    // Umrechnen in einen Vektor
    radAngle := DegToRad(angle);
    SinCos(radAngle, result.x, result.y);
  End;

Var
  DefaultSpeed: Single;
Begin
  (*
   * Alle Defaults setzen
   *)
  PongWars.Field := Nil;
  PongWars.JitterDirectionAndSpeed := true;
  PongWars.MinSpeed := 5;
  PongWars.MaxSpeed := 15;
  DefaultSpeed := (PongWars.MaxSpeed + PongWars.MinSpeed) / 2;
  PongWars.Paused := false;
  PongWars.Bright.Color := v3(
    (BrightCol And $FF) / 255,
    ((BrightCol Shr 8) And $FF) / 255,
    ((BrightCol Shr 16) And $FF) / 255
    );

  PongWars.Bright.Velocity := getRandomDirection * DefaultSpeed;
  PongWars.Bright.Velocity.x := abs(PongWars.Bright.Velocity.x); // Sicherstellen, dass der Ball nach Rechts geht ;)
  PongWars.Dark.Color := v3(
    (DarkCol And $FF) / 255,
    ((DarkCol Shr 8) And $FF) / 255,
    ((DarkCol Shr 16) And $FF) / 255
    );

  PongWars.Dark.Velocity := getRandomDirection * DefaultSpeed;
  PongWars.Dark.Velocity.x := -abs(PongWars.Dark.Velocity.x); // Sicherstellen, dass der Ball nach Links geht ;)
End;

Procedure TForm1.Simulate;
Const
  dt = 0.017; // 17ms

  Procedure Move(Var Pong: TPong);
  Begin
    pong.Position := pong.Position + pong.Velocity * dt;
  End;

  Procedure CollideWithWorld(Var Pong: TPong);
  Var
    InvMove: Boolean;
  Begin
    InvMove := false;
    // 1. Kollision gegen den Welt Rand
    If (pong.Position.x < 0) Or (pong.Position.x >= high(PongWars.Field)) Then Begin
      pong.Velocity.x := -pong.Velocity.x;
      InvMove := true;
    End;
    If (pong.Position.y < 0) Or (pong.Position.y >= high(PongWars.Field[0])) Then Begin
      pong.Velocity.y := -pong.Velocity.y;
      InvMove := true;
    End;
    If InvMove Then move(pong); // Wieder Zurück aufs Feld
  End;

  Procedure CollideWithBlocks(Var Pong: TPong; OwnColl: Boolean);
  Const
    Epsilon = 0.5;
    dirs: Array[0..3] Of TVector2 = (// Achtung ist wichtig für die Bounce Berechnung unten, dass die Reihenfolge stimmt
      (x: Epsilon; y: 0), // 0°
      (x: 0; y: - Epsilon), // 90°
      (x: - Epsilon; y: 0), // 180°
      (x: 0; y: Epsilon) // 270°
      );
  Var
    i: Integer;
    ipos: TPoint;
  Begin
    For i := 0 To 3 Do Begin
      ipos := Pong.Position + dirs[i];
      If (ipos.x < 0) Or (ipos.Y < 0) Or
        (ipos.x > high(PongWars.Field)) Or
        (ipos.Y > high(PongWars.Field[0])) Then Continue;
      If PongWars.Field[ipos.X, ipos.y] = OwnColl Then Begin
        PongWars.Field[ipos.X, ipos.y] := Not PongWars.Field[ipos.X, ipos.y];
        If abs(cos(i * pi / 2)) > abs(sin(i * pi / 2)) Then Begin
          pong.Velocity.x := -pong.Velocity.x;
        End
        Else Begin
          pong.Velocity.y := -pong.Velocity.y;
        End;
        move(pong);
      End;
    End;
  End;

  Procedure JitterDirectionAndSpeed(Var Pong: TPong);
  Var
    speed: Single;
  Begin
    pong.Velocity.x := pong.Velocity.x + random * 0.02 - 0.01;
    pong.Velocity.y := pong.Velocity.y + random * 0.02 - 0.01;
    speed := LenV2(pong.Velocity);
    // Limit the speed of the ball
    If speed < PongWars.MinSpeed Then Begin
      pong.Velocity := (pong.Velocity * PongWars.MinSpeed) / speed;
    End;
    If speed > PongWars.MaxSpeed Then Begin
      pong.Velocity := (pong.Velocity * PongWars.MaxSpeed) / speed;
    End;
  End;

Begin
  // Simuliert einen Zeitschritt
  // 1. Bewegen der Bälle
  Move(PongWars.Bright);
  Move(PongWars.Dark);
  // 2. Kollisionserkennung
  CollideWithWorld(PongWars.Bright);
  CollideWithWorld(PongWars.Dark);
  CollideWithBlocks(PongWars.Bright, true);
  CollideWithBlocks(PongWars.Dark, false);
  If PongWars.JitterDirectionAndSpeed Then Begin
    JitterDirectionAndSpeed(PongWars.Bright);
    JitterDirectionAndSpeed(PongWars.Dark);
  End;
End;

End.

