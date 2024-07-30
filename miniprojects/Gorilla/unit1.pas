(******************************************************************************)
(* Gorilla                                                         ??.??.???? *)
(*                                                                            *)
(* Version     : see ugorilla.pas                                             *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : reimplementation of the orig gorilla.bas from DOS            *)
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
(* History     : 0.01 - 0.03: uncknown                                        *)
(*               0.04: Einfügen richtiger LineEnding's                        *)
(*                     Sperren des 3. Players beim Netzwerkspiel, da sonst    *)
(*                     das gesamte Spiel kaputt geht                          *)
(*               0.05: refactor for publish                                   *)
(*                                                                            *)
(******************************************************************************)
 (*
  Todo:
    - Schwierigkeitsgrad Editierbar(Auswirkungen auf Wind, Rundenweise, Turn Weise) -> Was macht dann die Ki ?
    - Highscore Engine ( Wohl eher nicht .. )

  Offene Bugs:
  - Der Client kommt eigentlich nicht mehr aus einem Verbundenen Netzwerkspiel raus ( Außer durch ESC = Beenden )
  - Verbinden sich mehr als ein Netzwerkspieler, dann geht das nicht ...
  // *)
Unit Unit1;

{$MODE objfpc}{$H+}

{.$DEFINE DebuggMode}// Zeigt evtl aufgetretene OpenGL Fehler an.

Interface

Uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls,
  OpenGlcontext,
  uOpenGL_ASCII_Font, // http://corpsman.de/index.php?doc=opengl/simple_font
  ugorilla,
  math,
  dglOpenGL, // http://wiki.delphigl.com/index.php/dglOpenGL.pas
  lNetComponents, lNet; // http://lnet.wordpress.com/

Type

  { TForm1 }

  TForm1 = Class(TForm)
    LTCPComponent1: TLTCPComponent;
    OpenGLControl1: TOpenGLControl;
    Timer1: TTimer;
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure LTCPComponent1Accept(aSocket: TLSocket);
    Procedure LTCPComponent1CanSend(aSocket: TLSocket);
    Procedure LTCPComponent1Connect(aSocket: TLSocket);
    Procedure LTCPComponent1Disconnect(aSocket: TLSocket);
    Procedure LTCPComponent1Error(Const msg: String; aSocket: TLSocket);
    Procedure LTCPComponent1Receive(aSocket: TLSocket);
    Procedure OpenGLControl1KeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
    Procedure OpenGLControl1Paint(Sender: TObject);
    Procedure OpenGLControl1Resize(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  End;

Var
  Form1: TForm1;
  Initialized: Boolean = false; // Wenn True dann ist OpenGL initialisiert
  Gorilla: tgorilla; // Die Spiel Instanz

Implementation

{$R *.lfm}

{ TForm1 }

Var
  allowcnt: Integer = 0;

Procedure TForm1.OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
Begin
  If allowcnt > 2 Then Begin
    allow := false;
    exit;
  End;
  inc(allowcnt);
  // Sollen Dialoge beim Starten ausgeführt werden ist hier der Richtige Zeitpunkt
  If allowcnt = 1 Then Begin
    // Init dglOpenGL.pas , Teil 2
    ReadExtensions; // Anstatt der Extentions kann auch nur der Core geladen werden. ReadOpenGLCore;
    ReadImplementationProperties;
  End;
  If allowcnt = 2 Then Begin
    // glenable(GL_POINT_SMOOTH); //das würde die Punkte Glätten führt aber nicht zu den Pixelfehlern, welche wohl nur auf nicht Nvidia Graphikkarten auftreten
    Create_ASCII_Font();
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
  go2d(640, 480);
  Gorilla.Render;
  exit2d;
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
    glPointSize(ceil(max(OpenGLControl1.Width / 640, OpenGLControl1.Height / 480)));
  End;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  caption := 'Gorilla ver. ' + floattostrf(GorillaVersion / 100, fffixed, 7, 2) + ' support : www.Corpsman.de';
  Randomize;
  // Init dglOpenGL.pas , Teil 1
  If Not InitOpenGl Then Begin
    showmessage('Error, could not init dglOpenGL.pas');
    Halt;
  End;
  ClientWidth := 640;
  ClientHeight := 480;
  OpenGLControl1.Align := alClient;
  (*
  60 - FPS entsprechen
  0.01666666 ms
  Ist Interval auf 16 hängt das gesamte system, bei 17 nicht.
  Generell sollte die Interval Zahl also dynamisch zum Rechenaufwand, mindestens aber immer 17 sein.
  *)
  Timer1.Interval := 17;
  Network := LTCPComponent1;
  Gorilla := TGorilla.create;
End;

Procedure TForm1.LTCPComponent1Accept(aSocket: TLSocket);
Begin
  Gorilla.OnAccept(aSocket);
End;

Procedure TForm1.LTCPComponent1CanSend(aSocket: TLSocket);
Begin
  // On Can Send ist nur bei der Übertragung Großer Datenmengen notwendig, in Gorilla wird dies nicht benötigt.
End;

Procedure TForm1.LTCPComponent1Connect(aSocket: TLSocket);
Begin
  // On Connect, ist wenn der Client erfolgreich beim Server eingeloggt wurde, dies wird in Gorilla anders erkannt..
End;

Procedure TForm1.LTCPComponent1Disconnect(aSocket: TLSocket);
Begin
  Gorilla.OnDisconnect(aSocket);
End;

Procedure TForm1.LTCPComponent1Error(Const msg: String; aSocket: TLSocket);
Begin
  showmessage(msg);
  Gorilla.OnDisconnect(aSocket)
End;

Procedure TForm1.LTCPComponent1Receive(aSocket: TLSocket);
Var
  b: Byte;
  len: integer;
Begin
  len := aSocket.Get(b, 1);
  If len = 0 Then exit; // Es wurde nichts empfangen
  Gorilla.OnReceive(b, aSocket);
End;

Procedure TForm1.OpenGLControl1KeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  Gorilla.OnKeyDown(Key, Shift);
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  If LTCPComponent1.Connected Then Begin
    LTCPComponent1.Disconnect(true);
  End;
  timer1.Enabled := false;
  Initialized := false;
  Gorilla.Free;
  Gorilla := Nil;
End;

Procedure TForm1.Timer1Timer(Sender: TObject);
{$IFDEF DebuggMode}
Var
  i: Cardinal;
  p: Pchar;
{$ENDIF}
Begin
  If Initialized Then Begin
    OpenGLControl1.OnPaint(Nil);
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

