(******************************************************************************)
(* 2048                                                            ??.??.???? *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Implementation of the 2048 Game                              *)
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

{$DEFINE DebuggMode} // OpenGL Debugg Meldungen Abfangen und anzeigen.

{$DEFINE UseAILib} // Wenn Aktiv, dann kann mittels ai.dll eine KI nachgeladen werden.

Interface

Uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls,
  OpenGlcontext, LCLType, Menus, IniFiles, math,
  dglOpenGL // http://wiki.delphigl.com/index.php/dglOpenGL.pas
  , u2048
{$IFDEF UseAILib}
  , dynlibs
{$ENDIF}
  ;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    ApplicationProperties1: TApplicationProperties;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    OpenDialog1: TOpenDialog;
    OpenGLControl1: TOpenGLControl;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    Timer1: TTimer;
    Timer2: TTimer;
    Procedure ApplicationProperties1Idle(Sender: TObject; Var Done: Boolean);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure MenuItem1Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure OpenGLControl1KeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure OpenGLControl1KeyUp(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
    Procedure OpenGLControl1Paint(Sender: TObject);
    Procedure OpenGLControl1Resize(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
    Procedure Timer2Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Procedure Go2d();
    Procedure Exit2d();
  End;

  TField = Array Of Array Of int64;

  TAiMove = Function(data: TField; Boardsize: integer): integer; cdecl;
  TInfo = Function(): PChar; cdecl;

Var
  Form1: TForm1;
  Initialized: Boolean = false; // Wenn True dann ist OpenGL initialisiert
  Board: TBoard;
  maxp: int64 = 0;
  KeyDebounce: Boolean = True;
  KeyPressedDown: Boolean = false;
  blocked: Boolean = false;
  AiMove: TAiMove = Nil;
  info: TInfo = Nil;
  lib: TLibHandle = 0;
  finMessage: boolean;

Implementation

{$R *.lfm}

Uses LazUTF8;

Var
  ini: Tinifile;

  { TForm1 }

Procedure Tform1.Go2d();
Begin
  glMatrixMode(GL_PROJECTION);
  glPushMatrix(); // Store The Projection Matrix
  glLoadIdentity(); // Reset The Projection Matrix
  glOrtho(0, OpenGLControl1.Width, OpenGLControl1.height, 0, -1, 1); // Set Up An Ortho Screen
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix(); // Store old Modelview Matrix
  glLoadIdentity(); // Reset The Modelview Matrix
End;

Procedure Tform1.Exit2d();
Begin
  glMatrixMode(GL_PROJECTION);
  glPopMatrix(); // Restore old Projection Matrix
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix(); // Restore old Projection Matrix
End;

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
    glEnable(GL_DEPTH_TEST); // Tiefentest
    glDepthFunc(gl_less);
    // Der Anwendung erlauben zu Rendern.
    Initialized := True;
    If Not fileexists(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStrUTF8(0))) + 'font.ofnt') Then Begin
      showmessage('Error could not find : ' + IncludeTrailingPathDelimiter(ExtractFilePath(ParamStrUTF8(0))) + 'font.ofnt');
      halt;
    End;
    CreateFont(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStrUTF8(0))) + 'font.ofnt');
    Board := TBoard.Create;
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

  go2d;
  OpenGlFont.Size := 40;
  OpenGlFont.Color := clWhite;
  maxp := max(maxp, board.Points);
  OpenGlFont.Textout(10, 10, format('Points : %d    max Points : %d', [board.Points, maxp]));
  glTranslatef(0, 100, 0);
  board.Render;
  exit2d;
  OpenGLControl1.SwapBuffers;

  // Game Over ?
  If board.IsFull And Not (blocked) Then Begin
    timer2.enabled := false;
    blocked := true;
    finMessage := true;
  End;
End;

Procedure TForm1.OpenGLControl1Resize(Sender: TObject);
Begin
  If Initialized Then Begin
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glViewport(0, 0, OpenGLControl1.Width, OpenGLControl1.Height);
    gluPerspective(45.0, OpenGLControl1.Width / OpenGLControl1.Height, 0.1, 100.0);
    glMatrixMode(GL_MODELVIEW);
    board.Resize(OpenGLControl1.Width, OpenGLControl1.Height - 100);
  End;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  finMessage := false;
  // Init dglOpenGL.pas , Teil 1
  If Not InitOpenGl Then Begin
    showmessage('Error, could not init dglOpenGL.pas');
    Halt;
  End;
  (*
  60 - FPS entsprechen
  0.01666666 ms
  Ist Interval auf 16 hängt das gesamte system, bei 17 nicht.
  Generell sollte die Interval Zahl also dynamisch zum Rechenaufwand, mindestens aber immer 17 sein.
  *)
  Timer1.Interval := 17;
  OpenGLControl1.Align := alClient;
  Randomize;
  (*
   * Historie : 0.01 = Initial Version
   *            0.02 = Improvements in End-Game detection, added "ai-player", editable falltime
   *            0.03 = Einfügen "u" option
   *)
  caption := '2048 ver.: 0.02 remake by Corpsman, www.Corpsman.de';
  Constraints.MinHeight := 280;
  Constraints.MinWidth := 180;
  ini := TIniFile.Create(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStrUTF8(0))) + 'Settings.ini');
  BoardSize := ini.ReadInteger('General', 'Boardsize', BoardSize + 1) - 1;
  FallTime := ini.ReadInteger('General', 'FallTime', FallTime);

  maxp := StrToInt64(ini.ReadString(format('Board%dx%d', [BoardSize + 1, BoardSize + 1]), 'MaxPoints', '0'));
  OpenDialog1.InitialDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStrUTF8(0)));
  SaveDialog1.InitialDir := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStrUTF8(0)));
  PieceColors[0].BackColor := ini.ReadInteger('Color', 'b', PieceColors[0].BackColor);
  //PieceColors[0].FontColor := ini.ReadInteger('Color', 'f', PieceColors[0].FontColor); // Macht keinen Sinn
  PieceColors[1].BackColor := ini.ReadInteger('Color', '2b', PieceColors[1].BackColor);
  PieceColors[1].FontColor := ini.ReadInteger('Color', '2f', PieceColors[1].FontColor);
  PieceColors[2].BackColor := ini.ReadInteger('Color', '4b', PieceColors[2].BackColor);
  PieceColors[2].FontColor := ini.ReadInteger('Color', '4f', PieceColors[2].FontColor);
  PieceColors[3].BackColor := ini.ReadInteger('Color', '8b', PieceColors[3].BackColor);
  PieceColors[3].FontColor := ini.ReadInteger('Color', '8f', PieceColors[3].FontColor);
  PieceColors[4].BackColor := ini.ReadInteger('Color', '16b', PieceColors[4].BackColor);
  PieceColors[4].FontColor := ini.ReadInteger('Color', '16f', PieceColors[4].FontColor);
  PieceColors[5].BackColor := ini.ReadInteger('Color', '32b', PieceColors[5].BackColor);
  PieceColors[5].FontColor := ini.ReadInteger('Color', '32f', PieceColors[5].FontColor);
  PieceColors[6].BackColor := ini.ReadInteger('Color', '64b', PieceColors[6].BackColor);
  PieceColors[6].FontColor := ini.ReadInteger('Color', '64f', PieceColors[6].FontColor);
  PieceColors[7].BackColor := ini.ReadInteger('Color', '128b', PieceColors[7].BackColor);
  PieceColors[7].FontColor := ini.ReadInteger('Color', '128f', PieceColors[7].FontColor);
  PieceColors[8].BackColor := ini.ReadInteger('Color', '256b', PieceColors[8].BackColor);
  PieceColors[8].FontColor := ini.ReadInteger('Color', '256f', PieceColors[8].FontColor);
  PieceColors[9].BackColor := ini.ReadInteger('Color', '512b', PieceColors[9].BackColor);
  PieceColors[9].FontColor := ini.ReadInteger('Color', '512f', PieceColors[9].FontColor);
  PieceColors[10].BackColor := ini.ReadInteger('Color', '1024b', PieceColors[10].BackColor);
  PieceColors[10].FontColor := ini.ReadInteger('Color', '1024f', PieceColors[10].FontColor);
  PieceColors[11].BackColor := ini.ReadInteger('Color', '2048b', PieceColors[11].BackColor);
  PieceColors[11].FontColor := ini.ReadInteger('Color', '2048f', PieceColors[11].FontColor);
  PieceColors[12].BackColor := ini.ReadInteger('Color', '4096b', PieceColors[12].BackColor);
  PieceColors[12].FontColor := ini.ReadInteger('Color', '4096f', PieceColors[12].FontColor);
  KeyDebounce := ini.ReadBool('General', 'KeyDebounce', KeyDebounce);
  OpenGLControl1.Invalidate;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  Initialized := false;
  Board.Free;
  ini.WriteString(format('Board%dx%d', [BoardSize + 1, BoardSize + 1]), 'MaxPoints', inttostr(maxp));
  ini.free;
  If lib <> 0 Then Begin
    AiMove := Nil;
    UnloadLibrary(lib);
    lib := 0;
  End;
End;

Procedure TForm1.ApplicationProperties1Idle(Sender: TObject; Var Done: Boolean);
Begin
  If finMessage Then Begin
    finMessage := false;
    If ID_YES = Application.MessageBox('Game over, new game?', 'Question', MB_YESNO Or MB_ICONQUESTION) Then Begin
      board.restart;
      blocked := false;
    End
    Else Begin
      // close;
    End;
  End;
End;

Procedure TForm1.MenuItem1Click(Sender: TObject);
Var
  f: TFileStream;
Begin
  // Load Board
  If OpenDialog1.Execute Then Begin
    // Alte Board Max Sichern
    ini.WriteString(format('Board%dx%d', [BoardSize + 1, BoardSize + 1]), 'MaxPoints', inttostr(maxp));
    // Board Laden
    f := TFileStream.Create(OpenDialog1.FileName, fmopenread);
    Board.LoadFromStream(f);
    // Neue Board Max Laden
    maxp := StrToInt64(ini.ReadString(format('Board%dx%d', [BoardSize + 1, BoardSize + 1]), 'MaxPoints', '0'));
    f.free;
  End;
End;

Procedure TForm1.MenuItem2Click(Sender: TObject);
Var
  f: TFileStream;
Begin
  // Save Board
  If SaveDialog1.Execute Then Begin
    f := TFileStream.Create(SaveDialog1.FileName, fmcreate Or fmopenwrite);
    Board.SaveToStream(f);
    f.free;
  End;
End;

Procedure TForm1.OpenGLControl1KeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Var
  s: String;
Begin
  If key = VK_ESCAPE Then close;
  If key = vk_F12 Then close; // Boss Key
  If key = vk_A Then Begin
{$IFDEF UseAILib}
    If lib = 0 Then Begin
      s := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStrUTF8(0))) + {$IFDEF windows} 'ai.dll'{$ELSE} 'libai.so'{$ENDIF};
      lib := LoadLibrary(s);
      If lib = 0 Then Begin
        showmessage('Error unable to load : ' + LineEnding + LineEnding + s);
        exit;
      End;
      AiMove := TAiMove(GetProcAddress(lib, 'AiMove'));
      If Not assigned(aimove) Then Begin
        showmessage('Error could not load "AiMove" in ' + LineEnding + LineEnding + s);
        UnloadLibrary(lib);
        exit;
      End;
      info := TInfo(GetProcAddress(lib, 'Info'));
      If Not assigned(info) Then Begin
        showmessage('Error could not load "Info" in ' + LineEnding + LineEnding + s);
        UnloadLibrary(lib);
        exit;
      End;
    End;
    timer2.Enabled := Not timer2.Enabled;
{$ENDIF}
  End;
  If key = VK_u Then Begin
{$IFDEF UseAILib}
    If lib <> 0 Then Begin
      UnloadLibrary(lib);
      lib := 0;
      info := Nil;
      aimove := Nil;
      showmessage('Unload ai, sucessfully done.');
    End;
{$ENDIF}
  End;
  If key = vk_i Then Begin
{$IFDEF UseAILib}
    If lib = 0 Then Begin
      s := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStrUTF8(0))) + {$IFDEF Windows} 'ai.dll'{$ELSE} 'libai.so'{$ENDIF};
      lib := LoadLibrary(s);
      If lib = 0 Then Begin
        showmessage('Error unable to load : ' + LineEnding + LineEnding + s);
        exit;
      End;
      AiMove := TAiMove(GetProcAddress(lib, 'AiMove'));
      If Not assigned(aimove) Then Begin
        showmessage('Error could not load "AiMove" in ' + LineEnding + LineEnding + s);
        UnloadLibrary(lib);
        exit;
      End;
      info := TInfo(GetProcAddress(lib, 'Info'));
      If Not assigned(info) Then Begin
        showmessage('Error could not load "Info" in ' + LineEnding + LineEnding + s);
        UnloadLibrary(lib);
        exit;
      End;
    End;
    showmessage(info());
{$ENDIF}
  End;

  If key = vk_n Then Begin
    board.restart;
    blocked := false;
  End;
  If KeyDebounce And KeyPressedDown Then exit;
  If Key = vk_down Then Begin
    Board.StartFalling(dDown);
  End;
  If Key = vk_Up Then Begin
    Board.StartFalling(dup);
  End;
  If Key = vk_left Then Begin
    Board.StartFalling(dleft);
  End;
  If Key = vk_right Then Begin
    Board.StartFalling(dright);
  End;
  KeyPressedDown := True;
End;

Procedure TForm1.OpenGLControl1KeyUp(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  KeyPressedDown := false;
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
      showmessage('OpenGL Error (' + inttostr(i) + ') occured.' + #13#13 +
        'OpenGL Message : "' + p + '"'#13#13 +
        'Applikation will be terminated.');
      close;
    End;
{$ENDIF}
  End;
End;

Var
  ai_Retries: integer = 0;

Procedure TForm1.Timer2Timer(Sender: TObject);
Var
  b: Boolean;
  key: Word;
  f: TField;
  i: Integer;
  j: Integer;
Begin
  If Not assigned(AiMove) Then Begin
    timer2.enabled := false;
    showmessage('Error, no ai callback found.');
    exit;
  End;
  b := Not Board.IsFalling;
  If b Then Begin
    KeyPressedDown := false;
    setlength(f, BoardSize + 1, BoardSize + 1);
    For i := 0 To BoardSize Do
      For j := 0 To BoardSize Do
        f[i, j] := Board.index(i, j);
    key := AiMove(f, BoardSize + 1);
    If key <> 0 Then
      OpenGLControl1KeyDown(Nil, Key, []);
    If Not Board.IsFalling Then Begin
      inc(ai_Retries);
      // Wenn 60 mal nichts ging (ca. 3s Zeit), dann wird die Ki, zwangs abgebrochen
      If (ai_Retries > 60) Then Begin
        timer2.Enabled := false;
        showmessage('Ai gave 60 times an invalid command, ai will now be deactivated.');
      End;
    End
    Else Begin
      ai_Retries := 0;
    End;
    If key = 0 Then Begin
      timer2.Enabled := false;
      showmessage('Ai gave up.');
    End;
  End;
End;

End.

