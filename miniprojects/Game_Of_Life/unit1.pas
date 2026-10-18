(******************************************************************************)
(* GOL_Shader                                                      15.10.2026 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Game of life implemented in shaders                          *)
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
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, ComCtrls,
  OpenGlcontext
  (*
   * Kommt ein Linkerfehler wegen OpenGL dann: sudo apt-get install freeglut3-dev
   *)
  , dglOpenGL // http://wiki.delphigl.com/index.php/dglOpenGL.pas
  ;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    OpenDialog1: TOpenDialog;
    OpenGLControl1: TOpenGLControl;
    Timer1: TTimer;
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure OpenGLControl1KeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
    Procedure OpenGLControl1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure OpenGLControl1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    Procedure OpenGLControl1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    Procedure OpenGLControl1Paint(Sender: TObject);
    Procedure OpenGLControl1Resize(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private
    FTextures: Array[0..1] Of GLuint;
    FFramebuffers: Array[0..1] Of GLuint;
    FTextureWidth, FTextureHeight: Integer;
    FSimulationWidth, FSimulationHeight: Integer;
    FReadIndex: Integer;
    FFirstFrame: Boolean;
    FMouseDown: Boolean;
    FMouseX, FMouseY: Integer;
    FLastMouseX, FLastMouseY: Integer;
    FDisplayProgram: GLuint;
    ShaderProgram: GLuint;
    VAO: GLuint; // Vertex Array Object
    VBO: GLuint; // Vertex Buffer Object
    Procedure ResizeTextures;
    Procedure SetupFrame;
    Procedure DrawQuad(AProgram: GLuint);
    Procedure GetDisplayViewport(Out X, Y, W, H: Integer);
    Function WindowToCell(X, Y: Integer; Out CellX, CellY: Integer): Boolean;

  public
    { public declarations }
    Procedure Go2d();
    Procedure Exit2d();
  End;

Var
  Form1: TForm1;
  Initialized: Boolean = false; // Wenn True dann ist OpenGL initialisiert

Implementation

{$R *.lfm}

Uses LCLType, IntfGraphics, FPImage;

Const
  (*
   * Convert:  (0,0) .. (width,height) [Worldspace]
   *         to
   *           (-1,-1) .. (1,1) [Clipspace]
   *)
  VertexShader: PChar =
  '#version 330 core'#10 +
    'layout(location = 0) in vec2 aPos;'#10 +
    'uniform vec2 uNormRes;'#10 +
    'void main()'#10 +
    '{'#10 +
    '  // Convert from [0..width / height] to [0..1]'#10 +
    '  vec2 fragCoord = aPos * uNormRes;'#10 +
    '  // Convert from [0..1] to [-1..1]'#10 +
    '  vec2 ndc = fragCoord * 2.0 - 1.0;'#10 +
    '  gl_Position = vec4(ndc, 0.0, 1.0);'#10 +
    '}'
    ;

  GOLFragmentShader: PChar =
  '#version 330 core'#10 +
    'uniform sampler2D uTexture;'#10 +
    'uniform bool uInitialize;'#10 +
    'uniform bool uMouseDown;'#10 +
    'uniform vec2 uMouseFrom;'#10 +
    'uniform vec2 uMouseTo;'#10 +
    'out vec4 FragColor;'#10 +
    'void main()'#10 +
    '{'#10 +
    '  // actual position as integer vector'#10 +
    '  ivec2 cell = ivec2(gl_FragCoord.xy);'#10 +
    '  if (uInitialize) {'#10 +
    '    FragColor = vec4(0.0, 0.0, 0.0, 1.0);'#10 +
    '  } else {'#10 +
    '    // Read out current position and store'#10 +
    '    // in num = counts the number of alive neighbours'#10 +
    '    vec4 cur = texelFetch(uTexture, cell, 0);'#10 +
    '    float cellv = cur.r; // bakup atuall cell value'#10 +
    '    float num = 0.0;'#10 +
    '    for (int i = -1; i <= 1; i++) {'#10 +
    '      for (int j = -1; j <= 1; j++) {'#10 +
    '        ivec2 v = cell + ivec2(i, j);'#10 +
    '        if (all(greaterThanEqual(v, ivec2(0))) && all(lessThan(v, textureSize(uTexture, 0)))) {'#10 +
    '          num += texelFetch(uTexture, v, 0).r;'#10 +
    '        }'#10 +
    '      }'#10 +
    '    }'#10 +
    '    num -= cellv; // above loop "overshots" by also counting i,j = 0'#10 +
    '    // Apply the rules of GOL'#10 +
    '    if (cellv > 0.5) {'#10 +
    '      if (num < 1.5) {'#10 +
    '        cellv = 0.0;'#10 +
    '      }'#10 +
    '      if (num > 3.5) {'#10 +
    '        cellv = 0.0;'#10 +
    '      }'#10 +
    '    } else {'#10 +
    '      if (num > 2.5 && num < 3.5) {'#10 +
    '        cellv = 1.0;'#10 +
    '      }'#10 +
    '    }'#10 +
    '    // Write back cellv to the texture'#10 +
    '    FragColor = vec4(cellv, cellv, cellv, 1.0);'#10 +
    '  }'#10 +
    '  // Enable Mouse "drawing" by Alive all line cells on segment'#10 +
    '  // between uMouseTo and uMouseFrom'#10 +
    '  if (uMouseDown) {'#10 +
    '    vec2 segment = uMouseTo - uMouseFrom;'#10 +
    '    float segmentLengthSquared = dot(segment, segment);'#10 +
    '    vec2 pixel = gl_FragCoord.xy - vec2(0.5);'#10 +
    '    float t = 0.0;'#10 +
    '    if (segmentLengthSquared > 0.0) {'#10 +
    '      t = clamp(dot(pixel - uMouseFrom, segment) / segmentLengthSquared, 0.0, 1.0);'#10 +
    '    }'#10 +
    '    vec2 closestPoint = uMouseFrom + t * segment;'#10 +
    '    if (distance(pixel, closestPoint) <= 0.707) { // 0.707 = 1 / sqrt(2)'#10 +
    '    FragColor = vec4(1.0);'#10 +
    '    }'#10 +
    '  }'#10 +
    '}'
    ;

  DisplayFragmentShader: PChar =
  '#version 330 core'#10 +
    'uniform sampler2D uTexture;'#10 +
    'uniform vec2 uDisplayOrigin;'#10 +
    'uniform vec2 uDisplaySize;'#10 +
    'out vec4 FragColor;'#10 +
    'void main()'#10 +
    '{'#10 +
    '  vec2 uv = (gl_FragCoord.xy - uDisplayOrigin) / uDisplaySize;'#10 +
    '  ivec2 cell = ivec2(uv * vec2(textureSize(uTexture, 0)));'#10 +
    '  FragColor = texelFetch(uTexture, cell, 0);'#10 +
    '}';

  { TForm1 }

Procedure Tform1.Go2d();
Var
  LocTex: GLint;
Begin
  If ShaderProgram <> 0 Then Begin
    glUseProgram(ShaderProgram);
    glActiveTexture(GL_TEXTURE0);
    glBindTexture(GL_TEXTURE_2D, FTextures[FReadIndex]);
    LocTex := glGetUniformLocation(ShaderProgram, 'uTexture');
    glUniform1i(LocTex, 0);
    glUniform1i(glGetUniformLocation(ShaderProgram, 'uInitialize'), Ord(FFirstFrame));
    glUniform1i(glGetUniformLocation(ShaderProgram, 'uMouseDown'), Ord(FMouseDown));
    glUniform2f(glGetUniformLocation(ShaderProgram, 'uMouseFrom'), FLastMouseX,
      FTextureHeight - 1 - FLastMouseY);
    glUniform2f(glGetUniformLocation(ShaderProgram, 'uMouseTo'), FMouseX,
      FTextureHeight - 1 - FMouseY);
  End;
  glBindVertexArray(VAO);
End;

Procedure Tform1.Exit2d();
Begin
  glBindVertexArray(0);
  glUseProgram(0);
End;

Function CompileShader(Src: PChar; ShaderType: GLenum): GLuint;
Var
  S: GLuint;
  status: GLint;
  Log: Array[0..1023] Of char;
Begin
  result := 0;
  S := glCreateShader(ShaderType);
  glShaderSource(S, 1, @Src, Nil);
  glCompileShader(S);

  glGetShaderiv(S, GL_COMPILE_STATUS, @status);
  If status = 0 Then Begin
    glGetShaderInfoLog(S, 1024, Nil, @Log);
    glDeleteShader(S);
    Raise Exception.Create('Shader compilation error: ' + StrPas(@Log[0]));
  End;

  Result := S;
End;

Function CreateShaderProgram(VSrc, FSrc: PChar): GLuint;
Var
  vs, fs: GLuint;
  prog: GLuint;
  status: GLint;
  Log: Array[0..1023] Of char;
Begin
  Result := 0;
  vs := CompileShader(VSrc, GL_VERTEX_SHADER);
  If vs = 0 Then
    Exit;
  Try
    fs := CompileShader(FSrc, GL_FRAGMENT_SHADER);
  Except
    glDeleteShader(vs);
    Raise;
  End;
  If fs = 0 Then Begin
    glDeleteShader(vs);
    Exit;
  End;

  prog := glCreateProgram();
  glAttachShader(prog, vs);
  glAttachShader(prog, fs);
  glLinkProgram(prog);

  glGetProgramiv(prog, GL_LINK_STATUS, @status);
  If status = 0 Then Begin
    glGetProgramInfoLog(prog, 1024, Nil, @Log);
    glDeleteProgram(prog);
    glDeleteShader(vs);
    glDeleteShader(fs);
    Raise Exception.Create('Shader link error: ' + StrPas(@Log[0]));
  End;
  glDeleteShader(vs);
  glDeleteShader(fs);
  Result := prog;
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
    If Not Assigned(glCreateShader) Then Begin
      // On Windows it seems that you need to "reload" the core functions for proper function
      ReadExtensions;
      ReadImplementationProperties;
      // if still not available, then halt
      If Not Assigned(glCreateShader) Then Begin
        showmessage('glCreateShader not available, use legacy mode..');
        halt;
      End;
    End;
    ShaderProgram := CreateShaderProgram(VertexShader, GOLFragmentShader);
    FDisplayProgram := CreateShaderProgram(VertexShader, DisplayFragmentShader);
    glGenVertexArrays(1, @VAO);
    glGenBuffers(1, @VBO);

    // Der Anwendung erlauben zu Rendern.
    Initialized := True;
    FFirstFrame := True;
  End;
  Form1.Invalidate;
End;

Procedure TForm1.ResizeTextures;
Var
  NewTextures, NewFramebuffers: Array[0..1] Of GLuint;
  W, H, CopyWidth, CopyHeight, MaxSize, i: Integer;
  OldReadFramebuffer, OldDrawFramebuffer: GLint;
  Status: GLenum;
Begin
  W := OpenGLControl1.Width;
  H := OpenGLControl1.Height;
  If FSimulationWidth > 0 Then Begin
    W := FSimulationWidth;
    H := FSimulationHeight;
  End;
  If (W = FTextureWidth) And (H = FTextureHeight) Then Exit;
  glGetIntegerv(GL_MAX_TEXTURE_SIZE, @MaxSize);
  If (W > MaxSize) Or (H > MaxSize) Then
    Raise Exception.CreateFmt('Render texture %dx%d exceeds GL_MAX_TEXTURE_SIZE (%d)',
      [W, H, MaxSize]);

  FillChar(NewTextures, SizeOf(NewTextures), 0);
  FillChar(NewFramebuffers, SizeOf(NewFramebuffers), 0);
  glGetIntegerv(GL_READ_FRAMEBUFFER_BINDING, @OldReadFramebuffer);
  glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, @OldDrawFramebuffer);
  glGenTextures(2, @NewTextures[0]);
  glGenFramebuffers(2, @NewFramebuffers[0]);
  Try
    glActiveTexture(GL_TEXTURE0);
    glDisable(GL_SCISSOR_TEST);
    glClearColor(0, 0, 0, 1);
    For i := 0 To 1 Do Begin
      glBindTexture(GL_TEXTURE_2D, NewTextures[i]);
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA8, W, H, 0,
        GL_RGBA, GL_UNSIGNED_BYTE, Nil);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
      glBindFramebuffer(GL_FRAMEBUFFER, NewFramebuffers[i]);
      glFramebufferTexture2D(GL_FRAMEBUFFER, GL_COLOR_ATTACHMENT0,
        GL_TEXTURE_2D, NewTextures[i], 0);
      Status := glCheckFramebufferStatus(GL_FRAMEBUFFER);
      If Status <> GL_FRAMEBUFFER_COMPLETE Then
        Raise Exception.CreateFmt('Incomplete framebuffer: 0x%s', [IntToHex(Status, 4)]);
      glClear(GL_COLOR_BUFFER_BIT);

      If FFramebuffers[FReadIndex] <> 0 Then Begin
        CopyWidth := FTextureWidth;
        If W < CopyWidth Then CopyWidth := W;
        CopyHeight := FTextureHeight;
        If H < CopyHeight Then CopyHeight := H;
        // Gleiche Rechteckgroesse: Pixel erhalten, nicht skalieren.
        glBindFramebuffer(GL_READ_FRAMEBUFFER, FFramebuffers[FReadIndex]);
        glBlitFramebuffer(0, 0, CopyWidth, CopyHeight,
          0, 0, CopyWidth, CopyHeight, GL_COLOR_BUFFER_BIT, GL_NEAREST);
      End;
    End;
    glDeleteFramebuffers(2, @FFramebuffers[0]);
    glDeleteTextures(2, @FTextures[0]);
    FTextures := NewTextures;
    FFramebuffers := NewFramebuffers;
    FillChar(NewTextures, SizeOf(NewTextures), 0);
    FillChar(NewFramebuffers, SizeOf(NewFramebuffers), 0);
    FTextureWidth := W;
    FTextureHeight := H;
  Finally
    glBindFramebuffer(GL_READ_FRAMEBUFFER, OldReadFramebuffer);
    glBindFramebuffer(GL_DRAW_FRAMEBUFFER, OldDrawFramebuffer);
    glBindTexture(GL_TEXTURE_2D, 0);
    glDeleteFramebuffers(2, @NewFramebuffers[0]);
    glDeleteTextures(2, @NewTextures[0]);
  End;
End;

Procedure TForm1.DrawQuad(AProgram: GLuint);
Var
  vertices: Array[0..7] Of GLfloat;
  LocRes: GLint;
Begin
  glUseProgram(AProgram);
  LocRes := glGetUniformLocation(AProgram, 'uNormRes');
  glUniform2f(LocRes, 1 / FTextureWidth, 1 / FTextureHeight);
  glBindVertexArray(VAO);
  vertices[0] := 0;
  vertices[1] := 0; // bottom-left

  vertices[2] := FTextureWidth;
  vertices[3] := 0; // bottom-right

  vertices[4] := 0;
  vertices[5] := FTextureHeight; // top-left

  vertices[6] := FTextureWidth;
  vertices[7] := FTextureHeight; // top-right

  glBindBuffer(GL_ARRAY_BUFFER, VBO);
  glBufferData(GL_ARRAY_BUFFER, SizeOf(vertices), @vertices[0], GL_DYNAMIC_DRAW);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 2 * SizeOf(GLfloat), Nil);

  glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
End;

Procedure TForm1.SetupFrame;
Begin
  glViewport(0, 0, FTextureWidth, FTextureHeight);
  glDisable(GL_DEPTH_TEST);
  glDisable(GL_BLEND);
  glDisable(GL_SCISSOR_TEST);
End;

Procedure TForm1.GetDisplayViewport(Out X, Y, W, H: Integer);
Begin
  W := OpenGLControl1.Width;
  H := OpenGLControl1.Height;
  If Int64(W) * FTextureHeight > Int64(H) * FTextureWidth Then
    W := Int64(H) * FTextureWidth Div FTextureHeight
  Else
    H := Int64(W) * FTextureHeight Div FTextureWidth;
  If W < 1 Then W := 1;
  If H < 1 Then H := 1;
  X := (OpenGLControl1.Width - W) Div 2;
  Y := (OpenGLControl1.Height - H) Div 2;
End;

Function TForm1.WindowToCell(X, Y: Integer; Out CellX, CellY: Integer): Boolean;
Var
  ViewX, ViewY, ViewWidth, ViewHeight: Integer;
Begin
  Result := False;
  If (FTextureWidth <= 0) Or (FTextureHeight <= 0) Then Exit;
  GetDisplayViewport(ViewX, ViewY, ViewWidth, ViewHeight);
  Y := OpenGLControl1.Height - 1 - Y;
  If (X < ViewX) Or (X >= ViewX + ViewWidth) Or
    (Y < ViewY) Or (Y >= ViewY + ViewHeight) Then Exit;
  CellX := (2 * Int64(X - ViewX) + 1) * FTextureWidth Div (2 * Int64(ViewWidth));
  CellY := FTextureHeight - 1 -
    (2 * Int64(Y - ViewY) + 1) * FTextureHeight Div (2 * Int64(ViewHeight));
  Result := True;
End;

Procedure TForm1.OpenGLControl1Paint(Sender: TObject);
Var
  WriteIndex: Integer;
  WindowFramebuffer: GLint;
  ViewX, ViewY, ViewWidth, ViewHeight: Integer;
Begin
  If Not Initialized Then Exit;
  If (OpenGLControl1.Width <= 0) Or (OpenGLControl1.Height <= 0) Then Exit;
  If Not OpenGLControl1.MakeCurrent Then
    Raise Exception.Create('Could not activate OpenGL context');
  // GTK3 rendert das Fenster ebenfalls in ein FBO, nicht zwingend in FBO 0.
  glGetIntegerv(GL_DRAW_FRAMEBUFFER_BINDING, @WindowFramebuffer);
  ResizeTextures;
  SetupFrame;
  WriteIndex := 1 - FReadIndex;

  glBindFramebuffer(GL_FRAMEBUFFER, FFramebuffers[WriteIndex]);
  Go2d;
  DrawQuad(ShaderProgram);
  FLastMouseX := FMouseX;
  FLastMouseY := FMouseY;

  glBindFramebuffer(GL_FRAMEBUFFER, WindowFramebuffer);
  glClearColor(0, 0, 0, 1);
  glClear(GL_COLOR_BUFFER_BIT);
  GetDisplayViewport(ViewX, ViewY, ViewWidth, ViewHeight);
  glViewport(ViewX, ViewY, ViewWidth, ViewHeight);
  glUseProgram(FDisplayProgram);
  glBindTexture(GL_TEXTURE_2D, FTextures[WriteIndex]);
  glUniform1i(glGetUniformLocation(FDisplayProgram, 'uTexture'), 0);
  glUniform2f(glGetUniformLocation(FDisplayProgram, 'uDisplayOrigin'), ViewX, ViewY);
  glUniform2f(glGetUniformLocation(FDisplayProgram, 'uDisplaySize'), ViewWidth, ViewHeight);
  DrawQuad(FDisplayProgram);
  FReadIndex := WriteIndex;
  FFirstFrame := False;

  glBindFramebuffer(GL_FRAMEBUFFER, WindowFramebuffer);
  glBindTexture(GL_TEXTURE_2D, 0);
  Exit2d;

  OpenGLControl1.SwapBuffers;
End;

Procedure TForm1.OpenGLControl1Resize(Sender: TObject);
Begin
  // Texturen erst beim naechsten Paint mit aktuellem Kontext anpassen.
  OpenGLControl1.Invalidate;
End;

Procedure TForm1.OpenGLControl1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
Begin
  If Not WindowToCell(X, Y, FMouseX, FMouseY) Then Begin
    FMouseDown := False;
    Exit;
  End;
  If FMouseDown Then
    OpenGLControl1.Invalidate;
End;

Procedure TForm1.OpenGLControl1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
  Case Button Of
    mbRight: Begin
        FFirstFrame := true;
        OpenGLControl1.Invalidate;
      End;
    mbLeft: Begin
      If Not WindowToCell(X, Y, FMouseX, FMouseY) Then Exit;
        FMouseDown := True;
      FLastMouseX := FMouseX;
      FLastMouseY := FMouseY;
        OpenGLControl1.Invalidate;
      End;
  End;
End;

Procedure TForm1.OpenGLControl1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
  If Button <> mbLeft Then Exit;
  FMouseDown := False;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  caption := 'Shader GOL ver.: 0.01 by Corpsman, www.Corpsman.de';
  Application.Title := caption;

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
  OpenGLControl1.AutoResizeViewport := True; // This is crucial for GTK3, don't know why, but without it the demo does not work
  OpenGLControl1.Align := alClient;
  VAO := 0;
  VBO := 0;
  Timer1.Interval := 17;
End;

Procedure TForm1.FormDestroy(Sender: TObject);
Begin
  Timer1.Enabled := False;
  If (Initialized Or (ShaderProgram <> 0) Or (FDisplayProgram <> 0))
    And OpenGLControl1.MakeCurrent Then Begin
    If ShaderProgram <> 0 Then
      glDeleteProgram(ShaderProgram);
    If FDisplayProgram <> 0 Then
      glDeleteProgram(FDisplayProgram);
    glDeleteFramebuffers(2, @FFramebuffers[0]);
    glDeleteTextures(2, @FTextures[0]);
    If VAO <> 0 Then
      glDeleteVertexArrays(1, @VAO);
    If VBO <> 0 Then
      glDeleteBuffers(1, @VBO);
  End;
  Initialized := False;
End;

Procedure TForm1.OpenGLControl1KeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Var
  Bitmap: TBitmap;
  Image: TLazIntfImage;
  Pixels: Array Of Byte;
  PixelColor: TFPColor;
  X, Y, TextureIndex, MaxSize: Integer;
  PixelIndex: SizeInt;
  CellValue: Byte;
  TimerEnabled: Boolean;
Begin
  If (ssCtrl In Shift) And (key = VK_L) Then Begin
    Key := 0;
    If Not Initialized Then Exit;
    TimerEnabled := Timer1.Enabled;
    Timer1.Enabled := False;
    Try
      If Not OpenDialog1.Execute Then Exit;
      Bitmap := TBitmap.Create;
      Try
        Bitmap.LoadFromFile(OpenDialog1.Filename);
        If (Bitmap.Width <= 0) Or (Bitmap.Height <= 0) Then
          Raise Exception.Create('Bitmap is empty');
        If Not OpenGLControl1.MakeCurrent Then
          Raise Exception.Create('Could not activate OpenGL context');
        glGetIntegerv(GL_MAX_TEXTURE_SIZE, @MaxSize);
        If (Bitmap.Width > MaxSize) Or (Bitmap.Height > MaxSize) Then
          Raise Exception.CreateFmt('Bitmap %dx%d exceeds GL_MAX_TEXTURE_SIZE (%d)',
            [Bitmap.Width, Bitmap.Height, MaxSize]);
        SetLength(Pixels, SizeInt(Bitmap.Width) * Bitmap.Height * 4);
        Image := Bitmap.CreateIntfImage;
        Try
          For Y := 0 To Bitmap.Height - 1 Do Begin
            For X := 0 To Bitmap.Width - 1 Do Begin
              PixelColor := Image.Colors[X, Y];
              CellValue := 0;
              If (PixelColor.Red = 0) And (PixelColor.Green = 0) And
                (PixelColor.Blue = 0) Then CellValue := 255;
              PixelIndex := (SizeInt(Bitmap.Height - 1 - Y) * Bitmap.Width + X) * 4;
              Pixels[PixelIndex] := CellValue;
              Pixels[PixelIndex + 1] := CellValue;
              Pixels[PixelIndex + 2] := CellValue;
              Pixels[PixelIndex + 3] := 255;
            End;
          End;
        Finally
          Image.Free;
        End;
        FSimulationWidth := Bitmap.Width;
        FSimulationHeight := Bitmap.Height;
        ResizeTextures;
        For TextureIndex := 0 To 1 Do Begin
          glBindTexture(GL_TEXTURE_2D, FTextures[TextureIndex]);
          glTexSubImage2D(GL_TEXTURE_2D, 0, 0, 0, FTextureWidth, FTextureHeight,
            GL_RGBA, GL_UNSIGNED_BYTE, @Pixels[0]);
        End;
        glBindTexture(GL_TEXTURE_2D, 0);
        FFirstFrame := False;
        FMouseDown := False;
        OpenGLControl1.Invalidate;
      Finally
        Bitmap.Free;
      End;
    Finally
      Timer1.Enabled := TimerEnabled;
    End;
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
    If Not OpenGLControl1.MakeCurrent Then
      Raise Exception.Create('Could not activate OpenGL context');
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

