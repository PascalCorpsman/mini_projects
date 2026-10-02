(******************************************************************************)
(* Shader Lab                                                      28.09.2026 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Miniaml app to learn shaders                                 *)
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
  ExtCtrls, StdCtrls, ComCtrls, IniFiles,
  OpenGlcontext, SynEdit, SynHighlighterAny,
  (*
   * Kommt ein Linkerfehler wegen OpenGL dann: sudo apt-get install freeglut3-dev
   *)
  dglOpenGL // http://wiki.delphigl.com/index.php/dglOpenGL.pas
  ;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    OpenGLControl1: TOpenGLControl;
    PageControl1: TPageControl;
    SaveDialog1: TSaveDialog;
    SynAnySyn1: TSynAnySyn;
    SynAnySyn2: TSynAnySyn;
    SynEdit1: TSynEdit;
    SynEdit2: TSynEdit;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    Timer1: TTimer;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
    Procedure OpenGLControl1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    Procedure OpenGLControl1Paint(Sender: TObject);
    Procedure OpenGLControl1Resize(Sender: TObject);
    Procedure SynEdit1KeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState
      );
    Procedure SynEdit2KeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState
      );
    Procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    defcaption: String;
    Function LoadTextBlock(Ini: TCustomIniFile; Const Section, DefaultText: String): String;
    Procedure SaveTextBlock(Ini: TCustomIniFile; Const Section, aText: String);
    Procedure LoadProjectFromFile(Const AFileName: String);
    Procedure SaveProjectToFile(Const AFileName: String);
    Procedure UpdateUniformPanel;
  public
    { public declarations }
    Procedure Go2d();
    Procedure Exit2d();
  End;

Var
  Form1: TForm1;
  Initialized: Boolean = false; // Wenn True dann ist OpenGL initialisiert
  ShaderProgram: GLuint;
  VAO: GLuint; // Vertex Array Object
  VBO: GLuint; // Vertex Buffer Object
  DemoTexture: GLuint;
  StartTime: Double;
  CurrentVertexShader: GLuint;
  CurrentFragmentShader: GLuint;
  MouseX: Integer;
  MouseY: Integer;
  MouseXNorm: Single;
  MouseYNorm: Single;

Implementation

{$R *.lfm}

Uses LCLType, math;

{ TForm1 }

Procedure Tform1.Go2d();
Var
  LocRes: GLint;
  LocMouse: GLint;
  LocTex: GLint;
Begin
  If ShaderProgram <> 0 Then Begin
    glUseProgram(ShaderProgram);
    LocRes := glGetUniformLocation(ShaderProgram, 'uResolution');
    If LocRes >= 0 Then
      glUniform2f(LocRes, OpenGLControl1.Width, OpenGLControl1.Height);
    LocMouse := glGetUniformLocation(ShaderProgram, 'uMouse');
    If LocMouse >= 0 Then
      glUniform2f(LocMouse, MouseXNorm, MouseYNorm);
    If DemoTexture <> 0 Then Begin
      glActiveTexture(GL_TEXTURE0);
      glBindTexture(GL_TEXTURE_2D, DemoTexture);
      LocTex := glGetUniformLocation(ShaderProgram, 'uTexture');
      If LocTex >= 0 Then
        glUniform1i(LocTex, 0);
    End;
  End;
  glBindVertexArray(VAO);
End;

Procedure Tform1.Exit2d();
Begin
  glBindVertexArray(0);
  glUseProgram(0);
End;

Function CreateCheckerTexture: GLuint;
Var
  Pixels: Array Of Byte;
  X, Y: Integer;
  QuadX, QuadY: Integer;
  TileIsWhite: Boolean;
  Index: Integer;
  R, G, B: Byte;
  Tex: GLuint;
Begin
  SetLength(Pixels, 256 * 256 * 4);
  For Y := 0 To 255 Do Begin
    For X := 0 To 255 Do Begin
      TileIsWhite := (((X Div 16) + (Y Div 16)) Mod 2) = 0;
      QuadX := X Div 128;
      QuadY := Y Div 128;

      If TileIsWhite Then Begin
        Case (QuadY * 2) + QuadX Of
          0: Begin
              R := 255;
              G := 0;
              B := 0;
            End; // top-left: red
          1: Begin
              R := 0;
              G := 0;
              B := 255;
            End; // top-right: blue
          2: Begin
              R := 0;
              G := 255;
              B := 0;
            End; // bottom-left: green
        Else
          R := 255;
          G := 255;
          B := 255; // bottom-right: white
        End;
      End
      Else Begin
        R := 0;
        G := 0;
        B := 0;
      End;

      Index := ((Y * 256) + X) * 4;
      Pixels[Index + 0] := R;
      Pixels[Index + 1] := G;
      Pixels[Index + 2] := B;
      Pixels[Index + 3] := 255;
    End;
  End;

  glGenTextures(1, @Tex);
  glBindTexture(GL_TEXTURE_2D, Tex);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE);
  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, 256, 256, 0, GL_RGBA, GL_UNSIGNED_BYTE, @Pixels[0]);
  glBindTexture(GL_TEXTURE_2D, 0);

  Result := Tex;
End;

Function TForm1.LoadTextBlock(Ini: TCustomIniFile; Const Section, DefaultText: String): String;
Var
  LineCount: Integer;
  Lines: TStringList;
  I: Integer;
  s, KeyName: String;
Begin
  LineCount := Ini.ReadInteger(Section, 'LineCount', -1);
  If LineCount < 0 Then Begin
    Result := DefaultText;
    Exit;
  End;

  Lines := TStringList.Create;
  Try
    For I := 0 To LineCount - 1 Do Begin
      KeyName := Format('Line%.4d', [I]);
      s := Ini.ReadString(Section, KeyName, '');
      If pos('"', s) = 1 Then Begin
        delete(s, 1, 1);
        delete(s, length(s), 1);
      End;
      Lines.Add(s);
    End;
    Result := Lines.Text;
  Finally
    Lines.Free;
  End;
End;

Procedure TForm1.SaveTextBlock(Ini: TCustomIniFile; Const Section, aText: String);
Var
  Lines: TStringList;
  I: Integer;
Begin
  Lines := TStringList.Create;
  Try
    Lines.Text := aText;
    Ini.EraseSection(Section);
    Ini.WriteInteger(Section, 'LineCount', Lines.Count);
    For I := 0 To Lines.Count - 1 Do
      Ini.WriteString(Section, Format('Line%.4d', [I]), '"' + Lines[I] + '"');
  Finally
    Lines.Free;
  End;
End;

Procedure TForm1.UpdateUniformPanel;
Var
  elapsed: Double;
Begin
  elapsed := (GetTickCount64 - StartTime) / 1000.0;
  Label1.Caption := 'uTime: ' + FormatFloat('0.00', elapsed);
  Label2.Caption := 'uResolution: ' + IntToStr(OpenGLControl1.Width) + ' x ' + IntToStr(OpenGLControl1.Height);
  Label3.Caption := 'uMouse: ' + FormatFloat('0.000', MouseXNorm) + ' / ' + FormatFloat('0.000', MouseYNorm);
  If Assigned(PageControl1.ActivePage) Then
    Label4.Caption := 'Active shader: ' + PageControl1.ActivePage.Caption
  Else
    Label4.Caption := 'Active shader: -';
End;

Const
  DefaultVertexSrc: PChar =
  '#version 330 core'#10 +
    'layout(location = 0) in vec2 aPos;'#10 +
    'out vec2 fragCoord;'#10 +
    'out vec2 texCoord;'#10 +
    'uniform vec2 uResolution;'#10 +
    'uniform vec2 uMouse;'#10 +
    'uniform float uTime;'#10 +
    '// most simple transformation'#10 +
    'void main()'#10 +
    '{'#10 +
    '  fragCoord = aPos * uResolution;'#10 +
    '  texCoord = aPos;'#10 +
    '  vec2 ndc = aPos * 2.0 - 1.0;'#10 +
    '  gl_Position = vec4(ndc, 0.0, 1.0);'#10 +
    '}'
    ;

  DefaultFragmentSrc: PChar =
  '#version 330 core'#10 +
    'in vec2 fragCoord;'#10 +
    'in vec2 texCoord;'#10 +
    'uniform vec2 uResolution;'#10 +
    'uniform vec2 uMouse;'#10 +
    'uniform float uTime;'#10 +
    'uniform sampler2D uTexture;'#10 +
    'out vec4 FragColor;'#10 +
    '// demo shader'#10 +
    'void main()'#10 +
    '{'#10 +
    '  vec2 uv = fragCoord / uResolution;'#10 +
    '  FragColor = vec4(uv, 0.5, 1.0);'#10 +
    '}'
    ;

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
    form1.memo1.Append('Shader compilation error:' + LineEnding + Log);
    glDeleteShader(S);
    Exit;
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
  fs := CompileShader(FSrc, GL_FRAGMENT_SHADER);
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
    form1.memo1.Append('Shader compilation error:' + LineEnding + Log);
    glDeleteProgram(prog);
    prog := 0;
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
    ShaderProgram := CreateShaderProgram(DefaultVertexSrc, DefaultFragmentSrc);
    glGenVertexArrays(1, @VAO);
    glGenBuffers(1, @VBO);
    DemoTexture := CreateCheckerTexture;

    // Der Anwendung erlauben zu Rendern.
    Initialized := True;
    OpenGLControl1Resize(Nil);
  End;
  Form1.Invalidate;
End;

Procedure TForm1.OpenGLControl1Paint(Sender: TObject);
Var
  vertices: Array[0..15] Of GLfloat;
  locTime: GLint;
  elapsed: Double;
Begin
  If Not Initialized Then Exit;
  // Render Szene
  glClearColor(0.0, 0.0, 0.0, 0.0);
  glClear(GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT);
  If ShaderProgram = 0 Then Begin
    OpenGLControl1.SwapBuffers;
    Exit;
  End;
  Go2d;

  // Fullscreen Quad (0,0)-(1,1)
  vertices[0] := 0.0;
  vertices[1] := 0.0;
  vertices[2] := 0.0;
  vertices[3] := 0.0; // bottom-left
  vertices[4] := 1.0;
  vertices[5] := 0.0;
  vertices[6] := 1.0;
  vertices[7] := 0.0; // bottom-right
  vertices[8] := 0.0;
  vertices[9] := 1.0;
  vertices[10] := 0.0;
  vertices[11] := 1.0; // top-left
  vertices[12] := 1.0;
  vertices[13] := 1.0;
  vertices[14] := 1.0;
  vertices[15] := 1.0; // top-right

  glBindBuffer(GL_ARRAY_BUFFER, VBO);
  glBufferData(GL_ARRAY_BUFFER, SizeOf(vertices), @vertices[0], GL_DYNAMIC_DRAW);
  glEnableVertexAttribArray(0);
  glVertexAttribPointer(0, 2, GL_FLOAT, GL_FALSE, 4 * SizeOf(GLfloat), Nil);
  glEnableVertexAttribArray(1);
  glVertexAttribPointer(1, 2, GL_FLOAT, GL_FALSE, 4 * SizeOf(GLfloat), Pointer(2 * SizeOf(GLfloat)));

  // Pass time to shader
  elapsed := (GetTickCount64 - StartTime) / 1000.0;
  locTime := glGetUniformLocation(ShaderProgram, 'uTime');
  If locTime >= 0 Then
    glUniform1f(locTime, elapsed);

  glDrawArrays(GL_TRIANGLE_STRIP, 0, 4);
  Exit2d;
  OpenGLControl1.SwapBuffers;
End;

Procedure TForm1.OpenGLControl1Resize(Sender: TObject);
Begin
  If Initialized Then Begin
    If OpenGLControl1.MakeCurrent Then
      glViewport(0, 0, OpenGLControl1.Width, OpenGLControl1.Height);
    UpdateUniformPanel;
    OpenGLControl1.Invalidate;
  End;
End;

Procedure TForm1.SynEdit1KeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If key = VK_F9 Then Begin
    Button1.Click;
  End;
End;

Procedure TForm1.SynEdit2KeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If key = VK_F9 Then Begin
    Button1.Click;
  End;
End;

Procedure TForm1.OpenGLControl1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
Begin
  MouseX := X;
  MouseY := Y;
  If OpenGLControl1.Width > 0 Then
    MouseXNorm := X / OpenGLControl1.Width
  Else
    MouseXNorm := 0;
  If OpenGLControl1.Height > 0 Then
    MouseYNorm := Y / OpenGLControl1.Height
  Else
    MouseYNorm := 0;
  MouseXNorm := EnsureRange(MouseXNorm, 0, 1);
  MouseYNorm := EnsureRange(MouseYNorm, 0, 1);
  UpdateUniformPanel;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  src: String;
  pSrc: PChar;
  newProgram: GLuint;
  vsrc, fsrc: PChar;
Begin
  If Not Initialized Then Begin
    showmessage('OpenGL not initialized yet');
    Exit;
  End;
  Memo1.Clear;

  // Determine which tab is active and compile accordingly
  If PageControl1.ActivePageIndex = 0 Then Begin
    // Fragment Shader Tab
    src := SynEdit1.Text;
    pSrc := PChar(src);
    vsrc := PChar(SynEdit2.Text);
    fsrc := pSrc;
  End
  Else Begin
    // Vertex Shader Tab
    src := SynEdit2.Text;
    pSrc := PChar(src);
    vsrc := pSrc;
    fsrc := PChar(SynEdit1.Text);
  End;

  newProgram := CreateShaderProgram(vsrc, fsrc);
  If newProgram <> 0 Then Begin
    If ShaderProgram <> 0 Then
      glDeleteProgram(ShaderProgram);
    ShaderProgram := newProgram;
    StartTime := GetTickCount64;
    Memo1.Append('Shader compiled successfully!');
  End
  Else Begin
    If ShaderProgram <> 0 Then Begin
      glDeleteProgram(ShaderProgram);
      ShaderProgram := 0;
    End;
    Memo1.Append('Preview disabled until the next successful compile.');
  End;
  OpenGLControl1.Invalidate;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  If OpenDialog1.Execute Then Begin
    LoadProjectFromFile(OpenDialog1.FileName);
  End;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  SaveDialog1.FileName := OpenDialog1.FileName;
  If SaveDialog1.Execute Then Begin
    SaveProjectToFile(SaveDialog1.FileName);
  End;
End;

Procedure TForm1.LoadProjectFromFile(Const AFileName: String);
Var
  Ini: TMemIniFile;
Begin
  Ini := TMemIniFile.Create(AFileName);
  Try
    SynEdit1.Text := LoadTextBlock(Ini, 'Shaders.Fragment', DefaultFragmentSrc);
    SynEdit2.Text := LoadTextBlock(Ini, 'Shaders.Vertex', DefaultVertexSrc);
    PageControl1.ActivePageIndex := Ini.ReadInteger('UI', 'ActiveTabIndex', 0);
    Memo1.Clear;
    Memo1.Lines.Add('Loaded project: ' + ExtractFileName(AFileName));
    UpdateUniformPanel;
    caption := defcaption + ': ' + ExtractFileName(AFileName);
    If Initialized Then
      Button1Click(Self);
  Finally
    Ini.Free;
  End;
End;

Procedure TForm1.SaveProjectToFile(Const AFileName: String);
Var
  Ini: TMemIniFile;
Begin
  Ini := TMemIniFile.Create(AFileName);
  Try
    Ini.WriteString('Project', 'Format', 'Shader Lab');
    Ini.WriteString('Project', 'Version', '1');
    Ini.WriteInteger('UI', 'ActiveTabIndex', PageControl1.ActivePageIndex);
    SaveTextBlock(Ini, 'Shaders.Fragment', SynEdit1.Text);
    SaveTextBlock(Ini, 'Shaders.Vertex', SynEdit2.Text);
    Ini.UpdateFile;
    Memo1.Clear;
    Memo1.Lines.Add('Saved project: ' + ExtractFileName(AFileName));
    caption := defcaption + ': ' + ExtractFileName(AFileName);
  Finally
    Ini.Free;
  End;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  (*
   * Missing features:
   * - Code formater
   * - Maus Koordinaten via universal zur Verfügung stellen
   *)
  defcaption := 'Shader Lab ver.: 0.01 by Corpsman, www.Corpsman.de';
  caption := defcaption;
  Application.Title := defcaption;
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
  Memo1.Clear;

  // Configure Fragment Shader Highlighter
  With SynAnySyn1 Do Begin
    Comments := [csCStyle, csAnsiStyle]; // // und /* */ comments
    KeyWords.Clear; // GLSL Keywords
    KeyWords.AddCommaText(
      'const,void,main,in,out,uniform,if,else,for,while,do,return,true,false,discard,' +
      'float,int,uint,bool,vec2,vec3,vec4,ivec2,ivec3,ivec4,bvec2,bvec3,bvec4,' +
      'mat2,mat3,mat4,mat2x2,mat2x3,mat2x4,mat3x2,mat3x3,mat3x4,mat4x2,mat4x3,mat4x4,' +
      'sampler1D,sampler2D,sampler3D,samplerCube,samplerShadow,' +
      'gl_FragCoord,gl_FragColor,gl_Position,gl_PointSize,' +
      'abs,acos,asin,atan,cos,sin,tan,cosh,sinh,tanh,pow,exp,exp2,log,log2,sqrt,inversesqrt,' +
      'sign,floor,ceil,fract,mod,min,max,clamp,mix,step,smoothstep,' +
      'length,distance,dot,cross,normalize,faceforward,reflect,refract,' +
      'lessThan,lessThanEqual,greaterThan,greaterThanEqual,equal,notEqual,any,all,not,' +
      'texture,textureLod,textureProj');
  End;

  // Configure Vertex Shader Highlighter
  With SynAnySyn2 Do Begin
    Comments := [csCStyle, csAnsiStyle]; // // und /* */ comments
    KeyWords.Clear; // GLSL Keywords + Vertex-specific
    KeyWords.AddCommaText(
      'layout,location,' +
      'void,main,in,out,uniform,if,else,for,while,do,return,true,false,discard,' +
      'float,int,uint,bool,vec2,vec3,vec4,ivec2,ivec3,ivec4,bvec2,bvec3,bvec4,' +
      'mat2,mat3,mat4,mat2x2,mat2x3,mat2x4,mat3x2,mat3x3,mat3x4,mat4x2,mat4x3,mat4x4,' +
      'sampler1D,sampler2D,sampler3D,samplerCube,samplerShadow,' +
      'gl_Position,gl_PointSize,gl_VertexID,gl_InstanceID,' +
      'abs,acos,asin,atan,cos,sin,tan,cosh,sinh,tanh,pow,exp,exp2,log,log2,sqrt,inversesqrt,' +
      'sign,floor,ceil,fract,mod,min,max,clamp,mix,step,smoothstep,' +
      'length,distance,dot,cross,normalize,faceforward,reflect,refract,' +
      'lessThan,lessThanEqual,greaterThan,greaterThanEqual,equal,notEqual,any,all,not,' +
      'texture,textureLod,textureProj');
  End;

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
  VAO := 0;
  VBO := 0;
  Timer1.Interval := 17;
  MouseX := 0;
  MouseY := 0;

  // Initialize both editors with default shaders
  SynEdit1.Text := DefaultFragmentSrc;
  SynEdit2.Text := DefaultVertexSrc;

  PageControl1.ActivePageIndex := 1; // Default Fragment Shader

  StartTime := GetTickCount64;
  MouseXNorm := 0;
  MouseYNorm := 0;
  DemoTexture := 0;
  UpdateUniformPanel;
End;

Procedure TForm1.FormDestroy(Sender: TObject);
Begin
  If Initialized And OpenGLControl1.MakeCurrent Then Begin
    If ShaderProgram <> 0 Then
      glDeleteProgram(ShaderProgram);
    If VAO <> 0 Then
      glDeleteVertexArrays(1, @VAO);
    If VBO <> 0 Then
      glDeleteBuffers(1, @VBO);
    If DemoTexture <> 0 Then
      glDeleteTextures(1, @DemoTexture);
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
    UpdateUniformPanel;
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

End.

