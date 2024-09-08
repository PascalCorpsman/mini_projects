(******************************************************************************)
(* spirograph                                                      07.09.2024 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : arbiture drawing visualisation                               *)
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
  ExtCtrls, StdCtrls,
  OpenGlcontext,
  (*
   * Kommt ein Linkerfehler wegen OpenGL dann: sudo apt-get install freeglut3-dev
   *)
  dglOpenGL // http://wiki.delphigl.com/index.php/dglOpenGL.pas
  , uvectormath
  , Types;

Const
  Colors: Array Of TColor = (
    clMaroon
    , clred
    , clGreen
    , clLime
    , clNavy
    , clBlue
    , clDkGray
    , clLtGray
    //, clWhite // Das ist doof wegen der Listbox
    );

Type

  TSpiral = Record
    Len: Single;
    Rotation: Single;
    Rotation_Speed: Single;
    Direction: Boolean;
  End;

  TSpirals = Array Of TSpiral;

  TState = (sIdle, sSimulate);

  Tpts = Array Of tvector2;

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    CheckBox1: TCheckBox;
    CheckBox3: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    ListBox1: TListBox;
    OpenGLControl1: TOpenGLControl;
    ScrollBar1: TScrollBar;
    Timer1: TTimer;
    procedure Button10Click(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure Button8Click(Sender: TObject);
    Procedure Button9Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure ListBox1Click(Sender: TObject);
    Procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    Procedure OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
    Procedure OpenGLControl1Paint(Sender: TObject);
    Procedure OpenGLControl1Resize(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    Spiral: TSpirals;
    State: TState;
    pts: Tpts; // Leider funktioniert der Accumulationbuffer von OpenGL nicht mit OpenGlControl -> Also muss das von Hand nachgebildet werden.
    StartTime, LastRenderTime: QWord;
    Procedure RenderScene();
    Procedure StartSim;
    Procedure AddLCLElement;
    Procedure LoadLCLElement;
    Procedure UpdateLclElement;
    Procedure Reset;
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

Uses math;

{ TForm1 }

Procedure TForm1.Go2d;
Begin
  glMatrixMode(GL_PROJECTION);
  glPushMatrix(); // Store The Projection Matrix
  glLoadIdentity(); // Reset The Projection Matrix
  glOrtho(0, OpenGLControl1.Width, OpenGLControl1.height, 0, -1, 1); // Set Up An Ortho Screen
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix(); // Store old Modelview Matrix
  glLoadIdentity(); // Reset The Modelview Matrix
End;

Procedure TForm1.Exit2d;
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
    (*
    Man bedenke, jedesmal wenn der Renderingcontext neu erstellt wird, müssen sämtliche Graphiken neu Geladen werden.
    Bei Nutzung der TOpenGLGraphikengine, bedeutet dies, das hier ein clear durchgeführt werden mus !!
    *)
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
  go2d;
  RenderScene();
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
  End;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  caption := 'Spirograph ver. 0.01 by Corpsman';
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
  Spiral := Nil;
  State := sIdle;
  pts := Nil;
  edit1.text := '50';
  edit2.text := '1';
  Button6.Click;
End;

Procedure TForm1.ListBox1Click(Sender: TObject);
Begin
  LoadLCLElement;
End;

Procedure TForm1.ListBox1DrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
Begin
  If index <> ListBox1.ItemIndex Then Begin
    ListBox1.Canvas.Font.Color := Colors[index Mod Length(Colors)];
  End;
  // Das Ganz Normale "Anzeigen" Nachahmen
  ListBox1.Canvas.TextRect(ARect, ARect.Left + Scale96ToScreen(8),
    (ARect.Bottom + ARect.Top - ListBox1.Canvas.TextHeight('A')) Div 2
    , ListBox1.Items[index]);
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  If State = sIdle Then Begin
    StartSim;
  End
  Else Begin
    state := sIdle;
  End;
End;

procedure TForm1.Button10Click(Sender: TObject);
begin
  // Export Image
end;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Var
  t: TSpiral;
Begin
  // Move Up
  If ListBox1.ItemIndex > 0 Then Begin
    t := Spiral[ListBox1.ItemIndex - 1];
    Spiral[ListBox1.ItemIndex - 1] := Spiral[ListBox1.ItemIndex];
    Spiral[ListBox1.ItemIndex] := t;
    ListBox1.Items.Exchange(ListBox1.ItemIndex, ListBox1.ItemIndex - 1);
    ListBox1.ItemIndex := ListBox1.ItemIndex - 1;
    Reset;
  End;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Var
  t: TSpiral;
Begin
  // Move Down
  If ListBox1.ItemIndex < listbox1.items.count - 1 Then Begin
    t := Spiral[ListBox1.ItemIndex + 1];
    Spiral[ListBox1.ItemIndex + 1] := Spiral[ListBox1.ItemIndex];
    Spiral[ListBox1.ItemIndex] := t;
    ListBox1.Items.Exchange(ListBox1.ItemIndex, ListBox1.ItemIndex + 1);
    ListBox1.ItemIndex := ListBox1.ItemIndex + 1;
    Reset;
  End;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Var
  i: Integer;
Begin
  // Del Entry
  If ListBox1.ItemIndex <> -1 Then Begin
    For i := ListBox1.ItemIndex To high(Spiral) - 1 Do Begin
      Spiral[i] := Spiral[i + 1];
    End;
    setlength(Spiral, high(Spiral));
    ListBox1.Items.Delete(ListBox1.ItemIndex);
  End;
End;

Procedure TForm1.Button6Click(Sender: TObject);
Begin
  AddLCLElement;
End;

Procedure TForm1.Button7Click(Sender: TObject);
Begin
  UpdateLclElement;
End;

Procedure TForm1.Button8Click(Sender: TObject);
Begin
  // Load Settings
  // Todo: Implementieren
End;

Procedure TForm1.Button9Click(Sender: TObject);
Begin
  // Save Settings
  // Todo: Implementieren
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

Procedure TForm1.RenderScene;
Var
  i: Integer;
  n: QWord;
  d: Extended;
  m: TMatrix4x4;
  c: TColor;
Begin
  If State = sSimulate Then Begin
    n := GetTickCount64;
    //    caption := inttostr(n - StartTime);
    d := (n - LastRenderTime) / 1000;
    d := d * (100 - ScrollBar1.Position) / 100;
    LastRenderTime := n;
    For i := 0 To high(Spiral) Do Begin
      If Spiral[i].Direction Then Begin
        Spiral[i].Rotation := Spiral[i].Rotation + Spiral[i].Rotation_Speed * d * 360;
      End
      Else Begin
        Spiral[i].Rotation := Spiral[i].Rotation - Spiral[i].Rotation_Speed * d * 360;
      End;
    End;
  End;
  // Render Center
  glTranslatef(OpenGLControl1.Width / 2, OpenGLControl1.Height / 2, 0);
  glColor3f(1, 0, 0);
  glBegin(GL_POINTS);
  glVertex2f(0, 0);
  glend();
  glPushMatrix;
  // Render Moving Point
  For i := 0 To high(Spiral) Do Begin
    glRotatef(Spiral[i].Rotation, 0, 0, 1);
    glTranslatef(0, -Spiral[i].Len, 0);
  End;
  glColor3f(1, 1, 1);
  glBegin(GL_POINTS);
  glVertex2f(0, 0);
  glend();
  If State = sSimulate Then Begin
    glGetFloatv(GL_MODELVIEW_MATRIX, @m);
    setlength(pts, high(pts) + 2);
    pts[high(pts)] := v2(m[3, 0], m[3, 1]);
  End;
  glPopMatrix;

  // Render der Linien falls gewünscht
  If CheckBox1.Checked Then Begin
    glPushMatrix;
    For i := 0 To high(Spiral) Do Begin
      glRotatef(Spiral[i].Rotation, 0, 0, 1);
      c := Colors[i Mod length(colors)];
      glColor3ub(c And $FF, (c And $FF00) Shr 8, (c And $FF0000) Shr 16);
      glBegin(GL_LINES);
      glVertex2f(0, 0);
      glVertex2f(0, -Spiral[i].Len);
      glend();
      glTranslatef(0, -Spiral[i].Len, 0);
    End;
    glPopMatrix;
  End;
  //  If State = sSimulate Then Begin
  glPushMatrix;
  glTranslatef(-OpenGLControl1.Width / 2, -OpenGLControl1.Height / 2, 0);
  glColor3f(1, 1, 1);
  glBegin(GL_LINE_STRIP);
  For i := 0 To high(pts) Do Begin
    glVertex2fv(@pts[i]);
  End;
  glend;
  glPopMatrix;
  //  End;
End;

Procedure TForm1.StartSim;
Var
  i: Integer;
Begin
  State := sSimulate;
  LastRenderTime := GetTickCount64;
  StartTime := LastRenderTime;
  setlength(pts, 0);
  For i := 0 To high(Spiral) Do Begin
    Spiral[i].Rotation := 0;
  End;
End;

Procedure TForm1.AddLCLElement;
Begin
  setlength(Spiral, high(Spiral) + 2);
  Spiral[high(Spiral)].Rotation := 0;
  Spiral[high(Spiral)].Direction := CheckBox3.Checked;
  Spiral[high(Spiral)].Rotation_Speed := StrToFloatdef(edit2.text, 0);
  Spiral[high(Spiral)].Len := strtofloatdef(Edit1.Text, 10);
  ListBox1.Items.Add(format(
    '%0.2f %0.2f'
    , [
    Spiral[high(Spiral)].Len, IfThen(Spiral[high(Spiral)].Direction, -1.0, 1.0) * Spiral[high(Spiral)].Rotation_Speed
    ]));
  ListBox1.ItemIndex := ListBox1.Items.Count - 1;
  Reset;
End;

Procedure TForm1.LoadLCLElement;
Begin
  If ListBox1.ItemIndex <> -1 Then Begin
    Edit1.Text := FloatToStr(Spiral[ListBox1.ItemIndex].Len);
    Edit2.Text := FloatToStr(Spiral[ListBox1.ItemIndex].Rotation_Speed);
    CheckBox3.Checked := Spiral[ListBox1.ItemIndex].Direction;
  End
  Else Begin
    edit1.text := '';
    edit2.text := '';
    CheckBox3.Checked := false;
  End;
End;

Procedure TForm1.UpdateLclElement;
Var
  index: integer;
Begin

  If ListBox1.ItemIndex <> -1 Then Begin
    index := ListBox1.ItemIndex;
    Spiral[index].Rotation := 0;
    Spiral[index].Direction := CheckBox3.Checked;
    Spiral[index].Rotation_Speed := StrToFloatdef(edit2.text, 0);
    Spiral[index].Len := strtofloatdef(Edit1.Text, 10);
    ListBox1.Items[index] := format(
      '%0.2f %0.2f'
      , [
      Spiral[index].Len, IfThen(Spiral[index].Direction, -1.0, 1.0) * Spiral[index].Rotation_Speed
        ]);
    Reset;
  End;
End;

Procedure TForm1.Reset;
Var
  i: Integer;
Begin
  setlength(pts, 0);
  For i := 0 To high(Spiral) Do Begin
    Spiral[i].Rotation := 0;
  End;
End;

End.

