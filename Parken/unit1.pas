(******************************************************************************)
(* Parken                                                          ??.??.???? *)
(*                                                                            *)
(* Version     : 0.03                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Test / Demo application for trecktric curves                 *)
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

{.$DEFINE DebuggMode}// Betrifft nur die OpenGL Befehle

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, OpenGLContext, uparken, dglopengl, lclintf, lcltype, uvectormath,
  ugraphics;

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
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    OpenGLControl1: TOpenGLControl;
    Timer1: TTimer;
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure FormCreate(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure MenuItem3Click(Sender: TObject);
    Procedure MenuItem5Click(Sender: TObject);
    Procedure MenuItem6Click(Sender: TObject);
    Procedure MenuItem7Click(Sender: TObject);
    Procedure MenuItem8Click(Sender: TObject);
    Procedure MenuItem9Click(Sender: TObject);
    Procedure OpenGLControl1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure OpenGLControl1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    Procedure OpenGLControl1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure Timer1Timer(Sender: TObject);
    Procedure OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
    Procedure OpenGLControl1Paint(Sender: TObject);
    Procedure OpenGLControl1Resize(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  End;

Var
  Form1: TForm1;
  field: TArea = Nil;
  car: TCar = Nil;
  EditCar: TCar = Nil;
  EditEinAchsenAnhaenger: TEinAchsenAnhaenger = Nil;
  EditZweiAchsenAnhaenger: TZweiAchsenAnhaenger = Nil;
  EditTrailer: TBaseAnhaenger = Nil;
  EditArea: TArea = Nil;

  initialized: Boolean = false;

Implementation

{$R *.lfm}

Uses unit2, unit3, unit4, unit5, lazutf8, LazFileUtils;

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
  glenable(GL_LINE_SMOOTH);

  // Der Anwendung erlauben zu Rendern.
  Initialized := True;
  OpenGLControl1Resize(Nil);
End;

Procedure TForm1.OpenGLControl1Resize(Sender: TObject);
Begin
  If Not Initialized Then Exit;
  AppWidth := OpenGLControl1.Width;
  AppHeight := OpenGLControl1.Height;
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();
  glViewport(0, 0, OpenGLControl1.Width, OpenGLControl1.Height);
  gluPerspective(45.0, OpenGLControl1.Width / OpenGLControl1.Height, 0.1, 100.0);
  glMatrixMode(GL_MODELVIEW);
End;

Procedure TForm1.OpenGLControl1Paint(Sender: TObject);
Var
  c: TRGB;
Begin
  If Not Initialized Then Exit;
  // Render Szene
  glClearColor(1.0, 1.0, 1.0, 0.0);
  glClear(GL_COLOR_BUFFER_BIT);
  glLoadIdentity();
  Go2d;
  Case GameState Of
    gsError: Begin
        field.render;
        car.Render;
      End;
    gsInGame: Begin
        field.render;
        car.Render;
        If field.Collision(car) Then Begin
          field.SetCar(car);
        End;
      End;
    gsEditCar: Begin
        EditCar.render;
      End;
    gsEditTrailer: Begin
        If assigned(EditTrailer) Then
          EditTrailer.render;
      End;
    gsEditMap: Begin
        EditArea.render;
        If m1.X <> -1 Then Begin
          c := ColorToRGB(form4.Button1.Font.color);
          If form4.RadioGroup1.itemindex < 2 Then Begin
            glColor3f(c.r / 255, c.g / 255, c.b / 255);
          End
          Else Begin
            glColor3f(1, 0, 0);
          End;
          glbegin(gl_lines);
          glVertex2i(unit4.m.x, unit4.m.y);
          glVertex2i(m1.x, m1.y);
          glend;
        End;
      End;
  End;
  Exit2d;
  OpenGLControl1.SwapBuffers;
End;

Procedure TForm1.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Begin
  timer1.enabled := false;
  initialized := false;
  EditCar.free;
  EditEinAchsenAnhaenger.free;
  EditZweiAchsenAnhaenger.free;
  EditArea.free;
  car.free;
  field.free;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  // Init dglOpenGL.pas , Teil 1
  If Not InitOpenGl Then Begin
    showmessage('Error, could not init dglOpenGL.pas');
    Halt;
  End;
  OpenDialog1.InitialDir := ExtractFilePath(paramstr(0));
  OpenGLControl1.Align := alclient;
  car := TCar.create;
  EditCar := TCar.create;
  EditCar.ForceRenderAnhaengerKupplung := true;
  EditEinAchsenAnhaenger := TEinachsenAnhaenger.create(Nil);
  EditEinAchsenAnhaenger.ForceRenderAnhaengerKupplung := true;
  EditZweiAchsenAnhaenger := TZweiAchsenAnhaenger.Create(Nil);
  EditZweiAchsenAnhaenger.ForceRenderAnhaengerKupplung := true;
  EditArea := TArea.create;
  editarea.RenderStartPos := true;
  TEinachsenAnhaenger.create(car);
  Caption := 'Car Simulation ver. 0.03 by Corpsman, support : www.Corpsman.de';
  {
  TODO :

  - evtl.Anzeigen einer Vorschau beim Erzeugen der Quests
  - Dokumentation
  //}
  field := TArea.create;
  field.SetCarPosition(v2(field.AreaWidth - 250, field.AreaHeight / 2), 180);
  field.AddLine(v2(field.AreaWidth - 100, 100), v2(field.AreaWidth - 100, field.Areaheight - 100), ugraphics.ColorToRGB(clgreen));
  field.SetParkArea(v2(field.AreaWidth - 60, field.AreaHeight / 2 + 75), 50, 150, 270);
  field.SetCar(car);
  // Größe des Fensters Festlegen
  Constraints.MinHeight := 480;
  Constraints.MaxHeight := 480;
  Constraints.MinWidth := 640;
  Constraints.MaxWidth := 640;
End;

Procedure TForm1.MenuItem2Click(Sender: TObject);
Var
  fs: TFilestream;
  f: Textfile;
  p, s: String;
  i, j: Integer;
  an: TBaseVehicle;
Begin
  If OpenDialog1.Execute Then Begin
    If Form2.Visible Then
      form2.close;
    If Form3.Visible Then
      form3.close;
    If Form4.Visible Then
      form4.close;
    If Form5.Visible Then
      form5.close;
    p := systoutf8(IncludeTrailingBackslash(ExtractFilePath(OpenDialog1.FileName)));
    assignfile(f, OpenDialog1.FileName);
    reset(f);
    readln(f, s);
    If FileExistsutf8(p + s) Then Begin
      car.free;
      car := TCar.create;
      car.LoadFromFile(p + s);
    End
    Else Begin
      showmessage('Invalid Quest File, could not load :'#13#13 + p + s);
      GameState := gswait;
      closefile(f);
      exit;
    End;
    // Laden der Trailer
    readln(f, s);
    j := strtointdef(s, -1);
    For i := 0 To j - 1 Do Begin
      readln(f, s);
      If FileExistsutf8(p + s) Then Begin
        fs := TFileStream.create(utf8tosys(p + s), fmopenread);
        fs.read(j, sizeof(j));
        Case j Of
          0 {=EinachsenAnhaenger}: Begin
              If i = 0 Then
                an := TEinachsenAnhaenger.create(car)
              Else
                an := TEinachsenAnhaenger.create(an);
              an.LoadFromStream(fs);
            End;
          1 {=ZweiachsenAnhaenger}: Begin
              If i = 0 Then
                an := TZweiAchsenAnhaenger.create(car)
              Else
                an := TZweiAchsenAnhaenger.create(an);
              an.LoadFromStream(fs);
            End;
        End;
        fs.free;
      End
      Else Begin
        showmessage('Invalid Quest File, could not load :'#13#13 + p + s);
        GameState := gswait;
        closefile(f);
        exit;
      End;
    End;
    readln(f, s);
    If FileExistsutf8(p + s) Then Begin
      field.LoadFromFile(utf8tosys(p + s));
    End
    Else Begin
      showmessage('Invalid Quest File, could not load :'#13#13 + p + s);
      GameState := gswait;
      closefile(f);
      exit;
    End;
    closefile(f);
    // 1. Trivial Test
    field.SetCar(car);
    // Das Level ist nicht Spielbar
    If field.Collision(car) Then Begin
      showmessage('Invalid Quest-File, startposition doesn''t match with car.');
      GameState := gsError;
      Timer1.enabled := True;
      exit;
    End;
    // Alles hat geklappt, also Starten wir das Game
    GameState := gsInGame;
    Timer1.enabled := True;
  End;
End;

Procedure TForm1.MenuItem3Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.MenuItem5Click(Sender: TObject);
Begin
  If Form3.Visible Then
    form3.close;
  If Form4.Visible Then
    form4.close;
  If Form5.Visible Then
    form5.close;
  GameState := gsEditCar;
  Timer1.Enabled := true;
  EditCar.Alpha := car.Alpha;
  EditCar.Angle := car.Alpha;
  EditCar.H1 := car.h1;
  EditCar.H2 := car.h2;
  EditCar.H3 := car.h3;
  EditCar.H4 := car.h4;
  EditCar.w1 := car.w1;
  EditCar.w2 := car.w2;
  form2.top := form1.top;
  form2.left := form1.left + form1.Width + 9;
  // Übernehmen der Werte in die LCL
  form2.ScrollBar1.Position := round(editcar.Alpha);
  form2.ScrollBar2.Position := round(editcar.H1);
  form2.ScrollBar3.Position := round(editcar.H2);
  form2.ScrollBar4.Position := round(editcar.H3);
  form2.ScrollBar7.Position := round(editcar.H4);
  form2.ScrollBar5.Position := round(editcar.w1);
  form2.ScrollBar6.Position := round(editcar.w2);
  form2.Show;
End;

Procedure TForm1.MenuItem6Click(Sender: TObject);
Begin
  //   Es kann zuerst der Anhängertyp gewählt werden, dann dessen Einstellungen realisiert
  If Form2.Visible Then
    form2.close;
  If Form4.Visible Then
    form4.close;
  If Form5.Visible Then
    form5.close;
  GameState := gsEditTrailer;
  Timer1.Enabled := true;
  form3.ComboBox1.ItemIndex := 0;
  form3.ComboBox1.OnChange(Nil);
  form3.top := form1.top;
  form3.left := form1.left + form1.Width + 9;
  form3.show;
End;

Procedure TForm1.MenuItem7Click(Sender: TObject);
Begin
  If Form2.Visible Then
    form2.close;
  If Form3.Visible Then
    form3.close;
  If Form4.Visible Then
    form4.close;
  timer1.enabled := false;
  form5.label4.caption := '';
  form5.label5.caption := '';
  form5.ListBox1.clear;
  form5.Showmodal;
  GameState := gswait;
  timer1.enabled := True;
End;

Procedure TForm1.MenuItem8Click(Sender: TObject);
Begin
  If Form2.Visible Then
    form2.close;
  If Form3.Visible Then
    form3.close;
  If Form5.Visible Then
    form5.close;
  Timer1.Enabled := true;
  unit4.m1 := point(-1, -1);
  EditArea.clear;
  GameState := gsEditMap;
  form4.top := form1.top;
  form4.left := form1.left + form1.Width + 9;
  form4.show;
End;

Procedure TForm1.MenuItem9Click(Sender: TObject);
Begin
  field.SetCar(car);
  GameState := gsInGame;
  Timer1.enabled := True;
End;

Procedure TForm1.OpenGLControl1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  p1, p2: TPoint;
  v1_, v2_: TVector2;
Begin
  If GameState = gsEditMap Then Begin
    m := point(x, y);
    If ssright In shift Then
      unit4.m1 := point(-1, -1);
    If ssleft In shift Then Begin
      If m1.X = -1 Then Begin
        m1 := point(x, y);
      End
      Else Begin
        Case form4.RadioGroup1.ItemIndex Of
          // Add Line
          0: Begin
              EditArea.AddLine(v2(m1.x, m1.y), v2(x, y), ColorToRGB(form4.button1.font.color));
              unit4.m1 := point(-1, -1);
            End;
          // Add Label Linne
          1: Begin
              EditArea.AddLabelLine(v2(m1.x, m1.y), v2(x, y), ColorToRGB(form4.button1.font.color));
              unit4.m1 := point(-1, -1);
            End;
          // Del Line
          2: Begin
              p1 := m1;
              p2 := point(x, y);
              EditArea.DelLine(v2(p1.x, p1.y), v2(p2.x, p2.y));
              unit4.m1 := point(-1, -1);
            End;
          // Die Start Position
          3: Begin
              v1_ := v2(m1.X, m1.Y);
              v2_ := v2(x, y);
              v2_ := subv2(v1_, v2_);
              EditArea.SetCarPosition(v1_, ArcTangens(v2_.x, v2_.y) + 180);
              unit4.m1 := point(-1, -1);
            End;
          // Park Area
          4: Begin
              v1_ := v2(m1.X, m1.Y);
              v2_ := v2(x, y);
              v2_ := subv2(v1_, v2_);
              EditArea.SetParkArea(v1_, strtointdef(form4.edit1.text, 50), lenv2(v2_), ArcTangens(v2_.x, v2_.y) + 180);
              unit4.m1 := point(-1, -1);
            End;
        End;
      End;
    End;
  End;
End;

Procedure TForm1.OpenGLControl1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
Begin
  If GameState = gsEditMap Then
    unit4.m := point(x, y);
End;

Procedure TForm1.OpenGLControl1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  // So Gehts "Intuitiver"
  OpenGLControl1MouseDown(sender, Button, Shift, x, y);
End;

Procedure TForm1.Timer1Timer(Sender: TObject);
{$IFDEF DebuggMode}
Var
  i: Cardinal;
  p: Pchar;
{$ENDIF}
Begin
  If Initialized Then Begin
    OpenGLControl1.invalidate;
    // Steuerung des Auto's nur bei InGame
    If GameState = gsInGame Then Begin
      If abs(GetKeyState(vk_left)) > 1 Then Begin
        car.Angle := car.Angle - 2.0;
      End;
      If abs(GetKeyState(vk_right)) > 1 Then Begin
        car.Angle := car.Angle + 2.0;
      End;
      If abs(GetKeyState(vk_up)) > 1 Then Begin
        car.Speed := car.Speed + 1.1;
      End;
      If abs(GetKeyState(vk_down)) > 1 Then Begin
        car.Speed := car.Speed - 1.1;
      End;
      car.Move(0.04);
      If field.Finished(car) Then Begin
        GameState := gswait;
        timer1.enabled := false;
        Application.ProcessMessages;
        showmessage('You made it.');
        form1.OpenGLControl1.DoOnPaint;
      End;
    End;
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
    If abs(GetKeyState(VK_ESCAPE)) > 1 Then Begin
      Initialized := false;
      close;
    End;
  End;
End;

End.

