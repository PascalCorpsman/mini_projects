(******************************************************************************)
(* Clickomania                                                     ??.??.???? *)
(*                                                                            *)
(* Version     : see uclickomania.pas                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : <Module_description>                                         *)
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
(* History     : 0.01 - 0.03: Initial version / unkbown                       *)
(*               0.04 : release auf Github                                    *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

{.$DEFINE DebuggMode}

Uses
  dglopengl,
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Menus, Buttons, ComCtrls,
  OpenGlcontext,
  lclintf,
  lcltype,
  uopengl_graphikengine // Zum Laden von Graphiken
  , uopengl_spriteengine
  , uclickomania;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    OpenDialog1: TOpenDialog;
    OpenGLControl1: TOpenGLControl;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure MenuItem1Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
    Procedure OpenGLControl1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure OpenGLControl1Paint(Sender: TObject);
    Procedure OpenGLControl1Resize(Sender: TObject);
    Procedure SpeedButton1Click(Sender: TObject);
    Procedure SpeedButton2Click(Sender: TObject);
    Procedure SpeedButton3Click(Sender: TObject);
    Procedure SpeedButton4Click(Sender: TObject);
    Procedure SpeedButton5Click(Sender: TObject);
    Procedure SpeedButton7Click(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Function Callback(Sender: TObject; MetaData: Pointer): Boolean;
    Procedure Go2d();
    Procedure Exit2d();
    Procedure CenterWindow;
  End;

Var
  Form1: TForm1;
  Initialized: Boolean = false; // Wenn True dann ist OpenGL initialisiert
  ConfigFile: String;


Implementation

{$R *.lfm}

{$I clickomania.inc}

Uses Unit2, unit3, unit4, unit5, unit6, unit7;

{ TForm1 }

Var
  allowcnt: Integer = 0;

  // Setzt die Größe des Formulares in Abhängigkeit der Spiefeldgröße..

Procedure SetFormSize;
Begin
  // Das Forumlar
  form1.Width := FieldColumns * StonesWidth + 10;
  form1.height := FieldRows * StonesHeight + 67;
  // das OpenGL Widget
  form1.OpenGLControl1.Width := FieldColumns * StonesWidth;
  form1.OpenGLControl1.Height := FieldRows * StonesHeight;
  // resize verhindern
  form1.Constraints.MinWidth := FieldColumns * StonesWidth + 10;
  form1.Constraints.MaxWidth := FieldColumns * StonesWidth + 10;
  form1.Constraints.MinHeight := FieldRows * StonesHeight + 67;
  form1.Constraints.MaxHeight := FieldRows * StonesHeight + 67;
End;

Function CoordIsDeleteable(i, j: Integer): Boolean;
Begin
  result := false;
  If i > 0 Then
    If Field[i, j] = field[i - 1, j] Then
      result := true;
  If i < FieldColumns - 1 Then
    If Field[i, j] = field[i + 1, j] Then
      result := true;
  If j > 0 Then
    If Field[i, j] = field[i, j - 1] Then
      result := true;
  If j < FieldRows - 1 Then
    If Field[i, j] = field[i, j + 1] Then
      result := true;
  // Die Spezial Steine
  If (field[i, j] = SpezialStoneBomb) Or
    (field[i, j] = SpezialStoneRocketUp) Or
    (field[i, j] = SpezialStoneRocketDown) Or
    (field[i, j] = SpezialStoneRocketLeft) Or
    (field[i, j] = SpezialStoneRocketRight) Then
    result := true;
End;

Procedure UpdateVLCInfo(CanEndGame: Boolean = True);
Var
  i, j, cc, gc, c, jj, ii: Integer;
  procent: single;
Begin
  gc := 0;
  c := 0;
  For j := 0 To FieldRows - 1 Do
    For i := 0 To FieldColumns - 1 Do
      cntField[i, j] := False;
  For j := 0 To FieldRows - 1 Do
    For i := 0 To FieldColumns - 1 Do Begin
      // Zählen der Gessamt im Spiel vorkommenden Steine
      If field[i, j] >= 0 Then Begin
        inc(gc);
        // Zählen der Steine die auf Jeden Fall noch gelöscht werden können
        // Das muss leider mit dem cntField gemacht, werden, damit doppelt löschbare Steine nicht doppelt gezählt werden.
        If CoordIsDeleteable(i, j) Then cntfield[i, j] := True;
        Case field[i, j] Of
          SpezialStoneRocketUp: Begin
              For jj := j - 1 Downto 0 Do
                If (Field[i, jj] >= 0) Then cntfield[i, jj] := True;
            End;
          SpezialStoneRocketDown: Begin
              For jj := j + 1 To FieldRows - 1 Do
                If (Field[i, jj] >= 0) Then cntfield[i, jj] := True;
            End;
          SpezialStoneRocketLeft: Begin
              For ii := i - 1 Downto 0 Do
                If (Field[ii, j] >= 0) Then cntfield[ii, j] := True;
            End;
          SpezialStoneRocketRight: Begin
              For ii := i + 1 To FieldColumns - 1 Do
                If (Field[ii, j] >= 0) Then cntfield[ii, j] := True;
            End;
          SpezialStoneBomb: Begin
              For ii := i - 1 To i + 1 Do
                For jj := j - 1 To j + 1 Do Begin
                  If (ii >= 0) And (ii < FieldColumns) And
                    (jj >= 0) And (jj < FieldRows) Then
                    If (Field[ii, jj] >= 0) Then cntfield[ii, jj] := True;
                End;
            End;
        End;
      End;
    End;
  For j := 0 To FieldRows - 1 Do
    For i := 0 To FieldColumns - 1 Do
      If cntField[i, j] Then inc(c);
  cc := FieldColumns * FieldRows;
  // Anzeige der Ursprünglich im Spiel Vorhandenen Steine
  Form1.StatusBar1.Panels[0].Text := inttostr(cc);
  // Anzeige der noch exisitierenden Steine
  Form1.StatusBar1.Panels[1].Text := inttostr(gc);
  // Anzeige der Steine, welche noch auf jeden Fall gelöscht werden können
  If ScoreMode = smLow Then Begin
    Form1.StatusBar1.Panels[2].Text := inttostr(c);
    Form1.StatusBar1.Hint := '1. total stone count at beginning' + LineEnding +
      '2. amount off stones still in game' + LineEnding +
      '3. stones that can be clearad';
  End
  Else Begin
    Form1.StatusBar1.Panels[2].Text := inttostr(GameScores);
    Form1.StatusBar1.Hint := '1. total stone count at beginning' + LineEnding +
      '2. amount off stones still in game' + LineEnding +
      '3. game scores';
  End;
  // Anzeige von "Info" Texten
  If cc = gc Then
    // Beim Spielstart
    Form1.StatusBar1.Panels[3].Text := 'Please go ahead!'
  Else If (c = 0) And (gc = 0) Then
    // Wenn das Spiel gewonnen ist
    Form1.StatusBar1.Panels[3].Text := 'You''ve won the game!'
  Else If (c = 0) Then
    // Das Spiel wurde "verloren"
    Form1.StatusBar1.Panels[3].Text := 'The game''s over...'
  Else
    // Normal im Spiel
    Form1.StatusBar1.Panels[3].Text := '';
  // Wenn es keine Löschbaren Steine mehr gibt.
  If c = 0 Then Begin
    ClearUndo;
    // Wenn ein Fertiges Spiel Geladen wurde, könnte man sonst sich in die Highscore eintragen.
    If CanEndGame Then Begin
      procent := gc / cc;
      If (procent < 0.1) And (ScoreMode = smHigh) Then Begin
        If Procent = 0 Then
          application.MessageBox(pchar('You''ve won the game, this gives ' + inttostr(round(GameScores * (0.1 - procent))) + ' extra points.'), 'Info', MB_ICONINFORMATION Or MB_OK)
        Else
          application.MessageBox(pchar('You''ve  deletet more than ' + floattostrf(100 - Procent * 100, fffixed, 7, 2) + '% of the stones, this gives ' + inttostr(round(GameScores * (0.1 - procent))) + ' extra points.'), 'Info', MB_ICONINFORMATION Or MB_OK);
        GameScores := GameScores + round(GameScores * (0.1 - procent));
        Form1.StatusBar1.Panels[2].Text := inttostr(GameScores);
      End;
      form3.showmodal;
    End;
  End;
End;

// Zum finden des Monitors auf dem sich die Maus befindet

Function PointInRect(P: TPoint; {classes.} R: TRect): boolean;
Var
  t: Integer;
Begin
  If r.left > r.right Then Begin
    t := r.left;
    r.left := r.right;
    r.right := t;
  End;
  If r.top > r.bottom Then Begin
    t := r.Bottom;
    r.bottom := r.top;
    r.top := t;
  End;
  result := (r.left <= p.x) And (r.right >= p.x) And
    (r.top <= p.y) And (r.bottom >= p.y);
End;

Procedure TForm1.CenterWindow;
{$IFNDEF WINDOWS}
Var
  i: Integer;
  r: classes.Trect;
{$ENDIF}
Begin
  (*
    Wurde die größe des Formulares geändert setzen wir es zentriert auf den Aktiven Monitor
    Bei einem Multimonitorsystem wollen wir die Anwendung immer da starten wo der Mauscursor ist.
  *)
{$IFNDEF WINDOWS}
  If screen.MonitorCount <> 1 Then Begin
    For i := 0 To screen.MonitorCount - 1 Do Begin
      r := screen.Monitors[i].BoundsRect;
      If PointInRect(Mouse.CursorPos, r) Then Begin
        left := (screen.Monitors[i].width - form1.width) Div 2 + screen.Monitors[i].BoundsRect.left;
        top := (screen.Monitors[i].height - form1.height) Div 2 + screen.Monitors[i].BoundsRect.top;
        break;
      End;
    End;
  End
  Else Begin
    left := (screen.width - form1.width) Div 2;
    top := (screen.height - form1.height) Div 2;
  End;
{$ELSE}
  left := (screen.width - form1.width) Div 2;
  top := (screen.height - form1.height) Div 2;
{$ENDIF}
End;

Procedure TForm1.Go2d;
Begin
  glMatrixMode(GL_PROJECTION);
  glPushMatrix(); // Store The Projection Matrix
  glLoadIdentity(); // Reset The Projection Matrix
  //  glOrtho(0, 640, 0, 480, -1, 1); // Set Up An Ortho Screen
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
  (*
  Man bedenke, jedesmal wenn der Renderingcontext neu erstellt wird, müssen sämtliche Graphiken neu Geladen werden.
  Bei Nutzung der TOpenGLGraphikengine, bedeutet dies, das hier ein clear durchgeführt werden mus !!
  *)
  glenable(GL_TEXTURE_2D); // Texturen
  OpenGL_GraphikEngine.clear;
  bombtex := OpenGL_GraphikEngine.LoadAlphaGraphik(AppPath + 'GFX' + PathDelim + 'bomb.png');
  rockettex := OpenGL_GraphikEngine.LoadAlphaGraphik(AppPath + 'GFX' + PathDelim + 'rocket.png');
  Stonetex := OpenGL_GraphikEngine.LoadAlphaGraphik(AppPath + 'GFX' + PathDelim + 'stone.png');
  OpenGL_SpriteEngine.Clear;
  ExpSprite := OpenGL_SpriteEngine.AddSprite(
    OpenGL_GraphikEngine.LoadAlphaGraphik(AppPath + 'GFX' + PathDelim + 'explosion.png'), // Textur
    'Explosion', // Name des Sprites
    true, // Transparent
    Frect(0, 0, 1, 1), // Nutzrect auf dem Bild
    1, 1, // Breite, Höhe beim Rendern
    4, 4, // Anzahl der Frames Pro Zeile und Spalte
    0, 16, // Frame Start und Ende
    10, // Delay Pro bild
    // 100, // Delay Pro bild -- Debugg
    @Callback, Nil
    );
  // Der Anwendung erlauben zu Rendern.
  Initialized := True;
  OpenGLControl1Resize(Nil);
  // Nach dem Laden der Config Datei setzen wir alle entsprechenden Größen
  // und starten das 1. Spiel
  SpeedButton1Click(Nil);
  CenterWindow;
End;

Procedure TForm1.OpenGLControl1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  g: Integer;
Begin
  If (GameState = gsWaitForPlayerInput) Then Begin
    x := x Div StonesWidth;
    y := y Div StonesHeight;
    // Das Scoring soll immer angezeigt werden, wenn es gewünscht ist.
    form5.label2.caption := inttostr(x + 1) + ' x ' + inttostr(y + 1);
    g := ScoreCoord(x, y);
    form5.label5.caption := inttostr(g);
    If (g = 0) Or (g = 1) Then Begin
      form5.label6.caption := '0';
    End
    Else Begin
      form5.label6.caption := inttostr(score(g));
    End;
    If (ssleft In shift) Then Begin
      // Schnellcheck ob die Koordinate gelöscht werden kann.
      If CoordCanBeDeleted(x, y) Then Begin
        If Trackthegame Then Begin
          Push;
        End;
        If EnableAnimations Then Begin
          DeleteCoord(x, y);
          OpenGL_SpriteEngine.ResetSprite(ExpSprite);
          GameState := gsAnimExplosion;
        End
        Else Begin
          DeleteCoord(x, y);
          DropStonesInField;
          MoveStonesLeftInField;
          ClearAktTrigger;
          UpdateVLCInfo;
        End;
      End;
    End;
    // Anzeigen der Scores für diese Koordinate
    If (ssright In shift) Then Begin
      If Not form5.visible Then Begin
        form5.show;
        form5.left := form1.left + form1.width + 8;
        form5.top := form1.top;
      End;
    End;
  End;
End;

Procedure TForm1.OpenGLControl1Paint(Sender: TObject);
Begin
  If Not Initialized Then Exit;
  // Render Szene
  glClearColor(0.5, 0.5, 0.5, 0.0);
  glClear(GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity();
  go2d;
  glColor4f(1, 1, 1, 1);
  glBindTexture(GL_TEXTURE_2d, 0);
  // Hintergrundbild
  If Backstyle <> bsgray Then Begin
    glcolor3f(1, 1, 1);
    RenderQuad(point(0, 0), point(OpenGLControl1.Width, OpenGLControl1.Height), 180, false, Backtex);
    glBindTexture(GL_TEXTURE_2d, 0);
  End;
  glcolor3f(1, 1, 1);
  Case GameState Of
    gsWaitForPlayerInput: Begin
        // Das Spielfeld
        RenderField;
      End;
    gsAnimExplosion: Begin
        // Das Spielfeld
        RenderExplosionField;
        // Das Umschalten von Explosion zu DropStones geschieht mittels einer Callback aus der Sprite Enginge
      End;
    gsAnimDropStones: Begin
        RenderDropStoneField;
        // Alle Steine sind Runter gefallen, umschalten zum nach "Links Schrumpfen"
        If GetTickCount - DropStartTime > DropStoneDelay Then Begin
          GameState := gsAnimMoveStonesLeft;
          DropStonesInField;
          DropStartTime := GetTickCount;
        End;
      End;
    gsAnimMoveStonesLeft: Begin
        RenderMoveStonesLeft;
        // Das Spielfeld ist "Geschrumpft" es kann weiter gehen
        If GetTickCount - DropStartTime > DropStoneDelay Then Begin
          GameState := gsWaitForPlayerInput;
          MoveStonesLeftInField;
          ClearAktTrigger;
          UpdateVLCInfo;
        End;
      End;
  End;
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

Procedure TForm1.SpeedButton1Click(Sender: TObject);
Begin
  SetFormSize;
  initGame;
  UpdateVLCInfo;
End;

Procedure TForm1.SpeedButton2Click(Sender: TObject);
Begin
  If Trackthegame Then Begin
    If Pop Then
      UpdateVLCInfo;
  End;
End;

Procedure TForm1.SpeedButton3Click(Sender: TObject);
Var
  l: Tlabel;
  i: Integer;
  r: THighScoreResult;
Begin
  r := Highscore.ShowHighscores(high(field) + 1, high(field[0]) + 1, UsedColorNumbers, UsedSpezials, ScoreMode);
  For i := 5 To 14 Do Begin
    l := Tlabel(form4.FindComponent('Label' + inttostr(i + 10)));
    l.Caption := r.Results[i - 5].Name;
    l := Tlabel(form4.FindComponent('Label' + inttostr(i + 20)));
    l.Caption := inttostr(r.Results[i - 5].Points);
  End;
  If ScoreMode = smLow Then
    form4.Label3.Caption := 'average stones left'
  Else
    form4.Label3.Caption := 'average high scores';
  form4.Label2.Caption := inttostr(r.TotalGames);
  form4.Label4.Caption := inttostr(r.averagePoints);
  form4.Label36.Caption := inttostr(UsedColorNumbers);
  form4.label38.caption := inttostr(high(field) + 1) + ' x ' + inttostr(high(field[0]) + 1);
  form4.OrientLabels(Nil);
  If assigned(sender) Then
    form4.Showmodal;
End;

Procedure TForm1.SpeedButton4Click(Sender: TObject);
Var
  i: Integer;
Begin
  // Karteikarte Field
  form2.SpinEdit1.value := FieldRows;
  form2.SpinEdit2.value := FieldColumns;
  form2.SpinEdit3.value := StonesHeight;
  form2.SpinEdit4.value := StonesWidth;
  form2.SpinEdit3Change(Nil);

  // Karteikarte Stones
  form2.SpinEdit5.value := ColorNumbers;
  form2.CheckBox1.Checked := F_XsTextured;
  form2.ScrollBar1.Enabled := F_XsTextured;
  form2.ScrollBar1.Position := round(AlphaF_XsTextured * 100);
  form2.CheckBox2.Checked := F_XsVoyer;
  form2.CheckBox3.Checked := f_XsTracelines;
  form2.CheckBox4.Checked := SoundsPlaysounds;
  For i := 0 To high(StoneColors) Do Begin
    cls[i] := TRGBToColor(StoneColors[i]);
  End;

  // Karteikarte Background
  Case Backstyle Of
    bsDefault: Form2.RadioButton3.checked := true;
    bsFromPicture: Form2.RadioButton1.checked := true;
    bsFromDirectory: Form2.RadioButton2.checked := true;
  End;
  If length(DirectPicture) = 0 Then Begin
    form2.label15.caption := '[none selected]';
  End
  Else Begin
    form2.label15.caption := ExtractFileName(DirectPicture);
  End;
  backdirpic := DirectPicture;
  If length(DirectDir) = 0 Then Begin
    form2.label16.caption := '[none selected]';
  End
  Else Begin
    form2.label16.caption := DirectDir;
  End;
  backdirDir := DirectDir;

  // Karteikarte Actions
  form2.CheckBox6.checked := EnableSpecialStones;
  form2.CheckBox7.checked := EnableAnimations;
  form2.CheckBox8.checked := EnableAutoDropCountDown;
  form2.SpinEdit6.Value := Duration;

  // Karteikarte Special
  form2.CheckBox5.checked := Trackthegame;
  form2.RadioButton4.checked := ScoreMode = smLow;
  form2.RadioButton5.checked := ScoreMode = smHigh;
  form2.button7.enabled := ScoreMode = smHigh;

  //Optionen Starten
  If assigned(sender) Then Begin
    veraendert := false;
    form2.showmodal;
    If veraendert Then
      SpeedButton1Click(Nil);
  End;
End;

Procedure TForm1.SpeedButton5Click(Sender: TObject);
Begin
  // Sender = nil dann kommen wir von den Highscores
  form7.showmodal;
End;

Procedure TForm1.SpeedButton7Click(Sender: TObject);
Begin
  PopupMenu1.PopUp();
End;

// Dann bei OnCreate des Formulars

Procedure TForm1.FormCreate(Sender: TObject);
Var
  f: TFileStream;
  s: Single;
Begin
  CenterWindow; // Ausgelagert, da es in OpenGLControl1MakeCurrent nochmals gemacht werden muss
  (*

    Todo :

    - Alle in den Optionen Enabled = false Einstellungen fehlen noch.

    - Speedbutton6 ???

    Bekannte Bugs :

  *)
{$IFNDEF DebuggMode}
  Randomize;
{$ENDIF}
  Highscore.LoadFromFile(AppPath + 'highscores.dat');
  OpenDialog1.InitialDir := ExtractFileDir(paramstr(0));
  SaveDialog1.InitialDir := ExtractFileDir(paramstr(0));
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
  Timer1.Interval := 40;
  caption := 'Clickomania ver.: ' + floattostrf(version, fffixed, 7, 2);
  ConfigFile := IncludeTrailingPathDelimiter(ExtractFilePath(paramstr(0))) + 'user.cfg';
  // Laden Der CFG-File
  If FileExists(ConfigFile) Then Begin
    f := TFileStream.create(ConfigFile, fmOpenread);
    s := 0;
    f.read(s, SizeOf(s));
    If s <> version Then Begin
      f.Free;
      application.MessageBox('Config file has invalid version, will be deleted.', 'Error', MB_ICONWARNING Or MB_OK);
      If Not DeleteFile(ConfigFile) Then
        application.MessageBox('Could not delete config file.', 'Error', MB_ICONWARNING Or MB_OK);
      exit;
    End;
    // Karteikarte Field
    f.read(FieldColumns, sizeof(FieldColumns));
    f.read(FieldRows, sizeof(FieldRows));
    f.read(StonesHeight, sizeof(StonesHeight));
    f.read(StonesWidth, sizeof(StonesWidth));

    // Karteikarte Stones
    f.read(ColorNumbers, sizeof(ColorNumbers));
    f.read(F_XsTextured, sizeof(F_XsTextured));
    f.read(AlphaF_XsTextured, sizeof(AlphaF_XsTextured));
    f.read(F_XsVoyer, sizeof(F_XsVoyer));
    f.read(f_XsTracelines, sizeof(f_XsTracelines));
    f.read(SoundsPlaysounds, sizeof(SoundsPlaysounds));
    f.read(StoneColors, sizeof(StoneColors));

    // Karteikarte Background
    f.read(Backstyle, sizeof(TBackgroundstyle));
    DirectPicture := StringFromStream(f);
    DirectDir := StringFromStream(f);

    // Karteikarte Actions
    f.read(EnableSpecialStones, sizeof(EnableSpecialStones));
    f.read(EnableAnimations, sizeof(EnableAnimations));
    f.read(EnableAutoDropCountDown, sizeof(EnableAutoDropCountDown));
    f.read(Duration, sizeof(Duration));

    // Karteikarte Special
    f.read(Trackthegame, sizeof(Trackthegame));
    f.read(ScoreMode, sizeof(ScoreMode));

    f.free;
  End;
  SpeedButton2.Enabled := Trackthegame;
End;

Procedure TForm1.MenuItem1Click(Sender: TObject);
Var
  f: TFilestream;
  k, i, j: Integer;
  s: Single;
Begin
  If OpenDialog1.execute Then Begin
    f := TFileStream.Create(OpenDialog1.FileName, fmopenread);
    s := 0;
    f.read(s, sizeof(s));
    If s <> Version Then Begin
      Application.MessageBox('Invalid Level Version', 'Error', MB_ICONWarning Or MB_OK);
      f.free;
      exit;
    End;
    i := 0;
    j := 0;
    //    i := High(field) + 1;
    f.read(i, sizeof(i));
    //    j := High(field[0]) + 1;
    f.read(j, sizeof(j));
    setlength(field, i, j);
    For j := 0 To High(field[0]) Do
      For i := 0 To High(field) Do
        f.read(field[i, j], sizeof(field[i, j]));
    // Speichern der High Score Punkte
    k := 0;
    f.read(k, sizeof(k));
    i := $0AAAAAAA;
    GameScores := k Xor i;
    // Speichern ob Spezialsteine Benutzt wurden
    f.read(UsedSpezials, sizeof(UsedSpezials));
    // Speichern der Orginal Anzahl an Farben
    f.read(UsedColorNumbers, sizeof(UsedColorNumbers));
    f.free;
    UpdateVLCInfo(false);
  End;
End;

Procedure TForm1.MenuItem2Click(Sender: TObject);
Var
  f: TFilestream;
  k, i, j: Integer;
Begin
  If GameState = gsWaitForPlayerInput Then Begin
    If SaveDialog1.Execute Then Begin
      f := TFileStream.Create(SaveDialog1.FileName, fmcreate Or fmopenwrite);
      f.write(Version, sizeof(Version));
      // Speichern der Spielfeldgröße
      i := High(field) + 1;
      f.Write(i, sizeof(i));
      j := High(field[0]) + 1;
      f.Write(j, sizeof(j));
      For j := 0 To High(field[0]) Do
        For i := 0 To High(field) Do
          f.write(field[i, j], sizeof(field[i, j]));
      // Speichern der High Score Punkte
      k := $0AAAAAAA;
      k := GameScores Xor k;
      f.Write(k, sizeof(k));
      // Speichern ob Spezialsteine Benutzt wurden
      f.Write(UsedSpezials, sizeof(UsedSpezials));
      // Speichern der Orginal Anzahl an Farben
      f.Write(UsedColorNumbers, sizeof(UsedColorNumbers));
      f.free;
    End;
  End;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  Initialized := false;
  Highscore.SaveToFile(AppPath + 'highscores.dat');
  setlength(field, 0, 0);
  setlength(cntField, 0, 0);
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
      p := gluErrorString(i);
      showmessage('OpenGL Error (' + inttostr(i) + ') occured.' + LineEnding + LineEnding +
        'OpenGL Message : "' + p + '"' + LineEnding + LineEnding +
        'Applikation will be terminated.');
      Timer1.Enabled := false;
      close;
    End;
{$ENDIF}
  End;
End;

Function TForm1.Callback(Sender: TObject; MetaData: Pointer): Boolean;
Begin
  GameState := gsAnimDropStones;
  DropStartTime := gettickcount;
  result := true;
End;

End.

