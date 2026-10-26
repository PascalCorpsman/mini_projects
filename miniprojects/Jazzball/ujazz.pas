(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Jazz                                                  *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit ujazz;

{$MODE objfpc}{$H+}

Interface

Uses
  dialogs, controls,
  Classes, SysUtils, highscoreengine, dglopengl, uopengl_graphikengine,
  uopengl_spriteengine;

Type

  TGameState = (gmIngame, gmWait);

  Tlevel = Record
    Count, points, time, live, flaeche: integer;
    data: Array Of Array Of shortint;
  End;

  { Tball }

  Tball = Class
  private
    DirP: Array[0..3] Of Record
      Sprite: integer;
      FrameOffset: integer;
    End;
    Dir: integer;
    tot: boolean;
  public
    x: Single;
    y: Single;
    visible: Boolean;
    Constructor create(Number: Integer); reintroduce;
    Procedure Render;
    Procedure docollision;
    Procedure doMove;
    Procedure Zusammenstosss;
  End;

Const
  StoneWidth = 20;
  StoneHeight = 20;
  Version: Single = 1.2;

Var
  // Variablen die der User ändern darf
  teiler: integer = 5; // Alle 5 Level, bekommt man ein Leben mehr
  BallRotatespeed: integer = 800; // Zeit in Ms bis ein Ball eine Rotation geschafft hat
  Ballspeed: Single = 1.5; // Geschwindigkeit des Balles
  minball: integer = 1; // Mindest Ball Anzahl
  Windowscursor: Boolean = false; // Wenn True werden die Standard Cursor verwendet
  sinnlos: Boolean = false; // Abschalten aller Beschränkungen und einfügen der Wände durch Linksklick
  ballcollision: Boolean = false; // Noch nicht Implementiert
  sprung: integer = 75; // Prozent ab der das nächste Level nach ablauf der Zeit Akzeptiert wird
  barspeed: integer = 100; // Geschwindigkeit der Progressbalken
  autosprung: Integer = 95; // Prozent ab der automatisch das nächste Level gewählt wird
  Bezeichnung: String = 'Player'; // Spieler Name
  maxtime: integer = 120; // Zeit in Sekunden Pro Level
  ballspeedinc2: Single = 0; // Bälle werden Pro Level Schneller
  screenwidth: integer = 480; // Breite des Fensters
  screenheight: Integer = 424; // Höhe des Fensters

  // Pointer zu diversen Texturen
  BottomP: TGraphikItem;
  wallP: TGraphikItem;
  BarvollP: TGraphikItem;
  BarR_LP: TGraphikItem;
  BarL_RP: TGraphikItem;
  BarO_UP: TGraphikItem;
  BarU_OP: TGraphikItem;
  LoRuP: TGraphikItem;
  LuRoP: TGraphikItem;
  RuLoP: TGraphikItem;
  RoLuP: TGraphikItem;
  WaagrechtP: TGraphikItem;
  SenkrechtP: TGraphikItem;

  // Noch nicht definierte Variablen
  GameState: TGameState = gmWait;
  Amousepos: TPoint; // Speicherung der Mausposition für Rendering des Cursors

  ballspeedinc: Real;
  Level: Tlevel;
  Ball: Tball;
  Highscore: Thighscore;
  bartime, Uhrtime: Longword;
  Mousedir, BarOnProgress: boolean;
  Balls: Array Of TBall = Nil; // Alle Bälle

Procedure Readini; // Einlesen der Konfigurationsdatei
Procedure Writeini; // Einlesen der Konfigurationsdatei
Procedure NaechstesLevel; // Initialisiert das Nächste Level
Procedure RenderGame; // Rendert alles was mit OpenGL zu tun hat
Procedure LoadGraphiks; // Lädt alle fürs Spiel notwendigen Graphiken
Procedure BarProgress; // Bewegt die vom User geclickten Balken
Procedure Spielende; // Prüft ob ein Spielende ist
Procedure MoveBalls; // Bewegt die Bälle

Implementation

Uses unit1, uvectormath;

Function extractReal(s: String): real;
Var
  x: integer;
Begin
  If Length(s) = 0 Then Begin
    result := 0;
    exit;
  End;
  For x := length(s) Downto 1 Do
    If (s[x] = ' ') Or (s[x] = '=') Then Begin
      s := copy(s, x + 1, length(s));
      s := trim(s);
      break;
    End;
  DefaultFormatSettings.DecimalSeparator := '.';
  result := strtofloat(s);
End;

Function extractstring(s: String): String;
Var
  x: integer;
Begin
  For x := length(s) Downto 1 Do
    If (s[x] = ' ') Or (s[x] = '=') Then Begin
      s := copy(s, x + 1, length(s));
      s := trim(s);
      break;
    End;
  result := s;
End;

Function extractinteger(s: String): integer;
Var
  x: integer;
Begin
  For x := length(s) Downto 1 Do
    If (s[x] = ' ') Or (s[x] = '=') Then Begin
      s := copy(s, x + 1, length(s));
      s := trim(s);
      break;
    End;
  result := strtoint(s);
End;

Procedure Writeini; // Schreiben der Konfigurationsdatei
Var
  f: Textfile;
Begin
  If Fileexists(IncludeTrailingPathDelimiter(extractfilepath(paramstr(0))) + 'config.ini') Then Begin
    assignfile(f, IncludeTrailingPathDelimiter(extractfilepath(paramstr(0))) + 'config.ini');
    rewrite(f);
    writeln(f, 'WindowWidth = ' + inttostr(screenwidth));
    writeln(f, 'WindowHeight = ' + inttostr(screenheight));
    writeln(f, 'BallRotateSpeed = ' + inttostr(BallRotatespeed));
    writeln(f, 'BallSpeed = ' + floattostr(Ballspeed));
    writeln(f, 'Divider = ' + inttostr(Teiler));
    writeln(f, 'MinBall = ' + inttostr(minBall));
    writeln(f, 'WindowsCursor = ' + inttostr(ord(Windowscursor)));
    writeln(f, 'Senseless = ' + inttostr(ord(sinnlos)));
    writeln(f, 'BallCollision = ' + inttostr(ord(ballcollision)));
    writeln(f, 'MaxTime = ' + inttostr(maxtime));
    writeln(f, 'Jump = ' + inttostr(sprung));
    writeln(f, 'BarSpeed = ' + inttostr(barspeed));
    writeln(f, 'AutoJump = ' + inttostr(autosprung));
    writeln(f, 'PlayerName = ' + Bezeichnung);
    writeln(f, 'BallSpeedInc = ' + floattostr(ballspeedinc2));
    closefile(f);
  End;
End;

Procedure Readini; // Einlesen der Konfigurationsdatei
Var
  f: Textfile;
  s: String;
Begin
  If Fileexists(IncludeTrailingPathDelimiter(extractfilepath(paramstr(0))) + 'config.ini') Then Begin
    assignfile(f, IncludeTrailingPathDelimiter(extractfilepath(paramstr(0))) + 'config.ini');
    reset(f);
    readln(f, s);
    screenwidth := extractinteger(s);
    readln(f, s);
    screenheight := extractinteger(s);
    readln(f, s);
    BallRotatespeed := extractinteger(s);
    readln(f, s);
    Ballspeed := extractReal(s);
    readln(f, s);
    Teiler := extractinteger(s);
    readln(f, s);
    minBall := extractinteger(s);
    readln(f, s);
    Windowscursor := odd(extractinteger(s));
    readln(f, s);
    sinnlos := odd(extractinteger(s));
    readln(f, s);
    ballcollision := odd(extractinteger(s));
    readln(f, s);
    maxtime := (extractinteger(s));
    readln(f, s);
    sprung := (extractinteger(s));
    readln(f, s);
    barspeed := (extractinteger(s));
    readln(f, s);
    autosprung := (extractinteger(s));
    readln(f, s);
    Bezeichnung := extractstring(s);
    readln(f, s);
    ballspeedinc2 := extractReal(s);
    closefile(f);
  End;
End;

Procedure ArrayScalierenLoeschen;
Var
  x, y: integer;
Begin
  // Anpassen der Fenstergröße
  x := (screenwidth - 80) Div 20;
  y := (screenheight - 24) Div 20;
  form1.OpenGLControl1.Width := StoneWidth * x;
  form1.OpenGLControl1.height := StoneHeight * y;
  form1.Width := form1.OpenGLControl1.Width + 80;
  form1.height := form1.OpenGLControl1.height + 24;
  form1.Constraints.MinWidth := form1.OpenGLControl1.Width + 80;
  form1.Constraints.MaxWidth := form1.OpenGLControl1.Width + 80;
  form1.Constraints.MaxHeight := form1.OpenGLControl1.height + 24;
  form1.Constraints.MinHeight := form1.OpenGLControl1.height + 24;
  setlength(level.data, x, y);
  For x := 0 To high(level.data) Do
    For y := 0 To high(level.data[1]) Do
      level.data[x, y] := 0;
  For x := 0 To high(level.data) Do Begin
    level.data[x, 0] := 1;
    level.data[x, high(level.data[1])] := 1;
  End;
  For x := 0 To high(level.data[1]) Do Begin
    level.data[0, x] := 1;
    level.data[high(level.data), x] := 1;
  End;
End;

Procedure naechsteslevel;
Var
  i: integer;
Begin
  //  With form1 Do Begin
  If sinnlos Then Begin
    form1.image1.visible := false;
    form1.image2.visible := false;
    form1.label1.Visible := false;
    form1.label2.Visible := false;
    form1.label3.Visible := false;
    form1.label4.Visible := false;
    form1.label5.Visible := false;
  End
  Else Begin
    form1.label1.Visible := true;
    form1.label2.Visible := true;
    form1.label3.Visible := true;
    form1.label4.Visible := true;
    form1.label5.Visible := true;
    If Not Windowscursor Then Begin
      form1.OpenGLControl1.Cursor := crnone;
      form1.image1.visible := false;
      form1.image2.visible := false;
    End
    Else Begin
      form1.OpenGLControl1.Cursor := crdefault;
      form1.image1.visible := true;
      form1.image2.visible := false;
    End;
  End;
  BarOnProgress := false;
  mousedir := true;
  level.time := maxtime;
  form1.label2.caption := 'Time : ' + inttostr(level.time);
  level.live := ((level.Count + minball) Div teiler) + 1;
  form1.label4.caption := 'Lives : ' + inttostr(level.live);
  level.flaeche := 0;
  form1.label3.caption := 'Area : ' + inttostr(level.flaeche);
  OpenGL_SpriteEngine.Clear;
  ArrayScalierenLoeschen;
  For i := 0 To High(balls) Do
    balls[i].free;
  setlength(balls, (minball + level.count));
  For i := 0 To (minball + level.count) - 1 Do Begin // anzahl der Bälle
    balls[i] := Tball.create(i);
    balls[i].x := random(round((high(level.data) - 2) * 20)) + 20;
    balls[i].y := random(round((high(level.data[1]) - 2) * 20)) + 20;
  End;
  GameState := gmIngame;
End;

Procedure RenderGame;
Var
  xoff, yoff: Single;
  x1, y1: integer;
Begin
  // Rendern des Spielfeldes Ansich
  xoff := StoneWidth / 2;

  For x1 := 0 To high(level.data) Do Begin
    yoff := StoneHeight / 2;
    For y1 := 0 To high(level.data[1]) Do Begin
      Case level.data[x1, y1] Of
        0: RenderQuad(v2(xoff, yoff), 0, StoneWidth, StoneHeight, 0, BottomP); // Boden
        1: RenderQuad(v2(xoff, yoff), 0, StoneWidth, StoneHeight, 0, wallP); // Wand
        2: RenderQuad(v2(xoff, yoff), 0, StoneWidth, StoneHeight, 0, BarvollP); // Barvoll
        3: RenderQuad(v2(xoff, yoff), 0, StoneWidth, StoneHeight, 0, BarR_LP); // Bar von Rechts nach links
        4: RenderQuad(v2(xoff, yoff), 0, StoneWidth, StoneHeight, 0, BarL_RP); // Bar von Links nach Rechts
        5: RenderQuad(v2(xoff, yoff), 0, StoneWidth, StoneHeight, 0, BarO_UP); // Bar von oben nach unten
        6: RenderQuad(v2(xoff, yoff), 0, StoneWidth, StoneHeight, 0, BarU_OP); // Bar von unten nach oben
        7: RenderQuad(v2(xoff, yoff), 0, StoneWidth, StoneHeight, 0, BarvollP); //
        8: RenderQuad(v2(xoff, yoff), 0, StoneWidth, StoneHeight, 0, BarvollP); //
        9: RenderQuad(v2(xoff, yoff), 0, StoneWidth, StoneHeight, 0, BarvollP); //
        10: RenderQuad(v2(xoff, yoff), 0, StoneWidth, StoneHeight, 0, BarvollP); //
      End;
      yoff := yoff + StoneHeight;
    End;
    xoff := xoff + StoneWidth;
  End;

  // Rendern der Bälle und ihrer Animationen
  For x1 := 0 To High(balls) Do
    Balls[x1].Render;
  // Rendern des Cursors
  If Not (sinnlos) Then Begin
    If Not (windowscursor) Then Begin
      xoff := Amousepos.x;
      yoff := Amousepos.y;
      If Mousedir Then // waagrecht
        RenderAlphaQuad(v2(xoff, yoff), 0, 32, 32, 0, waagrechtp)
      Else // senkrecht
        RenderAlphaQuad(v2(xoff, yoff), 0, 32, 32, 0, senkrechtp);
    End; // ende Mousedir
  End;
End;

Procedure tausche(Var v1, v2: integer);
Var
  x: integer;
Begin
  If V2 < v1 Then Begin
    x := v1;
    v1 := v2;
    v2 := x;
  End;
End;

Function realyrectangle(x1, y1, x2, y2: integer): boolean;
Var
  z: integer;
Begin
  result := false;
  tausche(x1, x2);
  tausche(y1, y2);
  For z := x1 To x2 Do Begin
    If Level.data[z, y1] = 0 Then exit;
    If Level.data[z, y2] = 0 Then exit;
  End;
  For z := y1 To y2 Do Begin
    If Level.data[x1, z] = 0 Then exit;
    If Level.data[x2, z] = 0 Then exit;
  End;
  result := true;
End;

Procedure BallsinRectanglefill(x1, y1, x2, y2: integer);
Var
  z, x, y: integer;
Begin
  tausche(x1, x2);
  tausche(y1, y2);
  // Wenn ein Ball im Rechteck ist wird hier ausgestiegen
  For z := 0 To high(balls) Do
    If balls[z].visible Then
      // Warum steht hier 10 + und nicht 20 + ???
      If (balls[z].x > ((x1 * StoneWidth))) And
        (balls[z].x < (10 + (x2 * StoneWidth))) And
        (balls[z].y > ((y1 * StoneHeight))) And
        (balls[z].y < (10 + (y2 * StoneHeight))) Then Begin
        exit;
      End;
  // Setzen der Level Fläche auf Wand
  For x := x1 To x2 Do
    For y := y1 To y2 Do
      level.data[x, y] := 1;
End;


//Mal mit einer art floodfill versuchen!!!

Procedure RechteckKI(Dir: byte; x, y: integer);
Var
  x1, y1, x2, y2: integer;
  xbet, ybet: integer;
  z1: integer;
Begin
  x1 := 0;
  y1 := 0;
  x2 := 0;
  y2 := 0;
  xbet := x;
  ybet := y;

  Case dir Of
    1: Begin // mal mit zusammenfassen von 1 und 2 versuchen!!
        x1 := xbet - 1;
        For z1 := xbet To high(level.data) - 1 Do Begin
          If level.data[z1, ybet] = 0 Then Begin
            x2 := z1 - 1;
            break;
          End;
          If level.data[z1, ybet] > 1 Then Begin
            exit;
          End;
          If z1 = high(level.data) - 1 Then x2 := z1 + 1;
        End;
        y2 := ybet;
        For z1 := ybet Downto 1 Do Begin
          If level.data[x1, z1] = 0 Then Begin
            y1 := z1 + 1;
            break;
          End;
          If level.data[x1 + 1, z1 - 1] = 1 Then Begin
            y1 := z1 - 1;
            break;
          End;
          If z1 = 1 Then y1 := 0;
        End;
        If realyrectangle(x1, y1, x2, y2) Then BallsinRectanglefill(x1, y1, x2, y2);
        For z1 := ybet Downto 1 Do Begin
          If level.data[x2, z1] = 0 Then Begin
            y1 := z1 + 1;
            break;
          End;
          If level.data[x2 - 1, z1 - 1] = 1 Then Begin
            y1 := z1 - 1;
            break;
          End;
          If z1 = 1 Then y1 := 0;
        End;
        If realyrectangle(x1, y1, x2, y2) Then BallsinRectanglefill(x1, y1, x2, y2);
        y1 := ybet;
        For z1 := ybet To high(level.data[1]) - 1 Do Begin
          If level.data[x1, z1] = 0 Then Begin
            y2 := z1 - 1;
            break;
          End;
          If level.data[x1 + 1, z1 + 1] = 1 Then Begin
            y2 := z1 + 1;
            break;
          End;
          If z1 = high(level.data[1]) - 1 Then y2 := z1 + 1;
        End;
        If realyrectangle(x1, y1, x2, y2) Then BallsinRectanglefill(x1, y1, x2, y2);
        y1 := ybet;
        For z1 := ybet To high(level.data[1]) - 1 Do Begin
          If level.data[x2, z1] = 0 Then Begin
            y2 := z1 - 1;
            break;
          End;
          If level.data[x2 - 1, z1 + 1] = 1 Then Begin
            y2 := z1 + 1;
            break;
          End;
          If z1 = high(level.data[1]) - 1 Then y2 := z1 + 1;
        End;
        If realyrectangle(x1, y1, x2, y2) Then BallsinRectanglefill(x1, y1, x2, y2);
      End;
    2: Begin
        x2 := xbet + 1;
        For z1 := xbet Downto 1 Do Begin
          If level.data[z1, ybet] = 0 Then Begin
            x1 := z1 + 1;
            break;
          End;
          If level.data[z1, ybet] > 1 Then Begin
            exit;
          End;
          If z1 = 1 Then x1 := 0;
        End;
        y2 := ybet;
        For z1 := ybet Downto 1 Do Begin
          If level.data[x1, z1] = 0 Then Begin
            y1 := z1 + 1;
            break;
          End;
          If level.data[x1 + 1, z1 - 1] = 1 Then Begin
            y1 := z1 - 1;
            break;
          End;
          If z1 = 1 Then y1 := 0;
        End;
        If realyrectangle(x1, y1, x2, y2) Then BallsinRectanglefill(x1, y1, x2, y2);
        For z1 := ybet Downto 1 Do Begin
          If level.data[x2, z1] = 0 Then Begin
            y1 := z1 + 1;
            break;
          End;
          If level.data[x2 - 1, z1 - 1] = 1 Then Begin
            y1 := z1 - 1;
            break;
          End;
          If z1 = 1 Then y1 := 0;
        End;
        If realyrectangle(x1, y1, x2, y2) Then BallsinRectanglefill(x1, y1, x2, y2);
        y1 := ybet;
        For z1 := ybet To high(level.data[1]) - 1 Do Begin
          If level.data[x1, z1] = 0 Then Begin
            y2 := z1 - 1;
            break;
          End;
          If level.data[x1 + 1, z1 + 1] = 1 Then Begin
            y2 := z1 + 1;
            break;
          End;
          If z1 = high(level.data[1]) - 1 Then y2 := z1 + 1;
        End;
        If realyrectangle(x1, y1, x2, y2) Then BallsinRectanglefill(x1, y1, x2, y2);
        y1 := ybet;
        For z1 := ybet To high(level.data[1]) - 1 Do Begin
          If level.data[x2, z1] = 0 Then Begin
            y2 := z1 - 1;
            break;
          End;
          If level.data[x2 - 1, z1 + 1] = 1 Then Begin
            y2 := z1 + 1;
            break;
          End;
          If z1 = high(level.data[1]) - 1 Then y2 := z1 + 1;
        End;
        If realyrectangle(x1, y1, x2, y2) Then BallsinRectanglefill(x1, y1, x2, y2);
      End;
    3: Begin
        y2 := ybet + 1;
        For z1 := ybet Downto 1 Do Begin
          If Level.data[xbet, z1] = 0 Then Begin
            y1 := z1 + 1;
            break;
          End;

          If Level.data[xbet, z1] > 1 Then Begin
            exit;
          End;
          If z1 = 1 Then y1 := 0;
        End;
        //
        x2 := xbet;
        For z1 := xbet Downto 1 Do Begin
          If Level.data[z1, y1] = 0 Then Begin
            x1 := z1 + 1;
            break;
          End;
          If Level.data[z1 - 1, y1 + 1] = 1 Then Begin
            x1 := z1 - 1;
            break;
          End;
          If z1 = 1 Then x1 := 0;
        End;
        If realyrectangle(x1, y1, x2, y2) Then BallsinRectanglefill(x1, y1, x2, y2);

        x2 := xbet;
        For z1 := xbet Downto 1 Do Begin
          If Level.data[z1, y2] = 0 Then Begin
            x1 := z1 + 1;
            break;
          End;
          If Level.data[z1 - 1, y2 - 1] = 1 Then Begin
            x1 := z1 - 1;
            break;
          End;
          If z1 = 1 Then x1 := 0;
        End;
        If realyrectangle(x1, y1, x2, y2) Then BallsinRectanglefill(x1, y1, x2, y2);

        x1 := xbet;
        For z1 := xbet To high(level.data) - 1 Do Begin
          If Level.data[z1, y1] = 0 Then Begin
            x2 := z1 - 1;
            break;
          End;
          If Level.data[z1 + 1, y1 + 1] = 1 Then Begin
            x2 := z1 + 1;
            break;
          End;
          If z1 = high(level.data) - 1 Then x2 := z1 + 1;
        End;
        If realyrectangle(x1, y1, x2, y2) Then BallsinRectanglefill(x1, y1, x2, y2);

        For z1 := xbet To high(level.data) - 1 Do Begin
          If Level.data[z1, y2] = 0 Then Begin
            x2 := z1 - 1;
            break;
          End;
          If Level.data[z1 + 1, y2 - 1] = 1 Then Begin
            x2 := z1 + 1;
            break;
          End;
          If z1 = high(level.data) - 1 Then x2 := z1 + 1;
        End;
        If realyrectangle(x1, y1, x2, y2) Then BallsinRectanglefill(x1, y1, x2, y2);

      End;
    4: Begin
        y1 := ybet - 1;
        For z1 := ybet To high(level.data[1]) - 1 Do Begin
          If Level.data[xbet, z1] = 0 Then Begin
            y2 := z1 - 1;
            break;
          End;

          If Level.data[xbet, z1] > 1 Then Begin
            exit;
          End;
          If z1 = high(level.data[1]) - 1 Then y2 := z1 + 1;
        End;
        x2 := xbet;
        For z1 := xbet Downto 1 Do Begin
          If Level.data[z1, y1] = 0 Then Begin
            x1 := z1 + 1;
            break;
          End;
          If Level.data[z1 - 1, y1 - 1] = 1 Then Begin
            x1 := z1 - 1;
            break;
          End;
          If z1 = 1 Then x1 := 0;
        End;
        If realyrectangle(x1, y1, x2, y2) Then BallsinRectanglefill(x1, y1, x2, y2);

        For z1 := xbet Downto 1 Do Begin
          If Level.data[z1, y2] = 0 Then Begin
            x1 := z1 + 1;
            break;
          End;
          If Level.data[z1 - 1, y2 - 1] = 1 Then Begin
            x1 := z1 - 1;
            break;
          End;
          If z1 = 1 Then x1 := 0;
        End;
        If realyrectangle(x1, y1, x2, y2) Then BallsinRectanglefill(x1, y1, x2, y2);

        x1 := xbet;
        For z1 := xbet To high(level.data) - 1 Do Begin
          If Level.data[z1, y1] = 0 Then Begin
            x2 := z1 - 1;
            break;
          End;
          If Level.data[z1 + 1, y1 + 1] = 1 Then Begin
            x2 := z1 + 1;
            break;
          End;
          If z1 = high(level.data) - 1 Then x2 := z1 + 1;
        End;
        If realyrectangle(x1, y1, x2, y2) Then BallsinRectanglefill(x1, y1, x2, y2);

        For z1 := xbet To high(level.data) - 1 Do Begin
          If Level.data[z1, y2] = 0 Then Begin
            x2 := z1 - 1;
            break;
          End;
          If Level.data[z1 + 1, y2 - 1] = 1 Then Begin
            x2 := z1 + 1;
            break;
          End;
          If z1 = high(level.data) - 1 Then x2 := z1 + 1;
        End;
        If realyrectangle(x1, y1, x2, y2) Then BallsinRectanglefill(x1, y1, x2, y2);
      End;
  End;
End;

Procedure flaecheberechnen;
Var
  c, x, y, z: integer;
Begin
  c := 0;
  z := round((high(level.data) - 1) * (high(level.data[0]) - 1));
  For x := 1 To high(level.data) - 1 Do
    For y := 1 To high(level.data[1]) - 1 Do
      If level.data[x, y] = 1 Then inc(c);
  level.flaeche := round((c / z) * 100);
  form1.label3.caption := 'Area : ' + inttostr(level.flaeche);
  If level.flaeche >= autosprung Then spielende;
End;

Procedure barprogress;
Var
  x, y: integer;
Begin
  For x := 1 To high(level.data) - 1 Do
    For y := 1 To high(level.data[1]) - 1 Do
      Case Level.data[x, y] Of
        2: Begin
            If mousedir Then Begin // waagrecht
              Level.data[x, y] := 1;
              If Not (level.data[x + 1, y] = 1) Then level.data[x + 1, y] := 4;
              If Not (level.data[x - 1, y] = 1) Then level.data[x - 1, y] := 3;
            End
            Else Begin // senkrecht
              Level.data[x, y] := 1;
              If Not (level.data[x, y + 1] = 1) Then level.data[x, y + 1] := 5;
              If Not (level.data[x, y - 1] = 1) Then level.data[x, y - 1] := 6;
            End;
          End;
        3: level.data[x, y] := 7;
        4: level.data[x, y] := 8;
        5: level.data[x, y] := 9;
        6: level.data[x, y] := 10;
        7: Begin
            level.data[x, y] := 1;
            If Not (level.data[x - 1, y] = 1) Then
              level.data[x - 1, y] := 3
            Else
              RechteckKI(1, x, y);
          End;
        8: Begin
            level.data[x, y] := 1;
            If Not (level.data[x + 1, y] = 1) Then
              level.data[x + 1, y] := 4
            Else
              RechteckKI(2, x, y);
          End;
        9: Begin
            Level.data[x, y] := 1;
            If Not (level.data[x, y + 1] = 1) Then
              level.data[x, y + 1] := 5
            Else
              RechteckKI(3, x, y);
          End;
        10: Begin
            Level.data[x, y] := 1;
            If Not (level.data[x, y - 1] = 1) Then
              level.data[x, y - 1] := 6
            Else
              RechteckKI(4, x, y);
          End;
      End;
  flaecheberechnen;
  baronprogress := false;
  For x := 1 To high(level.data) - 1 Do
    For y := 1 To high(level.data[1]) - 1 Do
      If (Level.data[x, y] > 1) Then Begin
        baronprogress := true;
        exit;
      End;
End;

Procedure MoveBalls;
Var
  i: Integer;
Begin
  For i := 0 To High(balls) Do Begin
    balls[i].doMove;
  End;
End;

Procedure spielende;
Begin
  If level.flaeche < 95 Then flaecheberechnen;
  If level.flaeche >= Sprung Then inc(level.Count);
  form1.Label1.caption := 'Level : ' + inttostr(level.Count + 1);
  If level.live = 0 Then Begin
    GameState := gmWait;
    If Windowscursor Then Begin
      form1.OpenGLControl1.Cursor := crdefault;
    End;
    form1.Image1.visible := false;
    form1.Image2.visible := false;
    form1.Label1.visible := false;
    form1.Label2.visible := false;
    form1.Label3.visible := false;
    form1.Label4.visible := false;
    form1.Label5.visible := false;
    Showmessage(pchar('Finished with : ' + inttostr(level.points) + ' points'));
    Highscore.add(bezeichnung + ': ' + inttostr(level.points));
    Highscore.save;
    exit;
  End;
  ballspeedinc := ballspeedinc + ballspeedinc2;
  level.points := level.points + level.flaeche;
  form1.label5.caption := 'Points : ' + inttostr(level.points);
  arrayscalierenloeschen;
  naechsteslevel;
End;

Procedure LoadGraphiks;
Var
  s: String;
Begin
  s := IncludeTrailingPathDelimiter(ExtractFilePath(paramstr(0))) + 'gfx' + PathDelim;
  // Level und Bar Graphiken
  BottomP := OpenGL_GraphikEngine.LoadGraphikItem(s + 'bottom.png', smClamp);
  wallP := OpenGL_GraphikEngine.LoadGraphikItem(s + 'wall.png', smClamp);
  BarvollP := OpenGL_GraphikEngine.LoadGraphikItem(s + 'barvoll.png', smClamp);
  BarR_LP := OpenGL_GraphikEngine.LoadGraphikItem(s + 'barr_l.png', smClamp);
  BarL_RP := OpenGL_GraphikEngine.LoadGraphikItem(s + 'barl_r.png', smClamp);
  BarO_UP := OpenGL_GraphikEngine.LoadGraphikItem(s + 'baro_u.png', smClamp);
  BarU_OP := OpenGL_GraphikEngine.LoadGraphikItem(s + 'baru_o.png', smClamp);
  // Die Ball Animationen
  LoRuP := OpenGL_GraphikEngine.LoadAlphaGraphikItem(s + 'loru.png', smclamp);
  LuRoP := OpenGL_GraphikEngine.LoadAlphaGraphikItem(s + 'luro.png', smclamp);
  RuLoP := OpenGL_GraphikEngine.LoadAlphaGraphikItem(s + 'rulo.png', smclamp);
  RoLuP := OpenGL_GraphikEngine.LoadAlphaGraphikItem(s + 'rolu.png', smclamp);
  // die Cursor
  WaagrechtP := OpenGL_GraphikEngine.LoadAlphaGraphikItem(s + 'waagrecht.png', smclamp);
  SenkrechtP := OpenGL_GraphikEngine.LoadAlphaGraphikItem(s + 'senkrecht.png', smclamp);
End;

{ Tball }

Constructor Tball.create(Number: Integer);
Var
  i: Integer;
Begin
  Inherited create;
  tot := false;
  dir := random(4);
  dirp[0].Sprite := OpenGL_SpriteEngine.AddSprite(LoRuP, inttostr(Number) + 'LoRu', true, Frect(0, 0, lorup.OrigWidth / lorup.StretchedWidth, lorup.OrigHeight / lorup.StretchedHeight), StoneWidth, StoneHeight,
    14, 2, 0, 27, BallRotatespeed Div 28, Nil, Nil);
  dirp[1].Sprite := OpenGL_SpriteEngine.AddSprite(LuRoP, inttostr(Number) + 'LuRo', true, Frect(0, 0, LuRoP.OrigWidth / LuRoP.StretchedWidth, LuRoP.OrigHeight / LuRoP.StretchedHeight), StoneWidth, StoneHeight,
    14, 2, 0, 27, BallRotatespeed Div 28, Nil, Nil);
  dirp[2].Sprite := OpenGL_SpriteEngine.AddSprite(RuLoP, inttostr(Number) + 'RuLo', true, Frect(0, 0, RuLoP.OrigWidth / RuLoP.StretchedWidth, RuLoP.OrigHeight / RuLoP.StretchedHeight), StoneWidth, StoneHeight,
    14, 2, 0, 27, BallRotatespeed Div 28, Nil, Nil);
  dirp[3].Sprite := OpenGL_SpriteEngine.AddSprite(RoLuP, inttostr(Number) + 'RoLu', true, Frect(0, 0, RoLuP.OrigWidth / RoLuP.StretchedWidth, RoLuP.OrigHeight / RoLuP.StretchedHeight), StoneWidth, StoneHeight,
    14, 2, 0, 27, BallRotatespeed Div 28, Nil, Nil);
  For i := 0 To 3 Do
    dirp[i].FrameOffset := random(29) * (BallRotatespeed Div 28);
  Visible := true;
End;

Procedure Tball.Render;
Begin
  If Visible Then Begin
    OpenGL_SpriteEngine.RenderSprite(x, y, 0, dirp[dir].Sprite, dirp[dir].FrameOffset);
  End;
End;

Procedure Tball.docollision;
Begin
  // Noch nicht implementiert
End;

Procedure Tball.doMove;
Begin
  Case dir Of
    0: Begin
        x := x + (Ballspeed + ballspeedinc);
        y := y + (Ballspeed + ballspeedinc);
      End;
    1: Begin
        x := x + (Ballspeed + ballspeedinc);
        y := y - (Ballspeed + ballspeedinc);
      End;
    2: Begin
        x := x - (Ballspeed + ballspeedinc);
        y := y - (Ballspeed + ballspeedinc);
      End;
    3: Begin
        x := x - (Ballspeed + ballspeedinc);
        y := y + (Ballspeed + ballspeedinc);
      End;
  End;
  Zusammenstosss;
  If Ballcollision Then docollision;
End;

Procedure Tball.Zusammenstosss;
Var
  xb, yb: integer;
Begin
  If Not (tot) Then Begin
    xb := round(x - 12);
    yb := round(y - 12);
    If (xb < 0) Or ((xb Div 20) > high(level.data)) Then Begin
      TOT := true;
      visible := false;
      exit;
    End;
    If (yb < 0) Or ((yb Div 20) > high(level.data[1])) Then Begin
      TOT := true;
      visible := false;
      exit;
    End;
    If level.data[((xb + 10) Div 20), (yb Div 20) + 1] = 1 Then
      If Dir = 2 Then
        dir := 1
      Else
        dir := 0;
    If level.data[((xb + 10) Div 20) + 1, (yb Div 20) + 1] = 1 Then
      If Dir = 1 Then
        dir := 2
      Else
        dir := 3;
    If level.data[(xb Div 20) + 1, ((yb + 10) Div 20)] = 1 Then
      If dir = 1 Then
        dir := 0
      Else
        dir := 3;
    If level.data[(xb Div 20) + 1, ((yb + 10) Div 20) + 1] = 1 Then
      If dir = 0 Then
        dir := 1
      Else
        dir := 2;
    If Not sinnlos Then
      If (level.data[((xb + 10) Div 20), (yb Div 20) + 1] > 1) Or
      (level.data[((xb + 10) Div 20) + 1, (yb Div 20) + 1] > 1) Or
      (level.data[(xb Div 20) + 1, ((yb + 10) Div 20)] > 1) Or
      (level.data[(xb Div 20) + 1, ((yb + 10) Div 20) + 1] > 1) Then Begin
        If (level.data[((xb + 10) Div 20), (yb Div 20) + 1] > 1) Then level.data[((xb + 10) Div 20), (yb Div 20) + 1] := 1;
        If (level.data[((xb + 10) Div 20) + 1, (yb Div 20) + 1] > 1) Then level.data[((xb + 10) Div 20) + 1, (yb Div 20) + 1] := 1;
        If (level.data[(xb Div 20) + 1, ((yb + 10) Div 20)] > 1) Then level.data[(xb Div 20) + 1, ((yb + 10) Div 20)] := 1;
        If (level.data[(xb Div 20) + 1, ((yb + 10) Div 20) + 1] > 1) Then level.data[(xb Div 20) + 1, ((yb + 10) Div 20) + 1] := 1;
        dec(level.live);
        form1.label4.caption := 'Lives : ' + inttostr(level.live);
        tot := true;
        visible := false;
        If (level.live = 0) Then spielende;
        dir := 4;
      End;
  End;
End;

End.

