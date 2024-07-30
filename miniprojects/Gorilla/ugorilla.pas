(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Gorilla                                               *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit ugorilla;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, LCLIntf,
  LCLType,
  Graphics,
  forms,
  dialogs,
  math, // Sin, Cos
  dglopengl,
  lNetComponents,
  LResources,
  lnet,
  IntfGraphics, // TLazIntfImage type
  fpImage, // TFPColor type
  uvectormath, // http://corpsman.de/index.php?doc=opengl/opengl_graphik_engine
  uOpenGL_ASCII_Font; // http://corpsman.de/index.php?doc=opengl/simple_font

Const

  GorillaVersion = 005; // Spiel Versionsnummer

  (*
   * Op Codes für die Steuerung der Netzwerk Kommunikation
   *)
  opNoMessage = 0; // Es gab eigentlich gar keine Nachricht, trotzdem wurde ein OnReceive Event ausgelöst.
  opClientChangedName = 1; // Der Client Hat seinen Namen geändert, und teilt dies dem Server mit.
  opServerChangedName = 2; // Der Server hat seinen Namen geändert, und teilt dies dem Client mit.
  opChatMessage = 3; // Wenn eine Nachricht durch den Chat versendet wird.
  opServerChangedRoundsToPlay = 4; // Wenn der Server die Anzahl der zu spielenden Runden geändert hat.
  opServerChangedGravity = 5; // Wenn der Server die Schwerkraft ändert.
  opServerChangedMaxWind = 6; // Wenn der Server die Maximale Windstärke ändert
  opStartGame = 7; // Der Server sagt dem Client, er soll Startgame ausführen und damit die Variablen initialisieren.
  opClientReadyToPlay = 8; // Der Client hat erfolgreich das spiel initialisiert und es kann gestartet werden.
  opUpdateSchmierstring = 9; // Einer der beiden Spieler ( der Berechtigte ) hat den Schmierstring Wert geändert, und sendet ihn dem Anderen
  opReturnPressed = 10; // Der Berechtigte Spieler hat "Enter" gedrückt und schaltet damit die Angle/ Power Eingabe um.
  opShowStats = 11; // Die Runde wird Regulär beendet
  opCancelRound = 12; // Einer Der Spieler "bricht" ab. Dann zeigen wir die Statistik an
  opEndShowStats = 13; // Einer Der Spieler "Beendet" den ShowPlayerStats screen. Das muss dann beide Player umschalten, sonst entsteht beim anderen ein Bug, dass dieser sehen kann wie der Player seinen namen ändert !
  opServerSendStats = 14; // Der Server Sendet die Spieler Informationen, die gehen beim Client irgendwie verloren ...
  opDidNotAccept = 15; // Der Server ist schon verbunden aktzeptiert keine weiteren Verbindungen mehr

  (*
   * Für das Spiel wichtige konstanten.
   *)
  PlayerColideWidth = 5; // Die Halbe Breite des Spieler "Dead" Rectangles ( hälftig, weil von der mitte gemessen ) ( Maximal realistisch 9 )
  PlayerColideHeight = 28; // Die "Ganze" höhe des Spieler "Dead" Rectangles
  BananaKilldist = 100; // Ist die Banane weiter als Killdist Links und Rechts aus dem Monitor geflogen, wird die Simulation abgebrochen
  BananaKillspeed = 0.01; // Fliegt die Banane Langsamer als Killspeed wird die Simulation abgebrochen

  minUfoYPos = 40; // Der y-Bereich in dem das Ufo sich befinden darf
  maxUfoYpos = 120; // Der y-Bereich in dem das Ufo sich befinden darf
  minBuildingWidth = 30; // Minimale Gebäudebreite in Pixel
  maxBuildingWidth = 70; // Maximale Gebäudebreite in Pixel
  minBuildingHeight = 30; // Minimale Gebäudehöhe in Pixel
  maxBuildingHeight = 290; // Maximale Gebäudehöhe in Pixel
  GroundLine = 460; // Die 0-Linie der Gebäude
  BananaDamageRadius = 15; // Radius in Pixel, des Schadens den eine Banane an Gebäuden anrichtet
  PlayerDeadRadiusX = 55; // Die Ausdehnung der Ellipse die ein sterbender Spieler hinterläst
  PlayerDeadRadiusY = 25; // Die Ausdehnung der Ellipse die ein sterbender Spieler hinterläst

  ArmUpTime = 500; // Zeit in ms die der Gorilla seinen Arm Hebt ( bevor er wirft )
  BananaRotTime = 10; // Rotationsgeschwindigkeit der Banane im Flug
  VictoryDanceTime = 1000; // Periode für das Arm Wackeln des Siegers
  maxVictoryDanceTime = 7; // Maximale Anzahl an Perioden, die die Siegespose wiederhohlt wird.
  SunScareTime = 2000; // Die Zeit wie Lange die Sonne Erschricken ist.

  UFOTransparentColor = clFuchsia; // Die Farbe, welche in der Ufo Graphik, als Nicht Kollisionsfähig bezeichnet wird.
  BackGround = $A80000; // So ein komisches "Blau" als Hintergrund
  SunColor: TColor = $00FCFC; // Farbe der Sonne
  BananaColor: TColor = $00FFFF; // Farbe der Banane
  WindColor: TColor = $0000FF; // Farbe des Wind Pfeiles
  BuildingColors: Array[0..2] Of TColor = ($A8A800, $A8A8A8, $0000A8); // Mögliche Farben für Gebäude
  WindowColors: Array[0..1] Of TColor = ($54FCFC, $545454); // Mögliche Farben für Fenster
  GorillaColors: Array[0..3] Of TColor = ($54A8FC, $54FCA8, $A8FC54, $A854FC); // Die Gorilla Farben

{$DEFINE ImprovedCollisionDetection} // Ist dieser Switch an, dann kann es nicht mehr vorkommen, dass die Banane "durch" die Boundingbox des Gorillas hindurchfliegen kann.

Type

  TGorillaArmState = (gaUp = 0, gaDown = 1); // Ein Arm kann gehoben oder gesenkt sein

  tGorillastate = (
    gsStartScreen, // Startbildschirm
    gsEnterName, // Screen zum Abfragen von Namen
    gsEnterGravity, // Zur Eingabe der Aktuellen Gravitation
    gsEnterMaxWind, // Zur Eingabe des Maximal Möglichen Windes
    gsEnterRoundCount, // Screen zum Abfragen wie viele Runden gespielt werden sollen ?
    gsEnterAngle, // Ein Spieler Gibt Gerade den Winkel ein
    gsEnterPower, // Ein Spieler Gibt Gerade die Kraft ein
    gsStartThrow, // Bei Startthrow hebt der Gorilla den Arm, zum Werfen
    gsThrow, // Führt den Wurf und am Ende die Entsprechenden Auswertungen aus, danach entweder Enter Angle, oder New Round oder Auswertung.
    gsVictoryDance, // Der Als Letztes Übrig gebliebene Spieler führt einen Siegestanz auf
    gsViewPlayerStats, // Zeigt noch mal die Highscores an.
    (*
     * Ab Hier die Extra zustände fürs Netzwerk
     *)
    gsSelectNetworkMode, // Abfrage ob Server oder Client
    gsQuestionPort, // Abfrage, welcher Port verwendet werden soll.
    (*
     * Ab Hier Alle Client Spezifischen Stati
     *)
    gsQuestionIP, // Die Abfrage der Server IP
    gsWaitForServerToStart, // Der Client ist erfolgreich verbunden und wartet nun bis der Server startet. Nebenher darf er seinen namen eingeben
    (*
     * Ab Hier Alle Server Spezifischen Stati
     *)
    gsWaitForClient // Der Server Wartet bis der Client Connected hat.
    );

  TBanana = Record
{$IFDEF ImprovedCollisionDetection}
    OldPosition: TVector2; // Damit die Genauere Flugbahnkollision gemacht werden kann, muss jeweils die Alte Position mit geführt werden.
{$ENDIF}
    Position: TVector2; // Aktuelle Position der Banane
    Acceleration: TVector2; // Aktuelle Beschleunigung der Banane
    Speed: TVector2; // Aktuelle Geschwindigkeit der Banane
    Rotation: Integer; // Aktuelle Drehung der Banane
  End;

  Tfinish_reason = (
    ai_out_of_sceen, // Die KI, hat aus dem Monitor geschossen
    ai_hit_building, // die KI hat ein Gebäude getroffen
    ai_hit_ufo // Die KI hat das Ufo getroffen
    );

  TAi = Record
    initialized: Boolean; // Anzeige ob der KI-Datensatz initialisiert ist.
    finish_reason: Tfinish_reason; // Der Grund, warum die Banane "gelöscht" wurde
    lastthrowpos: Tvector2; // Die Position, an welcher die Banane "gelöscht" wurde
  End;

  TNetwork = Record
    active: Boolean; // Ist gerade ein Netzwerkkspiel ?
    port: word; // Der zu verwendende Port
    ServerIP: String; // der Client muss die IP des Servers wissen
    isServer: Boolean; // Ist die Anwendung Server, oder Client ?
    Socket: TLSocket; // Zur Unterscheidung, wer das Spiel verlassen hat
  End;

  { TGorilla }

  TGorilla = Class
  private
    FNetwork: TNetwork; // Alles was noch zusätzlich für den 2-Player Netzwerkmodus notwendig ist.
    fai: Tai; // Alle Daten die der KI zur Verfügung stehen ( + die FLast.. daten )
    fUfoGraphic: Tbitmap; // Die Graphik des Ufos, geladen aus der Ressource
    fUfoPos: TVector2; // Die Position des Ufos, wenns dens eins Gibt. ( nur für die KI notwendig )
    FmaxWindStrength: integer; // Maximale Windkraft
    FBanana: TBanana; // Eine Banane die Gerade so Rumfliegt
    fSun: DWord; // Speichert, ob die Sonne gerade "o" macht
    fWindStrength: TVector2; // Die Aktuelle Windstärke
    fGravity: TVector2; // Die Aktuelle Schwerkraft
    fGorillaBody: Array[-9..9, 0..28] Of Boolean; // Texturcontainer für einen Gorilla
    fGorillaArm: Array[0..7, 0..10] Of Boolean; // Texturcontainer für einen Arm
    fBananaTexture: Array[-4..4, -4..1] Of boolean; //Texturcontainer für eine Banane
    fSunNormalTexture: Array[-20..20, -15..15] Of boolean; //Texturcontainer für eine Sonne
    fSunScareTexture: Array[-20..20, -15..15] Of boolean; //Texturcontainer für eine Sonne
    fPlayerNames: Array[0..3] Of String; // Abspeichern der Spielernamen
    fPlayerPoints: Array[0..3] Of integer; // Abspeichern der Siege der Einzelnen Spieler
    fPlayerAlive: Array[0..3] Of boolean; // Abspeichern ob der Spieler im Spiel ist ( bei mehr als 2 Spielern notwendig )
    fLastPlayerAngle: Array[0..3] Of integer; // Der Zuletzt benutze Abwurfwinkel
    fLastPlayerPower: Array[0..3] Of integer; // Die Zuletzt benutzte Abwurfkraft
    fplayerpos: Array[0..3] Of tvector2; // Die "Positionsdaten" der Spieler
    fSunPos: TVector2; // Position der Sonne
    fPlayerCount: integer; // Anzahl der Gesamt Spieler in Spiel
    fRoundCount: integer; // Anzahl der zu Spielenden Runden
    fActualRound: integer; // Die Aktuell Gespielte Runde
    fActualPlayer: integer; // Der Aktuell gewählte Spieler
    fLastTime: Dword; // Zum Messen von Zeiten
    ftemporaryInt: Integer; // Quasi Globale Variablen zum wilden gebrauch
    ftemporaryString: String; // Quasi Globale Variablen zum wilden gebrauch
    fGorillaState: tGorillastate; // Der Aktuelle Spiel Modus
    fMap: Array[0..639, 0..479] Of TColor; // Die Hintergrundkarte = Alles was beschädigt werden kann
    Procedure SetOpenGLColor(Const Farbe: TColor); // Setzt die OpenGL Farbe auf Farbe
    Procedure RenderGorilla(pos: TVector2; GorillaColor: TColor; LeftArm, RightArm: TGorillaArmState); // Zeichnet einen Gorilla
    Procedure RenderBanana(); // Zeichnet die Banane + Rotation
    Procedure RenderWindStrength(); // Zeichnet den Wind Pfeil
    Procedure RenderSun(); // Zeichnet die Sonne
    Procedure EraseElippseInMap(x, y, r1, r2: integer); // Zeichnet eine Ellipse mit R1 in X-Richtung und R2- in Y-Richtung in fMap

    Procedure CollideBanana(); // Kolisionen der Banane mit allem und entsprechendes weiterschalten

    Procedure StartGame(); // Initialisiert alles zum Spielbegin
    Procedure NewRound(); // Initialisiert alles für eine neue Runde

    Procedure SetBuilding(StartX, EndX, Height: integer); // Zeichnet ein Gebäude in die Karte
    Procedure SetUfo(); // Zeichnet ein Ufo in die Karte
    Procedure ShowStats(); // Bereitet alles Vor, auf dass die Statistik angezeigt werden kann = Spiel Ende !!
    (*
     * Die Ki vesucht aus einer Mischung zwischen Heuristik und Intervallschachtelung zu Treffen.
     * Sie startet dabei immer mit 45° und ca. 450 Power
     *
     *  Einzig bisher bekanntes Manko :
     *
     *   Der Winkel nähert sich über die Anzahl der Fehlversuche immer mehr an 90° an, da er nie wieder "Flacher" gemacht wird.
     *
     *)
    Procedure Do_AI_move(); // Berechnet Winkel und Kraft für die Ki und "Zyndet"
    (*
     * Die Netzwerkspezifischen Sachen
     *)
    Function GetMessage(aSocket: TLSocket): String; // Liest von Socket einen String ( auch Leerstring ) und gibt diesen Zurück
    Function fgetChatName(): String; // Gibt den Netwerknamen des Spielers zurück ( für den Chat )
    Procedure OnAnglePowerReturn(); // Das Return wurde so ausgelagert, damit wenn es geändert werden muss alle OnReturns betroffen sind, ( Leichtere Wartbarkeit )
    Procedure SendPlayerStats(); // Der Server sendet dem Client alle Player Informationen
  public
    Property ChatName: String read fgetChatName; // Der Name des Spielers, welcher im Chat angezeigt wird
    Constructor create;
    Destructor Destroy; override;
    Procedure Render;
    Procedure OnKeyDown(Var Key: Word; Shift: TShiftState);
    (*
     * Die Netzwerkspezifischen Sachen
     *)
    Procedure SendMessage(OpCode: byte; Message: String); // Sendet einen String und entsprechend dessen OpCode
    Procedure OnDisconnect(aSocket: TLSocket);
    Procedure OnAccept(aSocket: TLSocket);
    Procedure OnReceive(OpCode: byte; aSocket: TLSocket); // Die Vor Verarbeitete Empfansroutine
  End;

Var
  Network: TLTCPComponent = Nil; // Der Pointer auf die Ln Komponente von TForm1, so kann man sich das "Uses unit1" unter implementation sparen...

Implementation

{ TGorilla }

Uses
  Unit2, // für die Online Hilfe
  unit3; // für den Chat

Constructor TGorilla.create;
Var
  i, j: integer;
Begin
  Inherited create;
  (*
   * Die Ufo Graphik wird als ressource Geladen, der Rest ist im Source hard gecodet
   *)
  fufographic := Tbitmap.Create;
  fufographic.LoadFromLazarusResource('ufo_template');
  fUfoPos := v2(-1, -1);
  FNetwork.active := false;
  FNetwork.port := 9876;
  FNetwork.ServerIP := '127.0.0.1';
  FNetwork.isServer := false;
  FNetwork.Socket := Nil;
  fSunPos := v2(320, 25);
  fGravity := v2(0, 10);
  fGorillaState := gsStartScreen;
  fLastTime := GetTickCount;
  fRoundCount := 3;
  FmaxWindStrength := 100;
  fPlayerNames[0] := 'Player1';
  fPlayerNames[1] := 'Player2';
  fPlayerNames[2] := 'Player3';
  fPlayerNames[3] := 'Player4';
  // Abspeichern der Gorilla Textur, das ginge sicherlich auch via TBitmap, aber so gehts auch *g*
  For i := -9 To 9 Do
    For j := 0 To 28 Do
      fGorillaBody[i, j] := false;
  // Der Kopf
  For i := -4 To 5 Do
    For j := 21 To 28 Do
      fGorillaBody[i, j] := true;
  fGorillaBody[-4, 28] := false;
  fGorillaBody[-4, 27] := false;
  fGorillaBody[-4, 23] := false;
  fGorillaBody[-4, 22] := false;
  fGorillaBody[-4, 21] := false;
  fGorillaBody[-3, 21] := false;
  For i := -2 To 3 Do
    fGorillaBody[i, 26] := false;
  fGorillaBody[-1, 24] := false;
  fGorillaBody[0, 24] := false;
  fGorillaBody[2, 24] := false;
  fGorillaBody[3, 24] := false;
  fGorillaBody[5, 28] := false;
  fGorillaBody[5, 27] := false;
  fGorillaBody[5, 23] := false;
  fGorillaBody[5, 22] := false;
  fGorillaBody[5, 21] := false;
  fGorillaBody[4, 21] := false;
  // Die Brust
  For i := -7 To 8 Do
    For j := 14 To 20 Do
      fGorillaBody[i, j] := true;
  fGorillaBody[-4, 14] := false;
  fGorillaBody[-3, 14] := false;
  fGorillaBody[-2, 14] := false;
  fGorillaBody[-1, 15] := false;
  fGorillaBody[0, 16] := false;
  fGorillaBody[1, 17] := false;
  fGorillaBody[1, 18] := false;
  fGorillaBody[2, 16] := false;
  fGorillaBody[3, 15] := false;
  fGorillaBody[4, 14] := false;
  fGorillaBody[5, 14] := false;
  fGorillaBody[6, 14] := false;
  // Die Hose
  For i := -5 To 6 Do
    For j := 8 To 13 Do
      fGorillaBody[i, j] := true;
  fGorillaBody[-6, 8] := true;
  // Linkes Bein
  For i := -8 To -5 Do
    For j := 0 To 7 Do
      fGorillaBody[i, j] := true;
  fGorillaBody[-4, 7] := true;
  fGorillaBody[-3, 7] := true;
  fGorillaBody[-4, 6] := true;
  fGorillaBody[-9, 5] := true;
  fGorillaBody[-9, 4] := true;
  fGorillaBody[-9, 3] := true;
  fGorillaBody[-9, 2] := true;
  fGorillaBody[-9, 1] := true;
  fGorillaBody[-4, 0] := true;
  // Rechtes Bein
  For i := 5 To 8 Do
    For j := 0 To 7 Do
      fGorillaBody[i, j] := true;
  fGorillaBody[4, 7] := true;
  fGorillaBody[3, 7] := true;
  fGorillaBody[4, 6] := true;
  fGorillaBody[9, 5] := true;
  fGorillaBody[9, 4] := true;
  fGorillaBody[9, 3] := true;
  fGorillaBody[9, 2] := true;
  fGorillaBody[9, 1] := true;
  fGorillaBody[4, 0] := true;
  // Ein Arm
  For i := 0 To 7 Do
    For j := 0 To 10 Do
      fGorillaArm[i, j] := false;
  fGorillaArm[2, 10] := true;
  fGorillaArm[3, 10] := true;
  fGorillaArm[4, 10] := true;
  For i := 2 To 5 Do
    For j := 7 To 9 Do
      fGorillaArm[i, j] := true;
  fGorillaArm[6, 8] := true;
  fGorillaArm[6, 7] := true;
  For i := 3 To 7 Do
    For j := 4 To 6 Do
      fGorillaArm[i, j] := true;
  For i := 2 To 6 Do
    For j := 2 To 3 Do
      fGorillaArm[i, j] := true;
  For i := 1 To 4 Do
    For j := 0 To 1 Do
      fGorillaArm[i, j] := true;
  fGorillaArm[5, 1] := true;
  fGorillaArm[0, 0] := true;
  // Die Banane
  For i := -4 To 4 Do
    For j := -4 To 1 Do
      fBananaTexture[i, j] := true;
  For i := -2 To 2 Do
    For j := 0 To 1 Do
      fBananaTexture[i, j] := false;
  fBananaTexture[-4, -2] := false;
  fBananaTexture[-4, -3] := false;
  fBananaTexture[-4, -4] := false;
  fBananaTexture[-3, -3] := false;
  fBananaTexture[-3, -4] := false;
  fBananaTexture[4, -2] := false;
  fBananaTexture[4, -3] := false;
  fBananaTexture[4, -4] := false;
  fBananaTexture[3, -3] := false;
  fBananaTexture[3, -4] := false;
  // die beiden Sonnen
  For i := -20 To 20 Do
    For j := -15 To 15 Do Begin
      fSunNormalTexture[i, j] := false;
    End;
  For i := -12 To 12 Do
    For j := -9 To 9 Do Begin
      fSunNormalTexture[i, j] := true;
    End;
  For i := -11 To -6 Do Begin
    fSunNormalTexture[i, -9] := false;
    fSunNormalTexture[i, -8] := false;
  End;
  For i := 7 To 10 Do Begin
    fSunNormalTexture[i, -9] := false;
    fSunNormalTexture[i, -8] := false;
  End;
  For j := 5 To 7 Do Begin
    fSunNormalTexture[11, -j] := false;
    fSunNormalTexture[12, -j] := false;
    fSunNormalTexture[-12, -j] := false;
  End;
  fSunNormalTexture[12, -3] := false;
  fSunNormalTexture[-12, -4] := false;
  fSunNormalTexture[-11, -5] := false;
  fSunNormalTexture[10, -6] := false;
  fSunNormalTexture[-11, -6] := false;
  fSunNormalTexture[9, -7] := false;
  fSunNormalTexture[-9, -7] := false;
  fSunNormalTexture[-10, -7] := false;
  fSunNormalTexture[6, -8] := false;
  fSunNormalTexture[12, -9] := false;
  fSunNormalTexture[11, -9] := false;
  fSunNormalTexture[5, -9] := false;
  fSunNormalTexture[4, -9] := false;
  fSunNormalTexture[-4, -9] := false;
  fSunNormalTexture[-12, -9] := false;
  For j := 10 To 15 Do Begin
    fSunNormalTexture[0, -j] := true;
  End;
  For i := 13 To 20 Do Begin
    fSunNormalTexture[i, 0] := true;
    fSunNormalTexture[-i, 0] := true;
  End;
  fSunNormalTexture[-8, -13] := true;
  fSunNormalTexture[-7, -12] := true;
  fSunNormalTexture[-7, -11] := true;
  fSunNormalTexture[-6, -10] := true;
  fSunNormalTexture[8, -13] := true;
  fSunNormalTexture[8, -12] := true;
  fSunNormalTexture[7, -11] := true;
  fSunNormalTexture[6, -10] := true;
  fSunNormalTexture[13, -9] := true;
  fSunNormalTexture[14, -10] := true;
  fSunNormalTexture[15, -10] := true;
  For i := 0 To 2 Do Begin
    fSunNormalTexture[13 + i, -4] := true;
    fSunNormalTexture[16 + i, -5] := true;
  End;
  fSunNormalTexture[-13, -8] := true;
  fSunNormalTexture[-14, -9] := true;
  fSunNormalTexture[-15, -10] := true;
  For i := 0 To 3 Do
    fSunNormalTexture[-17 + i, -4] := true;
  fSunNormalTexture[-13, -3] := true;
  fSunNormalTexture[-18, -5] := true;
  For i := -20 To 20 Do
    For j := -15 To -1 Do
      fSunNormalTexture[i, -j] := fSunNormalTexture[i, j];
  fSunNormalTexture[-3, 3] := false;
  fSunNormalTexture[-4, 2] := false;
  fSunNormalTexture[-3, 2] := false;
  fSunNormalTexture[-2, 2] := false;
  fSunNormalTexture[-3, 1] := false;
  For i := -20 To 20 Do
    For j := -15 To 15 Do Begin
      fSunScareTexture[i, j] := fSunNormalTexture[i, j];
    End;
  // Einbasteln des Lachenden Gesichtes
  fSunNormalTexture[-7, -3] := false;
  fSunNormalTexture[-6, -4] := false;
  fSunNormalTexture[-5, -4] := false;
  fSunNormalTexture[-4, -5] := false;
  fSunNormalTexture[-3, -5] := false;
  fSunNormalTexture[-2, -6] := false;
  fSunNormalTexture[-1, -6] := false;
  fSunNormalTexture[0, -6] := false;
  For i := -7 To -1 Do
    For j := -6 To 3 Do
      fSunNormalTexture[-i, j] := fSunNormalTexture[i, j];
  // Einbasteln des erschrockenen Gesichtes
  For i := -1 To 0 Do Begin
    fSunScareTexture[i, -4] := false;
    fSunScareTexture[i, -5] := false;
    fSunScareTexture[i, -6] := false;
    fSunScareTexture[i, -7] := false;
    fSunScareTexture[-i, -4] := false;
    fSunScareTexture[-i, -5] := false;
    fSunScareTexture[-i, -6] := false;
    fSunScareTexture[-i, -7] := false;
  End;
  fSunScareTexture[-2, -5] := false;
  fSunScareTexture[-2, -6] := false;
  fSunScareTexture[2, -5] := false;
  fSunScareTexture[2, -6] := false;
  For i := -7 To -1 Do
    For j := 1 To 3 Do
      fSunScareTexture[-i, j] := fSunScareTexture[i, j];
End;

Destructor TGorilla.Destroy;
Begin
  fufographic.free;
End;

Procedure TGorilla.EraseElippseInMap(x, y, r1, r2: integer);

  Procedure SetPixel(xp, yp: integer);
  Begin
    If (xp >= low(fmap)) And (xp <= High(fmap)) And (yp >= low(fmap[0])) And (yp <= High(fmap[0])) Then Begin
      fmap[xp, yp] := BackGround;
    End;
  End;

  Procedure Line(x1, x2, y: integer);
  Var
    i: integer;
  Begin
    For i := x1 To x2 Do
      Setpixel(i, y);
  End;

  Procedure drawEllipse(xm, ym, a, b: integer);
  Var
    e2, err, a2, b2, dx, dy: integer;
  Begin
    If (a = 0) And (b = 0) Then exit;
    // im I. Quadranten von links oben nach rechts unten
    dx := 0;
    dy := b;
    a2 := a * a;
    b2 := b * b;
    err := b2 - (2 * b - 1) * a2; // Fehler im 1. Schritt
    Repeat
      line(xm - dx, xm + dx, ym + dy); // I. Quadrant, // II. Quadrant
      line(xm - dx, xm + dx, ym - dy); // III. Quadrant, // IV. Quadrant
      e2 := 2 * err;
      If (e2 < (2 * dx + 1) * b2) Then Begin
        inc(dx);
        inc(err, (2 * dx + 1) * b2);
      End;
      If (e2 > -(2 * dy - 1) * a2) Then Begin
        dec(dy);
        dec(err, (2 * dy - 1) * a2);
      End;
    Until (dy < 0);
    inc(dx);
    While (dx < a) Do Begin // fehlerhafter Abbruch bei flachen Ellipsen (b=1)
      line(xm - dx, xm + dx, ym); // -> Spitze der Ellipse vollenden
      inc(dx);
    End;
  End;
Begin
  drawEllipse(x, y, r1, r2);
End;

Procedure TGorilla.ShowStats;
Begin
  (*
   * Diese Routine wird quasi als Spiel Ende Aufgerufen, hier können weitere Spiel auswertende Dinge geschehen
   *)
  // !! ACHTUNG !! dieser Code funktioniert zwar, es ist aber nicht sichergestellt das der Code nach dem if nur 1 mal beim beenden durchgeführt wird.
  // Will man unten mehr als einfach nur die State Machine umschalten dann sollte das geprüft werden !!
  If FNetwork.active Then Begin
    If fGorillaState <> gsViewPlayerStats Then Begin
      If FNetwork.isServer Then Begin
        // Der Server Aktualisiert lieber noch mal die Stats
        SendPlayerStats();
      End;
      SendMessage(opShowStats, '');
    End
    Else Begin
      exit;
    End;
  End;
  fGorillaState := gsViewPlayerStats;
End;

Procedure TGorilla.Do_AI_move;
Const
  WindKIScale = 3;
Var
  key: Word;
  calculated_Angle: integer;
  calculated_Power: integer;
Begin
  If (fai.initialized) Then Begin
    Case fai.finish_reason Of
      ai_hit_ufo: Begin
          // Durch das Ufo schiesen wir einfach durch !!
          calculated_Angle := fLastPlayerAngle[1];
          calculated_Power := fLastPlayerPower[1];
        End;
      ai_hit_building: Begin
          If abs(fplayerpos[1].x - fai.lastthrowpos.x) < 2 * maxBuildingWidth Then Begin
            // Die KI, hat das Gebäude direkt neben sich getroffen
            If fai.lastthrowpos.y > fplayerpos[1].y Then Begin
              // Das Geäude wurde zwar getroffen, doch tiefer, d.h. es war zu wenig kraft, weil der Wind so stark war
              calculated_Angle := fLastPlayerAngle[1];
              calculated_Power := fLastPlayerPower[1] * 2; // Ordentlich schwung drauf :)
            End
            Else Begin
              // Wir haben tatsächlich ein Gebäude in der Flugbahn erwischt ..
              calculated_Angle := (fLastPlayerAngle[1] + 90) Div 2;
              calculated_Power := 450 + round(fWindStrength.x * WindKIScale); // Reset der Wurfstärke
            End;
          End
          Else Begin
            // Die KI, Hat das Gebäude in der Nähe des Anderen Spielers getroffen
            If fplayerpos[0].x < fplayerpos[1].x Then Begin
              // Der Spieler Steht Links von der KI
              If fai.lastthrowpos.x > fplayerpos[0].x Then Begin
                // Die KI hat noch nicht weit genug geschossen
                calculated_Angle := fLastPlayerAngle[1];
                calculated_Power := fLastPlayerPower[1] + round(abs(fai.lastthrowpos.x - fplayerpos[0].x));
              End
              Else Begin
                // Die KI hat zu weit geschossen
                calculated_Angle := fLastPlayerAngle[1];
                calculated_Power := fLastPlayerPower[1] - round(abs(fai.lastthrowpos.x - fplayerpos[0].x));
              End;
            End
            Else Begin
              // Der Spieler Steht rechts von der KI
              If fai.lastthrowpos.x < fplayerpos[0].x Then Begin
                // Die KI hat noch nicht weit genug geschossen
                calculated_Angle := fLastPlayerAngle[1];
                calculated_Power := fLastPlayerPower[1] + round(abs(fai.lastthrowpos.x - fplayerpos[0].x));
              End
              Else Begin
                // Die KI hat zu weit geschossen
                calculated_Angle := fLastPlayerAngle[1];
                calculated_Power := fLastPlayerPower[1] - round(abs(fai.lastthrowpos.x - fplayerpos[0].x));
              End;
            End;
          End;
        End;
      ai_out_of_sceen: Begin
          If (fai.lastthrowpos.y > 480) And (abs(320 - fai.lastthrowpos.x) > 320 + BananaKilldist) Then Begin
            // Die KI hat offensichtlich so stark geschossen, das die Banane durch die Map nach unten durch gefallen ist
            calculated_Angle := fLastPlayerAngle[1];
            calculated_Power := fLastPlayerPower[1] Div 2; // Wir dämpfen das Temperament drastisch
          End
          Else Begin
            // Wir haben zu weit nach Links oder rechts geschossen, das gleichen wir erst mal durch einen Steileren Wurf aus
            calculated_Angle := fLastPlayerAngle[1];
            calculated_Power := fLastPlayerPower[1] - round(abs(fai.lastthrowpos.y - fplayerpos[0].y)); // Wir dämpfen das Temperament dynamisch, nach Wurfhöhe
          End;
        End;
    End;
  End
  Else Begin
    // Im Aller ersten Zug ist die Ki noch nicht initialisiert, da muss eine Heuristik her
    If fplayerpos[0].x < fplayerpos[1].x Then Begin
      // Der Spieler steht Links von der KI.
      calculated_Angle := 135;
      calculated_Power := 450 + round(fWindStrength.x * WindKIScale);
    End
    Else Begin
      // Der Spieler steht Rechts von der KI.
      calculated_Angle := 45;
      calculated_Power := 450 - round(fWindStrength.x * WindKIScale);
    End;
  End;
  // Einen Selfkill durch zu wenig Power verhindern
  If (calculated_Power < 100) Then Begin
    calculated_Power := 450 + round(fWindStrength.x * WindKIScale);
  End;
  (*
   * Die Berechneten Daten so übergeben, dass der OnKeydown Event die Ki Startet
   *)
  fLastPlayerAngle[1] := calculated_Angle;
  ftemporaryString := inttostr(calculated_Power);
  fGorillaState := gsEnterPower;
  key := VK_RETURN;
  OnKeyDown(key, []);
End;

Procedure TGorilla.SendMessage(OpCode: byte; Message: String);
Var
  data: Array Of byte;
  i: integer;
Begin
  setlength(data, length(Message) + 2);
  data[0] := OpCode;
  data[1] := length(Message);
  For i := 1 To Length(message) Do
    data[i + 1] := ord(message[i]);
  Network.Send(data[0], high(data) + 1);
End;

Procedure TGorilla.RenderGorilla(pos: TVector2; GorillaColor: TColor; LeftArm,
  RightArm: TGorillaArmState);
Var
  i, j: integer;
Begin
  glPushMatrix();
  glTranslatef(pos.X, pos.Y, 0);
  glBegin(GL_POINTS);
  // Der Kopf, der Körper die Beine
  SetOpenGLColor(GorillaColor);
  For i := low(fGorillaBody) To high(fGorillaBody) Do
    For j := low(fGorillaBody[0]) To high(fGorillaBody[0]) Do
      If fGorillaBody[i, 28 - j] Then glVertex2i(i, j - 28);
  If RightArm = gaDown Then Begin
    For i := low(fGorillaArm) To high(fGorillaArm) Do
      For j := low(fGorillaArm[0]) To high(fGorillaArm[0]) Do
        If fGorillaArm[i, 10 - j] Then glVertex2i(i + 7, j - 19);
  End
  Else Begin
    For i := low(fGorillaArm) To high(fGorillaArm) Do
      For j := low(fGorillaArm[0]) To high(fGorillaArm[0]) Do
        If fGorillaArm[i, 10 - j] Then glVertex2i(i + 7, -17 - j);
  End;
  If LeftArm = gaDown Then Begin
    For i := low(fGorillaArm) To high(fGorillaArm) Do
      For j := low(fGorillaArm[0]) To high(fGorillaArm[0]) Do
        If fGorillaArm[i, 10 - j] Then glVertex2i(-6 - i, j - 19);
  End
  Else Begin
    For i := low(fGorillaArm) To high(fGorillaArm) Do
      For j := low(fGorillaArm[0]) To high(fGorillaArm[0]) Do
        If fGorillaArm[i, 10 - j] Then glVertex2i(-6 - i, -17 - j);
  End;
  glend();
  glpopmatrix();
End;

Procedure TGorilla.SetOpenGLColor(Const Farbe: TColor);
Begin
  glColor3ub(byte(Farbe), byte(Farbe Shr 8), byte(Farbe Shr 16));
End;

Procedure TGorilla.StartGame;
Var
  i: integer;
Begin
  // Variablen die unabhängig der Spielerzahl genullt werden sollten
  For i := 0 To 3 Do Begin
    fPlayerAlive[i] := false;
    fPlayerPoints[i] := 0;
  End;
  fActualRound := 0;
  NewRound();
End;

Procedure TGorilla.SetBuilding(StartX, EndX, Height: integer);
Var
  i, j, x, y: integer;
  c: TColor;
Begin
  // Malen der Hauswand
  c := BuildingColors[random(high(BuildingColors) + 1)];
  For i := StartX + 1 To EndX - 1 Do
    For j := 0 To Height - 1 Do Begin
      fMap[i, GroundLine - j] := c;
    End;
  // Malen der Fenster
  For j := 0 To (height Div 18) - 1 Do
    For i := 0 To ((endx - startx) Div 10) - 1 Do Begin
      c := WindowColors[random(high(WindowColors) + 1)];
      For x := 3 To 6 Do
        For y := 3 To 10 Do Begin
          fMap[startx + i * 10 + x, GroundLine - height + j * 18 + y] := c;
        End;
    End;
End;

Procedure TGorilla.SetUfo;
Var
  i, j, x, y: integer;
  TempIntfImg: TLazIntfImage;
  c: Tcolor;
Begin
  x := round(fUfoPos.x);
  y := round(fUfoPos.y);
  x := x - fufographic.Width Div 2;
  TempIntfImg := TLazIntfImage.Create(0, 0);
  TempIntfImg.LoadFromBitmap(fufographic.Handle, fufographic.MaskHandle);
  For i := 0 To fufographic.Width - 1 Do
    For j := 0 To fufographic.Height - 1 Do Begin
      c := FPColorToTColor(TempIntfImg.Colors[i, j]);
      If (c <> UFOTransparentColor) Then Begin
        If (x + i >= 0) And (x + i < 640) And (y + j >= 0) And (y + j < 480) Then Begin
          fMap[x + i, y + j] := c;
        End;
      End;
    End;
  TempIntfImg.free;
End;

Procedure TGorilla.NewRound;
Var
  j, i, h: integer;
  tmp: TVector2;
  c: integer;
Begin
  // dem Client sagen, dass alles neu initialisert werden muss
  If FNetwork.active Then Begin
    fPlayerCount := 2; // im Netzwerk sind es immer 2 Spieler, egal was vorher eingestellt war
    If FNetwork.isServer Then Begin
      c := Random($7FFFFFFF);
      RandSeed := c;
      fPlayerCount := 2; // das ist bisher falsch initialisiert, damit es bei den Dialogen einfacher ist..
      // Dem Client Klar machen, dass es weiter geht.
      SendMessage(opStartGame, inttostr(c));
    End
    Else Begin
      // Der Client übernimmt das Seed aus Schmierint
      RandSeed := ftemporaryInt;
    End;
  End;
  // Den Nächsten Spieler Wählen der Dran ist
  fSun := GetTickCount - SunScareTime;
  fActualPlayer := random(max(2, fPlayerCount)) + 1;
  fWindStrength := v2(random(2 * fmaxWindStrength) - fmaxWindStrength, 0);
  // Init der Spieler
  For i := 0 To max(fPlayerCount - 1, 1) Do Begin
    fPlayerAlive[i] := true;
    fLastPlayerAngle[i] := 0;
    fLastPlayerPower[i] := 0;
  End;
  // Löschen von allem was auf der Karte ist
  For i := 0 To high(fmap) Do
    For j := 0 To high(fmap[0]) Do
      fMap[i, j] := BackGround;
  // Die Spielfeldbreite mit Gebäuden füllen
  j := 0;
  i := RandomRange(minBuildingWidth, maxBuildingWidth);
  fplayerpos[0].X := -1;
  fplayerpos[1].X := -1;
  fplayerpos[2].X := -1;
  fplayerpos[3].X := -1;
  While (i <= 640 - minBuildingWidth) Do Begin
    h := RandomRange(minBuildingHeight, maxBuildingHeight);
    SetBuilding(j, i, h);
    // Die Positionen der Spieler
    If (j <= 80) And (i >= 80) Then Begin
      fplayerpos[0].X := (i + j) Div 2;
      fplayerpos[0].y := GroundLine - h;
    End;
    If (i >= 640 - 80) And (fplayerpos[1].X = -1) Then Begin
      fplayerpos[1].X := (i + j) Div 2;
      fplayerpos[1].y := GroundLine - h;
    End;
    If (i >= 320 - maxBuildingWidth Div 2) And (fplayerpos[2].X = -1) And (fPlayerCount = 3) Then Begin
      fplayerpos[2].X := (i + j) Div 2;
      fplayerpos[2].y := GroundLine - h;
    End;
    If (i >= 250 - maxBuildingWidth Div 2) And (fplayerpos[2].X = -1) And (fPlayerCount = 4) Then Begin
      fplayerpos[2].X := (i + j) Div 2;
      fplayerpos[2].y := GroundLine - h;
    End;
    If (i >= 450 - maxBuildingWidth Div 2) And (fplayerpos[3].X = -1) Then Begin
      fplayerpos[3].X := (i + j) Div 2;
      fplayerpos[3].y := GroundLine - h;
    End;
    j := i;
    i := i + RandomRange(minBuildingWidth, maxBuildingWidth);
  End;
  h := RandomRange(minBuildingHeight, maxBuildingHeight);
  If (fplayerpos[1].X = -1) Then Begin
    fplayerpos[1].X := (640 + j) Div 2;
    fplayerpos[1].y := GroundLine - h;
  End;
  SetBuilding(j, 640, h);
  // Das Ufo Plazieren oder nicht
  fUfoPos := v2(-1, -1);
  If Random(100) > 50 Then Begin
    fUfoPos := v2(RandomRange(20, 620), RandomRange(minufoypos, maxufoypos));
    SetUfo();
  End;
  ftemporaryString := inttostr(fLastPlayerAngle[fActualPlayer - 1]);
  If ftemporaryString = '0' Then ftemporaryString := '';
  //  In Abhängigkeit von Playercount nun noch die Fplayerpos Tauschen
  For i := 0 To 25 Do Begin
    j := random(max(2, fPlayerCount));
    h := random(max(2, fPlayerCount));
    If h <> j Then Begin
      tmp := fplayerpos[j];
      fplayerpos[j] := fplayerpos[h];
      fplayerpos[h] := tmp;
    End;
  End;
  fActualRound := fActualRound + 1;
  If fActualRound > fRoundCount Then Begin
    fActualRound := fRoundCount;
    ShowStats();
    exit;
  End;
  If FNetwork.active Then Begin
    If FNetwork.isServer Then Begin
      // Der Server wartet bis der Client Fertig ist, und angibt, dass er nun die "Restdaten" bracht.
    End
    Else Begin
      // Der Client bestätigt, dass er nun auch soweit ist.
      SendMessage(opClientReadyToPlay, '');
      fGorillaState := gsEnterAngle;
    End;
  End
  Else Begin
    // Im 2,3 und 4-Spieler Mode gehts nun mit der Eingabe des Winkels Los
    fGorillaState := gsEnterAngle;
    // Im 1 Spieler Mode ist evtl. die Ki dran.
    If (fPlayerCount = 1) And (Not FNetwork.active) Then Begin
      fPlayerNames[1] := 'Computer';
      // Falls die KI den Zug beginnt
      fai.initialized := false;
      If (fActualPlayer = 2) Then
        Do_AI_move();
    End;
  End;
End;

Procedure TGorilla.RenderBanana;
Var
  i, j: integer;
Begin
  (*
   * Zeichnen und Rotation der Banane
   *)
  If GetTickCount - fLastTime > BananaRotTime Then Begin
    fLastTime := GetTickCount;
    FBanana.Rotation := (FBanana.Rotation + 22) Mod 360;
  End;
  glPushMatrix();
  glTranslatef(FBanana.Position.x, FBanana.Position.y, 0);
  glPushMatrix();
  glRotatef(FBanana.Rotation, 0, 0, 1);
  SetOpenGLColor(BananaColor);
  glbegin(GL_POINTS);
  // die Banane muss wegen den rotationen "übersampelt" werden, sonst entstehen während der Drehung Löcher
  For i := 2 * low(fBananaTexture) To 2 * high(fBananaTexture) Do
    For j := 2 * low(fBananaTexture[0]) To 2 * high(fBananaTexture[0]) Do
      If fBananaTexture[i Div 2, j Div 2] Then Begin
        glVertex2f(i / 2, j / 2);
      End;
  glend;
  glPopMatrix();
  glPopMatrix();
End;

Procedure TGorilla.CollideBanana;

  Procedure HitPlayer(index: integer);
  Begin
    fPlayerAlive[index] := false;
    If (fActualPlayer - 1) = index Then Begin
      // Der Selfkill
      fPlayerPoints[fActualPlayer - 1] := fPlayerPoints[fActualPlayer - 1] - 1;
    End
    Else Begin
      // Ein echter Kill
      fPlayerPoints[fActualPlayer - 1] := fPlayerPoints[fActualPlayer - 1] + 1;
    End;
    EraseElippseInMap(round(fplayerpos[index].x), round(fplayerpos[index].y - 14), PlayerDeadRadiusX, PlayerDeadRadiusY);
  End;

Var
  dt: Single;
  NextTurn: Boolean;
  i, j: integer;
{$IFDEF ImprovedCollisionDetection}
  tl, tr, bl, br, dummy: TVector2;
{$ENDIF}
Begin
  NextTurn := false;
  (*
   * Bewegung der Banane
   *)
  dt := 0.017;
  FBanana.Speed := FBanana.Speed + FBanana.Acceleration + (dt * fWindStrength);
  FBanana.Acceleration := FBanana.Acceleration + dt * fGravity;
{$IFDEF ImprovedCollisionDetection}
  FBanana.OldPosition := FBanana.Position; // Damit die Genauere Flugbahnkollision gemacht werden kann, muss jeweils die Alte Position mit geführt werden.
{$ENDIF}
  FBanana.Position := FBanana.Position + dt * FBanana.Speed;
  // Vernünftige Abbruch Bedingung, für das Verlassen des Screens
  If (FBanana.Position.y > 480) Or // zu weit unten
  (FBanana.Position.x > 640 + BananaKilldist) Or // zu weit Rechts
  (FBanana.Position.x < -BananaKilldist) Or // zu Weit Links
  ((abs(fGravity) < BananaKillspeed) And (abs(FBanana.Speed) < BananaKillspeed)) {// Zu Langsam ( Eigentlich alles = 0 ), ohne diese Prüfung, würde die Banne Ewig fliegen und sich nicht bewegen..} Then Begin
    NextTurn := true;
    If fActualPlayer = 2 Then Begin
      fai.initialized := true;
      fai.finish_reason := ai_out_of_sceen;
      fai.lastthrowpos := FBanana.Position;
    End;
  End;
  (*
   * Kollision mit Gebäuden / Ufo
   *)
  i := round(FBanana.Position.x);
  j := round(FBanana.Position.y);
  If (i >= low(fmap)) And (i <= High(fmap)) And (j >= low(fmap[0])) And (j <= High(fmap[0])) Then Begin
    If fMap[i, j] <> BackGround Then Begin
      NextTurn := true;
      EraseElippseInMap(i, j, BananaDamageRadius, BananaDamageRadius);
      If fActualPlayer = 2 Then Begin
        fai.initialized := true;
        fai.finish_reason := ai_hit_building;
        fai.lastthrowpos := FBanana.Position;
        If (j < fUfoPos.y + fufographic.Height) And (fUfoPos.y <> -1) Then Begin
          fai.finish_reason := ai_hit_ufo;
        End;
      End;
    End;
  End;
  (*
   * Kollision mit Spielern
   *)
  For i := 0 To max(1, fPlayerCount - 1) Do Begin
    // Nur Lebendige Spieler können Kolidieren
    If fPlayerAlive[i] Then Begin
{$IFDEF ImprovedCollisionDetection}
      // Die Verbesserte Bananen Kollision Rechnet eine Flugbahn Stückelung in Geradensequenzen und kollidiert diese mit der Boundingbox der Gorillas
      tl := fplayerpos[i] + v2(-PlayerColideWidth, 0);
      tr := fplayerpos[i] + v2(PlayerColideWidth, 0);
      bl := fplayerpos[i] + v2(-PlayerColideWidth, -PlayerColideHeight);
      br := fplayerpos[i] + v2(PlayerColideWidth, -PlayerColideHeight);
      If Not Nextturn And IntersectLine_segments(FBanana.OldPosition, FBanana.Position, tl, tr, dummy) Then Begin
        HitPlayer(i);
        NextTurn := true;
      End;
      If Not Nextturn And IntersectLine_segments(FBanana.OldPosition, FBanana.Position, tr, br, dummy) Then Begin
        HitPlayer(i);
        NextTurn := true;
      End;
      If Not Nextturn And IntersectLine_segments(FBanana.OldPosition, FBanana.Position, br, bl, dummy) Then Begin
        HitPlayer(i);
        NextTurn := true;
      End;
      If Not Nextturn And IntersectLine_segments(FBanana.OldPosition, FBanana.Position, bl, tl, dummy) Then Begin
        HitPlayer(i);
        NextTurn := true;
      End;
{$ELSE}
      // Der Kopf, der Körper und zwischen den Beinen ist "SterbeFläche"
      If PointInRect(FBanana.Position, fplayerpos[i] + v2(-PlayerColideWidth, 0), fplayerpos[i] + v2(PlayerColideWidth, -PlayerColideHeight)) Then Begin
        HitPlayer(i);
        NextTurn := true;
      End;
{$ENDIF}
    End;
  End;
  (*
   * Kollision mit der Sonne
   *)
  If PointInRect(FBanana.Position, fSunPos - v2(15, 20), fSunPos + v2(15, 20)) Then Begin
    fSun := GetTickCount;
  End;
  (*
   * Wenn der Flug Offiziell beendet ist
   *)
  If (NextTurn) Then Begin
    // Umschalten auf einen Spielder der Lebt
    Repeat
      fActualPlayer := ((fActualPlayer) Mod (max(2, fPlayerCount))) + 1;
    Until fPlayerAlive[fActualPlayer - 1];
    ftemporaryString := IntToStr(fLastPlayerAngle[fActualPlayer - 1]);
    If ftemporaryString = '0' Then ftemporaryString := '';
    fGorillaState := gsEnterAngle;
    j := 0;
    For i := 0 To max(fPlayerCount - 1, 1) Do Begin
      If fPlayerAlive[i] Then inc(j);
    End;
    (*
     * Nur noch 1 Spieler übrig, das Spiel wird beendet.
     *)
    If (j = 1) Then Begin
      ftemporaryInt := 0;
      fGorillaState := gsVictoryDance; // Starten der Spiel Vorbei Animation
    End
    Else Begin
      // im KI-Modus den Computer Spielen lassen
      If (fPlayerCount = 1) And (fActualPlayer = 2) And (Not (fnetwork.active)) Then Begin
        Do_AI_move();
      End;
    End;
  End;
End;

Procedure TGorilla.RenderWindStrength;
Var
  w: Single;
  i: integer;
Begin
  SetOpenGLColor(WindColor);
  If abs(fWindStrength) > 10 Then Begin
    glbegin(GL_LINES);
    glVertex2f(320, 470);
    glVertex2f(320 + fWindStrength.x, 470 + fWindStrength.y);
    glend();
    w := ArcTangens(fWindStrength.x, fWindStrength.y);
    glPushMatrix();
    glTranslatef(320 + fWindStrength.x, 471 + fWindStrength.y, 0);
    glPushMatrix();
    glRotatef(w, 0, 0, 1);
    glBegin(GL_POINTS);
    For i := 1 To 5 Do Begin
      glVertex2i(-i, i);
      glVertex2i(-i, -i);
    End;
    glend();
    glPopMatrix();
    glPopMatrix();
  End;
End;

Procedure TGorilla.RenderSun;
Var
  i, j: integer;
Begin
  SetOpenGLColor(SunColor);
  glPushMatrix();
  glTranslatef(fSunPos.x, fSunPos.y, 0);
  glPushMatrix();
  glBegin(GL_POINTS);
  If GetTickCount - fSun < SunScareTime Then Begin
    For i := low(fSunScareTexture) To high(fSunScareTexture) Do
      For j := low(fSunScareTexture[0]) To high(fSunScareTexture[0]) Do
        If fSunScareTexture[i, j] Then glVertex2i(i, 20 - j);
  End
  Else Begin
    For i := low(fSunNormalTexture) To high(fSunNormalTexture) Do
      For j := low(fSunNormalTexture[0]) To high(fSunNormalTexture[0]) Do
        If fSunNormalTexture[i, j] Then glVertex2i(i, 20 - j);
  End;
  glend;
  glPopMatrix();
  glPopMatrix();
End;

Procedure TGorilla.Render;
Var
  t, s: String;
  h, w, w2: integer;
  i, j: integer;
  l, r: TGorillaArmState;
Begin
  h := round(OpenGL_ASCII_Font.TextHeight('8'));
  Case fGorillaState Of
    gsWaitForServerToStart: Begin
        s := 'Client';
        OpenGL_ASCII_Font.Color := clwhite;
        OpenGL_ASCII_Font.Textout((320 - round(OpenGL_ASCII_Font.TextWidth(s)) Div 2), 60, s);
        s := 'You have to wait, until the server starts the game.';
        OpenGL_ASCII_Font.Color := clwhite;
        OpenGL_ASCII_Font.Textout((320 - round(OpenGL_ASCII_Font.TextWidth(s)) Div 2), 100, s);
        s := 'Use the "Tab" key to open a chat window.';
        OpenGL_ASCII_Font.Color := clwhite;
        OpenGL_ASCII_Font.Textout((320 - round(OpenGL_ASCII_Font.TextWidth(s)) Div 2), 380, s);
        (*
         * Alles zum Thema Namen Eingeben
         *)
        s := 'Please enter the max wind strength (default 100 m/s): '; //Dieser Text dient nur der "Breiten" Bestimmung
        w := round(OpenGL_ASCII_Font.TextWidth(s)) Div 2;
        For i := 1 To 2 Do Begin
          If (i = ftemporaryInt) {And (fGorillaState = gsEnterName) } Then Begin
            s := 'Please enter the name of player' + inttostr(ftemporaryInt) + ' : '; // Nur zur Berechnung von w2
            w2 := round(OpenGL_ASCII_Font.TextWidth(s));
            s := fPlayerNames[ftemporaryInt - 1];
            If GetTickCount - fLastTime < 500 Then s := s + '_';
            If GetTickCount - fLastTime > 1000 Then fLastTime := GetTickCount;
            OpenGL_ASCII_Font.Color := clLtGray;
            OpenGL_ASCII_Font.Textout(300 - w + w2, 120 + (i - 1) * h, s);
            w2 := round(OpenGL_ASCII_Font.TextWidth(s));
            s := 'Please enter the name of player' + inttostr(ftemporaryInt) + ' : ';
          End
          Else Begin
            s := format('Player%d : %s', [i, fPlayerNames[i - 1]]);
          End;
          OpenGL_ASCII_Font.Color := clwhite;
          OpenGL_ASCII_Font.Textout(300 - w, 120 + (i - 1) * h, s);
        End;
        (*
         * Alles zum Thema Rundenanzahl
         *)
        s := 'Rounds to play : ' + IntToStr(fRoundCount);
        OpenGL_ASCII_Font.Color := clwhite;
        OpenGL_ASCII_Font.Textout(300 - w, 100 + (5) * h, s);
        (*
         * Alles zum Thema Eingabe Schwerkraft
         *)
        s := format('Used Gravity : %d m/s^2', [round(fGravity.y)]);
        OpenGL_ASCII_Font.Color := clwhite;
        OpenGL_ASCII_Font.Textout(300 - w, 100 + (7) * h, s);
        (*
         * Alles zum Thema Eingabe Wind
         *)
        s := format('Used max wind strength : %d m/s', [FmaxWindStrength]);
        OpenGL_ASCII_Font.Color := clwhite;
        OpenGL_ASCII_Font.Textout(300 - w, 100 + (9) * h, s);
      End;
    gsQuestionIP,
      gsWaitForClient,
      gsQuestionPort: Begin
        If FNetwork.isServer Then Begin
          s := 'Server';
        End
        Else Begin
          s := 'Client';
        End;
        OpenGL_ASCII_Font.Color := clwhite;
        OpenGL_ASCII_Font.Textout((320 - round(OpenGL_ASCII_Font.TextWidth(s)) Div 2), 60, s);
        s := 'Please enter the Port you want to use (0..65536) : ';
        (*
         * Alles zum Thema Port Eingabe
         *)
        w := round(OpenGL_ASCII_Font.TextWidth(s));
        If fGorillaState = gsQuestionPort Then Begin
          w2 := round(OpenGL_ASCII_Font.TextWidth(s));
          s := ftemporaryString;
          If GetTickCount - fLastTime < 500 Then s := s + '_';
          If GetTickCount - fLastTime > 1000 Then fLastTime := GetTickCount;
          OpenGL_ASCII_Font.Color := clLtGray;
          OpenGL_ASCII_Font.Textout(320 - w Div 2 + w2, 100, s);
          s := 'Please enter the Port you want to use (0..65536) : ';
        End
        Else Begin
          s := 'Used Port : ' + inttostr(FNetwork.port);
        End;
        OpenGL_ASCII_Font.Color := clwhite;
        OpenGL_ASCII_Font.Textout(320 - w Div 2, 100, s);
        (*
         * Der Server Warted auf den Client
         *)
        If fGorillaState = gsWaitForClient Then Begin
          s := 'Waiting for client to connect...';
          OpenGL_ASCII_Font.Color := clwhite;
          OpenGL_ASCII_Font.Textout(320 - w Div 2, 100 + 3 * h, s);
        End;
        (*
         * Der Client gibt die Server IP ein
         *)
        If Not FNetwork.isServer Then Begin
          If (fGorillaState = gsQuestionIP) Then Begin
            s := 'Please enter the server IP-Address : ';
            w2 := round(OpenGL_ASCII_Font.TextWidth(s));
            s := ftemporaryString;
            If GetTickCount - fLastTime < 500 Then s := s + '_';
            If GetTickCount - fLastTime > 1000 Then fLastTime := GetTickCount;
            OpenGL_ASCII_Font.Color := clLtGray;
            OpenGL_ASCII_Font.Textout(320 - w Div 2 + w2, 100 + h, s);
            s := 'Please enter the server IP-Address : ';
          End
          Else Begin
            s := 'Used server IP-Address : ' + FNetwork.ServerIP;
          End;
          OpenGL_ASCII_Font.Color := clwhite;
          OpenGL_ASCII_Font.Textout(320 - w Div 2, 100 + h, s);
        End;
      End;
    gsSelectNetworkMode: Begin
        s :=
          '                 Please choose' + LineEnding +
          LineEnding +
          '  S = Server (need port forwarding in www mode)' + LineEnding +
          '  C = Client (need server IP)' + LineEnding +
          LineEnding +
          '  Q = Back to start screen';
        OpenGL_ASCII_Font.Color := clwhite;
        OpenGL_ASCII_Font.Textout((320 - round(OpenGL_ASCII_Font.TextWidth(s)) Div 2), 100, s);
      End;
    gsViewPlayerStats: Begin
        s := format(
          '          Player stats ' + LineEnding + LineEnding +
          '     Total rounds :  %3d' + LineEnding +
          '     Played rounds : %3d' + LineEnding, [fRoundCount, fActualRound]);
        s := s + LineEnding + LineEnding + '     Name        Points' + LineEnding + LineEnding;
        For i := 0 To max(1, fPlayerCount - 1) Do Begin
          s := s + format('     %10s   %3d' + LineEnding, [fPlayerNames[i], fPlayerPoints[i]]);
        End;
        s := s + LineEnding + LineEnding + LineEnding + LineEnding + LineEnding + 'Press any key to go to main menu.';
        OpenGL_ASCII_Font.Color := clwhite;
        OpenGL_ASCII_Font.Textout((320 - round(OpenGL_ASCII_Font.TextWidth(s)) Div 2), 100, s);
      End;
    gsStartScreen: Begin
        s :=
          '            Gorilla ver. ' + floattostrf(GorillaVersion / 100, fffixed, 7, 2) + LineEnding +
          LineEnding +
          'remake by Corpsman, support : www.Corpsman.de' + LineEnding +
          LineEnding +
          'Please choose :' + LineEnding +
          LineEnding +
          '  1 = Single player (against ai)' + LineEnding +
          '  2 = Head to head (the classic version)' + LineEnding +
          '  3 = three are two too much' + LineEnding +
          '  4 = last man standing' + LineEnding +
          LineEnding +
          '  N = Network mode' + LineEnding +
          LineEnding +
          LineEnding +
          '  H = Show help (ingame)' + LineEnding +
          '  Q = Goto start screen / quit (anytime)' + LineEnding +
          LineEnding +
          'ESC = Quit (anytime)';
        OpenGL_ASCII_Font.Color := clwhite;
        OpenGL_ASCII_Font.Textout((320 - round(OpenGL_ASCII_Font.TextWidth(s)) Div 2), 100, s);
        j := 0;
        If GetTickCount - fLastTime < 500 Then j := 1;
        If GetTickCount - fLastTime > 1000 Then fLastTime := GetTickCount;
        RenderGorilla(v2(50, 100), GorillaColors[0], TGorillaArmState(j), TGorillaArmState(1 - j));
        RenderGorilla(v2(590, 100), GorillaColors[0], TGorillaArmState(1 - j), TGorillaArmState(j));
      End;
    gsEnterGravity,
      gsEnterMaxWind,
      gsEnterRoundCount,
      gsEnterName: Begin
        (*
         * Alles zum Thema Namen Eingeben
         *)
        s := 'Please enter the max wind strength (default 100 m/s): '; //Dieser Text dient nur der "Breiten" Bestimmung
        w := round(OpenGL_ASCII_Font.TextWidth(s)) Div 2;
        For i := 1 To fPlayerCount Do Begin
          If (i = ftemporaryInt) And (fGorillaState = gsEnterName) Then Begin
            s := 'Please enter the name of player' + inttostr(ftemporaryInt) + ' : '; // Nur zur Berechnung von w2
            w2 := round(OpenGL_ASCII_Font.TextWidth(s));
            s := fPlayerNames[ftemporaryInt - 1];
            If GetTickCount - fLastTime < 500 Then s := s + '_';
            If GetTickCount - fLastTime > 1000 Then fLastTime := GetTickCount;
            OpenGL_ASCII_Font.Color := clLtGray;
            OpenGL_ASCII_Font.Textout(300 - w + w2, 100 + (i - 1) * h, s);
            w2 := round(OpenGL_ASCII_Font.TextWidth(s));
            s := 'Please enter the name of player' + inttostr(ftemporaryInt) + ' : ';
          End
          Else Begin
            s := format('Player%d : %s', [i, fPlayerNames[i - 1]]);
          End;
          OpenGL_ASCII_Font.Color := clwhite;
          OpenGL_ASCII_Font.Textout(300 - w, 100 + (i - 1) * h, s);
        End;
        (*
         * Im Netzwerkmodus wird der Spieler 2 Name von Hand gezeichnet
         *)
        If FNetwork.active Then Begin
          s := 'Server';
          OpenGL_ASCII_Font.Color := clwhite;
          OpenGL_ASCII_Font.Textout((320 - round(OpenGL_ASCII_Font.TextWidth(s)) Div 2), 60, s);
          s := format('Player%d : %s', [2, fPlayerNames[2 - 1]]);
          OpenGL_ASCII_Font.Color := clwhite;
          OpenGL_ASCII_Font.Textout(300 - w, 100 + (2 - 1) * h, s);
          s := 'Use the "Tab" key to open a chat window.';
          OpenGL_ASCII_Font.Color := clwhite;
          OpenGL_ASCII_Font.Textout((320 - round(OpenGL_ASCII_Font.TextWidth(s)) Div 2), 380, s);
        End;
        (*
         * Alles zum Thema Round count
         *)
        If (fGorillaState = gsEnterRoundCount) Then Begin
          s := 'Please enter the number of rounds to play : ';
          OpenGL_ASCII_Font.Color := clwhite;
          OpenGL_ASCII_Font.Textout(300 - w, 100 + (5) * h, s);
          w2 := round(OpenGL_ASCII_Font.TextWidth(s));
          s := ftemporaryString;
          If GetTickCount - fLastTime < 500 Then s := s + '_';
          If GetTickCount - fLastTime > 1000 Then fLastTime := GetTickCount;
          OpenGL_ASCII_Font.Color := clLtGray;
          OpenGL_ASCII_Font.Textout(300 - w + w2, 100 + (5) * h, s);
        End
        Else Begin
          s := 'Rounds to play : ' + IntToStr(fRoundCount);
          OpenGL_ASCII_Font.Color := clwhite;
          OpenGL_ASCII_Font.Textout(300 - w, 100 + (5) * h, s);
        End;
        (*
         * Alles zum Thema Eingabe Schwerkraft
         *)
        If (fGorillaState = gsEnterGravity) Then Begin
          s := 'Please enter the gravity (default 10 m/s^2): ';
          OpenGL_ASCII_Font.Color := clwhite;
          OpenGL_ASCII_Font.Textout(300 - w, 100 + (7) * h, s);
          w2 := round(OpenGL_ASCII_Font.TextWidth(s));
          s := ftemporaryString;
          If GetTickCount - fLastTime < 500 Then s := s + '_';
          If GetTickCount - fLastTime > 1000 Then fLastTime := GetTickCount;
          OpenGL_ASCII_Font.Color := clLtGray;
          OpenGL_ASCII_Font.Textout(300 - w + w2, 100 + (7) * h, s);
        End
        Else Begin
          s := format('Used Gravity : %d m/s^2', [round(fGravity.y)]);
          OpenGL_ASCII_Font.Color := clwhite;
          OpenGL_ASCII_Font.Textout(300 - w, 100 + (7) * h, s);
        End;
        (*
         * Alles zum Thema Eingabe Wind
         *)
        If (fGorillaState = gsEnterMaxWind) Then Begin
          s := 'Please enter the max wind strength (default 100 m/s): ';
          OpenGL_ASCII_Font.Color := clwhite;
          OpenGL_ASCII_Font.Textout(300 - w, 100 + (9) * h, s);
          w2 := round(OpenGL_ASCII_Font.TextWidth(s));
          s := ftemporaryString;
          If GetTickCount - fLastTime < 500 Then s := s + '_';
          If GetTickCount - fLastTime > 1000 Then fLastTime := GetTickCount;
          OpenGL_ASCII_Font.Color := clLtGray;
          OpenGL_ASCII_Font.Textout(300 - w + w2, 100 + (9) * h, s);
        End
        Else Begin
          s := format('Used max wind strength : %d m/s', [FmaxWindStrength]);
          OpenGL_ASCII_Font.Color := clwhite;
          OpenGL_ASCII_Font.Textout(300 - w, 100 + (9) * h, s);
        End;
      End;
    (*
     * Im Spiel..
     *)
    gsVictoryDance,
      gsStartThrow, gsThrow,
      gsEnterAngle, gsEnterPower: Begin
        // 1. Rendern der Karte
        glBegin(GL_QUADs);
        SetOpenGLColor(BackGround);
        glVertex2i(0, 0);
        glVertex2i(640, 0);
        glVertex2i(640, 480);
        glVertex2i(0, 480);
        glend();
        glBegin(GL_POINTS);
        For i := 0 To high(fMap) Do
          For j := 0 To high(fmap[0]) Do Begin
            If fMap[i, j] <> BackGround Then Begin
              SetOpenGLColor(fMap[i, j]);
              glVertex2i(i, j);
            End;
          End;
        glend;
        // 2. Rendern der Sonne, Wind
        RenderSun();
        RenderWindStrength();
        For i := 0 To max(fPlayerCount - 1, 1) Do Begin
          // 3. Rendern des Aktuellen Spielers
          If (fGorillaState = gsStartThrow) And (i = fActualPlayer - 1) Then Begin
            l := gaDown;
            r := gaDown;
            If FBanana.Speed.x >= 0 Then
              r := gaUp
            Else
              l := gaUp;
            If GetTickCount - fLastTime > ArmUpTime Then Begin
              fGorillaState := gsThrow;
            End;
          End
          Else Begin
            l := gaDown;
            r := gaDown;
            If (fGorillaState = gsVictoryDance) Then Begin
              If GetTickCount - fLastTime > (VictoryDanceTime Div 2) Then Begin
                l := gaUp;
              End;
              If GetTickCount - fLastTime > VictoryDanceTime Then Begin
                fLastTime := GetTickCount;
                ftemporaryInt := ftemporaryInt + 1;
                If (ftemporaryInt > maxVictoryDanceTime) Then Begin
                  If FNetwork.active Then Begin
                    // Das Verhindert dass der Client evtl. Schneller als der Server eine neue Runde startet und damit den Server durcheinander bringt.
                    If FNetwork.isServer Then Begin
                      NewRound(); // das Selbe wird noch mal durch onKey Down ausgelöst
                    End;
                  End
                  Else Begin
                    NewRound(); // das Selbe wird noch mal durch onKey Down ausgelöst
                  End;
                End;
              End;
              r := TGorillaArmState(1 - integer(l));
            End;
          End;
          // nur lebendige Gorillas werden Gezeichnet
          If fPlayerAlive[i] Then
            RenderGorilla(fplayerpos[i], GorillaColors[i], l, r);
          // 4. Rendern der aktuellen Spieler Informationen und Texte
          s := fPlayerNames[i] + ' : ' + inttostr(fPlayerPoints[i]);
          OpenGL_ASCII_Font.Color := GorillaColors[i];
          OpenGL_ASCII_Font.Textout(10, 10 + i * round(OpenGL_ASCII_Font.TextHeight('8')), s);
        End;
        // Wenn die Banane gerade Fliegt, dann muss sie Bewegt werden und Kolisionen auslösen
        If fGorillaState = gsThrow Then Begin
          RenderBanana();
          CollideBanana();
        End;
        // Rendern der Rundeninfo
        s := format('(%d/%d)', [fActualRound, fRoundCount]);
        OpenGL_ASCII_Font.Color := clWhite;
        OpenGL_ASCII_Font.Textout(320 - (round(OpenGL_ASCII_Font.TextWidth(s)) Div 2), 10, s);
        // Rendern der Dialog informationen
        s := 'Velocity : 0000 '; // Festlegen der "Breite"
        //s := 'Angle : 0000 ';  // ist Kürzer als das Obere, deswegen ausgeblendet
        OpenGL_ASCII_Font.Color := GorillaColors[fActualPlayer - 1];
        w := round(OpenGL_ASCII_Font.TextWidth(s));
        s := fPlayerNames[fActualPlayer - 1];
        w := max(w, round(OpenGL_ASCII_Font.TextWidth(s)));
        OpenGL_ASCII_Font.Textout(640 - w, 10, s);
        s := '';
        If fGorillaState = gsEnterAngle Then Begin
          s := 'Angle : ';
        End;
        If fGorillaState = gsEnterPower Then Begin
          s := 'Velocity : ';
        End;
        If s <> '' Then Begin
          t := ftemporaryString;
          If FNetwork.active Then Begin
            // Das Blinken natürlich nur, wenn der Aktuelle Spieler auch Berechtigt ist zu editieren
            If ((fActualPlayer = 1) And FNetwork.isServer) Or
              ((fActualPlayer = 2) And Not FNetwork.isServer) Then Begin
              If GetTickCount - fLastTime < 500 Then t := t + '_';
            End;
          End
          Else Begin
            If GetTickCount - fLastTime < 500 Then t := t + '_';
          End;
          If GetTickCount - fLastTime > 1000 Then fLastTime := GetTickCount;
          OpenGL_ASCII_Font.Textout(640 - w, 10 + round(OpenGL_ASCII_Font.TextHeight('8')), s);
          OpenGL_ASCII_Font.Color := clLtGray;
          OpenGL_ASCII_Font.Textout(640 + round(OpenGL_ASCII_Font.TextWidth(s)) - w, 10 + round(OpenGL_ASCII_Font.TextHeight('8')), t);
        End;
      End;
  End;
End;

Procedure TGorilla.OnAnglePowerReturn;
Var
  i: integer;
Begin
  Case fGorillaState Of
    gsEnterAngle: Begin
        i := StrToIntDef(ftemporaryString, 0);
        fLastPlayerAngle[fActualPlayer - 1] := i;
        fGorillaState := gsEnterPower;
        ftemporaryString := inttostr(fLastPlayerPower[fActualPlayer - 1]);
        If ftemporaryString = '0' Then ftemporaryString := '';
      End;
    gsEnterPower: Begin
        i := StrToIntDef(ftemporaryString, 0);
        (*
         * Start eines Wurfes mit allen Notwendigen Initialisierungen
         *)
        fLastPlayerPower[fActualPlayer - 1] := i;
        ftemporaryString := '';
        fGorillaState := gsStartThrow;
        fLastTime := GetTickCount;
        FBanana.Speed := V2(cos(degtorad(fLastPlayerAngle[fActualPlayer - 1])) * fLastPlayerPower[fActualPlayer - 1], -sin(degtorad(fLastPlayerAngle[fActualPlayer - 1])) * fLastPlayerPower[fActualPlayer - 1]);
        FBanana.Acceleration := v2(0, 0);
        FBanana.Rotation := random(360);
        FBanana.Position := v2(fplayerpos[fActualPlayer - 1].X, fplayerpos[fActualPlayer - 1].Y - 30);
{$IFDEF ImprovedCollisionDetection}
        FBanana.OldPosition := FBanana.Position; // Damit die Genauere Flugbahnkollision gemacht werden kann, muss jeweils die Alte Position mit geführt werden.
{$ENDIF}
      End;
  End;
End;

Procedure TGorilla.OnKeyDown(Var Key: Word; Shift: TShiftState);
Begin
  // ESC - Beendet immer.
  If (key = vk_escape) Then application.Terminate;
  // Im Netzwerkmodus kann die Tab Taste das Chat Fenster wieder zeigen
  If FNetwork.active And (key = VK_TAB) Then Begin
    If Not form3.Visible Then
      form3.Show;
  End;
  Case fGorillaState Of
    gsSelectNetworkMode: Begin
        If key = vk_q Then Begin
          fGorillaState := gsStartScreen; // Zurück ins Main Menü
        End;
        If Key = vk_S Then Begin
          // Der Server Startet
          FNetwork.isServer := true;
          ftemporaryString := IntToStr(FNetwork.port);
          fGorillaState := gsQuestionPort;
        End;
        If Key = vk_C Then Begin
          // Der Client Startet
          FNetwork.isServer := false;
          ftemporaryString := IntToStr(FNetwork.port);
          fGorillaState := gsQuestionPort;
        End;
      End;
    gsWaitForClient: Begin
        (*
         * Der Server wird hier in OnAccept weiter auf EnterName geschaltet
         *)
        If key = vk_q Then Begin
          fGorillaState := gsStartScreen; // Zurück ins Main Menü
          If Network.Connected Then Begin
            Network.Disconnect(true);
          End;
        End;
      End;
    gsWaitForServerToStart: Begin
        If Key = VK_BACK Then Begin
          delete(fPlayerNames[ftemporaryInt - 1], Length(fPlayerNames[ftemporaryInt - 1]), 1);
          SendMessage(OpClientChangedName, fPlayerNames[ftemporaryInt - 1]);
        End;
        // Namen sollen nur aus Groß, Klein Buchstaben und 0..9 bestehen.
        If (key In [VK_A..VK_Z]) Or
          (key In [VK_0..VK_9]) Or
          (key In [VK_NUMPAD0..VK_NUMPAD9]) Then Begin
          // Die Kleinen Buchstaben
          If (key In [VK_NUMPAD0..VK_NUMPAD9]) Then key := key - VK_NUMPAD0 + VK_0; // Umrechnen Numpad in zahlen
          If (key In [VK_A..VK_Z]) And Not (ssShift In Shift) Then key := key + 32;
          fPlayerNames[ftemporaryInt - 1] := fPlayerNames[ftemporaryInt - 1] + chr(Key);
          SendMessage(OpClientChangedName, fPlayerNames[ftemporaryInt - 1]);
        End;
      End;
    gsQuestionIP: Begin
        If key = vk_q Then Begin
          fGorillaState := gsStartScreen; // Zurück ins Main Menü
        End;
        // Eingabe des Portes
        If Key = VK_BACK Then Begin
          delete(ftemporaryString, Length(ftemporaryString), 1);
        End;
        If key = VK_OEM_PERIOD Then Begin
          ftemporaryString := ftemporaryString + '.';
        End;
        If (key In [VK_0..VK_9]) Or
          (key In [VK_NUMPAD0..VK_NUMPAD9]) Then Begin
          If (key In [VK_NUMPAD0..VK_NUMPAD9]) Then key := key - VK_NUMPAD0 + VK_0; // Umrechnen Numpad in zahlen
          ftemporaryString := ftemporaryString + chr(Key);
        End;
        If Key = VK_RETURN Then Begin
          FNetwork.ServerIP := ftemporaryString;
          If Network.Connected Then Begin
            Network.Disconnect(true);
          End;
          If Network.Connect(FNetwork.ServerIP, FNetwork.port) Then Begin
            //            form3.show;
            FNetwork.active := true;
            fGorillaState := gsWaitForServerToStart;
            ftemporaryString := fPlayerNames[1];
            ftemporaryInt := 2;
          End
          Else Begin
            showmessage('Error could not connect.');
            fGorillaState := gsStartScreen;
          End;
        End;
      End;
    gsQuestionPort: Begin
        If key = vk_q Then Begin
          fGorillaState := gsStartScreen; // Zurück ins Main Menü
        End;
        // Eingabe des Portes
        If Key = VK_BACK Then Begin
          delete(ftemporaryString, Length(ftemporaryString), 1);
        End;
        If (key In [VK_0..VK_9]) Or
          (key In [VK_NUMPAD0..VK_NUMPAD9]) Then Begin
          If (key In [VK_NUMPAD0..VK_NUMPAD9]) Then key := key - VK_NUMPAD0 + VK_0; // Umrechnen Numpad in zahlen
          ftemporaryString := ftemporaryString + chr(Key);
        End;
        If Key = VK_RETURN Then Begin
          FNetwork.port := strtointdef(ftemporaryString, 9876);
          If FNetwork.isServer Then Begin
            If Network.Connected Then Begin
              Network.Disconnect(true);
            End;
            Network.Listen(FNetwork.port);
            fGorillaState := gsWaitForClient; // Warten bis der Client Verbunden hat
          End
          Else Begin
            ftemporaryString := FNetwork.ServerIP;
            fGorillaState := gsQuestionIP;
          End;
        End;
      End;
    gsViewPlayerStats: Begin
        If FNetwork.active Then Begin
          // Im Netzwerkmodus kommen wir wieder in die Eingabe Maske
          If FNetwork.isServer Then Begin
            // Das Ganze so einstelllen, das der Server auch alles wieder richtig sieht ..
            ftemporaryInt := 1;
            fPlayerCount := 1;
            fGorillaState := gsEnterName;
          End
          Else Begin
            ftemporaryString := fPlayerNames[1];
            ftemporaryInt := 2;
            fPlayerCount := 0; // ist im Prinzip egal, sollte halt nicht 1 sein
            fGorillaState := gsWaitForServerToStart;
          End;
          SendMessage(opEndShowStats, ''); // Den Anderen zum Umschalten zwingen, falls er dies noch nicht hat
        End
        Else Begin
          fGorillaState := gsStartScreen; // Egal welche Taste Gedrückt wird, wir gehen zurück ins Main Menü
        End;
      End;
    gsVictoryDance: Begin
        If Key = vk_H Then Begin
          form2.show;
        End;
        If (key = VK_RETURN) Or (key = VK_SPACE) Then Begin
          NewRound(); // Das Selbe wird auch in Render automatisch ausgelöst..
        End;
        If (key = VK_Q) Then Begin
          ShowStats(); // Spielabbruch
        End;
      End;
    gsThrow, gsStartThrow: Begin
        If (key = VK_Q) Then Begin
          fActualRound := fActualRound - 1; // Die Runde wurde ja nicht beendet, zählt also nicht
          ShowStats(); // Spielabbruch
        End;
      End;
    gsEnterPower, gsEnterAngle: Begin
        If Key = vk_H Then Begin
          form2.show;
        End;
        If (key = VK_Q) Then Begin
          fActualRound := fActualRound - 1; // Die Runde wurde ja nicht beendet, zählt also nicht
          If FNetwork.active Then Begin
            SendMessage(opCancelRound, '');
          End;
          ShowStats(); // Spielabbruch
        End;
        If Key = VK_N Then Begin // Debugg
          If FNetwork.active And (Not FNetwork.isServer) Then Begin // Debugg
            exit; // Macht der Client das, dann haben wir ein Problem // Debugg
          End; // Debugg
          NewRound(); // Debugg
        End; // Debugg
        If Key = VK_BACK Then Begin
          // Prüfen ob der Spieler berechtigt ist den jeweiligen Wert zu editieren
          If FNetwork.active Then Begin
            If ((fActualPlayer = 2) And FNetwork.isServer) Or
              ((fActualPlayer = 1) And Not FNetwork.isServer) Then Begin
              exit;
            End;
          End;
          delete(ftemporaryString, Length(ftemporaryString), 1);
          If FNetwork.active Then Begin
            SendMessage(opUpdateSchmierstring, ftemporaryString);
          End;
        End;
        If key = VK_RETURN Then Begin
          // Prüfen ob der Spieler berechtigt ist den jeweiligen Wert zu editieren
          If FNetwork.active Then Begin
            If ((fActualPlayer = 2) And FNetwork.isServer) Or
              ((fActualPlayer = 1) And Not FNetwork.isServer) Then Begin
              exit;
            End;
            SendMessage(opReturnPressed, ''); // Dem Anderen Mitteilen das "Enter" gedrückt wurde.
          End;
          // Der Hier stehende Teil wurde wegen dem Netzwerkmodus ausgelagert, so kann OnReceive auch ein OnKeypress(vk_return, []) auslösen ohne Blockiert zu werden.
          OnAnglePowerReturn();
        End;
        If (key = VK_SUBTRACT) Or (key = VK_OEM_MINUS) Then Begin
          // Prüfen ob der Spieler berechtigt ist den jeweiligen Wert zu editieren
          If FNetwork.active Then Begin
            If ((fActualPlayer = 2) And FNetwork.isServer) Or
              ((fActualPlayer = 1) And Not FNetwork.isServer) Then Begin
              exit;
            End;
          End;
          If Length(ftemporaryString) = 0 Then Begin
            ftemporaryString := '-';
          End
          Else Begin
            If ftemporaryString[1] = '-' Then Begin
              delete(ftemporaryString, 1, 1);
            End
            Else Begin
              ftemporaryString := '-' + ftemporaryString;
            End;
          End;
          If FNetwork.active Then Begin
            SendMessage(opUpdateSchmierstring, ftemporaryString);
          End;
        End;
        // Die Rundenzahl bleigt nur eine Zahl
        If (key In [VK_0..VK_9]) Or
          (key In [VK_NUMPAD0..VK_NUMPAD9]) Then Begin
          If (key In [VK_NUMPAD0..VK_NUMPAD9]) Then key := key - VK_NUMPAD0 + VK_0; // Umrechnen Numpad in zahlen
          // Prüfen ob der Spieler berechtigt ist den jeweiligen Wert zu editieren
          If FNetwork.active Then Begin
            If ((fActualPlayer = 2) And FNetwork.isServer) Or
              ((fActualPlayer = 1) And Not FNetwork.isServer) Then Begin
              exit;
            End;
          End;
          ftemporaryString := ftemporaryString + chr(Key);
          If FNetwork.active Then Begin
            SendMessage(opUpdateSchmierstring, ftemporaryString);
          End;
        End;
      End;
    gsStartScreen: Begin
        If (key = VK_Q) Then Begin
          Application.Terminate;
        End;
        If (key In [VK_0..VK_9]) Or
          (key In [VK_NUMPAD0..VK_NUMPAD9]) Then Begin
          If (key In [VK_NUMPAD0..VK_NUMPAD9]) Then key := key - VK_NUMPAD0 + VK_0; // Umrechnen Numpad in zahlen
          fPlayerCount := Key - VK_1 + 1;
          ftemporaryInt := 1;
          fGorillaState := gsEnterName;
          fLastTime := GetTickCount;
          // Sicherstellen das das Netzwerk aus ist, falls es evtl irgendwo vergessen wurde..
          If Network.Connected Then Begin
            Network.Disconnect(true);
          End;
          FNetwork.active := false;
        End;
        If (key = vk_N) Then Begin
          fGorillaState := gsSelectNetworkMode;
        End;
      End;
    gsEnterGravity, gsEnterMaxWind,
      gsEnterRoundCount: Begin
        If (key = VK_Q) Then Begin
          fGorillaState := gsStartScreen;
          If FNetwork.active Then Begin
            Network.Disconnect(true);
          End;
        End;
        If Key = VK_BACK Then Begin
          delete(ftemporaryString, Length(ftemporaryString), 1);
          If FNetwork.active Then Begin
            Case fGorillaState Of
              gsEnterRoundCount: Begin
                  SendMessage(opServerChangedRoundsToPlay, ftemporaryString);
                End;
              gsEnterGravity: Begin
                  SendMessage(opServerChangedGravity, ftemporaryString);
                End;
              gsEnterMaxWind: Begin
                  SendMessage(opServerChangedMaxWind, ftemporaryString);
                End;
            End;
          End;
        End;
        If Key = VK_Up Then Begin
          Case fGorillaState Of
            gsEnterRoundCount: Begin
                ftemporaryInt := fPlayerCount;
                fGorillaState := gsEnterName;
                fLastTime := GetTickCount;
              End;
            gsEnterGravity: Begin
                If ftemporaryInt > fPlayerCount Then Begin
                  ftemporaryString := inttostr(froundcount);
                  fGorillaState := gsEnterRoundCount;
                End;
              End;
            gsEnterMaxWind: Begin
                FmaxWindStrength := abs(StrToIntDef(ftemporaryString, 100));
                ftemporaryString := inttostr(round(abs(fGravity)));
                fGorillaState := gsEnterGravity;
              End;
          End;
        End;
        If (key = VK_RETURN) Or (key = VK_DOWN) Then Begin
          Case fGorillaState Of
            gsEnterRoundCount: Begin
                fRoundCount := StrToIntDef(ftemporaryString, 1);
                fRoundCount := max(fRoundCount, 1);
                ftemporaryString := inttostr(round(abs(fGravity)));
                fGorillaState := gsEnterGravity;
              End;
            gsEnterGravity: Begin
                If ftemporaryString = '0' Then Begin
                  ftemporaryString := inttostr(random(95) + 1);
                End;
                fGravity := v2(0, abs(StrToIntDef(ftemporaryString, 10)));
                ftemporaryString := inttostr(FmaxWindStrength);
                fGorillaState := gsEnterMaxWind;
              End;
            gsEnterMaxWind: Begin
                FmaxWindStrength := abs(StrToIntDef(ftemporaryString, 100));
                ftemporaryString := '';
                StartGame();
              End;
          End;
        End;
        // Die Rundenzahl bleigt nur eine Zahl
        If (key In [VK_0..VK_9]) Or
          (key In [VK_NUMPAD0..VK_NUMPAD9]) Then Begin
          If (key In [VK_NUMPAD0..VK_NUMPAD9]) Then key := key - VK_NUMPAD0 + VK_0; // Umrechnen Numpad in zahlen
          ftemporaryString := ftemporaryString + chr(Key);
          // Hier ändert der Server seinen Namen
          If FNetwork.active Then Begin
            Case fGorillaState Of
              gsEnterRoundCount: Begin
                  SendMessage(opServerChangedRoundsToPlay, ftemporaryString);
                End;
              gsEnterGravity: Begin
                  SendMessage(opServerChangedGravity, ftemporaryString);
                End;
              gsEnterMaxWind: Begin
                  SendMessage(opServerChangedMaxWind, ftemporaryString);
                End;
            End;
          End;
        End;
      End;
    gsEnterName: Begin
        If Key = VK_BACK Then Begin
          delete(fPlayerNames[ftemporaryInt - 1], Length(fPlayerNames[ftemporaryInt - 1]), 1);
          // Hier ändert der Server seinen Namen
          If FNetwork.active Then Begin
            SendMessage(OpServerChangedName, fPlayerNames[ftemporaryInt - 1]);
          End;
        End;
        If (key = VK_RETURN) Or (key = VK_DOWN) Then Begin
          ftemporaryInt := ftemporaryInt + 1;
          If ftemporaryInt > fPlayerCount Then Begin
            ftemporaryString := inttostr(froundcount);
            fGorillaState := gsEnterRoundCount;
          End;
        End;
        If Key = VK_UP Then Begin
          ftemporaryInt := ftemporaryInt - 1;
          If ftemporaryInt = 0 Then Begin
            fGorillaState := gsStartScreen;
            If FNetwork.isServer Then Begin
              Network.Disconnect(true);
            End;
          End;
        End;
        // Anmen sollen nur aus Groß, Klein Buchstaben und 0..9 bestehen.
        If (key In [VK_A..VK_Z]) Or
          (key In [VK_0..VK_9]) Or
          (key In [VK_NUMPAD0..VK_NUMPAD9]) Then Begin
          // Die Kleinen Buchstaben
          If (key In [VK_NUMPAD0..VK_NUMPAD9]) Then key := key - VK_NUMPAD0 + VK_0; // Umrechnen Numpad in zahlen
          If (key In [VK_A..VK_Z]) And Not (ssShift In Shift) Then key := key + 32;
          fPlayerNames[ftemporaryInt - 1] := fPlayerNames[ftemporaryInt - 1] + chr(Key);
          // Hier ändert der Server seinen Namen
          If FNetwork.active Then Begin
            SendMessage(OpServerChangedName, fPlayerNames[ftemporaryInt - 1]);
          End;
        End;
      End;
  End;
End;

Procedure TGorilla.OnDisconnect(aSocket: TLSocket);
Begin
  If asocket = FNetwork.Socket Then Begin
    // Der Server muss sich merken mit welchem Socket er verbunden ist und nur beim trennen von diesem alles killen
     // Abschalten der Verbindung, egal was da nun war
    fGorillaState := gsStartScreen;
    If Network.Connected Then Begin
      Network.Disconnect(true);
    End;
    // ggf. den Chat ausschalten
    If Form3.Visible Then Begin
      Form3.Visible := false;
    End;
    FNetwork.active := false;
  End;
End;

Procedure TGorilla.OnAccept(aSocket: TLSocket);
Var
  b: byte;
Begin
  //  form3.show;
  If fGorillaState = gsWaitForClient Then Begin
    fGorillaState := gsEnterName;
    fPlayerCount := 1;
    ftemporaryInt := 1;
    FNetwork.active := true;
    FNetwork.Socket := aSocket;
  End
  Else Begin
    // Wir spielen schon oder what auch ever
    b := opDidNotAccept;
    asocket.Send(b, 1);
  End;
End;

Function TGorilla.GetMessage(aSocket: TLSocket): String;
Var
  b: Byte;
  s: String;
  i: integer;
  buffer: Array Of byte;
Begin
  aSocket.Get(b, 1);
  setlength(buffer, b);
  If b <> 0 Then Begin
    setlength(s, b);
    aSocket.Get(buffer[0], b);
    For i := 1 To b Do
      s[i] := chr(buffer[i - 1]);
  End
  Else
    s := '';
  result := s;
End;

Function TGorilla.fgetChatName: String;
Begin
  If FNetwork.isServer Then Begin
    result := fPlayerNames[0];
  End
  Else Begin
    result := fPlayerNames[1];
  End;
End;

Procedure TGorilla.SendPlayerStats;
Var
  s: String;
Begin
  // Sind wir überhaupt Berechtigt unsere Daten zu senden ?
  If FNetwork.active And FNetwork.isServer Then Begin
    s := inttostr(fPlayerPoints[0]) + '~' +
      inttostr(fPlayerPoints[1]) + '~' +
      inttostr(fActualRound);
    SendMessage(opServerSendStats, s);
  End;
End;

Procedure TGorilla.OnReceive(OpCode: byte; aSocket: TLSocket);
Var
  s, t: String;
  w: Word;
Begin
  Case OpCode Of
    opDidNotAccept: Begin
        OnDisconnect(FNetwork.Socket); // Der Server wollte uns nicht also Abschalten
        ShowMessage('Server did not accept any more connections.');
      End;
    opServerSendStats: Begin
        s := GetMessage(aSocket) + '~';
        // Player 1 Points
        t := copy(s, 1, pos('~', s) - 1);
        delete(s, 1, length(t) + 1);
        fPlayerPoints[0] := strtoint(t);
        // Player 2 Points
        t := copy(s, 1, pos('~', s) - 1);
        delete(s, 1, length(t) + 1);
        fPlayerPoints[1] := strtoint(t);
        // Die Aktuelle Runde
        t := copy(s, 1, pos('~', s) - 1);
        delete(s, 1, length(t) + 1);
        fActualRound := strtoint(t);
      End;
    opNoMessage: Begin
        // Ein Dummy, der nichts bewirkt, einfach nur das es definiert ist ..
      End;
    opClientChangedName: Begin
        fPlayerNames[1] := GetMessage(aSocket);
      End;
    opServerChangedName: Begin
        fPlayerNames[0] := GetMessage(aSocket);
      End;
    opChatMessage: Begin
        ReceiveChatMessage(GetMessage(aSocket));
      End;
    opServerChangedRoundsToPlay: Begin
        fRoundCount := StrToIntDef(GetMessage(aSocket), 0);
      End;
    opServerChangedGravity: Begin
        fGravity.y := StrToIntDef(GetMessage(aSocket), 0);
      End;
    opServerChangedMaxWind: Begin
        FmaxWindStrength := StrToIntDef(GetMessage(aSocket), 0);
      End;
    opStartGame: Begin
        ftemporaryInt := strtoint(GetMessage(aSocket));
        StartGame();
      End;
    opClientReadyToPlay: Begin
        GetMessage(aSocket); // Dummy den String Lesen
        SendPlayerStats();
        fGorillaState := gsEnterAngle;
      End;
    opUpdateSchmierstring: Begin
        ftemporaryString := GetMessage(aSocket);
      End;
    opReturnPressed: Begin
        GetMessage(aSocket); // Dummy den String Lesen
        OnAnglePowerReturn();
      End;
    opCancelRound: Begin
        GetMessage(aSocket); // Dummy den String Lesen
        fActualRound := fActualRound - 1; // Beim Rundenabbruch, wird eine Runde weniger gezählt
        ShowStats();
      End;
    opShowStats: Begin
        GetMessage(aSocket); // Dummy den String Lesen
        ShowStats();
      End;
    opEndShowStats: Begin
        w := 0; // Es wird ja eigentlich nichts gedrückt, also zeigen wir das auch an.
        OnKeyDown(w, []);
      End;
  End;
End;

Initialization

{$I gorilla.ressource}

End.

