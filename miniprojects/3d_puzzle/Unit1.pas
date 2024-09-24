(******************************************************************************)
(* 3D-Puzzle                                                       20.11.2006 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : This Sample try to show how to Implement a 3D Affenpuzzle    *)
(*               Solver.                                                      *)
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

{$MODE ObjFPC}{$H+}

Interface

Uses
  dglopengl, uvectormath,
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, OpenGLContext;

{$DEFINE DebuggMode}

Type

  { TForm1 }

  TForm1 = Class(TForm)
    OpenGLControl1: TOpenGLControl;
    RadioGroup1: TRadioGroup;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    Button7: TButton;
    Label1: TLabel;
    CheckBox1: TCheckBox;
    Timer1: TTimer;
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure FormResize(Sender: TObject);
    Procedure OpenGLControl1MakeCurrent(Sender: TObject; Var Allow: boolean);
    Procedure OpenGLControl1Paint(Sender: TObject);
    Procedure RadioGroup1Click(Sender: TObject);
    Procedure Panel1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
    // The Render Function
    Procedure Render;
    // Functions for 2D Mode
    Procedure Go2d;
    Procedure Exit2d;
  End;

  // 0 = Unbelegt, 1 .. 6 = Farben
  TCubeData = Array[0..4, 0..4, 0..4] Of Shortint;
  // True das teil belegt diese Koordinate, False das Teil belegt die Koordinate nicht
  TPiece = Array[0..4, 0..4] Of Boolean;
  // Hiermit Merken wir uns welceh Teile Bereits verbaut sind.
  TPieceBoolArray = Array[1..6] Of Boolean;

  // The Simulated 2D solution
Const
  ScreenWidth = 512;
  ScreenHeight = 512;
  ROT_SENSITIVITY = 0.5;
  // Die Farben der Seitenflächen
  Colors: Array[1..6] Of Tvector3 = (
    (x: 1; y: 0; z: 0),
    (x: 0; y: 1; z: 0),
    (x: 0; y: 0; z: 1),
    (x: 0; y: 1; z: 1),
    (x: 1; y: 0; z: 1),
    (x: 1; y: 1; z: 0)
    );
  // Offset für 2D Modus
  xoff = (512 - 400) Div 2;
  yoff = Xoff;
  // PieceBreite im 2D Modus
  PieceSize = 100;
  halfSize = PieceSize Div 2;

Var
  Form1: TForm1;
  // OpenGL Handles
  Initialized: Boolean = False;
  // Auswahl im menu deitlich schneller als jedesmal Radiogroupbox1.itemindex ab zu fragen.
  Switcher: Integer = 0;
  // Rotationsmatrix des Würfels
  RotMatrix: TMatrix4x4;
  // Alte Maus Daten
  aMouse: Tpoint;
  // Der Angezeigt Würfel
  Cube: TCubeData;
  //                           ist von 1 bis 6 wegen Switcher.
  VirtualPieces, Pieces: Array[1..6] Of TPiece;

Implementation

{$R *.lfm}

(*
Dreht ein Teil um 90° und gibt das gedrehte zurück.
*)

Function RotatePiece(Piece: TPiece): TPiece; // Fertig -- Getestet
Var
  i, j: Integer;
Begin
  For i := 0 To 4 Do
    For j := 0 To 4 Do
      result[4 - j, i] := Piece[i, j];
End;

(*
Spiegelt das Teil an der Y - Achse
*)

Function MirrowPiece(Piece: TPiece): TPiece; // Fertig -- Getestet
Var
  i, j: Integer;
Begin
  For i := 0 To 4 Do
    For j := 0 To 4 Do
      result[4 - i, j] := Piece[i, j];
End;

(*
1. Die Parameter sind nur Var weil wir Verhindern wollen das der Aufrufstack immer neuen Speicher Allokiert
   die gesammte Function hat aber keinen Schreibenden Zugriff !!!
2. Die Function Prüft ob ein Piece an Position Side in den Cube eingesetzt werden Könnte
   wenn Ja wird True zurückgegeben.

Side = 0 = Unten
Side = 1 = Hinten
Side = 2 = Rechts
Side = 3 = Vorne
Side = 4 = Links
Side = 5 = Oben

*)

Function PieceIsPlaceable(Var Piece: TPiece; Var Side: Smallint; Var Cube: TCubeData): Boolean; // Fertig -- Getestet
Var
  i: Integer;
Begin
  Result := True;
  Case Side Of
    0: Begin // Unten
        For i := 0 To 4 Do Begin
          // Hinten Unten
          If Piece[i, 0] And (Cube[i, 0, 0] <> 0) Then Result := False;
          // Vorne Unten
          If Piece[i, 4] And (Cube[i, 0, 4] <> 0) Then Result := False;
          // Links Unten
          If Piece[0, i] And (Cube[0, 0, i] <> 0) Then Result := False;
          // Rechts Unten
          If Piece[4, i] And (Cube[4, 0, i] <> 0) Then Result := False;
        End;
      End;
    1: Begin // Hinten
        For i := 0 To 4 Do Begin
          // Hinten Unten
          If Piece[i, 0] And (Cube[i, 0, 0] <> 0) Then Result := False;
          // Hinten Links
          If Piece[0, i] And (Cube[0, i, 0] <> 0) Then Result := False;
          // Hinten Rechts
          If Piece[4, i] And (Cube[4, i, 0] <> 0) Then Result := False;
          // Hinten Oben
          If Piece[i, 4] And (Cube[i, 4, 0] <> 0) Then Result := False;
        End;
      End;
    2: Begin // Rechts
        For i := 0 To 4 Do Begin
          // Rechts Unten
          If Piece[i, 0] And (Cube[4, 0, i] <> 0) Then Result := False;
          // Rechts Links
          If Piece[0, i] And (Cube[4, i, 0] <> 0) Then Result := False;
          // Rechts Rechts
          If Piece[4, i] And (Cube[4, i, 4] <> 0) Then Result := False;
          // Rechts Oben
          If Piece[i, 4] And (Cube[4, 4, i] <> 0) Then Result := False;
        End;
      End;
    3: Begin // Vorne
        For i := 0 To 4 Do Begin
          // Vorne Unten
          If Piece[i, 0] And (Cube[i, 0, 4] <> 0) Then Result := False;
          // Vorne Links
          If Piece[0, i] And (Cube[0, i, 4] <> 0) Then Result := False;
          // Vorne Rechts
          If Piece[4, i] And (Cube[4, i, 4] <> 0) Then Result := False;
          // Vorne Oben
          If Piece[i, 4] And (Cube[i, 4, 4] <> 0) Then Result := False;
        End;
      End;
    4: Begin // Links
        For i := 0 To 4 Do Begin
          // Links Unten
          If Piece[i, 0] And (Cube[0, 0, i] <> 0) Then Result := False;
          // Links Links
          If Piece[0, i] And (Cube[0, i, 0] <> 0) Then Result := False;
          // Links Links
          If Piece[4, i] And (Cube[0, i, 4] <> 0) Then Result := False;
          // Links Oben
          If Piece[i, 4] And (Cube[0, 4, i] <> 0) Then Result := False;
        End;
      End;
    5: Begin // Oben
        For i := 0 To 4 Do Begin
          // Hinten Oben
          If Piece[i, 0] And (Cube[i, 4, 0] <> 0) Then Result := False;
          // Vorne Oben
          If Piece[i, 4] And (Cube[i, 4, 4] <> 0) Then Result := False;
          // Links Oben
          If Piece[0, i] And (Cube[0, 4, i] <> 0) Then Result := False;
          // Rechts Oben
          If Piece[4, i] And (Cube[4, 4, i] <> 0) Then Result := False;
        End;
      End
  Else
    Result := False;
  End;
End;

(*
Diese Function setzt das Piece an die Side des Cube's ein und gibt den neuen zurück.
*)

Function PlacePiece(Piece: TPiece; Index, Side: smallint; Cube: TCubeData): TCubeData; // Fertig -- Getestet
Var
  i, j: Integer;
Begin
  result := Cube;
  Case Side Of
    0: Begin // Unten
        For i := 0 To 4 Do
          For j := 0 To 4 Do
            If Piece[i, j] Then Result[i, 0, j] := Index;
      End;
    1: Begin // Hinten
        For i := 0 To 4 Do
          For j := 0 To 4 Do
            If Piece[i, j] Then Result[i, j, 0] := Index;
      End;
    2: Begin // Rechts
        For i := 0 To 4 Do
          For j := 0 To 4 Do
            If Piece[i, j] Then Result[4, j, i] := Index;
      End;
    3: Begin // Vorne
        For i := 0 To 4 Do
          For j := 0 To 4 Do
            If Piece[i, j] Then Result[i, j, 4] := Index;
      End;
    4: Begin // Links
        For i := 0 To 4 Do
          For j := 0 To 4 Do
            If Piece[i, j] Then Result[0, j, i] := Index;
      End;
    5: Begin // Oben
        For i := 0 To 4 Do
          For j := 0 To 4 Do
            If Piece[i, j] Then Result[i, 4, j] := Index;
      End;
  End;
End;

(*
Gibt die Standart Teile aus
*)

Function ClearPiece(index: Integer): TPiece; // Fertig -- Getestet
Begin
  FillChar(Result, SizeOf(Result), 1);
  Case Index Of
    1: Begin
        result[0, 0] := false;
        result[2, 0] := false;
        result[4, 1] := false;
        result[0, 2] := false;
        result[4, 3] := false;
        result[0, 4] := false;
        result[2, 4] := false;
        result[4, 4] := false;
      End;
    2: Begin
        result[0, 0] := false;
        result[1, 0] := false;
        result[3, 0] := false;
        result[4, 0] := false;
        result[0, 1] := false;
        result[4, 1] := false;
        result[0, 3] := false;
        result[4, 3] := false;
        result[2, 4] := false;
        result[4, 4] := false;
      End;
    3: Begin
        result[2, 0] := false;
        result[4, 0] := false;
        result[0, 1] := false;
        result[4, 1] := false;
        result[0, 3] := false;
        result[4, 3] := false;
        result[0, 4] := false;
        result[2, 4] := false;
      End;
    4: Begin
        result[0, 0] := false;
        result[2, 0] := false;
        result[4, 0] := false;
        result[0, 1] := false;
        result[4, 1] := false;
        result[0, 3] := false;
        result[4, 3] := false;
        result[0, 4] := false;
        result[2, 4] := false;
        result[4, 4] := false;
      End;
    5: Begin
        result[2, 0] := false;
        result[0, 1] := false;
        result[4, 1] := false;
        result[0, 3] := false;
        result[4, 3] := false;
        result[0, 4] := false;
        result[2, 4] := false;
        result[4, 4] := false;
      End;
    6: Begin
        result[2, 0] := false;
        result[4, 0] := false;
        result[4, 1] := false;
        result[0, 2] := false;
        result[4, 3] := false;
        result[1, 4] := false;
        result[3, 4] := false;
        result[4, 4] := false;
      End;
  End;
End;

(*
Löscht den gesamten Würfel
*)

Function ClearCube: TCubeData; // Fertig -- Getestet
Begin
  FillChar(Result, SizeOf(Result), 0);
End;

(*
Diese Function berechnet einen Würfel aus den Gegebenen Teilen
Wenn die berechnung erfolgreich war wird True zurückgegeben und die Variable Cube enthält dann
den gelösten Würfel.

Bei einem Unlösbaren Cube ist Cube undefiniert.
*)

Function CalculateCube(Depth: Smallint; Var Cube: TCubeData; UsedPieces: TPieceBoolArray): Boolean; // Fertig -- Getestet
Var
  i, j: Integer;
  aPiece: TPiece;
  WorkCube: TCubeData;
Begin
  Result := False;
  // Die Tiefe gibt Gleichzeitig auch die Seiten an.
  Case Depth Of
    // Die Seite 0 Geht ja Immer, dadurch wissen wir das wir nur einen Aufruf haben und machen hier dann die Gesammte Initialisierung
    0: Begin // Unten
        // Reset Cube
        cube := ClearCube;
        // Speichern welches Teil bereits benutzt wurde und initialisieren der Anderen
        UsedPieces[1] := True;
        For i := 2 To 6 Do
          UsedPieces[i] := False;
        // Plazieren unseres 1. Teiles
        cube := PlacePiece(Pieces[depth + 1], depth + 1, depth, cube);
        // Rekursives Berechnen der Restteile
        result := CalculateCube(Depth + 1, cube, UsedPieces);
      End;
    1..5: Begin // Berechnen der Teile 2 .. 6
        // Da wir Teil 1 Ja immer Verbaun brauchen wir das hier nicht mehr prüfen und sparen CPU Zeit ;)
        For i := 2 To 6 Do
          // Nur noch nicht Verbaute Teile dürfen benutzt werden.
          If Not UsedPieces[i] Then Begin
            // Zwischenspeichern des Teiles
            Apiece := Pieces[i];
            // Testen der 1 .. 4 Fälle
            For j := 0 To 3 Do Begin
              // Wenn das Teil eingesetzt werden kann dann gehts weiter
              If PieceIsPlaceable(APiece, Depth, Cube) Then Begin
                // Teil Einsetzen
                WorkCube := PlacePiece(Apiece, i, depth, Cube);
                // Merken das das Teil Verbaut ist
                UsedPieces[i] := True;
                // Nächstes Teil Suchen
                result := CalculateCube(depth + 1, WorkCube, UsedPieces);
                // Wenn der Cube Gelöst werden Konnte
                If Result Then Begin
                  // Rückgabe des Gefundenen Cubes
                  Cube := WorkCube;
                  // Raus
                  Exit;
                End;
                // Rücksetzen der Teilnutzung
                UsedPieces[i] := false;
              End;
              // Das Teil um 90° drehen und damit den Nächsten Fall erzeugen
              Apiece := RotatePiece(Apiece);
            End;
            // Spiegeln des Teiles
            aPiece := MirrowPiece(apiece);
            // Testen der 5 .. 8 Fälle
            For j := 0 To 3 Do Begin
              // Wenn das Teil eingesetzt werden kann dann gehts weiter
              If PieceIsPlaceable(APiece, Depth, Cube) Then Begin
                // Teil Einsetzen
                WorkCube := PlacePiece(Apiece, i, depth, Cube);
                // Merken das das Teil Verbaut ist
                UsedPieces[i] := True;
                // Nächstes Teil Suchen
                result := CalculateCube(depth + 1, WorkCube, UsedPieces);
                // Wenn der Cube Gelöst werden Konnte
                If Result Then Begin
                  // Rückgabe des Gefundenen Cubes
                  Cube := WorkCube;
                  // Raus
                  Exit;
                End;
                // Rücksetzen der Teilnutzung
                UsedPieces[i] := false;
              End;
              // Das Teil um 90° drehen und damit den Nächsten Fall erzeugen
              Apiece := RotatePiece(Apiece);
            End;
          End;
      End;
    6: Begin
        // Eigentlich ist es unnötig diese Rekursionstiefe auf zu Rufen
        // Aber der RestCode wird dadurch "Eleganter" = Schneller und den
        // Overhead dieser einen Rekursion Erlauben wir uns einfach ;).
        Result := True; // RekursionsEnde , da Würfel Gelöst
      End;
  End;
End;

(*
Mit Licht sieht alles gleich viel Schöner aus.
*)

Procedure SetUpLighting;
Const
  ambient: Array[0..3] Of glfloat = (1.0, 1.0, 1.0, 1.0);
  //  position: Array[0..3] Of glfloat = (5.0, 4.0, 5.0, 1.0);
Begin
  glLightfv(GL_LIGHT0, GL_AMBIENT, @ambient);
  //  glLightfv(GL_LIGHT0, GL_SPECULAR, @ambient);
  //  glLightfv(GL_LIGHT0, GL_POSITION, @position);
  glEnable(GL_LIGHT0);
  glenable(Gl_lighting);
End;

(*
Rendern eines Quads im 2D - Modus
*)

Procedure RenderQuad(x, y: Integer);
Begin
  glpushmatrix;
  glTranslatef(x, y, 0);
  glbegin(gl_quads);
  glvertex3f(-halfSize, halfSize, 0);
  glvertex3f(halfSize, halfSize, 0);
  glvertex3f(halfSize, -halfSize, 0);
  glvertex3f(-halfSize, -halfSize, 0);
  glend;
  glpopmatrix;
End;

(*
Rendern eines Teiles im 2D - Modus
*)

Procedure Render2DPiece(piece: Tpiece; Color: Tvector3);
Var
  i, j: Integer;
Begin
  glcolor3fv(@Color);
  glpushmatrix;
  gltranslatef(xoff, yoff, 0);
  For i := 0 To 4 Do
    For j := 0 To 4 Do
      If Piece[i, j] Then
        RenderQuad(i * PieceSize, j * PieceSize);
  glpopmatrix;
End;

(*
Rendern eines kleinen Cubes, aus diesen wird der Gesamtcube zusammengesetzt.
*)

Procedure Render3DCube(position, Color: Tvector3);
Var
  b: Boolean;
Begin
  glpushmatrix;
  //glcolor3fv(@Color);
  // Bei Alten Graphikkarten braucht es hier einen Alphawert, keine Ahnung warum
  glcolor4f(color.x, color.y, color.z, 1);
  b := glIsEnabled(gl_lighting);
  If b Then Begin
    glColorMaterial(GL_FRONT, GL_DIFFUSE);
    glenable(GL_COLOR_MATERIAL);
  End;
  gltranslatef(position.x, position.y, position.z);
  glbegin(Gl_quads);
  // Top
  glNormal3f(0, 1, 0);
  glVertex3f(-0.5, 0.5, 0.5);
  glVertex3f(0.5, 0.5, 0.5);
  glVertex3f(0.5, 0.5, -0.5);
  glVertex3f(-0.5, 0.5, -0.5);
  // Bottom
  glNormal3f(0, -1, 0);
  glVertex3f(-0.5, -0.5, -0.5);
  glVertex3f(0.5, -0.5, -0.5);
  glVertex3f(0.5, -0.5, 0.5);
  glVertex3f(-0.5, -0.5, 0.5);
  // Front
  glNormal3f(0, 0, 1);
  glVertex3f(-0.5, -0.5, 0.5);
  glVertex3f(0.5, -0.5, 0.5);
  glVertex3f(0.5, 0.5, 0.5);
  glVertex3f(-0.5, 0.5, 0.5);
  // Back
  glNormal3f(0, 0, -1);
  glVertex3f(-0.5, 0.5, -0.5);
  glVertex3f(0.5, 0.5, -0.5);
  glVertex3f(0.5, -0.5, -0.5);
  glVertex3f(-0.5, -0.5, -0.5);
  // Right
  glNormal3f(1, 0, 0);
  glVertex3f(0.5, -0.5, 0.5);
  glVertex3f(0.5, -0.5, -0.5);
  glVertex3f(0.5, 0.5, -0.5);
  glVertex3f(0.5, 0.5, 0.5);
  // Left
  glNormal3f(-1, 0, 0);
  glVertex3f(-0.5, 0.5, 0.5);
  glVertex3f(-0.5, 0.5, -0.5);
  glVertex3f(-0.5, -0.5, -0.5);
  glVertex3f(-0.5, -0.5, 0.5);
  glend;
  If b Then
    gldisable(GL_COLOR_MATERIAL);
  glcolor4f(1, 1, 1, 1);

  glpopmatrix;
End;

Procedure TForm1.Go2d;
Begin
  glMatrixMode(GL_PROJECTION);
  glPushMatrix(); // Store The Projection Matrix
  glLoadIdentity(); // Reset The Projection Matrix
  glOrtho(0, Screenwidth, Screenheight, 0, -1, 1); // Set Up An Ortho Screen
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix(); // Store old Modelview Matrix
  glLoadIdentity(); // Reset The Modelview Matrix
End;

Procedure TForm1.Exit2d;
Begin
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix(); // Restore old Projection Matrix
  glMatrixMode(GL_PROJECTION);
  glPopMatrix(); // Restore old Projection Matrix
End;

(*
Render Szene
*)

Procedure Tform1.Render;
Var
  i, j, k: integer;
Begin
  If Not Initialized Then exit;
  // Clearscreen
  glClear(GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT);
  glViewport(0, 0, OpenGLControl1.width, OpenGLControl1.height);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
  Case Switcher Of
    0: Begin // 3D - Modus
        gluLookAt(5, 4, 5, 0, 0, 0, 0, 1, 0);
        // Würfel Drehen
        If CheckBox1.Checked Then Begin
          glenable(Gl_Lighting);
        End
        Else Begin
          gldisable(gl_Lighting);
        End;
        glMultMatrixf(@RotMatrix);
        glColor4f(1, 1, 1, 1);
        glBindTexture(GL_TEXTURE_2D, 0);
        // Rendern des Würfels
        For i := 0 To 4 Do
          For j := 0 To 4 Do
            For k := 0 To 4 Do
              If cube[i, j, k] <> 0 Then
                Render3DCube(v3(i - 2, j - 2, k - 2), colors[cube[i, j, k]]);
      End;
    1..6: Begin // 2D - Modus
        gldisable(gl_Lighting);
        // Switch to 2D mode
        Go2d;
        Render2DPiece(pieces[switcher], colors[switcher]);
        // Switch to 3D mode
        Exit2d;
        glenable(Gl_Lighting);
      End;
  End;
  // Redraw screen
  OpenGLControl1.SwapBuffers;
End;

Var
  allowcnt: integer = 0;

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

  // Berechnen des Gelösten Würfels, damit man am Anfang auch schon was sehen kann ;)
  Button3.onclick(Nil);
  // Setup OpenGL
  RotMatrix := IdentityMatrix4x4;
  glenable(gl_cull_face);
  glEnable(GL_DEPTH_TEST);
  glDepthFunc(GL_LESS);
  // Set Clear Color = black
  glClearColor(0, 0, 0, 0);
  // Resize Window
  OnResize(Nil);
  // Set OpenGL initialized
  Initialized := true;
  SetUpLighting;
  FormResize(Nil);
  Invalidate;
End;

Procedure TForm1.OpenGLControl1Paint(Sender: TObject);
Begin
  render;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Var
  i: integer;
  s: String;
Begin
  // Init dglOpenGL.pas , Teil 1
  If Not InitOpenGl Then Begin
    showmessage('Error, could not init dglOpenGL.pas');
    Halt;
  End;
  // Initialisierung
  Label1.caption := 'Click on Image' + LineEnding + 'to change piece.';
  s := IncludeTrailingBackslash(ExtractFilePath(ParamStr(0)));
  SaveDialog1.initialdir := s;
  OpenDialog1.initialdir := s;
  Caption := '3D-Puzzle solver by Corpsman | Support : www.Corpsman.de';
  // Initialisieren der Teile
  For i := 1 To 6 Do Begin
    pieces[i] := ClearPiece(i);
  End;
  // Der Timer ist leider notendig, da ein Render in MouseMove ein Ruckeln bewirkt, so aber nicht.
  timer1.Interval := 40;
  Timer1.enabled := True;
End;

Procedure TForm1.FormDestroy(Sender: TObject);
Begin
  // Free OpenGL
  Initialized := false;
End;

Procedure TForm1.FormResize(Sender: TObject);
Begin
  If Not Initialized Then exit;
  // resize OpenGL
  glViewport(0, 0, OpenGLControl1.Width, OpenGLControl1.height);
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity;
  gluPerspective(90, OpenGLControl1.clientwidth / OpenGLControl1.Clientheight, 0.1, 128);
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity;
End;

Procedure TForm1.RadioGroup1Click(Sender: TObject);
Begin
  switcher := RadioGroup1.itemindex;
  Label1.visible := RadioGroup1.itemindex In [1..6];
  Button1.visible := Not Label1.visible;
End;

Procedure TForm1.Panel1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  Case Switcher Of
    0: aMouse := Point(x, y);
    1..6:
      If ssleft In shift Then Begin
        x := (x - xoff + halfSize) Div PieceSize;
        y := (y - yoff + halfSize) Div PieceSize;
        If (x In [0..4]) And (y In [0..4]) Then
          Pieces[switcher][x, y] := Not Pieces[switcher][x, y];
        Cube := ClearCube;
      End;
  End;
End;

Procedure TForm1.Panel1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
Var
  rotx, roty: Single;
Begin
  If ((ssleft In shift) Or (ssRight In shift)) And (Switcher = 0) Then Begin
    rotY := (x - amouse.X) * ROT_SENSITIVITY;
    rotX := (y - amouse.Y) * ROT_SENSITIVITY;
    aMouse := Point(x, y);
    // Berechnen der neuen Rotationsmatrix, für den Cube
    glPushMatrix();
    glLoadIdentity();
    glRotatef(rotY, 0.0, 1.0, 0.0);
    If (ssleft In Shift) Then
      glRotatef(rotX, 1.0, 0.0, 0.0)
    Else
      glRotatef(-rotx, 0.0, 0.0, 1.0);
    glMultMatrixf(@RotMatrix);
    glGetFloatv(GL_MODELVIEW_MATRIX, @RotMatrix);
    glPopMatrix();
  End;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  RotMatrix := IdentityMatrix4x4;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Var
  f: Tfilestream;
Begin
  If Savedialog1.execute Then Begin
    f := TFileStream.create(Savedialog1.FileName, fmCreate Or fmOpenwrite);
    f.write(pieces, sizeof(pieces));
    f.free;
  End;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Var
  f: Tfilestream;
Begin
  If Opendialog1.Execute Then Begin
    f := TFileStream.create(Opendialog1.FileName, fmOpenread);
    f.read(pieces, sizeof(pieces));
    f.free;
    Cube := ClearCube;
  End;
End;

Procedure TForm1.Button7Click(Sender: TObject);
Var
  i: Integer;
Begin
  For i := 1 To 6 Do
    pieces[i] := ClearPiece(i);
  Button3.onclick(Nil);
End;

Procedure TForm1.Button3Click(Sender: TObject);
Var
  Dummy: TPieceBoolArray;
Begin
  If Not CalculateCube(0, cube, dummy) Then Begin
    cube := ClearCube;
    Showmessage('Sorry , this Pieces cannot make a Cube.');
  End
  Else Begin
    RadioGroup1.ItemIndex := 0;
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
    OpenGLControl1Paint(Nil);
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

