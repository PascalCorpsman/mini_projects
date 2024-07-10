(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Bridge builder                                        *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit ubridge_builder;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Controls, uscreens, OpenGlcontext, umap;

Const
  (*
   * History: 0.01 - Initialversion
   *)
  Version = '0.01';
  ScreenWidth = 800;
  ScreenHeight = 600;
  GridSize = 5;
  MaxEdgeLen = 100;

Type

  TColliderEdit = Record
    PointCount: integer;
    P1: TPoint;
    P2: TPoint;
    P: TPoint;
  End;

  TGameState = (gsNone, gsMainMenu, gsEditor, gsInGame);

  { TBridgeBuilder }

  TBridgeBuilder = Class
  private
    GameState: TGameState;

    MainScreen: TMainScreen;
    EditorScreen: TEditorScreen;
    InGameScreen: TInGameScreen;

    OnMouseWheelDownCapture, OnMouseWheelUpCapture: TMouseWheelUpDownEvent;
    OnMouseDownCapture: TMouseEvent;
    OnMouseMoveCapture: TMouseMoveEvent;
    OnMouseUpCapture: TMouseEvent;
    LastMousePos: TPoint;
    FinalZoneIndex: integer;
    StartNode: integer; // Der NodeIndex auf den geklickt wurde (im MouseDown)
    ColliderEdit: TColliderEdit;
    DeadzoneEdit: TColliderEdit;

    // InGameScreen
    Procedure OnLeaveButtonClick(Sender: TObject);
    Procedure OnRunButtonClick(Sender: TObject);

    // MainScreen
    Procedure OnExitButtonClick(Sender: TObject);
    Procedure OnEditorButtonClick(Sender: TObject);
    Procedure OnLoadButtonClick(Sender: TObject);

    // EditorScreen
    Procedure OnLoadBackTexClick(Sender: TObject);

    // Allgemeine Events
    Procedure OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    Procedure OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    Procedure OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    Procedure OnMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; Var Handled: Boolean);
    Procedure OnMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; Var Handled: Boolean);

    Procedure OnSelectEditCollider(Sender: TObject);
    Procedure OnSelectEditDeadzone(Sender: TObject);
  public
    Map: TMap;
    OnSwitchToEditor: TNotifyEvent;
    OnLeaveEditor: TNotifyEvent;
    OnLoadButton: TNotifyEvent;

    Constructor Create(); virtual;
    Destructor Destroy(); override;

    Procedure InitOpenGL(Owner: TOpenGLControl);
    Procedure Render();

    Procedure SwitchToMainMenu;
    Procedure SwitchToGame;
    Procedure LoadFromFile(Const aFilename: String);
  End;

Implementation

Uses unit1, math, uvectormath, uopengl_widgetset, dglOpenGL;

Function TransformRoutine(x, y: integer): TPoint;
Var
  dim: Array[0..3] Of Integer;
  xx, yy: integer;
Begin
  glGetIntegerv(GL_VIEWPORT, @dim[0]);
  xx := round(ConvertDimension(0, dim[2], x, 0, ScreenWidth));
  yy := round(ConvertDimension(0, dim[3], y, 0, ScreenHeight));
  result := Point(xx, yy);
End;

{ TBridgeBuilder }

Constructor TBridgeBuilder.Create;
Begin
  Inherited Create;
  GameState := gsNone;
  OnSwitchToEditor := Nil;
  OnLeaveEditor := Nil;
  map := TMap.Create();
End;

Destructor TBridgeBuilder.Destroy;
Begin
  map.free;
  MainScreen.free;
  EditorScreen.free;
  InGameScreen.free;
End;

Procedure TBridgeBuilder.OnLeaveButtonClick(Sender: TObject);
Begin
  SwitchToMainMenu;
End;

Procedure TBridgeBuilder.OnRunButtonClick(Sender: TObject);
Begin
  // TODO: Implementieren
End;

Procedure TBridgeBuilder.OnExitButtonClick(Sender: TObject);
Begin
  GameState := gsNone;
  form1.close;
End;

Procedure TBridgeBuilder.OnEditorButtonClick(Sender: TObject);
Begin
  OnSwitchToEditor(self);
  map.Clear;
  map.EditMode := true;
  EditorScreen.setStartPoint.Checked := true;
  GameState := gsEditor;
  StartNode := -1;
  //LoadFromFile(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'Levels' + PathDelim + 'Level_01.lvl'); // TODO: Debug remove !
//  LoadFromFile(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'Levels' + PathDelim + 'Level_02.lvl'); // TODO: Debug remove !
  MainScreen.visible := false;
  EditorScreen.visible := true;
  InGameScreen.visible := false;
End;

Procedure TBridgeBuilder.OnLoadButtonClick(Sender: TObject);
Begin
  OnLoadButton(self);
End;

Procedure TBridgeBuilder.OnLoadBackTexClick(Sender: TObject);
Begin
  If Form1.OpenPictureDialog1.Execute Then Begin
    map.LoadBackTexture(Form1.OpenPictureDialog1.FileName);
  End;
End;

Procedure TBridgeBuilder.OnMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  EndNode, gx, gy: Integer;
  len: TBaseType;
Begin
  LastMousePos := TransformRoutine(x, y);
  If assigned(Map) Then Begin
    gx := round((LastMousePos.x + Map.Offset.X) / map.Zoom);
    gy := round((LastMousePos.y + Map.Offset.Y) / map.Zoom);
    gx := (gx - gx Mod GridSize) + GridSize Div 2;
    gy := (gy - gy Mod GridSize) + GridSize Div 2;
    Case GameState Of
      gsInGame: Begin
          Hier weiter mit dem Einfügen / Löschen der Kanten
        End;
      gsEditor: Begin
          If ssleft In Shift Then Begin
            If EditorScreen.EditFixedPoints.Checked Then Begin
              map.AddFixedBolt(gx, gy);
            End;
            If EditorScreen.EditFinalZone.Checked Then Begin
              FinalZoneIndex := map.findFinalZoneIndex(gx, gy);
            End;
            If EditorScreen.SetStartPoint.Checked Then Begin
              map.StartPoint := v2(gx, gy);
            End;
            If EditorScreen.EditInitialEdges.Checked Then Begin
              If StartNode = -1 Then Begin
                StartNode := map.FindFixedBolt(gx, gy);
                If StartNode = -1 Then Begin
                  StartNode := map.AddFixedBolt(gx, gy);
                End;
              End
              Else Begin
                len := LenV2(map.fFixedBolts[StartNode] - v2(gx, gy));
                If len <= MaxEdgeLen Then Begin
                  EndNode := map.FindFixedBolt(gx, gy);
                  If EndNode = -1 Then Begin
                    EndNode := map.AddFixedBolt(gx, gy);
                  End;
                  map.ToggleFixedEdge(StartNode, EndNode);
                  If (ssShift In Shift) Then Begin
                    StartNode := EndNode;
                  End
                  Else Begin
                    StartNode := -1;
                  End;
                End;
              End;
            End;
            If EditorScreen.EditCollider.Checked Then Begin
              If ssShift In Shift Then Begin
                // Der Punkt soll verschoben werden
                ColliderEdit.p := map.FindColliderPoint(gx, gy);
              End
              Else Begin
                Case ColliderEdit.PointCount Of
                  0: Begin // Der 1. Punkt
                      ColliderEdit.p1 := point(gx, gy);
                      ColliderEdit.PointCount := 1;
                    End;
                  1: Begin // Der 2. Punkt
                      ColliderEdit.p2 := point(gx, gy);
                      ColliderEdit.PointCount := 2;
                    End;
                  2: Begin // Der 3. Punkt
                      map.ToggleCollider(ColliderEdit.p1, ColliderEdit.p2, point(gx, gy));
                      ColliderEdit.PointCount := 0;
                    End;
                End;
              End;
            End;
            If EditorScreen.EditDeadZones.Checked Then Begin
              If ssShift In Shift Then Begin
                // Der Punkt soll verschoben werden
                DeadzoneEdit.p := map.FindDeadzonePoint(gx, gy);
              End
              Else Begin
                Case DeadzoneEdit.PointCount Of
                  0: Begin // Der 1. Punkt
                      DeadzoneEdit.p1 := point(gx, gy);
                      DeadzoneEdit.PointCount := 1;
                    End;
                  1: Begin // Der 2. Punkt
                      DeadzoneEdit.p2 := point(gx, gy);
                      DeadzoneEdit.PointCount := 2;
                    End;
                  2: Begin // Der 3. Punkt
                      map.ToggleDeadzone(DeadzoneEdit.p1, DeadzoneEdit.p2, point(gx, gy));
                      DeadzoneEdit.PointCount := 0;
                    End;
                End;
              End;
            End;
          End;
          If ssright In shift Then Begin
            If EditorScreen.EditFixedPoints.Checked Or EditorScreen.EditInitialEdges.Checked Then Begin
              map.DelFixedBolt(gx, gy);
            End;
            StartNode := -1;
            ColliderEdit.PointCount := 0;
            DeadzoneEdit.PointCount := 0;
          End;
        End;
      gsInGame: Begin

        End;
    End;
  End;
  If assigned(OnMouseDownCapture) Then Begin
    OnMouseDownCapture(sender, Button, Shift, x, y);
  End;
End;

Procedure TBridgeBuilder.OnMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
Var
  gx, gy, dx, dy: Integer;
  NewMousePos: TPoint;
Begin
  NewMousePos := TransformRoutine(x, y);
  {// Debug Anfang
  gx := round((NewMousePos.x + Map.Offset.X) / map.Zoom);
  gy := round((NewMousePos.y + Map.Offset.Y) / map.Zoom);
  gx := (gx - gx Mod GridSize);
  gy := (gy - gy Mod GridSize);
  form1.caption := format('%d %d, %d %d, %f, %d %d', [X, Y, gx, gy, map.Zoom, map.Offset.X, map.Offset.Y]);
  // -- Debug Ende // }
  // Scroll the Map :)
  If (ssright In shift) And assigned(Map) Then Begin
    dx := round((NewMousePos.x - LastMousePos.x) / map.Zoom);
    dy := round((NewMousePos.y - LastMousePos.y) / map.Zoom);
    Map.Offset.x := max(0, min(round(map.Dim.x * map.Zoom) - ScreenWidth, Map.Offset.x - dx));
    Map.Offset.Y := max(0, min(round(map.Dim.y * map.Zoom) - ScreenHeight, Map.Offset.Y - dy));
  End;
  If GameState = gsEditor Then Begin
    If EditorScreen.EditFinalZone.Checked And (ssLeft In Shift) And (FinalZoneIndex <> -1) Then Begin
      gx := round((NewMousePos.x + Map.Offset.X) / map.Zoom);
      gy := round((NewMousePos.y + Map.Offset.Y) / map.Zoom);
      gx := (gx - gx Mod GridSize) + GridSize Div 2;
      gy := (gy - gy Mod GridSize) + GridSize Div 2;
      map.FinalZone[FinalZoneIndex] := v2(gx, gy);
    End;
    If EditorScreen.EditCollider.Checked And (ssShift In Shift) Then Begin
      gx := round((NewMousePos.x + Map.Offset.X) / map.Zoom);
      gy := round((NewMousePos.y + Map.Offset.Y) / map.Zoom);
      gx := (gx - gx Mod GridSize) + GridSize Div 2;
      gy := (gy - gy Mod GridSize) + GridSize Div 2;
      map.SetColliderpoint(ColliderEdit.p, point(gx, gy));
    End;
    If EditorScreen.EditDeadZones.Checked And (ssShift In Shift) Then Begin
      gx := round((NewMousePos.x + Map.Offset.X) / map.Zoom);
      gy := round((NewMousePos.y + Map.Offset.Y) / map.Zoom);
      gx := (gx - gx Mod GridSize) + GridSize Div 2;
      gy := (gy - gy Mod GridSize) + GridSize Div 2;
      map.SetDeadzonepoint(DeadzoneEdit.p, point(gx, gy));
    End;
  End;
  // Forward Mouse Movements to the rest ..
  If assigned(OnMouseMoveCapture) Then Begin
    OnMouseMoveCapture(sender, shift, x, y);
  End;
  LastMousePos := NewMousePos;
End;

Procedure TBridgeBuilder.OnMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  If GameState = gsEditor Then Begin
    ColliderEdit.P := point(-1, -1);
    DeadzoneEdit.P := point(-1, -1);
  End;
End;

Procedure TBridgeBuilder.OnMouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; Var Handled: Boolean);
Var
  g1, g2: TVector2;
Begin
  If assigned(Map) And (ssCtrl In Shift) Then Begin
    // TODO: Das Verschieben während des Zoom ist noch nicht ideal, aber schon mal erträglich.
    g1 := (MousePos + Map.Offset) / Map.Zoom;
    map.zoom := max(power(1 / 1.1, 10), map.zoom / 1.1);
    g2 := (MousePos + Map.Offset) / Map.Zoom;
    map.Offset := map.Offset + (g1 - g2);
    Map.Offset.x := max(0, min(round(map.Dim.x * map.Zoom) - ScreenWidth, Map.Offset.x));
    Map.Offset.Y := max(0, min(round(map.Dim.y * map.Zoom) - ScreenHeight, Map.Offset.Y));
  End;
  If assigned(OnMouseWheelDownCapture) Then OnMouseWheelDownCapture(sender, shift, MousePos, handled);
End;

Procedure TBridgeBuilder.OnMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; Var Handled: Boolean);
Var
  g1, g2: TVector2;
Begin
  If assigned(Map) And (ssCtrl In Shift) Then Begin
    // TODO: Das Verschieben während des Zoom ist noch nicht ideal, aber schon mal erträglich.
    g1 := (MousePos + Map.Offset) / Map.Zoom;
    map.zoom := min(power(1.1, 10), map.zoom * 1.1);
    g2 := (MousePos + Map.Offset) / Map.Zoom;
    map.Offset := map.Offset + (g1 - g2);
    Map.Offset.x := max(0, min(round(map.Dim.x * map.Zoom) - ScreenWidth, Map.Offset.x));
    Map.Offset.Y := max(0, min(round(map.Dim.y * map.Zoom) - ScreenHeight, Map.Offset.Y));
  End;
  If assigned(OnMouseWheelUpCapture) Then OnMouseWheelUpCapture(sender, shift, MousePos, handled);
End;

Procedure TBridgeBuilder.OnSelectEditCollider(Sender: TObject);
Begin
  If assigned(map) Then map.ShowCollider := TOpenGL_Radiobutton(sender).Checked;
End;

Procedure TBridgeBuilder.OnSelectEditDeadzone(Sender: TObject);
Begin
  If assigned(map) Then map.ShowDeadZones := TOpenGL_Radiobutton(sender).Checked;
End;

Procedure TBridgeBuilder.InitOpenGL(Owner: TOpenGLControl);
Begin
  OnMouseDownCapture := owner.OnMouseDown;
  owner.OnMouseDown := @OnMouseDown;
  OnMouseMoveCapture := owner.OnMouseMove;
  owner.OnMouseMove := @OnMouseMove;
  OnMouseUpCapture := owner.OnMouseUp;
  owner.OnMouseup := @OnMouseUp;

  OnMouseWheelDownCapture := owner.OnMouseWheelDown;
  owner.OnMouseWheelDown := @OnMouseWheelDown;

  OnMouseWheelUpCapture := owner.OnMouseWheelUp;
  owner.OnMouseWheelUp := @OnMouseWheelUp;

  MainScreen := TMainScreen.Create(Owner);
  MainScreen.ExitButton.OnClick := @OnExitButtonClick;
  MainScreen.EditorButton.OnClick := @OnEditorButtonClick;
  MainScreen.LoadButton.OnClick := @OnLoadButtonClick;
  EditorScreen := TEditorScreen.Create(Owner);
  EditorScreen.LoadBackGround.OnClick := @OnLoadBackTexClick;
  EditorScreen.EditCollider.OnChange := @OnSelectEditCollider;
  EditorScreen.EditDeadZones.OnChange := @OnSelectEditDeadzone;

  InGameScreen := TInGameScreen.Create(Owner);
  InGameScreen.LeaveButton.OnClick := @OnLeaveButtonClick;

  map.InitOpenGL;
  SwitchToMainMenu();
End;

Procedure TBridgeBuilder.Render;
Var
  len: Single;
  gx, gy: integer;
Begin
  Case GameState Of
    gsInGame: Begin
        map.Render();
        InGameScreen.Render;
      End;
    gsMainMenu: Begin
        MainScreen.Render;
      End;
    gsEditor: Begin
        map.Render();
        If EditorScreen.EditInitialEdges.Checked And (StartNode <> -1) Then Begin
          gx := round((LastMousePos.x + map.Offset.x) / map.Zoom);
          gy := round((LastMousePos.y + map.Offset.y) / map.Zoom);
          gx := (gx - gx Mod GridSize) + GridSize Div 2;
          gy := (gy - gy Mod GridSize) + GridSize Div 2;
          len := sqrt(sqr(map.fFixedBolts[StartNode].x - gx) + sqr(map.fFixedBolts[StartNode].y - gy));
          If len <= MaxEdgeLen Then Begin
            glBindTexture(GL_TEXTURE_2D, 0);
            glLineWidth(3);
            glColor3f(0.25, 0.25, 0.25);
            glPushMatrix;
            glTranslatef(-map.Offset.x, -map.Offset.y, 0);
            glBegin(GL_LINES);
            glVertex2f(map.fFixedBolts[StartNode].x * map.Zoom, map.fFixedBolts[StartNode].y * map.Zoom);
            glVertex2f(LastMousePos.x + map.Offset.x, LastMousePos.y + map.Offset.y);
            glEnd;
            glPopMatrix;
            glLineWidth(1);
          End;
        End;
        If EditorScreen.EditCollider.Checked Then Begin
          gx := round((LastMousePos.x + map.Offset.x) / map.Zoom);
          gy := round((LastMousePos.y + map.Offset.y) / map.Zoom);
          gx := (gx - gx Mod GridSize) + GridSize Div 2;
          gy := (gy - gy Mod GridSize) + GridSize Div 2;
          glBindTexture(GL_TEXTURE_2D, 0);
          glLineWidth(3);
          glColor3f(0.5, 0.5, 0.5);
          glPushMatrix;
          glTranslatef(-map.Offset.x, -map.Offset.y, 0);
          glBegin(GL_LINE_STRIP);
          If ColliderEdit.PointCount >= 1 Then glVertex2f(ColliderEdit.P1.x * map.Zoom, ColliderEdit.P1.y * map.Zoom);
          If ColliderEdit.PointCount >= 2 Then glVertex2f(ColliderEdit.P2.x * map.Zoom, ColliderEdit.P2.y * map.Zoom);
          glVertex2f(LastMousePos.x + map.Offset.x, LastMousePos.y + map.Offset.y);
          glEnd;
          glPopMatrix;
          glLineWidth(1);
        End;
        If EditorScreen.EditDeadZones.Checked Then Begin
          gx := round((LastMousePos.x + map.Offset.x) / map.Zoom);
          gy := round((LastMousePos.y + map.Offset.y) / map.Zoom);
          gx := (gx - gx Mod GridSize) + GridSize Div 2;
          gy := (gy - gy Mod GridSize) + GridSize Div 2;
          glBindTexture(GL_TEXTURE_2D, 0);
          glLineWidth(3);
          glColor3f(1, 0.5, 0.5);
          glPushMatrix;
          glTranslatef(-map.Offset.x, -map.Offset.y, 0);
          glBegin(GL_LINE_STRIP);
          If DeadzoneEdit.PointCount >= 1 Then glVertex2f(DeadzoneEdit.P1.x * map.Zoom, DeadzoneEdit.P1.y * map.Zoom);
          If DeadzoneEdit.PointCount >= 2 Then glVertex2f(DeadzoneEdit.P2.x * map.Zoom, DeadzoneEdit.P2.y * map.Zoom);
          glVertex2f(LastMousePos.x + map.Offset.x, LastMousePos.y + map.Offset.y);
          glEnd;
          glPopMatrix;
          glLineWidth(1);
        End;
        EditorScreen.Render;
      End;
  End;
End;

Procedure TBridgeBuilder.SwitchToMainMenu;
Begin
  OnLeaveEditor(self);
  If assigned(map) Then map.EditMode := false;
  GameState := gsMainMenu;
  MainScreen.visible := true;
  EditorScreen.visible := false;
  InGameScreen.visible := false;
End;

Procedure TBridgeBuilder.SwitchToGame;
Begin
  If assigned(map) Then map.EditMode := false;
  GameState := gsInGame;
  MainScreen.visible := false;
  EditorScreen.visible := false;
  InGameScreen.visible := true;
End;

Procedure TBridgeBuilder.LoadFromFile(Const aFilename: String);
Var
  dx, dy: single;
Begin
  map.LoadFromFile(aFilename);
  // Immer auf "Fullscreen" scrollen
  dx := ScreenWidth / map.Dim.X;
  dy := ScreenHeight / map.Dim.Y;
  map.Zoom := min(dx, dy);
End;

End.

