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
  GridSize = 5;
  MaxEdgeLen = 100;
Var
  ScreenWidth: integer = 800;
  ScreenHeight: integer = 600;

Type

  TGameState = (gsNone, gsMainMenu, gsEditor, gsInGame);

  { TBridgeBuilder }

  TBridgeBuilder = Class
  private
    GameState: TGameState;
    MainScreen: TMainScreen;
    EditorScreen: TEditorScreen;
    OnMouseDownCapture: TMouseEvent;
    OnMouseMoveCapture: TMouseMoveEvent;
    OnMouseUpCapture: TMouseEvent;
    LastMousePos: TPoint;
    FinalZoneIndex: integer;
    StartNode: integer; // Der NodeIndex auf den geklickt wurde (im MouseDown)

    Procedure OnExitButtonClick(Sender: TObject);
    Procedure OnEditorButtonClick(Sender: TObject);
    Procedure OnLoadButtonClick(Sender: TObject);
    Procedure OnLoadBackTexClick(Sender: TObject);

    Procedure OnMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    Procedure OnMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    Procedure OnMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  public
    Map: TMap;
    OnSwitchToEditor: TNotifyEvent;
    OnLeaveEditor: TNotifyEvent;

    Constructor Create(); virtual;
    Destructor Destroy(); override;

    Procedure InitOpenGL(Owner: TOpenGLControl);
    Procedure Render();

    Procedure SwitchToMainMenu;
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
  //  map.LoadFromFile(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'Levels' + PathDelim + 'Level_01.lvl'); // TODO: Debug remove !
  map.LoadFromFile(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'Levels' + PathDelim + 'Level_02.lvl'); // TODO: Debug remove !
  MainScreen.visible := false;
  EditorScreen.visible := true;

    Da Muss der Zoom mittels CTRL und Wheel rein !!

End;

Procedure TBridgeBuilder.OnLoadButtonClick(Sender: TObject);
Begin

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
    gx := (LastMousePos.x + Map.Offset.X);
    gy := (LastMousePos.y + Map.Offset.Y);
    gx := (gx - gx Mod GridSize) + GridSize Div 2;
    gy := (gy - gy Mod GridSize) + GridSize Div 2;
    Case GameState Of
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
          End;
          If ssright In shift Then Begin
            If EditorScreen.EditFixedPoints.Checked Or EditorScreen.EditInitialEdges.Checked Then Begin
              map.DelFixedBolt(gx, gy);
            End;
            StartNode := -1;
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
  // Scroll the Map :)
  If (ssright In shift) And assigned(Map) Then Begin
    dx := NewMousePos.x - LastMousePos.x;
    dy := NewMousePos.y - LastMousePos.y;
    Map.Offset.x := max(0, min(map.Dim.x - ScreenWidth, Map.Offset.x - dx));
    Map.Offset.Y := max(0, min(map.Dim.y - ScreenHeight, Map.Offset.Y - dy));
  End;
  If GameState = gsEditor Then Begin
    If EditorScreen.EditFinalZone.Checked And (ssLeft In Shift) And (FinalZoneIndex <> -1) Then Begin
      gx := (NewMousePos.x + Map.Offset.X);
      gy := (NewMousePos.y + Map.Offset.Y);
      gx := (gx - gx Mod GridSize) + GridSize Div 2;
      gy := (gy - gy Mod GridSize) + GridSize Div 2;
      map.FinalZone[FinalZoneIndex] := v2(gx, gy);
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
//Var
//  EndNode, gx, gy: integer;
Begin

  If GameState = gsEditor Then Begin
    //    If EditorScreen.EditInitialEdges.Checked And (StartNode <> -1) Then Begin
    //      If ssleft In shift Then Begin
    //        // Setzen
    //        If EditorScreen.EditInitialEdges.Checked Then Begin
    //          gx := (x + Map.Offset.X);
    //          gy := (y + Map.Offset.Y);
    //          gx := (gx - gx Mod GridSize) + GridSize Div 2;
    //          gy := (gy - gy Mod GridSize) + GridSize Div 2;
    //          EndNode := map.FindFixedBolt(gx, gy);
    //          If EndNode = -1 Then Begin
    //            EndNode := map.AddFixedBolt(gx, gy);
    //          End;
    //          map.addFixedEdge(StartNode, EndNode);
    //
    //        End;
    //      End;
    //      If ssRight In Shift Then Begin
    //        // Löschen, aber nur wenn es auch eine Ziel Edge gibt
    //
    //      End;
    //
    //      //      gx := (NewMousePos.x + Map.Offset.X);
    //      //      gy := (NewMousePos.y + Map.Offset.Y);
    //      //      gx := (gx - gx Mod GridSize) + GridSize Div 2;
    //      //      gy := (gy - gy Mod GridSize) + GridSize Div 2;
    //      //      map.FinalZone[FinalZoneIndex] := v2(gx, gy);
    //    End;
  End;
End;

Procedure TBridgeBuilder.InitOpenGL(Owner: TOpenGLControl);
Begin
  OnMouseDownCapture := owner.OnMouseDown;
  owner.OnMouseDown := @OnMouseDown;
  OnMouseMoveCapture := owner.OnMouseMove;
  owner.OnMouseMove := @OnMouseMove;
  OnMouseUpCapture := owner.OnMouseUp;
  owner.OnMouseup := @OnMouseUp;
  MainScreen := TMainScreen.Create(Owner);
  MainScreen.ExitButton.OnClick := @OnExitButtonClick;
  MainScreen.EditorButton.OnClick := @OnEditorButtonClick;
  MainScreen.LoadButton.OnClick := @OnLoadButtonClick;
  EditorScreen := TEditorScreen.Create(Owner);
  EditorScreen.LoadBackGround.OnClick := @OnLoadBackTexClick;

  map.InitOpenGL;
  SwitchToMainMenu();
End;

Procedure TBridgeBuilder.Render;
Var
  len: Single;
Begin
  Case GameState Of
    gsMainMenu: Begin
        MainScreen.Render;
      End;
    gsEditor: Begin
        map.Render();
        If EditorScreen.EditInitialEdges.Checked And (StartNode <> -1) Then Begin
          len := LenV2(map.fFixedBolts[StartNode] - LastMousePos - map.Offset);
          If len <= MaxEdgeLen Then Begin
            glBindTexture(GL_TEXTURE_2D, 0);
            glLineWidth(3);
            glColor3f(0.25, 0.25, 0.25);
            glPushMatrix;
            glTranslatef(-map.Offset.x, -map.Offset.y, 0);
            glBegin(GL_LINES);
            glVertex2f(map.fFixedBolts[StartNode].x, map.fFixedBolts[StartNode].y);
            glVertex2f(LastMousePos.x + map.Offset.x, LastMousePos.y + map.Offset.y);
            glEnd;
            glPopMatrix;
            glLineWidth(1);
          End;
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
End;

End.


