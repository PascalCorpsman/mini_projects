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
Unit uscreens;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, uopengl_graphikengine, uopengl_widgetset, OpenGlcontext, uOpenGL_ASCII_Font;

Type

  { TMainScreen }

  TMainScreen = Class
    ExitButton: TOpenGl_Button;
    LoadButton: TOpenGl_Button;
    EditorButton: TOpenGl_Button;
  private
    fTexture: TGraphikItem;
    Procedure SetVisible(AValue: Boolean);
  public
    Property Visible: Boolean write SetVisible;
    Constructor Create(Owner: TOpenGLControl); virtual;
    Destructor Destroy(); override;
    Procedure Render;
  End;

  { TEditorScreen }

  TEditorScreen = Class
    SetStartPoint: TOpenGL_Radiobutton;
    EditCollider: TOpenGL_Radiobutton;
    EditFixedPoints: TOpenGL_Radiobutton;
    EditFinalZone: TOpenGL_Radiobutton;
    EditInitialEdges: TOpenGL_Radiobutton;
    LoadBackGround: TOpenGl_Button;
  private
    Procedure SetVisible(AValue: Boolean);
  public
    Property Visible: Boolean write SetVisible;
    Constructor Create(Owner: TOpenGLControl); virtual;
    Destructor Destroy(); override;
    Procedure Render;
  End;

Implementation

Uses ubridge_builder, dglOpenGL;

{ TMainScreen }

Constructor TMainScreen.Create(Owner: TOpenGLControl);
Begin
  fTexture := OpenGL_GraphikEngine.LoadAlphaGraphikItem('GFX' + PathDelim + 'MainMenu.png', smClamp);

  LoadButton := TOpenGl_Button.Create(Owner);
  LoadButton.LoadTextures('GFX' + PathDelim + 'Load.png', 'GFX' + PathDelim + 'Load_s.png', 'GFX' + PathDelim + 'Load_s.png');
  LoadButton.Top := 250;
  LoadButton.Left := (ScreenWidth - LoadButton.Width) Div 2;

  EditorButton := TOpenGl_Button.Create(Owner);
  EditorButton.LoadTextures('GFX' + PathDelim + 'Editor.png', 'GFX' + PathDelim + 'Editor_s.png', 'GFX' + PathDelim + 'Editor_s.png');
  EditorButton.Top := 250 + LoadButton.Height + 24;
  EditorButton.Left := (ScreenWidth - EditorButton.Width) Div 2;

  ExitButton := TOpenGl_Button.Create(Owner);
  ExitButton.LoadTextures('GFX' + PathDelim + 'Exit.png', 'GFX' + PathDelim + 'Exit_s.png', 'GFX' + PathDelim + 'Exit_s.png');
  ExitButton.Top := ScreenHeight - ExitButton.Height - 24;
  ExitButton.Left := (ScreenWidth - ExitButton.Width) Div 2;
End;

Destructor TMainScreen.Destroy;
Begin
  ExitButton.Free;
End;

Procedure TMainScreen.SetVisible(AValue: Boolean);
Begin
  ExitButton.Visible := AValue;
  LoadButton.Visible := AValue;
  EditorButton.Visible := AValue;
End;

Procedure TMainScreen.Render;
Var
  s: String;
Begin
  RenderAlphaQuad(0, 0, fTexture);
  LoadButton.Render();
  EditorButton.Render();
  ExitButton.Render();
  glBindTexture(GL_TEXTURE_2D, 0);
  s := 'Version: ' + Version;
  OpenGL_ASCII_Font.Textout(round(ScreenWidth - OpenGL_ASCII_Font.TextWidth(s)), round(ScreenHeight - OpenGL_ASCII_Font.TextHeight(s)), s);
End;

{ TEditorScreen }

Procedure TEditorScreen.SetVisible(AValue: Boolean);
Begin
  SetStartPoint.Visible := AValue;
  EditCollider.Visible := AValue;
  EditFixedPoints.Visible := AValue;
  EditFinalZone.Visible := AValue;
  EditInitialEdges.Visible := AValue;
  LoadBackGround.Visible := AValue;
End;

Constructor TEditorScreen.Create(Owner: TOpenGLControl);
Begin
  Inherited Create;

  setStartPoint := TOpenGL_Radiobutton.Create(Owner, '');
  setStartPoint.Top := 10;
  setStartPoint.Left := 10;
  setStartPoint.Caption := 'Set startnode';

  EditFixedPoints := TOpenGL_Radiobutton.Create(Owner, '');
  EditFixedPoints.Top := setStartPoint.top + setStartPoint.Height + 5;
  EditFixedPoints.Left := 10;
  EditFixedPoints.Caption := 'Edit fixed points';

  EditCollider := TOpenGL_Radiobutton.Create(Owner, '');
  EditCollider.Top := EditFixedPoints.top + EditFixedPoints.Height + 5;
  EditCollider.Left := 10;
  EditCollider.Caption := 'Edit collider';

  EditFinalZone := TOpenGL_Radiobutton.Create(Owner, '');
  EditFinalZone.Top := EditCollider.top + EditCollider.Height + 5;
  EditFinalZone.Left := 10;
  EditFinalZone.Caption := 'Edit final zone';

  EditInitialEdges := TOpenGL_Radiobutton.Create(Owner, '');
  EditInitialEdges.Top := EditFinalZone.top + EditFinalZone.Height + 5;
  EditInitialEdges.Left := 10;
  EditInitialEdges.Caption := 'Edit initial edges';

  LoadBackGround := TOpenGl_Button.Create(Owner);
  LoadBackGround.caption := 'Load background';
  LoadBackGround.top := 10;
  LoadBackGround.width := round(OpenGL_ASCII_Font.TextWidth(LoadBackGround.caption)) + 20;
  LoadBackGround.Left := ScreenWidth - LoadBackGround.width - 10;
End;

Destructor TEditorScreen.Destroy;
Begin
  EditCollider.free;
  EditFixedPoints.free;
  EditFinalZone.free;
  LoadBackGround.free;
  setStartPoint.free;
  EditInitialEdges.free;
End;

Procedure TEditorScreen.Render;
Begin
  EditCollider.Render();
  EditFixedPoints.Render();
  EditFinalZone.Render();
  LoadBackGround.render();
  setStartPoint.render();
  EditInitialEdges.Render();
End;

End.

