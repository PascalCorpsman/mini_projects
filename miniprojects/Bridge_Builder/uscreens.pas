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

  public
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
  LoadButton.Left := 400 - LoadButton.Width Div 2;

  EditorButton := TOpenGl_Button.Create(Owner);
  EditorButton.LoadTextures('GFX' + PathDelim + 'Editor.png', 'GFX' + PathDelim + 'Editor_s.png', 'GFX' + PathDelim + 'Editor_s.png');
  EditorButton.Top := 250 + LoadButton.Height + 24;
  EditorButton.Left := 400 - EditorButton.Width Div 2;

  ExitButton := TOpenGl_Button.Create(Owner);
  ExitButton.LoadTextures('GFX' + PathDelim + 'Exit.png', 'GFX' + PathDelim + 'Exit_s.png', 'GFX' + PathDelim + 'Exit_s.png');
  ExitButton.Top := 600 - ExitButton.Height - 24;
  ExitButton.Left := 400 - ExitButton.Width Div 2;
End;

Destructor TMainScreen.Destroy;
Begin
  ExitButton.Free;
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
  OpenGL_ASCII_Font.Textout(round(800 - OpenGL_ASCII_Font.TextWidth(s)), round(600 - OpenGL_ASCII_Font.TextHeight(s)), s);
End;

End.

