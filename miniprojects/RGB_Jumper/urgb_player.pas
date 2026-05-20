(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of RGB_Jumper                                            *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit urgb_player;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, uvectormath, uopengl_graphikengine;

Type

  TPlayerColor = (pcBlue, pcGreen, pcRed);

  { TRGB_Player }

  TRGB_Player = Class
  private
    fPos: TVector2;
    fColor: TPlayerColor;
    tTextureR, tTextureG, tTextureB: TGraphikItem;
  public

    Force: TVector2;
    TouchesGround: Boolean;
    TouchesWall: Boolean;
    Property Color: TPlayerColor read fColor;
    Property Position: TVector2 read fPos;

    Constructor Create(); virtual;
    Destructor Destroy(); override;

    Procedure Initialize;

    Procedure SetPos(aPos: TVector2);
    Procedure SetColor(aColor: TPlayerColor);
    Function GetCamPosToRenderContext(): TVector2;
    Procedure Render;
  End;

Implementation

Uses dglOpenGL;

{ TRGB_Player }

Constructor TRGB_Player.Create;
Begin
  Inherited Create;
End;

Destructor TRGB_Player.Destroy;
Begin

End;

Procedure TRGB_Player.Initialize;
Begin
  tTextureR := OpenGL_GraphikEngine.LoadAlphaGraphikItem('GFX' + PathDelim + 'r.png');
  tTextureG := OpenGL_GraphikEngine.LoadAlphaGraphikItem('GFX' + PathDelim + 'g.png');
  tTextureB := OpenGL_GraphikEngine.LoadAlphaGraphikItem('GFX' + PathDelim + 'b.png');
End;

Procedure TRGB_Player.SetPos(aPos: TVector2);
Begin
  fPos := aPos;
End;

Procedure TRGB_Player.SetColor(aColor: TPlayerColor);
Begin
  fColor := aColor;
End;

Function TRGB_Player.GetCamPosToRenderContext(): TVector2;
Begin
  result := v2(-fPos.x * 8 + 64, -fPos.y * 8 + 64);
End;

Procedure TRGB_Player.Render;
Var
  item: TGraphikItem;
Begin
  Case fColor Of
    pcRed: item := tTextureR;
    pcGreen: item := tTextureG;
    pcBlue: item := tTextureB;
  End;
  RenderAlphaQuad(64, 64, 0, item);
End;

End.

