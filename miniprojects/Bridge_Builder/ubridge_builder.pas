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
  Classes, SysUtils, uscreens, OpenGlcontext;

Const
  (*
   * History: 0.01 - Initialversion
   *)
  Version = '0.01';

Type

  TGameState = (gsNone, gsMainMenu);

  { TBridgeBuilder }

  TBridgeBuilder = Class
  private
    GameState: TGameState;
    MainScreen: TMainScreen;
    Procedure OnExitButtonClick(Sender: TObject);
  public
    Constructor Create(); virtual;
    Destructor Destroy(); override;
    Procedure InitOpenGL(Owner: TOpenGLControl);
    Procedure Render();
  End;

Implementation

Uses unit1; // für form1.close

{ TBridgeBuilder }

Constructor TBridgeBuilder.Create;
Begin
  Inherited Create;
  GameState := gsNone;

End;

Destructor TBridgeBuilder.Destroy;
Begin
  MainScreen.free;
End;

Procedure TBridgeBuilder.OnExitButtonClick(Sender: TObject);
Begin
  form1.close;
End;

Procedure TBridgeBuilder.InitOpenGL(Owner: TOpenGLControl);
Begin
  MainScreen := TMainScreen.Create(Owner);
  MainScreen.ExitButton.OnClick := @OnExitButtonClick;

  GameState := gsMainMenu;
End;

Procedure TBridgeBuilder.Render;
Begin
  Case GameState Of
    gsMainMenu: Begin
        MainScreen.Render;
      End;
  End;
End;

End.

