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
Unit urgb_jumper;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, OpenGlcontext, uopengl_widgetset, ueventer, controls;

Type

  TGameState = (
    gsMainMenu
    , gsGame
    );

  { TGame }

  TGame = Class
  private
    fGameState: TGameState;
    fOwner: TOpenGLControl;
    FOnKeyDownCapture, FOnKeyUpCapture: TKeyEvent;
    (*
     * Main Menu
     *)
    fNewButton: TOpenGl_Button;
    fCloseButton: TOpenGl_Button;
    fMainBack: TOpenGl_Image;

    Procedure SetGameState(aNewGameState: TGameState);

    Procedure OnCloseButtonClick(Sender: TObject);

  protected
    Procedure FOnKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure FOnKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);

  public
    Constructor Create();
    Destructor Destroy; override;

    Procedure Initialize(Const Owner: TOpenGLControl); // Lädt alles was es so zu laden gibt (OpenGL-Technisch), wird einmalig in OnMakeCurrent

    Procedure Render();
  End;

Implementation

Uses unit1;

{ TGame }

Constructor TGame.Create;
Begin
  Inherited Create;
End;

Destructor TGame.Destroy;
Begin
  fNewButton.free;
  fCloseButton.free;
  fMainBack.free;
End;

Procedure TGame.SetGameState(aNewGameState: TGameState);
Begin
  // Reset der gesamten LCL
  fGameState := aNewGameState;

  fNewButton.Visible := false;
  fCloseButton.Visible := false;
  fMainBack.Visible := false;
  Case fGameState Of
    gsMainMenu: Begin
        fNewButton.Visible := true;
        fCloseButton.Visible := true;
        fMainBack.Visible := true;
      End;
    gsGame: Begin

      End;
  End;
End;

Procedure TGame.OnCloseButtonClick(Sender: TObject);
Begin
  form1.Close;
End;

Procedure TGame.FOnKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
Begin

  If assigned(FOnKeyDownCapture) Then Begin
    FOnKeyDownCapture(sender, key, shift);
  End;
End;

Procedure TGame.FOnKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
Begin

  If assigned(FOnKeyUpCapture) Then Begin
    FOnKeyUpCapture(sender, key, shift);
  End;
End;

Procedure TGame.Initialize(Const Owner: TOpenGLControl);
Begin
  fOwner := Owner;
  FOnKeyDownCapture := Owner.OnKeyDown;
  Owner.OnKeyDown := @FOnKeyDown;
  FOnKeyUpCapture := Owner.OnKeyUp;
  Owner.OnKeyUp := @FOnKeyUp;

  fNewButton := TOpenGl_Button.Create(fOwner);
  fNewButton.LoadTextures('GFX' + PathDelim + 'New_Up.png', 'GFX' + PathDelim + 'New_Up.png', 'GFX' + PathDelim + 'New_Down.png');
  fNewButton.Left := 55;
  fNewButton.Top := 51;

  fCloseButton := TOpenGl_Button.Create(fOwner);
  fCloseButton.LoadTextures('GFX' + PathDelim + 'Close_Up.png', 'GFX' + PathDelim + 'Close_Up.png', 'GFX' + PathDelim + 'Close_Down.png');
  fCloseButton.Left := 51;
  fCloseButton.Top := 67;
  fCloseButton.OnClick := @OnCloseButtonClick;

  fMainBack := TOpenGl_Image.Create(fOwner);
  fMainBack.SetImage('GFX' + PathDelim + 'Main.png');
  fMainBack.Top := 0;
  fMainBack.Left := 0;

  SetGameState(gsMainMenu);
End;

Procedure TGame.Render;
Begin
  Case fGameState Of
    gsMainMenu: Begin
        fMainBack.Render();
        fNewButton.Render();
        fCloseButton.Render();
      End;
    gsGame: Begin

      End;
  End;
End;

End.


