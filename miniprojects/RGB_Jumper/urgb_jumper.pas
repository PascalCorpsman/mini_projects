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
  Classes, SysUtils, OpenGlcontext, uopengl_widgetset, ueventer, controls,
  urgb_level;

Type

  TGameState = (
    gsMainMenu
    , gsGame
    , gsLevelError
    , gsWin
    );

  { TGame }

  TGame = Class
  private
    fGameState: TGameState;
    fOwner: TOpenGLControl;
    FOnKeyDownCapture, FOnKeyUpCapture: TKeyEvent;
    fActualLevelIndex: Integer;
    fActualLevel: TRGB_Level;

    (*
     * Main Menu
     *)
    fNewButton: TOpenGl_Button;
    fCloseButton: TOpenGl_Button;
    fMainBack: TOpenGl_Image;

    (*
     * Win Menu
     *)
    fWinBack: TOpenGl_Image;

    Procedure SetGameState(aNewGameState: TGameState);

    Procedure OnCloseButtonClick(Sender: TObject);
    Procedure OnNewButtonClick(Sender: TObject);

    Procedure LoadLevel(Const Level: integer);

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

Uses unit1, LCLType, uOpenGL_ASCII_Font, dglOpenGL, Graphics;

{ TGame }

Constructor TGame.Create;
Begin
  Inherited Create;
  fActualLevel := TRGB_Level.Create();
End;

Destructor TGame.Destroy;
Begin
  fNewButton.free;
  fCloseButton.free;
  fMainBack.free;
  fWinBack.free;
  fActualLevel.free;
End;

Procedure TGame.SetGameState(aNewGameState: TGameState);
Begin
  // Reset der gesamten LCL
  fGameState := aNewGameState;

  fNewButton.Visible := false;
  fCloseButton.Visible := false;
  fMainBack.Visible := false;
  fWinBack.Visible := false;
  Case fGameState Of
    gsMainMenu: Begin
        fNewButton.Visible := true;
        fCloseButton.Visible := true;
        fMainBack.Visible := true;
      End;
    gsGame: Begin

      End;
    gsLevelError: Begin

      End;
    gsWin: Begin
        fWinBack.Visible := true;
      End;
  End;
End;

Procedure TGame.OnCloseButtonClick(Sender: TObject);
Begin
  form1.Close;
End;

Procedure TGame.OnNewButtonClick(Sender: TObject);
Begin
  // Idee: Man muss die Levels immer von Level 1 Spielen
  LoadLevel(1);

  (*
   * Backlog:
   *  - Zeiten Messen => Highscores ?
   *  - "Tode" Messen => Highscores ?
   *)
End;

Procedure TGame.LoadLevel(Const Level: integer);
Var
  Filename: String;
Begin
  fActualLevelIndex := Level;
  Filename := 'Levels' + PathDelim + 'Level_' + inttostr(Level) + '.png';
  If Not FileExists(Filename) Then Begin
    SetGameState(gsWin);
  End
  Else Begin
    // 1. Level Laden und Prüfen
    If Not fActualLevel.LoadLevel(Filename) Then Begin
      SetGameState(gsLevelError);
      exit;
    End;
    // 2. Spielercharacter "Resetten" auf Spieler Start Pos
    // 3. Keystates Resetten
    // 4. Gamestate change -> Los Gehts
    SetGameState(gsGame);
  End;
End;

Procedure TGame.FOnKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
Begin
  Case fGameState Of
    gsMainMenu: Begin
        If key = VK_ESCAPE Then Begin
          fCloseButton.click;
        End;
      End;
    gsGame: Begin
        // R -> zu Rot
        // G -> zu Grün
        // B -> zu Blau
        // C -> Nächste Farbe im Zyklus "RGB"
        // U -> Reset auf Start Position (auch Reset des Levels)
        // Cursor -> Move Player
        If key = VK_ESCAPE Then SetGameState(gsMainMenu);
      End;
    gsLevelError: Begin
        If key = VK_N Then Begin
          LoadLevel(fActualLevelIndex + 1);
        End;
        If key = VK_ESCAPE Then SetGameState(gsMainMenu);
      End;
    gsWin: Begin
        // No matter which key -> return to Main menu ;)
        // TODO: goto Highscores ..
        SetGameState(gsMainMenu);
      End;
  End;
  If assigned(FOnKeyDownCapture) Then Begin
    FOnKeyDownCapture(sender, key, shift);
  End;
End;

Procedure TGame.FOnKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
Begin

  If fGameState = gsGame Then Begin

  End;
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
  fNewButton.OnClick := @OnNewButtonClick;

  fCloseButton := TOpenGl_Button.Create(fOwner);
  fCloseButton.LoadTextures('GFX' + PathDelim + 'Close_Up.png', 'GFX' + PathDelim + 'Close_Up.png', 'GFX' + PathDelim + 'Close_Down.png');
  fCloseButton.Left := 51;
  fCloseButton.Top := 67;
  fCloseButton.OnClick := @OnCloseButtonClick;

  fMainBack := TOpenGl_Image.Create(fOwner);
  fMainBack.SetImage('GFX' + PathDelim + 'Main.png');
  fMainBack.Top := 0;
  fMainBack.Left := 0;

  fWinBack := TOpenGl_Image.Create(fOwner);
  fWinBack.SetImage('GFX' + PathDelim + 'Win.png');
  fWinBack.Top := 0;
  fWinBack.Left := 0;

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
        fActualLevel.Render();
      End;
    gsLevelError: Begin
        glBindTexture(GL_TEXTURE_2D, 0);
        OpenGL_ASCII_Font.Color := clwhite;
        OpenGL_ASCII_Font.Textout(2, 32, 'Invalid level!');
        OpenGL_ASCII_Font.Textout(2, 52, 'n = next');
        OpenGL_ASCII_Font.Textout(2, 72, 'ESC = abort');
      End;
    gsWin: Begin
        fWinBack.Render();
      End;
  End;
End;

End.

