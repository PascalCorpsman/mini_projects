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
  urgb_level, urgb_player;

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
    fLastTimeStamp: QWord;
    fGameState: TGameState;
    fOwner: TOpenGLControl;
    FOnKeyDownCapture, FOnKeyUpCapture: TKeyEvent;
    fActualLevelIndex: Integer;
    fActualLevel: TRGB_Level;
    fPlayer: TRGB_Player;
    fRightPressed, fLeftPressed, fUpPressed: integer;
    fBlockAllKeyDownUntilOneRelease: Boolean;

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

    Procedure ApplyPhysics();

    Procedure ResetLevel();

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

Uses unit1, LCLType, uOpenGL_ASCII_Font, dglOpenGL, Graphics, uvectormath, urgb_physics;

Const
  JumpTime = 1000;

  { TGame }

Constructor TGame.Create;
Begin
  Inherited Create;
  fBlockAllKeyDownUntilOneRelease := false;
  fActualLevel := TRGB_Level.Create();
  fPlayer := TRGB_Player.Create();
End;

Destructor TGame.Destroy;
Begin
  fNewButton.free;
  fCloseButton.free;
  fMainBack.free;
  fWinBack.free;
  fActualLevel.free;
  fPlayer.free;
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
  form1.caption := DefCaption + ' Level ' + inttostr(Level);
  fActualLevelIndex := Level;
  Filename := 'Levels' + PathDelim + 'Level_' + inttostr(Level) + '.png';
  If Not FileExists(Filename) Then Begin
    fBlockAllKeyDownUntilOneRelease := true;
    SetGameState(gsWin);
  End
  Else Begin
    // 1. Level Laden und Prüfen
    If Not fActualLevel.LoadLevel(Filename) Then Begin
      SetGameState(gsLevelError);
      exit;
    End;
    // 2. Spielercharacter "Resetten" auf Spieler Start Pos
    fPlayer.setColor(pcBlue);
    ResetLevel;

    // 3. Keystates Resetten
    // 4. Gamestate change -> Los Gehts
    SetGameState(gsGame);
  End;
End;

Procedure TGame.ApplyPhysics;
Var
  p, aforce: TVector2;
  n, delta: QWord;
  ColReasons: TContactReasons;
Begin
  n := GetTickCount64;
  delta := n - fLastTimeStamp;
  fLastTimeStamp := n;
  If delta = 0 Then exit;
  ColReasons := fActualLevel.CollideWith(fPlayer);
  // 1. Collect All Player Forces
  aforce := gravity;
  p := v2(0, 0);

  fPlayer.TouchesGround := crRemoveDown In ColReasons;

  fPlayer.TouchesWall :=
    ((crRemoveRight In ColReasons) And (fRightPressed <> 0))
    Or ((crRemoveLeft In ColReasons) And (fLeftPressed <> 0));

  // Vertical Movement
  If Not (fPlayer.TouchesGround) And Not (fPlayer.TouchesWall) Then Begin
    p := p + aforce * delta / 1000;
  End;
  If (crRemoveUp In ColReasons) Then Begin
    fUpPressed := 0;
  End;
  If fUpPressed > 0 Then Begin
    If (JumpTime Div 2 < fUpPressed) Then Begin
      p := P - aforce * delta / 500;
    End;
    fUpPressed := fUpPressed - delta;
  End;
  // Horizontal Movement
  If (fRightPressed <> 0) And (Not (crRemoveRight In ColReasons)) Then Begin
    p := p + v2(1, 0) * delta / 200;
  End;
  If (fLeftPressed <> 0) And (Not (crRemoveLeft In ColReasons)) Then Begin
    p := p + v2(-1, 0) * delta / 200;
  End;
  // 2. Collide
  If crDie In ColReasons Then Begin
    // TODO: inc "Tode"
    ResetLevel();
    exit;
  End;
  If crWin In ColReasons Then Begin
    LoadLevel(fActualLevelIndex + 1);
    exit;
  End;
  If crChangeToR In ColReasons Then fPlayer.SetColor(pcRed);
  If crChangeToG In ColReasons Then fPlayer.SetColor(pcGreen);
  If crChangeToB In ColReasons Then fPlayer.SetColor(pcBlue);

  p := p + fPlayer.Position;
  If fPlayer.TouchesGround And (fUpPressed <= 0) Then Begin
    p.y := round(p.y);
  End;
  If fPlayer.TouchesWall And (fLeftPressed <> 0) And (crRemoveLeft In ColReasons) Then Begin
    p.x := round(p.x);
  End;
  If fPlayer.TouchesWall And (fRightPressed <> 0) And (crRemoveRight In ColReasons) Then Begin
    p.x := round(p.x);
  End;
  fPlayer.SetPos(p);
End;

Procedure TGame.ResetLevel();
Begin
  fPlayer.SetPos(fActualLevel.StartPos);
  fActualLevel.Reset;
  fLastTimeStamp := GetTickCount64;
  fRightPressed := 0;
  fLeftPressed := 0;
  fUpPressed := 0;
End;

Procedure TGame.FOnKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
Begin
  If fBlockAllKeyDownUntilOneRelease Then exit;
  Case fGameState Of
    gsMainMenu: Begin
        If key = VK_N Then fNewButton.Click;
        If (key = VK_ESCAPE) Or (key = VK_C) Then fCloseButton.click;
      End;
    gsGame: Begin
        // R -> zu Rot
        If key = VK_R Then fPlayer.SetColor(pcRed);
        // G -> zu Grün
        If key = VK_G Then fPlayer.SetColor(pcGreen);
        // B -> zu Blau
        If key = VK_B Then fPlayer.SetColor(pcBlue);
        // C -> Nächste Farbe im Zyklus "RGB"
        If key = VK_C Then Begin
          Case fPlayer.Color Of
            pcRed: fPlayer.SetColor(pcGreen);
            pcGreen: fPlayer.SetColor(pcBlue);
            pcBlue: fPlayer.SetColor(pcRed);
          End;
        End;
        // U -> Reset auf Start Position (auch Reset des Levels)
        If key = VK_U Then Begin
          // TODO: Inc Tode
          ResetLevel;
        End;
        // Cursor -> Move Player
        If key = VK_RIGHT Then fRightPressed := 1;
        If key = VK_LEFT Then fLeftPressed := 1;
        If key = VK_UP Then Begin
          If (fUpPressed <= 0) And ((fPlayer.TouchesGround) Or fplayer.TouchesWall) Then Begin
            fUpPressed := JumpTime;
          End;
        End;

        // Abort
        If key = VK_ESCAPE Then SetGameState(gsMainMenu);
      End;
    gsLevelError: Begin
        If key = VK_N Then LoadLevel(fActualLevelIndex + 1);
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
  fBlockAllKeyDownUntilOneRelease := false;
  If fGameState = gsGame Then Begin
    If key = VK_RIGHT Then fRightPressed := 0;
    If key = VK_LEFT Then fLeftPressed := 0;
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

  fPlayer.Initialize;
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
        ApplyPhysics;
        fPlayer.SetCamPosToRenderingContext;
        fActualLevel.Render();
        fPlayer.Render();
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

