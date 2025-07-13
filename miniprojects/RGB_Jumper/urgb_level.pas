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
Unit urgb_level;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, uopengl_graphikengine, uvectormath, urgb_player, urgb_physics;

Type

  TFieldAttribute = (
    faStart
    , faFin
    , faChangeToR
    , faChangeToG
    , faChangeToB
    , faR
    , faG
    , faB
    );

  TField = Set Of TFieldAttribute;

  { TRGB_Level }

  TRGB_Level = Class
  private
    fStartPos: TVector2;
    fTexture: TGraphikItem;
    fLevelData: Array Of Array Of TField;
  public
    Property StartPos: TVector2 read fStartPos;

    Constructor Create(); virtual;
    Destructor Destroy(); override;

    Function LoadLevel(Const aFilename: String): Boolean;
    Procedure Reset();

    Procedure Render();
    Function CollideWith(Var aPlayer: TRGB_Player): TContactReasons;
  End;

Implementation

Uses Graphics, IntfGraphics, FPImage;

Const
  WhiteColor: tfpcolor = (red: 255 * 257; Green: 255 * 257; Blue: 255 * 257; Alpha: 255 * 257);
  OrangeColor: tfpcolor = (red: 204 * 257; Green: 153 * 257; Blue: 0 * 257; Alpha: 255 * 257);

  RedColor: tfpcolor = (red: 204 * 257; Green: 0 * 257; Blue: 0 * 257; Alpha: 255 * 257);
  GreenColor: tfpcolor = (red: 0 * 257; Green: 153 * 257; Blue: 51 * 257; Alpha: 255 * 257);
  BlueColor: tfpcolor = (red: 51 * 257; Green: 51 * 257; Blue: 255 * 257; Alpha: 255 * 257);

  EmptyColor: tfpcolor = (red: 0; Green: 0; Blue: 0; Alpha: 0);

Procedure Nop();
Begin
End;

{ TRGB_Level }

Constructor TRGB_Level.Create;
Begin
  Inherited Create;
  fTexture.Image := 0;
End;

Destructor TRGB_Level.Destroy;
Begin

End;

Function TRGB_Level.LoadLevel(Const aFilename: String): Boolean;
Var
  p: TPortableNetworkGraphic;
  a, b: TBitmap;
  intfa, intfb: TLazIntfImage;
  i, j, x, y: Integer;
  c1, c2, c3: TFPColor;
  Flags: Integer;
Begin
  result := false;
  p := TPortableNetworkGraphic.Create;
  p.LoadFromFile(aFilename);
  b := TBitmap.Create;
  b.Assign(p);
  p.free;
  If ((b.Width Mod 8) <> 0) Or
    ((b.Height Mod 8) <> 0) Then Begin
    b.free;
    exit;
  End;
  intfb := b.CreateIntfImage;
  intfa := b.CreateIntfImage;
  setlength(fLevelData, b.Width Div 8, b.height Div 8);
  fStartPos := v2(-1, -1);
  Flags := 0;
  For i := 0 To high(fLevelData) Do Begin
    For j := 0 To high(fLevelData[i]) Do Begin
      // Heuristik zum "erkennen" der Jeweiligen Feldattribute, alles andere ist Unbekannt und wird ignoriert..
      fLevelData[i, j] := [];
      (*
       * Man braucht aktuell nicht alle Pixel zum erkennen es reichen die Folgenden
       *
       *   xx C1 xx xx xx xx xx xx
       *   xx C2 C3 xx xx xx xx xx
       *   xx xx xx xx xx xx xx xx
       *   xx xx xx xx xx xx xx xx
       *   xx xx xx xx xx xx xx xx
       *   xx xx xx xx xx xx xx xx
       *   xx xx xx xx xx xx xx xx
       *   xx xx xx xx xx xx xx xx
       *)

      c1 := intfb.Colors[i * 8 + 1, j * 8];
      c2 := intfb.Colors[i * 8 + 1, j * 8 + 1];
      c3 := intfb.Colors[i * 8 + 2, j * 8 + 1];

      If c1 = WhiteColor Then Begin
        Flags := Flags Or 1;
        fLevelData[i, j] := fLevelData[i, j] + [faStart];
        fStartPos := v2(i, j);
      End;

      If c1 = OrangeColor Then Begin
        fLevelData[i, j] := fLevelData[i, j] + [faFin];
        Flags := Flags Or 2;
      End;

      If (c2 = RedColor) And (c3 <> RedColor) Then fLevelData[i, j] := fLevelData[i, j] + [faChangeToR];
      If (c2 = GreenColor) And (c3 <> GreenColor) Then fLevelData[i, j] := fLevelData[i, j] + [faChangeToG];
      If (c2 = BlueColor) And (c3 <> BlueColor) Then fLevelData[i, j] := fLevelData[i, j] + [faChangeToB];

      If (c2 = RedColor) And (c3 = RedColor) Then fLevelData[i, j] := fLevelData[i, j] + [faR];
      If (c2 = GreenColor) And (c3 = GreenColor) Then fLevelData[i, j] := fLevelData[i, j] + [faG];
      If (c2 = BlueColor) And (c3 = BlueColor) Then fLevelData[i, j] := fLevelData[i, j] + [faB];

      // Manche Felder werden "gelöscht"
      If (fLevelData[i, j] * [faChangeToR, faChangeToG, faChangeToB, faStart]) <> [] Then Begin
        For x := 0 To 7 Do
          For y := 0 To 7 Do Begin
            intfa.Colors[i * 8 + x, j * 8 + y] := EmptyColor;
            intfb.Colors[i * 8 + x, j * 8 + y] := EmptyColor;
          End;
      End;

      // Manche Felder werden Anschließend wieder "restauriert"
      If (fLevelData[i, j] * [faChangeToR, faChangeToG, faChangeToB]) <> [] Then Begin
        c1 := EmptyColor;
        If faChangeToR In fLevelData[i, j] Then c1 := RedColor;
        If faChangeToG In fLevelData[i, j] Then c1 := GreenColor;
        If faChangeToB In fLevelData[i, j] Then c1 := BlueColor;
        For x := 0 To 7 Do
          For y := 0 To 7 Do Begin
            If ((x + y) Mod 2 = 0) Then Begin
              If Not ((x = 0) And (y = 0)) And
                Not ((x = 7) And (y = 0)) And
                Not ((x = 7) And (y = 7)) And
                Not ((x = 0) And (y = 7)) Then Begin
                intfa.Colors[i * 8 + x, j * 8 + y] := c1;
                intfb.Colors[i * 8 + x, j * 8 + y] := c1;
              End;
            End;
          End;
      End;
    End;
  End;

  // ggf. Alte textur löschen
  If fTexture.Image <> 0 Then Begin
    OpenGL_GraphikEngine.RemoveGraphik(fTexture.Image);
    fTexture.Image := 0;
  End;

  // Neue Backtex anhand obiger Manipulationen erstellen
  b.LoadFromIntfImage(intfb);
  a := TBitmap.Create;
  a.LoadFromIntfImage(intfa);
  fTexture.image := OpenGL_GraphikEngine.LoadAlphaGraphik(b, a, aFilename, smClamp);
  fTexture := OpenGL_GraphikEngine.GetInfo(fTexture.image);
  a.free;
  b.free;
  intfa.free;
  intfb.free;

  result := Flags = 3;
End;

Procedure TRGB_Level.Reset;
Begin
  // Aktuell ist alles Statisch, da gibt es noch nichts zum "resetten" ..
End;

Procedure TRGB_Level.Render;
Begin
  RenderQuad(0, 0, fTexture);
End;

Function TRGB_Level.CollideWith(Var aPlayer: TRGB_Player): TContactReasons;
(*
 *     10  11
 *    xxxxxxxx
 *   2x6xxxx9x5
 *    xxxxxxxx
 *    xxxxxxxx
 *    xxxxxxxx
 *    xxxxxxxx
 *   3x7xxxx8x4
 *    xxxxxxxx
 *     0    1
 *)

Const
  Delta = 1 / 8; // Eine Koordinate ist 8 Pixel Breit
Var
  i, x, y: integer;
  p: Array[0..11] Of TVector2;
Begin
  result := [];
  x := trunc(aPlayer.Position.x);
  y := trunc(aPlayer.Position.y);
  // Trigger die den Spieler gegen die Welt Blocken

  // World Leave Events ..
  If (y >= high(fLevelData)) Then Begin
    result := result + [crDie];
    exit;
  End;

  // Grav Collider
  p[0] := aPlayer.Position + v2(delta, 1 + delta);
  p[1] := aPlayer.Position + v2(7 * delta, 1 + delta);

  // Left Collider
  p[2] := aPlayer.Position + v2(-delta, Delta);
  p[3] := aPlayer.Position + v2(-delta, 7 * Delta);

  // Right Collider
  p[4] := aPlayer.Position + v2(1, Delta);
  p[5] := aPlayer.Position + v2(1, 7 * Delta);

  // Internal Trigger
  p[6] := aPlayer.Position + v2(delta, delta);
  p[7] := aPlayer.Position + v2(delta, 7 * delta);
  p[8] := aPlayer.Position + v2(7 * delta, 7 * delta);
  p[9] := aPlayer.Position + v2(7 * delta, delta);

  p[10] := aPlayer.Position + v2(delta, -delta);
  p[11] := aPlayer.Position + v2(7 * delta, -delta);

  // Schwerkraft Blocker
  For i := 0 To 1 Do Begin
    x := trunc(p[i].x);
    y := trunc(p[i].y);
    If y <= high(fLevelData[x]) Then Begin
      Case aPlayer.Color Of
        pcRed: If faR In fLevelData[x, y] Then result := result + [crRemoveDown];
        pcGreen: If faG In fLevelData[x, y] Then result := result + [crRemoveDown];
        pcBlue: If faB In fLevelData[x, y] Then result := result + [crRemoveDown];
      End;
    End;
  End;

  // Schwerkraft Blocker
  For i := 10 To 11 Do Begin
    x := trunc(p[i].x);
    y := trunc(p[i].y);
    If y <= high(fLevelData[x]) Then Begin
      Case aPlayer.Color Of
        pcRed: If faR In fLevelData[x, y] Then result := result + [crRemoveUp];
        pcGreen: If faG In fLevelData[x, y] Then result := result + [crRemoveUp];
        pcBlue: If faB In fLevelData[x, y] Then result := result + [crRemoveUp];
      End;
    End;
  End;

  // Left Blocker
  For i := 2 To 3 Do Begin
    x := trunc(p[i].x);
    y := trunc(p[i].y);
    If y <= high(fLevelData[x]) Then Begin
      Case aPlayer.Color Of
        pcRed: If faR In fLevelData[x, y] Then result := result + [crRemoveLeft];
        pcGreen: If faG In fLevelData[x, y] Then result := result + [crRemoveLeft];
        pcBlue: If faB In fLevelData[x, y] Then result := result + [crRemoveLeft];
      End;
    End;
  End;

  // Right Blocker
  For i := 4 To 5 Do Begin
    x := trunc(p[i].x);
    y := trunc(p[i].y);
    If y <= high(fLevelData[x]) Then Begin
      Case aPlayer.Color Of
        pcRed: If faR In fLevelData[x, y] Then result := result + [crRemoveRight];
        pcGreen: If faG In fLevelData[x, y] Then result := result + [crRemoveRight];
        pcBlue: If faB In fLevelData[x, y] Then result := result + [crRemoveRight];
      End;
    End;
  End;

  // Trigger die der Spieler "erreichen" Kann
  For i := 6 To 9 Do Begin
    x := trunc(p[i].x);
    y := trunc(p[i].y);
    If (x >= 0) And (x <= high(fLevelData)) And
      (y >= 0) And (y <= high(fLevelData[x])) Then Begin
      If faChangeToR In fLevelData[x, y] Then result := result + [crChangeToR];
      If faChangeToG In fLevelData[x, y] Then result := result + [crChangeToG];
      If faChangeToB In fLevelData[x, y] Then result := result + [crChangeToB];
      If faFin In fLevelData[x, y] Then result := result + [crWin];
    End;
  End;
End;

End.

