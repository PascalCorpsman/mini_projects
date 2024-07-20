(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Superdom                                              *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
(*
 * Eine Reverenz die gefunden wurde liegt unter:
 *   https://git.rockbox.org/cgit/rockbox.git/tree/apps/plugins/superdom.c?id=cb94b3ae2e
 * Codestand: 2024-07-20
 *)

Unit usuperdom;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Graphics, FileUtil, math,
  Dialogs,
  ugraphics,
  ubmp_graphikengine;

Const
  ColorBackHuman = $ADAAAD;
  ColorBackComputer = $525552;
  ColorBackGrid = $000000;
  ColorHuman = $A51C5A;
  ColorComputer = $299A4A;

  plHuman = 0;
  plComputer = 1;
  // TODO: Multi AI Support ? -> Dazu mus die TFieldColour aber eine int werden !
  // 0 = Human, > 1 = AI
  PlayerCount = 2;

  IMG_Tank = 0;
  IMG_Plane = 1;
  IMG_Nuke = 2;
  IMG_Men = 3;
  IMG_Farm = 4;
  IMG_Factory = 5;
  IMG_Player_offset = 6; // Die Anzahl der Bilder Pro Spieler

  // Some defines for the prices
  MenPrice = 1;
  MovePrice = 100;
  TankPrice = 300;
  PlanePrice = 600;
  FarmPrice = 1150;
  IndustryPrice = 1300;
  NukePrice = 2000;

  // surrender thresholds
  NORMAL_SURRENDER_THRESHOLD = 15;
  HARD_SURRENDER_THRESHOLD = 25;

  // AI settings
  AI_INVESTING_LEVEL = 2;
  AI_BUILD_NUKES_LEVEL = 3;
  AI_BUILD_INDS_FARMS_LEVEL = 2;

  // board size
  BOARD_SIZE = 10;
  NUM_SPACES = (BOARD_SIZE * BOARD_SIZE);
  //#define COLOUR_DARK fcComputer
  //#define COLOUR_LIGHT fcHuman

  // Hauptsächlich für die savegames, wenn sich da was ändert, sollte die Nummer geändert werden
  (*
   * Historie: 1 = Initialversion
   *           2 = +compdiff in TSuperdomSettings
   *               +Persistant_units in TSuperdomSettings
   *)
  SuperDomVersion: integer = 2;

Type

  TFieldColour = (fcHuman, fcComputer, fcUnused);
  TFieldObject = (foMen, foTank, foPlane, foFarm, foNuke, foIndustries);
  TGameState = (gsInvalid, gsBuyRessources, gsMoveUnits, gsWar);

  (* AI difficulty works like this:
   easy = 1:
    - no movement
    - no investing
    - will build factories if it has none
   medium = 2:
    - movement
    - investing
    - can build factories/farms if it has money
   hard = 3:
    - can buy/use nukes
    - investing
    - will hold out longer (surrender threshold 25)
   *)
  TCompDiff = (aiEasy, aiMedium, aiHard); // 1=easy 2=medium 3=hard

  TSubField = Record
    Colour: TFieldColour;
    Tank: Boolean;
    Plane: Boolean;
    Nuke: Boolean;
    Industries: Boolean;
    Farm: Boolean;
    Men: integer;
  End;

  TResources = Record
    cash: integer;
    food: integer;
    farms: integer;
    Industries: integer;
    men: integer;
    tanks: integer;
    planes: integer;
    nukes: integer;
    bank: integer;
    moves: integer;
  End;

  TSuperdomSettings = Record
    compstartfarms: integer;
    compstartinds: integer;
    humanstartfarms: integer;
    humanstartinds: integer;
    startcash: integer;
    startfood: integer;
    movesperturn: integer;
    compdiff: TCompDiff;
    spoil_enabled: Boolean; // Wenn True, dann "verfault" in jeder Runde 0 bis 10% des Essens
    persistent_units: Boolean; // Wenn True, dann überleben "Einheiten" eine Übernahme des Feldes
  End;

  (*
   * Das Eigentliche Spielfeld ist in 0..BOARD_SIZE -1, durch die "Verbreiterung" um 1 kann der Bound Check wegfallen.
   *)
  TField = Array[-1..BOARD_SIZE, -1..BOARD_SIZE] Of TSubField;

  TFieldInfo = Record
    HumanPower: integer;
    ComputerPower: integer;
  End;

  TUserMsgEvent = Procedure(Sender: TObject; Msg: String) Of Object;

  { tSuperDom }

  tSuperDom = Class
  private
    Fsuperdom_settings: TSuperdomSettings;
    FField: TField;
    FGameState: TGameState;
    FPlayer: Array[0..PlayerCount - 1] Of TResources;
    FCursor: Tpoint;
    Procedure CallUserEvent(); // Ruft die Callback zur Aktualisierung der Gui auf

    Function getmoves: boolean;
    Function getActive_Cell_Men_Count: integer;
    Function getActive_Cell_Tank: Boolean;
    Function getActive_Cell_Plane: Boolean;
    Function getActive_Cell_Nuke: Boolean;
    Function getInventory: String;

    (*
     * General gameplay
     *)
    Function killmen(colour: integer): integer;
    Procedure Gen_Resources();
    Function Average_Strength(colour: TFieldColour): integer;
    Function tile_has_item(object_type: TFieldObject; x, y: integer): boolean;

    (*
     * AI- Operations
     *)
    Procedure computer_allocate();
    Procedure computer_movement();
    Function computer_war(): Boolean;
    Function buy_resources(object_type: TFieldObject; x, y, nummen: integer): boolean; // AI- Buys ressources
    Function place_adjacent(tank: boolean; x, y: integer): Boolean;
    Function has_adjacent(x, y: integer): boolean;
    Function find_adjacent(x, y: integer): Tpoint;
    Function find_adjacent_target(x, y: integer; Out adj: Tpoint): Boolean;
    Procedure launch_nuke(colour: TFieldColour; nukex, nukey, targetx, targety: Integer);
  public
    UserUpdateEvent: TNotifyEvent;
    UserMsgEvent: TUserMsgEvent;
    Property Inventory: String read getInventory;
    Property Active_Cell_Men_Count: integer read getActive_Cell_Men_Count;
    Property Active_Cell_Tank: boolean read getActive_Cell_Tank;
    Property Active_Cell_Plane: boolean read getActive_Cell_Plane;
    Property Active_Cell_Nuke: boolean read getActive_Cell_Nuke;
    Property Moves: Boolean read getmoves;
    Property State: TGameState read FGameState;
    Property Settings: TSuperdomSettings read Fsuperdom_settings write Fsuperdom_settings;
    Constructor Create;
    Destructor destroy; override;
    Procedure Reset();
    Procedure RenderBattleField(Const Canvas: TCanvas; Const Rect: TRect);
    Function Calc_Strength(colour: TFieldColour; x, y: integer): integer; // Berechnet die Stärke des Spielers für das jeweilige Feld ( 10 <=> 1.0 )
    Procedure NewGame();
    Procedure SelectCell(x, y: integer);
    Function Update_Score(): TFieldInfo;
    Function PlayerCash: integer;
    Function PlayerBankCash: integer;
    Function MoveCash(money: integer): boolean; // + = Bar -> Bank, - = Bank -> Bar
    Function PlayerFood: integer;
    Function BuyMen(count: integer): boolean;
    Function BuyTank(): boolean;
    Function BuyPlane(): boolean;
    Function BuyFarm(): boolean;
    Function BuyIndustry(): boolean;
    Function BuyNuke(): boolean;
    Procedure FinishBuy();
    Procedure FinishMove();
    Function SpoilFood(): integer;
    Function FinishWar(): Boolean;
    Function CheckFinish(): String;
    Function MoveMen(x, y, count: integer): boolean;
    Function MoveTank(x, y: integer): boolean;
    Function MovePlane(x, y: integer): boolean;
    Function LaunchNuke(x, y: integer): String; // '' = OK, sonst fehlertext
    Function BuyMove(): integer;
    Function FeedMen(Colour: integer): integer;
    Function attack_territory(colour: TFieldColour; x, y: integer): Boolean;
    Function can_attack(): Boolean;
    Procedure SafeToFile(Filename: String);
    Procedure LoadFromFile(Filename: String);
    Function GetAvgStrength(): integer; // 0 = Beide Gleich Stark, <0 AI Stärker als Mensch, >0 Mensch Stärker als AI
  End;

Function AiToLevel(Value: TCompDiff): integer;

Implementation

Uses LazUTF8;

Function AiToLevel(Value: TCompDiff): integer;
Begin
  result := -1; // Not Known
  Case Value Of
    aiEasy: result := 1;
    aiMedium: result := 2;
    aiHard: result := 3;
  Else
    Raise Exception.Create('Error, "AiToLevel": missing implementation.');
  End;
End;

{ tSuperDom }

Procedure tSuperDom.CallUserEvent();
Begin
  If assigned(UserUpdateEvent) Then
    UserUpdateEvent(self);
End;

Procedure tSuperDom.Gen_Resources();
Var
  interest, inccash, incfood, ratecash, ratefood, i, j: integer;
Begin
  ratecash := 0;
  ratefood := 0;
  For i := 0 To PlayerCount - 1 Do Begin
    // Die Bank Zinsen Berechnen, für AI und Human unterschiedlich
    interest := 7 + random(6);
    inccash := 0;
    incfood := 0;
    FPlayer[i].bank := FPlayer[i].bank + round(interest * FPlayer[i].bank / 100);
    For j := 0 To FPlayer[i].Industries - 1 Do Begin
      inccash := inccash + 300 + random(200);
    End;
    For j := 0 To FPlayer[i].farms - 1 Do Begin
      incfood := incfood + 200 + random(200);
    End;
    If i = plHuman Then Begin
      If (FPlayer[i].Industries <> 0) Then
        ratecash := inccash Div FPlayer[i].Industries;
      If (FPlayer[i].farms <> 0) Then
        ratefood := incfood Div FPlayer[i].farms;
      If (ratecash > 450) Then Begin
        If (ratefood > 350) Then Begin
          ShowMessage('Patriotism sweeps the land, all production' + LineEnding +
            'is up this year!');
        End
        Else Begin
          ShowMessage('Factories working at maximum efficiency,' + LineEnding +
            'cash production up this year!');
        End;
      End
      Else If (ratecash > 350) Then Begin
        If (ratefood > 350) Then Begin
          ShowMessage('Record crop harvest this year!');
        End
        Else If (ratefood > 250) Then Begin
          ShowMessage('Production continues as normal');
        End
        Else Begin
          ShowMessage('Spoilage of crops leads to reduced farm' + LineEnding +
            'output this  year');
        End;
      End
      Else Begin
        If (ratefood > 350) Then Begin
          ShowMessage('Record crop harvest this year!');
        End
        Else If (ratefood > 250) Then Begin
          ShowMessage('Factory unions introduced. Industrial' + LineEnding +
            'production is down this year.');
        End
        Else Begin
          ShowMessage('Internet created. All production is down' + LineEnding +
            'due to time wasted.');
        End;
      End;
    End;
    FPlayer[i].cash := FPlayer[i].cash + inccash;
    FPlayer[i].food := FPlayer[i].food + incfood;
  End;
End;

Function tSuperDom.Average_Strength(colour: TFieldColour): integer;
Var
  i, j: integer;
  totalpower: integer;
Begin
  (* This function calculates the average strength of the given player,
   * used to determine when the computer wins or loses.
   *)
  totalpower := 0;
  For i := 0 To BOARD_SIZE - 1 Do Begin
    For j := 0 To BOARD_SIZE - 1 Do Begin
      If (FField[i, j].colour <> fcUnused) Then Begin
        totalpower := totalpower + calc_strength(colour, i, j);
      End;
    End;
  End;
  result := totalpower Div NUM_SPACES;
End;

Constructor tSuperDom.Create;
Var
  b: Tbitmap;
  s: String;
Begin
  Inherited create;
  UserUpdateEvent := Nil;
  FCursor := point(-1, -1);
  FPlayer[plHuman].cash := 0;
  FPlayer[plHuman].food := 0;
  Fsuperdom_settings.compstartfarms := 1;
  Fsuperdom_settings.compstartinds := 1;
  Fsuperdom_settings.humanstartfarms := 2;
  Fsuperdom_settings.humanstartinds := 2;
  Fsuperdom_settings.startcash := 0;
  Fsuperdom_settings.startfood := 0;
  Fsuperdom_settings.movesperturn := 2;
  Fsuperdom_settings.compdiff := aiMedium;
  Fsuperdom_settings.persistent_units := false;
  Fsuperdom_settings.spoil_enabled := false;
  FGameState := gsInvalid;
  s := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStrutf8(0))) + 'gfx' + PathDelim;
  b := TBitmap.Create;
  b.LoadFromFile(s + 'farm.bmp');
  SwapColor(b, clyellow, ColorHuman);
  BMP_GraphikEngine.LoadAlphaGraphik(b, 'HumanFarm');
  SwapColor(b, ColorHuman, ColorComputer);
  BMP_GraphikEngine.LoadAlphaGraphik(b, 'ComputerFarm');
  b.LoadFromFile(s + 'factory.bmp');
  SwapColor(b, clyellow, ColorHuman);
  BMP_GraphikEngine.LoadAlphaGraphik(b, 'HumanFactory');
  SwapColor(b, ColorHuman, ColorComputer);
  BMP_GraphikEngine.LoadAlphaGraphik(b, 'ComputerFactory');
  b.LoadFromFile(s + 'men.bmp');
  SwapColor(b, clyellow, ColorHuman);
  BMP_GraphikEngine.LoadAlphaGraphik(b, 'HumanMen');
  SwapColor(b, ColorHuman, ColorComputer);
  BMP_GraphikEngine.LoadAlphaGraphik(b, 'ComputerMen');
  b.LoadFromFile(s + 'tank.bmp');
  SwapColor(b, clyellow, ColorHuman);
  BMP_GraphikEngine.LoadAlphaGraphik(b, 'HumanTank');
  SwapColor(b, ColorHuman, ColorComputer);
  BMP_GraphikEngine.LoadAlphaGraphik(b, 'ComputerTank');
  b.LoadFromFile(s + 'nuke.bmp');
  SwapColor(b, clyellow, ColorHuman);
  BMP_GraphikEngine.LoadAlphaGraphik(b, 'HumanNuke');
  SwapColor(b, ColorHuman, ColorComputer);
  BMP_GraphikEngine.LoadAlphaGraphik(b, 'ComputerNuke');
  b.LoadFromFile(s + 'plane.bmp');
  SwapColor(b, clyellow, ColorHuman);
  BMP_GraphikEngine.LoadAlphaGraphik(b, 'HumanPlane');
  SwapColor(b, ColorHuman, ColorComputer);
  BMP_GraphikEngine.LoadAlphaGraphik(b, 'ComputerPlane');
  b.free;
  reset();
End;

Destructor tSuperDom.destroy;
Begin

End;

Procedure tSuperDom.Reset();
Var
  i, j: integer;
Begin
  For j := low(FField) To high(FField) Do
    For i := low(ffield[j]) To high(ffield[j]) Do Begin
      If (j = low(FField)) Or (j = high(FField)) Or
        (i = low(FField[j])) Or (i = high(FField[j])) Then Begin
        ffield[j, i].Colour := fcUnused;
      End
      Else Begin
        // In der Hoffnung, dass wir so eine 50 % Verteilung der Landfläche bekommen.
        If random(100) >= 50 Then Begin
          ffield[j, i].Colour := fcHuman;
        End
        Else Begin
          ffield[j, i].Colour := fcComputer;
        End;
      End;
      ffield[j, i].Farm := false;
      ffield[j, i].Industries := false;
      ffield[j, i].Men := 0;
      ffield[j, i].Nuke := false;
      ffield[j, i].Plane := false;
      ffield[j, i].Tank := false;
    End;
  FGameState := gsInvalid;
End;

Procedure tSuperDom.RenderBattleField(Const Canvas: TCanvas; Const Rect: TRect);
Var
  i, j: integer;
  r, h_2, w_3, w, h: Single;
  b: Tbitmap;
  s: Array[0..5] Of String;
Begin
  If FGameState = gsInvalid Then exit;
  w := abs(rect.Left - rect.Right) / 10;
  h := abs(rect.Top - Rect.Bottom) / 10;
  w_3 := w / 3;
  h_2 := h / 2;
  r := min(w_3, h_2);
  canvas.Pen.Color := ColorBackGrid;
  // Zeichnen der Einzelnen Felder + Deren Inhalt
  For i := 0 To BOARD_SIZE - 1 Do Begin
    For j := 0 To BOARD_SIZE - 1 Do Begin
      // Der Hintergrund = Besitzer Farbe
      If FField[i, j].Colour = fcHuman Then Begin
        canvas.Brush.Color := ColorBackHuman;
        s[0] := 'HumanFarm';
        s[1] := 'HumanFactory';
        s[2] := 'HumanMen';
        s[3] := 'HumanTank';
        s[4] := 'HumanNuke';
        s[5] := 'HumanPlane';
      End
      Else Begin
        canvas.Brush.Color := ColorBackComputer;
        s[0] := 'ComputerFarm';
        s[1] := 'ComputerFactory';
        s[2] := 'ComputerMen';
        s[3] := 'ComputerTank';
        s[4] := 'ComputerNuke';
        s[5] := 'ComputerPlane';
      End;
      canvas.Rectangle(
        rect.Left + round(i * w),
        rect.Top + round(j * h),
        rect.Left + round((i + 1) * w),
        rect.Top + round((j + 1) * h));
      If FField[i, j].Farm Then Begin
        b := BMP_GraphikEngine.Find(s[0]);
{$IFDEF LINUX}
        Stretchdraw(canvas,
{$ELSE}
        canvas.StretchDraw(
{$ENDIF}
          classes.rect(rect.Left + round(i * w + (w_3 - r) / 2),
          rect.top + round(j * h + (h_2 - r) / 2),
          rect.Left + round(i * w + (w_3 - r) / 2) + round(r),
          rect.top + round(j * h + (h_2 - r) / 2) + round(r)), b);
      End;
      If FField[i, j].Industries Then Begin
        b := BMP_GraphikEngine.Find(s[1]);
{$IFDEF LINUX}
        Stretchdraw(canvas,
{$ELSE}
        canvas.StretchDraw(
{$ENDIF}
          classes.rect(rect.Left + round(i * w + (w_3 - r) / 2),
          rect.top + round(j * h - (h_2 - r) / 2 + h_2),
          rect.Left + round(i * w + (w_3 - r) / 2) + round(r),
          rect.top + round(j * h - (h_2 - r) / 2 + h_2) + round(r)), b);
      End;
      If FField[i, j].Men <> 0 Then Begin
        b := BMP_GraphikEngine.Find(s[2]);
{$IFDEF LINUX}
        Stretchdraw(canvas,
{$ELSE}
        canvas.StretchDraw(
{$ENDIF}
          classes.rect(rect.Left + round(i * w + (w_3 - r) / 2 + w_3),
          rect.top + round(j * h + (h_2 - r) / 2),
          rect.Left + round(i * w + (w_3 - r) / 2 + w_3) + round(r),
          rect.top + round(j * h + (h_2 - r) / 2) + round(r)), b);
      End;
      If FField[i, j].Tank Then Begin
        b := BMP_GraphikEngine.Find(s[3]);
{$IFDEF LINUX}
        Stretchdraw(canvas,
{$ELSE}
        canvas.StretchDraw(
{$ENDIF}
          classes.rect(rect.Left + round(i * w + (w_3 - r) / 2 + w_3),
          rect.top + round(j * h - (h_2 - r) / 2 + h_2),
          rect.Left + round(i * w + (w_3 - r) / 2 + w_3) + round(r),
          rect.top + round(j * h - (h_2 - r) / 2 + h_2) + round(r)), b);
      End;
      If FField[i, j].Nuke Then Begin
        b := BMP_GraphikEngine.Find(s[4]);
{$IFDEF LINUX}
        Stretchdraw(canvas,
{$ELSE}
        canvas.StretchDraw(
{$ENDIF}
          classes.rect(rect.Left + round(i * w + (w_3 - r) / 2 + 2 * w_3),
          rect.top + round(j * h + (h_2 - r) / 2),
          rect.Left + round(i * w + (w_3 - r) / 2 + 2 * w_3) + round(r),
          rect.top + round(j * h + (h_2 - r) / 2) + round(r)), b);
      End;
      If FField[i, j].Plane Then Begin
        b := BMP_GraphikEngine.Find(s[5]);
{$IFDEF LINUX}
        Stretchdraw(canvas,
{$ELSE}
        canvas.StretchDraw(
{$ENDIF}
          classes.rect(rect.Left + round(i * w + (w_3 - r) / 2 + 2 * w_3),
          rect.top + round(j * h - (h_2 - r) / 2 + h_2),
          rect.Left + round(i * w + (w_3 - r) / 2 + 2 * w_3) + round(r),
          rect.top + round(j * h - (h_2 - r) / 2 + h_2) + round(r)), b);
      End;
    End;
  End;
  If FCursor.x <> -1 Then Begin
    Canvas.Pen.Color := clred;
    canvas.Brush.Style := bsClear;
    i := FCursor.x;
    j := FCursor.y;
    canvas.Rectangle(
      rect.Left + round(i * w),
      rect.Top + round(j * h),
      rect.Left + round((i + 1) * w),
      rect.Top + round((j + 1) * h));
  End;
End;

Function tSuperDom.Calc_Strength(colour: TFieldColour; x, y: integer): integer;
Var
  i, j, res: integer;
Begin
  res := 0;
  (*
   * Betrachtet werden die Umliegenden Felder, Diagonale Felder sind schwächer gewichtet
   *
   * Gewichtung     | Inhalt
   * ------------------------------
   *      0.133     | 1 Soldat
   *      1.0 / 0.5 | Besitzrecht
   *      2.0 / 1.0 | Nuklear Waffe
   *      3.0 / 1.5 | Panzer
   *      3.0 / 1.5 | Farm
   *      2.0       | Flugzeug
   *      2.0       | Industrie
   *)
  For i := -1 To 1 Do
    For j := -1 To 1 Do
      If (FField[x + i, y + j].Colour = colour) Then Begin
        If ((i = 0) Or (j = 0)) Then Begin
          // Waagrecht, Senkrecht
          res := res + 10; // Jedes Feld das uns gehört gewichtet
          If (FField[x + i, y + j].tank) Then
            res := res + 30;
          If (FField[x + i, y + j].Farm) Then
            res := res + 30;
          If (FField[x + i, y + j].Plane) Then
            res := res + 20;
          If (FField[x + i, y + j].Industries) Then
            res := res + 20;
          If (FField[x + i, y + j].Nuke) Then
            res := res + 20;
          If (FField[x + i, y + j].Men <> 0) Then Begin
            res := res + round(FField[x + i, y + j].Men * 133 / 1000);
          End;
        End
        Else Begin
          // Diagonal
          res := res + 5; // Jedes Feld das uns gehört gewichtet
          If (FField[x + i, y + j].tank) Then
            res := res + 15;
          If (FField[x + i, y + j].Farm) Then
            res := res + 15;
          If (FField[x + i, y + j].Plane) Then
            res := res + 20;
          If (FField[x + i, y + j].Industries) Then
            res := res + 20;
          If (FField[x + i, y + j].Nuke) Then
            res := res + 10;
          If (FField[x + i, y + j].Men <> 0) Then Begin
            res := res + round(FField[x + i, y + j].Men * 133 / 1000);
          End;
        End;
      End;
  result := res;
End;

Procedure tSuperDom.NewGame();
Var
  i, j: integer;
Begin
  // Feld Löschen + Besitzer zuweisen
  reset();
  // Farmen und Gedöns Einfügen
  FPlayer[plHuman].cash := Fsuperdom_settings.startcash;
  FPlayer[plHuman].food := Fsuperdom_settings.startfood;
  FPlayer[plHuman].tanks := 0;
  FPlayer[plHuman].planes := 0;
  FPlayer[plHuman].nukes := 0;
  FPlayer[plHuman].Industries := 0;
  FPlayer[plHuman].farms := 0;
  FPlayer[plHuman].men := 0;
  FPlayer[plHuman].bank := 0;
  FPlayer[plHuman].moves := 0;
  FPlayer[plComputer].cash := Fsuperdom_settings.startcash;
  FPlayer[plComputer].food := Fsuperdom_settings.startfood;
  FPlayer[plComputer].tanks := 0;
  FPlayer[plComputer].planes := 0;
  FPlayer[plComputer].nukes := 0;
  FPlayer[plComputer].Industries := 0;
  FPlayer[plComputer].farms := 0;
  FPlayer[plComputer].men := 0;
  FPlayer[plComputer].bank := 0;
  FPlayer[plComputer].moves := 0;
  While (FPlayer[plComputer].farms < fsuperdom_settings.compstartfarms) Do Begin
    i := random(BOARD_SIZE);
    j := random(BOARD_SIZE);
    If ((FField[i, j].colour = fcComputer) And (FField[i, j].farm = false)) Then Begin
      FField[i][j].farm := true;
      FPlayer[plComputer].farms := FPlayer[plComputer].farms + 1;
    End;
  End;
  While (FPlayer[plComputer].Industries < fsuperdom_settings.compstartinds) Do Begin
    i := random(BOARD_SIZE);
    j := random(BOARD_SIZE);
    If ((FField[i, j].colour = fcComputer) And (FField[i, j].Industries = false)) Then Begin
      FField[i, j].Industries := true;
      FPlayer[plComputer].Industries := FPlayer[plComputer].Industries + 1;
    End;
  End;
  While (FPlayer[plHuman].farms < fsuperdom_settings.humanstartfarms) Do Begin
    i := random(BOARD_SIZE);
    j := random(BOARD_SIZE);
    If ((FField[i, j].colour = fcHuman) And (FField[i, j].farm = false)) Then Begin
      FField[i, j].farm := true;
      FPlayer[plHuman].farms := FPlayer[plHuman].farms + 1;
    End;
  End;
  While (FPlayer[plHuman].Industries < fsuperdom_settings.humanstartinds) Do Begin
    i := random(BOARD_SIZE);
    j := random(BOARD_SIZE);
    If ((FField[i, j].colour = fcHuman) And (FField[i, j].Industries = false)) Then Begin
      FField[i, j].Industries := true;
      FPlayer[plHuman].Industries := FPlayer[plHuman].Industries + 1;
    End;
  End;
  Gen_Resources();
  FGameState := gsBuyRessources;
End;

Procedure tSuperDom.SelectCell(x, y: integer);
Begin
  If (x >= 0) And (x < BOARD_SIZE) And (y >= 0) And (y < BOARD_SIZE) Then Begin
    //    FOldCursor := FCursor;
    FCursor := point(x, y);
  End;
End;

Function tSuperDom.Update_Score(): TFieldInfo;
Begin
  If FCursor.x <> -1 Then Begin
    result.ComputerPower := Calc_Strength(fcComputer, FCursor.x, FCursor.y);
    result.HumanPower := Calc_Strength(fcHuman, FCursor.x, FCursor.y);
  End
  Else Begin
    result.ComputerPower := 0;
    result.HumanPower := 0;
  End;
End;

Function tSuperDom.PlayerCash: integer;
Begin
  result := FPlayer[plHuman].cash;
End;

Function tSuperDom.PlayerBankCash: integer;
Begin
  result := FPlayer[plHuman].bank;
End;

Function tSuperDom.MoveCash(money: integer): boolean;
Begin
  result := false;
  If money > 0 Then Begin
    money := min(money, FPlayer[plHuman].cash);
    FPlayer[plHuman].cash := FPlayer[plHuman].cash - money;
    FPlayer[plHuman].bank := FPlayer[plHuman].bank + money;
  End;
  If money < 0 Then Begin
    money := min(-money, FPlayer[plHuman].bank);
    FPlayer[plHuman].bank := FPlayer[plHuman].bank - money;
    FPlayer[plHuman].cash := FPlayer[plHuman].cash + money;
  End;
End;

Function tSuperDom.PlayerFood: integer;
Begin
  result := FPlayer[plHuman].food;
End;

Function tSuperDom.BuyMen(count: integer): boolean;
Begin
  result := FField[FCursor.x, FCursor.y].Colour = fcHuman;
  If (count > 0) And result Then Begin
    count := min(count, FPlayer[plHuman].cash);
    FPlayer[plHuman].cash := FPlayer[plHuman].cash - count;
    FField[FCursor.x, FCursor.y].Men := FField[FCursor.x, FCursor.y].Men + count;
    FPlayer[plHuman].men := FPlayer[plHuman].men + count;
  End;
End;

Function tSuperDom.BuyTank(): boolean;
Begin
  result := FField[FCursor.x, FCursor.y].Colour = fcHuman;
  If result Then Begin
    result := Not FField[FCursor.x, FCursor.y].Tank;
  End;
  If result Then Begin
    If FPlayer[plHuman].cash >= TankPrice Then Begin
      FPlayer[plHuman].cash := FPlayer[plHuman].cash - TankPrice;
      FField[FCursor.x, FCursor.y].Tank := true;
      FPlayer[plHuman].tanks := FPlayer[plHuman].tanks + 1;
    End;
  End;
End;

Function tSuperDom.BuyPlane(): boolean;
Begin
  result := FField[FCursor.x, FCursor.y].Colour = fcHuman;
  If result Then Begin
    result := Not FField[FCursor.x, FCursor.y].Plane;
  End;
  If result Then Begin
    If FPlayer[plHuman].cash >= PlanePrice Then Begin
      FPlayer[plHuman].cash := FPlayer[plHuman].cash - PlanePrice;
      FField[FCursor.x, FCursor.y].Plane := true;
      FPlayer[plHuman].planes := FPlayer[plHuman].planes + 1;
    End;
  End;
End;

Function tSuperDom.BuyFarm(): boolean;
Begin
  result := FField[FCursor.x, FCursor.y].Colour = fcHuman;
  If result Then Begin
    result := Not FField[FCursor.x, FCursor.y].Farm;
  End;
  If result Then Begin
    If FPlayer[plHuman].cash >= FarmPrice Then Begin
      FPlayer[plHuman].cash := FPlayer[plHuman].cash - FarmPrice;
      FField[FCursor.x, FCursor.y].Farm := true;
      FPlayer[plHuman].farms := FPlayer[plHuman].farms + 1;
    End;
  End;
End;

Function tSuperDom.BuyIndustry(): boolean;
Begin
  result := FField[FCursor.x, FCursor.y].Colour = fcHuman;
  If result Then Begin
    result := Not FField[FCursor.x, FCursor.y].Industries;
  End;
  If result Then Begin
    If FPlayer[plHuman].cash >= IndustryPrice Then Begin
      FPlayer[plHuman].cash := FPlayer[plHuman].cash - IndustryPrice;
      FField[FCursor.x, FCursor.y].Industries := true;
      FPlayer[plHuman].Industries := FPlayer[plHuman].Industries + 1;
    End;
  End;
End;

Function tSuperDom.BuyNuke(): boolean;
Begin
  result := FField[FCursor.x, FCursor.y].Colour = fcHuman;
  If result Then Begin
    result := Not FField[FCursor.x, FCursor.y].Nuke;
  End;
  If result Then Begin
    If FPlayer[plHuman].cash >= NukePrice Then Begin
      FPlayer[plHuman].cash := FPlayer[plHuman].cash - NukePrice;
      FField[FCursor.x, FCursor.y].Nuke := true;
      FPlayer[plHuman].nukes := FPlayer[plHuman].nukes + 1;
    End;
  End;
End;

Procedure tSuperDom.FinishBuy();
Begin
  // die KI muss kaufen, dann umschalten auf "Moving"
  computer_allocate();
  // next is movement
  FPlayer[plHuman].moves := Fsuperdom_settings.movesperturn;
  FPlayer[plComputer].moves := Fsuperdom_settings.movesperturn;
  FGameState := gsMoveUnits;
End;

Procedure tSuperDom.FinishMove();
Begin
  computer_movement();
  FPlayer[plHuman].moves := Fsuperdom_settings.movesperturn;
  FPlayer[plComputer].moves := Fsuperdom_settings.movesperturn;
  // next is war
  FGameState := gsWar;
End;

Function tSuperDom.SpoilFood(): integer;
Var
  spoil_amount: integer;
Begin
  result := 0;
  If Not Fsuperdom_settings.spoil_enabled Then exit;
  // spoil 0-10% of food, different amounts for computer/player
  result := trunc(FPlayer[plHuman].food * Random * 0.1);
  FPlayer[plHuman].food := FPlayer[plHuman].food - result;

  // now for computer
  spoil_amount := trunc(FPlayer[plComputer].food * Random * 0.1);
  FPlayer[plHuman].food := FPlayer[plHuman].food - spoil_amount;
End;

Function tSuperDom.FinishWar(): Boolean;
Begin
  result := Computer_War();
  // Einleiten der Neuen Runde
  Gen_Resources();
End;

Function tSuperDom.GetAvgStrength(): integer;
Begin
  result := (average_strength(fcHuman) - average_strength(fcComputer));
End;

Function tSuperDom.CheckFinish(): String;
Var
  avg_str_diff: integer;
Begin
  result := '';
  FGameState := gsBuyRessources;
  avg_str_diff := (average_strength(fcHuman) - average_strength(fcComputer));
  If Fsuperdom_settings.compdiff = aiHard Then Begin
    If (avg_str_diff > HARD_SURRENDER_THRESHOLD) Then Begin
      result := 'The computer has surrendered. You win.';
      FGameState := gsInvalid;
    End;
    If (-avg_str_diff > HARD_SURRENDER_THRESHOLD) Then Begin
      result := 'Your army have suffered terrible morale from' + LineEnding +
        'the bleak prospects of winning. You lose.';
      FGameState := gsInvalid;
    End;
  End
  Else Begin
    If (avg_str_diff > NORMAL_SURRENDER_THRESHOLD) Then Begin
      result := 'The computer has surrendered. You win.';
      FGameState := gsInvalid;
    End;
    If (-avg_str_diff > NORMAL_SURRENDER_THRESHOLD) Then Begin
      result := 'Your army have suffered terrible morale from' + LineEnding +
        'the bleak prospects of winning. You lose.';
      FGameState := gsInvalid;
    End;
  End;
End;

Function tSuperDom.MoveMen(x, y, count: integer): boolean;
Begin
  If count < 0 Then Begin
    result := true;
    exit;
  End;
  result := false;
  If (x >= 0) And (x < BOARD_SIZE) And (y >= 0) And (y < BOARD_SIZE) And (FPlayer[plHuman].moves > 0) Then Begin
    If FField[x, y].Colour = fcHuman Then Begin
      If (abs(x - FCursor.X) <= 1) And (abs(y - FCursor.y) <= 1) And
        Not ((x = FCursor.X) And (y = FCursor.y)) Then Begin
        count := min(count, FField[FCursor.X, FCursor.y].Men);
        FField[FCursor.X, FCursor.y].Men := FField[FCursor.X, FCursor.y].Men - count;
        FField[x, y].Men := FField[x, y].Men + count;
        FPlayer[plHuman].moves := FPlayer[plHuman].moves - 1;
        result := true;
      End;
    End;
  End;
End;

Function tSuperDom.MoveTank(x, y: integer): boolean;
Begin
  result := false;
  If (x >= 0) And (x < BOARD_SIZE) And (y >= 0) And (y < BOARD_SIZE) And (FPlayer[plHuman].moves > 0) Then Begin
    If FField[x, y].Colour = fcHuman Then Begin
      If (abs(x - FCursor.X) <= 1) And (abs(y - FCursor.y) <= 1) And
        Not ((x = FCursor.X) And (y = FCursor.y)) And (Not FField[x, y].Tank) Then Begin
        FField[FCursor.X, FCursor.y].Tank := false;
        FField[x, y].Tank := true;
        FPlayer[plHuman].moves := FPlayer[plHuman].moves - 1;
        result := true;
      End;
    End;
  End;
End;

Function tSuperDom.MovePlane(x, y: integer): boolean;
Begin
  result := false;
  If (x >= 0) And (x < BOARD_SIZE) And (y >= 0) And (y < BOARD_SIZE) And (FPlayer[plHuman].moves > 0) Then Begin
    If (FField[x, y].Colour = fcHuman) And (Not FField[x, y].Plane) Then Begin
      FField[FCursor.X, FCursor.y].Plane := false;
      FField[x, y].Plane := true;
      FPlayer[plHuman].moves := FPlayer[plHuman].moves - 1;
      result := true;
    End;
  End;
End;

Procedure tSuperDom.launch_nuke(colour: TFieldColour; nukex, nukey, targetx, targety: Integer);
Var
  Human: Boolean;
  res: integer;
Begin
  human := (colour = fcHuman);
  //    int temp;
  //    struct resources *res;
  //
  //    if(board[nukex][nukey].colour != colour)
  //    {
  //        if(human)
  //            rb->splash(HZ, "That isn't your territory");
  //        return RET_VAL_QUIT_ERR;
  //    }
  //    if(! board[nukex][nukey].nuke)
  //    {
  //        if(human)
  //            rb->splashf(HZ, "You don't have a nuke there");
  //        return RET_VAL_QUIT_ERR;
  //    }
  //    if(human)
  //    {
  //        rb->splash(HZ, "Select place to target with nuke");
  //        if((temp = select_square()) != RET_VAL_OK)
  //            return temp;
  //        targetx = cursor.x;
  //        targety = cursor.y;
  //    }
  If (human) Then Begin
    FPlayer[plHuman].nukes := FPlayer[plHuman].nukes - 1;
  End
  Else Begin
    FPlayer[plComputer].nukes := FPlayer[plHuman].nukes - 1;
  End;
  FField[nukex, nukey].Nuke := false;

  //    if(board[targetx][targety].colour == COLOUR_LIGHT)
  If (FField[targetx, targety].Colour = fcHuman) Then Begin
    res := plHuman;
  End
  Else Begin
    res := plComputer;
  End;
  FPlayer[res].men := FPlayer[res].men - FField[targetx, targety].Men;
  If FField[targetx, targety].Tank Then FPlayer[res].tanks := FPlayer[res].tanks - 1;
  If FField[targetx, targety].Plane Then FPlayer[res].planes := FPlayer[res].planes - 1;
  If FField[targetx, targety].Nuke Then FPlayer[res].nukes := FPlayer[res].nukes - 1;
  If FField[targetx, targety].Farm Then FPlayer[res].farms := FPlayer[res].farms - 1;
  If FField[targetx, targety].Industries Then FPlayer[res].Industries := FPlayer[res].Industries - 1;
  FField[targetx, targety].Men := 0;
  FField[targetx, targety].Tank := false;
  FField[targetx, targety].Plane := false;
  FField[targetx, targety].Nuke := false;
  FField[targetx, targety].Farm := false;
  FField[targetx, targety].Industries := false;
  // TODO: Fallout carried by wind
  UserUpdateEvent(Nil);
End;


Function tSuperDom.LaunchNuke(x, y: integer): String;
Begin
  // TODO: Muss noch überarbeitet werden -> Merge mit Launch_Nuke
    // Orig Zeile 1188, ist ein wenig hart kodiert, da die AI eh keine Nukes kauft oder schießt
  result := 'Invalid move';
  If (x >= 0) And (x < BOARD_SIZE) And (y >= 0) And (y < BOARD_SIZE) And (FPlayer[plHuman].moves > 0) Then Begin
    If FField[x, y].Colour = fcComputer Then Begin
      FField[FCursor.X, FCursor.y].Nuke := false;
      FPlayer[plHuman].nukes := FPlayer[plHuman].nukes - 1;
      FPlayer[plComputer].men := FPlayer[plComputer].men - FField[X, y].Men;
      If FField[X, y].Tank Then
        FPlayer[plComputer].tanks := FPlayer[plComputer].tanks - 1;
      If FField[X, y].Plane Then
        FPlayer[plComputer].planes := FPlayer[plComputer].planes - 1;
      If FField[X, y].Nuke Then
        FPlayer[plComputer].nukes := FPlayer[plComputer].nukes - 1;
      If FField[X, y].Farm Then
        FPlayer[plComputer].farms := FPlayer[plComputer].farms - 1;
      If FField[X, y].Industries Then
        FPlayer[plComputer].Industries := FPlayer[plComputer].Industries - 1;
      FField[X, y].Men := 0;
      FField[X, y].Tank := false;
      FField[X, y].Plane := false;
      FField[X, y].Nuke := false;
      FField[X, y].Farm := false;
      FField[X, y].Industries := false;
      FPlayer[plHuman].moves := FPlayer[plHuman].moves - 1;
      (* TODO: Fallout carried by wind *)
      result := '';
    End
    Else Begin
      result := 'That isn''t your territory';
    End;
  End;
End;

Function tSuperDom.BuyMove(): integer;
Begin
  If FPlayer[plHuman].cash >= MovePrice Then Begin
    FPlayer[plHuman].cash := FPlayer[plHuman].cash - MovePrice;
    FPlayer[plHuman].moves := FPlayer[plHuman].moves + 1;
  End;
  result := FPlayer[plHuman].moves;
End;

Function tSuperDom.FeedMen(Colour: integer): integer;
Begin
  result := 0;
  If (Colour = plHuman) Or (Colour = plComputer) Then Begin
    If FPlayer[Colour].food >= FPlayer[Colour].men Then Begin
      FPlayer[Colour].food := FPlayer[Colour].food - FPlayer[Colour].men;
      result := FPlayer[Colour].men;
    End
    Else Begin
      result := -killmen(Colour);
      FPlayer[Colour].food := 0;
      FPlayer[Colour].men := FPlayer[Colour].men + result;
    End;
  End;
End;

Function tSuperDom.attack_territory(colour: TFieldColour; x, y: integer): Boolean;
Var
  str_diff: integer;
  offres, defres: integer;
Begin
  result := false;
  If colour = fcHuman Then Begin
    x := FCursor.X;
    y := FCursor.Y;
    offres := plHuman;
    defres := plComputer;
  End
  Else Begin
    offres := plComputer;
    defres := plHuman;
  End;
  FPlayer[offres].moves := FPlayer[offres].moves - 1; // mit Zählen der Verfügbaren "Moves"
  str_diff := calc_strength(fcComputer, x, y) - calc_strength(fcHuman, x, y);
  If colour = fcHuman Then Begin
    str_diff := -str_diff;
  End;
  If (str_diff > 0) Or ((str_diff = 0) And (random(100) >= 50)) Then Begin
    FPlayer[defres].men := FPlayer[defres].men - FField[x, y].Men;
    If FField[x, y].Tank Then Begin
      FPlayer[defres].tanks := FPlayer[defres].tanks - 1;
    End;
    If FField[x, y].Plane Then Begin
      FPlayer[defres].planes := FPlayer[defres].planes - 1;
    End;
    If FField[x, y].Nuke Then Begin
      FPlayer[defres].nukes := FPlayer[defres].nukes - 1;
      FPlayer[offres].nukes := FPlayer[offres].nukes + 1;
    End;
    If FField[x, y].Industries Then Begin
      FPlayer[defres].Industries := FPlayer[defres].Industries - 1;
      FPlayer[offres].Industries := FPlayer[offres].Industries + 1;
    End;
    If FField[x, y].Farm Then Begin
      FPlayer[defres].farms := FPlayer[defres].farms - 1;
      FPlayer[offres].farms := FPlayer[offres].farms + 1;
    End;
    FField[x, y].Colour := colour;
    If Fsuperdom_settings.persistent_units Then Begin
      FPlayer[offres].men := FPlayer[offres].men + FField[x, y].Men;
      If FField[x, y].Tank Then Begin
        FPlayer[offres].tanks := FPlayer[offres].tanks + 1;
      End;
      If FField[x, y].Plane Then Begin
        FPlayer[offres].planes := FPlayer[offres].planes + 1;
      End;
    End
    Else Begin
      FField[x, y].Men := 0;
      FField[x, y].Tank := false;
      FField[x, y].Plane := false;
    End;
    result := true;
  End;
End;

Function tSuperDom.can_attack(): Boolean;
Begin
  result := (FCursor.X <> -1) And (FField[FCursor.X, FCursor.Y].Colour = fcComputer);
End;

Procedure tSuperDom.SafeToFile(Filename: String);
Var
  f: TFilestream;
  i: integer;
Begin
  f := TFileStream.Create(filename, fmcreate Or fmopenwrite);
  i := SuperDomVersion;
  f.Write(i, sizeof(integer));
  f.Write(FGameState, SizeOf(FGameState));
  f.write(FPlayer[0], sizeof(FPlayer[0]));
  f.write(FPlayer[1], sizeof(FPlayer[1]));
  f.write(FField, sizeof(FField));
  f.write(Fsuperdom_settings, sizeof(Fsuperdom_settings));
  // Weiter
  f.free;
End;

Procedure tSuperDom.LoadFromFile(Filename: String);
Var
  f: TFilestream;
  i: integer;
Begin
  f := TFileStream.Create(filename, fmopenread);
  i := 0;
  f.read(i, sizeof(integer));
  If (i = SuperDomVersion) Then Begin
    f.read(FGameState, SizeOf(FGameState));
    f.read(FPlayer[0], sizeof(FPlayer[0]));
    f.read(FPlayer[1], sizeof(FPlayer[1]));
    f.read(FField, sizeof(FField));
    f.read(Fsuperdom_settings, sizeof(Fsuperdom_settings));
    // Weiter
  End
  Else Begin
    Raise Exception.Create('Error invalid superdom savegame.');
  End;
  f.free;
End;

Function tSuperDom.killmen(colour: integer): integer;
Var
  percent, nummen, menkilled, i, j: integer;
  fc: TFieldColour;
Begin
  fc := fcUnused;
  If colour = plHuman Then
    fc := fcHuman;
  If colour = plComputer Then
    fc := fcComputer;
  percent := trunc(FPlayer[colour].food * 1000 / FPlayer[colour].men);
  menkilled := 0;
  For i := 0 To BOARD_SIZE - 1 Do Begin
    For j := 0 To BOARD_SIZE - 1 Do Begin
      If FField[i, j].Colour = fc Then Begin
        nummen := trunc((FField[i, j].men * percent) / 1000);
        menkilled := menkilled + FField[i, j].men - nummen;
        FField[i, j].men := nummen;
      End;
    End;
  End;
  result := menkilled;
End;

Function tSuperDom.computer_war(): Boolean;
Var
  i, j: integer;
  found_target: Boolean;
  adj: Tpoint;
Begin
  result := false;
  // Work out where to attack - prioritise the defence of buildings and nukes
  found_target := true;
  While (found_target) Do Begin
    found_target := false;
    For i := 0 To BOARD_SIZE - 1 Do Begin
      For j := 0 To BOARD_SIZE - 1 Do Begin
        If (FField[i, j].Colour = fcComputer) And
          (FField[i, j].Farm Or FField[i, j].Industries Or FField[i, j].Nuke) And find_adjacent_target(i, j, adj) Then Begin
          found_target := true;
          result := attack_territory(fcComputer, adj.x, adj.Y) Or result; // ACHTUNG bei Aktivierter Short Circuit Evaluation ( Default ), muss die Reihenfolge diese sein !!
          CallUserEvent();
          If FPlayer[plComputer].moves <= 0 Then Begin
            exit;
          End;
        End;
      End;
    End;
  End;
  // Defence stage done, move on to OFFENCE
  found_target := true;
  While (found_target) Do Begin
    found_target := false;
    For i := 0 To BOARD_SIZE - 1 Do Begin
      For j := 0 To BOARD_SIZE - 1 Do Begin
        If (FField[i, j].Colour = fcHuman) And
          (FField[i, j].Industries Or (FField[i, j].Farm) Or (FField[i, j].Nuke)) And
          (Calc_Strength(fcComputer, i, j) >= Calc_Strength(fcHuman, i, j)) Then Begin
          found_target := true;
          result := attack_territory(fcComputer, i, j) Or Result; // ACHTUNG bei Aktivierter Short Circuit Evaluation ( Default ), muss die Reihenfolge diese sein !!
          CallUserEvent();
          If FPlayer[plComputer].moves <= 0 Then Begin
            exit;
          End;
        End;
      End;
    End;
  End;
  // Spend leftover moves wherever attacking randomly
  found_target := true;
  While (found_target) Do Begin
    found_target := false;
    For i := 0 To BOARD_SIZE - 1 Do Begin
      For j := 0 To BOARD_SIZE - 1 Do Begin
        If (FField[i, j].Colour = fcHuman) And
          (Calc_Strength(fcComputer, i, j) >= Calc_Strength(fcHuman, i, j)) Then Begin
          found_target := true;
          result := attack_territory(fcComputer, i, j) Or result; // ACHTUNG bei Aktivierter Short Circuit Evaluation ( Default ), muss die Reihenfolge diese sein !!
          CallUserEvent();
          If FPlayer[plComputer].moves <= 0 Then Begin
            exit;
          End;
        End;
      End;
    End;
  End;
End;

Function tSuperDom.find_adjacent_target(x, y: integer; Out adj: Tpoint
  ): Boolean;
Begin
  (* Find a square next to a computer's farm or factory owned by the player
   * that is vulnerable. Return true on success, false otherwise *)
  If (FField[x + 1, y].Colour = fcHuman) And
    (Calc_Strength(fcHuman, x + 1, y) <= Calc_Strength(fcComputer, x + 1, y)) Then Begin
    adj.X := x + 1;
    adj.y := y;
    result := true;
    exit;
  End;
  If (FField[x - 1, y].Colour = fcHuman) And
    (Calc_Strength(fcHuman, x - 1, y) <= Calc_Strength(fcComputer, x - 1, y)) Then Begin
    adj.X := x - 1;
    adj.y := y;
    result := true;
    exit;
  End;
  If (FField[x, y - 1].Colour = fcHuman) And
    (Calc_Strength(fcHuman, x, y - 1) <= Calc_Strength(fcComputer, x, y - 1)) Then Begin
    adj.X := x;
    adj.y := y - 1;
    result := true;
    exit;
  End;
  If (FField[x, y + 1].Colour = fcHuman) And
    (Calc_Strength(fcHuman, x, y + 1) <= Calc_Strength(fcComputer, x, y + 1)) Then Begin
    adj.X := x;
    adj.y := y + 1;
    result := true;
    exit;
  End;
  result := false;
End;

Function tSuperDom.getInventory: String;
Begin
  result := format('Men : %d' + LineEnding +
    'Tanks : %d' + LineEnding +
    'Planes : %d' + LineEnding +
    'Factories : %d' + LineEnding +
    'Farms : %d' + LineEnding +
    'Nukes : %d' + LineEnding +
    'Cash : %d' + LineEnding +
    'Food : %d' + LineEnding +
    'Bank : %d' + LineEnding,
    [
    FPlayer[plHuman].men,
      FPlayer[plHuman].tanks,
      FPlayer[plHuman].planes,
      FPlayer[plHuman].Industries,
      FPlayer[plHuman].farms,
      FPlayer[plHuman].nukes,
      FPlayer[plHuman].cash,
      FPlayer[plHuman].food,
      FPlayer[plHuman].bank
      ]);
End;

Function tSuperDom.has_adjacent(x, y: integer): boolean;
Begin
  If ((FField[x, y].colour = fcHuman) And
    ((FField[x - 1, y].colour = fcComputer) Or
    (FField[x + 1, y].colour = fcComputer) Or
    (FField[x, y + 1].colour = fcComputer) Or
    (FField[x, y - 1].colour = fcComputer))) Then Begin
    result := true;
  End
  Else Begin
    result := false;
  End;
End;

Procedure tSuperDom.computer_allocate();
Type
  Tthreat = Record
    x: integer;
    y: integer;
    str_diff: integer;
  End;
Var
  cnt, i, j, k: integer;
  offensive: Boolean;
  threats: Array[0..3] Of Tthreat;
  targets: Array[0..1] Of Tthreat;
  numtargets, numthreats, total_str_diff, numterritory, str_diff, men_needed:
  integer;
  adj: TPoint;
  tank: boolean;
Begin
  (* Firstly, decide whether to go offensive or defensive.
   * This is primarily decided by the human player posing a threat to either
   * the computer's farms, factories or nukes *)
  offensive := true;
  numthreats := 0;
  total_str_diff := 0;
  numterritory := 0;
  // Alles Geld von der Bank abheben
  FPlayer[plComputer].cash := FPlayer[plComputer].cash + FPlayer[plComputer].bank;
  FPlayer[plComputer].bank := 0;
  // Ermitteln ob offensiv, oder defensive
  For i := 0 To BOARD_SIZE - 1 Do Begin
    For j := 0 To BOARD_SIZE - 1 Do Begin
      If (FField[i, j].Colour = fcComputer) Then Begin
        inc(numterritory);
        str_diff := calc_strength(fcHuman, i, j) - calc_strength(fcComputer, i, j);
        If (str_diff > 0) And ((FField[i, j].Industries Or FField[i, j].farm Or FField[i, j].Nuke)) Then Begin
          If (numthreats < 4) Then Begin
            offensive := false;
            threats[numthreats].x := i;
            threats[numthreats].y := j;
            threats[numthreats].str_diff := str_diff;
            inc(numthreats);
          End;
        End;
      End;
    End;
  End;
  // if the computer has no factories, build some ASAP
  If (FPlayer[plComputer].Industries = 0) Then Begin
    While ((FPlayer[plComputer].cash >= IndustryPrice) And (FPlayer[plComputer].Industries < numterritory)) Do Begin
      i := Random(BOARD_SIZE);
      j := Random(BOARD_SIZE);
      If (FField[i][j].colour = fcComputer) Then Begin
        buy_resources(foIndustries, i, j, 0);
      End;
    End;
  End;
  If ((AiToLevel(fsuperdom_settings.compdiff) >= AI_BUILD_INDS_FARMS_LEVEL) And (FPlayer[plComputer].cash >= FarmPrice + 100)) Then Begin
    cnt := 0;
    Repeat
      If (FPlayer[plComputer].farms < FPlayer[plComputer].Industries) Then Begin
        i := random(BOARD_SIZE);
        j := Random(BOARD_SIZE);
        If (FField[i, j].Colour = fcComputer) And Not (FField[i, j].Farm) Then Begin
          buy_resources(foFarm, i, j, 0);
          break;
        End;
      End
      Else Begin
        i := random(BOARD_SIZE);
        j := Random(BOARD_SIZE);
        If (FField[i, j].Colour = fcComputer) And Not (FField[i, j].Industries) Then Begin
          buy_resources(foIndustries, i, j, 0);
          break;
        End;
      End;
      inc(cnt);
    Until ((FPlayer[plComputer].cash < IndustryPrice + 100) Or (cnt >= 3));
  End;
  // AI will buy nukes first if possible
  If (FPlayer[plComputer].cash > NukePrice + TankPrice) And (AiToLevel(fsuperdom_settings.compdiff) >= AI_BUILD_NUKES_LEVEL) Then Begin
    While ((FPlayer[plComputer].cash >= NukePrice) And (FPlayer[plComputer].nukes < numterritory)) Do Begin
      i := random(BOARD_SIZE);
      j := Random(BOARD_SIZE);
      If (FField[i, j].Colour = fcComputer) And Not (FField[i, j].Nuke) Then Begin
        buy_resources(foNuke, i, j, 0);
      End;
    End;
  End;
  If (offensive) Then Begin
    (* The AI is going to go straight for the throat here and attack
     * the player's farms, nukes and factories. The amount of cash
     * the AI has to spend will determine how many targets there are *)
    If (FPlayer[plComputer].cash > 1200) Then Begin // 1200 is a figure I pulled out of nowhere. Adjust as needed
      numtargets := 2;
    End
    Else Begin
      numtargets := 1;
    End;
    (* Work out which target(s) to attack. They must have adjacent squares
     * owned by the computer. If none are found just place troops in
     * random places around the map until we run out of money *)
    k := 0;
    For i := 0 To BOARD_SIZE - 1 Do Begin
      For j := 0 To BOARD_SIZE - 1 Do Begin
        If (has_adjacent(i, j) And
          (FField[i, j].Industries Or FField[i, j].farm Or FField[i, j].Nuke)) Then Begin
          If (k < numtargets) Then Begin
            targets[k].x := i;
            targets[k].y := j;
            targets[k].str_diff := calc_strength(fcHuman, i, j) - calc_strength(fcComputer, i, j);
            inc(k);
          End;
        End;
      End;
    End;
    If (k = 0) Then Begin
      (* No targets found! Randomly pick squares and if they're owned
       * by the computer then stick a tank on it.
       *)
      While (FPlayer[plComputer].cash >= TankPrice) And
        (FPlayer[plComputer].tanks < numterritory) Do Begin
        i := random(BOARD_SIZE);
        j := random(BOARD_SIZE);
        If (FField[i, j].Colour = fcComputer) Then Begin
          buy_resources(foTank, i, j, 0);
        End;
      End;
    End
    Else Begin
      For i := 0 To k - 1 Do Begin
        str_diff := targets[i].str_diff;
        While (str_diff + 20 > 0) And (FPlayer[plComputer].cash > 0) And (FPlayer[plComputer].food * 1.09 >= FPlayer[plComputer].men) Do Begin
          // While we still need them keep placing men
          If Not place_adjacent(true, targets[i].x, targets[i].y) Then Begin
            adj := find_adjacent(targets[i].x, targets[i].y);
            men_needed := round((str_diff + 20) * 1000 / 133);
            If (FPlayer[plComputer].cash < men_needed) Then Begin
              men_needed := FPlayer[plComputer].cash;
            End;
            // Let at max 10% of the men die due to starvation -> Only when Attacking, in Defense the algorithm needs to be able to "Move" men by killing equally distributed
            // --> This will result in less "useless" buy's and will give the abillity to spend more money for tanks/planes/nukes in future moves
            If FPlayer[plComputer].food * 1.1 < FPlayer[plComputer].men + men_needed Then Begin
              men_needed := max(0, round(FPlayer[plComputer].food * 1.1) - FPlayer[plComputer].men);
            End;
            buy_resources(foMen, adj.X, adj.Y, men_needed);
            break;
          End;
          str_diff := calc_strength(fcHuman, targets[i].x, targets[i].y) - calc_strength(fcComputer, targets[i].x, targets[i].y);
        End;
      End;
    End;
  End
  Else Begin
    (* Work out what to place on each square to defend it.
     * Tanks are preferential because they do not require food,
     * but if the budget is tight then we fall back onto troops.
     * Conversely if cash is not an issue and there are already tanks in
     * place planes will be deployed. We would like a margin of at least
     * 20 points to be safe. *)
    For i := 0 To numthreats - 1 Do Begin
      total_str_diff := total_str_diff + threats[i].str_diff;
    End;
    If (total_str_diff + 20) * 10 > FPlayer[plComputer].cash Then Begin
      (* Not enough cash to accomodate all threats using tanks alone -
       * use men as a backup *)
      For i := 0 To numthreats - 1 Do Begin
        men_needed := round(((threats[i].str_diff + 20) * 1000) / 133);
        If (FPlayer[plComputer].cash < men_needed) Then Begin
          men_needed := FPlayer[plComputer].cash;
        End;
        buy_resources(foMen, threats[i].x, threats[i].y, men_needed);
      End;
    End
    Else Begin
      (* Tanks it is
       * Enough money to pay their way by planes? *)
      tank := ((total_str_diff + 20) * 15 >= FPlayer[plComputer].cash);
      For i := 0 To numthreats - 1 Do Begin
        str_diff := threats[i].str_diff;
        While (str_diff + 20 > 0) Do Begin
          If Not place_adjacent(tank, threats[i].x, threats[i].y) Then Begin
            (* No room for any more planes or tanks, revert to
             * men *)
            adj := find_adjacent(threats[i].x, threats[i].y);
            men_needed := round((str_diff + 20) * 1000 / 133);

            If (FPlayer[plComputer].cash < men_needed) Then Begin
              men_needed := FPlayer[plComputer].cash;
            End;
            buy_resources(foMen, threats[i].x, threats[i].y, men_needed);
            break;
          End;
          str_diff := calc_strength(fcHuman, threats[i].x, threats[i].y) - calc_strength(fcComputer, threats[i].x, threats[i].y);
        End;
      End;
    End;
  End;
  // invest money if AI is capable of doing so
  If AiToLevel(Fsuperdom_settings.compdiff) >= AI_INVESTING_LEVEL Then Begin
    FPlayer[plComputer].bank := FPlayer[plComputer].bank + FPlayer[plComputer].cash;
    FPlayer[plComputer].cash := 0;
  End;
End;

Procedure tSuperDom.computer_movement();
Var
  Nukes: Array Of TPoint;
  next_nuke, nukes_back: integer;
  i, j: integer;
  found_target: Boolean;
  adj: TPoint;
Begin
  // use nukes
  If (fsuperdom_settings.compdiff = aiHard) Then Begin
    If (FPlayer[plComputer].nukes > 0) Then Begin
      // Get all Locations of the nukes
      nukes_back := 0;
      Nukes := Nil;
      setlength(Nukes, FPlayer[plComputer].nukes);
      For i := 0 To BOARD_SIZE - 1 Do Begin
        For j := 0 To BOARD_SIZE - 1 Do Begin
          If FField[i, j].Nuke And (FField[i, j].Colour = fcComputer) Then Begin
            Nukes[nukes_back].x := i;
            Nukes[nukes_back].Y := j;
            inc(nukes_back);
          End;
        End;
      End;

      found_target := true;
      next_nuke := 0;
      // first, use nukes for defence
      While (found_target And (next_nuke < nukes_back)) Do Begin
        found_target := false;
        For i := 0 To BOARD_SIZE - 1 Do Begin
          For j := 0 To BOARD_SIZE - 1 Do Begin
            // Suchen aller "schützenswerten" Felder die bedroht werden
            If ((FField[i, j].Colour = fcComputer) And
              (FField[i, j].Farm Or FField[i, j].Industries Or FField[i, j].Nuke) And
              find_adjacent_target(i, j, adj)) Then Begin
              found_target := true;
              UserMsgEvent(Nil, format('Launching nuke for defence, from (%d, %d) to (%d, %d)', [nukes[next_nuke].x + 1, nukes[next_nuke].y + 1, adj.x + 1, adj.y + 1]));
              UserUpdateEvent(Nil);
              launch_nuke(fcComputer, nukes[next_nuke].x, nukes[next_nuke].y, adj.x, adj.y);
              inc(next_nuke);
            End;
          End;
        End;
      End;
      // if we still have any left over, use those for offence
      found_target := true;
      While (found_target And (next_nuke < nukes_back)) Do Begin
        found_target := false;
        For i := 0 To BOARD_SIZE - 1 Do Begin
          For j := 0 To BOARD_SIZE - 1 Do Begin
            If ((FField[i, j].Colour = fcHuman) And
              (FField[i][j].Industries Or FField[i][j].farm Or FField[i][j].nuke) And
              (calc_strength(fcComputer, i, j) <= calc_strength(fcHuman, i, j))) Then Begin
              found_target := true;
              UserMsgEvent(Nil, format('Launching nuke for offence, nuke index %d', [next_nuke + 1]));
              UserUpdateEvent(Nil);
              launch_nuke(fcComputer, nukes[next_nuke].x, nukes[next_nuke].y, i, j);
              inc(next_nuke);
            End;
          End;
        End;
      End;
    End;
  End;
  // TODO: move other units
End;

Function tSuperDom.buy_resources(object_type: TFieldObject; x, y, nummen: integer): boolean;
Var
  price: integer;
Begin
  result := false;
  price := 0;
  Case object_type Of
    foMen: price := menPrice * nummen;
    foTank: price := TankPrice;
    foPlane: price := PlanePrice;
    foFarm: price := FarmPrice;
    foIndustries: price := IndustryPrice;
    foNuke: price := NukePrice;
  End;
  If FPlayer[plComputer].cash < price Then exit;
  If FField[x, y].Colour <> fcComputer Then exit;
  If (object_type <> foMen) And tile_has_item(object_type, x, y) Then exit;
  // Alles I.O. der Deal kann steigen.
  Case object_type Of
    foMen: Begin
        FField[x, y].Men := FField[x, y].Men + nummen;
        FPlayer[plComputer].men := FPlayer[plComputer].men + nummen;
      End;
    foTank: Begin
        FField[x, y].Tank := true;
        FPlayer[plComputer].tanks := FPlayer[plComputer].tanks + 1;
      End;
    foPlane: Begin
        FField[x, y].Plane := true;
        FPlayer[plComputer].planes := FPlayer[plComputer].planes + 1;
      End;
    foFarm: Begin
        FField[x, y].Farm := true;
        FPlayer[plComputer].farms := FPlayer[plComputer].farms + 1;
      End;
    foIndustries: Begin
        FField[x, y].Industries := true;
        FPlayer[plComputer].Industries := FPlayer[plComputer].Industries + 1;
      End;
    foNuke: Begin
        FField[x, y].Nuke := true;
        FPlayer[plComputer].nukes := FPlayer[plComputer].nukes + 1;
      End;
  End;
  FPlayer[plComputer].cash := FPlayer[plComputer].cash - price;
  CallUserEvent();
  result := true;
End;

Function tSuperDom.tile_has_item(object_type: TFieldObject; x, y: integer): boolean;
Begin
  result := false;
  Case object_type Of
    foMen: result := FField[x, y].Men > 0;
    foNuke: result := FField[x, y].Nuke;
    foIndustries: result := FField[x, y].Industries;
    foFarm: result := FField[x, y].Farm;
    foPlane: result := FField[x, y].Plane;
    foTank: result := FField[x, y].Tank;
  End;
End;

Function tSuperDom.place_adjacent(tank: boolean; x, y: integer): Boolean;
Var
  type_: TFieldObject;
Begin
  If tank Then
    type_ := foTank
  Else
    type_ := foPlane;
  result := buy_resources(type_, x, y, 0);
  If result Then exit;
  result := buy_resources(type_, x - 1, y, 0);
  If result Then exit;
  result := buy_resources(type_, x + 1, y, 0);
  If result Then exit;
  result := buy_resources(type_, x, y - 1, 0);
  If result Then exit;
  result := buy_resources(type_, x, y + 1, 0);
End;

Function tSuperDom.find_adjacent(x, y: integer): Tpoint;
Begin
  (* Finds adjacent squares, returning squares without tanks on them
   * in preference to those with them *)
  If FField[x - 1, y].Colour = fcComputer Then Begin
    result := point(x - 1, y);
    exit;
  End;
  If FField[x + 1, y].Colour = fcComputer Then Begin
    result := point(x + 1, y);
    exit;
  End;
  If FField[x, y - 1].Colour = fcComputer Then Begin
    result := point(x, y - 1);
    exit;
  End;
  If FField[x, y + 1].Colour = fcComputer Then Begin
    result := point(x, y + 1);
    exit;
  End;
End;

Function tSuperDom.getmoves: boolean;
Begin
  result := FPlayer[plHuman].moves > 0;
End;

Function tSuperDom.getActive_Cell_Men_Count: integer;
Begin
  If (FCursor.X <> -1) And (FField[FCursor.X, FCursor.Y].Colour = fcHuman) Then Begin
    result := FField[FCursor.X, FCursor.Y].Men;
  End
  Else Begin
    result := 0;
  End;
End;

Function tSuperDom.getActive_Cell_Tank: Boolean;
Begin
  If (FCursor.X <> -1) And (FField[FCursor.X, FCursor.Y].Colour = fcHuman) Then Begin
    result := FField[FCursor.X, FCursor.Y].Tank;
  End
  Else Begin
    result := false;
  End;
End;

Function tSuperDom.getActive_Cell_Plane: Boolean;
Begin
  If (FCursor.X <> -1) And (FField[FCursor.X, FCursor.Y].Colour = fcHuman) Then Begin
    result := FField[FCursor.X, FCursor.Y].Plane;
  End
  Else Begin
    result := false;
  End;
End;

Function tSuperDom.getActive_Cell_Nuke: Boolean;
Begin
  If (FCursor.X <> -1) And (FField[FCursor.X, FCursor.Y].Colour = fcHuman) Then Begin
    result := FField[FCursor.X, FCursor.Y].Nuke;
  End
  Else Begin
    result := false;
  End;
End;

End.

