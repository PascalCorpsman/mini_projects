(******************************************************************************)
(* Superdom                                                        ??.??.???? *)
(*                                                                            *)
(* Version     : 0.04                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : This is the FPC-Translation of the Superdom game from the    *)
(*               rockbox MP3-Player OS.                                       *)
(*               Original Source: https://git.rockbox.org/cgit/rockbox.git/tree/apps/plugins/superdom.c?id=cb94b3ae2e *)
(*                                                                            *)
(*                                                                            *)
(* License     : See the file license.md, located under:                      *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(* Warranty    : There is no warranty, neither in correctness of the          *)
(*               implementation, nor anything other that could happen         *)
(*               or go wrong, use at your own risk.                           *)
(*                                                                            *)
(* Known Issues: none                                                         *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*               0.02 - Online hilfe                                          *)
(*                      Bugfix Launch Nuke                                    *)
(*               0.03 - Die Popup Menus als eigene Formulare, wenn gewünscht  *)
(*               0.04 - Update auf neueste GIT-Version (AI-Strength,          *)
(*                      persistent units, spoilage, ..)                       *)
(*                      Anzeige der AVG-Strength via Trackbar                 *)
(*                                                                            *)
(******************************************************************************)

Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Menus,
  ExtCtrls, StdCtrls, ComCtrls, usuperdom;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem19: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem20: TMenuItem;
    MenuItem21: TMenuItem;
    MenuItem22: TMenuItem;
    MenuItem23: TMenuItem;
    MenuItem24: TMenuItem;
    MenuItem25: TMenuItem;
    MenuItem26: TMenuItem;
    MenuItem27: TMenuItem;
    MenuItem28: TMenuItem;
    MenuItem29: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem30: TMenuItem;
    MenuItem31: TMenuItem;
    MenuItem32: TMenuItem;
    MenuItem33: TMenuItem;
    MenuItem34: TMenuItem;
    MenuItem35: TMenuItem;
    MenuItem36: TMenuItem;
    MenuItem37: TMenuItem;
    MenuItem38: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    PaintBox1: TPaintBox;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    PopupMenu3: TPopupMenu;
    SaveDialog1: TSaveDialog;
    TrackBar1: TTrackBar;
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure FormCreate(Sender: TObject);
    Procedure MenuItem10Click(Sender: TObject);
    Procedure MenuItem11Click(Sender: TObject);
    Procedure MenuItem12Click(Sender: TObject);
    Procedure MenuItem13Click(Sender: TObject);
    Procedure MenuItem14Click(Sender: TObject);
    Procedure MenuItem15Click(Sender: TObject);
    Procedure MenuItem16Click(Sender: TObject);
    Procedure MenuItem17Click(Sender: TObject);
    Procedure MenuItem18Click(Sender: TObject);
    Procedure MenuItem20Click(Sender: TObject);
    Procedure MenuItem21Click(Sender: TObject);
    Procedure MenuItem23Click(Sender: TObject);
    Procedure MenuItem26Click(Sender: TObject);
    Procedure MenuItem27Click(Sender: TObject);
    Procedure MenuItem28Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure MenuItem34Click(Sender: TObject);
    Procedure MenuItem36Click(Sender: TObject);
    Procedure MenuItem37Click(Sender: TObject);
    Procedure MenuItem38Click(Sender: TObject);
    Procedure MenuItem3Click(Sender: TObject);
    Procedure MenuItem5Click(Sender: TObject);
    Procedure MenuItem6Click(Sender: TObject);
    Procedure MenuItem7Click(Sender: TObject);
    Procedure MenuItem8Click(Sender: TObject);
    Procedure MenuItem9Click(Sender: TObject);
    Procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure PaintBox1Paint(Sender: TObject);
    Procedure TrackBar1Change(Sender: TObject);
  private
    { private declarations }
    MoveMen: Boolean;
    MoveTank: Boolean;
    MovePlane: Boolean;
    LaunchNuke: Boolean;
    Procedure SetPopUpMenu(aMenu: TPopupMenu);
  public
    { public declarations }
    Procedure UserUpdateEvent(Sender: TObject);
    Procedure UserMessageEvent(Sender: TObject; Msg: String);
  End;

Var
  Form1: TForm1;
  SuperDom: tSuperDom;

Implementation

{$R *.lfm}

Uses
  unit2, // Start Round Dialog
  unit3, // Zahleneingabe Dialog
  unit4, // Inventar
  unit5, // die PopupMenu's als Formular
  unit6, // Gameplay zum permanenten anzeigen ;)
  lazutf8;

{ TForm1 }

Procedure TForm1.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Begin
  SuperDom.Free;
  SuperDom := Nil;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  caption := 'Superdom remake of the Rockbox version 0.04 by Corpsman | www.Corpsman.de |';
  Randomize;
  SuperDom := tSuperDom.Create;
  superdom.UserUpdateEvent := @form1.UserUpdateEvent;
  superdom.UserMsgEvent := @form1.UserMessageEvent;
  OpenDialog1.InitialDir := ExtractFilePath(ParamStrUTF8(0));
  SaveDialog1.InitialDir := ExtractFilePath(ParamStrUTF8(0));
  label12.caption := '';
End;

Procedure TForm1.MenuItem10Click(Sender: TObject);
Begin
  If SuperDom.PlayerCash >= PlanePrice Then Begin
    If SuperDom.buyPlane() Then Begin
      PaintBox1.Invalidate;
    End
    Else Begin
      showmessage('Error, you are not allowed to place a plane here.');
    End;
  End
  Else Begin
    showmessage('You don''t have any Money to buy plane.');
  End;
End;

Procedure TForm1.MenuItem11Click(Sender: TObject);
Begin
  If SuperDom.PlayerCash >= FarmPrice Then Begin
    If SuperDom.buyFarm() Then Begin
      PaintBox1.Invalidate;
    End
    Else Begin
      showmessage('Error, you are not allowed to place a farm here.');
    End;
  End
  Else Begin
    showmessage('You don''t have any Money to buy farm.');
  End;
End;

Procedure TForm1.MenuItem12Click(Sender: TObject);
Begin
  If SuperDom.PlayerCash >= IndustryPrice Then Begin
    If SuperDom.BuyIndustry() Then Begin
      PaintBox1.Invalidate;
    End
    Else Begin
      showmessage('Error, you are not allowed to place a industry here.');
    End;
  End
  Else Begin
    showmessage('You don''t have any Money to buy industry.');
  End;
End;

Procedure TForm1.MenuItem13Click(Sender: TObject);
Begin
  If SuperDom.PlayerCash >= NukePrice Then Begin
    If SuperDom.BuyNuke() Then Begin
      PaintBox1.Invalidate;
    End
    Else Begin
      showmessage('Error, you are not allowed to place a nuke here.');
    End;
  End
  Else Begin
    showmessage('You don''t have any Money to buy nuke.');
  End;
End;

Procedure TForm1.MenuItem14Click(Sender: TObject);
Begin
  If SuperDom.moves Then Begin
    If SuperDom.Active_Cell_Men_Count > 0 Then Begin
      form3.ModalResult := mrCancel;
      form3.edit1.Text := inttostr(SuperDom.Active_Cell_Men_Count);
      form3.ShowModal;
      If Form3.ModalResult = mrOK Then Begin
        //        showmessage('Select where you want to move troops to');
        Label12.Caption := 'Select where you want' + LineEnding + 'to move troops to';
        label9.caption := '';
        MoveMen := True;
      End;
    End
    Else Begin
      showmessage('You don''t have troops there.');
    End;
  End
  Else Begin
    showmessage('You have no more moves left.' + LineEnding +
      'You can buy more for $' + inttostr(MovePrice) + ' each.');
  End;
End;

Procedure TForm1.MenuItem15Click(Sender: TObject);
Begin
  If SuperDom.moves Then Begin
    If SuperDom.Active_Cell_Tank Then Begin
      MoveTank := True;
      //      showmessage('Select where you want to move tank to');
      label12.Caption := 'Select where you want' + LineEnding + 'to move tank to';
      label9.caption := '';
    End
    Else Begin
      showmessage('You don''t have a tank there.');
    End;
  End
  Else Begin
    showmessage('You have no more moves left.' + LineEnding +
      'You can buy more for $' + inttostr(MovePrice) + ' each.');
  End;
End;

Procedure TForm1.MenuItem16Click(Sender: TObject);
Begin
  If SuperDom.moves Then Begin
    If SuperDom.Active_Cell_Plane Then Begin
      MovePlane := True;
      //      showmessage('Select where you want to move the plane to');
      label12.caption := 'Select where you want' + LineEnding + 'to move the plane to';
      label9.caption := '';
    End
    Else Begin
      showmessage('You don''t have a plane there.');
    End;
  End
  Else Begin
    showmessage('You have no more moves left.' + LineEnding +
      'You can buy more for $' + inttostr(MovePrice) + ' each.');
  End;
End;

Procedure TForm1.MenuItem17Click(Sender: TObject);
Begin
  If SuperDom.moves Then Begin
    If SuperDom.Active_Cell_Nuke Then Begin
      LaunchNuke := True;
      //      showmessage('Select place to target with nuke.');
      label12.caption := 'Select place to' + LineEnding + 'target with nuke.';
      label9.caption := '';
    End
    Else Begin
      showmessage('You don''t have a Nuke there.');
    End;
  End
  Else Begin
    showmessage('You have no more moves left.' + LineEnding +
      'You can buy more for $' + inttostr(MovePrice) + ' each.');
  End;
End;

Procedure TForm1.MenuItem18Click(Sender: TObject);
Begin
  // Finish Buy
  SuperDom.FinishBuy();
  // Dem Spieler Zeigen was die KI gekauft hat.
  PaintBox1.Invalidate();
  If SuperDom.State = gsBuyRessources Then Begin
    SetPopupMenu(PopupMenu1);
  End;
  If SuperDom.State = gsMoveUnits Then Begin
    SetPopupMenu(PopupMenu2);
  End;
  If SuperDom.State = gswar Then Begin
    SetPopupMenu(PopupMenu3);
  End;
End;

Procedure TForm1.MenuItem20Click(Sender: TObject);
Begin
  If SuperDom.PlayerCash > 0 Then Begin
    form3.ModalResult := mrCancel;
    form3.edit1.Text := inttostr(SuperDom.PlayerCash);
    form3.ShowModal;
    If Form3.ModalResult = mrOK Then Begin
      SuperDom.MoveCash(strtointdef(form3.Edit1.Text, 0));
      PaintBox1.Invalidate;
    End;
  End
  Else Begin
    showmessage('You don''t have any Money to invest.');
  End;
End;

Procedure TForm1.MenuItem21Click(Sender: TObject);
Begin
  If SuperDom.PlayerBankCash > 0 Then Begin
    form3.ModalResult := mrCancel;
    form3.edit1.Text := inttostr(SuperDom.PlayerBankCash);
    form3.ShowModal;
    If Form3.ModalResult = mrOK Then Begin
      SuperDom.MoveCash(-strtointdef(form3.Edit1.Text, 0));
      PaintBox1.Invalidate;
    End;
  End
  Else Begin
    showmessage('You don''t have any Money to withdraw.');
  End;
End;

Procedure TForm1.MenuItem23Click(Sender: TObject);
Begin
  If SuperDom.PlayerBankCash >= MovePrice Then Begin
    showmessage('You now have ' + inttostr(SuperDom.buyMove) + ' moves.');
  End
  Else Begin
    showmessage('You do not have enough money to by a aditional move.');
  End;
End;

Procedure TForm1.MenuItem26Click(Sender: TObject);
Var
  i: integer;
Begin
  // Ernähren der Männer
  i := Superdom.SpoilFood();
  If i > 0 Then Begin
    showmessage(format('Spoilage claims %d units of food', [i]));
  End;
  i := SuperDom.FeedMen(plHuman);
  If i > 0 Then Begin
    showmessage(format('Your men ate %d units of food', [i]));
  End;
  If i < 0 Then Begin
    showmessage(format('There was not enough food' + LineEnding +
      'to feed all your men, %d' + LineEnding +
      'men have died of starvation', [-i]));
  End;
  i := SuperDom.FeedMen(plComputer);
  If i < 0 Then Begin
    showmessage(format('The computer does not have' + LineEnding +
      'enough food to feed its men.' + LineEnding +
      '%d have died of starvation', [-i]));
  End;
  // War
  SuperDom.finishMove;
  // showmessage('War'+LineEnding+'Select territory to attack.');
  If SuperDom.State = gsBuyRessources Then Begin
    SetPopupMenu(PopupMenu1);
  End;
  If SuperDom.State = gsMoveUnits Then Begin
    SetPopupMenu(PopupMenu2);
  End;
  If SuperDom.State = gswar Then Begin
    SetPopupMenu(PopupMenu3);
  End;
End;

Procedure TForm1.MenuItem27Click(Sender: TObject);
Begin
  // Attack Territory
  If SuperDom.can_attack() Then Begin
    If SuperDom.attack_territory(fcHuman, 0, 0) Then Begin
      PaintBox1.Invalidate;
      //      Es scheint, als sei das automatische Weiterschalten nicht nicht drin..
            // Wir haben keine Spielzüge mehr frei
      If Not SuperDom.Moves Then Begin
        MenuItem28Click(Nil);
      End;
    End
    Else Begin
      showmessage('Your troops were unable to overcome' + LineEnding +
        'the enemy troops');
    End;
  End
  Else Begin
    showmessage('You can''t attack your own territory');
  End;
End;

Procedure TForm1.MenuItem28Click(Sender: TObject);
Var
  s: String;
Begin
  // Finish turn
  If SuperDom.FinishWar() Then Begin
    // Dem Spieler ist was Passiert.
  End
  Else Begin
    // Dem Spieler ist nichts passiert
    showmessage('The computer attempted to ' + LineEnding +
      'attack, but the invasion was' + LineEnding +
      'pushed back');
  End;
  PaintBox1.Invalidate;
  s := SuperDom.CheckFinish();
  If s <> '' Then Begin
    showmessage(s);
    If form4.Visible Then form4.Visible := false;
    If form5.Visible Then form5.Visible := false;
    TrackBar1.Position := 0;
  End;
  If SuperDom.State = gsBuyRessources Then Begin
    SetPopupMenu(PopupMenu1);
  End;
  If SuperDom.State = gsMoveUnits Then Begin
    SetPopupMenu(PopupMenu2);
  End;
  If SuperDom.State = gswar Then Begin
    SetPopupMenu(PopupMenu3);
  End;
End;

Procedure TForm1.MenuItem2Click(Sender: TObject);
Begin
  // New Game
  superdom.Reset(); // -- Das Killt das Aktuelle Spiel, auch wenn der User Abbrechen drückt ...
  form2.RadioGroup1.Visible := true;
  form2.RadioGroup1.ItemIndex := 1; // Default medium
  form2.CheckBox1.Visible := true;
  form2.CheckBox1.Checked := false; // Default, alles geht kaputt
  form2.CheckBox2.Visible := true;
  form2.CheckBox2.Checked := false; // Default, alles geht kaputt
  MenuItem5Click(Nil); // Erst mal die Einstellungen Prüfen Lassen.
  form2.RadioGroup1.Visible := false;
  form2.CheckBox1.Visible := false;
  form2.CheckBox2.Visible := false;
  If form2.ModalResult = mrOK Then Begin
    If AiToLevel(SuperDom.Settings.compdiff) >= 3 Then Begin
      TrackBar1.Max := HARD_SURRENDER_THRESHOLD;
      TrackBar1.Min := -HARD_SURRENDER_THRESHOLD;
    End
    Else Begin
      TrackBar1.Max := NORMAL_SURRENDER_THRESHOLD;
      TrackBar1.Min := -NORMAL_SURRENDER_THRESHOLD;
    End;
    MoveMen := false;
    SuperDom.NewGame();
    SetPopupMenu(PopupMenu1);
    PaintBox1.Invalidate;
  End;
End;

Procedure TForm1.MenuItem34Click(Sender: TObject);
Begin
  // Show Inventory
  form4.show;
End;

Procedure TForm1.MenuItem36Click(Sender: TObject);
Begin
  // Online Hilfe
  showmessage(
    'Super domination' + LineEnding +
    'Aim' + LineEnding +
    'Super Domination is a turn-based' + LineEnding +
    'strategy game where the aim is' + LineEnding +
    'to overcome the computer.' + LineEnding + LineEnding +
    'How to Play' + LineEnding +
    'Eay year you are allocated an amount of cash' + LineEnding +
    'and food depending on how many farms and' + LineEnding +
    'factories you control.' + LineEnding + LineEnding +
    'Use this cash and food to build and feed your' + LineEnding +
    'army. Each tile has a strength, calculated by' + LineEnding +
    'the ownershop of surrounding tiles and the type' + LineEnding +
    'and number of troops on them.' + LineEnding + LineEnding +
    'This is a port of the original Rockbox version, done by Corpsman.');

End;

Procedure TForm1.MenuItem37Click(Sender: TObject);
Begin
  MenuItem37.Checked := Not MenuItem37.Checked;
  SetPopUpMenu(PaintBox1.PopupMenu);
End;

Procedure TForm1.MenuItem38Click(Sender: TObject);
Begin
  // Gameplay
  form6.show;
End;

Procedure TForm1.MenuItem3Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.MenuItem5Click(Sender: TObject);
Var
  s: TSuperdomSettings;
Begin
  form2.ModalResult := mrCancel;
  Form2.SpinEdit1.Value := SuperDom.Settings.compstartfarms;
  Form2.SpinEdit2.Value := SuperDom.Settings.compstartinds;
  Form2.SpinEdit3.Value := SuperDom.Settings.humanstartfarms;
  Form2.SpinEdit4.Value := SuperDom.Settings.humanstartinds;
  Form2.SpinEdit5.Value := SuperDom.Settings.startcash;
  Form2.SpinEdit6.Value := SuperDom.Settings.startfood;
  Form2.SpinEdit7.Value := SuperDom.Settings.movesperturn;
  Form2.RadioGroup1.ItemIndex := 1;
  form2.ShowModal;
  If form2.ModalResult = mrOK Then Begin
    If SuperDom.State = gsInvalid Then Begin
      s.compstartfarms := Form2.SpinEdit1.Value;
      s.compstartinds := Form2.SpinEdit2.Value;
      s.humanstartfarms := Form2.SpinEdit3.Value;
      s.humanstartinds := Form2.SpinEdit4.Value;
      s.startcash := Form2.SpinEdit5.Value;
      s.startfood := Form2.SpinEdit6.Value;
      s.movesperturn := Form2.SpinEdit7.Value;
      // Sich selbst zurücksetztende "Features" werden natürlich dennoch übernommen
      Case Form2.RadioGroup1.ItemIndex Of
        0: s.compdiff := aiEasy;
        1: s.compdiff := aiMedium;
        2: s.compdiff := aiHard;
      End;
      s.persistent_units := form2.CheckBox1.Checked;
      s.spoil_enabled := form2.CheckBox2.Checked;
      SuperDom.Settings := s;
    End
    Else Begin
      showmessage('Not allowed at this time.');
    End;
  End;
End;

Procedure TForm1.MenuItem6Click(Sender: TObject);
Begin
  If OpenDialog1.Execute Then Begin
    SuperDom.LoadFromFile(OpenDialog1.FileName);
    MoveMen := false;
    MoveTank := false;
    MovePlane := false;
    LaunchNuke := false;
    PaintBox1.Invalidate;
  End;
End;

Procedure TForm1.MenuItem7Click(Sender: TObject);
Begin
  If MoveMen Or MoveTank Or MovePlane Or LaunchNuke Then Begin
    ShowMessage('Saving only after finishing moving.');
    exit;
  End;
  If SaveDialog1.Execute Then Begin
    SuperDom.SafeToFile(SaveDialog1.FileName);
  End;
End;

Procedure TForm1.MenuItem8Click(Sender: TObject);
Begin
  If SuperDom.PlayerCash > 0 Then Begin
    form3.ModalResult := mrCancel;
    form3.edit1.Text := inttostr(SuperDom.PlayerCash);
    form3.ShowModal;
    If Form3.ModalResult = mrOK Then Begin
      If SuperDom.buyMen(strtointdef(form3.Edit1.Text, 0)) Then Begin
        PaintBox1.Invalidate;
      End
      Else Begin
        showmessage('Error, you are not allowed to place men here.');
      End;
    End;
  End
  Else Begin
    showmessage('You don''t have any Money to buy men.');
  End;
End;

Procedure TForm1.MenuItem9Click(Sender: TObject);
Begin
  If SuperDom.PlayerCash >= TankPrice Then Begin
    If SuperDom.buyTank() Then Begin
      PaintBox1.Invalidate;
    End
    Else Begin
      showmessage('Error, you are not allowed to place a tank here.');
    End;
  End
  Else Begin
    showmessage('You don''t have any Money to buy tank.');
  End;
End;

Procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  s: String;
Begin
  x := x Div (PaintBox1.Width Div 10);
  y := y Div (PaintBox1.Height Div 10);
  If (x >= 0) And (x <= 9) And (y >= 0) And (y <= 9) Then Begin
    If MoveMen Then Begin
      If Not superdom.moveMen(x, y, strtointdef(form3.Edit1.Text, 0)) Then Begin
        showmessage('Invalid move');
      End;
      MoveMen := false;
      If (Not SuperDom.Moves) And (SuperDom.PlayerCash < MovePrice) Then Begin // Automatischer Finish Move's
        PaintBox1.Invalidate;
        MenuItem26Click(Nil);
      End;
    End;
    If MoveTank Then Begin
      If Not superdom.moveTank(x, y) Then Begin
        showmessage('Invalid move');
      End;
      MoveTank := false;
      If (Not SuperDom.Moves) And (SuperDom.PlayerCash < MovePrice) Then Begin // Automatischer Finish Move's
        PaintBox1.Invalidate;
        MenuItem26Click(Nil);
      End;
    End;
    If MovePlane Then Begin
      If Not superdom.MovePlane(x, y) Then Begin
        showmessage('Invalid move');
      End;
      MovePlane := false;
      If (Not SuperDom.Moves) And (SuperDom.PlayerCash < MovePrice) Then Begin // Automatischer Finish Move's
        PaintBox1.Invalidate;
        MenuItem26Click(Nil);
      End;
    End;
    If LaunchNuke Then Begin
      s := superdom.LaunchNuke(x, y);
      If s <> '' Then Begin
        showmessage(s);
      End;
      LaunchNuke := false;
      If (Not SuperDom.Moves) And (SuperDom.PlayerCash < MovePrice) Then Begin // Automatischer Finish Move's
        PaintBox1.Invalidate;
        MenuItem26Click(Nil);
      End;
    End;
    Superdom.SelectCell(x, y);
    //PaintBox1.Invalidate; // Eigentlich würde ich ja Invalidate nehmen, aber unter Windoof flackert das, mit OnPaint(nil) tuts das nicht..
    PaintBox1.OnPaint(Nil);
    Application.ProcessMessages;
    If SuperDom.State = gsBuyRessources Then Begin
      SetPopupMenu(PopupMenu1);
    End;
    If SuperDom.State = gsMoveUnits Then Begin
      SetPopupMenu(PopupMenu2);
    End;
    If SuperDom.State = gswar Then Begin
      SetPopupMenu(PopupMenu3);
    End;
  End
  Else Begin
    PaintBox1.PopupMenu := Nil;
  End;
End;

Procedure TForm1.PaintBox1Paint(Sender: TObject);
Var
  score: TFieldInfo;
Begin
  If assigned(SuperDom) And (SuperDom.State <> gsInvalid) Then Begin
    SuperDom.RenderBattleField(PaintBox1.Canvas, rect(0, 0, PaintBox1.Width, PaintBox1.Height));
    score := SuperDom.Update_Score;
    label5.caption := format('%d.%d', [score.HumanPower Div 10, score.HumanPower Mod 10]);
    label6.caption := format('%d.%d', [score.ComputerPower Div 10, score.ComputerPower Mod 10]);
    label7.caption := inttostr(SuperDom.PLayerCash);
    label8.caption := inttostr(SuperDom.PlayerFood);
    label11.caption := inttostr(SuperDom.PlayerBankCash);
    form4.label1.caption := superdom.Inventory;
    If (Not MoveMen) And
      (Not MoveTank) And
      (Not MovePlane) And
      (Not LaunchNuke) Then Begin
      Case SuperDom.State Of
        gsBuyRessources: label9.caption := 'Buy Phase';
        gsMoveUnits: label9.caption := 'Move Phase';
        gsWar: label9.caption := 'War';
      End;
      label12.caption := '';
    End;
    TrackBar1Change(Nil); // Refresh Strength Stat
  End
  Else Begin
    label5.caption := '';
    label6.caption := '';
    label7.caption := '';
    label8.caption := '';
    label9.caption := 'New Game ?';
    label11.caption := '';
    TrackBar1.Position := 0;
  End;
End;

Procedure TForm1.TrackBar1Change(Sender: TObject);
Begin
  TrackBar1.Position := SuperDom.GetAvgStrength(); // --> So wird das quasi zum Read Only ;)
End;

Procedure TForm1.SetPopUpMenu(aMenu: TPopupMenu);
Var
  i, c, d: Integer;
Begin
  PaintBox1.PopupMenu := aMenu;
  If Not assigned(aMenu) Then exit;
  // Löschen des alten
  For i := 0 To high(Form5.buttons) Do Begin
    form5.buttons[i].free;
  End;
  For i := 0 To high(form5.panels) Do Begin
    form5.panels[i].free;
  End;
  // erzeugen des neuen
  setlength(form5.buttons, aMenu.Items.Count);
  setlength(form5.panels, aMenu.Items.Count);
  c := 0;
  d := 0;
  For i := 0 To aMenu.Items.Count - 1 Do Begin
    If aMenu.Items[i].Caption <> '-' Then Begin
      form5.buttons[c] := Tbutton.Create(form5);
      form5.buttons[c].name := 'popupbutton' + inttostr(c);
      form5.buttons[c].parent := form5;
      form5.buttons[c].left := 10;
      form5.buttons[c].top := c * 40 + 5;
      form5.buttons[c].width := 200;
      form5.buttons[c].Caption := aMenu.Items[i].Caption;
      form5.buttons[c].OnClick := aMenu.Items[i].OnClick;
      inc(c);
    End
    Else Begin
      form5.panels[d] := TPanel.Create(form5);
      form5.panels[d].name := 'popuppanel' + inttostr(d);
      form5.panels[d].Parent := form5;
      form5.panels[d].Caption := '';
      form5.panels[d].left := 10;
      form5.panels[d].width := 200;
      form5.panels[d].height := 5;
      form5.panels[d].top := (c - 1) * 40 + 10 + form5.buttons[0].Height;
      inc(d);
    End;
  End;
  setlength(form5.buttons, c);
  setlength(form5.panels, d);
  form5.height := length(form5.buttons) * 40 + 10;
  form5.width := 220;
  If MenuItem37.Checked Then Begin
    If Not form5.Visible Then Begin
      form5.top := form1.top;
      form5.left := form1.left + form1.width + 10;
      form5.show;
    End;
  End;
End;

Procedure TForm1.UserUpdateEvent(Sender: TObject);
Begin
  sleep(500);
  PaintBox1.Invalidate;
  Application.ProcessMessages;
  sleep(500);
End;

Procedure TForm1.UserMessageEvent(Sender: TObject; Msg: String);
Begin
  ShowMessage(Msg);
End;

End.

