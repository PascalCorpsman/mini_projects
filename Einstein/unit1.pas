(******************************************************************************)
(* Einstein                                                        ??.??.???? *)
(*                                                                            *)
(* Version     : 0.03                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Program to solve Einstein like puzzles                       *)
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
(*               0.02 - Fix Absturz, wenn Brutefoce keine Lösung mehr findet. *)
(*               0.03 - IntDepends Regel implementiert                        *)
(*                                                                            *)
(******************************************************************************)
{
  http://www.techinsider.io/how-to-solve-einsteins-riddle-video-2015-9
  https://www.logisch-gedacht.de/logikraetsel/einsteinraetsel/loesung/
  https://udel.edu/~os/riddle-solution.html
}
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, Menus, ueinstein, ComCtrls;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    ApplicationProperties1: TApplicationProperties;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    ComboBox1: TComboBox;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    PageControl2: TPageControl;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    ScrollBar1: TScrollBar;
    StringGrid1: TStringGrid;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    TabSheet8: TTabSheet;
    Procedure ApplicationProperties1Idle(Sender: TObject; Var Done: Boolean);
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure MenuItem10Click(Sender: TObject);
    Procedure MenuItem11Click(Sender: TObject);
    Procedure MenuItem12Click(Sender: TObject);
    Procedure MenuItem13Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure MenuItem3Click(Sender: TObject);
    Procedure MenuItem4Click(Sender: TObject);
    Procedure MenuItem5Click(Sender: TObject);
    Procedure MenuItem6Click(Sender: TObject);
    Procedure MenuItem8Click(Sender: TObject);
    Procedure MenuItem9Click(Sender: TObject);
    Procedure OnHideSetPage(Sender: TObject);
    Procedure ScrollBar1Change(Sender: TObject);
    Procedure StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
  private
    { private declarations }
    defcaption: String;
    RuleList: Array Of TEinsteinRuleFrame;
    Filename: String;
    Procedure FixRulePositions;
    Function ResolveXY(x, y: integer): String; // X = Durchnummerierung der Mengen, y = Eintrag (beides beginnend mit 0, -1 = nichts, -2 = Name der Kategorie
    Procedure RefreshTexts;
    Procedure SelfDeleteCallback(Sender: TEinsteinRuleFrame);
    Procedure SavetoFile(FName: String);
    Procedure LoadFromFile(Fname: String);
    Procedure LogEvent(Logentry: String);
  public
    { public declarations }
    Procedure NewDimensions(SetCount, ElementPerSetCount: integer); // Löscht alles alte und Initialisiert entsprechend neu
  End;

Var
  Form1: TForm1;
  DeletionItem: integer = -1;

Implementation

Uses
  unit2, Unit3, unit4, ufifo,
  LazFileUtils,
  lazutf8, LCLType,
  math,
  IniFiles,
  usetframe,
  uonesetruleframe,
  uonesetattribruleframe,
  utwosetattribruleframe,
  utwosetruleframe,
  umultisetframe,
  uintdepframe;

Var
  Form1ShowFirst: Boolean = true; // Sorgt dafür das der Form1.Onshow nur 1 mal gemacht wird.

{$R *.lfm}

Const
  // Inhalt der Combobox zur Auswahl der Regeln
(*
  Depends on
  Depends distance on
  Eliminate
  Eliminate distance
  Left of
  Left distance of
  Right of
  Right distance of
  MultiEliminate
  IntegerDepends
*)
  IndexDependsOn = 0;
  IndexDistanceDependsOn = 1;
  IndexEliminate = 2;
  IndexDistanceEliminate = 3;
  IndexLeftOf = 4;
  IndexDistanceLeftOf = 5;
  IndexRightOf = 6;
  IndexDistanceRightOf = 7;
  IndexMultiEliminate = 8;
  IndexIntegerDepends = 9;
  // Farben
  Rot = 0;
  Gruen = 1;
  Gelb = 2;
  Blau = 3;
  Weiss = 4;
  // Nationalitäten
  Brite = 0;
  Schwede = 1;
  Daene = 2;
  Norweger = 3;
  Deutsche = 4;
  // Getränke
  Tee = 0;
  Kaffee = 1;
  Milch = 2;
  Bier = 3;
  Wasser = 4;
  // Zigaretten
  Pallmall = 0;
  Dunhill = 1;
  Marlboro = 2;
  Winfield = 3;
  Rothmanns = 4;
  // Tier
  Hund = 0;
  Vogel = 1;
  Katze = 2;
  Pferd = 3;
  //  Fisch = 4; -- Den gilt es zu bestimmen, deswegen ist der in keiner Regel ;)

(*
 * Die Musterlösung :
 *)
//Nummer      	1          2          3          4          5
//Farbe	        gelb       blau       rot        grün       weiß
//Nationalität	Norweger   Däne       Brite      Deutscher  Schwede
//Getränk       Wasser     Tee        Milch      Kaffee     Bier
//Zigaretten    Dunhill    Marlboro   Pall-Mall  Rothmanns  Winfield
//Tier	        Katze      Pferd      Vogel      Fisch      Hund

{ TForm1 }

Procedure TForm1.Button1Click(Sender: TObject);
Type
  TFieldQueue = specialize TBufferedFifo < TDataField > ;
  TStringQueue = specialize TBufferedFifo < String > ;

  Function Clon(Const Source: TDataField): TDataField;
  Var
    i, j, k: integer;
  Begin
    setlength(result, length(source), length(source[0]));
    For i := 0 To high(Source) Do Begin
      For j := 0 To high(source[i]) Do Begin
        setlength(result[i, j].pencils, length(source[i, j].pencils));
        result[i, j].value := Source[i, j].value;
        For k := 0 To high(source[i, j].pencils) Do Begin
          result[i, j].pencils[k] := source[i, j].pencils[k];
        End;
      End;
    End;
  End;

  Procedure Free_(Var Data: TDataField);
  Var
    i, j: Integer;
  Begin
    For i := 0 To high(data) Do Begin
      For j := 0 To high(data[i]) Do Begin
        setlength(data[i, j].pencils, 0);
      End;
      setlength(data[i], 0);
    End;
    setlength(data, 0);
  End;

Var
  x, xx, y, i, j, k: integer;
  Rules: Array Of TRule;
  m: TMemberSet;
  found: Boolean;
  s: String;
  q: TFieldQueue;
  sq: TStringQueue;
  b: Boolean;
  bkup: TDataField;
Begin
  // Wenn ein Feld aufgrund eines vorangegangenen Versuches via Bruteforce Platt gemacht wurde, dann wird es hier neu erzeugt.
  If Not assigned(data) Then Begin
    setlength(data, EinsteinWidth, EinsteinHeight);
    For i := 0 To EinsteinWidth - 1 Do Begin
      For j := 0 To EinsteinHeight - 1 Do Begin
        setlength(data[i, j].Pencils, EinsteinWidth);
      End;
    End;

  End;
  rules := Nil;
  form2.ListBox1.Clear;
  // Einlesen der Regeln
  For i := 0 To high(RuleList) Do Begin
    If RuleList[i].ValidRule Then Begin
      setlength(rules, high(Rules) + 2);
      rules[high(Rules)] := RuleList[i].DeriveRule;
      rules[high(Rules)].Number := i + 1;
      rules[high(Rules)].ResolveXY := @ResolveXY;
    End;
  End;
  // Ein Leeres Feld
  ResetData;
  If (Not solve(rules, @logEvent, MenuItem11.Checked, false)) And MenuItem10.Checked Then Begin // 1 mal Raten
    setlength(rules, high(Rules) + 2);
    setlength(m, EinsteinHeight);
    rules[high(rules)] := TGuess.Create(m);
    rules[high(Rules)].Number := length(rules);
    rules[high(Rules)].ResolveXY := @ResolveXY;
    For y := 0 To EinsteinHeight - 1 Do
      m[y] := Nichts;
    found := false;
    For x := 0 To EinsteinWidth - 1 Do Begin
      m[0] := x;
      For y := 1 To EinsteinHeight - 1 Do Begin
        For xx := 0 To EinsteinWidth - 1 Do Begin
          m[y] := xx;
          form2.ListBox1.Clear;
          (rules[high(Rules)] As TGuess).Active := false;
          (rules[high(Rules)] As TGuess).UpdateGuess(m);
          // Ein Leeres Feld
          ResetData;
          // 1. Soweit Lösen wie Möglich
          found := solve(rules, @LogEvent, MenuItem11.Checked, true); // Da die Regel evtl durch den Plausibilitätscheck fällt muss hier die Warnung bereits unterbunden werden.
          If Not found Then Begin
            LogEvent('Could not solve the puzzle, adding guess rule.');
            // 2. Die Vermutung anstellen und weiter Lösen
            (rules[high(Rules)] As TGuess).Active := true;
            // Was noch nicht sicher ist, ob geprüft werden muss, ob die Regel
            // einen Schaden Anrichtet oder nicht (wenn, dann fliegt die AV bei SetValue)
            found := solve(rules, @LogEvent, MenuItem11.Checked, true);
          End;
          If found Then break;
        End;
        If found Then break;
        m[y] := Nichts; // In dieser Teilmenge gabs keine Lösung, also in der Nächsten Suchen..
      End;
      If found Then break;
    End;
    If found Then Begin
      LogEvent('Attention, due to guessing the solution might not the one you''d expected.');
    End;
  End;
  If (Not IsSolved) And MenuItem13.Checked Then Begin // Da guessing und BruteForce sich ausschließen reicht diese Prüfung
    form2.ListBox1.items.Add('-- Enable bruteforce logic --');
    q := TFieldQueue.create;
    sq := TStringQueue.create;
    sq.Push(Form2.ListBox1.Items.Text);
    q.Push(clon(data));
    Free_(data);
    While (Not q.isempty) Do Begin
      data := q.Pop;
      Form2.ListBox1.Items.Text := sq.Pop;
      // 1. Versuchen zu Lösen
      found := solve(rules, @LogEvent, MenuItem11.Checked, true); // Da die Regel evtl durch den Plausibilitätscheck fällt muss hier die Warnung bereits unterbunden werden.
      If found Then Begin
        // Data ist Gültig, also die Queue leeren, damit die Ausgabe kommen kann
        While (Not q.isempty) Do Begin
          q.Pop;
        End;
        While (Not sq.isempty) Do Begin
          sq.Pop;
        End;
      End
      Else Begin
        // 2. Hat nicht Geklappt, das näcshte "Freie" Feld suchen und mit allen Kombinationen Puschen die es gibt.
        b := false;
        For i := 0 To EinsteinWidth - 1 Do Begin
          For j := 0 To EinsteinHeight - 1 Do Begin
            If Data[i, j].value = nichts Then Begin
              bkup := clon(data);
              free_(Data);
              For k := 0 To EinsteinWidth - 1 Do Begin
                If bkup[i, j].pencils[k] Then Begin
                  data := Clon(bkup);
                  s := Form2.ListBox1.Items.Text;
                  SetValue(i, j, k);
                  s := s + format('Set [%s, %s] = %s', [ResolveXY(0, i), ResolveXY(j, ueinstein.SetName), ResolveXY(j, k)]);
                  sq.Push(s);
                  q.Push(Clon(data));
                  free_(data);
                End;
              End;
              free_(bkup);
              b := true;
              break;
            End;
          End;
          If b Then break;
        End;
      End;
    End;
    sq.free;
    q.free;
  End;
  // Ausgabe
  // Regeln wieder Frei geben
  For i := 0 To high(rules) Do Begin
    rules[i].free;
  End;
  setlength(Rules, 0);

  StringGrid1.ColCount := EinsteinWidth + 1 + ord(MenuItem12.Checked);
  StringGrid1.RowCount := EinsteinHeight;
  For j := 0 To EinsteinHeight - 1 Do Begin
    StringGrid1.Cells[0, j] := ResolveXY(j, ueinstein.SetName);
    For i := 0 To EinsteinWidth - 1 Do Begin
      If assigned(data) Then Begin
        StringGrid1.Cells[i + 1, j] := ResolveXY(j, data[i, j].Value);
      End
      Else Begin
        StringGrid1.Cells[i + 1, j] := '';
      End;
    End;
  End;
  // Anzeigen der Mengeninhalte
  If MenuItem12.Checked Then Begin
    StringGrid1.Cells[StringGrid1.ColCount - 1, 0] := 'Sets';
    For j := 1 To EinsteinHeight - 1 Do Begin
      s := '';
      For i := 0 To EinsteinWidth - 1 Do Begin
        If s <> '' Then s := s + ', ';
        s := s + ResolveXY(j, i);
      End;
      StringGrid1.Cells[StringGrid1.ColCount - 1, j] := s;
    End;
  End;
  // Automatische Breitenanpassung
  For i := 0 To StringGrid1.ColCount - 1 Do Begin
    k := StringGrid1.Canvas.TextWidth(StringGrid1.Cells[i, 0]);
    For j := 0 To EinsteinHeight - 1 Do Begin
      k := max(k, StringGrid1.Canvas.TextWidth(StringGrid1.Cells[i, j]));
    End;
    If MenuItem8.Checked Then Begin
      StringGrid1.ColWidths[i] := k + 10 + 18;
    End
    Else Begin
      StringGrid1.ColWidths[i] := k + 10;
    End;
  End;
End;

Procedure TForm1.ApplicationProperties1Idle(Sender: TObject; Var Done: Boolean);
Var
  j: integer;
Begin
  If DeletionItem <> -1 Then Begin
    RuleList[DeletionItem].Free;
    For j := DeletionItem To high(RuleList) - 1 Do Begin
      RuleList[j] := RuleList[j + 1];
    End;
    SetLength(RuleList, high(RuleList));
    Scrollbar1.position := max(0, min(high(RuleList), Scrollbar1.position));
    FixRulePositions;
    DeletionItem := -1;
  End;
  done := true;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  SetLength(RuleList, high(RuleList) + 2);
  Case ComboBox1.ItemIndex Of
    // Alle 1 Set Regeln
    IndexDependsOn: Begin // Depends on
        RuleList[high(RuleList)] := TOneSetRuleFrame.Create(TabSheet7);
        RuleList[high(RuleList)].Rule := rsDependsOn;
      End;
    // Alle 2 Set Regeln
    IndexEliminate: Begin
        RuleList[high(RuleList)] := TTwoSetRuleFrame.Create(TabSheet7);
        RuleList[high(RuleList)].Rule := rsEliminate;
      End;
    IndexDistanceLeftOf: Begin
        RuleList[high(RuleList)] := TTwoSetAttribRuleFrame.Create(TabSheet7);
        RuleList[high(RuleList)].Rule := rsDistanceLeftOf;
        RuleList[high(RuleList)].SetAttributeCountAndLabels(['Distance']);
      End;
    IndexDistanceRightOf: Begin
        RuleList[high(RuleList)] := TTwoSetAttribRuleFrame.Create(TabSheet7);
        RuleList[high(RuleList)].Rule := rsDistanceRightOf;
        RuleList[high(RuleList)].SetAttributeCountAndLabels(['Distance']);
      End;
    IndexDistanceDependsOn: Begin
        RuleList[high(RuleList)] := TTwoSetAttribRuleFrame.Create(TabSheet7);
        RuleList[high(RuleList)].Rule := rsDistanceDependsOn;
        RuleList[high(RuleList)].SetAttributeCountAndLabels(['Distance']);
      End;
    IndexLeftOf: Begin
        RuleList[high(RuleList)] := TTwoSetRuleFrame.Create(TabSheet7);
        RuleList[high(RuleList)].Rule := rsLeftOf;
      End;
    IndexRightOf: Begin
        RuleList[high(RuleList)] := TTwoSetRuleFrame.Create(TabSheet7);
        RuleList[high(RuleList)].Rule := rsRightOf;
      End;
    IndexDistanceEliminate: Begin
        RuleList[high(RuleList)] := TTwoSetAttribRuleFrame.Create(TabSheet7);
        RuleList[high(RuleList)].Rule := rsDistanceEliminate;
        RuleList[high(RuleList)].SetAttributeCountAndLabels(['Distance']);
      End;
    IndexIntegerDepends: Begin
        RuleList[high(RuleList)] := TIntDepSetFrame.Create(TabSheet7);
        RuleList[high(RuleList)].Rule := rsIntDepends;
        //RuleList[high(RuleList)].SetAttributeCountAndLabels(['Distance']);
      End;
    IndexMultiEliminate: Begin
        RuleList[high(RuleList)] := TMultiSetFrame.Create(TabSheet7);
        RuleList[high(RuleList)].Rule := rsMultiEliminate;
      End
  Else Begin
      Raise exception.create('Error, missing implementation : ' + ComboBox1.Items[ComboBox1.ItemIndex]);
    End;
  End;
  // Callbacks aller
  RuleList[high(RuleList)].Parent := TabSheet7;
  RuleList[high(RuleList)].ResolveXY := @ResolveXY;
  RuleList[high(RuleList)].SelfDeleteCallback := @SelfDeleteCallback;

  // Neu Positionieren und Texte neu Einlesen
  ScrollBar1.Max := high(RuleList);
  ScrollBar1.Position := high(RuleList);

  FixRulePositions;
  RefreshTexts;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  // Es wird nur so komisch gescrollt, weil Lazarus heute den Senkrechten Scollbalken auf der Pagecontroll nicht mag, ka warum ..
  Scrollbar1.position := max(0, min(Scrollbar1.position + 1, High(RuleList)));
  FixRulePositions;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Begin
  // Es wird nur so komisch gescrollt, weil Lazarus heute den Senkrechten Scollbalken auf der Pagecontroll nicht mag, ka warum ..
  Scrollbar1.position := max(Scrollbar1.position - 1, 0);
  FixRulePositions;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Begin
  form4.show;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Var
  DataChanged: Boolean;
  i: integer;
Begin
  DataChanged := false;
  // Wurde eine Menge geändert ?
  For i := 0 To PageControl1.PageCount - 1 Do Begin
    DataChanged := DataChanged Or (PageControl1.Page[i].Components[0] As TSetframe).DataChanged;
  End;
  // Wurde eine Regel geändert ?
  For i := 0 To High(RuleList) Do Begin
    DataChanged := DataChanged Or RuleList[i].DataChanged;
  End;
  If DataChanged Then Begin
    If Application.MessageBox('Some settings have been changed, close without saving ?', 'Attention', mb_YesNo Or MB_ICONQUESTION) = ID_No Then Begin
      CanClose := False;
    End;
  End;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  defcaption := 'Einstein puzzle solver ver. 0.03 by Corpsman | www.Corpsman.de |';
  caption := defcaption;
  PageControl2.ActivePageIndex := 0;
  PageControl2.Align := alClient;
  Filename := '';
  StringGrid1.Color := clWhite;
  TabSheet6.OnHide := @OnHideSetPage;
End;

Procedure TForm1.FormShow(Sender: TObject);
Var
  s: tsetframe;
Begin
  If Form1ShowFirst Then Begin
    Form1ShowFirst := false;
    If FileExistsUTF8(ParamStrUTF8(1)) Then Begin
      LoadFromFile(ParamStrUTF8(1));
    End
    Else Begin
      (*
       * Initialisieren mit dem Klassischen Einstein Rätsel
       *)
      NewDimensions(6, 5);
      // Die Farben
      s := PageControl1.Pages[0].Components[0] As TSetframe;
      s.SetValues(['Places', '1', '2', '3', '4', '5']);

      s := PageControl1.Pages[1].Components[0] As TSetframe;
      s.SetValues(['Color', 'red', 'green', 'yellow', 'blue', 'white']);

      // Die Nationalitäten
      s := PageControl1.Pages[2].Components[0] As TSetframe;
      s.SetValues(['Nationality', 'Briton', 'Swedish', 'Dane', 'Norwegian', 'German']);

      // Die Getränke
      s := PageControl1.Pages[3].Components[0] As TSetframe;
      s.SetValues(['Drink', 'Tea', 'Coffee', 'Milk', 'Beer', 'Water']);

      // Die Zigaretten
      s := PageControl1.Pages[4].Components[0] As TSetframe;
      s.SetValues(['Smoke', 'Pallmall', 'Dunhill', 'Marlboro', 'Winfield', 'Rothmanns']);

      // Die Tiere
      s := PageControl1.Pages[5].Components[0] As TSetframe;
      s.SetValues(['Pet', 'Dog', 'Bird', 'Cat', 'Horse', 'Fish']);

      PageControl1.ActivePageIndex := 0; // Reset der Mengeneingabe Auswahl

      // Die Regeln

      ComboBox1.ItemIndex := IndexDependsOn; // Member
      button2.Click;
      RuleList[high(RuleList)].SetSet(0, TMS([Nichts, Rot, Brite, Nichts, Nichts, Nichts])); // Der Brite lebt im roten Haus.
      RuleList[high(RuleList)].SetUserHint(', The briton lifes in the red house.');

      ComboBox1.ItemIndex := IndexDependsOn; // Member
      button2.Click;
      RuleList[high(RuleList)].SetSet(0, TMS([Nichts, Nichts, Schwede, Nichts, Nichts, Hund])); // Der Schwede haelt sich einen Hund.
      RuleList[high(RuleList)].SetUserHint(', The Swedish owns the dog.');

      ComboBox1.ItemIndex := IndexDependsOn; // Member
      button2.Click;
      RuleList[high(RuleList)].SetSet(0, TMS([Nichts, Nichts, Daene, Tee, Nichts, Nichts])); // Der Daene trinkt gern Tee.
      RuleList[high(RuleList)].SetUserHint(', The Dane drinks tea.');

      ComboBox1.ItemIndex := IndexDistanceLeftOf; // Left of
      button2.Click;
      RuleList[high(RuleList)].SetSet(0, TMS([Nichts, Gruen, Nichts, Nichts, Nichts, Nichts])); // Das gruene Haus steht links neben dem weissen Haus.
      RuleList[high(RuleList)].SetSet(1, TMS([Nichts, Weiss, Nichts, Nichts, Nichts, Nichts])); // Das gruene Haus steht links neben dem weissen Haus.
      RuleList[high(RuleList)].SetAttribute(0, 1);
      RuleList[high(RuleList)].SetUserHint(', The green house is immediately to the right of the white house.');

      ComboBox1.ItemIndex := IndexDependsOn; // Member
      button2.Click;
      RuleList[high(RuleList)].SetSet(0, TMS([Nichts, Gruen, Nichts, Kaffee, Nichts, Nichts])); // Der Besitzer des gruenen Hauses trinkt Kaffee.
      RuleList[high(RuleList)].SetUserHint(', Coffee is drunk in the green house.');

      ComboBox1.ItemIndex := IndexDependsOn; // Member
      button2.Click;
      RuleList[high(RuleList)].SetSet(0, TMS([Nichts, Nichts, Nichts, Nichts, Pallmall, Vogel])); // Die Person,  die Pall Mall raucht, hat einen Vogel.
      RuleList[high(RuleList)].SetUserHint(', The Pallmall smoker owns the bird.');

      ComboBox1.ItemIndex := IndexDependsOn;
      button2.Click;
      RuleList[high(RuleList)].SetSet(0, TMS([2, Nichts, Nichts, Milch, Nichts, Nichts])); // Der Mann im mittleren Haus trinkt Milch.
      RuleList[high(RuleList)].SetUserHint(', Milk is drunk in the middle house.');

      ComboBox1.ItemIndex := IndexDependsOn; // Member
      button2.Click;
      RuleList[high(RuleList)].SetSet(0, TMS([Nichts, Gelb, Nichts, Nichts, Dunhill, Nichts])); // Der Bewohner des gelben Hauses raucht Dunhill.
      RuleList[high(RuleList)].SetUserHint(', Dunhills are smoked in the yellow house.');

      ComboBox1.ItemIndex := IndexDependsOn;
      button2.Click;
      RuleList[high(RuleList)].SetSet(0, TMS([0, Nichts, Norweger, Nichts, Nichts, Nichts])); // Der Norweger lebt im ersten Haus.
      RuleList[high(RuleList)].SetUserHint(', The Norwegian lives in the first house.');

      ComboBox1.ItemIndex := IndexDistanceDependsOn; // Neighbour of
      button2.Click;
      RuleList[high(RuleList)].SetSet(0, tms([Nichts, Nichts, Nichts, Nichts, Marlboro, Nichts])); // Der Marlboro-Raucher wohnt neben der Person mit der Katze.
      RuleList[high(RuleList)].SetSet(1, tms([Nichts, Nichts, Nichts, Nichts, Nichts, Katze])); // Der Marlboro-Raucher wohnt neben der Person mit der Katze.
      RuleList[high(RuleList)].SetAttribute(0, 1);
      RuleList[high(RuleList)].SetUserHint(', The man who smokes Marlboro lives in the house next to the man with the cat.');

      ComboBox1.ItemIndex := IndexDistanceDependsOn; // Neighbour of
      button2.Click;
      RuleList[high(RuleList)].SetSet(0, tms([Nichts, Nichts, Nichts, Nichts, Nichts, Pferd])); // Der Mann mit dem Pferd lebt neben der Person, die Dunhill raucht.
      RuleList[high(RuleList)].SetSet(1, tms([Nichts, Nichts, Nichts, Nichts, Dunhill, Nichts])); // Der Mann mit dem Pferd lebt neben der Person, die Dunhill raucht.
      RuleList[high(RuleList)].SetAttribute(0, 1);
      RuleList[high(RuleList)].SetUserHint(', Dunhills are smoked in the house next to the house where the horse is kept.');

      ComboBox1.ItemIndex := IndexDependsOn; // Member
      button2.Click;
      RuleList[high(RuleList)].SetSet(0, TMS([Nichts, Nichts, Nichts, Bier, Winfield, Nichts])); // Der Winfield-Raucher trinkt gern Bier.
      RuleList[high(RuleList)].SetUserHint(', The Winfield smoker drinks Beer.');

      ComboBox1.ItemIndex := IndexDistanceDependsOn; // Neighbour of
      button2.Click;
      RuleList[high(RuleList)].SetSet(0, tms([Nichts, Nichts, Norweger, Nichts, Nichts, Nichts])); // Der Norweger wohnt neben dem blauen Haus.
      RuleList[high(RuleList)].SetSet(1, tms([Nichts, Blau, Nichts, Nichts, Nichts, Nichts])); // Der Norweger wohnt neben dem blauen Haus.
      RuleList[high(RuleList)].SetAttribute(0, 1);
      RuleList[high(RuleList)].SetUserHint(', The Norwegian lives next to the blue house.');

      ComboBox1.ItemIndex := IndexDependsOn; // Member
      button2.Click;
      RuleList[high(RuleList)].SetSet(0, TMS([Nichts, Nichts, Deutsche, Nichts, Rothmanns, Nichts])); // Der Deutsche raucht Rothmanns.
      RuleList[high(RuleList)].SetUserHint(', The German smokes Rothmanns.');

      ComboBox1.ItemIndex := IndexDistanceDependsOn; // Neighbour of
      button2.Click;
      RuleList[high(RuleList)].SetSet(0, tms([Nichts, Nichts, Nichts, Nichts, Marlboro, Nichts])); // Der Marlboro-Raucher hat einen Nachbarn, der Wasser trinkt.
      RuleList[high(RuleList)].SetSet(1, tms([Nichts, Nichts, Nichts, Wasser, Nichts, Nichts])); // Der Marlboro-Raucher hat einen Nachbarn, der Wasser trinkt.
      RuleList[high(RuleList)].SetAttribute(0, 1);
      RuleList[high(RuleList)].SetUserHint(', The man who smokes Marlboro lifes next to the man who drinks water.');

      Scrollbar1.position := 0; // Nach ganz oben Scrollen
      FixRulePositions; // Anzeigen der Scrollposition
      ComboBox1.ItemIndex := 0; // Reset der Regelauswahl

      //  Button1.Click; -- Theoretisch könnte die Lösung zum start auch gleich berechnet werden, ..
    End;
  End;
End;

Procedure TForm1.MenuItem10Click(Sender: TObject);
Begin
  // Enable guessing
  MenuItem10.Checked := Not MenuItem10.Checked;
  If MenuItem10.Checked Then MenuItem13.Checked := false;
End;

Procedure TForm1.MenuItem11Click(Sender: TObject);
Begin
  // Add user hints to conclusions
  MenuItem11.Checked := Not MenuItem11.Checked;
End;

Procedure TForm1.MenuItem12Click(Sender: TObject);
Begin
  // show set contents on each row
  MenuItem12.Checked := Not MenuItem12.Checked;
End;

Procedure TForm1.MenuItem13Click(Sender: TObject);
Begin
  // Enable Brute Force
  MenuItem13.Checked := Not MenuItem13.Checked;
  If MenuItem13.Checked Then MenuItem10.Checked := false;
End;

Procedure TForm1.MenuItem2Click(Sender: TObject);
Begin
  form3.ModalResult := mrCancel;
  form3.edit1.text := inttostr(EinsteinHeight);
  form3.edit2.text := inttostr(EinsteinWidth);
  If form3.ShowModal = mrOK Then Begin
    NewDimensions(
      strtointdef(form3.edit1.text, EinsteinHeight),
      strtointdef(form3.edit2.text, EinsteinWidth)
      );
  End;
End;

Procedure TForm1.MenuItem3Click(Sender: TObject);
Begin
  // Load
  If Filename <> '' Then Begin
    OpenDialog1.InitialDir := ExtractFilePath(Filename);
  End
  Else Begin
    OpenDialog1.InitialDir := ExtractFilePath(ParamStrUTF8(0));
  End;
  If OpenDialog1.Execute Then Begin
    LoadFromFile(OpenDialog1.FileName);
  End;
End;

Procedure TForm1.MenuItem4Click(Sender: TObject);
Begin
  // Save
  If Filename <> '' Then Begin
    SavetoFile(Filename);
  End
  Else Begin
    MenuItem5Click(Nil);
  End;
End;

Procedure TForm1.MenuItem5Click(Sender: TObject);
Begin
  // Save as
  If Filename <> '' Then Begin
    SaveDialog1.InitialDir := ExtractFilePath(Filename);
  End
  Else Begin
    SaveDialog1.InitialDir := ExtractFilePath(ParamStrUTF8(0));
  End;
  If SaveDialog1.Execute Then Begin
    SavetoFile(SaveDialog1.Filename);
  End;
End;

Procedure TForm1.MenuItem6Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.MenuItem8Click(Sender: TObject);
Begin
  // Show pencils
  MenuItem8.Checked := Not MenuItem8.Checked;
  StringGrid1.Invalidate;
End;

Procedure TForm1.MenuItem9Click(Sender: TObject);
Begin
  form2.show;
End;

Procedure TForm1.OnHideSetPage(Sender: TObject);
Begin
  RefreshTexts; // Neu Einlesen der Texte beim Tabwechsel
End;

Procedure TForm1.ScrollBar1Change(Sender: TObject);
Begin
  FixRulePositions;
End;

Procedure TForm1.StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
  Procedure Plot(x, y: integer; Black: Boolean);
  Var
    c: TColor;
  Begin
    If Black Then
      c := clblack
    Else
      c := $BFBFBF;
    StringGrid1.Canvas.Pixels[x, y] := c;
    StringGrid1.Canvas.Pixels[x - 1, y] := c;
    StringGrid1.Canvas.Pixels[x + 1, y] := c;
    StringGrid1.Canvas.Pixels[x, y - 1] := c;
    StringGrid1.Canvas.Pixels[x, y + 1] := c;
  End;
Begin
  If MenuItem8.Checked Then Begin
    If (acol = 0) Or (arow = 0) Then exit;
    If (acol > length(data)) Or (aRow >= length(data[0])) Then exit;
    Case EinsteinWidth Of // Visualisierung der Pencils Hardcodiert Anhand der bisher benötigten Nummern, da fehlen also durchaus noch welche *g*
      3: Begin
          Plot((aRect.Right) + 0 - 14, (aRect.Top + aRect.Bottom) Div 2 - 6, data[aCol - 1, aRow].Pencils[0]);
          Plot((aRect.Right) - 6 - 14, (aRect.Top + aRect.Bottom) Div 2 + 6, data[aCol - 1, aRow].Pencils[1]);
          Plot((aRect.Right) + 6 - 14, (aRect.Top + aRect.Bottom) Div 2 + 6, data[aCol - 1, aRow].Pencils[2]);
        End;
      4: Begin
          Plot((aRect.Right) - 6 - 14, (aRect.Top + aRect.Bottom) Div 2 - 6, data[aCol - 1, aRow].Pencils[0]);
          Plot((aRect.Right) + 6 - 14, (aRect.Top + aRect.Bottom) Div 2 - 6, data[aCol - 1, aRow].Pencils[1]);
          Plot((aRect.Right) - 6 - 14, (aRect.Top + aRect.Bottom) Div 2 + 6, data[aCol - 1, aRow].Pencils[2]);
          Plot((aRect.Right) + 6 - 14, (aRect.Top + aRect.Bottom) Div 2 + 6, data[aCol - 1, aRow].Pencils[3]);
        End;
      5: Begin
          Plot((aRect.Right) - 6 - 14, (aRect.Top + aRect.Bottom) Div 2 - 6, data[aCol - 1, aRow].Pencils[0]);
          Plot((aRect.Right) + 6 - 14, (aRect.Top + aRect.Bottom) Div 2 - 6, data[aCol - 1, aRow].Pencils[1]);
          Plot((aRect.Right) + 0 - 14, (aRect.Top + aRect.Bottom) Div 2 - 0, data[aCol - 1, aRow].Pencils[2]);
          Plot((aRect.Right) - 6 - 14, (aRect.Top + aRect.Bottom) Div 2 + 6, data[aCol - 1, aRow].Pencils[3]);
          Plot((aRect.Right) + 6 - 14, (aRect.Top + aRect.Bottom) Div 2 + 6, data[aCol - 1, aRow].Pencils[4]);
        End;
      6: Begin
          Plot((aRect.Right) - 6 - 14, (aRect.Top + aRect.Bottom) Div 2 - 6, data[aCol - 1, aRow].Pencils[0]);
          Plot((aRect.Right) + 6 - 14, (aRect.Top + aRect.Bottom) Div 2 - 6, data[aCol - 1, aRow].Pencils[1]);
          Plot((aRect.Right) - 6 - 14, (aRect.Top + aRect.Bottom) Div 2 - 0, data[aCol - 1, aRow].Pencils[2]);
          Plot((aRect.Right) + 6 - 14, (aRect.Top + aRect.Bottom) Div 2 - 0, data[aCol - 1, aRow].Pencils[3]);
          Plot((aRect.Right) - 6 - 14, (aRect.Top + aRect.Bottom) Div 2 + 6, data[aCol - 1, aRow].Pencils[4]);
          Plot((aRect.Right) + 6 - 14, (aRect.Top + aRect.Bottom) Div 2 + 6, data[aCol - 1, aRow].Pencils[5]);
        End;
      7: Begin
          Plot((aRect.Right) - 6 - 14, (aRect.Top + aRect.Bottom) Div 2 - 6, data[aCol - 1, aRow].Pencils[0]);
          Plot((aRect.Right) + 6 - 14, (aRect.Top + aRect.Bottom) Div 2 - 6, data[aCol - 1, aRow].Pencils[1]);
          Plot((aRect.Right) - 6 - 14, (aRect.Top + aRect.Bottom) Div 2 - 0, data[aCol - 1, aRow].Pencils[2]);
          Plot((aRect.Right) + 6 - 14, (aRect.Top + aRect.Bottom) Div 2 - 0, data[aCol - 1, aRow].Pencils[3]);
          Plot((aRect.Right) - 6 - 14, (aRect.Top + aRect.Bottom) Div 2 + 6, data[aCol - 1, aRow].Pencils[4]);
          Plot((aRect.Right) + 6 - 14, (aRect.Top + aRect.Bottom) Div 2 + 6, data[aCol - 1, aRow].Pencils[5]);
          Plot((aRect.Right) + 0 - 14, (aRect.Top + aRect.Bottom) Div 2 + 0, data[aCol - 1, aRow].Pencils[6]);
        End;
      8: Begin
          Plot((aRect.Right) - 6 - 14, (aRect.Top + aRect.Bottom) Div 2 - 6, data[aCol - 1, aRow].Pencils[0]);
          Plot((aRect.Right) + 6 - 14, (aRect.Top + aRect.Bottom) Div 2 - 6, data[aCol - 1, aRow].Pencils[1]);
          Plot((aRect.Right) - 6 - 14, (aRect.Top + aRect.Bottom) Div 2 - 0, data[aCol - 1, aRow].Pencils[2]);
          Plot((aRect.Right) + 6 - 14, (aRect.Top + aRect.Bottom) Div 2 - 0, data[aCol - 1, aRow].Pencils[3]);
          Plot((aRect.Right) - 6 - 14, (aRect.Top + aRect.Bottom) Div 2 + 6, data[aCol - 1, aRow].Pencils[4]);
          Plot((aRect.Right) + 6 - 14, (aRect.Top + aRect.Bottom) Div 2 + 6, data[aCol - 1, aRow].Pencils[5]);
          Plot((aRect.Right) + 0 - 14, (aRect.Top + aRect.Bottom) Div 2 - 6, data[aCol - 1, aRow].Pencils[6]);
          Plot((aRect.Right) + 0 - 14, (aRect.Top + aRect.Bottom) Div 2 + 6, data[aCol - 1, aRow].Pencils[7]);
        End;
      9: Begin
          Plot((aRect.Right) - 6 - 14, (aRect.Top + aRect.Bottom) Div 2 - 6, data[aCol - 1, aRow].Pencils[0]);
          Plot((aRect.Right) - 6 - 14, (aRect.Top + aRect.Bottom) Div 2 - 0, data[aCol - 1, aRow].Pencils[1]);
          Plot((aRect.Right) - 6 - 14, (aRect.Top + aRect.Bottom) Div 2 + 6, data[aCol - 1, aRow].Pencils[2]);
          Plot((aRect.Right) + 0 - 14, (aRect.Top + aRect.Bottom) Div 2 - 6, data[aCol - 1, aRow].Pencils[3]);
          Plot((aRect.Right) + 0 - 14, (aRect.Top + aRect.Bottom) Div 2 + 0, data[aCol - 1, aRow].Pencils[4]);
          Plot((aRect.Right) + 0 - 14, (aRect.Top + aRect.Bottom) Div 2 + 6, data[aCol - 1, aRow].Pencils[5]);
          Plot((aRect.Right) + 6 - 14, (aRect.Top + aRect.Bottom) Div 2 - 6, data[aCol - 1, aRow].Pencils[6]);
          Plot((aRect.Right) + 6 - 14, (aRect.Top + aRect.Bottom) Div 2 - 0, data[aCol - 1, aRow].Pencils[7]);
          Plot((aRect.Right) + 6 - 14, (aRect.Top + aRect.Bottom) Div 2 + 6, data[aCol - 1, aRow].Pencils[8]);
        End;
    End;
  End;
End;

Procedure TForm1.FixRulePositions;
Var
  tl, i: integer;
Begin
  tl := 0;
  For i := 0 To high(RuleList) Do Begin
    If i >= Scrollbar1.position Then Begin
      RuleList[i].Visible := true;
      RuleList[i].name := 'Rule' + inttostr(i + 1);
      RuleList[i].Top := 50 + tl;
      RuleList[i].left := 10;
      RuleList[i].SetCaption(' Rule ' + inttostr(i + 1) + ' (' + RuleSelectorToString(RuleList[i].Rule) + ') ');
      tl := tl + RuleList[i].Height;
    End
    Else Begin
      RuleList[i].Visible := false;
    End;
  End;
End;

Function TForm1.ResolveXY(x, y: integer): String;
Var
  f: TSetframe;
Begin
  If y = Nichts Then Begin
    result := '-';
    exit;
  End;
  f := PageControl1.Pages[x].Components[0] As TSetframe;
  If y = ueinstein.SetName Then Begin
    result := f.Edit1.Text;
    exit;
  End;
  result := f.SetEdits[y].Text;
End;

Procedure TForm1.RefreshTexts;
Var
  i: Integer;
Begin
  For i := 0 To high(RuleList) Do Begin
    RuleList[i].RefreshTexts;
  End;
End;

Procedure TForm1.SelfDeleteCallback(Sender: TEinsteinRuleFrame);
Var
  i: integer;
Begin
  // Das Löschen muss so komisch gemacht werden, weil unter Windows sonst eine AV kommt
  // Der Eventhandler scheint noch auf das Freigegebene Element zugreifen zu wollen, das geht natürlich nicht
  // Im OnIdle wird nach DeleteItem geschaut und dann das entsprechende gelöscht
  If DeletionItem <> -1 Then exit;
  For i := 0 To high(RuleList) Do Begin
    If Sender = RuleList[i] Then Begin
      DeletionItem := i;
      exit;
    End;
  End;
End;

Procedure TForm1.SavetoFile(FName: String);
Var
  ini: Tinifile;
  f: TSetframe;
  i: Integer;
Begin
  // Vorher Löschen, das nur im File ist, was auch Rein gehört ..
  If FileExistsUTF8(FName) And (fname <> '') Then Begin
    If Not DeleteFileUTF8(FName) Then Begin
      ShowMessage('Error could not cleanup file : "' + Filename + '"' + LineEnding + 'Nothing stored.');
      exit;
    End;
  End;
  Filename := FName;
  ini := TIniFile.Create(FName);
  ini.WriteInteger('General', 'Setcount', EinsteinHeight);
  ini.WriteInteger('General', 'Elementcount', EinsteinWidth);
  For i := 0 To PageControl1.PageCount - 1 Do Begin
    f := TSetframe(PageControl1.Pages[i].Components[0]);
    f.SaveToFile(ini, i);
  End;
  ini.WriteInteger('Rules', 'Count', length(RuleList));
  For i := 0 To high(RuleList) Do Begin
    RuleList[i].SaveToFile(ini, i);
  End;
  ini.free;
  caption := defcaption + ' : ' + ExtractFileNameOnly(FName);
End;

Procedure TForm1.LoadFromFile(Fname: String);
Var
  ini: TIniFile;
  i, j: Integer;
  f: TSetframe;
  s: String;
Begin
  Filename := Fname;
  ini := TIniFile.Create(Fname);
  // Laden der Generellen Einstellungen
  EinsteinHeight := ini.ReadInteger('General', 'Setcount', EinsteinHeight);
  EinsteinWidth := ini.ReadInteger('General', 'Elementcount', EinsteinWidth);
  NewDimensions(EinsteinHeight, EinsteinWidth); // New, bzw anlegen der ganzen Platzhalter..
  // Laden der Mengen
  For i := 0 To PageControl1.PageCount - 1 Do Begin
    f := TSetframe(PageControl1.Pages[i].Components[0]);
    f.LoadFromFile(ini, i);
  End;
  // Laden der Regeln
  j := ini.ReadInteger('Rules', 'Count', 0);
  For i := 0 To j - 1 Do Begin
    s := ini.ReadString('Rule' + inttostr(i), 'Name', '');
    Case StringToRuleSelector(s) Of
      rsDependsOn: ComboBox1.ItemIndex := IndexDependsOn;
      rsDistanceDependsOn: ComboBox1.ItemIndex := IndexDistanceDependsOn;
      rsDistanceLeftOf: ComboBox1.ItemIndex := IndexDistanceLeftOf;
      rsDistanceRightOf: ComboBox1.ItemIndex := IndexDistanceRightOf;
      rsEliminate: ComboBox1.ItemIndex := IndexEliminate;
      rsLeftOf: ComboBox1.ItemIndex := IndexLeftOf;
      rsRightOf: ComboBox1.ItemIndex := IndexRightOf;
      rsDistanceEliminate: ComboBox1.ItemIndex := IndexDistanceEliminate;
      rsMultiEliminate: ComboBox1.ItemIndex := IndexMultiEliminate;
      rsIntDepends: ComboBox1.ItemIndex := IndexIntegerDepends;
    Else Begin
        Raise Exception.create('Error unknown rule : ' + s);
      End;
    End;
    Button2.Click; // Erzeugen der Geeigneten Regel
    RuleList[i].LoadFromFile(ini, i); // Dann Laden
  End;
  ini.free;
  // Alle Fenster auf init Zustand stellen
  ComboBox1.ItemIndex := 0;
  Scrollbar1.position := 0;
  FixRulePositions;
  RefreshTexts;
  PageControl2.ActivePageIndex := 0;
  PageControl1.ActivePageIndex := 0;
  caption := defcaption + ' : ' + ExtractFileNameOnly(Fname);

End;

Procedure TForm1.LogEvent(Logentry: String);
Begin
  form2.ListBox1.items.add(Logentry);
End;

Procedure TForm1.NewDimensions(SetCount, ElementPerSetCount: integer);
Var
  i, j: integer;
  t: TTabSheet;
  s: TSetframe;
Begin
  EinsteinHeight := SetCount;
  EinsteinWidth := ElementPerSetCount;
  setlength(data, EinsteinWidth, EinsteinHeight);
  For i := 0 To EinsteinWidth - 1 Do Begin
    For j := 0 To EinsteinHeight - 1 Do Begin
      setlength(data[i, j].Pencils, EinsteinWidth);
    End;
  End;
  // New
  // 1.1 Löschen der Grundmengen
  For i := 0 To PageControl1.PageCount - 1 Do Begin
    PageControl1.Pages[0].Free;
  End;
  // 1.2 Neu Erzeugen der Grundmengen Eingaben
  For i := 0 To EinsteinHeight - 1 Do Begin
    t := PageControl1.AddTabSheet;
    s := TSetframe.Create(t);
    s.parent := t;
    s.Align := alClient;
    s.FirstSet := i = 0;
    s.Edit1.OnChange(Nil);
  End;
  // 2.1 Löschen aller Regeln
  For i := 0 To High(RuleList) Do Begin
    RuleList[i].Free;
  End;
  setlength(RuleList, 0);
  // 3. Löschen der Ausgabe
  Button1.Click;
End;

End.

