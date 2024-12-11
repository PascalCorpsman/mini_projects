(******************************************************************************)
(* ALT_F2                                                          ??.??.???? *)
(*                                                                            *)
(* Version     : 0.49                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Application that calls other applications through shortcuts  *)
(*               and gives access to a simple calculater                      *)
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
(*               0.02 - GenMathCalc, kann nun unäres "Minus"                  *)
(*               0.03 - Hinzufügen weiterer Mathematischer Operanden (abs,    *)
(*                      sqr, sqrt), ändern der Bindungsstärke                 *)
(*               0.04 - Einfügen des "Edit" dialogs einer Eintragung          *)
(*               0.05 - Einfügen des Sortierens der Einträge nach             *)
(*                      Aufrufhäufigkeit                                      *)
(*               0.06 - Einfügen Ln Operator, Möglichkeit die Listbox         *)
(*                      Alphabetisch zu sortieren                             *)
(*               0.07 - Einfügen Option "Copy Result" des Taschenrechners.    *)
(*               0.08 - Bugfix mit Bindungsstärke "-"                         *)
(*               0.09 - Anzeige Fehlermeldung, wenn zu startende Anwendung    *)
(*                      nicht gefunden werden kann.                           *)
(*               0.10 - Integrieren in Mathsolver, dass bei nicht auswerten   *)
(*                      noch ein Versuch mit ")" gemacht wird.                *)
(*               0.11 - Einfügen "<<" und ">>" in den Mathesolver             *)
(*               0.12 - Einfügen "shl" und "shr" in den Mathesolver           *)
(*               0.13 - Versuch die immer wieder sporadisch aufkommenden      *)
(*                      Schreibfehler auf die .ini Datei ab zu fangen, ohne   *)
(*                      das das Tool abstürzt.                                *)
(*               0.14 - Einfügen Option "Delete" Zum entfernen eines Eintrages*)
(*                      aus der Liste                                         *)
(*               0.15 - Support für Hex/ Binärzahlen als Konstante Eingebbar  *)
(*               0.16 - Bugfix in Genmathcalt bei der Berechnung von 6-3-3 = 0*)
(*               0.17 - Bugfix AV beim Auswerten des Ausdrucks "=12:"         *)
(*                      Bugfix, verhindern Freigabe Nil Pointer               *)
(*               0.18 - Port to Linux Mate                                    *)
(*               0.19 - Unterstützung für 1 zeichen Links                     *)
(*                      Immer auf dem Screen auf dem die Maus grad ist öffnen.*)
(*               0.20 - Immer auf dem Screen auf dem die Maus grad ist öffnen *)
(*                      [optional deaktivierbar]                              *)
(*               0.21 - Skip Iconladen (global) auf Wunsch                    *)
(*               0.22 - Icons über interne Datenbank puffern.                 *)
(*               0.23 - Bei Multimonitorsystemen mit top <> 0 wurde die       *)
(*                      Anwendung evtl falsch positioniert.                   *)
(*               0.24 - Filtermaske im Import Icon Editor auf .bmp umgestellt.*)
(*               0.25 - Filtermaske im Load Dialog auf *.exe;*.bat umgestellt.*)
(*               0.26 - Aufsplittung Exec und Param unter Linux verbessert.   *)
(*               0.27 - Rechner hat nun auch mod                              *)
(*               0.28 - Mehrere Parameter unter Linux wieder aktiviert.       *)
(*               0.29 - Mathsolver aufgebohrt                                 *)
(*               0.30 - Optionale Ausgabe als hex, binärzahl von Int results  *)
(*                      Eintragen ins Ini aller Parametrisierbarer General    *)
(*                      Werte                                                 *)
(*               0.31 - Open containing Folder                                *)
(*               0.32 - Speichern letzten Pfad beim Edit-Image Dialog         *)
(*               0.33 - Bugfix für Linux, wenn Instanz Bereits geöffnet keine *)
(*                      Fehlermeldung mehr sondern öffnen der 1. Instanz      *)
(*               0.34 - Fix AV, when extracting a Icon from a non existing    *)
(*                      Application                                           *)
(*                      Parameter Support auch für Windows Aktiviert          *)
(*               0.35 - Abfangen AV beim Starten von Anwendungen              *)
(*                      Versuch Prozesse im Detached Mode laufen zu lassen    *)
(*                      Neuer Parser für Übergabeparameter                    *)
(*                      Drag Drop beim Anlegen neuer Anwendungen (Form2)      *)
(*               0.36 - Fix Bug, which triggers "add empty entry" when        *)
(*                      creating an entry via "Return" key in Form2           *)
(*               0.37 - Set Current Dir vor all Executables                   *)
(*               0.38 - remove PoDetached -> Fehler bei Consolenanwendungen   *)
(*               0.39 - F2-Shortcut                                           *)
(*               0.40 - Abfragen ob nicht existierender Eintrag Ausgeführt    *)
(*                      oder Angelegt werden soll.                            *)
(*               0.41 - Gruppieren der Binärzahlen in 4-Bit Nibbles           *)
(*               0.42 - Speedup dialog closing ( das .ini File war nicht      *)
(*                      gechached !)                                          *)
(*               0.43 - Fix AV on Entry creation (Linux Only)                 *)
(*               0.44 - Entfernen Abfrage Dialog von 0.40 -> erzeugt mehr     *)
(*                      Fehler als nutzen.                                    *)
(*               0.45 - Fix Bug, did not close when press "ESC"               *)
(*               0.46 - Tanh                                                  *)
(*               0.47 - Bessere Fehlermeldung in "Open Program folder"        *)
(*               0.48 - Hex Zahlen ebenfalls mit Nibbletrenner schreiben      *)
(*               0.49 - a^b für a < 0 und b ganzzahlig                        *)
(*                                                                            *)
(* Feature Request:     Ein "freifeld" mit dem man Infos zur Anwendung mit    *)
(*                      ablegen kann ??                                       *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

{.$DEFINE DEBUGG_MODE}
{$IFDEF Windows}
{.$DEFINE use_F1_instead_of_F2}// Some Users Need ALT + F1 instead of ALT + F2
{$ENDIF}

Interface

Uses
{$IFDEF Windows}
  ShellApi, windows, messages,
{$ENDIF}
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Buttons, umixfile,
  StdCtrls, types, ImgList, IniFiles, LCLIntf, Menus,
  umathsolver, UniqueInstance, math, Clipbrd;

Const
  ALT_F2_Version = '0.49';

Type

  TApp = Record
    AppName: String; // Der Name, Nach dem Gesucht wird
    RealName: String; // Der Aufrufparameter
    Icon: Integer; // Das Icon in der Imagelist
    Prio: integer; // Entspricht der Anzahl der Aufrufe, je mehr desto weiter Oben der Eintrag.
  End;

  TApps = Array Of TApp;

  { TForm1 }

  TForm1 = Class(TForm)
    Edit1: TEdit;
    ImageList1: TImageList;
    ListBox1: TListBox;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    UniqueInstance1: TUniqueInstance;
    Procedure Edit1KeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure Edit1KeyPress(Sender: TObject; Var Key: char);
    Procedure Edit1KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState
      );
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure ListBox1DblClick(Sender: TObject);
    Procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    Procedure ListBox1KeyPress(Sender: TObject; Var Key: char);
    Procedure ListBox1KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure ListBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure MenuItem10Click(Sender: TObject);
    Procedure MenuItem11Click(Sender: TObject);
    Procedure MenuItem12Click(Sender: TObject);
    Procedure MenuItem13Click(Sender: TObject);
    Procedure MenuItem14Click(Sender: TObject);
    Procedure MenuItem1Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure MenuItem3Click(Sender: TObject);
    Procedure MenuItem4Click(Sender: TObject);
    Procedure MenuItem5Click(Sender: TObject);
    Procedure MenuItem8Click(Sender: TObject);
    Procedure SpeedButton1Click(Sender: TObject);
    Procedure SpeedButton2Click(Sender: TObject);
    Procedure SpeedButton3Click(Sender: TObject);
    Procedure SpeedButton4Click(Sender: TObject);
    Procedure UniqueInstance1OtherInstance(Sender: TObject;
      ParamCount: Integer; Parameters: Array Of String);
  private
    { private declarations }
    Mixfile: TMixfile;
    Procedure Show_App(); // Zeigt das Fenster An
    Procedure Hide_App(); // Versteckt das Fenster wieder
    Procedure ShowDrob; // Richtet das Drop Down Menü ein
    Procedure LoadApps(); // Lädt alle Anwendungen aus der Ini Datei
    Procedure StoreApps(); // Speichert die Anwendungen in der ini Datei
    Function ExtractIcon(Filename: String): Integer; // Extrahiert das Icon aus der Datei, und Speichert es in der Imagelist ab, Rückgabe = Index in der Liste
    Procedure OpenApp(Filename: String); // Startet die Anwendung
    Procedure UpdateIniFile();
    Procedure CenterForm;
    Procedure WriteAllAvailableOptionsToIni();
  public
    { public declarations }
    Ini_File: Tinifile;
    Apps: TApps;
    Internal_ShowState: Boolean;
    Rechner: Boolean;
{$IFDEF Windows}
    id1: integer;
    Procedure WMHotKey(Var Msg: TMessage); message WM_HOTKEY;
{$ENDIF}
  End;

Var
  Form1: TForm1;
  def_Height: integer;
  Back_Color: TColor = Clblack;
  Font_Color: TColor = clWhite;
  Selected_Color: TColor = clNavy;
  OpenWhereMouseIs: Boolean = true; // Öffnet sich auf dem Monitor, auf welchem sich die Maus gerade befindet.

Implementation

{$R *.lfm}

Uses unit2, Unit3, lcltype, lazutf8, LazFileUtils, process, utf8process;

Type
  TExecParam = Record
    Executable: String;
    Params: Array Of String;
  End;

Function TryEvalString(Value: String; IntDetails: Boolean): String;
Begin
  If trim(value) = '' Then exit;
  value := StringReplace(value, ',', '.', [rfReplaceAll]); // Sicher ist sicher, falls der Text via COPY-Paste rein kam
  result := EvalString(value, IntDetails);
  If result = 'could not evaluate.' Then
    result := EvalString(value + ')', IntDetails);
End;

// Zum finden des Monitors auf dem sich die Maus befindet

Function PointInRect(P: TPoint; R: TRect): boolean;
Var
  t: Integer;
Begin
  If r.left > r.right Then Begin
    t := r.left;
    r.left := r.right;
    r.right := t;
  End;
  If r.top > r.bottom Then Begin
    t := r.Bottom;
    r.bottom := r.top;
    r.top := t;
  End;
  result := (r.left <= p.x) And (r.right >= p.x) And
    (r.top <= p.y) And (r.bottom >= p.y);
End;

{ TForm1 }

Procedure TForm1.CenterForm;
Var
  i: Integer;
  r: classes.Trect;
Begin
  If Not OpenWhereMouseIs Then exit;
  (*
  Bei einem Multimonitorsystem wollen wir die Anwendung immer da starten wo der Mauscursor ist.
  *)
  If screen.MonitorCount <> 1 Then Begin
    For i := 0 To screen.MonitorCount - 1 Do Begin
      r := screen.Monitors[i].BoundsRect;
      If PointInRect(Mouse.CursorPos, r) Then Begin
        left := (screen.Monitors[i].width - form1.width) Div 2 + screen.Monitors[i].BoundsRect.left;
        top := screen.Monitors[i].Top;
        break;
      End;
    End;
  End
  Else Begin
    left := (screen.width - form1.width) Div 2;
    top := 0;
  End;
End;

Procedure TForm1.WriteAllAvailableOptionsToIni();
Begin
  (*
   * Sorgt dafür das alle Parametrisierbaren Werte auch Sicher in der .ini Datei stehen, so das der User sie an oder Ab wählen kann
   *)
  Ini_File.WriteBool('General', 'OpenWhereMouseIs', Ini_File.ReadBool('General', 'OpenWhereMouseIs', false));
  Ini_File.WriteBool('General', 'SkipIcons', Ini_File.ReadBool('General', 'SkipIcons', false));
  Ini_File.WriteBool('General', 'ShowDetailedIntValues', Ini_File.ReadBool('General', 'ShowDetailedIntValues', false));
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  caption := 'ALT+F2';
  Ini_File := Nil;
  DefaultFormatSettings.DecimalSeparator := '.';
  LoadApps();
  Edit1.Text := '';
  MenuItem12.Visible := Ini_File.ReadBool('General', 'ShowDetailedIntValues', false);
  MenuItem13.Visible := Ini_File.ReadBool('General', 'ShowDetailedIntValues', false);
  listbox1.clear;
  Color := Back_Color;
  ListBox1.Color := Back_Color;
  ImageList1.DrawingStyle := dsTransparent;
  ListBox1.ScrollWidth := ListBox1.Width - 10; // Definitiv Deaktivieren des Horizontalen Scrollbalkens
  height := 38;
{$IFDEF Windows}
  // Register Hotkey ALT + F2
  id1 := GlobalAddAtom('Hotkey1');
{$IFDEF use_F1_instead_of_F2}
  RegisterHotKey(Handle, id1, MOD_ALT, VK_F1);
  MenuItem5.Caption := 'Close ALT+F1 tool';
{$ELSE}
  RegisterHotKey(Handle, id1, MOD_ALT, VK_F2);
{$ENDIF}
{$ENDIF}
  top := 0;
  left := (Screen.Width - Width) Div 2;
  Internal_ShowState := false;
{$IFDEF DEBUGG_MODE}
  Show_App();
{$ENDIF}
{$IFDEF Unix}
  Show_App();
{$ENDIF}
  def_Height := Height;
  Rechner := false;
End;

Procedure TForm1.FormShow(Sender: TObject);
Begin
  visible := Internal_ShowState;
End;

Procedure TForm1.ListBox1DblClick(Sender: TObject);
Var
  entry: integer;
  i: Integer;
Begin
  If Rechner Then exit;
  entry := -1;
  // Suchen des Auf zu Rufenden Eintrages
  If ListBox1.ItemIndex <> -1 Then Begin
    For i := 0 To high(apps) Do Begin
      If lowercase(ListBox1.Items[ListBox1.ItemIndex]) = lowercase(Apps[i].AppName) Then Begin
        entry := i;
        break;
      End;
    End;
  End
  Else Begin
    // Wir suchen ein Element haben aber die Liste noch nicht aufgebaut => das Element hat weniger als 2 Zeichen
    For i := 0 To high(apps) Do Begin
      If lowercase(Edit1.text) = lowercase(Apps[i].AppName) Then Begin
        entry := i;
        break;
      End;
    End;
  End;
  If entry = -1 Then Begin
    // Kein Eintrag gefunden, neu Anlegen
    form2.ModalResult := mrCancel;
    form2.Edit1.Text := Edit1.Text;
    form2.Edit2.Text := '';
    form2.ShowModal;
    If form2.ModalResult = mrOK Then Begin
      setlength(Apps, high(Apps) + 2);
      apps[high(Apps)].AppName := form2.Edit1.Text;
      apps[high(Apps)].RealName := form2.Edit2.Text;
      // Raus löschen der " ", falls mittels Pfad Kopieren eingefügt wurde.
      While pos('"', apps[high(Apps)].RealName) <> 0 Do Begin
        delete(apps[high(Apps)].RealName, pos('"', apps[high(Apps)].RealName), 1);
      End;
      apps[high(Apps)].Icon := ExtractIcon(apps[high(Apps)].RealName);
      apps[high(Apps)].Prio := 0;
      entry := high(Apps);
      StoreApps();
      UpdateIniFile();
    End;
  End;
  If entry <> -1 Then Begin
    //openurl(Apps[entry].RealName); // Das geht leider nicht, da es nicht den "Ausführen" in Pfad setzt.
    OpenApp(Apps[entry].RealName);
    Apps[entry].Prio := Apps[entry].Prio + 1;
    StoreApps();
    UpdateIniFile();
    // Eintrag gefunden, aufrufen, und Raus
    Hide_App();
  End;
End;

Procedure TForm1.ListBox1DrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
Var
  b: Boolean;
  i: Integer;
Begin
  // Hintergrund
  If (Rechner) Then Begin
    ListBox1.canvas.Brush.Color := Back_Color;
  End
  Else Begin
    If (index = ListBox1.ItemIndex) Then Begin
      ListBox1.canvas.Brush.Color := Selected_Color;
    End
    Else Begin
      ListBox1.canvas.Brush.Color := Back_Color;
    End;
  End;
  ListBox1.canvas.Pen.Color := Back_Color;
  ListBox1.canvas.Rectangle(ARect);
  // Das Passende Icon Suchen
  If Rechner Then Begin
    ImageList1.Draw(ListBox1.canvas, arect.Left + 5, arect.Top, 0);
  End
  Else Begin
    b := false;
    For i := 0 To high(Apps) Do
      If LowerCase(apps[i].AppName) = lowercase(ListBox1.items[Index]) Then Begin
        ImageList1.Draw(ListBox1.canvas, arect.Left + 5, arect.Top, apps[i].Icon);
        b := true;
        break;
      End;
    If Not b Then Begin // Das Bild wurde nicht gefunden, darf eigentlich nicht auftreten
      ImageList1.Draw(ListBox1.canvas, arect.Left + 5, arect.Top, 1);
    End;
  End;
  // Der Text
  ListBox1.canvas.Brush.Style := bsClear;
  ListBox1.canvas.Font.Color := Font_Color;
  ListBox1.canvas.TextOut(arect.Left + 32 + 10, arect.Top + 8, ListBox1.Items[Index]);
End;

Procedure TForm1.ListBox1KeyPress(Sender: TObject; Var Key: char);
Begin
  If key = #13 Then Begin
    ListBox1DblClick(Edit1);
  End;
End;

Procedure TForm1.ListBox1KeyUp(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If (key = VK_UP) And (ListBox1.ItemIndex = 0) Then Begin
    edit1.SetFocus;
  End;
  Invalidate;
End;

Procedure TForm1.ListBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  ListBox1.Invalidate;
End;

Procedure TForm1.MenuItem10Click(Sender: TObject);
Var
  s: String;
Begin
  // Copy Result
  s := TryEvalString(copy(Edit1.Text, 2, length(Edit1.Text)), Ini_File.ReadBool('General', 'ShowDetailedIntValues', false));
  If Ini_File.ReadBool('General', 'ShowDetailedIntValues', false) Then Begin
    If pos('(', s) <> 0 Then Begin
      s := trim(copy(s, 1, pos('(', s) - 1));
    End;
  End;
  Clipboard.AsText := s;
End;

Procedure TForm1.MenuItem12Click(Sender: TObject);
Var
  s: String;
Begin
  // Copy Hex Result
  s := TryEvalString(copy(Edit1.Text, 2, length(Edit1.Text)), Ini_File.ReadBool('General', 'ShowDetailedIntValues', false));
  If pos('(', s) <> 0 Then Begin
    s := copy(s, pos('(', s) + 1, length(s));
    s := copy(s, 1, pos('%', s) - 1);
    s := StringReplace(s, ' ', '', [rfReplaceAll]);
    Clipboard.AsText := s;
  End
  Else Begin
    Showmessage('Error, no hex value.');
  End;
End;

Procedure TForm1.MenuItem13Click(Sender: TObject);
Var
  s: String;
Begin
  // Copy Binary Result
  s := TryEvalString(copy(Edit1.Text, 2, length(Edit1.Text)), Ini_File.ReadBool('General', 'ShowDetailedIntValues', false));
  If pos('(', s) <> 0 Then Begin
    s := copy(s, pos('(', s) + 1, length(s));
    s := copy(s, pos('%', s) + 1, length(s));
    s := copy(s, 1, pos(')', s) - 1);
    s := StringReplace(s, ' ', '', [rfReplaceAll]);
    Clipboard.AsText := s;
  End
  Else Begin
    Showmessage('Error, no binary value.');
  End;
End;

Procedure TForm1.MenuItem14Click(Sender: TObject);
Var
  i: Integer;
  entry: Integer;
  fp: String;
Begin
  // Open Containing Folder
  If ListBox1.ItemIndex <> -1 Then Begin
    entry := -1;
    For i := 0 To high(apps) Do Begin
      If lowercase(ListBox1.Items[ListBox1.ItemIndex]) = lowercase(Apps[i].AppName) Then Begin
        entry := i;
        break;
      End;
    End;
    If (entry <> -1) Then Begin
      fp := ExtractFilePath(Apps[entry].RealName);
      If DirectoryExistsUTF8(fp) Then Begin
        OpenURL(fp);
      End
      Else Begin
        showmessage('Error: ' + fp + ' does not exist.');
      End;
    End
    Else Begin
      showmessage('Error could not find item.');
    End;
  End;
End;

Procedure TForm1.MenuItem11Click(Sender: TObject);
Var
  i: Integer;
  entry: Integer;
Begin
  // Edit
  If ListBox1.ItemIndex <> -1 Then Begin
    entry := -1;
    For i := 0 To high(apps) Do Begin
      If lowercase(ListBox1.Items[ListBox1.ItemIndex]) = lowercase(Apps[i].AppName) Then Begin
        entry := i;
        break;
      End;
    End;
    If (entry <> -1) Then Begin
      For i := entry To high(Apps) - 1 Do Begin
        Apps[i] := Apps[i + 1];
      End;
      setlength(Apps, high(Apps));
      StoreApps();
      UpdateIniFile();
    End
    Else Begin
      showmessage('Error could not find item.');
    End;
  End;
End;

Procedure TForm1.MenuItem1Click(Sender: TObject);
Var
  i: Integer;
  entry: Integer;
  Key: Word;
  tbm, b: Tbitmap;
  m: TMemoryStream;
Begin
  // Edit
  If ListBox1.ItemIndex <> -1 Then Begin
    entry := -1;
    For i := 0 To high(apps) Do Begin
      If lowercase(ListBox1.Items[ListBox1.ItemIndex]) = lowercase(Apps[i].AppName) Then Begin
        entry := i;
        break;
      End;
    End;
    If (entry <> -1) Then Begin
      form2.Edit1.Text := apps[entry].AppName;
      form2.Edit2.Text := apps[entry].RealName;
      form2.ModalResult := mrNone;
      Height := def_Height;
      form2.Button4.Visible := true;
      b := TBitmap.create;
      ImageList1.GetBitmap(apps[entry].Icon, b);
      form2.Image1.Picture.Assign(b);
      b.free;
      form2.Image1.Visible := true;
      form2.ShowModal;
      form2.Button4.Visible := false;
      form2.Image1.Visible := false;
      key := 0;
      Edit1KeyUp(Nil, Key, []);
      If form2.ModalResult = mrOK Then Begin
        If Not Ini_File.ReadBool('General', 'SkipIcons', false) Then Begin
          If apps[entry].RealName <> form2.Edit2.Text Then Begin
            // Entfernen des alten aus dem Mixfile
            Mixfile.deleteValue(apps[entry].RealName);
          End;
        End;
        apps[entry].AppName := form2.Edit1.Text;
        apps[entry].RealName := form2.Edit2.Text;
        // Das Bild im Mixfile Aktualsiieren
        If Not Ini_File.ReadBool('General', 'SkipIcons', false) Then Begin
          tbm := TBitmap.Create;
          tbm.assign(Form2.Image1.Picture);
          b := TBitmap.Create;
          b.Height := 32;
          b.Width := 32;
          b.canvas.StretchDraw(rect(0, 0, 32, 32), tbm);
          tbm.free;
          m := TMemoryStream.Create;
          b.SaveToStream(m);
          m.Position := 0;
          Mixfile.AddValue(apps[entry].RealName, m);
          b.TransparentColor := clFuchsia;
          b.Transparent := true;
          ImageList1.Replace(apps[entry].Icon, b, Nil);
          m.free;
          b.free;
        End;
        While pos('"', apps[entry].RealName) <> 0 Do Begin
          delete(apps[entry].RealName, pos('"', apps[entry].RealName), 1);
        End;
        StoreApps();
        UpdateIniFile();
      End;
    End
    Else Begin
      showmessage('Error could not find item.');
    End;
  End;
End;

Procedure TForm1.MenuItem2Click(Sender: TObject);
Var
  entry, i: Integer;
Begin
  // Reset Prio eines Einzeleintrages
  If ListBox1.ItemIndex <> -1 Then Begin
    entry := -1;
    For i := 0 To high(Apps) Do Begin
      If apps[i].AppName = ListBox1.items[ListBox1.ItemIndex] Then Begin
        entry := i;
        break;
      End;
    End;
    If entry <> -1 Then Begin
      apps[entry].Prio := 0;
      StoreApps();
      UpdateIniFile();
    End;
  End;
End;

Procedure TForm1.MenuItem3Click(Sender: TObject);
Var
  i: Integer;
Begin
  // Reset aller Prios
  For i := 0 To high(Apps) Do Begin
    Apps[i].Prio := 0;
  End;
  StoreApps();
  UpdateIniFile();
End;

Procedure TForm1.MenuItem4Click(Sender: TObject);
Begin
  // Insert As New Item
  ListBox1.ItemIndex := -1;
  ListBox1DblClick(Nil); // Nil, damit der Dialog zum Execute / Create Kommt
End;

Procedure TForm1.MenuItem5Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.MenuItem8Click(Sender: TObject);

  Procedure SortbyName(li, re: integer);
  Var
    p: String;
    l, r: Integer;
    s: String;
  Begin
    If Li < Re Then Begin
      p := lowercase(ListBox1.Items[Trunc((li + re) / 2)]);
      l := Li;
      r := re;
      While l < r Do Begin
        While CompareStr(lowercase(ListBox1.Items[l]), p) < 0 Do
          inc(l);
        While CompareStr(lowercase(ListBox1.Items[r]), p) > 0 Do
          dec(r);
        If L <= R Then Begin
          s := ListBox1.Items[l];
          ListBox1.Items[l] := ListBox1.Items[r];
          ListBox1.Items[r] := s;
          inc(l);
          dec(r);
        End;
      End;
      SortbyName(li, r);
      SortbyName(l, re);
    End;
  End;
Begin
  // Sort Alphabetically
  ListBox1.Items.BeginUpdate;
  SortbyName(0, ListBox1.Items.Count - 1);
  ListBox1.Items.EndUpdate;
End;

Procedure TForm1.SpeedButton1Click(Sender: TObject);
Begin
  form3.ShowModal;
  edit1.SetFocus;
End;

Procedure TForm1.SpeedButton2Click(Sender: TObject);
Begin
{$IFDEF Windows}
  OpenURL('taskmgr');
{$ENDIF}
{$IFDEF Unix}
  OpenApp('mate-system-monitor');
{$ENDIF}
  Hide_App();
End;

Procedure TForm1.SpeedButton3Click(Sender: TObject);
Var
  i: integer;
  Prios: Array Of integer;

  Procedure SortbyPrio(li, re: integer);
  Var
    l, r, h, p: Integer;
    s: String;
  Begin
    If Li < Re Then Begin
      p := prios[Trunc((li + re) / 2)]; // Auslesen des Pivo Elementes
      l := Li;
      r := re;
      While l < r Do Begin
        While prios[l] > p Do
          inc(l);
        While prios[r] < p Do
          dec(r);
        If L <= R Then Begin
          h := prios[l];
          s := ListBox1.Items[l];
          prios[l] := prios[r];
          ListBox1.Items[l] := ListBox1.Items[r];
          prios[r] := h;
          ListBox1.Items[r] := s;
          inc(l);
          dec(r);
        End;
      End;
      SortbyPrio(li, r);
      SortbyPrio(l, re);
    End;
  End;

Begin
  Rechner := false;
  ListBox1.Items.Clear;
  // Suchen aus der Liste und Anzeigen der Möglichen Ergebnisse
  setlength(Prios, high(apps) + 1);
  For i := 0 To high(Apps) Do Begin
    ListBox1.Items.Add(apps[i].AppName);
    prios[i] := apps[i].Prio;
  End;
  If ListBox1.Items.Count > 0 Then Begin
    ListBox1.TopIndex := 0;
    ListBox1.ItemIndex := 0;
    SortbyPrio(0, ListBox1.Items.Count - 1);
    setlength(Prios, 0);
  End;
  ListBox1.Invalidate;
  ShowDrob();
End;

Procedure TForm1.SpeedButton4Click(Sender: TObject);
Begin
  Hide_App();
End;

Procedure TForm1.UniqueInstance1OtherInstance(Sender: TObject;
  ParamCount: Integer; Parameters: Array Of String);
Begin
  Show_App();
End;

Procedure TForm1.Show_App();
Begin
  Internal_ShowState := true;
  If Not Visible Then Begin
    visible := true;
  End;
  Application.ProcessMessages;
  Edit1.SetFocus;
  Application.BringToFront;
  Application.ProcessMessages;
  Application.Restore;
  Application.ProcessMessages;
  Application.RestoreStayOnTop(true);
  CenterForm;
End;

Procedure TForm1.Hide_App();
Begin
{$IFDEF Unix}
  close;
  exit;
{$ENDIF}
{$IFDEF DEBUGG_MODE}
  close;
{$ELSE}
  // Ausblenden der Bisherigen Suchergebnisse
  Edit1.Text := '';
  ListBox1.Clear;
  ShowDrob;
  // Und Ausblenden der Anwendung
  Internal_ShowState := false;
  If Visible Then visible := false;
{$ENDIF}
End;

Procedure TForm1.ShowDrob;
Begin
  If ListBox1.Items.Count = 0 Then Begin
    Height := def_Height;
  End
  Else Begin
    Height := min(screen.Height Div 2, def_Height + 12 + 32 * ListBox1.Items.Count);
  End;
  ListBox1.Height := Height - 44;
End;

Procedure TForm1.LoadApps();
Var
  i: LongInt;
Begin
  Ini_File := TIniFile.Create(extractfilepath(ParamStrUTF8(0)) + 'ALT_F2.ini');
  Ini_File.CacheUpdates := true;
  WriteAllAvailableOptionsToIni();
  Mixfile := TMixfile.create(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStrUTF8(0))) + 'Icons.mix');
  i := Ini_File.ReadInteger('General', 'AppCount', 0);
  OpenWhereMouseIs := Ini_File.ReadBool('General', 'OpenWhereMouseIs', OpenWhereMouseIs);
  SetLength(Apps, i);
  For i := 0 To high(Apps) Do Begin
    apps[i].AppName := Ini_File.ReadString('App' + inttostr(i), 'SearchName', '');
    apps[i].RealName := Ini_File.ReadString('App' + inttostr(i), 'RealName', '');
    If pos('"', apps[i].RealName) > 1 Then Begin // RealName hat ein " aber nicht am Anfang oder Ende
      apps[i].RealName := '"' + apps[i].RealName + '"';
    End;
    apps[i].Prio := Ini_File.ReadInteger('App' + inttostr(i), 'Prio', 0);
    apps[i].Icon := ExtractIcon(apps[i].RealName);
  End;
End;

Procedure TForm1.StoreApps();
Var
  i: Integer;
Begin
  Ini_File.WriteInteger('General', 'AppCount', high(Apps) + 1);
  Ini_File.WriteBool('General', 'OpenWhereMouseIs', OpenWhereMouseIs);
  Ini_File.WriteBool('General', 'SkipIcons', Ini_File.ReadBool('General', 'SkipIcons', false));
  For i := 0 To high(Apps) Do Begin
    Ini_File.WriteString('App' + inttostr(i), 'SearchName', apps[i].AppName);
    Ini_File.WriteString('App' + inttostr(i), 'RealName', apps[i].RealName);
    Ini_File.WriteInteger('App' + inttostr(i), 'Prio', apps[i].Prio);
  End;
End;

Function TForm1.ExtractIcon(Filename: String): Integer;
Var
{$IFDEF Windows}
  LargeIco, SmallIco: hIcon;
  myIcon: TIcon;
{$ENDIF}
  m: TMemoryStream;
  b: TBitmap;
Begin
  result := 1; // Kann kein Icon Extrahiert werden wird es zum Zahnrad *g*
  If Ini_File.ReadBool('General', 'SkipIcons', false) Then exit;
  m := TMemoryStream.Create;
  If Mixfile.LoadValue(Filename, m) Then Begin
    b := TBitmap.Create;
    b.LoadFromStream(m);
    b.Transparent := true;
    result := ImageList1.Add(b, Nil);
    m.free;
    exit;
  End;
  m.free;
{$IFDEF Windows}
  If FileExistsUTF8(Filename) And (ExtractIconEx(PChar(FileName), 0, LargeIco, SmallIco, 1) > 0) Then Begin
    myIcon := TIcon.Create;
    Try
      myIcon.Transparent := True;
      myIcon.Masked := True;
      myIcon.Handle := LargeIco;
      result := ImageList1.AddIcon(myIcon);
      If (result = -1) Or (result >= ImageList1.Count) Then Begin
        // Wenn die Liste das Icon aus welchem Grund auch immer nicht adden konnte
        result := 1;
        b := TBitmap.Create;
        ImageList1.GetBitmap(result, b);
        m := TMemoryStream.Create;
        b.SaveToStream(m);
        m.Position := 0;
        Mixfile.AddValue(Filename, m);
        m.free;
        exit;
      End;
      b := TBitmap.Create;
      ImageList1.GetBitmap(result, b);
      m := TMemoryStream.Create;
      b.SaveToStream(m);
      m.Position := 0;
      Mixfile.AddValue(Filename, m);
      m.free;
    Finally
      myIcon.Free;
    End;
  End;
{$ENDIF}
  If result = 1 Then Begin // Wenn kein Icon angelegt werden konnte, dann hauen wir ein das Default Icon rein.
    b := TBitmap.Create;
    ImageList1.GetBitmap(result, b);
    m := TMemoryStream.Create;
    b.SaveToStream(m);
    m.Position := 0;
    Mixfile.AddValue(Filename, m);
    m.free;
  End;
End;

(*
 * Wandelt einen Kommandozeilenstring in eine Separate Liste der Parameter
 * und Anwendung um, berücksichtigt dabei "" zur Sicherung von Leerzeichen
 * es wird dabei davon ausgegangen, dass der Filename Prinzipiell i.o. ist
 *
 * Erlaubt sind:
 *  Anwendung "asd Sd"
 *  "Pfad/Anwendung" asd
 *  "Pfad/Anwendung" asd asd "asd" asd
 *
 * Alle Ergebnisse werden ohne " zurück gegeben
 *)

Function SplitFilenameToExecAndParam(Filename: String): TExecParam;
Var
  i: integer;
  at: String;
  inQuote: Boolean;
Begin
  result.Params := Nil;
  result.Executable := '';
  at := '';
  inQuote := false;
  Filename := Filename + ' '; // Anfügen eines abschließenden Leerzeichens, dass auch der letzte Parameter sicher erkannt wird !
  For i := 1 To length(Filename) Do Begin
    If Filename[i] = '"' Then Begin
      inQuote := Not inQuote;
    End
    Else Begin
      If inQuote Then Begin
        at := at + Filename[i];
      End
      Else Begin
        If Filename[i] = ' ' Then Begin
          If result.Executable = '' Then Begin
            result.Executable := at;
          End
          Else Begin
            setlength(result.Params, high(Result.Params) + 2);
            result.Params[high(Result.Params)] := at;
          End;
          at := '';
        End
        Else Begin
          at := at + Filename[i];
        End;
      End;
    End;
  End;
End;

Procedure TForm1.OpenApp(Filename: String);
Var
  p: TProcessUTF8;
  ep: TExecParam;
  i: Integer;
Begin
  p := TProcessUTF8.Create(Nil);
{$IFDEF WINDOWS}
  //p.Options := [poDetached, poNewProcessGroup];
  p.Options := [poNewProcessGroup]; //  Removed poDetached -> Macht unter Windows Probleme mit Consolen Anwendungen
{$ELSE}
  p.Options := [];
{$ENDIF}
  ep := SplitFilenameToExecAndParam(Filename);
  p.Executable := ep.Executable;
  p.CurrentDirectory := ExtractFilePath(ep.Executable);
  For i := 0 To high(ep.Params) Do Begin
    p.Parameters.Add(ep.Params[i]);
  End;
  Try
    p.Execute;
  Except
    On av: Exception Do Begin
      showmessage('Error on executing: ' + ep.Executable + LineEnding +
        'Error Message: ' + av.Message);
    End;
  End;
  p.free;
End;

Procedure TForm1.UpdateIniFile();
Begin
  If assigned(Ini_File) Then Begin
    Try
      Ini_File.UpdateFile; // Durch schreiben auf die Platte
    Except
      showmessage('Alt+F2 tool, error, while updating userdata logfile.');
    End;
  End
  Else Begin
    showmessage('Error userdata logfile not set.');
  End;
End;

{$IFDEF Windows}

Procedure TForm1.WMHotKey(Var Msg: TMessage);
Begin
  Show_App();
End;
{$ENDIF}

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  Ini_File.free;
  Ini_File := Nil;
  Mixfile.Free;
  Mixfile := Nil;
{$IFDEF Windows}
  UnRegisterHotKey(Handle, id1);
  GlobalDeleteAtom(id1);
{$ENDIF}
End;

Procedure TForm1.Edit1KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
Var
  i: Integer;
  prios: Array Of Integer;

  Procedure SortbyPrio(li, re: integer);
  Var
    l, r, h, p: Integer;
    s: String;
  Begin
    If Li < Re Then Begin
      p := prios[Trunc((li + re) / 2)]; // Auslesen des Pivo Elementes
      l := Li;
      r := re;
      While l < r Do Begin
        While prios[l] > p Do
          inc(l);
        While prios[r] < p Do
          dec(r);
        If L <= R Then Begin
          h := prios[l];
          s := ListBox1.Items[l];
          prios[l] := prios[r];
          ListBox1.Items[l] := ListBox1.Items[r];
          prios[r] := h;
          ListBox1.Items[r] := s;
          inc(l);
          dec(r);
        End;
      End;
      SortbyPrio(li, r);
      SortbyPrio(l, re);
    End;
  End;
Begin
  If Key = VK_ESCAPE Then Begin
    Hide_App();
    exit;
  End;
  (*
   * Wenn der User nen Neuen Datensatz anlegt, und im "Anlegen" dialog den FileLink mittels Return bestätigt
   * dann kommt der Fokus hier her zurück und der "Up" event feuert mit dem "Falschen" return.
   * Da das Edit1.text zu dem Zeitpunkt aber immer "Leer" ist (weil der App_hide Code der das neu angelegte element
   * aufruft es löscht), läst sich das hier zum Glück recht elegant abfangen ;)
   *)
  If trim(Edit1.Text) = '' Then exit;
  If key = VK_RETURN Then Begin
    Key := 0;
    ListBox1DblClick(Edit1);
    exit;
  End;
  Rechner := false;
  If (length(Edit1.Text) > 0) And (Edit1.Text[1] = '=') Then Begin
    Rechner := True;
    // Der Taschenrechner Modus
    If ListBox1.Items.Count <> 1 Then Begin
      ListBox1.Items.Clear;
      ListBox1.Items.Add('');
    End;
    // Ausrechnen was im Combobox Text Steht.
    listbox1.items[0] := TryEvalString(copy(Edit1.Text, 2, length(Edit1.Text)), Ini_File.ReadBool('General', 'ShowDetailedIntValues', false));
    ListBox1.ItemIndex := -1;
  End
  Else Begin
    // Der Normale Modus
    Rechner := false;
    ListBox1.Items.Clear;
    setlength(prios, high(Apps) + 1);
    // Suchen aus der Liste und Anzeigen der Möglichen Ergebnisse
    If length(Edit1.Text) > 1 Then Begin // Ab 2 Buchstaben soll gesucht werden.
      For i := 0 To high(Apps) Do Begin
        If pos(lowercase(Edit1.Text), lowercase(apps[i].AppName)) <> 0 Then Begin
          prios[ListBox1.Items.Count] := apps[i].Prio;
          ListBox1.Items.Add(apps[i].AppName);
        End;
      End;
    End;
    If ListBox1.Items.Count > 0 Then Begin
      setlength(prios, ListBox1.Items.Count);
      SortbyPrio(0, ListBox1.Items.Count - 1);
      ListBox1.ItemIndex := 0;
    End;
    setlength(prios, 0);
  End;
  MenuItem9.Visible := Rechner;
  MenuItem10.Visible := Rechner;
  ShowDrob();
End;

Procedure TForm1.Edit1KeyPress(Sender: TObject; Var Key: char);
Begin
  If rechner And (key = ',') Then Begin
    key := '.';
  End;
End;

Procedure TForm1.Edit1KeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState
  );
Begin
  If key = VK_DOWN Then Begin
    If Height > def_Height Then Begin
      ListBox1.SetFocus;
    End;
  End;
  If key = VK_F2 Then Begin
    If Height > def_Height Then Begin
      ListBox1.SetFocus;
      MenuItem1Click(Nil);
    End;
  End;
End;

End.

