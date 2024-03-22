(******************************************************************************)
(* HeapSim                                                         04.09.2015 *)
(*                                                                            *)
(* Version     : 0.03                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : This Programms implements a test framework to evaluate       *)
(*               different heap implementation strategies, as well as to      *)
(*               evaluate different allokation strategies in the daily        *)
(*               implementation of your programs.                             *)
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
(*               0.02 - Verbesserungen in TBitVectorHeap                      *)
(*               0.03 - Fix Div by 0 errors on very short simulations         *)
(*                                                                            *)
(******************************************************************************)
(*
 *
 * http://www.lowlevel.eu/wiki/Heapverwaltung
 * https://de.wikipedia.org/wiki/Garbage_Collection
 *
 *)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, TAGraph, TASeries, EpikTimer, Forms, Controls,
  Graphics, Dialogs, StdCtrls, ComCtrls, ExtCtrls, ColorBox, Menus, ubaseHeap,
  TACustomSeries, TATransformations;

Const
  _1MB = 1024 * 1024; // Kleinste Größe für ein

  (*
   * Der Bitvektor Heap
   * Nachteile
   *  - ca. 8 % des Speichers gehen durch das Bitfeld verloren.
   *  - Lineare Suche nach Freien Blöcken
   * Vorteile
   *  - Keine Freispeicherliste -> Kein ResortFreeList notwendig
   *)
  BitVecH = 'BitVectorHeap'; // Fertig - Implementiert
  (*
   * Der Buddy Heap versucht die Nachteile von Bitvektorheap zu minimieren
   * in dem die Suche durch einen Binärbaum ersetzt wird. Dies Kostet aber noch
   * mehr speicher und erhöht die Kleinstmögliche Allokierbare Blockgröße
   * drastisch.
   *)
  BuddyH = 'Buddy Heap';
  (*
   * Alle Nachfolgenden Heaps sind Freispeicherlisten Heaps.
   * Sie unterscheiden sich lediglich in der Methode, nach derer neue Speicher
   * blöcke Allokiert werden.
   *)
  FirstFitH = 'First fit Heap'; // Fertig - Implementiert
  NextFitH = 'Next fit Heap'; // Fertig - Implementiert
  BestFitH = 'Best fit Heap'; // Fertig - Implementiert
  WorstFitH = 'Worst fit Heap'; // Fertig - Implementiert
  (*
   * Wrapper für die FPC Heap Implementierung
   *)
  FPCH = 'FPC Heap';

  (* aus dem Compilerbau Skript :
  Lösung 1:
    Versuche Fragmentierung durch Vergabestrategie zu minimieren

  Lösung 2:
    Freie Speicherblöcke zusammenschieben („Kompaktifizierung“)

    • zu Lösung 1: Suche eines passenden Blocks in der Freispeicherliste nach
      • best-fit Methode: Auswahl des Blocks mit dem kleinsten Überschuss
        ⇒ kleine, unverwendbare Restblöcke bleiben über
        ⇒ Suche nach best-fit ist kostspielig
      • first-fit Methode: Auswahl des ersten Blocks mit ausreichender Größe
        ⇒ kleine Teile akkumulieren am Beginn der Liste (degenerierende
          Suchzeiten)
      • next-fit Methode: Freispeicherliste ist Ringliste; der „Anfang“ rotiert in der Liste; sonst wie first-fit
         ⇒ verteilt die kleinen Reste über die Halde
      • buddy Methode: suche nach genauer Größe; wenn nicht vorhanden, zerteile einen Block mit (z.B.) doppelter Größe
         ⇒ größere Hoffnung auf spätere Vergabe des Rests
      • worst-fit Methode: Auswahl des Blocks mit größtem Überschuss

  Bei allen diesen Varianten:
  • Es gibt Beispiele für die Überlegenheit der jeweiligen Strategie gegenüber
    allen anderen!
  • Es gilt das Knuth‘sche Gesetz, dass auf Dauer diese Strategien zu einem
    Zustand führen, in dem belegte zu freien Blöcken im Verhältnis 2:1 stehen.

               D.h. Fragmentierung ist so nicht vermeidbar!

    empirisch: next-fit oder first-fit noch „am besten“

    • zu Lösung 2:
      • teilweise Kompaktifizierung: Vereinigung aneinandergrenzender freier Speicherblöcke
        ⇒ teuer mit Freispeicherliste (Sortieren notwendig)
          einfach mit BitvektorImplementierung
      • vollständige Komapktifizierung:
        Zusammenschieben aller belegten Blöcke
        Schwierigkeit: Anpassung von Zeigern auf verschobene Bereiche (siehe
                       Garbage Collection wegen Erkennung der Zeiger)           !! Würde nur mit Trampolin pointern gehen !!

  *)

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button10: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Chart2: TChart;
    Chart3: TChart;
    Chart4: TChart;
    Chart4BarSeries1: TBarSeries;
    ChartAxisTransformations1: TChartAxisTransformations;
    ChartAxisTransformations1LogarithmAxisTransform1: TLogarithmAxisTransform;
    CheckBox1: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    ColorBox1: TColorBox;
    ColorBox2: TColorBox;
    ColorBox3: TColorBox;
    ColorBox4: TColorBox;
    ColorBox5: TColorBox;
    ColorBox6: TColorBox;
    ColorBox7: TColorBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Edit1: TEdit;
    Edit10: TEdit;
    Edit11: TEdit;
    Edit12: TEdit;
    Edit13: TEdit;
    Edit14: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    EpikTimer1: TEpikTimer;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    GroupBox5: TGroupBox;
    GroupBox6: TGroupBox;
    GroupBox7: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    RadioButton3: TRadioButton;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    SaveDialog3: TSaveDialog;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    Procedure Button1Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure Button8Click(Sender: TObject);
    Procedure Button9Click(Sender: TObject);
    Procedure Chart1DblClick(Sender: TObject);
    Procedure CheckBox1Change(Sender: TObject);
    Procedure CheckBox2Change(Sender: TObject);
    Procedure ComboBox1KeyPress(Sender: TObject; Var Key: char);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure MenuItem1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Procedure ExportChartAsCSV(Const Chart: TChart);
    Procedure ExportChartAsBitmap(Const Chart: TChart);
  End;

Var
  Form1: TForm1;
  Heap: TBaseHeap = Nil; // Der Heap mit dem Gearbeitet werden soll
  HeapSize: int64; // Die vom User gewählte HeapGröße, welche zur Verfügung steht.
  Panik: Boolean; // Wenn True, dann werden alle Operationen schnellstmöglich abgebrochen und die Simulation beendet.
  Version: String = ''; // Versionsstring wird in Form Create gesetzt.

Implementation

Uses ubitvectorheap, ufirstfitheap, ubestfitheap, unextfitheap, ubuddyheap,
  uworstfitheap, ufpcheap, ureport, LCLIntf, math, LazUTF8, IniFiles;

{$R *.lfm}

// Liefert eine Zufallszahl in [Min_ .. Max_]

Function RandomRange(Min_, Max_: integer): Integer;
Var
  d: integer;
Begin
  d := max_ - min_;
  result := random(d + 1) + min_;
End;

{ TForm1 }

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  HeapSize := StrToInt64Def(edit1.text, -1) * _1MB;
  //  HeapSize := 128; // debug
  If (HeapSize <= 0) Or (HeapSize > 2 Shl 31) Then Begin
    ShowMessage('Error invalid heap size : ' + Edit1.Text);
    exit;
  End;
  Case ComboBox1.Text Of
    BitVecH: Begin
        Heap := TBitVectorHeap.Create(HeapSize);
      End;
    BestFitH: Begin
        heap := TBestFitHeap.Create(HeapSize);
      End;
    WorstFitH: Begin
        heap := TWorstFitHeap.Create(HeapSize);
      End;
    FirstFitH: Begin
        heap := TFirstFitHeap.Create(HeapSize);
      End;
    NextFitH: Begin
        heap := TNextFitHeap.Create(HeapSize);
      End;
    BuddyH: Begin
        // Alle Zweierpotenzen sind Binär im Format "100..00" zieht man
        // eins ab kommt "011..11" raus. Verundet muss das 0 Ergeben,
        // wenn nicht wars keine Zweierpotenz ;)
        If ((HeapSize And (HeapSize - 1)) = 0) Then Begin
          heap := TBuddyHeap.create(HeapSize);
        End
        Else Begin
          ShowMessage('Error Heapsize need to be power of 2');
        End;
      End;
    FPCH: Begin
        heap := TFPCHeap.Create(HeapSize);
      End;
  End;
  If assigned(heap) Then Begin
    Button1.Enabled := false;
    Button5.Enabled := false;
    Button6.Enabled := true;
    Edit1.Enabled := false;
    ComboBox1.Enabled := false;
  End
  Else Begin
    showmessage('Error could not create : ' + ComboBox1.Text);
  End;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  // Free Heap
  If assigned(Heap) Then Heap.free;
  heap := Nil;
  Button1.Enabled := true;
  Edit1.Enabled := true;
  ComboBox1.Enabled := true;
  Button6.Enabled := false;
  Button5.Enabled := True;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Var
  ini: tinifile;
Begin
  // Speichern aller Einstellungen
  If SaveDialog1.Execute Then Begin
    ini := TIniFile.Create(SaveDialog1.FileName);
    ini.WriteString('Heap Properties', 'Strategy', ComboBox1.text);
    ini.WriteString('Heap Properties', 'Size', Edit1.text);
    ini.WriteString('Stress scenarios', 'Repeat', edit2.text);
    ini.WriteString('Stress scenarios', 'Seed', edit10.text);
    If RadioButton1.Checked Then Begin
      ini.Writeinteger('Stress scenarios', 'Methode', 0);
    End
    Else Begin
      If RadioButton2.Checked Then Begin
        ini.Writeinteger('Stress scenarios', 'Methode', 1);
      End
      Else Begin
        ini.Writeinteger('Stress scenarios', 'Methode', 2);
      End;
    End;
    ini.WriteString('Random blocks', 'Min', edit3.text);
    ini.WriteString('Random blocks', 'Max', edit4.text);
    ini.WriteString('Random blocks', 'Min livetime', edit8.text);
    ini.WriteString('Random blocks', 'Max livetime', edit9.text);
    ini.WriteString('Array sim', 'Min', edit5.text);
    ini.WriteString('Array sim', 'Max', edit6.text);
    ini.WriteString('Array sim', 'Size', edit7.text);
    ini.WriteString('Alternating alloc', 'List', edit13.text);
    ini.WriteString('Alternating alloc', 'Min livetime', edit11.text);
    ini.WriteString('Alternating alloc', 'Max livetime', edit12.text);

    ini.WriteString('Plot settings', 'Name', edit14.text);
    ini.WriteBool('Plot settings', 'Use filling', CheckBox4.Checked);
    ini.WriteString('Plot settings', 'filling Color', ColorToString(ColorBox4.Selected));
    ini.WriteBool('Plot settings', 'Use clipping', CheckBox7.Checked);
    ini.WriteString('Plot settings', 'clipping Color', ColorToString(ColorBox5.Selected));
    ini.WriteBool('Plot settings', 'Use absolute', CheckBox3.Checked);
    ini.WriteString('Plot settings', 'absolute Color', ColorToString(ColorBox1.Selected));
    ini.WriteBool('Plot settings', 'Use getmem', CheckBox5.Checked);
    ini.WriteString('Plot settings', 'getmem Color', ColorToString(ColorBox2.Selected));
    ini.WriteBool('Plot settings', 'Use freemem', CheckBox6.Checked);
    ini.WriteString('Plot settings', 'freemem Color', ColorToString(ColorBox3.Selected));
    ini.WriteBool('Plot settings', 'Use barcolor', CheckBox8.Checked);
    ini.WriteString('Plot settings', 'bar Color', ColorToString(ColorBox6.Selected));
    ini.WriteBool('Plot settings', 'Use sum', CheckBox9.Checked);
    ini.WriteString('Plot settings', 'sum Color', ColorToString(ColorBox7.Selected));

    ini.free;
  End;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Var
  ini: tinifile;
  i: integer;
Begin
  // Laden aller Einstellungen
  If OpenDialog1.Execute Then Begin
    ini := TIniFile.Create(OpenDialog1.FileName);

    ComboBox1.text := ini.ReadString('Heap Properties', 'Strategy', ComboBox1.text);
    Edit1.text := ini.ReadString('Heap Properties', 'Size', Edit1.text);
    edit2.text := ini.ReadString('Stress scenarios', 'Repeat', edit2.text);
    edit10.text := ini.ReadString('Stress scenarios', 'Seed', edit10.text);
    i := ini.Readinteger('Stress scenarios', 'Methode', 0);
    (FindComponent('RadioButton' + inttostr(i + 1)) As TRadioButton).Checked := true;

    edit3.text := ini.ReadString('Random blocks', 'Min', edit3.text);
    edit4.text := ini.ReadString('Random blocks', 'Max', edit4.text);
    edit8.text := ini.ReadString('Random blocks', 'Min livetime', edit8.text);
    edit9.text := ini.ReadString('Random blocks', 'Max livetime', edit9.text);
    edit5.text := ini.ReadString('Array sim', 'Min', edit5.text);
    edit6.text := ini.ReadString('Array sim', 'Max', edit6.text);
    edit7.text := ini.ReadString('Array sim', 'Size', edit7.text);
    edit13.text := ini.ReadString('Alternating alloc', 'List', edit13.text);
    edit11.text := ini.ReadString('Alternating alloc', 'Min livetime', edit11.text);
    edit12.text := ini.ReadString('Alternating alloc', 'Max livetime', edit12.text);

    edit14.text := ini.ReadString('Plot settings', 'Name', edit14.text);
    CheckBox4.Checked := ini.ReadBool('Plot settings', 'Use filling', CheckBox4.Checked);
    ColorBox4.Selected := StringToColor(ini.ReadString('Plot settings', 'filling Color', ColorToString(ColorBox4.Selected)));
    CheckBox7.Checked := ini.ReadBool('Plot settings', 'Use clipping', CheckBox7.Checked);
    ColorBox5.Selected := StringToColor(ini.ReadString('Plot settings', 'clipping Color', ColorToString(ColorBox5.Selected)));
    CheckBox3.Checked := ini.ReadBool('Plot settings', 'Use absolute', CheckBox3.Checked);
    ColorBox1.Selected := StringToColor(ini.ReadString('Plot settings', 'absolute Color', ColorToString(ColorBox1.Selected)));
    CheckBox5.Checked := ini.ReadBool('Plot settings', 'Use getmem', CheckBox5.Checked);
    ColorBox2.Selected := StringToColor(ini.ReadString('Plot settings', 'getmem Color', ColorToString(ColorBox2.Selected)));
    CheckBox6.Checked := ini.ReadBool('Plot settings', 'Use freemem', CheckBox6.Checked);
    ColorBox3.Selected := StringToColor(ini.ReadString('Plot settings', 'freemem Color', ColorToString(ColorBox3.Selected)));
    CheckBox8.Checked := ini.ReadBool('Plot settings', 'Use barcolor', CheckBox8.Checked);
    ColorBox6.Selected := StringToColor(ini.ReadString('Plot settings', 'bar Color', ColorToString(ColorBox6.Selected)));
    CheckBox9.Checked := ini.ReadBool('Plot settings', 'Use sum', CheckBox9.Checked);
    ColorBox7.Selected := StringToColor(ini.ReadString('sum settings', 'bar Color', ColorToString(ColorBox7.Selected)));

    ini.free;
  End;
End;

Procedure TForm1.Button6Click(Sender: TObject);
Type
  TSelector = (sRandomBlocks, sArraySim, sAlternatings);

  TBlock = Record
    CreateIteration: integer;
    DieItaration: integer;
    Data: Pointer;
    DataLen: Integer;
  End;

  TStatistikValues = Record
    Statcnt: integer;
    StatFloat: Extended;
    StatInt64: int64;
    Statiteration: integer;
  End;

Const // Entsprechen den Array Positionen im Statistics Array
  GetMemCalls = 0;
  MinGetMemtime = 1;
  AvgGetMemTime = 2;
  MaxGetMemTime = 3;
  FreememCalls = 4;
  SmallestBlock = 5;
  BiggestBlock = 6;
  MinFreeMemtime = 7;
  AvgFreeMemTime = 8;
  MaxFreeMemTime = 9;
  MaxAllocated = 10;
  MinLifetime = 11;
  AvgLifetime = 12;
  MaxLifetime = 13;
  FirstReorder = 14;
  sVerschnitt = 15;
  MaxVerschnitt = 16;
  MaxUsedBytes = 17;
  StatisticCount = 18; // Muss immer der Anzahl der zu loggenden Werte sein.

Var
  GetmemTime: TLineSeries;
  FreememTime: TLineSeries;
  Verschnitt: TLineSeries;
  Summe: TLineSeries;
  Statistics: Array Of TStatistikValues;
  AktualAllocated: int64;
  AlocBlockInfo: Array Of Record
    Len: integer;
    Count: integer;
  End;

  Procedure IncAlocBlockInfo(Len: integer);
  Var
    i: Integer;
    j: Integer;
  Begin
    If Not CheckBox8.Checked Then exit;
    For i := 0 To high(AlocBlockInfo) Do Begin
      // Gefunden erhöhen und Raus
      If AlocBlockInfo[i].Len = len Then Begin
        AlocBlockInfo[i].Count := AlocBlockInfo[i].Count + 1;
        exit;
      End;
      If AlocBlockInfo[i].Len < len Then Begin
        // Unseren Block gibt es nicht, er müsste aber Sortiertechnisch vor index i sein.
        setlength(AlocBlockInfo, high(AlocBlockInfo) + 2);
        For j := High(AlocBlockInfo) - 1 Downto i Do Begin
          AlocBlockInfo[j + 1] := AlocBlockInfo[j];
        End;
        AlocBlockInfo[i].Count := 1;
        AlocBlockInfo[i].Len := Len;
        exit;
      End;
    End;
    setlength(AlocBlockInfo, high(AlocBlockInfo) + 2);
    AlocBlockInfo[high(AlocBlockInfo)].Count := 1;
    AlocBlockInfo[high(AlocBlockInfo)].Len := len;
  End;

  Var
    dt, StartTime: int64;

  Function GetBlock(Iteration, Len, Livetime: integer): TBlock;
  Var
    i: integer;
    p: PByte;
    t: Double;
  Begin
    If panik Then exit;
    IncAlocBlockInfo(len);
    Statistics[GetMemCalls].StatInt64 := Statistics[GetMemCalls].StatInt64 + 1;
    result.CreateIteration := Iteration;
    result.DieItaration := Iteration + Livetime;
    result.DataLen := len;
    EpikTimer1.Clear();
    EpikTimer1.Start();
    result.data := heap.GetMem(len);
    EpikTimer1.Stop();
    If Not assigned(result.data) Then Begin
      dt := GetTickCount64 - StartTime;
      showmessage(format('Showmessage, error could not allocate %d bytes in iteration %d. Heap Full!', [len, Iteration]));
      panik := true;
      exit;
    End;
    AktualAllocated := AktualAllocated + len;
    If Statistics[MaxAllocated].StatInt64 < AktualAllocated Then Begin
      Statistics[MaxAllocated].StatInt64 := AktualAllocated;
      Statistics[MaxAllocated].Statiteration := Iteration;
    End;
    t := EpikTimer1.Elapsed() * 1000000; // in µs
    If assigned(GetmemTime) Then
      GetmemTime.AddXY(Iteration, t);
    i := Heap.MemSize(result.Data) - Result.DataLen;
    Statistics[sVerschnitt].StatInt64 := Statistics[sVerschnitt].StatInt64 + i;
    If Statistics[MaxVerschnitt].StatInt64 < Statistics[sVerschnitt].StatInt64 Then Begin
      Statistics[MaxVerschnitt].StatInt64 := Statistics[sVerschnitt].StatInt64;
      Statistics[MaxVerschnitt].Statiteration := Iteration;
    End;
    If (Statistics[MinGetMemtime].Statiteration = -1) Or (t < Statistics[MinGetMemtime].StatFloat) Then Begin
      Statistics[MinGetMemtime].StatFloat := t;
      Statistics[MinGetMemtime].Statiteration := Iteration;
    End;
    If (Statistics[MaxGetMemtime].Statiteration = -1) Or (t > Statistics[MaxGetMemtime].StatFloat) Then Begin
      Statistics[MaxGetMemtime].StatFloat := t;
      Statistics[MaxGetMemtime].Statiteration := Iteration;
    End;
    If (Statistics[SmallestBlock].Statiteration = -1) Or (len < Statistics[SmallestBlock].StatInt64) Then Begin
      Statistics[SmallestBlock].StatInt64 := len;
      Statistics[SmallestBlock].Statiteration := Iteration;
    End;
    If (Statistics[BiggestBlock].Statiteration = -1) Or (len > Statistics[BiggestBlock].StatInt64) Then Begin
      Statistics[BiggestBlock].StatInt64 := len;
      Statistics[BiggestBlock].Statiteration := Iteration;
    End;
    Statistics[AvgGetMemTime].StatInt64 := Statistics[AvgGetMemTime].StatInt64 + 1;
    Statistics[AvgGetMemTime].StatFloat := Statistics[AvgGetMemTime].StatFloat + t;

    If (Statistics[MinLifetime].Statiteration = -1) Or (Livetime < Statistics[MinLifetime].StatInt64) Then Begin
      Statistics[MinLifetime].StatInt64 := Livetime;
      Statistics[MinLifetime].Statiteration := Iteration;
    End;
    If (Statistics[MaxLifetime].Statiteration = -1) Or (Livetime > Statistics[MaxLifetime].StatInt64) Then Begin
      Statistics[MaxLifetime].StatInt64 := Livetime;
      Statistics[MaxLifetime].Statiteration := Iteration;
    End;
    Statistics[AvgLifetime].StatInt64 := Statistics[AvgLifetime].StatInt64 + Livetime;
    Statistics[AvgLifetime].Statcnt := Statistics[AvgLifetime].Statcnt + 1;

    p := result.Data;
    For i := 0 To len - 1 Do Begin
      p^ := (Iteration Mod 256);
      inc(p);
    End;
  End;

  Procedure FreeBlock(Const Block: TBlock);
  Var
    i: integer;
    p: PByte;
    t: Double;
  Begin
    If panik Then exit;
    i := heap.MemSize(block.Data) - Block.DataLen;
    Statistics[sVerschnitt].StatInt64 := Statistics[sVerschnitt].StatInt64 - i;
    Statistics[FreememCalls].StatInt64 := Statistics[FreememCalls].StatInt64 + 1;
    AktualAllocated := AktualAllocated - Block.DataLen;
    // Prüfen ob der Speicher nicht Fälschlicherweise überschrieben wurde
    p := Block.Data;
    For i := 0 To Block.DataLen - 1 Do Begin
      If p^ <> (Block.CreateIteration Mod 256) Then Begin
        Raise Exception.Create(format('Fatal error, heap cell should be %d but is %d.', [block.CreateIteration, p^]));
      End;
      inc(p);
    End;
    EpikTimer1.Clear();
    EpikTimer1.Start();
    i := heap.FreeMem(Block.Data);
    EpikTimer1.Stop();
    If i <> 0 Then Begin
      showmessage('Error, while freemem call, iteration : ' + inttostr(Block.DieItaration) + ' will abort now.');
      panik := true;
    End;
    t := EpikTimer1.Elapsed() * 1000000; // in µs
    If assigned(FreememTime) Then
      FreememTime.AddXY(Block.DieItaration, t);
    If (Statistics[MinFreeMemtime].Statiteration = -1) Or (t < Statistics[MinFreeMemtime].StatFloat) Then Begin
      Statistics[MinFreeMemtime].StatFloat := t;
      Statistics[MinFreeMemtime].Statiteration := Block.DieItaration;
    End;
    If (Statistics[MaxFreeMemTime].Statiteration = -1) Or (t > Statistics[MaxFreeMemTime].StatFloat) Then Begin
      Statistics[MaxFreeMemTime].StatFloat := t;
      Statistics[MaxFreeMemTime].Statiteration := Block.DieItaration;
    End;
    Statistics[AvgFreeMemTime].StatInt64 := Statistics[AvgFreeMemTime].StatInt64 + 1;
    Statistics[AvgFreeMemTime].StatFloat := Statistics[AvgFreeMemTime].StatFloat + t;
  End;

Var
  Blocks: Array Of TBlock;

  Procedure SortBlocks(li, re: integer);
  Var
    l, r, p: Integer;
    h: TBlock;
  Begin
    If Li < Re Then Begin
      p := blocks[Trunc((li + re) / 2)].DieItaration; // Auslesen des Pivo Elementes
      l := Li;
      r := re;
      While l < r Do Begin
        While blocks[l].DieItaration < p Do
          inc(l);
        While blocks[r].DieItaration > p Do
          dec(r);
        If L <= R Then Begin
          h := blocks[l];
          blocks[l] := blocks[r];
          blocks[r] := h;
          inc(l);
          dec(r);
        End;
      End;
      SortBlocks(li, r);
      SortBlocks(l, re);
    End;
  End;

Var
  ReportLines: Array Of Record
    Text, value: String;
  End;
  ReportLinesCnt: integer;

  Procedure AddReportLine(Text, Value: String);
  Begin
    ReportLines[ReportLinesCnt].Text := Text;
    ReportLines[ReportLinesCnt].value := Value;
    inc(ReportLinesCnt);
    If ReportLinesCnt > high(ReportLines) Then Begin
      setlength(ReportLines, high(ReportLines) + 101);
    End;
  End;

  Procedure CreateReport();
  Var
    i: integer;
    lt, lv: integer;
    p, s: String;
  Begin
    lt := 0;
    lv := 0;
    For i := 0 To ReportLinesCnt - 1 Do Begin
      lt := max(lt, utf8Length(ReportLines[i].Text));
      lv := max(lv, utf8length(ReportLines[i].value));
    End;
    form2.memo1.Lines.BeginUpdate;
    form2.Memo1.Clear;
    For i := 0 To ReportLinesCnt - 1 Do Begin
      // So Setzen, das zwischen dem Längsten Text und Value mindestens 5 Zeichen sind
      // Text ist dabei immer Linksbündig und p Rechtsbündig.
      s := ReportLines[i].Text;
      p := ReportLines[i].value;
      While utf8length(s) + utf8length(p) < lt + lv + 5 Do
        s := s + ' ';
      s := s + p;
      form2.memo1.Lines.Add(s);
    End;
    form2.memo1.Lines.EndUpdate;
  End;

Var
  n, i, ii: integer;
  Selector: TSelector;
  // Variablen für Random Blocks
  MinSize, MaxSize, Size: integer;
  MinLiveTime, MaxLiveTime: Integer;
  sizes: Array Of Integer;
  sizesptr: integer;
  s: String;
  chseed, j: Integer;
  Allocated, AbsoluteTime: TLineSeries;
  k: Integer;
  bs: TBarSeries;
Begin
  // Disable LCL
  GroupBox1.Enabled := false;
  GroupBox2.Enabled := false;
  GroupBox7.Enabled := false;
  Button6.Enabled := false;
  ComboBox2.Enabled := false;
  Button7.Enabled := true;
  Button10.Enabled := false;
  CheckBox10.Enabled := false;

  Application.ProcessMessages;
  // Eigentliche Abarbeitung
  // 1. Zufall initialisieren
  If trim(edit10.text) = '0' Then Begin
    Randomize;
    chseed := Random(high(Integer) - 1) + 1;
  End
  Else Begin
    chseed := strtoint(edit10.text);
  End;
  RandSeed := chseed;
  n := strtointdef(edit2.text, 0);
  Panik := false;
  EpikTimer1.CalibrateCallOverheads(EpikTimer1.SelectedTimebase^);
  EpikTimer1.CalibrateTickFrequency(EpikTimer1.SelectedTimebase^);
  Selector := sRandomBlocks;
  If RadioButton2.Checked Then Selector := sArraySim;
  If RadioButton3.Checked Then Selector := sAlternatings;
  blocks := nil;
  sizes := nil;
  setlength(ReportLines, 100);
  ReportLinesCnt := 0;
  Statistics := nil;
  setlength(Statistics, StatisticCount);
  Statistics[GetmemCalls].StatInt64 := 0;
  Statistics[FreememCalls].StatInt64 := 0;
  AktualAllocated := 0;
  Statistics[MaxAllocated].StatInt64 := 0;
  Statistics[MaxAllocated].Statiteration := 0;
  Statistics[MinGetMemtime].Statiteration := -1;
  Statistics[MaxGetMemTime].Statiteration := -1;
  Statistics[MinFreeMemtime].Statiteration := -1;
  Statistics[MaxFreeMemTime].Statiteration := -1;

  Statistics[AvgGetMemTime].StatInt64 := 0;
  Statistics[AvgGetMemTime].StatFloat := 0;
  Statistics[AvgFreeMemTime].StatInt64 := 0;
  Statistics[AvgFreeMemTime].StatFloat := 0;
  Statistics[SmallestBlock].Statiteration := -1;
  Statistics[BiggestBlock].Statiteration := -1;

  Statistics[MinLifetime].Statiteration := -1;
  Statistics[AvgLifetime].StatInt64 := 0;
  Statistics[AvgLifetime].Statcnt := 0;
  Statistics[MaxLifetime].Statiteration := -1;
  Statistics[FirstReorder].Statiteration := -1;
  Statistics[sVerschnitt].StatInt64 := 0;
  Statistics[MaxVerschnitt].StatInt64 := 0;
  Statistics[MaxUsedBytes].StatInt64 := 0;

  setlength(AlocBlockInfo, 0);
  Case Selector Of
    sRandomBlocks: Begin
        MinSize := strtointdef(edit3.text, -1);
        MaxSize := strtointdef(edit4.text, -1);
        MinLiveTime := strtointdef(edit8.text, -1);
        MaxLiveTime := strtointdef(edit9.text, -1);
        // Fehlerhandling, Range Checks
        If (MinSize <= 0) Or (MaxSize < MinSize) Then Begin
          showmessage('Error invalid size configuration.');
          exit;
        End;
        If (MinLiveTime <= 0) Or (MaxLiveTime < MinLiveTime) Then Begin
          showmessage('Error invalid livetime configuration.');
          exit;
        End;
      End;
    sArraySim: Begin
        MinSize := strtoint(edit5.text);
        MaxSize := strtoint(edit6.text);
        Size := strtoint(edit7.text);
        // Fehlerhandling, Range Checks
        If (MinSize <= 0) Or (MaxSize < MinSize) Then Begin
          showmessage('Error invalid length configuration.');
          exit;
        End;
        If (Size <= 0) Then Begin
          showmessage('Error invalid size configuration.');
          exit;
        End;
      End;
    sAlternatings: Begin
        s := Edit13.Text + ',';
        While trim(s) <> '' Do Begin
          SetLength(sizes, high(sizes) + 2);
          sizes[high(sizes)] := strtointdef(trim(copy(s, 1, pos(',', s) - 1)), -1);
          If sizes[high(sizes)] <= 0 Then Begin
            showmessage('Error invalid size configuration.');
            SetLength(sizes, 0);
            exit;
          End;
          delete(s, 1, pos(',', s));
        End;
        MinLiveTime := strtoint(edit11.text);
        MaxLiveTime := strtoint(edit12.text);
        If (MinLiveTime <= 0) Or (MaxLiveTime < MinLiveTime) Then Begin
          showmessage('Error invalid livetime configuration.');
          exit;
        End;
        sizesptr := 0;
      End;
  End;
  If Not CheckBox10.Checked Then Begin
    Chart1.DisableRedrawing;
    Chart2.DisableRedrawing;
    Chart3.DisableRedrawing;
    Chart4.DisableRedrawing;
  End;
  If CheckBox3.Checked Then Begin
    AbsoluteTime := TLineSeries.Create(Chart1);
    AbsoluteTime.Title := 'Time ' + Edit14.text;
    AbsoluteTime.SeriesColor := ColorBox1.Selected;
    AbsoluteTime.Pointer.Brush.Color := ColorBox1.Selected;
    AbsoluteTime.AxisIndexX := 1;
    Chart1.AddSeries(AbsoluteTime);
    AbsoluteTime.AddXY(0, 0);
  End
  Else
    AbsoluteTime := Nil;

  If CheckBox4.Checked Then Begin
    Allocated := TLineSeries.Create(Chart3);
    Allocated.Title := 'Alloc ' + Edit14.text;
    Allocated.SeriesColor := ColorBox4.Selected;
    Allocated.Pointer.Brush.Color := ColorBox4.Selected;
    Chart3.AddSeries(Allocated);
    Allocated.AddXY(0, 0);
  End
  Else
    Allocated := Nil;

  If CheckBox9.Checked Then Begin
    Summe := TLineSeries.Create(Chart3);
    Summe.Title := 'Sum ' + Edit14.text;
    Summe.SeriesColor := ColorBox7.Selected;
    Summe.Pointer.Brush.Color := ColorBox7.Selected;
    Chart3.AddSeries(Summe);
    Summe.AddXY(0, 0);
  End
  Else
    Summe := Nil;

  If CheckBox7.Checked Then Begin
    Verschnitt := TLineSeries.Create(Chart3);
    Verschnitt.Title := 'Clip ' + Edit14.text;
    Verschnitt.SeriesColor := ColorBox5.Selected;
    Verschnitt.Pointer.Brush.Color := ColorBox5.Selected;
    Chart3.AddSeries(Verschnitt);
    Verschnitt.AddXY(0, 0);
  End
  Else
    Verschnitt := Nil;

  If CheckBox5.Checked Then Begin
    GetmemTime := TLineSeries.Create(Chart2);
    GetmemTime.Title := 'Getmem ' + Edit14.text;
    GetmemTime.SeriesColor := ColorBox2.Selected;
    GetmemTime.Pointer.Brush.Color := ColorBox2.Selected;
    Chart2.AddSeries(GetmemTime);
  End
  Else
    GetmemTime := Nil;

  If CheckBox6.Checked Then Begin
    FreememTime := TLineSeries.Create(Chart2);
    FreememTime.Title := 'Freemem ' + Edit14.text;
    FreememTime.SeriesColor := ColorBox3.Selected;
    FreememTime.Pointer.Brush.Color := ColorBox3.Selected;
    Chart2.AddSeries(FreememTime);
  End
  Else
    FreememTime := Nil;

  If CheckBox8.Checked Then Begin
    bs := TBarSeries.Create(Chart2);
    bs.Title := 'Blocks ' + Edit14.text;
    bs.SeriesColor := ColorBox6.Selected;
    bs.BarBrush.Color := ColorBox6.Selected;
    Chart4.AddSeries(bs);
  End;

  CheckBox2Change(Nil); // Anpassen des Styles der Kurven
  heap.ReCreate();
  ii := 0;
  StartTime := gettickcount64;
  // Die Eigentliche Durchführung des Stests
  For i := 0 To n - 1 Do Begin
    Case Selector Of
      sRandomBlocks: Begin
          setlength(Blocks, high(Blocks) + 2);
          blocks[high(Blocks)] := GetBlock(i, RandomRange(MinSize, MaxSize), RandomRange(MinLiveTime, MaxLiveTime));
        End;
      sArraySim: Begin
          // Über die Lebenszeit werden die Blocks automatisch gelöscht ;)
          setlength(Blocks, high(Blocks) + 2);
          blocks[high(Blocks)] := GetBlock(i, (MinSize + (i Mod (MaxSize - MinSize + 1))) * Size, 1);
        End;
      sAlternatings: Begin
          setlength(Blocks, high(Blocks) + 2);
          blocks[high(Blocks)] := GetBlock(i, sizes[sizesptr], RandomRange(MinLiveTime, MaxLiveTime));
          sizesptr := (sizesptr + 1) Mod length(sizes);
        End;
    End;
    // Das muss vor dem Freigeben Gemessen werden, da der Freigabevorgang diese Werte ja wieder verringert.
    If statistics[sVerschnitt].StatInt64 + AktualAllocated > Statistics[MaxUsedBytes].StatInt64 Then Begin
      Statistics[MaxUsedBytes].StatInt64 := statistics[sVerschnitt].StatInt64 + AktualAllocated;
      Statistics[MaxUsedBytes].Statiteration := ii;
    End;
    j := 0;
    While j <= high(blocks) Do Begin
      If i >= blocks[j].DieItaration Then Begin
        FreeBlock(Blocks[j]);
        For k := j To high(Blocks) - 1 Do Begin
          Blocks[k] := Blocks[k + 1];
        End;
        setlength(blocks, high(blocks));
        dec(j);
      End;
      inc(j);
    End;
    ii := i + 1;
    // Die Absolut Zeit in ms
    If (i Mod 1000) = 999 Then Begin
      dt := GetTickCount64 - StartTime;
      // X- Achse = Anzahl der Durchläufe
      // Y- Achse = Vergange Zeit in s
      If assigned(AbsoluteTime) Then
        AbsoluteTime.AddXY((ii), dt / 1000);
    End;
    If assigned(Allocated) Then
      Allocated.AddXY(ii, (AktualAllocated * 100) / heap.HeapSize);
    If assigned(Verschnitt) Then
      Verschnitt.AddXY(ii, (statistics[sVerschnitt].StatInt64 * 100) / heap.HeapSize);
    If assigned(summe) Then
      Summe.AddXY(ii, ((statistics[sVerschnitt].StatInt64 + AktualAllocated) * 100) / heap.HeapSize);
    // Merken, wann zum ersten Mal die Freispeicherliste neu Organisiert wurde
    If Statistics[FirstReorder].Statiteration = -1 Then Begin
      If heap.FreeListReorganisations > 0 Then Begin
        Statistics[FirstReorder].Statiteration := ii;
      End;
    End;
    If i Mod 100 = 0 Then Begin
      Application.ProcessMessages;
    End;
    If panik Then break;
  End;
  // Zum Schluss alles Freigeben was noch übrig ist.
  SortBlocks(0, high(Blocks));
  For j := 0 To high(Blocks) Do Begin
    FreeBlock(Blocks[j]);
  End;
  // Zum Schluss noch eine Letzte Auswertung
  If Not panik Then Begin
    // Im Panik mode muss die Zeit vor der Messagebox bestimmt werden, sonst messen wir ja die Zeit mit, die der User zum klicken braucht ..
    dt := GetTickCount64 - StartTime;
  End;
  // X- Achse = Anzahl der Durchläufe
  // Y- Achse = Vergange Zeit in s
  If assigned(AbsoluteTime) Then
    AbsoluteTime.AddXY(ii, dt / 1000);
  If assigned(Allocated) Then
    Allocated.AddXY(ii, (AktualAllocated * 100) / heap.HeapSize);
  setlength(blocks, 0);

  // Ausgabe der Statistik
  AddReportLine('HeapSim ver. ' + Version + ' by Corpsman, www.Corpsman.de', '');
  AddReportLine('Plot name', edit14.text);
  AddReportLine('Allocated heap size from operating system [Byte]', inttostr(HeapSize));
  AddReportLine('Aviable heap size [%, Byte]', format('%0.2f, %d', [(heap.HeapSize * 100) / HeapSize, heap.HeapSize]));
  AddReportLine('Total getmem [%, Byte]', format('%0.2f, %d', [(heap.TotalBytesAllocated * 100) / heap.HeapSize, heap.TotalBytesAllocated]));
  AddReportLine('Total freemem [%, Byte]', format('%0.2f, %d', [(heap.TotalBytesFreed * 100) / heap.HeapSize, heap.TotalBytesFreed]));
  AddReportLine('', '');
  AddReportLine('Total getmem calls', inttostr(Statistics[GetmemCalls].StatInt64));
  AddReportLine('Min getmem time [iteration, µs]', format('%d, %0.1f', [Statistics[MinGetMemtime].Statiteration, Statistics[MinGetMemtime].StatFloat]));
  AddReportLine('Avg getmem time [µs]', format('%0.1f', [Statistics[AvgGetMemTime].StatFloat / max(1, Statistics[AvgGetMemTime].StatInt64)]));
  AddReportLine('Max getmem time [iteration, µs]', format('%d, %0.1f', [Statistics[MaxGetMemtime].Statiteration, Statistics[MaxGetMemtime].StatFloat]));
  AddReportLine('Smallest allocated block [iteration, Byte]', format('%d, %d', [Statistics[SmallestBlock].Statiteration, Statistics[SmallestBlock].StatInt64]));
  AddReportLine('Biggest allocoted block [iteration, Byte]', format('%d, %d', [Statistics[BiggestBlock].Statiteration, Statistics[BiggestBlock].StatInt64]));
  AddReportLine('', '');
  AddReportLine('Total freemem calls', inttostr(Statistics[FreeMemCalls].StatInt64));
  If Statistics[FreeMemCalls].StatInt64 <> 0 Then Begin // If there are no "Free's" there is also no valid statistic for that !
    AddReportLine('Min freemem time [iteration, µs]', format('%d, %0.1f', [Statistics[MinFreeMemtime].Statiteration, Statistics[MinFreeMemtime].StatFloat]));
    AddReportLine('Avg freemem time [µs]', format('%0.1f', [Statistics[AvgFreeMemTime].StatFloat / max(1, Statistics[AvgFreeMemTime].StatInt64)]));
    AddReportLine('Max freemem time [iteration, µs]', format('%d, %0.1f', [Statistics[MaxFreeMemtime].Statiteration, Statistics[MaxFreeMemtime].StatFloat]));
  End;
  AddReportLine('', '');
  AddReportLine('Max allocated bytes at the same time [iteration, Byte, %]', format('%d, %d, %0.2f', [Statistics[MaxAllocated].Statiteration, Statistics[MaxAllocated].StatInt64, (Statistics[MaxAllocated].StatInt64 * 100) / heap.HeapSize]));
  AddReportLine('Max used (allocated + clipped) bytes at the same time [iteration, Byte, %]', format('%d, %d, %0.2f', [Statistics[MaxUsedBytes].Statiteration, Statistics[MaxUsedBytes].StatInt64, (Statistics[MaxUsedBytes].StatInt64 * 100) / heap.HeapSize]));
  AddReportLine('', '');
  AddReportLine('Min lifetime [iterations]', format('%d', [Statistics[MinLifetime].StatInt64]));
  AddReportLine('Avg lifetime [iterations]', format('%0.1f', [Statistics[AvgLifetime].StatInt64 / max(1, Statistics[AvgLifetime].Statcnt)]));
  AddReportLine('Max lifetime [iterations]', format('%d', [Statistics[MaxLifetime].StatInt64]));
  AddReportLine('', '');
  If Statistics[FirstReorder].Statiteration <> -1 Then Begin
    AddReportLine('First iteration for freelist reorganization', inttostr(Statistics[FirstReorder].Statiteration));
  End;
  AddReportLine('Count of freelist reorganizations', inttostr(heap.FreeListReorganisations));
  If Statistics[MaxVerschnitt].StatInt64 <> 0 Then Begin
    AddReportLine('Maximum clipped memory [iteration, Byte, %]', format('%d %d %0.2f', [Statistics[MaxVerschnitt].Statiteration, Statistics[MaxVerschnitt].StatInt64, (Statistics[MaxVerschnitt].StatInt64 * 100) / heap.HeapSize]));
  End
  Else Begin
    AddReportLine('No memory clipped', '');
  End;
  AddReportLine('', '');
  AddReportLine('Random seed', inttostr(chseed));
  AddReportLine('Total time for simulation [s]', format('%0.3f', [dt / 1000]));
  If panik Then Begin
    AddReportLine('', '');
    AddReportLine('canceled simulation in iteration', inttostr(ii));
  End;
  If CheckBox8.Checked Then Begin
    For i := 0 To high(AlocBlockInfo) Do Begin
      bs.AddXY(AlocBlockInfo[i].Len, AlocBlockInfo[i].Count);
    End;
  End;
  setlength(AlocBlockInfo, 0);

  CreateReport();
  // Die Anderen Einstellungen werden "Klassisch" an das memo angehängt.
  form2.Memo1.Lines.Add('');
  form2.Memo1.Lines.Add('------------------------------------------------------------');
  form2.Memo1.Lines.add('Heap strategy: ' + ComboBox1.Text);
  If RadioButton1.Checked Then Begin
    form2.Memo1.Lines.add('Stress scenario:' + GroupBox5.Caption);
    form2.Memo1.Lines.add(' Repeations : ' + edit2.text);
    form2.Memo1.Lines.add(' ' + Label5.Caption + ': ' + edit3.text);
    form2.Memo1.Lines.add(' ' + Label6.Caption + ': ' + edit4.text);
    form2.Memo1.Lines.add(' ' + Label10.Caption + ': ' + edit8.text);
    form2.Memo1.Lines.add(' ' + Label11.Caption + ': ' + edit9.text);
  End;
  If RadioButton2.Checked Then Begin
    form2.Memo1.Lines.add('Stress scenario:' + GroupBox6.Caption);
    form2.Memo1.Lines.add(' Repeations : ' + edit2.text);
    form2.Memo1.Lines.add(' ' + Label7.Caption + ': ' + edit5.text);
    form2.Memo1.Lines.add(' ' + Label8.Caption + ': ' + edit6.text);
    form2.Memo1.Lines.add(' ' + Label9.Caption + ': ' + edit7.text);
  End;
  If RadioButton3.Checked Then Begin
    form2.Memo1.Lines.add('Stress scenario:' + GroupBox4.Caption);
    form2.Memo1.Lines.add(' Repeations : ' + edit2.text);
    form2.Memo1.Lines.add(' ' + Label15.Caption + ': ' + edit13.text);
    form2.Memo1.Lines.add(' ' + Label13.Caption + ': ' + edit11.text);
    form2.Memo1.Lines.add(' ' + Label14.Caption + ': ' + edit12.text);
  End;
  If CheckBox10.Checked Then Begin
    form2.Memo1.Lines.add('Refresh plots enabled.');
  End
  Else Begin
    form2.Memo1.Lines.add('Refresh plots disabled.');
  End;

  If Not CheckBox10.Checked Then Begin
    Chart1.EnableRedrawing;
    Chart2.EnableRedrawing;
    Chart3.EnableRedrawing;
    Chart4.EnableRedrawing;
  End;

  form2.showmodal;
  setlength(ReportLines, 0);
  // Enable LCL
  CheckBox10.Enabled := true;
  Button10.Enabled := true;
  ComboBox2.Enabled := true;
  GroupBox1.Enabled := true;
  GroupBox2.Enabled := true;
  GroupBox7.Enabled := true;
  Button6.Enabled := true;
  Button7.Enabled := false;
End;

Procedure TForm1.Button7Click(Sender: TObject);
Begin
  // Panik Abbruch
  Panik := true;
End;

Procedure TForm1.Button8Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button9Click(Sender: TObject);
//Var
//  p5, p, p2, p3, p4: Pointer;
Begin
  // Test Button
//  If Not assigned(Heap) Then exit;
//  p := heap.GetMem(1);
//  p2 := heap.GetMem(1);
//  p3 := heap.GetMem(1);
//  p4 := heap.GetMem(1);
//  p5 := heap.GetMem(1);
//  heap.FreeMem(p3);
//  heap.FreeMem(p2);
//  heap.FreeMem(p4);
//  heap.FreeMem(p);
//  heap.ReCreate;
End;

Procedure TForm1.Chart1DblClick(Sender: TObject);
Begin
  ChartAxisTransformations1LogarithmAxisTransform1.Enabled := Not ChartAxisTransformations1LogarithmAxisTransform1.Enabled;
End;

Procedure TForm1.CheckBox1Change(Sender: TObject);
Begin
  chart1.Legend.Visible := CheckBox1.Checked;
  chart2.Legend.Visible := CheckBox1.Checked;
  chart3.Legend.Visible := CheckBox1.Checked;
  chart4.Legend.Visible := CheckBox1.Checked;
End;

Procedure TForm1.CheckBox2Change(Sender: TObject);
  Procedure ToggleSeries(Const Chart: TChart);
  Var
    i: integer;
  Begin
    For i := 0 To chart.SeriesCount - 1 Do Begin
      (chart.Series[i] As TLineSeries).ShowPoints := CheckBox2.Checked;
      If CheckBox2.Checked Then Begin
        (chart.Series[i] As TLineSeries).LinePen.Style := psClear;
      End
      Else Begin
        (chart.Series[i] As TLineSeries).LinePen.Style := psSolid;
      End;
    End;
  End;
Begin
  ToggleSeries(Chart1);
  ToggleSeries(Chart2);
  ToggleSeries(Chart3);
  //  ToggleSeries(Chart4); -- Geht natürlich nicht, ist ja keine Lineseries drin
End;

Procedure TForm1.ComboBox1KeyPress(Sender: TObject; Var Key: char);
Begin
  key := #0;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  button7.Click; // Panik Abbruch
  While button7.Enabled Do // Warten bis Abbruch durchgeführt wurde
    Application.ProcessMessages;
  Button3.Click; // Heap Freigeben
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  Constraints.MinHeight := height;
  Constraints.MinWidth := Width;
  Version := '0.03';
  caption := 'HeapSim ver. ' + version + ' by Corpsman, www.Corpsman.de';
  ComboBox1.Items.Clear;
  ComboBox1.Items.Add(FirstFitH);
  ComboBox1.Items.Add(NextFitH);
  ComboBox1.Items.Add(BestFitH);
  ComboBox1.Items.Add(WorstFitH);
  ComboBox1.Items.Add(BuddyH);
  ComboBox1.Items.Add(BitVecH);
  ComboBox1.Items.Add(FPCH);
  ComboBox1.ItemIndex := 0;
  (*
   * So vorbelegen, dass man auch gleich was hübsches sehen kann.
   *)
  edit1.text := '1'; // Heap Size in MB -- Je Größer der Heap, desto Länger dauerts bis der Speicher neu Organisiert werden muss, die Effekte bleiben aber prinzipiell die Selben.
  edit2.text := '10000'; // Repeations
  edit10.text := '0'; // Auto Seed

  edit3.text := '128'; // Min Allock Block
  edit4.text := '2048'; // Max Allock Block
  edit8.text := '100'; // Min Livetime
  edit9.text := '1000'; // Max Livetime

  edit5.text := '10'; // Min Array Size
  edit6.text := '110'; // Max Array Size
  edit7.text := '100'; // Size of Array Element

  edit13.text := '481,123,1597'; // Block commalist
  edit11.text := '100'; // Min Livetime
  edit12.text := '1500'; // Max Livetime

  Edit14.text := 'Plot1';
  ColorBox1.Selected := clMaroon;
  ColorBox2.Selected := clred;
  ColorBox3.Selected := clgreen;
  ColorBox4.Selected := clgreen;
  ColorBox5.Selected := clred;
  ColorBox6.Selected := clMaroon;
  ColorBox7.Selected := clMaroon;
  OpenDialog1.InitialDir := ExtractFilePath(ParamStrUTF8(0));
  SaveDialog1.InitialDir := ExtractFilePath(ParamStrUTF8(0));
  SaveDialog2.InitialDir := ExtractFilePath(ParamStrUTF8(0));
  SaveDialog3.InitialDir := ExtractFilePath(ParamStrUTF8(0));

  ComboBox2.ItemIndex := ComboBox2.items.Count - 1;
  Button10.Click;
  PageControl1.ActivePageIndex := 0;

  EpikTimer1.CalibrateCallOverheads(EpikTimer1.SelectedTimebase^);
  EpikTimer1.CalibrateTickFrequency(EpikTimer1.SelectedTimebase^);
  EpikTimer1.Clear();
  EpikTimer1.Start();
  EpikTimer1.Stop();
End;

Procedure TForm1.MenuItem1Click(Sender: TObject);
Begin

  Case ComboBox2.ItemIndex Of
    0: Begin // Export as CSV
        ExportChartAsCSV(FindComponent('Chart' + inttostr(PageControl1.ActivePageIndex + 1)) As TChart);
      End;
    1: Begin // Export as bitmap
        ExportChartAsBitmap(FindComponent('Chart' + inttostr(PageControl1.ActivePageIndex + 1)) As TChart);
      End;
    2: Begin // Clear all statistics
        chart1.ClearSeries;
        chart2.ClearSeries;
        chart3.ClearSeries;
        chart4.ClearSeries;
      End;
  End;
End;

Procedure TForm1.ExportChartAsCSV(Const Chart: TChart);
Type
  TLine = Record
    Elements: Array Of Double;
    used: Boolean;
  End;

Var
  f: Textfile;
  s: String;
  i, j, k: Integer;
  minx, maxx: integer;
  serie: TBasicPointSeries;
  Data: Array Of TLine;
Begin
  If Not assigned(chart) Then Begin
    showmessage('Error could not find datasource, please contact programmer.');
    exit;
  End;
  If chart.SeriesCount = 0 Then Begin
    showmessage('Error nothing to export.');
    exit;
  End;
  If SaveDialog2.Execute Then Begin
    assignfile(f, utf8tosys(SaveDialog2.FileName));
    rewrite(f);
    s := '"' + chart.AxisList[1].Title.Caption + '"';
    For i := 0 To chart.SeriesCount - 1 Do Begin
      s := s + ';"' + (chart.Series[i] As TBasicPointSeries).title + ' ' + chart.AxisList[0].Title.Caption + '"';
    End;
    writeln(f, s);
    // Dimension Bestimmen
    Serie := chart.Series[0] As TBasicPointSeries;
    minx := round(serie.MinXValue);
    maxx := round(serie.MaxXValue);
    For i := 1 To chart.SeriesCount - 1 Do Begin
      Serie := chart.Series[i] As TBasicPointSeries;
      minx := min(minx, round(serie.MinXValue));
      maxx := max(maxx, round(serie.MaxXValue));
    End;
    // Leeren Datensatz mit gefüllter X-Achse erstellen
    Data := nil;
    setlength(Data, maxx - minx + 1);
    For i := 0 To high(data) Do Begin
      setlength(data[i].Elements, chart.SeriesCount + 1);
      data[i].used := false;
      data[i].Elements[0] := i + minx;
    End;
    // Series Einfüllen
    For i := 0 To chart.SeriesCount - 1 Do Begin
      Serie := chart.Series[i] As TBasicPointSeries;
      For j := 0 To serie.Count - 1 Do Begin
        k := round(serie.XValue[j]) - minx;
        data[k].used := true;
        data[k].Elements[i + 1] := serie.yValue[j];
      End;
    End;
    // Daten in datei Speichern
    For i := 0 To high(Data) Do Begin
      If Data[i].used Then Begin
        s := inttostr(round(data[i].Elements[0])) + ';';
        For j := 1 To high(data[i].Elements) Do Begin
          s := s + FloatToStr(data[i].Elements[j]);
          If j <> high(data[i].Elements) Then s := s + ';';
        End;
        writeln(f, s);
      End;
      setlength(data[i].Elements, 0);
    End;
    setlength(data, 0);
    CloseFile(f);
  End;
End;

Procedure TForm1.ExportChartAsBitmap(Const Chart: TChart);
Var
  b: Tbitmap;
Begin
  If Not assigned(chart) Then Begin
    showmessage('Error could not find datasource, please contact programmer.');
    exit;
  End;
  If SaveDialog3.Execute Then Begin
    // Ohne das Processmessages wird das Chart nicht gezeichnet und das Canvas ist Leer ..
    Application.ProcessMessages;
    sleep(1);
    Application.ProcessMessages;
    b := Tbitmap.create;
    b.width := Chart.Width;
    b.Height := Chart.Height;
    b.canvas.CopyRect(chart.ClientRect, chart.Canvas, chart.ClientRect);
    b.SaveToFile(SaveDialog3.FileName);
    b.free;
  End;
End;

End.

