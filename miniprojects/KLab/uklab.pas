(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of KLab                                                  *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit uklab;

{$MODE objfpc}{$H+}

Interface

Uses
  SynEdit, Dialogs, Classes, SysUtils, inifiles, ComCtrls, Forms,
  math, Process, lcltype, graphics;

Const
  DefaultEditorFontSize = 9;
  defAsmAttri = clgreen;
  defCommentAttri = clgray;
  defDirecAttri = clgreen;
  defIdentifierAttri = clblack;
  defInvalidAttri = clblack;
  defKeyAttri = clblack;
  defNumberAttri = clpurple;
  defRightEdge = clgray;
  defSpaceAttri = clwhite;
  defStringAttri = clolive;
  defSymbolAttri = clblack;
  defSelectedColor = clnavy;
  defSelectedForegroundColor = clwhite;
  defBackgroundColor = clnone; // Die Jeweils Andere Farbe, meistens Background

  DefCaption = 'KLab ver. 0.09 by Corpsman | www.Corpsman.de | ';

{$IFDEF Windows}
Const
  Error_no_Spaces_In_Paths = 'Error under windows, no paths containing spaces are allowed, please use different path names.';
{$ENDIF}

Type

  TFileContainer = Record
    Filename: String;
    Text: String;
    Changed: Boolean;
    TopLine: Integer;
  End;

  TFileContainerArray = Array Of TFileContainer;

  TKlabConfig = Record
    // Common
    Aviable_CPUS: TStringList;
    Logfile: String;
    HexFile: String;
    MapFile: String;
    OutFile: String;
    CPU: String;
    Clock: integer;
    // CodeStyle
    FontSize: Integer;

    // Compiler
    CompilerCommand: String;
    MCall: Boolean;
    Wstrict: Boolean;
    Wall: Boolean;
    g: Boolean;
    definecpu: Boolean;
    optimization: char;
    usesearchpath: Boolean;
    searchpaths: TStringList;
    uselibsearchpath: Boolean;
    libsearchpaths: TStringList;
    // Linker
    LinkerCommand: String;
    ObjectCopyCommand: String;
    // Programmer
    OverrideBaud: Boolean;
    SpecifyJTAGPeriod: Boolean;
    SpecifyProgrammerType: Boolean;
    SpecifyConnectionPort: Boolean;
    SpecifyExternalConfigFile: Boolean;
    DisableAutoErase: Boolean;
    DoNotWrite: Boolean;
    OverrideInvalidSignature: Boolean;
    CountErases: Boolean;
    RS232BaudRate: integer;
    JTAGPeriod: Integer; // Gespeichert * 10
    ProgrammerType: String;
    ConnectionPort: String;
    ExternalFile: String;
    EraseCount: integer;
    // Uart
    Uart_Device: String;
    Uart_BaudRate: integer;
    Uart_Flow_Control: integer;
    Uart_Bits: integer;
    Uart_Parity: integer;
    Uart_Stop: integer;
    // IDE
    Uart_autoDisconnect: Boolean;
    Uart_autoClean: Boolean;
  End;

  TCPU = Record
    // Zum Compilieren
    mmcu: String;
    maxcpu: integer;
    flash: integer;
    ram: integer;
    eeprom: integer;
    avrdudename: String;
    // Fuse Bits
    FuseHigh: Array[0..7] Of String;
    FuseHighEnabled: Byte;
    FuseLow: Array[0..7] Of String;
    FuseLowEnabled: Byte;
    FuseExtended: Array[0..7] Of String;
    FuseExtendedEnabled: Byte;
    FuseLock: Array[0..7] Of String;
    FuseLockEnabled: Byte;
  End;

  TFuseType = (highfuse, lowfuse, extendedfuse, lockfuse);

  TCommandString = Record
    Command: String;
    Params: String;
  End;

Var
  AppPath: String; // Der Pfad in dem Klab liegt

  KlabConfig: TKlabConfig;
  CPU: TCPU;

  AktualProjectFilename: String = '';
  AktualProjectWorkPath: String = '';

  AktualEditor: integer = 0;
  AktualSynedit: TSynEdit = Nil;
  OpenedFiles: TFileContainerArray;

  GlobalChanged: Boolean = false;
  OldLeftIndex: integer = -1;
  OldRightIndex: integer = -1;

  //  FileMonitorAktive: Boolean = true;

Procedure LoadDefaultEditorSettings();
Procedure LoadEditorProperties();
Procedure SaveEditorProperties();
Procedure SaveIDELastOpened(Filename: String);
Procedure Editorchange;
Procedure EditorReload;

Function LoadCPU(Filename: String): TCPU;

Procedure LoadFile(Filename: String); // Sucht je nach FileExtension den Passenden Loader raus

Procedure CloseSourceFile();
Procedure OpenSourceFile(Filename: String; Index: integer); // Öffnet eine Neue Datei Links oder Rechts

Procedure LoadProject(Filename: String);
Procedure NewProject(Filename: String); // Schaltet entsprechend alle LCL Objekte um
Procedure SaveProject(); // Speichert das Aktuelle Projekt unter AktualProjectFilename
Procedure SaveProjectAs(Filename: String);
Function FindIndexbyname(Filename: String): integer;
Procedure RemoveFileFromProject(Filename: String); // Entfernt einen Dateinamen aus dem Projekt

// Compile and Programm Funktions

Function ListOfAllCFiles(): TStringList; // Listet alle *.c Files des Projektes auf
Function ExecuteCommand(Command, Params: String): String; Overload; // Führt einen Befehl aus, und gibt dessen Ausgabe auch gleich in Messages aus
Function ExecuteCommand(Command: TCommandString): String; Overload; // Führt einen Befehl aus, und gibt dessen Ausgabe auch gleich in Messages aus
Function CreateCompileCommand(Infile, OutFile: String): TCommandString; // Liest die Compilerschalter aus und baut den Befehl zum Compilieren der Datei zusammen.
Function CreateLinkerCommand(Infiles: String): TCommandString; // Erzeugt den Linker Befehl, welcher die .map und .out Datei erzeugt
Function Check_File_Sizes(Used_CPU: TCPU; Filename: String): Boolean; // Überprüft ob das .out File in den angewählten Processor passt.
Function CreateObjectCopyCommand(): TCommandString; // Erzeugt den Befehl für die.hex Datei
Function CreateProgrammCommand(Hexfile: String; Used_CPU: TCPU): TCommandString; // Erzeugt den Befehl zum Ansteuern von AVR-dude mit angegebener CPU
Function CreateLoadProgrammCommand(Hexfile: String; Used_CPU: TCPU): TCommandString;
Function CreateVerifyProgrammCommand(Hexfile: String; Used_CPU: TCPU): TCommandString;
Function CreateEraseCommand(Used_CPU: TCPU): TCommandString;
Function CreateReadFuseCommand(TargetFile: String; kind: TFuseType; usedCPU: TCPU): TCommandString;
Function CreateWriteFuseCommand(FuseValue: byte; kind: TFuseType; usedCPU: TCPU): TCommandString;
Procedure AddMessage(Value: String); // Fügt einen Text in das Messages Menü ein
Function CursorPosToIndex(Const Text: TStrings; Pos: TPoint): integer;
Procedure DeleteAllFilesInDir(ADirectory: String); // Sucht Alle *.o, *.map, *.hex Files im Verzeichniss und Löscht diese

Procedure TextBlockMove(Const Lines: TStrings; SelStart, SelEnd: Integer; MoveRight: Boolean); // Fügt Leerzeichen ein, oder löscht diese ( jeweils zu beginn der selektierten Zeilen )

Implementation

Uses unit1, unit4, LazUTF8, LazFileUtils;

Procedure TextBlockMove(Const Lines: TStrings; SelStart, SelEnd: Integer; MoveRight: Boolean);
Var
  i, j, k: integer;
Begin
  If selend < selstart Then Begin
    i := selstart;
    selstart := selend;
    selend := i;
  End;
  If SelStart = SelEnd Then Begin
    // Eine einzige ganze Zeile Kommentieren -- Fertig, getestet.
    For i := Selstart Downto 1 Do
      If (Lines.text[i] = #10) Or (i = 1) Then Begin
        // wir haben nun zwar das crt aber die "Leerzeichen, am Anfang der Zeile müssen ignoriert werden."
        If (i <> 1) Then
          j := i + 1
        Else
          j := 1;
        If MoveRight Then Begin
          // Leerzeichen Einfügen
          Lines.text := copy(Lines.text, 1, i) + '  ' + copy(lines.text, i + 1, length(lines.text));
        End
        Else Begin
          // Leerzeichen Entfernen
          If (lines.text[j] = ' ') And (lines.text[j + 1] = ' ') Then
            Lines.text := copy(Lines.text, 1, j - 1) + copy(lines.text, j + 2, length(lines.text));
        End;
        exit;
      End;
  End
  Else Begin
    // Mehrere Zeilen wurden zum auskommentieren ausgewählt -- Fertig, getestet.
    If MoveRight Then Begin
      // Kommentar Einfügen
      i := SelEnd;
      j := selstart;
      While i >= j Do Begin
        If lines.text[i] = #10 Then Begin
          lines.text := copy(lines.text, 1, i) + '  ' + copy(lines.text, i + 1, length(lines.text));
        End;
        dec(i);
      End;
      i := j;
      While (i >= 1) And (lines.text[i] <> #10) Do Begin
        dec(i);
      End;
      lines.text := copy(lines.text, 1, i) + '  ' + copy(lines.text, i + 1, length(lines.text));
    End
    Else Begin
      // Kommentar Entfernen
      i := SelEnd;
      j := selstart;
      While i >= j Do Begin
        If lines.text[i] = #10 Then Begin
          k := i + 1;
          If (Lines.text[k] = ' ') And (Lines.text[k + 1] = ' ') Then Begin
            lines.text := copy(lines.text, 1, k - 1) + copy(lines.text, k + 2, length(lines.text));
          End;
        End;
        dec(i);
      End;
      While (i >= 1) And (lines.text[i] <> #10) Do Begin
        dec(i);
      End;
      k := i + 1;
      If (Lines.text[k] = ' ') And (Lines.text[k + 1] = ' ') Then Begin
        lines.text := copy(lines.text, 1, k - 1) + copy(lines.text, k + 2, length(lines.text));
      End;
    End;
  End;
End;

Procedure DeleteAllFilesInDir(ADirectory: String);
Var
  sr: TSearchRec;
  extensions: Array Of String;
  i: Integer;
Begin
  If Not DirectoryExistsUTF8(ADirectory) Then exit;
  ADirectory := IncludeTrailingPathDelimiter(ADirectory);
  setlength(extensions, 4);
  extensions[0] := '*.o';
  extensions[1] := '*.map';
  extensions[2] := '*.hex';
  extensions[3] := '*.out';
  For i := 0 To high(extensions) Do Begin
    If (FindFirstutf8(ADirectory + extensions[i], faAnyFile, SR) = 0) Then Begin
      Repeat
        If DeleteFileUTF8(ADirectory + sr.Name) Then Begin
          AddMessage('Deleted : ' + ADirectory + sr.Name);
        End
        Else Begin
          AddMessage('Could not delete : ' + ADirectory + sr.Name);
        End;
      Until FindNextutf8(SR) <> 0;
    End;
    FindCloseutf8(SR);
  End;
End;

Function CursorPosToIndex(Const Text: TStrings; Pos: TPoint): integer;
Const
{$IFDEF WINDOWS}
  crtlen = 2;
{$ELSE}
  crtlen = 1;
{$ENDIF}
Var
  i, res: Integer;
Begin
  res := min(length(text[pos.y - 1]), pos.x);
  For i := 0 To pos.y - 2 Do
    res := res + length(text[i]) + crtlen;
  CursorPosToIndex := res;
End;

Procedure AddMessage(Value: String);
Var
  sl: TStringlist;
  i: Integer;
  s: String;
Begin
  sl := TStringList.create;
  sl.Text := value;
  For i := 0 To sl.count - 1 Do Begin
    s := trim(sl[i]);
    If length(s) <> 0 Then Begin
      form1.ListBox1.Items.Add(s);
    End;
  End;
  sl.free;
  If Not form1.ListBox1.Visible Then
    Form1.MenuItem16Click(Form1); //Was hier übergeben wird ist eigentlich Egal, es mus nur <> Nil sein !!
  form1.ListBox1.ItemIndex := form1.ListBox1.Count - 1;
  application.ProcessMessages;
End;

Procedure LoadDefaultEditorSettings();
Begin
  form1.SynEdit1.font.Size := DefaultEditorFontSize;
  form1.SynEdit2.font.Size := DefaultEditorFontSize;
  // Scheme Settings
  KlabConfig.FontSize := DefaultEditorFontSize;
  form1.SynCppSyn1.AsmAttri.Foreground := defAsmAttri;
  form1.SynCppSyn1.CommentAttri.Foreground := defCommentAttri;
  form1.SynCppSyn1.DirecAttri.Foreground := defDirecAttri;
  form1.SynCppSyn1.IdentifierAttri.Foreground := defIdentifierAttri;
  form1.SynCppSyn1.InvalidAttri.Foreground := defInvalidAttri;
  form1.SynCppSyn1.KeyAttri.Foreground := defKeyAttri;
  form1.SynCppSyn1.NumberAttri.Foreground := defNumberAttri;
  form1.SynEdit1.RightEdgeColor := defRightEdge;
  form1.SynEdit2.RightEdgeColor := defRightEdge;
  form1.SynEdit1.Color := defSpaceAttri;
  form1.SynEdit2.Color := defSpaceAttri;
  form1.SynCppSyn1.StringAttri.Foreground := defStringAttri;
  form1.SynCppSyn1.SymbolAttri.Foreground := defSymbolAttri;
  form1.SynEdit1.SelectedColor.Background := defSelectedColor;
  form1.SynEdit2.SelectedColor.Background := defSelectedColor;
  form1.SynCppSyn1.AsmAttri.Background := defBackgroundColor;
  form1.SynCppSyn1.CommentAttri.Background := defBackgroundColor;
  form1.SynCppSyn1.DirecAttri.Background := defBackgroundColor;
  form1.SynCppSyn1.IdentifierAttri.Background := defBackgroundColor;
  form1.SynCppSyn1.InvalidAttri.Background := defBackgroundColor;
  form1.SynCppSyn1.KeyAttri.Background := defBackgroundColor;
  form1.SynCppSyn1.NumberAttri.Background := defBackgroundColor;
  form1.SynCppSyn1.StringAttri.Background := defBackgroundColor;
  form1.SynCppSyn1.SymbolAttri.Background := defBackgroundColor;
  form1.SynEdit1.SelectedColor.Foreground := defSelectedForegroundColor;
  form1.SynEdit2.SelectedColor.Foreground := defSelectedForegroundColor;
  // Common
  KlabConfig.HexFile := 'project.hex';
  KlabConfig.MapFile := 'project.map';
  KlabConfig.Logfile := 'klablog.txt';
  KlabConfig.OutFile := 'project.out';
  KlabConfig.CPU := 'atmega32';
  KlabConfig.Clock := 16000000;
  // Compiler
  KlabConfig.CompilerCommand := 'avr-gcc';
  KlabConfig.MCall := false;
  KlabConfig.Wstrict := false;
  KlabConfig.Wall := false;
  KlabConfig.g := false;
  KlabConfig.definecpu := true;
  KlabConfig.optimization := '0';
  KlabConfig.usesearchpath := false;
  KlabConfig.uselibsearchpath := false;
  If Not assigned(KlabConfig.searchpaths) Then KlabConfig.searchpaths := TStringList.create;
  KlabConfig.searchpaths.clear;
  KlabConfig.uselibsearchpath := false;
  If Not assigned(KlabConfig.libsearchpaths) Then KlabConfig.libsearchpaths := TStringList.create;
  KlabConfig.libsearchpaths.clear;
  // Linker
  KlabConfig.LinkerCommand := 'avr-gcc';
  KlabConfig.ObjectCopyCommand := 'avr-objcopy';
  // Programmer
  KlabConfig.OverrideBaud := false;
  KlabConfig.SpecifyJTAGPeriod := false;
  KlabConfig.SpecifyProgrammerType := false;
  KlabConfig.SpecifyConnectionPort := false;
  KlabConfig.SpecifyExternalConfigFile := false;
  KlabConfig.DisableAutoErase := false;
  KlabConfig.DoNotWrite := false;
  KlabConfig.OverrideInvalidSignature := false;
  KlabConfig.CountErases := false;
  KlabConfig.RS232BaudRate := 192000;
  KlabConfig.JTAGPeriod := 50;
  KlabConfig.ProgrammerType := '';
  KlabConfig.ConnectionPort := '';
  KlabConfig.ExternalFile := '';
  KlabConfig.EraseCount := 0;
  // Uart
{$IFDEF LINUX}
  KlabConfig.Uart_Device := '/dev/ttyUSB0';
{$ELSE}
  KlabConfig.Uart_Device := '';
{$ENDIF}
  KlabConfig.Uart_BaudRate := 9600;
  KlabConfig.Uart_Flow_Control := 0; // No Flow Control
  KlabConfig.Uart_Bits := 3; // 8 Bits / Character
  KlabConfig.Uart_Parity := 0; // No Parity
  KlabConfig.Uart_Stop := 0; // 1 Stop Bit
  KlabConfig.Uart_autoDisconnect := false;
  KlabConfig.Uart_autoClean := false;
End;

Function LoadCPU(Filename: String): TCPU;
Var
  s: String;
  f: TIniFile;
Begin
  s := AppPath + 'cpu' + PathDelim + Filename + '.ini';
  If FileExistsUTF8(s) Then Begin
    f := TIniFile.Create(s);
    // Zum Compilieren
    result.mmcu := f.ReadString('General', 'mmcu', '');
    result.maxcpu := f.Readinteger('General', 'maxcpu', 1);
    result.flash := f.Readinteger('General', 'flash', 1);
    result.ram := f.Readinteger('General', 'ram', 1);
    result.eeprom := f.Readinteger('General', 'eeprom', 1);
    result.avrdudename := f.ReadString('General', 'avrdudename', '');
    // Fuse Bits
    result.FuseHigh[7] := f.ReadString('FuseHigh', 'Bit7', 'Bit 7');
    result.FuseHigh[6] := f.ReadString('FuseHigh', 'Bit6', 'Bit 6');
    result.FuseHigh[5] := f.ReadString('FuseHigh', 'Bit5', 'Bit 5');
    result.FuseHigh[4] := f.ReadString('FuseHigh', 'Bit4', 'Bit 4');
    result.FuseHigh[3] := f.ReadString('FuseHigh', 'Bit3', 'Bit 3');
    result.FuseHigh[2] := f.ReadString('FuseHigh', 'Bit2', 'Bit 2');
    result.FuseHigh[1] := f.ReadString('FuseHigh', 'Bit1', 'Bit 1');
    result.FuseHigh[0] := f.ReadString('FuseHigh', 'Bit0', 'Bit 0');
    result.FuseHighEnabled := f.ReadInteger('FuseHigh', 'ENABLED', 255);

    result.FuseLow[7] := f.ReadString('FuseLow', 'Bit7', 'Bit 7');
    result.FuseLow[6] := f.ReadString('FuseLow', 'Bit6', 'Bit 6');
    result.FuseLow[5] := f.ReadString('FuseLow', 'Bit5', 'Bit 5');
    result.FuseLow[4] := f.ReadString('FuseLow', 'Bit4', 'Bit 4');
    result.FuseLow[3] := f.ReadString('FuseLow', 'Bit3', 'Bit 3');
    result.FuseLow[2] := f.ReadString('FuseLow', 'Bit2', 'Bit 2');
    result.FuseLow[1] := f.ReadString('FuseLow', 'Bit1', 'Bit 1');
    result.FuseLow[0] := f.ReadString('FuseLow', 'Bit0', 'Bit 0');
    result.FuseLowEnabled := f.ReadInteger('FuseLow', 'ENABLED', 255);

    result.FuseExtended[7] := f.ReadString('FuseExtended', 'Bit7', 'Bit 7');
    result.FuseExtended[6] := f.ReadString('FuseExtended', 'Bit6', 'Bit 6');
    result.FuseExtended[5] := f.ReadString('FuseExtended', 'Bit5', 'Bit 5');
    result.FuseExtended[4] := f.ReadString('FuseExtended', 'Bit4', 'Bit 4');
    result.FuseExtended[3] := f.ReadString('FuseExtended', 'Bit3', 'Bit 3');
    result.FuseExtended[2] := f.ReadString('FuseExtended', 'Bit2', 'Bit 2');
    result.FuseExtended[1] := f.ReadString('FuseExtended', 'Bit1', 'Bit 1');
    result.FuseExtended[0] := f.ReadString('FuseExtended', 'Bit0', 'Bit 0');
    result.FuseExtendedEnabled := f.ReadInteger('FuseExtended', 'ENABLED', 255);

    result.FuseLock[7] := f.ReadString('FuseLock', 'Bit7', 'Bit 7');
    result.FuseLock[6] := f.ReadString('FuseLock', 'Bit6', 'Bit 6');
    result.FuseLock[5] := f.ReadString('FuseLock', 'Bit5', 'Bit 5');
    result.FuseLock[4] := f.ReadString('FuseLock', 'Bit4', 'Bit 4');
    result.FuseLock[3] := f.ReadString('FuseLock', 'Bit3', 'Bit 3');
    result.FuseLock[2] := f.ReadString('FuseLock', 'Bit2', 'Bit 2');
    result.FuseLock[1] := f.ReadString('FuseLock', 'Bit1', 'Bit 1');
    result.FuseLock[0] := f.ReadString('FuseLock', 'Bit0', 'Bit 0');
    result.FuseLockEnabled := f.ReadInteger('FuseLock', 'ENABLED', 255);

    f.free;
  End
  Else Begin
    Raise Exception.Create('Error could not find : ' + s);
  End;
End;

Procedure LoadEditorSettings(Const Inifile: TIniFile);
Var
  i, j: integer;
  s: String;
Begin
  // Common
  KlabConfig.HexFile := Inifile.ReadString('Common', 'HexFile', KlabConfig.HexFile);
  KlabConfig.MapFile := Inifile.ReadString('Common', 'MapFile', KlabConfig.MapFile);
  KlabConfig.Logfile := Inifile.ReadString('Common', 'Logfile', KlabConfig.Logfile);
  KlabConfig.OutFile := Inifile.ReadString('Common', 'Outfile', KlabConfig.OutFile);
  KlabConfig.CPU := Inifile.ReadString('Common', 'CPU', KlabConfig.CPU);
  KlabConfig.Clock := Inifile.Readinteger('Common', 'Clock', KlabConfig.Clock);
  // CodeStyle
  // Todo : Die Font Style der jeweiligen muss noch gespeichert werden !!
  KlabConfig.FontSize := Inifile.Readinteger('CodeStyle', 'FontSize', KlabConfig.FontSize);
  form1.SynEdit1.font.Size := KlabConfig.FontSize;
  form1.SynEdit2.font.Size := KlabConfig.FontSize;
  form1.SynCppSyn1.AsmAttri.Foreground := StringToColor(Inifile.ReadString('CodeStyle', 'AsmAttriF', ColorToString(form1.SynCppSyn1.AsmAttri.Foreground)));
  form1.SynCppSyn1.CommentAttri.Foreground := StringToColor(Inifile.ReadString('CodeStyle', 'CommentAttriF', ColorToString(form1.SynCppSyn1.CommentAttri.Foreground)));
  form1.SynCppSyn1.DirecAttri.Foreground := StringToColor(Inifile.ReadString('CodeStyle', 'DirecAttriF', ColorToString(form1.SynCppSyn1.DirecAttri.Foreground)));
  form1.SynCppSyn1.IdentifierAttri.Foreground := StringToColor(Inifile.ReadString('CodeStyle', 'IdentifierAttriF', ColorToString(form1.SynCppSyn1.IdentifierAttri.Foreground)));
  form1.SynCppSyn1.InvalidAttri.Foreground := StringToColor(Inifile.ReadString('CodeStyle', 'InvalidAttriF', ColorToString(form1.SynCppSyn1.InvalidAttri.Foreground)));
  form1.SynCppSyn1.KeyAttri.Foreground := StringToColor(Inifile.ReadString('CodeStyle', 'KeyAttriF', ColorToString(form1.SynCppSyn1.KeyAttri.Foreground)));
  form1.SynCppSyn1.NumberAttri.Foreground := StringToColor(Inifile.ReadString('CodeStyle', 'NumberAttriF', ColorToString(form1.SynCppSyn1.NumberAttri.Foreground)));
  form1.SynEdit1.RightEdgeColor := StringToColor(Inifile.ReadString('CodeStyle', 'RightEdgeColor', ColorToString(form1.SynEdit1.RightEdgeColor)));
  form1.SynEdit2.RightEdgeColor := StringToColor(Inifile.ReadString('CodeStyle', 'RightEdgeColor', ColorToString(form1.SynEdit2.RightEdgeColor)));
  form1.SynEdit1.Color := StringToColor(Inifile.ReadString('CodeStyle', 'SpaceAttri', ColorToString(form1.SynEdit1.Color)));
  form1.SynEdit2.Color := StringToColor(Inifile.ReadString('CodeStyle', 'SpaceAttri', ColorToString(form1.SynEdit2.Color)));
  form1.SynCppSyn1.StringAttri.Foreground := StringToColor(Inifile.ReadString('CodeStyle', 'StringAttriF', ColorToString(form1.SynCppSyn1.StringAttri.Foreground)));
  form1.SynCppSyn1.SymbolAttri.Foreground := StringToColor(Inifile.ReadString('CodeStyle', 'SymbolAttriF', ColorToString(form1.SynCppSyn1.SymbolAttri.Foreground)));
  form1.SynEdit1.SelectedColor.Background := StringToColor(Inifile.ReadString('CodeStyle', 'SelectedColorB', ColorToString(form1.SynEdit1.SelectedColor.Background)));
  form1.SynEdit2.SelectedColor.Background := StringToColor(Inifile.ReadString('CodeStyle', 'SelectedColorB', ColorToString(form1.SynEdit2.SelectedColor.Background)));
  form1.SynCppSyn1.AsmAttri.Background := StringToColor(Inifile.ReadString('CodeStyle', 'AsmAttriB', ColorToString(form1.SynCppSyn1.AsmAttri.Background)));
  form1.SynCppSyn1.CommentAttri.Background := StringToColor(Inifile.ReadString('CodeStyle', 'CommentAttriB', ColorToString(form1.SynCppSyn1.CommentAttri.Background)));
  form1.SynCppSyn1.DirecAttri.Background := StringToColor(Inifile.ReadString('CodeStyle', 'DirecAttriB', ColorToString(form1.SynCppSyn1.DirecAttri.Background)));
  form1.SynCppSyn1.IdentifierAttri.Background := StringToColor(Inifile.ReadString('CodeStyle', 'IdentifierAttriB', ColorToString(form1.SynCppSyn1.IdentifierAttri.Background)));
  form1.SynCppSyn1.InvalidAttri.Background := StringToColor(Inifile.ReadString('CodeStyle', 'InvalidAttriB', ColorToString(form1.SynCppSyn1.InvalidAttri.Background)));
  form1.SynCppSyn1.KeyAttri.Background := StringToColor(Inifile.ReadString('CodeStyle', 'KeyAttriB', ColorToString(form1.SynCppSyn1.KeyAttri.Background)));
  form1.SynCppSyn1.NumberAttri.Background := StringToColor(Inifile.ReadString('CodeStyle', 'NumberAttriB', ColorToString(form1.SynCppSyn1.NumberAttri.Background)));
  form1.SynCppSyn1.StringAttri.Background := StringToColor(Inifile.ReadString('CodeStyle', 'StringAttriB', ColorToString(form1.SynCppSyn1.StringAttri.Background)));
  form1.SynCppSyn1.SymbolAttri.Background := StringToColor(Inifile.ReadString('CodeStyle', 'SymbolAttriB', ColorToString(form1.SynCppSyn1.SymbolAttri.Background)));
  form1.SynEdit1.SelectedColor.Foreground := StringToColor(Inifile.ReadString('CodeStyle', 'SelectedColorF', ColorToString(form1.SynEdit1.SelectedColor.Foreground)));
  form1.SynEdit2.SelectedColor.Foreground := StringToColor(Inifile.ReadString('CodeStyle', 'SelectedColorF', ColorToString(form1.SynEdit2.SelectedColor.Foreground)));
  // Compiler
  KlabConfig.CompilerCommand := Inifile.ReadString('Compiler', 'CompilerCommand', KlabConfig.CompilerCommand);
  KlabConfig.MCall := Inifile.ReadBool('Compiler', 'mcall', KlabConfig.MCall);
  KlabConfig.Wstrict := Inifile.ReadBool('Compiler', 'Wstrict', KlabConfig.Wstrict);
  KlabConfig.Wall := Inifile.ReadBool('Compiler', 'Wall', KlabConfig.Wall);
  KlabConfig.g := Inifile.ReadBool('Compiler', 'g', KlabConfig.g);
  KlabConfig.definecpu := Inifile.ReadBool('Compiler', 'definecpu', KlabConfig.definecpu);
  KlabConfig.optimization := Inifile.ReadString('Compiler', 'optimization', KlabConfig.optimization)[1];
  KlabConfig.usesearchpath := Inifile.ReadBool('Compiler', 'usesearchpath', KlabConfig.usesearchpath);
  KlabConfig.searchpaths.clear;
  i := Inifile.ReadInteger('SearchPaths', 'Count', 0);
  For j := 0 To i - 1 Do Begin
    s := Inifile.ReadString('SearchPaths', 'Path' + inttostr(j + 1), '');
    KlabConfig.searchpaths.Add(s);
  End;
  KlabConfig.uselibsearchpath := Inifile.ReadBool('Compiler', 'uselibsearchpath', KlabConfig.uselibsearchpath);
  KlabConfig.libsearchpaths.clear;
  i := Inifile.ReadInteger('LibSearchPaths', 'Count', 0);
  For j := 0 To i - 1 Do Begin
    s := Inifile.ReadString('LibSearchPaths', 'Path' + inttostr(j + 1), '');
    KlabConfig.Libsearchpaths.Add(s);
  End;
  // Linker
  KlabConfig.LinkerCommand := Inifile.ReadString('Linker', 'LinkerCommand', KlabConfig.LinkerCommand);
  KlabConfig.ObjectCopyCommand := Inifile.ReadString('Linker', 'ObjectCopyCommand', KlabConfig.ObjectCopyCommand);
  // Programmer
  KlabConfig.OverrideBaud := Inifile.ReadBool('Programmer', 'OverrideBaud', KlabConfig.OverrideBaud);
  KlabConfig.SpecifyJTAGPeriod := Inifile.ReadBool('Programmer', 'SpecifyJTAGPeriod', KlabConfig.SpecifyJTAGPeriod);
  KlabConfig.SpecifyProgrammerType := Inifile.ReadBool('Programmer', 'SpecifyProgrammerType', KlabConfig.SpecifyProgrammerType);
  KlabConfig.SpecifyConnectionPort := Inifile.ReadBool('Programmer', 'SpecifyConnectionPort', KlabConfig.SpecifyConnectionPort);
  KlabConfig.SpecifyExternalConfigFile := Inifile.ReadBool('Programmer', 'SpecifyExternalConfigFile', KlabConfig.SpecifyExternalConfigFile);
  KlabConfig.DisableAutoErase := Inifile.ReadBool('Programmer', 'DisableAutoErase', KlabConfig.DisableAutoErase);
  KlabConfig.DoNotWrite := Inifile.ReadBool('Programmer', 'DoNotWrite', KlabConfig.DoNotWrite);
  KlabConfig.OverrideInvalidSignature := Inifile.ReadBool('Programmer', 'OverrideInvalidSignature', KlabConfig.OverrideInvalidSignature);
  KlabConfig.CountErases := Inifile.ReadBool('Programmer', 'CountErases', KlabConfig.CountErases);
  KlabConfig.RS232BaudRate := Inifile.ReadInteger('Programmer', 'RS232BaudRate', KlabConfig.RS232BaudRate);
  KlabConfig.JTAGPeriod := Inifile.ReadInteger('Programmer', 'JTAGPeriod', KlabConfig.JTAGPeriod);
  KlabConfig.ProgrammerType := Inifile.ReadString('Programmer', 'ProgrammerType', KlabConfig.ProgrammerType);
  KlabConfig.ConnectionPort := Inifile.ReadString('Programmer', 'ConnectionPort', KlabConfig.ConnectionPort);
  KlabConfig.ExternalFile := Inifile.ReadString('Programmer', 'ExternalFile', KlabConfig.ExternalFile);
  KlabConfig.EraseCount := Inifile.ReadInteger('Programmer', 'EraseCount', KlabConfig.EraseCount);
  // Uart
  KlabConfig.Uart_Device := Inifile.ReadString('Uart', 'Device', KlabConfig.Uart_Device);
  KlabConfig.Uart_BaudRate := Inifile.ReadInteger('Uart', 'BaudRate', KlabConfig.Uart_BaudRate);
  KlabConfig.Uart_Flow_Control := Inifile.ReadInteger('Uart', 'Flow_Control', KlabConfig.Uart_Flow_Control);
  KlabConfig.Uart_Bits := Inifile.ReadInteger('Uart', 'Bits', KlabConfig.Uart_Bits);
  KlabConfig.Uart_Parity := Inifile.ReadInteger('Uart', 'Parity', KlabConfig.Uart_Parity);
  KlabConfig.Uart_Stop := Inifile.ReadInteger('Uart', 'Stop', KlabConfig.Uart_Stop);
  KlabConfig.Uart_autoDisconnect := Inifile.ReadBool('Uart', 'Disconnect', KlabConfig.Uart_autoDisconnect);
  KlabConfig.Uart_autoClean := Inifile.ReadBool('Uart', 'AutoClean', KlabConfig.Uart_autoClean);
  // laden der Aktuellen CPU
  cpu := LoadCPU(KlabConfig.CPU);
  // Konfiguration Uart
  form1.ComboBox1.text := KlabConfig.Uart_Device;
  form1.ComboBox2.text := inttostr(KlabConfig.Uart_BaudRate);
  form1.ComboBox3.ItemIndex := KlabConfig.Uart_Flow_Control;
  form1.ComboBox4.ItemIndex := KlabConfig.Uart_Stop;
  form1.ComboBox5.ItemIndex := KlabConfig.Uart_Parity;
  form1.ComboBox6.ItemIndex := KlabConfig.Uart_Bits;
  form1.CheckBox1.Checked := KlabConfig.Uart_autoDisconnect;
  form1.CheckBox2.Checked := KlabConfig.Uart_autoClean;
  // Zuletzt geöffnetes Project
  AktualProjectFilename := Inifile.ReadString('IDE', 'LastOpenedProject', AktualProjectFilename);
End;

Procedure SaveEditorSettings(Const Inifile: TIniFile);
Var
  j: integer;
Begin
  // Common
  Inifile.WriteString('Common', 'HexFile', KlabConfig.HexFile);
  Inifile.WriteString('Common', 'MapFile', KlabConfig.MapFile);
  Inifile.WriteString('Common', 'Logfile', KlabConfig.Logfile);
  Inifile.WriteString('Common', 'Outfile', KlabConfig.OutFile);
  Inifile.WriteString('Common', 'CPU', KlabConfig.CPU);
  Inifile.Writeinteger('Common', 'Clock', KlabConfig.Clock);
  // Code Style
  Inifile.Writeinteger('CodeStyle', 'FontSize', KlabConfig.FontSize);
  Inifile.WriteString('CodeStyle', 'AsmAttriF', ColorToString(form1.SynCppSyn1.AsmAttri.Foreground));
  Inifile.WriteString('CodeStyle', 'CommentAttriF', ColorToString(form1.SynCppSyn1.CommentAttri.Foreground));
  Inifile.WriteString('CodeStyle', 'DirecAttriF', ColorToString(form1.SynCppSyn1.DirecAttri.Foreground));
  Inifile.WriteString('CodeStyle', 'IdentifierAttriF', ColorToString(form1.SynCppSyn1.IdentifierAttri.Foreground));
  Inifile.WriteString('CodeStyle', 'InvalidAttriF', ColorToString(form1.SynCppSyn1.InvalidAttri.Foreground));
  Inifile.WriteString('CodeStyle', 'KeyAttriF', ColorToString(form1.SynCppSyn1.KeyAttri.Foreground));
  Inifile.WriteString('CodeStyle', 'NumberAttriF', ColorToString(form1.SynCppSyn1.NumberAttri.Foreground));
  Inifile.WriteString('CodeStyle', 'RightEdgeColor', ColorToString(form1.SynEdit1.RightEdgeColor));
  Inifile.WriteString('CodeStyle', 'SpaceAttri', ColorToString(form1.SynEdit1.Color));
  Inifile.WriteString('CodeStyle', 'StringAttriF', ColorToString(form1.SynCppSyn1.StringAttri.Foreground));
  Inifile.WriteString('CodeStyle', 'SymbolAttriF', ColorToString(form1.SynCppSyn1.SymbolAttri.Foreground));
  Inifile.WriteString('CodeStyle', 'SelectedColorB', ColorToString(form1.SynEdit1.SelectedColor.Background));
  Inifile.WriteString('CodeStyle', 'AsmAttriB', ColorToString(form1.SynCppSyn1.AsmAttri.Background));
  Inifile.WriteString('CodeStyle', 'CommentAttriB', ColorToString(form1.SynCppSyn1.CommentAttri.Background));
  Inifile.WriteString('CodeStyle', 'DirecAttriB', ColorToString(form1.SynCppSyn1.DirecAttri.Background));
  Inifile.WriteString('CodeStyle', 'IdentifierAttriB', ColorToString(form1.SynCppSyn1.IdentifierAttri.Background));
  Inifile.WriteString('CodeStyle', 'InvalidAttriB', ColorToString(form1.SynCppSyn1.InvalidAttri.Background));
  Inifile.WriteString('CodeStyle', 'KeyAttriB', ColorToString(form1.SynCppSyn1.KeyAttri.Background));
  Inifile.WriteString('CodeStyle', 'NumberAttriB', ColorToString(form1.SynCppSyn1.NumberAttri.Background));
  Inifile.WriteString('CodeStyle', 'StringAttriB', ColorToString(form1.SynCppSyn1.StringAttri.Background));
  Inifile.WriteString('CodeStyle', 'SymbolAttriB', ColorToString(form1.SynCppSyn1.SymbolAttri.Background));
  Inifile.WriteString('CodeStyle', 'SelectedColorF', ColorToString(form1.SynEdit1.SelectedColor.Foreground));
  // Compiler
  Inifile.WriteString('Compiler', 'CompilerCommand', KlabConfig.CompilerCommand);
  Inifile.WriteBool('Compiler', 'mcall', KlabConfig.MCall);
  Inifile.WriteBool('Compiler', 'Wstrict', KlabConfig.Wstrict);
  Inifile.WriteBool('Compiler', 'Wall', KlabConfig.Wall);
  Inifile.WriteBool('Compiler', 'g', KlabConfig.g);
  Inifile.WriteBool('Compiler', 'definecpu', KlabConfig.definecpu);
  Inifile.WriteString('Compiler', 'optimization', KlabConfig.optimization);
  Inifile.WriteBool('Compiler', 'usesearchpath', KlabConfig.usesearchpath);
  Inifile.WriteBool('Compiler', 'uselibsearchpath', KlabConfig.uselibsearchpath);
  Inifile.EraseSection('SearchPaths');
  Inifile.WriteInteger('SearchPaths', 'Count', KlabConfig.searchpaths.count);
  For j := 0 To KlabConfig.searchpaths.count - 1 Do Begin
    Inifile.WriteString('SearchPaths', 'Path' + inttostr(j + 1), KlabConfig.searchpaths[j]);
  End;
  KlabConfig.uselibsearchpath := Inifile.ReadBool('Compiler', 'uselibsearchpath', KlabConfig.uselibsearchpath);
  Inifile.EraseSection('LibSearchPaths');
  Inifile.WriteInteger('LibSearchPaths', 'Count', KlabConfig.libsearchpaths.count);
  For j := 0 To KlabConfig.libsearchpaths.count - 1 Do Begin
    Inifile.WriteString('LibSearchPaths', 'Path' + inttostr(j + 1), KlabConfig.libsearchpaths[j]);
  End;
  // Linker
  Inifile.WriteString('Linker', 'LinkerCommand', KlabConfig.LinkerCommand);
  Inifile.WriteString('Linker', 'ObjectCopyCommand', KlabConfig.ObjectCopyCommand);
  // Programmer
  Inifile.WriteBool('Programmer', 'OverrideBaud', KlabConfig.OverrideBaud);
  Inifile.WriteBool('Programmer', 'SpecifyJTAGPeriod', KlabConfig.SpecifyJTAGPeriod);
  Inifile.WriteBool('Programmer', 'SpecifyProgrammerType', KlabConfig.SpecifyProgrammerType);
  Inifile.WriteBool('Programmer', 'SpecifyConnectionPort', KlabConfig.SpecifyConnectionPort);
  Inifile.WriteBool('Programmer', 'SpecifyExternalConfigFile', KlabConfig.SpecifyExternalConfigFile);
  Inifile.WriteBool('Programmer', 'DisableAutoErase', KlabConfig.DisableAutoErase);
  Inifile.WriteBool('Programmer', 'DoNotWrite', KlabConfig.DoNotWrite);
  Inifile.WriteBool('Programmer', 'OverrideInvalidSignature', KlabConfig.OverrideInvalidSignature);
  Inifile.WriteBool('Programmer', 'CountErases', KlabConfig.CountErases);
  Inifile.WriteInteger('Programmer', 'RS232BaudRate', KlabConfig.RS232BaudRate);
  Inifile.WriteInteger('Programmer', 'JTAGPeriod', KlabConfig.JTAGPeriod);
  Inifile.WriteString('Programmer', 'ProgrammerType', KlabConfig.ProgrammerType);
  Inifile.WriteString('Programmer', 'ConnectionPort', KlabConfig.ConnectionPort);
  Inifile.WriteString('Programmer', 'ExternalFile', KlabConfig.ExternalFile);
  Inifile.WriteInteger('Programmer', 'EraseCount', KlabConfig.EraseCount);
  // Uart
  Inifile.WriteString('Uart', 'Device', KlabConfig.Uart_Device);
  Inifile.WriteInteger('Uart', 'BaudRate', KlabConfig.Uart_BaudRate);
  Inifile.WriteInteger('Uart', 'Flow_Control', KlabConfig.Uart_Flow_Control);
  Inifile.WriteInteger('Uart', 'Bits', KlabConfig.Uart_Bits);
  Inifile.WriteInteger('Uart', 'Parity', KlabConfig.Uart_Parity);
  Inifile.WriteInteger('Uart', 'Stop', KlabConfig.Uart_Stop);
  Inifile.WriteBool('Uart', 'Disconnect', KlabConfig.Uart_autoDisconnect);
  Inifile.WriteBool('Uart', 'AutoClean', KlabConfig.Uart_autoClean);
End;

Function ListOfAllCFiles: TStringList;
Var
  i: Integer;
Begin
  result := TStringList.Create;
  result.Clear;
  For i := 0 To high(OpenedFiles) Do Begin
    If lowercase(ExtractFileExt(OpenedFiles[i].Filename)) = '.c' Then
      result.Add(OpenedFiles[i].Filename);
  End;
End;

Function ExecuteCommand(Command, Params: String): String;
Var
  sl: TStringList;
  AProcess: TProcess;
  i: Integer;
{$IFDEF WINDOWS}
  MemStream: TMemoryStream;
  BytesRead: LongInt;
  NumBytes: LongInt;
Const
  READ_BYTES = 2048;
Begin
  If length(Command) = 0 Then exit;
  MemStream := TMemoryStream.Create;
  BytesRead := 0;
  AddMessage(Command + ' ' + Params);
  // Aufruf der Lese Software
  AProcess := TProcess.Create(Nil);
  AProcess.Options := [poUsePipes, poNoConsole, poStderrToOutPut];
  AProcess.Executable := utf8tosys(Command);
  AProcess.Parameters.Delimiter := ' '; // Das Sorgt dafür, dass die Parameter keine Leerzeichen haben dürfen !!
  AProcess.Parameters.DelimitedText := utf8tosys(Params);
  AProcess.Execute;
  While AProcess.Running Do Begin
    // make sure we have room
    MemStream.SetSize(BytesRead + READ_BYTES);
    // try reading it
    NumBytes := AProcess.Output.Read((MemStream.Memory + BytesRead)^, READ_BYTES);
    If NumBytes > 0 Then Begin
      Inc(BytesRead, NumBytes);
      Application.ProcessMessages;
    End
    Else Begin
      // no data, wait 100 ms
      Application.ProcessMessages;
      Sleep(100);
    End;
  End;
  // read last part
  Repeat
    // make sure we have room
    MemStream.SetSize(BytesRead + READ_BYTES);
    // try reading it
    NumBytes := AProcess.Output.Read((MemStream.Memory + BytesRead)^, READ_BYTES);
    If NumBytes > 0 Then Begin
      Inc(BytesRead, NumBytes);
      Application.ProcessMessages;
    End;
  Until NumBytes <= 0;
  MemStream.SetSize(BytesRead);
  sl := TStringList.Create;
  sl.LoadFromStream(MemStream);
  AProcess.free;
{$ELSE}
Begin
  If length(Command + Params) = 0 Then exit;
  // Entnommen aus  : http://wiki.lazarus.freepascal.org/Executing_External_Programs/de#Ein_verbessertes_Beispiel
  AProcess := TProcess.Create(Nil);
  AddMessage(Command + ' ' + Params);
  //AProcess.CommandLine := Command + ' >>' + KlabConfig.Logfile;
  AProcess.CommandLine := 'bash -c "' + Command + ' ' + Params + ' >' + KlabConfig.Logfile + ' 2>&1"';
  AProcess.Options := AProcess.Options + [poNoConsole, poWaitOnExit];
  AProcess.Execute;
  AProcess.Free;
  // Auslesen der Logfile
  sl := TStringList.create;
  sl.LoadFromFile(KlabConfig.Logfile);
{$ENDIF}
  result := sl.text;
  //  If trim(result) <> '' Then
  //    form1.ListBox1.Items.Add(result);
  If sl.count <> 0 Then Begin
    For i := 0 To sl.count - 1 Do Begin
      AddMessage(sl[i]);
    End;
  End;
  sl.free;
End;

Procedure SaveOldData;
// Mit einer Genesteten Routine, muss der eigentliche "save" Code nur 1 mal geschrieben werden.
  Procedure SaveEverythingNeeded(Const synedit: TSynedit; index: integer);
  Begin
    OpenedFiles[Index].Text := synedit.text;
    // Todo : Bookmarks und anderer Einstellungen, des Editor Feldes Speichern
    OpenedFiles[Index].TopLine := synedit.TopLine;
  End;
Begin
  // Speichern der Aktuell Editierten Texte + Aller Anderen Sachen
  If OldLeftIndex <> -1 Then Begin
    SaveEverythingNeeded(form1.SynEdit1, OldLeftIndex);
  End;
  If OldRightIndex <> -1 Then Begin
    SaveEverythingNeeded(form1.SynEdit2, OldRightIndex);
  End;
End;

Function FindIndexbyname(Filename: String): integer;
Var
  i: Integer;
Begin
  If Length(Filename) > 0 Then Begin
    If Filename[length(Filename)] = '*' Then
      Filename := copy(Filename, 1, length(Filename) - 1);
  End;
  result := -1;
  If Length(filename) = 0 Then Exit;
  // Wenn ein Vollständiger Dateiname Übergeben wurde
  For i := 0 To High(OpenedFiles) Do Begin
    If (Filename = OpenedFiles[i].Filename) Then Begin
      result := i;
      Exit;
    End;
  End;
  // Wenn nur ein Dateiname übergeben wurde
  For i := 0 To High(OpenedFiles) Do Begin
    If (Filename = ExtractFileName(OpenedFiles[i].Filename)) Then Begin
      result := i;
      Exit;
    End;
  End;
End;

Function ExecuteCommand(Command: TCommandString): String;
Begin
  result := ExecuteCommand(command.Command, command.Params);
End;

Function CreateCompileCommand(Infile, OutFile: String): TCommandString;
Var
  i: integer;
Begin
  If FileExistsUTF8(OutFile) Then Begin
    If DeleteFileUTF8(OutFile) Then Begin
      // form1.ListBox1.Items.Add('Deleted : ' + OutFile);
    End
    Else Begin
      AddMessage('Could not delete : ' + OutFile);
    End;
  End;
  result.Command := KlabConfig.CompilerCommand;
  result.Params := ' -mmcu=' + cpu.mmcu;
  If KlabConfig.MCall Then
    result.Params := result.Params + ' -mcall-prologues';
  If KlabConfig.Wstrict Then
    result.Params := result.Params + ' -Wstrict-prototypes';
  If KlabConfig.Wall Then
    result.Params := result.Params + ' -Wall';
  If KlabConfig.g Then
    result.Params := result.Params + ' -g';
  result.Params := result.Params + ' -O' + KlabConfig.optimization;
  If KlabConfig.definecpu Then
    result.Params := result.Params + ' -DF_CPU=' + inttostr(KlabConfig.Clock) + 'UL';
  If KlabConfig.usesearchpath Then Begin
    For i := 0 To KlabConfig.searchpaths.Count - 1 Do Begin
      If DirectoryExistsUTF8(KlabConfig.searchpaths[i]) Then Begin
        result.Params := result.Params + ' -I ' + IncludeTrailingPathDelimiter(KlabConfig.searchpaths[i]);
{$IFDEF WINDOWS}
        If pos(' ', IncludeTrailingPathDelimiter(KlabConfig.searchpaths[i])) <> 0 Then Begin
          result.Command := '';
          result.Params := '';
          addmessage(Error_no_Spaces_In_Paths);
          exit;
        End;
{$ENDIF}
      End;
    End;
  End;
  // Todo : Prüfen, ob die LibPfade hier Richtig sind ..
  If KlabConfig.uselibsearchpath Then Begin
    For i := 0 To KlabConfig.libsearchpaths.Count - 1 Do Begin
      If DirectoryExistsUTF8(KlabConfig.libsearchpaths[i]) Then Begin
        result.Params := result.Params + ' -L ' + KlabConfig.libsearchpaths[i];
{$IFDEF WINDOWS}
        If pos(' ', IncludeTrailingPathDelimiter(KlabConfig.libsearchpaths[i])) <> 0 Then Begin
          result.Command := '';
          result.Params := '';
          addmessage(Error_no_Spaces_In_Paths);
          exit;
        End;
{$ENDIF}
      End;
    End;
  End;
  result.Params := result.Params + ' -c ' + Infile;
  result.Params := result.Params + ' -o ' + OutFile;
End;

Function HextoInt(Value: String): integer;
Var
  m: cardinal;
  c: byte;
Begin
  value := uppercase(value);
  result := 0;
  m := 1;
  While length(value) <> 0 Do Begin
    c := ord(value[length(value)]);
    delete(value, length(value), 1);
    If c In [48..57] Then Begin
      result := result + (c - 48) * m;
    End
    Else Begin
      If c In [65..70] Then Begin
        result := result + (c - 55) * m;
      End
      Else Begin
        result := -1; // Anzeigen, dass es keine Hex Zahl ist.
        exit;
      End;
    End;
    m := m * 16;
  End;
End;

Function Check_File_Sizes(Used_CPU: TCPU; Filename: String): Boolean;
Var
  t, s: String;
  i: integer;
Begin
  result := true;
  If fileexistsutf8(filename) Then Begin
    // avr-size liefert die Ausgaben in % diese Parsen wir uns suchen auf Werte > 100.0, dann Fehler
    s := ExecuteCommand('avr-size', '-C --mcu=' + Used_CPU.mmcu + ' ' + filename);
    While (pos('%', s) <> 0) Do Begin
      t := copy(s, 1, pos('%', s) - 1);
      For i := length(t) Downto 1 Do Begin
        If t[i] = '(' Then Begin
          t := copy(t, i + 1, length(t));
          break;
        End;
      End;
      If (pos('.', t) <> 0) And (DefaultFormatSettings.DecimalSeparator <> '.') Then Begin
        t[pos('.', t)] := DefaultFormatSettings.DecimalSeparator;
      End;
      If (pos(',', t) <> 0) And (DefaultFormatSettings.DecimalSeparator <> ',') Then Begin
        t[pos(',', t)] := DefaultFormatSettings.DecimalSeparator;
      End;
      If StrToFloatDef(t, 101) > 100 Then Begin
        result := false;
        exit;
      End;
      delete(s, 1, pos('%', s));
    End;
  End
  Else Begin
    result := false;
  End;
End;

Function CreateLinkerCommand(Infiles: String): TCommandString;
Begin
  result.Command := KlabConfig.LinkerCommand;
  result.Params := infiles;
  result.Params := result.Params + ' -mmcu=' + cpu.mmcu;
  result.Params := result.Params + ' -o ' + AktualProjectWorkPath + KlabConfig.OutFile;
{$IFDEF Windows}
  result.Params := result.Params + ' -Wl,-Map=' + AktualProjectWorkPath + KlabConfig.MapFile;
{$ELSE}
  result.Params := result.Params + ' -Wl,-Map,' + AktualProjectWorkPath + KlabConfig.MapFile;
{$ENDIF}
End;

Function CreateObjectCopyCommand: TCommandString;
Begin
  result.Command := KlabConfig.ObjectCopyCommand;
  result.Params := ' -R .eeprom -O ihex ';
  result.Params := result.Params + AktualProjectWorkPath + KlabConfig.OutFile;
  result.Params := result.Params + ' ' + AktualProjectWorkPath + KlabConfig.HexFile;
End;

Function CreateProgrammCommand(Hexfile: String; Used_CPU: TCPU): TCommandString;
Begin
  result.command := 'avrdude';
  result.Params := '-p ' + Used_CPU.avrdudename;
  If KlabConfig.OverrideBaud Then
    result.Params := result.Params + ' -b ' + inttostr(KlabConfig.RS232BaudRate);
  If KlabConfig.SpecifyJTAGPeriod Then Begin
    result.Params := result.Params + ' -B ' + inttostr(KlabConfig.JTAGPeriod Div 10);
    If KlabConfig.JTAGPeriod Mod 10 <> 0 Then
      result.Params := result.Params + '.' + inttostr(KlabConfig.JTAGPeriod Mod 10);
  End;
  If KlabConfig.SpecifyProgrammerType Then
    result.Params := result.Params + ' -c ' + KlabConfig.ProgrammerType;
  If KlabConfig.SpecifyExternalConfigFile Then
    result.Params := result.Params + ' -C ' + KlabConfig.ExternalFile;
  If KlabConfig.CountErases Then
    result.Params := result.Params + ' -y';
  If KlabConfig.DoNotWrite Then
    result.Params := result.Params + ' -n';
  If KlabConfig.OverrideInvalidSignature Then
    result.Params := result.Params + ' -F';
  If KlabConfig.DisableAutoErase Then
    result.Params := result.Params + ' -D';
  result.Params := result.Params + ' -u -U flash:w:' + Hexfile + ':i';
End;

Function CreateLoadProgrammCommand(Hexfile: String; Used_CPU: TCPU): TCommandString;
Begin
  result.Command := 'avrdude';
  result.Params := '-p ' + Used_CPU.avrdudename;
  If KlabConfig.OverrideBaud Then
    result.Params := result.Params + ' -b ' + inttostr(KlabConfig.RS232BaudRate);
  If KlabConfig.SpecifyJTAGPeriod Then Begin
    result.Params := result.Params + ' -B ' + inttostr(KlabConfig.JTAGPeriod Div 10);
    If KlabConfig.JTAGPeriod Mod 10 <> 0 Then
      result.Params := result.Params + '.' + inttostr(KlabConfig.JTAGPeriod Mod 10);
  End;
  If KlabConfig.SpecifyProgrammerType Then
    result.Params := result.Params + ' -c ' + KlabConfig.ProgrammerType;
  If KlabConfig.SpecifyExternalConfigFile Then
    result.Params := result.Params + ' -C ' + KlabConfig.ExternalFile;
  If KlabConfig.CountErases Then
    result.Params := result.Params + ' -y';
  If KlabConfig.DoNotWrite Then
    result.Params := result.Params + ' -n';
  If KlabConfig.OverrideInvalidSignature Then
    result.Params := result.Params + ' -F';
  If KlabConfig.DisableAutoErase Then
    result.Params := result.Params + ' -D';
  result.Params := result.Params + ' -u -U flash:r:' + Hexfile + ':i';
End;

Function CreateVerifyProgrammCommand(Hexfile: String; Used_CPU: TCPU): TCommandString;
Begin
  result.Command := 'avrdude';
  result.Params := ' -p ' + Used_CPU.avrdudename;
  If KlabConfig.OverrideBaud Then
    result.Params := result.Params + ' -b ' + inttostr(KlabConfig.RS232BaudRate);
  If KlabConfig.SpecifyJTAGPeriod Then Begin
    result.Params := result.Params + ' -B ' + inttostr(KlabConfig.JTAGPeriod Div 10);
    If KlabConfig.JTAGPeriod Mod 10 <> 0 Then
      result.Params := result.Params + '.' + inttostr(KlabConfig.JTAGPeriod Mod 10);
  End;
  If KlabConfig.SpecifyProgrammerType Then
    result.Params := result.Params + ' -c ' + KlabConfig.ProgrammerType;
  If KlabConfig.SpecifyExternalConfigFile Then
    result.Params := result.Params + ' -C ' + KlabConfig.ExternalFile;
  If KlabConfig.CountErases Then
    result.Params := result.Params + ' -y';
  If KlabConfig.DoNotWrite Then
    result.Params := result.Params + ' -n';
  If KlabConfig.OverrideInvalidSignature Then
    result.Params := result.Params + ' -F';
  If KlabConfig.DisableAutoErase Then
    result.Params := result.Params + ' -D';
  result.Params := result.Params + ' -u -U flash:v:' + Hexfile + ':i';
End;

Function CreateEraseCommand(Used_CPU: TCPU): TCommandString;
Begin
  result.Command := 'avrdude';
  result.Params := '-p ' + Used_CPU.avrdudename;
  If KlabConfig.OverrideBaud Then
    result.Params := result.Params + ' -b ' + inttostr(KlabConfig.RS232BaudRate);
  If KlabConfig.SpecifyJTAGPeriod Then Begin
    result.Params := result.Params + ' -B ' + inttostr(KlabConfig.JTAGPeriod Div 10);
    If KlabConfig.JTAGPeriod Mod 10 <> 0 Then
      result.Params := result.Params + '.' + inttostr(KlabConfig.JTAGPeriod Mod 10);
  End;
  If KlabConfig.SpecifyProgrammerType Then
    result.Params := result.Params + ' -c ' + KlabConfig.ProgrammerType;
  If KlabConfig.SpecifyExternalConfigFile Then
    result.Params := result.Params + ' -C ' + KlabConfig.ExternalFile;
  If KlabConfig.CountErases Then
    result.Params := result.Params + ' -y';
  If KlabConfig.DoNotWrite Then
    result.Params := result.Params + ' -n';
  If KlabConfig.OverrideInvalidSignature Then
    result.Params := result.Params + ' -F';
  If KlabConfig.DisableAutoErase Then
    result.Params := result.Params + ' -D';
  result.Params := result.Params + ' -u -e';
End;

Function CreateReadFuseCommand(TargetFile: String; kind: TFuseType; usedCPU: TCPU): TCommandString;
Begin
  result.Command := 'avrdude';
  result.Params := '-p ' + usedCPU.avrdudename;
  If KlabConfig.SpecifyProgrammerType Then
    result.Params := result.Params + ' -c ' + KlabConfig.ProgrammerType;
  If KlabConfig.SpecifyExternalConfigFile Then
    result.Params := result.Params + ' -C ' + KlabConfig.ExternalFile;
  If KlabConfig.DoNotWrite Then
    result.Params := result.Params + ' -n';
  result.Params := result.Params + ' -u -U ';
  Case kind Of
    lowfuse: result.Params := result.Params + 'lfuse:r:';
    highfuse: result.Params := result.Params + 'hfuse:r:';
    lockfuse: result.Params := result.Params + 'lock:r:';
    extendedfuse: result.Params := result.Params + 'efuse:r:';
  End;
  result.Params := result.Params + TargetFile + ':r';
End;

Function CreateWriteFuseCommand(FuseValue: byte; kind: TFuseType; usedCPU: TCPU
  ): TCommandString;
Begin
  result.Command := 'avrdude';
  result.Params := '-p ' + usedCPU.avrdudename;
  If KlabConfig.SpecifyProgrammerType Then
    result.Params := result.Params + ' -c ' + KlabConfig.ProgrammerType;
  If KlabConfig.SpecifyExternalConfigFile Then
    result.Params := result.Params + ' -C ' + KlabConfig.ExternalFile;
  If KlabConfig.DoNotWrite Then
    result.Params := result.Params + ' -n';
  result.Params := result.Params + ' -u -U ';
  Case kind Of
    lowfuse: result.Params := result.Params + 'lfuse:w:0x';
    highfuse: result.Params := result.Params + 'hfuse:w:0x';
    lockfuse: result.Params := result.Params + 'lock:w:0x';
    extendedfuse: result.Params := result.Params + 'efuse:w:0x';
  End;
  result.Params := result.Params + lowercase(format('%.2x', [FuseValue])) + ':m';
End;

Procedure AddAviableCPUs;
Var
  sr: TSearchRec;
  ADirectory: String;
Begin
  ADirectory := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStrUTF8(0))) + 'cpu' + PathDelim;
  If (FindFirstutf8(ADirectory + '*.ini', faAnyFile, SR) = 0) Then Begin
    Repeat
      KlabConfig.Aviable_CPUS.Add(ExtractFileNameOnly(sr.Name));
    Until FindNextutf8(SR) <> 0;
  End;
  FindCloseutf8(SR);
End;

Procedure LoadEditorProperties;
Var
  f: TIniFile;
  s: String;
Begin
  LoadDefaultEditorSettings;
  // Laden der Verfügbaren Ziel CPU's
  If Not assigned(KlabConfig.Aviable_CPUS) Then
    KlabConfig.Aviable_CPUS := TStringList.Create;
  If Not assigned(KlabConfig.searchpaths) Then
    KlabConfig.searchpaths := TStringList.Create;
  If Not assigned(KlabConfig.libsearchpaths) Then
    KlabConfig.libsearchpaths := TStringList.Create;
  KlabConfig.Aviable_CPUS.Clear;
  KlabConfig.searchpaths.Clear;
  KlabConfig.libsearchpaths.clear;
  AddAviableCPUs;
  // Laden KLab Config
  s := AppPath + 'klab.cfg';
  f := TIniFile.Create(s);
  LoadEditorSettings(f);
  f.free;
End;

Procedure SaveEditorProperties;
Var
  s: String;
  f: TIniFile;
Begin
  s := AppPath + 'klab.cfg';
  f := TIniFile.Create(s);
  SaveEditorSettings(f);
  f.free;
End;

Procedure SaveIDELastOpened(Filename: String);
Var
  s: String;
  f: TIniFile;
Begin
  s := AppPath + 'klab.cfg';
  f := TIniFile.Create(s);
  // Zuletzt geöffnetes Project
  f.WriteString('IDE', 'LastOpenedProject', Filename);
  f.free;
End;

Procedure Editorchange;
Begin
  If Not GlobalChanged Then Begin
    form1.caption := form1.caption + '*';
  End;
  GlobalChanged := true;
  If AktualEditor = 0 Then Begin // Links hat sich was geändert
    If Not OpenedFiles[OldLeftIndex].Changed Then Begin
      Form1.tabControl1.tabs[Form1.tabControl1.tabIndex] := Form1.tabControl1.tabs[Form1.tabControl1.tabIndex] + '*';
    End;
    OpenedFiles[OldLeftIndex].Changed := true;
  End
  Else Begin // Rechts hat sich was geändert
    If Not OpenedFiles[OldRightIndex].Changed Then Begin
      Form1.TabControl2.tabs[Form1.TabControl2.tabIndex] := Form1.TabControl2.tabs[Form1.TabControl2.tabIndex] + '*';
    End;
    OpenedFiles[OldRightIndex].Changed := true;
  End;
End;

Procedure EditorReload;
Var
  s: String;
Begin
  If AktualEditor = 0 Then Begin
    s := OpenedFiles[OldLeftIndex].Filename;
    If (s[length(s)] = '*') Then
      s := copy(s, 1, length(s) - 1);
    form1.SynEdit1.Lines.LoadFromFile(utf8tosys(s));
  End
  Else Begin
    s := OpenedFiles[OldRightIndex].Filename;
    If (s[length(s)] = '*') Then
      s := copy(s, 1, length(s) - 1);
    form1.SynEdit2.Lines.LoadFromFile(utf8tosys(s));
  End;
End;

Procedure LoadFile(Filename: String);
Begin
  If lowercase(ExtractFileExt(filename)) = '.klab' Then Begin
    LoadProject(filename);
    If form1.caption[length(form1.caption)] = '*' Then
      form1.caption := copy(form1.caption, 1, length(form1.caption) - 1);
    GlobalChanged := false;
    form1.SaveDialog1.InitialDir := AktualProjectWorkPath;
    form1.SaveDialog2.InitialDir := AktualProjectWorkPath;
    form1.OpenDialog1.InitialDir := AktualProjectWorkPath;
    form1.OpenDialog2.InitialDir := AktualProjectWorkPath;
    If assigned(form4) Then
      form4.OpenDialog1.InitialDir := AktualProjectWorkPath;
  End;
  If lowercase(ExtractFileExt(filename)) = '.c' Then Begin
    If AktualProjectFilename = '' Then Begin
      showmessage('Error, you need first to create a project.');
      exit;
    End;
    OpenSourceFile(filename, 0);
  End;
  If lowercase(ExtractFileExt(filename)) = '.h' Then Begin
    If AktualProjectFilename = '' Then Begin
      showmessage('Error, you need first to create a project.');
      exit;
    End;
    OpenSourceFile(filename, 0);
  End;
End;

Procedure RemoveFileFromProject(Filename: String); // Entfernt einen Dateinamen aus dem Projekt
Var
  j, i: integer;
  s: String;
  child, p: TTreeNode;
Begin
  // 1. Sicherstellen, dass die entsprechende Datei nicht "göffnet" ist
  s := ExtractFileName(filename);
  For i := 0 To form1.TabControl1.Tabs.Count - 1 Do Begin
    If s = form1.TabControl1.Tabs[i] Then Begin
      showmessage('Error, you need to close "' + s + '" first.');
      exit;
    End;
  End;
  For i := 0 To form1.TabControl2.Tabs.Count - 1 Do Begin
    If s = form1.TabControl2.Tabs[i] Then Begin
      showmessage('Error, you need to close "' + s + '" first.');
      exit;
    End;
  End;
  // 2. Entfernen aus der OpenedFiles liste
  i := FindIndexbyname(filename);
  For j := i To high(OpenedFiles) - 1 Do Begin
    OpenedFiles[j] := OpenedFiles[j + 1];
  End;
  setlength(OpenedFiles, high(OpenedFiles));
  // Entfernen aus dem Treeview
  If (lowercase(ExtractFileExt(filename)) = '.c') Then Begin
    P := Form1.TreeView1.Items.FindNodeWithText('Source');
  End;
  If (lowercase(ExtractFileExt(filename)) = '.h') Then Begin
    P := Form1.TreeView1.Items.FindNodeWithText('Headers');
  End;
  child := p.FindNode(ExtractFileNameOnly(filename));
  child.Delete;
End;

Procedure CloseSourceFile;
Var
  j, i: integer;
Begin
  If AktualEditor = 0 Then Begin
    If Form1.tabControl1.tabIndex = -1 Then exit;
    OpenedFiles[OldLeftIndex].Text := form1.SynEdit1.Text;
    i := Form1.tabControl1.tabIndex;
    Form1.tabControl1.Tabs.delete(Form1.tabControl1.tabIndex);
    If Form1.tabControl1.Tabs.Count = 0 Then Begin
      form1.SynEdit1.Visible := false;
      OldLeftIndex := -1;
    End
    Else Begin
      // Neu Einer Alten Datei im Selben Editor
      j := FindIndexbyname(Form1.tabControl1.Tabs[max(i - 1, 0)]); // Kommt hier -1 dann haben wir ein Problem
      // In den Anderen Editor eine neue Datei Laden
      OpenSourceFile(OpenedFiles[j].Filename, 0);
      form1.tabControl1.PageIndex := j;
    End;
  End
  Else Begin
    If Form1.TabControl2.TabIndex = -1 Then exit;
    OpenedFiles[OldRightIndex].Text := form1.SynEdit2.Text;
    i := Form1.TabControl2.TabIndex;
    Form1.TabControl2.Tabs.delete(Form1.TabControl2.TabIndex);
    If Form1.TabControl2.Tabs.Count = 0 Then Begin
      form1.SynEdit2.Visible := false;
      OldRightIndex := -1;
      form1.TabControl2.Visible := false;
      form1.Splitter3.Visible := false;
    End
    Else Begin
      // Neu Einer Alten Datei im Selben Editor
      j := FindIndexbyname(Form1.TabControl2.Tabs[max(i - 1, 0)]); // Kommt hier -1 dann haben wir ein Problem
      // In den Anderen Editor eine neue Datei Laden
      OpenSourceFile(OpenedFiles[j].Filename, 1);
      form1.TabControl2.TabIndex := j;
    End;
  End;
  form1.FixLCLWidgetProperties;
  If Not GlobalChanged Then
    form1.caption := form1.caption + '*';
  GlobalChanged := true;
End;

Procedure CheckForSearchPath(Filename: String);
Var
  fp: String;
  i: integer;
Begin
  fp := IncludeTrailingPathDelimiter(ExtractFilePath(filename));
  If (AktualProjectWorkPath = fp) Then exit;
  // Schaun ob wir den Suchpfad schon haben
  For i := 0 To KlabConfig.searchpaths.Count - 1 Do Begin
    If (fp = IncludeTrailingPathDelimiter(KlabConfig.searchpaths[i])) Then exit;
  End;
  // Fragen ob der Suchpfad rein soll..
  If id_YES = application.MessageBox(pchar('The File : ' + ExtractFileName(filename) + ' is not within your searchpaths.'#13#10'Do you want to add its path : ' + fp + #13#10'to the searchpaths ?'), 'Question', MB_YESNO) Then Begin
    KlabConfig.searchpaths.Add(fp);
    KlabConfig.usesearchpath := true;
    If Not GlobalChanged Then
      form1.caption := form1.caption + '*';
    GlobalChanged := true;
  End;
End;

Procedure OpenSourceFile(Filename: String; Index: integer);
Var
  p: TTreeNode;
  s: String;
  osy, sy: tsynedit; // Das Zielsynedit
  tc: TTabControl; // Die Tabcontrol
  otc: TTabControl; // Die andere Tabcontrol
  ni: Integer; // die Neue Position der Datei
  j, i: Integer;
  sl: TStringList; // Zum Nachladen komplett neuen Sources
Begin
  SaveOldData;
  // Bestimmen des Speicherortes der Neuen Datei
  ni := FindIndexbyname(filename);
  If ni = -1 Then Begin
    // Eintragen in die Treeview
    If (lowercase(ExtractFileExt(filename)) = '.c') Then Begin
      P := Form1.TreeView1.Items.FindNodeWithText('Source');
    End;
    If (lowercase(ExtractFileExt(filename)) = '.h') Then Begin
      P := Form1.TreeView1.Items.FindNodeWithText('Headers');
    End;
    Form1.TreeView1.Items.AddChild(p, ExtractFileNameOnly(filename));
    // Laden von HDD
    setlength(OpenedFiles, high(OpenedFiles) + 2);
    ni := high(OpenedFiles);
    // Evtl muss der Pfad der Datei als suchpfad hinzugefügt werden.
    CheckForSearchPath(filename);
    If FileExistsUTF8(filename) Then Begin
      // Komplett Neu Laden einer Datei
      sl := TStringList.Create;
      sl.LoadFromFile(utf8tosys(filename));
      OpenedFiles[ni].Text := sl.Text;
      sl.free;
      OpenedFiles[ni].Changed := false;
    End
    Else Begin
      OpenedFiles[ni].Text := '';
      OpenedFiles[ni].Changed := True;
    End;
    OpenedFiles[ni].Filename := filename;
    //Todo : Auslesen der Bookmarks, und anderer Einstellungen
    OpenedFiles[ni].TopLine := 0;
  End;
  If index = -1 Then exit; // -1 = Nur eintragen in Treeview, dann raus
  If Index = 0 Then Begin
    If (OldLeftIndex = ni) Then exit; // Wir versuchen, die Aktuelle Datei noch mal zu laden.
    sy := Form1.SynEdit1;
    osy := Form1.SynEdit2;
    tc := Form1.TabControl1;
    otc := Form1.TabControl2;
    OldLeftIndex := ni;
  End;
  If Index = 1 Then Begin
    If (OldRightIndex = ni) Then exit; // Wir versuchen, die Aktuelle Datei noch mal zu laden.
    sy := Form1.SynEdit2;
    osy := Form1.SynEdit1;
    tc := Form1.TabControl2;
    otc := Form1.TabControl1;
    OldRightIndex := ni;
    // falls Rechts noch nicht Sichtbar ist, dann wirds das jetzt
    form1.Splitter3.Visible := true;
    form1.TabControl2.Visible := true;
  End;
  // Setzen der Neuen Einstellungen
  sy.Text := OpenedFiles[ni].Text;
  sy.TopLine := OpenedFiles[ni].TopLine;
  // Todo : Bookmarks und anderer Einstellungen, des Editor Feldes Speichern
  sy.Visible := true;
  // Evtl Entfernen des Dateinamen in der Alten Tabcontrol
  s := ExtractFileName(Filename);
  For i := 0 To otc.Tabs.Count - 1 Do Begin
    If (otc.Tabs[i] = s) Or (otc.Tabs[i] = s + '*') Then Begin
      otc.Tabs.Delete(i);
      // Im Alten muss dafür dann eine Neue Datei geladen werden.
      If otc.Tabs.count = 0 Then Begin
        osy.Visible := false;
        If otc.Name = form1.TabControl2.Name Then Begin
          OldRightIndex := -1;
          form1.TabControl2.Visible := false;
          form1.Splitter3.Visible := false;
        End;
      End
      Else Begin
        // Neu Einer Alten Datei im Selben Editor
        j := FindIndexbyname(otc.Tabs[max(i - 1, 0)]); // Kommt hier -1 dann haben wir ein Problem
        // In den Anderen Editor eine neue Datei Laden
        OpenSourceFile(OpenedFiles[j].Filename, 1 - index);
      End;
      break;
    End;
  End;
  // Evtl eintragen der Neuen Datei in die Neue Tabcontrol
  j := -1;
  For i := 0 To tc.Tabs.Count - 1 Do Begin
    If (tc.Tabs[i] = s) Or (tc.Tabs[i] = s + '*') Then Begin
      j := i;
      break;
    End;
  End;
  If j = -1 Then Begin
    If OpenedFiles[ni].Changed Then
      tc.Tabs.Add(s + '*')
    Else
      tc.Tabs.Add(s);
    j := tc.Tabs.Count - 1;
  End;
  If tc.TabIndex <> j Then
    tc.TabIndex := max(0, min(j, tc.Tabs.Count - 1));
  form1.FixLCLWidgetProperties;
  If Not GlobalChanged Then
    form1.caption := form1.caption + '*';
  GlobalChanged := true;
End;

Procedure NewProject(Filename: String);
Var
  p: TTreeNode;
Begin
  form1.Caption := defcaption + ' Opened : ' + ExtractFileNameOnly(filename) + '*';
  GlobalChanged := true;
  form1.ListBox1.Clear; // Löschen, aller Bisherigen Ausgaben
  GlobalChanged := true;
  OldLeftIndex := -1;
  OldRightIndex := -1;
  LoadEditorProperties(); // Laden der Default einstellungen
  // Rücksetzen der Ansichten
  AktualProjectFilename := Filename;
  AktualProjectWorkPath := IncludeTrailingPathDelimiter(ExtractFilePath(filename));
  form1.TabControl2.visible := false;
  form1.Splitter4.visible := false;
  form1.SynEdit1.Visible := false;
  // Anlegen der Default Treeview
  form1.TreeView1.Items.Clear;
  p := form1.TreeView1.Items.Add(Nil, ExtractFileNameonly(filename));
  form1.TreeView1.Items.AddChild(p, 'Headers');
  form1.TreeView1.Items.AddChild(p, 'Source');
  setlength(OpenedFiles, 0);
  form1.TabControl1.Tabs.clear;
  form1.TabControl2.Tabs.clear;
End;

Procedure SaveProject;
Begin
  SaveProjectAs(AktualProjectFilename);
End;

Procedure LoadProject(Filename: String);
Var
  f: TIniFile;
  i, j, k: integer;
  fname: String;
  tl, place: integer;
Begin
  If FileExistsUTF8(filename) Then Begin
    // Anpassen der LCL
    NewProject(filename); // Filemonitor.clear
    // Laden der Project Datei.
    f := TIniFile.Create(UTF8ToSys(filename));
    SetCurrentDir(ExtractFilePath(filename));
    // Editor Einstellungen
    LoadEditorSettings(f);
    // LCL Einstellungen
    Form1.MenuItem16.Checked := odd(f.ReadInteger('General', 'ShowMessageBox', 1));
    Form1.MenuItem17.Checked := odd(f.ReadInteger('General', 'ShowProjectManager', 1));
    Form1.MenuItem18.Checked := odd(f.ReadInteger('General', 'ShowTerminal', 1));
    form1.GroupBox1.Width := f.ReadInteger('General', 'ProjectManagerWidth', 100);
    form1.GroupBox2.Width := f.ReadInteger('General', 'TerminalWidth', 150);
    form1.ListBox1.Height := f.ReadInteger('General', 'MessageBoxHeight', 100);
    form1.TabControl2.Width := f.ReadInteger('General', 'Editor2Width', 150);
    // Dateiverwaltung
    i := f.ReadInteger('General', 'FileCount', -1);
    For j := 0 To i - 1 Do Begin
      fname := f.ReadString('File' + inttostr(j + 1), 'Filename', '');
      //      If Not FileExistsUTF8(fname) Then Begin
      fname := ExpandFileName(fname);
      //      End;
      place := f.Readinteger('File' + inttostr(j + 1), 'Place', -1);
      tl := f.Readinteger('File' + inttostr(j + 1), 'Topline', 0);
      If FileExistsUTF8(fname) Then Begin
        // Laden des Files
        OpenSourceFile(fname, place);
        // Übernehmen der TopLine
        // Todo : Bug Beheben, die Top Line wird nicht übernommen, warum ist noch unklar..
        If place = 0 Then Form1.SynEdit1.TopLine := tl;
        If place = 1 Then Form1.SynEdit2.TopLine := tl;
        k := FindIndexbyname(fname);
        If k <> -1 Then Begin
          OpenedFiles[k].TopLine := tl;
        End;
      End
      Else Begin
        showmessage('Error unable to load : ' + fname);
      End;
    End;
    Form1.MenuItem16Click(Nil);
    Form1.MenuItem17Click(Nil);
    Form1.MenuItem18Click(Nil);
    f.free;
    // Entfernen des "hat sich was geändert" flags in der Caption ( Kommt durch New Project )
    If form1.caption[length(form1.caption)] = '*' Then
      form1.caption := copy(form1.caption, 1, length(form1.caption) - 1);
    GlobalChanged := false;
  End
  Else Begin
    showmessage('Error unable to load : ' + filename);
  End;
End;

Procedure SaveProjectAs(Filename: String);
Var
  sl: TStringlist;
  f: TIniFile;
  j, i: Integer;
Begin
  //FileMonitorAktive := false; // Deaktivieren der FileMonitore
  SaveOldData; // Auslesen der Synedits und Speichern in die Lokale Variablen
  AktualProjectFilename := Filename;
  AktualProjectWorkPath := IncludeTrailingPathDelimiter(ExtractFilePath(filename));
  SaveIDELastOpened(filename);
  // Entfernen der "Noch zu Speichern Flags"
  For i := 0 To Form1.TabControl1.Tabs.Count - 1 Do Begin
    If form1.TabControl1.Tabs[i][length(form1.TabControl1.Tabs[i])] = '*' Then Begin
      form1.TabControl1.Tabs[i] := copy(form1.TabControl1.Tabs[i], 1, length(form1.TabControl1.Tabs[i]) - 1);
    End;
  End;
  For i := 0 To Form1.TabControl2.Tabs.Count - 1 Do Begin
    If form1.TabControl2.Tabs[i][length(form1.TabControl2.Tabs[i])] = '*' Then Begin
      form1.TabControl2.Tabs[i] := copy(form1.TabControl2.Tabs[i], 1, length(form1.TabControl2.Tabs[i]) - 1);
    End;
  End;
  For i := 0 To high(OpenedFiles) Do Begin
    OpenedFiles[i].Changed := false;
  End;
  // Abspeichern des Projektes
  f := TIniFile.Create(utf8tosys(Filename));
  sl := TStringList.Create;
  // Die Editor Einstellungen
  f.WriteInteger('General', 'ShowMessageBox', ord(Form1.MenuItem16.Checked));
  f.WriteInteger('General', 'ShowProjectManager', ord(Form1.MenuItem17.Checked));
  f.WriteInteger('General', 'ShowTerminal', ord(Form1.MenuItem18.Checked));
  f.WriteInteger('General', 'ProjectManagerWidth', form1.GroupBox1.Width);
  f.WriteInteger('General', 'TerminalWidth', form1.GroupBox2.Width);
  f.WriteInteger('General', 'MessageBoxHeight', form1.ListBox1.Height);
  f.WriteInteger('General', 'Editor2Width', form1.TabControl2.Width);
  f.WriteInteger('General', 'AktiveLeft', form1.TabControl1.TabIndex);
  f.WriteInteger('General', 'AktiveRight', form1.TabControl2.TabIndex);
  //  Speichern der Links, und Rechts "offenen" Dateien
  // Dazu müssen erst die "Überschüssigen Dateien "gelöscht" werden.
  i := f.ReadInteger('General', 'FileCount', high(OpenedFiles) + 1);
  For j := high(OpenedFiles) + 2 To i Do Begin
    f.EraseSection('File' + inttostr(j));
  End;
  f.WriteInteger('General', 'FileCount', high(OpenedFiles) + 1);
  SaveEditorSettings(f);
  For i := 0 To high(OpenedFiles) Do Begin
    // Abspeichern des Dateinamens
    // f.WriteString('File' + inttostr(i + 1), 'Filename', OpenedFiles[i].Filename);
    f.WriteString('File' + inttostr(i + 1), 'Filename', ExtractRelativepath(Filename, OpenedFiles[i].Filename)); // -- Support Relative Paths
    f.Writeinteger('File' + inttostr(i + 1), 'Place', -1); // Default nicht sichtbar
    f.Writeinteger('File' + inttostr(i + 1), 'Topline', OpenedFiles[i].TopLine);
    // Abspeichern der Datei
    sl.Text := OpenedFiles[i].Text;
    sl.SaveToFile(utf8tosys(OpenedFiles[i].Filename));
  End;
  // Aktivieren des Monitors
  // Abspeichern der Place Zugehörigkeit
  For i := 0 To form1.TabControl1.Tabs.count - 1 Do Begin
    j := FindIndexbyname(form1.TabControl1.Tabs[i]);
    f.Writeinteger('File' + inttostr(j + 1), 'Place', 0);
  End;
  For i := 0 To form1.TabControl2.Tabs.count - 1 Do Begin
    j := FindIndexbyname(form1.TabControl2.Tabs[i]);
    f.Writeinteger('File' + inttostr(j + 1), 'Place', 1);
  End;
  f.free;
  sl.free;
  If form1.caption[length(form1.caption)] = '*' Then
    form1.caption := copy(form1.caption, 1, length(form1.caption) - 1);
  GlobalChanged := false;
  form1.SynEdit1.MarkTextAsSaved;
  form1.SynEdit2.MarkTextAsSaved;
  // ein Bischen warten, das der CN_Monitor hinter Her kommt die ganzen Falsch Meldungen raus zu schieben
  For i := 0 To 99 Do Begin
    Application.ProcessMessages;
    Sleep(1);
  End;
  //  FileMonitorAktive := true; // Aktivieren der FileMonitore
End;

End.

