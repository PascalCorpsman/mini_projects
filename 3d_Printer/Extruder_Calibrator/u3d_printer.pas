(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of 3D-Printer gui for Raspberry Pi                       *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit u3d_printer;

{$MODE objfpc}{$H+}

(*

Doku als Graph: http://plantuml.com/

@startuml

[*] --> notconnected

state "not connected" as notconnected
state "connected send welcome" as connectedsendwelcome
state "send welcome wait for OK" as sendwelcomewaitforok
state "operating" as operating
state "execute external command" as executeexternalcommand
state "wait for OK" as waitforok
state "send gcode file" as sendgcodefile

notconnected --> connectedsendwelcome: connect(port)
connectedsendwelcome --> sendwelcomewaitforok: M110 N0
sendwelcomewaitforok --> connectedsendwelcome:on timeout
sendwelcomewaitforok --> operating: on OK
operating --> executeexternalcommand: execute command
operating --> sendgcodefile: send gcode file
executeexternalcommand --> waitforok: send command
sendgcodefile--> waitforok: send command
waitforok -->operating: on OK
waitforok -->notconnected: on timeout

@enduml
 *)

Interface

Uses
  Classes, SysUtils, synaser, pitools, upilogger, uunbufstringlist;

Const
  LineBufferCount = 1024; // Größe des Speichers der zuletzt gesendeten Befehle
  WaitForOKTimeout = 1500; // Zeit in ms bis das Warten auf ein OK abgrbrochen wird.
  WaitForG_Timeout = 60000; // Die G* codes dauern auch mal etwas länger
  TempRefreshInterval = 5000; // Egal wie viele Temperaturanfragen, erst wenn dieses Intervall abgelaufen ist wird eine erneute anfrage gesendet.
  SendIdentifier = 'S:'; // Wird dem Tracer als Präfix fürs Senden mit gegeben
  ReceiveIdentifier = 'R:'; // Wird dem Tracer als Präfix fürs Empfangen mit gegeben

Type
  TTempRecord = Record // Datenstruktur für eine Temperatur mit Soll und Ist
    isvalue: Double;
    shouldvalue: Double;
  End;

  TPrinterState = (
    psNotConnected, // Nicht verbunden
    psConnectedSendWelcome, // Verbunden, wartet darauf das der Liniennummer Rücksetz  G-Code mit "OK" bestätigt wird => Erst dann kanns losgehen.
    psSendWelcomeWaitingForOK, // Gleich wie psWaitingForCommandOK, nur eben während der initialisierungsroutine
    psOperating, // Wir sind verbunden, der Drucker ist initialisiert und wir warten darauf entweder einen Externen Befehl oder ein gCode File zu senden.
    psExecuteExternalCommand, // Initiiert die Ausführung eines externen GCodes
    psSendGCodeFile, // Initiiert oder führt das senden einer GCode Datei
    psWaitingForOK // Jeder Befehl der gesendet wird, wird vom Drucker quitiert mittels eines "OK"
    );

  TTracer = Procedure(Sender: TObject; Command: String) Of Object; // Callback Typ für den Tracer

  TGCodeFile = Record
    GCodeFile: TUnbufferedStringList; // das Tatsächlich geladene File
    LayerCount: Int64; // Anzahl der Layer der gesamten Datei
    ActualLayer: Int64; // Aktuelles Layer, das gerade gedruckt wird
    Filename: String; // Dateiname der G-Code Datei
    SendActive: Boolean; // True, wenn gerade eine Datei gesendet ist
    PrintStartedTime: int64; // Zeitpunkt an dem der erste G-Code ausgeführt wurde (also Druckzeit ohne Heizen)
    GCodeCount: int64; // Anzahl gültiger G-Codes in der Datei
    GCodeIndex: int64; // Der index des gerade Gelesenen GCodes
  End;

  { T3D_Printer }

  T3D_Printer = Class(Tthread)
  private
    fPort: String; // Die Serielle Schnittstelle auf die Verbunden wurde
    fSerial: TBlockSerial; // Die Serielle Schnittstelle auf die Verbunden wurde
    fReceiveBuffer: String; // Empfangspuffer, der alles Empfängt, was über die Uart rein kommt (dieser wird später anhand #10 zerlegt und in HandleReceivedLine verarbeitet

    fEchoLines: String; // Wenn der Drucker uns beim Starten was sendet oder auch während der Ausführung eines Befehls, dann wird das hier gesammelt, das "OK" überträgt es dann.
    fformatsettings: TFormatSettings; // Immer English

    fTemperatures: Array Of TTempRecord; // 2 Elementiges Array, Index = 0 = Nozzle, Index = 1 = Bett
    fLastTempRefresh: int64; // Zeitpunkt an dem das Letzte mal RefreshTemperatures aufgerufen wurde

    fStateBeforeSend, fState: TPrinterState; // Der DFA und der Zustand vor dem Aktuellen Zustand
    fState_psConnectSendWelcome_Counter: integer; // Schmiermerker für den Status psConnectedSendWelcome

    fLineBuffer: Array[0..LineBufferCount - 1] Of String; // Alle gesendeten Befehle werden gepuffert, falls ein Resend kommt
    fLineNumber: int64; // Aktueller Index in fLineBuffer

    fTracerSynchronText: String; // Der Text der an den Tracer Synchronisiert hochgereicht werden soll

    fThread_DisconnectFlag: Boolean; // Wenn True, dann muss der Thread den Zugriff auf fSerial abbrechen

    fExecGCodeCommandFlag: Boolean; // Wenn True, dann führt der Thread bei nächster Gelegenheit einen Externen Befehl aus
    fExecGCodeCommand: String; // Der Befehl, welcher als Externer Befehl ausgeführt werden soll
    fExecGCodeCommandResult: String; // Das Ergebnis, welches der Thread nachher synchronisiert hoch reicht.

    fWaitForOKTimestamp: int64; // Zeitpunkt an dem der Letzte G-Code abgesendet wurde, ab hier wird das Timeout berechnet

    fGCodeFile: TGCodeFile; // Alles was mit dem zu Druckenden G-Code File zu tun hat.
    fAbortPrintFlag: Boolean; // Wenn True, wird die Verbindung neu aufgebaut = abbruch aller Tätigkeiten

    fLogMessage: String; // Für den Logger (nicht Tracer)
    fLoglevel: TLoglevel; // Für den Logger (nicht Tracer)

    Procedure HandleLog(Message: String; Loglevel: TLoglevel); // Für den Logger (nicht Tracer)
    Procedure LogSync; // Für den Logger (nicht Tracer)

    (*
     * Routinen die in TThread.execute aufgerufen werden
     *)
    Procedure HandleReceivedLine(value: String);
    Procedure ResetConnection;
    Procedure SendCommand(Value: String); // Die Letzte Routine, welche einen String weg sendet
    Procedure SetWaitForOK;
    Procedure HandleOK(Error_Occured: Boolean; Line: String);

    Function NextGCode: String;
    Procedure RefreshTemperatures;
    Procedure Trace(Value: String); // Schnittstelle zum Aufrufen des Tracers (aus dem Thread heraus)

    Procedure SendToTracer;

    (*
     * Aktualisiert die Anzahl der Layer, gcodes ...
     *)
    Procedure UpdateGCodeInfo();

    Procedure PrintFinished; // Wird aufgerufen, wenn ein Druckauftrag erfolgreich beendet wurde.
  public
    OnPrintFinished: TNotifyEvent;
    Tracer: TTracer;
    Constructor Create();
    Destructor Destroy; override;

    Procedure Execute; override;
    Function Connect(Port: String): Boolean;
    Function IsConnected: Boolean;
    Procedure DisConnect;

    Function ExecGCode(GCode: String): String;

    Procedure SendGCodeFile(Filename: String);
    Procedure AbortPrinting; // !! ACHTUNG !!, das baut die Verbindung komplett neu auf, bricht einfach alles ab.
    Function IsSendingGCodeFile: Boolean;

    Function ActualLayer: int64;
    Function LayerCount: int64;
    Function SendingProgress: Double;
    Function ReadyInSeconds: int64;
    Function TimeFor10000GCodes: Double; // Die auf 10000 Gcode Zeilen gemittelte Zeit in s

    (*
     * Temperaturinformationen, !! Achtung !! werden nur alle TempRefreshInterval ms aktualisiert!
     * oder Vollautomatisch bei M190 bzw M109
     *)
    Function NozzleIsTemperature: double;
    Function NozzleShouldTemperature: double;
    Function BedIsTemperature: double;
    Function BedShouldTemperature: double;

  End;

Implementation

Uses math;

{ T3D_Printer }

Procedure T3D_Printer.HandleLog(Message: String; Loglevel: TLoglevel);
Begin
  fLoglevel := Loglevel;
  fLogMessage := Message;
  Synchronize(@LogSync);
End;

Procedure T3D_Printer.LogSync;
Begin
  log(fLogMessage, fLoglevel);
End;

Procedure T3D_Printer.HandleReceivedLine(value: String);
Var
  line: int64;
  s, t: String;
  handled: Boolean;
Begin
  handled := false;
  value := trim(value);
  Trace(ReceiveIdentifier + value);
  // Todo : Die Prüfsumme auch auswerten (kommt die überhaupt ?
  If pos('*', value) <> 0 Then Begin // evtl Abschneiden der Prüfsumme
    value := copy(value, 1, pos('*', value) - 1);
  End;
  If pos('ok', lowercase(value)) = 1 Then Begin // OK Kam, weiter gehts
    handled := true;
    If fEchoLines <> '' Then fEchoLines := fEchoLines + LineEnding;
    HandleOK(false, fEchoLines + trim(value));
    fEchoLines := ''; // Das macht Send Command eigentlich auch..
  End;
  // Der zuletzt gesendete G-Code
  s := fLineBuffer[fLineNumber Mod LineBufferCount];
  s := copy(s, pos(' ', s) + 1, length(s));
  // M109: Set Extruder Temperature and Wait
  // M190: Wait for bed temperature to reach target temp
  If (pos('M190', s) = 1) Then Begin // Wenn der Zuletzt gesendete Befehl, das Setzen der Nozzle oder Bett Temperatur mit Warten ist, dann muss die Timeout Funktion deaktiviert werden.
    // Wenn wir eine "Gültige" Antwort erhalten haben, dann reseten wir die Timeouts und übernehmen die Informationen
    If pos('T:', value) = 1 Then Begin
      handled := true;
      fWaitForOKTimestamp := GetTickCount64;
      fLastTempRefresh := GetTickCount64;
      setlength(fTemperatures, 2); // Eigentlich müsste das nicht mehr gemacht werden müssen...
      t := trim(copy(s, pos('S', s) + 1, length(s))) + '*'; // Auslesen der gesetzten Solltemperatur
      t := copy(t, 1, pos('*', t) - 1);
      fTemperatures[1].shouldvalue := strtofloatdef(t, 0, fformatsettings);
      t := value + ' '; // = T:19.37 E:0 B:32.9
      s := copy(t, 1, pos(' ', t) - 1);
      delete(t, 1, pos(' ', t));
      s := copy(s, pos(':', s) + 1, length(s));
      fTemperatures[0].isvalue := strtofloatdef(s, 0, fformatsettings);
      delete(t, 1, pos(' ', t)); // Abschneiden "E:0 "
      s := copy(t, 1, pos(' ', t) - 1);
      s := copy(s, pos(':', s) + 1, length(s));
      fTemperatures[1].isvalue := strtofloatdef(s, 0, fformatsettings);
    End;
  End
  Else Begin
    If (pos('M109', s) = 1) Then Begin
      If pos('T:', value) = 1 Then Begin
        handled := true;
        fWaitForOKTimestamp := GetTickCount64;
        fLastTempRefresh := GetTickCount64;
        setlength(fTemperatures, 2); // Eigentlich müsste das nicht mehr gemacht werden müssen...
        t := trim(copy(s, pos('S', s) + 1, length(s))) + '*'; // Auslesen der gesetzten Solltemperatur
        t := copy(t, 1, pos('*', t) - 1);
        fTemperatures[0].shouldvalue := strtofloatdef(t, 0, fformatsettings);
        t := value + ' '; // = T:67.3 E:0 W:?
        s := copy(t, 1, pos(' ', t) - 1);
        delete(t, 1, pos(' ', t));
        s := copy(s, pos(':', s) + 1, length(s));
        fTemperatures[0].isvalue := strtofloatdef(s, 0, fformatsettings);
        // das Bett wird nicht übertragen, also ändern wir einfach mal nichts ..
      End;
    End;
  End;
  // Wenn da irgendwas nicht stimmt, und der Drucker eine Zeile neu haben will
  If pos('resend:', lowercase(value)) = 1 Then Begin
    handled := true;
    // Todo: das muss noch Brauchbar geloggt werden
    s := copy(value, pos(':', value) + 1, length(value));
    line := StrToInt64def(s, -1);
    If line = -1 Then Begin
      Trace('T3D_Printer.HandleReceivedLine, resend failed: ' + value);
    End;
    // Todo : Log Info Resend
    Trace(SendIdentifier + fLineBuffer[line Mod LineBufferCount]);
    fSerial.SendString(fLineBuffer[line Mod LineBufferCount]);
  End;
  If pos('error', lowercase(value)) = 1 Then Begin
    handled := true;
    // Log Error, keine Ahnung was wir damit machen sollen ...
    HandleLog(value, llError);
  End;
  If (pos('ECHO', uppercase(value)) = 1) Then Begin
    handled := true;
    If fEchoLines <> '' Then Begin
      fEchoLines := fEchoLines + LineEnding;
    End;
    fEchoLines := fEchoLines + value;
  End;
  If (Not handled) Then Begin
    (*
     * Im Hochfahren / Initialisieren wird alles als Echo geschluckt.
     *)
    If (fState = psSendWelcomeWaitingForOK) Then Begin
      If fEchoLines <> '' Then Begin
        fEchoLines := fEchoLines + LineEnding;
      End;
      fEchoLines := fEchoLines + value;
    End
    Else Begin
      HandleLog('Unhandled receive: "' + value, llCritical);
    End;
  End;
End;

Procedure T3D_Printer.ResetConnection;
Begin
  fState := psNotConnected;
  If assigned(fSerial) Then Begin
    HandleLog('Lost uart connection, uart error code : ' + inttostr(fSerial.LastError), llcritical);
    fSerial.free;
  End
  Else Begin
    // Das hier darf eigentlich gar nicht vorkommen !
    HandleLog('Called reset Connection with fSerial=nil', llFatal);
  End;
  fSerial := Nil;
  fExecGCodeCommandResult := '';
  fExecGCodeCommandFlag := false;
  fGCodeFile.SendActive := false;
  If assigned(fGCodeFile.GCodeFile) Then fGCodeFile.GCodeFile.free;
  fGCodeFile.GCodeFile := Nil;
End;

Procedure T3D_Printer.SendCommand(Value: String);
  Function CalcCRC(Message: String): String;
  Var
    cs: uint8;
    b: uint8;
    i: Integer;
  Begin
    // Entnommen aus: http://reprap.org/wiki/G-code , dort unter "Checksum"
    result := '';
    cs := 0;
    For i := 1 To length(Message) Do Begin
      b := ord(Message[i]);
      cs := cs Xor b;
    End;
    result := inttostr(cs);
  End;
Begin
  fEchoLines := '';
  inc(fLineNumber);
  value := 'N' + IntToStr(fLineNumber) + ' ' + Value;
  value := value + '*' + CalcCRC(value) + #10;
  fLineBuffer[fLineNumber Mod LineBufferCount] := value; // Puffern, für evtl neu senden
  Trace(SendIdentifier + value);
  If assigned(fSerial) Then Begin
    If fSerial.LastError <> 0 Then Begin
      ResetConnection;
      exit;
    End;
    fSerial.SendString(Value);
  End;
  SetWaitForOK;
End;

Procedure T3D_Printer.SetWaitForOK;
Begin
  fStateBeforeSend := fState;
  If fState = psConnectedSendWelcome Then Begin
    fState := psSendWelcomeWaitingForOK;
  End
  Else Begin
    fState := psWaitingForOK;
  End;
  fWaitForOKTimestamp := GetTickCount64;
End;

Procedure T3D_Printer.HandleOK(Error_Occured: Boolean; Line: String);
Begin
  If Error_Occured Then Begin
    If fStateBeforeSend = psConnectedSendWelcome Then Begin
      // Das sorgt dafür, das wir theoretisch endlos neu Connected
      fState := psConnectedSendWelcome;
      // Exit; evtl das logging unten unterdrücken
    End
    Else Begin
      fState := psNotConnected; // Das ist evtl ein bischen Rabiat ..
      // Falls ein ExecGCode zum Fehler geführt hat.
      If fExecGCodeCommandFlag Then Begin
        fExecGCodeCommandResult := trim(line);
        fExecGCodeCommandFlag := false;
      End;
      // Im Falle dass wir gerade was senden, brechen wir das Komplett ab.
      fGCodeFile.SendActive := false;
      If assigned(fGCodeFile.GCodeFile) Then fGCodeFile.GCodeFile.free;
      fGCodeFile.GCodeFile := Nil;
      HandleLog('T3D_Printer.HandleOK, error occured:' + Line, llError);
    End;
  End
  Else Begin
    // Fehlerfrei ein OK bekommen, dann gehts wieder im Operationsmodus weiter
    fState := psOperating;
    Case fStateBeforeSend Of
      psConnectedSendWelcome: Begin
          // Wir machen das einfach mal 2 mal, ka warum ..
          inc(fState_psConnectSendWelcome_Counter);
          If fState_psConnectSendWelcome_Counter < 2 Then Begin
            fState := psConnectedSendWelcome;
          End;
        End;
      psExecuteExternalCommand: Begin
          fExecGCodeCommandResult := trim(line);
          fExecGCodeCommandFlag := false;
        End;
    End;
  End;
End;

Function T3D_Printer.NextGCode: String;
Var
  s: String;
Begin
  result := '';
  If Not Assigned(fGCodeFile.GCodeFile) Then exit;
  While Not fGCodeFile.GCodeFile.eof Do Begin
    s := fGCodeFile.GCodeFile.Readln();
    If pos(';', trim(s)) <> 1 Then Begin
      result := s;
      If pos(';', result) <> 0 Then Begin
        result := trim(copy(result, 1, pos(';', result) - 1));
      End;
      If trim(result) <> '' Then Begin
        inc(fGCodeFile.GCodeIndex);
        exit;
      End;
    End;
  End;
End;

Procedure T3D_Printer.RefreshTemperatures;
Var
  t, s: String;
Begin
  Try
    If fLastTempRefresh + TempRefreshInterval < GetTickCount64 Then Begin
      fLastTempRefresh := GetTickCount64;
      s := ExecGCode('M105');
      setlength(fTemperatures, 0);
      If (trim(s) = '') Or (pos('OK', uppercase(s)) = 0) Then exit;
      //            Nozzle ist soll, dann Bed ist soll
      //    "ok T:18.8 /0.0 B:26.8 /31.0 T0:18.8 /0.0 @:0 B@:127"

      // Todo: das muss noch besser werden, ist doch recht "Hart" Codiert
      s := s + ' ';
      setlength(fTemperatures, 2);
      t := copy(s, 1, pos(' ', s) - 1); // Das "ok " abschneiden
      delete(s, 1, pos(' ', s));
      t := copy(s, 1, pos(' ', s) - 1); //
      delete(s, 1, pos(' ', s));
      t := copy(t, pos(':', t) + 1, length(t));
      fTemperatures[0].isvalue := StrToFloatdef(t, 0, fformatsettings);
      t := copy(s, 1, pos(' ', s) - 1);
      delete(s, 1, pos(' ', s));
      t := copy(t, pos('/', t) + 1, length(t));
      fTemperatures[0].shouldvalue := StrToFloatdef(t, 0, fformatsettings);
      t := copy(s, 1, pos(' ', s) - 1);
      delete(s, 1, pos(' ', s));
      t := copy(t, pos(':', t) + 1, length(t));
      fTemperatures[1].isvalue := StrToFloatdef(t, 0, fformatsettings);
      t := copy(s, 1, pos(' ', s) - 1);
      delete(s, 1, pos(' ', s));
      t := copy(t, pos('/', t) + 1, length(t));
      fTemperatures[1].shouldvalue := StrToFloatdef(t, 0, fformatsettings);
    End;
  Except
    On av: exception Do Begin
      HandleLog('T3D_Printer.RefreshTemperatures [' + av.Message + '] = ' + s, llFatal);
    End;
  End;
End;

Procedure T3D_Printer.Trace(Value: String);
Begin
  If assigned(tracer) Then Begin
    fTracerSynchronText := value;
    Synchronize(@SendToTracer);
  End;
End;

Procedure T3D_Printer.SendToTracer;
Begin
  If assigned(tracer) Then Begin
    Tracer(self, fTracerSynchronText);
  End;
End;

Procedure T3D_Printer.UpdateGCodeInfo();
Var
  s: String;
  LCount: int64;
Begin
  LCount := 0;
  fGCodeFile.GCodeIndex := 0;
  fGCodeFile.GCodeFile.reset;
  s := trim(uppercase(NextGCode()));
  While s <> '' Do Begin
    If (pos('G', s) = 1) And (pos('Z', s) <> 0) Then Begin
      inc(LCount);
    End;
    s := uppercase(NextGCode());
  End;
  fGCodeFile.LayerCount := LCount;
  fGCodeFile.GCodeCount := fGCodeFile.GCodeIndex;
End;

Procedure T3D_Printer.PrintFinished;
Begin
  If assigned(OnPrintFinished) Then OnPrintFinished(self);
End;

Constructor T3D_Printer.Create();
Begin
  Inherited create(false);
  OnPrintFinished := Nil;
  fformatsettings := DefaultFormatSettings;
  fformatsettings.DecimalSeparator := '.';
  fTemperatures := Nil;
  fLastTempRefresh := GetTickCount64 - TempRefreshInterval;
  FreeOnTerminate := false;
  fThread_DisconnectFlag := false;
  fState := psNotConnected;
  fSerial := Nil;
  fReceiveBuffer := '';
  fGCodeFile.Filename := '';
  fGCodeFile.SendActive := false;
  fAbortPrintFlag := false;
End;

Destructor T3D_Printer.Destroy;
Begin
  Tracer := Nil; // Wir fahren den Drucker runter, da darf der tracer nicht mehr an sein, sonst gibts ne AV.
  Terminate; // Thread Beenden lassen
  DisConnect; // Verbindung beenden
  sleep(10); // das Dauert ja auch ein bischen..
  CheckSynchronize(10);
  If assigned(fSerial) Then fSerial.free;
  If assigned(fGCodeFile.GCodeFile) Then fGCodeFile.GCodeFile.free;
  fGCodeFile.GCodeFile := Nil;
  fSerial := Nil;
  Inherited Destroy;
End;

Procedure T3D_Printer.Execute;
Var
  t: int64;
  i: integer;
  s: String;
Begin
  t := GetTickCount64;
  While Not Terminated Do Begin
    Try
      If fThread_DisconnectFlag Then Begin // Abbruch von Außen
        fState := psNotConnected;
        fThread_DisconnectFlag := false;
      End;
      If fAbortPrintFlag Then Begin
        If assigned(fSerial) Then fSerial.Free;
        fSerial := Nil;
        fState := psNotConnected;
        fExecGCodeCommandFlag := false;
        fGCodeFile.SendActive := false;
        Connect(fPort);
        fAbortPrintFlag := false;
      End;
      // Steht die Verbindung noch ?
      If fState <> psNotConnected Then Begin
        If assigned(fSerial) And (fSerial.LastError <> 0) Then Begin // Abbruch der Verbindung
          ResetConnection;
        End;
      End;
      Case fState Of
        psNotConnected: sleep(1); // So lange wir nicht verbunden sind, machen wir erst mal gar nichts
        psConnectedSendWelcome: Begin
            If t + 1000 < GetTickCount64 Then Begin
              t := GetTickCount64;
              fLineNumber := -1;
              SendCommand('M110 N0'); // Zurücksetzen der Liniennummerierung auf 0
            End
            Else Begin
              Sleep(1);
            End;
          End;
        psSendWelcomeWaitingForOK,
          psWaitingForOK: Begin
            // Das Timeout ist abgelaufen
            If fWaitForOKTimestamp + WaitForOKTimeout <= GetTickCount64 Then Begin
              // Es Kam Kein OK, also melden wir das Zurück
              // in S steht nun der zuletzt gesendete Befehl
              s := uppercase(trim(fLineBuffer[fLineNumber Mod LineBufferCount]));
              s := copy(s, pos(' ', s) + 1, length(s));
              If pos('*', s) <> 0 Then Begin
                s := copy(s, 1, pos('*', s) - 1);
              End;
              // Bei den G* Befehlen haben wir ein längeres Timeout
              // bzw wenn wir gerade am Senden einer Datei sind, dann dauerts auch
              // Länger ..
              If fGCodeFile.SendActive Or
                (pos('G28', s) = 1) Or
                (pos('G0', s) = 1) Or
                (pos('G1', s) = 1) Then Begin
                If fWaitForOKTimestamp + WaitForG_Timeout <= GetTickCount64 Then Begin
                  HandleOK(True, 'Timeout wait for OK for command: ' + s);
                End
                Else Begin
                  sleep(1); // Wir Warten auf ein OK, und haben im Prinzip auch nichts zu tun, also CPU-Zeit Reduzieren.
                End;
              End
              Else Begin
                HandleOK(True, 'Timeout wait for OK for command: ' + s);
              End;
            End
            Else Begin
              sleep(1); // Wir Warten auf ein OK, und haben im Prinzip auch nichts zu tun, also CPU-Zeit Reduzieren.
            End;
          End;
        psOperating: Begin
            (*
             * Hier landen wir immer dann, wenn wir uns eine neue Aufgabe suchen
             *)
            If fExecGCodeCommandFlag Then Begin // Ein Externer CGCode Soll ausgeführt werden.
              fState := psExecuteExternalCommand;
              Continue;
            End;
            If fGCodeFile.SendActive Then Begin
              fstate := psSendGCodeFile;
              continue;
            End;
            // Wir haben im Operation mode nichts zu tun, räumen evtl noch auf und warten ansonsten
            If assigned(fGCodeFile.GCodeFile) Then fGCodeFile.GCodeFile.free;
            fGCodeFile.GCodeFile := Nil;
            sleep(1); // Wir Idlen im Operation mode..
          End;
        psSendGCodeFile: Begin
            If Assigned(fGCodeFile.GCodeFile) Then Begin
              // Wir Senden die Nächste GCode Zeile
              s := NextGCode;
              // Der erste G-Code startet die Zeitmessung
              If (fGCodeFile.PrintStartedTime = 0) And (pos('G', uppercase(s)) <> 0) Then Begin
                fGCodeFile.PrintStartedTime := GetTickCount64;
              End;
              // Ein G-Code der die Z-Layer anpasst zum Mitzählen des Aktuellen Layers
              If (pos('G', s) = 1) And (pos('Z', s) <> 0) Then Begin
                inc(fGCodeFile.ActualLayer);
              End;
              If s = '' Then Begin
                // Wir sind fertig.
                If assigned(OnPrintFinished) Then Begin
                  Synchronize(@PrintFinished); // Das zuerst, dann ist
                End;
                fGCodeFile.SendActive := false;
                fGCodeFile.PrintStartedTime := 0;
                fGCodeFile.GCodeFile.free;
                fGCodeFile.GCodeFile := Nil;
                fState := psOperating;
              End
              Else Begin
                SendCommand(s)
              End;
            End
            Else Begin
              // Wir Laden das ganze Ding erst mal
              If FileExists(fGCodeFile.Filename) Then Begin
                fGCodeFile.GCodeFile := TUnbufferedStringList.Create;
                // Todo : hier könnten evtl diverse Ladefehler abgefangen werden, und dann wieder zurück nach psOperating
                fGCodeFile.GCodeFile.LoadFromFile(fGCodeFile.Filename);
                fGCodeFile.GCodeIndex := 0;
                // Initialisiert
                // fGCodeFile.LayerCount
                // fGCodeFile.GCodeCount
                UpdateGCodeInfo();
                fGCodeFile.ActualLayer := 0;
                fGCodeFile.GCodeIndex := 0;
                fGCodeFile.GCodeFile.Reset();
              End
              Else Begin
                HandleLog('Could not open: ' + fGCodeFile.Filename, llCritical);
                fGCodeFile.SendActive := false;
                fState := psOperating;
              End;
            End;
          End;
        psExecuteExternalCommand: Begin
            SendCommand(fExecGCodeCommand);
          End;
      End;
      // Empfangen was geht
      If fState <> psNotConnected Then Begin
        i := fSerial.WaitingData;
        If i <> 0 Then Begin
          fReceiveBuffer := fReceiveBuffer + fSerial.RecvBufferStr(i, 1);
          While pos(#10, fReceiveBuffer) <> 0 Do Begin
            s := copy(fReceiveBuffer, 1, pos(#10, fReceiveBuffer) - 1);
            delete(fReceiveBuffer, 1, pos(#10, fReceiveBuffer));
            HandleReceivedLine(s);
          End;
        End;
      End;
    Except
      On av: exception Do Begin
        ResetConnection;
        HandleLog('Access Violation in thread [' + av.Message + ']', llFatal);
      End;
    End;
  End;
  fState := psNotConnected;
End;

Function T3D_Printer.Connect(Port: String): Boolean;
Var
  i: Integer;
Begin
  result := false;
  fPort := Port;
  // Einen evtl alten Lock Löschen
  If FileExists('/var/lock/LCK..' + ExtractFileName(Port)) Then Begin
    HandleLog('Deleting old /var/lock entry for :' + ExtractFileName(Port), llInfo);
    If Not DeleteFile('/var/lock/LCK..' + ExtractFileName(Port)) Then Begin
      HandleLog('Could not delete log from :' + port, llFatal);
    End;
  End;
  // Auf jeden Trennen
  Disconnect;
  // Auf jeden alle Aktuellen Anfragen Abbrechen
  fExecGCodeCommandFlag := false;
  fGCodeFile.SendActive := false;
  // Für die nächste Zeit temperaturanfragen sperren
  fLastTempRefresh := GetTickCount64;

  If assigned(fSerial) Then fSerial.Free;
  fSerial := TBlockSerial.Create;
  Try
    fSerial.Connect(Port);
    Sleep(100); // Warten bis Verbunden
    If (fserial.LastError <> 0) Then Begin
      fSerial.Free;
      fSerial := Nil;
      exit;
    End;
    fserial.Config(115200, 8, 'N', SB1, false, false);
    Sleep(100); // Das Device kann zum Teil recht langsam sein, mit dem Sleep sorgen wir dafür, dass auch sicher connected ist.
    If (fserial.LastError <> 0) Then Begin
      fSerial.Free;
      fSerial := Nil;
      exit;
    End;
    result := true;
    fState_psConnectSendWelcome_Counter := 0;
    For i := 0 To LineBufferCount - 1 Do Begin
      fLineBuffer[i] := '';
    End;
    fState := psConnectedSendWelcome;
  Except
    fSerial.Free;
    fSerial := Nil;
  End;
End;

Function T3D_Printer.IsConnected: Boolean;
Begin
  // So kann man while not IsConnected machen, und hat dann eine betriebsbereite Classe
  // Beim Vergleich auf fstate <> psNotConnected könnte die Initialisierung gestört werden
  result := (Not fAbortPrintFlag) And
    Assigned(fserial) And
    (Not (fstate In [psNotConnected, psConnectedSendWelcome, psSendWelcomeWaitingForOK]));
End;

Procedure T3D_Printer.DisConnect;
Begin
  // Hier muss man ein wenig mehr machen, da ja der Thread Freigeben muss
  fThread_DisconnectFlag := true;
  While fstate <> psNotConnected Do Begin
    Sleep(1);
    CheckSynchronize(1);
  End;
  If Assigned(fSerial) Then fSerial.free;
  fSerial := Nil;
End;

Function T3D_Printer.ExecGCode(GCode: String): String;
Begin
  result := '';
  If (Not IsConnected) Or fExecGCodeCommandFlag Then exit;
  fExecGCodeCommand := GCode;
  fExecGCodeCommandFlag := true;
  While fExecGCodeCommandFlag Do Begin
    CheckSynchronize(1);
  End;
  result := fExecGCodeCommandResult;
End;

Procedure T3D_Printer.SendGCodeFile(Filename: String);
Begin
  If Not IsConnected Then exit;
  fGCodeFile.Filename := Filename;
  fGCodeFile.PrintStartedTime := 0;
  fAbortPrintFlag := false;
  fGCodeFile.SendActive := true;
End;

Procedure T3D_Printer.AbortPrinting;
Begin
  fAbortPrintFlag := true;
End;

Function T3D_Printer.IsSendingGCodeFile: Boolean;
Begin
  result := Assigned(fGCodeFile.GCodeFile) And fGCodeFile.SendActive;
End;

Function T3D_Printer.ActualLayer: int64;
Begin
  result := fGCodeFile.ActualLayer;
End;

Function T3D_Printer.LayerCount: int64;
Begin
  result := fGCodeFile.LayerCount;
End;

Function T3D_Printer.SendingProgress: Double;
Begin
  If IsSendingGCodeFile And (Assigned(fGCodeFile.GCodeFile)) Then Begin
    result := fGCodeFile.GCodeIndex / max(fGCodeFile.GCodeCount, 1);
  End
  Else Begin
    result := 0;
  End;
End;

Function T3D_Printer.ReadyInSeconds: int64;
Var
  timetillnow: int64;
Begin
  // Das ist schon super so, aber evtl könnte man das noch glätten ??
  result := 0;
  If fGCodeFile.printstartedTime = 0 Then exit;
  If IsSendingGCodeFile And (Assigned(fGCodeFile.GCodeFile)) Then Begin
    timetillnow := (GetTickCount64 - fGCodeFile.printstartedTime) Div 1000; // Zeit bis jetzt seit Start des Drucks
    result := (timetillnow * fGCodeFile.GCodeCount) Div max(1, fGCodeFile.GCodeIndex);
    result := result - timetillnow;
  End;
End;

Function T3D_Printer.TimeFor10000GCodes: Double;
Var
  time: int64;
Begin
  // Die Teilung durch 1000 wird unten ja wieder neutralisiert, also sparen wir uns das hin und her gerechne ..
  time := (GetTickCount64 - fGCodeFile.PrintStartedTime) {Div 1000}; // Zeit in s
  result := (time * 10 {000}) / max(fGCodeFile.GCodeCount, 1);
End;

Function T3D_Printer.NozzleIsTemperature: double;
Begin
  RefreshTemperatures;
  result := 0;
  If high(fTemperatures) >= 0 Then Begin
    result := fTemperatures[0].isvalue;
  End;
End;

Function T3D_Printer.NozzleShouldTemperature: double;
Begin
  RefreshTemperatures;
  result := 0;
  If high(fTemperatures) >= 0 Then Begin
    result := fTemperatures[0].shouldvalue;
  End;
End;

Function T3D_Printer.BedIsTemperature: double;
Begin
  RefreshTemperatures;
  result := 0;
  If high(fTemperatures) >= 1 Then Begin
    result := fTemperatures[1].isvalue;
  End;
End;

Function T3D_Printer.BedShouldTemperature: double;
Begin
  RefreshTemperatures;
  result := 0;
  If high(fTemperatures) >= 1 Then Begin
    result := fTemperatures[1].shouldvalue;
  End;
End;

End.

