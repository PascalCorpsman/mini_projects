(******************************************************************************)
(* uuart_deprecated                                                31.03.2012 *)
(*                                                                            *)
(* Version     : 0.08                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : This Unit implements a Control Class for the Synaser         *)
(*               TBlockSerial, which allows you to easily access to the Uart  *)
(*               device.                                                      *)
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
(*               0.02 - Added Compilerswitches                                *)
(*                      Added RecvByteArr, Connected                          *)
(*               0.03 - Added SizeRecvBuffer, RecvByte                        *)
(*               0.04 - Added uuart.inc                                       *)
(*               0.05 - Added Flush                                           *)
(*               0.06 - Added GetSerialPortNames for Linux                    *)
(*               0.07 - Added Stoppbit forwarding from synaser                *)
(*               0.08 - Added Purge                                           *)
(*                                                                            *)
(******************************************************************************)
Unit uuart_deprecated;

{$MODE objfpc}{$H+}

(*
 * Soll die uuart.pas als Library genutzt werden, dann sollte die uuart.inc
 * aus dem Verzeichnis der uuart.pas gelöscht werden. Für jedes Projekt, welches
 * uuart.pas einbindet. Muss dann im Projektordner eine seperate uuart.inc
 * angelegt werden.
 *
 * Folgende Defines können/sollten definiert werden:
 *
 * UseLCL : Wenn die uuart Klasse unter der LCL läuft (ausgabe von Fehlern via Dialog)
 * UseConsole : Wenn die uuart Klasse fehler auf die Konsole loggen soll.
 *
 *)
{$I uuart.inc}

Interface

Uses
  Classes, SysUtils, synaser
{$IFDEF UseLCL}
  , dialogs //debug
{$ENDIF}
  ;

Const

  SB1 = synaser.SB1;
  SB1andHalf = synaser.SB1andHalf;
  SB2 = synaser.SB2;

Type

  { TUart }
  TUart = Class
  private
    freceivebuffer: integer;
    fserial: TBlockSerial; // Zur Kommunikation ( Uart Schnittstelle )
    Function getconnected: Boolean;
    Function GetLastError: integer;
    Function GetReceivebuffer: integer;
    Procedure setReceifebuffer(value: integer);
  public
    Property Connected: Boolean read getconnected; // True wenn verbunden
    Property SizeRecvBuffer: integer read GetReceivebuffer write setReceifebuffer;
    Property LastError: integer read GetLastError;

    Constructor create; virtual; Deprecated 'Do not use this class anymore!';
    Destructor destroy; override;
    Function connect(Port: String; Baudrate, Bits: Integer; Parity: Char; Stop: Integer; softflow, hardflow: boolean): Boolean;
    Procedure disconnect(); // Schliest eine Verbindung
    Procedure Close; // Schliest eine Verbindung

    Function Read2(CharCount: integer; Timeout: integer = 10): String; // Liest wenn Möglich von der Uart, aber nur CharCount Zeichen
    Function Read(Timeout: Integer = 10): String; // Liest wenn möglich von der Uart
    Function RecvTerminated(Timeout: Integer; Const Terminator: String): String; // Empfängt bis daten mit Terminator angekommen sind oder ein Timeout Stattfand.
    Procedure RecvByteArr(Out Data: TBytes; Timeout: Integer = 10); // Empfängt beliebig viele Bytes bis Timeout Lang nichts mehr kommt
    Function RecvByte(Out Error: Boolean; Timeout: Integer = 10): Byte; // Empfängt ein Byte, Error = True => Keine Daten innerhalb von Timeout empfangen

    Procedure Write(Const Value: String); // Schreibt einen String
    Procedure WriteByteArr(Const Data: TBytes); // Sendet einen Byte Datanstrom
    (*
     * Do not use this in a while loop !
     * The internal buffers are updated via "?" (simply calling the Application.ProcessMessages / Application.HandleMessage / sleep does not do the trick)
     *
     * Only Known working method is polling via Timer or handling in OnIdle
     *)
    Function WaitingData(): integer;

    Procedure Flush(); // Empties the Read Buffer
    Procedure Purge(); // Unconditional Empty of all buffers
  End;

Function GetSerialPortNames(): String; Deprecated 'use version from uuart.pas';

Implementation

Function GetSerialPortNames(): String;
{$IFDEF WINDOWS}
Begin
  result := synaser.GetSerialPortNames();
{$ELSE}
Var
  sl: TStringlist;
Var
  Info: TSearchRec;
  hdl: THandle;
  b: Boolean;
Begin
  sl := TStringlist.create;
  If FindFirst('/dev/tty*', faSysFile, Info) = 0 Then Begin
    Repeat
      b := true;
      Try
        hdl := FileOpen('/dev/' + info.Name, fmOpenReadWrite);
        If hdl = -1 Then Begin
          b := false;
        End;
      Except
        b := false;
      End;
      If hdl >= 0 Then Begin
        FileClose(hdl);
      End;
      If b Then Begin
        sl.Add('/dev/' + info.Name);
      End;
    Until FindNext(info) <> 0;
  End;
  FindClose(Info);
  result := sl.CommaText;
  sl.free;
{$ENDIF}
End;


{ TUart }

Function ErrorCodeToString(ErrorCode: integer): String;
Begin
  // Die Bedeutung der Errorcodes ist hier :
  // http://synapse.ararat.cz/doc/help/synaser.html
  // bzw. hier :
  // http://msdn.microsoft.com/en-us/library/windows/desktop/ms681382%28v=vs.85%29.aspx
  (*
   * Generell bei Fehlern unter Linux, prüfen ob der User in der "tty" bzw. "dialout" gruppe ist !
   *)
  Case ErrorCode Of
    // Es sind beileibe nicht alle aufgeführt, nur die die bisher mal auftraten ...
    0: result := 'ERROR_SUCCESS';
    2: result := '2 = ERROR_FILE_NOT_FOUND';
    5: result := '5 = ERROR_ACCESS_DENIED';
    13: result := '13 = The data is invalid.';
    87: result := '87 = ERROR_INVALID_PARAMETER';
    9991: Begin
        result := '9991 = ERROR_ALREADY_OWNED';
{$IFDEF LINUX}
        result := result + LineEnding;
        result := result + 'See in "' + LockfileDirectory + '" for lockfile informations.';
{$ENDIF}
      End
  Else
    result := inttostr(ErrorCode);
  End;
End;

Function TUart.getconnected(): Boolean;
Begin
  result := Assigned(fserial);
  // TODO: Hier könnte man noch mehr machen ..
End;

Function TUart.GetLastError: integer;
Begin
  If assigned(fserial) Then Begin
    result := fserial.LastError;
  End
  Else Begin
    result := 1;
  End;
End;

Function TUart.GetReceivebuffer: integer;
Begin
  result := freceivebuffer;
End;

Procedure TUart.setReceifebuffer(value: integer);
Begin
{$IFDEF LINUX}
{$IFDEF UseLCL}
  showmessage('SizeRecvBuffer only valid under Windows');
{$ENDIF}
{$IFDEF UseConsole}
  writeln('SizeRecvBuffer only valid under Windows');
{$ENDIF}
{$ELSE}
  If assigned(fserial) Then Begin
    Raise Exception.Create('Error set receivebuffer while connected.');
  End
  Else Begin
    freceivebuffer := value;
  End;
{$ENDIF}
End;

Constructor TUart.create;
Begin
  Inherited create;
  fserial := Nil;
  freceivebuffer := 4096; // Default value from Synaser
End;

Destructor TUart.destroy;
Begin
  close;
End;

Function TUart.connect(Port: String; Baudrate, Bits: Integer; Parity: Char;
  Stop: Integer; softflow, hardflow: boolean): Boolean;
Begin
  result := false;
  If assigned(fserial) Then fserial.free;
  fserial := TBlockSerial.Create;
{$IFDEF Windows}
  fserial.SizeRecvBuffer := freceivebuffer;
{$ENDIF}
  fserial.Connect(Port);
  Sleep(100); // Das Device kann zum Teil recht langsam sein, mit dem Sleep sorgen wir dafür, dass auch sicher connected ist.
  If (fserial.LastError <> 0) Then Begin
    // Raise Exception.Create('Error, could not connect to device error number : ' + ErrorCodeToString(fserial.LastError));
{$IFDEF UseLCL}
    showmessage('Error, could not connect to device error number : ' + ErrorCodeToString(fserial.LastError));
{$ENDIF}
{$IFDEF UseConsole}
    writeln('Error, could not connect to device error number : ' + ErrorCodeToString(fserial.LastError));
{$ENDIF}
    close();
    exit;
  End;
  //  fserial.Config(Baudrate, 8, 'N', SB1, false, false);
  fserial.Config(Baudrate, Bits, Parity, Stop, softflow, hardflow);
  Sleep(100); // Das Device kann zum Teil recht langsam sein, mit dem Sleep sorgen wir dafür, dass auch sicher connected ist.
  If (fserial.LastError <> 0) Then Begin
    // Raise Exception.Create(format('Error, could not config device erronumber : %d', [fserial.LastError]));
{$IFDEF UseLCL}
    showmessage('Error, could not config device error number : ' + ErrorCodeToString(fserial.LastError));
{$ENDIF}
{$IFDEF UseConsole}
    writeln('Error, could not config device error number : ' + ErrorCodeToString(fserial.LastError));
{$ENDIF}
    close();
    Exit;
  End;
  result := true;
End;

Procedure TUart.disconnect();
Begin
  close();
End;

Function TUart.Read2(CharCount: integer; Timeout: integer): String;
Var
  b: Boolean;
  c: Char;
Begin
  result := '';
  If Not assigned(fserial) Then exit;
  b := true;
  While b Do Begin
    c := chr(fserial.RecvByte(Timeout));
    If fserial.LastError = 0 Then Begin
      result := result + c;
      CharCount := CharCount - 1;
      If Charcount <= 0 Then
        exit;
    End
    Else Begin
      b := false;
    End;
  End;
End;

Function TUart.Read(Timeout: Integer): String;
Var
  b: Boolean;
  c: Char;
Begin
  result := '';
  If Not assigned(fserial) Then exit;
  b := true;
  While b Do Begin
    c := chr(fserial.RecvByte(Timeout));
    If fserial.LastError = 0 Then Begin
      result := result + c;
    End
    Else Begin
      b := false;
    End;
  End;
End;

Procedure TUart.WriteByteArr(Const Data: TBytes);
Var
  i: integer;
Begin
  If Not assigned(fserial) Then exit;
  For i := 0 To high(data) Do
    fserial.SendByte(data[i]);
End;

Function TUart.WaitingData(): integer;
Begin
  result := 0;
  If assigned(fserial) Then result := fserial.WaitingData;
End;

Procedure TUart.Flush();
Var
  i: integer;
Begin
  // Alles was Da ist Lesen und Wegschmeisen
  If assigned(fserial) Then Begin
    For i := 0 To fserial.WaitingData - 1 Do
      fserial.RecvByte(0);
  End;
End;

Procedure TUart.Purge();
Begin
  If assigned(fserial) Then Begin
    fserial.Purge;
  End;
End;

Function TUart.RecvTerminated(Timeout: Integer; Const Terminator: String
  ): String;
Begin
  If assigned(fserial) Then Begin
    result := fserial.RecvTerminated(Timeout, Terminator);
  End
  Else
    result := '';
End;

Procedure TUart.RecvByteArr(Out Data: TBytes; Timeout: Integer);
Var
  b: Boolean;
  c: Byte;
  cnt: integer;
Begin
  If Not assigned(fserial) Then Begin
    setlength(data, 0);
    exit;
  End;
  cnt := 0;
  setlength(data, 100);
  b := true;
  While b Do Begin
    c := fserial.RecvByte(Timeout);
    If fserial.LastError = 0 Then Begin
      data[cnt] := c;
      inc(cnt);
      If cnt > high(data) Then Begin
        setlength(data, high(data) + 101);
      End;
    End
    Else Begin
      b := false;
    End;
  End;
  setlength(data, cnt);
End;

Function TUart.RecvByte(Out Error: Boolean; Timeout: Integer): Byte;
Begin
  If assigned(fserial) Then Begin
    result := fserial.RecvByte(Timeout);
    error := fserial.LastError <> 0;
  End
  Else Begin
    error := true;
    result := 0;
  End;
End;

Procedure TUart.Write(Const Value: String);
Var
  data: TBytes;
  i: Integer;
Begin
  setlength(data, length(value));
  For i := 0 To high(data) Do Begin
    data[i] := ord(value[i + 1]);
  End;
  WriteByteArr(data);
  setlength(data, 0);
End;

Procedure TUart.Close;
Begin
  If assigned(fserial) Then Begin
    fserial.free;
  End;
  fserial := Nil;
End;

End.

