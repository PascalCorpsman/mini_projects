(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Lan chat                                              *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit ulanchatcommon;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils;

Const
  UDPPingPort = 54321; // Need to be bigger than 1000 on Linux !
  UDPRandomChiffre = 152; // Random number, fairly choosen by google search ;)
  (*
   * History: 1 = Initial version
   *          2 = Abort File Transfer
   *          3 = Server Administration..
   *          4 = Server Info
   *)
  ProtokollVersion = 4;

  (*
   * Nachrichten von Clients an den Server
   *)
  MSG_New_Participant = 1;
  MSG_Change_Password = 2;
  MSG_Login_to_server_settings = 3;
  MSG_Remove_Known_Participant = 4;
  MSG_Request_Server_Info = 5;

  (*
   * Nachrichten vom Server an die Clients
   *)
  MSG_New_Participant_Result = 100;
  MSG_Known_Participant_List = 101;
  MSG_Change_Password_Result = 102;
  MSG_Login_to_server_settings_Result = 103;
  MSG_Remove_Known_Participant_Result = 104;
  MSG_Request_Server_Info_Result = 105;


  (*
   * Nachrichten die sowohl Client als auch Server Versenden
   *)
  Msg_Message = 200;
  MSG_File_Transfer_Request = 201; // Client1 -> Server -> Client2
  MSG_File_Transfer_Request_result = 202; // Client2 -> Server -> Client1
  MSG_File_Transfer_FileContent = 203;
  MSG_File_Transfer_FileComplete = 204;
  MSG_File_Transfer_File_Next_Packet = 205;
  MSG_File_Transfer_File_Abort = 206;

  (*
   * Fehlercodes
   *)
  Error_No_Error = $0001;
  File_Transfer_Accepted = $0002;

  Error_Invalid_Protocol_Version = $F001;
  Error_User_Already_logged_in = $F002;
  Error_Invalid_Password = $F003;

  File_Transfer_No_due_to_Receiver_not_Online = $F004;
  File_Transfer_No_due_to_Receiver_occupied = $F005;
  File_Transfer_No_Receiver_does_not_want = $F006;
  File_Transfer_Abort_by_Receiver = $F007;
  File_Transfer_Abort_by_Sender = $F008;
  File_Transfer_Abort_Missing_Endpoint = $F009;

  Error_Not_Allowed_to_Delet_Online_User = $F00A;

Function ErrorcodeToString(aValue: uint16): String;

Function GetFileSize(Const Filename: String): int64;
Function FileSizeToString(Value: Int64): String;
Function PrettyTime(Time_in_ms: UInt64): String;

Implementation

Function ErrorcodeToString(aValue: uint16): String;
Begin
  result := format('%0.4X', [aValue]);
  Case aValue Of
    Error_No_Error: result := 'OK';
    File_Transfer_Accepted: result := 'OK';

    Error_Invalid_Protocol_Version: result := 'Invalid protocol version';
    Error_User_Already_logged_in: result := 'user with same name already logged in';
    Error_Invalid_Password: result := 'invalid login password';

    File_Transfer_No_due_to_Receiver_not_Online: result := 'Receiver is not online';
    File_Transfer_No_due_to_Receiver_occupied: result := 'Receiver already receives a file';
    File_Transfer_No_Receiver_does_not_want: result := 'Receiver reject file transfer';
    File_Transfer_Abort_by_Receiver: result := 'Receiver abort file transfer';
    File_Transfer_Abort_by_Sender: result := 'Sender abort file transfer';
    File_Transfer_Abort_Missing_Endpoint: result := 'Endpoint disconnected';

    Error_Not_Allowed_to_Delet_Online_User: result := 'It''s not allowed to a user that is online';
  End;
End;

Function FileSizeToString(Value: Int64): String;
Var
  s: char;
  r: Int64;
Begin
  s := ' ';
  r := 0;
  If value >= 1000 Then Begin
    s := 'K';
    r := value Mod 1000;
    value := value Div 1000;
  End;
  If value >= 1000 Then Begin
    s := 'M';
    r := value Mod 1000;
    value := value Div 1000;
  End;
  If value >= 1000 Then Begin
    s := 'G';
    r := value Mod 1000;
    value := value Div 1000;
  End;
  If value >= 1000 Then Begin
    s := 'T';
    r := value Mod 1000;
    value := value Div 1000;
  End;
  If value >= 1000 Then Begin
    s := 'P';
    r := value Mod 1000;
    value := value Div 1000;
  End;
  If (r Div 100) <> 0 Then Begin
    value := value * 10 + round(r / 100);
    result := format('%0.1f%sB', [value / 10, s]);
  End
  Else
    result := inttostr(value) + s + 'B'
End; 

Function PrettyTime(Time_in_ms: UInt64): String;
Var
  hs, digits, sts, sep, s: String;
  st, i: integer;
  b: Boolean;
Begin
  s := 'ms';
  hs := '';
  sep := DefaultFormatSettings.DecimalSeparator;
  st := 0;
  b := false;
  digits := '3';
  // [0 .. 60[ s
  If Time_in_ms >= 1000 Then Begin
    st := Time_in_ms Mod 1000;
    Time_in_ms := Time_in_ms Div 1000;
    s := 's';
    b := true;
  End;
  // [1 .. 60[ min
  If (Time_in_ms >= 60) And b Then Begin
    st := Time_in_ms Mod 60;
    Time_in_ms := Time_in_ms Div 60;
    s := 'min';
    sep := DefaultFormatSettings.TimeSeparator;
    digits := '2';
  End
  Else
    b := false;
  // [1 .. 24[ h
  If (Time_in_ms >= 60) And b Then Begin
    st := Time_in_ms Mod 60;
    Time_in_ms := Time_in_ms Div 60;
    s := 'h';
  End
  Else
    b := false;
  // [1 ..  d
  If (Time_in_ms >= 24) And b Then Begin
    st := Time_in_ms Mod 24;
    Time_in_ms := Time_in_ms Div 24;
    hs := 'd';
    If st <> 0 Then s := 'h';
    sep := ' ';
    digits := '1';
  End
  Else
    b := false;
  // Ausgabe mit oder ohne Nachkomma
  If st <> 0 Then Begin
    sts := format('%0.' + digits + 'd', [st]);
    If (s = 's') Then Begin // Bei Sekunden die endenden 0-en löschen
      For i := length(sts) Downto 1 Do Begin
        If sts[i] = '0' Then Begin
          delete(sts, i, 1);
        End
        Else Begin
          break;
        End;
      End;
    End;
    result := inttostr(Time_in_ms) + hs + sep + sts + s;
  End
  Else Begin
    result := inttostr(Time_in_ms) + s;
  End;
End;

(*
 * https://www.freepascal.org/docs-html/rtl/system/filesize.html
 *)

Function GetFileSize(Const Filename: String): int64;
Var
  f: File Of Byte;
Begin
  If FileExists(Filename) Then Begin
    assignfile(f, Filename);
    Try
      reset(f);
      result := FileSize(f);
    Finally
      close(f);
    End;
  End
  Else Begin
    result := 0;
  End;
End;

End.

