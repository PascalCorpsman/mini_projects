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
  (*
   * History: 1 = Initial version
   *          2 = Abort File Transfer
   *)
  ProtokollVersion = 2;

  (*
   * Nachrichten von Clients an den Server
   *)
  MSG_New_Participant = 1;

  (*
   * Nachrichten vom Server an die Clients
   *)
  MSG_New_Participant_Result = 100;
  MSG_Known_Participant_List = 101;

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

Function ErrorcodeToString(aValue: uint16): String;

Function GetFileSize(Const Filename: String): int64;
Function FileSizeToString(Value: Int64): String;

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
  End;
End;

Function FileSizeToString(Value: Int64): String;
Var
  s: char;
  r: Int64;
Begin
  s := ' ';
  r := 0;
  If value > 1024 Then Begin
    s := 'K';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If value > 1024 Then Begin
    s := 'M';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If value > 1024 Then Begin
    s := 'G';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If value > 1024 Then Begin
    s := 'T';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If value > 1024 Then Begin
    s := 'P';
    r := value Mod 1024;
    value := value Div 1024;
  End;
  If (r Div 100) <> 0 Then
    result := inttostr(value) + ',' + inttostr(r Div 100) + s + 'B'
  Else
    result := inttostr(value) + s + 'B'
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

