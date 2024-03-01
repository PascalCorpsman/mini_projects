(******************************************************************************)
(* Lan chat                                                        03.12.2023 *)
(*                                                                            *)
(* Version     : 0.06                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Simple server based chat Program for local networks          *)
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
(* Wishlist    : - Filetransfer                                               *)
(*                                                                            *)
(* Known Issues: none                                                         *)
(*                                                                            *)
(* History     : 0.01 - Initialversion                                        *)
(*               0.02 - Support Passwords                                     *)
(*               0.03 - File transfer support                                 *)
(*               0.04 - Settings for Server                                   *)
(*               0.05 - Add UDP for autodetecting                             *)
(*               0.06 - Automatic restart on Exception                        *)
(*                                                                            *)
(******************************************************************************)
Program f_server;

Uses
{$IFDEF LINUX}
  BaseUnix,
{$ENDIF}
  SysUtils, userver;

Var
  server: TLANChatServer;

{$IFDEF LINUX}

Procedure DoKill(Signal: CInt); cdecl;
Begin
  If Signal = SIGTERM Then Begin
    writeln('Going down through sigkill..');
    server.Terminate;
  End;
End;

Procedure InstallSigHandler;
Var
  Action: PSigActionRec;
Begin
  New(Action);
  Action^.sa_handler := SigActionHandler(@DoKill);
  FillChar(Action^.sa_mask, SizeOf(Action^.sa_mask), #0);
  Action^.sa_flags := 0;
  Action^.sa_restorer := Nil;
  If FPSigaction(SIGTERM, Action, Nil) <> 0 Then Begin
    WriteLn('Error: ', fpgeterrno);
    Halt(1);
  End;
  Dispose(Action);
End;
{$ENDIF}

Var
  NeedRunning: Boolean;
  info: TServerinfo;
Begin
{$IFDEF LINUX}
  InstallSigHandler;
{$ENDIF}
  writeln('LAN chat server ver. 0.06');
  LanChatVersion := 006;
  info.StartTimeStamp := GetTickCount64;
  info.Restarts := 0;
  info.LastRestartReason := '';
  server := TLANChatServer.Create();
  NeedRunning := true;
  While NeedRunning Do Begin
    Try
      server.Setinfo(Info);
      server.Execute();
      // Termination by User -> Leave the while loop
      If Not server.Running Then Begin
        NeedRunning := false;
      End;
    Except
      On av: Exception Do Begin
        inc(info.Restarts);
        info.LastRestartReason := av.Message;
        writeln('Got an exception:');
        writeln('');
        writeln('  ' + av.Message);
        writeln('');
        writeln('going down.');
        Try
          server.free;
        Except
        End;
        sleep(1000);
        (*
         * Restart the server
         *)
        server := TLANChatServer.Create();
      End;
    End;
  End;
  server.Free;
End.

