(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Extruder calibrator                                   *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit upilogger;

{$MODE objfpc}{$H+}

// This is a dummy file so that u3d_printer does not needs to be changed for this
// project

Interface

Uses
  Classes, SysUtils;

Type
  TLoglevel = (llfatal, llcritical, llerror, llwarning, lldebug, llinfo, lltrace);

Procedure Log(Text: String; Loglevel: TLoglevel);

Implementation

Procedure Log(Text: String; Loglevel: TLoglevel);
Begin

End;

End.

