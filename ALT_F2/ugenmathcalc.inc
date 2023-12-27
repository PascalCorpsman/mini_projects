(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of ALT_F2                                                *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)

// Die Unit Läuft mit und ohne Tokenizer

{$DEFINE UseTokenizer}

// Die Unit Läuft mit "Immer" einseitigen Unären Symbolen, und mit "Links, Rechts" seitigen Symbolen
{.$DEFINE Beidseitig}

{.$DEFINE ALLOW_RENDERTREE} // Wenn True, dann kann der PCalctree auf ein TCanvas visualisiert werden.


