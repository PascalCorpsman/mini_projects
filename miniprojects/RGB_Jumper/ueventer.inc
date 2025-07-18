(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of RGB_Jumper                                            *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)

   // Hier wird der Uses Teil voll Fertig gemacht (damit alle notwendigen Units für TOwnerClass zur Verfügung gestellt werden können)
//, ExtCtrls;
, OpenGLContext;

// Die Klasse welche die zu Kapernden Methoden bereit stellt

Type
  //  TOwnerClass = TPaintBox;
  TOwnerClass = TOpenGLControl;

{$DEFINE KeyEvents} // Nicht jede TOwnerClass hat KeyEvents, deswegen sind die Optional => TODO: Manuelles einpflegen der Key Events

