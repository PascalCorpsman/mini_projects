(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Episode manager                                       *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Program EpisodenManager;

{$MODE ObjFPC}{$H+}

Uses
  Forms,
  Interfaces,
  Unit1 In 'Unit1.pas' {Form1},
  Unit2 In 'Unit2.pas' {Form2};

Begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
End.

