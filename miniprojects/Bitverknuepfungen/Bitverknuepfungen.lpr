(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Bitverknuepfungen                                     *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Program Bitverknuepfungen;

{$MODE ObjFPC}{$H+}

Uses
  Forms, Interfaces,
  Unit1 In 'Unit1.pas' {Form1},
  Unit2 In 'Unit2.pas' {Form2},
  Unit3 In 'Unit3.pas' {Form3},
  Unit4 In 'Unit4.pas' {Form4};

Begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TForm4, Form4);
  Application.Run;
End.

