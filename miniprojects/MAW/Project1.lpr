(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of MAW                                                   *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Program Project1;

{$MODE ObjFPC}{$H+}

Uses
  Forms, lazopenglcontext, Interfaces,
  Unit1 In 'Unit1.pas', dglOpenGL {Form1};

Begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
End.

