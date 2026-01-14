(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Spectrum_analyzer                                     *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Program project1;

{$MODE objfpc}{$H+}

Uses
{$IFDEF UNIX}
  cthreads,
{$ENDIF}
{$IFDEF HASAMIGA}
  athreads,
{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Unit1, uspectrum_analyzer, uwave, Unit2
  { you can add units after this };

{$R *.res}

Begin
{$IFDEF Windows}
{$IF declared(UseHeapTrace)}
  GlobalSkipIfNoLeaks := True; // supported as of debugger version 3.2.0
{$ENDIF}
{$ENDIF}
  RequireDerivedFormResource := True;
  Application.Scaled := True;
{$PUSH}{$WARN 5044 OFF}
  Application.MainFormOnTaskbar := True;
{$POP}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
End.

