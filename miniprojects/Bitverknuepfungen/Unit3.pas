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
Unit Unit3;

{$MODE ObjFPC}{$H+}

Interface

Uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls,
  Forms, Dialogs, ComCtrls;

Type

  { TForm3 }

  TForm3 = Class(TForm)
    ProgressBar1: TProgressBar;
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

Var
  Form3: TForm3;

Implementation

{$R *.lfm}

End.

