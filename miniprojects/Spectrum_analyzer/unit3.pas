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
Unit Unit3;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

Type

  { TForm3 }

  TForm3 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Procedure FormCreate(Sender: TObject);
  private
    Function getDurationInMs: integer;
    Function getSampleRate: Integer;

  public
    Property SampleRate: Integer read getSampleRate;
    Property DurationInMs: integer read getDurationInMs;

  End;

Var
  Form3: TForm3;

Implementation

{$R *.lfm}

{ TForm3 }

Procedure TForm3.FormCreate(Sender: TObject);
Begin
  caption := 'Create empty board';
  edit1.text := '1000';
  edit2.text := '44100';

End;

Function TForm3.getDurationInMs: integer;
Begin
  result := strtointdef(edit1.text, 1000);
End;

Function TForm3.getSampleRate: Integer;
Begin
  result := strtointdef(edit2.text, 44100);
End;

End.

