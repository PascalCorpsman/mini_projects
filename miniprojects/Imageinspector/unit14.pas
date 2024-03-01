(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of ImageInspector                                        *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit14;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons;

Type

  { TForm14 }

  TForm14 = Class(TForm)
    BitBtn1: TBitBtn;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Procedure BitBtn1Click(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private

  public

  End;

Var
  Form14: TForm14;

Implementation

{$R *.lfm}

Uses lclintf, unit1;

{ TForm14 }

Procedure TForm14.FormCreate(Sender: TObject);
Begin
  caption := 'Options';
  label1.caption := format('This is a %d-Bit Application', [sizeof(Pointer) * 8]);
End;

Procedure TForm14.Button1Click(Sender: TObject);
Begin
  openurl(GetAppConfigDir(false));
End;

Procedure TForm14.BitBtn1Click(Sender: TObject);
Begin
  showmessage(
    unit1.Defcaption + LineEnding + LineEnding +
    'Autor: Uwe Schächterle' + LineEnding +
    'Contributor: Julian Bauknecht' + LineEnding +
    'Homepage: www.Corpsman.de' + LineEnding +
    'License: ' + LineEnding +
    '   This program is postcardware, see Homepage' + LineEnding +
    '   for futher details' + LineEnding +
    'Warranty: There is no warranty!' + LineEnding +
    'Description: ' + LineEnding +
    '   This tool is intended to measure distances' + LineEnding +
    '   and areas on images');
End;

End.

