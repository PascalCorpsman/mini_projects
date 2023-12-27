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
Unit Unit3;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

Type

  { TForm3 }

  TForm3 = Class(TForm)
    OK: TButton;
    Label1: TLabel;
    Procedure FormCreate(Sender: TObject);
    Procedure OKClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  End;

Var
  Form3: TForm3;

Implementation

{$R *.lfm}

Uses unit1;

{ TForm3 }

Procedure TForm3.OKClick(Sender: TObject);
Begin
  close;
End;

Procedure TForm3.FormCreate(Sender: TObject);
Begin
  Tform(self).Constraints.MaxHeight := Tform(self).Height;
  Tform(self).Constraints.MinHeight := Tform(self).Height;
  Tform(self).Constraints.Maxwidth := Tform(self).width;
  Tform(self).Constraints.Minwidth := Tform(self).width;
  color := clBlack;
  label1.font.Color := clLime;
  caption := 'Options';
{$IFDEF Windows}
  Label1.caption := 'This is KDE-Shortcutbar ver. ' + ALT_F2_Version + ' for Windows.' + LineEnding + LineEnding +
{$ELSE}
  Label1.caption := 'This is KDE-Shortcutbar ver. ' + ALT_F2_Version + ' for Linux.' + LineEnding + LineEnding +
{$ENDIF}
  'by Corpsman' + LineEnding + LineEnding +
    'support : www.Corpsman.de' + LineEnding +
    'license : https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md' + LineEnding +
    'waranty : There is no waranty !' + LineEnding + LineEnding +
    'usage : Type in the application you want to start and' + LineEnding +
    '             press return.' + LineEnding + LineEnding +
    '             if you start with "=" you can use a simple calculator.' + LineEnding +
    '                           (0x* = Hex numbers, %* = Binary numbers)';
End;

End.

