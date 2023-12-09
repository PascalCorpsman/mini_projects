(******************************************************************************)
(* Intercept theorem                                               ??.??.???? *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Demo to calculate the fourth missing value of the intercept  *)
(*               theorem                                                      *)
(*                                                                            *)
(* License     : See the file license.md, located under:                      *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(* Warranty    : There is no warranty, neither in correctness of the          *)
(*               implementation, nor anything other that could happen         *)
(*               or go wrong, use at your own risk.                           *)
(*                                                                            *)
(* Known Issues: none                                                         *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Label1: TLabel;
    Procedure Button1Click(Sender: TObject);
    Procedure Edit1KeyPress(Sender: TObject; Var Key: char);
    Procedure FormCreate(Sender: TObject);
  private

  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
  edit1.text := '100';
  edit2.text := '50';
  edit3.text := '200';
  edit4.text := '?';
  edit5.text := '';
  caption := 'Intercept theorem ver. 0.01';
End;

Procedure TForm1.Edit1KeyPress(Sender: TObject; Var Key: char);
Begin
  If key = #13 Then Button1.Click;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  //
  If trim(edit1.text) = '?' Then Begin
    edit5.text := FloatToStr(strtofloat(edit3.text) * strtofloat(edit2.text) / strtofloat(edit4.text));
  End;
  If trim(edit2.text) = '?' Then Begin
    edit5.text := FloatToStr(strtofloat(edit1.text) * strtofloat(edit4.text) / strtofloat(edit3.text));
  End;
  If trim(edit3.text) = '?' Then Begin
    edit5.text := FloatToStr(strtofloat(edit1.text) * strtofloat(edit4.text) / strtofloat(edit2.text));
  End;
  If trim(edit4.text) = '?' Then Begin
    edit5.text := FloatToStr(strtofloat(edit3.text) * strtofloat(edit2.text) / strtofloat(edit1.text));
  End;
End;

End.

