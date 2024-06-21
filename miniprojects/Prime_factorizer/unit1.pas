(******************************************************************************)
(* Prime factorizer                                                21.06.2024 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : calc the prime factors of a given number                     *)
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
    Label1: TLabel;
    Memo1: TMemo;
    Procedure Button1Click(Sender: TObject);
    Procedure Edit1KeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure FormCreate(Sender: TObject);
  private

  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses LCLType, math;

{ TForm1 }

Procedure TForm1.Button1Click(Sender: TObject);
Var
  sqrtn, n, i, j: uint64;
  s: String;
Begin
  Memo1.Clear;
  // Load the requested number and a bit of error handling ;)
  edit1.text := trim(edit1.text);
  n := StrToUInt64Def(edit1.text, 1);
  If (n = 1) And (edit1.text <> '1') Then Begin
    memo1.Append('Error, could not handle: ' + Edit1.Text);
    exit;
  End;
  // 1 is not a prime, but we need to plot at least something
  If n = 1 Then Begin
    memo1.Append('1');
    exit;
  End;
  sqrtn := ceil(sqrt(n)); // Calc upper boarder for shortcut testing ;) -> This speedsup the test by a lot if n is a high prime !
  // Calc all prime factors
  // The algorithm is a bit like the Sieve of Eratosthenes algorithm, but only for n and not all below n
  // Loop Unroll for i = 2
  i := 2;
  j := 0;
  While n Mod i = 0 Do Begin
    inc(j);
    n := n Div i;
  End;
  If j <> 0 Then Begin
    s := format('%d^%d', [i, j]);
    Memo1.Append(s);
  End;
  // Now test each odd number until n is 1 ;)
  i := 3;
  While n <> 1 Do Begin
    s := '';
    j := 0;
    While n Mod i = 0 Do Begin
      inc(j);
      n := n Div i;
    End;
    If j <> 0 Then Begin
      s := format('%d^%d', [i, j]);
      Memo1.Append(s);
    End;
    inc(i, 2);
    // Shortcut, what ever "rest" is now left, it is itself a prime ;)
    If i > sqrtn Then Begin
      i := n;
    End;
  End;
End;

Procedure TForm1.Edit1KeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState
  );
Begin
  If key = vk_return Then button1.Click;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  caption := 'Prime factor calc ver. 0.01';
  edit1.text := '42';
  Button1.Click;
End;

End.

