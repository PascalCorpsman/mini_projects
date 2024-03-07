(******************************************************************************)
(* Fixed_Comma                                                     ??.??.???? *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Demonstration of fixed comma numbers                         *)
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
(*
 * Online :
 *   Wikipedia: https://en.wikipedia.org/wiki/Q_(number_format)
 *   Wiki second source: https://www.pathpartnertech.com/representing-decimal-data-in-q-format/
 *   Q15 Online: https://www.venea.net/web/q_format_conversion
 *   Q15 Online Rechner: http://www.bytecraft.com/Fixed_Point_Converter
 *   Q*  Online Berechner nur Dezimal: https://www.rfwireless-world.com/calculators/floating-vs-fixed-point-converter.html
 *   Demoprogram: https://www.venea.net/web/qformatconverter
 * FPC Forum: https://forum.lazarus.freepascal.org/index.php?topic=17354.0
 *)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    RadioGroup1: TRadioGroup;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Edit1Change(Sender: TObject);
    Procedure Edit2KeyPress(Sender: TObject; Var Key: char);
    Procedure FormCreate(Sender: TObject);
  private

  public
    Function ShowAndCheckRanges(n, p: integer; s: Boolean): Boolean;
  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses ufixed;

{ TForm1 }

Function IntToHexString(Value: uint64; n: integer): String;
Begin
  result := 'Invalid n';
  Case n Of
    8: result := format('0x%0.2X', [UInt8(Value)]);
    16: result := format('0x%0.4X', [UInt16(Value)]);
    32: result := format('0x%0.8X', [UInt32(Value)]);
  End;
End;

Function HexStringToInt(Value: String; n: integer): uint64;
Const
  conv = '0123456789ABCDEF';
Var
  tmp, tmp2: uint64;
Begin
  result := 0;
  value := uppercase(value);
  If pos('0X', Value) = 1 Then Begin
    value := copy(Value, 3, length(value));
  End;
  While value <> '' Do Begin
    result := result * 16;
    result := result + pos(value[1], conv) - 1;
    delete(value, 1, 1);
  End;
  // Wenn das Höchste Bit gesetzt ist, dann muss dieses bis zum Bit 64 durch gespiegelt werden
  If ((1 Shl (n - 1)) And result <> 0) Then Begin
    tmp := uint64($FFFFFFFFFFFFFFFF);
    tmp2 := (1 Shl n) - 1;
    tmp := tmp And (Not tmp2);
    result := result Or tmp;
  End;
End;

Function DoubleToString(Value: Double; Precision: integer = 32): String;
Begin
  result := Format('%.' + inttostr(Precision) + 'f', [value]);
  While length(result) > 1 Do Begin
    If (result[length(result)] = '0') Or (result[length(result)] = FormatSettings.DecimalSeparator) Then Begin
      delete(result, length(Result), 1);
    End
    Else Begin
      exit;
    End;
  End;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
  caption := 'Fixed number simulator ver 0.01';
  ComboBox1.Items.Clear;
  ComboBox1.Items.Add('8');
  ComboBox1.Items.Add('16');
  //ComboBox1.Items.Add('32'); -- Irgendwie komisch
  //ComboBox1.Items.Add('64'); -- Mal schaun ob wir das auch hin kriegen
  ComboBox1.ItemIndex := 1;
  edit1.text := '15';
  edit2.text := floattostr(0.5);
  edit4.text := '0x0';
  edit5.text := '';
  edit6.text := '';
  Button1.Click();
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  a: TFixedComma;
  n, p: integer;
Begin
  // Convert to Fixed
  n := strtointdef(ComboBox1.Text, -1);
  p := strtointdef(Edit1.Text, -1);
  If ShowAndCheckRanges(n, p, CheckBox1.Checked) Then Begin
    a := TFixedComma.Create(n, p, CheckBox1.Checked);
    a.Value := strtofloat(edit2.text);
    edit3.text := IntToHexString(a.RaWValue, n);
    a.free;
  End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Var
  a, b, c: TFixedComma;
  n, p: integer;
Begin
  // Change einer Eingabe
  n := strtointdef(ComboBox1.Text, -1);
  p := strtointdef(Edit1.Text, -1);
  If ShowAndCheckRanges(n, p, CheckBox1.Checked) Then Begin
    // Calc
    a := TFixedComma.Create(n, p, CheckBox1.Checked);
    b := TFixedComma.Create(n, p, CheckBox1.Checked);
    a.RaWValue := HexStringToInt(edit3.text, n);
    b.RaWValue := HexStringToInt(edit4.text, n);
    c := a + b;
    Edit5.Text := IntToHexString(c.RaWValue, n);
    a.free;
    b.free;
    c.free;
  End;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  edit4.text := edit3.text;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Var
  a: TFixedComma;
  n, p: integer;
Begin
  // Convert Fixed to Float
  n := strtointdef(ComboBox1.Text, -1);
  p := strtointdef(Edit1.Text, -1);
  If ShowAndCheckRanges(n, p, CheckBox1.Checked) Then Begin
    // Calc
    a := TFixedComma.Create(n, p, CheckBox1.Checked);
    a.RaWValue := HexStringToInt(edit5.text, n);
    Edit6.Text := DoubleToString(a.Value);
    a.free;
  End;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button6Click(Sender: TObject);
Var
  a: TFixedComma;
Var
  n, p: integer;
Begin
  // Change einer Eingabe
  n := strtointdef(ComboBox1.Text, -1);
  p := strtointdef(Edit1.Text, -1);
  If ShowAndCheckRanges(n, p, CheckBox1.Checked) Then Begin
    a := TFixedComma.Create(n, p, CheckBox1.Checked);
    a.RaWValue := $7FFF;
    memo1.text := format('%.' + inttostr(p) + 'f', [a.Value]);
    a.free;
  End;
End;

Procedure TForm1.Edit1Change(Sender: TObject);
Var
  n, p: integer;
Begin
  // Change einer Eingabe
  n := strtointdef(ComboBox1.Text, -1);
  p := strtointdef(Edit1.Text, -1);
  ShowAndCheckRanges(n, p, CheckBox1.Checked);
End;

Procedure TForm1.Edit2KeyPress(Sender: TObject; Var Key: char);
Begin
  If key = #13 Then Begin
    Button1.Click;
    exit;
  End;
  If Not (key In ['0'..'9', #8, '-']) Then Begin
    If Not (key = FormatSettings.DecimalSeparator) Then
      key := FormatSettings.DecimalSeparator;
  End;
End;

Function TForm1.ShowAndCheckRanges(n, p: integer; s: Boolean): Boolean;
Var
  ub, lb: TFixedComma;
Begin
  memo1.text := 'Invalid settings.';
  result := false;
  If (n <= 0) Or (n Mod 8 <> 0) Then exit;
  If (p <= 0) Or (p >= n) Then exit;
  ub := TFixedComma.Create(n, p, s);
  lb := TFixedComma.Create(n, p, s);
  If s Then Begin
    Case n Of
      8: Begin
          ub.RaWValue := $7F;
          lb.RaWValue := uint64($FFFFFFFFFFFFFF80);
        End;
      16: Begin
          ub.RaWValue := $7FFF;
          lb.RaWValue := uint64($FFFFFFFFFFFF8000);
        End;
      32: Begin
          ub.RaWValue := $7FFFFFFF;
          lb.RaWValue := uint64($FFFFFFFF80000000);
        End;
    End;
  End
  Else Begin
    lb.RaWValue := $0;
    Case n Of
      8: ub.RaWValue := $FF;
      16: ub.RaWValue := $FFFF;
      32: ub.RaWValue := $FFFFFFFF;
    End;
  End;
  memo1.Text := format('Q%d = Q%d.%d', [p, n - p - 1, p]) + LineEnding +
    //  'Range = [' + DoubleToString(lb.Value) + '..' + DoubleToString(ub.Value) + ']'; -- So wäre es so genau wie nur irgend möglich
  'Range = [' + DoubleToString(lb.Value, 5) + '..' + DoubleToString(ub.Value, 5) + ']';
  lb.free;
  ub.free;
  result := true;
End;

End.

