(******************************************************************************)
(* RAM-User                                                        11.07.2025 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Application to allocate RAM                                  *)
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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Timer1: TTimer;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Edit1Change(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private
    fBlocks: Array Of Array Of Byte;
    fBlockIndex, fBlocksize, fBlockCount: Integer;
    Procedure updateRAMUsageInfo;
  public

  End;

Var
  Form1: TForm1;

Implementation

Uses math;

{$R *.lfm}

Function FileSizeToString(Value: Int64): String;
Var
  s: char;
  r: Int64;
Begin
  s := ' ';
  r := 0;
  If value >= 1000 Then Begin
    s := 'K';
    r := value Mod 1000;
    value := value Div 1000;
  End;
  If value >= 1000 Then Begin
    s := 'M';
    r := value Mod 1000;
    value := value Div 1000;
  End;
  If value >= 1000 Then Begin
    s := 'G';
    r := value Mod 1000;
    value := value Div 1000;
  End;
  If value >= 1000 Then Begin
    s := 'T';
    r := value Mod 1000;
    value := value Div 1000;
  End;
  If value >= 1000 Then Begin
    s := 'P';
    r := value Mod 1000;
    value := value Div 1000;
  End;
  If (r Div 100) <> 0 Then Begin
    value := value * 10 + round(r / 100);
    result := format('%0.1f%sB', [value / 10, s]);
  End
  Else
    result := inttostr(value) + s + 'B'
End; 

{ TForm1 }

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  Button2Click(Nil);
  close;
End;

Procedure TForm1.Edit1Change(Sender: TObject);
Var
  Blocksize, BlockCount: Integer;
Begin
  Blocksize := strtointdef(edit1.text, 0);
  BlockCount := strtointdef(edit2.text, 0);
  label5.caption := FileSizeToString(Blocksize * BlockCount);
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  Button2Click(Nil);
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  Randomize;
  (*
   * History: 0.01 = Initialversion (11.07.2025)
   *)
  caption := 'RAM user, by Corpsman ver 0.01';
  edit1.text := '1048576'; // 1 MB
  // edit1.text := '20971520'; // 20 MB
  edit2.text := '1024';
  edit3.text := '10';
  label7.caption := '';
  fBlocks := Nil;
End;

Procedure TForm1.Timer1Timer(Sender: TObject);
Var
  j: Integer;
Begin
  If fBlockIndex <= high(fBlocks) Then Begin
    setlength(fBlocks[fBlockIndex], fBlocksize);
    If CheckBox1.Checked Then Begin
      For j := 0 To high(fBlocks[fBlockIndex]) Do Begin
        fBlocks[fBlockIndex, j] := Random(256);
      End;
    End;
    fBlockIndex := fBlockIndex + 1;
    updateRAMUsageInfo;
  End
  Else Begin
    Timer1.Enabled := false;
  End;
End;

Procedure TForm1.updateRAMUsageInfo;
Var
  i: integer;
  s: UInt64;
Begin
  s := length(fBlocks) * 4;
  For i := 0 To high(fBlocks) Do Begin
    s := s + length(fBlocks[i]);
  End;
  label7.caption := FileSizeToString(s);
End;

Procedure TForm1.Button2Click(Sender: TObject);
Var
  i: Integer;
Begin
  // Free
  Button1.Enabled := true;
  Button2.Enabled := false;
  Timer1.Enabled := false;
  For i := 0 To high(fBlocks) Do Begin
    setlength(fBlocks[i], 0);
  End;
  setlength(fBlocks, 0);
  label7.caption := '';
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  i, Delta, j: integer;
Begin
  fBlocksize := strtointdef(edit1.text, 0);
  fBlockCount := strtointdef(edit2.text, 0);
  // Allocate
  Button1.Enabled := false;
  If RadioButton1.Checked Then Begin
    // Instant
    setlength(fBlocks, fBlockCount);
    For i := 0 To fBlockCount - 1 Do Begin
      setlength(fblocks[i], fBlocksize);
      If CheckBox1.Checked Then Begin
        For j := 0 To high(fBlocks[i]) Do Begin
          fBlocks[i, j] := Random(256);
        End;
      End;
    End;
    updateRAMUsageInfo;
  End
  Else Begin
    // Over Time
    Delta := max(10, strtointdef(edit3.text, 100));
    Timer1.Interval := Delta;
    Timer1.Enabled := true;
    setlength(fBlocks, fBlockCount);
    fBlockIndex := 0;
  End;
  Button2.Enabled := true;
End;

End.


