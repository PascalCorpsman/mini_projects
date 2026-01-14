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
Unit Unit2;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, uwave;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Procedure Edit1Change(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private
    fWave: TWave;
    Function getCutFrequency: integer;
    Function getHzPerRow: Single;
    Function getSegmentSize: integer;

  public
    Property SegmentSize: integer read getSegmentSize;
    Property HzPerRow: Single read getHzPerRow;
    Property CutFrequency: integer read getCutFrequency;
    Procedure InitWith(Const wave: TWave);
  End;

Var
  Form2: TForm2;

Implementation

{$R *.lfm}

Uses math;

{ TForm2 }

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  caption := 'Wave settings';
  edit1.text := '10'; // 10 ms Pro Spalte
  edit2.text := '3000'; // Alles oberhalb 3khz wird nicht mehr Dargestellt
  fWave := Nil;
End;

Procedure TForm2.Edit1Change(Sender: TObject);
Begin
  label13.Caption := inttostr(SegmentSize);
  label16.Caption := Format('%0.2f', [HzPerRow]);
End;

Function TForm2.getSegmentSize: integer;
Var
  duration: Single;
  msPerColum, TotalColums: integer;
Begin
  If Not assigned(fWave) Then Begin
    result := 0;
    exit;
  End;
  duration := fWave.SampleCount / fWave.SampleRate;
  msPerColum := strtointdef(edit1.text, 10);
  TotalColums := ceil(duration / (msPerColum * 0.001));
  result := fWave.SampleCount Div TotalColums;
End;

Function TForm2.getHzPerRow: Single;
Begin
  If Not assigned(fWave) Then Begin
    result := 0;
    exit;
  End;
  result := fWave.SampleRate / getSegmentSize();
End;

Function TForm2.getCutFrequency: integer;
Begin
  result := strtointdef(edit2.text, 3000);
End;

Procedure TForm2.InitWith(Const wave: TWave);
Begin
  fWave := wave;
  label2.caption := inttostr(fwave.SampleRate);
  label4.caption := inttostr(fwave.SampleCount);
  label8.caption := inttostr(fwave.ChannelCount);
  label6.caption := format('%0.2fs', [(fwave.SampleCount / fwave.SampleRate)]);
  Edit1Change(Nil);
End;

End.

