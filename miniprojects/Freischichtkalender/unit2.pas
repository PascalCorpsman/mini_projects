(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Freischichtkalender                                   *)
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

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ufcal;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    RadioGroup1: TRadioGroup;
    Procedure Button1Click(Sender: TObject);
    Procedure Edit1Change(Sender: TObject);
    Procedure Edit2Change(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private

  public
    cal: TFCal;
  End;

Var
  Form2: TForm2;

Implementation

{$R *.lfm}

{ TForm2 }

Procedure TForm2.Button1Click(Sender: TObject);
Var
  sd, sm, ed, em, i: integer;
Begin
  sd := strtoint(edit1.text);
  sm := strtoint(edit2.text);
  ed := strtoint(edit4.text);
  em := strtoint(edit5.text);
  Case RadioGroup1.ItemIndex Of
    0: Begin // Holidays
        For i := sm * 100 + sd To em * 100 + ed Do Begin
          If (i Mod 100 <= 31) Then Begin // Das sind einige Zu viel stört aber nicht
            setlength(cal.Holidays, length(cal.Holidays) + 1);
            cal.Holidays[high(cal.Holidays)].Day := i Mod 100;
            cal.Holidays[high(cal.Holidays)].Month := i Div 100;
            cal.Holidays[high(cal.Holidays)].Title := edit3.text;
          End;
        End;
      End;
    1: Begin //Vacation
        For i := sm * 100 + sd To em * 100 + ed Do Begin
          If (i Mod 100 <= 31) Then Begin // Das sind einige Zu viel stört aber nicht
            setlength(cal.VacationDays, length(cal.VacationDays) + 1);
            cal.VacationDays[high(cal.VacationDays)].Day := i Mod 100;
            cal.VacationDays[high(cal.VacationDays)].Month := i Div 100;
            cal.VacationDays[high(cal.VacationDays)].Title := edit3.text;
          End;
        End;
      End;
    2: Begin // Label
        For i := sm * 100 + sd To em * 100 + ed Do Begin
          If (i Mod 100 <= 31) Then Begin // Das sind einige Zu viel stört aber nicht
            setlength(cal.LabeledDays, length(cal.LabeledDays) + 1);
            cal.LabeledDays[high(cal.LabeledDays)].Day := i Mod 100;
            cal.LabeledDays[high(cal.LabeledDays)].Month := i Div 100;
            cal.LabeledDays[high(cal.LabeledDays)].Title := edit3.text;
          End;
        End;
      End;
  End;
  ModalResult := mrOK;
End;

Procedure TForm2.Edit1Change(Sender: TObject);
Begin
  edit4.text := edit1.text;
End;

Procedure TForm2.Edit2Change(Sender: TObject);
Begin
  edit5.text := edit2.text;
End;

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  caption := 'Add';
  edit1.text := '';
  edit2.text := '';
  edit3.text := '';
  edit4.text := '';
  edit5.text := '';
End;

End.

