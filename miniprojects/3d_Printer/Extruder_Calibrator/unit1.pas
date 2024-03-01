(******************************************************************************)
(* Extruder calibrator                                             ??.??.???? *)
(*                                                                            *)
(* Version     : 0.03                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Application to calibrate the e-steps for a 3D-Printer        *)
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
(*               0.02 - Integrieren Wizard                                    *)
(*               0.03 - FIX, did not work with marlin Firmware                *)
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
    Memo1: TMemo;
    Memo2: TMemo;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private
    Procedure OnTrace(Sender: TObject; Message: String);

  public

  End;

Var
  Form1: TForm1;
  fformatsettings: TFormatSettings;

Implementation

{$R *.lfm}

Uses u3d_printer, Unit2;

(*
Alte E-Steps = 95.00 ohne 3:1 Aufsatz
              285.00 mit  3:1 Aufsatz -> 395.8
*)

// Quelle : https://mattshub.com/2017/04/19/extruder-calibration/

Function CalcNewESteps(old_esteps, measured_len: Double): Double;
Var
  stepstaken, extruded_len: Double;
Begin
  extruded_len := 120 - measured_len;
  stepstaken := old_esteps * 100;
  result := stepstaken / extruded_len;
End;

Function ExtractOldStepsFromPrinterMessage(s: String): Double;
Var
  sl: TStringList;
  i: Integer;
  t: String;
Begin
  result := 0;
  sl := TStringList.Create;
  sl.Text := s;
  For i := 0 To sl.Count - 2 Do Begin
    If pos('STEPS PER UNIT', uppercase(trim(sl[i]))) <> 0 Then Begin
      t := uppercase(sl[i + 1]);
      t := copy(t, pos(' E', t) + 2, length(t)) + ' ';
      t := copy(t, 1, pos(' ', t) - 1);
      result := StrToFloatDef(t, 0, fformatsettings);
      sl.free;
      exit;
    End;
  End;

  (*
  echo:Steps per unit:
  echo:  M92 X80.00 Y80.00 Z400.00 E95.00
  echo:Maximum feedrates (mm/s):
  echo:  M203 X400.00 Y400.00 Z4.00 E25.00
  echo:Maximum Acceleration (mm/s2):
  echo:  M201 X9000 Y5000 Z50 E10000
  echo:Acceleration: S=acceleration, T=retract acceleration
  echo:  M204 S1000.00 T1000.00
  echo:Advanced variables: S=Min feedrate (mm/s), T=Min travel feedrate (mm/s), B=minimum segment time (ms), X=maximum XY jerk (mm/s),  Z=maximum Z jerk (mm/s),  E=maximum E jerk (mm/s)
  echo:  M205 S0.00 T0.00 B20000 X20.00 Z0.30 E10.00
  echo:Home offset (mm):
  echo:  M206 X0.00 Y0.00 Z0.00
  echo:PID settings:
  echo:   M301 P22.20 I1.08 D114.00

  *)
  sl.free;
End;

{ TForm1 }

Procedure TForm1.Button1Click(Sender: TObject);
Var
  NewSteps, measured_len, old_esteps: Double;
Begin
  measured_len := strtofloat(edit1.text);
  old_esteps := strtofloat(edit2.text);
  newsteps := CalcNewESteps(old_esteps, measured_len);
  label3.caption := 'New e-steps: ' + format('%.1f', [newsteps], fformatsettings);
  edit3.text := 'M92 E' + format('%.1f', [newsteps], fformatsettings);
End;

Procedure TForm1.Button2Click(Sender: TObject);
  Function ResultOK(aValue: String): Boolean;
  Var
    sl: TStringList;
  Begin
    result := false;
    sl := TStringList.Create;
    sl.Text := aValue;
    If sl.Count <> 0 Then Begin
      If pos('OK', uppercase(sl[sl.Count - 1])) <> 0 Then Begin
        result := true;
      End;
    End;
    sl.free;
  End;

Var
  p: T3D_Printer;
  measuredlength, newsteps, oldsteps: Double;
  s: String;
Begin
  showmessage('This wizard automates as much as possible, for your extruder calibration.' + LineEnding + LineEnding +
    '1. draw a mark on your filament 120mm away from a fixpoint' + LineEnding + LineEnding +
    'Click OK if you have done this.');
  p := T3D_Printer.Create();
  memo2.clear;
  p.Tracer := @OnTrace;
  If Not p.Connect(edit4.text) Then Begin
    showmessage('Could not connect to printer.');
    p.free;
    exit;
  End;
  // Warten bis erfolgreich verbunden
  While Not p.IsConnected Do Begin
    CheckSynchronize(1);
    sleep(1);
  End;
  sleep(5000); // Dem Drucker Zeit geben "online" zu kommen
  // Auslesen der alten E-Steps
  s := trim(uppercase(p.ExecGCode('M503')));
  oldsteps := ExtractOldStepsFromPrinterMessage(s);
  If oldsteps <= 0 Then Begin
    showmessage('Could not read e-steps value from printer');
    p.free;
    exit;
  End;
  // Nozzle Aufheizen
  If Not ResultOK(p.ExecGCode('M109 S' + trim(edit5.text))) Then Begin
    showmessage('Could not set Nozzle temperature');
    p.free;
    exit;
  End;
  // Umschalten Relativ Modus
  If Not ResultOK(p.ExecGCode('M83')) Then Begin
    showmessage('Could not set Relative Mode');
    p.free;
    exit;
  End;
  // Geschwindigkeit auf 50% reduzieren
  If Not ResultOK(p.ExecGCode('M220 S50')) Then Begin
    showmessage('Could not set print speed to 50%');
    p.free;
    exit;
  End;
  // 10cm Filament Extrudieren
  If Not ResultOK(p.ExecGCode('G1 E100 F100')) Then Begin
    showmessage('Could not extrude 100mm of filament');
    p.free;
    exit;
  End;
  form2.edit1.text := floattostr(20);
  If form2.ShowModal <> mrOK Then Begin
    showmessage('Cancel by User.');
    p.free;
    exit;
  End;
  measuredlength := StrToFloatDef(trim(form2.Edit1.Text), 0);
  If measuredlength <= 0 Then Begin
    showmessage('Invalid measured lengt: ' + form2.Edit1.Text);
    p.free;
    exit;
  End;
  newsteps := CalcNewESteps(oldsteps, measuredlength);
  // Dem User die Ergebnisse zeigen
  edit1.text := floattostr(measuredlength);
  edit2.Text := FloatToStr(oldsteps);
  Button1Click(Nil);
  If newsteps <= 0 Then Begin
    showmessage('Invalid calculated new e-steps: ' + floattostr(newsteps));
    p.free;
    exit;
  End;
  If Not ResultOK(p.ExecGCode('M92 E' + format('%.1f', [newsteps], fformatsettings))) Then Begin
    showmessage('Could not send new e-steps value to printer.');
    p.free;
    exit;
  End;
  If pos('ECHO:SETTINGS STORED', trim(uppercase(p.ExecGCode('M500')))) <> 0 Then Begin
    showmessage('Could not store values to printer memory');
    p.free;
    exit;
  End;
  p.free;
  showmessage('Done. printer is now calibrated.' + LineEnding +
    'It''s recomended to repeat this step at least once to test wheter the calibration succeed.');
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  caption := 'Extruder calibrator v. 0.03';
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
  fformatsettings := DefaultFormatSettings;
  fformatsettings.DecimalSeparator := '.';
  Memo2.clear;
  edit1.text := floattostr(20.0);
  edit2.text := floattostr(95.0);
  label3.caption := '';
  edit3.text := '';
  edit5.text := '220';
{$IFDEF Windows}
  edit4.text := 'COM1';
{$ELSE}
  edit4.text := '/dev/ttyUSB0';
{$ENDIF}
End;

Procedure TForm1.OnTrace(Sender: TObject; Message: String);
Begin
  Memo2.Lines.Add(Message);
  memo2.SelStart := length(Memo2.Text);
  Application.ProcessMessages;
End;

End.

