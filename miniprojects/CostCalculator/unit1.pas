(******************************************************************************)
(* Costcalculator                                                  10.09.2025 *)
(*                                                                            *)
(* Version     : 0.02                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : App to calculate current prices for houses                   *)
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
(*               0.02 - Umrechner für Akkugrößen                              *)
(*                      Umstellen auf Eng                                     *)
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
    Edit11: TEdit;
    Edit12: TEdit;
    Edit2: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    Edit9: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Memo1: TMemo;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Edit6Change(Sender: TObject);
    Procedure Edit9Change(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private

  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses math;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  caption := 'Costcalculater ver. 0.02';
  edit1.text := format('%0.3f', [0.34]);
  edit2.text := format('%0.3f', [0.082]);

  // Debug 2024
  edit4.text := floattostr(2.96);
  edit5.text := floattostr(2.14);
  edit6.text := floattostr(1.63);
  edit7.text := floattostr(5.13);
  edit8.text := floattostr(2.22);
  edit9.text := floattostr(1.63);

  edit11.text := format('%0.1f', [18.4]);
  edit12.text := format('%0.1f', [18.4]);
  Constraints.MinWidth := Width;
  Constraints.MinHeight := Height;
  Memo1.Clear;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Edit6Change(Sender: TObject);
Begin
  edit9.text := edit6.text;
End;

Procedure TForm1.Edit9Change(Sender: TObject);
Begin
  edit6.text := edit9.text;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Const
  cur = '€';
  Epsilon = 0.001;
Var
  AkkuScale, akkuist, akkusoll,
    vNetz, vAkku, vSonne,
    eNetz, eAkku, eSonne,
    kStrom, eStrom
    , StromKostenOhnePV, ErtragPVBei100ProzentEinspeisung, Autarkiegrad,
    TatsaechlicheStromkosten, ErtragDurchPV,
    VerrechnungBezugVsEinspeisung, ErsparnisDurchNutzungPV,
    MonatlicherAbschlagOhnePV, MonatlicherAbschlagMitPV,
    MonatlicherAbschlagStromanbieter: Single;
Begin
  kStrom := StrToFloatdef(edit1.text, 0);
  eStrom := StrToFloatdef(edit2.text, 0);

  vNetz := StrToFloatdef(edit4.text, 0);
  vAkku := StrToFloatdef(edit5.text, 0);
  vSonne := StrToFloatdef(edit6.text, 0);
  eNetz := StrToFloatdef(edit7.text, 0);
  eAkku := StrToFloatdef(edit8.text, 0);
  eSonne := StrToFloatdef(edit9.text, 0);

  If abs(vSonne - eSonne) > Epsilon Then Begin
    Showmessage('Error, "Sun->House" from consumption is different to generation..');
    exit;
  End;

  // Ohne Akku, muss alles was durch den Akku ging, durchs Netz
  akkuist := StrToFloatdef(edit11.text, 0);
  akkusoll := StrToFloatdef(edit12.text, 0);
  // Der Akku soll umsgalliert werden
  If akkuist <> 0 Then Begin
    AkkuScale := (akkusoll / akkuist);
    vNetz := vNetz + vAkku * (1 - AkkuScale);
    eNetz := eNetz + eAkku * (1 - AkkuScale);
    vAkku := vAkku * AkkuScale;
    eAkku := eAkku * AkkuScale;
  End;
  // Die Ganzen Formeln
  StromKostenOhnePV := (vNetz + vAkku + vSonne) * 1000 * kStrom;
  TatsaechlicheStromkosten := vNetz * 1000 * kStrom;

  ErtragPVBei100ProzentEinspeisung := (eNetz + eAkku + eSonne) * 1000 * eStrom;
  ErtragDurchPV := eNetz * 1000 * eStrom;

  Autarkiegrad := (vAkku + vSonne) / (vNetz + vAkku + vSonne) * 100;
  // Gespart wird dadurch, das anstatt den Bezugspreis zu bezahlen,
  // nur der Verkaufspreis bezahlt werden muss -> Die Differenz ist die Ersparnis
  ErsparnisDurchNutzungPV := (vAkku + vSonne) * 1000 * (kStrom - eStrom);

  VerrechnungBezugVsEinspeisung := -(TatsaechlicheStromkosten - ErtragDurchPV);

  MonatlicherAbschlagOhnePV := StromKostenOhnePV / 12;
  MonatlicherAbschlagMitPV := TatsaechlicheStromkosten / 12;
  MonatlicherAbschlagStromanbieter := ErtragDurchPV / 12;

  memo1.Clear;
  memo1.Append(format('Total consumption                   : %7.2f %s', [
    vNetz + vAkku + vSonne
      , 'MWh'
      ]));
  memo1.Append(format('Total generation                    : %7.2f %s', [
    eNetz + eAkku + eSonne
      , 'MWh'
      ]));
  memo1.Append('');

  memo1.Append(format('Total cost if no PV is present      : %7.2f %s', [
    StromKostenOhnePV
      , cur
      ]));
  memo1.Append(format('Profit by PV                        : %7.2f %s', [
    ErtragDurchPV
      , cur
      ]));
  memo1.Append(format('Profit by PV (at 100%% feed)         : %7.2f %s', [
    ErtragPVBei100ProzentEinspeisung
      , cur
      ]));
  memo1.Append(format('Savings through the use of PV+Akku  : %7.2f %s', [
    ErsparnisDurchNutzungPV, cur
      ]));
  memo1.Append('');

  memo1.Append(format('degree of self-sufficiency          : %7.0f %% (%0.2fMWh)', [
    Autarkiegrad
      , (vAkku + vSonne)
      ]));
  memo1.Append(format('Akku efficiency                     : %7.0f %% (Loss: %0.2fMWh)', [
    (vAkku / max(1, eAkku)) * 100
      , eAkku - vAkku
      ]));
  memo1.Append('-----------------------------------------------');

  memo1.Append(format('Actual electricity costs with PV    : %7.2f %s', [
    TatsaechlicheStromkosten
      , cur
      ]));
  memo1.Append(format('Monthly payment without use of PV   : %7.2f %s', [
    MonatlicherAbschlagOhnePV
      , cur
      ]));
  memo1.Append(format('Monthly payment with use of PV      : %7.2f %s', [
    MonatlicherAbschlagMitPV
      , cur
      ]));
  memo1.Append(format('Monthly electricity provider payment: %7.2f %s', [
    MonatlicherAbschlagStromanbieter
      , cur
      ]));
  memo1.Append('');

  memo1.Append(format('Billing of consumption / feed-in    :%8.2f %s (positive = profit, negative = costs)', [
    VerrechnungBezugVsEinspeisung
      , cur
      ]));
  memo1.Append(format('Total Monthly difference(considered):%8.2f %s (positive = costs, negative = profit)', [
    -VerrechnungBezugVsEinspeisung / 12
      , cur
      ]));
End;

End.

