(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Einstein                                              *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit usetframe;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ComCtrls, IniFiles;

Type

  { TSetframe }

  TSetframe = Class(TFrame)
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Procedure Edit1Change(Sender: TObject);
  protected
    fDataChanged: Boolean;
  private
    Procedure SetFirstSet(AValue: Boolean);
    Procedure MarkAsChanged(Sender: TObject);
    { private declarations }
  public
    { public declarations }
    SetEdits: Array Of TEdit;
    SetLabels: Array Of TLabel;
    Property FirstSet: Boolean write SetFirstSet;
    Constructor Create(TheOwner: TComponent); override;
    Destructor Destroy; override;

    Procedure SaveToFile(Const ini: Tinifile; index: integer);
    Procedure LoadFromFile(Const ini: Tinifile; index: integer);

    Procedure SetValues(Const Data: Array Of String); // Ist nur für den OnCreate Code 1. Element ist die Caption, der rest die Elemente
    Function DataChanged: Boolean;
  End;

Implementation

Uses ueinstein;

{$R *.lfm}

{ TSetframe }

Procedure TSetframe.Edit1Change(Sender: TObject);
Var
  i: Integer;
Begin
  TTabSheet(parent).Caption := edit1.text;
  For i := 0 To EinsteinWidth - 1 Do Begin
    SetLabels[i].Caption := Edit1.Text + ' ' + inttostr(i + 1);
  End;
  MarkAsChanged(Nil);
End;

Procedure TSetframe.SetFirstSet(AValue: Boolean);
Begin
  label2.Visible := AValue;
  label3.Visible := AValue;
End;

Procedure TSetframe.MarkAsChanged(Sender: TObject);
Begin
  fDataChanged := true;
End;

Constructor TSetframe.Create(TheOwner: TComponent);
Var
  i: integer;
Begin
  Inherited Create(TheOwner);
  fDataChanged := false;
  setlength(SetEdits, EinsteinWidth);
  setlength(SetLabels, EinsteinWidth);
  For i := 0 To EinsteinWidth - 1 Do Begin
    SetLabels[i] := TLabel.Create(self);
    SetLabels[i].Parent := Self;
    SetLabels[i].Left := 8;
    SetLabels[i].Top := 56 + i * 32;
    SetEdits[i] := TEdit.Create(Self);
    SetEdits[i].Parent := Self;
    SetEdits[i].Left := 100;
    SetEdits[i].Top := 48 + i * 32;
    SetEdits[i].Width := 150;
    SetEdits[i].OnChange := @MarkAsChanged;
  End;
  label2.top := SetLabels[0].Top;
  label2.Left := SetEdits[0].Left + SetEdits[0].Width + 15;
  label3.top := SetLabels[EinsteinWidth - 1].Top;
  label3.Left := SetEdits[EinsteinWidth - 1].Left + SetEdits[EinsteinWidth - 1].Width + 15;
End;

Destructor TSetframe.Destroy;
Begin
  Inherited Destroy;
End;

Procedure TSetframe.SaveToFile(Const ini: Tinifile; index: integer);
Var
  Section: String;
  i: Integer;
Begin
  Section := 'Set' + inttostr(index);
  ini.WriteString(Section, 'Name', Edit1.text);
  For i := 0 To high(SetEdits) Do Begin
    ini.WriteString(Section, 'Element' + IntToStr(i), SetEdits[i].Text);
  End;
  fDataChanged := false;
End;

Procedure TSetframe.LoadFromFile(Const ini: Tinifile; index: integer);
Var
  Section: String;
  i: Integer;
Begin
  Section := 'Set' + inttostr(index);
  Edit1.text := ini.ReadString(Section, 'Name', '');
  For i := 0 To high(SetEdits) Do Begin
    SetEdits[i].Text := ini.ReadString(Section, 'Element' + IntToStr(i), '');
  End;
  fDataChanged := false;
End;

Procedure TSetframe.SetValues(Const Data: Array Of String);
Var
  i: integer;
Begin
  edit1.text := data[0];
  For i := 0 To high(data) - 1 Do Begin
    SetEdits[i].Text := data[i + 1];
  End;
  fDataChanged := false; // wird nur im OnShow Code gemacht, also als Initialisierung, deswegen keine Änderung, obwohl streng genommen falsch
End;

Function TSetframe.DataChanged: Boolean;
Begin
  result := fDataChanged;
End;

End.

