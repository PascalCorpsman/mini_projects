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
Unit uonesetruleframe;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ueinstein, IniFiles;

Type

  { TOneSetRuleFrame }

  TOneSetRuleFrame = Class(TEinsteinRuleFrame)
    Button1: TButton;
    GroupBox1: TGroupBox;
    Procedure Button1Click(Sender: TObject);
  private
    { private declarations }
    SetLabels: Array Of TLabel;
    SetComboboxes: Array Of TComboBox;
    HintLabel: TLabel;
    HintEdit: TEdit;
  public
    { public declarations }
    Constructor Create(TheOwner: TComponent); override;

    Procedure SetCaption(Value: String); override;

    Procedure RefreshTexts; override;

    Procedure SetSet(Index: integer; Set_: TMemberSet); override;
    Function GetSet(Index: integer): TMemberSet; override;

    Procedure SaveToFile(Const Ini: TInifile; Index: integer); override;
    Procedure LoadFromFile(Const Ini: TInifile; Index: integer); override;

    Function ValidRule: Boolean; override;
    Function DeriveRule: TRule; override;

    Procedure SetAttribute(Index: integer; Value: integer); override;
    Procedure SetAttributeCountAndLabels(Const Labels: Array Of String); override;
    Procedure SetUserHint(Data: String); override;
  End;

Implementation

Uses math;

{$R *.lfm}

{ TOneSetRuleFrame }

Procedure TOneSetRuleFrame.Button1Click(Sender: TObject);
Begin
  // Del (damit ist die eigene Regel gemeint)
  SelfDeleteCallback(self);
End;

Constructor TOneSetRuleFrame.Create(TheOwner: TComponent);
Var
  i: Integer;
Begin
  Inherited Create(TheOwner);
  setlength(SetLabels, EinsteinHeight);
  setlength(SetComboboxes, EinsteinHeight);
  For i := 0 To EinsteinHeight - 1 Do Begin
    SetLabels[i] := TLabel.Create(GroupBox1);
    SetLabels[i].Parent := GroupBox1;
    SetLabels[i].Name := 'SetLabel' + IntToStr(i);
    SetLabels[i].Caption := 'Label' + inttostr(i + 1);
    SetLabels[i].Top := 8;
    SetLabels[i].Left := 8 + 112 * i;
    SetComboboxes[i] := TComboBox.Create(GroupBox1);
    SetComboboxes[i].Parent := GroupBox1;
    SetComboboxes[i].Name := 'SetCombobox' + IntToStr(i);
    SetComboboxes[i].Text := '';
    SetComboboxes[i].Items.Clear;
    SetComboboxes[i].Top := 32;
    SetComboboxes[i].Left := 8 + 112 * i;
    SetComboboxes[i].Width := 100;
    SetComboboxes[i].ReadOnly := true;
    SetComboboxes[i].OnChange := @MarkAsChanged;
  End;
  GroupBox1.Width := 568 + (EinsteinHeight - 5) * 112;
  Button1.left := 584 + (EinsteinHeight - 5) * 112;
  self.width := 642 + (EinsteinHeight - 5) * 112;

  HintLabel := TLabel.Create(GroupBox1);
  HintLabel.Parent := GroupBox1;
  HintLabel.Left := 8;
  HintLabel.Top := 32 + 32;
  HintLabel.caption := 'Hint : ';
  HintLabel.Name := 'HintLabel';

  HintEdit := TEdit.Create(GroupBox1);
  HintEdit.Parent := GroupBox1;
  HintEdit.Name := 'HintEdit';
  HintEdit.Top := 32 + 32;
  HintEdit.Left := 50;
  HintEdit.text := '';
  HintEdit.Width := GroupBox1.Width - 50 - 10;
  HintEdit.OnChange := @MarkAsChanged;
End;

Procedure TOneSetRuleFrame.SetCaption(Value: String);
Begin
  GroupBox1.Caption := value;
End;

Procedure TOneSetRuleFrame.RefreshTexts;
Var
  i, j, k: integer;
  c: TComboBox;
Begin
  For i := 0 To EinsteinHeight - 1 Do Begin
    // Todo : Umbaun wie utwosetruleframe
    SetLabels[i].Caption := ResolveXY(i, -2);
    c := SetComboboxes[i];
    k := c.ItemIndex;
    c.Items.Clear;
    c.items.add('-');
    For j := 0 To EinsteinWidth - 1 Do Begin
      c.items.add(ResolveXY(i, j));
    End;
    c.ItemIndex := max(0, k);
  End;
End;

Procedure TOneSetRuleFrame.SetSet(Index: integer; Set_: TMemberSet);
Var
  i: Integer;
Begin
  If index <> 0 Then Begin
    Raise Exception.create('Error invalid index');
  End;
  For i := 0 To high(Set_) Do Begin
    SetComboboxes[i].ItemIndex := Set_[i] + 1;
  End;
  fDataChanged := false;
End;

Function TOneSetRuleFrame.GetSet(Index: integer): TMemberSet;
Var
  i: Integer;
Begin
  setlength(result, EinsteinHeight);
  For i := 0 To EinsteinHeight - 1 Do Begin
    result[i] := SetComboboxes[i].ItemIndex - 1;
  End;
End;

Procedure TOneSetRuleFrame.SaveToFile(Const Ini: TInifile; Index: integer);
Var
  Section: String;
  i: Integer;
Begin
  Inherited;
  Section := 'Rule' + inttostr(Index);
  ini.WriteString(Section, 'Name', RuleSelectorToString(Rule));
  For i := 0 To EinsteinHeight - 1 Do Begin
    ini.WriteInteger(Section, 'Set0_Element' + inttostr(i), SetComboboxes[i].ItemIndex - 1);
  End;
  ini.WriteString(Section, 'Hint', HintEdit.Text);
End;

Procedure TOneSetRuleFrame.LoadFromFile(Const Ini: TInifile; Index: integer);
Var
  Section: String;
  i: Integer;
  s: String;
Begin
  Section := 'Rule' + inttostr(Index);
  s := ini.ReadString(Section, 'Name', '?');
  Rule := StringToRuleSelector(s);
  For i := 0 To EinsteinHeight - 1 Do Begin
    SetComboboxes[i].ItemIndex := ini.ReadInteger(Section, 'Set0_Element' + inttostr(i), -1) + 1;
  End;
  HintEdit.Text := ini.ReadString(Section, 'Hint', '');
  Inherited;
End;

Function TOneSetRuleFrame.ValidRule: Boolean;
Var
  i: Integer;
Begin
  result := false;
  // 1. Prüfen ob mindestens 1 Element aus der Grundmenge gewählt wurde
  For i := 0 To high(SetComboboxes) Do Begin
    result := result Or (SetComboboxes[i].ItemIndex <> 0);
  End;
End;

Function TOneSetRuleFrame.DeriveRule: TRule;
Begin
  result := Nil;
  Case Rule Of
    rsDependsOn: result := TDependsOn.Create(GetSet(0));
  Else Begin
      Raise exception.Create('Fehler nicht implementierte Regel : TOneSetRuleFrame.DeriveRule.' + RuleSelectorToString(rule));
    End;
  End;
  result.Hint := HintEdit.Text;
End;

Procedure TOneSetRuleFrame.SetAttribute(Index: integer; Value: integer);
Begin
  Raise exception.create('Ungültiger Aufruf, diese Klasse unterstützt keine Attribute.');
End;

Procedure TOneSetRuleFrame.SetAttributeCountAndLabels(
  Const Labels: Array Of String);
Begin
  Raise exception.create('Ungültiger Aufruf, diese Klasse unterstützt keine Attribute.');
End;

Procedure TOneSetRuleFrame.SetUserHint(Data: String);
Begin
  HintEdit.Text := data;
  fDataChanged := false;
End;

End.

