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
Unit utwosetruleframe;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ueinstein, IniFiles;

Type

  { TTwoSetRuleFrame }

  TTwoSetRuleFrame = Class(TEinsteinRuleFrame)
    Button1: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Procedure Button1Click(Sender: TObject);
  private
    { private declarations }
    SetLabels: Array Of TLabel;
    SetComboboxes: Array Of TComboBox;
    Set2Comboboxes: Array Of TComboBox;
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

{$R *.lfm}

{ TTwoSetRuleFrame }

Procedure TTwoSetRuleFrame.Button1Click(Sender: TObject);
Begin
  // Del (damit ist die eigene Regel gemeint)
  SelfDeleteCallback(self);
End;

Constructor TTwoSetRuleFrame.Create(TheOwner: TComponent);
Var
  i, j: Integer;
Begin
  Inherited Create(TheOwner);
  setlength(SetLabels, EinsteinHeight);
  setlength(SetComboboxes, EinsteinHeight);
  setlength(Set2Comboboxes, EinsteinHeight);
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

    Set2Comboboxes[i] := TComboBox.Create(GroupBox1);
    Set2Comboboxes[i].Parent := GroupBox1;
    Set2Comboboxes[i].Name := 'Set2Combobox' + IntToStr(i);
    Set2Comboboxes[i].Text := '';
    Set2Comboboxes[i].Items.Clear;
    Set2Comboboxes[i].Top := 96;
    Set2Comboboxes[i].Left := 8 + 112 * i;
    Set2Comboboxes[i].Width := 100;
    Set2Comboboxes[i].ReadOnly := true;
    Set2Comboboxes[i].OnChange := @MarkAsChanged;
    SetComboboxes[i].items.add('-');
    Set2Comboboxes[i].items.add('-');
    For j := 0 To EinsteinWidth - 1 Do Begin
      SetComboboxes[i].items.add('');
      Set2Comboboxes[i].items.add('');
    End;
    SetComboboxes[i].ItemIndex := 0;
    Set2Comboboxes[i].ItemIndex := 0;
  End;
  GroupBox1.Width := 568 + (EinsteinHeight - 5) * 112;
  Button1.left := 584 + (EinsteinHeight - 5) * 112;
  self.width := 642 + (EinsteinHeight - 5) * 112;

  HintLabel := TLabel.Create(GroupBox1);
  HintLabel.Parent := GroupBox1;
  HintLabel.Left := 8;
  HintLabel.Top := 96 + 32;
  HintLabel.caption := 'Hint : ';
  HintLabel.Name := 'HintLabel';

  HintEdit := TEdit.Create(GroupBox1);
  HintEdit.Parent := GroupBox1;
  HintEdit.Name := 'HintEdit';
  HintEdit.Top := 96 + 32;
  HintEdit.Left := 50;
  HintEdit.text := '';
  HintEdit.Width := GroupBox1.Width - 50 - 10;
  HintEdit.OnChange := @MarkAsChanged;
End;

Procedure TTwoSetRuleFrame.SetCaption(Value: String);
Begin
  GroupBox1.Caption := value;
End;

Procedure TTwoSetRuleFrame.RefreshTexts;
Var
  i, j: integer;
Begin
  For i := 0 To EinsteinHeight - 1 Do Begin
    SetLabels[i].Caption := ResolveXY(i, -2);
    For j := 0 To EinsteinWidth - 1 Do Begin
      SetComboboxes[i].Items[j + 1] := ResolveXY(i, j);
      Set2Comboboxes[i].Items[j + 1] := ResolveXY(i, j);
    End;
  End;
  label1.Caption := RuleSelectorToString(Rule);
End;

Procedure TTwoSetRuleFrame.SetSet(Index: integer; Set_: TMemberSet);
Var
  i: Integer;
Begin
  If index = 0 Then Begin
    For i := 0 To high(Set_) Do Begin
      SetComboboxes[i].ItemIndex := Set_[i] + 1;
    End;
    fDataChanged := false;
    exit;
  End;
  If index = 1 Then Begin
    For i := 0 To high(Set_) Do Begin
      Set2Comboboxes[i].ItemIndex := Set_[i] + 1;
    End;
    fDataChanged := false;
    exit;
  End;
  Raise Exception.create('Error invalid index');
End;

Function TTwoSetRuleFrame.GetSet(Index: integer): TMemberSet;
Var
  i: Integer;
Begin
  result := Nil; // Eine AV Provozieren wenn Index nicht stimmt
  If index = 0 Then Begin
    setlength(result, EinsteinHeight);
    For i := 0 To EinsteinHeight - 1 Do Begin
      result[i] := SetComboboxes[i].ItemIndex - 1;
    End;
  End;
  If index = 1 Then Begin
    setlength(result, EinsteinHeight);
    For i := 0 To EinsteinHeight - 1 Do Begin
      result[i] := Set2Comboboxes[i].ItemIndex - 1;
    End;
  End;
End;

Procedure TTwoSetRuleFrame.SaveToFile(Const Ini: TInifile; Index: integer);
Var
  Section: String;
  i: Integer;
Begin
  Inherited;
  Section := 'Rule' + inttostr(Index);
  ini.WriteString(Section, 'Name', RuleSelectorToString(Rule));
  For i := 0 To EinsteinHeight - 1 Do Begin
    ini.WriteInteger(Section, 'Set0_Element' + inttostr(i), SetComboboxes[i].ItemIndex - 1);
    ini.WriteInteger(Section, 'Set1_Element' + inttostr(i), Set2Comboboxes[i].ItemIndex - 1);
  End;
  ini.WriteString(Section, 'Hint', HintEdit.Text);
End;

Procedure TTwoSetRuleFrame.LoadFromFile(Const Ini: TInifile; Index: integer);
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
    Set2Comboboxes[i].ItemIndex := ini.ReadInteger(Section, 'Set1_Element' + inttostr(i), -1) + 1;
  End;
  HintEdit.Text := ini.ReadString(Section, 'Hint', '');
  Inherited;
End;

Function TTwoSetRuleFrame.ValidRule: Boolean;
Var
  i: Integer;
  res2: Boolean;
Begin
  result := false;
  // 1. Prüfen ob mindestens 1 Element aus der Grundmenge gewählt wurde
  For i := 0 To high(SetComboboxes) Do Begin
    result := result Or (SetComboboxes[i].ItemIndex <> 0);
  End;
  res2 := false;
  // 1. Prüfen ob mindestens 1 Element aus der Grundmenge gewählt wurde
  For i := 0 To high(Set2Comboboxes) Do Begin
    res2 := res2 Or (Set2Comboboxes[i].ItemIndex <> 0);
  End;
  result := result And res2;
End;

Function TTwoSetRuleFrame.DeriveRule: TRule;
Begin
  result := Nil;
  Case Rule Of
    rsEliminate: result := TEliminate.Create(GetSet(0), GetSet(1));
    rsLeftOf: result := TLeftOf.Create(GetSet(0), GetSet(1));
    rsRightOf: result := TRightof.Create(GetSet(0), GetSet(1));
    //rsDirectLeftOf: result := TDirectLeftOf.Create(GetSet(0), GetSet(1));
    //rsDirectRightOf: result := TDirectRightof.Create(GetSet(0), GetSet(1));
    //rsNeighbourOf: result := TNeighbourOf.Create(GetSet(0), GetSet(1));
    //rsNotNeighbourOf: result := TNotNeighbourOf.Create(GetSet(0), GetSet(1));
  Else Begin
      Raise exception.Create('Fehler nicht implementierte Regel : TTwoSetRuleFrame.DeriveRule.' + RuleSelectorToString(rule));
    End;
  End;
  result.Hint := HintEdit.Text;
End;

Procedure TTwoSetRuleFrame.SetAttribute(Index: integer; Value: integer);
Begin
  Raise exception.create('Ungültiger Aufruf, diese Klasse unterstützt keine Attribute.');
End;

Procedure TTwoSetRuleFrame.SetAttributeCountAndLabels(
  Const Labels: Array Of String);
Begin
  Raise exception.create('Ungültiger Aufruf, diese Klasse unterstützt keine Attribute.');
End;

Procedure TTwoSetRuleFrame.SetUserHint(Data: String);
Begin
  HintEdit.Text := data;
  fDataChanged := false;
End;

End.

