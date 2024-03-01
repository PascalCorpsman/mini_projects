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
Unit umultisetframe;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ueinstein, IniFiles;

Type

  { TMultiSetFrame }

  TMultiSetFrame = Class(TEinsteinRuleFrame)
    Button1: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Procedure Button1Click(Sender: TObject);
  private
    { private declarations }
    SetLabels: Array Of TLabel;
    Set1Checkboxes: Array Of Array Of TCheckBox;
    Set2Checkboxes: Array Of Array Of TCheckBox;
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

{ TMultiSetFrame }

Procedure TMultiSetFrame.Button1Click(Sender: TObject);
Begin
  // Del (damit ist die eigene Regel gemeint)
  SelfDeleteCallback(self);
End;

Constructor TMultiSetFrame.Create(TheOwner: TComponent);
Var
  i, j: Integer;
Begin
  Inherited Create(TheOwner);
  setlength(SetLabels, EinsteinHeight);
  setlength(Set1Checkboxes, EinsteinHeight, EinsteinWidth);
  setlength(Set2Checkboxes, EinsteinHeight, EinsteinWidth);
  For i := 0 To EinsteinHeight - 1 Do Begin
    SetLabels[i] := TLabel.Create(GroupBox1);
    SetLabels[i].Parent := GroupBox1;
    SetLabels[i].Name := 'SetLabel' + IntToStr(i);
    SetLabels[i].Caption := 'Label' + inttostr(i + 1);
    SetLabels[i].Top := 8;
    SetLabels[i].Left := 8 + 112 * i;

    For j := 0 To EinsteinWidth - 1 Do Begin
      Set1Checkboxes[i, j] := TCheckBox.Create(GroupBox1);
      Set1Checkboxes[i, j].parent := GroupBox1;
      Set1Checkboxes[i, j].name := 'Set1CheckBox' + inttostr(i) + '_' + inttostr(j);
      Set1Checkboxes[i, j].top := 32 + j * 25;
      Set1Checkboxes[i, j].left := 8 + 112 * i;
      Set1Checkboxes[i, j].onChange := @MarkAsChanged;

      Set2Checkboxes[i, j] := TCheckBox.Create(GroupBox1);
      Set2Checkboxes[i, j].parent := GroupBox1;
      Set2Checkboxes[i, j].name := 'Set2CheckBox' + inttostr(i) + '_' + inttostr(j);
      Set2Checkboxes[i, j].top := 32 + j * 25 + 32 + EinsteinWidth * 25;
      Set2Checkboxes[i, j].left := 8 + 112 * i;
      Set2Checkboxes[i, j].onChange := @MarkAsChanged;
    End;
  End;
  label1.Top := 32 + EinsteinWidth * 25;
  GroupBox1.Width := 568 + (EinsteinHeight - 5) * 112;
  Button1.left := 584 + (EinsteinHeight - 5) * 112;
  self.width := 642 + (EinsteinHeight - 5) * 112;

  HintLabel := TLabel.Create(GroupBox1);
  HintLabel.Parent := GroupBox1;
  HintLabel.Left := 8;
  HintLabel.Top := 32 + EinsteinWidth * 25 * 2 + 32;
  HintLabel.caption := 'Hint : ';
  HintLabel.Name := 'HintLabel';

  HintEdit := TEdit.Create(GroupBox1);
  HintEdit.Parent := GroupBox1;
  HintEdit.Name := 'HintEdit';
  HintEdit.Top := 32 + EinsteinWidth * 25 * 2 + 32;
  HintEdit.Left := 50;
  HintEdit.text := '';
  HintEdit.Width := GroupBox1.Width - 50 - 10;
  HintEdit.OnChange := @MarkAsChanged;

  self.Height := 32 + EinsteinWidth * 25 * 2 + 96;
  GroupBox1.Height := self.Height - 10;
  button1.Height := self.Height - 10;
End;

Procedure TMultiSetFrame.SetCaption(Value: String);
Begin
  GroupBox1.Caption := value;
End;

Procedure TMultiSetFrame.RefreshTexts;
Var
  i, j: integer;
Begin
  For i := 0 To EinsteinHeight - 1 Do Begin
    SetLabels[i].Caption := ResolveXY(i, -2);
    For j := 0 To EinsteinWidth - 1 Do Begin
      Set1Checkboxes[i, j].caption := ResolveXY(i, j);
      Set2Checkboxes[i, j].caption := ResolveXY(i, j);
    End;
  End;
  label1.Caption := RuleSelectorToString(Rule);
End;

Procedure TMultiSetFrame.SetSet(Index: integer; Set_: TMemberSet);
Begin
  Raise exception.create('TMultiSetFrame.SetSet : Braucht keine Sau.');
End;

Function TMultiSetFrame.GetSet(Index: integer): TMemberSet;
Var
  i: Integer;
Begin
  setlength(result, EinsteinWidth);
  For i := 0 To high(result) Do Begin
    result[i] := Nichts;
  End;
  If index >= EinsteinHeight Then Begin
    index := index - EinsteinHeight;
    For i := 0 To EinsteinWidth - 1 Do Begin
      If Set2Checkboxes[index, i].checked Then Begin
        result[i] := i;
      End;
    End;
  End
  Else Begin
    For i := 0 To EinsteinWidth - 1 Do Begin
      If Set1Checkboxes[index, i].checked Then Begin
        result[i] := i;
      End;
    End;
  End;
End;

Procedure TMultiSetFrame.SaveToFile(Const Ini: TInifile; Index: integer);
Var
  Section: String;
  i, j: Integer;
Begin
  Inherited;
  Section := 'Rule' + inttostr(Index);
  ini.WriteString(Section, 'Name', RuleSelectorToString(Rule));
  For i := 0 To EinsteinHeight - 1 Do Begin
    For j := 0 To EinsteinWidth - 1 Do Begin
      ini.WriteBool(Section, format('Set0_%d_%d', [i, j]), Set1Checkboxes[i, j].checked);
      ini.WriteBool(Section, format('Set1_%d_%d', [i, j]), Set2Checkboxes[i, j].checked);
    End;
  End;
  ini.WriteString(Section, 'Hint', HintEdit.Text);
End;

Procedure TMultiSetFrame.LoadFromFile(Const Ini: TInifile; Index: integer);
Var
  Section: String;
  i, j: Integer;
Begin
  Section := 'Rule' + inttostr(Index);
  ini.WriteString(Section, 'Name', RuleSelectorToString(Rule));
  For i := 0 To EinsteinHeight - 1 Do Begin
    For j := 0 To EinsteinWidth - 1 Do Begin
      Set1Checkboxes[i, j].checked := ini.ReadBool(Section, format('Set0_%d_%d', [i, j]), false);
      Set2Checkboxes[i, j].checked := ini.ReadBool(Section, format('Set1_%d_%d', [i, j]), false);
    End;
  End;
  HintEdit.Text := ini.ReadString(Section, 'Hint', '');
  Inherited LoadFromFile(Ini, Index);
End;

Function TMultiSetFrame.ValidRule: Boolean;
Begin
  result := true;
End;

Function TMultiSetFrame.DeriveRule: TRule;
Var
  i: Integer;
Begin
  result := Nil;
  Case Rule Of
    rsMultiEliminate: result := TMultiEliminate.Create();
  Else Begin
      Raise exception.Create('Fehler nicht implementierte Regel : TTwoSetRuleFrame.DeriveRule.' + RuleSelectorToString(rule));
    End;
  End;
  For i := 0 To EinsteinHeight - 1 Do Begin
    TMultiRule(result).SetSet(false, i, GetSet(i));
    TMultiRule(result).SetSet(True, i, GetSet(i + EinsteinHeight));
  End;
  result.Hint := HintEdit.Text;
End;

Procedure TMultiSetFrame.SetAttribute(Index: integer; Value: integer);
Begin
  Raise exception.create('Ungültiger Aufruf, diese Klasse unterstützt keine Attribute.');

End;

Procedure TMultiSetFrame.SetAttributeCountAndLabels(
  Const Labels: Array Of String);
Begin
  Raise exception.create('Ungültiger Aufruf, diese Klasse unterstützt keine Attribute.');
End;

Procedure TMultiSetFrame.SetUserHint(Data: String);
Begin
  HintEdit.Text := data;
  fDataChanged := false;
End;

End.

