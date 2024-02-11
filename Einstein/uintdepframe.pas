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
Unit uintdepframe;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, StdCtrls, ueinstein, IniFiles;

Type

  { TIntDepSetFrame }

  TIntDepSetFrame = Class(TEinsteinRuleFrame)
    Button1: TButton;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    ComboBox3: TComboBox;
    Edit1: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Procedure Button1Click(Sender: TObject);
    Procedure ComboBox2Change(Sender: TObject);
  private
    { private declarations }
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

{ TIntDepSetFrame }

Procedure TIntDepSetFrame.Button1Click(Sender: TObject);
Begin
  // Del (damit ist die eigene Regel gemeint)
  SelfDeleteCallback(self);
End;

Procedure TIntDepSetFrame.ComboBox2Change(Sender: TObject);
Begin
  label2.caption := ComboBox2.Text;
  MarkAsChanged(self);
End;

Constructor TIntDepSetFrame.Create(TheOwner: TComponent);
Var
  i, j: Integer;
Begin
  Inherited Create(TheOwner);
  For i := 0 To EinsteinHeight - 1 Do Begin
    ComboBox2.Items.add('');
    For j := 0 To EinsteinWidth - 1 Do Begin
      ComboBox1.Items.add('');
      ComboBox3.Items.add('');
    End;
  End;
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

  ComboBox1.Text := '';
  ComboBox2.Text := '';
  ComboBox3.Text := '';
  Edit1.text := '';
  label2.caption := '';
End;

Procedure TIntDepSetFrame.SetCaption(Value: String);
Begin
  GroupBox1.Caption := value;
End;

Procedure TIntDepSetFrame.RefreshTexts;
Var
  i, j: integer;
  s: String;
Begin
  For i := 0 To EinsteinHeight - 1 Do Begin
    ComboBox2.Items[i] := (ResolveXY(i, -2));
    For j := 0 To EinsteinWidth - 1 Do Begin
      s := ResolveXY(i, j) + ' (' + ResolveXY(i, -2) + ')';
      //      s := ResolveXY(i, j); // -- Oder So ohne dass die Mengenlabels mit drin stehen ?
      ComboBox1.Items[i * EinsteinWidth + j] := s;
      ComboBox3.Items[i * EinsteinWidth + j] := s;
    End;
  End;
  label2.caption := ComboBox2.Text;
End;

Procedure TIntDepSetFrame.SetSet(Index: integer; Set_: TMemberSet);
Begin
  Raise exception.create('TIntDepSetFrame.SetSet : Braucht keine Sau.');
End;

Function TIntDepSetFrame.GetSet(Index: integer): TMemberSet;
Begin
  result := Nil;
  Raise exception.create('TIntDepSetFrame.SetSet : Braucht keine Sau.');
End;

Procedure TIntDepSetFrame.SaveToFile(Const Ini: TInifile; Index: integer);
Var
  Section: String;
Begin
  Inherited;
  Section := 'Rule' + inttostr(Index);
  ini.WriteString(Section, 'Name', RuleSelectorToString(Rule));

  ini.WriteInteger(section, 'Element1', ComboBox1.ItemIndex);
  ini.WriteInteger(section, 'Cathegory', ComboBox2.ItemIndex);
  ini.WriteInteger(section, 'Element2', ComboBox3.ItemIndex);
  ini.WriteString(section, 'difference', Edit1.Text);

  ini.WriteString(Section, 'Hint', HintEdit.Text);
End;

Procedure TIntDepSetFrame.LoadFromFile(Const Ini: TInifile; Index: integer);
Var
  Section: String;
  s: String;
Begin
  Section := 'Rule' + inttostr(Index);
  s := ini.ReadString(Section, 'Name', '?');
  Rule := StringToRuleSelector(s);

  ComboBox1.ItemIndex := ini.readInteger(section, 'Element1', -1);
  ComboBox2.ItemIndex := ini.readInteger(section, 'Cathegory', -1);
  ComboBox3.ItemIndex := ini.readInteger(section, 'Element2', -1);
  Edit1.Text := ini.ReadString(section, 'difference', '');

  HintEdit.Text := ini.ReadString(Section, 'Hint', '');
  ComboBox2Change(Nil);
  fDataChanged := false;
  Inherited LoadFromFile(Ini, Index);
End;

Function TIntDepSetFrame.ValidRule: Boolean;
Begin
  result := true;
End;

Function TIntDepSetFrame.DeriveRule: TRule;
Begin
  result := Nil;
  Case Rule Of
    rsIntDepends: result := TIntDependence.Create(ComboBox1.ItemIndex, ComboBox2.ItemIndex, ComboBox3.ItemIndex, strtointdef(edit1.text, 0));
  Else Begin
      Raise exception.Create('Fehler nicht implementierte Regel : TTwoSetRuleFrame.DeriveRule.' + RuleSelectorToString(rule));
    End;
  End;
  result.Hint := HintEdit.Text;
End;

Procedure TIntDepSetFrame.SetAttribute(Index: integer; Value: integer);
Begin
  Raise exception.create('Ungültiger Aufruf, diese Klasse unterstützt keine Attribute.');
End;

Procedure TIntDepSetFrame.SetAttributeCountAndLabels(
  Const Labels: Array Of String);
Begin
  Raise exception.create('Ungültiger Aufruf, diese Klasse unterstützt keine Attribute.');
End;

Procedure TIntDepSetFrame.SetUserHint(Data: String);
Begin
  HintEdit.Text := data;
  fDataChanged := false;
End;

End.

