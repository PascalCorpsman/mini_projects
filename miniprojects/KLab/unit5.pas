(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of KLab                                                  *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit5;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

Type

  { TForm5 }

  TForm5 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    GroupBox4: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Procedure Button2Click(Sender: TObject);
    Procedure CheckBox5Click(Sender: TObject);
    Procedure CheckBox6Click(Sender: TObject);
    Procedure CheckBox7Click(Sender: TObject);
    Procedure CheckBox8Click(Sender: TObject);
    Procedure CheckBox9Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Function Replace_text(Data: String; OneTime: Boolean; Out ltchange: integer): String;
    Procedure DoReplace(Onetime: Boolean; Var P: Tpoint; Var TL: Integer);
  End;

Var
  Form5: TForm5;

Implementation

{$R *.lfm}

Uses uklab, unit1, math;

{ TForm5 }

Function CursorPosToIndex(Const Text: TStrings; Pos: TPoint): integer;
Const
{$IFDEF WINDOWS}
  crtlen = 2;
{$ELSE}
  crtlen = 1;
{$ENDIF}
Var
  i, res: Integer;
Begin
  res := min(length(text[pos.y - 1]), pos.x);
  For i := 0 To pos.y - 2 Do
    res := res + length(text[i]) + crtlen;
  CursorPosToIndex := res;
End;

Function killEndSpace(Const Data: String): integer;
Begin
  result := length(data);
  While ((result >= 1) And (data[result] = ' ')) Do
    result := result - 1;
End;

Function ReversePos(Substr: String; S: String): Integer;
Var
  i, j: Integer;
  b: Boolean;
Begin
  result := 0;
  For i := length(s) - Length(Substr) Downto 1 Do Begin
    b := true;
    For j := 1 To Length(substr) Do
      If s[i + j] <> substr[j] Then Begin
        b := false;
        break;
      End;
    If b Then Begin
      result := i + 1;
      exit;
    End;
  End;
End;

Function TForm5.Replace_text(Data: String; OneTime: Boolean; Out ltchange: integer): String;
Var
  Searchtext, Replacetext: String;
  b: Boolean;
Begin
  result := '';
  Searchtext := ComboBox1.text;
  Replacetext := ComboBox2.text;
  If Not CheckBox1.checked Then Begin // Wenn nicht Case Sensitiv
    Searchtext := lowercase(Searchtext);
    Replacetext := Replacetext;
    If CheckBox6.checked Then Begin // Rückwärts Ersetzen
      b := True;
      While b Do Begin
        b := false;
        If ReversePos(Searchtext, lowercase(data)) <> 0 Then Begin
          b := True;
          result := Replacetext + copy(data, ReversePos(Searchtext, lowercase(data)) + length(Searchtext), length(data));
          delete(data, ReversePos(Searchtext, lowercase(data)), length(data));
        End;
        If onetime Then b := false; // Wenn nur ein Ergebniss gesetzt werden soll.
      End;
      ltchange := length(data);
      result := data + result;
    End
    Else Begin // Vorwärts Ersetzen
      b := True;
      While b Do Begin
        b := false;
        If pos(Searchtext, lowercase(data)) <> 0 Then Begin
          b := True;
          result := result + copy(data, 1, pos(Searchtext, lowercase(data)) - 1) + Replacetext;
          delete(data, 1, pos(Searchtext, lowercase(data)) - 1 + length(Searchtext));
        End;
        If onetime Then b := false; // Wenn nur ein Ergebniss gesetzt werden soll.
      End;
      ltchange := length(result);
      result := result + Data;
    End;
  End
  Else Begin
    If CheckBox6.checked Then Begin // Rückwärts Ersetzen
      b := True;
      While b Do Begin
        b := false;
        If ReversePos(Searchtext, {lowercase}(data)) <> 0 Then Begin
          b := True;
          result := Replacetext + copy(data, ReversePos(Searchtext, lowercase(data)) + length(Searchtext), length(data));
          delete(data, ReversePos(Searchtext, lowercase(data)), length(data));
        End;
        If onetime Then b := false; // Wenn nur ein Ergebniss gesetzt werden soll.
      End;
      ltchange := length(data);
      result := data + result;
    End
    Else Begin // Vorwärts Ersetzen
      b := True;
      While b Do Begin
        b := false;
        If pos(Searchtext, {lowercase}(data)) <> 0 Then Begin
          b := True;
          result := result + copy(data, 1, pos(Searchtext, lowercase(data)) - 1) + Replacetext;
          delete(data, 1, pos(Searchtext, lowercase(data)) - 1 + length(Searchtext));
        End;
        If onetime Then b := false; // Wenn nur ein Ergebniss gesetzt werden soll.
      End;
      ltchange := length(result);
      result := result + Data;
    End;
  End;
End;

Procedure TForm5.DoReplace(Onetime: Boolean; Var P: Tpoint; Var TL: Integer);
Var
  sl, ltc: Integer;
  s, t, u: String;
Begin
  If checkbox8.checked Then Begin // Selektierter Text -- Suchrichtung EGAL
    (*
    Das die Richtung egal ist, da sind wir uns noch nicht so sicher ...
    *)
    If length(AktualSynedit.SelText) <> 0 Then Begin
      sl := killEndSpace(AktualSynedit.SelText); // Muss so gemacht werden, da die Selektierung auch "Länger" als eine Zeile sein kann
      u := copy(AktualSynedit.Text, 1, AktualSynedit.SelStart - 1);
      s := Replace_text(AktualSynedit.SelText, Onetime, ltc);
      t := copy(AktualSynedit.Text, AktualSynedit.SelStart + sl, length(AktualSynedit.Text));
      AktualSynedit.Text := u + copy(s, 1, ltc);
      p.Y := AktualSynedit.Lines.count;
      p.x := length(AktualSynedit.Lines[AktualSynedit.Lines.count - 1]) + 1;
      AktualSynedit.Text := u + s + t;
    End
    Else Begin
      Application.MessageBox('nothing selected.', 'Error', 0);
      exit;
    End;
  End;
  If checkbox7.checked Then Begin // Ganzer Text
    If CheckBox10.Checked Then Begin // Von Anfang an
      s := Replace_text(AktualSynedit.Text, Onetime, ltc);
      AktualSynedit.Text := copy(s, 1, ltc);
      p.Y := AktualSynedit.Lines.count;
      p.x := length(AktualSynedit.Lines[AktualSynedit.Lines.count - 1]) + 1;
      AktualSynedit.Text := s;
    End
    Else Begin // Vom Cursor an
      If CheckBox6.Checked Then Begin // Rückwärts Suche
        s := Replace_text(copy(AktualSynedit.text, 1, CursorPosToIndex(AktualSynedit.Lines, AktualSynedit.CaretXY) - 1), Onetime, ltc);
        t := copy(AktualSynedit.Text, CursorPosToIndex(AktualSynedit.Lines, AktualSynedit.CaretXY), length(AktualSynedit.Text));
        AktualSynedit.Text := copy(s, 1, ltc);
        p.Y := AktualSynedit.Lines.count;
        p.x := length(AktualSynedit.Lines[AktualSynedit.Lines.count - 1]) + 1;
        AktualSynedit.Text := s + t;
      End
      Else Begin // Vorwärts Suche
        s := Replace_text(copy(AktualSynedit.Text, CursorPosToIndex(AktualSynedit.Lines, AktualSynedit.CaretXY), length(AktualSynedit.Text)), Onetime, ltc);
        t := copy(AktualSynedit.Text, 1, CursorPosToIndex(AktualSynedit.Lines, AktualSynedit.CaretXY) - 1) + copy(s, 1, ltc);
        AktualSynedit.Text := t;
        p.Y := AktualSynedit.Lines.count;
        p.x := length(AktualSynedit.Lines[AktualSynedit.Lines.count - 1]) + 1;
        AktualSynedit.Text := t + copy(s, ltc + 1, length(s));
        If ltc = 0 Then Begin
          Application.MessageBox('Nothing found to replace with.', 'Info', 0);
        End;
      End;
    End;
  End;
  tl := tl; // Verhindert die Warnung, evtl müssen wir auch noch an TopLine Ran ..
End;


Procedure TForm5.FormCreate(Sender: TObject);
Begin
  Tform(self).Constraints.MaxHeight := Tform(self).Height;
  Tform(self).Constraints.MinHeight := Tform(self).Height;
  Tform(self).Constraints.Maxwidth := Tform(self).width;
  Tform(self).Constraints.Minwidth := Tform(self).width;
  combobox1.text := '';
  combobox2.text := '';
  form5.CheckBox9.Checked := True; // Von Cursor Pos
  form5.CheckBox10.Checked := false; // Vom Beginn an
  form5.CheckBox5.Checked := true; // Richtung Vorwärts
  form5.CheckBox6.Checked := false; // Richtung Rückwärts
End;

Procedure TForm5.CheckBox9Click(Sender: TObject);
Begin
  If CheckBox10.Checked Then CheckBox10.Checked := false;
End;

Procedure TForm5.CheckBox8Click(Sender: TObject);
Begin
  If CheckBox8.checked Then Begin
    checkbox7.checked := false;
    GroupBox4.Enabled := false;
  End;
End;

Procedure TForm5.CheckBox7Click(Sender: TObject);
Begin
  If CheckBox7.checked Then Begin
    checkbox8.checked := false;
    GroupBox4.Enabled := true;
  End;
End;

Procedure TForm5.CheckBox6Click(Sender: TObject);
Begin
  If checkbox6.checked Then checkbox5.checked := false;
End;

Procedure TForm5.CheckBox5Click(Sender: TObject);
Begin
  If checkbox5.checked Then checkbox6.checked := false;
End;

Procedure TForm5.Button2Click(Sender: TObject);
Var
  b: Boolean;
  tl, i: Integer;
  p: Tpoint;
Begin
  If (length(ComboBox1.text) = 0) Then exit;
  // Übernehmen der Begriffe
  b := True;
  For i := 0 To ComboBox1.items.count - 1 Do
    If lowercase(ComboBox1.text) = lowercase(ComboBox1.items[i]) Then Begin
      b := false;
      break;
    End;
  If b Then
    ComboBox1.Items.add(ComboBox1.text);
  If length(ComboBox2.text) <> 0 Then Begin
    b := True;
    For i := 0 To ComboBox2.items.count - 1 Do
      If lowercase(ComboBox2.text) = lowercase(ComboBox2.items[i]) Then Begin
        b := false;
        break;
      End;
    If b Then
      ComboBox2.Items.add(ComboBox2.text);
  End;
  // Bakup der Daten
  tl := AktualSynedit.TopLine;
  p := AktualSynedit.CaretXY;
  // Das Eigentliche Ersetzten
  DoReplace(false, p, tl);
  // setzen der Daten
  If (tl + AktualSynedit.LinesInWindow >= p.y) And (tl <= p.y) Then Begin
    AktualSynedit.TopLine := tl;
  End
  Else Begin
    AktualSynedit.TopLine := max(0, p.y - AktualSynedit.LinesInWindow + 1);
  End;
  AktualSynedit.CaretXY := p;
  form1.SynEdit1Change(Nil);
  close;
End;

End.

