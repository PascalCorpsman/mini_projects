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
Unit Unit6;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, lcltype;

Type

  { TForm6 }

  TForm6 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    Edit1: TEdit;
    Label1: TLabel;
    RadioGroup1: TRadioGroup;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Edit1KeyPress(Sender: TObject; Var Key: char);
    Procedure Edit1KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  End;

Var
  Form6: TForm6;

Implementation

{$R *.lfm}

Uses uklab, math;

(*
Geht davon aus das Offset, im Bereich
1 .. Length(String) liegt

Leifert das 1. Vorkommen von Substr, nach Offset
*)

Function PosEx(Substr, Source: String; Offset: Integer): integer;
Var
  tmp: String;
  t: Integer;
Begin
  result := 0;
  tmp := copy(source, offset, length(source));
  t := pos(Substr, tmp);
  If t <> 0 Then
    result := t + Offset - 1;
End;

Function posrev(Substr: String; S: String): Integer;
Var
  i, j: Integer;
  b: Boolean;
Begin
  result := 0;
  i := length(s) - length(Substr) + 1;
  While i > 0 Do Begin
    b := True;
    For j := 1 To Length(Substr) Do
      If s[i - 1 + j] <> Substr[j] Then Begin
        b := False;
        break;
      End;
    If b Then Begin
      result := i;
      exit;
    End;
    dec(i);
  End;
End;

(*
Gleich wie PosEx nur Rückwärts suchen
*)

Function PosRevEx(Substr, Source: String; Offset: Integer): integer;
Var
  tmp: String;
  t: Integer;
Begin
  //  result := 0;
  tmp := copy(source, 1, Offset);
  t := posRev(Substr, tmp);
  //  If t <> 0 Then
  result := t;
End;

{ TForm6 }

Procedure TForm6.FormCreate(Sender: TObject);
Begin
  Tform(self).Constraints.MaxHeight := Tform(self).Height;
  Tform(self).Constraints.MinHeight := Tform(self).Height;
  Tform(self).Constraints.Maxwidth := Tform(self).width;
  Tform(self).Constraints.Minwidth := Tform(self).width;
  edit1.text := '';
  caption := 'Find..';
End;

Procedure TForm6.Button1Click(Sender: TObject);
Var
  t, x, y: Integer;
  suchtext, s: String;
  weiter: Boolean;
Begin
  // Die Aktuelle Zeile und Spalte Auslesen.
  x := AktualSynedit.CaretX;
  y := AktualSynedit.Carety - 1;
  If CheckBox1.checked Then Begin
    (*
    Das ist So eigentlich nicht ganz richtig, da ja ein Wort auch am Anfang,
    oder am Ende einer Zeile Stehen kann.
    Aber das Ganze wird nachher "Weggehackt"
    *)
    suchtext := ' ' + Edit1.text + ' ';
  End
  Else Begin
    suchtext := Edit1.text;
  End;
  weiter := true;
  Case RadioGroup1.ItemIndex Of
    // Rückwärts suchen
    0: Begin
        // Die Aktuelle Zeile Auslesen
        If CheckBox1.checked Then
          s := ' ' + AktualSynedit.Lines[y] + ' '
        Else
          s := AktualSynedit.Lines[y];
        // Das ist eigentlich falsch, aber nur so können wir mehrfach suchanfragen handeln.
        x := max(1, x - length(suchtext));
        While weiter Do Begin
          // Case sensitives Suchen
          If CheckBox2.checked Then
            t := PosRevEx(suchtext, s, x)
          Else
            t := PosRevEx(lowercase(suchtext), lowercase(s), x);
          // Wenn was gefunden wird
          If t <> 0 Then Begin
            If CheckBox1.checked Then
              AktualSynedit.CaretX := t - 1
            Else
              AktualSynedit.CaretX := t;
            AktualSynedit.Carety := y + 1;
            AktualSynedit.SelectWord;
            weiter := false;
          End;
          dec(y);
          If (y < 0) And Weiter Then Begin
            If id_NO = Application.MessageBox('Reached begin of text, wrap ?', 'Question', MB_YESNO Or MB_ICONQUESTION) Then
              weiter := false;
            y := AktualSynedit.lines.count - 1;
          End;
          // Die Aktuelle Zeile Auslesen
          If CheckBox1.checked Then
            s := ' ' + AktualSynedit.Lines[y] + ' '
          Else
            s := AktualSynedit.Lines[y];
          x := length(s);
        End;
      End;
    // Vorwärts Suchen
    1: Begin
        While weiter Do Begin
          // Die Aktuelle Zeile Auslesen
          If CheckBox1.checked Then
            s := ' ' + AktualSynedit.Lines[y] + ' '
          Else
            s := AktualSynedit.Lines[y];
          // Case sensitives Suchen
          If CheckBox2.checked Then
            t := posEx(suchtext, s, x)
          Else
            t := posEx(lowercase(suchtext), lowercase(s), x);
          // Wenn was gefunden wird
          If t <> 0 Then Begin
            If CheckBox1.checked Then
              AktualSynedit.CaretX := t - 1
            Else
              AktualSynedit.CaretX := t;
            AktualSynedit.Carety := y + 1;
            AktualSynedit.SelectWord;
            weiter := false;
          End;
          x := 1;
          inc(y);
          If (y >= AktualSynedit.lines.count) And Weiter Then Begin
            If id_NO = Application.MessageBox('Reached end of text, wrap ?', 'Question', MB_YESNO Or MB_ICONQUESTION) Then
              weiter := false;
            y := 0;
          End;
        End;
      End;
  End;
End;

Procedure TForm6.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm6.Edit1KeyPress(Sender: TObject; Var Key: char);
Begin
  If (key = #13) And button1.visible Then Begin
    button1.onclick(Nil);
  End;

End;

Procedure TForm6.Edit1KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);
Begin
  button1.enabled := length(Edit1.text) <> 0;

End;

End.

