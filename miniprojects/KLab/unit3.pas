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
Unit Unit3;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  lclintf;

Type

  { TForm3 }

  TForm3 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    CheckBox10: TCheckBox;
    CheckBox11: TCheckBox;
    CheckBox12: TCheckBox;
    CheckBox13: TCheckBox;
    CheckBox14: TCheckBox;
    CheckBox15: TCheckBox;
    CheckBox16: TCheckBox;
    CheckBox17: TCheckBox;
    CheckBox18: TCheckBox;
    CheckBox19: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox20: TCheckBox;
    CheckBox21: TCheckBox;
    CheckBox22: TCheckBox;
    CheckBox23: TCheckBox;
    CheckBox24: TCheckBox;
    CheckBox25: TCheckBox;
    CheckBox26: TCheckBox;
    CheckBox27: TCheckBox;
    CheckBox28: TCheckBox;
    CheckBox29: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox30: TCheckBox;
    CheckBox31: TCheckBox;
    CheckBox32: TCheckBox;
    CheckBox33: TCheckBox;
    CheckBox34: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label19: TLabel;
    Label2: TLabel;
    Label20: TLabel;
    Label21: TLabel;
    Label22: TLabel;
    Label23: TLabel;
    Label24: TLabel;
    Label25: TLabel;
    Label26: TLabel;
    Label27: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure CheckBox16Change(Sender: TObject);
    Procedure CheckBox24Change(Sender: TObject);
    Procedure CheckBox32Change(Sender: TObject);
    Procedure CheckBox8Change(Sender: TObject);
    Procedure ComboBox1Change(Sender: TObject);
    Procedure Edit1KeyPress(Sender: TObject; Var Key: char);
    Procedure Edit2KeyPress(Sender: TObject; Var Key: char);
    Procedure Edit3KeyPress(Sender: TObject; Var Key: char);
    Procedure Edit4KeyPress(Sender: TObject; Var Key: char);
    Procedure FormCreate(Sender: TObject);
    Procedure Label18Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    Procedure loadcpu(used_cpu: String);
  End;

Var
  Form3: TForm3;

Implementation

{$R *.lfm}

Uses uklab, LazFileUtils;

Var
  cp: TCPU;

Function HextoIntdef(Value: String; DefVaule: Cardinal): Cardinal;
Var
  m: cardinal;
  c: byte;
Begin
  value := uppercase(value);
  result := 0;
  m := 1;
  While length(value) <> 0 Do Begin
    c := ord(value[length(value)]);
    delete(value, length(value), 1);
{$HINTS OFF}
    If c In [48..57] Then Begin // alle normalen Zahlen
      result := result + (c - 48) * m;
    End
    Else Begin
      If c In [65..70] Then Begin // Alle relevanten Buchstaben
        result := result + (c - 55) * m;
      End
      Else Begin // Fehler
        result := DefVaule;
        exit;
      End;
    End;
{$HINTS ON}
    m := m * 16;
  End;
End;

{ TForm3 }

Procedure TForm3.FormCreate(Sender: TObject);
Begin
  Tform(self).Constraints.MaxHeight := Tform(self).Height;
  Tform(self).Constraints.MinHeight := Tform(self).Height;
  Tform(self).Constraints.Maxwidth := Tform(self).width;
  Tform(self).Constraints.Minwidth := Tform(self).width;
End;

Procedure TForm3.Label18Click(Sender: TObject);
Begin
  OpenURL(label18.caption);
End;

Procedure TForm3.loadcpu(used_cpu: String);
Var
  i: integer;
  cb: TCheckBox;
  b: byte;
Begin
  ComboBox1.Text := used_cpu;
  cp := uklab.loadcpu(used_cpu);
  b := cp.FuseHighEnabled;
  edit1.enabled := b <> 0;
  label20.enabled := b <> 0;
  label24.enabled := b <> 0;
  For i := 1 To 8 Do Begin
    cb := TCheckbox(FindComponent('Checkbox' + inttostr(i)));
    cb.Caption := cp.FuseHigh[i - 1];
    cb.Enabled := (b And 1) = 1;
    cb.Checked := false;
    b := b Div 2;
  End;
  b := cp.FuseLowEnabled;
  edit2.enabled := b <> 0;
  label21.enabled := b <> 0;
  label25.enabled := b <> 0;
  For i := 1 To 8 Do Begin
    cb := TCheckbox(FindComponent('Checkbox' + inttostr(i + 8)));
    cb.Caption := cp.FuseLow[i - 1];
    cb.Enabled := (b And 1) = 1;
    cb.Checked := false;
    b := b Div 2;
  End;
  b := cp.FuseExtendedEnabled;
  edit3.enabled := b <> 0;
  label22.enabled := b <> 0;
  label26.enabled := b <> 0;
  For i := 1 To 8 Do Begin
    cb := TCheckbox(FindComponent('Checkbox' + inttostr(i + 16)));
    cb.Caption := cp.FuseExtended[i - 1];
    cb.Enabled := (b And 1) = 1;
    cb.Checked := false;
    b := b Div 2;
  End;
  b := cp.FuseLockEnabled;
  edit4.enabled := b <> 0;
  label23.enabled := b <> 0;
  label27.enabled := b <> 0;
  For i := 1 To 8 Do Begin
    cb := TCheckbox(FindComponent('Checkbox' + inttostr(i + 24)));
    cb.Caption := cp.FuseLock[i - 1];
    cb.Enabled := (b And 1) = 1;
    cb.Checked := false;
    b := b Div 2;
  End;
  Edit1.text := 'FF';
  Edit2.text := 'FF';
  Edit3.text := 'FF';
  Edit4.text := 'FF';
End;

Procedure TForm3.Button1Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm3.Button2Click(Sender: TObject);
Var
  t, s: String;
  f: TFileStream;
  res: byte;
  i: integer;
  cb: TCheckBox;
Begin
  s := AktualProjectWorkPath;
  If s = '' Then
    s := AppPath;
  // High Fuse Byte
  If cp.FuseHighEnabled <> 0 Then Begin
    t := s + '.hfuse';
    If FileExistsUTF8(t) Then
      DeleteFileUTF8(t);
    ExecuteCommand(CreateReadFuseCommand(t, highfuse, cp));
    If FileExistsUTF8(t) Then Begin
      f := TFileStream.Create(t, fmopenread);
      res := 0;
      f.Read(res, sizeof(res));
      f.free;
      For i := 1 To 8 Do Begin
        cb := TCheckBox(FindComponent('Checkbox' + inttostr(i)));
        cb.Checked := Not ((res And 1) = 1);
        res := res Div 2;
      End;
    End;
  End;
  // Low Fuse Byte
  If cp.FuseLowEnabled <> 0 Then Begin
    t := s + '.lfuse';
    If FileExistsUTF8(t) Then
      DeleteFileUTF8(t);
    ExecuteCommand(CreateReadFuseCommand(t, lowfuse, cp));
    If FileExistsUTF8(t) Then Begin
      f := TFileStream.Create(t, fmopenread);
      res := 0;
      f.Read(res, sizeof(res));
      f.free;
      For i := 1 To 8 Do Begin
        cb := TCheckBox(FindComponent('Checkbox' + inttostr(i + 8)));
        cb.Checked := Not ((res And 1) = 1);
        res := res Div 2;
      End;
    End;
  End;
  // Extended Fuse Byte
  If cp.FuseExtendedEnabled <> 0 Then Begin
    t := s + '.efuse';
    If FileExistsUTF8(t) Then
      DeleteFileUTF8(t);
    ExecuteCommand(CreateReadFuseCommand(t, extendedfuse, cp));
    If FileExistsUTF8(t) Then Begin
      f := TFileStream.Create(t, fmopenread);
      res := 0;
      f.Read(res, sizeof(res));
      f.free;
      For i := 1 To 8 Do Begin
        cb := TCheckBox(FindComponent('Checkbox' + inttostr(i + 16)));
        cb.Checked := Not ((res And 1) = 1);
        res := res Div 2;
      End;
    End;
  End;
  // Lock Fuse Byte
  If cp.FuseLockEnabled <> 0 Then Begin
    t := s + '.lock';
    If FileExistsUTF8(t) Then
      DeleteFileUTF8(t);
    ExecuteCommand(CreateReadFuseCommand(t, lockfuse, cp));
    If FileExistsUTF8(t) Then Begin
      f := TFileStream.Create(t, fmopenread);
      res := 0;
      f.Read(res, sizeof(res));
      f.free;
      For i := 1 To 8 Do Begin
        cb := TCheckBox(FindComponent('Checkbox' + inttostr(i + 24)));
        cb.Checked := Not ((res And 1) = 1);
        res := res Div 2;
      End;
    End;
  End;
End;

Procedure TForm3.Button3Click(Sender: TObject);
Var
  resp, res: byte;
  i: integer;
  cb: TCheckBox;
Begin
  // High Fuse Byte
  If cp.FuseHighEnabled <> 0 Then Begin
    res := 0;
    resp := 1;
    For i := 1 To 8 Do Begin
      cb := TCheckBox(FindComponent('Checkbox' + inttostr(i)));
      If Not cb.Checked Then
        res := res Or resp;
      resp := resp * 2;
    End;
    ExecuteCommand(CreateWriteFuseCommand(res, highfuse, cp));
  End;
  // Low Fuse Byte
  If cp.FuseLowEnabled <> 0 Then Begin
    res := 0;
    resp := 1;
    For i := 1 To 8 Do Begin
      cb := TCheckBox(FindComponent('Checkbox' + inttostr(i + 8)));
      If Not cb.Checked Then
        res := res Or resp;
      resp := resp * 2;
    End;
    ExecuteCommand(CreateWriteFuseCommand(res, lowfuse, cp));
  End;
  // Extended Fuse Byte
  If cp.FuseExtendedEnabled <> 0 Then Begin
    res := 0;
    resp := 1;
    For i := 1 To 8 Do Begin
      cb := TCheckBox(FindComponent('Checkbox' + inttostr(i + 16)));
      If Not cb.Checked Then
        res := res Or resp;
      resp := resp * 2;
    End;
    ExecuteCommand(CreateWriteFuseCommand(res, extendedfuse, cp));
  End;
  // Lock Fuse Byte
  If cp.FuseLockEnabled <> 0 Then Begin
    res := 0;
    resp := 1;
    For i := 1 To 8 Do Begin
      cb := TCheckBox(FindComponent('Checkbox' + inttostr(i + 24)));
      If Not cb.Checked Then
        res := res Or resp;
      resp := resp * 2;
    End;
    ExecuteCommand(CreateWriteFuseCommand(res, lockfuse, cp));
  End;
End;

Procedure TForm3.CheckBox16Change(Sender: TObject);
Var
  resp, res: byte;
  i: integer;
  cb: TCheckBox;
Begin
  If cp.FuseLowEnabled <> 0 Then Begin
    res := 0;
    resp := 1;
    For i := 1 To 8 Do Begin
      cb := TCheckBox(FindComponent('Checkbox' + inttostr(i + 8)));
      If Not cb.Checked Then
        res := res Or resp;
      resp := resp * 2;
    End;
  End;
  edit2.text := format('%.02X', [res]);
End;

Procedure TForm3.CheckBox24Change(Sender: TObject);
Var
  resp, res: byte;
  i: integer;
  cb: TCheckBox;
Begin
  If cp.FuseExtendedEnabled <> 0 Then Begin
    res := 0;
    resp := 1;
    For i := 1 To 8 Do Begin
      cb := TCheckBox(FindComponent('Checkbox' + inttostr(i + 16)));
      If Not cb.Checked Then
        res := res Or resp;
      resp := resp * 2;
    End;
  End;
  edit3.text := format('%.02X', [res]);
End;

Procedure TForm3.CheckBox32Change(Sender: TObject);
Var
  resp, res: byte;
  i: integer;
  cb: TCheckBox;
Begin
  If cp.FuseLockEnabled <> 0 Then Begin
    res := 0;
    resp := 1;
    For i := 1 To 8 Do Begin
      cb := TCheckBox(FindComponent('Checkbox' + inttostr(i + 24)));
      If Not cb.Checked Then
        res := res Or resp;
      resp := resp * 2;
    End;
  End;
  edit4.text := format('%.02X', [res]);
End;

Procedure TForm3.CheckBox8Change(Sender: TObject);
Var
  resp, res: byte;
  i: integer;
  cb: TCheckBox;
Begin
  // High Fuse Byte
  If cp.FuseHighEnabled <> 0 Then Begin
    res := 0;
    resp := 1;
    For i := 1 To 8 Do Begin
      cb := TCheckBox(FindComponent('Checkbox' + inttostr(i)));
      If Not cb.Checked Then
        res := res Or resp;
      resp := resp * 2;
    End;
  End;
  edit1.text := format('%.02X', [res]);
End;

Procedure TForm3.ComboBox1Change(Sender: TObject);
Begin
  loadcpu(ComboBox1.Text);
End;

Procedure TForm3.Edit1KeyPress(Sender: TObject; Var Key: char);
Var
  resp, res, i: integer;
  cb: TCheckBox;
Begin
  If key = #13 Then Begin
    res := HextoIntdef(edit1.text, 256);
    If (res >= 0) And (res < 256) Then Begin
      // High Fuse Byte
      If cp.FuseHighEnabled <> 0 Then Begin
        //        res := 0;
        resp := 1;
        For i := 1 To 8 Do Begin
          cb := TCheckBox(FindComponent('Checkbox' + inttostr(i)));
          If cb.Enabled Then
            cb.checked := Not ((resp And res) = resp);
          resp := resp * 2;
        End;
      End;
    End;
  End;
End;

Procedure TForm3.Edit2KeyPress(Sender: TObject; Var Key: char);
Var
  resp, res, i: integer;
  cb: TCheckBox;
Begin
  If key = #13 Then Begin
    res := HextoIntdef(edit2.text, 256);
    If (res >= 0) And (res < 256) Then Begin
      If cp.FuseLowEnabled <> 0 Then Begin
        resp := 1;
        For i := 1 To 8 Do Begin
          cb := TCheckBox(FindComponent('Checkbox' + inttostr(i + 8)));
          If cb.Enabled Then
            cb.checked := Not ((resp And res) = resp);
          resp := resp * 2;
        End;
      End;
    End;
  End;
End;

Procedure TForm3.Edit3KeyPress(Sender: TObject; Var Key: char);
Var
  resp, res, i: integer;
  cb: TCheckBox;
Begin
  If key = #13 Then Begin
    res := HextoIntdef(edit3.text, 256);
    If (res >= 0) And (res < 256) Then Begin
      If cp.FuseHighEnabled <> 0 Then Begin
        resp := 1;
        For i := 1 To 8 Do Begin
          cb := TCheckBox(FindComponent('Checkbox' + inttostr(i + 16)));
          If cb.Enabled Then
            cb.checked := Not ((resp And res) = resp);
          resp := resp * 2;
        End;
      End;
    End;
  End;
End;

Procedure TForm3.Edit4KeyPress(Sender: TObject; Var Key: char);
Var
  resp, res, i: integer;
  cb: TCheckBox;
Begin
  If key = #13 Then Begin
    res := HextoIntdef(edit4.text, 256);
    If (res >= 0) And (res < 256) Then Begin
      If cp.FuseLockEnabled <> 0 Then Begin
        resp := 1;
        For i := 1 To 8 Do Begin
          cb := TCheckBox(FindComponent('Checkbox' + inttostr(i + 24)));
          If cb.Enabled Then
            cb.checked := Not ((resp And res) = resp);
          resp := resp * 2;
        End;
      End;
    End;
  End;
End;

End.

