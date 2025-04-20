(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of PixelEditor                                           *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit8;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons, StdCtrls,
  upixelimage, uLineEdit, ugraphics;

Type

  { TForm8 }

  TForm8 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    LineEdit1: TLineEdit;
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure SpeedButton1Click(Sender: TObject);
  private
    fDebounceTime: integer;
    fLastTick: int64;
    Function MapColor(aColor: TRGBA): TRGBA;
  public
    fBackupImage: TPixelImage;
    faImage: TPixelImage;
    Procedure Init(Const aImage: TPixelImage);
    Procedure OnLineEdit1PointChange(Sender: TObject);
  End;

Var
  Form8: TForm8;

Implementation

{$R *.lfm}

Uses math, upixeleditor_types;

{ TForm8 }

Procedure TForm8.FormCreate(Sender: TObject);
Begin
  caption := 'Color curve';
  fBackupImage := TPixelImage.Create();
  faImage := Nil;
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
  LineEdit1 := TLineEdit.Create(self);
  LineEdit1.Name := 'LineEdit1';
  LineEdit1.Parent := self;
  LineEdit1.Top := 10;
  LineEdit1.Left := 10;
  LineEdit1.Width := Scale96ToForm(256);
  LineEdit1.Height := Scale96ToForm(256);
  LineEdit1.BrushColor := clBlack;
  LineEdit1.PenColor := clYellow;
  LineEdit1.GridColor := clYellow Div 3;
  LineEdit1.GridPercent := 25;
  LineEdit1.LineColor := clWhite;
  LineEdit1.PointBoxColor := clBlue;
  LineEdit1.OnPointsChange := @OnLineEdit1PointChange;
End;

Procedure TForm8.Button5Click(Sender: TObject);
Var
  sl: TStringList;
  i: Integer;
Begin
  // Save Curve
  If SaveDialog1.Execute Then Begin
    DefaultFormatSettings.DecimalSeparator := '.';
    sl := TStringList.Create;
    sl.Add('#');
    sl.Add('# Pixeleditor curve file');
    sl.Add('#');
    sl.Add(format('%d points', [LineEdit1.PointCount]));
    For i := 0 To LineEdit1.PointCount - 1 Do Begin
      If (i <> 0) And (i <> LineEdit1.PointCount - 1) Then Begin
        sl.Add(format('X_%d=%0.2f', [i + 1, LineEdit1.Point[i].x]));
      End
      Else Begin
        If i = 0 Then Begin
          sl.Add(format('X_%d=0 # always 0', [i + 1]));
        End
        Else Begin
          sl.Add(format('X_%d=100 # always 100', [i + 1]));
        End;
      End;
      sl.Add(format('Y_%d=%0.2f', [i + 1, LineEdit1.Point[i].y]));
    End;
    sl.SaveToFile(SaveDialog1.FileName);
    sl.free;
  End;
End;

Procedure TForm8.Button3Click(Sender: TObject);
Begin
  // Reset
  LineEdit1.Reset();
  OnLineEdit1PointChange(Nil);
End;

Procedure TForm8.Button4Click(Sender: TObject);
Var
  sl: TStringList;
  num, i: Integer;
  a: Array Of TPercentPoint;
  s: String;
  sa: TStringArray;
  f: Single;
Begin
  If OpenDialog1.Execute Then Begin
    DefaultFormatSettings.DecimalSeparator := '.';
    sl := TStringList.Create;
    Try
      sl.LoadFromFile(OpenDialog1.FileName);
      a := Nil;
      For i := 0 To sl.Count - 1 Do Begin
        If pos('#', trim(sl[i])) = 1 Then Continue;
        If pos('points', sl[i]) <> 0 Then Begin
          s := trim(copy(sl[i], 1, pos('points', sl[i]) - 1));
          setlength(a, max(0, strtointdef(s, 0)));
          Continue;
        End;
        s := sl[i];
        If pos('#', s) <> 0 Then Begin
          delete(s, pos('#', s), length(s));
        End;
        If pos('=', s) <> 0 Then Begin
          // Parsen von <CHAR><NUM>=<FLOAT>
          // if num -1 not in a -> Fehler raus
          sa := trim(s).Split(['_', '=']);
          If length(sa) <> 3 Then Begin
            showmessage('Error, do not understand: ' + sl[i]);
            exit;
          End;
          num := strtointdef(sa[1], -1) - 1;
          If (num < 0) Or (num > high(a)) Then Begin
            showmessage('Error, do not understand: ' + sl[i]);
            exit;
          End;
          sa[0] := uppercase(sa[0]);
          If Not (sa[0][1] In ['X', 'Y']) Then Begin
            showmessage('Error, do not understand: ' + sl[i]);
            exit;
          End;
          f := StrToFloatDef(sa[2], -1);
          If (f < 0) Or (f > 100) Then Begin
            showmessage('Error, do not understand: ' + sl[i]);
            exit;
          End;
          If sa[0] = 'X' Then Begin
            a[num].X := f;
          End
          Else Begin
            a[num].Y := f;
          End;
          Continue;
        End
        Else Begin
          showmessage('Error, do not understand: ' + sl[i]);
          exit;
        End;
      End;
      // Alles erfolgreich in A eingelesen
      For i := 0 To high(a) - 1 Do Begin
        If a[i].X >= a[i + 1].X Then Begin
          showmessage('Error, invalid point definitions -> abort.');
          exit;
        End;
      End;
      // Alles gültig -> übernehmen
      LineEdit1.Reset();
      For i := 0 To high(a) Do Begin
        If (i <> 0) And (i <> high(a)) Then Begin
          LineEdit1.AddPoint(a[i]);
        End
        Else Begin
          LineEdit1.SetPointYValue(i, a[i].Y);
        End;
      End;
      OnLineEdit1PointChange(Nil);
    Finally
      sl.free;
    End;
  End;
End;

Procedure TForm8.FormDestroy(Sender: TObject);
Begin
  fBackupImage.free;
  fBackupImage := Nil;
End;

Procedure TForm8.SpeedButton1Click(Sender: TObject);
Begin
  OnLineEdit1PointChange(Nil);
End;

Function TForm8.MapColor(aColor: TRGBA): TRGBA;
Begin
  If SpeedButton1.Down Then Begin
    aColor.r := min(255, max(0, round(LineEdit1.F(aColor.r / 2.55) * 2.55)));
  End;
  If SpeedButton2.Down Then Begin
    aColor.g := min(255, max(0, round(LineEdit1.F(aColor.g / 2.55) * 2.55)));
  End;
  If SpeedButton3.Down Then Begin
    aColor.b := min(255, max(0, round(LineEdit1.F(aColor.b / 2.55) * 2.55)));
  End;
  result := aColor;
End;

Procedure TForm8.Init(Const aImage: TPixelImage);
Begin
  fBackupImage.CloneFrom(aImage);
  faImage := aImage;
  fDebounceTime := 0;
  fLastTick := GetTickCount64;
  OnLineEdit1PointChange(Nil);
End;

Procedure TForm8.OnLineEdit1PointChange(Sender: TObject);
Var
  n: Int64;
Begin
  n := GetTickCount64;
  If n - fLastTick < fDebounceTime Then Begin
    exit;
  End;
  fLastTick := n;
  faImage.BeginUpdate;
  faImage.CloneFrom(fBackupImage);
  faImage.MapColors(@MapColor);
  faImage.EndUpdate;
  fDebounceTime := max(fDebounceTime, GetTickCount64 - fLastTick);
End;

End.

