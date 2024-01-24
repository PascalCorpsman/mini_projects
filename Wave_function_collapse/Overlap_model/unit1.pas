(******************************************************************************)
(* Wave function collapse (Ovelap mode)                            23.01.2024 *)
(*                                                                            *)
(* Version     : 0.03                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Demonstration application of Wave function collapse algorithm*)
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
(*               0.02 - Cleanup                                               *)
(*                      Floor feature                                         *)
(*                      Improve gui                                           *)
(*               0.03 - allow wrap                                            *)
(*                                                                            *)
(******************************************************************************)
(*
 * This started as FPC port of https://github.com/D-T-666/wave-function-collapse-p5
 *)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ExtDlgs, uwfc;

Const
  Refreshrate = 100;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    OpenPictureDialog1: TOpenPictureDialog;
    SaveDialog1: TSaveDialog;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure CheckBox1Click(Sender: TObject);
    Procedure CheckBox2Click(Sender: TObject);
    Procedure CheckBox3Click(Sender: TObject);
    Procedure Edit1Change(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
  private
    wfc: TWFC;
    lt: Int64;
    Pattern: TBitmap;
    Procedure OnUpdatedStep(Sender: TObject; Const Info: TInfo);
    Procedure LoadPattern(Const Filename: String);
  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}


(*
 * Formatiert TimeInmS als möglich hübsche Zeiteinheit
 *
 * 0ms bis x Tage [ Jahre werden nicht unterstützt da sonst schaltjahre und ettliches mehr berücksichtigt werden müssen
 * 0 => 0ms
 * 500 => 500ms
 * 1000 => 1s
 * 1500 => 1,5s
 * 65000 => 1:05min
 * 80000 => 1:20min
 * 3541000 => 59:01min
 * 3600000 => 1h
 * 3660000 => 1:01h
 * 86400000 => 1d
 * 129600000 => 1d 12h
 * 30762000000 => 356d 1h
 *)

Function PrettyTime(Time_in_ms: UInt64): String;
Var
  hs, digits, sts, sep, s: String;
  st, i: integer;
  b: Boolean;
Begin
  s := 'ms';
  hs := '';
  sep := DefaultFormatSettings.DecimalSeparator;
  st := 0;
  b := false;
  digits := '3';
  // [0 .. 60[ s
  If Time_in_ms >= 1000 Then Begin
    st := Time_in_ms Mod 1000;
    Time_in_ms := Time_in_ms Div 1000;
    s := 's';
    b := true;
  End;
  // [1 .. 60[ min
  If (Time_in_ms >= 60) And b Then Begin
    st := Time_in_ms Mod 60;
    Time_in_ms := Time_in_ms Div 60;
    s := 'min';
    sep := DefaultFormatSettings.TimeSeparator;
    digits := '2';
  End
  Else
    b := false;
  // [1 .. 24[ h
  If (Time_in_ms >= 60) And b Then Begin
    st := Time_in_ms Mod 60;
    Time_in_ms := Time_in_ms Div 60;
    s := 'h';
  End
  Else
    b := false;
  // [1 ..  d
  If (Time_in_ms >= 24) And b Then Begin
    st := Time_in_ms Mod 24;
    Time_in_ms := Time_in_ms Div 24;
    hs := 'd';
    If st <> 0 Then s := 'h';
    sep := ' ';
    digits := '1';
  End
  Else
    b := false;
  // Ausgabe mit oder ohne Nachkomma
  If st <> 0 Then Begin
    sts := format('%0.' + digits + 'd', [st]);
    If (s = 's') Then Begin // Bei Sekunden die endenden 0-en löschen
      For i := length(sts) Downto 1 Do Begin
        If sts[i] = '0' Then Begin
          delete(sts, i, 1);
        End
        Else Begin
          break;
        End;
      End;
    End;
    result := inttostr(Time_in_ms) + hs + sep + sts + s;
  End
  Else Begin
    result := inttostr(Time_in_ms) + s;
  End;
End;

{ TForm1 }

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  If assigned(wfc) Then wfc.free;
  wfc := Nil;
  If assigned(pattern) Then Begin
    wfc := TWFC.Create;
    If CheckBox3.Checked Or CheckBox2.Checked Then CheckBox1.Checked := false;
    wfc.InitFromImage(pattern, strtointdef(edit1.text, 3), CheckBox1.Checked, Not CheckBox2.Checked, Not CheckBox3.Checked);
    button2.Enabled := true;
  End
  Else Begin
    showmessage('Error, no image set.');
  End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Var
  t: int64;
  Dummy: TInfo;
Begin
  If Not assigned(wfc) Then exit;
  button1.Enabled := false;
  button2.Enabled := false;
  button5.visible := true;
  lt := GetTickCount64 - 2 * Refreshrate;
  wfc.OnUpdatedStep := @OnUpdatedStep;
  t := GetTickCount64;
  wfc.Run(strtointdef(edit2.text, 40), strtointdef(edit3.text, 40));
  caption := 'Took ' + PrettyTime(GetTickCount64 - t);
  lt := GetTickCount64 - 2 * Refreshrate;
  Dummy.Backlog := 0;
  Dummy.CollapsedCells := 0;
  Dummy.TotalCellsToCollapse := 0;
  OnUpdatedStep(Nil, Dummy);
  button5.visible := false;
  button2.Enabled := True;
  button1.Enabled := True;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  If OpenPictureDialog1.Execute Then Begin
    LoadPattern(OpenPictureDialog1.FileName);
  End;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Var
  b: TBitmap;
  p: TPortableNetworkGraphic;
Begin
  If Not assigned(wfc) Then exit;
  If SaveDialog1.Execute Then Begin
    b := wfc.GetImage();
    p := TPortableNetworkGraphic.Create;
    p.Assign(b);
    b.free;
    p.SaveToFile(SaveDialog1.FileName);
    p.free;
  End;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Begin
  If assigned(wfc) Then wfc.abort;
End;

Procedure TForm1.Button6Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.CheckBox1Click(Sender: TObject);
Begin
  button2.Enabled := false;
End;

Procedure TForm1.CheckBox2Click(Sender: TObject);
Begin
  button2.Enabled := false;
  If CheckBox3.Checked Or CheckBox2.Checked Then CheckBox1.Checked := false;
End;

Procedure TForm1.CheckBox3Click(Sender: TObject);
Begin
  button2.Enabled := false;
  If CheckBox3.Checked Or CheckBox2.Checked Then CheckBox1.Checked := false;
End;

Procedure TForm1.Edit1Change(Sender: TObject);
Begin
  button2.Enabled := false;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  // Abort if is running
  button5.Click;
  Application.ProcessMessages;
  // Now Free everything
  wfc.Free;
  wfc := Nil;
  If assigned(Pattern) Then Pattern.free;
  Pattern := Nil;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  Randomize;
  caption := 'Wave function collapse demo ver. 0.03';
  edit1.text := '3';
  edit2.text := '40';
  edit3.text := '40';
  CheckBox1.Checked := true; // Allow Rotate
  CheckBox2.Checked := true; // Allow Wrap vertical
  CheckBox3.Checked := true; // Allow Wrap Horizontal
  Pattern := Nil;
  LoadPattern('data' + PathDelim + 'demo-1.png');
  label4.caption := '';
  // Debug
  // LoadPattern('data' + PathDelim + 'demo-flowers2.png');
  // edit2.text := '100';
  // edit3.text := '100';
  // CheckBox1.Checked := false; // Allow Rotate
  // CheckBox2.Checked := false; // Allow Wrap vertical
  // CheckBox3.Checked := true; // Allow Wrap Horizontal
  // *)
End;

Procedure TForm1.OnUpdatedStep(Sender: TObject; Const Info: TInfo);
Var
  aTick: Int64;
  b: TBitmap;
Begin
  If Not assigned(wfc) Then exit;
  aTick := GetTickCount64;
  If lt + Refreshrate <= aTick Then Begin
    lt := aTick;
    b := wfc.GetImage();
    Image1.Picture.Assign(b);
    b.free;
    Image1.Invalidate;
    If assigned(sender) Then Begin
      label4.caption := format(
        'Cells total: %d' + LineEnding +
        'Cells done: %d' + LineEnding +
        'Backlog: %d',
        [info.TotalCellsToCollapse, info.CollapsedCells, info.Backlog]);
    End
    Else Begin
      label4.Caption := '';
    End;
    Application.ProcessMessages;
  End;
End;

Procedure TForm1.LoadPattern(Const Filename: String);
Var
  p: TPortableNetworkGraphic;
Begin
  If assigned(Pattern) Then Pattern.free;
  Pattern := Nil;
  Case LowerCase(ExtractFileExt(Filename)) Of
    '.png': Begin
        p := TPortableNetworkGraphic.Create;
        p.LoadFromFile(Filename);
        pattern := TBitmap.Create;
        Pattern.Assign(p);
        p.free;
      End;
    '.bmp': Begin
        pattern := TBitmap.Create;
        pattern.LoadFromFile(Filename);
      End;
  Else Begin
      showmessage('Not supported file format.');
    End;
  End;
  If assigned(Pattern) Then Begin
    Image2.Picture.Assign(Pattern);
  End
  Else Begin
    Image2.Picture.Clear;
  End;
  button2.Enabled := false;
End;

End.

