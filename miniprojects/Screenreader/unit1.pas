(******************************************************************************)
(* Screenreader                                                    13.01.2025 *)
(*                                                                            *)
(* Version     : 0.05                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : application to easy take one or multiple partly screenshots  *)
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
(* History     : 0.01 - Windows Version                                       *)
(*               0.02 - Portierung nach Linux                                 *)
(*               0.03 - Speedup Linuxport                                     *)
(*               0.04 - Editierbare Koordinaten                               *)
(*               0.05 - Store Settings                                        *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  IniPropStorage;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    CheckBox1: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    IniPropStorage1: TIniPropStorage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Timer1: TTimer;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Edit4KeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure Edit5KeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private
    Procedure PlotPs();
  public
    p1, p2: TPoint;
    Psel: Boolean;
    n: integer;
  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses lclintf, lcltype, math, IntfGraphics, fpImage, GraphType;
(*
Erzeugt einen Teil Screenshot und gibt diesen zurück
*)

Function CaptureScreenRect(aTopLeft, aBottomRight: TPoint): TBitmap;
{$IFDEF WINDOWS}
Var
  ScreenDC: HDC;
  tmp: TBitmap;
Begin
  tmp := TBitmap.Create;
  tmp.Width := Screen.DesktopWidth;
  tmp.Height := Screen.DesktopHeight;
  tmp.Canvas.Brush.Color := clWhite;
  tmp.Canvas.FillRect(0, 0, tmp.Width, tmp.Height);
  ScreenDC := GetDC(GetDesktopWindow);
  BitBlt(tmp.Canvas.Handle, 0, 0, tmp.Width, tmp.Height, ScreenDC, Screen.DesktopLeft, Screen.DesktopTop, SRCCOPY);
  ReleaseDC(0, ScreenDC);
  result := TBitmap.Create;
  result.Width := abs(aTopLeft.X - aBottomRight.X + 1);
  result.Height := abs(aTopLeft.Y - aBottomRight.Y + 1);
  result.Canvas.Draw(-min(aTopLeft.X, aBottomRight.X), -min(aTopLeft.Y, aBottomRight.Y), tmp);
  tmp.free;
End;
{$ENDIF}
{$IFDEF LINUX}
Var
  ScreenDC: HDC;
  IntfImg: TLazIntfImage;
  R: TRect;
  RawImage: TRawImage;
Begin
  result := TBitmap.Create;
  ScreenDC := GetDC(0);
  IntfImg := TLazIntfImage.Create(0, 0, []);
  R.TopLeft := aTopLeft;
  R.BottomRight := aBottomRight;
  If Not RawImage_FromDevice(RawImage, ScreenDC, R) Then
    Raise FPImageException.Create('Failed to get raw image from device');
  IntfImg.SetRawImage(RawImage);
  result.LoadFromIntfImage(IntfImg);
  IntfImg.free;
  ReleaseDC(0, ScreenDC);
End;
{$ENDIF}

{ TForm1 }

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Var
  p: TPoint;
Begin
  // Einlesen der Cursor Positionen
  p := Mouse.CursorPos;
  If Psel Then Begin
    p2 := p;
  End
  Else Begin
    p1 := p;
  End;
  Psel := Not Psel;
  PlotPs();
End;

Procedure TForm1.Button4Click(Sender: TObject);
Begin
  If Timer1.Enabled Then Begin
    Button4.Caption := 'start capturing';
  End
  Else Begin
    Timer1.Interval := max(1000 Div strtoint(edit1.text), 10);
    Button4.Caption := 'stop capturing';
  End;
  Timer1.Enabled := Not Timer1.Enabled;
End;

Procedure TForm1.Edit4KeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState
  );
Var
  a, b, s: String;
  x, y: integer;
Begin
  If key = VK_RETURN Then Begin
    s := Edit4.Text;
    a := trim(copy(s, 1, pos('/', s) - 1));
    b := trim(copy(s, pos('/', s) + 1, length(s)));
    Try
      x := strtoint(a);
      y := strtoint(b);
      p1.X := x;
      p1.Y := y;
      showmessage('new coords set.');
    Except
      showmessage('Error, during convertion, no changes made.');
    End;
  End;
End;

Procedure TForm1.Edit5KeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState
  );
Var
  a, b, s: String;
  x, y: integer;
Begin
  If key = VK_RETURN Then Begin
    s := Edit5.Text;
    a := trim(copy(s, 1, pos('/', s) - 1));
    b := trim(copy(s, pos('/', s) + 1, length(s)));
    Try
      x := strtoint(a);
      y := strtoint(b);
      p2.X := x;
      p2.Y := y;
      showmessage('new coords set.');
    Except
      showmessage('Error, during convertion, no changes made.');
    End;
  End;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  IniPropStorage1.WriteInteger('p1x', p1.X);
  IniPropStorage1.WriteInteger('p1y', p1.y);
  IniPropStorage1.WriteInteger('p2x', p2.X);
  IniPropStorage1.WriteInteger('p2y', p2.y);
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  caption := 'Screeny ver. 0.05';
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
  Constraints.MinWidth := Width;
  p1 := point(0, 0);
  p2 := point(0, 0);
  p1.x := IniPropStorage1.ReadInteger('p1x', p1.x);
  p1.y := IniPropStorage1.ReadInteger('p1y', p1.y);
  p2.x := IniPropStorage1.ReadInteger('p2x', p2.x);
  p2.y := IniPropStorage1.ReadInteger('p2y', p2.y);
  PlotPs();
  Label2.caption := '';
  edit1.text := '25';
  edit3.text := 'screenshoot';
  Edit2.text := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'tmp';
  n := 0;
End;

Procedure TForm1.Timer1Timer(Sender: TObject);
Var
  bm: TBitmap;
  d: QWord;
  s: String;
  png: TPortableNetworkGraphic;
Begin
  d := GetTickCount64;
  s := IncludeTrailingBackslash(Edit2.text);
  If Not ForceDirectories(s) Then Begin
    Button4Click(Nil); // Stop recording
    showmessage('Error, could not create : ' + s);
    exit;
  End;
  bm := CaptureScreenRect(point(min(p1.x, p2.x), min(p1.y, p2.Y)), point(max(p1.x, p2.x), max(p1.y, p2.Y)));
  Try
    If CheckBox1.Checked Then Begin
      png := TPortableNetworkGraphic.Create;
      png.Assign(bm);
      Try
        png.SaveToFile(s + edit3.text + format('%0.5d', [n]) + '.png');
      Except
        On av: exception Do Begin
          Button4Click(Nil); // Stop recording
          showmessage(av.Message);
          png.free;
          bm.Free;
          exit;
        End;
      End;
      png.free;
    End
    Else Begin
      bm.SaveToFile(s + edit3.text + format('%0.5d', [n]) + '.bmp');
    End;
  Except
    On av: exception Do Begin
      Button4Click(Nil); // Stop recording
      showmessage(av.Message);
      bm.Free;
      exit;
    End;
  End;
  bm.Free;
  inc(n);
  label2.caption :=
    'Time [ms]: ' + inttostr(GetTickCount64 - d) + LineEnding +
    'Imgcount = ' + inttostr(n);
End;

Procedure TForm1.PlotPs();
Begin
  edit4.text := format('%d / %d', [p1.x, p1.y]);
  edit5.text := format('%d / %d', [p2.x, p2.y]);
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  // Macht genau 1 Bild
  Timer1Timer(Nil);
End;

End.

