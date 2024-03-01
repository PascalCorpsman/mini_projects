(******************************************************************************)
(* Color_test                                                      ??.??.???? *)
(*                                                                            *)
(* Version     : 0.02                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : <Module_description>                                         *)
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
(*               0.02 - Besseres Hook Handling unter Windows.                 *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
{$IFDEF Windows}
  windows,
{$ELSE}
  LCLType, LCLIntf,
{$ENDIF}
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ComCtrls, Spin
  ;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
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
    Label28: TLabel;
    Label29: TLabel;
    Label3: TLabel;
    Label30: TLabel;
    Label31: TLabel;
    Label32: TLabel;
    Label33: TLabel;
    Label34: TLabel;
    Label35: TLabel;
    Label36: TLabel;
    Label37: TLabel;
    Label38: TLabel;
    Label39: TLabel;
    Label4: TLabel;
    Label40: TLabel;
    Label41: TLabel;
    Label42: TLabel;
    Label43: TLabel;
    Label44: TLabel;
    Label45: TLabel;
    Label46: TLabel;
    Label47: TLabel;
    Label48: TLabel;
    Label49: TLabel;
    Label5: TLabel;
    Label50: TLabel;
    Label51: TLabel;
    Label52: TLabel;
    Label53: TLabel;
    Label54: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    PaintBox2: TPaintBox;
    PaintBox3: TPaintBox;
    PaintBox4: TPaintBox;
    PaintBox5: TPaintBox;
    PaintBox6: TPaintBox;
    PaintBox7: TPaintBox;
    Panel1: TPanel;
    Panel2: TPanel;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    ScrollBar3: TScrollBar;
    SpinEdit1: TSpinEdit;
    SpinEdit2: TSpinEdit;
    SpinEdit3: TSpinEdit;
    Timer1: TTimer;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormPaint(Sender: TObject);
    Procedure ScrollBar1Change(Sender: TObject);
    Procedure ScrollBar2Change(Sender: TObject);
    Procedure ScrollBar3Change(Sender: TObject);
    Procedure SpinEdit1Change(Sender: TObject);
    Procedure SpinEdit2Change(Sender: TObject);
    Procedure SpinEdit3Change(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private
    Procedure RefreshGraphikPreview;
    Procedure CalcMaxDistColor(Var r: Byte; Var g: Byte; Var b: Byte);
    Procedure LoadColor(C: TColor);

  public

  End;

Var
  InfoShown: Boolean = false;
  Form1: TForm1;
  mHook: cardinal = 0;

Implementation

{$R *.lfm}

Uses math, Clipbrd;

Function PointInRect(P: TPoint; R: TRect): Boolean;
Begin
  result := (p.x >= r.Left) And (p.x <= r.Right) And
    (p.Y >= r.Top) And (p.Y <= r.Bottom);
End;

(*
Erzeugt einen Screenshot und gibt diesen zurück
*)

Function CaptureScreen(): Graphics.TBitmap; // -- Das ist besser weil es den Gesammten Screen aufnimmt !
{$IFDEF WINDOWS}
Var
  ScreenDC: HDC;
Begin
  Result := Graphics.TBitmap.Create;
  Result.Width := Screen.DesktopWidth;
  Result.Height := Screen.DesktopHeight;
  Result.Canvas.Brush.Color := clWhite;
  Result.Canvas.FillRect(0, 0, Result.Width, Result.Height);
  ScreenDC := GetDC(GetDesktopWindow);
  BitBlt(Result.Canvas.Handle, 0, 0, Result.Width, Result.Height, ScreenDC, Screen.DesktopLeft, Screen.DesktopTop, SRCCOPY);
  ReleaseDC(0, ScreenDC);
End;
{$ENDIF}

{$IFDEF LINUX}
Var
  ScreenDC: HDC;
Begin
  Result := TBitmap.Create;
  With Result Do Begin
    Height := Screen.Height;
    Width := Screen.Width;
  End;
  ScreenDC := GetDC(0);
  Result.LoadFromDevice(ScreenDC);
  ReleaseDC(0, ScreenDC);
End;
{$ENDIF}

{$IFDEF Windows}
Type
  MouseLLHookStruct = Record
    pt: TPoint;
    mouseData: cardinal;
    flags: cardinal;
    time: cardinal;
    dwExtraInfo: cardinal;
  End;

Function LowLevelMouseHookProc(nCode: LongInt; WPARAM: WPARAM; lParam: LPARAM): LRESULT; stdcall;
Var
  info: ^MouseLLHookStruct absolute lParam;
  b: TBitmap;
  c: TColor;
  x, y: Integer;
Begin
  With info^ Do
    Case wParam Of
      wm_lbuttondown: Begin
          UnhookWindowsHookEx(mHook);
          mHook := 0;
          //Form1.caption := IntToStr(Mouse.CursorPos.X) + ':' + IntToStr(Mouse.CursorPos.Y);
          x := Mouse.CursorPos.x;
          y := Mouse.CursorPos.Y;
          b := CaptureScreen();
          c := b.canvas.Pixels[X, y];
          b.free;
          Form1.LoadColor(c);
          form1.Timer1.Enabled := true;
          result := 0;
          exit;
        End;
    End;
  result := CallNextHookEx(mHook, nCode, wParam, lParam);
End;
{$ENDIF}

Function Luminance(r, g, b: Byte): Byte;
Begin
  // Quelle: https://de.wikipedia.org/wiki/Luminanz
  result := min(255, max(0, round(r * 0.299 + g * 0.587 + b * 0.114)));
End;

{ TForm1 }

Procedure TForm1.ScrollBar1Change(Sender: TObject);
Begin
  SpinEdit1.Value := ScrollBar1.Position;
  Invalidate;
End;

Procedure TForm1.ScrollBar2Change(Sender: TObject);
Begin
  SpinEdit2.Value := ScrollBar2.Position;
  Invalidate;
End;

Procedure TForm1.ScrollBar3Change(Sender: TObject);
Begin
  SpinEdit3.Value := ScrollBar3.Position;
  Invalidate;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
  caption := 'Color calculator ver. 0.02';
  Application.Title := caption;
  ScrollBar1.Position := 229;
  ScrollBar2.Position := 160;
  ScrollBar3.Position := 153;
  Panel1.Caption := '';
  Panel2.Caption := '';
  Invalidate;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button2Click(Sender: TObject);
{$IFDEF Windows}
Const
  wh_mouse_ll = 14;
{$ELSE}
Var
  b: TBitmap;
  p: TPoint;
  c: TColor;
{$ENDIF}
Begin
  Cursor := crCross;
{$IFDEF Windows}
  If mhook = 0 Then Begin
    mHook := SetWindowsHookEx(wh_mouse_ll, @LowLevelMouseHookProc, hInstance, 0);
  End;
{$ELSE}
  If Not InfoShown Then Begin
    InfoShown := true;
    ShowMessage('move the cursor to the position you want to scan the color of, then press this button again using the keyboard.');
    exit;
  End;
  b := CaptureScreen();
  p := Mouse.CursorPos;
  c := b.canvas.Pixels[p.x, p.y];
  LoadColor(c);
  b.free;
{$ENDIF}
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  Clipboard.AsText := Format('$00%0.2X%0.2X%0.2X', [SpinEdit3.Value, SpinEdit2.Value, SpinEdit1.Value]);
End;

Procedure TForm1.FormPaint(Sender: TObject);
Begin
  RefreshGraphikPreview;
End;

Procedure TForm1.SpinEdit1Change(Sender: TObject);
Begin
  ScrollBar1.Position := SpinEdit1.Value;
  Invalidate;
End;

Procedure TForm1.SpinEdit2Change(Sender: TObject);
Begin
  ScrollBar2.Position := SpinEdit2.Value;
  Invalidate;
End;

Procedure TForm1.SpinEdit3Change(Sender: TObject);
Begin
  ScrollBar3.Position := SpinEdit3.Value;
  Invalidate;
End;

Procedure TForm1.Timer1Timer(Sender: TObject);
Begin
  Application.Restore;
  Application.BringToFront;
  Form1.BringToFront;
  Timer1.Enabled := false;
End;

Procedure TForm1.RefreshGraphikPreview;
Var
  i: integer;
  l, r, g, b: byte;
  c, m, y, k: integer;
Begin
  r := SpinEdit1.Value;
  g := SpinEdit2.Value;
  b := SpinEdit3.Value;
  l := Luminance(r, g, b);
  For i := 0 To 255 Do Begin
    PaintBox3.Canvas.Pen.Color := RGBToColor(i, g, b);
    PaintBox3.Canvas.Line(i, 0, i, 11);
    PaintBox4.Canvas.Pen.Color := RGBToColor(r, i, b);
    PaintBox4.Canvas.Line(i, 0, i, 11);
    PaintBox5.Canvas.Pen.Color := RGBToColor(r, g, i);
    PaintBox5.Canvas.Line(i, 0, i, 11);
  End;
  // Die Vorschau der Farbe
  PaintBox2.canvas.brush.Color := RGBToColor(r, g, b);
  PaintBox2.canvas.Rectangle(-1, -1, 65, 65);
  // Eine Abgestufte Vorschau
  // 229 160 153
  // 239 198 192
  // 236 192 189
  // 236 184 173
  // 214 125 119
  // 215 104  97
  // Die CMY Berechnung
  c := 255 - r;
  m := 255 - g;
  y := 255 - b;
  label4.caption := inttostr(c);
  label5.caption := inttostr(m);
  label6.caption := inttostr(y);
  // Die CMYK Berechnung
  k := min(c, min(m, y));
  c := c - k;
  m := m - k;
  y := y - k;
  label10.caption := inttostr(c);
  label11.caption := inttostr(m);
  label12.caption := inttostr(y);
  label13.caption := inttostr(k);
  label16.caption :=
    format('#%0.2x%0.2x%0.2x', [r, g, b]) + LineEnding +
    inttostr(b * 256 * 256 + g * 256 + r) + LineEnding +
    format('$00%0.2x%0.2x%0.2x', [b, g, r]) + LineEnding +
    format('0x00%0.2x%0.2x%0.2x', [b, g, r]) + LineEnding +
    format('0x%0.2x%0.2x%0.2x', [b, g, r])
    ;
  // -- Ab Hier Komplementärfarbe --
  r := 255 - r;
  g := 255 - g;
  b := 255 - b;
  label53.Caption := inttostr(abs(l - Luminance(r, g, b)));
  PaintBox6.canvas.brush.Color := RGBToColor(r, g, b);
  PaintBox6.canvas.Rectangle(-1, -1, 65, 65);
  // Die CMY Berechnung
  c := 255 - r;
  m := 255 - g;
  y := 255 - b;
  label22.caption := inttostr(c);
  label23.caption := inttostr(m);
  label24.caption := inttostr(y);
  // Die CMYK Berechnung
  k := min(c, min(m, y));
  c := c - k;
  m := m - k;
  y := y - k;
  label28.caption := inttostr(c);
  label29.caption := inttostr(m);
  label30.caption := inttostr(y);
  label31.caption := inttostr(k);
  label18.caption :=
    format('#%0.2x%0.2x%0.2x', [r, g, b]) + LineEnding +
    inttostr(b * 256 * 256 + g * 256 + r) + LineEnding +
    format('$00%0.2x%0.2x%0.2x', [b, g, r]) + LineEnding +
    format('0x00%0.2x%0.2x%0.2x', [b, g, r]) + LineEnding +
    format('0x%0.2x%0.2x%0.2x', [b, g, r])
    ;
  // -- Ab hier die Max Distance Farbe
  r := SpinEdit1.Value;
  g := SpinEdit2.Value;
  b := SpinEdit3.Value;
  CalcMaxDistColor(r, g, b);
  label54.Caption := inttostr(abs(l - Luminance(r, g, b)));
  PaintBox7.canvas.brush.Color := RGBToColor(r, g, b);
  PaintBox7.canvas.Rectangle(-1, -1, 65, 65);
  // Die CMY Berechnung
  c := 255 - r;
  m := 255 - g;
  y := 255 - b;
  label39.caption := inttostr(c);
  label40.caption := inttostr(m);
  label41.caption := inttostr(y);
  // Die CMYK Berechnung
  k := min(c, min(m, y));
  c := c - k;
  m := m - k;
  y := y - k;
  label45.caption := inttostr(c);
  label46.caption := inttostr(m);
  label47.caption := inttostr(y);
  label48.caption := inttostr(k);
  label35.caption :=
    format('#%0.2x%0.2x%0.2x', [r, g, b]) + LineEnding +
    inttostr(b * 256 * 256 + g * 256 + r) + LineEnding +
    format('$00%0.2x%0.2x%0.2x', [b, g, r]) + LineEnding +
    format('0x00%0.2x%0.2x%0.2x', [b, g, r]) + LineEnding +
    format('0x%0.2x%0.2x%0.2x', [b, g, r])
    ;
End;

Procedure TForm1.CalcMaxDistColor(Var r: Byte; Var g: Byte; Var b: Byte);
Var
  s: Single;
  rr, gg, bb: integer;
  c: integer;
Begin
  // Berechnen des Differenzvektors zur 128 128 128
  rr := 128 - r;
  gg := 128 - g;
  bb := 128 - b;
  If (rr = 0) And (gg = 0) And (bb = 0) Then Begin // Richtungsvektor = 0 => schwarz oder weis
    r := 0;
    g := 0;
    b := 0;
    exit;
  End;
  // Die Komponente des Differenzvektors, welche am größten ist bestimmt automatisch auch die Schnittebene
  If abs(rr) > abs(gg) Then Begin
    If abs(rr) > abs(bb) Then Begin
      // RR am Größten
      c := 1;
    End
    Else Begin
      // BB am Größten
      c := 3;
    End;
  End
  Else Begin
    If abs(gg) > abs(bb) Then Begin
      // GG am Größten
      c := 2;
    End
    Else Begin
      // BB am Größten
      c := 3;
    End;
  End;
  // Berechnen der Skallierung, so das differenzvektor *s + 128 128 128 in einer Komponente auf 255 kommt
  Case c Of
    1: Begin // abs(rr) am Größten
        s := 128 / abs(rr);
      End;
    2: Begin // abs(gg) am Größten
        s := 128 / abs(gg);
      End;
    3: Begin // abs(bb) am Größten
        s := 128 / abs(bb);
      End;
  End;
  r := min(255, max(0, round(rr * s) + 128));
  g := min(255, max(0, round(gg * s) + 128));
  b := min(255, max(0, round(bb * s) + 128));
End;

Procedure TForm1.LoadColor(C: TColor);
Begin
  form1.SpinEdit1.Value := c And $FF;
  form1.SpinEdit2.Value := (c And $FF00) Shr 8;
  form1.SpinEdit3.Value := (c And $FF0000) Shr 16;
  Form1.Invalidate;
End;

End.

