(******************************************************************************)
(* WaveShaper                                                      06.08.2025 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Tool to convert images into .wav files that have the same    *)
(*               shape ;).                                                    *)
(*               Inspired by:                                                 *)
(*                     https://youtu.be/qeUAHHPt-LY?si=haVvvZ1xqHj8FCQ9       *)
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
(*                                                                            *)
(******************************************************************************)

Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtDlgs,
  ExtCtrls, TAGraph, TASeries, uwave, Bass;

Type

  TColumInfo = Record
    Mid: Single;
    Upper, Lower: integer;
  End;

  TImageData = Record
    Width, Height: Integer;
    data: Array Of TColumInfo;
  End;

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Chart1: TChart;
    Chart1LineSeries1: TLineSeries;
    Edit1: TEdit;
    Edit2: TEdit;
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Memo1: TMemo;
    OpenDialog1: TOpenDialog;
    OpenPictureDialog1: TOpenPictureDialog;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    ScrollBar1: TScrollBar;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure Image2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure ScrollBar1Change(Sender: TObject);
  private
    Wave: TWave;
    PreviewStream: HSTREAM;
    WavePos: Integer;
    ImageData: TImageData;
    Procedure AddWaveToLCL;
  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses ugraphics, math, uvectormath;

Const
  DefSampleRate = 44100;

Function GetPreviewData(handle: HSTREAM; buffer: Pointer; length: DWORD; user: Pointer): DWORD;
{$IFDEF MSWINDOWS} stdcall{$ELSE} cdecl{$ENDIF};
Var
  buf: PSingle;
  len, i: Integer;
Begin
  buf := buffer;
  len := max(0, min(length Div 4, Form1.Wave.SampleCount - Form1.WavePos)); // 4- Byte Pro Wert * 1 Kanäle
  For i := 0 To len - 1 Do Begin
    buf^ := Form1.Wave.Sample[0, Form1.WavePos];
    inc(buf);
    Form1.WavePos := Form1.WavePos + 1;
  End;
  result := len * 4;
  If len = 0 Then Begin
    BASS_ChannelStop(handle);
    form1.button4.Enabled := true;
  End;
End;

{ TForm1 }

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  If OpenPictureDialog1.Execute Then Begin
    Image1.Picture.LoadFromFile(OpenPictureDialog1.FileName);
  End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Var
  b: TBitmap;
  freq, Lower, Upper: Integer;
  i, j, k: integer;
  NeededPeriods: Single;
Begin
  b := TBitmap.Create;
  b.Assign(Image1.Picture.Bitmap);
  // 1. Binarisieren
  Binarisate(b, 128);
  // 2. Senkrechten "Konkav" machen
  // TODO: umstellen auf LazIntfImage
  ImageData.Width := b.Width;
  ImageData.Height := b.Height;
  setlength(ImageData.data, b.Width);

  For i := 0 To b.Width - 1 Do Begin
    Upper := -1;
    Lower := -1;
    ImageData.data[i].Lower := -1;
    ImageData.data[i].Upper := -1;
    ImageData.data[i].Mid := -1;
    For j := 0 To b.Height - 1 Do Begin
      If upper = -1 Then Begin
        If b.Canvas.Pixels[i, j] <> clWhite Then Begin
          upper := j;
        End;
      End;
      If Lower = -1 Then Begin
        If b.Canvas.Pixels[i, b.Height - j - 1] <> clWhite Then Begin
          Lower := b.Height - j - 1;
        End;
      End;
      If (upper <> -1) And (lower <> -1) Then Begin
        For k := Upper + 1 To Lower - 1 Do Begin
          b.Canvas.Pixels[i, k] := clBlack;
        End;
        b.Canvas.Pixels[i, (Upper + Lower) Div 2] := clRed;
        ImageData.data[i].Lower := Lower;
        ImageData.data[i].Upper := Upper;
        ImageData.data[i].Mid := (Upper + Lower) / 2;
        break;
      End;
    End;
  End;
  Image2.Picture.Assign(b);
  // Berechnen einer Potentiellen Duration
  (*
   * Das ganze wird so gestaltet, dass Pro Pixel eine Periode genutzt werden kann.
   *)
  freq := strtointdef(edit1.Text, 0);
  NeededPeriods := b.Width / freq;
  edit2.text := format('%f', [NeededPeriods]);
  b.free;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  If OpenDialog1.Execute Then Begin
    If assigned(wave) Then wave.free;
    wave := TWave.Create;
    wave.LoadFromFile(OpenDialog1.FileName);
    memo1.clear;
    memo1.Lines.Add(ExtractFileName(OpenDialog1.FileName));
    AddWaveToLCL;
  End;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Begin
  // Preview
  If Not assigned(Wave) Then Begin
    showmessage('Error, no wav defined.');
    exit;
  End;
  WavePos := 0;
  If PreviewStream <> 0 Then Begin
    BASS_ChannelStop(PreviewStream);
    BASS_StreamFree(PreviewStream);
  End;
  PreviewStream := BASS_StreamCreate(wave.SampleRate, 1, BASS_SAMPLE_FLOAT, @GetPreviewData, Nil);

  If BASS_ChannelPlay(PreviewStream, false) Then Begin
    button4.Enabled := false;
  End
  Else Begin
    showmessage('Could not start stream playback');
    exit;
  End;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Var
  Frequence: Integer;

  Function f(t: Single): Single;
  Begin
    result := sin(2 * pi * Frequence * t);
  End;

Var
  dist, Offset, Scale, SamplesPerPixel, SampleValue, duration: Single;
  x, Samples, SampleRate, i: Integer;
Begin
  // Create Wave by Frequency and Image ;)
  memo1.Clear;
  If ImageData.Width = 0 Then Begin
    showmessage('Error, no input image');
    exit;
  End;
  duration := StrToFloatdef(edit2.text, 0);
  If duration <= 0 Then Begin
    showmessage('Error, invalid duration');
    exit;
  End;
  Frequence := StrToIntDef(edit1.text, 0);
  If Frequence <= 0 Then Begin
    showmessage('Error, invalid frequence');
    exit;
  End;
  SampleRate := DefSampleRate;
  If assigned(Wave) Then wave.free;
  wave := TWave.Create;
  Samples := round(duration * SampleRate);
  wave.InitNewBuffer(1, SampleRate, 16, Samples);
  SamplesPerPixel := SampleRate / Frequence;
  For i := 0 To Samples - 1 Do Begin
    // Die Reine "unveränderte" Wave
    SampleValue := f(i / SampleRate);
    // Ausrechen der X - Position im Bild
    x := trunc(i / SamplesPerPixel);
    If (x > high(ImageData.data)) Or (ImageData.data[x].Mid = -1) Then Begin
      // Der Datenpunkt hat gar keinen Wert -> Das Ausgangssignal ist "0"
      SampleValue := 0;
    End
    Else Begin
      Scale := 0.25;
      If SampleValue <= 0 Then Begin // Die Scallierung Unterhalb der Mittellinie
        dist := abs(ImageData.data[x].Mid - ImageData.data[x].Lower);
      End
      Else Begin // Die Scallierung überhalb der Mittellinie
        dist := abs(ImageData.data[x].Mid - ImageData.data[x].Upper);
      End;
      // Dist ist in 0.. Height / 2 und muss skalliert werden auf 0..1
      scale := ConvertDimension(0, ImageData.Height / 2, dist, 0, 1);
      Offset := ConvertDimension(0, ImageData.Height - 1, ImageData.data[x].Mid, 1, -1);
      SampleValue := clamp(Scale * SampleValue + Offset, -1, 1);
    End;
    wave.Sample[0, i] := SampleValue;
  End;
  memo1.lines.add(format('Sine with: %d Hz', [Frequence]));
  AddWaveToLCL()
End;

Procedure TForm1.Button6Click(Sender: TObject);
Var
  SourceWave: TWave;
Begin
  // Create Wave From Different Wave
  memo1.Clear;
  If ImageData.Width = 0 Then Begin
    showmessage('Error, no input image');
    exit;
  End;
  If OpenDialog1.Execute Then Begin
//    hier weiter..
  End;
End;

Procedure TForm1.Button7Click(Sender: TObject);
Begin
  // Export Wave
  If Not assigned(Wave) Then Begin
    ShowMessage('Error, nothing to export.');
    exit;
  End;
  If SaveDialog2.Execute Then Begin
    If Not wave.SaveToFile(SaveDialog2.FileName) Then Begin
      ShowMessage('Error, during export.');
    End;
  End;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  If assigned(wave) Then wave.free;
  wave := Nil;
  If PreviewStream <> 0 Then Begin
    BASS_ChannelStop(PreviewStream);
    BASS_StreamFree(PreviewStream);
  End;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  caption := 'WaveShaper ver. 0.01 by Corpsman';
  Wave := Nil;
  If (BASS_GetVersion() Shr 16) <> Bassversion Then Begin
    showmessage('Unable to init the Bass Library ver. :' + BASSVERSIONTEXT);
    halt;
  End;
  If (Not Bass_init(-1, DefSampleRate, 0, {$IFDEF Windows}0{$ELSE}Nil{$ENDIF}, Nil)) Then Begin
    showmessage('Unable to init sound device.');
    halt;
  End;
  PreviewStream := 0;
  Memo1.Clear;
  edit1.text := '200';
  edit2.text := '';
  ImageData.Width := 0;
  ImageData.Height := 0;
  ImageData.data := Nil;
  ScrollBar1.Position := round(100 - BASS_GetVolume() * 100);
  ScrollBar1Change(Nil);
  // Debug Remove
  Image1.Picture.LoadFromFile('Cow.png');
  //Image1.Picture.LoadFromFile('Test.png');
End;

Procedure TForm1.Image2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  If ssright In shift Then Begin
    If SaveDialog1.Execute Then Begin
      image2.Picture.Bitmap.SaveToFile(SaveDialog1.FileName);
    End;
  End;
End;

Procedure TForm1.ScrollBar1Change(Sender: TObject);
Begin
  label5.caption := format('%d %%', [100 - ScrollBar1.Position]);
  BASS_SetVolume(1 - ScrollBar1.Position / 100);
End;

Procedure TForm1.AddWaveToLCL;
Var
  i: Integer;
Begin
  Chart1LineSeries1.Clear;
  For i := 0 To wave.SampleCount - 1 Do Begin
    Chart1LineSeries1.Add(wave.Sample[0, i]);
  End;
  memo1.Lines.Add(format('Samplerate: %d', [Wave.SampleRate]));
  memo1.Lines.Add(format('Samples: %d', [Wave.SampleCount]));
  memo1.Lines.Add(format('Length [s]: %f', [Wave.SampleCount / Wave.SampleRate]));
End;

End.


