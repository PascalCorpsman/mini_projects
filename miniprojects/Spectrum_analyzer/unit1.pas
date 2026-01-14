(******************************************************************************)
(* Spectrum_analyzer                                               14.01.2026 *)
(*                                                                            *)
(* Version     : 0.02                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Tool to see and edit frequency spectres of .wav files.       *)
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
(*               0.02 - Enable Edit                                           *)
(*                                                                            *)
(******************************************************************************)
(*
 * Linux users need to install:
 * sudo aptitude install libfftw3-dev
 *
 * Windows users neet do download and install fftw from
 * https://www.fftw.org/
 *)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, uspectrum_analyzer;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ScrollBar1: TScrollBar;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure CheckBox1Change(Sender: TObject);
    Procedure CheckBox3Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
  private
    WaveLoaded: Boolean; // True, wenn WaveStft gültig ist.
    OverlayStft, WaveStft: TStftResult;
    Scaling: TScaling;
    MaxAmplitude: Single;
    SpectrumPreview: TBitmap;
    MaxVisHz: Integer;
    GradientColors: Array[0..255] Of TColor;
    Function CursorToFPoint(x, y: integer): TFPoint;
    Procedure RefreshPreview;
    Function GetMaxAmplitude(): Single;
  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses math
  , uWave
  , Unit2
  , Unit3
  ;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Var
  G: TBitmap;
  i: Integer;
Begin
  caption := 'Spectrum analyzer ver. 0.01 by Corpsman, www.Corpsman.de';
  SpectrumPreview := TBitmap.Create;
  SpectrumPreview.Width := Image1.Width;
  SpectrumPreview.Height := Image1.Height;
  label4.caption := '';
  label5.caption := '';
  label6.caption := '';

  // Preload Gradient Colors
  G := TBitmap.Create;
  G.LoadFromFile('gradient.bmp');
  For i := 0 To 255 Do Begin
    GradientColors[i] := g.Canvas.Pixels[i, 0];
  End;
  G.free;
  WaveLoaded := false;
End;

Procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  If CheckBox2.Checked Then Begin
    Image1MouseMove(sender, shift, x, y);
  End;
End;

Procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
Var
  p: TFPoint;
  amp: Single;
Begin
  // Anzeigen "wo" sich der Cursor gerade befindet
  If WaveLoaded Then Begin
    p := CursorToFPoint(x, y);
    label6.caption := format('(%0.2fs, %0.2fHz)', [p.x, p.y]);
  End;
  If CheckBox2.Checked Then Begin
    // Edit Mode
    If (ssleft In shift) Or (ssRight In Shift) Then Begin
      If ssleft In shift Then Begin
        amp := MaxAmplitude * 0.05;
      End
      Else Begin
        amp := -MaxAmplitude * 0.05;
      End;
      If CheckBox3.Checked Then Begin
        PaintIntoStft(OverlayStft, x, Image1.Height - 1 - y, ScrollBar1.Position, amp, MaxVisHz);
      End
      Else Begin
        PaintIntoStft(WaveStft, x, Image1.Height - 1 - y, ScrollBar1.Position, amp, MaxVisHz);
      End;
      RefreshPreview;
    End;
  End;
End;

Function TForm1.CursorToFPoint(x, y: integer): TFPoint;
Var
  Duration: Single;
Begin
  If Not waveloaded Then Begin
    result.x := 0;
    result.y := 0;
    exit;
  End;
  // 0 .. image1.Width -1 -> 0 .. Duration;
  Duration := WaveStft.SampleCount / WaveStft.SampleRate;
  result.x := (x / Image1.Width) * Duration;
  y := Image1.Height - y - 1;
  result.y := (y / Image1.Height) * MaxVisHz;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  SpectrumPreview.free;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  wave: TWave;
  SegmentSize: Integer;
Begin
  // Load Wave
  // OpenDialog1.FileName := 'punctual_thunderstorm.wav';
  // OpenDialog1.FileName := 'swimming_pool.wav';
  // OpenDialog1.FileName := 'Out.wav';
  If OpenDialog1.Execute Then Begin
    wave := TWave.Create;
    If Not wave.LoadFromFile(OpenDialog1.FileName) Then Begin
      wave.free;
      showmessage('Error, unable to load: ' + OpenDialog1.FileName);
      exit;
    End;
    form2.InitWith(wave);
    If form2.ShowModal <> mrOK Then Begin
      wave.free;
      exit;
    End;
    label5.caption := ExtractFileName(OpenDialog1.FileName);
    label7.Visible := false;
    // 1. das Spektrum Berechnen
    SegmentSize := form2.SegmentSize;
    MaxVisHz := form2.CutFrequency;
    WaveStft := ComputeSTFT(WaveToSignal(wave), SegmentSize, SegmentSize Div 2, true);
    WaveLoaded := true;
    // 2. das Spektrum Visualisieren ;)
    RefreshPreview;
    wave.free;
    CheckBox2.Checked := false; // do not Edit by default
    CheckBox3.Checked := false; // no overlay mode
    MaxAmplitude := GetMaxAmplitude();
  End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Var
  wave: TWave;
  i: Integer;
  SegmentSize: Integer;
Begin
  // Create Empty Board
  If Form3.ShowModal <> mrOK Then exit;
  wave := TWave.Create;
  wave.InitNewBuffer(1, form3.SampleRate, 16, round((form3.DurationInMs * form3.SampleRate) / 1000));
  form2.InitWith(wave);
  If form2.ShowModal <> mrOK Then Begin
    wave.free;
    exit;
  End;
  label5.caption := '';
  label7.Visible := false;
  // We start with silence
  For i := 0 To wave.SampleCount - 1 Do Begin
    wave.Sample[0, i] := 0;
  End;
  // 1. das Spektrum Berechnen
  SegmentSize := form2.SegmentSize;
  MaxVisHz := form2.CutFrequency;
  WaveStft := ComputeSTFT(WaveToSignal(wave), SegmentSize, SegmentSize Div 2, true);
  WaveLoaded := true;
  // 2. das Spektrum Visualisieren ;)
  RefreshPreview;
  wave.free;
  CheckBox2.Checked := true;
  CheckBox3.Checked := false; // no overlay mode
  MaxAmplitude := 2000; // Was auch immer hier der beste wert ist ... ?
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  // Erhöhe sichtbare Frequenzen um 500hz
  MaxVisHz := MaxVisHz + 500;
  RefreshPreview;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Begin
  // Verringere sichtbare Frequenzen um 500hz
  MaxVisHz := MaxVisHz - 500;
  RefreshPreview;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Var
  sig: TSignal;
  wave: TWave;
  i: Integer;
Begin
  // SaveDialog1.FileName := 'Out.wav';
  If SaveDialog1.Execute Then Begin
    If CheckBox3.Checked Then Begin
      sig := ReconstructSignal(OverlayStft);
    End
    Else Begin
      sig := ReconstructSignal(WaveStft);
    End;
    // TODO: Noch Raus kriegen womit hier "idealerweise" scalliert werden muss
    NormalizeSignal(sig, WaveStft.Segments[0].SampleCount);
    wave := TWave.Create;
    wave.InitNewBuffer(1, WaveStft.SampleRate, 16, WaveStft.SampleCount);
    For i := 0 To sig.NumSamples - 1 Do Begin
      wave.Sample[0, i] := sig.Samples[i].re;
    End;
    wave.SaveToFile(SaveDialog1.FileName);
    wave.free;
    showmessage('done.');
  End;
End;

Procedure TForm1.CheckBox1Change(Sender: TObject);
Begin
  RefreshPreview;
End;

Procedure TForm1.CheckBox3Click(Sender: TObject);
Var
  i, j: Integer;
Begin
  If Not WaveLoaded Then Begin
    showmessage('Error, nothing loaded, please load something first.');
    exit;
  End;
  If CheckBox3.Checked Then Begin
    showmessage('Overlay will be created, image will be grayscale, overlay in colors.' + LineEnding +
      'save .wav button will only use overlay.');
    CheckBox1.Checked := false;
    CheckBox2.Checked := true;
    // Clone WaveStft to OverlayStft, but "empty"
    OverlayStft.SampleRate := WaveStft.SampleRate;
    OverlayStft.SampleCount := WaveStft.SampleCount;
    setlength(OverlayStft.Segments, length(WaveStft.Segments));
    For i := 0 To high(WaveStft.Segments) Do Begin
      OverlayStft.Segments[i].SampleCount := WaveStft.Segments[i].SampleCount;
      OverlayStft.Segments[i].StartIndex := WaveStft.Segments[i].StartIndex;
      setlength(OverlayStft.Segments[i].Spectrum, length(WaveStft.Segments[i].Spectrum));
      For j := 0 To high(WaveStft.Segments[i].Spectrum) Do Begin
        OverlayStft.Segments[i].Spectrum[j].Frequency := WaveStft.Segments[i].Spectrum[j].Frequency;
        OverlayStft.Segments[i].Spectrum[j].Amplitude := 0;
        OverlayStft.Segments[i].Spectrum[j].Phase := 0;
      End;
    End;
    RefreshPreview;
  End;
End;

Procedure TForm1.RefreshPreview;
Begin
  If CheckBox3.Checked Then Begin
    Scaling := StftToBmp(WaveStft, SpectrumPreview, MaxVisHz, Nil, false);
    StftToBmp(OverlayStft, SpectrumPreview, MaxVisHz, @GradientColors[0], true);
  End
  Else Begin
    If CheckBox1.Checked Then Begin
      Scaling := StftToBmp(WaveStft, SpectrumPreview, MaxVisHz, @GradientColors[0], false);
    End
    Else Begin
      Scaling := StftToBmp(WaveStft, SpectrumPreview, MaxVisHz, Nil, false);
    End;
  End;
  Image1.Picture.Assign(SpectrumPreview);
  label2.Caption := format('%d Hz', [MaxVisHz]);
  label4.caption := format('%0.2fs', [WaveStft.SampleCount / WaveStft.SampleRate]);
End;

Function TForm1.GetMaxAmplitude(): Single;
Var
  i, j: Integer;
Begin
  Result := 0;
  For i := 0 To high(WaveStft.Segments) Do Begin
    For j := 0 To high(WaveStft.Segments[0].Spectrum) Do Begin
      Result := max(Result, WaveStft.Segments[i].Spectrum[j].Amplitude);
    End;
  End;
End;

End.

