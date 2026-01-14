(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Spectrum_analyzer                                     *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit uspectrum_analyzer;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Graphics, IntfGraphics, fftw_s, uwave;

Type
  TScaling = Record
    TimeScaling: Single;
    FrequencyScaling: Single;
  End;

  { Short-Time Fourier Transform (STFT) }

  Tcomplex_singleArray = Array Of complex_single; // This way we are directly Byte compatible to fftw ;)

  TSignal = Record
    Samples: Tcomplex_singleArray;
    SampleRate: integer;
    NumSamples: integer;
  End;

  TFrequencyDataItem = Record
    Frequency: Single; // in Hz
    Amplitude: Single; // Re part of FFT-component
    Phase: Single; // Im part of FFT-component in radiant
  End;

  TFrequencyData = Array Of TFrequencyDataItem;

  TStftSegment = Record
    Spectrum: TFrequencyData;
    StartIndex: Integer;
    SampleCount: Integer;
  End;

  TStftResult = Record
    Segments: Array Of TStftSegment;
    SampleRate: Integer;
    SampleCount: Integer;
  End;

  pTColor = ^TColor;

Function WaveToSignal(Const Wave: TWave): TSignal;

Function ComputeSTFT(Const signal: TSignal; segmentSize, hopSize: Integer; smoothWindow: Boolean): TStftResult;

(*
 * Wenn Gradient Definiert ist, dann muss Gradient 256 elemente enthalten !, sonst graustufen
 *)
Function StftToBmp(Const Stft: TStftResult; Const bmp: TBitmap; MaxFrequency: Integer; Gradient: pTColor): TScaling;

Implementation

Uses math;

Type
  TSpectrogramMap =
    Array Of // Dim 1 = segments (timeslices)
  Array Of // Dim 2 = frequences
  Single;

Function DFT_Compute(Const aSignals: Tcomplex_singleArray; aSampleRate: integer): TFrequencyData;
Var
  plan: fftw_plan_single;
  tmpResults: Tcomplex_singleArray;
  mag: ValReal;
  phase: Float;
  N, i: Integer;
Begin
  result := Nil;
  tmpResults := Nil;
  n := Length(aSignals);
  setlength(tmpResults, N);
  plan := fftw_plan_dft_1d(
    N,
    @aSignals[0].re,
    @tmpResults[0].re,
    fftw_forward,
    [fftw_estimate]
    );
  fftw_execute(plan);
  fftw_destroy_plan(plan);

  setlength(result, n);
  For i := 0 To N - 1 Do Begin
    mag := Sqrt(tmpResults[i].re * tmpResults[i].re + tmpResults[i].im * tmpResults[i].im);
    phase := ArcTan2(tmpResults[i].im, tmpResults[i].re);
    Result[i].Amplitude := mag;
    Result[i].Phase := phase;
    Result[i].Frequency := i * aSampleRate / N; // in Hz
  End;
End;

Function WaveToSignal(Const Wave: TWave): TSignal;
Var
  i: Integer;
Begin
  result.Samples := Nil;
  setlength(result.Samples, wave.SampleCount);
  For i := 0 To Wave.SampleCount - 1 Do Begin
    result.Samples[i].im := 0;
    result.Samples[i].re := Wave.Sample[0, i];
  End;
  result.SampleRate := Wave.SampleRate;
  result.NumSamples := Wave.SampleCount;
End;

//(* C# source https://youtu.be/08mmKNLQVHU?si=awNvvyid7AtpUwza 13:10
//public static void ApplyHannWindow(Span<float> samples)
//{
//  const float TAU = 2 * PI;
//
//  for (int i = 0; i < samples.Length; i++)
//  {
//    float t = i / (samples.Length -1f); // [0..1]
//    float smoothWindow = 0.5 * (1 - cos(t * TAU));
//    samples[i] *= smoothWindow;
//  }
//}
//*)

Procedure ApplyHannWindow(Var samples: Tcomplex_singleArray);
Const
  TAU = 2 * Pi;
Var
  i: Integer;
  t: Single;
  smoothWindow: Single;
  lenMinus1: Single;
Begin
  If Length(samples) < 2 Then
    Exit;

  lenMinus1 := Length(samples) - 1;

  For i := 0 To High(samples) Do Begin
    t := i / lenMinus1; // [0..1]
    smoothWindow := 0.5 * (1 - Cos(t * TAU));
    samples[i].re := samples[i].re * smoothWindow;
    samples[i].im := samples[i].im * smoothWindow;
  End;
End;

//(* C# source https://youtu.be/08mmKNLQVHU?si=awNvvyid7AtpUwza 4:36, with fix at 15:10
//public static StftResult ComputeSTFT(Signal signal, int segmentSize, int hopSize, bool smoothWindow)
//{
//  List<StftSegment> segments = new();
//
//  for (int offset = 0; offset < signal.NumSamples; offset += segmentSize)
//  {
//    // Get segment samples (taking care not to go out of bounds on last segment)
//    int numSamplesRemaining = signal.NumSamples - offset;
//    int segmentLength = Min(segmentSize, numSamplesRemaining);
//    Span<float> segmentSamples = signal.Samples.AsSpan(offset, segmentLength);
//    if (smoothWindow) ApplyHannWindow(segmentSamples);
//
//    // Run DFT (discrete fourier transform) to get frequency data for the current segment
//    FrequencyData[] spectrum = DFT.Compute(segmentSamples, signal.SampleRate);
//    StftSegment segment = new(spectrum, offset, segmentLength);
//    segments.add(segment)
//  }
//
//  return new StftResul(segments.ToArray(), signal.SampleRate, signal.NumSamples);
//}
//*)

Function ComputeSTFT(Const signal: TSignal; segmentSize, hopSize: Integer;
  smoothWindow: Boolean): TStftResult;
Var
  segments: Array Of TStftSegment;
  segmentCount: Integer;
  offset: Integer;
  numSamplesRemaining: Integer;
  segmentLength, i: Integer;
  segmentSamples: Tcomplex_singleArray;
  spectrum: TFrequencyData;
Begin
  segments := Nil;
  segmentSamples := Nil;
  SetLength(segments, (signal.NumSamples Div (hopSize)) + 1);
  segmentCount := 0;
  offset := 0;
  While offset < signal.NumSamples Do Begin
    // Get segment samples (taking care not to go out of bounds on last segment)
    numSamplesRemaining := signal.NumSamples - offset;
    segmentLength := Min(segmentSize, numSamplesRemaining);
    SetLength(segmentSamples, segmentSize);
    Move(signal.Samples[offset], segmentSamples[0], segmentLength * SizeOf(complex_single));
    // Das aller Letzte Fenster füllen wir mit 0en auf, sonst stimmen die "frequenzen" nachher nicht..
    If numSamplesRemaining < segmentSize Then Begin
      For i := numSamplesRemaining To segmentSize - 1 Do Begin
        segmentSamples[i].im := 0;
        segmentSamples[i].re := 0;
      End;
    End;
    If smoothWindow Then ApplyHannWindow(segmentSamples);

    // DFT berechnen
    spectrum := DFT_Compute(segmentSamples, signal.SampleRate);

    // Segment anhängen

    segments[segmentCount].Spectrum := spectrum;
    segments[segmentCount].StartIndex := offset;
    segments[segmentCount].SampleCount := segmentLength;
    Inc(segmentCount);

    Inc(offset, hopSize);
  End;
  SetLength(segments, segmentCount);

  // Ergebnis zusammenbauen
  Result.Segments := segments;
  Result.SampleRate := signal.SampleRate;
  Result.SampleCount := signal.NumSamples;
End;

(*
 * Stft ist im Prinzip ein 2D-Image, dessen Amplitude Werte
 * nun "passend" in das bmp Scalliert werden (Quasi eine Art Stretchdraw)
 * Dabei wird width umscalliert auf bmp.width
 * und MaxFrequency auf bmp.height
 *)

Function StftToBmp(Const Stft: TStftResult; Const bmp: TBitmap;
  MaxFrequency: Integer; Gradient: pTColor): TScaling;
Const
  decibelsDisplayMin = -80; // z.B. -80 dB als untere Grenze
  decibelsDisplayMax = 0; // 0 dB als obere Grenze
Var
  AmplitudeMax: Single;

  Function GetColorAt(time, freq: single): TColor;
  Var
    decibelsi, timei, freqi, timeip, freqip: Integer;
    decibels, Amplitude, a1, a2, times, freqs: Single;
  Begin
    result := 255;
    timei := trunc(time);
    times := 1 - (time - timei);
    freqi := trunc(freq);
    freqs := 1 - (freq - freqi);
    timei := min(timei, high(Stft.Segments));
    freqi := min(freqi, high(Stft.Segments[0].Spectrum));
    timeip := min(high(Stft.Segments), timei + 1);
    freqip := min(high(Stft.Segments[0].Spectrum), freqi + 1);
    // Bestimmen der Aktuellen Amplitude durch Biulineare Lineare Interpolation
    a1 := Stft.Segments[timei].Spectrum[freqi].Amplitude * freqs +
      Stft.Segments[timei].Spectrum[freqip].Amplitude * (1 - freqs);
    a2 := Stft.Segments[timeip].Spectrum[freqi].Amplitude * freqs +
      Stft.Segments[timeip].Spectrum[freqip].Amplitude * (1 - freqs);
    Amplitude := a1 * times + a2 * (1 - times);
    // Umrechnen der Amplitude in einen DB-Wert
    If amplitude <= 0 Then
      decibels := decibelsDisplayMin // avoid log10(0)
    Else
      decibels := 20 * Log10(amplitude / amplitudeMax);
    // Umskallieren des DB-Werts auf [0..255]
    decibelsi := round(255 * (decibels - decibelsDisplayMin) / (decibelsDisplayMax - decibelsDisplayMin));
    If decibelsi < 0 Then decibelsi := 0;
    If decibelsi > 255 Then decibelsi := 255;
    // Umrechnen des Skallierten DB Wertes in eine Farbe
    If assigned(Gradient) Then Begin
      result := Gradient[decibelsi];
    End
    Else Begin
      decibelsi := 255 - decibelsi;
      result := decibelsi * $010101;
    End;
  End;

Var
  width, height: SizeInt;
  i, j: Integer; // Koordinate in bmp, oder Zählschleifen..
  ii, jj: Float; // Koordinate in stft
  c: TColor;
  intf: TLazIntfImage;
Begin
  width := Length(stft.Segments);
  If width = 0 Then exit;
  height := -1;
  For i := 0 To high(stft.Segments[0].Spectrum) Do Begin
    If stft.Segments[0].Spectrum[i].Frequency <= MaxFrequency Then Begin
      height := i;
    End
    Else Begin
      break;
    End;
  End;
  If height = -1 Then exit;

  // Maximalwert der Amplitude berechnen
  amplitudeMax := 0;
  For i := 0 To width - 1 Do Begin
    For j := 0 To height - 1 Do Begin
      amplitudeMax := max(amplitudeMax, stft.Segments[i].Spectrum[j].Amplitude);
    End;
  End;

  // Das Rechteck Width, height wird nun auf bmp "aufgezogen"
  // Wir tasten im BMP-Raum ab ;)
  intf := bmp.CreateIntfImage;
  For i := 0 To bmp.Width - 1 Do Begin
    ii := i * width / (bmp.Width - 1);
    For j := 0 To bmp.Height - 1 Do Begin
      jj := (bmp.Height - 1 - j) * height / (bmp.Height - 1); // y-Achse ist invertiert ..
      c := GetColorAt(ii, jj);
      intf.Colors[i, j] := TColorToFPColor(c);
    End;
  End;
  bmp.LoadFromIntfImage(intf);
  intf.Free;
  result.TimeScaling := width / (bmp.Width - 1);
  result.FrequencyScaling := height / (bmp.Height - 1);
End;

End.

