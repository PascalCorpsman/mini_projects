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

  TFPoint = Record
    x, y: Single;
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
Function StftToBmp(Const Stft: TStftResult; Const bmp: TBitmap; MaxFrequency: Integer; Gradient: pTColor; IgnoreZeroElements: Boolean): TScaling;

(*
 * malt auf gegebenen Pixelkoordinaten im Spektrum, alles sehr Spooky aber geht ;)
 *)
Procedure PaintIntoStft(Var stft: TStftResult;
  x, y: Integer;
  radius: Integer;
  intensity: Single;
  MaxFrequency: integer
  );

Function ReconstructSignal(Const stft: TStftResult): TSignal;

(*
 * Kürzt Signal wieder so ein, dass seine Samples.re in [-1..1] passen
 * MaxVal = 0 => Automatisches Scaling, sonst wird mit 1/MaxVal scalliert
 *)
Procedure NormalizeSignal(Var aSignal: TSignal; MaxVal: Single = 0);

Implementation

Uses math;

Procedure NormalizeSignal(Var aSignal: TSignal; MaxVal: Single);
Var
  i: Integer;
Begin
  // Maximum der Samples bestimmen
  If maxVal = 0 Then Begin
    For i := 0 To aSignal.NumSamples - 1 Do Begin
      If Abs(aSignal.Samples[i].re) > maxVal Then Begin
        maxVal := Abs(aSignal.Samples[i].re);
      End;
    End;
  End;
  If maxVal = 0 Then maxVal := 1;
  For i := 0 To aSignal.NumSamples - 1 Do Begin
    aSignal.Samples[i].re := aSignal.Samples[i].re / maxVal;
  End;
End;

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

(* C# source https://youtu.be/08mmKNLQVHU?si=awNvvyid7AtpUwza 1:59

public static Signal GenerateSignal(FrequencyData[] spectrum, int sampleRate, int sampleCount)
{
  float[] samples = new float[sampeCount];
  float duration = sampleCount / (float)sampleRate;

  for (int i = 0; i < sampleCount; i++)
  {
    float timeNorm = i / (float)(samplecount); // [0, 1)
    float time = timeNorm * duration;

    foreach (FrequencyData wave in spectrum)
    {
      samples[i] += cos(time * TAU * wave.Frequency * wave.Phase) * wave.Amplitude;
    }
  }
  return new Signal(samples, sampleRate);
}
*)

Function GenerateSignal(Const spectrum: TFrequencyData; sampleRate, sampleCount: Integer): TSignal;
Var
  duration, time, timeNorm: Single;
  i, j: Integer;
Begin
  result.Samples := Nil;
  result.SampleRate := sampleRate;
  result.NumSamples := sampleCount;
  SetLength(result.samples, sampleCount);
  duration := sampleCount / sampleRate;

  For i := 0 To sampleCount - 1 Do Begin
    timeNorm := i / sampleCount;
    time := timeNorm * duration;
    result.samples[i].re := 0;
    result.samples[i].im := 0;
    For j := 0 To High(spectrum) Do Begin
      // Try to reduce cos calculations as much as possible ;)
      If abs(spectrum[j].Amplitude) > 1 Then Begin
        result.samples[i].re := result.samples[i].re + Cos(time * 2 * Pi * spectrum[j].Frequency + spectrum[j].Phase) * spectrum[j].Amplitude;
      End;
    End;
  End;
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
  MaxFrequency: Integer; Gradient: pTColor; IgnoreZeroElements: Boolean
  ): TScaling;
Const
  decibelsDisplayMin = -80; // z.B. -80 dB als untere Grenze
  decibelsDisplayMax = 0; // 0 dB als obere Grenze
Var
  AmplitudeMax: Single;

  Function GetNormalizedAmplitudeAt(time, freq: single): Byte;
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
    result := 255 - decibelsi;
  End;

Var
  width, height: SizeInt;
  i, j: Integer; // Koordinate in bmp, oder Zählschleifen..
  ii, jj: Float; // Koordinate in stft
  c: TColor;
  intf: TLazIntfImage;
  ci: byte;
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
      ci := GetNormalizedAmplitudeAt(ii, jj);
      If (ci < 255) Or (Not IgnoreZeroElements) Then Begin
        // Umrechnen des Skallierten DB Wertes in eine Farbe
        If assigned(Gradient) Then Begin
          c := Gradient[255 - ci];
        End
        Else Begin
          c := ci * $010101;
        End;
        intf.Colors[i, j] := TColorToFPColor(c);
      End;
    End;
  End;
  bmp.LoadFromIntfImage(intf);
  intf.Free;
  result.TimeScaling := width / (bmp.Width - 1);
  result.FrequencyScaling := height / (bmp.Height - 1);
End;

Procedure PaintIntoStft(Var stft: TStftResult; x, y: Integer; radius: Integer;
  intensity: Single; MaxFrequency: integer);
Var
  i, j: Integer;
  sqrdist, width, height, ii, jj, sqr_Radius: integer;
  amplitude: Single;
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
  sqr_Radius := radius * radius;
  For i := -radius To radius Do Begin
    ii := (x + i) * width Div 640;
    If (ii < 0) Or (ii > high(stft.Segments)) Then Continue;
    For j := -radius To radius Do Begin
      jj := (y + j) * height Div 480;
      If (jj < 0) Or (jj > high(stft.Segments[ii].Spectrum)) Then Continue;
      sqrdist := Sqr(i) + Sqr(j);
      If sqrdist <= sqr_Radius Then Begin
        amplitude := intensity * (1 - sqrt(sqrdist / sqr_radius));
        stft.Segments[ii].Spectrum[jj].Amplitude := max(0,
          stft.Segments[ii].Spectrum[jj].Amplitude + amplitude);
      End;
    End;
  End;
End;

(* C# source https://youtu.be/08mmKNLQVHU?si=awNvvyid7AtpUwza 16:00
public static Signal ReconstructSignal(StftResult stft)
{
  float[] allSamples = new float[stft.SampleCount];
  forech (StftSegment segment in stft.Segments)
  {
    float[] segmentSamples = Generate(segment.Spectrum, stft.SampleRate, segment.SampleCount);

    // Add reconstructed segment into full reconstruction
    for (int i = 0; i < segment.SampleCount; i++)
    {
      allSamples[i + segment.SampleOffset] += segmentSamples[i];
    }

  return new Signal(allSamples, stft.SampleRate);
}
*)

Function ReconstructSignal(Const stft: TStftResult): TSignal;
Var
  allSamples: Array Of Single;
  segmentSamples: TSignal;
  segment: TStftSegment;
  i, segIndex: Integer;
Begin
  (*
   * !! Achtung !!, dieser Code nimmt bei der Summierung das Hann Fenster nicht
   * Nicht richtig in betracht, es funktioniert nur, weil wir bei der Generierung
   * das Hann Fenster exakt um die hälfte verschoben haben !
   *)

  // Gesamtsamplepuffer
  allSamples := Nil;
  SetLength(allSamples, stft.SampleCount);

  // Initialisieren (wichtig bei Additionen)
  For i := 0 To High(allSamples) Do
    allSamples[i] := 0.0;

  // Alle Segmente überlagert addieren
  For segIndex := 0 To High(stft.Segments) Do Begin
    segment := stft.Segments[segIndex];

    segmentSamples := GenerateSignal(
      segment.Spectrum,
      stft.SampleRate,
      segment.SampleCount
      );

    For i := 0 To segment.SampleCount - 1 Do Begin
      If (i + segment.StartIndex) < Length(allSamples) Then
        allSamples[i + segment.StartIndex] := allSamples[i + segment.StartIndex] + segmentSamples.Samples[i].re;
    End;
  End;

  // Signal zusammenbauen
  Result.Samples := Nil;
  SetLength(Result.Samples, stft.SampleCount);
  For i := 0 To High(allSamples) Do Begin
    Result.Samples[i].re := allSamples[i];
    Result.Samples[i].im := 0.0;
  End;

  Result.SampleRate := stft.SampleRate;
  Result.NumSamples := stft.SampleCount;

End;


End.

