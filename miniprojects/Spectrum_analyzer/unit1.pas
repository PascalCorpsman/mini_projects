(******************************************************************************)
(* Spectrum_analyzer                                               14.01.2026 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
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
    CheckBox1: TCheckBox;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    OpenDialog1: TOpenDialog;
    Procedure Button1Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure CheckBox1Change(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure Image1MouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer
      );
  private
    WaveLoaded: Boolean; // True, wenn WaveStft gültig ist.
    WaveStft: TStftResult;
    Scaling: TScaling;

    SpectrumPreview: TBitmap;
    MaxVisHz: Integer;
    GradientColors: Array[0..255] Of TColor;
    Procedure RefreshPreview;
  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses uWave, Unit2;

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

Procedure TForm1.Image1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
Var
  atime, aHz, Duration: Single;
Begin
  // Anzeigen "wo" sich der Cursor gerade befindet
  If WaveLoaded Then Begin
    // 0 .. image1.Width -1 -> 0 .. Duration;
    Duration := WaveStft.SampleCount / WaveStft.SampleRate;
    atime := (x / Image1.Width) * Duration;
    y := Image1.Height - y - 1;
    aHz := (y / Image1.Height) * MaxVisHz;
    label6.caption := format('(%0.2fs, %0.2fHz)', [atime, ahz]);
  End;
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
  //  OpenDialog1.FileName := 'punctual_thunderstorm.wav';
  //  OpenDialog1.FileName := 'swimming_pool.wav';
  If OpenDialog1.Execute Then
  Begin
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
  End;
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

Procedure TForm1.CheckBox1Change(Sender: TObject);
Begin
  RefreshPreview;
End;

Procedure TForm1.RefreshPreview;
Begin
  If CheckBox1.Checked Then Begin
    Scaling := StftToBmp(WaveStft, SpectrumPreview, MaxVisHz, @GradientColors[0]);
  End
  Else Begin
    Scaling := StftToBmp(WaveStft, SpectrumPreview, MaxVisHz, Nil);
  End;
  Image1.Picture.Assign(SpectrumPreview);
  label2.Caption := format('%d Hz', [MaxVisHz]);
  label4.caption := format('%0.2fs', [WaveStft.SampleCount / WaveStft.SampleRate]);
End;



End.





