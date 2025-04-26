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
Unit Unit9;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Buttons,
  upixelimage, uconvolute, upixeleditor_types;

Type

  { TForm9 }

  TForm9 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    ComboBox1: TComboBox;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure SpeedButton1Click(Sender: TObject);
  private
    Convolutions: TConvolutions;
    Procedure LoadConvolutionFile();
  public
    fBackupImage: TPixelImage;
    faImage: TPixelImage;
    Function Init(Const aImage: TPixelImage): Boolean;
  End;

Var
  Form9: TForm9;

Implementation

{$R *.lfm}

{ TForm9 }

Procedure TForm9.FormCreate(Sender: TObject);
Begin
  caption := 'Convolution ..';
  Convolutions := Nil;
  fBackupImage := TPixelImage.Create();
  faImage := Nil;
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
End;

Procedure TForm9.FormDestroy(Sender: TObject);
Begin
  fBackupImage.free;
  fBackupImage := Nil;
End;

Procedure TForm9.SpeedButton1Click(Sender: TObject);
Var
  Channels: TChannels;
Begin
  // Aktuelle Operationen Anwenden
  If ComboBox1.Items.Count = 0 Then exit;
  faImage.BeginUpdate;
  faImage.CloneFrom(fBackupImage);
  Channels := [];
  If SpeedButton1.Down Then Channels := Channels + [cR];
  If SpeedButton2.Down Then Channels := Channels + [cG];
  If SpeedButton3.Down Then Channels := Channels + [cB];
  faImage.Convolute(Convolutions[ComboBox1.ItemIndex], Channels);
  faImage.EndUpdate;
End;

Procedure TForm9.LoadConvolutionFile();
Var
  i: Integer;
Begin
  ComboBox1.items.BeginUpdate;
  ComboBox1.Items.Clear;
  Convolutions := FileToConvolutions('convmatr.def');
  For i := 0 To high(Convolutions) Do Begin
    ComboBox1.Items.Add(Convolutions[i].Name);
  End;
  ComboBox1.items.EndUpdate;
  If ComboBox1.Items.Count <> 0 Then ComboBox1.ItemIndex := 0;
End;

Function TForm9.Init(Const aImage: TPixelImage): Boolean;
Begin
  result := false;
  LoadConvolutionFile();
  If Not assigned(Convolutions) Then Begin
    showmessage('Error, could not load "convmatr.def"');
    exit;
  End;
  fBackupImage.CloneFrom(aImage);
  faImage := aImage;
  SpeedButton1Click(Nil);
  Result := true;
End;

End.

