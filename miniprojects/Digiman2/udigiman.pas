(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Digiman                                               *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit udigiman;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Graphics;

Const
  Grid = 4;

Type

  TState = (sOff, sOn, sOffToOn, sOnToOff, sUndefined);

  TDigiman = Class;

  (*
   * Ein Element welches in TDigiman verwaltet wird
   *)

  { TDigimanElement }

  TDigimanElement = Class
  private
    fOwner: TDigiman;
  protected
    Function getHeight: integer; virtual; // Abstract
    Function getWidth: integer; virtual; // Abstract
    Function LoadImage(aFilename: String): TBitmap; // Helper function for constructors
  public
    Top: integer;
    Left: integer;

    Property Width: integer read getWidth;
    Property Height: integer read getHeight;

    Constructor Create(); virtual;
    Destructor Destroy(); override;

    Procedure setPosition(aX, aY: Integer);
    Procedure RenderTo(Const Canvas: TCanvas); virtual; // Abstract

    Procedure Click; virtual;
    Function Clone: TDigimanElement; virtual abstract;
  End;

  (*
   * Der Eigentliche Emulator
   *)
  TDigiman = Class
  private
    fElements: Array Of TDigimanElement;

  public
    ShowPegel: Boolean;

    Constructor Create(); virtual;
    Destructor Destroy(); override;

    Procedure RenderTo(Const Canvas: TCanvas);

    Procedure AddElement(Const aElement: TDigimanElement);

    Function GetElementAtPos(x, y: integer): TDigimanElement;
  End;

  { TUserInput }

  TUserInput = Class(TDigimanElement)
  private
    fState: TState;
    fUpImage, fDownImage: TBitmap;
  protected
    Function getHeight: integer; override;
    Function getWidth: integer; override;

  public
    Constructor Create(); override;
    Destructor Destroy(); override;

    Procedure RenderTo(Const Canvas: TCanvas); override;

    Procedure Click; override;

    Function Clone: TDigimanElement; override;
  End;

  { TProbe }

  TProbe = Class(TDigimanElement)
  private
    fState: TState;
    fOnImage, fOffImage, fUnknownImage: TBitmap;
  protected
    Function getHeight: integer; override;
    Function getWidth: integer; override;
  public
    Constructor Create(); override;
    Destructor Destroy(); override;

    Procedure RenderTo(Const Canvas: TCanvas); override;

    Function Clone: TDigimanElement; override;
  End;

  { TNot }

  TNot = Class(TDigimanElement)
  private
    fImage: TBitmap;
  protected
    Function getHeight: integer; override;
    Function getWidth: integer; override;
  public
    Constructor Create(); override;
    Destructor Destroy(); override;

    Procedure RenderTo(Const Canvas: TCanvas); override;

    Function Clone: TDigimanElement; override;
  End;

  { TAnd }

  TAnd = Class(TDigimanElement)
  private
    fImage: TBitmap;
  protected
    Function getHeight: integer; override;
    Function getWidth: integer; override;
  public
    Constructor Create(); override;
    Destructor Destroy(); override;

    Procedure RenderTo(Const Canvas: TCanvas); override;

    Function Clone: TDigimanElement; override;
  End;

  { TOr }

  TOr = Class(TAnd)
  public
    Constructor Create(); override;

    Function Clone: TDigimanElement; override;
  End;

  { TNor }

  TNOr = Class(TAnd)
  public
    Constructor Create(); override;

    Function Clone: TDigimanElement; override;
  End;

  { TNand }

  TNand = Class(TAnd)
  public
    Constructor Create(); override;

    Function Clone: TDigimanElement; override;
  End;

Implementation

Procedure LineStateToCanvas(Const aCanvas: TCanvas; aPos: Tpoint; aState: TState);
Begin
  If aState In [sOffToOn, sUndefined, sOn] Then Begin
    acanvas.Pixels[aPos.x, apos.y] := clRed;
    acanvas.Pixels[aPos.x + 1, apos.y] := clRed;
    acanvas.Pixels[aPos.x, apos.y - 1] := clRed;
    acanvas.Pixels[aPos.x + 1, apos.y - 1] := clRed;
    acanvas.Pixels[aPos.x, apos.y - 4] := clRed;
    acanvas.Pixels[aPos.x + 1, apos.y - 4] := clRed;
    acanvas.Pixels[aPos.x, apos.y - 5] := clRed;
    acanvas.Pixels[aPos.x + 1, apos.y - 5] := clRed;
  End;
  If aState In [sOffToOn, sOn] Then Begin
    acanvas.Pixels[aPos.x, apos.y - 2] := clRed;
    acanvas.Pixels[aPos.x + 1, apos.y - 2] := clRed;
    acanvas.Pixels[aPos.x, apos.y - 3] := clRed;
    acanvas.Pixels[aPos.x + 1, apos.y - 3] := clRed;
  End;
End;

{ TDigimanElement }

Constructor TDigimanElement.Create;
Begin
  Inherited create;
  fOwner := Nil;
End;

Destructor TDigimanElement.Destroy;
Begin

End;

Function TDigimanElement.getHeight: integer;
Begin
  result := 0;
  Raise exception.Create(ClassName + '.getHeight not implemented.');
End;

Function TDigimanElement.getWidth: integer;
Begin
  result := 0;
  Raise exception.Create(ClassName + '.getWidth not implemented.');
End;

Function TDigimanElement.LoadImage(aFilename: String): TBitmap;
Begin
  result := TBitmap.Create;
  result.LoadFromFile('GFX' + PathDelim + aFilename);
  result.TransparentColor := clFuchsia;
  result.Transparent := true;
End;

Procedure TDigimanElement.setPosition(aX, aY: Integer);
Begin
  Left := (aX - Width Div 2);
  Top := (aY - Height Div 2);
  Left := Left - Left Mod Grid;
  Top := Top - Top Mod Grid;
End;

Procedure TDigimanElement.RenderTo(Const Canvas: TCanvas);
Begin
  Raise exception.Create(ClassName + '.Render not implemented.');
End;

Procedure TDigimanElement.Click;
Begin
  // Nichts, ..
End;

Constructor TDigiman.Create;
Begin
  Inherited Create();
  fElements := Nil;
  ShowPegel := true;
End;

Destructor TDigiman.Destroy;
Var
  i: Integer;
Begin
  For i := 0 To high(fElements) Do
    fElements[i].Free;
End;

Procedure TDigiman.RenderTo(Const Canvas: TCanvas);
Var
  i: Integer;
Begin
  For i := 0 To high(fElements) Do Begin
    fElements[i].RenderTo(Canvas);
  End;

End;

Procedure TDigiman.AddElement(Const aElement: TDigimanElement);
Begin
  aElement.fOwner := self;

  setlength(fElements, high(fElements) + 2);
  fElements[high(fElements)] := aElement;
End;

Function TDigiman.GetElementAtPos(x, y: integer): TDigimanElement;
Var
  i: Integer;
Begin
  result := Nil;
  For i := 0 To high(fElements) Do Begin
    If (fElements[i].Left <= x) And (fElements[i].Left + fElements[i].Width >= x) And
      (fElements[i].Top <= y) And (fElements[i].Top + fElements[i].Height >= y) Then Begin
      result := fElements[i];
      exit;
    End;
  End;
End;

{ TUserInput }

Function TUserInput.getHeight: integer;
Begin
  Result := fUpImage.Height;
End;

Function TUserInput.getWidth: integer;
Begin
  Result := fUpImage.Width;
End;

Constructor TUserInput.Create;
Begin
  fUpImage := LoadImage('Userinput_up.bmp');
  fDownImage := LoadImage('Userinput_down.bmp');
  fState := sOff;
End;

Destructor TUserInput.Destroy;
Begin
  fUpImage.Free;
  fDownImage.Free;
  fUpImage := Nil;
  fDownImage := Nil;
  Inherited Destroy();
End;

Procedure TUserInput.RenderTo(Const Canvas: TCanvas);
Begin
  Case fState Of
    sOff: canvas.Draw(Left, Top, fUpImage);
    sOn: canvas.Draw(Left, Top, fDownImage);
  End;
  If fOwner.ShowPegel Then Begin
    LineStateToCanvas(Canvas, point(left + 26, top + 7), fState);
  End;
End;

Procedure TUserInput.Click;
Begin
  Case fState Of
    sOff: fState := sOn;
    sOn: fState := sOff;
  End;
End;

Function TUserInput.Clone: TDigimanElement;
Begin
  result := TUserInput.Create();
End;

{ TProbe }

Function TProbe.getHeight: integer;
Begin
  Result := fOnImage.Height;
End;

Function TProbe.getWidth: integer;
Begin
  Result := fOnImage.Width;
End;

Constructor TProbe.Create;
Begin
  Inherited Create();
  fOnImage := LoadImage('Probe_on.bmp');
  fOffImage := LoadImage('Probe_off.bmp');
  fUnknownImage := LoadImage('Probe_undef.bmp');
  fState := sUndefined;
End;

Destructor TProbe.Destroy;
Begin
  fOffImage.free;
  fOnImage.free;
  fUnknownImage.Free;

  fOffImage := Nil;
  fOnImage := Nil;
  fUnknownImage := Nil;
  Inherited Destroy();
End;

Procedure TProbe.RenderTo(Const Canvas: TCanvas);
Begin
  Case fState Of
    sOff, sOnToOff: canvas.Draw(Left, Top, fOffImage);
    sOn, sOffToOn: canvas.Draw(Left, Top, fOnImage);
    sUndefined: canvas.Draw(Left, Top, fUnknownImage);
  End;
End;

Function TProbe.Clone: TDigimanElement;
Begin
  result := TProbe.Create();
End;

{ TNot }

Function TNot.getHeight: integer;
Begin
  Result := fImage.Height;
End;

Function TNot.getWidth: integer;
Begin
  Result := fImage.Width;
End;

Constructor TNot.Create();
Begin
  Inherited Create();
  fImage := LoadImage('Not.bmp');
End;

Destructor TNot.Destroy();
Begin
  fImage.Free;
  fImage := Nil;
  Inherited Destroy();
End;

Procedure TNot.RenderTo(Const Canvas: TCanvas);
Begin
  canvas.Draw(Left, Top, fImage);
End;

Function TNot.Clone: TDigimanElement;
Begin
  result := TNot.Create();
End;

{ TAnd }

Constructor TAnd.Create();
Begin
  Inherited Create();
  fImage := LoadImage('And.bmp');
End;

Destructor TAnd.Destroy();
Begin
  fImage.free;
  fImage := Nil;
  Inherited Destroy();
End;


Function TAnd.getHeight: integer;
Begin
  Result := fImage.Height;
End;

Function TAnd.getWidth: integer;
Begin
  Result := fImage.Width;
End;

Procedure TAnd.RenderTo(Const Canvas: TCanvas);
Begin
  canvas.Draw(Left, Top, fImage);
End;

Function TAnd.Clone: TDigimanElement;
Begin
  result := TAnd.Create();
End;

{ TOr }

Constructor TOr.Create();
Begin
  Inherited Create();
  fImage.free;
  fImage := LoadImage('Or.bmp');
End;

Function TOr.Clone: TDigimanElement;
Begin
  Result := TOr.Create();
End;

{ TNOr }

Constructor TNOr.Create();
Begin
  Inherited Create();
  fImage.free;
  fImage := LoadImage('Nor.bmp');
End;

Function TNOr.Clone: TDigimanElement;
Begin
  Result := TNor.Create();
End;

{ TNand }

Constructor TNand.Create();
Begin
  Inherited Create();
  fImage.free;
  fImage := LoadImage('Nand.bmp');
End;

Function TNand.Clone: TDigimanElement;
Begin
  Result := TNand.Create();
End;

End.

