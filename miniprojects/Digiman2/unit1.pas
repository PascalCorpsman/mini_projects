(******************************************************************************)
(* Digiman2                                                        31.01.2026 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Tool to simulate digital circuits                            *)
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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  udigiman;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    CheckBox1: TCheckBox;
    PaintBox1: TPaintBox;
    PaintBox2: TPaintBox;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    Procedure CheckBox1Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    Procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure PaintBox1Paint(Sender: TObject);
    Procedure PaintBox2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure PaintBox2Paint(Sender: TObject);
  private
    fSelector, fEngine: TDigiman;
    fMouseDownPos, fMouseMovePos: TPoint;
    fSelectedElement: TDigimanElement;
    fAdderElement: TDigimanElement;
  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);

  Procedure AddElementToSelector(Element: TDigimanElement; x: integer);
  Begin
    Element.setPosition(x, 32);
    fSelector.AddElement(Element);
  End;

Begin
  caption := 'Digiman2 ver. 0.01, by Corpsman, www.Corpsman.de';
  fEngine := TDigiman.Create;
  // Init Selector
  fSelector := TDigiman.Create;
  AddElementToSelector(TUserInput.Create(), 32);
  AddElementToSelector(TProbe.Create(), 32 + 50);
  AddElementToSelector(TNot.Create(), 32 + 100);
  AddElementToSelector(TOr.Create(), 32 + 150);
  AddElementToSelector(TAnd.Create(), 32 + 200);
  AddElementToSelector(TNOr.Create(), 32 + 250);
  AddElementToSelector(TNAnd.Create(), 32 + 300);

  fAdderElement := Nil;

  CheckBox1Click(Nil);
  PaintBox2.Invalidate;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  fEngine.Free;
  fEngine := Nil;
  fSelector.free;
  fSelector := Nil;
End;

Procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  e: TDigimanElement;
Begin
  fMouseDownPos := Point(x, y);
  fMouseMovePos := Point(x, y);
  fSelectedElement := Nil;
  If ssleft In shift Then Begin
    fSelectedElement := fEngine.GetElementAtPos(x, y);
    If assigned(fSelectedElement) Then exit;

    If assigned(fAdderElement) Then Begin
      e := fAdderElement.Clone;
      e.setPosition(x, y);
      fEngine.AddElement(e);
      PaintBox1.Invalidate;
    End;
  End;

  If ssright In shift Then Begin
    fAdderElement := Nil;
  End;
End;

Procedure TForm1.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
Begin
  If ssleft In shift Then Begin
    If assigned(fSelectedElement) Then Begin
      fSelectedElement.setPosition(x, y);
      PaintBox1.Invalidate;
    End;
  End;
  fMouseMovePos := Point(x, y);
  If assigned(fAdderElement) Then Begin
    PaintBox1.Invalidate;
  End;
End;

Procedure TForm1.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  If fMouseMovePos = fMouseDownPos Then Begin
    If Assigned(fSelectedElement) Then Begin
      fSelectedElement.Click;
      PaintBox1.Invalidate;
    End;
  End;
  fSelectedElement := Nil;
End;

Procedure TForm1.PaintBox1Paint(Sender: TObject);
Var
  at, al: Integer;
Begin
  PaintBox1.Canvas.brush.color := clWhite;
  PaintBox1.Canvas.Rectangle(-1, -1, PaintBox1.Width + 1, PaintBox1.Height + 1);
  fEngine.RenderTo(PaintBox1.Canvas);
  If Assigned(fAdderElement) Then Begin
    al := fAdderElement.Left;
    at := fAdderElement.Top;
    fAdderElement.setPosition(fMouseMovePos.X, fMouseMovePos.y);
    fAdderElement.RenderTo(PaintBox1.Canvas);
    fAdderElement.Left := al;
    fAdderElement.Top := at;
  End;
End;

Procedure TForm1.PaintBox2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  fAdderElement := fSelector.GetElementAtPos(x, y);
End;

Procedure TForm1.PaintBox2Paint(Sender: TObject);
Begin
  PaintBox2.Canvas.brush.color := clWhite;
  PaintBox2.Canvas.Rectangle(-1, -1, PaintBox2.Width + 1, PaintBox2.Height + 1);
  fSelector.RenderTo(PaintBox2.Canvas);
End;

Procedure TForm1.CheckBox1Click(Sender: TObject);
Begin
  fEngine.ShowPegel := CheckBox1.Checked;
  PaintBox1.Invalidate;
End;

End.

