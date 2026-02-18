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
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    OpenDialog1: TOpenDialog;
    PaintBox1: TPaintBox;
    PaintBox2: TPaintBox;
    SaveDialog1: TSaveDialog;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    ScrollBar3: TScrollBar;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure CheckBox1Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure PaintBox1MouseEnter(Sender: TObject);
    Procedure PaintBox1MouseLeave(Sender: TObject);
    Procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    Procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure PaintBox1Paint(Sender: TObject);
    Procedure PaintBox2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure PaintBox2Paint(Sender: TObject);
    Procedure ScrollBar2Change(Sender: TObject);
    Procedure ScrollBar3Change(Sender: TObject);
  private
    defcaption: String;
    fFormShowOnce: Boolean;
    fSelector, fEngine: TDigiman;
    fMouseIsIn: Boolean;
    fMouseDownPos, fMouseMovePos: TPoint;
    fSelectedElement: TDigimanElement;
    fAdderElement: TDigimanElement;
    fLineCreateHelper: TLineCreateHelper;
  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);

  Procedure AddElementToSelector(Element: TDigimanElement; x, y: integer);
  Begin
    Element.setPosition(x, 32 + y);
    fSelector.AddElement(Element);
  End;

Var
  i: Integer;
  ut: TUserText;
Begin
  defcaption := 'Digiman2 ver. 0.01, by Corpsman, www.Corpsman.de';
  caption := defcaption;
  fFormShowOnce := true;
  fEngine := TDigiman.Create;
  // Init Selector
  fSelector := TDigiman.Create;
  fSelector.ShowPegel := false;
  fSelector.ShowConnectionPoints := false;
  // Die Basik Elemente
  // TODO: Dauer "0", Dauer "1"
  AddElementToSelector(TUserInput.Create(), 32, 0);
  AddElementToSelector(TProbe.Create(), 32 + 50, 0);
  AddElementToSelector(TNot.Create(), 32 + 100, 0);
  AddElementToSelector(TOr.Create(), 32 + 150, 0);
  AddElementToSelector(TAnd.Create(), 32 + 200, 0);
  AddElementToSelector(TNOr.Create(), 32 + 250, 0);
  AddElementToSelector(TNAnd.Create(), 32 + 300, 0);
  ut := TUserText.Create();
  ut.Text := 'Text';
  AddElementToSelector(ut, 32 + 350, -8);
  // TODO: Flip Flops (RS, T, D ..)

  // TODO: Multiplexer, 7-Segment anzeige
  AddElementToSelector(THalfAdder.Create(), 32, PaintBox2.Height * 2);
  AddElementToSelector(TFullAdder.Create(), 32 + 50, PaintBox2.Height * 2);

  // TODO: Taktgenerator

  // Die Controlls die immer da sind
  For i := 0 To ScrollBar3.Max Do Begin
    AddElementToSelector(TLineTool.Create(), PaintBox2.Width - 32 - 8, PaintBox2.Height * i - 16);
    AddElementToSelector(TEraser.Create(), PaintBox2.Width - 16, PaintBox2.Height * i - 16);
  End;

  fAdderElement := Nil;
  fLineCreateHelper := TLineCreateHelper.Create;
  CheckBox1Click(Nil);
  PaintBox2.Invalidate;
End;

Procedure TForm1.FormShow(Sender: TObject);
Begin
  If Not fFormShowOnce Then exit;
  fFormShowOnce := false;
  (*// Debug "remove"
  If FileExists('First_save.ckt') Then Begin
    fEngine.LoadFromFile('First_save.ckt');
    PaintBox1.Invalidate;
  End;
  // End -- Debug *)
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  fEngine.Free;
  fEngine := Nil;
  fSelector.free;
  fSelector := Nil;
  fLineCreateHelper.free;
End;

Procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  e: TDigimanElement;
  InIndex, OutIndex: integer;
Begin
  fMouseDownPos := Point(x, y);
  fMouseMovePos := Point(x, y);
  fSelectedElement := Nil;
  If ssleft In shift Then Begin
    fSelectedElement := fEngine.GetElementAtPos(x + ScrollBar1.Position, y + ScrollBar2.Position);

    If assigned(fAdderElement) And (fAdderElement Is TTool) Then Begin
      // Das "Lösch" Tool
      If (fAdderElement Is TEraser) And (assigned(fSelectedElement)) Then Begin
        fEngine.DelElement(fSelectedElement);
        fSelectedElement := Nil; // Das Element gibt es nun ja nicht mehr ..
        PaintBox1.Invalidate;
      End;
      // Das Linientool
      If (fAdderElement Is TLineTool) And (assigned(fSelectedElement)) Then Begin
        If fSelectedElement.InOutPointHit(x + ScrollBar1.Position, y + ScrollBar2.Position, InIndex, OutIndex) Then Begin
          // Die Linie Startet oder endet
          If fLineCreateHelper.Mode = lcmIdle Then Begin
            // Eine Neue Linie wird erstellt
            fLineCreateHelper.StartNewLine(fSelectedElement, InIndex, OutIndex);
          End
          Else Begin
            // Eine Linie wird Beendet
            If Not fLineCreateHelper.EndLine(fSelectedElement, InIndex, OutIndex) Then Begin
              showmessage('Error, connection not allowed.');
            End;
          End;
        End;
        fSelectedElement := Nil;
        PaintBox1.Invalidate;
        exit;
      End;
      // Eine Linie wird erstellt und gerade "gezogen"
      If fLineCreateHelper.Mode <> lcmIdle Then Begin
        fLineCreateHelper.AddCorner(x + ScrollBar1.Position, y + ScrollBar2.Position);
        PaintBox1.Invalidate;
      End;
      exit;
    End;

    // Wir haben auf ein Existierendes Element geklickt, dieses aber nicht mit den Tools
    // Bearbeitet -> raus hier
    If assigned(fSelectedElement) Then exit;

    // Wir haben ins Leere geklickt und wollen ein neues Element einfügen
    If assigned(fAdderElement) Then Begin
      e := fAdderElement.Clone;
      e.setPosition(x + ScrollBar1.Position, y + ScrollBar2.Position);
      fEngine.AddElement(e);
      PaintBox1.Invalidate;
    End;
  End;

  If ssright In shift Then Begin
    // Abwahl des aktuellen Elementes
    If fAdderElement Is TLineTool Then Begin
      // ggf. Revert des letzten Liniensegmentes
      If fLineCreateHelper.Mode <> lcmIdle Then Begin
        fLineCreateHelper.DelLastCorner;
        PaintBox1.Invalidate;
        exit;
      End;
    End;
    // Löschen des Aktuellen Tools
    fAdderElement := Nil;
    PaintBox1.Cursor := crDefault;
    fEngine.ShowConnectionPoints := false;

    PaintBox1.Invalidate;
  End;
End;

Procedure TForm1.PaintBox1MouseEnter(Sender: TObject);
Begin
  fMouseIsIn := true;
  PaintBox1.Invalidate;
End;

Procedure TForm1.PaintBox1MouseLeave(Sender: TObject);
Begin
  fMouseIsIn := false;
  PaintBox1.Invalidate;
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
  If fLineCreateHelper.Mode <> lcmIdle Then Begin
    fLineCreateHelper.SetActualMousePosition(x + ScrollBar1.Position, y + ScrollBar2.Position);
    PaintBox1.Invalidate;
  End;
End;

Procedure TForm1.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  If Assigned(fSelectedElement) Then Begin
    If fMouseMovePos = fMouseDownPos Then Begin
      fSelectedElement.Click;
    End
    Else Begin
      fEngine.RecalculateShortConnections;
    End;
    PaintBox1.Invalidate;
  End;
  fSelectedElement := Nil;
End;

Procedure TForm1.PaintBox1Paint(Sender: TObject);
Var
  at, al: Integer;
Begin
  PaintBox1.Canvas.brush.color := clWhite;
  PaintBox1.Canvas.Rectangle(-1, -1, PaintBox1.Width + 1, PaintBox1.Height + 1);
  // Manchmal brauchen die Signale ein bisschen biss sie "durchgerechnet" sind
  // Lässt man die Engine 2 mal hinter einander rechnen scheint das das ganze zu beheben
  // Sauber fühlt sich das aber nicht an ...
  fEngine.RenderTo(PaintBox1.Canvas, point(ScrollBar1.Position, ScrollBar2.Position));
  fEngine.RenderTo(PaintBox1.Canvas, point(ScrollBar1.Position, ScrollBar2.Position));
  // Preview des hinzu zu fügenden Elementes
  If Assigned(fAdderElement) And (fMouseIsIn) Then Begin
    al := fAdderElement.Left;
    at := fAdderElement.Top;
    // Das Linientool wird "geschiftet" gezeichnet, so dass Links Unten des Bildes = Mouseposition ist.
    If fAdderElement Is TLineTool Then Begin
      // Das Linetool soll explizit nicht über die Rasterung Gerendert werden !!
      //fAdderElement.setPosition(fMouseMovePos.X + fAdderElement.Width Div 2, fMouseMovePos.y - fAdderElement.Height Div 2);
      // Also machen wir das hier von "Hand"
      fAdderElement.left := fMouseMovePos.X;
      fAdderElement.Top := fMouseMovePos.y - fAdderElement.Height;
    End
    Else Begin
      fAdderElement.setPosition(fMouseMovePos.X, fMouseMovePos.y);
    End;
    fAdderElement.RenderTo(PaintBox1.Canvas, point(0, 0));
    fAdderElement.Left := al;
    fAdderElement.Top := at;
  End;
  If fLineCreateHelper.Mode <> lcmIdle Then Begin
    fLineCreateHelper.RenderTo(PaintBox1.Canvas, point(ScrollBar1.Position, ScrollBar2.Position));
  End;
End;

Procedure TForm1.PaintBox2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  fAdderElement := fSelector.GetElementAtPos(x, y + ScrollBar3.Position * PaintBox2.Height);
  If assigned(fAdderElement) Then Begin
    PaintBox1.Cursor := crNone;
  End
  Else Begin
    PaintBox1.Cursor := crDefault;
  End;
  If fAdderElement Is TLineTool Then Begin
    fEngine.ShowConnectionPoints := true;
    PaintBox1.Invalidate;
  End
  Else Begin
    If fEngine.ShowConnectionPoints Then Begin
      fEngine.ShowConnectionPoints := false;
      PaintBox1.Invalidate;
    End;
  End;
End;

Procedure TForm1.PaintBox2Paint(Sender: TObject);
Begin
  PaintBox2.Canvas.brush.color := clWhite;
  PaintBox2.Canvas.Rectangle(-1, -1, PaintBox2.Width + 1, PaintBox2.Height + 1);
  fSelector.RenderTo(PaintBox2.Canvas, point(0, ScrollBar3.Position * PaintBox2.Height));
End;

Procedure TForm1.ScrollBar2Change(Sender: TObject);
Begin
  PaintBox1.Invalidate;
End;

Procedure TForm1.ScrollBar3Change(Sender: TObject);
Begin
  PaintBox2.Invalidate;
End;

Procedure TForm1.CheckBox1Click(Sender: TObject);
Begin
  fEngine.ShowPegel := CheckBox1.Checked;
  PaintBox1.Invalidate;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  // Clear
  ScrollBar1.Position := 0;
  ScrollBar2.Position := 0;
  fEngine.clear;
  PaintBox1.Invalidate;
  caption := defcaption;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  If SaveDialog1.Execute Then Begin
    fEngine.SaveToFile(SaveDialog1.FileName);
    caption := ExtractFileName(SaveDialog1.FileName);
  End;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  If OpenDialog1.Execute Then Begin
    caption := ExtractFileName(OpenDialog1.FileName);
    fEngine.LoadFromFile(OpenDialog1.FileName);
    PaintBox1.Invalidate;
  End;
End;

End.

