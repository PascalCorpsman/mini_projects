(******************************************************************************)
(* example PingPong                                                ??.??.???? *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Demonstrates a easy billard engine                           *)
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

{$MODE ObjFPC}{$H+}

Interface

Uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  elements, ExtCtrls;

Type
  TForm1 = Class(TForm)
    ApplicationEvents1: TApplicationProperties;
    Timer1: TTimer;
    Procedure ApplicationEvents1Idle(Sender: TObject; Var Done: Boolean);
    Procedure FormKeyDown(Sender: TObject; Var Key: Word;
      Shift: TShiftState);
    Procedure FormCreate(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure Timer1Timer(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

Const
  Friction = 0; // 0 = keine Reibung , 1 = 00 viel Reibung => Reibung mus in [0..1] sein

Var
  Form1: TForm1;
  Things: Array Of TBall;
  info: Boolean = true;
  infoc: Integer = 0;
  Hinderniss: Boolean = True;
  initialized: Boolean = false;
  bm: Tbitmap;

Implementation

{$R *.lfm}

Procedure Render;
Var
  i, j: Integer;
Begin
  (*
   * !! Attention !!
   * This demo does not take the time between 2 Render steps into account
   * -> This means the simulation is directly dependant to the FrameRate !
   *    If you plan to use this code in a real application you need to modify this !
   *)
  // Wenn noch nicht initialisiert ist passiert auch nichts
  If Not initialized Then exit;
  // Bildschirm löschen
  With bm.canvas Do Begin
    pen.color := clblack;
    brush.style := bssolid;
    brush.color := clblack;
    Rectangle(0, 0, bm.width, bm.height);
    If Hinderniss Then Begin
      brush.color := clwhite;
      pen.color := clwhite;
      Rectangle(form1.ClientWidth Div 2 - 20, form1.Clientheight Div 2 - 20, form1.ClientWidth Div 2 + 20, form1.Clientheight Div 2 + 20);
    End;
  End;

  // Rendern der einzelkugeln in den Tempscreen
  For i := 0 To High(Things) Do
    Things[i].render(bm.canvas);

  // Eine Kleine Info anzeigen
  If info Then
    With bm.canvas Do Begin
      font.color := clwhite;
      brush.style := bsclear;
      textout(10, 30, 'ESC = exit');
      textout(10, 50, '+ = increase speed');
      textout(10, 70, '- = decrease speed');
      Textout(10, 90, 'Return = toggle barrier');
    End;

  // Neuzeichnen des Formulars
  form1.canvas.Draw(0, 0, bm);

  // Bewegen der einzelnen Kugeln
  For i := 0 To High(Things) Do Begin
    Things[i].Move;
    (* Das hier ist neu *)
    Things[i].ApplyFriction(Friction);
  End;

  // Kollision der Kugeln untereinander
  For i := 0 To high(Things) Do
    For j := i + 1 To high(Things) Do
      Things[i].CollideWithOther(Things[j]);

  // Collision mit den Wänden.
  For i := 0 To High(Things) Do Begin
    Things[i].BorderCollision(form1.clientrect);
    If Hinderniss Then
      Things[i].BorderCollision(Rect(form1.ClientWidth Div 2 - 20, form1.Clientheight Div 2 - 20, form1.ClientWidth Div 2 + 20, form1.Clientheight Div 2 + 20), false);
  End;
End;

Procedure TForm1.ApplicationEvents1Idle(Sender: TObject;
  Var Done: Boolean);
Begin
  render;
  done := false;
End;

Procedure TForm1.FormKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Var
  i: integer;
  t: tfpoint;
Begin
  If Key = 27 Then Close;
  If Key = VK_add Then Begin
    For i := 0 To high(Things) Do
      If assigned(Things[i]) Then Begin
        t := Things[i].SpeedVektor;
        t.x := t.x * 2;
        t.y := t.y * 2;
        Things[i].SpeedVektor := t;
      End;
  End;
  If Key = VK_SUBTRACT Then Begin
    For i := 0 To high(Things) Do
      If assigned(Things[i]) Then Begin
        t := Things[i].SpeedVektor;
        t.x := t.x / 2;
        t.y := t.y / 2;
        Things[i].SpeedVektor := t;
      End;
  End;
  If Key = 13 Then Hinderniss := Not Hinderniss;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Var
  i, w, h: Integer;
Begin
  caption := 'This Sample was made by Corpsman Support : www.Corpsman.de';
  Randomize;
  bm := Tbitmap.create;
  bm.Width := ClientWidth;
  bm.Height := ClientHeight;
  bm.PixelFormat := pf24bit;
  setlength(Things, 10);
  For i := 0 To High(Things) Do Begin
    //    Things[i] := TBall.create(point(0, 0), point(0, 0), 20, 0);
    Things[i] := TBall.create(point(0, 0), point(0, 0), 40 + random(30), 0);
    Things[i].CalculateMass;
  End;
  w := width Div 5;
  h := height Div 4;
  Things[0].position := Point(w, h * 2);
  Things[1].Position := Point(w * 2, h);
  Things[2].Position := Point(w * 3, h);
  Things[3].Position := Point(w * 4, h);
  Things[4].Position := Point(w * 2, h * 2);
  Things[5].Position := Point(w * 3, h * 2);
  Things[6].Position := Point(w * 4, h * 2);
  Things[7].Position := Point(w * 2, h * 3);
  Things[8].Position := Point(w * 3, h * 3);
  Things[9].Position := Point(w * 4, h * 3);
  // Die Richtung für unseren StarBall setzen
  Things[0].SpeedVektor := Point(Cos((-45) * Pi / 180) / 4, sin(-45 * Pi / 180) / 4);
  Form1.Timer1.enabled := true;
  initialized := true;
End;

Procedure TForm1.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Var
  i: Integer;
Begin
  initialized := false;
  For i := 0 To High(Things) Do
    Things[i].free;
  bm.free;
End;

Procedure TForm1.Timer1Timer(Sender: TObject);
Begin
  inc(infoc);
  If infoc > 5 Then Begin
    info := false;
    timer1.enabled := false;
  End;
End;

End.

