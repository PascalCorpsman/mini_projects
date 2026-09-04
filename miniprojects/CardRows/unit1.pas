(******************************************************************************)
(* Cardrows                                                        04.09.2026 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Calculate startcard order for a given target sequence        *)
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
  uplayingcard;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Label1: TLabel;
    RadioGroup1: TRadioGroup;
    ScrollBox1: TScrollBox;
    Procedure FormCreate(Sender: TObject);
    Procedure FormResize(Sender: TObject);
    Procedure RadioGroup1Click(Sender: TObject);
  private
    fCards: Array Of TPlayingCard;
    tTargetOrder: Array Of TPlayingCard;
    fTargetCards: Array Of TPlayingCard;
    Procedure OnPreviewCardClick(Sender: TObject);
    Procedure RecalculateTargetCards;
    Procedure ClearTargetCards;
  public
    Procedure RecreatePreviewCards;
  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  caption := 'Cardrows ver. 0.01 by Corpsman, www.Corpsman.de';
  RadioGroup1.ItemIndex := 0;
  fCards := Nil;
  tTargetOrder := Nil;
  fTargetCards := Nil;
  RecreatePreviewCards();
End;

Procedure TForm1.FormResize(Sender: TObject);
Begin
  If Not assigned(fCards) Then exit;
  ScrollBox1.Left := Scale96ToForm(8);
  ScrollBox1.Width := ClientWidth - Scale96ToForm(2 * 8);
  ScrollBox1.Top := fCards[high(fCards)].Top + fCards[high(fCards)].Height + Scale96ToForm(8);
  ScrollBox1.Height := ClientHeight - ScrollBox1.Top - Scale96ToForm(8);
  label1.top := fCards[high(fCards)].Height + Scale96ToForm(8);
End;

Procedure TForm1.RadioGroup1Click(Sender: TObject);
Begin
  RecreatePreviewCards;
End;

Procedure TForm1.OnPreviewCardClick(Sender: TObject);
Begin
  setlength(tTargetOrder, high(tTargetOrder) + 2);
  tTargetOrder[high(tTargetOrder)] := TPlayingCard.Create(ScrollBox1);
  tTargetOrder[high(tTargetOrder)].Name := 'TargetCard' + inttostr(high(tTargetOrder));
  tTargetOrder[high(tTargetOrder)].Parent := ScrollBox1;
  tTargetOrder[high(tTargetOrder)].Top := Label1.Top + Label1.Height + Scale96ToForm(8);
  tTargetOrder[high(tTargetOrder)].Left := Scale96ToForm(8 + high(tTargetOrder) * (tTargetOrder[high(tTargetOrder)].Width + 8));
  tTargetOrder[high(tTargetOrder)].FaceIndex := TPlayingCard(sender).FaceIndex;
  RecalculateTargetCards;
End;

Procedure TForm1.RecalculateTargetCards;
Var
  aIndex: integer;

  Function getNextFreeIndex: integer;
  Begin
    result := aIndex;
    // First Hit = "empty"
    While assigned(fTargetCards[result]) Do Begin
      result := (result + 1) Mod length(fTargetCards);
    End;
    result := (result + 1) Mod length(fTargetCards);
    // Second Hit = "NextIndex"
    While assigned(fTargetCards[result]) Do Begin
      result := (result + 1) Mod length(fTargetCards);
    End;
  End;

Var
  i: Integer;
Begin
  ClearTargetCards;
  setlength(fTargetCards, length(tTargetOrder));
  For i := 0 To high(fTargetCards) Do Begin
    fTargetCards[i] := Nil;
  End;
  aIndex := 0;
  For i := 0 To high(fTargetCards) Do Begin
    fTargetCards[aIndex] := TPlayingCard.Create(ScrollBox1);
    fTargetCards[aIndex].Name := 'TargetPlayingcard' + inttostr(aIndex);
    fTargetCards[aIndex].Parent := ScrollBox1;
    fTargetCards[aIndex].Left := Scale96ToForm(8) + Scale96ToForm(aIndex * (fCards[0].Width + 8));
    fTargetCards[aIndex].Top := Scale96ToForm(8);
    fTargetCards[aIndex].FaceIndex := tTargetOrder[i].FaceIndex;
    If i <> high(fTargetCards) Then Begin
      aIndex := getNextFreeIndex;
    End;
  End;
End;

Procedure TForm1.ClearTargetCards;
Var
  i: Integer;
Begin
  For i := 0 To high(fTargetCards) Do Begin
    fTargetCards[i].Free;
  End;
  setlength(fTargetCards, 0);
End;

Procedure TForm1.RecreatePreviewCards;
Const
  FaceIndexWrapper: Array[0..7] Of integer = (7, 8, 9, 10, 11, 12, 13, 1);
Var
  i, j: Integer;
Begin
  // 1. Clear
  For i := 0 To high(fCards) Do
    fCards[i].Free;
  setlength(fCards, 0);

  For i := 0 To high(tTargetOrder) Do
    tTargetOrder[i].Free;
  setlength(tTargetOrder, 0);

  // 2. Create
  Case RadioGroup1.ItemIndex Of
    0: Begin // Binary Mode
        setlength(fCards, 2);
        fCards[0] := TPlayingCard.Create(self);
        fCards[0].Name := 'Playingcard0';
        fCards[0].Parent := self;
        fCards[0].Left := Scale96ToForm(8);
        fCards[0].Top := RadioGroup1.Top + RadioGroup1.Height + Scale96ToForm(8);
        fCards[0].Suit := 0;
        fCards[0].OnClick := @OnPreviewCardClick;

        fCards[1] := TPlayingCard.Create(self);
        fCards[1].Name := 'Playingcard1';
        fCards[1].Parent := self;
        fCards[1].Left := Scale96ToForm(8) + fCards[0].Width + Scale96ToForm(8);
        fCards[1].Top := RadioGroup1.Top + RadioGroup1.Height + Scale96ToForm(8);
        fCards[1].Suit := 2;
        fCards[1].OnClick := @OnPreviewCardClick;
      End;
    1: Begin // Full
        setlength(fCards, 32);
        For j := 0 To 3 Do Begin
          For i := 0 To 7 Do Begin
            fCards[j * 8 + i] := TPlayingCard.Create(self);
            fCards[j * 8 + i].Name := 'Playingcard' + inttostr(j * 8 + i);
            fCards[j * 8 + i].Parent := self;
            fCards[j * 8 + i].Left := Scale96ToForm(8) + Scale96ToForm(i * (fCards[0].Width + 8));
            fCards[j * 8 + i].Top := RadioGroup1.Top + RadioGroup1.Height + Scale96ToForm(8) + Scale96ToForm(j * (fCards[0].Height + 8));
            fCards[j * 8 + i].Suit := j;
            fCards[j * 8 + i].Value := FaceIndexWrapper[i];
            fCards[j * 8 + i].OnClick := @OnPreviewCardClick;
          End;
        End;
      End;
  End;
  FormResize(Nil);
  ClearTargetCards;
End;

End.

