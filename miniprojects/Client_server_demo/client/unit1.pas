(******************************************************************************)
(* Client_Server Demo                                              12.02.2026 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Client Application                                           *)
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
  lNetComponents, uChunkmanager, lNet, ugameshare;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LTCPComponent1: TLTCPComponent;
    PaintBox1: TPaintBox;
    Timer1: TTimer;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormKeyPress(Sender: TObject; Var Key: char);
    Procedure LTCPComponent1Connect(aSocket: TLSocket);
    Procedure LTCPComponent1Disconnect(aSocket: TLSocket);
    Procedure LTCPComponent1Error(Const msg: String; aSocket: TLSocket);
    Procedure PaintBox1Paint(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
  private
    fChunkManager: TChunkManager;
    fPlayer: TPlayer;
    fplayers: Array Of TPlayer;
    fItems: Array Of TItem;
    Procedure OnReceivedChunk(Sender: TObject; Const Chunk: TChunk);
    Procedure SendPlayerPos;
    Procedure PlaceItem;
    Procedure SendChunk(UserDefinedID: Integer; Data: TStream);
    Procedure HandleGetGameData(Const data: TStream);
  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses LCLType;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  edit1.text := '127.0.0.1';
  edit2.text := '9876';
  fChunkManager := TChunkManager.create;
  fChunkManager.OnReceivedChunk := @OnReceivedChunk;
  fChunkManager.RegisterConnection(LTCPComponent1);
  fItems := Nil;
End;

Procedure TForm1.FormKeyPress(Sender: TObject; Var Key: char);
Begin
  // Cursor is
  If (key = 'W') Or (key = 'w') Then Begin
    fPlayer.y := fPlayer.y - DeltaMoveOnKeyPress;
  End;
  If (key = 'S') Or (key = 's') Then Begin
    fPlayer.y := fPlayer.y + DeltaMoveOnKeyPress;
  End;
  If (key = 'A') Or (key = 'a') Then Begin
    fPlayer.x := fPlayer.x - DeltaMoveOnKeyPress;
  End;
  If (key = 'D') Or (key = 'd') Then Begin
    fPlayer.x := fPlayer.x + DeltaMoveOnKeyPress;
  End;
  If (key = 'F') Or (key = 'f') Then Begin
    PlaceItem;
  End;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  // Connect
  fPlayer.PlayerID := -1;
  fChunkManager.Connect(edit1.text, strtointdef(edit2.text, 9876));
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  fChunkManager.Disconnect(false);
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  Button2Click(Nil);
  fChunkManager.Disconnect(true);
  fChunkManager.Free;
  fChunkManager := Nil;
End;

Procedure TForm1.LTCPComponent1Connect(aSocket: TLSocket);
Begin
  Button1.Enabled := false;
  Button2.Enabled := true;
  Edit1.Enabled := false;
  Edit2.Enabled := false;
End;

Procedure TForm1.LTCPComponent1Disconnect(aSocket: TLSocket);
Begin
  Button1.Enabled := true;
  Button2.Enabled := false;
  Edit1.Enabled := true;
  Edit2.Enabled := true;
  setlength(fPlayers, 0);
  setlength(fItems, 0);
  PaintBox1.Invalidate;
End;

Procedure TForm1.LTCPComponent1Error(Const msg: String; aSocket: TLSocket);
Begin
  LTCPComponent1Disconnect(aSocket);
  ShowMessage(msg);
End;

Procedure TForm1.PaintBox1Paint(Sender: TObject);
Var
  i: Integer;
  s: String;
Begin
  PaintBox1.Canvas.Brush.Color := clBlack;
  PaintBox1.Canvas.Rectangle(-1, -1, PaintBox1.Width + 1, PaintBox1.Height + 1);
  // First Items
  For i := 0 To high(fItems) Do Begin
    If fItems[i].OwnerId = fPlayer.PlayerID Then Begin
      PaintBox1.Canvas.Font.Color := clGray;
    End
    Else Begin
      PaintBox1.Canvas.Font.Color := clLime;
    End;
    s := 'x';
    PaintBox1.Canvas.TextOut(
      fItems[i].x - PaintBox1.Canvas.TextWidth(s) Div 2,
      fItems[i].y - PaintBox1.Canvas.TextHeight(s) Div 2,
      s);
  End;
  // Second Player
  PaintBox1.Canvas.Font.Color := clWhite;
  For i := 0 To high(fplayers) Do Begin
    s := inttostr(fplayers[i].PlayerID);
    If fplayers[i].PlayerID = fPlayer.PlayerID Then Begin
      s := 'O';
      PaintBox1.Canvas.TextOut(8, 8, 'Points: ' + inttostr(fplayers[i].Points));
    End;
    PaintBox1.Canvas.TextOut(
      fplayers[i].x - PaintBox1.Canvas.TextWidth(s) Div 2,
      fplayers[i].y - PaintBox1.Canvas.TextHeight(s) Div 2,
      s);
  End;
End;

Procedure TForm1.Timer1Timer(Sender: TObject);
Begin
  If fChunkManager.Connected Then PaintBox1.Invalidate;
  SendPLayerPos;
End;

Procedure TForm1.OnReceivedChunk(Sender: TObject; Const Chunk: TChunk);
Begin
  Case Chunk.UserDefinedID Of
    msgSetPlayerID: Begin
        Chunk.Data.Read(fPlayer.PlayerID, sizeof(fPlayer.PlayerID));
        fPlayer.x := random(PaintBox1.Width - 100) + 50;
        fPlayer.y := random(PaintBox1.Height - 100) + 50;
        setlength(fplayers, 1);
        fPlayers[0] := fPlayer;
        SendPLayerPos;
        PaintBox1.Invalidate;
      End;
    msgAktualGameData: Begin
        HandleGetGameData(Chunk.Data);
      End;
  End;
End;

Procedure TForm1.SendPlayerPos;
Var
  m: TMemoryStream;
Begin
  m := TMemoryStream.Create;
  m.Write(fPlayer.x, sizeof(fPlayer.x));
  m.Write(fPlayer.y, sizeof(fPlayer.y));
  SendChunk(msgPlayerPosition, m);
End;

Procedure TForm1.PlaceItem;
Var
  m: TMemoryStream;
Begin
  m := TMemoryStream.Create;
  m.Write(fPlayer.x, sizeof(fPlayer.x));
  m.Write(fPlayer.y, sizeof(fPlayer.y));
  SendChunk(msgPLayerPlacedItem, m);
End;

Procedure TForm1.SendChunk(UserDefinedID: Integer; Data: TStream);
Begin
  fChunkManager.SendChunk(UserDefinedID, data);
  // Todo: Logging ?
End;

Procedure TForm1.HandleGetGameData(Const data: TStream);
Var
  i: Integer;
Begin
  // First read all Player Positions
  i := 0;
  data.Read(i, sizeof(i));
  If length(fplayers) <> i Then SetLength(fplayers, i);
  For i := 0 To high(fplayers) Do Begin
    data.Read(fplayers[i], sizeof(fplayers[i]));
  End;
  // Second Read the Items
  i := 0;
  data.Read(i, sizeof(i));
  If length(fItems) <> i Then SetLength(fItems, i);
  For i := 0 To high(fItems) Do Begin
    data.Read(fItems[i], sizeof(fItems[i]));
  End;
End;

End.

