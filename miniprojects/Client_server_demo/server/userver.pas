(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Client_Server Demo                                    *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit userver;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, uChunkmanager, Lnet, ugameshare;

Type

  { TServer }

  TServer = Class
  private
    fTerminated: Boolean;
    fTCP: TLTcp;
    fChunkManager: TChunkManager;
    fplayer: Array Of TPlayer;
    fItems: Array Of TItem;
    fLastFrameTick: QWord;
  protected
    Procedure OnAccept(aSocket: TLSocket);
    Procedure OnDisconnect(aSocket: TLSocket);
    Procedure OnError(Const msg: String; aSocket: TLSocket);

    Procedure OnReceivedChunk(Sender: TObject; Const Chunk: TChunk);

    Procedure HandleUpdatePlayersPosition(Const Data: Tstream; UID: Integer);
    Procedure HandlePlayerPlacedItem(Const Data: Tstream; UID: Integer);

    Procedure CreateVirtualFrame;
    Procedure UpdateGamingData;

    Function SendChunk(UserDefinedID: Integer; Data: TStream; UID: integer): Boolean;
  public
    Constructor Create(aPort: Integer); virtual;
    Destructor Destroy; override;
    Procedure Execute;
  End;

Implementation

Uses Crt;

Function collide(Const aPlayer: TPlayer; Const aItem: TItem): boolean;
Begin
  result :=
    (aPlayer.PlayerID <> aItem.OwnerId) And
    (aItem.x >= aPlayer.x - DeltaMoveOnKeyPress) And
    (aItem.x <= aPlayer.x + DeltaMoveOnKeyPress) And
    (aItem.y >= aPlayer.y - DeltaMoveOnKeyPress) And
    (aItem.y <= aPlayer.y + DeltaMoveOnKeyPress);
End;

{ TServer }

Procedure TServer.OnAccept(aSocket: TLSocket);
Var
  id: integer;
  m: TMemoryStream;
Begin
  id := fChunkManager.SocketToUID(aSocket);
  If id <= 0 Then Begin
    writeln('Got invalid accept.');
    exit;
  End;
  setlength(fplayer, high(fplayer) + 2);
  fplayer[high(fplayer)].PlayerID := id;
  fplayer[high(fplayer)].x := 0;
  fplayer[high(fplayer)].y := 0;
  fChunkManager.SetNoDelay(true);
  writeln('Accepted player: ' + inttostr(id));
  m := TMemoryStream.Create;
  m.Write(id, SizeOf(id));
  SendChunk(msgSetPlayerID, m, id);
End;

Procedure TServer.OnDisconnect(aSocket: TLSocket);
Var
  id, i, j: Integer;
Begin
  id := fChunkManager.SocketToUID(aSocket);
  If id <= 0 Then Begin
    writeln('Lost unknown player.');
    exit;
  End;
  For i := 0 To high(fplayer) Do Begin
    If fplayer[i].PlayerID = id Then Begin
      writeln('Lost player: ' + IntToStr(id));
      // Free the players data
      For j := i To high(fplayer) - 1 Do Begin
        fplayer[j] := fplayer[j + 1];
      End;
      setlength(fplayer, high(fplayer));
      break;
    End;
  End;
  If Not assigned(fplayer) Then Begin
    setlength(fItems, 0);
    writeln('Lost last player');
  End;
End;

Procedure TServer.OnError(Const msg: String; aSocket: TLSocket);
Begin
  writeln('Error: ' + msg);
  OnDisconnect(aSocket);
End;

Procedure TServer.OnReceivedChunk(Sender: TObject; Const Chunk: TChunk);
Begin
  Case Chunk.UserDefinedID Of
    msgPlayerPosition: HandleUpdatePlayersPosition(Chunk.Data, Chunk.UID);
    msgPLayerPlacedItem: HandlePlayerPlacedItem(Chunk.Data, Chunk.UID);
  End;
End;

Procedure TServer.HandleUpdatePlayersPosition(Const Data: Tstream; UID: Integer
  );
Var
  i: Integer;
Begin
  For i := 0 To high(fplayer) Do Begin
    If fplayer[i].PlayerID = UID Then Begin
      data.Read(fplayer[i].x, sizeof(fplayer[i].x));
      data.Read(fplayer[i].y, sizeof(fplayer[i].y));
      break;
    End;
  End;
End;

Procedure TServer.HandlePlayerPlacedItem(Const Data: Tstream; UID: Integer);
Var
  item: TItem;
  i: Integer;
Begin
  item.OwnerId := UID;
  data.Read(item.x, sizeof(item.x));
  data.Read(item.y, sizeof(item.y));
  For i := 0 To high(fItems) Do Begin
    If (fItems[i].x = item.x) And
      (fItems[i].y = item.y) Then exit;
  End;
  writeln(format('Player %d placed item on (%d/%d)', [UID, item.x, item.y]));
  setlength(fItems, high(fItems) + 2);
  fItems[high(fItems)] := item;
End;

Procedure TServer.CreateVirtualFrame;
Var
  m: TMemoryStream;
  i: Integer;
Begin
  // First Update Gaming Data
  UpdateGamingData;

  // Second Send Gaming Data
  // Send Cyclic Data to Clients
  m := TMemoryStream.Create;
  // First Player Positions
  i := length(fplayer);
  m.Write(i, SizeOf(i));
  For i := 0 To high(fplayer) Do Begin
    m.Write(fplayer[i], sizeof(fplayer[i]));
  End;
  // Second Items
  i := length(fItems);
  m.Write(i, SizeOf(i));
  For i := 0 To high(fItems) Do Begin
    m.Write(fItems[i], sizeof(fItems[i]));
  End;
  SendChunk(msgAktualGameData, m, 0);
End;

Procedure TServer.UpdateGamingData;
Var
  i, j, k: Integer;
Begin
  For i := 0 To high(fplayer) Do Begin
    For j := high(fItems) Downto 0 Do Begin
      If collide(fplayer[i], fItems[j]) Then Begin
        inc(fplayer[i].Points);
        For k := j To high(fItems) - 1 Do Begin
          fItems[k] := fItems[k + 1];
        End;
        setlength(fItems, high(fItems));
      End;
    End;
  End;
End;

Function TServer.SendChunk(UserDefinedID: Integer; Data: TStream; UID: integer
  ): Boolean;
Begin
  result := fChunkManager.SendChunk(UserDefinedID, data, uid);
  // TODO: Evaluate result, do logging ...
End;

Constructor TServer.Create(aPort: Integer);
Begin
  writeln('Create testserver on: ' + inttostr(aPort));
  writeln('press "ESC" in console window to close.');
  fTerminated := false;
  fTCP := TLTcp.Create(Nil);
  fTCP.ReuseAddress := true; // Bei Absturz kann so sofort wieder neu verbunden werden
  fTCP.OnAccept := @OnAccept;
  fTCP.OnError := @OnError;
  fTCP.OnDisconnect := @OnDisconnect;

  fChunkManager := TChunkManager.create;
  fChunkManager.RegisterConnection(fTCP);
  fChunkManager.OnReceivedChunk := @OnReceivedChunk;

  If Not fChunkManager.Listen(aPort) Then Begin
    fTerminated := true;
    writeln('Error, could not listen on port: ' + inttostr(aPort));
  End;
  fplayer := Nil;
  fItems := Nil;
End;

Destructor TServer.Destroy;
Begin
  writeln('Free server');
  If assigned(fChunkManager) Then Begin
    fChunkManager.Disconnect(true);
    fChunkManager.free;
  End;
  // Das Disconnect macht ja der ChunkManager !
  While fTCP.Connected Do Begin
    fTCP.CallAction;
  End;
  ftcp.free;
End;

Procedure TServer.Execute;
Var
  n: QWord;
Begin
  fLastFrameTick := GetTickCount64;
  While Not fTerminated Do Begin
    n := GetTickCount64;
    If fLastFrameTick + FrameDeltaInMs <= n Then Begin
      fLastFrameTick := n;
      CreateVirtualFrame;
    End;
    sleep(1);
    fChunkManager.CallAction();
    (*
     * Ab hier geht es nur noch darum zu erkennen ob die Anwendung beendet werden
     * soll.
     *)
    If KeyPressed Then Begin
      Case ReadKey() Of
        #27: Begin
            writeln('Close by user input.');
            fTerminated := true;
          End;
      End;
    End;
  End;
End;

End.

