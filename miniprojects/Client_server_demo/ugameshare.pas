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
Unit ugameshare;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils;

Const
  FrameDeltaInMs = 40;
  DeltaMoveOnKeyPress = 5;

Type
  // Attention as this datastruct is directly used to stream, it shall not
  // contain any dynamic data !
  TPlayer = Record
    PlayerID: Integer; // ID given by Chunkmanager
    x, y: integer;
    Points: integer; // Number of collected Items
    // ADD static user fields, no dynamic fields (like strings / arrays !)
  End;

  TItem = Record
    OwnerId: Integer; // ID given by Chunkmanager
    x, y: integer;
  End;

Const
  // Messages from server to client
  msgSetPlayerID = 0;
  msgAktualGameData = 1;

  // Messages from client to server
  msgPlayerPosition = 100;
  msgPLayerPlacedItem = 101;

  // Messages that can be send from both, client and server

Implementation

End.

