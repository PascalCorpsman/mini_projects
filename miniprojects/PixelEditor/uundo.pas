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
Unit uundo;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, ugraphics, uimage;

Const
  RecordChunkSize = 1024;

Type

  TPixelChange = Record
    x, y: integer;
    From: TRGBA;
  End;

  TRecording = Array Of TPixelChange;

  { TUndoEngine }

  TUndoEngine = Class
  private
    fRecordings: Array Of TRecording;
    faRecord: TRecording;
    faRecordCount: integer;
  public
    Constructor Create(); virtual;
    Destructor Destroy(); override;

    Procedure Clear;

    Procedure StartNewRecording;
    Procedure RecordPixelChange(x, y: integer; from: TRGBA);
    Procedure PushRecording;

    Procedure PopRecording(Const image: TImage);

  End;

Implementation

{ TUndoEngine }

Constructor TUndoEngine.Create;
Begin
  Inherited Create;
  fRecordings := Nil;
  faRecord := Nil;
End;

Destructor TUndoEngine.Destroy;
Begin
  Clear;
End;

Procedure TUndoEngine.Clear;
Var
  i: Integer;
Begin
  For i := 0 To high(fRecordings) Do Begin
    setlength(fRecordings[i], 0);
  End;
  setlength(fRecordings, 0);
  setlength(faRecord, 0);
  fRecordings := Nil;
  faRecord := Nil;
End;

Procedure TUndoEngine.StartNewRecording;
Begin
  (* TODO: Rein oder raus ?
  If faRecordCount <> 0 Then Begin
    PushRecording;
  End;
  // Ende Rein oder Raus  *)
  setlength(faRecord, RecordChunkSize);
  faRecordCount := 0;
End;

Procedure TUndoEngine.RecordPixelChange(x, y: integer; from: TRGBA);
Begin
  faRecord[faRecordCount].x := x;
  faRecord[faRecordCount].y := y;
  faRecord[faRecordCount].From := from;
  inc(faRecordCount);
  If (faRecordCount > high(faRecord)) Then Begin
    setlength(faRecord, length(faRecord) + RecordChunkSize);
  End;
End;

Procedure TUndoEngine.PopRecording(Const image: TImage);
Var
  i: Integer;
Begin
  If length(fRecordings) = 0 Then exit;
  // Alle Änderungen Rückgängig machen
  For i := 0 To high(fRecordings[high(fRecordings)]) Do Begin
    image.SetColorAt(
      fRecordings[high(fRecordings)][i].x,
      fRecordings[high(fRecordings)][i].y,
      lMiddle, // TODO: Das ist a biss arg statisch, sollten wir je mehrere Layer unterstützen wollen.
      fRecordings[high(fRecordings)][i].From
      );
  End;
  // Das oberste Element vom Stack löschen ;)
  setlength(fRecordings[high(fRecordings)], 0);
  setlength(fRecordings, high(fRecordings));
End;

Procedure TUndoEngine.PushRecording;
Begin
  If faRecordCount = 0 Then exit;
  setlength(faRecord, faRecordCount);
  setlength(fRecordings, high(fRecordings) + 2);
  fRecordings[high(fRecordings)] := faRecord;
  faRecord := Nil;
  faRecordCount := 0;
End;

End.

