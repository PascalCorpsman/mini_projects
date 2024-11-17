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
  Classes, SysUtils, ugraphics, upixelimage, upixeleditor_types;

Const
  RecordChunkSize = 1024;

Type

  TRecordingKind = (rkUnknown, rkPixelChange, rkSizeChange);

  TPixelChange = Record
    x, y: integer;
    From: TRGBA;
  End;

  TSizeChange = Record
    OldWidth, OldHeight: Integer;
  End;

  TRecord = Record
    Kind: TRecordingKind;
    PixelChange: Array Of TPixelChange;
    SizeChange: TSizeChange;
  End;

  { TUndoEngine }

  TUndoEngine = Class
  private
    fRecordings: Array Of TRecord;
    // Der Aktuelle Record der im Aufbau ist
    faRecord: TRecord;
    // Hilfen für den Aufbau des Aktuellen Record
    fPixelChangeRecordCount: integer;
  public
    Constructor Create(); virtual;
    Destructor Destroy(); override;

    Procedure Clear;

    (*
     * Startet ein neues Recording, mus also immer als 1. aufgerufen werden
     *)
    Procedure StartNewRecording;

    Procedure RecordPixelChange(x, y: integer; from: TRGBA); // Zeichnet eine Pixeländerung auf
    Procedure RecordSizeChange(NewHeight, NewWidth: Integer; ScaleMode: TScaleMode; Const Image: TPixelImage);

    (*
     * Beendet eine Aufzeichnung und legt diese auf dem Stack ab
     *)
    Procedure PushRecording;

    (*
     * Wendet die zuletzt pegushte Aufzeichnung auf Image an und löscht das oberste Element
     *)
    Procedure PopRecording(Const image: TPixelImage);
  End;

Implementation

Uses math;

{ TUndoEngine }

Constructor TUndoEngine.Create;
Begin
  Inherited Create;
  fRecordings := Nil;
  faRecord.Kind := rkUnknown;
  faRecord.PixelChange := Nil;
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
    setlength(fRecordings[i].PixelChange, 0);
  End;
  setlength(fRecordings, 0);
  setlength(faRecord.PixelChange, 0);
  fRecordings := Nil;
  faRecord.Kind := rkUnknown;
  setlength(faRecord.PixelChange, RecordChunkSize);
  fPixelChangeRecordCount := 0;
End;

Procedure TUndoEngine.StartNewRecording;
Begin
  faRecord.Kind := rkUnknown;
  setlength(faRecord.PixelChange, RecordChunkSize);
  fPixelChangeRecordCount := 0;
End;

Procedure TUndoEngine.RecordPixelChange(x, y: integer; from: TRGBA);
Begin
  If (faRecord.Kind <> rkPixelChange) And
    (faRecord.Kind <> rkUnknown) Then Begin
    Raise exception.create('Error, mixing undo recordings.');
  End;
  If Not assigned(faRecord.PixelChange) Then Begin // Zur Not starten wir das neue Recording eben selbst.
    StartNewRecording;
  End;
  faRecord.Kind := rkPixelChange;
  faRecord.PixelChange[fPixelChangeRecordCount].x := x;
  faRecord.PixelChange[fPixelChangeRecordCount].y := y;
  faRecord.PixelChange[fPixelChangeRecordCount].From := from;
  inc(fPixelChangeRecordCount);
  If (fPixelChangeRecordCount > high(faRecord.PixelChange)) Then Begin
    setlength(faRecord.PixelChange, length(faRecord.PixelChange) + RecordChunkSize);
  End;
End;

Procedure TUndoEngine.RecordSizeChange(NewHeight, NewWidth: Integer;
  ScaleMode: TScaleMode; Const Image: TPixelImage);
Var
  i, j: Integer;
  c: TRGBA;
Begin
  If (faRecord.Kind <> rkSizeChange) And
    (faRecord.Kind <> rkUnknown) Then Begin
    Raise exception.create('Error, mixing undo recordings.');
  End;
  If Not assigned(faRecord.PixelChange) Then Begin // Zur Not starten wir das neue Recording eben selbst.
    StartNewRecording;
  End;
  faRecord.Kind := rkSizeChange;
  faRecord.SizeChange.OldWidth := Image.Width;
  faRecord.SizeChange.OldHeight := Image.Height;
  If ScaleMode = smResize Then Begin
    // Retten der Pixel, die durch die Verkleinerung verloren gehen
    // Das wird 3 mal gemacht
    // Der Rechte Teil
    If faRecord.SizeChange.OldWidth > NewWidth Then Begin
      faRecord.Kind := rkPixelChange;
      For i := NewWidth To faRecord.SizeChange.OldWidth - 1 Do Begin
        For j := 0 To min(NewHeight - 1, faRecord.SizeChange.OldHeight - 1) Do Begin
          c := image.GetColorAt(i, j);
          If (c <> ColorTransparent) Then Begin
            RecordPixelChange(i, j, c);
          End;
        End;
      End;
      faRecord.Kind := rkSizeChange;
    End;
    // Der Untere Teil
    If faRecord.SizeChange.OldHeight > NewHeight Then Begin
      faRecord.Kind := rkPixelChange;
      For j := NewHeight To faRecord.SizeChange.OldHeight - 1 Do Begin
        For i := 0 To min(NewWidth - 1, faRecord.SizeChange.OldWidth - 1) Do Begin
          c := image.GetColorAt(i, j);
          If (c <> ColorTransparent) Then Begin
            RecordPixelChange(i, j, c);
          End;
        End;
      End;
      faRecord.Kind := rkSizeChange;
    End;
    // Der Rechte Untere Teil
    If (faRecord.SizeChange.OldWidth > NewWidth) And
      (faRecord.SizeChange.OldHeight > NewHeight) Then Begin
      faRecord.Kind := rkPixelChange;
      For j := NewHeight To faRecord.SizeChange.OldHeight - 1 Do Begin
        For i := NewWidth To faRecord.SizeChange.OldWidth - 1 Do Begin
          c := image.GetColorAt(i, j);
          If (c <> ColorTransparent) Then Begin
            RecordPixelChange(i, j, c);
          End;
        End;
      End;
      faRecord.Kind := rkSizeChange;
    End;
  End
  Else Begin
    // Wird Irgendwie interpoliert muss das ganze Bild gespeichert werden !
    faRecord.Kind := rkPixelChange;
    For j := 0 To faRecord.SizeChange.OldHeight - 1 Do Begin
      For i := 0 To faRecord.SizeChange.OldWidth - 1 Do Begin
        c := image.GetColorAt(i, j);
        RecordPixelChange(i, j, c);
      End;
    End;
    faRecord.Kind := rkSizeChange;
  End;
End;

Procedure TUndoEngine.PopRecording(Const image: TPixelImage);
Var
  i: Integer;
Begin
  If length(fRecordings) = 0 Then exit;
  // Alle Änderungen Rückgängig machen
  image.BeginUpdate;
  Case fRecordings[high(fRecordings)].Kind Of
    rkPixelChange: Begin
        For i := 0 To high(fRecordings[high(fRecordings)].PixelChange) Do Begin
          image.SetColorAt(
            fRecordings[high(fRecordings)].PixelChange[i].x,
            fRecordings[high(fRecordings)].PixelChange[i].y,
            fRecordings[high(fRecordings)].PixelChange[i].From
            );
        End;
        setlength(fRecordings[high(fRecordings)].PixelChange, 0);
      End;
    rkSizeChange: Begin
        // Das Rescale packen wir in einen eigenen Begin / End Update block
        image.EndUpdate;
        image.Rescale(fRecordings[high(fRecordings)].SizeChange.OldWidth, fRecordings[high(fRecordings)].SizeChange.OldHeight, smResize);
        image.BeginUpdate;
        For i := 0 To high(fRecordings[high(fRecordings)].PixelChange) Do Begin
          image.SetColorAt(
            fRecordings[high(fRecordings)].PixelChange[i].x,
            fRecordings[high(fRecordings)].PixelChange[i].y,
            fRecordings[high(fRecordings)].PixelChange[i].From
            );
        End;
        setlength(fRecordings[high(fRecordings)].PixelChange, 0);
      End;
  End;
  // Das oberste Element vom Stack löschen ;)
  setlength(fRecordings, high(fRecordings));
  image.EndUpdate;
End;

Procedure TUndoEngine.PushRecording;
Begin
  If faRecord.Kind = rkUnknown Then exit;
  Case faRecord.Kind Of
    rkPixelChange: Begin
        If fPixelChangeRecordCount = 0 Then exit;
        setlength(faRecord.PixelChange, fPixelChangeRecordCount);
        setlength(fRecordings, high(fRecordings) + 2);
        fRecordings[high(fRecordings)] := faRecord;
        faRecord.PixelChange := Nil;
        fPixelChangeRecordCount := 0;
      End;
    rkSizeChange: Begin
        setlength(faRecord.PixelChange, fPixelChangeRecordCount);
        setlength(fRecordings, high(fRecordings) + 2);
        fRecordings[high(fRecordings)] := faRecord;
        faRecord.PixelChange := Nil;
        fPixelChangeRecordCount := 0;
      End;
  End;
  faRecord.Kind := rkUnknown;
End;

End.

