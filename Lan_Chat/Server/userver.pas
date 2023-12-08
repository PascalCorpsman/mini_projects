Unit userver;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, uChunkmanager, ulanchatcommon, lNet;

Type

  TParticipant = Record
    Name: String; // Klarname
    uID: Integer; // UID von TChunkManager
  End;

  TKnownParticipant = Record
    Name: String; // Klarname
    PWHash: String; // Passwort Hash zur Login Prüfung
  End;

  { TLANChatServer }

  TLANChatServer = Class
  private
    fParticipants: Array Of TParticipant;
    fKnownParticipants: Array Of TKnownParticipant;
    fConnection: TChunkManager;
    fTCPConnection: TLTcp;
    frunning: Boolean;
    fNeedSendKnownParticipantList: Boolean;

    Procedure OnAccept(aSocket: TLSocket);
    Procedure OnDisconnect(aSocket: TLSocket);
    Procedure OnError(Const msg: String; aSocket: TLSocket);

    Procedure OnReceivedChunk(Sender: TObject; Const Chunk: TChunk);

    Procedure HandleNewParticipant(Const aChunk: TChunk);
    Procedure HandleMessageForwarding(Const aChunk: TChunk);
    Procedure HandleFileTransferRequest(Const aChunk: TChunk);
    Procedure HandleFileTransferRequest_Result(Const aChunk: TChunk);
    Procedure HandleTransferFileContent(Const aChunk: TChunk);
    Procedure HandleTransferFileFinished(Const aChunk: TChunk);
    Procedure HandleTransferFileRequestNextPacket(Const aChunk: TChunk);
    Procedure HandleTransferFileAbort(Const aChunk: TChunk);

    Procedure SendKnownParticipantList();
    Function GetParticipantIndex(aName: String): Integer;
    Function GetParticipantIndex(uID: Integer): Integer;

    Procedure StoreMessage(aRecipient, aSender, aMsg: String);
    Procedure SendMessage(aRecipient, aSender, aMsg: String);

    Procedure ReadStoredMessages(aRecipient: String);
  public
    Constructor Create(); virtual;
    Destructor Destroy(); override;
    Procedure Execute();
    Procedure Terminate();
  End;

Implementation

Uses Crt;

{ TLANChatServer }

Constructor TLANChatServer.Create;
Var
  sl: TStringList;
  s: String;
  i: Integer;
Begin
  Inherited Create();
  fNeedSendKnownParticipantList := false;
  fConnection := TChunkManager.create;
  fTCPConnection := TLTcp.Create(Nil);
  fTCPConnection.OnAccept := @OnAccept;
  fTCPConnection.OnDisconnect := @OnDisconnect;
  fTCPConnection.OnError := @OnError;
  fTCPConnection.ReuseAddress := true; // Sonst ist das mit den Neustarts immer so nervig

  fConnection.RegisterConnection(fTCPConnection);
  fConnection.OnReceivedChunk := @OnReceivedChunk;
  fParticipants := Nil;
  fKnownParticipants := Nil;
  If FileExists('known.txt') Then Begin
    sl := TStringList.Create;
    sl.LoadFromFile('known.txt');
    For i := 0 To sl.Count - 1 Do Begin
      s := sl[i];
      If trim(s) <> '' Then Begin
        setlength(fKnownParticipants, high(fKnownParticipants) + 2);
        fKnownParticipants[high(fKnownParticipants)].Name := copy(s, 1, pos(';', s) - 1);
        fKnownParticipants[high(fKnownParticipants)].PWHash := copy(s, pos(';', s) + 1, length(s));
      End;
    End;
    sl.free;
  End;
End;

Destructor TLANChatServer.Destroy;
Begin
  fConnection.Disconnect(true);
  fConnection.Free;
End;

Procedure TLANChatServer.OnReceivedChunk(Sender: TObject; Const Chunk: TChunk);
Begin
  Case Chunk.UserDefinedID Of
    MSG_New_Participant: HandleNewParticipant(Chunk);
    Msg_Message: HandleMessageForwarding(Chunk);
    MSG_File_Transfer_Request: HandleFileTransferRequest(Chunk);
    MSG_File_Transfer_Request_result: HandleFileTransferRequest_Result(Chunk);
    MSG_File_Transfer_FileContent: HandleTransferFileContent(Chunk);
    MSG_File_Transfer_FileComplete: HandleTransferFileFinished(Chunk);
    MSG_File_Transfer_File_Next_Packet: HandleTransferFileRequestNextPacket(Chunk);
    MSG_File_Transfer_File_Abort: HandleTransferFileAbort(Chunk);
  Else Begin
      writeln('Error unknown message id: ' + inttostr(Chunk.UserDefinedID));
    End;
  End;
End;

Procedure TLANChatServer.OnAccept(aSocket: TLSocket);
Begin

End;

Procedure TLANChatServer.OnDisconnect(aSocket: TLSocket);
Var
  index, id, j: Integer;
Begin
  id := fConnection.SocketToUID(aSocket);
  If id = 0 Then Begin
    writeln('Lost unknown user');
    exit;
  End;
  index := GetParticipantIndex(id);
  If index = -1 Then Begin
    writeln('Lost unknown user');
    exit;
  End;
  writeln('Lost: ' + fParticipants[index].Name);
  For j := index To high(fParticipants) - 1 Do Begin
    fParticipants[j] := fParticipants[j + 1];
  End;
  setlength(fParticipants, high(fParticipants));
  fNeedSendKnownParticipantList := true;
End;

Procedure TLANChatServer.OnError(Const msg: String; aSocket: TLSocket);
Begin
  writeln('Socket error: ' + msg);
End;

Procedure TLANChatServer.HandleNewParticipant(Const aChunk: TChunk);
Var
  Allowed, found: Boolean;
  pv, i: Integer;
  nName, nPWHash: String;
  data: TMemoryStream;
  sl: TStringList;
  aError: uint16;
Begin
  Allowed := true;
  aError := Error_No_Error;
  nName := aChunk.Data.ReadAnsiString;
  nPWHash := aChunk.Data.ReadAnsiString;
  pv := 0;
  aChunk.Data.read(pv, sizeof(pv));
  If pv <> ProtokollVersion Then Begin
    writeln('Reject: ' + nName + ' -> invalid protocolversion');
    data := TMemoryStream.Create;
    Allowed := false;
    data.Write(Allowed, SizeOf(Allowed));
    aError := Error_Invalid_Protocol_Version;
    data.Write(aError, SizeOf(aError));
    fConnection.SendChunk(MSG_New_Participant_Result, data, aChunk.UID);
    exit;
  End;
  For i := 0 To high(fParticipants) Do Begin
    If lowercase(fParticipants[i].Name) = lowercase(nName) Then Begin
      Allowed := false;
      aError := Error_User_Already_logged_in;
      break;
    End;
  End;
  found := false;
  For i := 0 To high(fKnownParticipants) Do Begin
    If lowercase(fKnownParticipants[i].Name) = lowercase(nName) Then Begin
      found := true;
      If fKnownParticipants[i].PWHash <> nPWHash Then Begin
        allowed := false;
        aError := Error_Invalid_Password;
      End;
      break;
    End;
  End;
  data := TMemoryStream.Create;
  data.Write(Allowed, SizeOf(Allowed));
  data.Write(aError, SizeOf(aError));
  fConnection.SendChunk(MSG_New_Participant_Result, data, aChunk.UID);
  // Wenn der Teilnehmer Aktzeptiert wurde, dann teilen wir das allen mit
  If Allowed Then Begin
    writeln('Accept: ' + nName);
    setlength(fParticipants, high(fParticipants) + 2);
    fParticipants[high(fParticipants)].Name := nName;
    fParticipants[high(fParticipants)].uID := aChunk.UID;
    If Not found Then Begin
      setlength(fKnownParticipants, high(fKnownParticipants) + 2);
      fKnownParticipants[high(fKnownParticipants)].Name := nName;
      fKnownParticipants[high(fKnownParticipants)].PWHash := nPWHash;
      sl := TStringList.Create;
      If FileExists('known.txt') Then Begin
        sl.LoadFromFile('known.txt');
      End;
      sl.add(nName + ';' + nPWHash);
      sl.SaveToFile('known.txt');
      sl.free;
    End
    Else Begin
      // Der User ist Bekannt gäbe es werte die Aktualisiert werden müssten stände dies hier ...
    End;
    SendKnownParticipantList();
    ReadStoredMessages(nName);
  End
  Else Begin
    writeln('Reject: ' + nName);
  End;
End;

Procedure TLANChatServer.HandleMessageForwarding(Const aChunk: TChunk);
Var
  Sender, Recipient, MSG: String;
  Index: Integer;
Begin
  Recipient := aChunk.Data.ReadAnsiString;
  Sender := aChunk.Data.ReadAnsiString;
  MSG := aChunk.Data.ReadAnsiString;
  Index := GetParticipantIndex(Recipient);
  If index = -1 Then Begin // Der Empfänger ist gerade nicht Online -> Wir Speichern die Nachricht für Später
    StoreMessage(Recipient, Sender, MSG);
  End
  Else Begin
    SendMessage(Recipient, Sender, MSG);
  End;
End;

Procedure TLANChatServer.HandleFileTransferRequest(Const aChunk: TChunk);
Var
  aSender, aReceiver: String;
  Index: Integer;
  data: TMemoryStream;
  aReason: uint16;
Begin
  aReceiver := aChunk.Data.ReadAnsiString;
  aSender := aChunk.Data.ReadAnsiString;
  Index := GetParticipantIndex(aReceiver);
  data := TMemoryStream.Create;
  If index = -1 Then Begin
    // Der Empfänger ist gerade nicht Online -> File transfer Ablehnen
    data.WriteAnsiString(aSender);
    data.WriteAnsiString(aReceiver);
    aReason := File_Transfer_No_due_to_Receiver_not_Online;
    data.Write(aReason, sizeof(aReason));
    fConnection.SendChunk(MSG_File_Transfer_Request_result, data, aChunk.UID);
  End
  Else Begin
    // Der Empfänger ist Online -> Anfrage Weiter leiten
    aChunk.Data.Position := 0;
    data.CopyFrom(aChunk.Data, aChunk.Data.Size);
    fConnection.SendChunk(MSG_File_Transfer_Request, data, fParticipants[index].uID);
  End;
End;

Procedure TLANChatServer.HandleFileTransferRequest_Result(Const aChunk: TChunk);
Var
  aReceiver: String;
  aSender: String;
  Index: Integer;
  data: TMemoryStream;
  aReason: uint16;
Begin
  aReceiver := aChunk.Data.ReadAnsiString;
  Index := GetParticipantIndex(aReceiver);
  data := TMemoryStream.Create;
  If index = -1 Then Begin
    aSender := aChunk.Data.ReadAnsiString;
    // Die Gegenstelle gibt es nicht mehr -> Abbruch
    data.WriteAnsiString(aSender);
    data.WriteAnsiString(aReceiver);
    aReason := File_Transfer_Abort_Missing_Endpoint;
    data.Write(aReason, SizeOf(aReason));
    fConnection.SendChunk(MSG_File_Transfer_File_Abort, data, aChunk.UID);
  End
  Else Begin
    // Der Gegenstelle die Antwort weiter leiten
    aChunk.Data.Position := 0;
    data.CopyFrom(aChunk.Data, aChunk.Data.Size);
    fConnection.SendChunk(MSG_File_Transfer_Request_result, data, fParticipants[index].uID);
  End;
End;

Procedure TLANChatServer.HandleTransferFileContent(Const aChunk: TChunk);
Var
  aReceiver: String;
  aSender: String;
  Index: Integer;
  data: TMemoryStream;
  aReason: uint16;
Begin
  aReceiver := aChunk.Data.ReadAnsiString;
  Index := GetParticipantIndex(aReceiver);
  data := TMemoryStream.Create;
  If index = -1 Then Begin
    aSender := aChunk.Data.ReadAnsiString;
    // Die Gegenstelle gibt es nicht mehr -> Abbruch
    data.WriteAnsiString(aSender);
    data.WriteAnsiString(aReceiver);
    aReason := File_Transfer_Abort_Missing_Endpoint;
    data.Write(aReason, SizeOf(aReason));
    fConnection.SendChunk(MSG_File_Transfer_File_Abort, data, aChunk.UID);
  End
  Else Begin
    // Der Gegenstelle die Antwort weiter leiten
    aChunk.Data.Position := 0;
    data.CopyFrom(aChunk.Data, aChunk.Data.Size);
    fConnection.SendChunk(MSG_File_Transfer_FileContent, data, fParticipants[index].uID);
  End;
End;

Procedure TLANChatServer.HandleTransferFileFinished(Const aChunk: TChunk);
Var
  aReceiver, aSender: String;
  Index: Integer;
  data: TMemoryStream;
  aReason: uint16;
Begin
  aReceiver := aChunk.Data.ReadAnsiString;
  Index := GetParticipantIndex(aReceiver);
  data := TMemoryStream.Create;
  If index = -1 Then Begin
    aSender := aChunk.Data.ReadAnsiString;
    // Die Gegenstelle gibt es nicht mehr -> Abbruch
    data.WriteAnsiString(aSender);
    data.WriteAnsiString(aReceiver);
    aReason := File_Transfer_Abort_Missing_Endpoint;
    data.Write(aReason, SizeOf(aReason));
    fConnection.SendChunk(MSG_File_Transfer_File_Abort, data, aChunk.UID);
  End
  Else Begin
    // Der Gegenstelle die Antwort weiter leiten
    aChunk.Data.Position := 0;
    data.CopyFrom(aChunk.Data, aChunk.Data.Size);
    fConnection.SendChunk(MSG_File_Transfer_FileComplete, data, fParticipants[index].uID);
  End;
End;

Procedure TLANChatServer.HandleTransferFileRequestNextPacket(
  Const aChunk: TChunk);
Var
  aReceiver, aSender: String;
  Index: Integer;
  data: TMemoryStream;
  aReason: uint16;
Begin
  aReceiver := aChunk.Data.ReadAnsiString;
  Index := GetParticipantIndex(aReceiver);
  data := TMemoryStream.Create;
  If index = -1 Then Begin
    aSender := aChunk.Data.ReadAnsiString;
    // Die Gegenstelle gibt es nicht mehr -> Abbruch
    data.WriteAnsiString(aSender);
    data.WriteAnsiString(aReceiver);
    aReason := File_Transfer_Abort_Missing_Endpoint;
    data.Write(aReason, SizeOf(aReason));
    fConnection.SendChunk(MSG_File_Transfer_File_Abort, data, aChunk.UID);
  End
  Else Begin
    // Der Gegenstelle die Antwort weiter leiten
    aChunk.Data.Position := 0;
    data.CopyFrom(aChunk.Data, aChunk.Data.Size);
    fConnection.SendChunk(MSG_File_Transfer_File_Next_Packet, data, fParticipants[index].uID);
  End;
End;

Procedure TLANChatServer.HandleTransferFileAbort(Const aChunk: TChunk);
Var
  aReceiver: String;
  Index: Integer;
  data: TMemoryStream;
Begin
  aReceiver := aChunk.Data.ReadAnsiString;
  Index := GetParticipantIndex(aReceiver);
  If index = -1 Then Begin
    // Nichts, die gegenstelle gibt es nicht mehr, wir brauchen uns nicht selbst sagen dass wir abbrechen müssen
  End
  Else Begin
    // Der Gegenstelle die Antwort weiter leiten
    data := TMemoryStream.Create;
    aChunk.Data.Position := 0;
    data.CopyFrom(aChunk.Data, aChunk.Data.Size);
    fConnection.SendChunk(MSG_File_Transfer_File_Abort, data, fParticipants[index].uID);
  End;
End;

Procedure TLANChatServer.SendKnownParticipantList;
Var
  data: TMemoryStream;
  i: integer;
  b: Boolean;
Begin
  If fParticipants = Nil Then exit; // Keiner Mehr da zum Informieren ..
  data := TMemoryStream.Create;
  i := length(fKnownParticipants);
  data.Write(i, SizeOf(i));
  For i := 0 To high(fKnownParticipants) Do Begin
    data.WriteAnsiString(fKnownParticipants[i].Name);
    b := GetParticipantIndex(fKnownParticipants[i].Name) >= 0;
    data.Write(b, sizeof(b));
  End;
  fConnection.SendChunk(MSG_Known_Participant_List, data, 0)
End;

Function TLANChatServer.GetParticipantIndex(aName: String): Integer;
Var
  i: Integer;
Begin
  result := -1;
  For i := 0 To high(fParticipants) Do Begin
    If lowercase(fParticipants[i].Name) = lowercase(aName) Then Begin
      result := i;
      break;
    End;
  End;
End;

Function TLANChatServer.GetParticipantIndex(uID: Integer): Integer;
Var
  i: Integer;
Begin
  result := -1;
  For i := 0 To high(fParticipants) Do Begin
    If fParticipants[i].uID = uID Then Begin
      result := i;
      break;
    End;
  End;
End;

Procedure TLANChatServer.StoreMessage(aRecipient, aSender, aMsg: String);
Var
  sl: TStringList;
Begin
  sl := TStringList.Create;
  If FileExists(lowercase(aRecipient)) Then Begin
    sl.LoadFromFile(lowercase(aRecipient));
  End;
  sl.Append(aSender + ';' + aMsg);
  sl.SaveToFile(lowercase(aRecipient));
  sl.free;
End;

Procedure TLANChatServer.SendMessage(aRecipient, aSender, aMsg: String);
Var
  data: TMemoryStream;
  index, id: Integer;
Begin
  index := GetParticipantIndex(aRecipient);
  id := fParticipants[index].uID;
  data := TMemoryStream.Create;
  data.WriteAnsiString(aSender);
  data.WriteAnsiString(aMsg);
  fConnection.SendChunk(Msg_Message, data, id);
End;

Procedure TLANChatServer.ReadStoredMessages(aRecipient: String);
Var
  sl: TStringList;
  aMsg, aSender, s: String;
  i: Integer;
Begin
  If FileExists(lowercase(aRecipient)) Then Begin
    sl := TStringList.Create;
    sl.LoadFromFile(lowercase(aRecipient));
    For i := 0 To sl.Count - 1 Do Begin
      s := sl[i];
      aSender := copy(s, 1, pos(';', s) - 1);
      aMsg := copy(s, pos(';', s) + 1, length(s));
      SendMessage(aRecipient, aSender, aMsg);
    End;
    sl.free;
    DeleteFile(lowercase(aRecipient));
  End;
End;

Procedure TLANChatServer.Execute;
Var
  c: Char;
Begin
  // TODO: Make Configurable
  If Not fConnection.Listen(1234) Then Begin
    writeln('Error, could not listen on port: 1234');
    exit;
  End;
  frunning := true;
  While frunning Do Begin
    fConnection.CallAction();
    // At least one Participant is lost -> Send new state to all
    If fNeedSendKnownParticipantList Then Begin
      fNeedSendKnownParticipantList := false;
      SendKnownParticipantList();
    End;
    sleep(1);
    If KeyPressed Then Begin
      c := ReadKey;
      If c = #27 Then Begin
        writeln('Going down by user input');
        frunning := false;
      End;
    End;
  End;
End;

Procedure TLANChatServer.Terminate;
Begin
  frunning := false;
End;

End.

