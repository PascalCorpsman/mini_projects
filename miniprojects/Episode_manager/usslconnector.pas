(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of FileChecker2                                          *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit usslconnector;

{$MODE ObjFPC}{$H+}

{.$DEFINE Debug}

Interface

Uses
  Classes, SysUtils;

(*
 * Functions to "talk" to server on lowest level
 *
 * Usage:
 *    1. Login
 *    2. Call whatever functions you want
 *    3. Logout
 *)

Function Login(URL, Port, aClientID, UserName, Password: String): Boolean;

Procedure Logout;

Function DownloadDB(DBUsername: String): TMemorystream;
Function SendDB(Const db: TStream): Boolean;

Function SetPassword(NewPassword: String): Boolean;

Function ReloadSettings(): Boolean;
Function GetDBList(): TStringList;
Function GetUserList(): TStringList;
Function SetUserRights(UserName: String; Rights: Integer): Boolean;
Function delUser(Username: String): Boolean;
Function addUser(Username, Password: String; Rights: integer): Boolean;

(*
 * Helper Routines, that are often needed, when writing a client, more higher level
 *)

// After Successfully logged in, this routine calls
// - GetDBList
// - Popups a dialog which DB to download
// - Downloads the selected DB and returns it as stream
// Nil -> Abort or error
Function RequestaDBAndDownloadIt(): TMemoryStream;

Implementation

Uses
  base64, Dialogs, Forms, StdCtrls, ExtCtrls, Controls
  , DCPrijndael, DCPsha256, DCPcrypt2
  , fphttpclient, opensslsockets
  ;

Var
  LoggedIn: Boolean = false;
  Client: TFPHttpClient = Nil;
  BaseURL: String = '';

Procedure nop();
Begin

End;

Type

  { TDBListQuestionForm }

  TDBListQuestionForm = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    RadioGroup1: TRadioGroup;
    //    Procedure FormCreate(Sender: TObject);
  private

  public
    // Weil die Form keine Ressource hat, muss sie mittels CreateNew erzeugt werden, was auch immer das für einen unterschied macht ...
    Constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
    Procedure InitWith(Const list: TStringList);
  End;

Function Login(URL, Port, aClientID, UserName, Password: String): Boolean;
Var
{$IFDEF Debug}
  i: integer;
  b: Byte;
  ServerToken: TBytes;
{$ENDIF}
  rndString, AuthHeader, Challengeres: String;
  m, unEncryptedSeed, EncryptedSeed: TMemoryStream;
  EncrytpStream: TDCP_rijndael;
Begin
  result := false;
  Logout;
  If port <> '' Then Begin
    url := url + ':' + Port;
  End;
  client := TFPHttpClient.Create(Nil);
  // 1. Anfrage der Challenge
  AuthHeader := 'Username ' + EncodeStringBase64(Username) + ' Client ' + EncodeStringBase64(aClientID);
  client.AddHeader('Authorization', AuthHeader);
  Try
    rndString := client.Get(URL + '/getchallenge');
  Except
    // Wir steigen hier aus, wenn die URL gar nicht erst erreichbar ist..
    On av: Exception Do Begin
      showmessage(av.Message + LineEnding +
        'HTTP.ResultCode: ' + inttostr(client.ResponseStatusCode) + ' ; ' + client.ResponseStatusText
        );
      exit;
    End;
  End;
  If client.ResponseStatusCode <> 200 Then exit;
  rndString := DecodeStringBase64(rndString);
  unEncryptedSeed := TMemoryStream.Create;
  unEncryptedSeed.Write(rndString[1], length(rndString));
{$IFDEF Debug}
  writeln('Got challenge:');
  unEncryptedSeed.Position := 0;
  b := 0;
  For i := 0 To unEncryptedSeed.Size - 1 Do Begin
    unEncryptedSeed.read(b, sizeof(b));
    write(format(' %0.2X', [b]));
  End;
  writeln('');
{$ENDIF}
  unEncryptedSeed.Position := 0;
  // 2. Verschlüsseln des Random mit eigenem PW
  EncryptedSeed := TMemoryStream.Create;
  EncrytpStream := TDCP_rijndael.Create(Nil);
  EncrytpStream.InitStr(Password, TDCP_sha256);
  EncrytpStream.CipherMode := cmCBC; // Cipher-Block Chaining (CBC)
  EncrytpStream.EncryptStream(unEncryptedSeed, EncryptedSeed, unEncryptedSeed.Size);
  EncrytpStream.free;
  unEncryptedSeed.free;
  // 3. Senden Verschlüsseltes Random an Server
  setlength(rndString, EncryptedSeed.Size);
  EncryptedSeed.Position := 0;
  EncryptedSeed.Read(rndString[1], EncryptedSeed.Size);
{$IFDEF Debug}
  writeln('Send challenge responce:');
  EncryptedSeed.Position := 0;
  b := 0;
  For i := 0 To EncryptedSeed.Size - 1 Do Begin
    EncryptedSeed.read(b, sizeof(b));
    write(format(' %0.2X', [b]));
  End;
  writeln('');
{$ENDIF}
  EncryptedSeed.free;
  rndString := EncodeStringBase64(rndString);
  m := TMemoryStream.Create;
  m.Write(rndString[1], length(rndString));
  m.Position := 0;
  Client.RequestBody := m;
  Challengeres := Client.Post(url + '/requesttoken');
  m.free;
  If client.ResponseStatusCode <> 200 Then exit;
  // 4 Antwort Akzeptiert
  Challengeres := DecodeStringBase64(Challengeres);
{$IFDEF Debug}
  ServerToken := Nil;
  setlength(ServerToken, length(Challengeres));
  For i := 0 To high(ServerToken) Do Begin
    ServerToken[i] := ord(Challengeres[i + 1]);
  End;
  writeln('Got token:');
  For i := 0 To high(ServerToken) Do Begin
    write(format(' %0.2X', [ServerToken[i]]));
  End;
  writeln('');
{$ENDIF}
  // TODO: Eigentlich müsste das free und create nicht sein, aber ohne kann man Header nicht neu setzen :/
  client.free;
  client := TFPHttpClient.Create(Nil);
  AuthHeader := 'Bearer ' + EncodeStringBase64(Challengeres) + ' Username ' + EncodeStringBase64(Username) + ' Client ' + EncodeStringBase64(aClientID);
  client.AddHeader('Authorization', AuthHeader);
  BaseURL := url;
  LoggedIn := true;
  result := true;
End;

Procedure Logout;
Begin
  If assigned(Client) Then Begin
    Client.free;
  End;
  Client := Nil;
  BaseURL := '';
  LoggedIn := false;
End;

Function DownloadDB(DBUsername: String): TMemorystream;
Var
  db, dbString: String;
  m: TMemoryStream;
Begin
  result := Nil;
  If Not LoggedIn Then exit;
  DBString := EncodeStringBase64(DBUsername);
  m := TMemoryStream.Create;
  m.Write(DBString[1], length(DBString));
  m.Position := 0;
  Try
    client.RequestBody := m;
    db := client.Get(BaseURL + '/getdb');
  Except
    client.RequestBody := Nil;
    m.free;
    exit;
  End;
  m.free;
  client.RequestBody := Nil;
  If client.ResponseStatusCode <> 200 Then exit;
  dbString := DecodeStringBase64(db);
  result := TMemoryStream.Create;
  result.Write(dbString[1], length(dbString));
  result.Position := 0;
End;

Function SendDB(Const db: TStream): Boolean;
Var
  DBString: String;
  m: TMemoryStream;
Begin
  result := false;
  If Not LoggedIn Then exit;
  DBString := '';
  setlength(DBString, db.Size);
  db.Read(DBString[1], db.Size);
  DBString := EncodeStringBase64(DBString);
  m := TMemoryStream.Create;
  m.Write(DBString[1], length(DBString));
  m.Position := 0;
  Try
    Client.RequestBody := m;
    client.Post(BaseURL + '/setdb');
  Except
    client.RequestBody := Nil;
    m.free;
    exit;
  End;
  m.free;
  client.RequestBody := Nil;
  If client.ResponseStatusCode <> 200 Then exit;
  result := true;
End;

Function SetPassword(NewPassword: String): Boolean;
Var
  m: TMemoryStream;
Begin
  result := false;
  If Not LoggedIn Then exit;
  NewPassword := EncodeStringBase64(NewPassword);
  m := TMemoryStream.Create;
  m.Write(NewPassword[1], length(NewPassword));
  m.Position := 0;
  Try
    Client.RequestBody := m;
    client.Post(BaseURL + '/setpassword');
  Except
    client.RequestBody := Nil;
    m.free;
    exit;
  End;
  m.free;
  client.RequestBody := Nil;
  If client.ResponseStatusCode <> 200 Then exit;
  result := true;
End;

Function ReloadSettings: Boolean;
Begin
  result := false;
  If Not LoggedIn Then exit;
  Try
    client.Post(BaseURL + '/reloadsettings');
  Except
    exit;
  End;
  If client.ResponseStatusCode <> 200 Then exit;
  result := true;
End;

Function GetDBList: TStringList;
Var
  dbList: String;
Begin
  result := Nil;
  If Not LoggedIn Then exit;
  Try
    dbList := client.Get(BaseURL + '/getdblist');
  Except
    exit;
  End;
  If client.ResponseStatusCode <> 200 Then exit;
  dbList := DecodeStringBase64(dbList);
  result := TStringList.Create;
  result.Text := dbList;
End;

Function GetUserList: TStringList;
Var
  dbList: String;
Begin
  result := Nil;
  If Not LoggedIn Then exit;
  Try
    dbList := client.Get(BaseURL + '/getuserlist');
  Except
    exit;
  End;
  If client.ResponseStatusCode <> 200 Then exit;
  dbList := DecodeStringBase64(dbList);
  result := TStringList.Create;
  result.Text := dbList;
End;

Function SetUserRights(UserName: String; Rights: Integer): Boolean;
Var
  m: TMemoryStream;
  content: String;
Begin
  result := false;
  If Not LoggedIn Then exit;
  content := EncodeStringBase64(format('%s;%d', [UserName, Rights]));
  m := TMemoryStream.Create;
  m.Write(content[1], length(content));
  m.Position := 0;
  Try
    Client.RequestBody := m;
    client.Post(BaseURL + '/setuserright');
  Except
    Client.RequestBody := Nil;
    m.free;
    exit;
  End;
  m.free;
  Client.RequestBody := Nil;
  If client.ResponseStatusCode <> 200 Then exit;
  result := true;
End;

Function delUser(Username: String): Boolean;
Var
  m: TMemoryStream;
  content: String;
Begin
  result := false;
  If Not LoggedIn Then exit;
  content := EncodeStringBase64(Username);
  m := TMemoryStream.Create;
  m.Write(content[1], length(content));
  m.Position := 0;
  Try
    Client.RequestBody := m;
    client.Post(BaseURL + '/deluser');
  Except
    Client.RequestBody := Nil;
    m.free;
    exit;
  End;
  m.free;
  Client.RequestBody := Nil;
  If client.ResponseStatusCode <> 200 Then exit;
  result := true;
End;

Function addUser(Username, Password: String; Rights: integer): Boolean;
Var
  m: TMemoryStream;
  content: String;
Begin
  result := false;
  If Not LoggedIn Then exit;
  content := EncodeStringBase64(format('%d;%s;%s', [Rights, Username, Password]));
  m := TMemoryStream.Create;
  m.Write(content[1], length(content));
  m.Position := 0;
  Try
    Client.RequestBody := m;
    client.Post(BaseURL + '/adduser');
  Except
    Client.RequestBody := Nil;
    m.free;
    exit;
  End;
  m.free;
  Client.RequestBody := Nil;
  If client.ResponseStatusCode <> 200 Then exit;
  result := true;
End;

Function RequestaDBAndDownloadIt(): TMemoryStream;
Var
  sl: TStringList;
  f: TDBListQuestionForm;
Begin
  result := Nil;
  If Not LoggedIn Then Begin
    showmessage('Error, not logged in.');
    exit;
  End;
  sl := GetDBList();
  If Not assigned(sl) Then Begin
    showmessage('Error, unable to load database list.');
    Logout;
    sl.free;
    exit;
  End;
  If sl.Count = 0 Then Begin
    showmessage('Error, no databases on the server available.');
    Logout;
    sl.free;
    exit;
  End;
  f := TDBListQuestionForm.CreateNew(Nil);
  f.InitWith(sl);
  sl.free;
  If f.ShowModal <> mrOK Then Begin
    f.free;
    exit;
  End;
  result := DownloadDB(f.RadioGroup1.Items[f.RadioGroup1.ItemIndex]);
  f.free;
End;

{ TDBListQuestionForm }

Constructor TDBListQuestionForm.CreateNew(AOwner: TComponent; Num: Integer);
Begin
  Inherited CreateNew(AOwner, Num);
  position := poScreenCenter;
  width := 324;
  height := 162;
  caption := 'Select database';
  button1 := TButton.Create(self);
  button1.name := 'Button1';
  button1.Parent := self;
  button1.Top := 128;
  button1.Left := 237;
  button1.ModalResult := mrOK;
  button1.Anchors := [akRight, akBottom];
  button1.Caption := 'OK';

  button2 := TButton.Create(self);
  button2.name := 'Button2';
  button2.Parent := self;
  button2.Top := 128;
  button2.Left := 8;
  button2.ModalResult := mrCancel;
  button2.Anchors := [akLeft, akBottom];
  button2.Caption := 'Cancel';

  RadioGroup1 := TRadioGroup.Create(self);
  RadioGroup1.name := 'RadioGroup1';
  RadioGroup1.Parent := self;
  RadioGroup1.Top := 8;
  RadioGroup1.Left := 8;
  RadioGroup1.Width := 304;
  RadioGroup1.Height := 105;
  RadioGroup1.Caption := '';
End;

Procedure TDBListQuestionForm.InitWith(Const list: TStringList);
Var
  i: Integer;
Begin
  RadioGroup1.Items.Clear;
  For i := 0 To list.Count - 1 Do Begin
    RadioGroup1.Items.Add(list[i]);
  End;
  If RadioGroup1.Items.Count <> 0 Then RadioGroup1.ItemIndex := 0;
End;

End.

