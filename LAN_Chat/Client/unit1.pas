(******************************************************************************)
(* Lan chat                                                        03.12.2023 *)
(*                                                                            *)
(* Version     : 0.11                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Simple server based chat Program for local networks          *)
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
(* Wishlist    : - Filetransfer bytes / second measuring                      *)
(*                                                                            *)
(* Known Issues: none                                                         *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*               0.02 - File transfer support                                 *)
(*                      Adjustable sound volume                               *)
(*               0.03 - Adjust HTML-Chat formating to support                 *)
(*                        HtmlView and IpHtml                                 *)
(*                    - Abort file transfer                                   *)
(*                    - Auto reconnect                                        *)
(*               0.04 - Fix Wrong coloring on received messages               *)
(*               0.05 - Minor file transfer control flow fixes                *)
(*               0.06 - Improve Gui, support for Hot Links                    *)
(*                      Filedrop support                                      *)
(*                      improve error messages on file transfer reject        *)
(*               0.07 - Trayicon for Linux and autofocus on Edit field        *)
(*               0.08 - Emoji shortcuts                                       *)
(*                      DND Button - unterdrückung von Show on New Message    *)
(*               0.09 - UniqueInstance                                        *)
(*               0.10 - Add some keyboard shortcuts for Emoji'S               *)
(*               0.11 - UDP- Broadcast to auto detect server                  *)
(*                      Show Byte progress during transfer                    *)
(*                                                                            *)
(******************************************************************************)
(*  Silk icon set 1.3 used                                                    *)
(*  ----------------------                                                    *)
(*  Mark James                                                                *)
(*   https://peacocksoftware.com/silk                                         *)
(*                                                                            *)
(*  This work is licensed under a                                             *)
(*  Creative Commons Attribution 2.5 License.                                 *)
(*  [ http://creativecommons.org/licenses/by/2.5/ ]                           *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, IniPropStorage,
  StdCtrls, ExtCtrls, PairSplitter, Buttons, Menus, UniqueInstance,
  lNetComponents, lNet, uChunkmanager, Types, BASS, IpHtml
  ;


Const
  ILBell = 0; // Deprecated
  ILGear = 1; // Deprecated
  ILOffline = 2;
  ILOnline = 3;
  ILNewMessage = 4;
  ILUploadFile = 5;
  ILServerSetup = 6;
  ILUserDND = 7; // -- Not yet used
  ILDND = 8;
  ILChats = 9;
  ILDNDCHats = 10;

  (*
   * Laut Chunkmanager Dokumentation darf man nicht mehr als 64KB auf 1 mal senden (=> die Paketgröße muss also unter 64KB bleiben)
   * -> (1024 + X) * (64 - 1) ist maximal aber eben noch drunter
   *
   * => Eigentlich sollte der ChunkManager diese Grenze obsolet machen. Es scheint aber so zu sein, dass das Paket komplett in den Puffer geht
   *    und dann triggert L-Net die OnCanSend nicht mehr, stattdessen geht die CPU auf 100% ...
   *
   * BTW. Es ist egal ob man 1 Paket > 64KB sendet, oder ob man in der selben Zeitscheibe N-Chunks sendet, welche zusammen mehr als 64 KB haben...
   *)

  ChunkSize = 1024; // Anzahl an Bytes die beim File Transfer auf einen Schlag versendet werden
  PacketSize = 64 - 1; // Macht man das zu Groß dann Knallt es irgendwo, aber wo ist die Magische Grenze und warum knallt es ?

  //  ChunkSize = 128; // Debugg !! Attention, transfer will be incredible slow, due to the massive message overhead !
  //  PacketSize = 8; // Debugg !! Attention, transfer will be incredible slow, due to the massive message overhead !

Type

  TColorDest = (cdLeft, cdRight, cdTimeStamp, cdBackground);

  TFileSendData = Record
    // 0 = Idle
    // 1 = Waiting for Request Result
    // 2 = receiving data
    // 3 = Sending
    State: Integer;
    Filename: String; // Quell / Ziel Dateiname, je nachdem von Welcher Seite wir kommen ;)
    FileStream: TFileStream; // Filehandle to store / load
    aPosition: int64; // The Amount of bytes that are transfered up until now
    aSize: int64; // Total Filesize when finished

    FileSender: String; // Derjenige der die Datei sendet
    FileReceiver: String; // The Name of the Participant which will receive the file
    ChunkCounter: Integer; // Der Empfänger muss die Chunks mit zählen, damit er weis wann er Quitieren soll
  End;

  TPosition = (pRight, pLeft);

  TParticipant = Record
    Name: String; // Der Name des anderen Users
    nm: Boolean; // True, wenn der User noch ungesehene Neue Nachrichten hat
    Online: Boolean; // True = User ist Online, false = User ist Offline
  End;

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Edit1: TEdit;
    ImageList1: TImageList;
    IniPropStorage1: TIniPropStorage;
    Label1: TLabel;
    ListBox1: TListBox;
    LTCPComponent1: TLTCPComponent;
    LUDPComponent1: TLUDPComponent;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    OpenDialog1: TOpenDialog;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    SaveDialog1: TSaveDialog;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    Timer1: TTimer;
    TrayIcon1: TTrayIcon;
    UniqueInstance1: TUniqueInstance;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Edit1KeyPress(Sender: TObject; Var Key: char);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDropFiles(Sender: TObject; Const FileNames: Array Of String);
    Procedure FormShow(Sender: TObject);
    Procedure IpHtmlPanel1HotClick(Sender: TObject);
    Procedure ListBox1Click(Sender: TObject);
    Procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    Procedure LTCPComponent1Connect(aSocket: TLSocket);
    Procedure LTCPComponent1Disconnect(aSocket: TLSocket);
    Procedure LUDPComponent1Receive(aSocket: TLSocket);
    Procedure MenuItem1Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure MenuItem3Click(Sender: TObject);
    Procedure MenuItem4Click(Sender: TObject);
    Procedure MenuItem5Click(Sender: TObject);
    Procedure MenuItem6Click(Sender: TObject);
    Procedure SpeedButton1Click(Sender: TObject);
    Procedure SpeedButton2Click(Sender: TObject);
    Procedure SpeedButton3Click(Sender: TObject);
    Procedure Timer1Timer(Sender: TObject);
    Procedure TrayIcon1Click(Sender: TObject);
    Procedure UniqueInstance1OtherInstance(Sender: TObject;
      ParamCount: Integer; Const Parameters: Array Of String);
  public
    HtmlViewer1: TIpHtmlPanel;
  private
    defcaption: String;
    fconnection: TChunkManager;
    fParticipants: Array Of TParticipant;

    NM_Sound: HSTREAM; // für PlayInfoSound;
    FileSendData: TFileSendData; // Fürs File Transfer

    Procedure LoadTextToHTMLView(Const aText: String);

    Procedure PlayInfoSound;

    Procedure ConnectToServer();
    Procedure OnReceivedChunk(Sender: TObject; Const Chunk: TChunk);

    Procedure HandleNewParticipantResult(Const Chunk: TChunk);
    Procedure HandleKnownParticipantList(Const Chunk: TChunk);
    Procedure HandleGetMessage(Const Chunk: TChunk);
    Procedure HandleFileTransferRequest(Const Chunk: TChunk);
    Procedure HandleFileTransferRequestResult(Const Chunk: TChunk);
    Procedure HandleReceiveFileTransferContent(Const Chunk: TChunk);
    Procedure HandleTransferNextPacket(Const Chunk: TChunk);
    Procedure HandleTransferFileComplete(Const Chunk: TChunk);
    Procedure HandleAbortFile(Const Chunk: TChunk);
    Procedure HandleLoginToServerSettingsResult(Const Chunk: TChunk);
    Procedure HandlePasswordChangeResult(Const Chunk: TChunk);
    Procedure HandleRemoveKnownParticipantResult(Const Chunk: TChunk);

    Procedure OpenOptions();
    Procedure AppendLog(aParticipant: String; aPos: TPosition; aMessage: String);
    Procedure SendFile(Const Filename: String);

    Procedure SetColorForChat(aChatName: String; aColor: TColor; aDest: TColorDest);
    Procedure SetNotConnected;
    Procedure SendFileChunk();
    Procedure AbortFileTransfer(Sender: TObject);
  public

  End;

Var
  Form1: TForm1;
  Form1ShowOnce: Boolean = true;

Implementation

{$R *.lfm}

Uses
  Unit2 // Settings
  , unit3 // File Transfer progress
  , unit4 // Server Settings
  , unit5 // Emoji's
  , math
  , ulanchatcommon, md5, LCLType, Clipbrd, FileUtil, LazFileUtils, LCLIntf, strutils;

Function ColorAsHTMLColor(aColor: TColor): String;
Var
  r, g, b: Byte;
Begin
  r := aColor And $FF;
  g := (aColor And $FF00) Shr 8;
  b := (aColor And $FF0000) Shr 16;
  result := format('#%0.2X%0.2X%0.2X', [r, g, b]);
End;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
  //- Via UDP-Broadcast nen Server suchen und dessen IP dann vorschlagen in den Einstellungen !
  //- Löschen Alter Nachrichten  (1 Tag und älter)
  //- Auto Update
  //- CI/CD in GIT
  //- Deaktivieren des Connect Timers bei falschen Settings.!
  defcaption := 'Lan chat ver. 0.11';
  (*
   * Know Bug: das ding scrollt nicht immer sauber nach unten..
   *)
  // TODO: Ist das so clever, das jeder user seine Farbe selbst bestimmen darf und die bei den Anderen auch so angezeigt wird ?
  (*
   * Die Linux Version von Lazarus tickt komplett aus, wenn diese Komponente auf dem Formular ist
   * -> Also erzeugen wir sie zur Laufzeit ;)
   *)
  HtmlViewer1 := TIpHtmlPanel.Create(PairSplitter1.Sides[1]);
  HtmlViewer1.OnHotClick := @IpHtmlPanel1HotClick;
  HtmlViewer1.Name := 'HtmlViewer1';
  HtmlViewer1.Parent := PairSplitter1.Sides[1];
  HtmlViewer1.Left := 8;
  HtmlViewer1.Height := 480;
  HtmlViewer1.Top := 0;
  HtmlViewer1.Width := 515;
  HtmlViewer1.Anchors := [akTop, akLeft, akRight, akBottom];
  HtmlViewer1.PopupMenu := PopupMenu1;
  HtmlViewer1.TabOrder := 2;

  IniPropStorage1.IniFileName := 'f_client.settings';
  fconnection := TChunkManager.create;
  fconnection.RegisterConnection(LTCPComponent1);
  fconnection.OnReceivedChunk := @OnReceivedChunk;
  edit1.text := '';
  If (BASS_GetVersion() Shr 16) <> Bassversion Then Begin
    showmessage('Unable to init the Bass Library ver. :' + BASSVERSIONTEXT);
    halt;
  End;
  If Not Bass_init(-1, 44100, BASS_DEVICE_DMIX, {$IFDEF Windows}0{$ELSE}Nil{$ENDIF}, Nil) Then Begin
    showmessage('Unable to init the device, Error code :' + inttostr(BASS_ErrorGetCode));
    halt;
  End;
  NM_Sound := BASS_StreamCreateFile(false, pchar('nm.wav'), 0, 0, 0);
  If NM_Sound = 0 Then Begin
    showmessage('Error unable to load : nm.wav' + LineEnding + 'Error code :' + inttostr(BASS_ErrorGetCode));
  End;
  ListBox1.Color := IniPropStorage1.ReadInteger('BackColor', clwhite);
  SetNotConnected;
  Timer1.Enabled := false;
End;

Procedure TForm1.FormDropFiles(Sender: TObject; Const FileNames: Array Of String
  );
Begin
  SendFile(FileNames[0]);
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Var
  i: Integer;
Begin
  If FileSendData.State <> 0 Then Begin
    // Hoffentlich kommt die Nachricht noch Raus
    AbortFileTransfer(Nil);
    For i := 0 To 100 Do Begin
      fconnection.CallAction();
      Application.ProcessMessages;
    End;
  End;
  fconnection.Disconnect(true);
  Application.ProcessMessages;
  fconnection.Free;
  // Free Bass
  If NM_Sound <> 0 Then
    If Not BASS_ChannelStop(NM_Sound) Then
      Showmessage('Error could not stop player, Error code :' + inttostr(BASS_ErrorGetCode));
  If Not Bass_Free Then
    showmessage('Unable to free Bass, Error code :' + inttostr(BASS_ErrorGetCode));
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button1Click(Sender: TObject);
  Function EncapsulateURLsWithHRef(value: String): String;
  Var
    i, j: integer;
    pre, suff, url: String;
  Begin
    // TODO: Bei mehr wie einem Link knallt dass
    i := pos('http', value);
    If i = 0 Then Begin
      result := value;
    End
    Else Begin
      pre := copy(value, 1, i - 1);
      url := '';
      For j := i To length(value) Do Begin
        If value[j] = ' ' Then Begin
          url := copy(value, i + 1, j - i);
          suff := copy(value, j, length(value));
          break;
        End;
      End;
      If url = '' Then Begin
        url := copy(value, i, length(value));
        suff := '';
      End;
      result := pre + ' <a href="' + url + '"> ' + url + ' </a> ' + suff;
      result := trim(result);
    End;
  End;

Var
  aText, aMsg: String;
  data: TMemoryStream;
Begin
  // Send
  If Not fconnection.Connected Then Begin
    showmessage('Error not connected.');
    SetNotConnected;
    exit;
  End;
  If ListBox1.ItemIndex = -1 Then Begin
    showmessage('Error, no recipient selected.');
    exit;
  End;
  If trim(Edit1.Text) = '' Then Begin
    exit;
  End;
  (*
   * Wenn Nachricht enthält  http<Irgendwas> aber ohne href
   *
   * <a href="http<Irgendwas>">http<Irgendwas><\a>
   *
   *)
  aText := EncapsulateURLsWithHRef(Edit1.Text);
  aText := StringReplace(aText, '(y)', '👍', [rfReplaceAll]);
  aText := StringReplace(aText, '(n)', '👎', [rfReplaceAll]);
  aText := StringReplace(aText, ':)', '🙂', [rfReplaceAll]);
  aText := StringReplace(aText, ';)', '😉', [rfReplaceAll]);
  aText := StringReplace(aText, ':D', '😁', [rfReplaceAll]);
  aText := StringReplace(aText, '8|', '😎', [rfReplaceAll]);
  aText := StringReplace(aText, ':(', '☹️', [rfReplaceAll]);
  aText := StringReplace(aText, '<3', '❤️', [rfReplaceAll]);
  edit1.text := '';
  aMsg := ' <font color="TimeColor"> ' + formatdatetime('DD.MM.YYY HH:MM:SS', now) + ' </font> <br> ' + aText + ' ';
  data := TMemoryStream.Create;
  data.WriteAnsiString(ListBox1.Items[ListBox1.ItemIndex]);
  data.WriteAnsiString(IniPropStorage1.ReadString('UserName', ''));
  data.WriteAnsiString(aMsg);
  fconnection.SendChunk(Msg_Message, data);
  aMsg := ' <font color="' + ColorAsHTMLColor(IniPropStorage1.ReadInteger('TimeColor', $808080)) + '"> ' + formatdatetime('DD.MM.YYY HH:MM:SS', now) + ' </font> <br> ' + aText + ' ';

  AppendLog(ListBox1.Items[ListBox1.ItemIndex], pRight, aMsg);
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  MenuItem6Click(Nil);
  TrayIcon1.Show;
  visible := false;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Var
  p: TPoint;
Begin
  p := ControlToScreen(point(button4.left, button4.Top + button4.Height));
  form5.Top := p.Y + Scale96ToForm(10);
  form5.left := p.x + Scale96ToForm(165);
  form5.ShowModal;
End;

Procedure TForm1.Edit1KeyPress(Sender: TObject; Var Key: char);
Begin
  If key = #13 Then Button1.Click;
End;

Procedure TForm1.FormShow(Sender: TObject);
Var
  fServerIP: String;
Begin
  If Form1ShowOnce Then Begin
    Form1ShowOnce := false;
    form3.RegisterAbortCallback(@AbortFileTransfer);
    fServerIP := IniPropStorage1.ReadString('ServerIP', '');
    If fServerIP = '' Then Begin
      showmessage('Error, no server set, please edit settings, first.');
      OpenOptions();
    End
    Else Begin
      ConnectToServer();
      If IniPropStorage1.ReadBoolean('Start_hidden', false) Then Begin
        Button3.Click; // Hide
      End;
    End;
  End;
  If visible Then Edit1.SetFocus;
End;

Procedure TForm1.IpHtmlPanel1HotClick(Sender: TObject);
Begin
  If HtmlViewer1.HotURL <> '' Then openurl(HtmlViewer1.HotURL);
End;

Procedure TForm1.ListBox1Click(Sender: TObject);
Var
  sl: TStringList;
  aParticipant: String;
  i: Integer;

Begin
  // Auswählen eines Chats
  //TODO: ggf den alten Chat wegspeichern
  If ListBox1.ItemIndex <> -1 Then Begin
    aParticipant := ListBox1.Items[ListBox1.ItemIndex];
  End
  Else Begin
    // Es gibt keine Chats
    exit;
  End;
  sl := TStringList.Create;
  If FileExists(lowercase(aParticipant) + '.chat') Then Begin
    sl.LoadFromFile(lowercase(aParticipant) + '.chat');
  End
  Else Begin
    // Eine Leere Seite
    sl.text := '<!DOCTYPE html>' + LineEnding +
      '<html>' + LineEnding +
      '<head>' + LineEnding +
      '</head>' + LineEnding +
      '<body style="background-color: ' + ColorAsHTMLColor(IniPropStorage1.ReadInteger('BackColor', clwhite)) + ';">' + LineEnding +
      'no chats found..' + LineEnding +
      '</body>' + LineEnding +
      '</html>';
  End;
  LoadTextToHTMLView(sl.text);
  sl.free;
  // Löschen der Markierung "ungelesen"
  For i := 0 To high(fParticipants) Do Begin
    If LowerCase(fParticipants[i].Name) = lowercase(aParticipant) Then Begin
      If fParticipants[i].nm Then Begin
        fParticipants[i].nm := false;
        ListBox1.Invalidate;
      End;
    End;
  End;
End;

Procedure TForm1.ListBox1DrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
Var
  s: String;
  offset, i: Integer;
  r: TRect;
  imageIndex: Integer;
Begin
  s := lowercase(ListBox1.items[index]);
  For i := 0 To high(fParticipants) Do Begin
    If lowercase(fParticipants[i].Name) = s Then Begin
      If fParticipants[i].nm Then Begin
        imageIndex := ILNewMessage;
      End
      Else Begin
        If fParticipants[i].Online Then Begin
          imageIndex := ILOnline;
        End
        Else Begin
          imageIndex := ILOffline;
        End;
      End;
      break;
    End;
  End;
  // Clear Background
  ListBox1.Canvas.Brush.Color := IniPropStorage1.ReadInteger('BackColor', clwhite);
  ListBox1.Canvas.pen.Color := IniPropStorage1.ReadInteger('BackColor', clwhite);
  If odSelected In State Then Begin
    ListBox1.Canvas.Brush.Color := clMenuHighlight;
    ListBox1.Canvas.pen.Color := clMenuHighlight;
  End;
  ListBox1.Canvas.Rectangle(ARect);
  // Render User info
  r.left := 8;
  r.top := ARect.Top;
  r.Bottom := ARect.Bottom;
  r.Right := ARect.Bottom - ARect.Top;
  ImageList1.StretchDraw(ListBox1.Canvas, imageIndex, r);
  offset := r.Right - r.Left + 8;
  // Render Username
  ListBox1.Canvas.Font.Color := IniPropStorage1.ReadInteger('OtherColor', clblack);
  ListBox1.Canvas.TextOut(ARect.Left + 8 + offset, (ARect.Top + ARect.Bottom - ListBox1.Canvas.TextHeight(ListBox1.items[index])) Div 2, ListBox1.items[index]);
End;

Procedure TForm1.LTCPComponent1Connect(aSocket: TLSocket);
Var
  data: TMemoryStream;
  fUser, fpw, fpwHash: String;
  i: integer;
Begin
  // Sobald wir verbunden sind melden wir uns beim Server an
  data := TMemoryStream.Create;
  fUser := IniPropStorage1.ReadString('UserName', '');
  fpw := IniPropStorage1.ReadString('Password', '');
  If fpw = '' Then Begin
    fpw := PasswordBox('Question', 'Please enter password for ' + fUser);
  End;
  If fpw = '' Then Begin
    fconnection.Disconnect(true);
    ShowMessage('No access without password, skip now.');
    exit;
  End;
  fpwHash := MD5Print(MD5String(fpw));
  data.WriteAnsiString(fUser);
  data.WriteAnsiString(fpwHash);
  i := ProtokollVersion;
  data.Write(i, sizeof(i));
  fconnection.SendChunk(MSG_New_Participant, data);
  Timer1.Enabled := false;
End;

Procedure TForm1.LTCPComponent1Disconnect(aSocket: TLSocket);
Begin
  SetNotConnected;
End;

Procedure TForm1.LUDPComponent1Receive(aSocket: TLSocket);
Var
  Buffer: Array[0..1023] Of byte;
  cnt: integer;
  B: Byte;
  i: Integer;
  serverIP, ServerInfo, ServerPort: String;
Begin
  Repeat
    cnt := aSocket.Get(buffer, 1024);
    If cnt <> 0 Then Begin
      b := UDPRandomChiffre;
      ServerInfo := '';
      setlength(ServerInfo, cnt - 1);
      For i := 0 To cnt - 1 Do Begin
        b := b Xor buffer[i];
        If i <> cnt - 1 Then Begin
          ServerInfo[i + 1] := chr(buffer[i]);
        End;
      End;
      If b = 0 Then Begin // Entschlüsselung geglückt
        serverIP := aSocket.PeerAddress;
        If serverIP <> '127.0.0.1' Then Begin // Den Loopback adapter klammern wir aus, da der auch über die IP der Netzwerkkarte rein kommt und sonst doppelt wäre
          ServerPort := copy(ServerInfo, pos(':', ServerInfo) + 1, length(ServerInfo));
          form2.Edit1.Text := serverIP;
          form2.Edit2.Text := ServerPort;
          form2.Timer1.Enabled := false;
        End;
      End;
    End;
  Until cnt = 0;
End;

Procedure TForm1.MenuItem1Click(Sender: TObject);
Begin
  // Clear Chat History
  If ListBox1.ItemIndex = -1 Then exit;
  If FileExists(lowercase(ListBox1.Items[ListBox1.ItemIndex]) + '.chat') Then Begin
    DeleteFile(lowercase(ListBox1.Items[ListBox1.ItemIndex]) + '.chat');
  End;
  ListBox1.Click;
End;

Procedure TForm1.MenuItem2Click(Sender: TObject);
Begin
  // Send a file
  If OpenDialog1.Execute Then Begin
    SendFile(OpenDialog1.FileName);
  End;
End;

Procedure TForm1.MenuItem3Click(Sender: TObject);
Begin
  TrayIcon1Click(Nil);
End;

Procedure TForm1.MenuItem4Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.MenuItem5Click(Sender: TObject);
Begin
  SpeedButton1.Click;
End;

Procedure TForm1.MenuItem6Click(Sender: TObject);
Var
  b: TBitmap;
  index, i: integer;
Begin
  b := TBitmap.Create;
  b.Width := ImageList1.Width;
  b.Height := ImageList1.Height;
  If MenuItem6.Checked Then Begin
    index := ILDND;
    // Wenn irgend ein Chat eine Ungelesene Nachricht hat, passen wir das Icon entsprechend an ..
    For i := 0 To High(fParticipants) Do Begin
      If fParticipants[i].nm Then Begin
        index := ILDNDCHats;
        break;
      End;
    End;
  End
  Else Begin
    index := ILChats;
  End;
  b.canvas.Brush.Color := clWhite;
  b.canvas.Rectangle(-1, -1, 17, 17);
  b.TransparentColor := clWhite;
  b.Transparent := true;
  ImageList1.Draw(b.Canvas, 0, 0, index);
  TrayIcon1.Icon.Assign(b);
  TrayIcon1.InternalUpdate;
  TrayIcon1.Show;
  //  Form1.Invalidate;
  b.free;
End;

Procedure TForm1.SpeedButton1Click(Sender: TObject);
Begin
  // Optionen
  OpenOptions();
End;

Procedure TForm1.SpeedButton2Click(Sender: TObject);
Begin
  MenuItem2Click(Nil);
End;

Procedure TForm1.SpeedButton3Click(Sender: TObject);
Var
  pw, pwhash: String;
  data: TMemoryStream;
Begin
  // Server Settings
  pw := PasswordBox('Action', 'Please enter server password to access server settings');
  If trim(pw) = '' Then exit;
  pwhash := MD5Print(MD5String(pw));
  data := TMemoryStream.Create;
  data.WriteAnsiString(pwhash);
  fconnection.SendChunk(MSG_Login_to_server_settings, data);
End;

Procedure TForm1.Timer1Timer(Sender: TObject);
Begin
  // Reconnect alle 5s
  ConnectToServer();
End;

Procedure TForm1.TrayIcon1Click(Sender: TObject);
Begin
  // Show
  TrayIcon1.Visible := false;
  Visible := true;
  MenuItem6.Checked := false; // Das DND nehmen wir automatisch weg, weil der User nun ja definitiv wieder was sehen will..
  BringToFront;
End;

Procedure TForm1.UniqueInstance1OtherInstance(Sender: TObject;
  ParamCount: Integer; Const Parameters: Array Of String);
Begin
  If Visible Then Begin
    form1.BringToFront;
  End
  Else Begin
    MenuItem3Click(Nil);
  End;
End;

Procedure TForm1.LoadTextToHTMLView(Const aText: String);
Var
  fs: TStringStream;
  pHTML: TIpHtml;
Begin
  Try
    fs := TStringStream.Create(aText);
    Try
      pHTML := TIpHtml.Create; // Beware: Will be freed automatically by IpHtmlPanel1
      pHTML.LoadFromStream(fs);
    Finally
      fs.Free;
    End;
    HtmlViewer1.SetHtml(pHTML);
    Application.ProcessMessages;
    // TODO: Warum geht das unter Linux nicht ?
    HtmlViewer1.VScrollPos := high(integer);
  Except
    On E: Exception Do Begin
      MessageDlg('TForm1.LoadTextToHTMLView: ' + E.Message, mtError, [mbCancel], 0);
    End;
  End;
  PairSplitter1.Invalidate;
End;

Procedure TForm1.PlayInfoSound;
Begin
  If IniPropStorage1.ReadBoolean('Play_Sound', true) Then Begin
    If Not BASS_SetConfig(BASS_CONFIG_GVOL_STREAM, IniPropStorage1.ReadInteger('Volume', 10000)) Then // global MOD volume (0-10000)
      showmessage('Error could not Switch the Volume, Error code :' + inttostr(BASS_ErrorGetCode));
    If NM_Sound <> 0 Then
      If Not BASS_ChannelPlay(NM_Sound, false) Then
        showmessage('Error could not play in channel, Error code :' + inttostr(BASS_ErrorGetCode));
  End;
End;

Procedure TForm1.ConnectToServer;
Var
  fServerPort: LongInt;
  fServerIP, fUser: String;
Begin
  Timer1.Enabled := false;
  FileSendData.State := 0;
  If LTCPComponent1.Connected Then Begin
    LTCPComponent1.Disconnect(true);
  End;
  fServerPort := IniPropStorage1.ReadInteger('Port', 1234);
  fServerIP := IniPropStorage1.ReadString('ServerIP', '');
  fUser := IniPropStorage1.ReadString('UserName', '');
  If (trim(fUser) = '') Or (trim(fServerIP) = '') Then Begin
    SpeedButton1.Click;
    exit;
  End;
  If Not fconnection.Connect(fServerIP, fServerPort) Then Begin
    showmessage('Error, could not connect to server.');
    SetNotConnected;
  End;
End;

Procedure TForm1.OnReceivedChunk(Sender: TObject; Const Chunk: TChunk);
Begin
  Case Chunk.UserDefinedID Of
    MSG_New_Participant_Result: HandleNewParticipantResult(Chunk);
    MSG_Known_Participant_List: HandleKnownParticipantList(Chunk);
    Msg_Message: HandleGetMessage(Chunk);
    MSG_File_Transfer_Request: HandleFileTransferRequest(Chunk);
    MSG_File_Transfer_Request_result: HandleFileTransferRequestResult(Chunk);
    MSG_File_Transfer_FileContent: HandleReceiveFileTransferContent(Chunk);
    MSG_File_Transfer_File_Next_Packet: HandleTransferNextPacket(chunk);
    MSG_File_Transfer_FileComplete: HandleTransferFileComplete(chunk);
    MSG_File_Transfer_File_Abort: HandleAbortFile(chunk);
    MSG_Login_to_server_settings_Result: HandleLoginToServerSettingsResult(Chunk);
    MSG_Change_Password_Result: HandlePasswordChangeResult(Chunk);
    MSG_Remove_Known_Participant_Result: HandleRemoveKnownParticipantResult(Chunk);
  Else Begin
      showmessage('Error, got unknown message id: ' + inttostr(Chunk.UserDefinedID));
    End;
  End;
End;

Procedure TForm1.HandleNewParticipantResult(Const Chunk: TChunk);
Var
  Allowed: Boolean;
  aError: uint16;
Begin
  allowed := false;
  Chunk.Data.Read(Allowed, SizeOf(Allowed));
  aError := $FFFF;
  Chunk.Data.Read(aError, sizeof(aError));
  If Allowed Then Begin
    // Nichts zu tun die Participant Liste kommt von alleine
    caption := defcaption + ', logged in as: ' + IniPropStorage1.ReadString('Username', '');
  End
  Else Begin
    caption := defcaption + ', not connected.';
    showmessage('Error, server did not accept connection: ' + ErrorcodeToString(aError));
    fconnection.Disconnect(true);
  End;
End;

Procedure TForm1.HandleKnownParticipantList(Const Chunk: TChunk);

  Procedure Quick(li, re: integer);
  Var
    l, r: Integer;
    p: String;
    h: TParticipant;
  Begin
    If Li < Re Then Begin
      p := lowercase(fParticipants[Trunc((li + re) / 2)].Name); // Auslesen des Pivo Elementes
      l := Li;
      r := re;
      While l < r Do Begin
        While CompareStr(lowercase(fParticipants[l].Name), p) < 0 Do
          inc(l);
        While CompareStr(lowercase(fParticipants[r].Name), p) > 0 Do
          dec(r);
        If L <= R Then Begin
          h := fParticipants[l];
          fParticipants[l] := fParticipants[r];
          fParticipants[r] := h;
          inc(l);
          dec(r);
        End;
      End;
      quick(li, r);
      quick(l, re);
    End;
  End;

Var
  aName, oName, s: String;
  cnt, i, j: integer;
  bakup: Array Of TParticipant;
  aOnline: Boolean;
Begin
  // Merken des bisher ausgewählten Users, falls es einen gibt ..
  s := '';
  If ListBox1.Items.Count <> 0 Then Begin
    s := ListBox1.Items[ListBox1.ItemIndex];
  End;
  ListBox1.Clear;
  cnt := 0;
  chunk.Data.Read(cnt, sizeof(cnt));
  bakup := Nil;
  setlength(bakup, length(fParticipants));
  For i := 0 To high(bakup) Do Begin
    bakup[i].Name := fParticipants[i].Name;
    bakup[i].nm := fParticipants[i].nm;
  End;
  setlength(fParticipants, cnt);
  oName := IniPropStorage1.ReadString('Username', '');
  For i := 0 To high(fParticipants) Do Begin
    aName := Chunk.Data.ReadAnsiString;
    aOnline := false;
    Chunk.Data.Read(aOnline, sizeof(aOnline));
    fParticipants[i].Name := aName;
    fParticipants[i].nm := false;
    fParticipants[i].Online := aOnline;
    For j := 0 To high(bakup) Do Begin
      // "retten" des Alle Nachrichten gelesen flags
      If (lowercase(bakup[j].Name) = LowerCase(aName))
        And (LowerCase(aName) <> lowercase(oName)) Then Begin
        fParticipants[i].nm := bakup[j].nm;
        break;
      End;
    End;
  End;
  // Sortieren der Participants Aufsteigend nach Name
  Quick(0, high(fParticipants));
  For i := 0 To high(fParticipants) Do Begin
    If lowercase(fParticipants[i].Name) <> lowercase(oName) Then Begin
      ListBox1.Items.Add(fParticipants[i].Name);
    End;
  End;
  // Anwählen des alten Chats
  If s <> '' Then Begin
    For i := 0 To ListBox1.Items.Count - 1 Do Begin
      If LowerCase(s) = LowerCase(ListBox1.Items[i]) Then Begin
        ListBox1.ItemIndex := i;
        ListBox1.Click;
        break;
      End;
    End;
  End
  Else Begin
    If ListBox1.Items.Count <> 0 Then Begin
      ListBox1.ItemIndex := 0;
      ListBox1.Click;
    End;
  End;
  // TODO: Suchen aller .chat Dateien und Löschen derer die nicht mehr in der Listbox sind !
End;

Procedure TForm1.HandleGetMessage(Const Chunk: TChunk);
Var
  aSender, aMsg: String;
  i, j: Integer;
Begin
  aSender := chunk.data.ReadAnsiString();
  aMsg := chunk.data.ReadAnsiString();
  aMsg := StringReplace(aMsg, '<font color="TimeColor">', '<font color="' + ColorAsHTMLColor(IniPropStorage1.ReadInteger('TimeColor', $808080)) + '">', []);
  AppendLog(aSender, pLeft, aMsg);
  // Die Glocke anzeigen, wenn der Partner nicht eh schon angezeigt wird..
  For i := 0 To high(fParticipants) Do Begin
    If lowercase(fParticipants[i].Name) = LowerCase(aSender) Then Begin
      For j := 0 To ListBox1.Items.Count - 1 Do Begin
        If lowercase(ListBox1.Items[j]) = LowerCase(aSender) Then Begin
          If j <> ListBox1.ItemIndex Then Begin
            fParticipants[i].nm := true;
            ListBox1.Invalidate;
          End;
        End;
      End;
      break;
    End;
  End;
  //  ListBox1.Click;
  PlayInfoSound();
  If (MenuItem6.Checked) Then Begin
    If TrayIcon1.Visible Then Begin
      MenuItem6Click(Nil); // Update the Tray Icon ;)
    End;
  End
  Else Begin
    If IniPropStorage1.ReadBoolean('Show_on_new_message', false) Then Begin
      If Not visible Then Begin
        // Wir sollen Hoch popen, dann wählen wir auch gleich den Richtigen an ..
        For i := 0 To ListBox1.Items.Count - 1 Do Begin
          If lowercase(ListBox1.Items[i]) = LowerCase(aSender) Then Begin
            ListBox1.ItemIndex := i;
            break;
          End;
        End;
        ListBox1.Click;
        ListBox1.Invalidate;
        TrayIcon1.OnClick(Nil);
      End;
    End;
  End;
End;

Procedure TForm1.HandleFileTransferRequest(Const Chunk: TChunk);
Var
  aReceiver, aSender, aFilename: String;
  data: TMemoryStream;
  aFileSize: int64;
  aReason: uint16;
Begin
  aReceiver := Chunk.Data.ReadAnsiString;
  aSender := Chunk.Data.ReadAnsiString;
  aFilename := Chunk.Data.ReadAnsiString;
  aFileSize := 0;
  Chunk.Data.Read(aFileSize, sizeof(aFileSize));
  assert(aReceiver = IniPropStorage1.ReadString('UserName', ''), 'TForm1.HandleFileTransferRequest: Error, wrong client');
  aReason := $FFFF;
  If FileSendData.State <> 0 Then Begin
    // Wir sind Beschäftigt -> Ablehnen
  End;
  // Akustisch den Empfänger "erinnern"
  PlayInfoSound();
  FileSendData.State := 2; // Wir sind ab jetzt auf empfangen
  If ID_YES = Application.MessageBox(pchar(format('%s want to send you the file %s of size %s do you want to accept this request?', [aSender, aFilename, FileSizeToString(aFileSize)])), 'File transfer request', MB_ICONQUESTION Or MB_YESNO) Then Begin
    SaveDialog1.FileName := aFilename;
    If FileSendData.State <> 2 Then exit; // Der Sender hat inzwischen abgebrochen
    If SaveDialog1.Execute Then Begin
      If FileSendData.State <> 2 Then exit; // Der Sender hat inzwischen abgebrochen
      FileSendData.ChunkCounter := 0;
      FileSendData.Filename := SaveDialog1.FileName;
      FileSendData.FileStream := TFileStream.Create(FileSendData.Filename, fmCreate Or fmOpenWrite);
      FileSendData.aSize := aFileSize;
      FileSendData.FileReceiver := aReceiver;
      FileSendData.FileSender := aSender;
      aReason := File_Transfer_Accepted;
      AppendLog(aSender, pRight, 'Accept: ' + ExtractFileName(FileSendData.Filename));
    End
    Else Begin
      aReason := File_Transfer_No_Receiver_does_not_want;
    End;
  End
  Else Begin
    aReason := File_Transfer_No_Receiver_does_not_want;
  End;
  If FileSendData.State <> 2 Then exit; // Der Sender hat inzwischen abgebrochen
  data := TMemoryStream.Create;
  data.WriteAnsiString(aSender);
  data.WriteAnsiString(aReceiver);
  data.Write(aReason, sizeof(aReason));
  fconnection.SendChunk(MSG_File_Transfer_Request_result, data);
End;

Procedure TForm1.HandleFileTransferRequestResult(Const Chunk: TChunk);
Var
  aReason: UInt16;
Begin
  aReason := $FFFF;
  // TODO: ggf mehr Prechecks
  Chunk.Data.ReadAnsiString; // Überlesen Empfänger
  Chunk.Data.ReadAnsiString; // Überlesen Sender
  Chunk.Data.Read(aReason, sizeof(aReason));
  Case aReason Of
    File_Transfer_Accepted: Begin
        // Alles ist geklärt, wir starten das Senden der Datei
        FileSendData.FileStream := TFileStream.Create(FileSendData.Filename, fmOpenRead);
        FileSendData.State := 3;
        FileSendData.aPosition := 0;
        // Anstoßen des 1. Chunks
        SendFileChunk();
      End;
    File_Transfer_No_due_to_Receiver_not_Online,
      File_Transfer_No_due_to_Receiver_occupied,
      File_Transfer_No_Receiver_does_not_want: Begin
        FileSendData.State := 0; // Back to Idle
        If assigned(FileSendData.FileStream) Then Begin
          FileSendData.FileStream.free;
        End;
        FileSendData.FileStream := Nil;
        If form3.Visible Then form3.Hide;
        AppendLog(FileSendData.FileReceiver, pRight, 'Error, file transfer aborted: ' + ErrorcodeToString(aReason));
      End;
  Else Begin
      FileSendData.State := 0; // Back to Idle
      If assigned(FileSendData.FileStream) Then Begin
        FileSendData.FileStream.free;
      End;
      FileSendData.FileStream := Nil;
      If form3.Visible Then form3.Hide;
      AppendLog(FileSendData.FileReceiver, pRight, 'Error, file transfer aborted: ' + ErrorcodeToString(aReason));
    End;
  End;
End;

Procedure TForm1.HandleReceiveFileTransferContent(Const Chunk: TChunk);
Var
  aSender, aReceiver: String;
  Buffer: TMemoryStream;
Begin
  // Noch Mehr Prechecks ?
  If FileSendData.State = 2 Then Begin
    aReceiver := Chunk.Data.ReadAnsiString;
    aSender := Chunk.Data.ReadAnsiString;
    buffer := TMemoryStream.Create;
    buffer.CopyFrom(chunk.Data, Chunk.Data.Size - Chunk.Data.Position);
    inc(FileSendData.ChunkCounter, 1);
    buffer.Position := 0;
    FileSendData.FileStream.CopyFrom(buffer, buffer.Size);
    FileSendData.aPosition := FileSendData.aPosition + Buffer.Size;
    buffer.free;
    If FileSendData.ChunkCounter = PacketSize Then Begin
      FileSendData.ChunkCounter := 0;
      buffer := TMemoryStream.Create;
      buffer.WriteAnsiString(aSender);
      buffer.WriteAnsiString(aReceiver);
      fconnection.SendChunk(MSG_File_Transfer_File_Next_Packet, buffer);
    End;
    If Not Form3.Visible Then Begin
      form3.Label1.Caption := FileSendData.Filename;
      form3.ProgressBar1.Position := 0;
      form3.Show;
    End;
    form3.ProgressBar1.Position := round(FileSendData.aPosition * 100 / FileSendData.aSize);
    form3.Label2.Caption := format('%d KB of %d KB', [FileSendData.aPosition Div 1024, FileSendData.aSize Div 1024]);
  End;
End;

Procedure TForm1.HandleTransferNextPacket(Const Chunk: TChunk);
Begin
  // TODO: Mehr Prechecks
  SendFileChunk();
End;

Procedure TForm1.HandleTransferFileComplete(Const Chunk: TChunk);
Var
  aSender: String;
Begin
  If FileSendData.State <> 0 Then Begin // = 2 ?
    Chunk.Data.ReadAnsiString; // Den Brauchen wir nicht ..
    aSender := Chunk.Data.ReadAnsiString;
    // TODO: Mehr Checks
    FileSendData.State := 0;
    FileSendData.FileStream.Free;
    FileSendData.FileStream := Nil;
    FileSendData.aPosition := 0;
    FileSendData.aSize := 0;
    AppendLog(aSender, pRight, 'transfer finished.');
    form3.Hide; // Fertig -> Anzeigen
  End;
End;

Procedure TForm1.HandleAbortFile(Const Chunk: TChunk);
Var
  aReason: uint16;
Begin
  If FileSendData.State <> 0 Then Begin
    Chunk.Data.ReadAnsiString;
    Chunk.Data.ReadAnsiString;
    aReason := $FFFF;
    Chunk.Data.Read(aReason, sizeof(aReason));
    If IniPropStorage1.ReadString('UserName', '') = FileSendData.FileSender Then Begin
      AppendLog(FileSendData.FileReceiver, pRight, 'transfer aborted.');
    End
    Else Begin
      AppendLog(FileSendData.FileSender, pRight, 'transfer aborted.');
    End;
    FileSendData.State := 0;
    If assigned(FileSendData.FileStream) Then Begin
      FileSendData.FileStream.free;
    End;
    FileSendData.FileStream := Nil;
  End;
  If form3.Visible Then form3.Hide;
End;

Procedure TForm1.HandleLoginToServerSettingsResult(Const Chunk: TChunk);
Var
  aResult: uint16;
  p: Array Of String;
  i: Integer;
Begin
  aResult := $FFFF;
  Chunk.Data.Read(aResult, sizeof(aResult));
  If aResult = Error_No_Error Then Begin
    p := Nil;
    setlength(p, ListBox1.Items.Count);
    For i := 0 To high(p) Do Begin
      p[i] := ListBox1.Items[i];
    End;
    form4.Init(fconnection, p);
    form4.Show;
  End
  Else Begin
    ShowMessage(ErrorcodeToString(aResult));
  End;
End;

Procedure TForm1.HandlePasswordChangeResult(Const Chunk: TChunk);
Var
  aResult: uint16;
Begin
  aResult := $FFFF;
  Chunk.Data.Read(aResult, sizeof(aResult));
  If aResult = Error_No_Error Then Begin
    form4.Edit1.Text := '';
    form4.Edit2.Text := '';
    form4.Edit3.Text := '';
    showmessage('Done.');
  End
  Else Begin
    ShowMessage(ErrorcodeToString(aResult));
  End;
End;

Procedure TForm1.HandleRemoveKnownParticipantResult(Const Chunk: TChunk);
Var
  aResult: uint16;
Begin
  aResult := $FFFF;
  Chunk.Data.Read(aResult, sizeof(aResult));
  If aResult = Error_No_Error Then Begin
    form4.ListBox1.Items.Delete(form4.ListBox1.ItemIndex);
    showmessage('Done.');
  End
  Else Begin
    ShowMessage(ErrorcodeToString(aResult));
  End;
End;

Procedure TForm1.OpenOptions;
Var
  oldtColor, oldoColor, oldbColor, oldColor: TColor;
  sl: TStringList;
  i: Integer;
  s: String;
Begin
  form2.Edit3.Text := IniPropStorage1.ReadString('UserName', '');
  form2.Edit4.Text := IniPropStorage1.ReadString('Password', '');
  form2.Shape1.Brush.Color := IniPropStorage1.ReadInteger('UserColor', clBlack);
  form2.Shape3.Brush.Color := IniPropStorage1.ReadInteger('OtherColor', clblack);
  form2.Shape4.Brush.Color := IniPropStorage1.ReadInteger('TimeColor', $808080);
  form2.Shape2.Brush.Color := IniPropStorage1.ReadInteger('BackColor', clwhite);
  form2.Edit1.Text := IniPropStorage1.ReadString('ServerIP', '');
  form2.Edit2.Text := inttostr(IniPropStorage1.ReadInteger('Port', 1234));
  form2.CheckBox1.Checked := IniPropStorage1.ReadBoolean('Start_hidden', false);
  form2.CheckBox2.Checked := IniPropStorage1.ReadBoolean('Play_Sound', true);
  form2.CheckBox3.Checked := IniPropStorage1.ReadBoolean('Show_on_new_message', false);
  form2.ScrollBar1.Position := IniPropStorage1.ReadInteger('Volume', 10000);
  oldColor := form2.Shape1.Brush.Color;
  oldbColor := form2.Shape2.Brush.Color;
  oldoColor := form2.Shape3.Brush.Color;
  oldtColor := form2.Shape4.Brush.Color;
  If form2.ShowModal = mrOK Then Begin
    IniPropStorage1.WriteString('UserName', form2.Edit3.Text);
    IniPropStorage1.WriteString('ServerIP', form2.Edit1.Text);
    IniPropStorage1.WriteInteger('UserColor', form2.Shape1.Brush.Color);
    IniPropStorage1.WriteInteger('OtherColor', form2.Shape3.Brush.Color);
    IniPropStorage1.WriteInteger('TimeColor', form2.Shape4.Brush.Color);
    IniPropStorage1.WriteInteger('BackColor', form2.Shape2.Brush.Color);
    IniPropStorage1.WriteInteger('Port', strtointdef(form2.Edit2.Text, 1234));
    IniPropStorage1.WriteString('Password', form2.Edit4.Text);
    IniPropStorage1.WriteBoolean('Start_hidden', form2.CheckBox1.Checked);
    IniPropStorage1.WriteBoolean('Play_Sound', form2.CheckBox2.Checked);
    IniPropStorage1.WriteBoolean('Show_on_new_message', form2.CheckBox3.Checked);
    IniPropStorage1.WriteInteger('Volume', form2.ScrollBar1.Position);

    If oldtColor <> form2.Shape4.Brush.Color Then Begin
      // Suchen aller .chat Dateien im Verzeichnus und Anpassen der eigenen
      sl := FindAllFiles(extractfilepath(ParamStr(0)), '*.chat', false);
      For i := 0 To sl.Count - 1 Do Begin
        s := ExtractFileNameWithoutExt(ExtractFileNameOnly(sl[i]));
        SetColorForChat(s, form2.Shape4.Brush.Color, cdTimeStamp);
      End;
      sl.free;
    End;
    If oldColor <> form2.Shape1.Brush.Color Then Begin
      // Suchen aller .chat Dateien im Verzeichnus und Anpassen der eigenen
      sl := FindAllFiles(extractfilepath(ParamStr(0)), '*.chat', false);
      For i := 0 To sl.Count - 1 Do Begin
        s := ExtractFileNameWithoutExt(ExtractFileNameOnly(sl[i]));
        SetColorForChat(s, form2.Shape1.Brush.Color, cdRight);
      End;
      sl.free;
    End;
    If oldoColor <> form2.Shape3.Brush.Color Then Begin
      // Suchen aller .chat Dateien im Verzeichnus und Anpassen der eigenen
      sl := FindAllFiles(extractfilepath(ParamStr(0)), '*.chat', false);
      For i := 0 To sl.Count - 1 Do Begin
        s := ExtractFileNameWithoutExt(ExtractFileNameOnly(sl[i]));
        SetColorForChat(s, form2.Shape3.Brush.Color, cdLeft);
      End;
      sl.free;
    End;
    If oldbColor <> form2.Shape2.Brush.Color Then Begin
      ListBox1.Color := IniPropStorage1.ReadInteger('BackColor', clwhite);
      ListBox1.Invalidate;
      sl := FindAllFiles(extractfilepath(ParamStr(0)), '*.chat', false);
      For i := 0 To sl.Count - 1 Do Begin
        s := ExtractFileNameWithoutExt(ExtractFileNameOnly(sl[i]));
        SetColorForChat(s, form2.Shape2.Brush.Color, cdBackground);
      End;
      sl.free;
    End;
    ConnectToServer();
  End
  Else Begin
    If Not fconnection.Connected Then Begin
      showmessage('Error, you do need to edit settings to be able to '
        + 'connect to a server.');
    End;
    exit;
  End;
  // Egal wie, nu ist UDP-Technisch schluss..
  If LUDPComponent1.Connected Then LUDPComponent1.Disconnect(true);
  form2.timer1.enabled := false;
End;

Procedure TForm1.AppendLog(aParticipant: String; aPos: TPosition;
  aMessage: String);
Var
  sl: TStringList;
  atxt: String;
  i: Integer;
Begin
  Case aPos Of
    pLeft: atxt := '<p class="left-aligned"> ' + aMessage + '</p>';
    pRight: atxt := '<p class="right-aligned">' + aMessage + '</p>';
  End;
  sl := TStringList.Create;
  If FileExists(lowercase(aParticipant) + '.chat') Then Begin
    sl.LoadFromFile(lowercase(aParticipant) + '.chat');
  End
  Else Begin
    sl.text := '<!DOCTYPE html>' + LineEnding +
      '<html lang="en">' + LineEnding +
      '<head>' + LineEnding +
      '    <meta charset="UTF-8">' + LineEnding +
      '    <meta name="viewport" content="width=device-width, initial-scale=1.0">' + LineEnding +
      '    <style type="text/css">' + LineEnding +
      '        .left-aligned {' + LineEnding +
      '            text-align: left;' + LineEnding +
      '            color: ' + ColorAsHTMLColor(IniPropStorage1.ReadInteger('OtherColor', clblack)) + ';' + LineEnding +
      '        }' + LineEnding +
      '        .right-aligned {' + LineEnding +
      '            text-align: right;' + LineEnding +
      '            color: ' + ColorAsHTMLColor(IniPropStorage1.ReadInteger('UserColor', clblack)) + ';' + LineEnding +
      '        }' + LineEnding +
      '    </style>' + LineEnding +
      '</head>' + LineEnding +
      '<body style="background-color: ' + ColorAsHTMLColor(IniPropStorage1.ReadInteger('BackColor', clwhite)) + ';">' + LineEnding +
      '</body>' + LineEnding +
      '</html>';
  End;
  For i := sl.Count - 1 Downto 0 Do Begin
    If pos('</body>', sl[i]) <> 0 Then Begin
      sl.Insert(i, atxt);
      break;
    End;
  End;
  sl.SaveToFile(lowercase(aParticipant) + '.chat');
  sl.free;
  // Wir haben einen Chat Aktualisiert der gerade angewählt war, also Refresh
  If lowercase(ListBox1.items[ListBox1.ItemIndex]) = lowercase(aParticipant) Then Begin
    ListBox1.Click;
  End;
End;

Procedure TForm1.SendFile(Const Filename: String);
Var
  aSize: int64;
  data: TMemoryStream;
Begin
  If Not fconnection.Connected Then Begin
    ShowMessage('Error, not connected.');
    exit;
  End;
  If ListBox1.ItemIndex = -1 Then Begin
    ShowMessage('Error, no one to send data.');
    exit;
  End;
  If FileSendData.State <> 0 Then Begin
    ShowMessage('Error, you are not allowed to send more than one file at once.');
    exit;
  End;
  aSize := GetFileSize(Filename);
  If aSize <> 0 Then Begin
    FileSendData.State := 1;
    FileSendData.Filename := FileName;
    FileSendData.FileReceiver := ListBox1.Items[ListBox1.ItemIndex];
    FileSendData.FileSender := IniPropStorage1.ReadString('UserName', '');
    FileSendData.aSize := aSize;
    data := TMemoryStream.Create;
    data.WriteAnsiString(ListBox1.Items[ListBox1.ItemIndex]);
    data.WriteAnsiString(IniPropStorage1.ReadString('UserName', ''));
    data.WriteAnsiString(ExtractFileName(FileSendData.Filename));
    data.Write(aSize, sizeof(aSize));
    fconnection.SendChunk(MSG_File_Transfer_Request, data);
    AppendLog(ListBox1.Items[ListBox1.ItemIndex], pRight, 'Initiated file '
      + 'transfer: ' + ExtractFileName(FileSendData.Filename));
    // Das muss jetzt schon angezeigt werden, sonst kommt der Sender nur durch neustart in ein Resend
    If Not form3.Visible Then Begin
      form3.Label1.Caption := FileSendData.Filename;
      form3.ProgressBar1.Position := 0;
      form3.Show;
    End;
  End
  Else Begin
    showmessage('Error, its not allowed to send file of size = 0');
  End;
End;

Procedure TForm1.SetColorForChat(aChatName: String; aColor: TColor;
  aDest: TColorDest);
Var
  pre, suff, fn, sm: String;
  sl: TStringList;
  i, j: Integer;
  aimed: Boolean;
Begin
  fn := lowercase(aChatName) + '.chat';
  If Not FileExists(fn) Then exit;
  sl := TStringList.Create;
  sl.LoadFromFile(fn);
  Case aDest Of
    cdLeft, cdRight: Begin // Suche in den Styles
        (*
          Sucht das Color Attribut in der folgenden Struktur und ersetzt es passend
          .left-aligned {
            text-align: left;
            color: #000000;
          }
        *)
        Case aDest Of
          cdLeft: sm := '.left-aligned';
          cdRight: sm := '.right-aligned';
        End;
        aimed := false;
        For i := 0 To sl.Count - 1 Do Begin
          If pos(sm, sl[i]) <> 0 Then Begin
            aimed := true;
          End;
          If aimed Then Begin
            If pos('color:', sl[i]) <> 0 Then Begin
              sl[i] := '            color: ' + ColorAsHTMLColor(aColor) + ';';
              break;
            End;
            If pos('}', sl[i]) <> 0 Then Begin // Offensichtlich ein Fehler
              break;
            End;
          End;
        End;
      End;
    cdTimeStamp: Begin
        For i := 0 To sl.Count - 1 Do Begin
          j := pos('<font color="', sl[i]);
          If j <> 0 Then Begin
            pre := copy(sl[i], 1, j + length('<font color="') - 1);
            suff := copy(sl[i], j + length('<font color="') + 6 + 1, length(sl[i]));
            sl[i] := pre + ColorAsHTMLColor(aColor) + suff;
          End;
        End;
      End;
    cdBackground: Begin
        sm := '<body style="background-color:';
        For i := 0 To sl.Count - 1 Do Begin
          If pos(sm, sl[i]) <> 0 Then Begin
            sl[i] := '<body style="background-color: ' + ColorAsHTMLColor(aColor) + ';">';
            break;
          End;
        End;
      End;
  End;
  sl.SaveToFile(fn);
  sl.free;
End;

Procedure TForm1.SetNotConnected;
Var
  sl: TStringList;
Begin
  caption := defcaption + ', not connected.';
  sl := TStringList.Create;
  sl.text := '<!DOCTYPE html>' + LineEnding +
    '<html>' + LineEnding +
    '<head>' + LineEnding +
    '</head>' + LineEnding +
    '<body style="background-color: ' + ColorAsHTMLColor(IniPropStorage1.ReadInteger('BackColor', clwhite)) + ';">' + LineEnding +
    'not connected..' + LineEnding +
    '</body>' + LineEnding +
    '</html>';
  LoadTextToHTMLView(sl.text);
  sl.free;
  ListBox1.Clear;
  setlength(fParticipants, 0);
  FileSendData.State := 0;
  If assigned(FileSendData.FileStream) Then FileSendData.FileStream.Free;
  FileSendData.FileStream := Nil;
  timer1.enabled := true;
End;

Procedure TForm1.SendFileChunk;
Var
  cnt, i: Integer;
  data: TMemoryStream;
  aSize: Int64;
Begin
  If FileSendData.State <> 3 Then exit; // Wenn wir nicht senden, dann sind das noch irgendwelche Teardown altlasten -> Ignorieren
  If FileSendData.aPosition < FileSendData.aSize Then Begin
    // 1 Packet versenden
    For i := 0 To PacketSize - 1 Do Begin
      data := TMemoryStream.Create;
      data.WriteAnsiString(FileSendData.FileReceiver);
      data.WriteAnsiString(IniPropStorage1.ReadString('UserName', ''));
      cnt := min(FileSendData.aSize - FileSendData.aPosition, ChunkSize);
      aSize := data.CopyFrom(FileSendData.FileStream, cnt);
      inc(FileSendData.aPosition, aSize);
      fconnection.SendChunk(MSG_File_Transfer_FileContent, data);
      If aSize <> ChunkSize Then Begin
        // Datei Ende Erreicht, Ende Gelände -> Fertig machen
        data := TMemoryStream.Create;
        data.WriteAnsiString(FileSendData.FileReceiver);
        data.WriteAnsiString(IniPropStorage1.ReadString('UserName', ''));
        fconnection.SendChunk(MSG_File_Transfer_FileComplete, data);
        FileSendData.State := 0;
        FileSendData.FileStream.free;
        FileSendData.FileStream := Nil;
        AppendLog(FileSendData.FileReceiver, pRight, 'transfer finished.');
        form3.Hide; // Fertig -> Anzeigen
        break;
      End;
      form3.ProgressBar1.Position := round(FileSendData.aPosition * 100 / FileSendData.aSize);
      fconnection.CallAction();
      Application.ProcessMessages;
    End;
  End;
End;

Procedure TForm1.AbortFileTransfer(Sender: TObject);
Var
  data: TMemoryStream;
  aReason: uint16;
Begin
  If FileSendData.State <> 0 Then Begin
    data := TMemoryStream.Create;
    If IniPropStorage1.ReadString('UserName', '') = FileSendData.FileSender Then Begin
      // Sender Bricht Ab
      data.WriteAnsiString(FileSendData.FileReceiver);
      data.WriteAnsiString(FileSendData.FileSender);
      aReason := File_Transfer_Abort_by_Sender;
      AppendLog(FileSendData.FileReceiver, pRight, 'transfer aborted.');
    End
    Else Begin
      // Empfänger Bricht ab
      data.WriteAnsiString(FileSendData.FileSender);
      data.WriteAnsiString(FileSendData.FileReceiver);
      aReason := File_Transfer_Abort_by_Receiver;
      AppendLog(FileSendData.FileSender, pRight, 'transfer aborted.');
    End;
    data.Write(aReason, sizeof(aReason));
    fconnection.SendChunk(MSG_File_Transfer_File_Abort, data);
    // Sauberer Teardown
    FileSendData.State := 0;
    If assigned(FileSendData.FileStream) Then Begin // Bei State = 1 kann der Stream noch NIL sein !
      FileSendData.FileStream.free;
    End;
    FileSendData.FileStream := Nil;
    If form3.Visible Then form3.Hide;
  End;
End;

End.

