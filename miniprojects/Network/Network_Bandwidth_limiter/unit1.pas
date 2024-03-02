(******************************************************************************)
(* Network bandwidth limiter                                       ??.??.???? *)
(*                                                                            *)
(* Version     : 0.02                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Can simulate a bandwith limited connection.                  *)
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
(*             - Programm works only with Single Connections, if more than one*)
(*               connection is made to "PC1", only the first connected gets   *)
(*               response messages from "PC2" => Multiple connections are     *)
(*               forced to close see : LTCPComponent1Accept                   *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*               0.02 - Support skript Modus (nur für starten)                *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, lNetComponents, Forms, Controls, Graphics,
  Dialogs, StdCtrls, lNet, ufifo;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    ApplicationProperties1: TApplicationProperties;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    LTCPComponent1: TLTCPComponent;
    LTCPComponent2: TLTCPComponent;
    Procedure ApplicationProperties1Idle(Sender: TObject; Var Done: Boolean);
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
    Procedure LTCPComponent1Accept(aSocket: TLSocket);
    Procedure LTCPComponent1CanSend(aSocket: TLSocket);
    Procedure LTCPComponent1Disconnect(aSocket: TLSocket);
    Procedure LTCPComponent1Error(Const msg: String; aSocket: TLSocket);
    Procedure LTCPComponent1Receive(aSocket: TLSocket);
    Procedure LTCPComponent2CanSend(aSocket: TLSocket);
    Procedure LTCPComponent2Connect(aSocket: TLSocket);
    Procedure LTCPComponent2Error(Const msg: String; aSocket: TLSocket);
    Procedure LTCPComponent2Receive(aSocket: TLSocket);
  private
    { private declarations }
  public
    { public declarations }
  End;

  TDataSet = Record // Daten "Pro Datenpacket" welches hin und Her geht
    Data: Array Of Byte;
    Position: integer;
  End;

  PDataSet = ^TDataSet;

  TPDataSetQueue = specialize TBufferedFifo < PDataSet > ;

  TPC_State = Record
    clients: integer;
  End;

Var
  Form1: TForm1;
  pc2_to_pc1, pc1_to_pc2: TPDataSetQueue;
  BandWidth, pc2_to_pc1_S, pc1_to_pc2_S: int64;
  PC1, PC2: TPC_State;
  LastCheck: int64; // Das Letze mal an dem geprüft wurde, dass alle Bandbreiten eingehalten werden.

Implementation

{$R *.lfm}

Uses math, lazutf8;

Const
  junksize = 1024;

Var
  Form1ShowOnce: Boolean = True;

  { TForm1 }

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.ApplicationProperties1Idle(Sender: TObject; Var Done: Boolean);
Var
  p: PDataSet;
  s: TLSocket;
  d, n: int64;
Begin
  done := false;
  If Not pc1_to_pc2.isempty Then Begin
    If LTCPComponent2.Connected Then Begin
      p := pc1_to_pc2.Top;
      s := LTCPComponent2.Iterator;
      s.UserData := p;
      LTCPComponent2.OnCanSend(s);
    End
    Else Begin
      // Wegwerfen der Daten
      p := pc1_to_pc2.Pop;
      setlength(p^.Data, 0);
      dispose(p);
    End;
  End;

  If Not pc2_to_pc1.isempty Then Begin
    If LTCPComponent1.Connected Then Begin
      p := pc2_to_pc1.Top;
      s := LTCPComponent1.Iterator;
      s.UserData := p;
      LTCPComponent1.OnCanSend(s);
    End
    Else Begin
      // Wegwerfen der Daten
      p := pc2_to_pc1.Pop;
      setlength(p^.Data, 0);
      dispose(p);
    End;
  End;
  // Wieder Freigeben von Bandbreite
  n := GetTickCount64;
  d := n - LastCheck;
  If d > 0 Then Begin
    d := round((d * BandWidth) / 1000); // Berechnen wie viele Bytes wieder "Versendet" werden dürfen.
    pc2_to_pc1_S := max(0, pc2_to_pc1_S - d); // Zählt wie viel der Bandbreite verbraucht wurde
    pc1_to_pc2_S := max(0, pc1_to_pc2_S - d); // Zählt wie viel der Bandbreite verbraucht wurde
    LastCheck := n;
  End;
  sleep(1);
End;

Procedure TForm1.Button1Click(Sender: TObject);
Begin
  // Apply Bandwidth
  BandWidth := strtointdef(edit2.text, 0) * 1024;
  pc2_to_pc1_S := 0; // Zählt wie viel der Bandbreite verbraucht wurde
  pc1_to_pc2_S := 0; // Zählt wie viel der Bandbreite verbraucht wurde
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  // Start Listen
  If button3.caption = 'Start listen' Then Begin
    If LTCPComponent1.Listen(strtoint(Edit1.Text)) Then Begin
      button3.caption := 'Stop listen';
    End;
  End
  Else Begin
    LTCPComponent1.Disconnect(true);
    While LTCPComponent1.Connected Do
      Application.ProcessMessages;
    button3.caption := 'Start listen';
  End;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Begin
  If Button4.Caption = 'Connect' Then Begin
    If LTCPComponent2.Connected Then LTCPComponent2.Disconnect(true);
    If Not LTCPComponent2.Connect(Edit4.Text, strtoint(edit3.text)) Then Begin
      showmessage('Error unable to connect.');
    End;
  End
  Else Begin // Disconnect
    LTCPComponent2.Disconnect(true);
    While LTCPComponent2.Connected Do
      Application.ProcessMessages;
    Button4.Caption := 'Connect';
  End;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Var
  p: PDataSet;
Begin
  LTCPComponent2.Disconnect(true);
  LTCPComponent1.Disconnect(true);
  While (LTCPComponent2.Connected Or LTCPComponent1.Connected) Do
    Application.ProcessMessages;
  While Not pc1_to_pc2.isempty Do Begin
    p := pc1_to_pc2.Pop;
    setlength(p^.Data, 0);
    dispose(p);
  End;
  While Not pc2_to_pc1.isempty Do Begin
    p := pc2_to_pc1.Pop;
    setlength(p^.Data, 0);
    dispose(p);
  End;
  pc2_to_pc1.free;
  pc1_to_pc2.free;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Begin
  showmessage(
    'Online help Network bandwidth limiter' + LineEnding +
    'Usage in skript mode : ' + LineEnding + LineEnding +
    '-l xx : Set listenport to xx and autostart listening' + LineEnding +
    '-b xx : Set bandwith limit to xx' + LineEnding +
    '-c xx:yy : Connect to IP xx on port yy and autosart'
    );
End;

Procedure TForm1.FormShow(Sender: TObject);
Var
  i: integer;
  c, bw, sl: Boolean;
  s: String;

Begin
  If Form1ShowOnce Then Begin
    Form1ShowOnce := false;
    sl := false;
    bw := false;
    c := false;
    For i := 1 To Paramcount - 1 Do Begin
      If ParamStrUTF8(i) = '-l' Then Begin
        edit1.text := ParamStrUTF8(i + 1);
        sl := true;
      End;
      If ParamStrUTF8(i) = '-b' Then Begin
        edit2.text := ParamStrUTF8(i + 1);
        bw := true;
      End;
      If ParamStrUTF8(i) = '-c' Then Begin
        s := ParamStrUTF8(i + 1);
        edit4.text := copy(s, 1, pos(':', s) - 1);
        edit3.text := copy(s, pos(':', s) + 1, length(s));
        c := true;
      End;
    End;
    If bw Then Begin
      Button1.Click;
    End;
    If sl Then Begin
      Button3.Click;
    End;
    If c Then Begin
      Button4.Click;
    End;
  End;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  Tform(self).Constraints.MaxHeight := Tform(self).Height;
  Tform(self).Constraints.MinHeight := Tform(self).Height;
  Tform(self).Constraints.Maxwidth := Tform(self).width;
  Tform(self).Constraints.Minwidth := Tform(self).width;
  label8.caption := '0';
  caption := 'Network bandwidth limiter ver. 0.02';

  // PC1
  Edit1.text := '1235';
  Edit2.text := '50'; // 50 kbs / s

  // PC2
  Edit4.text := '127.0.0.1';
  Edit3.text := '1234';

  pc2_to_pc1 := TPDataSetQueue.create;
  pc1_to_pc2 := TPDataSetQueue.create;
  //pc1.connected := false;
  pc1.clients := 0;
  //pc2.connected := false;
  pc2.clients := 0;
  Button1Click(Nil);
  LastCheck := GetTickCount64;
End;

Procedure TForm1.LTCPComponent1Accept(aSocket: TLSocket);
Begin
  // Mehr wie eine Eingehende Verindung unterbinden !
  If pc1.clients > 0 Then Begin
    aSocket.Disconnect(true);
    exit;
  End;
  inc(pc1.clients);
  label8.caption := 'Clients: ' + inttostr(pc1.clients);
End;

Procedure TForm1.LTCPComponent1CanSend(aSocket: TLSocket);
Var
  data: Array[0..junksize - 1] Of byte;
  p: PDataSet;
  j: Integer;
  i: Integer;
  asent, sent: Integer;
Begin
  p := aSocket.UserData;
  If assigned(p) Then Begin
    // Dafür Sorgen, dass p^Socket immer auf einen Gültigen Socket zeigt, an welchen wir auch etwas senden dürfen.
    Repeat
      j := 0;
      For i := p^.Position To high(p^.Data) Do Begin
        data[j] := p^.Data[i];
        inc(j);
        If j >= junksize Then break;
      End;
      If BandWidth <> 0 Then Begin // Wenn die Bandbreitenlimitierung an ist.
        asent := BandWidth - pc2_to_pc1_S; // Wieviel darf gesendet werden ?
        If asent > 0 Then Begin
          j := min(asent, j);
        End
        Else Begin
          j := 0; // Die Bandbreitenlimitierung hat zugeschlagen
        End;
      End;
      If j > 0 Then Begin
        sent := LTCPComponent1.send(data, j, aSocket);
      End
      Else Begin
        sent := 0;
      End;
      p^.Position := p^.Position + sent; // Weiter Schieben des Datenpointers
      pc2_to_pc1_S := pc2_to_pc1_S + sent; // Aufzählen wie viel schon gesendet wurde
      If (p^.Position >= length(p^.Data)) Then Begin // Dieser Datensatz hat nichts mehr zum Versenden
        sent := 0;
        SetLength(p^.Data, 0);
        Dispose(p);
        pc2_to_pc1.pop; // Löschen aus der Queue, der ist ja abgearbeitet
      End;
    Until sent = 0;
  End;
End;

Procedure TForm1.LTCPComponent1Disconnect(aSocket: TLSocket);
Begin
  dec(PC1.clients);
  If pc1.clients < 0 Then pc1.clients := 0;
  label8.caption := 'Clients: ' + inttostr(pc1.clients);
End;

Procedure TForm1.LTCPComponent1Error(Const msg: String; aSocket: TLSocket);
Begin
  showmessage('PC1: ' + msg);
End;

Procedure TForm1.LTCPComponent1Receive(aSocket: TLSocket);
Var
  buf: Array[0..junksize - 1] Of byte;
  p: PDataSet;
  k, i: integer;
  j: Integer;
Begin
  // Alles Lesen was es zu lesen gibt und mit AbsendeZeitstempel versehen
  new(p);
  p^.Data := Nil;
  p^.Position := 0;
  i := asocket.Get(buf, junksize);
  While i <> 0 Do Begin
    j := high(p^.Data) + 1;
    setlength(p^.Data, j + i);
    For k := 0 To i - 1 Do Begin
      p^.Data[j + k] := buf[k];
    End;
    i := asocket.Get(buf, junksize);
  End;
  // Ab in die Warteschlange zum Versenden.
  If length(p^.Data) = 0 Then Begin
    Dispose(p);
  End
  Else Begin
    pc1_to_pc2.Push(p);
  End;
End;

Procedure TForm1.LTCPComponent2CanSend(aSocket: TLSocket);
Var
  data: Array[0..junksize - 1] Of byte;
  p: PDataSet;
  j: Integer;
  i: Integer;
  asent, sent: Integer;
Begin
  p := aSocket.UserData;
  If assigned(p) Then Begin
    // Dafür Sorgen, dass p^Socket immer auf einen Gültigen Socket zeigt, an welchen wir auch etwas senden dürfen.
    Repeat
      j := 0;
      For i := p^.Position To high(p^.Data) Do Begin
        data[j] := p^.Data[i];
        inc(j);
        If j >= junksize Then break;
      End;
      If BandWidth <> 0 Then Begin // Wenn die Bandbreitenlimitierung an ist.
        asent := BandWidth - pc1_to_pc2_S; // Wieviel darf gesendet werden ?
        If asent > 0 Then Begin
          j := min(asent, j);
        End
        Else Begin
          j := 0; // Die Bandbreitenlimitierung hat zugeschlagen
        End;
      End;
      If j > 0 Then Begin
        sent := LTCPComponent2.send(data, j, aSocket);
      End
      Else Begin
        sent := 0;
      End;
      p^.Position := p^.Position + sent; // Weiter Schieben des Datenpointers
      pc1_to_pc2_S := pc1_to_pc2_S + sent; // Aufzählen wie viel schon gesendet wurde
      If (p^.Position >= length(p^.Data)) Then Begin // Dieser Datensatz hat nichts mehr zum Versenden
        sent := 0;
        SetLength(p^.Data, 0);
        Dispose(p);
        pc1_to_pc2.pop; // Löschen aus der Queue, der ist ja abgearbeitet
      End;
    Until sent = 0;
  End;
End;

Procedure TForm1.LTCPComponent2Connect(aSocket: TLSocket);
Begin
  Button4.caption := 'Disconnect';
End;

Procedure TForm1.LTCPComponent2Error(Const msg: String; aSocket: TLSocket);
Begin
  showmessage('PC2: ' + msg);
End;

Procedure TForm1.LTCPComponent2Receive(aSocket: TLSocket);
Var
  buf: Array[0..junksize - 1] Of byte;
  p: PDataSet;
  k, i: integer;
  j: Integer;
Begin
  // Alles Lesen was es zu lesen gibt und mit AbsendeZeitstempel versehen
  new(p);
  p^.Data := Nil;
  p^.Position := 0;
  i := asocket.Get(buf, junksize);
  While i <> 0 Do Begin
    j := high(p^.Data) + 1;
    setlength(p^.Data, j + i);
    For k := 0 To i - 1 Do Begin
      p^.Data[j + k] := buf[k];
    End;
    i := asocket.Get(buf, junksize);
  End;
  // Ab in die Warteschlange zum Versenden.
  If length(p^.Data) = 0 Then Begin
    Dispose(p);
  End
  Else Begin
    pc2_to_pc1.Push(p);
  End;
End;

End.

