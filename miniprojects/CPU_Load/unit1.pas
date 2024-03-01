(******************************************************************************)
(* CPU-Load                                                        ??.??.???? *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Application to create a multi threaded CPU-Load              *)
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
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls;

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
    Label4: TLabel;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  End;

  { TLoadThread }

  TLoadThread = Class(TThread)
  private
    fstartTime: int64;
    fDelta: int64;
    a: Array[0..1023] Of integer;
    Procedure Register_Thread;
    Procedure UnRegister_Thread;
  public
    Constructor create(Delta: integer); reintroduce;
    Procedure Execute; override;
  End;

Var
  Form1: TForm1;
  cnt: integer;
  obj: TLoadThread;

Implementation

{$R *.lfm}

{ TLoadThread }

Constructor TLoadThread.create(Delta: integer);
Var
  i: Integer;
Begin
  Inherited create(false);
  FreeOnTerminate := true;
  fstartTime := GetTickCount64;
  fDelta := Delta;
  For i := 0 To high(a) Do
    a[i] := i;
End;

Procedure TLoadThread.Execute;
Var
  i: Integer;
Begin
  Synchronize(@Register_Thread);
  While fstartTime + 1000 * fDelta >= GetTickCount64 Do Begin
    For i := 0 To high(a) Do Begin
      a[(i + i * i) Mod length(a)] := ((a[(i + (i + 2) * i) Mod length(a)] + a[i]) Mod 65536) + 1;
    End;
  End;
  Synchronize(@UnRegister_Thread);
End;

Procedure TLoadThread.Register_Thread;
Begin
  inc(cnt);
  form1.label4.caption := 'Aktive Threads : ' + inttostr(cnt);
End;

Procedure TLoadThread.UnRegister_Thread;
Begin
  dec(cnt);
  form1.label4.caption := 'Aktive Threads : ' + inttostr(cnt);
End;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  caption := 'CPU-Load ver. 0.01 by Corpsman';
  edit1.text := '8';
  edit2.text := '10';
  label4.caption := 'Active threads : 0';
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  i, k: integer;
  j: integer;
Begin
  Button1.Enabled := false;
  i := strtoint(edit1.text);
  j := strtoint(edit2.text);
  For k := 0 To i - 1 Do Begin
    While (cnt >= 100) Do Begin
      sleep(1);
    End;
    obj := TLoadThread.create(j);
  End;
  Button1.Enabled := true;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  canclose := cnt = 0;
  If Not CanClose Then Begin
    showmessage('Error closing only possible, if no impact thread is active.');
  End;
End;

End.

