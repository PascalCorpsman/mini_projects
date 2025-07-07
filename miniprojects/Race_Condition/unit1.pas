(******************************************************************************)
(* Race_Condition_demo                                             ??.??.???? *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : <Module_description>                                         *)
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

// http://www.freepascal.org/docs-html/prog/progsu102.html
{$IFDEF Windows}
{$MAXSTACKSIZE $100000}
{$ENDIF}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls
  , syncobjs // für TCriticalSection
{$IFDEF Linux}
  , LCLIntf
{$ENDIF}
  ;

Const
  UpperNumerBorder = 10000000; // Schön Groß, das es auch ein bischen dauert.

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Memo1: TMemo;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: boolean);
    Procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    Procedure BlockLCL;
    Procedure UnBlockLCL;
  public
    { public declarations }

    Procedure PlottInfo(Title: String; thds, Time, Value: integer);
  End;

  { TPrimeRaceCondition }

  TPrimeRaceCondition = Class(TThread)
  private
    ffrom_: integer;
    fto_: integer;
    Procedure Register_Thread;
    Procedure UnRegister_Thread;
  public
    Constructor create(From_, To_: integer); reintroduce;
    Procedure Execute; override;
  End;

  { TPrimeCritical }

  TPrimeCritical = Class(TTHread)
  private
    ffrom_: integer;
    fto_: integer;
    Procedure Register_Thread;
    Procedure UnRegister_Thread;
  public
    Constructor create(From_, To_: integer); reintroduce;
    Procedure Execute; override;
  End;

  { TPrimeSynchronized }

  TPrimeSynchronized = Class(TTHread)
  private
    ffrom_: integer;
    fto_: integer;
    Procedure IncPrimeCount;
    Procedure Register_Thread;
    Procedure UnRegister_Thread;
  public
    Constructor create(From_, To_: integer); reintroduce;
    Procedure Execute; override;
  End;

  { TPrimeQueued }

  TPrimeQueued = Class(TTHread)
  private
    ffrom_: integer;
    fto_: integer;
    Procedure IncPrimeCount;
    Procedure Register_Thread;
    Procedure UnRegister_Thread;
  public
    Constructor create(From_, To_: integer); reintroduce;
    Procedure Execute; override;
  End;

Var
  Form1: TForm1;
  cnt: integer;
  Unsafe: TPrimeRaceCondition;
  PrimeCritical: TPrimeCritical;
  PrimeSynchronized: TPrimeSynchronized;
  PrimeQueued: TPrimeQueued;
  CS: TCriticalSection;
  PrimeCount: integer;
  StartTime: Int64;

Implementation

{$R *.lfm}

{.$DEFINE V1}
{$DEFINE V2} // ist ca. 2 mal schneller als V1

{$IFDEF V1}

Function IsPrime(value: integer): Boolean;
Var
  up, i: Integer;
Begin
  If value <= 1 Then Begin
    result := false;
  End
  Else Begin
    result := true;
    For i := 2 To trunc(sqrt(value)) Do Begin
      If value Mod i = 0 Then Begin
        result := false;
        exit;
      End;
    End;
  End;
End;
{$ENDIF}

{$IFDEF V2}

Function IsPrime(value: integer): Boolean;
Var
  up, i: Integer;
Begin
  If value <= 1 Then Begin
    result := false;
  End
  Else Begin
    result := true;
    If value Mod 2 = 0 Then Begin
      result := false;
      exit;
    End;
    i := 3;
    up := trunc(sqrt(value));
    While i <= up Do Begin
      If value Mod i = 0 Then Begin
        result := false;
        exit;
      End;
      inc(i, 2);
    End;
  End;
End;
{$ENDIF}

{ TPrimeSynchronized }

Procedure TPrimeSynchronized.Register_Thread;
Begin
  If (cnt) = 0 Then Begin
    StartTime := GetTickCount64;
  End;
  inc(cnt);
  form1.label4.caption := 'Active Threads : ' + inttostr(cnt);
End;

Procedure TPrimeSynchronized.UnRegister_Thread;
Begin
  dec(cnt);
  form1.label4.caption := 'Active Threads : ' + inttostr(cnt);
  If cnt = 0 Then Begin
    form1.UnBlockLCL;
    form1.PlottInfo('Synchronized', strtoint(form1.edit1.text), GetTickCount64() - StartTime, PrimeCount);
  End;
End;

Constructor TPrimeSynchronized.create(From_, To_: integer);
Begin
  Inherited create(false);
  ffrom_ := From_;
  fto_ := To_;
  FreeOnTerminate := true;
End;

Procedure TPrimeSynchronized.Execute;
Var
  i: Integer;
Begin
  Synchronize(@Register_Thread);
  For i := ffrom_ To fto_ Do Begin
    If IsPrime(i) Then Begin
      Synchronize(@IncPrimeCount);
    End;
  End;
  Synchronize(@UnRegister_Thread);
End;

Procedure TPrimeSynchronized.IncPrimeCount;
Begin
  inc(PrimeCount);
End;

{ TPrimeQueued }

Procedure TPrimeQueued.IncPrimeCount;
Begin
  inc(PrimeCount);
End;

Procedure TPrimeQueued.Register_Thread;
Begin
  If (cnt) = 0 Then Begin
    StartTime := GetTickCount64;
  End;
  inc(cnt);
  form1.label4.caption := 'Active Threads : ' + inttostr(cnt);

End;

Procedure TPrimeQueued.UnRegister_Thread;
Begin
  dec(cnt);
  form1.label4.caption := 'Active Threads : ' + inttostr(cnt);
  If cnt = 0 Then Begin
    form1.UnBlockLCL;
    form1.PlottInfo('Queue', strtoint(form1.edit1.text), GetTickCount64() - StartTime, PrimeCount);
  End;
End;

Constructor TPrimeQueued.create(From_, To_: integer);
Begin
  Inherited create(false);
  ffrom_ := From_;
  fto_ := To_;
  FreeOnTerminate := true;
End;

Procedure TPrimeQueued.Execute;
Var
  i: Integer;
Begin
  Synchronize(@Register_Thread);
  For i := ffrom_ To fto_ Do Begin
    If IsPrime(i) Then Begin
      Queue(@IncPrimeCount);
    End;
  End;
  Synchronize(@UnRegister_Thread);
End;

{ TPrimeCritical }

Procedure TPrimeCritical.Register_Thread;
Begin
  If (cnt) = 0 Then Begin
    StartTime := GetTickCount64;
  End;
  inc(cnt);
  form1.label4.caption := 'Active Threads : ' + inttostr(cnt);
End;

Procedure TPrimeCritical.UnRegister_Thread;
Begin
  dec(cnt);
  form1.label4.caption := 'Active Threads : ' + inttostr(cnt);
  If cnt = 0 Then Begin
    form1.UnBlockLCL;
    form1.PlottInfo('Critical ', strtoint(form1.edit1.text), GetTickCount64() - StartTime, PrimeCount);
  End;
End;

Constructor TPrimeCritical.create(From_, To_: integer);
Begin
  Inherited create(false);
  ffrom_ := From_;
  fto_ := To_;
  FreeOnTerminate := true;
End;

Procedure TPrimeCritical.Execute;
Var
  i: Integer;
Begin
  Synchronize(@Register_Thread);
  For i := ffrom_ To fto_ Do Begin
    If IsPrime(i) Then Begin
      cs.Acquire;
      Try
        inc(PrimeCount);
      Finally
        cs.Release;
      End;
    End;
  End;
  Synchronize(@UnRegister_Thread);
End;

{ TLoadThread }

Constructor TPrimeRaceCondition.create(From_, To_: integer);
Begin
  Inherited create(false);
  ffrom_ := From_;
  fto_ := To_;
  FreeOnTerminate := true;
End;

Procedure TPrimeRaceCondition.Execute;
Var
  i: Integer;
Begin
  Synchronize(@Register_Thread);
  For i := ffrom_ To fto_ Do Begin
    If IsPrime(i) Then Begin
      inc(PrimeCount);
    End;
  End;
  Synchronize(@UnRegister_Thread);
End;

Procedure TPrimeRaceCondition.Register_Thread;
Begin
  If (cnt) = 0 Then Begin
    StartTime := GetTickCount64;
  End;
  inc(cnt);
  form1.label4.caption := 'Active Threads : ' + inttostr(cnt);
End;

Procedure TPrimeRaceCondition.UnRegister_Thread;
Begin
  dec(cnt);
  form1.label4.caption := 'Active Threads : ' + inttostr(cnt);
  If cnt = 0 Then Begin
    form1.UnBlockLCL;
    form1.PlottInfo('Unsave', strtoint(form1.edit1.text), GetTickCount64() - StartTime, PrimeCount);
  End;
End;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  label1.caption := 'With this program you can see what a race condition is, and how you can prevent it.' + LineEnding +
    'To show race the problem the number of prims below ' + IntToStr(UpperNumerBorder) + ' will be calculated.';
  memo1.clear;
  caption := 'Prime Calculus ver. 0.01 by corpsman | www.Corpsman.de |';
  cs := TCriticalSection.Create;
  edit1.text := '8';
  label4.caption := 'Active Threads : 0';
End;

Procedure TForm1.BlockLCL;
Begin
  Button1.Enabled := false;
  Button4.Enabled := false;
  Button5.Enabled := false;
  Button6.Enabled := false;
End;

Procedure TForm1.UnBlockLCL;
Begin
  Button1.Enabled := true;
  Button4.Enabled := true;
  Button5.Enabled := true;
  Button6.Enabled := true;
End;

Procedure TForm1.PlottInfo(Title: String; thds, Time, Value: integer);
Begin
  While length(Title) < 13 Do
    title := title + ' ';
  memo1.lines.add(Title + format('Threads(%3d), Time(%dms) = %d', [thds, time, value]));
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  i, k: integer;
Begin
  // Unsave
  BlockLCL();
  PrimeCount := 0;
  i := strtoint(edit1.text);
  If i < 1 Then Begin
    i := 1;
  End;
  For k := 0 To i - 1 Do Begin
    While (cnt >= 100) Do Begin
      sleep(1);
    End;
    Unsafe := TPrimeRaceCondition.create(round((UpperNumerBorder * (k)) / i), round((UpperNumerBorder * (k + 1)) / i));
  End;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Var
  i, k: integer;
Begin
  // Save Critical
  BlockLCL();
  PrimeCount := 0;
  i := strtoint(edit1.text);
  If i < 1 Then Begin
    i := 1;
  End;
  For k := 0 To i - 1 Do Begin
    While (cnt >= 100) Do Begin
      //showmessage('asd');
      sleep(1);
    End;
    PrimeCritical := TPrimeCritical.create(round((UpperNumerBorder * (k)) / i), round((UpperNumerBorder * (k + 1)) / i));
  End;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Var
  i, k: integer;
Begin
  // Save Synchronized
  BlockLCL;
  PrimeCount := 0;
  i := strtoint(edit1.text);
  If i < 1 Then Begin
    i := 1;
  End;
  For k := 0 To i - 1 Do Begin
    PrimeSynchronized := TPrimeSynchronized.create(round((UpperNumerBorder * (k)) / i), round((UpperNumerBorder * (k + 1)) / i));
  End;
End;

Procedure TForm1.Button6Click(Sender: TObject);
Var
  i, k: integer;
Begin
  // Save Queued
  BlockLCL;
  PrimeCount := 0;
  i := strtoint(edit1.text);
  If i < 1 Then Begin
    i := 1;
  End;
  For k := 0 To i - 1 Do Begin
    PrimeQueued := TPrimeQueued.create(round((UpperNumerBorder * (k)) / i), round((UpperNumerBorder * (k + 1)) / i));
  End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Var
  PC, i: Integer;
Begin
  // Save in Main Thread
  PC := 0;
  StartTime := GetTickCount64;
  For i := 2 To UpperNumerBorder Do Begin
    If IsPrime(i) Then Begin
      inc(PC);
    End;
  End;
  PlottInfo('Main Thread', 1, GetTickCount64() - StartTime, PC);
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: boolean);
Begin
  canclose := cnt = 0;
  If Not CanClose Then Begin
    showmessage('Error closing only possible, if no impact thread is active.');
  End;
  If CanClose Then Begin
    cs.Free;
  End;
End;

End.

