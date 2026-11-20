(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of GIT gui                                               *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit ugitprogress;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ExtCtrls;

Type

  { TGitProgress }

  TGitProgress = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Memo1: TMemo;
    ProgressBar1: TProgressBar;
    Procedure Button1Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
  private
    fAbort: Boolean;
    fRunning: Boolean;
    fScrollPending: Boolean;
    fScrollTimer: TTimer;
    fOutputTop, fOutputLeft: Integer;
    ProjectRoot: String;
    Procedure UpdateOutput(CompletedLines: TStrings; Const PartialLine: String;
      Var HasPartialLine: Boolean);
    Procedure ScrollOutput(Sender: TObject);
    Procedure SetOutputScroll(aTop, aLeft: Integer);
{$IFDEF UNIX}
    Procedure CreateProcessGroup(Sender: TObject);
{$ENDIF}
    Function RunProcess(Workdir, Executeable: String; Params: Array Of String): Boolean;

  public
    Function RunVisualCommand(aDir, aCommand: String; params: Array Of String): boolean;
    Procedure SetContinueButtonTo(aCaption: String); // TODO: um die Popup Einträge Erweitern ...
    Procedure InitProgressbar(aMax: integer);
  End;

Var
  GitProgress: TGitProgress;

Implementation

Uses process, UTF8Process, LazUTF8
{$IFDEF LCLgtk3}
  , LazGtk3, Gtk3Widgets
{$ENDIF}
{$IFDEF UNIX}
  , BaseUnix
{$ENDIF}
{$IFDEF Windows}
  , Windows, JwaWinBase
{$ENDIF}
  ;

{$R *.lfm}

{ TGitProgress }

Procedure TGitProgress.Button1Click(Sender: TObject);
Begin
  // Abort
  fAbort := true;
  Button1.Enabled := false;
End;

Procedure TGitProgress.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  CanClose := Not fRunning;
  If fRunning Then
    Button1Click(Sender)
  Else
    ModalResult := mrClose;
End;

Procedure TGitProgress.Button3Click(Sender: TObject);
Var
  p: TProcessUTF8;
  fn: String;
Begin
  // Push
  Case button3.caption Of
    'Commit': Begin
        p := TProcessUTF8.Create(Nil);
        fn := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'g_commit';
{$IFDEF Windows}
        fn := fn + '.exe';
{$ENDIF}
        p.Options := [poWaitOnExit, poNoConsole];
        p.Executable := fn;
        p.Parameters.Add(ProjectRoot);
        p.Execute;
        Application.ProcessMessages;
        sleep(100);
        p.Free;
        // Und Feierabend
        ModalResult := mrOK;
      End;
    'Push': Begin
        p := TProcessUTF8.Create(Nil);
        fn := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + 'g_push';
{$IFDEF Windows}
        fn := fn + '.exe';
{$ENDIF}
        p.Options := [poWaitOnExit, poNoConsole];
        p.Executable := fn;
        p.Parameters.Add(ProjectRoot);
        p.Execute;
        Application.ProcessMessages;
        sleep(100);
        p.Free;
        // Und Feierabend
        ModalResult := mrOK;
      End;
  Else Begin
      showmessage(button3.caption + ' not yet implemented.');
    End;
  End;
End;

Procedure TGitProgress.FormCreate(Sender: TObject);
Begin
  ProjectRoot := '';
  button3.Enabled := false;
  fScrollTimer := TTimer.Create(Self);
  fScrollTimer.Enabled := false;
  fScrollTimer.Interval := 40;
  fScrollTimer.OnTimer := @ScrollOutput;
End;

Procedure TGitProgress.UpdateOutput(CompletedLines: TStrings;
  Const PartialLine: String; Var HasPartialLine: Boolean);
Var
  FollowOutput: Boolean;
  OldTop, OldLeft, OldStart, OldLength, i: Integer;
Begin
  OldTop := Memo1.VertScrollBar.Position;
  OldLeft := Memo1.HorzScrollBar.Position;
  OldStart := Memo1.SelStart;
  OldLength := Memo1.SelLength;
  FollowOutput := (OldLength = 0) And (fScrollPending Or
    (OldTop + Memo1.VertScrollBar.Page >= Memo1.VertScrollBar.Range - 1));
  Memo1.Lines.BeginUpdate;
  Try
    i := 0;
    If HasPartialLine Then Begin
      If CompletedLines.Count > 0 Then Begin
        Memo1.Lines[Memo1.Lines.Count - 1] := CompletedLines[0];
        i := 1;
      End
      Else If PartialLine <> '' Then
        Memo1.Lines[Memo1.Lines.Count - 1] := PartialLine
      Else
        Memo1.Lines.Delete(Memo1.Lines.Count - 1);
    End;
    While i < CompletedLines.Count Do Begin
      Memo1.Lines.Add(CompletedLines[i]);
      Inc(i);
    End;
    If (PartialLine <> '') And
      ((Not HasPartialLine) Or (CompletedLines.Count > 0)) Then
      Memo1.Lines.Add(PartialLine);
    HasPartialLine := PartialLine <> '';
  Finally
    Memo1.Lines.EndUpdate;
  End;
  If FollowOutput Then Begin
    // GTK calculates the new scroll range after the text update returns.
    If Not fScrollPending Then Begin
      fScrollPending := true;
      fScrollTimer.Enabled := true;
    End;
  End
  Else Begin
    Memo1.SelStart := OldStart;
    Memo1.SelLength := OldLength;
    SetOutputScroll(OldTop, OldLeft);
    fScrollPending := false;
    fOutputTop := OldTop;
    fOutputLeft := OldLeft;
    fScrollTimer.Enabled := true;
  End;
End;

Procedure TGitProgress.ScrollOutput(Sender: TObject);
Var
  OldLeft: Integer;
Begin
  fScrollTimer.Enabled := false;
  If Visible Then Begin
    If fScrollPending Then Begin
      If Memo1.SelLength = 0 Then Begin
        OldLeft := Memo1.HorzScrollBar.Position;
        Memo1.SelStart := UTF8Length(Memo1.Text);
        SetOutputScroll(Memo1.VertScrollBar.Range, OldLeft);
      End;
    End
    Else
      SetOutputScroll(fOutputTop, fOutputLeft);
  End;
  fScrollPending := false;
End;

Procedure TGitProgress.SetOutputScroll(aTop, aLeft: Integer);
{$IFDEF LCLgtk3}
Var
  ScrollWindow: PGtkScrolledWindow;
{$ENDIF}
Begin
{$IFDEF LCLgtk3}
  // Avoid recursive scrollbar signal initialization in the GTK3 LCL backend.
  ScrollWindow := PGtkScrolledWindow(TGtk3Memo(Memo1.Handle).Widget);
  gtk_adjustment_set_value(gtk_scrolled_window_get_vadjustment(ScrollWindow), aTop);
  gtk_adjustment_set_value(gtk_scrolled_window_get_hadjustment(ScrollWindow), aLeft);
{$ELSE}
  Memo1.VertScrollBar.Position := aTop;
  Memo1.HorzScrollBar.Position := aLeft;
{$ENDIF}
End;

{$IFDEF UNIX}
Procedure TGitProgress.CreateProcessGroup(Sender: TObject);
Const
  ErrorText = 'Could not create process group.'#10;
Begin
  // This runs in the child, before exec; never access the GUI here.
  If fpSetSid = -1 Then Begin
    fpWrite(2, ErrorText[1], Length(ErrorText));
    fpExit(127);
  End;
End;
{$ENDIF}

Function TGitProgress.RunProcess(Workdir, Executeable: String;
  Params: Array Of String): Boolean;
Const
  BUF_SIZE = 2048;
  UPDATE_INTERVAL = 40;
  READ_BUDGET = 65536;
Var
  P: TProcessUTF8;
  i, Start, BytesThisPass, LastChar: Integer;
  Buffer: Array[0..BUF_SIZE - 1] Of char;
  BytesRead: LongInt;
  Chunk, CurrentLine, PartialLine: String;
  CompletedLines: TStringList;
  PendingCR, HasPartialLine, Dirty, Terminated: Boolean;
  LastUpdate: QWord;
{$IFDEF Windows}
  Job: Windows.THandle;
{$ENDIF}

  Procedure StopProcess;
  Begin
{$IFDEF UNIX}
    If (fpKill(-p.ProcessID, SIGKILL) <> 0) And
      (fpGetErrNo <> ESysESRCH) Then
      RaiseLastOSError;
{$ELSE}
{$IFDEF Windows}
    If Not TerminateJobObject(Job, 1) Then
      RaiseLastOSError;
{$ELSE}
    If p.Running And Not p.Terminate(1) Then
      RaiseLastOSError;
{$ENDIF}
{$ENDIF}
  End;

  Procedure FlushOutput(FinalUpdate: Boolean);
  Begin
    If Not Dirty Then exit;
    PartialLine := CurrentLine;
    If (Not FinalUpdate) And (PartialLine <> '') Then Begin
      // Do not hand a UTF-8 character split across pipe reads to the widgetset.
      LastChar := Length(PartialLine);
      While (LastChar > 1) And
        ((Ord(PartialLine[LastChar]) And $C0) = $80) Do
        Dec(LastChar);
      If LastChar + UTF8CodepointSizeFast(@PartialLine[LastChar]) - 1 >
        Length(PartialLine) Then
        SetLength(PartialLine, LastChar - 1);
    End;
    UpdateOutput(CompletedLines, PartialLine, HasPartialLine);
    CompletedLines.Clear;
    Dirty := false;
  End;

Begin
  fRunning := true;
  button2.Enabled := false;
  button1.Enabled := true;
  button3.Enabled := false;
  result := false;
  p := Nil;
  CompletedLines := Nil;
{$IFDEF Windows}
  Job := 0;
{$ENDIF}
  Try
    CompletedLines := TStringList.Create;
    p := TProcessUTF8.Create(Nil);
    p.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
    p.CurrentDirectory := Workdir;
    p.Executable := Executeable;
    For i := 0 To high(Params) Do Begin
      p.Parameters.Add(Params[i]);
    End;
{$IFDEF UNIX}
    p.OnForkEvent := @CreateProcessGroup;
{$ENDIF}
{$IFDEF Windows}
    Job := CreateJobObjectW(Nil, Nil);
    If Job = 0 Then RaiseLastOSError;
    p.Options := p.Options + [poRunSuspended];
{$ENDIF}
    p.execute;
{$IFDEF Windows}
    If Not AssignProcessToJobObject(Job, p.ProcessHandle) Then
      RaiseLastOSError;
    If p.Resume = -1 Then RaiseLastOSError;
{$ENDIF}
    CurrentLine := '';
    PendingCR := false;
    HasPartialLine := false;
    Dirty := false;
    Terminated := false;
    LastUpdate := GetTickCount64;
    While p.Running Or (p.Output.NumBytesAvailable > 0) Do Begin
      BytesThisPass := 0;
      While (p.Output.NumBytesAvailable > 0) And
        (BytesThisPass < READ_BUDGET) And
        (GetTickCount64 - LastUpdate < UPDATE_INTERVAL) Do Begin
        BytesRead := p.Output.Read(Buffer, BUF_SIZE);
        If BytesRead = 0 Then break;
        Inc(BytesThisPass, BytesRead);
        SetString(Chunk, PChar(@Buffer[0]), BytesRead);
        Dirty := true;
        Start := 0;
        For i := 0 To BytesRead - 1 Do Begin
          If PendingCR Then Begin
            If Buffer[i] <> #10 Then CurrentLine := '';
            PendingCR := false;
          End;
          If Buffer[i] In [#10, #13] Then Begin
            CurrentLine := CurrentLine + Copy(Chunk, Start + 1, i - Start);
            Start := i + 1;
            If Buffer[i] = #10 Then Begin
              CompletedLines.Add(CurrentLine);
              CurrentLine := '';
            End
            Else
              PendingCR := true;
          End;
        End;
        CurrentLine := CurrentLine + Copy(Chunk, Start + 1, BytesRead - Start);
      End;
      If GetTickCount64 - LastUpdate >= UPDATE_INTERVAL Then Begin
        FlushOutput(false);
        LastUpdate := GetTickCount64;
      End;
      Application.ProcessMessages;
      If Application.Terminated Then fAbort := true;
      If fAbort And Not Terminated Then Begin
        StopProcess;
        Terminated := true;
      End;
      If BytesThisPass = 0 Then Sleep(10);
    End;
    FlushOutput(true);
    result := (p.ExitCode = 0) And Not fAbort;
    If (Not result) And (Not fAbort) Then Begin
      CompletedLines.Add('Exit code: ' + IntToStr(p.ExitCode));
      HasPartialLine := false;
      UpdateOutput(CompletedLines, '', HasPartialLine);
    End;
  Finally
    Try
      If Assigned(p) And p.Running Then Begin
        StopProcess;
        // Also covers a suspended Windows process whose job assignment failed.
        If p.Running And Not p.Terminate(1) Then RaiseLastOSError;
      End;
    Finally
      p.Free;
      CompletedLines.Free;
{$IFDEF Windows}
      If Job <> 0 Then CloseHandle(Job);
{$ENDIF}
      fRunning := false;
      button2.Enabled := true;
      button1.Enabled := false;
    End;
  End;
End;

Function TGitProgress.RunVisualCommand(aDir, aCommand: String;
  params: Array Of String): boolean;
Var
  t: QWord;
  Status: String;
  StatusLines: TStringList;
  HasPartialLine: Boolean;
Begin
  If fRunning Then
    raise EInvalidOperation.Create('A Git command is already running.');
  ProjectRoot := aDir;
  fScrollTimer.Enabled := false;
  fScrollPending := false;
  ModalResult := mrNone;
  Button3.Enabled := false;
  fAbort := false;
  t := GetTickCount64;
  StatusLines := TStringList.Create;
  Try
  Try
    result := RunProcess(adir, acommand, params);
  Except
    On E: EProcess Do Begin
      StatusLines.Add('Error: ' + E.Message);
      result := false;
    End;
    On E: EOSError Do Begin
      StatusLines.Add('Error: ' + E.Message);
      result := false;
    End;
    On E: EStreamError Do Begin
      StatusLines.Add('Error: ' + E.Message);
      result := false;
    End;
  End;
  If result Then Begin
    ProgressBar1.Position := ProgressBar1.Max;
    Status := 'Success';
    ProgressBar1.Color := clGreen;
    Button3.Enabled := true;
  End
  Else Begin
    If fAbort Then Status := 'Aborted' Else Status := 'Failed';
    ProgressBar1.Color := clRed;
  End;
  StatusLines.Add('');
  StatusLines.Add(Status + ' (' + inttostr(GetTickCount64 - t) + ' ms @ ' +
    FormatDateTime('dd.mm.yyyy hh:nn:ss', Now) + ')');
  HasPartialLine := false;
  UpdateOutput(StatusLines, '', HasPartialLine);
  Finally
    StatusLines.Free;
  End;
  // Wir Warten bis der User Beendet
  While (ModalResult = mrNone) And (Not fAbort) And
    (Not Application.Terminated) Do Begin
    Application.ProcessMessages;
    sleep(10);
  End;
  hide;
  fScrollTimer.Enabled := false;
  fScrollPending := false;
  result := result And Not fAbort And Not Application.Terminated;
End;

Procedure TGitProgress.SetContinueButtonTo(aCaption: String);
Begin
  Button3.caption := aCaption;
End;

Procedure TGitProgress.InitProgressbar(aMax: integer);
Begin
  ProgressBar1.Max := aMax;
  ProgressBar1.Position := 0;
  ProgressBar1.Color := clDefault;
End;

End.

