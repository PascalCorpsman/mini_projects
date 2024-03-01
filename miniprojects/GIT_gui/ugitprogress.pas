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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls;

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
    Procedure FormCreate(Sender: TObject);
  private
    fAbort: Boolean;
    ProjectRoot: String;
    Function RunProcess(Workdir, Executeable: String; Params: Array Of String): Boolean;

  public
    Function RunVisualCommand(aDir, aCommand: String; params: Array Of String): boolean;
    Procedure SetContinueButtonTo(aCaption: String); // TODO: um die Popup Einträge Erweitern ...
  End;

Var
  GitProgress: TGitProgress;

Implementation

Uses process, UTF8Process;

{$R *.lfm}

{ TGitProgress }

Procedure TGitProgress.Button1Click(Sender: TObject);
Begin
  // Abort
  fAbort := true;
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
End;

Function TGitProgress.RunProcess(Workdir, Executeable: String;
  Params: Array Of String): Boolean;
Const
  BUF_SIZE = 2048; // Buffer size for reading the output in chunks
Var
  P: TProcessUTF8;
  i: Integer;
  Buffer: Array[0..BUF_SIZE] Of char;
  BytesRead: LongInt;
  t, s, cd: String;
  InBuffer: String;
  index: Integer;
Begin
  button2.Enabled := false;
  button1.Enabled := true;
  result := false;
  cd := GetCurrentDir;
  Try
    SetCurrentDir(Workdir);
    p := TProcessUTF8.Create(Nil);
    p.Options := [poUsePipes, poStderrToOutPut, poNoConsole];
    p.Executable := Executeable;
    For i := 0 To high(Params) Do Begin
      p.Parameters.Add(Params[i]);
    End;
    p.execute;
    s := '';
    InBuffer := '';
    Buffer[0] := #0; // Compilerwarnung platt machen
    (*
     * Loop until Programm is finished and all buffers are empty
     *)
    While p.Running Or (p.Output.NumBytesAvailable > 0) Do Begin
      While (p.Output.NumBytesAvailable > 0) Do Begin
        BytesRead := p.Output.Read(Buffer, BUF_SIZE);
        setlength(s, BytesRead);
        For i := 1 To BytesRead Do Begin
          s[i] := Buffer[i - 1];
        End;
        InBuffer := InBuffer + s;
        // Zeilenweises Auslesen
        index := pos(#10, InBuffer); // Git nutzt immer #10 als CRT ;)
        While index <> 0 Do Begin
          t := copy(InBuffer, 1, index - 1);
          Memo1.Lines.Append(t);
          delete(InBuffer, 1, index);
          index := pos(#10, InBuffer);
        End;
      End;
      // if user wants to interact with the scripts
      //If SendToConsole <> '' Then Begin
        //P.Input.Write(SendToConsole[1], Length(SendToConsole));
        //SendToConsole := '';
      //End;
      Application.ProcessMessages;
      Sleep(1);
      // Der Benutzer will das das Programm beendet wird, also schiesen wir hier den Prozess händisch ab!
      If fAbort Then Begin
        p.Terminate(1);
      End;
    End;
    If InBuffer <> '' Then Begin
      Memo1.Lines.Append(InBuffer);
    End;
    result := p.ExitCode = 0;
    p.free;
  Finally
    SetCurrentDir(cd);
    button2.Enabled := true;
    button1.Enabled := false;
  End;
End;

Function TGitProgress.RunVisualCommand(aDir, aCommand: String;
  params: Array Of String): boolean;
Var
  t: QWord;
Begin
  ProjectRoot := aDir;
  result := true;
  fAbort := false;
  t := GetTickCount64;
  If RunProcess(adir, acommand, params) Then Begin
    ProgressBar1.Position := ProgressBar1.Max;
    Memo1.Append('');
    Memo1.Append('Success (' + inttostr(GetTickCount64 - t) + ' ms @ ' + FormatDateTime('dd.mm.yyyy hh:nn:ss', Now) + ')');
    Button3.Enabled := true;
  End
  Else Begin
    Memo1.Append('');
    Memo1.Append('Failed (' + inttostr(GetTickCount64 - t) + ' ms @ ' + FormatDateTime('dd.mm.yyyy hh:nn:ss', Now) + ')');
  End;
  // Wir Warten bis der User Beendet
  While (ModalResult = mrNone) And (Not fAbort) Do Begin
    Application.ProcessMessages;
    sleep(1);
  End;
  hide;
  result := Not fAbort;
End;

Procedure TGitProgress.SetContinueButtonTo(aCaption: String);
Begin
  Button3.caption := aCaption;
End;

End.

