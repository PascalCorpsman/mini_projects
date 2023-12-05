(******************************************************************************)
(* GIT gui                                                         26.11.2023 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Tortoise GIT like gui for the GIT application                *)
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
Unit ugit_common;

{$MODE ObjFPC}{$H+}

Interface
(*

https://til.simonwillison.net/jq/git-log-json

https://git-scm.com/docs/pretty-formats

https://stackoverflow.com/questions/4600445/git-log-output-to-xml-json-or-yaml

https://stackoverflow.com/questions/51228892/get-json-from-git-commands-such-as-git-status
*)

Uses
  Classes, SysUtils;

Const
  TextNotVersioned = 'Unknown';
  TextModified = 'Modified';
  TextAdded = 'Added';
  TextDeleted = 'Deleted';
  TextRenamed = 'Renamed';
  TextCopied = 'Copied';

Type

  TCommitStatus = (
    csNotVersioned,
    csModified,
    csAddedWithModifications,
    csModifiedWithFurtherModifications,
    csAdded,
    csDeleted,
    csRenamed,
    csCopied
    );

  TCommitFileInfo = Record
    FileName: String;
    Status: TCommitStatus;
    LinesAdded: integer;
    LinesRemoved: integer;
  End;

  TBranchSelector = Record
    Local, Remote: String;
  End;

  TCommitInformations = Record
    BranchSelector: TBranchSelector;
    RepoRoot: String;
    CommitFileInfo: Array Of TCommitFileInfo;
  End;

  TRepoSettings = Record
    UserName: String;
    EMail: String;
    eof: String;
    DiffTool: String;
  End;

  TBranchInfo = Record
    Locals: Array Of String;
    ActualLocal: String;
    Remotes: Array Of String;
    Destinations: Array Of String;
  End;

  (*
   * Für den Commit Dialog
   * - Liste aller hinzugefügten / Modifizierten Dateien...
   *)
Function GetCommitInformations(Const aDir: String): TCommitInformations;

(*
 * Für die Optionen Username, E-mail ....
 *)
Function GetRepoSettings(Const aDir: String; aSource: String): TRepoSettings;
Function GetEffectiveRepoSettings(Const aDir: String): TRepoSettings; // Leitet aus den verfügbaren konfigurationen diejenige ab, welche "effektiv" gillt
Procedure SetRepoSettings(Const aDir: String; aSource: String; Const aValue: TRepoSettings);

(*
 * Listet alle Localen und Remote Branches
 *)
Function GetBrancheInfo(Const aDir: String): TBranchInfo;

(*
 * Leer, wenn aDir nicht Teil eines Git Repo's ist
 * sonst Root des Git Repos
 *)
Function GetRepoRoot(Const aDir: String): String;

(*
 * Helper Routines
 *)
Function StatusToString(Const Value: TCommitStatus): String;

(*
 * Führt direkt einen Befehl in Workdiraus
 *)
Function RunCommand(WorkDir: String; Command: String; Params: Array Of String): TStringlist;

Implementation

Uses UTF8Process, process;

Procedure Nop();
Begin

End;

Function RunCommand(WorkDir: String; Command: String; Params: Array Of String
  ): TStringlist;
Var
  AD: String;
  P: TProcessUTF8;
  i: Integer;
Begin
  result := TStringList.Create;
  ad := GetCurrentDir;
  SetCurrentDir(WorkDir);
  p := TProcessUTF8.Create(Nil);
  p.Options := [poUsePipes, poStderrToOutPut, poNoConsole, poWaitOnExit];
  p.Executable := Command;
  For i := 0 To high(Params) Do Begin
    p.Parameters.Add(Params[i]);
  End;
  p.Execute;
  result.LoadFromStream(p.Output);
  p.free;
  SetCurrentDir(ad);
End;

Function ExtractBranchSelector(value: String): TBranchSelector;
Begin
  result.Local := '';
  result.Remote := '';
  If pos('##', value) = 0 Then exit;
  If pos('...', value) = 0 Then exit;
  value := trim(copy(value, pos('##', value) + 2, length(value)));
  result.Local := copy(value, 1, pos('...', value) - 1);
  result.Remote := copy(value, pos('...', value) + 3, length(value));
End;

(*
The git status --porcelain command produces a machine-readable output with two-character status codes for each file. The status codes represent the status of each file in the working directory. Here are the possible results:

    Untracked files:
        ??: Untracked file

    Changes in the working directory:
        M: Modified
        AM: Added with modifications
        MM: Modified with further modifications
        A : Added (space indicates a new, previously untracked file)
        D : Deleted
        R : Renamed
        C : Copied

    Changes staged for commit:
        M: Modified
        AM: Added with modifications
        MM: Modified with further modifications
        A : Added
        D : Deleted
        R : Renamed
        C : Copied

    Unmerged paths:
        DD: Both deleted
        AU: Added by us
        UD: Deleted by them
        UA: Added by them
        DU: Deleted by us
        AA: Both added
        UU: Both modified

    Submodules:
        SM: Submodule changed
        S: Submodule added
        U: Submodule untracked
        X: Submodule has uncommitted changes
        -: Submodule is not initialized

Each line of the output represents the status of a file in the working directory. The first character represents the status of the index (staging area), and the second character represents the status of the working directory. If a file is untracked, the index status is a space.

For example:

    M in the first column indicates modifications in the index.
    M in the second column indicates modifications in the working directory.

You might also encounter combinations like AM or MM indicating that a file has been modified and then had additional modifications in the working directory or staging area, respectively.
*)

Function StrToStatus(Value: String): TCommitStatus;
Begin
  value := trim(value);
  Case value Of
    '??': result := csNotVersioned;
    'M': result := csModified;
    'AM': result := csAddedWithModifications;
    'MM': result := csModifiedWithFurtherModifications;
    'A': result := csAdded;
    'D': result := csDeleted;
    'R': result := csRenamed;
    'C': result := csCopied;
  Else Begin
      Raise exception.create('StrToStatus: Error, unknown value "' + Value + '"');
    End;
  End;
End;

Function StatusToString(Const Value: TCommitStatus): String;
Begin
  Case value Of
    csNotVersioned: result := TextNotVersioned;
    csModified
      , csAddedWithModifications
      , csModifiedWithFurtherModifications
      : result := TextModified;
    csAdded: result := TextAdded;
    csDeleted: result := TextDeleted;
    csRenamed: result := TextRenamed;
    csCopied: result := TextCopied;
  Else Begin
      result := TextNotVersioned;
    End;
  End;
End;

Function ExtractCommitFileInfo(value: String): TCommitFileInfo;
Begin
  result.LinesAdded := 0;
  result.LinesRemoved := 0;
  result.Status := StrToStatus(copy(value, 1, 2));
  result.FileName := copy(value, 4, length(value));
  If pos('"', result.FileName) = 1 Then Begin
    delete(result.FileName, 1, 1);
    delete(result.FileName, length(Result.FileName), 1);
  End;
End;

Procedure UpdateCommitFileInfo(Var value: TCommitFileInfo; Data: String);
Var
  i: Integer;
Begin
  For i := 1 To length(data) Do Begin
    If data[i] = '+' Then inc(value.LinesAdded);
    If data[i] = '-' Then inc(value.LinesRemoved);
  End;
End;

Function GetCommitInformations(Const aDir: String): TCommitInformations;
Var
  Res: TStringlist;
  i: Integer;
  fn, s: String;
Begin
  result.RepoRoot := aDir;
  // 1. Liste der Geänderten Dateien und des Aktuellen Branchs auslesen
  res := RunCommand(aDir, 'git', ['status', '-sb', '--porcelain']);
  result.BranchSelector := ExtractBranchSelector(res[0]);
  setlength(result.CommitFileInfo, Res.Count - 1);
  For i := 1 To Res.Count - 1 Do Begin
    result.CommitFileInfo[i - 1] := ExtractCommitFileInfo(Res[i]);
  End;
  res.free;
  // 2. Die "Diffs" nachladen
  For i := 0 To high(result.CommitFileInfo) Do Begin
    Case result.CommitFileInfo[i].Status Of
      csNotVersioned: Begin
          // Nichts, nicht versionierte Dateien werden nicht Linienmäßig betrachtet
        End;
      csAdded: Begin
          fn := IncludeTrailingPathDelimiter(aDir) + result.CommitFileInfo[i].FileName;
          res := TStringList.Create;
          res.LoadFromFile(fn);
          result.CommitFileInfo[i].LinesAdded := res.Count;
          res.free;
        End;
      csDeleted: Begin
          // Die Gelöschte Datei existiert ja nicht mehr, also lassen wir sie uns "anzeigen" und lesen die Größe von Hand aus ..
          res := RunCommand(aDir, 'git', ['show', 'HEAD:' + result.CommitFileInfo[i].FileName]);
          result.CommitFileInfo[i].LinesRemoved := res.Count;
          res.free;
        End;
      csModified: Begin
          res := RunCommand(aDir, 'git', ['diff', '--stat', result.CommitFileInfo[i].FileName]);
          If res.Count = 2 Then Begin
            UpdateCommitFileInfo(result.CommitFileInfo[i], res[0]);
          End
          Else Begin
            s := res.text;
            If s <> '' Then Begin
              Raise exception.create('Noch mal genau nachsehen für: ' + result.CommitFileInfo[i].FileName + LineEnding + s);
            End;
          End;
          res.free;
        End;
    Else Begin
        Raise Exception.create('GetCommitInformations: Unhandled Status, need more implementations !');
      End;
    End;
  End;
End;

Function GetBrancheInfo(Const aDir: String): TBranchInfo;
Var
  res: TStringList;
  i: Integer;
  s: String;
Begin
  result.Locals := Nil;
  result.Remotes := Nil;
  result.Destinations := Nil;
  result.ActualLocal := '';
  // Die Localen
  res := RunCommand(aDir, 'git', ['branch']);
  setlength(result.Locals, res.Count);
  For i := 0 To res.count - 1 Do Begin
    s := res[i];
    result.Locals[i] := trim(copy(s, 3, length(s)));
    If pos('*', copy(s, 1, 2)) <> 0 Then Begin
      result.ActualLocal := result.Locals[i];
    End;
  End;
  res.free;
  // Destinations
  res := RunCommand(aDir, 'git', ['remote']);
  setlength(result.Destinations, res.count);
  For i := 0 To res.Count - 1 Do Begin
    Result.Destinations[i] := trim(res[i]);
  End;

  // Die Remotes
  res := RunCommand(aDir, 'git', ['branch', '-r']);
  setlength(result.Remotes, res.Count);
  For i := 0 To res.Count - 1 Do Begin
    result.Remotes[i] := trim(res[i]);
  End;
  res.free;
End;

Function GetRepoRoot(Const aDir: String): String;
Var
  Res: TStringlist;
Begin
  result := '';
  res := RunCommand(aDir, 'git', ['rev-parse', '--show-toplevel']);
  If res.count > 0 Then Begin
    result := res[0];
{$IFDEF Windows}
    result := StringReplace(result, '/', PathDelim, [rfReplaceAll]);
{$ENDIF}
    If DirectoryExists(Result) Then Begin
      result := IncludeTrailingPathDelimiter(Result);
    End
    Else Begin
      result := '';
    End;
  End;
  res.free;
End;

Function GetRepoSettings(Const aDir: String; aSource: String): TRepoSettings;
Var
  Res: TStringlist;
Begin
  aSource := '--' + aSource;
  // Username
  res := RunCommand(adir, 'git', ['config', aSource, 'user.name']);
  result.UserName := trim(res.text);
  res.free;
  // E-Mail
  res := RunCommand(adir, 'git', ['config', aSource, 'user.email']);
  result.EMail := trim(res.text);
  res.free;
  // Difftool
  res := RunCommand(adir, 'git', ['config', aSource, 'diff.tool']);
  result.DiffTool := trim(res.text);
  res.free;
  // LineEndings
  res := RunCommand(adir, 'git', ['config', aSource, 'core.eol']);
  result.eof := trim(res.text);
  res.free;
End;

Function GetEffectiveRepoSettings(Const aDir: String): TRepoSettings;
Var
  aGlobal, aSystem: TRepoSettings;
Begin
  // Loacl führt
  Result := GetRepoSettings(aDir, 'local');
  // Local definiert nicht "ausreichend" -> Erben von global
  If (Result.UserName = '') Or
    (Result.eof = '') Or
    (Result.EMail = '') Or
    (Result.DiffTool = '') Then Begin
    aGlobal := GetRepoSettings(aDir, 'global');
    If Result.UserName = '' Then result.UserName := aGlobal.UserName;
    If Result.eof = '' Then result.eof := aGlobal.eof;
    If Result.EMail = '' Then result.EMail := aGlobal.EMail;
    If Result.DiffTool = '' Then result.DiffTool := aGlobal.DiffTool;
  End;
  // (local und global) definieren nicht "ausreichend" -> Erben von system
  If (Result.UserName = '') Or
    (Result.eof = '') Or
    (Result.EMail = '') Or
    (Result.DiffTool = '') Then Begin
    aSystem := GetRepoSettings(aDir, 'system');
    If Result.UserName = '' Then result.UserName := aSystem.UserName;
    If Result.eof = '' Then result.eof := aSystem.eof;
    If Result.EMail = '' Then result.EMail := aSystem.EMail;
    If Result.DiffTool = '' Then result.DiffTool := aSystem.DiffTool;
  End;
End;

Procedure SetRepoSettings(Const aDir: String; aSource: String;
  Const aValue: TRepoSettings);
Var
  Res: TStringlist;
Begin
  aSource := '--' + aSource;
  // Username
  If trim(aValue.UserName) <> '' Then Begin
    res := RunCommand(adir, 'git', ['config', aSource, 'user.name', trim(aValue.UserName)]);
    res.free;
  End;
  // E-Mail
  If trim(aValue.EMail) <> '' Then Begin
    res := RunCommand(adir, 'git', ['config', aSource, 'user.email', trim(aValue.EMail)]);
    res.free;
  End;
  // Difftool
  If trim(aValue.DiffTool) <> '' Then Begin
    res := RunCommand(adir, 'git', ['config', aSource, 'diff.tool', trim(aValue.DiffTool)]);
    res.free;
  End;
  // LineEndings
  If trim(aValue.eof) <> '' Then Begin
    res := RunCommand(adir, 'git', ['config', aSource, 'core.eol', trim(aValue.eof)]);
    res.free;
  End;
End;

End.

