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
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Procedure FormCreate(Sender: TObject);
    Procedure FormShow(Sender: TObject);
  private

  public
    ProjectRoot: String;

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses ugit_common, LazFileUtils, LazUTF8, ugitprogress, LCLType, lclintf;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Var
  aDir: String;
  i: Integer;
Begin
  (*
   * Known Bugs:
   *)
  aDir := '';
  For i := 1 To Paramcount Do Begin
    If DirectoryExistsUTF8(trim(ParamStrUTF8(i))) Then Begin
      aDir := IncludeTrailingPathDelimiter(trim(ParamStrUTF8(i)));
      break;
    End;
    If FileExistsUTF8(trim(ParamStrUTF8(i))) Then Begin
      aDir := trim(ParamStrUTF8(i));
      break;
    End;
  End;
  ProjectRoot := GetRepoRoot(aDir);
  If ProjectRoot = '' Then Begin
    showmessage('"' + aDir + '" is not a valid git repository.');
    halt;
  End;
End;

Procedure TForm1.FormShow(Sender: TObject);
Var
  s: String;
  sl: TStringList;
  i: Integer;
  a: TStringArray;
Begin
  GitProgress.InitProgressbar(ParamCount);
  GitProgress.Memo1.Clear;
  GitProgress.ModalResult := mrNone;
  GitProgress.SetContinueButtonTo('Commit');
  GitProgress.Show;
  sl := TStringList.create;
  sl.Text := ParamStrUTF8(1);
  a := Nil;
  setlength(a, sl.Count + 3);
  a[0] := 'add';
  a[1] := '-v';
  a[2] := '-f';
  s := '';
  For i := 0 To sl.Count - 1 Do Begin
    a[i + 3] := sl[i];
    s := s + ' "' + sl[i] + '"';
  End;
  GitProgress.Memo1.Lines.Add('git add -v -f ' + trim(s));
  GitProgress.RunVisualCommand(ProjectRoot, 'git', a);
  sl.free;
  close;
End;

End.

