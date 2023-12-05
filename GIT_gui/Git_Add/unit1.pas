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
Begin
  GitProgress.ProgressBar1.Position := 0;
  GitProgress.ProgressBar1.Max := ParamCount;
  GitProgress.Memo1.Clear;
  GitProgress.ModalResult := mrNone;
  GitProgress.SetContinueButtonTo('Commit');
  GitProgress.Show;
  // TODO: Das sollte auch mit mehr als nur einer Datei gehen ..
  GitProgress.Memo1.Lines.Add('git add -v "' + trim(ParamStrUTF8(1)) + '"');
  If GitProgress.RunVisualCommand(ProjectRoot, 'git', ['add', '-v', trim(ParamStrUTF8(1))]) Then Begin
    close;
  End
  Else Begin
    close;
  End;
End;

End.

