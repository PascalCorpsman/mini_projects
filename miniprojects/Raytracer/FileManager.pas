(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of SimpleRay                                             *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit FileManager;

{$MODE Delphi}

Interface

Uses sysutils, classes, uTokenizer, uncommenter;

Type

  TFiledata = Record
    Filename: String;
    Tokens: TTokenarray;
  End;

  TFilemanager = Class
  private
    FFiles: Array Of TFiledata;
  public
    Constructor Create;
    Destructor Destroy; override;
    Function ParseFile(Filename: String; Tokenizer: TTokenizer; Uncommenter: TUnCommenter): TTokenarray;
    Procedure Clear;
  End;

Implementation

{ TFilemanager }

Procedure TFilemanager.Clear;
Var
  i: Integer;
Begin
  For i := 0 To high(FFiles) Do
    Setlength(FFiles[i].Tokens, 0);
  Setlength(FFIles, 0);
End;

Constructor TFilemanager.Create;
Begin
  Inherited;
  Setlength(FFIles, 0);
End;

Destructor TFilemanager.Destroy;
Begin
  //  Inherited;
  clear;
End;

(*
Im Prinzip ist das hier das Laden und das Fparse des TRayParsers in einem.
*)

Function TFilemanager.ParseFile(Filename: String; Tokenizer: TTokenizer;
  Uncommenter: TUnCommenter): TTokenarray;
Var
  tmp: TTokenarray;
  rlc, n: Integer;
  i: Integer;
  l: Tstringlist;
Begin
  setlength(Result, 0);
  //  Filename := lowercase(Filename); -- Bei Linux ist das der Todestoß
  //  If fileexists(Filename) Then Begin // braucht nicht, da das EatExternal schon macht
  // Prüfen ob die File schon mal geladen wurde
  For i := 0 To High(FFiles) Do
    If lowercase(Ffiles[i].Filename) = lowercase(Filename) Then Begin
      result := Ffiles[i].Tokens;
      exit;
    End;
  // Laden der File
  Setlength(FFiles, high(FFiles) + 2);
  FFIles[high(FFIles)].Filename := Filename;
  l := TStringList.create;
  l.LoadFromFile(Filename);
  (******************************************************************************)
  tmp := Tokenizer.scan(Uncommenter.Uncomment(l.text));
  l.free;
  // Umformatieren das die Zeilennummern stimmen, und Rauswerfen der zusatzinformationen.
  rlc := 0;
  // Gab es überhaupt irgendwas zum Parsen ?
  If High(tmp) <> -1 Then Begin
    // Einlesen der Zeilennummern aus den Tokens
    n := -1;
    For i := High(tmp) Downto 1 Do Begin
      If tmp[i - 1].value = '~' Then Begin
        n := strtoint(tmp[i].value);
        inc(rlc, 2);
      End;
      If n <> -1 Then
        tmp[i].Line := n;
    End;
    tmp[0].line := n; // Beim 1. Token mus das Manuel gemacht werden.
    // Übernehmen der Daten in die Globale Tokenliste
    Setlength(FFIles[high(FFiles)].tokens, High(tmp) + 1 - rlc);
    n := 0;
    i := 0;
    While i <= High(tmp) Do Begin
      If tmp[i].value <> '~' Then Begin
        FFIles[high(FFiles)].tokens[n] := Tmp[i];
        inc(n);
      End
      Else Begin
        inc(i);
      End;
      inc(i);
    End;
    setlength(tmp, 0);
    // So nun da alles Geparst ist gehts an Analysieren der Tokens
    If High(FFIles[high(FFiles)].tokens) <> -1 Then Begin
      setlength(FFIles[high(FFiles)].tokens, high(FFIles[high(FFiles)].tokens) + 2);
      FFIles[high(FFiles)].tokens[high(FFIles[high(FFiles)].tokens)].Value := '"End of File"';
      FFIles[high(FFiles)].tokens[high(FFIles[high(FFiles)].tokens)].Line := FFIles[high(FFiles)].tokens[high(FFIles[high(FFiles)].tokens) - 1].Line;
      Result := FFIles[high(FFiles)].tokens;
    End;
  End;
End;

End.

