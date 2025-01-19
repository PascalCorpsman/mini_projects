(******************************************************************************)
(* uunbufstringlist                                                ??.??.???? *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : a Stringlist that reads directly from the harddrive without  *)
(*               buffering the filecontent in RAM                             *)
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
Unit uunbufstringlist;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils;

Type

  { TUnbufferedStringList }

  TUnbufferedStringList = Class
  private
    IOBuffer: Array[0..(1024 * 4) - 1] Of byte; //  Der Raspi scheint eine 4KB Paging buffer size zu haben ..
    fFilename: String;
    ffile: TextFile;
    Procedure CloseFile;
  public
    Constructor Create;
    Destructor Destroy; override;
    (*
     * Datei wird beim erneuten Laden oder .free Automatisch geschlossen
     *)
    Function LoadFromFile(Const Filename: String): Boolean;

    (*
     * Zugriff auf die Daten..
     *)
    Procedure Reset();
    Function EOF: Boolean;
    Function Readln(): String;
  End;

Implementation

{ TUnbufferedStringList }

Constructor TUnbufferedStringList.Create;
Begin
  fFilename := '';
End;

Destructor TUnbufferedStringList.Destroy;
Begin
  CloseFile;
  Inherited Destroy;
End;

Procedure TUnbufferedStringList.CloseFile;
Begin
  If fFilename <> '' Then objpas.CloseFile(ffile);
End;

Function TUnbufferedStringList.LoadFromFile(Const Filename: String): Boolean;
Begin
  result := false;
  CloseFile;
  fFilename := '';
  If FileExists(Filename) Then Begin
    fFilename := Filename;
    assignfile(ffile, Filename);
    system.reset(ffile);
    system.SetTextBuf(ffile, IOBuffer);
    result := true;
  End;
End;

Procedure TUnbufferedStringList.Reset();
Begin
  If fFilename <> '' Then Begin
    system.Reset(ffile);
  End;
End;

Function TUnbufferedStringList.EOF: Boolean;
Begin
  result := true;
  If fFilename <> '' Then Begin
    result := system.EOF(ffile);
  End;
End;

Function TUnbufferedStringList.Readln(): String;
Begin
  result := '';
  If fFilename <> '' Then Begin
    system.Readln(ffile, result);
  End;
End;

End.

