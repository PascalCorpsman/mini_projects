(******************************************************************************)
(* TMixFile ver. 0.01                                              2018.05.15 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Corpsman                                                     *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Class that can hold lots of Streams differented by a         *)
(*               unique name(value).                                          *)
(*                                                                            *)
(* License     : This component is postcardware for non commercial use only.  *)
(*               If you like the component, send me a postcard:               *)
(*                                                                            *)
(*                    Uwe Schächterle                                         *)
(*                    Buhlstraße 85                                           *)
(*                    71384 Weinstadt - Germany                               *)
(*                                                                            *)
(*               It is not allowed to change or remove this license from any  *)
(*               source file of the project.                                  *)
(*                                                                            *)
(*                                                                            *)
(* Warranty    : There is no warranty, use at your own risk.                  *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*                                                                            *)
(* Known Bugs  : none                                                         *)
(*                                                                            *)
(******************************************************************************)

Unit umixfile;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils;

Type

  (*
   * Achtung die Mix Datei wird komplett in den RAM geladen, dieser sollte also
   * groß genug sein (oder die Mix Datei entsprechend klein *g* )!
   *)

  { TMixfile }

  TMixfile = Class
  private
    fFilename: String;
  public
    Constructor Create(Filename: String);
    Destructor Destroy; override;
    (*
     * ACHTUNG ValueName ist Case Sensitiv !!
     *)
    Function LoadValue(ValueName: String; Const Data: TStream): Boolean;

    Function AddValue(ValueName: String; Data: TStream): Boolean;

    Function DeleteValue(ValueName: String): Boolean; // True, wenn das Element gefunden und gelöscht werden konnte

    Function Values: TStringlist;
  End;

Implementation

{ TMixfile }

Constructor TMixfile.Create(Filename: String);
Begin
  Inherited create;
  fFilename := Filename;
End;

Destructor TMixfile.Destroy;
Begin

End;

Function TMixfile.LoadValue(ValueName: String; Const Data: TStream): Boolean;
Var
  f: TFileStream;
  s: String;
  sz: int64;
Begin
  result := false;
  If Not FileExists(fFilename) Then exit;
  f := TFileStream.Create(fFilename, fmOpenRead);
  While f.Position < f.Size Do Begin
    s := f.ReadAnsiString;
    sz := 0;
    f.Read(sz, sizeof(sz));
    If s = ValueName Then Begin
      data.CopyFrom(f, sz);
      data.Position := data.Position - sz;
      f.free;
      result := true;
      exit;
    End;
    f.Position := f.Position + sz;
  End;
  f.free;
End;

Function TMixfile.AddValue(ValueName: String; Data: TStream): Boolean;
Var
  f: TFileStream;
  m: TMemoryStream;
  sz: int64;
Begin
  result := false;
  sz := Data.Size - data.Position;
  If sz = 0 Then exit; // Der inhalt der Daten ist Leer
  m := TMemoryStream.Create;
  // 1. evtl vorhandene Daten laden / bzw. altes Element löschen
  If FileExists(fFilename) Then Begin
    DeleteValue(ValueName);
    f := TFileStream.Create(fFilename, fmOpenRead);
    m.CopyFrom(f, f.Size);
    f.free;
  End;
  // 2. Anhängen des Elementes
  m.WriteAnsiString(ValueName);
  m.Write(sz, sizeof(sz));
  m.CopyFrom(data, sz);
  m.Position := 0;
  f := TFileStream.Create(fFilename, fmOpenWrite Or fmCreate);
  f.CopyFrom(m, m.Size);
  f.free;
  result := true;
End;

Function TMixfile.DeleteValue(ValueName: String): Boolean;
Var
  f: TFileStream;
  m: TMemoryStream;
  s: String;
  op, sz: int64;
Begin
  result := false;
  If Not FileExists(fFilename) Then exit; // Wo es nichts gibt braucht auch nichts gelöscht werden.
  f := TFileStream.Create(fFilename, fmOpenRead);
  m := TMemoryStream.Create;
  m.CopyFrom(f, f.Size);
  f.free;
  m.Position := 0;
  While m.Position < m.Size Do Begin
    op := m.Position;
    s := m.ReadAnsiString;
    sz := 0;
    m.Read(sz, sizeof(sz));
    If s = ValueName Then Begin
      f := TFileStream.Create(fFilename, fmcreate Or fmOpenWrite);
      m.Position := 0;
      // Kopieren von allem was vor dem Element da war
      If op <> 0 Then Begin
        f.CopyFrom(m, op);
      End;
      // Das zu entfernende Element "überlesen"
      s := m.ReadAnsiString;
      sz := 0;
      m.Read(sz, sizeof(sz));
      m.Position := m.Position + sz;
      // Den Rest danach wieder Rein kopieren
      If (m.Size - m.Position) <> 0 Then Begin
        f.CopyFrom(m, m.Size - m.Position);
      End;
      f.free;
      result := true;
      m.Free;
      exit;
    End;
    m.Position := m.Position + sz;
  End;
  m.Free;
End;

Function TMixfile.Values: TStringlist;
Var
  f: TFileStream;
  sz: int64;
Begin
  result := TStringList.Create;
  If Not FileExists(fFilename) Then exit;
  f := TFileStream.Create(fFilename, fmOpenRead);
  While f.Position < f.Size Do Begin
    result.add(f.ReadAnsiString);
    sz := 0;
    f.Read(sz, sizeof(sz));
    f.Position := f.Position + sz;
  End;
  f.free;
End;

End.

