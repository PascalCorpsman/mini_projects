(******************************************************************************)
(* Highscore Engine                                                ??.??.???? *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Provides a simple highscore engine                           *)
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


Unit uHighscoreEngine;

Interface

Uses
  SysUtils, // inttostr, Fileexists
  Classes // TFilestream
  ;

Const
  HighscoreEngineVersion: byte = 001;

Type
  TItem = Record
    Name: String;
    Points: integer;
  End;

  TItemList = Array Of TItem;

  THighscoreEngine = Class
  private
    FPassword: Array Of byte;
    FPasswordPointer: integer;
    FMaxItems: integer;
    FFilename: String;
    Fdata: TItemlist;
    FChanged: boolean;
    Procedure QuickSortAsc(Var Value: TItemList; li, re: integer);
    Procedure QuickSortDesc(Var Value: TItemList; li, re: integer);
    Procedure WriteString(Const Stream: TStream; Value: String);
    Procedure WriteInteger(Const Stream: TStream; Value: integer);
    Procedure WriteByte(Const Stream: TStream; Value: byte);
    Function ReadString(Const Stream: TStream): String;
    Function ReadInteger(Const Stream: TStream): integer;
    Function ReadByte(Const Stream: TStream): byte;
  public
    InsertAlways: boolean; // Wenn dieser Wert = True ist dann wird ein Neuer Item im zweifel als Letzer eingefügt.

    Property Changed: boolean read FChanged;

    Constructor Create(Filename, Password: String; MaxItems: integer);
    Destructor Destroy; override;

    Procedure LoadFromStream(Const Stream: Tstream);
    Procedure SaveToStream(Const Stream: Tstream);

    Procedure Clear;

    Procedure Save;

    Procedure Add(Name: String; Points: integer);

    Function Show(Ascending: boolean): TItemlist;
  End;

Implementation

{ THighscoreEngine }

Constructor THighscoreEngine.Create(Filename, Password: String; MaxItems: integer);
Var
  f: TFileStream;
  m: TMemoryStream;
  i: integer;
Begin
  Inherited Create;
  InsertAlways := False;
  FMaxItems := MaxItems;
  FFilename := Filename;
  setlength(FPassword, length(Password));
  For i := 1 To Length(Password) Do
    Fpassword[i - 1] := Ord(password[i]);
  Clear;
  FChanged := False;
  If FileExists(Filename) Then Begin
    f := TFileStream.Create(FFilename, fmOpenRead);
    m := TMemoryStream.Create;
    m.CopyFrom(f, f.size);
    f.Free;
    m.position := 0;
    LoadFromStream(m);
    m.Free;
  End;
End;

Destructor THighscoreEngine.Destroy;
Begin
  Clear;
End;

Procedure THighscoreEngine.Add(Name: String; Points: integer);
Begin
  FChanged := True;
  If InsertAlways Then Begin
    If high(Fdata) >= FMaxItems - 1 Then Begin
      QuickSortDesc(fdata, 0, high(fdata));
      If fdata[high(fdata)].Points >= Points Then Begin
        fdata[high(fdata)].Name := Name;
        fdata[high(fdata)].Points := Points;
      End
      Else Begin
        setlength(fdata, high(fdata) + 2);
        fdata[high(fdata)].Name := Name;
        fdata[high(fdata)].Points := Points;
        QuickSortDesc(fdata, 0, high(fdata));
        setlength(fdata, FMaxItems);
      End;
    End
    Else Begin
      setlength(fdata, high(fdata) + 2);
      fdata[high(fdata)].Name := Name;
      fdata[high(fdata)].Points := Points;
    End;
  End
  Else Begin
    setlength(fdata, high(fdata) + 2);
    fdata[high(fdata)].Name := Name;
    fdata[high(fdata)].Points := Points;
    If high(Fdata) >= FMaxItems - 1 Then Begin
      QuickSortDesc(fdata, 0, high(fdata));
      setlength(fdata, FMaxItems);
    End;
  End;
End;

Procedure THighscoreEngine.Clear;
Begin
  SetLength(fdata, 0);
  FChanged := True;
End;

Procedure THighscoreEngine.Save;
Var
  f: Tfilestream;
  m: TMemorystream;
Begin
  m := TMemoryStream.Create;
  SaveToStream(m);
  f := TFileStream.Create(ffilename, fmcreate Or fmopenwrite);
  m.position := 0;
  f.CopyFrom(m, m.size);
  m.Free;
  f.Free;
End;

Function THighscoreEngine.Show(Ascending: boolean): TItemlist;
Var
  i: integer;
Begin
  result := Nil;
  setlength(Result, high(Fdata) + 1);
  For i := 0 To high(Fdata) Do
    Result[i] := Fdata[i];
  If Ascending Then
    QuickSortAsc(Result, 0, high(Result))
  Else
    QuickSortDesc(Result, 0, high(Result));
End;

Procedure THighscoreEngine.LoadFromStream(Const Stream: Tstream);
Var
  dummy: byte;
  i: integer;
Begin
  FChanged := False;
  FPasswordPointer := 0;
  dummy := ReadByte(Stream);
  If dummy <> HighscoreEngineVersion Then
    Raise Exception.Create('Error : Invalid file version.');
  i := ReadInteger(stream);
  setlength(fdata, i);
  For i := 0 To High(Fdata) Do Begin
    Fdata[i].Name := ReadString(Stream);
    Fdata[i].Points := ReadInteger(Stream);
  End;
End;

Procedure THighscoreEngine.SaveToStream(Const Stream: Tstream);
Var
  i: integer;
Begin
  FChanged := False;
  FPasswordPointer := 0;
  WriteByte(stream, HighscoreEngineVersion);
  i := High(Fdata) + 1;
  writeInteger(Stream, i);
  For i := 0 To High(Fdata) Do Begin
    WriteString(Stream, Fdata[i].Name);
    WriteInteger(Stream, Fdata[i].Points);
  End;
End;

Function THighscoreEngine.ReadByte(Const Stream: TStream): byte;
Var
  b: byte;
Begin
  b := 0;
  stream.Read(b, sizeof(b));
  Result := b Xor fpassword[fpasswordpointer];
  fpasswordpointer := (fpasswordpointer + 1) Mod (high(fpassword) + 1);
End;

Function THighscoreEngine.ReadInteger(Const Stream: TStream): integer;
Var
  b: byte;
Begin
  b := ReadByte(Stream);
  Result := b;
  b := ReadByte(Stream);
  Result := Result Or (b Shl 8);
  b := ReadByte(Stream);
  Result := Result Or (b Shl 16);
  b := ReadByte(Stream);
  Result := Result Or (b Shl 24);
End;

Function THighscoreEngine.ReadString(Const Stream: TStream): String;
Var
  i: integer;
  b: byte;
Begin
  result := '';
  i := ReadInteger(stream);
  setlength(Result, i);
  For i := 1 To Length(Result) Do Begin
    b := ReadByte(stream);
    Result[i] := chr(b);
  End;
End;

Procedure THighscoreEngine.WriteByte(Const Stream: TStream; Value: byte);
Var
  b: byte;
Begin
  b := Value Xor fpassword[fpasswordpointer];
  stream.Write(b, sizeof(b));
  fpasswordpointer := (fpasswordpointer + 1) Mod (high(fpassword) + 1);
End;

Procedure THighscoreEngine.WriteInteger(Const Stream: TStream; Value: integer);
Begin
  WriteByte(stream, byte(Value));
  WriteByte(stream, byte(Value Shr 8));
  WriteByte(stream, byte(Value Shr 16));
  WriteByte(stream, byte(Value Shr 24));
End;

Procedure THighscoreEngine.WriteString(Const Stream: TStream; Value: String);
Var
  i: integer;
Begin
  i := length(Value);
  WriteInteger(stream, i);
  For i := 1 To length(Value) Do
    WriteByte(stream, Ord(Value[i]));
End;

Procedure THighscoreEngine.QuickSortAsc(Var Value: TItemList; li, re: integer);
Var
  h: TItem;
  l, r, p: integer;
Begin
  If Li < Re Then Begin
    p := Value[Trunc((li + re) / 2)].Points; // Auslesen des Pivo Elementes
    l := Li;
    r := re;
    While l < r Do Begin
      While Value[l].Points < p Do
        Inc(l);
      While Value[r].Points > p Do
        Dec(r);
      If L <= R Then Begin
        h := Value[l];
        Value[l] := Value[r];
        Value[r] := h;
        Inc(l);
        Dec(r);
      End;
    End;
    QuickSortAsc(Value, li, r);
    QuickSortAsc(Value, l, re);
  End;
End;

Procedure THighscoreEngine.QuickSortDesc(Var Value: TItemList; li, re: integer);
Var
  h: TItem;
  l, r, p: integer;
Begin
  If Li < Re Then Begin
    p := Value[Trunc((li + re) / 2)].Points; // Auslesen des Pivo Elementes
    l := Li;
    r := re;
    While l < r Do Begin
      While Value[l].Points > p Do
        Inc(l);
      While Value[r].Points < p Do
        Dec(r);
      If L <= R Then Begin
        h := Value[l];
        Value[l] := Value[r];
        Value[r] := h;
        Inc(l);
        Dec(r);
      End;
    End;
    QuickSortDesc(Value, li, r);
    QuickSortDesc(Value, l, re);
  End;
End;

End.

