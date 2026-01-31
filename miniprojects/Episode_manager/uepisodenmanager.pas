(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Episode manager                                       *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit uepisodenmanager;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils;

Const
  ClientID = 'EpisodenManager';

  // Trenner = chr(195) + chr(166); das "æ" zeichen
  Trenner = '~';
  PrettyTrenner = '|';

Type

  TDatensatz = Record
    Gesehen: Boolean;
    Serie: String;
    Staffel: String;
    Episodenname: String;
  End;

  { TDatabase }

  TDatabase = Class
  private
    Fdata: Array Of TDatensatz;
    Fchanged: Boolean;
  public
    Constructor create;
    Destructor Destroy; override;

    Function SavetoFile(filename: String): Boolean;
    Function LoadFromFile(filename: String): Boolean;
    Function LoadFromStream(Const aStream: TStream): Boolean;

    Function Changed: Boolean;
    Function GebeAlleSerien: Tstringlist;
    Function GebeAlleStaffeln: Tstringlist;
    Function GebeAlleDatensaetzeMit(Serie: String; Staffel: String; IgnoriereGesehen: Boolean): Tstringlist;
    Procedure Sort;
    Procedure Clear;
    Procedure AddItem(Serie: String; Staffel: String; Episodenname: String);
    Procedure MarkWithValue(Const List: TStringlist; Value: Boolean);
    Procedure MergeOtherIn(Const other: TDatabase);

    Class Function Pretty(Value: TDatensatz): String;
    Class Function PrettyToDatensatz(Value: String): TDatensatz;
    Class Function Encode(Value: TDatensatz): String;
    Class Function Decode(Value: String): TDatensatz;

    Function ReplaceWith(Const aSource, aDest: TDatensatz): Boolean;
    Function DeleteDataset(Const aDataset: TDatensatz): Boolean;

  End;

Implementation

Uses
  dialogs, Controls
  , unit4
  ;

Operator = (Const a, b: TDatensatz): Boolean;
Begin
  result :=
    (a.Serie = b.Serie)
    And (a.Staffel = b.Staffel)
    And (a.Episodenname = b.Episodenname);
  // Gesehen wird ignoriert
End;


{ TDatabase }

Procedure TDatabase.AddItem(Serie: String; Staffel: String; Episodenname: String
  );
Begin
  Fchanged := true;
  setlength(Fdata, high(Fdata) + 2);
  Fdata[high(Fdata)].Gesehen := false;
  Fdata[high(Fdata)].Serie := serie;
  Fdata[high(Fdata)].Staffel := Staffel;
  Fdata[high(Fdata)].Episodenname := Episodenname;
  sort;
End;

Constructor TDatabase.create;
Begin
  Inherited;
  Fchanged := false;
  setlength(Fdata, 0);
End;

Destructor TDatabase.Destroy;
Begin
  setlength(Fdata, 0);
End;

Function TDatabase.Changed: Boolean;
Begin
  result := Fchanged;
End;

Procedure TDatabase.Clear;
Begin
  setlength(Fdata, 0);
End;

Class Function TDatabase.Decode(Value: String): TDatensatz;
Begin
  // Ein Typischer Datensatz
  // Serie Trenner Staffel Trenner Episodenname Trenner 1
  result.Serie := copy(Value, 1, pos(Trenner, value) - 1);
  Delete(Value, 1, pos(Trenner, value) + Length(Trenner) - 1);
  result.Staffel := copy(Value, 1, pos(Trenner, value) - 1);
  Delete(Value, 1, pos(Trenner, value) + Length(Trenner) - 1);
  result.Episodenname := copy(Value, 1, pos(Trenner, value) - 1);
  Delete(Value, 1, pos(Trenner, value) + Length(Trenner) - 1);
  If Length(value) = 1 Then
    result.Gesehen := odd(strtoint(Value))
  Else
    result.Gesehen := false;
End;

Class Function TDatabase.Encode(Value: TDatensatz): String;
Begin
  result := Value.serie + Trenner + Value.Staffel + Trenner + value.Episodenname + Trenner + inttostr(ord(value.Gesehen));
End;

Function TDatabase.LoadFromFile(filename: String): Boolean;
Var
  m: TMemoryStream;
Begin
  If FileExists(Filename) Then Begin
    m := TMemoryStream.Create;
    m.LoadFromFile(filename);
    result := LoadFromStream(m);
    m.free;
  End
  Else
    Result := false;
End;

Function TDatabase.LoadFromStream(Const aStream: TStream): Boolean;
Var
  l: Tstringlist;
  i: Integer;
Begin
  Fchanged := false;
  l := TStringlist.create;
  result := True;
  Try
    l.LoadFromStream(aStream);
    setlength(Fdata, l.count);
    For i := 0 To l.count - 1 Do
      If Length(l[i]) <> 0 Then
        Fdata[i] := Decode(l[i]);
  Except
    result := false;
    Fchanged := true;
  End;
  l.free;
End;

Function TDatabase.SavetoFile(filename: String): Boolean;
Var
  f: Textfile;
  i: Integer;
Begin
  Fchanged := false;
  Try
    assignfile(f, filename);
    rewrite(f);
    For i := 0 To High(Fdata) Do
      writeln(f, encode(Fdata[i]));
    closefile(f);
    result := true;
  Except
    result := false;
    Fchanged := true;
  End;
End;

Procedure TDatabase.Sort;
  Procedure Quick(li, re: integer);
  Var
    l, r, p: Integer;
    h: TDatensatz;
  Begin
    If Li < Re Then Begin
      p := {Data[} Trunc((li + re) / 2) {]}; // Auslesen des Pivo Elementes
      l := Li;
      r := re;
      While l < r Do Begin
        While CompareStr(encode(Fdata[l]), encode(Fdata[p])) < 0 Do
          //        While Fdata[l] < p Do
          inc(l);
        While CompareStr(encode(Fdata[r]), encode(Fdata[p])) > 0 Do
          //        While Fdata[r] > p Do
          dec(r);
        If L <= R Then Begin
          h := Fdata[l];
          fdata[l] := FData[r];
          fdata[r] := h;
          inc(l);
          dec(r);
        End;
      End;
      quick(li, r);
      quick(l, re);
    End;
  End;
Begin
  Quick(0, high(Fdata));
End;

Function TDatabase.GebeAlleSerien: Tstringlist;
Var
  i: Integer;
Begin
  result := TStringList.create;
  result.Duplicates := dupIgnore;
  result.Sorted := true;
  For i := 0 To High(fdata) Do
    result.add(fdata[i].Serie);
End;

Function TDatabase.GebeAlleStaffeln: Tstringlist;
Var
  i: Integer;
Begin
  result := TStringList.create;
  result.Duplicates := dupIgnore;
  result.Sorted := true;
  For i := 0 To High(fdata) Do
    result.add(fdata[i].Staffel);
End;

Function TDatabase.GebeAlleDatensaetzeMit(Serie: String; Staffel: String;
  IgnoriereGesehen: Boolean): Tstringlist;
Var
  i: Integer;
Begin
  result := TStringList.create;
  For i := 0 To High(Fdata) Do
    If (Fdata[i].Serie = Serie) And (Fdata[i].Staffel = Staffel) Then Begin
      If IgnoriereGesehen Then
        result.add(pretty(Fdata[i]))
      Else If Not Fdata[i].Gesehen Then
        result.add(pretty(Fdata[i]));
    End;
End;

Class Function TDatabase.Pretty(Value: TDatensatz): String;
Begin
  If Value.Gesehen Then
    result := 'Seen = yes ' + PrettyTrenner + ' '
  Else
    result := 'Seen =  no ' + PrettyTrenner + ' ';
  result := result + 'Series : ' + Value.Serie +
    ' ' + PrettyTrenner + ' Season : ' + Value.Staffel +
    ' ' + PrettyTrenner + ' Episode : ' + Value.Episodenname;
End;

Class Function TDatabase.PrettyToDatensatz(Value: String): TDatensatz;
Var
  s: String;
Begin
  s := Copy(value, 1, pos(PrettyTrenner, value));
  delete(value, 1, pos(PrettyTrenner, value) + 1);
  result.Gesehen := pos('yes', s) <> 0;
  s := Copy(value, 1, pos(PrettyTrenner, value) - 2);
  delete(value, 1, pos(PrettyTrenner, value) + 1);
  result.Serie := copy(s, pos(':', s) + 2, length(s));
  s := Copy(value, 1, pos(PrettyTrenner, value) - 2);
  delete(value, 1, pos(PrettyTrenner, value) + 1);
  result.Staffel := copy(s, pos(':', s) + 2, length(s));
  s := value;
  result.Episodenname := copy(s, pos(':', s) + 2, length(s));
End;

Function TDatabase.ReplaceWith(Const aSource, aDest: TDatensatz): Boolean;
Var
  i: Integer;
Begin
  result := false;
  For i := 0 To high(Fdata) Do Begin
    If (Fdata[i] = aSource)
      // and (Fdata[i].Gesehen = aSource.Gesehen) -- Braucht es eigentlich nicht
    Then Begin
      Fdata[i] := aDest;
      Fchanged := true;
      result := true;
      exit;
    End;
  End;
End;

Function TDatabase.DeleteDataset(Const aDataset: TDatensatz): Boolean;
Var
  i, j: Integer;
Begin
  result := false;
  For i := 0 To high(Fdata) Do Begin
    If Fdata[i] = aDataset Then Begin
      For j := i To high(Fdata) - 1 Do Begin
        Fdata[j] := Fdata[j + 1];
      End;
      setlength(Fdata, high(Fdata));
      Fchanged := true;
      result := true;
      exit;
    End;
  End;
End;

Procedure TDatabase.MarkWithValue(Const List: TStringlist; Value: Boolean);
Var
  a, i: Integer;
  dt: TDatensatz;
Begin
  If list.count <> 0 Then Begin
    a := 0;
    dt := PrettyToDatensatz(list[a]);
    For i := 0 To High(fdata) Do
      If (Fdata[i].Serie = dt.Serie) And
        (Fdata[i].Staffel = dt.Staffel) And
        (Fdata[i].Episodenname = dt.Episodenname) Then Begin
        Fchanged := True;
        Fdata[i].Gesehen := Value;
        inc(a);
        If a = list.count Then
          exit
        Else Begin
          dt := PrettyToDatensatz(list[a]);
        End;
      End;
  End;
End;

Procedure TDatabase.MergeOtherIn(Const other: TDatabase);
Var
  a, b, i: Integer;
  sb, sa: String;
  d: TDatensatz;
Begin
  sort;
  other.Sort;
  form4.Clear;
  a := 0;
  b := 0;
  While (a < high(Fdata)) And (b < high(other.Fdata)) Do Begin
    // Der Angenehm Einfache Fall ;)
    If Fdata[a] = other.Fdata[b] Then Begin
      inc(a);
      inc(b);
    End
    Else Begin
      sa := Encode(Fdata[a]);
      sb := Encode(other.Fdata[b]);
      If CompareStr(sa, sb) < 0 Then Begin
        form4.Insert(sa, '');
        inc(a);
      End
      Else Begin
        form4.Insert('', sb);
        inc(b);
      End;
    End;
  End;
  While (a < high(Fdata)) Do Begin
    sa := Encode(Fdata[a]);
    form4.Insert(sa, '');
    inc(a);
  End;
  While (b < high(other.Fdata)) Do Begin
    sb := Encode(other.Fdata[b]);
    form4.Insert('', sb);
    inc(b);
  End;
  If form4.StringGrid1.RowCount = 1 Then Begin
    showmessage('DB''s are in sync.');
    exit;
  End;
  form4.StringGrid1.AutoSizeColumns;
  If form4.ShowModal = mrOK Then Begin
    For i := 1 To form4.StringGrid1.RowCount - 1 Do Begin
      If form4.aDirs[i] <> IndexNothing Then Begin
        If form4.StringGrid1.Cells[0, i] = '' Then Begin
          // Der Rechte Datensatz wird hinzugefügt
          d := TDatabase.PrettyToDatensatz(form4.StringGrid1.Cells[2, i]);
          AddItem(d.Serie, d.Staffel, d.Episodenname);
        End
        Else Begin
          // Der Linke Datensatz wird gelöscht
          d := TDatabase.PrettyToDatensatz(form4.StringGrid1.Cells[0, i]);
          DeleteDataset(d);
        End;
      End;
    End;
  End;
End;

End.

