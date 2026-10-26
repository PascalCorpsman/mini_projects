Unit highscoreengine;

{$MODE ObjFPC}{$H+}

Interface

Uses classes, sysutils;

Type
  Thighscore = Class
  protected
    data: Tstringlist;
    Sourcepath: String;
    max: integer;
    Function EnCode(s: String; m: Integer): String;
    Function DeCode(s: String; m: Integer): String;
    Function checkdata(item: String): boolean;
    Function extractinteger(data_: String): integer;
    Function shifre(s: String): String;
    Function deshifre(s: String): String;
    Procedure sortieren;
  public
    Constructor create(source: String; Maxitems: integer);
    Function add(Item: String): boolean;
    Function info: String;
    Procedure save;
    Procedure clear;
    Function show: TStrings;
    Function support: String;
    Function Whattodo: String;
    Destructor free;
  End;

Implementation

Constructor Thighscore.create(source: String; Maxitems: integer);
Var
  f: Textfile;
  bet: String;
  x: integer;
Begin
  sourcepath := source;
  max := maxitems;
  data := tstringlist.create;
  If fileexists(source) Then Begin
    assignfile(f, source);
    reset(f);
    x := 0;
    While Not eof(f) Do Begin
      readln(f, bet);
      data.Add(deshifre(decode(bet, 12 + (X + 3))));
      inc(x);
    End;
    closefile(f);
  End
  Else
    data.clear;
End;

Function Thighscore.deshifre(s: String): String;
Var
  x: integer;
Begin
  result := '';
  For x := 1 To length(s) Do
    Result := result + encode(s[x], (x + 2) * (x + 2));
End;

Function Thighscore.shifre(s: String): String;
Var
  x: integer;
Begin
  result := '';
  For x := 1 To length(s) Do
    Result := result + decode(s[x], (x + 2) * (x + 2));
End;

Function Thighscore.EnCode(s: String; m: Integer): String;
Var
  str: String;
  i: Integer;
Begin
  Str := '';
  For i := 1 To Length(s) Do
    Insert(char(ord(s[i]) + m), str, Length(Str) + 1);
  Result := Str;
End;

Function Thighscore.DeCode(s: String; m: Integer): String;
Var
  str: String;
  i: Integer;
Begin
  Str := '';
  For i := 1 To Length(s) Do
    Insert(char(ord(s[i]) - m), str, Length(Str) + 1);
  Result := Str;
End;

Procedure Thighscore.save;
Var
  f: Textfile;
  x: integer;
Begin
  assignfile(f, sourcepath);
  rewrite(f);
  For x := 0 To data.count - 1 Do
    writeln(f, shifre(encode(data[x], 12 + (x + 3))));
  closefile(f);
End;

Procedure Thighscore.clear;
Begin
  data.clear;
End;

Function Thighscore.add(Item: String): boolean;
Begin
  Result := false;
  If checkdata(item) Then Begin
    Data.Add(item);
    If data.count >= 2 Then sortieren;
    result := true;
  End;
End;

Function Thighscore.extractinteger(data_: String): integer;
Var
  x: integer;
  bet: String;
Begin
  result := 0;
  For x := length(data_) Downto 1 Do
    If Data_[x] = ' ' Then Begin
      bet := copy(data_, x + 1, Length(data_));
      result := strtoint(bet);
      break;
    End;
End;

Procedure thighscore.sortieren;
Var
  Bet: String;
  Z1, Z2: integer;
Begin
  For Z1 := data.count - 1 Downto 1 Do Begin
    For z2 := 1 To z1 Do Begin
      If extractinteger(data[z2]) > extractinteger(data[z2 - 1]) Then Begin
        bet := data[z2 - 1];
        data[z2 - 1] := data[z2];
        data[z2] := bet;
      End;
    End;
  End;
  While data.count > max Do
    data.Delete(data.count - 1);
End;

Function Thighscore.show: Tstrings;
Begin
  Result := data;
End;

Function thighscore.checkdata(item: String): boolean;
Var
  bet: String;
  i: integer;
Begin
  result := true;
  For i := length(item) Downto 1 Do
    If item[i] = ' ' Then Begin
      bet := copy(item, i + 1, length(item));
      break;
    End;
  Try
    strtoint(bet);
  Except Result := false;
  End;
End;

Function Thighscore.support: String;
Begin
  result := 'Support: www.Corpsman.de.vu';
End;

Function Thighscore.Whattodo: String;
Begin
  result := 'Die Engine verlangt einen String+leerzeichen+Punktzahl' + #13 + 'Achtung bei destroy wird nicht gespeichert!';
End;

Function Thighscore.info: String;
Begin
  result := 'Highscoresortengine ver 1.2 by Corpsman | Targetsoft |';
End;

Destructor Thighscore.free;
Begin
  data.Destroy;
End;

End.
