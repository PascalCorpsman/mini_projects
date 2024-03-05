(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Bitverknuepfungen                                     *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit2;

{$MODE ObjFPC}{$H+}

Interface

Uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

Type
  TForm2 = Class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Button2: TButton;
    SaveDialog1: TSaveDialog;
    Procedure Button2Click(Sender: TObject);
    Procedure Button1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

Var
  Form2: TForm2;

Implementation

Uses Unit1;

Var
  Names: Array Of String;

{$R *.lfm}

Function SaveinLimboole(Formel: String): String;
  Function Reallykomming(Fml: String; y: integer; V1: String): Boolean;
  Var
    x: Integer;
  Begin
    Result := true;
    fml := lowercase(Fml);
    v1 := lowercase(V1);
    If y > 1 Then
      If (fml[y - 1] = '~') And (Fml[y] = v1[1]) Then result := false;
    If Fml[y] <> v1[1] Then result := false;
    If Result Then Begin
      For x := y To y + length(V1) - 1 Do
        If Fml[x] <> v1[x - Y + 1] Then Begin
          result := false;
          break;
        End;
    End;
  End;

Type
  TMy = Record
    Old, neu: String;
  End;

Label
  noamol, bloed;
Var
  z, y, x: Integer;
  f: Array Of Tmy;
  bo: String;

Begin
  x := 1;
  // Einfügen der Entsprechenden Syntax Ende
  Formel := lowercase(Formel);
  While x <= length(Formel) Do Begin
    If Formel[x] = '}' Then Begin // Einfügen des Xor
      Formel[x] := '!';
      insert(' ', Formel, x);
      insert('>', Formel, x);
      insert('-', Formel, x);
      insert('<', Formel, x);
      insert(' ', Formel, x);
    End;
    If Formel[x] = '/' Then Formel[x] := '&'; // Einfügen Und
    If Formel[x] = '\' Then Formel[x] := '|'; // Einfügen oder
    If Formel[x] = '[' Then Begin // Einfügen Impliziert
      Formel[x] := ' ';
      insert('>', Formel, x);
      insert('-', Formel, x);
      insert(' ', Formel, x);
    End;
    If Formel[x] = '{' Then Begin // Einfügen Äquivalenz
      Formel[x] := ' ';
      insert('>', Formel, x);
      insert('-', Formel, x);
      insert('<', Formel, x);
      insert(' ', Formel, x);
    End;
    inc(x);
  End;
  // Einfügen der Entsprechenden Syntax Ende
  // Sortieren der Neuen Variablen nach länge und dann ersetzen
  setlength(f, high(Variablen) + 1);
  For x := 0 To high(Variablen) Do Begin
    f[x].Old := lowercase(Variablen[x].Name);
    f[x].neu := lowercase(Names[x]);
  End;
  For x := high(f) Downto 1 Do
    For y := 1 To X Do
      If Length(f[y].old) > length(f[y - 1].old) Then Begin
        bo := f[y].old;
        f[y].old := f[y - 1].old;
        f[y - 1].old := bo;
        bo := f[y].Neu;
        f[y].neu := f[y - 1].neu;
        f[y - 1].Neu := bo;
      End;
  // Umbenennen der Variablen
  //Ersetzen der Namen in Formel von F.old nach F neu und speichern in Result !!!!!
  Result := '';
  For x := 0 To high(F) Do
    noamol:
    For y := 1 To length(Formel) - length(f[x].old) + 1 Do
      If Reallykomming(Formel, y, f[x].old) Then Begin
        bo := '~' + inttostr(X) + '~';
        For z := y To y + length(f[x].old) - 1 Do
          Formel[Z] := ' ';
        insert(bo, Formel, y);
        Goto noamol;
      End;
  // löschen der ganzen Nullen
  While (pos(' ', Formel) <> 0) Do
    delete(Formel, pos(' ', Formel), 1);
  // Aus '~X~' wird nun die entsprechende neue Variable
  For x := 0 To High(f) Do
    While (pos('~' + inttostr(x) + '~', Formel) <> 0) Do Begin
      y := pos('~' + inttostr(x) + '~', Formel);
      delete(Formel, y, length('~' + inttostr(x) + '~'));
      insert(f[x].neu, Formel, y);
    End;
  // löschen der ganzen Nullen
  While (pos(' ', Formel) <> 0) Do
    delete(Formel, pos(' ', Formel), 1);
  // nun müssen nur noch Implikation und Äquivalenz wieder in Leerzeichen gehüllt werden und Fertig !!
  bloed:
  For x := 1 To length(Formel) Do Begin
    If Formel[x] = '<' Then
      If x > 1 Then Begin
        If (Formel[x - 1] <> ' ') Then Begin
          insert(' ', Formel, x);
          Goto bloed;
        End;
      End
      Else Begin
        insert(' ', Formel, x);
        Goto bloed;
      End;
    If Formel[x] = '>' Then
      If Formel[x + 1] <> ' ' Then Begin
        insert(' ', Formel, x + 1);
        Goto bloed;
      End;
    If Formel[x] = '-' Then
      If x > 1 Then Begin
        If (Formel[x - 1] <> ' ') And (Formel[x - 1] <> '<') Then Begin
          insert(' ', Formel, x);
          Goto bloed;
        End;
      End
      Else Begin
        insert(' ', Formel, x);
        Goto bloed;
      End;
  End;
  Result := uppercase(Formel);
End;

Procedure GetnewVarnames;
Var
  x: Integer;
  s: String;
Begin
  setlength(Names, high(Variablen) + 1);
  For x := 0 To form2.memo1.lines.count - 1 Do Begin
    s := form2.Memo1.lines[x];
    Names[X] := copy(s, pos('>', s) + 1, length(s));
  End;
End;

Procedure TForm2.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm2.Button1Click(Sender: TObject);
Var
  f: textfile;
  s: String;
  x: Integer;
Begin
  If Memo1.lines.count <> high(Variablen) + 1 Then Begin
    application.Messagebox('The var count is different to the var count in the formel', 'Error', MB_ICONWARNING);
    exit;
  End;
  For x := 0 To memo1.lines.count - 1 Do
    If Pos('>', Memo1.lines[x]) = 0 Then Begin
      application.Messagebox('There are some var''s unidentyfieed !', 'Error', MB_ICONWARNING);
      exit;
    End;
  If savedialog1.execute Then Begin
    s := ClearFormel(form1.Combobox1.text);
    GetnewVarnames;
    s := SaveinLimboole(s);
    assignfile(f, savedialog1.filename);
    rewrite(f);
    writeln(f, s);
    closefile(f);
  End;
End;

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  savedialog1.InitialDir := extractfilepath(application.exename);
End;

End.

