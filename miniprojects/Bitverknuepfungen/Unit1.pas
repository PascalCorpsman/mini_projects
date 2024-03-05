(******************************************************************************)
(* Bitverknuepfungen                                               ??.??.???? *)
(*                                                                            *)
(* Version     : 1.07                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Evaluates arithmetic formulas                                *)
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
(*               0.02 - 1.07 - Unknown                                        *)
(*                                                                            *)
(******************************************************************************)

Unit Unit1;

{$MODE ObjFPC}{$H+}

Interface

Uses
  LCLIntf, LCLType, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Grids, Menus;

Type
  TForm1 = Class(TForm)
    GroupBox1: TGroupBox;
    Button1: TButton;
    ComboBox1: TComboBox;
    Button2: TButton;
    PopupMenu1: TPopupMenu;
    Save1: TMenuItem;
    Load1: TMenuItem;
    OpenDialog1: TOpenDialog;
    SaveDialog1: TSaveDialog;
    ExtracttoLimboole1: TMenuItem;
    ShowOnlyResult11: TMenuItem;
    ShowonlyResult01: TMenuItem;
    SavewithoutValues1: TMenuItem;
    Colourize1: TMenuItem;
    StringGrid1: TStringGrid;
    Resortlist1: TMenuItem;
    ImportfromLimboole1: TMenuItem;
    Support1: TMenuItem;
    Help1: TMenuItem;
    PopupMenu2: TPopupMenu;
    GetonlyResult11: TMenuItem;
    GetonlyResult01: TMenuItem;
    OpenDialog2: TOpenDialog;
    MakeKVDiagram1: TMenuItem;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure FormResize(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormPaint(Sender: TObject);
    Procedure ComboBox1KeyPress(Sender: TObject; Var Key: Char);
    Procedure Load1Click(Sender: TObject);
    Procedure Save1Click(Sender: TObject);
    Procedure ExtracttoLimboole1Click(Sender: TObject);
    Procedure ShowOnlyResult11Click(Sender: TObject);
    Procedure ShowonlyResult01Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure SavewithoutValues1Click(Sender: TObject);
    Procedure Colourize1Click(Sender: TObject);
    Procedure StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    Procedure StringGrid1SelectCell(Sender: TObject; ACol, ARow: Integer;
      Var CanSelect: Boolean);
    Procedure Resortlist1Click(Sender: TObject);
    Procedure ImportfromLimboole1Click(Sender: TObject);
    Procedure Support1Click(Sender: TObject);
    Procedure Help1Click(Sender: TObject);
    Procedure GetonlyResult11Click(Sender: TObject);
    Procedure GetonlyResult01Click(Sender: TObject);
    Procedure MakeKVDiagram1Click(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  End;

  TVar = Record
    Name: String;
    Value: Boolean;
  End;

Const
  ver = ' 1.07 ';

Var
  Form1: TForm1;
  Variablen: Array Of TVar;
  SRow, Rowc: integer;
  Abbrechen, first: Boolean;

Function ClearFormel(Formel: String): String;
Function informel(f: String): boolean;

Implementation

Uses Unit2, Unit3, Unit4;

{$R *.lfm}

Function pot(Basis, Exponent: Integer): integer;
Var
  x: Integer;
Begin
  result := 1;
  If Exponent <> 0 Then
    For x := 1 To Exponent Do
      Result := Result * Basis;
End;

Function informel(f: String): boolean;
Var
  x: integer;
Begin
  result := false;
  For x := 1 To length(f) Do
    If f[x] In ['a'..'z', 'A'..'Z', '0'..'9'] Then Begin
      result := true;
      break;
    End;
End;

Procedure stepform3;
Begin
  If Not (Assigned(Form3)) Then exit;
  form3.ProgressBar1.Position := form3.ProgressBar1.Position + 1;
End;

Procedure Posform3;
Begin
  If Not (Assigned(Form3)) Then exit;
  Form3.top := Form1.top + Form1.button1.height + 71;
  Form3.left := form1.left + Form1.combobox1.width + 21;
  form3.ProgressBar1.Max := Pot(2, high(Variablen) + 1) Div 10;
End;

Procedure initForm3;
Var
  x: integer;
Begin
  For x := 0 To form1.PopupMenu1.Items.count - 1 Do
    form1.PopupMenu1.Items[x].enabled := false;
  If Not (Assigned(Form3)) Then exit;
  Abbrechen := false;
  PosForm3;
  form3.ProgressBar1.Position := 0;
  abbrechen := false;
  form1.Button2.caption := '&Cancel';
  Form3.Show;
End;

Procedure DisinitForm3;
Var
  x: Integer;
Begin
  For x := 0 To form1.PopupMenu1.Items.count - 1 Do
    form1.PopupMenu1.Items[x].enabled := True;

  If Not (Assigned(Form3)) Then exit;
  form3.close;
  form1.Button2.caption := '&Close';
End;

Function GetFormelResult(Formel: String): String;
  Function Parseinner(Value: String): char;
  Label
    Zurueck;
  Var
    c, b: char;
    x: integer;
  Begin
    zurueck:
    // Negation
    If pos('!', Value) <> 0 Then
      For x := length(value) Downto 1 Do
        If Value[x] = '!' Then Begin
          b := value[x + 1];
          If B = '0' Then
            b := '1'
          Else
            b := '0';
          delete(Value, x, 1);
          Value[x] := b;
          Goto zurueck;
        End;
    // Negation
    // und operatoren
    If pos('/', Value) <> 0 Then Begin
      x := pos('/', Value);
      b := value[x + 1];
      c := value[x - 1];
      If (B = '1') And (c = '1') Then
        b := '1'
      Else
        b := '0';
      delete(Value, x - 1, 2);
      Value[x - 1] := b;
      Goto zurueck;
    End;
    // und operatoren
    // oder operatoren
    If pos('\', Value) <> 0 Then Begin
      x := pos('\', Value);
      b := value[x + 1];
      c := value[x - 1];
      If (B = '0') And (c = '0') Then
        b := '0'
      Else
        b := '1';
      delete(Value, x - 1, 2);
      Value[x - 1] := b;
      Goto zurueck;
    End;
    // oder operatoren
    // Antivalenzoperationen
    If pos('}', Value) <> 0 Then Begin
      x := pos('}', Value);
      b := value[x + 1];
      c := value[x - 1];
      If ((B = '0') And (c = '1')) Or ((B = '1') And (c = '0')) Then
        b := '1'
      Else
        b := '0';
      delete(Value, x - 1, 2);
      Value[x - 1] := b;
      Goto zurueck;
    End;
    // Implikation
    If pos('[', Value) <> 0 Then Begin
      x := pos('[', Value);
      b := value[x + 1];
      c := value[x - 1];
      If (C = '1') And (b = '0') Then
        b := '0'
      Else
        b := '1';
      delete(Value, x - 1, 2);
      Value[x - 1] := b;
      Goto zurueck;
    End;
    // Äquivalenz
    If pos('{', Value) <> 0 Then Begin
      x := pos('{', Value);
      b := value[x + 1];
      c := value[x - 1];
      If ((B = '0') And (c = '0')) Or ((B = '1') And (c = '1')) Then
        b := '1'
      Else
        b := '0';
      delete(Value, x - 1, 2);
      Value[x - 1] := b;
      Goto zurueck;
    End;
    result := value[1];
  End;

  Function getvarvalues(Value: String): String;
    Function GetVar(V: String): Char;
    Var
      y: integer;
      s: String;
    Begin
      s := '';
      For y := 0 To high(variablen) Do
        If Comparestr(lowercase(v), lowercase(Variablen[y].name)) = 0 Then Begin
          s := inttostr(ord(Variablen[y].Value));
          break;
        End;
      result := s[1];
    End;

  Var
    f: Array Of String;
    z, x, y: Integer;
    b: String;
  Begin
    Value := lowercase(Value);
    setlength(f, high(Variablen) + 1);
    For x := 0 To high(Variablen) Do
      f[x] := lowercase(Variablen[x].Name);
    For x := high(f) Downto 1 Do
      For y := 1 To X Do
        If Length(f[y]) > length(f[y - 1]) Then Begin
          b := f[y];
          f[y] := f[y - 1];
          f[y - 1] := b;
        End;
    For x := 0 To high(f) Do
      While pos(f[x], Value) <> 0 Do Begin
        If Length(f[x]) = 1 Then Begin
          Value[pos(f[x], Value)] := GetVar(f[x]);
        End
        Else Begin
          z := pos(f[x], Value);
          Value[z] := GetVar(f[x]);
          For y := 1 To length(f[x]) - 1 Do
            Value[z + y] := ' ';
        End;
      End;
    While pos(' ', Value) <> 0 Do
      Delete(Value, pos(' ', Value), 1);
    result := value;
  End;
Var
  x: Integer;
  s: String;
Begin
  formel := Getvarvalues(Formel);
  // Lösen der Klammern von innen nach außen
  While pos(')', formel) <> 0 Do
    For x := pos(')', formel) Downto 1 Do
      If Formel[x] = '(' Then Begin
        s := copy(Formel, x + 1, pos(')', formel) - 1 - x);
        Delete(Formel, x + 1, length(s) + 1);
        formel[x] := Parseinner(s);
        break;
      End;
  // Lösen der Klammern von innen nach außen
  // Ausgabe des Ergebnisses
  Result := Parseinner(Formel);
End;

Function ClearFormel(Formel: String): String;
Var
  x: integer;
Begin
  // Not --> !
  While Pos('not', lowercase(Formel)) <> 0 Do Begin
    x := Pos('not', lowercase(Formel));
    Formel[x] := '!';
    Formel[x + 1] := ' ';
    Formel[x + 2] := ' ';
  End;
  // AND --> /
  While Pos('and', lowercase(Formel)) <> 0 Do Begin
    x := Pos('and', lowercase(Formel));
    Formel[x] := '/';
    Formel[x + 1] := ' ';
    Formel[x + 2] := ' ';
  End;
  // xor --> }
  While Pos('xor', lowercase(Formel)) <> 0 Do Begin
    x := Pos('xor', lowercase(Formel));
    Formel[x] := '}';
    Formel[x + 1] := ' ';
    Formel[x + 2] := ' ';
  End;
  // or --> \
  While Pos('or', lowercase(Formel)) <> 0 Do Begin
    x := Pos('or', lowercase(Formel));
    Formel[x] := '\';
    Formel[x + 1] := ' ';
  End;
  // <-> --> {
  While Pos('<->', lowercase(Formel)) <> 0 Do Begin
    x := Pos('<->', lowercase(Formel));
    Formel[x] := '{';
    Formel[x + 1] := ' ';
    Formel[x + 2] := ' ';
  End;
  // -> --> [
  While Pos('->', lowercase(Formel)) <> 0 Do Begin
    x := Pos('->', lowercase(Formel));
    Formel[x] := '[';
    Formel[x + 1] := ' ';
  End;
  // Entfernen aller Leerstellen
  While Pos(' ', lowercase(Formel)) <> 0 Do
    delete(Formel, Pos(' ', lowercase(Formel)), 1);
  Result := Formel;
  // ------------------------------------------ Formel Bereinigen
End;

Procedure GetVarpara(Formel: String);
  Function min(V1, V2: Integer): Integer;
  Begin
    result := V1;
    If V2 < result Then Result := V2;
    If V1 = 0 Then Result := V2;
    If V2 = 0 Then
      Result := V1
  End;

  Function findlowest(F: String): Integer;
    // Der Ausgeklammerte Teil war zu unflexibel der neue sollte
    // besser Funktionieren und Bugs verhindern.
  Var //r:Array [0..5]of Integer;
    x: integer;
  Begin
    Result := -1;
    delete(F, 1, 1);
    For x := 1 To length(f) Do
      If Not (f[x] In ['a'..'z', '0'..'9', 'A'..'Z']) Then Begin
        Result := x + 1;
        break;
      End;
    (* r[0]:=pos('/',lowercase(F));
     r[1]:=pos('\',Lowercase(F));
     r[2]:=pos('[',lowercase(F));
     r[3]:=pos('}',lowercase(F));
     r[4]:=pos(')',lowercase(F));
     r[5]:=pos('{',lowercase(F));
     Result:=min(r[0],r[1]);
     Result:=min(result,r[2]);
     Result:=min(result,r[3]);
     Result:=min(result,r[4]);
     Result:=min(result,r[5]);
     inc(Result);*)
  End;
Label
  jupi;
Var
  x, y: Integer;
  b: Tstringlist;
  s: String;
Begin
  Formel := Formel + '/'; // bug behebung
  b := Tstringlist.create;
  b.Sorted := true;
  b.Duplicates := dupIgnore;
  x := -1;
  y := 1;
  While inFormel(Formel) Do Begin
    If (formel[y] In ['a'..'z', 'A'..'Z', '0'..'9']) And (x = -1) Then x := y;
    inc(y);
    If X <> -1 Then Begin
      y := findlowest(Formel);
      If y > X Then Begin
        s := copy(Formel, x, y - x);
        b.add(s);
      End;
      delete(Formel, 1, y);
      x := -1;
      y := 1;
    End;
  End;
  jupi:
  For x := 0 To b.count - 1 Do
    If (b[x] = '1') Or (b[x] = '0') Then Begin
      b.delete(x);
      Goto jupi;
    End;
  Setlength(Variablen, b.count);
  For x := 0 To b.count - 1 Do Begin
    Variablen[b.count - 1 - x].Name := b[x];
    Variablen[b.count - 1 - x].Value := false;
  End;
  b.Free;
  Rowc := 0;
End;

Function AllVartrue: Boolean;
Var
  x: Integer;
Begin
  Result := true;
  For x := 0 To high(Variablen) Do
    If Not Variablen[x].Value Then Begin
      result := false;
      break;
    End;
End;

Function Checkformel(Formel: String): boolean;
Var
  c, x: Integer;
Begin
  result := true;
  // Überprüfen der Klammerung
  c := 0;
  For x := 1 To length(Formel) Do Begin
    If Formel[x] = '(' Then inc(c);
    If Formel[x] = ')' Then dec(c);
    If c < 0 Then Begin
      result := false;
      break;
    End;
  End;
  If (Result) And (Pos('(', Formel) <> 0) Then
    For x := 1 To length(Formel) Do
      If (Formel[x] = ')') And (Formel[x + 1] = '(') Then Begin
        Result := false;
        break;
      End;
  If c <> 0 Then result := false;
  // Überprüfen der Klammerung
  // Überprüfen ob überhaupt was eingegeben wurde !!
  If Length(Formel) = 0 Then result := false;
  // Überprüfen ob Terme wie AndAnd vorkommen
  Formel := Clearformel(Formel);
  For x := 1 To length(Formel) - 1 Do
    If Formel[x] In ['/', '}', '\', '{', '['] Then
      If Formel[x + 1] In ['/', '}', '\', '{', '['] Then Begin
        Result := false;
        break;
      End;
End;

Procedure DrawVariablen;
Var
  x: integer;
Begin
  For x := 0 To high(Variablen) Do Begin
    Form1.StringGrid1.Cells[x, rowc + 1] := inttostr(ord(Variablen[x].value));
  End;
End;

Procedure IncVariablen;
Label
  bk;
Var
  x: Integer;
Begin
  x := high(Variablen);
  bk:
  If Variablen[x].value Then Begin
    Variablen[x].value := false;
    dec(x);
    Goto bk;
  End
  Else
    Variablen[x].value := true;
End;

Function getValue(Index: Integer): Integer;
Var
  x: Integer;
Begin
  Result := 0;
  For x := form1.Stringgrid1.colcount - 2 Downto 0 Do
    If form1.Stringgrid1.Cells[x, Index] = '1' Then
      Result := Result + pot(2, (form1.Stringgrid1.colcount - 2) - x);
End;

Procedure SetStringgrid;
Var
  x: Integer;
Begin
  form1.Stringgrid1.FixedCols := 0;
  form1.Stringgrid1.colcount := high(Variablen) + 2;
  form1.Stringgrid1.rowcount := pot(2, high(Variablen) + 1) + 1;
  For x := 0 To high(Variablen) Do
    form1.Stringgrid1.Cells[x, 0] := Uppercase(variablen[x].Name);
  form1.Stringgrid1.Cells[high(Variablen) + 1, 0] := 'Result';
End;

Procedure SetStringgrid2;
Var
  x: Integer;
Begin
  form1.Stringgrid1.FixedCols := 0;
  form1.Stringgrid1.colcount := high(Variablen) + 2;
  form1.Stringgrid1.rowcount := 1;
  For x := 0 To high(Variablen) Do
    form1.Stringgrid1.Cells[x, 0] := Uppercase(variablen[x].Name);
  form1.Stringgrid1.Cells[high(Variablen) + 1, 0] := 'Result';
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  Formel: String;
  a: Boolean;
  x: Integer;
Begin
  Formel := combobox1.text;
  If Checkformel(Formel) Then Begin
    StringGrid1.DefaultRowHeight := 24;
    Formel := ClearFormel(Formel);
    GetVarpara(Formel);
    SetStringgrid;
    x := 0;
    Initform3;
    While (Not AllVartrue) And (Not abbrechen) Do Begin
      inc(x);
      If x Mod 10 = 0 Then stepform3;
      Application.ProcessMessages;
      DrawVariablen;
      form1.StringGrid1.Cells[form1.StringGrid1.colcount - 1, rowc + 1] := GetFormelResult(Formel);
      IncVariablen;
      inc(Rowc);
    End;
    DisinitForm3;
    DrawVariablen;
    form1.StringGrid1.Cells[form1.StringGrid1.colcount - 1, rowc + 1] := GetFormelResult(Formel);
    If Assigned(Form4) Then
      If high(Variablen) <= 4 Then Begin
        If Form4.visible Then Drawform4;
      End
      Else If Form4.visible Then
        Form4.visible := false;
    a := true;
    For x := 0 To ComboBox1.items.count - 1 Do
      If Comparestr(lowercase(ClearFormel(Combobox1.text)), lowercase(ClearFormel(Combobox1.items[x]))) = 0 Then a := false;
    If A Then Combobox1.items.Insert(0, Combobox1.text);
  End
  Else
    application.Messagebox('Error in Formel', 'Info', mb_OK);
End;

Procedure TForm1.Button2Click(Sender: TObject);
Begin
  If Button2.caption = '&Close' Then Close;
  If Button2.caption = '&Cancel' Then Abbrechen := true;
End;

Procedure TForm1.FormResize(Sender: TObject);
Begin
  Button2.left := form1.width - button2.width - 22;
  Button1.left := button2.left - button1.width - 7;
  Combobox1.width := form1.width - button1.width - Button2.width - combobox1.left - 42;
  Posform3;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  Form1.caption := 'Bit Calculator ver. ' + Ver + ' by Corpsman | www.Corpsman.de';
  SRow := 1;
  Button1.onclick(Nil);
  first := true;
  Opendialog1.InitialDir := extractfilepath(application.exename);
  Opendialog2.InitialDir := extractfilepath(application.exename);
  savedialog1.InitialDir := extractfilepath(application.exename);
End;

Procedure TForm1.FormPaint(Sender: TObject);
Var
  f: Textfile;
  s: String;
Begin
  If (length(Paramstr(1)) <> 0) And first Then Begin
    first := false;
    savedialog1.InitialDir := extractfilepath(Paramstr(0));
    opendialog1.InitialDir := extractfilepath(Paramstr(0));
    Opendialog2.InitialDir := extractfilepath(Paramstr(0));
    assignfile(f, Paramstr(1));
    Reset(f);
    Readln(f, s);
    combobox1.text := s;
    closefile(f);
    Button1.onclick(Nil);
  End;
  Combobox1.SetFocus;
End;

Procedure TForm1.ComboBox1KeyPress(Sender: TObject; Var Key: Char);
Begin
  // Erlauben von Copy Paste
  If (ord(Key) = 22) Or (ord(Key) = 24) Or (ord(Key) = 3) Then exit;
  If Key = #13 Then Button1.onclick(Nil);
  If Not (Key In ['0'..'9', 'a'..'z', 'A'..'Z', ' ', '(', ')', '<', '>', '-', #8]) Then key := #0;
End;

Procedure TForm1.Load1Click(Sender: TObject);
Var
  f: Textfile;
  s: String;
Begin
  If Opendialog1.execute Then Begin
    savedialog1.InitialDir := extractfilepath(opendialog1.filename);
    form2.savedialog1.InitialDir := extractfilepath(application.exename);
    opendialog1.InitialDir := extractfilepath(opendialog1.filename);
    opendialog2.InitialDir := extractfilepath(opendialog1.filename);
    assignfile(f, Opendialog1.FileName);
    Reset(f);
    Readln(f, s);
    combobox1.text := s;
    closefile(f);
    button1.onclick(Nil);
  End;
End;

Procedure TForm1.Save1Click(Sender: TObject);
Var
  x, y: integer;
  f: Textfile;
  s: String;
Begin
  If Savedialog1.execute Then Begin
    Initform3;
    form2.savedialog1.InitialDir := extractfilepath(application.exename);
    savedialog1.InitialDir := extractfilepath(savedialog1.filename);
    opendialog1.InitialDir := extractfilepath(savedialog1.filename);
    opendialog2.InitialDir := extractfilepath(savedialog1.filename);
    assignfile(f, Savedialog1.FileName);
    rewrite(f);
    Writeln(f, combobox1.text);
    For y := 0 To stringgrid1.rowcount - 1 Do Begin
      If Abbrechen Then Break;
      If y Mod 10 = 0 Then stepform3;
      If Y = 1 Then Begin
        s := '-';
        For x := 0 To stringgrid1.Colcount * 2 Do
          s := s + '-';
        writeln(f, s);
      End;
      s := '|';
      For x := 0 To stringgrid1.Colcount - 1 Do Begin
        s := s + stringgrid1.cells[x, y] + '|';
      End;
      writeln(f, s);
    End;
    closefile(f);
    disinitform3;
  End;
End;

Procedure TForm1.ExtracttoLimboole1Click(Sender: TObject);
Var
  x: Integer;
Begin
  Form2.Memo1.clear;
  For x := 0 To High(Variablen) Do Begin
    Form2.Memo1.lines.add(Uppercase(Variablen[x].Name + ' -> ' + Variablen[x].Name));
  End;
  Form2.showmodal;
End;

Procedure TForm1.ShowOnlyResult11Click(Sender: TObject);
Var
  x, y: integer;
  tmp: Tstringgrid;
Begin
  // Nur die Zeilen mit Result = 1 Zeigen
  Colourize1.Checked := false;
  InitForm3;
  tmp := TStringGrid.Create(Self);
  tmp.Visible := false;
  tmp.FixedCols := 0;
  tmp.FixedRows := 0;
  tmp.ColCount := stringgrid1.ColCount;
  tmp.rowcount := 0;
  StringGrid1.visible := false;
  For x := 0 To Stringgrid1.rowcount - 1 Do Begin
    If Abbrechen Then Break;
    Application.ProcessMessages;
    If x Mod 10 = 0 Then stepform3;
    If Stringgrid1.cells[Stringgrid1.colcount - 1, x] = '1' Then Begin
      tmp.rowcount := tmp.rowcount + 1;
      For y := 0 To tmp.colcount - 1 Do
        tmp.cells[y, tmp.RowCount - 1] := StringGrid1.cells[y, x];
    End;
  End;
  Stringgrid1.RowCount := tmp.RowCount;
  For y := 1 To tmp.rowcount - 1 Do
    For x := 0 To tmp.colcount - 1 Do
      Stringgrid1.cells[x, y] := tmp.Cells[x, y];
  StringGrid1.visible := True;
  tmp.free;
  Disinitform3;
End;

Procedure TForm1.ShowonlyResult01Click(Sender: TObject);
Var
  x, y: integer;
  tmp: Tstringgrid;
Begin
  // Nur die Zeilen mit Result = 1 Zeigen
  Colourize1.Checked := false;
  InitForm3;
  tmp := TStringGrid.Create(Self);
  tmp.Visible := false;
  tmp.FixedCols := 0;
  tmp.FixedRows := 0;
  tmp.ColCount := stringgrid1.ColCount;
  tmp.rowcount := 0;
  StringGrid1.visible := false;
  For x := 0 To Stringgrid1.rowcount - 1 Do Begin
    If Abbrechen Then Break;
    Application.ProcessMessages;
    If x Mod 10 = 0 Then stepform3;
    If Stringgrid1.cells[Stringgrid1.colcount - 1, x] = '0' Then Begin
      tmp.rowcount := tmp.rowcount + 1;
      For y := 0 To tmp.colcount - 1 Do
        tmp.cells[y, tmp.RowCount - 1] := StringGrid1.cells[y, x];
    End;
  End;
  Stringgrid1.RowCount := tmp.RowCount;
  For y := 1 To tmp.rowcount - 1 Do
    For x := 0 To tmp.colcount - 1 Do
      Stringgrid1.cells[x, y] := tmp.Cells[x, y];
  StringGrid1.visible := True;
  tmp.free;
  Disinitform3;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  Abbrechen := true;
End;

Procedure TForm1.SavewithoutValues1Click(Sender: TObject);
Var
  f: Textfile;
Begin
  If Savedialog1.execute Then Begin
    form2.savedialog1.InitialDir := extractfilepath(application.exename);
    savedialog1.InitialDir := extractfilepath(savedialog1.filename);
    opendialog1.InitialDir := extractfilepath(savedialog1.filename);
    opendialog2.InitialDir := extractfilepath(savedialog1.filename);
    assignfile(f, Savedialog1.FileName);
    rewrite(f);
    Writeln(f, combobox1.text);
    closefile(f);
  End;
End;

Procedure TForm1.Colourize1Click(Sender: TObject);
Begin
  Colourize1.Checked := Not Colourize1.Checked;
  StringGrid1.Repaint;
End;

Procedure TForm1.StringGrid1DrawCell(Sender: TObject; ACol, ARow: Integer;
  Rect: TRect; State: TGridDrawState);
Var
  sg: TStringgrid;
Begin
  If Colourize1.Checked Then Begin
    sg := TStringGrid(Sender);
    If State = [gdFixed] Then
      sg.Canvas.Brush.Color := clbtnface
    Else Begin
      If sg.Cells[sg.colcount - 1, arow] = '0' Then
        sg.Canvas.Brush.Color := clred
      Else
        sg.Canvas.Brush.Color := Clgreen;
    End;
    sg.Canvas.font.Color := clblack;
    If SRow = Arow Then Begin
      sg.Canvas.Brush.Color := clnavy;
      sg.Canvas.font.Color := clwhite;
    End;
    sg.Canvas.FillRect(Rect);
    sg.Canvas.TextRect(Rect, Rect.Left + 2, Rect.Top + 2, sg.Cells[aCol, aRow]);
  End;
End;

Procedure TForm1.StringGrid1SelectCell(Sender: TObject; ACol,
  ARow: Integer; Var CanSelect: Boolean);
Begin
  SRow := Arow;
End;

Procedure TForm1.Resortlist1Click(Sender: TObject);
Var
  x, y, z: Integer;
  s: String;
Begin
  InitForm3;
  For x := Stringgrid1.rowcount - 1 Downto 2 Do Begin
    Application.ProcessMessages;
    If x Mod 10 = 0 Then stepform3;
    For y := 2 To x Do
      If GetValue(y) < GetValue(y - 1) Then
        For z := 0 To Stringgrid1.Colcount - 1 Do Begin
          s := Stringgrid1.cells[z, y];
          Stringgrid1.cells[z, y] := Stringgrid1.cells[z, y - 1];
          Stringgrid1.cells[z, y - 1] := s;
        End;
  End;
  Disinitform3;
End;

Procedure TForm1.ImportfromLimboole1Click(Sender: TObject);
Var
  f: Textfile;
  Formel, s: String;
  x: integer;
Begin
  If Opendialog2.execute Then Begin
    Formel := '';
    savedialog1.InitialDir := extractfilepath(opendialog2.filename);
    form2.savedialog1.InitialDir := extractfilepath(application.exename);
    opendialog2.InitialDir := extractfilepath(opendialog2.filename);
    opendialog1.InitialDir := extractfilepath(opendialog2.filename);
    assignfile(f, opendialog2.FileName);
    Reset(f);
    While Not EOF(F) Do Begin
      Readln(f, s);
      For x := 1 To length(s) Do Begin
        If S[x] In ['a'..'z', 'A'..'Z', '0'..'9', '>', '-', '<', ' ', '(', ')'] Then Formel := Formel + s[x];
        If s[x] = '&' Then Formel := Formel + 'and';
        If s[x] = '|' Then Formel := Formel + 'or';
        If s[x] = '!' Then Formel := Formel + 'not';
        If s[x] = '%' Then break; // bei kommentaren wird nichts gemacht !!
      End;
    End;
    closefile(f);
    ComboBox1.text := Formel;
    Button1.onclick(Nil);
  End;
End;

Procedure TForm1.Support1Click(Sender: TObject);
Begin
  application.Messagebox(pchar('This programm is made by personal interests' + LineEnding +
    'of the programmer Corpsman.' + LineEnding + LineEnding +
    'There is no garanty that it work correctly !!' + LineEnding + LineEnding +
    'If you find a bug please report it under :' + LineEnding +
    'www.Corpsman.de' + LineEnding + LineEnding +
    'Lizense: this product is postcardware see homepage for details.'), 'Info', MB_ICONINFORMATION);
End;

Procedure TForm1.Help1Click(Sender: TObject);
Begin
  application.Messagebox(pchar('Just type a formel in the text field' + LineEnding +
    'and click on get.' + LineEnding + LineEnding +
    'In some case it could be good to use the' + LineEnding +
    'right mouse button whily clicking on get.'
    ), 'Info', MB_ICONINFORMATION);
End;

Procedure TForm1.GetonlyResult11Click(Sender: TObject);
Var
  s, Formel: String;
  a: Boolean;
  x: Integer;
Begin // Result = 1
  If Form4.visible Then Form4.close;
  Colourize1.Checked := false;
  Formel := combobox1.text;
  If Checkformel(Formel) Then Begin
    StringGrid1.DefaultRowHeight := 24;
    Formel := ClearFormel(Formel);
    GetVarpara(Formel);
    SetStringgrid2;
    x := 0;
    Initform3;
    While (Not AllVartrue) And (Not abbrechen) Do Begin
      inc(x);
      If x Mod 10 = 0 Then stepform3;
      Application.ProcessMessages;
      DrawVariablen;
      s := GetFormelResult(Formel);
      If S = '1' Then Begin
        StringGrid1.RowCount := rowc + 2;
        form1.StringGrid1.Cells[form1.StringGrid1.colcount - 1, rowc + 1] := s;
        inc(Rowc);
      End;
      IncVariablen;
    End;
    DisinitForm3;
    DrawVariablen;
    s := GetFormelResult(Formel);
    If S = '1' Then Begin
      StringGrid1.RowCount := rowc + 2;
      form1.StringGrid1.Cells[form1.StringGrid1.colcount - 1, rowc + 1] := s;
    End;
    a := true;
    For x := 0 To ComboBox1.items.count - 1 Do
      If Comparestr(lowercase(ClearFormel(Combobox1.text)), lowercase(ClearFormel(Combobox1.items[x]))) = 0 Then a := false;
    If A Then Combobox1.items.Insert(0, Combobox1.text);
    StringGrid1.FixedRows := 1;
  End
  Else
    application.Messagebox('Error in Formel', 'Info', mb_OK);
End;

Procedure TForm1.GetonlyResult01Click(Sender: TObject);
Var
  s, Formel: String;
  a: Boolean;
  x: Integer;
Begin // Result = 0
  If Form4.visible Then Form4.close;
  Colourize1.Checked := false;
  Formel := combobox1.text;
  If Checkformel(Formel) Then Begin
    StringGrid1.DefaultRowHeight := 24;
    Formel := ClearFormel(Formel);
    GetVarpara(Formel);
    SetStringgrid2;
    x := 0;
    Initform3;
    While (Not AllVartrue) And (Not abbrechen) Do Begin
      inc(x);
      If x Mod 10 = 0 Then stepform3;
      Application.ProcessMessages;
      DrawVariablen;
      s := GetFormelResult(Formel);
      If S = '0' Then Begin
        StringGrid1.RowCount := rowc + 2;
        form1.StringGrid1.Cells[form1.StringGrid1.colcount - 1, rowc + 1] := s;
        inc(Rowc);
      End;
      IncVariablen;
    End;
    DisinitForm3;
    DrawVariablen;
    s := GetFormelResult(Formel);
    If S = '0' Then Begin
      StringGrid1.RowCount := rowc + 2;
      form1.StringGrid1.Cells[form1.StringGrid1.colcount - 1, rowc + 1] := s;
    End;
    a := true;
    For x := 0 To ComboBox1.items.count - 1 Do
      If Comparestr(lowercase(ClearFormel(Combobox1.text)), lowercase(ClearFormel(Combobox1.items[x]))) = 0 Then a := false;
    If A Then Combobox1.items.Insert(0, Combobox1.text);
    StringGrid1.FixedRows := 1;
  End
  Else
    application.Messagebox('Error in Formel', 'Info', mb_OK);
End;

Procedure TForm1.MakeKVDiagram1Click(Sender: TObject);
Const
  c = 6;
Begin
  If High(Variablen) > C - 1 Then Begin
    application.Messagebox(pchar('No K-V- Diagramm over ' + inttostr(C) + ' variables'), 'Error', MB_ICONWARNING);
  End
  Else Begin
    If Stringgrid1.rowcount = pot(2, high(Variablen) + 1) + 1 Then
      Resortlist1Click(Nil)
    Else
      Button1.onclick(Nil);
    Form4.Caption := 'K - V - Diagramm ' + Combobox1.text;
    Drawform4;
    Form4.show;
  End;
End;

End.

