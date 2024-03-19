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
Unit RayParser;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Dialogs, // Debuggen
  graphics, sysutils, Raytracer, raytracer_math, uTokenizer, uncommenter, FileManager,
  IntfGraphics, fpImage, LCLType;

Type
  (*
  Ach wäre es schön wenn man diese Records nicht von ausen sichtbar wären.

  Wir brauchen dieses Record nur deswegen weil die Primitive des Raytracers
  eine Standartisiertes Material haben und die TexturKoordinaten rausgeführt
  sind. Da das aber eigentlich zusammengehört baut der Parser die
  Materialeigenschaften so um das es wieder passt.
  *)
  TmaterialData = Record
    Material: TMaterial;
    TexCoords: Tvector2fDynArr;
  End;

  TStringArr = Array Of String;

  (*
  Die Folgenden Records erlauben Variablen, für jeden Variablentyp ein Rekord ..
  *)
  TRayFloats = Record
    name: String;
    value: TRayFloat;
  End;

  TRay2Floats = Record
    name: String;
    value: TVector2f;
  End;

  TRay3Floats = Record
    name: String;
    value: TVector3f;
  End;

  TRay4Floats = Record
    name: String;
    value: TVector4f;
  End;

  TRayStrings = Record
    name: String;
    value: String;
  End;

  TRayBooleans = Record
    name: String;
    value: Boolean;
  End;

  TRayModes = Record
    name: String;
    value: TInterpolationMode;
  End;

  (*
  Der eigentliche Parser der eine Scene einliest und dann den Raytracer
  bzw den Texturmanager entsprechend Konfiguriert.
  *)
  TRayparser = Class
  private
    ATokenline: String;
    RAToken: String;
    Atoken: String;
    FTokenPointer: Integer;
    FGlobalErrorMessage: String;
    FTokens: TTokenarray;
    FTokenizer: TTokenizer;
    FUncommenter: TUnCommenter;
    FRaytracer: TRaytracer;
    FTextureManager: TTextureManager;
    FCancelParsing: Boolean;
    Fglobalfilename: String;
    FFilename: String;
    FRayfloats: Array Of TRayFloats;
    FRay2floats: Array Of TRay2Floats;
    FRay3floats: Array Of TRay3Floats;
    FRay4floats: Array Of TRay4Floats;
    FRayStrings: Array Of TRayStrings;
    FRayBooleans: Array Of TRayBooleans;
    FRayModes: Array Of TRayModes;
    FFilemanager: TFilemanager;
    //    Function StringToFloat(Value: String): TRayFloat;
    Function isValidFloat(Value: String): boolean;
    Function isValidInteger(Value: String): boolean;
    Function isValidIdentifier(Value: String): Boolean;
    Procedure Abbruch(Value: String);
    Procedure Warning(Value: String);
    Procedure Analyze;
    Procedure EatRaytracerOptions;
    Procedure EatVariables;
    Procedure EatLight;
    Procedure EatToken;
    Procedure EatSemicolon;
    Function EatExternals: TPrimitiveArray;
    Function EatSphere: TSphere;
    Function EatQuad: TQuad;
    Function EatTriangle: TTriangle;
    Function EatMaterial(TexParameterCount: integer; ShadowLineOption: Boolean; SmoothShadowOption: Boolean): TmaterialData;
    Function Eatstring: String;
    Function EatInteger: Integer;
    Function EatSingle: TRayFloat;
    Function EatTexInterpolationMode: TInterpolationMode;
    Function EatBoolean: Boolean;
    Function Eatvector2f: tvector2f;
    Function EatVector3f: tvector3f;
    Function EatVector4f: tvector4f;
    Procedure KonfiguretoParse;
    Procedure KonfiguretoFormat;
    Function ValueisonlySpace(Value: String): Boolean;
    Function Fparse(Value: String): TPrimitiveArray;
    Function FAnalyze: TPrimitiveArray;
  public
    Imagewidth: Integer;
    Imageheight: Integer;
    Constructor create(Raytracer: Traytracer; TextureManager: TTextureManager);
    Destructor Destroy; override;
    Function Parse(Value: String; Filename: String): String;
    Function Format(Value: String; KeyWords: TStringArr; AllKeyWords: TStringArr): String;
  End;

Function OnlyFirstUP(Value: String): String;

Implementation

Var
  FFloatseparator: String;

Function OnlyFirstUP(Value: String): String;
Begin
  If length(Value) <> 0 Then Begin
    Result := Lowercase(Value);
    result[1] := Uppercase(Value)[1];
  End
  Else
    Result := '';
End;

Function GetfloatSeparator: String;
Var
  f: Real;
  erg: String;
Begin
  f := 1.2;
  erg := floattostr(f);
  erg := copy(erg, pos('1', erg) + 1, pos('2', erg) - pos('1', erg) - 1);
  result := erg;
End;

Function StringToFloat(Value: String): TRayFloat;
Var
  i: Integer;
Begin
  i := pos('.', value);
  If i <> 0 Then
    value[i] := FFloatseparator[1];
  result := strtofloat(value);
End;

{ TRayparser }

Constructor TRayparser.create(Raytracer: Traytracer; TextureManager: TTextureManager);
Begin
  Inherited Create;
  FFloatseparator := GetfloatSeparator;
  FTokenizer := TTokenizer.create;
  FUncommenter := TUnCommenter.create;
  FRaytracer := Raytracer;
  FTextureManager := TextureManager;
  FFilemanager := TFilemanager.create;
End;

Destructor TRayparser.Destroy;
Begin
  FFilemanager.free;
  Ftokenizer.free;
  FUncommenter.free;
  FRaytracer := Nil;
End;

Procedure TRayparser.KonfiguretoParse;
Begin
  // Konfigurieren des Tokenizers
  FTokenizer.ClearRules;
  FTokenizer.ClearOperators;
  FTokenizer.ClearSeperators;
  FTokenizer.CaseSensitive := false;
  // Einfügen aller Zeichen die zwar Tokens Trennen aber selbst keine Tokens sind
  FTokenizer.Addseperator(#32);
{$IFDEF Windows}
  FTokenizer.Addseperator(#13#10);
{$ELSE}
  //  FTokenizer.Addseperator(#13); // Eigentlich sollte das nicht sein
  FTokenizer.Addseperator(#10);
  FTokenizer.Addseperator(#13);
{$ENDIF}
  // Einfügen aller Zeichen die Tokens trennen und selbst welche sind.
  FTokenizer.Addoperator('=');
  FTokenizer.Addoperator('{');
  FTokenizer.Addoperator('}');
  FTokenizer.Addoperator(')');
  FTokenizer.Addoperator('(');
  FTokenizer.Addoperator('/');
  FTokenizer.Addoperator(',');
  FTokenizer.Addoperator(';');
  // Ist für das Scannen nach Zeilen
  FTokenizer.Addoperator('~');
  // Einfügen der String Regel
  FTokenizer.AddRule('"', '"');

  // Konfigurieren des UnCommenters
  FUncommenter.ExtraCharLine := '~';
  FUncommenter.clearrules;
  FUncommenter.DellEmptyLines := true;
  FUncommenter.CaseSensitive := False;
  FUncommenter.NumberLines := True;
  FUncommenter.AddRule('//', '', true);
  //  FUncommenter.AddRule('//', #10, false);
  FUncommenter.AddRule('(*', '*)', false);
End;

Procedure TRayparser.KonfiguretoFormat;
Begin
  // Konfigurieren des Tokenizers
  FTokenizer.ClearRules;
  FTokenizer.ClearOperators;
  FTokenizer.ClearSeperators;
  FTokenizer.CaseSensitive := false;
  // Einfügen aller Zeichen die zwar Tokens Trennen aber selbst keine Tokens sind
  FTokenizer.Addseperator(#32);
  // Einfügen aller Zeichen die Tokens trennen und selbst welche sind.
//  FTokenizer.Addoperator(' ');
//  FTokenizer.Addoperator(#13#10);
{$IFDEF Windows}
  FTokenizer.Addoperator(#13#10);
{$ELSE}
  //  FTokenizer.Addseperator(#13); // Eigentlich sollte das nicht sein
  FTokenizer.Addoperator(#10);
{$ENDIF}
  FTokenizer.Addoperator('=');
  FTokenizer.Addoperator('{');
  FTokenizer.Addoperator('}');
  FTokenizer.Addoperator(')');
  FTokenizer.Addoperator('(');
  FTokenizer.Addoperator('/');
  FTokenizer.Addoperator(';');
  // Einfügen der String Regel
  FTokenizer.AddRule('"', '"');
  FTokenizer.AddRule('(*', '*)');
{$IFDEF Windows}
  FTokenizer.AddRule('//', #13#10);
{$ELSE}
  FTokenizer.AddRule('//', #10);
{$ENDIF}
  // Konfigurieren des UnCommenters
  FUncommenter.clearrules;
  FUncommenter.ExtraCharLine := '~';
  FUncommenter.CaseSensitive := False;
End;

Function TRayparser.ValueisonlySpace(Value: String): Boolean;
Var
  i: Integer;
Begin
  If length(Value) = 0 Then Begin
    result := False;
    exit;
  End;
  i := pos(' ', Value);
  While i <> 0 Do Begin
    delete(value, i, 1);
    i := pos(' ', Value);
  End;
  result := length(Value) = 0;
End;

Function TRayparser.Format(Value: String; KeyWords: TStringArr; AllKeyWords: TStringArr): String;
Var
  tmp: TTokenarray;
  lctoken, offset, erg: String;
  i, j: integer;
  insertOffset: Boolean;
  HasToinsertCRT: Boolean;
  insertedSpaced: Boolean;
  HasToInsertASpace: Boolean;
Begin
  For i := 0 To High(keywords) Do
    keywords[i] := lowercase(keywords[i]);
  For i := 0 To High(allkeywords) Do
    Allkeywords[i] := lowercase(allkeywords[i]);
  offset := '';
  HasToInsertASpace := False;
  insertedSpaced := False;
  HasToinsertCRT := false;
  KonfiguretoFormat;
  erg := '';
  insertOffset := False;
  tmp := FTokenizer.scan(Value);
  For i := 0 To high(tmp) Do Begin
    If tmp[i].Value = '}' Then Begin
      delete(offset, 1, 2);
    End;
    If insertOffset Then Begin
      erg := erg + Offset;
      insertedSpaced := True;
      If ValueisonlySpace(tmp[i].value) Then
        tmp[i].value := '';
    End;
    insertOffset := false;
    (*
    das Problem ist das ( ein Präfix von ( * ist !!!
    das mus wieder rausgehackt werden
    *)
    If (pos('(*', tmp[i].Value) = 1) And (erg[length(erg)] = '(') Then Begin
      delete(erg, length(erg), 1);
    End;
    (*
    das Problem ist das / ein Präfix von // ist !!!
    das mus wieder rausgehackt werden
    *)
    If (pos('//', tmp[i].Value) = 1) And (Length(erg) <> 0) Then
      If (erg[length(erg)] = '/') Then Begin
        If Length(erg) > 1 Then Begin
{$IFDEF Windows}
          If (erg[length(erg) - 1] <> ' ') And (erg[length(erg) - 1] <> #13) Then
{$ELSE}
          If (erg[length(erg) - 1] <> ' ') And (erg[length(erg) - 1] <> #10) And (erg[length(erg) - 1] <> #13) Then
{$ENDIF}
            erg[length(erg)] := ' '
          Else
            delete(erg, length(erg), 1);
        End
        Else
          delete(erg, length(erg), 1);
      End;
    // Dieser Hack sorgt dafür das die // Kommentare nicht eine Zeile nach unten geschoben werden.
    If HasToinsertCRT Then Begin
      If i <= (high(tmp) - 1) Then
        If (Pos('/', tmp[i].value) = 1) And (Pos('//', tmp[i + 1].value) = 1) Then HasToinsertCRT := false;
    End;

    (**************************************************************************)

    (* Groß Kleinschreibung*)
    lctoken := lowercase(tmp[i].Value);
    For j := 0 To High(allkeywords) Do
      If lctoken = allkeywords[j] Then Begin
        tmp[i].Value := OnlyFirstUP(tmp[i].Value);
        break;
      End;
    If HasToinsertCRT Then Begin
{$IFDEF Windows}
      If (tmp[i].Value = #13) Then
        erg := erg + tmp[i].Value
      Else Begin
        erg := erg + #13#10 + offset + tmp[i].Value;
{$ELSE}
      If (tmp[i].Value = #10) Or (tmp[i].Value = #13) Then
        erg := erg + tmp[i].Value
      Else Begin
        erg := erg + #10 + offset + tmp[i].Value;
{$ENDIF}
      End;
      HasToinsertCRT := False;
    End
    Else Begin
      If Not (ValueisonlySpace(tmp[i].value) And insertedSpaced) Then Begin
        If (tmp[i].value = '=') And (length(erg) <> 0) Then
          If Erg[length(erg)] <> ' ' Then erg := Erg + ' ';
        If HasToInsertASpace And (pos(' ', tmp[i].Value) <> 1) Then
          erg := Erg + ' ';
        HasToInsertASpace := False;
        erg := erg + tmp[i].Value;
      End;
    End;
    (**************************************************************************)
    If ValueisonlySpace(tmp[i].value) Then
      insertedSpaced := True
    Else
      insertedSpaced := false;
    If Pos('//', tmp[i].value) = 1 Then
      insertOffset := True;
    For j := 0 To High(keywords) Do
      If lctoken = keywords[j] Then Begin
        HasToinsertCRT := true;
        break;
      End;
    If tmp[i].Value = '=' Then
      HasToInsertASpace := True;
    If tmp[i].Value = ';' Then
      HasToinsertCRT := True;
    If tmp[i].Value = '}' Then Begin
      HasToinsertCRT := True;
    End;
    If tmp[i].Value = '{' Then Begin
      offset := offset + '  ';
      HasToinsertCRT := True;
    End;
    If (tmp[i].Value = #13) Or (tmp[i].Value = #10) Then
      insertOffset := True;
  End;
  result := erg;
End;
(*
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ACHTUNG
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

Änderungen in Dieser Function müssen in Fparse auch gemacht werden !!!!

*)

Function TRayparser.Parse(Value: String; Filename: String): String;
Var
  tmp: TTokenarray;
  rlc, i, n: Integer;
  //  s: String;
  //  f: Textfile;
Begin
  If Length(Filename) <> 0 Then
    FFilename := extractfilepath(Filename)
  Else
    FFilename := extractfilepath(paramstr(0));
  // Löschen aller bisher geladenen Dateien
  FFilemanager.Clear;
  Imagewidth := 400;
  Imageheight := 400;
  Fglobalfilename := '';
  setlength(FRayfloats, 0);
  setlength(FRay2floats, 0);
  setlength(FRay3floats, 0);
  setlength(FRay4floats, 0);
  setlength(FRayStrings, 0);
  setlength(FRayBooleans, 0);
  setlength(FRayModes, 0);
  //  s := FUncommenter.Uncomment(Value);
  //  Showmessage(s);
  //  tmp := FTokenizer.scan(s);
  // zerlegen in die Einzelnen Tokens
  KonfiguretoParse;
  //  s := ;
  //  assignfile(f, 'c:\temp\test1.txt');
  //rewrite(f);
  //writeln(f,s);
  //  reset(f);
  //  readln(f, s);
  //  closefile(f);

  tmp := FTokenizer.scan(FUncommenter.Uncomment(Value));
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
    Setlength(FTokens, High(tmp) + 1 - rlc);
    n := 0;
    i := 0;
    While i <= High(tmp) Do Begin
      If tmp[i].value <> '~' Then Begin
        FTokens[n] := Tmp[i];
        inc(n);
      End
      Else Begin
        inc(i);
      End;
      inc(i);
    End;
    setlength(tmp, 0);
    // So nun da alles Geparst ist gehts an Analysieren der Tokens
    If High(FTokens) <> -1 Then Begin
      setlength(Ftokens, high(Ftokens) + 2);
      Ftokens[high(ftokens)].Value := '"End of File"';
      Ftokens[high(ftokens)].Line := Ftokens[high(ftokens) - 1].Line;
      // Erst mal initialisieren
      FTokenPointer := -1;
      FGlobalErrorMessage := '';
      FCancelParsing := False;
      // Zurücksetzen des Raytracers
      FRaytracer.ClearScene;
      // Zurücksetzen des Texturmanagers
      FTextureManager.Clear;
      // einlesen des 1. Tokens
      EatToken;
      // Starten der Analyse
      Analyze;
      setlength(Ftokens, 0);
      // Ausgeben der Gefundenen Fehler
      Result := FGlobalErrorMessage;
    End
    Else
      Result := 'Error nothing to parse. 0';
  End
  Else
    Result := 'Error nothing to parse. 0';
End;

Procedure TRayparser.EatToken;
Begin
  inc(FTokenPointer);
  If high(fTokens) >= FTokenPointer Then Begin
    RAToken := Ftokens[FTokenPointer].value;
    Atoken := lowercase(Ftokens[FTokenPointer].value);
    ATokenline := inttostr(Ftokens[FTokenPointer].Line + 1);
  End
  Else Begin
    RAtoken := '';
    Atoken := '';
    ATokenline := inttostr(Ftokens[High(Ftokens)].Line + 1);
  End;
End;

Procedure TRayparser.Abbruch(Value: String);
Begin
  FGlobalErrorMessage := FGlobalErrorMessage + #13 + Value + ATokenline;
  FCancelParsing := True;
  AToken := '';
End;

Procedure Trayparser.Warning(Value: String);
Begin
  FGlobalErrorMessage := FGlobalErrorMessage + #13 + Value + ATokenline;
End;

(*
Liest einen Term der Form

= on;

oder

= off;

ein und gibt den entsprechenedn Boolschen wert zurück
*)

Function TRayparser.EatBoolean: Boolean;
Var
  i: Integer;
Begin
  result := False;
  If FCancelParsing Then exit;
  If AToken = '=' Then Begin
    EatToken;
    If AToken = 'on' Then Begin
      result := True;
      EatToken;
      EatSemicolon;
    End
    Else If Atoken = 'off' Then Begin
      result := False;
      EatToken;
      EatSemicolon;
    End
    Else Begin
      For i := 0 To high(Fraybooleans) Do
        If Fraybooleans[i].name = Atoken Then Begin
          result := Fraybooleans[i].value;
          eattoken;
          EatSemicolon;
          exit;
        End;
      abbruch('Error missing "on" or "off" ' + Fglobalfilename + ' at line ');
    End;
  End
  Else
    Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
End;
(*
Liest einen Term der Form

!! ACHTUNG !! das ; wird Absichtlich nicht mit eingelesen !!

( 0.0 / 0.0 )

ein und gibt den entsprechenedn Vector zurück

*)

Function TRayparser.Eatvector2f: tvector2f;
Var
  x, y: TRayFloat;
  i: integer;
Begin
  result := v2(0, 0);
  If FCancelParsing Then exit;
  If atoken = '(' Then Begin
    EatToken;
    x := EatSingle;
    If Atoken = ',' Then Begin
      Eattoken;
      y := EatSingle;
      If Atoken = ')' Then Begin
        eattoken;
        result := v2(x, y);
      End
      Else
        Abbruch('Error missing ")" ' + Fglobalfilename + ' at line ');
    End
    Else
      Abbruch('Error missing "," ' + Fglobalfilename + ' at line ');
  End
  Else Begin
    // Wenn wir anstatt eines Floats einen Variablenwert einlesen.
    For i := 0 To High(FRay2floats) Do
      If atoken = FRay2floats[i].name Then Begin
        result := FRay2floats[i].value;
        Eattoken;
        exit;
      End;
    //    Abbruch('Error missing "(" ' + Fglobalfilename + ' at line ');
    Abbruch('Error "' + Ftokens[Ftokenpointer].value + '" is not a valid 2D-Vector ' + Fglobalfilename + ' at line ');
  End;
End;

(*
Liest einen Term der Form

!! ACHTUNG !! das ; wird Absichtlich nicht mit eingelesen !!

( 0.0 / 0.0 / 0.0 )

ein und gibt den entsprechenedn Vector zurück

*)

Function TRayparser.eatvector3f: tvector3f;
Var
  x, y, z: TRayFloat;
  i: Integer;
Begin
  result := v3(0, 0, 0);
  If FCancelParsing Then exit;
  If atoken = '(' Then Begin
    EatToken;
    x := EatSingle;
    If Atoken = ',' Then Begin
      Eattoken;
      y := EatSingle;
      If Atoken = ',' Then Begin
        Eattoken;
        z := Eatsingle;
        If Atoken = ')' Then Begin
          eattoken;
          result := v3(x, y, z);
        End
        Else
          Abbruch('Error missing ")" ' + Fglobalfilename + ' at line ');
      End
      Else
        Abbruch('Error missing "," ' + Fglobalfilename + ' at line ');
    End
    Else
      Abbruch('Error missing "," ' + Fglobalfilename + ' at line ');
  End
  Else Begin
    // Wenn wir anstatt eines Floats einen Variablenwert einlesen.
    For i := 0 To High(FRay3floats) Do
      If atoken = FRay3floats[i].name Then Begin
        result := FRay3floats[i].value;
        Eattoken;
        exit;
      End;
    //    Abbruch('Error missing "(" ' + Fglobalfilename + ' at line ');
    Abbruch('Error "' + Ftokens[Ftokenpointer].value + '" is not a valid 3D-Vector ' + Fglobalfilename + ' at line ');
  End;
End;

(*
Liest einen Term der Form

!! ACHTUNG !! das ; wird Absichtlich nicht mit eingelesen !!

( 0.0 / 0.0 / 0.0 / 0.0 )

ein und gibt den entsprechenedn Vector zurück

*)

Function TRayparser.EatVector4f: tvector4f;
Var
  x, y, z, a: TRayFloat;
  i: integer;
Begin
  result := v4(0, 0, 0, 0);
  If FCancelParsing Then exit;
  If atoken = '(' Then Begin
    EatToken;
    x := EatSingle;
    If Atoken = ',' Then Begin
      Eattoken;
      y := EatSingle;
      If Atoken = ',' Then Begin
        Eattoken;
        z := EatSingle;
        If Atoken = ',' Then Begin
          Eattoken;
          a := EatSingle;
          If Atoken = ')' Then Begin
            eattoken;
            result := v4(x, y, z, a);
          End
          Else
            Abbruch('Error missing ")" ' + Fglobalfilename + ' at line ');
        End
        Else
          Abbruch('Error missing "," ' + Fglobalfilename + ' at line ');
      End
      Else
        Abbruch('Error missing "," ' + Fglobalfilename + ' at line ');
    End
    Else
      Abbruch('Error missing "," ' + Fglobalfilename + ' at line ');
  End
  Else Begin
    // Wenn wir anstatt eines Floats einen Variablenwert einlesen.
    For i := 0 To High(FRay4floats) Do
      If atoken = FRay4floats[i].name Then Begin
        result := FRay4floats[i].value;
        Eattoken;
        exit;
      End;
    Abbruch('Error "' + Ftokens[Ftokenpointer].value + '" is not a valid 4D-Vector ' + Fglobalfilename + ' at line ');
  End;
  //    Abbruch('Error missing "(" ' + Fglobalfilename + ' at line ');
End;

Procedure TRayparser.EatSemicolon;
Begin
  If FCancelParsing Then exit;
  If Atoken = ';' Then
    EatToken
  Else
    abbruch('Error missing ";" ' + Fglobalfilename + ' at line ');
End;

(*
Gibt True zurück wenn die Zahl der Form

Vorzeichen = +, -
Ziffer = 0, 1, 2, 3, 4, 5, 6, 7, 8, 9

[Vorzeichen] (Ziffer)^+ ["."(Ziffer)^+]

ist.
*)

Function TRayparser.isValidFloat(Value: String): boolean;
Var
  b: boolean;
Begin
  result := True;
  If Length(value) = 0 Then Begin
    result := false;
    exit;
  End;
  // Vorzeichen Abschneiden
  If (value[1] = '-') Or (value[1] = '+') Then delete(value, 1, 1);
  If Length(value) = 0 Then Begin
    result := false;
    exit;
  End;
  b := True;
  While b Do Begin
    If Length(value) > 0 Then Begin
      If Value[1] In ['0'..'9'] Then
        Delete(value, 1, 1)
      Else If Value[1] = '.' Then
        b := False
      Else Begin
        Result := false;
        exit;
      End;
    End
    Else
      exit;
  End;
  If Value[1] = '.' Then
    delete(value, 1, 1)
  Else Begin
    Result := False;
    exit;
  End;
  If Length(value) = 0 Then Begin
    result := false;
    exit;
  End;
  b := True;
  While b Do Begin
    If Length(value) > 0 Then Begin
      If Value[1] In ['0'..'9'] Then
        Delete(value, 1, 1)
      Else Begin
        Result := false;
        exit;
      End;
    End
    Else
      exit;
  End;
End;

(*
Liest einen Term der Form

= "String"
ein und gibt dann

String

zurück.

Das @local@, @scenepath@ token wird hier auch gleich ersetzt.

*)

Function TRayparser.Eatstring: String;
Var
  i: integer;
Begin
  Result := '';
  If Atoken = '=' Then Begin
    Eattoken;
    If Length(Atoken) >= 2 Then Begin
      If (AToken[1] = '"') And (Atoken[Length(Atoken)] = '"') Then Begin
        result := copy(RAToken, 2, length(RAToken) - 2);
        EatToken;
        If FCancelParsing Then
          result := '';
      End
      Else Begin
        For i := 0 To high(FRaystrings) Do
          If Fraystrings[i].name = Atoken Then Begin
            result := Fraystrings[i].Value;
            eattoken;
            exit;
          End;
        Abbruch('Error "' + RAToken + '"is not a valid String ' + Fglobalfilename + ' at line ');
      End;
    End
    Else
      Abbruch('Error "' + RAToken + '"is not a valid String ' + Fglobalfilename + ' at line ');
  End
  Else
    Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
  If pos('@local@', lowercase(result)) = 1 Then Begin
    result := IncludeTrailingBackslash(extractfilepath(paramstr(0))) + copy(result, 8, length(result));
  End;
  If pos('@scenepath@', lowercase(result)) = 1 Then Begin
    result := IncludeTrailingBackslash(FFilename) + copy(result, 12, length(result));
  End;
End;

(*
Liest einen Term der Form

nearest;

ein und gibt diesen als TInterpolationMode zurück
*)

Function TRayparser.EatTexInterpolationMode: TInterpolationMode;
Var
  i: Integer;
Begin
  Result := Nearest_Neighbour;
  If Atoken = 'nearest' Then Begin
    Eattoken;
    result := Nearest_Neighbour;
    EatSemicolon;
  End
  Else If atoken = 'biliniar' Then Begin
    Eattoken;
    result := Biliniar;
    EatSemicolon;
  End
  Else If atoken = 'cos' Then Begin
    Eattoken;
    result := Cosinus;
    EatSemicolon;
  End
  Else If atoken = 'triliniar' Then Begin
    Eattoken;
    result := Triliniar;
    EatSemicolon;
  End
  Else Begin
    For i := 0 To high(FRayModes) Do
      If FRayModes[i].name = Atoken Then Begin
        result := FRayModes[i].value;
        eattoken;
        EatSemicolon;
        exit;
      End;
    Abbruch('Error missing "nearest" or "biliniar" or "cos" or "triliniar" ' + Fglobalfilename + ' at line ');
  End;
End;
(*
Liest einen Term der Form

0.0

ein und und gibt diesen als Float wieder zurück

*)

Function TRayparser.eatSingle: TRayFloat;
Var
  i: Integer;
  v: TRayFloat;
  num: String;
Begin
  result := 0;
  If isValidFloat(atoken) Then Begin
    result := StringToFloat(Atoken);
    eattoken;
  End
  Else Begin
    v := 1;
    num := Atoken;
    // für das Negieren einer Variablen ...
    If Length(num) <> 0 Then
      If num[1] = '-' Then Begin
        delete(num, 1, 1);
        v := -1;
      End;
    // Wenn wir anstatt eines Floats einen Variablenwert einlesen.
    For i := 0 To High(FRayfloats) Do
      If num = FRayfloats[i].name Then Begin
        result := v * FRayfloats[i].value;
        Eattoken;
        exit;
      End;
    Abbruch('Error "' + Ftokens[Ftokenpointer].value + '" is not a valid single ' + Fglobalfilename + ' at line ');
  End;
End;

(*
Liest einen term der Form

!! ACHTUNG !! das ; wird Absichtlich nicht mit eingelesen !!

= 0

ein.
*)

Function TRayparser.EatInteger: Integer;
Begin
  result := 0;
  If FCancelParsing Then exit;
  If Atoken = '=' Then Begin
    Eattoken;
    If isValidInteger(Atoken) Then Begin
      result := strtoint(Atoken);
      Eattoken;
    End
    Else
      Abbruch('Error "' + Ftokens[Ftokenpointer].value + '" is not a valid integer ' + Fglobalfilename + ' at line ');
  End
  Else
    Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
End;

Function TRayparser.isValidIdentifier(Value: String): Boolean;

Begin
  result := true;
  If Length(Value) < 2 Then Begin
    result := False;
    exit;
  End;
  While length(value) <> 0 Do Begin
    If Not (value[1] In ['a'..'z', 'A'..'Z', '_', '0'..'9']) Then Begin
      result := False;
      exit;
    End;
    delete(Value, 1, 1);
  End;
End;

Function TRayparser.isValidInteger(Value: String): boolean;
Var
  b: boolean;
Begin
  result := True;
  If Length(value) = 0 Then Begin
    result := false;
    exit;
  End;
  // Vorzeichen Abschneiden
  If (value[1] = '-') Or (value[1] = '+') Then delete(value, 1, 1);
  If Length(value) = 0 Then Begin
    result := false;
    exit;
  End;
  b := True;
  While b Do Begin
    If Length(value) > 0 Then Begin
      If Value[1] In ['0'..'9'] Then
        Delete(value, 1, 1)
      Else Begin
        Result := false;
        exit;
      End;
    End
    Else
      exit;
  End;
End;

Function TRayparser.FAnalyze: TPrimitiveArray;
Var
  Fquad: TQuad;
  FTriangle: TTriangle;
  FSPhere: TSphere;
  onereaded: Boolean;
  Felements: TPrimitiveArray;
  i, j: Integer;
Begin
  Setlength(Result, 0);
  While (FTokenPointer <= High(Ftokens)) And Not (FCancelParsing) Do Begin
    onereaded := False;
    // Einlesen der Raytracer Einstellungen
    If Atoken = 'raytracer' Then Begin
      onereaded := True;
      EatRaytracerOptions; // Fertig Getestet
    End;
    // Einlesen von Variablen
    If Atoken = 'variables' Then Begin
      onereaded := True;
      EatVariables;
    End;
    // Einlesen einer Lichtquelle
    If Atoken = 'light' Then Begin
      onereaded := True;
      EatLight; // Fertig Getestet
    End;
    // Einlesen eines Dreiecks
    If Atoken = 'triangle' Then Begin
      onereaded := True;
      Ftriangle := EatTriangle;
      // War das Parsing Erfolgreich, dann wird das Dreieck Geadded.
      If Ftriangle <> Nil Then Begin
        Setlength(result, high(result) + 2);
        result[high(result)] := Ftriangle;
      End;
    End;
    // Einlesen eines Vierecks
    If Atoken = 'quad' Then Begin
      onereaded := True;
      Fquad := eatQuad;
      // War das Parsing Erfolgreich, dann wird das Dreieck Geadded.
      If Fquad <> Nil Then Begin
        Setlength(result, high(result) + 2);
        result[high(result)] := Fquad;
      End;
    End;
    // Einlesen Kugel
    If Atoken = 'sphere' Then Begin
      onereaded := True;
      FSPhere := EatSphere;
      // War das Parsing Erfolgreich, dann wird das Dreieck Geadded.
      If FSPhere <> Nil Then Begin
        Setlength(result, high(result) + 2);
        result[high(result)] := FSPhere;
      End;
    End;
    // Laden anderer Dateien
    If atoken = 'external' Then Begin
      onereaded := True;
      setlength(Felements, 0); // Warnungen loswerden
      Felements := EatExternals;
      If Not FCancelParsing Then Begin
        j := high(result) + 1;
        setlength(result, j + high(Felements) + 1);
        For i := 0 To high(Felements) Do Begin
          result[j + i] := Felements[i];
        End;
      End;
    End;
    If atoken = '"end of file"' Then Begin
      onereaded := True;
      EatToken;
    End;
    If Not onereaded Then Begin
      FGlobalErrorMessage := FGlobalErrorMessage + #13 +
        'Error unknown Identifier "' + Ftokens[FTokenPointer].Value + '" ' + Fglobalfilename + ' at line ' + inttostr(Ftokens[FTokenPointer].Line + 1);
      exit;
    End;
  End;
End;

(*
Liest die Tokenliste aus und Versucht entsprechend zu handeln

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ACHTUNG
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

Wird hier was geändert mus das auch in Fanalyze gemacht werden !!!
*)

Procedure TRayparser.Analyze;
Var
  Fquad: TQuad;
  FTriangle: TTriangle;
  FSPhere: TSphere;
  onereaded: Boolean;
  Felements: TPrimitiveArray;
  i: integer;
Begin
  While (FTokenPointer <= High(Ftokens)) And Not (FCancelParsing) Do Begin
    onereaded := False;
    // Einlesen der Raytracer Einstellungen
    If Atoken = 'raytracer' Then Begin
      onereaded := True;
      EatRaytracerOptions; // Fertig Getestet
    End;
    // Einlesen von Variablen
    If Atoken = 'variables' Then Begin
      onereaded := True;
      EatVariables;
    End;
    // Einlesen einer Lichtquelle
    If Atoken = 'light' Then Begin
      onereaded := True;
      EatLight; // Fertig Getestet
    End;
    // Einlesen eines Dreiecks
    If Atoken = 'triangle' Then Begin
      onereaded := True;
      Ftriangle := EatTriangle;
      // War das Parsing Erfolgreich, dann wird das Dreieck Geadded.
      If Ftriangle <> Nil Then Begin
        FRaytracer.AddPrimitive(Ftriangle);
        Ftriangle.Free;
      End;
    End;
    // Einlesen eines Vierecks
    If Atoken = 'quad' Then Begin
      onereaded := True;
      Fquad := eatQuad;
      // War das Parsing Erfolgreich, dann wird das Dreieck Geadded.
      If Fquad <> Nil Then Begin
        FRaytracer.AddPrimitive(Fquad);
        Fquad.Free;
      End;
    End;
    // Einlesen Kugel
    If Atoken = 'sphere' Then Begin
      onereaded := True;
      FSPhere := EatSphere;
      // War das Parsing Erfolgreich, dann wird das Dreieck Geadded.
      If FSPhere <> Nil Then Begin
        FRaytracer.AddPrimitive(FSPhere);
        FSPhere.Free;
      End;
    End;
    // Laden anderer Dateien
    If atoken = 'external' Then Begin
      onereaded := True;
      setlength(Felements, 0); // Warnungen loswerden
      Felements := EatExternals;
      // Einfügen aller Objecte in den Raytracer, oder bei Abbruch die Bestehenden Löschen .
      For i := 0 To High(FElements) Do Begin
        If Not FCancelParsing Then
          FRaytracer.AddPrimitive(Felements[i]);
        Felements[i].free;
      End;
    End;
    If atoken = '"end of file"' Then Begin
      onereaded := True;
      EatToken;
    End;
    If Not onereaded Then Begin
      FGlobalErrorMessage := FGlobalErrorMessage + #13 +
        'Error unknown Identifier "' + Ftokens[FTokenPointer].Value + '" ' + Fglobalfilename + ' at line ' + inttostr(Ftokens[FTokenPointer].Line + 1);
      exit;
    End;
  End;
End;

(*
Diese File ist fast die Kopie von Parse

!!!!!!!!!!!!!!! ACHTUNG !!!!!!!!!!!!!!!!!!

Value ist hier der Dateiname, nicht die Daten !!

Das ist so damit wir die Datei über den Filemanager Laden können !!!

*)

Function TRayparser.Fparse(Value: String): TPrimitiveArray;
Begin
  // Das Nutzen des Filemanagers beschleunigt das Parsen deutlich !!
  // Da mehrfach geladene Dateien nicht mehrfach geparst werden müssen.
  Ftokens := FFilemanager.ParseFile(Value, FTokenizer, FUncommenter);
  If High(FTokens) <> -1 Then Begin
    // Erst mal initialisieren
    FTokenPointer := -1;
    EatToken;
    // Starten der Analyse
    Result := FAnalyze;
  End;
End;

Function TRayparser.EatExternals: TPrimitiveArray;
Var
  (*
  Alle Alten Daten speichern.
  *)
  OldExternalFile: String;
  oldtokens: TTokenarray;
  oldtokenpointer: Integer;
  oldtokenline: String;
  oldRatoken, oldatoken: String;
  fElements: TPrimitiveArray;
  onereaded: Boolean;
  i, j: Integer;
  Filename: String;
  Defined: Boolean;
  rtv: TVector4f;
  tv: TVector3f;
Begin
  Filename := '';
  Defined := false;
  setlength(result, 0);
  setlength(Felements, 0); // Warnungen loswerden
  oldtokens := Nil; // Keine Ahnung warum er da ne Warnung bringt wenn man diese Zeile weg lässt.
  Eattoken;
  If atoken = '{' Then Begin
    EatToken;
    // Alles Lesen was es Gibt
    While (Atoken <> '}') And (Not FCancelParsing) Do Begin
      onereaded := false;
      If atoken = 'filename' Then Begin
        onereaded := True;
        Eattoken;
        Filename := Eatstring;
        If FileExists(Filename + '.scene') { *Converted from FileExists*  } Then Begin
          Defined := True;
          OldExternalFile := Fglobalfilename;
          Oldtokens := ftokens;
          oldtokenpointer := FTokenPointer;
          oldRatoken := RAToken;
          oldatoken := Atoken;
          oldtokenline := ATokenline;
          Fglobalfilename := ',[' + ExtractFileName(Filename + '.scene]');
          // Laden der Externen Datei
          fElements := Fparse(Filename + '.scene');
          j := high(result) + 1;
          setlength(result, j + high(Felements) + 1);
          For i := 0 To high(fElements) Do Begin
            result[i + j] := Felements[i];
          End;
          If FCancelParsing Then
            abbruch('Error in "' + filename + '.scene" ' + Fglobalfilename + ' at line ');
          ftokens := Oldtokens;
          Fglobalfilename := OldExternalFile;
          FTokenPointer := oldtokenpointer;
          RAToken := oldRatoken;
          Atoken := oldatoken;
          ATokenline := oldtokenline;
        End
        Else
          Abbruch('Error could not open "' + filename + '.scene" ' + Fglobalfilename + ' at line ');
        EatSemicolon;
        //        End;
      End;
      // Einlesen von Variablen
      If Atoken = 'variables' Then Begin
        onereaded := True;
        EatVariables;
      End;
      If Atoken = 'rotate' Then Begin
        If Defined Then Begin
          eattoken;
          onereaded := True;
          If atoken = '=' Then Begin
            Eattoken;
            rtv := EatVector4f;
            EatSemicolon;
            If Not FCancelParsing Then
              For i := 0 To High(Result) Do
                Result[i].Rotate(rtv.x, rtv.y, rtv.z, rtv.w);
          End
          Else
            Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
        End
        Else
          Abbruch('Rotation only after loading external file allowed' + Fglobalfilename + ' at line ');
      End;
      If Atoken = 'translate' Then Begin
        If Defined Then Begin
          eattoken;
          onereaded := True;
          If atoken = '=' Then Begin
            Eattoken;
            tv := EatVector3f;
            EatSemicolon;
            If Not FCancelParsing Then
              For i := 0 To High(Result) Do
                Result[i].Translate(tv);
          End
          Else
            Abbruch('Error missing "=" at line ');
        End
        Else
          Abbruch('Translation only after loading external file allowed ' + Fglobalfilename + ' at line ');
      End;
      // Wenn ein Gänzlich unbekannter Token kommt verhindern wir so die Endlosschleife
      If Not onereaded Then Begin
        Abbruch('Error unknown value "' + Ftokens[FTokenPointer].Value + '" ' + Fglobalfilename + ' at line ');
      End;
    End;
    eattoken;
  End
  Else
    Abbruch('Error missing "{" ' + Fglobalfilename + ' at line ');
End;

Procedure TRayparser.EatVariables;
Var
  onereaded: Boolean;
  p, i: Integer;
Begin
  EatToken;
  If Atoken = '{' Then Begin
    EatToken;
    // Alles Lesen was es Gibt
    While (Atoken <> '}') And (Not FCancelParsing) Do Begin
      onereaded := false;
      If isValidIdentifier(Atoken) Then Begin
        Case Atoken[length(atoken)] Of
          '1': Begin
              onereaded := True;
              // berechnen der Position der Variablen im Array
              p := -1;
              For i := 0 To High(FRayfloats) Do
                If FRayfloats[i].name = Atoken Then Begin
                  p := i;
                  break;
                End;
              // Gibts die Variable noch nicht Legen wir sie an.
              If p = -1 Then Begin
                setlength(FRayfloats, high(FRayfloats) + 2);
                p := high(FRayfloats);
                FRayfloats[p].name := Atoken;
                FRayfloats[p].value := 0;
              End;
              // Auslesen der Variablen
              EatToken;
              If atoken = '=' Then Begin
                Eattoken;
                FRayfloats[p].value := EatSingle;
                EatSemicolon;
              End
              Else
                Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
            End;
          '2': Begin
              onereaded := True;
              // berechnen der Position der Variablen im Array
              p := -1;
              For i := 0 To High(FRay2floats) Do
                If FRay2floats[i].name = Atoken Then Begin
                  p := i;
                  break;
                End;
              // Gibts die Variable noch nicht Legen wir sie an.
              If p = -1 Then Begin
                setlength(FRay2floats, high(FRay2floats) + 2);
                p := high(FRay2floats);
                FRay2floats[p].name := Atoken;
                FRay2floats[p].value := v2(0, 0);
              End;
              // Auslesen der Variablen
              EatToken;
              If atoken = '=' Then Begin
                Eattoken;
                FRay2floats[p].value := Eatvector2f;
                EatSemicolon;
              End
              Else
                Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
            End;
          '3': Begin
              onereaded := True;
              // berechnen der Position der Variablen im Array
              p := -1;
              For i := 0 To High(FRay3floats) Do
                If FRay3floats[i].name = Atoken Then Begin
                  p := i;
                  break;
                End;
              // Gibts die Variable noch nicht Legen wir sie an.
              If p = -1 Then Begin
                setlength(FRay3floats, high(FRay3floats) + 2);
                p := high(FRay3floats);
                FRay3floats[p].name := Atoken;
                FRay3floats[p].value := v3(0, 0, 0);
              End;
              // Auslesen der Variablen
              EatToken;
              If atoken = '=' Then Begin
                Eattoken;
                FRay3floats[p].value := Eatvector3f;
                EatSemicolon;
              End
              Else
                Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
            End;
          '4': Begin
              onereaded := True;
              // berechnen der Position der Variablen im Array
              p := -1;
              For i := 0 To High(FRay4floats) Do
                If FRay4floats[i].name = Atoken Then Begin
                  p := i;
                  break;
                End;
              // Gibts die Variable noch nicht Legen wir sie an.
              If p = -1 Then Begin
                setlength(FRay4floats, high(FRay4floats) + 2);
                p := high(FRay4floats);
                FRay4floats[p].name := Atoken;
                FRay4floats[p].value := v4(0, 0, 0, 0);
              End;
              // Auslesen der Variablen
              EatToken;
              If atoken = '=' Then Begin
                Eattoken;
                FRay4floats[p].value := Eatvector4f;
                EatSemicolon;
              End
              Else
                Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
            End;
          's': Begin
              onereaded := True;
              // berechnen der Position der Variablen im Array
              p := -1;
              For i := 0 To High(FRayStrings) Do
                If FRayStrings[i].name = Atoken Then Begin
                  p := i;
                  break;
                End;
              // Gibts die Variable noch nicht Legen wir sie an.
              If p = -1 Then Begin
                setlength(FRayStrings, high(FRayStrings) + 2);
                p := high(FRayStrings);
                FRayStrings[p].name := Atoken;
                FRayStrings[p].value := '';
              End;
              // Auslesen der Variablen
              EatToken;
              FRayStrings[p].value := Eatstring;
              EatSemicolon;
            End;
          'b': Begin
              onereaded := True;
              // berechnen der Position der Variablen im Array
              p := -1;
              For i := 0 To High(Fraybooleans) Do
                If Fraybooleans[i].name = Atoken Then Begin
                  p := i;
                  break;
                End;
              // Gibts die Variable noch nicht Legen wir sie an.
              If p = -1 Then Begin
                setlength(Fraybooleans, high(Fraybooleans) + 2);
                p := high(Fraybooleans);
                Fraybooleans[p].name := Atoken;
                Fraybooleans[p].value := false;
              End;
              // Auslesen der Variablen
              EatToken;
              Fraybooleans[p].value := EatBoolean;
            End;
          'm': Begin
              onereaded := True;
              // berechnen der Position der Variablen im Array
              p := -1;
              For i := 0 To High(FRayModes) Do
                If FRayModes[i].name = Atoken Then Begin
                  p := i;
                  break;
                End;
              // Gibts die Variable noch nicht Legen wir sie an.
              If p = -1 Then Begin
                setlength(FRayModes, high(FRayModes) + 2);
                p := high(FRayModes);
                FRayModes[p].name := Atoken;
                FRayModes[p].value := Nearest_Neighbour;
              End;
              // Auslesen der Variablen
              EatToken;
              If atoken = '=' Then Begin
                Eattoken;
                FRayModes[p].value := EatTexInterpolationMode;
              End
              Else
                Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');

            End;
        End;
      End;
      // Wenn ein Gänzlich unbekannter Token kommt verhindern wir so die Endlosschleife
      If Not onereaded Then Begin
        Abbruch('Error unknown value "' + Ftokens[FTokenPointer].Value + '" ' + Fglobalfilename + ' at line ');
      End;
    End;
    EatToken;
  End
  Else
    Abbruch('Error missing "{" ' + Fglobalfilename + ' at line ');
End;

Procedure TRayparser.EatRaytracerOptions;
Var
  ep, eu, ed: TVector3f;
  onereaded: Boolean;
  i, rd: Integer;
  fl: TRayFloat;
  bl, tr: TVector2f;
  z: TRayFloat;
Begin
  If FCancelParsing Then Begin
    Exit;
  End;
  EatToken;
  If Atoken = '{' Then Begin
    EatToken;
    // Alles Lesen was es Gibt
    While (Atoken <> '}') And (Not FCancelParsing) Do Begin
      onereaded := false;
      If atoken = 'fog' Then Begin
        onereaded := True;
        EatToken;
        FRaytracer.UseFog := EatBoolean;
      End;
      If atoken = 'fogmin' Then Begin
        onereaded := True;
        EatToken;
        If atoken = '=' Then Begin
          EatToken;
          fl := EatSingle;
          If fl >= 0 Then
            FRaytracer.FogMin := fl
          Else
            Abbruch('Error Fogmin has to be >= 0 ' + Fglobalfilename + ' at line ');
          EatSemicolon;
        End
        Else
          Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
      End;
      If atoken = 'fogcolor' Then Begin
        onereaded := True;
        EatToken;
        If atoken = '=' Then Begin
          Eattoken;
          FRaytracer.FogColor := EatVector3f;
          fl := Min(FRaytracer.FogColor.x, min(FRaytracer.FogColor.y, FRaytracer.FogColor.z));
          If fl < 0 Then
            Abbruch('Error FogColor has invalid values ' + Fglobalfilename + ' at line ');
          EatSemicolon;
        End
        Else
          Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
      End;
      If atoken = 'fogmax' Then Begin
        onereaded := True;
        EatToken;
        If atoken = '=' Then Begin
          EatToken;
          fl := EatSingle;
          If fl >= 0 Then
            FRaytracer.FogMax := fl
          Else
            Abbruch('Error Fogmax has to be > 0 ' + Fglobalfilename + ' at line ');
          EatSemicolon;
        End
        Else
          Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
      End;
      If Atoken = 'fogmode' Then Begin
        onereaded := True;
        EatToken;
        If Atoken = '=' Then Begin
          Eattoken;
          If atoken = 'linear' Then Begin
            FRaytracer.FogMode := Linear;
            Eattoken;
            EatSemicolon;
          End
          Else If Atoken = 'cos' Then Begin
            FRaytracer.FogMode := Cos;
            Eattoken;
            EatSemicolon;
          End
          Else
            Abbruch('Error missing "linear" or "cos" ' + Fglobalfilename + ' at line ');
        End
        Else
          Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
      End;
      If atoken = 'mindistance' Then Begin
        onereaded := True;
        EatToken;
        If atoken = '=' Then Begin
          EatToken;
          fl := EatSingle;
          If Not FCancelParsing Then Begin
            If fl > 0 Then
              Minfloat := fl
            Else
              Abbruch('Error Mindistance has to be > 0 ' + Fglobalfilename + ' at line ');
          End;
          EatSemicolon;
        End
        Else
          Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
      End;
      If atoken = 'maxdistance' Then Begin
        onereaded := True;
        EatToken;
        If atoken = '=' Then Begin
          EatToken;
          fl := EatSingle;
          If Not FCancelParsing Then Begin
            If fl > 0 Then
              Maxfloat := fl
            Else
              Abbruch('Error Maxdistance has to be > 0 ' + Fglobalfilename + ' at line ');
          End;
          EatSemicolon;
        End
        Else
          Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
      End;
      If atoken = 'imagewidth' Then Begin
        onereaded := True;
        EatToken;
        i := EatInteger;
        If Not FCancelParsing Then Begin
          If i > 0 Then
            Imagewidth := i
          Else
            Abbruch('Error Imagewidth has to be > 0 ' + Fglobalfilename + ' at line ');
        End;
        EatSemicolon;
      End;
      If atoken = 'imageheight' Then Begin
        onereaded := True;
        EatToken;
        i := EatInteger;
        If Not FCancelParsing Then Begin
          If i > 0 Then
            imageheight := i
          Else
            Abbruch('Error Imageheight has to be > 0 ' + Fglobalfilename + ' at line ');
        End;
        EatSemicolon;
      End;
      // Einlesen der LightOverflowMode Einstellung
      If Atoken = 'lightoverflowmode' Then Begin
        onereaded := True;
        EatToken;
        If AToken = '=' Then Begin
          EatToken;
          If Atoken = 'clamp' Then Begin
            FRaytracer.LightOverflowMode := Clamp;
            EatToken;
            EatSemicolon;
          End
          Else If Atoken = 'scale' Then Begin
            FRaytracer.LightOverflowMode := Scale;
            EatToken;
            EatSemicolon;
          End
          Else
            Abbruch('Error missing "clamp" or "scale" ' + Fglobalfilename + ' at line ');
        End
        Else
          Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
      End;
      //        Ortho = ((0.0/0.0),(1.0/1.0),1);
      If Atoken = 'ortho' Then Begin
        onereaded := True;
        EatToken;
        If atoken = '=' Then Begin
          Eattoken;
          If atoken = '(' Then Begin
            EatToken;
            bl := Eatvector2f;
            If atoken = ',' Then Begin
              Eattoken;
              tr := Eatvector2f;
              If Atoken = ',' Then Begin
                Eattoken;
                z := EatSingle;
                If atoken = ')' Then Begin
                  Eattoken;
                  If Not FCancelParsing Then
                    FRaytracer.SetOrtho(bl, tr, z);
                  EatSemicolon;
                End
                Else
                  Abbruch('Error missing ")" ' + Fglobalfilename + ' at line ');
              End
              Else
                Abbruch('Error missing "," ' + Fglobalfilename + ' at line ');
            End
            Else
              Abbruch('Error missing "," ' + Fglobalfilename + ' at line ');
          End
          Else
            Abbruch('Error missing "(" ' + Fglobalfilename + ' at line ');
        End
        Else
          Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
      End;
      // Einlesen der OrthoProjection Einstellung
      If atoken = 'orthoprojection' Then Begin
        onereaded := True;
        EatToken;
        FRaytracer.UseOrthoProjection := EatBoolean;
      End;
      // Einlesen der UseCullFaceing Einstellung
      If atoken = 'cullfaceing' Then Begin
        onereaded := True;
        EatToken;
        FRaytracer.UseCullfaceing := EatBoolean;
      End;
      If Atoken = 'cullfacemode' Then Begin
        onereaded := True;
        EatToken;
        If Atoken = '=' Then Begin
          Eattoken;
          If atoken = 'front' Then Begin
            FRaytracer.CullFaceMode := FrontFace;
            Eattoken;
            EatSemicolon;
          End
          Else If Atoken = 'back' Then Begin
            FRaytracer.CullFaceMode := BackFace;
            Eattoken;
            EatSemicolon;
          End
          Else
            Abbruch('Error missing "front" or "back" ' + Fglobalfilename + ' at line ');
        End
        Else
          Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
      End;
      If Atoken = 'backgroundcolor' Then Begin
        onereaded := True;
        EatToken;
        If atoken = '=' Then Begin
          Eattoken;
          FRaytracer.BackgroundColor := eatvector3f;
        End
        Else
          Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
        EatSemicolon;
      End;
      If Atoken = 'rekursiondepth' Then Begin
        onereaded := True;
        EatToken;
        rd := EatInteger;
        If rd >= 0 Then
          FRaytracer.RekursionDepth := rd
        Else
          Abbruch('Error Rekursiondepth has to be >= 0 ' + Fglobalfilename + ' at line ');
        EatSemicolon;
      End;
      If atoken = 'lookat' Then Begin
        onereaded := True;
        EatToken;
        If AToken = '=' Then Begin
          Eattoken;
          If atoken = '(' Then Begin
            Eattoken;
            ep := EatVector3f;
            If atoken = ',' Then Begin
              Eattoken;
              eu := EatVector3f;
              If atoken = ',' Then Begin
                Eattoken;
                ed := EatVector3f;
                If atoken = ')' Then Begin
                  Eattoken;
                  EatSemicolon;
                  If Not FCancelParsing Then
                    FRaytracer.LookAt(ep, eu, ed);
                End
                Else
                  Abbruch('Error missing ")" ' + Fglobalfilename + ' at line ');
              End
              Else
                Abbruch('Error missing "," ' + Fglobalfilename + ' at line ');
            End
            Else
              Abbruch('Error missing "," ' + Fglobalfilename + ' at line ');
          End
          Else
            Abbruch('Error missing "(" ' + Fglobalfilename + ' at line ');
        End
        Else
          Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
      End;
      If aToken = 'eye' Then Begin
        onereaded := True;
        EatToken;
        If AToken = '=' Then Begin
          Eattoken;
          If atoken = '(' Then Begin
            Eattoken;
            ep := EatVector3f;
            If atoken = ',' Then Begin
              Eattoken;
              eu := EatVector3f;
              If atoken = ',' Then Begin
                Eattoken;
                ed := EatVector3f;
                If atoken = ')' Then Begin
                  Eattoken;
                  EatSemicolon;
                  If Not FCancelParsing Then
                    FRaytracer.SetEye(ep, eu, ed);
                End
                Else
                  Abbruch('Error missing ")" ' + Fglobalfilename + ' at line ');
              End
              Else
                Abbruch('Error missing "," ' + Fglobalfilename + ' at line ');
            End
            Else
              Abbruch('Error missing "," ' + Fglobalfilename + ' at line ');
          End
          Else
            Abbruch('Error missing "(" ' + Fglobalfilename + ' at line ');
        End
        Else
          Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
      End;
      // Wenn ein Gänzlich unbekannter Token kommt verhindern wir so die Endlosschleife
      If Not onereaded Then Begin
        Abbruch('Error unknown value "' + Ftokens[FTokenPointer].Value + '" ' + Fglobalfilename + ' at line ');
      End;
    End;
    EatToken;
  End
  Else
    Abbruch('Error missing "{" ' + Fglobalfilename + ' at line ');
End;

Procedure TRayparser.EatLight;
Var
  onereaded: Boolean;
  Flight: tlight;
  fl: TRayFloat;
  Defined: Boolean;
Begin
  If FCancelParsing Then Exit;
  Defined := False;
  Flight := Tlight.create;
  EatToken;
  If Atoken = '{' Then Begin
    EatToken;
    // Alles Lesen was es Gibt
    While (Atoken <> '}') And (Not FCancelParsing) Do Begin
      onereaded := false;
      // Auslesen der Positionsdaten
      If Atoken = 'position' Then Begin
        Defined := True;
        onereaded := True;
        Eattoken;
        If atoken = '=' Then Begin
          Eattoken;
          Flight.Position := EatVector3f;
          EatSemicolon;
        End
        Else
          Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
      End;
      // Auslesen ob die Lichtquelle Active ist.
      If atoken = 'active' Then Begin
        onereaded := True;
        Eattoken;
        Flight.IsActive := EatBoolean;
      End;
      // Auslesen der Ambienten Farbe
      If atoken = 'ambientcolor' Then Begin
        onereaded := True;
        Eattoken;
        If atoken = '=' Then Begin
          Eattoken;
          Flight.material.AmbientColor := EatVector3f;
          fl := Min(Flight.material.AmbientColor.x, min(Flight.material.AmbientColor.y, Flight.material.AmbientColor.z));
          If fl < 0 Then
            Abbruch('Error AmbientColor has invalid values ' + Fglobalfilename + ' at line ');
          EatSemicolon;
        End
        Else
          Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
      End;
      // Auslesen der Diffusen Farbe
      If atoken = 'diffusecolor' Then Begin
        onereaded := True;
        Eattoken;
        If atoken = '=' Then Begin
          Eattoken;
          Flight.material.DiffuseColor := EatVector3f;
          fl := Min(Flight.material.DiffuseColor.x, min(Flight.material.DiffuseColor.y, Flight.material.DiffuseColor.z));
          If fl < 0 Then
            Abbruch('Error DiffuseColor has invalid values ' + Fglobalfilename + ' at line ');
          EatSemicolon;
        End
        Else
          Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
      End;
      // Auslesen der Specularen Farbe
      If atoken = 'specularcolor' Then Begin
        onereaded := True;
        Eattoken;
        If atoken = '=' Then Begin
          Eattoken;
          Flight.material.SpecularColor := EatVector3f;
          fl := Min(Flight.material.SpecularColor.x, min(Flight.material.SpecularColor.y, Flight.material.SpecularColor.z));
          If fl < 0 Then
            Abbruch('Error SpecularColor has invalid values ' + Fglobalfilename + ' at line ');
          EatSemicolon;
        End
        Else
          Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
      End;
      // Wenn ein Gänzlich unbekannter Token kommt verhindern wir so die Endlosschleife
      If Not onereaded Then Begin
        Abbruch('Error unknown value "' + Ftokens[FTokenPointer].Value + '" ' + Fglobalfilename + ' at line ');
      End;
    End;
    If Not Defined Then
      Abbruch('Error Light was not definied ' + Fglobalfilename + ' ');
    EatToken;
  End
  Else
    Abbruch('Error missing "{" ' + Fglobalfilename + ' at line ');
  // Bei erfolgreichem Parsing wird die Lichtquelle geadded.
  If Not FCancelParsing Then
    FRaytracer.AddPrimitive(Flight);
  // Danach kann sie wieder zerstört werden.
  Flight.free;
End;

Function TRayparser.EatTriangle: TTriangle;
Var
  onereaded: Boolean;
  ps: Array[0..2] Of TVector3f;
  Translation: TVector3f;
  rotation: TVector4f;
  defined: Boolean;
  Material: TmaterialData;
Begin
  If FCancelParsing Then Begin
    Result := Nil;
    Exit;
  End;
  Result := TTriangle.create;
  defined := False; // True nachdem die Punkte übergeben wurden.
  EatToken;
  If Atoken = '{' Then Begin
    EatToken;
    // Alles Lesen was es Gibt
    While (Atoken <> '}') And (Not FCancelParsing) Do Begin
      onereaded := false;
      If aToken = 'points' Then Begin
        onereaded := True;
        EatToken;
        If Atoken = '=' Then Begin
          EatToken;
          If Atoken = '(' Then Begin
            EatToken;
            ps[0] := EatVector3f;
            If Atoken = ',' Then Begin
              Eattoken;
              ps[1] := EatVector3f;
              If Atoken = ',' Then Begin
                Eattoken;
                ps[2] := EatVector3f;
                If Atoken = ')' Then Begin
                  Eattoken;
                  EatSemicolon;
                  // Punkte wurden Erfolgreich eingelesen
                  If Not FCancelParsing Then Begin
                    Defined := True;
                    Result.Define(Ps[0], ps[1], ps[2]);
                  End;
                End
                Else
                  Abbruch('Error missing ")" ' + Fglobalfilename + ' at line ');
              End
              Else
                Abbruch('Error missing "," ' + Fglobalfilename + ' at line ');
            End
            Else
              Abbruch('Error missing "," ' + Fglobalfilename + ' at line ');
          End
          Else
            Abbruch('Error missing "(" ' + Fglobalfilename + ' at line ');
        End
        Else
          Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
      End;
      If Atoken = 'rotate' Then Begin
        If defined Then Begin
          onereaded := True;
          Eattoken;
          If Atoken = '=' Then Begin
            Eattoken;
            rotation := eatVector4f;
            EatSemicolon;
            If Not FCancelParsing Then Begin
              result.Rotate(Rotation.x, rotation.y, rotation.z, rotation.w);
            End;
          End
          Else
            Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
        End
        Else
          Abbruch('Rotation only after point declaration allowed ' + Fglobalfilename + ' at line ');
      End;
      If atoken = 'translate' Then Begin
        If defined Then Begin
          onereaded := True;
          Eattoken;
          If atoken = '=' Then Begin
            EatToken;
            Translation := EatVector3f;
            EatSemicolon;
            If Not FCancelParsing Then Begin
              result.Translate(Translation);
            End;
          End
          Else
            Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
        End
        Else
          Abbruch('Translation only after point declaration allowed ' + Fglobalfilename + ' at line ');
      End;
      If atoken = 'material' Then Begin
        onereaded := True;
        Material := EatMaterial(3, false, True);
        If Not FCancelParsing Then Begin
          Result.Material := Material.Material;
          If high(Material.TexCoords) <> -1 Then Begin
            Result.TextureCoord[0] := Material.TexCoords[0];
            Result.TextureCoord[1] := Material.TexCoords[1];
            Result.TextureCoord[2] := Material.TexCoords[2];
            setlength(Material.TexCoords, 0);
          End;
        End;
      End;
      // Wenn ein Gänzlich unbekannter Token kommt verhindern wir so die Endlosschleife
      If Not onereaded Then Begin
        Abbruch('Error unknown value "' + Ftokens[FTokenPointer].Value + '" ' + Fglobalfilename + ' at line ');
      End;
    End;
    If Not defined Then
      Abbruch('Error Triangle was not definied ' + Fglobalfilename + ' ');
    EatToken;
  End
  Else
    Abbruch('Error missing "{" ' + Fglobalfilename + ' at line ');
  // Bei einem Fehler wird das Triangle wieder gelöscht
  If FCancelParsing Then Begin
    Result.free;
    result := Nil;
  End;
End;

Function TRayparser.EatSphere: TSphere;
Var
  Material: TmaterialData;
  rotation: TVector4f;
  Translation: TVector3f;
  ps: TVector3f;
  defined,
    pointread, radiusread,
    onereaded: Boolean;
  fl: TRayFloat;
Begin
  If FCancelParsing Then Begin
    Result := Nil;
    Exit;
  End;
  EatToken;
  Result := TSphere.create;
  pointread := false;
  radiusread := false;
  defined := False;
  fl := 0;
  ps := v3(0, 0, 0);
  If Atoken = '{' Then Begin
    EatToken;
    // Alles Lesen was es Gibt
    While (Atoken <> '}') And (Not FCancelParsing) Do Begin
      onereaded := false;
      If atoken = 'position' Then Begin
        onereaded := True;
        Eattoken;
        If Atoken = '=' Then Begin
          Eattoken;
          ps := EatVector3f;
          If Not FCancelParsing Then pointread := True;
          EatSemicolon;
        End
        Else
          Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
      End;
      If Atoken = 'radius' Then Begin
        onereaded := True;
        Eattoken;
        If atoken = '=' Then Begin
          Eattoken;
          fl := EatSingle;
          If Not FCancelParsing Then radiusread := True;
          EatSemicolon;
        End
        Else
          Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
      End;
      If (Not defined) And radiusread And pointread Then Begin
        defined := True;
        result.Define(ps, fl);
      End;
      If Atoken = 'rotate' Then Begin
        If defined Then Begin
          onereaded := True;
          Eattoken;
          If Atoken = '=' Then Begin
            Eattoken;
            rotation := eatVector4f;
            EatSemicolon;
            If Not FCancelParsing Then Begin
              result.Rotate(Rotation.x, rotation.y, rotation.z, rotation.w);
            End;
          End
          Else
            Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
        End
        Else
          Abbruch('Rotation only after point/ radius declaration allowed ' + Fglobalfilename + ' at line ');
      End;
      If atoken = 'translate' Then Begin
        If defined Then Begin
          onereaded := True;
          Eattoken;
          If atoken = '=' Then Begin
            EatToken;
            Translation := EatVector3f;
            EatSemicolon;
            If Not FCancelParsing Then Begin
              result.Translate(Translation);
            End;
          End
          Else
            Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
        End
        Else
          Abbruch('Translation only after point/radius declaration allowed ' + Fglobalfilename + ' at line ');
      End;
      If atoken = 'material' Then Begin
        onereaded := True;
        Material := EatMaterial(0, true, true);
        If Not FCancelParsing Then Begin
          Result.Material := Material.Material;
          result.material.SoftshadowBorder := Material.Material.SoftshadowBorder;
          result.material.SoftShadowMode := Material.Material.SoftShadowMode;
        End;
      End;
      // Wenn ein Gänzlich unbekannter Token kommt verhindern wir so die Endlosschleife
      If Not onereaded Then Begin
        Abbruch('Error unknown value "' + Ftokens[FTokenPointer].Value + '" ' + Fglobalfilename + ' at line ');
      End;
    End;
    If Not defined Then
      Abbruch('Error Sphere was not definied ' + Fglobalfilename + ' ');
    EatToken;
  End
  Else
    Abbruch('Error missing "{" ' + Fglobalfilename + ' at line ');

  If FCancelParsing Then Begin
    result.free;
    result := Nil;
  End;
End;

Function TRayparser.EatQuad: TQuad;
Var
  onereaded: Boolean;
  ps: Array[0..3] Of TVector3f;
  Translation: TVector3f;
  rotation: TVector4f;
  defined: Boolean;
  Material: TmaterialData;
Begin
  EatToken;
  defined := False;
  If FCancelParsing Then Begin
    Result := Nil;
    Exit;
  End;
  result := TQuad.create;
  If Atoken = '{' Then Begin
    EatToken;
    // Alles Lesen was es Gibt
    While (Atoken <> '}') And (Not FCancelParsing) Do Begin
      onereaded := false;
      If aToken = 'points' Then Begin
        onereaded := True;
        EatToken;
        If Atoken = '=' Then Begin
          EatToken;
          If Atoken = '(' Then Begin
            EatToken;
            ps[0] := EatVector3f;
            If Atoken = ',' Then Begin
              Eattoken;
              ps[1] := EatVector3f;
              If Atoken = ',' Then Begin
                Eattoken;
                ps[2] := EatVector3f;
                If Atoken = ',' Then Begin
                  Eattoken;
                  ps[3] := EatVector3f;
                  If Atoken = ')' Then Begin
                    Eattoken;
                    EatSemicolon;
                    // Punkte wurden Erfolgreich eingelesen
                    If Not FCancelParsing Then Begin
                      Defined := True;
                      Result.Define(Ps[0], ps[1], ps[2], ps[3]);
                    End;
                  End
                  Else
                    Abbruch('Error missing ")" ' + Fglobalfilename + ' at line ');
                End
                Else
                  Abbruch('Error missing "," ' + Fglobalfilename + ' at line ');
              End
              Else
                Abbruch('Error missing "," ' + Fglobalfilename + ' at line ');
            End
            Else
              Abbruch('Error missing "," ' + Fglobalfilename + ' at line ');
          End
          Else
            Abbruch('Error missing "(" ' + Fglobalfilename + ' at line ');
        End
        Else
          Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
      End;
      If Atoken = 'rotate' Then Begin
        If defined Then Begin
          onereaded := True;
          Eattoken;
          If Atoken = '=' Then Begin
            Eattoken;
            rotation := eatVector4f;
            EatSemicolon;
            If Not FCancelParsing Then Begin
              result.Rotate(Rotation.x, rotation.y, rotation.z, rotation.w);
            End;
          End
          Else
            Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
        End
        Else
          Abbruch('Rotation only after point declaration allowed ' + Fglobalfilename + ' at line ');
      End;
      If atoken = 'translate' Then Begin
        If defined Then Begin
          onereaded := True;
          Eattoken;
          If atoken = '=' Then Begin
            EatToken;
            Translation := EatVector3f;
            EatSemicolon;
            If Not FCancelParsing Then Begin
              result.Translate(Translation);
            End;
          End
          Else
            Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
        End
        Else
          Abbruch('Translation only after point declaration allowed ' + Fglobalfilename + ' at line ');
      End;
      If atoken = 'material' Then Begin
        onereaded := True;
        Material := EatMaterial(4, false, false);
        If Not FCancelParsing Then Begin
          Result.Material := Material.Material;
          If high(Material.TexCoords) <> -1 Then Begin
            Result.TextureCoord[0] := Material.TexCoords[0];
            Result.TextureCoord[1] := Material.TexCoords[1];
            Result.TextureCoord[2] := Material.TexCoords[2];
            Result.TextureCoord[3] := Material.TexCoords[3];
            setlength(Material.TexCoords, 0);
          End;
        End;
      End;
      // Wenn ein Gänzlich unbekannter Token kommt verhindern wir so die Endlosschleife
      If Not onereaded Then Begin
        Abbruch('Error unknown value "' + Ftokens[FTokenPointer].Value + '" ' + Fglobalfilename + ' at line ');
      End;
    End;
    If Not defined Then
      Abbruch('Error Quad was not definied ' + Fglobalfilename + ' ');
    EatToken;
  End
  Else
    Abbruch('Error missing "{" ' + Fglobalfilename + ' at line ');
  If FCancelParsing Then Begin
    Result.free;
    result := Nil;
  End;
End;

Function TRayparser.EatMaterial(TexParameterCount: integer; ShadowLineOption: Boolean; SmoothShadowOption: Boolean): TmaterialData;
Var
  Texloaded: Boolean;
  TexCoordsreaded: Boolean;
  onereaded: Boolean;
  Filename: String;
  bm: TLazIntfImage;
  fl: TRayFloat;
  i: integer;
Begin
  // Initialisieren
  TexCoordsreaded := false;
  Texloaded := False;
  setlength(result.TexCoords, 0);
  Result.material := InitialisedMaterial;
  If FCancelParsing Then Exit;
  // Starten des Parsens
  Eattoken;
  If Atoken = '{' Then Begin
    Eattoken;
    // Alles Lesen was es Gibt
    While (Atoken <> '}') And (Not FCancelParsing) Do Begin
      onereaded := false;
      If atoken = 'smoothshadow' Then Begin
        onereaded := true;
        Eattoken;
        If SmoothShadowOption Then Begin
          If atoken = '=' Then Begin
            Eattoken;
            result.material.SmoothShadow := EatSingle;
            fl := Min(result.material.EmissionColor.x, min(result.material.EmissionColor.y, result.material.EmissionColor.z));
            If fl < 0 Then
              Abbruch('Error smoothshadow has invalid values ' + Fglobalfilename + ' at line ');
            EatSemicolon;
          End
          Else
            Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
        End
        Else
          Abbruch('Error smoothshadow not supported vor this primitive ' + Fglobalfilename + ' at line ');
      End;
      If atoken = 'smoothshadowmode' Then Begin
        onereaded := true;
        Eattoken;
        If SmoothShadowOption Then Begin
          If atoken = '=' Then Begin
            Eattoken;
            If atoken = 'linear' Then Begin
              Eattoken;
              result.material.SmoothShadowMode := linear;
              EatSemicolon;
            End
            Else If atoken = 'cos' Then Begin
              Eattoken;
              result.material.SmoothShadowMode := cos;
              EatSemicolon;
            End
            Else
              Abbruch('Error missing "linear" or "cos" ' + Fglobalfilename + ' at line ');
          End
          Else
            Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
        End
        Else
          Abbruch('Error smoothshadowmode not supported vor this primitive ' + Fglobalfilename + ' at line ');
      End;

      If atoken = 'emissioncolor' Then Begin
        onereaded := true;
        Eattoken;
        If atoken = '=' Then Begin
          Eattoken;
          result.material.EmissionColor := EatVector3f;
          fl := Min(result.material.EmissionColor.x, min(result.material.EmissionColor.y, result.material.EmissionColor.z));
          If fl < 0 Then
            Abbruch('Error EmissionColor has invalid values ' + Fglobalfilename + ' at line ');
          EatSemicolon;
        End
        Else
          Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
      End;
      If atoken = 'shadowborder' Then Begin
        onereaded := true;
        Eattoken;
        If ShadowLineOption Then Begin
          If atoken = '=' Then Begin
            Eattoken;
            fl := EatSingle;
            If fl >= 0 Then Begin
              result.material.SoftshadowBorder := fl;
              EatSemicolon;
            End
            Else
              Abbruch('Error shadowborder value out of range ' + Fglobalfilename + ' at line ');
          End
          Else
            Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
        End
        Else
          Abbruch('Error shadowborder not supported vor this primitive ' + Fglobalfilename + ' at line ');
      End;
      If atoken = 'shadowbordermode' Then Begin
        onereaded := true;
        Eattoken;
        If ShadowLineOption Then Begin
          If atoken = '=' Then Begin
            Eattoken;
            If atoken = 'linear' Then Begin
              Eattoken;
              result.Material.SoftShadowMode := linear;
              EatSemicolon;
            End
            Else If atoken = 'cos' Then Begin
              Eattoken;
              result.Material.SoftShadowMode := cos;
              EatSemicolon;
            End
            Else
              Abbruch('Error missing "linear" or "cos" ' + Fglobalfilename + ' at line ');
          End
          Else
            Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
        End
        Else
          Abbruch('Error shadowbordermode not supported vor this primitive ' + Fglobalfilename + ' at line ');
      End;
      If atoken = 'transparencetexture' Then Begin
        onereaded := true;
        Eattoken;
        Filename := eatstring;
        If Not FCancelParsing Then Begin
          If FileExists(Filename) { *Converted from FileExists*  } Then Begin
            bm := FTextureManager.AddTexture(Filename);
            If assigned(bm) Then Begin
              Texloaded := True;
              result.Material.TransparentTexture := bm;
              result.Material.Shared_Texture := True;
            End
            Else
              Abbruch('Error Could not Load "' + Filename + '" ' + Fglobalfilename + ' at line ');
          End
          Else
            Abbruch('Error could not Load "' + Filename + '" ' + Fglobalfilename + ' at line ');
        End;
        EatSemicolon;
      End;
      If atoken = 'texture' Then Begin
        onereaded := true;
        Eattoken;
        Filename := eatstring;
        If Not FCancelParsing Then Begin
          If FileExists(Filename) { *Converted from FileExists*  } Then Begin
            bm := FTextureManager.AddTexture(Filename);
            If assigned(bm) Then Begin
              Texloaded := True;
              result.Material.Texture := bm;
              result.Material.Shared_Texture := True;
            End
            Else
              Abbruch('Error Could not Load "' + Filename + '" ' + Fglobalfilename + ' at line ');
          End
          Else
            Abbruch('Error could not Load "' + Filename + '" ' + Fglobalfilename + ' at line ');
        End;
        EatSemicolon;
      End;
      If Atoken = 'textureparameter' Then Begin
        onereaded := true;
        Eattoken;
        If atoken = '=' Then Begin
          Eattoken;
          If Atoken = 'clamp' Then Begin
            Eattoken;
            result.Material.TexParameter := ClampMode;
            EatSemicolon;
          End
          Else If atoken = 'repeat' Then Begin
            Eattoken;
            result.Material.TexParameter := RepeatMode;
            EatSemicolon;
          End
          Else
            Abbruch('Error missing "clamp" or "repeat" ' + Fglobalfilename + ' at line ');
        End
        Else
          Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
      End;
      If Atoken = 'textureinterpolation' Then Begin
        onereaded := true;
        Eattoken;
        If atoken = '=' Then Begin
          Eattoken;
          result.Material.TexInterpolationMode := EatTexInterpolationMode;
        End
        Else
          Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
      End;
      If Atoken = 'reflectance' Then Begin
        onereaded := True;
        eattoken;
        If Atoken = '=' Then Begin
          Eattoken;
          fl := eatSingle;
          If Not ((fl >= 0) And (fl <= 1.0)) Then
            Abbruch('Error reflectance value out of range ' + Fglobalfilename + ' at line ');
          If Not FCancelParsing Then Begin
            result.Material.Reflectance := fl;
          End;
          EatSemicolon;
        End
        Else
          Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
      End;
      If Atoken = 'transparence' Then Begin
        onereaded := True;
        eattoken;
        If Atoken = '=' Then Begin
          EatToken;
          fl := eatSingle;
          If Not ((fl >= 0) And (fl <= 1.0)) Then
            Abbruch('Error reflectance value out of range ' + Fglobalfilename + ' at line ');
          If Not FCancelParsing Then Begin
            result.Material.Transparence := fl;
          End;
          EatSemicolon;
        End
        Else
          Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
      End;
      // Auslesen der Ambienten Farbe
      If atoken = 'ambientcolor' Then Begin
        onereaded := True;
        Eattoken;
        If atoken = '=' Then Begin
          Eattoken;
          result.material.AmbientColor := EatVector3f;
          fl := Min(result.material.AmbientColor.x, min(result.material.AmbientColor.y, result.material.AmbientColor.z));
          If fl < 0 Then
            Abbruch('Error AmbientColor has invalid values ' + Fglobalfilename + ' at line ');
          EatSemicolon;
        End
        Else
          Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
      End;
      // Auslesen der Diffusen Farbe
      If atoken = 'diffusecolor' Then Begin
        onereaded := True;
        Eattoken;
        If atoken = '=' Then Begin
          Eattoken;
          result.material.DiffuseColor := EatVector3f;
          fl := Min(result.material.DiffuseColor.x, min(result.material.DiffuseColor.y, result.material.DiffuseColor.z));
          If fl < 0 Then
            Abbruch('Error DiffuseColor has invalid values ' + Fglobalfilename + ' at line ');
          EatSemicolon;
        End
        Else
          Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
      End;
      // Auslesen der Specularen Farbe
      If atoken = 'specularcolor' Then Begin
        onereaded := True;
        Eattoken;
        If atoken = '=' Then Begin
          Eattoken;
          result.material.SpecularColor := EatVector3f;
          fl := Min(result.material.SpecularColor.x, min(result.material.SpecularColor.y, result.material.SpecularColor.z));
          If fl < 0 Then
            Abbruch('Error SpecularColor has invalid values ' + Fglobalfilename + ' at line ');
          EatSemicolon;
        End
        Else
          Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
      End;
      If Atoken = 'highlightexponent' Then Begin
        onereaded := True;
        eattoken;
        If Atoken = '=' Then Begin
          Eattoken;
          fl := eatSingle;
          If Not FCancelParsing Then Begin
            If fl >= 0 Then
              result.Material.highlightexponent := fl
            Else
              Abbruch('Error HighlightExponent has to be >= 0 ' + Fglobalfilename + ' at line ');
          End;
          EatSemicolon;
        End
        Else
          Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
      End;
      If atoken = 'texturecoordinates' Then Begin
        If TexParameterCount = 0 Then Begin
          onereaded := True;
          eattoken;
          Abbruch('Error Primitive don''t support TextureCoordinates ' + Fglobalfilename + ' ');
        End
        Else Begin
          onereaded := True;
          eattoken;
          If atoken = '=' Then Begin
            Eattoken;
            If atoken = '(' Then Begin
              Eattoken;
              setlength(result.TexCoords, TexParameterCount);
              For i := 0 To TexParameterCount - 1 Do Begin
                If Not FCancelParsing Then Begin
                  result.TexCoords[i] := Eatvector2f;
                  If i <> (TexParameterCount - 1) Then Begin
                    If atoken = ',' Then
                      Eattoken
                    Else
                      Abbruch('Error missing "," ' + Fglobalfilename + ' at line ');
                  End;
                End;
              End;
              If atoken = ')' Then Begin
                Eattoken;
                EatSemicolon;
                If Not FcancelPArsing Then
                  TexCoordsreaded := True;
              End
              Else
                Abbruch('Error missing ")" ' + Fglobalfilename + ' at line ');
            End
            Else
              Abbruch('Error missing "(" ' + Fglobalfilename + ' at line ');
          End
          Else
            Abbruch('Error missing "=" ' + Fglobalfilename + ' at line ');
        End;
      End;
      // Wenn ein Gänzlich unbekannter Token kommt verhindern wir so die Endlosschleife
      If Not onereaded Then Begin
        Abbruch('Error unknown value "' + Ftokens[FTokenPointer].Value + '" ' + Fglobalfilename + ' at line ');
      End;
    End;
    If TexCoordsreaded And Not Texloaded Then
      Warning('Warning, Texturecoodinates are defined, but no Texture ' + Fglobalfilename + ' at line ');
    EatToken;
  End
  Else
    Abbruch('Error missing "{" ' + Fglobalfilename + ' at line ');
  // Löschen der Texturcoordinaten
  If FCancelParsing Then
    SetLength(result.TexCoords, 0);
End;

(*   das ist das Minimale um ein Object ein zu lesen !!
Procedure TRayparser.EatLight;
Var
  onereaded: Boolean;
Begin
  EatToken;
  If Atoken = '{' Then Begin
    EatToken;
    // Alles Lesen was es Gibt
    While (Atoken <> '}') And (Not FCancelParsing) Do Begin
      onereaded := false;

      // Wenn ein Gänzlich unbekannter Token kommt verhindern wir so die Endlosschleife
      If Not onereaded Then Begin
        Abbruch('Error unknown value "' + Ftokens[FTokenPointer].Value + '" ' + Fglobalfilename + ' at line ');
      End;
    End;
    EatToken;
  End
  Else
    Abbruch('Error missing "{" ' + Fglobalfilename + ' at line ');
End;
*)

End.

