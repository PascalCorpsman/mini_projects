(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Shader_Lab                                            *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
(*                                                                            *)
(* Shader Code Formatter                                                      *)
(*                                                                            *)
(* Code Implementation : GitHub Copilot                                       *)
(*                                                                            *)
(******************************************************************************)
Unit ushader_formator;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils;

Function FormatShaderProgramm(Const aShaderProgram: String): String;

Implementation

Function AddSpacesAroundOperators(Const aLine: String): String;
Var
  i: Integer;
  c, NextC, PrevC: Char;
  InString: Boolean;
  _Operator: String;
  Result2: String;
  CommentStart: Integer;
Begin
  Result2 := '';
  InString := False;
  CommentStart := 0;
  i := 1;

  // Finde Kommentar-Start
  While i <= Length(aLine) Do Begin
    If aLine[i] = '"' Then
      InString := Not InString;
    If Not InString And (i < Length(aLine)) And (aLine[i] = '/') And (aLine[i + 1] = '/') Then Begin
      CommentStart := i;
      Break;
    End;
    Inc(i);
  End;

  InString := False;
  i := 1;

  While i <= Length(aLine) Do Begin
    // Wenn wir beim Kommentar angekommen sind, den Rest unverändert anhängen
    If (CommentStart > 0) And (i = CommentStart) Then Begin
      Result2 := Result2 + Copy(aLine, i, MaxInt);
      Break;
    End;

    c := aLine[i];

    // String tracking
    If c = '"' Then
      InString := Not InString;

    If Not InString Then Begin
      NextC := #0;
      PrevC := #0;
      If i > 1 Then
        PrevC := aLine[i - 1];
      If i < Length(aLine) Then
        NextC := aLine[i + 1];

      // Zwei-Zeichen Operatoren (aber NICHT //)
      _Operator := '';
      If (i < Length(aLine)) And (Pos(c + NextC, '==!=<=>=&&||') > 0) Then Begin
        _Operator := c + NextC;
        // Space vor Operator
        If (Length(Result2) > 0) And (Result2[Length(Result2)] <> ' ') Then
          Result2 := Result2 + ' ';
        Result2 := Result2 + _Operator + ' ';
        Inc(i, 2);
        Continue;
      End;

      // Einzel-Zeichen Operatoren (aber nicht vor //)
      If Pos(c, '=+-*/%<>') > 0 Then Begin
        // Ausnahmen: + und - könnten Zahlenvorzeichen sein
        If (c = '+') Or (c = '-') Then Begin
          If (Length(Result2) > 0) And (PrevC <> ' ') And (PrevC <> '(') And (PrevC <> ',') Then Begin
            If Result2[Length(Result2)] <> ' ' Then
              Result2 := Result2 + ' ';
            Result2 := Result2 + c + ' ';
            Inc(i);
            Continue;
          End;
        End
        Else If c = '/' Then Begin
          // Nur bei / Spaces einfügen wenn es NICHT vor einem weiteren / ist
          If NextC <> '/' Then Begin
            If (Length(Result2) > 0) And (Result2[Length(Result2)] <> ' ') Then
              Result2 := Result2 + ' ';
            Result2 := Result2 + c + ' ';
            Inc(i);
            Continue;
          End;
        End
        Else Begin
          // =, *, %, <, > bekommen immer Spaces
          If (Length(Result2) > 0) And (Result2[Length(Result2)] <> ' ') Then
            Result2 := Result2 + ' ';
          Result2 := Result2 + c + ' ';
          Inc(i);
          Continue;
        End;
      End;
    End;

    Result2 := Result2 + c;
    Inc(i);
  End;

  // Mehrfach-Spaces reduzieren
  Result2 := StringReplace(Result2, '  ', ' ', [rfReplaceAll]);
  Result2 := Trim(Result2);
  Result := Result2;
End;

Procedure BreakLongCondition(Const Condition: String; Var Result: String;
  IndentLevel: Integer);
Var
  Parts: TStringList;
  i: Integer;
  CurrentPart: String;
  s: String;
Begin
  Parts := TStringList.Create;
  Try
    s := Condition;
    // Teile bei && und || auf
    s := StringReplace(s, ' && ', #10'&&'#10, [rfReplaceAll]);
    s := StringReplace(s, ' || ', #10'||'#10, [rfReplaceAll]);
    Parts.Text := s;

    For i := 0 To Parts.Count - 1 Do Begin
      CurrentPart := Trim(Parts[i]);
      If CurrentPart <> '' Then Begin
        If i = 0 Then
          Result := Result + CurrentPart + #10
        Else
          Result := Result + StringOfChar(' ', (IndentLevel + 1) * 2) + CurrentPart;

        If i < Parts.Count - 1 Then
          Result := Result + #10;
      End;
    End;
  Finally
    Parts.Free;
  End;
End;

Function FormatIfStatement(Const Statement: String; IndentLevel: Integer): String;
Var
  Condition: String;
  RestOfLine: String;
  BracketLevel: Integer;
  i: Integer;
  InString: Boolean;
  c: Char;
  ConditionLines: TStringList;
Begin
  ConditionLines := TStringList.Create;
  Try
    // Finde den Anfang der Bedingung
    i := Pos('(', Statement);
    If i = 0 Then Begin
      Result := StringOfChar(' ', IndentLevel * 2) + Statement;
      Exit;
    End;

    // Extrahiere die komplette Bedingung
    Condition := '';
    BracketLevel := 0;
    InString := False;

    For i := i To Length(Statement) Do Begin
      c := Statement[i];
      If c = '"' Then
        InString := Not InString;

      If Not InString Then Begin
        If c = '(' Then
          Inc(BracketLevel)
        Else If c = ')' Then Begin
          Dec(BracketLevel);
          Condition := Condition + c;
          If BracketLevel = 0 Then
            Break;
        End;
      End;
      Condition := Condition + c;
    End;

    RestOfLine := Trim(Copy(Statement, Length(Condition) + 1, MaxInt));

    // Wenn Bedingung kurz, auf eine Zeile
    If Length(Condition) < 50 Then Begin
      Result := StringOfChar(' ', IndentLevel * 2) + 'if ' + AddSpacesAroundOperators(Condition);
      If RestOfLine <> '' Then
        Result := Result + ' ' + RestOfLine;
    End
    Else Begin
      // Lange Bedingung: umbrechen bei && und ||
      Result := StringOfChar(' ', IndentLevel * 2) + 'if (';
      Condition := Copy(Condition, 2, Length(Condition) - 2); // Remove ( )
      BreakLongCondition(Condition, Result, IndentLevel);
      Result := Result + ')';
      If RestOfLine <> '' Then
        Result := Result + ' ' + RestOfLine;
    End;
  Finally
    ConditionLines.Free;
  End;
End;

Function FormatShaderProgramm(Const aShaderProgram: String): String;
Var
  Lines: TStringList;
  FormattedLines: TStringList;
  i: Integer;
  TrimmedLine: String;
  IndentLevel: Integer;
  j: Integer;
  c: Char;
  InString: Boolean;
  LastWasEmpty: Boolean;
  BraceSplit: TStringList;
  k: Integer;
  Part: String;
  InBlockComment: Boolean;
  HasBlockStart: Integer;
  HasBlockEnd: Integer;
Begin
  Lines := TStringList.Create;
  FormattedLines := TStringList.Create;
  BraceSplit := TStringList.Create;
  Try
    Lines.Text := aShaderProgram;
    IndentLevel := 0;
    LastWasEmpty := False;
    InBlockComment := False;

    For i := 0 To Lines.Count - 1 Do Begin
      TrimmedLine := Trim(Lines[i]);

      // Leere Zeilen überspringen (max. eine Leerzeile)
      If TrimmedLine = '' Then Begin
        If Not LastWasEmpty Then Begin
          FormattedLines.Add('');
          LastWasEmpty := True;
        End;
        Continue;
      End;
      LastWasEmpty := False;

      // Prüfe auf Block-Kommentar-Status
      HasBlockStart := Pos('/*', TrimmedLine);
      HasBlockEnd := Pos('*/', TrimmedLine);

      // Wenn wir bereits in einem Block-Kommentar sind
      If InBlockComment Then Begin
        FormattedLines.Add(StringOfChar(' ', IndentLevel * 2) + TrimmedLine);
        // Prüfe ob dieser Block endet
        If HasBlockEnd > 0 Then
          InBlockComment := False;
        Continue;
      End;

      // Neue Block-Kommentar startet hier
      If HasBlockStart > 0 Then Begin
        FormattedLines.Add(StringOfChar(' ', IndentLevel * 2) + TrimmedLine);
        // Wenn Block auf dieser Zeile nicht endet, setze Flag
        If HasBlockEnd = 0 Then
          InBlockComment := True;
        Continue;
      End;

      // Braces von Zeilen separieren (nur wenn nicht in Kommentar)
      BraceSplit.Clear;
      Part := '';
      InString := False;

      For j := 1 To Length(TrimmedLine) Do Begin
        c := TrimmedLine[j];

        // String-Status verfolgen
        If c = '"' Then
          InString := Not InString;

        If Not InString Then Begin
          If c = '{' Then Begin
            If Part <> '' Then
              BraceSplit.Add(Trim(Part));
            BraceSplit.Add('{');
            Part := '';
          End
          Else If c = '}' Then Begin
            If Part <> '' Then
              BraceSplit.Add(Trim(Part));
            BraceSplit.Add('}');
            Part := '';
          End
          Else Begin
            Part := Part + c;
          End;
        End
        Else Begin
          Part := Part + c;
        End;
      End;

      If Part <> '' Then
        BraceSplit.Add(Trim(Part));

      // Verarbeitete Teile formatieren
      For k := 0 To BraceSplit.Count - 1 Do Begin
        Part := BraceSplit[k];

        If Part = '{' Then Begin
          // Öffnende Brace auf neue Zeile, Indent erhöhen
          FormattedLines.Add(StringOfChar(' ', IndentLevel * 2) + '{');
          Inc(IndentLevel);
        End
        Else If Part = '}' Then Begin
          // Schließende Brace mit reduziertem Indent
          If IndentLevel > 0 Then
            Dec(IndentLevel);
          FormattedLines.Add(StringOfChar(' ', IndentLevel * 2) + '}');
        End
        Else If Part <> '' Then Begin
          // Normale Code-Zeile: Lange if-Bedingungen umbrechen
          If Pos('if ', LowerCase(Part)) = 1 Then
            FormattedLines.Add(FormatIfStatement(Part, IndentLevel))
          Else
            FormattedLines.Add(StringOfChar(' ', IndentLevel * 2) + AddSpacesAroundOperators(Part));
        End;
      End;
    End;

    Result := FormattedLines.Text;
  Finally
    Lines.Free;
    FormattedLines.Free;
    BraceSplit.Free;
  End;
End;

End.

