(******************************************************************************)
(* Lazcomment                                                      08.09.2009 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : comments or uncomments selected textblocks                   *)
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
Unit ulazcomment;

{$MODE objfpc}{$H+}

Interface

Uses classes;

Procedure Comment(Const Lines: TStrings; SelStart, SelEnd: Integer; is_C_Code: Boolean = false);

Implementation

Procedure Comment(Const Lines: TStrings; SelStart, SelEnd: Integer; is_C_Code: Boolean = false);
Var
  b: Boolean;
  i, j, k: integer;
Begin
  If selend < selstart Then Begin
    i := selstart;
    selstart := selend;
    selend := i;
  End;
  If SelStart = SelEnd Then Begin
    // Eine einzige ganze Zeile Kommentieren -- Fertig, getestet.
    For i := Selstart Downto 1 Do
      If (Lines.text[i] = #10) Or (i = 1) Then Begin
        // wir haben nun zwar das crt aber die "Leerzeichen, am Anfang der Zeile müssen ignoriert werden."
        If (i <> 1) Then
          j := i + 1
        Else
          j := 1;
        While lines.text[j] = #32 Do
          inc(j);
        If (lines.text[j] = '/') And (lines.text[j + 1] = '/') Then Begin
          // Kommentar Entfernen
          Lines.text := copy(Lines.text, 1, j - 1) + copy(lines.text, j + 2, length(lines.text));
          exit;
        End
        Else Begin
          // Kommentar Einfügen
          Lines.text := copy(Lines.text, 1, i) + '//' + copy(lines.text, i + 1, length(lines.text));
          exit;
        End;
      End;
  End
  Else Begin
    // Es mus unterschieden werden, zwischen kommentar in einer Zeile, oder mehrere Zeilen Auskommentieren.
    b := false;
    For i := SelStart To SelEnd Do
      If lines.text[i] = #10 Then Begin
        b := True;
        break;
      End;
    If b Then Begin
      // Mehrere Zeilen wurden zum auskommentieren ausgewählt -- Fertig, getestet.
      (*
      Erst mal ermitteln, ob ein oder Auskommentiert werden mus.
      *)
      i := selstart;
      While lines.text[i] <> #10 Do
        dec(i);
      inc(i);
      While lines.text[i] = #32 Do
        inc(i);
      If (lines.text[i] = '/') And (lines.text[i + 1] = '/') Then Begin
        // Kommentar Entfernen
        i := SelEnd;
        j := selstart;
        While i >= j Do Begin
          If lines.text[i] = #10 Then Begin
            k := i + 1;
            While lines.text[k] = #32 Do
              inc(k);
            If (Lines.text[k] = '/') And (Lines.text[k + 1] = '/') Then Begin
              lines.text := copy(lines.text, 1, k - 1) + copy(lines.text, k + 2, length(lines.text));
            End;
          End;
          dec(i);
        End;
        While (i >= 1) And (lines.text[i] <> #10) Do Begin
          dec(i);
        End;
        k := i + 1;
        While lines.text[k] = #32 Do
          inc(k);
        If (Lines.text[k] = '/') And (Lines.text[k + 1] = '/') Then Begin
          lines.text := copy(lines.text, 1, k - 1) + copy(lines.text, k + 2, length(lines.text));
        End;
      End
      Else Begin
        // Kommentar Einfügen
        i := SelEnd;
        j := selstart;
        While i >= j Do Begin
          If lines.text[i] = #10 Then Begin
            lines.text := copy(lines.text, 1, i) + '//' + copy(lines.text, i + 1, length(lines.text));
          End;
          dec(i);
        End;
        i := j;
        While (i >= 1) And (lines.text[i] <> #10) Do Begin
          dec(i);
        End;
        lines.text := copy(lines.text, 1, i) + '//' + copy(lines.text, i + 1, length(lines.text));
      End;
    End
    Else Begin
      //Ein kurzer Teilstring in einer Zeile wurde ausgewählt und wird mit { } auskommentiert. -- Fertig, getestet.
      If is_C_Code Then Begin
        If (Lines.text[selstart] = '/') And (Lines.text[selstart + 1] = '*') And (Lines.text[selend - 2] = '*') And (Lines.text[selend - 1] = '/') Then Begin
          // Kommentar Entfernen
          lines.text := copy(lines.text, 1, selstart - 1) +
            copy(lines.text, Selstart + 2, selend - selstart - 4) +
            copy(lines.text, selend, length(lines.text));
        End
        Else Begin
          // Kommentar Einfügen
          Lines.text := copy(Lines.text, 1, selstart - 1) + '/*' +
            copy(lines.text, selstart, selend - selstart) + '*/' +
            copy(Lines.text, SelEnd, length(lines.text));
        End;
      End
      Else Begin
        If (Lines.text[selstart] = '{') And (Lines.text[selend - 1] = '}') Then Begin
          // Kommentar Entfernen
          lines.text := copy(lines.text, 1, selstart - 1) +
            copy(lines.text, Selstart + 1, selend - selstart - 2) +
            copy(lines.text, selend, length(lines.text));
        End
        Else Begin
          // Kommentar Einfügen
          Lines.text := copy(Lines.text, 1, selstart - 1) + '{' +
            copy(lines.text, selstart, selend - selstart) + '}' +
            copy(Lines.text, SelEnd, length(lines.text));
        End;
      End;
    End;
  End;
End;

End.

