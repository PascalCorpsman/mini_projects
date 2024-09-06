(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Freischichtkalender                                   *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit ufcal;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Graphics;

Type

  tCalenderRange = (crJanToJun, crJulToDec);

  (* Es Fehlt noch die "Markierung" von Ferien

    +-----------------+      +------------------------+
    |Wo Mo 1 | Neujahr|      | Wo  Mo  1  _c34 Neujahr|
    +        +--------+      +_c1 _c2 _c3       _c4   +
    |   Di 2 | Neujahr|
    +        +--------+
    |   Mi 3 | Neujahr|
    +        +--------+
    | 1 Do 4 | Neujahr|
    +        +--------+
    |   Fr 5 | Neujahr|
    +        +--------+
    |   Sa 6   Neujahr|
    +                 +
    |   So 7   Neujahr|
    +-----------------+
    |Wo Mo 1 | Neujahr|
    +        +--------+
    |   Di 2 | Neujahr|
    +-----------------+

    Zellenbreiten in mm : 4, 4, 4, 20
    Zellenhöhe in mm    : 4
  *)
  tDaySettings = Record
    Col1: String;
    Col2: String;
    Col3: String;
    Col4: String;
    DarkBack: Boolean;
    DarkC1: Boolean;
    _C1, _C2, _C3, _C4: Boolean;
    _C34: Boolean;
  End;

  TSubDate = Record
    Month: integer;
    Day: Integer;
    Title: String;
  End;

  (*
   * Aufbau des Kalenders
   * +-----------------------------------------
   * |               Überschrift
   * |
   * +----------------
   * | Monat   | Monat
   * | AT 21   | AT 21
   * +----------------
   * |
   * +----------------
   * |
   * +----------------
   * 6-Monate Breit
   * 31 + 4 Tage Hoch
   *)

  { TFCal }

  TFCal = Class
  private
    Procedure CheckHolidays(Month, Day: integer; Out Vacation: Boolean; Out
      Holiday: Boolean; Out Caption: String);
  public
    VacationDays: Array Of TSubDate;
    Holidays: Array Of TSubdate;
    LabeledDays: Array Of TSubdate;
    Year: integer;
    CalenderTitle: String;
    FootNote: String;
    Constructor Create;
    Destructor Destroy; override;
    Procedure RenderToCanvas(Const Canvas: TCanvas; Rect: TRect; Range: tCalenderRange);
  End;

Implementation

Uses math;

Const
  A4_Width_mm = 210;
  A4_Height_mm = 297;
  Col1_Width_mm = 4; // Spaltenbreiten C1 - C3
  Col2_Width_mm = 20; // Spaltenbreite C4
  MarginTop_mm = 25;
  DayHeight_mm = 3.2;
  LineWidth_mm = 0.25;
  Backgray = $E0E0E0; // Feiertags / Ferien Ausgrauung
  BackHoliday = $FFC855; // B G R

Function IntToDay(Day: integer): String;
Begin
  result := '';
  Case day Of
    0: result := 'Mo';
    1: result := 'Di';
    2: result := 'Mi';
    3: result := 'Do';
    4: result := 'Fr';
    5: result := 'Sa';
    6: result := 'So';
  End;
End;

Function InitDay(WDay: integer; Week: integer; Day: integer; Vacation, Holiday: Boolean; Caption: String = ''): tDaySettings; // Ein Wochentag
Begin
  result.Col1 := '';
  If wday = 0 Then result.Col1 := 'Wo'; // Immer Montags steht "Wo" = Woche
  If wday = 1 Then result.Col1 := inttostr(week); // Immer Dienstags steht die Wochennummer
  result.Col2 := Inttostr(day);
  result.Col3 := IntToDay(WDay);
  result.Col4 := Caption;
  result.DarkBack := false;
  result.DarkC1 := Vacation;
  result._C1 := false;
  result._C2 := false;
  result._C3 := false;
  //If wday = 5 Then Begin
    //result._C4 := false;
  //End
  //Else Begin
  result._C4 := true;
  //End;
  If wday = 6 Then Begin
    result._C1 := true;
    result._C2 := true;
    result._C3 := true;
  End;
  result._C34 := Caption = '';
  If Holiday Or (wday >= 5) Then Begin
    result.DarkBack := true;
    result._C34 := false;
  End;
End;

{ TFCal }

Procedure TFCal.CheckHolidays(Month, Day: integer; Out Vacation: Boolean; Out Holiday: Boolean; Out Caption: String);
Var
  i: Integer;
Begin
  Holiday := false;
  Vacation := false;
  Caption := '';
  For i := 0 To high(Holidays) Do Begin
    If (Holidays[i].Day = Day) And (Holidays[i].Month = Month) Then Begin
      Holiday := true;
      If Holidays[i].Title <> '' Then Begin
        Caption := Holidays[i].Title;
      End;
      break;
    End;
  End;
  For i := 0 To high(VacationDays) Do Begin
    If (VacationDays[i].Day = Day) And (VacationDays[i].Month = Month) Then Begin
      Vacation := true;
      If VacationDays[i].Title <> '' Then Begin
        Caption := VacationDays[i].Title;
      End;
      break;
    End;
  End;
  For i := 0 To high(LabeledDays) Do Begin
    If (LabeledDays[i].Day = Day) And (LabeledDays[i].Month = Month) Then Begin
      If LabeledDays[i].Title <> '' Then Begin
        Caption := LabeledDays[i].Title;
      End;
      break;
    End;
  End;
End;

Constructor TFCal.Create;
  Function ToSubDate(Day, Month: integer; Caption: String): TSubDate;
  Begin
    result.Day := Day;
    result.Month := Month;
    result.Title := Caption;
  End;

Begin
  setlength(Holidays, 5);
  Holidays[0] := ToSubDate(1, 1, 'Neujahr');
  Holidays[1] := ToSubDate(6, 1, 'Hl. 3 Könige');
  Holidays[2] := ToSubDate(1, 5, 'Tag der Arbeit');
  Holidays[3] := ToSubDate(25, 12, 'Weihnachten');
  Holidays[4] := ToSubDate(26, 12, 'Weihnachten');
  VacationDays := Nil;
  setlength(LabeledDays, 2);
  LabeledDays[0] := ToSubDate(24, 12, 'Hl. Abend');
  LabeledDays[1] := ToSubDate(31, 12, 'Sylvester');
End;

Destructor TFCal.Destroy;
Begin
  Inherited Destroy;
End;

Procedure TFCal.RenderToCanvas(Const Canvas: TCanvas; Rect: TRect;
  Range: tCalenderRange);
Var
  c1w, c2w, mt, ml, cw, ch: integer;
  at: Array[0..5] Of integer;

  Procedure TrimFontToWidth(w: integer);
  Const
    elems: Array[0..7] Of String = ('Mo', 'Di', 'Mi', 'Do', 'Fr', 'Sa', 'So', 'Wo');
  Var
    ss, s: String;
    i, j: integer;
  Begin
    // Finden des Breitesten textes der so Vorkommt
    // 1. Die Tage
    j := canvas.TextWidth('10');
    s := '10';
    For i := 11 To 31 Do Begin
      ss := inttostr(i);
      If Canvas.TextWidth(ss) > j Then Begin
        s := ss;
        j := Canvas.TextWidth(ss);
      End;
    End;
    // Die Beschriftungen
    For i := 0 To high(elems) Do Begin
      ss := elems[i];
      If Canvas.TextWidth(ss) >= j Then Begin
        s := ss;
        j := Canvas.TextWidth(ss);
      End;
    End;
    canvas.Font.Size := 12; // Irgendwas, nur nicht die "0" die es im default ist.
    While canvas.TextWidth(s) < w Do Begin
      canvas.Font.Size := canvas.Font.Size + 1;
    End;
    While canvas.TextWidth(s) > w Do Begin
      canvas.Font.Size := canvas.Font.Size - 1;
    End;
  End;

  Procedure TrimFontToHeight(h: integer);
  Var
    s: String;
  Begin
    s := 'Hj';
    canvas.Font.Size := 12; // Irgendwas, nur nicht die "0" die es im default ist.
    While canvas.TextHeight(s) < h Do Begin
      canvas.Font.Size := canvas.Font.Size + 1;
    End;
    While canvas.TextHeight(s) >= h Do Begin
      canvas.Font.Size := canvas.Font.Size - 1;
    End;
  End;

  Procedure RenderDayCell(x, y: Integer; Const Day: tDaySettings);
  Var
    t, l: integer;
  Begin
    t := mt + ch * y;
    l := ml + cw * x;
    If day.DarkBack Then Begin // Die Ganze Zelle ist Mit Hintergrund
      canvas.Pen.Color := BackGray;
      canvas.Brush.Color := BackGray;
      canvas.Brush.Style := bsSolid;
      canvas.Rectangle(l, t + 1, l + cw, t + ch + 1);
    End
    Else Begin
      canvas.Brush.Color := clWhite;
    End;
    If day.DarkC1 Then Begin // Ferien, die aber nicht unbedingt Feiertage sein müssen
      canvas.Pen.Color := BackHoliday;
      canvas.Brush.Color := BackHoliday;
      canvas.Brush.Style := bsSolid;
      canvas.Rectangle(l + c1w, t + 1, l + 2 * c1w, t + ch + 1);
    End
    Else Begin
      canvas.Brush.Color := clWhite;
    End;
    canvas.Brush.Style := bsClear;
    canvas.Pen.Color := clBlack;
    // Die beiden Linken und Rechten Begrenzungslinien sind immer da
    canvas.MoveTo(l, t);
    canvas.LineTo(l, t + ch);
    canvas.MoveTo(l + cw - 1, t);
    canvas.LineTo(l + cw - 1, t + ch);
    If day._C1 Then Begin
      canvas.MoveTo(l, t + ch);
      canvas.LineTo(l + c1w, t + ch);
    End;
    If day._C2 Then Begin
      canvas.MoveTo(l + c1w, t + ch);
      canvas.LineTo(l + 2 * c1w, t + ch);
    End;
    If day._C2 Then Begin
      canvas.MoveTo(l + 2 * c1w, t + ch);
      canvas.LineTo(l + 3 * c1w, t + ch);
    End;
    If day._C4 Then Begin
      canvas.MoveTo(l + 3 * c1w, t + ch);
      canvas.LineTo(l + 3 * c1w + c2w, t + ch);
    End;
    If day._C34 Then Begin
      canvas.MoveTo(l + 3 * c1w, t);
      canvas.LineTo(l + 3 * c1w, t + ch);
    End;
    If day.Col1 <> '' Then Begin
      // Rechtsbündig
      Canvas.TextOut(l + c1w - canvas.TextWidth(day.col1), t + (ch - canvas.TextHeight(day.col1)) Div 2, day.Col1);
    End;
    If day.Col2 <> '' Then Begin
      // Rechtsbündig
      Canvas.TextOut(l + 2 * c1w - canvas.TextWidth(day.col2), t + (ch - canvas.TextHeight(day.col2)) Div 2, day.Col2);
    End;
    If day.Col3 <> '' Then Begin
      // Linksbündig
      Canvas.TextOut(l + 2 * c1w, t + (ch - canvas.TextHeight(day.col3)) Div 2, day.Col3);
    End;
    If day.Col4 <> '' Then Begin
      // Linksbündig
      Canvas.TextOut(l + 3 * c1w, t + (ch - canvas.TextHeight(day.col4)) Div 2, day.Col4);
    End;
  End;

  Procedure RenderCaption();
  Var
    w, h: integer;
  Begin
    w := 6 * cw;
    h := 2 * ch;
    canvas.Rectangle(ml, mt, ml + w, mt + h);
    canvas.TextOut(ml + (w - Canvas.TextWidth(CalenderTitle)) Div 2, mt + (h - Canvas.TextHeight(CalenderTitle)) Div 2, CalenderTitle);
  End;

  Procedure RenderMonth();
  Const
    M1: Array[0..5] Of String = ('Januar', 'Februar', 'März', 'April', 'Mai', 'Juni');
    M2: Array[0..5] Of String = ('Juli', 'August', 'September', 'Oktober', 'November', 'Dezember');
  Var
    h, t, i: integer;
    s: String;
  Begin
    t := mt + 2 * ch;
    h := 2 * ch;
    For i := 0 To 5 Do Begin
      canvas.Rectangle(ml + i * cw, t, ml + (i + 1) * cw, t + h);
      // Rendern des Monatsnamens
      If range = crJanToJun Then Begin
        s := M1[i];
      End
      Else Begin
        s := M2[i];
      End;
      canvas.TextOut(ml + i * cw + (cw - canvas.TextWidth(s)) Div 2, t + (ch - canvas.TextHeight(s)) Div 2, s);
      s := 'AT ' + inttostr(at[i]);
      canvas.TextOut(ml + i * cw + (cw - canvas.TextWidth(s)) Div 2, t + ch + (ch - canvas.TextHeight(s)) Div 2, s);
    End;
  End;

  Procedure RenderDays();
  Var
    t, l, mo, d, i, j, w: integer;
    md: Array[0..5] Of Integer;
    c: String;
    v, hd: Boolean;
    day: tDaySettings;
  Begin
    d := DayOfWeek(EncodeDate(Year, 1, 1));
    d := (d + 5) Mod 7; // Konvertiere Sonntag = 1 zu Montag = 0
    // Quelle : https://www.aktuelle-kalenderwoche.org/
    If d <= 3 Then Begin // Die erste Kalenderwoche eines Jahres ist die Woche, die mindestens vier Tage des neuen Jahres beinhaltet.
      w := 1;
    End
    Else Begin
      w := 0; // 0- Stimmt natürlich nicht, aber nur so ist die Rechnung dann stimmig
    End;
    mo := 1;
    md[0] := 31;
    If IsLeapYear(Year) Then Begin
      md[1] := 29;
    End
    Else Begin
      md[1] := 28;
    End;
    md[2] := 31;
    md[3] := 30;
    md[4] := 31;
    md[5] := 30;
    If range = crJulToDec Then Begin
      mo := 7;
      // Weiter Zählen der Wochen und Tage auf 1.7. => das ginge sicherlich auch Eleganter ;)
      For i := 0 To 5 Do Begin
        For j := 1 To md[i] Do Begin
          d := (d + 1) Mod 7;
          If d = 0 Then inc(w);
        End;
      End;
      md[0] := 31;
      md[1] := 31;
      md[2] := 30;
      md[3] := 31;
      md[4] := 30;
      md[5] := 31;
    End;
    For i := 0 To 5 Do Begin
      For j := 1 To 31 Do Begin
        If j <= md[i] Then Begin
          CheckHolidays(i + mo, j, v, hd, c);
          day := InitDay(d, w, j, v, hd, c);
          If (range = crJanToJun) And (i = 0) Then Begin // 1.1 & 2.1 werden "gehackt" damit die auch die Woche anzeigen
            If (j = 1) And (d <= 5) Then Begin // 1.1. an Mo - Sa
              day.Col1 := 'Wo';
            End;
            If (j = 2) And (d <> 0) Then Begin // 2.1. Nicht an einem Montag
              If w = 0 Then Begin // Die Kalenderwoche ist noch die Nummer aus dem Vorjahr
                // Wenn der 2.1 ein Sa ist, dann hat das Vorjahr 53 Wochen, sost 52
                If d = 5 Then Begin // Stimmt das immer ?
                  day.Col1 := inttostr(53);
                End
                Else Begin
                  day.Col1 := inttostr(52);
                End;
              End
              Else Begin
                day.Col1 := inttostr(w);
              End;
            End;
          End;
          If (j = md[i]) And (d = 6) Then Begin
            day._C1 := false;
            day._C2 := false;
            day._C3 := false;
          End;
          RenderDayCell(i, j + 3, day);
          // Mit Zählen der Arbeitstage
          If (Not (hd)) And (d < 5) Then Begin
            at[i] := at[i] + 1;
          End;
          d := (d + 1) Mod 7;
          If d = 0 Then inc(w);
          //If (j = md[i]) And (j <> 31) Then Begin
          //  // Zeichnen des Abschließenden unteren Striches für den letzten im Monat
          //  t := mt + ch * (j + 3);
          //  l := ml + cw * i;
          //  canvas.MoveTo(l, t + ch);
          //  canvas.LineTo(l + cw - 1, t + ch);
          //End;
        End
        Else Begin
          t := mt + ch * (j + 3);
          l := ml + cw * i;
          canvas.MoveTo(l, t);
          canvas.LineTo(l, t + ch);
          canvas.MoveTo(l + cw - 1, t);
          canvas.LineTo(l + cw - 1, t + ch);
        End;
      End;
    End;
  End;
Var
  xmm, ymm: Double;
  i: integer;
Begin
  // Die Ganzen Skalierungen
  xmm := (rect.Right - Rect.Left) / A4_Width_mm;
  ymm := (rect.Bottom - Rect.Top) / A4_Height_mm;
  mt := round(MarginTop_mm * ymm) + Rect.Top; // Margin Top
  c1w := round(Col1_Width_mm * xmm); // Die Kleine Zellenbreite
  c2w := round(Col2_Width_mm * xmm); // Die Große Zellenbreite
  cw := 3 * c1w + c2w; // Die Gesamt Zellenbreite
  ch := round(DayHeight_mm * ymm); // Die Zellenhöhe
  ml := ((rect.Right - Rect.Left) - 6 * cw) Div 2 + rect.left; // Horizontal Zentriert
  canvas.Pen.Width := max(1, round(xmm * LineWidth_mm));
  // Das Eigentliche Befüllen
  TrimFontToHeight(2 * ch - 1);
  RenderCaption();
  For i := 0 To 5 Do Begin
    at[i] := 0;
  End;
  TrimFontToWidth(c1w);
  RenderDays(); // Berechnet die Arbeitstage Pro Monat noch mit
  //  TrimFontToHeight(ch);
  RenderMonth();
  // Lower Finish Line
  Canvas.MoveTo(ml, mt + 35 * ch);
  Canvas.LineTo(ml + 6 * cw, mt + 35 * ch);
  If FootNote <> '' Then Begin
    canvas.TextOut(ml, mt + 35 * ch + 1, FootNote);
  End;
End;

End.

