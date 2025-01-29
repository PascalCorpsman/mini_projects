(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Bingo                                                 *)
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
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  PrintersDlgs;

Type

  TBingoCard = Record
    Title: String;
    Numbers: Array Of boolean; // True = Die Zahl wird verwendet
    RowCount: Integer;
    ColCount: Integer;
    EmptyFieldsPerCol: Integer;
  End;

  { TForm2 }

  TForm2 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    PrintDialog1: TPrintDialog;
    ProgressBar1: TProgressBar;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
  private
    fCardTitle: String;
    fColumsPerCard: integer;
    fEmptyFieldPerColum: integer;
    fRowsPerCard: integer;
    fCardCount: integer;
    fCardsPerPage: integer;
    fCards: Array Of TBingoCard;
    Procedure PrintCard(tl, Dim: TPoint; Cardindex: integer);
    Procedure PrintCards(StartIndex: Integer; CardsPerPage: Integer);
    Procedure Clear;
  public

  End;

Var
  Form2: TForm2;

Implementation

{$R *.lfm}

Uses
  Printers, math, unit1;

Operator = (a, b: TBingoCard): Boolean;
Var
  i: Integer;
Begin
  result := true;
  For i := low(a.Numbers) To high(a.Numbers) Do Begin
    If a.Numbers[i] <> b.Numbers[i] Then Begin
      result := false;
      break;
    End;
  End;
End;


(*
 * Berechnet die Anzahl an Kombinationen, K Zahlen aus N zu nehmen
 * z.B.  P(49,6)= 13983816
 *
 *   (n)          n!
 *   (k)  =  -----------
 *           k! * (n-k)!
 *)

Function Binomial(N, K: integer): int64;
Var
  i: Integer;
  numerator, denominator: Array Of integer;
  nbool, denbool, ready: Boolean;
Begin
  numerator := Nil;
  denominator := Nil;
  result := 1;
  If (k = n) Or (k > n) Then Begin
    exit;
  End;
  setlength(numerator, n - 1);
  For i := 2 To n Do
    numerator[i - 2] := i;
  setlength(denominator, n - 2);
  For i := 2 To k Do
    denominator[i - 2] := i;
  For i := 2 To (n - k) Do
    denominator[k - 3 + i] := i;
  ready := false;
  While Not ready Do Begin
    nbool := false;
    denbool := false;
    For i := 0 To High(numerator) Do
      If numerator[i] <> 1 Then Begin
        result := result * numerator[i];
        numerator[i] := 1;
        nbool := True;
        break;
      End;
    For i := 0 To High(denominator) Do
      If denominator[i] <> 1 Then
        If result Mod denominator[i] = 0 Then Begin
          denbool := True;
          result := result Div denominator[i];
          denominator[i] := 1;
        End;
    ready := (Not nbool) And (Not denbool);
  End;
End;

{ TForm2 }

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  caption := 'Print cards';
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
  edit3.text := '';
  Edit4.text := '9';
  edit6.text := '4';
  edit5.text := '3';
  edit1.text := '4';
  edit2.text := '4';
  label7.caption := '';
  fCards := Nil;
End;

Procedure DrawCell(tl, Dim: TPoint; Text: String);
Var
  dx, dy: integer;
Begin
  If text = '' Then Begin
    printer.Canvas.Brush.Color := clBlack;
  End
  Else Begin
    printer.Canvas.Brush.Color := clWhite;
  End;
  printer.Canvas.Pen.Color := clblack;
  printer.Canvas.Rectangle(
    tl.X, tl.Y,
    tl.X + Dim.x, tl.Y + dim.y
    );
  If text <> '' Then Begin
    printer.Canvas.Brush.Color := clWhite;
    printer.Canvas.TextOut(
      tl.x + (dim.X - Printer.Canvas.TextWidth(text)) Div 2,
      tl.Y + (dim.Y - Printer.Canvas.TextHeight(text)) Div 2,
      text);
  End
  Else Begin
    // einen kleinen Zierrahmen ?
    dx := dim.x Div 10;
    dy := dim.y Div 10;
    printer.Canvas.Pen.Color := clWhite;
    printer.Canvas.RoundRect(
      tl.X + dx, tl.Y + dy,
      tl.X + Dim.x - dx, tl.Y + dim.y - dy,
      dx, dy
      );
  End;
End;

Procedure TForm2.PrintCard(tl, Dim: TPoint; Cardindex: integer);
Var
  yBase, d, rows, i, j, num: Integer;
  CellHeight, CellWidth: Single;
  rowSkipCounters: Array Of integer;
  ctl, cdim: TPoint;
  t: String;
Begin
  If Cardindex > high(fCards) Then exit;
  // Der Rahmen der Karte
  printer.Canvas.Pen.Color := clblack;
  Printer.Canvas.Brush.Color := clWhite;
  printer.Canvas.Rectangle(
    tl.X, tl.Y,
    tl.X + Dim.x, tl.Y + dim.y
    );
  CellWidth := dim.x / fCards[Cardindex].ColCount;
  If fCards[Cardindex].Title = '' Then Begin
    rows := fCards[Cardindex].RowCount;
  End
  Else Begin
    rows := fCards[Cardindex].RowCount + 1;
  End;
  CellHeight := dim.Y / rows;
  d := (round(min(CellWidth, CellHeight)) * 95) Div 100;
  Printer.Canvas.Font.Size := 1;
  While (Printer.Canvas.TextHeight(inttostr(MaxNumber)) < d) And
    (Printer.Canvas.TextWidth(inttostr(MaxNumber)) < d) Do Begin
    Printer.Canvas.Font.Size := Printer.Canvas.Font.Size + 1;
  End;
  Printer.Canvas.Font.Size := Printer.Canvas.Font.Size - 1;
  // Wenn es einen Titel gibt
  yBase := 0;
  If fCards[Cardindex].Title <> '' Then Begin
    Printer.Canvas.Brush.Color := clWhite;
    Printer.Canvas.TextOut(
      tl.X + (dim.x - Printer.Canvas.TextWidth(fCards[Cardindex].Title)) Div 2,
      tl.y + (round(CellHeight) - Printer.Canvas.TextHeight(fCards[Cardindex].Title)) Div 2,
      fCards[Cardindex].Title
      );
    yBase := round(CellHeight);
  End;
  cdim := point(
    round(CellWidth),
    round(CellHeight)
    );
  (*
  Die Karten wurden so Generiert, das in jeder Karte
  (fColumsPerCard - fEmptyFieldPerColum) * fRowsPerCard viele Zahlen "Aktiv" sind.
  *)
  rowSkipCounters := Nil;
  setlength(rowSkipCounters, fCards[Cardindex].RowCount);
  FillChar(rowSkipCounters[0], sizeof(rowSkipCounters[0]) * length(rowSkipCounters), 0);
  num := -1;
  For i := 0 To fCards[Cardindex].ColCount - 1 Do Begin
    For j := 0 To fCards[Cardindex].RowCount - 1 Do Begin
      If
        (fCards[Cardindex].EmptyFieldsPerCol <> 0) And (
        // Nach Wahrscheinlichkeit ein "Leeres" Feld setzen, wenn das gewünscht ist
        (
        (random(100) <= fCards[Cardindex].EmptyFieldsPerCol * 100 / fCards[Cardindex].ColCount) And
        (rowSkipCounters[j] < fCards[Cardindex].EmptyFieldsPerCol))
        // Oder Zwangsleer setzen weil es sonst hinten Raus nicht mehr reicht
        Or (i >= fCards[Cardindex].ColCount - (fCards[Cardindex].EmptyFieldsPerCol - rowSkipCounters[j]))
        )
        Then Begin
        t := '';
        rowSkipCounters[j] := rowSkipCounters[j] + 1;
      End
      Else Begin
        num := num + 1;
        While (Not fCards[Cardindex].Numbers[num]) Do Begin
          num := num + 1;
        End;
        t := inttostr(num + 1);
      End;
      ctl := point(
        round(i * CellWidth),
        round(j * CellHeight) + yBase
        );
      ctl := ctl + tl;
      DrawCell(ctl, cdim, t);
    End;
  End;
End;

Procedure TForm2.PrintCards(StartIndex: Integer; CardsPerPage: Integer);
Var
  aBorderWidth: Integer;
  CardWidth, CardHeight, i: integer;
  dim, tl: TPoint;
Begin
  aBorderWidth := (max(printer.PageWidth, printer.PageHeight) * 5) Div 100; // Geraten
  CardWidth := printer.PageWidth - 2 * aBorderWidth;
  CardHeight := (printer.PageHeight - (CardsPerPage + 1) * aBorderWidth) Div CardsPerPage;
  dim := point(CardWidth, CardHeight);
  For i := 0 To CardsPerPage - 1 Do Begin
    tl := point(aBorderWidth, aBorderWidth + (aBorderWidth + CardHeight) * i);
    PrintCard(tl, dim, StartIndex + i);
  End;
End;

Procedure TForm2.Clear;
Var
  i: Integer;
Begin
  For i := 0 To high(fCards) Do Begin
    setlength(fCards[i].Numbers, 0);
  End;
  setlength(fCards, 0);
End;

Procedure TForm2.Button1Click(Sender: TObject);
Var
  NumbersPerCard,
    index, i, count, j: Integer;
  needNew: Boolean;
Begin
  // 0. Init and Prechecks
  Clear;
  fCardTitle := edit3.text;
  fColumsPerCard := strtoint(edit4.text);
  fEmptyFieldPerColum := strtoint(edit6.text);
  fRowsPerCard := strtoint(edit5.text);
  fCardCount := strtoint(edit1.text);
  fCardsPerPage := strtoint(edit2.text);
  // Prechecks ob die Kartengenerierung überhaupt möglich ist.
  NumbersPerCard := fRowsPerCard * (fColumsPerCard - fEmptyFieldPerColum);
  If NumbersPerCard > MaxNumber Then Begin
    showmessage('Error, to much fields for cards, please increase "Empty field per colum" or decrease "Columns per card" or "Rows per card"');
    exit;
  End;
  If Binomial(MaxNumber, NumbersPerCard) < fCardCount Then Begin
    showmessage('Error, with the given setting it is not possible to create the requested number of cards');
    exit;
  End;

  // 1. Generieren aller Karten
  label7.caption := 'Generating cards.';
  ProgressBar1.Position := 0;
  ProgressBar1.Max := fCardCount;
  setlength(fcards, fCardCount);
  For i := 0 To high(fCards) Do Begin
    setlength(fCards[i].Numbers, MaxNumber);
    needNew := true;
    While needNew Do Begin
      // Alles Löschen
      FillChar(fCards[i].Numbers[0], sizeof(fCards[i].Numbers[0]) * MaxNumber, 0);
      fCards[i].Title := fCardTitle;
      fCards[i].ColCount := fColumsPerCard;
      fCards[i].RowCount := fRowsPerCard;
      fCards[i].EmptyFieldsPerCol := fEmptyFieldPerColum;
      // Zahlen "Generieren"
      For j := 0 To (fColumsPerCard - fEmptyFieldPerColum) * fRowsPerCard - 1 Do Begin
        index := Random(MaxNumber);
        While fCards[i].Numbers[index] Do Begin
          index := Random(MaxNumber);
        End;
        fCards[i].Numbers[index] := true;
      End;
      // Vergleichen ob auch ja keine 2 Gleichen Karten erstellt wurden
      needNew := false;
      For j := 0 To i - 1 Do Begin
        If fCards[i] = fCards[j] Then Begin
          needNew := true;
          break;
        End;
      End;
    End;
    // Update der LCL, dass der User auch nen Fortschritt sieht ;)
    ProgressBar1.Position := i;
    If i Mod fCardsPerPage = 0 Then Begin
      Application.ProcessMessages;
    End;
  End;
  // 2. Drucken der Generierten Karten
  Printer.Orientation := poPortrait;
  If PrintDialog1.Execute Then Begin
    // Printer.PrinterIndex wird über den Druckerdialog gesetzt ?
    Printer.Title := 'Bingo card creator';
    Printer.BeginDoc;
    count := fCardCount Div fCardsPerPage;
    If fCardCount Mod fCardsPerPage <> 0 Then Begin
      inc(count)
    End;
    ProgressBar1.Position := 0;
    ProgressBar1.Max := count - 1;
    Application.ProcessMessages;
    For i := 0 To count - 1 Do Begin
      PrintCards(i * fCardsPerPage, fCardsPerPage);
      If i <> count - 1 Then Begin
        Printer.NewPage;
      End;
      ProgressBar1.Position := i;
      Application.ProcessMessages;
    End;
    Printer.EndDoc;
  End;
End;

Procedure TForm2.Button2Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm2.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  Clear;
End;

End.

