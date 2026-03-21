(******************************************************************************)
(* Sortdemo                                                        ??.??.???? *)
(*                                                                            *)
(* Version     : 0.03                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Interactive comparison of 4 different Integer Sort algorithms*)
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
(*               0.02 - ?                                                     *)
(*               0.03 - translate gui to english                              *)
(*                                                                            *)
(******************************************************************************)
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, ufilo;

Type

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Edit1: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    PaintBox1: TPaintBox;
    PaintBox2: TPaintBox;
    PaintBox3: TPaintBox;
    PaintBox4: TPaintBox;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button5Click(Sender: TObject);
    Procedure FormClose(Sender: TObject; Var CloseAction: TCloseAction);
    Procedure FormCreate(Sender: TObject);
    Procedure PaintBox1Paint(Sender: TObject);
    Procedure PaintBox2Paint(Sender: TObject);
    Procedure PaintBox3Paint(Sender: TObject);
    Procedure PaintBox4Paint(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  End;

Const
  cnt = 200;

Type
  TArr = Array[0..cnt - 1] Of integer; // Datentyp für die Array's

  (*
  Da Quicksort ein Rekursiver Algorithmus ist, ist es ein klein wenig aufwendiger
  wenn man ihn Schritt für Schritt simulieren will. D.h. man mus den Aufrufstack
  der Rekursion von Hand nach bauen, dazu wird TQuickrec benötigt.
  *)
  TQuickrec = Record
    qlo, qhi, ilo, ihi, mi: integer;
  End;

  TQuickRecStack = specialize TFiLo < TQuickrec > ;

Var
  Form1: TForm1;
  run: Boolean; // Wenn True dann läuft eine "simulation"
  HeapArr, BubbleArr, SelectArr, QuickArr: TArr; // Jedes Verfahren sortiert die selben Zahlen
  HeapFin, BubbleFin, SelectFin, QuickFin: Boolean; // True wenn, das jeweilige Verfahren "Fertig" ist.

  TMPc: TBitmap; // Double Buffered Rendering, damit wir kein Flackern haben

  // TMP Variablen für Bubblesort
  bubbleb: Boolean;
  bubblei, bubblej: Integer;
  bubblesteps: Integer = 0;

  // TMP Variablen für Selectionsort
  Seli, Selj: Integer;
  selsteps: Integer = 0;

  // TMP Variablen für Quicksort
  QuickStack: TQuickRecStack = Nil;
  Quicksteps: Integer = 0;

  // TMP Variablen für ButtomUpHeapSort
  HeapSteps: Integer = 0;
  HeapState: Integer;
  Heapx: Integer;
  SinkFromState, SinkIndex, SinkArraylength, SinkElement, SinkLinkesKind, SinkEinsinkindex, SinkRechtesKind, SinkVater: Integer;
  Sinkbool, Sink2Bool: Boolean;
  HeapB: integer;

Implementation

{$R *.lfm}

Procedure Push(qlo, qhi, iLo, iHi, mi: Integer);
Var
  p: TQuickrec;
Begin
  If assigned(QuickStack) Then Begin
    p.ilo := ilo;
    p.ihi := iHi;
    p.qlo := qlo;
    p.qhi := qHi;
    p.mi := mi;
    QuickStack.Push(p);
  End;
End;

Procedure Pop(Out qlo, qhi, iLo, iHi, mi: integer);
Var
  p: TQuickrec;
Begin
  If assigned(QuickStack) Then Begin
    p := QuickStack.Pop;
    ilo := p.ilo;
    iHi := p.ihi;
    qlo := p.qlo;
    qHi := p.qhi;
    mi := p.mi;
  End
  Else Begin
    ilo := 0;
    ihi := 0;
    mi := 0;
  End;
End;

Function isempty: boolean;
Begin
  If assigned(QuickStack) Then
    result := QuickStack.IsEmpty
  Else
    result := true;
End;

{ TForm1 }

Procedure PaintArr(Const Canvas: TCanvas; Const Arr: TArr);
Var
  i: Integer;
Begin
  // Löschen
  canvas.brush.color := clwhite;
  canvas.Brush.Style := bssolid;
  canvas.Rectangle(-1, -1, 201, 401);
  // Malen
  canvas.pen.Color := clblack;
  For i := 0 To cnt - 1 Do Begin
    canvas.MoveTo(0, i * 2);
    canvas.LineTo(arr[i], i * 2);
  End;
End;

Procedure TForm1.FormCreate(Sender: TObject);
Begin
  caption := 'Sort-Speed-Demo ver. 0.03 by Corpsman, support : www.Corpsman.de';
  Tform(self).Constraints.MaxHeight := Tform(self).Height;
  Tform(self).Constraints.MinHeight := Tform(self).Height;
  Tform(self).Constraints.Maxwidth := Tform(self).width;
  Tform(self).Constraints.Minwidth := Tform(self).width;
  tmpc := TBitmap.create;
  tmpc.PixelFormat := pf24bit;
  tmpc.Width := 200;
  tmpc.height := 400;
  randomize;
  QuickStack := TQuickRecStack.create;
  edit1.text := '1';
  button3.OnClick(Nil);
End;

Procedure TForm1.FormClose(Sender: TObject; Var CloseAction: TCloseAction);
Var
  dii, djj, di, dj, mi: Integer;
Begin
  If run Then Begin
    run := false;
    Application.ProcessMessages;
  End;
  TMPc.free;
  While Not isempty() Do
    pop(di, dj, dii, djj, mi);
  QuickStack.free;
  QuickStack := Nil;
End;

Procedure TForm1.PaintBox1Paint(Sender: TObject);
Begin
  PaintArr(tmpc.Canvas, BubbleArr);
  PaintBox1.Canvas.Draw(0, 0, tmpc);
End;

Procedure TForm1.PaintBox2Paint(Sender: TObject);
Begin
  PaintArr(tmpc.Canvas, SelectArr);
  PaintBox2.Canvas.Draw(0, 0, tmpc);
End;

Procedure TForm1.PaintBox3Paint(Sender: TObject);
Begin
  PaintArr(tmpc.Canvas, QuickArr);
  PaintBox3.Canvas.Draw(0, 0, tmpc);
End;

Procedure TForm1.PaintBox4Paint(Sender: TObject);
Begin
  PaintArr(tmpc.Canvas, HeapArr);
  PaintBox4.Canvas.Draw(0, 0, tmpc);
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  d: Integer;
Begin
  If Button1.Caption = '&Start' Then Begin
    // evtl. Neustart
    If BubbleFin And SelectFin And QuickFin And HeapFin Then Begin
      Button3.OnClick(Nil);
    End;
    run := true;
    Button1.Caption := '&Stop';
  End
  Else Begin
    Button1.Caption := '&Start';
    run := false;
  End;
  d := strtointdef(edit1.text, 10);
  While run Do Begin
    If d > 0 Then
      sleep(d);
    Button2.OnClick(Nil);
    If BubbleFin And SelectFin And QuickFin And HeapFin Then Begin
      Button1.Caption := '&Start';
      run := false;
    End;
  End;
End;

Procedure TForm1.Button2Click(Sender: TObject);
Var
  Lo, Hi, Mid, qlo, qhi, t: Integer;
  b: Boolean;
Begin
  (*
  Der Inhalt dieses Buttons ist normiert auf Vertauschungen
  1 Klick = 1 Tauschoperation in jeweils allen 4 verfahren
  *)
  // Neustart -- ACHTUNG DAS Funktioniert nur bei Einzelschritten, für die Animation ist es nochmal in Button1.Onclick
  If BubbleFin And SelectFin And QuickFin And HeapFin Then Begin
    Button3.OnClick(Nil);
  End;

  // Bubblesort
  If Not BubbleFin Then Begin
    inc(bubblesteps);
    b := true;
    While b Do Begin
      If BubbleArr[bubblej] < BubbleArr[bubblej - 1] Then Begin
        t := BubbleArr[bubblej];
        BubbleArr[bubblej] := BubbleArr[bubblej - 1];
        BubbleArr[bubblej - 1] := t;
        bubbleb := false;
        b := false;
      End;
      dec(bubblej);
      If bubblej = bubblei - 1 Then Begin
        bubblej := cnt - 1;
        inc(bubblei);
        If bubblei = cnt Then
          BubbleFin := true;
        If bubbleb Then
          BubbleFin := true;
        bubbleb := true;
      End;
      If BubbleFin Then
        b := false;
    End;
    If BubbleFin Then Begin
      Label1.Caption := Label1.Caption + ', ready [' + inttostr(bubblesteps) + '].';
    End;
    // Ausgabe
    PaintArr(TMPc.Canvas, BubbleArr);
    PaintBox1.Canvas.Draw(0, 0, TMPc);
  End;

  // Selectionsort
  If Not SelectFin Then Begin
    inc(selsteps);
    b := true;
    While b Do Begin
      //  for I := Low(A) to High(A) - 1 do
      //    for J := High(A) downto I + 1 do
      If SelectArr[selI] > SelectArr[selJ] Then Begin
        T := SelectArr[selI];
        SelectArr[selI] := SelectArr[selj];
        SelectArr[selj] := T;
        b := false;
      End;
      dec(selj);
      If selj <= seli Then Begin
        inc(seli);
        selj := cnt - 1;
        If seli = cnt - 1 Then Begin
          b := false;
          SelectFin := true;
        End;
      End;
    End;
    If SelectFin Then Begin
      Label2.Caption := Label2.Caption + ', ready [' + inttostr(selsteps) + '].';
    End;
    // Ausgabe
    PaintArr(TMPc.Canvas, SelectArr);
    PaintBox2.Canvas.Draw(0, 0, TMPc);
  End;

  // Quicksort
  If Not QuickFin Then Begin
    inc(Quicksteps);
    b := True;
    While b Do Begin
      If Not isempty Then Begin
        pop(qlo, qhi, lo, hi, mid);
        Repeat
          While QuickArr[Lo] < Mid Do
            Inc(Lo);
          While QuickArr[Hi] > Mid Do
            Dec(Hi);
          If Lo <= Hi Then Begin
            // Diese If und Ihr Else Teil sind normalerweise nicht
            // Aber ich wollte das nur "Echte" Tauschoperationen gezählt werden.
            If QuickArr[Lo] <> QuickArr[Hi] Then Begin
              b := false;
              T := QuickArr[Lo];
              QuickArr[Lo] := QuickArr[Hi];
              QuickArr[Hi] := T;
              push(qlo, qhi, lo, hi, mid);
            End
            Else Begin
              Inc(Lo);
              Dec(Hi);
            End;
          End;
        Until (Lo > Hi) Or (Not b);
        If b Then Begin
          If Hi > qLo Then Begin
            //          QuickSort(A, qLo, Hi);
            push(qlo, hi, qlo, hi, QuickArr[(qlo + hi) Div 2]);
          End;
          If Lo < qHi Then Begin
            //          QuickSort(A, Lo, qHi);
            push(lo, qhi, lo, qhi, QuickArr[(lo + qhi) Div 2]);
          End;
        End;
      End
      Else Begin
        QuickFin := true;
        b := false;
      End;
    End;
    If QuickFin Then Begin
      Label3.Caption := Label3.Caption + ', ready [' + inttostr(Quicksteps) + '].';
    End;
    // Ausgabe
    PaintArr(TMPc.Canvas, QuickArr);
    PaintBox3.Canvas.Draw(0, 0, TMPc);
  End;

  // Buttom Up Heap Sort
  If Not HeapFin Then Begin
    inc(HeapSteps);
    b := true;
    While b Do Begin
      Case HeapState Of
        0: Begin // Der Init Button wurde gedrückt
            Heapx := Trunc((high(HeapArr) - 1) / 2) + 1;
            HeapState := 1;
          End;
        1: Begin // Der Initialheap wird erzeugt
            heapx := heapx - 1;
            // Umschalten zum Eigentlichen Sortieren
            If Heapx = -1 Then Begin
              HeapState := 4;
              HeapX := High(HeapArr) + 1;
            End
            Else Begin
              // Aufruf Sink(Heapx, High(HeapArr);
              HeapState := 2;
              SinkArraylength := High(HeapArr);
              SinkIndex := HeapX;
              Sinkeinsinkindex := Sinkindex;
              SinkElement := HeapArr[Sinkindex];
              SinkBool := true;
              Sink2Bool := false;
              SinkFromState := 1;
            End;
          End;
        2: Begin
            //  Procedure Sink(Index, Arraylength: integer);
            While Sinkbool And b Do Begin // Suche des Einsinkpfades und gleichzeitiges Hochschieben aller Elemente.
              SinkLinkesKind := ((Sinkeinsinkindex) * 2) + 1;
              SinkRechtesKind := ((SinkEinsinkindex + 1) * 2);
              If SinkRechtesKind <= SinkArraylength Then Begin
                If HeapArr[SinkLinkeskind] < HeapArr[SinkRechteskind] Then Begin
                  HeapArr[SinkEinsinkindex] := HeapArr[SinkRechteskind];
                  SinkEinsinkindex := SinkRechteskind;
                  b := false;
                End
                Else Begin
                  If HeapArr[SinkEinsinkindex] <> HeapArr[SinkLinkeskind] Then
                    b := false;
                  HeapArr[SinkEinsinkindex] := HeapArr[SinkLinkeskind];
                  SinkEinsinkindex := SinkLinkeskind;
                End;
              End
              Else Begin
                Sinkbool := false;
                If SinkLinkeskind <= SinkArraylength Then Begin
                  If HeapArr[SinkEinsinkindex] <> HeapArr[SinkLinkeskind] Then
                    b := false;
                  HeapArr[SinkEinsinkindex] := HeapArr[SinkLinkeskind];
                  SinkEinsinkindex := SinkLinkeskind;
                End;
              End;
            End;
            // Nun wird das Element von Unten Hochgeschoben.
            If Not Sinkbool Then Begin
              If Heaparr[SinkEinsinkindex] <> SinkElement Then b := false;
              Heaparr[SinkEinsinkindex] := SinkElement;
              Sink2Bool := true;
              HeapState := 3;
            End;
          End;
        3: Begin
            While Sink2Bool And b Do Begin
              SinkVater := Trunc((SinkEinsinkindex - 1) / 2);
              If (HeapArr[SinkVater] < HeapArr[SinkEinsinkindex]) And (SinkVater >= SinkIndex) Then Begin
                If HeapArr[SinkVater] <> HeapArr[SinkEinsinkindex] Then
                  b := false;
                SinkElement := HeapArr[SinkVater];
                HeapArr[SinkVater] := HeapArr[SinkEinsinkindex];
                HeapArr[SinkEinsinkindex] := Sinkelement;
                SinkEinsinkindex := Sinkvater;
              End
              Else
                Sink2Bool := false;
            End;
            If Not Sink2Bool Then Begin
              // Sink ist Fertig "Rücksprung
              HeapState := SinkFromState;
            End;
          End;
        // Begin des Sortierens
        4: Begin
            dec(heapX);
            If HeapX = 0 Then Begin
              // Heapsort Fertig.
              b := false;
              HeapFin := true;
            End
            Else Begin
              //  For x := High(data) Downto 1 Do Begin
              If HeapArr[Heapx] <> HeapArr[0] Then
                b := false;
              Heapb := HeapArr[Heapx];
              Heaparr[Heapx] := HeapArr[0];
              HeapArr[0] := Heapb;
              SinkIndex := 0;
              SinkArraylength := HeapX - 1;
              SinkFromState := 4;
              SinkBool := true;
              Sink2Bool := false;
              Sinkeinsinkindex := Sinkindex;
              SinkElement := HeapArr[Sinkindex];
              SinkBool := true;
              Sink2Bool := false;
              HeapState := 2;
              //    sink(0, x - 1);
            End;
          End;
      End;
    End;
    // Ausgabe
    If HeapFin Then Begin
      Label5.Caption := Label5.Caption + ', ready [' + inttostr(HeapSteps) + '].';
    End;
    PaintArr(TMPc.Canvas, HeapArr);
    PaintBox4.Canvas.Draw(0, 0, TMPc);
  End;
  Application.ProcessMessages;
End;

Procedure TForm1.Button3Click(Sender: TObject);
Var
  i: Integer;
Begin
  run := false;
  Application.ProcessMessages;
  For i := 0 To cnt - 1 Do Begin
    BubbleArr[i] := Random(PaintBox1.Width) + 1;
    SelectArr[i] := BubbleArr[i];
    QuickArr[i] := BubbleArr[i];
    HeapArr[i] := BubbleArr[i];
  End;
  // Bubblesort Init
  BubbleFin := false;
  bubblei := 1;
  bubblej := cnt - 1;
  bubbleb := true;
  label1.caption := 'Bubblesort';
  bubblesteps := 0;

  // Selectionsort Init
  SelectFin := false;
  Seli := 0;
  Selj := cnt - 1;
  label2.caption := 'Selectionsort';
  selsteps := 0;

  // Quicksort Init
  QuickFin := false;
  label3.caption := 'Quicksort';
  Quicksteps := 0;
  push(0, cnt - 1, 0, cnt - 1, QuickArr[(cnt - 1) Div 2]);

  // Buttom Up Heap Sort
  HeapFin := false;
  label5.caption := 'ButtomUpHeapsort';
  HeapSteps := 0;
  HeapState := 0;

  // Repaint
  PaintBox1.Refresh;
  PaintBox2.Refresh;
  PaintBox3.Refresh;
  PaintBox4.Refresh;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.Button5Click(Sender: TObject);
Var
  i: Integer;
Begin
  Button3Click(Nil);
  run := false;
  Application.ProcessMessages;
  For i := 0 To cnt - 1 Do Begin
    BubbleArr[i] := cnt - i;
    SelectArr[i] := BubbleArr[i];
    QuickArr[i] := BubbleArr[i];
    HeapArr[i] := BubbleArr[i];
  End;
  // Repaint
  PaintBox1.Refresh;
  PaintBox2.Refresh;
  PaintBox3.Refresh;
  PaintBox4.Refresh;
End;

End.

