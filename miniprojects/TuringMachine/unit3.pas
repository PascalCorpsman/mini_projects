(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of TuringMachine                                         *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit Unit3;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Menus, math;

Type

  TEdge = Record
    Angle1: integer; // Der Ausgangswinkel der Kante an Node 1
    Angle2: integer; // Der Ausgangswinkel der Kante an Node 2
    Node2: integer; // Die Ziel Node
    Node2Index: integer; // Index der Node 2 das spart dann den such aufwand beim Rendern ..
    Condition: String; // Die Kondition
    used: Boolean;
  End;

  TNode = Record
    Name: integer; // Der Name der Node
    Edges: Array Of TEdge; // Alle Kanten, welche von der Node weggehen
    Position: TPoint; // Die Position des Nodes
    used: Boolean; // Zum Erkennen, ob es diese Kante noch gibt
    isFinState: boolean; // Zeigt den endzustand an.
  End;


  { TForm3 }

  TForm3 = Class(TForm)
    Button1: TButton;
    Image1: TImage;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    PopupMenu1: TPopupMenu;
    SaveDialog1: TSaveDialog;
    Procedure Button1Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure Image1Paint(Sender: TObject);
    Procedure MenuItem1Click(Sender: TObject);
    Procedure MenuItem2Click(Sender: TObject);
    Procedure MenuItem4Click(Sender: TObject);
    Procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    Procedure PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure PaintBox1Resize(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    tmpCanvas: TBitmap;
    Nodes: Array Of TNode;
    //    Edges: Array Of TEdge;
    AktuelNode: integer;
    AktuelEdge: Tpoint;
    mpos: TPoint;
    Function RenderAlphaLine(p1, p2: Tpoint; A1, A2: integer): TPoint;
    Procedure UpdateDFA;
    Procedure RenderDFA;
    Procedure ClearGraph;
  End;

Var
  Form3: TForm3;

Implementation

{$R *.lfm}

Uses unit1;

{ TForm3 }

Function PointInRect(Const P, TL, BR: TPoint): Boolean;
Begin
  result :=
    (p.x <= max(tl.x, br.x)) And
    (p.x >= min(tl.x, br.x)) And
    (p.y <= max(tl.y, br.y)) And
    (p.y >= min(tl.y, br.y));
End;

Procedure TForm3.FormCreate(Sender: TObject);
Begin
  randomize;
  caption := 'Mealy machine - Editor';
  SaveDialog1.InitialDir := ExtractFilePath(ParamStr(0));
  tmpCanvas := TBitmap.Create;
  AktuelNode := -1;
  AktuelEdge := point(-1, -1);
End;

Procedure TForm3.FormDestroy(Sender: TObject);
Begin
  tmpCanvas.free;
  ClearGraph;
End;

Procedure TForm3.Image1Paint(Sender: TObject);
Begin
  RenderDFA;
End;

Procedure TForm3.MenuItem1Click(Sender: TObject);
Begin
  // Save image as
  If SaveDialog1.Execute Then Begin
    tmpCanvas.SaveToFile(SaveDialog1.FileName);
  End;
End;

Procedure TForm3.MenuItem2Click(Sender: TObject);
Var
  c, r, i: integer;
Begin
  // Order as circle
  r := min(Image1.Width Div 2, Image1.Height Div 2) - 15;
  c := high(Nodes) + 1;
  For i := 0 To high(Nodes) Do Begin
    nodes[i].Position := point(Image1.Width Div 2 + round(cos(degtorad(360 * i / c)) * r),
      Image1.Height Div 2 + round(sin(degtorad(360 * i / c)) * r));
  End;
  RenderDFA;
End;

Procedure TForm3.MenuItem4Click(Sender: TObject);
Begin
  showmessage('Click on "nodes" and use the mouse to move them.' + LineEnding + LineEnding +
    'Click on the "edge labels" and move the mouse' + LineEnding +
    'to change the enter and exit angles of the edge.');
End;

Procedure TForm3.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  i, j: integer;
  p: Tpoint;
Begin
  (*
   * Durch Klick auf die Kantenbeschriftung können die Winkel der ein und ausgänge dieser verändert werden
   * Durch Klich auf die Knoten können diese verschoben werden.
   *)
  mpos := point(x, y);
  For i := 0 To high(nodes) Do Begin

    // Auswahl einer Kante
    For j := 0 To high(nodes[i].Edges) Do Begin
      p := RenderAlphaLine(
        nodes[i].Position,
        nodes[Nodes[i].Edges[j].Node2Index].Position,
        Nodes[i].Edges[j].Angle1,
        Nodes[i].Edges[j].Angle2
        );
      If PointInRect(mpos,
        point(p.X - 15, p.y - 15),
        point(p.X + 15, p.y + 15)) Then Begin
        AktuelEdge.x := i;
        AktuelEdge.y := j;
        break;
      End;
    End;
    // Auswahl eines Kotens
    If PointInRect(mpos,
      point(nodes[i].Position.X - 15, nodes[i].Position.y - 15),
      point(nodes[i].Position.X + 15, nodes[i].Position.y + 15)) Then Begin
      AktuelNode := i;
      break;
    End;
  End;
  RenderDFA;
End;

Procedure TForm3.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
Var
  d: TPoint;
Begin
  If AktuelEdge.X <> -1 Then Begin
    d := point(mpos.X - x, mpos.y - y);
    Nodes[AktuelEdge.X].Edges[AktuelEdge.y].Angle1 :=
      Nodes[AktuelEdge.X].Edges[AktuelEdge.y].Angle1 + d.x;
    Nodes[AktuelEdge.X].Edges[AktuelEdge.y].Angle2 :=
      Nodes[AktuelEdge.X].Edges[AktuelEdge.y].Angle2 + d.y;
    mpos := point(x, y);
    RenderDFA();
  End;
  If AktuelNode <> -1 Then Begin
    nodes[AktuelNode].Position.x := nodes[AktuelNode].Position.x - (mpos.x - x);
    nodes[AktuelNode].Position.y := nodes[AktuelNode].Position.y - (mpos.y - y);
    mpos := point(x, y);
    RenderDFA();
  End;
End;

Procedure TForm3.PaintBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  AktuelNode := -1;
  AktuelEdge := point(-1, -1);
  RenderDFA;
End;

Procedure TForm3.PaintBox1Resize(Sender: TObject);
Begin
  Image1.Invalidate;
End;

Function TForm3.RenderAlphaLine(p1, p2: Tpoint; A1, A2: integer): TPoint;

  Function bezier(f: Single; p1, p2, p3, p4: TPoint): Tpoint;
  Var
    ip1, ip2, ip3, iip1, iip2: tpoint;
    dx, dy: single;
  Begin
    dx := p1.x - p2.x;
    dy := p1.y - p2.y;
    ip1 := point(round(p2.x + f * dx), round(p2.y + f * dy));
    dx := p2.x - p3.x;
    dy := p2.y - p3.y;
    ip2 := point(round(p3.x + f * dx), round(p3.y + f * dy));
    dx := p3.x - p4.x;
    dy := p3.y - p4.y;
    ip3 := point(round(p4.x + f * dx), round(p4.y + f * dy));
    dx := ip1.x - ip2.x;
    dy := ip1.y - ip2.y;
    iip1 := point(round(ip2.x + f * dx), round(ip2.y + f * dy));
    dx := ip2.x - ip3.x;
    dy := ip2.y - ip3.y;
    iip2 := point(round(ip3.x + f * dx), round(ip3.y + f * dy));
    dx := iip1.x - iip2.x;
    dy := iip1.y - iip2.y;
    result := point(round(iip2.x + f * dx), round(iip2.y + f * dy));
  End;

Const
  innerLen = 100;
Var
  Endpunkt, richtung, p3, p4: TPoint;
  i: integer;
  l: Float;
Begin
  p3 := point(
    p1.X + round(cos(degtorad(a1)) * innerLen),
    p1.y + round(sin(degtorad(a1)) * innerLen)
    );
  p4 := point(
    p2.X + round(cos(degtorad(a2)) * innerLen),
    p2.y + round(sin(degtorad(a2)) * innerLen)
    );
  richtung := point((p4.x - p2.x), (p4.y - p2.y));
  l := sqrt(sqr(richtung.x) + sqr(richtung.y));
  richtung.x := round(richtung.x * 15 / l);
  richtung.y := round(richtung.y * 15 / l);
  Endpunkt := point(p2.x + richtung.x, p2.y + richtung.y);

  result := bezier(1 / 2, p1, p3, p4, Endpunkt);

  tmpCanvas.Canvas.MoveTo(Endpunkt);
  For i := 1 To 20 Do Begin
    richtung := bezier(i / 20, p1, p3, p4, Endpunkt);
    tmpCanvas.Canvas.LineTo(richtung);
  End;
  // Die Pfeilspizte
  richtung.x := round(cos(degtorad(a2 + 45)) * 15);
  richtung.y := round(sin(degtorad(a2 + 45)) * 15);
  tmpCanvas.Canvas.MoveTo(Endpunkt);
  tmpCanvas.Canvas.lineTo(Endpunkt.x + richtung.x, Endpunkt.y + richtung.y);
  richtung.x := round(cos(degtorad(a2 - 45)) * 15);
  richtung.y := round(sin(degtorad(a2 - 45)) * 15);
  tmpCanvas.Canvas.MoveTo(Endpunkt);
  tmpCanvas.Canvas.lineTo(Endpunkt.x + richtung.x, Endpunkt.y + richtung.y);
End;

Procedure TForm3.UpdateDFA;
Var
  i, j, k, l: integer;
  astate, nstate: integer;
  rSymbol: String;
  b2, b: Boolean;
Begin
  AktuelNode := -1;
  AktuelEdge := point(-1, -1);
  // Init
  For i := 0 To high(Nodes) Do Begin
    nodes[i].used := false;
    For j := 0 To high(nodes[i].Edges) Do Begin
      nodes[i].Edges[j].used := false;
    End;
  End;
  // Einfügen, Bestätigen der Nodes + Einfügen der Kanten
  For i := 1 To form1.StringGrid1.RowCount - 1 Do Begin
    astate := strtoint(Form1.StringGrid1.Cells[1, i]);
    nstate := strtoint(Form1.StringGrid1.Cells[5, i]);
    rSymbol := Form1.StringGrid1.Cells[2, i] + '/(' + Form1.StringGrid1.Cells[4, i][1] + ' ' + Form1.StringGrid1.Cells[3, i] + ')';
    b := false;
    For j := 0 To high(Nodes) Do Begin
      // Einfügen des Quell State
      If astate = nodes[j].Name Then Begin
        nodes[j].used := true;
        b := true;
        b2 := false;
        For k := 0 To high(nodes[j].Edges) Do Begin
          // Node und Kante sind bekannt.
          If (nodes[j].Edges[k].Node2 = nstate) And
            (lowercase(nodes[j].Edges[k].Condition) = lowercase(rSymbol)) Then Begin
            nodes[j].Edges[k].used := true;
            b2 := true;
          End;
        End;
        // Es gibt zwar die Node, aber noch nicht die Abgehende Kannte
        If Not b2 Then Begin
          setlength(Nodes[j].Edges, high(Nodes[j].Edges) + 2);
          Nodes[j].Edges[high(Nodes[j].Edges)].used := true;
          Nodes[j].Edges[high(Nodes[j].Edges)].Condition := rSymbol;
          Nodes[j].Edges[high(Nodes[j].Edges)].Node2 := nstate;
          Nodes[j].Edges[high(Nodes[j].Edges)].Angle1 := random(360);
          Nodes[j].Edges[high(Nodes[j].Edges)].Angle2 := random(360);
        End;
        break;
      End;
    End;
    // Die Node gibt es noch gar nicht
    If Not b Then Begin
      setlength(Nodes, high(Nodes) + 2);
      nodes[high(nodes)].used := true;
      nodes[high(nodes)].Position := point(random(image1.Width - 40) + 20, random(image1.Height - 40) + 20);
      nodes[high(nodes)].Name := astate;
      setlength(Nodes[high(nodes)].Edges, high(Nodes[high(nodes)].Edges) + 2);
      Nodes[high(nodes)].Edges[high(Nodes[high(nodes)].Edges)].used := true;
      Nodes[high(nodes)].Edges[high(Nodes[high(nodes)].Edges)].Condition := rSymbol;
      Nodes[high(nodes)].Edges[high(Nodes[high(nodes)].Edges)].Node2 := nstate;
      Nodes[high(nodes)].Edges[high(Nodes[high(nodes)].Edges)].Angle1 := random(360);
      Nodes[high(nodes)].Edges[high(Nodes[high(nodes)].Edges)].Angle2 := random(360);
    End;
    // Einfügen des Ziel State
    b := false;
    For j := 0 To high(Nodes) Do Begin
      // Einfügen des Quell State
      If nstate = nodes[j].Name Then Begin
        nodes[j].used := true;
        b := true;
      End;
    End;
    If Not b Then Begin
      setlength(Nodes, high(Nodes) + 2);
      nodes[high(nodes)].used := true;
      nodes[high(nodes)].Position := point(random(image1.Width - 40) + 20, random(image1.Height - 40) + 20);
      nodes[high(nodes)].Name := nstate;
    End;
  End;
  // Löschen von unbenutzten Einträgen
  i := high(Nodes);
  While i >= 0 Do Begin
    If Not (Nodes[i].used) Then Begin
      // Löschen Aller Kanten auf das Node
      For j := 0 To High(nodes) Do Begin
        k := high(Nodes[j].Edges);
        While k >= 0 Do Begin
          // Die Kante zeigt auf den Node der gelöscht wird
          If nodes[j].Edges[k].Node2 = nodes[i].Name Then Begin
            For l := k To high(nodes[j].Edges) - 1 Do Begin
              nodes[j].Edges[l] := nodes[j].Edges[l + 1];
            End;
            setlength(nodes[j].Edges, high(nodes[j].Edges));
          End;
          dec(k);
        End;
      End;
      // Löschen des Nodes ansich
      setlength(nodes[i].Edges, 0); // Löschen der Gesamten Node Liste
      For k := i To high(Nodes) - 1 Do Begin
        nodes[k] := Nodes[k + 1];
      End;
      setlength(nodes, high(nodes));
    End;
    dec(i);
  End;
  // So nun da alles gelöscht und geändert wurde, mappen wir die Node2 um auf den entsprechenden Array index..
  For i := 0 To high(Nodes) Do Begin
    nodes[i].isFinState := Form1.SpinEdit1.Value = Nodes[i].Name;
    For j := 0 To high(nodes[i].Edges) Do Begin
      For k := 0 To high(Nodes) Do Begin
        If Nodes[i].Edges[j].Node2 = nodes[k].Name Then Begin
          Nodes[i].Edges[j].Node2Index := k;
          break;
        End;
      End;
    End;
  End;
End;

Procedure TForm3.RenderDFA;
Const
  border = 8;
Var
  i, k: integer;
  s: String;
  w, h: integer;
  p: Tpoint;
Begin
  If Not assigned(tmpCanvas) Then exit;
  tmpCanvas.BeginUpdate(True);
  tmpCanvas.Width := image1.Width;
  tmpCanvas.Height := image1.Height;
  tmpCanvas.Canvas.Pen.Color := clwhite;
  tmpCanvas.Canvas.Brush.Color := clwhite;
  tmpCanvas.Canvas.Brush.Style := bsSolid;
  tmpCanvas.Canvas.Rectangle(-1, -1, image1.Width + 1, image1.Height + 1);
  // rendern aller Kanten
  For i := 0 To high(nodes) Do Begin
    For k := 0 To high(nodes[i].Edges) Do Begin
      If (AktuelEdge.x = i) And (AktuelEdge.y = k) Then Begin
        tmpCanvas.Canvas.Pen.Color := clred;
      End
      Else Begin
        tmpCanvas.Canvas.Pen.Color := clblack;
      End;
      p := RenderAlphaLine(
        nodes[i].Position,
        nodes[Nodes[i].Edges[k].Node2Index].Position,
        Nodes[i].Edges[k].Angle1,
        Nodes[i].Edges[k].Angle2
        );
      s := Nodes[i].Edges[k].Condition;
      h := tmpCanvas.Canvas.TextHeight(s);
      w := tmpCanvas.Canvas.TextWidth(s);
      tmpCanvas.Canvas.TextOut(P.X - w Div 2, P.y - h Div 2, s);
    End;
  End;
  // rendern Aller Nodes
  For i := 0 To high(Nodes) Do Begin
    If AktuelNode = i Then Begin
      tmpCanvas.Canvas.Pen.Color := clred;
    End
    Else Begin
      tmpCanvas.Canvas.Pen.Color := clblack;
    End;
    If nodes[i].Name = form1.SpinEdit2.Value Then Begin
      tmpCanvas.Canvas.Brush.Color := clgreen;
    End
    Else Begin
      tmpCanvas.Canvas.Brush.Color := clwhite;
    End;
    s := inttostr(Nodes[i].Name);
    h := tmpCanvas.Canvas.TextHeight(s) + border;
    w := tmpCanvas.Canvas.TextWidth(s) + border;
    If Nodes[i].isFinState Then Begin
      tmpCanvas.Canvas.Ellipse(
        nodes[i].Position.X - w Div 2 - 3, nodes[i].Position.y - h Div 2 - 3,
        nodes[i].Position.X + w Div 2 + 3, nodes[i].Position.y + h Div 2 + 3);
    End;
    tmpCanvas.Canvas.Ellipse(
      nodes[i].Position.X - w Div 2, nodes[i].Position.y - h Div 2,
      nodes[i].Position.X + w Div 2, nodes[i].Position.y + h Div 2);
    If tmpCanvas.Canvas.Brush.Color = clgreen Then Begin
      tmpCanvas.Canvas.Font.Color := clwhite;
    End;
    tmpCanvas.Canvas.TextOut(nodes[i].Position.X - w Div 2 + border Div 2, nodes[i].Position.y - h Div 2 + border Div 2, s);
    tmpCanvas.Canvas.Font.Color := clblack;
    tmpCanvas.Canvas.Brush.Color := clwhite;
  End;
  tmpCanvas.EndUpdate();
  // Und Anzeigen..
  image1.Canvas.Draw(0, 0, tmpCanvas);
End;

Procedure TForm3.ClearGraph;
Var
  i: integer;
Begin
  For i := 0 To high(nodes) Do Begin
    setlength(Nodes[i].Edges, 0);
  End;
  setlength(nodes, 0);
End;

Procedure TForm3.Button1Click(Sender: TObject);
Begin
  close;
End;

End.

