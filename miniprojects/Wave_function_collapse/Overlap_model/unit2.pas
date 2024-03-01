Unit Unit2;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  ExtDlgs, Types;

Const
  Zoom = 10;

Type

  { TForm2 }

  TForm2 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
    ListBox1: TListBox;
    OpenPictureDialog1: TOpenPictureDialog;
    PaintBox1: TPaintBox;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
    Procedure ListBox1DrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    Procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    Procedure PaintBox1Paint(Sender: TObject);
  private
    img: TBitmap;
  public
    Grid: Array Of Array Of Record
      forced: Boolean;
      Col: Tcolor;
    End;
    MostCol: TColor;

    Procedure Init(Const Image: TBitmap; w, h: integer);
  End;

Var
  Form2: TForm2;

Implementation

{$R *.lfm}

Uses unit1;

{ TForm2 }

Procedure TForm2.ListBox1DrawItem(Control: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
Begin
  ListBox1.Canvas.Brush.Color := StringToColor(ListBox1.Items[index]);
  ListBox1.Canvas.Rectangle(
    ARect.Left + 3,
    ARect.Top + 3,
    //    ARect.Left + 3 + ARect.Bottom - ARect.Top - 6,
    ARect.Right - 3,
    ARect.Bottom - 3
    );
End;

Procedure TForm2.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  x := x Div Zoom;
  y := y Div zoom;
  If (x < 0) Or (x >= length(Grid)) Or (y < 0) Or (y >= length(Grid[x])) Then exit;
  If ssLeft In shift Then Begin
    Grid[x, y].Col := stringtocolor(ListBox1.Items[ListBox1.ItemIndex]);
    Grid[x, y].forced := true;
  End;
  If ssright In shift Then Begin
    Grid[x, y].forced := false;
  End;
  PaintBox1.Invalidate;
End;

Procedure TForm2.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
Begin
  x := x Div Zoom;
  y := y Div zoom;
  If (x < 0) Or (x >= length(Grid)) Or (y < 0) Or (y >= length(Grid[x])) Then exit;
  If ssLeft In shift Then Begin
    Grid[x, y].Col := stringtocolor(ListBox1.Items[ListBox1.ItemIndex]);
    Grid[x, y].forced := true;
  End;
  If ssright In shift Then Begin
    Grid[x, y].forced := false;
  End;
  PaintBox1.Invalidate;
End;

Procedure TForm2.PaintBox1Paint(Sender: TObject);
Var
  i, j: Integer;
Begin
  PaintBox1.Canvas.Draw(0, 0, img);
  For i := 0 To high(Grid) Do Begin
    For j := 0 To high(Grid) Do Begin
      If Grid[i, j].forced Then Begin
        PaintBox1.Canvas.Brush.Color := Grid[i, j].Col;
        PaintBox1.Canvas.Pen.Color := clred;
        PaintBox1.Canvas.Rectangle(i * Zoom, j * Zoom, (i + 1) * Zoom, (j + 1) * Zoom);
      End;
    End;
  End;
End;

Procedure TForm2.FormCreate(Sender: TObject);
Begin
  img := TBitmap.Create;
  Grid := Nil;
End;

Procedure TForm2.Button1Click(Sender: TObject);
Var
  i, j: Integer;
Begin
  // reset Grid
  For i := 0 To high(Grid) Do Begin
    For j := 0 To high(Grid[i]) Do Begin
      Grid[i, j].forced := false;
    End;
  End;
  PaintBox1.Invalidate;
End;

Procedure TForm2.Button2Click(Sender: TObject);
Begin
  modalresult := mrOK;
End;

Procedure TForm2.Button3Click(Sender: TObject);
Var
  b: TBitmap;
  c: TColor;
  i, j: Integer;
  p: TPortableNetworkGraphic;
Begin
  If OpenPictureDialog1.Execute Then Begin
    b := TBitmap.Create;

    Case lowercase(ExtractFileExt(OpenPictureDialog1.FileName)) Of
      '.bmp': b.LoadFromFile(OpenPictureDialog1.FileName);
      '.png': Begin
          p := TPortableNetworkGraphic.Create;
          p.LoadFromFile(OpenPictureDialog1.FileName);
          b.Assign(p);
          p.free;
        End;
    End;

    form1.Edit2.Text := inttostr(b.Width);
    form1.Edit3.Text := inttostr(b.Height);
    img.Width := b.Width * zoom;
    img.Height := b.Height * zoom;
    img.Canvas.Rectangle(-1, -1, img.Width + 1, img.Height + 1);
    setlength(Grid, b.Width, b.Height);
    Button1.Click;
    For i := 0 To b.Width - 1 Do Begin
      For j := 0 To b.Height - 1 Do Begin
        c := b.Canvas.Pixels[i, j];
        If c <> MostCol Then Begin
          Grid[i, j].forced := true;
          Grid[i, j].Col := c;
        End;
      End;
    End;
    PaintBox1.Width := img.Width;
    PaintBox1.Height := img.Height;
    PaintBox1.Invalidate;
    b.free;
  End;
End;

Procedure TForm2.FormDestroy(Sender: TObject);
Begin
  img.free;
  img := Nil;
End;

Procedure TForm2.Init(Const Image: TBitmap; w, h: integer);
Var
  c: Array Of Record
    col: TColor;
    cnt: integer;
  End;

  i, j, k: Integer;

  found: Boolean;
Begin
  // Extract all colors of Image
  c := Nil;
  For i := 0 To Image.Width - 1 Do Begin
    For j := 0 To Image.Height - 1 Do Begin
      MostCol := image.Canvas.Pixels[i, j];
      found := false;
      For k := 0 To high(c) Do Begin
        If c[k].Col = MostCol Then Begin
          found := true;
          c[k].cnt := c[k].cnt + 1;
          break;
        End;
      End;
      If Not found Then Begin
        setlength(c, high(c) + 2);
        c[high(c)].Col := MostCol;
        c[high(c)].cnt := 1;
      End;
    End;
  End;
  ListBox1.Clear;
  j := c[0].cnt;
  MostCol := c[0].Col;
  For i := 0 To high(c) Do Begin
    ListBox1.items.add(ColorToString(c[i].Col));
    If c[i].cnt > j Then Begin
      j := c[i].cnt;
      MostCol := c[i].Col;
    End;
  End;
  ListBox1.ItemIndex := 0;

  img.Width := w * Zoom;
  img.Height := h * Zoom;
  img.Canvas.Brush.Color := mostCol;
  img.Canvas.Rectangle(-1, -1, img.Width + 1, img.Height + 1);
  PaintBox1.Width := img.Width;
  PaintBox1.Height := img.Height;
  PaintBox1.Invalidate;
  setlength(Grid, w, h);
  button3.hint := ColorToString(mostcol) + ' will be interpreted as "not set"';
End;

End.

