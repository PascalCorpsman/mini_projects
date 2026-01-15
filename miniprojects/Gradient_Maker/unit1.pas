(******************************************************************************)
(* Gradient_maker                                                  14.01.2026 *)
(*                                                                            *)
(* Version     : 0.01                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Tool to create gradient textures, inspired by                *)
(*                 https://coolors.co/gradient-maker/random                   *)
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
Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Menus, IntfGraphics, FPImage, uvectormath;

Type

  { TKnob }

  TKnob = Class
  private
    fx: integer;
    fPercent: Single;
    selimg, img: TBitmap;
    Function getX: Integer;
    Procedure setPercent(AValue: Single);
    Procedure setX(AValue: Integer);
  public
    Color: TColor;
    Selected: Boolean; // Wird via PaintBox1.Paint bestimmt
    Property Percent: Single read fPercent write setPercent; // Wird aus X berechnet
    Property X: Integer read getX write setX; // Reale Position im Panel, wird via PaintBox1.MouseMove bestimmt

    Constructor Create(); virtual;
    Destructor Destroy; override;

    Procedure Render(Const Canvas: TCanvas);
    Function Hit(aX: integer): Boolean;
  End;

  { TForm1 }

  TForm1 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    ColorButton1: TColorButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    MenuItem1: TMenuItem;
    OpenDialog1: TOpenDialog;
    PaintBox1: TPaintBox;
    PaintBox2: TPaintBox;
    PopupMenu1: TPopupMenu;
    RadioGroup1: TRadioGroup;
    RadioGroup2: TRadioGroup;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    Procedure Button1Click(Sender: TObject);
    Procedure Button2Click(Sender: TObject);
    Procedure Button3Click(Sender: TObject);
    Procedure CheckBox1Click(Sender: TObject);
    Procedure ColorButton1ColorChanged(Sender: TObject);
    Procedure Edit1KeyPress(Sender: TObject; Var Key: char);
    Procedure Edit2KeyPress(Sender: TObject; Var Key: char);
    Procedure Edit4KeyPress(Sender: TObject; Var Key: char);
    Procedure Edit5KeyPress(Sender: TObject; Var Key: char);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure MenuItem1Click(Sender: TObject);
    Procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    Procedure PaintBox1Paint(Sender: TObject);
    Procedure PaintBox2Paint(Sender: TObject);
    Procedure RadioGroup1Click(Sender: TObject);
    Procedure RadioGroup2Click(Sender: TObject);
    Procedure ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode;
      Var ScrollPos: Integer);
    Procedure ScrollBar2Change(Sender: TObject);
  private
    Buffer: Array[-5..105] Of TColor; // Relevant sind nur 0..100, aber der Slider schiest minimal drüber raus ;)
    SliderMask: Array Of Array Of Boolean;
    SliderBmp, TargetBmp: TBitmap;
    KnobDx: integer;
    Knobs: Array Of TKnob;
    KnobIndex: Integer;
    Procedure DrawSlider();
    Procedure DrawPreview();
    Procedure KnobIndexToLCL();
    // X in [0..100] , außerhalb wird geclampt
    Function GetGradientColor(Percent: Single): TColor;
    Function InterpolateColors(c1, c2: TColor; Scale: Single): TColor;
    Function PixelToPercent(i, j: integer; Dir: TVector2): Single;
    Function GetDir(): TVector2;
  public

  End;

Var
  Form1: TForm1;

Implementation

{$R *.lfm}

Uses Math, LCLType, IniFiles;

Const
  PanelBorder = 16;
  PanelBorder_d_2 = PanelBorder Div 2;
  PanelBorder_m_2 = PanelBorder * 2;

  { TKnob }

Function TKnob.getX: Integer;
Begin
  result := fx;
End;

Procedure TKnob.setPercent(AValue: Single);
Begin
  fPercent := AValue;
  fx := round(ConvertDimension(0, 100, fPercent, PanelBorder, form1.PaintBox1.Width - PanelBorder));
End;

Procedure TKnob.setX(AValue: Integer);
Begin
  fx := AValue;
  fPercent := 100 * (fx - PanelBorder) / (Form1.PaintBox1.Width - 2 * PanelBorder);
End;

Constructor TKnob.Create;
Begin
  Inherited Create;
  img := TBitmap.Create;
  img.width := PanelBorder_m_2;
  img.Height := PanelBorder_m_2;
  img.TransparentColor := clFuchsia;
  img.Transparent := true;
  img.canvas.Brush.Color := clFuchsia;
  img.canvas.Rectangle(-1, -1, PanelBorder_m_2 + 1, PanelBorder_m_2 + 1);
  img.canvas.pen.Color := clGray;
  img.canvas.Brush.Color := clWhite;
  img.canvas.Ellipse(0, 0, PanelBorder_m_2, PanelBorder_m_2);
  img.canvas.pen.Color := clGray;
  img.canvas.Brush.Color := clFuchsia;
  img.canvas.Ellipse(PanelBorder_d_2, PanelBorder_d_2, 3 * PanelBorder_d_2, 3 * PanelBorder_d_2);
  selimg := TBitmap.Create;
  selimg.width := PanelBorder_m_2;
  selimg.Height := PanelBorder_m_2;
  selimg.TransparentColor := clFuchsia;
  selimg.Transparent := true;
  selimg.canvas.Brush.Color := clFuchsia;
  selimg.canvas.Rectangle(-1, -1, PanelBorder_m_2 + 1, PanelBorder_m_2 + 1);
  selimg.canvas.pen.Color := clBlack;
  selimg.canvas.Brush.Color := clGray;
  selimg.canvas.Ellipse(0, 0, PanelBorder_m_2, PanelBorder_m_2);
  selimg.canvas.pen.Color := clBlack;
  selimg.canvas.Brush.Color := clFuchsia;
  selimg.canvas.Ellipse(PanelBorder_d_2, PanelBorder_d_2, 3 * PanelBorder_d_2, 3 * PanelBorder_d_2);
End;

Destructor TKnob.Destroy;
Begin
  img.free;
  selimg.Free;
End;

Procedure TKnob.Render(Const Canvas: TCanvas);
Begin
  If Selected Then Begin
    canvas.Draw(x - PanelBorder, 0, selimg);
  End
  Else Begin
    canvas.Draw(x - PanelBorder, 0, img);
  End;
End;

Function TKnob.Hit(aX: integer): Boolean;
Begin
  result := (x >= aX - PanelBorder) And (x <= aX + PanelBorder);
End;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Var
  i, j: Integer;
Begin
  caption := 'Gradient Maker ver. 0.01 by Corpsman, www.Corpsman.de';
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
  SliderBmp := TBitmap.Create;
  TargetBmp := TBitmap.Create;
  setlength(Knobs, 2);
  Knobs[0] := TKnob.Create();
  Knobs[0].X := PanelBorder;
  Knobs[0].Color := clBlack;
  Knobs[0].Color := clLime;
  Knobs[1] := TKnob.Create();
  Knobs[1].X := PaintBox1.Width - PanelBorder;
  Knobs[1].Color := clRed;
  SliderBmp.Width := PaintBox1.Width - PanelBorder;
  SliderBmp.Height := PanelBorder;
  SliderBmp.Canvas.Brush.Color := clFuchsia;
  SliderBmp.Canvas.Rectangle(-1, -1, SliderBmp.Width + 1, SliderBmp.Height + 1);
  SliderBmp.Canvas.Brush.Color := clWhite;
  SliderBmp.Canvas.Pen.Color := clWhite;
  SliderBmp.Canvas.Rectangle(PanelBorder_d_2, 0, SliderBmp.Width - PanelBorder_d_2, SliderBmp.Height);
  SliderBmp.Canvas.Ellipse(0, 0, PanelBorder, SliderBmp.Height);
  SliderBmp.Canvas.Ellipse(SliderBmp.Width - PanelBorder, 0, SliderBmp.Width, SliderBmp.Height);
  setlength(SliderMask, SliderBmp.Width, SliderBmp.Height);
  For i := 0 To SliderBmp.Width - 1 Do
    For j := 0 To SliderBmp.Height - 1 Do Begin
      SliderMask[i, j] := SliderBmp.Canvas.Pixels[i, j] = clFuchsia;
    End;
  edit2.text := '0';
  edit3.text := '256';
  edit4.text := '256';
  DrawSlider;
  button3.click;
  KnobIndex := 0;
  KnobIndexToLCL();
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Var
  i: Integer;
Begin
  TargetBmp.free;
  SliderBmp.free;
  For i := 0 To high(Knobs) Do
    Knobs[i].Free;
End;

Procedure TForm1.MenuItem1Click(Sender: TObject);
Begin
  // Save Result
  If SaveDialog1.Execute Then Begin
    TargetBmp.SaveToFile(SaveDialog1.FileName);
  End;
End;

Procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  i: Integer;
Begin
  // Select Knob ?
  knobIndex := -1;
  For i := 0 To high(Knobs) Do Begin
    If Knobs[i].Hit(x) Then Begin
      knobIndex := i;
      break;
    End;
  End;
  // Löschen
  If (ssright In shift) And (KnobIndex <> -1) And (length(Knobs) > 1) Then Begin
    Knobs[KnobIndex].free;
    For i := KnobIndex To high(Knobs) - 1 Do Begin
      Knobs[i] := Knobs[i + 1];
    End;
    setlength(Knobs, high(Knobs));
    KnobIndex := -1;
    KnobIndexToLCL();
    DrawSlider();
    DrawPreview();
  End;
  // Hinzufügen
  If (ssLeft In shift) Then Begin
    If (KnobIndex = -1) Then Begin
      setlength(Knobs, high(Knobs) + 2);
      Knobs[high(Knobs)] := TKnob.Create();
      KnobIndex := high(Knobs);
      Knobs[KnobIndex].X := x;
      Knobs[KnobIndex].Color := clBlack;
      DrawSlider();
      DrawPreview();
    End;
    KnobDx := Knobs[KnobIndex].X - x;
    KnobIndexToLCL();
  End;
End;

Procedure TForm1.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
Begin
  // Move Knob
  If (ssleft In shift) And (KnobIndex <> -1) Then Begin
    Knobs[KnobIndex].X := max(PanelBorder, min(PaintBox1.Width - PanelBorder, x + KnobDx));
    KnobIndexToLCL();
    DrawSlider();
    DrawPreview();
  End;
End;

Procedure TForm1.PaintBox1Paint(Sender: TObject);
Var
  i: Integer;
Begin
  PaintBox1.Canvas.Draw(PanelBorder_d_2, PanelBorder_d_2, SliderBmp);
  // 2. Die Buttons drüber ;)
  For i := 0 To high(Knobs) Do Begin
    Knobs[i].Selected := i = KnobIndex;
    Knobs[i].Render(PaintBox1.Canvas);
  End;
End;

Procedure TForm1.PaintBox2Paint(Sender: TObject);
Begin
  PaintBox2.Canvas.Draw(0, 0, TargetBmp);
End;

Procedure TForm1.Button3Click(Sender: TObject);
Begin
  // Set Target Dimensions
  TargetBmp.Width := StrToIntDef(edit3.text, 256);
  TargetBmp.Height := StrToIntDef(edit4.text, 256);
  PaintBox2.Width := TargetBmp.Width;
  PaintBox2.Height := TargetBmp.Height;
  DrawPreview;
End;

Procedure TForm1.CheckBox1Click(Sender: TObject);
Begin
  DrawSlider();
  DrawPreview();
End;

Procedure TForm1.Button2Click(Sender: TObject);
Var
  ini: TIniFile;
  i: Integer;
Begin
  // Save Gradient
  If SaveDialog2.Execute Then Begin
    ini := TIniFile.Create(SaveDialog2.FileName);
    ini.WriteString('General', 'Width', edit3.text);
    ini.WriteString('General', 'Height', edit4.text);
    ini.WriteString('General', 'Type', RadioGroup1.items[RadioGroup1.ItemIndex]);
    ini.WriteString('General', 'Rotation', edit2.text);
    ini.WriteString('General', 'interpolation', RadioGroup2.items[RadioGroup2.ItemIndex]);
    ini.WriteInteger('Knobs', 'Count', length(Knobs));
    For i := 0 To high(Knobs) Do Begin
      ini.WriteInteger('Knobs', 'Color' + inttostr(i), Knobs[i].Color);
      ini.WriteInteger('Knobs', 'Percent' + inttostr(i), round(Knobs[i].Percent));
    End;
    ini.free;
  End;
End;

Procedure TForm1.Button1Click(Sender: TObject);
Var
  ini: TIniFile;
  i: Integer;
  s: String;
Begin
  If OpenDialog1.Execute Then Begin
    ini := TIniFile.Create(OpenDialog1.FileName);
    edit3.text := ini.ReadString('General', 'Width', edit3.text);
    edit4.text := ini.ReadString('General', 'Height', edit4.text);
    s := ini.ReadString('General', 'Type', RadioGroup1.items[RadioGroup1.ItemIndex]);
    For i := 0 To RadioGroup1.Items.Count - 1 Do Begin
      If s = RadioGroup1.Items[i] Then RadioGroup1.ItemIndex := i;
    End;
    edit2.text := ini.ReadString('General', 'Rotation', edit2.text);
    ScrollBar2.Position := strtointdef(edit2.text, 0);
    s := ini.ReadString('General', 'interpolation', RadioGroup2.items[RadioGroup2.ItemIndex]);
    For i := 0 To RadioGroup2.Items.Count - 1 Do Begin
      If s = RadioGroup2.Items[i] Then RadioGroup2.ItemIndex := i;
    End;
    For i := 0 To high(Knobs) Do
      Knobs[i].free;
    i := ini.ReadInteger('Knobs', 'Count', 1);
    setlength(Knobs, i);
    For i := 0 To high(Knobs) Do Begin
      Knobs[i] := TKnob.Create();
      Knobs[i].Color := ini.ReadInteger('Knobs', 'Color' + inttostr(i), clblack);
      Knobs[i].Percent := ini.ReadInteger('Knobs', 'Percent' + inttostr(i), 0);
    End;
    ini.free;
  End;
  DrawSlider();
  DrawPreview();
End;

Procedure TForm1.ColorButton1ColorChanged(Sender: TObject);
Begin
  If KnobIndex < 0 Then exit;
  If ColorButton1.ButtonColor <> Knobs[KnobIndex].Color Then Begin
    Knobs[KnobIndex].Color := ColorButton1.ButtonColor;
    KnobIndexToLCL;
    DrawSlider();
    DrawPreview();
  End;
End;

Procedure TForm1.Edit1KeyPress(Sender: TObject; Var Key: char);
Var
  s: String;
  r, g, b, dummy: integer;
Begin
  If key = chr(vk_Return) Then Begin
    If KnobIndex < 0 Then exit;
    s := trim(edit1.Text);
    If length(s) <> 7 Then Begin
      showmessage('Error: invalid format, need to be: #RRGGBB');
      exit;
    End;
    delete(s, 1, 1);
    val('$' + copy(s, 1, 2), r, dummy);
    If dummy <> 0 Then Begin
      showmessage('Error: ' + copy(s, 1, 2) + ' is not a correct hex value.');
      exit;
    End;
    val('$' + copy(s, 3, 2), g, dummy);
    If dummy <> 0 Then Begin
      showmessage('Error: ' + copy(s, 3, 2) + ' is not a correct hex value.');
      exit;
    End;
    val('$' + copy(s, 5, 2), b, dummy);
    If dummy <> 0 Then Begin
      showmessage('Error: ' + copy(s, 5, 2) + ' is not a correct hex value.');
      exit;
    End;
    Knobs[KnobIndex].Color := r + (g Shl 8) + (b Shl 16);
    KnobIndexToLCL();
    DrawSlider();
    DrawPreview();
  End;
End;

Procedure TForm1.Edit2KeyPress(Sender: TObject; Var Key: char);
Begin
  If key = chr(vk_Return) Then Begin
    ScrollBar2.Position := strtointdef(edit2.text, 0);
    DrawPreview();
  End;
End;

Procedure TForm1.Edit4KeyPress(Sender: TObject; Var Key: char);
Begin
  If key = chr(vk_Return) Then Button3.Click;
End;

Procedure TForm1.Edit5KeyPress(Sender: TObject; Var Key: char);
Var
  f: Single;
  dummy: Integer;
Begin
  If key = chr(vk_Return) Then Begin
    f := StrToFloatDef(edit5.text, 0);
    ScrollBar1.Position := round(f * 10);
    dummy := ScrollBar1.Position;
    ScrollBar1.OnScroll(ScrollBar1, scPosition, dummy);
  End;
End;

Procedure TForm1.RadioGroup1Click(Sender: TObject);
Begin
  Case RadioGroup1.ItemIndex Of
    0: Begin // Linear
        label5.Visible := true;
        edit2.Visible := true;
        label6.Visible := true;
        ScrollBar2.Visible := true;
      End;
    1: Begin // Radial
        label5.Visible := false;
        edit2.Visible := false;
        label6.Visible := false;
        ScrollBar2.Visible := false;
      End;
  End;
  DrawPreview;
End;

Procedure TForm1.RadioGroup2Click(Sender: TObject);
Begin
  DrawPreview;
End;

Procedure TForm1.ScrollBar1Scroll(Sender: TObject; ScrollCode: TScrollCode;
  Var ScrollPos: Integer);
Begin
  If KnobIndex < 0 Then exit;
  Knobs[KnobIndex].Percent := ScrollBar1.Position / 10;
  edit5.text := format('%0.1f', [ScrollBar1.Position / 10]);
  DrawSlider();
  DrawPreview();
End;

Procedure TForm1.ScrollBar2Change(Sender: TObject);
Begin
  edit2.text := inttostr(ScrollBar2.Position);
  DrawPreview();
End;

Procedure TForm1.DrawSlider;
Var
  intf: TLazIntfImage;
  x, y: Integer;
  p: Single;
  c: TColor;
  empty, fpc: TFPColor;
Begin
  // 1. Den Preview Gradienten Malen
  intf := SliderBmp.CreateIntfImage;
  // TODO: Wie die exakte Hintergrundfarbe ermittelm das alles geht nicht :(
  //  empty := TColorToFPColor(clBackground);
  empty := TColorToFPColor($D6 * $010101); //TColorToFPColor(form1.GetDefaultColor(dctBrush));
  // Flush the buffer;)
  For x := low(Buffer) To high(Buffer) Do Begin
    buffer[x] := -1;
  End;
  For x := 0 To SliderBmp.Width - 1 Do Begin
    p := ConvertDimension(PanelBorder_d_2, SliderBmp.Width - PanelBorder_d_2, x, 0, 100);
    c := GetGradientColor(p);
    fpc := TColorToFPColor(c);
    For y := 0 To SliderBmp.Height - 1 Do Begin
      If SliderMask[x, y] Then Begin
        intf.Colors[x, y] := empty;
      End
      Else Begin
        intf.Colors[x, y] := fpc;
      End;
    End;
  End;
  SliderBmp.LoadFromIntfImage(intf);
  intf.free;
  PaintBox1.Invalidate;
End;

Procedure TForm1.DrawPreview;
Var
  intf: TLazIntfImage;
  j, i: Integer;
  p: Single;
  c: TColor;
  fpc: TFPColor;
  //  q: QWord; // -- Debug Performance calculation
  dir: TVector2;
Begin
  // Update image
  intf := TargetBmp.CreateIntfImage;
  //  q := GetTickCount64;
  dir := getDir(); // Precalc direction in Linear Mode, this saves millions of sin / cos calculations!
  For j := 0 To TargetBmp.Height - 1 Do Begin
    For i := 0 To TargetBmp.Width - 1 Do Begin
      p := PixelToPercent(i, j, dir);
      c := GetGradientColor(p);
      fpc := TColorToFPColor(c);
      intf.Colors[i, j] := fpc;
    End;
  End;
  //  q := GetTickCount64 - q;
  //  caption := inttostr(q);
  TargetBmp.LoadFromIntfImage(intf);
  intf.free;
  // Paint it ;)
  PaintBox2.Invalidate;
End;

Procedure TForm1.KnobIndexToLCL;
Var
  c: TColor;
Begin
  If KnobIndex = -1 Then Begin
    edit1.text := '#000000';
    ColorButton1.Color := clBlack;
    ScrollBar1.Position := 0;
    RadioGroup1.ItemIndex := 0;
    Edit2.Text := '90';
  End
  Else Begin
    c := Knobs[KnobIndex].Color;
    edit1.text := format('#%0.2X%0.2X%0.2X', [c And $FF, (c Shr 8) And $FF, (c Shr 16) And $FF]);
    ColorButton1.ButtonColor := c;
    ScrollBar1.Position := round(Knobs[KnobIndex].Percent * 10);
    Edit5.text := format('%0.1f', [Knobs[KnobIndex].Percent]);
  End;
End;

Function TForm1.GetGradientColor(Percent: Single): TColor;
Var
  i, index, BiggestLowerIndex, LowestBiggerIndex: Integer;
  pNorm, delta: Single;
Begin
  If (percent > 0) And (buffer[round(Percent)] <> -1) And (CheckBox1.Checked) Then Begin
    result := buffer[round(Percent)];
    exit;
  End;
  // 1. das Offensichtliche Clamping ;)
  If Percent < 0 Then Begin
    index := 0;
    For i := 1 To high(Knobs) Do Begin
      If Knobs[i].Percent < Knobs[index].Percent Then index := i;
    End;
    result := Knobs[index].Color;
    Buffer[round(Percent)] := result;
    exit;
  End;
  If Percent > 100 Then Begin
    index := 0;
    For i := 1 To high(Knobs) Do Begin
      If Knobs[i].Percent > Knobs[index].Percent Then index := i;
    End;
    result := Knobs[index].Color;
    Buffer[round(Percent)] := result;
    exit;
  End;
  // Percent ist in [0..100]
  // wir suchen den größten Index der <= Percent ist und
  BiggestLowerIndex := -1;
  LowestBiggerIndex := -1;
  For i := 0 To high(Knobs) Do Begin
    If (Knobs[i].Percent <= Percent) Then Begin
      If BiggestLowerIndex = -1 Then Begin
        BiggestLowerIndex := i;
      End
      Else Begin
        If (Knobs[i].Percent > Knobs[BiggestLowerIndex].Percent) Then BiggestLowerIndex := i;
      End;
    End;
    If (Knobs[i].Percent >= Percent) Then Begin
      If LowestBiggerIndex = -1 Then Begin
        LowestBiggerIndex := i;
      End
      Else Begin
        If (Knobs[i].Percent < Knobs[LowestBiggerIndex].Percent) Then LowestBiggerIndex := i;
      End;
    End;
  End;
  If (LowestBiggerIndex = -1) Then Begin
    // Es Gibt keinen Kleinsten Größten -> Folglich sind wir Rechts vom Größten
    result := GetGradientColor(101);
    Buffer[round(Percent)] := result;
    exit;
  End;
  If (BiggestLowerIndex = -1) Then Begin
    // Es Gibt keinen größten Kleineren -> Folglich sind wir Links vom Kleinsten
    result := GetGradientColor(-1);
    Buffer[round(Percent)] := result;
    exit;
  End;
  // Nun müssen wir zwischen BiggestLowerIndex und LowestBiggerIndex interpolieren
  delta := Knobs[LowestBiggerIndex].Percent - Knobs[BiggestLowerIndex].Percent;
  If delta = 0 Then Begin
    result := Knobs[LowestBiggerIndex].Color;
    Buffer[round(Percent)] := result;
    exit;
  End;
  pNorm := (Percent - Knobs[BiggestLowerIndex].Percent) / delta; // Normiert auf 0..1
  result := InterpolateColors(Knobs[BiggestLowerIndex].Color, Knobs[LowestBiggerIndex].Color, 1 - pNorm);
  Buffer[round(Percent)] := result;
End;

Function TForm1.InterpolateColors(c1, c2: TColor; Scale: Single): TColor;
Var
  r, g, b: integer;
  r1, g1, b1: integer;
  r2, g2, b2: integer;
Begin
  r1 := c1 And $FF;
  g1 := (c1 Shr 8) And $FF;
  b1 := (c1 Shr 16) And $FF;
  r2 := c2 And $FF;
  g2 := (c2 Shr 8) And $FF;
  b2 := (c2 Shr 16) And $FF;
  Case RadioGroup2.ItemIndex Of
    0: Begin // Linear
        // Nothing to do
      End;
    1: Begin
        scale := (1 - cos(pi * Scale)) / 2;
      End;
  End;
  r := min(255, max(0, round(r1 * scale + r2 * (1 - Scale))));
  g := min(255, max(0, round(g1 * scale + g2 * (1 - Scale))));
  b := min(255, max(0, round(b1 * scale + b2 * (1 - Scale))));

  result := (b Shl 16) Or (g Shl 8) Or r;
End;

Function TForm1.PixelToPercent(i, j: integer; Dir: TVector2): Single;
Var
  w, h: Integer;
  wd2, hd2: Single;
  p: TVector2;
Begin
  w := TargetBmp.Width;
  h := TargetBmp.Height;
  wd2 := w / 2;
  hd2 := h / 2;
  Case RadioGroup1.ItemIndex Of
    0: Begin // Linear
        p := v2((i - wd2) / wd2, (j - hd2) / hd2);
        result := (100 * (p * dir) + 50);
      End;
    1: Begin // Radial
        result := 100 * sqrt(
          (sqr(wd2 - i) + sqr(hd2 - j)) /
          (sqr(wd2) + sqr(hd2))
          );
      End;
  End;
End;

Function TForm1.GetDir(): TVector2;
Var
  adeg, f, scale, a, s, c: Single;
Begin
  // the correctionfactor f is so a WTF thing, but without
  // It is wrong (with it its mostly "right")
  adeg := strtointdef(edit2.text, 0);
  a := DegToRad(adeg);
  While adeg < 0 Do
    adeg := adeg + 90;
  While adeg > 90 Do
    adeg := adeg - 90;
  If adeg >= 45 Then Begin
    f := 2 - (adeg - 45) / 45;
  End
  Else Begin
    f := 1 + adeg / 45;
  End;
  // f := 1 + abs(sin(2 * a)); -- This is the ChatGTP sollution, but it's also not good :/
  sincos(a, s, c);
  result := v2(c, -s);
  scale := 1 / (2 * f * max(abs(s), abs(c)));
  result := result * scale;
End;

End.

