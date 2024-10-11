Unit Unit7;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  Spin, LCLType, upixeleditor_types;

Type

  { TForm7 }

  TForm7 = Class(TForm)
    Button1: TButton;
    Button2: TButton;
    FloatSpinEdit1: TFloatSpinEdit;
    Image1: TImage;
    Label1: TLabel;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    Procedure FloatSpinEdit1Change(Sender: TObject);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDestroy(Sender: TObject);
  private
    fInImage: TBitmap;
  public
    Procedure InitFromPixelArea(Const Data: TPixelArea);
    Function GetScaleMode(): TScaleMode;
  End;

Var
  Form7: TForm7;

Implementation

{$R *.lfm}

Uses
  IntfGraphics
  , ugraphics
  ;

{ TForm7 }

Procedure TForm7.FormCreate(Sender: TObject);
Begin
  caption := 'Rotation';
  fInImage := TBitmap.Create;
End;

Procedure TForm7.FloatSpinEdit1Change(Sender: TObject);
Var
  b: TBitmap;
  mode: TInterpolationMode;
Begin
  b := TBitmap.Create;
  b.Assign(fInImage);
  mode := imNearestNeighbour;
  If RadioButton2.Checked Then mode := imBilinear;
  RotateDegrees(b, FloatSpinEdit1.Value, mode);
  Image1.Picture.Assign(b);
  b.free;
End;

Procedure TForm7.FormDestroy(Sender: TObject);
Begin
  fInImage.free;
  fInImage := Nil;
End;

Procedure TForm7.InitFromPixelArea(Const Data: TPixelArea);
Var
  i, j: Integer;
  tmp: TLazIntfImage;
  DestHandle, DestMaskHandle: HBitmap;
Begin
  fInImage.Width := length(data);
  fInImage.Height := length(data[0]);
  tmp := TLazIntfImage.Create(0, 0);
  tmp.LoadFromBitmap(fInImage.Handle, fInImage.MaskHandle);
  For i := 0 To high(data) Do Begin
    For j := 0 To high(data[i]) Do Begin
      tmp.Colors[i, j] := RGBAToFPColor(data[i, j]);
    End;
  End;
  tmp.CreateBitmaps(DestHandle, DestMaskHandle, false);
  fInImage.Handle := DestHandle;
  fInImage.MaskHandle := DestMaskHandle;
  tmp.free;
  FloatSpinEdit1.Value := 0.0;
  RadioButton1.Checked := true;
  FloatSpinEdit1Change(Nil);
End;

Function TForm7.GetScaleMode: TScaleMode;
Begin
  result := smScale;
  If RadioButton2.Checked Then result := smSmoothScale;
End;

End.

