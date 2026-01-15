(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of Imageshop                                             *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit unodes;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Graphics, Dialogs, extctrls, controls, ueventer, uinterpreter;

Type

  THintEvent = Procedure(Sender: TObject; aHint: String) Of Object;

  TPixel = Record
    r, g, b, a: Single;
  End;

  TDirection = (dLeft, dRight);

  { TConnector }

  (*
   * Der Connector hat 2 Modi
   * Links oder Rechts
   * Er Rendert sich immer zentriert Links oder Rechts der angegebenen Koordinaten
   *)
  TConnector = Class(TEventerClass)
  private
    fcanvas: TCanvas;
    fdirection: TDirection;
  protected
  public
    Constructor Create(aOwner: TPaintBox; Direction: TDirection); virtual reintroduce;
    Procedure Render;
  End;

  { TCButton }

  TCButton = Class(TEventerClass)
  private
    fCaption: String;
    fCanvas: TCanvas;
    Procedure SetCaption(AValue: String);
  protected
  public
    Property Caption: String read fCaption write SetCaption;
    Constructor Create(aOwner: TPaintBox); override;
    Procedure Render;
  End;

  { TCSlider }

  TCSlider = Class(TEventerClass)
  private
    fMouseMode: integer;
    fCanvas: TCanvas;
    fPosition: Single;
    Function GetPosition: Single;
    Procedure SetPosition(AValue: Single);
  protected
    Procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    Procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    Procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    OnChange: TNotifyEvent;
    Property Position: Single read GetPosition write SetPosition;
    Constructor Create(aOwner: TPaintBox); override;
    Procedure Render;
  End;

  { TNode }

  TNode = Class
  private
    fCanvas: TCanvas;
    fCaption: String;
    fLeft: integer;
    fTop: integer;
    fwidth: integer;
    fHeight: integer;
    fOutConnector: TConnector;
    fInConnectors: Array Of TConnector; // Für die Kindklassen
    fCloseButton: TCButton;
    Function GetClientRect: Trect;
    Procedure setCaption(AValue: String);
    Procedure Setleft(AValue: integer); virtual;
    Procedure settop(AValue: integer); virtual;
    Procedure CloseButtonClick(Sender: TObject);
    Procedure OutConnectClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    Function RecursionCheck(Level: integer; Border: integer): boolean; // Wird genau aufgerufen wie GetImageDimension, zählt die Aufrufe mit und wenn Border überschritten wird, dann kommt true
  protected
    Function GetHint: String; virtual;
  public
    OnCloseButtonClick: TNotifyEvent; // Wenn der Schließen Button Gedrückt wird
    OnOutConnectClick: TNotifyEvent; // Wenn auf den Out Connector geklickt wird
    OnChange: TNotifyEvent; // Irgend etwas hat sich in der Pixel Verarbeitenden Richtung veränder, das Ausgabebild sollte neu erzeugt werden.
    OnHint: THintEvent;
    InNode: TNode; // Für die Kindklassen
    In2Node: TNode; // Für die Kindklassen
    Property Caption: String read fCaption write setCaption;
    Property Width: integer read fwidth;
    Property Height: integer read fHeight;
    Property Left: integer read fLeft write SetLeft;
    Property Top: integer read fTop write settop;
    Property ClientRect: Trect read GetClientRect;
    Constructor Create(Owner: TPaintBox; x, y: integer); virtual;
    Destructor Destroy; override;
    Procedure Render; virtual;

    Function GetImageDimension(): Tpoint; virtual;
    Function GetPixelValue(x, y: Single): TPixel; virtual; abstract;
  End;

  { TOneInputNode }

  (*
   * Ein Parameterloser Operator
   *)
  TOneInputNode = Class(TNode)
  private
    Procedure InButtonClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  protected
    (*
     * Ein Neuer Operand muss lediglich diese Function überschreiben, alles andere ist bereits durch das Framework erledigt *g*.
     *)
    Function DoThePixelOperation(Const Pixel: TPixel; x, y: Single): TPixel; virtual;
  public
    OnInButtonClick: TNotifyEvent;
    Constructor Create(Owner: TPaintBox; x, y: integer); override;
    Function GetImageDimension(): Tpoint; override;
    Function GetPixelValue(x, y: Single): TPixel; override;
  End;

  { TTwoInputNode }

  (*
   * Ein Parameterloser Blend Operator
   *)
  TTwoInputNode = Class(TNode)
  private
    Procedure InButtonClick(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    Procedure InButton2Click(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  protected
    (*
     * Ein Neuer Operand muss lediglich diese Function überschreiben, alles andere ist bereits durch das Framework erledigt *g*.
     *)
    Function DoThePixelOperation(Const Pixel1, Pixel2: TPixel; x, y: Single): TPixel; virtual;
  public
    OnInButtonClick: TNotifyEvent;
    OnInButton2Click: TNotifyEvent;
    Constructor Create(Owner: TPaintBox; x, y: integer); override;
    Function GetImageDimension(): Tpoint; override;
    Function GetPixelValue(x, y: Single): TPixel; override;
  End;

  { TOneInputSliderNode }

  (*
   * Ein Operator mit einem Slider
   *)
  TOneInputSliderNode = Class(TOneInputNode)
  private
    fSlider: TCSlider;
    Function DoThePixelOperation(Const Pixel1: TPixel; x, y: Single): TPixel; override; // Das Ist Absicht, das hier die Sichtbarkeit verringert wird, die wird ja durch die Slider Variante ersetzt
    Procedure OnSliderChange(Sender: TObject);
    Procedure SetSliderPosition(AValue: Single);
  protected
    Function GetSliderPosition: Single; // Damit der Sliderwert in den Kindklassen sichtbar wird
    Function GetHint: String; override;
    Procedure Setleft(AValue: integer); override;
    Procedure settop(AValue: integer); override;
    (*
     * Ein Neuer Operand muss lediglich diese Function überschreiben, alles andere ist bereits durch das Framework erledigt *g*.
     *)
    Function DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y, SliderValue: Single): TPixel; virtual;
  public
    Property SliderPosition: Single read GetSliderPosition write SetSliderPosition;
    Constructor Create(Owner: TPaintBox; x, y: integer); override;
    Destructor Destroy; override;
    Procedure Render; override;
  End;

  { TCustomOperator }

  TCustomOperator = Class(TOneInputSliderNode)
  private
    fcomp: Tinterpreter;
    fFilename: String;
    Function getFilename: String;
  protected
    Function DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y, SliderValue: Single): TPixel; override;
  public
    Property Filename: String read getFilename;
    Constructor Create(Owner: TPaintBox; x, y: integer); override;
    Destructor Destroy; override;
    Function LoadSourceCode(Const aFilename: String): Boolean;
  End;

  { TTwoInputSliderNode }

  (*
   * Ein Blend Operator mit einem Slider
   *)
  TTwoInputSliderNode = Class(TTwoInputNode)
  private
    fSlider: TCSlider;
    Function DoThePixelOperation(Const Pixel1, Pixel2: TPixel; x, y: Single): TPixel; override; // Das Ist Absicht, das hier die Sichtbarkeit verringert wird, die wird ja durch die Slider Variante ersetzt
    Procedure OnSliderChange(Sender: TObject);
    Procedure SetSliderPosition(AValue: Single);
  protected
    Function GetSliderPosition: Single; // Damit der Sliderwert in den Kindklassen sichtbar wird
    Function GetHint: String; override;
    Procedure Setleft(AValue: integer); override;
    Procedure settop(AValue: integer); override;
    (*
     * Ein Neuer Operand muss lediglich diese Function überschreiben, alles andere ist bereits durch das Framework erledigt *g*.
     *)
    Function DoTheSliderPixelOperation(Const Pixel1, Pixel2: TPixel; x, y, SliderValue: Single): TPixel; virtual;
  public
    Property SliderPosition: Single read GetSliderPosition write SetSliderPosition;
    Constructor Create(Owner: TPaintBox; x, y: integer); override;
    Destructor Destroy; override;
    Procedure Render; override;
  End;

  { TCustomBlender }

  TCustomBlender = Class(TTwoInputSliderNode)
  private
    fcomp: Tinterpreter;
    fFilename: String;
    Function getFilename: String;
  protected
    Function DoTheSliderPixelOperation(Const Pixel1, Pixel2: TPixel; x, y, SliderValue: Single): TPixel; override;
  public
    Property Filename: String read getFilename;
    Constructor Create(Owner: TPaintBox; x, y: integer); override;
    Destructor Destroy; override;
    Function LoadSourceCode(Const aFilename: String): Boolean;
  End;

  { TImageSource }

  TImageSource = Class(TOneInputNode) // Fertig.
  private
    FOpenDialog: TOpenDialog;
    fButton: TCButton;
    fDimension: TPoint;
    fPixels: Array Of Array Of TPixel;
    fFilename: String;
    Procedure LoadImage(Const Filename: String);
    Procedure SetFilename(AValue: String);
    Procedure Setleft(AValue: integer); override;
    Procedure settop(AValue: integer); override;
    Procedure OnLoadButtonClick(Sender: TObject);
  protected
    Function DoThePixelOperation(Const Pixel: TPixel; x, y: Single): TPixel; override;
  public
    Property Filename: String read fFilename write SetFilename; // Nur damit die ladenroutine auch eine Datei setzen kann
    Constructor Create(Owner: TPaintBox; x, y: integer); override;
    Destructor Destroy; override;
    Procedure Render; override;
    Function GetImageDimension(): Tpoint; override;
  End;

  { TImageDest }

  TImageDest = Class(TOneInputNode)
  private
    FSaveDialog: TSaveDialog;
    fButton: TCButton;
    Procedure Setleft(AValue: integer); override;
    Procedure settop(AValue: integer); override;
    Procedure OnSaveButtonClick(Sender: TObject);
  public
    Constructor Create(Owner: TPaintBox; x, y: integer); override;
    Destructor Destroy; override;
    Procedure Render; override;
    Function CheckEndlesRecursion(TotalNodeCount: integer): Boolean; // True, wenn die Auswertung in einer EndlosRekursion enden würde
  End;

Function Pixel(r, g, b, a: Single): TPixel;

Implementation

Uses math, IntfGraphics, fpImage, LCLType, LazFileUtils;

Function Pixel(r, g, b, a: Single): TPixel;
Begin
  result.r := r;
  result.g := g;
  result.b := b;
  result.a := a;
End;

{ TCustomBlender }

Function TCustomBlender.getFilename: String;
Begin
  result := fFilename;
End;

Function TCustomBlender.DoTheSliderPixelOperation(Const Pixel1, Pixel2: TPixel;
  x, y, SliderValue: Single): TPixel;
Var
  t, s: String;
  params: TStringArray;
Begin
  setlength(params, 5);
  formatsettings.DecimalSeparator := '.';
  params[0] := format('%f;%f;%f;%f', [pixel1.r, pixel1.g, pixel1.b, pixel1.a]);
  params[1] := format('%f;%f;%f;%f', [pixel2.r, pixel2.g, pixel2.b, pixel2.a]);
  params[2] := format('%f', [x]);
  params[3] := format('%f', [y]);
  params[4] := format('%f', [SliderValue]);
  s := fcomp.CallFunction('DoTheSliderPixelOperation', params) + ';';

  t := copy(s, 1, pos(';', s) - 1);
  delete(s, 1, pos(';', s));
  result.r := strtofloat(t);

  t := copy(s, 1, pos(';', s) - 1);
  delete(s, 1, pos(';', s));
  result.g := strtofloat(t);

  t := copy(s, 1, pos(';', s) - 1);
  delete(s, 1, pos(';', s));
  result.b := strtofloat(t);

  t := copy(s, 1, pos(';', s) - 1);
  delete(s, 1, pos(';', s));
  result.a := strtofloat(t);
End;

Constructor TCustomBlender.Create(Owner: TPaintBox; x, y: integer);
Begin
  Inherited Create(Owner, x, y);
  fcomp := Tinterpreter.create;
  fFilename := '';
End;

Destructor TCustomBlender.Destroy;
Begin
  fcomp.free;
  Inherited Destroy;
End;

Function TCustomBlender.LoadSourceCode(Const aFilename: String): Boolean;
Var
  sl: TStringList;
  ss: TStringArray;
  i: Integer;
Begin

  sl := TStringList.Create;
  sl.LoadFromFile(aFilename);
  result := fcomp.Compile(sl.Text);
  sl.free;
  caption := ExtractFileNameOnly(aFilename);
  If result Then Begin
    ss := fcomp.GetFunctionList;
    result := false;
    For i := 0 To high(ss) Do Begin
      If lowercase(ss[i]) = lowercase('DoTheSliderPixelOperation(<TPixel>, <TPixel>, <Single>, <Single>, <Single>):<TPixel>;') Then Begin
        result := true;
        fFilename := aFilename;
        exit;
      End;
    End;
  End;
End;

{ TCustomOperator }

Function TCustomOperator.getFilename: String;
Begin
  result := fFilename;
End;

Function TCustomOperator.DoTheSliderPixelOperation(Const Pixel1: TPixel; x, y,
  SliderValue: Single): TPixel;
Var
  t, s: String;
  params: TStringArray;
Begin
  setlength(params, 4);
  formatsettings.DecimalSeparator := '.';
  params[0] := format('%f;%f;%f;%f', [pixel1.r, pixel1.g, pixel1.b, pixel1.a]);
  params[1] := format('%f', [x]);
  params[2] := format('%f', [y]);
  params[3] := format('%f', [SliderValue]);
  s := fcomp.CallFunction('DoTheSliderPixelOperation', params) + ';';

  t := copy(s, 1, pos(';', s) - 1);
  delete(s, 1, pos(';', s));
  result.r := strtofloat(t);

  t := copy(s, 1, pos(';', s) - 1);
  delete(s, 1, pos(';', s));
  result.g := strtofloat(t);

  t := copy(s, 1, pos(';', s) - 1);
  delete(s, 1, pos(';', s));
  result.b := strtofloat(t);

  t := copy(s, 1, pos(';', s) - 1);
  delete(s, 1, pos(';', s));
  result.a := strtofloat(t);
End;

Constructor TCustomOperator.Create(Owner: TPaintBox; x, y: integer);
Begin
  Inherited Create(Owner, x, y);
  fcomp := Tinterpreter.create;
  fFilename := '';
End;

Destructor TCustomOperator.Destroy;
Begin
  fcomp.free;
  Inherited Destroy;
End;

Function TCustomOperator.LoadSourceCode(Const aFilename: String): Boolean;
Var
  sl: TStringList;
  ss: TStringArray;
  i: Integer;
Begin
  sl := TStringList.Create;
  sl.LoadFromFile(aFilename);
  result := fcomp.Compile(sl.Text);
  sl.free;
  caption := ExtractFileNameOnly(aFilename);
  If result Then Begin
    ss := fcomp.GetFunctionList;
    result := false;
    For i := 0 To high(ss) Do Begin
      If lowercase(ss[i]) = lowercase('DoTheSliderPixelOperation(<TPixel>, <Single>, <Single>, <Single>):<TPixel>;') Then Begin
        result := true;
        fFilename := aFilename;
        exit;
      End;
    End;
  End;
End;

{ TCSlider }

Function TCSlider.GetPosition: Single;
Begin
  result := fPosition;
End;

Procedure TCSlider.SetPosition(AValue: Single);
Begin
  If fPosition = AValue Then exit;
  fPosition := min(1, max(0, AValue));
  If assigned(OnChange) Then OnChange(self);
End;

Procedure TCSlider.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
Begin
  fMouseMode := 0;
  If Not (ssleft In shift) Then exit;
  fMouseMode := 1;
  MouseMove(shift, x, y);
End;

Procedure TCSlider.MouseMove(Shift: TShiftState; X, Y: Integer);
Var
  p: integer;
Begin
  If fMouseMode = 1 Then Begin
    p := x - 4;
    fPosition := math.min(1, math.max(0, p / (Width - 10)));
    If Assigned(OnChange) Then OnChange(self);
    Render;
  End;
End;

Procedure TCSlider.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
Begin
  fMouseMode := 0;
End;

Constructor TCSlider.Create(aOwner: TPaintBox);
Begin
  Inherited create(aOwner);
  fCanvas := aOwner.Canvas;
  OnChange := Nil;
  width := 75;
  Height := 25;
  fPosition := 0;
  fMouseMode := 0;
End;

Procedure TCSlider.Render;
Var
  l, p: integer;
Begin
  // Clear Background
  fCanvas.Pen.Color := clSilver;
  fCanvas.Brush.Color := clSilver;
  fCanvas.Brush.Style := bsSolid;
  fCanvas.Rectangle(Left, Top, Left + Width, Top + Height);
  // Render Sliderline
  fCanvas.Pen.Color := clBlack;
  fCanvas.MoveTo(left + 3, top + Height Div 2);
  fCanvas.LineTo(left + Width - 3, top + Height Div 2);
  // render Button
  l := Width - 10;
  p := 4 + round(l * fPosition);
  fCanvas.Brush.Color := clSilver;
  fCanvas.Brush.Style := bsSolid;
  fCanvas.Rectangle(Left + p - 4, Top, Left + p + 5, Top + Height);
End;

{ TTwoInputSliderNode }

Function TTwoInputSliderNode.DoThePixelOperation(Const Pixel1, Pixel2: TPixel;
  x, y: Single): TPixel;
Begin
  Result := DoTheSliderPixelOperation(Pixel1, Pixel2, x, y, fSlider.Position);
End;

Procedure TTwoInputSliderNode.OnSliderChange(Sender: TObject);
Begin
  If assigned(OnChange) Then OnChange(self);
  If assigned(OnHint) Then Begin
    onHint(self, GetHint());
  End;
End;

Procedure TTwoInputSliderNode.SetSliderPosition(AValue: Single);
Begin
  fslider.Position := AValue;
End;

Function TTwoInputSliderNode.GetSliderPosition: Single;
Begin
  result := fSlider.Position;
End;

Function TTwoInputSliderNode.GetHint: String;
Begin
  Result := Inherited GetHint + format('%1.3f', [fSlider.Position]);
End;

Procedure TTwoInputSliderNode.Setleft(AValue: integer);
Begin
  Inherited Setleft(AValue);
  fSlider.Left := AValue + 5;
End;

Procedure TTwoInputSliderNode.settop(AValue: integer);
Begin
  Inherited settop(AValue);
  fSlider.top := fTop + fHeight - 5 - fSlider.Height;
End;

Function TTwoInputSliderNode.DoTheSliderPixelOperation(Const Pixel1,
  Pixel2: TPixel; x, y, SliderValue: Single): TPixel;
Begin
  result := pixel1;
End;

Constructor TTwoInputSliderNode.Create(Owner: TPaintBox; x, y: integer);
Begin
  Inherited Create(Owner, x, y);
  fSlider := TCSlider.Create(Owner);
  fSlider.Width := fwidth - 10;
  fSlider.left := x + 5;
  fSlider.top := y + fHeight - 5 - fSlider.Height;
  fSlider.OnChange := @OnSliderChange;
End;

Destructor TTwoInputSliderNode.Destroy;
Begin
  fslider.free;
  Inherited Destroy;
End;

Procedure TTwoInputSliderNode.Render;
Begin
  Inherited Render;
  fSlider.Render;
End;

{ TOneInputSliderNode }

Function TOneInputSliderNode.DoThePixelOperation(Const Pixel1: TPixel; x,
  y: Single): TPixel;
Begin
  Result := DoTheSliderPixelOperation(Pixel1, x, y, fSlider.Position);
End;

Procedure TOneInputSliderNode.OnSliderChange(Sender: TObject);
Begin
  If Assigned(OnChange) Then OnChange(self);
  If assigned(OnHint) Then Begin
    onHint(self, GetHint());
  End;
End;

Procedure TOneInputSliderNode.SetSliderPosition(AValue: Single);
Begin
  fSlider.Position := AValue;
End;

Function TOneInputSliderNode.GetSliderPosition: Single;
Begin
  result := fSlider.Position;
End;

Function TOneInputSliderNode.GetHint: String;
Begin
  Result := Inherited GetHint + format('%1.3f', [fSlider.Position]);
End;

Procedure TOneInputSliderNode.Setleft(AValue: integer);
Begin
  Inherited Setleft(AValue);
  fSlider.Left := AValue + 5;
End;

Procedure TOneInputSliderNode.settop(AValue: integer);
Begin
  Inherited settop(AValue);
  fSlider.top := fTop + fHeight - 5 - fSlider.Height;
End;

Function TOneInputSliderNode.DoTheSliderPixelOperation(Const Pixel1: TPixel; x,
  y, SliderValue: Single): TPixel;
Begin
  result := Pixel1;
End;

Constructor TOneInputSliderNode.Create(Owner: TPaintBox; x, y: integer);
Begin
  Inherited Create(Owner, x, y);
  fSlider := TCSlider.Create(Owner);
  fSlider.Width := fwidth - 10;
  fSlider.left := x + 5;
  fSlider.top := y + fHeight - 5 - fSlider.Height;
  fSlider.OnChange := @OnSliderChange;
End;

Destructor TOneInputSliderNode.Destroy;
Begin
  fslider.free;
  Inherited Destroy;
End;

Procedure TOneInputSliderNode.Render;
Begin
  Inherited Render;
  fSlider.Render;
End;

{ TCButton }

Procedure TCButton.SetCaption(AValue: String);
Begin
  If fCaption = AValue Then Exit;
  fCaption := AValue;
  Render;
End;

Constructor TCButton.Create(aOwner: TPaintBox);
Begin
  Inherited create(aOwner);
  fCanvas := aOwner.Canvas;
  Top := 0;
  Height := 0;
  width := 75;
  Height := 25;
  fCaption := 'TCButton';
End;

Procedure TCButton.Render;
Begin
  fCanvas.Brush.Style := bsClear;
  fCanvas.Pen.Color := clBlack;
  fCanvas.Rectangle(left, top, left + Width, Top + Height);
  If fCaption <> '' Then Begin
    fCanvas.TextOut(
      left + (Width - fcanvas.TextWidth(fCaption)) Div 2,
      top + (Height - fCanvas.TextHeight(fCaption)) Div 2,
      fCaption
      );
  End;
End;

{ TImageSource }

Procedure TImageSource.LoadImage(Const Filename: String);
Var
  ext: String;
  p: TPortableNetworkGraphic;
  b: TBitmap;
  jp: TJPEGImage;
  TempIntfImg: TLazIntfImage;
  CurColor: TFPColor;
  i, j: Integer;
Begin
  fFilename := IncludeTrailingPathDelimiter(ExtractRelativepath(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))), IncludeTrailingPathDelimiter(ExtractFilePath(Filename)))) + ExtractFileName(Filename);
  ext := lowercase(ExtractFileExt(Filename));
  b := TBitmap.Create;
  Case ext Of
    '.png': Begin
        p := TPortableNetworkGraphic.Create;
        p.LoadFromFile(Filename);
        b.Assign(p);
        p.free;
      End;
    '.bmp': Begin
        b.LoadFromFile(Filename);
      End;
    '.jpg': Begin
        jp := TJPEGImage.Create;
        jp.LoadFromFile(Filename);
        b.Assign(jp);
        jp.free;
      End;
  End;
  fDimension := point(b.Width, b.Height);
  TempIntfImg := TLazIntfImage.Create(0, 0);
  TempIntfImg.LoadFromBitmap(b.Handle, b.MaskHandle);
  setlength(fPixels, b.Width, b.Height);
  For i := 0 To b.Width - 1 Do Begin
    For j := 0 To b.Height - 1 Do Begin
      CurColor := TempIntfImg.Colors[i, j];
      fPixels[i, j].r := (CurColor.red Shr 8) / 255;
      fPixels[i, j].g := (CurColor.green Shr 8) / 255;
      fPixels[i, j].b := (CurColor.blue Shr 8) / 255;
      fPixels[i, j].a := 1 - (CurColor.alpha Shr 8) / 255;
    End;
  End;
  TempIntfImg.free;
  b.free;
  caption := ExtractFileName(Filename);
End;

Procedure TImageSource.SetFilename(AValue: String);
Begin
  avalue := IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0))) + AValue;
  If FileExists(AValue) Then Begin
    LoadImage(AValue);
  End;
End;

Procedure TImageSource.Setleft(AValue: integer);
Begin
  Inherited Setleft(AValue);
  fButton.Left := AValue + 5;
End;

Procedure TImageSource.settop(AValue: integer);
Begin
  Inherited settop(AValue);
  fButton.top := fTop + fHeight - 5 - fButton.Height;
End;

Procedure TImageSource.OnLoadButtonClick(Sender: TObject);
Begin
  If FOpenDialog.Execute Then Begin
    LoadImage(FOpenDialog.FileName);
    If Assigned(OnChange) Then OnChange(self);
  End;
End;

Function TImageSource.DoThePixelOperation(Const Pixel: TPixel; x, y: Single
  ): TPixel;
Var
  i, j: integer;
Begin
  If (fDimension.x < 1) Or (fDimension.y < 1) Then Begin
    result := unodes.pixel(0, 0, 0, 1);
  End
  Else Begin
    i := min(fDimension.x - 1, max(0, round(fDimension.x * x)));
    j := min(fDimension.y - 1, max(0, round(fDimension.y * y)));
    result := fPixels[i, j];
  End;
End;

Constructor TImageSource.Create(Owner: TPaintBox; x, y: integer);
Var
  i: integer;
Begin
  Inherited Create(Owner, x, y);
  fPixels := Nil;
  fDimension := point(0, 0);
  fButton := TCButton.Create(Owner);
  fbutton.Width := fwidth - 10;
  fButton.left := x + 5;
  fButton.top := y + fHeight - 5 - fButton.Height;
  fButton.OnClick := @OnLoadButtonClick;
  fButton.Caption := 'Load';
  For i := 0 To high(fInConnectors) Do Begin
    fInConnectors[i].Free;
  End;
  setlength(fInConnectors, 0);
  FOpenDialog := TOpenDialog.Create(Nil);
  FOpenDialog.DefaultExt := '.bmp';
  FOpenDialog.Filter := 'Supported|*.bmp;*.jpg;*.png|All|*.*';
  FOpenDialog.FilterIndex := 0;
  Caption := '';
End;

Destructor TImageSource.Destroy;
Begin
  fButton.Free;
  FOpenDialog.free;
  Inherited Destroy;
End;

Procedure TImageSource.Render;
Begin
  Inherited Render;
  fButton.Render;
End;

Function TImageSource.GetImageDimension(): Tpoint;
Begin
  Result := fDimension;
End;

{ TTwoInputNode }

Procedure TTwoInputNode.InButtonClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  If assigned(OnInButtonClick) Then Begin
    OnInButtonClick(self);
  End;
End;

Procedure TTwoInputNode.InButton2Click(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  If assigned(OnInButton2Click) Then Begin
    OnInButton2Click(self);
  End;
End;

Function TTwoInputNode.DoThePixelOperation(Const Pixel1, Pixel2: TPixel; x,
  y: Single): TPixel;
Begin
  result := Pixel1;
End;

Constructor TTwoInputNode.Create(Owner: TPaintBox; x, y: integer);
Var
  c, i: Integer;
Begin
  Inherited Create(Owner, x, y);
  OnInButtonClick := Nil;
  OnInButton2Click := Nil;
  setlength(fInConnectors, 2);
  fInConnectors[0] := TConnector.Create(Owner, dLeft);
  fInConnectors[0].Left := fLeft - fInConnectors[0].Width;
  fInConnectors[0].OnMouseDown := @InButtonClick;
  fInConnectors[0].OnMouseUp := @InButtonClick;
  fInConnectors[1] := TConnector.Create(Owner, dLeft);
  fInConnectors[1].Left := fLeft - fInConnectors[0].Width;
  fInConnectors[1].OnMouseDown := @InButton2Click;
  fInConnectors[1].OnMouseUp := @InButton2Click;
  c := length(fInConnectors);
  For i := 0 To high(fInConnectors) Do Begin
    fInConnectors[i].top := fTop + (fHeight * (i + 1)) Div c - fHeight Div (2 * c) - fInConnectors[i].Height Div 2;
  End;
End;

Function TTwoInputNode.GetImageDimension(): Tpoint;
Var
  d1, d2: TPoint;
Begin
  d1 := point(0, 0);
  d2 := point(0, 0);
  If assigned(InNode) Then d1 := InNode.GetImageDimension();
  If assigned(In2Node) Then d2 := In2Node.GetImageDimension();
  result.x := max(d1.x, d2.x);
  result.y := max(d1.y, d2.y);
End;

Function TTwoInputNode.GetPixelValue(x, y: Single): TPixel;
Var
  p1, p2: TPixel;
Begin
  p1 := pixel(0, 0, 0, 0);
  p2 := pixel(0, 0, 0, 0);
  If assigned(InNode) Then p1 := InNode.GetPixelValue(x, y);
  If assigned(In2Node) Then p2 := In2Node.GetPixelValue(x, y);
  result := DoThePixelOperation(p1, p2, x, y);
End;

{ TImageDest }

Procedure TImageDest.Setleft(AValue: integer);
Begin
  Inherited Setleft(AValue);
  fButton.Left := AValue + 5;
End;

Procedure TImageDest.settop(AValue: integer);
Begin
  Inherited settop(AValue);
  fButton.top := fTop + fHeight - 5 - fButton.Height;
End;

Procedure TImageDest.OnSaveButtonClick(Sender: TObject);
  Function Cl(s: Single): integer;
  Begin
    result := round(s * 255);
    If result > 255 Then result := 255;
    If result < 0 Then result := 0;
  End;

Var
  p: Tpoint;
  bi: Tbitmap;
  v: TPixel;
  r, g, b, i, j: integer;
Begin
  p := GetImageDimension();
  If (p.x = 0) Or (p.y = 0) Then Begin
    ShowMessage('No Image to save.');
    exit;
  End;
  If FSaveDialog.execute Then Begin
    // Todo: einen Ordentlichen Algorithmus implementieren, siehe Load von TImageSource
    bi := Tbitmap.create;
    bi.Width := p.x;
    bi.Height := p.y;
    For i := 0 To bi.Width - 1 Do Begin
      For j := 0 To bi.Height - 1 Do Begin
        v := GetPixelValue(i / (p.x - 1), j / (p.y - 1));
        r := cl(v.r);
        g := cl(v.g);
        b := cl(v.b);
        bi.Canvas.pixels[i, j] := b Shl 16 Or g Shl 8 Or r;
      End;
    End;
    bi.SaveToFile(FSaveDialog.FileName);
    bi.free;
  End;
End;

Constructor TImageDest.Create(Owner: TPaintBox; x, y: integer);
Begin
  Inherited Create(Owner, x, y);
  fButton := TCButton.Create(Owner);
  fCloseButton.Free;
  fCloseButton := Nil;
  fOutConnector.Free;
  fOutConnector := Nil;
  caption := 'Final output';
  fbutton.Width := fwidth - 10;
  fButton.left := x + 5;
  fButton.top := y + fHeight - 5 - fButton.Height;
  fButton.OnClick := @OnSaveButtonClick;
  fButton.Caption := 'Save';
  FSaveDialog := TSaveDialog.Create(Nil);
  FSaveDialog.DefaultExt := '.bmp';
  FSaveDialog.Filter := 'Bitmap|*.bmp|All|*.*';
  FSaveDialog.FilterIndex := 0;
End;

Destructor TImageDest.Destroy;
Begin
  FSaveDialog.free;
  fButton.free;
  Inherited Destroy;
End;

Procedure TImageDest.Render;
Begin
  Inherited Render;
  fButton.render;
End;

Function TImageDest.CheckEndlesRecursion(TotalNodeCount: integer): Boolean;
Begin
  result := true;
  If Assigned(InNode) Then Begin
    result := Not InNode.RecursionCheck(0, 2 * TotalNodeCount);
  End;
End;

{ TOneInputNode }

Procedure TOneInputNode.InButtonClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  If assigned(OnInButtonClick) Then Begin
    OnInButtonClick(self);
  End;
End;

Function TOneInputNode.DoThePixelOperation(Const Pixel: TPixel; x, y: Single
  ): TPixel;
Begin
  result := Pixel; // irgend ein Quatsch, muss von Kindklasse überschrieben werden
End;

Constructor TOneInputNode.Create(Owner: TPaintBox; x, y: integer);
Begin
  Inherited Create(Owner, x, y);
  OnInButtonClick := Nil;
  setlength(fInConnectors, 1);
  fInConnectors[0] := TConnector.Create(Owner, dLeft);
  fInConnectors[0].Left := fLeft - fInConnectors[0].Width;
  fInConnectors[0].Top := fTop + (fHeight - fInConnectors[0].Height) Div 2;
  fInConnectors[0].OnMouseDown := @InButtonClick;
  fInConnectors[0].OnMouseUp := @InButtonClick;
End;

Function TOneInputNode.GetImageDimension(): Tpoint;
Begin
  Result := Inherited GetImageDimension();
  If assigned(InNode) Then result := InNode.GetImageDimension();
End;

Function TOneInputNode.GetPixelValue(x, y: Single): TPixel;
Var
  p1: TPixel;
Begin
  p1 := pixel(0, 0, 0, 0);
  If assigned(InNode) Then p1 := InNode.GetPixelValue(x, y);
  result := DoThePixelOperation(p1, x, y);
End;

{ TConnector }

Constructor TConnector.Create(aOwner: TPaintBox; Direction: TDirection);
Begin
  Inherited create(aOwner);
  fcanvas := owner.Canvas;
  fdirection := Direction;
  left := 0;
  top := 0;
  width := 10;
  height := 10;
End;

Procedure TConnector.Render;
Begin
  fcanvas.Pen.Color := clBlack;
  fcanvas.Brush.Color := clWhite;
  fcanvas.Brush.Style := bsSolid;
  If fdirection = dRight Then Begin
    // der Kleine Bobel
    fcanvas.Ellipse(left, top + Height Div 2 - 2, left + 4, top + Height Div 2 + 2);
    // Der Stuzen
    fcanvas.MoveTo(left + Width, top);
    fcanvas.LineTo(left + 4, top);
    fcanvas.LineTo(left + 4, top + Height - 1);
    fcanvas.LineTo(left + Width + 1, top + Height - 1);
  End
  Else Begin
    // der Kleine Bobel
    fcanvas.Ellipse(left + Width - 4, top + Height Div 2 - 2, left + Width, top + Height Div 2 + 2);
    // Der Stuzen
    fcanvas.MoveTo(left, top);
    fcanvas.LineTo(left + Width - 4, top);
    fcanvas.LineTo(left + Width - 4, top + Height - 1);
    fcanvas.LineTo(left - 1, top + Height - 1);
  End;
End;

{ TNode }

Procedure TNode.setCaption(AValue: String);
Begin
  If fCaption = AValue Then Exit;
  fCaption := AValue;
  Render;
End;

Procedure TNode.Setleft(AValue: integer);
Var
  i: Integer;
Begin
  If fleft = AValue Then Exit;
  fleft := AValue;
  If assigned(fCloseButton) Then Begin
    fCloseButton.Left := fleft + fwidth - fCloseButton.Width - 4;
  End;
  If assigned(fOutConnector) Then Begin
    fOutConnector.Left := fLeft + fwidth;
  End;
  For i := 0 To high(fInConnectors) Do Begin
    fInConnectors[i].Left := fLeft - fInConnectors[i].Width;
  End;
End;

Procedure TNode.settop(AValue: integer);
Var
  i, c: integer;
Begin
  If ftop = AValue Then Exit;
  ftop := AValue;
  If assigned(fCloseButton) Then Begin
    fCloseButton.Top := ftop + 4;
  End;
  If assigned(fOutConnector) Then Begin
    fOutConnector.Top := ftop + (fHeight - fOutConnector.Height) Div 2;
  End;
  c := length(fInConnectors);
  For i := 0 To high(fInConnectors) Do Begin
    fInConnectors[i].top := fTop + (fHeight * (i + 1)) Div c - fHeight Div (2 * c) - fInConnectors[i].Height Div 2;
  End;
End;

Procedure TNode.CloseButtonClick(Sender: TObject);
Begin
  If assigned(OnCloseButtonClick) Then Begin
    OnCloseButtonClick(self);
  End;
End;

Procedure TNode.OutConnectClick(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  If assigned(OnOutConnectClick) Then
    OnOutConnectClick(self);
End;

Function TNode.RecursionCheck(Level: integer; Border: integer): boolean;
Begin
  result := false;
  If (Level >= Border) Then Begin
    result := true;
    exit;
  End;
  If Assigned(InNode) Then Begin
    result := InNode.RecursionCheck(level + 1, Border);
  End;
  If Not result Then Begin
    If Assigned(In2Node) Then Begin
      result := In2Node.RecursionCheck(level + 1, Border);
    End;
  End;
End;

Function TNode.GetHint: String;
Begin
  result := '';
End;

Function TNode.GetClientRect: Trect;
Begin
  result.Left := fLeft;
  result.Right := fLeft + fwidth;
  result.Top := fTop;
  result.Bottom := fTop + fHeight;
End;

Constructor TNode.Create(Owner: TPaintBox; x, y: integer);
Begin
  Inherited create;
  fcaption := copy(ClassName, 2, length(ClassName));
  InNode := Nil;
  In2Node := Nil;
  OnCloseButtonClick := Nil;
  OnCloseButtonClick := Nil;
  OnChange := Nil;
  OnHint := Nil;
  fCanvas := Owner.Canvas;
  fLeft := x;
  fTop := y;
  fwidth := 100;
  fHeight := 60;
  fOutConnector := TConnector.Create(Owner, dRight);
  fOutConnector.Top := ftop + (fHeight - fOutConnector.Height) Div 2;
  fOutConnector.Left := fLeft + fwidth;
  fOutConnector.OnMouseDown := @OutConnectClick;
  fOutConnector.OnMouseUp := @OutConnectClick;
  fInConnectors := Nil;
  fCloseButton := TCButton.Create(Owner);
  fCloseButton.Caption := 'X';
  fCloseButton.Width := fCanvas.TextWidth('X') + 4;
  fCloseButton.Height := fCanvas.TextHeight('X');
  fCloseButton.Left := fleft + fwidth - fCloseButton.Width - 4;
  fCloseButton.Top := ftop + 4;
  fCloseButton.OnClick := @CloseButtonClick;
End;

Destructor TNode.Destroy;
Var
  i: Integer;
Begin
  If assigned(fCloseButton) Then Begin
    fCloseButton.Free;
  End;
  If Assigned(fOutConnector) Then Begin
    fOutConnector.Free;
  End;
  For i := 0 To high(fInConnectors) Do Begin
    fInConnectors[i].free;
  End;
End;

Procedure TNode.Render;
Var
  i, c: integer;
  p1, p2, p3, p4: TPoint;
Begin
  // Clear Background
  fCanvas.Brush.Color := clSilver;
  fCanvas.Brush.Style := bsSolid;
  fCanvas.Pen.Color := clBlack;
  fCanvas.Rectangle(fLeft, fTop, fLeft + fwidth, fTop + fHeight);
  If assigned(fOutConnector) Then Begin
    fOutConnector.Render();
  End;
  If assigned(fInConnectors) Then Begin
    For i := 0 To high(fInConnectors) Do Begin
      fInConnectors[i].Render;
    End;
  End;
  If caption <> '' Then Begin
    fCanvas.Brush.Style := bsClear;
    fcanvas.TextOut(fLeft + 3, fTop + 3, fCaption);
    c := fcanvas.TextHeight(fCaption);
    fcanvas.MoveTo(fLeft, fTop + 6 + c);
    fcanvas.Lineto(fLeft + fwidth, fTop + 6 + c);
  End;
  If assigned(fCloseButton) Then Begin
    fCloseButton.Render;
  End;
  // Wir Rendern eine Kurve von InNode(out) zu unserem In1
  If assigned(InNode) Then Begin
    c := length(fInConnectors);
    p1 := point(InNode.Left + InNode.Width + InNode.fOutConnector.Width - 1, inNode.top + InNode.Height Div 2);
    p4 := point(left - fInConnectors[0].Width Div 2 - 3, fTop + (fHeight * (0 + 1)) Div c - fHeight Div (2 * c));
    p2.y := p1.y;
    p3.y := p4.y;
    p2.x := p1.x + 50;
    p3.x := p4.x - 50;
    fCanvas.Pen.Color := clBlack;
    fCanvas.Pen.Width := 2;
    fCanvas.PolyBezier([p1, p2, p3, p4], false, false);
    fCanvas.Pen.Width := 1;
    fCanvas.Brush.Color := clWhite;
    fCanvas.Brush.Style := bsSolid;
    fCanvas.Ellipse(p1.x - 4, p1.y - 4, p1.x + 4, p1.y + 4);
    fCanvas.Ellipse(p4.x - 4, p4.y - 4, p4.x + 4, p4.y + 4);
  End;
  // Wir Rendern eine Kurve von In2Node(out) zu unserem In2
  If assigned(In2Node) Then Begin
    c := length(fInConnectors);
    p1 := point(In2Node.Left + In2Node.Width + In2Node.fOutConnector.Width - 1, in2Node.top + In2Node.Height Div 2);
    p4 := point(left - fInConnectors[0].Width Div 2 - 3, fTop + (fHeight * (1 + 1)) Div c - fHeight Div (2 * c));
    p2.y := p1.y;
    p3.y := p4.y;
    p2.x := p1.x + 50;
    p3.x := p4.x - 50;
    fCanvas.Pen.Color := clBlack;
    fCanvas.Pen.Width := 2;
    fCanvas.PolyBezier([p1, p2, p3, p4], false, false);
    fCanvas.Pen.Width := 1;
    fCanvas.Brush.Color := clWhite;
    fCanvas.Brush.Style := bsSolid;
    fCanvas.Ellipse(p1.x - 4, p1.y - 4, p1.x + 4, p1.y + 4);
    fCanvas.Ellipse(p4.x - 4, p4.y - 4, p4.x + 4, p4.y + 4);
  End;
End;

Function TNode.GetImageDimension(): Tpoint;
Begin
  result := point(0, 0);
End;

End.

