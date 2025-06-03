(******************************************************************************)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* This file is part of PixelEditor                                           *)
(*                                                                            *)
(*  See the file license.md, located under:                                   *)
(*  https://github.com/PascalCorpsman/Software_Licenses/blob/main/license.md  *)
(*  for details about the license.                                            *)
(*                                                                            *)
(*               It is not allowed to change or remove this text from any     *)
(*               source file of the project.                                  *)
(*                                                                            *)
(******************************************************************************)
Unit upixeleditorlcl;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, controls, SysUtils, OpenGLContext, uopengl_widgetset, ugraphics,
  uopengl_graphikengine, ExtCtrls, Graphics;

Type

  TColorEvent = Procedure(Const Color: TRGBA) Of Object;

  { TOpenGL_Bevel }

  TOpenGL_Bevel = Class(TOpenGl_Image)
  protected
    fmDown: Boolean;
    fStyle: TBevelStyle;
    Procedure OnRender(); override;

    Procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    Procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

  public
    RaisedColor: TRGBA;
    LoweredColor: TRGBA;

    Property Style: TBevelStyle read fStyle write fStyle;

    Procedure Click; override;

    Constructor Create(aOwner: TOpenGLControl); override;
  End;

  { TOpenGL_ToggleButton }

  TOpenGL_ToggleButton = Class(TOpenGL_Bevel)
  protected
    FDownimage: TGraphikItem;
    Procedure Click; override;
    Procedure DblClick; override;
    Procedure OnRender(); override;
    Procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

  public
    Procedure SetDownImage(OpenGLIndex: integer); overload;
  End;

  { TOpenGL_ColorBox }

  TOpenGL_ColorBox = Class(TOpenGL_BaseClass)
  private
    fLoweredColor: TRGBA;
    fRaisedColor: TRGBA;
    fDefaultColor: TRGBA;
  protected
    fStyle: TBevelStyle;
    fColor: TRGBA;
    fmDown: Boolean;
    Procedure OnRender(); override;

    Procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    Procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    Function getcolor: TRGBA; virtual;
    Procedure setColor(AValue: TRGBA); virtual;
    Function getDefaultColor: TRGBA; virtual;
    Procedure setDefaultColor(AValue: TRGBA); virtual;
    Function getLoweredColor: TRGBA; virtual;
    Function getRaisedColor: TRGBA; virtual;
    Procedure setLoweredColor(AValue: TRGBA); virtual;
    Procedure setRaisedColor(AValue: TRGBA); virtual;
  public
    Property DefaultColor: TRGBA read getDefaultColor write setDefaultColor;
    Property RaisedColor: TRGBA read getLoweredColor write setRaisedColor;
    Property LoweredColor: TRGBA read getRaisedColor write setLoweredColor;

    Property Color: TRGBA read getcolor write setColor;
    Property Style: TBevelStyle read fStyle write fStyle;

    Property OnClick;
    Property OnDblClick;
    Property OnMouseUp;
    Property OnMouseDown;

    Constructor Create(aOwner: TOpenGLControl); override;
  End;

  { TOpenGL_ForeBackGroundColorBox }

  TOpenGL_ForeBackGroundColorBox = Class(TOpenGl_BaseClass)
  private
  protected
    Procedure OnRender(); override;
  public
    FrontColor: TRGBA;
    BackColor: TRGBA;
    Constructor Create(aOwner: TOpenGLControl); override;
  End;

  TColorBoxEvent = Procedure(Const C: TOpenGL_ColorBox) Of Object;

  { TOpenGL_Textbox }

  TOpenGL_Textbox = Class(TOpenGl_Label)
  protected
    Procedure Setcaption(value: String); override;
    Procedure OnRender(); override;
  public
    Alignment: TAlignment;
    Layout: TTextLayout;
    BorderColor: TRGBA;
    BackColor: TRGBA;
    Constructor Create(aOwner: TOpenGLControl; FontFile: String); override;
  End;

  TDelta = Record
    r, g, b: integer;
  End;

  { TPlus }

  TPlus = Class(TOpenGl_BaseClass)
  protected
    Procedure OnRender(); override;
    Procedure OnClickEvent(Sender: TObject);
  public
    Color: TRGBA;
    Target: TOpenGL_ColorBox;
    Delta: TDelta;
    OnUpdate: TColorBoxEvent;
    Constructor Create(aOwner: TOpenGLControl); override;
  End;

  { TMinus }

  TMinus = Class(TPlus)
  protected
    Procedure OnRender(); override;
  End;

  { TOpenGL_ColorPicDialog }

  TOpenGL_ColorPicDialog = Class(TOpenGl_Image)
  private
    Procedure ApplyColor(Const Color: TRGBA); overload;
    Procedure ApplyColor(Const CB: TOpenGL_ColorBox); overload;
    Function getShower: TOpenGL_ColorBox;
    Procedure setOnLoadColorPalette(AValue: TNotifyEvent);
    Procedure setOnSaveColorPalette(AValue: TNotifyEvent);
  protected
    fSelectorTex: TGraphikItem;
    fColorTable: TOpenGl_Image;
    fColorTableRaw: TBitmap;
    fColorInfo: TOpenGl_Label;
    fPicColorButton: TOpenGL_Textbox;
    fResetButton: TOpenGL_Textbox;

    fBlack: TOpenGL_ColorBox;
    fDarken: TOpenGL_ColorBox;
    fRed: TOpenGL_ColorBox;
    fGreen: TOpenGL_ColorBox;
    fBlue: TOpenGL_ColorBox;
    fBrighten: TOpenGL_ColorBox;
    fWhite: TOpenGL_ColorBox;

    fDarkenPlus: TPlus;
    fRedPlus: TPlus;
    fGreenPlus: TPlus;
    fBluePlus: TPlus;
    fWhitePlus: TPlus;

    fDarkenMinus: TMinus;
    fRedMinus: TMinus;
    fGreenMinus: TMinus;
    fBlueMinus: TMinus;
    fWhiteMinus: TMinus;
    fShower: TOpenGL_ColorBox;

    fOpenButton: TOpenGL_Textbox;
    fSaveAsButton: TOpenGL_Textbox;
    fSetValuesButton: TOpenGL_Textbox;
    fColorAsHex: Boolean;

    Procedure OnRender(); override;
    Procedure SetVisible(AValue: Boolean); override;
    Procedure SetLeft(AValue: integer); override;
    Procedure SetTop(AValue: integer); override;

    Procedure OnPicColorClick(Sender: TObject);
    Procedure OnColorDBLClick(Sender: TObject);
    Procedure OnResetColorClick(Sender: TObject);
    Procedure OnSetColorValuesClick(Sender: TObject);
    Procedure OpenColorTableMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    Procedure OnColorClick(Sender: TObject);

  public
    CriticalError: String;
    SelectorPos: integer;
    OnSetColor: TColorBoxEvent;
    Property OnSaveColorPalette: TNotifyEvent write setOnSaveColorPalette;
    Property OnLoadColorPalette: TNotifyEvent write setOnLoadColorPalette;

    Property Shower: TOpenGL_ColorBox read getShower;

    Constructor Create(aOwner: TOpenGLControl); override;
    Destructor Destroy; override;
    Procedure LoadColor(aColor: TOpenGL_ColorBox; aColorAsHex: Boolean);
  End;

Procedure RenderTransparentQuad(x, y, w, h: Single);

Implementation

Uses
  math, FileUtil, Forms, StdCtrls
  , dglOpenGL
  , uvectormath
  , upixeleditor_types
  ;

Type

  { TRGBDialog }

  TRGBDialog = Class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Button1: TButton;
    Button2: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
  private
    Procedure OnEditKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
  public
    Constructor CreateNew(AOwner: TComponent; Num: Integer = 0); override;
  End;

Function Delta(r, g, b: integer): TDelta;
Begin
  result.r := r;
  result.g := g;
  result.b := b;
End;

Procedure RenderTransparentQuad(x, y, w, h: Single);
Begin
  glColor3ub(TransparentDarkLuminance, TransparentDarkLuminance, TransparentDarkLuminance);
  glBegin(GL_QUADS);
  glVertex2f(x, y);
  glVertex2f(x + w / 2, y);
  glVertex2f(x + w / 2, y + h / 2);
  glVertex2f(x, y + h / 2);
  glVertex2f(x + w / 2, y + h / 2);
  glVertex2f(x + w, y + h / 2);
  glVertex2f(x + w, y + h);
  glVertex2f(x + w / 2, y + h);
  glend;
  glColor3ub(TransparentBrightLuminance, TransparentBrightLuminance, TransparentBrightLuminance);
  glBegin(GL_QUADS);
  glVertex2f(x + w / 2, y);
  glVertex2f(x + w, y);
  glVertex2f(x + w, y + h / 2);
  glVertex2f(x + w / 2, y + h / 2);
  glVertex2f(x, y + h / 2);
  glVertex2f(x + w / 2, y + h / 2);
  glVertex2f(x + w / 2, y + h);
  glVertex2f(x, y + h);
  glend;
End;

{ TRGBDialog }

Procedure TRGBDialog.OnEditKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  If key = 13 { VK_Return } Then button2.Click;
  If key = 27 { VK_ESCAPE } Then button1.Click;
End;

Constructor TRGBDialog.CreateNew(AOwner: TComponent; Num: Integer);
Begin
  Inherited CreateNew(AOwner, Num);
  caption := 'Edit color';
  BorderStyle := bsDialog;
  Position := poScreenCenter;
  Color := clGray;
  width := Scale96ToScreen(200);
  Height := Scale96ToScreen(100);
  Constraints.MinWidth := Width;
  Constraints.MaxWidth := Width;
  Constraints.MinHeight := Height;
  Constraints.MaxHeight := Height;
  label1 := TLabel.Create(self);
  label1.name := 'Label1';
  label1.Parent := self;
  label1.Caption := 'R-Value';
  label1.Left := Scale96ToScreen(8);
  label1.Top := Scale96ToScreen(8);
  Edit1 := TEdit.Create(self);
  Edit1.name := 'Edit1';
  Edit1.Parent := self;
  edit1.left := Scale96ToScreen(8);
  edit1.Width := Scale96ToScreen(50);
  edit1.Top := label1.top + Label1.Height + Scale96ToScreen(8);
  Edit2 := TEdit.Create(self);
  Edit2.name := 'Edit2';
  Edit2.Parent := self;
  edit2.left := edit1.Left + edit1.Width + Scale96ToScreen(8);
  edit2.Width := Scale96ToScreen(50);
  edit2.Top := label1.top + Label1.Height + Scale96ToScreen(8);
  Edit3 := TEdit.Create(self);
  Edit3.name := 'Edit3';
  Edit3.Parent := self;
  edit3.left := Edit2.left + edit2.Width + Scale96ToScreen(8);
  edit3.Width := Scale96ToScreen(50);
  edit3.Top := label1.top + Label1.Height + Scale96ToScreen(8);
  label2 := TLabel.Create(self);
  label2.name := 'Label2';
  label2.Parent := self;
  label2.Caption := 'G-Value';
  label2.Left := Edit2.Left;
  label2.Top := Scale96ToScreen(8);
  label3 := TLabel.Create(self);
  label3.name := 'Label3';
  label3.Parent := self;
  label3.Caption := 'B-Value';
  label3.Left := Edit3.Left;
  label3.Top := Scale96ToScreen(8);
  Button1 := TButton.Create(self);
  Button1.name := 'Button1';
  Button1.Parent := self;
  Button1.Left := Scale96ToScreen(8);
  Button1.Top := Edit1.Top + Edit1.Height + Scale96ToScreen(8);
  Button1.Caption := 'Cancel';
  Button1.ModalResult := mrCancel;
  Button2 := TButton.Create(self);
  Button2.name := 'Button2';
  Button2.Parent := self;
  Button2.Left := width - button2.Width - Scale96ToScreen(8);
  Button2.Top := Edit1.Top + Edit1.Height + Scale96ToScreen(8);
  Button2.Caption := 'OK';
  Button2.ModalResult := mrOK;
  label1.font.color := clSilver;
  label2.font.color := clSilver;
  label3.font.color := clSilver;
  Button1.Color := clGray;
  Button1.font.Color := clSilver;
  Button2.Color := clGray;
  Button2.font.Color := clSilver;
  Edit1.Color := clGray;
  Edit1.font.Color := clSilver;
  Edit2.Color := clGray;
  Edit2.font.Color := clSilver;
  Edit3.Color := clGray;
  Edit3.font.Color := clSilver;
  edit1.OnKeyDown := @OnEditKeyDown;
  edit2.OnKeyDown := @OnEditKeyDown;
  edit3.OnKeyDown := @OnEditKeyDown;
End;

{ TOpenGL_Bevel }

Procedure TOpenGL_Bevel.OnRender;
Var
  p: TPoint;
Begin
  If Not Visible Then exit;
  Inherited OnRender();
  glBindTexture(GL_TEXTURE_2D, 0);
  glPushMatrix;
  glTranslatef(Left, Top, 0.01);
  If (fStyle = bsRaised) Or (fmDown) Then Begin
    glColor3ub(RaisedColor.r, RaisedColor.g, RaisedColor.b);
  End
  Else Begin
    glColor3ub(LoweredColor.r, LoweredColor.g, LoweredColor.b);
  End;
  glLineWidth(max(FOwner.Width / ScreenWidth, FOwner.Height / ScreenHeight) * 2);
  // glLineWidth(max(FOwner.Width / 640, FOwner.Height / 480) * 1); // Debug, zum Ausmessen der Positionen !
  glBegin(GL_LINE_LOOP);
  glVertex2f(0, 1);
  glVertex2f(Width - 1, 1);
  glVertex2f(Width - 1, Height);
  glVertex2f(0, Height);
  glend;
  glLineWidth(1);
  glPopMatrix;
  If fShowHint And (hint <> '') Then Begin
    p := fOwner.ScreenToClient(Mouse.CursorPos);
    p.x := round(p.x * ScreenWidth / fOwner.Width);
    p.Y := round(p.Y * ScreenHeight / fOwner.Height);
    RenderHint(p, hint);
  End;
End;

Procedure TOpenGL_Bevel.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
Begin
  Inherited MouseDown(Button, Shift, X, Y);
  fmDown := ssLeft In Shift;
End;

Procedure TOpenGL_Bevel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
Begin
  Inherited MouseUp(Button, Shift, X, Y);
  fmDown := false;
End;

Procedure TOpenGL_Bevel.Click;
Begin
  Inherited Click;
End;

Constructor TOpenGL_Bevel.Create(aOwner: TOpenGLControl);
Begin
  Inherited Create(aOwner);
  IgnoreDepthtest := false;
  fStyle := bsLowered;
  fmDown := false;
  RaisedColor := RGBA($FF, $FF, 0, AlphaOpaque);
  LoweredColor := RGBA(0, 0, 0, AlphaOpaque);
End;

{ TOpenGL_ToggleButton }

Procedure TOpenGL_ToggleButton.Click;
Begin
  If fStyle = bsLowered Then Begin
    fStyle := bsRaised;
  End
  Else Begin
    fStyle := bsLowered;
  End;
  Inherited Click();
End;

Procedure TOpenGL_ToggleButton.DblClick;
Begin
  Click;
End;

Procedure TOpenGL_ToggleButton.OnRender;
Var
  tmp: TGraphikItem;
  bool: Boolean;
Begin
  bool := fStyle = bsRaised;
  fStyle := bsLowered;
  If bool Then Begin
    tmp := Fimage;
    Fimage := FDownimage;
  End;
  Inherited OnRender();
  If bool Then Begin
    fStyle := bsRaised;
    Fimage := tmp;
  End;
End;

Procedure TOpenGL_ToggleButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Begin
  Inherited MouseDown(Button, Shift, X, Y);
  fmDown := false;
End;

Procedure TOpenGL_ToggleButton.SetDownImage(OpenGLIndex: integer);
Begin
  fDownImage := OpenGL_GraphikEngine.GetInfo(OpenGLIndex);
End;

{ TOpenGL_ColorBox }

Procedure TOpenGL_ColorBox.setLoweredColor(AValue: TRGBA);
Begin
  If fLoweredColor = AValue Then Exit;
  fLoweredColor := AValue;
End;

Procedure TOpenGL_ColorBox.setRaisedColor(AValue: TRGBA);
Begin
  If fRaisedColor = AValue Then Exit;
  fRaisedColor := AValue;
End;

Function TOpenGL_ColorBox.getcolor: TRGBA;
Begin
  result := fColor;
End;

Procedure TOpenGL_ColorBox.setColor(AValue: TRGBA);
Begin
  fColor := AValue;
End;

Function TOpenGL_ColorBox.getDefaultColor: TRGBA;
Begin
  result := fDefaultColor;
End;

Procedure TOpenGL_ColorBox.setDefaultColor(AValue: TRGBA);
Begin
  fDefaultColor := AValue;
End;

Procedure TOpenGL_ColorBox.OnRender;
var
  p: TPoint;
Begin
  If Not Visible Then exit;
  glBindTexture(GL_TEXTURE_2D, 0);

  glPushMatrix;
  glTranslatef(Left, Top, 0);

  glLineWidth(max(FOwner.Width / 640, FOwner.Height / 480) * 2);
  // glLineWidth(max(FOwner.Width / 640, FOwner.Height / 480) * 1); // Debug, zum Ausmessen der Positionen !

  If fColor = ColorTransparent Then Begin
    RenderTransparentQuad(0, 0, Width - 1, Height - 1);
  End
  Else Begin
    glColor3ub(fColor.r, fColor.g, fColor.b);
    glBegin(GL_QUADS);
    glVertex2f(0, 0);
    glVertex2f(Width - 1, 0);
    glVertex2f(Width - 1, Height - 1);
    glVertex2f(0, Height - 1);
    glend;
  End;

  If (fStyle = bsRaised) Or (fmDown) Then Begin
    glColor3ub(RaisedColor.r, RaisedColor.g, RaisedColor.b);
  End
  Else Begin
    glColor3ub(LoweredColor.r, LoweredColor.g, LoweredColor.b);
  End;
  glTranslatef(0, 0, 0.01);
  glBegin(GL_LINE_LOOP);
  glVertex2f(0, 1);
  glVertex2f(Width - 1, 1);
  glVertex2f(Width - 1, Height);
  glVertex2f(0, Height);
  glend;

  glLineWidth(1);
  glPopMatrix;

  If fShowHint And (hint <> '') Then Begin
    p := fOwner.ScreenToClient(Mouse.CursorPos);
    p.x := round(p.x * ScreenWidth / fOwner.Width);
    p.Y := round(p.Y * ScreenHeight / fOwner.Height);
    RenderHint(p, hint);
  End;
End;

Procedure TOpenGL_ColorBox.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
Begin
  Inherited MouseDown(Button, Shift, X, Y);
  fmDown := ssLeft In Shift;
End;

Procedure TOpenGL_ColorBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
Begin
  Inherited MouseUp(Button, Shift, X, Y);
  fmDown := false;
End;

Function TOpenGL_ColorBox.getLoweredColor: TRGBA;
Begin
  result := fLoweredColor;
End;

Function TOpenGL_ColorBox.getRaisedColor: TRGBA;
Begin
  result := RaisedColor;
End;

Constructor TOpenGL_ColorBox.Create(aOwner: TOpenGLControl);
Begin
  Inherited Create(aOwner);
  fColor := RGBA(0, 0, 0, AlphaOpaque);
  RaisedColor := RGBA($FF, $FF, 0, AlphaOpaque);
  LoweredColor := RGBA(0, 0, 0, AlphaOpaque);
  IgnoreDepthtest := false;
End;

{ TOpenGL_ForeBackGroundColorBox }

Procedure TOpenGL_ForeBackGroundColorBox.OnRender();
Var
  w, h, B1, B2: integer;
Begin
  If Not Visible Then exit;
  glBindTexture(GL_TEXTURE_2D, 0);
  B1 := width Div 7; // Durch Probieren ermittelt ;)
  B2 := 2 * b1;
  glPushMatrix;
  glTranslatef(Left, Top, 0);
  glLineWidth(max(FOwner.Width / 640, FOwner.Height / 480) * 2);
  // Die Hintergrundfarbe
  If BackColor = ColorTransparent Then Begin
    w := Width - B1 - B2;
    h := Height - B1 - B2;
    RenderTransparentQuad(b2, b2, w, h);
  End
  Else Begin
    glColor3ub(BackColor.r, BackColor.g, BackColor.b);
    glBegin(GL_QUADS);
    glVertex2f(B2, B2);
    glVertex2f(Width - B1, B2);
    glVertex2f(Width - B1, Height - B1);
    glVertex2f(B2, Height - B1);
    glend;
  End;
  glColor3ub(192, 192, 192);
  glBegin(GL_LINE_LOOP);
  glVertex2f(B2, B2);
  glVertex2f(Width - B1, B2);
  glVertex2f(Width - B1, Height - B1);
  glVertex2f(B2, Height - B1);
  glend;
  // Die Fordergrundfarbe
  glTranslatef(0, 0, 0.01);
  If FrontColor = ColorTransparent Then Begin
    w := Width - B1 - B2;
    h := Height - B1 - B2;
    RenderTransparentQuad(b1, b1, w, h);
  End
  Else Begin
    glColor3ub(FrontColor.r, FrontColor.g, FrontColor.b);
    glBegin(GL_QUADS);
    glVertex2f(B1, B1);
    glVertex2f(Width - B2, B1);
    glVertex2f(Width - B2, Height - B2);
    glVertex2f(B1, Height - B2);
    glend;
  End;

  glColor3ub(192, 192, 192);
  glBegin(GL_LINE_LOOP);
  glVertex2f(B1, B1);
  glVertex2f(Width - B2, B1);
  glVertex2f(Width - B2, Height - B2);
  glVertex2f(B1, Height - B2);
  glend;

  // Der Rahmen
  glColor3ub(192, 192, 192);
  glBegin(GL_LINE_LOOP);
  glVertex2f(0, 1);
  glVertex2f(Width - 1, 1);
  glVertex2f(Width - 1, Height);
  glVertex2f(0, Height);
  glend;
  glLineWidth(1);
  glPopMatrix;
End;

Constructor TOpenGL_ForeBackGroundColorBox.Create(aOwner: TOpenGLControl);
Begin
  Inherited Create(aOwner);
  FrontColor := RGBA(0, 0, 0, AlphaOpaque);
  BackColor := RGBA(1, 1, 1, AlphaOpaque);
  IgnoreDepthtest := false;
End;

{ TOpenGL_Textbox }

Procedure TOpenGL_Textbox.Setcaption(value: String);
Begin
  // inherited Setcaption(value); -- Das würde die Größe anpassen, was wir hier explizit nicht wollen !
  fcaption := value;
End;

Procedure TOpenGL_Textbox.OnRender;
Var
  p: TPoint;
Begin
  If BackColor.a <> 255 Then Begin
    glBindTexture(GL_TEXTURE_2D, 0);
    glPushMatrix;
    glTranslatef(Left, Top, 0);
    glColor3ub(BackColor.r, BackColor.g, BackColor.b);
    glBegin(GL_QUADS);
    glVertex2f(0, 1);
    glVertex2f(Width - 1, 1);
    glVertex2f(Width - 1, Height);
    glVertex2f(0, Height);
    glend;
    glPopMatrix;
  End;
  glPushMatrix;
  Case Layout Of
    tlTop: Begin
        // Nichts zu tun
      End;
    tlCenter: Begin
        gltranslatef(0, (Height - FFont.TextHeight(fcaption)) / 2 + 1, 0);
      End;
    tlBottom: Begin
        gltranslatef(0, (Height - FFont.TextHeight(fcaption)), 0);
      End;
  End;
  Case Alignment Of
    taLeftJustify: Begin
        // Nichts zu tun
      End;
    taCenter: Begin
        gltranslatef((Width - FFont.TextWidth(fcaption)) / 2 - 1, 0, 0);
      End;
    taRightJustify: Begin
        gltranslatef((Width - FFont.TextWidth(fcaption)), 0, 0);
      End;
  End;
  Inherited OnRender();
  glPopMatrix;
  // Den Rahmen Rendern
  glBindTexture(GL_TEXTURE_2D, 0);
  glPushMatrix;
  glTranslatef(Left, Top, 0.01);
  glColor3ub(BorderColor.r, BorderColor.g, BorderColor.b);
  glLineWidth(max(FOwner.Width / ScreenWidth, FOwner.Height / ScreenHeight) * 2);
  // glLineWidth(max(FOwner.Width / 640, FOwner.Height / 480) * 1); // Debug, zum Ausmessen der Positionen !
  glBegin(GL_LINE_LOOP);
  glVertex2f(0, 1);
  glVertex2f(Width - 1, 1);
  glVertex2f(Width - 1, Height);
  glVertex2f(0, Height);
  glend;
  glLineWidth(1);
  glPopMatrix;
  If fShowHint And (hint <> '') Then Begin
    p := fOwner.ScreenToClient(Mouse.CursorPos);
    p.x := round(p.x * ScreenWidth / fOwner.Width);
    p.Y := round(p.Y * ScreenHeight / fOwner.Height);
    RenderHint(p, hint);
  End;
End;

Constructor TOpenGL_Textbox.Create(aOwner: TOpenGLControl; FontFile: String);
Begin
  Inherited Create(aOwner, FontFile);
  Layout := tlTop;
  Alignment := taLeftJustify;
  BorderColor := RGBA(0, 0, 0, AlphaOpaque);
  BackColor := RGBA(0, 0, 0, AlphaTranslucent);
  IgnoreDepthtest := false;
End;

{ TPlus }

Procedure TPlus.OnRender;
Begin
  glPushMatrix;
  glTranslatef(left, top, 0);
  glColor4ub(192, 192, 192, 0);
  glBegin(GL_QUADS);
  glVertex2f(0, Height / 2 - 3);
  glVertex2f(Width, Height / 2 - 3);
  glVertex2f(Width, Height / 2 + 3);
  glVertex2f(0, Height / 2 + 3);
  glVertex2f(Width / 2 - 3, 0);
  glVertex2f(Width / 2 + 3, 0);
  glVertex2f(Width / 2 + 3, Height);
  glVertex2f(Width / 2 - 3, Height);
  glend;
  glColor4ub(Color.r, Color.g, Color.b, 0);
  glTranslatef(0, 0, 0.01);
  glBegin(GL_QUADS);
  glVertex2f(1, Height / 2 - 2);
  glVertex2f(Width - 1, Height / 2 - 2);
  glVertex2f(Width - 1, Height / 2 + 2);
  glVertex2f(1, Height / 2 + 2);
  glVertex2f(Width / 2 - 2, 1);
  glVertex2f(Width / 2 + 2, 1);
  glVertex2f(Width / 2 + 2, Height - 1);
  glVertex2f(Width / 2 - 2, Height - 1);
  glend;
  glPopMatrix;
End;

Procedure TPlus.OnClickEvent(Sender: TObject);
Begin
  If assigned(Target) Then Begin
    Target.Color := ClampAdd(Target.Color, delta.r, delta.g, delta.b);
    If assigned(OnUpdate) Then Begin
      OnUpdate(Target);
    End;
  End;
End;

Constructor TPlus.Create(aOwner: TOpenGLControl);
Begin
  Inherited Create(aOwner);
  Color := RGBA(0, 0, 0, AlphaOpaque);
  Target := Nil;
  Delta.r := 0;
  Delta.g := 0;
  Delta.b := 0;
  OnUpdate := Nil;
  OnClick := @OnClickEvent;
  OnDblClick := @OnClickEvent;
  IgnoreDepthtest := false;
End;

{ TMinus }

Procedure TMinus.OnRender();
Begin
  glPushMatrix;
  glTranslatef(left, top, 0);
  glColor4ub(192, 192, 192, 0);
  glBegin(GL_QUADS);
  glVertex2f(0, Height / 2 - 3);
  glVertex2f(Width, Height / 2 - 3);
  glVertex2f(Width, Height / 2 + 3);
  glVertex2f(0, Height / 2 + 3);
  glend;
  glColor4ub(Color.r, Color.g, Color.b, 0);
  glTranslatef(0, 0, 0.01);
  glBegin(GL_QUADS);
  glVertex2f(1, Height / 2 - 2);
  glVertex2f(Width - 1, Height / 2 - 2);
  glVertex2f(Width - 1, Height / 2 + 2);
  glVertex2f(1, Height / 2 + 2);
  glend;
  glPopMatrix;
End;

{ TOpenGL_ColorPicDialog }

Procedure TOpenGL_ColorPicDialog.ApplyColor(Const Color: TRGBA);
Var
  s, t: String;
  i: integer;
Begin
  fDarken.Color := ClampAdd(Color, -30, -30, -30);
  fRed.Color := ClampAdd(Color, 30, 0, 0);
  fGreen.Color := ClampAdd(Color, 0, 30, 0);
  fBlue.Color := ClampAdd(Color, 0, 0, 30);
  fBrighten.Color := ClampAdd(Color, 30, 30, 30);
  fPicColorButton.FontColor := v3(Color.r / 255, Color.g / 255, Color.b / 255);
  s := RGBAToFormatString(Color, fColorAsHex);
  i := pos('(', s);
  t := copy(s, i, length(s));
  fColorInfo.caption := copy(s, 1, i - 2) + LineEnding + t.PadLeft(i - 2);
End;

Procedure TOpenGL_ColorPicDialog.ApplyColor(Const CB: TOpenGL_ColorBox);
Begin
  ApplyColor(cb.Color);
End;

Function TOpenGL_ColorPicDialog.getShower: TOpenGL_ColorBox;
Begin
  result := fShower;
End;

Procedure TOpenGL_ColorPicDialog.setOnLoadColorPalette(AValue: TNotifyEvent);
Begin
  fOpenButton.OnClick := AValue;
End;

Procedure TOpenGL_ColorPicDialog.setOnSaveColorPalette(AValue: TNotifyEvent);
Begin
  fSaveAsButton.OnClick := AValue;
End;

Procedure TOpenGL_ColorPicDialog.OnRender;
Begin
  Inherited OnRender();
  glPushMatrix;
  glTranslatef(0, 0, 0.01);
  // Rendern der Kind Komponenten
  fColorTable.Render();
  fPicColorButton.Render();
  fColorInfo.Render();
  fBlack.render();
  fDarken.render();
  fRed.render();
  fGreen.render();
  fBlue.render();
  fBrighten.render();
  fWhite.render();
  fDarkenMinus.Render();
  fRedMinus.Render();
  fGreenMinus.Render();
  fBlueMinus.Render();
  fWhiteMinus.Render();
  fDarkenPlus.Render();
  fRedPlus.Render();
  fGreenPlus.Render();
  fBluePlus.Render();
  fWhitePlus.Render();
  fSaveAsButton.Render();
  fOpenButton.Render();
  fResetButton.Render();
  fSetValuesButton.Render();

  // Den kleinen Auswahlzeiger malen ;)
  glPushMatrix;
  glColor4f(1, 1, 1, 1);
  glTranslatef(Left, Top, 0);
  glTranslatef(2 + 8 + 35 * SelectorPos, height - 10, 0);
  RenderAlphaQuad(0, 0, fSelectorTex);
  glPopMatrix;
  glPopMatrix;
End;

Procedure TOpenGL_ColorPicDialog.SetVisible(AValue: Boolean);
Begin
  Inherited SetVisible(AValue);
  fColorTable.Visible := AValue;
  fPicColorButton.Visible := AValue;
  fColorInfo.Visible := AValue;
  fBlack.Visible := AValue;
  fDarken.Visible := AValue;
  fRed.Visible := AValue;
  fGreen.Visible := AValue;
  fBlue.Visible := AValue;
  fBrighten.Visible := AValue;
  fWhite.Visible := AValue;
  fDarkenMinus.Visible := AValue;
  fRedMinus.Visible := AValue;
  fGreenMinus.Visible := AValue;
  fBlueMinus.Visible := AValue;
  fWhiteMinus.Visible := AValue;
  fDarkenPlus.Visible := AValue;
  fRedPlus.Visible := AValue;
  fGreenPlus.Visible := AValue;
  fBluePlus.Visible := AValue;
  fWhitePlus.Visible := AValue;
  fOpenButton.Visible := AValue;
  fSaveAsButton.Visible := AValue;
  fResetButton.Visible := AValue;
  fSetValuesButton.Visible := AValue;
End;

Procedure TOpenGL_ColorPicDialog.SetLeft(AValue: integer);
Begin
  Inherited SetLeft(AValue);
  fColorTable.Left := AValue + 22;
  fPicColorButton.Left := AValue + 187;
  fColorInfo.Left := AValue + 187;
  fBlack.Left := AValue + 28;
  fDarken.Left := AValue + 28 + 20;
  fRed.Left := AValue + 28 + 2 * 20;
  fGreen.Left := AValue + 28 + 3 * 20;
  fBlue.Left := AValue + 28 + 4 * 20;
  fBrighten.Left := AValue + 28 + 5 * 20;
  fWhite.Left := AValue + 28 + 6 * 20;
  fDarkenMinus.Left := AValue + 28 + 20;
  fRedMinus.Left := AValue + 28 + 2 * 20;
  fGreenMinus.Left := AValue + 28 + 3 * 20;
  fBlueMinus.Left := AValue + 28 + 4 * 20;
  fWhiteMinus.Left := AValue + 28 + 5 * 20;
  fDarkenPlus.Left := AValue + 28 + 20;
  fRedPlus.Left := AValue + 28 + 2 * 20;
  fGreenPlus.Left := AValue + 28 + 3 * 20;
  fBluePlus.Left := AValue + 28 + 4 * 20;
  fWhitePlus.Left := AValue + 28 + 5 * 20;
  fOpenButton.Left := AValue + 15;
  fSaveAsButton.Left := AValue + 37;
  fResetButton.Left := AValue + 240;
  fSetValuesButton.Left := AValue + 15;
End;

Procedure TOpenGL_ColorPicDialog.SetTop(AValue: integer);
Begin
  Inherited SetTop(AValue);
  fColorTable.Top := AValue + 77;
  fPicColorButton.Top := AValue + 33 + 4;
  fColorInfo.top := AValue + 14 - 4;
  fBlack.top := AValue + 33;
  fDarken.top := AValue + 33;
  fRed.top := AValue + 33;
  fGreen.top := AValue + 33;
  fBlue.top := AValue + 33;
  fBrighten.top := AValue + 33;
  fWhite.top := AValue + 33;
  fDarkenMinus.top := AValue + 33 + 20;
  fRedMinus.top := AValue + 33 + 20;
  fGreenMinus.top := AValue + 33 + 20;
  fBlueMinus.top := AValue + 33 + 20;
  fWhiteMinus.top := AValue + 33 + 20;
  fDarkenPlus.top := AValue + 33 - 20;
  fRedPlus.top := AValue + 33 - 20;
  fGreenPlus.top := AValue + 33 - 20;
  fBluePlus.top := AValue + 33 - 20;
  fWhitePlus.top := AValue + 33 - 20;
  fOpenButton.top := AValue + 250;
  fSaveAsButton.top := AValue + 250;
  fResetButton.top := AValue + 250;
  fSetValuesButton.Top := AValue + 33 + 4 + 26;
End;

Procedure TOpenGL_ColorPicDialog.OnPicColorClick(Sender: TObject);
Begin
  If assigned(fShower) Then Begin
    fShower.Color := RGBA(
      min(255, max(0, round(fPicColorButton.FontColor.x * 255))),
      min(255, max(0, round(fPicColorButton.FontColor.y * 255))),
      min(255, max(0, round(fPicColorButton.FontColor.z * 255))),
      AlphaOpaque);
    OnSetColor(fShower);
  End;
  fShower := Nil;
  Visible := false;
End;

Procedure TOpenGL_ColorPicDialog.OnColorDBLClick(Sender: TObject);
Begin
  fPicColorButton.Click;
End;

Procedure TOpenGL_ColorPicDialog.OnResetColorClick(Sender: TObject);
Begin
  If assigned(fShower) Then Begin
    ApplyColor(fShower.DefaultColor);
  End;
End;

Procedure TOpenGL_ColorPicDialog.OnSetColorValuesClick(Sender: TObject);
Var
  Dialog: TRGBDialog;
  s, t: String;
  c: TRGBA;
  dummy: integer;
Begin
  s := fSetValuesButton.Hint;
  fSetValuesButton.Hint := '';
  Dialog := TRGBDialog.CreateNew(Nil);
  If fColorAsHex Then Begin
    dialog.Edit1.Text := format('%0.2X', [fShower.Color.r]);
    dialog.Edit2.Text := format('%0.2X', [fShower.Color.g]);
    dialog.Edit3.Text := format('%0.2X', [fShower.Color.b]);
  End
  Else Begin
    dialog.Edit1.Text := format('%d', [fShower.Color.r]);
    dialog.Edit2.Text := format('%d', [fShower.Color.g]);
    dialog.Edit3.Text := format('%d', [fShower.Color.b]);
  End;
  If Dialog.ShowModal = mrOK Then Begin
    c.a := fShower.Color.a;
    If fColorAsHex Then Begin
      t := Dialog.Edit1.Text;
      val('$' + t, c.r, Dummy);
      If dummy <> 0 Then c.r := max(0, min(255, fShower.Color.r));
      t := Dialog.Edit2.Text;
      val('$' + t, c.g, Dummy);
      If dummy <> 0 Then c.g := max(0, min(255, fShower.Color.g));
      t := Dialog.Edit3.Text;
      val('$' + t, c.b, Dummy);
      If dummy <> 0 Then c.b := max(0, min(255, fShower.Color.b));
    End
    Else Begin
      c.r := max(0, min(255, strtointdef(dialog.Edit1.Text, fShower.Color.r)));
      c.g := max(0, min(255, strtointdef(dialog.Edit2.Text, fShower.Color.g)));
      c.b := max(0, min(255, strtointdef(dialog.Edit3.Text, fShower.Color.b)));
    End;
    ApplyColor(c);
  End;
  fSetValuesButton.Hint := s;
  Dialog.Free;
  Dialog := Nil;
End;

Procedure TOpenGL_ColorPicDialog.OpenColorTableMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  c: TColor;
Begin
  c := clBlack;
  If (x >= 0) And (x < fColorTableRaw.Width) And
    (y >= 0) And (y < fColorTableRaw.Height) Then Begin
    c := fColorTableRaw.Canvas.Pixels[x, y];
  End;
  If c <> clBlack Then Begin
    ApplyColor(ColorToRGBA(c, AlphaOpaque));
  End;
End;

Procedure TOpenGL_ColorPicDialog.OnColorClick(Sender: TObject);
Begin
  ApplyColor((sender As TOpenGL_ColorBox));
End;

Constructor TOpenGL_ColorPicDialog.Create(aOwner: TOpenGLControl);

  Function LoadAlphaColorGraphik(Const Filename: String): integer;
  Begin
    // 1. Ganz normal Laden
    result := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + Filename, Fuchsia, smClamp);
    If result = 0 Then Begin
      // 2. Der User hat das Repo geklont aber die Dateien nicht korrekt um kopiert
      If FileExists('..' + PathDelim + 'GFX' + PathDelim + Filename) Then Begin
        // 3. Dann machen wir das geschwind für den User ..
        If ForceDirectories('GFX') Then Begin
          If copyfile('..' + PathDelim + 'GFX' + PathDelim + Filename, 'GFX' + PathDelim + Filename) Then Begin
            result := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + Filename, Fuchsia, smClamp);
          End;
        End;
      End;
    End;
    If result = 0 Then Begin
      CriticalError := Filename;
    End;
  End;

  Function LoadGraphik(Const Filename: String): integer;
  Begin
    // 1. Ganz normal Laden
    result := OpenGL_GraphikEngine.LoadGraphik('GFX' + PathDelim + Filename, smClamp);
    If result = 0 Then Begin
      // 2. Der User hat das Repo geklont aber die Dateien nicht korrekt um kopiert
      If FileExists('..' + PathDelim + 'GFX' + PathDelim + Filename) Then Begin
        // 3. Dann machen wir das geschwind für den User ..
        If ForceDirectories('GFX') Then Begin
          If copyfile('..' + PathDelim + 'GFX' + PathDelim + Filename, 'GFX' + PathDelim + Filename) Then Begin
            result := OpenGL_GraphikEngine.LoadGraphik('GFX' + PathDelim + Filename, smClamp);
          End;
        End;
      End;
    End;
    If result = 0 Then Begin
      CriticalError := Filename;
    End;
  End;

Var
  img: Integer;
Begin
  IgnoreDepthtest := false;
  CriticalError := '';
  // Alle Elemente vor dem eigentlichen erstellt werden
  // Das hat 2 Gründe
  // 1. Nur so können sie die OnMouse* Events Capturen
  // 2. Sonst braucht man Nil Prüfungen im SetTop, SetLeft
  fResetButton := TOpenGL_Textbox.Create(aOwner, ''); // Muss vor fColorTable erzeugt werden !
  fOpenButton := TOpenGL_Textbox.Create(aOwner, ''); // Muss vor fColorTable erzeugt werden !
  fSaveAsButton := TOpenGL_Textbox.Create(aOwner, ''); // Muss vor fColorTable erzeugt werden !
  fSetValuesButton := TOpenGL_Textbox.Create(aOwner, ''); // Muss vor fColorTable erzeugt werden !
  fColorTable := TOpenGl_Image.Create(aOwner);
  fPicColorButton := TOpenGL_Textbox.Create(aOwner, '');
  fColorInfo := TOpenGl_Label.Create(aOwner, '');
  fBlack := TOpenGL_ColorBox.Create(aOwner);
  fDarken := TOpenGL_ColorBox.Create(aOwner);
  fRed := TOpenGL_ColorBox.Create(aOwner);
  fGreen := TOpenGL_ColorBox.Create(aOwner);
  fBlue := TOpenGL_ColorBox.Create(aOwner);
  fBrighten := TOpenGL_ColorBox.Create(aOwner);
  fWhite := TOpenGL_ColorBox.Create(aOwner);
  fDarkenMinus := TMinus.Create(aOwner);
  fRedMinus := TMinus.Create(aOwner);
  fGreenMinus := TMinus.Create(aOwner);
  fBlueMinus := TMinus.Create(aOwner);
  fWhiteMinus := TMinus.Create(aOwner);
  fDarkenPlus := TPlus.Create(aOwner);
  fRedPlus := TPlus.Create(aOwner);
  fGreenPlus := TPlus.Create(aOwner);
  fBluePlus := TPlus.Create(aOwner);
  fWhitePlus := TPlus.Create(aOwner);
  Inherited Create(aOwner);
  Transparent := true;
  img := LoadAlphaColorGraphik('Color_Pic_Dialog.bmp');
  If img = 0 Then exit;
  SetImage(img);
  SelectorPos := 0;

  img := LoadAlphaColorGraphik('Arror_down.bmp');
  If img = 0 Then exit;
  fSelectorTex := OpenGL_GraphikEngine.GetInfo(img);

  img := LoadGraphik('ColorPalette.bmp');
  If img = 0 Then exit;

  fColorTableRaw := TBitmap.Create;
  fColorTableRaw.LoadFromFile('GFX' + PathDelim + 'ColorPalette.bmp'); // Die Probe findet 4 Zeilen drüber eh schon statt ;)
  fColorTable.SetImage(img);
  fColorTable.OnMouseDown := @OpenColorTableMouseDown;
  fColorTable.OnDblClick := @OnColorDBLClick;

  fPicColorButton.FontColor := v3(192 / 255, 192 / 255, 192 / 255);
  fPicColorButton.Caption := 'Pick color';
  fPicColorButton.Width := 86;
  fPicColorButton.Height := 18;
  fPicColorButton.Layout := tlCenter;
  fPicColorButton.Alignment := taCenter;
  fPicColorButton.OnClick := @OnPicColorClick;
  fPicColorButton.BorderColor := rgba(192, 192, 192, AlphaOpaque);
  fPicColorButton.BackColor := rgba(128, 128, 128, AlphaOpaque);

  fColorInfo.FontColor := v3(192 / 255, 192 / 255, 192 / 255);
  fShower := Nil;
  OnSetColor := Nil;

  fBlack.RaisedColor := RGBA(192, 192, 192, AlphaOpaque);
  fBlack.LoweredColor := RGBA(192, 192, 192, AlphaOpaque);
  fBlack.Height := 18;
  fBlack.Width := 18;
  fBlack.Color := RGBA(0, 0, 0, AlphaOpaque);
  fBlack.OnClick := @OnColorClick;
  fBlack.OnDblClick := @OnColorDBLClick;

  fDarken.RaisedColor := RGBA(192, 192, 192, AlphaOpaque);
  fDarken.LoweredColor := RGBA(192, 192, 192, AlphaOpaque);
  fDarken.Height := 18;
  fDarken.Width := 18;
  fDarken.OnClick := @OnColorClick;
  fDarken.OnDblClick := @OnColorDBLClick;

  fRed.RaisedColor := RGBA(192, 192, 192, AlphaOpaque);
  fRed.LoweredColor := RGBA(192, 192, 192, AlphaOpaque);
  fRed.Height := 18;
  fRed.Width := 18;
  fRed.OnClick := @OnColorClick;
  fRed.OnDblClick := @OnColorDBLClick;

  fGreen.RaisedColor := RGBA(192, 192, 192, AlphaOpaque);
  fGreen.LoweredColor := RGBA(192, 192, 192, AlphaOpaque);
  fGreen.Height := 18;
  fGreen.Width := 18;
  fGreen.OnClick := @OnColorClick;
  fGreen.OnDblClick := @OnColorDBLClick;

  fBlue.RaisedColor := RGBA(192, 192, 192, AlphaOpaque);
  fBlue.LoweredColor := RGBA(192, 192, 192, AlphaOpaque);
  fBlue.Height := 18;
  fBlue.Width := 18;
  fBlue.OnClick := @OnColorClick;
  fBlue.OnDblClick := @OnColorDBLClick;

  fBrighten.RaisedColor := RGBA(192, 192, 192, AlphaOpaque);
  fBrighten.LoweredColor := RGBA(192, 192, 192, AlphaOpaque);
  fBrighten.Height := 18;
  fBrighten.Width := 18;
  fBrighten.OnClick := @OnColorClick;
  fBrighten.OnDblClick := @OnColorDBLClick;

  fWhite.RaisedColor := RGBA(192, 192, 192, AlphaOpaque);
  fWhite.LoweredColor := RGBA(192, 192, 192, AlphaOpaque);
  fWhite.Height := 18;
  fWhite.Width := 18;
  fWhite.Color := RGBA(255, 255, 255, AlphaOpaque);
  fWhite.OnClick := @OnColorClick;
  fWhite.OnDblClick := @OnColorDBLClick;

  fDarkenMinus.Color := RGBA(0, 0, 0, AlphaOpaque);
  fDarkenMinus.Height := 18;
  fDarkenMinus.Width := 17;
  fDarkenMinus.Delta := delta(15, 15, 15);
  fDarkenMinus.Target := fDarken;
  fDarkenMinus.OnUpdate := @ApplyColor;

  fRedMinus.Color := RGBA(255, 0, 0, AlphaOpaque);
  fRedMinus.Height := 18;
  fRedMinus.Width := 17;
  fRedMinus.Delta := delta(-15, 0, 0);
  fRedMinus.Target := fRed;
  fRedMinus.OnUpdate := @ApplyColor;

  fGreenMinus.Color := RGBA(0, 255, 0, AlphaOpaque);
  fGreenMinus.Height := 18;
  fGreenMinus.Width := 17;
  fGreenMinus.Delta := delta(0, -15, 0);
  fGreenMinus.Target := fGreen;
  fGreenMinus.OnUpdate := @ApplyColor;

  fBlueMinus.Color := RGBA(0, 0, 255, AlphaOpaque);
  fBlueMinus.Height := 18;
  fBlueMinus.Width := 17;
  fBlueMinus.Delta := delta(0, 0, -15);
  fBlueMinus.Target := fBlue;
  fBlueMinus.OnUpdate := @ApplyColor;

  fWhiteMinus.Color := RGBA(255, 255, 255, AlphaOpaque);
  fWhiteMinus.Height := 18;
  fWhiteMinus.Width := 17;
  fWhiteMinus.Delta := delta(-15, -15, -15);
  fWhiteMinus.Target := fBrighten;
  fWhiteMinus.OnUpdate := @ApplyColor;

  fDarkenPlus.Color := RGBA(0, 0, 0, AlphaOpaque);
  fDarkenPlus.Height := 16;
  fDarkenPlus.Width := 16;
  fDarkenPlus.Delta := delta(-15, -15, -15);
  fDarkenPlus.Target := fDarken;
  fDarkenPlus.OnUpdate := @ApplyColor;

  fRedPlus.Color := RGBA(255, 0, 0, AlphaOpaque);
  fRedPlus.Height := 16;
  fRedPlus.Width := 16;
  fRedPlus.Delta := delta(15, 0, 0);
  fRedPlus.Target := fRed;
  fRedPlus.OnUpdate := @ApplyColor;

  fGreenPlus.Color := RGBA(0, 255, 0, AlphaOpaque);
  fGreenPlus.Height := 16;
  fGreenPlus.Width := 16;
  fGreenPlus.Delta := delta(0, 15, 0);
  fGreenPlus.Target := fGreen;
  fGreenPlus.OnUpdate := @ApplyColor;

  fBluePlus.Color := RGBA(0, 0, 255, AlphaOpaque);
  fBluePlus.Height := 16;
  fBluePlus.Width := 16;
  fBluePlus.Delta := delta(0, 0, 15);
  fBluePlus.Target := fBlue;
  fBluePlus.OnUpdate := @ApplyColor;

  fWhitePlus.Color := RGBA(255, 255, 255, AlphaOpaque);
  fWhitePlus.Height := 16;
  fWhitePlus.Width := 16;
  fWhitePlus.Delta := delta(15, 15, 15);
  fWhitePlus.Target := fBrighten;
  fWhitePlus.OnUpdate := @ApplyColor;

  fOpenButton.FontColor := v3(192 / 255, 192 / 255, 192 / 255);
  fOpenButton.Caption := 'L';
  fOpenButton.Width := 18;
  fOpenButton.Height := 18;
  fOpenButton.Layout := tlCenter;
  fOpenButton.Alignment := taCenter;
  fOpenButton.BorderColor := rgba(192, 192, 192, AlphaOpaque);
  fOpenButton.BackColor := rgba(128, 128, 128, AlphaOpaque);
  fOpenButton.Hint := 'Load color palette';

  fSaveAsButton.FontColor := v3(192 / 255, 192 / 255, 192 / 255);
  fSaveAsButton.Caption := 'S';
  fSaveAsButton.Width := 18;
  fSaveAsButton.Height := 18;
  fSaveAsButton.Layout := tlCenter;
  fSaveAsButton.Alignment := taCenter;
  fSaveAsButton.BorderColor := rgba(192, 192, 192, AlphaOpaque);
  fSaveAsButton.BackColor := rgba(128, 128, 128, AlphaOpaque);
  fSaveAsButton.Hint := 'Store color palette';

  fResetButton.FontColor := v3(192 / 255, 192 / 255, 192 / 255);
  fResetButton.Caption := 'R';
  fResetButton.Width := 18;
  fResetButton.Height := 18;
  fResetButton.Layout := tlCenter;
  fResetButton.Alignment := taCenter;
  fResetButton.BorderColor := rgba(192, 192, 192, AlphaOpaque);
  fResetButton.BackColor := rgba(128, 128, 128, AlphaOpaque);
  fResetButton.OnClick := @OnResetColorClick;
  fResetButton.Hint := 'Reset to default';

  fSetValuesButton.FontColor := v3(192 / 255, 192 / 255, 192 / 255);
  fSetValuesButton.Caption := 'E';
  fSetValuesButton.Width := 18;
  fSetValuesButton.Height := 18;
  fSetValuesButton.Layout := tlCenter;
  fSetValuesButton.Alignment := taCenter;
  fSetValuesButton.BorderColor := rgba(192, 192, 192, AlphaOpaque);
  fSetValuesButton.BackColor := rgba(128, 128, 128, AlphaOpaque);
  fSetValuesButton.OnClick := @OnSetColorValuesClick;
  fSetValuesButton.Hint := 'Set RGB values by hand';
End;

Destructor TOpenGL_ColorPicDialog.Destroy;
Begin
  fColorTable.free;
  fColorTable := Nil;
  fColorTableRaw.free;
  fColorTableRaw := Nil;
  fPicColorButton.Free;
  fPicColorButton := Nil;
  fColorInfo.free;
  fColorInfo := Nil;
  fBlack.free;
  fBlack := Nil;
  fDarken.free;
  fDarken := Nil;
  fWhite.free;
  fWhite := Nil;
  fRed.free;
  fRed := Nil;
  fGreen.free;
  fGreen := Nil;
  fBlue.free;
  fBlue := Nil;
  fBrighten.free;
  fBrighten := Nil;
  fDarkenMinus.free;
  fDarkenMinus := Nil;
  fRedMinus.free;
  fRedMinus := Nil;
  fGreenMinus.free;
  fGreenMinus := Nil;
  fBlueMinus.free;
  fBlueMinus := Nil;
  fWhiteMinus.free;
  fWhiteMinus := Nil;
  fDarkenPlus.free;
  fDarkenPlus := Nil;
  fRedPlus.free;
  fRedPlus := Nil;
  fGreenPlus.free;
  fGreenPlus := Nil;
  fBluePlus.free;
  fBluePlus := Nil;
  fWhitePlus.free;
  fWhitePlus := Nil;
  fOpenButton.free;
  fOpenButton := Nil;
  fSaveAsButton.free;
  fSaveAsButton := Nil;
  fResetButton.free;
  fResetButton := Nil;
  fSetValuesButton.free;
  fSetValuesButton := Nil;
  Inherited Destroy;
End;

Procedure TOpenGL_ColorPicDialog.LoadColor(aColor: TOpenGL_ColorBox;
  aColorAsHex: Boolean);
Begin
  fColorAsHex := aColorAsHex;
  fShower := aColor;
  ApplyColor(fShower.Color);
End;

End.

