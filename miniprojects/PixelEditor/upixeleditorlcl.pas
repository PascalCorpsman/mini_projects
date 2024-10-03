Unit upixeleditorlcl;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, controls, SysUtils, OpenGLContext, uopengl_widgetset, ugraphics,
  uopengl_graphikengine, ExtCtrls, Graphics;

Type

  TBevelStyle = ExtCtrls.TBevelStyle;
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
    Constructor Create(Owner: TOpenGLControl); override;
  End;

  { TOpenGL_ToggleButton }

  TOpenGL_ToggleButton = Class(TOpenGL_Bevel)
  protected
    FDownimage: TGraphikItem;
    Procedure Click; override;
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

    Constructor Create(Owner: TOpenGLControl); override;
  End;

  TColorBoxEvent = Procedure(Const C: TOpenGL_ColorBox) Of Object;

  { TOpenGL_TransparentColorBox }

  TOpenGL_TransparentColorBox = Class(TOpenGL_ColorBox)
  private
    fBevel: TOpenGL_Bevel;
    Function getOnClick: TNotifyEvent;
    Function getTransparent: Boolean;
    Procedure setOnClick(AValue: TNotifyEvent);
    Procedure setTransparent(AValue: Boolean);
  protected
    Procedure OnRender(); override;

    Function getLoweredColor: TRGBA; override;
    Function getRaisedColor: TRGBA; override;
    Procedure setLoweredColor(AValue: TRGBA); override;
    Procedure setRaisedColor(AValue: TRGBA); override;

    Function getColor: TRGBA; override;
    Function getDefaultColor: TRGBA; override;

    Procedure SetHeight(AValue: integer); override;
    Procedure SetLeft(AValue: integer); override;
    Procedure SetTop(AValue: integer); override;
    Procedure SetWidth(AValue: integer); override;

  public
    Property OnClick: TNotifyEvent read getOnClick write setOnClick;
    Property Transparent: Boolean read getTransparent write setTransparent;
    Procedure SetImage(aImage: integer);
    Property Color: TRGBA read getColor;
    Constructor Create(Owner: TOpenGLControl); override;
    Destructor Destroy; override;
  End;

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
    Constructor Create(Owner: TOpenGLControl; FontFile: String); override;
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
    Constructor Create(Owner: TOpenGLControl); override;
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

    Procedure OnRender(); override;
    Procedure SetVisible(AValue: Boolean); override;
    Procedure SetLeft(AValue: integer); override;
    Procedure SetTop(AValue: integer); override;

    Procedure OnPicColorClick(Sender: TObject);
    Procedure OnColorDBLClick(Sender: TObject);
    Procedure OnResetColorClick(Sender: TObject);
    Procedure OpenColorTableMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    Procedure OnColorClick(Sender: TObject);

  public
    SelectorPos: integer;
    OnSetColor: TColorBoxEvent;

    Property Shower: TOpenGL_ColorBox read getShower;

    Constructor Create(Owner: TOpenGLControl); override;
    Destructor Destroy; override;
    Procedure LoadColor(aColor: TOpenGL_ColorBox);
  End;

Implementation

Uses
  math
  , dglOpenGL
  , uvectormath
  ;

Function Delta(r, g, b: integer): TDelta;
Begin
  result.r := r;
  result.g := g;
  result.b := b;
End;

(*
 * Addiert die R,G,B Werte als Delta auf Color und berücksichtigt
 * Dabei überläufe ;)
 *)

Function ClampAdd(Color: TRGBA; R, G, B: Integer): TRGBA;
  Procedure Fix(Var aa, bb, cc: Integer);
  Var
    d: integer;
  Begin
    If aa > 255 Then Begin
      d := aa - 255;
      aa := 255;
      bb := min(255, max(0, bb - d));
      cc := min(255, max(0, cc - d));
    End;
    If aa < 0 Then Begin
      d := -aa;
      aa := 0;
      bb := min(255, max(0, bb + d));
      cc := min(255, max(0, cc + d));
    End;
  End;

Var
  tr, tg, tb: Integer;
Begin
  If (r = g) And (g = b) Then Begin
    // Eine Allgemeine Aufhellung / Verdunklung
    result.r := max(0, min(255, Color.r + R));
    result.g := max(0, min(255, Color.g + g));
    result.b := max(0, min(255, Color.b + b));
  End
  Else Begin
    // Eine Verstärkung eines einzelnen Farbkanals
    tr := Color.r + r;
    tg := Color.g + g;
    tb := Color.b + b;
    fix(tr, tb, tg);
    fix(tg, tr, tb);
    fix(tb, tg, tr);
    result.r := tr;
    result.g := tg;
    result.b := tb;
  End;
  result.a := Result.a;
End;

{ TOpenGL_Bevel }

Procedure TOpenGL_Bevel.OnRender;
Begin
  If Not Visible Then exit;
  Inherited OnRender();
  glBindTexture(GL_TEXTURE_2D, 0);
  glPushMatrix;
  glTranslatef(Left, Top, 0);
  If (fStyle = bsRaised) Or (fmDown) Then Begin
    glColor3ub(RaisedColor.r, RaisedColor.g, RaisedColor.b);
  End
  Else Begin
    glColor3ub(LoweredColor.r, LoweredColor.g, LoweredColor.b);
  End;
  glLineWidth(max(FOwner.Width / 640, FOwner.Height / 480) * 2);
  // glLineWidth(max(FOwner.Width / 640, FOwner.Height / 480) * 1); // Debug, zum Ausmessen der Positionen !
  glBegin(GL_LINE_LOOP);
  glVertex2f(0, 1);
  glVertex2f(Width - 1, 1);
  glVertex2f(Width - 1, Height);
  glVertex2f(0, Height);
  glend;
  glLineWidth(1);
  glPopMatrix;
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

Constructor TOpenGL_Bevel.Create(Owner: TOpenGLControl);
Begin
  Inherited Create(Owner);
  fStyle := bsLowered;
  fmDown := false;
  RaisedColor := RGBA($FF, $FF, 0, 0);
  LoweredColor := RGBA(0, 0, 0, 0);
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
Begin
  If Not Visible Then exit;
  glBindTexture(GL_TEXTURE_2D, 0);

  glPushMatrix;
  glTranslatef(Left, Top, 0);

  glLineWidth(max(FOwner.Width / 640, FOwner.Height / 480) * 2);
  // glLineWidth(max(FOwner.Width / 640, FOwner.Height / 480) * 1); // Debug, zum Ausmessen der Positionen !

  glColor3ub(fColor.r, fColor.g, fColor.b);
  glBegin(GL_QUADS);
  glVertex2f(0, 0);
  glVertex2f(Width - 1, 0);
  glVertex2f(Width - 1, Height - 1);
  glVertex2f(0, Height - 1);
  glend;

  If (fStyle = bsRaised) Or (fmDown) Then Begin
    glColor3ub(RaisedColor.r, RaisedColor.g, RaisedColor.b);
  End
  Else Begin
    glColor3ub(LoweredColor.r, LoweredColor.g, LoweredColor.b);
  End;

  glBegin(GL_LINE_LOOP);
  glVertex2f(0, 1);
  glVertex2f(Width - 1, 1);
  glVertex2f(Width - 1, Height);
  glVertex2f(0, Height);
  glend;

  glLineWidth(1);
  glPopMatrix;
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

Constructor TOpenGL_ColorBox.Create(Owner: TOpenGLControl);
Begin
  Inherited Create(Owner);
  fColor := RGBA(0, 0, 0, 0);
  RaisedColor := RGBA($FF, $FF, 0, 0);
  LoweredColor := RGBA(0, 0, 0, 0);
End;

{ TOpenGL_TransparentColorBox }

Function TOpenGL_TransparentColorBox.getTransparent: Boolean;
Begin
  result := fBevel.Transparent;
End;

Function TOpenGL_TransparentColorBox.getOnClick: TNotifyEvent;
Begin
  result := fBevel.OnClick;
End;

Function TOpenGL_TransparentColorBox.getColor: TRGBA;
Begin
  result := RGBA(0, 0, 0, 255);
End;

Function TOpenGL_TransparentColorBox.getDefaultColor: TRGBA;
Begin
  Result := RGBA(0, 0, 0, 255);
End;

Procedure TOpenGL_TransparentColorBox.setOnClick(AValue: TNotifyEvent);
Begin
  fBevel.OnClick := AValue;
End;

Procedure TOpenGL_TransparentColorBox.setTransparent(AValue: Boolean);
Begin
  fBevel.Transparent := AValue;
End;

Procedure TOpenGL_TransparentColorBox.OnRender;
Begin
  fBevel.Render();
End;

Function TOpenGL_TransparentColorBox.getLoweredColor: TRGBA;
Begin
  Result := fBevel.LoweredColor;
End;

Function TOpenGL_TransparentColorBox.getRaisedColor: TRGBA;
Begin
  Result := fBevel.RaisedColor;
End;

Procedure TOpenGL_TransparentColorBox.setLoweredColor(AValue: TRGBA);
Begin
  fBevel.LoweredColor := AValue;
End;

Procedure TOpenGL_TransparentColorBox.setRaisedColor(AValue: TRGBA);
Begin
  fBevel.RaisedColor := AValue;
End;

Procedure TOpenGL_TransparentColorBox.SetHeight(AValue: integer);
Begin
  Inherited SetHeight(AValue);
  fBevel.Height := AValue;
End;

Procedure TOpenGL_TransparentColorBox.SetLeft(AValue: integer);
Begin
  Inherited SetLeft(AValue);
  fBevel.Left := AValue;
End;

Procedure TOpenGL_TransparentColorBox.SetTop(AValue: integer);
Begin
  Inherited SetTop(AValue);
  fBevel.Top := AValue;
End;

Procedure TOpenGL_TransparentColorBox.SetWidth(AValue: integer);
Begin
  Inherited SetWidth(AValue);
  fBevel.Width := AValue;
End;

Procedure TOpenGL_TransparentColorBox.SetImage(aImage: integer);
Begin
  fBevel.SetImage(aImage);
End;

Constructor TOpenGL_TransparentColorBox.Create(Owner: TOpenGLControl);
Begin
  fBevel := TOpenGL_Bevel.Create(Owner);
  Inherited Create(Owner);
End;

Destructor TOpenGL_TransparentColorBox.Destroy;
Begin
  fBevel.Free;
  Inherited Destroy;
End;

{ TOpenGL_Textbox }

Procedure TOpenGL_Textbox.Setcaption(value: String);
Begin
  // inherited Setcaption(value); -- Das würde die Größe anpassen, was wir hier explizit nicht wollen !
  fcaption := value;
End;

Procedure TOpenGL_Textbox.OnRender;
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
  glLineWidth(max(FOwner.Width / 640, FOwner.Height / 480) * 2);
  // glLineWidth(max(FOwner.Width / 640, FOwner.Height / 480) * 1); // Debug, zum Ausmessen der Positionen !
  glBegin(GL_LINE_LOOP);
  glVertex2f(0, 1);
  glVertex2f(Width - 1, 1);
  glVertex2f(Width - 1, Height);
  glVertex2f(0, Height);
  glend;
  glLineWidth(1);
  glPopMatrix;
End;

Constructor TOpenGL_Textbox.Create(Owner: TOpenGLControl; FontFile: String);
Begin
  Inherited Create(Owner, FontFile);
  Layout := tlTop;
  Alignment := taLeftJustify;
  BorderColor := RGBA(0, 0, 0, 0);
  BackColor := RGBA(0, 0, 0, 255);
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

Constructor TPlus.Create(Owner: TOpenGLControl);
Begin
  Inherited Create(Owner);
  Color := RGBA(0, 0, 0, 0);
  Target := Nil;
  Delta.r := 0;
  Delta.g := 0;
  Delta.b := 0;
  OnUpdate := Nil;
  OnClick := @OnClickEvent;
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
Begin
  fDarken.Color := ClampAdd(Color, -30, -30, -30);
  fRed.Color := ClampAdd(Color, 30, 0, 0);
  fGreen.Color := ClampAdd(Color, 0, 30, 0);
  fBlue.Color := ClampAdd(Color, 0, 0, 30);
  fBrighten.Color := ClampAdd(Color, 30, 30, 30);
  fPicColorButton.FontColor := v3(Color.r / 255, Color.g / 255, Color.b / 255);
  fColorInfo.caption := format('%d/%d/%d', [Color.r, Color.g, Color.b]);
End;

Procedure TOpenGL_ColorPicDialog.ApplyColor(Const CB: TOpenGL_ColorBox);
Begin
  If cb <> fDarken Then fDarken.Color := ClampAdd(cb.Color, -30, -30, -30);
  If cb <> fRed Then fRed.Color := ClampAdd(cb.Color, 30, 0, 0);
  If cb <> fGreen Then fGreen.Color := ClampAdd(cb.Color, 0, 30, 0);
  If cb <> fBlue Then fBlue.Color := ClampAdd(cb.Color, 0, 0, 30);
  If cb <> fBrighten Then fBrighten.Color := ClampAdd(cb.Color, 30, 30, 30);
  fPicColorButton.FontColor := v3(cb.Color.r / 255, cb.Color.g / 255, cb.Color.b / 255);
  fColorInfo.caption := format('%d/%d/%d', [cb.Color.r, cb.Color.g, cb.Color.b]);
End;

Function TOpenGL_ColorPicDialog.getShower: TOpenGL_ColorBox;
Begin
  result := fShower;
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
  fResetButton.Render();

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
  fResetButton.Visible := AValue;
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
  fResetButton.Left := AValue + 240;
End;

Procedure TOpenGL_ColorPicDialog.SetTop(AValue: integer);
Begin
  Inherited SetTop(AValue);
  fColorTable.Top := AValue + 77;
  fPicColorButton.Top := AValue + 33;
  fColorInfo.top := AValue + 14;
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
  fResetButton.top := AValue + 250;
End;

Procedure TOpenGL_ColorPicDialog.OnPicColorClick(Sender: TObject);
Begin
  If assigned(fShower) Then Begin
    fShower.Color := RGBA(
      min(255, max(0, round(fPicColorButton.FontColor.x * 255))),
      min(255, max(0, round(fPicColorButton.FontColor.y * 255))),
      min(255, max(0, round(fPicColorButton.FontColor.z * 255))), 0);
    OnSetColor(fShower);
  End;
  fShower := Nil;
  visible := false;
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
    ApplyColor(ColorToRGBA(c, 0));
  End;
End;

Procedure TOpenGL_ColorPicDialog.OnColorClick(Sender: TObject);
Begin
  ApplyColor((sender As TOpenGL_ColorBox));
End;

Constructor TOpenGL_ColorPicDialog.Create(Owner: TOpenGLControl);
Var
  img: Integer;
Begin
  // Alle Elemente vor dem eigentlichen erstellt werden
  // Das hat 2 Gründe
  // 1. Nur so können sie die OnMouse* Events Capturen
  // 2. Sonst braucht man Nil Prüfungen im SetTop, SetLeft
  fResetButton := TOpenGL_Textbox.Create(Owner, '');
  fColorTable := TOpenGl_Image.Create(Owner);
  fPicColorButton := TOpenGL_Textbox.Create(Owner, '');
  fColorInfo := TOpenGl_Label.Create(Owner, '');
  fBlack := TOpenGL_ColorBox.Create(Owner);
  fDarken := TOpenGL_ColorBox.Create(Owner);
  fRed := TOpenGL_ColorBox.Create(Owner);
  fGreen := TOpenGL_ColorBox.Create(Owner);
  fBlue := TOpenGL_ColorBox.Create(Owner);
  fBrighten := TOpenGL_ColorBox.Create(Owner);
  fWhite := TOpenGL_ColorBox.Create(Owner);
  fDarkenMinus := TMinus.Create(Owner);
  fRedMinus := TMinus.Create(Owner);
  fGreenMinus := TMinus.Create(Owner);
  fBlueMinus := TMinus.Create(Owner);
  fWhiteMinus := TMinus.Create(Owner);
  fDarkenPlus := TPlus.Create(Owner);
  fRedPlus := TPlus.Create(Owner);
  fGreenPlus := TPlus.Create(Owner);
  fBluePlus := TPlus.Create(Owner);
  fWhitePlus := TPlus.Create(Owner);
  Inherited Create(Owner);
  Transparent := true;
  img := OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Color_Pic_Dialog.bmp', Fuchsia, smClamp);
  SetImage(img);
  SelectorPos := 0;
  fSelectorTex := OpenGL_GraphikEngine.GetInfo(OpenGL_GraphikEngine.LoadAlphaColorGraphik('GFX' + PathDelim + 'Arror_down.bmp', Fuchsia, smClamp));

  img := OpenGL_GraphikEngine.LoadGraphik('GFX' + PathDelim + 'ColorPalette.bmp', smClamp);
  fColorTableRaw := TBitmap.Create;
  fColorTableRaw.LoadFromFile('GFX' + PathDelim + 'ColorPalette.bmp');
  fColorTable.SetImage(img);
  fColorTable.OnMouseDown := @OpenColorTableMouseDown;
  fColorTable.OnDblClick := @OnColorDBLClick;

  fPicColorButton.FontColor := v3(192 / 255, 192 / 255, 192 / 255);
  fPicColorButton.Caption := 'Pic color';
  fPicColorButton.Width := 86;
  fPicColorButton.Height := 18;
  fPicColorButton.Layout := tlCenter;
  fPicColorButton.Alignment := taCenter;
  fPicColorButton.OnClick := @OnPicColorClick;
  fPicColorButton.BorderColor := rgba(192, 192, 192, 0);
  fPicColorButton.BackColor := rgba(128, 128, 128, 0);

  fColorInfo.FontColor := v3(192 / 255, 192 / 255, 192 / 255);
  fShower := Nil;
  OnSetColor := Nil;

  fBlack.RaisedColor := RGBA(192, 192, 192, 0);
  fBlack.LoweredColor := RGBA(192, 192, 192, 0);
  fBlack.Height := 18;
  fBlack.Width := 18;
  fBlack.Color := RGBA(0, 0, 0, 0);
  fBlack.OnClick := @OnColorClick;
  fBlack.OnDblClick := @OnColorDBLClick;

  fDarken.RaisedColor := RGBA(192, 192, 192, 0);
  fDarken.LoweredColor := RGBA(192, 192, 192, 0);
  fDarken.Height := 18;
  fDarken.Width := 18;
  fDarken.OnClick := @OnColorClick;
  fDarken.OnDblClick := @OnColorDBLClick;

  fRed.RaisedColor := RGBA(192, 192, 192, 0);
  fRed.LoweredColor := RGBA(192, 192, 192, 0);
  fRed.Height := 18;
  fRed.Width := 18;
  fRed.OnClick := @OnColorClick;
  fRed.OnDblClick := @OnColorDBLClick;

  fGreen.RaisedColor := RGBA(192, 192, 192, 0);
  fGreen.LoweredColor := RGBA(192, 192, 192, 0);
  fGreen.Height := 18;
  fGreen.Width := 18;
  fGreen.OnClick := @OnColorClick;
  fGreen.OnDblClick := @OnColorDBLClick;

  fBlue.RaisedColor := RGBA(192, 192, 192, 0);
  fBlue.LoweredColor := RGBA(192, 192, 192, 0);
  fBlue.Height := 18;
  fBlue.Width := 18;
  fBlue.OnClick := @OnColorClick;
  fBlue.OnDblClick := @OnColorDBLClick;

  fBrighten.RaisedColor := RGBA(192, 192, 192, 0);
  fBrighten.LoweredColor := RGBA(192, 192, 192, 0);
  fBrighten.Height := 18;
  fBrighten.Width := 18;
  fBrighten.OnClick := @OnColorClick;
  fBrighten.OnDblClick := @OnColorDBLClick;

  fWhite.RaisedColor := RGBA(192, 192, 192, 0);
  fWhite.LoweredColor := RGBA(192, 192, 192, 0);
  fWhite.Height := 18;
  fWhite.Width := 18;
  fWhite.Color := RGBA(255, 255, 255, 0);
  fWhite.OnClick := @OnColorClick;
  fWhite.OnDblClick := @OnColorDBLClick;

  fDarkenMinus.Color := RGBA(0, 0, 0, 0);
  fDarkenMinus.Height := 18;
  fDarkenMinus.Width := 17;
  fDarkenMinus.Delta := delta(15, 15, 15);
  fDarkenMinus.Target := fDarken;
  fDarkenMinus.OnUpdate := @ApplyColor;

  fRedMinus.Color := RGBA(255, 0, 0, 0);
  fRedMinus.Height := 18;
  fRedMinus.Width := 17;
  fRedMinus.Delta := delta(-15, 0, 0);
  fRedMinus.Target := fRed;
  fRedMinus.OnUpdate := @ApplyColor;

  fGreenMinus.Color := RGBA(0, 255, 0, 0);
  fGreenMinus.Height := 18;
  fGreenMinus.Width := 17;
  fGreenMinus.Delta := delta(0, -15, 0);
  fGreenMinus.Target := fGreen;
  fGreenMinus.OnUpdate := @ApplyColor;

  fBlueMinus.Color := RGBA(0, 0, 255, 0);
  fBlueMinus.Height := 18;
  fBlueMinus.Width := 17;
  fBlueMinus.Delta := delta(0, 0, -15);
  fBlueMinus.Target := fBlue;
  fBlueMinus.OnUpdate := @ApplyColor;

  fWhiteMinus.Color := RGBA(255, 255, 255, 0);
  fWhiteMinus.Height := 18;
  fWhiteMinus.Width := 17;
  fWhiteMinus.Delta := delta(-15, -15, -15);
  fWhiteMinus.Target := fBrighten;
  fWhiteMinus.OnUpdate := @ApplyColor;

  fDarkenPlus.Color := RGBA(0, 0, 0, 0);
  fDarkenPlus.Height := 16;
  fDarkenPlus.Width := 16;
  fDarkenPlus.Delta := delta(-15, -15, -15);
  fDarkenPlus.Target := fDarken;
  fDarkenPlus.OnUpdate := @ApplyColor;

  fRedPlus.Color := RGBA(255, 0, 0, 0);
  fRedPlus.Height := 16;
  fRedPlus.Width := 16;
  fRedPlus.Delta := delta(15, 0, 0);
  fRedPlus.Target := fRed;
  fRedPlus.OnUpdate := @ApplyColor;

  fGreenPlus.Color := RGBA(0, 255, 0, 0);
  fGreenPlus.Height := 16;
  fGreenPlus.Width := 16;
  fGreenPlus.Delta := delta(0, 15, 0);
  fGreenPlus.Target := fGreen;
  fGreenPlus.OnUpdate := @ApplyColor;

  fBluePlus.Color := RGBA(0, 0, 255, 0);
  fBluePlus.Height := 16;
  fBluePlus.Width := 16;
  fBluePlus.Delta := delta(0, 0, 15);
  fBluePlus.Target := fBlue;
  fBluePlus.OnUpdate := @ApplyColor;

  fWhitePlus.Color := RGBA(255, 255, 255, 0);
  fWhitePlus.Height := 16;
  fWhitePlus.Width := 16;
  fWhitePlus.Delta := delta(15, 15, 15);
  fWhitePlus.Target := fBrighten;
  fWhitePlus.OnUpdate := @ApplyColor;

  fResetButton.FontColor := v3(192 / 255, 192 / 255, 192 / 255);
  fResetButton.Caption := 'R';
  fResetButton.Width := 18;
  fResetButton.Height := 18;
  fResetButton.Layout := tlCenter;
  fResetButton.Alignment := taCenter;
  fResetButton.BorderColor := rgba(192, 192, 192, 0);
  fResetButton.BackColor := rgba(128, 128, 128, 0);
  fResetButton.OnClick := @OnResetColorClick;
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
  fResetButton.free;
  fResetButton := Nil;
  Inherited Destroy;
End;

Procedure TOpenGL_ColorPicDialog.LoadColor(aColor: TOpenGL_ColorBox);
Begin
  fShower := aColor;
  ApplyColor(fShower.Color);
End;

End.

