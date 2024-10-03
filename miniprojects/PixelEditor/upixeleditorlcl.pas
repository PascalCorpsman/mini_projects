Unit upixeleditorlcl;

{$MODE ObjFPC}{$H+}

Interface

Uses
  Classes, controls, SysUtils, OpenGLContext, uopengl_widgetset, ugraphics,
  uopengl_graphikengine, ExtCtrls, Graphics;

Type

  TBevelStyle = ExtCtrls.TBevelStyle;
  TColorEvent = Procedure(Const C: TRGBA) Of Object;

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
  protected
    fStyle: TBevelStyle;
    fColor: TRGBA;
    fmDown: Boolean;
    Procedure OnRender(); override;

    Procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    Procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

  public
    RaisedColor: TRGBA;
    LoweredColor: TRGBA;

    Property Color: TRGBA read fColor write fColor;
    Property Style: TBevelStyle read fStyle write fStyle;

    Property OnClick;

    Constructor Create(Owner: TOpenGLControl); override;
  End;

  { TOpenGL_Textbox }

  TOpenGL_Textbox = Class(TOpenGl_Label)
  protected
    Procedure Setcaption(value: String); override;
    Procedure OnRender(); override;
  public
    Alignment: TAlignment;
    Layout: TTextLayout;
    Constructor Create(Owner: TOpenGLControl; FontFile: String); override;
  End;

  TCustomOpenGl_Edit = Class(TOpenGl_Edit)
  private
  public
    Property OnClick;
  End;

  { TPlus }

  TPlus = Class(TOpenGl_BaseClass)
  protected
    Procedure OnRender(); override;
  public
    Color: TRGBA;
    Property OnClick;
    Constructor Create(Owner: TOpenGLControl); override;
  End;

  { TMinus }

  TMinus = Class(TPlus)
  protected
    Procedure OnRender(); override;
  End;

  { TOpenGL_ColorPicDialog }

  TOpenGL_ColorPicDialog = Class(TOpenGl_Image)
  protected
    fSelectorTex: TGraphikItem;
    fColorTable: TOpenGl_Image;
    fColorTableRaw: TBitmap;
    fColorInfo: TOpenGl_Label;
    fPicColorButton: TCustomOpenGl_Edit;

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

    Procedure OnRender(); override;
    Procedure SetVisible(AValue: Boolean); override;
    Procedure SetLeft(AValue: integer); override;
    Procedure SetTop(AValue: integer); override;

    Procedure OnPicColorClick(Sender: TObject);
    Procedure OnPicColorDBLClick(Sender: TObject);
    Procedure OpenColorTableMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  public
    SelectorPos: integer;
    Shower: TOpenGL_ColorBox;
    OnSetColor: TColorEvent;
    Constructor Create(Owner: TOpenGLControl); override;
    Destructor Destroy; override;
    Procedure LoadColor(aColor: TRGBA);
  End;

Implementation

Uses
  math
  , dglOpenGL
  , uvectormath
  ;

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

Constructor TOpenGL_ColorBox.Create(Owner: TOpenGLControl);
Begin
  Inherited Create(Owner);
  fColor := RGBA(0, 0, 0, 0);
  RaisedColor := RGBA($FF, $FF, 0, 0);
  LoweredColor := RGBA(0, 0, 0, 0);
End;

{ TOpenGL_Textbox }

Procedure TOpenGL_Textbox.Setcaption(value: String);
Begin
  // inherited Setcaption(value); -- Das würde die Größe anpassen, was wir hier explizit nicht wollen !
  fcaption := value;
End;

Procedure TOpenGL_Textbox.OnRender;
Begin
  glPushMatrix;
  Case Layout Of
    tlTop: Begin
        // Nichts zu tun
      End;
    tlCenter: Begin
        gltranslatef(0, (Height - FFont.TextHeight(fcaption)) / 2, 0);
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
        gltranslatef((Width - FFont.TextWidth(fcaption)) / 2, 0, 0);
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
  glTranslatef(Left, Top, 0);
  glColor3ub($0, $0, $0);
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
End;

{ TPlus }

Procedure TPlus.OnRender();
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

Constructor TPlus.Create(Owner: TOpenGLControl);
Begin
  Inherited Create(Owner);
  Color := RGBA(0, 0, 0, 0);
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

Procedure TOpenGL_ColorPicDialog.OnRender;
Begin
  Inherited OnRender();
  glPushMatrix;
  glTranslatef(0, 0, 0.01);
  // Rendern der Kind Komponenten
  fColorTable.Render();
  fPicColorButton.enabled := false;
  fPicColorButton.Render();
  fPicColorButton.enabled := true;
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
End;

Procedure TOpenGL_ColorPicDialog.OnPicColorClick(Sender: TObject);
Begin
  If assigned(Shower) Then Begin
    Shower.Color := RGBA(
      min(255, max(0, round(fPicColorButton.FontColor.x * 255))),
      min(255, max(0, round(fPicColorButton.FontColor.y * 255))),
      min(255, max(0, round(fPicColorButton.FontColor.z * 255))), 0);
    OnSetColor(Shower.Color);
  End;
  Shower := Nil;
  visible := false;
End;

Procedure TOpenGL_ColorPicDialog.OnPicColorDBLClick(Sender: TObject);
Begin
  fPicColorButton.Click;
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
    LoadColor(ColorToRGBA(c, 0));
  End;
End;

Constructor TOpenGL_ColorPicDialog.Create(Owner: TOpenGLControl);
Var
  img: Integer;
Begin
  // Alle Elemente vor dem eigentlichen erstellt werden
  // Das hat 2 Gründe
  // 1. Nur so können sie die OnMouse* Events Capturen
  // 2. Sonst braucht man Nil Prüfungen im SetTop, SetLeft
  fColorTable := TOpenGl_Image.Create(Owner);
  fPicColorButton := TCustomOpenGl_Edit.Create(Owner, '');
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
  fColorTable.OnDblClick := @OnPicColorDBLClick;
  fPicColorButton.FontColor := v3(192 / 255, 192 / 255, 192 / 255);
  fPicColorButton.BorderColor := v3(192 / 255, 192 / 255, 192 / 255);
  fPicColorButton.Color := v3(128 / 255, 128 / 255, 128 / 255);
  fPicColorButton.Text := 'Pic color';
  fPicColorButton.Width := 86;
  fPicColorButton.OnClick := @OnPicColorClick;
  fColorInfo.FontColor := v3(192 / 255, 192 / 255, 192 / 255);
  Shower := Nil;
  OnSetColor := Nil;

  fBlack.RaisedColor := RGBA(192, 192, 192, 0);
  fBlack.LoweredColor := RGBA(192, 192, 192, 0);
  fBlack.Height := 18;
  fBlack.Width := 18;
  fBlack.Color := RGBA(0, 0, 0, 0);

  fDarken.RaisedColor := RGBA(192, 192, 192, 0);
  fDarken.LoweredColor := RGBA(192, 192, 192, 0);
  fDarken.Height := 18;
  fDarken.Width := 18;

  fRed.RaisedColor := RGBA(192, 192, 192, 0);
  fRed.LoweredColor := RGBA(192, 192, 192, 0);
  fRed.Height := 18;
  fRed.Width := 18;

  fGreen.RaisedColor := RGBA(192, 192, 192, 0);
  fGreen.LoweredColor := RGBA(192, 192, 192, 0);
  fGreen.Height := 18;
  fGreen.Width := 18;

  fBlue.RaisedColor := RGBA(192, 192, 192, 0);
  fBlue.LoweredColor := RGBA(192, 192, 192, 0);
  fBlue.Height := 18;
  fBlue.Width := 18;

  fBrighten.RaisedColor := RGBA(192, 192, 192, 0);
  fBrighten.LoweredColor := RGBA(192, 192, 192, 0);
  fBrighten.Height := 18;
  fBrighten.Width := 18;

  fWhite.RaisedColor := RGBA(192, 192, 192, 0);
  fWhite.LoweredColor := RGBA(192, 192, 192, 0);
  fWhite.Height := 18;
  fWhite.Width := 18;
  fWhite.Color := RGBA(255, 255, 255, 0);

  fDarkenMinus.Color := RGBA(0, 0, 0, 0);
  fDarkenMinus.Height := 18;
  fDarkenMinus.Width := 17;
  fRedMinus.Color := RGBA(255, 0, 0, 0);
  fRedMinus.Height := 18;
  fRedMinus.Width := 17;
  fGreenMinus.Color := RGBA(0, 255, 0, 0);
  fGreenMinus.Height := 18;
  fGreenMinus.Width := 17;
  fBlueMinus.Color := RGBA(0, 0, 255, 0);
  fBlueMinus.Height := 18;
  fBlueMinus.Width := 17;
  fWhiteMinus.Color := RGBA(255, 255, 255, 0);
  fWhiteMinus.Height := 18;
  fWhiteMinus.Width := 17;

  fDarkenPlus.Color := RGBA(0, 0, 0, 0);
  fDarkenPlus.Height := 16;
  fDarkenPlus.Width := 16;
  fRedPlus.Color := RGBA(255, 0, 0, 0);
  fRedPlus.Height := 16;
  fRedPlus.Width := 16;
  fGreenPlus.Color := RGBA(0, 255, 0, 0);
  fGreenPlus.Height := 16;
  fGreenPlus.Width := 16;
  fBluePlus.Color := RGBA(0, 0, 255, 0);
  fBluePlus.Height := 16;
  fBluePlus.Width := 16;
  fWhitePlus.Color := RGBA(255, 255, 255, 0);
  fWhitePlus.Height := 16;
  fWhitePlus.Width := 16;
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

  Inherited Destroy;
End;

Procedure TOpenGL_ColorPicDialog.LoadColor(aColor: TRGBA);
Begin
  fPicColorButton.FontColor := v3(aColor.r / 255, aColor.g / 255, aColor.b / 255);
  fColorInfo.caption := format('%d/%d/%d', [aColor.r, aColor.g, aColor.b]);
End;

End.

