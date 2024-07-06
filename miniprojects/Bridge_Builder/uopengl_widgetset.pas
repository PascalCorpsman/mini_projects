(******************************************************************************)
(* uOpenGL_WidgetSet.pas                                           ??.??.???? *)
(*                                                                            *)
(* Version     : 0.07                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : <Module_description>                                         *)
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
(* Known Issues: -Wird bei einem Scrollbar an stelle x geklickt so dass der   *)
(*                Scrollbutton nach dem Click eigentlich Rechts vom Cursor    *)
(*                stehen sollte, dann wird der Scrollbutton nicht nach Rechts *)
(*                sondern nach Links verschoben..                             *)
(*                  []     [] x     []                                        *)
(*                                                                            *)
(* History     : 0.01 - Initial version                                       *)
(*               0.02 = Einfügen "ColorToVector"                              *)
(*               0.03 = Einfügen TOpenGl_Listbox.ScrollDown                   *)
(*               0.04 = Fix Speicherloch Bei Freigabe                         *)
(*               0.05 = Erlauben OnClick für TOpenGl_Image                    *)
(*               0.06 = Umbau auf ueventer.pas                                *)
(*               0.07 = Umstellen auf smClamp => deutlich bessere Graphiken   *)
(*                                                                            *)
(******************************************************************************)

Unit uopengl_widgetset;

{$MODE ObjFPC}{$H+}

Interface

Uses
  OpenGLContext,
  dglOpenGL,
  uvectormath,
  forms, // TScrollBarKind = (sbHorizontal, sbVertical);
  Graphics,
  sysutils,
  Controls,
  Classes,
  uOpenGL_ASCII_Font,
  uopengl_truetype_font,
  math,
  LCLType,
  lclintf,
  uopengl_graphikengine,
  ueventer;

Type

  { TOpenGl_BaseClass }

  TOpenGl_BaseClass = Class(TEventerClass) // Die Basisklasse, welche die ganzen Captures und Events Handled, alles im Protected Teil steht zum freien "Override" bereit.
  private
  protected
    Procedure OnRender(); virtual; abstract;
  public
    Name: String; // Einfach nur so, wird intern eigentlich nicht benötig.

    Constructor Create(Owner: TOpenGLControl); override;

    Procedure Render(); // Damit alles Funktionieren kann wie gewünscht muss vorher die Routine WidgetSetGo2d aufgerufen werden !!
  End;

  { TOpenGL_BaseFontClass }

  TOpenGl_BaseFontClass = Class(TOpenGL_BaseClass)
  protected
    Function fGetFontColor: TVector3;
    Procedure fSetFontColor(Value: TVector3);
    Function FGetFontSize: single;
    Procedure FSetFontSize(value: Single);
  protected
    FFont: TOpenGL_TrueType_Font; // Das Kontrollelement bekommt noch zusätzlich eine OpenGLFont
    Property FontColor: TVector3 read fGetFontColor write fSetFontColor;
    Property FontSize: Single read FGetFontSize write FSetFontSize;
  public
    Constructor Create(Owner: TOpenGLControl; FontFile: String); virtual; reintroduce;
    Destructor destroy; override;
  End;

  { TOpenGl_Image }

  TOpenGl_Image = Class(TOpenGL_BaseClass)
  protected
    Fimage: TGraphikItem;
    Procedure OnRender(); override;
    Procedure UpdateDimension;
  public
    Transparent: Boolean; // Das ist noch nicht wirklich schön funktionert aber erst mal ..
    Property OnClick;
    Property OnMouseDown;
    Property OnMouseMove;
    Property OnMouseUp;
    Procedure SetHeight(AValue: integer); override;
    Procedure SetWidth(AValue: integer); override;

    Procedure SetImage(Filename: String); overload;
    Procedure SetImage(OpenGLIndex: integer); overload;

    Constructor Create(Owner: TOpenGLControl); override;
  End;

  { TOpenGL_Label }

  TOpenGl_Label = Class(TOpenGL_BaseFontClass)
  private
    fcaption: String;
  protected
    Procedure OnRender(); override;
    Procedure Setcaption(value: String);
  public
    Property Caption: String read fcaption write Setcaption;
    Property FontColor;
    Property FontSize;
  End;

  { TOpenGL_Button }

  TOpenGl_Button = Class(TOpenGL_BaseClass)
  private
    FNormalTex: TGraphikItem;
    fHoverTex: TGraphikItem;
    fDownTex: TGraphikItem;
  protected
    Procedure SetHeight(AValue: integer); override;
    Procedure SetWidth(AValue: integer); override;
    Procedure OnRender(); override;
    Procedure KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState); override;
  public
    Caption: String;
    Property OnClick;
    (*
     * Lädt die 3 Texturen
     * Normal = die die immer angezeigt wird
     * Hover = die die angezeigt wird, solange sich dir Maus darüber bewegt
     * Down = die die angezeigt wird, solange die Maus gedrückt wurde
     *)
    Procedure LoadTextures(Normal, Hover, Down: String);
  End;

  { TOpenGL_Button_Alpha }

  TOpenGl_Button_Alpha = Class(TOpenGL_Button)
  private
  protected
    Procedure OnRender(); override;
  public
    (*
     * Lädt die 3 Texturen
     * Normal = die die immer angezeigt wird
     * Hover = die die angezeigt wird, solange sich dir Maus darüber bewegt
     * Down = die die angezeigt wird, solange die Maus gedrückt wurde
     * Mask = Alpha Maske ( für alle Gleich )
     *)
    Procedure LoadTextures(Normal, Hover, Down, Mask: String);
  End;

  { TOpenGL_Scrollbar }

  TOpenGL_Scrollbar = Class(TOpenGL_BaseClass)
  private
    FScrollButton: TOpenGL_Button;
    fUpButton: TOpenGL_Button;
    fDownButton: TOpenGL_Button;
    FKind: TScrollBarKind;
    FMax: Integer;
    FMin: integer;
    FPosition: integer;
    FScrolling: Boolean; // Wie  FScrollButton.FMouseDown nur eben im Scrollbar verfügbar.
    Procedure SetKind(AValue: TScrollBarKind);
    Procedure SetMax(AValue: Integer);
    Procedure SetMin(AValue: integer);
    Procedure SetPosition(AValue: integer);
    Procedure OnUpClick(Sender: TObject);
    Procedure OnDownClick(Sender: TObject);
    Procedure OnScrollMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    Procedure OnScrollMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  protected
    Procedure OnRender(); override;
    Procedure SetLeft(AValue: integer); override;
    Procedure SetTop(AValue: integer); override;
    Procedure SetWidth(Value: integer); override;
    Procedure SetHeight(Value: integer); override;
    Procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    Procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    Procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    Procedure UpdateScrollButtonPosition();
  public
    SmallChange: integer;
    LargeChange: integer;
    OnChange: TNotifyEvent;
    Property Position: integer read FPosition write SetPosition;
    Property Min: integer read FMin write SetMin;
    Property Max: Integer read FMax write SetMax;
    Property Kind: TScrollBarKind read FKind write SetKind;
    Constructor Create(Owner: TOpenGLControl); override;
    Destructor Destroy(); override;
  End;

  { TOpenGL_Listbox }

  TOpenGl_Listbox = Class(TOpenGL_BaseFontClass)
  private
    fScrollbar: TOpenGL_Scrollbar;
    fTopIndex: integer;
    FItems: TStringList;
    FLineColors: TVector3Array;
    Procedure fSetItem(Index: integer; Value: String);
    Function fGetItem(Index: integer): String;
    Procedure fSetLineColor(Index: integer; Value: TVector3);
    Function fGetLineColor(index: integer): TVector3;
    Function fgetSorted: boolean;
    Procedure SetSorted(value: Boolean);
    Procedure OnScrollbarChange(Sender: TObject);
    Function getItemCount: integer;
  protected
    Procedure OnRender(); override;

    Procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    Procedure KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState); override;

    Procedure SetLeft(AValue: integer); override;
    Procedure SetTop(AValue: integer); override;
    Procedure SetWidth(Value: integer); override;
    Procedure SetHeight(Value: integer); override;
  public
    Color: TVector3; // HintergrundFarbe
    BorderColor: TVector3; // RandFarbe
    ItemIndex: integer; // Das Aktuell Ausgewählte Element ( oder -1)
    Property Sorted: Boolean read fgetSorted write SetSorted; // Wenn True, dann wird beim Einfügen ( und nur da ) sortiert eingefügt.
    Property OnDblClick;
    Property FontColor; // Der Default Wert der Font beim Adden, hinterher muss man LineColor Nehmen

    Property Items[index: integer]: String read fGetItem write fSetItem;
    Property LineColor[index: integer]: TVector3 read fGetLineColor write fSetLineColor;
    Property ItemCount: Integer read getItemCount;

    Constructor create(Owner: TOpenGLControl; FontFile: String); override;
    Destructor destroy; override;

    Function AddItem(Value: String; Color_: TVector3): integer; overload; // Jede Zeile kann ihre eigene Schriftfarbe haben
    Function AddItem(Value: String): integer; overload; // Jede Zeile kann ihre eigene Schriftfarbe haben
    Procedure ScrollDown;
    Procedure Clear();
  End;

  { TOpenGl_Edit }

  TOpenGl_Edit = Class(TOpenGL_BaseFontClass)
  private
    FLastTimeStamp: Dword;
    FDashVisible: Boolean;
  protected
    Procedure OnRender(); override;
    Function GetHeight: Integer; override;
    Procedure SetHeight(AValue: integer); override;

    Procedure KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState); override;
  public
    Text: String;
    Color: TVector3; // Die Hintergrundfarbe
    BorderColor: TVector3; // Die "RandFarbe"
    PassWordChar: Char;
    Property OnKeyPress;
    Constructor Create(Owner: TOpenGLControl; FontFile: String); override;
  End;

Procedure WidgetSetGo2d(Width_2D, Height_2d: Integer);
Procedure WidgetSetExit2d();
Function ColorToVector(Color: TColor): TVector3;

Implementation

Var
  _2DWidth, _2DHeight: integer;

Procedure WidgetSetGo2d(Width_2D, Height_2d: Integer);
Begin
  _2DWidth := Width_2D;
  _2DHeight := Height_2d;
  glMatrixMode(GL_PROJECTION);
  glPushMatrix(); // Store The Projection Matrix
  glLoadIdentity(); // Reset The Projection Matrix
  glOrtho(0, Width_2D, Height_2d, 0, -1, 1); // Set Up An Ortho Screen
  glMatrixMode(GL_MODELVIEW);
  glPushMatrix(); // Store old Modelview Matrix
  glLoadIdentity(); // Reset The Modelview Matrix
End;

Procedure WidgetSetExit2d;
Begin
  glMatrixMode(GL_PROJECTION);
  glPopMatrix(); // Restore old Projection Matrix
  glMatrixMode(GL_MODELVIEW);
  glPopMatrix(); // Restore old Projection Matrix
End;

Function WidgetSetTransformRoutine(x, y: integer): TPoint;
Var
  dim: Array[0..3] Of Integer;
  xx, yy: integer;
Begin
  If (_2DWidth = 0) Or (_2DHeight = 0) Then Begin
    Raise exception.create('Error 2D-Dimension not found. Did you use WidgetSetGo2d ?');
  End;
  glGetIntegerv(GL_VIEWPORT, @dim[0]);
  xx := round(ConvertDimension(0, dim[2], x, 0, _2DWidth));
  yy := round(ConvertDimension(0, dim[3], y, 0, _2DHeight));
  result := Point(xx, yy);
End;

Function ColorToVector(Color: TColor): TVector3;
Begin
  // TColor ist gespeichert als BGR
  result.x := (color And $FF) / $FF;
  result.y := ((color And $FF00) Shr 8) / $FF;
  result.z := ((color And $FF0000) Shr 16) / $FF;
End;

{ TOpenGL_Scrollbar }

Constructor TOpenGL_Scrollbar.Create(Owner: TOpenGLControl);
Begin
  (*
   * Dadurch, dass die Elemente zuerst erstellt werden hat das 2 Vorteile
   * 1. Alle Sonderfälle auf Nil fallen weg
   * 2. Im Eventer klauen sie der Komponente die OnMouse* Events -> und sind
   *    damit wie WYSIWYG ;)
   *)
  fUpButton := TOpenGL_Button.create(Owner);
  fScrollButton := TOpenGL_Button.create(Owner);
  fDownButton := TOpenGL_Button.create(Owner);
  Inherited Create(Owner);
  fUpButton.OnClick := @OnUpClick;
  fDownButton.OnClick := @OnDownClick;
  FScrollButton.OnMouseDown := @OnScrollMouseDown;
  FScrollButton.OnMouseUp := @OnScrollMouseUp;
  fkind := sbVertical;
  FScrolling := false;
  fMin := 0;
  FMax := 100;
  FPosition := 0;
  SmallChange := 1;
  LargeChange := 1;
  OnChange := Nil;
  // TODO: die 15x15 sind hardcodiert, das könnte "Besser" werden.
  fUpButton.Height := 15;
  fUpButton.Width := 15;
  Fdownbutton.Height := 15;
  Fdownbutton.Width := 15;
  FScrollButton.Height := 15;
  FScrollButton.Width := 15;
  // Initialisieren der Button Dimensiontn
  Top := 10;
  Left := 10;
  Width := 15;
  Height := 100;
End;

Destructor TOpenGL_Scrollbar.Destroy();
Begin
  FScrollButton.free;
  fUpButton.free;
  fDownButton.free;
  Inherited Destroy;
End;

Procedure TOpenGL_Scrollbar.SetKind(AValue: TScrollBarKind);
Var
  w, h: integer;
Begin
  If FKind = AValue Then Exit;
  w := Width;
  h := Height;
  FKind := AValue;
  fUpButton.Width := 15;
  fUpButton.Height := 15;
  FScrollButton.Width := 15;
  FScrollButton.Height := 15;
  fDownButton.Width := 15;
  fDownButton.Width := 15;
  // So werden alle Unterelemente auf jeden Fall neu gesetzt...
  SetWidth(h);
  SetHeight(w);
  SetLeft(left);
  SetTop(Top);
  UpdateScrollButtonPosition();
End;

Procedure TOpenGL_Scrollbar.SetMax(AValue: Integer);
Begin
  If FMax = AValue Then Exit;
  If avalue <= fmin Then exit;
  FMax := AValue;
  Position := math.min(fmax, Position);
  UpdateScrollButtonPosition;
End;

Procedure TOpenGL_Scrollbar.SetMin(AValue: integer);
Begin
  If FMin = AValue Then Exit;
  If avalue >= fmax Then exit;
  FMin := AValue;
  Position := math.max(fmin, Position);
  UpdateScrollButtonPosition;
End;

Procedure TOpenGL_Scrollbar.SetPosition(AValue: integer);
Begin
  avalue := math.min(fmax, math.max(fmin, AValue));
  If FPosition = AValue Then Exit;
  FPosition := AValue;
  UpdateScrollButtonPosition;
  If assigned(OnChange) Then
    OnChange(self);
End;

Procedure TOpenGL_Scrollbar.OnUpClick(Sender: TObject);
Begin
  Position := Position - SmallChange;
End;

Procedure TOpenGL_Scrollbar.OnDownClick(Sender: TObject);
Begin
  Position := Position + SmallChange;
End;

Procedure TOpenGL_Scrollbar.OnScrollMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
  FScrolling := ssleft In shift;
End;

Procedure TOpenGL_Scrollbar.OnScrollMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
  FScrolling := false;
End;

Procedure TOpenGL_Scrollbar.OnRender();
Begin
  If FMouseHover Then Begin
    glcolor4f(0, 1, 1, 1);
  End
  Else Begin
    glcolor4f(1, 1, 1, 1);
  End;
  glbegin(GL_QUADS);
  glVertex2f(left, top + Height);
  glVertex2f(left + Width, top + Height);
  glVertex2f(left + Width, Top);
  glVertex2f(left, top);
  glend;
  fUpButton.Render();
  fDownButton.Render();
  FScrollButton.Render();
End;

Procedure TOpenGL_Scrollbar.SetLeft(AValue: integer);
Begin
  Inherited SetLeft(AValue);
  fUpButton.Left := AValue;
  If FKind = sbVertical Then Begin
    FScrollButton.Left := AValue;
    fDownButton.Left := AValue;
  End
  Else Begin
    UpdateScrollButtonPosition();
    fDownButton.Left := Left + Width - 15;
  End;
End;

Procedure TOpenGL_Scrollbar.SetTop(AValue: integer);
Begin
  Inherited SetTop(AValue);
  fUpButton.Top := AValue;
  If FKind = sbVertical Then Begin
    UpdateScrollButtonPosition();
    fDownButton.Top := AValue + Height - 15;
  End
  Else Begin
    FScrollButton.Top := AValue;
    fDownButton.Top := AValue;
  End;
End;

Procedure TOpenGL_Scrollbar.SetWidth(Value: integer);
Begin
  Inherited SetWidth(Value);
  If FKind = sbVertical Then Begin
    fUpButton.Width := value;
    FScrollButton.Width := value;
    fDownButton.Width := value;
  End
  Else Begin
    UpdateScrollButtonPosition();
    fDownButton.left := left + value - fDownButton.Width;
  End;
End;

Procedure TOpenGL_Scrollbar.SetHeight(Value: integer);
Begin
  Inherited SetHeight(Value);
  If FKind = sbVertical Then Begin
    UpdateScrollButtonPosition();
    fDownButton.Top := top + value - fDownButton.Height;
  End
  Else Begin
    fUpButton.Height := value;
    FScrollButton.Height := value;
    fDownButton.Height := value;
  End;
End;

Procedure TOpenGL_Scrollbar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
Begin
  Inherited MouseDown(Button, Shift, X, Y);
  If (Not fUpButton.FMouseHover) And (Not fDownButton.FMouseHover) And (Not FScrollButton.FMouseHover) Then Begin
    If FKind = sbVertical Then Begin
      If FScrollButton.Top + Top < y Then Begin
        Position := Position + LargeChange;
      End
      Else Begin
        Position := Position - LargeChange;
      End;
    End
    Else Begin
      If FScrollButton.left + left < x Then Begin
        Position := Position + LargeChange;
      End
      Else Begin
        Position := Position - LargeChange;
      End;
    End;
    UpdateScrollButtonPosition;
  End;
End;

Procedure TOpenGL_Scrollbar.MouseMove(Shift: TShiftState; X, Y: Integer);
Var
  P: Single;
Begin
  Inherited MouseMove(Shift, X, Y);
  If FScrolling Then Begin
    If FKind = sbVertical Then Begin
      If (y >= fUpButton.Height) And (y <= height - fdownButton.Height) Then Begin
        p := ConvertDimension(fUpButton.Height, height - fdownButton.Height, y, fmin, fmax);
        Position := round(p);
      End;
    End
    Else Begin
      If (x >= fUpButton.width) And (x <= width - fdownButton.width) Then Begin
        p := ConvertDimension(fUpButton.width, width - fdownButton.width, x, fmin, fmax);
        Position := round(p);
      End;
    End;
    UpdateScrollButtonPosition;
  End;
End;

Procedure TOpenGL_Scrollbar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
Begin
  Inherited MouseUp(Button, Shift, X, Y);
  FScrolling := false;
End;

Procedure TOpenGL_Scrollbar.UpdateScrollButtonPosition();
Var
  delta: integer;
  s: Single;
Begin
  If FKind = sbVertical Then Begin
    delta := height - fUpButton.Height - fDownButton.Height - FScrollButton.Height;
    s := ConvertDimension(fmin, fmax, FPosition, 0, delta);
    FScrollButton.Top := top + round(s) + fUpButton.Height;
  End
  Else Begin
    delta := width - fUpButton.width - fDownButton.width - FScrollButton.width;
    s := ConvertDimension(fmin, fmax, FPosition, 0, delta);
    FScrollButton.left := left + round(s) + fUpButton.width;
  End;
End;

{ TOpenGL_Button_Alpha }

Procedure TOpenGL_Button_Alpha.OnRender;
Var
  b: {$IFDEF USE_GL}Byte{$ELSE}Boolean{$ENDIF};
Begin
  B := glIsEnabled(gl_Blend);
  If Not (b{$IFDEF USE_GL} = 1{$ENDIF}) Then
    glenable(gl_Blend);
  glBlendFunc(GL_ONE_MINUS_SRC_ALPHA, GL_SRC_ALPHA);
  Inherited OnRender;
  If Not (b{$IFDEF USE_GL} = 1{$ENDIF}) Then
    gldisable(gl_blend);
End;

Procedure TOpenGL_Button_Alpha.LoadTextures(Normal, Hover, Down, Mask: String);
Begin
  FNormalTex := OpenGL_GraphikEngine.LoadAlphaGraphikItem(normal, Mask, smClamp);
  fHoverTex := OpenGL_GraphikEngine.LoadAlphaGraphikItem(hover, Mask, smClamp);
  fDownTex := OpenGL_GraphikEngine.LoadAlphaGraphikItem(down, Mask, smClamp);
  // TODO: Theoretisch müsste man hier ne AV-Werfen, wenn die Größen der Texturen unterschiedlich sind ...
  Width := FNormalTex.OrigWidth;
  Height := FNormalTex.OrigHeight;
End;

{ TOpenGL_Image }

Constructor TOpenGl_Image.Create(Owner: TOpenGLControl);
Begin
  Inherited Create(Owner);
  Transparent := false;
End;

Procedure TOpenGl_Image.OnRender();
Var
  bool: GLboolean;
Begin
  If Transparent Then Begin
    RenderAlphaQuad(top, left, Fimage);
  End
  Else Begin
    bool := glIsEnabled(gl_Blend);
    If bool Then
      gldisable(gl_blend);
    glColor4f(1, 1, 1, 1);
    RenderQuad(top, left, Fimage);
    If bool Then
      glenable(gl_blend);
  End;
End;

Procedure TOpenGl_Image.UpdateDimension;
Begin
  Width := fImage.OrigWidth;
  Height := fImage.OrigHeight;
End;

Procedure TOpenGl_Image.SetHeight(AValue: integer);
Var
  s: Single;
Begin
  Inherited SetHeight(AValue);
  (*
   * Das Verhältniss von Fimage.OrigHeight / Fimage.StretchedHeight darf
   * sich durch die Größenänderung nicht verändern, sonst stimmt das Rendern
   * nachher nicht mehr ..
   *)
  If Fimage.StretchedHeight = 0 Then
    s := 1
  Else
    s := Fimage.OrigHeight / Fimage.StretchedHeight;
  Fimage.OrigHeight := AValue;
  Fimage.StretchedHeight := round(Fimage.OrigHeight / s);
End;

Procedure TOpenGl_Image.SetWidth(AValue: integer);
Var
  s: Single;
Begin
  Inherited SetWidth(AValue);
  (*
   * Das Verhältniss von Fimage.OrigHeight / Fimage.StretchedHeight darf
   * sich durch die Größenänderung nicht verändern, sonst stimmt das Rendern
   * nachher nicht mehr ..
   *)
  If Fimage.StretchedWidth = 0 Then
    s := 1
  Else
    s := Fimage.OrigWidth / Fimage.StretchedWidth;
  Fimage.OrigWidth := AValue;
  Fimage.StretchedWidth := round(Fimage.OrigWidth / s);
End;

Procedure TOpenGl_Image.SetImage(Filename: String);
Begin
  fImage := OpenGL_GraphikEngine.LoadGraphikItem(Filename, smClamp);
  UpdateDimension;
End;

Procedure TOpenGl_Image.SetImage(OpenGLIndex: integer);
Begin
  fImage := OpenGL_GraphikEngine.GetInfo(OpenGLIndex);
  UpdateDimension;
End;

{ TOpenGL_Listbox }

Constructor TOpenGl_Listbox.create(Owner: TOpenGLControl; FontFile: String);
Begin
  fScrollbar := TOpenGL_Scrollbar.create(Owner);
  Inherited Create(Owner, FontFile);
  fScrollbar.OnChange := @OnScrollbarChange;
  FItems := TStringList.Create;
  fScrollbar.top := 0;
  fScrollbar.Max := 0;
  fScrollbar.Visible := false;
  Color := v3(0, 0, 0);
  BorderColor := v3(0.5, 0.5, 0.5);
  FontColor := v3(1, 1, 1);
  Sorted := false;
  fTopIndex := 0;
  ItemIndex := -1;
  Width := 150;
  height := 100;
  clear;
End;

Procedure TOpenGl_Listbox.SetWidth(Value: integer);
Begin
  Inherited SetWidth(Value);
  fScrollbar.left := left + Width - fScrollbar.Width;
End;

Procedure TOpenGl_Listbox.SetHeight(Value: integer);
Begin
  Inherited SetHeight(Value);
  fScrollbar.Height := value;
End;

Destructor TOpenGl_Listbox.destroy;
Begin
  Clear;
  fitems.free;
  fScrollbar.free;
  Inherited destroy;
End;

Procedure TOpenGl_Listbox.fSetItem(Index: integer; Value: String);
Begin
  If (index >= 0) And (index < FItems.Count) Then
    FItems[index] := value;
End;

Function TOpenGl_Listbox.fGetItem(Index: integer): String;
Begin
  result := '';
  If (index >= 0) And (index < FItems.Count) Then
    result := Fitems[index];
End;

Procedure TOpenGl_Listbox.fSetLineColor(Index: integer; Value: TVector3);
Begin
  If (index >= 0) And (index < FItems.Count) Then
    FLineColors[index] := Value;
End;

Function TOpenGl_Listbox.fGetLineColor(index: integer): TVector3;
Begin
  result := v3(0, 0, 0);
  If (index >= 0) And (index < FItems.Count) Then
    result := FLineColors[index];
End;

Function TOpenGl_Listbox.fgetSorted: boolean;
Begin
  result := FItems.Sorted;
End;

Procedure TOpenGl_Listbox.SetSorted(value: Boolean);
Begin
  FItems.Sorted := value;
End;

Procedure TOpenGl_Listbox.OnScrollbarChange(Sender: TObject);
Begin
  fTopIndex := fScrollbar.Position;
End;

Function TOpenGl_Listbox.getItemCount: integer;
Begin
  result := FItems.Count;
End;

Procedure TOpenGl_Listbox.OnRender();
Var
  lw, nw, ps: Single;
  dim: Array[0..3] Of Integer;
  i: integer;
  s: String;
  ScrollbarWidth: integer;
Begin
  If fScrollbar.Visible Then Begin
    ScrollbarWidth := fScrollbar.Width;
  End
  Else Begin
    ScrollbarWidth := 0;
  End;
  glGetIntegerv(GL_VIEWPORT, @dim[0]);
  nw := max(dim[2] / _2DWidth, dim[3] / _2DHeight);
  glGetFloatv(GL_POINT_SIZE, @ps);
  glGetFloatv(GL_LINE_WIDTH, @lw);
  glPointSize(nw); // Wegen der Font
  glBindTexture(GL_TEXTURE_2D, 0);
  // Erst mal nen Hintergrund malen
  glColor3f(Color.x, Color.y, Color.z);
  glbegin(GL_QUADS);
  glvertex2f(left, top + height);
  glvertex2f(left + width - ScrollbarWidth, top + height);
  glvertex2f(left + width - ScrollbarWidth, top);
  glvertex2f(left, top);
  glend;
  glLineWidth(4);
  glColor3f(BorderColor.x, BorderColor.y, BorderColor.z);
  glbegin(GL_LINE_LOOP);
  glvertex2f(left, top + height);
  glvertex2f(left + width, top + height);
  glvertex2f(left + width, top);
  glvertex2f(left, top);
  glend;
  // Rendern der Elemente
  For i := 0 To trunc((height - 8) / ffont.TextHeight('8')) - 1 Do Begin
    If fTopIndex + i < FItems.Count Then Begin
      // Anzeigen der Selectierten zeile
      If fTopIndex + i = ItemIndex Then Begin
        glLineWidth(2);
        glColor3f(0, 0, 1);
        glbegin(GL_QUADS);
        glvertex2f(left + 2, 4 + top + round(ffont.TextHeight('8') * (i + 1)));
        glvertex2f(left + width - 2 - ScrollbarWidth, 4 + top + round(ffont.TextHeight('8') * (i + 1)));
        glvertex2f(left + width - 2 - ScrollbarWidth, 4 + top + round(ffont.TextHeight('8') * (i + 0)));
        glvertex2f(left + 2, 4 + top + round(ffont.TextHeight('8') * (i + 0)));
        glend;
        glLineWidth(4);
        glColor3f(1, 0, 0);
        glbegin(GL_LINE_LOOP);
        glvertex2f(left + 2, 4 + top + round(ffont.TextHeight('8') * (i + 1)));
        glvertex2f(left + width - 2 - ScrollbarWidth, 4 + top + round(ffont.TextHeight('8') * (i + 1)));
        glvertex2f(left + width - 2 - ScrollbarWidth, 4 + top + round(ffont.TextHeight('8') * (i + 0)));
        glvertex2f(left + 2, 4 + top + round(ffont.TextHeight('8') * (i + 0)));
        glend;
      End;
      ffont.Color3v := FLineColors[fTopIndex + i];
      s := FItems[i + fTopIndex];
      While (ffont.TextWidth(s) > width - 4 - ScrollbarWidth) And (s <> '') Do Begin
        delete(s, length(s), 1);
      End;
      ffont.Textout(left + 4, 4 + top + round(ffont.TextHeight('8') * i), s);
    End
    Else Begin
      break;
    End;
  End;
  glLineWidth(lw);
  glPointSize(ps);
  fScrollbar.Render();
End;

Procedure TOpenGl_Listbox.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
Begin
  Inherited MouseUp(Button, Shift, X, Y);
  If Not fScrollbar.FMouseHover Then Begin
    ItemIndex := min(FItems.Count - 1, y Div round(ffont.TextHeight('8')) + fTopIndex);
  End;
End;

Procedure TOpenGl_Listbox.KeyUp(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Var
  deltaPage: integer;
Begin
  Inherited KeyUp(Sender, Key, Shift);
  If Not FFocus Then exit;
  If key = VK_RETURN Then Begin
    DblClick();
  End;
  If Key = vk_up Then Begin
    ItemIndex := max(0, ItemIndex - 1);
    If ItemIndex < fTopIndex Then fTopIndex := ItemIndex;
    fScrollbar.Position := fTopIndex;
  End;
  If Key = vk_down Then Begin
    ItemIndex := min(FItems.Count - 1, ItemIndex + 1);
    If (ItemIndex + fTopIndex + 1) * ffont.TextHeight('8') > height - 8 Then Begin
      fTopIndex := min(fTopIndex + 1, Fitems.count - integer(trunc((height - 8) / ffont.TextHeight('8'))));
    End;
    fScrollbar.Position := fTopIndex;
  End;
  If key = VK_NEXT Then Begin
    deltaPage := trunc((height - 8) / ffont.TextHeight('8'));
    ItemIndex := min(FItems.Count - 1, ItemIndex + deltaPage);
    If (ItemIndex + fTopIndex + 1) * ffont.TextHeight('8') > height - 8 Then Begin
      fTopIndex := min(fTopIndex + deltaPage, Fitems.count - integer(trunc((height - 8) / ffont.TextHeight('8'))));
    End;
    fScrollbar.Position := fTopIndex;
  End;
  If key = VK_PRIOR Then Begin
    deltaPage := trunc((height - 8) / ffont.TextHeight('8'));
    ItemIndex := max(0, ItemIndex - deltaPage);
    If ItemIndex < fTopIndex Then Begin
      fTopIndex := max(fTopIndex - deltaPage, 0);
    End;
    fScrollbar.Position := fTopIndex;
  End;
End;

Procedure TOpenGl_Listbox.SetLeft(AValue: integer);
Begin
  Inherited SetLeft(AValue);
  fScrollbar.Left := left + Width - fScrollbar.Width;
End;

Procedure TOpenGl_Listbox.SetTop(AValue: integer);
Begin
  Inherited SetTop(AValue);
  fScrollbar.Top := Top;
End;

Function TOpenGl_Listbox.AddItem(Value: String; Color_: TVector3): integer;
Var
  deltaPage, j, i: integer;
Begin
  i := FItems.Add(value);
  setlength(FLineColors, high(FLineColors) + 2);
  If Sorted Then Begin
    For j := high(FLineColors) Downto i + 1 Do
      FLineColors[j] := FLineColors[j - 1];
  End;
  FLineColors[i] := Color_;
  result := i;
  deltaPage := trunc((height - 8) / ffont.TextHeight('8'));
  If fitems.Count - deltaPage > 0 Then Begin
    fScrollbar.Visible := true;
    fScrollbar.Max := fitems.Count - deltaPage;
  End;
End;

Function TOpenGl_Listbox.AddItem(Value: String): integer;
Begin
  result := AddItem(value, FontColor);
End;

Procedure TOpenGl_Listbox.ScrollDown;
Begin
  If fScrollbar.Visible Then Begin
    fScrollbar.Position := fScrollbar.Max;
  End;
End;

Procedure TOpenGl_Listbox.Clear();
Begin
  setlength(FLineColors, 0);
  FItems.Clear;
  fTopIndex := 0;
  ItemIndex := -1;
  fScrollbar.Visible := false;
End;

{ TOpenGL_Button }

Procedure TOpenGl_Button.SetHeight(AValue: integer);
Var
  s: Single;
Begin
  Inherited SetHeight(AValue);
  (*
   * Das Verhältniss von FNormalTex.OrigHeight / FNormalTex.StretchedHeight darf
   * sich durch die Größenänderung nicht verändern, sonst stimmt das Rendern
   * nachher nicht mehr ..
   *)
  s := FNormalTex.OrigHeight / FNormalTex.StretchedHeight;
  FNormalTex.OrigHeight := AValue;
  FNormalTex.StretchedHeight := round(FNormalTex.OrigHeight / s);

  s := fDownTex.OrigHeight / fDownTex.StretchedHeight;
  fDownTex.OrigHeight := AValue;
  fDownTex.StretchedHeight := round(fDownTex.OrigHeight / s);

  s := fHoverTex.OrigHeight / fHoverTex.StretchedHeight;
  fHoverTex.OrigHeight := AValue;
  fHoverTex.StretchedHeight := round(fHoverTex.OrigHeight / s);
End;

Procedure TOpenGl_Button.SetWidth(AValue: integer);
Var
  s: Single;
Begin
  Inherited SetWidth(AValue);
  (*
   * Das Verhältniss von FNormalTex.OrigHeight / FNormalTex.StretchedHeight darf
   * sich durch die Größenänderung nicht verändern, sonst stimmt das Rendern
   * nachher nicht mehr ..
   *)
  s := FNormalTex.OrigWidth / FNormalTex.StretchedWidth;
  FNormalTex.OrigWidth := AValue;
  FNormalTex.StretchedWidth := round(FNormalTex.OrigWidth / s);

  s := fDownTex.OrigWidth / fDownTex.StretchedWidth;
  fDownTex.OrigWidth := AValue;
  fDownTex.StretchedWidth := round(fDownTex.OrigWidth / s);

  s := fHoverTex.OrigWidth / fHoverTex.StretchedWidth;
  fHoverTex.OrigWidth := AValue;
  fHoverTex.StretchedWidth := round(fHoverTex.OrigWidth / s);
End;

Procedure TOpenGl_Button.OnRender();
Begin
  glColor4f(1, 1, 1, 1);
  If fMouseDown Then Begin
    If fDownTex.Image = 0 Then Begin
      glcolor3f(1, 0, 0);
      glbegin(GL_QUADS);
      glVertex2f(left, top + Height);
      glVertex2f(left + Width, top + Height);
      glVertex2f(left + Width, Top);
      glVertex2f(left, top);
      glend;
    End
    Else Begin
      RenderAlphaQuad(top, left, fDownTex);
    End;
  End
  Else Begin
    If FMouseHover Then Begin
      If fHoverTex.Image = 0 Then Begin
        glcolor3f(0, 1, 0);
        glbegin(GL_QUADS);
        glVertex2f(left, top + Height);
        glVertex2f(left + Width, top + Height);
        glVertex2f(left + Width, Top);
        glVertex2f(left, top);
        glend;
      End
      Else Begin
        RenderAlphaQuad(top, left, fHoverTex);
      End;
    End
    Else Begin
      If FNormalTex.Image = 0 Then Begin
        glcolor3f(0, 0, 1);
        glbegin(GL_QUADS);
        glVertex2f(left, top + Height);
        glVertex2f(left + Width, top + Height);
        glVertex2f(left + Width, Top);
        glVertex2f(left, top);
        glend;
      End
      Else Begin
        RenderAlphaQuad(top, left, FNormalTex);
      End;
    End;
  End;
End;

Procedure TOpenGl_Button.KeyUp(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Var
  s: String;
Begin
  Inherited KeyUp(Sender, Key, Shift);
  (*
   * Wenn eine Caption mit einem & definiert wurde, dann wird das hier abgefragt
   * und mittels Tastatur kann dann der OnClick Event ausgelöst werden.
   *)
  If Visible And (pos('&', Caption) <> 0) Then Begin
    s := uppercase(copy(caption, pos('&', Caption) + 1, 1));
    If length(s) <> 0 Then Begin
      If (ssalt In shift) And (key = Ord(s[1])) Then Begin
        Click();
      End;
    End;
  End;
End;

Procedure TOpenGl_Button.LoadTextures(Normal, Hover, Down: String);
Begin
  FNormalTex := OpenGL_GraphikEngine.LoadAlphaGraphikItem(normal, smClamp);
  fHoverTex := OpenGL_GraphikEngine.LoadAlphaGraphikItem(hover, smClamp);
  fDownTex := OpenGL_GraphikEngine.LoadAlphaGraphikItem(down, smClamp);
  // TODO: Theoretisch müsste man hier ne AV-Werfen, wenn die Größen der Texturen unterschiedlich sind ...
  Width := FNormalTex.OrigWidth;
  Height := FNormalTex.OrigHeight;
End;

{ TOpenGL_Label }

Procedure TOpenGL_Label.OnRender;
Var
  nw, ps: Single;
  d: Boolean;
  dim: Array[0..3] Of Integer;
Begin
  If Visible Then Begin
    glGetIntegerv(GL_VIEWPORT, @dim[0]);
    nw := max(dim[2] / _2DWidth, dim[3] / _2DHeight);
    glGetFloatv(GL_POINT_SIZE, @ps);
    glPointSize(nw);
    d := glIsEnabled(GL_DEPTH_TEST);
    If d Then Begin
      glDisable(GL_DEPTH_TEST);
    End;
    ffont.Textout(left, top, Caption);
    glPointSize(ps);
    If d Then Begin
      glenable(GL_DEPTH_TEST);
    End;
  End;
End;

Procedure TOpenGl_Label.Setcaption(value: String);
Begin
  fcaption := value;
  Height := round(FFont.TextHeight(Caption));
  Width := round(FFont.TextWidth(Caption));
End;

{ TOPenGL_Edit }

Constructor TOpenGl_Edit.Create(Owner: TOpenGLControl; FontFile: String);
Begin
  Inherited create(Owner, FontFile);
  FLastTimeStamp := GetTickCount;
  BorderColor := v3(0.5, 0.5, 0.5);
  Color := v3(0.25, 0.25, 0.25);
  FontColor := v3(1, 1, 1);
  FDashVisible := false;
  PassWordChar := #0;
  Text := Self.ClassName;
End;

Procedure TOpenGl_Edit.OnRender();
Var
  lw: Single;
  tw: integer;
  tex: String;
  i: Integer;
Begin
  glBindTexture(GL_TEXTURE_2D, 0);
  glGetFloatv(GL_LINE_WIDTH, @lw);
  // Zuerst den Hintergrund
  glcolor3f(Color.x, Color.y, Color.z);
  glbegin(GL_QUADS);
  glVertex2f(left, top + Height);
  glVertex2f(left + Width, top + Height);
  glVertex2f(left + Width, top);
  glVertex2f(left, top);
  glend();
  glLineWidth(2);
  glcolor3f(BorderColor.x, BorderColor.y, BorderColor.z);
  glbegin(GL_LINE_LOOP);
  glVertex2f(left, top + Height);
  glVertex2f(left + Width, top + Height);
  glVertex2f(left + Width, top);
  glVertex2f(left, top);
  glend();
  tex := text;
  If PassWordChar <> #0 Then Begin
    For i := 1 To length(tex) Do Begin
      tex[i] := PassWordChar;
    End;
  End;
  While (FFont.TextWidth(tex) > Width - 6) And (tex <> '') Do Begin
    delete(tex, 1, 1);
  End;
  FFont.Color3v := FontColor;
  FFont.Textout(left + 2, top + 2, tex);
  // Dann den Cursor
  If FFocus Then Begin
    If FDashVisible Then Begin
      tw := round(FFont.TextWidth(Tex));
      glcolor3f(BorderColor.x, BorderColor.y, BorderColor.z);
      glbegin(GL_LINES);
      glVertex2f(left + tw + 4, top + 3);
      glVertex2f(left + tw + 4, top + height - 3);
      glend();
    End;
  End;
  If FLastTimeStamp + 1000 < GetTickCount Then Begin
    FDashVisible := Not FDashVisible;
    FLastTimeStamp := GetTickCount;
  End;
  glLineWidth(lw);
End;

Function TOpenGl_Edit.GetHeight: Integer;
Begin
  Result := round(FFont.TextHeight('8')) + 4;
End;

Procedure TOpenGl_Edit.SetHeight(AValue: integer);
Begin
  // Die Höhe eines TEdit kann nicht geändert werden, bzw passt sich das Edit immer an die schrifthöhe an !
  Inherited SetHeight(GetHeight);
End;

Procedure TOpenGl_Edit.KeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState
  );
Begin
  Inherited KeyUp(Sender, Key, Shift);
  If FFocus Then Begin
    If (key > 31) And (key < 128) Then Begin // Das Mag ein wenig Rabiat sein, Reicht aber aus, da Sonderzeichen der FFont wohl Probleme machen..
      If (key >= VK_A) And (key <= VK_Z) Then Begin
        If ssShift In Shift Then Begin
          Text := text + chr(key);
        End
        Else Begin
          Text := text + chr(key - VK_A + 97); // 97 = Ascii Wert für "a"
        End;
      End
      Else Begin
        Text := text + chr(key);
      End;
    End
    Else Begin
      If key = VK_BACK Then Begin
        text := copy(text, 1, length(text) - 1);
      End;
    End;
  End;
End;

{ TOpenGL_BaseFontClass }

Function TOpenGL_BaseFontClass.fGetFontColor: TVector3;
Begin
  result := FFont.Color3v;
End;

Procedure TOpenGL_BaseFontClass.fSetFontColor(Value: TVector3);
Begin
  FFont.Color3v := value;
End;

Function TOpenGL_BaseFontClass.FGetFontSize: single;
Begin
  result := FFont.Size;
End;

Procedure TOpenGL_BaseFontClass.FSetFontSize(value: Single);
Begin
  FFont.Size := value;
End;

Constructor TOpenGL_BaseFontClass.create(Owner: TOpenGLControl; FontFile: String
  );
Begin
  FFont := TOpenGL_TrueType_Font.Create();
  ffont.LoadfromFile(FontFile);
  Inherited create(Owner);
End;

Destructor TOpenGL_BaseFontClass.destroy;
Begin
  ffont.free;
  Inherited destroy;
End;

{ TOpenGL_BaseClass }

Constructor TOpenGl_BaseClass.Create(Owner: TOpenGLControl);
Begin
  Inherited create(Owner);
  Name := '';
  Top := 10;
  Left := 10;
  Width := 100;
  Height := 25;
End;

Procedure TOpenGl_BaseClass.Render();
Var
  l, d: Boolean;
Begin
  If Visible Then Begin
    l := glIsEnabled(GL_LIGHTING);
    If l Then Begin
      glDisable(GL_LIGHTING);
    End;
    d := glIsEnabled(GL_DEPTH_TEST);
    If d Then Begin
      glDisable(GL_DEPTH_TEST);
    End;
    glcolor4f(1, 1, 1, 1);
    OnRender();
    If d Then Begin
      glenable(GL_DEPTH_TEST);
    End;
    If l Then Begin
      glEnable(GL_LIGHTING);
    End;
  End;
End;

Initialization
  ueventer.TransformRoutine := @WidgetSetTransformRoutine;

End.

