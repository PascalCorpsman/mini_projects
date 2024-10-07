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
Unit upixeleditor;

{$MODE ObjFPC}{$H+}
{$MODESWITCH nestedprocvars}

Interface

Uses
  Graphics, Classes, SysUtils, Controls, OpenGLContext, uopengl_widgetset, upixeleditorlcl,
  ExtCtrls, uimage, ugraphics, upixeleditor_types, uundo;

Const
  (*
   * History: 0.01 - Initialversion
   *)
  Version = '0.01';

  (*
   * History: 1 - Initialversion
   *          2 - ADD Colorpalette
   *)
  PixelEditorFileversion: integer = 2;

  (*
   * History: 1 - Initialversion
   *)
  ColorPaletteFileversion: integer = 1;

Type

  { TPixelEditor }

  TPixelEditor = Class
  private
    fSettings: TSettings;
    fCursor: TCursor;
    FOwner: TOpenGLControl;
    fScrollInfo: TScrollInfo;
    fZoom: integer; // Akruelle Zoomstufe in %
    fAktualLayer: TLayer; // TODO: das kann stand jetzt raus !
    fImage: TImage; // Das Object um das es hier eigentlich geht ;)
    fUndo: TUndoEngine;

    FElements: Array Of TOpenGL_BaseClass;

    // Titelleiste oben
    NewButton: TOpenGL_Bevel;
    OpenButton: TOpenGL_Bevel;
    SaveButton: TOpenGL_Bevel;
    SaveAsButton: TOpenGL_Bevel;
    ExitButton: TOpenGL_Bevel;
    GridButton: TOpenGL_Bevel;
    ZoomInButton: TOpenGL_Bevel;
    ZoomInfoTextbox: TOpenGL_Textbox;
    ZoomOutButton: TOpenGL_Bevel;
    OptionsButton: TOpenGL_Bevel;
    UndoButton: TOpenGL_Bevel;

    // Menüleiste Links
    SelectButton: TOpenGL_Bevel;
    SelectModeButton: TOpenGL_ToggleButton;
    RotateCounterClockwise90: TOpenGL_Bevel;
    RotateClockwise90: TOpenGL_Bevel;
    Rotate180: TOpenGL_Bevel;
    RotateAngle: TOpenGL_Bevel;
    BrightenButton: TOpenGL_Bevel;
    DarkenButton: TOpenGL_Bevel;
    CurserSize1: TOpenGL_Bevel;
    CurserSize2: TOpenGL_Bevel;
    CurserSize3: TOpenGL_Bevel;
    CurserSize4: TOpenGL_Bevel;
    EraserButton: TOpenGL_Bevel;
    PencilButton: TOpenGL_Bevel;
    CursorRoundShape1: TOpenGL_Bevel;
    CursorRoundShape2: TOpenGL_Bevel;
    CursorRoundShape3: TOpenGL_Bevel;
    CursorSquareShape1: TOpenGL_Bevel;
    CursorSquareShape2: TOpenGL_Bevel;
    CursorSquareShape3: TOpenGL_Bevel;
    LineButton: TOpenGL_Bevel;
    CircleButton: TOpenGL_Bevel;
    SquareButton: TOpenGL_Bevel;
    OutlineButton: TOpenGL_Bevel;
    FilledButton: TOpenGL_Bevel;
    MirrorButton: TOpenGL_Bevel;
    MirrorCenterButton: TOpenGL_ToggleButton;
    MirrorHorButton: TOpenGL_Bevel;
    MirrorVertButton: TOpenGL_Bevel;
    Mirror4Button: TOpenGL_Bevel;
    FloodFillButton: TOpenGL_Bevel;
    FloodFillModeButton: TOpenGL_Bevel;
    ColorPickButton: TOpenGL_Bevel;

    // Menüleiste unten
    ColorPicDialog: TOpenGL_ColorPicDialog;

    ColorPreview: TOpenGL_ForeBackGroundColorBox;
    ColorTransparent: TOpenGL_TransparentColorBox;
    Color1: TOpenGL_ColorBox;
    Color2: TOpenGL_ColorBox;
    Color3: TOpenGL_ColorBox;
    Color4: TOpenGL_ColorBox;
    Color5: TOpenGL_ColorBox;
    Color6: TOpenGL_ColorBox;
    Color7: TOpenGL_ColorBox;
    Color8: TOpenGL_ColorBox;

    AktColorInfoLabel: TOpenGl_Label;

    InfoLabel: TOpenGl_Label; // Anzeige Aktuelle Position und Pixelfarbe unter Position
    InfoDetailLabel: TOpenGl_Label; // Zeigt beim Linien/ Ellipse/ Rechteck tool die "Delta's" an

    //    SelectLayerButton: TOpenGL_Bevel;

    Procedure OnNewButtonClick(Sender: TObject);
    Procedure OnOpenButtonClick(Sender: TObject);
    Procedure OnSaveButtonClick(Sender: TObject);
    Procedure OnSaveAsButtonClick(Sender: TObject);
    Procedure OnExitButtonClick(Sender: TObject);
    Procedure OnGridButtonClick(Sender: TObject);
    Procedure OnZoomOutButtonClick(Sender: TObject);
    Procedure OnZoomInButtonClick(Sender: TObject);
    Procedure OnOptionsButtonClick(Sender: TObject);
    Procedure OnUndoButtonClick(Sender: TObject);

    Procedure OnSelectButtonClick(Sender: TObject);
    Procedure OnRotateCounterClockwise90ButtonClick(Sender: TObject);
    Procedure OnRotateClockwise90ButtonClick(Sender: TObject);
    Procedure OnRotate180ButtonClick(Sender: TObject);
    Procedure OnRotateAngleButtonClick(Sender: TObject);
    Procedure OnBrightenButtonClick(Sender: TObject);
    Procedure OnDarkenButtonClick(Sender: TObject);
    Procedure OnCurserSizeButtonClick(Sender: TObject);
    Procedure OnCursorShapeClick(Sender: TObject);
    Procedure OnEraserButtonClick(Sender: TObject);
    Procedure OnPencilButtonClick(Sender: TObject);
    Procedure OnLineButtonClick(Sender: TObject);
    Procedure OnCircleButtonClick(Sender: TObject);
    Procedure OnSquareButtonClick(Sender: TObject);
    Procedure OnOutlineButtonClick(Sender: TObject);
    Procedure OnMirrorButtonClick(Sender: TObject);
    Procedure OnMirrorModeButtonClick(Sender: TObject);

    Procedure OnFloodFillButtonClick(Sender: TObject);
    Procedure OnFloodFillModeButtonClick(Sender: TObject);
    Procedure OnColorPickButtonClick(Sender: TObject);

    Procedure OnSelectTransparentColorClick(Sender: TObject);
    Procedure OnColorClick(Sender: TObject);
    Procedure OnColorDblClick(Sender: TObject);
    Procedure OnColorMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    Procedure OnSelectLayerButtonClick(Sender: TObject);

    Procedure OpenGLControlKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure OpenGLControlKeyUp(Sender: TObject; Var Key: Word; Shift: TShiftState);

    Procedure OpenGLControlMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    Procedure OpenGLControlMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    Procedure OpenGLControlMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

    Procedure OpenGLControlMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; Var Handled: Boolean);
    Procedure OpenGLControlMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; Var Handled: Boolean);

    Procedure OnSaveColorPaletteButtonClick(Sender: TObject);
    Procedure OnLoadColorPaletteButtonClick(Sender: TObject);

    Procedure RenderGrid;
    Procedure RenderImage;
    Procedure RenderLCL;
    Procedure RenderCursor;

    Procedure NewImage(aWidth, aHeight: Integer);
    Procedure SetZoom(ZoomValue: integer);
    Procedure Zoom(ZoomIn: Boolean);

    Function getChanged: Boolean;
    Procedure LoadColorDialogColor(Sender: TObject);
    Function CursorToPixel(x, y: integer): TPoint;
    Function CursorIsInImageWindow: Boolean;
    Procedure SetLeftColor(Const c: TOpenGL_ColorBox);
    Procedure UpdateInfoLabel;
    Procedure SelectTool(aTool: TTool);
    Procedure LoadSettings;
    Procedure PasteImageFromClipboard;
    Procedure SaveImage(Const aFilename: String);
    Procedure CursorToPixelOperation(Callback: TPixelCallback);

    Procedure SetImagePixelByCursor(i, j: integer);
    Procedure SetOpenGLPixelByCursor(i, j: integer);
  public

    Property Changed: Boolean read getChanged;

    Constructor Create; virtual;
    Destructor Destroy; override;

    Procedure MakeCurrent(Owner: TOpenGLControl);

    Procedure Render();

    Procedure CheckScrollBorders;
    Procedure LoadImage(Const aFilename: String);
  End;

Var
  defcaption: String;

Implementation

Uses
  Forms, Dialogs, LCLType, math, Clipbrd, LCLIntf // LCL- Units
  , dglOpenGL // OpenGL Header
  , uOpenGL_ASCII_Font, uopengl_graphikengine // Corspan OpenGL-Engine
  , uvectormath // Math library
  , unit1 // Dialogs / Close
  , unit2 // Options
  , unit3 // Neu
  , unit4 // Export BMP Settings Dialog
  ;

{ TPixelEditor }

Procedure TPixelEditor.OnNewButtonClick(Sender: TObject);
Begin
  form3.Edit1.Text := inttostr(fImage.Width);
  form3.Edit2.Text := inttostr(fImage.Height);
  If form3.ShowModal = mrOK Then Begin
    NewImage(strtointdef(form3.Edit1.Text, fImage.Width), strtointdef(form3.Edit2.Text, fImage.Height));
  End;
End;

Function TPixelEditor.getChanged: Boolean;
Begin
  result := fImage.Changed;
End;

Procedure TPixelEditor.OnOpenButtonClick(Sender: TObject);
Begin
  If fImage.Changed Then Begin
    If ID_NO = Application.MessageBox('There are unsaved changes which will get lost. Do you really want to load without saving?', 'Question', MB_YESNO Or MB_ICONQUESTION) Then Begin
      exit
    End;
  End;
  If fImage.Filename <> '' Then Begin
    form1.OpenDialog1.InitialDir := ExtractFileDir(fImage.Filename);
  End;
  form1.OpenDialog1.DefaultExt := fSettings.DefaultExt;
  If Form1.OpenDialog1.Execute Then Begin
    fImage.Clear(); // Sicherstellen dass das Changed Flag zurück gesetzt ist.
    LoadImage(Form1.OpenDialog1.FileName);
  End;
End;

Procedure TPixelEditor.OnSaveButtonClick(Sender: TObject);
Begin
  If fImage.Filename = '' Then Begin
    OnSaveAsButtonClick(SaveAsButton);
  End
  Else Begin
    SaveImage(fImage.Filename);
  End;
End;

Procedure TPixelEditor.OnSaveAsButtonClick(Sender: TObject);
Begin
  If fImage.Filename <> '' Then Begin
    form1.SaveDialog1.InitialDir := ExtractFileDir(fImage.Filename);
  End;
  form1.SaveDialog1.DefaultExt := fSettings.DefaultExt;
  If form1.SaveDialog1.Execute Then Begin
    SaveImage(form1.SaveDialog1.Filename);
  End;
End;

Procedure TPixelEditor.OnExitButtonClick(Sender: TObject);
Begin
  form1.Close;
End;

Procedure TPixelEditor.OnGridButtonClick(Sender: TObject);
Begin
  // Der Gridbutton togglet eigentlich nur, den Rest macht ja Render :)
  If GridButton.Style = bsLowered Then Begin
    GridButton.Style := bsRaised;
  End
  Else Begin
    GridButton.Style := bsLowered;
  End;
End;

Procedure TPixelEditor.OnZoomOutButtonClick(Sender: TObject);
Begin
  Zoom(false);
End;

Procedure TPixelEditor.OnZoomInButtonClick(Sender: TObject);
Begin
  Zoom(true);
End;

Procedure TPixelEditor.OnOptionsButtonClick(Sender: TObject);
Begin
  // Settings to LCL
  form2.CheckBox1.Checked := fSettings.GridAboveImage;
  Case GetValue('DefaultExt', '.pe') Of
    '.pe': Form2.ComboBox1.ItemIndex := 0;
    '.bmp': Form2.ComboBox1.ItemIndex := 1;
    '.png': Form2.ComboBox1.ItemIndex := 2;
  End;
  form2.ShowModal;
  // LCL to .ini
  SetValue('GridAboveImage', inttostr(ord(Form2.CheckBox1.Checked)));
  Case Form2.ComboBox1.ItemIndex Of
    0: SetValue('DefaultExt', '.pe');
    1: SetValue('DefaultExt', '.bmp');
    2: SetValue('DefaultExt', '.png');
  End;
  LoadSettings;
End;

Procedure TPixelEditor.OnUndoButtonClick(Sender: TObject);
Begin
  fUndo.PopRecording(fImage);
  UpdateInfoLabel;
End;

Procedure TPixelEditor.OnSelectButtonClick(Sender: TObject);
Begin
  SelectTool(tSelect);
End;

Procedure TPixelEditor.OnRotateCounterClockwise90ButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnRotateClockwise90ButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnRotate180ButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnRotateAngleButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnBrightenButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnDarkenButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnCurserSizeButtonClick(Sender: TObject);
Begin
  CurserSize1.Style := ifthen(sender = CurserSize1, bsRaised, bsLowered);
  CurserSize2.Style := ifthen(sender = CurserSize2, bsRaised, bsLowered);
  CurserSize3.Style := ifthen(sender = CurserSize3, bsRaised, bsLowered);
  CurserSize4.Style := ifthen(sender = CurserSize4, bsRaised, bsLowered);
  fCursor.Compact.Size := ifthen(sender = CurserSize1, cs1_1, fCursor.Compact.Size);
  fCursor.Compact.Size := ifthen(sender = CurserSize2, cs3_3, fCursor.Compact.Size);
  fCursor.Compact.Size := ifthen(sender = CurserSize3, cs5_5, fCursor.Compact.Size);
  fCursor.Compact.Size := ifthen(sender = CurserSize4, cs7_7, fCursor.Compact.Size);
End;

Procedure TPixelEditor.OnSelectLayerButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OpenGLControlKeyDown(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  // Global Hotkeys
  If (key = VK_N) And (ssCtrl In Shift) Then OnNewButtonClick(NewButton);
  If (key = VK_O) And (ssCtrl In Shift) Then OnOptionsButtonClick(OptionsButton);
  If (key = VK_S) And (ssCtrl In Shift) Then OnSaveButtonClick(SaveButton);
  If (key = VK_V) And (ssCtrl In Shift) Then PasteImageFromClipboard;
  fCursor.Shift := ssShift In Shift;
End;

Procedure TPixelEditor.OpenGLControlKeyUp(Sender: TObject; Var Key: Word;
  Shift: TShiftState);
Begin
  fCursor.Shift := ssShift In Shift;
End;

Procedure TPixelEditor.OpenGLControlMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Var
  c: TRGBA;
Begin
  If ColorPicDialog.Visible Then exit; // ColorPicDialog Modal emulieren ;)
  fScrollInfo.ScrollPos := point(x, y);
  fCursor.Compact.PixelPos := CursorToPixel(x, y);
  fCursor.Pos := point(x, y);
  fCursor.PixelDownPos := fCursor.Compact.PixelPos;
  fCursor.LeftMouseButton := ssleft In Shift;
  fCursor.RightMouseButton := ssRight In Shift;
  If ssLeft In shift Then Begin
    If (CursorIsInImageWindow()) And (Not ColorPicDialog.Visible) Then Begin
      fUndo.StartNewRecording;
      Case fCursor.Tool Of
        tPen, tEraser, tMirror: Begin
            CursorToPixelOperation(@SetImagePixelByCursor);
          End;
        tPincette: Begin
            c := fImage.GetColorAt(fCursor.Compact.PixelPos.X, fCursor.Compact.PixelPos.y, fAktualLayer);
            fCursor.LeftColor.Color := c;
            SetLeftColor(fCursor.LeftColor);
          End;
      End;
    End;
  End;
  If ssRight In shift Then Begin
    If (CursorIsInImageWindow()) And (Not ColorPicDialog.Visible) Then Begin
      If fCursor.Tool = tMirror Then Begin
        fCursor.Origin := fCursor.Compact.PixelPos;
      End;
    End;
  End;
  UpdateInfoLabel();
End;

Procedure TPixelEditor.OpenGLControlMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
Var
  dx, dy: integer;
Begin
  If ColorPicDialog.Visible Then exit; // ColorPicDialog Modal emulieren ;)
  fCursor.Compact.PixelPos := CursorToPixel(x, y);
  fCursor.Pos := point(x, y);
  If ssLeft In shift Then Begin
    If (CursorIsInImageWindow()) And (Not ColorPicDialog.Visible) Then Begin
      If fCursor.Tool In [tPen, tEraser, tMirror] Then Begin
        CursorToPixelOperation(@SetImagePixelByCursor);
      End;
    End;
  End;
  If ssRight In shift Then Begin
    dx := (fScrollInfo.ScrollPos.x - x) * ScreenWidth Div FOwner.Width;
    dy := (fScrollInfo.ScrollPos.y - y) * ScreenHeight Div FOwner.Height;
    fScrollInfo.GlobalXOffset := fScrollInfo.GlobalXOffset + dx;
    fScrollInfo.GlobalYOffset := fScrollInfo.GlobalyOffset + dy;
    CheckScrollBorders();
  End;
  fScrollInfo.ScrollPos := point(x, y);
  UpdateInfoLabel();
End;

Procedure TPixelEditor.OpenGLControlMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
Begin
  // Den Dialog Schließen, wenn der User Außerhalb clickt ..
  If ColorPicDialog.Visible Then Begin
    ColorPicDialog.Visible := false;
    exit;
  End;
  fCursor.Compact.PixelPos := CursorToPixel(x, y);
  fCursor.Pos := Point(x, y);
  If (button = mbLeft) And (CursorIsInImageWindow()) And (Not ColorPicDialog.Visible) Then Begin
    Case fCursor.Tool Of
      tEraser, tPen, tMirror,
        tLine, tEllipse, tBucket,
        tRectangle: Begin
          CursorToPixelOperation(@SetImagePixelByCursor);
          fundo.PushRecording;
        End;
      tPincette: Begin
          If fCursor.LastTool = tEraser Then Begin
            fCursor.LastTool := tPen;
          End;
          SelectTool(fCursor.LastTool);
        End;
    End;
  End;
  fCursor.PixelDownPos := point(-1, -1);
  fCursor.LeftMouseButton := ssleft In Shift;
  fCursor.RightMouseButton := ssRight In Shift;
End;

Procedure TPixelEditor.OpenGLControlMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; Var Handled: Boolean);
Begin
  Zoom(true);
End;

Procedure TPixelEditor.OpenGLControlMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; Var Handled: Boolean);
Begin
  Zoom(false);
End;

Procedure TPixelEditor.OnSaveColorPaletteButtonClick(Sender: TObject);
Var
  m: TMemoryStream;
Begin
  If form1.SaveDialog2.Execute Then Begin
    m := TMemoryStream.Create;
    m.Write(ColorPaletteFileversion, sizeof(ColorPaletteFileversion));
    m.Write(Color1.Color, sizeof(Color1.Color));
    m.Write(Color2.Color, sizeof(Color2.Color));
    m.Write(Color3.Color, sizeof(Color3.Color));
    m.Write(Color4.Color, sizeof(Color4.Color));
    m.Write(Color5.Color, sizeof(Color5.Color));
    m.Write(Color6.Color, sizeof(Color6.Color));
    m.Write(Color7.Color, sizeof(Color7.Color));
    m.Write(Color8.Color, sizeof(Color8.Color));
    m.SaveToFile(form1.SaveDialog2.FileName);
    m.free;
  End;
End;

Procedure TPixelEditor.OnLoadColorPaletteButtonClick(Sender: TObject);
Var
  m: TMemoryStream;
  FileVersion: integer;
  c: TRGBA;
Begin
  If form1.OpenDialog2.Execute Then Begin
    m := TMemoryStream.Create;
    m.LoadFromFile(form1.OpenDialog2.FileName);
    FileVersion := -1;
    m.Read(FileVersion, SizeOf(FileVersion));
    If FileVersion > ColorPaletteFileversion Then Begin
      showmessage('Error, invalid file version.');
      m.free;
      exit;
    End;
    c := rgba(0, 0, 0, 0);
    m.Read(c, sizeof(C));
    Color1.Color := c;
    m.Read(c, sizeof(C));
    Color2.Color := c;
    m.Read(c, sizeof(C));
    Color3.Color := c;
    m.Read(c, sizeof(C));
    Color4.Color := c;
    m.Read(c, sizeof(C));
    Color5.Color := c;
    m.Read(c, sizeof(C));
    Color6.Color := c;
    m.Read(c, sizeof(C));
    Color7.Color := c;
    m.Read(c, sizeof(C));
    Color8.Color := c;
    SetLeftColor(ColorPicDialog.Shower);
    ColorPicDialog.Visible := false;
    m.free;
  End;
End;

Procedure TPixelEditor.RenderGrid;
Var
  zf: Single;
  i: Integer;
Begin
  // Der Generelle Hintergrund
  glPushMatrix;
  glTranslatef(0, 0, LayerBackGroundColor);
  glBindTexture(GL_TEXTURE_2D, 0);
  glColor3ub(51, 51, 51);
  glbegin(GL_QUADS);
  glVertex2f(0, 0);
  glVertex2f(640, 0);
  glVertex2f(640, 480);
  glVertex2f(0, 480);
  glEnd;
  glPopMatrix;
  zf := (fZoom / 100);
  // Der Rahmen um die Graphik für "niedrige" Zoom stufen
  glPushMatrix;
  glTranslatef(WindowLeft - fScrollInfo.GlobalXOffset, WindowTop - fScrollInfo.GlobalYOffset, 0); // Anfahren obere Linke Ecke
  // Verzerrung Raus Rechnen
  glScalef(ScreenWidth / FOwner.Width, ScreenHeight / FOwner.Height, 1);
  If fSettings.GridAboveImage Then Begin
    glTranslatef(0, 0, LayerForeGroundGrid);
  End
  Else Begin
    glTranslatef(0, 0, LayerBackGroundGrid);
  End;
  glColor3ub(102, 102, 102);
  glLineWidth(1);
  glBegin(GL_LINES);
  glVertex2f(0, 0);
  glVertex2f(0, fImage.Height * zf);
  glend;
  glBegin(GL_LINES);
  glVertex2f(fImage.Width * zf, 0);
  glVertex2f(fImage.Width * zf, fImage.Height * zf);
  glVertex2f(0, fImage.Height * zf);
  glVertex2f(fImage.Width * zf, fImage.Height * zf);
  glend;
  // Kein Grid gewünscht / Sinnvoll
  If (GridButton.Style = bsLowered) Or (fZoom <= 100) Then Begin
    glPopMatrix;
    exit;
  End;
  For i := 0 To max(fImage.Width, fImage.Height) Do Begin
    If i Mod 5 = 0 Then glLineWidth(2);
    glBegin(GL_LINES);
    If i <= fImage.Width Then Begin
      glVertex2f(i * zf, 0);
      glVertex2f(i * zf, fImage.Height * zf);
    End;
    If i <= fImage.Height Then Begin
      glVertex2f(0, i * zf);
      glVertex2f(fImage.Width * zf, i * zf);
    End;
    glend;
    glLineWidth(1);
  End;
  glPopMatrix;
End;

Procedure TPixelEditor.RenderImage;
Begin
  glPushMatrix;
  glTranslatef(WindowLeft - fScrollInfo.GlobalXOffset, WindowTop - fScrollInfo.GlobalYOffset, LayerImage); // Anfahren der Linken Oberen Ecke
  glColor4f(1, 1, 1, 1);
  // Zoom und Verzerrung rausrechnen
  glScalef(fZoom / 100 * ScreenWidth / FOwner.Width, fZoom / 100 * ScreenHeight / FOwner.Height, 1);
  fImage.Render();
  glPopMatrix;
End;

Procedure TPixelEditor.OnEraserButtonClick(Sender: TObject);
Begin
  If EraserButton.Style = bsRaised Then Begin
    SelectTool(tPen);
  End
  Else Begin
    SelectTool(tEraser);
  End;
End;

Procedure TPixelEditor.OnPencilButtonClick(Sender: TObject);
Begin
  SelectTool(TPen);
End;

Procedure TPixelEditor.OnCursorShapeClick(Sender: TObject);
Begin
  CursorRoundShape1.Style := ifthen(sender = CursorRoundShape1, bsRaised, bsLowered);
  CursorRoundShape2.Style := ifthen(sender = CursorRoundShape2, bsRaised, bsLowered);
  CursorRoundShape3.Style := ifthen(sender = CursorRoundShape3, bsRaised, bsLowered);
  CursorSquareShape1.Style := ifthen(sender = CursorSquareShape1, bsRaised, bsLowered);
  CursorSquareShape2.Style := ifthen(sender = CursorSquareShape2, bsRaised, bsLowered);
  CursorSquareShape3.Style := ifthen(sender = CursorSquareShape3, bsRaised, bsLowered);
  fCursor.Compact.Shape := ifthen(sender = CursorRoundShape1, csDot, fCursor.Compact.Shape);
  fCursor.Compact.Shape := ifthen(sender = CursorRoundShape2, csSmallPoint, fCursor.Compact.Shape);
  fCursor.Compact.Shape := ifthen(sender = CursorRoundShape3, csBigPoint, fCursor.Compact.Shape);
  fCursor.Compact.Shape := ifthen(sender = CursorSquareShape1, csSmallQuad, fCursor.Compact.Shape);
  fCursor.Compact.Shape := ifthen(sender = CursorSquareShape2, csQuad, fCursor.Compact.Shape);
  fCursor.Compact.Shape := ifthen(sender = CursorSquareShape3, csBigQuad, fCursor.Compact.Shape);
End;

Procedure TPixelEditor.OnLineButtonClick(Sender: TObject);
Begin
  SelectTool(tLine);
End;

Procedure TPixelEditor.OnCircleButtonClick(Sender: TObject);
Begin
  SelectTool(tEllipse);
End;

Procedure TPixelEditor.OnSquareButtonClick(Sender: TObject);
Begin
  SelectTool(tRectangle);
End;

Procedure TPixelEditor.OnOutlineButtonClick(Sender: TObject);
Begin
  OutlineButton.Style := ifthen(sender = OutlineButton, bsRaised, bsLowered);
  FilledButton.Style := ifthen(sender = FilledButton, bsRaised, bsLowered);
  fCursor.Outline := sender = OutlineButton;
End;

Procedure TPixelEditor.OnMirrorButtonClick(Sender: TObject);
Begin
  If MirrorButton.Style = bsRaised Then Begin
    SelectTool(tPen);
  End
  Else Begin
    SelectTool(tMirror);
  End;
End;

Procedure TPixelEditor.OnMirrorModeButtonClick(Sender: TObject);
Begin
  Mirror4Button.Style := ifthen(sender = Mirror4Button, bsRaised, bsLowered);
  MirrorVertButton.Style := ifthen(sender = MirrorVertButton, bsRaised, bsLowered);
  MirrorHorButton.Style := ifthen(sender = MirrorHorButton, bsRaised, bsLowered);
End;

Procedure TPixelEditor.OnFloodFillButtonClick(Sender: TObject);
Begin
  SelectTool(tBucket);
End;

Procedure TPixelEditor.OnFloodFillModeButtonClick(Sender: TObject);
Begin

End;

Procedure TPixelEditor.OnColorPickButtonClick(Sender: TObject);
Begin
  SelectTool(tPincette);
End;

Procedure TPixelEditor.OnSelectTransparentColorClick(Sender: TObject);
Begin
  ColorPicDialog.Visible := false;
  SetLeftColor(ColorTransparent);
End;


Procedure TPixelEditor.LoadColorDialogColor(Sender: TObject);
Begin
  If sender = Color1 Then ColorPicDialog.SelectorPos := 0;
  If sender = Color2 Then ColorPicDialog.SelectorPos := 1;
  If sender = Color3 Then ColorPicDialog.SelectorPos := 2;
  If sender = Color4 Then ColorPicDialog.SelectorPos := 3;
  If sender = Color5 Then ColorPicDialog.SelectorPos := 4;
  If sender = Color6 Then ColorPicDialog.SelectorPos := 5;
  If sender = Color7 Then ColorPicDialog.SelectorPos := 6;
  If sender = Color8 Then ColorPicDialog.SelectorPos := 7;
  ColorPicDialog.LoadColor(sender As TOpenGL_ColorBox);
End;

Procedure TPixelEditor.OnColorClick(Sender: TObject);
Begin
  // Der User schaltet direkt die Colorpicbox um
  If ColorPicDialog.Visible And (ColorPicDialog.Shower <> Sender) Then Begin
    SetLeftColor(sender As TOpenGL_ColorBox);
    LoadColorDialogColor(sender);
    exit;
  End;
  If fCursor.LeftColor = sender Then Begin
    ColorPicDialog.Visible := Not ColorPicDialog.Visible;
    If ColorPicDialog.Visible Then LoadColorDialogColor(sender);
  End
  Else Begin
    SetLeftColor(sender As TOpenGL_ColorBox);
  End;
End;

Procedure TPixelEditor.OnColorDblClick(Sender: TObject);
Begin
  LoadColorDialogColor(sender);
  ColorPicDialog.Visible := true;
End;

Procedure TPixelEditor.OnColorMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
Var
  c: TRGBA;
Begin
  If button = mbRight Then Begin
    ColorPicDialog.Visible := false;
    c := (sender As TOpenGL_ColorBox).Color;
    ColorPreview.BackColor := c;
    fCursor.RightColor := c;
  End;
End;

Procedure TPixelEditor.RenderLCL;
Var
  i: integer;
Begin
  // 1. Rendern des Grauen Hintergrunds
  glBindTexture(GL_TEXTURE_2D, 0);
  glPushMatrix;
  glTranslatef(0, 0, LayerFormColor);
  glColor3ub($80, $80, $80);
  glBegin(GL_QUADS);
  // Oben Toolbar
  glVertex2f(-1, -1);
  glVertex2f(-1, 38);
  glVertex2f(641, 38);
  glVertex2f(641, -1);
  // Links Toolbar
  glVertex2f(-1, -1);
  glVertex2f(-1, 481);
  glVertex2f(75, 481);
  glVertex2f(75, -1);
  // Rechts
  glVertex2f(640 - 3, -1);
  glVertex2f(640 - 3, 480);
  glVertex2f(641, 480);
  glVertex2f(641, -1);
  // Unten Toolbar
  glVertex2f(-1, 425);
  glVertex2f(-1, 481);
  glVertex2f(641, 481);
  glVertex2f(641, 425);
  glend();
  glPopMatrix;

  // 2. Rendern der Eigentlichen LCL Elemente
  glPushMatrix;
  glTranslatef(0, 0, LayerLCL);
  For i := 0 To high(FElements) Do Begin
    FElements[i].Render();
  End;
  glPopMatrix;
End;

Procedure TPixelEditor.CursorToPixelOperation(Callback: TPixelCallback);
Var
  p: TPoint;
  Dummy: TCompactCursor;
  off, i, j: Integer;
Begin
  (*
   * Die nachfolgenden Algorithmen benötigen sehr häufig, dass man sich auf den "Ursprung" Bezieht
   * -> aus diesem Grund wird ein "Künstlicher" Cursor verwendet der immer im Ursprung sitzt
   *    beim Aufruf der Callback, wird dann die Aptuelle Position als Offset wieder mit rein gerechnet.
   *)
  Case fCursor.Tool Of
    tPincette: Callback(fCursor.Compact.PixelPos.X, fCursor.Compact.PixelPos.y);
    teraser, tPen: FoldCursorOnPixel(fCursor.Compact, Callback);
    tLine: Begin
        If (fCursor.PixelDownPos.x <> -1) And fCursor.LeftMouseButton Then Begin
          // DownPos und Aktuelle Position müssen für die "Projektion" getauscht werden !
          Dummy := fCursor.Compact;
          dummy.PixelPos := fCursor.PixelDownPos;
          p := fCursor.Compact.PixelPos - fCursor.PixelDownPos;
          If fCursor.Shift Then Begin
            // Der Punkt kann irgendwo liegen, er soll aber so "Projiziert" werden, dass er auf einen der 2 Hauptdiagonel oder den 2 Koordinaten Achsen liegt
            p := MovePointToNextMainAxis(p);
          End;
          p := p + fCursor.PixelDownPos;
          Bresenham_Line(dummy, p, Callback);
        End
        Else Begin
          If fCursor.Compact.PixelPos.X <> -1 Then Begin
            FoldCursorOnPixel(fCursor.Compact, Callback);
          End;
        End;
      End;
    tEllipse: Begin
        If (fCursor.PixelDownPos.x <> -1) And fCursor.LeftMouseButton Then Begin
          Dummy := fCursor.Compact;
          dummy.PixelPos := fCursor.PixelDownPos;
          p := fCursor.Compact.PixelPos - fCursor.PixelDownPos;
          If fCursor.Shift Then Begin
            // Der Punkt kann irgendwo liegen, er soll aber so "Projiziert" werden, dass er auf einen der 2 Hauptdiagonel oder den 2 Koordinaten Achsen liegt
            p := AdjustToMaxAbsValue(p);
          End;
          p := p + fCursor.PixelDownPos;
          Bresenham_Ellipse(dummy, p, OutlineButton.Style = bsLowered, Callback);
        End
        Else Begin
          If fCursor.Compact.PixelPos.X <> -1 Then Begin
            FoldCursorOnPixel(fCursor.Compact, Callback);
          End;
        End;
      End;
    tRectangle: Begin
        If (fCursor.PixelDownPos.x <> -1) And fCursor.LeftMouseButton Then Begin
          Dummy := fCursor.Compact;
          dummy.PixelPos := fCursor.PixelDownPos;
          p := fCursor.Compact.PixelPos - fCursor.PixelDownPos;
          If fCursor.Shift Then Begin
            // Der Punkt kann irgendwo liegen, er soll aber so "Projiziert" werden, dass er auf einen der 2 Hauptdiagonel oder den 2 Koordinaten Achsen liegt
            p := AdjustToMaxAbsValue(p);
          End;
          p := p + fCursor.PixelDownPos;
          If OutlineButton.Style = bsLowered Then Begin
            For i := min(p.X, dummy.PixelPos.x) To max(p.X, dummy.PixelPos.x) Do Begin
              For j := min(p.Y, dummy.PixelPos.y) To max(p.Y, dummy.PixelPos.y) Do Begin
                Callback(i, j);
              End;
            End;
          End
          Else Begin
            // TODO: bei negativ ist da ein off by one drin
            RectangleOutline(dummy, p, Callback);
          End;
        End
        Else Begin
          If fCursor.Compact.PixelPos.X <> -1 Then Begin
            FoldCursorOnPixel(fCursor.Compact, Callback);
          End;
        End;
      End;
    tMirror: Begin
        If (fCursor.Compact.PixelPos.x <> -1) Then Begin
          off := 0;
          // Dadurch, dass die Cursor selbst ja nicht Mittelpunktsymetrisch sind, müssen manche Kombinationen noch mal Extra "verschoben" werden
          If (CursorRoundShape2.Style = bsRaised)
            Or (CursorRoundShape3.Style = bsRaised)
            Or (CursorSquareShape1.Style = bsRaised)
            Then Begin
            off := 1;
          End;
          Mirror(fCursor.Compact, fCursor.Origin, MirrorCenterButton.Style = bsRaised,
            (Mirror4Button.Style = bsRaised) Or (MirrorHorButton.Style = bsRaised),
            (Mirror4Button.Style = bsRaised) Or (MirrorVertButton.Style = bsRaised), off, Callback);
        End;
      End;
    tBucket: Begin
        upixeleditor_types.FloodFill(
          fImage.GetColorAt(fCursor.Compact.PixelPos.X, fCursor.Compact.PixelPos.y, fAktualLayer),
          point(fCursor.Compact.PixelPos.X, fCursor.Compact.PixelPos.y),
          0, // TODO: Abweichung in Prozent muss noch implementiert werden !
          fAktualLayer,
          fimage,
          Callback);
      End;
  End;
End;

Procedure TPixelEditor.SetImagePixelByCursor(i, j: integer);
Var
  c: TRGBA;
Begin
  c := fImage.GetColorAt(i, j, fAktualLayer);
  If EraserButton.Style = bsRaised Then Begin
    If c <> upixeleditor_types.ColorTransparent Then
      fUndo.RecordPixelChange(i, j, c);
    fImage.SetColorAt(i, j, fAktualLayer, upixeleditor_types.ColorTransparent);
  End
  Else Begin
    If c <> fCursor.LeftColor.Color Then
      fUndo.RecordPixelChange(i, j, c);
    fImage.SetColorAt(i, j, fAktualLayer, fCursor.LeftColor.Color);
  End;
End;

Procedure TPixelEditor.SetOpenGLPixelByCursor(i, j: integer);
Begin
  If (i >= 0) And (i < fImage.Width) And
    (j >= 0) And (j < fImage.Height) Then Begin
    glVertex2f(i, j);
  End;
End;

Procedure TPixelEditor.RenderCursor;
Var
  off: Single;
  c: TRGBA;
Begin
  If Not CursorIsInImageWindow Then exit;
  glPushMatrix;
  glTranslatef(WindowLeft - fScrollInfo.GlobalXOffset, WindowTop - fScrollInfo.GlobalYOffset, LayerCursor); // Anfahren der Linken Oberen Ecke
  glColor4f(1, 1, 1, 1);
  glBindTexture(GL_TEXTURE_2D, 0);
  // Zoom und Verzerrung rausrechnen so dass 1 OpenGL Pixel = 1 Bild Pixel ist
  glScalef(fZoom / 100 * ScreenWidth / FOwner.Width, fZoom / 100 * ScreenHeight / FOwner.Height, 1);
  // Anfahren des Cursor Mittelpunkts
  glTranslatef(0.5, 0.5, 0);
  glPointSize(fZoom / 100);
  c := fCursor.LeftColor.Color;
  glColor3ub(c.r, c.g, c.b);
  glBegin(GL_POINTS);
  CursorToPixelOperation(@SetOpenGLPixelByCursor);
  glEnd;
  glPointSize(1);
  // Der Mirror Cursor muss noch die "Achsen" einmalen
  If fCursor.Tool = tMirror Then Begin
    glColor3ub(255, 255, 0);
    glBegin(GL_LINES);
    If MirrorCenterButton.style = bsLowered Then Begin
      off := 0;
    End
    Else Begin
      off := 0.5;
    End;
    If (Mirror4Button.Style = bsRaised) Or
      (MirrorVertButton.Style = bsRaised) Then Begin
      glVertex2f(fCursor.Origin.X - off, -0.5);
      glVertex2f(fCursor.Origin.X - off, fImage.Height - 0.5);
    End;
    If (Mirror4Button.Style = bsRaised) Or
      (MirrorHorButton.Style = bsRaised) Then Begin
      glVertex2f(-0.5, fCursor.Origin.Y - off);
      glVertex2f(fImage.Width - 0.5, fCursor.Origin.Y - off);
    End;
    glEnd;
  End;
  glPopMatrix;
End;

Procedure TPixelEditor.NewImage(aWidth, aHeight: Integer);
Begin
  // Reset aller Curser
  fUndo.Clear;
  SetZoom(1000);
  fImage.SetSize(aWidth, aHeight);
  fAktualLayer := lMiddle;
  fScrollInfo.GlobalXOffset := 0;
  fScrollInfo.GlobalYOffset := 0;
  ColorPicDialog.Visible := false;

  fCursor.compact.PixelPos.x := -1;
  fCursor.PixelDownPos.x := -1;
  fCursor.Pos.x := -1;
  fCursor.Shift := false;
  fCursor.LeftMouseButton := false;
  fCursor.RightMouseButton := false;
  fCursor.Origin.X := aWidth Div 2;
  fCursor.Origin.Y := aHeight Div 2;

  CheckScrollBorders();
  UpdateInfoLabel();
End;

Procedure TPixelEditor.SetZoom(ZoomValue: integer);
Begin
  fZoom := ZoomValue;
  ZoomInfoTextbox.Caption := inttostr(ZoomValue) + '%';
End;

Procedure TPixelEditor.Zoom(ZoomIn: Boolean);
Var
  i: integer;
  p1, p2: TPoint;
Begin
  // Store the old position
  p1 := CursorToPixel(fCursor.Pos.X, fCursor.Pos.Y);
  // Do the Zoom
  If ZoomIn Then Begin
    For i := 0 To high(ZoomLevels) Do Begin
      If fZoom = ZoomLevels[i] Then Begin
        SetZoom(ZoomLevels[max(0, i - 1)]);
        break;
      End;
    End;
  End
  Else Begin
    For i := 0 To high(ZoomLevels) Do Begin
      If fZoom = ZoomLevels[i] Then Begin
        SetZoom(ZoomLevels[min(high(ZoomLevels), i + 1)]);
        break;
      End;
    End;
  End;
  // Calc the new "wrong" position
  p2 := CursorToPixel(fCursor.Pos.X, fCursor.Pos.y);
  // "Scroll" so that the new position is the old one ;)
  fScrollInfo.GlobalXOffset := fScrollInfo.GlobalXOffset + (p1.x - p2.x) * fZoom Div 100;
  fScrollInfo.GlobalyOffset := fScrollInfo.GlobalyOffset + (p1.Y - p2.Y) * fZoom Div 100;
  // Let the scrollbars do their constraint thing
  // Nachziehen des Cursors sonst springt der beim Zoomen
  fCursor.compact.PixelPos := CursorToPixel(fCursor.Pos.x, fCursor.Pos.y);
  CheckScrollBorders();
  UpdateInfoLabel;
End;

Function TPixelEditor.CursorToPixel(x, y: integer): TPoint;
Var
  rx, ry: Single;
  riy, rix: Integer;
Begin
  result := point(-1, -1);
  rx := x;
  ry := y;
  // 1. Scrolling Raus Rechnen
  rx := rx + fScrollInfo.GlobalXOffset * FOwner.Width Div ScreenWidth;
  ry := ry + fScrollInfo.GlobalYOffset * FOwner.Height Div ScreenHeight;
  // 2. Translation auf 0 / 0
  rx := rx - (WindowLeft * FOwner.Width / ScreenWidth);
  ry := ry - (WindowTop * FOwner.Height / ScreenHeight);
  // 3. Berücksichtigen des Zooms
  rx := rx * 100 / fZoom;
  ry := ry * 100 / fZoom;
  // 4. Anpassen Pixel Mittelpunkt
  rx := rx - 0.5;
  ry := ry - 0.5;
  // 5. Limitieren auf die Image Größe
  rix := round(rx);
  riy := round(ry);
  If (rix >= 0) And (rix < fImage.Width) And
    (riy >= 0) And (riy < fImage.Height) Then Begin
    result := point(rix, riy);
  End;
End;

Function TPixelEditor.CursorIsInImageWindow: Boolean;
Begin
  result := Not ((fCursor.compact.PixelPos.x = -1) Or // Braucht es eigentlich nicht, aber schaden tut's auch nicht ..
    (fCursor.Pos.x < WindowLeft * FOwner.Width / ScreenWidth) Or
    (fCursor.Pos.y < WindowTop * fowner.Height / ScreenHeight) Or
    (fCursor.Pos.x - WindowLeft * FOwner.Width / ScreenWidth >= fImage.Width * fZoom Div 100) Or
    (fCursor.Pos.y - WindowTop * fowner.Height / ScreenHeight >= fImage.Height * fZoom Div 100) Or
    (fCursor.Pos.x > FOwner.Width - FOwner.Width * (ScreenWidth - WindowRight + 1) / ScreenWidth) Or
    (fCursor.Pos.y > FOwner.Height - FOwner.Height * (ScreenHeight - WindowBottom + 1) / ScreenHeight));
End;

Procedure TPixelEditor.SetLeftColor(Const c: TOpenGL_ColorBox);
Begin
  ColorPreview.FrontColor := c.Color;
  fCursor.LeftColor := c;
  If c.Color.a = 0 Then Begin
    AktColorInfoLabel.caption := format('%d/%d/%d', [c.color.r, c.color.g, c.color.b]);
  End
  Else Begin
    AktColorInfoLabel.caption := '';
  End;
End;

Procedure TPixelEditor.UpdateInfoLabel;
Var
  c: TRGBA;
Begin
  If Not CursorIsInImageWindow() Then Begin
    InfoLabel.caption := '';
    InfoDetailLabel.Caption := '';
  End
  Else Begin
    c := fImage.GetColorAt(fCursor.compact.PixelPos.x, fCursor.compact.PixelPos.y, fAktualLayer);
    InfoLabel.caption := format('%d,%d', [fCursor.compact.PixelPos.x, fCursor.compact.PixelPos.y]);
    If c.a = 0 Then Begin
      InfoLabel.caption := InfoLabel.caption + LineEnding + format('%d/%d/%d', [c.r, c.g, c.b]);
    End;
    // TODO: InfoDetailLabel.Caption befüllen
  End;
End;

Procedure TPixelEditor.SelectTool(aTool: TTool);
Const
  PenTools = [tEraser, tPen, tLine, tEllipse, tRectangle, tMirror];
Begin
  ColorPicDialog.Visible := false;
  SelectButton.Style := ifThen(atool = tSelect, bsRaised, bsLowered);
  SelectModeButton.Visible := atool = tSelect;
  RotateCounterClockwise90.Visible := atool = tSelect;
  RotateClockwise90.Visible := atool = tSelect;
  Rotate180.Visible := atool = tSelect;
  RotateAngle.Visible := atool = tSelect;

  BrightenButton.Style := ifThen(atool = tBrighten, bsRaised, bsLowered);
  DarkenButton.Style := ifThen(atool = tDarken, bsRaised, bsLowered);

  CurserSize1.Visible := atool In PenTools;
  CurserSize2.Visible := atool In PenTools;
  CurserSize3.Visible := atool In PenTools;
  CurserSize4.Visible := atool In PenTools;

  EraserButton.Style := ifThen(atool = tEraser, bsRaised, bsLowered);
  PencilButton.Style := ifThen(atool In [TPen, tMirror, tEraser], bsRaised, bsLowered);
  LineButton.Style := ifThen(atool = tLine, bsRaised, bsLowered);
  CircleButton.Style := ifThen(atool = tEllipse, bsRaised, bsLowered);
  SquareButton.Style := ifThen(atool = tRectangle, bsRaised, bsLowered);
  MirrorButton.Style := ifThen(atool = tMirror, bsRaised, bsLowered);
  CursorRoundShape1.Visible := atool In PenTools;
  CursorRoundShape2.Visible := atool In PenTools;
  CursorRoundShape3.Visible := atool In PenTools;
  CursorSquareShape1.Visible := atool In PenTools;
  CursorSquareShape2.Visible := atool In PenTools;
  CursorSquareShape3.Visible := atool In PenTools;
  OutlineButton.Visible := aTool In [tEllipse, tRectangle];
  FilledButton.Visible := aTool In [tEllipse, tRectangle];
  MirrorCenterButton.Visible := aTool = tMirror;
  MirrorHorButton.Visible := aTool = tMirror;
  MirrorVertButton.Visible := aTool = tMirror;
  Mirror4Button.Visible := aTool = tMirror;

  FloodFillButton.Style := ifThen(atool = tBucket, bsRaised, bsLowered);
  FloodFillModeButton.Visible := aTool = tBucket;

  ColorPickButton.Style := ifThen(atool = tPincette, bsRaised, bsLowered);
  // Übernehmen des Cursor Tools ;)
  fCursor.LastTool := fCursor.Tool;
  fCursor.Tool := aTool;
End;

Procedure TPixelEditor.CheckScrollBorders;
Var
  z, WindowWidthInPixel, WindowHeightInPixel: integer;
Begin
  z := fZoom Div 100;
  fScrollInfo.GlobalXOffset := max(fScrollInfo.GlobalXOffset, 0);
  fScrollInfo.GlobalYOffset := max(fScrollInfo.GlobalYOffset, 0);
  WindowWidthInPixel := (ScreenWidth - WindowLeft - (ScreenWidth - WindowRight)) * FOwner.Width Div ScreenWidth;
  WindowHeightInPixel := (ScreenHeight - WindowTop - (ScreenHeight - WindowBottom)) * FOwner.Height Div ScreenHeight;
  If (fScrollInfo.GlobalXOffset) * FOwner.Width / ScreenWidth > fImage.width * z - WindowWidthInPixel Then Begin
    fScrollInfo.GlobalXOffset := max(0, (fImage.width * z - WindowWidthInPixel) * ScreenWidth Div FOwner.Width);
  End;
  If (fScrollInfo.GlobalyOffset) * FOwner.Height / ScreenHeight > fImage.Height * z - WindowHeightInPixel Then Begin
    fScrollInfo.GlobalyOffset := max(0, (fImage.Height * z - WindowHeightInPixel) * ScreenHeight Div FOwner.Height);
  End;
End;

Procedure TPixelEditor.LoadSettings;
Begin
  fSettings.GridAboveImage := GetValue('GridAboveImage', '1') = '1';
  fSettings.DefaultExt := GetValue('DefaultExt', '.pe');
End;

Procedure TPixelEditor.PasteImageFromClipboard;
Var
  b: Tbitmap;
  i, j: Integer;
  fn: String;
  c: TRGBA;
Begin
  If Clipboard.HasFormat(PredefinedClipboardFormat(pcfBitmap)) Then Begin
    b := TBitmap.Create;
    b.LoadFromClipboardFormat(PredefinedClipboardFormat(pcfBitmap));
    // TODO: Paste secondary Color as Transparent ..
    // TODO: Remove, only for testing, paste content from Clipboard *g*
    fn := fImage.Filename;
    fImage.SetSize(b.Width, b.Height);
    Fimage.Filename := fn;
    For i := 0 To b.Width - 1 Do Begin
      For j := 0 To b.Height - 1 Do Begin
        // TODO: Nur machen, wenn der SelectMode auf Transparent gestellt ist
        c := ColorToRGBA(b.Canvas.Pixels[i, j], 0);
        If (c.r = fCursor.RightColor.r) And
          (c.g = fCursor.RightColor.g) And
          (c.b = fCursor.RightColor.b) Then Begin
          c := upixeleditor_types.ColorTransparent;
        End;
        fImage.SetColorAt(i, j, fAktualLayer, c);
      End;
    End;
    b.free;
  End;
End;

Procedure TPixelEditor.SaveImage(Const aFilename: String);
Var
  m: TMemoryStream;
Begin
  Case LowerCase(ExtractFileExt(aFilename)) Of
    '.png': Begin
        fImage.ExportAsPNG(aFilename, fAktualLayer);
      End;
    '.bmp': Begin
        form4.Shape1.Brush.Color := clFuchsia;
        form4.caption := 'BMP export settings';
        If form4.ShowModal = mrOK Then Begin
          fImage.ExportAsBMP(aFilename, fAktualLayer, ColorToRGBA(form4.Shape1.Brush.Color));
        End
        Else Begin
          showmessage('Skip, nothing saved.');
        End;
      End;
    '.pe': Begin
        m := TMemoryStream.Create;
        m.Write(PixelEditorFileversion, sizeof(PixelEditorFileversion));
        m.Write(fAktualLayer, sizeof(TLayer));
        m.Write(Color1.Color, sizeof(Color1.Color));
        m.Write(Color2.Color, sizeof(Color2.Color));
        m.Write(Color3.Color, sizeof(Color3.Color));
        m.Write(Color4.Color, sizeof(Color4.Color));
        m.Write(Color5.Color, sizeof(Color5.Color));
        m.Write(Color6.Color, sizeof(Color6.Color));
        m.Write(Color7.Color, sizeof(Color7.Color));
        m.Write(Color8.Color, sizeof(Color8.Color));
        fImage.AppendToPEStream(m, aFilename);
        m.SaveToFile(aFilename);
        m.free;
      End;
  Else Begin
      showmessage('Error unknown fileextension "' + ExtractFileExt(aFilename) + '" nothing will be saved.');
      exit;
    End;
  End;
  form1.caption := defcaption + ', ' + ExtractFileName(aFilename);
  Application.Title := ExtractFileName(aFilename);
End;

Procedure TPixelEditor.LoadImage(Const aFilename: String);
Var
  m: TMemoryStream;
  LoadedFileVersion, i: Integer;
  c: TRGBA;
Begin
  If fImage.Changed Then Begin
    If ID_NO = Application.MessageBox('There are unsaved changes which will get lost. Do you really want to load without saving?', 'Question', MB_YESNO Or MB_ICONQUESTION) Then Begin
      exit
    End;
  End;
  Case LowerCase(ExtractFileExt(aFilename)) Of
    '.png': Begin
        fImage.ImportFromPNG(aFilename);
        fAktualLayer := lMiddle;
      End;
    '.bmp': Begin
        form4.Shape1.Brush.Color := clFuchsia;
        form4.caption := 'BMP import settings';
        If form4.ShowModal = mrOK Then Begin
          fImage.ImportFromBMP(aFilename, ColorToRGBA(form4.Shape1.Brush.Color));
          fAktualLayer := lMiddle;
        End
        Else Begin
          showmessage('Skip, nothing loaded.');
          exit;
        End;
      End;
    '.pe': Begin
        m := TMemoryStream.Create;
        m.LoadFromFile(aFilename);
        LoadedFileVersion := -1;
        m.Read(LoadedFileVersion, sizeof(i));
        // TODO: In Zukunft kann hier dann ein Fileversion angepasster Lader sein Werk tun ;)
        If LoadedFileVersion > PixelEditorFileversion Then Begin
          showmessage('Error, invalid file version.');
          m.free;
          exit;
        End;
        m.Read(fAktualLayer, SizeOf(fAktualLayer));
        If LoadedFileVersion >= 2 Then Begin
          c := RGBA(0, 0, 0, 255);
          m.Read(C, sizeof(C));
          color1.Color := c;
          m.Read(C, sizeof(C));
          color2.Color := c;
          m.Read(C, sizeof(C));
          color3.Color := c;
          m.Read(C, sizeof(C));
          color4.Color := c;
          m.Read(C, sizeof(C));
          color5.Color := c;
          m.Read(C, sizeof(C));
          color6.Color := c;
          m.Read(C, sizeof(C));
          color7.Color := c;
          m.Read(C, sizeof(C));
          color8.Color := c;
        End;
        fImage.LoadFromPEStream(m, aFilename, fAktualLayer);
        m.free;
      End;
  Else Begin
      showmessage('Error unknown fileextension "' + ExtractFileExt(aFilename) + '" nothing will be loaded.');
      exit;
    End;
  End;
  form1.caption := defcaption + ', ' + ExtractFileName(aFilename);
  Application.Title := ExtractFileName(aFilename);
  fScrollInfo.GlobalXOffset := 0;
  fScrollInfo.GlobalYOffset := 0;
  CheckScrollBorders;
  fUndo.Clear;
  UpdateInfoLabel;
End;

Constructor TPixelEditor.Create;
Begin
  Inherited Create;
  fImage := TImage.Create();
  fUndo := TUndoEngine.Create();
End;

Destructor TPixelEditor.Destroy;
Var
  i: Integer;
Begin
  fUndo.free;
  fImage.Free;
  For i := 0 To high(FElements) Do Begin
    FElements[i].Free;
  End;
  setlength(FElements, 0);
End;

Procedure TPixelEditor.MakeCurrent(Owner: TOpenGLControl);
  Procedure AddElement(Const value: TOpenGL_BaseClass);
  Begin
    setlength(FElements, high(FElements) + 2);
    FElements[high(FElements)] := value;
  End;

Var
  image: Integer;
Begin

  FOwner := Owner;

  FElements := Nil;

  owner.OnMouseWheelDown := @OpenGLControlMouseWheelDown;
  owner.OnMouseWheelup := @OpenGLControlMouseWheelUp;
  owner.OnMouseDown := @OpenGLControlMouseDown;
  owner.OnMouseMove := @OpenGLControlMouseMove;
  owner.OnMouseUp := @OpenGLControlMouseUp;
  owner.OnKeyDown := @OpenGLControlKeyDown;
  owner.OnKeyUp := @OpenGLControlKeyUp;

{$I upixeleditor_constructor.inc}

  LoadSettings;

  //NewImage(128, 128);
  NewImage(32, 32);

  // Settings die nur 1 mal pro Programstart zurück gesetzt werden
  OnCurserSizeButtonClick(CurserSize1);
  OnCursorShapeClick(CursorRoundShape1);
  OnOutlineButtonClick(OutlineButton);
  OnMirrorModeButtonClick(MirrorVertButton);
  MirrorCenterButton.Style := bsRaised;
  GridButton.Style := bsRaised;
  OnColorClick(Color1);
  SelectTool(TPen);
  fCursor.RightColor := upixeleditor_types.ColorTransparent;
End;

Procedure TPixelEditor.Render;
Begin
  RenderGrid;
  RenderImage;
  RenderCursor;
  RenderLCL;
End;

End.

