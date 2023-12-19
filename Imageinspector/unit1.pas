(******************************************************************************)
(* Imageinspector                                                  ??.??.???? *)
(*                                                                            *)
(* Version     : 0.07                                                         *)
(*                                                                            *)
(* Author      : Uwe Schächterle (Corpsman)                                   *)
(*                                                                            *)
(* Support     : www.Corpsman.de                                              *)
(*                                                                            *)
(* Description : Image manipulation and measurement application               *)
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
(* Known Issues:                                                              *)
(*              -Beim Fill Rendering stimmen die Arc Kurven nicht mehr        *)
(*               gescheit, weil sie dann Eliptisch werden                     *)
(*              -Der Bildexport funktioniert nur für das Bild nicht für die   *)
(*               Measurement Objekte                                          *)
(*              -Die BrushColor ist beim TArc nicht Unsichtbar, obwohl so     *)
(*               eingestellt (Nur unter Linux ein Problem)                    *)
(*              -Die fMeasureElements Texte sind Tiefenabhängig, um das zu    *)
(*               fixen müssten die Texte in einem zweiten Rendering als       *)
(*               allerletztes gerendert werden.                               *)
(*                                                                            *)
(* History     : 0.01 (HP) - Initial version                                  *)
(*               0.02 (HP) - Einheit Editierbar gemacht                       *)
(*                           Fix Anchor Trenner "optionen" "Exit"             *)
(*                           Einheiten Sprachlich angepasst anzeigen          *)
(*               0.03 (HP) - Editierbares Textfeld via Popupmenu              *)
(*                           Undo für Eraser                                  *)
(*                           Fix Projekt Correction Algorithmus               *)
(*                           Fix Height Correction Algorithmus                *)
(*               0.04 (HP) - Zuschneiden Tool                                 *)
(*                           Fertigstellen Projection Correction Algorithmus  *)
(*                           Neuer Höhenkorrekturalgorithmus                  *)
(*               0.05      - Deaktivieren von Modiviern beim Laden eines      *)
(*                           Bildes.                                          *)
(*               0.06      - Einführen UndoStack für Measurement Elemente und *)
(*                           Image                                            *)
(*                           Fix Bug where Measurement Elements Texts are     *)
(*                           Editable                                         *)
(*               0.07      - Wenn die Größe drastisch verkleinert wurde, hat  *)
(*                           es die Metrik Messpunkte aus dem Bild            *)
(*                                                                            *)
(******************************************************************************)
  (*
   * Missing Features: -Changed ist noch nicht ordentlich drin
   *)
  (*
       Als nächstes

       - Code Formater im Skript Editor

       Manipulate:
       - Gamma Korrektur ?
       - HDR Korrektur ?

       Measurement:
       - Magic Wand

       - Bei den Elementen die in der Mitte noch keinen Knopf zum "Komplett" verschieben haben:
           wie bei Arrow "Unsichtbare" Knöpfe einbauen
   *)

Unit Unit1;

{$MODE objfpc}{$H+}

Interface

Uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, Buttons, Menus, Types, ueventer, uwidgets, uinterpreter;

Const
  (*
   * Historie: 1 =  Initialversion
   *           2 = ?
   *)
  ImageInspectorFileVersion: Int32 = 2;

  Panel_index_Filename = 0;
  Panel_index_Dimension = 1;
  Panel_index_Cursorpos = 2;
  Panel_index_RGB = 3;

  View_Index_Fill = 0; // Zoomstufe Bildfüllend
  //View_Index Fit = ?; Eine Kannte Fill die andere im Korrekten Aspektionsverhältnis
  View_Index_8_1 = 1; // 2:1
  View_Index_4_1 = 2; // 2:1
  View_Index_2_1 = 3; // 2:1
  View_Index_1_1 = 4; // 1:1
  View_Index_1_2 = 5; // 1:2
  View_Index_1_4 = 6; // 1:2
  View_Index_1_8 = 7; // 1:2

Type

  TFileType = (ftBMP, ftPNG, ftJPG, ftUnknown);

  TSkript = Record
    Enabled: Boolean;
    Filename: String;
    Interpreter: TInterpreter;
  End;

  { TForm1 }

  TForm1 = Class(TForm)
    Bevel1: TBevel;
    Bevel10: TBevel;
    Bevel11: TBevel;
    Bevel12: TBevel;
    Bevel13: TBevel;
    Bevel14: TBevel;
    Bevel15: TBevel;
    Bevel16: TBevel;
    Bevel17: TBevel;
    Bevel18: TBevel;
    Bevel19: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    Bevel4: TBevel;
    Bevel5: TBevel;
    Bevel6: TBevel;
    Bevel7: TBevel;
    Bevel8: TBevel;
    Bevel9: TBevel;
    BitBtn1: TBitBtn;
    BitBtn10: TBitBtn;
    BitBtn11: TBitBtn;
    BitBtn12: TBitBtn;
    BitBtn13: TBitBtn;
    BitBtn14: TBitBtn;
    BitBtn15: TBitBtn;
    BitBtn16: TBitBtn;
    BitBtn17: TBitBtn;
    BitBtn18: TBitBtn;
    BitBtn19: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn20: TBitBtn;
    BitBtn21: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    BitBtn7: TBitBtn;
    BitBtn8: TBitBtn;
    BitBtn9: TBitBtn;
    Button4: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Edit1: TEdit;
    FontDialog1: TFontDialog;
    Image1: TImage;
    Image10: TImage;
    Image11: TImage;
    Image12: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    Image8: TImage;
    Image9: TImage;
    ImageList1: TImageList;
    ImageList2: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    MenuItem1: TMenuItem;
    OpenDialog1: TOpenDialog;
    OpenDialog2: TOpenDialog;
    PageControl1: TPageControl;
    PaintBox1: TPaintBox;
    PopupMenu1: TPopupMenu;
    PopupMenu2: TPopupMenu;
    SaveDialog1: TSaveDialog;
    SaveDialog2: TSaveDialog;
    SaveDialog3: TSaveDialog;
    ScrollBar1: TScrollBar;
    ScrollBar2: TScrollBar;
    SpeedButton1: TSpeedButton;
    SpeedButton10: TSpeedButton;
    SpeedButton11: TSpeedButton;
    SpeedButton12: TSpeedButton;
    SpeedButton13: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    StatusBar1: TStatusBar;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    Procedure Bevel12ChangeBounds(Sender: TObject);
    Procedure Bevel13ChangeBounds(Sender: TObject);
    Procedure Bevel14ChangeBounds(Sender: TObject);
    Procedure Bevel15ChangeBounds(Sender: TObject);
    Procedure Bevel16ChangeBounds(Sender: TObject);
    Procedure Bevel19ChangeBounds(Sender: TObject);
    Procedure Bevel1ChangeBounds(Sender: TObject);
    Procedure Bevel5ChangeBounds(Sender: TObject);
    Procedure Bevel6ChangeBounds(Sender: TObject);
    Procedure Bevel7ChangeBounds(Sender: TObject);
    Procedure Bevel8ChangeBounds(Sender: TObject);
    Procedure Bevel9ChangeBounds(Sender: TObject);
    Procedure BitBtn10Click(Sender: TObject);
    Procedure BitBtn11Click(Sender: TObject);
    Procedure BitBtn12Click(Sender: TObject);
    Procedure BitBtn13Click(Sender: TObject);
    Procedure BitBtn14Click(Sender: TObject);
    Procedure BitBtn15Click(Sender: TObject);
    Procedure BitBtn16Click(Sender: TObject);
    Procedure BitBtn17Click(Sender: TObject);
    Procedure BitBtn18Click(Sender: TObject);
    Procedure BitBtn1Click(Sender: TObject);
    Procedure BitBtn20Click(Sender: TObject);
    Procedure BitBtn21Click(Sender: TObject);
    Procedure BitBtn2Click(Sender: TObject);
    Procedure BitBtn3Click(Sender: TObject);
    Procedure BitBtn4Click(Sender: TObject);
    Procedure BitBtn5Click(Sender: TObject);
    Procedure BitBtn6Click(Sender: TObject);
    Procedure BitBtn7Click(Sender: TObject);
    Procedure BitBtn8Click(Sender: TObject);
    Procedure BitBtn9Click(Sender: TObject);
    Procedure Button4Click(Sender: TObject);
    Procedure Button6Click(Sender: TObject);
    Procedure Button7Click(Sender: TObject);
    Procedure Button8Click(Sender: TObject);
    Procedure CheckBox2Click(Sender: TObject);
    Procedure ComboBox1Change(Sender: TObject);
    Procedure ComboBox2Change(Sender: TObject);
    Procedure Edit1Change(Sender: TObject);
    Procedure FormChangeBounds(Sender: TObject);
    Procedure FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
    Procedure FormCreate(Sender: TObject);
    Procedure FormDropFiles(Sender: TObject; Const FileNames: Array Of String);
    Procedure FormKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState);
    Procedure MenuItem1Click(Sender: TObject);
    Procedure PageControl1Change(Sender: TObject);
    Procedure PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    Procedure PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    Procedure PaintBox1MouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; Var Handled: Boolean);
    Procedure PaintBox1MouseWheelLeft(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; Var Handled: Boolean);
    Procedure PaintBox1MouseWheelRight(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; Var Handled: Boolean);
    Procedure PaintBox1MouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; Var Handled: Boolean);
    Procedure PaintBox1Paint(Sender: TObject);
    Procedure PaintBox1Resize(Sender: TObject);
    Procedure ScrollBar1Change(Sender: TObject);
    Procedure SpeedButton13Click(Sender: TObject);
    Procedure SpeedButton1Click(Sender: TObject);
    Procedure Bevel1Click(Sender: TObject);
    Procedure Bevel12Click(Sender: TObject);
    Procedure Bevel13Click(Sender: TObject);
    Procedure Bevel14Click(Sender: TObject);
    Procedure Bevel15Click(Sender: TObject);
    Procedure Bevel16Click(Sender: TObject);
    Procedure Bevel19Click(Sender: TObject);
    Procedure Bevel5Click(Sender: TObject);
    Procedure Bevel6Click(Sender: TObject);
    Procedure Bevel7Click(Sender: TObject);
    Procedure Bevel8Click(Sender: TObject);
    Procedure Bevel9Click(Sender: TObject);
    Procedure OnEditTextField(Sender: TObject);
  private
    forma: TFormatSettings;
    FUndoImg: Array Of TBitmap;
    fUndoImgCnt: integer;
    fUndoObj: Array Of TMeasureElement;
    fUndoObjCnt: integer;
    MouseAbsolute: Tpoint; // Die Aktuelle MausPosition in Paintbox Koordinaten
    fMouseDownPos: TPoint; // Wird Beim Mouse Down genommen
    fPreview: TPreview;
    br: Tpoint;
    fMetrik: TMetrik;
    fPerspectiveCorrection: TPerspectiveCorrection;
    fHeightCorrection: THeightCorrection;
    fRotateCorrection: TRotateCorrection;
    fCutTool: TRectangle;
    fChanged: Boolean;

    Function AskChange(): Boolean; // True, wenn Wechsel erlaubt, sonst false
    Procedure SetChanged();

    Procedure Disable_All_Measurement_Elements();
    Function GlobalToImage(P: Tpoint; Relative: Boolean = false): TPoint; // Rechnet PaintBox X / Y Koordinaten in FImage Koordinaten um
    Function ImageToGlobal(P: Tpoint; Relative: Boolean = false): TPoint; // Inverse zu GlobalToImage
    Procedure RenderLegend();
    Procedure ResetMetrik;
    Procedure OnMetrikChange(Sender: TObject);
    Procedure Clear;
    Procedure SetFormSetPositionToBevel(Const frm: TForm; Const Obj: TBevel);
    Procedure DisableAllManipulate_except(Sender: TBevel);
    Procedure DisableAllView_Except(Sender: TBevel);
    Procedure UpdateSizeInfo();
    Procedure ZoomOnGlobalPos(Pos: TPoint; Direction: Integer); // Direction [-1, 1], pos = Paintbox1.canvas Position
    Procedure RefreshHistogram();

    Procedure PushUndoImg(Const Image: TBitmap);
    Function PopUndoImg(): TBitmap;
    Procedure ClearImgUndoStack();

    Procedure PushUndoObj(Const Obj: TMeasureElement);
    Function PopUndoObj(): TMeasureElement;
    Procedure ClearObjUndoStack();

    Procedure ObjChange(Sender: TObject);

    Function GetFileTypeFromFile(Const Filename: String): TFileType;
    Function SaveImageAs(Const Image: TBitmap; Filename: String; FileType: TFileType): Boolean;

    (*
     * Ab hier alles was für die Skripte Benötigt wird
     *)
    Procedure InitScripts();
    Procedure CreateInitialSkripts(Path: String);
    Procedure OnScriptClick(Sender: TObject);
    Procedure CheckAndCreateAllSkripts();

    Function Interpreter_AbsoluteFromRelativeX(Const Params: Array Of String): String;
    Function Interpreter_AbsoluteFromRelativeY(Const Params: Array Of String): String;
    Function Interpreter_DrawCircle(Const Params: Array Of String): String;
    Function Interpreter_DrawFilledCircle(Const Params: Array Of String): String;
    Function Interpreter_DrawFilledRectangle(Const Params: Array Of String): String;
    Function Interpreter_DrawLine(Const Params: Array Of String): String;
    Function Interpreter_DrawText(Const Params: Array Of String): String;
    Function Interpreter_GetMouseX(Const Params: Array Of String): String;
    Function Interpreter_GetMouseY(Const Params: Array Of String): String;
    Function Interpreter_GetScaling(Const Params: Array Of String): String;
    Function Interpreter_GetTimeStamp(Const Params: Array Of String): String;
    Function Interpreter_GraphicsHeight(Const Params: Array Of String): String;
    Function Interpreter_GraphicsWidth(Const Params: Array Of String): String;
    Function Interpreter_IntToStr(Const Params: Array Of String): String;
    Function Interpreter_FloatToStr(Const Params: Array Of String): String;
    Function Interpreter_max(Const Params: Array Of String): String;
    Function Interpreter_min(Const Params: Array Of String): String;
    Function Interpreter_RelativeFromAbsoluteX(Const Params: Array Of String): String;
    Function Interpreter_RelativeFromAbsoluteY(Const Params: Array Of String): String;
    Function Interpreter_SetBrushColor(Const Params: Array Of String): String;
    Function Interpreter_SetColor(Const Params: Array Of String): String;
    Function Interpreter_SetFont(Const Params: Array Of String): String;
    Function Interpreter_SetFontColor(Const Params: Array Of String): String;
    Function Interpreter_SetLineWidth(Const Params: Array Of String): String;
    Function Interpreter_TextHeight(Const Params: Array Of String): String;
    Function Interpreter_TextWidth(Const Params: Array Of String): String;

  public
    fMeasureElements: Array Of TMeasureElement;
    fImage: TBitmap;
    fStretchbmp: TBitmap;
    fProjectFileName: String;
    fSkripts: Array Of TSkript;
    Procedure ReCreateStretchBMP();

    Procedure LoadImage(Filename: String);
    Procedure SaveProject(Filename: String);
    Procedure LoadProject(Filename: String);
    Function NewInterpreter(): Tinterpreter;
  End;

Var
  Form1: TForm1;
  Defcaption: String;

Function TransformRoutine2(x, y: integer; Relative: Boolean): TPoint;
Function InverseTransformRoutine(x, y: integer; Relative: Boolean = false): TPoint;

Function PixelToDistance(Len: Single): Single;
Function PixelToArea(Area: Single): Single;

Function GetPixelUnit(): String;
Function GetPixelAreaUnit(): String;

Function GetValue(Section, Ident, Default: String): String;
Procedure SetValue(Section, Ident, value: String);

Implementation

{$R *.lfm}

Uses ugraphics, LCLType
  , Unit2 // Enter Text dialog
  , Unit3 // Verzeichnung Settings
  , unit4 // Vignettierung Settings
  , unit5 // Posterize
  , unit6 // Script Editor
  //, unit7 // Online Hilfe, Script Editor
  , unit8 // Histogram Ansicht
  , unit9 // Histogramm Korrektur
  , unit10 // Belichtung
  , unit11 // Image
  , unit12 // Tabular
  , unit13 // Farbe
  , unit14 // Optionen
  , uvectormath
  , FileUtil
  , LazFileUtils
  , math
  , intfgraphics, lazcanvas, FPCanvas // Stretch Bitmap to Bitmap
  , LConvEncoding, IniFiles
  ;

Var
  ini: TIniFile = Nil;

Function GetValue(Section, Ident, Default: String): String;
Begin
  If assigned(ini) Then Begin
    result := ini.ReadString(Section, Ident, Default);
  End
  Else Begin
    result := Default;
  End;
End;

Procedure SetValue(Section, Ident, value: String);
Begin
  If assigned(ini) Then Begin
    ini.WriteString(Section, Ident, value);
  End;
End;

Function GetPixelUnit(): String;
Begin
  result := form1.ComboBox2.Text;
End;

Function GetPixelAreaUnit(): String;
Begin
  result := form1.ComboBox2.Text + '²';
End;

Function PixelToDistance(Len: Single): Single;
Begin
  result := form1.fMetrik.PixelToDistance(len);
End;

Function PixelToArea(Area: Single): Single;
Begin
  result := form1.fMetrik.PixelToArea(Area);
End;

Function TransformRoutine(x, y: integer): TPoint;
Begin
  result := form1.GlobalToImage(point(x, y));
End;

Function TransformRoutine2(x, y: integer; Relative: Boolean): TPoint;
Begin
  result := form1.GlobalToImage(point(x, y), Relative);
End;

Function InverseTransformRoutine(x, y: integer; Relative: Boolean): TPoint;
Begin
  result := form1.ImageToGlobal(point(x, y), Relative);
End;

(*
 * Quelle: https://wiki.freepascal.org/Developing_with_Graphics#Using_the_non-native_StretchDraw_from_LazCanvas
 *)

Procedure StretchDrawBitmapToBitmap(SourceBitmap, DestBitmap: TBitmap; DestWidth, DestHeight: integer);
Var
  DestIntfImage, SourceIntfImage: TLazIntfImage;
  DestCanvas: TLazCanvas;
Begin
  // Prepare the destination

  DestIntfImage := TLazIntfImage.Create(0, 0);
  DestIntfImage.LoadFromBitmap(DestBitmap.Handle, 0);

  DestCanvas := TLazCanvas.Create(DestIntfImage);

  //Prepare the source
  SourceIntfImage := TLazIntfImage.Create(0, 0);
  SourceIntfImage.LoadFromBitmap(SourceBitmap.Handle, 0);

  // Execute the stretch draw via TFPSharpInterpolation
  DestCanvas.Interpolation := TFPSharpInterpolation.Create;
  DestCanvas.StretchDraw(0, 0, DestWidth, DestHeight, SourceIntfImage);

  // Reload the image into the TBitmap
  DestBitmap.LoadFromIntfImage(DestIntfImage);

  SourceIntfImage.Free;
  DestCanvas.Interpolation.Free;
  DestCanvas.Free;
  DestIntfImage.Free;
End;

{ TForm1 }

Procedure TForm1.FormCreate(Sender: TObject);
Var
  f: String;
  i: Integer;
Begin
  forma := DefaultFormatSettings;
  forma.DecimalSeparator := '.';
  fUndoObj := Nil;
  FUndoImg := Nil;

  ini := TIniFile.Create(IncludeTrailingPathDelimiter(GetAppConfigDir(false)) + 'settings.ini');

  Defcaption := 'Image inspector ver. 0.07';
  Caption := Defcaption;
  MouseAbsolute := point(0, 0);
  fProjectFileName := '';
  fimage := Nil;
  setlength(FUndoImg, strtointdef(GetValue('General', 'MaxUndoImg', '10'), 10));
  For i := 0 To high(FUndoImg) Do Begin
    FUndoImg[i] := Nil;
  End;
  fUndoImgCnt := 0;
  setlength(fUndoObj, strtointdef(GetValue('General', 'MaxUndoObj', '100'), 100));
  For i := 0 To high(fUndoObj) Do Begin
    fUndoObj[i] := Nil;
  End;
  fUndoObjCnt := 0;
  fStretchbmp := TBitmap.Create;
  PageControl1.ActivePageIndex := 0;
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
  ueventer.TransformRoutine := @TransformRoutine;
  fMetrik := TMetrik.create(Paintbox1);
  fMetrik.OnChange := @OnMetrikChange;
  fPerspectiveCorrection := tPerspectiveCorrection.Create(PaintBox1);
  fHeightCorrection := THeightCorrection.Create(PaintBox1);
  fRotateCorrection := TRotateCorrection.create(PaintBox1);
  fCutTool := TRectangle.Create(PaintBox1);
  fCutTool.Visible := false;
  ResetMetrik; // -- Das Setzt das Changed
  fMetrik.Visible := false;
  Bevel1.OnClick := @Bevel1Click;
  Bevel5.OnClick := @Bevel5Click;
  Bevel6.OnClick := @Bevel6Click;
  Bevel7.OnClick := @Bevel7Click;
  Bevel8.OnClick := @Bevel8Click;
  Bevel9.OnClick := @Bevel9Click;
  Bevel12.OnClick := @Bevel12Click;
  Bevel13.OnClick := @Bevel13Click;
  Bevel14.OnClick := @Bevel14Click;
  Bevel15.OnClick := @Bevel15Click;
  Bevel16.OnClick := @Bevel16Click;
  Bevel19.OnClick := @Bevel19Click;

  fMeasureElements := Nil;
  fPreview := TPreview.Create();
  fPreview.Clear;
  fChanged := false;
  ComboBox1.ItemIndex := View_Index_1_1;
  InitScripts();
  If (ParamStr(1) <> '') And FileExists(ParamStr(1)) Then Begin
    If lowercase(ExtractFileExt(ParamStr(1))) = '.iip' Then Begin
      LoadProject(ParamStr(1));
    End
    Else Begin
      LoadImage(ParamStr(1));
    End;
  End
  Else Begin
    If GetValue('General', 'RememberLast', '1') = '1' Then Begin
      f := GetValue('General', 'LastFile', '');
      If (f <> '') And FileExists(f) Then Begin
        If LowerCase(ExtractFileExt(f)) = '.iip' Then Begin
          LoadProject(f);
        End
        Else Begin
          LoadImage(f);
        End;
      End;
    End;
  End;


  (*
  // Debug alles ab hier sollte wieder raus !!

  //LoadImage('..' + PathDelim + 'Lena.png');
  //LoadImage('..' + PathDelim + 'Lena_gray.png');
  //LoadImage('Images' + PathDelim + 'Img_colors.png');
  //LoadImage('Images' + PathDelim + 'Img_Grid.png');
  //LoadImage('Images' + PathDelim + 'Img_obstacles.png');
  //LoadImage('Images' + PathDelim + 'Face.bmp');
  //LoadImage('Images' + PathDelim + 'Tonnen_Verzerrung.bmp');
  //LoadImage('Images' + PathDelim + 'Scale_Demo.bmp');
  //LoadImage('/home/corpsman/Desktop/Vorher.jpg');
  //LoadProject('/home/corpsman/Desktop/asd.iip');
  //LoadProject('Images' + PathDelim + 'Demo_Project.iip');
  fChanged := false;
  //PageControl1.ActivePage := TabSheet5;
  //PageControl1.ActivePage := TabSheet4;
  //PageControl1.ActivePage := TabSheet3;
  //PageControl1.ActivePage := TabSheet2;
  //ComboBox1.ItemIndex := 0;
  // *)
End;

Procedure TForm1.FormDropFiles(Sender: TObject; Const FileNames: Array Of String
  );
Var
  f: String;
Begin
  If length(FileNames) > 0 Then Begin
    f := FileNames[0];

    If (f <> '') And FileExists(f) Then Begin
      If LowerCase(ExtractFileExt(f)) = '.iip' Then Begin
        LoadProject(f);
      End
      Else Begin
        LoadImage(f);
      End;
    End;
  End;
End;

Procedure TForm1.FormCloseQuery(Sender: TObject; Var CanClose: Boolean);
Begin
  If fChanged Then Begin
    If ID_NO = application.MessageBox('Closing without save will loss all changes. Do you really want to close without save ?', 'Warning', MB_YESNO Or MB_ICONWARNING) Then Begin
      CanClose := false;
      exit;
    End;
  End;
  Clear;
  ClearObjUndoStack();
  ClearImgUndoStack();
  fPreview.Free;
  fPerspectiveCorrection.Free;
  fHeightCorrection.free;
  fRotateCorrection.free;
  fStretchbmp.free;
  fStretchbmp := Nil;
  fCutTool.Free;
  fCutTool := Nil;
  ini.free;
  ini := Nil;
End;

Procedure TForm1.FormKeyDown(Sender: TObject; Var Key: Word; Shift: TShiftState
  );
Var
  i, j: Integer;
Begin
  (*
   * TPaintbox hat keine Tastatur Ereignisse, also fangen wir die über das Formular ab
   *)
  If PageControl1.ActivePage = TabSheet5 Then Begin // Meassure Dialog
    If SpeedButton1.Down And (key = VK_DELETE) Then Begin // Auswahl Dialog, Element Löschen
      For i := 0 To high(fMeasureElements) Do Begin
        If fMeasureElements[i].Enabled Then Begin
          fMeasureElements[i].Free;
          For j := i To high(fMeasureElements) - 1 Do Begin
            fMeasureElements[j] := fMeasureElements[j + 1];
          End;
          setlength(fMeasureElements, high(fMeasureElements));
          PaintBox1.Invalidate;
          break;
        End;
      End;
    End;
  End;
End;

Procedure TForm1.OnEditTextField(Sender: TObject);
Begin
  If sender Is TTextField Then Begin
    If Not (sender As TTextField).ReadOnly Then Begin
      form2.Memo1.Text := (sender As TTextField).Text;
      If form2.ShowModal = mrOK Then Begin
        (sender As TTextField).Text := form2.Memo1.Text;
        PaintBox1.Invalidate;
      End;
    End;
  End;
End;

Procedure TForm1.MenuItem1Click(Sender: TObject);
Var
  i: integer;
Begin
  // Edit Text, from Textfield
  For i := 0 To high(fMeasureElements) Do Begin
    If (fMeasureElements[i].Enabled) And (fMeasureElements[i] Is TText) Then Begin
      form2.Memo1.Text := (fMeasureElements[i] As TText).GetText();
      If form2.ShowModal = mrOK Then Begin
        (fMeasureElements[i] As TText).SetText(form2.Memo1.Text);
        PaintBox1.Invalidate;
      End;
    End;
  End;
End;

Procedure TForm1.PageControl1Change(Sender: TObject);
Begin
  // Beim Wechsel der Tabs ist auf jedenfall Schluss
  fPreview.Clear;
  Disable_All_Measurement_Elements();
  If PageControl1.ActivePage = TabSheet5 Then Begin // Der "Löschen" Cursor
    If SpeedButton13.Down Then Begin
      PaintBox1.Cursor := crCross;
    End;
  End;
  If Bevel1.Style = bsLowered Then Begin // Ausschalten Metrik
    Bevel1Click(Nil);
  End;
  DisableAllManipulate_except(Nil);
  DisableAllView_Except(Nil);
  PaintBox1.Invalidate;
End;

Procedure TForm1.PaintBox1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);

  Procedure StartKind(Kind: TPreviewKind);
  Var
    off, p: Tpoint;
  Begin
    fPreview.Clear;
    fPreview.Kind := Kind;
    setlength(fPreview.Points, 1);
    p := GlobalToImage(point(x, y));
    off := GlobalToImage(point(5, 5), true);
    fPreview.Points[0] := point(p.x - off.x, p.y - off.y);
    p.X := p.x - off.x;
    p.Y := p.Y - off.Y;
    fPreview.aMousePos := p;
  End;

  Procedure AddPrevPoint();
  Var
    off, p: Tpoint;
  Begin
    setlength(fPreview.Points, high(fPreview.Points) + 2);
    p := GlobalToImage(point(x, y));
    off := GlobalToImage(point(5, 5), true);
    fPreview.Points[high(fPreview.Points)] := point(p.x - off.x, p.y - off.y);
    p.X := p.x - off.x;
    p.Y := p.Y - off.Y;
    fPreview.aMousePos := p;
  End;

  Procedure Finish(C: TMeasureElementClass);
  Var
    cl: TMeasureElement;
  Begin
    setlength(fMeasureElements, high(fMeasureElements) + 2);
    cl := c.Create(PaintBox1);
    fMeasureElements[high(fMeasureElements)] := cl;
    cl.SetPoints(fPreview.Points);
    cl.InitText;
    cl.Enabled := false;
    cl.TextVisible := CheckBox2.Checked;
    cl.OnChange := @ObjChange;
    fPreview.Clear;
    If form12.Visible Then Begin
      form12.RefreshData;
    End;
    SetChanged();
  End;

Var
  i, j: Integer;
  p: Tpoint;
Begin
  MouseAbsolute := point(x, y);
  fMouseDownPos := point(x, y);
  If PageControl1.ActivePage = TabSheet5 Then Begin // Meassurement Tab
    If ssleft In shift Then Begin
      If SpeedButton1.Down And SpeedButton1.Enabled Then Begin // Select
        p := GlobalToImage(point(x, y));
        PaintBox1.PopupMenu := Nil;
        For i := 0 To high(fMeasureElements) Do Begin
          If fMeasureElements[i].Hit(p.x, p.y) Then Begin
            fMeasureElements[i].Enabled := true;
            If fMeasureElements[i] Is TText Then Begin
              PaintBox1.PopupMenu := PopupMenu2;
            End;
          End
          Else Begin
            fMeasureElements[i].Enabled := false;
          End;
        End;
      End
      Else Begin
        Disable_All_Measurement_Elements();
        If SpeedButton13.Down Then Begin // wieder einschalten, falls notwendig...
          PaintBox1.Cursor := crCross;
        End;
      End;
      If SpeedButton2.Down Then Begin // Line
        If fPreview.Kind <> pkLine Then Begin
          StartKind(pkLine);
        End
        Else Begin
          AddPrevPoint();
          Finish(TMultiLine);
        End;
      End;
      If SpeedButton3.Down Then Begin // Multiline
        If fPreview.Kind <> pkMultiLine Then Begin
          StartKind(pkMultiLine);
        End
        Else Begin
          AddPrevPoint();
        End;
      End;
      If SpeedButton4.Down Then Begin // Polygon
        If fPreview.Kind <> pkPolygon Then Begin
          StartKind(pkPolygon);
        End
        Else Begin
          AddPrevPoint();
        End;
      End;
      If SpeedButton5.Down Then Begin // Rectangle
        If fPreview.Kind <> pkRectangle Then Begin
          StartKind(pkRectangle);
        End
        Else Begin
          AddPrevPoint();
          Finish(TRectangle);
        End;
      End;
      If SpeedButton6.Down Then Begin // Line Point Distance
        If fPreview.Kind <> pkLinePoint Then Begin
          StartKind(pkLinePoint);
        End
        Else Begin
          AddPrevPoint();
          If length(fPreview.Points) = 3 Then Begin
            Finish(TLinePoint);
          End;
        End;
      End;
      If SpeedButton7.Down Then Begin // Kreis definiert durch 2 Punkte
        If fPreview.Kind <> pk2PointCircle Then Begin
          StartKind(pk2PointCircle);
        End
        Else Begin
          AddPrevPoint();
          Finish(T2PointCircle);
        End;
      End;
      If SpeedButton8.Down Then Begin // Kreis definiert durch 3 Punkte
        If fPreview.Kind <> pk3PointCircle Then Begin
          StartKind(pk3PointCircle);
        End
        Else Begin
          AddPrevPoint();
          If length(fPreview.Points) = 3 Then Begin
            Finish(T3PointCircle);
          End;
        End;
      End;
      If SpeedButton9.Down Then Begin // Winkelmessung
        If fPreview.Kind <> pkAngle Then Begin
          StartKind(pkAngle);
        End
        Else Begin
          AddPrevPoint();
          If length(fPreview.Points) = 3 Then Begin
            Finish(TAngle);
          End;
        End;
      End;
      If SpeedButton10.Down Then Begin // Kreisbogen definiert durch 3 Punkte
        If fPreview.Kind <> pkArc Then Begin
          StartKind(pkArc);
        End
        Else Begin
          AddPrevPoint();
          If length(fPreview.Points) = 3 Then Begin
            Finish(TArc);
          End;
        End;
      End;
      If SpeedButton11.Down Then Begin // Ein Pfeil zum Zeigen
        If fPreview.Kind <> pkArrow Then Begin
          StartKind(pkArrow);
        End
        Else Begin
          AddPrevPoint();
          Finish(TArrow);
        End;
      End;
      If SpeedButton12.Down Then Begin
        form2.Memo1.Clear;
        If form2.ShowModal = mrOK Then Begin
          fPreview.Clear;
          AddPrevPoint(); // Einen Punkt Anfügen, damit das Element weis, wo es erzeugt werden soll.
          Finish(TText);
          (fMeasureElements[high(fMeasureElements)] As TText).SetText(Form2.Memo1.Text);
        End;
      End;
      If SpeedButton13.Down Then Begin // Erase
        p := GlobalToImage(point(x, y));
        For i := 0 To high(fMeasureElements) Do Begin
          If fMeasureElements[i].Hit(p.x, p.y) Then Begin
            PushUndoObj(fMeasureElements[i]);
            For j := i To high(fMeasureElements) - 1 Do Begin
              fMeasureElements[j] := fMeasureElements[j + 1];
            End;
            setlength(fMeasureElements, high(fMeasureElements));
            If form12.Visible Then Begin
              form12.RefreshData;
            End;
            break;
          End;
        End;
      End;
    End;
    If ssright In shift Then Begin // Abbruch der meisten Elemente, oder Fertigstellen
      If SpeedButton3.Down Then Begin // Multiline - Abschliesen
        If length(fPreview.Points) >= 2 Then Begin
          Finish(TMultiLine);
        End;
      End;
      If SpeedButton4.Down Then Begin // Polygon - Abschliesen
        If length(fPreview.Points) >= 3 Then Begin
          Finish(TPolygon);
        End;
      End;
      fPreview.Clear;
    End;
  End;
  PaintBox1.Invalidate;
End;

Procedure TForm1.PaintBox1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
Var
  c: TColor;
  off, p: TPoint;
  b: Boolean;
  i: Integer;
Begin
  MouseAbsolute := Point(x, y);
  If Not assigned(fImage) Then Begin
    StatusBar1.Panels[Panel_index_Cursorpos].Text := '-';
    StatusBar1.Panels[Panel_index_RGB].Text := '-';
    exit;
  End;
  p := GlobalToImage(point(x, y));
  StatusBar1.Panels[Panel_index_Cursorpos].Text := format('X,Y = %d,%d', [p.x, p.y]);
  c := fImage.Canvas.Pixels[p.x, p.y];
  StatusBar1.Panels[Panel_index_RGB].Text := format('RGB = (%d,%d,%d)', [c And $FF, (c And $FF00) Shr 8, (c And $FF0000) Shr 16]);
  If fPreview.Kind <> pkNone Then Begin
    off := GlobalToImage(point(5, 5), true);
    p.X := p.x - off.x;
    p.Y := p.Y - off.Y;
    fPreview.aMousePos := p;
    PaintBox1.Invalidate;
  End;
  // Draging by user
  If ssleft In shift Then Begin
    // Wenn ein Element angewählt ist schalten wir das Dragging aus ;)
    // Via And eine Liste auf alles was so verschoben werden kann
    b := fPreview.Kind = pkNone;
    b := b And (Not (fMetrik.Enabled And fMetrik.Visible));
    b := b And (Not (fPerspectiveCorrection.Enabled And fPerspectiveCorrection.Visible));
    b := b And (Not (fHeightCorrection.Enabled And fHeightCorrection.Visible));
    b := b And (Not (fRotateCorrection.Enabled And fRotateCorrection.Visible));
    b := b And (Not (fCutTool.Enabled And fCutTool.Visible));
    If b Then Begin
      For i := 0 To high(fMeasureElements) Do Begin
        If fMeasureElements[i].Enabled Then Begin
          b := false;
          break;
        End;
      End;
    End;
    If b Then Begin
      p := fMouseDownPos - point(x, y);
      If ScrollBar1.Visible Then Begin
        ScrollBar1.Position := ScrollBar1.Position + p.x;
      End;
      If ScrollBar2.Visible Then Begin
        ScrollBar2.Position := ScrollBar2.Position + p.Y;
      End;
      PaintBox1.Invalidate;
    End;
  End;
  // Es Gibt Skripte die Reagieren auf die Mausposition -> dann muss immer neu gezeichnet werden.
  For i := 0 To high(fSkripts) Do Begin
    If fSkripts[i].Enabled Then Begin
      PaintBox1.Invalidate;
    End;
  End;
  fMouseDownPos := point(x, y);
End;

Procedure TForm1.PaintBox1MouseWheelDown(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; Var Handled: Boolean);
Begin
  If (ssmiddle In shift) Or (ssShift In Shift) Then Begin
    If ScrollBar1.Visible Then Begin
      ScrollBar1.Position := ScrollBar1.Position + round(ScrollBar1.Max * 0.1);
      PaintBox1.Invalidate;
    End;
  End
  Else Begin
    If ssCtrl In shift Then Begin
      ZoomOnGlobalPos(MousePos, 1);
    End
    Else Begin
      If ScrollBar2.Visible Then Begin
        ScrollBar2.Position := ScrollBar2.Position + round(ScrollBar2.Max * 0.1);
        PaintBox1.Invalidate;
      End;
    End;
  End;
End;

Procedure TForm1.PaintBox1MouseWheelLeft(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; Var Handled: Boolean);
Begin
  If ScrollBar1.Visible Then Begin
    ScrollBar1.Position := ScrollBar1.Position - round(ScrollBar1.Max * 0.1);
    PaintBox1.Invalidate;
  End;
End;

Procedure TForm1.PaintBox1MouseWheelRight(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; Var Handled: Boolean);
Begin
  If ScrollBar1.Visible Then Begin
    ScrollBar1.Position := ScrollBar1.Position + round(ScrollBar1.Max * 0.1);
    PaintBox1.Invalidate;
  End;
End;

Procedure TForm1.PaintBox1MouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; Var Handled: Boolean);
Begin
  If (ssmiddle In shift) Or (ssShift In Shift) Then Begin
    If ScrollBar1.Visible Then Begin
      ScrollBar1.Position := ScrollBar1.Position - round(ScrollBar1.Max * 0.1);
      PaintBox1.Invalidate;
    End;
  End
  Else Begin
    If ssCtrl In shift Then Begin
      ZoomOnGlobalPos(MousePos, -1);
    End
    Else Begin
      If ScrollBar2.Visible Then Begin
        ScrollBar2.Position := ScrollBar2.Position - round(ScrollBar2.Max * 0.1);
        PaintBox1.Invalidate;
      End;
    End;
  End;
End;

Procedure TForm1.PaintBox1Paint(Sender: TObject);
  Procedure ResetCanvasSettings();
  Begin
    // Default Einstellungen die der User Theoretisch geändert haben könnte
    PaintBox1.Canvas.Pen.Width := 1;
    PaintBox1.Canvas.Pen.Color := clblack;
    PaintBox1.Canvas.Brush.Color := clBlack;
    PaintBox1.Canvas.Brush.Style := bsSolid;
    PaintBox1.Canvas.Font.Color := clWhite;
    PaintBox1.Canvas.Font.Name := 'default';
    PaintBox1.Canvas.Font.Size := 0;
  End;
Var
  i: Integer;
Begin
  If Not assigned(fImage) Then exit;
  // Das Eigentliche Bild
  // PaintBox1.Canvas.StretchDraw(rect(-ScrollBar1.Position, -ScrollBar2.Position, br.X - ScrollBar1.Position, br.Y - ScrollBar2.Position), fImage);
  PaintBox1.Canvas.draw(-ScrollBar1.Position, -ScrollBar2.Position, fStretchbmp);
  // Die Skripte
  For i := 0 To high(fSkripts) Do Begin
    If fSkripts[i].Enabled Then Begin
      ResetCanvasSettings();
      Try
        fSkripts[i].Interpreter.CallFunction('OnRender', []);
      Except
        fSkripts[i].Enabled := false;
      End;
    End;
  End;
  If form6.Visible And form6.InterpreterValid Then Begin
    ResetCanvasSettings();
    form6.interpreter.CallFunction('OnRender', []);
  End;
  ResetCanvasSettings();
  // Die Zusatzsachen
  fMetrik.Render();
  If CheckBox1.Checked Then Begin
    RenderLegend();
  End;
  For i := 0 To high(fMeasureElements) Do Begin
    fMeasureElements[i].Render();
  End;
  fPerspectiveCorrection.Render;
  fHeightCorrection.Render;
  fRotateCorrection.Render;
  fCutTool.Render;
  // Die Vorschau immer als Letztes
  fPreview.Render(PaintBox1.Canvas);
End;

Procedure TForm1.PaintBox1Resize(Sender: TObject);
Begin
  ComboBox1.OnChange(Nil);
End;

Procedure TForm1.ScrollBar1Change(Sender: TObject);
Begin
  PaintBox1.Invalidate;
End;

Procedure TForm1.Bevel1ChangeBounds(Sender: TObject);
Begin
  // Den Doppelclick des Bevels gibt es nicht das der zu Click führt
  // Der Gesuchte Code wurde händisch eingefügt und steht daher 6 Zeilen tiefer ;)
End;

Procedure TForm1.Bevel1Click(Sender: TObject);
Begin
  If Not assigned(fImage) Then exit;
  // Edit Metrik
  If Bevel1.Style = bsRaised Then Begin
    Bevel1.Style := bsLowered;
    fMetrik.Visible := true;
    Edit1.Text := format('%0.1f', [fMetrik.Distance]);
    Edit1.Enabled := true;
    label3.Enabled := true;
    SpeedButton1.Down := true;
    SpeedButton1.Enabled := false;
    SpeedButton2.Enabled := false;
    SpeedButton3.Enabled := false;
    SpeedButton4.Enabled := false;
    SpeedButton5.Enabled := false;
    SpeedButton6.Enabled := false;
    SpeedButton7.Enabled := false;
    SpeedButton8.Enabled := false;
    SpeedButton9.Enabled := false;
    SpeedButton10.Enabled := false;
    SpeedButton11.Enabled := false;
    SpeedButton12.Enabled := false;
    SpeedButton13.Enabled := false;
    Disable_All_Measurement_Elements();
  End
  Else Begin
    Bevel1.Style := bsRaised;
    fMetrik.Visible := false;
    Edit1.Enabled := false;
    label3.Enabled := false;
    SpeedButton1.Enabled := true;
    SpeedButton2.Enabled := true;
    SpeedButton3.Enabled := true;
    SpeedButton4.Enabled := true;
    SpeedButton5.Enabled := true;
    SpeedButton6.Enabled := true;
    SpeedButton7.Enabled := true;
    SpeedButton8.Enabled := true;
    SpeedButton9.Enabled := true;
    SpeedButton10.Enabled := true;
    SpeedButton11.Enabled := true;
    SpeedButton12.Enabled := true;
    SpeedButton13.Enabled := true;
  End;
  PaintBox1.Invalidate;
End;

Procedure TForm1.Bevel12ChangeBounds(Sender: TObject);
Begin
  // Den Doppelclick des Bevels gibt es nicht das der zu Click führt
  // Der Gesuchte Code wurde händisch eingefügt und steht daher 6 Zeilen tiefer ;)
End;

Procedure TForm1.Bevel12Click(Sender: TObject);
Begin
  // Posterize
  If Not assigned(fImage) Then exit;
  DisableAllManipulate_except(Bevel12);
  If Bevel12.Style = bsRaised Then Begin
    Bevel12.Style := bsLowered;
    form5.CreateBackup();
    form5.resetSettings;
    form5.Show;
    SetFormSetPositionToBevel(form5, Bevel12);
  End
  Else Begin
    form5.DoTheMagic(true);
    PushUndoImg(form5.FImageBackup);
    Bevel12.Style := bsRaised;
    form5.Hide;
  End;
  PaintBox1.Invalidate;
End;

Procedure TForm1.Bevel13ChangeBounds(Sender: TObject);
Begin
  // Den Doppelclick des Bevels gibt es nicht das der zu Click führt
  // Der Gesuchte Code wurde händisch eingefügt und steht daher 6 Zeilen tiefer ;)
End;

Procedure TForm1.Bevel13Click(Sender: TObject);
Begin
  // Histogramm Korrectur
  If Not assigned(fImage) Then exit;
  DisableAllManipulate_except(Bevel13);
  If Bevel13.Style = bsRaised Then Begin
    Bevel13.Style := bsLowered;
    form9.CreateBackup();
    form9.resetSettings;
    form9.Show;
    SetFormSetPositionToBevel(form9, Bevel13);
  End
  Else Begin
    form9.DoTheMagic(true);
    PushUndoImg(form9.FImageBackup);
    Bevel13.Style := bsRaised;
    form9.Hide;
  End;
  PaintBox1.Invalidate;
End;

Procedure TForm1.Bevel14ChangeBounds(Sender: TObject);
Begin
  // Den Doppelclick des Bevels gibt es nicht das der zu Click führt
  // Der Gesuchte Code wurde händisch eingefügt und steht daher 6 Zeilen tiefer ;)
End;

Procedure TForm1.Bevel14Click(Sender: TObject);
Begin
  // Belichtung
  If Not assigned(fImage) Then exit;
  DisableAllManipulate_except(Bevel14);
  If Bevel14.Style = bsRaised Then Begin
    Bevel14.Style := bsLowered;
    form10.CreateBackup();
    form10.resetSettings;
    form10.Show;
    SetFormSetPositionToBevel(form10, Bevel14);
  End
  Else Begin
    form10.DoTheMagic(true);
    PushUndoImg(form10.FImageBackup);
    Bevel14.Style := bsRaised;
    form10.Hide;
  End;
  PaintBox1.Invalidate;
End;

Procedure TForm1.Bevel15ChangeBounds(Sender: TObject);
Begin
  // Den Doppelclick des Bevels gibt es nicht das der zu Click führt
  // Der Gesuchte Code wurde händisch eingefügt und steht daher 6 Zeilen tiefer ;)
End;

Procedure TForm1.Bevel15Click(Sender: TObject);
Begin
  // Belichtung
  If Not assigned(fImage) Then exit;
  DisableAllManipulate_except(Bevel15);
  If Bevel15.Style = bsRaised Then Begin
    Bevel15.Style := bsLowered;
    form11.CreateBackup();
    form11.resetSettings;
    form11.Show;
    SetFormSetPositionToBevel(form11, Bevel15);
  End
  Else Begin
    form11.DoTheMagic(true);
    PushUndoImg(form11.FImageBackup);
    Bevel15.Style := bsRaised;
    form11.Hide;
  End;
  PaintBox1.Invalidate;
End;

Procedure TForm1.Bevel16ChangeBounds(Sender: TObject);
Begin
  // Den Doppelclick des Bevels gibt es nicht das der zu Click führt
  // Der Gesuchte Code wurde händisch eingefügt und steht daher 6 Zeilen tiefer ;)
End;

Procedure TForm1.Bevel16Click(Sender: TObject);
Begin
  // Belichtung
  If Not assigned(fImage) Then exit;
  DisableAllManipulate_except(Bevel16);
  If Bevel16.Style = bsRaised Then Begin
    Bevel16.Style := bsLowered;
    form13.CreateBackup();
    form13.resetSettings;
    form13.Show;
    SetFormSetPositionToBevel(form13, Bevel16);
  End
  Else Begin
    form13.DoTheMagic(true);
    PushUndoImg(form13.FImageBackup);
    Bevel16.Style := bsRaised;
    form13.Hide;
  End;
  PaintBox1.Invalidate;
End;

Procedure TForm1.Bevel19ChangeBounds(Sender: TObject);
Begin
  // Den Doppelclick des Bevels gibt es nicht das der zu Click führt
  // Der Gesuchte Code wurde händisch eingefügt und steht daher 6 Zeilen tiefer ;)
End;

Procedure TForm1.Bevel19Click(Sender: TObject);
Var
  pa: TPointArray;
Begin
  // Cut
  If Not assigned(fImage) Then exit;
  DisableAllManipulate_except(Bevel19);
  If Bevel19.Style = bsRaised Then Begin
    Bevel19.Style := bsLowered;
    fCutTool.Visible := true;
    Button8.Visible := true;
    pa := Nil;
    setlength(pa, 2);
    pa[0] := GlobalToImage(point(PaintBox1.Width Div 4, PaintBox1.Height Div 4));
    pa[1] := GlobalToImage(point((PaintBox1.Width * 3) Div 4, (PaintBox1.Height * 3) Div 4));
    fCutTool.SetPoints(Pa);
  End
  Else Begin
    fCutTool.Visible := false;
    Bevel19.Style := bsRaised;
    Button8.Visible := false;
  End;
  PaintBox1.Invalidate;
End;

Procedure TForm1.Bevel5ChangeBounds(Sender: TObject);
Begin
  // Den Doppelclick des Bevels gibt es nicht das der zu Click führt
  // Der Gesuchte Code wurde händisch eingefügt und steht daher 6 Zeilen tiefer ;)
End;

Procedure TForm1.Bevel5Click(Sender: TObject);
Var
  pa: TPointArray;
Begin
  If Not assigned(fImage) Then exit;
  DisableAllManipulate_except(Bevel5);
  // Edit Projektion
  If Bevel5.Style = bsRaised Then Begin
    Bevel5.Style := bsLowered;
    button4.visible := true;
    CheckBox3.Visible := true;
    pa := Nil;
    setlength(pa, 4);
    pa[0] := GlobalToImage(point(PaintBox1.Width Div 4, PaintBox1.Height Div 4));
    pa[1] := GlobalToImage(point(PaintBox1.Width Div 4, (PaintBox1.Height * 3) Div 4));
    pa[2] := GlobalToImage(point((PaintBox1.Width * 3) Div 4, PaintBox1.Height Div 4));
    pa[3] := GlobalToImage(point((PaintBox1.Width * 3) Div 4, (PaintBox1.Height * 3) Div 4));
    fPerspectiveCorrection.SetPoints(pa);
    fPerspectiveCorrection.Visible := true;
  End
  Else Begin
    Bevel5.Style := bsRaised;
    button4.visible := false;
    CheckBox3.Visible := false;
    fPerspectiveCorrection.Visible := false;
  End;
  PaintBox1.Invalidate;
End;

Procedure TForm1.Bevel6ChangeBounds(Sender: TObject);
Begin
  // Den Doppelclick des Bevels gibt es nicht das der zu Click führt
  // Der Gesuchte Code wurde händisch eingefügt und steht daher 6 Zeilen tiefer ;)
End;

Procedure TForm1.Bevel6Click(Sender: TObject);
Var
  pa: TPointArray;
Begin
  If Not assigned(fImage) Then exit;
  DisableAllManipulate_except(Bevel6);
  // Edit Projektion
  If Bevel6.Style = bsRaised Then Begin
    Bevel6.Style := bsLowered;
    button6.visible := true;
    setlength(pa, 4);

    // Die Senkrechte
    pa[0] := GlobalToImage(point(PaintBox1.Width Div 4, PaintBox1.Height Div 4));
    pa[1] := GlobalToImage(point(PaintBox1.Width Div 4, (PaintBox1.Height * 3) Div 4));

    // Die Waagrechte
    pa[2] := GlobalToImage(point((PaintBox1.Width * 2) Div 4, PaintBox1.Height Div 2));
    pa[3] := GlobalToImage(point((PaintBox1.Width * 3) Div 4, PaintBox1.Height Div 2));

    fHeightCorrection.Texts[0] := format('%0.1f', [100.0]);
    fHeightCorrection.Texts[1] := format('%0.1f', [100.0]);

    fHeightCorrection.SetPoints(pa);
    fHeightCorrection.Visible := true;
  End
  Else Begin
    Bevel6.Style := bsRaised;
    button6.visible := false;
    fHeightCorrection.Visible := false;
  End;
  PaintBox1.Invalidate;
End;

Procedure TForm1.DisableAllManipulate_except(Sender: TBevel);
Begin
  If (Sender <> Bevel5) And (Bevel5.Style = bsLowered) Then Begin // Tiefenkorrektur
    Bevel5Click(Nil);
  End;
  If (Sender <> Bevel6) And (Bevel6.Style = bsLowered) Then Begin // Höhenkorrektur
    Bevel6Click(Nil);
  End;
  If (Sender <> Bevel7) And (Bevel7.Style = bsLowered) Then Begin // Verzeichnung
    Bevel7Click(Nil);
  End;
  If (Sender <> Bevel8) And (Bevel8.Style = bsLowered) Then Begin // Vignettierung
    Bevel8Click(Nil);
  End;
  If (Sender <> Bevel12) And (Bevel12.Style = bsLowered) Then Begin // Posterize
    Bevel12Click(Nil);
  End;
  If (Sender <> Bevel13) And (Bevel13.Style = bsLowered) Then Begin // Histogram Manipulation
    Bevel13Click(Nil);
  End;
  If (Sender <> Bevel14) And (Bevel14.Style = bsLowered) Then Begin // Belichtung
    Bevel14Click(Nil);
  End;
  If (Sender <> Bevel15) And (Bevel15.Style = bsLowered) Then Begin // Bild
    Bevel15Click(Nil);
  End;
  If (Sender <> Bevel16) And (Bevel16.Style = bsLowered) Then Begin // Farbe
    Bevel16Click(Nil);
  End;
  If (Sender <> Bevel19) And (Bevel19.Style = bsLowered) Then Begin // Cut
    Bevel19Click(Nil);
  End;
End;

Procedure TForm1.DisableAllView_Except(Sender: TBevel);
Begin
  If (Sender <> Bevel9) And (Bevel9.Style = bsLowered) Then Begin // Freies drehen
    Bevel9Click(Nil);
  End;
End;

Procedure TForm1.UpdateSizeInfo();
Begin
  ComboBox1.OnChange(Nil);
  StatusBar1.Panels[Panel_index_Dimension].Text := format('%dx%d', [fImage.Width, fImage.Height]);
  ResetMetrik; // Neu Initialisieren der Metrik, da ja die Größe der Graphik geändert wurde.
End;

Procedure TForm1.ZoomOnGlobalPos(Pos: TPoint; Direction: Integer);
Var
  p1, p2, p3, p4: TPoint;
Begin
  // Cursor Position Vorher Speichern
  p1 := GlobalToImage(Pos);
  // Zoom Durchführen
  ComboBox1.ItemIndex := clamp(ComboBox1.ItemIndex + direction, 0, ComboBox1.Items.Count - 1);
  ComboBox1Change(Nil);
  // Cursor Position wieder Herstellen
  p2 := GlobalToImage(Pos);
  p3 := p2 - p1;
  p4 := ImageToGlobal(p3, true);
  If ScrollBar1.Visible Then Begin
    ScrollBar1.Position := ScrollBar1.Position - (p4.x);
  End;
  If ScrollBar2.Visible Then Begin
    ScrollBar2.Position := ScrollBar2.Position - (p4.Y);
  End;
End;

Procedure TForm1.RefreshHistogram();
Begin
  If assigned(form8) And form8.Visible Then Begin
    form8.RefreshData();
  End;
End;

Procedure TForm1.PushUndoImg(Const Image: TBitmap);
Var
  i: Integer;
Begin
  // Umschalten LCL
  BitBtn18.Enabled := true;
  BitBtn18.ImageIndex := 28;
  BitBtn19.Enabled := true;
  BitBtn19.ImageIndex := 28;
  // Push des Obj in den Stack
  FUndoImg[fUndoImgCnt] := TBitmap.Create;
  FUndoImg[fUndoImgCnt].Assign(Image);
  inc(fUndoImgCnt);
  // Der "Stack" ist voll -> ältestes Element löschen
  If fUndoImgCnt = length(FUndoImg) Then Begin
    FUndoImg[0].free;
    For i := 0 To high(FUndoImg) - 1 Do Begin
      FUndoImg[i] := FUndoImg[i + 1];
    End;
    FUndoImg[high(FUndoImg)] := Nil;
    dec(fUndoImgCnt);
  End;
End;

Function TForm1.PopUndoImg(): TBitmap;
Begin
  result := Nil;
  // Wenn es noch was auf dem Stack gibt, geben wir es raus
  If fUndoImgCnt > 0 Then Begin
    dec(fUndoImgCnt);
    result := FUndoImg[fUndoImgCnt];
  End;
  // Der Stack ist Leer die LCL ausschalten
  If fUndoImgCnt = 0 Then Begin
    BitBtn18.Enabled := false;
    BitBtn18.ImageIndex := 29;
    BitBtn19.Enabled := false;
    BitBtn19.ImageIndex := 29;
  End;
End;

Procedure TForm1.ClearImgUndoStack();
Var
  img: TBitmap;
Begin
  img := PopUndoImg();
  While assigned(img) Do Begin
    img.free;
    img := PopUndoImg();
  End;
End;

Procedure TForm1.ObjChange(Sender: TObject);
Begin
  If form12.Visible Then Begin
    If sender = fMetrik Then Begin
      form12.RefreshData;
    End
    Else Begin
      form12.RefreshDataObj(Sender As TMeasureElement);
    End;
  End;
  SetChanged();
End;

Function TForm1.GetFileTypeFromFile(Const Filename: String): TFileType;
// Quelle: https://en.wikipedia.org/wiki/List_of_file_signatures
Var
  f: TFileStream;
  StartBytes: Array[0..11] Of Byte;
  i: Integer;
Begin
  result := ftUnknown;
  If Not FileExists(Filename) Then exit;
  f := TFileStream.Create(Filename, fmOpenRead);
  For i := 0 To high(StartBytes) Do Begin
    StartBytes[i] := 0;
    f.Read(StartBytes[i], sizeof(byte));
  End;

  If (StartBytes[0] = $42)
    And (StartBytes[1] = $4D) Then Begin
    result := ftBMP;
  End;

  If (StartBytes[0] = $89)
    And (StartBytes[1] = $50)
    And (StartBytes[2] = $4E)
    And (StartBytes[3] = $47)
    And (StartBytes[4] = $0D)
    And (StartBytes[5] = $0A)
    And (StartBytes[6] = $1A)
    And (StartBytes[7] = $0A)
    Then Begin
    result := ftPNG;
  End;

  If (StartBytes[0] = $FF)
    And (StartBytes[1] = $D8)
    And (StartBytes[2] = $FF)
    Then Begin
    result := ftJPG;
  End;

  f.free;
End;

Function TForm1.SaveImageAs(Const Image: TBitmap; Filename: String;
  FileType: TFileType): Boolean;
Var
  png: TPortableNetworkGraphic;
  jpg: TJPEGImage;
Begin
  result := false;
  Case FileType Of
    ftBMP: Begin
        Try
          Image.SaveToFile(Filename);
          result := true;
        Except
        End;
      End;
    ftPNG: Begin
        png := TPortableNetworkGraphic.Create;
        png.Assign(Image);
        Try
          png.SaveToFile(Filename);
          result := true;
        Finally
          png.free;
        End;
      End;
    ftJPG: Begin
        jpg := TJPEGImage.Create;
        jpg.Assign(Image);
        Try
          jpg.SaveToFile(Filename);
          result := true;
        Finally
          jpg.free;
        End;
      End;
  End;
End;

Procedure TForm1.InitScripts();
Var
  p: String;
  sl: TStringList;
  i: Integer;
  item: TMenuItem;
Begin
  // Löschen aller Popup Menu einträge
  For i := PopupMenu1.Items.Count - 1 Downto 0 Do Begin
    PopupMenu1.Items[i].Free;
  End;
  p := IncludeTrailingPathDelimiter(GetAppConfigDir(false)) + 'scripts';
  ForceDirectories(p);
  sl := FindAllFiles(p, '*.script', false);
  If sl.count = 0 Then Begin // Wenn es gar keine Skripte gibt, legen wir initial ein paar an ;)
    CreateInitialSkripts(p);
    sl.free;
    sl := FindAllFiles(p, '*.script', false);
  End;
  setlength(fSkripts, sl.Count);
  For i := 0 To sl.Count - 1 Do Begin
    fSkripts[i].Filename := sl[i];
    fSkripts[i].Enabled := false;
    fSkripts[i].Interpreter := Nil;
    item := TMenuItem.Create(PopupMenu1);
    item.Name := 'Skript_Menu_' + inttostr(i);
    item.Caption := ExtractFileNameOnly(sl[i]);
    item.Tag := i;
    item.OnClick := @OnScriptClick;
    PopupMenu1.Items.Add(item);
  End;
  sl.free;
  CheckAndCreateAllSkripts();
  PaintBox1.Invalidate;
End;

Procedure TForm1.CreateInitialSkripts(Path: String);
Var
  sl: TStringList;
Begin
  path := IncludeTrailingPathDelimiter(path);
  sl := TStringList.Create;
  // Das Crosshairs Skript
  sl.text :=
    '(* Draw a crosshairs' + LineEnding +
    '  in the image Center *)' + LineEnding +
    '' + LineEnding +
    'Procedure OnRender();' + LineEnding +
    '// here you can declare your variables' + LineEnding +
    'var cx, cy, d: integer;' + LineEnding +
    'Begin' + LineEnding +
    '  // Calculate the center of the Image' + LineEnding +
    '  cx := GraphicsWidth() div 2;' + LineEnding +
    '  cy := GraphicsHeight() div 2;' + LineEnding +
    '' + LineEnding +
    '  // Set Drawing Color to red' + LineEnding +
    '  SetColor(255, 0, 0);' + LineEnding +
    '' + LineEnding +
    '  // Calculate the Ring Width' + LineEnding +
    '  d := min(GraphicsWidth(), GraphicsHeight()) div 20;' + LineEnding +
    '' + LineEnding +
    '  // Draw the thing' + LineEnding +
    '  DrawCircle(cx - 2 * d, cy - 2 * d, cx + 2 * d, cy + 2 * d);' + LineEnding +
    '  DrawCircle(cx - d, cy - d, cx + d, cy + d);' + LineEnding +
    '  DrawLine(cx - 3 * d, cy, cx + 3 * d, cy);' + LineEnding +
    '  DrawLine(cx, cy - 3 * d, cx, cy + 3 * d);' + LineEnding +
    'End;';
  sl.SaveToFile(Path + 'crosshairs.script');
  // Das Gris Script
  sl.text :=
    '(*' + LineEnding +
    ' * Draw a grid each 50 pixel' + LineEnding +
    ' *)' + LineEnding +
    '' + LineEnding +
    'Procedure OnRender();' + LineEnding +
    'Var' + LineEnding +
    '  w, h, d, i: integer;' + LineEnding +
    'Begin' + LineEnding +
    '  d := 50; // Adjust as needed' + LineEnding +
    '  w := GraphicsWidth();' + LineEnding +
    '  h := GraphicsHeight();' + LineEnding +
    '  SetColor(255, 0, 0);' + LineEnding +
    '  For i := 1 To trunc(w Div d) Do Begin' + LineEnding +
    '    DrawLine(i * d, 0, i * d, h);' + LineEnding +
    '  End;' + LineEnding +
    '  For i := 1 To trunc(h Div d) Do Begin' + LineEnding +
    '    DrawLine(0, d * i, w, d * i);' + LineEnding +
    '  End;' + LineEnding +
    'End;';
  sl.SaveToFile(Path + 'grid.script');
  // Beispiel Ruler
  sl.text :=
    '(*' + LineEnding +
    ' * A local funktion' + LineEnding +
    ' *)' + LineEnding +
    '' + LineEnding +
    'Function _10_in_pixel(): Single;' + LineEnding +
    'Begin' + LineEnding +
    '  result := 1000 / GetScaling();' + LineEnding +
    'End;' + LineEnding +
    '' + LineEnding +
    '(*' + LineEnding +
    ' * Draw a ruler with a small mark each 10 units, and a big mark each 50 units' + LineEnding +
    ' *)' + LineEnding +
    '' + LineEnding +
    'Procedure OnRender();' + LineEnding +
    'Var' + LineEnding +
    '  f: Single;' + LineEnding +
    '  h, gw, gh, i: integer;' + LineEnding +
    'Begin' + LineEnding +
    '  f := _10_in_pixel();' + LineEnding +
    '  gw := GraphicsWidth;' + LineEnding +
    '  gh := GraphicsHeight;' + LineEnding +
    '' + LineEnding +
    '  SetColor(255, 0, 0);' + LineEnding +
    '' + LineEnding +
    '  // Draw the horizontal Ruler' + LineEnding +
    '  For i := 0 To (round(gw / f)) Do Begin' + LineEnding +
    '    If (i Mod 5 = 0) Then Begin' + LineEnding +
    '      h := round(f) * 2;' + LineEnding +
    '      SetLineWidth(4);' + LineEnding +
    '    End' + LineEnding +
    '    Else Begin' + LineEnding +
    '      h := round(f);' + LineEnding +
    '      SetLineWidth(2);' + LineEnding +
    '    End;' + LineEnding +
    '    DrawLine(round(i * f), gh, round(i * f), gh - h);' + LineEnding +
    '  End;' + LineEnding +
    '' + LineEnding +
    '  // Draw the vertical Ruler' + LineEnding +
    '  For i := 0 To (round(gh / f)) Do Begin' + LineEnding +
    '    If (i Mod 5 = 0) Then Begin' + LineEnding +
    '      h := round(f) * 2;' + LineEnding +
    '      SetLineWidth(4);' + LineEnding +
    '    End' + LineEnding +
    '    Else Begin' + LineEnding +
    '      h := round(f);' + LineEnding +
    '      SetLineWidth(2);' + LineEnding +
    '    End;' + LineEnding +
    '    DrawLine(0, gh - round(i * f), h, gh - round(i * f));' + LineEnding +
    '  End;' + LineEnding +
    'End;';
  sl.SaveToFile(Path + 'ruler.script');
  // Beispiel Text an Cursor Pos
  sl.text :=
    'Procedure DrawTextbox(x, y: integer; Text: String);' + LineEnding +
    'Var' + LineEnding +
    '  bx, by, w, h: integer;' + LineEnding +
    'Begin' + LineEnding +
    '  // Enlarge the Textblock a little bit' + LineEnding +
    '  bx := RelativeFromAbsoluteX(2, true);' + LineEnding +
    '  by := RelativeFromAbsoluteY(2, true);' + LineEnding +
    '  w := RelativeFromAbsoluteX(TextWidth(Text), true);' + LineEnding +
    '  h := RelativeFromAbsoluteY(TextHeight(Text), true);' + LineEnding +
    '  SetColor(0, 0, 0);' + LineEnding +
    '  SetFontColor(255, 255, 255);' + LineEnding +
    '  SetBrushColor(0, 0, 0);' + LineEnding +
    '  DrawFilledRectangle(x, y, x + w + 2 * bx, y + h + 2 * by);' + LineEnding +
    '  SetBrushColor(0, 0, 0);' + LineEnding +
    '  DrawText(x + bx, y + by, text);' + LineEnding +
    '  // Draw the Cursor' + LineEnding +
    '  SetColor(255, 0, 0);' + LineEnding +
    '  drawline(x, y - h, x, y + h);' + LineEnding +
    '  drawline(x - h, y, x + h, y);' + LineEnding +
    'End;' + LineEnding +
    '' + LineEnding +
    'Function _10_in_pixel(): Single;' + LineEnding +
    'Begin' + LineEnding +
    '  result := 1000 / GetScaling();' + LineEnding +
    'End;' + LineEnding +
    '' + LineEnding +
    '(*' + LineEnding +
    ' * Draws the Image Position right below the mouse Cursor' + LineEnding +
    ' *)' + LineEnding +
    '' + LineEnding +
    'Procedure OnRender();' + LineEnding +
    'Var' + LineEnding +
    '  gh, ix, iy, mx, my: Integer;' + LineEnding +
    '  Text: String;' + LineEnding +
    '  px, py, f: Single;' + LineEnding +
    'Begin' + LineEnding +
    '  // Get Absolute Mouse Position' + LineEnding +
    '  mx := GetMouseX();' + LineEnding +
    '  my := GetMouseY();' + LineEnding +
    '' + LineEnding +
    '  // Convert Mouse Position to Image Position' + LineEnding +
    '  ix := RelativeFromAbsoluteX(mx, false);' + LineEnding +
    '  iy := RelativeFromAbsoluteY(my, false);' + LineEnding +
    '' + LineEnding +
    '  // Calculate the Coordinates according the Image Metrik' + LineEnding +
    '  f := _10_in_pixel();' + LineEnding +
    '  gh := GraphicsHeight;' + LineEnding +
    '  px := ix / f;' + LineEnding +
    '  py := (gh - iy) / f;' + LineEnding +
    '' + LineEnding +
    '  // Create the Info' + LineEnding +
    '  Text :=' + LineEnding +
    '    // Pixel Coordinates' + LineEnding +
    '    inttostr(ix) + '' x '' + inttostr(iy)' + LineEnding +
    '    + '' = ''' + LineEnding +
    '    // Image Coordinates' + LineEnding +
    '    + floattostr(px, 1) + '' x '' + floattostr(py, 1);' + LineEnding +
    '' + LineEnding +
    '  // Draw it to the screen' + LineEnding +
    '  DrawTextbox(ix, iy, text);' + LineEnding +
    'End;';
  sl.SaveToFile(Path + 'mouse_position.script');
  // Beispiel Timestamp
  sl.text :=
    '(*' + LineEnding +
    ' * Draws a actual timestamp on the image' + LineEnding +
    ' *)' + LineEnding +
    'Procedure OnRender();' + LineEnding +
    'Begin' + LineEnding +
    '  SetFont(''Arial'', 30);' + LineEnding +
    '  SetFontColor(255, 0, 0);' + LineEnding +
    '  DrawText(10, 10, GetTimeStamp(''c''));' + LineEnding +
    'End;';
  sl.SaveToFile(Path + 'time_stamp.script');
End;

Procedure TForm1.ReCreateStretchBMP();
Var
  m: TMatrix3x3;
Begin
  fStretchbmp.Width := fImage.Width;
  fStretchbmp.Height := fImage.Height;
  fStretchbmp.Canvas.Draw(0, 0, fImage);
  m := IdentityMatrix3x3;
  m[0, 0] := br.x / fImage.Width;
  m[1, 1] := br.Y / fImage.Height;
  If (m[0, 0] < 1) Or (m[1, 1] < 1) Then Begin
    MulImage(fStretchbmp, m, imBilinear);
  End
  Else Begin
    If (m[0, 0] = 1) And (m[1, 1] = 1) Then Begin // 1:1 Skallierung
      fStretchbmp.Canvas.Draw(0, 0, fImage);
    End
    Else Begin
      //MulImage(fStretchbmp, m, imNearestNeighbour); // -- das geht zwar ist aber viel zu langsam: (
      fStretchbmp.Free;
      fStretchbmp := TBitmap.Create;
      fStretchbmp.Width := br.x;
      fStretchbmp.Height := br.y;
      StretchDrawBitmapToBitmap(fImage, fStretchbmp, br.x, br.y);
    End;
  End;
  RefreshHistogram();
End;

Procedure TForm1.Bevel7ChangeBounds(Sender: TObject);
Begin
  // Den Doppelclick des Bevels gibt es nicht das der zu Click führt
  // Der Gesuchte Code wurde händisch eingefügt und steht daher 6 Zeilen tiefer ;)
End;

Procedure TForm1.Bevel7Click(Sender: TObject);
Begin
  If Not assigned(fImage) Then exit;
  DisableAllManipulate_except(Bevel7);
  // Edit Kissenförmige Verzerrung
  If Bevel7.Style = bsRaised Then Begin
    Bevel7.Style := bsLowered;
    form3.CreateBackup();
    form3.resetSettings;
    form3.Show;
    SetFormSetPositionToBevel(form3, Bevel7);
  End
  Else Begin
    // Beim Beenden Rendern wir noch mal wie "Geleckt"
    form3.DoTheMagic(true);
    PushUndoImg(form3.FImageBackup);
    Bevel7.Style := bsRaised;
    form3.Hide;
  End;
  PaintBox1.Invalidate;
End;

Procedure TForm1.Bevel8ChangeBounds(Sender: TObject);
Begin
  // Den Doppelclick des Bevels gibt es nicht das der zu Click führt
  // Der Gesuchte Code wurde händisch eingefügt und steht daher 6 Zeilen tiefer ;)
End;

Procedure TForm1.Bevel8Click(Sender: TObject);
Begin
  If Not assigned(fImage) Then exit;
  DisableAllManipulate_except(Bevel8);
  // Edit Kissenförmige Verzerrung
  If Bevel8.Style = bsRaised Then Begin
    Bevel8.Style := bsLowered;
    form4.CreateBackup();
    form4.resetSettings;
    form4.Show;
    SetFormSetPositionToBevel(form4, Bevel8);
  End
  Else Begin
    // Beim Beenden Rendern wir noch mal wie "Geleckt"
    form4.DoTheMagic(true);
    PushUndoImg(form4.FImageBackup);
    Bevel8.Style := bsRaised;
    form4.Hide;
  End;
  PaintBox1.Invalidate;
End;

Procedure TForm1.Bevel9ChangeBounds(Sender: TObject);
Begin
  // Den Doppelclick des Bevels gibt es nicht das der zu Click führt
  // Der Gesuchte Code wurde händisch eingefügt und steht daher 6 Zeilen tiefer ;)
End;

Procedure TForm1.BitBtn10Click(Sender: TObject);
Var
  b: TBitmap;
  r: TRect;
  sh, sv, pw, ph, ci: integer;
  ft: TFileType;
Begin
  If SaveDialog2.Execute Then Begin

    ft := ftBMP;
    Case lowercase(ExtractFileExt(SaveDialog2.FileName)) Of
      '.png': ft := ftPNG;
      '.jpg': ft := ftJPG;
    End;
    If Not SaveImageAs(fImage, SaveDialog2.FileName, ft) Then Begin
      showmessage('Error unable to store: ' + SaveDialog2.FileName);
    End;
{$IFDEF adsdasd}
    Das hier geht leider nicht: (
      // Bakup Aller Relevanten Einstellungen
      sh := ScrollBar1.Position;
      sv := ScrollBar2.Position;
      ci := ComboBox1.ItemIndex;
      pw := PaintBox1.Width;
      ph := PaintBox1.Height;
      // Setzen auf 1:1 Scaling und Rendern
      ScrollBar1.Position := 0;
      ScrollBar2.Position := 0;
      ComboBox1.ItemIndex := View_Index_1_1;
      PaintBox1.Width := fImage.Width;
      PaintBox1.Height := fImage.Height;
      br := point(fImage.Width, fImage.Height);
      // render
      PaintBox1Paint(Nil);
      b := TBitmap.Create;
      b.Height := fImage.Height;
      b.Width := fImage.Width;
      r := rect(0, 0, fImage.Width, fImage.Height);
      b.Canvas.CopyRect(r, PaintBox1.Canvas, r);
      ft := ftBMP;
      Case lowercase(ExtractFileExt(SaveDialog2.FileName)) Of
        '.png': ft := ftPNG;
        '.jpg': ft := ftJPG;
  End;
  If Not SaveImageAs(b, SaveDialog2.FileName, ft) Then Begin
    showmessage('Error unable to store: ' + SaveDialog2.FileName);
  End;
  b.free;
  // Wieder Herstellen
  PaintBox1.Width := pw;
  PaintBox1.Height := ph;
  ComboBox1.ItemIndex := ci;
  ComboBox1.OnChange(Nil);
  ScrollBar1.Position := sh;
  ScrollBar2.Position := sv;
  PaintBox1Paint(Nil);
{$ENDIF}
End;
End;

Procedure TForm1.BitBtn11Click(Sender: TObject);
Begin
  If OpenDialog2.Execute Then Begin
    LoadProject(OpenDialog2.FileName);
  End;
End;

Procedure TForm1.BitBtn12Click(Sender: TObject);
Begin
  If Not assigned(fImage) Then Begin
    showmessage('No Image loaded, no project to store.');
    exit;
  End;
  If FileExists(fProjectFileName) And (fProjectFileName <> '') Then Begin
    SaveProject(fProjectFileName);
  End
  Else Begin
    BitBtn13.Click;
  End;
End;

Procedure TForm1.BitBtn13Click(Sender: TObject);
Begin
  If Not assigned(fImage) Then Begin
    showmessage('No Image loaded, no project to store.');
    exit;
  End;
  If SaveDialog1.Execute Then Begin
    SaveProject(SaveDialog1.FileName);
  End;
End;

Procedure TForm1.BitBtn14Click(Sender: TObject);
Var
  m: TMatrixNxM;
Begin
  If Not assigned(fImage) Then exit;
  PushUndoImg(fImage);
  DisableAllManipulate_except(Nil);
  // 5x5 Laplace Matrix entnommen aus ISBN: 978-3-540-21888-3, Seite 65
  m := MNxM(5,
    [
    0, 0, -1, 0, 0,
      0, -1, -2, -1, 0,
      -1, -2, 16, -2, -1,
      0, -1, -2, -1, 0,
      0, 0, -1, 0, 0
      ]);
  FoldImage(fImage, m, true);
  ReCreateStretchBMP();
  SetChanged();
  PaintBox1.Invalidate;
End;

Procedure TForm1.BitBtn15Click(Sender: TObject);
Var
  Activated: TStringList;
  i, j: Integer;
Begin
  // Skripte Bearbeiten
  form6.init(IncludeTrailingPathDelimiter(GetAppConfigDir(false)) + 'scripts');
  Activated := TStringList.Create;
  For i := 0 To high(fSkripts) Do Begin
    If fSkripts[i].Enabled Then Begin
      Activated.Add(fSkripts[i].Filename);
    End;
  End;
  form6.ShowModal;
  InitScripts();
  For i := 0 To Activated.Count - 1 Do Begin
    For j := 0 To high(fSkripts) Do Begin
      If fSkripts[j].Filename = Activated[i] Then Begin
        fSkripts[j].Enabled := true;
        break;
      End;
    End;
  End;
  CheckAndCreateAllSkripts();
  Activated.Free;
End;

Procedure TForm1.BitBtn16Click(Sender: TObject);
Begin
  // Scripte Aktivieren / Deaktivieren
  PopupMenu1.PopUp(form1.Left + BitBtn16.Left + 14, form1.Top + BitBtn16.top + 110);
End;

Procedure TForm1.BitBtn17Click(Sender: TObject);
Begin
  // Anzeige Berechnen Histogram
  If assigned(fImage) Then Begin
    form8.Show();
    RefreshHistogram();
  End;
End;

Procedure TForm1.BitBtn18Click(Sender: TObject);
Var
  img: Tbitmap;
Begin
  // Do Undo
  img := PopUndoImg();
  If assigned(img) Then Begin
    fImage.Assign(img);
    ReCreateStretchBMP();
    UpdateSizeInfo();
    img.free;
  End;
End;

Procedure TForm1.OnScriptClick(Sender: TObject);
Var
  item: TMenuItem;
  index: integer;
Begin
  // Der OnClick der Skripte
  item := sender As TMenuItem;
  item.Checked := Not item.Checked;
  index := item.Tag;
  fSkripts[index].Enabled := item.Checked;
  CheckAndCreateAllSkripts();
  PaintBox1.Invalidate;
End;

Procedure TForm1.CheckAndCreateAllSkripts();
Var
  i, j: Integer;
  sl: TStringList;
Begin
  For i := 0 To high(fSkripts) Do Begin
    // Ein Skript wurde Aktiviert
    If fSkripts[i].Enabled Then Begin
      If Not assigned(fSkripts[i].Interpreter) Then Begin
        fSkripts[i].Interpreter := NewInterpreter();
        sl := TStringList.Create;
        sl.LoadFromFile(fSkripts[i].Filename);
        If Not fSkripts[i].Interpreter.Compile(sl.Text) Then Begin // Wenn nicht Compiliert werden Kann wird das Skript gleich wieder deaktiviert.
          ShowMessage(fSkripts[i].Interpreter.Errormessage + LineEnding + LineEnding + 'Script will be deactivated.');
          fSkripts[i].Interpreter.Free;
          fSkripts[i].Interpreter := Nil;
          fSkripts[i].Enabled := false;
        End;
        sl.free;
      End;
    End
    Else Begin
      If assigned(fSkripts[i].Interpreter) Then Begin
        fSkripts[i].Interpreter.Free;
        fSkripts[i].Interpreter := Nil;
      End;
    End;
    For j := 0 To PopupMenu1.Items.Count - 1 Do Begin
      If PopupMenu1.Items[i].Tag = i Then Begin
        PopupMenu1.Items[i].Checked := fSkripts[i].Enabled;
      End;
    End;
  End;
End;

Function TForm1.NewInterpreter(): Tinterpreter;
Begin
  result := Tinterpreter.create;
  result.RegisterExternalCall('AbsoluteFromRelativeX', [vtint, vtbool], vtint, @Interpreter_AbsoluteFromRelativeX);
  result.RegisterExternalCall('AbsoluteFromRelativeY', [vtint, vtbool], vtint, @Interpreter_AbsoluteFromRelativeY);
  result.RegisterExternalCall('DrawCircle', [vtint, vtint, vtint, vtint], vtint, @Interpreter_DrawCircle);
  result.RegisterExternalCall('DrawFilledCircle', [vtint, vtint, vtint, vtint], vtint, @Interpreter_DrawFilledCircle);
  result.RegisterExternalCall('DrawFilledRectangle', [vtint, vtint, vtint, vtint], vtint, @Interpreter_DrawFilledRectangle);
  result.RegisterExternalCall('DrawLine', [vtint, vtint, vtint, vtint], vtint, @Interpreter_DrawLine);
  result.RegisterExternalCall('DrawText', [vtint, vtint, vtString], vtnone, @Interpreter_DrawText);
  result.RegisterExternalCall('FloatToStr', [vtfloat, vtint], vtString, @Interpreter_FloatToStr);
  result.RegisterExternalCall('GetMouseX', [], vtint, @Interpreter_GetMouseX);
  result.RegisterExternalCall('GetMouseY', [], vtint, @Interpreter_GetMouseY);
  result.RegisterExternalCall('GetScaling', [], vtfloat, @Interpreter_GetScaling);
  result.RegisterExternalCall('GetTimeStamp', [vtString], vtString, @Interpreter_GetTimeStamp);
  result.RegisterExternalCall('GraphicsHeight', [], vtint, @Interpreter_GraphicsHeight);
  result.RegisterExternalCall('GraphicsWidth', [], vtint, @Interpreter_GraphicsWidth);
  result.RegisterExternalCall('IntToStr', [vtint], vtString, @Interpreter_IntToStr);
  result.RegisterExternalCall('max', [vtint, vtint], vtint, @Interpreter_max);
  result.RegisterExternalCall('min', [vtint, vtint], vtint, @Interpreter_min);
  result.RegisterExternalCall('RelativeFromAbsoluteX', [vtint, vtbool], vtint, @Interpreter_RelativeFromAbsoluteX);
  result.RegisterExternalCall('RelativeFromAbsoluteY', [vtint, vtbool], vtint, @Interpreter_RelativeFromAbsoluteY);
  result.RegisterExternalCall('SetBrushColor', [vtint, vtint, vtint], vtint, @Interpreter_SetBrushColor);
  result.RegisterExternalCall('SetColor', [vtint, vtint, vtint], vtint, @Interpreter_SetColor);
  result.RegisterExternalCall('SetFont', [vtString, vtint], vtnone, @Interpreter_SetFont);
  result.RegisterExternalCall('SetFontColor', [vtint, vtint, vtint], vtint, @Interpreter_SetFontColor);
  result.RegisterExternalCall('SetLineWidth', [vtint], vtnone, @Interpreter_SetLineWidth);
  result.RegisterExternalCall('TextHeight', [vtString], vtint, @Interpreter_TextHeight);
  result.RegisterExternalCall('TextWidth', [vtString], vtint, @Interpreter_TextWidth);
End;

Function TForm1.Interpreter_GraphicsWidth(Const Params: Array Of String
  ): String;
Begin
  If assigned(fImage) Then Begin
    result := inttostr(fImage.Width);
  End
  Else Begin
    result := '0';
  End;
End;

Function TForm1.Interpreter_IntToStr(Const Params: Array Of String): String;
Begin
  result := Params[0];
End;

Function TForm1.Interpreter_FloatToStr(Const Params: Array Of String): String;
Var
  f: Single;
Begin
  f := StrToFloat(Params[0], forma);
  result := format('%0.' + Params[1] + 'f', [f]);
End;

Function TForm1.Interpreter_GraphicsHeight(Const Params: Array Of String
  ): String;
Begin
  If assigned(fImage) Then Begin
    result := inttostr(fImage.Height);
  End
  Else Begin
    result := '0';
  End;
End;

Function TForm1.Interpreter_GetScaling(Const Params: Array Of String): String;
Begin
  result := floattostr(fMetrik.PixelToDistance(100), forma);
End;

Function TForm1.Interpreter_GetTimeStamp(Const Params: Array Of String): String;
Begin
  Try
    result := FormatDateTime(params[0], now(), forma);
  Except
    result := 'Error invalid forumla: ' + params[0];
  End;
End;

Function TForm1.Interpreter_SetColor(Const Params: Array Of String): String;
Var
  r, g, b: Integer;
Begin
  result := '';
  r := clamp(strtointdef(Params[0], 0), 0, 255);
  g := clamp(strtointdef(Params[1], 0), 0, 255);
  b := clamp(strtointdef(Params[2], 0), 0, 255);
  PaintBox1.Canvas.Pen.Color := (b Shl 16) Or (g Shl 8) Or (r);
End;

Function TForm1.Interpreter_SetFont(Const Params: Array Of String): String;
Begin
  result := '';
  PaintBox1.Canvas.Font.Name := params[0];
  PaintBox1.Canvas.Font.Size := strtointdef(Params[1], 0);
End;

Function TForm1.Interpreter_SetFontColor(Const Params: Array Of String): String;
Var
  r, g, b: Integer;
Begin
  result := '';
  r := clamp(strtointdef(Params[0], 0), 0, 255);
  g := clamp(strtointdef(Params[1], 0), 0, 255);
  b := clamp(strtointdef(Params[2], 0), 0, 255);
  PaintBox1.Canvas.Font.Color := (b Shl 16) Or (g Shl 8) Or (r);
End;

Function TForm1.Interpreter_SetBrushColor(Const Params: Array Of String
  ): String;
Var
  r, g, b: Integer;
Begin
  result := '';
  r := clamp(strtointdef(Params[0], 0), 0, 255);
  g := clamp(strtointdef(Params[1], 0), 0, 255);
  b := clamp(strtointdef(Params[2], 0), 0, 255);
  PaintBox1.Canvas.Brush.Color := (b Shl 16) Or (g Shl 8) Or (r);
End;

Function TForm1.Interpreter_SetLineWidth(Const Params: Array Of String): String;
Var
  w: integer;
Begin
  result := '';
  w := clamp(strtointdef(Params[0], 0), 1, 255);
  PaintBox1.Canvas.Pen.Width := w;
End;

Function TForm1.Interpreter_TextHeight(Const Params: Array Of String): String;
//Var
//  p: Tpoint;
Begin
  //  p := ImageToGlobal(point(0, TextHeight(PaintBox1.Canvas, Params[0])));
  //  result := inttostr(p.y);
  result := inttostr(TextHeight(PaintBox1.Canvas, Params[0]));
End;

Function TForm1.Interpreter_TextWidth(Const Params: Array Of String): String;
Begin
  result := inttostr(TextWidth(PaintBox1.Canvas, Params[0]));
End;

Function TForm1.Interpreter_min(Const Params: Array Of String): String;
Var
  a, b: integer;
Begin
  a := strtointdef(Params[0], 0);
  b := strtointdef(Params[1], 0);
  result := inttostr(min(a, b));
End;

Function TForm1.Interpreter_RelativeFromAbsoluteX(Const Params: Array Of String
  ): String;
Var
  p: TPoint;
Begin
  p := point(strtointdef(Params[0], 0), 0);
  p := GlobalToImage(p, lowercase(params[1]) = '1');
  result := inttostr(p.x);
End;

Function TForm1.Interpreter_RelativeFromAbsoluteY(Const Params: Array Of String
  ): String;
Var
  p: TPoint;
Begin
  p := point(0, strtointdef(Params[0], 0));
  p := GlobalToImage(p, lowercase(params[1]) = '1');
  result := inttostr(p.Y);
End;

Function TForm1.Interpreter_AbsoluteFromRelativeX(Const Params: Array Of String
  ): String;
Var
  p: TPoint;
Begin
  p := point(strtointdef(Params[0], 0), 0);
  p := ImageToGlobal(p, lowercase(params[1]) = '1');
  result := inttostr(p.x);
End;

Function TForm1.Interpreter_AbsoluteFromRelativeY(Const Params: Array Of String
  ): String;
Var
  p: TPoint;
Begin
  p := point(0, strtointdef(Params[0], 0));
  p := ImageToGlobal(p, lowercase(params[1]) = '1');
  result := inttostr(p.Y);
End;

Function TForm1.Interpreter_max(Const Params: Array Of String): String;
Var
  a, b: integer;
Begin
  a := strtointdef(Params[0], 0);
  b := strtointdef(Params[1], 0);
  result := inttostr(max(a, b));
End;

Function TForm1.Interpreter_DrawLine(Const Params: Array Of String): String;
Var
  xs, ys, xe, ye: integer;
  p1, p2: TPoint;
Begin
  result := '';
  xs := strtointdef(Params[0], 0);
  ys := strtointdef(Params[1], 0);
  xe := strtointdef(Params[2], 0);
  ye := strtointdef(Params[3], 0);
  p1 := ImageToGlobal(point(xs, ys));
  p2 := ImageToGlobal(point(xe, ye));
  PaintBox1.Canvas.Line(p1.x, p1.y, p2.x, p2.y);
End;

Function TForm1.Interpreter_DrawText(Const Params: Array Of String): String;
Var
  xs, ys: integer;
  p1: TPoint;
Begin
  result := '';
  xs := strtointdef(Params[0], 0);
  ys := strtointdef(Params[1], 0);
  p1 := ImageToGlobal(point(xs, ys));
  PaintBox1.Canvas.Brush.Style := bsClear;
  PaintBox1.Canvas.TextOut(p1.x, p1.y, params[2]);
  PaintBox1.Canvas.Brush.Style := bsSolid;
End;

Function TForm1.Interpreter_GetMouseX(Const Params: Array Of String): String;
Begin
  result := inttostr(MouseAbsolute.x);
End;

Function TForm1.Interpreter_GetMouseY(Const Params: Array Of String): String;
Begin
  result := inttostr(MouseAbsolute.Y);
End;

Function TForm1.Interpreter_DrawCircle(Const Params: Array Of String): String;
Var
  xs, ys, xe, ye: integer;
  bs: TFPBrushStyle;
  p1, p2: TPoint;
Begin
  result := '';
  xs := strtointdef(Params[0], 0);
  ys := strtointdef(Params[1], 0);
  xe := strtointdef(Params[2], 0);
  ye := strtointdef(Params[3], 0);
  p1 := ImageToGlobal(point(xs, ys));
  p2 := ImageToGlobal(point(xe, ye));
  bs := PaintBox1.Canvas.Brush.Style;
  PaintBox1.Canvas.Brush.Style := bsClear;
  PaintBox1.Canvas.Ellipse(p1.x, p1.y, p2.x, p2.y);
  PaintBox1.Canvas.Brush.Style := bs;
End;

Function TForm1.Interpreter_DrawFilledCircle(Const Params: Array Of String
  ): String;
Var
  xs, ys, xe, ye: integer;
  bs: TFPBrushStyle;
  p1, p2: TPoint;
Begin
  result := '';
  xs := strtointdef(Params[0], 0);
  ys := strtointdef(Params[1], 0);
  xe := strtointdef(Params[2], 0);
  ye := strtointdef(Params[3], 0);
  p1 := ImageToGlobal(point(xs, ys));
  p2 := ImageToGlobal(point(xe, ye));
  bs := PaintBox1.Canvas.Brush.Style;
  PaintBox1.Canvas.Brush.Style := bsSolid;
  PaintBox1.Canvas.Ellipse(p1.x, p1.y, p2.x, p2.y);
  PaintBox1.Canvas.Brush.Style := bs;
End;

Function TForm1.Interpreter_DrawFilledRectangle(Const Params: Array Of String
  ): String;
Var
  xs, ys, xe, ye: integer;
  bs: TFPBrushStyle;
  p1, p2: TPoint;
Begin
  result := '';
  xs := strtointdef(Params[0], 0);
  ys := strtointdef(Params[1], 0);
  xe := strtointdef(Params[2], 0);
  ye := strtointdef(Params[3], 0);
  p1 := ImageToGlobal(point(xs, ys));
  p2 := ImageToGlobal(point(xe, ye));
  bs := PaintBox1.Canvas.Brush.Style;
  PaintBox1.Canvas.Brush.Style := bsSolid;
  PaintBox1.Canvas.Rectangle(p1.x, p1.y, p2.x, p2.y);
  PaintBox1.Canvas.Brush.Style := bs;
End;

Procedure TForm1.Bevel9Click(Sender: TObject);
Var
  pa: TPointArray;
Begin
  If Not assigned(fImage) Then exit;
  DisableAllView_Except(Bevel9);
  If Bevel9.Style = bsRaised Then Begin
    Bevel9.Style := bsLowered;
    button7.visible := true;
    setlength(pa, 2);

    pa[0] := GlobalToImage(point(PaintBox1.Width Div 4, PaintBox1.Height Div 2));
    pa[1] := GlobalToImage(point((PaintBox1.Width * 3) Div 4, PaintBox1.Height Div 2));

    fRotateCorrection.SetPoints(pa);
    fRotateCorrection.Visible := true;
  End
  Else Begin
    Bevel9.Style := bsRaised;
    button7.visible := false;
    fRotateCorrection.Visible := false;
  End;
  PaintBox1.Invalidate;
End;

Function TForm1.AskChange(): Boolean;
Begin
  If fChanged Then Begin
    If ID_YES = application.MessageBox('You did some changes, they get lost if you proceed now, do you want to continue?', 'Warning', MB_YESNO Or MB_ICONQUESTION) Then Begin
      result := true;
    End
    Else Begin
      result := false;
    End;
  End
  Else Begin
    result := true;
  End;
End;

Procedure TForm1.PushUndoObj(Const Obj: TMeasureElement);
Var
  i: Integer;
Begin
  // Umschalten der LCL
  BitBtn21.ImageIndex := 4;
  BitBtn21.Enabled := true;
  // Push des Obj in den Stack
  fUndoObj[fUndoObjCnt] := obj;
  inc(fUndoObjCnt);
  // Der "Stack" ist voll -> ältestes Element löschen
  If fUndoObjCnt = length(fUndoObj) Then Begin
    fUndoObj[0].free;
    For i := 0 To high(fUndoObj) - 1 Do Begin
      fUndoObj[i] := fUndoObj[i + 1];
    End;
    fUndoObj[high(fUndoObj)] := Nil;
    dec(fUndoObjCnt);
  End;
End;

Function TForm1.PopUndoObj(): TMeasureElement;
Begin
  result := Nil;
  // Wenn es noch was auf dem Stack gibt, geben wir es raus
  If fUndoObjCnt > 0 Then Begin
    dec(fUndoObjCnt);
    result := fUndoObj[fUndoObjCnt];
  End;
  // Der Stack ist Leer die LCL ausschalten
  If fUndoObjCnt = 0 Then Begin
    BitBtn21.ImageIndex := 3;
    BitBtn21.Enabled := false;
  End;
End;

Procedure TForm1.ClearObjUndoStack();
Var
  obj: TMeasureElement;
Begin
  obj := PopUndoObj();
  While assigned(obj) Do Begin
    obj.free;
    obj := PopUndoObj();
  End;
End;

Procedure TForm1.SpeedButton13Click(Sender: TObject);
Begin
  Disable_All_Measurement_Elements();
  PaintBox1.Cursor := crCross;
End;

Procedure TForm1.SpeedButton1Click(Sender: TObject);
Begin
  Disable_All_Measurement_Elements();
End;

Function TForm1.GlobalToImage(P: Tpoint; Relative: Boolean): TPoint;
Begin
  If Not assigned(fImage) Then Begin
    // TODO: ka was hier Clever ist ...
    result := p;
    exit;
  End;
  Case ComboBox1.ItemIndex Of
    View_Index_Fill: Begin
        // Hier gibt es keine Relativ Auswertung, weil Fill ja genau auf die Paintbox hin skalliert
        result.x := round(convert_dimension(0, PaintBox1.Width, p.x, 0, fImage.Width));
        result.Y := round(convert_dimension(0, PaintBox1.Height, p.y, 0, fImage.Height));
      End;
    View_Index_8_1: Begin
        If relative Then Begin
          result.x := p.x Div 8;
          result.Y := p.Y Div 8;
        End
        Else Begin
          result.x := (p.x + ScrollBar1.Position) Div 8;
          result.Y := (p.Y + ScrollBar2.Position) Div 8;
        End;
      End;
    View_Index_4_1: Begin
        If relative Then Begin
          result.x := p.x Div 4;
          result.Y := p.Y Div 4;
        End
        Else Begin
          result.x := (p.x + ScrollBar1.Position) Div 4;
          result.Y := (p.Y + ScrollBar2.Position) Div 4;
        End;
      End;
    View_Index_2_1: Begin
        If relative Then Begin
          result.x := p.x Div 2;
          result.Y := p.Y Div 2;
        End
        Else Begin
          result.x := (p.x + ScrollBar1.Position) Div 2;
          result.Y := (p.Y + ScrollBar2.Position) Div 2;
        End;
      End;
    View_Index_1_1: Begin
        If relative Then Begin
          result.x := p.x;
          result.Y := p.Y;
        End
        Else Begin
          result.x := p.x + ScrollBar1.Position;
          result.Y := p.Y + ScrollBar2.Position;
        End;
      End;
    View_Index_1_2: Begin
        If relative Then Begin
          result.x := p.x * 2;
          result.Y := p.Y * 2;
        End
        Else Begin
          result.x := (p.x + ScrollBar1.Position) * 2;
          result.Y := (p.Y + ScrollBar2.Position) * 2;
        End;
      End;
    View_Index_1_4: Begin
        If relative Then Begin
          result.x := p.x * 4;
          result.Y := p.Y * 4;
        End
        Else Begin
          result.x := (p.x + ScrollBar1.Position) * 4;
          result.Y := (p.Y + ScrollBar2.Position) * 4;
        End;
      End;
    View_Index_1_8: Begin
        If relative Then Begin
          result.x := p.x * 8;
          result.Y := p.Y * 8;
        End
        Else Begin
          result.x := (p.x + ScrollBar1.Position) * 8;
          result.Y := (p.Y + ScrollBar2.Position) * 8;
        End;
      End;
  Else Begin
      Raise Exception.Create('TForm1.GlobalToImage: Missing implementation for "' + ComboBox1.Text + '"');
    End;
  End;
End;

Procedure TForm1.Disable_All_Measurement_Elements();
Var
  i: integer;
Begin
  PaintBox1.PopupMenu := Nil;
  PaintBox1.Cursor := crDefault;
  For i := 0 To high(fMeasureElements) Do Begin
    fMeasureElements[i].Enabled := false;
  End;
  PaintBox1.Invalidate;
End;

Function TForm1.ImageToGlobal(P: Tpoint; Relative: Boolean): TPoint;
Begin
  If Not assigned(fImage) Then Begin
    // TODO: ka was hier Clever ist ...
    result := p;
    exit;
  End;
  Case ComboBox1.ItemIndex Of
    View_Index_Fill: Begin
        // Hier gibt es keine Relativ Auswertung, weil Fill ja genau auf die Paintbox hin skalliert
        result.x := round(convert_dimension(0, fImage.Width, p.x, 0, PaintBox1.Width));
        result.Y := round(convert_dimension(0, fImage.Height, p.y, 0, PaintBox1.Height));
      End;
    View_Index_8_1: Begin
        If relative Then Begin
          result.x := p.x * 8;
          result.Y := p.Y * 8;
        End
        Else Begin
          result.x := (p.x * 8) - ScrollBar1.Position;
          result.Y := (p.Y * 8) - ScrollBar2.Position;
        End;
      End;
    View_Index_4_1: Begin
        If relative Then Begin
          result.x := p.x * 4;
          result.Y := p.Y * 4;
        End
        Else Begin
          result.x := (p.x * 4) - ScrollBar1.Position;
          result.Y := (p.Y * 4) - ScrollBar2.Position;
        End;
      End;
    View_Index_2_1: Begin
        If relative Then Begin
          result.x := p.x * 2;
          result.Y := p.Y * 2;
        End
        Else Begin
          result.x := (p.x * 2) - ScrollBar1.Position;
          result.Y := (p.Y * 2) - ScrollBar2.Position;
        End;
      End;
    View_Index_1_1: Begin
        If Relative Then Begin
          result.x := p.x;
          result.Y := p.Y;
        End
        Else Begin
          result.x := p.x - ScrollBar1.Position;
          result.Y := p.Y - ScrollBar2.Position;
        End;
      End;
    View_Index_1_2: Begin
        If relative Then Begin
          result.x := p.x Div 2;
          result.Y := p.Y Div 2;
        End
        Else Begin
          result.x := (p.x Div 2) - ScrollBar1.Position;
          result.Y := (p.Y Div 2) - ScrollBar2.Position;
        End;
      End;
    View_Index_1_4: Begin
        If relative Then Begin
          result.x := p.x Div 4;
          result.Y := p.Y Div 4;
        End
        Else Begin
          result.x := (p.x Div 4) - ScrollBar1.Position;
          result.Y := (p.Y Div 4) - ScrollBar2.Position;
        End;
      End;
    View_Index_1_8: Begin
        If relative Then Begin
          result.x := p.x Div 8;
          result.Y := p.Y Div 8;
        End
        Else Begin
          result.x := (p.x Div 8) - ScrollBar1.Position;
          result.Y := (p.Y Div 8) - ScrollBar2.Position;
        End;
      End;
  Else Begin
      Raise Exception.Create('TForm1.ImageToGlobal: Missing implementation for "' + ComboBox1.Text + '"');
    End;
  End;
End;

Procedure TForm1.RenderLegend();
Var
  s: String;
  p: TPoint;
Begin
  (* Rendert eine 90 Pixel Lange Strecke*)
  p := GlobalToImage(point(90, 0), True); // 90 Pixel umrechnen in Bild Koordinaten
  PaintBox1.Canvas.Pen.Color := clblack;
  PaintBox1.Canvas.Brush.Color := clwhite;
  PaintBox1.Canvas.Rectangle(10, PaintBox1.Height - 50, 110, PaintBox1.Height - 10);
  s := format('%0.1f ', [fMetrik.PixelToDistance(p.x)]) + GetPixelUnit();
  PaintBox1.Canvas.Font.Color := clblack;
  PaintBox1.Canvas.TextOut(60 - PaintBox1.Canvas.TextWidth(s) Div 2, PaintBox1.Height - 45, s);
  PaintBox1.Canvas.Pen.Width := 3;
  PaintBox1.Canvas.Line(15, PaintBox1.Height - 33, 15, PaintBox1.Height - 17);
  PaintBox1.Canvas.Line(15, PaintBox1.Height - 25, 105, PaintBox1.Height - 25);
  PaintBox1.Canvas.Line(105, PaintBox1.Height - 33, 105, PaintBox1.Height - 17);
  PaintBox1.Canvas.Pen.Width := 1;
End;

Procedure TForm1.ResetMetrik;
Begin
  ComboBox2.Text := 'mm';
  If assigned(fImage) Then Begin
    fMetrik.SetPoints([point(fImage.Width Div 4, fImage.Height Div 2), point((fImage.Width * 3) Div 4, fImage.Height Div 2)]);
    fMetrik.Distance := 100;
  End
  Else Begin
    fMetrik.SetPoints([point(PaintBox1.Width Div 4, PaintBox1.Height Div 2), point((PaintBox1.Width * 3) Div 4, PaintBox1.Height Div 2)]);
    fMetrik.Distance := 100;
  End;
  fMetrik.Visible := Bevel1.Style = bsLowered;
  Edit1.Text := format('%0.1f', [fMetrik.Distance]);
End;

Procedure TForm1.OnMetrikChange(Sender: TObject);
Begin
  // Aktualisieren aller Elemente damit deren Flächen / Längen neu berechnet werden
  ComboBox2Change(Nil);
  SetChanged();
  If assigned(form12) And form12.Visible Then Begin
    form12.RefreshData;
  End;
End;

Procedure TForm1.Clear;
Var
  i: Integer;
Begin
  If assigned(fImage) Then fImage.Free;
  fImage := Nil;
  For i := 0 To high(fMeasureElements) Do Begin
    fMeasureElements[i].Free;
  End;
  setlength(fMeasureElements, 0);
  fMetrik.free;
  fMetrik := Nil;
  ScrollBar1.Position := 0;
  ScrollBar2.Position := 0;
End;

Procedure TForm1.SetChanged();
Begin
  fChanged := true;
  // TODO: Caption Anpassen
End;

Procedure TForm1.SetFormSetPositionToBevel(Const frm: TForm; Const Obj: TBevel);
Begin
  frm.Top := Form1.Top + 125;
  frm.Left := form1.Left + Obj.Left + 19;
End;

Procedure TForm1.Button4Click(Sender: TObject);
Var
  P: TPointArray;
Begin
  // Do the Perspektive Korrection
  PushUndoImg(fImage);
  p := fPerspectiveCorrection.GetPoints();
  CorrectProjection(fImage, p[0], p[1], p[2], p[3], imBilinear, wmBlack, CheckBox3.Checked);
  Bevel5Click(Nil); // Wieder Ausschalten als Bestätigung
  ReCreateStretchBMP();
  SetChanged();
  UpdateSizeInfo();
  PaintBox1.Invalidate;
End;

Procedure TForm1.Button6Click(Sender: TObject);
Const
  (*
   * Alle Winkel zwischen den beiden Strecken die Kleiner sind werden abgewiesen
   * => Dieser Wert ist empirisch bestimmt
   *)
  toSmalAngle = 25;

Var
  P: TPointArray;
  v1, v2: TVector2;
  lr1, lr2: Single;
  a, px, py: Single; //dy, dx: Single;
  m: tmatrix3x3;
  sm: TMatrixNxM;
  pa: TPointArray;
Begin
  // Do the Height Equalization
  p := fHeightCorrection.GetPoints();
  v1 := p[1] - p[0];
  v2 := p[3] - p[2];
  a := AngleV2(v1, v2);
  If (a < toSmalAngle) Or (a > 180 - toSmalAngle) Then Begin
    showmessage('Error, angle to small to get a good scaling, please choose a wider angle.');
    exit;
  End;
  lr1 := strtofloatdef(trim(fHeightCorrection.Texts[0]), -1);
  lr2 := strtofloatdef(trim(fHeightCorrection.Texts[1]), -1);
  If (lr1 <= 0) Or (lr2 <= 0) Then Begin
    showmessage('Error, invalid value for distances.');
    exit;
  End;
  PushUndoImg(fImage);
  (*
   * Init the matrix to solve:  (The equations is sponsored by Julian Bauknecht ;) )
   *
   * sqrt( (px*v1.x)^2 + (py*v1.y)^2 ) = lr1
   * sqrt( (px*v2.x)^2 + (py*v2.y)^2 ) = lr2
   *
   * px, py = Scaling for x / y achsis to fitt lr1 and lr2
   * v* = corresponding vectors in Image
   * lr* = given vector Length by user
   *)
  sm := ZeroNxM(3, 2);
  sm[0, 0] := sqr(v1.x);
  sm[1, 0] := sqr(v1.y);
  sm[2, 0] := sqr(lr1);
  sm[0, 1] := sqr(v2.x);
  sm[1, 1] := sqr(v2.y);
  sm[2, 1] := sqr(lr2);
  GaussJordan(sm);
  px := sqrt(sm[2, 0]);
  py := sqrt(sm[2, 1]);
  m := IdentityMatrix3x3;
  m[1, 1] := py / px;
  MulImage(fImage, m, imBilinear);
  setlength(pa, 2);
  pa[0] := p[2];
  pa[1] := p[2];
  pa[1].x := round(p[2].x + v2.x);
  fMetrik.SetPoints(pa);
  fMetrik.Distance := v2.x * px;
  edit1.text := Format('%0.1f', [fMetrik.Distance]);
  fMetrik.Visible := false;
  Bevel6Click(Nil); // Wieder Ausschalten als Bestätigung
  UpdateSizeInfo();
  ReCreateStretchBMP();
  SetChanged();
  PaintBox1.Invalidate;
End;

Procedure TForm1.Button7Click(Sender: TObject);
Var
  P: TPointArray;
  m2: TMatrix2x2;
  m: TMatrix3x3;
  angle: Single;
  i, j: Integer;
Begin
  PushUndoImg(fImage);
  // Rotation Correction Tool
  p := fRotateCorrection.GetPoints();
  angle := AngleV2_2(P[1] - p[0], v2(1, 0));
  m2 := CalculateRotationMatrix(-angle);
  m := IdentityMatrix3x3;
  For i := 0 To 1 Do
    For j := 0 To 1 Do
      m[i, j] := m2[i, j];
  MulImage(fImage, m, imBilinear);
  Bevel9Click(Nil); // Wieder Ausschalten als Bestätigung
  UpdateSizeInfo();
  ReCreateStretchBMP();
  SetChanged();
  PaintBox1.Invalidate;
End;

Procedure TForm1.Button8Click(Sender: TObject);
Var
  tmp: TBitmap;
  pts: TPointArray;
  d: TVector2;
Begin
  // Cut
  If Not assigned(fImage) Then exit;
  PushUndoImg(fImage);
  tmp := TBitmap.Create;
  pts := fCutTool.GetPoints();
  d := pts[1] - pts[0];
  tmp.Width := round(abs(d.x));
  tmp.Height := round(abs(d.y));
  tmp.Canvas.Draw(-min(pts[0].X, pts[1].x), -min(pts[0].Y, pts[1].Y), fImage);
  fImage.Assign(tmp);
  tmp.free;
  UpdateSizeInfo();
  ReCreateStretchBMP();
  SetChanged();
  Bevel19Click(Nil); // Wieder Ausschalten als Bestätigung
  PaintBox1.Invalidate;
End;

Procedure TForm1.CheckBox2Click(Sender: TObject);
Var
  i: Integer;
Begin
  For i := 0 To high(fMeasureElements) Do Begin
    fMeasureElements[i].TextVisible := CheckBox2.Checked;
  End;
  PaintBox1.Invalidate;
End;

Procedure TForm1.BitBtn1Click(Sender: TObject);
Begin
  // Rotate Left
  If Not assigned(fImage) Then exit;
  PushUndoImg(fImage);
  RotateCounterClockWise90Degrees(fImage);
  UpdateSizeInfo();
  ReCreateStretchBMP();
  SetChanged();
  PaintBox1.Invalidate;
End;

Procedure TForm1.BitBtn20Click(Sender: TObject);
Var
  ol, uo, i: integer;
Begin
  form14.CheckBox1.Checked := GetValue('General', 'RememberLast', '1') = '1';
  form14.Edit1.Text := inttostr(strtointdef(GetValue('General', 'MaxUndoImg', '10'), 10));
  form14.Edit2.Text := inttostr(strtointdef(GetValue('General', 'MaxUndoObj', '100'), 100));
  If form14.ShowModal = mrOK Then Begin
    SetValue('General', 'RememberLast', inttostr(ord(form14.CheckBox1.Checked)));
    // undo Objects
    uo := strtointdef(Form14.Edit2.Text, -1);
    If uo > 0 Then Begin
      // Die Stack Länge hat sich geändert
      If uo <> length(fUndoObj) Then Begin
        If uo > length(fUndoObj) Then Begin // Der Leichte Fall, der Stack wird größer
          ol := length(fUndoObj);
          setlength(fUndoObj, uo);
          For i := ol To uo - 1 Do Begin
            fUndoObj[i] := Nil;
          End;
        End
        Else Begin // Der Stack wird kleiner
          If uo > fUndoObjCnt Then Begin // Der Stack wird zwar gekürtzt, läuft aber nicht über
            setlength(fUndoObj, uo);
          End
          Else Begin
            // Der Gekürzte Stack läuft direkt über
            While fUndoObjCnt >= uo Do Begin
              fUndoObj[0].free;
              For i := 0 To high(fUndoObj) - 1 Do Begin
                fUndoObj[i] := fUndoObj[i + 1];
              End;
              fUndoObj[high(fUndoObj)] := Nil;
              dec(fUndoObjCnt);
            End;
            setlength(fUndoObj, uo);
          End;
        End;
      End;
      SetValue('General', 'MaxUndoObj', inttostr(uo));
    End;
    // Undo Images
    uo := strtointdef(Form14.Edit1.Text, -1);
    If uo > 0 Then Begin
      // Die Stack Länge hat sich geändert
      If uo <> length(FUndoImg) Then Begin
        If uo > length(FUndoImg) Then Begin // Der Leichte Fall, der Stack wird größer
          ol := length(FUndoImg);
          setlength(FUndoImg, uo);
          For i := ol To uo - 1 Do Begin
            FUndoImg[i] := Nil;
          End;
        End
        Else Begin // Der Stack wird kleiner
          If uo > fUndoImgCnt Then Begin // Der Stack wird zwar gekürtzt, läuft aber nicht über
            setlength(FUndoImg, uo);
          End
          Else Begin
            // Der Gekürzte Stack läuft direkt über
            While fUndoImgCnt >= uo Do Begin
              FUndoImg[0].free;
              For i := 0 To high(FUndoImg) - 1 Do Begin
                FUndoImg[i] := FUndoImg[i + 1];
              End;
              FUndoImg[high(FUndoImg)] := Nil;
              dec(fUndoImgCnt);
            End;
            setlength(FUndoImg, uo);
          End;
        End;
      End;
      SetValue('General', 'MaxUndoImg', inttostr(uo));
    End;
  End;
End;

Procedure TForm1.BitBtn21Click(Sender: TObject);
Var
  obj: TMeasureElement;
Begin
  // Undo Delete
  If assigned(fUndoObj) Then Begin
    obj := PopUndoObj();
    If assigned(obj) Then Begin
      setlength(fMeasureElements, high(fMeasureElements) + 2);
      fMeasureElements[high(fMeasureElements)] := Obj;
    End;
    PaintBox1.Invalidate;
  End;
End;

Procedure TForm1.BitBtn2Click(Sender: TObject);
Begin
  // Rotate Right
  If Not assigned(fImage) Then exit;
  PushUndoImg(fImage);
  RotateClockWise90Degrees(fImage);
  UpdateSizeInfo();
  ReCreateStretchBMP();
  SetChanged();
  PaintBox1.Invalidate;
End;

Procedure TForm1.BitBtn3Click(Sender: TObject);
Begin
  // Left To Right
  If Not assigned(fImage) Then exit;
  PushUndoImg(fImage);
  ;
  LeftToRight(fImage);
  ReCreateStretchBMP();
  SetChanged();
  PaintBox1.Invalidate;
End;

Procedure TForm1.BitBtn4Click(Sender: TObject);
Begin
  // Upside Down
  If Not assigned(fImage) Then exit;
  PushUndoImg(fImage);
  UpSideDown(fImage);
  ReCreateStretchBMP();
  SetChanged();
  PaintBox1.Invalidate;
End;

Procedure TForm1.BitBtn5Click(Sender: TObject);
Var
  i: Integer;
Begin
  // Select Font for Paintbox
  FontDialog1.Font := PaintBox1.Font;
  If FontDialog1.Execute Then Begin
    PaintBox1.Font := FontDialog1.Font;
    For i := 0 To high(fMeasureElements) Do Begin
      fMeasureElements[i].RefreshText();
    End;
    PaintBox1.Invalidate;
  End;
End;

Procedure TForm1.BitBtn6Click(Sender: TObject);
Begin
  // Show Table
  form12.Show;
  form12.RefreshData;
End;

Procedure TForm1.BitBtn7Click(Sender: TObject);
Var
  sl: TStringList;
  s: String;
  j, i: Integer;
Begin
  // CSV Export
  If SaveDialog3.Execute Then Begin
    form12.RefreshData;
    // Alle Daten sind gesammelt -> Speichern
    sl := TStringList.Create;
    For j := 0 To Form12.StringGrid1.RowCount - 1 Do Begin
      s := '';
      For i := 0 To form12.StringGrid1.ColCount - 1 Do Begin
        If i = 0 Then Begin
          s := form12.StringGrid1.Cells[i, j];
        End
        Else Begin
          s := s + ';' + form12.StringGrid1.Cells[i, j];
        End;
      End;
      sl.add(s);
    End;
    // Sicherstellen das der UTF8 BOM enthalten ist, sonst macht excel mucken
    sl.Text := utf8toutf8bom(sl.text);
    sl.SaveToFile(SaveDialog3.FileName);
    sl.free;
  End;
End;

Procedure TForm1.BitBtn8Click(Sender: TObject);
Begin
  close;
End;

Procedure TForm1.BitBtn9Click(Sender: TObject);
Begin
  // TODO: If Changed dann erst mal Speichern ..
  If OpenDialog1.Execute Then Begin
    LoadImage(OpenDialog1.FileName);
  End;
End;

Procedure TForm1.ComboBox1Change(Sender: TObject);
  Procedure Calc_Scrollbars();
  Begin
    If br.X > PaintBox1.Width Then Begin
      ScrollBar1.Max := br.X - PaintBox1.Width;
      ScrollBar1.Visible := true;
    End
    Else Begin
      ScrollBar1.Position := 0;
      ScrollBar1.Visible := false;
    End;
    If br.Y > PaintBox1.Height Then Begin
      ScrollBar2.Max := br.Y - PaintBox1.Height;
      ScrollBar2.Visible := true;
    End
    Else Begin
      ScrollBar2.Position := 0;
      ScrollBar2.Visible := false;
    End;
  End;

Begin
  // Change View
  If Not assigned(fImage) Then exit;
  Case ComboBox1.ItemIndex Of
    //    View_Index_Fit: Begin
    //        // Wir skallieren das Größere auf die Paintbox und das kleinere Propertional
    //        If fImage.Width > fImage.Height Then Begin
    //        End
    //        Else Begin
    //        End;
    //        Calc_Scrollbars();
    //      End;
    View_Index_Fill: Begin
        ScrollBar1.Visible := false;
        ScrollBar1.Position := 0;
        ScrollBar2.Visible := false;
        ScrollBar2.Position := 0;
        br := point(PaintBox1.Width, PaintBox1.Height);
      End;
    View_Index_8_1: Begin
        br := point(fImage.Width * 8, fImage.Height * 8);
        Calc_Scrollbars();
      End;
    View_Index_4_1: Begin
        br := point(fImage.Width * 4, fImage.Height * 4);
        Calc_Scrollbars();
      End;
    View_Index_2_1: Begin
        br := point(fImage.Width * 2, fImage.Height * 2);
        Calc_Scrollbars();
      End;
    View_Index_1_1: Begin
        br := point(fImage.Width, fImage.Height);
        Calc_Scrollbars();
      End;
    View_Index_1_2: Begin
        br := point(fImage.Width Div 2, fImage.Height Div 2);
        Calc_Scrollbars();
      End;
    View_Index_1_4: Begin
        br := point(fImage.Width Div 4, fImage.Height Div 4);
        Calc_Scrollbars();
      End;
    View_Index_1_8: Begin
        br := point(fImage.Width Div 8, fImage.Height Div 8);
        Calc_Scrollbars();
      End;
  End;
  ReCreateStretchBMP;
  PaintBox1.Invalidate;
End;

Procedure TForm1.ComboBox2Change(Sender: TObject);
Var
  i: integer;
Begin
  For i := 0 To high(fMeasureElements) Do Begin
    fMeasureElements[i].RefreshText();
  End;
  SetChanged();
  PaintBox1.Invalidate;
End;

Procedure TForm1.Edit1Change(Sender: TObject);
Begin
  fMetrik.Distance := strtofloatdef(edit1.text, 100); // Hier kein Def
  SetChanged();
  PaintBox1.Invalidate;
End;

Procedure TForm1.FormChangeBounds(Sender: TObject);
Begin
  If form3.visible Then Begin
    SetFormSetPositionToBevel(form3, Bevel7);

  End;
  If form4.visible Then Begin
    SetFormSetPositionToBevel(form4, Bevel8);
  End;
End;

Procedure TForm1.LoadImage(Filename: String);
Var
  jp: TJPEGImage;
  p: TPortableNetworkGraphic;
  i: Integer;
Begin
  If Not FileExists(Filename) Then Begin
    showmessage('File: ' + Filename + ' does not exist.');
    exit;
  End;
  If Not AskChange() Then exit;
  ClearImgUndoStack();
  ClearObjUndoStack();
  DisableAllManipulate_except(Nil);
  DisableAllView_Except(Nil);
  SetValue('General', 'LastFile', Filename);
  If assigned(fImage) Then fImage.Free;
  fProjectFileName := ''; // Ein neues Bild Laden macht automatisch ein neues Project
  Case GetFileTypeFromFile(Filename) Of
    ftPNG: Begin
        p := TPortableNetworkGraphic.Create;
        p.LoadFromFile(Filename);
        fImage := TBitmap.Create;
        fImage.Assign(p);
        p.free;
      End;
    ftBMP: Begin
        fImage := TBitmap.Create;
        fImage.LoadFromFile(Filename);
      End;
    ftJPG: Begin
        jp := TJPEGImage.Create;
        jp.LoadFromFile(Filename);
        fImage := TBitmap.Create;
        fImage.Assign(jp);
        jp.free;
      End
  Else Begin
      Raise Exception.create('Unknown fileformat.');
    End;
  End;
  label4.visible := false;
  For i := 0 To high(fMeasureElements) Do Begin
    fMeasureElements[i].free;
  End;
  setlength(fMeasureElements, 0);
  StatusBar1.Panels[Panel_index_Filename].Text := ExtractFileName(Filename);
  ComboBox1.ItemIndex := View_Index_1_1;
  ComboBox1Change(Nil);
  UpdateSizeInfo();
  fChanged := false;
  PaintBox1.Invalidate;
End;

Procedure TForm1.SaveProject(Filename: String);
Var
  f: TFileStream;
  i: integer;
  j: Int32;
Begin
  SetValue('General', 'LastFile', Filename);
  fProjectFileName := Filename;
  f := TFileStream.Create(Filename, fmCreate Or fmOpenWrite);
  // Die Version
  f.write(ImageInspectorFileVersion, sizeof(ImageInspectorFileVersion));
  // Das Bild
  fImage.SaveToStream(f);
  // Die Metrik
  fMetrik.SaveToStream(f);
  // Die Measure Elemente
  j := length(fMeasureElements);
  f.Write(j, sizeof(j));
  For i := 0 To high(fMeasureElements) Do Begin
    fMeasureElements[i].SaveToStream(f);
  End;
  // Einstellungen der LCL ?
  // Show Legend, Zoom
  f.WriteAnsiString(PaintBox1.Font.Name);
  f.write(PaintBox1.Font.Style, sizeof(PaintBox1.Font.Style));
  f.write(PaintBox1.Font.Size, sizeof(PaintBox1.Font.Size));

  f.free;
  StatusBar1.Panels[Panel_index_Filename].Text := ExtractFileName(Filename);
  fChanged := false;
End;

Procedure TForm1.LoadProject(Filename: String);
Var
  FileVersion, j: int32;
  f: TFileStream;
  i: Integer;
  style: TFontStyles;
Begin
  If Not FileExists(Filename) Then exit;
  If Not AskChange() Then exit;
  ClearImgUndoStack();
  ClearObjUndoStack();
  clear;
  fProjectFileName := Filename;
  SetValue('General', 'LastFile', Filename);
  f := TFileStream.Create(Filename, fmOpenRead);
  // Die Version
  FileVersion := -1;
  f.Read(FileVersion, SizeOf(FileVersion)); // For future Use
  // Das Bild
  fimage := TBitmap.Create;
  fImage.LoadFromStream(f);
  // Die Metrik
  fMetrik := TMeasureElement.LoadFromStream(f, PaintBox1) As TMetrik;
  fMetrik.OnChange := @OnMetrikChange;
  // Die Measure Elemente
  j := 0;
  f.Read(j, SizeOf(j));
  setlength(fMeasureElements, j);
  For i := 0 To high(fMeasureElements) Do Begin
    fMeasureElements[i] := TMeasureElement.LoadFromStream(f, PaintBox1);
    fMeasureElements[i].Enabled := false;
    fMeasureElements[i].OnChange := @ObjChange;
  End;
  // Einstellungen der LCL ?
  If FileVersion >= 2 Then Begin
    PaintBox1.Font.Name := f.ReadAnsiString();
    style := [];
    f.Read(style, sizeof(TFontStyles));
    PaintBox1.Font.Style := style;
    j := 0;
    f.Read(j, sizeof(PaintBox1.Font.Size));
    PaintBox1.Font.Size := j;
    For i := 0 To high(fMeasureElements) Do Begin
      fMeasureElements[i].RefreshText();
    End;
  End;
  f.free;
  // Refresh LCL
  fMetrik.Visible := Bevel1.Style = bsLowered;
  Edit1.Text := format('%0.1f', [fMetrik.Distance]);
  CheckBox2Click(Nil); // Text Visible für alle Elemente richtig setzen
  ComboBox1Change(Nil); // Zoom Initialisieren
  UpdateSizeInfo();
  StatusBar1.Panels[Panel_index_Filename].Text := ExtractFileName(Filename);
  If assigned(form12) And form12.Visible Then Begin
    form12.RefreshData;
  End;
  label4.visible := false;
  fChanged := false;
  PaintBox1.Invalidate;
End;

End.

